/* Incremental, generational, concurrent GC using MPS.
   Copyright (C) 2024 Free Software Foundation, Inc.

Author: Gerd Möllmann <gerd@gnu.org>

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>. */

/* Todo:

   + create area root, area scanner
   + staticpro roots
   + built-in symbols root
   + buffer-locals roots
   + specpdl
   + pdumper
   + intervals, overlays
   + run MPS tests
   + --enable-checking
   + mark_lread
   + marl_window
   + alloc conses

   - dump_context

   I think this is handled by scanning what mem_insert has, since
   intervals and overlays are allocated from blocks that are
registered with mem_insert.
   + thread roots (control stack), main thread
   + thread-local allocation points
   + mps_arena_step, idle time.
   + face cache
   + glyph matrices
   + HAVE_TEXT_CONVERSION - can't do it
   - complete cons_skip etc.

   - telemetry
   - symbols, strings etc
   - emacs_abort -> something nicer

*/

// clang-format off

#include <config.h>
#include <stdint.h>

#ifdef HAVE_MPS

#include <mps.h>
#include <mpsavm.h>
#include <mpscamc.h>
#include <mpscams.h>
#include <stdlib.h>
#include "lisp.h"
#include "buffer.h"
#include "thread.h"
#include "pdumper.h"
#include "dispextern.h"
#include "igc.h"

/* For simplicity, I don't suport some stuff.  Should maybe done in
   configure.ac.  */

#ifndef USE_LSB_TAG
#error "USE_LSB_TAG required"
#endif
#ifdef WIDE_EMACS_INT
#error "WIDE_EMACS_INT not supported"
#endif

/* Frames have stuff for text conversion which contains Lisp_Objects, so
   this must be scanned/fixed for MPS, and must be some form of root in
   a mixed GC, so that scanning the Lisp_Objects woiuld prevent moving
   them in memory.  MacOS doesn't HAVE_TEXT_CONVERSION, so that I can't
   do this.  */

#ifdef HAVE_TEXT_CONVERSION
#error "HAVE_TEXT_CONVERSION not supported"
#endif

#ifdef IGC_DEBUG_POOL
#define IGC_CHECK_POOL()				\
do							\
  {							\
    mps_pool_check_fenceposts (global_igc->cons_pool);	\
    mps_pool_check_free_space (global_igc->cons_pool);	\
  } while (0)
#else
#define IGC_CHECK_POOL() (void) 0
#endif

#ifdef IGC_DEBUG
#define IGC_ASSERT(expr)      if (!(expr)) emacs_abort (); else
#define IGC_ASSERT_ALIGNED(p) IGC_ASSERT ((uintptr_t) (p) % GCALIGNMENT == 0)
#else
#define IGC_ASSERT(expr)
#define IGC_ASSERT_ALIGNED(p)
#endif

/* In MPS scan functions it is not easy to call C functions (see the MPS
   documentation).  Rather than taking the risk of using functions from
   lisp.h, which may may not be inlined, I'm therfore using macros. I
   assume that Lisp_Objects are EMACS_INTs, and we are using the 3
   lowest bits for tags, for simplicity.  */

#define IGC_XLI(obj)   lisp_h_XLI (obj)
#define IGC_XIL(obj)   lisp_h_XIL (obj)
#define IGC_XLP(obj)   lisp_h_XLP (obj)

#define IGC_XTYPE(obj) lisp_h_XTYPE (obj)
#define IGC_FIXNUMP(obj) lisp_h_FIXNUMP (obj)

#define IGC_XPNTR(a)						\
  (BARE_SYMBOL_P (a)						\
   ? (char *) lispsym + (XLI (a) - LISP_WORD_TAG (Lisp_Symbol))	\
   : (char *) XLP (a) - (XLI (a) & ~VALMASK));

#define IGC_MAKE_LISP_OBJ(type, untagged) \
  TAG_PTR_INITIALLY (type, untagged)


static mps_res_t scan_mem_area (mps_ss_t ss, void *start,
				void *end, void *closure);
static mps_res_t scan_staticvec (mps_ss_t ss, void *start,
				 void *end, void *closure);
static mps_res_t scan_lisp_objs (mps_ss_t ss, void *start, void *end,
				 void *closure);
static mps_res_t scan_faces_by_id (mps_ss_t ss, void *start, void *end,
				    void *closure);
static mps_res_t scan_glyph_rows (mps_ss_t ss, void *start, void *end,
				  void *closure);

#define IGC_CHECK_RES(res)			\
  if ((res) != MPS_RES_OK)			\
    emacs_abort ();				\
  else

#define IGC_WITH_PARKED(gc)			\
  for (int i = (mps_arena_park(gc->arena), 1);	\
       i;					\
       i = (mps_arena_release (gc->arena), 0))

/* Very poor man's template for double-linked lists.  */

#define IGC_DEFINE_LIST(data)				\
  typedef struct data##_list				\
  {							\
    struct data##_list *next, *prev;			\
    data d;						\
  } data##_list;					\
							\
  static data##_list *					\
  data##_list_push (data##_list **head, data *d)	\
  {							\
    data##_list *r = xzalloc (sizeof *r);		\
    r->d = *d;						\
    r->next = *head;					\
    r->prev = NULL;					\
							\
    if (r->next)					\
      r->next->prev = r;				\
    *head = r;						\
    return r;						\
  }							\
							\
  static void						\
  data##_list_remove (data *d, data##_list **head,	\
		      data##_list *r)			\
  {							\
    if (r->next)					\
      r->next->prev = r->prev;				\
    if (r->prev)					\
      r->prev->next = r->next;				\
    else						\
      *head = r->next;					\
    *d = r->d;						\
    xfree (r);						\
  }

struct igc_root
{
  struct igc *gc;
  mps_root_t root;
};

typedef struct igc_root igc_root;
IGC_DEFINE_LIST (igc_root);

struct igc_thread
{
  struct igc *gc;
  mps_thr_t thr;
  void *cold;
  struct igc_root_list *specpdl_root;
  mps_ap_t cons_ap;
};

typedef struct igc_thread igc_thread;
IGC_DEFINE_LIST (igc_thread);

struct igc
{
  mps_arena_t arena;
  mps_chain_t chain;
  mps_pool_t cons_pool;
  mps_fmt_t cons_fmt;
  struct igc_root_list *roots;
  struct igc_thread_list *threads;
};

static struct igc *global_igc;


/***********************************************************************
				Roots
 ***********************************************************************/

/* Add ROOT to the root registry of GC.  Value is a pointer to a new
   igc_root_list struct for the root.  */

static struct igc_root_list *
register_root (struct igc *gc, mps_root_t root)
{
  struct igc_root r = { .gc = gc, .root = root };
  return igc_root_list_push (&gc->roots, &r);
}

/* Remove root R from its root registry, and free it.  Value is the MPS
   root that was registered.  */

static mps_root_t
deregister_root (struct igc_root_list *r)
{
  struct igc_root root;
  igc_root_list_remove (&root, &r->d.gc->roots, r);
  return root.root;
}

/* Destroy the MPS root in R, and deregister it.  */

static void
remove_root (struct igc_root_list *r)
{
  mps_root_destroy (deregister_root (r));
}

/* Destroy all registered roots of GC.  */

static void
remove_all_roots (struct igc *gc)
{
  while (gc->roots)
    remove_root (gc->roots);
}

/* Called from mem_insert.  Create an MPS root for the memory area
   between START and END, and remember it in the root registry of
   global_igc.  */

void *
igc_on_mem_insert (void *start, void *end)
{
  mps_root_t root;
  mps_res_t res = mps_root_create_area (&root, global_igc->arena,
					mps_rank_ambig (), 0, start,
					end, scan_mem_area, NULL);
  IGC_CHECK_RES (res);
  return register_root (global_igc, root);
}

/* Called from mem_delete.  Remove the correspoing node INFO from the
   registry.  */

void
igc_on_mem_delete (void *info)
{
  remove_root ((struct igc_root_list *) info);
}

/* Add a root for staticvec to GC.  */

static void
add_staticvec_root (struct igc *gc)
{
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
			    staticvec,
			    staticvec + ARRAYELTS (staticvec),
			    scan_staticvec, NULL);
  IGC_CHECK_RES (res);
  register_root (gc, root);
}

static void
add_builtin_symbols_root (struct igc *gc)
{
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
			    lispsym,
			    lispsym + ARRAYELTS (lispsym),
			    scan_mem_area, NULL);
  IGC_CHECK_RES (res);
  register_root (gc, root);
}

/* Odeally, we shoudl not scan the entire area, only to the current
   ptr. And ptr might change in the mutator.  Don't know how this could
   be done with MPS running concurrently.  Instead, make sure that the
   part of the stack that is not used is zeroed.  */

static void
add_specpdl_root (struct igc_thread_list *t)
{
  // For the initial thread, specpdl will be initialzed by
  // init_eval_once, and will be NULL until that happens.
  if (specpdl == NULL)
    return;

  struct igc *gc = t->d.gc;
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
			    specpdl, specpdl_end,
			    scan_mem_area, NULL);
  IGC_CHECK_RES (res);
  t->d.specpdl_root = register_root (gc, root);
}

void
igc_on_specbinding_unused (union specbinding *b)
{
  memset (b, 0, sizeof *b);
}

void
igc_on_alloc_main_thread_specpdl (void)
{
  struct igc_thread_list *t = current_thread->gc_info;
  add_specpdl_root (t);
}

/* Called when specpdl gets reallacated.  */

void
igc_on_grow_specpdl (void)
{
  struct igc_thread_list *t = current_thread->gc_info;
  // FIXME: can we avoid parking?
  IGC_WITH_PARKED (t->d.gc)
    {
      remove_root (t->d.specpdl_root);
      t->d.specpdl_root = NULL;
      add_specpdl_root (t);
    }
}

/* Add a root to GC for scanning buffer B.  */

static void
add_buffer_root (struct igc *gc, struct buffer *b)
{
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
			    &b->name_,
			    &b->own_text,
			    scan_lisp_objs, NULL);
  IGC_CHECK_RES (res);
  register_root (gc, root);
}

/* All all known static roots in Emacs to GC.  */

static void
add_static_roots (struct igc *gc)
{
  add_buffer_root (gc, &buffer_defaults);
  add_buffer_root (gc, &buffer_local_symbols);
  add_staticvec_root (gc);
  add_builtin_symbols_root (gc);
}

/* Add a root for a thread given by T.  */

static void
add_thread_root (struct igc_thread_list *t)
{
  struct igc *gc = t->d.gc;
  mps_root_t root;
  mps_res_t res
    = mps_root_create_thread_tagged (&root,
				     gc->arena,
				     mps_rank_ambig (),
				     0,
				     t->d.thr,
				     mps_scan_area_masked,
				     VALMASK,
				     0,
				     t->d.cold);
  IGC_CHECK_RES (res);
  register_root (gc, root);
}

/* Called after a pdump has been loaded.  Add the area as root.  */

void
igc_on_pdump_loaded (void)
{
  struct igc *gc = global_igc;
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
			    (void *) dump_public.start,
			    (void *) dump_public.end,
			    scan_mem_area, NULL);
  IGC_CHECK_RES (res);
  register_root (gc, root);
}

/* For all faces in a face cache, we need to fix the lface vector of
   Lisp_Objects.  */

void
igc_on_make_face_cache (void *c)
{
  struct face_cache *cache = c;
  struct igc *gc = global_igc;
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
			    (void *) cache->faces_by_id,
			    (void *) (cache->faces_by_id + cache->size),
			    scan_faces_by_id, NULL);
  IGC_CHECK_RES (res);
  cache->igc_info = register_root (gc, root);
}

void
igc_on_free_face_cache (void *c)
{
  struct face_cache *cache = c;
  remove_root (cache->igc_info);
  cache->igc_info = NULL;
}

void
igc_on_face_cache_change (void *c)
{
  /* FIXME: can we avoid parking? The idea would be to add a new root
     first, and then remove the old one, so that there is no gap in
     which we don't have no root.  Alas, MPS says that no two roots may
     overlap, which could be the case with realloc.  */
  IGC_WITH_PARKED (global_igc)
    {
      igc_on_free_face_cache (c);
      igc_on_make_face_cache (c);
    }
}

void
igc_on_adjust_glyph_matrix (void *m)
{
  struct igc *gc = global_igc;
  struct glyph_matrix *matrix = m;
  IGC_WITH_PARKED (gc)
    {
      if (matrix->igc_info)
	remove_root (matrix->igc_info);
      mps_root_t root;
      mps_res_t res
	= mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
				matrix->rows,
				(void *) (matrix->rows + matrix->rows_allocated),
				scan_glyph_rows, NULL);
      IGC_CHECK_RES (res);
      matrix->igc_info = register_root (gc, root);
    }
}

void
igc_on_free_glyph_matrix (void *m)
{
  struct glyph_matrix *matrix = m;
  if (matrix->igc_info)
    {
      remove_root (matrix->igc_info);
      matrix->igc_info = NULL;
    }
}

void *
igc_on_grow_read_stack (void *info, void *start, void *end)
{
  struct igc *gc = global_igc;
  IGC_WITH_PARKED (gc)
    {
      if (info)
	remove_root (info);
      mps_root_t root;
      mps_res_t res
	= mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
				start, end,
				scan_mem_area, NULL);
      IGC_CHECK_RES (res);
      info = register_root (gc, root);
    }

  return info;
}

static void
release_arena (void)
{
  mps_arena_release (global_igc->arena);
}

specpdl_ref
igc_inhibit_garbage_collection (void)
{
  specpdl_ref count = SPECPDL_INDEX ();
  mps_arena_park (global_igc->arena);
  record_unwind_protect_void (release_arena);
  return count;
}


/***********************************************************************
			   Allocation Points
 ***********************************************************************/

static void
make_thread_aps (struct igc_thread *t)
{
  struct igc *gc = t->gc;
  mps_res_t res;

  res = mps_ap_create_k (&t->cons_ap, gc->cons_pool, mps_args_none);
  IGC_CHECK_RES (res);
}

static void
free_thread_aps (struct igc_thread_list *t)
{
  mps_ap_destroy (t->d.cons_ap);
  t->d.cons_ap = NULL;
}


/***********************************************************************
				Threads
 ***********************************************************************/

static struct igc_thread_list *
register_thread (struct igc *gc, mps_thr_t thr, void *cold)
{
  struct igc_thread t = { .gc = gc, .thr = thr, .cold = cold };
  return igc_thread_list_push (&gc->threads, &t);
}

static mps_thr_t
deregister_thread (struct igc_thread_list *t)
{
  struct igc_thread thread;
  igc_thread_list_remove (&thread, &t->d.gc->threads, t);
  return thread.thr;
}

/* Called from run_thread.  */

void *
igc_thread_add (const void *cold)
{
  mps_thr_t thr;
  mps_res_t res = mps_thread_reg (&thr, global_igc->arena);
  IGC_CHECK_RES (res);

  struct igc_thread_list *t
    = register_thread (global_igc, thr, (void *) cold);

  add_thread_root (t);
  add_specpdl_root (t);
  make_thread_aps (&t->d);
  return t;
}

/* Called from run_thread.  */

void
igc_thread_remove (void *info)
{
  struct igc_thread_list *t = info;
  free_thread_aps (t);
  mps_thread_dereg (deregister_thread (t));
}

static void
free_all_threads (struct igc *gc)
{
  while (gc->threads)
    igc_thread_remove (gc->threads);
}

static void
add_main_thread (void)
{
  current_thread->gc_info = igc_thread_add (stack_bottom);
}



/***********************************************************************
				Scanning
 ***********************************************************************/

/* Fix a Lisp_Object at *P.  SS ist the MPS scan state.  */

static mps_res_t
fix_lisp_obj (mps_ss_t ss, Lisp_Object *p)
{
  if (IGC_FIXNUMP (*p))
    return MPS_RES_OK;

  MPS_SCAN_BEGIN (ss)
    {
      mps_addr_t untagged = IGC_XPNTR (*p);
      if (MPS_FIX1 (ss, untagged))
	{
	  mps_res_t res = MPS_FIX2 (ss, &untagged);
	  if (res != MPS_RES_OK)
	    return res;
	  enum Lisp_Type type = IGC_XTYPE (*p);
	  *p = IGC_MAKE_LISP_OBJ (type, untagged);
	}
    }
  MPS_SCAN_END (ss);

  return MPS_RES_OK;
}

#define IGC_FIX_LISP_OBJ(ss, p)				\
  do							\
    {							\
      mps_res_t res;					\
      MPS_FIX_CALL (ss, res = fix_lisp_obj (ss, p));	\
      if (res != MPS_RES_OK)				\
	return res;					\
    }							\
  while (0)

/* Horrible shit to avoid unused variable warnings.  */

#define IGC_SCAN_BEGIN(ss)			\
  {						\
    MPS_SCAN_BEGIN (ss)				\
    (void) _mps_zs;				\
    (void) _mps_ufs;				\
    (void) _mps_w;				\
    (void) _mps_wt;

#define IGC_SCAN_END(ss)	MPS_SCAN_END (ss); }

static int fwdsig;
#define IGC_FWDSIG ((mps_addr_t) &fwdsig)

struct igc_fwd
{
  mps_addr_t sig;
  mps_addr_t new;
};

static void
forward (mps_addr_t old, mps_addr_t new)
{
  struct igc_fwd m = { .sig = IGC_FWDSIG, .new = new };
  struct igc_fwd *f = old;
  *f = m;
}

static mps_addr_t
is_forwarded (mps_addr_t addr)
{
  struct igc_fwd *f = addr;
  return f->sig == IGC_FWDSIG ? f->new : NULL;
}

/* Scan a vector of glyph_rows.  */

static mps_res_t
scan_glyph_rows (mps_ss_t ss, void *start, void *end, void *closure)
{
  IGC_SCAN_BEGIN (ss)
    {
      for (struct glyph_row *row = start; row < (struct glyph_row *) end; ++row)
	{
	  struct glyph *glyph = row->glyphs[LEFT_MARGIN_AREA];
	  struct glyph *end = row->glyphs[LAST_AREA];
	  for (; glyph < end; ++glyph)
	    IGC_FIX_LISP_OBJ (ss, &glyph->object);
	}
    }
  IGC_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_faces_by_id (mps_ss_t ss, void *start, void *end, void *closure)
{
  IGC_SCAN_BEGIN (ss)
  {
    for (struct face **p = start; p < (struct face **) end; ++p)
      if (*p)
	{
	  struct face *face = *p;
	  for (int i = 0; i < ARRAYELTS (face->lface); ++i)
	    IGC_FIX_LISP_OBJ (ss, &face->lface[i]);
	}
  }
  IGC_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Scan a memory area at [START, END). SS is the MPS scan state.
   CLOSURE is ignored.  */

static mps_res_t
scan_mem_area (mps_ss_t ss, void *start, void *end, void *closure)
{
  IGC_SCAN_BEGIN (ss)
  {
    for (Lisp_Object *p = start; p < (Lisp_Object *) end; ++p)
      IGC_FIX_LISP_OBJ (ss, p);
  }
  IGC_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Scan staticvec in the interval [START, END). SS is the MPS scan
   state.  CLOSURE is ignored.  */

static mps_res_t
scan_staticvec (mps_ss_t ss, void *start, void *end, void *closure)
{
  IGC_SCAN_BEGIN (ss)
  {
    /* I don't want to rely on staticidx ATM. Instead, ignore NULL
       entries.  */
    for (Lisp_Object **p = start; p < (Lisp_Object **) end; ++p)
      if (*p)
	IGC_FIX_LISP_OBJ (ss, *p);
  }
  IGC_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Scan staticvec in the interval [START, END). SS is the MPS scan
   state.  CLOSURE is ignored.  */

static mps_res_t
scan_lisp_objs (mps_ss_t ss, void *start, void *end, void *closure)
{
  IGC_SCAN_BEGIN (ss)
    {
      for (Lisp_Object *p = start; p < (Lisp_Object *) end; ++p)
	IGC_FIX_LISP_OBJ (ss, p);
    }
  IGC_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Scan a Lisp_Cons.  */

static mps_res_t
cons_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  IGC_SCAN_BEGIN (ss)
    {
      for (struct Lisp_Cons *cons = (struct Lisp_Cons *) base;
	   cons < (struct Lisp_Cons *) limit;
	   ++cons)
	{
	  IGC_FIX_LISP_OBJ (ss, &cons->u.s.car);
	  IGC_FIX_LISP_OBJ (ss, &cons->u.s.u.cdr);
	}
    }
  IGC_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
cons_skip (mps_addr_t addr)
{
  return (char *) addr + sizeof (struct Lisp_Cons);
}

/* Called by MPS when object at OLD has been moved to NEW.  Must replace
   *OLD with a forwarding marker that points to NEW.  */

static void
cons_fwd (mps_addr_t old, mps_addr_t new)
{
  forward (old, new);
}

static mps_addr_t
cons_isfwd (mps_addr_t addr)
{
  return is_forwarded (addr);
}

static void
cons_pad (mps_addr_t addr, size_t size)
{
  memset (addr, 0, size);
}


/***********************************************************************
				Walking
 ***********************************************************************/

struct igc_walk
{
  void (* fun) (Lisp_Object);
  int count;
};

static void
visit_lisp_obj (Lisp_Object obj, struct igc_walk *walk)
{
  if (IGC_FIXNUMP (obj))
    return;

  mps_addr_t ref = IGC_XPNTR (obj);
  if (!mps_arena_has_addr (global_igc->arena, ref))
    {
      walk->fun (obj);
      ++walk->count;
    }
}

static mps_res_t
cons_scan_area (mps_ss_t ss, mps_addr_t base, mps_addr_t limit,
		void *closure)
{
  struct igc_walk *walk = closure;
  for (struct Lisp_Cons *p = base; p < (struct Lisp_Cons *) limit; ++p)
    {
      visit_lisp_obj (p->u.s.car, walk);
      visit_lisp_obj (p->u.s.u.cdr, walk);
    }

  return MPS_RES_OK;
}

/* */

static void
mark_old_gc_objects (struct igc *gc)
{
  IGC_WITH_PARKED (gc)
    {
      struct igc_walk walk = { .fun = mark_object };
      mps_pool_walk (gc->cons_pool, cons_scan_area, &walk);
    }
}

/* Called when the old GC runs.  Mark objects managed by the old GC
   which are referenced from MPS objects.  */

void
igc_on_old_gc (void)
{
  mark_old_gc_objects (global_igc);
}

#ifdef IGC_DEBUG

static void
check_cons (Lisp_Object obj)
{
  if (STRINGP (obj))
    {
      struct Lisp_String *ptr = XSTRING (obj);
      IGC_ASSERT (ptr != NULL);
    }
}


void igc_debug_conses (struct igc *gc);

void
igc_debug_conses (struct igc *gc)
{
  IGC_WITH_PARKED (gc)
    {
      struct igc_walk walk = { .fun = check_cons };
      mps_pool_walk (gc->cons_pool, cons_scan_area, &walk);
    }
}

#endif

/***********************************************************************
				Finalization
 ***********************************************************************/

/* ADDR is a block registered for finalization with mps_finalize.
   AFAICT, this is always a PVEC_FINALIZER.  */

static void
do_finalize (struct igc *gc, mps_addr_t addr)
{
  struct Lisp_Finalizer *fin = addr;
  if (!NILP (fin->function))
    {
      Lisp_Object fun = fin->function;
      fin->function = Qnil;
      run_finalizer_function (fun);
    }
}

static void
handle_messages (struct igc *gc)
{
  mps_message_type_t type;
  while (mps_message_queue_type (&type, gc->arena))
    {
      mps_message_t msg;
      if (mps_message_get (&msg, gc->arena, type))
	{
	  IGC_ASSERT (type == mps_message_type_finalization ());
	  mps_addr_t addr;
	  mps_message_finalization_ref (&addr, gc->arena, msg);
	  do_finalize (gc, addr);
	  mps_message_discard (gc->arena, msg);
	}
    }
}

static void
enable_finalization (struct igc *gc, bool enable)
{
  mps_message_type_t type = mps_message_type_finalization ();
  if (enable)
    mps_message_type_enable (gc->arena, type);
  else
    mps_message_type_disable (gc->arena, type);
}

void
igc_handle_messages (void)
{
  handle_messages (global_igc);
}

void
igc_on_idle (void)
{
  mps_arena_step (global_igc->arena, 0.01, 0);
}


/***********************************************************************
			    Allocation
 ***********************************************************************/

static mps_ap_t
current_cons_ap (void)
{
  struct igc_thread_list *t = current_thread->gc_info;
  return t->d.cons_ap;
}

void igc_break (void)
{
}

Lisp_Object
igc_make_cons (Lisp_Object car, Lisp_Object cdr)
{
  mps_ap_t ap = current_cons_ap ();
  size_t size = sizeof (struct Lisp_Cons);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, size);
      IGC_CHECK_RES (res);
      struct Lisp_Cons *cons = p;
      cons->u.s.car = car;
      cons->u.s.u.cdr = cdr;
    }
  while (!mps_commit (ap, p, size));

  IGC_ASSERT_ALIGNED (p);

  return make_lisp_ptr (p, Lisp_Cons);
}


/***********************************************************************
			    Setup/Tear down
 ***********************************************************************/

static struct igc *
make_igc (void)
{
  struct igc *gc = xzalloc (sizeof *gc);
  mps_res_t res;

  // Arena
  MPS_ARGS_BEGIN (args)
  {
    res = mps_arena_create_k (&gc->arena, mps_arena_class_vm (), args);
  }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);

  // Generations
  mps_gen_param_s gen_params[]
    = { { 32000, 0.8 }, { 5 * 32009, 0.4 } };
  res = mps_chain_create (&gc->chain, gc->arena, ARRAYELTS (gen_params),
			  gen_params);
  IGC_CHECK_RES (res);

  // Object format for conses.
  MPS_ARGS_BEGIN (args)
  {
    MPS_ARGS_ADD (args, MPS_KEY_FMT_ALIGN, GCALIGNMENT);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_HEADER_SIZE, 0);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_SCAN, cons_scan);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_SKIP, cons_skip);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_FWD, cons_fwd);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_ISFWD, cons_isfwd);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_PAD, cons_pad);
    res = mps_fmt_create_k (&gc->cons_fmt, gc->arena, args);
  }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);

#ifdef IGC_DEBUG_POOL
  mps_class_t ams_pool_class = mps_class_ams_debug ();
#else
  mps_class_t ams_pool_class = mps_class_ams ();
#endif

  mps_pool_debug_option_s debug_options = {
    "fencepost", 9,
    "free", 4,
  };

  // Pool for conses. Since conses have no type field which would let
  // us recognize them when mixed with other objects, use a dedicated
  // pool.
  MPS_ARGS_BEGIN (args)
    {
      MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &debug_options);
      MPS_ARGS_ADD (args, MPS_KEY_FORMAT, gc->cons_fmt);
      MPS_ARGS_ADD (args, MPS_KEY_CHAIN, gc->chain);
      MPS_ARGS_ADD (args, MPS_KEY_INTERIOR, 0);
      res = mps_pool_create_k (&gc->cons_pool, gc->arena,
			       ams_pool_class, args);
    }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);

  add_static_roots (gc);
  enable_finalization (gc, true);

  return gc;
}

static void
free_igc (struct igc *gc)
{
  free_all_threads (gc);
  mps_pool_destroy (gc->cons_pool);
  mps_fmt_destroy (gc->cons_fmt);
  remove_all_roots (gc);
  mps_chain_destroy (gc->chain);
  mps_arena_destroy (gc->arena);
  xfree (gc);
}

static void
free_global_igc (void)
{
  free_igc (global_igc);
}

void
syms_of_igc (void)
{
}

void
init_igc (void)
{
  global_igc = make_igc ();
  atexit (free_global_igc);
  add_main_thread ();
}

#endif // HAVE_MPS