/* Incremental, generational, concurrent GC using MPS.
   Copyright (C) 2024 Free Software Foundation, Inc.

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

// clang-format off

#include <config.h>
#include <stdio.h>

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
//#include "puresize.h"
#include "intervals.h"

#ifndef USE_LSB_TAG
#error "USE_LSB_TAG required"
#endif
#ifdef WIDE_EMACS_INT
#error "WIDE_EMACS_INT not supported"
#endif
#if USE_STACK_LISP_OBJECTS
#error "USE_STACK_LISP_OBJECTS not supported"
#endif

#pragma GCC diagnostic ignored "-Wunused-function"

/* Frames have stuff for text conversion which contains Lisp_Objects, so
   this must be some form of root.  MacOS doesn't HAVE_TEXT_CONVERSION,
   so that I left this out.  */

#ifdef HAVE_TEXT_CONVERSION
#error "HAVE_TEXT_CONVERSION not supported"
#endif

#ifdef IGC_DEBUG_POOL
#define IGC_CHECK_POOLS()					\
  do								\
    {								\
      mps_pool_check_fenceposts (global_igc->cons_pool);	\
      mps_pool_check_free_space (global_igc->cons_pool);	\
      mps_pool_check_fenceposts (global_igc->symbol_pool);	\
      mps_pool_check_free_space (global_igc->symbol_pool);	\
    } while (0)
#else
#define IGC_CHECK_POOLS() (void) 0
#endif

#ifdef IGC_DEBUG
#define IGC_ASSERT(expr) if (!(expr)) emacs_abort (); else
#else
#define IGC_ASSERT(expr) (void) 9
#endif

/* I can't remember what it does otherwise.  */
#define igc_static_assert(x) verify (x)

/* Mask for the tag part of a reference.  */
#define IGC_TAG_MASK (~ VALMASK)

static mps_res_t scan_area_ambig (mps_ss_t ss, void *start,
				  void *end, void *closure);
static mps_res_t scan_staticvec (mps_ss_t ss, void *start,
				 void *end, void *closure);
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

/* A MPS root that we created. */

struct igc_root {
  struct igc *gc;
  mps_root_t root;
  /* Memory covered, END can be NULL for control stacks.   */
  void *start, *end;
};

typedef struct igc_root igc_root;
IGC_DEFINE_LIST (igc_root);

enum igc_type {
  IGC_TYPE_CONS,
  IGC_TYPE_SYMBOL,
  IGC_TYPE_INTERVAL,
  IGC_TYPE_STRING,
  IGC_TYPE_STRING_DATA,
  IGC_TYPE_VECTOR,
  IGC_TYPE_LAST
};

/* An MPS thread we registered.  */

struct igc_thread {
  struct igc *gc;
  mps_thr_t thr;
  void *stack_start;
  struct igc_root_list *specpdl_root;
  mps_ap_t ap[IGC_TYPE_LAST];
};

typedef struct igc_thread igc_thread;
IGC_DEFINE_LIST (igc_thread);

/* Registry for MPS objects.  */

struct igc {
  mps_arena_t arena;
  mps_chain_t chain;
  mps_fmt_t fmt[IGC_TYPE_LAST];
  mps_pool_t pool[IGC_TYPE_LAST];;
  struct igc_root_list *roots;
  struct igc_thread_list *threads;
};

/* Global MPS object registry.  */
static struct igc *global_igc;


/***********************************************************************
				Registry
 ***********************************************************************/

/* Add ROOT for given memory area START, END to the registry GC.  Value
   is a pointer to a new igc_root_list struct for the root.  */

static struct igc_root_list *
register_root (struct igc *gc, mps_root_t root, void *start, void *end)
{
  struct igc_root r = { .gc = gc, .root = root, .start = start, .end = end };
  return igc_root_list_push (&gc->roots, &r);
}

/* Remove root R from its registry, and free it.  Value is the MPS root
   that was registered.  */

static mps_root_t
deregister_root (struct igc_root_list *r)
{
  struct igc_root root;
  igc_root_list_remove (&root, &r->d.gc->roots, r);
  return root.root;
}

/* Destroy the MPS root in R, and deregister it.  */

static void
destroy_root (struct igc_root_list *r)
{
  mps_root_destroy (deregister_root (r));
}

/* Destroy all registered roots of GC.  */

static void
destroy_all_roots (struct igc *gc)
{
  while (gc->roots)
    destroy_root (gc->roots);
}

/* Create an ambigus root for the memory area [START, END), and register
   it in GC.  Value is the a pointer to the igc_root_list in which the
   root was registered.  */

static igc_root_list *
create_ambig_root (struct igc *gc, void *start, void *end)
{
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area_tagged (&root,
				   gc->arena,
				   mps_rank_ambig (),
				   0, /* MPS_PROT_... */
				   start,
				   end,
				   scan_area_ambig,
				   IGC_TAG_MASK,
				   0);
  IGC_CHECK_RES (res);
  return register_root (gc, root, start, end);
}

/* Allocate SIZE bytes of memory, and register the allocated block as an
   ambigous root.  */

void *
igc_xalloc_ambig_root (size_t size)
{
  char *start = xzalloc (size);
  create_ambig_root (global_igc, start, start + size);
  return start;
}

/* Find a root with a given start address START in the registry GC.  */

static igc_root_list *
find_root_with_start (struct igc *gc, void *start)
{
  for (struct igc_root_list *r = gc->roots; r; r = r->next)
    if (r->d.start == start)
      return r;
  return NULL;
}

/* Free a block P that has been created with igc_malloc_ambig_root.  */

void
igc_xfree_ambig_root (void *p)
{
  if (p == NULL)
    return;

  struct igc_root_list *r = find_root_with_start (global_igc, p);
  IGC_ASSERT (r != NULL);
  destroy_root (r);
  xfree (p);
}

/* Add a root for staticvec to GC.  */

static void
add_staticvec_root (struct igc *gc)
{
  void *start =staticvec, *end = staticvec + ARRAYELTS (staticvec);
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
			    start, end, scan_staticvec, NULL);
  IGC_CHECK_RES (res);
  register_root (gc, root, start, end);
}

/* Add a root for lispsym to GC.  */

static void
add_lispsym_root (struct igc *gc)
{
  void *start = lispsym, *end = lispsym + ARRAYELTS (lispsym);
  // Maybe we could do better than using an ambiguous root.
  create_ambig_root (gc, start, end);
}

/* Odeally, we shoudl not scan the entire area, only to the current
   ptr. And ptr might change in the mutator.  Don't know how this could
   be done with MPS running concurrently.  Instead, make sure that the
   part of the stack that is not used is zeroed.  */

static void
create_specpdl_root (struct igc_thread_list *t)
{
  // For the initial thread, specpdl will be initialzed by
  // init_eval_once, and will be NULL until that happens.
  if (specpdl)
    {
      struct igc *gc = t->d.gc;
      // Maybe we could do better than using an ambiguous root.
      t->d.specpdl_root = create_ambig_root (gc, specpdl, specpdl_end);
    }
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
  create_specpdl_root (t);
}

/* Called when specpdl gets reallacated.  */

void
igc_on_grow_specpdl (void)
{
  struct igc_thread_list *t = current_thread->gc_info;
  // FIXME: can we avoid parking?
  IGC_WITH_PARKED (t->d.gc)
    {
      destroy_root (t->d.specpdl_root);
      t->d.specpdl_root = NULL;
      create_specpdl_root (t);
    }
}

/* Add a root to GC for scanning buffer B.  */

static void
add_buffer_root (struct igc *gc, struct buffer *b)
{
  void *start = &b->name_, *end = &b->own_text;
  // Maybe we could do better than using an ambiguous root.
  create_ambig_root (gc, start, end);
}

/* All all known static roots in Emacs to GC.  */

static void
add_static_roots (struct igc *gc)
{
  add_buffer_root (gc, &buffer_defaults);
  add_buffer_root (gc, &buffer_local_symbols);
  add_staticvec_root (gc);
  add_lispsym_root (gc);
}

/* Add a root for a thread given by T.  */

static void
create_thread_root (struct igc_thread_list *t)
{
  struct igc *gc = t->d.gc;
  mps_root_t root;
  mps_res_t res
    = mps_root_create_thread_tagged (&root, gc->arena, mps_rank_ambig (),
				     0, t->d.thr, scan_area_ambig,
				     IGC_TAG_MASK, 0, t->d.stack_start);
  IGC_CHECK_RES (res);
  register_root (gc, root, t->d.stack_start, NULL);
}

/* Called after a pdump s been loaded.  Add the area as root
   because there could be references in it.  */

void
igc_on_pdump_loaded (void)
{
  struct igc *gc = global_igc;
  void *start = (void *) dump_public.start, *end = (void *) dump_public.end;
  create_ambig_root (gc, start, end);
}

/* For all faces in a face cache, we need to fix the lface vector of
   Lisp_Objects.  */

void
igc_on_make_face_cache (void *c)
{
  struct face_cache *cache = c;
  struct igc *gc = global_igc;
  void *start = (void *) cache->faces_by_id;
  void *end = (void *) (cache->faces_by_id + cache->size);
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
			    start, end, scan_faces_by_id, NULL);
  IGC_CHECK_RES (res);
  cache->igc_info = register_root (gc, root, start, end);
}

void
igc_on_free_face_cache (void *c)
{
  struct face_cache *cache = c;
  destroy_root (cache->igc_info);
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
	destroy_root (matrix->igc_info);
      mps_root_t root;
      void *start = matrix->rows;
      void *end = (void *) (matrix->rows + matrix->rows_allocated);
      mps_res_t res
	= mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
				start, end,
				scan_glyph_rows, NULL);
      IGC_CHECK_RES (res);
      matrix->igc_info = register_root (gc, root, start, end);
    }
}

void
igc_on_free_glyph_matrix (void *m)
{
  struct glyph_matrix *matrix = m;
  if (matrix->igc_info)
    {
      destroy_root (matrix->igc_info);
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
	destroy_root (info);
      info = create_ambig_root (gc, start, end);
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
create_thread_aps (struct igc_thread *t)
{
  struct igc *gc = t->gc;

  for (enum igc_type type = 0; type < IGC_TYPE_LAST; ++type)
    {
      mps_res_t res = mps_ap_create_k (&t->ap[type], gc->pool[type],
				       mps_args_none);
      IGC_CHECK_RES (res);
    }
}

static void
destroy_thread_aps (struct igc_thread_list *t)
{
  for (enum igc_type i = 0; i < IGC_TYPE_LAST; ++i)
    mps_ap_destroy (t->d.ap[i]);
}


/***********************************************************************
				Threads
 ***********************************************************************/

static struct igc_thread_list *
register_thread (struct igc *gc, mps_thr_t thr, void *cold)
{
  struct igc_thread t = { .gc = gc, .thr = thr, .stack_start = cold };
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
igc_thread_add (const void *stack_start)
{
  mps_thr_t thr;
  mps_res_t res = mps_thread_reg (&thr, global_igc->arena);
  IGC_CHECK_RES (res);

  struct igc_thread_list *t
    = register_thread (global_igc, thr, (void *) stack_start);

  create_thread_root (t);
  create_specpdl_root (t);
  create_thread_aps (&t->d);
  return t;
}

/* Called from run_thread.  */

void
igc_thread_remove (void *info)
{
  struct igc_thread_list *t = info;
  destroy_thread_aps (t);
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

/* Horrible shit to avoid unused variable warnings.  */

static int fwdsig;
#define IGC_FWDSIG ((mps_addr_t) &fwdsig)

struct igc_fwd {
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

static int padsig;
#define IGC_PADSIG ((mps_addr_t) &padsig)

struct igc_pad {
  mps_addr_t sig;
};

igc_static_assert (sizeof (struct Lisp_Cons) >= sizeof (struct igc_fwd));
igc_static_assert (sizeof (struct interval) >= sizeof (struct igc_fwd));
igc_static_assert (sizeof (struct Lisp_Cons) >= sizeof (struct igc_pad));
igc_static_assert (sizeof (struct interval) >= sizeof (struct igc_pad));

static void
pad (mps_addr_t addr, size_t size)
{
  struct igc_pad padding = { .sig = IGC_PADSIG };
  IGC_ASSERT (size <= sizeof padding);

  *(struct igc_pad *) addr = padding;
  char *p = (char *) addr + sizeof padding;
  char *end = (char *) addr + size;
  while (p < end)
    {
      static const char string[] = "padding";
      const size_t n = min (sizeof string, end - p);
      memcpy (p, string, n);
      p += n;
    }
}

static bool
is_padding (mps_addr_t addr)
{
  struct igc_pad *p = addr;
  return p->sig == IGC_PADSIG;
}

/* These may come from MPS_SCAN_BEGIN / END.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"

static mps_res_t
fix_lisp_obj (mps_ss_t ss, Lisp_Object *pobj)
{
  MPS_SCAN_BEGIN (ss)
    {
      mps_word_t *p = (mps_word_t *) pobj;
      mps_word_t word = *p;
      mps_word_t tag = word & IGC_TAG_MASK;

      if (tag == Lisp_Int0 && tag == Lisp_Int1)
	return MPS_RES_OK;

      if (tag == Lisp_Symbol)
	{
	  mps_word_t off = word ^ tag;
	  mps_addr_t ref = (mps_addr_t) ((char *) lispsym + off);
	  if (MPS_FIX1 (ss, ref))
	    {
	      mps_res_t res = MPS_FIX2 (ss, &ref);
	      if (res != MPS_RES_OK)
		return res;
	      mps_word_t new_off = (char *) ref - (char *) lispsym;
	      mps_addr_t new_ref = (mps_addr_t) ((char *) lispsym + new_off);
	      *p = (mps_word_t) new_ref | tag;
	    }
	}
      else
	{
	  mps_addr_t ref = (mps_addr_t) (word ^ tag);
	  if (MPS_FIX1 (ss, ref))
	    {
	      mps_res_t res = MPS_FIX2 (ss, &ref);
	      if (res != MPS_RES_OK)
		return res;
	      *p = (mps_word_t) ref | tag;
	    }
	}
    }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

#define IGC_FIX_OBJ(p)					\
  do {							\
    mps_res_t res;					\
    MPS_FIX_CALL (ss, res = fix_lisp_obj (ss, (p)));	\
    if (res != MPS_RES_OK)				\
      return res;					\
  } while (0)

/* Scan a vector of glyph_rows.  */

static mps_res_t
scan_glyph_rows (mps_ss_t ss, void *start, void *end, void *closure)
{
  //fprintf (stderr, "*** scan_glyph_rows %p\n", start);
  MPS_SCAN_BEGIN (ss)
    {
      for (struct glyph_row *row = start; row < (struct glyph_row *) end; ++row)
	{
	  struct glyph *glyph = row->glyphs[LEFT_MARGIN_AREA];
	  struct glyph *end = row->glyphs[LAST_AREA];
	  for (; glyph < end; ++glyph)
	    IGC_FIX_OBJ (&glyph->object);
	}
    }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_faces_by_id (mps_ss_t ss, void *start, void *end, void *closure)
{
  //fprintf (stderr, "*** scan_faces_by_id %p\n", start);
  MPS_SCAN_BEGIN (ss)
  {
    for (struct face **p = start; p < (struct face **) end; ++p)
      if (*p)
	{
	  struct face *face = *p;
	  for (int i = 0; i < ARRAYELTS (face->lface); ++i)
	    IGC_FIX_OBJ (&face->lface[i]);
	}
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Scan staticvec in the interval [START, END). SS is the MPS scan
   state.  CLOSURE is ignored.  */

static mps_res_t
scan_staticvec (mps_ss_t ss, void *start, void *end, void *closure)
{
  //fprintf (stderr, "*** scan_staticvec %p\n", start);
  MPS_SCAN_BEGIN (ss)
    {
      for (int i = 0; i < staticidx; ++i)
	IGC_FIX_OBJ ((Lisp_Object *) staticvec[i]);
    }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* The pointer part of a tagged word for symbols contains an offset from
   lispsym. and the Lisp_Symbol tag is zero.  */

static mps_res_t
scan_area_ambig (mps_ss_t ss, void *start, void *end, void *closure)
{
  //fprintf (stderr, "*** scan_area_ambig %p\n", start);
  MPS_SCAN_BEGIN (ss)
    {
      for (mps_word_t *p = start; p < (mps_word_t *) end; ++p)
	{
	  mps_word_t word = *p;
	  mps_word_t tag = word & IGC_TAG_MASK;

	  if (tag == Lisp_Int0 && tag == Lisp_Int1)
	    continue;

	  // Assuming word is a normal pointer
	  mps_addr_t ref = (mps_addr_t) (word ^ tag);
	  if (MPS_FIX1 (ss, ref))
	    {
	      mps_res_t res = MPS_FIX2 (ss, &ref);
	      if (res != MPS_RES_OK)
		return res;
	    }

	  // Assuming ref is a symbol reference.
	  if (tag == Lisp_Symbol)
	    {
	      mps_word_t off = word ^ tag;
	      mps_addr_t ref = (mps_addr_t) ((char *) lispsym + off);
	      if (MPS_FIX1 (ss, ref))
		{
		  mps_res_t res = MPS_FIX2 (ss, &ref);
		  if (res != MPS_RES_OK)
		    return res;
		}
	    }
	}
    }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Scan a Lisp_Cons.  Must be able to handle padding and forwaring
   objects. */

static mps_res_t
cons_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  //fprintf (stderr, "*** cons_scan %p\n", base);
  MPS_SCAN_BEGIN (ss)
    {
      for (struct Lisp_Cons *cons = (struct Lisp_Cons *) base;
	   cons < (struct Lisp_Cons *) limit;
	   ++cons)
	{
	  if (is_forwarded (cons) || is_padding (cons))
	    continue;
	  IGC_FIX_OBJ (&cons->u.s.car);
	  IGC_FIX_OBJ (&cons->u.s.u.cdr);
	}
    }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
cons_skip (mps_addr_t addr)
{
  return (char *) addr + sizeof (struct Lisp_Cons);
}

static mps_res_t
symbol_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  //fprintf (stderr, "*** symbol_scan %p\n", base);
  MPS_SCAN_BEGIN (ss)
    {
      for (struct Lisp_Symbol *sym = (struct Lisp_Symbol *) base;
	   sym < (struct Lisp_Symbol *) limit;
	   ++sym)
	{
	  if (is_forwarded (sym) || is_padding (sym))
	    continue;

	  IGC_FIX_OBJ (&sym->u.s.name);
	  if (sym->u.s.redirect == SYMBOL_PLAINVAL)
	    IGC_FIX_OBJ (&sym->u.s.val.value);
	  IGC_FIX_OBJ (&sym->u.s.function);
	  IGC_FIX_OBJ (&sym->u.s.plist);
	  IGC_FIX_OBJ (&sym->u.s.package);
	}
    }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
symbol_skip (mps_addr_t addr)
{
  return (char *) addr + sizeof (struct Lisp_Symbol);
}

#define IGC_FIX12_PTR(ss, expr)					\
  do								\
    {								\
      mps_res_t res = MPS_FIX12 (ss, (mps_addr_t *) &expr);	\
      if (res != MPS_RES_OK)					\
	return res;						\
    } while (0)

static mps_res_t
string_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
    {
      for (struct Lisp_String *s = (struct Lisp_String *) base;
	   s < (struct Lisp_String *) limit;
	   ++s)
	{
	  if (is_forwarded (s) || is_padding (s))
	    continue;

	  IGC_FIX12_PTR (ss, s->u.s.data);
	  // INTERVAL intervals
	}
    }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
string_skip (mps_addr_t addr)
{
  return (char *) addr + sizeof (struct Lisp_String);
}

/* There are several ways one could store strings in MPS. For example,
   one could append string data to Lisp_Strings. For simplicity,
   I store string data in a pool of its own, so that I don't have to
   change the rest of Emacs.  The folllowing is a small header stored
   with string data to be able to skip, forward etc.  */

struct igc_sdata {
  mps_addr_t object_end;
  unsigned char contents[];
};

static unsigned char *
sdata_contents (struct igc_sdata *d)
{
  return d->contents;
}

/* Value is the address just past the object being skipped. String
   data is always NUL terminated.  */

static mps_addr_t
string_data_skip (mps_addr_t addr)
{
  return ((struct igc_sdata *) addr)->object_end;
}

static mps_res_t
interval_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
    {
      for (struct interval *iv = (struct interval *) base;
	   iv < (struct interval *) limit;
	   ++iv)
	{
	  if (is_forwarded (iv) || is_padding (iv))
	    continue;

	  IGC_FIX12_PTR (ss, iv->left);
	  IGC_FIX12_PTR (ss, iv->right);
	  if (iv->up_obj)
	    IGC_FIX_OBJ (&iv->up.obj);
	  else
	    IGC_FIX12_PTR (ss, iv->up.interval);
	  IGC_FIX_OBJ (&iv->plist);
	}
    }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
interval_skip (mps_addr_t addr)
{
  return (char *) addr + sizeof (struct interval);
}

static mps_res_t
vector_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
    {
      for (struct interval *iv = (struct interval *) base;
	   iv < (struct interval *) limit;
	   ++iv)
	{
	  IGC_FIX_OBJ (&iv->plist);
	}
    }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
vector_skip (mps_addr_t addr)
{
  return (char *) addr + sizeof (struct interval);
}

#pragma GCC diagnostic pop


////////////////////////////////////////////////////////////////////////
///                             Finalization
////////////////////////////////////////////////////////////////////////

// ADDR is the address of an object registered for finalization with
// mps_finalize.  We have to find out in which pool ADDR lies, if any,
// because not all objects have a common header.  So, sometimes we have
// to use the pool to determine the object type.

static enum igc_type
type_of_addr (struct igc *gc, mps_addr_t addr)
{
  mps_pool_t pool;
  if (mps_addr_pool (&pool, gc->arena, addr))
    for (enum igc_type i = 0; i < IGC_TYPE_LAST; ++i)
      if (pool == gc->pool[i])
	return i;
  return IGC_TYPE_LAST;
}

static void
do_finalize (struct igc *gc, mps_addr_t addr)
{
  switch (type_of_addr (gc, addr))
    {
    case IGC_TYPE_CONS:
    case IGC_TYPE_SYMBOL:
    case IGC_TYPE_INTERVAL:
    case IGC_TYPE_STRING:
    case IGC_TYPE_STRING_DATA:
    case IGC_TYPE_VECTOR:
    case IGC_TYPE_LAST:
      break;
    }
}

static void
handle_messages (struct igc *gc)
{
  mps_message_type_t type;
  while (mps_message_queue_type (&type, gc->arena))
    {
      mps_message_t msg;
      if (!mps_message_get (&msg, gc->arena, type))
	continue;

      if (type == mps_message_type_finalization ())
	{
	  mps_addr_t addr;
	  mps_message_finalization_ref (&addr, gc->arena, msg);
	  do_finalize (gc, addr);
	}
      else if (type == mps_message_type_gc_start ())
	{
	  const char *why = mps_message_gc_start_why (gc->arena, msg);
	  fprintf (stderr, "*** IGC start %s\n", why);
	}

      mps_message_discard (gc->arena, msg);
    }
}

static void
enable_messages (struct igc *gc, bool enable)
{
  void (* fun) (mps_arena_t, mps_message_type_t)
    = enable ? mps_message_type_enable : mps_message_type_disable;
  fun (gc->arena, mps_message_type_finalization ());
  fun (gc->arena, mps_message_type_gc_start ());
}

void
igc_handle_messages (void)
{
  handle_messages (global_igc);
}

void
igc_on_idle (void)
{
  handle_messages (global_igc);
  mps_arena_step (global_igc->arena, 0.1, 0);
}


/***********************************************************************
			    Allocation
 ***********************************************************************/

static mps_ap_t
thread_ap (enum igc_type type)
{
  struct igc_thread_list *t = current_thread->gc_info;
  return t->d.ap[type];
}

void igc_break (void)
{
}

Lisp_Object
igc_make_cons (Lisp_Object car, Lisp_Object cdr)
{
  mps_ap_t ap = thread_ap (IGC_TYPE_CONS);
  size_t nbytes = sizeof (struct Lisp_Cons);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      struct Lisp_Cons *cons = p;
      cons->u.s.car = car;
      cons->u.s.u.cdr = cdr;
    }
  while (!mps_commit (ap, p, nbytes));
  return make_lisp_ptr (p, Lisp_Cons);
}

Lisp_Object
igc_alloc_symbol (void)
{
  mps_ap_t ap = thread_ap (IGC_TYPE_SYMBOL);
  size_t nbytes = sizeof (struct Lisp_Symbol);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      struct Lisp_Symbol *s = p;
      s->u.s.redirect = SYMBOL_PLAINVAL;
      s->u.s.name = Qnil;
      s->u.s.val.value = Qnil;
      s->u.s.function = Qnil;
      s->u.s.plist = Qnil;
      s->u.s.package = Qnil;
    }
  while (!mps_commit (ap, p, nbytes));
  return make_lisp_symbol ((struct Lisp_Symbol *) p);
}

static struct igc_sdata *
alloc_string_data (size_t nbytes)
{
  mps_ap_t ap = thread_ap (IGC_TYPE_STRING_DATA);
  size_t size = sizeof (struct igc_sdata) + nbytes;
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, size);
      IGC_CHECK_RES (res);
      // Initialize before we let it loose on the world.
      struct igc_sdata *s = p;
      s->object_end = (char *) s + size;
    }
  while (!mps_commit (ap, p, size));
  return p;
}

static Lisp_Object
igc_make_multibyte_string (size_t nchars, size_t nbytes, bool clear)
{
  struct igc_sdata *data = alloc_string_data (nbytes);
  if (clear)
    memset (sdata_contents (data), 0, nbytes);

  mps_ap_t ap = thread_ap (IGC_TYPE_STRING);
  size_t size = sizeof (struct Lisp_String);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, size);
      IGC_CHECK_RES (res);
      struct Lisp_String *s = p;
      s->u.s.size = nchars;
      s->u.s.size_byte = nbytes;
      s->u.s.intervals = NULL;
      s->u.s.data = sdata_contents (data);
    }
  while (!mps_commit (ap, p, size));
  return make_lisp_ptr (p, Lisp_String);
}

static struct interval *
igc_make_interval (void)
{
  mps_ap_t ap = thread_ap (IGC_TYPE_INTERVAL);
  size_t nbytes = sizeof (struct interval);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      igc_static_assert (NIL_IS_ZERO);
      memset (p, 0, nbytes);
    }
  while (!mps_commit (ap, p, nbytes));
  return p;
}

static struct Lisp_Vector *
igc_make_vectorlike (size_t nelems)
{
  mps_ap_t ap = thread_ap (IGC_TYPE_VECTOR);
  size_t nbytes = offsetof (struct Lisp_Vector, contents)
    + nelems * sizeof (Lisp_Object);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      igc_static_assert (NIL_IS_ZERO);
      memset (p, 0, nbytes);
    }
  while (!mps_commit (ap, p, nbytes));
  return p;
}


/***********************************************************************
			    Setup/Tear down
 ***********************************************************************/

/* In a debug pool, fill fencepost and freed objects with a
   byte pattern. This is ignored in non-debug pools.

   (lldb) memory read cons_ptr
   0x17735fe68: 66 72 65 65 66 72 65 65 66 72 65 65 66 72 65 65  freefreefreefree
   0x17735fe78: 66 72 65 65 66 72 65 65 66 72 65 65 66 72 65 65  freefreefreefree

   I think this feature is only supported in AMS pools. */

static mps_pool_debug_option_s debug_options = {
  "fence", 5,
  "free", 4,
};

struct igc_init {
  mps_class_t pool_class;
  size_t align;
  mps_fmt_scan_t scan;
  mps_fmt_skip_t skip;
};

static void
make_arena (struct igc *gc)
{
  mps_res_t res;
  MPS_ARGS_BEGIN (args)
    {
      res = mps_arena_create_k (&gc->arena, mps_arena_class_vm (), args);
    }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);

  // Generations
  mps_gen_param_s gens[]
    = { { 32000, 0.8 }, { 5 * 32009, 0.4 } };
  res = mps_chain_create (&gc->chain, gc->arena, ARRAYELTS (gens), gens);
  IGC_CHECK_RES (res);
}

static void
make_fmt (struct igc *gc, enum igc_type type, struct igc_init *init)
{
  mps_res_t res;
  MPS_ARGS_BEGIN (args)
    {
      MPS_ARGS_ADD (args, MPS_KEY_FMT_ALIGN, init->align);
      MPS_ARGS_ADD (args, MPS_KEY_FMT_HEADER_SIZE, 0);
      if (init->scan)
	MPS_ARGS_ADD (args, MPS_KEY_FMT_SCAN, init->scan);
      MPS_ARGS_ADD (args, MPS_KEY_FMT_SKIP, init->skip);
      MPS_ARGS_ADD (args, MPS_KEY_FMT_FWD, forward);
      MPS_ARGS_ADD (args, MPS_KEY_FMT_ISFWD, is_forwarded);
      MPS_ARGS_ADD (args, MPS_KEY_FMT_PAD, pad);
      res = mps_fmt_create_k (&gc->fmt[type], gc->arena, args);
    }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);
}

static void
make_pool (struct igc *gc, enum igc_type type, struct igc_init *init)
{
  mps_res_t res;
  MPS_ARGS_BEGIN (args)
    {
      MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &debug_options);
      MPS_ARGS_ADD (args, MPS_KEY_FORMAT, gc->fmt[type]);
      MPS_ARGS_ADD (args, MPS_KEY_CHAIN, gc->chain);
      MPS_ARGS_ADD (args, MPS_KEY_INTERIOR, 0);
      res = mps_pool_create_k (&gc->pool[type], gc->arena,
			       init->pool_class, args);
    }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);
}

static struct igc *
make_igc (void)
{
  struct igc *gc = xzalloc (sizeof *gc);
  make_arena (gc);

  struct igc_init inits[IGC_TYPE_LAST] = {
    { .pool_class = mps_class_amc (), .align = GCALIGNMENT,
      .scan = cons_scan, .skip = cons_skip },
    { .pool_class = mps_class_amc (), .align = GCALIGNMENT,
      .scan = symbol_scan, .skip = symbol_skip },
    { .pool_class = mps_class_amc (), .align = GCALIGNMENT,
      .scan = interval_scan, .skip = interval_skip },
    { .pool_class = mps_class_amc (), .align = GCALIGNMENT,
      .scan = string_scan, .skip = string_skip },
    { .pool_class = mps_class_amcz (),
      .align = max (sizeof (struct igc_fwd), sizeof (struct igc_pad)),
      .scan = NULL, .skip = string_data_skip },
    { .pool_class = mps_class_amc (), .align = GCALIGNMENT,
      .scan = vector_scan, .skip = vector_skip },
  };
  for (enum igc_type type = 0; type < IGC_TYPE_LAST; ++type)
    {
      struct igc_init *init = inits + type;
      make_fmt (gc, type, init);
      make_pool (gc, type, init);
    }

  add_static_roots (gc);
  enable_messages (gc, true);
  return gc;
}

static void
free_igc (struct igc *gc)
{
  free_all_threads (gc);
  for (enum igc_type type = 0; type < IGC_TYPE_LAST; ++type)
    {
      mps_pool_destroy (gc->pool[type]);
      mps_fmt_destroy (gc->fmt[type]);
    }
  destroy_all_roots (gc);
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
init_igc (void)
{
  global_igc = make_igc ();
  atexit (free_global_igc);
  add_main_thread ();
}

void
syms_of_igc (void)
{
}

#endif // HAVE_MPS
