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

/*
  - itree: buffer -> itree_tree, overlay <-> itree_node. The backref
    from the node to the overlay means we need to fix it, and it is
    currently xalloc'd. Alloc itree nodes from MPS.

  - terminal -> image_cache->images -> image, and image has refs,
    everything is xmalloc'd. Put images in MPS to scan them.
    Since images are managed manually, alloc from pool, don't xfree,
    don't finalize. Find refs to images, by makiing cache::images
    an ambig root.

  - frame -> face_cache::faces_by_id -> face -> font. font is pvec.
    face has refs. Same procedure as for images.

  - window -> glyph_matrix -> glyph_row[] -> glyph[], and for frames
    with glyph_pool. Could do the same as for faces, images, but make
    glyphs ambiguous roots for trying it out (igc_x*alloc etc).

  - hash_table -> key_and_value which is malloc'd. Rewrite so that
    key and value are separate. Must be done because of AWL restrictins.

  - compact_font_caches + inhibt. Can't be done this way, but isn't
    essential.
 */

// clang-format on

#include <config.h>
#include <limits.h>

#ifdef HAVE_MPS

# include <mps.h>
# include <mpsavm.h>
# include <mpscamc.h>
# include <mpscawl.h>
# include <mpslib.h>
# include <stdlib.h>
# include "lisp.h"
# include "buffer.h"
# include "dispextern.h"
# include "emacs-module.h"
# include "igc.h"
# include "intervals.h"
# include "itree.h"
# include "pdumper.h"
# include "termhooks.h"
# include "thread.h"

# ifndef USE_LSB_TAG
#  error "USE_LSB_TAG required"
# endif
# ifdef WIDE_EMACS_INT
#  error "WIDE_EMACS_INT not supported"
# endif
# if USE_STACK_LISP_OBJECTS
#  error "USE_STACK_LISP_OBJECTS not supported"
# endif

// #pragma GCC diagnostic ignored "-Wunused-function"

// Neither do I know what this is, nor is it supported on macOS.
# ifdef HAVE_TEXT_CONVERSION
#  error "HAVE_TEXT_CONVERSION not supported"
# endif

/* Note: Emacs will call allocation functions whlle aborting, which
   leads to interesting phenomena when an assertion fails inside a
   function called from MPS while holding a lock, and find that we
   already own the lock while allocatin.

   The fucntion signature must be that of mps_lib_assert_fail_t.  */

static void
igc_assert_fail (const char *file, unsigned line, const char *msg)
{
  die (msg, file, line);
}

# ifdef IGC_DEBUG
#  define igc_assert(expr)                         \
    if (!(expr))                                   \
      igc_assert_fail (__FILE__, __LINE__, #expr); \
    else
# else
#  define igc_assert(expr) (void) 9
# endif

# define igc_static_assert(x) verify (x)

# define IGC_TAG_MASK (~VALMASK)

/* Min and max addresses we got from MPS allocations. See fix_lisp_obj
   comments.  */
static mps_addr_t min_addr, max_addr;

static void
record_alloc (mps_addr_t addr, mps_word_t nbytes)
{
  if (min_addr == NULL || addr < min_addr)
    min_addr = addr;
  addr = (char *) addr + nbytes;
  if (max_addr == NULL || addr > max_addr)
    max_addr = addr;
}

static bool
is_mps (const mps_addr_t addr)
{
  return true;
  return addr >= min_addr && addr < max_addr;
}

enum
{
  IGC_ALIGN = GCALIGNMENT,
  IGC_ALIGN_DFLT = IGC_ALIGN,
};

static bool
is_aligned (const mps_addr_t addr)
{
  return ((mps_word_t) addr & IGC_TAG_MASK) == 0;
}

# define IGC_CHECK_RES(res) \
   if ((res) != MPS_RES_OK) \
     emacs_abort ();        \
   else

# define IGC_WITH_PARKED(gc)                        \
   for (int i = (mps_arena_park (gc->arena), 1); i; \
	i = (mps_arena_release (gc->arena), 0))

# define IGC_DEFINE_LIST(data)                                        \
   typedef struct data##_list                                         \
   {                                                                  \
     struct data##_list *next, *prev;                                 \
     data d;                                                          \
   } data##_list;                                                     \
                                                                      \
   static data##_list *data##_list_push (data##_list **head, data *d) \
   {                                                                  \
     data##_list *r = xzalloc (sizeof *r);                            \
     r->d = *d;                                                       \
     r->next = *head;                                                 \
     r->prev = NULL;                                                  \
     if (r->next)                                                     \
       r->next->prev = r;                                             \
     *head = r;                                                       \
     return r;                                                        \
   }                                                                  \
                                                                      \
   static void data##_list_remove (data *d, data##_list **head,       \
				   data##_list *r)                    \
   {                                                                  \
     if (r->next)                                                     \
       r->next->prev = r->prev;                                       \
     if (r->prev)                                                     \
       r->prev->next = r->next;                                       \
     else                                                             \
       *head = r->next;                                               \
     *d = r->d;                                                       \
     xfree (r);                                                       \
   }

struct igc_root
{
  struct igc *gc;
  mps_root_t root;
  void *start, *end;
};

typedef struct igc_root igc_root;
IGC_DEFINE_LIST (igc_root);

enum igc_obj_type
{
  IGC_OBJ_INVALID,
  IGC_OBJ_PAD,
  IGC_OBJ_FWD,
  IGC_OBJ_CONS,
  IGC_OBJ_SYMBOL,
  IGC_OBJ_INTERVAL,
  IGC_OBJ_STRING,
  IGC_OBJ_STRING_DATA,
  IGC_OBJ_VECTOR,
  IGC_OBJ_ITREE_NODE,
  IGC_OBJ_IMAGE,
  IGC_OBJ_FACE,
  IGC_OBJ_FLOAT,
  IGC_OBJ_WEAK,
  IGC_OBJ_LAST
};

/* We always have a header which makes it possible to have an
   address-independant hash, which is (a) much easier to handle than MPS
   location dependencies, and (b) makes it possible to implement sxhash
   variants in a way that works even if GCs happen between calls.  */
enum
{
  IGC_TYPE_BITS = 8,
  IGC_HASH_BITS = 24,
  IGC_SIZE_BITS = 32
};

struct igc_header
{
  enum igc_obj_type obj_type : IGC_TYPE_BITS;
  unsigned hash : IGC_HASH_BITS;
  // Could let this count in words...
  mps_word_t obj_size : IGC_SIZE_BITS;
};

igc_static_assert (sizeof (struct igc_header) == 8);

struct igc_fwd
{
  struct igc_header header;
  mps_addr_t new_base_addr;
};

static mps_addr_t
client_to_base (mps_addr_t client_addr)
{
  return (char *) client_addr - sizeof (struct igc_header);
}

static mps_addr_t
base_to_client (mps_addr_t base_addr)
{
  return (char *) base_addr + sizeof (struct igc_header);
}

static size_t
igc_round (size_t nbytes, size_t align)
{
  return ROUNDUP (nbytes, align);
}

static size_t
obj_size (size_t nbytes)
{
  nbytes += sizeof (struct igc_header);
  nbytes = max (nbytes, sizeof (struct igc_fwd));
  nbytes = igc_round (nbytes, IGC_ALIGN_DFLT);
  return nbytes;
}

struct igc_thread
{
  struct igc *gc;
  mps_thr_t thr;
  void *stack_start;
  mps_ap_t dflt_ap;
  mps_ap_t leaf_ap;
  mps_ap_t weak_strong_ap;
  mps_ap_t weak_weak_ap;
};

typedef struct igc_thread igc_thread;
IGC_DEFINE_LIST (igc_thread);

struct igc
{
  mps_arena_t arena;
  mps_chain_t chain;
  mps_fmt_t dflt_fmt;
  mps_pool_t dflt_pool;
  mps_fmt_t leaf_fmt;
  mps_pool_t leaf_pool;
  mps_fmt_t weak_fmt;
  mps_pool_t weak_pool;
  struct igc_root_list *roots;
  struct igc_thread_list *threads;
};

static struct igc *global_igc;

static struct igc_root_list *
register_root (struct igc *gc, mps_root_t root, void *start, void *end)
{
  struct igc_root r = { .gc = gc, .root = root, .start = start, .end = end };
  return igc_root_list_push (&gc->roots, &r);
}

static mps_root_t
deregister_root (struct igc_root_list *r)
{
  struct igc_root root;
  igc_root_list_remove (&root, &r->d.gc->roots, r);
  return root.root;
}

static void
destroy_root (struct igc_root_list *r)
{
  mps_root_destroy (deregister_root (r));
}

static void
destroy_all_roots (struct igc *gc)
{
  while (gc->roots)
    destroy_root (gc->roots);
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

static mps_res_t
create_weak_ap (mps_ap_t *ap, struct igc_thread *t, bool weak)
{
  struct igc *gc = t->gc;
  mps_res_t res;
  mps_pool_t pool = gc->weak_pool;
  MPS_ARGS_BEGIN (args)
  {
    MPS_ARGS_ADD (args, MPS_KEY_RANK,
		  weak ? mps_rank_weak () : mps_rank_exact ());
    res = mps_ap_create_k (ap, pool, args);
  }
  MPS_ARGS_END (args);
  return res;
}

static void
create_thread_aps (struct igc_thread *t)
{
  struct igc *gc = t->gc;
  mps_res_t res;
  res = mps_ap_create_k (&t->dflt_ap, gc->dflt_pool, mps_args_none);
  IGC_CHECK_RES (res);
  res = mps_ap_create_k (&t->leaf_ap, gc->leaf_pool, mps_args_none);
  IGC_CHECK_RES (res);
  res = create_weak_ap (&t->weak_strong_ap, t, false);
  IGC_CHECK_RES (res);
  res = create_weak_ap (&t->weak_weak_ap, t, true);
  IGC_CHECK_RES (res);
}

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

# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wunused-variable"

static mps_res_t
fix_lisp_obj (mps_ss_t ss, Lisp_Object *pobj)
{
  MPS_SCAN_BEGIN (ss)
  {
    mps_word_t *p = (mps_word_t *) pobj;
    mps_word_t word = *p;
    mps_word_t tag = word & IGC_TAG_MASK;

    if (tag == Lisp_Int0 || tag == Lisp_Int1)
      return MPS_RES_OK;
    else if (tag == Lisp_Type_Unused0)
      emacs_abort ();

    if (tag == Lisp_Symbol)
      {
	ptrdiff_t off = word ^ tag;
	mps_addr_t client = (mps_addr_t) ((char *) lispsym + off);
	if (is_mps (client))
	  {
	    mps_addr_t base = client_to_base (client);
	    if (MPS_FIX1 (ss, base))
	      {
		mps_res_t res = MPS_FIX2 (ss, &base);
		if (res != MPS_RES_OK)
		  return res;
		client = base_to_client (base);
		ptrdiff_t new_off = (char *) client - (char *) lispsym;
		*p = new_off | tag;
	      }
	  }
      }
    else
      {
	/* I have encountered cases where MPS_FIX1 returns true, but the
	   reference is somewhere completely off, so that MPS_FIX2 asserts.
	   IOW, MPS_FIX1 has undefined behavior if called on an address that
	   is not in the arena.  */
	mps_addr_t client = (mps_addr_t) (word ^ tag);
	if (is_mps (client))
	  {
	    mps_addr_t base = client_to_base (client);
	    if (MPS_FIX1 (ss, base))
	      {
		mps_res_t res = MPS_FIX2 (ss, &base);
		if (res != MPS_RES_OK)
		  return res;
		client = base_to_client (base);
		*p = (mps_word_t) client | tag;
	      }
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_raw (mps_ss_t ss, mps_addr_t *p)
{
  MPS_SCAN_BEGIN (ss)
  {
    mps_addr_t client = *p;
    if (is_mps (client) && is_aligned (client))
      {
	mps_addr_t base = client_to_base (client);
	if (MPS_FIX1 (ss, base))
	  {
	    mps_res_t res = MPS_FIX2 (ss, &base);
	    if (res != MPS_RES_OK)
	      return res;
	    *p = base_to_client (base);
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

# define IGC_FIX12_OBJ(ss, p)                           \
   do                                                   \
     {                                                  \
       mps_res_t res;                                   \
       MPS_FIX_CALL (ss, res = fix_lisp_obj (ss, (p))); \
       if (res != MPS_RES_OK)                           \
	 return res;                                    \
     }                                                  \
   while (0)

# define IGC_FIX12_RAW(ss, p)                                     \
   do                                                             \
     {                                                            \
       mps_res_t res;                                             \
       MPS_FIX_CALL (ss, res = fix_raw (ss, (mps_addr_t *) (p))); \
       if (res != MPS_RES_OK)                                     \
	 return res;                                              \
     }                                                            \
   while (0)

# define IGC_FIX12_NOBJS(ss, a, n)                            \
   do                                                         \
     {                                                        \
       mps_res_t res;                                         \
       MPS_FIX_CALL ((ss), res = fix_array ((ss), (a), (n))); \
       if (res != MPS_RES_OK)                                 \
	 return res;                                          \
     }                                                        \
   while (0)

# define IGC_FIX_CALL(ss, expr)         \
   do                                   \
     {                                  \
       mps_res_t res;                   \
       MPS_FIX_CALL (ss, res = (expr)); \
       if (res != MPS_RES_OK)           \
	 return res;                    \
     }                                  \
   while (0)

# define IGC_FIX_CALL_FN(ss, type, client_addr, fn) \
   do                                               \
     {                                              \
       type *obj_ = (type *) client_addr;           \
       mps_res_t res;                               \
       MPS_FIX_CALL (ss, res = fn (ss, obj_));      \
       if (res != MPS_RES_OK)                       \
	 return res;                                \
     }                                              \
   while (0)

static mps_res_t
fix_array (mps_ss_t ss, Lisp_Object *array, size_t n)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (size_t i = 0; i < n; ++i)
      IGC_FIX12_OBJ (ss, &array[i]);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_staticvec (mps_ss_t ss, void *start, void *end, void *closure)
{
  igc_assert (start == staticvec);
  MPS_SCAN_BEGIN (ss)
  {
    for (int i = 0; i < staticidx; ++i)
      {
	igc_assert (staticvec[i] != NULL);
	/* staticvec is declared as having pointers to const
	   Lisp_Object, for an unknown reason.  */
	IGC_FIX12_OBJ (ss, (Lisp_Object *) staticvec[i]);
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_symbol (mps_ss_t ss, struct Lisp_Symbol *sym)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_OBJ (ss, &sym->u.s.name);
    if (sym->u.s.redirect == SYMBOL_PLAINVAL)
      IGC_FIX12_OBJ (ss, &sym->u.s.val.value);
    IGC_FIX12_OBJ (ss, &sym->u.s.function);
    IGC_FIX12_OBJ (ss, &sym->u.s.plist);
    IGC_FIX12_OBJ (ss, &sym->u.s.package);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_lispsym (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (struct Lisp_Symbol *sym = start; sym < (struct Lisp_Symbol *) end;
	 ++sym)
      IGC_FIX_CALL (ss, fix_symbol (ss, sym));
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Scan the area of memory [START, END) ambiguously. In general,
   references may be either tagged words or pointers. This is used for
   blocks allocated with malloc and thread stacks. */

static mps_res_t
scan_ambig (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (mps_word_t *p = start; p < (mps_word_t *) end; ++p)
      {
	mps_word_t word = *p;
	mps_word_t tag = word & IGC_TAG_MASK;

	/* If the references in the object being scanned are
	   ambiguous then MPS_FIX2() does not update the
	   reference (because it can’t know if it’s a
	   genuine reference). The MPS handles an ambiguous
	   reference by pinning the block pointed to so that
	   it cannot move. */
	mps_addr_t ref = (mps_addr_t) word;
	mps_res_t res = MPS_FIX12 (ss, &ref);
	if (res != MPS_RES_OK)
	  return res;

	switch (tag)
	  {
	  case Lisp_Int0:
	  case Lisp_Int1:
	  case Lisp_Type_Unused0:
	    break;

	  case Lisp_Symbol:
	    {
	      ptrdiff_t off = word ^ tag;
	      ref = (mps_addr_t) ((char *) lispsym + off);
	      res = MPS_FIX12 (ss, &ref);
	      if (res != MPS_RES_OK)
		return res;
	    }
	    break;

	  default:
	    ref = (mps_addr_t) (word ^ tag);
	    res = MPS_FIX12 (ss, &ref);
	    if (res != MPS_RES_OK)
	      return res;
	    break;
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/***********************************************************************
			 Default pad, fwd, ...
 ***********************************************************************/

static void
dflt_pad (mps_addr_t base_addr, mps_word_t nbytes)
{
  igc_assert (nbytes > 0);
  struct igc_header *h = base_addr;
  h->obj_type = IGC_OBJ_PAD;
  h->obj_size = nbytes;
  igc_assert (h->obj_size >= sizeof (struct igc_header));
}

static void
dflt_fwd (mps_addr_t old_base_addr, mps_addr_t new_base_addr)
{
  struct igc_header *h = old_base_addr;
  igc_assert (h->obj_size >= sizeof (struct igc_fwd));
  igc_assert (h->obj_type != IGC_OBJ_PAD);
  struct igc_fwd *f = old_base_addr;
  f->header.obj_type = IGC_OBJ_FWD;
  f->new_base_addr = new_base_addr;
}

static mps_addr_t
is_dflt_fwd (mps_addr_t base_addr)
{
  struct igc_fwd *f = base_addr;
  if (f->header.obj_type == IGC_OBJ_FWD)
    return f->new_base_addr;
  return NULL;
}

static mps_addr_t
dflt_skip (mps_addr_t base_addr)
{
  struct igc_header *h = base_addr;
  mps_addr_t next = (char *) base_addr + h->obj_size;
  igc_assert (h->obj_size >= sizeof (struct igc_header));
  return next;
}

static mps_res_t
fix_string (mps_ss_t ss, struct Lisp_String *s)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_RAW (ss, &s->u.s.data);
    IGC_FIX12_RAW (ss, &s->u.s.intervals);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_interval (mps_ss_t ss, struct interval *iv)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_RAW (ss, &iv->left);
    IGC_FIX12_RAW (ss, &iv->right);
    if (iv->up_obj)
      IGC_FIX12_OBJ (ss, &iv->up.obj);
    else
      IGC_FIX12_RAW (ss, iv->up.interval);
    IGC_FIX12_OBJ (ss, &iv->plist);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_itree_node (mps_ss_t ss, struct itree_node *n)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_RAW (ss, &n->parent);
    IGC_FIX12_RAW (ss, &n->left);
    IGC_FIX12_RAW (ss, &n->right);
    IGC_FIX12_OBJ (ss, &n->data);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_image (mps_ss_t ss, struct image *i)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_OBJ (ss, &i->spec);
    IGC_FIX12_OBJ (ss, &i->dependencies);
    IGC_FIX12_OBJ (ss, &i->lisp_data);
    IGC_FIX12_RAW (ss, &i->next);
    IGC_FIX12_RAW (ss, &i->prev);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_face (mps_ss_t ss, struct face *f)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_NOBJS (ss, f->lface, ARRAYELTS (f->lface));
    IGC_FIX12_RAW (ss, &f->font);
    IGC_FIX12_RAW (ss, &f->next);
    IGC_FIX12_RAW (ss, &f->prev);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_weak (mps_ss_t ss, mps_addr_t base)
{
  MPS_SCAN_BEGIN (ss) { igc_assert (!"fix_weak"); }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_cons (mps_ss_t ss, struct Lisp_Cons *cons)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_OBJ (ss, &cons->u.s.car);
    IGC_FIX12_OBJ (ss, &cons->u.s.u.cdr);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t fix_vector (mps_ss_t ss, struct Lisp_Vector *v);

static mps_res_t
dflt_scan (mps_ss_t ss, mps_addr_t base_start, mps_addr_t base_limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (mps_addr_t base = base_start; base < base_limit;
	 base = dflt_skip (base))
      {
	mps_addr_t client = base_to_client (base);
	struct igc_header *header = base;
	switch (header->obj_type)
	  {
	  case IGC_OBJ_INVALID:
	    emacs_abort ();

	  case IGC_OBJ_PAD:
	  case IGC_OBJ_FWD:
	    continue;

	  case IGC_OBJ_CONS:
	    IGC_FIX_CALL_FN (ss, struct Lisp_Cons, client, fix_cons);
	    break;

	  case IGC_OBJ_STRING_DATA:
	  case IGC_OBJ_FLOAT:
	  case IGC_OBJ_LAST:
	    emacs_abort ();

	  case IGC_OBJ_SYMBOL:
	    IGC_FIX_CALL_FN (ss, struct Lisp_Symbol, client, fix_symbol);
	    break;

	  case IGC_OBJ_INTERVAL:
	    IGC_FIX_CALL_FN (ss, struct interval, client, fix_interval);
	    break;

	  case IGC_OBJ_STRING:
	    IGC_FIX_CALL_FN (ss, struct Lisp_String, client, fix_string);
	    break;

	  case IGC_OBJ_VECTOR:
	    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, client, fix_vector);
	    break;

	  case IGC_OBJ_ITREE_NODE:
	    IGC_FIX_CALL_FN (ss, struct itree_node, client, fix_itree_node);
	    break;

	  case IGC_OBJ_IMAGE:
	    IGC_FIX_CALL_FN (ss, struct image, client, fix_image);
	    break;

	  case IGC_OBJ_FACE:
	    IGC_FIX_CALL_FN (ss, struct face, client, fix_face);
	    break;

	  case IGC_OBJ_WEAK:
	    IGC_FIX_CALL_FN (ss, mps_word_t, client, fix_weak);
	    break;
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/***********************************************************************
				Vectors
 ***********************************************************************/

static enum pvec_type
pseudo_vector_type (const struct Lisp_Vector *v)
{
  return PSEUDOVECTOR_TYPE (v);
}

static mps_res_t
fix_vectorlike (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    ptrdiff_t size = v->header.size;
    if (size & PSEUDOVECTOR_FLAG)
      size &= PSEUDOVECTOR_SIZE_MASK;
    struct igc_header *h = client_to_base (v);
    igc_assert (h->obj_size >= size * word_size);
    IGC_FIX12_NOBJS (ss, v->contents, size);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_buffer (mps_ss_t ss, struct buffer *b)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, b, fix_vectorlike);
    IGC_FIX12_RAW (ss, &b->text->intervals);
    IGC_FIX12_RAW (ss, &b->text->markers);
    IGC_FIX12_RAW (ss, &b->own_text.intervals);
    IGC_FIX12_RAW (ss, &b->own_text.markers);
    IGC_FIX12_RAW (ss, &b->base_buffer);
    if (b->overlays)
      IGC_FIX12_RAW (ss, &b->overlays->root);
    // FIXME: special handling of undo_list?
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_frame (mps_ss_t ss, struct frame *f)
{
  MPS_SCAN_BEGIN (ss)
  {
    // output_data;
    // terminal
    // face_cache *
    // glyph_pool
    // glyph matrices
    // struct font_driver_list *font_driver_list;
    // struct text_conversion_state conversion;
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, f, fix_vectorlike);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_window (mps_ss_t ss, struct window *w)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, w, fix_vectorlike);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_hash_table (mps_ss_t ss, struct Lisp_Hash_Table *h)
{
  MPS_SCAN_BEGIN (ss)
  {
    // FIXME: weak */
    IGC_FIX12_NOBJS (ss, h->key, h->table_size);
    IGC_FIX12_NOBJS (ss, h->value, h->table_size);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_char_table (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    int size = v->header.size & PSEUDOVECTOR_SIZE_MASK;
    enum pvec_type type = pseudo_vector_type (v);
    int idx = type == PVEC_SUB_CHAR_TABLE ? SUB_CHAR_TABLE_OFFSET : 0;
    for (int i = idx; i < size; ++i)
      IGC_FIX12_OBJ (ss, &v->contents[i]);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_overlay (mps_ss_t ss, struct Lisp_Overlay *o)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_RAW (ss, &o->buffer);
    IGC_FIX12_OBJ (ss, &o->plist);
    IGC_FIX12_RAW (ss, &o->interval);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_subr (mps_ss_t ss, struct Lisp_Subr *s)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_OBJ (ss, &s->command_modes);
# ifdef HAVE_NATIVE_COMP
    IGC_FIX12_OBJ (ss, &s->intspec.native);
    IGC_FIX12_OBJ (ss, &s->command_modes);
    IGC_FIX12_OBJ (ss, &s->native_comp_u);
    IGC_FIX12_OBJ (ss, &s->lambda_list);
    IGC_FIX12_OBJ (ss, &s->type);
# endif
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_misc_ptr (mps_ss_t ss, struct Lisp_Misc_Ptr *p)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, p, fix_vectorlike);
    IGC_FIX12_RAW (ss, &p->pointer);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_user_ptr (mps_ss_t ss, struct Lisp_User_Ptr *p)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, p, fix_vectorlike);
    IGC_FIX12_RAW (ss, &p->p);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_thread (mps_ss_t ss, struct thread_state *s)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, s, fix_vectorlike);
    IGC_FIX12_RAW (ss, &s->m_current_buffer);
    IGC_FIX12_RAW (ss, &s->next_thread);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_mutex (mps_ss_t ss, struct Lisp_Mutex *m)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, m, fix_vectorlike);
    IGC_FIX12_RAW (ss, &m->name);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_terminal (mps_ss_t ss, struct terminal *t)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, t, fix_vectorlike);
    IGC_FIX12_RAW (ss, &t->next_terminal);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_marker (mps_ss_t ss, struct Lisp_Marker *m)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_RAW (ss, &m->buffer);
    IGC_FIX12_RAW (ss, &m->next);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_finalizer (mps_ss_t ss, struct Lisp_Finalizer *f)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, f, fix_vectorlike);
    igc_assert (!"PVEC_FINALIZER");
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

# ifdef HAVE_XWIDGETS

static mps_res_t
fix_xwidget (mps_ss_t ss, struct xwidget *w)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, w, fix_vectorlike);
    igc_assert (!"xwidget");
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_xwidget_view (mps_ss_t ss, struct xwidget_view *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_vectorlike);
    igc_assert (!"xwidget_view");
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

# endif // HAVE_XWIDGETS

static mps_res_t
fix_other (mps_ss_t ss, void *o)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, o, fix_vectorlike);
    // Not used on macOS. Some scroll bar stuff in w32?
    igc_assert (!"other");
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Note that there is a small window after committing a vectorlike
   allocation where the object is zeroed, and so the vector header is
   also zero.  This doesn't have an adverse effect. */

static mps_res_t
fix_vector (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    switch (pseudo_vector_type (v))
      {
      case PVEC_BUFFER:
	IGC_FIX_CALL_FN (ss, struct buffer, v, fix_buffer);
	break;

      case PVEC_FRAME:
	IGC_FIX_CALL_FN (ss, struct frame, v, fix_frame);
	break;

      case PVEC_WINDOW:
	IGC_FIX_CALL_FN (ss, struct window, v, fix_window);
	break;

      case PVEC_HASH_TABLE:
	IGC_FIX_CALL_FN (ss, struct Lisp_Hash_Table, v, fix_hash_table);
	break;

      case PVEC_CHAR_TABLE:
      case PVEC_SUB_CHAR_TABLE:
	IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_char_table);
	break;

      case PVEC_BOOL_VECTOR:
	break;

      case PVEC_OVERLAY:
	IGC_FIX_CALL_FN (ss, struct Lisp_Overlay, v, fix_overlay);
	break;

      case PVEC_SUBR:
	IGC_FIX_CALL_FN (ss, struct Lisp_Subr, v, fix_subr);
	break;

      case PVEC_FREE:
	emacs_abort ();

      case PVEC_FINALIZER:
	IGC_FIX_CALL_FN (ss, struct Lisp_Finalizer, v, fix_finalizer);
	break;

      case PVEC_OTHER:
	IGC_FIX_CALL_FN (ss, void, v, fix_other);
	break;

      case PVEC_MISC_PTR:
	IGC_FIX_CALL_FN (ss, struct Lisp_Misc_Ptr, v, fix_misc_ptr);
	break;

      case PVEC_USER_PTR:
	IGC_FIX_CALL_FN (ss, struct Lisp_User_Ptr, v, fix_user_ptr);
	break;

# ifdef HAVE_XWIDGETS
      case PVEC_XWIDGET:
	IGC_FIX_CALL_FN (ss, struct xwidget, v, fix_xwidget);
	break;

      case PVEC_XWIDGET_VIEW:
	IGC_FIX_CALL_FN (ss, struct xwidget_view, v, fix_xwidget_view);
	break;
# endif

      case PVEC_THREAD:
	IGC_FIX_CALL_FN (ss, struct thread_state, v, fix_thread);
	break;

      case PVEC_MUTEX:
	IGC_FIX_CALL_FN (ss, struct Lisp_Mutex, v, fix_mutex);
	break;

      case PVEC_TERMINAL:
	IGC_FIX_CALL_FN (ss, struct terminal, v, fix_terminal);
	break;

      case PVEC_MARKER:
	IGC_FIX_CALL_FN (ss, struct Lisp_Marker, v, fix_marker);
	break;

      default:
	IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_vectorlike);
	break;
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

# pragma GCC diagnostic pop

static igc_root_list *
create_ambig_root (struct igc *gc, void *start, void *end)
{
  mps_root_t root;
  mps_res_t res = mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
					start, end, scan_ambig, 0);
  IGC_CHECK_RES (res);
  return register_root (gc, root, start, end);
}

static mps_rm_t
root_mode_inner (void)
{
  /* Issue https://github.com/Ravenbrook/mps/issues/285  */
  if (MPS_RM_PROT == MPS_RM_PROT_INNER)
    return 0;
  return MPS_RM_PROT_INNER + MPS_RM_PROT;
}

static void
create_staticvec_root (struct igc *gc)
{
  void *start = staticvec, *end = staticvec + ARRAYELTS (staticvec);
  mps_root_t root;
  mps_res_t res = mps_root_create_area (&root, gc->arena, mps_rank_exact (),
					root_mode_inner (), start, end,
					scan_staticvec, NULL);
  IGC_CHECK_RES (res);
  register_root (gc, root, start, end);
}

static void
create_lispsym_root (struct igc *gc)
{
  void *start = lispsym, *end = lispsym + ARRAYELTS (lispsym);
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, mps_rank_exact (),
			    root_mode_inner (), start, end, scan_lispsym, NULL);
  IGC_CHECK_RES (res);
  register_root (gc, root, start, end);
}

static void
create_buffer_root (struct igc *gc, struct buffer *b)
{
  void *start = &b->name_, *end = &b->own_text;
  create_ambig_root (gc, start, end);
}

static void
create_terminal_list_root (struct igc *gc)
{
  void *start = &terminal_list;
  void *end = (char *) start + sizeof (terminal_list);
  create_ambig_root (gc, start, end);
}

static void
create_static_roots (struct igc *gc)
{
  create_buffer_root (gc, &buffer_defaults);
  create_buffer_root (gc, &buffer_local_symbols);
  create_staticvec_root (gc);
  create_lispsym_root (gc);
  create_terminal_list_root (gc);
}

static void
create_thread_root (struct igc_thread_list *t)
{
  struct igc *gc = t->d.gc;
  mps_root_t root;
  mps_res_t res
    = mps_root_create_thread_scanned (&root, gc->arena, mps_rank_ambig (), 0,
				      t->d.thr, scan_ambig, 0,
				      t->d.stack_start);
  IGC_CHECK_RES (res);
  register_root (gc, root, t->d.stack_start, NULL);
}

void *
igc_thread_add (const void *stack_start)
{
  mps_thr_t thr;
  mps_res_t res = mps_thread_reg (&thr, global_igc->arena);
  IGC_CHECK_RES (res);
  struct igc_thread_list *t
    = register_thread (global_igc, thr, (void *) stack_start);
  create_thread_root (t);
  create_thread_aps (&t->d);
  return t;
}

void
igc_thread_remove (void *info)
{
  struct igc_thread_list *t = info;
  mps_ap_destroy (t->d.dflt_ap);
  mps_ap_destroy (t->d.leaf_ap);
  mps_ap_destroy (t->d.weak_strong_ap);
  mps_ap_destroy (t->d.weak_weak_ap);
  mps_thread_dereg (deregister_thread (t));
}

static void
add_main_thread (void)
{
  igc_assert (current_thread->gc_info == NULL);
  current_thread->gc_info = igc_thread_add (stack_bottom);
}

void
igc_on_pdump_loaded (void)
{
  void *start = (void *) dump_public.start;
  void *end = (void *) dump_public.end;
  create_ambig_root (global_igc, start, end);
}

void *
igc_on_grow_rdstack (void *info, void *start, void *end)
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

static igc_root_list *
find_root (void *start)
{
  for (igc_root_list *r = global_igc->roots; r; r = r->next)
    if (r->d.start == start)
      return r;
  return NULL;
}

void *
igc_xzalloc (size_t size)
{
  void *p = xzalloc (size);
  create_ambig_root (global_igc, p, (char *) p + size);
  return p;
}

void *
igc_xmalloc (size_t size)
{
  void *p = xmalloc (size);
  create_ambig_root (global_igc, p, (char *) p + size);
  return p;
}

void
igc_xfree (void *p)
{
  if (p == NULL)
    return;
  struct igc_root_list *r = find_root (p);
  igc_assert (r != NULL);
  destroy_root (r);
  xfree (p);
}

void *
igc_xpalloc (void *pa, ptrdiff_t *nitems, ptrdiff_t nitems_incr_min,
	     ptrdiff_t nitems_max, ptrdiff_t item_size)
{
  IGC_WITH_PARKED (global_igc)
  {
    if (pa)
      {
	struct igc_root_list *r = find_root (pa);
	if (r)
	  destroy_root (r);
      }
    pa = xpalloc (pa, nitems, nitems_incr_min, nitems_max, item_size);
    char *end = (char *) pa + *nitems * item_size;
    create_ambig_root (global_igc, pa, end);
  }
  return pa;
}

void *
igc_xnrealloc (void *pa, ptrdiff_t nitems, ptrdiff_t item_size)
{
  IGC_WITH_PARKED (global_igc)
  {
    if (pa)
      {
	struct igc_root_list *r = find_root (pa);
	if (r)
	  destroy_root (r);
      }
    pa = xnrealloc (pa, nitems, item_size);
    char *end = (char *) pa + nitems * item_size;
    create_ambig_root (global_igc, pa, end);
  }
  return pa;
}

static bool
type_of_addr (struct igc *gc, mps_addr_t base_addr, enum igc_obj_type *obj_type)
{
  mps_pool_t pool;
  if (!mps_addr_pool (&pool, gc->arena, base_addr))
    return false;

  struct igc_header *h = base_addr;
  *obj_type = h->obj_type;
  return true;
}

static void
finalize (struct igc *gc, mps_addr_t base_addr)
{
  enum igc_obj_type obj_type;
  if (!type_of_addr (gc, base_addr, &obj_type))
    emacs_abort ();
  switch (obj_type)
    {
    case IGC_OBJ_INVALID:
    case IGC_OBJ_PAD:
    case IGC_OBJ_FWD:
    case IGC_OBJ_CONS:
    case IGC_OBJ_SYMBOL:
    case IGC_OBJ_INTERVAL:
    case IGC_OBJ_STRING:
    case IGC_OBJ_STRING_DATA:
    case IGC_OBJ_VECTOR:
    case IGC_OBJ_ITREE_NODE:
    case IGC_OBJ_IMAGE:
    case IGC_OBJ_FACE:
    case IGC_OBJ_FLOAT:
    case IGC_OBJ_WEAK:
    case IGC_OBJ_LAST:
      break;
    }
}

/* Process MPS messages. This should be extended to handle messages only
   for a certain amoount of time. See mps_clock_t, mps_clock, and
   mps_clocks_per_sec functions.  */

static void
process_messages (struct igc *gc)
{
  mps_message_type_t type;
  while (mps_message_queue_type (&type, gc->arena))
    {
      mps_message_t msg;
      if (!mps_message_get (&msg, gc->arena, type))
	continue;

      if (type == mps_message_type_finalization ())
	{
	  mps_addr_t base_addr;
	  mps_message_finalization_ref (&base_addr, gc->arena, msg);
	  finalize (gc, base_addr);
	}
      else if (type == mps_message_type_gc_start ())
	{
	  const char *why = mps_message_gc_start_why (gc->arena, msg);
	  fprintf (stderr, "*** MPS GC start: %s\n", why);
	}

      mps_message_discard (gc->arena, msg);
    }
}

static void
enable_messages (struct igc *gc, bool enable)
{
  void (*fun) (mps_arena_t, mps_message_type_t)
    = enable ? mps_message_type_enable : mps_message_type_disable;
  fun (gc->arena, mps_message_type_finalization ());
  fun (gc->arena, mps_message_type_gc_start ());
}

void
igc_process_messages (void)
{
  process_messages (global_igc);
}

void
igc_on_idle (void)
{
  process_messages (global_igc);
  mps_arena_step (global_igc->arena, 0.1, 0);
}

static mps_ap_t
thread_ap (enum igc_obj_type type)
{
  struct igc_thread_list *t = current_thread->gc_info;
  switch (type)
    {
    case IGC_OBJ_INVALID:
    case IGC_OBJ_PAD:
    case IGC_OBJ_FWD:
    case IGC_OBJ_LAST:
    case IGC_OBJ_WEAK:
      emacs_abort ();

    case IGC_OBJ_CONS:
    case IGC_OBJ_SYMBOL:
    case IGC_OBJ_INTERVAL:
    case IGC_OBJ_STRING:
    case IGC_OBJ_VECTOR:
    case IGC_OBJ_ITREE_NODE:
    case IGC_OBJ_IMAGE:
    case IGC_OBJ_FACE:
      return t->d.dflt_ap;

    case IGC_OBJ_STRING_DATA:
    case IGC_OBJ_FLOAT:
      return t->d.leaf_ap;
    }
}

void
igc_break (void)
{
}

static unsigned
obj_hash (void)
{
  static unsigned obj_count = 0;
  if (obj_count == (1 << IGC_HASH_BITS))
    obj_count = 0;
  return obj_count++;
}

size_t
igc_hash (Lisp_Object key)
{
  mps_word_t word = XLI (key);
  mps_word_t tag = word & IGC_TAG_MASK;
  switch (tag)
    {
    case Lisp_Type_Unused0:
      emacs_abort ();

    case Lisp_Int0:
    case Lisp_Int1:
      return word;

    case Lisp_Symbol:
      {
	ptrdiff_t off = word ^ tag;
	mps_addr_t sym = (mps_addr_t) ((char *) lispsym + off);
	if (c_symbol_p (sym))
	  return word;
	struct igc_header *h = client_to_base (sym);
	return h->hash;
      }

    case Lisp_String:
    case Lisp_Vectorlike:
    case Lisp_Cons:
    case Lisp_Float:
      {
	mps_addr_t client = (mps_addr_t) (word ^ tag);
	if (is_mps (client))
	  {
	    struct igc_header *h = client_to_base (client);
	    return h->hash;
	  }
	return word;
      }
    }

  emacs_abort ();
}

static mps_addr_t
alloc (size_t size, enum igc_obj_type type)
{
  mps_ap_t ap = thread_ap (type);
  mps_addr_t p, obj;
  size = obj_size (size);
  do
    {
      mps_res_t res = mps_reserve (&p, ap, size);
      IGC_CHECK_RES (res);
      // Object _must_ have valid contents before commit
      memclear (p, size);
      struct igc_header *h = p;
      h->obj_type = type;
      h->hash = obj_hash ();
      igc_assert (size < ((size_t) 1 << IGC_SIZE_BITS));
      h->obj_size = size;
      obj = base_to_client (p);
    }
  while (!mps_commit (ap, p, size));
  record_alloc (p, size);
  return obj;
}

Lisp_Object
igc_make_cons (Lisp_Object car, Lisp_Object cdr)
{
  struct Lisp_Cons *cons = alloc (sizeof *cons, IGC_OBJ_CONS);
  cons->u.s.car = car;
  cons->u.s.u.cdr = cdr;
  return make_lisp_ptr (cons, Lisp_Cons);
}

Lisp_Object
igc_alloc_symbol (void)
{
  struct Lisp_Symbol *sym = alloc (sizeof *sym, IGC_OBJ_SYMBOL);
  return make_lisp_symbol (sym);
}

Lisp_Object
igc_make_float (double val)
{
  struct Lisp_Float *f = alloc (sizeof *f, IGC_OBJ_FLOAT);
  f->u.data = val;
  return make_lisp_ptr (f, Lisp_Float);
}

static unsigned char *
alloc_string_data (size_t nbytes, bool clear)
{
  unsigned char *data = alloc (nbytes + 1, IGC_OBJ_STRING_DATA);
  data[nbytes] = 0;
  return data;
}

/* Reallocate multibyte STRING data when a single character is
   replaced. The character is at byte offset BYTE_POS in the string.
   The character being replaced is CHAR_LEN bytes long, and the
   character that will replace it is NEW_CLEN bytes long.  Return the
   address where the caller should store the new character.  */
unsigned char *
igc_replace_char (Lisp_Object string, ptrdiff_t at_byte_pos,
		  ptrdiff_t old_char_len, ptrdiff_t new_char_len)
{
  struct Lisp_String *s = XSTRING (string);

  // Replacing caaracters of the same length.
  if (old_char_len == new_char_len)
    return s->u.s.data + at_byte_pos;

  ptrdiff_t old_nbytes = SBYTES (string);
  ptrdiff_t nbytes_needed = old_nbytes + (new_char_len - old_char_len);
  struct igc_header *old_header = client_to_base (s->u.s.data);
  ptrdiff_t capacity = old_header->obj_size - sizeof *old_header;
  if (capacity < nbytes_needed)
    {
      unsigned char *new_data = alloc_string_data (nbytes_needed, false);
      memcpy (new_data, SDATA (string), old_nbytes);
      s->u.s.data = new_data;
    }

  // Set up string as if the character had been inserted.
  s->u.s.size_byte = nbytes_needed;
  unsigned char *insertion_addr = s->u.s.data + at_byte_pos;
  memmove (insertion_addr + new_char_len, insertion_addr + old_char_len,
	   new_char_len - old_char_len);
  return insertion_addr;
}

Lisp_Object
igc_make_string (size_t nchars, size_t nbytes, bool unibyte, bool clear)
{
  struct Lisp_String *s = alloc (sizeof *s, IGC_OBJ_STRING);
  s->u.s.size = nchars;
  s->u.s.size_byte = unibyte ? -1 : nbytes;
  s->u.s.data = alloc_string_data (nbytes, clear);
  return make_lisp_ptr (s, Lisp_String);
}

Lisp_Object
igc_make_multibyte_string (size_t nchars, size_t nbytes, bool clear)
{
  return igc_make_string (nchars, nbytes, false, clear);
}

Lisp_Object
igc_make_unibyte_string (size_t nchars, size_t nbytes, bool clear)
{
  return igc_make_string (nchars, nbytes, true, clear);
}

struct interval *
igc_make_interval (void)
{
  return alloc (sizeof (struct interval), IGC_OBJ_INTERVAL);
}

struct Lisp_Vector *
igc_alloc_pseudovector (size_t nwords_mem, size_t nwords_lisp,
			size_t nwords_zero, enum pvec_type tag)
{
  struct Lisp_Vector *v
    = alloc (header_size + nwords_mem * word_size, IGC_OBJ_VECTOR);
  XSETPVECTYPESIZE (v, tag, nwords_lisp, nwords_mem - nwords_lisp);
  return v;
}

struct Lisp_Vector *
igc_alloc_vector (ptrdiff_t len)
{
  struct Lisp_Vector *v = alloc (header_size + len * word_size, IGC_OBJ_VECTOR);
  v->header.size = len;
  return v;
}

struct Lisp_Vector *
igc_alloc_record (ptrdiff_t len)
{
  struct Lisp_Vector *v = alloc (header_size + len * word_size, IGC_OBJ_VECTOR);
  v->header.size = len;
  XSETPVECTYPE (v, PVEC_RECORD);
  return v;
}

struct itree_node *
igc_make_itree_node (void)
{
  struct itree_node *n = alloc (sizeof *n, IGC_OBJ_ITREE_NODE);
  return n;
}

struct image *
igc_make_image (void)
{
  struct image *img = alloc (sizeof *img, IGC_OBJ_IMAGE);
  return img;
}

struct face *
igc_make_face (void)
{
  struct face *face = alloc (sizeof *face, IGC_OBJ_FACE);
  return face;
}

Lisp_Object
igc_make_finalizer (Lisp_Object function)
{
  eassert (!"igc_amke_finalizer");
  return Qnil;
}

int
igc_valid_lisp_object_p (Lisp_Object obj)
{
  return 1;
}

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

  mps_gen_param_s gens[] = { { 32000, 0.8 }, { 5 * 32009, 0.4 } };
  res = mps_chain_create (&gc->chain, gc->arena, ARRAYELTS (gens), gens);
  IGC_CHECK_RES (res);
}

static mps_fmt_t
make_dflt_fmt (struct igc *gc)
{
  mps_res_t res;
  mps_fmt_t fmt;
  MPS_ARGS_BEGIN (args)
  {
    MPS_ARGS_ADD (args, MPS_KEY_FMT_ALIGN, IGC_ALIGN);
    /* Don't use in-band headers. I suspect they ahve problems,
       specifically amcSegScanNailedRange calls NailboardGet with a
       client address, which calls NailboardGet, and one can see that
       the the board contains base addresses which leads to an assertion
       failure. */
    // MPS_ARGS_ADD (args, MPS_KEY_FMT_HEADER_SIZE, 0);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_SCAN, dflt_scan);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_SKIP, dflt_skip);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_FWD, dflt_fwd);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_ISFWD, is_dflt_fwd);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_PAD, dflt_pad);
    res = mps_fmt_create_k (&fmt, gc->arena, args);
  }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);
  return fmt;
}

static mps_pool_t
make_pool_with_class (struct igc *gc, mps_fmt_t fmt, mps_class_t cls)
{
  mps_res_t res;
  mps_pool_t pool;
  MPS_ARGS_BEGIN (args)
  {
    MPS_ARGS_ADD (args, MPS_KEY_FORMAT, fmt);
    MPS_ARGS_ADD (args, MPS_KEY_CHAIN, gc->chain);
    MPS_ARGS_ADD (args, MPS_KEY_INTERIOR, true);
    res = mps_pool_create_k (&pool, gc->arena, cls, args);
  }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);
  return pool;
}

static mps_pool_t
make_pool_amc (struct igc *gc, mps_fmt_t fmt)
{
  return make_pool_with_class (gc, fmt, mps_class_amc ());
}

static mps_pool_t
make_pool_awl (struct igc *gc, mps_fmt_t fmt)
{
  return make_pool_with_class (gc, fmt, mps_class_awl ());
}

static mps_pool_t
make_pool_amcz (struct igc *gc, mps_fmt_t fmt)
{
  return make_pool_with_class (gc, fmt, mps_class_amcz ());
}

static struct igc *
make_igc (void)
{
  struct igc *gc = xzalloc (sizeof *gc);
  make_arena (gc);

  gc->dflt_fmt = make_dflt_fmt (gc);
  gc->dflt_pool = make_pool_amc (gc, gc->dflt_fmt);
  gc->leaf_fmt = make_dflt_fmt (gc);
  gc->leaf_pool = make_pool_amcz (gc, gc->leaf_fmt);
  gc->weak_fmt = make_dflt_fmt (gc);
  gc->weak_pool = make_pool_awl (gc, gc->weak_fmt);

  create_static_roots (gc);
  enable_messages (gc, true);
  return gc;
}

static void
free_igc (struct igc *gc)
{
  while (gc->threads)
    igc_thread_remove (gc->threads);
  mps_pool_destroy (gc->dflt_pool);
  mps_fmt_destroy (gc->dflt_fmt);
  mps_pool_destroy (gc->leaf_pool);
  mps_fmt_destroy (gc->leaf_fmt);
  mps_pool_destroy (gc->weak_pool);
  mps_fmt_destroy (gc->weak_fmt);
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
  mps_lib_assert_fail_install (igc_assert_fail);
  global_igc = make_igc ();
  atexit (free_global_igc);
  add_main_thread ();
}

void
syms_of_igc (void)
{
}

#endif

// Local Variables:
// mode: c++
// End:
