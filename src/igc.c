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
  - Does C have something like C++ thread_local etc?

  - itree: buffer -> itree_tree, overlay <-> itree_node. The backref
    from the node to the overlay means we need to fix it, and it is
    currently xalloc'd. Workaround for now: alloc itree nodes from a
    pool of their own, like intervals.

  - terminal -> image_cache->images -> image, and image has refs,
    everything is xmalloc'd. Put images in a pool to scan them.
    Since images are managed manually, alloc from pool, don't xfree,
    don't finalize. Find refs to images, by makiing cache::images
    an ambig root.

  - frame -> face_cache::faces_by_id -> face -> font. font is pvec.
    face has refs. Same procedure as for images.

  - window -> glyph_matrix -> glyph_row[] -> glyph[], and for frames
    with glyph_pool. Could do the same as for faces, images, but make
    glyphs ambiguous roots for trying it out (igc_x*alloc etc).

  - hash_table -> key_and_value which is malloc'd. Rewrite so that
    the ht has everything in its objects. Needed because we only have
    exclusive access to objs being scanned themselves, and we need
    to do things for eq hts (address changes).

  - compact_font_caches + inhibt. Can't be done this way, but isn't
    essential.

  - For now use igc_pad for everything and set pool alignment
    accordingly. Note that scan methods must cope with padding
    objects, and so on. Could be optimized but is out of scope for
    now.

  - Skip methods must align too. And they must be able to cope with
    forwarding and padding. See MPS docs.

  - weak hash tables

  - byte-code must be immovalble? See make-byte-code, pin_string,
    also in lread.c
 */

// clang-format on

#include <config.h>
#include <sys/_types/_size_t.h>
#include <sys/param.h>
#include "bignum.h"

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

// Let's abort for now instead of emacs_abort. Reason is that
// Emacs will call allocation functons whlle aborting, which
// leads to interesting phenomena when we IGC_ASSERT inside a
// function called from MPS while holding a lock, and find that
// we already own the lock while allocatin.

# ifdef IGC_DEBUG
#  define IGC_ASSERT(expr) \
    if (!(expr))           \
      abort ();            \
    else
# else
#  define IGC_ASSERT(expr) (void) 9
# endif

# define igc_static_assert(x) verify (x)

# define IGC_TAG_MASK (~VALMASK)

static mps_addr_t min_addr, max_addr;

static void
record_addr (mps_addr_t addr, mps_word_t nbytes)
{
  if (min_addr == NULL || addr < min_addr)
    min_addr = addr;
  addr = (char *) addr + nbytes;
  if (max_addr == NULL || addr > max_addr)
    max_addr = addr;
}

static bool
is_in_range (mps_addr_t addr)
{
  return addr >= min_addr && addr < max_addr;
}

enum
{
  IGC_ALIGN = GCALIGNMENT,
  IGC_ALIGN_DFLT = IGC_ALIGN,
};

# if 0
igc_static_assert (sizeof (struct igc_header) == sizeof (mps_word_t));
igc_static_assert (sizeof (struct igc_fwd) == 2 * sizeof (mps_word_t));
igc_static_assert (IGC_ALIGN_DFLT >= sizeof (struct igc_header));
# endif

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

struct igc_header
{
  enum igc_obj_type type : 8;
  mps_word_t total_nbytes : sizeof (mps_word_t) * CHAR_BIT - 8;
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
  return roundup (nbytes, align);
}

static size_t
igc_obj_size (size_t nbytes)
{
  nbytes += sizeof (struct igc_header);
  return igc_round (nbytes, IGC_ALIGN_DFLT);
}

struct igc_thread
{
  struct igc *gc;
  mps_thr_t thr;
  void *stack_start;
  struct igc_root_list *specpdl_root;
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

static enum pvec_type
pseudo_vector_type (const struct Lisp_Vector *v)
{
  return PSEUDOVECTOR_TYPE (v);
}

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

    if (tag == Lisp_Symbol)
      {
	mps_word_t off = word ^ tag;
	mps_addr_t client = (mps_addr_t) ((char *) lispsym + off);
	if (!c_symbol_p (client) && MPS_FIX1 (ss, client))
	  {
	    // MPS_FIX2 doc: The only exception is for references to
	    // objects belonging to a format with in-band headers: the
	    // header size must not be subtracted from these references.
	    mps_res_t res = MPS_FIX2 (ss, &client);
	    if (res != MPS_RES_OK)
	      return res;
	    mps_word_t new_off = (char *) client - (char *) lispsym;
	    *p = new_off | tag;
	  }
	return MPS_RES_OK;
      }

    // I have encountered cases where MPS_FIX1 returns true, but the
    // reference is somewhere completely off, so that MPS_FIX2 asserts.
    // IOW, MPS_FIX1 has undefined behavior if called on an address that
    // is not in the arena.
    mps_addr_t client = (mps_addr_t) (word ^ tag);
    if (is_in_range (client))
      {
	mps_pool_t pool;
	IGC_ASSERT (mps_addr_pool (&pool, global_igc->arena, client));
	if (MPS_FIX1 (ss, client))
	  {
	    // MPS_FIX2 doc: The only exception is for references to
	    // objects belonging to a format with in-band headers: the
	    // header size must not be subtracted from these references.
	    mps_res_t res = MPS_FIX2 (ss, &client);
	    if (res != MPS_RES_OK)
	      return res;
	    *p = (mps_word_t) client | tag;
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_raw (mps_ss_t ss, mps_addr_t *p)
{
  if (*p == NULL || c_symbol_p (*p))
    return MPS_RES_OK;

  MPS_SCAN_BEGIN (ss)
  {
    // Can be a pointer to a Lisp_Symbol. Cannot be an offset from
    // lispsym.
    mps_addr_t base = client_to_base (*p);
    if (MPS_FIX1 (ss, base))
      {
	mps_res_t res = MPS_FIX2 (ss, &base);
	if (res != MPS_RES_OK)
	  return res;
	mps_addr_t client = base_to_client (base);
	*p = client;
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
       type *obj_ = client_addr;                    \
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
  IGC_ASSERT (start == staticvec);
  MPS_SCAN_BEGIN (ss)
  {
    for (int i = 0; i < staticidx; ++i)
      {
	IGC_ASSERT (staticvec[i] != NULL);
	// staticvec is declared as having pointers to const
	// Lisp_Object, for whatever reason.
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

static mps_res_t
scan_specbindings (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    mps_res_t res;
    for (union specbinding *pdl = start; pdl < (union specbinding *) end; ++pdl)
      {
	switch (pdl->kind)
	  {
	  case SPECPDL_UNWIND:
	    IGC_FIX12_OBJ (ss, &pdl->unwind.arg);
	    break;

	  case SPECPDL_UNWIND_ARRAY:
	    IGC_FIX12_NOBJS (ss, pdl->unwind_array.array,
			     pdl->unwind_array.nelts);
	    break;

	  case SPECPDL_UNWIND_EXCURSION:
	    IGC_FIX12_OBJ (ss, &pdl->unwind_excursion.marker);
	    IGC_FIX12_OBJ (ss, &pdl->unwind_excursion.window);
	    break;

	  case SPECPDL_BACKTRACE:
	    {
	      IGC_FIX12_OBJ (ss, &pdl->bt.function);
	      ptrdiff_t nargs = pdl->bt.nargs;
	      if (nargs == UNEVALLED)
		nargs = 1;
	      IGC_FIX12_NOBJS (ss, pdl->bt.args, nargs);
	    }
	    break;

# ifdef HAVE_MODULES
	  case SPECPDL_MODULE_RUNTIME:
	    break;

	    // If I am not mistaken, the emacs_env in this binding
	    // actually lives on the stack (see module-load e.g.).
	    // So, we don't have to do something here for the Lisp
	    // objects in emacs_env.
	  case SPECPDL_MODULE_ENVIRONMENT:
	    break;
# endif
	  case SPECPDL_LET_DEFAULT:
	  case SPECPDL_LET_LOCAL:
	    IGC_FIX12_OBJ (ss, &pdl->let.where);
	    FALLTHROUGH;
	  case SPECPDL_LET:
	    IGC_FIX12_OBJ (ss, &pdl->let.symbol);
	    IGC_FIX12_OBJ (ss, &pdl->let.old_value);
	    break;

	  case SPECPDL_UNWIND_PTR:
	    break;

	  case SPECPDL_UNWIND_INT:
	  case SPECPDL_UNWIND_INTMAX:
	  case SPECPDL_UNWIND_VOID:
	  case SPECPDL_NOP:
	    break;
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_area_ambig (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (mps_word_t *p = start; p < (mps_word_t *) end; ++p)
      {
	mps_word_t word = *p;
	mps_word_t tag = word & IGC_TAG_MASK;

	if (tag == Lisp_Int0 && tag == Lisp_Int1)
	  continue;

	mps_addr_t ref = (mps_addr_t) (word ^ tag);
	if (MPS_FIX1 (ss, ref))
	  {
	    mps_res_t res = MPS_FIX2 (ss, &ref);
	    if (res != MPS_RES_OK)
	      return res;
	  }

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

/***********************************************************************
			 Default pad, fwd, ...
 ***********************************************************************/

struct igc_fwd
{
  struct igc_header header;
  mps_addr_t client_new_addr;
};

static void
dflt_pad (mps_addr_t base_addr, mps_word_t nbytes)
{
  struct igc_header *h = base_addr;
  h->type = IGC_OBJ_PAD;
  h->total_nbytes = nbytes;
}

static void
dflt_fwd (mps_addr_t client_old, mps_addr_t client_new_addr)
{
  struct igc_fwd *f = client_to_base (client_old);
  f->header.type = IGC_OBJ_FWD;
  f->client_new_addr = client_new_addr;
}

static mps_addr_t
is_dflt_fwd (mps_addr_t client_addr)
{
  struct igc_fwd *f = client_to_base (client_addr);
  if (f->header.type == IGC_OBJ_FWD)
    return f->client_new_addr;
  return NULL;
}

static mps_addr_t
dflt_skip (mps_addr_t client_addr)
{
  struct igc_header *h = client_to_base (client_addr);
  return (char *) h + h->total_nbytes;
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
  MPS_SCAN_BEGIN (ss) { IGC_ASSERT (!"fix_weak"); }
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
dflt_scan (mps_ss_t ss, mps_addr_t client_base, mps_addr_t client_limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    while (client_base < client_limit)
      {
	mps_addr_t client = client_base;
	client_base = dflt_skip (client_base);

	struct igc_header *header = client_to_base (client);
	switch (header->type)
	  {
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

static bool
is_pseudo_vector (const struct Lisp_Vector *v)
{
  return (v->header.size & PSEUDOVECTOR_FLAG) != 0;
}

static bool
is_plain_vector (const struct Lisp_Vector *v)
{
  return !is_pseudo_vector (v);
}

static size_t
pseudo_vector_nobjs (const struct Lisp_Vector *v)
{
  return v->header.size & PSEUDOVECTOR_SIZE_MASK;
}

static bool
is_bool_vector (const struct Lisp_Vector *v)
{
  IGC_ASSERT (is_pseudo_vector (v));
  return pseudo_vector_type (v) == PVEC_BOOL_VECTOR;
}

static mps_res_t
fix_vector (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    if (is_plain_vector (v))
      {
	IGC_FIX12_NOBJS (ss, v->contents, v->header.size);
      }
    else
      {
	// Fix Lisp object part of normal pseudo vectors.
	if (!is_bool_vector (v))
	  {
	    const size_t nobjs = pseudo_vector_nobjs (v);
	    IGC_FIX12_NOBJS (ss, v->contents, nobjs);
	  }

	switch (pseudo_vector_type (v))
	  {
	  case PVEC_VECTOR_FORWARD:
	  case PVEC_VECTOR_PAD:
	  case PVEC_FREE:
	    IGC_ASSERT (!"unexpected PVEC type");
	    break;

	  case PVEC_NORMAL_VECTOR:
	  case PVEC_BIGNUM:
	    // Nothing to do
	    break;

	  case PVEC_FINALIZER:
	    // Unclear, has a circurlar list of weak references?
	    IGC_ASSERT (!"PVEC_FINALIZER");
	    break;

	  case PVEC_SYMBOL_WITH_POS:
	    {
	      struct Lisp_Symbol_With_Pos *p = (void *) v;
	      IGC_FIX12_OBJ (ss, &p->sym);
	      IGC_FIX12_RAW (ss, &p->pos);
	    }
	    break;

	  case PVEC_MISC_PTR:
	    {
	      struct Lisp_Misc_Ptr *p = (void *) v;
	      IGC_FIX12_RAW (ss, &p->pointer);
	    }
	    break;

	  case PVEC_USER_PTR:
	    {
	      struct Lisp_User_Ptr *p = (void *) v;
	      IGC_FIX12_RAW (ss, &p->p);
	    }
	    break;

	  case PVEC_PROCESS:
	  case PVEC_BOOL_VECTOR:
	  case PVEC_WINDOW_CONFIGURATION:
	  case PVEC_PACKAGE:
	    // Nothing to do
	    break;

	  case PVEC_OTHER:
	    IGC_ASSERT (!"PVEC_OTHER");
	    break;

	  case PVEC_XWIDGET:
	  case PVEC_XWIDGET_VIEW:
	    IGC_ASSERT (!"PVEC_WIDGET*");
	    break;

	  case PVEC_THREAD:
	    {
	      struct thread_state *p = (void *) v;
	      IGC_FIX12_RAW (ss, &p->m_current_buffer);
	      IGC_FIX12_RAW (ss, &p->next_thread);
	    }
	    break;

	  case PVEC_MUTEX:
	    {
	      struct Lisp_Mutex *p = (void *) v;
	      IGC_FIX12_RAW (ss, &p->name);
	    }
	    break;

	  case PVEC_CONDVAR:
	  case PVEC_MODULE_FUNCTION:
	    // Nothing to do
	    break;

	  case PVEC_NATIVE_COMP_UNIT:
	  case PVEC_TS_PARSER:
	  case PVEC_TS_NODE:
	  case PVEC_TS_COMPILED_QUERY:
	  case PVEC_SQLITE:
	  case PVEC_COMPILED:
	  case PVEC_RECORD:
	  case PVEC_FONT:
	    // Nothing to do
	    break;

	  case PVEC_BUFFER:
	    {
	      struct buffer *b = (void *) v;
	      IGC_FIX12_RAW (ss, &b->text->intervals);
	      IGC_FIX12_RAW (ss, &b->text->markers);
	      IGC_FIX12_RAW (ss, &b->own_text.intervals);
	      IGC_FIX12_RAW (ss, &b->own_text.markers);
	      IGC_FIX12_RAW (ss, &b->base_buffer);
	      if (b->overlays)
		IGC_FIX12_RAW (ss, &b->overlays->root);
	      // FIXME: special handling of undo_list?
	    }
	    break;

	  case PVEC_FRAME:
	    {
	      // output_data;
	      // terminal
	      // face_cache *
	      // glyph_pool
	      // glyph matrices
	      // struct font_driver_list *font_driver_list;
	      // struct text_conversion_state conversion;
	      struct frame *f = (void *) v;
	      // eassert (false);
	    }
	    break;

	  case PVEC_WINDOW:
	    // All Lisp_Objects as part of pseudo-vector, but there
	    // are glyph_matrix pointers, in case we do something with
	    // that.
	    break;

	  case PVEC_HASH_TABLE:
	    {
	      struct Lisp_Hash_Table *h = (void *) v;
	      IGC_FIX12_NOBJS (ss, h->key, h->table_size);
	      IGC_FIX12_NOBJS (ss, h->value, h->table_size);
	    }
	    break;

	  case PVEC_CHAR_TABLE:
	  case PVEC_SUB_CHAR_TABLE:
	    // See also mark_char_table :-/
	    {
	      int size = v->header.size & PSEUDOVECTOR_SIZE_MASK;
	      enum pvec_type type = pseudo_vector_type (v);
	      int idx
		= (type == PVEC_SUB_CHAR_TABLE ? SUB_CHAR_TABLE_OFFSET : 0);
	      for (int i = idx; i < size; ++i)
		IGC_FIX12_OBJ (ss, &v->contents[i]);
	    }
	    break;

	  case PVEC_OVERLAY:
	    {
	      struct Lisp_Overlay *p = (void *) v;
	      IGC_FIX12_RAW (ss, &p->buffer);
	      IGC_FIX12_OBJ (ss, &p->plist);
	      IGC_FIX12_RAW (ss, &p->interval);
	    }
	    break;

	  case PVEC_TERMINAL:
	    {
	      struct terminal *p = (void *) v;
	      IGC_FIX12_RAW (ss, &p->next_terminal);
	    }
	    break;

	  case PVEC_SUBR:
	    {
	      struct Lisp_Subr *p = (void *) v;
	      IGC_FIX12_OBJ (ss, &p->command_modes);
# ifdef HAVE_NATIVE_COMP
	      IGC_FIX12_OBJ (ss, &p->intspec.native);
	      IGC_FIX12_OBJ (ss, &p->command_modes);
	      IGC_FIX12_OBJ (ss, &p->native_comp_u);
	      IGC_FIX12_OBJ (ss, &p->lambda_list);
	      IGC_FIX12_OBJ (ss, &p->type);
# endif
	    }
	    break;

	  case PVEC_MARKER:
	    {
	      struct Lisp_Marker *p = (void *) v;
	      IGC_FIX12_RAW (ss, &p->buffer);
	      IGC_FIX12_RAW (ss, &p->next);
	    }
	    break;
	  }
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
  mps_res_t res
    = mps_root_create_area_tagged (&root, gc->arena, mps_rank_ambig (), 0,
				   start, end, scan_area_ambig, IGC_TAG_MASK,
				   0);
  IGC_CHECK_RES (res);
  return register_root (gc, root, start, end);
}

static mps_rm_t
root_mode_inner (void)
{
  // Issue submitted to MPS
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
create_specpdl_root (struct igc_thread_list *t)
{
  if (specpdl == NULL)
    return;

  struct igc *gc = t->d.gc;
  void *start = specpdl, *end = specpdl_end;
  mps_root_t root;
  mps_res_t res = mps_root_create_area (&root, gc->arena, mps_rank_exact (), 0,
					start, end, scan_specbindings, NULL);
  IGC_CHECK_RES (res);
  t->d.specpdl_root = register_root (gc, root, start, end);
}

void
igc_on_specbinding_unused (union specbinding *b)
{
  memset (b, 0, sizeof *b);
}

void
igc_on_grow_specpdl (void)
{
  // Note that no two roots may overlap, so we have to temporarily
  // stop the collector while replacing one root with another (xpalloc
  // may realloc). Alternatives: (1) don't realloc, (2) alloc specpdl
  // from MPS pool that is scanned.
  struct igc_thread_list *t = current_thread->gc_info;
  IGC_WITH_PARKED (t->d.gc)
  {
    destroy_root (t->d.specpdl_root);
    create_specpdl_root (t);
  }
}

void
igc_on_alloc_main_thread_specpdl (void)
{
  create_specpdl_root (current_thread->gc_info);
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
    = mps_root_create_thread_tagged (&root, gc->arena, mps_rank_ambig (), 0,
				     t->d.thr, scan_area_ambig, IGC_TAG_MASK, 0,
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
  create_specpdl_root (t);
  create_thread_aps (&t->d);
  return t;
}

void
igc_thread_remove (void *info)
{
  struct igc_thread_list *t = info;
  destroy_root (t->d.specpdl_root);
  mps_ap_destroy (t->d.dflt_ap);
  mps_ap_destroy (t->d.leaf_ap);
  mps_ap_destroy (t->d.weak_strong_ap);
  mps_ap_destroy (t->d.weak_weak_ap);
  mps_thread_dereg (deregister_thread (t));
}

static void
add_main_thread (void)
{
  IGC_ASSERT (current_thread->gc_info == NULL);
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
  IGC_ASSERT (r != NULL);
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
type_of_addr (struct igc *gc, mps_addr_t addr, enum igc_obj_type *obj_type)
{
  mps_pool_t pool;
  if (!mps_addr_pool (&pool, gc->arena, addr))
    return false;

  struct igc_header *h = client_to_base (addr);
  *obj_type = h->type;
  return true;
}

static void
do_finalize (struct igc *gc, mps_addr_t addr)
{
  enum igc_obj_type obj_type;
  if (!type_of_addr (gc, addr, &obj_type))
    emacs_abort ();
  switch (obj_type)
    {
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

Lisp_Object
igc_make_cons (Lisp_Object car, Lisp_Object cdr)
{
  enum igc_obj_type type = IGC_OBJ_CONS;
  mps_ap_t ap = thread_ap (type);
  size_t nbytes = igc_obj_size (sizeof (struct Lisp_Cons));
  mps_addr_t p;
  struct Lisp_Cons *cons;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      struct igc_header *h = p;
      h->type = type;
      h->total_nbytes = nbytes;
      cons = base_to_client (p);
      cons->u.s.car = car;
      cons->u.s.u.cdr = cdr;
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
  return make_lisp_ptr (cons, Lisp_Cons);
}

Lisp_Object
igc_alloc_symbol (void)
{
  enum igc_obj_type type = IGC_OBJ_SYMBOL;
  mps_ap_t ap = thread_ap (type);
  size_t nbytes = igc_obj_size (sizeof (struct Lisp_Symbol));
  mps_addr_t p;
  struct Lisp_Symbol *sym;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      memclear (p, nbytes);
      struct igc_header *h = p;
      h->type = IGC_OBJ_SYMBOL;
      h->total_nbytes = nbytes;
      sym = base_to_client (p);
      sym->u.s.redirect = SYMBOL_PLAINVAL;
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
  return make_lisp_symbol (sym);
}

Lisp_Object
igc_make_float (double val)
{
  enum igc_obj_type type = IGC_OBJ_FLOAT;
  mps_ap_t ap = thread_ap (type);
  size_t nbytes = igc_obj_size (sizeof (struct Lisp_Float));
  mps_addr_t p;
  struct Lisp_Float *f;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      struct igc_header *h = p;
      h->type = IGC_OBJ_FLOAT;
      h->total_nbytes = nbytes;
      f = base_to_client (p);
      f->u.data = val;
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
  Lisp_Object obj;
  XSETFLOAT (obj, f);
  return obj;
}

static unsigned char *
alloc_string_data (size_t nbytes)
{
  enum igc_obj_type type = IGC_OBJ_STRING_DATA;
  mps_ap_t ap = thread_ap (type);
  // One word more make sure we have enough room for igc_fwd
  nbytes = igc_obj_size (sizeof (mps_addr_t) + nbytes);
  IGC_ASSERT (nbytes >= sizeof (struct igc_fwd));
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      struct igc_header *h = p;
      h->type = IGC_OBJ_STRING_DATA;
      h->total_nbytes = nbytes;
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
  return base_to_client (p);
}

// Reallocate multibyte STRING data when a single character is
// replaced. The character is at byte offset BYTE_POS in the string.
// The character being replaced is CHAR_LEN bytes long, and the
// character that will replace it is NEW_CLEN bytes long.  Return the
// address where the caller should store the new character.
unsigned char *
igc_replace_char (Lisp_Object string, ptrdiff_t at_byte_pos,
		  ptrdiff_t old_char_len, ptrdiff_t new_char_len)
{
  struct Lisp_String *s = XSTRING (string);

  // Replacing caaracters of the same length.
  if (old_char_len == new_char_len)
    return s->u.s.data + at_byte_pos;

  // If new char doesn't fit, make a new string data
  ptrdiff_t old_nbytes = SBYTES (string);
  ptrdiff_t nbytes_needed = old_nbytes + (new_char_len - old_char_len);
  struct igc_header *old_header = client_to_base (s->u.s.data);
  ptrdiff_t capacity = old_header->total_nbytes - sizeof *old_header;
  if (capacity < nbytes_needed)
    {
      unsigned char *new_data = alloc_string_data (nbytes_needed);
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
  unsigned char *data = alloc_string_data (nbytes);
  if (clear)
    memset (data, 0, nbytes);

  enum igc_obj_type type = IGC_OBJ_STRING;
  mps_ap_t ap = thread_ap (type);
  size_t string_nbytes = igc_obj_size (sizeof (struct Lisp_String));
  mps_addr_t p;
  struct Lisp_String *s;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, string_nbytes);
      IGC_CHECK_RES (res);
      struct igc_header *h = p;
      h->type = type;
      h->total_nbytes = string_nbytes;
      s = base_to_client (p);
      s->u.s.size = nchars;
      s->u.s.size_byte = unibyte ? -1 : nbytes;
      s->u.s.intervals = NULL;
      s->u.s.data = data;
    }
  while (!mps_commit (ap, p, string_nbytes));
  record_addr (p, nbytes);
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
  enum igc_obj_type type = IGC_OBJ_INTERVAL;
  mps_ap_t ap = thread_ap (type);
  size_t nbytes = igc_obj_size (sizeof (struct interval));
  mps_addr_t p;
  struct interval *iv;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      memclear (p, nbytes);
      struct igc_header *h = p;
      h->type = type;
      h->total_nbytes = nbytes;
      iv = base_to_client (p);
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
  return iv;
}

// All lens in words
struct Lisp_Vector *
igc_alloc_pseudovector (size_t nwords_mem, size_t nwords_lisp,
			size_t nwords_zero, enum pvec_type tag)
{
  enum igc_obj_type type = IGC_OBJ_VECTOR;
  mps_ap_t ap = thread_ap (type);
  size_t nbytes = igc_obj_size (header_size + nwords_mem * word_size);
  mps_addr_t p;
  struct Lisp_Vector *v;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      struct igc_header *h = p;
      h->type = type;
      h->total_nbytes = nbytes;
      v = base_to_client (p);
      memclear (v->contents, nwords_zero * word_size);
      XSETPVECTYPESIZE (v, tag, nwords_lisp, nwords_mem - nwords_lisp);
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
  return v;
}

struct Lisp_Vector *
igc_alloc_vector (ptrdiff_t len)
{
  enum igc_obj_type type = (IGC_OBJ_VECTOR);
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes = igc_obj_size (header_size + len * word_size);
  mps_addr_t p;
  struct Lisp_Vector *v;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      memclear (p, nbytes);
      struct igc_header *h = p;
      h->type = type;
      h->total_nbytes = nbytes;
      v = base_to_client (p);
      v->header.size = len;
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
  return v;
}

struct Lisp_Vector *
igc_alloc_record (ptrdiff_t len)
{
  enum igc_obj_type type = (IGC_OBJ_VECTOR);
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes = igc_obj_size (header_size + len * word_size);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      memclear (p, nbytes);
      struct Lisp_Vector *v = p;
      v->header.size = len;
      XSETPVECTYPE (v, PVEC_RECORD);
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
  return p;
}

struct itree_node *
igc_make_itree_node (void)
{
  enum igc_obj_type type = IGC_OBJ_ITREE_NODE;
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes = igc_obj_size (sizeof (struct itree_node));
  mps_addr_t p;
  struct itree_node *n;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      memclear (p, nbytes);
      struct igc_header *h = p;
      h->type = type;
      h->total_nbytes = nbytes;
      n = base_to_client (p);
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
  return n;
}

struct image *
igc_make_image (void)
{
  enum igc_obj_type type = IGC_OBJ_IMAGE;
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes = igc_obj_size (sizeof (struct image));
  mps_addr_t p;
  struct image *img;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      memclear (p, nbytes);
      struct igc_header *h = p;
      h->type = type;
      h->total_nbytes = nbytes;
      img = base_to_client (p);
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
  return img;
}

struct face *
igc_make_face (void)
{
  enum igc_obj_type type = IGC_OBJ_FACE;
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes = igc_obj_size (sizeof (struct face));
  mps_addr_t p;
  struct face *face;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      memclear (p, nbytes);
      face = base_to_client (p);
    }
  while (!mps_commit (ap, p, nbytes));
  record_addr (p, nbytes);
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
    MPS_ARGS_ADD (args, MPS_KEY_FMT_HEADER_SIZE, sizeof (struct igc_header));
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
    MPS_ARGS_ADD (args, MPS_KEY_INTERIOR, false);
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

static void
igc_assert_fail (const char *file, unsigned line, const char *msg)
{
  die (msg, file, line);
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
