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

#ifdef HAVE_MPS

# include <mps.h>
# include <mpsavm.h>
# include <mpscamc.h>
# include <mpscawl.h>
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

# ifdef IGC_DEBUG
#  define IGC_ASSERT(expr) \
    if (!(expr))           \
      emacs_abort ();      \
    else
# else
#  define IGC_ASSERT(expr) (void) 9
# endif

# define igc_static_assert(x) verify (x)

# define IGC_TAG_MASK (~VALMASK)

enum igc_pool_class
{
  IGC_AMC,
  IGC_AWL,
  IGC_AMCZ
};

struct igc_init
{
  enum igc_pool_class class_type;
  mps_class_t pool_class;
  size_t align;
  bool interior_pointers;
  mps_fmt_scan_t scan;
  mps_fmt_skip_t skip;
  mps_fmt_fwd_t forward;
  mps_fmt_isfwd_t is_forwarded;
  mps_fmt_pad_t pad;
};

static struct igc_init igc_inits[];

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

enum igc_type
{
  IGC_TYPE_CONS,
  IGC_TYPE_SYMBOL,
  IGC_TYPE_INTERVAL,
  IGC_TYPE_STRING,
  IGC_TYPE_STRING_DATA,
  IGC_TYPE_VECTOR,
  IGC_TYPE_ITREE_NODE,
  IGC_TYPE_IMAGE,
  IGC_TYPE_FACE,
  IGC_TYPE_FLOAT,
  IGC_TYPE_WEAK,
  IGC_TYPE_LAST
};

static size_t
igc_round (size_t nbytes, size_t align)
{
  return roundup (nbytes, align);
}

static size_t
igc_round_to_pool (size_t nbytes, enum igc_type type)
{
  return igc_round (nbytes, igc_inits[type].align);
}

static bool
is_aligned (mps_addr_t p, enum igc_type type)
{
  size_t align = igc_inits[type].align;
  mps_word_t w = (mps_word_t) p;
  return w % align == 0;
}

struct igc_thread
{
  struct igc *gc;
  mps_thr_t thr;
  void *stack_start;
  struct igc_root_list *specpdl_root;
  mps_ap_t ap[IGC_TYPE_LAST];
};

typedef struct igc_thread igc_thread;
IGC_DEFINE_LIST (igc_thread);

struct igc
{
  mps_arena_t arena;
  mps_chain_t chain;
  mps_fmt_t fmt[IGC_TYPE_LAST];
  mps_pool_t pool[IGC_TYPE_LAST];
  ;
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

static void
create_thread_aps (struct igc_thread *t)
{
  for (enum igc_type type = 0; type < IGC_TYPE_LAST; ++type)
    {
      mps_res_t res
	= mps_ap_create_k (&t->ap[type], t->gc->pool[type], mps_args_none);
      IGC_CHECK_RES (res);
    }
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

# define IGC_FWDSIG  ~((mps_word_t) 0)
# define IGC_FWDSIG2 (IGC_FWDSIG - 1)
# define IGC_PADSIG  (IGC_FWDSIG - 2)

struct igc_fwd
{
  mps_word_t sig;
  mps_addr_t new_addr;
};

struct igc_pad
{
  mps_word_t sig;
  mps_addr_t padding_end;
};

igc_static_assert (sizeof (struct Lisp_Cons) >= sizeof (struct igc_fwd));
igc_static_assert (sizeof (struct interval) >= sizeof (struct igc_fwd));
igc_static_assert (sizeof (struct Lisp_Cons) >= sizeof (struct igc_pad));
igc_static_assert (sizeof (struct interval) >= sizeof (struct igc_pad));

static void
forward (mps_addr_t old, mps_addr_t new)
{
  struct igc_fwd m = { .sig = IGC_FWDSIG, .new_addr = new };
  *(struct igc_fwd *) old = m;
}

static mps_addr_t
is_forwarded (const mps_addr_t addr)
{
  struct igc_fwd *f = addr;
  return f->sig == IGC_FWDSIG ? f->new_addr : NULL;
}

static void
pad (mps_addr_t addr, size_t nbytes)
{
  IGC_ASSERT (nbytes >= sizeof (struct igc_pad));
  mps_addr_t end = (char *) addr + nbytes;
  struct igc_pad p = { .sig = IGC_PADSIG, .padding_end = end };
  *(struct igc_pad *) addr = p;
}

static bool
is_padding (const mps_addr_t addr)
{
  struct igc_pad *p = addr;
  return p->sig == IGC_PADSIG;
}

static mps_addr_t
padding_end (mps_addr_t addr)
{
  IGC_ASSERT (is_padding (addr));
  struct igc_pad *p = addr;
  return p->padding_end;
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

    if (tag == Lisp_Int0 && tag == Lisp_Int1)
      return MPS_RES_OK;

    if (tag == Lisp_Symbol)
      {
	mps_word_t off = word ^ tag;
	mps_addr_t ref = (mps_addr_t) ((char *) lispsym + off);
	if (is_aligned (ref, IGC_TYPE_SYMBOL)
	    && MPS_FIX1 (ss, ref))
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

# define IGC_FIX12_OBJ(ss, p)                           \
   do                                                   \
     {                                                  \
       mps_res_t res;                                   \
       MPS_FIX_CALL (ss, res = fix_lisp_obj (ss, (p))); \
       if (res != MPS_RES_OK)                           \
	 return res;                                    \
     }                                                  \
   while (0)

# define IGC_FIX12_RAW(ss, p)                              \
   do                                                      \
     {                                                     \
       mps_res_t res = MPS_FIX12 (ss, (mps_addr_t *) (p)); \
       if (res != MPS_RES_OK)                              \
	 return res;                                       \
     }                                                     \
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
  MPS_SCAN_BEGIN (ss)
  {
    for (Lisp_Object **p = start; p < (Lisp_Object **) end; ++p)
      if (*p)
	IGC_FIX12_OBJ (ss, *p);
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

	  default:
	    IGC_ASSERT (false);
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

static mps_addr_t
cons_skip (mps_addr_t addr)
{
  if (is_padding (addr))
    return padding_end (addr);
  return (char *) addr
    + igc_round_to_pool (sizeof (struct Lisp_Cons), IGC_TYPE_CONS);
}

static mps_res_t
cons_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    while (base < limit)
      {
	struct Lisp_Cons *cons = base;
	base = cons_skip (base);

	if (is_forwarded (cons) || is_padding (cons))
	  continue;
	IGC_FIX12_OBJ (ss, &cons->u.s.car);
	IGC_FIX12_OBJ (ss, &cons->u.s.u.cdr);
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
symbol_skip (mps_addr_t addr)
{
  if (is_padding (addr))
    return padding_end (addr);

  return (char *) addr + igc_round_to_pool (sizeof (struct Lisp_Symbol), IGC_TYPE_SYMBOL);
}

static mps_res_t
symbol_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    while (base < limit)
      {
	struct Lisp_Symbol *sym = base;
	base = symbol_skip (base);

	if (is_forwarded (sym) || is_padding (sym))
	  continue;
	IGC_FIX_CALL (ss, fix_symbol (ss, sym));
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

struct igc_sdata
{
  mps_word_t capacity;
  mps_addr_t unused;		// so that sizeof >= sizeof fwd
  unsigned char contents[];
};

igc_static_assert (sizeof (struct igc_sdata) >= sizeof (struct igc_fwd));

static unsigned char *
sdata_contents (struct igc_sdata *d)
{
  mps_addr_t p = &d->contents[0];
  IGC_ASSERT (is_aligned (p, IGC_TYPE_STRING_DATA));
  return p;
}

static ptrdiff_t
sdata_capacity (const struct igc_sdata *d)
{
  return d->capacity;
}

static struct igc_sdata *
contents_to_sdata (unsigned char *p)
{
  return (struct igc_sdata *) ((char *) p - sizeof (struct igc_sdata));
}

static mps_addr_t
string_data_skip (mps_addr_t addr)
{
  if (is_padding (addr))
    return padding_end (addr);
  mps_addr_t new_addr = is_forwarded (addr);
  struct igc_sdata *sdata = new_addr ? new_addr : addr;
  return (char *) addr + sizeof *sdata + sdata->capacity;
}

static mps_addr_t
string_skip (mps_addr_t addr)
{
  if (is_padding (addr))
    return padding_end (addr);
  return (char *) addr
	 + igc_round_to_pool (sizeof (struct Lisp_String), IGC_TYPE_STRING);
}

static mps_res_t
string_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    while (base < limit)
      {
	struct Lisp_String *s = base;
	base = string_skip (base);
	if (is_forwarded (s) || is_padding (s))
	  continue;

	// Looks like MPS does not like to FIX12 an interior pointer
	// into the string data pool, whether or not the string data
	// pool itself was created with or without interior pointer
	// support. The docs speak of ambiguous pointers that the
	// interior pointer setting supports, and that has to be taken
	// literally.
	struct igc_sdata *sdata = contents_to_sdata (s->u.s.data);
	IGC_FIX12_RAW (ss, &sdata);
	s->u.s.data = sdata_contents (sdata);
	IGC_FIX12_RAW (ss, &s->u.s.intervals);
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
float_skip (mps_addr_t addr)
{
  if (is_padding (addr))
    return padding_end (addr);
  return (char *) addr
	 + igc_round_to_pool (sizeof (struct Lisp_Float), IGC_TYPE_FLOAT);
}

static mps_addr_t
interval_skip (mps_addr_t addr)
{
  if (is_padding (addr))
    return padding_end (addr);
  return (char *) addr
    + igc_round_to_pool (sizeof (struct interval), IGC_TYPE_INTERVAL);
}

static mps_res_t
interval_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    while (base < limit)
      {
	struct interval *iv = base;
	base = interval_skip (base);

	if (is_forwarded (iv) || is_padding (iv))
	  continue;

	IGC_FIX12_RAW (ss, &iv->left);
	IGC_FIX12_RAW (ss, &iv->right);
	if (iv->up_obj)
	  IGC_FIX12_OBJ (ss, &iv->up.obj);
	else
	  IGC_FIX12_RAW (ss, iv->up.interval);
	IGC_FIX12_OBJ (ss, &iv->plist);
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
itree_skip (mps_addr_t addr)
{
  if (is_padding (addr))
    return padding_end (addr);
  return (char *) addr
	 + igc_round_to_pool (sizeof (struct itree_node), IGC_TYPE_ITREE_NODE);
}

static mps_res_t
itree_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    while (base < limit)
      {
	struct itree_node *n = base;
	base = itree_skip (base);

	if (is_forwarded (n) || is_padding (n))
	  continue;
	IGC_FIX12_RAW (ss, &n->parent);
	IGC_FIX12_RAW (ss, &n->left);
	IGC_FIX12_RAW (ss, &n->right);
	IGC_FIX12_OBJ (ss, &n->data);
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
image_skip (mps_addr_t addr)
{
  if (is_padding (addr))
    return padding_end (addr);
  return (char *) addr
    + igc_round_to_pool (sizeof (struct itree_node), IGC_TYPE_IMAGE);
}

static mps_res_t
image_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    while (base < limit)
      {
	struct image *i = base;
	base = image_skip (base);

	if (is_forwarded (i) || is_padding (i))
	  continue;
	IGC_FIX12_OBJ (ss, &i->spec);
	IGC_FIX12_OBJ (ss, &i->dependencies);
	IGC_FIX12_OBJ (ss, &i->lisp_data);
	IGC_FIX12_RAW (ss, &i->next);
	IGC_FIX12_RAW (ss, &i->prev);
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
face_skip (mps_addr_t addr)
{
  if (is_padding (addr))
    return padding_end (addr);
  return (char *) addr
	 + igc_round_to_pool (sizeof (struct face), IGC_TYPE_FACE);
}

static mps_res_t
face_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    while (base < limit)
      {
	struct face *face = base;
	base = face_skip (base);

	if (is_forwarded (face) || is_padding (face))
	  continue;
	IGC_FIX12_NOBJS (ss, face->lface, ARRAYELTS (face->lface));
	IGC_FIX12_RAW (ss, &face->font);
	IGC_FIX12_RAW (ss, &face->next);
	IGC_FIX12_RAW (ss, &face->prev);
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

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

static size_t
pseudo_vector_rest_nwords (const struct Lisp_Vector *v)
{
  return (v->header.size & PSEUDOVECTOR_REST_MASK) >> PSEUDOVECTOR_SIZE_BITS;
}

static enum pvec_type
pseudo_vector_type (const struct Lisp_Vector *v)
{
  return PSEUDOVECTOR_TYPE (v);
}

static bool
is_bool_vector (const struct Lisp_Vector *v)
{
  return is_pseudo_vector (v) && pseudo_vector_type (v) == PVEC_BOOL_VECTOR;
}

static bool
is_hash_impl (const struct Lisp_Vector *v)
{
  return is_pseudo_vector (v) && pseudo_vector_type (v) == PVEC_HASH_IMPL;
}

static size_t
vector_size (const struct Lisp_Vector *v)
{
  // lisp.h defines header_size, word_size, bool_header_size
  size_t nwords = v->header.size;
  size_t hsize = header_size;
  if (is_pseudo_vector (v))
    {
      if (is_hash_impl (v))
	{
	  struct hash_impl *h = (struct hash_impl *) v;
	  return hash_impl_nbytes (h->table_size);
	}
      else if (is_bool_vector (v))
	{
	  struct Lisp_Bool_Vector *bv = (struct Lisp_Bool_Vector *) v;
	  hsize = bool_header_size;
	  nwords = bool_vector_words (bv->size);
	}
      else
	nwords = pseudo_vector_nobjs (v) + pseudo_vector_rest_nwords (v);
    }

  return hsize + nwords * word_size;
}

static void
set_pseudo_vector_type (union vectorlike_header *header, enum pvec_type type)
{
  header->size |= (PSEUDOVECTOR_FLAG | (type << PSEUDOVECTOR_AREA_BITS));
}

struct igc_vector_pad
{
  union vectorlike_header header;
  mps_word_t nbytes;
};

static void
vector_pad (mps_addr_t addr, mps_word_t nbytes)
{
  struct igc_vector_pad *p = addr;
  set_pseudo_vector_type (&p->header, PVEC_VECTOR_PAD);
  p->nbytes = nbytes;
}

static bool
is_vector_padding (mps_addr_t addr)
{
  return pseudo_vector_type (addr) == PVEC_VECTOR_PAD;
}

static mps_addr_t
vector_padding_end (mps_addr_t addr)
{
  IGC_ASSERT (is_vector_padding (addr));
  struct igc_vector_pad *p = addr;
  return (char *) addr + p->nbytes;
}

struct igc_vector_fwd
{
  union vectorlike_header header;
  mps_addr_t new_addr;
};

static void
vector_forward (mps_addr_t old, mps_addr_t new_addr)
{
  struct igc_vector_fwd *f = old;
  set_pseudo_vector_type (&f->header, PVEC_VECTOR_FORWARD);
  f->new_addr = new_addr;
}

static mps_addr_t
is_vector_forwarded (mps_addr_t addr)
{
  if (pseudo_vector_type (addr) != PVEC_VECTOR_FORWARD)
    return NULL;
  struct igc_vector_fwd *f = addr;
  return f->new_addr;
}

static mps_addr_t
vector_skip (mps_addr_t addr)
{
  if (is_vector_padding (addr))
    return vector_padding_end (addr);
  mps_addr_t new_addr = is_vector_forwarded (addr);
  mps_addr_t vec_addr = new_addr ? new_addr : addr;
  ptrdiff_t nbytes = vector_size (vec_addr);
  return (char *) addr + igc_round_to_pool (nbytes, IGC_TYPE_VECTOR);
}

static mps_res_t
vector_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    while (base < limit)
      {
	struct Lisp_Vector *v = base;
	mps_addr_t vbase = base;
	base = vector_skip (base);

	if (is_vector_forwarded (v) || is_vector_padding (v))
	  continue;

	// Fix contents of normal vectors.
	if (is_plain_vector (v))
	  {
	    IGC_FIX12_NOBJS (ss, v->contents, v->header.size);
	    continue;
	  }

	// Fix Lisp object part of normal pseudo vectors.
	if (!is_bool_vector (v) && !is_hash_impl (v))
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
	      struct Lisp_Symbol_With_Pos *p = vbase;
	      IGC_FIX12_OBJ (ss, &p->sym);
	      IGC_FIX12_RAW (ss, &p->pos);
	    }
	    break;

	  case PVEC_MISC_PTR:
	    {
	      struct Lisp_Misc_Ptr *p = vbase;
	      IGC_FIX12_RAW (ss, &p->pointer);
	    }
	    break;

	  case PVEC_USER_PTR:
	    {
	      struct Lisp_User_Ptr *p = vbase;
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
	      struct thread_state *p = vbase;
	      IGC_FIX12_RAW (ss, &p->m_current_buffer);
	      IGC_FIX12_RAW (ss, &p->next_thread);
	    }
	    break;

	  case PVEC_MUTEX:
	    {
	      struct Lisp_Mutex *p = vbase;
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
	      struct buffer *b = vbase;
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
	      struct frame *f = vbase;
	      eassert (false);
	    }
	    break;

	  case PVEC_WINDOW:
	    // All Lisp_Objects as part of pseudo-vector, but there
	    // are glyph_matrix pointers, in case we do something with
	    // that.
	    break;

	  case PVEC_HASH_TABLE:
	    {
	      struct Lisp_Hash_Table *p = vbase;
	      IGC_FIX12_RAW (ss, &p->i);
	    }
	    break;

	  case PVEC_HASH_IMPL:
	    {
	      struct hash_impl *h = vbase;
	      eassert (h->weakness == Weak_None);
	      for (ptrdiff_t i = 0, n = h->count; n > 0 && i < h->table_size;
		   ++i)
		{
		  struct hash_entry *e = h->entries + i;
		  if (!hash_unused_entry_key_p (e->key))
		    {
		      IGC_FIX12_OBJ (ss, &e->key);
		      IGC_FIX12_OBJ (ss, &e->value);
		      --n;
		    }
		}
	    }
	    break;

	  case PVEC_CHAR_TABLE:
	  case PVEC_SUB_CHAR_TABLE:
	    // See also mark_char_table :-/
	    {
	      struct Lisp_Vector *v = vbase;
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
	      struct Lisp_Overlay *p = vbase;
	      IGC_FIX12_RAW (ss, &p->buffer);
	      IGC_FIX12_OBJ (ss, &p->plist);
	      IGC_FIX12_RAW (ss, &p->interval);
	    }
	    break;

	  case PVEC_TERMINAL:
	    {
	      struct terminal *p = vbase;
	      IGC_FIX12_RAW (ss, &p->next_terminal);
	    }
	    break;

	  case PVEC_SUBR:
	    {
	      struct Lisp_Subr *p = vbase;
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
	      struct Lisp_Marker *p = vbase;
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

enum igc_weak_type
{
  IGC_WEAK_HASH_IMPL
};

struct igc_weak
{
  mps_word_t object_nbytes;
  enum igc_weak_type type;
};

static mps_addr_t
weak_skip (mps_addr_t addr)
{
  if (is_padding (addr))
    return padding_end (addr);
  struct igc_weak *w = addr;
  return (char *) addr + w->object_nbytes;
}

static mps_addr_t
weak_obj (mps_addr_t base)
{
  return (char *) base + sizeof (struct igc_weak);
}

static mps_res_t
weak_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    while (base < limit)
      {
	struct igc_weak *w = base;
	base = weak_skip (base);
	eassert (w->type == IGC_WEAK_HASH_IMPL);
	struct hash_impl *h = weak_obj (w);
	eassert (h->weakness != Weak_None);
	eassert (!"weak table");
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
					root_mode_inner (), start,
					end, scan_staticvec, NULL);
  IGC_CHECK_RES (res);
  register_root (gc, root, start, end);
}

static void
create_lispsym_root (struct igc *gc)
{
  void *start = lispsym, *end = lispsym + ARRAYELTS (lispsym);
  mps_root_t root;
  mps_res_t res = mps_root_create_area (&root, gc->arena, mps_rank_exact (),
					root_mode_inner (), start,
					end, scan_lispsym, NULL);
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
  igc_static_assert (NIL_IS_ZERO);
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
  for (enum igc_type i = 0; i < IGC_TYPE_LAST; ++i)
    mps_ap_destroy (t->d.ap[i]);
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
  igc_static_assert (NIL_IS_ZERO);
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
    case IGC_TYPE_ITREE_NODE:
    case IGC_TYPE_IMAGE:
    case IGC_TYPE_FACE:
    case IGC_TYPE_FLOAT:
    case IGC_TYPE_WEAK:
    case IGC_TYPE_LAST:
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
thread_ap (enum igc_type type)
{
  struct igc_thread_list *t = current_thread->gc_info;
  return t->d.ap[type];
}

enum
{
  IGC_ALIGNMENT = max (sizeof (struct igc_fwd), sizeof (struct igc_pad)),
  IGC_VALIGNMENT = 32
};

static struct igc_init igc_inits[IGC_TYPE_LAST] = {
  [IGC_TYPE_CONS] = { .class_type = IGC_AMC,
		      .align = IGC_ALIGNMENT,
		      .interior_pointers = false,
		      .forward = forward,
		      .pad = pad,
		      .is_forwarded = is_forwarded,
		      .scan = cons_scan,
		      .skip = cons_skip },
  [IGC_TYPE_SYMBOL] = { .class_type = IGC_AMC,
			.align = IGC_ALIGNMENT,
			.interior_pointers = false,
			.forward = forward,
			.pad = pad,
			.is_forwarded = is_forwarded,
			.scan = symbol_scan,
			.skip = symbol_skip },
  [IGC_TYPE_INTERVAL] = { .class_type = IGC_AMC,
			  .align = IGC_ALIGNMENT,
			  .interior_pointers = false,
			  .forward = forward,
			  .pad = pad,
			  .is_forwarded = is_forwarded,
			  .scan = interval_scan,
			  .skip = interval_skip },
  [IGC_TYPE_STRING] = { .class_type = IGC_AMC,
			.align = IGC_ALIGNMENT,
			.interior_pointers = false,
			.forward = forward,
			.pad = pad,
			.is_forwarded = is_forwarded,
			.scan = string_scan,
			.skip = string_skip },
  [IGC_TYPE_STRING_DATA] = { .class_type = IGC_AMCZ,
			     .align = IGC_ALIGNMENT,
			     .interior_pointers = true,
			     .forward = forward,
			     .is_forwarded = is_forwarded,
			     .pad = pad,
			     .scan = NULL,
			     .skip = string_data_skip },
  [IGC_TYPE_VECTOR] = { .class_type = IGC_AMC,
			.align = IGC_VALIGNMENT,
			.interior_pointers = false,
			.forward = vector_forward,
			.is_forwarded = is_vector_forwarded,
			.pad = vector_pad,
			.scan = vector_scan,
			.skip = vector_skip },
  [IGC_TYPE_ITREE_NODE] = { .class_type = IGC_AMC,
			    .align = IGC_ALIGNMENT,
			    .interior_pointers = false,
			    .forward = forward,
			    .is_forwarded = is_forwarded,
			    .pad = pad,
			    .scan = itree_scan,
			    .skip = itree_skip },
  [IGC_TYPE_IMAGE] = { .class_type = IGC_AMC,
		       .align = IGC_ALIGNMENT,
		       .interior_pointers = false,
		       .forward = forward,
		       .is_forwarded = is_forwarded,
		       .pad = pad,
		       .scan = image_scan,
		       .skip = image_skip },
  [IGC_TYPE_FACE] = { .class_type = IGC_AMC,
		      .align = IGC_ALIGNMENT,
		      .interior_pointers = false,
		      .forward = forward,
		      .is_forwarded = is_forwarded,
		      .pad = pad,
		      .scan = face_scan,
		      .skip = face_skip },
  [IGC_TYPE_FLOAT] = { .class_type = IGC_AMCZ,
		       .align = IGC_ALIGNMENT,
		       .interior_pointers = false,
		       .forward = forward,
		       .is_forwarded = is_forwarded,
		       .pad = pad,
		       .scan = NULL,
		       .skip = float_skip },
  [IGC_TYPE_WEAK] = { .class_type = IGC_AWL,
		      .align = IGC_ALIGNMENT,
		      // Maybe better use a format with header
		      .interior_pointers = true,
		      .forward = forward,
		      .is_forwarded = is_forwarded,
		      .pad = pad,
		      .scan = weak_scan,
		      .skip = weak_skip },
};

void
igc_break (void)
{
}

Lisp_Object
igc_make_cons (Lisp_Object car, Lisp_Object cdr)
{
  enum igc_type type = IGC_TYPE_CONS;
  mps_ap_t ap = thread_ap (type);
  size_t nbytes = igc_round_to_pool (sizeof (struct Lisp_Cons), type);
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
  enum igc_type type = IGC_TYPE_SYMBOL;
  mps_ap_t ap = thread_ap (type);
  size_t nbytes = igc_round_to_pool (sizeof (struct Lisp_Symbol), type);
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

Lisp_Object
igc_make_float (double val)
{
  enum igc_type type = IGC_TYPE_FLOAT;
  mps_ap_t ap = thread_ap (type);
  size_t nbytes = igc_round_to_pool (sizeof (struct Lisp_Float), type);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      struct Lisp_Float *f = p;
      f->u.data = val;
    }
  while (!mps_commit (ap, p, nbytes));
  Lisp_Object obj;
  XSETFLOAT (obj, p);
  return obj;
}

static struct igc_sdata *
alloc_string_data (size_t nbytes)
{
  enum igc_type type = IGC_TYPE_STRING_DATA;
  mps_ap_t ap = thread_ap (type);
  nbytes = igc_round_to_pool (sizeof (struct igc_sdata) + nbytes, type);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      // Initialize before we let it loose on the world.
      struct igc_sdata *s = p;
      s->capacity = nbytes - sizeof (struct igc_sdata);
    }
  while (!mps_commit (ap, p, nbytes));
  return p;
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
  struct igc_sdata *old_sdata = contents_to_sdata (s->u.s.data);
  ptrdiff_t old_nbytes = SBYTES (string);
  ptrdiff_t nbytes_needed = old_nbytes + (new_char_len - old_char_len);
  if (sdata_capacity (old_sdata) < nbytes_needed)
    {
      struct igc_sdata *new_sdata = alloc_string_data (nbytes_needed);
      memcpy (new_sdata->contents, old_sdata->contents, old_nbytes);
      s->u.s.data = new_sdata->contents;
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
  struct igc_sdata *data = alloc_string_data (nbytes);
  if (clear)
    memset (sdata_contents (data), 0, nbytes);

  enum igc_type type = IGC_TYPE_STRING;
  mps_ap_t ap = thread_ap (type);
  size_t size = igc_round_to_pool (sizeof (struct Lisp_String), type);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, size);
      IGC_CHECK_RES (res);
      struct Lisp_String *s = p;
      s->u.s.size = nchars;
      s->u.s.size_byte = unibyte ? -1 : nbytes;
      s->u.s.intervals = NULL;
      s->u.s.data = sdata_contents (data);
    }
  while (!mps_commit (ap, p, size));
  return make_lisp_ptr (p, Lisp_String);
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
  enum igc_type type = IGC_TYPE_INTERVAL;
  mps_ap_t ap = thread_ap (type);
  size_t nbytes = igc_round_to_pool (sizeof (struct interval), type);
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

// All lens in words
struct Lisp_Vector *
igc_alloc_pseudovector (size_t nwords_mem, size_t nwords_lisp,
			size_t nwords_zero, enum pvec_type tag)
{
  enum igc_type type = IGC_TYPE_VECTOR;
  mps_ap_t ap = thread_ap (type);
  size_t nbytes
    = igc_round_to_pool (header_size + nwords_mem * word_size, type);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      igc_static_assert (NIL_IS_ZERO);
      struct Lisp_Vector *v = p;
      memclear (v->contents, nwords_zero * word_size);
      XSETPVECTYPESIZE (v, tag, nwords_lisp, nwords_mem - nwords_lisp);
    }
  while (!mps_commit (ap, p, nbytes));
  return p;
}

struct Lisp_Vector *
igc_alloc_vector (ptrdiff_t len)
{
  enum igc_type type = (IGC_TYPE_VECTOR);
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes = igc_round_to_pool (header_size + len * word_size, type);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      igc_static_assert (NIL_IS_ZERO);
      memclear (p, nbytes);
      struct Lisp_Vector *v = p;
      v->header.size = len;
    }
  while (!mps_commit (ap, p, nbytes));
  return p;
}

struct itree_node *
igc_make_itree_node (void)
{
  enum igc_type type = IGC_TYPE_ITREE_NODE;
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes = igc_round_to_pool (sizeof (struct itree_node), type);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      igc_static_assert (NIL_IS_ZERO);
      memclear (p, nbytes);
    }
  while (!mps_commit (ap, p, nbytes));
  return p;
}

struct image *
igc_make_image (void)
{
  enum igc_type type = IGC_TYPE_IMAGE;
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes = igc_round_to_pool (sizeof (struct image), type);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      igc_static_assert (NIL_IS_ZERO);
      memclear (p, nbytes);
    }
  while (!mps_commit (ap, p, nbytes));
  return p;
}

struct face *
igc_make_face (void)
{
  enum igc_type type = IGC_TYPE_FACE;
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes = igc_round_to_pool (sizeof (struct face), type);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      igc_static_assert (NIL_IS_ZERO);
      memclear (p, nbytes);
    }
  while (!mps_commit (ap, p, nbytes));
  return p;
}

static struct hash_impl *
make_weak_hash_impl (ptrdiff_t nentries, hash_table_weakness_t weak)
{
  enum igc_type type = IGC_TYPE_WEAK;
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes_obj = hash_impl_nbytes (nentries) + sizeof (struct igc_weak);
  ptrdiff_t nbytes = igc_round_to_pool (nbytes_obj, type);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      memclear(p, nbytes);
      struct igc_weak *w = p;
      w->type = IGC_WEAK_HASH_IMPL;
      w->object_nbytes = nbytes;
      struct hash_impl *h = weak_obj (w);
      set_weakness (h, weak);
      set_table_size (h, nentries);
      set_index_bits (h, compute_hash_index_bits (nentries));
      XSETPVECTYPESIZE (h, PVEC_HASH_IMPL, 0, 0);
    }
  while (!mps_commit (ap, p, nbytes));
  return weak_obj (p);
}

struct hash_impl *
igc_make_hash_impl (ptrdiff_t nentries, hash_table_weakness_t weak)
{
  if (weak != Weak_None)
    return make_weak_hash_impl (nentries, weak);

  enum igc_type type = IGC_TYPE_VECTOR;
  mps_ap_t ap = thread_ap (type);
  ptrdiff_t nbytes = igc_round_to_pool (hash_impl_nbytes (nentries), type);
  mps_addr_t p;
  do
    {
      mps_res_t res = mps_reserve (&p, ap, nbytes);
      IGC_CHECK_RES (res);
      memclear(p, nbytes);
      struct hash_impl *h = p;
      set_weakness (h, weak);
      set_table_size (h, nentries);
      set_index_bits (h, compute_hash_index_bits (nentries));
      XSETPVECTYPESIZE (h, PVEC_HASH_IMPL, 0, 0);
    }
  while (!mps_commit (ap, p, nbytes));
  return p;
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

static mps_pool_debug_option_s debug_options = {
  "fence",
  5,
  "free",
  4,
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

  mps_gen_param_s gens[] = { { 32000, 0.8 }, { 5 * 32009, 0.4 } };
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
    MPS_ARGS_ADD (args, MPS_KEY_FMT_PAD, init->pad);
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
    MPS_ARGS_ADD (args, MPS_KEY_POOL_DEBUG_OPTIONS, &debug_options);
    MPS_ARGS_ADD (args, MPS_KEY_FORMAT, gc->fmt[type]);
    MPS_ARGS_ADD (args, MPS_KEY_CHAIN, gc->chain);
    MPS_ARGS_ADD (args, MPS_KEY_INTERIOR, init->interior_pointers);
    res
      = mps_pool_create_k (&gc->pool[type], gc->arena, init->pool_class, args);
  }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);
}

static void
runtime_setup_mps_class (struct igc_init *init)
{
  switch (init->class_type)
    {
    case IGC_AMC:
      init->pool_class = mps_class_amc ();
      break;
    case IGC_AWL:
      init->pool_class = mps_class_awl ();
      break;
    case IGC_AMCZ:
      init->pool_class = mps_class_amcz ();
      break;
    }
}

static struct igc *
make_igc (void)
{
  struct igc *gc = xzalloc (sizeof *gc);
  make_arena (gc);

  for (enum igc_type type = 0; type < IGC_TYPE_LAST; ++type)
    {
      struct igc_init *init = igc_inits + type;
      runtime_setup_mps_class (init);
      make_fmt (gc, type, init);
      make_pool (gc, type, init);
    }

  create_static_roots (gc);
  enable_messages (gc, true);
  return gc;
}

static void
free_igc (struct igc *gc)
{
  while (gc->threads)
    igc_thread_remove (gc->threads);
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

#endif

// Local Variables:
// mode: c++
// End:
