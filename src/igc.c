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

/* Random notes:

   - As long as some Lisp objects are managed by alloc.c, we have to use
   non-moving MPS pools, because we can't fix references in alloc.c.



   - Use mps_arena_step during idle time. This lets MPS take a specified
     maximum amount of time (default 10ms) for its work.

*/

// clang-format on

/* In MPS scan functions it is not easy to call C functions (see the MPS
   documentation). Rather than taking the risk of using functions from
   lisp.h which may may not be inlined, I'm therfore using some macros,
   and assume that Lisp_Objs are EMACS_INTs, and we are using the 3
   lowest bits for tags.  */

#define IGC_TAG(obj) ((EMACS_INT) (obj) & 0x7)
#define IGC_UNTAGGED(obj) ((EMACS_INT) (obj) & ~0x7)
#define IGC_MAKE_LISP_OBJ(untagged, tag) \
  ((Lisp_Object) ((EMACS_INT) (untagged) | (tag)))
#define IGC_FIXNUMP(obj) \
  (IGC_TAG (obj) == Lisp_Int0 || IGC_TAG (obj) == Lisp_Int1)


#include <config.h>
#include "lisp.h"
#include "igc.h"

#ifdef HAVE_MPS

#if !USE_LSB_TAG
# error "Need USE_LSB_TAG"
#endif

#include <stdlib.h>
#include <mps.h>
#include <mpsavm.h>
#include <mpscamc.h>
#include <mpscams.h>

static mps_arena_t arena = NULL;
static mps_chain_t chain;
static mps_pool_t cons_pool;

struct igc_root
{
  struct igc_root *next, *prev;
  mps_root_t root;
};

struct igc_root *roots = NULL;

static struct igc_root *
register_root (mps_root_t root)
{
  struct igc_root *r = xmalloc (sizeof *r);
  r->next = roots;
  r->prev = NULL;
  if (r->next)
    r->next->prev = r;
  roots = r;
  return r;
}

static mps_root_t
deregister_root (struct igc_root *r)
{
  mps_root_t root = r->root;
  if (r->next)
    r->next->prev = r->prev;
  if (r->prev)
    r->prev->next = r->next;
  if (r == roots)
    roots = r->next;
  xfree (r);
  return root;
}

struct igc_root *
igc_add_mem_root (void *start, void *end)
{
  mps_res_t res;
  mps_root_t root;
  res = mps_root_create_area (&root);
  if (res != MPS_RES_OK)
    emacs_abort ();
  return register_root (root);
}

void
igc_remove_root (struct igc_root *r)
{
  mps_root_destroy (deregister_root (r));
}

static void
remove_all_roots (void)
{
  while (roots)
    igc_remove_root (roots);
}

/* Fix the Lisp_Object at *P. SS is the MPS scan state.

   We need to tag/untag Lisp_Objects to work with MPS.. And one cannot
   easily call functions from scan functions according to the MPS
   documentation (see there). So, I'm treating them as EMACS_INTs.  */

# define IGC_FIX_LISP_OBJ(ss, p)			      \
  if (!IGC_FIXNUMP (*(p)))				      \
    {							      \
      EMACS_INT _untagged = IGC_UNTAGGED (*(p));	      \
      mps_addr_t _addr = (mps_addr_t) _untagged;	      \
      if (MPS_FIX1 ((ss), _addr))			      \
	{						      \
	  mps_res_t _res = MPS_FIX2 ((ss), &_addr);	      \
	  if (_res != MPS_RES_OK)			      \
	    return _res;				      \
	  EMACS_INT _tag = IGC_TAG (*(p));		      \
	  *(p) = IGC_MAKE_LISP_OBJ (_addr, _tag);	      \
	}						      \
    }							      \
  else

static mps_res_t
cons_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN (ss);
  struct Lisp_Cons *cons = (struct Lisp_Cons *) base;
  IGC_FIX_LISP_OBJ (ss, &cons->u.s.car);
  IGC_FIX_LISP_OBJ (ss, &cons->u.s.u.cdr);
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_addr_t
cons_skip (mps_addr_t addr)
{
  const struct Lisp_Cons *cons = addr;
  return (mps_addr_t) (cons + 1);
}

static void
cons_fwd (mps_addr_t old, mps_addr_t new)
{
  // unclear
}

static mps_addr_t
cons_isfwd (mps_addr_t addr)
{
  return NULL;
}

static void
cons_pad (mps_addr_t addr, size_t size)
{
}

static void
create_arena (void)
{
  mps_res_t res;

  MPS_ARGS_BEGIN (args) {
    res = mps_arena_create_k (&arena, mps_arena_class_vm (), args);
  } MPS_ARGS_END (args);
  if (res != MPS_RES_OK)
    emacs_abort ();

  // Generations
  mps_gen_param_s gen_params[] = {
    {32000, 0.8},
    {5 * 32009, 0.4}
  };
  res = mps_chain_create (&chain, arena, ARRAYELTS (gen_params), gen_params);
  if (res != MPS_RES_OK)
    emacs_abort ();

  // Object format for conses.
  mps_fmt_t cons_fmt;
  MPS_ARGS_BEGIN (args) {
    MPS_ARGS_ADD (args, MPS_KEY_FMT_ALIGN, 8);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_HEADER_SIZE, 0);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, cons_scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, cons_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, cons_fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, cons_isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, cons_pad);
    res = mps_fmt_create_k (&cons_fmt, arena, args);
  } MPS_ARGS_END (args);
  if (res != MPS_RES_OK)
    emacs_abort ();

  // Pool for conses. Since conses have no type field which would let us
  // recognize them when mixed with other objects, use a dedicated pool.
  MPS_ARGS_BEGIN (args) {
    MPS_ARGS_ADD (args, MPS_KEY_FORMAT, cons_fmt);
    MPS_ARGS_ADD (args, MPS_KEY_CHAIN, chain);
    MPS_ARGS_ADD (args, MPS_KEY_INTERIOR, 0);
    res = mps_pool_create_k (&cons_pool, arena, mps_class_ams (), args);
  } MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    emacs_abort ();
}

static void
destroy_arena (void)
{
  remove_all_roots ();
  mps_arena_destroy (arena);
}

void
syms_of_igc (void)
{
}

void
init_igc_once (void)
{
  if (!arena)
    {
      create_arena ();
      atexit (destroy_arena);
    }
}

void
init_igc (void)
{
  init_igc_once ();
}

#endif // HAVE_MPS
