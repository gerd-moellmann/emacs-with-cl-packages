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

- Use mps_arena_step during idle time. This lets MPS take a specified
  maximum amount of time (default 10ms) for its work.

*/

// clang-format on

#include <config.h>
#include "lisp.h"

#ifdef HAVE_MPS

#if !USE_LSB_TAG
# error "Need USE_LSB_TAG"
#endif

#include <stdlib.h>
#include <mps.h>
#include <mpsavm.h>
#include "mpscamc.h"

static mps_arena_t arena = NULL;
static mps_chain_t chain;
static mps_pool_t cons_pool;

static mps_res_t
cons_scan (mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
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

  mps_gen_param_s gen_params[] = {
    {32000, 0.8},
    {5 * 32009, 0.4}
  };
  res = mps_chain_create(&chain, arena, ARRAYELTS (gen_params), gen_params);
  if (res != MPS_RES_OK)
    emacs_abort ();

  /* Object format for conses.  */
  mps_fmt_t cons_fmt;
  MPS_ARGS_BEGIN (args) {
    MPS_ARGS_ADD (args, MPS_KEY_FMT_ALIGN, 8);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_HEADER_SIZE, 0);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, cons_scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, cons_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, cons_fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, cons_isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, cons_pad);
    res = mps_fmt_create_k(&cons_fmt, arena, args);
  } MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    emacs_abort ();

  /* Pool for conses. Since conses have no type field which would let us
     recognize them when mixed with other objects, use a dedicated
     pool.  */
  MPS_ARGS_BEGIN (args) {
    MPS_ARGS_ADD (args, MPS_KEY_FORMAT, cons_fmt);
    MPS_ARGS_ADD (args, MPS_KEY_CHAIN, chain);
    MPS_ARGS_ADD (args, MPS_KEY_INTERIOR, 0);
    res = mps_pool_create_k (&cons_pool, arena, mps_class_amc (), args);
  } MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    emacs_abort ();
}

static void
destroy_arena (void)
{
  mps_arena_destroy (arena);
}

/* Not called when starting a dumped Emacs.  */

void
syms_of_igc (void)
{
}

/* Not called when starting a dumped Emacs.  */

void
init_igc_once (void)
{
  if (!arena)
    {
      create_arena ();
      atexit (destroy_arena);
    }
}

/* Called when starting a dumped Emacs.  */

void
init_igc (void)
{
  init_igc_once ();
}

#endif // HAVE_MPS
