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

// clang-format off

#include <config.h>
#include "lisp.h"

#ifdef HAVE_MPS
#include <stdlib.h>
#include <mps.h>
#include <mpsavm.h>

static mps_arena_t arena = NULL;

static void
create_arena (void)
{
  mps_res_t res;
  MPS_ARGS_BEGIN (args) {
    MPS_ARGS_ADD (args, MPS_KEY_PAUSE_TIME, 0.01);
    res = mps_arena_create_k(&arena, mps_arena_class_vm (), args);
  } MPS_ARGS_END (args);
  if (res != MPS_RES_OK)
    abort ();
}

static void
destroy_arena (void)
{
  if (!arena)
    return;
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
