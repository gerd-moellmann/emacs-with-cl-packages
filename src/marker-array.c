/* Arrays of markers.
   Copyright (C) 2025 Free Software Foundation, Inc.

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

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "marker-array.h"

/* Return a marker array iterator for buffer B.  */

struct marker_array_it
marker_array_it_init (struct buffer *b)
{
  if (VECTORP (BUF_MARKERS (b)))
    {
      const Lisp_Object markers = BUF_MARKERS (b);
      struct Lisp_Vector *v = XVECTOR (markers);
      const ptrdiff_t slot = XFIXNUM (marker_array_head (v));
      if (slot >= 0)
	return (struct marker_array_it)
	  {.slot = slot, .v = v, .marker = marker_array_marker (v, slot)};
    }
  return (struct marker_array_it) {.marker = Qnil};
}

static void
set_next (struct Lisp_Vector *v, ptrdiff_t slot, Lisp_Object next)
{
  const ptrdiff_t i
    = marker_array_slot_to_index (slot) + MARKER_ARRAY_OFFSET_NEXT;
  v->contents[i] = next;
}

static void
set_prev (struct Lisp_Vector *v, ptrdiff_t slot, Lisp_Object prev)
{
  const ptrdiff_t i
    = marker_array_slot_to_index (slot) + MARKER_ARRAY_OFFSET_PREV;
  v->contents[i] = prev;
}

static Lisp_Object
next_free (struct Lisp_Vector *v, ptrdiff_t slot)
{
  const ptrdiff_t i
    = marker_array_slot_to_index (slot) + MARKER_ARRAY_OFFSET_MARKER;
  return v->contents[i];
}

static void
set_next_free (struct Lisp_Vector *v, ptrdiff_t slot, Lisp_Object next)
{
  const ptrdiff_t i
    = marker_array_slot_to_index (slot) + MARKER_ARRAY_OFFSET_MARKER;
  v->contents[i] = next;
}

static Lisp_Object
free_list (const struct Lisp_Vector *v)
{
  return v->contents[MARKER_ARRAY_FREE_LIST];
}

static void
set_free_list (struct Lisp_Vector *v, Lisp_Object slot)
{
  v->contents[MARKER_ARRAY_FREE_LIST] = slot;
}

static void
push_free_list (struct Lisp_Vector *v, ptrdiff_t slot)
{
  set_next (v, slot, make_fixnum (-1));
  set_prev (v, slot, make_fixnum (-1));
  set_next_free (v, slot, free_list (v));
  set_free_list (v, make_fixnum (slot));
}

static ptrdiff_t
pop_free_list (struct Lisp_Vector *v)
{
  ptrdiff_t free = XFIXNUM (free_list (v));
  set_free_list (v, next_free (v, free));
  return free;
}

static Lisp_Object
head (const struct Lisp_Vector *v)
{
  return v->contents[MARKER_ARRAY_HEAD];
}

static Lisp_Object
set_head (struct Lisp_Vector *v, Lisp_Object slot)
{
  return v->contents[MARKER_ARRAY_HEAD] = slot;
}

static ptrdiff_t
capacity (const struct Lisp_Vector *v)
{
  return ((v->header.size - MARKER_ARRAY_HEADER_SIZE)
	  / MARKER_ARRAY_ENTRY_SIZE);
}

static void
copy_entry (struct Lisp_Vector *to, const struct Lisp_Vector *from,
	    ptrdiff_t slot)
{
  const ptrdiff_t start = marker_array_slot_to_index (slot);
  for (ptrdiff_t i = 0; i < MARKER_ARRAY_ENTRY_SIZE; ++i)
    to->contents[start + i] = from->contents[start + i];
}

static void
copy (struct Lisp_Vector *to, const struct Lisp_Vector *from)
{
  for (ptrdiff_t slot = 0; slot < capacity (from); ++slot)
    copy_entry (to, from, slot);
}

static Lisp_Object
alloc_marker_vector (ptrdiff_t len, Lisp_Object init)
{
  return Qnil;
}

static Lisp_Object
larger_marker_array (Lisp_Object v)
{
  eassert (NILP (v)
	   || (VECTORP (v)
	       && XFIXNUM (free_list (XVECTOR (v))) < 0));
  const ptrdiff_t old_nslots = NILP (v) ? 0 : capacity (XVECTOR (v));
  const ptrdiff_t new_nslots = max (4, 2 * old_nslots);
  const ptrdiff_t alloc_len = (new_nslots * MARKER_ARRAY_ENTRY_SIZE
			       + MARKER_ARRAY_HEADER_SIZE);
  Lisp_Object new_v = alloc_marker_vector (alloc_len, Qnil);

  /* Copy existing items. */
  struct Lisp_Vector *xnew_v = XVECTOR (new_v);
  ptrdiff_t free_start;
  if (VECTORP (v))
    {
      struct Lisp_Vector *xv = XVECTOR (v);
      set_free_list (xnew_v, free_list (xv));
      set_head (xnew_v, head (xv));
      copy (xnew_v, xv);
      free_start = capacity (xv);
    }
  else
    {
      set_head (xnew_v, make_fixnum (-1));
      free_start = 0;
    }

  /* Add new items to free-list.  */
  for (ptrdiff_t slot = free_start; slot < capacity (xnew_v); ++slot)
    push_free_list (xnew_v, slot + 1);

  return new_v;
}

void
marker_array_add_marker (struct buffer *b, struct Lisp_Marker *m)
{
  Lisp_Object v = BUF_MARKERS (b);
  eassert (NILP (v) || VECTORP (v));
  struct Lisp_Vector *xv = NILP (v) ? NULL : XVECTOR (v);

  ptrdiff_t slot = NILP (v) ? -1 : XFIXNUM (free_list (xv));
  if (slot < 0)
    {
      v = BUF_MARKERS (b) = larger_marker_array (v);
      xv = XVECTOR (v);
      slot = XFIXNUM (free_list (xv));
    }

  IGC_MA_FREE_LIST (xv) = IGC_MA_MARKER (xv, slot);
  IGC_MA_MARKER (xv, slot) = make_lisp_ptr (m, Lisp_Vectorlike);
  IGC_MA_NEXT (xv, slot) = IGC_MA_HEAD (xv);
  IGC_MA_PREV (xv, slot) = make_fixnum (-1);
  IGC_MA_HEAD (xv) = make_fixnum (slot);

  ptrdiff_t next = XFIXNUM (IGC_MA_NEXT (xv, slot));
  if (next >= 0)
    IGC_MA_PREV (xv, next) = make_fixnum (slot);
  m->slot = slot;
  m->buffer = b;
}

#if 0
void
igc_remove_marker (struct buffer *b, struct Lisp_Marker *m)
{
  Lisp_Object v = BUF_MARKERS (b);
  igc_assert (VECTORP (v));
  struct Lisp_Vector *xv = XVECTOR (v);
  igc_assert (m->slot >= 0 && m->slot < IGC_MA_CAPACITY (xv));
  igc_assert (MARKERP (IGC_MA_MARKER (xv, m->slot))
	      && XMARKER (IGC_MA_MARKER (xv, m->slot)) == m);
  unchain (xv, m->slot);
  m->slot = -1;
  m->buffer = NULL;
}

void
igc_remove_all_markers (struct buffer *b)
{
  Lisp_Object v = BUF_MARKERS (b);
  if (VECTORP (v))
    {
      struct Lisp_Vector *xv = XVECTOR (v);
      for (ptrdiff_t slot = 0; slot < IGC_MA_CAPACITY (xv); ++slot)
	if (MARKERP (IGC_MA_MARKER (xv, slot)))
	  XMARKER (IGC_MA_MARKER (xv, slot))->buffer = NULL;
      BUF_MARKERS (b) = Qnil;
    }
}
#endif
