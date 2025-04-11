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

// Don't use next_free / set_next_free
// Maybe use macros for lvalue

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

#define IDX(s, o) (marker_array_slot_to_index (s) + MARKER_ARRAY_OFFSET_##o)
#define NEXT(v, s) (v)->contents[IDX (s, NEXT)]
#define PREV(v, s) (v)->contents[IDX (s, PREV)]
#define MARKER(v, s) (v)->contents[IDX (s, MARKER)]
#define NEXT_FREE(v, s) MARKER (v, s)
#define FREE_LIST(v) (v)->contents[MARKER_ARRAY_FREE_LIST]
#define HEAD(v) (v)->contents[MARKER_ARRAY_HEAD]
#define NONE make_fixnum (-1)
#define NONEP(x) (FIXNUMP (x) && XFIXNUM(x) == -1)

/* Add entry SLOT of V to its free-list.  */

static void
push_free_list (struct Lisp_Vector *v, ptrdiff_t slot)
{
  NEXT_FREE (v, slot) = FREE_LIST (v);
  FREE_LIST (v) = make_fixnum (slot);
}

/* Pop the next free slot from the free-list of V.
   Return its slot number.  */

static ptrdiff_t
pop_free_list (struct Lisp_Vector *v)
{
  eassert (!NONEP (FREE_LIST (v)));
  const ptrdiff_t free = XFIXNUM (FREE_LIST (v));
  FREE_LIST (v) = NEXT_FREE (v, free);
  return free;
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
  FREE_LIST (to) = FREE_LIST (from);
  HEAD (to) = HEAD (from);
  for (ptrdiff_t slot = 0; slot < capacity (from); ++slot)
    copy_entry (to, from, slot);
}

static ptrdiff_t
add_entry (Lisp_Object markers, Lisp_Object marker)
{
  struct Lisp_Vector *v = XVECTOR (markers);
  const ptrdiff_t slot = pop_free_list (v);

  MARKER (v, slot) = marker;
  NEXT (v, slot) = HEAD (v);
  PREV (v, slot) = NONE;
  const ptrdiff_t old_head = XFIXNUM (marker_array_head (v));
  HEAD (v) = make_fixnum (slot);
  if (old_head >= 0)
    PREV (v, old_head) = make_fixnum (slot);

  return slot;
}

static bool
has_room (Lisp_Object markers)
{
  return VECTORP (markers) && !NONEP (FREE_LIST (XVECTOR (markers)));
}

static Lisp_Object
alloc_marker_vector (ptrdiff_t len, Lisp_Object init)
{
  return Qnil;
}

static Lisp_Object
larger_marker_array (Lisp_Object v)
{
  eassert (NILP (v) || (VECTORP (v) && NONEP (FREE_LIST (XVECTOR (v)))));
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
      copy (xnew_v, xv);
      free_start = capacity (xv);
    }
  else
    {
      HEAD (xnew_v) = NONE;
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
  Lisp_Object markers = BUF_MARKERS (b);
  eassert (NILP (markers) || VECTORP (markers));

  if (!has_room (markers))
    {
      markers = larger_marker_array (markers);
      BUF_MARKERS (b) = markers;
    }

  m->slot = add_entry (markers, make_lisp_ptr (m, Lisp_Vectorlike));
  m->buffer = b;
}

static void
unchain (struct Lisp_Vector *v, const ptrdiff_t slot)
{
  MARKER (v, slot) = FREE_LIST (v);
  FREE_LIST (v) = make_fixnum (slot);

  Lisp_Object prev = PREV (v, slot);
  Lisp_Object next = NEXT (v, slot);

  if (NONEP (prev))
    HEAD (v) = NEXT (v, slot);
  else
    NEXT (v, XFIXNUM (prev)) = next;

  if (!NONEP (next))
    PREV (v, XFIXNUM (next)) = prev;

  push_free_list (v, slot);
}

void
marker_array_remove_marker (struct buffer *b, struct Lisp_Marker *m)
{
  Lisp_Object v = BUF_MARKERS (b);
  eassert (VECTORP (v));
  struct Lisp_Vector *xv = XVECTOR (v);
  eassert (m->slot >= 0 && m->slot < capacity (xv));
  eassert (MARKERP (MARKER (xv, m->slot)));
  eassert (XMARKER (MARKER (xv, m->slot)) == m);
  unchain (xv, m->slot);
  m->slot = -1;
  m->buffer = NULL;
}

#if 0
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
