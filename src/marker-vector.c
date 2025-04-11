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
#include "marker-vector.h"

#define IDX(e, o) (marker_vector_entry_to_index (e) \
		   + MARKER_VECTOR_OFFSET_##o)

/* Access fields of an entry E of marker vecgor V as lvalues.  */
#define NEXT(v, e) (v)->contents[IDX ((e), NEXT)]
#define PREV(v, e) (v)->contents[IDX ((e), PREV)]
#define MARKER(v, e) (v)->contents[IDX ((e), MARKER)]
#define NEXT_FREE(v, e) MARKER (v, e)

/* Access header fields of marker vecgor V as lvalues.  */
#define FREE_LIST(v) (v)->contents[MARKER_VECTOR_FREE_LIST]
#define HEAD(v) (v)->contents[MARKER_VECTOR_HEAD]

/* End marker in lists.  */
#define NONE make_fixnum (-1)
#define NONEP(x) (FIXNUMP (x) && XFIXNUM(x) == -1)

/* Return a marker vector iterator for buffer B.  */

struct marker_vector_it
marker_vector_it_init (struct buffer *b)
{
  const Lisp_Object markers = BUF_MARKERS (b);
  if (VECTORP (markers))
    {
      struct Lisp_Vector *mv = XVECTOR (markers);
      const ptrdiff_t entry = XFIXNUM (HEAD (mv));
      if (entry >= 0)
	return (struct marker_vector_it)
	  {.v = mv, .entry = entry, .marker = MARKER (mv, entry)};
    }

  return (struct marker_vector_it) {.marker = Qnil};
}

/* Add entry ENTRY of V to its free-list.  */

static void
push_free_list (struct Lisp_Vector *v, const ptrdiff_t entry)
{
  NEXT_FREE (v, entry) = FREE_LIST (v);
  FREE_LIST (v) = make_fixnum (entry);
}

/* Pop the next free entry from the free-list of V and return its entry
   number.  */

static ptrdiff_t
pop_free_list (struct Lisp_Vector *v)
{
  eassert (!NONEP (FREE_LIST (v)));
  const ptrdiff_t free = XFIXNUM (FREE_LIST (v));
  FREE_LIST (v) = NEXT_FREE (v, free);
  return free;
}

/* Return the capacity of marker vector V in entries.  */

static ptrdiff_t
capacity (const struct Lisp_Vector *v)
{
  return ((v->header.size - MARKER_VECTOR_HEADER_SIZE)
	  / MARKER_VECTOR_ENTRY_SIZE);
}

/* Copy entry ENTRY of marker vector FROM to TO.  */

static void
copy_entry (struct Lisp_Vector *to, const struct Lisp_Vector *from,
	    ptrdiff_t entry)
{
  const ptrdiff_t start = marker_vector_entry_to_index (entry);
  for (ptrdiff_t i = 0; i < MARKER_VECTOR_ENTRY_SIZE; ++i)
    to->contents[start + i] = from->contents[start + i];
}

/* Copy marker VECTOR FROM to TO.  */

static void
copy (struct Lisp_Vector *to, const struct Lisp_Vector *from)
{
  FREE_LIST (to) = FREE_LIST (from);
  HEAD (to) = HEAD (from);
  for (ptrdiff_t entry = 0; entry < capacity (from); ++entry)
    copy_entry (to, from, entry);
}

/* Add a new entry for MARKER to marker array MARKERS and return
   its entry number.  */

static ptrdiff_t
add_entry (Lisp_Object markers, Lisp_Object marker)
{
  struct Lisp_Vector *v = XVECTOR (markers);
  const ptrdiff_t entry = pop_free_list (v);

  MARKER (v, entry) = marker;
  NEXT (v, entry) = HEAD (v);
  PREV (v, entry) = NONE;
  HEAD (v) = make_fixnum (entry);

  if (!NONEP (NEXT (v, entry)))
    PREV (v, XFIXNUM (NEXT (v, entry))) = make_fixnum (entry);

  return entry;
}

/* Value is true if marker vector MARKERS has enough room for a new
   entry.  */

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

/* Return a new vector that is larger then marker vector V.  V must be
   full.  */

static Lisp_Object
larger_marker_vector (Lisp_Object v)
{
  eassert (NILP (v) || (VECTORP (v) && NONEP (FREE_LIST (XVECTOR (v)))));
  const ptrdiff_t old_nentrys = NILP (v) ? 0 : capacity (XVECTOR (v));
  const ptrdiff_t new_nentrys = max (4, 2 * old_nentrys);
  const ptrdiff_t alloc_len = (new_nentrys * MARKER_VECTOR_ENTRY_SIZE
			       + MARKER_VECTOR_HEADER_SIZE);
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
  for (ptrdiff_t entry = free_start; entry < capacity (xnew_v); ++entry)
    push_free_list (xnew_v, entry + 1);

  return new_v;
}

/* Add marker M to the marker vector of B.  If B has no marker vector yet,
   make one.  If B's marker vector is full, make a new larger one.  */

void
marker_vector_add_marker (struct buffer *b, struct Lisp_Marker *m)
{
  Lisp_Object mv = BUF_MARKERS (b);
  eassert (NILP (mv) || VECTORP (mv));

  if (!has_room (mv))
    {
      mv = larger_marker_vector (mv);
      BUF_MARKERS (b) = mv;
    }

  Lisp_Object marker = make_lisp_ptr (m, Lisp_Vectorlike);
  m->mv_entry = add_entry (mv, marker);
  m->buffer = b;
}

/* Take marker at ENTRY out of the list of used markers in marker
   vector V and put it in the free-list.  */

static void
unchain (struct Lisp_Vector *v, const ptrdiff_t entry)
{
  const Lisp_Object prev = PREV (v, entry);
  const Lisp_Object next = NEXT (v, entry);

  if (NONEP (prev))
    HEAD (v) = NEXT (v, entry);
  else
    NEXT (v, XFIXNUM (prev)) = next;

  if (!NONEP (next))
    PREV (v, XFIXNUM (next)) = prev;

  push_free_list (v, entry);
}

/* Remove marker M from the markers of buffer B.  */

void
marker_vector_remove_marker (struct buffer *b, struct Lisp_Marker *m)
{
  Lisp_Object v = BUF_MARKERS (b);
  eassert (VECTORP (v));
  struct Lisp_Vector *xv = XVECTOR (v);
  eassert (m->mv_entry >= 0 && m->mv_entry < capacity (xv));
  eassert (MARKERP (MARKER (xv, m->mv_entry)));
  eassert (XMARKER (MARKER (xv, m->mv_entry)) == m);
  unchain (xv, m->mv_entry);
  m->mv_entry = -1;
  m->buffer = NULL;
}

/* Remove all markers from buffer B.  */

void
marker_vector_remove_all_markers (struct buffer *b)
{
  Lisp_Object v = BUF_MARKERS (b);
  if (VECTORP (v))
    {
      struct Lisp_Vector *xv = XVECTOR (v);
      for (ptrdiff_t entry = 0; entry < capacity (xv); ++entry)
	if (MARKERP (MARKER (xv, entry)))
	  XMARKER (MARKER (xv, entry))->buffer = NULL;
      BUF_MARKERS (b) = Qnil;
    }
}
