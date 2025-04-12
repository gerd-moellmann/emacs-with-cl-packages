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
#define FREE(v) (v)->contents[MARKER_VECTOR_FREE]
#define HEAD(v) (v)->contents[MARKER_VECTOR_HEAD]

/* End marker in lists.  */
#define NONE make_fixnum (-1)
#define IS_NONE(x) (FIXNUMP (x) && XFIXNUM(x) == -1)

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
push_free (struct Lisp_Vector *v, const ptrdiff_t entry)
{
  NEXT_FREE (v, entry) = FREE (v);
  FREE (v) = make_fixnum (entry);
}

/* Pop the next free entry from the free-list of V and return its entry
   number.  */

static ptrdiff_t
pop_free (struct Lisp_Vector *v)
{
  eassert (!IS_NONE (FREE (v)));
  const ptrdiff_t free = XFIXNUM (FREE (v));
  FREE (v) = NEXT_FREE (v, free);
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
  FREE (to) = FREE (from);
  HEAD (to) = HEAD (from);
  for (ptrdiff_t entry = 0; entry < capacity (from); ++entry)
    copy_entry (to, from, entry);
}

/* Value is true if marker vector MARKERS has enough room for a new
   entry.  */

static bool
has_room (Lisp_Object markers)
{
  return VECTORP (markers) && !IS_NONE (FREE (XVECTOR (markers)));
}

/* Add a new entry for M to marker array MV and return
   its entry number.  */

static ptrdiff_t
add_entry (Lisp_Object mv, struct Lisp_Marker *m)
{
  struct Lisp_Vector *v = XVECTOR (mv);
  const ptrdiff_t entry = pop_free (v);

  Lisp_Object marker = make_lisp_ptr (m, Lisp_Vectorlike);
  MARKER (v, entry) = marker;
  NEXT (v, entry) = HEAD (v);
  PREV (v, entry) = NONE;
  HEAD (v) = make_fixnum (entry);

  if (!IS_NONE (NEXT (v, entry)))
    PREV (v, XFIXNUM (NEXT (v, entry))) = make_fixnum (entry);

  return entry;
}

/* Take ENTRY out of the list of used markers in marker vector V..  */

static void
remove_entry (struct Lisp_Vector *v, const ptrdiff_t entry)
{
  const Lisp_Object prev = PREV (v, entry);
  const Lisp_Object next = NEXT (v, entry);

  if (IS_NONE (prev))
    HEAD (v) = NEXT (v, entry);
  else
    NEXT (v, XFIXNUM (prev)) = next;

  if (!IS_NONE (next))
    PREV (v, XFIXNUM (next)) = prev;
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
  eassert (NILP (v) || (VECTORP (v) && IS_NONE (FREE (XVECTOR (v)))));
  const ptrdiff_t old_nentrys = NILP (v) ? 0 : capacity (XVECTOR (v));
  const ptrdiff_t new_nentrys = max (4, 2 * old_nentrys);
  const ptrdiff_t alloc_len = (new_nentrys * MARKER_VECTOR_ENTRY_SIZE
			       + MARKER_VECTOR_HEADER_SIZE);
  Lisp_Object new_v = alloc_marker_vector (alloc_len, Qnil);

  /* Copy existing entries. */
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

  /* Add rest of entries to free-list.  */
  for (ptrdiff_t entry = free_start; entry < capacity (xnew_v); ++entry)
    push_free (xnew_v, entry + 1);

  return new_v;
}

/* Add marker M to the marker vector of B.  If B has no marker vector yet,
   make one.  If B's marker vector is full, make a new larger one.  */

void
marker_vector_add (struct buffer *b, struct Lisp_Marker *m)
{
  Lisp_Object mv = BUF_MARKERS (b);
  eassert (NILP (mv) || VECTORP (mv));

  if (!has_room (mv))
    {
      mv = larger_marker_vector (mv);
      BUF_MARKERS (b) = mv;
    }

  m->entry = add_entry (mv, m);
  m->buffer = b;
}

/* Remove marker M from the markers of buffer B.  */

void
marker_vector_remove (struct buffer *b, struct Lisp_Marker *m)
{
  Lisp_Object mv = BUF_MARKERS (b);
  eassert (VECTORP (mv));
  struct Lisp_Vector *v = XVECTOR (mv);
  eassert (m->entry >= 0 && m->entry < capacity (v));
  eassert (MARKERP (MARKER (v, m->entry)));
  eassert (XMARKER (MARKER (v, m->entry)) == m);
  remove_entry (v, m->entry);
  push_free (v, m->entry);
  m->entry = -1;
  m->buffer = NULL;
}

/* Remove all markers from buffer B.  */

void
marker_vector_clear (struct buffer *b)
{
  Lisp_Object mv = BUF_MARKERS (b);
  if (VECTORP (mv))
    {
      struct Lisp_Vector *v = XVECTOR (mv);
      for (ptrdiff_t entry = 0; entry < capacity (v); ++entry)
	if (MARKERP (MARKER (v, entry)))
	  XMARKER (MARKER (v, entry))->buffer = NULL;
      BUF_MARKERS (b) = Qnil;
    }
}
