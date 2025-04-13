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

/* A marker vector is used to hold the markers of a buffer. The vector
   is a normal Lisp vector that consists of a header and a number of
   entries for each marker.

   +------+------+---------+---------+-------------+
   | HEAD | FREE | entry 0 | entry 1 | ...         |
   +------+------+---------+---------+-------------+
   |<- header -->|

   Entries consist of 3 vector slots MARKER, NEXT and PREV. MARKER
   holds a marker, if the entry is in use. NEXT and PREV are entry
   numbers used to form a doubly-linked list in the marker vector.

   HEAD is the entry number of the start of the doubly-linked list
   of used entries.

   FREE is the entry number of the next free entry in the marker vector.
   Free entries form a singly-linked list using the NEXT field of
   entries. */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "marker-vector.h"
#include "igc.h"

#define IDX(e, o) (marker_vector_entry_to_index (e) \
		   + MARKER_VECTOR_OFFSET_##o)

/* Access fields of an entry E of marker vecgor V as lvalues.  */
#define NEXT(v, e) (v)->contents[IDX ((e), NEXT)]
#define PREV(v, e) (v)->contents[IDX ((e), PREV)]
#define MARKER(v, e) (v)->contents[IDX ((e), MARKER)]

/* Access header fields of marker vecgor V as lvalues.  */
#define FREE(v) (v)->contents[MARKER_VECTOR_FREE]
#define HEAD(v) (v)->contents[MARKER_VECTOR_HEAD]

/* End marker in lists.  */
#define NONE make_fixnum (-1)
#define IS_NONE(x) (FIXNUMP (x) && XFIXNUM (x) == -1)

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

/* Return the capacity of marker vector V in number of entries.  */

static ptrdiff_t
capacity (const struct Lisp_Vector *v)
{
  return ((v->header.size - MARKER_VECTOR_HEADER_SIZE)
	  / MARKER_VECTOR_ENTRY_SIZE);
}

/* Add entry ENTRY of V to its free-list.  */

static void
push_free (struct Lisp_Vector *v, const ptrdiff_t entry)
{
  MARKER (v, entry) = Qnil;
  PREV (v, entry) = NONE;
  NEXT (v, entry) = FREE (v);
  FREE (v) = make_fixnum (entry);
}

/* Pop the next free entry from the free-list of V and return its entry
   number.  */

static ptrdiff_t
pop_free (struct Lisp_Vector *v)
{
  eassert (!IS_NONE (FREE (v)));
  const ptrdiff_t free = XFIXNUM (FREE (v));
  eassert (free >= 0 && free < capacity (v));
  FREE (v) = NEXT (v, free);
  return free;
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
    HEAD (v) = next;
  else
    NEXT (v, XFIXNUM (prev)) = next;

  if (!IS_NONE (next))
    PREV (v, XFIXNUM (next)) = prev;
}

static Lisp_Object
alloc_marker_vector (ptrdiff_t len)
{
#ifdef HAVE_MPS
  return igc_alloc_marker_vector (len);
#endif
}

/* Return a new marker vector that is like OLD_MV but larger.  */

static Lisp_Object
larger_marker_vector (Lisp_Object old_mv)
{
  const ptrdiff_t old_capacity = NILP (old_mv) ? 0 : capacity (XVECTOR (old_mv));
  const ptrdiff_t new_capacity = max (10, 2 * old_capacity);
  const ptrdiff_t alloc_len = (new_capacity * MARKER_VECTOR_ENTRY_SIZE
			       + MARKER_VECTOR_HEADER_SIZE);
  Lisp_Object new_mv = alloc_marker_vector (alloc_len);
  struct Lisp_Vector *new_v = XVECTOR (new_mv);

  /* Copy existing entries. */
  if (VECTORP (old_mv))
    {
      struct Lisp_Vector *old_v = XVECTOR (old_mv);
      eassert (IS_NONE (FREE (old_v)));
      const size_t nbytes = old_v->header.size * sizeof (Lisp_Object);
      memcpy (new_v->contents, old_v->contents, nbytes);
    }
  else
    {
      HEAD (new_v) = NONE;
      FREE (new_v) = NONE;
    }

  /* Add rest of entries to free-list.  */
  for (ptrdiff_t e = new_capacity - 1; e >= old_capacity; --e)
    push_free (new_v, e);

  return new_mv;
}

static void
check_marker_vector (const struct Lisp_Vector *v)
{
  size_t nfree = 0;
  for (Lisp_Object e = FREE (v); !IS_NONE (e); e = NEXT (v, XFIXNUM (e)))
    {
      ptrdiff_t i = XFIXNUM (e);
      eassert (NILP (MARKER (v, i)));
      eassert (IS_NONE (PREV (v, i)));
      ++nfree;
    }

  size_t nused = 0;
  for (Lisp_Object e = HEAD (v); !IS_NONE (e); e = NEXT (v, XFIXNUM (e)))
    {
      ptrdiff_t i = XFIXNUM (e);
      eassert (MARKERP (MARKER (v, i)));
      struct Lisp_Marker *m = XMARKER (MARKER (v, i));
      eassert (m->entry == i);
      eassert (m->buffer != NULL);
      struct Lisp_Vector *mv = XVECTOR (BUF_MARKERS (m->buffer));
      eassert (mv == v);
      ++nused;
    }

  size_t ndo = 0;
  DO_MARKER_SLOTS (v, i)
  {
    Lisp_Object marker = v->contents[i];
    eassert (NILP (marker) || MARKERP (marker));
    const ptrdiff_t entry = (i - MARKER_VECTOR_HEADER_SIZE) / MARKER_VECTOR_ENTRY_SIZE;
    eassert (EQ (marker, MARKER (v, entry)));
    if (MARKERP (marker))
      {
	struct Lisp_Marker *m = XMARKER (marker);
	eassert (XVECTOR (BUF_MARKERS (m->buffer)) == v);
	eassert (m->entry == entry);
      }
    ++ndo;
  }
  END_DO_MARKERS;

  eassert (ndo == nused + nfree);
  eassert ((nused + nfree) * MARKER_VECTOR_ENTRY_SIZE
	   + MARKER_VECTOR_HEADER_SIZE == v->header.size);
}

/* Make sure that the marker vector of B has room for a new entry.
   Value is the marker vector.  */

static Lisp_Object
make_room_for_entry (struct buffer *b)
{
  Lisp_Object mv = BUF_MARKERS (b);
  if (NILP (mv) || IS_NONE (FREE (XVECTOR (mv))))
    {
      mv = larger_marker_vector (mv);
      BUF_MARKERS (b) = mv;
    }
  return mv;
}

/* Add marker M to the marker vector of B.  If B has no marker vector yet,
   make one.  If B's marker vector is full, make a new larger one.  */

void
marker_vector_add (struct buffer *b, struct Lisp_Marker *m)
{
  Lisp_Object mv = make_room_for_entry (b);
  m->entry = add_entry (mv, m);
  m->buffer = b;
  check_marker_vector (XVECTOR (mv));
}

/* Remove marker M from the markers of buffer B.  */

void
marker_vector_remove (struct Lisp_Vector *v, struct Lisp_Marker *m)
{
  eassert (m->entry >= 0 && m->entry < capacity (v));
  eassert (MARKERP (MARKER (v, m->entry)));
  eassert (XMARKER (MARKER (v, m->entry)) == m);
  remove_entry (v, m->entry);
  push_free (v, m->entry);
  m->entry = -1;
  m->buffer = NULL;
  check_marker_vector (v);
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
	  {
	    struct Lisp_Marker *m = XMARKER (MARKER (v, entry));
	    m->buffer = NULL;
	    m->entry = -1;
	  }
      BUF_MARKERS (b) = Qnil;
    }
}
