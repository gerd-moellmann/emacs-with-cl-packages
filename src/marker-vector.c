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
   entries for each marker. A Lisp vector is used because the vector
   references markers "weakly", and that's what easy for igc.

   +-------------+---------+---------+-------------+
   | FREE        | entry 0 | entry 1 | ...         |
   +-------------+---------+---------+-------------+
   |<- header -->|

   Entries consist of 3 vector slots MARKER, BYTEPOS and CHARPOS. MARKER
   holds a marker, if the entry is in use. BYTEPOS and CHARPOS are not
   yet used. (The idea is to move the positions from markers here,
   which speeds up adjusting positions when the text changes.)

   FREE is the array index of the next free entry in the marker vector.
   Free entries form a singly-linked list using the MARKER field of
   entries.

   Iteration over marker vectors is done naively by iterating over
   all slots of the vector that can contain markers, skipping
   those that don't.  */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "marker-vector.h"
#include "igc.h"

/* Minimum number of entries to allocate.  */
#define MARKER_ARRAY_MIN_SIZE ((100 * MARKER_VECTOR_ENTRY_SIZE) \
			       + MARKER_VECTOR_HEADER_SIZE)

#define IDX(e, o) ((e) + MARKER_VECTOR_OFFSET_##o)

/* Access fields of an entry E of marker vecgor V as lvalues.  */
#define BYTEPOS(v, e) (v)->contents[IDX ((e), BYTEPOS)]
#define CHARPOS(v, e) (v)->contents[IDX ((e), CHARPOS)]
#define MARKER(v, e) (v)->contents[IDX ((e), MARKER)]
#define NEXT_FREE(v, e) MARKER (v, e)

/* Access header fields of marker vecgor V as lvalues.  */
#define FREE(v) (v)->contents[MARKER_VECTOR_FREE]

/* Return a marker vector iterator for buffer B.  */

struct marker_vector_it
marker_vector_it_init (struct buffer *b)
{
  Lisp_Object mv = BUF_MARKERS (b);
  if (NILP (mv))
    return (struct marker_vector_it) {.i = 0};

  for (ptrdiff_t i = MARKER_VECTOR_HEADER_SIZE + MARKER_VECTOR_OFFSET_MARKER;
       i < ASIZE (mv); i += MARKER_VECTOR_ENTRY_SIZE)
    if (MARKERP (AREF (mv, i)))
      return (struct marker_vector_it) {.i = i, .mv = mv};

  return (struct marker_vector_it) {.i = 0};
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
  NEXT_FREE (v, entry) = FREE (v);
  FREE (v) = make_fixnum (entry);
}

/* Pop the next free entry from the free-list of V and return its entry
   number.  */

static ptrdiff_t
pop_free (struct Lisp_Vector *v)
{
  eassert (!NILP (FREE (v)));
  const ptrdiff_t free = XFIXNUM (FREE (v));
  eassert (free >= 0 && free < capacity (v));
  FREE (v) = NEXT_FREE (v, free);
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
  return entry;
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
  const ptrdiff_t old_size = NILP (old_mv) ? 0 : ASIZE (old_mv);
  const ptrdiff_t new_size = max (MARKER_ARRAY_MIN_SIZE, 2 * old_size);
  Lisp_Object new_mv = alloc_marker_vector (new_size);
  struct Lisp_Vector *new_v = XVECTOR (new_mv);

  /* Copy existing entries. */
  if (VECTORP (old_mv))
    {
      struct Lisp_Vector *old_v = XVECTOR (old_mv);
      eassert (NILP (FREE (old_v)));
      const size_t nbytes = ASIZE (old_mv) * sizeof (Lisp_Object);
      memcpy (new_v->contents, old_v->contents, nbytes);
    }

  /* Add new entries to free-list.  */
  for (ptrdiff_t e = new_size - MARKER_VECTOR_ENTRY_SIZE;
       e >= MARKER_VECTOR_HEADER_SIZE; e -= MARKER_VECTOR_ENTRY_SIZE)
    push_free (new_v, e);

  return new_mv;
}

static void
check_marker_vector (const struct Lisp_Vector *v)
{
  size_t nfree = 0;
  for (Lisp_Object e = FREE (v); !NILP (e); e = NEXT_FREE (v, XFIXNUM (e)))
    ++nfree;

  size_t nused = 0;
  for (ptrdiff_t e = MARKER_VECTOR_HEADER_SIZE;
       e < capacity (v); e += MARKER_VECTOR_ENTRY_SIZE)
    {
      if (MARKERP (MARKER (v, e)))
	{
	  struct Lisp_Marker *m = XMARKER (MARKER (v, e));
	  eassert (m->entry == e);
	  eassert (m->buffer != NULL);
	  struct Lisp_Vector *mv = XVECTOR (BUF_MARKERS (m->buffer));
	  eassert (mv == v);
	  ++nused;
	}
    }

  eassert ((nused + nfree) * MARKER_VECTOR_ENTRY_SIZE
	   + MARKER_VECTOR_HEADER_SIZE == v->header.size);
}

/* Make sure that the marker vector of B has room for a new entry.
   Value is the marker vector.  */

static Lisp_Object
ensure_room (struct buffer *b)
{
  Lisp_Object mv = BUF_MARKERS (b);
  if (NILP (mv) || NILP (FREE (XVECTOR (mv))))
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
  Lisp_Object mv = ensure_room (b);
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
  /* Nota this sets the marker of the entry to a non-marker. */
  push_free (v, m->entry);
  m->entry = 0;
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
	    m->entry = 0;
	  }
      BUF_MARKERS (b) = Qnil;
    }
}
