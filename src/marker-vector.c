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

   Entries consist of 3 vector slots MARKER and CHARPOS. MARKER holds a
   marker, if the entry is in use. CHARPOS is not yet used. (The idea is
   to move the positions from Lisp_Marker here, which speeds up
   adjusting positions when the text changes.)

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
#ifdef HAVE_MPS
#include "igc.h"
#endif

enum
{
  MARKER_VECTOR_OFFSET_NEXT_FREE = MARKER_VECTOR_OFFSET_MARKER
};

/* You have to check the code below if you want this to not be equal to
   the MARKER offset. */
static_assert (MARKER_VECTOR_OFFSET_NEXT_FREE == MARKER_VECTOR_OFFSET_MARKER);
static_assert (MARKER_VECTOR_HEADER_SIZE > 0);

/* Minimum number of entries to allocate.  */
#define MARKER_VECTOR_INITIAL_ENTRIES 10

/* Access fields of an entry E of marker vecgor V as lvalues.  */
#define MARKER(v, e) (v)->contents[(e) + MARKER_VECTOR_OFFSET_MARKER]
#define CHARPOS(v, e) (v)->contents[(e) + MARKER_VECTOR_OFFSET_CHARPOS]
#define NEXT_FREE(v, e) (v)->contents[(e) + MARKER_VECTOR_OFFSET_NEXT_FREE]

/* Access header fields of marker vecgor V as lvalues.  */
#define FREE(v) (v)->contents[MARKER_VECTOR_FREE]

/* Return the size of marker vector V. This is like ASIZE but for a
   pointer.  */

static ptrdiff_t
vsize (const struct Lisp_Vector *v)
{
  Lisp_Object vector = make_lisp_ptr ((struct Lisp_Vector*) v, Lisp_Vectorlike);
  return gc_asize (vector);
}

static void
check_is_entry (const struct Lisp_Vector *v, ptrdiff_t entry)
{
  eassert (entry >= MARKER_VECTOR_HEADER_SIZE);
  eassert (entry < vsize (v));
  eassert ((entry - MARKER_VECTOR_HEADER_SIZE) % MARKER_VECTOR_ENTRY_SIZE == 0);
}

/* Add entry ENTRY of V to its free-list. This implicitly sets
   MARKER to not be a marker */

static void
push_free (struct Lisp_Vector *v, const ptrdiff_t entry)
{
  check_is_entry (v, entry);
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
  eassert (free >= 0 && free < vsize (v));
  FREE (v) = NEXT_FREE (v, free);
  check_is_entry (v, free);
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
#else
  return make_vector (len, Qnil);
#endif
}

/* Expensive pre- and post-condition checking.  */

static void
check_marker_vector (Lisp_Object mv, bool allocating)
{
#ifdef ENABLE_CHECKING
  const struct Lisp_Vector *v = XVECTOR (mv);

  size_t nfree = 0;
  for (Lisp_Object e = FREE (v); !NILP (e); e = NEXT_FREE (v, XFIXNUM (e)))
    ++nfree;

  size_t nused = 0;
  DO_MARKERS_VECTOR (mv, m)
    {
      eassert (m->entry == i_);
      eassert (m->buffer != NULL);
      if (!allocating)
	{
	  struct Lisp_Vector *mv = XVECTOR (BUF_MARKERS (m->buffer));
	  eassert (mv == v);
	}
      ++nused;
    }
  END_DO_MARKERS;

  eassert ((nused + nfree) * MARKER_VECTOR_ENTRY_SIZE
	   + MARKER_VECTOR_HEADER_SIZE == vsize (v));
#endif
}

/* Add all entries of MV starting with FIRST to the free list.  */

static void
add_to_free_list (Lisp_Object mv, ptrdiff_t first)
{
  struct Lisp_Vector *v = XVECTOR (mv);
  for (ptrdiff_t e = ASIZE (mv) - MARKER_VECTOR_ENTRY_SIZE;
       e >= first; e -= MARKER_VECTOR_ENTRY_SIZE)
    push_free (v, e);
}

/* Make a new marker vector.  */

Lisp_Object
make_marker_vector (void)
{
  const ptrdiff_t len
    = (MARKER_VECTOR_INITIAL_ENTRIES * MARKER_VECTOR_ENTRY_SIZE
       + MARKER_VECTOR_HEADER_SIZE);
  Lisp_Object mv = alloc_marker_vector (len);
  add_to_free_list (mv, MARKER_VECTOR_HEADER_SIZE);
  check_marker_vector (mv, true);
  return mv;
}

/* Return a new marker vector that is like OLD_MV but larger.  */

static Lisp_Object
larger_marker_vector (Lisp_Object old_mv)
{
  const ptrdiff_t old_size = ASIZE (old_mv);
  const ptrdiff_t old_wo_header = old_size - MARKER_VECTOR_HEADER_SIZE;
  const ptrdiff_t new_size = 2 * old_wo_header + MARKER_VECTOR_HEADER_SIZE;

  Lisp_Object new_mv = alloc_marker_vector (new_size);
  struct Lisp_Vector *new_v = XVECTOR (new_mv);

  /* Copy existing entries. */
  struct Lisp_Vector *old_v = XVECTOR (old_mv);
  eassert (NILP (FREE (old_v)));
  const size_t nbytes = old_size * sizeof (Lisp_Object);
  memcpy (new_v->contents, old_v->contents, nbytes);

  /* Add new entries to free-list.  */
  add_to_free_list (new_mv, old_size);
  check_marker_vector (new_mv, true);
  return new_mv;
}

/* Make sure that the marker vector of B has room for a new entry.
   Value is the marker vector.  */

static Lisp_Object
ensure_room (struct buffer *b)
{
  Lisp_Object mv = BUF_MARKERS (b);
  if (NILP (FREE (XVECTOR (mv))))
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
  check_marker_vector (mv, false);
  m->entry = add_entry (mv, m);
  m->buffer = b;
  check_marker_vector (mv, false);
}

/* Remove marker M from marker vector MV.  */

void
marker_vector_remove (Lisp_Object mv, struct Lisp_Marker *m)
{
  check_marker_vector (mv, false);
  struct Lisp_Vector *v = XVECTOR (mv);
  check_is_entry (v, m->entry);
  eassert (MARKERP (MARKER (v, m->entry)));
  eassert (XMARKER (MARKER (v, m->entry)) == m);
  push_free (v, m->entry);
  m->entry = 0;
  m->buffer = NULL;
  check_marker_vector (mv, false);
}

/* Free all markers from buffer B.  Called from kill-buffer.  */

void
marker_vector_clear (struct buffer *b)
{
  Lisp_Object mv = BUF_MARKERS (b);
  struct Lisp_Vector *v = XVECTOR (mv);
  check_marker_vector (mv, false);
  for (ptrdiff_t e = MARKER_VECTOR_HEADER_SIZE; e < ASIZE (mv);
       e += MARKER_VECTOR_ENTRY_SIZE)
    if (MARKERP (MARKER (v, e)))
      {
	struct Lisp_Marker *m = XMARKER (MARKER (v, e));
	m->buffer = NULL;
	m->entry = 0;
	MARKER (v, e) = Qnil;
      }
  BUF_MARKERS (b) = Qnil;
}
