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

#ifndef EMACS_MARKER_ARRAY_H
#define EMACS_MARKER_ARRAY_H

#include <config.h>
#include "lisp.h"

/* Iterator over marker arrays.  */

struct marker_array_it
{
  struct Lisp_Vector *v;
  Lisp_Object marker;
  ptrdiff_t slot;
};

/* A marker array is a Lisp vector starting with a header of
   MARKER_ARRAY_HEADER_SIZE Lisp_Objects, followed by entries
   of MARKER_ARRAY_ENTRY_SIZE Lisp_Objects.  */

enum
{
  /* Size of header and entries in number of Lisp_Objects.  */
  MARKER_ARRAY_HEADER_SIZE = 2,
  MARKER_ARRAY_ENTRY_SIZE = 3,

  /* Indices of header.  */
  MARKER_ARRAY_FREE_LIST = 0,
  MARKER_ARRAY_HEAD = 1,

  /* Relative indices of entries.  */
  MARKER_ARRAY_OFFSET_NEXT = 0,
  MARKER_ARRAY_OFFSET_PREV = 1,
  /* If entry is free, this is the index of the next free entry in the
     free-list. */
  MARKER_ARRAY_OFFSET_MARKER = 2,
};

INLINE ptrdiff_t
marker_array_slot_to_index (const ptrdiff_t slot)
{
  return ((slot * MARKER_ARRAY_ENTRY_SIZE)
	  + MARKER_ARRAY_HEADER_SIZE);
}

INLINE Lisp_Object
marker_array_head (const struct Lisp_Vector *v)
{
  return v->contents[MARKER_ARRAY_HEAD];
}

INLINE Lisp_Object
marker_array_marker (const struct Lisp_Vector *v, const ptrdiff_t slot)
{
  return v->contents[marker_array_slot_to_index (slot)
		     + MARKER_ARRAY_OFFSET_MARKER];
}

INLINE Lisp_Object
marker_array_next (const struct Lisp_Vector *v, const ptrdiff_t slot)
{
  return v->contents[marker_array_slot_to_index (slot)
		     + MARKER_ARRAY_OFFSET_NEXT];
}

INLINE Lisp_Object
marker_array_prev (const struct Lisp_Vector *v, const ptrdiff_t slot)
{
  return v->contents[marker_array_slot_to_index (slot)
		     + MARKER_ARRAY_OFFSET_PREV];
}

INLINE bool
marker_array_it_is_valid (const struct marker_array_it *it)
{
  return MARKERP (it->marker);
}

INLINE void
marker_array_it_set_to_bext (struct marker_array_it *it)
{
  it->slot = XFIXNUM (marker_array_next (it->v, it->slot));
  if (it->slot >= 0)
    it->marker = marker_array_marker (it->v, it->slot);
  else
    it->marker = Qnil;
}

INLINE struct Lisp_Marker *
marker_array_it_marker (struct marker_array_it *it)
{
  return XMARKER (it->marker);
}

struct marker_array_it marker_array_it_init (struct buffer *b);

# define DO_MARKERS1(b, m)					\
  for (struct marker_array_it it_ = marker_array_it_init (b);	\
       marker_array_it_is_valid (&it_);				\
       marker_array_it_set_to_next (&it_))				\
    {								\
       struct Lisp_Marker *m = marker_array_it_marker (&it_);

# define END_DO_MARKERS }

void marker_array_add_marker (struct buffer *b, struct Lisp_Marker *m);

#endif /* EMACS_MARKER_ARRAY_H */
