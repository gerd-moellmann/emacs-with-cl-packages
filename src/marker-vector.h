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

#ifndef EMACS_MARKER_VECTOR_H
#define EMACS_MARKER_VECTOR_H

#include <config.h>
#include "lisp.h"

/* Iterator over marker arrays.  */

struct marker_vector_it
{
  struct Lisp_Vector *v;
  Lisp_Object marker;
  ptrdiff_t entry;
};

/* A marker array is a Lisp vector starting with a header of
   MARKER_VECTOR_HEADER_SIZE Lisp_Objects, followed by entries
   of MARKER_VECTOR_ENTRY_SIZE Lisp_Objects.  */

enum
{
  /* Size of header and entries in number of Lisp_Objects.  */
  MARKER_VECTOR_HEADER_SIZE = 2,
  MARKER_VECTOR_ENTRY_SIZE = 3,

  /* Indices of header.  */
  MARKER_VECTOR_FREE_LIST = 0,
  MARKER_VECTOR_HEAD = 1,

  /* Relative indices of entries.  */
  MARKER_VECTOR_OFFSET_NEXT = 0,
  MARKER_VECTOR_OFFSET_PREV = 1,
  MARKER_VECTOR_OFFSET_MARKER = 2,
};

INLINE ptrdiff_t
marker_vector_entry_to_index (const ptrdiff_t entry)
{
  return ((entry * MARKER_VECTOR_ENTRY_SIZE)
	  + MARKER_VECTOR_HEADER_SIZE);
}

INLINE Lisp_Object
marker_vector_head (const struct Lisp_Vector *v)
{
  return v->contents[MARKER_VECTOR_HEAD];
}

INLINE Lisp_Object
marker_vector_marker (const struct Lisp_Vector *v, const ptrdiff_t entry)
{
  return v->contents[marker_vector_entry_to_index (entry)
		     + MARKER_VECTOR_OFFSET_MARKER];
}

INLINE Lisp_Object
marker_vector_next (const struct Lisp_Vector *v, const ptrdiff_t entry)
{
  return v->contents[marker_vector_entry_to_index (entry)
		     + MARKER_VECTOR_OFFSET_NEXT];
}

INLINE Lisp_Object
marker_vector_prev (const struct Lisp_Vector *v, const ptrdiff_t entry)
{
  return v->contents[marker_vector_entry_to_index (entry)
		     + MARKER_VECTOR_OFFSET_PREV];
}

INLINE bool
marker_vector_it_is_valid (const struct marker_vector_it *it)
{
  return MARKERP (it->marker);
}

INLINE void
marker_vector_it_set_to_bext (struct marker_vector_it *it)
{
  it->entry = XFIXNUM (marker_vector_next (it->v, it->entry));
  if (it->entry >= 0)
    it->marker = marker_vector_marker (it->v, it->entry);
  else
    it->marker = Qnil;
}

INLINE struct Lisp_Marker *
marker_vector_it_marker (struct marker_vector_it *it)
{
  return XMARKER (it->marker);
}

# define DO_MARKER_VECTOR(b, m)					\
  for (struct marker_vector_it it_ = marker_vector_it_init (b);	\
       marker_vector_it_is_valid (&it_);			\
       marker_vector_it_set_to_next (&it_))			\
    {								\
       struct Lisp_Marker *m = marker_vector_it_marker (&it_);

# define END_DO_MARKER_VECTORS }

struct marker_vector_it marker_vector_it_init (struct buffer *b);
void marker_vector_add_marker (struct buffer *b, struct Lisp_Marker *m);
void marker_vector_remove_marker (struct buffer *b, struct Lisp_Marker *m);
void marker_vector_remove_all_markers (struct buffer *b);

#endif /* EMACS_MARKER_VECTOR_H */
