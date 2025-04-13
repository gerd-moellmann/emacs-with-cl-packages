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
  Lisp_Object mv;
  ptrdiff_t i;
};

/* A marker array is a Lisp vector starting with a header of
   MARKER_VECTOR_HEADER_SIZE Lisp_Objects, followed by entries
   of MARKER_VECTOR_ENTRY_SIZE Lisp_Objects.  */

enum
{
  /* Size of header and entries in number of Lisp_Objects.  */
  MARKER_VECTOR_HEADER_SIZE = 1,
  MARKER_VECTOR_ENTRY_SIZE = 3,

  /* Indices of header.  */
  MARKER_VECTOR_FREE = 0,

  /* Relative indices of entries.  */
  MARKER_VECTOR_OFFSET_MARKER = 0,
  MARKER_VECTOR_OFFSET_BYTEPOS = 1,
  MARKER_VECTOR_OFFSET_CHARPOS = 2,
};

INLINE bool
marker_vector_it_is_valid (const struct marker_vector_it *it)
{
  return it->i > 0;
}

INLINE struct Lisp_Marker *
marker_vector_it_marker (const struct marker_vector_it *it)
{
  return XMARKER (AREF (it->mv, it->i));
}

INLINE void
marker_vector_it_set_to_next (struct marker_vector_it *it)
{
  for (it->i += MARKER_VECTOR_ENTRY_SIZE;
       it->i < ASIZE (it->mv);
       it->i += MARKER_VECTOR_ENTRY_SIZE)
    if (MARKERP (AREF (it->mv, it->i)))
      return;
  it->i = 0;
}

# define DO_MARKERS(b, m)					\
  for (struct marker_vector_it it_ = marker_vector_it_init (b);	\
       marker_vector_it_is_valid (&it_);			\
       marker_vector_it_set_to_next (&it_))			\
    {								\
       struct Lisp_Marker *m = marker_vector_it_marker (&it_);

# define END_DO_MARKERS }

struct marker_vector_it marker_vector_it_init (struct buffer *b);
void marker_vector_add (struct buffer *b, struct Lisp_Marker *m);
void marker_vector_remove (struct Lisp_Vector *v, struct Lisp_Marker *m);
void marker_vector_clear (struct buffer *b);

#endif /* EMACS_MARKER_VECTOR_H */
