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
