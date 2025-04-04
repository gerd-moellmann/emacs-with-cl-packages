/* Text index for character positions.
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

/* A text index is used to map character positions in buffer text to
   byte positions and vice versa.  (One could also think of using it for
   other things like line numbers, but that is currently not done.)

   The index divides buffer text into intervals of constant size =
   number of bytes.  The index consists of an array of character
   positions where the entry at index SLOT is the character position of
   the character containing the byte position SLOT * interval-size.  Note
   that the byte position at interval boundaries can be in the middle of a
   multi-byte character.

   To find the character position corresponding to a byte position
   BYTEPOS, we look up the character position in the index at BYTEPOS /
   interval size.  From there, we can scan forward in the text until we
   reach BYTEPOS, counting characters.  We also scan backward from the
   interval end, if that is closer.

   To find the byte position BYTEPOS corresponding to a given character
   position CHARPOS, we search the index for the last entry SLOT whose
   character position is <= CHARPOS.  That entry corresponds to a byte
   position SLOT * interval size.  From there, we scan the text until we
   reach BYTEPOS, counting characters until we reach CHARPOS.  The byte
   position reached at the end is BYTEPOS.  We also scan backward from
   the interval end, if that looks advantageous.

   Why divide the text into intervals of bytes instead of[C
   characters?  Dividing the text into intervals of characters makes
   scanning overhead less uniform, since characters can be of different
   lengths (1 to 5 bytes).  */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include "lisp.h"
#include "buffer.h"
#include "text-index.h"

// clang-format off

struct text_index
{
  /* Value at index IDX is the character position of byte position IDX *
     INTERVAL.  Note that that byte position may be in the middle of a
     character.  The value at index 0 is BEG.  */
  ptrdiff_t *charpos;

  /* Number of valid entries in the above array.  This is always at least 1
     because the first entry is BEG.  */
  size_t nentries;

  /* Number of entries allocated.  */
  size_t capacity;

  /* Number of bytes in an interval. This is here instead of using a
     constant mainly because it makes things more flexible for
     experimentation.  The overhead is probably not worth worrying about
     in the grander scheme of things anyway.  */
  size_t interval;
};

/* Return the index slot for BYTEPOS in index TI.  */

static ptrdiff_t
index_slot (const struct text_index *ti, ptrdiff_t bytepos)
{
  return bytepos / ti->interval;
}

/* Return the byte position in index TI corresponding index entry SLOT.
   Note that this position cab be in the middle of a multi-byte
   character.  */

static ptrdiff_t
index_bytepos (const struct text_index *ti, ptrdiff_t slot)
{
  return slot * ti->interval;
}

/* Return the character position in index TI corresponding index entry
   SLOT.  */

static ptrdiff_t
index_charpos (const struct text_index *ti, ptrdiff_t slot)
{
  return ti->charpos[slot];
}

/* Return the slot of index TI for the largest character position <=
   CHARPOS.  */

static ptrdiff_t
index_charpos_slot (const struct text_index *ti, ptrdiff_t charpos)
{
  ptrdiff_t slot = -1;
  for (ptrdiff_t low = 0, high = ti->nentries - 1; low <= high;)
    {
      ptrdiff_t mid = low + (high - low) / 2;
      if (ti->charpos[mid] <= charpos)
	{
	  slot = mid;
	  low = mid + 1;
	}
      else
	high = mid - 1;
    }

  eassert (slot >= 0 && slot < ti->nentries);
  return slot;
}

/* Given a byte position BYTEPOS in buffer B, return the byte position
   where the character starts that contains BYTEPOS: */

static ptrdiff_t
char_start_bytepos (struct buffer *b, ptrdiff_t bytepos)
{
  while (!CHAR_HEAD_P (BUF_FETCH_BYTE (b, bytepos)))
    --bytepos;
  return bytepos;
}

/* Allocate and return a text index structure with enough room for a
   text of length NBYTES bytes.  */

static struct text_index *
make_text_index (size_t nbytes)
{
  struct text_index *ti = xzalloc (sizeof *ti);
  ti->interval = text_index_interval;
  ti->capacity = 1 + index_slot (ti, nbytes);
  ti->charpos = xnmalloc (ti->capacity, sizeof *ti->charpos);
  ti->charpos[0] = BEG;
  ti->nentries = 1;
  return ti;
}

/* Free the text index TI.  TI may be NULL.  */

void
text_index_free (struct text_index *ti)
{
  if (ti == NULL)
    return;
  xfree (ti->charpos);
  xfree (ti);
}

/* Make text index TI large enough to contain BYTEPOS.  */

static void
enlarge_index (struct text_index *ti, ptrdiff_t bytepos)
{
  ptrdiff_t needed_slots = index_slot (ti, bytepos) + 1;
  if (needed_slots > ti->capacity)
    {
      ti->capacity = max (needed_slots, 2 * ti->capacity);
      ti->charpos = xnrealloc (ti->charpos, ti->capacity,
			       sizeof *ti->charpos);
    }
}

/* Invalidate index entries for all positions > BYTEPOS in buffer B.
   Note that the entry for BYTEPOS itself, if it is at an interval
   boundary, remains unchanged.  */

void
text_index_invalidate (struct buffer *b, ptrdiff_t bytepos)
{
  struct text_index *ti = b->text->index;
  if (ti == NULL)
    return;
  ptrdiff_t last_valid = index_slot (ti, bytepos);
  ti->nentries = min (ti->nentries, last_valid + 1);
}

/* Return index TI's maximum indexed character position.  */

static ptrdiff_t
max_indexed_charpos (const struct text_index *ti)
{
  if (ti)
    return ti->charpos[ti->nentries - 1];
  return 0;
}

/* Return index TI's maximum indexed byte position.  */

static ptrdiff_t
max_indexed_bytepos (const struct text_index *ti)
{
  if (ti == NULL)
    return 0;
  if (ti->nentries == 1)
    return BEG_BYTE;
  return (ti->nentries - 1) * ti->interval;
}

/* Build text index of buffer B up to and including TO_BYTEPOS.  */

static void
build_index_to_bytepos (struct buffer *b, ptrdiff_t to_bytepos)
{
  struct text_index *ti = b->text->index;
  enlarge_index (ti, to_bytepos);

  eassert (to_bytepos >= BEG_BYTE && to_bytepos <= BUF_Z_BYTE (b));
  eassert (to_bytepos > max_indexed_bytepos (ti));

  /* Start at the byte position of the index entry <= TO_BYTEPOS.  if
     TO_BYTEPOS equals the byte position of that entry, this is okay,
     because the character position at that byte position cannot have
     changed.  */
  ptrdiff_t slot = index_slot (ti,  to_bytepos);
  ptrdiff_t charpos = index_charpos (ti, slot);
  ptrdiff_t bytepos = index_bytepos (ti, slot);
  ptrdiff_t next_stop = index_bytepos (ti, slot + 1);

  /* Loop over bytes, starting one after the index entry we start from
     because we are only interested in yet unknown entries, and the
     one at SLOT is assumed to stay unchanged.  */
  for (++bytepos; bytepos <= to_bytepos; ++bytepos)
    {
      unsigned char byte = BUF_FETCH_BYTE (b, bytepos);
      if (CHAR_HEAD_P (byte))
	++charpos;

      if (bytepos == next_stop)
	{
	  ti->charpos[index_slot (ti, bytepos)] = charpos;
	  next_stop += ti->interval;
	}
    }
}

/* Build text index of buffer B up to and including TO_CHARPOS.  */

static void
build_index_to_charpos (struct buffer *b, const ptrdiff_t to_charpos)
{
  struct text_index *ti = b->text->index;
  eassert (to_charpos >= BEG && to_charpos <= BUF_Z (b));
  eassert (to_charpos > max_indexed_charpos (ti));

  /* Start at the last index entry.  */
  const ptrdiff_t slot = ti->nentries - 1;
  ptrdiff_t charpos = index_charpos (ti, slot);
  ptrdiff_t bytepos = index_bytepos (ti, slot);
  ptrdiff_t next_stop = bytepos + ti->interval;

  /* Not enough bytes left to make a new index entry?  */
  const ptrdiff_t z_byte = BUF_Z_BYTE (b);
  if (next_stop > z_byte)
    return;

  /* Loop over bytes, starting one after the index entry, until we
     reach the interesting character position.  */
  for (++bytepos; bytepos < z_byte; ++bytepos)
    {
      unsigned char byte = BUF_FETCH_BYTE (b, bytepos);
      if (CHAR_HEAD_P (byte))
	++charpos;

      if (bytepos == next_stop)
	{
	  ti->charpos[index_slot (ti, bytepos)] = charpos;
	  if (charpos >= to_charpos)
	    break;
	  next_stop += ti->interval;
	}
    }
}

/* Make sure tht buffer B has a text index.  */

static void
ensure_has_index (struct buffer *b)
{
  if (b->text->index == NULL)
    b->text->index = make_text_index (BUF_Z_BYTE (b));
}

/* Make sure that buffer B's text index contains BYTEPOS.  */

static void
ensure_bytepos_indexed (struct buffer *b, ptrdiff_t bytepos)
{
  ensure_has_index (b);
  if (bytepos > max_indexed_bytepos (b->text->index))
    build_index_to_bytepos (b, bytepos);
}

/* Make sure that buffer B's text index contains CHARPOS.  */

static void
ensure_charpos_indexed (struct buffer *b, ptrdiff_t charpos)
{
  ensure_has_index (b);
  if (charpos > max_indexed_charpos (b->text->index))
    build_index_to_charpos (b, charpos);
}

/* In buffer B, starting from index entry SLOT, scan forward in B's
   text to TO_BYTEPOS, and return the corresponding character
   position.  */

static ptrdiff_t
charpos_scanning_forward_to_bytepos (struct buffer *b, ptrdiff_t slot,
				     const ptrdiff_t to_bytepos)
{
  const struct text_index *ti = b->text->index;
  ptrdiff_t bytepos = index_bytepos (ti, slot);
  ptrdiff_t charpos = index_charpos (ti, slot);
  for (++bytepos; bytepos <= to_bytepos; ++bytepos)
    {
      const unsigned char byte = BUF_FETCH_BYTE (b, bytepos);
      if (CHAR_HEAD_P (byte))
	++charpos;
    }
  return charpos;
}

/* In buffer B, starting from index entry SLOT, scan backward in B's
   text to TO_BYTEPOS, and return the corresponding character
   position.  */

static ptrdiff_t
charpos_scanning_backward_to_bytepos (struct buffer *b, ptrdiff_t slot,
				      const ptrdiff_t to_bytepos)
{
  const struct text_index *ti = b->text->index;
  ptrdiff_t bytepos = char_start_bytepos (b, index_bytepos (ti, slot));
  ptrdiff_t charpos = index_charpos (ti, slot);
  for (--bytepos; bytepos >= to_bytepos; --bytepos)
    {
      const unsigned char byte = BUF_FETCH_BYTE (b, bytepos);
      if (CHAR_HEAD_P (byte))
	--charpos;
    }
  return charpos;
}

/* Return true if we can backward scanning to find the character
   position of BYTEPOS in buffer B, */

static bool
can_scan_backward (const struct buffer *b, const ptrdiff_t bytepos)
{
  const struct text_index *ti = b->text->index;
  const ptrdiff_t slot = index_slot (ti, bytepos);
  return slot + 1 < ti->nentries;
}

ptrdiff_t
text_index_bytepos_to_charpos (struct buffer *b, const ptrdiff_t bytepos)
{
  ensure_bytepos_indexed (b, bytepos);
  struct text_index *ti = b->text->index;
  const ptrdiff_t slot = index_slot (ti, bytepos);
  const ptrdiff_t indexed_bytepos = index_bytepos (ti, slot);
  if (bytepos - indexed_bytepos < ti->interval / 2
      || !can_scan_backward (b, bytepos))
    return charpos_scanning_forward_to_bytepos (b, slot, bytepos);
  return charpos_scanning_backward_to_bytepos (b, slot + 1, bytepos);
}

static ptrdiff_t
bytepos_scanning_forward_to_charpos (struct buffer *b, ptrdiff_t slot,
				     ptrdiff_t to_charpos)
{
  const struct text_index *ti = b->text->index;
  ptrdiff_t bytepos = index_bytepos (ti, slot);
  ptrdiff_t charpos = index_charpos (ti, slot);
  for (++bytepos; charpos < to_charpos; ++bytepos)
    {
      const unsigned char byte = BUF_FETCH_BYTE (b, bytepos);
      if (CHAR_HEAD_P (byte))
	++charpos;
    }
  return bytepos;
}

static ptrdiff_t
bytepos_scanning_backward_to_charpos (struct buffer *b,
				      const ptrdiff_t slot,
				      const ptrdiff_t to_charpos)
{
  const struct text_index *ti = b->text->index;
  ptrdiff_t bytepos = char_start_bytepos (b, index_bytepos (ti, slot));
  ptrdiff_t charpos = index_charpos (ti, slot);
  for (--bytepos; charpos > to_charpos; --bytepos)
    {
      const unsigned char byte = BUF_FETCH_BYTE (b, bytepos);
      if (CHAR_HEAD_P (byte))
	--charpos;
    }
  return bytepos;
}

ptrdiff_t
text_index_charpos_to_bytepos (struct buffer *b, ptrdiff_t charpos)
{
  ensure_charpos_indexed (b, charpos);
  struct text_index *ti = b->text->index;
  ptrdiff_t slot = index_charpos_slot (ti, charpos);
  /* One could also try to employ (Z, Z_BYTE) as a known position to
     scan backwards from, but I'm not sure it's worth it.  */
  if (slot + 1 < ti->nentries
      && (index_charpos (ti, slot + 1) - charpos
	  < charpos - index_charpos (ti, slot)))
    return bytepos_scanning_backward_to_charpos (b, slot + 1, charpos);
  return bytepos_scanning_forward_to_charpos (b, slot, charpos);
}

static void
check_text_index_used (void)
{
  if (!use_text_index)
    error ("text index is not in use");
}

DEFUN ("text-index--position-bytes", Ftext_index__position_bytes,
       Stext_index__position_bytes, 1, 1, 0,
       doc: /* Return the byte position for character position CHARPOS.
If POSITION is out of range, the value is nil.  */)
  (Lisp_Object charpos)
{
  check_text_index_used ();
  EMACS_INT pos = fix_position (charpos);
  if (pos < BEG || pos > Z)
    return Qnil;
  ptrdiff_t bytepos = text_index_charpos_to_bytepos (current_buffer, pos);
  return make_fixnum (bytepos);
}

DEFUN ("text-index--byte-to-position", Ftext_index__byte_to_position,
       Stext_index__byte_to_position, 1, 1, 0,
       doc: /* Return the character position for byte position BYTEPOS.
If BYTEPOS is out of range, the value is nil.  */)
  (Lisp_Object bytepos)
{
  check_text_index_used ();
  CHECK_FIXNUM (bytepos);
  ptrdiff_t pos_byte = XFIXNUM (bytepos);
  if (pos_byte < BEG_BYTE || pos_byte > Z_BYTE)
    return Qnil;
  ptrdiff_t charpos = text_index_bytepos_to_charpos (current_buffer, pos_byte);
  return make_fixnum (charpos);
}

DEFUN ("text-index--set-interval-bytes", Ftext_index__set_interval,
       Stext_index__set_interval, 1, 1, 0,
       doc: /* Set interval size to NBYTES, a positive, non-zero integer.  */)
  (Lisp_Object nbytes)
{
  text_index_interval = check_integer_range (nbytes, 1, 1000000);
  Lisp_Object buffers = Fbuffer_list (Qnil);
  FOR_EACH_TAIL (buffers)
    {
      struct buffer *b = XBUFFER (XCAR (buffers));
      text_index_free (b->own_text.index);
      b->own_text.index = NULL;
    }
  return Qnil;
}

void
init_text_index (void)
{
}

void
syms_of_text_index (void)
{
  defsubr (&Stext_index__position_bytes);
  defsubr (&Stext_index__byte_to_position);
  defsubr (&Stext_index__set_interval);

  /* FIXME: For experimenting only.  */
  DEFVAR_INT ("text-index-interval", text_index_interval, doc: /* */);
  text_index_interval = 160;
  DEFVAR_BOOL ("use-text-index", use_text_index, doc: /* */);
  use_text_index = true;

  Fprovide (intern_c_string ("text-index"), Qnil);
}
