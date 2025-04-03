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

/* A text index is used to map character positions in buffer text to byte
   positions and vice versa.

   The index divides (buffer) text into chunks of equal size
   TEXT_INDEX_CHUNK_BYTES number of bytes.  Every TEXT_INDEX_CHUNK_BYTES
   bytes of a buffer's text, the index contains an entry which is the
   character position of the character containing the the given byte
   position.

   Note that a given byte position at a chunk boundary can be in the
   middle of a multi-byte character.

   To find the character position corresponding to a given byte position
   BYTEPOS, we look up the character position in the index at BYTEPOS /
   TEXT_INDEX_CHUNK_BYTES.  From there we scan forward in the text until
   we reach BYTEPOS, counting characters.

   To find the byte position BYTEPOS corresponding to a given character
   position CHARPOS, we search in the index for the last entry SLOT
   whose character position is <= CHARPOS.  That entry corresponds to
   a byte position SLOT * TEXT_INDEX_CHUNK_BYTES.  From that byte position
   we scan the text until we reach BYTEPOS, counting characters until we
   reach CHARPOS.  The byte position reached at the end is BYTEPOS.

   Why divide the text into chunks of bytes instead of chunks of
   characters?  Dividing the text into chunks of characters makes
   scanning overhead less uniform, since characters can be of different
   lengths (1 to 5 bytes.  */

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
     TEXT_INDEX_CHUNK_BYTES.  Note that that byte position may be in the
     middle of a character.  The value at index 0 is BEG.  */
  ptrdiff_t *charpos;

  /* Number of valid entries in the above array.  This is always at least 1
     because the first entry is BEG.  */
  size_t nentries;

  /* Number of entries allocated.  */
  size_t capacity;
};

/* FIXME: For test purposes.  Should become a constant.  */
#define CHUNK_BYTES text_index_chunk_bytes

/* Return the index slot for BYTEPOS.  */

static ptrdiff_t
index_slot (ptrdiff_t bytepos)
{
  return bytepos / CHUNK_BYTES;
}

static ptrdiff_t
index_bytepos (ptrdiff_t slot)
{
  return slot * CHUNK_BYTES;
}

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
  ti->capacity = 1 + index_slot (nbytes);
  ti->charpos = xnmalloc (ti->capacity, sizeof *ti->charpos);
  ti->charpos[0] = BEG;
  ti->nentries = 1;
  return ti;
}

/* Free the text index TI if not NULL.  */

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
  ptrdiff_t needed_slots = index_slot (bytepos) + 1;
  if (needed_slots > ti->capacity)
    {
      ti->capacity = max (needed_slots, 2 * ti->capacity);
      ti->charpos = xnrealloc (ti->charpos, ti->capacity,
			       sizeof *ti->charpos);
    }
}

/* Invalidate index entries for all positions > BYTEPOS in buffer B.
   Note that the entry for BYTEPOS itself, if it is at a chunk boundary,
   remains unchanged.  */

void
text_index_invalidate (struct buffer *b, ptrdiff_t bytepos)
{
  struct text_index *ti = b->text->index;
  if (ti == NULL)
    return;
  ptrdiff_t last_valid = index_slot (bytepos);
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
  return (ti->nentries - 1) * CHUNK_BYTES;
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
  ptrdiff_t slot = index_slot (to_bytepos);
  ptrdiff_t charpos = index_charpos (ti, slot);
  ptrdiff_t bytepos = index_bytepos (slot);
  ptrdiff_t next_stop = index_bytepos (slot + 1);

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
	  ti->charpos[index_slot (bytepos)] = charpos;
	  next_stop += CHUNK_BYTES;
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
  ptrdiff_t bytepos = index_bytepos (slot);
  ptrdiff_t next_stop = bytepos + CHUNK_BYTES;

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
	  ti->charpos[index_slot (bytepos)] = charpos;
	  if (charpos >= to_charpos)
	    break;
	  next_stop += CHUNK_BYTES;
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
  ptrdiff_t bytepos = index_bytepos (slot);
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
  ptrdiff_t bytepos = char_start_bytepos (b, index_bytepos (slot));
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
  const ptrdiff_t slot = index_slot (bytepos);
  return slot + 1 < ti->nentries;
}

ptrdiff_t
text_index_bytepos_to_charpos (struct buffer *b, const ptrdiff_t bytepos)
{
  ensure_bytepos_indexed (b, bytepos);

  const ptrdiff_t slot = index_slot (bytepos);
  const ptrdiff_t indexed_bytepos = index_bytepos (slot);
  if (bytepos - indexed_bytepos < CHUNK_BYTES / 2
      || !can_scan_backward (b, bytepos))
    return charpos_scanning_forward_to_bytepos (b, slot, bytepos);
  return charpos_scanning_backward_to_bytepos (b, slot + 1, bytepos);
}

static ptrdiff_t
bytepos_scanning_forward_to_charpos (struct buffer *b, ptrdiff_t slot,
				     ptrdiff_t to_charpos)
{
  const struct text_index *ti = b->text->index;
  ptrdiff_t bytepos = index_bytepos (slot);
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
  ptrdiff_t bytepos = char_start_bytepos (b, index_bytepos (slot));
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

DEFUN ("text-index--position-bytes", Ftext_index__position_bytes,
       Stext_index__position_bytes, 1, 1, 0,
       doc: /* Return the byte position for character position CHARPOS.
If POSITION is out of range, the value is nil.  */)
  (Lisp_Object charpos)
{
  return Qnil;
}

DEFUN ("text-index--byte-to-position", Ftext_index__byte_to_position,
       Stext_index__byte_to_position, 1, 1, 0,
       doc: /* Return the character position for byte position BYTEPOS.
If BYTEPOS is out of range, the value is nil.  */)
  (Lisp_Object bytepos)
{
  CHECK_FIXNUM (bytepos);
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

  /* FIXME: For test purposes.  Should become a constant.  */
  DEFVAR_INT ("text-index--chunk-bytes", text_index_chunk_bytes, doc: /* */);
  text_index_chunk_bytes = 160;

  Fprovide (intern_c_string ("text-index"), Qnil);
}
