/* Basic character set support.

Copyright (C) 2001-2025 Free Software Foundation, Inc.

Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
  2005, 2006, 2007, 2008, 2009, 2010, 2011
  National Institute of Advanced Industrial Science and Technology (AIST)
  Registration Number H14PRO021

Copyright (C) 2003, 2004
  National Institute of Advanced Industrial Science and Technology (AIST)
  Registration Number H13PRO009

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <sys/types.h>
#include "lisp.h"
#include "character.h"
#include "charset.h"
#include "coding.h"
#include "buffer.h"
#include "sysstdio.h"
#include "pdumper.h"
#include "igc.h"

/*** GENERAL NOTES on CODED CHARACTER SETS (CHARSETS) ***

  A coded character set ("charset" hereafter) is a meaningful
  collection (i.e. language, culture, functionality, etc.) of
  characters.  Emacs handles multiple charsets at once.  In Emacs Lisp
  code, a charset is represented by a symbol.  In C code, a charset is
  represented by its ID number or by a pointer to a struct charset.

  The actual information about each charset is stored in two places.
  Lispy information is stored in the hash table Vcharset_hash_table as
  a vector (charset attributes).  The other information is stored in
  charset_table as a struct charset.

*/

/* Hash table that contains attributes of each charset.  Keys are
   charset symbols, and values are vectors of charset attributes.  */
Lisp_Object Vcharset_hash_table;

/* Table of struct charset.  */
struct charset *charset_table;
int charset_table_size;
int charset_table_used;

/* Special charsets corresponding to symbols.  */
int charset_ascii;
int charset_eight_bit;
static int charset_iso_8859_1;
int charset_unicode;
static int charset_emacs;

/* The other special charsets.  */
int charset_jisx0201_roman;
int charset_jisx0208_1978;
int charset_jisx0208;
int charset_ksc5601;

/* Charset of unibyte characters.  */
int charset_unibyte;

/* List of charsets ordered by the priority.  */
Lisp_Object Vcharset_ordered_list;

/* Sub-list of Vcharset_ordered_list that contains all non-preferred
   charsets.  */
Lisp_Object Vcharset_non_preferred_head;

/* Incremented every time we change the priority of charsets.
   Wraps around.  */
EMACS_UINT charset_ordered_list_tick;

/* List of iso-2022 charsets.  */
Lisp_Object Viso_2022_charset_list;

/* List of emacs-mule charsets.  */
Lisp_Object Vemacs_mule_charset_list;

int emacs_mule_charset[256];

/* Mapping table from ISO2022's charset (specified by DIMENSION,
   CHARS, and FINAL-CHAR) to Emacs's charset.  */
int iso_charset_table[ISO_MAX_DIMENSION][ISO_MAX_CHARS][ISO_MAX_FINAL];

#define CODE_POINT_TO_INDEX(charset, code)				\
  ((charset)->code_linear_p						\
   ? (int) ((code) - (charset)->min_code)				\
   : (((charset)->code_space_mask[(code) >> 24] & 0x8)			\
      && ((charset)->code_space_mask[((code) >> 16) & 0xFF] & 0x4)	\
      && ((charset)->code_space_mask[((code) >> 8) & 0xFF] & 0x2)	\
      && ((charset)->code_space_mask[(code) & 0xFF] & 0x1))		\
   ? (int) (((((code) >> 24) - (charset)->code_space[12])		\
	     * (charset)->code_space[11])				\
	    + (((((code) >> 16) & 0xFF) - (charset)->code_space[8])	\
	       * (charset)->code_space[7])				\
	    + (((((code) >> 8) & 0xFF) - (charset)->code_space[4])	\
	       * (charset)->code_space[3])				\
	    + (((code) & 0xFF) - (charset)->code_space[0])		\
	    - ((charset)->char_index_offset))				\
   : -1)


/* Return the code-point for the character index IDX in CHARSET.
   IDX should be an unsigned int variable in a valid range (which is
   always in nonnegative int range too).  IDX contains garbage afterwards.  */

#define INDEX_TO_CODE_POINT(charset, idx)				     \
  ((charset)->code_linear_p						     \
   ? (idx) + (charset)->min_code					     \
   : (idx += (charset)->char_index_offset,				     \
      (((charset)->code_space[0] + (idx) % (charset)->code_space[2])	     \
       | (((charset)->code_space[4]					     \
	   + ((idx) / (charset)->code_space[3] % (charset)->code_space[6]))  \
	  << 8)								     \
       | (((charset)->code_space[8]					     \
	   + ((idx) / (charset)->code_space[7] % (charset)->code_space[10])) \
	  << 16)							     \
       | (((charset)->code_space[12] + ((idx) / (charset)->code_space[11]))  \
	  << 24))))

/* Structure to hold mapping tables for a charset.  Used by temacs
   invoked for dumping.  */

static struct
{
  /* The current charset for which the following tables are setup.  */
  struct charset *current;

  /* 1 iff the following table is used for encoder.  */
  short for_encoder;

  /* When the following table is used for encoding, minimum and
     maximum character of the current charset.  */
  int min_char, max_char;

  /* A Unicode character corresponding to the code index 0 (i.e. the
     minimum code-point) of the current charset, or -1 if the code
     index 0 is not a Unicode character.  This is checked when
     table.encoder[CHAR] is zero.  */
  int zero_index_char;

  union {
    /* Table mapping code-indices (not code-points) of the current
       charset to Unicode characters.  If decoder[CHAR] is -1, CHAR
       doesn't belong to the current charset.  */
    int decoder[0x10000];
    /* Table mapping Unicode characters to code-indices of the current
       charset.  The first 0x10000 elements are for BMP (0..0xFFFF),
       and the last 0x10000 are for SMP (0x10000..0x1FFFF) or SIP
       (0x20000..0x2FFFF).  Note that there is no charset map that
       uses both SMP and SIP.  */
    unsigned short encoder[0x20000];
  } table;
} *temp_charset_work;

#define SET_TEMP_CHARSET_WORK_ENCODER(C, CODE)			\
  do {								\
    if ((CODE) == 0)						\
      temp_charset_work->zero_index_char = (C);			\
    else if ((C) < 0x20000)					\
      temp_charset_work->table.encoder[(C)] = (CODE);		\
    else							\
      temp_charset_work->table.encoder[(C) - 0x10000] = (CODE);	\
  } while (0)

#define GET_TEMP_CHARSET_WORK_ENCODER(C)				  \
  ((C) == temp_charset_work->zero_index_char ? 0			  \
   : (C) < 0x20000 ? (temp_charset_work->table.encoder[(C)]		  \
		      ? (int) temp_charset_work->table.encoder[(C)] : -1) \
   : temp_charset_work->table.encoder[(C) - 0x10000]			  \
   ? temp_charset_work->table.encoder[(C) - 0x10000] : -1)

#define SET_TEMP_CHARSET_WORK_DECODER(C, CODE)	\
  (temp_charset_work->table.decoder[(CODE)] = (C))

#define GET_TEMP_CHARSET_WORK_DECODER(CODE)	\
  (temp_charset_work->table.decoder[(CODE)])


/* Set to 1 to warn that a charset map is loaded and thus a buffer
   text and a string data may be relocated.  */
bool charset_map_loaded;

struct charset_map_entries
{
  struct {
    unsigned from, to;
    int c;
  } entry[0x10000];
  struct charset_map_entries *next;
};

/* Load the mapping information of CHARSET from ENTRIES for
   initializing (CONTROL_FLAG == 0), decoding (CONTROL_FLAG == 1), and
   encoding (CONTROL_FLAG == 2).

   If CONTROL_FLAG is 0, setup CHARSET->min_char, CHARSET->max_char,
   and CHARSET->fast_map.

   If CONTROL_FLAG is 1, setup the following tables according to
   CHARSET->method and inhibit_load_charset_map.

   CHARSET->method       | inhibit_lcm == 0   | inhibit_lcm == 1
   ----------------------+--------------------+---------------------------
   CHARSET_METHOD_MAP    | CHARSET->decoder   | temp_charset_work->decoder
   ----------------------+--------------------+---------------------------
   CHARSET_METHOD_OFFSET | Vchar_unify_table  | temp_charset_work->decoder

   If CONTROL_FLAG is 2, setup the following tables.

   CHARSET->method       | inhibit_lcm == 0   | inhibit_lcm == 1
   ----------------------+--------------------+---------------------------
   CHARSET_METHOD_MAP    | CHARSET->encoder   | temp_charset_work->encoder
   ----------------------+--------------------+--------------------------
   CHARSET_METHOD_OFFSET | CHARSET->deunifier | temp_charset_work->encoder
*/

static void
load_charset_map (struct charset *charset, struct charset_map_entries *entries, int n_entries, int control_flag)
{
  Lisp_Object vec UNINIT;
  Lisp_Object table UNINIT;
  unsigned max_code = CHARSET_MAX_CODE (charset);
  bool ascii_compatible_p = charset->ascii_compatible_p;
  int min_char, max_char, nonascii_min_char;
  int i;
  unsigned char *fast_map = charset->fast_map;

  if (n_entries <= 0)
    return;

  if (control_flag)
    {
      if (! inhibit_load_charset_map)
	{
	  if (control_flag == 1)
	    {
	      if (charset->method == CHARSET_METHOD_MAP)
		{
		  int n = CODE_POINT_TO_INDEX (charset, max_code) + 1;

		  vec = make_vector (n, make_fixnum (-1));
		  set_charset_attr (charset, charset_decoder, vec);
		}
	      else
		{
		  char_table_set_range (Vchar_unify_table,
					charset->min_char, charset->max_char,
					Qnil);
		}
	    }
	  else
	    {
	      table = Fmake_char_table (Qnil, Qnil);
	      set_charset_attr (charset,
				(charset->method == CHARSET_METHOD_MAP
				 ? charset_encoder : charset_deunifier),
				table);
	    }
	}
      else
	{
	  if (! temp_charset_work)
	    temp_charset_work = xmalloc (sizeof *temp_charset_work);
	  if (control_flag == 1)
	    {
	      memset (temp_charset_work->table.decoder, -1,
		      sizeof (int) * 0x10000);
	    }
	  else
	    {
	      memset (temp_charset_work->table.encoder, 0,
		      sizeof (unsigned short) * 0x20000);
	      temp_charset_work->zero_index_char = -1;
	    }
	  temp_charset_work->current = charset;
	  temp_charset_work->for_encoder = (control_flag == 2);
	  control_flag += 2;
	}
      charset_map_loaded = 1;
    }

  min_char = max_char = entries->entry[0].c;
  nonascii_min_char = MAX_CHAR;
  for (i = 0; i < n_entries; i++)
    {
      unsigned from, to;
      int from_index, to_index, lim_index;
      int from_c, to_c;
      int idx = i % 0x10000;

      if (i > 0 && idx == 0)
	entries = entries->next;
      from = entries->entry[idx].from;
      to = entries->entry[idx].to;
      from_c = entries->entry[idx].c;
      from_index = CODE_POINT_TO_INDEX (charset, from);
      if (from == to)
	{
	  to_index = from_index;
	  to_c = from_c;
	}
      else
	{
	  to_index = CODE_POINT_TO_INDEX (charset, to);
	  to_c = from_c + (to_index - from_index);
	}
      if (from_index < 0 || to_index < 0)
	continue;
      lim_index = to_index + 1;

      if (to_c > max_char)
	max_char = to_c;
      else if (from_c < min_char)
	min_char = from_c;

      if (control_flag == 1)
	{
	  if (charset->method == CHARSET_METHOD_MAP)
	    for (; from_index < lim_index; from_index++, from_c++)
	      ASET (vec, from_index, make_fixnum (from_c));
	  else
	    for (; from_index < lim_index; from_index++, from_c++)
	      CHAR_TABLE_SET (Vchar_unify_table,
			      CHARSET_CODE_OFFSET (charset) + from_index,
			      make_fixnum (from_c));
	}
      else if (control_flag == 2)
	{
	  if (charset->method == CHARSET_METHOD_MAP
	      && CHARSET_COMPACT_CODES_P (charset))
	    for (; from_index < lim_index; from_index++, from_c++)
	      {
		unsigned code = from_index;
		code = INDEX_TO_CODE_POINT (charset, code);

		if (NILP (CHAR_TABLE_REF (table, from_c)))
		  CHAR_TABLE_SET (table, from_c, make_fixnum (code));
	      }
	  else
	    for (; from_index < lim_index; from_index++, from_c++)
	      {
		if (NILP (CHAR_TABLE_REF (table, from_c)))
		  CHAR_TABLE_SET (table, from_c, make_fixnum (from_index));
	      }
	}
      else if (control_flag == 3)
	for (; from_index < lim_index; from_index++, from_c++)
	  SET_TEMP_CHARSET_WORK_DECODER (from_c, from_index);
      else if (control_flag == 4)
	for (; from_index < lim_index; from_index++, from_c++)
	  SET_TEMP_CHARSET_WORK_ENCODER (from_c, from_index);
      else			/* control_flag == 0 */
	{
	  if (ascii_compatible_p)
	    {
	      if (! ASCII_CHAR_P (from_c))
		{
		  if (from_c < nonascii_min_char)
		    nonascii_min_char = from_c;
		}
	      else if (! ASCII_CHAR_P (to_c))
		{
		  nonascii_min_char = 0x80;
		}
	    }

	  for (; from_c <= to_c; from_c++)
	    CHARSET_FAST_MAP_SET (from_c, fast_map);
	}
    }

  if (control_flag == 0)
    {
      CHARSET_MIN_CHAR (charset) = (ascii_compatible_p
				    ? nonascii_min_char : min_char);
      CHARSET_MAX_CHAR (charset) = max_char;
    }
  else if (control_flag == 4)
    {
      temp_charset_work->min_char = min_char;
      temp_charset_work->max_char = max_char;
    }
}


/* Read a hexadecimal number (preceded by "0x") from the file FP while
   paying attention to comment character '#'.  LOOKAHEAD is the
   lookahead byte if it is nonnegative.  Store into *TERMINATOR the
   input byte after the number, or EOF if an end-of-file or input
   error occurred.  Set *OVERFLOW if the number overflows.  */

static unsigned
read_hex (FILE *fp, int lookahead, int *terminator, bool *overflow)
{
  int c = lookahead < 0 ? getc (fp) : lookahead;

  while (true)
    {
      if (c == '#')
	do
	  c = getc (fp);
	while (0 <= c && c != '\n');
      else if (c == '0')
	{
	  c = getc (fp);
	  if (c < 0 || c == 'x')
	    break;
	}
      if (c < 0)
	break;
      c = getc (fp);
    }

  unsigned n = 0;
  bool v = false;

  if (0 <= c)
    while (true)
      {
	c = getc (fp);
	int digit = char_hexdigit (c);
	if (digit < 0)
	  break;
	v |= INT_LEFT_SHIFT_OVERFLOW (n, 4);
	n = (n << 4) + digit;
      }

  *terminator = c;
  *overflow |= v;
  return n;
}

/* Return a mapping vector for CHARSET loaded from MAPFILE.
   Each line of MAPFILE has this form
	0xAAAA 0xCCCC
   where 0xAAAA is a code-point and 0xCCCC is the corresponding
   character code, or this form
	0xAAAA-0xBBBB 0xCCCC
   where 0xAAAA and 0xBBBB are code-points specifying a range, and
   0xCCCC is the first character code of the range.

   The returned vector has this form:
	[ CODE1 CHAR1 CODE2 CHAR2 .... ]
   where CODE1 is a code-point or a cons of code-points specifying a
   range.

   Note that this function uses `openp' to open MAPFILE but ignores
   `file-name-handler-alist' to avoid running any Lisp code.  */

static void
load_charset_map_from_file (struct charset *charset, Lisp_Object mapfile,
			    int control_flag)
{
  unsigned min_code = CHARSET_MIN_CODE (charset);
  unsigned max_code = CHARSET_MAX_CODE (charset);
  int fd;
  FILE *fp;
  struct charset_map_entries *head, *entries;
  int n_entries;
  AUTO_STRING (map, ".map");
  AUTO_STRING (txt, ".txt");
  AUTO_LIST2 (suffixes, map, txt);
  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_protect_nothing ();
  specbind (Qfile_name_handler_alist, Qnil);
  fd = openp (Vcharset_map_path, mapfile, suffixes, NULL, Qnil, false, false,
	      NULL);
  fp = fd < 0 ? 0 : emacs_fdopen (fd, "r");
  if (!fp)
    {
      int open_errno = errno;
      emacs_close (fd);
      report_file_errno ("Loading charset map", mapfile, open_errno);
    }
  set_unwind_protect_ptr (count, fclose_unwind, fp);
  unbind_to (specpdl_ref_add (count, 1), Qnil);

  /* Use record_xmalloc, as `charset_map_entries' is
     large (larger than MAX_ALLOCA).  */
  head = record_xmalloc (sizeof *head);
  entries = head;
  memset (entries, 0, sizeof (struct charset_map_entries));

  n_entries = 0;
  int ch = -1;
  while (true)
    {
      bool overflow = false;
      unsigned from = read_hex (fp, ch, &ch, &overflow), to;
      if (ch < 0)
	break;
      if (ch == '-')
	{
	  to = read_hex (fp, -1, &ch, &overflow);
	  if (ch < 0)
	    break;
	}
      else
	{
	  to = from;
	  ch = -1;
	}
      unsigned c = read_hex (fp, ch, &ch, &overflow);
      if (ch < 0)
	break;

      if (overflow)
	continue;
      if (from < min_code || to > max_code || from > to || c > MAX_CHAR)
	continue;

      if (n_entries == 0x10000)
	{
	  entries->next = record_xmalloc (sizeof *entries->next);
	  entries = entries->next;
	  memset (entries, 0, sizeof (struct charset_map_entries));
	  n_entries = 0;
	}
      int idx = n_entries;
      entries->entry[idx].from = from;
      entries->entry[idx].to = to;
      entries->entry[idx].c = c;
      n_entries++;
    }
  emacs_fclose (fp);
  clear_unwind_protect (count);

  load_charset_map (charset, head, n_entries, control_flag);
  unbind_to (count, Qnil);
}

static void
load_charset_map_from_vector (struct charset *charset, Lisp_Object vec, int control_flag)
{
  unsigned min_code = CHARSET_MIN_CODE (charset);
  unsigned max_code = CHARSET_MAX_CODE (charset);
  struct charset_map_entries *head, *entries;
  int n_entries;
  int len = ASIZE (vec);
  int i;
  USE_SAFE_ALLOCA;

  if (len % 2 == 1)
    {
      add_to_log ("Failure in loading charset map: %V", vec);
      return;
    }

  /* Use SAFE_ALLOCA instead of alloca, as `charset_map_entries' is
     large (larger than MAX_ALLOCA).  */
  head = SAFE_ALLOCA (sizeof *head);
  entries = head;
  memset (entries, 0, sizeof (struct charset_map_entries));

  n_entries = 0;
  for (i = 0; i < len; i += 2)
    {
      Lisp_Object val, val2;
      unsigned from, to;
      EMACS_INT c;
      int idx;

      val = AREF (vec, i);
      if (CONSP (val))
	{
	  val2 = XCDR (val);
	  val = XCAR (val);
	  from = XFIXNAT (val);
	  to = XFIXNAT (val2);
	}
      else
	from = to = XFIXNAT (val);
      val = AREF (vec, i + 1);
      CHECK_FIXNAT (val);
      c = XFIXNAT (val);

      if (from < min_code || to > max_code || from > to || c > MAX_CHAR)
	continue;

      if (n_entries > 0 && (n_entries % 0x10000) == 0)
	{
	  entries->next = SAFE_ALLOCA (sizeof *entries->next);
	  entries = entries->next;
	  memset (entries, 0, sizeof (struct charset_map_entries));
	}
      idx = n_entries % 0x10000;
      entries->entry[idx].from = from;
      entries->entry[idx].to = to;
      entries->entry[idx].c = c;
      n_entries++;
    }

  load_charset_map (charset, head, n_entries, control_flag);
  SAFE_FREE ();
}


/* Load a mapping table for CHARSET.  CONTROL-FLAG tells what kind of
   map it is (see the comment of load_charset_map for the detail).  */

static void
load_charset (struct charset *charset, int control_flag)
{
  Lisp_Object map;

  if (inhibit_load_charset_map
      && temp_charset_work
      && charset == temp_charset_work->current
      && ((control_flag == 2) == temp_charset_work->for_encoder))
    return;

  if (CHARSET_METHOD (charset) == CHARSET_METHOD_MAP)
    map = CHARSET_MAP (charset);
  else
    {
      if (! CHARSET_UNIFIED_P (charset))
	emacs_abort ();
      map = CHARSET_UNIFY_MAP (charset);
    }
  if (STRINGP (map))
    load_charset_map_from_file (charset, map, control_flag);
  else
    load_charset_map_from_vector (charset, map, control_flag);
}


DEFUN ("charsetp", Fcharsetp, Scharsetp, 1, 1, 0,
       doc: /* Return non-nil if and only if OBJECT is a charset.*/)
  (Lisp_Object object)
{
  return (CHARSETP (object) ? Qt : Qnil);
}


static void
map_charset_for_dump (void (*c_function) (Lisp_Object, Lisp_Object),
		      Lisp_Object function, Lisp_Object arg,
		      unsigned int from, unsigned int to)
{
  int from_idx = CODE_POINT_TO_INDEX (temp_charset_work->current, from);
  int to_idx = CODE_POINT_TO_INDEX (temp_charset_work->current, to);
  Lisp_Object range = Fcons (Qnil, Qnil);
  int c, stop;

  c = temp_charset_work->min_char;
  stop = (temp_charset_work->max_char < 0x20000
	  ? temp_charset_work->max_char : 0xFFFF);

  while (1)
    {
      int idx = GET_TEMP_CHARSET_WORK_ENCODER (c);

      if (idx >= from_idx && idx <= to_idx)
	{
	  if (NILP (XCAR (range)))
	    XSETCAR (range, make_fixnum (c));
	}
      else if (! NILP (XCAR (range)))
	{
	  XSETCDR (range, make_fixnum (c - 1));
	  if (c_function)
	    (*c_function) (arg, range);
	  else
	    calln (function, range, arg);
	  XSETCAR (range, Qnil);
	}
      if (c == stop)
	{
	  if (c == temp_charset_work->max_char)
	    {
	      if (! NILP (XCAR (range)))
		{
		  XSETCDR (range, make_fixnum (c));
		  if (c_function)
		    (*c_function) (arg, range);
		  else
		    calln (function, range, arg);
		}
	      break;
	    }
	  c = 0x1FFFF;
	  stop = temp_charset_work->max_char;
	}
      c++;
    }
}

void
map_charset_chars (void (*c_function)(Lisp_Object, Lisp_Object), Lisp_Object function,
		   Lisp_Object arg, struct charset *charset, unsigned from, unsigned to)
{
  Lisp_Object range;
  bool partial = (from > CHARSET_MIN_CODE (charset)
		  || to < CHARSET_MAX_CODE (charset));

  if (CHARSET_METHOD (charset) == CHARSET_METHOD_OFFSET)
    {
      int from_idx = CODE_POINT_TO_INDEX (charset, from);
      int to_idx = CODE_POINT_TO_INDEX (charset, to);
      int from_c = from_idx + CHARSET_CODE_OFFSET (charset);
      int to_c = to_idx + CHARSET_CODE_OFFSET (charset);

      if (CHARSET_UNIFIED_P (charset))
	{
	  if (! CHAR_TABLE_P (CHARSET_DEUNIFIER (charset)))
	    load_charset (charset, 2);
	  if (CHAR_TABLE_P (CHARSET_DEUNIFIER (charset)))
	    map_char_table_for_charset (c_function, function,
					CHARSET_DEUNIFIER (charset), arg,
					partial ? charset : NULL, from, to);
	  else
	    map_charset_for_dump (c_function, function, arg, from, to);
	}

      range = Fcons (make_fixnum (from_c), make_fixnum (to_c));
      if (NILP (function))
	(*c_function) (arg, range);
      else
	calln (function, range, arg);
    }
  else if (CHARSET_METHOD (charset) == CHARSET_METHOD_MAP)
    {
      if (! CHAR_TABLE_P (CHARSET_ENCODER (charset)))
	load_charset (charset, 2);
      if (CHAR_TABLE_P (CHARSET_ENCODER (charset)))
	map_char_table_for_charset (c_function, function,
				    CHARSET_ENCODER (charset), arg,
				    partial ? charset : NULL, from, to);
      else
	map_charset_for_dump (c_function, function, arg, from, to);
    }
  else if (CHARSET_METHOD (charset) == CHARSET_METHOD_SUBSET)
    {
      Lisp_Object subset_info;
      int offset;

      subset_info = CHARSET_SUBSET (charset);
      charset = CHARSET_FROM_ID (XFIXNAT (AREF (subset_info, 0)));
      offset = XFIXNUM (AREF (subset_info, 3));
      from -= offset;
      if (from < XFIXNAT (AREF (subset_info, 1)))
	from = XFIXNAT (AREF (subset_info, 1));
      to -= offset;
      if (to > XFIXNAT (AREF (subset_info, 2)))
	to = XFIXNAT (AREF (subset_info, 2));
      map_charset_chars (c_function, function, arg, charset, from, to);
    }
  else				/* i.e. CHARSET_METHOD_SUPERSET */
    {
      Lisp_Object parents;

      for (parents = CHARSET_SUPERSET (charset); CONSP (parents);
	   parents = XCDR (parents))
	{
	  int offset;
	  unsigned this_from, this_to;

	  charset = CHARSET_FROM_ID (XFIXNAT (XCAR (XCAR (parents))));
	  offset = XFIXNUM (XCDR (XCAR (parents)));
	  this_from = from > offset ? from - offset : 0;
	  this_to = to > offset ? to - offset : 0;
	  if (this_from < CHARSET_MIN_CODE (charset))
	    this_from = CHARSET_MIN_CODE (charset);
	  if (this_to > CHARSET_MAX_CODE (charset))
	    this_to = CHARSET_MAX_CODE (charset);
	  map_charset_chars (c_function, function, arg, charset,
			     this_from, this_to);
	}
    }
}

DEFUN ("map-charset-chars", Fmap_charset_chars, Smap_charset_chars, 2, 5, 0,
       doc: /* Call FUNCTION for all characters in CHARSET.
Optional 3rd argument ARG is an additional argument to be passed
to FUNCTION, see below.
Optional 4th and 5th arguments FROM-CODE and TO-CODE specify the
range of code points (in CHARSET) of target characters on which to
map the FUNCTION.  Note that these are not character codes, but code
points of CHARSET; for the difference see `decode-char' and
`list-charset-chars'.  If FROM-CODE is nil or imitted, it stands for
the first code point of CHARSET; if TO-CODE is nil or omitted, it
stands for the last code point of CHARSET.

FUNCTION will be called with two arguments: RANGE and ARG.
RANGE is a cons (FROM .  TO), where FROM and TO specify a range of
characters that belong to CHARSET on which FUNCTION should do its
job.  FROM and TO are Emacs character codes, unlike FROM-CODE and
TO-CODE, which are CHARSET code points.  */)
  (Lisp_Object function, Lisp_Object charset, Lisp_Object arg, Lisp_Object from_code, Lisp_Object to_code)
{
  struct charset *cs;
  unsigned from, to;

  CHECK_CHARSET_GET_CHARSET (charset, cs);
  if (NILP (from_code))
    from = CHARSET_MIN_CODE (cs);
  else
    {
      CHECK_FIXNAT (from_code);
      from = XFIXNUM (from_code);
      if (from < CHARSET_MIN_CODE (cs))
	from = CHARSET_MIN_CODE (cs);
    }
  if (NILP (to_code))
    to = CHARSET_MAX_CODE (cs);
  else
    {
      CHECK_FIXNAT (to_code);
      to = XFIXNUM (to_code);
      if (to > CHARSET_MAX_CODE (cs))
	to = CHARSET_MAX_CODE (cs);
    }
  map_charset_chars (NULL, function, arg, cs, from, to);
  return Qnil;
}


/* Define a charset according to the arguments.  The Nth argument is
   the Nth attribute of the charset (the last attribute `charset-id'
   is not included).  See the docstring of `define-charset' for the
   detail.  */

DEFUN ("define-charset-internal", Fdefine_charset_internal,
       Sdefine_charset_internal, charset_arg_max, MANY, 0,
       doc: /* For internal use only.
usage: (define-charset-internal ...)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  /* Charset attr vector.  */
  Lisp_Object attrs;
  Lisp_Object val;
  struct Lisp_Hash_Table *hash_table = XHASH_TABLE (Vcharset_hash_table);
  int i, j;
  struct charset charset;
  int id;
  int dimension;
  bool new_definition_p;
  int nchars;

  memset (&charset, 0, sizeof (charset));

  if (nargs != charset_arg_max)
    Fsignal (Qwrong_number_of_arguments,
	     Fcons (Qdefine_charset_internal,
		    make_fixnum (nargs)));

  attrs = make_nil_vector (charset_attr_max);

  CHECK_SYMBOL (args[charset_arg_name]);
  ASET (attrs, charset_name, args[charset_arg_name]);

  val = args[charset_arg_code_space];
  for (i = 0, dimension = 0, nchars = 1; ; i++)
    {
      Lisp_Object min_byte_obj = Faref (val, make_fixnum (i * 2));
      Lisp_Object max_byte_obj = Faref (val, make_fixnum (i * 2 + 1));
      int min_byte = check_integer_range (min_byte_obj, 0, 255);
      int max_byte = check_integer_range (max_byte_obj, min_byte, 255);
      charset.code_space[i * 4] = min_byte;
      charset.code_space[i * 4 + 1] = max_byte;
      charset.code_space[i * 4 + 2] = max_byte - min_byte + 1;
      if (max_byte > 0)
	dimension = i + 1;
      if (i == 3)
	break;
      nchars *= charset.code_space[i * 4 + 2];
      charset.code_space[i * 4 + 3] = nchars;
    }

  val = args[charset_arg_dimension];
  charset.dimension
    = !NILP (val) ? check_integer_range (val, 1, 4) : dimension;

  charset.code_linear_p
    = (charset.dimension == 1
       || (charset.code_space[2] == 256
	   && (charset.dimension == 2
	       || (charset.code_space[6] == 256
		   && (charset.dimension == 3
		       || charset.code_space[10] == 256)))));

  if (! charset.code_linear_p)
    {
      charset.code_space_mask = xzalloc (256);
      for (i = 0; i < 4; i++)
	for (j = charset.code_space[i * 4]; j <= charset.code_space[i * 4 + 1];
	     j++)
	  charset.code_space_mask[j] |= (1 << i);
    }

  charset.iso_chars_96 = charset.code_space[2] == 96;

  charset.min_code = (charset.code_space[0]
		      | (charset.code_space[4] << 8)
		      | (charset.code_space[8] << 16)
		      | ((unsigned) charset.code_space[12] << 24));
  charset.max_code = (charset.code_space[1]
		      | (charset.code_space[5] << 8)
		      | (charset.code_space[9] << 16)
		      | ((unsigned) charset.code_space[13] << 24));
  charset.char_index_offset = 0;

  val = args[charset_arg_min_code];
  if (! NILP (val))
    {
      unsigned code = cons_to_unsigned (val, UINT_MAX);

      if (code < charset.min_code
	  || code > charset.max_code)
	args_out_of_range_3 (INT_TO_INTEGER (charset.min_code),
			     INT_TO_INTEGER (charset.max_code), val);
      charset.char_index_offset = CODE_POINT_TO_INDEX (&charset, code);
      charset.min_code = code;
    }

  val = args[charset_arg_max_code];
  if (! NILP (val))
    {
      unsigned code = cons_to_unsigned (val, UINT_MAX);

      if (code < charset.min_code
	  || code > charset.max_code)
	args_out_of_range_3 (INT_TO_INTEGER (charset.min_code),
			     INT_TO_INTEGER (charset.max_code), val);
      charset.max_code = code;
    }

  charset.compact_codes_p = charset.max_code < 0x10000;

  val = args[charset_arg_invalid_code];
  if (NILP (val))
    {
      if (charset.min_code > 0)
	charset.invalid_code = 0;
      else
	{
	  if (charset.max_code < UINT_MAX)
	    charset.invalid_code = charset.max_code + 1;
	  else
	    error ("Attribute :invalid-code must be specified");
	}
    }
  else
    charset.invalid_code = cons_to_unsigned (val, UINT_MAX);

  val = args[charset_arg_iso_final];
  if (NILP (val))
    charset.iso_final = -1;
  else
    {
      CHECK_FIXNUM (val);
      if (XFIXNUM (val) < '0' || XFIXNUM (val) > 127)
	error ("Invalid iso-final-char: %"pI"d", XFIXNUM (val));
      charset.iso_final = XFIXNUM (val);
    }

  val = args[charset_arg_iso_revision];
  charset.iso_revision = !NILP (val) ? check_integer_range (val, -1, 63) : -1;

  val = args[charset_arg_emacs_mule_id];
  if (NILP (val))
    charset.emacs_mule_id = -1;
  else
    {
      CHECK_FIXNAT (val);
      if ((XFIXNUM (val) > 0 && XFIXNUM (val) <= 128) || XFIXNUM (val) >= 256)
	error ("Invalid emacs-mule-id: %"pI"d", XFIXNUM (val));
      charset.emacs_mule_id = XFIXNUM (val);
    }

  charset.ascii_compatible_p = ! NILP (args[charset_arg_ascii_compatible_p]);

  charset.supplementary_p = ! NILP (args[charset_arg_supplementary_p]);

  charset.unified_p = 0;

  memset (charset.fast_map, 0, sizeof (charset.fast_map));

  if (! NILP (args[charset_arg_code_offset]))
    {
      val = args[charset_arg_code_offset];
      CHECK_CHARACTER (val);

      charset.method = CHARSET_METHOD_OFFSET;
      charset.code_offset = XFIXNUM (val);

      i = CODE_POINT_TO_INDEX (&charset, charset.max_code);
      if (MAX_CHAR - charset.code_offset < i)
	error ("Unsupported max char: %d + %ud > MAX_CHAR (%d)",
	       i, charset.max_code, MAX_CHAR);
      charset.max_char = i + charset.code_offset;
      i = CODE_POINT_TO_INDEX (&charset, charset.min_code);
      charset.min_char = i + charset.code_offset;

      i = (charset.min_char >> 7) << 7;
      for (; i < 0x10000 && i <= charset.max_char; i += 128)
	CHARSET_FAST_MAP_SET (i, charset.fast_map);
      i = (i >> 12) << 12;
      for (; i <= charset.max_char; i += 0x1000)
	CHARSET_FAST_MAP_SET (i, charset.fast_map);
      if (charset.code_offset == 0 && charset.max_char >= 0x80)
	charset.ascii_compatible_p = 1;
    }
  else if (! NILP (args[charset_arg_map]))
    {
      val = args[charset_arg_map];
      ASET (attrs, charset_map, val);
      charset.method = CHARSET_METHOD_MAP;
    }
  else if (! NILP (args[charset_arg_subset]))
    {
      Lisp_Object parent;
      Lisp_Object parent_min_code, parent_max_code, parent_code_offset;
      struct charset *parent_charset;

      val = args[charset_arg_subset];
      parent = Fcar (val);
      CHECK_CHARSET_GET_CHARSET (parent, parent_charset);
      parent_min_code = Fnth (make_fixnum (1), val);
      CHECK_FIXNAT (parent_min_code);
      parent_max_code = Fnth (make_fixnum (2), val);
      CHECK_FIXNAT (parent_max_code);
      parent_code_offset = Fnth (make_fixnum (3), val);
      CHECK_FIXNUM (parent_code_offset);
      ASET (attrs, charset_subset,
	    CALLN (Fvector, make_fixnum (parent_charset->id),
		   parent_min_code, parent_max_code, parent_code_offset));

      charset.method = CHARSET_METHOD_SUBSET;
      /* Here, we just copy the parent's fast_map.  It's not accurate,
	 but at least it works for quickly detecting which character
	 DOESN'T belong to this charset.  */
      memcpy (charset.fast_map, parent_charset->fast_map,
	      sizeof charset.fast_map);

      /* We also copy these for parents.  */
      charset.min_char = parent_charset->min_char;
      charset.max_char = parent_charset->max_char;
    }
  else if (! NILP (args[charset_arg_superset]))
    {
      val = args[charset_arg_superset];
      charset.method = CHARSET_METHOD_SUPERSET;
      val = Fcopy_sequence (val);
      ASET (attrs, charset_superset, val);

      charset.min_char = MAX_CHAR;
      charset.max_char = 0;
      for (; ! NILP (val); val = Fcdr (val))
	{
	  Lisp_Object elt, car_part, cdr_part;
	  int this_id, offset;
	  struct charset *this_charset;

	  elt = Fcar (val);
	  if (CONSP (elt))
	    {
	      car_part = XCAR (elt);
	      cdr_part = XCDR (elt);
	      CHECK_CHARSET_GET_ID (car_part, this_id);
	      offset = check_integer_range (cdr_part, INT_MIN, INT_MAX);
	    }
	  else
	    {
	      CHECK_CHARSET_GET_ID (elt, this_id);
	      offset = 0;
	    }
	  XSETCAR (val, Fcons (make_fixnum (this_id), make_fixnum (offset)));

	  this_charset = CHARSET_FROM_ID (this_id);
	  if (charset.min_char > this_charset->min_char)
	    charset.min_char = this_charset->min_char;
	  if (charset.max_char < this_charset->max_char)
	    charset.max_char = this_charset->max_char;
	  for (i = 0; i < 190; i++)
	    charset.fast_map[i] |= this_charset->fast_map[i];
	}
    }
  else
    error ("None of :code-offset, :map, :parents are specified");

  val = args[charset_arg_unify_map];
  if (! NILP (val) && !STRINGP (val))
    CHECK_VECTOR (val);
  ASET (attrs, charset_unify_map, val);

  CHECK_LIST (args[charset_arg_plist]);
  ASET (attrs, charset_plist, args[charset_arg_plist]);

  hash_hash_t hash_code;
  ptrdiff_t hash_index
    = hash_find_get_hash (hash_table, args[charset_arg_name], &hash_code);
  if (hash_index >= 0)
    {
      new_definition_p = false;
      id = XFIXNAT (CHARSET_SYMBOL_ID (args[charset_arg_name]));
      set_hash_value_slot (hash_table, hash_index, attrs);
    }
  else
    {
      hash_put (hash_table, args[charset_arg_name], attrs, hash_code);
      if (charset_table_used == charset_table_size)
	{
	  /* Ensure that charset IDs fit into 'int' as well as into the
	     restriction imposed by fixnums.  Although the 'int' restriction
	     could be removed, too much other code would need altering; for
	     example, the IDs are stuffed into struct
	     coding_system.charbuf[i] entries, which are 'int'.  */
	  int old_size = charset_table_size;
	  ptrdiff_t new_size = old_size;
	  struct charset *new_table =
#ifdef HAVE_MPS
	    igc_xpalloc_ambig
#else
	    xpalloc
#endif
	    (0, &new_size, 1,
	     min (INT_MAX, MOST_POSITIVE_FIXNUM),
	     sizeof *charset_table);
          memcpy (new_table, charset_table, old_size * sizeof *new_table);
          charset_table = new_table;
	  charset_table_size = new_size;
	  /* FIXME: This leaks memory, as the old charset_table becomes
	     unreachable.  If the old charset table is charset_table_init
	     then this leak is intentional; otherwise, it's unclear.
	     If the latter memory leak is intentional, a
	     comment should be added to explain this.  If not, the old
	     charset_table should be freed, by passing it as the 1st argument
	     to xpalloc and removing the memcpy.  */
	}
      id = charset_table_used++;
      new_definition_p = 1;
    }

  ASET (attrs, charset_id, make_fixnum (id));
  charset.id = id;
  charset.attributes = attrs;
  charset_table[id] = charset;

  if (charset.method == CHARSET_METHOD_MAP)
    {
      load_charset (&charset, 0);
      charset_table[id] = charset;
    }

  if (charset.iso_final >= 0)
    {
      ISO_CHARSET_TABLE (charset.dimension, charset.iso_chars_96,
			 charset.iso_final) = id;
      if (new_definition_p)
	Viso_2022_charset_list = nconc2 (Viso_2022_charset_list, list1i (id));
      if (ISO_CHARSET_TABLE (1, 0, 'J') == id)
	charset_jisx0201_roman = id;
      else if (ISO_CHARSET_TABLE (2, 0, '@') == id)
	charset_jisx0208_1978 = id;
      else if (ISO_CHARSET_TABLE (2, 0, 'B') == id)
	charset_jisx0208 = id;
      else if (ISO_CHARSET_TABLE (2, 0, 'C') == id)
	charset_ksc5601 = id;
    }

  if (charset.emacs_mule_id >= 0)
    {
      emacs_mule_charset[charset.emacs_mule_id] = id;
      if (charset.emacs_mule_id < 0xA0)
	emacs_mule_bytes[charset.emacs_mule_id] = charset.dimension + 1;
      else
	emacs_mule_bytes[charset.emacs_mule_id] = charset.dimension + 2;
      if (new_definition_p)
	Vemacs_mule_charset_list = nconc2 (Vemacs_mule_charset_list,
					   list1i (id));
    }

  if (new_definition_p)
    {
      Vcharset_list = Fcons (args[charset_arg_name], Vcharset_list);
      if (charset.supplementary_p)
	Vcharset_ordered_list = nconc2 (Vcharset_ordered_list, list1i (id));
      else
	{
	  Lisp_Object tail;

	  for (tail = Vcharset_ordered_list; CONSP (tail); tail = XCDR (tail))
	    {
	      struct charset *cs = CHARSET_FROM_ID (XFIXNUM (XCAR (tail)));

	      if (cs->supplementary_p)
		break;
	    }
	  if (EQ (tail, Vcharset_ordered_list))
	    Vcharset_ordered_list = Fcons (make_fixnum (id),
					   Vcharset_ordered_list);
	  else if (NILP (tail))
	    Vcharset_ordered_list = nconc2 (Vcharset_ordered_list,
					    list1i (id));
	  else
	    {
	      val = Fcons (XCAR (tail), XCDR (tail));
	      XSETCDR (tail, val);
	      XSETCAR (tail, make_fixnum (id));
	    }
	}
      charset_ordered_list_tick++;
    }

  return Qnil;
}


/* Same as Fdefine_charset_internal but arguments are more convenient
   to call from C (typically in syms_of_charset).  This can define a
   charset of `offset' method only.  Return the ID of the new
   charset.  */

static int
define_charset_internal (Lisp_Object name,
			 int dimension,
			 const char *code_space_chars,
			 unsigned min_code, unsigned max_code,
			 int iso_final, int iso_revision, int emacs_mule_id,
			 bool ascii_compatible, bool supplementary,
			 int code_offset)
{
  const unsigned char *code_space = (const unsigned char *) code_space_chars;
  Lisp_Object args[charset_arg_max];
  Lisp_Object val;
  int i;

  args[charset_arg_name] = name;
  args[charset_arg_dimension] = make_fixnum (dimension);
  val = make_uninit_vector (8);
  for (i = 0; i < 8; i++)
    ASET (val, i, make_fixnum (code_space[i]));
  args[charset_arg_code_space] = val;
  args[charset_arg_min_code] = make_fixnum (min_code);
  args[charset_arg_max_code] = make_fixnum (max_code);
  args[charset_arg_iso_final]
    = (iso_final < 0 ? Qnil : make_fixnum (iso_final));
  args[charset_arg_iso_revision] = make_fixnum (iso_revision);
  args[charset_arg_emacs_mule_id]
    = (emacs_mule_id < 0 ? Qnil : make_fixnum (emacs_mule_id));
  args[charset_arg_ascii_compatible_p] = ascii_compatible ? Qt : Qnil;
  args[charset_arg_supplementary_p] = supplementary ? Qt : Qnil;
  args[charset_arg_invalid_code] = Qnil;
  args[charset_arg_code_offset] = make_fixnum (code_offset);
  args[charset_arg_map] = Qnil;
  args[charset_arg_subset] = Qnil;
  args[charset_arg_superset] = Qnil;
  args[charset_arg_unify_map] = Qnil;

  args[charset_arg_plist] =
     list (QCname,
	   args[charset_arg_name],
	   intern_c_string (":dimension"),
	   args[charset_arg_dimension],
	   intern_c_string (":code-space"),
	   args[charset_arg_code_space],
	   intern_c_string (":iso-final-char"),
	   args[charset_arg_iso_final],
	   intern_c_string (":emacs-mule-id"),
	   args[charset_arg_emacs_mule_id],
	   QCascii_compatible_p,
	   args[charset_arg_ascii_compatible_p],
	   intern_c_string (":code-offset"),
	   args[charset_arg_code_offset]);
  Fdefine_charset_internal (charset_arg_max, args);

  return XFIXNUM (CHARSET_SYMBOL_ID (name));
}


DEFUN ("define-charset-alias", Fdefine_charset_alias,
       Sdefine_charset_alias, 2, 2, 0,
       doc: /* Define ALIAS as an alias for charset CHARSET.  */)
  (Lisp_Object alias, Lisp_Object charset)
{
  Lisp_Object attr;

  CHECK_CHARSET_GET_ATTR (charset, attr);
  Fputhash (alias, attr, Vcharset_hash_table);
  Vcharset_list = Fcons (alias, Vcharset_list);
  return Qnil;
}


DEFUN ("charset-plist", Fcharset_plist, Scharset_plist, 1, 1, 0,
       doc: /* Return the property list of CHARSET.  */)
  (Lisp_Object charset)
{
  Lisp_Object attrs;

  CHECK_CHARSET_GET_ATTR (charset, attrs);
  return CHARSET_ATTR_PLIST (attrs);
}


DEFUN ("set-charset-plist", Fset_charset_plist, Sset_charset_plist, 2, 2, 0,
       doc: /* Set CHARSET's property list to PLIST.  */)
  (Lisp_Object charset, Lisp_Object plist)
{
  Lisp_Object attrs;

  CHECK_CHARSET_GET_ATTR (charset, attrs);
  ASET (attrs, charset_plist, plist);
  return plist;
}


DEFUN ("unify-charset", Funify_charset, Sunify_charset, 1, 3, 0,
       doc: /* Unify characters of CHARSET with Unicode.
This means reading the relevant file and installing the table defined
by CHARSET's `:unify-map' property.

Optional second arg UNIFY-MAP is a file name string or a vector.  It has
the same meaning as the `:unify-map' attribute in the function
`define-charset' (which see).

Optional third argument DEUNIFY, if non-nil, means to de-unify CHARSET.  */)
  (Lisp_Object charset, Lisp_Object unify_map, Lisp_Object deunify)
{
  int id;
  struct charset *cs;

  CHECK_CHARSET_GET_ID (charset, id);
  cs = CHARSET_FROM_ID (id);
  if (NILP (deunify)
      ? CHARSET_UNIFIED_P (cs) && ! NILP (CHARSET_DEUNIFIER (cs))
      : ! CHARSET_UNIFIED_P (cs))
    return Qnil;

  CHARSET_UNIFIED_P (cs) = 0;
  if (NILP (deunify))
    {
      if (CHARSET_METHOD (cs) != CHARSET_METHOD_OFFSET
	  || CHARSET_CODE_OFFSET (cs) < 0x110000)
	error ("Can't unify charset: %s", SDATA (SYMBOL_NAME (charset)));
      if (NILP (unify_map))
	unify_map = CHARSET_UNIFY_MAP (cs);
      else
	{
	  if (! STRINGP (unify_map) && ! VECTORP (unify_map))
	    signal_error ("Bad unify-map", unify_map);
	  set_charset_attr (cs, charset_unify_map, unify_map);
	}
      if (NILP (Vchar_unify_table))
	Vchar_unify_table = Fmake_char_table (Qnil, Qnil);
      char_table_set_range (Vchar_unify_table,
			    cs->min_char, cs->max_char, charset);
      CHARSET_UNIFIED_P (cs) = 1;
    }
  else if (CHAR_TABLE_P (Vchar_unify_table))
    {
      unsigned min_code = CHARSET_MIN_CODE (cs);
      unsigned max_code = CHARSET_MAX_CODE (cs);
      int min_char = DECODE_CHAR (cs, min_code);
      int max_char = DECODE_CHAR (cs, max_code);

      char_table_set_range (Vchar_unify_table, min_char, max_char, Qnil);
    }

  return Qnil;
}

/* Check that DIMENSION, CHARS, and FINAL_CHAR specify a valid ISO charset.
   Return true if it's a 96-character set, false if 94.  */

static bool
check_iso_charset_parameter (Lisp_Object dimension, Lisp_Object chars,
			     Lisp_Object final_char)
{
  CHECK_FIXNUM (dimension);
  CHECK_FIXNUM (chars);
  CHECK_CHARACTER (final_char);

  if (! (1 <= XFIXNUM (dimension) && XFIXNUM (dimension) <= 3))
    error ("Invalid DIMENSION %"pI"d, it should be 1, 2, or 3",
	   XFIXNUM (dimension));

  bool chars_flag = XFIXNUM (chars) == 96;
  if (! (chars_flag || XFIXNUM (chars) == 94))
    error ("Invalid CHARS %"pI"d, it should be 94 or 96", XFIXNUM (chars));

  int final_ch = XFIXNAT (final_char);
  if (! ('0' <= final_ch && final_ch <= '~'))
    error ("Invalid FINAL-CHAR `%c', it should be `0'..`~'", final_ch);

  return chars_flag;
}

DEFUN ("get-unused-iso-final-char", Fget_unused_iso_final_char,
       Sget_unused_iso_final_char, 2, 2, 0,
       doc: /*
Return an unused ISO final char for a charset of DIMENSION and CHARS.
DIMENSION is the number of bytes to represent a character: 1 or 2.
CHARS is the number of characters in a dimension: 94 or 96.

This final char is for private use, thus the range is `0' (48) .. `?' (63).
If there's no unused final char for the specified kind of charset,
return nil.  */)
  (Lisp_Object dimension, Lisp_Object chars)
{
  bool chars_flag = check_iso_charset_parameter (dimension, chars,
						 make_fixnum ('0'));
  for (int final_char = '0'; final_char <= '?'; final_char++)
    if (ISO_CHARSET_TABLE (XFIXNUM (dimension), chars_flag, final_char) < 0)
      return make_fixnum (final_char);
  return Qnil;
}


DEFUN ("declare-equiv-charset", Fdeclare_equiv_charset, Sdeclare_equiv_charset,
       4, 4, 0,
       doc: /* Declare an equivalent charset for ISO-2022 decoding.

On decoding by an ISO-2022 base coding system, when a charset
specified by DIMENSION, CHARS, and FINAL-CHAR is designated, behave as
if CHARSET is designated instead.  */)
  (Lisp_Object dimension, Lisp_Object chars, Lisp_Object final_char, Lisp_Object charset)
{
  int id;

  CHECK_CHARSET_GET_ID (charset, id);
  bool chars_flag = check_iso_charset_parameter (dimension, chars, final_char);
  ISO_CHARSET_TABLE (XFIXNUM (dimension), chars_flag, XFIXNAT (final_char)) = id;
  return Qnil;
}


/* Return information about charsets in the text at PTR of NBYTES
   bytes, which are NCHARS characters.  The value is:

	0: Each character is represented by one byte.  This is always
	   true for a unibyte string.  For a multibyte string, true if
	   it contains only ASCII characters.

	1: No charsets other than ascii, control-1, and latin-1 are
	   found.

	2: Otherwise.
*/

int
string_xstring_p (Lisp_Object string)
{
  const unsigned char *p = SDATA (string);
  const unsigned char *endp = p + SBYTES (string);

  if (SCHARS (string) == SBYTES (string))
    return 0;

  while (p < endp)
    {
      int c = string_char_advance (&p);

      if (c >= 0x100)
	return 2;
    }
  return 1;
}


/* Find charsets in the string at PTR of NCHARS and NBYTES.

   CHARSETS is a vector.  If Nth element is non-nil, it means the
   charset whose id is N is already found.

   It may lookup a translation table TABLE if supplied.  */

static void
find_charsets_in_text (const unsigned char *ptr, ptrdiff_t nchars,
		       ptrdiff_t nbytes, Lisp_Object charsets,
		       Lisp_Object table, bool multibyte)
{
  const unsigned char *pend = ptr + nbytes;

  if (nchars == nbytes)
    {
      if (multibyte)
	ASET (charsets, charset_ascii, Qt);
      else
	while (ptr < pend)
	  {
	    int c = *ptr++;

	    if (!NILP (table))
	      c = translate_char (table, c);
	    if (ASCII_CHAR_P (c))
	      ASET (charsets, charset_ascii, Qt);
	    else
	      ASET (charsets, charset_eight_bit, Qt);
	  }
    }
  else
    {
      while (ptr < pend)
	{
	  int c = string_char_advance (&ptr);
	  struct charset *charset;

	  if (!NILP (table))
	    c = translate_char (table, c);
	  charset = CHAR_CHARSET (c);
	  ASET (charsets, CHARSET_ID (charset), Qt);
	}
    }
}

DEFUN ("find-charset-region", Ffind_charset_region, Sfind_charset_region,
       2, 3, 0,
       doc: /* Return a list of charsets in the region between BEG and END.
BEG and END are buffer positions.
Optional arg TABLE if non-nil is a translation table to look up.

If the current buffer is unibyte, the returned list may contain
only `ascii', `eight-bit-control', and `eight-bit-graphic'.  */)
  (Lisp_Object beg, Lisp_Object end, Lisp_Object table)
{
  Lisp_Object charsets;
  ptrdiff_t from, from_byte, to, stop, stop_byte;
  int i;
  Lisp_Object val;
  bool multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));

  validate_region (&beg, &end);
  from = XFIXNAT (beg);
  stop = to = XFIXNAT (end);

  if (from < GPT && GPT < to)
    {
      stop = GPT;
      stop_byte = GPT_BYTE;
    }
  else
    stop_byte = CHAR_TO_BYTE (stop);

  from_byte = CHAR_TO_BYTE (from);

  charsets = make_nil_vector (charset_table_used);
  while (1)
    {
      find_charsets_in_text (BYTE_POS_ADDR (from_byte), stop - from,
			     stop_byte - from_byte, charsets, table,
			     multibyte);
      if (stop < to)
	{
	  from = stop, from_byte = stop_byte;
	  stop = to, stop_byte = CHAR_TO_BYTE (stop);
	}
      else
	break;
    }

  val = Qnil;
  for (i = charset_table_used - 1; i >= 0; i--)
    if (!NILP (AREF (charsets, i)))
      val = Fcons (CHARSET_NAME (charset_table + i), val);
  return val;
}

DEFUN ("find-charset-string", Ffind_charset_string, Sfind_charset_string,
       1, 2, 0,
       doc: /* Return a list of charsets in STR.
Optional arg TABLE if non-nil is a translation table to look up.

If STR is unibyte, the returned list may contain
only `ascii', `eight-bit-control', and `eight-bit-graphic'. */)
  (Lisp_Object str, Lisp_Object table)
{
  CHECK_STRING (str);

  Lisp_Object charsets = make_nil_vector (charset_table_used);
  find_charsets_in_text (SDATA (str), SCHARS (str), SBYTES (str),
			 charsets, table,
			 STRING_MULTIBYTE (str));
  Lisp_Object val = Qnil;
  for (int i = charset_table_used - 1; i >= 0; i--)
    if (!NILP (AREF (charsets, i)))
      val = Fcons (CHARSET_NAME (charset_table + i), val);
  return val;
}



/* Return a unified character code for C (>= 0x110000).  VAL is a
   value of Vchar_unify_table for C; i.e. it is nil, an integer, or a
   charset symbol.  */
static int
maybe_unify_char (int c, Lisp_Object val)
{
  struct charset *charset;

  if (FIXNUMP (val))
    return XFIXNAT (val);
  if (NILP (val))
    return c;

  CHECK_CHARSET_GET_CHARSET (val, charset);
#ifdef REL_ALLOC
  /* The call to load_charset below can allocate memory, which screws
     callers of this function through STRING_CHAR_* macros that hold C
     pointers to buffer text, if REL_ALLOC is used.  */
  r_alloc_inhibit_buffer_relocation (1);
#endif
  load_charset (charset, 1);
  if (! inhibit_load_charset_map)
    {
      val = CHAR_TABLE_REF (Vchar_unify_table, c);
      if (! NILP (val))
	c = XFIXNAT (val);
    }
  else
    {
      int code_index = c - CHARSET_CODE_OFFSET (charset);
      int unified = GET_TEMP_CHARSET_WORK_DECODER (code_index);

      if (unified > 0)
	c = unified;
    }
#ifdef REL_ALLOC
  r_alloc_inhibit_buffer_relocation (0);
#endif
  return c;
}


/* Return a character corresponding to the code-point CODE of
   CHARSET.  */

int
decode_char (struct charset *charset, unsigned int code)
{
  int c, char_index;
  enum charset_method method = CHARSET_METHOD (charset);

  if (code < CHARSET_MIN_CODE (charset) || code > CHARSET_MAX_CODE (charset))
    return -1;

  if (method == CHARSET_METHOD_SUBSET)
    {
      Lisp_Object subset_info;

      subset_info = CHARSET_SUBSET (charset);
      charset = CHARSET_FROM_ID (XFIXNAT (AREF (subset_info, 0)));
      code -= XFIXNUM (AREF (subset_info, 3));
      if (code < XFIXNAT (AREF (subset_info, 1))
	  || code > XFIXNAT (AREF (subset_info, 2)))
	c = -1;
      else
	c = DECODE_CHAR (charset, code);
    }
  else if (method == CHARSET_METHOD_SUPERSET)
    {
      Lisp_Object parents;

      parents = CHARSET_SUPERSET (charset);
      c = -1;
      for (; CONSP (parents); parents = XCDR (parents))
	{
	  int id = XFIXNUM (XCAR (XCAR (parents)));
	  int code_offset = XFIXNUM (XCDR (XCAR (parents)));
	  unsigned this_code = code - code_offset;

	  charset = CHARSET_FROM_ID (id);
	  if ((c = DECODE_CHAR (charset, this_code)) >= 0)
	    break;
	}
    }
  else
    {
      char_index = CODE_POINT_TO_INDEX (charset, code);
      if (char_index < 0)
	return -1;

      if (method == CHARSET_METHOD_MAP)
	{
	  Lisp_Object decoder;

	  decoder = CHARSET_DECODER (charset);
	  if (! VECTORP (decoder))
	    {
	      load_charset (charset, 1);
	      decoder = CHARSET_DECODER (charset);
	    }
	  if (VECTORP (decoder))
	    c = XFIXNUM (AREF (decoder, char_index));
	  else
	    c = GET_TEMP_CHARSET_WORK_DECODER (char_index);
	}
      else			/* method == CHARSET_METHOD_OFFSET */
	{
	  c = char_index + CHARSET_CODE_OFFSET (charset);
	  if (CHARSET_UNIFIED_P (charset)
	      && MAX_UNICODE_CHAR < c && c <= MAX_5_BYTE_CHAR)
	    {
	      /* Unify C with a Unicode character if possible.  */
	      Lisp_Object val = CHAR_TABLE_REF (Vchar_unify_table, c);
	      c = maybe_unify_char (c, val);
	    }
	}
    }

  return c;
}

/* Variable used temporarily by the macro ENCODE_CHAR.  */
Lisp_Object charset_work;

/* Return a code-point of C in CHARSET.  If C doesn't belong to
   CHARSET, return CHARSET_INVALID_CODE (CHARSET).  If STRICT is true,
   use CHARSET's strict_max_char instead of max_char.  */

unsigned
encode_char (struct charset *charset, int c)
{
  unsigned code;
  enum charset_method method = CHARSET_METHOD (charset);

  if (CHARSET_UNIFIED_P (charset))
    {
      Lisp_Object deunifier;
      int code_index = -1;

      deunifier = CHARSET_DEUNIFIER (charset);
      if (! CHAR_TABLE_P (deunifier))
	{
	  load_charset (charset, 2);
	  deunifier = CHARSET_DEUNIFIER (charset);
	}
      if (CHAR_TABLE_P (deunifier))
	{
	  Lisp_Object deunified = CHAR_TABLE_REF (deunifier, c);

	  if (FIXNUMP (deunified))
	    code_index = XFIXNUM (deunified);
	}
      else
	{
	  code_index = GET_TEMP_CHARSET_WORK_ENCODER (c);
	}
      if (code_index >= 0)
	c = CHARSET_CODE_OFFSET (charset) + code_index;
    }

  if (method == CHARSET_METHOD_SUBSET)
    {
      Lisp_Object subset_info;
      struct charset *this_charset;

      subset_info = CHARSET_SUBSET (charset);
      this_charset = CHARSET_FROM_ID (XFIXNAT (AREF (subset_info, 0)));
      code = ENCODE_CHAR (this_charset, c);
      if (code == CHARSET_INVALID_CODE (this_charset)
	  || code < XFIXNAT (AREF (subset_info, 1))
	  || code > XFIXNAT (AREF (subset_info, 2)))
	return CHARSET_INVALID_CODE (charset);
      code += XFIXNUM (AREF (subset_info, 3));
      return code;
    }

  if (method == CHARSET_METHOD_SUPERSET)
    {
      Lisp_Object parents;

      parents = CHARSET_SUPERSET (charset);
      for (; CONSP (parents); parents = XCDR (parents))
	{
	  int id = XFIXNUM (XCAR (XCAR (parents)));
	  int code_offset = XFIXNUM (XCDR (XCAR (parents)));
	  struct charset *this_charset = CHARSET_FROM_ID (id);

	  code = ENCODE_CHAR (this_charset, c);
	  if (code != CHARSET_INVALID_CODE (this_charset))
	    return code + code_offset;
	}
      return CHARSET_INVALID_CODE (charset);
    }

  if (! CHARSET_FAST_MAP_REF (c, charset->fast_map)
      || c < CHARSET_MIN_CHAR (charset) || c > CHARSET_MAX_CHAR (charset))
    return CHARSET_INVALID_CODE (charset);

  if (method == CHARSET_METHOD_MAP)
    {
      Lisp_Object encoder;
      Lisp_Object val;

      encoder = CHARSET_ENCODER (charset);
      if (! CHAR_TABLE_P (CHARSET_ENCODER (charset)))
	{
	  load_charset (charset, 2);
	  encoder = CHARSET_ENCODER (charset);
	}
      if (CHAR_TABLE_P (encoder))
	{
	  val = CHAR_TABLE_REF (encoder, c);
	  if (NILP (val))
	    return CHARSET_INVALID_CODE (charset);
	  code = XFIXNUM (val);
	  if (! CHARSET_COMPACT_CODES_P (charset))
	    code = INDEX_TO_CODE_POINT (charset, code);
	}
      else
	{
	  code = GET_TEMP_CHARSET_WORK_ENCODER (c);
	  code = INDEX_TO_CODE_POINT (charset, code);
	}
    }
  else				/* method == CHARSET_METHOD_OFFSET */
    {
      unsigned code_index = c - CHARSET_CODE_OFFSET (charset);

      code = INDEX_TO_CODE_POINT (charset, code_index);
    }

  return code;
}


DEFUN ("decode-char", Fdecode_char, Sdecode_char, 2, 2, 0,
       doc: /* Decode the pair of CHARSET and CODE-POINT into a character.
Return nil if CODE-POINT is not valid in CHARSET.

CODE-POINT may be a cons (HIGHER-16-BIT-VALUE . LOWER-16-BIT-VALUE),
although this usage is obsolescent.  */)
  (Lisp_Object charset, Lisp_Object code_point)
{
  int c, id;
  unsigned code;
  struct charset *charsetp;

  CHECK_CHARSET_GET_ID (charset, id);
  code = cons_to_unsigned (code_point, UINT_MAX);
  charsetp = CHARSET_FROM_ID (id);
  c = DECODE_CHAR (charsetp, code);
  return (c >= 0 ? make_fixnum (c) : Qnil);
}


DEFUN ("encode-char", Fencode_char, Sencode_char, 2, 2, 0,
       doc: /* Encode the character CH into a code-point of CHARSET.
Return the encoded code-point as an integer,
or nil if CHARSET doesn't support CH.  */)
  (Lisp_Object ch, Lisp_Object charset)
{
  int c, id;
  unsigned code;
  struct charset *charsetp;

  CHECK_CHARSET_GET_ID (charset, id);
  CHECK_CHARACTER (ch);
  c = XFIXNAT (ch);
  charsetp = CHARSET_FROM_ID (id);
  code = ENCODE_CHAR (charsetp, c);
  if (code == CHARSET_INVALID_CODE (charsetp))
    return Qnil;
  /* There are much fewer codepoints in the world than we have positive
     fixnums, so it could be argued that we never really need a bignum,
     e.g. Unicode codepoints only need 21bit, and China's GB-10830
     can fit in 22bit.  Yet we encode GB-10830's chars in a sparse way
     (we just take the 4byte sequences as a 32bit int), so some
     GB-10830 chars (such as 0x81308130 in etc/charsets/gb108304.map) end
     up represented as bignums if EMACS_INT is 32 bits.  */
  return INT_TO_INTEGER (code);
}


DEFUN ("make-char", Fmake_char, Smake_char, 1, 5, 0,
       doc:
       /* Return a character of CHARSET whose position codes are CODEn.

CODE1 through CODE4 are optional, but if you don't supply sufficient
position codes, it is assumed that the minimum code in each dimension
is specified.  */)
  (Lisp_Object charset, Lisp_Object code1, Lisp_Object code2, Lisp_Object code3, Lisp_Object code4)
{
  int id, dimension;
  struct charset *charsetp;
  unsigned code;
  int c;

  CHECK_CHARSET_GET_ID (charset, id);
  charsetp = CHARSET_FROM_ID (id);

  dimension = CHARSET_DIMENSION (charsetp);
  if (NILP (code1))
    code = (CHARSET_ASCII_COMPATIBLE_P (charsetp)
	    ? 0 : CHARSET_MIN_CODE (charsetp));
  else
    {
      CHECK_FIXNAT (code1);
      if (XFIXNAT (code1) >= 0x100)
	args_out_of_range (make_fixnum (0xFF), code1);
      code = XFIXNAT (code1);

      if (dimension > 1)
	{
	  code <<= 8;
	  if (NILP (code2))
	    code |= charsetp->code_space[(dimension - 2) * 4];
	  else
	    {
	      CHECK_FIXNAT (code2);
	      if (XFIXNAT (code2) >= 0x100)
		args_out_of_range (make_fixnum (0xFF), code2);
	      code |= XFIXNAT (code2);
	    }

	  if (dimension > 2)
	    {
	      code <<= 8;
	      if (NILP (code3))
		code |= charsetp->code_space[(dimension - 3) * 4];
	      else
		{
		  CHECK_FIXNAT (code3);
		  if (XFIXNAT (code3) >= 0x100)
		    args_out_of_range (make_fixnum (0xFF), code3);
		  code |= XFIXNAT (code3);
		}

	      if (dimension > 3)
		{
		  code <<= 8;
		  if (NILP (code4))
		    code |= charsetp->code_space[0];
		  else
		    {
		      CHECK_FIXNAT (code4);
		      if (XFIXNAT (code4) >= 0x100)
			args_out_of_range (make_fixnum (0xFF), code4);
		      code |= XFIXNAT (code4);
		    }
		}
	    }
	}
    }

  if (CHARSET_ISO_FINAL (charsetp) >= 0)
    code &= 0x7F7F7F7F;
  c = DECODE_CHAR (charsetp, code);
  if (c < 0)
    error ("Invalid code(s)");
  return make_fixnum (c);
}


/* Return the first charset in CHARSET_LIST that contains C.
   CHARSET_LIST is a list of charset IDs.  If it is nil, use
   Vcharset_ordered_list.  */

struct charset *
char_charset (int c, Lisp_Object charset_list, unsigned int *code_return)
{
  bool maybe_null = 0;

  if (NILP (charset_list))
    charset_list = Vcharset_ordered_list;
  else
    maybe_null = 1;

  while (CONSP (charset_list))
    {
      struct charset *charset = CHARSET_FROM_ID (XFIXNUM (XCAR (charset_list)));
      unsigned code = ENCODE_CHAR (charset, c);

      if (code != CHARSET_INVALID_CODE (charset))
	{
	  if (code_return)
	    *code_return = code;
	  return charset;
	}
      charset_list = XCDR (charset_list);
      if (! maybe_null
	  && c <= MAX_UNICODE_CHAR
	  && EQ (charset_list, Vcharset_non_preferred_head))
	return CHARSET_FROM_ID (charset_unicode);
    }
  return (maybe_null ? NULL
	  : c <= MAX_5_BYTE_CHAR ? CHARSET_FROM_ID (charset_emacs)
	  : CHARSET_FROM_ID (charset_eight_bit));
}


DEFUN ("split-char", Fsplit_char, Ssplit_char, 1, 1, 0,
       doc:
       /*Return list of charset and one to four position-codes of CH.
The charset is decided by the current priority order of charsets.
A position-code is a byte value of each dimension of the code-point of
CH in the charset.  */)
  (Lisp_Object ch)
{
  struct charset *charset;
  int c, dimension;
  unsigned code;
  Lisp_Object val;

  CHECK_CHARACTER (ch);
  c = XFIXNAT (ch);
  charset = CHAR_CHARSET (c);
  if (! charset)
    emacs_abort ();
  code = ENCODE_CHAR (charset, c);
  if (code == CHARSET_INVALID_CODE (charset))
    emacs_abort ();
  dimension = CHARSET_DIMENSION (charset);
  for (val = Qnil; dimension > 0; dimension--)
    {
      val = Fcons (make_fixnum (code & 0xFF), val);
      code >>= 8;
    }
  return Fcons (CHARSET_NAME (charset), val);
}


DEFUN ("char-charset", Fchar_charset, Schar_charset, 1, 2, 0,
       doc: /* Return the charset of highest priority that contains CH.
ASCII characters are an exception: for them, this function always
returns `ascii'.
If optional 2nd arg RESTRICTION is non-nil, it is a list of charsets
from which to find the charset.  It may also be a coding system.  In
that case, find the charset from what supported by that coding system.  */)
  (Lisp_Object ch, Lisp_Object restriction)
{
  struct charset *charset;

  CHECK_CHARACTER (ch);
  if (NILP (restriction))
    charset = CHAR_CHARSET (XFIXNUM (ch));
  else
    {
      if (CONSP (restriction))
	{
	  int c = XFIXNAT (ch);

	  for (; CONSP (restriction); restriction = XCDR (restriction))
	    {
	      struct charset *rcharset;

	      CHECK_CHARSET_GET_CHARSET (XCAR (restriction), rcharset);
	      if (ENCODE_CHAR (rcharset, c) != CHARSET_INVALID_CODE (rcharset))
		return XCAR (restriction);
	    }
	  return Qnil;
	}
      restriction = coding_system_charset_list (restriction);
      charset = char_charset (XFIXNUM (ch), restriction, NULL);
      if (! charset)
	return Qnil;
    }
  return (CHARSET_NAME (charset));
}


DEFUN ("charset-after", Fcharset_after, Scharset_after, 0, 1, 0,
       doc: /*
Return charset of a character in the current buffer at position POS.
If POS is nil, it defaults to the current point.
If POS is out of range, the value is nil.  */)
  (Lisp_Object pos)
{
  Lisp_Object ch;
  struct charset *charset;

  ch = Fchar_after (pos);
  if (! FIXNUMP (ch))
    return ch;
  charset = CHAR_CHARSET (XFIXNUM (ch));
  return (CHARSET_NAME (charset));
}


DEFUN ("iso-charset", Fiso_charset, Siso_charset, 3, 3, 0,
       doc: /*
Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR.

ISO 2022's designation sequence (escape sequence) distinguishes charsets
by their DIMENSION, CHARS, and FINAL-CHAR,
whereas Emacs distinguishes them by charset symbol.
See the documentation of the function `charset-info' for the meanings of
DIMENSION, CHARS, and FINAL-CHAR.  */)
  (Lisp_Object dimension, Lisp_Object chars, Lisp_Object final_char)
{
  bool chars_flag = check_iso_charset_parameter (dimension, chars, final_char);
  int id = ISO_CHARSET_TABLE (XFIXNUM (dimension), chars_flag,
			      XFIXNAT (final_char));
  return (id >= 0 ? CHARSET_NAME (CHARSET_FROM_ID (id)) : Qnil);
}


DEFUN ("clear-charset-maps", Fclear_charset_maps, Sclear_charset_maps,
       0, 0, 0,
       doc: /*
Internal use only.
Clear temporary charset mapping tables.
It should be called only from temacs invoked for dumping.  */)
  (void)
{
  if (temp_charset_work)
    {
      xfree (temp_charset_work);
      temp_charset_work = NULL;
    }

  if (CHAR_TABLE_P (Vchar_unify_table))
    Foptimize_char_table (Vchar_unify_table, Qnil);

  return Qnil;
}

DEFUN ("charset-priority-list", Fcharset_priority_list,
       Scharset_priority_list, 0, 1, 0,
       doc: /* Return the list of charsets ordered by priority.
HIGHESTP non-nil means just return the highest priority one.  */)
  (Lisp_Object highestp)
{
  Lisp_Object val = Qnil, list = Vcharset_ordered_list;

  if (!NILP (highestp))
    return CHARSET_NAME (CHARSET_FROM_ID (XFIXNUM (Fcar (list))));

  while (!NILP (list))
    {
      val = Fcons (CHARSET_NAME (CHARSET_FROM_ID (XFIXNUM (XCAR (list)))), val);
      list = XCDR (list);
    }
  return Fnreverse (val);
}

DEFUN ("set-charset-priority", Fset_charset_priority, Sset_charset_priority,
       1, MANY, 0,
       doc: /* Assign higher priority to the charsets given as arguments.
usage: (set-charset-priority &rest charsets)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object new_head, old_list;
  Lisp_Object list_2022, list_emacs_mule;
  ptrdiff_t i;
  int id;

  old_list = Fcopy_sequence (Vcharset_ordered_list);
  new_head = Qnil;
  for (i = 0; i < nargs; i++)
    {
      CHECK_CHARSET_GET_ID (args[i], id);
      if (! NILP (Fmemq (make_fixnum (id), old_list)))
	{
	  old_list = Fdelq (make_fixnum (id), old_list);
	  new_head = Fcons (make_fixnum (id), new_head);
	}
    }
  Vcharset_non_preferred_head = old_list;
  Vcharset_ordered_list = nconc2 (Fnreverse (new_head), old_list);

  charset_ordered_list_tick++;

  charset_unibyte = -1;
  for (old_list = Vcharset_ordered_list, list_2022 = list_emacs_mule = Qnil;
       CONSP (old_list); old_list = XCDR (old_list))
    {
      if (! NILP (Fmemq (XCAR (old_list), Viso_2022_charset_list)))
	list_2022 = Fcons (XCAR (old_list), list_2022);
      if (! NILP (Fmemq (XCAR (old_list), Vemacs_mule_charset_list)))
	list_emacs_mule = Fcons (XCAR (old_list), list_emacs_mule);
      if (charset_unibyte < 0)
	{
	  struct charset *charset = CHARSET_FROM_ID (XFIXNUM (XCAR (old_list)));

	  if (CHARSET_DIMENSION (charset) == 1
	      && CHARSET_ASCII_COMPATIBLE_P (charset)
	      && CHARSET_MAX_CHAR (charset) >= 0x80)
	    charset_unibyte = CHARSET_ID (charset);
	}
    }
  Viso_2022_charset_list = Fnreverse (list_2022);
  Vemacs_mule_charset_list = Fnreverse (list_emacs_mule);
  if (charset_unibyte < 0)
    charset_unibyte = charset_iso_8859_1;

  return Qnil;
}

DEFUN ("charset-id-internal", Fcharset_id_internal, Scharset_id_internal,
       0, 1, 0,
       doc: /* Internal use only.
Return charset identification number of CHARSET.  */)
  (Lisp_Object charset)
{
  int id;

  CHECK_CHARSET_GET_ID (charset, id);
  return make_fixnum (id);
}

struct charset_sort_data
{
  Lisp_Object charset;
  int id;
  ptrdiff_t priority;
};

static int
charset_compare (const void *d1, const void *d2)
{
  const struct charset_sort_data *data1 = d1, *data2 = d2;
  if (data1->priority != data2->priority)
    return data1->priority < data2->priority ? -1 : 1;
  return 0;
}

DEFUN ("sort-charsets", Fsort_charsets, Ssort_charsets, 1, 1, 0,
       doc: /* Sort charset list CHARSETS by a priority of each charset.
Return the sorted list.  CHARSETS is modified by side effects.
See also `charset-priority-list' and `set-charset-priority'.  */)
     (Lisp_Object charsets)
{
  ptrdiff_t n = list_length (charsets), i, j;
  int done;
  Lisp_Object tail, elt, attrs;
  struct charset_sort_data *sort_data;
  int id, min_id = INT_MAX, max_id = INT_MIN;
  USE_SAFE_ALLOCA;

  if (n == 0)
    return Qnil;
  SAFE_NALLOCA (sort_data, 1, n);
  for (tail = charsets, i = 0; CONSP (tail); tail = XCDR (tail), i++)
    {
      elt = XCAR (tail);
      CHECK_CHARSET_GET_ATTR (elt, attrs);
      sort_data[i].charset = elt;
      sort_data[i].id = id = XFIXNUM (CHARSET_ATTR_ID (attrs));
      if (id < min_id)
	min_id = id;
      if (id > max_id)
	max_id = id;
    }
  for (done = 0, tail = Vcharset_ordered_list, i = 0;
       done < n && CONSP (tail); tail = XCDR (tail), i++)
    {
      elt = XCAR (tail);
      id = XFIXNAT (elt);
      if (id >= min_id && id <= max_id)
	for (j = 0; j < n; j++)
	  if (sort_data[j].id == id)
	    {
	      sort_data[j].priority = i;
	      done++;
	    }
    }
  qsort (sort_data, n, sizeof *sort_data, charset_compare);
  for (i = 0, tail = charsets; CONSP (tail); tail = XCDR (tail), i++)
    XSETCAR (tail, sort_data[i].charset);
  SAFE_FREE ();
  return charsets;
}

#ifndef HAVE_MPS
/* Not strictly necessary, because all charset attributes are also
   reachable from `Vcharset_hash_table`.  */
void
mark_charset (void)
{
  for (int i = 0; i < charset_table_used; i++)
    mark_object (charset_table[i].attributes);
}
#endif


void
init_charset (void)
{
  Lisp_Object tempdir;
  tempdir = Fexpand_file_name (build_string ("charsets"), Vdata_directory);
  if (! file_accessible_directory_p (tempdir))
    {
      /* This used to be non-fatal (dir_warning), but it should not
         happen, and if it does sooner or later it will cause some
         obscure problem (eg bug#6401), so better exit.  */
      fprintf (stderr,
	       ("Error: %s: %s\n"
		"Emacs will not function correctly "
		"without the character map files.\n"
		"%s"
		"Please check your installation!\n"),
	       SDATA (tempdir), strerror (errno),
	       (egetenv ("EMACSDATA")
		? ("The EMACSDATA environment variable is set.  "
		   "Maybe it has the wrong value?\n")
		: ""));
      exit (1);
    }

  Vcharset_map_path = list1 (tempdir);
}


void
init_charset_once (void)
{
  int i, j, k;

  for (i = 0; i < ISO_MAX_DIMENSION; i++)
    for (j = 0; j < ISO_MAX_CHARS; j++)
      for (k = 0; k < ISO_MAX_FINAL; k++)
        iso_charset_table[i][j][k] = -1;

  PDUMPER_REMEMBER_SCALAR (iso_charset_table);

  for (i = 0; i < 256; i++)
    emacs_mule_charset[i] = -1;

  PDUMPER_REMEMBER_SCALAR (emacs_mule_charset);

  charset_jisx0201_roman = -1;
  PDUMPER_REMEMBER_SCALAR (charset_jisx0201_roman);

  charset_jisx0208_1978 = -1;
  PDUMPER_REMEMBER_SCALAR (charset_jisx0208_1978);

  charset_jisx0208 = -1;
  PDUMPER_REMEMBER_SCALAR (charset_jisx0208);

  charset_ksc5601 = -1;
  PDUMPER_REMEMBER_SCALAR (charset_ksc5601);
}

/* Allocate an initial charset table that is large enough to handle
   Emacs while it is bootstrapping.  As of September 2011, the size
   needs to be at least 166; make it a bit bigger to allow for future
   expansion.

   Don't make the value so small that the table is reallocated during
   bootstrapping, as glibc malloc calls larger than just under 64 KiB
   during an initial bootstrap wreak havoc after dumping; see the
   M_MMAP_THRESHOLD value in alloc.c, plus there is an extra overhead
   internal to glibc malloc and perhaps to Emacs malloc debugging.  */
struct charset charset_table_init[180];

void
syms_of_charset (void)
{
  DEFSYM (Qcharsetp, "charsetp");
  DEFSYM (Qdefine_charset_internal, "define-charset-internal");

  /* Special charset symbols.  */
  DEFSYM (Qascii, "ascii");
  DEFSYM (Qunicode, "unicode");
  DEFSYM (Qemacs, "emacs");
  DEFSYM (Qeight_bit, "eight-bit");
  DEFSYM (Qiso_8859_1, "iso-8859-1");

  staticpro (&Vcharset_ordered_list);
  Vcharset_ordered_list = Qnil;

#ifdef HAVE_MPS
  staticpro (&Vcharset_non_preferred_head);
  Vcharset_non_preferred_head = Qnil;
#endif

  staticpro (&Viso_2022_charset_list);
  Viso_2022_charset_list = Qnil;

  staticpro (&Vemacs_mule_charset_list);
  Vemacs_mule_charset_list = Qnil;

  staticpro (&Vcharset_hash_table);
  Vcharset_hash_table = CALLN (Fmake_hash_table, QCtest, Qeq);

  charset_table = charset_table_init;
  charset_table_size = ARRAYELTS (charset_table_init);
  PDUMPER_REMEMBER_SCALAR (charset_table_size);
  charset_table_used = 0;
  PDUMPER_REMEMBER_SCALAR (charset_table_used);

  defsubr (&Scharsetp);
  defsubr (&Smap_charset_chars);
  defsubr (&Sdefine_charset_internal);
  defsubr (&Sdefine_charset_alias);
  defsubr (&Scharset_plist);
  defsubr (&Sset_charset_plist);
  defsubr (&Sunify_charset);
  defsubr (&Sget_unused_iso_final_char);
  defsubr (&Sdeclare_equiv_charset);
  defsubr (&Sfind_charset_region);
  defsubr (&Sfind_charset_string);
  defsubr (&Sdecode_char);
  defsubr (&Sencode_char);
  defsubr (&Ssplit_char);
  defsubr (&Smake_char);
  defsubr (&Schar_charset);
  defsubr (&Scharset_after);
  defsubr (&Siso_charset);
  defsubr (&Sclear_charset_maps);
  defsubr (&Scharset_priority_list);
  defsubr (&Sset_charset_priority);
  defsubr (&Scharset_id_internal);
  defsubr (&Ssort_charsets);

  DEFVAR_LISP ("charset-map-path", Vcharset_map_path,
	       doc: /* List of directories to search for charset map files.  */);
  Vcharset_map_path = Qnil;

  DEFVAR_BOOL ("inhibit-load-charset-map", inhibit_load_charset_map,
	       doc: /* Inhibit loading of charset maps.  Used when dumping Emacs.  */);
  inhibit_load_charset_map = 0;

  DEFVAR_LISP ("charset-list", Vcharset_list,
	       doc: /* List of all charsets ever defined.  */);
  Vcharset_list = Qnil;

  DEFVAR_LISP ("current-iso639-language", Vcurrent_iso639_language,
	       doc: /* ISO639 language mnemonic symbol for the current language environment.
If the current language environment is for multiple languages (e.g. "Latin-1"),
the value may be a list of mnemonics.  */);
  Vcurrent_iso639_language = Qnil;

  charset_ascii
    = define_charset_internal (Qascii, 1, "\x00\x7F\0\0\0\0\0",
                               0, 127, 'B', -1, 0, 1, 0, 0);
  PDUMPER_REMEMBER_SCALAR (charset_ascii);

  charset_iso_8859_1
    = define_charset_internal (Qiso_8859_1, 1, "\x00\xFF\0\0\0\0\0",
                               0, 255, -1, -1, -1, 1, 0, 0);
  PDUMPER_REMEMBER_SCALAR (charset_iso_8859_1);

  charset_unicode
    = define_charset_internal (Qunicode, 3, "\x00\xFF\x00\xFF\x00\x10\0",
                               0, MAX_UNICODE_CHAR, -1, 0, -1, 1, 0, 0);
  PDUMPER_REMEMBER_SCALAR (charset_unicode);

  charset_emacs
    = define_charset_internal (Qemacs, 3, "\x00\xFF\x00\xFF\x00\x3F\0",
                               0, MAX_5_BYTE_CHAR, -1, 0, -1, 1, 1, 0);
  PDUMPER_REMEMBER_SCALAR (charset_emacs);

  charset_eight_bit
    = define_charset_internal (Qeight_bit, 1, "\x80\xFF\0\0\0\0\0",
			       128, 255, -1, 0, -1, 0, 1,
                               MAX_5_BYTE_CHAR + 1);
  PDUMPER_REMEMBER_SCALAR (charset_eight_bit);

  charset_unibyte = charset_iso_8859_1;
  PDUMPER_REMEMBER_SCALAR (charset_unibyte);
}
