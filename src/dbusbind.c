/* Elisp bindings for D-Bus.
   Copyright (C) 2007-2025 Free Software Foundation, Inc.

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

#ifdef HAVE_DBUS
#include <stdio.h>
#include <stdlib.h>
#include <dbus/dbus.h>

#include "lisp.h"
#include "termhooks.h"
#include "keyboard.h"
#include "pdumper.h"
#include "process.h"
#include "igc.h"

#ifndef DBUS_NUM_MESSAGE_TYPES
#define DBUS_NUM_MESSAGE_TYPES 5
#endif


/* Some platforms define the symbol "interface", but we want to use it
 * as a variable name below.  */

#ifdef interface
#undef interface
#endif


/* Alist of D-Bus buses we are polling for messages.
   The key is the symbol or string of the bus, and the value is the
   connection address.  For every bus, just one connection is counted.
   If there shall be a second connection to the same bus, a different
   symbol or string for the bus must be chosen.  On Lisp level, a bus
   stands for the associated connection.  */
static Lisp_Object xd_registered_buses;

/* Whether we are reading a D-Bus event.  */
static bool xd_in_read_queued_messages = 0;


/* We use "xd_" and "XD_" as prefix for all internal symbols, because
   we don't want to poison other namespaces with "dbus_".  */

/* Raise a signal.  If we are reading events, we cannot signal; we
   throw to xd_read_queued_messages then.  */
#define XD_SIGNAL1(arg)							\
  do {									\
    if (xd_in_read_queued_messages)					\
      Fthrow (Qdbus_error, Qnil);					\
    else								\
      xsignal1 (Qdbus_error, arg);					\
  } while (0)

#define XD_SIGNAL2(arg1, arg2)						\
  do {									\
    if (xd_in_read_queued_messages)					\
      Fthrow (Qdbus_error, Qnil);					\
    else								\
      xsignal2 (Qdbus_error, arg1, arg2);				\
  } while (0)

#define XD_SIGNAL3(arg1, arg2, arg3)					\
  do {									\
    if (xd_in_read_queued_messages)					\
      Fthrow (Qdbus_error, Qnil);					\
    else								\
      xsignal3 (Qdbus_error, arg1, arg2, arg3);				\
  } while (0)

/* Raise a Lisp error from a D-Bus ERROR.  */
#define XD_ERROR(error)							\
  do {									\
    /* Remove the trailing newline.  */					\
    char const *mess = error.message;					\
    char const *nl = strchr (mess, '\n');				\
    Lisp_Object err = make_string (mess, nl ? nl - mess : strlen (mess)); \
    dbus_error_free (&error);						\
    XD_SIGNAL1 (err);							\
  } while (0)

/* Macros for debugging.  In order to enable them, build with
   "make MYCPPFLAGS='-DDBUS_DEBUG'".  */
#ifdef DBUS_DEBUG
#define XD_DEBUG_MESSAGE(...)						\
  do {									\
    char s[1024];							\
    snprintf (s, sizeof s, __VA_ARGS__);				\
    if (!noninteractive)						\
      printf ("%s: %s\n", __func__, s);					\
    message ("%s: %s", __func__, s);					\
  } while (0)
#define XD_DEBUG_VALID_LISP_OBJECT_P(object)				\
  do {									\
    if (!valid_lisp_object_p (object))					\
      {									\
	XD_DEBUG_MESSAGE ("%d Assertion failure", __LINE__);		\
	XD_SIGNAL1 (build_string ("Assertion failure"));		\
      }									\
  } while (0)

#else /* !DBUS_DEBUG */
# define XD_DEBUG_MESSAGE(...)						\
  do {									\
    if (!NILP (Vdbus_debug))						\
      {									\
	char s[1024];							\
	snprintf (s, sizeof s, __VA_ARGS__);				\
	message ("%s: %s", __func__, s);				\
      }									\
  } while (0)
# define XD_DEBUG_VALID_LISP_OBJECT_P(object)
#endif

/* Check whether TYPE is a basic DBusType.  */
#ifdef HAVE_DBUS_TYPE_IS_VALID
#define XD_BASIC_DBUS_TYPE(type)					\
  (dbus_type_is_valid (type) && dbus_type_is_basic (type))
#else
#ifdef DBUS_TYPE_UNIX_FD
#define XD_BASIC_DBUS_TYPE(type)					\
  ((type ==  DBUS_TYPE_BYTE)						\
   || (type ==  DBUS_TYPE_BOOLEAN)					\
   || (type ==  DBUS_TYPE_INT16)					\
   || (type ==  DBUS_TYPE_UINT16)					\
   || (type ==  DBUS_TYPE_INT32)					\
   || (type ==  DBUS_TYPE_UINT32)					\
   || (type ==  DBUS_TYPE_INT64)					\
   || (type ==  DBUS_TYPE_UINT64)					\
   || (type ==  DBUS_TYPE_DOUBLE)					\
   || (type ==  DBUS_TYPE_STRING)					\
   || (type ==  DBUS_TYPE_OBJECT_PATH)					\
   || (type ==  DBUS_TYPE_SIGNATURE)					\
   || (type ==  DBUS_TYPE_UNIX_FD))
#else
#define XD_BASIC_DBUS_TYPE(type)					\
  ((type ==  DBUS_TYPE_BYTE)						\
   || (type ==  DBUS_TYPE_BOOLEAN)					\
   || (type ==  DBUS_TYPE_INT16)					\
   || (type ==  DBUS_TYPE_UINT16)					\
   || (type ==  DBUS_TYPE_INT32)					\
   || (type ==  DBUS_TYPE_UINT32)					\
   || (type ==  DBUS_TYPE_INT64)					\
   || (type ==  DBUS_TYPE_UINT64)					\
   || (type ==  DBUS_TYPE_DOUBLE)					\
   || (type ==  DBUS_TYPE_STRING)					\
   || (type ==  DBUS_TYPE_OBJECT_PATH)					\
   || (type ==  DBUS_TYPE_SIGNATURE))
#endif
#endif

/* This was a macro.  On Solaris 2.11 it was said to compile for
   hours, when optimization is enabled.  So we have transferred it into
   a function.  */
/* Determine the DBusType of a given Lisp symbol.  OBJECT must be one
   of the predefined D-Bus type symbols.  */
static int
xd_symbol_to_dbus_type (Lisp_Object object)
{
  return
    (EQ (object, QCbyte) ? DBUS_TYPE_BYTE
     : EQ (object, QCboolean) ? DBUS_TYPE_BOOLEAN
     : EQ (object, QCint16) ? DBUS_TYPE_INT16
     : EQ (object, QCuint16) ? DBUS_TYPE_UINT16
     : EQ (object, QCint32) ? DBUS_TYPE_INT32
     : EQ (object, QCuint32) ? DBUS_TYPE_UINT32
     : EQ (object, QCint64) ? DBUS_TYPE_INT64
     : EQ (object, QCuint64) ? DBUS_TYPE_UINT64
     : EQ (object, QCdouble) ? DBUS_TYPE_DOUBLE
     : EQ (object, QCstring) ? DBUS_TYPE_STRING
     : EQ (object, QCobject_path) ? DBUS_TYPE_OBJECT_PATH
     : EQ (object, QCsignature) ? DBUS_TYPE_SIGNATURE
#ifdef DBUS_TYPE_UNIX_FD
     : EQ (object, QCunix_fd) ? DBUS_TYPE_UNIX_FD
#endif
     : EQ (object, QCarray) ? DBUS_TYPE_ARRAY
     : EQ (object, QCvariant) ? DBUS_TYPE_VARIANT
     : EQ (object, QCstruct) ? DBUS_TYPE_STRUCT
     : EQ (object, QCdict_entry) ? DBUS_TYPE_DICT_ENTRY
     : DBUS_TYPE_INVALID);
}

/* Determine the Lisp symbol of DBusType.  */
static Lisp_Object
xd_dbus_type_to_symbol (int type)
{
  return
    (type == DBUS_TYPE_BYTE) ? QCbyte
    : (type == DBUS_TYPE_BOOLEAN) ? QCboolean
    : (type == DBUS_TYPE_INT16) ? QCint16
    : (type == DBUS_TYPE_UINT16) ? QCuint16
    : (type == DBUS_TYPE_INT32) ? QCint32
    : (type == DBUS_TYPE_UINT32) ? QCuint32
    : (type == DBUS_TYPE_INT64) ? QCint64
    : (type == DBUS_TYPE_UINT64) ? QCuint64
    : (type == DBUS_TYPE_DOUBLE) ? QCdouble
    : (type == DBUS_TYPE_STRING) ? QCstring
    : (type == DBUS_TYPE_OBJECT_PATH) ? QCobject_path
    : (type == DBUS_TYPE_SIGNATURE) ? QCsignature
#ifdef DBUS_TYPE_UNIX_FD
    : (type == DBUS_TYPE_UNIX_FD) ? QCunix_fd
#endif
    : (type == DBUS_TYPE_ARRAY) ? QCarray
    : (type == DBUS_TYPE_VARIANT) ? QCvariant
    : (type == DBUS_TYPE_STRUCT) ? QCstruct
    : (type ==  DBUS_TYPE_DICT_ENTRY) ? QCdict_entry
    : Qnil;
}

#define XD_KEYWORDP(object) !NILP (Fkeywordp (object))

/* Check whether a Lisp symbol is a predefined D-Bus type symbol.  */
#define XD_DBUS_TYPE_P(object)						\
  XD_KEYWORDP (object) &&						\
    ((xd_symbol_to_dbus_type (object) != DBUS_TYPE_INVALID))

/* Determine the DBusType of a given Lisp OBJECT.  It is used to
   convert Lisp objects, being arguments of `dbus-call-method' or
   `dbus-send-signal', into corresponding C values appended as
   arguments to a D-Bus message.  */
#define XD_OBJECT_TO_DBUS_TYPE(object)					\
  ((EQ (object, Qt) || NILP (object)) ? DBUS_TYPE_BOOLEAN		\
   : (FIXNATP (object)) ? DBUS_TYPE_UINT32				\
   : (FIXNUMP (object)) ? DBUS_TYPE_INT32				\
   : (FLOATP (object)) ? DBUS_TYPE_DOUBLE				\
   : (STRINGP (object)) ? DBUS_TYPE_STRING				\
   : (XD_DBUS_TYPE_P (object)) ? xd_symbol_to_dbus_type (object)	\
   : (CONSP (object))							\
   ? ((XD_DBUS_TYPE_P (XCAR (object)))					\
      ? ((XD_BASIC_DBUS_TYPE (xd_symbol_to_dbus_type (XCAR (object))))	\
	 ? DBUS_TYPE_ARRAY						\
	 : xd_symbol_to_dbus_type (XCAR (object)))			\
      : DBUS_TYPE_ARRAY)						\
   : DBUS_TYPE_INVALID)

/* Return a list pointer which does not have a Lisp symbol as car.  */
#define XD_NEXT_VALUE(object)						\
  ((XD_DBUS_TYPE_P (CAR_SAFE (object))) ? CDR_SAFE (object) : object)

/* Transform the message type to its string representation for debug
   messages.  */
#define XD_MESSAGE_TYPE_TO_STRING(mtype)				\
  ((mtype == DBUS_MESSAGE_TYPE_INVALID)					\
  ? "DBUS_MESSAGE_TYPE_INVALID"						\
  : (mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)				\
  ? "DBUS_MESSAGE_TYPE_METHOD_CALL"					\
  : (mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)				\
  ? "DBUS_MESSAGE_TYPE_METHOD_RETURN"					\
  : (mtype == DBUS_MESSAGE_TYPE_ERROR)					\
   ? "DBUS_MESSAGE_TYPE_ERROR"						\
   : "DBUS_MESSAGE_TYPE_SIGNAL")

/* Transform the object to its string representation for debug
   messages.  */
static char *
XD_OBJECT_TO_STRING (Lisp_Object object)
{
  AUTO_STRING (format, "%s");
  return SSDATA (CALLN (Fformat, format, object));
}

#define XD_DBUS_VALIDATE_BUS_ADDRESS(bus)				\
  do {									\
    char const *session_bus_address = egetenv ("DBUS_SESSION_BUS_ADDRESS"); \
    if (STRINGP (bus))							\
      {									\
	DBusAddressEntry **entries;					\
	int len;							\
	DBusError derror;						\
	dbus_error_init (&derror);					\
	if (!dbus_parse_address (SSDATA (bus), &entries, &len, &derror)) \
	  XD_ERROR (derror);						\
	/* Cleanup.  */							\
	dbus_error_free (&derror);					\
	dbus_address_entries_free (entries);				\
	/* Canonicalize session bus address.  */			\
	if ((session_bus_address != NULL)				\
	    && (!NILP (Fstring_equal					\
		       (bus, build_string (session_bus_address)))))	\
	  bus = QCsession;						\
      }									\
									\
    else								\
      {									\
	CHECK_SYMBOL (bus);						\
	if (!(EQ (bus, QCsystem) || EQ (bus, QCsession)			\
	      || EQ (bus, QCsystem_private)				\
	      || EQ (bus, QCsession_private)))				\
	  XD_SIGNAL2 (build_string ("Wrong bus name"), bus);		\
	/* We do not want to have an autolaunch for the session bus.  */ \
	if ((EQ (bus, QCsession) || EQ (bus, QCsession_private))	\
	    && session_bus_address == NULL)				\
	  XD_SIGNAL2 (build_string ("No connection to bus"), bus);	\
      }									\
  } while (0)

#if (HAVE_DBUS_VALIDATE_BUS_NAME || HAVE_DBUS_VALIDATE_PATH		\
     || HAVE_DBUS_VALIDATE_INTERFACE || HAVE_DBUS_VALIDATE_MEMBER)
#define XD_DBUS_VALIDATE_OBJECT(object, func)				\
  do {									\
    if (!NILP (object))							\
      {									\
	DBusError derror;						\
	CHECK_STRING (object);						\
	dbus_error_init (&derror);					\
	if (!func (SSDATA (object), &derror))				\
	  XD_ERROR (derror);						\
	/* Cleanup.  */							\
	dbus_error_free (&derror);					\
      }									\
  } while (0)
#endif

#if HAVE_DBUS_VALIDATE_BUS_NAME
#define XD_DBUS_VALIDATE_BUS_NAME(bus_name)				\
  XD_DBUS_VALIDATE_OBJECT(bus_name, dbus_validate_bus_name);
#else
#define XD_DBUS_VALIDATE_BUS_NAME(bus_name)				\
  if (!NILP (bus_name)) CHECK_STRING (bus_name);
#endif

#if HAVE_DBUS_VALIDATE_PATH
#define XD_DBUS_VALIDATE_PATH(path)					\
  XD_DBUS_VALIDATE_OBJECT(path, dbus_validate_path);
#else
#define XD_DBUS_VALIDATE_PATH(path)					\
  if (!NILP (path)) CHECK_STRING (path);
#endif

#if HAVE_DBUS_VALIDATE_INTERFACE
#define XD_DBUS_VALIDATE_INTERFACE(interface)				\
  XD_DBUS_VALIDATE_OBJECT(interface, dbus_validate_interface);
#else
#define XD_DBUS_VALIDATE_INTERFACE(interface)				\
  if (!NILP (interface)) CHECK_STRING (interface);
#endif

#if HAVE_DBUS_VALIDATE_MEMBER
#define XD_DBUS_VALIDATE_MEMBER(member)					\
  XD_DBUS_VALIDATE_OBJECT(member, dbus_validate_member);
#else
#define XD_DBUS_VALIDATE_MEMBER(member)					\
  if (!NILP (member)) CHECK_STRING (member);
#endif

/* Append to SIGNATURE a copy of X, making sure SIGNATURE does
   not become too long.  */
static void
xd_signature_cat (char *signature, char const *x)
{
  ptrdiff_t siglen = strlen (signature);
  ptrdiff_t xlen = strlen (x);
  if (DBUS_MAXIMUM_SIGNATURE_LENGTH - xlen <= siglen)
    string_overflow ();
  strcpy (signature + siglen, x);
}

/* Compute SIGNATURE of OBJECT.  It must have a form that it can be
   used in dbus_message_iter_open_container.  DTYPE is the DBusType
   the object is related to.  It is passed as argument, because it
   cannot be detected in basic type objects, when they are preceded by
   a type symbol.  PARENT_TYPE is the DBusType of a container this
   signature is embedded, or DBUS_TYPE_INVALID.  It is needed for the
   check that DBUS_TYPE_DICT_ENTRY occurs only as array element.  */
static void
xd_signature (char *signature, int dtype, int parent_type, Lisp_Object object)
{
  int subtype;
  Lisp_Object elt;
  char const *subsig;
  char x[DBUS_MAXIMUM_SIGNATURE_LENGTH];

  elt = object;

  switch (dtype)
    {
    case DBUS_TYPE_BYTE:
    case DBUS_TYPE_UINT16:
      CHECK_FIXNAT (object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_BOOLEAN:
      /* There must be an argument.  */
      if (EQ (QCboolean, object))
	wrong_type_argument (Qbooleanp, object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_INT16:
      CHECK_FIXNUM (object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_UINT32:
    case DBUS_TYPE_UINT64:
#ifdef DBUS_TYPE_UNIX_FD
    case DBUS_TYPE_UNIX_FD:
#endif
    case DBUS_TYPE_INT32:
    case DBUS_TYPE_INT64:
    case DBUS_TYPE_DOUBLE:
      CHECK_NUMBER (object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_STRING:
    case DBUS_TYPE_OBJECT_PATH:
    case DBUS_TYPE_SIGNATURE:
      /* We don't check the syntax of signature.  This will be done by
	 libdbus.  */
      if (dtype == DBUS_TYPE_OBJECT_PATH)
	XD_DBUS_VALIDATE_PATH (object)
      else
	CHECK_STRING (object);
      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_ARRAY:
      /* Check that all list elements have the same D-Bus type.  For
	 complex element types, we just check the container type, not
	 the whole element's signature.  */
      CHECK_CONS (object);

      /* Type symbol is optional.  */
      if (EQ (QCarray, XCAR (elt)))
	elt = XD_NEXT_VALUE (elt);

      /* If the array is empty, DBUS_TYPE_STRING is the default
	 element type.  */
      if (NILP (elt))
	{
	  subtype = DBUS_TYPE_STRING;
	  subsig = DBUS_TYPE_STRING_AS_STRING;
	}
      else
	{
	  subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
	  xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
	  subsig = x;
	}

      /* If the element type is DBUS_TYPE_SIGNATURE, and this is the
	 only element, the value of this element is used as the
	 array's element signature.  */
      if (subtype == DBUS_TYPE_SIGNATURE)
	{
	  Lisp_Object elt1 = XD_NEXT_VALUE (elt);
	  if (CONSP (elt1) && STRINGP (XCAR (elt1)) && NILP (XCDR (elt1)))
	    {
	      subsig = SSDATA (XCAR (elt1));
	      elt = Qnil;
	    }
	}

      while (!NILP (elt))
	{
	  char x[DBUS_MAXIMUM_SIGNATURE_LENGTH];
	  subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
	  xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
	  if (strcmp (subsig, x) != 0)
	    wrong_type_argument (QD_Bus, CAR_SAFE (elt));
	  elt = CDR_SAFE (XD_NEXT_VALUE (elt));
	}

      signature[0] = dtype;
      signature[1] = '\0';
      xd_signature_cat (signature, subsig);
      break;

    case DBUS_TYPE_VARIANT:
      /* Check that there is exactly one list element.  */
      CHECK_CONS (object);

      elt = XD_NEXT_VALUE (elt);
      CHECK_CONS (elt);
      subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
      xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));

      if (!NILP (CDR_SAFE (XD_NEXT_VALUE (elt))))
	wrong_type_argument (QD_Bus, CAR_SAFE (CDR_SAFE (XD_NEXT_VALUE (elt))));

      sprintf (signature, "%c", dtype);
      break;

    case DBUS_TYPE_STRUCT:
      /* A struct list might contain any (but zero) number of elements
	 with different types.  No further check needed.  */
      CHECK_CONS (object);

      elt = XD_NEXT_VALUE (elt);
      CHECK_CONS (elt);

      /* Compose the signature from the elements.  It is enclosed by
	 parentheses.  */
      sprintf (signature, "%c", DBUS_STRUCT_BEGIN_CHAR );
      while (!NILP (elt))
	{
	  subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
	  xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
	  xd_signature_cat (signature, x);
	  elt = CDR_SAFE (XD_NEXT_VALUE (elt));
	}
      xd_signature_cat (signature, DBUS_STRUCT_END_CHAR_AS_STRING);
      break;

    case DBUS_TYPE_DICT_ENTRY:
      /* Check that there are exactly two list elements, and the first
	 one is of basic type.  The dictionary entry itself must be an
	 element of an array.  */
      CHECK_CONS (object);

      /* Check the parent object type.  */
      if (parent_type != DBUS_TYPE_ARRAY)
	wrong_type_argument (QD_Bus, object);

      /* Compose the signature from the elements.  It is enclosed by
	 curly braces.  */
      sprintf (signature, "%c", DBUS_DICT_ENTRY_BEGIN_CHAR);

      /* First element.  */
      elt = XD_NEXT_VALUE (elt);
      CHECK_CONS (elt);
      subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
      xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
      xd_signature_cat (signature, x);

      if (!XD_BASIC_DBUS_TYPE (subtype))
	wrong_type_argument (QD_Bus, CAR_SAFE (XD_NEXT_VALUE (elt)));

      /* Second element.  */
      elt = CDR_SAFE (XD_NEXT_VALUE (elt));
      CHECK_CONS (elt);
      subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
      xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
      xd_signature_cat (signature, x);

      if (!NILP (CDR_SAFE (XD_NEXT_VALUE (elt))))
	wrong_type_argument (QD_Bus, CAR_SAFE (CDR_SAFE (XD_NEXT_VALUE (elt))));

      /* Closing signature.  */
      xd_signature_cat (signature, DBUS_DICT_ENTRY_END_CHAR_AS_STRING);
      break;

    default:
      wrong_type_argument (QD_Bus, object);
    }

  XD_DEBUG_MESSAGE ("%s", signature);
}

/* Convert X to a signed integer with bounds LO and HI.  */
static intmax_t
xd_extract_signed (Lisp_Object x, intmax_t lo, intmax_t hi)
{
  CHECK_NUMBER (x);
  if (INTEGERP (x))
    {
      intmax_t i;
      if (integer_to_intmax (x, &i) && lo <= i && i <= hi)
	return i;
    }
  else
    {
      double d = XFLOAT_DATA (x);
      if (lo <= d && d < 1.0 + hi)
	{
	  intmax_t n = d;
	  if (n == d)
	    return n;
	}
    }

  if (xd_in_read_queued_messages)
    Fthrow (Qdbus_error, Qnil);
  else
    args_out_of_range_3 (x, INT_TO_INTEGER (lo), INT_TO_INTEGER (hi));
}

/* Convert X to an unsigned integer with bounds 0 and HI.  */
static uintmax_t
xd_extract_unsigned (Lisp_Object x, uintmax_t hi)
{
  CHECK_NUMBER (x);
  if (INTEGERP (x))
    {
      uintmax_t i;
      if (integer_to_uintmax (x, &i) && i <= hi)
	return i;
    }
  else
    {
      double d = XFLOAT_DATA (x);
      if (0 <= d && d < 1.0 + hi)
	{
	  uintmax_t n = d;
	  if (n == d)
	    return n;
	}
    }

  if (xd_in_read_queued_messages)
    Fthrow (Qdbus_error, Qnil);
  else
    args_out_of_range_3 (x, make_fixnum (0), INT_TO_INTEGER (hi));
}

/* Append C value, extracted from Lisp OBJECT, to iteration ITER.
   DTYPE must be a valid DBusType.  It is used to convert Lisp
   objects, being arguments of `dbus-call-method' or
   `dbus-send-signal', into corresponding C values appended as
   arguments to a D-Bus message.  */
static void
xd_append_arg (int dtype, Lisp_Object object, DBusMessageIter *iter)
{
  char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];
  DBusMessageIter subiter;

  if (XD_BASIC_DBUS_TYPE (dtype))
    switch (dtype)
      {
      case DBUS_TYPE_BYTE:
	CHECK_FIXNAT (object);
	{
	  unsigned char val = XFIXNAT (object) & 0xFF;
	  XD_DEBUG_MESSAGE ("%c %u", dtype, val);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_BOOLEAN:
	/* There must be an argument.  */
	if (EQ (QCboolean, object))
	  wrong_type_argument (Qbooleanp, object);
	{
	  dbus_bool_t val = (NILP (object)) ? FALSE : TRUE;
	  XD_DEBUG_MESSAGE ("%c %s", dtype, (val == FALSE) ? "false" : "true");
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_INT16:
	{
	  dbus_int16_t val =
	    xd_extract_signed (object,
			       TYPE_MINIMUM (dbus_int16_t),
			       TYPE_MAXIMUM (dbus_int16_t));
	  int pval = val;
	  XD_DEBUG_MESSAGE ("%c %d", dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_UINT16:
	{
	  dbus_uint16_t val =
	    xd_extract_unsigned (object,
				 TYPE_MAXIMUM (dbus_uint16_t));
	  unsigned int pval = val;
	  XD_DEBUG_MESSAGE ("%c %u", dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_INT32:
	{
	  dbus_int32_t val =
	    xd_extract_signed (object,
			       TYPE_MINIMUM (dbus_int32_t),
			       TYPE_MAXIMUM (dbus_int32_t));
	  int pval = val;
	  XD_DEBUG_MESSAGE ("%c %d", dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_UINT32:
#ifdef DBUS_TYPE_UNIX_FD
      case DBUS_TYPE_UNIX_FD:
#endif
	{
	  dbus_uint32_t val =
	    xd_extract_unsigned (object,
				 TYPE_MAXIMUM (dbus_uint32_t));
	  unsigned int pval = val;
	  XD_DEBUG_MESSAGE ("%c %u", dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_INT64:
	{
	  dbus_int64_t val =
	    xd_extract_signed (object,
			       TYPE_MINIMUM (dbus_int64_t),
			       TYPE_MAXIMUM (dbus_int64_t));
	  intmax_t pval = val;
	  XD_DEBUG_MESSAGE ("%c %"PRIdMAX, dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_UINT64:
	{
	  dbus_uint64_t val =
	    xd_extract_unsigned (object,
				 TYPE_MAXIMUM (dbus_uint64_t));
	  uintmax_t pval = val;
	  XD_DEBUG_MESSAGE ("%c %"PRIuMAX, dtype, pval);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_DOUBLE:
	{
	  double val = extract_float (object);
	  XD_DEBUG_MESSAGE ("%c %f", dtype, val);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}

      case DBUS_TYPE_STRING:
      case DBUS_TYPE_OBJECT_PATH:
      case DBUS_TYPE_SIGNATURE:
	/* We don't check the syntax of signature.  This will be done
	   by libdbus.  */
	if (dtype == DBUS_TYPE_OBJECT_PATH)
	  XD_DBUS_VALIDATE_PATH (object)
	else
	  CHECK_STRING (object);
	{
	  /* We need to send a valid UTF-8 string.  We could encode `object'
	     but by not encoding it, we guarantee it's valid utf-8, even if
	     it contains eight-bit-bytes.  Of course, you can still send
	     manually-crafted junk by passing a unibyte string.  */
	  char *val = SSDATA (object);
	  XD_DEBUG_MESSAGE ("%c %s", dtype, val);
	  if (!dbus_message_iter_append_basic (iter, dtype, &val))
	    XD_SIGNAL2 (build_string ("Unable to append argument"), object);
	  return;
	}
      }

  else /* Compound types.  */
    {

      /* All compound types except array have a type symbol.  For
	 array, it is optional.  Skip it.  */
      if (!XD_BASIC_DBUS_TYPE (XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object))))
	object = XD_NEXT_VALUE (object);

      /* Open new subiteration.  */
      switch (dtype)
	{
	case DBUS_TYPE_ARRAY:
	  /* An array has only elements of the same type.  So it is
	     sufficient to check the first element's signature
	     only.  */

	  if (NILP (object))
	    /* If the array is empty, DBUS_TYPE_STRING is the default
	       element type.  */
	    strcpy (signature, DBUS_TYPE_STRING_AS_STRING);

	  else
	    {
	      /* If the element type is DBUS_TYPE_SIGNATURE, and this is
		 the only element, the value of this element is used as
		 the array's element signature.  */
	      if (CONSP (object) && (XD_OBJECT_TO_DBUS_TYPE (XCAR (object))
				     == DBUS_TYPE_SIGNATURE))
		{
		  Lisp_Object val = XD_NEXT_VALUE (object);
		  if (CONSP (val) && STRINGP (XCAR (val)) && NILP (XCDR (val))
		      && SBYTES (XCAR (val)) < DBUS_MAXIMUM_SIGNATURE_LENGTH)
		    {
		      lispstpcpy (signature, XCAR (val));
		      object = Qnil;
		    }
		}

	      if (!NILP (object))
		xd_signature (signature,
			      XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object)),
			      dtype, CAR_SAFE (XD_NEXT_VALUE (object)));
	    }

	  XD_DEBUG_MESSAGE ("%c %s %s", dtype, signature,
			    XD_OBJECT_TO_STRING (object));
	  if (!dbus_message_iter_open_container (iter, dtype,
						 signature, &subiter))
	    XD_SIGNAL3 (build_string ("Cannot open container"),
			make_fixnum (dtype), build_string (signature));
	  break;

	case DBUS_TYPE_VARIANT:
	  /* A variant has just one element.  */
	  xd_signature (signature, XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object)),
			dtype, CAR_SAFE (XD_NEXT_VALUE (object)));

	  XD_DEBUG_MESSAGE ("%c %s %s", dtype, signature,
			    XD_OBJECT_TO_STRING (object));
	  if (!dbus_message_iter_open_container (iter, dtype,
						 signature, &subiter))
	    XD_SIGNAL3 (build_string ("Cannot open container"),
			make_fixnum (dtype), build_string (signature));
	  break;

	case DBUS_TYPE_STRUCT:
	case DBUS_TYPE_DICT_ENTRY:
	  /* These containers do not require a signature.  */
	  XD_DEBUG_MESSAGE ("%c %s", dtype, XD_OBJECT_TO_STRING (object));
	  if (!dbus_message_iter_open_container (iter, dtype, NULL, &subiter))
	    XD_SIGNAL2 (build_string ("Cannot open container"),
			make_fixnum (dtype));
	  break;
	}

      /* Loop over list elements.  */
      while (!NILP (object))
	{
	  dtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object));
	  object = XD_NEXT_VALUE (object);

	  xd_append_arg (dtype, CAR_SAFE (object), &subiter);

	  object = CDR_SAFE (object);
	}

      /* Close the subiteration.  */
      if (!dbus_message_iter_close_container (iter, &subiter))
	XD_SIGNAL2 (build_string ("Cannot close container"),
		    make_fixnum (dtype));
    }
}

/* Retrieve C value from a DBusMessageIter structure ITER, and return
   a converted Lisp object.  The type DTYPE of the argument of the
   D-Bus message must be a valid DBusType.  Compound D-Bus types
   result always in a Lisp list.  */
static Lisp_Object
xd_retrieve_arg (int dtype, DBusMessageIter *iter)
{

  switch (dtype)
    {
    case DBUS_TYPE_BYTE:
      {
	unsigned int val;
	dbus_message_iter_get_basic (iter, &val);
	val = val & 0xFF;
	XD_DEBUG_MESSAGE ("%c %u", dtype, val);
	return list2 (xd_dbus_type_to_symbol (dtype), make_fixnum (val));
      }

    case DBUS_TYPE_BOOLEAN:
      {
	dbus_bool_t val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%c %s", dtype, (val == FALSE) ? "false" : "true");
	return list2 (xd_dbus_type_to_symbol (dtype),
		      (val == FALSE) ? Qnil : Qt);
      }

    case DBUS_TYPE_INT16:
      {
	dbus_int16_t val;
	int pval;
	dbus_message_iter_get_basic (iter, &val);
	pval = val;
	XD_DEBUG_MESSAGE ("%c %d", dtype, pval);
	return list2 (xd_dbus_type_to_symbol (dtype), make_fixnum (val));
      }

    case DBUS_TYPE_UINT16:
      {
	dbus_uint16_t val;
	int pval;
	dbus_message_iter_get_basic (iter, &val);
	pval = val;
	XD_DEBUG_MESSAGE ("%c %d", dtype, pval);
	return list2 (xd_dbus_type_to_symbol (dtype), make_fixnum (val));
      }

    case DBUS_TYPE_INT32:
      {
	dbus_int32_t val;
	int pval;
	dbus_message_iter_get_basic (iter, &val);
	pval = val;
	XD_DEBUG_MESSAGE ("%c %d", dtype, pval);
	return list2 (xd_dbus_type_to_symbol (dtype), INT_TO_INTEGER (val));
      }

    case DBUS_TYPE_UINT32:
#ifdef DBUS_TYPE_UNIX_FD
    case DBUS_TYPE_UNIX_FD:
#endif
      {
	dbus_uint32_t val;
	unsigned int pval;
	dbus_message_iter_get_basic (iter, &val);
	pval = val;
	XD_DEBUG_MESSAGE ("%c %u", dtype, pval);
	return list2 (xd_dbus_type_to_symbol (dtype), INT_TO_INTEGER (val));
      }

    case DBUS_TYPE_INT64:
      {
	dbus_int64_t val;
	dbus_message_iter_get_basic (iter, &val);
	intmax_t pval = val;
	XD_DEBUG_MESSAGE ("%c %"PRIdMAX, dtype, pval);
	return list2 (xd_dbus_type_to_symbol (dtype), INT_TO_INTEGER (val));
      }

    case DBUS_TYPE_UINT64:
      {
	dbus_uint64_t val;
	dbus_message_iter_get_basic (iter, &val);
	uintmax_t pval = val;
	XD_DEBUG_MESSAGE ("%c %"PRIuMAX, dtype, pval);
	return list2 (xd_dbus_type_to_symbol (dtype), INT_TO_INTEGER (val));
      }

    case DBUS_TYPE_DOUBLE:
      {
	double val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%c %f", dtype, val);
	return list2 (xd_dbus_type_to_symbol (dtype), make_float (val));
      }

    case DBUS_TYPE_STRING:
    case DBUS_TYPE_OBJECT_PATH:
    case DBUS_TYPE_SIGNATURE:
      {
	char *val;
	dbus_message_iter_get_basic (iter, &val);
	XD_DEBUG_MESSAGE ("%c %s", dtype, val);
	return list2 (xd_dbus_type_to_symbol (dtype), build_string (val));
      }

    case DBUS_TYPE_ARRAY:
    case DBUS_TYPE_VARIANT:
    case DBUS_TYPE_STRUCT:
    case DBUS_TYPE_DICT_ENTRY:
      {
	Lisp_Object result;
	DBusMessageIter subiter;
	int subtype;
	result = Qnil;
	dbus_message_iter_recurse (iter, &subiter);
	while ((subtype = dbus_message_iter_get_arg_type (&subiter))
	       != DBUS_TYPE_INVALID)
	  {
	    result = Fcons (xd_retrieve_arg (subtype, &subiter), result);
	    dbus_message_iter_next (&subiter);
	  }
	XD_DEBUG_MESSAGE ("%c %s", dtype, XD_OBJECT_TO_STRING (result));
	return Fcons (xd_dbus_type_to_symbol (dtype), Fnreverse (result));
      }

    default:
      XD_DEBUG_MESSAGE ("DBusType '%c' not supported", dtype);
      return Qnil;
    }
}

/* Return the number of references of the shared CONNECTION.  */
static ptrdiff_t
xd_get_connection_references (DBusConnection *connection)
{
  ptrdiff_t *refcount;

  /* We cannot access the DBusConnection structure, it is not public.
     But we know, that the reference counter is the first field in
     that structure.  */
  refcount = (void *) &connection;
  refcount =  (void *) *refcount;
  return *refcount;
}

/* Convert a Lisp D-Bus object to a pointer.  */
static DBusConnection *
xd_lisp_dbus_to_dbus (Lisp_Object bus)
{
  return xmint_pointer (Fcar (bus));
}

#ifdef HAVE_MPS
/* Convert a Lisp D-Bus object to a pointer.  */
static Lisp_Object *
xd_lisp_dbus_to_pin (Lisp_Object bus)
{
  return xmint_pointer (Fcdr (bus));
}
#endif

/* Return D-Bus connection address.
   BUS is either a Lisp symbol, :system, :session, :system-private or
   :session-private, or a string denoting the bus address.  */
static DBusConnection *
xd_get_connection_address (Lisp_Object bus)
{
  DBusConnection *connection;
  Lisp_Object val;

  val = CDR_SAFE (Fassoc (bus, xd_registered_buses, Qnil));
  if (NILP (val))
    XD_SIGNAL2 (build_string ("No connection to bus"), bus);
  else
    connection = xd_lisp_dbus_to_dbus (val);

  if (!dbus_connection_get_is_connected (connection))
    XD_SIGNAL2 (build_string ("No connection to bus"), bus);

  return connection;
}

/* Return the file descriptor for WATCH, -1 if not found.  */
static int
xd_find_watch_fd (DBusWatch *watch)
{
#if HAVE_DBUS_WATCH_GET_UNIX_FD
  /* TODO: Reverse these on w32, which prefers the opposite.  */
  int fd = dbus_watch_get_unix_fd (watch);
  if (fd == -1)
    fd = dbus_watch_get_socket (watch);
#else
  int fd = dbus_watch_get_fd (watch);
#endif
  return fd;
}

/* Prototype.  */
static void xd_read_queued_messages (int fd, void *data);

/* Start monitoring WATCH for possible I/O.  */
static dbus_bool_t
xd_add_watch (DBusWatch *watch, void *data)
{
  unsigned int flags = dbus_watch_get_flags (watch);
  int fd = xd_find_watch_fd (watch);

  XD_DEBUG_MESSAGE ("fd %d, write %u, enabled %u",
		    fd, flags & DBUS_WATCH_WRITABLE,
		    dbus_watch_get_enabled (watch));

  if (fd == -1)
    return FALSE;

  if (dbus_watch_get_enabled (watch))
    {
      if (flags & DBUS_WATCH_WRITABLE)
        add_write_fd (fd, xd_read_queued_messages, data);
      if (flags & DBUS_WATCH_READABLE)
        add_read_fd (fd, xd_read_queued_messages, data);
    }
  return TRUE;
}

/* Stop monitoring WATCH for possible I/O.
   DATA is the used bus, either a string or QCsystem, QCsession,
   QCsystem_private or QCsession_private.  */
static void
xd_remove_watch (DBusWatch *watch, void *data)
{
  unsigned int flags = dbus_watch_get_flags (watch);
  int fd = xd_find_watch_fd (watch);

  XD_DEBUG_MESSAGE ("fd %d", fd);

  if (fd == -1)
    return;

  /* Unset session environment.  */
#if 0
  /* This is buggy, since unsetenv is not thread-safe.  */
  if (XSYMBOL (QCsession) == data) || (XSYMBOL (QCsession_private) == data)
    {
      XD_DEBUG_MESSAGE ("unsetenv DBUS_SESSION_BUS_ADDRESS");
      unsetenv ("DBUS_SESSION_BUS_ADDRESS");
    }
#endif

  if (flags & DBUS_WATCH_WRITABLE)
    delete_write_fd (fd);
  if (flags & DBUS_WATCH_READABLE)
    delete_read_fd (fd);
}

/* Toggle monitoring WATCH for possible I/O.  */
static void
xd_toggle_watch (DBusWatch *watch, void *data)
{
  if (dbus_watch_get_enabled (watch))
    xd_add_watch (watch, data);
  else
    xd_remove_watch (watch, data);
}

/* Close connection to D-Bus BUS.  */
static void
xd_close_bus (Lisp_Object bus)
{
  DBusConnection *connection;
  Lisp_Object val;
  Lisp_Object busobj;

  /* Check whether we are connected.  */
  val = Fassoc (bus, xd_registered_buses, Qnil);
  if (NILP (val))
    return;

  busobj = CDR_SAFE (val);
  if (NILP (busobj)) {
    xd_registered_buses = Fdelete (val, xd_registered_buses);
    return;
  }

  /* Retrieve bus address.  */
  connection = xd_lisp_dbus_to_dbus (busobj);
#ifdef HAVE_MPS
  Lisp_Object *pin = xd_lisp_dbus_to_pin (busobj);
#endif

  if (xd_get_connection_references (connection) == 1)
    {
      /* Close connection, if there isn't another shared application.  */
      XD_DEBUG_MESSAGE ("Close connection to bus %s",
			XD_OBJECT_TO_STRING (bus));
      dbus_connection_close (connection);

      xd_registered_buses = Fdelete (val, xd_registered_buses);
#ifdef HAVE_MPS
      igc_xfree (pin);
#endif
    }

  else
    /* Decrement reference count.  */
    dbus_connection_unref (connection);

  /* Return.  */
  return;
}

DEFUN ("dbus--init-bus", Fdbus__init_bus, Sdbus__init_bus, 1, 2, 0,
       doc: /* Establish the connection to D-Bus BUS.

This function is dbus internal.  You almost certainly want to use
`dbus-init-bus'.

BUS can be either the symbol `:system' or the symbol `:session', or it
can be a string denoting the address of the corresponding bus.  For
the system and session buses, this function is called when loading
`dbus.el', there is no need to call it again.

A special case is BUS being the symbol `:system-private' or
`:session-private'.  These symbols still denote the system or session
bus, but using a private connection.  They should not be used outside
dbus.el.

The function returns a number, which counts the connections this Emacs
session has established to the BUS under the same unique name (see
`dbus-get-unique-name').  It depends on the libraries Emacs is linked
with, and on the environment Emacs is running.  For example, if Emacs
is linked with the gtk toolkit, and it runs in a GTK-aware environment
like Gnome, another connection might already be established.

When PRIVATE is non-nil, a new connection is established instead of
reusing an existing one.  It results in a new unique name at the bus.
This can be used, if it is necessary to distinguish from another
connection used in the same Emacs process, like the one established by
GTK+.  It should be used with care for at least the `:system' and
`:session' buses, because other Emacs Lisp packages might already use
this connection to those buses.  */)
  (Lisp_Object bus, Lisp_Object private)
{
  DBusConnection *connection;
  DBusError derror;
  Lisp_Object val;
  ptrdiff_t refcount;

  /* Check parameter.  */
  if (!NILP (private))
    bus = EQ (bus, QCsystem)
      ? QCsystem_private
      : EQ (bus, QCsession) ? QCsession_private : bus;
  XD_DBUS_VALIDATE_BUS_ADDRESS (bus);

  /* Close bus if it is already open.  */
  xd_close_bus (bus);

  /* Check, whether we are still connected.  */
  val = Fassoc (bus, xd_registered_buses, Qnil);
  if (!NILP (val))
    {
      connection = xd_get_connection_address (bus);
      dbus_connection_ref (connection);
    }

  else
    {
      /* Initialize.  */
      dbus_error_init (&derror);

      /* Open the connection.  */
      if (STRINGP (bus))
	if (NILP (private))
	  connection = dbus_connection_open (SSDATA (bus), &derror);
	else
	  connection = dbus_connection_open_private (SSDATA (bus), &derror);

      else
	{
	  DBusBusType bustype
	    = EQ (bus, QCsystem) || EQ (bus, QCsystem_private)
	    ? DBUS_BUS_SYSTEM : DBUS_BUS_SESSION;
	  if (NILP (private))
	    connection = dbus_bus_get (bustype, &derror);
	  else
	    connection = dbus_bus_get_private (bustype, &derror);
	}

      if (dbus_error_is_set (&derror))
	XD_ERROR (derror);

      if (connection == NULL)
	XD_SIGNAL2 (build_string ("No connection to bus"), bus);

      /* If it is not the system or session bus, we must register
	 ourselves.  Otherwise, we have called dbus_bus_get{_private},
	 which has configured us to exit if the connection closes - we
	 undo this setting.  */
      if (STRINGP (bus))
	dbus_bus_register (connection, &derror);
      else
	dbus_connection_set_exit_on_disconnect (connection, FALSE);

      if (dbus_error_is_set (&derror))
	XD_ERROR (derror);

      /* Add the watch functions.  We pass also the bus as data, in
	 order to distinguish between the buses in xd_remove_watch.  */
#ifdef HAVE_MPS
      Lisp_Object* pin = igc_xzalloc_ambig (sizeof *pin);
      *pin = bus;
#else
      Lisp_Object* pin = NULL;
#endif

      if (!dbus_connection_set_watch_functions (connection,
						xd_add_watch,
						xd_remove_watch,
						xd_toggle_watch,
						XD_KEYWORDP (bus)
						? (void *) XSYMBOL (bus)
						: (void *) XSTRING (bus),
						NULL))
	XD_SIGNAL1 (build_string ("Cannot add watch functions"));

      /* Add bus to list of registered buses.  */
      val = Fcons (make_mint_ptr (connection), make_mint_ptr (pin));
      xd_registered_buses = Fcons (Fcons (bus, val), xd_registered_buses);

      /* Cleanup.  */
      dbus_error_free (&derror);
    }

  XD_DEBUG_MESSAGE ("Registered buses: %s",
		    XD_OBJECT_TO_STRING (xd_registered_buses));

  /* Return reference counter.  */
  refcount = xd_get_connection_references (connection);
  XD_DEBUG_MESSAGE ("Bus %s, Reference counter %"pD"d",
		    XD_OBJECT_TO_STRING (bus), refcount);
  return make_fixnum (refcount);
}

DEFUN ("dbus-get-unique-name", Fdbus_get_unique_name, Sdbus_get_unique_name,
       1, 1, 0,
       doc: /* Return the unique name of Emacs registered at D-Bus BUS.  */)
  (Lisp_Object bus)
{
  DBusConnection *connection;
  const char *name;

  /* Check parameter.  */
  XD_DBUS_VALIDATE_BUS_ADDRESS (bus);

  /* Retrieve bus address.  */
  connection = xd_get_connection_address (bus);

  /* Request the name.  */
  name = dbus_bus_get_unique_name (connection);
  if (name == NULL)
    XD_SIGNAL1 (build_string ("No unique name available"));

  /* Return.  */
  return build_string (name);
}

DEFUN ("dbus-message-internal", Fdbus_message_internal, Sdbus_message_internal,
       4, MANY, 0,
       doc: /* Send a D-Bus message.
This is an internal function, it shall not be used outside dbus.el.

The following usages are expected:

`dbus-call-method', `dbus-call-method-asynchronously':
  (dbus-message-internal
    dbus-message-type-method-call BUS SERVICE PATH INTERFACE METHOD HANDLER
    &optional :timeout TIMEOUT :authorizable AUTH &rest ARGS)

`dbus-send-signal':
  (dbus-message-internal
    dbus-message-type-signal BUS SERVICE PATH INTERFACE SIGNAL &rest ARGS)

`dbus-method-return-internal':
  (dbus-message-internal
    dbus-message-type-method-return BUS SERVICE SERIAL &rest ARGS)

`dbus-method-error-internal':
  (dbus-message-internal
    dbus-message-type-error BUS SERVICE SERIAL ERROR-NAME &rest ARGS)

`dbus-check-arguments': (does not send a message)
  (dbus-message-internal
    dbus-message-type-invalid BUS SERVICE &rest ARGS)

usage: (dbus-message-internal &rest REST)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object message_type, bus, service, handler;
  Lisp_Object path = Qnil;
  Lisp_Object interface = Qnil;
  Lisp_Object member = Qnil;
  Lisp_Object error_name = Qnil;
  Lisp_Object result;
  DBusConnection *connection;
  DBusMessage *dmessage;
  DBusMessageIter iter;
  int dtype;
  int mtype;
  dbus_uint32_t serial = 0;
  unsigned int ui_serial;
  int timeout = -1;
  ptrdiff_t count, count0;
  char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];

  /* Initialize parameters.  */
  message_type = args[0];
  bus = args[1];
  service = args[2];
  handler = Qnil;

  CHECK_FIXNAT (message_type);
  if (! (DBUS_MESSAGE_TYPE_INVALID <= XFIXNAT (message_type)
	 && XFIXNAT (message_type) < DBUS_NUM_MESSAGE_TYPES))
    XD_SIGNAL2 (build_string ("Invalid message type"), message_type);
  mtype = XFIXNAT (message_type);

  if ((mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)
      || (mtype == DBUS_MESSAGE_TYPE_SIGNAL))
    {
      path = args[3];
      interface = args[4];
      member = args[5];
      if (mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)
	handler = args[6];
      count = (mtype == DBUS_MESSAGE_TYPE_METHOD_CALL) ? 7 : 6;
    }
  else if ((mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)
	   || (mtype == DBUS_MESSAGE_TYPE_ERROR))
    {
      serial = xd_extract_unsigned (args[3], TYPE_MAXIMUM (dbus_uint32_t));
      if (mtype == DBUS_MESSAGE_TYPE_ERROR)
	error_name = args[4];
      count = (mtype == DBUS_MESSAGE_TYPE_ERROR) ? 5 : 4;
    }
  else /* DBUS_MESSAGE_TYPE_INVALID  */
    count = 3;

  /* Check parameters.  */
  XD_DBUS_VALIDATE_BUS_ADDRESS (bus);
  XD_DBUS_VALIDATE_BUS_NAME (service);
  if (nargs < count)
    xsignal2 (Qwrong_number_of_arguments,
	      Qdbus_message_internal,
	      make_fixnum (nargs));

  if ((mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)
      || (mtype == DBUS_MESSAGE_TYPE_SIGNAL))
    {
      XD_DBUS_VALIDATE_PATH (path);
      XD_DBUS_VALIDATE_INTERFACE (interface);
      XD_DBUS_VALIDATE_MEMBER (member);
      if (!NILP (handler) && !FUNCTIONP (handler))
	wrong_type_argument (Qinvalid_function, handler);
    }

  /* Trace parameters.  */
  switch (mtype)
    {
    case DBUS_MESSAGE_TYPE_METHOD_CALL:
      XD_DEBUG_MESSAGE ("%s %s %s %s %s %s %s",
			XD_MESSAGE_TYPE_TO_STRING (mtype),
			XD_OBJECT_TO_STRING (bus),
			XD_OBJECT_TO_STRING (service),
			XD_OBJECT_TO_STRING (path),
			XD_OBJECT_TO_STRING (interface),
			XD_OBJECT_TO_STRING (member),
			XD_OBJECT_TO_STRING (handler));
      break;
    case DBUS_MESSAGE_TYPE_SIGNAL:
      XD_DEBUG_MESSAGE ("%s %s %s %s %s %s",
			XD_MESSAGE_TYPE_TO_STRING (mtype),
			XD_OBJECT_TO_STRING (bus),
			XD_OBJECT_TO_STRING (service),
			XD_OBJECT_TO_STRING (path),
			XD_OBJECT_TO_STRING (interface),
			XD_OBJECT_TO_STRING (member));
      break;
    case DBUS_MESSAGE_TYPE_METHOD_RETURN:
      ui_serial = serial;
      XD_DEBUG_MESSAGE ("%s %s %s %u",
			XD_MESSAGE_TYPE_TO_STRING (mtype),
			XD_OBJECT_TO_STRING (bus),
			XD_OBJECT_TO_STRING (service),
			ui_serial);
       break;
    case DBUS_MESSAGE_TYPE_ERROR:
      ui_serial = serial;
      XD_DEBUG_MESSAGE ("%s %s %s %u %s",
			XD_MESSAGE_TYPE_TO_STRING (mtype),
			XD_OBJECT_TO_STRING (bus),
			XD_OBJECT_TO_STRING (service),
			ui_serial,
			XD_OBJECT_TO_STRING (error_name));
      break;
    default: /* DBUS_MESSAGE_TYPE_INVALID  */
      XD_DEBUG_MESSAGE ("%s %s %s",
			XD_MESSAGE_TYPE_TO_STRING (mtype),
			XD_OBJECT_TO_STRING (bus),
			XD_OBJECT_TO_STRING (service));
    }

  /* Retrieve bus address.  */
  connection = xd_get_connection_address (bus);

  /* Create the D-Bus message.  Since DBUS_MESSAGE_TYPE_INVALID is not
     a valid message type, we mockup it with DBUS_MESSAGE_TYPE_SIGNAL.  */
  dmessage = dbus_message_new
    ((mtype == DBUS_MESSAGE_TYPE_INVALID) ? DBUS_MESSAGE_TYPE_SIGNAL : mtype);
  if (dmessage == NULL)
    XD_SIGNAL1 (build_string ("Unable to create a new message"));

  if ((STRINGP (service)) && (mtype != DBUS_MESSAGE_TYPE_INVALID))
    {
      if (mtype != DBUS_MESSAGE_TYPE_SIGNAL)
	/* Set destination.  */
	{
	  if (!dbus_message_set_destination (dmessage, SSDATA (service)))
	    XD_SIGNAL2 (build_string ("Unable to set the destination"),
			service);
	}

      else
	/* Set destination for unicast signals.  */
	{
	  Lisp_Object uname;

	  /* If it is the same unique name as we are registered at the
	     bus or an unknown name, we regard it as broadcast message
	     due to backward compatibility.  */
	  if (dbus_bus_name_has_owner (connection, SSDATA (service), NULL))
	    uname = calln (Qdbus_get_name_owner, bus, service);
	  else
	    uname = Qnil;

	  if (STRINGP (uname)
	      && (strcmp (dbus_bus_get_unique_name (connection), SSDATA (uname))
		  != 0)
	      && (!dbus_message_set_destination (dmessage, SSDATA (service))))
	    XD_SIGNAL2 (build_string ("Unable to set signal destination"),
			service);
	}
    }

  /* Set message parameters.  */
  if ((mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)
      || (mtype == DBUS_MESSAGE_TYPE_SIGNAL))
    {
      if ((!dbus_message_set_path (dmessage, SSDATA (path)))
	  || (!dbus_message_set_interface (dmessage, SSDATA (interface)))
	  || (!dbus_message_set_member (dmessage, SSDATA (member))))
	XD_SIGNAL1 (build_string ("Unable to set the message parameter"));
    }

  else if ((mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)
	   || (mtype == DBUS_MESSAGE_TYPE_ERROR))
    {
      if (!dbus_message_set_reply_serial (dmessage, serial))
	XD_SIGNAL1 (build_string ("Unable to create a return message"));

      if ((mtype == DBUS_MESSAGE_TYPE_ERROR)
	  && (!dbus_message_set_error_name (dmessage, SSDATA (error_name))))
	XD_SIGNAL1 (build_string ("Unable to create an error message"));
    }

  while ((count + 2 <= nargs))
    {
      /* Check for timeout parameter.  */
      if (EQ (args[count], QCtimeout))
        {
	  if (mtype != DBUS_MESSAGE_TYPE_METHOD_CALL)
	    XD_SIGNAL1
	      (build_string (":timeout is only supported on method calls"));

          CHECK_FIXNAT (args[count+1]);
          timeout = min (XFIXNAT (args[count+1]), INT_MAX);
          count = count + 2;
	}
      /* Check for authorizable parameter.  */
      else if (EQ (args[count], QCauthorizable))
        {
	  if (mtype != DBUS_MESSAGE_TYPE_METHOD_CALL)
	    XD_SIGNAL1
	      (build_string (":authorizable is only supported on method calls"));

	  /* Ignore this keyword if unsupported.  */
#ifdef HAVE_DBUS_MESSAGE_SET_ALLOW_INTERACTIVE_AUTHORIZATION
	  dbus_message_set_allow_interactive_authorization
	    (dmessage, NILP (args[count+1]) ? FALSE : TRUE);
#else
	  XD_DEBUG_MESSAGE (":authorizable not supported");
#endif

          count = count + 2;
	}
      else break;

    }

  /* Initialize parameter list of message.  */
  dbus_message_iter_init_append (dmessage, &iter);

  /* Append parameters to the message.  */
  count0 = count - 1;
  for (; count < nargs; ++count)
    {
      dtype = XD_OBJECT_TO_DBUS_TYPE (args[count]);
      if (count + 1 < nargs && XD_DBUS_TYPE_P (args[count]))
	{
	  XD_DEBUG_VALID_LISP_OBJECT_P (args[count]);
	  XD_DEBUG_VALID_LISP_OBJECT_P (args[count+1]);
	  XD_DEBUG_MESSAGE ("Parameter%"pD"d: %s Parameter%"pD"d: %s",
			    count - count0,
			    XD_OBJECT_TO_STRING (args[count]),
			    count + 1 - count0,
			    XD_OBJECT_TO_STRING (args[count+1]));
	  ++count;
	}
      else
	{
	  XD_DEBUG_VALID_LISP_OBJECT_P (args[count]);
	  XD_DEBUG_MESSAGE ("Parameter%"pD"d: %s", count - count0,
			    XD_OBJECT_TO_STRING (args[count]));
	}

      /* Check for valid signature.  We use DBUS_TYPE_INVALID as
	 indication that there is no parent type.  */
      xd_signature (signature, dtype, DBUS_TYPE_INVALID, args[count]);

      xd_append_arg (dtype, args[count], &iter);
    }

  if (mtype == DBUS_MESSAGE_TYPE_INVALID)
    result = Qt;

  else if (!NILP (handler))
    {
      /* Send the message.  The message is just added to the outgoing
	 message queue.  */
      if (!dbus_connection_send_with_reply (connection, dmessage,
					    NULL, timeout))
	XD_SIGNAL1 (build_string ("Cannot send message"));

      /* The result is the key in Vdbus_registered_objects_table.  */
      serial = dbus_message_get_serial (dmessage);
      result = list3 (QCserial, bus, INT_TO_INTEGER (serial));

      /* Create a hash table entry.  */
      Fputhash (result, handler, Vdbus_registered_objects_table);
    }
  else
    {
      /* Send the message.  The message is just added to the outgoing
	 message queue.  */
      if (!dbus_connection_send (connection, dmessage, NULL))
	XD_SIGNAL1 (build_string ("Cannot send message"));

      result = Qnil;
    }

  if (mtype != DBUS_MESSAGE_TYPE_INVALID)
    XD_DEBUG_MESSAGE ("Message sent: %s", XD_OBJECT_TO_STRING (result));

  /* Cleanup.  */
  dbus_message_unref (dmessage);

  /* Return the result.  */
  return result;
}

/* Read one queued incoming message of the D-Bus BUS.
   BUS is either a Lisp symbol, :system, :session, :system-private or
   :session-private, or a string denoting the bus address.  */
static void
xd_read_message_1 (DBusConnection *connection, Lisp_Object bus)
{
  Lisp_Object args, key, value;
  struct input_event event;
  DBusMessage *dmessage;
  DBusMessageIter iter;
  int dtype;
  int mtype;
  dbus_uint32_t serial;
  unsigned int ui_serial;
  const char *uname, *destination, *path, *interface, *member, *error_name;

  dmessage = dbus_connection_pop_message (connection);

  /* Return if there is no queued message.  */
  if (dmessage == NULL)
    return;

  /* Collect the parameters.  */
  args = Qnil;

  /* Loop over the resulting parameters.  Construct a list.  */
  if (dbus_message_iter_init (dmessage, &iter))
    {
      while ((dtype = dbus_message_iter_get_arg_type (&iter))
	     != DBUS_TYPE_INVALID)
	{
	  args = Fcons (xd_retrieve_arg (dtype, &iter), args);
	  dbus_message_iter_next (&iter);
	}
      /* The arguments are stored in reverse order.  Reorder them.  */
      args = Fnreverse (args);
    }

  /* Read message type, message serial, unique name, object path,
     interface, member and error name from the message.  */
  mtype = dbus_message_get_type (dmessage);
  ui_serial = serial =
    ((mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)
     || (mtype == DBUS_MESSAGE_TYPE_ERROR))
    ? dbus_message_get_reply_serial (dmessage)
    : dbus_message_get_serial (dmessage);
  uname = dbus_message_get_sender (dmessage);
  destination = dbus_message_get_destination (dmessage);
  path = dbus_message_get_path (dmessage);
  interface = dbus_message_get_interface (dmessage);
  member = dbus_message_get_member (dmessage);
  error_name = dbus_message_get_error_name (dmessage);

  XD_DEBUG_MESSAGE ("Event received: %s %u %s %s %s %s %s %s",
		    XD_MESSAGE_TYPE_TO_STRING (mtype),
		    ui_serial, uname, destination, path, interface,
		    mtype == DBUS_MESSAGE_TYPE_ERROR ? error_name : member,
		    XD_OBJECT_TO_STRING (args));

  if (mtype == DBUS_MESSAGE_TYPE_INVALID)
    goto cleanup;

  else if ((mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)
	   || (mtype == DBUS_MESSAGE_TYPE_ERROR))
    {
      /* Search for a registered function of the message.  */
      key = list3 (QCserial, bus, INT_TO_INTEGER (serial));
      value = Fgethash (key, Vdbus_registered_objects_table, Qnil);

      /* There shall be exactly one entry.  Construct an event.  */
      if (NILP (value))
	goto monitor;

      /* Remove the entry.  */
      Fremhash (key, Vdbus_registered_objects_table);

      /* Construct an event.  */
      EVENT_INIT (event);
      event.kind = DBUS_EVENT;
      event.frame_or_window = Qnil;
      /* Handler.  */
      event.arg = Fcons (value, args);
    }

  else /* DBUS_MESSAGE_TYPE_METHOD_CALL, DBUS_MESSAGE_TYPE_SIGNAL.  */
    {
      /* Vdbus_registered_objects_table requires non-nil interface and
	 member.  */
      if ((interface == NULL) || (member == NULL))
	goto monitor;

      /* Search for a registered function of the message.  */
      key = list4 (mtype == DBUS_MESSAGE_TYPE_METHOD_CALL ? QCmethod : QCsignal,
		   bus, build_string (interface), build_string (member));
      value = Fgethash (key, Vdbus_registered_objects_table, Qnil);

      /* A signal could be registered with a nil interface or member.  */
      if (mtype == DBUS_MESSAGE_TYPE_SIGNAL)
	{
	  key = list4 (QCsignal, bus, Qnil, build_string (member));
	  value = CALLN (Fappend, value,
			 Fgethash (key, Vdbus_registered_objects_table, Qnil));

	  key = list4 (QCsignal, bus, build_string (interface), Qnil);
	  value = CALLN (Fappend, value,
			 Fgethash (key, Vdbus_registered_objects_table, Qnil));

	  key = list4 (QCsignal, bus, Qnil, Qnil);
	  value = CALLN (Fappend, value,
			 Fgethash (key, Vdbus_registered_objects_table, Qnil));
	}

      /* Loop over the registered functions.  Construct an event.  */
      for (; !NILP (value); value = CDR_SAFE (value))
	{
	  key = CAR_SAFE (value);
	  Lisp_Object key_uname = CAR_SAFE (key);
	  /* key has the structure (UNAME SERVICE PATH HANDLER).  */
	  if (uname && !NILP (key_uname)
	      && strcmp (uname, SSDATA (key_uname)) != 0)
	    continue;
	  Lisp_Object key_service_etc = CDR_SAFE (key);
	  Lisp_Object key_path_etc = CDR_SAFE (key_service_etc);
	  Lisp_Object key_path = CAR_SAFE (key_path_etc);
	  if (path && !NILP (key_path)
	      && strcmp (path, SSDATA (key_path)) != 0)
	    continue;
	  Lisp_Object handler = CAR_SAFE (CDR_SAFE (key_path_etc));
	  if (NILP (handler))
	    continue;

	  /* Construct an event and exit the loop.  */
	  EVENT_INIT (event);
	  event.kind = DBUS_EVENT;
	  event.frame_or_window = Qnil;
	  event.arg = Fcons (handler, args);
	  break;
	}

      if (NILP (value))
	goto monitor;
    }

  /* Add type, serial, uname, destination, path, interface and member
     or error_name to the event.  */
  event.arg
    = Fcons (mtype == DBUS_MESSAGE_TYPE_ERROR
	     ? error_name == NULL ? Qnil : build_string (error_name)
	     : member == NULL ? Qnil : build_string (member),
	     event.arg);
  event.arg = Fcons ((interface == NULL ? Qnil : build_string (interface)),
		     event.arg);
  event.arg = Fcons ((path == NULL ? Qnil : build_string (path)),
		     event.arg);
  event.arg = Fcons ((destination == NULL ? Qnil : build_string (destination)),
		     event.arg);
  event.arg = Fcons ((uname == NULL ? Qnil : build_string (uname)),
		     event.arg);
  event.arg = Fcons (INT_TO_INTEGER (serial), event.arg);
  event.arg = Fcons (make_fixnum (mtype), event.arg);

  /* Add the bus symbol to the event.  */
  event.arg = Fcons (bus, event.arg);

  /* Store it into the input event queue.  */
  kbd_buffer_store_event (&event);

  XD_DEBUG_MESSAGE ("Event stored: %s", XD_OBJECT_TO_STRING (event.arg));

  /* Monitor.  */
 monitor:
  /* Search for a registered function of the message.  */
  key = list2 (QCmonitor, bus);
  value = Fgethash (key, Vdbus_registered_objects_table, Qnil);

  /* There shall be exactly one entry.  Construct an event.  */
  if (NILP (value))
    goto cleanup;

  /* Construct an event.  */
  EVENT_INIT (event);
  event.kind = DBUS_EVENT;
  event.frame_or_window = Qnil;

  /* Add type, serial, uname, destination, path, interface, member
     or error_name and handler to the event.  */
  event.arg
    = Fcons (CAR_SAFE (CDR_SAFE (CDR_SAFE (CDR_SAFE (CAR_SAFE (value))))),
	     args);
  event.arg
    = Fcons (mtype == DBUS_MESSAGE_TYPE_ERROR
	     ? error_name == NULL ? Qnil : build_string (error_name)
	     : member == NULL ? Qnil : build_string (member),
	     event.arg);
  event.arg = Fcons ((interface == NULL ? Qnil : build_string (interface)),
		     event.arg);
  event.arg = Fcons ((path == NULL ? Qnil : build_string (path)),
		     event.arg);
  event.arg = Fcons ((destination == NULL ? Qnil : build_string (destination)),
		     event.arg);
  event.arg = Fcons ((uname == NULL ? Qnil : build_string (uname)),
		     event.arg);
  event.arg = Fcons (INT_TO_INTEGER (serial), event.arg);
  event.arg = Fcons (make_fixnum (mtype), event.arg);

  /* Add the bus symbol to the event.  */
  event.arg = Fcons (bus, event.arg);

  /* Store it into the input event queue.  */
  kbd_buffer_store_event (&event);

  XD_DEBUG_MESSAGE ("Monitor event stored: %s", XD_OBJECT_TO_STRING (event.arg));

  /* Cleanup.  */
 cleanup:
  dbus_message_unref (dmessage);
}

/* Read queued incoming messages of the D-Bus BUS.
   BUS is either a Lisp symbol, :system, :session, :system-private or
   :session-private, or a string denoting the bus address.  */
static Lisp_Object
xd_read_message (Lisp_Object bus)
{
  /* Retrieve bus address.  */
  DBusConnection *connection = xd_get_connection_address (bus);

  /* Non blocking read of the next available message.  */
  dbus_connection_read_write (connection, 0);

  while (dbus_connection_get_dispatch_status (connection)
         != DBUS_DISPATCH_COMPLETE)
    xd_read_message_1 (connection, bus);
  return Qnil;
}

/* Callback called when something is ready to read or write.  */
static void
xd_read_queued_messages (int fd, void *data)
{
  Lisp_Object busp = xd_registered_buses;
  Lisp_Object bus = Qnil;
  Lisp_Object key;

  /* Find bus related to fd.  */
  if (data != NULL)
    while (!NILP (busp))
      {
	key = CAR_SAFE (CAR_SAFE (busp));
	if ((XD_KEYWORDP (key) && XSYMBOL (key) == data)
	    || (STRINGP (key) && XSTRING (key) == data))
	  bus = key;
	busp = CDR_SAFE (busp);
      }

  if (NILP (bus))
    return;

  /* We ignore all Lisp errors during the call.  */
  xd_in_read_queued_messages = 1;
  internal_catch (Qdbus_error, xd_read_message, bus);
  xd_in_read_queued_messages = 0;
}


void
init_dbusbind (void)
{
  /* We do not want to abort.  */
  xputenv ("DBUS_FATAL_WARNINGS=0");
}

static void
syms_of_dbusbind_for_pdumper (void)
{
  xd_registered_buses = Qnil;
}

void
syms_of_dbusbind (void)
{
  defsubr (&Sdbus__init_bus);
  defsubr (&Sdbus_get_unique_name);

  DEFSYM (Qdbus_message_internal, "dbus-message-internal");
  defsubr (&Sdbus_message_internal);

  /* D-Bus error symbol.  */
  DEFSYM (Qdbus_error, "dbus-error");
  Fput (Qdbus_error, Qerror_conditions,
	list2 (Qdbus_error, Qerror));
  Fput (Qdbus_error, Qerror_message,
	build_string ("D-Bus error"));
  DEFSYM (QD_Bus, "D-Bus");

  /* Lisp symbols of the system and session buses.  */
  DEFSYM (QCsystem, ":system");
  DEFSYM (QCsession, ":session");
  DEFSYM (QCsystem_private, ":system-private");
  DEFSYM (QCsession_private, ":session-private");

  /* Lisp symbol for method call timeout.  */
  DEFSYM (QCtimeout, ":timeout");

  /* Lisp symbol for method interactive authorization.  */
  DEFSYM (QCauthorizable, ":authorizable");

  /* Lisp symbols of D-Bus types.  */
  DEFSYM (QCbyte, ":byte");
  DEFSYM (QCboolean, ":boolean");
  DEFSYM (QCint16, ":int16");
  DEFSYM (QCuint16, ":uint16");
  DEFSYM (QCint32, ":int32");
  DEFSYM (QCuint32, ":uint32");
  DEFSYM (QCint64, ":int64");
  DEFSYM (QCuint64, ":uint64");
  DEFSYM (QCdouble, ":double");
  DEFSYM (QCstring, ":string");
  DEFSYM (QCobject_path, ":object-path");
  DEFSYM (QCsignature, ":signature");
#ifdef DBUS_TYPE_UNIX_FD
  DEFSYM (QCunix_fd, ":unix-fd");
#endif
  DEFSYM (QCarray, ":array");
  DEFSYM (QCvariant, ":variant");
  DEFSYM (QCstruct, ":struct");
  DEFSYM (QCdict_entry, ":dict-entry");

  /* Lisp symbols of objects in `dbus-registered-objects-table'.
     `:property', which does exist there as well, is not declared here.  */
  DEFSYM (QCserial, ":serial");
  DEFSYM (QCmethod, ":method");
  DEFSYM (QCsignal, ":signal");
  DEFSYM (QCmonitor, ":monitor");

  /* Miscellaneous Lisp symbols.  */
  DEFSYM (Qdbus_get_name_owner, "dbus-get-name-owner");

  DEFVAR_LISP ("dbus-compiled-version",
	       Vdbus_compiled_version,
    doc: /* The version of D-Bus Emacs is compiled against.  */);
#ifdef DBUS_VERSION_STRING
  Vdbus_compiled_version = build_string (DBUS_VERSION_STRING);
#else
  Vdbus_compiled_version = Qnil;
#endif

  DEFVAR_LISP ("dbus-runtime-version",
	       Vdbus_runtime_version,
    doc: /* The version of D-Bus Emacs runs with.  */);
  {
#ifdef DBUS_VERSION
    int major, minor, micro;
    dbus_get_version (&major, &minor, &micro);
    Vdbus_runtime_version
      = make_formatted_string ("%d.%d.%d", major, minor, micro);
#else
    Vdbus_runtime_version = Qnil;
#endif
  }

  DEFVAR_LISP ("dbus-message-type-invalid",
	       Vdbus_message_type_invalid,
    doc: /* This value is never a valid message type.  */);
  Vdbus_message_type_invalid = make_fixnum (DBUS_MESSAGE_TYPE_INVALID);

  DEFVAR_LISP ("dbus-message-type-method-call",
	       Vdbus_message_type_method_call,
    doc: /* Message type of a method call message.  */);
  Vdbus_message_type_method_call = make_fixnum (DBUS_MESSAGE_TYPE_METHOD_CALL);

  DEFVAR_LISP ("dbus-message-type-method-return",
	       Vdbus_message_type_method_return,
    doc: /* Message type of a method return message.  */);
  Vdbus_message_type_method_return
    = make_fixnum (DBUS_MESSAGE_TYPE_METHOD_RETURN);

  DEFVAR_LISP ("dbus-message-type-error",
	       Vdbus_message_type_error,
    doc: /* Message type of an error reply message.  */);
  Vdbus_message_type_error = make_fixnum (DBUS_MESSAGE_TYPE_ERROR);

  DEFVAR_LISP ("dbus-message-type-signal",
	       Vdbus_message_type_signal,
    doc: /* Message type of a signal message.  */);
  Vdbus_message_type_signal = make_fixnum (DBUS_MESSAGE_TYPE_SIGNAL);

  DEFVAR_LISP ("dbus-registered-objects-table",
	       Vdbus_registered_objects_table,
    doc: /* Hash table of registered functions for D-Bus.

There are two different uses of the hash table: for accessing
registered interfaces properties, targeted by signals, method calls or
monitors, and for calling handlers in case of non-blocking method call
returns.

In the first case, the key in the hash table is the list (TYPE BUS
[INTERFACE MEMBER]).  TYPE is one of the Lisp symbols `:method',
`:signal', `:property' or `:monitor'.  BUS is either a Lisp symbol,
`:system', `:session', `:system-private' or `:session-private', or a
string denoting the bus address.  INTERFACE is a string which denotes
a D-Bus interface, and MEMBER, also a string, is either a method, a
signal or a property INTERFACE is offering.  All arguments can be nil.

The value in the hash table is a list of quadruple lists ((UNAME
SERVICE PATH OBJECT [RULE]) ...).  SERVICE is the service name as
registered, UNAME is the corresponding unique name.  In case of
registered methods, properties and monitors, UNAME is nil.  PATH is
the object path of the sending object.  All of them can be nil, which
means a wildcard then.

OBJECT is either the handler to be called when a D-Bus message, which
matches the key criteria, arrives (TYPE `:method', `:signal' and
`:monitor'), or a list (ACCESS EMITS-SIGNAL VALUE) for TYPE
`:property'.

For entries of type `:signal' or `:monitor', there is also a fifth
element RULE, which keeps the match string the signal or monitor is
registered with.

In the second case, the key in the hash table is the list (:serial BUS
SERIAL).  BUS is either a Lisp symbol, `:system' or `:session', or a
string denoting the bus address.  SERIAL is the serial number of the
non-blocking method call, a reply is expected.  Both arguments must
not be nil.  The value in the hash table is HANDLER, the function to
be called when the D-Bus reply message arrives.  */);
  Vdbus_registered_objects_table = CALLN (Fmake_hash_table, QCtest, Qequal);

  DEFVAR_LISP ("dbus-debug", Vdbus_debug,
    doc: /* If non-nil, debug messages of D-Bus bindings are raised.  */);
#ifdef DBUS_DEBUG
  Vdbus_debug = Qt;
  /* We can also set environment variable DBUS_VERBOSE=1 in order to
     see more traces.  This requires libdbus-1 to be configured with
     --enable-verbose-mode.  */
#else
  Vdbus_debug = Qnil;
#endif

  /* Initialize internal objects.  */
  pdumper_do_now_and_after_load (syms_of_dbusbind_for_pdumper);
  staticpro (&xd_registered_buses);

  Fprovide (intern_c_string ("dbusbind"), Qnil);
}

#endif /* HAVE_DBUS */
