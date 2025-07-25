/* Keyboard macros.

Copyright (C) 1985-1986, 1993, 2000-2025 Free Software Foundation, Inc.

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

#include "lisp.h"
#include "macros.h"
#include "igc.h"
#include "window.h"
#include "keyboard.h"

/* Number of successful iterations so far
   for innermost keyboard macro.
   This is not bound at each level,
   so after an error, it describes the innermost interrupted macro.  */

EMACS_INT executing_kbd_macro_iterations;

/* This is the macro that was executing.
   This is not bound at each level,
   so after an error, it describes the innermost interrupted macro.
   We use it only as a kind of flag, so no need to protect it.  */

Lisp_Object executing_kbd_macro;

DEFUN ("start-kbd-macro", Fstart_kbd_macro, Sstart_kbd_macro, 1, 2, "P",
       doc: /* Record subsequent keyboard input, defining a keyboard macro.
The commands are recorded even as they are executed.
Use \\[end-kbd-macro] to finish recording and make the macro available.
Use \\[name-last-kbd-macro] to give it a permanent name.
Non-nil arg (prefix arg) means append to last macro defined;
this begins by re-executing that macro as if you typed it again.
If optional second arg, NO-EXEC, is non-nil, do not re-execute last
macro before appending to it.  */)
  (Lisp_Object append, Lisp_Object no_exec)
{
  if (!NILP (KVAR (current_kboard, defining_kbd_macro)))
    error ("Already defining kbd macro");

  if (!current_kboard->kbd_macro_buffer)
    {
#ifdef HAVE_MPS
      current_kboard->kbd_macro_buffer
	= igc_xalloc_lisp_objs_exact (30, "kbd-macro-buffer");
#else
      current_kboard->kbd_macro_buffer = xmalloc (30 * word_size);
#endif
      current_kboard->kbd_macro_bufsize = 30;
      current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_buffer;
      current_kboard->kbd_macro_end = current_kboard->kbd_macro_buffer;
    }
  update_mode_lines = 19;
  if (NILP (append))
    {
      if (current_kboard->kbd_macro_bufsize > 200)
	{
#ifdef HAVE_MPS
	  current_kboard->kbd_macro_buffer
	    = igc_xnrealloc_lisp_objs_exact (current_kboard->kbd_macro_bufsize,
					     current_kboard->kbd_macro_buffer,
					     30, "kbd-macro-buffer");
#else
	  current_kboard->kbd_macro_buffer
	    = xrealloc (current_kboard->kbd_macro_buffer, 30 * word_size);
#endif
	  current_kboard->kbd_macro_bufsize = 30;
	}
      current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_buffer;
      current_kboard->kbd_macro_end = current_kboard->kbd_macro_buffer;
      message1 ("Defining kbd macro...");
    }
  else
    {
      int incr = 30;
      ptrdiff_t i, len;
      bool cvt;

      /* Check the type of last-kbd-macro in case Lisp code changed it.  */
      len = CHECK_VECTOR_OR_STRING (KVAR (current_kboard, Vlast_kbd_macro));

      /* Copy last-kbd-macro into the buffer, in case the Lisp code
	 has put another macro there.  */
      if (current_kboard->kbd_macro_bufsize - incr < len)
	{
#ifdef HAVE_MPS
	  struct kboard *kb = current_kboard;
	  kb->kbd_macro_buffer
	    = igc_xpalloc_lisp_objs_exact (kb->kbd_macro_buffer,
					   &kb->kbd_macro_bufsize,
					   len - kb->kbd_macro_bufsize + incr,
					   -1, "kbd-macro-buffer");
#else
	  current_kboard->kbd_macro_buffer
	    = xpalloc (current_kboard->kbd_macro_buffer,
		       &current_kboard->kbd_macro_bufsize,
		       len - current_kboard->kbd_macro_bufsize + incr, -1,
		       sizeof *current_kboard->kbd_macro_buffer);
#endif
	}

      /* Must convert meta modifier when copying string to vector.  */
      cvt = STRINGP (KVAR (current_kboard, Vlast_kbd_macro));
      for (i = 0; i < len; i++)
	{
	  Lisp_Object c;
	  c = Faref (KVAR (current_kboard, Vlast_kbd_macro), make_fixnum (i));
	  if (cvt && FIXNATP (c) && (XFIXNAT (c) & 0x80))
	    XSETFASTINT (c, CHAR_META | (XFIXNAT (c) & ~0x80));
	  current_kboard->kbd_macro_buffer[i] = c;
	}

      current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_buffer + len;
      current_kboard->kbd_macro_end = current_kboard->kbd_macro_ptr;

      /* Re-execute the macro we are appending to,
	 for consistency of behavior.  */
      if (NILP (no_exec))
	Fexecute_kbd_macro (KVAR (current_kboard, Vlast_kbd_macro),
			    make_fixnum (1), Qnil);

      message1 ("Appending to kbd macro...");
    }
  kset_defining_kbd_macro (current_kboard, Qt);

  return Qnil;
}

/* Finish defining the current keyboard macro.  */

void
end_kbd_macro (void)
{
  kset_defining_kbd_macro (current_kboard, Qnil);
  update_mode_lines = 20;
  kset_last_kbd_macro
    (current_kboard,
     Fvector ((current_kboard->kbd_macro_end
	       - current_kboard->kbd_macro_buffer),
	      current_kboard->kbd_macro_buffer));
}

DEFUN ("end-kbd-macro", Fend_kbd_macro, Send_kbd_macro, 0, 2, "p",
       doc: /* Finish defining a keyboard macro.
The definition was started by \\[start-kbd-macro].
The macro is now available for use via \\[call-last-kbd-macro],
or it can be given a name with \\[name-last-kbd-macro] and then invoked
under that name.

With numeric arg, repeat macro now that many times,
counting the definition just completed as the first repetition.
An argument of zero means repeat until error.

In Lisp, optional second arg LOOPFUNC may be a function that is called prior to
each iteration of the macro.  Iteration stops if LOOPFUNC returns nil.  */)
  (Lisp_Object repeat, Lisp_Object loopfunc)
{
  if (NILP (KVAR (current_kboard, defining_kbd_macro)))
    error ("Not defining kbd macro");

  if (NILP (repeat))
    XSETFASTINT (repeat, 1);
  else
    CHECK_FIXNUM (repeat);

  if (!NILP (KVAR (current_kboard, defining_kbd_macro)))
    {
      end_kbd_macro ();
      message1 ("Keyboard macro defined");
    }

  if (XFIXNAT (repeat) == 0)
    Fexecute_kbd_macro (KVAR (current_kboard, Vlast_kbd_macro), repeat, loopfunc);
  else if (XFIXNUM (repeat) > 1)
    {
      XSETINT (repeat, XFIXNUM (repeat) - 1);
      Fexecute_kbd_macro (KVAR (current_kboard, Vlast_kbd_macro),
			  repeat, loopfunc);
    }
  return Qnil;
}

/* Store character c into kbd macro being defined.  */

void
store_kbd_macro_char (Lisp_Object c)
{
  struct kboard *kb = current_kboard;

  if (!NILP (KVAR (kb, defining_kbd_macro)))
    {
      if (kb->kbd_macro_ptr - kb->kbd_macro_buffer == kb->kbd_macro_bufsize)
	{
	  ptrdiff_t ptr_offset = kb->kbd_macro_ptr - kb->kbd_macro_buffer;
	  ptrdiff_t end_offset = kb->kbd_macro_end - kb->kbd_macro_buffer;
#ifdef HAVE_MPS
	  kb->kbd_macro_buffer
	    = igc_xpalloc_lisp_objs_exact (kb->kbd_macro_buffer,
					   &kb->kbd_macro_bufsize,
					   1, -1, "kbd-macro-buffer");
#else
	  kb->kbd_macro_buffer = xpalloc (kb->kbd_macro_buffer,
					  &kb->kbd_macro_bufsize,
					  1, -1, sizeof *kb->kbd_macro_buffer);
#endif
	  kb->kbd_macro_ptr = kb->kbd_macro_buffer + ptr_offset;
	  kb->kbd_macro_end = kb->kbd_macro_buffer + end_offset;
	}

      *kb->kbd_macro_ptr++ = c;
    }
}

/* Declare that all chars stored so far in the kbd macro being defined
 really belong to it.  This is done in between editor commands.  */

void
finalize_kbd_macro_chars (void)
{
  current_kboard->kbd_macro_end = current_kboard->kbd_macro_ptr;
}

DEFUN ("cancel-kbd-macro-events", Fcancel_kbd_macro_events,
       Scancel_kbd_macro_events, 0, 0, 0,
       doc: /* Cancel the events added to a keyboard macro for this command.  */)
  (void)
{
  current_kboard->kbd_macro_ptr = current_kboard->kbd_macro_end;
  return Qnil;
}

DEFUN ("store-kbd-macro-event", Fstore_kbd_macro_event,
       Sstore_kbd_macro_event, 1, 1, 0,
       doc: /* Store EVENT into the keyboard macro being defined.  */)
  (Lisp_Object event)
{
  store_kbd_macro_char (event);
  return Qnil;
}

DEFUN ("call-last-kbd-macro", Fcall_last_kbd_macro, Scall_last_kbd_macro,
       0, 2, "p",
       doc: /* Call the last keyboard macro that you defined with \\[start-kbd-macro].

A prefix argument serves as a repeat count.  Zero means repeat until error.

To make a macro permanent so you can call it even after
defining others, use \\[name-last-kbd-macro].

In Lisp, optional second arg LOOPFUNC may be a function that is called prior to
each iteration of the macro.  Iteration stops if LOOPFUNC returns nil.  */)
  (Lisp_Object prefix, Lisp_Object loopfunc)
{
  /* Don't interfere with recognition of the previous command
     from before this macro started.  */
  Vthis_command = KVAR (current_kboard, Vlast_command);
  /* C-x z after the macro should repeat the macro.  */
  Vreal_this_command = KVAR (current_kboard, Vlast_kbd_macro);

  if (! NILP (KVAR (current_kboard, defining_kbd_macro)))
    error ("Can't execute anonymous macro while defining one");
  else if (NILP (KVAR (current_kboard, Vlast_kbd_macro)))
    error ("No kbd macro has been defined");
  else
    Fexecute_kbd_macro (KVAR (current_kboard, Vlast_kbd_macro), prefix, loopfunc);

  /* command_loop_1 sets this to nil before it returns;
     get back the last command within the macro
     so that it can be last, again, after we return.  */
  Vthis_command = KVAR (current_kboard, Vlast_command);

  return Qnil;
}

/* Restore Vexecuting_kbd_macro and executing_kbd_macro_index.
   Called when the unwind-protect in Fexecute_kbd_macro gets invoked.  */

static void
pop_kbd_macro (Lisp_Object info)
{
  Lisp_Object tem;
  Vexecuting_kbd_macro = XCAR (info);
  tem = XCDR (info);
  integer_to_intmax (XCAR (tem), &executing_kbd_macro_index);
  Vreal_this_command = XCDR (tem);
  run_hook (Qkbd_macro_termination_hook);
}

DEFUN ("execute-kbd-macro", Fexecute_kbd_macro, Sexecute_kbd_macro, 1, 3, 0,
       doc: /* Execute MACRO as a sequence of events.
If MACRO is a string or vector, then the events in it are executed
exactly as if they had been input by the user.

If MACRO is a symbol, its function definition is used.  If that is
another symbol, this process repeats.  Eventually the result should be
a string or vector.  If the result is not a symbol, string, or vector,
an error is signaled.

COUNT is a repeat count, or nil for once, or 0 for infinite loop.

Optional third arg LOOPFUNC may be a function that is called prior to
each iteration of the macro.  Iteration stops if LOOPFUNC returns nil.

The buffer shown in the currently selected window will be made the current
buffer before the macro is executed.  */)
  (Lisp_Object macro, Lisp_Object count, Lisp_Object loopfunc)
{
  Lisp_Object final;
  Lisp_Object tem;
  specpdl_ref pdlcount = SPECPDL_INDEX ();
  EMACS_INT repeat = 1;
  EMACS_INT success_count = 0;

  executing_kbd_macro_iterations = 0;

  if (!NILP (count))
    {
      count = Fprefix_numeric_value (count);
      repeat = XFIXNUM (count);
    }

  final = indirect_function (macro);
  if (!STRINGP (final) && !VECTORP (final))
    error ("Keyboard macros must be strings or vectors");

  tem = Fcons (Vexecuting_kbd_macro,
	       Fcons (make_int (executing_kbd_macro_index),
		      Vreal_this_command));
  record_unwind_protect (pop_kbd_macro, tem);

  /* The following loop starts the execution of possibly multiple
     iterations of the macro.

     The state variables that control the execution of a single
     iteration are Vexecuting_kbd_macro and executing_kbd_macro_index,
     which can be accessed from lisp. The purpose of the variables
     executing_kbd_macro and executing_kbd_macro_iteration is to
     remember the most recently started macro and its iteration count.
     This makes it possible to produce a meaningful message in case of
     errors during the execution of the macro.

     In a single iteration, individual characters from the macro are
     read by read_char, which takes care of incrementing
     executing_kbd_macro_index after each character.

     The end of a macro iteration is handled as follows:
      - read_key_sequence asks at_end_of_macro_p whether the end of the
        iteration has been reached.  If so, it returns the magic value 0
        to command_loop_1.
      - command_loop_1 returns Qnil to command_loop_2.
      - command_loop_2 returns Qnil to this function
        (but only the returning is relevant, not the actual value).

     Macro executions form a stack.  After the last iteration of the
     execution of one stack item, or in case of an error during one of
     the iterations, pop_kbd_macro (invoked via unwind-protect) will
     restore Vexecuting_kbd_macro and executing_kbd_macro_index, and
     run 'kbd-macro-termination-hook'.

     If read_char happens to be called at the end of a macro iteration,
     but before read_key_sequence could handle the end (e.g., when lisp
     code calls 'read-event', 'read-char', or 'read-char-exclusive'),
     read_char will simply continue reading other available input
     (Bug#68272).  Vexecuting_kbd_macro and executing_kbd_macro remain
     untouched until the end of the iteration is handled.

     This is similar (in observable behavior) to a possibly simpler
     implementation of keyboard macros in which this function pushed all
     characters of the macro into the incoming event queue and returned
     immediately.  Maybe this is the implementation that we ideally
     would like to have, but switching to it will require a larger code
     change.  */
  do
    {
      Vexecuting_kbd_macro = final;
      executing_kbd_macro = final;
      executing_kbd_macro_index = 0;

      kset_prefix_arg (current_kboard, Qnil);

      if (!NILP (loopfunc))
	{
	  Lisp_Object cont;
	  cont = call0 (loopfunc);
	  if (NILP (cont))
	    break;
	}

      command_loop_2 (list1 (Qminibuffer_quit));

      executing_kbd_macro_iterations = ++success_count;

      maybe_quit ();
    }
  while (--repeat
	 && (STRINGP (Vexecuting_kbd_macro) || VECTORP (Vexecuting_kbd_macro)));

  executing_kbd_macro = Qnil;

  Vreal_this_command = Vexecuting_kbd_macro;

  return unbind_to (pdlcount, Qnil);
}

void
init_macros (void)
{
  Vexecuting_kbd_macro = Qnil;
  executing_kbd_macro = Qnil;
}

/* Whether the execution of a macro has reached its end.
   This should be called only while executing a macro.  */

bool
at_end_of_macro_p (void)
{
  eassume (!NILP (Vexecuting_kbd_macro));
  /* Some things replace the macro with t to force an early exit.  */
  return EQ (Vexecuting_kbd_macro, Qt)
    || executing_kbd_macro_index >= XFIXNAT (Flength (Vexecuting_kbd_macro));
}

void
syms_of_macros (void)
{
  DEFVAR_LISP ("kbd-macro-termination-hook", Vkbd_macro_termination_hook,
               doc: /* Normal hook run whenever a keyboard macro terminates.
This is run whether the macro ends normally or prematurely due to an error.  */);
  Vkbd_macro_termination_hook = Qnil;
  DEFSYM (Qkbd_macro_termination_hook, "kbd-macro-termination-hook");

  defsubr (&Sstart_kbd_macro);
  defsubr (&Send_kbd_macro);
  defsubr (&Scall_last_kbd_macro);
  defsubr (&Sexecute_kbd_macro);
  defsubr (&Scancel_kbd_macro_events);
  defsubr (&Sstore_kbd_macro_event);

  DEFVAR_KBOARD ("defining-kbd-macro", defining_kbd_macro,
		 doc: /* Non-nil while a keyboard macro is being defined.  Don't set this!
The value is the symbol `append' while appending to the definition of
an existing macro.  */);

  DEFVAR_LISP ("executing-kbd-macro", Vexecuting_kbd_macro,
	       doc: /* Currently executing keyboard macro (string or vector).
This is nil when not executing a keyboard macro.  */);

  DEFVAR_INT ("executing-kbd-macro-index", executing_kbd_macro_index,
	      doc: /* Index in currently executing keyboard macro; undefined if none executing.  */);

  DEFVAR_KBOARD ("last-kbd-macro", Vlast_kbd_macro,
		 doc: /* Last kbd macro defined, as a string or vector; nil if none defined.  */);
}
