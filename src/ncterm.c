#include <config.h>
#include "lisp.h"
#include "ncterm.h"
#include "termhooks.h"
#include <notcurses/notcurses.h>
#include <stdlib.h>

static void
ncterm_set_terminal_hooks (struct terminal *t)
{
}

static struct terminal *
find_or_make_terminal (const char *name)
{
  struct terminal *t = get_named_terminal (name);
  if (t)
    return t;

  t = create_terminal (output_ncterm, NULL);
  struct ncterm_display_info *dpyinfo = xzalloc (sizeof *dpyinfo);
  t->display_info.ncterm = dpyinfo;
  dpyinfo->terminal = t;
  ncterm_set_terminal_hooks (t);
  return t;
}

struct terminal *
ncterm_init_terminal (const char *name, const char *terminal_type, bool must_succeed)
{
  if (!terminal_type && must_succeed)
    fatal ("Unknown terminal type");

  struct terminal *t = find_or_make_terminal (name);
  struct ncterm_display_info *dpyinfo = t->display_info.ncterm;

  struct notcurses_options opts =
  {
    .termtype = NULL,
    .loglevel = NCLOGLEVEL_TRACE,
    .margin_l = 0, .margin_r = 0, .margin_t = 0, .margin_b = 0,
    .flags = NCOPTION_INHIBIT_SETLOCALE,
  };
  dpyinfo->nc = notcurses_init (&opts, NULL);
  if (dpyinfo->nc == NULL)
    fatal ("Cannot notcurses_init");

#if 0
  {
    /* Open the terminal device.  */

    /* If !ctty, don't recognize it as our controlling terminal, and
       don't make it the controlling tty if we don't have one now.

       Alas, O_IGNORE_CTTY is a GNU extension that seems to be only
       defined on Hurd.  On other systems, we need to explicitly
       dissociate ourselves from the controlling tty when we want to
       open a frame on the same terminal.  */
    int flags = O_RDWR | O_NOCTTY | (ctty ? 0 : O_IGNORE_CTTY);
    int fd = emacs_open (name, flags, 0);
    tty->input = tty->output
      = ((fd < 0 || ! isatty (fd))
	 ? NULL
	 : emacs_fdopen (fd, "w+"));

    if (! tty->input)
      {
	char const *diagnostic
	  = (fd < 0) ? "Could not open file: %s" : "Not a tty device: %s";
	emacs_close (fd);
        delete_terminal_internal (terminal);
	maybe_fatal (must_succeed, terminal, diagnostic, diagnostic, name);
      }

    tty->name = xstrdup (name);
    terminal->name = xstrdup (name);

    if (!O_IGNORE_CTTY && !ctty)
      dissociate_if_controlling_tty (fd);
  }

  tty->type = xstrdup (terminal_type);

  add_keyboard_wait_descriptor (fileno (tty->input));

  Wcm_clear (tty);

  /* On some systems, tgetent tries to access the controlling
     terminal.  */
  block_tty_out_signal (&oldset);
  status = tgetent (tty->termcap_term_buffer, terminal_type);
  if (tty->termcap_term_buffer[TERMCAP_BUFFER_SIZE - 1])
    emacs_abort ();
  unblock_tty_out_signal (&oldset);

  if (status < 0)
    {
#ifdef TERMINFO
      maybe_fatal (must_succeed, terminal,
                   "Cannot open terminfo database file",
                   "Cannot open terminfo database file");
#else
      maybe_fatal (must_succeed, terminal,
                   "Cannot open termcap database file",
                   "Cannot open termcap database file");
#endif
    }
  if (status == 0)
    {
      maybe_fatal (must_succeed, terminal,
                   "Terminal type %s is not defined",
                   "Terminal type %s is not defined.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command 'TERM=...; export TERM' (C-shell:\n\
'setenv TERM ...') to specify the correct type.  It may be necessary\n"
#ifdef TERMINFO
"to do 'unset TERMINFO' (C-shell: 'unsetenv TERMINFO') as well.",
#else
"to do 'unset TERMCAP' (C-shell: 'unsetenv TERMCAP') as well.",
#endif
                   terminal_type);
    }
#endif
  return t;
}

void
init_ncterm (void)
{
#if 0
  struct notcurses_options opts =
  {
    .termtype = NULL,
    .loglevel = NCLOGLEVEL_TRACE,
    .margin_l = 0, .margin_r = 0, .margin_t = 0, .margin_b = 0,
    .flags = NCOPTION_INHIBIT_SETLOCALE,
  };
  nc = notcurses_init (&opts, NULL);
  if (nc == NULL)
    fatal ("");
  atexit (ncterm_atexit);
#endif
}

void
syms_of_ncterm (void)
{
  DEFSYM (Qnotcurses, "notcurses");
  Fprovide (Qnotcurses, Qnil);
}
