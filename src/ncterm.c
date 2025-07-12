#include <config.h>
#include "lisp.h"
#include "ncterm.h"
#include <notcurses/notcurses.h>
#include <stdlib.h>

static struct notcurses *nc = NULL;

static void
ncterm_atexit (void)
{
  notcurses_stop (nc);
}

void
init_ncterm (void)
{
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
}

void
syms_of_ncterm (void)
{
  DEFSYM (Qnotcurses, "notcurses");
  Fprovide (Qnotcurses, Qnil);
}
