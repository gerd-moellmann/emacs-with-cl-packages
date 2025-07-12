// -*- c-file-style: "webkit" -*-

#include "ncterm.h"
#include "blockinput.h"
#include "lisp.h"
#include "termhooks.h"
#include <config.h>
#include <notcurses/notcurses.h>
#include <stdlib.h>

static void delete_terminal_hook(struct terminal* t) {
    block_input();
    struct ncterm_display_info* dpyinfo = t->display_info.ncterm;
    if (dpyinfo->nc)
        notcurses_stop(dpyinfo->nc);
    xfree((char*)dpyinfo->name);
    xfree(dpyinfo);
    t->display_info.ncterm = NULL;
    unblock_input();
}

static void delete_frame_hook(struct frame* f) {
}

static void ncterm_set_terminal_hooks(struct terminal* t) {
    t->delete_terminal_hook = delete_terminal_hook;
    t->delete_frame_hook = delete_frame_hook;
}

/* Find an existing or create a new notcurses terminal named NAME. */

static struct terminal* find_or_make_terminal(const char* name) {
    struct terminal* t = get_named_terminal(name);
    if (t)
        return t;
    t = create_terminal(output_ncterm, NULL);
    struct ncterm_display_info* dpyinfo = xzalloc(sizeof *dpyinfo);
    dpyinfo->terminal = t;
    t->display_info.ncterm = dpyinfo;
    dpyinfo->name = xstrdup(name);
    t->name = xstrdup(name);
    ncterm_set_terminal_hooks(t);
    return t;
}

/* Initialize a notcurses terminal.  NAME is the name of the terminal.
   TERMINAL_TYPE is the type of terminal (think $TERM).  */

struct terminal* ncterm_init_terminal(const char* name, const char* terminal_type, bool must_succeed) {
    if (!terminal_type && must_succeed)
        fatal("Unknown terminal type");

    struct terminal* t = find_or_make_terminal(name);
    struct ncterm_display_info* dpyinfo = t->display_info.ncterm;

    dpyinfo->fd = emacs_open(name, O_RDWR | O_NOCTTY, 0);
    if (dpyinfo->fd < 0) {
        delete_terminal(t);
        fatal("Could not open file: %s", name);
    }
    dpyinfo->output = emacs_fdopen(dpyinfo->fd, "w+");
    dpyinfo->input = dpyinfo->output;
    add_keyboard_wait_descriptor(fileno(dpyinfo->input));

    struct notcurses_options opts = {
        .termtype = terminal_type,
        .loglevel = NCLOGLEVEL_TRACE,
        .margin_l = 0,
        .margin_r = 0,
        .margin_t = 0,
        .margin_b = 0,
        .flags = NCOPTION_INHIBIT_SETLOCALE,
    };
    dpyinfo->nc = notcurses_init(&opts, dpyinfo->output);
    if (dpyinfo->nc == NULL)
        fatal("Cannot notcurses_init");

    return t;
}

void init_ncterm(void) {
}

void syms_of_ncterm(void) {
    DEFSYM(Qnotcurses, "notcurses");
    Fprovide(Qnotcurses, Qnil);
}
