// -*- c-file-style: "webkit" -*-

#ifndef EMACS_NCTERM_H
#define EMACS_NCTERM_H

#include "config.h"
#include "lisp.h"

struct ncterm_display_info {
    struct terminal* terminal;
    struct notcurses* nc;
    const char* name;
    int fd;
    FILE* input;
    FILE* output;
};

void init_ncterm(void);
void syms_of_ncterm(void);
struct terminal* ncterm_init_terminal(const char* name, const char* terminal_type,
    bool must_succeed);

#endif
