## What is it?

It is my fork of Emacs, if you will.

This is Emacs' master branch, with stuff added (and some stuff removed,
which I was too lazy to support).

I am using this on a daily basis on macOS with an init file that I can
also use with an unchanged master. I make no efforts to find or fix
bugs in stuff that I don't use personally.

My init file uses `use-package` and `straight`, and I am using
packages like `vertico`, `consult`, `corfu`, `magit`, `org`, `gnus`
etc., so I'd say it's pretty backwards compatible.

## Who's that?

`C-h i d m emacs RET s gerd RET`

Former head maintainer for Emacs 21 who didn't use Emacs for 2ß years
after stepping down, and who doesn't consider himself a member of the
GNU/Emacs project today.

I don't follow `emacs-devel` or `emacs-bugs`.

## What's the plan?

None.

## Features

### Common Lisp packages (not ported)

You can use packages much like in Common Lisp. For details, read
`lisp/emacs-lisp/pkg.el` and `src/pkg.c`.

### Incremental and generational GC (ported)

GC based on Ravenbrook MPS, ported to mainstream Emacs in early 2024,
branch `feature/igc`.

### Child frames and tooltips on ttys (ported and landed)

Ported to mainstream Emacs and has landed in master.

### DTrace support (not ported)

DTrace USDT provider. Configure `--with-dtrace=no` to disable it. This
is only tested on macOS.

### TTY menus in Lisp (not ported)

See `lisp/tm.el`. Can also can be used with mainstream master.  Use `M-x
tm-menu-mode` to activate. See the doc string of that function. The
menu's behavior is patterned after what macOS does.

### Markers, Text indices, and marker vectors (ported)

A text index is a data structure which supports byte/character position
conversions with predictable performance.  See `text-index.c`.  A marker
vector is used as replacement for doubly-linked lists of markers, with
better performance. See `marker-vector.c`. Port found in
`scratch/text-index`.

## Emacs Mac Port (not ported)

I am using `mac` instead of `NS`. See
https://github.com/jdtsmith/emacs-mac, Also ported `igc` to `mac`.
