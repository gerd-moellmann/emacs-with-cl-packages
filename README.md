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

## What's not there/not supported

* Documentation
* Tests
* Support for symbol shortcuts

## Common Lisp packages

### What's there

You can use packages much like in Common Lisp. For details, read
`lisp/emacs-lisp/pkg.el` and `src/pkg.c`, and maybe
`admin/cl-packages.org`, although I haven't kept that up-to-date for
some time, I guess.

### What's the plan?

None. I don't think CL packages will land in Emacs in my lifetime.
The resistance against this, or anything CL for that matter, is simply
too high among current Emacs maintainers.

So, why do this? Because I can. Some people like to tinker with their
init files, others go a step further :-).

## New GC

This is an incremental and generational GC that is based on Ravenbrook
MPS.

I ported this to mainline Emacs (branch feature/igc on
`savannah.gnu.org`) and it was developed further there and here.  At the
moment the future of this branch is unclear.

Works well for me on macOS. I'm using this daily.

## Child frames on ttys

This has meanwhile landed in master on savannah.

## DTrace support

My Emacs is now beginning to be a DTrace USDT provider. Configure
`--with-dtrace=no` to disable it. This is only tested on
macOS. Development is ongoing in slow pace.

## TTY menus in Lisp

This is an implementation of TTY menus that is entirely written in Emacs
Lisp. See lisp/tm.el. That file can also be used with a current master
from savannah. Use `M-x tm-menu-mode` to activate. See the doc string of
that function. The menu's behavior is patterned after what macOS does.

It's unclear at the moment if that will land in GNU.
