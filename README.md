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
* Support for purespace
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

This is an incremental and concurrent GC that is based on Ravenbrook
MPS.

I ported this to the mainline GNU/Emacs (branch scratch/igc on savannah)
and it wsa developed further there and here. Pip Cet has taken over
further development at some point because I don't want to maintain this
(or anything else) in mainline Emacs.

Works well for me on macOS. I'm using this daily.

## Child frames on ttys

The NS port of Emacs shows severe stability problems for me so that I
finally stopped building with or using NS. In other words, I'm using
Emacs on a terminal, more specifically iTerm2 with `kkp.el`.

Since I was a user of Corfu, and Posframe with Vertico, Consult, and
Transient on GUI Emacs, I wanted that on the terminal as well, and here
we are.

To use this, make sure to use up-to-date Corfu and Posframe
packages. Both packages now check for the presence of the
`tty-child-frames` feature and use child frames on ttys.

You might also want to add these lines to make things look nicer.

```
(push '(tty-non-selected-cursor . t) vertico-posframe-parameters)
(push '(undecorated . nil) vertico-posframe-parameters))
(push '(undecorated . nil) transient-posframe-parameters))
```

Sorry, can't tell what to do for other packages, like maybe Company,
because I'm not using them.

A port of this to plain Emacs is available from savannah, branch
`scratch/tty-child-frames`.

If you want something like tooltips on ttys, take a look at tty-tip.el
in this repo.
