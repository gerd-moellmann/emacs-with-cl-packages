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
* The new `obarray` type implemented in C

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

So, why do this?  Because I can. Some people like to tinker with their
init files, others go a step further...

## New GC

This is a new GC that is based on Ravenbrook MPS.

I ported this to the mainline GNU/Emacs (branch scratch/igc on savannah)
and it wsa developed further there and here. Pip Cet has taken over
further development at some point because I didn't want to do it
anymore.

Works quite well for me on macOS. I'm using this daily.

## Child frames on ttys

The NS port of Emacs shows severe stability problems for me so that I
finally stopped building with or using NS. In other words, I'm using
Emacs on a terminal, more specifically iTerm with `kkp.el`.

Since I was a user of Posframe with Vertico and Transient on GUI Emacs,
I wanted that on the terminal as well.

This is work in progress. It still has some problem, but it can already
be used.

Some things have to be added to one's init file to convince Posframe an
Corfu that they can use child frames. As of 2024-10-10 I use

```
(defun posframe-workable-p ()
  "Test posframe workable status."
  (and (>= emacs-major-version 26)
       (not (or noninteractive
                emacs-basic-display
                (or (featurep 'tty-child-frames)
                    (not (display-graphic-p)))
                (eq (frame-parameter (selected-frame) 'minibuffer) 'only)))))))

(cl-defgeneric corfu--popup-support-p ()
  "Return non-nil if child frames are supported."
  (featurep 'tty-child-frames))

(push '(tty-cursor . selected-frame) corfu--frame-parameters)
(unless (display-graphic-p)
  (setq vertico-posframe-parameters '((no-special-glyphs . t)
    				(undecorated . nil))))
(unless (display-graphic-p)
  (push '(tty-cursor . nil) transient-posframe-parameters)
  (push '(undecorated . nil) transient-posframe-parameters))

```

But it's work in progress, This might change at any point.