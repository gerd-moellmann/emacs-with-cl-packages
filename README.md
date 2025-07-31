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
macOS. Development is ongoing in a very slow pace.

## TTY menus in Lisp

This is an implementation of TTY menus that is entirely written in Emacs
Lisp. See lisp/tm.el. That file can also be used with a current master
from savannah. Use `M-x tm-menu-mode` to activate. See the doc string of
that function. The menu's behavior is patterned after what macOS does.

It's unclear at the moment if that will land in GNU because I haven't
offered it for inclusion, and reason for that is that I'm not interested
in the inevitable discussions about the macOS menu behavior and whatnot.

## Markers, Text indices, and marker vectors

Emacs internal text encoding is an extended UTF-8. Characters can be
between 1 and 5 bytes long in this encoding. Such a variable-length
representation requires a conversion between character and byte
positions.

This position conversion is currently sped up in Emacs by consulting the
doubly-linked list of markers that each buffer has. Each marker contains
a character and a byte position. A heuristic is used to pick a suitable
marker from whose known position one can scan forward or backward in the
buffer text to convert character to byte positions and vice versa.

This has some problems:

- Adding a marker is O(1), but deleting a marker is O(N).

- Iteration over markers to update them when text is inserted/deleted
  accesses memory all over the place, the marker objects.

- A possibly large number of "cache marker" are produced to make it more
  likely that the heuristic finds suitable markers.

- The heuristic doesn't really work in some use cases.

What I've done here is:

- Add text indices: A text index is a data structure which supports such
position conversions with predictable performance and without relying on
markers and heuristics. The implementation can be found in
`text-index.c`. Please read the comment at the start of that file for
details.

- Add marker vectors: I had already changed the doubly-linked list of
markers to use marker vectors in igc. This is now ported to the non-igc
case, so that both can use a common implementation. (Please note that
`feature/igc` still uses the old implementation. It will use the new one
should this land in master.)

- Remove positions from markers: Store the character position of a marker
in the marker vector and compute the byte position as needed using text
indices. This allows position adjustments when text changes t be done by
iterating over the marker vector without touching other memory.

### Status

I am using this on a daily basis.

There is also a branch `scratch/text-index` on savannah where I ported
this to master.  If that lands in master or when is unclear.  Stefan
Monnier seems interested, and has run some benchmarks that look
good. One notorious case was sped up by 2 orders of magnitude in an
early version.

## Emacs Mac Pprt

The branch `cl-packages-mac` is merged with
https://github.com/jdtsmith/emacs-mac. It builds and runs the mac port,
but only without igc. IOW, igc has not been ported to the mac front
end. I have no concrete plans if or when to port igc to mac.

It builds NS as before, with and without igc.

Caveat: It is not 100% clear if the mac Lisp changes interfere with
NS.
