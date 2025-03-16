# emacs-mac

This is an experimental build of the [emacs-mac](https://bitbucket.org/mituharu/emacs-mac) (aka Carbon[^1] Emacs, Emacs-mac) port of emacs, updated for Emacs v30.1.  

> [!WARNING]
> This is an experimental build of `emacs-mac`; there will certainly be bugs. We are looking for feedback and testing from experienced users.  If you are familiar with or willing to learn about running new builds of Emacs under a debugger, perfect.  If you are a Mac developer familiar with ObjC or Mac Window frameworks, even better!  Other users should stick to the official NS build for now.

## Status

Working well:

- MacOS 15 on ARM64 (M2)

Compiling but crashing:

- MacOS 12 on X86_64 (Intel)

Please open an issue to discuss experiences on other systems.

## Install

See the `emacs-mac-30_1_exp` branch and the file `README-mac` for compile instructions.  Briefly:

```bash
./autogen.sh
CFLAGS="-O3 -mcpu=native" ./configure --with-native-compilation --with-tree-sitter --with-rsvg --enable-mac-app=yes --without-imagemagic  # or whatever config options you use
make
sudo make-install  # optional, compresses EL files and installs some resources in /usr/local/share/emacs/30.1.50
```

You'll find the app under `mac`.

Note that, as usual, you sometimes need to:

```
~/code/emacs/emacs-mac/mac
% ln -s ../native-lisp Emacs.app/Contents/
```

to associate the native lisp files.

>[!IMPORTANT]
> Please open an issue to report your experiences, even if you encounter no problems.  Mention your OS version, CPU, and any other relevant details.


## Debugging

If you get crashes or just want to help with debugging, it would be useful to run emacs under `lldb`.  Here's how:

1. Build emacs-mac with debug flags:
   ```
    CFLAGS="-O0 -g3" ./configure --with-native-compilation --with-tree-sitter --with-rsvg --enable-mac-app=yes --without-imagemagick
    ```
2.  In an `~/.lldbinit` file, add `settings set target.load-cwd-lldbinit true`, so Emacs can read the custom lldb commands it has defined.
3.  Start the emacs binary from the `src/` directory, like:
    ```bash
    %lldb ../mac/Emacs.app/Contents/MacOS/Emacs
    ```
    Then `run` (or better, `run -Q`).
1. Now cause your crash to occur, go `up` to the frame of interest, and use `xprint` on the potentially problematic variables.
2. You can also try `gui` which is a little curses-based terminal GUI inside lldb (slow for me though).

## Additions

Additional features added on top of `emacs-mac` and Emacs proper:

- `New Frame` Dock Menu Item

## Notes

You can read about the issues encountered during the merge of Emacs v30 in the [debugging notes](https://github.com/jdtsmith/emacs-mac/blob/emacs-mac-30_1_exp/devel_update_notes.org).

[^1]: Calling this the "Carbon" port is a vestigial nod to its origins back in the pre-OSX days. It is also what `M-x emacs-version` says.  But Carbon is a misnomer now.  The ancient Carbon API never supported 64bit applications, was deprecated and removed by Apple in 2019.  A few convenience functions do remain (e.g. `Carbon.h`), and these are used by the NS build as well.  **Both NS and emacs-mac** are Cocoa applications.
