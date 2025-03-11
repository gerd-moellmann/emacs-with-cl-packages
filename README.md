# emacs-mac

This is an experimental build of the [emacs-mac](https://bitbucket.org/mituharu/emacs-mac) (aka Carbon Emacs, Emacs-mac) port of emacs, updated for Emacs v30.1.  

> [!WARNING]
> This is an experimental build of `emacs-mac`; there will certainly be bugs. Looking for feedback and testing from experienced users.  If you are familiar with running new builds of Emacs under a debugger, perfect.  If you are a Mac developer familiar with ObjC or Mac Window frameworks, even better!  

## Install

See the `emacs-mac-30_1_exp` branch and the file `README-mac` for compile instructions.  Briefly:

```bash
./autogen.sh
CFLAGS="-O3 -mcpu=native" ./configure --with-native-compilation --with-tree-sitter --with-rsvg --enable-mac-app=yes --without-imagemagic  # or whatever config options you use
make
sudo make-install  # optional, compresses EL files and installs some resources in /usr/local/share/emacs/30.1.50
```

You'll find the app under `mac`

Note that, as usual, you sometimes need to:

```
~/code/emacs/emacs-mac/mac
% ln -s ../native-lisp Emacs.app/Contents/
```

to associate the native lisp files.

## Debugging

If you get crashes or just want to help with debugging, it would be useful to debug using `lldb`.  Here's how:

1. Build emacs-mac with debug flags:
   ```
    CFLAGS="-O0 -g3" ./configure --with-native-compilation --with-tree-sitter --with-rsvg --enable-mac-app=yes --without-imagemagick
    ```
2.  In an `~/.lldbinit` file, add `settings set target.load-cwd-lldbinit true`, so Emacs can read the custom lldb commands it has defined.
3.  Start the binary from the `src/` directory, like:
    ```bash
    %lldb ../mac/Emacs.app/Contents/MacOS/Emacs
    ```
    Then `run` or better, `run -Q`
1. Now you cause your crash to occur, go `up` to the frame of interest, and use `xprint` on the potentially problematic variables.
2. You can also try `gui` which is a little curses-based terminal GUI for lldb (slow for me though).

## Notes

You can read about the issues encountered in the [debugging notes](https://github.com/jdtsmith/emacs-mac/blob/emacs-mac-30_1_exp/devel_update_notes.org).
