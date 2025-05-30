# emacs-mac

This is an experimental build of the [emacs-mac](https://bitbucket.org/mituharu/emacs-mac) (aka Carbon[^1] Emacs) port of emacs, updated for Emacs v30.1.

> [!WARNING]
> This is an experimental build of `emacs-mac`; there will certainly be bugs. We are looking for feedback and testing from experienced users.  If you are familiar with or willing to learn about running new builds of Emacs under a debugger, perfect.  If you are a Mac developer familiar with ObjC or Mac Window frameworks, even better!  Other users should stick to the official NS build or recent v29.4 emacs-mac release for now.

## Status

Known working systems:

- MacOS 15 (Sequoia) on ARM64 (M1, M2, M3, M4), X86_64 (Intel)
- MacOS 14 (Sonoma) on ARM64 (M1, M3)
- MacOS 12 (Monterey) on X86_64 (Intel)

Please see the [issues](../../issues) for advice on build configurations for your system.

>[!IMPORTANT]
> Please open an [issue](../../issues) to report your experiences, even if you encounter no problems.  Mention your OS version, CPU, and any other relevant details, including the build/configure flags you used.

This build is kept current with the [`emacs-30`](https://github.com/emacs-mirror/emacs/tree/emacs-30) release branch.

## Install

See the `emacs-mac-30_1_exp` branch and the file `README-mac` for compile instructions.  Briefly:

> [!NOTE]
> On Apple, `gcc` is actually aliased to the `clang` compiler, which is required to build `emacs-mac`, as `gcc` either cannot build for the architecture (Apple Silicon) or does not support [blocks](https://en.wikipedia.org/wiki/Blocks_(C_language_extension)), which this build uses heavily.


### Install (optional) libraries

If you'd like to build with tree-sitter support, native-compilation, and RSVG (all recommended), first install the necessary libraries:

```bash
brew install tree-sitter libgccjit librsvg
```

### Compile and install

```bash
./autogen.sh
CFLAGS="-O3 -mcpu=native" ./configure --with-native-compilation --with-tree-sitter --enable-mac-app=yes  # tune config options to your liking
make
sudo make install  # optional, compresses EL files and installs some resources in /usr/local/share/emacs/30.1.50
```

You'll find the app under `mac`.

If you choose not to `make install`, you may need to:

```
% cd mac
% ln -s ../native-lisp Emacs.app/Contents/
```

to associate the native lisp files.

## Additions

Additional features/fixes added on top of `emacs-mac` and Emacs proper:

### Features

- A `New Frame` Dock Menu Item

### Bug fixes

- Take care to avoid crashes when selecting certain fonts from the system font panel.
- Prevent zombie "Emacs Web Content" processes [on SVG load](../../issues/9), ~~restoring normal WebView SVG rendering for MacOS v14+~~.  Update: `WebView` is deprecated, so this has been reverted and another workaround installed. It's recommended to build with RSVG (it is enabled by default if the `librsvg2` library is found during build).

## Debugging

If you get crashes or just want to help with debugging, it would be very useful to run emacs-mac under `lldb`, the clang debugger.  Here's how:

1. Build emacs-mac with debug flags:
   ```
    CFLAGS="-O0 -g3" ./configure --with-native-compilation --with-tree-sitter --enable-mac-app=yes
    ```
2.  In an `~/.lldbinit` file, add `settings set target.load-cwd-lldbinit true`, so Emacs can read the custom lldb commands it has defined.
3.  Start the emacs binary from the `src/` directory, like:
    ```bash
    %lldb ../mac/Emacs.app/Contents/MacOS/Emacs
    ```
    Then `run` (or better, `run -Q`).
1. Now cause your crash to occur, go `up` to the frame of interest, and use `xprint`, `p`, etc. on the potentially problematic variables.
2. You can also try `gui` which is a little curses-based terminal GUI inside lldb (slow for me though).

## Notes

You can read about the issues encountered during the merge of Emacs v30 in the [debugging notes](https://github.com/jdtsmith/emacs-mac/blob/emacs-mac-30_1_exp/devel_update_notes.org).

[^1]: Calling this the "Carbon" port is a vestigial nod to its origins back in the pre-OSX days. It is also what `M-x emacs-version` says.  But Carbon is a misnomer now.  The ancient Carbon API never supported 64bit applications, and was deprecated and removed by Apple in 2019.  A few convenience functions do remain (e.g. `Carbon.h`), and these are used by the NS build as well.  **Both NS and emacs-mac are Cocoa applications**.
