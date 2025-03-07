# emacs-mac

This is an experimental build of the [emacs-mac](https://bitbucket.org/mituharu/emacs-mac) (aka Carbon Emacs, Emacs-mac) port of emacs, updated for Emacs v30.1.  

> [!WARNING]
> This is an experimental build of `emacs-mac`; there will certainly be bugs. Looking for feedback and testing from experienced users.  If you are familiar with running new builds of Emacs under a debugger, perfect.  If you are a Mac developer familiar with ObjC or Mac Window frameworks, even better!  

## Install

See the `emacs-mac-30_1_exp` branch and the file `README-mac` for compile instructions.  Briefly:

```bash
./autogen.sh
./configure --with-native-compilation --with-tree-sitter --with-rsvg --enable-mac-app=yes --without-imagemagic  # or whatever config options you use
make
sudo make-install  # optional, compresses EL files and installs some resources in /usr/local/share/emacs/30.1.50
```

Note that, as usual, you sometimes need to:

```
~/code/emacs/emacs-mac/mac
% ln -s ../native-lisp Emacs.app/Contents/
```

to associate the native lisp files.

## 

