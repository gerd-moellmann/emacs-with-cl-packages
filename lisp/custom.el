;;; custom.el --- tools for declaring and initializing options  -*- lexical-binding: t -*-
;;
;; Copyright (C) 1996-1997, 1999, 2001-2025 Free Software Foundation,
;; Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: help, faces
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file only contains the code needed to declare and initialize
;; user options.  The code to customize options is autoloaded from
;; `cus-edit.el' and is documented in the Emacs Lisp Reference manual.

;; The code implementing face declarations is in `cus-face.el'.

;;; Code:

(require 'widget)

(defvar custom-define-hook nil
  ;; Customize information for this option is in `cus-edit.el'.
  "Hook called after defining each customize option.")

(defvar custom-dont-initialize nil
  "Non-nil means `defcustom' should not initialize the variable.
That is used for the sake of `custom-make-dependencies'.
Users should not set it.")

(defvar custom-current-group-alist nil
  "Alist of (FILE . GROUP) indicating the current group to use for FILE.")

;;; The `defcustom' Macro.

(defun custom-initialize-default (symbol exp)
  "Initialize SYMBOL with EXP.
This will do nothing if symbol already has a default binding.
Otherwise, if symbol has a `saved-value' property, it will evaluate
the car of that and use it as the default binding for symbol.
Otherwise, EXP will be evaluated and used as the default binding for
symbol."
  (condition-case nil
      (default-toplevel-value symbol)   ;Test presence of default value.
    (void-variable
     ;; The var is not initialized yet.
     (set-default-toplevel-value
      symbol (eval (let ((sv (get symbol 'saved-value)))
                     (if sv (car sv) exp))
                   t)))))

(defun custom-initialize-set (symbol exp)
  "Initialize SYMBOL based on EXP.
If the symbol doesn't have a default binding already, then set it
using its `:set' function (or `set-default-toplevel-value' if it
has none).

The value is either the value in the symbol's `saved-value' property,
if any, or the value of EXP."
  (condition-case nil
      (default-toplevel-value symbol)
    (error
     (funcall (or (get symbol 'custom-set) #'set-default-toplevel-value)
              symbol
              (eval (let ((sv (get symbol 'saved-value)))
                      (if sv (car sv) exp)))))))

(declare-function widget-apply "wid-edit" (widget property &rest args))
(defun custom-initialize-reset (symbol exp)
  "Initialize SYMBOL based on EXP.
Set the symbol, using its `:set' function (or `set-default-toplevel-value'
if it has none).

The value is either the symbol's current value
 (as obtained using the `:get' function), if any,
or the value in the symbol's `saved-value' property if any,
or (last of all) the value of EXP."
  ;; If this value has been set with `setopt' (for instance in
  ;; ~/.emacs), we didn't necessarily know the type of the user option
  ;; then.  So check now, and issue a warning if it's wrong.
  (let ((value (get symbol 'custom-check-value)))
    (when value
      (let ((type (get symbol 'custom-type)))
        (when (and type
                   (boundp symbol)
                   (eq (car value) (symbol-value symbol))
                   ;; Check that the type is correct.
                   (not (widget-apply (widget-convert type)
                                      :match (car value))))
          (warn "Value `%S' for `%s' does not match type %s"
                value symbol type)))))
  (funcall (or (get symbol 'custom-set) #'set-default-toplevel-value)
           symbol
           (condition-case nil
               (let ((def (default-toplevel-value symbol))
                     (getter (get symbol 'custom-get)))
                 (if getter (funcall getter symbol) def))
             (error
              (eval (let ((sv (get symbol 'saved-value)))
                      (if sv (car sv) exp)))))))

(defun custom-initialize-changed (symbol exp)
  "Initialize SYMBOL with EXP.
Like `custom-initialize-reset', but only use the `:set' function if
not using the standard setting.
For the standard setting, use `set-default-toplevel-value'."
  (condition-case nil
      (let ((def (default-toplevel-value symbol)))
        (funcall (or (get symbol 'custom-set) #'set-default-toplevel-value)
                 symbol
                 (let ((getter (get symbol 'custom-get)))
                   (if getter (funcall getter symbol) def))))
    (error
     (cond
      ((get symbol 'saved-value)
       (funcall (or (get symbol 'custom-set) #'set-default-toplevel-value)
                symbol
                (eval (car (get symbol 'saved-value)))))
      (t
       (set-default-toplevel-value symbol (eval exp)))))))

(defvar custom-delayed-init-variables nil
  "List of variables whose initialization is pending until startup.
Once this list has been processed, this var is set to a non-list value.")

(defun custom-initialize-delay (symbol value)
  "Delay initialization of SYMBOL to the next Emacs start.
This is used in files that are preloaded (or for autoloaded
variables), so that the initialization is done in the run-time
context rather than the build-time context.  This also has the
side-effect that the (delayed) initialization is performed with
the :set function."
  ;; Defvar it so as to mark it special, etc (bug#25770).
  (internal--define-uninitialized-variable symbol)

  ;; Until the var is actually initialized, it is kept unbound.
  ;; This seemed to be at least as good as setting it to an arbitrary
  ;; value like nil (evaluating `value' is not an option because it
  ;; may have undesirable side-effects).
  (if (listp custom-delayed-init-variables)
      (push symbol custom-delayed-init-variables)
    ;; In case this is called after startup, there is no "later" to which to
    ;; delay it, so initialize it "normally" (bug#47072).
    (custom-initialize-reset symbol value)))

(defun custom-declare-variable (symbol default doc &rest args)
  "Like `defcustom', but SYMBOL and DEFAULT are evaluated as normal arguments.
DEFAULT should be an expression to evaluate to compute the default value,
not the default value itself.

DEFAULT is stored as SYMBOL's standard value, in SYMBOL's property
`standard-value'.  At the same time, SYMBOL's property `force-value' is
set to nil, as the value is no longer rogue."
  (put symbol 'standard-value (list default))
  ;; Maybe this option was rogue in an earlier version.  It no longer is.
  (when (get symbol 'force-value)
    (put symbol 'force-value nil))
  (if (keywordp doc)
      (error "Doc string is missing"))
  (let ((initialize #'custom-initialize-reset)
        (requests nil)
        ;; Whether automatically buffer-local.
        buffer-local)
    (unless (memq :group args)
      (let ((cg (custom-current-group)))
        (when cg
          (custom-add-to-group cg symbol 'custom-variable))))
    (while args
      (let ((keyword (pop args)))
	(unless (symbolp keyword)
	  (error "Junk in args %S" args))
        (unless args
          (error "Keyword %s is missing an argument" keyword))
	(let ((value (pop args)))
          ;; Can't use `pcase' because it is loaded after `custom.el'
          ;; during bootstrap.  See `loadup.el'.
	  (cond ((eq keyword :initialize)
		 (setq initialize value))
		((eq keyword :set)
		 (put symbol 'custom-set value))
		((eq keyword :get)
		 (put symbol 'custom-get value))
		((eq keyword :require)
		 (push value requests))
		((eq keyword :risky)
		 (put symbol 'risky-local-variable value))
		((eq keyword :safe)
		 (put symbol 'safe-local-variable value))
                ((eq keyword :local)
                 (when (memq value '(t permanent))
                   (setq buffer-local t))
                 (when (memq value '(permanent permanent-only))
                   (put symbol 'permanent-local t)))
		((eq keyword :type)
                 (put symbol 'custom-type value))
		((eq keyword :options)
		 (if (get symbol 'custom-options)
		     ;; Slow safe code to avoid duplicates.
		     (mapc (lambda (option)
			     (custom-add-option symbol option))
			   value)
		   ;; Fast code for the common case.
		   (put symbol 'custom-options (copy-sequence value))))
		(t
		 (custom-handle-keyword symbol keyword value
					'custom-variable))))))
    ;; Set the docstring, record the var on load-history, as well
    ;; as set the special-variable-p flag.
    (internal--define-uninitialized-variable symbol doc)
    (put symbol 'custom-requests requests)
    ;; Do the actual initialization.
    (unless custom-dont-initialize
      (funcall initialize symbol default)
      ;; If there is a value under saved-value that wasn't saved by the user,
      ;; reset it: we used that property to stash the value, but we don't need
      ;; it anymore.
      ;; This can happen given the following:
      ;; 1. The user loaded a theme that had a setting for an unbound
      ;; variable, so we stashed the theme setting under the saved-value
      ;; property in `custom-theme-recalc-variable'.
      ;; 2. Then, Emacs evaluated the defcustom for the option
      ;; (e.g., something required the file where the option is defined).
      ;; If we don't reset it and the user later sets this variable via
      ;; Customize, we might end up saving the theme setting in the custom-file.
      ;; See the test `custom-test-no-saved-value-after-customizing-option'.
      (let ((theme (caar (get symbol 'theme-value))))
        (when (and theme (not (eq theme 'user)) (get symbol 'saved-value))
          (put symbol 'saved-value nil))))
    (when buffer-local
      (make-variable-buffer-local symbol)))
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro defcustom (symbol standard doc &rest args)
  "Declare SYMBOL as a customizable variable.
SYMBOL is the variable name; it should not be quoted.
STANDARD is an expression specifying the variable's standard
value.  It should not be quoted.  It is evaluated once by
`defcustom', and the value is assigned to SYMBOL if the variable
is unbound.  The expression itself is also stored, so that
Customize can re-evaluate it later to get the standard value.
DOC is the variable documentation.

This macro uses `defvar' as a subroutine, which also marks the
variable as \"special\", so that it is always dynamically bound
even when `lexical-binding' is t.

The remaining arguments to `defcustom' should have the form

   [KEYWORD VALUE]...

The following keywords are meaningful:

:type	VALUE should be a widget type for editing the symbol's value.
	Every `defcustom' should specify a value for this keyword.
        See Info node `(elisp) Customization Types' for a list of
        base types and useful composite types.
:options VALUE should be a list of valid members of the widget type.
:initialize
	VALUE should be a function used to initialize the
	variable.  It takes two arguments, the symbol and value
	given in the `defcustom' call.  The default is
	`custom-initialize-reset'.
:set	VALUE should be a function to set the value of the symbol
	when using the Customize user interface.  It takes two arguments,
	the symbol to set and the value to give it.  The function should
	not modify its value argument destructively.  The default choice
	of function is `set-default-toplevel-value'.  If this keyword is
	defined, modifying the value of SYMBOL via `setopt' will call the
	function specified by VALUE to install the new value.
:get	VALUE should be a function to extract the value of symbol.
	The function takes one argument, a symbol, and should return
	the current value for that symbol.  The default choice of function
	is `default-toplevel-value'.
:require
	VALUE should be a feature symbol.  If you save a value
	for this option, then when your init file loads the value,
	it does (require VALUE) first.
:set-after VARIABLES
	Specifies that SYMBOL should be set after the list of variables
        VARIABLES when both have been customized.
:risky	Set SYMBOL's `risky-local-variable' property to VALUE.
:safe	Set SYMBOL's `safe-local-variable' property to VALUE.
        See Info node `(elisp) File Local Variables'.
:local  If VALUE is t, mark SYMBOL as automatically buffer-local.
        If VALUE is `permanent', also set SYMBOL's `permanent-local'
        property to t.
        If VALUE is `permanent-only', set SYMBOL's `permanent-local'
        property to t, but do not mark it as automatically buffer-local.

The following common keywords are also meaningful.

:group  VALUE should be a customization group.
        Add SYMBOL (or FACE with `defface') to that group instead of
        the default group.  Can be repeated.
:link LINK-DATA
        Include an external link after the documentation string for this
        item.  This is a sentence containing an active field which
        references some other documentation.

        There are several alternatives you can use for LINK-DATA:

        (custom-manual INFO-NODE)
             Link to an Info node; INFO-NODE is a string which specifies
             the node name, as in \"(emacs)Top\".

        (info-link INFO-NODE)
             Like `custom-manual' except that the link appears in the
             customization buffer with the Info node name.

        (url-link URL)
             Link to a web page; URL is a string which specifies the URL.

        (emacs-commentary-link LIBRARY)
             Link to the commentary section of LIBRARY.

        (emacs-library-link LIBRARY)
             Link to an Emacs Lisp LIBRARY file.

        (file-link FILE)
             Link to FILE.

        (function-link FUNCTION)
             Link to the documentation of FUNCTION.

        (variable-link VARIABLE)
             Link to the documentation of VARIABLE.

        (custom-group-link GROUP)
             Link to another customization GROUP.

        You can specify the text to use in the customization buffer by
        adding `:tag NAME' after the first element of the LINK-DATA; for
        example, (info-link :tag \"foo\" \"(emacs)Top\") makes a link to the
        Emacs manual which appears in the buffer as `foo'.

        An item can have more than one external link; however, most items
        have none at all.
:version
        VALUE should be a string specifying that the variable was
        first introduced, or its default value was changed, in Emacs
        version VERSION.
:package-version
        VALUE should be a list with the form (PACKAGE . VERSION)
        specifying that the variable was first introduced, or its
        default value was changed, in PACKAGE version VERSION.  This
        keyword takes priority over :version.  For packages which
        are bundled with Emacs releases, the PACKAGE and VERSION
        must appear in the alist `customize-package-emacs-version-alist'.
        Since PACKAGE must be unique and the user might see it in an
        error message, a good choice is the official name of the
        package, such as MH-E or Gnus.
:tag LABEL
        Use LABEL, a string, instead of the item's name, to label the item
        in customization menus and buffers.
:load FILE
        Load file FILE (a string) before displaying this customization
        item.  Loading is done with `load', and only if the file is
        not already loaded.

If SYMBOL has a local binding, then this form affects the local
binding.  This is normally not what you want.  Thus, if you need
to load a file defining variables with this form, or with
`defvar' or `defconst', you should always load that file
_outside_ any bindings for these variables.  (`defvar' and
`defconst' behave similarly in this respect.)

This macro calls `custom-declare-variable'.  If you want to
programmatically alter a customizable variable (for instance, to
write a package that extends the syntax of a variable), you can
call that function directly.

See Info node `(elisp) Customization' in the Emacs Lisp manual
for more information."
  (declare (doc-string 3) (debug (name body))
           (indent defun))
  `(custom-declare-variable
    ',symbol
    ,(if lexical-binding
         ;; The STANDARD arg should be an expression that evaluates to
         ;; the standard value.  The use of `eval' for it is spread
         ;; over many different places and hence difficult to
         ;; eliminate, yet we want to make sure that the `standard'
         ;; expression is checked by the byte-compiler, and that
         ;; lexical-binding is obeyed, so quote the expression with
         ;; `lambda' rather than with `quote'.
         ``(funcall #',(lambda () ,standard))
       `',standard)
    ,doc
    ,@args))

;;; The `defface' Macro.

(defmacro defface (face spec doc &rest args)
  "Declare FACE as a customizable face that defaults to SPEC.
FACE does not need to be quoted.

Third argument DOC is the face documentation.

If FACE has been set with `custom-theme-set-faces', set the face
attributes as specified by that function, otherwise set the face
attributes according to SPEC.

The remaining arguments should have the form [KEYWORD VALUE]...
For a list of valid keywords, see the common keywords listed in
`defcustom'.

SPEC should be a \"face spec\", i.e., an alist of the form

   ((DISPLAY . ATTS)...)

where DISPLAY is a form specifying conditions to match certain
terminals and ATTS is a property list (ATTR VALUE ATTR VALUE...)
specifying face attributes and values for frames on those
terminals.  On each terminal, the first element with a matching
DISPLAY specification takes effect, and the remaining elements in
SPEC are disregarded.

As a special exception, in the first element of SPEC, DISPLAY can
be the special value `default'.  Then the ATTS in that element
act as defaults for all the following elements.

For backward compatibility, elements of SPEC can be written
as (DISPLAY ATTS) instead of (DISPLAY . ATTS).

Each DISPLAY can have the following values:
 - `default' (only in the first element).
 - The symbol t, which matches all terminals.
 - An alist of conditions.  Each alist element must have the form
   (REQ ITEM...).  A matching terminal must satisfy each
   specified condition by matching one of its ITEMs.  Each REQ
   must be one of the following:
   - `type' (the terminal type).
     Each ITEM must be one of the values returned by
     `window-system'.  Under X, additional allowed values are
     `motif', `lucid', `gtk' and `x-toolkit'.
   - `class' (the terminal's color support).
     Each ITEM should be one of `color', `grayscale', or `mono'.
   - `background' (what color is used for the background text)
     Each ITEM should be one of `light' or `dark'.
   - `min-colors' (the minimum number of supported colors)
     Each ITEM should be an integer, which is compared with the
     result of `display-color-cells'.
   - `supports' (match terminals supporting certain attributes).
     Each ITEM should be a list of face attributes.  See
     `display-supports-face-attributes-p' for more information on
     exactly how testing is done.

In the ATTS property list, possible attributes are `:family', `:font',
`:foundry', `:width', `:height', `:weight', `:slant', `:underline',
`:overline', `:strike-through', `:box', `:foreground', `:distant-foreground',
`:background', `:stipple', `:inverse-video', `:extend', and `:inherit'.

See Info node `(elisp) Faces' in the Emacs Lisp manual for more
information."
  (declare (doc-string 3) (indent defun))
  ;; It is better not to use backquote in this file,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (nconc (list 'custom-declare-face (list 'quote face) spec doc) args))

;;; The `defgroup' Macro.

(defun custom-current-group ()
  (cdr (assoc load-file-name custom-current-group-alist)))

(defun custom-declare-group (symbol members doc &rest args)
  "Like `defgroup', but SYMBOL is evaluated as a normal argument."
  (while members
    (apply #'custom-add-to-group symbol (car members))
    (setq members (cdr members)))
  (when doc
    ;; This text doesn't get into DOC.
    (put symbol 'group-documentation doc))
  (while args
    (let ((arg (car args)))
      (setq args (cdr args))
      (unless (symbolp arg)
	(error "Junk in args %S" args))
      (let ((keyword arg)
	    (value (car args)))
	(unless args
	  (error "Keyword %s is missing an argument" keyword))
	(setq args (cdr args))
	(cond ((eq keyword :prefix)
               (put symbol 'custom-prefix value))
	      (t
	       (custom-handle-keyword symbol keyword value
				      'custom-group))))))
  ;; Record the group on the `current' list.
  (let ((elt (assoc load-file-name custom-current-group-alist)))
    (if elt (setcdr elt symbol)
      (push (cons load-file-name symbol) custom-current-group-alist)))
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro defgroup (symbol members doc &rest args)
  "Declare SYMBOL as a customization group containing MEMBERS.
SYMBOL does not need to be quoted.

Third argument DOC is the group documentation.  This should be a short
description of the group, beginning with a capital and ending with
a period.  Words other than the first should not be capitalized, if they
are not usually written so.

MEMBERS should be an alist of the form ((NAME WIDGET)...) where
NAME is a symbol and WIDGET is a widget for editing that symbol.
Useful widgets are `custom-variable' for editing variables,
`custom-face' for editing faces, and `custom-group' for editing groups.

The remaining arguments should have the form

   [KEYWORD VALUE]...

For a list of valid keywords, see the common keywords listed in
`defcustom'.  The keyword :prefix can only be used for
customization groups, and means that the given string should be
removed from variable names before creating unlispified names,
when the user option `custom-unlispify-remove-prefixes' is
non-nil.

See Info node `(elisp) Customization' in the Emacs Lisp manual
for more information."
  (declare (doc-string 3) (indent defun))
  ;; It is better not to use backquote in this file,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (nconc (list 'custom-declare-group (list 'quote symbol) members doc) args))

(defun custom-add-to-group (group option widget)
  "To existing GROUP add a new OPTION of type WIDGET.
If there already is an entry for OPTION and WIDGET, nothing is done."
  (let ((members (get group 'custom-group))
	(entry (list option widget)))
    (unless (member entry members)
      (put group 'custom-group (nconc members (list entry))))))

(defun custom-group-of-mode (mode)
  "Return the custom group corresponding to the major or minor MODE.
If no such group is found, return nil."
  (or (get mode 'custom-mode-group)
      (if (or (get mode 'custom-group)
	      (and (string-match "-mode\\'" (symbol-name mode))
		   (get (setq mode (intern (substring (symbol-name mode)
						      0 (match-beginning 0))))
			'custom-group)))
	  mode)))

;;; Properties.

(defun custom-handle-all-keywords (symbol args type)
  "For customization option SYMBOL, handle keyword arguments ARGS.
Third argument TYPE is the custom option type."
  (unless (memq :group args)
    (let ((cg (custom-current-group)))
      (when cg
        (custom-add-to-group cg symbol type))))
  (while args
    (let ((arg (car args)))
      (setq args (cdr args))
      (unless (symbolp arg)
	(error "Junk in args %S" args))
      (let ((keyword arg)
	    (value (car args)))
	(unless args
	  (error "Keyword %s is missing an argument" keyword))
	(setq args (cdr args))
	(custom-handle-keyword symbol keyword value type)))))

(defun custom-handle-keyword (symbol keyword value type)
  "For customization option SYMBOL, handle KEYWORD with VALUE.
Fourth argument TYPE is the custom option type."
  (cond ((eq keyword :group)
	 (custom-add-to-group value symbol type))
	((eq keyword :version)
	 (custom-add-version symbol value))
	((eq keyword :package-version)
	 (custom-add-package-version symbol value))
	((eq keyword :link)
	 (custom-add-link symbol value))
	((eq keyword :load)
	 (custom-add-load symbol value))
	((eq keyword :tag)
	 (put symbol 'custom-tag value))
	((eq keyword :set-after)
	 (custom-add-dependencies symbol value))
	(t
	 (error "Unknown keyword %s" keyword))))

(defun custom-add-dependencies (symbol value)
  "To the custom option SYMBOL, add dependencies specified by VALUE.
VALUE should be a list of symbols.  For each symbol in that list,
this specifies that SYMBOL should be set after the specified symbol,
if both appear in constructs like `custom-set-variables'."
  (unless (listp value)
    (error "Invalid custom dependency `%s'" value))
  (let* ((deps (get symbol 'custom-dependencies))
	 (new-deps deps))
    (while value
      (let ((dep (car value)))
	(unless (symbolp dep)
	  (error "Invalid custom dependency `%s'" dep))
	(unless (memq dep new-deps)
	  (setq new-deps (cons dep new-deps)))
	(setq value (cdr value))))
    (unless (eq deps new-deps)
      (put symbol 'custom-dependencies new-deps))))

(defun custom-add-option (symbol option)
  "To the variable SYMBOL add OPTION.

If SYMBOL's custom type is a hook, OPTION should be a hook member.
If SYMBOL's custom type is an alist, OPTION specifies a symbol
to offer to the user as a possible key in the alist.
For other custom types, this has no effect."
  (let ((options (get symbol 'custom-options)))
    (unless (member option options)
      (put symbol 'custom-options (cons option options)))))
(defalias 'custom-add-frequent-value #'custom-add-option)

(defun custom-add-link (symbol widget)
  "To the custom option SYMBOL add the link WIDGET."
  (let ((links (get symbol 'custom-links)))
    (unless (member widget links)
      (put symbol 'custom-links (cons widget links)))))

(defun custom-add-version (symbol version)
  "To the custom option SYMBOL add the version VERSION."
  (put symbol 'custom-version version))

(defun custom-add-package-version (symbol version)
  "To the custom option SYMBOL add the package version VERSION."
  (put symbol 'custom-package-version version))

(defun custom-add-load (symbol load)
  "To the custom option SYMBOL add the dependency LOAD.
LOAD should be either a library file name, or a feature name."
  (let ((loads (get symbol 'custom-loads)))
    (unless (member load loads)
      (put symbol 'custom-loads (cons load loads)))))

(defun custom-autoload (symbol load &optional noset)
  "Mark SYMBOL as autoloaded custom variable and add dependency LOAD.
If NOSET is non-nil, don't bother autoloading LOAD when setting the variable."
  (put symbol 'custom-autoload (if noset 'noset t))
  (custom-add-load symbol load))

(defun custom-variable-p (variable)
  "Return non-nil if VARIABLE is a customizable variable.
A customizable variable is either (i) a variable whose property
list contains a non-nil `standard-value' or `custom-autoload'
property, or (ii) an alias for another customizable variable."
  (declare (ftype (function (symbol) t))
           (side-effect-free t))
  (when (symbolp variable)
    (setq variable (indirect-variable variable))
    (or (get variable 'standard-value)
	(get variable 'custom-autoload))))

(defun custom--standard-value (variable)
  "Return the standard value of VARIABLE."
  (eval (car (get variable 'standard-value)) t))

(defun custom--standard-value-p (variable value)
  "Return non-nil if VALUE is `equal' to the standard value of VARIABLE."
  (let ((sv (get variable 'standard-value)))
    (and sv (equal value (eval (with-demoted-errors "%S" (car sv)) t)))))

(defun custom-note-var-changed (variable)
  "Inform Custom that VARIABLE has been set (changed).
VARIABLE is a symbol that names a user option.
The result is that the change is treated as having been made through Custom."
  (put variable 'customized-value (list (custom-quote (eval variable)))))

;; Loading files needed to customize a symbol.
;; This is in custom.el because menu-bar.el needs it for toggle cmds.

(defvar custom-load-recursion nil
  "Hack to avoid recursive dependencies.")

(defun custom-load-symbol (symbol)
  "Load all dependencies for SYMBOL."
  (unless custom-load-recursion
    (let ((custom-load-recursion t))
      ;; Load these files if not already done,
      ;; to make sure we know all the dependencies of SYMBOL.
      (ignore-errors
        (require 'cus-load))
      (ignore-errors
        (require 'cus-start))
      (dolist (load (get symbol 'custom-loads))
        (cond ((symbolp load) (ignore-errors (require load)))
	      ;; This is subsumed by the test below, but it's much faster.
	      ((assoc load load-history))
	      ;; This was just (assoc (locate-library load) load-history)
	      ;; but has been optimized not to load locate-library
	      ;; if not necessary.
	      ((let ((regexp (concat "\\(\\`\\|/\\)" (regexp-quote load)
				     "\\(\\'\\|\\.\\)"))
		     (found nil))
		 (dolist (loaded load-history)
		   (and (stringp (car loaded))
			(string-match-p regexp (car loaded))
			(setq found t)))
		 found))
	      ;; Without this, we would load cus-edit recursively.
	      ;; We are still loading it when we call this,
	      ;; and it is not in load-history yet.
	      ((equal load "cus-edit"))
              (t (ignore-errors (load load))))))))

(defvar custom-local-buffer nil
  "Non-nil, in a Customization buffer, means customize a specific buffer.
If this variable is non-nil, it should be a buffer,
and it means customize the local bindings of that buffer.
This variable is a permanent local, and it normally has a local binding
in every Customization buffer.")
(put 'custom-local-buffer 'permanent-local t)

(defun custom-set-default (variable value)
  "Default :set function for a customizable variable.
Normally, this sets the default value of VARIABLE to VALUE,
but if `custom-local-buffer' is non-nil,
this sets the local binding in that buffer instead."
  (if custom-local-buffer
      (with-current-buffer custom-local-buffer
	(set variable value))
    (set-default-toplevel-value variable value)))

(defun custom-set-minor-mode (variable value)
  ":set function for minor mode variables.
Normally, this sets the default value of VARIABLE to nil if VALUE
is nil and to t otherwise,
but if `custom-local-buffer' is non-nil,
this sets the local binding in that buffer instead."
  (if custom-local-buffer
      (with-current-buffer custom-local-buffer
	(funcall variable (if value 1 0)))
    (funcall variable (if value 1 0))))

(defun custom-quote (sexp)
  "Quote SEXP if it is not self quoting."
  ;; Can't use `macroexp-quote' because it is loaded after `custom.el'
  ;; during bootstrap.  See `loadup.el'.
  (if (and (not (consp sexp))
           (or (keywordp sexp)
               (not (symbolp sexp))
               (booleanp sexp)))
      sexp
    (list 'quote sexp)))

(defun customize-mark-to-save (symbol)
  "Mark SYMBOL for later saving.

If the default value of SYMBOL is different from the standard value,
set the `saved-value' property to a list whose car evaluates to the
default value.  Otherwise, set it to nil.

To actually save the value, call `custom-save-all'.

Return non-nil if the `saved-value' property actually changed."
  (custom-load-symbol symbol)
  (let* ((get (or (get symbol 'custom-get) #'default-value))
	 (value (funcall get symbol))
	 (saved (get symbol 'saved-value))
	 (comment (get symbol 'customized-variable-comment)))
    ;; Save default value if different from standard value.
    (put symbol 'saved-value
         (unless (custom--standard-value-p symbol value)
           (list (custom-quote value))))
    ;; Clear customized information (set, but not saved).
    (put symbol 'customized-value nil)
    ;; Save any comment that might have been set.
    (when comment
      (put symbol 'saved-variable-comment comment))
    (not (equal saved (get symbol 'saved-value)))))

(defun customize-mark-as-set (symbol)
  "Mark current value of SYMBOL as being set from customize.

If the default value of SYMBOL is different from the saved value if any,
or else if it is different from the standard value, set the
`customized-value' property to a list whose car evaluates to the
default value.  Otherwise, set it to nil.

Return non-nil if the `customized-value' property actually changed."
  (custom-load-symbol symbol)
  (let* ((get (or (get symbol 'custom-get) #'default-value))
	 (value (funcall get symbol))
	 (customized (get symbol 'customized-value))
	 (old (or (get symbol 'saved-value) (get symbol 'standard-value))))
    ;; Mark default value as set if different from old value.
    (if (not (and old
                  (equal value (ignore-errors
                                 (eval (car old))))))
	(progn (put symbol 'customized-value (list (custom-quote value)))
	       (custom-push-theme 'theme-value symbol 'user 'set
				  (custom-quote value)))
      (custom-push-theme 'theme-value symbol 'user
                         (if (get symbol 'saved-value) 'set 'reset)
                         (custom-quote value))
      (put symbol 'customized-value nil))
    ;; Changed?
    (not (equal customized (get symbol 'customized-value)))))

(defun custom-reevaluate-setting (symbol)
  "Reset the value of SYMBOL by re-evaluating its saved or standard value.
Use the :set function to do so.  This is useful for customizable options
that are defined before their standard value can really be computed.
E.g. dumped variables whose default depends on run-time information."
  ;; We are initializing
  ;; the variable, and normally any :set function would not apply.
  ;; For custom-initialize-delay, however, it is documented that "the
  ;; (delayed) initialization is performed with the :set function".
  ;; This is needed by eg global-font-lock-mode, which uses
  ;; custom-initialize-delay but needs the :set function custom-set-minor-mode
  ;; to also run during initialization.  So, long story short, we
  ;; always do the funcall step, even if symbol was not bound before.
  (funcall (or (get symbol 'custom-set) #'set-default)
	   symbol
	   (eval (car (or (get symbol 'saved-value)
	                  (get symbol 'standard-value))))))


;;; Custom Themes

;; Custom themes are collections of settings that can be enabled or
;; disabled as a unit.

;; Each Custom theme is defined by a symbol, called the theme name.
;; The `theme-settings' property of the theme name records the
;; variable and face settings of the theme.  This property is a list
;; of elements, each of the form
;;
;;     (PROP SYMBOL THEME VALUE)
;;
;;  - PROP is either `theme-value' or `theme-face'
;;  - SYMBOL is the face or variable name
;;  - THEME is the theme name (redundant, but simplifies the code)
;;  - VALUE is an expression that gives the theme's setting for SYMBOL.
;;
;; The theme name also has a `theme-feature' property, whose value is
;; specified when the theme is defined (see `custom-declare-theme').
;; Usually, this is just a symbol named THEME-theme.  This lets
;; external libraries call (require 'foo-theme).

;; In addition, each symbol (either a variable or a face) affected by
;; an *enabled* theme has a `theme-value' or `theme-face' property,
;; which is a list of elements each of the form
;;
;;     (THEME VALUE)
;;
;; which have the same meanings as in `theme-settings'.
;;
;; The `theme-value' and `theme-face' lists are ordered by decreasing
;; theme precedence.  Thus, the first element is always the one that
;; is in effect.

;; Each theme is stored in a theme file, with filename THEME-theme.el.
;; Loading a theme basically involves calling (load "THEME-theme")
;; This is done by the function `load-theme'.  Loading a theme
;; automatically enables it.
;;
;; When a theme is enabled, the `theme-value' and `theme-face'
;; properties for the affected symbols are set.  When a theme is
;; disabled, its settings are removed from the `theme-value' and
;; `theme-face' properties, but the theme's own `theme-settings'
;; property remains unchanged.

(defvar custom-known-themes '(user changed)
   "Themes that have been defined with `deftheme'.
The default value is the list (user changed).  The theme `changed'
contains the settings before custom themes are applied.  The theme
`user' contains all the settings the user customized and saved.
Additional themes declared with the `deftheme' macro will be added
to the front of this list.")

(defsubst custom-theme-p (theme)
  "Non-nil when THEME has been defined."
  (memq theme custom-known-themes))

(defsubst custom-check-theme (theme)
  "Check whether THEME is valid, and signal an error if it is not."
  (unless (custom-theme-p theme)
    (error "Unknown theme `%s'" theme)))

(defun custom--should-apply-setting (theme)
  "Non-nil if settings for the theme THEME should apply immediately.

Theme settings apply immediately if:
- THEME is already enabled.
- THEME is being enabled via `enable-theme' or an interactive call to
  `load-theme'.
- THEME is the `user' theme."
  (or (memq theme custom-enabled-themes)
      (null custom--inhibit-theme-enable)
      (and (eq custom--inhibit-theme-enable 'apply-only-user)
           (eq theme 'user))))

(defun custom-push-theme (prop symbol theme mode &optional value)
  "Record VALUE for face or variable SYMBOL in custom theme THEME.
PROP is `theme-face' for a face, `theme-value' for a variable.

MODE can be either the symbol `set' or the symbol `reset'.  If it is the
symbol `set', then VALUE is the value to use.  If it is the symbol
`reset', then SYMBOL will be removed from THEME (VALUE is ignored).

See `custom-known-themes' for a list of known themes."
  (unless (memq prop '(theme-value theme-face theme-icon))
    (error "Unknown theme property"))
  (let* ((old (get symbol prop))
	 (setting (assq theme old))  ; '(theme value)
	 (theme-settings             ; '(prop symbol theme value)
	  (get theme 'theme-settings)))
    (cond
     ;; Remove a setting:
     ((eq mode 'reset)
      (when setting
	(let (res)
	  (dolist (theme-setting theme-settings)
	    (if (and (eq (car  theme-setting) prop)
		     (eq (cadr theme-setting) symbol))
		(setq res theme-setting)))
	  (put theme 'theme-settings (delq res theme-settings)))
	(put symbol prop (delq setting old))))
     ;; Alter an existing setting:
     (setting
      (let (res)
	(dolist (theme-setting theme-settings)
	  (if (and (eq (car  theme-setting) prop)
		   (eq (cadr theme-setting) symbol))
	      (setq res theme-setting)))
	(put theme 'theme-settings
	     (cons (list prop symbol theme value)
		   (delq res theme-settings)))
        ;; It's tempting to use setcar here, but that could
        ;; inadvertently modify other properties in SYMBOL's proplist,
        ;; if those just happen to share elements with the value of PROP.
        (put symbol prop (cons (list theme value) (delq setting old)))))
     ;; Add a new setting:
     (t
      (when (custom--should-apply-setting theme)
	(unless old
	  ;; If the user changed a variable outside of Customize, save
	  ;; the value to a fake theme, `changed'.  If the theme is
	  ;; later disabled, we use this to bring back the old value.
	  ;;
	  ;; For faces, we just use `face--new-frame-defaults' to
	  ;; recompute when the theme is disabled.
	  (when (and (eq prop 'theme-value)
		     (boundp symbol))
	    (let ((val (symbol-value symbol)))
	      (unless (or
                       ;; We only do this trick if the current value
                       ;; is different from the standard value.
                       (custom--standard-value-p symbol val)
                       ;; And we don't do it if we would end up recording
                       ;; the same value for the user theme.  This way we avoid
                       ;; having ((user VALUE) (changed VALUE)).  That would be
                       ;; useless, because we don't disable the user theme.
                       (and (eq theme 'user) (equal (custom-quote val) value)))
		(setq old `((changed ,(custom-quote val))))))))
	(put symbol prop (cons (list theme value) old)))
      (put theme 'theme-settings
	   (cons (list prop symbol theme value) theme-settings))))))

(defun custom-fix-face-spec (spec)
  "Convert face SPEC, replacing obsolete :bold and :italic attributes.
Also change :reverse-video to :inverse-video."
  (when (listp spec)
    (if (or (memq :bold spec)
	    (memq :italic spec)
	    (memq :reverse-video spec))
	(let (result)
	  (while spec
	    (let ((key (car spec))
		  (val (car (cdr spec))))
	      (cond ((eq key :italic)
		     (push :slant result)
		     (push (if val 'italic 'normal) result))
		    ((eq key :bold)
		     (push :weight result)
		     (push (if val 'bold 'normal) result))
		    ((eq key :reverse-video)
		     (push :inverse-video result)
		     (push val result))
		    (t
		     (push key result)
		     (push val result))))
	    (setq spec (cddr spec)))
	  (nreverse result))
      spec)))

(defun custom-set-variables (&rest args)
  "Install user customizations of variable values specified in ARGS.
These settings are registered as theme `user'.
The arguments should each be a list of the form:

  (SYMBOL EXP [NOW [REQUEST [COMMENT]]])

This stores EXP (without evaluating it) as the saved value for SYMBOL.
If NOW is present and non-nil, then also evaluate EXP and set
the default value for the SYMBOL to the value of EXP.

REQUEST is a list of features we must require in order to
handle SYMBOL properly.
COMMENT is a comment string about SYMBOL."
  (apply #'custom-theme-set-variables 'user args))

(defun custom-theme-set-variables (theme &rest args)
  "Initialize variables for theme THEME according to settings in ARGS.
Each of the arguments in ARGS should be a list of this form:

  (SYMBOL EXP [NOW [REQUEST [COMMENT]]])

SYMBOL is the variable name, and EXP is an expression which
evaluates to the customized value.  EXP will also be stored,
without evaluating it, in SYMBOL's `saved-value' property, so
that it can be restored via the Customize interface.  It is also
added to the alist in SYMBOL's `theme-value' property (by
calling `custom-push-theme').

NOW, if present and non-nil, means to install the variable's
value directly now, even if its `defcustom' declaration has not
been executed.  This is for internal use only.

REQUEST is a list of features to `require' (which are loaded
prior to evaluating EXP).

COMMENT is a comment string about SYMBOL."
  (custom-check-theme theme)
  ;; Process all the needed autoloads before anything else, so that the
  ;; subsequent code has all the info it needs (e.g. which var corresponds
  ;; to a minor mode), regardless of the ordering of the variables.
  (dolist (entry args)
    (let* ((symbol (indirect-variable (nth 0 entry))))
      (unless (or (get symbol 'standard-value)
                  (memq (get symbol 'custom-autoload) '(nil noset)))
        ;; This symbol needs to be autoloaded, even just for a `set'.
        (custom-load-symbol symbol))))
  (setq args (custom--sort-vars args))
  (dolist (entry args)
    (unless (listp entry)
      (error "Incompatible Custom theme spec"))
    (let* ((symbol (indirect-variable (nth 0 entry)))
	   (value (nth 1 entry)))
      (custom-push-theme 'theme-value symbol theme 'set value)
      (when (custom--should-apply-setting theme)
	;; Now set the variable.
	(let* ((now (nth 2 entry))
	       (requests (nth 3 entry))
	       (comment (nth 4 entry))
	       set)
	  (when requests
	    (put symbol 'custom-requests requests)
            ;; Load any libraries that the setting has specified as
            ;; being required, but don't error out if the package has
            ;; been removed.
            (mapc (lambda (lib) (require lib nil t)) requests))
          (setq set (or (get symbol 'custom-set) #'custom-set-default))
	  (put symbol 'saved-value (list value))
	  (put symbol 'saved-variable-comment comment)
	  ;; Allow for errors in the case where the setter has
	  ;; changed between versions, say, but let the user know.
	  (condition-case data
	      (cond (now
		     ;; Rogue variable, set it now.
		     (put symbol 'force-value t)
		     (funcall set symbol (eval value)))
		    ((default-boundp symbol)
		     ;; Something already set this, overwrite it.
		     (funcall set symbol (eval value))))
	    (error
	     (message "Error setting %s: %s" symbol data)))
	  (and (or now (default-boundp symbol))
	       (put symbol 'variable-comment comment)))))))

(defvar custom--sort-vars-table)
(defvar custom--sort-vars-result)

(defun custom--sort-vars (vars)
  "Sort VARS based on custom dependencies.
VARS is a list whose elements have the same form as the ARGS
arguments to `custom-theme-set-variables'.  Return the sorted
list, in which A occurs before B if B was defined with a
`:set-after' keyword specifying A (see `defcustom')."
  (let ((custom--sort-vars-table (make-hash-table))
	(dependants (make-hash-table))
	(custom--sort-vars-result nil)
	last)
    ;; Construct a pair of tables keyed with the symbols of VARS.
    (dolist (var vars)
      (puthash (car var) (cons t var) custom--sort-vars-table)
      (puthash (car var) var dependants))
    ;; From the second table, remove symbols that are depended-on.
    (dolist (var vars)
      (dolist (dep (get (car var) 'custom-dependencies))
	(remhash dep dependants)))
    ;; If a variable is "stand-alone", put it last if it's a minor
    ;; mode or has a :require flag.  This is not really necessary, but
    ;; putting minor modes last helps ensure that the mode function
    ;; sees other customized values rather than default values.
    (maphash (lambda (sym var)
	       (when (and (null (get sym 'custom-dependencies))
			  (or (nth 3 var)
			      (eq (get sym 'custom-set)
				  'custom-set-minor-mode)))
		 (remhash sym dependants)
		 (push var last)))
	     dependants)
    ;; The remaining symbols depend on others but are not
    ;; depended-upon.  Do a depth-first topological sort.
    (maphash #'custom--sort-vars-1 dependants)
    (nreverse (append last custom--sort-vars-result))))

(defun custom--sort-vars-1 (sym &optional _ignored)
  (let ((elt (gethash sym custom--sort-vars-table)))
    ;; The car of the hash table value is nil if the variable has
    ;; already been processed, `dependant' if it is a dependant in the
    ;; current graph descent, and t otherwise.
    (when elt
      (cond
       ((eq (car elt) 'dependant)
	(error "Circular custom dependency on `%s'" sym))
       ((car elt)
	(setcar elt 'dependant)
	(dolist (dep (get sym 'custom-dependencies))
	  (custom--sort-vars-1 dep))
	(setcar elt nil)
	(push (cdr elt) custom--sort-vars-result))))))


;;; Defining themes.

;; A theme file is named `THEME-theme.el' (where THEME is the theme
;; name) found in `custom-theme-load-path'.  It has this format:
;;
;;   (deftheme THEME
;;     DOCSTRING)
;;
;;   (custom-theme-set-variables
;;    'THEME
;;    [THEME-VARIABLES])
;;
;;   (custom-theme-set-faces
;;    'THEME
;;    [THEME-FACES])
;;
;;   (provide-theme 'THEME)


(defmacro deftheme (theme &optional doc &rest properties)
  "Declare THEME to be a Custom theme.
The optional argument DOC is a doc string describing the theme.
PROPERTIES are interpreted as a property list that will be stored
in the `theme-properties' property for THEME.

Any theme `foo' should be defined in a file called `foo-theme.el';
see `custom-make-theme-feature' for more information."
  (declare (doc-string 2)
           (indent 1))
  (let ((feature (custom-make-theme-feature theme)))
    ;; It is better not to use backquote in this file,
    ;; because that makes a bootstrapping problem
    ;; if you need to recompile all the Lisp files using interpreted code.
    (list 'custom-declare-theme (list 'quote theme) (list 'quote feature) doc
          (cons 'list properties))))

(defun custom-declare-theme (theme feature &optional doc properties)
  "Like `deftheme', but THEME is evaluated as a normal argument.
FEATURE is the feature this theme provides.  Normally, this is a
symbol created from THEME by `custom-make-theme-feature'.  The
optional argument DOC may contain the documentation for THEME.
The optional argument PROPERTIES may contain a property list of
attributes associated with THEME."
  (unless (custom-theme-name-valid-p theme)
    (error "Custom theme cannot be named %S" theme))
  (unless (memq theme custom-known-themes)
    (push theme custom-known-themes))
  (put theme 'theme-feature feature)
  (when doc
    (put theme 'theme-documentation doc))
  (when properties
    (put theme 'theme-properties properties)))

(defun custom-make-theme-feature (theme)
  "Given a symbol THEME, create a new symbol by appending \"-theme\".
Store this symbol in the `theme-feature' property of THEME.
Calling `provide-theme' to provide THEME actually puts `THEME-theme'
into `features'.

This allows for a file-name convention for autoloading themes:
Every theme X has a property `provide-theme' whose value is \"X-theme\".
\(load-theme X) then attempts to load the file `X-theme.el'."
  (intern (concat (symbol-name theme) "-theme")))

;;; Loading themes.

(defcustom custom-theme-directory user-emacs-directory
  "Default user directory for storing custom theme files.
The command `customize-create-theme' writes theme files into this
directory.  By default, Emacs searches for custom themes in this
directory first---see `custom-theme-load-path'."
  :initialize #'custom-initialize-delay
  :type 'directory
  :group 'customize
  :version "22.1")

(defvar custom-theme-load-path (list 'custom-theme-directory t)
  "List of directories to search for custom theme files.
When loading custom themes (e.g. in `customize-themes' and
`load-theme'), Emacs searches for theme files in the specified
order.  Each element in the list should be one of the following:
- the symbol `custom-theme-directory', meaning the value of
  `custom-theme-directory'.
- the symbol t, meaning the built-in theme directory (a directory
  named \"themes\" in `data-directory').
- a directory name (a string).

Each theme file is named THEME-theme.el, where THEME is the theme
name.

This variable is designed for use in lisp code (including
external packages).  For manual user customizations, use
`custom-theme-directory' instead.")

(defvar custom--inhibit-theme-enable 'apply-only-user
  "Whether the custom-theme-set-* functions act immediately.

If the theme argument for those functions is an already enabled theme,
the theme settings always apply immediately, ignoring this variable.

If nil, `custom-theme-set-variables' and `custom-theme-set-faces'
change the current values of the given variable or face.  If
t, they just make a record of the theme settings.  If the
value is `apply-only-user', then apply setting to the
`user' theme immediately and defer other updates.")

(defun provide-theme (theme)
  "Indicate that this file provides THEME.
This calls `provide' to provide the feature name stored in THEME's
property `theme-feature' (which is usually a symbol created by
`custom-make-theme-feature')."
  (unless (custom-theme-name-valid-p theme)
    (error "Custom theme cannot be named %S" theme))
  (custom-check-theme theme)
  (provide (get theme 'theme-feature)))

(defun require-theme (feature &optional noerror)
  "Load FEATURE from a file along `custom-theme-load-path'.

This function is like `require', but searches along
`custom-theme-load-path' instead of `load-path'.  It can be used
by Custom themes to load supporting Lisp files when `require' is
unsuitable.

If FEATURE is not already loaded, search for a file named FEATURE
with an added `.elc' or `.el' suffix, in that order, in the
directories specified by `custom-theme-load-path'.

Return FEATURE if the file is successfully found and loaded, or
if FEATURE was already loaded.  If the file fails to load, signal
an error.  If optional argument NOERROR is non-nil, return nil
instead of signaling an error.  If the file loads but does not
provide FEATURE, signal an error.  This cannot be suppressed."
  (cond
   ((featurep feature) feature)
   ((let* ((path (custom-theme--load-path))
           (file (locate-file (symbol-name feature) path '(".elc" ".el"))))
      (and file (require feature (file-name-sans-extension file) noerror))))
   ((not noerror)
    (signal 'file-missing `("Cannot open load file" "No such file or directory"
                            ,(symbol-name feature))))))

(defcustom custom-safe-themes '(default)
  "Themes that are considered safe to load.
If the value is a list, each element should be either the SHA-256
hash of a safe theme file, or the symbol `default', which stands
for any theme in the built-in Emacs theme directory (a directory
named \"themes\" in `data-directory').

If the value is t, Emacs treats all themes as safe.

This variable cannot be set in a Custom theme."
  :type '(choice (repeat :tag "List of safe themes"
			 (choice string
				 (const :tag "Built-in themes" default)))
		 (const :tag "All themes" t))
  :group 'customize
  :risky t
  :version "24.1")

(defun load-theme (theme &optional no-confirm no-enable)
  "Load Custom theme named THEME from its file and possibly enable it.
The theme file is named THEME-theme.el, in one of the directories
specified by `custom-theme-load-path'.

If the theme is not considered safe by `custom-safe-themes',
prompt the user for confirmation before loading it.  But if
optional arg NO-CONFIRM is non-nil, load the theme without
prompting.

Normally, this function also enables THEME.  If optional arg
NO-ENABLE is non-nil, load the theme but don't enable it, unless
the theme was already enabled.

Note that enabling THEME does not disable any other
already-enabled themes.  If THEME is enabled, it has the highest
precedence (after `user') among enabled themes.  To disable other
themes, use `disable-theme'.

This function is normally called through Customize when setting
`custom-enabled-themes'.  If used directly in your init file, it
should be called with a non-nil NO-CONFIRM argument, or after
`custom-safe-themes' has been loaded.

Return t if THEME was successfully loaded, nil otherwise."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))
    nil nil))
  (unless (symbolp theme)
    (signal 'wrong-type-argument (list 'symbolp theme)))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  ;; If THEME is already enabled, re-enable it after loading, even if
  ;; NO-ENABLE is t.
  (if no-enable
      (setq no-enable (not (custom-theme-enabled-p theme))))
  ;; If reloading, clear out the old theme settings.
  (when (custom-theme-p theme)
    (disable-theme theme)
    (put theme 'theme-settings nil)
    (put theme 'theme-feature nil)
    (put theme 'theme-documentation nil))
  (let ((file (locate-file (concat (symbol-name theme) "-theme.el")
                           (custom-theme--load-path)
                           '("" "c")))
        (custom--inhibit-theme-enable t))
    ;; Check file safety with `custom-safe-themes', prompting the
    ;; user if necessary.
    (cond ((not file)
           (error "Unable to find theme file for `%s'" theme))
          ((or no-confirm
               (eq custom-safe-themes t)
               (and (memq 'default custom-safe-themes)
                    (equal (file-name-directory file)
                           (expand-file-name "themes/" data-directory))))
           ;; Theme is safe; load byte-compiled version if available.
           (load (file-name-sans-extension file) nil t nil t))
          ((with-temp-buffer
             (insert-file-contents file)
             (let ((hash (secure-hash 'sha256 (current-buffer))))
               (when (or (member hash custom-safe-themes)
                         (custom-theme-load-confirm hash))
                 (eval-buffer nil nil file)
                 t))))
          (t
           (error "Unable to load theme `%s'" theme))))
  (when-let* ((obs (get theme 'byte-obsolete-info)))
    (display-warning 'initialization
                     (format "The `%s' theme is obsolete%s"
                             theme
                             (if (nth 2 obs)
                                 (format " since Emacs %s" (nth 2 obs))
                               ""))))
  ;; Optimization: if the theme changes the `default' face, put that
  ;; entry first.  This avoids some `frame-set-background-mode' rigmarole
  ;; by assigning the new background immediately.
  (let* ((settings (get theme 'theme-settings))
         (tail settings)
         found)
    (while (and tail (not found))
      (and (eq (nth 0 (car tail)) 'theme-face)
           (eq (nth 1 (car tail)) 'default)
           (setq found (car tail)))
      (setq tail (cdr tail)))
    (when found
      (put theme 'theme-settings (cons found (delq found settings)))))
  ;; Finally, enable the theme.
  (unless no-enable
    (enable-theme theme))
  t)

(defun theme-list-variants (theme &rest list)
  "Return a list of theme variants for THEME.
By default this will use all known custom themes (see
`custom-available-themes') to check for variants.  This can be
restricted if the optional argument LIST containing a list of
theme symbols to consider."
  (let* ((properties (get theme 'theme-properties))
         (family (plist-get properties :family)))
    (seq-filter
     (lambda (variant)
       (and (eq (plist-get (get variant 'theme-properties) :family)
                family)
            (not (eq variant theme))))
     (or list (custom-available-themes)))))

(defun theme-choose-variant (&optional no-confirm no-enable)
  "Switch from the current theme to one of its variants.
The current theme will be disabled before variant is enabled.  If
the current theme has only one variant, switch to that variant
without prompting, otherwise prompt for the variant to select.
See `load-theme' for the meaning of NO-CONFIRM and NO-ENABLE."
  (interactive)
  (let ((active-color-schemes
         (seq-filter
          (lambda (theme)
            ;; FIXME: As most themes currently do not have a `:kind'
            ;; tag, it is assumed that a theme is a color scheme by
            ;; default.  This should be reconsidered in the future.
            (memq (plist-get (get theme 'theme-properties) :kind)
                  '(color-scheme nil)))
          custom-enabled-themes)))
    (cond
     ((length= active-color-schemes 0)
      (user-error "No theme is active, cannot toggle"))
     ((length> active-color-schemes 1)
      (user-error "More than one theme active, cannot unambiguously toggle")))
    (let* ((theme (car active-color-schemes))
           (family (plist-get (get theme 'theme-properties) :family)))
      (unless family
        (error "Theme `%s' does not have any known variants" theme))
      (let* ((variants (theme-list-variants theme))
             (choice (cond
                      ((null variants)
                       (error "`%s' has no variants" theme))
                      ((length= variants 1)
                       (car variants))
                      ((intern (completing-read "Load custom theme: " variants))))))
        (disable-theme theme)
        (load-theme choice no-confirm no-enable)))))

(defalias 'toggle-theme #'theme-choose-variant)

(defun custom-theme-load-confirm (hash)
  "Query the user about loading a Custom theme that may not be safe.
The theme should be in the current buffer.  If the user agrees,
query also about adding HASH to `custom-safe-themes'."
  (unless noninteractive
    (save-window-excursion
      (rename-buffer "*Custom Theme*" t)
      (emacs-lisp-mode)
      (pop-to-buffer (current-buffer))
      (goto-char (point-min))
      (prog1 (when (y-or-n-p "Loading a theme can run Lisp code.  Really load? ")
	       ;; Offer to save to `custom-safe-themes'.
	       (and (or custom-file user-init-file)
		    (y-or-n-p "Treat this theme as safe in future sessions? ")
		    (customize-push-and-save 'custom-safe-themes (list hash)))
	       t)
	(quit-window)))))

(defun custom-theme-name-valid-p (name)
  "Return t if NAME is a valid name for a Custom theme, nil otherwise.
NAME should be a symbol."
  (and (not (memq name '(nil user changed)))
       (symbolp name)
       (not (string= "" (symbol-name name)))))

(defun custom-available-themes ()
  "Return a list of Custom themes available for loading.
Search the directories specified by `custom-theme-load-path' for
files named FOO-theme.el, and return a list of FOO symbols.

The returned symbols may not correspond to themes that have been
loaded, and no effort is made to check that the files contain
valid Custom themes.  For a list of loaded themes, check the
variable `custom-known-themes'."
  (let ((suffix "-theme\\.el\\'")
        themes)
    (dolist (dir (custom-theme--load-path))
      ;; `custom-theme--load-path' promises DIR exists and is a
      ;; directory, but `custom.el' is loaded too early during
      ;; bootstrap to use `cl-lib' macros, so guard with
      ;; `file-directory-p' instead of calling `cl-assert'.
      (dolist (file (and (file-directory-p dir)
                         (directory-files dir nil suffix)))
        (let ((theme (intern (substring file 0 (string-match-p suffix file)))))
          (and (custom-theme-name-valid-p theme)
               (not (memq theme themes))
               (push theme themes)))))
    (nreverse themes)))

(defun custom-theme--load-path ()
  "Expand `custom-theme-load-path' into a list of directories.
Members of `custom-theme-load-path' that either don't exist or
are not directories are omitted from the expansion."
  (let (lpath)
    (dolist (f custom-theme-load-path)
      (cond ((eq f 'custom-theme-directory)
	     (setq f custom-theme-directory))
	    ((eq f t)
	     (setq f (expand-file-name "themes" data-directory))))
      (if (file-directory-p f)
	  (push f lpath)))
    (nreverse lpath)))


;;; Enabling and disabling loaded themes.

(defcustom enable-theme-functions nil
  "Abnormal hook that is run after a theme has been enabled.
The functions in the hook are called with one parameter -- the
 name of the theme that's been enabled (as a symbol)."
  :type 'hook
  :group 'customize
  :version "29.1")

(defcustom disable-theme-functions nil
  "Abnormal hook that is run after a theme has been disabled.
The functions in the hook are called with one parameter -- the
 name of the theme that's been disabled (as a symbol)."
  :type 'hook
  :group 'customize
  :version "29.1")

(defun enable-theme (theme)
  "Reenable all variable and face settings defined by THEME.
THEME should be either `user', or a theme loaded via `load-theme'.

After this function completes, THEME will have the highest
precedence (after `user') among enabled themes.

Note that any already-enabled themes remain enabled after this
function runs.  To disable other themes, use `disable-theme'.

After THEME has been enabled, runs `enable-theme-functions'."
  (interactive (list (intern
		      (completing-read
		       "Enable custom theme: "
		       obarray (lambda (sym) (get sym 'theme-settings)) t))))
  (unless (custom-theme-p theme)
    (error "Undefined Custom theme %s" theme))
  (let ((settings (get theme 'theme-settings)) ; '(prop symbol theme value)
        ;; We are enabling the theme, so don't inhibit enabling it.  (Bug#34027)
        (custom--inhibit-theme-enable nil))
    ;; Loop through theme settings, recalculating vars/faces.
    (dolist (s settings)
      (let* ((prop (car s))
             (symbol (cadr s))
             (spec-list (get symbol prop))
             (val (and (boundp symbol) (symbol-value symbol))))
        ;; We can't call `custom-push-theme' when enabling the theme: it's not
        ;; that the theme settings have changed, it's just that we want to
        ;; enable those settings.  But we might need to save a user setting
        ;; outside of Customize, in order to get back to it when disabling
        ;; the theme, just like in `custom-push-theme'.
        (when (and (custom--should-apply-setting theme)
                   ;; Only do it for variables; for faces, using
                   ;; `face-new-frame-defaults' is enough.
                   (eq prop 'theme-value)
                   (boundp symbol)
                   (not (or spec-list
                            ;; Only if the current value is different from
                            ;; the standard value.
                            (custom--standard-value-p symbol val)
                            ;; And only if the changed value is different
                            ;; from the new value under the user theme.
                            (and (eq theme 'user)
                                 (equal (custom-quote val) (nth 3 s))))))
          (setq spec-list `((changed ,(custom-quote val)))))
        (put symbol prop (cons (cddr s) (assq-delete-all theme spec-list)))
	(cond
	 ((eq prop 'theme-face)
	  (custom-theme-recalc-face symbol))
	 ((eq prop 'theme-value)
	  ;; Ignore `custom-enabled-themes' and `custom-safe-themes'.
	  (unless (memq symbol '(custom-enabled-themes custom-safe-themes))
	    (custom-theme-recalc-variable symbol)))))))
  (unless (eq theme 'user)
    (setq custom-enabled-themes
	  (cons theme (remq theme custom-enabled-themes)))
    ;; Give the `user' theme the highest priority.
    (enable-theme 'user))
  ;; Allow callers to react to the enabling.
  (run-hook-with-args 'enable-theme-functions theme))

(defcustom custom-enabled-themes nil
  "List of enabled Custom Themes, highest precedence first.
This list does not include the `user' theme, which is set by
Customize and always takes precedence over other Custom Themes.

This variable cannot be defined inside a Custom theme; there, it
is simply ignored.

Setting this variable through Customize calls `enable-theme' or
`load-theme' for each theme in the list."
  :group 'customize
  :type  '(repeat symbol)
  :set-after '(custom-theme-directory custom-theme-load-path
				      custom-safe-themes)
  :risky t
  :set (lambda (symbol themes)
	 (let (failures)
	   (setq themes (delq 'user (delete-dups themes)))
	   ;; Disable all themes not in THEMES.
           (dolist (theme (and (boundp symbol)
                               (symbol-value symbol)))
             (unless (memq theme themes)
               (disable-theme theme)))
	   ;; Call `enable-theme' or `load-theme' on each of THEMES.
	   (dolist (theme (reverse themes))
	     (condition-case nil
		 (if (custom-theme-p theme)
		     (enable-theme theme)
		   (load-theme theme))
               (error (push theme failures)
                      (setq themes (delq theme themes)))))
	   (enable-theme 'user)
	   (custom-set-default symbol themes)
           (when failures
             (message "Failed to enable theme(s): %s"
                      (mapconcat #'symbol-name failures ", "))))))

(defsubst custom-theme-enabled-p (theme)
  "Return non-nil if THEME is enabled."
  (memq theme custom-enabled-themes))

(defun disable-theme (theme)
  "Disable all variable and face settings defined by THEME.
See `custom-enabled-themes' for a list of enabled themes.

After THEME has been disabled, runs `disable-theme-functions'."
  (interactive (list (intern
		      (completing-read
		       "Disable custom theme: "
                       (mapcar #'symbol-name custom-enabled-themes)
		       nil t))))
  (when (custom-theme-enabled-p theme)
    (let ((settings (get theme 'theme-settings)))
      (dolist (s settings)
	(let* ((prop   (car s))
	       (symbol (cadr s))
	       (val (assq-delete-all theme (get symbol prop))))
          (put symbol prop val)
	  (cond
	   ((eq prop 'theme-value)
            (custom-theme-recalc-variable symbol)
            ;; We might have to reset the stashed value of the variable, if
            ;; no other theme is customizing it.  Without this, loading a theme
            ;; that has a setting for an unbound user option and then disabling
            ;; it will leave this lingering setting for the option, and if then
            ;; Emacs evaluates the defcustom the saved-value might be used to
            ;; set the variable.  (Bug#20766)
            (unless (get symbol 'theme-value)
              (put symbol 'saved-value nil)))
	   ((eq prop 'theme-face)
	    ;; If the face spec specified by this theme is in the
	    ;; saved-face property, reset that property.
	    (when (equal (nth 3 s) (get symbol 'saved-face))
              (put symbol 'saved-face (cadar val))))))))
    ;; Recompute faces on all frames.
    (dolist (frame (frame-list))
      ;; We must reset the fg and bg color frame parameters, or
      ;; `face-set-after-frame-default' will use the existing
      ;; parameters, which could be from the disabled theme.
      (set-frame-parameter frame 'background-color
                           (custom--frame-color-default
                            frame :background "background" "Background"
                            "unspecified-bg" "white"))
      (set-frame-parameter frame 'foreground-color
                           (custom--frame-color-default
                            frame :foreground "foreground" "Foreground"
                            "unspecified-fg" "black"))
      (face-set-after-frame-default frame))
    (setq custom-enabled-themes
          (delq theme custom-enabled-themes))
    ;; Allow callers to react to the disabling.
    (run-hook-with-args 'disable-theme-functions theme)))

;; Only used if window-system not null.
(declare-function x-get-resource "frame.c"
		  (attribute class &optional component subclass))

(defun custom--frame-color-default (frame attribute resource-attr resource-class
					  tty-default x-default)
  (let ((col (face-attribute 'default attribute t)))
    (cond
     ((and col (not (eq col 'unspecified))) col)
     ((null (window-system frame)) tty-default)
     ((setq col (x-get-resource resource-attr resource-class)) col)
     (t x-default))))

(defun custom-variable-theme-value (variable)
  "Return (list VALUE) indicating the custom theme value of VARIABLE.
That is to say, it specifies what the value should be according to
currently enabled custom themes.

This function returns nil if no custom theme specifies a value for VARIABLE."
  (let ((theme-value (get variable 'theme-value)))
    (if theme-value
	(cdr (car theme-value)))))

(defun custom-theme-recalc-variable (variable)
  "Set VARIABLE according to currently enabled custom themes."
  (let ((valspec (custom-variable-theme-value variable)))
    ;; We used to save VALSPEC under the saved-value property unconditionally,
    ;; but that is a recipe for trouble because we might end up saving session
    ;; customizations if the user loads a theme.  (Bug#21355)
    ;; It's better to only use the saved-value property to stash the value only
    ;; if we really need to stash it (i.e., VARIABLE is void).
    (condition-case nil
        (default-toplevel-value variable) ; See if it doesn't fail.
      (void-variable (when valspec
                       (put variable 'saved-value valspec))))
    (unless valspec
      (setq valspec (get variable 'standard-value)))
    (if (and valspec
	     (or (get variable 'force-value)
		 (default-boundp variable)))
        (funcall (or (get variable 'custom-set) #'set-default) variable
		 (eval (car valspec))))))

(defun custom-theme-recalc-face (face)
  "Set FACE according to currently enabled custom themes.
If FACE is not initialized as a face, do nothing; otherwise call
`face-spec-recalc' to recalculate the face on all frames."
  (if (get face 'face-alias)
      (setq face (get face 'face-alias)))
  (if (facep face)
      ;; Reset the faces for each frame.
      (dolist (frame (frame-list))
	(face-spec-recalc face frame))))


;;; XEmacs compatibility functions

;; In XEmacs, when you reset a Custom Theme, you have to specify the
;; theme to reset it to.  We just apply the next available theme, so
;; just ignore the IGNORED arguments.

(defun custom-theme-reset-variables (theme &rest args)
  "Reset some variable settings in THEME to their values in other themes.
Each of the arguments ARGS has this form:

    (VARIABLE IGNORED)

This means reset VARIABLE.  (The argument IGNORED is ignored)."
  (custom-check-theme theme)
  (dolist (arg args)
    (custom-push-theme 'theme-value (car arg) theme 'reset)))

(defun custom-reset-variables (&rest args)
  "Reset the specs of some variables to their values in other themes.
This creates settings in the `user' theme.

Each of the arguments ARGS has this form:

    (VARIABLE IGNORED)

This means reset VARIABLE.  (The argument IGNORED is ignored)."
  (declare (obsolete nil "29.1"))
    (apply #'custom-theme-reset-variables 'user args))

(defun custom-add-choice (variable choice)
  "Add CHOICE to the custom type of VARIABLE.
If a choice with the same tag already exists, no action is taken."
  (let ((choices (get variable 'custom-type)))
    (unless (eq (car choices) 'choice)
      (error "Not a choice type: %s" choices))
    (unless (seq-find (lambda (elem)
                        (equal (caddr (member :tag elem))
                               (caddr (member :tag choice))))
                      (cdr choices))
      ;; Put the new choice at the end.
      (put variable 'custom-type
           (append choices (list choice))))))

(defun custom--add-custom-loads (symbol loads)
  ;; Don't overwrite existing `custom-loads'.
  (dolist (load (get symbol 'custom-loads))
    (unless (memq load loads)
      (push load loads)))
  (put symbol 'custom-loads loads))

(provide 'custom)

;;; custom.el ends here
