;;; abbrev.el --- abbrev mode commands for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 1985-2025 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: abbrev convenience
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

;; This facility is documented in the Emacs Manual.

;; Todo:

;; - Cleanup name space.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'obarray)

(defgroup abbrev-mode nil
  "Word abbreviations mode."
  :link '(custom-manual "(emacs)Abbrevs")
  :group 'abbrev)

(defcustom abbrev-file-name
  (locate-user-emacs-file "abbrev_defs" ".abbrev_defs")
  "Default name of file from which to read and where to save abbrevs."
  :initialize #'custom-initialize-delay
  :type 'file)

(defcustom only-global-abbrevs nil
  "Non-nil means user plans to use only global abbrevs.
This makes the commands that normally define mode-specific abbrevs
define global abbrevs instead."
  :type 'boolean
  :group 'abbrev-mode
  :group 'convenience)

(define-minor-mode abbrev-mode
  "Toggle Abbrev mode in the current buffer.

In Abbrev mode, inserting an abbreviation causes it to expand and
be replaced by its expansion."
  ;; It's defined in C, this stops the 'define-minor-mode' macro from
  ;; defining it again.
  :variable abbrev-mode)

(put 'abbrev-mode 'safe-local-variable #'booleanp)


(define-obsolete-variable-alias 'edit-abbrevs-map
  'edit-abbrevs-mode-map "24.4")
(defvar-keymap edit-abbrevs-mode-map
  :doc "Keymap used in `edit-abbrevs'."
  "C-x C-s" #'abbrev-edit-save-buffer
  "C-x C-w" #'abbrev-edit-save-to-file
  "C-c C-c" #'edit-abbrevs-redefine)

(defun kill-all-abbrevs ()
  "Undefine all defined abbrevs."
  (interactive)
  (dolist (tablesym abbrev-table-name-list)
    (clear-abbrev-table (symbol-value tablesym))))

(defun copy-abbrev-table (table)
  "Make a new abbrev-table with the same abbrevs as TABLE.
This function does not copy property lists of the abbrevs.
See `define-abbrev' for the documentation of abbrev properties."
  (let ((new-table (make-abbrev-table)))
    (obarray-map
     (lambda (symbol)
       (define-abbrev new-table
	 (symbol-name symbol)
	 (symbol-value symbol)
	 (symbol-function symbol)))
     table)
    new-table))

(defun insert-abbrevs ()
  "Insert the description of all defined abbrevs after point.
Set mark after the inserted text."
  (interactive)
  (push-mark
   (save-excursion
     (dolist (tablesym abbrev-table-name-list)
       (insert-abbrev-table-description tablesym t))
     (point))))

(defun list-abbrevs (&optional local)
  "Display a list of the defined abbrevs.
If LOCAL is non-nil (interactively, when invoked with a
prefix arg), display only local, i.e. mode-specific, abbrevs.
Otherwise display all the abbrevs."
  (interactive "P")
  (display-buffer (prepare-abbrev-list-buffer local)))

(defun abbrev-table-name (table)
  "Return the name of the specified abbrev TABLE."
  (let ((tables abbrev-table-name-list)
	found)
    (while (and (not found) tables)
      (when (eq (symbol-value (car tables)) table)
	(setq found (car tables)))
      (setq tables (cdr tables)))
    found))

(defun prepare-abbrev-list-buffer (&optional local)
  "Return buffer listing abbreviations and expansions for each abbrev table.

If LOCAL is non-nil, include in the buffer only the local abbrevs."
  (let ((local-table local-abbrev-table))
    (with-current-buffer (get-buffer-create "*Abbrevs*")
      (erase-buffer)
      (if local
          (insert-abbrev-table-description
           (abbrev-table-name local-table) t)
        (let (empty-tables)
	  (dolist (table abbrev-table-name-list)
	    (if (abbrev-table-empty-p (symbol-value table))
		(push table empty-tables)
	      (insert-abbrev-table-description table t)))
	  (dolist (table (nreverse empty-tables))
	    (insert-abbrev-table-description table t)))
        ;; Note: `list-abbrevs' can display only local abbrevs, in
        ;; which case editing could lose abbrevs of other tables.
        ;; Thus enter `edit-abbrevs-mode' only if LOCAL is nil.
        (edit-abbrevs-mode))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (current-buffer))))

(defun edit-abbrevs ()
  "Alter abbrev definitions by editing the list of abbrevs.
This selects a buffer containing the list of abbrev definitions
with point located in the abbrev table for the current buffer, and
turns on `edit-abbrevs-mode' in the buffer with the list of abbrevs.

You can edit the abbrevs and type \\<edit-abbrevs-mode-map>\\[edit-abbrevs-redefine] \
to redefine abbrevs
according to your editing.

The abbrevs editing buffer contains a header line for each
abbrev table, which is the abbrev table name in parentheses.

The header line is followed by one line per abbrev in that table:

    NAME   USECOUNT   EXPANSION   HOOK

where NAME and EXPANSION are strings with quotes,
USECOUNT is an integer, and HOOK is any valid function
that may be omitted (it is usually omitted)."
  (interactive)
  (let ((table-name (abbrev-table-name local-abbrev-table)))
    (switch-to-buffer (prepare-abbrev-list-buffer))
    (when (and table-name
               (search-forward
                (concat "(" (symbol-name table-name) ")\n\n") nil t))
      (goto-char (match-end 0)))))

(defun edit-abbrevs-redefine ()
  "Redefine abbrevs according to current buffer contents."
  (interactive nil edit-abbrevs-mode)
  (save-restriction
    (widen)
    (define-abbrevs t)
    (set-buffer-modified-p nil)))

(defun define-abbrevs (&optional arg)
  "Define abbrevs according to current visible buffer contents.
See documentation of `edit-abbrevs' for info on the format of the
text you must have in the buffer.
If ARG is non-nil (interactively, when invoked with a prefix
argument), eliminate all abbrev definitions except the ones
defined by the current buffer contents."
  (interactive "P")
  (if arg (kill-all-abbrevs))
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp)) (re-search-forward "^(" nil t))
      (let* ((buf (current-buffer))
	     (table (read buf))
	     abbrevs name hook exp count sys)
	(forward-line 1)
	(while (and (not (eobp))
                    ;; Advance as long as we're looking at blank lines
                    ;; or we have an abbrev.
                    (looking-at "[ \t\n]\\|\\(\"\\)"))
          (when (match-string 1)
	    (setq name (read buf) count (read buf))
	    (if (equal count '(sys))
	        (setq sys t count (read buf))
	      (setq sys nil))
	    (setq exp (read buf))
	    (skip-chars-backward " \t\n\f")
	    (setq hook (if (not (eolp)) (read buf)))
	    (skip-chars-backward " \t\n\f")
	    (setq abbrevs (cons (list name exp hook count sys) abbrevs)))
          (forward-line 1))
	(define-abbrev-table table abbrevs)))))

(defun read-abbrev-file (&optional file quietly)
  "Read abbrev definitions from file written with `write-abbrev-file'.
Optional argument FILE is the name of the file to read;
it defaults to the value of `abbrev-file-name'.
Optional second argument QUIETLY non-nil means don't display a message
about loading the abbrevs."
  (interactive
   (list
    (read-file-name (format-prompt "Read abbrev file" abbrev-file-name)
		    nil abbrev-file-name t)))
  (let ((warning-inhibit-types '((files missing-lexbind-cookie))))
    (load (or file abbrev-file-name) nil quietly))
  (setq abbrevs-changed nil))

(defun quietly-read-abbrev-file (&optional file)
  "Quietly read abbrev definitions from file written with `write-abbrev-file'.
Optional argument FILE is the name of the file to read;
it defaults to the value of `abbrev-file-name'.
Do not display any messages about loading the abbrevs."
					;(interactive "fRead abbrev file: ")
  (read-abbrev-file file t))

(defun write-abbrev-file (&optional file verbose)
  "Write all user-level abbrev definitions to a file of Lisp code.
This does not include system abbrevs; it includes only the abbrev tables
listed in `abbrev-table-name-list'.
The file written can be loaded in another session to define the same abbrevs.
The argument FILE is the file name to write.  If omitted or nil, it defaults
to the value of `abbrev-file-name'.
If VERBOSE is non-nil, display a message indicating the file where the
abbrevs have been saved."
  (interactive
   (list
    (read-file-name "Write abbrev file: "
		    (file-name-directory (expand-file-name abbrev-file-name))
		    abbrev-file-name)))
  (or (and file (> (length file) 0))
      (setq file abbrev-file-name))
  (let ((coding-system-for-write 'utf-8))
    (with-temp-buffer
      (dolist (table
	       ;; We sort the table in order to ease the automatic
	       ;; merging of different versions of the user's abbrevs
	       ;; file.  This is useful, for example, when the
	       ;; user keeps their home directory in a revision
	       ;; control system, and therefore keeps multiple
	       ;; slightly-differing loosely synchronized copies.
	       (sort (copy-sequence abbrev-table-name-list)
		     (lambda (s1 s2)
		       (string< (symbol-name s1)
				(symbol-name s2)))))
	(if (abbrev--table-symbols table)
            (insert-abbrev-table-description table nil)))
      (when (unencodable-char-position (point-min) (point-max) 'utf-8)
	(setq coding-system-for-write 'utf-8-emacs))
      (goto-char (point-min))
      (insert (format ";; -*- coding: %S; lexical-binding: t -*-\n"
                      coding-system-for-write))
      (write-region nil nil file nil (and (not verbose) 0)))))

(defun abbrev-edit-save-to-file (file)
  "Save to FILE all the user-level abbrev definitions in current buffer."
  (interactive
   (list (read-file-name "Save abbrevs to file: "
			 (file-name-directory
			  (expand-file-name abbrev-file-name))
                         abbrev-file-name))
   edit-abbrevs-mode)
  (edit-abbrevs-redefine)
  (write-abbrev-file file t))

(defun abbrev-edit-save-buffer ()
  "Save all the user-level abbrev definitions in current buffer.
The saved abbrevs are written to the file specified by
`abbrev-file-name'."
  (interactive nil edit-abbrevs-mode)
  (abbrev-edit-save-to-file abbrev-file-name)
  (setq abbrevs-changed nil))


(defun add-mode-abbrev (arg)
  "Define a mode-specific abbrev whose expansion is the last word before point.
If there's an active region, use that as the expansion.

Prefix argument ARG says how many words before point to use for the expansion;
zero means the entire region is the expansion.

A negative ARG means to undefine the specified abbrev.

This command reads the abbreviation from the minibuffer.

See also `inverse-add-mode-abbrev', which performs the opposite task:
if the abbreviation is already in the buffer, use that command to define
a mode-specific abbrev by specifying its expansion in the minibuffer.

Don't use this function in a Lisp program; use `define-abbrev' instead."
  (interactive "P")
  (add-abbrev
   (if only-global-abbrevs
       global-abbrev-table
     (or local-abbrev-table
	 (error "No per-mode abbrev table")))
   "Mode" arg))

(defun add-global-abbrev (arg)
  "Define a global (all modes) abbrev whose expansion is last word before point.
If there's an active region, use that as the expansion.

Prefix argument ARG says how many words before point to use for the expansion;
zero means the entire region is the expansion.

A negative ARG means to undefine the specified abbrev.

This command reads the abbreviation from the minibuffer.

See also `inverse-add-global-abbrev', which performs the opposite task:
if the abbreviation is already in the buffer, use that command to define
a global abbrev by specifying its expansion in the minibuffer.

Don't use this function in a Lisp program; use `define-abbrev' instead."
  (interactive "P")
  (add-abbrev global-abbrev-table "Global" arg))

(defun add-abbrev (table type arg)
  "Define abbrev in TABLE, whose expansion is ARG words before point.
Read the abbreviation from the minibuffer, with prompt TYPE.

ARG of zero means the entire region is the expansion.

A negative ARG means to undefine the specified abbrev.

TYPE is an arbitrary string used to prompt user for the kind of
abbrev, such as \"Global\", \"Mode\".  (This has no influence on the
choice of the actual TABLE).

See `inverse-add-abbrev' for the opposite task.

Don't use this function in a Lisp program; use `define-abbrev' instead."
  (let ((exp
         (cond
          ((or (and (null arg) (use-region-p))
               (zerop (prefix-numeric-value arg)))
           (buffer-substring-no-properties (region-beginning) (region-end)))
          ((> (prefix-numeric-value arg) 0)
	   (buffer-substring-no-properties
	    (point)
	    (save-excursion
              (forward-word (- (prefix-numeric-value arg)))
              (point))))))
	name)
    (setq name
	  (read-string (format (if exp "%s abbrev that expands into \"%s\": "
				 "Undefine %s abbrev: ")
			       type exp)))
    (set-text-properties 0 (length name) nil name)
    (if (or (null exp)
	    (not (abbrev-expansion name table))
	    (y-or-n-p (format "%s expands into \"%s\"; redefine? "
                              name (abbrev-expansion name table))))
	(define-abbrev table (downcase name) exp))))

(defun inverse-add-mode-abbrev (n)
  "Define the word before point as a mode-specific abbreviation.
With prefix argument N, define the Nth word before point as the
abbreviation.  Negative N means use the Nth word after point.

If `only-global-abbrevs' is non-nil, this command defines a
global (mode-independent) abbrev instead of a mode-specific one.

This command reads the expansion from the minibuffer, defines the
abbrev, and then expands the abbreviation in the current buffer.

See also `add-mode-abbrev', which performs the opposite task:
if the expansion is already in the buffer, use that command
to define an abbrev by specifying the abbreviation in the minibuffer."
  (interactive "p")
  (inverse-add-abbrev
   (if only-global-abbrevs
       global-abbrev-table
     (or local-abbrev-table
	 (error "No per-mode abbrev table")))
   "Mode" n))

(defun inverse-add-global-abbrev (n)
  "Define the word before point as a global (mode-independent) abbreviation.
With prefix argument N, define the Nth word before point as the
abbreviation.  Negative N means use the Nth word after point.

This command reads the expansion from the minibuffer, defines the
abbrev, and then expands the abbreviation in the current buffer.

See also `add-global-abbrev', which performs the opposite task:
if the expansion is already in the buffer, use that command
to define an abbrev by specifying the abbreviation in the minibuffer."
  (interactive "p")
  (inverse-add-abbrev global-abbrev-table "Global" n))

(defun inverse-add-abbrev (table type arg)
  "Define the word before point as an abbrev in TABLE.
Read the expansion from the minibuffer, using prompt TYPE, define
the abbrev, and then expand the abbreviation in the current
buffer.

ARG means use the ARG-th word before point as the abbreviation.
Negative ARG means use the ARG-th word after point.

TYPE is an arbitrary string used to prompt user for the kind of
abbrev, such as \"Global\", \"Mode\".  (This has no influence on the
choice of the actual TABLE).

See also `add-abbrev', which performs the opposite task."
  (let (name exp start end)
    (save-excursion
      (forward-word (1+ (- arg)))
      (skip-syntax-backward "^w")
      (setq end (point))
      (backward-word 1)
      (setq start (point)
	    name (buffer-substring-no-properties start end)))

    (setq exp (read-string (format "Expansion for %s abbrev \"%s\": " type name)
			   nil nil nil t))
    (when (or (not (abbrev-expansion name table))
	      (y-or-n-p (format "%s expands into \"%s\"; redefine? "
				name (abbrev-expansion name table))))
      (define-abbrev table (downcase name) exp)
      (save-excursion
	(goto-char end)
	(expand-abbrev)))))

(defun abbrev-prefix-mark (&optional arg)
  "Mark point as the beginning of an abbreviation.
The abbrev to be expanded starts at point rather than at the
beginning of a word.  This way, you can expand an abbrev with
a prefix: insert the prefix, use this command, then insert the
abbrev.

This command inserts a hyphen after the prefix, and if the abbrev
is subsequently expanded, this hyphen will be removed.

If the prefix is itself an abbrev, this command expands it,
unless ARG is non-nil.  Interactively, ARG is the prefix
argument."
  (interactive "P")
  (or arg (expand-abbrev))
  (setq abbrev-start-location (point-marker)
	abbrev-start-location-buffer (current-buffer))
  (insert "-"))

(defun expand-region-abbrevs (start end &optional noquery)
  "For each abbrev occurrence in the region, offer to expand it.
Ask the user to type `y' or `n' for each occurrence.
A prefix argument means don't query; expand all abbrevs."
  (interactive "r\nP")
  (save-excursion
    (goto-char start)
    (let ((lim (- (point-max) end))
	  pnt string)
      (while (and (not (eobp))
		  (progn (forward-word 1)
			 (<= (setq pnt (point)) (- (point-max) lim))))
	(if (abbrev-expansion
	     (setq string
		   (buffer-substring-no-properties
		    (save-excursion (forward-word -1) (point))
		    pnt)))
	    (if (or noquery (y-or-n-p (format-message "Expand `%s'? " string)))
		(expand-abbrev)))))))

;;; Abbrev properties.

(defun abbrev-table-get (table prop)
  "Get the property PROP of abbrev table TABLE."
  (let ((sym (obarray-get table "")))
    (if sym (get sym prop))))

(defun abbrev-table-put (table prop val)
  "Set the property PROP of abbrev table TABLE to VAL."
  (let ((sym (obarray-put table "")))
    (set sym nil)	     ; Make sure it won't be confused for an abbrev.
    (put sym prop val)))

(defalias 'abbrev-get #'get
  "Get the property PROP of abbrev ABBREV
See `define-abbrev' for the effect of some special properties.

\(fn ABBREV PROP)")

(defalias 'abbrev-put #'put
  "Set the property PROP of abbrev ABBREV to value VAL.
See `define-abbrev' for the effect of some special properties.

\(fn ABBREV PROP VAL)")

;;; Code that used to be implemented in src/abbrev.c

(defvar abbrev-table-name-list '(fundamental-mode-abbrev-table
				 global-abbrev-table)
  "List of symbols whose values are abbrev tables.")

(defun make-abbrev-table (&optional props)
  "Create a new, empty abbrev table object.
PROPS is a list of properties."
  (let ((table (obarray-make)))
    ;; Each abbrev-table has a `modiff' counter which can be used to detect
    ;; when an abbreviation was added.  An example of use would be to
    ;; construct :regexp dynamically as the union of all abbrev names, so
    ;; `modiff' can let us detect that an abbrev was added and hence :regexp
    ;; needs to be refreshed.
    ;; The presence of `modiff' entry is also used as a tag indicating this
    ;; vector is really an abbrev-table.
    (abbrev-table-put table :abbrev-table-modiff 0)
    (while (consp props)
      (abbrev-table-put table (pop props) (pop props)))
    table))

(defun abbrev-table-p (object)
  "Return non-nil if OBJECT is an abbrev table."
  (and (obarrayp object)
       (numberp (ignore-error wrong-type-argument
                  (abbrev-table-get object :abbrev-table-modiff)))))

(defun abbrev-table-empty-p (object &optional ignore-system)
  "Return nil if there are no abbrev symbols in OBJECT.
If IGNORE-SYSTEM is non-nil, system definitions are ignored."
  (unless (abbrev-table-p object)
    (error "Non abbrev table object"))
  (not (catch 'some
	 (obarray-map (lambda (abbrev)
                        (unless (or (zerop (length (symbol-name abbrev)))
                                    (and ignore-system
                                         (abbrev-get abbrev :system)))
                          (throw 'some t)))
                      object))))

(defvar global-abbrev-table (make-abbrev-table)
  "The abbrev table whose abbrevs affect all buffers.
Each buffer may also have a local abbrev table.
If it does, the local table overrides the global one
for any particular abbrev defined in both.")

(defvar abbrev-minor-mode-table-alist nil
  "Alist of abbrev tables to use for minor modes.
Each element looks like (VARIABLE . ABBREV-TABLE);
ABBREV-TABLE is active whenever VARIABLE's value is non-nil;
VARIABLE is supposed to be a minor-mode variable.
ABBREV-TABLE can also be a list of abbrev tables.")

(defvar fundamental-mode-abbrev-table
  (let ((table (make-abbrev-table)))
    ;; Set local-abbrev-table's default to be fundamental-mode-abbrev-table.
    (setq-default local-abbrev-table table)
    table)
  "The abbrev table of mode-specific abbrevs for Fundamental Mode.")

(defvar abbrevs-changed nil
  "Non-nil if any word abbrevs were defined or altered.
This causes `save-some-buffers' to offer to save the abbrevs.")

(defcustom abbrev-all-caps nil
  "Non-nil means expand multi-word abbrevs in all caps if the abbrev was so."
  :type 'boolean)

(defvar abbrev-start-location nil
  "Buffer position for `expand-abbrev' to use as the start of the abbrev.
When nil, use the word before point as the abbrev.
Calling `expand-abbrev' sets this to nil.")

(defvar abbrev-start-location-buffer nil
  "Buffer that `abbrev-start-location' has been set for.
Trying to expand an abbrev in any other buffer clears `abbrev-start-location'.")

(defvar last-abbrev nil
  "The abbrev-symbol of the last abbrev expanded.  See `abbrev-symbol'.")

(defvar last-abbrev-text nil
  "The exact text of the last abbrev that was expanded.
It is nil if the abbrev has already been unexpanded.")

(defvar last-abbrev-location 0
  "The location of the start of the last abbrev that was expanded.")

;; (defvar-local local-abbrev-table fundamental-mode-abbrev-table
;;   "Local (mode-specific) abbrev table of current buffer.")

(defun clear-abbrev-table (table)
  "Undefine all abbrevs in abbrev table TABLE, leaving TABLE empty."
  (setq abbrevs-changed t)
  (let* ((sym (obarray-get table "")))
    (obarray-clear table)
    ;; Preserve the table's properties.
    (cl-assert sym)
    (let ((newsym (obarray-put table "")))
      (set newsym nil)	     ; Make sure it won't be confused for an abbrev.
      (setplist newsym (symbol-plist sym)))
    (abbrev-table-put table :abbrev-table-modiff
                      (1+ (abbrev-table-get table :abbrev-table-modiff))))
  ;; For backward compatibility, always return nil.
  nil)

(defun define-abbrev (table abbrev expansion &optional hook &rest props)
  "Define ABBREV in TABLE, to expand into EXPANSION and optionally call HOOK.
ABBREV must be a string, and should be lower-case.
EXPANSION should usually be a string.
To undefine an abbrev, define it with EXPANSION = nil.
If HOOK is non-nil, it should be a function of no arguments;
it is called after EXPANSION is inserted.
If EXPANSION is not a string (and not nil), the abbrev is a
 special one, which does not expand in the usual way but only
 runs HOOK.

If HOOK is a non-nil symbol with a non-nil `no-self-insert' property,
it can control whether the character that triggered abbrev expansion
is inserted.  If such a HOOK returns non-nil, the character is not
inserted.  If such a HOOK returns nil, then so does `abbrev-insert'
\(and `expand-abbrev'), as if no abbrev expansion had taken place.

PROPS is a property list.  The following properties are special:
- `:count': the value for the abbrev's usage-count, which is incremented each
  time the abbrev is used (the default is zero).
- `:system': if non-nil, says that this is a \"system\" abbreviation
  which should not be saved in the user's abbreviation file.
  Unless `:system' is `force', a system abbreviation will not
  overwrite a non-system abbreviation of the same name.
- `:case-fixed': non-nil means that abbreviations are looked up without
  case-folding, and the expansion is not capitalized/upcased.
- `:enable-function': a function of no arguments which returns non-nil
  if the abbrev should be used for a particular call of `expand-abbrev'.

An obsolete but still supported calling form is:

\(define-abbrev TABLE NAME EXPANSION &optional HOOK COUNT SYSTEM)."
  (declare (indent defun))
  (when (and (consp props) (or (null (car props)) (numberp (car props))))
    ;; Old-style calling convention.
    (setq props `(:count ,(car props)
                  ,@(if (cadr props) (list :system (cadr props))))))
  (unless (plist-get props :count)
    (setq props (plist-put props :count 0)))
  (setq props (plist-put props :abbrev-table-modiff
                         (abbrev-table-get table :abbrev-table-modiff)))
  (let ((system-flag (plist-get props :system))
        (sym (obarray-put table abbrev)))
    ;; Don't override a prior user-defined abbrev with a system abbrev,
    ;; unless system-flag is `force'.
    (unless (and (not (memq system-flag '(nil force)))
                 (boundp sym) (symbol-value sym)
                 (not (abbrev-get sym :system)))
      (unless (or system-flag
                  (and (boundp sym)
                       ;; load-file-name
                       (equal (symbol-value sym) expansion)
                       (equal (symbol-function sym) hook)))
        (setq abbrevs-changed t))
      (set sym expansion)
      (fset sym hook)
      (setplist sym
                ;; Don't store the `force' value of `system-flag' into
                ;; the :system property.
                (if (eq 'force system-flag) (plist-put props :system t) props))
      (abbrev-table-put table :abbrev-table-modiff
                        (1+ (abbrev-table-get table :abbrev-table-modiff))))
    abbrev))

(defun abbrev--check-chars (abbrev global)
  "Check if the characters in ABBREV have word syntax in either the
current (if global is nil) or standard syntax table."
  (with-syntax-table
      (cond ((null global) (syntax-table))
            ;; ((syntax-table-p global) global)
            (t (standard-syntax-table)))
    (when (string-match "\\W" abbrev)
      (let ((badchars ())
            (pos 0))
        (while (string-match "\\W" abbrev pos)
          (cl-pushnew (aref abbrev (match-beginning 0)) badchars)
          (setq pos (1+ pos)))
        (error "Some abbrev characters (%s) are not word constituents %s"
               (apply #'string (nreverse badchars))
               (if global "in the standard syntax" "in this mode"))))))

(defun define-global-abbrev (abbrev expansion)
  "Define ABBREV as a global abbreviation that expands into EXPANSION.
The characters in ABBREV must all be word constituents in the standard
syntax table."
  (interactive "sDefine global abbrev: \nsExpansion for %s: ")
  (abbrev--check-chars abbrev 'global)
  (define-abbrev global-abbrev-table (downcase abbrev) expansion))

(defun define-mode-abbrev (abbrev expansion)
  "Define ABBREV as a mode-specific abbreviation that expands into EXPANSION.
The characters in ABBREV must all be word-constituents in the current mode."
  (interactive "sDefine mode abbrev: \nsExpansion for %s: ")
  (unless local-abbrev-table
    (error "Major mode has no abbrev table"))
  (abbrev--check-chars abbrev nil)
  (define-abbrev local-abbrev-table (downcase abbrev) expansion))

(defun abbrev--active-tables (&optional tables)
  "Return the list of abbrev tables that are currently active.
TABLES, if non-nil, overrides the usual rules.  It can hold
either a single abbrev table or a list of abbrev tables."
  ;; We could just remove the `tables' arg and let callers use
  ;; (or table (abbrev--active-tables)) but then they'd have to be careful
  ;; to treat the distinction between a single table and a list of tables.
  (cond
   ((consp tables) tables)
   ((obarrayp tables) (list tables))
   (t
    (let ((tables (if (listp local-abbrev-table)
                      (append local-abbrev-table
                              (list global-abbrev-table))
                    (list local-abbrev-table global-abbrev-table))))
      ;; Add the minor-mode abbrev tables.
      (dolist (x abbrev-minor-mode-table-alist)
        (when (and (symbolp (car x)) (boundp (car x)) (symbol-value (car x)))
          (setq tables
                (if (listp (cdr x))
                    (append (cdr x) tables) (cons (cdr x) tables)))))
      tables))))


(defun abbrev--symbol (abbrev table)
  "Return the symbol representing abbrev named ABBREV in TABLE.
This symbol's name is ABBREV, but it is not the canonical symbol of that name;
it is interned in the abbrev-table TABLE rather than the normal obarray.
The value is nil if such an abbrev is not defined."
  (let* ((case-fold (not (abbrev-table-get table :case-fixed)))
         ;; In case the table doesn't set :case-fixed but some of the
         ;; abbrevs do, we have to be careful.
         (sym
          ;; First try without case-folding.
          (or (obarray-get table abbrev)
              (when case-fold
                ;; We didn't find any abbrev, try case-folding.
                (let ((sym (obarray-get table (downcase abbrev))))
                  ;; Only use it if it doesn't require :case-fixed.
                  (and sym (not (abbrev-get sym :case-fixed))
                       sym))))))
    (if (symbol-value sym)
        sym)))

(defun abbrev-symbol (abbrev &optional table)
  "Return the symbol representing the abbrev named ABBREV in TABLE.
This symbol's name is ABBREV, but it is not the canonical symbol of that name;
it is interned in an abbrev-table rather than the normal obarray.
The value is nil if such an abbrev is not defined.
Optional second arg TABLE is the abbrev table to look it up in.
The default is to try buffer's mode-specific abbrev table, then global table."
  (let ((tables (abbrev--active-tables table))
        sym)
    (while (and tables (not sym))
      (let* ((table (pop tables)))
        (setq tables (append (abbrev-table-get table :parents) tables))
        (setq sym (abbrev--symbol abbrev table))))
    sym))


(defun abbrev-expansion (abbrev &optional table)
  "Return the string that ABBREV expands into in the current buffer.
Optionally specify an abbrev TABLE as second arg;
then ABBREV is looked up in that table only."
  (symbol-value (abbrev-symbol abbrev table)))


(defun abbrev--before-point ()
  "Try and find an abbrev before point.  Return it if found, nil otherwise."
  (unless (eq abbrev-start-location-buffer (current-buffer))
    (setq abbrev-start-location nil))

  (let ((tables (abbrev--active-tables))
        (pos (point))
        start end name res)

    (if abbrev-start-location
        (progn
          (setq start abbrev-start-location)
          (setq abbrev-start-location nil)
          ;; Remove the hyphen inserted by `abbrev-prefix-mark'.
          (when (and (< start (point-max))
                     (eq (char-after start) ?-))
            (delete-region start (1+ start))
            (setq pos (1- pos)))
          (skip-syntax-backward " ")
          (setq end (point))
          (when (> end start)
            (setq name (buffer-substring start end))
            (goto-char pos)               ; Restore point.
            (list (abbrev-symbol name tables) name start end)))

      (while (and tables (not (car res)))
        (let* ((table (pop tables))
               (enable-fun (abbrev-table-get table :enable-function)))
          (setq tables (append (abbrev-table-get table :parents) tables))
          (setq res
                (and (or (not enable-fun) (funcall enable-fun))
                     (let ((re (abbrev-table-get table :regexp)))
                       (if (null re)
                           ;; We used to default `re' to "\\<\\(\\w+\\)\\W*"
                           ;; but when words-include-escapes is set, that
                           ;; is not right and fixing it is boring.
                           (let ((lim (point)))
                             (backward-word 1)
                             (setq start (point))
                             (forward-word 1)
                             (setq end (min (point) lim)))
                         (when (looking-back re (line-beginning-position))
                           (setq start (match-beginning 1))
                           (setq end   (match-end 1)))))
                     (setq name  (buffer-substring start end))
                     (let ((abbrev (abbrev--symbol name table)))
                       (when abbrev
                         (setq enable-fun (abbrev-get abbrev :enable-function))
                         (and (or (not enable-fun) (funcall enable-fun))
                              ;; This will also look it up in parent tables.
                              ;; This is not on purpose, but it seems harmless.
                              (list abbrev name start end))))))
          ;; Restore point.
          (goto-char pos)))
      res)))

(defun abbrev-insert (abbrev &optional name wordstart wordend)
  "Insert abbrev ABBREV at point.
If non-nil, NAME is the name by which this abbrev was found.
If non-nil, WORDSTART is the buffer position where to insert the abbrev.
If WORDEND is non-nil, it is a buffer position; the abbrev replaces the
previous text between WORDSTART and WORDEND.
Return ABBREV if the expansion should be considered as having taken place.
The return value can be influenced by a `no-self-insert' property;
see `define-abbrev' for details."
  (unless name (setq name (symbol-name abbrev)))
  (unless wordstart (setq wordstart (point)))
  (unless wordend (setq wordend wordstart))
  ;; Increment use count.
  (abbrev-put abbrev :count (1+ (abbrev-get abbrev :count)))
  (let ((value abbrev))
    ;; If this abbrev has an expansion, delete the abbrev
    ;; and insert the expansion.
    (when (stringp (symbol-value abbrev))
      (goto-char wordstart)
      ;; Insert at beginning so that markers at the end (e.g. point)
      ;; are preserved.
      (insert (symbol-value abbrev))
      (delete-char (- wordend wordstart))
      (let ((case-fold-search nil))
        ;; If the abbrev's name is different from the buffer text (the
        ;; only difference should be capitalization), then we may want
        ;; to adjust the capitalization of the expansion.
        (when (and (not (equal name (symbol-name abbrev)))
                   (string-match "[[:upper:]]" name))
          (if (not (string-match "[[:lower:]]" name))
              ;; Abbrev was all caps.  If expansion is multiple words,
              ;; normally capitalize each word.
              (if (and (not abbrev-all-caps)
                       (save-excursion
                         (> (progn (backward-word 1) (point))
                            (progn (goto-char wordstart)
                                   (forward-word 1) (point)))))
                  (upcase-initials-region wordstart (point))
                (upcase-region wordstart (point)))
            ;; Abbrev included some caps.  Cap first initial of expansion.
            (let ((end (point)))
              ;; Find the initial.
              (goto-char wordstart)
              (skip-syntax-forward "^w" (1- end))
              ;; Change just that.
              (upcase-initials-region (point) (1+ (point)))
              (goto-char end))))))
    ;; Now point is at the end of the expansion and the beginning is
    ;; in last-abbrev-location.
    (when (symbol-function abbrev)
      (let* ((hook (symbol-function abbrev))
             (expanded
              ;; If the abbrev has a hook function, run it.
              (funcall hook)))
        ;; In addition, if the hook function is a symbol with
        ;; a non-nil `no-self-insert' property, let the value it
        ;; returned specify whether we consider that an expansion took
        ;; place.  If it returns nil, no expansion has been done.
        (if (and (symbolp hook)
                 (null expanded)
                 (get hook 'no-self-insert))
            (setq value nil))))
    value))

(defvar abbrev-expand-functions nil
  "Wrapper hook around `abbrev--default-expand'.")
(make-obsolete-variable 'abbrev-expand-functions 'abbrev-expand-function "24.4")

(defvar abbrev-expand-function #'abbrev--default-expand
  "Function that `expand-abbrev' uses to perform abbrev expansion.
Takes no arguments, and should return the abbrev symbol if expansion
took place.")

(defcustom abbrev-suggest nil
  "Non-nil means suggest using abbrevs to save typing.
When abbrev mode is active and this option is non-nil, Emacs will
suggest in the echo area to use an existing abbrev if doing so
will save enough typing.  See `abbrev-suggest-hint-threshold' for
the definition of \"enough typing\"."
    :type 'boolean
    :version "28.1")

(defcustom abbrev-suggest-hint-threshold 3
  "Threshold for when to suggest to use an abbrev to save typing.
The threshold is the amount of typing, in terms of the number of
characters, that would be saved by using the abbrev.  The
thinking is that if the expansion is only a few characters
longer than the abbrev, the benefit of informing the user is not
significant.  If you always want to be informed about existing
abbrevs for the text you type, set this value to zero or less.
This setting only applies if `abbrev-suggest' is non-nil."
  :type 'natnum
  :version "28.1")

(defun abbrev--suggest-get-active-tables-including-parents ()
  "Return a list of all active abbrev tables, including parent tables."
  (let* ((tables (abbrev--active-tables))
	 (all tables))
    (dolist (table tables)
      (setq all (append (abbrev-table-get table :parents) all)))
    all))

(defun abbrev--suggest-get-active-abbrev-expansions ()
  "Return a list of all the active abbrev expansions.
Includes expansions from parent abbrev tables."
    (let (expansions)
      (dolist (table (abbrev--suggest-get-active-tables-including-parents))
	(mapatoms (lambda (e)
		    (let ((value (symbol-value (abbrev--symbol e table))))
		      (when value
                        (push (cons value (symbol-name e)) expansions))))
		  table))
      expansions))

(defun abbrev--suggest-count-words (expansion)
  "Return the number of words in EXPANSION.
Expansion is a string of one or more words."
    (length (split-string expansion " " t)))

(defun abbrev--suggest-get-previous-words (n)
  "Return the N words before point, spaces included."
    (let ((end (point)))
      (save-excursion
	(backward-word n)
	(replace-regexp-in-string
	 "\\s " " "
	 (buffer-substring-no-properties (point) end)))))

(defun abbrev--suggest-above-threshold (expansion)
  "Return non-nil if the abbrev in EXPANSION provides significant savings.
A significant saving, here, means the difference in length between
the abbrev and its expansion is not below the threshold specified
by the value of `abbrev-suggest-hint-threshold'.
EXPANSION is a cons cell where the car is the expansion and the cdr is
the abbrev."
    (>= (- (length (car expansion))
	   (length (cdr expansion)))
	abbrev-suggest-hint-threshold))

(defvar abbrev--suggest-saved-recommendations nil
    "Keeps the list of expansions that have abbrevs defined.
The user can show this list by calling
`abbrev-suggest-show-report'.")

(defun abbrev--suggest-inform-user (expansion)
    "Display a message to the user about the existing abbrev.
EXPANSION is a cons cell where the `car' is the expansion and the
`cdr' is the abbrev."
    (run-with-idle-timer
     1 nil
     (lambda ()
       (message "You can write `%s' using the abbrev `%s'."
                                   (car expansion) (cdr expansion))))
    (push expansion abbrev--suggest-saved-recommendations))

(defun abbrev--suggest-shortest-abbrev (new current)
    "Return the shortest of the two abbrevs given by NEW and CURRENT.
NEW and CURRENT are cons cells where the `car' is the expansion
and the `cdr' is the abbrev."
    (if (not current)
	new
      (if (< (length (cdr new))
	     (length (cdr current)))
	  new
	current)))

(defun abbrev--suggest-maybe-suggest ()
    "Suggest an abbrev to the user based on the word(s) before point.
Uses `abbrev-suggest-hint-threshold' to find out if the user should be
informed about the existing abbrev."
    (let (words abbrev-found word-count)
      (dolist (expansion (abbrev--suggest-get-active-abbrev-expansions))
	(setq word-count (abbrev--suggest-count-words (car expansion))
	      words (abbrev--suggest-get-previous-words word-count))
	(let ((case-fold-search t))
	  (when (and (> word-count 0)
		     (string-match (car expansion) words)
		     (abbrev--suggest-above-threshold expansion))
	    (setq abbrev-found (abbrev--suggest-shortest-abbrev
				expansion abbrev-found)))))
      (when abbrev-found
	(abbrev--suggest-inform-user abbrev-found))))

(defun abbrev--suggest-get-totals ()
    "Return a list of all expansions and how many times they were used.
Each expansion in the returned list is a cons cell where the `car' is the
expansion text and the `cdr' is the number of times the expansion has been
typed."
    (let (total cell)
      (dolist (expansion abbrev--suggest-saved-recommendations)
	(if (not (assoc (car expansion) total))
	    (push (cons (car expansion) 1) total)
	  (setq cell (assoc (car expansion) total))
	  (setcdr cell (1+ (cdr cell)))))
      total))

(defun abbrev-suggest-show-report ()
  "Show a buffer with the list of abbrevs you could have used.
This shows the abbrevs you've \"missed\" because you typed the
full text instead of the abbrevs that expand into that text."
  (interactive)
  (let ((totals (abbrev--suggest-get-totals))
	(buf (get-buffer-create "*abbrev-suggest*")))
    (set-buffer buf)
    (erase-buffer)
        (insert (substitute-command-keys "** Abbrev expansion usage **

Below is a list of expansions for which abbrevs are defined, and
the number of times the expansion was typed manually.  To display
and edit all abbrevs, type \\[edit-abbrevs].\n\n"))
	(dolist (expansion totals)
	  (insert (format " %s: %d\n" (car expansion) (cdr expansion))))
	(display-buffer buf)))

(defun expand-abbrev ()
  "Expand the abbrev before point, if there is an abbrev there.
Effective when explicitly called even when `abbrev-mode' is nil.
Calls the value of `abbrev-expand-function' with no argument to do
the work, and returns whatever it does.  (That return value should
be the abbrev symbol if expansion occurred, else nil.)"
  (interactive)
  (or (funcall abbrev-expand-function)
      (if abbrev-suggest
          (abbrev--suggest-maybe-suggest))))

(defun abbrev--default-expand ()
  "Default function to use for `abbrev-expand-function'.
This also respects the obsolete wrapper hook `abbrev-expand-functions'.
\(See `with-wrapper-hook' for details about wrapper hooks.)
Calls `abbrev-insert' to insert any expansion, and returns what it does."
  (subr--with-wrapper-hook-no-warnings abbrev-expand-functions ()
    (pcase-let ((`(,sym ,name ,wordstart ,wordend) (abbrev--before-point)))
      (when sym
        (let ((startpos (copy-marker (point) t))
              (endmark (copy-marker wordend t)))
          (unless (or ;; executing-kbd-macro
                   noninteractive
                   (window-minibuffer-p))
            ;; Add an undo boundary, in case we are doing this for
            ;; a self-inserting command which has avoided making one so far.
            (undo-boundary))
          ;; Now sym is the abbrev symbol.
          (setq last-abbrev-text name)
          (setq last-abbrev sym)
          (setq last-abbrev-location wordstart)
          ;; If this abbrev has an expansion, delete the abbrev
          ;; and insert the expansion.
          (prog1
              (abbrev-insert sym name wordstart wordend)
            ;; Yuck!!  If expand-abbrev is called with point slightly
            ;; further than the end of the abbrev, move point back to
            ;; where it started.
            (if (and (> startpos endmark)
                     (= (point) endmark)) ;Obey skeletons that move point.
                (goto-char startpos))))))))

(defun unexpand-abbrev ()
  "Undo the expansion of the last abbrev that expanded.
This differs from ordinary undo in that other editing done since then
is not undone."
  (interactive)
  (save-excursion
    (when (<= (point-min) last-abbrev-location (point-max))
      (goto-char last-abbrev-location)
      (when (stringp last-abbrev-text)
        ;; This isn't correct if last-abbrev's hook was used
        ;; to do the expansion.
        (let ((val (symbol-value last-abbrev)))
          (unless (stringp val)
            (error "Value of abbrev-symbol must be a string"))
          ;; Don't inherit properties here; just copy from old contents.
          (replace-region-contents (point) (+ (point) (length val))
                                   last-abbrev-text 0)
          (goto-char (+ (point) (length last-abbrev-text)))
          (setq last-abbrev-text nil))))))

(defun abbrev--write (sym)
  "Write the abbrev in a `read'able form.
Presumes that `standard-output' points to `current-buffer'."
  (insert "    (")
  (prin1 (symbol-name sym))
  (insert " ")
  (prin1 (symbol-value sym))
  (insert " ")
  (prin1 (symbol-function sym))
  (insert " :count ")
  (prin1 (abbrev-get sym :count))
  (when (abbrev-get sym :case-fixed)
    (insert " :case-fixed ")
    (prin1 (abbrev-get sym :case-fixed)))
  (when (abbrev-get sym :enable-function)
    (insert " :enable-function ")
    (prin1 (abbrev-get sym :enable-function)))
  (insert ")\n"))

(defun abbrev--describe (sym)
  "Describe abbrev SYM.
Print on `standard-output' the abbrev, count of use, expansion."
  (when (symbol-value sym)
    (prin1 (symbol-name sym))
    (if (null (abbrev-get sym :system))
        (indent-to 15 1)
      (insert " (sys)")
      (indent-to 20 1))
    (prin1 (abbrev-get sym :count))
    (indent-to 20 1)
    (prin1 (symbol-value sym))
    (when (symbol-function sym)
      (indent-to 45 1)
      (prin1 (symbol-function sym)))
    (terpri)))

(defun insert-abbrev-table-description (name &optional readable)
  "Insert before point a full description of abbrev table named NAME.
NAME is a symbol whose value is an abbrev table.
If optional 2nd arg READABLE is non-nil, insert a human-readable
description.

If READABLE is nil, insert an expression.  The expression is
a call to `define-abbrev-table' that, when evaluated, will define
the abbrev table NAME exactly as it is currently defined.
Abbrevs marked as \"system abbrevs\" are ignored."
  (let ((symbols (abbrev--table-symbols name readable)))
    (setq symbols (sort symbols #'string-lessp))
    (let ((standard-output (current-buffer)))
      (if readable
          (progn
            (insert "(")
            (prin1 name)
            (insert ")\n\n")
            (mapc #'abbrev--describe symbols)
            (insert "\n\n"))
        (insert "(define-abbrev-table '")
        (prin1 name)
        (if (null symbols)
            (insert " '())\n\n")
          (insert "\n  '(\n")
          (mapc #'abbrev--write symbols)
          (insert "   ))\n\n")))
      nil)))

(defun abbrev--table-symbols (name &optional system)
  "Return the user abbrev symbols in the abbrev table named NAME.
NAME is a symbol whose value is an abbrev table.  System abbrevs
are omitted unless SYSTEM is non-nil."
  (let ((table (symbol-value name))
        (symbols ()))
    (mapatoms (lambda (sym)
                (if (and (symbol-value sym) (or system (not (abbrev-get sym :system))))
                    (push sym symbols)))
              table)
    symbols))

(defun define-abbrev-table (tablename definitions
                                      &optional docstring &rest props)
  "Define TABLENAME (a symbol) as an abbrev table name.
Define abbrevs in it according to DEFINITIONS, which is a list of elements
of the form (ABBREVNAME EXPANSION ...) that are passed to `define-abbrev'.
PROPS is a property list to apply to the table.
Properties with special meaning:
- `:parents' contains a list of abbrev tables from which this table inherits
  abbreviations.
- `:case-fixed' non-nil means that abbreviations are looked up without
  case-folding, and the expansion is not capitalized/upcased.
- `:regexp' is a regular expression that specifies how to extract the
  name of the abbrev before point.  The submatch 1 is treated
  as the potential name of an abbrev.  If `:regexp' is nil, the default
  behavior uses `backward-word' and `forward-word' to extract the name
  of the abbrev, which can therefore by default only be a single word.
- `:enable-function' can be set to a function of no arguments which returns
  non-nil if and only if the abbrevs in this table should be used for this
  instance of `expand-abbrev'."
  (declare (doc-string 3) (indent defun))
  ;; We used to manually add the docstring, but we also want to record this
  ;; location as the definition of the variable (in load-history), so we may
  ;; as well just use `defvar'.
  (when (and docstring props (symbolp docstring))
    ;; There is really no docstring, instead the docstring arg
    ;; is a property name.
    (push docstring props) (setq docstring nil))
  (defvar-1 tablename nil docstring)
  (let ((table (if (boundp tablename) (symbol-value tablename))))
    (unless table
      (setq table (make-abbrev-table))
      (set tablename table)
      (unless (memq tablename abbrev-table-name-list)
        (push tablename abbrev-table-name-list)))
    ;; We used to just pass them to `make-abbrev-table', but that fails
    ;; if the table was pre-existing as is the case if it was created by
    ;; loading the user's abbrev file.
    (while (consp props)
      (unless (cdr props) (error "Missing value for property %S" (car props)))
      (abbrev-table-put table (pop props) (pop props)))
    (dolist (elt definitions)
      (apply #'define-abbrev table elt))))

(defun abbrev-table-menu (table &optional prompt sortfun)
  "Return a menu that shows all abbrevs in TABLE.
Selecting an entry runs `abbrev-insert' for that entry's abbrev.
PROMPT is the prompt to use for the keymap.
SORTFUN is passed to `sort' to change the default ordering."
  (unless sortfun (setq sortfun 'string-lessp))
  (let ((entries ()))
    (obarray-map (lambda (abbrev)
                   (when (symbol-value abbrev)
                     (let ((name (symbol-name abbrev)))
                       (push `(,(intern name) menu-item ,name
                               (lambda () (interactive)
                                 (abbrev-insert ',abbrev)))
                             entries))))
                 table)
    (nconc (make-sparse-keymap prompt)
           (sort entries (lambda (x y)
                           (funcall sortfun (nth 2 x) (nth 2 y)))))))

(defface abbrev-table-name
  '((t :inherit font-lock-function-name-face))
  "Face used for displaying the abbrev table name in `edit-abbrevs-mode'."
  :version "29.1")

(defvar edit-abbrevs-mode-font-lock-keywords
  `((,(rx bol "("
          ;; lisp-mode-symbol-regexp
          (regexp "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
          ")" eol)
     0 'abbrev-table-name)))

;; Keep it after define-abbrev-table, since define-derived-mode uses
;; define-abbrev-table.
(define-derived-mode edit-abbrevs-mode fundamental-mode "Edit-Abbrevs"
  "Major mode for editing the list of abbrev definitions.
This mode is for editing abbrevs in a buffer prepared by `edit-abbrevs',
which see."
  :interactive nil
  (setq-local font-lock-defaults
              '(edit-abbrevs-mode-font-lock-keywords nil nil ((?_ . "w"))))
  (setq font-lock-multiline nil))

(defun abbrev--possibly-save (query &optional arg)
  "Hook function for use by `save-some-buffers-functions'.

Maybe save abbrevs, and record whether we either saved them or asked to."
  ;; Query mode.
  (if (eq query 'query)
      (and save-abbrevs abbrevs-changed)
    (and save-abbrevs
         abbrevs-changed
         (prog1
	     (if (or arg
		     (eq save-abbrevs 'silently)
		     (y-or-n-p (format "Save abbrevs in %s? " abbrev-file-name)))
	         (progn
                   (write-abbrev-file nil)
                   nil)
               ;; Inhibit message in `save-some-buffers'.
	       t)
           ;; Don't ask again whether saved or user said no.
           (setq abbrevs-changed nil)))))

(add-hook 'save-some-buffers-functions #'abbrev--possibly-save)

(provide 'abbrev)

;;; abbrev.el ends here
