;;; derived.el --- allow inheritance of major modes  -*- lexical-binding: t; -*-
;; (formerly mode-clone.el)

;; Copyright (C) 1993-2025 Free Software Foundation, Inc.

;; Author: David Megginson <dmeggins@aix1.uottawa.ca>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: extensions
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

;; GNU Emacs is already, in a sense, object oriented -- each object
;; (buffer) belongs to a class (major mode), and that class defines
;; the relationship between messages (input events) and methods
;; (commands) by means of a keymap.
;;
;; The only thing missing is a good scheme of inheritance.  It is
;; possible to simulate a single level of inheritance with generous
;; use of hooks and a bit of work -- sgml-mode, for example, also runs
;; the hooks for text-mode, and keymaps can inherit from other keymaps
;; -- but generally, each major mode ends up reinventing the wheel.
;; Ideally, someone should redesign all of Emacs's major modes to
;; follow a more conventional object-oriented system: when defining a
;; new major mode, the user should need only to name the existing mode
;; it is most similar to, then list the (few) differences.
;;
;; In the mean time, this package offers most of the advantages of
;; full inheritance with the existing major modes.  The macro
;; `define-derived-mode' allows the user to make a variant of an existing
;; major mode, with its own keymap.  The new mode will inherit the key
;; bindings of its parent, and will, in fact, run its parent first
;; every time it is called.  For example, the commands
;;
;;  (define-derived-mode hypertext-mode text-mode "Hypertext"
;;    "Major mode for hypertext.\n\n\\{hypertext-mode-map}"
;;    (setq case-fold-search nil))
;;
;;  (define-key hypertext-mode-map [down-mouse-3] 'do-hyper-link)
;;
;; will create a function `hypertext-mode' with its own (sparse)
;; keymap `hypertext-mode-map.'  The command M-x hypertext-mode will
;; perform the following actions:
;;
;; - run the command (text-mode) to get its default setup
;; - replace the current keymap with 'hypertext-mode-map,' which will
;;   inherit from 'text-mode-map'.
;; - replace the current syntax table with
;;   'hypertext-mode-syntax-table', which will borrow its defaults
;;   from the current text-mode-syntax-table.
;; - replace the current abbrev table with
;;   'hypertext-mode-abbrev-table', which will borrow its defaults
;;   from the current text-mode-abbrev table
;; - change the mode line to read "Hypertext"
;; - assign the value 'hypertext-mode' to the 'major-mode' variable
;; - run the body of commands provided in the macro -- in this case,
;;   set the local variable `case-fold-search' to nil.
;;
;; The advantages of this system are threefold.  First, text mode is
;; untouched -- if you had added the new keystroke to `text-mode-map,'
;; possibly using hooks, you would have added it to all text buffers
;; -- here, it appears only in hypertext buffers, where it makes
;; sense.  Second, it is possible to build even further, and make
;; a derived mode from a derived mode.  The commands
;;
;;   (define-derived-mode html-mode hypertext-mode "HTML")
;;   [various key definitions]
;;
;; will add a new major mode for HTML with very little fuss.
;;
;; Note also the function `derived-mode-p' which can tell if the current
;; mode derives from another.  In a hypertext-mode, buffer, for example,
;; (derived-mode-p 'text-mode) would return non-nil.  This should always
;; be used in place of (eq major-mode 'text-mode).

;;; Code:

;;; PRIVATE: defsubst must be defined before they are first used

(defsubst derived-mode-hook-name (mode)
  "Construct a mode-hook name based on the symbol MODE."
  (intern (concat (symbol-name mode) "-hook")))

(defsubst derived-mode-map-name (mode)
  "Construct a map name based on the symbol MODE."
  (intern (concat (symbol-name mode) "-map")))

(defsubst derived-mode-syntax-table-name (mode)
  "Construct a syntax-table name based on the symbol MODE."
  (intern (concat (symbol-name mode) "-syntax-table")))

(defsubst derived-mode-abbrev-table-name (mode)
  "Construct an abbrev-table name based on the symbol MODE."
  (intern (concat (symbol-name mode) "-abbrev-table")))

;; PUBLIC: define a new major mode which inherits from an existing one.

;;;###autoload
(defmacro define-derived-mode (child parent name &optional docstring &rest body)
  "Create a new mode CHILD which is a variant of an existing mode PARENT.

The arguments are as follows:

CHILD:     the name of the command for the derived mode.
PARENT:    the name of the command for the parent mode (e.g. `text-mode')
           or nil if there is no parent.
NAME:      a string that will appear in the mode line (e.g. \"HTML\")
DOCSTRING: an optional documentation string--if you do not supply one,
           the function will attempt to invent something useful.
KEYWORD-ARGS:
           optional arguments in the form of pairs of keyword and value.
           The following keyword arguments are currently supported:

           :group GROUP
                   Declare the customization group that corresponds
                   to this mode.  The command `customize-mode' uses this.
           :syntax-table TABLE
                   Use TABLE instead of the default (CHILD-syntax-table).
                   A nil value means to simply use the same syntax-table
                   as the parent.
           :abbrev-table TABLE
                   Use TABLE instead of the default (CHILD-abbrev-table).
                   A nil value means to simply use the same abbrev-table
                   as the parent.
           :after-hook FORM
                   A single Lisp form which is evaluated after the mode
                   hooks have been run.  It should not be quoted.
           :interactive BOOLEAN
                   Whether the derived mode should be `interactive' or not.
                   The default is t.

BODY:      forms to execute just before running the
           hooks for the new mode.  Do not use `interactive' here.

Here is how you could define LaTeX-Thesis mode as a variant of LaTeX mode:

  (define-derived-mode LaTeX-thesis-mode LaTeX-mode \"LaTeX-Thesis\")

You could then make new key bindings for `LaTeX-thesis-mode-map'
without changing regular LaTeX mode.  In this example, BODY is empty,
and DOCSTRING is generated by default.

As a more complex example, the following command uses `sgml-mode' as
the parent, and then sets the variable `case-fold-search' to nil:

  (define-derived-mode article-mode sgml-mode \"Article\"
    \"Major mode for editing technical articles.\"
    (setq case-fold-search nil))

Note that if the documentation string had been left out, it would have
been generated automatically, with a reference to the keymap.

The new mode runs the hook named MODE-hook.  For `foo-mode',
the hook will be named `foo-mode-hook'.

See Info node `(elisp)Derived Modes' for more details.

\(fn CHILD PARENT NAME [DOCSTRING] [KEYWORD-ARGS...] &rest BODY)"
  (declare (debug (&define name symbolp sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
	   (doc-string 4)
	   (indent defun))

  (when (and docstring (not (stringp docstring)))
    ;; Some trickiness, since what appears to be the docstring may really be
    ;; the first element of the body.
    (push docstring body)
    (setq docstring nil))

  (when (eq parent 'fundamental-mode) (setq parent nil))

  (let ((map (derived-mode-map-name child))
	(syntax (derived-mode-syntax-table-name child))
	(abbrev (derived-mode-abbrev-table-name child))
	(declare-abbrev t)
	(declare-syntax t)
	(hook (derived-mode-hook-name child))
	(group nil)
        (interactive t)
        (after-hook nil))

    ;; Process the keyword args.
    (while (keywordp (car body))
      (pcase (pop body)
	(:group (setq group (pop body)))
	(:abbrev-table (setq abbrev (pop body)) (setq declare-abbrev nil))
	(:syntax-table (setq syntax (pop body)) (setq declare-syntax nil))
        (:after-hook (setq after-hook (pop body)))
        (:interactive (setq interactive (pop body)))
	(_ (pop body))))

    (setq docstring (derived-mode-make-docstring
		     parent child docstring syntax abbrev))

    `(progn
       (defvar ,hook nil)
       (unless (get ',hook 'variable-documentation)
         (put ',hook 'variable-documentation
              ,(format "Hook run after entering `%S'.
No problems result if this variable is not bound.
`add-hook' automatically binds it.  (This is true for all hook variables.)"
                       child)))
       (unless (boundp ',map)
	 (put ',map 'definition-name ',child))
       (with-no-warnings (defvar-keymap ,map))
       (unless (get ',map 'variable-documentation)
	 (put ',map 'variable-documentation
              ,(format "Keymap for `%s'." child)))
       ,(if declare-syntax
	    `(progn
               (defvar ,syntax)
	       (unless (boundp ',syntax)
		 (put ',syntax 'definition-name ',child)
		 (defvar ,syntax (make-syntax-table)))
	       (unless (get ',syntax 'variable-documentation)
		 (put ',syntax 'variable-documentation
                      ,(format "Syntax table for `%s'." child)))))
       ,(if declare-abbrev
	    `(progn
               (defvar ,abbrev)
	       (unless (boundp ',abbrev)
		 (put ',abbrev 'definition-name ',child)
		 (defvar ,abbrev
		   (progn (define-abbrev-table ',abbrev nil) ,abbrev)))
	       (unless (get ',abbrev 'variable-documentation)
		 (put ',abbrev 'variable-documentation
                      ,(format "Abbrev table for `%s'." child)))))
       (if (fboundp 'derived-mode-set-parent) ;; Emacs≥30.1
           (derived-mode-set-parent ',child ',parent)
         (put ',child 'derived-mode-parent ',parent))
       ,(if group `(put ',child 'custom-mode-group ,group))

       (defun ,child ()
	 ,docstring
	 ,(and interactive '(interactive))
					; Run the parent.
	 (delay-mode-hooks

	  (,(or parent 'kill-all-local-variables))
					; Identify the child mode.
	  (setq major-mode (quote ,child))
	  (setq mode-name ,name)
					; Identify special modes.
	  ,(when parent
	     `(progn
		(if (get (quote ,parent) 'mode-class)
		    (put (quote ,child) 'mode-class
			 (get (quote ,parent) 'mode-class)))
					; Set up maps and tables.
		(unless (keymap-parent ,map)
                  ;; It would probably be better to set the keymap's parent
                  ;; at the toplevel rather than inside the mode function,
                  ;; but this is not easy for at least the following reasons:
                  ;; - the parent (and its keymap) may not yet be loaded.
                  ;; - the parent's keymap name may be called something else
                  ;;   than <parent>-mode-map.
		  (set-keymap-parent ,map (current-local-map)))
		,(when declare-syntax
		   `(let ((parent (char-table-parent ,syntax)))
		      (unless (and parent
				   (not (eq parent (standard-syntax-table))))
			(set-char-table-parent ,syntax (syntax-table)))))
                ,(when declare-abbrev
                   `(unless (or (abbrev-table-get ,abbrev :parents)
                                ;; This can happen if the major mode defines
                                ;; the abbrev-table to be its parent's.
                                (eq ,abbrev local-abbrev-table))
                      (abbrev-table-put ,abbrev :parents
                                        (list local-abbrev-table))))))
	  (use-local-map ,map)
	  ,(when syntax `(set-syntax-table ,syntax))
	  ,(when abbrev `(setq local-abbrev-table ,abbrev))
					; Splice in the body (if any).
	  ,@body
	  )
	 ,@(when after-hook
	     `((push (lambda () ,after-hook) delayed-after-hook-functions)))
	 ;; Run the hooks (and delayed-after-hook-functions), if any.
	 (run-mode-hooks ',hook)))))


;;; PRIVATE

(defun derived-mode-make-docstring (parent child &optional
					   docstring syntax abbrev)
  "Construct a docstring for a new mode if none is provided."

  (let ((map (derived-mode-map-name child))
	(hook (derived-mode-hook-name child)))

    (unless (stringp docstring)
      ;; Use a default docstring.
      (setq docstring
	    (if (null parent)
                (concat
                 "Major-mode.\n"
                 (internal--format-docstring-line
                  "Uses keymap `%s'%s%s." map
                  (if abbrev (format "%s abbrev table `%s'"
                                     (if syntax "," " and") abbrev) "")
                  (if syntax (format " and syntax-table `%s'" syntax) "")))
	      (format "Major mode derived from `%s' by `define-derived-mode'.
It inherits all of the parent's attributes, but has its own keymap%s:

%s

which more-or-less shadow%s %s's corresponding table%s."
		      parent
		      (cond ((and abbrev syntax)
			     ",\nabbrev table and syntax table")
			    (abbrev "\nand abbrev table")
			    (syntax "\nand syntax table")
			    (t ""))
                      (internal--format-docstring-line
                       "  `%s'%s"
                       map
                       (cond ((and abbrev syntax)
                              (format ", `%s' and `%s'" abbrev syntax))
                             ((or abbrev syntax)
                              (format " and `%s'" (or abbrev syntax)))
                             (t "")))
		      (if (or abbrev syntax) "" "s")
		      parent
		      (if (or abbrev syntax) "s" "")))))

    (unless (string-match (regexp-quote (symbol-name hook)) docstring)
      ;; Make sure the docstring mentions the mode's hook.
      (setq docstring
            (concat docstring "\n\n"
                    (internal--format-docstring-line
                     "%s%s%s"
                     (if (null parent)
                         "This mode "
                       (concat
                        "In addition to any hooks its parent mode "
                        (if (string-match (format "[`‘]%s['’]"
                                                  (regexp-quote
                                                   (symbol-name parent)))
                                          docstring)
                            nil
                          (format "`%s' " parent))
                        "might have run, this mode "))
                     (format "runs the hook `%s'" hook)
                     ", as the final or penultimate step during initialization."))))

    (unless (string-match "\\\\[{[]" docstring)
      ;; And don't forget to put the mode's keymap.
      (setq docstring (concat docstring "\n\n\\{" (symbol-name map) "}")))

    docstring))


(provide 'derived)

;;; derived.el ends here
