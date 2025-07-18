;;; reftex.el --- minor mode for doing \label, \ref, \cite, \index in LaTeX  -*- lexical-binding: t; -*-

;; Copyright (C) 1997-2025 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

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

;; RefTeX is a minor mode with distinct support for \ref, \label, \cite,
;; and \index commands in (multi-file) LaTeX documents.
;; - A table of contents provides easy access to any part of a document.
;; - Labels are created semi-automatically.
;; - Definition context of labels is provided when creating a reference.
;; - Citations are simplified with efficient database lookup.
;; - Text phrases can be collected in a file, for later global indexing.
;; - The index preview buffer helps to check and edit index entries.
;;
;; There is an extensive Texinfo document describing RefTeX in detail.
;; One way to view this documentation is `M-x reftex-info RET'.
;;
;; The documentation in various formats is also available at
;;
;;     https://www.gnu.org/software/auctex/manual/reftex.index.html
;;
;; RefTeX is bundled with Emacs.
;; If you need to install it yourself, you can find a distribution at
;;
;;    https://www.gnu.org/software/auctex/reftex.html
;;
;; RefTeX was written by Carsten Dominik <carsten.dominik@gmail.com> with
;; contributions from Stephen Eglen.  It is currently maintained by
;; the AUCTeX project.

;;; Code:

(eval-when-compile (require 'cl-lib))
(when (< emacs-major-version 28)  ; preloaded in Emacs 28
  (require 'easymenu))

(defvar reftex-tables-dirty t
  "Flag showing if tables need to be re-computed.")

(eval-and-compile
  (defun reftex-set-dirty (symbol value)
    (setq reftex-tables-dirty t)
    (set symbol value)))


;; Configuration variables
(require 'reftex-vars)


;;; Autoloads to ensure loading of support files when necessary
(require 'reftex-loaddefs)

;; We autoload tons of functions from these files, but some have
;; a single function that needs to be globally autoloaded.
;; The alternative is to use a Makefile rule + distinct autoload
;; cookie (eg ;;;###reftex-autoload) for internal autoloads,
;; as eg calendar/ does.  But that seemed like overkill for 4 functions.

;;;###autoload(autoload 'reftex-citation "reftex-cite" nil t)
;;;###autoload(autoload 'reftex-all-document-files "reftex-parse")
;;;###autoload(autoload 'reftex-isearch-minor-mode "reftex-global" nil t)
;;;###autoload(autoload 'reftex-index-phrases-mode "reftex-index" nil t)

;; Generated functions.
(autoload 'reftex-varioref-vref "reftex-ref"
  "Make a varioref reference." t)
(autoload 'reftex-fancyref-fref "reftex-ref"
  "Make a fancyref \\fref reference." t)
(autoload 'reftex-fancyref-Fref "reftex-ref"
  "Make a fancyref \\Fref reference." t)

;;; =========================================================================
;;;
;;; Define the formal stuff for a minor mode named RefTeX.
;;;

(defconst reftex-version emacs-version
  "Version string for RefTeX.")

(defvar reftex-mode-map
  (let ((map (make-sparse-keymap)))
    ;; The default bindings in the mode map.
    (define-key map "\C-c=" #'reftex-toc)
    (define-key map "\C-c-" #'reftex-toc-recenter)
    (define-key map "\C-c(" #'reftex-label)
    (define-key map "\C-c)" #'reftex-reference)
    (define-key map "\C-c[" #'reftex-citation)
    (define-key map "\C-c<" #'reftex-index)
    (define-key map "\C-c>" #'reftex-display-index)
    (define-key map "\C-c/" #'reftex-index-selection-or-word)
    (define-key map "\C-c\\" #'reftex-index-phrase-selection-or-word)
    (define-key map "\C-c|" #'reftex-index-visit-phrases-buffer)
    (define-key map "\C-c&" #'reftex-view-crossref)

    ;; Bind `reftex-mouse-view-crossref' only when the key is still free
    (unless (key-binding [(shift mouse-2)])
      (define-key map [(shift mouse-2)] #'reftex-mouse-view-crossref))

    ;; For most of these commands there are already bindings in place.
    ;; Setting `reftex-extra-bindings' really is only there to spare users
    ;; the hassle of defining bindings in the user space themselves.  This
    ;; is why they violate the key binding recommendations.
    (when reftex-extra-bindings
      (define-key map "\C-ct" #'reftex-toc)
      (define-key map "\C-cl" #'reftex-label)
      (define-key map "\C-cr" #'reftex-reference)
      (define-key map "\C-cc" #'reftex-citation)
      (define-key map "\C-cv" #'reftex-view-crossref)
      (define-key map "\C-cg" #'reftex-grep-document)
      (define-key map "\C-cs" #'reftex-search-document))

    map)
  "Keymap for RefTeX mode.")

(defvar reftex-mode-menu nil)
(defvar reftex-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "." st)
    (modify-syntax-entry ?\) "." st)
    st))

(defvar reftex-syntax-table-for-bib
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\' "." st)
    (modify-syntax-entry ?\" "." st)
    (modify-syntax-entry ?\[ "." st)
    (modify-syntax-entry ?\] "." st)
    (modify-syntax-entry ?\( "." st)
    (modify-syntax-entry ?\) "." st)
    st))

;; The following definitions are out of place, but I need them here
;; to make the compilation of reftex-mode not complain.
(defvar reftex-auto-view-crossref-timer nil
  "The timer used for auto-view-crossref.")
(defvar reftex-toc-auto-recenter-timer nil
  "The idle timer used to recenter the toc window.")

;;;###autoload
(defun turn-on-reftex ()
  "Turn on RefTeX mode."
  (reftex-mode t))

(put 'reftex-mode :included '(memq major-mode '(latex-mode tex-mode)))
(put 'reftex-mode :menu-tag "RefTeX Mode")
;;;###autoload
(define-minor-mode reftex-mode
  "Minor mode with distinct support for \\label, \\ref and \\cite in LaTeX.

\\<reftex-mode-map>A Table of Contents of the entire (multifile) document with browsing
capabilities is available with `\\[reftex-toc]'.

Labels can be created with `\\[reftex-label]' and referenced with `\\[reftex-reference]'.
When referencing, you get a menu with all labels of a given type and
context of the label definition.  The selected label is inserted as a
\\ref macro.

Citations can be made with `\\[reftex-citation]' which will use a regular expression
to pull out a *formatted* list of articles from your BibTeX
database.  The selected citation is inserted as a \\cite macro.

Index entries can be made with `\\[reftex-index-selection-or-word]' which indexes the word at point
or the current selection.  More general index entries are created with
`\\[reftex-index]'.  `\\[reftex-display-index]' displays the compiled index.

Most command have help available on the fly.  This help is accessed by
pressing `?' to any prompt mentioning this feature.

Extensive documentation about RefTeX is available in Info format.
You can view this information with `\\[reftex-info]'.

\\{reftex-mode-map}
Under X, these and other functions will also be available as `Ref' menu
on the menu bar.

------------------------------------------------------------------------------"
  :lighter " Ref" :keymap reftex-mode-map
  (if reftex-mode
      (progn
        ;; Mode was turned on
        (and reftex-plug-into-AUCTeX
             (reftex-plug-into-AUCTeX))
        (unless (get 'reftex-auto-view-crossref 'initialized)
          (and reftex-auto-view-crossref
               (reftex-toggle-auto-view-crossref))
          (put 'reftex-auto-view-crossref 'initialized t))
        (unless (get 'reftex-auto-recenter-toc 'initialized)
          (and (eq reftex-auto-recenter-toc t)
               (reftex-toggle-auto-toc-recenter))
          (put 'reftex-auto-recenter-toc 'initialized t))

        (run-hooks 'reftex-mode-hook))))

(defvar reftex-docstruct-symbol)
(defun reftex-kill-buffer-hook ()
  "Save RefTeX's parse file for this buffer if the information has changed."
  ;; Save the parsing information if it was modified.
  ;; This function should be installed in `kill-buffer-hook'.
  ;; We are careful to make sure nothing goes wrong in this function.
  (when (and (boundp 'reftex-mode)  reftex-mode
             (boundp 'reftex-save-parse-info)  reftex-save-parse-info
             (boundp 'reftex-docstruct-symbol)  reftex-docstruct-symbol
             (symbol-value reftex-docstruct-symbol)
             (get reftex-docstruct-symbol 'modified))
    ;; Write the file.
    (condition-case nil
        (reftex-access-parse-file 'write)
      (error nil))))

(defun reftex-kill-emacs-hook ()
  "Call `reftex-kill-buffer-hook' on all buffers."
  ;; This function should be installed in `kill-emacs-hook'.
  (save-excursion
    (mapcar (lambda (buf)
              (set-buffer buf)
              (reftex-kill-buffer-hook))
            (buffer-list))))

;;; =========================================================================
;;;
;;; Silence warnings about variables in other packages.
(defvar TeX-master)
(defvar LaTeX-section-hook)
(defvar LaTeX-label-function)
(defvar tex-main-file)
(defvar outline-minor-mode)

;;; =========================================================================
;;;
;;; Helper functions for handling both file names and buffer objects.
;;;

(defun reftex--get-buffer-identifier (&optional buffer)
  "Return the base buffer's file name or buffer identifier.
For file buffers, returns the file name of the base buffer.
For non-file buffers, return the base buffer object itself.
When BUFFER is nil, use the current buffer."
  (let* ((buffer (or (buffer-base-buffer buffer) buffer (current-buffer))))
    (or (buffer-local-value 'buffer-file-name buffer)
        buffer)))

(defun reftex--get-directory (file-or-buffer)
  "Get the directory associated with FILE-OR-BUFFER.
FILE-OR-BUFFER can be a file name or a buffer object."
  (if (bufferp file-or-buffer)
      (buffer-local-value 'default-directory file-or-buffer)
    (file-name-directory file-or-buffer)))

(defun reftex--abbreviate-name (file-or-buffer)
  "Get a nice display name for FILE-OR-BUFFER.
For files, returns the abbreviated file name.
For buffers, returns the buffer name."
  (if (bufferp file-or-buffer)
      (prin1-to-string file-or-buffer)
    (abbreviate-file-name file-or-buffer)))

(defun reftex--get-basename (file-or-buffer)
  "Get the base name (without extension) for FILE-OR-BUFFER.
For file names, returns the file name without directory and extension.
For buffer objects, returns a sanitized version of the buffer name
suitable for use in LaTeX labels."
  (if (bufferp file-or-buffer)
      (file-name-base (buffer-name file-or-buffer))
    (file-name-base file-or-buffer)))

(defun reftex--get-truename (file-or-buffer)
  "Get the canonical form of FILE-OR-BUFFER's identity.
For files, returns the result of file-truename.
For buffer objects, returns the buffer object itself."
  (if (bufferp file-or-buffer)
      file-or-buffer
    (file-truename file-or-buffer)))

;;; =========================================================================
;;;
;;; Multibuffer Variables
;;;
;; Technical notes: These work as follows: We keep just one list
;; of labels for each master file - this can save a lot of memory.
;; `reftex-master-index-list' is an alist which connects the true file name
;; of each master file with the symbols holding the information on that
;; document.  Each buffer has local variables which point to these symbols.

;; List of variables which handle the multifile stuff.
;; This list is used to tie, untie, and reset these symbols.
(defconst reftex-multifile-symbols
  '(reftex-docstruct-symbol))

;; Alist connecting master file names with the corresponding lisp symbols.
(defvar reftex-master-index-list nil)

;; Last index used for a master file.
(defvar reftex-multifile-index 0)

;; Variable holding the symbol with the label list of the document.
(defvar-local reftex-docstruct-symbol nil)

(defun reftex-next-multifile-index ()
  "Return the next free index for multifile symbols."
  (incf reftex-multifile-index))

(defun reftex--remove-buffer-from-master-index ()
  "Remove current buffer from `reftex-master-index-list'."
  (setq reftex-master-index-list
        (assq-delete-all (current-buffer) reftex-master-index-list)))

(defun reftex-tie-multifile-symbols ()
  "Tie the buffer-local symbols to globals connected with the master file.
If the symbols for the current master file do not exist, they are created."
  (let* ((master (reftex-TeX-master-file))
         (master (reftex--get-truename master))
         (index (assoc master reftex-master-index-list))
         (symlist reftex-multifile-symbols)
         symbol symname newflag)
    ;; Find the correct index.
    (if index
        ;; Symbols do exist
        (setq index (cdr index))
      ;; Get a new index and add info to the alist.
      (setq index (reftex-next-multifile-index)
            newflag t)
      (push (cons master index) reftex-master-index-list)
      (when (bufferp master)
        (with-current-buffer master
          (add-hook 'kill-buffer-hook
                    #'reftex--remove-buffer-from-master-index nil t))))

    ;; Get/create symbols and tie them.
    (while symlist
      (setq symbol (car symlist)
            symlist (cdr symlist)
            symname (symbol-name symbol))
      (set symbol (intern (concat symname "-" (int-to-string index))))
      (put (symbol-value symbol) :master-index index)
      ;; Initialize if new symbols.
      (when newflag
        (set (symbol-value symbol) nil)
        (put (symbol-value symbol) 'reftex-index-macros-style '(default))
	(put (symbol-value symbol) 'reftex-ref-style-list
	     reftex-ref-style-default-list)))

    ;; Return t if the symbols did already exist, nil when we've made them.
    (not newflag)))

(defun reftex-untie-multifile-symbols ()
  "Remove ties from multifile symbols, so that next use makes new ones."
  (let ((symlist reftex-multifile-symbols)
        (symbol nil))
    (while symlist
      (setq symbol  (car symlist)
            symlist (cdr symlist))
      (set symbol nil))))

(defun reftex-TeX-master-file ()
  "Return the name of the master file associated with the current buffer.
When AUCTeX is loaded, we will use it's more sophisticated method.
We also support the default TeX and LaTeX modes by checking for a
variable `tex-main-file'."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (let
        ;; Set master to a file name (possibly non-existent), or nil:
        ((master
          (cond
	   ;; Test if we're in a subfile using the subfiles document
	   ;; class, e.g., \documentclass[main.tex]{subfiles}.  It's
	   ;; argument is the main file, however it's not really the
	   ;; master file in `TeX-master-file' or `tex-main-file's
	   ;; sense.  It should be used for references but not for
	   ;; compilation, thus subfiles use a setting of
	   ;; `TeX-master'/`tex-main-file' being themselves.
	   ((save-excursion
              (goto-char (point-min))
              (re-search-forward
               "^[[:space:]]*\\\\documentclass\\[\\([^]]+\\)\\]{subfiles}"
               nil t))
            (match-string-no-properties 1))
           ;; AUCTeX is loaded.  Use its mechanism.
           ((fboundp 'TeX-master-file)
            (condition-case nil
                (TeX-master-file t)
              (error (buffer-file-name))))
           ;; Emacs LaTeX mode
           ((fboundp 'tex-main-file)
            (condition-case nil
                (tex-main-file)
              (error (buffer-file-name))))
           ;; Check the `TeX-master' variable.
           ((boundp 'TeX-master)
            (cond
             ((eq TeX-master t)
              (buffer-file-name))
             ((or (stringp TeX-master) (bufferp TeX-master)) TeX-master)
             (t
              (setq TeX-master (read-file-name "Master file: "
                                               nil nil t nil)))))
           ;; Check the `tex-main-file' variable.
           ((boundp 'tex-main-file)
            ;; This is the variable from the default TeX modes.
            (cond
             ((stringp tex-main-file)
              ;; ok, this must be it
              tex-main-file)
             (t
              ;; In this case, the buffer is its own master.
              (buffer-file-name))))
           ;; We know nothing about master file.  Assume this is a
           ;; master file.
           (t
            (buffer-file-name)))))
      (cond
       ((not (stringp master)))
       ((or (file-exists-p (concat master ".tex"))
            (find-buffer-visiting (concat master ".tex")))
        ;; Ahh, an extra .tex was missing...
        (setq master (concat master ".tex")))
       ((or (file-exists-p master)
            (find-buffer-visiting master))
        ;; We either see the file, or have a buffer on it.  OK.
        )
       (t
        ;; Use buffer file name.
        (setq master (buffer-file-name))))
      (if (stringp master)
          (expand-file-name master)
        (or master (current-buffer))))))

(defun reftex-is-multi ()
  "Tell if this is a multifile document.  When not sure, say yes."
  (let ((entry (assq 'is-multi (symbol-value reftex-docstruct-symbol))))
    (if entry
        (nth 1 entry)
      t)))

(defun reftex-set-cite-format (value)
  "Set the document-local VALUE of `reftex-cite-format'.
When such a value exists, it overwrites the setting given with
`reftex-cite-format'.  See the documentation of `reftex-cite-format'
for possible values.  This function should be used from AUCTeX style files."
  (unless reftex-docstruct-symbol
    (reftex-tie-multifile-symbols))
  (when (and reftex-docstruct-symbol
             (symbolp reftex-docstruct-symbol))
    (put reftex-docstruct-symbol 'reftex-cite-format value)))

(defun reftex-get-cite-format ()
  "Return the current citation format.
Either the document-local value in `reftex-cite-format-symbol', or the
global value in `reftex-cite-format'."
  (if (and reftex-docstruct-symbol
           (symbolp reftex-docstruct-symbol)
           (get reftex-docstruct-symbol 'reftex-cite-format))
      (get reftex-docstruct-symbol 'reftex-cite-format)
    reftex-cite-format))

(defun reftex-add-index-macros (entry-list)
  "Add index macro descriptions to `reftex-index-macros-style'.
The format of ENTRY-LIST is exactly like `reftex-index-macros'.  See there
for details.
This function makes it possible to support RefTeX from AUCTeX style files.
The entries in ENTRY-LIST will be processed after the user settings in
`reftex-index-entries', and before the defaults.  Any changes made to
`reftex-index-macros-style' will raise a flag to the effect that
the label information is recompiled on next use."
  (unless reftex-docstruct-symbol
    (reftex-tie-multifile-symbols))
  (when (and reftex-docstruct-symbol
             (symbolp reftex-docstruct-symbol))
    (let ((list (get reftex-docstruct-symbol 'reftex-index-macros-style))
          entry changed)
      (while entry-list
        (setq entry (pop entry-list))
        ;; When it is a symbol, remove all other symbols
        (and (symbolp entry)
             (not (memq entry list))
             (setq list (seq-remove #'symbolp list)))
        ;; Add to list unless already member
        (unless (member entry list)
          (setq reftex-tables-dirty t
                changed t)
          (push entry list)))
      (when changed
        (put reftex-docstruct-symbol 'reftex-index-macros-style list)))))

(defun reftex-ref-style-activate (style)
  "Activate the referencing style STYLE."
  (reftex-ref-style-toggle style 'activate))

(defun reftex-ref-style-toggle (style &optional action)
  "Activate or deactivate the referencing style STYLE.
With the optional argument ACTION a certain action can be forced.
The symbol `activate' will activate the style and `deactivate'
will deactivate it."
  (unless reftex-docstruct-symbol
    (reftex-tie-multifile-symbols))
  (when (and reftex-docstruct-symbol
             (symbolp reftex-docstruct-symbol))
    (let ((list (get reftex-docstruct-symbol 'reftex-ref-style-list))
          changed)
      (cond ((eq action 'activate)
	     (unless (member style list)
	       (setq reftex-tables-dirty t
		     changed t)
               (setq list (append list (list style)))))
	    ((eq action 'deactivate)
	     (when (member style list)
	       (setq reftex-tables-dirty t
		     changed t)
	       (setq list (delete style list))))
	    (t
             (setq list (if (member style list)
		            (delete style list)
                          (append list (list style))))
	     (setq reftex-tables-dirty t
		   changed t)))
      (when changed
        (put reftex-docstruct-symbol 'reftex-ref-style-list list)))))

(defun reftex-ref-style-list ()
  "Return the list of referencing styles to be active at the moment."
  ;; Initialize the value of `reftex-ref-style-list' and tie it to the
  ;; docstruct symbol if necessary.
  (unless reftex-docstruct-symbol
    (reftex-tie-multifile-symbols))
  (if (and reftex-docstruct-symbol
           (symbolp reftex-docstruct-symbol)
           (get reftex-docstruct-symbol 'reftex-ref-style-list))
      (get reftex-docstruct-symbol 'reftex-ref-style-list)
    reftex-ref-style-default-list))

;;; =========================================================================
;;;
;;; Functions to compile the tables, reset the mode etc.

;; The following constants are derived from `reftex-label-alist'.

;; Prompt used for label type queries directed to the user.
(defvar reftex-type-query-prompt nil)

;; Help string for label type queries.
(defvar reftex-type-query-help nil)

;; Alist relating label type to reference format.
(defvar reftex-typekey-to-format-alist nil)

;; Alist relating label type to label prefix.
(defvar reftex-typekey-to-prefix-alist nil)

;; Alist relating environments or macros to label type and context regexp.
(defvar reftex-env-or-mac-alist nil)

;; List of special environment parser functions
(defvar reftex-special-env-parsers nil)

;; List of macros carrying a label.
(defvar reftex-label-mac-list nil)

;; List of environments carrying a label.
(defvar reftex-label-env-list nil)

;; List of all typekey letters in use.
(defvar reftex-typekey-list nil)

;; Alist relating magic words to a label type.
(defvar reftex-words-to-typekey-alist nil)
;; Alist relating label prefixes to a label type.
(defvar reftex-prefix-to-typekey-alist nil)

;; The last list-of-labels entry used in a reference.
(defvar reftex-last-used-reference (list nil nil nil nil))

;; Alist relating index macros to other info.
(defvar reftex-key-to-index-macro-alist nil)
;; Prompt for index macro queries
(defvar reftex-query-index-macro-prompt nil)
;; Help string for index macro queries
(defvar reftex-query-index-macro-help nil)

;; The message when follow-mode is suspended
(defvar reftex-no-follow-message
  "No follow-mode into unvisited file.  Press SPC to visit it.")
(defvar reftex-no-info-message
  "%s: info not available, use `\\[reftex-view-crossref]' to get it.")

;; Global variables used for communication between functions.
(defvar reftex-default-context-position nil)
(defvar reftex-location-start nil)
(defvar reftex-call-back-to-this-buffer nil)
(defvar reftex-select-return-marker (make-marker))
(defvar reftex-active-toc nil)
(defvar reftex-tex-path nil)
(defvar reftex-bib-path nil)
(defvar reftex-select-marked nil)
(defvar reftex-last-follow-point nil)
(defvar reftex-latex-syntax-table nil)
(defvar reftex-prefix nil)
(defvar reftex-section-levels-all nil)
(defvar reftex-buffers-with-changed-invisibility nil)
(defvar reftex-callback-fwd t)
(defvar reftex-last-toc-master nil
  "Stores the name of the tex file that `reftex-toc' was last run on.")
;; Marker for return point from recursive edit
(defvar reftex-recursive-edit-marker (make-marker))

;; List of buffers created temporarily for lookup, which should be killed.
(defvar reftex-buffers-to-kill nil)

;; Regexp to find anything.
(defvar reftex-section-regexp nil)
(defvar reftex-section-or-include-regexp nil)
(defvar reftex-index-macro-regexp nil)
(defvar reftex-index-level-re nil)
(defvar reftex-index-key-end-re nil)
(defvar reftex-find-index-entry-regexp-format nil)
(defvar reftex-everything-regexp nil)
(defvar reftex-everything-regexp-no-index nil)
(defvar reftex-index-re nil)
(defvar reftex-find-citation-regexp-format
  "\\\\\\([a-zA-Z]*cite[*a-zA-Z]*\\*?\\|bibentry\\)\\(\\[[^]]*\\]\\|{[^}]*}\\)*{\\([^}]*,\\)?\\(%s\\)[},]")
(defvar reftex-find-reference-format
  "\\\\\\(ref[a-zA-Z]*\\|[a-zA-Z]*ref\\(range\\)?\\)\\*?\\(\\[[^]]*\\]\\|{[^}]*}\\)*{\\(%s\\)}")
(defvar reftex-macros-with-labels nil)
(defvar reftex-macros-with-index nil)
(defvar reftex-index-macro-alist nil)
(defvar reftex-find-label-regexp-format nil)
(defvar reftex-find-label-regexp-format2 nil)

;; Constants for making RefTeX open to Texinfo hooking
(defvar reftex-section-pre-regexp "\\\\")
;; Including `\' as a character to be matched at the end of the regexp
;; will allow stuff like \begin{foo}\label{bar} to be matched.  This
;; will make the parser to advance one char too much.  Therefore
;; `reftex-parse-from-file' will step one char back if a section is
;; found.
(defvar reftex-section-post-regexp "\\*?\\(\\[[^]]*\\]\\)?[[{ \t\r\n\\]")
(defvar reftex-section-info-function 'reftex-section-info)

(defvar reftex-memory nil
  "Memorizes old variable values to indicate changes in these variables.")

;; A list of all variables in the cache.
;; The cache is used to save the compiled versions of some variables.
(defconst reftex-cache-variables
  '(reftex-memory ;; This MUST ALWAYS be the first!

    ;; Outline
    reftex-section-levels-all

    ;; Labels
    reftex-env-or-mac-alist
    reftex-special-env-parsers
    reftex-macros-with-labels
    reftex-label-mac-list
    reftex-label-env-list
    reftex-typekey-list
    reftex-typekey-to-format-alist
    reftex-typekey-to-prefix-alist
    reftex-words-to-typekey-alist
    reftex-prefix-to-typekey-alist
    reftex-type-query-prompt
    reftex-type-query-help

    ;; Index
    reftex-index-macro-alist
    reftex-macros-with-index
    reftex-query-index-macro-prompt
    reftex-query-index-macro-help
    reftex-key-to-index-macro-alist

    ;; Regular expressions
    reftex-section-regexp
    reftex-section-or-include-regexp
    reftex-index-re
    reftex-everything-regexp
    reftex-everything-regexp-no-index
    reftex-find-label-regexp-format
    reftex-find-label-regexp-format2
    reftex-find-index-entry-regexp-format
))

(defun reftex-ensure-compiled-variables ()
  "Recompile the label alist when necessary."
  (let* ((mem reftex-memory)
         (cache (get reftex-docstruct-symbol 'reftex-cache))
         (cmem  (car cache))
         (alist reftex-label-alist)
         (levels (get reftex-docstruct-symbol 'reftex-section-levels))
         (style (get reftex-docstruct-symbol 'reftex-label-alist-style))
         (default reftex-default-label-alist-entries)
         (index reftex-index-macros)
         (istyle (get reftex-docstruct-symbol 'reftex-index-macros-style)))
    (cond
     (reftex-tables-dirty (reftex-compile-variables))
     ((and (eq alist   (nth 0 mem))
           (eq levels  (nth 1 mem))
           (eq style   (nth 2 mem))
           (eq default (nth 3 mem))
           (eq index   (nth 4 mem))
           (eq istyle  (nth 5 mem))))  ;; everything is OK
     ((and (eq alist   (nth 0 cmem))
           (eq levels  (nth 1 cmem))
           (eq style   (nth 2 cmem))
           (eq default (nth 2 cmem))
           (eq index   (nth 4 cmem))
           (eq istyle  (nth 5 cmem)))
      ;; restore the cache
      (message "Restoring cache")
      (mapcar (lambda (sym) (set sym (pop cache))) reftex-cache-variables))
     (t (reftex-compile-variables)))))

(defun reftex-reset-mode ()
  "Reset RefTeX Mode.
This will re-compile the configuration information and remove all
current scanning information and the parse file to enforce a rescan
on next use."
  (interactive)

  ;; Reset the file search path variables
  (dolist (prop '(status master-dir recursive-path rec-type))
    (put 'reftex-tex-path prop nil)
    (put 'reftex-bib-path prop nil))

  ;; Kill temporary buffers associated with RefTeX - just in case they
  ;; were not cleaned up properly
  (save-excursion
    (let ((buffer-list '("*RefTeX Help*" "*RefTeX Select*"
                         "*Duplicate Labels*" "*toc*" " *RefTeX-scratch*"))
          buf)
      (while (setq buf (pop buffer-list))
        (if (get-buffer buf)
            (kill-buffer buf))))
    (reftex-erase-all-selection-and-index-buffers))

  ;; Make sure the current document will be rescanned soon.
  (reftex-reset-scanning-information)

  ;; Remove any parse info file
  (reftex-access-parse-file 'kill)

  ;; Plug functions into AUCTeX if the user option says so.
  (and reftex-plug-into-AUCTeX
       (reftex-plug-into-AUCTeX))

  (reftex-compile-variables))

;;;###autoload
(defun reftex-reset-scanning-information ()
  "Reset the symbols containing information from buffer scanning.
This enforces rescanning the buffer on next use."
  (if (equal reftex-last-toc-master (reftex-TeX-master-file))
      (reftex-erase-buffer "*toc*"))
  (let ((symlist reftex-multifile-symbols)
        symbol)
    (while symlist
      (setq symbol (car symlist)
            symlist (cdr symlist))
      (if (and (symbolp (symbol-value symbol))
               (not (null (symbol-value symbol))))
          (set (symbol-value symbol) nil)))))

(defun reftex-erase-all-selection-and-index-buffers ()
  "Remove all selection buffers associated with current document."
  (mapc
   (lambda (type)
     (reftex-erase-buffer (reftex-make-selection-buffer-name type)))
   reftex-typekey-list)
  ;; Kill all index buffers
  (mapc
   (lambda (tag)
     (reftex-kill-buffer (reftex-make-index-buffer-name tag)))
   (cdr (assoc 'index-tags (symbol-value reftex-docstruct-symbol)))))

(defun reftex-compile-variables ()
  "Compile the information in reftex-label-alist & Co."
  (message "Compiling label environment definitions...")

  ;; Update AUCTeX style information
  (when (and (featurep 'tex-site) (fboundp 'TeX-update-style))
    (condition-case nil (TeX-update-style) (error nil)))

  ;; Record that we have done this, and what we have used.
  (setq reftex-tables-dirty nil)
  (setq reftex-memory
        (list reftex-label-alist
              (get reftex-docstruct-symbol 'reftex-section-levels)
              (get reftex-docstruct-symbol 'reftex-label-alist-style)
              reftex-default-label-alist-entries
              reftex-index-macros
              (get reftex-docstruct-symbol 'reftex-index-macros-style)))

  ;; Compile information in reftex-label-alist
  (let ((all (reftex-uniquify-by-car
              (reftex-splice-symbols-into-list
               (append reftex-label-alist
                       (get reftex-docstruct-symbol
                            'reftex-label-alist-style)
                       reftex-default-label-alist-entries)
               reftex-label-alist-builtin)
              '(nil)))
        (all-index (reftex-uniquify-by-car
                    (reftex-splice-symbols-into-list
                     (append reftex-index-macros
                             (get reftex-docstruct-symbol
                                  'reftex-index-macros-style)
                             '(default))
                     reftex-index-macros-builtin)))
        entry env-or-mac typekeychar typekey prefix context word
        fmt reffmt labelfmt wordlist qh-list macros-with-labels
        nargs nlabel opt-args cell sum i
        macro verify repeat nindex tag key toc-level toc-levels)

    (setq reftex-words-to-typekey-alist nil
          reftex-prefix-to-typekey-alist
          '(("sec:" . "s") ("cha:" . "s") ("chap:" . "s"))
          reftex-typekey-list nil
          reftex-typekey-to-format-alist nil
          reftex-typekey-to-prefix-alist nil
          reftex-env-or-mac-alist nil
          reftex-label-env-list nil
          reftex-label-mac-list nil)
    (while all
      (catch 'next-entry
        (setq entry (car all)
              env-or-mac (car entry)
              entry (cdr entry)
              all (cdr all))
        (if (null env-or-mac)
            (setq env-or-mac ""))
        (if (stringp (car entry))
            ;; This is before version 2.00 - convert entry to new format
            ;; This is just to keep old users happy
            (setq entry (cons (string-to-char (car entry))
                              (cons (concat (car entry) ":")
                                    (cdr entry)))))
        (setq typekeychar (nth 0 entry)
              typekey (if typekeychar (char-to-string typekeychar) nil)
              prefix (nth 1 entry)
              fmt (nth 2 entry)
              context (nth 3 entry)
              wordlist (nth 4 entry)
              toc-level (nth 5 entry))
        (if (stringp wordlist)
            ;; This is before version 2.04 - convert to new format
            (setq wordlist (nthcdr 4 entry)))

        (if (and (stringp fmt)
                 (string-search "@" fmt))
            ;; Special syntax for specifying a label format
            (setq fmt (split-string fmt "@+"))
          (setq fmt (list "\\label{%s}" fmt)))
        (setq labelfmt (car fmt)
              reffmt (nth 1 fmt))
        ;; Note a new typekey
        (if typekey
            (cl-pushnew typekey reftex-typekey-list :test #'equal))
        (if (and typekey prefix
                 (not (assoc prefix reftex-prefix-to-typekey-alist)))
            (cl-pushnew (cons prefix typekey) reftex-prefix-to-typekey-alist
                        :test #'equal))
        (if (and typekey prefix
                 (not (assoc typekey reftex-typekey-to-prefix-alist)))
            (cl-pushnew (cons typekey prefix) reftex-typekey-to-prefix-alist
                        :test #'equal))
        ;; Check if this is a macro or environment
        (cond
         ((symbolp env-or-mac)
          ;; A special parser function
          (unless (fboundp env-or-mac)
            (message "Warning: %s does not seem to be a valid function"
                     env-or-mac))
          (setq nargs nil nlabel nil opt-args nil)
          (cl-pushnew env-or-mac reftex-special-env-parsers)
          (setq env-or-mac (symbol-name env-or-mac)))
         ((string-match "\\`\\\\" env-or-mac)
          ;; It's a macro
          (let ((result (reftex-parse-args env-or-mac)))
            (setq env-or-mac (or (cl-first result) env-or-mac)
                  nargs (cl-second result)
                  nlabel (cl-third result)
                  opt-args (cl-fourth result))
            (if nlabel (cl-pushnew env-or-mac macros-with-labels :test #'equal)))
          (if typekey (cl-pushnew env-or-mac reftex-label-mac-list :test #'equal)))
         (t
          ;; It's an environment
          (setq nargs nil nlabel nil opt-args nil)
          (cond ((string= env-or-mac "any"))
                ((string= env-or-mac ""))
                ((string= env-or-mac "section"))
                (t
                 (cl-pushnew env-or-mac reftex-label-env-list :test #'equal)
                 (if toc-level
                     (let ((string (format "begin{%s}" env-or-mac)))
                       (or (assoc string toc-levels)
                           (push (cons string toc-level) toc-levels))))))))
        ;; Translate some special context cases
        (when (assq context reftex-default-context-regexps)
          (setq context
                (format
                 (cdr (assq context reftex-default-context-regexps))
                 (regexp-quote env-or-mac))))
        ;; See if this is the first format for this typekey
        (and reffmt
             (not (assoc typekey reftex-typekey-to-format-alist))
             (push (cons typekey reffmt) reftex-typekey-to-format-alist))
        ;; See if this is the first definition for this env-or-mac
        (and (not (string= env-or-mac "any"))
             (not (string= env-or-mac ""))
             (not (assoc env-or-mac reftex-env-or-mac-alist))
             (push (list env-or-mac typekey context labelfmt
                         nargs nlabel opt-args)
                   reftex-env-or-mac-alist))
        ;; Are the magic words regular expressions?  Quote normal words.
        (if (eq (car wordlist) 'regexp)
            (setq wordlist (cdr wordlist))
          (setq wordlist (mapcar #'regexp-quote wordlist)))
        ;; Remember the first association of each word.
        (while (stringp (setq word (pop wordlist)))
          (or (assoc word reftex-words-to-typekey-alist)
              (push (cons word typekey) reftex-words-to-typekey-alist)))
        (cond
         ((string= "" env-or-mac) nil)
         ((setq cell (assoc typekey qh-list))
          (push env-or-mac (cdr cell)))
         (typekey
          (push (list typekey env-or-mac) qh-list)))))

    (setq reftex-typekey-to-prefix-alist
          (nreverse reftex-typekey-to-prefix-alist))

    ;; Prepare the typekey query prompt and help string.
    (setq qh-list
          (sort qh-list
                (lambda (x1 x2)
                  (string< (downcase (car x1)) (downcase (car x2))))))
    (setq reftex-type-query-prompt
          (concat "Label type: ["
                  (mapconcat (lambda(x) (format "%s" (car x)))
                             qh-list "")
                  "]"))
    ;; In the help string, we need to wrap lines...
    (setq reftex-type-query-help
          (concat
           "SELECT A LABEL TYPE:\n--------------------\n"
           (mapconcat
            (lambda(x)
              (setq sum 0)
              (format " [%s]   %s"
                      (car x)
                      (mapconcat (lambda(env)
                                   (setq sum (+ sum (length env)))
                                   (if (< sum 60)
                                       env
                                     (setq sum 0)
                                     (concat "\n       " env)))
                                 (cdr x) " ")))
            qh-list "\n")))

    ;; Convert magic words to regular expressions.  We make regular expressions
    ;; which allow for some chars from the ref format to be in the buffer.
    ;; These characters will be seen and removed.
    (setq reftex-words-to-typekey-alist
          (mapcar
           (lambda (x)
             (setq word (car x)
                   typekey (cdr x)
                   fmt (cdr (assoc typekey reftex-typekey-to-format-alist)))
             (setq word (concat "\\W\\(" word "[ \t\n\r]*\\)\\("))
             (setq i 0)
             (while (and (< i 10)   ; maximum number of format chars allowed
                         (< i (length fmt))
                         (not (member (aref fmt i) '(?%))))
               (setq word (concat word "\\|" (regexp-quote
                                              (substring fmt 0 (1+ i)))))
               (incf i))
             (cons (concat word "\\)\\=") typekey))
           (nreverse reftex-words-to-typekey-alist)))

    ;; Parse the index macros
    (setq reftex-index-macro-alist nil
          reftex-key-to-index-macro-alist nil
          reftex-macros-with-index nil)
    (while all-index
      (setq entry (car all-index)
            macro (car entry)
            tag (nth 1 entry)
            key (nth 2 entry)
            prefix (or (nth 3 entry) "")
            verify (nth 4 entry)
            ;; For repeat, we need to be compatible with older code
            ;; This information used to be given only for the default macro,
            ;; but later we required to have it for *every* index macro
            repeat (cond ((> (length entry) 5) (nth 5 entry))
                         ((and (eq key (car reftex-index-default-macro))
                               (> (length reftex-index-default-macro) 2))
                          ;; User has old setting - respect it
                          (nth 2 reftex-index-default-macro))
                         (t t))
            all-index (cdr all-index))
      (let ((result (reftex-parse-args macro)))
        (setq macro (or (cl-first result) macro)
              nargs (cl-second result)
              nindex (cl-third result)
              opt-args (cl-fourth result))
        (unless (member macro reftex-macros-with-index)
          ;;           0     1    2      3     4     5       6        7
          (push (list macro tag prefix verify nargs nindex opt-args repeat)
                reftex-index-macro-alist)
          (or (assoc key reftex-key-to-index-macro-alist)
              (push (list key macro) reftex-key-to-index-macro-alist))
          (push macro reftex-macros-with-index))))
    ;; Make the prompt and help string for index macros query
    (setq reftex-key-to-index-macro-alist
          (sort reftex-key-to-index-macro-alist
                (lambda (a b) (< (downcase (car a)) (downcase (car b))))))
    (setq reftex-query-index-macro-prompt
          (concat "Index macro: ["
                  (mapconcat (lambda (x) (char-to-string (car x)))
                             reftex-key-to-index-macro-alist "")
                  "]"))
    (setq i 0
          reftex-query-index-macro-help
          (concat
           "SELECT A MACRO:\n---------------\n"
           (mapconcat
            (lambda(x)
              (format "[%c] %-20.20s%s" (car x) (nth 1 x)
                      (if (= 0 (mod (incf i) 3)) "\n" "")))
            reftex-key-to-index-macro-alist "")))

    ;; Make the full list of section levels
    (setq reftex-section-levels-all
          (append toc-levels
                  (get reftex-docstruct-symbol 'reftex-section-levels)
                  reftex-section-levels))

    ;; Calculate the regular expressions
    (let* (;; (wbol "\\(\\`\\|[\n\r]\\)[ \t]*")
           ;; Need to keep the empty group because match numbers are
           ;; hard coded
           (wbol (concat "\\(^\\)"
                         (when (string-suffix-p ".dtx" (buffer-file-name) t)
                           "%")
                         "[ \t]*"))
           (label-re (concat "\\(?:"
			     (mapconcat #'identity reftex-label-regexps "\\|")
			     "\\)"))
           (include-re (concat wbol
                               "\\\\\\("
                               (mapconcat #'identity
                                          reftex-include-file-commands "\\|")
                               "\\)[{ \t]+\\([^} \t\n\r]+\\)"))
           (section-re
            (concat wbol reftex-section-pre-regexp "\\("
                    (mapconcat (lambda (x) (regexp-quote (car x)))
                               reftex-section-levels-all "\\|")
                    "\\)" reftex-section-post-regexp))
           (appendix-re (concat wbol "\\(\\\\appendix\\)"))
           (macro-re
            (if macros-with-labels
                (concat "\\("
                        (mapconcat #'regexp-quote macros-with-labels "\\|")
                        "\\)[[{]")
              ""))
           (index-re
            (concat "\\("
                    (mapconcat #'regexp-quote reftex-macros-with-index "\\|")
                    "\\)[[{]"))
           (find-index-re-format
            (concat "\\("
                    (mapconcat #'regexp-quote reftex-macros-with-index "\\|")
                    "\\)\\([[{][^]}]*[]}]\\)*[[{]\\(%s\\)[]}]"))
           (find-label-re-format
            (concat "\\("
		    "label[[:space:]]*=[[:space:]]*"
		    "\\|"
                    (mapconcat #'regexp-quote (append '("\\label")
                                                      macros-with-labels)
                               "\\|")
                    "\\)\\([[{][^]}]*[]}]\\)*[[{]\\(%s\\)[]}]"))
           (index-level-re
            (regexp-quote (nth 0 reftex-index-special-chars)))
           (index-key-end-re ;; ^]- not allowed
            (concat "[^" (nth 3 reftex-index-special-chars) "]"
                    "[" (nth 1 reftex-index-special-chars)
                    (nth 2 reftex-index-special-chars) "]"))
           )
      (setq reftex-section-regexp section-re
            reftex-section-or-include-regexp
            (concat section-re "\\|" include-re)
            reftex-everything-regexp
            (concat label-re "\\|" section-re "\\|" include-re
                    "\\|" appendix-re
                    "\\|" index-re
                    (if macros-with-labels "\\|" "") macro-re)
            reftex-everything-regexp-no-index
            (concat label-re "\\|" section-re "\\|" include-re
                    "\\|" appendix-re
                    "\\|" "\\(\\\\6\\\\3\\\\1\\)" ; This is unlikely to match
                    (if macros-with-labels "\\|" "") macro-re)
            reftex-index-re index-re
            reftex-index-level-re index-level-re
            reftex-index-key-end-re index-key-end-re
            reftex-macros-with-labels macros-with-labels
            reftex-find-index-entry-regexp-format find-index-re-format
            reftex-find-label-regexp-format find-label-re-format
            reftex-find-label-regexp-format2
            "\\([]} \t\n\r]\\)\\([[{]\\)\\(%s\\)[]}]")
      (message "Compiling label environment definitions...done")))
  (put reftex-docstruct-symbol 'reftex-cache
       (mapcar #'symbol-value reftex-cache-variables)))

(defun reftex-parse-args (macro)
  "Return a list of MACRO name, nargs, arg-nr.
arg-nr is label and a list of optional argument indices."
  (if (string-match "[[{]\\*?[]}]" macro)
      (progn
        (let ((must-match (substring macro 0 (match-beginning 0)))
              (args (substring macro (match-beginning 0)))
              opt-list nlabel (cnt 0))
          (while (string-match "\\`[[{]\\(\\*\\)?[]}]" args)
            (incf cnt)
            (when (eq ?\[ (string-to-char args))
              (push cnt opt-list))
            (when (and (match-end 1)
                       (not nlabel))
              (setq nlabel cnt))
            (setq args (substring args (match-end 0))))
          (list must-match cnt nlabel opt-list)))
    nil))

;;; =========================================================================
;;;
;;; Accessing the parse information

(defun reftex-access-scan-info (&optional rescan file)
  "Ensure access to the scanning info for the current file.
When the multifile symbols are not yet tied, tie them.
When they are empty or RESCAN is non-nil, scan the document.
But, when RESCAN is -1, don't rescan even if docstruct is empty.
When FILE is non-nil, parse only from that file."

  ;; Make sure we have the symbols tied
  (if (eq reftex-docstruct-symbol nil)
      ;; Symbols are not yet tied: Tie them.
      (reftex-tie-multifile-symbols))

  (reftex-ensure-compiled-variables)

  (when (or (null (symbol-value reftex-docstruct-symbol))
            (member rescan '(t 1 (4) (16))))
    ;; The docstruct will change: Remove selection buffers.
    (save-excursion
      (reftex-erase-buffer "*toc*")
      (reftex-erase-all-selection-and-index-buffers)))

  (if (and (null (symbol-value reftex-docstruct-symbol))
           (not (member rescan '(t 1 (4) (16))))
           reftex-save-parse-info)
      ;; Try to read the stuff from a file
      (reftex-access-parse-file 'read))

  (cond
   ((equal rescan -1))  ;; We are not allowed to scan.
   ((not (symbol-value reftex-docstruct-symbol))
    ;; Scan the whole document
    (reftex-do-parse 1 file))
   ((member rescan '(t 1 (4) (16)))
    ;; Scan whatever was required by the caller.
    (reftex-do-parse rescan file))))

(defun reftex-scanning-info-available-p ()
  "Is the scanning info about the current document available?"
  (unless reftex-docstruct-symbol
    (reftex-tie-multifile-symbols))
  (and (symbolp reftex-docstruct-symbol)
       (symbol-value reftex-docstruct-symbol)
       t))

(defun reftex-silence-toc-markers (list n)
  "Set all toc markers in the first N entries in LIST to nil."
  (while (and list (> (decf n) -1))
    (and (eq (car (car list)) 'toc)
         (markerp (nth 4 (car list)))
         (set-marker (nth 4 (car list)) nil))
    (pop list)))

(defun reftex-access-parse-file (action)
  "Perform ACTION on the parse file (the .rel file).
Valid actions are: readable, restore, read, kill, write.
For non-file buffers, persistence operations are skipped."
  (let* ((list (symbol-value reftex-docstruct-symbol))
         (docstruct-symbol reftex-docstruct-symbol)
         (master (reftex-TeX-master-file))
         (enable-local-variables nil)
         (non-file (bufferp master))
         (file (if non-file
                   nil
                 (if (string-match "\\.[a-zA-Z]+\\'" master)
                     (concat (substring master 0 (match-beginning 0))
                             reftex-parse-file-extension)
                   (concat master reftex-parse-file-extension)))))
    (cond
     ;; For non-file buffers, skip file operations but allow initialization.
     (non-file (cond ((eq action 'readable) nil)
                     ((eq action 'read) nil)
                     ((eq action 'kill) t)
                     ((eq action 'restore)
                      (error "Cannot restore for non-file buffer"))))
     ((eq action 'readable)
      (file-readable-p file))
     ((eq action 'restore)
      (put reftex-docstruct-symbol 'modified nil)
      (if (eq reftex-docstruct-symbol nil)
          ;; Symbols are not yet tied: Tie them.
          (reftex-tie-multifile-symbols))
      (if (file-exists-p file)
          ;; load the file and return t for success
          (condition-case nil
              (progn (load-file file) t)
            (error (set reftex-docstruct-symbol nil)
                   (error "Error while loading file %s" file)))
        ;; Throw an exception if the file does not exist
        (error "No restore file %s" file)))
     ((eq action 'read)
      (put reftex-docstruct-symbol 'modified nil)
      (if (file-exists-p file)
          ;; load the file and return t for success
          (condition-case nil
              (progn
                (load-file file)
                (reftex-check-parse-consistency)
                t)
            (error (message "Error while restoring file %s" file)
                   (set reftex-docstruct-symbol nil)
                   nil))
        ;; return nil for failure, but no exception
        nil))
     ((eq action 'kill)
      ;; Remove the file
      (when (and (file-exists-p file) (file-writable-p file))
        (message "Unlinking file %s" file)
        (delete-file file)))
     (t
      (put docstruct-symbol 'modified nil)
      (save-excursion
        (if (file-writable-p file)
            (with-temp-file file
              (message "Writing parse file %s" (abbreviate-file-name file))
              (insert ";; RefTeX parse info file\n")
              (insert (format ";; File: %s\n" master))
              (insert (format ";; User: %s (%s)\n\n"
                              (user-login-name) (user-full-name)))
              (insert "(set reftex-docstruct-symbol '(\n\n")
              (let ((standard-output (current-buffer)))
                (mapc
                 (lambda (x)
                   (cond ((eq (car x) 'toc)
                          ;; A toc entry. Do not save the marker.
                          ;; Save the markers  position at position 8
                          (print (list 'toc "toc" (nth 2 x) (nth 3 x)
                                       nil (nth 5 x) (nth 6 x) (nth 7 x)
                                       (or (and (markerp (nth 4 x))
                                                (marker-position (nth 4 x)))
                                           (nth 8 x)))))
                         ((and (not (eq t reftex-support-index))
                               (eq (car x) 'index))
                          ;; Don't save index entries
                          )
                         (t (print x))))
                 list))
              (insert "))\n\n"))
          (error "Cannot write to file %s" file)))
      t))))

(defun reftex-check-parse-consistency ()
  "Check if parse file is consistent, throw an error if not."

  ;; Check if the master is the same: when moving a document, this will see it.
  (let* ((real-master (reftex-TeX-master-file))
         (parsed-master
          (nth 1 (assq 'bof (symbol-value reftex-docstruct-symbol)))))
    ;; Skip this check for buffer objects.
    (unless (equal (reftex--get-truename real-master)
                   (reftex--get-truename parsed-master))
      (message "Master file name in load file is different: %s versus %s"
               parsed-master real-master)
      (error "Master file name error")))

  ;; Check for the existence of all document files
;;;  (let* ((all (symbol-value reftex-docstruct-symbol)))
;;;    (while all
;;;      (when (and (eq (car (car all)) 'bof)
;;;              (not (file-regular-p (nth 1 (car all)))))
;;;     (message "File %s in saved parse info not available" (cdr (car all)))
;;;     (error "File not found"))
;;;      (setq all (cdr all))))
  )

(defun reftex-select-external-document (xr-alist xr-index)
  "Return index of an external document."
  (let* ((len (length xr-alist)) (highest (1- (+ ?0 len)))
         (prompt (format "[%c-%c] Select    TAB: Read prefix with completion"
                         ?0 highest))
         key prefix)
    (cond
     ((= len 1)
      (message "No external documents available")
      (ding) (sit-for 1) 0)
     ((= len 2)
      (- 1 xr-index))
     (t
      (save-excursion
        (let* ((length (apply #'max (mapcar
                                     (lambda(x) (length (car x))) xr-alist)))
               (fmt (format " [%%c]  %%-%ds  %%s\n" length))
               (n (1- ?0)))
          (setq key
                (reftex-select-with-char
                 prompt
                 (concat
                  "SELECT EXTERNAL DOCUMENT\n------------------------\n"
                  (mapconcat
                   (lambda (x)
                     (format fmt (incf n) (or (car x) "")
                             (reftex--abbreviate-name (cdr x))))
                   xr-alist ""))
                 nil t))
          (cond
           ((and (>= key ?0) (<= key highest)) (- key ?0))
           ((= key ?\C-i)
            (setq prefix (completing-read "Prefix: " xr-alist nil t))
            (- len (length (memq (assoc prefix xr-alist) xr-alist))))
           (t (error "Invalid document selection [%c]" key)))))))))

;;; =========================================================================
;;;
;;; Finding files

(defun reftex-locate-file (file type master-dir &optional die)
  "Find FILE of type TYPE in MASTER-DIR or on the path associated with TYPE.
If the file does not have any of the valid extensions for TYPE,
try first the default extension and only then the naked file name.
When DIE is non-nil, throw an error if file not found.
When FILE is a buffer object, return that buffer."
  (if (bufferp file)
      file
    (let* ((rec-values (if reftex-search-unrecursed-path-first '(nil t) '(t)))
         (extensions (cdr (assoc type reftex-file-extensions)))
         (def-ext (car extensions))
         (ext-re (concat "\\("
                         (mapconcat #'regexp-quote extensions "\\|")
                         "\\)\\'"))
         (files (if (string-match ext-re file)
                    (cons file nil)
		  (if reftex-try-all-extensions
		      (append (mapcar (lambda (x) (concat file x))
				      extensions)
			      (list file))
		    (list (concat file def-ext) file))))
         path old-path file1 f fs)
    (cond
     ((file-name-absolute-p file)
      (while (setq f (pop files))
	(if (file-regular-p f)
	    (setq file1 f files nil))))
     ((and reftex-use-external-file-finders
           (assoc type reftex-external-file-finders))
      (setq file1 (reftex-find-file-externally file type master-dir)))
     (t
      (while (and (null file1) rec-values)
        (setq path (reftex-access-search-path
                    type (pop rec-values) master-dir file))
	(setq fs files)
	(while (and (null file1) (setq f (pop fs)))
	  (when (or (null old-path)
		    (not (eq old-path path)))
	    (setq old-path path
		  path (cons master-dir path))
	    (setq file1 (reftex-find-file-on-path f path master-dir)))))))
    (cond (file1 file1)
          (die (error "No such file: %s" file) nil)
          (t (message "No such file: %s (ignored)" file) nil)))))

(defun reftex-find-file-externally (file type &optional master-dir)
  "Use external program to find FILE.
The program is taken from `reftex-external-file-finders'.
Interpret relative path definitions starting from MASTER-DIR."
  (let ((default-directory (or master-dir default-directory))
        (prg (cdr (assoc type reftex-external-file-finders)))
        out)
    (if (string-match "%f" prg)
        (setq prg (replace-match file t t prg)))
    (setq out (apply #'reftex-process-string (split-string prg)))
    (if (string-match "[ \t\n]+\\'" out)     ; chomp
        (setq out (replace-match "" nil nil out)))
    (cond ((equal out "") nil)
          ((file-regular-p out) (expand-file-name out master-dir))
          (t nil))))

(defun reftex-process-string (program &rest args)
  "Execute PROGRAM with arguments ARGS and return its STDOUT as a string."
  (let ((calling-dir default-directory))  ; remember default directory
    (with-output-to-string
      (with-current-buffer standard-output
        (let ((default-directory calling-dir)) ; set default directory
          (apply #'call-process program nil '(t nil) nil args))))))

(defun reftex-access-search-path (type &optional recurse master-dir file)
  "Access path from environment variables.  TYPE is either \"tex\" or \"bib\".
When RECURSE is t, expand path elements ending in `//' recursively.
Relative path elements are left as they are.  However, relative recursive
elements are expanded with MASTER-DIR as default directory.
The expanded path is cached for the next search.
FILE is just for the progress message.
Returns the derived path."
  (let* ((pathvar (intern (concat "reftex-" type "-path"))))
    (when (null (get pathvar 'status))
      ;; Get basic path
      (set pathvar
           (reftex-uniquify
            (reftex-parse-colon-path
             (mapconcat
              (lambda(x)
                (if (string-match "^!" x)
                    (apply #'reftex-process-string
                           (split-string (substring x 1)))
                  (or (getenv x) x)))
              ;; For consistency, the next line should look like this:
              ;;  (cdr (assoc type reftex-path-environment))
              ;; However, historically we have separate options for the
              ;; environment variables, so we have to do this:
              (symbol-value (intern (concat "reftex-" type
                                            "path-environment-variables")))
              path-separator))))
      (put pathvar 'status 'split)
      ;; Check if we have recursive elements
      (let ((path (symbol-value pathvar)) dir rec)
        (while (setq dir (pop path))
          (when (string= (substring dir -2) "//")
            (if (file-name-absolute-p dir)
                (setq rec (or rec 'absolute))
              (setq rec 'relative))))
        (put pathvar 'rec-type rec)))

    (if recurse
        ;; Return the recursive expansion of the path
        (cond
         ((not (get pathvar 'rec-type))
          ;; Path does not contain recursive elements - use simple path
          (symbol-value pathvar))
         ((or (not (get pathvar 'recursive-path))
              (and (eq (get pathvar 'rec-type) 'relative)
                   (not (equal master-dir (get pathvar 'master-dir)))))
          ;; Either: We don't have a recursive expansion yet.
          ;; or:     Relative recursive path elements need to be expanded
          ;;         relative to new default directory
          (message "Expanding search path to find %s file: %s ..." type file)
          (put pathvar 'recursive-path
               (reftex-expand-path (symbol-value pathvar) master-dir))
          (put pathvar 'master-dir master-dir)
          (get pathvar 'recursive-path))
         (t
          ;; Recursive path computed earlier is still OK.
          (get pathvar 'recursive-path)))
      ;; The simple path was requested
      (symbol-value pathvar))))

(defun reftex-find-file-on-path (file path &optional def-dir)
   "Find FILE along the directory list PATH.
DEF-DIR is the default directory for expanding relative path elements."
  (catch 'exit
    (when (file-name-absolute-p file)
      (if (file-regular-p file)
          (throw 'exit file)
        (throw 'exit nil)))
    (let* ((thepath path) file1 dir)
      (while (setq dir (pop thepath))
        (when (string= (substring dir -2) "//")
          (setq dir (substring dir 0 -1)))
        (setq file1 (expand-file-name file (expand-file-name dir def-dir)))
        (if (file-regular-p file1)
            (throw 'exit file1)))
      ;; No such file
      nil)))

(defun reftex-parse-colon-path (path)
  "Like parse-colon-parse, but // or /~ are left alone.
Trailing ! or !! will be converted into `//' (emTeX convention)"
  (mapcar
   (lambda (dir)
     (if (string-match "\\(//+\\|/*!+\\)\\'" dir)
         (setq dir (replace-match "//" t t dir)))
     (file-name-as-directory dir))
   (delete "" (split-string path (concat path-separator "+")))))

(defun reftex-expand-path (path &optional default-dir)
  "Expand parts of path ending in `//' recursively into directory list.
Relative recursive path elements are expanded relative to DEFAULT-DIR."
  (let (path1 dir recursive)
    (while (setq dir (pop path))
      (if (setq recursive (string= (substring dir -2) "//"))
          (setq dir (substring dir 0 -1)))
      (if (and recursive
               (not (file-name-absolute-p dir)))
          (setq dir (expand-file-name dir default-dir)))
      (if recursive
          ;; Expand recursively
          (setq path1 (append (reftex-recursive-directory-list dir) path1))
        ;; Keep unchanged
        (push dir path1)))
    (nreverse path1)))

(defun reftex-recursive-directory-list (dir)
  "Return a list of all directories below DIR, including DIR itself."
  (let ((path (list dir)) path1 file files)
    (while (setq dir (pop path))
      (when (file-directory-p dir)
        (setq files (nreverse (directory-files dir t "[^.]")))
        (while (setq file (pop files))
          (if (file-directory-p file)
              (push (file-name-as-directory file) path)))
        (push dir path1)))
    path1))

;;; =========================================================================
;;;
;;; Some generally useful functions

(defun reftex-typekey-check (typekey conf-variable &optional n)
  "Check if CONF-VARIABLE is true or contains TYPEKEY."
  (and n (setq conf-variable (nth n conf-variable)))
  (or (eq conf-variable t)
      (and (stringp conf-variable)
           (let ((case-fold-search nil))
             (string-match (concat "[" conf-variable "]") typekey)))))

(defun reftex-check-recursive-edit ()
  "Check if we are already in a recursive edit.
Abort with helpful message if so."
  (if (marker-position reftex-recursive-edit-marker)
      (error
       (substitute-command-keys
        "In unfinished selection process. Finish, or abort with \\[abort-recursive-edit]"))))

(defun reftex-in-comment ()
  "Return non-nil if point is in a comment."
  (save-excursion
    (save-match-data
      (let ((pos (point)))
	(beginning-of-line)
	(re-search-forward
	 (or comment-start-skip
	     ;; The parser may open files in fundamental mode if
	     ;; `reftex-initialize-temporary-buffers' is nil, so here
	     ;; is a default suitable for plain TeX and LaTeX.
	     "\\(\\(^\\|[^\\\n]\\)\\(\\\\\\\\\\)*\\)\\(%+[ \t]*\\)")
	 pos t)))))

(defun reftex-no-props (string)
  "Return STRING with all text properties removed."
  (and (stringp string)
       (set-text-properties 0 (length string) nil string))
  string)

(defun reftex-match-string (n)
  "Match string without properties."
  (when (match-beginning n)
    (buffer-substring-no-properties (match-beginning n) (match-end n))))

(define-obsolete-function-alias 'reftex-region-active-p #'use-region-p "28.1")

(defun reftex-kill-buffer (buffer)
  "Kill BUFFER if it exists."
  (and (setq buffer (get-buffer buffer))
       (kill-buffer buffer)))

(defun reftex-erase-buffer (&optional buffer)
  "Erase BUFFER if it exists.  BUFFER defaults to current buffer.
This even erases read-only buffers."
  (cond
   ((null buffer)
    ;; erase current buffer
    (let ((buffer-read-only nil)) (erase-buffer)))
   ((setq buffer (get-buffer buffer))
    ;; buffer exists
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))))))

(defun reftex-this-word (&optional class)
  "Grab the word around point."
  (setq class (or class "-a-zA-Z0-9:_/.*;|"))
  (save-excursion
    (buffer-substring-no-properties
     (progn (skip-chars-backward class) (point))
     (progn (skip-chars-forward  class) (point)))))

(defun reftex-number (n unit &optional ending)
  (if (and (integerp n) (stringp unit))
      (format "%d %s%s" n unit (if (= n 1) "" (or ending "s")))
    ""))

(defun reftex-all-assq (key list)
  "Return a list of all associations of KEY in LIST.  Comparison with eq."
  (let (rtn)
    (while (setq list (memq (assq key list) list))
      (push (car list) rtn)
      (pop list))
    (nreverse rtn)))

(defun reftex-all-assoc-string (key list)
  "Return a list of all associations of KEY in LIST.  Comparison with string=."
  (let (rtn)
    (while list
      (if (string= (car (car list)) key)
          (push (car list) rtn))
      (pop list))
    (nreverse rtn)))

(defun reftex-last-assoc-before-elt (key elt list &optional exclusive)
  "Find the last association of KEY in LIST before or at ELT.
ELT is found in LIST with equal, not eq.
Returns nil when either KEY or elt are not found in LIST.
When EXCLUSIVE is non-nil, ELT cannot be the return value.
On success, returns the association."
  (let* ((elt (car (member elt list))) (ex (not exclusive)) ass last-ass)
    (while (and (setq ass (assoc key list))
                (setq list (memq ass list))
                (or ex (not (eq elt (car list))))
                (memq elt list))
      (setq last-ass ass
            list (cdr list)))
    last-ass))

(defun reftex-sublist-nth (list nth predicate &optional completion)
  "Make a list of the NTH elements of all members of LIST which fulfill PREDICATE.
When COMPLETION is non-nil, make all elements of the resulting
list also a list, so that the result can be used for completion."
  (let (rtn)
    (while list
      (if (funcall predicate (car list))
          (push (if completion
                    (list (nth nth (car list)))
                  (nth nth (car list)))
                rtn))
      (setq list (cdr list)))
    (nreverse rtn)))

(defun reftex-make-selection-buffer-name (type &optional index)
  "Make unique name for a selection buffer."
  (format " *RefTeX[%s][%d]*"
          type (or index (get reftex-docstruct-symbol :master-index) 0)))

(defun reftex-make-index-buffer-name (tag &optional cnt)
  "Make unique name for an index buffer."
  (format "*Index[%s][%d]*"
          tag (or cnt (get reftex-docstruct-symbol :master-index) 0)))

(defun reftex-truncate (string ncols &optional ellipses padding)
  "Truncate STRING to NCOLS characters.
When PADDING is non-nil, and string is shorter than NCOLS, fill with
white space to NCOLS characters.  When ELLIPSES is non-nil and the
string needs to be truncated, replace last 3 characters by dots."
  (setq string
        (if (<= (length string) ncols)
            string
          (if ellipses
              (concat (substring string 0 (- ncols 3)) "...")
            (substring string 0 ncols))))
  (if padding
      (format (format "%%-%ds" ncols) string)
    string))

(defun reftex-nearest-match (regexp &optional max-length)
  "Find the nearest match of REGEXP.  Set the match data.
If POS is given, calculate distances relative to it.
Return nil if there is no match."
  (let ((pos (point))
        (dist (or max-length (length regexp)))
        match1 match2 match)
    (goto-char (min (+ pos dist) (point-max)))
    (when (re-search-backward regexp nil t)
      (setq match1 (match-data)))
    (goto-char (max (- pos dist) (point-min)))
    (when (re-search-forward regexp nil t)
      (setq match2 (match-data)))
    (goto-char pos)
    (setq match
          (cond
           ((not match1) match2)
           ((not match2) match1)
           ((< (abs (- pos (car match1))) (abs (- pos (car match2)))) match1)
           (t match2)))
    (if match (progn (set-match-data match) t) nil)))

(defun reftex-auto-mode-alist ()
  "Return an `auto-mode-alist' with only the .gz (etc) thingies.
Stolen from gnus nnheader."
  (let ((alist auto-mode-alist)
        out)
    (while alist
      (when (listp (cdr (car alist)))
        (push (car alist) out))
      (pop alist))
    (nreverse out)))

(defun reftex-enlarge-to-fit (buf2 &optional keep-current)
  "Enlarge other window displaying buffer to show whole buffer if possible.
If KEEP-CURRENT in non-nil, current buffer must remain visible."
  (let* ((win1 (selected-window))
         (buf1 (current-buffer))
         (win2 (get-buffer-window buf2))) ;; Only on current frame.
    (when win2
      (select-window win2)
      (unless (and (pos-visible-in-window-p (point-min))
                   (pos-visible-in-window-p (point-max)))
        (enlarge-window (1+ (- (count-lines (point-min) (point-max))
                               (window-height))))))
    (cond
     ((window-live-p win1) (select-window win1))
     (keep-current
      ;; we must have the old buffer!
      (switch-to-buffer-other-window buf1)
      (shrink-window (- (window-height) window-min-height))))))

(defun reftex-select-with-char (prompt help-string &optional delay-time scroll)
  "Offer to select something with PROMPT.
After DELAY-TIME seconds, also with HELP-STRING.  When SCROLL is
non-nil, use \\`SPC' and \\`DEL' to scroll help window."
  (let ((char ?\?))
    (save-window-excursion
      (catch 'exit
        (message "%s   (?=Help)" prompt)
        (when (or (sit-for (or delay-time 0))
                  (= ?\? (setq char (read-char-exclusive))))
          (reftex-kill-buffer "*RefTeX Select*")
          (switch-to-buffer-other-window "*RefTeX Select*")
          (insert help-string)
          (goto-char 1)
          (unless (and (pos-visible-in-window-p (point-min))
                       (pos-visible-in-window-p (point-max)))
            (enlarge-window (1+ (- (count-lines (point-min) (point-max))
                                   (window-height)))))
          (setq truncate-lines t))
        (if (and (pos-visible-in-window-p (point-min))
                 (pos-visible-in-window-p (point-max)))
            nil
          (setq prompt (concat prompt (if scroll "   (SPC/DEL=Scroll)" ""))))
        (message "%s" prompt)
        (and (equal char ?\?) (setq char (read-char-exclusive)))
        (while t
          (cond ((equal char ?\C-g) (keyboard-quit))
                ((equal char ?\?))
                ((and scroll (equal char ?\ ))
                 (condition-case nil (scroll-up) (error nil))
                 (message "%s" prompt))
                ((and scroll (equal char ?\C-? ))
                 (condition-case nil (scroll-down) (error nil))
                 (message "%s" prompt))
                (t (message "")
		   (reftex-kill-buffer "*RefTeX Select*")
                   (throw 'exit char)))
          (setq char (read-char-exclusive)))))))


(defun reftex-make-regexp-allow-for-ctrl-m (string)
  "Convert STRING into a regexp, allowing ^M for \\n and vice versa."
  (let ((start -2))
    (setq string (regexp-quote string))
    (while (setq start (string-match "[\n\r]" string (+ 3 start)))
      (setq string (replace-match "[\n\r]" nil t string)))
    string))

(define-obsolete-function-alias 'reftex-get-buffer-visiting
  #'find-buffer-visiting "28.1")

(defun reftex-visited-files (list)
  "Takes a list of filenames and returns the buffers of those already visited."
  (delq nil (mapcar (lambda (x) (if (find-buffer-visiting x) x nil))
                    list)))

(defun reftex-get-file-buffer-force (file &optional mark-to-kill)
  "Return a buffer visiting file.  Make one, if necessary.
If neither such a buffer nor the file exist, return nil.
If MARK-TO-KILL is t and there is no live buffer, visit the file with
initializations according to `reftex-initialize-temporary-buffers',
and mark the buffer to be killed after use."

  (let ((buf (if (bufferp file)
                 file
               (find-buffer-visiting file))))

    (cond (buf
           ;; We have it already as a buffer - just return it
           buf)

          ((file-readable-p file)
           ;; At least there is such a file and we can read it.

           (if (or (not mark-to-kill)
                   (eq t reftex-initialize-temporary-buffers))

               ;; Visit the file with full magic
               (setq buf (find-file-noselect file))

             ;; Else: Visit the file just briefly, without or
             ;;       with limited Magic

             ;; The magic goes away
             (cl-letf ((format-alist nil)
                       (auto-mode-alist (reftex-auto-mode-alist))
                       ((default-value 'major-mode) 'fundamental-mode)
                       (enable-local-variables nil)
                       (after-insert-file-functions nil))
               (setq buf (find-file-noselect file)))

             ;; Is there a hook to run?
             (when (listp reftex-initialize-temporary-buffers)
               (with-current-buffer buf
                 (run-hooks 'reftex-initialize-temporary-buffers))))

           ;; Let's see if we got a license to kill :-|
           (and mark-to-kill
                (cl-pushnew buf reftex-buffers-to-kill))

           ;; Return the new buffer
           buf)

          ;; If no such file exists, return nil
          (t nil))))

(defun reftex-kill-temporary-buffers (&optional buffer)
  "Kill all buffers in the list reftex-kill-temporary-buffers."
  (cond
   (buffer
    (when (member buffer reftex-buffers-to-kill)
      (kill-buffer buffer)
      (setq reftex-buffers-to-kill
            (delete buffer reftex-buffers-to-kill))))
   (t
    (while (setq buffer (pop reftex-buffers-to-kill))
      (when (bufferp buffer)
        (and (buffer-modified-p buffer)
             (y-or-n-p (format "Save file %s? "
                               (buffer-file-name buffer)))
             (with-current-buffer buffer
               (save-buffer)))
        (kill-buffer buffer))
      (pop reftex-buffers-to-kill)))))

(defun reftex-splice-symbols-into-list (list alist)
  "Splice the association in ALIST of any symbols in LIST into the list.
Return new list."
  (let (rtn tmp)
    (while list
      (while (and (not (null (car list)))  ;; keep list elements nil
                  (symbolp (car list)))
        (setq tmp (car list))
        (cond
         ((assoc tmp alist)
          (setq list (append (nth 2 (assoc tmp alist)) (cdr list))))
         (t
          (error "Cannot treat symbol %s in reftex-label-alist"
                 (symbol-name tmp)))))
      (push (pop list) rtn))
    (nreverse rtn)))

(defun reftex-uniquify (list &optional sort)
  "Return a list of all strings in LIST, but each only once.
Keep order unless SORT is set (faster!)."
  (setq list (copy-sequence list))
  (if sort
      (progn
	(setq list (sort list #'string<))
	(let ((p list))
	  (while (cdr p)
	    (if (string= (car p) (car (cdr p)))
		(setcdr p (cdr (cdr p)))
	      (setq p (cdr p)))))
	list)
    (let ((p list) lst elt)
      ;; push all sublists into lst in reverse(!) order
      (while p
	(push p lst)
	(setq p (cdr p)))
      ;; sort all sublists
      (setq lst (sort lst (lambda (x1 x2) (string< (car x1) (car x2)))))
      (while (cdr lst)
	(setq elt (car (car lst)))
	;; for equal elements in the sorted sublist, replace the
	;; last(!) original list member with nil
	(when (string= elt (car (cadr lst)))
	  (setcar (pop lst) nil)
	  (while (and (cdr lst) (string= elt (car (cadr lst))))
	    (setcar (pop lst) nil)))
	(pop lst)))
    ;; weed out all nils and return.
    (delq nil list)))

(defun reftex-uniquify-by-car (alist &optional keep-list sort)
  "Return a list of all elements in ALIST, but each car only once.
Elements of KEEP-LIST are not removed even if duplicate.
The order is kept unless SORT is set (faster!)."
  (setq keep-list (sort (copy-sequence keep-list) #'string<)
	alist (copy-sequence alist))
  (if sort
      (let (lst elt)
	(setq alist (sort alist (lambda(a b) (string< (car a) (car b)))))
	(setq lst alist)
	(while (cdr lst)
	  (setq elt (car (car lst)))
	  (when (string= elt (car (cadr lst)))
	    (while (and keep-list (string< (car keep-list) elt))
	      (pop keep-list))
	    (if (and keep-list (string= elt (car keep-list)))
		(progn
		  (pop lst)
		  (while (and (cdr lst)
			      (string= elt (car (cadr lst))))
		    (pop lst)))
	      (setcdr lst (cdr (cdr lst)))
	      (while (and (cdr lst)
			  (string= elt (car (cadr lst))))
		(setcdr lst (cdr (cdr lst))))))
	  (pop lst))
	alist)
    (let ((p alist) lst elt)
      (while p
	(push p lst)
	(setq p (cdr p)))
      (setq lst (sort lst (lambda(a b) (string< (car (car a))
						(car (car b))))))
      (while (cdr lst)
	(setq elt (car (car (car lst))))
	(when (string= elt (car (car (cadr lst))))
	  (while (and keep-list (string< (car keep-list) elt))
	    (pop keep-list))
	  (if (and keep-list (string= elt (car keep-list)))
	      (progn
		(pop lst)
		(while (and (cdr lst)
			    (string= elt (car (car (cadr lst)))))
		  (pop lst)))
	    (setcar (pop lst) nil)
	    (while (and (cdr lst)
			(string= elt (car (car (cadr lst)))))
	      (setcar (pop lst) nil))))
	(pop lst)))
    (delq nil alist)))

(defun reftex-remove-if (predicate list)
  "Nondestructively remove all items from LIST which satisfy PREDICATE."
  (let (result)
    (dolist (elt list (nreverse result))
      (unless (funcall predicate elt)
	(push elt result)))))

(defun reftex-abbreviate-title (string)
  (reftex-convert-string string "[-~ \t\n\r,;]" nil t t
                         5 40 nil 1 " " (nth 5 reftex-derive-label-parameters)))

(defun reftex-convert-string (string split-re invalid-re dot keep-fp
                                     nwords maxchar invalid abbrev sep
                                     ignore-words &optional downcase)
  "Convert STRING (a sentence) to something shorter.
SPLIT-RE     is the regular expression used to split the string into words.
INVALID-RE   matches characters which are invalid in the final string.
DOT          t means add dots to abbreviated words.
KEEP-FP      t means to keep a final punctuation when applicable.
NWORDS       Number of words to use.
MAXCHAR      Maximum number of characters in the final string.
INVALID      nil: Throw away any words containing stuff matched with INVALID-RE.
             t:   Throw away only the matched part, not the whole word.
ABBREV       nil: Never abbreviate words.
             t:   Always abbreviate words (see `reftex-abbrev-parameters').
             not t and not nil: Abbreviate words if necessary to shorten
                                string below MAXCHAR.
SEP          String separating different words in the output string.
IGNORE-WORDS List of words which should be removed from the string."

  (let* ((words0 (split-string string (or split-re "[ \t\n\r]")))
         (reftex-label-illegal-re (or invalid-re "\000"))
         (abbrev-re (concat
                     "\\`\\("
                     (make-string (nth 0 reftex-abbrev-parameters) ?.)
                     "[" (nth 2 reftex-abbrev-parameters) "]*"
                     "\\)"
                     "[" (nth 3 reftex-abbrev-parameters) "]"
                     (make-string (1- (nth 1 reftex-abbrev-parameters)) ?.)))
         words word)

    ;; Remove words from the ignore list or with funny characters
    (while (setq word (pop words0))
      (if downcase (setq word (downcase word)))
      (cond
       ((member (downcase word) ignore-words))
       ((string-match reftex-label-illegal-re word)
        (when invalid
          (while (string-match reftex-label-illegal-re word)
            (setq word (replace-match "" nil nil word)))
          (push word words)))
       (t
        (push word words))))
    (setq words (nreverse words))

    ;; Restrict number of words
    (if (> (length words) nwords)
        (setcdr (nthcdr (1- nwords) words) nil))

    ;; First, try to use all words
    (setq string (mapconcat #'identity words sep))

    ;; Abbreviate words if enforced by user settings or string length
    (if (or (eq t abbrev)
            (and abbrev
                 (> (length string) maxchar)))
        (setq words
              (mapcar
               (lambda (w) (if (string-match abbrev-re w)
                               (if dot
                                   (concat (match-string 1 w) ".")
                                 (match-string 1 w))
                             w))
               words)
              string (mapconcat #'identity words sep)))

    ;; Shorten if still to long
    (setq string
          (if (> (length string) maxchar)
              (substring string 0 maxchar)
            string))

    ;; Delete the final punctuation, if any
    (if (and (not keep-fp) (string-match "\\s.+\\'" string))
        (setq string (replace-match "" nil nil string)))
    string))

(defun reftex-nicify-text (text)
  "Make TEXT nice for inclusion as context into label menu."
  ;; 1. remove line breaks and extra white space
  (while (string-match "[\n\r\t]\\|[ \t][ \t]+" text)
    (setq text (replace-match " " nil t text)))
  ;; 2. cut before the next `\end{' or `\item' or `\\'
  (if (string-match "\\(\\\\end{\\|\\\\item\\|\\\\\\\\\\).*" text)
      (setq text (replace-match "" nil t text)))
  ;; 3. kill the embedded label
  (if (string-match "\\\\label{[^}]*}" text)
      (setq text (replace-match "" nil t text)))
  ;; 4. remove leading garbage
  (if (string-match "\\`[ }]+" text)
      (setq text (replace-match "" nil t text)))
  ;; 5. limit length
  (cond
   ((> (length text) 100) (substring text 0 100))
   ((= (length text) 0) (make-string 1 ?\ ))
   (t text)))


;;; =========================================================================
;;;
;;; Fontification and Highlighting

(defun reftex-refontify ()
  "Return t if we need to refontify context."
  (and reftex-use-fonts
       (or (eq t reftex-refontify-context)
           (and (eq 1 reftex-refontify-context)
                ;; Test of we use the font-lock version of x-symbol
                (and (featurep 'x-symbol-tex) (not (boundp 'x-symbol-mode)))))))

(defun reftex-fontify-select-label-buffer (parent-buffer)
  "Fontify the `*RefTeX Select*' buffer.
Buffer is temporarily renamed to start with none-SPC char, because
Font-Lock otherwise refuses operation."
  (run-hook-with-args 'reftex-pre-refontification-functions
                      parent-buffer 'reftex-ref)
  (let* ((oldname (buffer-name))
         (newname (concat "Fontify-me-" oldname)))
    (unwind-protect
        (progn
          ;; Rename buffer temporarily to start without space (because
          ;; of font-lock)
          (rename-buffer newname t)
          ;; Good: we have the indirection functions
          (setq-local font-lock-fontify-region-function
                      #'reftex-select-font-lock-fontify-region)
          (let ((major-mode 'latex-mode))
            (font-lock-mode 1)))
      (rename-buffer oldname))))

(defun reftex-select-font-lock-fontify-region (beg end &optional _loudly)
  "Fontify a region, but only lines starting with a dot."
  (let ((func (if (fboundp 'font-lock-default-fontify-region)
                  'font-lock-default-fontify-region
                'font-lock-fontify-region))
        beg1 end1)
    (goto-char beg)
    (while (re-search-forward "^\\." end t)
      (setq beg1 (point) end1 (progn (skip-chars-forward "^\n") (point)))
      (funcall func beg1 end1 nil)
      (goto-char end1))))

(defun reftex-select-font-lock-unfontify (&rest _ignore) t)

(defun reftex-verified-face (&rest faces)
  "Return the first valid face in FACES, or nil if none is valid.
Also, when finding a nil element in FACES, return nil.  This
function is just a safety net to catch name changes of builtin
fonts.  Currently it is only used for reftex-label-face."
  (let (face)
    (catch 'exit
      (while (setq face (pop faces))
        (if (facep face) (throw 'exit face))))))

(define-obsolete-function-alias 'reftex-make-overlay #'make-overlay "28.1")
(define-obsolete-function-alias 'reftex-overlay-put #'overlay-put "28.1")
(define-obsolete-function-alias 'reftex-move-overlay #'move-overlay "28.1")
(define-obsolete-function-alias 'reftex-delete-overlay #'delete-overlay "28.1")

;; We keep a vector with several different overlays to do our highlighting.
(defvar reftex-highlight-overlays [nil nil nil])

;; Initialize the overlays
(aset reftex-highlight-overlays 0 (make-overlay 1 1))
(overlay-put (aref reftex-highlight-overlays 0)
             'face 'highlight)
(aset reftex-highlight-overlays 1 (make-overlay 1 1))
(overlay-put (aref reftex-highlight-overlays 1)
             'face reftex-cursor-selected-face)
(aset reftex-highlight-overlays 2 (make-overlay 1 1))
(overlay-put (aref reftex-highlight-overlays 2)
             'face reftex-cursor-selected-face)

;; Two functions for activating and deactivation highlight overlays
(defun reftex-highlight (index begin end &optional buffer)
  "Highlight a region with overlay INDEX."
  (move-overlay (aref reftex-highlight-overlays index)
                begin end (or buffer (current-buffer))))
(defun reftex-unhighlight (index)
  "Detach overlay INDEX."
  (delete-overlay (aref reftex-highlight-overlays index)))

(defun reftex-highlight-shall-die ()
  "Function used in pre-command-hook to remove highlights."
  (remove-hook 'pre-command-hook #'reftex-highlight-shall-die)
  (reftex-unhighlight 0))

;;; =========================================================================
;;;
;;; Keybindings

(defvar bibtex-mode-map)

;; Bind `reftex-view-crossref-from-bibtex' in BibTeX mode map
(eval-after-load
 "bibtex"
 '(define-key bibtex-mode-map "\C-c&" #'reftex-view-crossref-from-bibtex))

;;; =========================================================================
;;;
;;; Menu

;; Define a menu for the menu bar if Emacs is running under X

(defvar-local reftex-isearch-minor-mode nil)

(easy-menu-define reftex-mode-menu reftex-mode-map
 "Menu used in RefTeX mode."
 `("Ref"
   ["Table of Contents"       reftex-toc t]
   ["Recenter TOC"            reftex-toc-recenter t]
   "--"
   ["\\label"                 reftex-label t]
   ["\\ref"                   reftex-reference t]
   ["\\cite"                  reftex-citation t]
   ("\\index"
    ["\\index"                    reftex-index t]
    ["\\index{THIS}"              reftex-index-selection-or-word t]
    "--"
    ["Add THIS to Index Phrases"  reftex-index-phrase-selection-or-word t]
    ["Visit Phrase Buffer"        reftex-index-visit-phrases-buffer t]
    ["Apply Phrases to Region"    reftex-index-phrases-apply-to-region t]
    "--"
    ["Display the Index"          reftex-display-index t])
   "--"
   ["View Crossref"           reftex-view-crossref t]
   "--"
   ("Parse Document"
    ["One File"               reftex-parse-one reftex-enable-partial-scans]
    ["Entire Document"        reftex-parse-all t]
    ["Save to File"           (reftex-access-parse-file 'write)
     (> (length (symbol-value reftex-docstruct-symbol)) 0)]
    ["Restore from File"      (reftex-access-parse-file 'restore) t])
   ("Global Actions"
    ["Search Whole Document"  reftex-search-document t]
    ["Search Again"           tags-loop-continue t]
    ["Replace in Document"    reftex-query-replace-document t]
    ["Grep on Document"       reftex-grep-document t]
    "--"
    ["Goto Label"             reftex-goto-label t]
    ["Find Duplicate Labels"  reftex-find-duplicate-labels t]
    ["Change Label and Refs"  reftex-change-label t]
    ["Renumber Simple Labels" reftex-renumber-simple-labels t]
    "--"
    ["Create BibTeX File"     reftex-create-bibtex-file t]
    "--"
    ["Create TAGS File"       reftex-create-tags-file t]
    "--"
    ["Save Document"          reftex-save-all-document-buffers t])
   "--"
   ("Options"
    "PARSER"
    ["Partial Scans"
     (setq reftex-enable-partial-scans (not reftex-enable-partial-scans))
     :style toggle :selected reftex-enable-partial-scans]
    ["Auto-Save Parse Info"
     (setq reftex-save-parse-info (not reftex-save-parse-info))
     :style toggle :selected reftex-save-parse-info]
    "--"
    "TOC RECENTER"
    ["Automatic Recenter" reftex-toggle-auto-toc-recenter
     :style toggle :selected reftex-toc-auto-recenter-timer]
    "--"
    "CROSSREF INFO"
    ["Automatic Info" reftex-toggle-auto-view-crossref
     :style toggle :selected reftex-auto-view-crossref-timer]
    ["...in Echo Area" (setq reftex-auto-view-crossref t)
     :style radio :selected (eq reftex-auto-view-crossref t)]
    ["...in Other Window" (setq reftex-auto-view-crossref 'window)
     :style radio :selected (eq reftex-auto-view-crossref 'window)]
    "--"
    "MISC"
    ["AUCTeX Interface" reftex-toggle-plug-into-AUCTeX
     :style toggle :selected reftex-plug-into-AUCTeX]
    ["isearch whole document" reftex-isearch-minor-mode
     :style toggle :selected reftex-isearch-minor-mode])
   ("Reference Style"
    ,@(let (list item)
	(dolist (elt reftex-ref-style-alist)
	  (setq elt (car elt)
		item (vector
		      elt
		      `(reftex-ref-style-toggle ,elt)
		      :style 'toggle
		      :selected `(member ,elt (reftex-ref-style-list))))
	  (unless (member item list)
            (setq list (append list (list item)))))
	list))
   ("Citation Style"
    ,@(mapcar
       (lambda (x)
         (vector
          (capitalize (symbol-name (car x)))
          (list 'reftex-set-cite-format (list 'quote (car x)))
          :style 'radio :selected
          (list 'eq (list 'reftex-get-cite-format) (list 'quote (car x)))))
       reftex-cite-format-builtin)
    "--"
    "Sort Database Matches"
    ["Not" (setq reftex-sort-bibtex-matches nil)
     :style radio :selected (eq reftex-sort-bibtex-matches nil)]
    ["by Author" (setq reftex-sort-bibtex-matches 'author)
     :style radio :selected (eq reftex-sort-bibtex-matches 'author)]
    ["by Year" (setq reftex-sort-bibtex-matches 'year)
     :style radio :selected (eq reftex-sort-bibtex-matches 'year)]
    ["by Year, reversed" (setq reftex-sort-bibtex-matches 'reverse-year)
     :style radio :selected (eq reftex-sort-bibtex-matches 'reverse-year)])
   ("Index Style"
    ,@(mapcar
       (lambda (x)
         (vector
          (capitalize (symbol-name (car x)))
          (list 'reftex-add-index-macros (list 'list (list 'quote (car x))))
          :style 'radio :selected
          (list 'memq (list 'quote (car x))
                (list 'get 'reftex-docstruct-symbol
                      (list 'quote 'reftex-index-macros-style)))))
       reftex-index-macros-builtin))
   "--"
    ["Reset RefTeX Mode"       reftex-reset-mode t]
   "--"
   ("Customize"
    ["Browse RefTeX Group" reftex-customize t]
    "--"
    ["Build Full Customize Menu" reftex-create-customize-menu])
   ("Documentation"
    ["Info" reftex-info t]
    ["Commentary" reftex-show-commentary t])))

(defun reftex-customize ()
  "Call the customize function with reftex as argument."
  (interactive)
  (customize-browse 'reftex))

(defun reftex-create-customize-menu ()
  "Create a full customization menu for RefTeX, insert it into the menu."
  (interactive)
  (easy-menu-change
   '("Ref") "Customize"
   `(["Browse RefTeX group" reftex-customize t]
     "--"
     ,(customize-menu-create 'reftex)
     ["Set" Custom-set t]
     ["Save" Custom-save t]
     ["Reset to Current" Custom-reset-current t]
     ["Reset to Saved" Custom-reset-saved t]
     ["Reset to Standard Settings" Custom-reset-standard t]))
  (message "\"Ref\"-menu now contains full customization menu"))


;;; Misc

(defun reftex-show-commentary ()
  "Use the finder to view the file documentation from `reftex.el'."
  (interactive)
  (finder-commentary "reftex.el"))

(defun reftex-info (&optional node)
  "Read documentation for RefTeX in the info system.
With optional NODE, go directly to that node."
  (interactive)
  (info (format "(reftex)%s" (or node ""))))

(defun reftex-report-bug ()
  "Report a bug in RefTeX.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your RefTeX version and configuration."
  (interactive)
  (require 'reporter)
  (defvar reporter-prompt-for-summary-p)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     "bug-auctex@gnu.org, bug-gnu-emacs@gnu.org"
     reftex-version
     (list 'window-system
	   'reftex-plug-into-AUCTeX)
     nil nil
     "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen.

Check if the bug is reproducible with an up-to-date version of
RefTeX available from https://www.gnu.org/software/auctex/.

If the bug is triggered by a specific (La)TeX file, you should try
to produce a minimal sample file showing the problem and include it
in your report.

Your bug report will be posted to the AUCTeX bug reporting list.
------------------------------------------------------------------------")))

;;; Install the kill-buffer and kill-emacs hooks ------------------------------

(add-hook 'kill-buffer-hook #'reftex-kill-buffer-hook)
(unless noninteractive
  (add-hook 'kill-emacs-hook #'reftex-kill-emacs-hook))

;;; Run Hook ------------------------------------------------------------------

(run-hooks 'reftex-load-hook)

;;; That's it! ----------------------------------------------------------------

(setq reftex-tables-dirty t)  ; in case this file is evaluated by hand

(define-obsolete-function-alias 'reftex-window-height #'window-height "30.1")

(defun reftex-use-fonts ()
  (declare (obsolete "use variable `reftex-use-fonts' instead." "30.1"))
  reftex-use-fonts)

(defun reftex-remove-symbols-from-list (list)
  (declare (obsolete seq-remove "30.1"))
  (seq-remove #'symbolp list))

(provide 'reftex)

;;; reftex.el ends here
