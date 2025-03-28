;;; info-look.el --- major-mode-sensitive Info index lookup facility -*- lexical-binding: t -*-

;; Copyright (C) 1995-1999, 2001-2025 Free Software Foundation, Inc.

;; Author: Ralph Schleicher <rs@ralph-schleicher.de>
;; Keywords: help languages

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

;; Really cool code to lookup info indexes.
;; Try especially info-lookup-symbol (aka C-h S).

;; Some additional sources of (Tex)info files for non-GNU packages:
;;
;; Scheme: https://groups.csail.mit.edu/mac/ftpdir/scm/r5rs.info.tar.gz
;; LaTeX: https://mirrors.ctan.org/info/latex2e-help-texinfo/latex2e.texi
;;  (or CTAN mirrors)
;; Python: https://www.python.org/ftp/python/doc/
;; SICP: https://github.com/webframp/sicp-info

;; Traditionally, makeinfo quoted `like this', but version 5 and later
;; quotes 'like this' or ‘like this’.  Doc specs with patterns
;; therefore match open and close quotes with ['`‘] and ['’],
;; respectively.

;;; Code:

(require 'info)
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

(defgroup info-lookup nil
  "Major mode sensitive help agent."
  :group 'help :group 'languages)

(defvar-local info-lookup-mode nil
  "Symbol of the current buffer's help mode.
Help is provided according to the buffer's major mode if value is nil.")

(defcustom info-lookup-other-window-flag t
 "Non-nil means pop up the Info buffer in another window."
 :type 'boolean)

(defcustom info-lookup-highlight-face 'match
  "Face for highlighting looked up help items.
Setting this variable to nil disables highlighting."
  :type 'face)

(defvar info-lookup-highlight-overlay nil
  "Overlay object used for highlighting.")

(defcustom info-lookup-file-name-alist
  '(("\\`ac\\(local\\|site\\|include\\)\\.m4\\'" . autoconf-mode))
  "Alist of file names handled specially.
List elements are cons cells of the form

    (REGEXP . MODE)

If a file name matches REGEXP, then use help mode MODE instead of the
buffer's major mode."
  :type '(repeat (cons (regexp :tag "Regexp")
					   (symbol :tag "Mode"))))

(defvar info-lookup-history nil
  "History of previous input lines.")

(defvar info-lookup-alist nil
  "Alist of known help topics.
Cons cells are of the form

    (HELP-TOPIC . HELP-DATA)

HELP-TOPIC is the symbol of a help topic.
HELP-DATA is a HELP-TOPIC's public data set.
 Value is an alist with elements of the form

    (HELP-MODE REGEXP IGNORE-CASE DOC-SPEC PARSE-RULE OTHER-MODES)

HELP-MODE is either a mode's symbol, or a cons cell of the
form (HELP-MODE . SYMBOL-PREFIX), where SYMBOL-PREFIX is the
prefix (the part up to the first dash) of names of symbols whose
documentation is specified by DOC-SPEC.
REGEXP is a regular expression matching those help items whose
 documentation can be looked up via DOC-SPEC.
IGNORE-CASE is non-nil if help items are case insensitive.
DOC-SPEC is a list of documentation specifications of the form

    (INFO-NODE TRANS-FUNC PREFIX SUFFIX)

INFO-NODE is the name (including file name part) of an Info index.
TRANS-FUNC is a function translating index entries into help items;
 nil means add only those index entries matching REGEXP, a string
 means prepend string to the first word of all index entries.
PREFIX and SUFFIX are parts of a regular expression.  If one of
 them is non-nil then search the help item's Info node for the
 first occurrence of the regular expression `PREFIX ITEM SUFFIX'.
 ITEM will be highlighted with `info-lookup-highlight-face' if this
 variable is not nil.
PARSE-RULE is either the symbol name of a function or a regular
 expression for guessing the default help item at point.  Fuzzy
 regular expressions like \"[_a-zA-Z0-9]+\" do a better job if
 there are no clear delimiters; do not try to write too complex
 expressions.  PARSE-RULE defaults to REGEXP.
OTHER-MODES is a list of cross references to other help modes.")

(defsubst info-lookup->topic-value (topic)
  (cdr (assoc topic info-lookup-alist)))

(defsubst info-lookup->mode-value (topic mode)
  (assoc mode (info-lookup->topic-value topic)))

(defun info-lookup--expand-info (info)
  ;; We have a dynamic doc-spec function.
  (when (and (null (nth 3 info))
             (nth 6 info)
             (functionp (nth 6 info)))
    (setf (nth 3 info) (funcall (nth 6 info))
          (nth 6 info) nil))
  info)

(defsubst info-lookup->regexp (topic mode)
  (nth 1 (info-lookup->mode-value topic mode)))

(defsubst info-lookup->ignore-case (topic mode)
  (nth 2 (info-lookup->mode-value topic mode)))

(defsubst info-lookup->doc-spec (topic mode)
  (nth 3 (info-lookup->mode-value topic mode)))

(defsubst info-lookup->parse-rule (topic mode)
  (nth 4 (info-lookup->mode-value topic mode)))

(defsubst info-lookup->other-modes (topic mode)
  (nth 5 (info-lookup->mode-value topic mode)))

(defun info-lookup-add-help (&rest arg)
  "Add or update a help specification.
Function arguments are specified as keyword/argument pairs:

    (KEYWORD . ARGUMENT)

KEYWORD is either `:topic', `:mode', `:regexp', `:ignore-case',
 `:doc-spec', `:parse-rule', `:other-modes' or `:doc-spec-function'.
  `:doc-spec-function' is used to compute a `:doc-spec', but instead of
  doing so at load time, this is done when the user asks for info on
  the mode in question.

ARGUMENT is the value corresponding to KEYWORD.  The meaning of the values
is explained in the documentation of the variable `info-lookup-alist': for
example, the value corresponding to `:topic' is documented as HELP-TOPIC,
the value of `:mode' as HELP-MODE, etc..

If no topic or mode option has been specified, then the help topic defaults
to `symbol', and the help mode defaults to the current major mode."
  (apply #'info-lookup-add-help* nil arg))

(defun info-lookup-maybe-add-help (&rest arg)
  "Add a help specification if none is defined.
See the documentation of the function `info-lookup-add-help'
for more details."
  (apply #'info-lookup-add-help* t arg))

(defun info-lookup-add-help* (maybe &rest arg)
  (let (topic mode regexp ignore-case doc-spec
	      parse-rule other-modes keyword value
              doc-spec-function)
    (setq topic 'symbol
	  mode major-mode
	  regexp "\\w+")
    (while arg
      (setq keyword (car arg))
      (or (symbolp keyword)
	  (error "Junk in argument list \"%S\"" arg))
      (setq arg (cdr arg))
      (and (null arg)
	   (error "Keyword \"%S\" is missing an argument" keyword))
      (setq value (car arg)
	    arg (cdr arg))
      (cond ((eq keyword :topic)
	     (setq topic value))
	    ((eq keyword :mode)
	     (setq mode value))
	    ((eq keyword :regexp)
	     (setq regexp value))
	    ((eq keyword :ignore-case)
	     (setq ignore-case value))
	    ((eq keyword :doc-spec)
	     (setq doc-spec value))
	    ((eq keyword :doc-spec-function)
	     (setq doc-spec-function value))
	    ((eq keyword :parse-rule)
	     (setq parse-rule value))
	    ((eq keyword :other-modes)
	     (setq other-modes value))
	    (t
	     (error "Unknown keyword \"%S\"" keyword))))
    (or (and maybe (info-lookup->mode-value topic mode))
	(let* ((data (list regexp ignore-case doc-spec parse-rule other-modes
                           doc-spec-function))
	       (topic-cell (or (assoc topic info-lookup-alist)
			       (car (setq info-lookup-alist
					  (cons (cons topic nil)
						info-lookup-alist)))))
	       (mode-cell (assoc mode topic-cell)))
	  (if (null mode-cell)
	      (setcdr topic-cell (cons (cons mode data) (cdr topic-cell)))
	    (setcdr mode-cell data))))
    nil))

(defvar info-lookup-cache nil
  "Cache storing data maintained automatically by the program.
Value is an alist with cons cell of the form

    (HELP-TOPIC . ((HELP-MODE INITIALIZED COMPLETIONS REFER-MODES) ...))

HELP-TOPIC is the symbol of a help topic.
HELP-MODE is a mode's symbol.
INITIALIZED is nil if HELP-MODE is uninitialized, t if
 HELP-MODE is initialized, and `0' means HELP-MODE is
 initialized but void.
COMPLETIONS is an alist of documented help items.
REFER-MODES is a list of other help modes to use.")

(defsubst info-lookup->cache (topic)
  (or (assoc topic info-lookup-cache)
      (car (setq info-lookup-cache
		 (cons (cons topic nil)
		       info-lookup-cache)))))

(defun info-lookup->topic-cache (topic)
  (cdr (info-lookup->cache topic)))

(defun info-lookup->mode-cache (topic mode)
  (assoc mode (info-lookup->topic-cache topic)))

(defun info-lookup->initialized (topic mode)
  (nth 1 (info-lookup->mode-cache topic mode)))

(defun info-lookup->completions (topic mode)
  (or (info-lookup->initialized topic mode)
      (info-lookup-setup-mode topic mode))
  (nth 2 (info-lookup->mode-cache topic mode)))

(defun info-lookup->refer-modes (topic mode)
  (or (info-lookup->initialized topic mode)
      (info-lookup-setup-mode topic mode))
  (nth 3 (info-lookup->mode-cache topic mode)))

(defun info-lookup->all-modes (topic mode)
  (cons mode (info-lookup->refer-modes topic mode)))

(defun info-lookup-quick-all-modes (topic mode)
  (cons mode (info-lookup->other-modes topic mode)))

;;;###autoload
(defun info-lookup-reset ()
  "Throw away all cached data.
This command is useful if the user wants to start at the beginning without
quitting Emacs, for example, after some Info documents were updated on the
system."
  (interactive)
  (setq info-lookup-cache nil))

;;;###autoload (put 'info-lookup-symbol 'info-file "emacs")
;;;###autoload
(defun info-lookup-symbol (symbol &optional mode same-window)
  "Look up and display documentation of SYMBOL in the relevant Info manual.
SYMBOL should be an identifier: a function or method, a macro, a variable,
a data type, a class, etc.

Interactively, prompt for SYMBOL; you can use \\<minibuffer-local-completion-map>\\[next-history-element] in the minibuffer
to yank the default argument value into the minibuffer so you can edit it.
The default symbol is the one found at point.

MODE is the major mode whose Info manuals to search for the documentation
of SYMBOL.  It defaults to the current buffer's `major-mode'; if that
mode doesn't have any Info manuals known to Emacs, the command will
prompt for MODE to use, with completion.  With prefix arg, the command
always prompts for MODE.

Is SAME-WINDOW, try to reuse the current window instead of
popping up a new one."
  (interactive
   (info-lookup-interactive-arguments 'symbol current-prefix-arg))
  (info-lookup 'symbol symbol mode same-window))

;;;###autoload (put 'info-lookup-file 'info-file "emacs")
;;;###autoload
(defun info-lookup-file (file &optional mode)
  "Look up and display documentation of FILE in the relevant Info manual.
FILE should be the name of a file; a notable example is a standard header
file that is part of the C or C++ standard library.

Interactively, prompt for FILE; you can use \\<minibuffer-local-completion-map>\\[next-history-element] in the minibuffer
to yank the default argument value into the minibuffer so you can edit it.
The default file name is the one found at point.

MODE is the major mode whose Info manuals to search for the documentation
of FILE.  It defaults to the current buffer's `major-mode'; if that
mode doesn't have any Info manuals known to Emacs, the command will
prompt for MODE to use, with completion.  With prefix arg, the command
always prompts for MODE."
  (interactive
   (info-lookup-interactive-arguments 'file current-prefix-arg))
  (info-lookup 'file file mode))

(defun info-lookup-interactive-arguments (topic &optional query)
  "Read and return argument value (and help mode) for help TOPIC.
TOPIC should be any known symbol of a help topic, such as `file'
or `symbol'.  See the documentation of HELP-TOPIC in the doc
string of `info-lookup-alist'.
If optional argument QUERY is non-nil, query for the help mode."
  (let* ((mode (cond (query
		      (info-lookup-change-mode topic))
		     ((when-let*
                          ((info (info-lookup->mode-value
                                  topic (info-lookup-select-mode))))
                        (info-lookup--expand-info info))
                      info-lookup-mode)
		     ((info-lookup-change-mode topic))))
	 (completions (info-lookup->completions topic mode))
	 (default (info-lookup-guess-default topic mode))
	 (completion-ignore-case (info-lookup->ignore-case topic mode))
	 (enable-recursive-minibuffers t)
	 (value (completing-read
		 (format-prompt "Describe %s" default topic)
		 completions nil nil nil 'info-lookup-history default)))
    (list (if (equal value "") default value) mode)))

(defun info-lookup-select-mode ()
  (when (and (not info-lookup-mode) (buffer-file-name))
    (let ((file-name (file-name-nondirectory (buffer-file-name)))
	  (file-name-alist info-lookup-file-name-alist))
      (while (and (not info-lookup-mode) file-name-alist)
	(when (string-match (caar file-name-alist) file-name)
	  (setq info-lookup-mode (cdar file-name-alist)))
	(setq file-name-alist (cdr file-name-alist)))))

  ;; If major-mode has no setups in info-lookup-alist, under any topic, then
  ;; search up through `derived-mode-all-parents' to find a parent mode which
  ;; does have some setups.  This means that a `define-derived-mode' with no
  ;; setups of its own will select its parent mode for lookups, if one of
  ;; its parents has some setups.  Good for example on `makefile-gmake-mode'
  ;; and similar derivatives of `makefile-mode'.
  ;;
  (let ((modes (derived-mode-all-parents major-mode))) ;; Look for `mode' with some setups.
    (while (and modes (not info-lookup-mode))
      (dolist (topic-cell info-lookup-alist) ;; Usually only two topics here.
        (if (info-lookup->mode-value (car topic-cell) (car modes))
            (setq info-lookup-mode (car modes))))
      (setq modes (cdr modes))))

  (or info-lookup-mode (setq info-lookup-mode major-mode)))

(defun info-lookup-change-mode (topic)
  (let* ((completions (mapcar (lambda (arg)
                                (let ((mode-spec (car arg)))
                                  (and (consp mode-spec)
                                       (setq mode-spec (car mode-spec)))
				  (cons (symbol-name mode-spec) mode-spec)))
			      (info-lookup->topic-value topic)))
	 (mode (completing-read
		(format "Major mode whose manuals to search for this %s: "
                        topic)
		completions nil t nil 'info-lookup-history)))
    (or (setq mode (cdr (assoc mode completions)))
	(error "No manuals available for %s" topic))
    (or (info-lookup->mode-value topic mode)
	(error "The manuals of `%s' have no %s help" mode topic))
    (setq info-lookup-mode mode)))

(defun info-lookup--item-to-mode (item mode)
  (let ((spec (cons mode (car (split-string (if (stringp item)
                                                item
                                              (symbol-name item))
                                            "-")))))
    (if (assoc spec (cdr (assq 'symbol info-lookup-alist)))
        spec
      mode)))

(defun info-lookup (topic item mode &optional same-window)
  "Display the documentation of TOPIC whose name is ITEM, using MODE's manuals.
TOPIC should be any known symbol of a help topic type, such as `file'
or `symbol'.  See the documentation of HELP-TOPIC in the doc
string of `info-lookup-alist'.
ITEM is the item whose documentation to search: file name if
TOPIC is `file', a symbol if TOPIC is `symbol', etc.
MODE is the `major-mode' whose Info manuals to search for documentation
of ITEM; if it's nil, the function uses `info-lookup-file-name-alist'
and the current buffer's file name to guess the mode.

If SAME-WINDOW, reuse the current window.  If nil, pop to a
different window."
  (or mode (setq mode (info-lookup-select-mode)))
  (setq mode (info-lookup--item-to-mode item mode))
  (let* ((completions (info-lookup->completions topic mode))
         (ignore-case (info-lookup->ignore-case topic mode))
         (entry (or (assoc (if ignore-case (downcase item) item) completions)
                    (assoc-string item completions t)
                    (error "Not documented as a %s: %s" topic (or item ""))))
         (modes (info-lookup->all-modes topic mode))
         (window (selected-window))
	 (new-Info-history
	  ;; Avoid clobbering Info-history with nodes searched during
	  ;; lookup.  If lookup succeeds set `Info-history' to
	  ;; `new-Info-history'.
	  (when (get-buffer "*info*")
	    (with-current-buffer "*info*"
	      (cons (list Info-current-file Info-current-node (point))
		    Info-history))))
         found doc-spec node prefix suffix doc-found)
    (unless (eq major-mode 'Info-mode)
      (if (not info-lookup-other-window-flag)
	  (info)
	(save-window-excursion (info))
        (if same-window
            (pop-to-buffer-same-window "*info*")
	  (let* ((info-window (get-buffer-window "*info*" t))
	         (info-frame (and info-window (window-frame info-window))))
	    (if (and info-frame
		     (not (eq info-frame (selected-frame)))
		     (display-multi-frame-p)
		     (memq info-frame (frames-on-display-list)))
	        ;; *info* is visible in another frame on same display.
	        ;; Raise that frame and select the window.
	        (progn
		  (select-window info-window)
		  (raise-frame info-frame))
	      ;; In any other case, switch to *info* another window.
	      (switch-to-buffer-other-window "*info*"))))))
    (while (and (not found) modes)
      (setq doc-spec (info-lookup->doc-spec topic (car modes)))
      (while (and (not found) doc-spec)
	(setq node (nth 0 (car doc-spec))
	      prefix (nth 2 (car doc-spec))
	      suffix (nth 3 (car doc-spec)))
	(when (condition-case nil
		  (progn
		    ;; Don't need Index menu fontifications here, and
		    ;; they slow down the lookup.
		    (let (Info-fontify-maximum-menu-size
			  Info-history-list)
		      (Info-goto-node node)
		      (setq doc-found t)))
		(error
		 (message "Cannot access Info node %s" node)
		 (sit-for 1)
		 nil))
	  (condition-case nil
	      (progn
                ;; Don't use Info-menu, it forces case-fold-search to t
                (let ((case-fold-search nil))
                  (re-search-forward
                   (concat "^\\* " (regexp-quote (or (cdr entry) (car entry)))
                           ":")))
                (Info-follow-nearest-node)
		(setq found t)
		(if (or prefix suffix)
		    (let ((case-fold-search
			   (info-lookup->ignore-case topic (car modes)))
			  (buffer-read-only nil))
		      (goto-char (point-min))
		      (re-search-forward
		       (concat prefix (regexp-quote (car entry)) suffix))
		      (goto-char (match-beginning 0))
		      (and (display-color-p) info-lookup-highlight-face
			   ;; Search again for ITEM so that the first
			   ;; occurrence of ITEM will be highlighted.
			   (re-search-forward (regexp-quote (car entry)))
			   (let ((start (match-beginning 0))
				 (end (match-end 0)))
			     (if (overlayp info-lookup-highlight-overlay)
				 (move-overlay info-lookup-highlight-overlay
					       start end (current-buffer))
			       (setq info-lookup-highlight-overlay
				     (make-overlay start end))))
			   (overlay-put info-lookup-highlight-overlay
					'face info-lookup-highlight-face)))))
	    (error nil)))
	(setq doc-spec (cdr doc-spec)))
      (setq modes (cdr modes)))
    ;; Alert the user if case was munged, and do this after bringing up the
    ;; info buffer since that can print messages
    (unless (or ignore-case
                (string-equal item (car entry)))
      (message "Found in different case: %s" (car entry)))
    (when found
      (setq Info-history new-Info-history))
    (or doc-found
	(error "Info documentation for lookup was not found"))
    ;; Don't leave the Info buffer if the help item couldn't be looked up.
    (if (and info-lookup-other-window-flag found)
	(select-window window))))

(defun info-lookup-setup-mode (topic mode)
  "Initialize the internal data structure."
  (or (info-lookup->initialized topic mode)
      (let ((initialized 0)
	    cell data completions refer-modes Info-history-list)
	(if (not (info-lookup->mode-value topic mode))
	    (message "No %s help available for `%s'" topic mode)
	  ;; Recursively setup cross references.
	  ;; But refer only to non-void modes.
	  (dolist (arg (info-lookup->other-modes topic mode))
	    (or (info-lookup->initialized topic arg)
		(info-lookup-setup-mode topic arg))
	    (and (eq (info-lookup->initialized topic arg) t)
		 (setq refer-modes (cons arg refer-modes))))
	  (setq refer-modes (nreverse refer-modes))
	  ;; Build the full completion alist.
	  (setq completions
		(nconc (condition-case nil
			   (info-lookup-make-completions topic mode)
			 (error nil))
		       (apply #'append
			      (mapcar (lambda (arg)
					(info-lookup->completions topic arg))
				      refer-modes))))
	  (setq initialized t))
	;; Update `info-lookup-cache'.
	(setq cell (info-lookup->mode-cache topic mode)
	      data (list initialized completions refer-modes))
	(if (not cell)
	    (setcdr (info-lookup->cache topic)
		    (cons (cons mode data) (info-lookup->topic-cache topic)))
	  (setcdr cell data))
	initialized)))

(defun info-lookup-make-completions (topic mode)
  "Create a unique alist from all index entries."
  (let ((doc-spec (info-lookup->doc-spec topic mode))
	(regexp (concat "^\\(" (info-lookup->regexp topic mode)
			"\\)\\([ \t].*\\)?$"))
	Info-history-list Info-fontify-maximum-menu-size
	node trans entry item prefix result doc-found
	(buffer (get-buffer-create " temp-info-look")))
    (with-current-buffer buffer
      (Info-mode))
    (while doc-spec
      (setq node (nth 0 (car doc-spec))
	    trans (cond ((eq (nth 1 (car doc-spec)) nil)
			 (lambda (arg)
			   (if (string-match regexp arg)
			       (match-string 1 arg))))
			((stringp (nth 1 (car doc-spec)))
			 (setq prefix (nth 1 (car doc-spec)))
			 (lambda (arg)
			   (if (string-match "^\\([^: \t\n]+\\)" arg)
			       (concat prefix (match-string 1 arg)))))
			(t (nth 1 (car doc-spec)))))
      (with-current-buffer buffer
	(message "Processing Info node `%s'..." node)
	(when (condition-case nil
		  (progn
		    (Info-goto-node node)
		    (setq doc-found t))
		(error
		 (message "Cannot access Info node `%s'" node)
		 (sit-for 1)
		 nil))
	  (condition-case nil
	      (progn
		(goto-char (point-min))
		(and (search-forward "\n* Menu:" nil t)
		     (while (re-search-forward "\n\\* \\(.*\\): " nil t)
		       (setq entry (match-string 1)
			     item (funcall trans entry))
		       ;; `trans' can return nil if the regexp doesn't match.
		       (when (and item
				  ;; Sometimes there's more than one Menu:
				  (not (string= entry "Menu")))
			 (and (info-lookup->ignore-case topic mode)
			      (setq item (downcase item)))
			 (and (string-equal entry item)
			      (setq entry nil))
			 (and (or (assoc item result)
				  (setq result (cons (cons item entry)
						     result))))))))
	    (error nil))))
      (message "Processing Info node `%s'...done" node)
      (setq doc-spec (cdr doc-spec)))
    (or doc-found
	(error "Info documentation for lookup was not found"))
    result))

(defun info-lookup-guess-default (topic mode)
  "Return a guess for a symbol to look up, based on text around point.
Try all related modes applicable to TOPIC and MODE.
Return nil if there is nothing appropriate in the buffer near point."
  (let ((modes (info-lookup->all-modes topic mode))
	guess)
    (while (and (not guess) modes)
      (setq guess (info-lookup-guess-default* topic (car modes))
	    modes (cdr modes)))
    ;; Collapse whitespace characters.
    (when guess
      (let ((pos 0))
	(while (string-match "[ \t\n]+" guess pos)
	  (setq pos (1+ (match-beginning 0)))
	  (setq guess (replace-match " " t t guess)))))
    guess))

(defun info-lookup-guess-default* (topic mode)
  (let ((case-fold-search (info-lookup->ignore-case topic mode))
	(rule (or (info-lookup->parse-rule topic mode)
		  (info-lookup->regexp topic mode)))
	(start (point)) end regexp subexp result)
    (save-excursion
      (if (functionp rule)
	  (setq result (funcall rule))
	(if (consp rule)
	    (setq regexp (car rule)
		  subexp (cdr rule))
	  (setq regexp rule
		subexp 0))
	;; If at start of symbol, don't go back to end of previous one.
	(if (save-match-data
	      (looking-at "[ \t\n]"))
	    (skip-chars-backward " \t\n"))
	(setq end (point))
	(while (and (re-search-backward regexp nil t)
		    (looking-at regexp)
		    (>= (match-end 0) end))
	  (setq result (match-string subexp)))
	(if (not result)
	    (progn
	      (goto-char start)
	      (skip-chars-forward " \t\n")
	      (and (looking-at regexp)
		   (setq result (match-string subexp)))))))
    result))

(defun info-lookup-guess-c-symbol ()
  "Get the C symbol at point."
  (condition-case nil
      (progn
	(skip-syntax-backward "w_")
	(let ((start (point)) prefix name)
	  ;; Test for a leading `struct', `union', or `enum' keyword
	  ;; but ignore names like `foo_struct'.
	  (setq prefix (and (< (skip-chars-backward " \t\n") 0)
			    (< (skip-chars-backward "_a-zA-Z0-9") 0)
			    (looking-at "\\(struct\\|union\\|enum\\)\\s ")
			    (concat (match-string 1) " ")))
	  (goto-char start)
	  (and (looking-at "[_a-zA-Z][_a-zA-Z0-9]*")
	       (setq name (match-string 0)))
	  ;; Caveat!  Look forward if point is at `struct' etc.
	  (and (not prefix)
	       (or (string-equal name "struct")
		   (string-equal name "union")
		   (string-equal name "enum"))
	       (looking-at "[a-z]+\\s +\\([_a-zA-Z][_a-zA-Z0-9]*\\)")
	       (setq prefix (concat name " ")
		     name (match-string 1)))
	  (and (or prefix name)
	       (concat prefix name))))
    (error nil)))

(defun info-lookup-guess-custom-symbol ()
  "Get symbol at point in custom buffers."
  (declare (obsolete nil "28.1"))
  (condition-case nil
      (save-excursion
	(let ((case-fold-search t)
	      (ignored-chars "][()`'‘’,:.\" \t\n")
	      (significant-chars "^][()`'‘’,:.\" \t\n")
	      beg end)
	  (cond
	   ((and (memq (get-char-property (point) 'face)
			 '(custom-variable-tag custom-variable-obsolete
			   custom-variable-tag-face))
		   (setq beg (previous-single-char-property-change
			      (point) 'face nil (line-beginning-position)))
		   (setq end (next-single-char-property-change
			      (point) 'face nil (line-end-position)))
		   (> end beg))
	    (subst-char-in-string
	     ?\s ?\- (buffer-substring-no-properties beg end)))
	   ((or (and (looking-at (concat "[" significant-chars "]"))
		     (save-excursion
		       (skip-chars-backward significant-chars)
		       (setq beg (point)))
		     (skip-chars-forward significant-chars)
		     (setq end (point))
		     (> end beg))
		(and (looking-at "[ \t\n]")
		     (looking-back (concat "[" significant-chars "]")
                                   (1- (point)))
		     (setq end (point))
		     (skip-chars-backward significant-chars)
		     (setq beg (point))
		     (> end beg))
		(and (skip-chars-forward ignored-chars)
		     (setq beg (point))
		     (skip-chars-forward significant-chars)
		     (setq end (point))
		     (> end beg)))
	    (buffer-substring-no-properties beg end)))))
    (error nil)))

(defun info-lookup-guess-gdb-script-symbol ()
  "Get symbol at point in GDB script buffers."
  (condition-case nil
      (save-excursion
        (back-to-indentation)
        ;; Try to find the current line's full command in the index;
        ;; and default to the longest subset that is found.
        (when (looking-at "[-a-z]+\\(\\s-[-a-z]+\\)*")
          (let ((str-list (split-string (match-string-no-properties 0)
                                        "\\s-+" t))
                (completions (info-lookup->completions 'symbol
                                                       'gdb-script-mode)))
            (catch 'result
              (while str-list
                (let ((str (string-join str-list " ")))
                  (when (assoc str completions)
                    (throw 'result str))
                  ;; 'nbutlast' will not destructively set its argument
                  ;; to nil when the argument is a list of 1 element.
                  (if (= (length str-list) 1)
                      (setq str-list nil)
                    (nbutlast str-list))))))))
    (error nil)))

;;;###autoload
(defun info-complete-symbol (&optional mode)
  "Perform completion on symbol preceding point."
  (interactive)
  (info-complete 'symbol
		 (or mode
		     (if (info-lookup->mode-value
			  'symbol (info-lookup-select-mode))
			 info-lookup-mode
		       (info-lookup-change-mode 'symbol)))))

;;;###autoload
(defun info-complete-file (&optional mode)
  "Perform completion on file preceding point."
  (interactive)
  (info-complete 'file
		 (or mode
		     (if (info-lookup->mode-value
			  'file (info-lookup-select-mode))
			 info-lookup-mode
		       (info-lookup-change-mode 'file)))))

(defun info-lookup-completions-at-point (topic mode)
  "Try to complete a help item."
  (or mode (setq mode (info-lookup-select-mode)))
  (when (info-lookup->mode-value topic mode)
    (let ((modes (info-lookup-quick-all-modes topic mode))
          (start (point))
          try)
      (while (and (not try) modes)
        (setq mode (car modes)
              modes (cdr modes)
              try (info-lookup-guess-default* topic mode))
        (goto-char start))
      (when try
        (let ((completions (info-lookup->completions topic mode)))
          (when completions
            (when (info-lookup->ignore-case topic mode)
              (setq completions
                    (lambda (string pred action)
                      (let ((completion-ignore-case t))
                        (complete-with-action
                         action completions string pred)))))
            (save-excursion
              ;; Find the original symbol and zap it.
              (end-of-line)
              (while (and (search-backward try nil t)
                          (< start (point))))
              (list (match-beginning 0) (match-end 0) completions
                    :exclusive 'no))))))))

(defun info-complete (topic mode)
  "Try to complete a help item."
  (barf-if-buffer-read-only)
  (when-let* ((info (info-lookup->mode-value topic mode)))
    (info-lookup--expand-info info))
  (let ((data (info-lookup-completions-at-point topic mode)))
    (if (null data)
        (error "No %s completion available for `%s' at point" topic mode)
      (completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)))))


;;; Initialize some common modes.

(info-lookup-maybe-add-help
 :mode 'c-mode :topic 'symbol
 :regexp "\\(struct \\|union \\|enum \\)?[_a-zA-Z][_a-zA-Z0-9]*"
 :doc-spec '(("(libc)Function Index" nil
	      "^[ \t]+-+ \\(Function\\|Macro\\): .*\\<" "\\>")
             ;; prefix/suffix has to match things like
             ;;   " -- Macro: int F_DUPFD"
             ;;   " -- Variable: char * tzname [2]"
             ;;   "`DBL_MAX'"    (texinfo @table)
             ;; suffix "\\>" is not used because that sends DBL_MAX to
             ;; DBL_MAX_EXP ("_" is a non-word char)
	     ("(libc)Variable Index" nil
              "^\\([ \t]+-+ \\(Variable\\|Macro\\): .*\\<\\|['`‘]\\)"
              "\\( \\|['’]?$\\)")
	     ("(libc)Type Index" nil
	      "^[ \t]+-+ Data Type: \\<" "\\>")
	     ("(termcap)Var Index" nil
	      "^[ \t]*['`‘]" "['’]"))
 :parse-rule 'info-lookup-guess-c-symbol)

(info-lookup-maybe-add-help
 :mode 'c-mode :topic 'file
 :regexp "[_a-zA-Z0-9./+-]+"
 :doc-spec '(("(libc)File Index")))

(info-lookup-maybe-add-help
 :mode 'bison-mode
 :regexp "[:;|]\\|%\\([%{}]\\|[_a-z]+\\)\\|YY[_A-Z]+\\|yy[_a-z]+"
 :doc-spec '(("(bison)Index" nil
	      "['`‘]" "['’]"))
 :parse-rule "[:;|]\\|%\\([%{}]\\|[_a-zA-Z][_a-zA-Z0-9]*\\)"
 :other-modes '(c-mode))

(info-lookup-maybe-add-help
 :mode 'makefile-mode
 :regexp "\\$[^({]\\|\\.[_A-Z]*\\|[_a-zA-Z][_a-zA-Z0-9-]*"
 :doc-spec '(("(make)Name Index" nil
	      "^[ \t]*['`‘]" "['’]"))
 :parse-rule "\\$[^({]\\|\\.[_A-Z]*\\|[_a-zA-Z0-9-]+")

(info-lookup-maybe-add-help
 :topic      'symbol
 :mode       'makefile-automake-mode
 ;; similar regexp/parse-rule as makefile-mode, but also the following
 ;; (which have index entries),
 ;;   "##" special automake comment
 ;;   "+=" append operator, separate from the GNU make one
 :regexp     "\\$[^({]\\|\\.[_A-Z]*\\|[_a-zA-Z][_a-zA-Z0-9-]*\\|##\\|\\+="
 :parse-rule "\\$[^({]\\|\\.[_A-Z]*\\|[_a-zA-Z0-9-]+\\|##\\|\\+="
 :doc-spec   '(
               ;; "(automake)Macro Index" is autoconf macros used in
               ;; configure.ac, not Makefile.am, so don't have that here.
               ("(automake)Variable Index" nil "^[ \t]*['`‘]" "['’]")
               ;; In automake 1.4 macros and variables were a combined node.
               ("(automake)Macro and Variable Index" nil "^[ \t]*['`‘]"
		"['’]")
               ;; Directives like "if" are in the "General Index".
               ;; Prefix "`" since the text for say `+=' isn't always an
               ;; @item etc and so not always at the start of a line.
               ("(automake)General Index" nil "['`‘]" "['’]")
               ;; In automake 1.3 there was just a single "Index" node.
               ("(automake)Index" nil "['`‘]" "['’]"))
 :other-modes '(makefile-mode))

(info-lookup-maybe-add-help
 :mode 'texinfo-mode
 :regexp "@\\([a-zA-Z]+\\|[^a-zA-Z]\\)"
 :doc-spec `(("(texinfo)Command and Variable Index"
              ;; Ignore Emacs commands and prepend a `@'.
              ,(lambda (item)
                 (if (string-match "^\\([a-zA-Z]+\\|[^a-zA-Z]\\)\\( .*\\)?$" item)
                     (concat "@" (match-string 1 item))))
              "['`‘]" "['’ ]")))

(info-lookup-maybe-add-help
 :mode 'm4-mode
 :regexp "[_a-zA-Z][_a-zA-Z0-9]*"
 :doc-spec '(("(m4)Macro index"))
 :parse-rule "[_a-zA-Z0-9]+")

(info-lookup-maybe-add-help
 :mode 'autoconf-mode
 :regexp "A[CM]_[_A-Z0-9]+"
 :doc-spec `(;; Autoconf Macro Index entries are without an "AC_" prefix,
             ;; but with "AH_" or "AU_" for those.  So add "AC_" if there
             ;; isn't already an "A._".
             ("(autoconf)Autoconf Macro Index"
              ,(lambda (item)
                 (if (string-match "^A._" item) item (concat "AC_" item)))
              "^[ \t]+-+ \\(Macro\\|Variable\\): .*\\<" "\\>")
             ;; M4 Macro Index entries are without "AS_" prefixes, and
             ;; mostly without "m4_" prefixes.  "dnl" is an exception, not
             ;; wanting any prefix.  So AS_ is added back to upper-case
             ;; names (if needed), m4_ to others which don't already an m4_.
             ("(autoconf)M4 Macro Index"
              ,(lambda (item)
                 (let ((case-fold-search nil))
                   (cond ((or (string-equal item "dnl")
                              (string-match "^m4_" item)
                              ;; Autoconf 2.62 index includes some macros
                              ;; (e.g., AS_HELP_STRING), so avoid prefixing.
                              (string-match "^AS_" item))
                          item)
                         ((string-match "^[A-Z0-9_]+$" item)
                          (concat "AS_" item))
                         (t
                          (concat "m4_" item)))))
              "^[ \t]+-+ Macro: .*\\<" "\\>")
             ;; Autotest Macro Index entries are without "AT_".
             ("(autoconf)Autotest Macro Index" "AT_"
	      "^[ \t]+-+ Macro: .*\\<" "\\>")
	     ;; This is for older versions (probably pre autoconf 2.5x):
	     ("(autoconf)Macro Index" "AC_"
	      "^[ \t]+-+ \\(Macro\\|Variable\\): .*\\<" "\\>")
	     ;; Automake has index entries for its notes on various autoconf
	     ;; macros (eg. AC_PROG_CC).  Ensure this is after the autoconf
	     ;; index, so as to prefer the autoconf docs.
	     ("(automake)Macro and Variable Index" nil
	      "^[ \t]*['`‘]" "['’]"))
 ;; Autoconf symbols are M4 macros.  Thus use M4's parser.
 :parse-rule 'ignore
 :other-modes '(m4-mode))

(info-lookup-maybe-add-help
 :mode 'awk-mode
 :regexp "[_a-zA-Z]+"
 :doc-spec `(("(gawk)Index"
              ,(lambda (item)
                 (let ((case-fold-search nil))
                   (cond
                    ;; `BEGIN' and `END'.
                    ((string-match "^\\([A-Z]+\\) special pattern\\b" item)
                     (match-string 1 item))
                    ;; `if', `while', `do', ...
                    ((string-match "^\\([a-z]+\\) statement\\b" item)
                     (if (not (string-equal (match-string 1 item) "control"))
                         (match-string 1 item)))
                    ;; `NR', `NF', ...
                    ((string-match "^[A-Z]+$" item)
                     item)
                    ;; Built-in functions (matches to many entries).
                    ((string-match "^[a-z]+$" item)
                     item))))
              "['`‘]" "\\([ \t]*([^)]*)\\)?['’]")))

(info-lookup-maybe-add-help
 :mode 'perl-mode
 :regexp "[$@%][^a-zA-Z]\\|\\$\\^[A-Z]\\|[$@%]?[a-zA-Z][_a-zA-Z0-9]*"
 :doc-spec `(("(perl5)Function Index"
              ,(lambda (item)
                 (if (string-match "^\\([a-zA-Z0-9]+\\)" item)
                     (match-string 1 item)))
              "^" "\\b")
             ("(perl5)Variable Index"
              ,(lambda (item)
                 ;; Work around bad formatted array variables.
                 (let ((sym (cond ((or (string-match "^\\$\\(.\\|@@\\)$" item)
                                       (string-match "^\\$\\^[A-Z]$" item))
                                   item)
                                  ((string-match
                                    "^\\([$%@]\\|@@\\)?[_a-zA-Z0-9]+" item)
                                   (match-string 0 item))
                                  (t ""))))
                   (if (string-match "@@" sym)
                       (setq sym (concat (substring sym 0 (match-beginning 0))
                                         (substring sym (1- (match-end 0))))))
                   (if (string-equal sym "") nil sym)))
              "^" "\\b"))
 :parse-rule "[$@%]?\\([_a-zA-Z0-9]+\\|[^a-zA-Z]\\)")

(info-lookup-maybe-add-help
 :mode 'python-mode
 ;; Debian includes Python info files, but they're version-named
 ;; instead of having a symlink.
 :doc-spec-function
 (lambda ()
   ;; Python is released annually (PEP 602).
   (let* ((yy (- (decoded-time-year (decode-time (current-time))) 2000))
          (manual (cl-loop for version from yy downto 7
                           for name = (format "python3.%d" version)
                           if (Info-find-file name t)
                           return name
                           finally return "python")))
     `((,(format "(%s)Index" manual))
       (,(format "(%s)Python Module Index" manual))))))

(info-lookup-maybe-add-help
 :mode 'perl-mode
 :regexp "[$@%][^a-zA-Z]\\|\\$\\^[A-Z]\\|[$@%]?[a-zA-Z][_a-zA-Z0-9]*")

(info-lookup-maybe-add-help
 :mode 'latex-mode
 :regexp "\\\\\\([a-zA-Z]+\\|[^a-zA-Z]\\)"
 :doc-spec `((,(if (Info-find-file "latex2e" t)
                   ;; From CTAN's https://ctan.org/pkg/latex2e-help-texinfo
                   ;; and https://puszcza.gnu.org.ua/projects/latexrefman/
		   "(latex2e)Index"
                 ;; From https://savannah.nongnu.org/projects/latex-manual/
		 "(latex-manual)Command Index")
	      ;; \frac{NUM}{DEN} etc can have more than one {xx} argument.
	      ;; \sqrt[ROOT]{num} and others can have square brackets.
	      nil "[`'‘]" "\\({[^}]*}|\\[[^]]*\\]\\)*['’]")))


(info-lookup-maybe-add-help
 :mode 'emacs-lisp-mode
 :regexp "[^][()`'‘’,\" \t\n]+"
 :doc-spec '(;; Commands with key sequences appear in nodes as `foo' and
             ;; those without as `M-x foo'.
             ("(emacs)Command Index"  nil "['`‘]\\(M-x[ \t\n]+\\)?" "['’]")
             ;; Variables normally appear in nodes as just `foo'.
             ("(emacs)Variable Index" nil "['`‘]" "['’]")
             ;; Almost all functions, variables, etc appear in nodes as
             ;; " -- Function: foo" etc.  A small number of aliases and
             ;; symbols appear only as `foo', and will miss out on exact
             ;; positions.  Allowing `foo' would hit too many false matches
             ;; for things that should go to Function: etc, and those latter
             ;; are much more important.  Perhaps this could change if some
             ;; sort of fallback match scheme existed.
             ("(elisp)Index"          nil "^ -+ .*: " "\\( \\|$\\)")
             ("(cl)Function Index"    nil "^ -+ .*: " "\\( \\|$\\)")
             ("(cl)Variable Index"    nil "^ -+ .*: " "\\( \\|$\\)")))

(info-lookup-maybe-add-help
 :mode 'emacs-lisp-only
 :regexp "[^][()`'‘’,\" \t\n]+"
 :doc-spec '(("(elisp)Index"          nil "^ -+ .*: " "\\( \\|$\\)")
             ("(cl)Function Index"    nil "^ -+ .*: " "\\( \\|$\\)")
             ("(cl)Variable Index"    nil "^ -+ .*: " "\\( \\|$\\)")))

(mapc
 (lambda (elem)
   (let* ((prefix (car elem)))
     (info-lookup-add-help
      :mode (cons 'emacs-lisp-mode prefix)
      :regexp (concat "\\b" prefix "-[^][()`'‘’,\" \t\n]+")
      :doc-spec (cl-loop for node in (cdr elem)
                         collect
                         (list (if (string-match-p "^(" node)
                                   node
                                 (format "(%s)%s" prefix node))
                               nil "^ -+ .*: " "\\( \\|$\\)")))))
 ;; Below we have a list of prefixes (used to match on symbols in
 ;; `emacs-lisp-mode') and the nodes where the function/variable
 ;; indices live.  If the prefix is different than the name of the
 ;; manual, then the full "(manual)Node" name has to be used.
 '(("auth" "Function Index" "Variable Index")
   ("autotype" "Command Index" "Variable Index")
   ("calc" "Lisp Function Index" "Variable Index")
   ;;("cc-mode" "Variable Index" "Command and Function Index")
   ("dbus" "Index")
   ("ediff" "Index")
   ("eieio" "Function Index")
   ("gnutls" "(emacs-gnutls)Variable Index" "(emacs-gnutls)Function Index")
   ("mm" "(emacs-mime)Index")
   ("eglot" "Index")
   ("epa" "Variable Index" "Function Index")
   ("ert" "Index")
   ("eshell" "Function and Variable Index")
   ("eudc" "Index")
   ("eww" "Variable Index" "Lisp Function Index")
   ("flymake" "Index")
   ("forms" "Index")
   ("gnus" "Index")
   ("htmlfontify" "Functions" "Variables & Customization")
   ("idlwave" "Index")
   ("ido" "Variable Index" "Function Index")
   ("info" "Index")
   ("mairix" "(mairix-el)Variable Index" "(mairix-el)Function Index")
   ("message" "Index")
   ("mh" "(mh-e)Option Index" "(mh-e)Command Index")
   ("newsticker" "Index")
   ("octave" "(octave-mode)Variable Index" "(octave-mode)Lisp Function Index")
   ("org" "Variable Index" "Command and Function Index")
   ("rcirc" "Variable Index" "Index")
   ("reftex" "Index")
   ("sasl" "Variable Index" "Function Index")
   ("sc" "Variable Index")
   ("semantic" "Index")
   ("ses" "Index")
   ("sieve" "Index")
   ("smtpmail" "Function and Variable Index")
   ("srecode" "Index")
   ("tramp" "Variable Index" "Function Index")
   ("url" "Variable Index" "Function Index")
   ("use-package" "Index")
   ("vhdl" "(vhdl-mode)Variable Index" "(vhdl-mode)Command Index")
   ("viper" "Variable Index" "Function Index")
   ("vtable" "Index")
   ("widget" "Index")
   ("wisent" "Index")
   ("woman" "Variable Index" "Command Index")))

;; docstrings talk about elisp, so have apropos-mode follow emacs-lisp-mode
(info-lookup-maybe-add-help
 :mode 'apropos-mode
 :regexp "[^][()`'‘’,\" \t\n]+" ;; same as emacs-lisp-mode above
 :other-modes '(emacs-lisp-mode))

(info-lookup-maybe-add-help
 :mode 'lisp-interaction-mode
 :regexp "[^][()`'‘’,\" \t\n]+"
 :parse-rule 'ignore
 :other-modes '(emacs-lisp-mode))

(info-lookup-maybe-add-help
 :mode 'inferior-emacs-lisp-mode
 :regexp "[^][()`'‘’,:\" \t\n]+"
 :other-modes '(emacs-lisp-mode))

(info-lookup-maybe-add-help
 :mode 'lisp-mode
 :regexp "[^()`'‘’,\" \t\n]+"
 :parse-rule 'ignore
 :other-modes '(emacs-lisp-mode))

(info-lookup-maybe-add-help
 :mode 'scheme-mode
 :regexp "[^()`'‘’,\" \t\n]+"
 :ignore-case t
 ;; Aubrey Jaffer's rendition from <https://people.csail.mit.edu/jaffer/SCM>
 :doc-spec '(("(r5rs)Index" nil
	      "^[ \t]+-+ [^:]+:[ \t]*" "\\b")))

(info-lookup-maybe-add-help
 :mode 'octave-mode
 :regexp "[_a-zA-Z0-9]+\\|\\s.+\\|[-!=^|*/.\\,><~&+]\\{1,3\\}\\|[][();,\"']"
 :doc-spec `(("(octave)Function Index" nil
              "^ -+ [^:]+:[ ]+\\(\\[[^=]*=[ ]+\\)?" nil)
             ("(octave)Variable Index" nil "^ -+ [^:]+:[ ]+" nil)
             ("(octave)Operator Index" nil nil nil)
             ;; Catch lines of the form "xyz statement"
             ("(octave)Concept Index"
              ,(lambda (item)
                 (cond
                  ((string-match "^\\([A-Z]+\\) statement\\b" item)
                   (match-string 1 item))
                  (t nil)))
              nil; "^ -+ [^:]+:[ ]+" don't think this prefix is useful here.
              nil)))

(info-lookup-maybe-add-help
 :mode 'maxima-mode
 :ignore-case t
 :regexp "[a-zA-Z0-9_%]+"
 :doc-spec '( ("(maxima)Function and Variable Index" nil
	       "^ -+ [^:]+:[ ]+\\(\\[[^=]*=[ ]+\\)?" nil)))

(info-lookup-maybe-add-help
 :mode 'inferior-maxima-mode
 :regexp "[a-zA-Z0-9_%]+"
 :other-modes '(maxima-mode))

;; coreutils and bash builtins overlap in places, eg. printf, so there's a
;; question which should come first.  Some of the coreutils descriptions are
;; more detailed, but if bash is usually /bin/sh on a GNU system then the
;; builtins will be what's normally run.
;;
;; Maybe special variables like $? should be matched as $?, not just ?.
;; This would avoid a clash between variable $! and negation !, or variable
;; $# and comment # (though comment # is not currently indexed in bash).
;; Unfortunately if $? etc is the symbol, then we wouldn't be taken to the
;; exact spot in the relevant node, since the bash manual has just `?' etc
;; there.  Maybe an extension to the prefix/suffix scheme could help this.

(info-lookup-maybe-add-help
 :mode 'sh-mode :topic 'symbol
 ;; bash has "." and ":" in its index, but those chars will probably never
 ;; work in info, so don't bother matching them in the regexp.
 :regexp "\\([a-zA-Z0-9_-]+\\|[!{}@*#?$]\\|\\[\\[?\\|]]?\\)"
 :doc-spec `(("(bash)Builtin Index"       nil "^['`‘]" "[ .'’]")
             ("(bash)Reserved Word Index" nil "^['`‘]" "[ .'’]")
             ("(bash)Variable Index"      nil "^['`‘]" "[ .'’]")

             ;; coreutils (version 4.5.10) doesn't have a separate program
             ;; index, so exclude extraneous stuff (most of it) by demanding
             ;; "[a-z]+" in the trans-func.
             ;; coreutils version 8.1 has node "Concept Index" and past
             ;; versions have node "Index", look for both, whichever is
             ;; absent is quietly ignored
             ("(coreutils)Index"
              ,(lambda (item) (if (string-match "\\`[a-z]+\\'" item) item)))
             ("(coreutils)Concept Index"
              ,(lambda (item) (if (string-match "\\`[a-z]+\\'" item) item)))

             ;; diff (version 2.8.1) has only a few programs, index entries
             ;; are things like "foo invocation".
             ("(diff)Index"
              ,(lambda (item)
                 (if (string-match "\\`\\([a-z]+\\) invocation\\'" item)
                     (match-string 1 item))))
             ;; there's no plain "sed" index entry as such, mung another
             ;; hopefully unique one to get to the invocation section
             ("(sed)Concept Index"
              ,(lambda (item)
                 (if (string-equal item "Standard input, processing as input")
                     "sed")))
             ;; there's no plain "awk" or "gawk" index entries, mung other
             ;; hopefully unique ones to get to the command line options
             ("(gawk)Index"
              ,(lambda (item)
                 (cond ((string-equal item "gawk, extensions, disabling")
                        "awk")
                       ((string-equal item "gawk, versions of, information about, printing")
                        "gawk"))))))

;; This misses some things which occur as node names but not in the
;; index.  Unfortunately it also picks up the wrong one of multiple
;; entries for the same term in some cases.  --fx
(info-lookup-maybe-add-help
 :mode 'cfengine-mode
 :regexp "[[:alnum:]_]+\\(?:()\\)?"
 :doc-spec `(("(cfengine-Reference)Variable Index"
              ,(lambda (item)
                 ;; Index entries may be like `IsPlain()'
                 (if (string-match "\\([[:alnum:]_]+\\)()" item)
                     (match-string 1 item)
                   item))
              ;; This gets functions in evaluated classes.  Other
              ;; possible patterns don't seem to work too well.
              "['`‘]" "(")))

(info-lookup-maybe-add-help
 :mode 'Custom-mode
 :ignore-case t
 :regexp "[^][()`'‘’,:\" \t\n]+"
 :parse-rule (lambda ()
               (when-let* ((symbol (get-text-property (point) 'custom-data)))
                 (symbol-name symbol)))
 :other-modes '(emacs-lisp-mode))

(info-lookup-maybe-add-help
 :mode 'help-mode
 :regexp "[^][()`'‘’,:\" \t\n]+"
 :other-modes '(emacs-lisp-mode))

(info-lookup-maybe-add-help
 :mode 'gdb-script-mode
 :ignore-case nil
 :regexp "\\([-a-z]+\\(\\s-+[-a-z]+\\)*\\)"
 :doc-spec '(("(gdb)Command and Variable Index" nil
              nil nil))
 :parse-rule 'info-lookup-guess-gdb-script-symbol)

(provide 'info-look)

;;; info-look.el ends here
