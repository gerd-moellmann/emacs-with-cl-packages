;;; minibuf-eldef.el --- Only show defaults in prompts when applicable  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2000-2025 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: convenience

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
;; Defines the mode `minibuffer-electric-default-mode'.
;;
;; When active, minibuffer prompts that show a default value only show
;; the default when it's applicable -- that is, when hitting RET would
;; yield the default value.  If the user modifies the input such that
;; hitting RET would enter a non-default value, the prompt is modified
;; to remove the default indication (which is restored if the input is
;; ever restore to the match the initial input).

;;; Code:

(defvar minibuffer-eldef-shorten-default)

(defun minibuffer-default--in-prompt-regexps ()
  (cons
   (list
    (concat
     "\\("
     (if (string-match "%s" minibuffer-default-prompt-format)
         (concat
          (regexp-quote (substring minibuffer-default-prompt-format
                                   0 (match-beginning 0)))
          "\\(.*?\\)"
          (regexp-quote (substring minibuffer-default-prompt-format
                                   (match-end 0))))
       (regexp-quote minibuffer-default-prompt-format))
     "\\): ")
    1 (and minibuffer-eldef-shorten-default " [\\2]"))
   `(("\\( (default\\(?: is\\)? \\(.*\\))\\):? \\'"
      1 ,(if minibuffer-eldef-shorten-default " [\\2]"))
     ("([^(]+?\\(, default\\(?: is\\)? \\(.*\\)\\)):? \\'" 1)
     ("\\( \\[.*\\]\\):? *\\'" 1))))

(defcustom minibuffer-eldef-shorten-default nil
  "If non-nil, shorten \"(default ...)\" to \"[...]\" in minibuffer prompts."
  :set (lambda (symbol value)
         (set-default symbol value)
	 (setq-default minibuffer-default-in-prompt-regexps
		       (minibuffer-default--in-prompt-regexps)))
  :type 'boolean
  :group 'minibuffer
  :version "24.3")
(make-obsolete-variable 'minibuffer-eldef-shorten-default
                        'minibuffer-default-prompt-format "29.1")

(defvar minibuffer-default-in-prompt-regexps
  (minibuffer-default--in-prompt-regexps)
  "A list of regexps matching the parts of minibuffer prompts showing defaults.
When `minibuffer-electric-default-mode' is active, these regexps are
used to identify the portions of prompts to elide.

Each entry is of the form (REGEXP MATCH-NUM &optional REWRITE),
where REGEXP should match the default part of the prompt,
MATCH-NUM is the subgroup that matched the actual default indicator,
and REWRITE, if present, is a string to pass to `replace-match' that
should be displayed in its place.")


;;; Internal variables

;; A list of minibuffers to which we've added a post-command-hook.
(defvar minibuf-eldef-frobbed-minibufs nil)

;;; The following are all local variables in the minibuffer

;; Input pre-inserted into the minibuffer before the user can edit it.
(defvar-local minibuf-eldef-initial-input)

;; and the length of the buffer with it inserted.
(defvar-local minibuf-eldef-initial-buffer-length)

;; True if the current minibuffer prompt contains the default spec.
(defvar-local minibuf-eldef-showing-default-in-prompt)

;; An overlay covering the default portion of the prompt
(defvar-local minibuf-eldef-overlay)


;;; Hook functions

;; This function goes on minibuffer-setup-hook
(defun minibuf-eldef-setup-minibuffer ()
  "Set up a minibuffer for `minibuffer-electric-default-mode'.
The prompt and initial input should already have been inserted."
  (let ((regexps minibuffer-default-in-prompt-regexps)
	(match nil))
    (save-excursion
      (save-restriction
	;; Narrow to only the prompt.
	(goto-char (point-min))
	(narrow-to-region (point) (minibuffer-prompt-end))
	;; See if the prompt contains a default input indicator.
	(while regexps
	  (setq match (pop regexps))
	  (cond
           ((not (re-search-forward (if (stringp match) match (car match))
                                    nil t))
            ;; No match yet, try the next rule.
	    (setq match nil))
           ((and (consp (cdr-safe match)) (nth 2 match))
            ;; Matched a replacement rule.
            (let* ((inhibit-read-only t)
                   (buffer-undo-list t)
                   (submatch (nth 1 match))
                   (replacement (nth 2 match))
                   (props (text-properties-at (match-beginning submatch))))
              (replace-match replacement nil nil nil submatch)
              (set-text-properties (match-beginning submatch)
                                   (match-end submatch)
                                   props)
              ;; Replacement done, now keep trying with subsequent rules.
              (setq match nil)
              (goto-char (point-min))))
           ;; Matched a non-replacement (i.e. electric hide) rule, no need to
           ;; keep trying.
           (t (setq regexps nil))))))
    (if (not match)
	;; No match for electric hiding, so just make sure our
	;; post-command-hook isn't left around.
	(remove-hook 'post-command-hook #'minibuf-eldef-update-minibuffer t)
      ;; Yup; set things up so we can frob the prompt as the state of
      ;; the input string changes.
      (setq match (if (consp match) (cdr match) 0))
      (setq match (if (consp match) (car match) match))
      (setq minibuf-eldef-overlay
	    (make-overlay (match-beginning match) (match-end match)))
      (setq minibuf-eldef-showing-default-in-prompt t)
      (setq minibuf-eldef-initial-input
	    (minibuffer-contents-no-properties))
      (setq minibuf-eldef-initial-buffer-length (point-max))
      (add-to-list 'minibuf-eldef-frobbed-minibufs (current-buffer))
      (add-hook 'post-command-hook #'minibuf-eldef-update-minibuffer nil t))))

;; post-command-hook to swap prompts when necessary
(defun minibuf-eldef-update-minibuffer ()
  "Update a minibuffer's prompt to include a default only when applicable.
This is intended to be used as a minibuffer `post-command-hook' for
`minibuffer-electric-default-mode'; the minibuffer should have already
been set up by `minibuf-eldef-setup-minibuffer'."
  (unless (eq minibuf-eldef-showing-default-in-prompt
	      (and (= (point-max) minibuf-eldef-initial-buffer-length)
		   (string-equal (minibuffer-contents-no-properties)
				 minibuf-eldef-initial-input)))
    ;; Swap state.
    (setq minibuf-eldef-showing-default-in-prompt
	  (not minibuf-eldef-showing-default-in-prompt))
    (overlay-put minibuf-eldef-overlay 'invisible
                 (not minibuf-eldef-showing-default-in-prompt))))


;;;###autoload
(define-minor-mode minibuffer-electric-default-mode
  "Toggle Minibuffer Electric Default mode.

Minibuffer Electric Default mode is a global minor mode.  When
enabled, minibuffer prompts that show a default value only show
the default when it's applicable -- that is, when hitting RET
would yield the default value.  If the user modifies the input
such that hitting RET would enter a non-default value, the prompt
is modified to remove the default indication."
  :global t
  :group 'minibuffer
  (if minibuffer-electric-default-mode
      ;; Enable the mode
      (add-hook 'minibuffer-setup-hook 'minibuf-eldef-setup-minibuffer)
    ;; Disable the mode
    (remove-hook 'minibuffer-setup-hook 'minibuf-eldef-setup-minibuffer)
    ;; Remove our entry from any post-command-hook variable's it's still in
    (dolist (minibuf minibuf-eldef-frobbed-minibufs)
      (with-current-buffer minibuf
	(remove-hook 'post-command-hook #'minibuf-eldef-update-minibuffer t)))
    (setq minibuf-eldef-frobbed-minibufs nil)))


(provide 'minibuf-eldef)

;;; minibuf-eldef.el ends here
