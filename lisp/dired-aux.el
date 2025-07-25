;;; dired-aux.el --- less commonly used parts of dired -*- lexical-binding: t -*-

;; Copyright (C) 1985-2025 Free Software Foundation, Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>.
;; Maintainer: emacs-devel@gnu.org
;; Keywords: files
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

;; The parts of Dired mode not normally used.  This is a space-saving hack
;; to avoid having to load a large mode when all that's wanted are a few
;; functions.

;; Rewritten in 1990/1991 to add tree features, file marking and
;; sorting by Sebastian Kremer <sk@thp.uni-koeln.de>.
;; Finished up by rms in 1992.


;;; Code:

(require 'cl-lib)
(require 'dired)

(defvar dired-create-files-failures nil
  "Variable where `dired-create-files' records failing file names.
Functions that operate recursively can store additional names
into this list; they also should call `dired-log' to log the errors.")


;;; Diffing and compressing

(defconst dired-star-subst-regexp "\\(^\\|[ \t]\\)\\*\\([ \t]\\|$\\)")
(defconst dired-quark-subst-regexp "\\(^\\|[ \t]\\)\\?\\([ \t]\\|$\\)")
(make-obsolete-variable 'dired-star-subst-regexp nil "26.1")
(make-obsolete-variable 'dired-quark-subst-regexp nil "26.1")

(defun dired-isolated-string-re (string)
  "Return a regexp to match STRING isolated.
Isolated means that STRING is surrounded by spaces or at the beginning/end
of a string followed/prefixed with an space.
The regexp capture the preceding blank, STRING and the following blank as
the groups 1, 2 and 3 respectively."
  (format "\\(?1:\\`\\|[ \t]\\)\\(?2:%s\\)\\(?3:[ \t]\\|\\'\\)" string))

(defun dired--star-or-qmark-p (string match &optional keep start)
  "Return non-nil if STRING contains isolated MATCH or `\\=`?\\=`'.
MATCH should be the strings \"?\", `\\=`?\\=`', \"*\" or nil.  The latter
means STRING contains either \"?\" or `\\=`?\\=`' or \"*\".
If optional arg KEEP is non-nil, then preserve the match data.  Otherwise,
this function changes it and saves MATCH as the second match group.
START is the position to start matching from.

Isolated means that MATCH is surrounded by spaces or at the beginning/end
of STRING followed/prefixed with an space.  A match to `\\=`?\\=`',
isolated or not, is also valid."
  (let ((regexp (dired-isolated-string-re (if match (regexp-quote match) "[*?]"))))
    (when (or (null match) (equal match "?"))
      (cl-callf concat regexp "\\|\\(?1:\\)\\(?2:`\\?`\\)\\(?3:\\)"))
    (funcall (if keep #'string-match-p #'string-match) regexp string start)))

(defun dired--need-confirm-positions (command string)
  "Search for non-isolated matches of STRING in COMMAND.
Return a list of positions that match STRING, but would not be
considered \"isolated\" by `dired--star-or-qmark-p'."
  (cl-assert (= (length string) 1))
  (let ((start 0)
        (isolated-char-positions nil)
        (confirm-positions nil)
        (regexp (regexp-quote string)))
    ;; Collect all ? and * surrounded by spaces and ? .
    (while (dired--star-or-qmark-p command string nil start)
      (push (cons (match-beginning 2) (match-end 2))
            isolated-char-positions)
      (setq start (match-end 2)))
    ;; Now collect any remaining ? and *.
    (setq start 0)
    (while (string-match regexp command start)
      (unless (cl-member (match-beginning 0) isolated-char-positions
                         :test (lambda (pos match)
                                 (<= (car match) pos (cdr match))))
        (push (match-beginning 0) confirm-positions))
      (setq start (match-end 0)))
    confirm-positions))

(defun dired--mark-positions (positions)
  (let ((markers (make-string
                  (1+ (apply #'max positions))
                  ?\s)))
    (dolist (pos positions)
      (setf (aref markers pos) ?^))
    markers))

(defun dired--highlight-no-subst-chars (positions command mark)
  (cl-callf substring-no-properties command)
  (dolist (pos positions)
    (add-face-text-property pos (1+ pos) 'warning nil command))
  (if mark
      (concat command "\n" (dired--mark-positions positions))
    command))

(defun dired--no-subst-explain (buf char-positions command mark-positions)
  (with-current-buffer buf
    (erase-buffer)
    (insert
     (format-message "\
If your command contains occurrences of `*' surrounded by
whitespace, `dired-do-shell-command' substitutes them for the
entire file list to process.  Otherwise, if your command contains
occurrences of `?' surrounded by whitespace or `%s', Dired will
run the command once for each file, substituting `?' for each
file name.

Your command contains occurrences of `%s' that will not be
substituted, and will be passed through normally to the shell.

%s

(Press ^ to %s markers below these occurrences.)
"
   "`"
   (string (aref command (car char-positions)))
   (dired--highlight-no-subst-chars char-positions command mark-positions)
   (if mark-positions "remove" "add")))))

(defun dired--no-subst-ask (char nb-occur details)
  (let ((hilit-char (propertize (string char) 'face 'warning))
        (choices `(?y ?n ?? ,@(when details '(?^)))))
    (read-char-choice
     (format-message
      (ngettext
       "%d occurrence of `%s' will not be substituted.  Proceed? (%s) "
       "%d occurrences of `%s' will not be substituted.  Proceed? (%s) "
       nb-occur)
      nb-occur hilit-char (mapconcat #'string choices ", "))
     choices)))

(defun dired--no-subst-confirm (char-positions command)
  (let ((help-buf (get-buffer-create "*Dired help*"))
        (char (aref command (car char-positions)))
        (nb-occur (length char-positions))
        (done nil)
        (details nil)
        (markers nil)
        proceed)
    (unwind-protect
        (save-window-excursion
          (while (not done)
            (cl-case (dired--no-subst-ask char nb-occur details)
              (?y
               (setq done t
                     proceed t))
              (?n
               (setq done t
                     proceed nil))
              (??
               (if details
                   (progn
                     (quit-window nil details)
                     (setq details nil))
                 (dired--no-subst-explain
                  help-buf char-positions command markers)
                 (setq details (display-buffer help-buf))))
              (?^
               (setq markers (not markers))
               (dired--no-subst-explain
                help-buf char-positions command markers)))))
      (kill-buffer help-buf))
    proceed))

;;;###autoload
(defun dired-diff (file &optional switches)
  "Compare file at point with FILE using `diff'.
If called interactively, prompt for FILE.
If the mark is active in Transient Mark mode, use the file at the mark
as the default for FILE.  (That's the mark set by \\[set-mark-command],
not by Dired's \\[dired-mark] command.)
If the file at point has a backup file, use that as the default FILE.
If the file at point is a backup file, use its original, if that exists
and can be found.  Note that customizations of `backup-directory-alist'
and `make-backup-file-name-function' change where this function searches
for the backup file, and affect its ability to find the original of a
backup file.

FILE is the first argument given to the `diff' function.  The file at
point is the second argument given to `diff'.

With prefix arg, prompt for second argument SWITCHES, which is
the string of command switches used as the third argument of `diff'."
  (interactive
   (let* ((current (dired-get-filename t))
	  ;; Get the latest existing backup file or its original.
	  (oldf (if (backup-file-name-p current)
		    (file-name-sans-versions current)
		  (diff-latest-backup-file current)))
	  ;; Get the file at the mark.
	  (file-at-mark (if (and transient-mark-mode mark-active)
			    (save-excursion (goto-char (mark t))
					    (dired-get-filename t t))))
          (separate-dir (and oldf
                             (not (equal (file-name-directory oldf)
                                         (dired-current-directory)))))
	  (default-file (or file-at-mark
                            ;; If the file with which to compare
                            ;; doesn't exist, or we cannot intuit it,
                            ;; we forget that name and don't show it
                            ;; as the default, as an indication to the
                            ;; user that she should type the file
                            ;; name.
			    (and (if (and oldf (file-readable-p oldf)) oldf)
                                 (if separate-dir
                                     oldf
                                   (file-name-nondirectory oldf)))))
	  ;; Use it as default if it's not the same as the current file,
	  ;; and the target dir is current or there is a default file.
	  (default (if (and (not (equal default-file current))
			    (or (equal (dired-dwim-target-directory)
				       (dired-current-directory))
				default-file))
		       default-file))
	  (target-dir (if default
                          (if separate-dir
                              (file-name-directory default)
                            (dired-current-directory))
			(dired-dwim-target-directory)))
	  (defaults (append
                     (if (backup-file-name-p current)
                         ;; This is a backup file -- put the other
                         ;; main file, and the other backup files into
                         ;; the `M-n' list.
                         (delete (expand-file-name current)
                                 (cons (expand-file-name
                                        (file-name-sans-versions current))
                                       (file-backup-file-names
                                        (file-name-sans-versions current))))
                       ;; Non-backup file -- use the backup files as
                       ;; `M-n' candidates.
                       (file-backup-file-names current))
                     (dired-dwim-target-defaults (list current) target-dir))))
     (list
      (minibuffer-with-setup-hook
	  (lambda ()
            (setq-local minibuffer-default-add-function nil)
	    (setq minibuffer-default defaults))
	(read-file-name (format-prompt "Diff %s with" default current)
	                target-dir default t))
      (if current-prefix-arg
	  (read-string "Options for diff: "
		       (if (stringp diff-switches)
			   diff-switches
			 (mapconcat #'identity diff-switches " "))))))
   dired-mode)
  (let ((current (dired-get-filename t)))
    (when (or (equal (expand-file-name file)
		     (expand-file-name current))
	      (and (file-directory-p file)
		   (equal (expand-file-name current file)
			  (expand-file-name current))))
      (error "Attempt to compare the file to itself"))
    (if (and (backup-file-name-p current)
	     (equal file (file-name-sans-versions current)))
	(diff current file switches)
      (diff file current switches))))

;;;###autoload
(defun dired-backup-diff (&optional switches)
  "Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for argument SWITCHES which is options for `diff'."
  (interactive
   (if current-prefix-arg
       (list (read-string "Options for diff: "
			  (if (stringp diff-switches)
			      diff-switches
			    (mapconcat #'identity diff-switches " "))))
     nil)
   dired-mode)
  (diff-backup (dired-get-filename) switches))

;;;###autoload
(defun dired-compare-directories (dir2 predicate)
  "Mark files with different file attributes in two Dired buffers.
Compare file attributes of files in the current directory
with file attributes in directory DIR2 using PREDICATE on pairs of files
with the same name.  Mark files for which PREDICATE returns non-nil.
Mark files with different names if PREDICATE is nil (or interactively
with empty input at the predicate prompt).

PREDICATE is a Lisp expression that can refer to the following variables:

    size1, size2   - file size in bytes
    mtime1, mtime2 - last modification time in seconds, as a float
    fa1, fa2       - list of file attributes
                     returned by function `file-attributes'

    where 1 refers to attribute of file in the current Dired buffer
    and 2 to attribute of file in second Dired buffer.

Examples of PREDICATE:

    (> mtime1 mtime2) - mark newer files
    (not (= size1 size2)) - mark files with different sizes
    (not (string= (file-attribute-modes fa1)  - mark files with different modes
                  (file-attribute-modes fa2)))
    (not (and (= (file-attribute-user-id fa1) - mark files with different UID
                 (file-attribute-user-id fa2))
              (= (file-attribute-group-id fa1) - and GID.
                 (file-attribute-group-id fa2))))

If the region is active in Transient Mark mode, mark files
only in the active region if `dired-mark-region' is non-nil."
  (interactive
   (list
    (let* ((target-dir (dired-dwim-target-directory))
	   (defaults (dired-dwim-target-defaults nil target-dir)))
      (minibuffer-with-setup-hook
	  (lambda ()
            (setq-local minibuffer-default-add-function nil)
	    (setq minibuffer-default defaults))
	(read-directory-name (format "Compare %s with: "
				     (dired-current-directory))
			     target-dir target-dir t)))
    (read-from-minibuffer "Mark if (lisp expr or RET): " nil nil t nil "nil"))
   dired-mode)
  (let* ((dir1 (dired-current-directory))
         (file-alist1 (dired-files-attributes dir1))
         (file-alist2 (dired-files-attributes dir2))
	 file-list1 file-list2)
    (setq file-alist1 (delq (assoc "." file-alist1) file-alist1))
    (setq file-alist1 (delq (assoc ".." file-alist1) file-alist1))
    (setq file-alist2 (delq (assoc "." file-alist2) file-alist2))
    (setq file-alist2 (delq (assoc ".." file-alist2) file-alist2))
    (setq file-list1 (mapcar
		      #'cadr
                      (dired-file-set-difference
                       file-alist1 file-alist2
		       predicate))
	  file-list2 (mapcar
		      #'cadr
                      (dired-file-set-difference
                       file-alist2 file-alist1
		       predicate)))
    (dired-fun-in-all-buffers
     dir1 nil
     (lambda ()
       (dired-mark-if
        (member (dired-get-filename nil t) file-list1) nil)))
    (dired-fun-in-all-buffers
     dir2 nil
     (lambda ()
       (dired-mark-if
        (member (dired-get-filename nil t) file-list2) nil)))
    (message "Marked in dir1: %s, in dir2: %s"
             (format-message (ngettext "%d file" "%d files" (length file-list1))
                             (length file-list1))
             (format-message (ngettext "%d file" "%d files" (length file-list2))
                             (length file-list2)))))

(defun dired-file-set-difference (list1 list2 predicate)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all file items that appear in LIST1 but not LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
PREDICATE (see `dired-compare-directories') is an additional match
condition.  Two file items are considered to match if they are equal
*and* PREDICATE evaluates to t."
  (if (or (null list1) (null list2))
      list1
    (let (res)
      (dolist (file1 list1)
	(unless (let ((list list2))
		  (while (and list
			      (let* ((file2 (car list))
                                     (fa1 (car (cddr file1)))
                                     (fa2 (car (cddr file2))))
                                (or
                                 (not (equal (car file1) (car file2)))
                                 (eval predicate
                                       `((fa1 . ,fa1)
                                         (fa2 . ,fa2)
                                         (size1 . ,(file-attribute-size fa1))
                                         (size2 . ,(file-attribute-size fa2))
                                         (mtime1
                                          . ,(float-time (file-attribute-modification-time fa1)))
                                         (mtime2
                                          . ,(float-time (file-attribute-modification-time fa2)))
                                         )))))
		    (setq list (cdr list)))
		  list)
	  (push file1 res)))
      (nreverse res))))

(defun dired-files-attributes (dir)
  "Return a list of all file names and attributes from DIR.
List has a form of (file-name full-file-name (attribute-list))."
  (mapcar
   (lambda (file-name)
     (let ((full-file-name (expand-file-name file-name dir)))
       (list file-name
             full-file-name
             (file-attributes full-file-name))))
   (directory-files dir)))


;;; Change file attributes

(defun dired-do-chxxx (attribute-name program op-symbol arg)
  ;; Change file attributes (group, owner, timestamp) of marked files and
  ;; refresh their file lines.
  ;; ATTRIBUTE-NAME is a string describing the attribute to the user.
  ;; PROGRAM is the program used to change the attribute.
  ;; OP-SYMBOL is the type of operation (for use in `dired-mark-pop-up').
  ;; ARG describes which files to use, as in `dired-get-marked-files'.
  (let* ((files (dired-get-marked-files t arg nil nil t))
	 ;; The source of default file attributes is the file at point.
	 (default-file (dired-get-filename t t))
	 (default (when default-file
		    (cond ((eq op-symbol 'touch)
			   (format-time-string
			    "%Y%m%d%H%M.%S"
			    (file-attribute-modification-time
			     (file-attributes default-file))))
			  ((eq op-symbol 'chown)
			   (file-attribute-user-id
			    (file-attributes default-file 'string)))
			  ((eq op-symbol 'chgrp)
			   (file-attribute-group-id
			    (file-attributes default-file 'string))))))
         (prompt (format-prompt "Change %s of %%s to"
                                (when (eq op-symbol 'touch)
                                    "now")
                                attribute-name))
	 (new-attribute (dired-mark-read-string prompt nil op-symbol
						arg files default
						(cond ((eq op-symbol 'chown)
						       (system-users))
						      ((eq op-symbol 'chgrp)
						       (system-groups)))))
	 (operation (concat program " " new-attribute))
         ;; When file-name-coding-system is set to something different
         ;; from locale-coding-system, leaving the encoding
         ;; determination to call-process will do the wrong thing,
         ;; because the arguments in this case are file names, not
         ;; just some arbitrary text.  (This must be bound last, to
         ;; avoid adverse effects on any of the preceding forms.)
         (coding-system-for-write (or file-name-coding-system
                                      default-file-name-coding-system))
	 failures)
    (setq failures
	  (dired-bunch-files 10000
			     #'dired-check-process
			     (append
			      (list operation program)
			      (unless (or (string-equal new-attribute "")
					  ;; Use `eq' instead of `equal'
					  ;; to detect empty input (bug#12399).
					  (eq new-attribute default))
				(if (eq op-symbol 'touch)
				    (list "-t" new-attribute)
				  (list new-attribute)))
			      (if (string-match-p "gnu" system-configuration)
				  '("--") nil))
			     files))
    (dired-do-redisplay arg);; moves point if ARG is an integer
    (if failures
	(dired-log-summary
	 (format "%s: error" operation)
	 nil)))
  (dired-post-do-command))

;;;###autoload
(defun dired-do-chmod (&optional arg)
  "Change the mode of the marked (or next ARG) files.
Both octal numeric modes like `644' and symbolic modes like `g+w'
are supported.
After invoking the command, \
type \\<minibuffer-local-completion-map>\\[next-history-element] \
to pull the file attributes
of the file at point into the minibuffer.

See Info node `(coreutils)File permissions' for more information.
Alternatively, see the man page for \"chmod(1)\".

Note that on MS-Windows only the `w' (write) bit is meaningful:
resetting it makes the file read-only.  Changing any other bit
has no effect on MS-Windows."
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
	 ;; The source of default file attributes is the file at point.
	 (default-file (dired-get-filename t t))
	 (modestr (when default-file
		    (file-attribute-modes (file-attributes default-file))))
	 (default
	   (and (stringp modestr)
		(string-match "^.\\(...\\)\\(...\\)\\(...\\)$" modestr)
		(string-replace
		 "-" ""
		 (format "u=%s,g=%s,o=%s"
			 (match-string 1 modestr)
			 (match-string 2 modestr)
			 (match-string 3 modestr)))))
	 (modes (dired-mark-read-string
		 "Change mode of %s to: "
		 nil 'chmod arg files default))
	 num-modes)
    (cond ((or (equal modes "")
	       ;; Use `eq' instead of `equal'
	       ;; to detect empty input (bug#12399).
	       (eq modes default))
	   ;; We used to treat empty input as DEFAULT, but that is not
	   ;; such a good idea (Bug#9361).
	   (error "No file mode specified"))
	  ((string-match-p "^[0-7]+" modes)
	   (setq num-modes (string-to-number modes 8))))

    (dolist (file files)
      (set-file-modes
       file
       (if num-modes num-modes
	 (file-modes-symbolic-to-number modes (file-modes file 'nofollow)))
       'nofollow))
    (dired-do-redisplay arg))
  (dired-post-do-command))

;;;###autoload
(defun dired-do-chgrp (&optional arg)
  "Change the group of the marked (or next ARG) files.
After invoking the command, \
type \\<minibuffer-local-completion-map>\\[next-history-element] \
to pull the file attributes
of the file at point into the minibuffer."
  (interactive "P" dired-mode)
  (if (and (memq system-type '(ms-dos windows-nt))
           (not (file-remote-p default-directory)))
      (error "chgrp not supported on this system"))
  (dired-do-chxxx "Group" "chgrp" 'chgrp arg))

;;;###autoload
(defun dired-do-chown (&optional arg)
  "Change the owner of the marked (or next ARG) files.
After invoking the command, \
type \\<minibuffer-local-completion-map>\\[next-history-element] \
to pull the file attributes
of the file at point into the minibuffer."
  (interactive "P" dired-mode)
  (if (and (memq system-type '(ms-dos windows-nt))
           (not (file-remote-p default-directory)))
      (error "chown not supported on this system"))
  (dired-do-chxxx "Owner" dired-chown-program 'chown arg))

;;;###autoload
(defun dired-do-touch (&optional arg)
  "Change the timestamp of the marked (or next ARG) files.
This calls touch.
After invoking the command, \
type \\<minibuffer-local-completion-map>\\[next-history-element] \
to pull the file attributes
of the file at point into the minibuffer."
  (interactive "P" dired-mode)
  (dired-do-chxxx "Timestamp" dired-touch-program 'touch arg))

;; Process all the files in FILES in batches of a convenient size,
;; by means of (FUNCALL FUNCTION ARGS... SOME-FILES...).
;; Batches are chosen to need less than MAX chars for the file names,
;; allowing 3 extra characters of separator per file name.
(defun dired-bunch-files (max function args files)
  (let (pending
	past
	(pending-length 0)
	failures)
    ;; Accumulate files as long as they fit in MAX chars,
    ;; then process the ones accumulated so far.
    (while files
      (let* ((thisfile (car files))
	     (thislength (+ (length thisfile) 3))
	     (rest (cdr files)))
	;; If we have at least 1 pending file
	;; and this file won't fit in the length limit, process now.
	(if (and pending (> (+ thislength pending-length) max))
	    (setq pending (nreverse pending)
		  ;; The elements of PENDING are now in forward order.
		  ;; Do the operation and record failures.
		  failures (nconc (apply function (append args pending))
				  failures)
		  ;; Transfer the elements of PENDING onto PAST
		  ;; and clear it out.  Now PAST contains the first N files
		  ;; specified (for some N), and FILES contains the rest.
		  past (nconc past pending)
		  pending nil
		  pending-length 0))
	;; Do (setq pending (cons thisfile pending))
	;; but reuse the cons that was in `files'.
	(setcdr files pending)
	(setq pending files)
	(setq pending-length (+ thislength pending-length))
	(setq files rest)))
    (setq pending (nreverse pending))
    (prog1
	(nconc (apply function (append args pending))
	       failures)
      ;; Now the original list FILES has been put back as it was.
      (nconc past pending))))

(defvar lpr-printer-switch)

;;;###autoload
(defun dired-do-print (&optional arg)
  "Print the marked (or next ARG) files.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default."
  (interactive "P" dired-mode)
  (require 'lpr)
  (let* ((file-list (dired-get-marked-files t arg nil nil t))
	 (lpr-switches
	  (if (and (stringp printer-name)
		   (string< "" printer-name))
	      (cons (concat lpr-printer-switch printer-name)
		    lpr-switches)
	    lpr-switches))
	 (command (dired-mark-read-string
		   "Print %s with: "
                   (mapconcat #'identity
			      (cons lpr-command
				    (if (stringp lpr-switches)
					(list lpr-switches)
				      lpr-switches))
			      " ")
		   'print arg file-list)))
    (dired-run-shell-command (dired-shell-stuff-it command file-list nil)))
  (dired-post-do-command))

(defun dired-mark-read-string (prompt initial op-symbol arg files
			              &optional default-value collection)
  "Read args for a Dired marked-files command, prompting with PROMPT.
Return the user input (a string).

INITIAL, if non-nil, is the initial minibuffer input.
OP-SYMBOL is an operation symbol (see `dired-no-confirm').
ARG is normally the prefix argument for the calling command;
it is passed as the first argument to `dired-mark-prompt'.
FILES should be a list of marked files' names.

Optional arg DEFAULT-VALUE is a default value or list of default
values, passed as the seventh arg to `completing-read'.

Optional arg COLLECTION is a collection of possible completions,
passed as the second arg to `completing-read'."
  (apply #'dired-mark-pop-up
         nil op-symbol files
         (if (eq op-symbol 'touch) 'read-from-minibuffer 'completing-read)
         (format prompt (dired-mark-prompt arg files))
         (if (eq op-symbol 'touch)
             `(,initial nil nil nil ,default-value)
           `(,collection nil nil ,initial nil ,default-value nil))))


;;; Cleaning a directory: flagging some backups for deletion

(defvar dired-file-version-alist)

;;;###autoload
(defun dired-clean-directory (keep)
  "Flag numerical backups for deletion.
Spares `dired-kept-versions' latest versions, and `kept-old-versions' oldest.
Positive prefix arg KEEP overrides `dired-kept-versions';
Negative prefix arg KEEP overrides `kept-old-versions' with KEEP made positive.

To clear the flags on these files, you can use \\[dired-flag-backup-files]
with a prefix argument."
  (interactive "P" dired-mode)
  (setq keep (if keep (prefix-numeric-value keep) dired-kept-versions))
  (let ((early-retention (if (< keep 0) (- keep) kept-old-versions))
	(late-retention (if (<= keep 0) dired-kept-versions keep))
	(dired-file-version-alist ()))
    (message "Cleaning numerical backups (keeping %d late, %d old)..."
	     late-retention early-retention)
    ;; Look at each file.
    ;; If the file has numeric backup versions,
    ;; put on dired-file-version-alist an element of the form
    ;; (FILENAME . VERSION-NUMBER-LIST)
    (dired-map-dired-file-lines #'dired-collect-file-versions)
    ;; Sort each VERSION-NUMBER-LIST,
    ;; and remove the versions not to be deleted.
    (let ((fval dired-file-version-alist))
      (while fval
	(let* ((sorted-v-list (cons 'q (sort (cdr (car fval)) '<)))
	       (v-count (length sorted-v-list)))
	  (if (> v-count (+ early-retention late-retention))
	      (rplacd (nthcdr early-retention sorted-v-list)
		      (nthcdr (- v-count late-retention)
			      sorted-v-list)))
	  (rplacd (car fval)
		  (cdr sorted-v-list)))
	(setq fval (cdr fval))))
    ;; Look at each file.  If it is a numeric backup file,
    ;; find it in a VERSION-NUMBER-LIST and maybe flag it for deletion.
    (dired-map-dired-file-lines #'dired-trample-file-versions)
    (message "Cleaning numerical backups...done")))


;;; Subroutines of dired-clean-directory

(defun dired-map-dired-file-lines (fun)
  ;; Perform FUN with point at the end of each non-directory line.
  ;; FUN takes one argument, the absolute filename.
  (save-excursion
    (let (file buffer-read-only)
      (goto-char (point-min))
      (while (not (eobp))
	(save-excursion
	  (and (not (looking-at-p dired-re-dir))
	       (not (eolp))
	       (setq file (dired-get-filename nil t)) ; nil on non-file
	       (progn (end-of-line)
		      (funcall fun file))))
	(forward-line 1)))))

(defvar backup-extract-version-start)  ; used in backup-extract-version

(defun dired-collect-file-versions (fn)
  (let ((fn (file-name-sans-versions fn)))
    ;; Only do work if this file is not already in the alist.
    (if (assoc fn dired-file-version-alist)
	nil
      ;; If it looks like file FN has versions, return a list of the versions.
      ;;That is a list of strings which are file names.
      ;;The caller may want to flag some of these files for deletion.
      (let* ((base-versions
	      (concat (file-name-nondirectory fn) ".~"))
	     (backup-extract-version-start (length base-versions))
	     (possibilities (file-name-all-completions
			     base-versions
			     (file-name-directory fn)))
	     (versions (mapcar #'backup-extract-version possibilities)))
	(if versions
	    (setq dired-file-version-alist
		  (cons (cons fn versions)
			dired-file-version-alist)))))))

(defun dired-trample-file-versions (fn)
  (let* ((start-vn (string-match-p "\\.~[0-9]+~$" fn))
	 base-version-list)
    (and start-vn
	 (setq base-version-list	; there was a base version to which
	       (assoc (substring fn 0 start-vn)	; this looks like a
		      dired-file-version-alist))	; subversion
	 (not (memq (string-to-number (substring fn (+ 2 start-vn)))
		    base-version-list))	; this one doesn't make the cut
	 (progn (beginning-of-line)
		(delete-char 1)
		(insert dired-del-marker)))))


;;; Shell commands

;; This is an extra function so that you can redefine it, e.g., to use gmhist.
(defun dired-read-shell-command (prompt arg files)
  "Read a Dired shell command.
PROMPT should be a format string with one \"%s\" format sequence,
which is replaced by the value returned by `dired-mark-prompt',
with ARG and FILES as its arguments.  FILES should be a list of
file names.  The result is used as the prompt.

Use `dired-guess-shell-command' to offer a smarter default choice
of shell command."
  (setq prompt (format prompt (dired-mark-prompt arg files)))
  (dired-mark-pop-up nil 'shell files
                     'dired-guess-shell-command prompt files))

;;;###autoload
(defcustom dired-confirm-shell-command t
  "Whether `dired-do-shell-command' should prompt for confirmation.
If non-nil, prompt for confirmation if the shell command contains
potentially unsafe wildcard characters.  If nil, never prompt for
confirmation."
  :type 'boolean
  :group 'dired
  :version "29.1")

;;;###autoload
(defun dired-do-async-shell-command (command &optional arg file-list)
  "Run a shell command COMMAND on the marked files asynchronously.

Like `dired-do-shell-command', but adds `&' at the end of COMMAND
to execute it asynchronously.

When operating on multiple files, asynchronous commands
are executed in the background on each file in parallel.
In shell syntax this means separating the individual commands
with `&'.  However, when COMMAND ends in `;' or `;&' then commands
are executed in the background on each file sequentially waiting
for each command to terminate before running the next command.
In shell syntax this means separating the individual commands with `;'.

The output appears in the buffer named by `shell-command-buffer-name-async'.

Commands that are run asynchronously do not accept user input."
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files))
   dired-mode)
  (unless (string-match-p "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (dired-do-shell-command command arg file-list))

;;;###autoload
(defun dired-do-shell-command (command &optional arg file-list)
  "Run a shell command COMMAND on the marked files.
If no files are marked or a numeric prefix arg is given,
the next ARG files are used.  Just \\[universal-argument] means the current file.
The prompt mentions the file(s) or the marker, as appropriate.

If there is a `*' in COMMAND, surrounded by whitespace, this runs
COMMAND just once with the entire file list substituted there.

If there is no `*', but there is a `?' in COMMAND, surrounded by
whitespace, or a `\\=`?\\=`' this runs COMMAND on each file
individually with the file name substituted for `?' or `\\=`?\\=`'.

Otherwise, this runs COMMAND on each file individually with the
file name added at the end of COMMAND (separated by a space).

`*' and `?' when not surrounded by whitespace nor `\\=`' have no special
significance for `dired-do-shell-command', and are passed through
normally to the shell, but you must confirm first.

If you want to use `*' as a shell wildcard with whitespace around
it, write `*\"\"' in place of just `*'.  This is equivalent to just
`*' in the shell, but avoids Dired's special handling.

If COMMAND ends in `&', `;', or `;&', it is executed in the
background asynchronously, and the output appears in the buffer named
by `shell-command-buffer-name-async'.  When operating on multiple files
and COMMAND ends in `&', the shell command is executed on each file
in parallel.  However, when COMMAND ends in `;' or `;&', then commands
are executed in the background on each file sequentially waiting for
each command to terminate before running the next command.  You can
also use `dired-do-async-shell-command' that automatically adds `&'.

Otherwise, COMMAND is executed synchronously, and the output
appears in the buffer named by `shell-command-buffer-name'.

This feature does not try to redisplay Dired buffers afterward, as
there's no telling what files COMMAND may have changed.
Type \\[dired-do-redisplay] to redisplay the marked files.

When COMMAND runs, its working directory is the top-level directory
of the Dired buffer, so output files usually are created there
instead of in a subdir.

In a noninteractive call (from Lisp code), you must specify
the list of file names explicitly with the FILE-LIST argument, which
can be produced by `dired-get-marked-files', for example.

`dired-guess-shell-alist-default', `dired-guess-shell-alist-optional'
and `dired-guess-shell-alist-user' are consulted when the user is
prompted for the shell command to use interactively.

Also see the `dired-confirm-shell-command' variable."
  ;; Functions dired-run-shell-command and dired-shell-stuff-it do the
  ;; actual work and can be redefined for customization.
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "! on %s: " current-prefix-arg files)
      current-prefix-arg
      files))
   dired-mode)
  (let* ((on-each (not (dired--star-or-qmark-p command "*" 'keep)))
	 (no-subst (not (dired--star-or-qmark-p command "?" 'keep)))
         (confirmations nil)
         ;; Get confirmation for wildcards that may have been meant
         ;; to control substitution of a file name or the file name list.
         (ok (cond
              ((not (or on-each no-subst))
               (error "You can not combine `*' and `?' substitution marks"))
              ((not dired-confirm-shell-command)
               t)
              ((setq confirmations (dired--need-confirm-positions command "*"))
               (dired--no-subst-confirm confirmations command))
              ((setq confirmations (dired--need-confirm-positions command "?"))
               (dired--no-subst-confirm confirmations command))
              (t))))
    (cond ((not ok) (message "Command canceled"))
          (t
           (if on-each
	       (dired-bunch-files (- 10000 (length command))
	                          (lambda (&rest files)
	                            (dired-run-shell-command
                                     (dired-shell-stuff-it command files t arg)))
	                          nil file-list)
	     ;; execute the shell command
	     (dired-run-shell-command
              (dired-shell-stuff-it command file-list nil arg))))))
  (dired-post-do-command))

;; Might use {,} for bash or csh:
(defvar dired-mark-prefix ""
  "Prepended to marked files in Dired shell commands.")
(defvar dired-mark-postfix ""
  "Appended to marked files in Dired shell commands.")
(defvar dired-mark-separator " "
  "Separates marked files in Dired shell commands.")

(defun dired-shell-stuff-it (command file-list on-each &optional _raw-arg)
  ;; "Make up a shell command line from COMMAND and FILE-LIST.
  ;; If ON-EACH is t, COMMAND should be applied to each file, else
  ;; simply concat all files and apply COMMAND to this.
  ;; FILE-LIST's elements will be quoted for the shell."
  ;; Might be redefined for smarter things and could then use RAW-ARG
  ;; (coming from interactive P and currently ignored) to decide what to do.
  ;; Smart would be a way to access basename or extension of file names.
  (let* ((in-background (string-match "[ \t]*&[ \t]*\\'" command))
	 (command (if in-background
		      (substring command 0 (match-beginning 0))
		    command))
	 (sequentially (string-match "[ \t]*;[ \t]*\\'" command))
	 (command (if sequentially
		      (substring command 0 (match-beginning 0))
		    command))
         (parallel-in-background
          (and in-background (not sequentially) (not (eq system-type 'ms-dos))))
         (w32-shell (and (fboundp 'w32-shell-dos-semantics)
                         (w32-shell-dos-semantics)))
         (file-remote (file-remote-p default-directory))
         ;; The way to run a command in background in Windows shells
         ;; is to use the START command.  The /B switch means not to
         ;; create a new window for the command.
         (cmd-prefix (if (and w32-shell (not file-remote)) "start /b " ""))
         ;; Windows shells don't support chaining with ";", they use
         ;; "&" instead.
         (cmd-sep (if (and (or (not w32-shell) file-remote)
			   (not parallel-in-background))
		      "; " "& "))
	 (stuff-it
	  (if (dired--star-or-qmark-p command nil 'keep)
	      (lambda (x)
		(let ((retval (concat cmd-prefix command)))
		  (while (dired--star-or-qmark-p retval nil)
		    (setq retval (replace-match x t t retval 2)))
		  retval))
	    (lambda (x) (concat cmd-prefix command dired-mark-separator x)))))
    ;; If a file name starts with "-", add a "./" to avoid the command
    ;; interpreting it as a command line switch.
    (setq file-list (mapcar (lambda (file)
                              (if (string-match "\\`-" file)
                                  (concat "./" file)
                                file))
                            file-list))
    (concat
     (cond
      (on-each
       (format "%s%s"
               (mapconcat stuff-it (mapcar #'shell-quote-argument file-list)
                          cmd-sep)
               ;; POSIX shells running a list of commands in the background
               ;; (LIST = cmd_1 & [cmd_2 & ... cmd_i & ... cmd_N &])
               ;; return once cmd_N ends, i.e., the shell does not
               ;; wait for cmd_i to finish before executing cmd_i+1.
               ;; That means, running (shell-command LIST) may not show
               ;; the output of all the commands (Bug#23206).
               ;; Add 'wait' to force those POSIX shells to wait until
               ;; all commands finish.
               (or (and parallel-in-background (not w32-shell)
                        " & wait")
                   "")))
      (t
       (let ((files (mapconcat #'shell-quote-argument
                               file-list dired-mark-separator)))
         (when (cdr file-list)
           (setq files (concat dired-mark-prefix files dired-mark-postfix)))
         (concat
          (funcall stuff-it files)
          ;; Be consistent in how we treat inputs to commands -- do
          ;; the same here as in the `on-each' case.
          (if (and in-background (not w32-shell))
              " & wait"
            "")))))
     (or (and in-background "& ")
         ""))))

;; This is an extra function so that it can be redefined by ange-ftp.
;;;###autoload
(defun dired-run-shell-command (command)
  (let ((handler
	 (find-file-name-handler (directory-file-name default-directory)
				 'shell-command)))
    (if handler (apply handler 'shell-command (list command))
      (shell-command command)))
  ;; Return nil for sake of nconc in dired-bunch-files.
  nil)


(defun dired-check-process (msg program &rest arguments)
  "Display MSG, then run PROGRAM, and log any error messages from it.
ARGUMENTS should be strings to be passed to PROGRAM as command-line
arguments.

If PROGRAM exits successfully, display \"MSG...done\" and return nil.
If PROGRAM exits abnormally, save in `dired-log-buffer' the command
that invoked PROGRAM and the messages it emitted, and return either
the offending ARGUMENTS or PROGRAM if no ARGUMENTS were provided."
  (let ((dir default-directory)
        (reporter (make-progress-reporter msg))
        err-buffer err)
    (save-excursion
      ;; Get a clean buffer for error output:
      (setq err-buffer (get-buffer-create " *dired-check-process output*"))
      (set-buffer err-buffer)
      (erase-buffer)
      (setq default-directory dir	; caller's default-directory
	    err (not (eq 0 (apply #'process-file program nil t nil arguments))))
      (dired-uncache dir)
      (if err
	  (progn
	    (dired-log (concat program " " (prin1-to-string arguments) "\n"))
	    (dired-log err-buffer)
	    (or arguments program t))
	(kill-buffer err-buffer)
        (progress-reporter-done reporter)
        nil))))

(defun dired-shell-command (cmd)
  "Run CMD, and check for output.
On error, pop up the log buffer.
Return the result of `process-file' - zero for success."
  (let ((out-buffer " *dired-check-process output*")
        (dir default-directory))
    (with-current-buffer (get-buffer-create out-buffer)
      (erase-buffer)
      (let ((default-directory dir) res)
        (with-connection-local-variables
         (setq res (process-file
                    shell-file-name
                    nil
                    t
                    nil
                    shell-command-switch
                    cmd))
         (dired-uncache dir)
         (unless (zerop res)
           (pop-to-buffer out-buffer))
         res)))))


;;; Guess shell command

;; * `dired-guess-shell-command' provides smarter defaults for
;;    `dired-read-shell-command'.
;;
;; * `dired-guess-shell-command' calls `dired-guess-default' with list of
;;    marked files.
;;
;; * Parse `dired-guess-shell-alist-user', `dired-guess-shell-alist-default',
;;   `dired-guess-shell-alist-optional' (in that order) for the first REGEXP
;;   that matches the first file in the file list.
;;
;; * If the REGEXP matches all the entries of the file list then evaluate
;;   COMMAND, which is either a string or a Lisp expression returning a
;;   string.  COMMAND may be a list of commands.
;;
;; * Return this command to `dired-guess-shell-command' which prompts user
;;   with it.  The list of commands is put into the list of default values.
;;   If a command is used successfully then it is stored permanently in
;;   `dired-shell-command-history'.

;; Guess what shell command to apply to a file.
(defvar dired-shell-command-history nil
  "History list for commands that read dired-shell commands.")

;; Default list of shell commands.

;; NOTE: Use `gunzip -c' instead of `zcat' on `.gz' files.  Some do not
;; install GNU zip's version of zcat.

(autoload 'Man-support-local-filenames "man")
(autoload 'vc-responsible-backend "vc")
(autoload 'vc-backend-for-registration "vc")

(defvar dired-guess-shell-alist-default
  (list
   (list "\\.tar\\'"
         '(if dired-guess-shell-gnutar
              (concat dired-guess-shell-gnutar " xvf")
            "tar xvf")
         ;; Extract files into a separate subdirectory
         '(if dired-guess-shell-gnutar
              (concat "mkdir " (file-name-sans-extension file)
                      "; " dired-guess-shell-gnutar " -C "
                      (file-name-sans-extension file) " -xvf")
            (concat "mkdir " (file-name-sans-extension file)
                    "; tar -C " (file-name-sans-extension file) " -xvf"))
         ;; List archive contents.
         '(if dired-guess-shell-gnutar
              (concat dired-guess-shell-gnutar " tvf")
            "tar tvf"))

   ;; REGEXPS for compressed archives must come before the .Z rule to
   ;; be recognized:
   (list "\\.tar\\.Z\\'"
         ;; Untar it.
         '(if dired-guess-shell-gnutar
              (concat dired-guess-shell-gnutar " zxvf")
            (concat "zcat * | tar xvf -"))
         ;; Optional conversion to gzip format.
         '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
                  " " dired-guess-shell-znew-switches))

   ;; gzip'ed archives
   (list "\\.t\\(ar\\.\\)?gz\\'"
         '(if dired-guess-shell-gnutar
              (concat dired-guess-shell-gnutar " zxvf")
            (concat "gunzip -qc * | tar xvf -"))
         ;; Extract files into a separate subdirectory
         '(if dired-guess-shell-gnutar
              (concat "mkdir " (file-name-sans-extension file)
                      "; " dired-guess-shell-gnutar " -C "
                      (file-name-sans-extension file) " -zxvf")
            (concat "mkdir " (file-name-sans-extension file)
                    "; gunzip -qc * | tar -C "
                    (file-name-sans-extension file) " -xvf -"))
         ;; Optional decompression.
         '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q" ""))
         ;; List archive contents.
         '(if dired-guess-shell-gnutar
              (concat dired-guess-shell-gnutar " ztvf")
            (concat "gunzip -qc * | tar tvf -")))

   ;; bzip2'ed archives
   (list "\\.t\\(ar\\.bz2\\|bz\\)\\'"
         "bunzip2 -c * | tar xvf -"
         ;; Extract files into a separate subdirectory
         '(concat "mkdir " (file-name-sans-extension file)
                  "; bunzip2 -c * | tar -C "
                  (file-name-sans-extension file) " -xvf -")
         ;; Optional decompression.
         "bunzip2")

   ;; xz'ed archives
   (list "\\.t\\(ar\\.\\)?xz\\'"
         "unxz -c * | tar xvf -"
         ;; Extract files into a separate subdirectory
         '(concat "mkdir " (file-name-sans-extension file)
                  "; unxz -c * | tar -C "
                  (file-name-sans-extension file) " -xvf -")
         ;; Optional decompression.
         "unxz")

   ;; zstandard archives
   `(,(rx (or ".tar.zst" ".tzst") eos) "unzstd -c ? | tar -xf -")
   `(,(rx ".zst" eos)                  "unzstd --rm")

   '("\\.shar\\.Z\\'" "zcat * | unshar")
   '("\\.shar\\.g?z\\'" "gunzip -qc * | unshar")

   '("\\.e?ps\\'" "ghostview" "xloadimage" "lpr")
   (list "\\.e?ps\\.g?z\\'" "gunzip -qc * | ghostview -"
         ;; Optional decompression.
         '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.e?ps\\.Z\\'" "zcat * | ghostview -"
         ;; Optional conversion to gzip format.
         '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
                  " " dired-guess-shell-znew-switches))

   (list "\\.patch\\'"
         '(if (eq (ignore-errors (vc-responsible-backend default-directory)) 'Git)
              "cat * | git apply"
            "cat * | patch"))
   (list "\\.patch\\.g?z\\'" "gunzip -qc * | patch"
         ;; Optional decompression.
         '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.patch\\.Z\\'" "zcat * | patch"
         ;; Optional conversion to gzip format.
         '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
                  " " dired-guess-shell-znew-switches))

   ;; The following four extensions are useful with dired-man ("N" key)
   ;; FIXME "man ./" does not work with dired-do-shell-command,
   ;; because there seems to be no way for us to modify the filename,
   ;; only the command.  Hmph.  `dired-man' works though.
   (list "\\.\\(?:[0-9]\\|man\\)\\'"
         '(let ((loc (Man-support-local-filenames)))
            (cond ((eq loc 'man-db) "man -l")
                  ((eq loc 'man) "man ./")
                  (t
                   "cat * | tbl | nroff -man -h | col -b"))))
   (list "\\.\\(?:[0-9]\\|man\\)\\.g?z\\'"
         '(let ((loc (Man-support-local-filenames)))
            (cond ((eq loc 'man-db)
                   "man -l")
                  ((eq loc 'man)
                   "man ./")
                  (t "gunzip -qc * | tbl | nroff -man -h | col -b")))
         ;; Optional decompression.
         '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.[0-9]\\.Z\\'"
         '(let ((loc (Man-support-local-filenames)))
            (cond ((eq loc 'man-db) "man -l")
                  ((eq loc 'man) "man ./")
                  (t "zcat * | tbl | nroff -man -h | col -b")))
         ;; Optional conversion to gzip format.
         '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
                  " " dired-guess-shell-znew-switches))
   '("\\.pod\\'" "perldoc" "pod2man * | nroff -man")

   '("\\.uu\\'" "uudecode")		; for uudecoded files
   '("\\.sh\\'" "sh")			; execute shell scripts
   '("\\.tex\\'" "latex" "tex")
   '("\\.texi\\(nfo\\)?\\'" "makeinfo" "texi2dvi")
   '("\\.rpm\\'" "rpm -qilp" "rpm -ivh")
   '("\\.dia\\'" "dia")
   '("\\.mgp\\'" "mgp")

   ;; Some other popular archivers.
   (list "\\.zip\\'" "unzip" "unzip -l"
         ;; Extract files into a separate subdirectory
         '(concat "unzip" (if dired-guess-shell-gzip-quiet " -q")
                  " -d " (file-name-sans-extension file)))
   '("\\.zoo\\'" "zoo x//")
   '("\\.lzh\\'" "lharc x")
   '("\\.arc\\'" "arc x")
   '("\\.shar\\'" "unshar")
   '("\\.rar\\'" "unrar x")
   '("\\.7z\\'" "7z x")

   ;; Compression.
   (list "\\.g?z\\'" '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.dz\\'" "dictunzip")
   (list "\\.bz2\\'" "bunzip2")
   (list "\\.xz\\'" "unxz")
   (list "\\.Z\\'" "uncompress"
         ;; Optional conversion to gzip format.
         '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
                  " " dired-guess-shell-znew-switches))

   '("\\.sign?\\'" "gpg --verify"))
  "Default alist used for shell command guessing.
See also `dired-guess-shell-alist-optional' and
`dired-guess-shell-alist-user'.")

(defvar dired-guess-shell-alist-optional
  (list
   '("\\.dvi\\'" "xdvi" "dvips")	; preview and printing
   '("\\.au\\'" "play")			; play Sun audiofiles
   '("\\.mpe?g\\'\\|\\.avi\\'" "xine -p")
   '("\\.ogg\\'" "ogg123")
   '("\\.mp3\\'" "mpg123")
   '("\\.wav\\'" "play")
   '("\\.hqx\\'" "mcvert")
   '("\\.xbm\\'" "bitmap")		; view X11 bitmaps
   '("\\.gp\\'" "gnuplot")
   '("\\.p[bgpn]m\\'" "xloadimage")
   '("\\.gif\\'" "xloadimage")		; view gif pictures
   '("\\.tif\\'" "xloadimage")
   '("\\.png\\'" "display")		; xloadimage 4.1 doesn't grok PNG
   '("\\.jpe?g\\'" "xloadimage")
   '("\\.fig\\'" "xfig")		; edit fig pictures
   '("\\.out\\'" "xgraph")		; for plotting purposes.
   '("\\.pdf\\'" "xpdf")
   '("\\.doc\\'" "antiword" "strings"))
  "Optional alist used for shell command guessing.
Unlike `dired-guess-shell-alist-default' that contains mostly the
standard commands that handle the files with corresponding extensions
such as the `tar' command handling the files with the `.tar' extension,
this list contains the commands such as media players and viewers
that don't exist on many systems where other alternatives are available.

See also `dired-guess-shell-alist-user'.")

(defun dired-guess-default (files)
  "Return a shell command, or a list of commands, appropriate for FILES.
See `dired-guess-shell-alist-user'."
  (let* ((case-fold-search dired-guess-shell-case-fold-search)
         (programs
          (delete-dups
           (mapcar
            (lambda (command)
              (eval command `((file . ,(car files)))))
            (seq-reduce
             #'append
             (mapcar #'cdr
                     (seq-filter (lambda (elem)
                                   (seq-every-p
                                    (lambda (file)
                                      (string-match-p (car elem) file))
                                    files))
                                 (append dired-guess-shell-alist-user
                                         dired-guess-shell-alist-default
                                         dired-guess-shell-alist-optional)))
             nil)))))
    (if (length= programs 1)
        (car programs)
      programs)))

;;;###autoload
(defun dired-guess-shell-command (prompt files)
  "Ask user with PROMPT for a shell command, guessing a default from FILES."
  (let ((default (shell-command-guess files))
        default-list val)
    (if (null default)
        ;; Nothing to guess
        (read-shell-command prompt nil 'dired-shell-command-history)
      (setq prompt (replace-regexp-in-string ": $" " " prompt))
      (if (listp default)
          ;; More than one guess
          (setq default-list default
                default (car default)
                prompt (concat
                        prompt
                        (format "{%d guesses} " (length default-list))))
        ;; Just one guess
        (setq default-list (list default)))
      ;; Put the first guess in the prompt but not in the initial value.
      (setq prompt (concat prompt (format "[%s]: " default)))
      ;; All guesses can be retrieved with M-n
      (setq val (read-shell-command prompt nil
                                    'dired-shell-command-history
                                    default-list))
      ;; If we got a return, then return default.
      (if (equal val "") default val))))

(defcustom shell-command-guess-functions
  '(shell-command-guess-dired-optional
    shell-command-guess-mailcap
    shell-command-guess-xdg
    shell-command-guess-dired-default
    shell-command-guess-dired-user)
  "List of functions that guess shell commands for files.
Each function receives a list of commands and a list of file names
and should return the same list of commands with changes
such as new commands added to the beginning of the list.
In this case the commands from the last entry
will be at the top of the resulted list."
  :type '(repeat
          (choice (function-item shell-command-guess-dired-user)
                  (function-item shell-command-guess-dired-default)
                  (function-item shell-command-guess-dired-optional)
                  (function-item shell-command-guess-mailcap)
                  (function-item shell-command-guess-xdg)
                  (function-item shell-command-guess-open)
                  (function :tag "Custom function")))
  :group 'dired
  :version "30.1")

(defun shell-command-guess (files)
  "Return a list of shell commands, appropriate for FILES.
The list is populated by calling functions from
`shell-command-guess-functions'.  Each function receives the list
of commands and the list of file names and returns the same list
after adding own commands to the composite list."
  (let ((commands nil))
    (run-hook-wrapped 'shell-command-guess-functions
                      (lambda (fun)
                        (setq commands (funcall fun commands files))
                        nil))
    commands))

(defun shell-command-guess-dired-user (commands files)
  "Populate COMMANDS using `dired-guess-shell-alist-user'.
This excludes `dired-guess-shell-alist-default' and
`dired-guess-shell-alist-optional'."
  (let ((dired-guess-shell-alist-default nil)
        (dired-guess-shell-alist-optional nil))
    (append (ensure-list (dired-guess-default files)) commands)))

(defun shell-command-guess-dired-default (commands files)
  "Populate COMMANDS using `dired-guess-shell-alist-default'.
This excludes `dired-guess-shell-alist-user' and
`dired-guess-shell-alist-optional'."
  (let ((dired-guess-shell-alist-user nil)
        (dired-guess-shell-alist-optional nil))
    (append (ensure-list (dired-guess-default files)) commands)))

(defun shell-command-guess-dired-optional (commands files)
  "Populate COMMANDS using `dired-guess-shell-alist-optional'.
This excludes `dired-guess-shell-alist-user' and
`dired-guess-shell-alist-default'."
  (let ((dired-guess-shell-alist-user nil)
        (dired-guess-shell-alist-default nil))
    (append (ensure-list (dired-guess-default files)) commands)))

(declare-function mailcap-file-default-commands "mailcap" (files))

(defun shell-command-guess-mailcap (commands files)
  "Populate COMMANDS by MIME types of FILES."
  (require 'mailcap)
  (append (mailcap-file-default-commands files) commands))

(declare-function xdg-mime-apps "xdg" (mime))
(declare-function xdg-desktop-read-file "xdg" (filename &optional group))

(defun shell-command-guess-xdg (commands files)
  "Populate COMMANDS by XDG configuration for FILES."
  (require 'xdg)
  (let* ((xdg-mime (when (executable-find "xdg-mime")
                     (string-trim-right
                      (shell-command-to-string
                       (concat "xdg-mime query filetype "
                               (shell-quote-argument (car files)))))))
         (xdg-mime-apps (unless (or (null xdg-mime)
                                    (string-empty-p xdg-mime))
                          (xdg-mime-apps xdg-mime)))
         (xdg-commands
          (mapcar (lambda (desktop)
                    (setq desktop (xdg-desktop-read-file desktop))
                    (propertize
                     (replace-regexp-in-string
                      " .*" "" (gethash "Exec" desktop))
                     'name (gethash "Name" desktop)))
                  xdg-mime-apps)))
    (append xdg-commands commands)))

(defcustom shell-command-guess-open
  (cond
   ((executable-find "xdg-open")
    "xdg-open")
   ((memq system-type '(gnu/linux darwin))
    "open")
   ((memq system-type '(windows-nt ms-dos))
    "start")
   ((eq system-type 'cygwin)
    "cygstart")
   ((executable-find "run-mailcap")
    "run-mailcap"))
  "A shell command to open a file externally."
  :type '(choice (const :tag "None" nil) string)
  :group 'dired
  :version "30.1")

(defun shell-command-guess-open (commands _files)
  "Populate COMMANDS by the `open' command."
  (append (ensure-list shell-command-guess-open) commands))

(declare-function w32-shell-execute "w32fns.c")

;;;###autoload
(defun shell-command-do-open (files)
  "Open each of FILES using an external program.
This \"opens\" the file(s) using the external command that is most
appropriate for the file(s) according to the system conventions."
  (let ((command shell-command-guess-open))
    (when (and (memq system-type '(windows-nt))
               (equal command "start"))
      (setq command "open"))
    (if command
        (cond
         ((memq system-type '(ms-dos))
          (dolist (file files)
            (shell-command (concat command " " (shell-quote-argument file)))))
         ((memq system-type '(windows-nt))
          (dolist (file files)
            (w32-shell-execute command (convert-standard-filename file))))
         ((memq system-type '(cygwin))
          (dolist (file files)
            (call-process command nil nil nil file)))
         ((memq system-type '(darwin))
          (dolist (file files)
            (start-process (concat command " " file) nil command file)))
         (t
          (dolist (file files)
            (call-process command nil 0 nil file))))
      (error "Open not supported on this system"))))

;;;###autoload
(defun dired-do-open (&optional arg)
  "Open all marked (or next ARG) files using an external program.
This \"opens\" the file(s) using the external command that is most
appropriate for the file(s) according to the system conventions.
If files are marked, run the command on each marked file.  Otherwise,
run it on the next ARG files, or on the file at mouse-click, or on the
file at point.  The appropriate command to \"open\" a file on each
system is determined by `shell-command-guess-open'."
  (interactive "P" dired-mode)
  (shell-command-do-open (if (mouse-event-p last-nonmenu-event)
                             (save-excursion
                               (mouse-set-point last-nonmenu-event)
                               (dired-get-marked-files nil arg))
                           (dired-get-marked-files nil arg))))


;;; Commands that delete or redisplay part of the dired buffer

(defun dired-kill-line (&optional arg)
  "Kill the current line (not the files).
With a prefix argument, kill that many lines starting with the current line.
(A negative argument kills backward.)"
  (interactive "P" dired-mode)
  (setq arg (prefix-numeric-value arg))
  (let (buffer-read-only file)
    (while (/= 0 arg)
      (setq file (dired-get-filename nil t))
      (if (not file)
	  (error "Can only kill file lines")
	(save-excursion (and file
			     (dired-goto-subdir file)
			     (dired-kill-subdir)))
	(delete-region (line-beginning-position)
		       (progn (forward-line 1) (point)))
	(if (> arg 0)
	    (setq arg (1- arg))
	  (setq arg (1+ arg))
	  (forward-line -1))))
    (dired-move-to-filename)))

;;;###autoload
(defun dired-do-kill-lines (&optional arg fmt init-count)
  "Remove all marked lines, or the next ARG lines.
The files or directories on those lines are _not_ deleted.  Only the
Dired listing is affected.  To restore the removals, use \\[revert-buffer].

With a numeric prefix arg, remove that many lines going forward,
starting with the current line.  (A negative prefix arg removes lines
going backward.)

If you use a prefix arg to remove the line for a subdir whose listing
you have inserted into the Dired buffer, then that subdir listing is
also removed.

To remove a subdir listing _without_ removing the subdir's line in its
parent listing, go to the header line of the subdir listing and use
this command with any prefix arg.

When called from Lisp, non-nil INIT-COUNT is added to the number of
lines removed by this invocation, for the reporting message.

A FMT of \"\" will suppress the messaging."
  ;; Returns count of killed lines.
  (interactive "P" dired-mode)
  (if arg
      (if (dired-get-subdir)
          (dired-kill-subdir)
        (dired-kill-line arg))
    (save-excursion
      (goto-char (point-min))
      (let ((count (or init-count  0))
            (regexp (dired-marker-regexp))
            (inhibit-read-only t))
        (while (and (not (eobp))
                    (re-search-forward regexp nil t))
          (setq count (1+ count))
          (delete-region (line-beginning-position)
                         (progn (forward-line 1) (point))))
        (unless (equal "" fmt)
          (message (or fmt "Killed %d line%s.") count (dired-plural-s count)))
        count))))


;;; Compression

(defun dired-compress ()
  ;; Compress or uncompress the current file.
  ;; Return nil for success, offending filename else.
  (let* (buffer-read-only
	 (from-file (dired-get-filename))
	 (new-file (dired-compress-file from-file)))
    (if new-file
	(let ((start (point)))
	  ;; Remove any preexisting entry for the name NEW-FILE.
	  (ignore-errors (dired-remove-entry new-file))
	  (goto-char start)
	  ;; Now replace the current line with an entry for NEW-FILE,
	  ;; if it exists.  But don't remove the current line if
	  ;; either FROM-FILE or NEW-FILE is a directory, because
	  ;; compressing/uncompressing directories doesn't remove the
	  ;; original.  If NEW-FILE doesn't exist, assume that we are
	  ;; out of sync with the current directory, and revert it.
	  ;; This can happen, for example, when unpacking a .tar.gz
	  ;; archive which adds files to the current directory (as
	  ;; opposed to adding them to a directory whose name is
	  ;; NEW-FILE).
          (if (file-exists-p new-file)
              (if (or (file-directory-p from-file)
                      (file-directory-p new-file))
                  (dired-add-entry new-file nil t)
                (dired-update-file-line new-file))
            (dired-fun-in-all-buffers (dired-current-directory)
                                      nil #'revert-buffer))
          nil)
      (dired-log (concat "Failed to (un)compress " from-file))
      from-file)))

(defvar dired-compress-file-suffixes
  '(
    ;; "tar -zxf" isn't used because it's not available on the
    ;; Solaris 10 version of tar (obsolete in 2024?).
    ;; Same thing on AIX 7.1 (obsolete 2023?) and 7.2 (obsolete 2022?).
    ("\\.tar\\.gz\\'" "" "gzip -dc %i | tar -xf -")
    ("\\.tar\\.xz\\'" "" "xz -dc %i | tar -xf -")
    ("\\.tgz\\'" "" "gzip -dc %i | tar -xf -")
    ("\\.gz\\'" "" "gzip -d")
    ("\\.lz\\'" "" "lzip -d")
    ("\\.Z\\'" "" "uncompress")
    ;; For .z, try gunzip.  It might be an old gzip file,
    ;; or it might be from compact? pack? (which?) but gunzip handles both.
    ("\\.z\\'" "" "gzip -d")
    ("\\.dz\\'" "" "dictunzip")
    ("\\.tbz\\'" ".tar" "bunzip2")
    ("\\.bz2\\'" "" "bunzip2")
    ("\\.xz\\'" "" "unxz")
    ("\\.zip\\'" "" "unzip -o -d %o %i")
    ("\\.tar\\.zst\\'" "" "unzstd -c %i | tar -xf -")
    ("\\.tzst\\'" "" "unzstd -c %i | tar -xf -")
    ("\\.zst\\'" "" "unzstd --rm")
    ("\\.7z\\'" "" "7z x -aoa -o%o %i")
    ;; This item controls naming for compression.
    ("\\.tar\\'" ".tgz" nil))
  "Control changes in file name suffixes for compression and uncompression.
Each element specifies one transformation rule, and has the form:
  (REGEXP NEW-SUFFIX PROGRAM)
The rule applies when the old file name matches REGEXP.
The new file name is computed by deleting the part that matches REGEXP
 (as well as anything after that), then adding NEW-SUFFIX in its place.
If PROGRAM is non-nil, the rule is an uncompression rule,
and uncompression is done by running PROGRAM.

Within PROGRAM, %i denotes the input file, and %o denotes the
output file.

Otherwise, the rule is a compression rule, and compression is done with gzip.
ARGS are command switches passed to PROGRAM.")

(defcustom dired-compress-file-default-suffix nil
  "Default suffix for compressing a single file.
If nil, \".gz\" will be used."
  :type '(choice (const :tag ".gz" nil) string)
  :group 'dired
  :version "28.1")

(defvar dired-compress-file-alist
  '(("\\.gz\\'" . "gzip -9f %i")
    ("\\.bz2\\'" . "bzip2 -9f %i")
    ("\\.xz\\'" . "xz -9f %i")
    ("\\.zst\\'" . "zstd -qf -19 --rm -o %o %i"))
  "Controls the compression shell command for `dired-do-compress-to'.

Each element is (REGEXP . CMD), where REGEXP is the name of the
archive to which you want to compress, and CMD is the
corresponding command.

Within CMD, %i denotes the input file(s), and %o denotes the
output file.  %i path(s) are relative, while %o is absolute.")

(defcustom dired-compress-directory-default-suffix nil
  "Default suffix for compressing a directory.
If nil, \".tar.gz\" will be used."
  :type '(choice (const :tag ".tar.gz" nil) string)
  :group 'dired
  :version "28.1")

(defvar dired-compress-files-alist
  '(("\\.tar\\.gz\\'" . "tar -cf - %i | gzip -c9 > %o")
    ("\\.tar\\.bz2\\'" . "tar -cf - %i | bzip2 -c9 > %o")
    ("\\.tar\\.xz\\'" . "tar -cf - %i | xz -c9 > %o")
    ("\\.tar\\.zst\\'" . "tar -cf - %i | zstd -19 -o %o")
    ("\\.tar\\.lz\\'" . "tar -cf - %i | lzip -c9 > %o")
    ("\\.tar\\.lzo\\'" . "tar -cf - %i | lzop -c9 > %o")
    ("\\.zip\\'" . "zip %o -r --filesync %i")
    ("\\.pax\\'" . "pax -wf %o %i"))
  "Controls the compression shell command for `dired-do-compress-to'.

Each element is (REGEXP . CMD), where REGEXP is the name of the
archive to which you want to compress, and CMD is the
corresponding command.

Within CMD, %i denotes the input file(s), and %o denotes the
output file.  %i path(s) are relative, while %o is absolute.")

;;;###autoload
(defun dired-do-compress-to ()
  "Compress selected files and directories to an archive.
Prompt for the archive file name.
Choose the archiving command based on the archive file-name extension
and `dired-compress-files-alist'."
  (interactive nil dired-mode)
  (let* ((in-files (dired-get-marked-files nil nil nil nil t))
         (out-file (expand-file-name (read-file-name "Compress to: ")))
         (rule (cl-find-if
                (lambda (x)
                  (string-match (car x) out-file))
                dired-compress-files-alist)))
    (cond ((not rule)
           (error
            "No compression rule found for %s, see `dired-compress-files-alist'"
            out-file))
          ((and (file-exists-p out-file)
                (not (y-or-n-p
                      (format "%s exists, overwrite?"
                              (abbreviate-file-name out-file)))))
           (message "Compression aborted"))
          (t
           (when (zerop
                  (dired-shell-command
                   (format-spec (cdr rule)
                                `((?o . ,(shell-quote-argument
                                          (file-local-name out-file)))
                                  (?i . ,(mapconcat
                                          (lambda (in-file)
                                            (shell-quote-argument
                                             (file-relative-name in-file)))
                                          in-files " "))))))
             (message (ngettext "Compressed %d file to %s"
			        "Compressed %d files to %s"
			        (length in-files))
                      (length in-files)
                      (file-name-nondirectory out-file))))))
  (dired-post-do-command))

;;;###autoload
(defun dired-compress-file (file)
  "Compress or uncompress FILE.
Return the name of the compressed or uncompressed file.
Return nil if no change in files."
  (let ((handler (find-file-name-handler file 'dired-compress-file))
        suffix newname
        (suffixes dired-compress-file-suffixes)
        command)
    ;; See if any suffix rule matches this file name.
    (while suffixes
      (let (case-fold-search)
        (if (string-match (car (car suffixes)) file)
            (setq suffix (car suffixes) suffixes nil))
        (setq suffixes (cdr suffixes))))
    ;; If so, compute desired new name.
    (if suffix
        (setq newname (concat (substring file 0 (match-beginning 0))
                              (nth 1 suffix))))
    (cond (handler
           (funcall handler 'dired-compress-file file))
          ((file-symlink-p file)
           nil)
          ((and suffix (setq command (nth 2 suffix)))
           (if (string-match "%[io]" command)
               (prog1 (setq newname (file-name-as-directory newname))
                 (dired-shell-command
                  (replace-regexp-in-string
                   "%o" (shell-quote-argument (file-local-name newname))
                   (replace-regexp-in-string
                    "%i" (shell-quote-argument (file-local-name file))
                    command
                    nil t)
                   nil t)))
             ;; We found an uncompression rule.
             (let ((match (string-search " " command))
                   (msg (concat "Uncompressing " file)))
               (unless (if match
                           (dired-check-process msg
                                                (substring command 0 match)
                                                (substring command (1+ match))
                                                (file-local-name file))
                         (dired-check-process msg
                                              command
                                              (file-local-name file)))
                 newname))))
          (t
           ;; We don't recognize the file as compressed, so compress it.
           ;; Try gzip; if we don't have that, use compress.
           (condition-case nil
               (if (file-directory-p file)
                   (let* ((suffix
                           (or dired-compress-directory-default-suffix
                               ".tar.gz"))
                          (rule (cl-find-if
                                 (lambda (x) (string-match-p (car x) suffix))
                                 dired-compress-files-alist)))
                     (if rule
                         (let ((out-name (concat file suffix))
                               (default-directory (file-name-directory file)))
                           (dired-shell-command
                            (replace-regexp-in-string
                             "%o" (shell-quote-argument
                                   (file-local-name out-name))
                             (replace-regexp-in-string
                              "%i" (shell-quote-argument
                                    (file-name-nondirectory file))
                              (cdr rule)
                              nil t)
                             nil t))
                           out-name)
                       (user-error
                        "No compression rule found for \
`dired-compress-directory-default-suffix' %s, see `dired-compress-files-alist' for\
 the supported suffixes list"
                        dired-compress-directory-default-suffix)))
                 (let* ((suffix (or dired-compress-file-default-suffix ".gz"))
                        (out-name (concat file suffix))
                        (rule (cl-find-if
                               (lambda (x) (string-match-p (car x) suffix))
                               dired-compress-file-alist)))
                   (if (not rule)
                       (user-error "No compression rule found for suffix %s, \
see `dired-compress-file-alist' for the supported suffixes list"
                                   dired-compress-file-default-suffix)
                     (and (file-exists-p file)
                          (or (not (file-exists-p out-name))
                              (y-or-n-p
                               (format
                                "File %s already exists.  Really compress? "
                                out-name)))
                          (dired-shell-command
                           (replace-regexp-in-string
                            "%o" (shell-quote-argument
                                  (file-local-name out-name))
                            (replace-regexp-in-string
                             "%i" (shell-quote-argument (file-local-name file))
                             (cdr rule)
                             nil t)
                            nil t))
                          (or (file-exists-p out-name)
                              (setq out-name (concat file ".z")))
                          ;; Rename the compressed file to NEWNAME
                          ;; if it hasn't got that name already.
                          (if (and newname (not (equal newname out-name)))
                              (progn
                                (rename-file out-name newname t)
                                newname)
                            out-name)))))
             (file-error
              (if (not (dired-check-process (concat "Compressing " file)
                                            "compress" "-f"
                                            (file-local-name file)))
                  ;; Don't use NEWNAME with `compress'.
                  (concat file ".Z"))))))))

(defun dired-mark-confirm (op-symbol arg)
  ;; Request confirmation from the user that the operation described
  ;; by OP-SYMBOL is to be performed on the marked files.
  ;; Confirmation consists in a y-or-n question with a file list
  ;; pop-up unless OP-SYMBOL is a member of `dired-no-confirm'.
  ;; The files used are determined by ARG (as in dired-get-marked-files).
  (or (eq dired-no-confirm t)
      (memq op-symbol dired-no-confirm)
      ;; Pass t for DISTINGUISH-ONE-MARKED so that a single file which
      ;; is marked pops up a window.  That will help the user see
      ;; it isn't the current line file.
      (let ((files (dired-get-marked-files t arg nil t t))
	    (string (if (eq op-symbol 'compress) "Compress or uncompress"
		      (capitalize (symbol-name op-symbol)))))
	(dired-mark-pop-up nil op-symbol files #'y-or-n-p
			   (concat string " "
				   (dired-mark-prompt arg files) "? ")))))

(defun dired-map-over-marks-check (fun arg op-symbol &optional show-progress)
  ;;  "Map FUN over marked files (with second ARG like in dired-map-over-marks)
  ;; and display failures.

  ;; FUN takes zero args.  It returns non-nil (the offending object, e.g.
  ;; the short form of the filename) for a failure and probably logs a
  ;; detailed error explanation using function `dired-log'.

  ;; OP-SYMBOL is a symbol describing the operation performed (e.g.
  ;; `compress').  It is used with `dired-mark-pop-up' to prompt the user
  ;; (e.g. with `Compress * [2 files]? ') and to display errors (e.g.
  ;; `Failed to compress 1 of 2 files - type W to see why ("foo")')

  ;; SHOW-PROGRESS if non-nil means redisplay dired after each file."
  (if (dired-mark-confirm op-symbol arg)
      (let* ((total-list;; all of FUN's return values
	      (dired-map-over-marks (funcall fun) arg show-progress))
	     (total (length total-list))
	     (failures (delq nil total-list))
	     (count (length failures))
	     (string (if (eq op-symbol 'compress) "Compress or uncompress"
		       (capitalize (symbol-name op-symbol)))))
	(if (not failures)
	    (message (ngettext "%s: %d file." "%s: %d files." total)
		     string total)
	  ;; end this bunch of errors:
	  (dired-log-summary
	   (format (ngettext "Failed to %s %d of %d file"
                             "Failed to %s %d of %d files"
                             total)
		   (downcase string) count total)
	   failures)))))

;;;###autoload
(defun dired-query (sym prompt &rest args)
  "Format PROMPT with ARGS, query user, and store the result in SYM.
The return value is either nil or t.

The user may type y or SPC to accept once; n or DEL to skip once;
! to accept this and subsequent queries; or q or ESC to decline
this and subsequent queries.

If SYM is already bound to a non-nil value, this function may
return automatically without querying the user.  If SYM is !,
return t; if SYM is q or ESC, return nil."
  (let* ((char (symbol-value sym))
	 (char-choices '(?y ?\s ?n ?\177 ?! ?q ?\e)))
    (cond ((eq char ?!)
	   t)       ; accept, and don't ask again
	  ((memq char '(?q ?\e))
	   nil)     ; skip, and don't ask again
	  (t        ; no previous answer - ask now
	   (setq prompt
		 (concat (apply #'format-message prompt args)
			 (if help-form
			     (format " [Type yn!q or %s] "
                                     (help-key))
			   " [Type y, n, q or !] ")))
	   (set sym (setq char (read-char-choice prompt char-choices)))
	   (if (memq char '(?y ?\s ?!)) t)))))


;;;###autoload
(defun dired-do-compress (&optional arg)
  "Compress or uncompress marked (or next ARG) files.
If invoked on a directory, compress all of the files in
the directory and all of its subdirectories, recursively,
into a .tar.gz archive.
If invoked on a .tar.gz or a .tgz or a .zip or a .7z archive,
uncompress and unpack all the files in the archive.

When called from Lisp, if ARG is the symbol `marked', compress
only the marked files, or none if no files are marked."
  (interactive "P" dired-mode)
  (dired-map-over-marks-check #'dired-compress arg 'compress t))


;;; Commands for Emacs Lisp files - load and byte compile

(defun dired-byte-compile ()
  ;; Return nil for success, offending file name else.
  (let* ((filename (dired-get-filename))
	 elc-file buffer-read-only failure)
    (condition-case err
	(save-excursion (byte-compile-file filename))
      (error
       (setq failure err)))
    (setq elc-file (byte-compile-dest-file filename))
    (or (file-exists-p elc-file)
	(setq failure t))
    (if failure
	(progn
	  (dired-log "Byte compile error for %s:\n%s\n" filename failure)
	  (dired-make-relative filename))
      (dired-remove-file elc-file)
      (forward-line)			; insert .elc after its .el file
      (dired-add-file elc-file)
      nil)))

;;;###autoload
(defun dired-do-byte-compile (&optional arg)
  "Byte compile marked (or next ARG) Emacs Lisp files.

When called from Lisp, if ARG is the symbol `marked', byte-compile
only the marked files, or none if no files are marked."
  (interactive "P" dired-mode)
  (dired-map-over-marks-check #'dired-byte-compile arg 'byte-compile t))

(defun dired-load ()
  ;; Return nil for success, offending file name else.
  (let ((file (dired-get-filename)) failure)
    (condition-case err
        (load file nil nil t)
      (error (setq failure err)))
    (if (not failure)
	nil
      (dired-log "Load error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

;;;###autoload
(defun dired-do-load (&optional arg)
  "Load the marked (or next ARG) Emacs Lisp files.

When called from Lisp, if ARG is the symbol `marked', load
only the marked files, or none if no files are marked."
  (interactive "P" dired-mode)
  (dired-map-over-marks-check #'dired-load arg 'load t))

;;;###autoload
(defun dired-do-redisplay (&optional arg test-for-subdir)
  "Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing.

Dired remembers switches specified with a prefix arg, so that reverting
the buffer will not reset them.  However, using `dired-undo' to re-insert
or delete subdirectories can bypass this machinery.  Hence, you sometimes
may have to reset some subdirectory switches after a `dired-undo'.
You can reset all subdirectory switches to the default using
\\<dired-mode-map>\\[dired-reset-subdir-switches].
See Info node `(emacs)Subdir switches' for more details.

When called from Lisp, if ARG is the symbol `marked', redisplay
only the marked files, or none if no files are marked."
  ;; Moves point if the next ARG files are redisplayed.
  (interactive "P\np" dired-mode)
  (if (and test-for-subdir (dired-get-subdir))
      (let* ((dir (dired-get-subdir))
	     (switches (cdr (assoc-string dir dired-switches-alist))))
	(dired-insert-subdir
	 dir
	 (when arg
	   (read-string "Switches for listing: "
			(or switches
			    dired-subdir-switches
			    dired-actual-switches)))))
    (message "Redisplaying...")
    ;; message much faster than making dired-map-over-marks show progress
    (dired-uncache
     (if (consp dired-directory) (car dired-directory) dired-directory))
    (dired-map-over-marks (let ((fname (dired-get-filename nil t))
				;; Postpone readin hook till we map
				;; over all marked files (Bug#6810).
				(dired-after-readin-hook nil))
			    (if (not fname)
				(error "No file on this line")
			      (message "Redisplaying... %s" fname)
			      (dired-update-file-line fname)))
			  arg)
    (run-hooks 'dired-after-readin-hook)
    (dired-move-to-filename)
    (message "Redisplaying...done")))

(defun dired-reset-subdir-switches ()
  "Set `dired-switches-alist' to nil and revert Dired buffer."
  (interactive nil dired-mode)
  (setq dired-switches-alist nil)
  (revert-buffer))


(defun dired-update-file-line (file)
  ;; Delete the current line, and insert an entry for FILE.
  ;; If FILE is nil, then just delete the current line.
  ;; Keeps any marks that may be present in column one (doing this
  ;; here is faster than with dired-add-entry's optional arg).
  ;; Does not update other dired buffers.  Use dired-relist-entry for that.
  (let* ((opoint (line-beginning-position))
         (char (char-after opoint))
         (buffer-read-only))
    (delete-region opoint (progn (forward-line 1) (point)))
    (if file
        (progn
          (dired-add-entry file nil t)
          ;; Replace space by old marker without moving point.
          ;; Faster than goto+insdel inside a save-excursion?
          (when char
            (subst-char-in-region opoint (1+ opoint) ?\s char)))))
  (dired-move-to-filename))

;;;###autoload
(defun dired-add-file (filename &optional marker-char)
  (dired-fun-in-all-buffers
   (file-name-directory filename) (file-name-nondirectory filename)
   #'dired-add-entry filename marker-char))

(defvar dired-omit-mode)
(declare-function dired-omit-regexp "dired-x" ())
(defvar dired-omit-localp)

(defun dired-add-entry (filename &optional marker-char relative)
  "Add a new Dired entry for FILENAME.
Optionally mark it with MARKER-CHAR (a character, else uses
`dired-marker-char').  Note that this adds the entry `out of order'
if files are sorted by time, etc.
Skips files that match `dired-trivial-filenames'.
Exposes hidden subdirectories if a file is added there.

If `dired-x' is loaded and `dired-omit-mode' is enabled, skips
files matching `dired-omit-regexp'."
  (if (or (not (featurep 'dired-x))
	  (not dired-omit-mode)
	  ;; Avoid calling ls for files that are going to be omitted anyway.
	  (let ((omit-re (dired-omit-regexp)))
	    (or (string= omit-re "")
		(not (string-match-p omit-re
				     (cond
				      ((eq 'no-dir dired-omit-localp)
				       filename)
				      ((eq t dired-omit-localp)
				       (dired-make-relative filename))
				      (t
				       (dired-make-absolute
					filename
					(file-name-directory filename)))))))))
      ;; Do it!
      (progn
	(setq filename (directory-file-name filename))
	;; Entry is always for files, even if they happen to also be directories
	(let* ((opoint (point))
	       (cur-dir (dired-current-directory))
	       (directory (if relative cur-dir (file-name-directory filename)))
	       reason)
	  (setq filename
		(if relative
		    (file-relative-name filename directory)
		  (file-name-nondirectory filename))
		reason
		(catch 'not-found
		  (if (string= directory cur-dir)
		      (progn
			(end-of-line)
			(if (dired--hidden-p)
			    (dired-unhide-subdir))
			;; We are already where we should be, except when
			;; point is before the subdir line or its total line.
			(let ((p (dired-after-subdir-garbage cur-dir)))
			  (if (< (point) p)
			      (goto-char p))))
		    ;; else try to find correct place to insert
		    (if (dired-goto-subdir directory)
			(progn ;; unhide if necessary
			  (if (dired--hidden-p)
			      ;; Point is at end of subdir line.
			      (dired-unhide-subdir))
			  ;; found - skip subdir and `total' line
			  ;; and uninteresting files like . and ..
			  ;; This better not move into the next subdir!
			  (dired-goto-next-nontrivial-file))
		      ;; not found
		      (throw 'not-found "Subdir not found")))
		  (let (buffer-read-only opoint)
		    (beginning-of-line)
		    (setq opoint (point))
		    ;; Don't expand `.'.
		    ;; Show just the file name within directory.
		    (let ((default-directory directory))
		      (dired-insert-directory
		       directory
		       (concat dired-actual-switches " -d")
		       (list filename)))
		    (goto-char opoint)
		    ;; Put in desired marker char.
		    (when marker-char
		      (let ((dired-marker-char
			     (if (integerp marker-char) marker-char
			       dired-marker-char)))
			(dired-mark nil)))
		    ;; Compensate for a bug in ange-ftp.
		    ;; It inserts the file's absolute name, rather than
		    ;; the relative one.  That may be hard to fix since it
		    ;; is probably controlled by something in ftp.
		    (goto-char opoint)
		    (let ((inserted-name (dired-get-filename 'verbatim)))
		      (if (file-name-directory inserted-name)
			  (let (props)
			    (end-of-line)
			    (forward-char (- (length inserted-name)))
			    (setq props (text-properties-at (point)))
			    (delete-char (length inserted-name))
			    (let ((pt (point)))
			      (insert filename)
			      (set-text-properties pt (point) props))
			    (forward-char 1))
			(forward-line 1)))
		    (forward-line -1)
		    (if dired-after-readin-hook
			;; The subdir-alist is not affected...
			(save-excursion ; ...so we can run it right now:
			  (save-restriction
			    (beginning-of-line)
			    (narrow-to-region (point)
					      (line-beginning-position 2))
			    (run-hooks 'dired-after-readin-hook))))
		    (dired-move-to-filename))
		  ;; return nil if all went well
		  nil))
	  (if reason	; don't move away on failure
	      (goto-char opoint))
	  (not reason))) ; return t on success, nil else
    ;; Don't do it (dired-omit-mode).
    ;; Return t for success (perhaps we should return file-exists-p).
    t))

(defun dired-after-subdir-garbage (dir)
  ;; Return pos of first file line of DIR, skipping header and total
  ;; or wildcard lines.
  ;; Important: never moves into the next subdir.
  ;; DIR is assumed to be unhidden.
  (save-excursion
    (or (dired-goto-subdir dir) (error "This cannot happen"))
    (forward-line 1)
    (while (and (not (eolp))		; don't cross subdir boundary
		(not (dired-move-to-filename)))
      (forward-line 1))
    (point)))

;;;###autoload
(defun dired-remove-file (file)
  "Remove entry FILE on each Dired buffer.
Note this doesn't delete FILE in the file system.
See `dired-delete-file' in case you wish that."
  (dired-fun-in-all-buffers
   (file-name-directory file) (file-name-nondirectory file)
   #'dired-remove-entry file))

;;;###autoload
(defun dired-relist-file (file)
  "Create or update the line for FILE in all Dired buffers it would belong in."
  (dired-fun-in-all-buffers (file-name-directory file)
			    (file-name-nondirectory file)
			    #'dired-relist-entry file))

(defun dired-relist-entry (file)
  ;; Relist the line for FILE, or just add it if it did not exist.
  ;; FILE must be an absolute file name.
  (let (buffer-read-only marker)
    ;; If cursor is already on FILE's line delete-region will cause
    ;; save-excursion to fail because of floating makers,
    ;; moving point to beginning of line.  Sigh.
    (save-excursion
      (and (dired-goto-file file)
	   (delete-region (progn (beginning-of-line)
				 (setq marker (following-char))
				 (point))
			  (line-beginning-position 2)))
      (setq file (directory-file-name file))
      (dired-add-entry file (if (eq ?\s marker) nil marker)))))


;;; Copy, move/rename, making hard and symbolic links

(defcustom dired-backup-overwrite nil
  "If non-nil, Dired makes backups of files before overwriting them.
By default, Dired asks whether to make a backup file for a file that
is about to be overwritten, but if this variable's value is `always',
that suppresses the confirmation, and backup files are always created."
  :type '(choice (const :tag "Do not backup files to be overwritten" nil)
		 (const :tag "Always backup files to be overwritten" always)
		 (other :tag
                        "Ask whether to backup files to be overwritten" t))
  :group 'dired)

;; This is a fluid var used in dired-handle-overwrite.  It should be
;; let-bound whenever dired-copy-file etc are called.  See
;; dired-create-files for an example.
(defvar dired-overwrite-confirmed)

(defun dired-handle-overwrite (to)
  ;; Save old version of file TO that is to be overwritten.
  ;; `dired-overwrite-confirmed' and `overwrite-backup-query' are fluid vars
  ;; from dired-create-files.
  (let (backup)
    (when (and dired-backup-overwrite
	       dired-overwrite-confirmed
	       (setq backup (car (find-backup-file-name to)))
	       (or (eq 'always dired-backup-overwrite)
		   (dired-query 'overwrite-backup-query
				"Make backup for existing file `%s'? "
				to)))
      (rename-file to backup 0)	; confirm overwrite of old backup
      (dired-relist-entry backup))))

;;;###autoload
(defun dired-copy-file (from to ok-flag)
  (dired-handle-overwrite to)
  (dired-copy-file-recursive from to ok-flag dired-copy-preserve-time t
                             dired-recursive-copies dired-copy-dereference))

(declare-function make-symbolic-link "fileio.c")

(defcustom dired-create-destination-dirs nil
  "Whether Dired should create destination dirs when copying/removing files.
If nil, don't create non-existent destination directories.
If `ask', ask the user whether to create them.
If `always', create them without asking.

Also see `dired-create-destination-dirs-on-trailing-dirsep'."
  :type '(choice (const :tag "Never create non-existent dirs" nil)
		 (const :tag "Always create non-existent dirs" always)
		 (const :tag "Ask whether to create non-existent dirs" ask))
  :group 'dired
  :version "27.1")

(defcustom dired-create-destination-dirs-on-trailing-dirsep nil
  "If non-nil, consider a file name ending in a slash as a directory to create.

If this variable is non-nil and a single destination file name of
a Dired command ends in a directory separator (/), that file name
will be treated as a non-existent directory, and that directory
will be created if the variable `dired-create-destination-dirs'
says so.

For example, if both `dired-create-destination-dirs' and this
option are non-nil, renaming a directory named `old_name' to
`new_name/' (note the trailing directory separator) will
create `new_name' if it does not already exist, and will
move `old_name' into it.  By contrast, if only `new_name'
(without the trailing /) is given, or this option or
`dired-create-destination-dirs' is nil, `old_name' will be
renamed to `new_name'."
  :type '(choice
          (const :tag "Create directories when destination ends in a slash" t)
          (const :tag "Do not create destination directories" nil))
  :group 'dired
  :version "29.1")

(defun dired-maybe-create-dirs (dir)
  "Create DIR if doesn't exist according to `dired-create-destination-dirs'."
  (when (and dired-create-destination-dirs (not (file-exists-p dir)))
    (if (or (eq dired-create-destination-dirs 'always)
            (yes-or-no-p (format "Create destination dir `%s'? " dir)))
        (dired-create-directory dir))))

(defun dired-copy-file-recursive (from to ok-flag &optional
                                       preserve-time top recursive
                                       dereference)
  (when (and (eq t (file-attribute-type (file-attributes from)))
	     (file-in-directory-p to from))
    (error "Cannot copy `%s' into its subdirectory `%s'" from to))
  (let ((attrs (file-attributes from)))
    (if (and recursive
	     (eq t (file-attribute-type attrs))
	     (or (eq recursive 'always)
		 (yes-or-no-p (format "Copy %s recursively? " from))))
	(copy-directory from to preserve-time)
      (or top (dired-handle-overwrite to))
      (condition-case err
	  (if (and (not dereference)
                   (stringp (file-attribute-type attrs)))
	      ;; It is a symlink
	      (make-symbolic-link (file-attribute-type attrs) to ok-flag)
            (dired-maybe-create-dirs (file-name-directory to))
	    (copy-file from to ok-flag preserve-time))
	(file-date-error
	 (push (dired-make-relative from)
	       dired-create-files-failures)
	 (dired-log "Can't set date on %s:\n%s\n" from err))))))

(defcustom dired-vc-rename-file nil
  "Whether Dired should register file renaming in underlying VC system.
If nil, Dired renames files using `rename-file'.
If non-nil, and the renamed files are under version control,
Dired renames them using `vc-rename-file'."
  :type '(choice (const :tag "Rename all files directly" nil)
                 (const :tag "Rename versioned files via version control" t))
  :group 'dired
  :version "27.1")

;;;###autoload
(defun dired-rename-file (file newname ok-if-already-exists)
  "Rename FILE to NEWNAME.
Signal a `file-already-exists' error if a file NEWNAME already exists
unless OK-IF-ALREADY-EXISTS is non-nil."
  (let ((file-is-dir-p (file-directory-p file)))
    (dired-handle-overwrite newname)
    (dired-maybe-create-dirs (file-name-directory newname))
    (if (and dired-vc-rename-file
             (vc-backend file)
             (ignore-errors (vc-responsible-backend newname)))
        (vc-rename-file file newname)
      ;; error is caught in -create-files
      (rename-file file newname ok-if-already-exists))
    ;; Silently rename the visited file of any buffer visiting this file.
    (and (get-file-buffer file)
         (with-current-buffer (get-file-buffer file)
	   (set-visited-file-name newname nil t)))
    (dired-remove-file file)
    ;; See if it's an inserted subdir, and rename that, too.
    (when file-is-dir-p
      (dired-rename-subdir file newname))))

(defun dired-rename-subdir (from-dir to-dir)
  (setq from-dir (file-name-as-directory from-dir)
	to-dir (file-name-as-directory to-dir))
  (dired-fun-in-all-buffers from-dir nil
			    #'dired-rename-subdir-1 from-dir to-dir)
  ;; Update visited file name of all affected buffers
  (let ((expanded-from-dir (expand-file-name from-dir))
	(blist (buffer-list)))
    (while blist
      (with-current-buffer (car blist)
	(if (and buffer-file-name
		 (dired-in-this-tree-p buffer-file-name expanded-from-dir))
	    (let ((modflag (buffer-modified-p))
		  (to-file (replace-regexp-in-string
			    (concat "^" (regexp-quote from-dir))
			    to-dir
			    buffer-file-name)))
	      (set-visited-file-name to-file)
	      (set-buffer-modified-p modflag))))
      (setq blist (cdr blist)))))

(defun dired-rename-subdir-1 (dir to)
  ;; Rename DIR to TO in headerlines and dired-subdir-alist, if DIR or
  ;; one of its subdirectories is expanded in this buffer.
  (let ((expanded-dir (expand-file-name dir))
	(alist dired-subdir-alist)
	(elt nil))
    (while alist
      (setq elt (car alist)
	    alist (cdr alist))
      (if (dired-in-this-tree-p (car elt) expanded-dir)
	  ;; ELT's subdir is affected by the rename
	  (dired-rename-subdir-2 elt dir to)))
    (if (equal dir default-directory)
	;; if top level directory was renamed, lots of things have to be
	;; updated:
	(progn
	  (dired-unadvertise dir)	; we no longer dired DIR...
	  (setq default-directory to
		dired-directory (expand-file-name;; this is correct
				 ;; with and without wildcards
				 (file-name-nondirectory (if (stringp dired-directory)
                                                             dired-directory
                                                           (car dired-directory)))
				 to))
	  (let ((new-name (file-name-nondirectory
			   (directory-file-name (if (stringp dired-directory)
                                                    dired-directory
                                                  (car dired-directory))))))
	    ;; try to rename buffer, but just leave old name if new
	    ;; name would already exist (don't try appending "<%d>")
	    (or (get-buffer new-name)
		(rename-buffer new-name)))
	  ;; ... we dired TO now:
	  (dired-advertise)))))

(defun dired-rename-subdir-2 (elt dir to)
  ;; Update the headerline and dired-subdir-alist element, as well as
  ;; dired-switches-alist element, of directory described by
  ;; alist-element ELT to reflect the moving of DIR to TO.  Thus, ELT
  ;; describes either DIR itself or a subdir of DIR.
  (save-excursion
    (let ((regexp (regexp-quote (directory-file-name dir)))
	  (newtext (directory-file-name to))
	  buffer-read-only)
      (goto-char (cdr elt))
      ;; Update subdir headerline in buffer
      (if (not (looking-at dired-subdir-regexp))
	  (error "%s not found where expected - dired-subdir-alist broken?"
		 dir)
	(goto-char (match-beginning 1))
	(if (re-search-forward regexp (match-end 1) t)
	    (replace-match newtext t t)
	  (error "Expected to find `%s' in headerline of %s" dir (car elt))))
      ;; Update buffer-local dired-subdir-alist and dired-switches-alist
      (let ((cons (assoc-string (car elt) dired-switches-alist))
	    (cur-dir (dired-normalize-subdir
		      (replace-regexp-in-string regexp newtext (car elt)))))
	(setcar elt cur-dir)
	(when cons (setcar cons cur-dir))))))

;; Bound in dired-create-files
(defvar overwrite-query)
(defvar overwrite-backup-query)


;;; The basic function for half a dozen variations on cp/mv/ln/ln -s

(defun dired-create-files (file-creator operation fn-list name-constructor
					&optional marker-char)
  "Create one or more new files from a list of existing files FN-LIST.
This function also handles querying the user, updating Dired
buffers, and displaying a success or failure message.

FILE-CREATOR should be a function.  It is called once for each
file in FN-LIST, and must create a new file, querying the user
and updating Dired buffers as necessary.  It should accept three
arguments: the old file name, the new name, and an argument
OK-IF-ALREADY-EXISTS with the same meaning as in `copy-file'.

OPERATION should be a capitalized string describing the operation
performed (e.g. `Copy').  It is used for error logging.

FN-LIST is the list of files to copy (full absolute file names).

NAME-CONSTRUCTOR should be a function accepting a single
argument, the name of an old file, and returning either the
corresponding new file name or nil to skip.

If optional argument MARKER-CHAR is non-nil, mark each
newly-created file's Dired entry with the character MARKER-CHAR,
or with the current marker character if MARKER-CHAR is t."
  (let (dired-create-files-failures failures
	skipped (success-count 0) (total (length fn-list)))
    (let (to overwrite-query
	     overwrite-backup-query)	; for dired-handle-overwrite
      (dolist (from fn-list)
        ;; Position point on the current file -- this is useful if
        ;; handling a number of files to show where we're working at.
        (dired-goto-file from)
        (setq to (funcall name-constructor from))
        (if (equal to from)
            (progn
              (setq to nil)
              (dired-log "Cannot %s to same file: %s\n"
                         (downcase operation) from)))
        (if (not to)
            (setq skipped (cons (dired-make-relative from) skipped))
          (let* ((overwrite (file-exists-p to))
                 (dired-overwrite-confirmed ; for dired-handle-overwrite
                  (and overwrite
                       (let ((help-form (format-message
                                         (substitute-command-keys "\
Type \\`SPC' or \\`y' to overwrite file `%s',
\\`DEL' or \\`n' to skip to next,
\\`ESC' or \\`q' to not overwrite any of the remaining files,
\\`!' to overwrite all remaining files with no more questions.") to)))
                         (dired-query 'overwrite-query
                                      "Overwrite `%s'?" to))))
                 ;; must determine if FROM is marked before file-creator
                 ;; gets a chance to delete it (in case of a move).
                 (actual-marker-char
                  (cond  ((integerp marker-char) marker-char)
                         (marker-char (dired-file-marker from)) ; slow
                         (t nil))))
            ;; Handle the `dired-copy-file' file-creator specially
            ;; When copying a directory to another directory or
            ;; possibly to itself or one of its subdirectories.
            ;; e.g "~/foo/" => "~/test/"
            ;; or "~/foo/" =>"~/foo/"
            ;; or "~/foo/ => ~/foo/bar/")
            ;; In this case the 'name-constructor' have set the destination
            ;; TO to "~/test/foo" because the old emacs23 behavior
            ;; of `copy-directory' was to not create the subdirectory
            ;; and instead copy the contents.
            ;; With the new behavior of `copy-directory'
            ;; (similar to the `cp' shell command) we don't
            ;; need such a construction of the target directory,
            ;; so modify the destination TO to "~/test/" instead of "~/test/foo/".
            (let ((destname (file-name-directory to)))
              (when (and (file-directory-p from)
                         (file-directory-p to)
                         (eq file-creator 'dired-copy-file))
                (setq to destname))
	      ;; If DESTNAME is a subdirectory of FROM, not a symlink,
	      ;; and the method in use is copying, signal an error.
	      (and (eq t (file-attribute-type (file-attributes destname)))
		   (eq file-creator 'dired-copy-file)
		   (file-in-directory-p destname from)
		   (error "Cannot copy `%s' into its subdirectory `%s'"
			  from to)))
            ;; Check, that `dired-do-symlink' does not create symlinks
            ;; on different hosts.
            (when (and (eq file-creator 'make-symbolic-link)
                       (not (equal (file-remote-p from) (file-remote-p to))))
	      (error "Cannot symlink `%s' to `%s' on another host" from to))
            (condition-case err
                (progn
                  (funcall file-creator from to dired-overwrite-confirmed)
                  (if overwrite
                      ;; If we get here, file-creator hasn't been aborted
                      ;; and the old entry (if any) has to be deleted
                      ;; before adding the new entry.
                      (dired-remove-file to))
                  (setq success-count (1+ success-count))
                  (message "%s: %d of %d" operation success-count total)
                  (dired-add-file to actual-marker-char))
              (file-error		; FILE-CREATOR aborted
               (progn
                 (push (dired-make-relative from)
                       failures)
                 (dired-log "%s: `%s' to `%s' failed:\n%s\n"
                            operation from to err))))))))
    (cond
     (dired-create-files-failures
      (setq failures (nconc failures dired-create-files-failures))
      (dired-log-summary
       (format (ngettext "%s failed for %d file in %d requests"
			 "%s failed for %d files in %d requests"
			 (length failures))
	       operation (length failures) total)
       failures))
     (failures
      (dired-log-summary
       (format (ngettext "%s: %d of %d file failed"
			 "%s: %d of %d files failed"
			 total)
	       operation (length failures) total)
       failures))
     (skipped
      (dired-log-summary
       (format (ngettext "%s: %d of %d file skipped"
			 "%s: %d of %d files skipped"
			 total)
	       operation (length skipped) total)
       skipped))
     (t
      (message (ngettext "%s: %d file done"
			 "%s: %d files done"
			 success-count)
	       operation success-count))))
  (dired-move-to-filename))


(defcustom dired-do-revert-buffer nil
  "Automatically revert Dired buffers after `dired-do' operations.
This option controls whether to refresh the directory listing in a
Dired buffer that is the destination of one of these operations:
`dired-do-copy', `dired-do-rename', `dired-do-symlink', `dired-do-hardlink'.
If the value is t, always revert the Dired buffer updated in the result
of these operations.
If the value is a function, it is called with the destination directory name
as a single argument, and the buffer is reverted after Dired operations
if the function returns non-nil."
  :type '(choice
          (const :tag "Don't revert Dired buffers of destination directories"
                 nil)
          (const :tag "Always revert buffers of destination directories" t)
          (const :tag "Revert only Dired buffers showing local directories"
                 (lambda (dir) (not (file-remote-p dir))))
          (function :tag "Predicate function to determine whether to revert"))
  :group 'dired
  :version "28.1")

(defun dired-do-create-files (op-symbol file-creator operation arg
					&optional marker-char op1
					how-to)
  "Create a new file for each marked file.
Prompt user for a target directory in which to create the new
  files.  The target may also be a non-directory file, if only
  one file is marked.  The initial suggestion for target is the
  Dired buffer's current directory (or, if `dired-dwim-target' is
  non-nil, the current directory of a neighboring Dired window).

OP-SYMBOL is the symbol for the operation.  Function `dired-mark-pop-up'
  will determine whether pop-ups are appropriate for this OP-SYMBOL.

FILE-CREATOR and OPERATION as in `dired-create-files'.

ARG as in `dired-get-marked-files'.

Optional arg MARKER-CHAR as in `dired-create-files'.

Optional arg OP1 is an alternate form for OPERATION if there is
  only one file.

Optional arg HOW-TO determines how to treat the target.
  If HOW-TO is nil, use `file-directory-p' to determine if the
   target is a directory.  If so, the marked file(s) are created
   inside that directory.
  If HOW-TO is t, target is always treated as a plain file.
  Otherwise, HOW-TO should be a function of one argument, TARGET.
   If its return value is nil, TARGET is regarded as a plain file.
   If it return value is a list, TARGET is a generalized
    directory (e.g. some sort of archive).  The first element of
    this list must be a function with at least four arguments:
      operation - as OPERATION above.
      rfn-list  - list of the relative names for the marked files.
      fn-list   - list of the absolute names for the marked files.
      target    - the name of the target itself.
    The rest of elements of the list returned by HOW-TO are optional
    arguments for the function that is the first element of the list.

    This can be useful because by default, copying a single file
    would replace the tar file.  But this could be overridden to
    add or replace entries in the tar file.

   For any other return value, TARGET is treated as a directory."
  (or op1 (setq op1 operation))
  (let ((ret nil))
    (let* ((fn-list (dired-get-marked-files nil arg nil nil t))
	   (rfn-list (mapcar #'dired-make-relative fn-list))
	   (dired-one-file	; fluid variable inside dired-create-files
	    (and (consp fn-list) (null (cdr fn-list)) (car fn-list)))
	   (target-dir (dired-dwim-target-directory))
	   (default (and dired-one-file
		         (not dired-dwim-target) ; Bug#25609
		         (expand-file-name (file-name-nondirectory
                                            (car fn-list))
					   target-dir)))
	   (defaults (dired-dwim-target-defaults fn-list target-dir))
	   (target (expand-file-name ; fluid variable inside dired-create-files
		    (minibuffer-with-setup-hook
		        (lambda ()
                          (setq-local minibuffer-default-add-function nil)
			  (setq minibuffer-default defaults))
		      (dired-mark-read-file-name
                       (format "%s %%s %s: "
                               (if dired-one-file op1 operation)
                               (if (memq op-symbol '(symlink hardlink))
                                   ;; Linking operations create links
                                   ;; from the prompted file name; the
                                   ;; other operations copy (etc) to the
                                   ;; prompted file name.
                                   "from" "to"))
		       target-dir op-symbol arg rfn-list default))))
	   (into-dir
            (progn
              (when
                  (or
                   (not dired-one-file)
                   (and dired-create-destination-dirs-on-trailing-dirsep
                        (directory-name-p target)))
                (dired-maybe-create-dirs target))
              (cond ((null how-to)
		     ;; Allow users to change the letter case of
		     ;; a directory on a case-insensitive
		     ;; filesystem.  If we don't test these
		     ;; conditions up front, file-directory-p
		     ;; below will return t on a case-insensitive
		     ;; filesystem, and Emacs will try to move
		     ;; foo -> foo/foo, which fails.
		     (if (and (file-name-case-insensitive-p (car fn-list))
			      (eq op-symbol 'move)
			      dired-one-file
			      (string= (downcase
				        (expand-file-name (car fn-list)))
				       (downcase
				        (expand-file-name target)))
			      (not (string=
				    (file-name-nondirectory (car fn-list))
				    (file-name-nondirectory target))))
		         nil
		       (file-directory-p target)))
		    ((eq how-to t) nil)
		    (t (funcall how-to target))))))
      (setq ret
            (if (and (consp into-dir) (functionp (car into-dir)))
	        (apply (car into-dir) operation rfn-list fn-list target
                       (cdr into-dir))
              (if (not (or dired-one-file into-dir))
	          (error "Marked %s: target must be a directory: %s"
                         operation target))
              (if (and (not (file-directory-p (car fn-list)))
                       (not (file-directory-p target))
                       (directory-name-p target))
                  (error "%s: Target directory does not exist: %s"
                         operation target))
              ;; rename-file bombs when moving directories unless we do this:
              (or into-dir (setq target (directory-file-name target)))
              (prog1
                  (dired-create-files
                   file-creator operation fn-list
                   (if into-dir			; target is a directory
	               ;; This function uses fluid variable target when called
	               ;; inside dired-create-files:
	               (lambda (from)
	                 (expand-file-name (file-name-nondirectory from)
                                           target))
	             (lambda (_from) target))
                   marker-char)
                (when (or (eq dired-do-revert-buffer t)
                          (and (functionp dired-do-revert-buffer)
                               (funcall dired-do-revert-buffer target)))
                  (dired-fun-in-all-buffers (file-name-directory target) nil
                                            #'revert-buffer))))))
    (dired-post-do-command)
    ;; The return value isn't very well defined but is used by
    ;; `dired-test-bug30624'.
    ret))

;; Read arguments for a marked-files command that wants a file name,
;; perhaps popping up the list of marked files.
;; ARG is the prefix arg and indicates whether the files came from
;; marks (ARG=nil) or a repeat factor (integerp ARG).
;; If the current file was used, the list has but one element and ARG
;; does not matter. (It is non-nil, non-integer in that case, namely '(4)).
;; DEFAULT is the default value to return if the user just hits RET;
;; if it is omitted or nil, then the name of the directory is used.

(defun dired-mark-read-file-name (prompt dir op-symbol arg files
					 &optional default)
  (dired-mark-pop-up
   nil op-symbol files
   #'read-file-name
   (format prompt (dired-mark-prompt arg files)) dir default))

(defun dired-dwim-target-directories ()
  (if (functionp dired-dwim-target)
      (funcall dired-dwim-target)
    (dired-dwim-target-next)))

(defun dired-dwim-target-next (&optional all-frames)
  ;; Return directories from all next windows with dired-mode buffers.
  (mapcan (lambda (w)
            (with-current-buffer (window-buffer w)
              (when (eq major-mode 'dired-mode)
                (list (dired-current-directory)))))
          (delq (selected-window) (window-list-1
                                   (next-window nil 'nomini all-frames)
                                   'nomini all-frames))))

(defun dired-dwim-target-next-visible ()
  ;; Return directories from all next visible windows with dired-mode buffers.
  (dired-dwim-target-next 'visible))

(defun dired-dwim-target-recent ()
  ;; Return directories from all visible windows with dired-mode buffers
  ;; ordered by most-recently-used.
  (mapcar #'cdr (sort (mapcan (lambda (w)
                                (with-current-buffer (window-buffer w)
                                  (when (eq major-mode 'dired-mode)
                                    (list (cons (window-use-time w)
                                                (dired-current-directory))))))
                              (delq (selected-window)
                                    (window-list-1 nil 'nomini 'visible)))
                      (lambda (a b) (> (car a) (car b))))))

(defun dired-dwim-target-directory ()
  ;; Try to guess which target directory the user may want.
  ;; If there is a dired buffer displayed in one of the next windows,
  ;; use its current subdir, else use current subdir of this dired buffer.
  (let ((this-dir (and (eq major-mode 'dired-mode)
		       (dired-current-directory))))
    ;; non-dired buffer may want to profit from this function, e.g. vm-uudecode
    (if dired-dwim-target
	(or (car (dired-dwim-target-directories)) this-dir)
      this-dir)))

(defun dired-dwim-target-defaults (fn-list target-dir)
  ;; Return a list of default values for file-reading functions in Dired.
  ;; This list may contain directories from Dired buffers in other windows.
  ;; `fn-list' is a list of file names used to build a list of defaults.
  ;; When nil or more than one element, a list of defaults will
  ;; contain only directory names.  `target-dir' is a directory name
  ;; to exclude from the returned list, for the case when this
  ;; directory name is already presented in initial input.
  ;; For Dired operations that support `dired-dwim-target',
  ;; the argument `target-dir' should have the value returned
  ;; from `dired-dwim-target-directory'.
  (let ((dired-one-file
	 (and (consp fn-list) (null (cdr fn-list)) (car fn-list)))
	(current-dir (and (eq major-mode 'dired-mode)
			  (dired-current-directory)))
	;; Get a list of directories of visible buffers in dired-mode.
	(dired-dirs (dired-dwim-target-directories)))
    ;; Force the current dir to be the first in the list.
    (setq dired-dirs
	  (delete-dups (delq nil (cons current-dir dired-dirs))))
    ;; Remove the target dir (if specified) or the current dir from
    ;; default values, because it should be already in initial input.
    (setq dired-dirs (delete (or target-dir current-dir) dired-dirs))
    ;; Return a list of default values.
    (if dired-one-file
	;; For one file operation, provide a list that contains
	;; other directories, other directories with the appended filename
	;; and the current directory with the appended filename, e.g.
	;; 1. /TARGET-DIR/
	;; 2. /TARGET-DIR/FILENAME
	;; 3. /CURRENT-DIR/FILENAME
	(append dired-dirs
		(mapcar (lambda (dir)
			  (expand-file-name
			   (file-name-nondirectory (car fn-list)) dir))
			(reverse dired-dirs))
		(list (expand-file-name
		       (file-name-nondirectory (car fn-list))
		       (or target-dir current-dir))))
      ;; For multi-file operation, return only a list of other directories.
      dired-dirs)))


;; We use this function in `dired-create-directory' and
;; `dired-create-empty-file'; the return value is the new entry
;; in the updated Dired buffer.
(defun dired--find-topmost-parent-dir (filename)
  "Return the topmost nonexistent parent dir of FILENAME.
FILENAME is a full file name."
  (let ((try filename) new)
    (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
	    try (directory-file-name (file-name-directory try))))
    new))

;;;###autoload
(defun dired-create-directory (directory)
  "Create a directory called DIRECTORY.
Parent directories of DIRECTORY are created as needed.
If DIRECTORY already exists, signal an error."
  (interactive
   (list (read-file-name "Create directory: " (dired-current-directory)))
   dired-mode)
  (let* ((expanded (directory-file-name (expand-file-name directory)))
	 new)
    (if (file-exists-p expanded)
	(error "Cannot create directory %s: file exists" expanded))
    (setq new (dired--find-topmost-parent-dir expanded))
    (make-directory expanded t)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))

(defcustom dired-create-empty-file-in-current-directory nil
  "Whether `dired-create-empty-file' acts on the current directory.
If non-nil, `dired-create-empty-file' creates a new empty file and adds
an entry for it (or its topmost new parent directory if created) under
the current subdirectory in the Dired buffer by default (otherwise, it
adds the new file (and new subdirectories if provided) to whichever
directory the user enters at the prompt).  If nil,
`dired-create-empty-file' acts on the default directory by default."
  :type 'boolean
  :group 'dired
  :version "31.1")

;;;###autoload
(defun dired-create-empty-file (file)
  "Create an empty file called FILE.
Parent directories of FILE are created as needed.
Add an entry in the Dired buffer for the topmost new parent
directory of FILE, if created, otherwise for the new file.
If user option `dired-create-empty-file-in-current-directory' is
non-nil, act on the current subdirectory by default, otherwise act on
the default directory by default.
If FILE already exists, signal an error."
  (interactive
   (list (read-file-name "Create empty file: "
                         (and dired-create-empty-file-in-current-directory
                              (dired-current-directory))))
   dired-mode)
  (let* ((expanded (expand-file-name file))
         new)
    (if (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))
    (setq new (dired--find-topmost-parent-dir expanded))
    (make-empty-file file 'parents)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))

(defun dired-into-dir-with-symlinks (target)
  (and (file-directory-p target)
       (not (file-symlink-p target))))
;; This may not always be what you want, especially if target is your
;; home directory and it happens to be a symbolic link, as is often the
;; case with NFS and automounters.  Or if you want to make symlinks
;; into directories that themselves are only symlinks, also quite
;; common.

;; So we don't use this function as value for HOW-TO in
;; dired-do-symlink, which has the minor disadvantage of
;; making links *into* a symlinked-dir, when you really wanted to
;; *overwrite* that symlink.  In that (rare, I guess) case, you'll
;; just have to remove that symlink by hand before making your marked
;; symlinks.

(defvar dired-copy-how-to-fn nil
  "Either nil or a function used by `dired-do-copy' to determine target.
See HOW-TO argument for `dired-do-create-files' for an explanation.")

;;;###autoload
(defun dired-do-copy (&optional arg)
  "Copy all marked (or next ARG) files, or copy the current file.
ARG has to be numeric for above functionality.  See
`dired-get-marked-files' for more details.

When operating on just the current file, prompt for the new name.

When operating on multiple or marked files, prompt for a target
directory, and make the new copies in that directory, with the
same names as the original files.  The initial suggestion for the
target directory is the Dired buffer's current directory (or, if
`dired-dwim-target' is non-nil, the current directory of a
neighboring Dired window).

If `dired-copy-preserve-time' is non-nil, this command preserves
the modification time of each old file in the copy, similar to
the \"-p\" option for the \"cp\" shell command.

The `dired-keep-marker-copy' user option controls how this
command handles file marking.  The default is to mark all new
copies of files with a \"C\" mark.

This command copies symbolic links by creating new ones,
similar to the \"-d\" option for the \"cp\" shell command.
But if `dired-copy-dereference' is non-nil, the symbolic
links are dereferenced and then copied, similar to the \"-L\"
option for the \"cp\" shell command.  If ARG is a cons with
element 4 (\\[universal-argument]), the inverted value of
`dired-copy-dereference' will be used.

Also see `dired-do-revert-buffer'."
  (interactive "P" dired-mode)
  (let ((dired-recursive-copies dired-recursive-copies)
        (dired-copy-dereference (if (equal arg '(4))
                                    (not dired-copy-dereference)
                                  dired-copy-dereference)))
    (dired-do-create-files 'copy #'dired-copy-file
			   "Copy"
			   arg dired-keep-marker-copy
			   nil dired-copy-how-to-fn)))

;;;###autoload
(defun dired-do-symlink (&optional arg)
  "Make symbolic links to current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new symbolic links are made in that directory
with the same names that the files currently have.  The default
suggested for the target directory depends on the value of
`dired-dwim-target', which see.

For relative symlinks, use \\[dired-do-relsymlink].

Also see `dired-do-revert-buffer'."
  (interactive "P" dired-mode)
  (dired-do-create-files 'symlink #'make-symbolic-link
                         "Symlink" arg dired-keep-marker-symlink))

;;;###autoload
(defun dired-do-relsymlink (&optional arg)
  "Relative symlink all marked (or next ARG) files into a directory.
Otherwise make a relative symbolic link to the current file.
This creates relative symbolic links like

    foo -> ../bar/foo

not absolute ones like

    foo -> /ugly/file/name/that/may/change/any/day/bar/foo

For absolute symlinks, use \\[dired-do-symlink]."
  (interactive "P" dired-mode)
  (dired-do-create-files 'relsymlink #'dired-make-relative-symlink
                         "RelSymLink" arg dired-keep-marker-relsymlink))

(defun dired-make-relative-symlink (file1 file2 &optional ok-if-already-exists)
  "Make a symbolic link (pointing to FILE1) in FILE2.
The link is relative (if possible), for example

    \"/vol/tex/bin/foo\" \"/vol/local/bin/foo\"

results in

    \"../../tex/bin/foo\" \"/vol/local/bin/foo\""
  (interactive "FRelSymLink: \nFRelSymLink %s: \np")
  (let (name1 name2 len1 len2 (index 0) sub)
    (setq file1 (expand-file-name file1)
          file2 (expand-file-name file2)
          len1 (length file1)
          len2 (length file2))
    ;; Find common initial file name components:
    (let (next)
      (while (and (setq next (string-search "/" file1 index))
                  (< (setq next (1+ next)) (min len1 len2))
                  ;; For the comparison, both substrings must end in
                  ;; `/', so NEXT is *one plus* the result of the
                  ;; string-search.
                  ;; E.g., consider the case of linking "/tmp/a/abc"
                  ;; to "/tmp/abc" erroneously giving "/tmp/a" instead
                  ;; of "/tmp/" as common initial component
                  (string-equal (substring file1 0 next)
                                (substring file2 0 next)))
        (setq index next))
      (setq name2 file2
            sub (substring file1 0 index)
            name1 (substring file1 index)))
    (if (string-equal sub "/")
        ;; No common initial file name found
        (setq name1 file1)
      ;; Else they have a common parent directory
      (let ((tem (substring file2 index))
            (start 0)
            (count 0))
        ;; Count number of slashes we must compensate for ...
        (while (setq start (string-search "/" tem start))
          (setq count (1+ count)
                start (1+ start)))
        ;; ... and prepend a "../" for each slash found:
        (dotimes (_ count)
          (setq name1 (concat "../" name1)))))
    (make-symbolic-link
     (directory-file-name name1)        ; must not link to foo/
                                        ; (trailing slash!)
     name2 ok-if-already-exists)))

;;;###autoload
(defun dired-do-hardlink (&optional arg)
  "Add names (hard links) current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new hard links are made in that directory
with the same names that the files currently have.  The default
suggested for the target directory depends on the value of
`dired-dwim-target', which see.

Also see `dired-do-revert-buffer'."
  (interactive "P" dired-mode)
  (dired-do-create-files 'hardlink #'dired-hardlink
                         "Hardlink" arg dired-keep-marker-hardlink))

(defun dired-hardlink (file newname &optional ok-if-already-exists)
  (dired-handle-overwrite newname)
  ;; error is caught in -create-files
  (add-name-to-file file newname ok-if-already-exists)
  ;; Update the link count
  (dired-relist-file file))

;;;###autoload
(defun dired-do-rename (&optional arg)
  "Rename current file or all marked (or next ARG) files.
When renaming just the current file, you specify the new name.
When renaming multiple or marked files, you specify a directory.
This command also renames any buffers that are visiting the files.
The default suggested for the target directory depends on the value
of `dired-dwim-target', which see.

Also see `dired-do-revert-buffer'."
  (interactive "P" dired-mode)
  (when (seq-find (lambda (file)
                    (member (file-name-nondirectory file) '("." "..")))
                  (dired-get-marked-files nil arg))
    (user-error "Can't rename \".\" or \"..\" files"))
  (dired-do-create-files 'move #'dired-rename-file
			 "Move" arg dired-keep-marker-rename "Rename")
  (dired-post-do-command))


;;; Operate on files matched by regexp

(defvar rename-regexp-query)

(defun dired-do-create-files-regexp
    (file-creator operation arg regexp newname &optional whole-name marker-char)
  ;; Create a new file for each marked file using regexps.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-get-marked-files.
  ;; Matches each marked file against REGEXP and constructs the new
  ;;   filename from NEWNAME (like in function replace-match).
  ;; Optional arg WHOLE-NAME means match/replace the whole file name
  ;;   instead of only the non-directory part of the file.
  ;; Optional arg MARKER-CHAR as in dired-create-files.
  (let* ((fn-list (dired-get-marked-files nil arg))
	 (operation-prompt (concat operation " `%s' to `%s'?"))
         (rename-regexp-help-form (format-message
                                   (substitute-command-keys "\
Type \\`SPC' or \\`y' to %s one match, \\`DEL' or \\`n' to skip to next,
\\`!' to %s all remaining matches with no more questions.")
                                   (downcase operation)
                                   (downcase operation)))
	 (regexp-name-constructor
	  ;; Function to construct new filename using REGEXP and NEWNAME:
	  (if whole-name		; easy (but rare) case
	      (lambda (from)
		(let ((to (dired-string-replace-match regexp from newname))
		      ;; must bind help-form directly around call to
		      ;; dired-query
		      (help-form rename-regexp-help-form))
		  (if to
		      (and (dired-query 'rename-regexp-query
					operation-prompt
					from
					to)
			   to)
		    (dired-log "%s: %s did not match regexp %s\n"
			       operation from regexp))))
	    ;; not whole-name, replace non-directory part only
	    (lambda (from)
	      (let* ((new (dired-string-replace-match
			   regexp (file-name-nondirectory from) newname))
		     (to (and new	; nil means there was no match
			      (expand-file-name new
						(file-name-directory from))))
		     (help-form rename-regexp-help-form))
		(if to
		    (and (dired-query 'rename-regexp-query
				      operation-prompt
				      (dired-make-relative from)
				      (dired-make-relative to))
			 to)
		  (dired-log "%s: %s did not match regexp %s\n"
			     operation (file-name-nondirectory from) regexp))))))
	 rename-regexp-query)
    (dired-create-files
     file-creator operation fn-list regexp-name-constructor marker-char)))

(defun dired-mark-read-regexp (operation)
  ;; Prompt user about performing OPERATION.
  ;; Read and return list of: regexp newname arg whole-name.
  (let* ((whole-name
	  (equal 0 (prefix-numeric-value current-prefix-arg)))
	 (arg
	  (if whole-name nil current-prefix-arg))
	 (regexp
	  (read-regexp
	   (concat (if whole-name "Abs. " "") operation " from (regexp): ")
	   nil 'dired-regexp-history))
	 (newname
	  (read-string
	   (concat (if whole-name "Abs. " "") operation " " regexp " to: "))))
    (list regexp newname arg whole-name)))

;;;###autoload
(defun dired-do-rename-regexp (regexp newname &optional arg whole-name)
  "Rename selected files whose names match REGEXP to NEWNAME.

With non-zero prefix argument ARG, the command operates on the next ARG
files.  Otherwise, it operates on all the marked files, or the current
file if none are marked.

As each match is found, the user must type a character saying
  what to do with it.  For directions, type \\[help-command] at that time.
NEWNAME may contain \\=\\<n> or \\& as in `query-replace-regexp'.
REGEXP defaults to the last regexp used.

With a zero prefix arg, renaming by regexp affects the absolute file name.
Normally, only the non-directory part of the file name is used and changed."
  (interactive (dired-mark-read-regexp "Rename") dired-mode)
  (dired-do-create-files-regexp
   #'dired-rename-file
   "Rename" arg regexp newname whole-name dired-keep-marker-rename))

;;;###autoload
(defun dired-do-copy-regexp (regexp newname &optional arg whole-name)
  "Copy selected files whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "Copy") dired-mode)
  (let ((dired-recursive-copies nil))	; No recursive copies.
    (dired-do-create-files-regexp
     #'dired-copy-file
     (if dired-copy-preserve-time "Copy [-p]" "Copy")
     arg regexp newname whole-name dired-keep-marker-copy)))

;;;###autoload
(defun dired-do-hardlink-regexp (regexp newname &optional arg whole-name)
  "Hardlink selected files whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "HardLink") dired-mode)
  (dired-do-create-files-regexp
   #'add-name-to-file
   "HardLink" arg regexp newname whole-name dired-keep-marker-hardlink))

;;;###autoload
(defun dired-do-symlink-regexp (regexp newname &optional arg whole-name)
  "Symlink selected files whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "SymLink") dired-mode)
  (dired-do-create-files-regexp
   #'make-symbolic-link
   "SymLink" arg regexp newname whole-name dired-keep-marker-symlink))

;;;###autoload
(defun dired-do-relsymlink-regexp (regexp newname &optional arg whole-name)
  "RelSymlink all marked files containing REGEXP to NEWNAME.
See functions `dired-do-rename-regexp' and `dired-do-relsymlink'
for more info."
  (interactive (dired-mark-read-regexp "RelSymLink") dired-mode)
  (dired-do-create-files-regexp
   #'dired-make-relative-symlink
   "RelSymLink" arg regexp newname whole-name dired-keep-marker-relsymlink))


;;; Change case of file names

(defvar rename-non-directory-query)

(defun dired-create-files-non-directory
    (file-creator basename-constructor operation arg)
  ;; Perform FILE-CREATOR on the non-directory part of marked files
  ;; using function BASENAME-CONSTRUCTOR, with query for each file.
  ;; OPERATION like in dired-create-files, ARG as in dired-get-marked-files.
  (let (rename-non-directory-query)
    (dired-create-files
     file-creator
     operation
     (dired-get-marked-files nil arg)
     (lambda (from)
       (let ((to (concat (file-name-directory from)
			 (funcall basename-constructor
				  (file-name-nondirectory from)))))
         (and (let ((help-form (format-message
                                (substitute-command-keys "\
Type \\`SPC' or \\`y' to %s one file, \\`DEL' or \\`n' to skip to next,
\\`!' to %s all remaining matches with no more questions.")
                                (downcase operation)
                                (downcase operation))))
		(dired-query 'rename-non-directory-query
			     (concat operation " `%s' to `%s'")
			     (dired-make-relative from)
			     (dired-make-relative to)))
	      to)))
     dired-keep-marker-rename)))

(defun dired-rename-non-directory (basename-constructor operation arg)
  (dired-create-files-non-directory
   #'dired-rename-file
   basename-constructor operation arg))

;;;###autoload
(defun dired-upcase (&optional arg)
  "Rename all marked (or next ARG) files to upper case."
  (interactive "P" dired-mode)
  (dired-rename-non-directory #'upcase "Rename upcase" arg))

;;;###autoload
(defun dired-downcase (&optional arg)
  "Rename all marked (or next ARG) files to lower case."
  (interactive "P" dired-mode)
  (dired-rename-non-directory #'downcase "Rename downcase" arg))


;;; Insert subdirectory

;;;###autoload
(defun dired-maybe-insert-subdir (dirname &optional
					  switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same Dired buffer.
If it is already present, just move to it (type \\[dired-do-redisplay] to refresh),
  else inserts it at its natural place (as `ls -lR' would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output.

Dired remembers switches specified with a prefix arg, so that reverting
the buffer will not reset them.  However, using `dired-undo' to re-insert
or delete subdirectories can bypass this machinery.  Hence, you sometimes
may have to reset some subdirectory switches after a `dired-undo'.
You can reset all subdirectory switches to the default using
\\<dired-mode-map>\\[dired-reset-subdir-switches].
See Info node `(emacs)Subdir switches' for more details."
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (read-string "Switches for listing: "
			  (or dired-subdir-switches dired-actual-switches))))
   dired-mode)
  (let ((opoint (point)))
    ;; We don't need a marker for opoint as the subdir is always
    ;; inserted *after* opoint.
    (setq dirname (file-name-as-directory dirname))
    (or (and (not switches)
	     (when (dired-goto-subdir dirname)
	       (unless (dired-subdir-hidden-p dirname)
		 (dired-initial-position dirname))
	       t))
	(dired-insert-subdir dirname switches no-error-if-not-dir-p))
    ;; Push mark so that it's easy to find back.  Do this after the
    ;; insert message so that the user sees the `Mark set' message.
    (push-mark opoint)))

;;;###autoload
(defun dired-insert-subdir (dirname &optional switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same Dired buffer.
If it is already present, overwrite the previous entry;
  otherwise, insert it at its natural place (as `ls -lR' would
  have done).
With a prefix arg, you may edit the `ls' switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output."
  ;; NO-ERROR-IF-NOT-DIR-P needed for special filesystems like
  ;; Prospero where dired-ls does the right thing, but
  ;; file-directory-p has not been redefined.
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (read-string "Switches for listing: "
			  (or dired-subdir-switches dired-actual-switches))))
   dired-mode)
  (setq dirname (file-name-as-directory (expand-file-name dirname)))
  (or no-error-if-not-dir-p
      (file-directory-p dirname)
      (error  "Attempt to insert a non-directory: %s" dirname))
  (let ((elt (assoc dirname dired-subdir-alist))
	(cons (assoc-string dirname dired-switches-alist))
	(modflag (buffer-modified-p))
	(old-switches switches)
	switches-have-R mark-alist case-fold-search buffer-read-only)
    (and (not switches) cons (setq switches (cdr cons)))
    (dired-insert-subdir-validate dirname switches)
    ;; case-fold-search is nil now, so we can test for capital `R':
    (if (setq switches-have-R (and switches (string-match-p "R" switches)))
	;; avoid duplicated subdirs
	(setq mark-alist (dired-kill-tree dirname t)))
    (if elt
	;; If subdir is already present, remove it and remember its marks
	(setq mark-alist (nconc (dired-insert-subdir-del elt) mark-alist))
      (dired-insert-subdir-newpos dirname)) ; else compute new position
    (dired-insert-subdir-doupdate
     dirname elt (dired-insert-subdir-doinsert dirname switches))
    (when old-switches
      (if cons
	  (setcdr cons switches)
	(push (cons dirname switches) dired-switches-alist)))
    (when switches-have-R
      (dired-build-subdir-alist switches)
      (setq switches (string-replace "R" "" switches))
      (dolist (cur-ass dired-subdir-alist)
	(let ((cur-dir (car cur-ass)))
	  (and (dired-in-this-tree-p cur-dir dirname)
	       (let ((cur-cons (assoc-string cur-dir dired-switches-alist)))
		 (if cur-cons
		     (setcdr cur-cons switches)
		   (push (cons cur-dir switches) dired-switches-alist)))))))
    (dired-initial-position dirname)
    (save-excursion (dired-mark-remembered mark-alist))
    (restore-buffer-modified-p modflag)))

(defun dired-insert-subdir-validate (dirname &optional switches)
  ;; Check that it is valid to insert DIRNAME with SWITCHES.
  ;; Signal an error if invalid (e.g. user typed `i' on `..').
  (or (dired-in-this-tree-p dirname (expand-file-name default-directory))
      (error  "%s: Not in this directory tree" dirname))
  (let ((real-switches (or switches dired-subdir-switches)))
    (when real-switches
      (let (case-fold-search)
	(mapcar
	 (lambda (x)
	   (or (eq (null (string-match-p x real-switches))
		   (null (string-match-p x dired-actual-switches)))
	       (error
		"Can't have dirs with and without -%s switches together" x)))
	 ;; all switches that make a difference to dired-get-filename:
	 '("F" "b"))))))

(defun dired-alist-add (dir new-marker)
  ;; Add new DIR at NEW-MARKER.  Sort alist.
  (dired-alist-add-1 dir new-marker)
  (dired-alist-sort))

(defun dired-alist-sort ()
  ;; Keep the alist sorted on buffer position.
  (setq dired-subdir-alist
	(sort dired-subdir-alist
	      (lambda (elt1 elt2)
                (> (cdr elt1)
                   (cdr elt2))))))

(defun dired-kill-tree (dirname &optional remember-marks kill-root)
  "Kill all proper subdirs of DIRNAME, excluding DIRNAME itself.
Interactively, you can kill DIRNAME as well by using a prefix argument.
In interactive use, the command prompts for DIRNAME.

When called from Lisp, if REMEMBER-MARKS is non-nil, return an alist
of marked files.  If KILL-ROOT is non-nil, kill DIRNAME as well."
  (interactive "DKill tree below directory: \ni\nP" dired-mode)
  (setq dirname (file-name-as-directory (expand-file-name dirname)))
  (let ((s-alist dired-subdir-alist) dir m-alist)
    (while s-alist
      (setq dir (car (car s-alist))
	    s-alist (cdr s-alist))
      (and (or kill-root (not (string-equal dir dirname)))
	   (dired-in-this-tree-p dir dirname)
	   (dired-goto-subdir dir)
	   (setq m-alist (nconc (dired-kill-subdir remember-marks) m-alist))))
    m-alist))

(defun dired-insert-subdir-newpos (new-dir)
  ;; Find pos for new subdir, according to tree order.
  ;;(goto-char (point-max))
  (let ((alist dired-subdir-alist) elt dir new-pos)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    dir (car elt))
      (if (dired-tree-lessp dir new-dir)
	  ;; Insert NEW-DIR after DIR
	  (setq new-pos (dired-get-subdir-max elt)
		alist nil)))
    (goto-char new-pos))
  ;; want a separating newline between subdirs
  (or (eobp)
      (forward-line -1))
  (insert "\n")
  (point))

(defun dired-insert-subdir-del (element)
  ;; Erase an already present subdir (given by ELEMENT) from buffer.
  ;; Move to that buffer position.  Return a mark-alist.
  (let ((begin-marker (cdr element)))
    (goto-char begin-marker)
    ;; Are at beginning of subdir (and inside it!).  Now determine its end:
    (goto-char (dired-subdir-max))
    (or (eobp);; want a separating newline _between_ subdirs:
	(forward-char -1))
    (prog1
	(dired-remember-marks begin-marker (point))
      (delete-region begin-marker (point)))))

(defun dired-insert-subdir-doinsert (dirname switches)
  ;; Insert ls output after point.
  ;; Return the boundary of the inserted text (as list of BEG and END).
  (save-excursion
    (let ((begin (point)))
      (let ((dired-actual-switches
	     (or switches
		 dired-subdir-switches
		 (string-replace "R" "" dired-actual-switches))))
	(if (equal dirname (car (car (last dired-subdir-alist))))
	    ;; If doing the top level directory of the buffer,
	    ;; redo it as specified in dired-directory.
	    (dired-readin-insert)
	  (dired-insert-directory dirname dired-actual-switches nil nil t)))
      (list begin (point)))))

(defun dired-insert-subdir-doupdate (dirname elt beg-end)
  ;; Point is at the correct subdir alist position for ELT,
  ;; BEG-END is the subdir-region (as list of begin and end).
  (if elt				; subdir was already present
      ;; update its position (should actually be unchanged)
      (set-marker (cdr elt) (point-marker))
    (dired-alist-add dirname (point-marker)))
  ;; The hook may depend on the subdir-alist containing the just
  ;; inserted subdir, so run it after dired-alist-add:
  (if dired-after-readin-hook
      (save-excursion
	(let ((begin (nth 0 beg-end))
	      (end (nth 1 beg-end)))
	  (goto-char begin)
	  (save-restriction
	    (narrow-to-region begin end)
	    ;; hook may add or delete lines, but the subdir boundary
	    ;; marker floats
	    (run-hooks 'dired-after-readin-hook))))))

(defun dired-tree-lessp (dir1 dir2)
  ;; Lexicographic order on file name components, like `ls -lR':
  ;; DIR1 < DIR2 if DIR1 comes *before* DIR2 in an `ls -lR' listing,
  ;;   i.e., if DIR1 is a (grand)parent dir of DIR2,
  ;;   or DIR1 and DIR2 are in the same parentdir and their last
  ;;   components are string-lessp.
  ;; Thus ("/usr/" "/usr/bin") and ("/usr/a/" "/usr/b/") are tree-lessp.
  ;; string-lessp could arguably be replaced by file-newer-than-file-p
  ;;   if dired-actual-switches contained t.
  (setq dir1 (file-name-as-directory dir1)
	dir2 (file-name-as-directory dir2))
  (let ((components-1 (split-string dir1 "/"))
	(components-2 (split-string dir2 "/")))
    (while (and components-1
		components-2
		(equal (car components-1) (car components-2)))
      (setq components-1 (cdr components-1)
	    components-2 (cdr components-2)))
    (let ((c1 (car components-1))
	  (c2 (car components-2)))

      (cond ((and c1 c2)
	     (string-lessp c1 c2))
	    ((and (null c1) (null c2))
	     nil)			; they are equal, not lessp
	    ((null c1)			; c2 is a subdir of c1: c1<c2
	     t)
	    ((null c2)			; c1 is a subdir of c2: c1>c2
	     nil)
	    (t (error "This can't happen"))))))

(defun dired-split (pat str &optional limit)
  "Splitting on regexp PAT, turn string STR into a list of substrings.
Optional third arg LIMIT (>= 1) is a limit to the length of the
resulting list.
Thus, if SEP is a regexp that only matches itself,

   (mapconcat #\\='identity (dired-split SEP STRING) SEP)

is always equal to STRING."
  (declare (obsolete split-string "29.1"))
  (let* ((start (string-match pat str))
	 (result (list (substring str 0 start)))
	 (count 1)
	 (end (if start (match-end 0))))
    (if end				; else nothing left
	(while (and (or (not (integerp limit))
			(< count limit))
		    (string-match pat str end))
	  (setq start (match-beginning 0)
		count (1+ count)
		result (cons (substring str end start) result)
		end (match-end 0)
		start end)
	  ))
    (if (and (or (not (integerp limit))
		 (< count limit))
	     end)			; else nothing left
	(setq result
	      (cons (substring str end) result)))
    (nreverse result)))


;;; Moving by subdirectories

;;;###autoload
(defun dired-prev-subdir (arg &optional no-error-if-not-found no-skip)
  "Go to previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line."
  ;;(interactive "p")
  (interactive
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   ;; if on subdir start already, don't stay there!
	   (if (dired-get-subdir) 1 0)))
   dired-mode)
  (dired-next-subdir (- arg) no-error-if-not-found no-skip))

;;;###autoload
(defun dired-goto-subdir (dir)
  "Go to end of header line of inserted directory DIR in this Dired buffer.
When called interactively, prompt for the inserted subdirectory
to go to.

Return value of point on success, otherwise return nil.
The next char is \\n."
  (interactive
   (prog1				; let push-mark display its message
       (list (expand-file-name
              (completing-read "Goto inserted directory: "
                               dired-subdir-alist nil t
                               (dired-current-directory))))
     (push-mark))
   dired-mode)
  (setq dir (file-name-as-directory dir))
  (let ((elt (assoc dir dired-subdir-alist)))
    (and elt
         (goto-char (cdr elt))
	 ;; dired-subdir-hidden-p and dired-add-entry depend on point being
	 ;; at \n after this function succeeds.
	 (progn (end-of-line)
		(point)))))

;;;###autoload
(defun dired-mark-subdir-files ()
  "Mark all files except `.' and `..' in current subdirectory.
If the Dired buffer shows multiple directories, this command
marks the files listed in the subdirectory that point is in."
  (interactive nil dired-mode)
  (let ((p-min (dired-subdir-min)))
    (dired-mark-files-in-region p-min (dired-subdir-max))))

;;;###autoload
(defun dired-kill-subdir (&optional remember-marks)
  "Remove all lines of current subdirectory.
Lower levels are unaffected."
  ;; With optional REMEMBER-MARKS, return a mark-alist.
  (interactive nil dired-mode)
  (let* ((beg (dired-subdir-min))
	 (end (dired-subdir-max))
	 (modflag (buffer-modified-p))
	 (cur-dir (dired-current-directory))
	 (cons (assoc-string cur-dir dired-switches-alist))
	 buffer-read-only)
    (when (equal cur-dir (expand-file-name default-directory))
      (error "Attempt to kill top level directory"))
    (prog1
	(if remember-marks (dired-remember-marks beg end))
      (delete-region beg end)
      (if (eobp)			; don't leave final blank line
	  (delete-char -1))
      (dired-unsubdir cur-dir)
      (when cons
	(setq dired-switches-alist (delete cons dired-switches-alist)))
      (restore-buffer-modified-p modflag))))

(defun dired-unsubdir (dir)
  ;; Remove DIR from the alist
  (setq dired-subdir-alist
	(delq (assoc dir dired-subdir-alist) dired-subdir-alist)))

;;;###autoload
(defun dired-tree-up (arg)
  "Go up ARG levels in the Dired tree."
  (interactive "p" dired-mode)
  (let ((dir (dired-current-directory)))
    (while (>= arg 1)
      (setq arg (1- arg)
	    dir (file-name-directory (directory-file-name dir))))
    ;;(setq dir (expand-file-name dir))
    (or (dired-goto-subdir dir)
	(error "Cannot go up to %s - not in this tree" dir))))

;;;###autoload
(defun dired-tree-down ()
  "Go down in the Dired tree."
  (interactive nil dired-mode)
  (let ((dir (dired-current-directory)) ; has slash
	pos case-fold-search)		; filenames are case sensitive
    (let ((rest (reverse dired-subdir-alist)) elt)
      (while rest
	(setq elt (car rest)
	      rest (cdr rest))
	(if (dired-in-this-tree-p (directory-file-name (car elt)) dir)
	    (setq rest nil
		  pos (dired-goto-subdir (car elt))))))
    (if pos
	(goto-char pos)
      (error "At the bottom"))))


;;; Hiding

;;;###autoload
(defun dired-hide-subdir (arg)
  "Hide or unhide the current subdirectory and move to next directory.
Optional prefix arg is a repeat factor.
Use \\[dired-hide-all] to (un)hide all directories."
  (interactive "p" dired-mode)
  (with-silent-modifications
    (while (>=  (setq arg (1- arg)) 0)
      (let* ((cur-dir (dired-current-directory))
	     (hidden-p (dired-subdir-hidden-p cur-dir))
	     (elt (assoc cur-dir dired-subdir-alist))
	     (end-pos (1- (dired-get-subdir-max elt)))
	     buffer-read-only)
	;; keep header line visible, hide rest
	(goto-char (cdr elt))
	(end-of-line)
	(if hidden-p
	    (dired--unhide (point) end-pos)
	  (dired--hide (point) end-pos)))
      (dired-next-subdir 1 t))))

;;;###autoload
(defun dired-hide-all (&optional _ignored)
  "Hide all subdirectories, leaving only their header lines.
If there is already something hidden, make everything visible again.
Use \\[dired-hide-subdir] to (un)hide a particular subdirectory."
  (interactive "P" dired-mode)
  (with-silent-modifications
    (if (text-property-any (point-min) (point-max) 'invisible 'dired)
	(dired--unhide (point-min) (point-max))
      ;; hide
      (let ((pos (point-max)))		; pos of end of last directory
        (dolist (subdir dired-subdir-alist)
          (let ((start (cdr subdir)) ; pos of prev dir
		(end (save-excursion
		       (goto-char pos) ; current dir
		       ;; we're somewhere on current dir's line
		       (forward-line -1)
		       (point))))
            (dired--hide start end))
          (setq pos (cdr subdir))))))) ; prev dir gets current dir


;;; Search only in file names in the Dired buffer

(defcustom dired-isearch-filenames nil
  "If non-nil, Isearch in Dired buffers matches only file names.
If t, Isearch in Dired always matches only file names.
If `dwim', Isearch matches file names when initial point position is on
a file name.  Otherwise, Isearch searches the whole buffer without
restrictions."
  :type '(choice (const :tag "Search entire Dired buffer" nil)
		 (const :tag "Search only file names in Dired buffers" t)
		 (const :tag
                        "Search only file names if point starts on a file name"
                        dwim))
  :group 'dired
  :version "23.1")

(define-minor-mode dired-isearch-filenames-mode
  "Toggle file names searching on or off.
When on, Isearch skips matches outside file names using the search function
`dired-isearch-search-filenames' that matches only at file names.
When off, it uses the default search function."
  :lighter nil
  (if dired-isearch-filenames-mode
      (add-function :around (local 'isearch-search-fun-function)
                    #'dired-isearch-search-filenames
                    '((isearch-message-prefix . "filename ")))
    (remove-function (local 'isearch-search-fun-function)
                     #'dired-isearch-search-filenames))
  (when isearch-mode
    (setq isearch-success t isearch-adjusted t)
    (isearch-update)))

;;;###autoload
(defun dired-isearch-filenames-setup ()
  "Set up isearch to search in Dired file names.
Intended to be added to `isearch-mode-hook'."
  (when (or (eq dired-isearch-filenames t)
	    (and (eq dired-isearch-filenames 'dwim)
		 (get-text-property (point) 'dired-filename)))
    (define-key isearch-mode-map "\M-sff" 'dired-isearch-filenames-mode)
    (dired-isearch-filenames-mode 1)
    (add-hook 'isearch-mode-end-hook #'dired-isearch-filenames-end nil t)))

(defun dired-isearch-filenames-end ()
  "Clean up the Dired file name search after terminating isearch."
  (define-key isearch-mode-map "\M-sff" nil)
  (dired-isearch-filenames-mode -1)
  (remove-hook 'isearch-mode-end-hook #'dired-isearch-filenames-end t)
  (unless isearch-suspended
    (kill-local-variable 'dired-isearch-filenames)))

(defun dired-isearch-search-filenames (orig-fun)
  "Return the function that searches inside file names.
The returned function narrows the search to match the search string
only as part of a file name enclosed by the text property `dired-filename'.
It's intended to override the default search function."
  (isearch-search-fun-in-text-property
   (funcall orig-fun) '(dired-filename dired-symlink-filename)))

;;;###autoload
(defun dired-isearch-filenames ()
  "Search for a string using Isearch only in file names in the Dired buffer."
  (interactive nil dired-mode)
  (setq-local dired-isearch-filenames t)
  (isearch-forward nil t))

;;;###autoload
(defun dired-isearch-filenames-regexp ()
  "Search for a regexp using Isearch only in file names in the Dired buffer."
  (interactive nil dired-mode)
  (setq-local dired-isearch-filenames t)
  (isearch-forward-regexp nil t))


;;; Functions for searching in tags style among marked files

;;;###autoload
(defun dired-do-isearch ()
  "Search for a string through all marked files using Isearch."
  (interactive nil dired-mode)
  (multi-isearch-files
   (prog1 (dired-get-marked-files nil nil
                                  #'dired-nondirectory-p nil t)
     (dired-post-do-command))))

;;;###autoload
(defun dired-do-isearch-regexp ()
  "Search for a regexp through all marked files using Isearch."
  (interactive nil dired-mode)
  (prog1 (multi-isearch-files-regexp
          (dired-get-marked-files nil nil
                                  'dired-nondirectory-p nil t))
    (dired-post-do-command)))

(declare-function fileloop-continue "fileloop" ())

;;;###autoload
(defun dired-do-search (regexp)
  "Search through all marked files for a match for REGEXP.
If no files are marked, search through the file under point.

Stops when a match is found.

To continue searching for next match, use command \\[fileloop-continue]."
  (interactive "sSearch marked files (regexp): " dired-mode)
  (fileloop-initialize-search
   regexp
   (dired-get-marked-files nil nil #'dired-nondirectory-p)
   'default)
  (dired-post-do-command)
  (fileloop-continue))

;;;###autoload
(defun dired-do-query-replace-regexp (from to &optional delimited)
  "Do `query-replace-regexp' of FROM with TO, on all marked files.
As each match is found, the user must type a character saying
what to do with it.  Type SPC or `y' to replace the match,
DEL or `n' to skip and go to the next match.  For more directions,
type \\[help-command] at that time.

Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit the query-replace loop (\\[keyboard-quit], RET or q), you can
resume the query replace with the command \\[fileloop-continue]."
  (interactive
   (let ((common
	  (query-replace-read-args
	   "Query replace regexp in marked files" t t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)))
   dired-mode)
  (dolist (file (dired-get-marked-files nil nil #'dired-nondirectory-p nil t))
    (let ((buffer (get-file-buffer file)))
      (if (and buffer (with-current-buffer buffer
			buffer-read-only))
	  (error "File `%s' is visited read-only" file))))
  (dired-post-do-command)
  (fileloop-initialize-replace
   from to (dired-get-marked-files nil nil #'dired-nondirectory-p)
   (if (equal from (downcase from)) nil 'default)
   delimited)
  (fileloop-continue))

;;;###autoload
(defun dired-do-replace-regexp-as-diff (from to &optional delimited)
  "Do `replace-regexp' of FROM with TO as diff, on all marked files.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
The replacements are displayed in the buffer *replace-diff* that
you can later apply as a patch after reviewing the changes."
  (interactive
   (let ((common
          (query-replace-read-args
           "Replace regexp as diff in marked files" t t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (dired-post-do-command)
  (multi-file-replace-regexp-as-diff
   (dired-get-marked-files nil nil #'dired-nondirectory-p)
   from to delimited))

(declare-function xref-query-replace-in-results "xref")
(declare-function project--files-in-directory "project")

;;;###autoload
(defun dired-do-find-regexp (regexp)
  "Find all matches for REGEXP in all marked files.

If no files are marked, use the file under point.

For any marked directory, all of its files are searched recursively.
However, files matching `grep-find-ignored-files' and subdirectories
matching `grep-find-ignored-directories' are skipped in the marked
directories.

REGEXP should use constructs supported by your local `grep' command."
  (interactive "sSearch marked files (regexp): " dired-mode)
  (require 'grep)
  (require 'xref)
  (declare-function rgrep-find-ignored-directories "grep" (dir))
  (declare-function grep-find-ignored-files "grep" (dir))
  (let* ((marks (dired-get-marked-files nil nil nil nil t))
         (ignores (nconc (mapcar
                          #'file-name-as-directory
                          (rgrep-find-ignored-directories default-directory))
                         (grep-find-ignored-files default-directory)))
         (fetcher
          (lambda ()
            (let (files xrefs)
              (mapc
               (lambda (mark)
                 (if (file-directory-p mark)
                     (setq files (nconc
                                  (project--files-in-directory mark ignores "*")
                                  files))
                   (push mark files)))
               (reverse marks))
              (message "Searching...")
              (setq xrefs
                    (xref-matches-in-files regexp files))
              (unless xrefs
                (user-error "No matches for: %s" regexp))
              (message "Searching...done")
              xrefs))))
    (dired-post-do-command)
    (xref-show-xrefs fetcher nil)))

;;;###autoload
(defun dired-do-find-regexp-and-replace (from to)
  "Replace matches of FROM with TO, in all marked files.

As each match is found, the user must type a character saying
what to do with it.  Type SPC or `y' to replace the match,
DEL or `n' to skip and go to the next match.  For more directions,
type \\[help-command] at that time.

If no files are marked, use the file under point.

For any marked directory, matches in all of its files are replaced,
recursively.  However, files matching `grep-find-ignored-files'
and subdirectories matching `grep-find-ignored-directories' are skipped
in the marked directories.

REGEXP should use constructs supported by your local `grep' command.

Also see `query-replace' for user options that affect how this
function works."
  (interactive
   (let ((common
          (query-replace-read-args
           "Query replace regexp in marked files" t t t)))
     (list (nth 0 common) (nth 1 common)))
   dired-mode)
  (require 'xref)
  (defvar xref-show-xrefs-function)
  (defvar xref-auto-jump-to-first-xref)
  (with-current-buffer
      (let ((xref-show-xrefs-function
             ;; Some future-proofing (bug#44905).
             (custom--standard-value 'xref-show-xrefs-function))
            ;; Disable auto-jumping, it will mess up replacement logic.
            xref-auto-jump-to-first-xref)
        (dired-do-find-regexp from))
    (xref-query-replace-in-results from to)))

(defun dired-nondirectory-p (file)
  (not (file-directory-p file)))

;;;###autoload
(defun dired-show-file-type (file &optional deref-symlinks)
  "Print the type of FILE, according to the `file' command.
If you give a prefix argument \\[universal-argument] to this command, and
FILE is a symbolic link, then the command will print the type
of the target of the link instead."
  (interactive (list (dired-get-filename t) current-prefix-arg) dired-mode)
  (let (process-file-side-effects)
    (with-temp-buffer
      (if deref-symlinks
	  (process-file "file" nil t t "-L" "--" file)
	(process-file "file" nil t t "--" file))
      (when (bolp)
	(delete-char -1))
      (message "%s" (buffer-string)))))


;;; Version control from dired

(declare-function vc-dir-unmark-all-files "vc-dir")
(declare-function vc-dir-mark-files "vc-dir")

;;;###autoload
(defun dired-vc-next-action (verbose)
  "Do the next logical version control operation on marked files/directories.
The VC control operation will operate on a fileset which includes
the marked files/directories.  If no files/directories are marked, the
fileset will include the single file/directory shown on the current line.

If only regular files are in the fileset, call `vc-next-action' with
the same value of the VERBOSE argument (interactively, the prefix
argument).

If one or more directories are in the fileset, start `vc-dir' in the
root directory of the repository that includes the current directory,
with all directories in the fileset marked in the VC-Directory buffer
that were marked in the original Dired buffer.  If the current directory
doesn't belong to a VCS repository, prompt for a repository directory.
In this case, the VERBOSE argument is ignored."
  (interactive "P" dired-mode)
  (let* ((marked-files
          (dired-get-marked-files nil nil nil nil t))
         (mark-files
          (when (cl-some #'file-directory-p marked-files)
            ;; Fix deficiency of Dired by adding slash to dirs
            (mapcar (lambda (file)
                      (if (file-directory-p file)
                          (file-name-as-directory file)
                        file))
                    marked-files))))
    (dired-post-do-command)
    (if mark-files
        (let ((transient-hook (make-symbol "vc-dir-mark-files")))
          (fset transient-hook
                (lambda ()
                  (remove-hook 'vc-dir-refresh-hook transient-hook t)
                  (vc-dir-unmark-all-files t)
                  (vc-dir-mark-files mark-files)))
          (vc-dir-root)
          (add-hook 'vc-dir-refresh-hook transient-hook nil t))
      (vc-next-action verbose))))

(declare-function vc-only-files-state-and-model "vc")

;;;###autoload
(defun dired-vc-deduce-fileset
    (&optional state-model-only-files not-state-changing)
  (let* ((files (dired-get-marked-files nil nil nil nil t))
         (backend (or (vc-responsible-backend default-directory t)
                      (vc-backend-for-registration (car files)))))
    (when (and (not not-state-changing)
               (cl-some #'file-directory-p files))
      (user-error "\
State-changing VC operations on directories supported only from VC-Dir"))
    (if state-model-only-files
        (let ((only-files-list (mapcar (lambda (file)
                                         (cons file (vc-state file)))
                                       files)))
          (cl-list* backend files
                    (vc-only-files-state-and-model only-files-list
                                                   backend)))
      (list backend files))))


(provide 'dired-aux)

;; Local Variables:
;; generated-autoload-file: "dired-loaddefs.el"
;; End:

;;; dired-aux.el ends here
