;;; -*- lexical-binding: t; symbol-packages: t -*-
;;; lldbx.el --- Additional support for LLDB

;; Copyright (C) 2024 Free Software Foundation,;; Inc.

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

;; Additional support for working with LLDB. Source files being
;; debugged are real-only, and commands are invoked with
;; single-letter keys, or with a transient.

;; - display / undisplay

(require 'gud)
(require 'transient)

(defun lldbx-quit ()
  "Disable LLDBX mode in all buffers of the same session."
  (interactive)
  (let ((gud-buf gud-comint-buffer))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (and (eq gud-comint-buffer gud-buf)
		   (bound-and-true-p lldbx-mode))
	  (lldbx-mode -1))))))

(defun lldbx-goto-lldb ()
  "Switch to the buffer running LLDB in left side window."
  (interactive)
  (display-buffer gud-comint-buffer '(nil (inhibit-same-window . t))))

;; Repeat-mode will offer to repeat gut-* commands, which is not
;; helpful. Use our own symbols without the repeat property.
(defalias 'lldbx-continue 'gud-cont)
(defalias 'lldbx-finish 'gud-finish)
(defalias 'lldbx-run-to-here 'gud-until)
(defalias 'lldbx-nexti 'gud-nexti)
(defalias 'lldbx-stepi 'gud-stepi)
(defalias 'lldbx-next 'gud-next)
(defalias 'lldbx-step 'gud-step)
(defalias 'lldbx-set-breakpoint 'gud-break)
(defalias 'lldbx-delete-breakpoint 'gud-remove)
(defalias 'lldbx-down 'gud-down)
(defalias 'lldbx-up 'gud-up)
(defalias 'lldbx-pstar 'gud-pstar)
(defalias 'lldbx-print 'gud-print)
(defalias 'lldbx-print-lisp-object 'gud-pv)
(defalias 'lldbx-run 'gud-run)

;;; Some additional commands for LLDB.
(gud-def lldbx-backtrace "bt %p" nil)
(gud-def lldbx-xbacktrace "xbacktrace" nil)
(gud-def lldbx-frame "f %p" nil)
(gud-def lldbx-list-breakpoints "break list" nil)
(gud-def lldbx-kill-process "process kill" nil)
(gud-def lldbx-vars "var" nil)
(gud-def lldbx-set-temporary-breakpoint "_regexp-tbreak %f:%l" nil)

(defun lldbx-command (cmd)
  "Run an LLDB command read from the minibuffer."
  (interactive "MLLDB command: ")
  (gud-basic-call cmd))

(transient-define-prefix lldbx-transient ()
    "Gud commands plus extras for LLDB."
    [:level 1
     ["Run"
      ("c" "Continue" lldbx-continue)
      ("f" "Finish" lldbx-finish)
      ("h" "Goto here" lldbx-run-to-here)
      ("in" "Next instr" lldbx-nexti)
      ("is" "Step instr" lldbx-stepi)
      ("n" "Next" lldbx-next)
      ("s" "Step" lldbx-step)]
     ["Breakpoint"
      ("bb" "Set" lldbx-set-breakpoint)
      ("bd" "Delete" lldbx-delete-breakpoint)
      ("bl" "List" lldbx-list-breakpoints)
      ("bt" "Set temporary" lldbx-set-temporary-breakpoint)]
     ["Frame"
      ("t" "Backtrace" lldbx-backtrace)
      ("d" "Down" lldbx-down)
      ("g" "Goto frame>" lldbx-frame)
      ("u" "Up" lldbx-up)
      ("x" "xbacktrace" lldbx-xbacktrace)]
     ["Print"
      ("*" "Print *expr" lldbx-pstar)
      ("p" "Print expr" lldbx-print)
      ("v" "Variables" lldbx-vars)
      ("o" "Print Lisp var" lldbx-print-lisp-object)]
     ["Misc"
      ("k" "Kill process" lldbx-kill-process)
      ("l" "Goto LLDB buffer" lldbx-goto-lldb)
      ("y" "Run any command" lldbx-command)
      ("q" "Quit LLDBX" lldbx-quit)
      ("r" "Run process" lldbx-run)]])

;;;###autoload
(define-minor-mode lldbx-mode
  "Minor mode for read-only source buffers in LLDB."
  :lighter " ⚠️"
  :keymap '((" " . lldbx-command)
	    ("?" . lldbx-transient)
	    ("*" . lldbx-pstar)
	    ("0" . digit-argument)
	    ("1" . digit-argument)
	    ("2" . digit-argument)
	    ("3" . digit-argument)
	    ("4" . digit-argument)
	    ("5" . digit-argument)
	    ("6" . digit-argument)
	    ("7" . digit-argument)
	    ("8" . digit-argument)
	    ("9" . digit-argument)
	    ("bb" . lldbx-set-breakpoint)
	    ("bd" . lldbx-delete-breakpoint)
	    ("bl" . lldbx-list-breakpoints)
	    ("bt" . lldbx-set-temporary-breakpoint)
	    ("c" . lldbx-continue)
	    ("d" . lldbx-down)
	    ("f" . lldbx-finish)
	    ("h" . lldbx-run-to-here)
	    ("in" . lldbx-nexti)
	    ("is" . lldbx-stepi)
	    ("k" . lldbx-kill-process)
	    ("l" . lldbx-goto-lldb)
	    ("g" . lldbx-frame)
	    ("n" . lldbx-next)
	    ("o" . lldbx-print-lisp-object)
	    ("p" . lldbx-print)
	    ("q" . lldbx-quit)
	    ("r" . lldbx-run)
	    ("s" . lldbx-step)
	    ("t" . lldbx-backtrace)
	    ("u" . lldbx-up)
	    ("v" . lldbx-vars)
	    ("x" . lldbx-xbacktrace)
	    ("y" . lldbx-command))
  (setq buffer-read-only (not (null lldbx-mode))))

;; Adtivate the mode when Gud finds files.
(defun lldbx-find-file (orig-fun &rest args)
  (let ((buf (apply orig-fun args)))
    (when buf
      (with-current-buffer buf
	(unless lldbx-mode
	  (lldbx-mode 1))))
    buf))

;; Deactivate the mode when LLDB finishes
(defun lldbx-reset (orig-fun &rest args)
  (let ((val (apply orig-fun args)))
    (lldbx-quit)
    val))

;; Make Gud reuse windows.
(defun lldbx-display-line (orig-fun &rest args)
  (let* ((true-file (car args))
	 (buf (with-current-buffer gud-comint-buffer
		(gud-find-file true-file))))
    (when (and buf
	       (null (get-buffer-window buf))
	       (not (eq (current-buffer) gud-comint-buffer)))
      ;; The file is not yet displayed in a window. Make it so that it
      ;; is displayed in the selected window, so that Gud can find it.
      ;; Do this only if we are not in the Gud buffer itself.
      (set-window-buffer nil buf)))
  (apply orig-fun args))

(advice-add 'gud-find-file :around #'lldbx-find-file)
(advice-add 'gud-reset :around #'lldbx-reset)
(advice-add 'gud-display-line :around #'lldbx-display-line)

;; Gud doesn't like read-only prompts, which should be fixed, but I
;; don't want to spend time on that.
(add-hook 'lldb-mode-hook
	  #'(lambda ()
	      (setq-local comint-prompt-read-only nil)))

(provide 'lldbx)
