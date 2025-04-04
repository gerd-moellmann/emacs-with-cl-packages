;;; tab-bar-tests.el --- Tests for tab-bar.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>

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

;;; Code:

(require 'ert)

(defun tab-bar-tests-close-other-tabs (arg)
  (tab-bar-tabs-set nil)
  (tab-rename "1")
  (tab-new) (tab-rename "2") ;; (tab-switch "2")
  (tab-new) (tab-rename "3") ;; (tab-switch "3")
  (should (eq (length (tab-bar-tabs)) 3))
  (should (equal (alist-get 'name (tab-bar--current-tab-find)) "3"))
  (tab-bar-close-other-tabs arg)
  (should (equal (alist-get 'name (tab-bar--current-tab-find))
                 (if arg (number-to-string (max 1 (min arg 3))) "3")))
  (should (eq (length (tab-bar-tabs)) 1))
  (should (eq (length tab-bar-closed-tabs) 2))
  (tab-undo)
  (tab-undo)
  (should (equal (tab-undo) "No more closed tabs to undo"))
  (should (eq (length (tab-bar-tabs)) 3))
  (should (eq (length tab-bar-closed-tabs) 0)))

(ert-deftest tab-bar-tests-close-other-tabs-default ()
  (tab-bar-tests-close-other-tabs nil)
  ;; Clean up tabs afterwards
  (tab-bar-tabs-set nil))

(ert-deftest tab-bar-tests-close-other-tabs-with-arg ()
  (dotimes (i 5) (tab-bar-tests-close-other-tabs i))
  ;; Clean up tabs afterwards
  (tab-bar-tabs-set nil))

(ert-deftest tab-bar-tests-quit-restore-window ()
  (let* ((frame-params (when noninteractive
                         '((window-system . nil)
                           (tty-type . "linux"))))
         (pop-up-frame-alist frame-params)
         (frame-auto-hide-function 'delete-frame))

    ;; 1.1. 'quit-restore-window' should delete the frame
    ;; from initial window (bug#59862)
    (progn
      (should (eq (length (frame-list)) 1))
      (other-frame-prefix)
      (info)
      (should (eq (length (frame-list)) 2))
      (should (equal (buffer-name) "*info*"))
      (view-echo-area-messages)
      (other-window 1)
      (should (eq (length (window-list)) 2))
      (should (equal (buffer-name) "*Messages*"))
      (quit-window)
      (should (eq (length (window-list)) 1))
      (should (equal (buffer-name) "*info*"))
      (quit-window)
      (should (eq (length (frame-list)) 1)))

    ;; 1.2. 'quit-restore-window' should not delete the frame
    ;; from non-initial window (bug#59862)
    (progn
      (should (eq (length (frame-list)) 1))
      (other-frame-prefix)
      (info)
      (should (eq (length (frame-list)) 2))
      (should (equal (buffer-name) "*info*"))
      (view-echo-area-messages)
      (should (eq (length (window-list)) 2))
      (should (equal (buffer-name) "*info*"))
      (quit-window)
      (should (eq (length (window-list)) 1))
      (should (eq (length (frame-list)) 2))
      ;; FIXME: uncomment (should (equal (buffer-name) "*Messages*"))
      (quit-window)
      (should (eq (length (frame-list)) 2))
      ;; Clean up the frame afterwards
      (delete-frame))

    ;; 2.1. 'quit-restore-window' should close the tab
    ;; from initial window (bug#59862)
    (progn
      (should (eq (length (tab-bar-tabs)) 1))
      (other-tab-prefix)
      (info)
      (should (eq (length (tab-bar-tabs)) 2))
      (should (equal (buffer-name) "*info*"))
      (view-echo-area-messages)
      (other-window 1)
      (should (eq (length (window-list)) 2))
      (should (equal (buffer-name) "*Messages*"))
      (quit-window)
      (should (eq (length (window-list)) 1))
      (should (equal (buffer-name) "*info*"))
      (quit-window)
      (should (eq (length (tab-bar-tabs)) 1)))

    ;; 2.2. 'quit-restore-window' should not close the tab
    ;; from non-initial window (bug#59862)
    (progn
      (should (eq (length (tab-bar-tabs)) 1))
      (other-tab-prefix)
      (info)
      (should (eq (length (tab-bar-tabs)) 2))
      (should (equal (buffer-name) "*info*"))
      (view-echo-area-messages)
      (should (eq (length (window-list)) 2))
      (should (equal (buffer-name) "*info*"))
      (quit-window)
      (should (eq (length (window-list)) 1))
      (should (eq (length (tab-bar-tabs)) 2))
      (should (equal (buffer-name) "*Messages*"))
      (quit-window)
      (should (eq (length (tab-bar-tabs)) 2))
      ;; Clean up the tab afterwards
      (tab-close))

    ;; 3. Don't delete the frame with dedicated window
    ;; from the second tab (bug#71386)
    (with-selected-frame (make-frame frame-params)
      (switch-to-buffer (generate-new-buffer "test1"))
      (tab-new)
      (switch-to-buffer (generate-new-buffer "test2"))
      (set-window-dedicated-p (selected-window) t)
      (kill-buffer)
      (should (eq (length (frame-list)) 2))
      (should (eq (length (tab-bar-tabs)) 1))
      ;; But now should delete the frame with dedicated window
      ;; from the last tab
      (set-window-dedicated-p (selected-window) t)
      (kill-buffer)
      (should (eq (length (frame-list)) 1)))

    ;; Clean up tabs afterwards
    (tab-bar-tabs-set nil)))

(provide 'tab-bar-tests)
;;; tab-bar-tests.el ends here
