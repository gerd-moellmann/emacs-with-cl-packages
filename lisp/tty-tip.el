;;; -*- lexical-binding: t; symbol-packages: t; -*-
;;; tty-tip.el --- Display help in tooltips on ttys

;; Copyright (C) 2024 Free Software Foundation, Inc.

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

;; Largely copied from show-paren.el's child frame code.

(defvar tty-tip--frame nil)

(defun tty-tip--redirect-focus ()
  "Redirect focus from child frame."
  (redirect-frame-focus tty-tip--frame (frame-parent tty-tip--frame)))

(defun tty-tip--buffer (text)
  (with-current-buffer
      (get-buffer-create " *tty-tip*")
    ;; Redirect focus to parent.
    (add-hook 'pre-command-hook #'tty-tip--delete-frame nil t)
    ;; Use an empty keymap.
    (use-local-map (make-keymap))
    (dolist (var '((mode-line-format . nil)
                   (header-line-format . nil)
                   (tab-line-format . nil)
                   (tab-bar-format . nil) ;; Emacs 28 tab-bar-format
                   (frame-title-format . "")
                   (truncate-lines . t)
                   (cursor-in-non-selected-windows . nil)
                   (cursor-type . nil)
                   (show-trailing-whitespace . nil)
                   (display-line-numbers . nil)
                   (left-fringe-width . nil)
                   (right-fringe-width . nil)
                   (left-margin-width . 0)
                   (right-margin-width . 0)
                   (fringes-outside-margins . 0)
                   (buffer-read-only . t)))
      (set (make-local-variable (car var)) (cdr var)))
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))
    (current-buffer)))

(defvar tty-tip-frame-parameters
  `((visibility . nil)
    (background-color . "lightyellow")
    (foreground-color . "black")
    (width . 0) (height . 0)
    (min-width . t) (min-height . t)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (border-width . 0)
    (child-frame-border-width . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (no-other-window . t)
    (no-delete-other-windows . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t)))

(defun tty-tip--delete-frame ()
  (when tty-tip--frame
    (delete-frame tty-tip--frame)
    (setq tty-tip--frame nil))
  (remove-hook 'post-command-hook #'tty-tip--delete-frame))

(defun tty-tip--compute-position ()
  (let* ((pos (mouse-position))
         (mouse-x (car (cdr pos)))
         (mouse-y (cdr (cdr pos)))
	 (x (+ mouse-x 1))
	 (y (+ mouse-y 1))
	 (tip-width (frame-width tty-tip--frame))
	 (tip-height (frame-height tty-tip--frame))
	 (tty-width (display-pixel-width))
	 (tty-height (display-pixel-height)))
    (when (> (+ x tip-width) tty-width)
      (setq x (max 0 (- x tip-width 1))))
    (when (> (+ y tip-height) tty-height)
      (setq y (max 0 (- y tip-height 1))))
    (cons x y)))

(defun tty-tip--show-help (text)
  "Function to add to `show-help-function'.
TEXT is the text to display.  TEXT nil means cancel the display."
  (tty-tip--delete-frame)
  (when text
    (let* ((minibuffer (minibuffer-window (window-frame)))
           (buffer (tty-tip--buffer text))
           (window-min-height 1)
           (window-min-width 1)
           after-make-frame-functions
	   (text-lines (string-lines text)))
      (tty-tip--delete-frame)
      (setq tty-tip--frame
            (make-frame
             `((parent-frame . ,(window-frame))
               (minibuffer . ,minibuffer)
               ,@tty-tip-frame-parameters)))
      (let ((win (frame-root-window tty-tip--frame)))
        (set-window-buffer win buffer)
        (set-window-dedicated-p win t)
        (set-frame-size tty-tip--frame
                        (apply #'max (mapcar #'string-width text-lines))
                        (length text-lines))
        (let* ((pos (tty-tip--compute-position))
               (x (car pos))
               (y (cdr pos)))
	  (set-frame-position tty-tip--frame x y))
        (make-frame-visible tty-tip--frame)
        (add-hook 'post-command-hook #'tty-tip--delete-frame)))))

;;;###autoload
(define-minor-mode tty-tip-mode
  "Global minor mode for displaying help in tty child frames."
  :global t :group 'help
  (unless (display-graphic-p)
    (if tty-tip-mode
	(setq show-help-function #'tty-tip--show-help)
      (setq show-help-function nil))))

(provide 'tty-tip)

;;; End
