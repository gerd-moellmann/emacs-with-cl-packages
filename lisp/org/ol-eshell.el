;;; ol-eshell.el --- Links to Working Directories in Eshell  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2025 Free Software Foundation, Inc.

;; Author: Konrad Hinsen <konrad.hinsen AT fastmail.net>

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

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'eshell)
(require 'esh-mode)
(require 'ol)

(declare-function eshell/pwd "em-dirs.el" (&rest args))

(org-link-set-parameters "eshell"
			 :follow #'org-eshell-open
			 :store #'org-eshell-store-link)

(defun org-eshell-open (link _)
  "Switch to an eshell buffer and execute a command line for LINK.
The LINK can be just a command line (executed in the default
eshell buffer) or a command line prefixed by a buffer name
followed by a colon."
  (let* ((buffer-and-command
          (if (string-match "\\([A-Za-z0-9+*-]+\\):\\(.*\\)" link)
	      (list (match-string 1 link)
		    (match-string 2 link))
            (list eshell-buffer-name link)))
         (eshell-buffer-name (car buffer-and-command))
         (command (cadr buffer-and-command)))
    (if (get-buffer eshell-buffer-name)
        (pop-to-buffer
         eshell-buffer-name
         (if (boundp 'display-comint-buffer-action) ; Emacs >= 29, <= 30
             display-comint-buffer-action
           '(display-buffer-same-window (inhibit-same-window) (category . comint))))
      (eshell))
    (goto-char (point-max))
    (eshell-kill-input)
    (insert command)
    (eshell-send-input)))

(defun org-eshell-store-link (&optional _interactive?)
  "Store eshell link.
When opened, the link switches back to the current eshell buffer and
the current working directory."
  (when (eq major-mode 'eshell-mode)
    (let* ((command (concat "cd " (eshell/pwd)))
           (link  (concat (buffer-name) ":" command)))
      (org-link-store-props
       :link (concat "eshell:" link)
       :description command))))

(provide 'ol-eshell)

;;; ol-eshell.el ends here
