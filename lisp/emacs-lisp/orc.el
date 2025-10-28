;;; orc.el --- LLVM ORC jit compilation of Lisp code -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

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

;; Native compilation uses the GCC jit compilation library. The Lisp
;; part of the native compiler translates byte code to an IR that is
;; translated to GCC jit IR in comp.c in a last pass (function
;; comp--final).
;;
;; For ORC, we reuse the native compiler except its last pass
;; comp--final. We translate the native compiler's IR to C which we can
;; pass to LLVM ORC. The intermediate C representation is used because
;; it is much simpler to generate.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'comp)

(cl-defstruct orc
  "Context for an ORC compilation."
  (comp-ctxt nil))

(defvar orc--indent-width 2
  "Indentation width to use.")

(defvar orc--indent-level ß
  "Current indentation level.")

;; For now, generate a preprocessed lisp.h that can be included in
;; native compiled C files produced from Lisp. This is ca. 9000 lines
;; but it's much easier to get something working.

(defvar orc-lisp-h "orc-lisp.h"
  "Name of a preprocessed lisp.h that is used in ORC C files.")

(defvar orc-ctxt nil
  "Current ORC compilation context")

(defmacro with-orc-indentation (&body body)
  "Execute BODY with `org--indent-level' incremented."
  (declare (indent 0) (debug t))
  `(let ((orc--indent-level (1+ orc--indent-level)))
     ,@body))

(defun orc-format (format &rest args)
  "Print an indented line to the current buffer.
FORMAT is a format string for `format', and ARGS are arguments for it.
Indentation is according to the current value of `org--indent-level'."
  (indent-to (* orc--indent-level orc--indent-width))
  (insert (apply #'format format args)))

(defun orc-format-line (format &rest args)
  "Like `org-format' but add a newline at the end."
  (apply #'orc-format args)
  (insert "\n"))

(defmacro with-orc-compound (&body body)
  "Insert a C compound statement around BODY."
  (declare (indent 0) (debug t))
  `(progn
     (with-orc-indentation
       (org-format-line "{")
       (with-orc-indentation ,@body)
       (org-format-line "}"))))

(defun orc--prelude ()
  (insert "#include \"%s\"\n\n" orc-lisp-h))

(defun orc--final-pass (ctxt)
  "Final native compiler pass for ORC."
  (let ((orc-ctxt (make-orc :comp-ctxt ctxt)))
    (with-current-buffer (get-buffer-create "*ORC*")
      (erase-buffer)
      (orc--prelude))))

(defun orc--around-comp--final (_old-fun &rest _args)
  "Around-advice for `comp--final'."
  (cl-assert (comp-ctxt-p comp-ctxt))
  (orc--final-pass comp-ctxt))

(define-minor-mode orc-mode
  "Global minor mode for compilation using LLVM ORC."
  :global t :group 'lisp
  (if orc-mode
      (advice-add 'comp--final :around #'orc--around-comp--final)
    (advice-remove 'comp--final #'orc--around-comp--final)))

(provide 'orc)
