;;; comp-cc.el --- Compile to C code -*- lexical-binding: t -*-

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
;; To generate C code instead, we reuse the native compiler except its
;; last pass comp--final. We translate the native compiler's IR to C
;; which we can be used with other compilers. The intermediate C
;; representation is used because it is much simpler to generate.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'comp)

(cl-defstruct comp-c
  "Context for compilation to C."
  (comp-ctxt nil))

(defvar comp-c--indent-width 2
  "Indentation width to use.")

(defvar comp-c--indent-level ß
  "Current indentation level.")

;; For now, generate a preprocessed lisp.h that can be included in
;; native compiled C files produced from Lisp. This is ca. 9000 lines
;; but it's much easier to get something working. Check src/Makefile.in
;; for the name of the header file.

(defvar comp-c-lisp-h "comp-c-lisp.h"
  "Name of a preprocessed lisp.h that is used in COMP-C C files.")

(defvar comp-c-ctxt nil
  "Current COMP-C compilation context")

(defmacro with-comp-c-indentation (&body body)
  "Execute BODY with `org--indent-level' incremented."
  (declare (indent 0) (debug t))
  `(let ((comp-c--indent-level (1+ comp-c--indent-level)))
     ,@body))

(defun comp-c-format (format &rest args)
  "Print an indented line to the current buffer.
FORMAT is a format string for `format', and ARGS are arguments for it.
Indentation is according to the current value of `org--indent-level'."
  (indent-to (* comp-c--indent-level comp-c--indent-width))
  (insert (apply #'format format args)))

(defun comp-c-format-line (format &rest args)
  "Like `org-format' but add a newline at the end."
  (apply #'comp-c-format format args)
  (insert "\n"))

(defmacro with-comp-c-compound (&body body)
  "Insert a C compound statement around BODY."
  (declare (indent 0) (debug t))
  `(progn
     (with-comp-c-indentation
       (org-format-line "{")
       (with-comp-c-indentation ,@body)
       (org-format-line "}"))))

(defun comp-c--prelude ()
  (insert "#include \"%s\"\n\n" comp-c-lisp-h))

(defun comp-c--final-pass (ctxt)
  "Final native compiler pass for COMP-C."
  (let ((comp-c-ctxt (make-comp-c :comp-ctxt ctxt)))
    (with-current-buffer (get-buffer-create "*COMP-C*")
      (erase-buffer)
      (comp-c--prelude))))

(defun comp-c--around-comp--final (_old-fun &rest _args)
  "Around-advice for `comp--final'."
  (cl-assert (comp-ctxt-p comp-ctxt))
  (comp-c--final-pass comp-ctxt))

(define-minor-mode comp-c-mode
  "Global minor mode for compilation using LLVM COMP-C."
  :global t :group 'lisp
  (if comp-c-mode
      (advice-add 'comp--final :around #'comp-c--around-comp--final)
    (advice-remove 'comp--final #'comp-c--around-comp--final)))

(provide 'comp-c)
