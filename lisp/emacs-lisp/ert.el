;;; ert.el --- Emacs Lisp Regression Testing  -*- lexical-binding: t -*-

;; Copyright (C) 2007-2025 Free Software Foundation, Inc.

;; Author: Christian Ohler <ohler@gnu.org>
;; Keywords: lisp, tools

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

;; ERT is a tool for automated testing in Emacs Lisp.  Its main
;; features are facilities for defining and running test cases and
;; reporting the results as well as for debugging test failures
;; interactively.
;;
;; The main entry points are `ert-deftest', which is similar to
;; `defun' but defines a test, and `ert-run-tests-interactively',
;; which runs tests and offers an interactive interface for inspecting
;; results and debugging.  There is also
;; `ert-run-tests-batch-and-exit' for non-interactive use.
;;
;; The body of `ert-deftest' forms resembles a function body, but the
;; additional operators `should', `should-not', `should-error',
;; `skip-when' and `skip-unless' are available.  `should' is similar
;; to cl's `assert', but signals a different error when its condition
;; is violated that is caught and processed by ERT.  In addition, it
;; analyzes its argument form and records information that helps
;; debugging (`cl-assert' tries to do something similar when its
;; second argument SHOW-ARGS is true, but `should' is more
;; sophisticated).  For information on `should-not' and
;; `should-error', see their docstrings.  The `skip-when' and
;; `skip-unless' forms skip the test immediately, which is useful for
;; checking the test environment (like availability of features,
;; external binaries, etc).
;;
;; See ERT's Info manual `(ert) Top' as well as the docstrings for
;; more details.  To see some examples of tests written in ERT, see
;; the test suite distributed with the Emacs source distribution (in
;; the "test" directory).

;;; Code:

(require 'cl-lib)
(require 'debug)
(require 'backtrace)
(require 'ewoc)
(require 'find-func)
(require 'pp)
(require 'map)

(autoload 'xml-escape-string "xml.el")

;;; UI customization options.

(defgroup ert ()
  "ERT, the Emacs Lisp regression testing tool."
  :prefix "ert-"
  :group 'lisp)

(defcustom ert-batch-backtrace-right-margin 70
  "Maximum length of lines in ERT backtraces in batch mode.
Use nil for no limit (caution: backtrace lines can be very long)."
  :type '(choice (const :tag "No truncation" nil) integer))

(defvar ert-batch-print-length 10
  "`print-length' setting used in `ert-run-tests-batch'.

When formatting lists in test conditions, `print-length' will be
temporarily set to this value.  See also
`ert-batch-backtrace-line-length' for its effect on stack
traces.")

(defvar ert-batch-print-level 5
  "`print-level' setting used in `ert-run-tests-batch'.

When formatting lists in test conditions, `print-level' will be
temporarily set to this value.  See also
`ert-batch-backtrace-line-length' for its effect on stack
traces.")

(defvar ert-batch-backtrace-line-length t
  "Target length for lines in ERT batch backtraces.

Even modest settings for `print-length' and `print-level' can
produce extremely long lines in backtraces and lengthy delays in
forming them.  This variable governs the target maximum line
length by manipulating these two variables while printing stack
traces.  Setting this variable to t will reuse the value of
`backtrace-line-length' while printing stack traces in ERT batch
mode.  Any other value will be temporarily bound to
`backtrace-line-length' when producing stack traces in batch
mode.")

(defface ert-test-result-expected '((((class color) (background light))
                                     :background "green1")
                                    (((class color) (background dark))
                                     :background "green3"))
  "Face used for expected results in the ERT results buffer.")

(defface ert-test-result-unexpected '((((class color) (background light))
                                       :background "red1")
                                      (((class color) (background dark))
                                       :background "red3"))
  "Face used for unexpected results in the ERT results buffer.")

;;; Defining and locating tests.

;; The data structure that represents a test case.
(cl-defstruct ert-test
  (name nil)
  (documentation nil)
  (body (cl-assert nil))
  (most-recent-result nil)
  (expected-result-type ':passed)
  (tags '())
  (file-name nil))

(defun ert-test-boundp (symbol)
  "Return non-nil if SYMBOL names a test."
  (and (get symbol 'ert--test) t))

(defun ert-get-test (symbol)
  "If SYMBOL names a test, return that.  Signal an error otherwise."
  (unless (ert-test-boundp symbol) (error "No test named `%S'" symbol))
  (get symbol 'ert--test))

(defun ert-set-test (symbol definition)
  "Make SYMBOL name the test DEFINITION, and return DEFINITION."
  (when (eq symbol 'nil)
    ;; We disallow nil since `ert-test-at-point' and related functions
    ;; want to return a test name, but also need an out-of-band value
    ;; on failure.  Nil is the most natural out-of-band value; using 0
    ;; or "" or signaling an error would be too awkward.
    ;;
    ;; Note that nil is still a valid value for the `name' slot in
    ;; ert-test objects.  It designates an anonymous test.
    (error "Attempt to define a test named nil"))
  (when (and noninteractive (get symbol 'ert--test))
    ;; Make sure duplicated tests are discovered since the older test would
    ;; be ignored silently otherwise.
    (error "Test `%s' redefined (or loaded twice)" symbol))
  (define-symbol-prop symbol 'ert--test definition)
  definition)

(defun ert-make-test-unbound (symbol)
  "Make SYMBOL name no test.  Return SYMBOL."
  (cl-remprop symbol 'ert--test)
  symbol)

(defun ert--parse-keys-and-body (keys-and-body)
  "Split KEYS-AND-BODY into keyword-and-value pairs and the remaining body.

KEYS-AND-BODY should have the form of a property list, with the
exception that only keywords are permitted as keys and that the
tail -- the body -- is a list of forms that does not start with a
keyword.

Returns a two-element list containing the keys-and-values plist
and the body."
  (let ((extracted-key-accu '())
        (remaining keys-and-body))
    (while (keywordp (car-safe remaining))
      (let ((keyword (pop remaining)))
        (unless (consp remaining)
          (error "Value expected after keyword %S in %S"
                 keyword keys-and-body))
        (when (assoc keyword extracted-key-accu)
          (warn "Keyword %S appears more than once in %S" keyword
                keys-and-body))
        (push (cons keyword (pop remaining)) extracted-key-accu)))
    (setq extracted-key-accu (nreverse extracted-key-accu))
    (list (cl-loop for (key . value) in extracted-key-accu
                   collect key
                   collect value)
          remaining)))

;;;###autoload
(cl-defmacro ert-deftest (name () &body docstring-keys-and-body)
  "Define NAME (a symbol) as a test.

BODY is evaluated as a `progn' when the test is run.  It should
signal a condition on failure or just return if the test passes.

`should', `should-not', `should-error', `skip-when', and
`skip-unless' are useful for assertions in BODY.

Use `ert' to run tests interactively.

Tests that are expected to fail can be marked as such
using :expected-result.  See `ert-test-result-type-p' for a
description of valid values for RESULT-TYPE.

Macros in BODY are expanded when the test is defined, not when it
is run.  If a macro (possibly with side effects) is to be tested,
it has to be wrapped in `(eval (quote ...))'.

If NAME is already defined as a test and Emacs is running
in batch mode, an error is signaled.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] \
[:tags \\='(TAG...)] BODY...)"
  (declare (debug (&define [&name "test@" symbolp]
			   sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  (let ((documentation nil)
        (documentation-supplied-p nil))
    (when (stringp (car docstring-keys-and-body))
      (setq documentation (pop docstring-keys-and-body)
            documentation-supplied-p t))
    (cl-destructuring-bind
        ((&key (expected-result nil expected-result-supplied-p)
               (tags nil tags-supplied-p))
         body)
        (ert--parse-keys-and-body docstring-keys-and-body)
      `(cl-macrolet ((skip-when (form) `(ert--skip-when ,form))
                     (skip-unless (form) `(ert--skip-unless ,form)))
         (ert-set-test ',name
                       (make-ert-test
                        :name ',name
                        ,@(when documentation-supplied-p
                            `(:documentation ,documentation))
                        ,@(when expected-result-supplied-p
                            `(:expected-result-type ,expected-result))
                        ,@(when tags-supplied-p
                            `(:tags ,tags))
                        ;; Add `nil' after the body to enable compiler warnings
                        ;; about unused computations at the end.
                        :body (lambda () ,@body nil)
                        :file-name ,(or (macroexp-file-name) buffer-file-name)))
         ',name))))

(defvar ert--find-test-regexp
  (concat "^\\s-*(ert-deftest"
          find-function-space-re
          "%s\\(\\s-\\|$\\)")
  "The regexp the `find-function' mechanisms use for finding test definitions.")

(define-error 'ert-test-failed "Test failed")
(define-error 'ert-test-skipped "Test skipped")

(defun ert-pass ()
  "Terminate the current test and mark it passed.  Does not return."
  (throw 'ert--pass nil))

(defun ert-fail (data)
  "Terminate the current test and mark it failed.  Does not return.
DATA is displayed to the user and should state the reason of the failure."
  (signal 'ert-test-failed (list data)))

(defun ert-skip (data)
  "Terminate the current test and mark it skipped.  Does not return.
DATA is displayed to the user and should state the reason for skipping."
  (signal 'ert-test-skipped (list data)))


;;; The `should' macros.

(defvar ert--should-execution-observer nil)

(defun ert--signal-should-execution (form-description)
  "Tell the current `should' form observer (if any) about FORM-DESCRIPTION."
  (when ert--should-execution-observer
    (funcall ert--should-execution-observer form-description)))

(defun ert--special-operator-p (thing)
  "Return non-nil if THING is a symbol naming a special operator."
  (and (symbolp thing)
       (let ((definition (indirect-function thing)))
         (and (subrp definition)
              (eql (cdr (subr-arity definition)) 'unevalled)))))

;; FIXME: Code inside of here should probably be evaluated like it is
;; outside of tests, with the sole exception of error handling
(defun ert--expand-should-1 (whole form inner-expander)
  "Helper function for the `should' macro and its variants."
  (let ((form
         ;; catch macroexpansion errors
         (condition-case err
             (macroexpand-all form macroexpand-all-environment)
           (error `(signal ',(car err) ',(cdr err))))))
    (cond
     ((or (atom form) (ert--special-operator-p (car form)))
      (let ((value (gensym "value-")))
        `(let ((,value (gensym "ert-form-evaluation-aborted-")))
           ,(funcall inner-expander
                     `(setq ,value ,form)
                     `(list ',whole :form ',form :value ,value)
                     value)
           ,value)))
     (t
      (let ((fn-name (car form))
            (arg-forms (cdr form)))
        (cl-assert (or (symbolp fn-name)
                       (and (consp fn-name)
                            (eql (car fn-name) 'lambda)
                            (listp (cdr fn-name)))))
        (let ((fn (gensym "fn-"))
              (args (gensym "args-"))
              (value (gensym "value-"))
              (default-value (gensym "ert-form-evaluation-aborted-")))
          `(let* ((,fn (function ,fn-name))
                  (,args (condition-case err
                             (list ,@arg-forms)
                           (error (progn (setq ,fn #'signal)
                                         (list (car err)
                                               (cdr err)))))))
             (let ((,value ',default-value))
               ,(funcall inner-expander
                         `(setq ,value (apply ,fn ,args))
                         `(nconc (list ',whole)
                                 (list :form `(,,fn ,@,args))
                                 (unless (eql ,value ',default-value)
                                   (list :value ,value))
                                 (unless (eql ,value ',default-value)
                                   (when-let* ((-explainer-
                                                (ert--get-explainer ',fn-name)))
                                     (list :explanation
                                           (apply -explainer- ,args)))))
                         value)
               ,value))))))))

(defun ert--get-explainer (fn-name)
  (when (symbolp fn-name)
    (cl-loop for fn in (cons fn-name (function-alias-p fn-name))
             for explainer = (get fn 'ert-explainer)
             when explainer
             return explainer)))

(defun ert--expand-should (whole form inner-expander)
  "Helper function for the `should' macro and its variants.

Analyzes FORM and returns an expression that has the same
semantics under evaluation but records additional debugging
information.

INNER-EXPANDER should be a function and is called with two
arguments: INNER-FORM and FORM-DESCRIPTION-FORM, where INNER-FORM
is an expression equivalent to FORM, and FORM-DESCRIPTION-FORM is
an expression that returns a description of FORM.  INNER-EXPANDER
should return code that calls INNER-FORM and performs the checks
and error signaling specific to the particular variant of
`should'.  The code that INNER-EXPANDER returns must not call
FORM-DESCRIPTION-FORM before it has called INNER-FORM."
  (ert--expand-should-1
   whole form
   (lambda (inner-form form-description-form value-var)
     (let ((form-description (gensym "form-description-")))
       `(let (,form-description)
          ,(funcall inner-expander
                    `(unwind-protect
                         ,inner-form
                       (setq ,form-description ,form-description-form)
                       (ert--signal-should-execution ,form-description))
                    `,form-description
                    value-var))))))

(cl-defmacro should (form)
  "Evaluate FORM.  If it returns nil, abort the current test as failed.

Returns the value of FORM."
  (declare (debug t))
  (ert--expand-should `(should ,form) form
                      (lambda (inner-form form-description-form _value-var)
                        `(unless ,inner-form
                           (ert-fail ,form-description-form)))))

(cl-defmacro should-not (form)
  "Evaluate FORM.  If it returns non-nil, abort the current test as failed.

Returns nil."
  (declare (debug t))
  (ert--expand-should `(should-not ,form) form
                      (lambda (inner-form form-description-form _value-var)
                        `(unless (not ,inner-form)
                           (ert-fail ,form-description-form)))))

(defun ert--should-error-handle-error (form-description-fn
                                       condition type exclude-subtypes)
  "Helper function for `should-error'.

Determines whether CONDITION matches TYPE and EXCLUDE-SUBTYPES,
and aborts the current test as failed if it doesn't."
  (let ((signaled-conditions (get (car condition) 'error-conditions))
        (handled-conditions (pcase-exhaustive type
                              ((pred listp) type)
                              ((pred symbolp) (list type)))))
    (cl-assert signaled-conditions)
    (unless (cl-intersection signaled-conditions handled-conditions)
      (ert-fail (append
                 (funcall form-description-fn)
                 (list
                  :condition condition
                  :fail-reason (concat "the error signaled did not"
                                       " have the expected type")))))
    (when exclude-subtypes
      (unless (member (car condition) handled-conditions)
        (ert-fail (append
                   (funcall form-description-fn)
                   (list
                    :condition condition
                    :fail-reason (concat "the error signaled was a subtype"
                                         " of the expected type"))))))))

;; FIXME: The expansion will evaluate the keyword args (if any) in
;; nonstandard order.
(cl-defmacro should-error (form &rest keys &key type exclude-subtypes)
  "Evaluate FORM and check that it signals an error.

If no error was signaled, abort the test as failed and
return (ERROR-SYMBOL . DATA) from the error.

You can also match specific errors using the KEYWORD-ARGS arguments,
which is specified as keyword/argument pairs.  The following arguments
are defined:

:type TYPE -- If TYPE is non-nil, the error signaled needs to match
TYPE.  TYPE should be a list of condition names.  It can also be a
symbol, which is equivalent to a one-element list containing that
symbol.

:exclude-subtypes EXCLUDED -- If EXCLUDED is non-nil, the error matches
TYPE only if it is an element of TYPE.  If nil (the default), the error
matches TYPE if one of its condition names is an element of TYPE.

\(fn FORM &rest KEYWORD-ARGS)"
  (declare (debug t))
  (unless type (setq type ''error))
  (ert--expand-should
   `(should-error ,form ,@keys)
   form
   (lambda (inner-form form-description-form value-var)
     (let ((errorp (gensym "errorp"))
           (form-description-fn (gensym "form-description-fn-")))
       `(let ((,errorp nil)
              (,form-description-fn (lambda () ,form-description-form)))
          (condition-case -condition-
              ,inner-form
            ;; We can't use ,type here because we want to evaluate it.
            (error
             (setq ,errorp t)
             (ert--should-error-handle-error ,form-description-fn
                                             -condition-
                                             ,type ,exclude-subtypes)
             (setq ,value-var -condition-)))
          (unless ,errorp
            (ert-fail (append
                       (funcall ,form-description-fn)
                       (list
                        :fail-reason "did not signal an error")))))))))

(cl-defmacro ert--skip-when (form)
  "Evaluate FORM.  If it returns t, skip the current test.
Errors during evaluation are caught and handled like t."
  (declare (debug t))
  (ert--expand-should `(skip-when ,form) form
                      (lambda (inner-form form-description-form _value-var)
                        `(when (condition-case nil ,inner-form (t t))
                           (ert-skip ,form-description-form)))))

(cl-defmacro ert--skip-unless (form)
  "Evaluate FORM.  If it returns nil, skip the current test.
Errors during evaluation are caught and handled like nil."
  (declare (debug t))
  (ert--expand-should `(skip-unless ,form) form
                      (lambda (inner-form form-description-form _value-var)
                        `(unless (ignore-errors ,inner-form)
                           (ert-skip ,form-description-form)))))


;;; Explanation of `should' failures.

;; TODO(ohler): Rework explanations so that they are displayed in a
;; similar way to `ert-info' messages; in particular, allow text
;; buttons in explanations that give more detail or open an ediff
;; buffer.  Perhaps explanations should be reported through `ert-info'
;; rather than as part of the condition.

(defun ert--explain-format-atom (x)
  "Format the atom X for `ert--explain-equal'."
  (pcase x
    ((pred characterp) (list x (format "#x%x" x) (format "?%c" x)))
    ((pred integerp) (list x (format "#x%x" x)))
    (_ x)))

(defun ert--explain-equal-rec (a b)
  "Return a programmer-readable explanation of why A and B are not `equal'.
Return nil if they are."
  (if (not (eq (type-of a) (type-of b)))
      `(different-types ,a ,b)
    (pcase a
      ((pred consp)
       (let ((a-length (proper-list-p a))
             (b-length (proper-list-p b)))
         (if (not (eq (not a-length) (not b-length)))
             `(one-list-proper-one-improper ,a ,b)
           (if a-length
               (if (/= a-length b-length)
                   `(proper-lists-of-different-length ,a-length ,b-length
                                                      ,a ,b
                                                      first-mismatch-at
                                                      ,(cl-mismatch a b :test 'equal))
                 (cl-loop for i from 0
                          for ai in a
                          for bi in b
                          for xi = (ert--explain-equal-rec ai bi)
                          do (when xi (cl-return `(list-elt ,i ,xi)))
                          finally (cl-assert (equal a b) t)))
             (let ((car-x (ert--explain-equal-rec (car a) (car b))))
               (if car-x
                   `(car ,car-x)
                 (let ((cdr-x (ert--explain-equal-rec (cdr a) (cdr b))))
                   (if cdr-x
                       `(cdr ,cdr-x)
                     (cl-assert (equal a b) t)
                     nil))))))))
      ((pred cl-struct-p)
       (cl-loop for slot in (cl-struct-slot-info (type-of a))
                for ai across a
                for bi across b
                for xf = (ert--explain-equal-rec ai bi)
                do (when xf (cl-return `(struct-field ,(car slot) ,xf)))
                finally (cl-assert (equal a b) t)))
      ((or (pred arrayp) (pred recordp))
       ;; For mixed unibyte/multibyte string comparisons, make both multibyte.
       (when (and (stringp a)
                  (xor (multibyte-string-p a) (multibyte-string-p b)))
         (setq a (string-to-multibyte a))
         (setq b (string-to-multibyte b)))
       (if (/= (length a) (length b))
           `(arrays-of-different-length ,(length a) ,(length b)
                                        ,a ,b
                                        ,@(unless (char-table-p a)
                                            `(first-mismatch-at
                                              ,(cl-mismatch a b :test 'equal))))
         (cl-loop for i from 0
                  for ai across a
                  for bi across b
                  for xi = (ert--explain-equal-rec ai bi)
                  do (when xi (cl-return `(array-elt ,i ,xi)))
                  finally (cl-assert (equal a b) t))))
      (_
       (if (not (equal a b))
           (if (and (symbolp a) (symbolp b) (string= a b))
               `(different-symbols-with-the-same-name ,a ,b)
             `(different-atoms ,(ert--explain-format-atom a)
                               ,(ert--explain-format-atom b)))
         nil)))))

(defun ert--explain-equal (a b)
  "Explainer function for `equal'."
  ;; Do a quick comparison in C to avoid running our expensive
  ;; comparison when possible.
  (if (equal a b)
      nil
    (ert--explain-equal-rec a b)))
(put 'equal 'ert-explainer 'ert--explain-equal)

(defun ert--explain-string-equal (a b)
  "Explainer function for `string-equal'."
  ;; Convert if they are symbols.
  (if (string-equal a b)
      nil
    (let ((as (if (symbolp a) (symbol-name a) a))
          (bs (if (symbolp b) (symbol-name b) b)))
      (ert--explain-equal-rec as bs))))
(put 'string-equal 'ert-explainer 'ert--explain-string-equal)

(defun ert--significant-plist-keys (plist)
  "Return the keys of PLIST that have non-null values, in order."
  (cl-assert (evenp (length plist)) t)
  (cl-loop for (key value . rest) on plist by #'cddr
           unless (or (null value) (memq key accu)) collect key into accu
           finally (cl-return accu)))

(defun ert--plist-difference-explanation (a b)
  "Return a programmer-readable explanation of why A and B are different plists.

Returns nil if they are equivalent, i.e., have the same value for
each key, where absent values are treated as nil.  The order of
key/value pairs in each list does not matter."
  (cl-assert (evenp (length a)) t)
  (cl-assert (evenp (length b)) t)
  ;; Normalizing the plists would be another way to do this but it
  ;; requires a total ordering on all lisp objects (since any object
  ;; is valid as a text property key).  Perhaps defining such an
  ;; ordering is useful in other contexts, too, but it's a lot of
  ;; work, so let's punt on it for now.
  (let* ((keys-a (ert--significant-plist-keys a))
         (keys-b (ert--significant-plist-keys b))
         (keys-in-a-not-in-b (cl-set-difference keys-a keys-b :test 'eq))
         (keys-in-b-not-in-a (cl-set-difference keys-b keys-a :test 'eq)))
    (cl-flet ((explain-with-key (key)
                (let ((value-a (plist-get a key))
                      (value-b (plist-get b key)))
                  (cl-assert (not (equal value-a value-b)) t)
                  `(different-properties-for-key
                    ,key ,(ert--explain-equal-including-properties value-a
                                                                   value-b)))))
      (cond (keys-in-a-not-in-b
             (explain-with-key (car keys-in-a-not-in-b)))
            (keys-in-b-not-in-a
             (explain-with-key (car keys-in-b-not-in-a)))
            (t
             (cl-loop for key in keys-a
                      when (not (equal (plist-get a key) (plist-get b key)))
                      return (explain-with-key key)))))))

(defun ert--abbreviate-string (s len suffixp)
  "Shorten string S to at most LEN chars.

If SUFFIXP is non-nil, returns a suffix of S, otherwise a prefix."
  (let ((n (length s)))
    (cond ((< n len)
           s)
          (suffixp
           (substring s (- n len)))
          (t
           (substring s 0 len)))))

(defun ert--explain-equal-including-properties-rec (a b)
  "Return explanation of why A and B are not `equal-including-properties'.
Return nil if they are."
  (if (not (equal a b))
      (ert--explain-equal a b)
    (cl-assert (stringp a) t)
    (cl-assert (stringp b) t)
    (cl-assert (eql (length a) (length b)) t)
    (cl-loop for i from 0 to (length a)
             for props-a = (text-properties-at i a)
             for props-b = (text-properties-at i b)
             for difference = (ert--plist-difference-explanation
                               props-a props-b)
             do (when difference
                  (cl-return `(char ,i ,(substring-no-properties a i (1+ i))
                                    ,difference
                                    context-before
                                    ,(ert--abbreviate-string
                                      (substring-no-properties a 0 i)
                                      10 t)
                                    context-after
                                    ,(ert--abbreviate-string
                                      (substring-no-properties a (1+ i))
                                      10 nil))))
             finally (cl-assert (equal-including-properties a b) t))))

(defun ert--explain-equal-including-properties (a b)
  "Explainer function for `equal-including-properties'."
  ;; Do a quick comparison in C to avoid running our expensive
  ;; comparison when possible.
  (if (equal-including-properties a b)
      nil
    (ert--explain-equal-including-properties-rec a b)))
(put 'equal-including-properties 'ert-explainer
     'ert--explain-equal-including-properties)

(defun ert--explain-time-equal-p (a b)
  "Explainer function for `time-equal-p'.
A and B are the time values to compare."
  (declare (ftype (function (t t) list))
           (side-effect-free t))
  (unless (time-equal-p a b)
    `(different-time-values
      ,(format-time-string "%F %T.%N%z" a t)
      ,(format-time-string "%F %T.%N%z" b t)
      difference
      ,(format-time-string "%s.%N" (time-subtract a b) t))))
(function-put #'time-equal-p 'ert-explainer #'ert--explain-time-equal-p)

;;; Implementation of `ert-info'.

;; TODO(ohler): The name `info' clashes with
;; `ert--test-execution-info'.  One or both should be renamed.
(defvar ert--infos '()
  "The stack of `ert-info' infos that currently apply.

Bound dynamically.  This is a list of (PREFIX . MESSAGE) pairs.")

(cl-defmacro ert-info ((message-form &key ((:prefix prefix-form) "Info: "))
                       &body body)
  "Evaluate MESSAGE-FORM and BODY, and report the message if BODY fails.

To be used within ERT tests.  MESSAGE-FORM should evaluate to a
string that will be displayed together with the test result if
the test fails.  MESSAGE-FORM can also evaluate to a function; in
this case, it will be called when displaying the info.

PREFIX-FORM should evaluate to a string as well and is displayed
in front of the value of MESSAGE-FORM."
  (declare (debug ((form &rest [sexp form]) body))
	   (indent 1))
  `(let ((ert--infos (cons (cons ,prefix-form ,message-form) ert--infos)))
     ,@body))


;;; Facilities for running a single test.

(defvar ert-debug-on-error nil
  "Non-nil means enter debugger when a test fails or terminates with an error.")

;; The data structures that represent the result of running a test.
(cl-defstruct ert-test-result
  (messages nil)
  (should-forms nil)
  (duration 0)
  )
(cl-defstruct (ert-test-passed (:include ert-test-result)))
(cl-defstruct (ert-test-result-with-condition (:include ert-test-result))
  (condition (cl-assert nil))
  (backtrace (cl-assert nil))
  (infos (cl-assert nil)))
(cl-defstruct (ert-test-quit (:include ert-test-result-with-condition)))
(cl-defstruct (ert-test-failed (:include ert-test-result-with-condition)))
(cl-defstruct (ert-test-skipped (:include ert-test-result-with-condition)))
(cl-defstruct (ert-test-aborted-with-non-local-exit
               (:include ert-test-result)))

;; A container for the state of the execution of a single test and
;; environment data needed during its execution.
(cl-defstruct ert--test-execution-info
  (test (cl-assert nil))
  (result (cl-assert nil))
  ;; A thunk that may be called when RESULT has been set to its final
  ;; value and test execution should be terminated.  Should not
  ;; return.
  (exit-continuation (cl-assert nil))
  ;; The binding of `ert-debug-on-error' that is in effect for the
  ;; execution of the current test.  We store it to avoid being
  ;; affected by any new bindings the test itself may establish.  (I
  ;; don't remember whether this feature is important.)
  ert-debug-on-error)

(defun ert--run-test-debugger (info condition debugfun)
  "Error handler used during the test run.

This function records failures and errors and either terminates
the test silently or calls the interactive debugger, as
appropriate.

INFO is the `ert--test-execution-info' corresponding to this test run.
ERR is the error object."
  (let* ((type (cl-case (car condition)
                 ((quit) 'quit)
		 ((ert-test-skipped) 'skipped)
                 (otherwise 'failed)))
         ;; We store the backtrace in the result object for
         ;; `ert-results-pop-to-backtrace-for-test-at-point'.
         ;; This means we have to limit `print-level' and
         ;; `print-length' when printing result objects.  That
         ;; might not be worth while when we can also use
         ;; `ert-results-rerun-test-at-point-debugging-errors',
         ;; (i.e., when running interactively) but having the
         ;; backtrace ready for printing is important for batch
         ;; use.
         ;;
         ;; Grab the frames above ourselves.
         (backtrace (cdr (backtrace-get-frames debugfun)))
         (infos (reverse ert--infos)))
    (setf (ert--test-execution-info-result info)
          (cl-ecase type
            (quit
             (make-ert-test-quit :condition condition
                                 :backtrace backtrace
                                 :infos infos))
            (skipped
             (make-ert-test-skipped :condition condition
                                    :backtrace backtrace
                                    :infos infos))
            (failed
             (make-ert-test-failed :condition condition
                                   :backtrace backtrace
                                   :infos infos))))
    ;; FIXME: We should probably implement more fine-grained
    ;; control a la non-t `debug-on-error' here.
    (cond
     ((ert--test-execution-info-ert-debug-on-error info)
      ;; The `debugfun' arg tells `debug' which backtrace frame starts
      ;; the "entering the debugger" code so it can hide those frames
      ;; from the backtrace.
      (funcall debugger 'error condition :backtrace-base debugfun))
     (t))
    (funcall (ert--test-execution-info-exit-continuation info))))

(defun ert--run-test-internal (test-execution-info)
  "Low-level function to run a test according to TEST-EXECUTION-INFO.

This mainly sets up debugger-related bindings."
  (setf (ert--test-execution-info-ert-debug-on-error test-execution-info)
        ert-debug-on-error)
  (catch 'ert--pass
    ;; For now, each test gets its own temp buffer and its own
    ;; window excursion, just to be safe.  If this turns out to be
    ;; too expensive, we can remove it.
    (with-temp-buffer
      (save-window-excursion
        (let ((lexical-binding t) ;;FIXME: Why?
              (ert--infos '()))
          (letrec ((debugfun (lambda (err)
                               (ert--run-test-debugger test-execution-info
                                                       err debugfun))))
            (handler-bind (((error quit) debugfun))
              (funcall (ert-test-body (ert--test-execution-info-test
                                       test-execution-info))))))))
    (ert-pass))
  (setf (ert--test-execution-info-result test-execution-info)
        (make-ert-test-passed))
  nil)

(defun ert--force-message-log-buffer-truncation ()
  "Immediately truncate *Messages* buffer according to `message-log-max'.

This can be useful after reducing the value of `message-log-max'."
  (with-current-buffer (messages-buffer)
    ;; This is a reimplementation of this part of message_dolog() in xdisp.c:
    ;; if (FIXNATP (Vmessage_log_max))
    ;;   {
    ;;     scan_newline (Z, Z_BYTE, BEG, BEG_BYTE,
    ;;                   -XFIXNAT (Vmessage_log_max) - 1, false);
    ;;     del_range_both (BEG, BEG_BYTE, PT, PT_BYTE, false);
    ;;   }
    (when (natnump message-log-max)
      (let ((begin (point-min))
            (end (save-excursion
                   (goto-char (point-max))
                   (forward-line (- message-log-max))
                   (point)))
            (inhibit-read-only t))
        (delete-region begin end)))))

(defvar ert--running-tests nil
  "List of tests that are currently in execution.

This list is empty while no test is running, has one element
while a test is running, two elements while a test run from
inside a test is running, etc.  The list is in order of nesting,
innermost test first.

The elements are of type `ert-test'.")

(defun ert-run-test (ert-test)
  "Run ERT-TEST.

Returns the result and stores it in ERT-TEST's `most-recent-result' slot."
  (setf (ert-test-most-recent-result ert-test) nil)
  (cl-block error
    (let ((begin-marker
           (with-current-buffer (messages-buffer)
             (point-max-marker))))
      (unwind-protect
          (let ((info (make-ert--test-execution-info
                       :test ert-test
                       :result
                       (make-ert-test-aborted-with-non-local-exit)
                       :exit-continuation (lambda ()
                                            (cl-return-from error nil))))
                (should-form-accu (list)))
            (unwind-protect
                (let ((ert--should-execution-observer
                       (lambda (form-description)
                         (push form-description should-form-accu)))
                      (message-log-max t)
                      (ert--running-tests (cons ert-test ert--running-tests)))
                  (ert--run-test-internal info))
              (let ((result (ert--test-execution-info-result info)))
                (setf (ert-test-result-messages result)
                      (with-current-buffer (messages-buffer)
                        (buffer-substring begin-marker (point-max))))
                (ert--force-message-log-buffer-truncation)
                (setq should-form-accu (nreverse should-form-accu))
                (setf (ert-test-result-should-forms result)
                      should-form-accu)
                (setf (ert-test-most-recent-result ert-test) result))))
        (set-marker begin-marker nil))))
  (ert-test-most-recent-result ert-test))

(defun ert-running-test ()
  "Return the top-level test currently executing."
  (car (last ert--running-tests)))


;;; Test selectors.

(defun ert-test-result-type-p (result result-type)
  "Return non-nil if RESULT matches type RESULT-TYPE.

Valid result types:

nil -- Never matches.
t -- Always matches.
:failed, :passed, :skipped -- Matches corresponding results.
\(and TYPES...) -- Matches if all TYPES match.
\(or TYPES...) -- Matches if some TYPES match.
\(not TYPE) -- Matches if TYPE does not match.
\(satisfies PREDICATE) -- Matches if PREDICATE returns true when called with
                           RESULT."
  ;; It would be easy to add `member' and `eql' types etc., but I
  ;; haven't bothered yet.
  (pcase-exhaustive result-type
    ('nil nil)
    ('t t)
    (:failed (ert-test-failed-p result))
    (:passed (ert-test-passed-p result))
    (:skipped (ert-test-skipped-p result))
    (`(,operator . ,operands)
     (cl-ecase operator
       (and
        (cl-case (length operands)
          (0 t)
          (t
           (and (ert-test-result-type-p result (car operands))
                (ert-test-result-type-p result `(and ,@(cdr operands)))))))
       (or
        (cl-case (length operands)
          (0 nil)
          (t
           (or (ert-test-result-type-p result (car operands))
               (ert-test-result-type-p result `(or ,@(cdr operands)))))))
       (not
        (cl-assert (eql (length operands) 1))
        (not (ert-test-result-type-p result (car operands))))
       (satisfies
        (cl-assert (eql (length operands) 1))
        (funcall (car operands) result))))))

(defun ert-test-result-expected-p (test result)
  "Return non-nil if TEST's expected result type matches RESULT."
  (or
   (ert-test-result-type-p result :skipped)
   (ert-test-result-type-p result (ert-test-expected-result-type test))))

(defun ert-select-tests (selector universe)
  "Return a list of tests that match SELECTOR.

UNIVERSE specifies the set of tests to select from; it should be a list
of tests, or t, which refers to all tests named by symbols in `obarray'.

Valid SELECTORs:

nil  -- Selects the empty set.
t    -- Selects all of UNIVERSE.  If UNIVERSE is t, selects all tests.
:new -- Selects all tests that have not been run yet.
:failed, :passed       -- Select tests according to their most recent result.
:expected, :unexpected -- Select tests according to their most recent result.
a string -- A regular expression selecting all tests with matching names.
a test   -- (i.e., an object of the `ert-test' data-type) Selects that test.
a symbol -- Selects the test named by the symbol, signals an
    `ert-test-unbound' error if no such test.
\(member TESTS...) -- Selects the elements of TESTS, a list of tests
    or symbols naming tests.
\(eql TEST) -- Selects TEST, a test or a symbol naming a test.
\(and SELECTORS...) -- Selects the tests that match all SELECTORS.
\(or SELECTORS...)  -- Selects the tests that match any of the SELECTORS.
\(not SELECTOR)     -- Selects all tests that do not match SELECTOR.
\(tag TAG) -- Selects all tests that have TAG on their tags list.
    A tag is an arbitrary label you can apply when you define a test.
\(satisfies PREDICATE) -- Selects all tests that satisfy PREDICATE.
    PREDICATE is a function that takes an ert-test object as argument,
    and returns non-nil if it is selected.

Only selectors that require a superset of tests, such
as (satisfies ...), strings, :new, etc. make use of UNIVERSE.
Selectors that do not, such as (member ...), just return the
set implied by them without checking whether it is really
contained in UNIVERSE."
  ;; This code needs to match the cases in
  ;; `ert--insert-human-readable-selector'.
  (pcase-exhaustive selector
    ('nil nil)
    ('t (pcase-exhaustive universe
          ((pred listp) universe)
          (`t (ert-select-tests "" universe))))
    (:new (ert-select-tests
           `(satisfies ,(lambda (test)
                          (null (ert-test-most-recent-result test))))
           universe))
    (:failed (ert-select-tests
              `(satisfies ,(lambda (test)
                             (ert-test-result-type-p
                              (ert-test-most-recent-result test)
                              ':failed)))
              universe))
    (:passed (ert-select-tests
              `(satisfies ,(lambda (test)
                             (ert-test-result-type-p
                              (ert-test-most-recent-result test)
                              ':passed)))
              universe))
    (:expected (ert-select-tests
                `(satisfies
                  ,(lambda (test)
                     (ert-test-result-expected-p
                      test
                      (ert-test-most-recent-result test))))
                universe))
    (:unexpected (ert-select-tests '(not :expected) universe))
    ((pred stringp)
     (pcase-exhaustive universe
       (`t (mapcar #'ert-get-test
                   (apropos-internal selector #'ert-test-boundp)))
       ((pred listp)
        (cl-remove-if-not (lambda (test)
                            (and (ert-test-name test)
                                 (string-match selector
                                               (symbol-name
                                                (ert-test-name test)))))
                          universe))))
    ((pred ert-test-p) (list selector))
    ((pred symbolp)
     (unless (ert-test-boundp selector)
       (signal 'ert-test-unbound (list selector)))
     (list (ert-get-test selector)))
    (`(member . ,operands)
     (mapcar (lambda (purported-test)
               (pcase-exhaustive purported-test
                 ((pred symbolp)
                  (unless (ert-test-boundp purported-test)
                    (signal 'ert-test-unbound
                            (list purported-test)))
                  (ert-get-test purported-test))
                 ((pred ert-test-p) purported-test)))
             operands))
    (`(eql ,operand)
     (ert-select-tests `(member ,operand) universe))
    ;; Do these definitions of AND, NOT and OR satisfy de Morgan's
    ;; laws?  Should they?
    (`(and)
     (ert-select-tests 't universe))
    (`(and ,first . ,rest)
     (ert-select-tests `(and ,@rest)
                       (ert-select-tests first universe)))
    (`(not ,operand)
     (let ((all-tests (ert-select-tests 't universe)))
       (cl-set-difference all-tests
                          (ert-select-tests operand all-tests))))
    (`(or)
     (ert-select-tests 'nil universe))
    (`(or ,first . ,rest)
     (cl-union (ert-select-tests first universe)
               (ert-select-tests `(or ,@rest) universe)))
    (`(tag ,tag)
     (ert-select-tests `(satisfies
                         ,(lambda (test)
                            (member tag (ert-test-tags test))))
                       universe))
    (`(satisfies ,predicate)
     (cl-remove-if-not predicate
                       (ert-select-tests 't universe)))))

(define-error 'ert-test-unbound "ERT test is unbound")

(defun ert--insert-human-readable-selector (selector)
  "Insert a human-readable presentation of SELECTOR into the current buffer."
  ;; This is needed to avoid printing the (huge) contents of the
  ;; `backtrace' slot of the result objects in the
  ;; `most-recent-result' slots of test case objects in (eql ...) or
  ;; (member ...) selectors.
  (cl-labels ((rec (selector)
                ;; This code needs to match the cases in
                ;; `ert-select-tests'.
                (pcase-exhaustive selector
                  ((or
                    ;; 'nil 't :new :failed :passed :expected :unexpected
                    (pred stringp)
                    (pred symbolp))
                   selector)
                  ((pred ert-test-p)
                   (if (ert-test-name selector)
                       (make-symbol (format "<%S>" (ert-test-name selector)))
                     (make-symbol "<unnamed test>")))
                  (`(,operator . ,operands)
                   (pcase operator
                     ((or 'member 'eql 'and 'not 'or)
                      `(,operator ,@(mapcar #'rec operands)))
                     ((or 'tag 'satisfies)
                      selector))))))
    (insert (format "%S" (rec selector)))))


;;; Facilities for running a whole set of tests.

;; The data structure that contains the set of tests being executed
;; during one particular test run, their results, the state of the
;; execution, and some statistics.
;;
;; The data about results and expected results of tests may seem
;; redundant here, since the test objects also carry such information.
;; However, the information in the test objects may be more recent, it
;; may correspond to a different test run.  We need the information
;; that corresponds to this run in order to be able to update the
;; statistics correctly when a test is re-run interactively and has a
;; different result than before.
(cl-defstruct ert--stats
  (selector (cl-assert nil))
  ;; The tests, in order.
  (tests (cl-assert nil) :type vector)
  ;; A map of test names (or the test objects themselves for unnamed
  ;; tests) to indices into the `tests' vector.
  (test-map (cl-assert nil) :type hash-table)
  ;; The results of the tests during this run, in order.
  (test-results (cl-assert nil) :type vector)
  ;; The start times of the tests, in order, as reported by
  ;; `current-time'.
  (test-start-times (cl-assert nil) :type vector)
  ;; The end times of the tests, in order, as reported by
  ;; `current-time'.
  (test-end-times (cl-assert nil) :type vector)
  (passed-expected 0)
  (passed-unexpected 0)
  (failed-expected 0)
  (failed-unexpected 0)
  (skipped 0)
  (start-time nil)
  (end-time nil)
  (aborted-p nil)
  (current-test nil)
  ;; The time at or after which the next redisplay should occur, as a
  ;; float.
  (next-redisplay 0.0))

(defun ert-stats-completed-expected (stats)
  "Return the number of tests in STATS that had expected results."
  (+ (ert--stats-passed-expected stats)
     (ert--stats-failed-expected stats)))

(defun ert-stats-completed-unexpected (stats)
  "Return the number of tests in STATS that had unexpected results."
  (+ (ert--stats-passed-unexpected stats)
     (ert--stats-failed-unexpected stats)))

(defun ert-stats-skipped (stats)
  "Number of tests in STATS that have skipped."
  (ert--stats-skipped stats))

(defun ert-stats-completed (stats)
  "Number of tests in STATS that have run so far."
  (+ (ert-stats-completed-expected stats)
     (ert-stats-completed-unexpected stats)
     (ert-stats-skipped stats)))

(defun ert-stats-total (stats)
  "Number of tests in STATS, regardless of whether they have run yet."
  (length (ert--stats-tests stats)))

;; The stats object of the current run, dynamically bound.  This is
;; used for the mode line progress indicator.
(defvar ert--current-run-stats nil)

(defun ert--stats-test-key (test)
  "Return the key used for TEST in the test map of ert--stats objects.

Returns the name of TEST if it has one, or TEST itself otherwise."
  (or (ert-test-name test) test))

(defun ert--stats-set-test-and-result (stats pos test result)
  "Change STATS by replacing the test at position POS with TEST and RESULT.

Also changes the counters in STATS to match."
  (let* ((tests (ert--stats-tests stats))
         (results (ert--stats-test-results stats))
         (old-test (aref tests pos))
         (map (ert--stats-test-map stats)))
    (cl-flet ((update (d)
                (if (ert-test-result-expected-p (aref tests pos)
                                                (aref results pos))
                    (cl-etypecase (aref results pos)
                      (ert-test-passed
                       (incf (ert--stats-passed-expected stats) d))
                      (ert-test-failed
                       (incf (ert--stats-failed-expected stats) d))
		      (ert-test-skipped
                       (incf (ert--stats-skipped stats) d))
                      (null)
                      (ert-test-aborted-with-non-local-exit)
                      (ert-test-quit))
                  (cl-etypecase (aref results pos)
                    (ert-test-passed
                     (incf (ert--stats-passed-unexpected stats) d))
                    (ert-test-failed
                     (incf (ert--stats-failed-unexpected stats) d))
                    (ert-test-skipped
                     (incf (ert--stats-skipped stats) d))
                    (null)
                    (ert-test-aborted-with-non-local-exit)
                    (ert-test-quit)))))
      ;; Adjust counters to remove the result that is currently in stats.
      (update -1)
      ;; Put new test and result into stats.
      (setf (aref tests pos) test
            (aref results pos) result)
      (remhash (ert--stats-test-key old-test) map)
      (setf (gethash (ert--stats-test-key test) map) pos)
      ;; Adjust counters to match new result.
      (update +1)
      nil)))

(defun ert--make-stats (tests selector)
  "Create a new `ert--stats' object for running TESTS.

SELECTOR is the selector that was used to select TESTS."
  (setq tests (cl-coerce tests 'vector))
  (let ((map (make-hash-table :size (length tests))))
    (cl-loop for i from 0
             for test across tests
             for key = (ert--stats-test-key test) do
             (cl-assert (not (gethash key map)))
             (setf (gethash key map) i))
    (make-ert--stats :selector selector
                     :tests tests
                     :test-map map
                     :test-results (make-vector (length tests) nil)
                     :test-start-times (make-vector (length tests) nil)
                     :test-end-times (make-vector (length tests) nil))))

(defun ert-run-or-rerun-test (stats test listener)
  ;; checkdoc-order: nil
  "Run the single test TEST and record the result using STATS and LISTENER."
  (let ((ert--current-run-stats stats)
        (pos (ert--stats-test-pos stats test)))
    (ert--stats-set-test-and-result stats pos test nil)
    ;; Call listener after setting/before resetting
    ;; (ert--stats-current-test stats); the listener might refresh the
    ;; mode line display, and if the value is not set yet/any more
    ;; during this refresh, the mode line will flicker unnecessarily.
    (setf (ert--stats-current-test stats) test)
    (funcall listener 'test-started stats test)
    (setf (ert-test-most-recent-result test) nil)
    (setf (aref (ert--stats-test-start-times stats) pos) (current-time))
    (unwind-protect
        (ert-run-test test)
      (setf (aref (ert--stats-test-end-times stats) pos) (current-time))
      (let ((result (ert-test-most-recent-result test)))
        (setf (ert-test-result-duration result)
              (float-time
               (time-subtract
                (aref (ert--stats-test-end-times stats) pos)
                (aref (ert--stats-test-start-times stats) pos))))
        (ert--stats-set-test-and-result stats pos test result)
        (funcall listener 'test-ended stats test result))
      (setf (ert--stats-current-test stats) nil))))

(defun ert-run-tests (selector listener &optional interactively)
  "Run the tests specified by SELECTOR, sending progress updates to LISTENER."
  (let* ((tests (ert-select-tests selector t))
         (stats (ert--make-stats tests selector)))
    (setf (ert--stats-start-time stats) (current-time))
    (funcall listener 'run-started stats)
    (let ((abortedp t))
      (unwind-protect
          (let ((ert--current-run-stats stats))
            (force-mode-line-update)
            (unwind-protect
		(cl-loop for test in tests do
			 (ert-run-or-rerun-test stats test listener)
			 (when (and interactively
				    (ert-test-quit-p
				     (ert-test-most-recent-result test))
				    (y-or-n-p "Abort testing? "))
			   (cl-return))
			 finally (setq abortedp nil))
              (setf (ert--stats-aborted-p stats) abortedp)
              (setf (ert--stats-end-time stats) (current-time))
              (funcall listener 'run-ended stats abortedp)))
        (force-mode-line-update))
      stats)))

(defun ert--stats-test-pos (stats test)
  ;; checkdoc-order: nil
  "Return the position (index) of TEST in the run represented by STATS."
  (gethash (ert--stats-test-key test) (ert--stats-test-map stats)))


;;; Formatting functions shared across UIs.

(defun ert--format-time-iso8601 (time)
  "Format TIME in the variant of ISO 8601 used for timestamps in ERT."
  (format-time-string "%Y-%m-%d %T%z" time))

(defun ert-char-for-test-result (result expectedp)
  "Return a character that represents the test result RESULT.

EXPECTEDP specifies whether the result was expected."
  (let ((s (cl-etypecase result
             (ert-test-passed ".P")
             (ert-test-failed "fF")
             (ert-test-skipped "sS")
             (null "--")
             (ert-test-aborted-with-non-local-exit "aA")
             (ert-test-quit "qQ"))))
    (elt s (if expectedp 0 1))))

(defun ert-string-for-test-result (result expectedp)
  "Return a string that represents the test result RESULT.

EXPECTEDP specifies whether the result was expected."
  (let ((s (cl-etypecase result
             (ert-test-passed '("passed" "PASSED"))
             (ert-test-failed '("failed" "FAILED"))
             (ert-test-skipped '("skipped" "SKIPPED"))
             (null '("unknown" "UNKNOWN"))
             (ert-test-aborted-with-non-local-exit '("aborted" "ABORTED"))
             (ert-test-quit '("quit" "QUIT")))))
    (elt s (if expectedp 0 1))))

(defun ert-reason-for-test-result (result)
  "Return the reason given for RESULT, as a string.

The reason is the argument given when invoking `ert-fail' or `ert-skip'.
It is output using `prin1' prefixed by two spaces.

If no reason was given, or for a successful RESULT, return the
empty string."
  (let ((reason
         (and
          (ert-test-result-with-condition-p result)
          (cadr (ert-test-result-with-condition-condition result))))
        (print-escape-newlines t)
        (print-level 6)
        (print-length 10))
    (if reason (format "  %S" reason) "")))

(defun ert--pp-with-indentation-and-newline (object)
  "Pretty-print OBJECT, indenting it to the current column of point.
Ensures a final newline is inserted."
  (let ((pp-escape-newlines t)
        (print-escape-control-characters t))
    (pp object (current-buffer))))

(defun ert--insert-infos (result)
  "Insert `ert-info' infos from RESULT into current buffer.

RESULT must be an `ert-test-result-with-condition'."
  (cl-check-type result ert-test-result-with-condition)
  (dolist (info (ert-test-result-with-condition-infos result))
    (cl-destructuring-bind (prefix . message) info
      (let ((begin (point))
            (indentation (make-string (+ (length prefix) 4) ?\s))
            (end nil))
        (unwind-protect
            (progn
              (when (functionp message)
                (setq message (funcall message)))
              (insert message "\n")
              (setq end (point-marker))
              (goto-char begin)
              (insert "    " prefix)
              (forward-line 1)
              (while (< (point) end)
                (insert indentation)
                (forward-line 1)))
          (when end (set-marker end nil)))))))


;;; Running tests in batch mode.

(defvar ert-quiet nil
  "Non-nil makes ERT only print important information in batch mode.")

(defun ert-test-location (test)
  "Return a string description the source location of TEST."
  (when-let* ((loc
               (ignore-errors
                 (find-function-search-for-symbol
                  (ert-test-name test) 'ert--test (ert-test-file-name test)))))
    (let* ((buffer (car loc))
           (point (cdr loc))
           (file (file-relative-name (buffer-file-name buffer)))
           (line (with-current-buffer buffer
                   (line-number-at-pos point))))
      (format "at %s:%s" file line))))

(defvar ert-batch-backtrace-right-margin 70
  "The maximum line length for printing backtraces in `ert-run-tests-batch'.")

;;;###autoload
(defun ert-run-tests-batch (&optional selector)
  "Run the tests specified by SELECTOR, printing results to the terminal.

SELECTOR selects which tests to run as described in `ert-select-tests' when
called with its second argument t, except if SELECTOR is nil, in which case
all tests rather than none will be run; this makes the command line
 \"emacs -batch -l my-tests.el -f ert-run-tests-batch-and-exit\" useful.

Returns the stats object."
  (unless selector (setq selector 't))
  (ert-run-tests
   selector
   (lambda (event-type &rest event-args)
     (cl-ecase event-type
       (run-started
        (unless ert-quiet
          (cl-destructuring-bind (stats) event-args
            (message "Running %s tests (%s, selector `%S')"
                     (length (ert--stats-tests stats))
                     (ert--format-time-iso8601 (ert--stats-start-time stats))
                     selector))))
       (run-ended
        (cl-destructuring-bind (stats abortedp) event-args
          (let ((unexpected (ert-stats-completed-unexpected stats))
                (skipped (ert-stats-skipped stats))
		(expected-failures (ert--stats-failed-expected stats)))
            (message "\n%sRan %s tests, %s results as expected, %s unexpected%s (%s, %f sec)%s\n"
                     (if (not abortedp)
                         ""
                       "Aborted: ")
                     (ert-stats-total stats)
                     (ert-stats-completed-expected stats)
                     unexpected
                     (if (zerop skipped)
                         ""
                       (format ", %s skipped" skipped))
                     (ert--format-time-iso8601 (ert--stats-end-time stats))
                     (float-time
                      (time-subtract
                       (ert--stats-end-time stats)
                       (ert--stats-start-time stats)))
                     (if (zerop expected-failures)
                         ""
                       (format "\n%s expected failures" expected-failures)))
            (unless (zerop unexpected)
              (message "%s unexpected results:" unexpected)
              (cl-loop for test across (ert--stats-tests stats)
                       for result = (ert-test-most-recent-result test) do
                       (when (not (ert-test-result-expected-p test result))
                         (message "%9s  %S%s"
                                  (ert-string-for-test-result result nil)
                                  (ert-test-name test)
                                  (if (plusp
                                       (length (getenv "EMACS_TEST_VERBOSE")))
                                      (ert-reason-for-test-result result)
                                    ""))))
              (message "%s" ""))
            (unless (zerop skipped)
              (message "%s skipped results:" skipped)
              (cl-loop for test across (ert--stats-tests stats)
                       for result = (ert-test-most-recent-result test) do
                       (when (ert-test-result-type-p result :skipped)
                         (message "%9s  %S%s"
                                  (ert-string-for-test-result result nil)
                                  (ert-test-name test)
                                  (if (plusp
                                       (length (getenv "EMACS_TEST_VERBOSE")))
                                      (ert-reason-for-test-result result)
                                    ""))))
              (message "%s" ""))
            (when (getenv "EMACS_TEST_JUNIT_REPORT")
              (ert-write-junit-test-report stats)))))
       (test-started)
       (test-ended
        (cl-destructuring-bind (stats test result) event-args
          (unless (ert-test-result-expected-p test result)
            (cl-etypecase result
              (ert-test-passed
               (message "Test %S passed unexpectedly" (ert-test-name test)))
              (ert-test-result-with-condition
               (message "Test %S backtrace:" (ert-test-name test))
               (with-temp-buffer
                 (let ((backtrace-line-length
                        (if (eq ert-batch-backtrace-line-length t)
                            backtrace-line-length
                          ert-batch-backtrace-line-length))
                       (print-level ert-batch-print-level)
                       (print-length ert-batch-print-length))
                   (insert (backtrace-to-string
                            (ert-test-result-with-condition-backtrace result))))
                 (if (not ert-batch-backtrace-right-margin)
                     (message "%s"
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))
                   (goto-char (point-min))
                   (while (not (eobp))
                     (let ((start (point))
                           (end (line-end-position)))
                       (setq end (min end
                                      (+ start
                                         ert-batch-backtrace-right-margin)))
                       (message "%s" (buffer-substring-no-properties
                                      start end)))
                     (forward-line 1))))
               (with-temp-buffer
                 (ert--insert-infos result)
                 (insert "    ")
                 (let ((print-escape-newlines t)
                       (print-level ert-batch-print-level)
                       (print-length ert-batch-print-length))
                   (ert--pp-with-indentation-and-newline
                    (ert-test-result-with-condition-condition result)))
                 (goto-char (1- (point-max)))
                 (cl-assert (looking-at "\n"))
                 (delete-char 1)
                 (message "Test %S condition:" (ert-test-name test))
                 (message "%s" (buffer-string))))
              (ert-test-aborted-with-non-local-exit
               (message "Test %S aborted with non-local exit"
                        (ert-test-name test)))
              (ert-test-quit
               (message "Quit during %S" (ert-test-name test)))))
          (unless ert-quiet
            (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                   (format-string (concat "%9s  %"
                                          (prin1-to-string (length max))
                                          "s/" max "  %S (%f sec)%s")))
              (message format-string
                       (ert-string-for-test-result result
                                                   (ert-test-result-expected-p
                                                    test result))
                       (1+ (ert--stats-test-pos stats test))
                       (ert-test-name test)
                       (ert-test-result-duration result)
                       (if (ert-test-result-expected-p test result)
                           ""
                         (concat " " (ert-test-location test))))))))))
   nil))

;;;###autoload
(defun ert-run-tests-batch-and-exit (&optional selector)
  "Like `ert-run-tests-batch', but exits Emacs when done.

The exit status will be 0 if all test results were as expected, 1
on unexpected results, or 2 if the tool detected an error outside
of the tests (e.g. invalid SELECTOR or bug in the code that runs
the tests)."
  (or noninteractive
      (user-error "This function is only for use in batch mode"))
  (let ((eln-dir (and (featurep 'native-compile)
                      (make-temp-file "test-nativecomp-cache-" t))))
    (when eln-dir
      (startup-redirect-eln-cache eln-dir))
    ;; Better crash loudly than attempting to recover from undefined
    ;; behavior.
    (setq attempt-stack-overflow-recovery nil
          attempt-orderly-shutdown-on-fatal-signal nil)
    (unwind-protect
        (let ((stats (ert-run-tests-batch selector)))
          (when eln-dir
            (ignore-errors
              (delete-directory eln-dir t)))
          (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
      (unwind-protect
          (progn
            (message "Error running tests")
            (backtrace))
        (when eln-dir
          (ignore-errors
            (delete-directory eln-dir t)))
        (kill-emacs 2)))))

(defvar ert-load-file-name nil
  "The name of the loaded ERT test file, a string.
Usually, it is not needed to be defined, but if different ERT
test packages depend on each other, it might be helpful.")

(defun ert-write-junit-test-report (stats)
  "Write a JUnit test report, generated from STATS."
  ;; https://www.ibm.com/docs/en/developer-for-zos/14.1.0?topic=formats-junit-xml-format
  ;; https://llg.cubic.org/docs/junit/
  (when-let* ((symbol (car (apropos-internal "" #'ert-test-boundp)))
              (test-file (symbol-file symbol 'ert--test))
              (test-report
               (file-name-with-extension
                (or ert-load-file-name test-file) "xml")))
    (with-temp-file test-report
      (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
      (insert (format "<testsuites name=\"%s\" tests=\"%s\" errors=\"%s\" failures=\"%s\" skipped=\"%s\" time=\"%s\">\n"
                      (file-name-nondirectory test-report)
                      (ert-stats-total stats)
                      (if (ert--stats-aborted-p stats) 1 0)
                      (ert-stats-completed-unexpected stats)
                      (ert-stats-skipped stats)
                      (float-time
                       (time-subtract
                        (ert--stats-end-time stats)
                        (ert--stats-start-time stats)))))
      (insert (format "  <testsuite id=\"0\" name=\"%s\" tests=\"%s\" errors=\"%s\" failures=\"%s\" skipped=\"%s\" time=\"%s\" timestamp=\"%s\">\n"
                      (file-name-nondirectory test-report)
                      (ert-stats-total stats)
                      (if (ert--stats-aborted-p stats) 1 0)
                      (ert-stats-completed-unexpected stats)
                      (ert-stats-skipped stats)
                      (float-time
                       (time-subtract
                        (ert--stats-end-time stats)
                        (ert--stats-start-time stats)))
                      (ert--format-time-iso8601 (ert--stats-end-time stats))))
      ;; If the test has aborted, `ert--stats-selector' might return
      ;; huge junk.  Skip this.
      (when (< (length (format "%s" (ert--stats-selector stats))) 1024)
        (insert "    <properties>\n"
                (format "      <property name=\"selector\" value=\"%s\"/>\n"
                        (xml-escape-string
                         (format "%s" (ert--stats-selector stats)) 'noerror))
                "    </properties>\n"))
      (cl-loop for test across (ert--stats-tests stats)
               for result = (ert-test-most-recent-result test) do
               (insert (format "    <testcase name=\"%s\" status=\"%s\" time=\"%s\""
                               (xml-escape-string
                                (symbol-name (ert-test-name test)) 'noerror)
                               (ert-string-for-test-result
                                result
                                (ert-test-result-expected-p test result))
                               (ert-test-result-duration result)))
               (if (and (ert-test-result-expected-p test result)
                        (not (ert-test-aborted-with-non-local-exit-p result))
                        (not (ert-test-skipped-p result))
                        (zerop (length (ert-test-result-messages result))))
                   (insert "/>\n")
                 (insert ">\n")
                 (cond
                  ((ert-test-skipped-p result)
                   (insert (format "      <skipped message=\"%s\" type=\"%s\">\n"
                                   (xml-escape-string
                                    (string-trim
                                     (ert-reason-for-test-result result))
                                    'noerror)
                                   (ert-string-for-test-result
                                    result
                                    (ert-test-result-expected-p
                                     test result)))
                           (xml-escape-string
                            (string-trim
                             (ert-reason-for-test-result result))
                            'noerror)
                           "\n"
                           "      </skipped>\n"))
                  ((ert-test-aborted-with-non-local-exit-p result)
                   (insert (format "      <error message=\"%s\" type=\"%s\">\n"
                                   (file-name-nondirectory test-report)
                                   (ert-string-for-test-result
                                    result
                                    (ert-test-result-expected-p
                                     test result)))
                           (format "Test %s aborted with non-local exit\n"
                                   (xml-escape-string
                                    (symbol-name (ert-test-name test)) 'noerror))
                           "      </error>\n"))
                  ((not (ert-test-result-type-p
                         result (ert-test-expected-result-type test)))
                   (insert (format "      <failure message=\"%s\" type=\"%s\">\n"
                                   (xml-escape-string
                                    (string-trim
                                     (ert-reason-for-test-result result))
                                    'noerror)
                                   (ert-string-for-test-result
                                    result
                                    (ert-test-result-expected-p
                                     test result)))
                           (xml-escape-string
                            (string-trim
                             (ert-reason-for-test-result result))
                            'noerror)
                           "\n"
                           "      </failure>\n")))
                 (unless (zerop (length (ert-test-result-messages result)))
                   (insert "      <system-out>\n"
                           (xml-escape-string
                            (ert-test-result-messages result) 'noerror)
                           "      </system-out>\n"))
                 (insert "    </testcase>\n")))
      (insert "  </testsuite>\n")
      (insert "</testsuites>\n"))))

(defun ert-write-junit-test-summary-report (&rest logfiles)
  "Write a JUnit summary test report, generated from LOGFILES."
  (let ((report (file-name-with-extension
                 (getenv "EMACS_TEST_JUNIT_REPORT") "xml"))
        (tests 0) (errors 0) (failures 0) (skipped 0) (time 0) (id 0))
    (with-temp-file report
      (dolist (logfile logfiles)
        (let ((test-report (file-name-with-extension logfile "xml")))
          (if (not (file-readable-p test-report))
              (let* ((logfile (file-name-with-extension logfile "log"))
                     (logfile-contents
                      (when (file-readable-p logfile)
                        (with-temp-buffer
                          (insert-file-contents-literally logfile)
                          (buffer-string)))))
                (unless
                    ;; No defined tests, perhaps a helper file.
                    (and logfile-contents
                         (string-match-p "^Running 0 tests" logfile-contents))
                  (insert (format "  <testsuite id=\"%s\" name=\"%s\" tests=\"1\" errors=\"1\" failures=\"0\" skipped=\"0\" time=\"0\" timestamp=\"%s\">\n"
                                  id test-report
				  (ert--format-time-iso8601 nil)))
                  (insert (format "    <testcase name=\"Test report missing %s\" status=\"error\" time=\"0\">\n"
                                  (file-name-nondirectory test-report)))
                  (insert (format "      <error message=\"Test report missing %s\" type=\"error\">\n"
                                  (file-name-nondirectory test-report)))
                  (when logfile-contents
                    (insert (xml-escape-string logfile-contents 'noerror)))
                  (insert "      </error>\n"
                          "    </testcase>\n"
                          "  </testsuite>\n")
                  (incf errors 1)
                  (incf id 1)))

            (insert-file-contents-literally test-report)
            (when (looking-at-p
                   (regexp-quote "<?xml version=\"1.0\" encoding=\"utf-8\"?>"))
              (delete-region (point) (line-beginning-position 2)))
            (when (looking-at
                   "<testsuites name=\".+\" tests=\"\\(.+\\)\" errors=\"\\(.+\\)\" failures=\"\\(.+\\)\" skipped=\"\\(.+\\)\" time=\"\\(.+\\)\">")
              (incf tests (string-to-number (match-string 1)))
              (incf errors (string-to-number (match-string 2)))
              (incf failures (string-to-number (match-string 3)))
              (incf skipped (string-to-number (match-string 4)))
              (incf time (string-to-number (match-string 5)))
              (delete-region (point) (line-beginning-position 2)))
            (when (looking-at "  <testsuite id=\"\\(0\\)\"")
              (replace-match (number-to-string id) nil nil nil 1)
              (incf id 1))
            (goto-char (point-max))
            (beginning-of-line 0)
            (when (looking-at-p "</testsuites>")
              (delete-region (point) (line-beginning-position 2))))

          (narrow-to-region (point-max) (point-max))))

      (insert "</testsuites>\n")
      (widen)
      (goto-char (point-min))
      (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
      (insert (format "<testsuites name=\"%s\" tests=\"%s\" errors=\"%s\" failures=\"%s\" skipped=\"%s\" time=\"%s\">\n"
                      (file-name-nondirectory report)
                      tests errors failures skipped time)))))

(defun ert-summarize-tests-batch-and-exit (&optional high)
  "Summarize the results of testing.
Expects to be called in batch mode, with logfiles as command-line arguments.
The logfiles should have the `ert-run-tests-batch' format.  When finished,
this exits Emacs, with status as per `ert-run-tests-batch-and-exit'.

If HIGH is a natural number, the HIGH long lasting tests are summarized."
  (or noninteractive
      (user-error "This function is only for use in batch mode"))
  (or (natnump high) (setq high 0))
  ;; Better crash loudly than attempting to recover from undefined
  ;; behavior.
  (setq attempt-stack-overflow-recovery nil
        attempt-orderly-shutdown-on-fatal-signal nil)
  (when (getenv "EMACS_TEST_JUNIT_REPORT")
    (apply #'ert-write-junit-test-summary-report command-line-args-left))
  (let ((nlogs (length command-line-args-left))
        (ntests 0) (nrun 0) (nexpected 0) (nunexpected 0) (nskipped 0)
        nnotrun logfile notests badtests unexpected skipped tests)
    (with-temp-buffer
      (while (setq logfile (pop command-line-args-left))
        (erase-buffer)
        (when (file-readable-p logfile) (insert-file-contents logfile))
        (if (not (re-search-forward "^Running \\([0-9]+\\) tests" nil t))
            (push logfile notests)
          (setq ntests (+ ntests (string-to-number (match-string 1))))
          (if (not (re-search-forward "^\\(Aborted: \\)?\
Ran \\([0-9]+\\) tests, \\([0-9]+\\) results as expected\
\\(?:, \\([0-9]+\\) unexpected\\)?\
\\(?:, \\([0-9]+\\) skipped\\)?" nil t))
              (push logfile badtests)
            (if (match-string 1) (push logfile badtests))
            (setq nrun (+ nrun (string-to-number (match-string 2)))
                  nexpected (+ nexpected (string-to-number (match-string 3))))
            (when (match-string 4)
	      (let ((n (string-to-number (match-string 4))))
		(unless (zerop n)
		  (push logfile unexpected)
		  (setq nunexpected (+ nunexpected n)))))
            (when (match-string 5)
              (push logfile skipped)
              (setq nskipped (+ nskipped
                                (string-to-number (match-string 5)))))
            (unless (zerop high)
              (goto-char (point-min))
              (while (< (point) (point-max))
                (if (looking-at "^\\s-+\\w+\\s-+[[:digit:]]+/[[:digit:]]+\\s-+\\S-+\\s-+(\\([.[:digit:]]+\\)\\s-+sec)$")
                    (push (cons (string-to-number (match-string 1))
                                (match-string 0))
                          tests))
                (forward-line)))))))
    (setq nnotrun (- ntests nrun))
    (message "\nSUMMARY OF TEST RESULTS")
    (message "-----------------------")
    (message "Files examined: %d" nlogs)
    (message "Ran %d tests%s, %d results as expected, %d unexpected, %d skipped"
             nrun
             (if (zerop nnotrun) "" (format ", %d failed to run" nnotrun))
             nexpected nunexpected nskipped)
    (when notests
      (message "%d files did not contain any tests:" (length notests))
      (mapc (lambda (l) (message "  %s" l)) notests))
    (when badtests
      (message "%d files did not finish:" (length badtests))
      (mapc (lambda (l) (message "  %s" l)) badtests)
      (if (or (getenv "EMACS_HYDRA_CI") (getenv "EMACS_EMBA_CI"))
          (with-temp-buffer
            (dolist (f badtests)
              (erase-buffer)
              (insert-file-contents f)
              (message "Contents of unfinished file %s:" f)
              (message "-----\n%s\n-----" (buffer-string))))))
    (when unexpected
      (message "%d files contained unexpected results:" (length unexpected))
      (mapc (lambda (l) (message "  %s" l)) unexpected))
    (unless (or (null tests) (zerop high))
      (message "\nLONG-RUNNING TESTS")
      (message "------------------")
      (setq tests (ntake high (sort tests (lambda (x y) (> (car x) (car y))))))
      (message "%s" (mapconcat #'cdr tests "\n")))
    ;; More details on hydra and emba, where the logs are harder to get to.
    (when (and (or (getenv "EMACS_HYDRA_CI") (getenv "EMACS_EMBA_CI"))
               (not (zerop (+ nunexpected nskipped))))
      (message "\nDETAILS")
      (message "-------")
      (with-temp-buffer
        (dolist (x (list (list skipped "skipped" "SKIPPED")
                         (list unexpected "unexpected"
                               "\\(?:FAILED\\|PASSED\\)")))
          (mapc (lambda (l)
                  (erase-buffer)
                  (insert-file-contents l)
                  (message "%s:" l)
                  (when (re-search-forward (format "^[ \t]*[0-9]+ %s results:"
                                                   (nth 1 x))
                                           nil t)
                    (while (and (zerop (forward-line 1))
                                (looking-at (format "^[ \t]*%s" (nth 2 x))))
                      (message "%s" (buffer-substring (line-beginning-position)
                                                      (line-end-position))))))
                (car x)))))
    (kill-emacs (cond ((or notests badtests (not (zerop nnotrun))) 2)
                      (unexpected 1)
                      (t 0)))))

;;; Utility functions for load/unload actions.

(defun ert--activate-font-lock-keywords ()
  "Activate font-lock keywords for some of ERT's symbols."
  (font-lock-add-keywords
   nil
   '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))

(cl-defun ert--remove-from-list (list-var element &key key test)
  "Remove ELEMENT from the value of LIST-VAR if present.

This can be used as an inverse of `add-to-list'."
  (unless key (setq key #'identity))
  (unless test (setq test #'equal))
  (setf (symbol-value list-var)
        (cl-remove element
                   (symbol-value list-var)
                   :key key
                   :test test)))


;;; Some basic interactive functions.

(defun ert-read-test-name (prompt &optional default history
                                  add-default-to-prompt)
  "Read the name of a test and return it as a symbol.

Prompt with PROMPT.  If DEFAULT is a valid test name, use it as a
default.  HISTORY is the history to use; see `completing-read'.
If ADD-DEFAULT-TO-PROMPT is non-nil, PROMPT will be modified to
include the default, if any.

Signals an error if no test name was read."
  (cl-etypecase default
    (string (let ((symbol (intern-soft default)))
              (unless (and symbol (ert-test-boundp symbol))
                (setq default nil))))
    (symbol (setq default
                  (if (ert-test-boundp default)
                      (symbol-name default)
                    nil)))
    (ert-test (setq default (ert-test-name default))))
  (when add-default-to-prompt
    (setq prompt (format-prompt prompt default)))
  (let ((input (completing-read prompt obarray #'ert-test-boundp
                                t nil history default nil)))
    ;; completing-read returns an empty string if default was nil and
    ;; the user just hit enter.
    (let ((sym (intern-soft input)))
      (if (ert-test-boundp sym)
          sym
        (user-error "Input does not name a test")))))

(defun ert-read-test-name-at-point (prompt)
  "Read the name of a test and return it as a symbol.
As a default, use the symbol at point, or the test at point if in
the ERT results buffer.  Prompt with PROMPT, augmented with the
default (if any)."
  (ert-read-test-name prompt (ert-test-at-point) nil t))

(defun ert-find-test-other-window (test-name)
  "Find, in another window, the definition of TEST-NAME."
  (interactive (list (ert-read-test-name-at-point "Find test definition")))
  (find-function-do-it test-name 'ert--test 'switch-to-buffer-other-window))

(defun ert-delete-test (test-name)
  "Make the test TEST-NAME unbound.

Nothing more than an interactive interface to `ert-make-test-unbound'."
  (interactive (list (ert-read-test-name-at-point "Delete test")))
  (ert-make-test-unbound test-name))

(defun ert-delete-all-tests ()
  "Make all symbols in `obarray' name no test."
  (interactive)
  (when (called-interactively-p 'any)
    (unless (y-or-n-p "Delete all tests? ")
      (user-error "Aborted")))
  ;; We can't use `ert-select-tests' here since that gives us only
  ;; test objects, and going from them back to the test name symbols
  ;; can fail if the `ert-test' defstruct has been redefined.
  (mapc #'ert-make-test-unbound (apropos-internal "" #'ert-test-boundp))
  t)


;;; Display of test progress and results.

;; An entry in the results buffer ewoc.  There is one entry per test.
(cl-defstruct ert--ewoc-entry
  (test (cl-assert nil))
  ;; If the result of this test was expected, its ewoc entry is hidden
  ;; initially.
  (hidden-p (cl-assert nil))
  ;; An ewoc entry may be collapsed to hide details such as the error
  ;; condition.
  ;;
  ;; I'm not sure the ability to expand and collapse entries is still
  ;; a useful feature.
  (expanded-p t)
  ;; By default, the ewoc entry presents the error condition with
  ;; certain limits on how much to print (`print-level',
  ;; `print-length').  The user can interactively switch to a set of
  ;; higher limits.
  (extended-printer-limits-p nil))

;; Variables local to the results buffer.

;; The ewoc.
(defvar ert--results-ewoc)
;; The stats object.
(defvar ert--results-stats)
;; A string with one character per test.  Each character represents
;; the result of the corresponding test.  The string is displayed near
;; the top of the buffer and serves as a progress bar.
(defvar ert--results-progress-bar-string)
;; The position where the progress bar button begins.
(defvar ert--results-progress-bar-button-begin)
;; The test result listener that updates the buffer when tests are run.
(defvar ert--results-listener)

(defun ert-insert-test-name-button (test-name)
  "Insert a button that links to TEST-NAME."
  (insert-text-button (format "%S" test-name)
                      :type 'ert--test-name-button
                      'ert-test-name test-name))

(defun ert--results-format-expected-unexpected (expected unexpected)
  "Return a string indicating EXPECTED expected results, UNEXPECTED unexpected."
  (if (zerop unexpected)
      (format "%s" expected)
    (format "%s (%s unexpected)" (+ expected unexpected) unexpected)))

(defun ert--results-update-ewoc-hf (ewoc stats)
  "Update the header and footer of EWOC to show certain information from STATS.

Also sets `ert--results-progress-bar-button-begin'."
  (let ((run-count (ert-stats-completed stats))
        (results-buffer (current-buffer))
        ;; Need to save buffer-local value.
        (font-lock font-lock-mode))
    (ewoc-set-hf
     ewoc
     ;; header
     (with-temp-buffer
       (insert "Selector: ")
       (ert--insert-human-readable-selector (ert--stats-selector stats))
       (insert "\n")
       (insert
        (format (concat "Passed:  %s\n"
                        "Failed:  %s\n"
                        "Skipped: %s\n"
                        "Total:   %s/%s\n\n")
                (ert--results-format-expected-unexpected
                 (ert--stats-passed-expected stats)
                 (ert--stats-passed-unexpected stats))
                (ert--results-format-expected-unexpected
                 (ert--stats-failed-expected stats)
                 (ert--stats-failed-unexpected stats))
                (ert-stats-skipped stats)
                run-count
                (ert-stats-total stats)))
       (insert
        (format "Started at:   %s\n"
                (ert--format-time-iso8601 (ert--stats-start-time stats))))
       ;; FIXME: This is ugly.  Need to properly define invariants of
       ;; the `stats' data structure.
       (let ((state (cond ((ert--stats-aborted-p stats) 'aborted)
                          ((ert--stats-current-test stats) 'running)
                          ((ert--stats-end-time stats) 'finished)
                          (t 'preparing))))
         (cl-ecase state
           (preparing
            (insert ""))
           (aborted
            (cond ((ert--stats-current-test stats)
                   (insert "Aborted during test: ")
                   (ert-insert-test-name-button
                    (ert-test-name (ert--stats-current-test stats))))
                  (t
                   (insert "Aborted."))))
           (running
            (cl-assert (ert--stats-current-test stats))
            (insert "Running test: ")
            (ert-insert-test-name-button (ert-test-name
                                          (ert--stats-current-test stats))))
           (finished
            (cl-assert (not (ert--stats-current-test stats)))
            (insert "Finished.")))
         (insert "\n")
         (if (ert--stats-end-time stats)
             (insert
              (format "%s%s\n"
                      (if (ert--stats-aborted-p stats)
                          "Aborted at:   "
                        "Finished at:  ")
                      (ert--format-time-iso8601 (ert--stats-end-time stats))))
           (insert "\n"))
         (insert "\n"))
       (let ((progress-bar-string (with-current-buffer results-buffer
                                    ert--results-progress-bar-string)))
         (let ((progress-bar-button-begin
                (insert-text-button progress-bar-string
                                    :type 'ert--results-progress-bar-button
                                    'face (or (and font-lock
                                                   (ert-face-for-stats stats))
                                              'button))))
           ;; The header gets copied verbatim to the results buffer,
           ;; and all positions remain the same, so
           ;; `progress-bar-button-begin' will be the right position
           ;; even in the results buffer.
           (with-current-buffer results-buffer
             (setq-local ert--results-progress-bar-button-begin
                         progress-bar-button-begin))))
       (insert "\n\n")
       (buffer-string))
     ;; footer
     ;;
     ;; We actually want an empty footer, but that would trigger a bug
     ;; in ewoc, sometimes clearing the entire buffer.  (It's possible
     ;; that this bug has been fixed since this has been tested; we
     ;; should test it again.)
     "\n")))

(defvar ert-test-run-redisplay-interval-secs .1
  "How many seconds ERT should wait between redisplays while running tests.

While running tests, ERT shows the current progress, and this variable
determines how frequently the progress display is updated.")

(defun ert--results-update-stats-display (ewoc stats)
  "Update EWOC and the mode line to show data from STATS."
  ;; TODO(ohler): investigate using `make-progress-reporter'.
  (ert--results-update-ewoc-hf ewoc stats)
  (force-mode-line-update)
  (redisplay t)
  (setf (ert--stats-next-redisplay stats)
	(float-time (time-add nil ert-test-run-redisplay-interval-secs))))

(defun ert--results-update-stats-display-maybe (ewoc stats)
  "Call `ert--results-update-stats-display' if not called recently.

EWOC and STATS are arguments for `ert--results-update-stats-display'."
  (unless (time-less-p nil (ert--stats-next-redisplay stats))
    (ert--results-update-stats-display ewoc stats)))

(defun ert--tests-running-mode-line-indicator ()
  "Return a string for the mode line that shows the test run progress."
  (let* ((stats ert--current-run-stats)
         (tests-total (ert-stats-total stats))
         (tests-completed (ert-stats-completed stats)))
    (if (>= tests-completed tests-total)
        (format " ERT(%s/%s,finished)" tests-completed tests-total)
      (format " ERT(%s/%s):%s"
              (1+ tests-completed)
              tests-total
              (if (null (ert--stats-current-test stats))
                  "?"
                (format "%S"
                        (ert-test-name (ert--stats-current-test stats))))))))

(defun ert--make-xrefs-region (begin end)
  "Attach cross-references to function names between BEGIN and END.

BEGIN and END specify a region in the current buffer."
  (save-excursion
    (goto-char begin)
    (while (progn
             (goto-char (+ (point) 2))
             (skip-syntax-forward "^w_")
             (< (point) end))
      (let* ((beg (point))
             (end (progn (skip-syntax-forward "w_") (point)))
             (sym (intern-soft (buffer-substring-no-properties
                                beg end)))
             (file (and sym (symbol-file sym 'defun))))
        (when file
          (goto-char beg)
          ;; help-xref-button needs to operate on something matched
          ;; by a regexp, so set that up for it.
          (re-search-forward "\\(\\sw\\|\\s_\\)+")
          (help-xref-button 0 'help-function-def sym file)))
      (forward-line 1))))

(defun ert--string-first-line (s)
  "Return the first line of S, or S if it contains no newlines.

The return value does not include the line terminator."
  (substring s 0 (cl-position ?\n s)))

(defun ert-face-for-test-result (expectedp)
  "Return a face that shows whether a test result was expected or unexpected.

If EXPECTEDP is nil, returns the face for unexpected results; if
non-nil, returns the face for expected results.."
  (if expectedp 'ert-test-result-expected 'ert-test-result-unexpected))

(defun ert-face-for-stats (stats)
  "Return a face that represents STATS."
  (cond ((ert--stats-aborted-p stats) 'nil)
        ((plusp (ert-stats-completed-unexpected stats))
         (ert-face-for-test-result nil))
        ((eql (ert-stats-completed-expected stats) (ert-stats-total stats))
         (ert-face-for-test-result t))
        (t 'nil)))

(defun ert--print-test-for-ewoc (entry)
  "The ewoc print function for ewoc test entries.  ENTRY is the entry to print."
  (let* ((test (ert--ewoc-entry-test entry))
         (stats ert--results-stats)
         (result (let ((pos (ert--stats-test-pos stats test)))
                   (cl-assert pos)
                   (aref (ert--stats-test-results stats) pos)))
         (hiddenp (ert--ewoc-entry-hidden-p entry))
         (expandedp (ert--ewoc-entry-expanded-p entry))
         (extended-printer-limits-p (ert--ewoc-entry-extended-printer-limits-p
                                     entry)))
    (cond (hiddenp)
          (t
           (let ((expectedp (ert-test-result-expected-p test result)))
             (insert-text-button (format "%c" (ert-char-for-test-result
                                               result expectedp))
                                 :type 'ert--results-expand-collapse-button
                                 'face (or (and font-lock-mode
                                                (ert-face-for-test-result
                                                 expectedp))
                                           'button)))
           (insert " ")
           (ert-insert-test-name-button (ert-test-name test))
           (insert "\n")
           (when (and expandedp (not (eql result 'nil)))
             (when (ert-test-documentation test)
               (insert "    "
                       (propertize
                        (ert--string-first-line
                         (substitute-command-keys
                          (ert-test-documentation test)))
                        'font-lock-face 'font-lock-doc-face)
                       "\n"))
             (cl-etypecase result
               (ert-test-passed
                (if (ert-test-result-expected-p test result)
                    (insert "    passed\n")
                  (insert "    passed unexpectedly\n"))
                (insert ""))
               (ert-test-result-with-condition
                (ert--insert-infos result)
                (let ((print-escape-newlines t)
                      (print-level (if extended-printer-limits-p 12 6))
                      (print-length (if extended-printer-limits-p 100 10)))
                  (insert "    ")
                  (let ((begin (point)))
                    (ert--pp-with-indentation-and-newline
                     (ert-test-result-with-condition-condition result))
                    (ert--make-xrefs-region begin (point)))))
               (ert-test-aborted-with-non-local-exit
                (insert "    aborted\n"))
               (ert-test-quit
                (insert "    quit\n")))
             (insert "\n")))))
  nil)

(defun ert--results-font-lock-function (enabledp)
  "Redraw the ERT results buffer after `font-lock-mode' was switched on or off.

ENABLEDP is true if `font-lock-mode' is switched on, false
otherwise."
  (ert--results-update-ewoc-hf ert--results-ewoc ert--results-stats)
  (ewoc-refresh ert--results-ewoc)
  (font-lock-default-function enabledp))

(defvar ert--output-buffer-name "*ert*")

(defun ert--setup-results-buffer (stats listener)
  "Set up a test results buffer.

STATS is the stats object; LISTENER is the results listener."
  (let ((buffer (get-buffer-create ert--output-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-results-mode)
        ;; Erase buffer again in case switching out of the previous
        ;; mode inserted anything.  (This happens e.g. when switching
        ;; from ert-results-mode to ert-results-mode when
        ;; font-lock-mode turns itself off in change-major-mode-hook.)
        (erase-buffer)
        (setq-local font-lock-function
                    'ert--results-font-lock-function)
        (let ((ewoc (ewoc-create 'ert--print-test-for-ewoc nil nil t)))
          (setq-local ert--results-ewoc ewoc)
          (setq-local ert--results-stats stats)
          (setq-local ert--results-progress-bar-string
                      (make-string (ert-stats-total stats)
                                   (ert-char-for-test-result nil t)))
          (setq-local ert--results-listener listener)
          (cl-loop for test across (ert--stats-tests stats) do
                   (ewoc-enter-last ewoc
                                    (make-ert--ewoc-entry :test test
                                                          :hidden-p t)))
          (ert--results-update-ewoc-hf ert--results-ewoc ert--results-stats)
          (goto-char (1- (point-max)))
          buffer)))))

(defvar ert--selector-history nil
  "List of recent test selectors read from terminal.")

;;;###autoload
(defun ert-run-tests-interactively (selector)
  "Run the tests specified by SELECTOR and display the results in a buffer.

SELECTOR selects which tests to run as described in `ert-select-tests'
when called with its second argument t.  Interactively, prompt for
SELECTOR; the default t means run all the defined tests."
  (interactive
   (list (let ((default (if ert--selector-history
                            ;; Can't use `first' here as this form is
                            ;; not compiled, and `first' is not
                            ;; defined without cl.
                            (car ert--selector-history)
                          "t"))
               (symbol-packages t))
           (read
            (completing-read (format-prompt "Run tests" default)
                             obarray #'ert-test-boundp nil nil
                             'ert--selector-history default nil)))))
  (let (buffer listener)
    (setq listener
          (lambda (event-type &rest event-args)
            (cl-ecase event-type
              (run-started
               (cl-destructuring-bind (stats) event-args
                 (setq buffer (ert--setup-results-buffer stats listener))
                 (pop-to-buffer buffer)))
              (run-ended
               (cl-destructuring-bind (stats abortedp) event-args
                 (message
                          "%sRan %s tests, %s results were as expected%s%s"
                          (if (not abortedp)
                              ""
                            "Aborted: ")
                          (ert-stats-total stats)
                          (ert-stats-completed-expected stats)
                          (let ((unexpected
                                 (ert-stats-completed-unexpected stats)))
                            (if (zerop unexpected)
                                ""
                              (format ", %s unexpected" unexpected)))
                          (let ((skipped
                                 (ert-stats-skipped stats)))
                            (if (zerop skipped)
                                ""
                              (format ", %s skipped" skipped))))
                 (ert--results-update-stats-display (with-current-buffer buffer
                                                      ert--results-ewoc)
                                                    stats)))
              (test-started
               (cl-destructuring-bind (stats test) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ert--results-ewoc)
                          (pos (ert--stats-test-pos stats test))
                          (node (ewoc-nth ewoc pos)))
                     (cl-assert node)
                     (setf (ert--ewoc-entry-test (ewoc-data node)) test)
                     (aset ert--results-progress-bar-string pos
                           (ert-char-for-test-result nil t))
                     (ert--results-update-stats-display-maybe ewoc stats)
                     (ewoc-invalidate ewoc node)))))
              (test-ended
               (cl-destructuring-bind (stats test result) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ert--results-ewoc)
                          (pos (ert--stats-test-pos stats test))
                          (node (ewoc-nth ewoc pos)))
                     (when (ert--ewoc-entry-hidden-p (ewoc-data node))
                       (setf (ert--ewoc-entry-hidden-p (ewoc-data node))
                             (ert-test-result-expected-p test result)))
                     (aset ert--results-progress-bar-string pos
                           (ert-char-for-test-result result
                                                     (ert-test-result-expected-p
                                                      test result)))
                     (ert--results-update-stats-display-maybe ewoc stats)
                     (ewoc-invalidate ewoc node))))))))
    (ert-run-tests selector listener t)))

;;;###autoload
(defalias 'ert #'ert-run-tests-interactively)


;;; Simple view mode for auxiliary information like stack traces or
;;; messages.  Mainly binds "q" for quit.

(define-derived-mode ert-simple-view-mode special-mode "ERT-View"
  "Major mode for viewing auxiliary information in ERT.")

;;; Commands and button actions for the results buffer.

(define-derived-mode ert-results-mode special-mode "ERT-Results"
  "Major mode for viewing results of ERT test runs."
  :interactive nil
  (setq-local revert-buffer-function
              (lambda (&rest _) (ert-results-rerun-all-tests))))

(cl-loop for (key binding) in
         '( ;; Stuff that's not in the menu.
           ("\t" forward-button)
           ([backtab] backward-button)
           ("j" ert-results-jump-between-summary-and-result)
           ("L" ert-results-toggle-printer-limits-for-test-at-point)
           ("n" ert-results-next-test)
           ("p" ert-results-previous-test)
           ;; Stuff that is in the menu.
           ("R" ert-results-rerun-all-tests)
           ("r" ert-results-rerun-test-at-point)
           ("d" ert-results-rerun-test-at-point-debugging-errors)
           ("." ert-results-find-test-at-point-other-window)
           ("b" ert-results-pop-to-backtrace-for-test-at-point)
           ("m" ert-results-pop-to-messages-for-test-at-point)
           ("l" ert-results-pop-to-should-forms-for-test-at-point)
           ("h" ert-results-describe-test-at-point)
           ("D" ert-delete-test)
           ("T" ert-results-pop-to-timings)
           )
         do
         (define-key ert-results-mode-map key binding))

(easy-menu-define ert-results-mode-menu ert-results-mode-map
  "Menu for `ert-results-mode'."
  '("ERT Results"
    ["Re-run all tests" ert-results-rerun-all-tests]
    "--"
    ;; FIXME?  Why are there (at least) 3 different ways to decide if
    ;; there is a test at point?
    ["Re-run test" ert-results-rerun-test-at-point
     :active (car (ert--results-test-at-point-allow-redefinition))]
    ["Debug test" ert-results-rerun-test-at-point-debugging-errors
     :active (car (ert--results-test-at-point-allow-redefinition))]
    ["Show test definition" ert-results-find-test-at-point-other-window
     :active (ert-test-at-point)]
    "--"
    ["Show backtrace" ert-results-pop-to-backtrace-for-test-at-point
     :active (ert--results-test-at-point-no-redefinition)]
    ["Show messages" ert-results-pop-to-messages-for-test-at-point
     :active (ert--results-test-at-point-no-redefinition)]
    ["Show `should' forms" ert-results-pop-to-should-forms-for-test-at-point
     :active (ert--results-test-at-point-no-redefinition)]
    ["Describe test" ert-results-describe-test-at-point
     :active (ert--results-test-at-point-no-redefinition)]
    "--"
    ["Delete test" ert-delete-test]
    "--"
    ["Show execution time of each test" ert-results-pop-to-timings]
    ))

(define-button-type 'ert--results-progress-bar-button
  'action #'ert--results-progress-bar-button-action
  'help-echo #'ert--results-progress-bar-button-help-echo)

(define-button-type 'ert--test-name-button
  'action #'ert--test-name-button-action
  'help-echo "mouse-2, RET: Find test definition")

(define-button-type 'ert--results-expand-collapse-button
  'action #'ert--results-expand-collapse-button-action
  'help-echo "mouse-2, RET: Expand/collapse test result")

(defun ert--results-test-node-or-null-at-point ()
  "If point is on a valid ewoc node, return it; return nil otherwise.

To be used in the ERT results buffer."
  (let* ((ewoc ert--results-ewoc)
         (node (ewoc-locate ewoc)))
    ;; `ewoc-locate' will return an arbitrary node when point is on
    ;; header or footer, or when all nodes are invisible.  So we need
    ;; to validate its return value here.
    ;;
    ;; Update: I'm seeing nil being returned in some cases now,
    ;; perhaps this has been changed?
    (if (and node
             (>= (point) (ewoc-location node))
             (not (ert--ewoc-entry-hidden-p (ewoc-data node))))
        node
      nil)))

(defun ert--results-test-node-at-point ()
  "If point is on a valid ewoc node, return it; signal an error otherwise.

To be used in the ERT results buffer."
  (or (ert--results-test-node-or-null-at-point)
      (user-error "No test at point")))

(defun ert-results-next-test ()
  "Move point to the next test.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (ert--results-move (ewoc-locate ert--results-ewoc) 'ewoc-next
                     "No tests below"))

(defun ert-results-previous-test ()
  "Move point to the previous test.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (ert--results-move (ewoc-locate ert--results-ewoc) 'ewoc-prev
                     "No tests above"))

(defun ert--results-move (node ewoc-fn error-message)
  "Move point from NODE to the previous or next node.

EWOC-FN specifies the direction and should be either `ewoc-prev'
or `ewoc-next'.  If there are no more nodes in that direction, a
user-error is signaled with the message ERROR-MESSAGE."
  (cl-loop
   (setq node (funcall ewoc-fn ert--results-ewoc node))
   (when (null node)
     (user-error "%s" error-message))
   (unless (ert--ewoc-entry-hidden-p (ewoc-data node))
     (goto-char (ewoc-location node))
     (cl-return))))

(defun ert--results-expand-collapse-button-action (_button)
  "Expand or collapse the test node BUTTON belongs to."
  (let* ((ewoc ert--results-ewoc)
         (node (save-excursion
                 (goto-char (ert--button-action-position))
                 (ert--results-test-node-at-point)))
         (entry (ewoc-data node)))
    (setf (ert--ewoc-entry-expanded-p entry)
          (not (ert--ewoc-entry-expanded-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ert-results-find-test-at-point-other-window ()
  "Find the definition of the test at point in another window.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (let ((name (ert-test-at-point)))
    (unless name
      (user-error "No test at point"))
    (ert-find-test-other-window name)))

(defun ert--test-name-button-action (button)
  "Find the definition of the test BUTTON belongs to, in another window."
  ;; work with either ert-insert-test-name-button or help-xref-button
  (let ((name (or (button-get button 'ert-test-name)
                  (car (button-get button 'help-args)))))
    (ert-find-test-other-window name)))

(defun ert--ewoc-position (ewoc node)
  ;; checkdoc-order: nil
  "Return the position of NODE in EWOC, or nil if NODE is not in EWOC."
  (cl-loop for i from 0
           for node-here = (ewoc-nth ewoc 0) then (ewoc-next ewoc node-here)
           do (when (eql node node-here)
                (cl-return i))
           finally (cl-return nil)))

(defun ert-results-jump-between-summary-and-result ()
  "Jump back and forth between the test run summary and individual test results.

From an ewoc node, jumps to the character that represents the
same test in the progress bar, and vice versa.

To be used in the ERT results buffer."
  ;; Maybe this command isn't actually needed much, but if it is, it
  ;; seems like an indication that the UI design is not optimal.  If
  ;; jumping back and forth between a summary at the top of the buffer
  ;; and the error log in the remainder of the buffer is useful, then
  ;; the summary apparently needs to be easily accessible from the
  ;; error log, and perhaps it would be better to have it in a
  ;; separate buffer to keep it visible.
  (interactive nil ert-results-mode)
  (let ((ewoc ert--results-ewoc)
        (progress-bar-begin ert--results-progress-bar-button-begin))
    (cond ((ert--results-test-node-or-null-at-point)
           (let* ((node (ert--results-test-node-at-point))
                  (pos (ert--ewoc-position ewoc node)))
             (goto-char (+ progress-bar-begin pos))))
          ((and (<= progress-bar-begin (point))
                (< (point) (button-end (button-at progress-bar-begin))))
           (let* ((node (ewoc-nth ewoc (- (point) progress-bar-begin)))
                  (entry (ewoc-data node)))
             (when (ert--ewoc-entry-hidden-p entry)
               (setf (ert--ewoc-entry-hidden-p entry) nil)
               (ewoc-invalidate ewoc node))
             (ewoc-goto-node ewoc node)))
          (t
           (goto-char progress-bar-begin)))))

(defun ert-test-at-point ()
  "Return the name of the test at point as a symbol, or nil if none."
  (or (and (eql major-mode 'ert-results-mode)
           (let ((test (ert--results-test-at-point-no-redefinition)))
             (and test (ert-test-name test))))
      (let* ((thing (thing-at-point 'symbol))
             (sym (intern-soft thing)))
        (and (ert-test-boundp sym)
             sym))))

(defun ert--results-test-at-point-no-redefinition (&optional error)
  "Return the test at point, or nil.
If optional argument ERROR is non-nil, signal an error rather than return nil.
To be used in the ERT results buffer."
  (cl-assert (eql major-mode 'ert-results-mode))
  (or
   (if (ert--results-test-node-or-null-at-point)
       (let* ((node (ert--results-test-node-at-point))
              (test (ert--ewoc-entry-test (ewoc-data node))))
         test)
     (let ((progress-bar-begin ert--results-progress-bar-button-begin))
       (when (and (<= progress-bar-begin (point))
                  (< (point) (button-end (button-at progress-bar-begin))))
         (let* ((test-index (- (point) progress-bar-begin))
                (test (aref (ert--stats-tests ert--results-stats)
                           test-index)))
           test))))
   (if error (user-error "No test at point"))))

(defun ert--results-test-at-point-allow-redefinition ()
  "Look up the test at point, and check whether it has been redefined.

To be used in the ERT results buffer.

Returns a list of two elements: the test (or nil) and a symbol
specifying whether the test has been redefined.

If a new test has been defined with the same name as the test at
point, replaces the test at point with the new test, and returns
the new test and the symbol `redefined'.

If the test has been deleted, returns the old test and the symbol
`deleted'.

If the test is still current, returns the test and the symbol nil.

If there is no test at point, returns a list with two nils."
  (let ((test (ert--results-test-at-point-no-redefinition)))
    (cond ((null test)
           `(nil nil))
          ((null (ert-test-name test))
           `(,test nil))
          (t
           (let* ((name (ert-test-name test))
                  (new-test (and (ert-test-boundp name)
                                 (ert-get-test name))))
             (cond ((eql test new-test)
                    `(,test nil))
                   ((null new-test)
                    `(,test deleted))
                   (t
                    (ert--results-update-after-test-redefinition
                     (ert--stats-test-pos ert--results-stats test)
                     new-test)
                    `(,new-test redefined))))))))

(defun ert--results-update-after-test-redefinition (pos new-test)
  "Update results buffer after the test at pos POS has been redefined.

Also updates the stats object.  NEW-TEST is the new test
definition."
  (let* ((stats ert--results-stats)
         (ewoc ert--results-ewoc)
         (node (ewoc-nth ewoc pos))
         (entry (ewoc-data node)))
    (ert--stats-set-test-and-result stats pos new-test nil)
    (setf (ert--ewoc-entry-test entry) new-test
          (aref ert--results-progress-bar-string pos) (ert-char-for-test-result
                                                       nil t))
    (ewoc-invalidate ewoc node))
  nil)

(defun ert--button-action-position ()
  "The buffer position where the last button action was triggered."
  (cond ((integerp last-command-event)
         (point))
        ((eventp last-command-event)
         (posn-point (event-start last-command-event)))
        (t (cl-assert nil))))

(defun ert--results-progress-bar-button-action (_button)
  "Jump to details for the test represented by the character clicked in BUTTON."
  (goto-char (ert--button-action-position))
  (ert-results-jump-between-summary-and-result))

(defun ert--results-progress-bar-button-help-echo (_window object pos)
  "Show the test name in `help-echo'."
  (format
   "%s\nmouse-2, RET: Reveal test result"
   (with-current-buffer object
     (save-excursion
       (goto-char pos)
       (or (ert-test-at-point) "")))))

(defun ert-results-rerun-all-tests ()
  "Re-run all tests, using the same selector.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (cl-assert (eql major-mode 'ert-results-mode))
  (let ((selector (ert--stats-selector ert--results-stats)))
    (ert-run-tests-interactively selector)))

(defun ert-results-rerun-test-at-point ()
  "Re-run the test at point.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (cl-destructuring-bind (test redefinition-state)
      (ert--results-test-at-point-allow-redefinition)
    (when (null test)
      (user-error "No test at point"))
    (let* ((stats ert--results-stats)
           (progress-message (format "Running %stest %S"
                                     (cl-ecase redefinition-state
                                       ((nil) "")
                                       (redefined "new definition of ")
                                       (deleted "deleted "))
                                     (ert-test-name test))))
      ;; Need to save and restore point manually here: When point is on
      ;; the first visible ewoc entry while the header is updated, point
      ;; moves to the top of the buffer.  This is undesirable, and a
      ;; simple `save-excursion' doesn't prevent it.
      (let ((point (point)))
        (unwind-protect
            (unwind-protect
                (progn
                  (message "%s..." progress-message)
                  (ert-run-or-rerun-test stats test
                                         ert--results-listener))
              (ert--results-update-stats-display ert--results-ewoc stats)
              (message "%s...%s"
                       progress-message
                       (let ((result (ert-test-most-recent-result test)))
                         (ert-string-for-test-result
                          result (ert-test-result-expected-p test result)))))
          (goto-char point))))))

(defun ert-results-rerun-test-at-point-debugging-errors ()
  "Re-run the test at point with `ert-debug-on-error' bound to t.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (let ((ert-debug-on-error t))
    (ert-results-rerun-test-at-point)))

(defun ert-results-pop-to-backtrace-for-test-at-point ()
  "Display the backtrace for the test at point.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (let* ((test (ert--results-test-at-point-no-redefinition t))
         (stats ert--results-stats)
         (pos (ert--stats-test-pos stats test))
         (result (aref (ert--stats-test-results stats) pos)))
    (cl-etypecase result
      (ert-test-passed (error "Test passed, no backtrace available"))
      (ert-test-result-with-condition
       (let ((buffer (get-buffer-create "*ERT Backtrace*")))
         (pop-to-buffer buffer)
         (unless (derived-mode-p 'backtrace-mode)
           (backtrace-mode))
         (setq backtrace-insert-header-function
               (lambda () (ert--insert-backtrace-header (ert-test-name test)))
               backtrace-frames (ert-test-result-with-condition-backtrace result))
         (backtrace-print)
         (goto-char (point-min)))))))

(defun ert--insert-backtrace-header (name)
  (insert (substitute-command-keys "Backtrace for test `"))
  (ert-insert-test-name-button name)
  (insert (substitute-command-keys "':\n")))

(defun ert-results-pop-to-messages-for-test-at-point ()
  "Display the part of the *Messages* buffer generated during the test at point.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (let* ((test (ert--results-test-at-point-no-redefinition t))
         (stats ert--results-stats)
         (pos (ert--stats-test-pos stats test))
         (result (aref (ert--stats-test-results stats) pos)))
    (let ((buffer (get-buffer-create "*ERT Messages*")))
      (pop-to-buffer buffer)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-simple-view-mode)
        (insert (ert-test-result-messages result))
        (goto-char (point-min))
        (insert (substitute-command-keys "Messages for test `"))
        (ert-insert-test-name-button (ert-test-name test))
        (insert (substitute-command-keys "':\n"))))))

(defun ert-results-pop-to-should-forms-for-test-at-point ()
  "Display the list of `should' forms executed during the test at point.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (let* ((test (ert--results-test-at-point-no-redefinition t))
         (stats ert--results-stats)
         (pos (ert--stats-test-pos stats test))
         (result (aref (ert--stats-test-results stats) pos)))
    (let ((buffer (get-buffer-create "*ERT list of should forms*")))
      (pop-to-buffer buffer)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-simple-view-mode)
        (if (null (ert-test-result-should-forms result))
            (insert "\n(No should forms during this test.)\n")
          (cl-loop for form-description
                   in (ert-test-result-should-forms result)
                   for i from 1 do
                   (insert "\n")
                   (insert (format "%s: " i))
                   (let ((begin (point)))
                     (ert--pp-with-indentation-and-newline form-description)
                     (ert--make-xrefs-region begin (point)))))
        (goto-char (point-min))
        (insert (substitute-command-keys
                 "`should' forms executed during test `"))
        (ert-insert-test-name-button (ert-test-name test))
        (insert (substitute-command-keys "':\n"))
        (insert "\n")
        (insert (concat "(Values are shallow copies and may have "
                        "looked different during the test if they\n"
                        "have been modified destructively.)\n"))
        (forward-line 1)))))

(defun ert-results-toggle-printer-limits-for-test-at-point ()
  "Toggle how much of the condition to print for the test at point.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (let* ((ewoc ert--results-ewoc)
         (node (ert--results-test-node-at-point))
         (entry (ewoc-data node)))
    (setf (ert--ewoc-entry-extended-printer-limits-p entry)
          (not (ert--ewoc-entry-extended-printer-limits-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ert-results-pop-to-timings ()
  "Display test timings for the last run.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (let* ((stats ert--results-stats)
         (buffer (get-buffer-create "*ERT timings*"))
         (data (cl-loop for test across (ert--stats-tests stats)
                        for start-time across (ert--stats-test-start-times
                                               stats)
                        for end-time across (ert--stats-test-end-times stats)
                        collect (list test
                                      (float-time (time-subtract
                                                   end-time start-time))))))
    (setq data (sort data (lambda (a b)
                            (> (cl-second a) (cl-second b)))))
    (pop-to-buffer buffer)
    (let ((inhibit-read-only t))
      (buffer-disable-undo)
      (erase-buffer)
      (ert-simple-view-mode)
      (if (null data)
          (insert "(No data)\n")
        (insert (format "%-3s  %8s %8s\n" "" "time" "cumul"))
        (cl-loop for (test time) in data
                 for cumul-time = time then (+ cumul-time time)
                 for i from 1 do
                 (progn
                   (insert (format "%3s: %8.3f %8.3f " i time cumul-time))
                   (ert-insert-test-name-button (ert-test-name test))
                   (insert "\n"))))
      (goto-char (point-min))
      (insert "Tests by run time (seconds):\n\n")
      (forward-line 1))))

;;;###autoload
(defun ert-describe-test (test-or-test-name)
  "Display the documentation for TEST-OR-TEST-NAME (a symbol or ert-test)."
  (interactive (list (ert-read-test-name-at-point "Describe test")))
  (let (test-name
        test-definition)
    (cl-etypecase test-or-test-name
      (symbol (setq test-name test-or-test-name
                    test-definition (ert-get-test test-or-test-name)))
      (ert-test (setq test-name (ert-test-name test-or-test-name)
                      test-definition test-or-test-name)))
    (help-setup-xref (list #'ert-describe-test test-or-test-name)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (insert (if test-name (format "%S" test-name) "<anonymous test>"))
          (insert " is a test")
          (let ((file-name (and test-name
                                (symbol-file test-name 'ert--test))))
            (when file-name
              (insert (format-message " defined in `%s'"
                                      (file-name-nondirectory file-name)))
              (save-excursion
                (re-search-backward (substitute-command-keys "`\\([^`']+\\)'"))
                (help-xref-button 1 'ert--test-name-button
                                  test-name file-name)))
            (insert ".")
            (fill-region-as-paragraph (point-min) (point))
            (insert "\n\n")
            (unless (and (ert-test-boundp test-name)
                         (eql (ert-get-test test-name) test-definition))
              (let ((begin (point)))
                (insert "Note: This test has been redefined or deleted, "
                        "this documentation refers to an old definition.")
                (fill-region-as-paragraph begin (point)))
              (insert "\n\n"))
            (insert (substitute-command-keys
                     (or (ert-test-documentation test-definition)
                         "It is not documented."))
                    "\n")
            ;; For describe-symbol-backends.
            (buffer-string)))))))

(defun ert-results-describe-test-at-point ()
  "Display the documentation of the test at point.

To be used in the ERT results buffer."
  (interactive nil ert-results-mode)
  (ert-describe-test (ert--results-test-at-point-no-redefinition t)))


;;; Actions on load/unload.

(require 'help-mode)
(add-to-list 'describe-symbol-backends
             `("ERT test" ,#'ert-test-boundp
               ,(lambda (s _b _f) (ert-describe-test s))))

(add-to-list 'find-function-regexp-alist '(ert--test . ert--find-test-regexp))
(add-to-list 'minor-mode-alist '(ert--current-run-stats
                                 (:eval
                                  (ert--tests-running-mode-line-indicator))))
(add-hook 'emacs-lisp-mode-hook #'ert--activate-font-lock-keywords)

(defun ert-unload-function ()
  "Unload function to undo the side-effects of loading ert.el."
  (ert--remove-from-list 'find-function-regexp-alist 'ert--test :key #'car)
  (ert--remove-from-list 'minor-mode-alist 'ert--current-run-stats :key #'car)
  (ert--remove-from-list 'emacs-lisp-mode-hook
                         'ert--activate-font-lock-keywords)
  nil)

;;; erts files.

(defun ert-test-erts-file (file &optional transform)
  "Parse FILE as a file containing before/after parts (an erts file).

This function puts the \"before\" section of an .erts file into a
temporary buffer, calls the TRANSFORM function, and then compares
the result with the \"after\" section.

See Info node `(ert) erts files' for more information on how to
write erts files."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((gen-specs (list (cons 'dummy t)
                           (cons 'code transform))))
      ;; Find the start of a test.
      (while (re-search-forward "^=-=\n" nil t)
        (setq gen-specs (ert-test--erts-test gen-specs file))
        ;; Search to the end of the test.
        (re-search-forward "^=-=-=\n")))))

(defun ert-test--erts-test (gen-specs file)
  (let* ((file-buffer (current-buffer))
         (specs (ert--erts-specifications (match-beginning 0)))
         (name (cdr (assq 'name specs)))
         (start-before (point))
         (end-after (if (re-search-forward "^=-=-=\n" nil t)
                        (match-beginning 0)
                      (point-max)))
         (skip (cdr (assq 'skip specs)))
         end-before start-after
         after after-point)
    (unless name
      (error "No name for test case"))
    (if (and skip
             (eval (car (read-from-string skip))))
        ;; Skipping this test.
        (goto-char end-after)
      ;; Do the test.
      (goto-char end-after)
      ;; We have a separate after section.
      (if (re-search-backward "^=-=\n" start-before t)
          (setq end-before (match-beginning 0)
                start-after (match-end 0))
        (setq end-before end-after
              start-after start-before))
      ;; Update persistent specs.
      (when-let* ((point-char (assq 'point-char specs)))
        (setq gen-specs
              (map-insert gen-specs 'point-char (cdr point-char))))
      (when-let* ((code (cdr (assq 'code specs))))
        (setq gen-specs
              (map-insert gen-specs 'code (car (read-from-string code)))))
      ;; Get the "after" strings.
      (with-temp-buffer
        (insert-buffer-substring file-buffer start-after end-after)
        (ert--erts-unquote)
        ;; Remove the newline at the end of the buffer.
        (when-let* ((no-newline (cdr (assq 'no-after-newline specs))))
          (goto-char (point-min))
          (when (re-search-forward "\n\\'" nil t)
            (delete-region (match-beginning 0) (match-end 0))))
        ;; Get the expected "after" point.
        (when-let* ((point-char (cdr (assq 'point-char gen-specs))))
          (goto-char (point-min))
          (when (search-forward point-char nil t)
            (delete-region (match-beginning 0) (match-end 0))
            (setq after-point (point))))
        (setq after (buffer-string)))
      ;; Do the test.
      (with-temp-buffer
        (insert-buffer-substring file-buffer start-before end-before)
        (ert--erts-unquote)
        ;; Remove the newline at the end of the buffer.
        (when-let* ((no-newline (cdr (assq 'no-before-newline specs))))
          (goto-char (point-min))
          (when (re-search-forward "\n\\'" nil t)
            (delete-region (match-beginning 0) (match-end 0))))
        (goto-char (point-min))
        ;; Place point in the specified place.
        (when-let* ((point-char (cdr (assq 'point-char gen-specs))))
          (when (search-forward point-char nil t)
            (delete-region (match-beginning 0) (match-end 0))))
        (let ((code (cdr (assq 'code gen-specs))))
          (unless code
            (error "No code to run the transform"))
          (funcall code))
        (unless (equal (buffer-string) after)
          (ert-fail (list (format "Mismatch in test \"%s\", file %s"
                                  name file)
                          (buffer-string)
                          after)))
        (when (and after-point
                   (not (= after-point (point))))
          (ert-fail (list (format "Point wrong in test \"%s\", expected point %d, actual %d, file %s"
                                  name
                                  after-point (point)
                                  file)
                          (buffer-string)))))))
  ;; Return the new value of the general specifications.
  gen-specs)

(defun ert--erts-unquote ()
  (goto-char (point-min))
  (while (re-search-forward "^\\=-=\\(-=\\)$" nil t)
    (delete-region (match-beginning 0) (1+ (match-beginning 0)))))

(defun ert--erts-specifications (end)
  "Find specifications before point (back to the previous test)."
  (save-excursion
    (goto-char end)
    (goto-char
     (if (re-search-backward "^=-=-=\n" nil t)
         (match-end 0)
       (point-min)))
    (let ((specs nil))
      (while (< (point) end)
        (if (looking-at "\\([^ \n\t:]+\\):\\([ \t]+\\)?\\(.*\\)")
            (let ((name (intern (downcase (match-string 1))))
                  (value (match-string 3)))
              (forward-line 1)
              (while (looking-at "[ \t]+\\(.*\\)")
                (setq value (concat value (match-string 1)))
                (forward-line 1))
              (push (cons name (substring-no-properties value)) specs))
          (forward-line 1)))
      (nreverse specs))))


;;; Buffer related helpers

(defun ert--text-button (string &rest properties)
  "Return a string containing STRING as a text button with PROPERTIES.

See `make-text-button'."
  (with-temp-buffer
    (insert string)
    (apply #'make-text-button (point-min) (point-max) properties)
    (buffer-string)))

(defun ert--format-test-buffer-name (base-name)
  "Compute a test buffer name based on BASE-NAME.

Helper function for `ert--test-buffers'."
  (format "*Test buffer (%s)%s*"
          (or (and (ert-running-test)
                   (ert-test-name (ert-running-test)))
              "<anonymous test>")
          (if base-name
              (format ": %s" base-name)
            "")))

(defvar ert--test-buffers (make-hash-table :weakness t)
  "Table of all test buffers.  Keys are the buffer objects, values are t.

The main use of this table is for `ert-kill-all-test-buffers'.
Not all buffers in this table are necessarily live, but all live
test buffers are in this table.")

(define-button-type 'ert--test-buffer-button
  'action #'ert--test-buffer-button-action
  'help-echo "mouse-2, RET: Pop to test buffer")

(defun ert--test-buffer-button-action (button)
  "Pop to the test buffer that BUTTON is associated with."
  (pop-to-buffer (button-get button 'ert--test-buffer)))

(defun ert--call-with-test-buffer (ert--base-name ert--thunk)
  "Helper function for `ert-with-test-buffer'.

Create a test buffer with a name based on ERT--BASE-NAME and run
ERT--THUNK with that buffer as current."
  (let* ((ert--buffer (generate-new-buffer
                    (ert--format-test-buffer-name ert--base-name)))
         (ert--button (ert--text-button (buffer-name ert--buffer)
                                  :type 'ert--test-buffer-button
                                  'ert--test-buffer ert--buffer)))
    (puthash ert--buffer 't ert--test-buffers)
    ;; We don't use `unwind-protect' here since we want to kill the
    ;; buffer only on success.
    (prog1 (with-current-buffer ert--buffer
             (ert-info (ert--button :prefix "Buffer: ")
               (funcall ert--thunk)))
      (kill-buffer ert--buffer)
      (remhash ert--buffer ert--test-buffers))))

(cl-defmacro ert-with-test-buffer ((&key ((:name name-form))
                                      ((:selected select-form)))
                                &body body)
  "Create a test buffer and run BODY in that buffer.

To be used in ERT tests.  If BODY finishes successfully, the test buffer
is killed; if there is an error, the test buffer is kept around for
further inspection.  The name of the buffer is derived from the name of
the test and the result of NAME-FORM.

If SELECT-FORM is non-nil, switch to the test buffer before running
BODY, as if body was in `ert-with-buffer-selected'.

The return value is the last form in BODY."
  (declare (debug ((":name" form) (":selected" form) def-body))
           (indent 1))
  `(ert--call-with-test-buffer
    ,name-form
    ,(if select-form
         `(lambda () (ert-with-buffer-selected (current-buffer)
                  ,@body))
       `(lambda () ,@body))))

(defun ert-kill-all-test-buffers ()
  "Kill all test buffers that are still live."
  (interactive)
  (let ((count 0))
    (maphash (lambda (buffer _dummy)
               (when (or (not (buffer-live-p buffer))
                         (kill-buffer buffer))
                 (incf count)))
             ert--test-buffers)
    (message "%s out of %s test buffers killed"
             count (hash-table-count ert--test-buffers)))
  ;; It could be that some test buffers were actually kept alive
  ;; (e.g., due to `kill-buffer-query-functions').  I'm not sure what
  ;; to do about this.  For now, let's just forget them.
  (clrhash ert--test-buffers)
  nil)

(cl-defmacro ert-with-buffer-selected (buffer-or-name &body body)
  "Display a buffer in a temporary selected window and run BODY.

If BUFFER-OR-NAME is nil, the current buffer is used.

The buffer is made the current buffer, and the temporary window
becomes the `selected-window', before BODY is evaluated.  The
modification hooks `before-change-functions' and
`after-change-functions' are not inhibited during the evaluation
of BODY, which makes it easier to use `execute-kbd-macro' to
simulate user interaction.  The window configuration is restored
before returning, even if BODY exits nonlocally.  The return
value is the last form in BODY."
  (declare (debug (form body)) (indent 1))
  `(save-window-excursion
     (with-current-buffer (or ,buffer-or-name (current-buffer))
       (with-selected-window (display-buffer (current-buffer))
         ,@body))))

(defun ert-call-with-buffer-renamed (buffer-name thunk)
  "Protect the buffer named BUFFER-NAME from side-effects and run THUNK.

Renames the buffer BUFFER-NAME to a new temporary name, creates a
new buffer named BUFFER-NAME, executes THUNK, kills the new
buffer, and renames the original buffer back to BUFFER-NAME.

This is useful if THUNK has undesirable side-effects on an Emacs
buffer with a fixed name such as *Messages*."
  (let ((new-buffer-name (generate-new-buffer-name
                          (format "%s orig buffer" buffer-name))))
    (with-current-buffer (get-buffer-create buffer-name)
      (rename-buffer new-buffer-name))
    (unwind-protect
        (progn
          (get-buffer-create buffer-name)
          (funcall thunk))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (with-current-buffer new-buffer-name
        (rename-buffer buffer-name)))))

(cl-defmacro ert-with-buffer-renamed ((buffer-name-form) &body body)
  "Protect the buffer named BUFFER-NAME from side-effects and run BODY.

See `ert-call-with-buffer-renamed' for details."
  (declare (indent 1))
  `(ert-call-with-buffer-renamed ,buffer-name-form (lambda () ,@body)))

;;; Obsolete

(define-obsolete-function-alias 'ert-equal-including-properties
  #'equal-including-properties "29.1")
(put 'ert-equal-including-properties 'ert-explainer
     'ert--explain-equal-including-properties)

(define-obsolete-function-alias 'ert--unload-function 'ert-unload-function "31.1")

(provide 'ert)

;;; ert.el ends here
