;;;   -*- lexical-binding: t; symbol-packages: t -*-
;;; cl-macs.el --- Common Lisp macros

;; Copyright (C) 1993, 2001-2025 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Old-Version: 2.02
;; Keywords: extensions
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

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains the portions of the Common Lisp extensions
;; package which should be autoloaded, but need only be present
;; if the compiler or interpreter is used---this file is not
;; necessary for executing compiled code.

;; See cl.el for Change Log.


;;; Code:

(require 'cl-lib)
(require 'macroexp)
;; `gv' is required here because cl-macs can be loaded before loaddefs.el.
(require 'gv)

(defmacro cl--pop2 (place)
  (declare (debug edebug-sexps))
  `(prog1 (car (cdr ,place))
     (setq ,place (cdr (cdr ,place)))))

(defvar cl--optimize-safety)
(defvar cl--optimize-speed)

;;; Initialization.

;; Place compiler macros at the beginning, otherwise uses of the corresponding
;; functions can lead to recursive-loads that prevent the calls from
;; being optimized.

;;;###autoload
(defun cl--compiler-macro-list* (_form arg &rest others)
  (let* ((args (reverse (cons arg others)))
	 (form (car args)))
    (while (setq args (cdr args))
      (setq form `(cons ,(car args) ,form)))
    form))

;;;###autoload
(define-obsolete-function-alias 'cl--compiler-macro-cXXr
  #'internal--compiler-macro-cXXr "25.1")

;;; Some predicates for analyzing Lisp forms.
;; These are used by various
;; macro expanders to optimize the results in certain common cases.

(defconst cl--simple-funcs '(car cdr nth aref elt if and or + - 1+ 1- min max
			    car-safe cdr-safe progn prog1 prog2))
(defconst cl--safe-funcs '(* / % length memq list vector vectorp
			  < > <= >= = error))

(defun cl--simple-expr-p (x &optional size)
  "Check if no side effects, and executes quickly."
  (or size (setq size 10))
  (if (and (consp x) (not (memq (car x) '(quote function cl-function))))
      (and (symbolp (car x))
	   (or (memq (car x) cl--simple-funcs)
	       (get (car x) 'side-effect-free))
	   (progn
	     (setq size (1- size))
	     (while (and (setq x (cdr x))
			 (setq size (cl--simple-expr-p (car x) size))))
	     (and (null x) (>= size 0) size)))
    (and (> size 0) (1- size))))

(defun cl--simple-exprs-p (xs)
  "Map `cl--simple-expr-p' to each element of list XS."
  (while (and xs (cl--simple-expr-p (car xs)))
    (setq xs (cdr xs)))
  (not xs))

(defun cl--safe-expr-p (x)
  "Check if no side effects."
  (or (not (and (consp x) (not (memq (car x) '(quote function cl-function)))))
      (and (symbolp (car x))
	   (or (memq (car x) cl--simple-funcs)
	       (memq (car x) cl--safe-funcs)
	       (get (car x) 'side-effect-free))
	   (progn
	     (while (and (setq x (cdr x)) (cl--safe-expr-p (car x))))
	     (null x)))))

(defun cl--const-expr-p (x)
  "Check if X is constant (i.e., no side effects or dependencies).

See `macroexp-const-p' for similar functionality without cl-lib dependency."
  (cond ((consp x)
	 (or (eq (car x) 'quote)
	     (and (memq (car x) '(function cl-function))
		  (or (symbolp (nth 1 x))
		      (and (eq (car-safe (nth 1 x)) 'lambda) 'func)))))
	((symbolp x) (and (memq x '(nil t)) t))
	(t t)))

(defun cl--const-expr-val (x)
  "Return the value of X known at compile-time.
If X is not known at compile time, return nil.  Before testing
whether X is known at compile time, macroexpand it completely in
`macroexpand-all-environment'."
  (let ((x (macroexpand-all x macroexpand-all-environment)))
    (if (macroexp-const-p x)
        (if (consp x) (nth 1 x) x))))

(defun cl--expr-contains (x y)
  "Count number of times X refers to Y.  Return nil for 0 times."
  ;; FIXME: This is naive, and it will cl-count Y as referred twice in
  ;; (let ((Y 1)) Y) even though it should be 0.  Also it is often called on
  ;; non-macroexpanded code, so it may also miss some occurrences that would
  ;; only appear in the expanded code.
  (cond ((equal y x) 1)
	((and (consp x) (not (memq (car x) '(quote function cl-function))))
	 (let ((sum 0))
	   (while (consp x)
	     (setq sum (+ sum (or (cl--expr-contains (pop x) y) 0))))
	   (setq sum (+ sum (or (cl--expr-contains x y) 0)))
	   (and (> sum 0) sum)))
	(t nil)))

(defun cl--expr-contains-any (x y)
  (while (and y (not (cl--expr-contains x (car y)))) (pop y))
  y)

(defun cl--expr-depends-p (x y)
  "Check whether X may depend on any of the symbols in Y."
  (and (not (macroexp-const-p x))
       (or (not (cl--safe-expr-p x)) (cl--expr-contains-any x y))))

;;; Symbols.

(defvar cl--gensym-counter 0)
;;;###autoload
(defun cl-gensym (&optional prefix)
  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\"."
  (declare (obsolete gensym "31.1"))
  (let ((pfix (if (stringp prefix) prefix "G"))
	(num (if (integerp prefix) prefix
	       (prog1 cl--gensym-counter
		 (setq cl--gensym-counter (1+ cl--gensym-counter))))))
    (make-symbol (format "%s%d" pfix num))))

(defvar cl--gentemp-counter 0)
;;;###autoload
(defun cl-gentemp (&optional prefix)
  "Generate a new interned symbol with a unique name.
The name is made by appending a number to PREFIX, default \"T\"."
  (let ((pfix (if (stringp prefix) prefix "T"))
	name)
    (while (intern-soft (setq name (format "%s%d" pfix cl--gentemp-counter)))
      (setq cl--gentemp-counter (1+ cl--gentemp-counter)))
    (intern name)))


;;; Program structure.

(def-edebug-elem-spec 'cl-declarations
  '(&rest ("cl-declare" &rest sexp)))

(def-edebug-elem-spec 'cl-declarations-or-string
  '(lambda-doc &or ("declare" def-declarations) cl-declarations))

(def-edebug-elem-spec 'cl-lambda-list
  '(([&rest cl-lambda-arg]
    [&optional ["&optional" cl-&optional-arg &rest cl-&optional-arg]]
    [&optional ["&rest" cl-lambda-arg]]
    [&optional ["&key" [cl-&key-arg &rest cl-&key-arg]
		&optional "&allow-other-keys"]]
    [&optional ["&aux" &rest
		&or (cl-lambda-arg &optional def-form) arg]]
    . [&or arg nil])))

(def-edebug-elem-spec 'cl-&optional-arg
  '(&or (cl-lambda-arg &optional def-form arg) arg))

(def-edebug-elem-spec 'cl-&key-arg
  '(&or ([&or (symbolp cl-lambda-arg) arg] &optional def-form arg) arg))

(def-edebug-elem-spec 'cl-lambda-arg
  '(&or arg cl-lambda-list1))

(def-edebug-elem-spec 'cl-lambda-list1
  '(([&optional ["&whole" arg]] ;; only allowed at lower levels
     [&rest cl-lambda-arg]
     [&optional ["&optional" cl-&optional-arg &rest cl-&optional-arg]]
     [&optional ["&rest" cl-lambda-arg]]
     [&optional ["&key" cl-&key-arg &rest cl-&key-arg
                 &optional "&allow-other-keys"]]
     [&optional ["&aux" &rest
                 &or (cl-lambda-arg &optional def-form) arg]]
     . [&or arg nil])))

(def-edebug-elem-spec 'cl-type-spec '(sexp))

(defconst cl--lambda-list-keywords
  '(&optional &rest &key &allow-other-keys &aux &whole &body &environment))

;; Internal hacks used in formal arg lists:
;; - &cl-quote: Added to formal-arglists to mean that any default value
;;   mentioned in the formal arglist should be considered as implicitly
;;   quoted rather than evaluated.  This is used in `cl-defsubst' when
;;   performing compiler-macro-expansion, since at that time the
;;   arguments hold expressions rather than values.
;; - &cl-defs (DEF . DEFS): Gives the default value to use for missing
;;   optional arguments which don't have an explicit default value.
;;   DEFS is an alist mapping vars to their default default value.
;;   and DEF is the default default to use for all other vars.

(defvar cl--bind-block) ;Name of surrounding block, only use for `signal' data.
(defvar cl--bind-defs) ;(DEF . DEFS) giving the "default default" for optargs.
(defvar cl--bind-enquote)      ;Non-nil if &cl-quote was in the formal arglist!
(defvar cl--bind-lets) (defvar cl--bind-forms)

(defun cl--slet (bindings body &optional nowarn)
  "Like `cl--slet*' but for \"parallel let\"."
  (let ((dyns nil)) ;Vars declared as dynbound among the bindings?
    (when lexical-binding
      (dolist (binding bindings) ;; `seq-some' lead to bootstrap problems.
        (when (macroexp--dynamic-variable-p (car binding))
          (push (car binding) dyns))))
    (cond
     (dyns
      (let ((form `(funcall (lambda (,@(mapcar #'car bindings))
                              ,@(macroexp-unprogn body))
                            ,@(mapcar #'cadr bindings))))
        (if (not nowarn) form
          `(with-suppressed-warnings ((lexical ,@dyns)) ,form))))
     ((null (cdr bindings))
      (macroexp-let* bindings body))
     (t `(let ,bindings ,@(macroexp-unprogn body))))))

(defun cl--slet* (bindings body)
  "Like `macroexp-let*' but uses static scoping for all the BINDINGS."
  (if (null bindings) body
    (cl--slet `(,(car bindings)) (cl--slet* (cdr bindings) body))))

(defun cl--transform-lambda (form bind-block)
  "Transform a function form FORM of name BIND-BLOCK.
BIND-BLOCK is the name of the symbol to which the function will be bound,
and which will be used for the name of the `cl-block' surrounding the
function's body.
FORM is of the form (ARGS . BODY)."
  (let* ((args (car form)) (body (cdr form)) (orig-args args)
	 (cl--bind-block bind-block) (cl--bind-defs nil) (cl--bind-enquote nil)
         (parsed-body (macroexp-parse-body body))
	 (header (car parsed-body)) (simple-args nil))
    (setq body (cdr parsed-body))
    ;; "(. X) to (&rest X)" conversion already done in cl--do-arglist, but we
    ;; do it here as well, so as to be able to see if we can avoid
    ;; cl--do-arglist.
    (setq args (if (listp args) (cl-copy-list args) (list '&rest args)))
    (let ((p (last args))) (if (cdr p) (setcdr p (list '&rest (cdr p)))))
    (let ((cl-defs (memq '&cl-defs args)))
      (when cl-defs
        (setq cl--bind-defs (cadr cl-defs))
	;; Remove "&cl-defs DEFS" from args.
        (setcdr cl-defs (cddr cl-defs))
	(setq args (delq '&cl-defs args))))
    (if (setq cl--bind-enquote (memq '&cl-quote args))
	(setq args (delq '&cl-quote args)))
    (if (memq '&whole args) (error "&whole not currently implemented"))
    (let* ((p (memq '&environment args))
           (v (cadr p)))
      (if p (setq args (nconc (delq (car p) (delq v args))
                              `(&aux (,v macroexpand-all-environment))))))
    ;; Take away all the simple args whose parsing can be handled more
    ;; efficiently by a plain old `lambda' than the manual parsing generated
    ;; by `cl--do-arglist'.
    (let ((optional nil))
      (while (and args (symbolp (car args))
                  (not (memq (car args) '(nil &rest &body &key &aux)))
                  (or (not optional)
                      ;; Optional args whose default is nil are simple.
                      (null (nth 1 (assq (car args) (cdr cl--bind-defs)))))
                  (not (and (eq (car args) '&optional) (setq optional t)
                            (car cl--bind-defs))))
        (push (pop args) simple-args))
      (when optional
        (if args (push '&optional args))
        ;; Don't keep a dummy trailing &optional without actual optional args.
        (if (eq '&optional (car simple-args)) (pop simple-args))))
    (or (eq cl--bind-block 'cl-none)
	(setq body (list `(cl-block ,cl--bind-block ,@body))))
    (let* ((cl--bind-lets nil) (cl--bind-forms nil)
           (rest-args
            (cond
             ((null args) nil)
             ((eq (car args) '&aux)
              (cl--do-&aux args)
              (setq cl--bind-lets (nreverse cl--bind-lets))
              nil)
             (t ;; `simple-args' doesn't handle all the parsing that we need,
              ;; so we pass the rest to cl--do-arglist which will do
              ;; "manual" parsing.
              (let ((slen (length simple-args))
                    (usage-str
                      ;; Macro expansion can take place in the middle of
                      ;; apparently harmless computation, so it should not
                      ;; touch the match-data.
                      (save-match-data
                        (help--docstring-quote
                         (let ((print-gensym nil) (print-quoted t)
                               (print-escape-newlines t))
                           (format "%S" (cons 'fn (cl--make-usage-args
                                                   orig-args))))))))
                (when (memq '&optional simple-args)
                  (decf slen))
                (setq header
                      (cons
                       (if (eq :documentation (car-safe (car header)))
                           `(:documentation (help-add-fundoc-usage
                                             ,(cadr (pop header))
                                             ,usage-str))
                         (help-add-fundoc-usage
                          (if (stringp (car header)) (pop header))
                          ;; Be careful with make-symbol and (back)quote,
                          ;; see bug#12884.
                          usage-str))
                       header))
                ;; FIXME: we'd want to choose an arg name for the &rest param
                ;; and pass that as `expr' to cl--do-arglist, but that ends up
                ;; generating code with a redundant let-binding, so we instead
                ;; pass a dummy and then look in cl--bind-lets to find what var
                ;; this was bound to.
                (cl--do-arglist args :dummy slen)
                (setq cl--bind-lets (nreverse cl--bind-lets))
                ;; (cl-assert (eq :dummy (nth 1 (car cl--bind-lets))))
                (list '&rest (car (pop cl--bind-lets))))))))
      `((,@(nreverse simple-args) ,@rest-args)
        ,@header
        ;; Function arguments are unconditionally statically scoped (bug#47552).
        ,(cl--slet* cl--bind-lets
                    (macroexp-progn
                     `(,@(nreverse cl--bind-forms)
                       ,@body)))))))

;;;###autoload
(defmacro cl-defun (name args &rest body)
  "Define NAME as a function.
Like normal `defun', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (cl-block NAME ...).

The full form of a Common Lisp function argument list is

   (VAR...
    [&optional (VAR [INITFORM [SVAR]])...]
    [&rest|&body VAR]
    [&key (([KEYWORD] VAR) [INITFORM [SVAR]])... [&allow-other-keys]]
    [&aux (VAR [INITFORM])...])

VAR may be replaced recursively with an argument list for
destructuring, `&whole' is supported within these sublists.  If
SVAR, INITFORM, and KEYWORD are all omitted, then `(VAR)' may be
written simply `VAR'.  See the Info node `(cl)Argument Lists' for
more details.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (debug
            ;; Same as defun but use cl-lambda-list.
            (&define [&name symbolp]
                     cl-lambda-list
                     cl-declarations-or-string
                     [&optional ("interactive" interactive)]
                     def-body))
           (doc-string 3)
           (indent 2))
  `(defun ,name ,@(cl--transform-lambda (cons args body) name)))

;;;###autoload
(defmacro cl-iter-defun (name args &rest body)
  "Define NAME as a generator function.
Like normal `iter-defun', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (cl-block NAME ...).

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (debug
            ;; Same as iter-defun but use cl-lambda-list.
            (&define [&name sexp]   ;Allow (setf ...) additionally to symbols.
                     cl-lambda-list
                     cl-declarations-or-string
                     [&optional ("interactive" interactive)]
                     def-body))
           (doc-string 3)
           (indent 2))
  (require 'generator)
  `(iter-defun ,name ,@(cl--transform-lambda (cons args body) name)))

;; The lambda list for macros is different from that of normal lambdas.

;; `cl-macro-list' is shared between a few different use cases that
;; don't all support exactly the same set of special keywords: the
;; debug spec accepts hence a superset of what the macros
;; actually support.
;; For example &environment is only allowed as first or last items in the
;; top level list.

(def-edebug-elem-spec 'cl-macro-list
  '(([&optional "&whole" arg] ; Only for compiler-macros or at lower levels.
     [&optional "&environment" arg]     ; Only at top-level.
     [&rest cl-macro-arg]
     [&optional ["&optional" &rest
		 &or (cl-macro-arg &optional def-form cl-macro-arg) arg]]
     [&optional [[&or "&rest" "&body"] cl-macro-arg]]
     [&optional ["&key" [&rest
			 [&or ([&or (symbolp cl-macro-arg) arg]
			       &optional def-form cl-macro-arg)
			      arg]]
		 &optional "&allow-other-keys"]]
     [&optional ["&aux" &rest
		 &or (cl-macro-arg &optional def-form) arg]]
     [&optional "&environment" arg]     ; Only at top-level.
     . [&or arg nil]                    ; Only allowed at lower levels.
     )))

(def-edebug-elem-spec 'cl-macro-arg
  '(&or arg cl-macro-list))

;;;###autoload
(defmacro cl-defmacro (name args &rest body)
  "Define NAME as a macro.
Like normal `defmacro', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (cl-block NAME ...).

The full form of a Common Lisp macro argument list is

   (VAR...
    [&optional (VAR [INITFORM [SVAR]])...]
    [&rest|&body VAR]
    [&key (([KEYWORD] VAR) [INITFORM [SVAR]])... [&allow-other-keys]]
    [&aux (VAR [INITFORM])...]
    [&environment VAR])

VAR may be replaced recursively with an argument list for
destructuring, `&whole' is supported within these sublists.  If
SVAR, INITFORM, and KEYWORD are all omitted, then `(VAR)' may be
written simply `VAR'.  See the Info node `(cl)Argument Lists' for
more details.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (debug
            (&define name cl-macro-list cl-declarations-or-string def-body))
           (doc-string 3)
           (indent 2))
  `(defmacro ,name ,@(cl--transform-lambda (cons args body) name)))

(def-edebug-elem-spec 'cl-lambda-expr
  '(&define ("lambda" cl-lambda-list
	     cl-declarations-or-string
	     [&optional ("interactive" interactive)]
	     def-body)))

;; Redefine function-form to also match cl-function
(def-edebug-elem-spec 'function-form
  ;; form at the end could also handle "function",
  ;; but recognize it specially to avoid wrapping function forms.
  '(&or ([&or "quote" "function"] &or symbolp lambda-expr)
        ("cl-function" cl-function)
        form))

;;;###autoload
(defmacro cl-function (func)
  "Introduce a function.
Like normal `function', except that if argument is a lambda form,
its argument list allows full Common Lisp conventions."
  (declare (debug (&or symbolp cl-lambda-expr)))
  (if (eq (car-safe func) 'lambda)
      `(function (lambda . ,(cl--transform-lambda (cdr func) 'cl-none)))
    `(function ,func)))

(defun cl--make-usage-var (x)
  "X can be a var or a (destructuring) lambda-list."
  (cond
   ((symbolp x) (make-symbol (upcase (symbol-name x))))
   ((consp x) (cl--make-usage-args x))
   (t x)))

(defun cl--make-usage-args (arglist)
  (let ((aux (ignore-errors (cl-position '&aux arglist))))
    (when aux
      ;; `&aux' args aren't arguments, so let's just drop them from the
      ;; usage info.
      (setq arglist (cl-subseq arglist 0 aux))))
  (if (not (proper-list-p arglist))
      (let* ((last (last arglist))
             (tail (cdr last)))
        (unwind-protect
            (progn
              (setcdr last nil)
              (nconc (cl--make-usage-args arglist) (cl--make-usage-var tail)))
          (setcdr last tail)))
    ;; `orig-args' can contain &cl-defs.
    (let ((x (memq '&cl-defs arglist)))
      (when x (setq arglist (delq (car x) (remq (cadr x) arglist)))))
    (let ((state nil))
      (mapcar (lambda (x)
                (cond
                 ((symbolp x)
                  (let ((first (aref (symbol-name x) 0)))
                    (if (eq ?\& first)
                        (setq state x)
                      ;; Strip a leading underscore, since it only
                      ;; means that this argument is unused.
                      (make-symbol (upcase (if (eq ?_ first)
                                               (substring (symbol-name x) 1)
                                             (symbol-name x)))))))
                 ((not (consp x)) x)
                 ((memq state '(nil &rest)) (cl--make-usage-args x))
                 (t      ;(VAR INITFORM SVAR) or ((KEYWORD VAR) INITFORM SVAR).
                  (cl-list*
                   (if (and (consp (car x)) (eq state '&key))
                       (list (caar x) (cl--make-usage-var (nth 1 (car x))))
                     (cl--make-usage-var (car x)))
                   (nth 1 x)                        ;INITFORM.
                   (cl--make-usage-args (nthcdr 2 x)) ;SVAR.
                   ))))
              arglist))))

(defun cl--do-&aux (args)
  (while (and (eq (car args) '&aux) (pop args))
    (while (and args (not (memq (car args) cl--lambda-list-keywords)))
      (if (consp (car args))
          (if (and cl--bind-enquote (cadar args))
              (cl--do-arglist (caar args)
                              `',(cadr (pop args)))
            (cl--do-arglist (caar args) (cadr (pop args))))
        (cl--do-arglist (pop args) nil))))
  (if args (error "Malformed argument list ends with: %S" args)))

(defun cl--do-arglist (args expr &optional num)   ; uses cl--bind-*
  (if (nlistp args)
      (if (or (memq args cl--lambda-list-keywords) (not (symbolp args)))
	  (error "Invalid argument name: %s" args)
	(push (list args expr) cl--bind-lets))
    (setq args (cl-copy-list args))
    (let ((p (last args))) (if (cdr p) (setcdr p (list '&rest (cdr p)))))
    (let ((p (memq '&body args))) (if p (setcar p '&rest)))
    (if (memq '&environment args) (error "&environment used incorrectly"))
    (let ((restarg (memq '&rest args))
	  (safety (if (macroexp-compiling-p) cl--optimize-safety 3))
	  (keys t)
	  (laterarg nil) (exactarg nil) minarg)
      (or num (setq num 0))
      (setq restarg (if (listp (cadr restarg))
                        (make-symbol "--cl-rest--")
                      (cadr restarg)))
      (push (list restarg expr) cl--bind-lets)
      (if (eq (car args) '&whole)
	  (push (list (cl--pop2 args) restarg) cl--bind-lets))
      (let ((p args))
	(setq minarg restarg)
	(while (and p (not (memq (car p) cl--lambda-list-keywords)))
	  (or (eq p args) (setq minarg (list 'cdr minarg)))
	  (setq p (cdr p)))
	(if (memq (car p) '(nil &aux))
	    (setq minarg `(= (length ,restarg)
                             ,(length (cl-ldiff args p)))
		  exactarg (not (eq args p)))))
      (while (and args (not (memq (car args) cl--lambda-list-keywords)))
	(let ((poparg (list (if (or (cdr args) (not exactarg)) 'pop 'car-safe)
			    restarg)))
	  (cl--do-arglist
	   (pop args)
	   (if (or laterarg (= safety 0)) poparg
	     `(if ,minarg ,poparg
                (signal 'wrong-number-of-arguments
                        (list ,(and (not (eq cl--bind-block 'cl-none))
                                    `',cl--bind-block)
                              (length ,restarg)))))))
	(setq num (1+ num) laterarg t))
      (while (and (eq (car args) '&optional) (pop args))
	(while (and args (not (memq (car args) cl--lambda-list-keywords)))
	  (let ((arg (pop args)))
	    (or (consp arg) (setq arg (list arg)))
	    (if (cddr arg) (cl--do-arglist (nth 2 arg) `(and ,restarg t)))
	    (let ((def (if (cdr arg) (nth 1 arg)
			 (or (car cl--bind-defs)
			     (nth 1 (assq (car arg) cl--bind-defs)))))
		  (poparg `(pop ,restarg)))
	      (and def cl--bind-enquote (setq def `',def))
	      (cl--do-arglist (car arg)
			     (if def `(if ,restarg ,poparg ,def) poparg))
	      (setq num (1+ num))))))
      (if (eq (car args) '&rest)
	  (let ((arg (cl--pop2 args)))
	    (if (consp arg) (cl--do-arglist arg restarg)))
	(or (eq (car args) '&key) (= safety 0) exactarg
	    (push `(if ,restarg
                       (signal 'wrong-number-of-arguments
                               (list
                                ,(and (not (eq cl--bind-block 'cl-none))
                                      `',cl--bind-block)
                                (+ ,num (length ,restarg)))))
                  cl--bind-forms)))
      (while (and (eq (car args) '&key) (pop args))
        (unless (listp keys) (setq keys nil))
	(while (and args (not (memq (car args) cl--lambda-list-keywords)))
	  (let ((arg (pop args)))
	    (or (consp arg) (setq arg (list arg)))
	    (let* ((karg (if (consp (car arg)) (caar arg)
                           (let ((name (symbol-name (car arg))))
                             ;; Strip a leading underscore, since it only
                             ;; means that this argument is unused, but
                             ;; shouldn't affect the key's name (bug#12367).
                             (if (eq ?_ (aref name 0))
                                 (setq name (substring name 1)))
                             (intern (format ":%s" name)))))
                   (varg (if (consp (car arg)) (cadar arg) (car arg)))
		   (def (if (cdr arg) (cadr arg)
                          ;; The ordering between those two or clauses is
                          ;; irrelevant, since in practice only one of the two
                          ;; is ever non-nil (the car is only used for
                          ;; cl-deftype which doesn't use the cdr).
			  (or (car cl--bind-defs)
                              (cadr (assq varg cl--bind-defs)))))
                   (look `(plist-member ,restarg ',karg)))
	      (and def cl--bind-enquote (setq def `',def))
	      (if (cddr arg)
		  (let* ((temp (or (nth 2 arg) (make-symbol "--cl-var--")))
			 (val `(car (cdr ,temp))))
		    (cl--do-arglist temp look)
		    (cl--do-arglist varg
				   `(if ,temp
                                        (prog1 ,val (setq ,temp t))
                                      ,def)))
		(cl--do-arglist
		 varg
		 `(car (cdr ,(if (null def)
				 look
			       `(or ,look
                                    ,(if (eq (cl--const-expr-p def) t)
					 `'(nil ,(cl--const-expr-val def))
				       `(list nil ,def))))))))
	      (push karg keys)))))
      (when (consp keys) (setq keys (nreverse keys)))
      (or (and (eq (car args) '&allow-other-keys) (pop args))
	  (= safety 0)
          (cond
           ((eq keys t) nil)            ;No &keys at all
           ((null keys)                 ;A &key but no actual keys specified.
            (push `(when ,restarg
                     (error ,(format "Keyword argument %%s not one of %s"
                                     keys)
                            (car ,restarg)))
                  cl--bind-forms))
           (t
	    (let* ((var (make-symbol "--cl-keys--"))
		   (allow '(:allow-other-keys))
		   (check `(while ,var
                             (cond
                              ((memq (car ,var) ',(append keys allow))
                               (unless (cdr ,var)
                                 (error "Missing argument for %s" (car ,var)))
                               (setq ,var (cdr (cdr ,var))))
                              ((car (cdr (memq (quote ,@allow) ,restarg)))
                               (setq ,var nil))
                              (t
                               (error
                                ,(format "Keyword argument %%s not one of %s"
                                         keys)
                                (car ,var)))))))
	      (push `(let ((,var ,restarg)) ,check) cl--bind-forms)))))
      (cl--do-&aux args)
      nil)))

(defun cl--arglist-args (args)
  (if (nlistp args) (list args)
    (let ((res nil) (kind nil) arg)
      (while (consp args)
	(setq arg (pop args))
	(if (memq arg cl--lambda-list-keywords) (setq kind arg)
	  (if (eq arg '&cl-defs) (pop args)
	    (and (consp arg) kind (setq arg (car arg)))
	    (and (consp arg) (cdr arg) (eq kind '&key) (setq arg (cadr arg)))
	    (setq res (nconc res (cl--arglist-args arg))))))
      (nconc res (and args (list args))))))

;;;###autoload
(defmacro cl-destructuring-bind (args expr &rest body)
  "Bind the variables in ARGS to the result of EXPR and execute BODY."
  (declare (indent 2)
           (debug (&define cl-macro-list def-form cl-declarations def-body)))
  (let* ((cl--bind-lets nil)
         (cl--bind-forms nil)
	 (cl--bind-defs nil)
         (cl--bind-block args)
         (cl--bind-enquote nil))
    (cl--do-arglist (or args '(&aux)) expr)
    (macroexp-let* (nreverse cl--bind-lets)
                   (macroexp-progn (append (nreverse cl--bind-forms) body)))))


;;; The `cl-eval-when' form.

(defvar cl--not-toplevel nil)

;;;###autoload
(defmacro cl-eval-when (when &rest body)
  "Control when BODY is evaluated.
If `compile' is in WHEN, BODY is evaluated when compiled at top-level.
If `load' is in WHEN, BODY is evaluated when loaded after top-level compile.
If `eval' is in WHEN, BODY is evaluated when interpreted or at non-top-level.

\(fn (WHEN...) BODY...)"
  (declare (indent 1) (debug (sexp body)))
  (if (and (macroexp-compiling-p)
	   (not cl--not-toplevel) (not (boundp 'for-effect))) ;Horrible kludge.
      (let ((comp (or (memq 'compile when) (memq :compile-toplevel when)))
	    (cl--not-toplevel t))
	(if (or (memq 'load when) (memq :load-toplevel when))
	    (if comp (cons 'progn (mapcar #'cl--compile-time-too body))
	      `(if nil nil ,@body))
	  (progn (if comp (eval (cons 'progn body) lexical-binding)) nil)))
    (and (or (memq 'eval when) (memq :execute when))
	 (cons 'progn body))))

(defun cl--compile-time-too (form)
  (or (and (symbolp (car-safe form)) (get (car-safe form) 'byte-hunk-handler))
      (setq form (macroexpand
		  form (cons '(cl-eval-when) macroexpand-all-environment))))
  (cond ((eq (car-safe form) 'progn)
	 (cons 'progn (mapcar #'cl--compile-time-too (cdr form))))
	((eq (car-safe form) 'cl-eval-when)
	 (let ((when (nth 1 form)))
	   (if (or (memq 'eval when) (memq :execute when))
	       `(cl-eval-when (compile ,@when) ,@(cddr form))
	     form)))
	(t (eval form lexical-binding) form)))

;;;###autoload
(defmacro cl-load-time-value (form &optional _read-only)
  "Like `progn', but evaluates the body at load time.
The result of the body appears to the compiler as a quoted constant."
  (declare (debug (form &optional sexp)))
  (if (macroexp-compiling-p)
      (let* ((temp (cl-gentemp "--cl-load-time--"))
	     (set `(setq ,temp ,form)))
	(if (and (fboundp 'byte-compile-file-form-defmumble)
		 (boundp 'this-kind) (boundp 'that-one))
            ;; Else, we can't output right away, so we have to delay it to the
            ;; next time we're at the top-level.
            ;; FIXME: Use advice-add/remove.
            (fset 'byte-compile-file-form
                  (let ((old (symbol-function 'byte-compile-file-form)))
                    (lambda (form)
                      (fset 'byte-compile-file-form old)
                      (byte-compile-file-form set)
                      (byte-compile-file-form form))))
          ;; If we're not in the middle of compiling something, we can
          ;; output directly to byte-compile-outbuffer, to make sure
          ;; temp is set before we use it.
          (print set byte-compile--outbuffer))
	temp)
    `',(eval form lexical-binding)))


;;; Conditional control structures.

;;;###autoload
(defmacro cl-case (expr &rest clauses)
  "Eval EXPR and choose among clauses on that value.
Each clause looks like (KEYLIST BODY...).  EXPR is evaluated and
compared against each key in each KEYLIST; the corresponding BODY
is evaluated.  If no clause succeeds, this macro returns nil.  A
single non-nil atom may be used in place of a KEYLIST of one
atom.  A KEYLIST of t or `otherwise' is allowed only in the final
clause, and matches if no other keys match.  Key values are
compared by `eql'.

\(fn EXPR (KEYLIST BODY...)...)"
  (declare (indent 1) (debug (form &rest (sexp body))))
  (macroexp-let2 macroexp-copyable-p temp expr
    (let* ((head-list nil)
           (has-otherwise nil))
      `(cond
        ,@(mapcar
           (lambda (c)
             (cons (cond (has-otherwise
                          (error "Misplaced t or `otherwise' clause"))
                         ((memq (car c) '(t otherwise))
                          (setq has-otherwise t)
                          t)
                         ((eq (car c) 'cl--ecase-error-flag)
                          `(error "cl-ecase failed: %s, %s"
                                  ,temp ',(reverse head-list)))
                         ((null (car c))
                          (macroexp-warn-and-return
                           "Case nil will never match"
                           nil 'suspicious))
                         ((and (consp (car c)) (cdar c) (not (cddar c))
                               (memq (caar c) '(quote function)))
                          (macroexp-warn-and-return
                           (format-message
                            (concat "Case %s will match `%s'.  If "
                                    "that's intended, write %s "
                                    "instead.  Otherwise, don't "
                                    "quote `%s'.")
                            (car c) (caar c) (list (cadar c) (caar c))
                            (cadar c))
                           `(cl-member ,temp ',(car c)) 'suspicious))
                         ((listp (car c))
                          (setq head-list (append (car c) head-list))
                          `(cl-member ,temp ',(car c)))
                         (t
                          (if (memq (car c) head-list)
                              (error "Duplicate key in case: %s"
                                     (car c)))
                          (push (car c) head-list)
                          `(eql ,temp ',(car c))))
                   (or (cdr c) '(nil))))
           clauses)))))

;;;###autoload
(defmacro cl-ecase (expr &rest clauses)
  "Like `cl-case', but error if no case fits.
`otherwise'-clauses are not allowed.
\n(fn EXPR (KEYLIST BODY...)...)"
  (declare (indent 1) (debug cl-case))
  `(cl-case ,expr ,@clauses (cl--ecase-error-flag)))

;;;###autoload
(defmacro cl-typecase (expr &rest clauses)
  "Eval EXPR and choose among clauses on that value.
Each clause looks like (TYPE BODY...).  EXPR is evaluated and, if it
satisfies TYPE, the corresponding BODY is evaluated.  If no clause succeeds,
this macro returns nil.  A TYPE of t or `otherwise' is allowed only in the
final clause, and matches if no other keys match.
\n(fn EXPR (TYPE BODY...)...)"
  (declare (indent 1)
           (debug (form &rest ([&or cl-type-spec "otherwise"] body))))
  (macroexp-let2 macroexp-copyable-p temp expr
    (let* ((type-list nil))
      (cons
       'cond
       (mapcar
        (lambda (c)
          (cons (cond ((eq (car c) 'otherwise) t)
                      ((eq (car c) 'cl--ecase-error-flag)
                       `(error "cl-etypecase failed: %s, %s"
                               ,temp ',(reverse type-list)))
                      (t
                       (push (car c) type-list)
                       `(cl-typep ,temp ',(car c))))
                (or (cdr c) '(nil))))
        clauses)))))

;;;###autoload
(defmacro cl-etypecase (expr &rest clauses)
  "Like `cl-typecase', but error if no case fits.
`otherwise'-clauses are not allowed.
\n(fn EXPR (TYPE BODY...)...)"
  (declare (indent 1) (debug cl-typecase))
  `(cl-typecase ,expr ,@clauses (cl--ecase-error-flag)))


;;; Blocks and exits.

;;;###autoload
(defmacro cl-block (name &rest body)
  "Define a lexically-scoped block named NAME.
NAME may be any symbol.  Code inside the BODY forms can call `cl-return-from'
to jump prematurely out of the block.  This differs from `catch' and `throw'
in two respects:  First, the NAME is an unevaluated symbol rather than a
quoted symbol or other form; and second, NAME is lexically rather than
dynamically scoped:  Only references to it within BODY will work.  These
references may appear inside macro expansions, but not inside functions
called from BODY."
  (declare (indent 1) (debug (symbolp body)))
  (if (cl--safe-expr-p `(progn ,@body)) `(progn ,@body)
    (let ((var (intern (format "--cl-block-%s--" name))))
      `(cl--block-wrapper
        ;; Build a unique "tag" in the form of a fresh cons.
        ;; We include `var' in the cons, just in case it help debugging.
        (let ((,var (cons ',var nil)))
         (catch ,var
           ,@body))))))

;;;###autoload
(defmacro cl-return (&optional result)
  "Return from the block named nil.
This is equivalent to `(cl-return-from nil RESULT)'."
  (declare (debug (&optional form)))
  `(cl-return-from nil ,result))

;;;###autoload
(defmacro cl-return-from (name &optional result)
  "Return from the block named NAME.
This jumps out to the innermost enclosing `(cl-block NAME ...)' form,
returning RESULT from that form (or nil if RESULT is omitted).
This is compatible with Common Lisp, but note that `defun' and
`defmacro' do not create implicit blocks as they do in Common Lisp."
  (declare (indent 1) (debug (symbolp &optional form)))
  (let ((name2 (intern (format "--cl-block-%s--" name))))
    `(cl--block-throw ,name2 ,result)))


;;; The "cl-loop" macro.

(defvar cl--loop-args) (defvar cl--loop-accum-var) (defvar cl--loop-accum-vars)
(defvar cl--loop-bindings) (defvar cl--loop-body) (defvar cl--loop-conditions)
(defvar cl--loop-finally)
(defvar cl--loop-finish-flag)           ;Symbol set to nil to exit the loop?
(defvar cl--loop-first-flag)
(defvar cl--loop-initially) (defvar cl--loop-iterator-function)
(defvar cl--loop-name)
(defvar cl--loop-result) (defvar cl--loop-result-explicit)
(defvar cl--loop-result-var) (defvar cl--loop-steps)
(defvar cl--loop-symbol-macs)

(defun cl--normalize (word)
  (if (and word (symbolp word) (not (keywordp word)))
      (intern (symbol-name word) *keyword-package*)
    word))

(defun cl--loop-set-iterator-function (kind iterator)
  (if cl--loop-iterator-function
      ;; FIXME: Of course, we could make it work, but why bother.
      (error "Iteration on %S does not support this combination" kind)
    (setq cl--loop-iterator-function iterator)))

;;;###autoload
(defmacro cl-loop (&rest loop-args)
  "The Common Lisp `loop' macro.
Valid clauses include:
  For clauses:
    for VAR from/upfrom/downfrom EXPR1 to/upto/downto/above/below EXPR2
        [by EXPR3]
    for VAR = EXPR1 then EXPR2
    for VAR in/on/in-ref LIST [by FUNC]
    for VAR across/across-ref ARRAY
    for VAR being:
      the elements of/of-ref SEQUENCE [using (index VAR2)]
      the symbols [of OBARRAY]
      the hash-keys/hash-values of HASH-TABLE [using (hash-values/hash-keys V2)]
      the key-codes/key-bindings/key-seqs of KEYMAP [using (key-bindings VAR2)]
      the overlays/intervals [of BUFFER] [from POS1] [to POS2]
      the frames/buffers
      the windows [of FRAME]
  Iteration clauses:
    repeat INTEGER
    while/until/always/never/thereis CONDITION
  Accumulation clauses:
    collect/append/nconc/concat/vconcat/count/sum/maximize/minimize FORM
      [into VAR]
  Miscellaneous clauses:
    with VAR = INIT
    if/when/unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    named NAME
    initially/finally [do] EXPRS...
    do EXPRS...
    [finally] return EXPR

For more details, see Info node `(cl)Loop Facility'.

\(fn CLAUSE...)"
  (declare (debug (&rest &or
                         ;; These are usually followed by a symbol, but it can
                         ;; actually be any destructuring-bind pattern, which
                         ;; would erroneously match `form'.
                         [[&or "for" "as" "with" "and"] sexp]
                         ;; These are followed by expressions which could
                         ;; erroneously match `symbolp'.
                         [[&or "from" "upfrom" "downfrom" "to" "upto" "downto"
                               "above" "below" "by" "in" "on" "=" "across"
                               "repeat" "while" "until" "always" "never"
                               "thereis" "collect" "append" "nconc" "sum"
                               "count" "maximize" "minimize"
                               "if" "when" "unless"
                               "return"]
                          form]
                         ["using" (symbolp symbolp)]
                         ;; Simple default, which covers 99% of the cases.
                         symbolp form)))
  (if (not (memq t (mapcar #'symbolp
                           (delq nil (delq t (cl-copy-list loop-args))))))
      `(cl-block nil (while t ,@loop-args))
    (let ((cl--loop-args loop-args) (cl--loop-name nil) (cl--loop-bindings nil)
	  (cl--loop-body nil)		(cl--loop-steps nil)
	  (cl--loop-result nil)		(cl--loop-result-explicit nil)
	  (cl--loop-result-var nil)	(cl--loop-finish-flag nil)
	  (cl--loop-accum-var nil)	(cl--loop-accum-vars nil)
	  (cl--loop-initially nil)	(cl--loop-finally nil)
	  (cl--loop-iterator-function nil) (cl--loop-first-flag nil)
          (cl--loop-symbol-macs nil)
          (cl--loop-conditions nil))
      ;; Here is more or less how those dynbind vars are used after looping
      ;; over cl--parse-loop-clause:
      ;;
      ;; (cl-block ,cl--loop-name
      ;;   (cl-symbol-macrolet ,cl--loop-symbol-macs
      ;;     (foldl #'cl--loop-let
      ;;            `((,cl--loop-result-var)
      ;;              ((,cl--loop-first-flag t))
      ;;              ((,cl--loop-finish-flag t))
      ;;              ,@cl--loop-bindings)
      ;;           ,@(nreverse cl--loop-initially)
      ;;           (while                   ;(well: cl--loop-iterator-function)
      ;;               ,(car (cl--loop-build-ands (nreverse cl--loop-body)))
      ;;             ,@(cadr (cl--loop-build-ands (nreverse cl--loop-body)))
      ;;             ,@(nreverse cl--loop-steps)
      ;;             (setq ,cl--loop-first-flag nil))
      ;;           (if (not ,cl--loop-finish-flag) ;FIXME: Why `if' vs `progn'?
      ;;               ,cl--loop-result-var
      ;;             ,@(nreverse cl--loop-finally)
      ;;             ,(or cl--loop-result-explicit
      ;;                  cl--loop-result)))))
      ;;
      (setq cl--loop-args (append cl--loop-args '(cl-end-loop)))
      (while (not (eq (car cl--loop-args) 'cl-end-loop))
        (cl--parse-loop-clause))
      (if cl--loop-finish-flag
	  (push `((,cl--loop-finish-flag t)) cl--loop-bindings))
      (if cl--loop-first-flag
	  (progn (push `((,cl--loop-first-flag t)) cl--loop-bindings)
		 (push `(setq ,cl--loop-first-flag nil) cl--loop-steps)))
      (let* ((epilogue (nconc (nreverse cl--loop-finally)
			      (list (or cl--loop-result-explicit
                                        cl--loop-result))))
	     (ands (cl--loop-build-ands (nreverse cl--loop-body)))
	     (while-body (nconc (cadr ands) (nreverse cl--loop-steps)))
	     (body (append
		    (nreverse cl--loop-initially)
		    (list (if cl--loop-iterator-function
			      `(cl-block --cl-finish--
                                 ,(funcall cl--loop-iterator-function
                                           (if (eq (car ands) t) while-body
                                             (cons `(or ,(car ands)
                                                        (cl-return-from
                                                            --cl-finish--
                                                          nil))
                                                   while-body))))
			    `(while ,(car ands) ,@while-body)))
		    (if cl--loop-finish-flag
			(if (equal epilogue '(nil)) (list cl--loop-result-var)
			  `((if ,cl--loop-finish-flag
				(progn ,@epilogue) ,cl--loop-result-var)))
		      epilogue))))
	(if cl--loop-result-var
            (push (list cl--loop-result-var) cl--loop-bindings))
	(while cl--loop-bindings
	  (if (cdar cl--loop-bindings)
	      (setq body (list (cl--loop-let (pop cl--loop-bindings) body t)))
	    (let ((lets nil))
	      (while (and cl--loop-bindings
			  (not (cdar cl--loop-bindings)))
		(push (car (pop cl--loop-bindings)) lets))
	      (setq body (list (cl--loop-let lets body nil))))))
	(if cl--loop-symbol-macs
	    (setq body
                  (list `(cl-symbol-macrolet ,cl--loop-symbol-macs ,@body))))
	`(cl-block ,cl--loop-name ,@body)))))

(defmacro cl--push-clause-loop-body (clause)
  "Apply CLAUSE to both `cl--loop-conditions' and `cl--loop-body'."
  (macroexp-let2 nil sym clause
    `(progn
       (push ,sym cl--loop-conditions)
       (push ,sym cl--loop-body))))

;; Below is a complete spec for cl-loop, in several parts that correspond
;; to the syntax given in CLtL2.  The specs do more than specify where
;; the forms are; it also specifies, as much as Edebug allows, all the
;; syntactically valid cl-loop clauses.  The disadvantage of this
;; completeness is rigidity, but the "for ... being" clause allows
;; arbitrary extensions of the form: [symbolp &rest &or symbolp form].

;; (def-edebug-spec cl-loop
;;   ([&optional ["named" symbolp]]
;;    [&rest
;;     &or
;;     ["repeat" form]
;;     loop-for-as
;;     loop-with
;;     loop-initial-final]
;;    [&rest loop-clause]
;;    ))

;; (def-edebug-elem-spec 'loop-with
;;  '("with" loop-var
;;    loop-type-spec
;;    [&optional ["=" form]]
;;    &rest ["and" loop-var
;; 	  loop-type-spec
;; 	  [&optional ["=" form]]]))

;; (def-edebug-elem-spec 'loop-for-as
;;  '([&or "for" "as"] loop-for-as-subclause
;;    &rest ["and" loop-for-as-subclause]))

;; (def-edebug-elem-spec 'loop-for-as-subclause
;;  '(loop-var
;;    loop-type-spec
;;    &or
;;    [[&or "in" "on" "in-ref" "across-ref"]
;;     form &optional ["by" function-form]]

;;    ["=" form &optional ["then" form]]
;;    ["across" form]
;;    ["being"
;;     [&or "the" "each"]
;;     &or
;;     [[&or "element" "elements"]
;;      [&or "of" "in" "of-ref"] form
;;      &optional "using" ["index" symbolp]];; is this right?
;;     [[&or "hash-key" "hash-keys"
;; 	  "hash-value" "hash-values"]
;;      [&or "of" "in"]
;;      hash-table-p &optional ["using" ([&or "hash-value" "hash-values"
;; 					   "hash-key" "hash-keys"] sexp)]]

;;     [[&or "symbol" "present-symbol" "external-symbol"
;; 	  "symbols" "present-symbols" "external-symbols"]
;;      [&or "in" "of"] package-p]

;;     ;; Extensions for Emacs Lisp, including Lucid Emacs.
;;     [[&or "frame" "frames"
;; 	  "screen" "screens"
;; 	  "buffer" "buffers"]]

;;     [[&or "window" "windows"]
;;      [&or "of" "in"] form]

;;     [[&or "overlay" "overlays"
;; 	  "extent" "extents"]
;;      [&or "of" "in"] form
;;      &optional [[&or "from" "to"] form]]

;;     [[&or "interval" "intervals"]
;;      [&or "in" "of"] form
;;      &optional [[&or "from" "to"] form]
;;      ["property" form]]

;;     [[&or "key-code" "key-codes"
;; 	  "key-seq" "key-seqs"
;; 	  "key-binding" "key-bindings"]
;;      [&or "in" "of"] form
;;      &optional ["using" ([&or "key-code" "key-codes"
;; 			      "key-seq" "key-seqs"
;; 			      "key-binding" "key-bindings"]
;; 			 sexp)]]
;;     ;; For arbitrary extensions, recognize anything else.
;;     [symbolp &rest &or symbolp form]
;;     ]

;;    ;; arithmetic - must be last since all parts are optional.
;;    [[&optional [[&or "from" "downfrom" "upfrom"] form]]
;;     [&optional [[&or "to" "downto" "upto" "below" "above"] form]]
;;     [&optional ["by" form]]
;;     ]))

;; (def-edebug-elem-spec 'loop-initial-final
;;  '(&or ["initially"
;; 	;; [&optional &or "do" "doing"]  ;; CLtL2 doesn't allow this.
;; 	&rest loop-non-atomic-expr]
;;        ["finally" &or
;; 	[[&optional &or "do" "doing"] &rest loop-non-atomic-expr]
;; 	["return" form]]))

;; (def-edebug-elem-spec 'loop-and-clause
;;   '(loop-clause &rest ["and" loop-clause]))

;; (def-edebug-elem-spec 'loop-clause
;;  '(&or
;;    [[&or "while" "until" "always" "never" "thereis"] form]

;;    [[&or "collect" "collecting"
;; 	 "append" "appending"
;; 	 "nconc" "nconcing"
;; 	 "concat" "vconcat"] form
;; 	 [&optional ["into" loop-var]]]

;;    [[&or "count" "counting"
;; 	 "sum" "summing"
;; 	 "maximize" "maximizing"
;; 	 "minimize" "minimizing"] form
;; 	 [&optional ["into" loop-var]]
;; 	 loop-type-spec]

;;    [[&or "if" "when" "unless"]
;;     form loop-and-clause
;;     [&optional ["else" loop-and-clause]]
;;     [&optional "end"]]

;;    [[&or "do" "doing"] &rest loop-non-atomic-expr]

;;    ["return" form]
;;    loop-initial-final
;;    ))

;; (def-edebug-elem-spec 'loop-non-atomic-expr
;;   '([&not atom] form))

;; (def-edebug-elem-spec 'loop-var
;;   ;; The symbolp must be last alternative to recognize e.g. (a b . c)
;;   ;; loop-var =>
;;   ;; (loop-var . [&or nil loop-var])
;;   ;; (symbolp . [&or nil loop-var])
;;   ;; (symbolp . loop-var)
;;   ;; (symbolp . (symbolp . [&or nil loop-var]))
;;   ;; (symbolp . (symbolp . loop-var))
;;   ;; (symbolp . (symbolp . symbolp)) == (symbolp symbolp . symbolp)
;;   '(&or (loop-var . [&or nil loop-var]) [gate symbolp]))

;; (def-edebug-elem-spec 'loop-type-spec
;;   '(&optional ["of-type" loop-d-type-spec]))

;; (def-edebug-elem-spec 'loop-d-type-spec
;;   '(&or (loop-d-type-spec . [&or nil loop-d-type-spec]) cl-type-spec))

(defun cl--parse-loop-clause ()		; uses loop-*
  (let ((word (cl--normalize (pop cl--loop-args)))
	(hash-types '(:hash-key :hash-keys :hash-value :hash-values))
	(key-types '(:key-code :key-codes :key-seq :key-seqs
		     :key-binding :key-bindings)))
    (cond

     ((null cl--loop-args)
      (error "Malformed `cl-loop' macro"))

     ((eq word :named)
      (setq cl--loop-name (pop cl--loop-args)))

     ((eq word :initially)
      (if (memq (cl--normalize (car cl--loop-args)) '(:do :doing))
          (pop cl--loop-args))
      (or (consp (car cl--loop-args))
          (error "Syntax error on `initially' clause"))
      (while (consp (car cl--loop-args))
	(push (pop cl--loop-args) cl--loop-initially)))

     ((eq word :finally)
      (if (eq (cl--normalize (car cl--loop-args)) :return)
	  (setq cl--loop-result-explicit
                (or (cl--pop2 cl--loop-args) '(quote nil)))
	(if (memq (cl--normalize (car cl--loop-args)) '(:do :doing))
            (pop cl--loop-args))
	(or (consp (car cl--loop-args))
            (error "Syntax error on `finally' clause"))
	(if (and (eq (cl--normalize (caar cl--loop-args)) :return)
                 (null cl--loop-name))
	    (setq cl--loop-result-explicit
                  (or (nth 1 (pop cl--loop-args)) '(quote nil)))
	  (while (consp (car cl--loop-args))
	    (push (pop cl--loop-args) cl--loop-finally)))))

     ((memq word '(:for :as))
      (let ((loop-for-bindings nil) (loop-for-sets nil) (loop-for-steps nil)
	    (ands nil))
	(while
            ;; Use `gensym' rather than `make-symbol'.  It's important that
	    ;; (not (eq (symbol-name var1) (symbol-name var2))) because
	    ;; these vars get added to the macro-environment.
	    (let ((var (or (pop cl--loop-args) (gensym "--cl-var--"))))
	      (setq word (cl--normalize (pop cl--loop-args)))
	      (if (eq word :being) (setq word (cl--normalize (pop cl--loop-args))))
	      (if (memq word '(:the :each)) (setq word (cl--normalize (pop cl--loop-args))))
	      (if (memq word '(:buffer :buffers))
		  (setq word :in
                        cl--loop-args (cons '(buffer-list) cl--loop-args)))
	      (cond

	       ((memq word '(:from :downfrom :upfrom :to :downto :upto
			     :above :below :by))
		(push word cl--loop-args)
		(if (memq (cl--normalize (car cl--loop-args)) '(:downto :above))
		    (error "Must specify `from' value for downward cl-loop"))
		(let* ((down (or (eq (cl--normalize (car cl--loop-args)) :downfrom)
				 (memq (cl--normalize (nth 2 cl--loop-args))
                                       '(:downto :above))))
		       (excl (or (memq (cl--normalize (car cl--loop-args)) '(:above :below))
				 (memq (cl--normalize (nth 2 cl--loop-args))
                                       '(:above :below))))
		       (start (and (memq (cl--normalize (car cl--loop-args))
                                         '(:from :upfrom :downfrom))
				   (cl--pop2 cl--loop-args)))
		       (end (and (memq (cl--normalize (car cl--loop-args))
				       '(:to :upto :downto :above :below))
				 (cl--pop2 cl--loop-args)))
		       (step (and (eq (cl--normalize (car cl--loop-args)) :by)
                                  (cl--pop2 cl--loop-args)))
		       (end-var (and (not (macroexp-const-p end))
				     (make-symbol "--cl-var--")))
		       (step-var (and (not (macroexp-const-p step))
				      (make-symbol "--cl-var--"))))
		  (and step (numberp step) (<= step 0)
		       (error "Loop `by' value is not positive: %s" step))
		  (push (list var (or start 0)) loop-for-bindings)
		  (if end-var (push (list end-var end) loop-for-bindings))
		  (if step-var (push (list step-var step)
				     loop-for-bindings))
		  (when end
                    (cl--push-clause-loop-body
                     (list
                      (if down (if excl '> '>=) (if excl '< '<=))
                      var (or end-var end))))
		  (push (list var (list (if down '- '+) var
					(or step-var step 1)))
			loop-for-steps)))

	       ((memq word '(:in :in-ref :on))
		(let* ((on (eq word :on))
		       (temp (if (and on (symbolp var))
				 var (make-symbol "--cl-var--"))))
		  (push (list temp (pop cl--loop-args)) loop-for-bindings)
                  (cl--push-clause-loop-body `(consp ,temp))
		  (if (eq word :in-ref)
		      (push (list var `(car ,temp)) cl--loop-symbol-macs)
		    (or (eq temp var)
			(progn
			  (push (list var nil) loop-for-bindings)
			  (push (list var (if on temp `(car ,temp)))
				loop-for-sets))))
		  (push (list temp
			      (if (eq (cl--normalize (car cl--loop-args)) :by)
				  (let ((step (cl--pop2 cl--loop-args)))
				    (if (and (memq (car-safe step)
						   '(quote function
							   cl-function))
					     (symbolp (nth 1 step)))
					(list (nth 1 step) temp)
				      `(funcall ,step ,temp)))
				`(cdr ,temp)))
			loop-for-steps)))

	       ((eq word :=)
		(let* ((start (pop cl--loop-args))
		       (then (if (eq (cl--normalize (car cl--loop-args)) :then)
                                 (cl--pop2 cl--loop-args) start))
                       (first-assign (or cl--loop-first-flag
					 (setq cl--loop-first-flag
					       (make-symbol "--cl-var--")))))
		  (push (list var nil) loop-for-bindings)
		  (if (or ands (eq (car cl--loop-args) 'and))
		      (progn
			(push `(,var (if ,first-assign ,start ,var)) loop-for-sets)
			(push `(,var (if ,(car (cl--loop-build-ands
                                                (nreverse cl--loop-conditions)))
                                         ,then ,var))
                              loop-for-steps))
                    (push (if (eq start then)
		              `(,var ,then)
                            `(,var (if ,first-assign ,start ,then)))
                          loop-for-sets))))

	       ((memq word '(:across :across-ref))
		(let ((temp-vec (make-symbol "--cl-vec--"))
		      (temp-idx (make-symbol "--cl-idx--")))
		  (push (list temp-vec (pop cl--loop-args)) loop-for-bindings)
		  (push (list temp-idx -1) loop-for-bindings)
                  (push `(setq ,temp-idx (1+ ,temp-idx)) cl--loop-body)
		  (cl--push-clause-loop-body
                   `(< ,temp-idx (length ,temp-vec)))
		  (if (eq word :across-ref)
		      (push (list var `(aref ,temp-vec ,temp-idx))
			    cl--loop-symbol-macs)
		    (push (list var nil) loop-for-bindings)
		    (push (list var `(aref ,temp-vec ,temp-idx))
			  loop-for-sets))))

	       ((memq word '(:element :elements))
		(let ((ref (or (memq (cl--normalize (car cl--loop-args)) '(:in-ref :of-ref))
			       (and (not (memq (cl--normalize (car cl--loop-args)) '(:in :of)))
				    (error "Expected `of'"))))
		      (seq (cl--pop2 cl--loop-args))
		      (temp-seq (make-symbol "--cl-seq--"))
		      (temp-idx
                       (if (eq (cl--normalize (car cl--loop-args)) :using)
                           (if (and (= (length (cadr cl--loop-args)) 2)
                                    (eq (caadr cl--loop-args) 'index))
                               (cadr (cl--pop2 cl--loop-args))
                             (error "Bad `using' clause"))
                         (make-symbol "--cl-idx--"))))
		  (push (list temp-seq seq) loop-for-bindings)
		  (push (list temp-idx 0) loop-for-bindings)
		  (if ref
                      (let ((temp-len (make-symbol "--cl-len--")))
			(push (list temp-len `(length ,temp-seq))
			      loop-for-bindings)
			(push (list var `(elt ,temp-seq ,temp-idx))
			      cl--loop-symbol-macs)
                        (cl--push-clause-loop-body `(< ,temp-idx ,temp-len)))
		    (push (list var nil) loop-for-bindings)
		    (cl--push-clause-loop-body `(and ,temp-seq
                                                     (or (consp ,temp-seq)
                                                         (< ,temp-idx (length ,temp-seq)))))
		    (push (list var `(if (consp ,temp-seq)
                                         (pop ,temp-seq)
                                       (aref ,temp-seq ,temp-idx)))
			  loop-for-sets))
		  (push (list temp-idx `(1+ ,temp-idx))
			loop-for-steps)))

	       ((memq word hash-types)
		(or (memq (cl--normalize (car cl--loop-args)) '(:in :of))
                    (error "Expected `of'"))
		(let* ((table (cl--pop2 cl--loop-args))
		       (other
                        (if (eq (cl--normalize (car cl--loop-args)) :using)
                            (if (and (= (length (cadr cl--loop-args)) 2)
                                     (memq (cl--normalize (caadr cl--loop-args)) hash-types)
                                     (not (eq (caadr cl--loop-args) word)))
                                (cadr (cl--pop2 cl--loop-args))
                              (error "Bad `using' clause"))
                          (make-symbol "--cl-var--"))))
		  (if (memq word '(:hash-value :hash-values))
		      (setq var (prog1 other (setq other var))))
		  (cl--loop-set-iterator-function
                   'hash-tables (lambda (body)
                                  `(maphash (lambda (,var ,other) . ,body)
                                            ,table)))))

	       ((memq word '(:symbol :present-symbol :external-symbol
			     :symbols :present-symbols :external-symbols))
		(let ((ob (and (memq (cl--normalize (car cl--loop-args)) '(:in :of))
                               (cl--pop2 cl--loop-args))))
		  (cl--loop-set-iterator-function
                   'symbols (lambda (body)
                              `(mapatoms (lambda (,var) . ,body) ,ob)))))

	       ((memq word '(:overlay :overlays :extent :extents))
		(let ((buf nil) (from nil) (to nil))
		  (while (memq (cl--normalize (car cl--loop-args)) '(:in :of :from :to))
		    (cond ((eq (cl--normalize (car cl--loop-args)) :from)
                           (setq from (cl--pop2 cl--loop-args)))
			  ((eq (cl--normalize (car cl--loop-args)) :to)
                           (setq to (cl--pop2 cl--loop-args)))
			  (t (setq buf (cl--pop2 cl--loop-args)))))
		  (cl--loop-set-iterator-function
                   'overlays (lambda (body)
                               `(cl--map-overlays
                                 (lambda (,var ,(make-symbol "--cl-var--"))
                                   (progn . ,body) nil)
                                 ,buf ,from ,to)))))

	       ((memq word '(:interval :intervals))
		(let ((buf nil) (prop nil) (from nil) (to nil)
		      (var1 (make-symbol "--cl-var1--"))
		      (var2 (make-symbol "--cl-var2--")))
		  (while (memq (cl--normalize (car cl--loop-args)) '(:in :of :property :from :to))
		    (cond ((eq (cl--normalize (car cl--loop-args)) :from)
                           (setq from (cl--pop2 cl--loop-args)))
			  ((eq (cl--normalize (car cl--loop-args)) :to)
                           (setq to (cl--pop2 cl--loop-args)))
			  ((eq (cl--normalize (car cl--loop-args)) :property)
			   (setq prop (cl--pop2 cl--loop-args)))
			  (t (setq buf (cl--pop2 cl--loop-args)))))
		  (if (and (consp var) (symbolp (car var)) (symbolp (cdr var)))
		      (setq var1 (car var) var2 (cdr var))
		    (push (list var nil) loop-for-bindings)
		    (push (list var `(cons ,var1 ,var2)) loop-for-sets))
		  (cl--loop-set-iterator-function
                   'intervals (lambda (body)
                                `(cl--map-intervals
                                  (lambda (,var1 ,var2) . ,body)
                                  ,buf ,prop ,from ,to)))))

	       ((memq word key-types)
		(or (memq (cl--normalize (car cl--loop-args)) '(:in :of))
                    (error "Expected `of'"))
                (let ((map (cl--pop2 cl--loop-args))
		      (other
                       (if (eq (cl--normalize (car cl--loop-args)) :using)
                           (if (and (= (length (cadr cl--loop-args)) 2)
                                    (memq (cl--normalize (caadr cl--loop-args)) key-types)
                                    (not (eq (cl--normalize (caadr cl--loop-args)) word)))
                               (cadr (cl--pop2 cl--loop-args))
                             (error "Bad `using' clause"))
                         (make-symbol "--cl-var--"))))
		  (if (memq word '(:key-binding :key-bindings))
		      (setq var (prog1 other (setq other var))))
		  (cl--loop-set-iterator-function
                   'keys (lambda (body)
                           `(,(if (memq word '(:key-seq :key-seqs))
                                  'cl--map-keymap-recursively 'map-keymap)
                             (lambda (,var ,other) . ,body) ,map)))))

	       ((memq word '(:frame :frames :screen :screens))
		(let ((temp (make-symbol "--cl-var--")))
		  (push (list var  '(selected-frame))
			loop-for-bindings)
		  (push (list temp nil) loop-for-bindings)
		  (cl--push-clause-loop-body `(prog1 (not (eq ,var ,temp))
                                                (or ,temp (setq ,temp ,var))))
		  (push (list var `(next-frame ,var))
			loop-for-steps)))

	       ((memq word '(:window :windows))
		(let ((scr (and (memq (cl--normalize (car cl--loop-args)) '(:in :of))
                                (cl--pop2 cl--loop-args)))
		      (temp (make-symbol "--cl-var--"))
		      (minip (make-symbol "--cl-minip--")))
		  (push (list var (if scr
				      `(frame-selected-window ,scr)
				    '(selected-window)))
			loop-for-bindings)
		  ;; If we started in the minibuffer, we need to
		  ;; ensure that next-window will bring us back there
		  ;; at some point.  (Bug#7492).
		  ;; (Consider using walk-windows instead of cl-loop if
		  ;; you care about such things.)
		  (push (list minip `(minibufferp (window-buffer ,var)))
			loop-for-bindings)
		  (push (list temp nil) loop-for-bindings)
		  (cl--push-clause-loop-body `(prog1 (not (eq ,var ,temp))
                                                (or ,temp (setq ,temp ,var))))
		  (push (list var `(next-window ,var ,minip))
			loop-for-steps)))

	       (t
		;; This is an advertised interface: (info "(cl)Other Clauses").
		(let ((handler (and (symbolp word)
				    (get word 'cl-loop-for-handler))))
		  (if handler
		      (funcall handler var)
		    (error "Expected a `for' preposition, found %s" word)))))
	      (eq (car cl--loop-args) 'and))
	  (setq ands t)
	  (pop cl--loop-args))
	(if (and ands loop-for-bindings)
	    (push (nreverse loop-for-bindings) cl--loop-bindings)
	  (setq cl--loop-bindings (nconc (mapcar #'list loop-for-bindings)
				         cl--loop-bindings)))
	(if loop-for-sets
	    (push `(progn
                     ,(cl--loop-let (nreverse loop-for-sets) 'setq ands)
                     t)
                  cl--loop-body))
	(when loop-for-steps
	  (push (cons (if ands 'cl-psetq 'setq)
		      (apply #'append (nreverse loop-for-steps)))
		cl--loop-steps))))

     ((eq word :repeat)
      (let ((temp (make-symbol "--cl-var--")))
	(push (list (list temp (pop cl--loop-args))) cl--loop-bindings)
	(push `(>= (setq ,temp (1- ,temp)) 0) cl--loop-body)))

     ((memq word '(:collect :collecting))
      (let ((what (pop cl--loop-args))
	    (var (cl--loop-handle-accum nil 'nreverse)))
	(if (eq var cl--loop-accum-var)
	    (push `(progn (push ,what ,var) t) cl--loop-body)
	  (push `(progn
                   (setq ,var (nconc ,var (list ,what)))
                   t)
                cl--loop-body))))

     ((memq word '(:nconc :nconcing :append :appending))
      (let ((what (pop cl--loop-args))
	    (var (cl--loop-handle-accum nil 'nreverse)))
	(push `(progn
                 (setq ,var
                       ,(if (eq var cl--loop-accum-var)
                            `(nconc
                              (,(if (memq word '(nconc nconcing))
                                    #'nreverse #'reverse)
                               ,what)
                              ,var)
                          `(,(if (memq word '(nconc nconcing))
                                 #'nconc #'append)
                            ,var ,what)))
                 t)
              cl--loop-body)))

     ((memq word '(:concat :concating))
      (let ((what (pop cl--loop-args))
	    (var (cl--loop-handle-accum "")))
	(push `(progn (cl-callf concat ,var ,what) t) cl--loop-body)))

     ((memq word '(:vconcat :vconcating))
      (let ((what (pop cl--loop-args))
	    (var (cl--loop-handle-accum [])))
	(push `(progn (cl-callf vconcat ,var ,what) t) cl--loop-body)))

     ((memq word '(:sum :summing))
      (let ((what (pop cl--loop-args))
	    (var (cl--loop-handle-accum 0)))
        (push `(progn (incf ,var ,what) t) cl--loop-body)))

     ((memq word '(:count :counting))
      (let ((what (pop cl--loop-args))
	    (var (cl--loop-handle-accum 0)))
        (push `(progn (if ,what (incf ,var)) t) cl--loop-body)))

     ((memq word '(:minimize :minimizing :maximize :maximizing))
      (push `(progn ,(macroexp-let2 macroexp-copyable-p temp
                                    (pop cl--loop-args)
                       (let* ((var (cl--loop-handle-accum nil))
                              (func (if (memq word '(:minimize
                                                     :minimizing))
                                        'min 'max)))
                         `(setq ,var (if ,var (,func ,var ,temp) ,temp))))
                    t)
            cl--loop-body))

     ((eq word :with)
      (let ((bindings nil))
	(while (progn (push (list (pop cl--loop-args)
				  (and (eq (cl--normalize (car cl--loop-args)) :=)
                                       (cl--pop2 cl--loop-args)))
			    bindings)
		      (eq (cl--normalize (car cl--loop-args)) :and))
	  (pop cl--loop-args))
	(push (nreverse bindings) cl--loop-bindings)))

     ((eq word :while)
      (push (pop cl--loop-args) cl--loop-body))

     ((eq word :until)
      (push `(not ,(pop cl--loop-args)) cl--loop-body))

     ((eq word :always)
      (or cl--loop-finish-flag
          (setq cl--loop-finish-flag (make-symbol "--cl-flag--")))
      (push `(setq ,cl--loop-finish-flag ,(pop cl--loop-args)) cl--loop-body)
      (setq cl--loop-result t))

     ((eq word :never)
      (or cl--loop-finish-flag
          (setq cl--loop-finish-flag (make-symbol "--cl-flag--")))
      (push `(setq ,cl--loop-finish-flag (not ,(pop cl--loop-args)))
	    cl--loop-body)
      (setq cl--loop-result t))

     ((eq word :thereis)
      (or cl--loop-finish-flag
          (setq cl--loop-finish-flag (make-symbol "--cl-flag--")))
      (or cl--loop-result-var
          (setq cl--loop-result-var (make-symbol "--cl-var--")))
      (push `(setq ,cl--loop-finish-flag
                   (not (setq ,cl--loop-result-var ,(pop cl--loop-args))))
	    cl--loop-body))

     ((memq word '(:if :when :unless))
      (let* ((cond (pop cl--loop-args))
	     (then (let ((cl--loop-body nil))
		     (cl--parse-loop-clause)
		     (cl--loop-build-ands (nreverse cl--loop-body))))
	     (else (let ((cl--loop-body nil))
		     (if (eq (cl--normalize (car cl--loop-args)) :else)
			 (progn (pop cl--loop-args) (cl--parse-loop-clause)))
		     (cl--loop-build-ands (nreverse cl--loop-body))))
	     (simple (and (eq (car then) t) (eq (car else) t))))
	(if (eq (cl--normalize (car cl--loop-args)) :end) (pop cl--loop-args))
	(if (eq word :unless) (setq then (prog1 else (setq else then))))
	(let ((form (cons (if simple (cons 'progn (nth 1 then)) (nth 2 then))
			  (if simple (nth 1 else) (list (nth 2 else))))))
	  (setq form (if (cl--expr-contains form 'it)
                         `(let ((it ,cond)) (if it ,@form))
                       `(if ,cond ,@form)))
	  (push (if simple `(progn ,form t) form) cl--loop-body))))

     ((memq word '(:do :doing))
      (let ((body nil))
	(or (consp (car cl--loop-args)) (error "Syntax error on `do' clause"))
	(while (consp (car cl--loop-args)) (push (pop cl--loop-args) body))
	(push (cons 'progn (nreverse (cons t body))) cl--loop-body)))

     ((eq word :return)
      (or cl--loop-finish-flag
          (setq cl--loop-finish-flag (make-symbol "--cl-var--")))
      (or cl--loop-result-var
          (setq cl--loop-result-var (make-symbol "--cl-var--")))
      (push `(setq ,cl--loop-result-var ,(pop cl--loop-args)
                   ,cl--loop-finish-flag nil)
            cl--loop-body))

     (t
      ;; This is an advertised interface: (info "(cl)Other Clauses").
      (let ((handler (and (symbolp word) (get word 'cl-loop-handler))))
	(or handler (error "Expected a cl-loop keyword, found %s" word))
	(funcall handler))))
    (if (eq (car cl--loop-args) 'and)
	(progn (pop cl--loop-args) (cl--parse-loop-clause)))))

(defun cl--unused-var-p (sym)
  (or (null sym) (eq ?_ (aref (symbol-name sym) 0))))

(defun cl--loop-let (specs body par)    ; modifies cl--loop-bindings
  "Build an expression equivalent to (let SPECS BODY).
SPECS can include bindings using `cl-loop's destructuring (not to be
confused with the patterns of `cl-destructuring-bind').
If PAR is nil, do the bindings step by step, like `let*'.
If BODY is `setq', then use SPECS for assignments rather than for bindings."
  (let ((temps nil) (new nil))
    (when par
      (let ((p specs))
        (while (and p (or (symbolp (car-safe (car p))) (null (cadar p))))
          (setq p (cdr p)))
        (when p
          (setq par nil)
          (dolist (spec specs)
            (or (macroexp-const-p (cadr spec))
                (let ((temp (make-symbol "--cl-var--")))
                  (push (list temp (cadr spec)) temps)
                  (setcar (cdr spec) temp)))))))
    (while specs
      (let* ((binding (pop specs))
             (spec (car-safe binding)))
        (if (and (consp binding) (or (consp spec) (cl--unused-var-p spec)))
            (let* ((nspecs nil)
                   (expr (car (cdr-safe binding)))
                   (temp (last spec 0)))
              (if (and (cl--unused-var-p temp) (null expr))
                  nil ;; Don't bother declaring/setting `temp' since it won't
		      ;; be used when `expr' is nil, anyway.
		(when (or (null temp)
                          (and (eq body 'setq) (cl--unused-var-p temp)))
                  ;; Prefer a fresh uninterned symbol over "_to", to avoid
                  ;; warnings that we set an unused variable.
                  (setq temp (make-symbol "--cl-var--"))
                  ;; Make sure this temp variable is locally declared.
                  (when (eq body 'setq)
                    (push (list (list temp)) cl--loop-bindings)))
                (push (list temp expr) new))
              (while (consp spec)
                (push (list (pop spec)
                            (and expr (list (if spec 'pop 'car) temp)))
                      nspecs))
              (setq specs (nconc (nreverse nspecs) specs)))
          (push binding new))))
    (if (eq body 'setq)
	(let ((set (cons (if par 'cl-psetq 'setq)
                         (apply #'nconc (nreverse new)))))
	  (if temps `(let* ,(nreverse temps) ,set) set))
      `(,(if par 'let 'let*)
        ,(nconc (nreverse temps) (nreverse new)) ,@body))))

(defun cl--loop-handle-accum (def &optional func) ; uses loop-*
  (if (eq (car cl--loop-args) 'into)
      (let ((var (cl--pop2 cl--loop-args)))
	(or (memq var cl--loop-accum-vars)
	    (progn (push (list (list var def)) cl--loop-bindings)
		   (push var cl--loop-accum-vars)))
	var)
    (or cl--loop-accum-var
	(progn
	  (push (list (list
                       (setq cl--loop-accum-var (make-symbol "--cl-var--"))
                       def))
                cl--loop-bindings)
	  (setq cl--loop-result (if func (list func cl--loop-accum-var)
                                  cl--loop-accum-var))
	  cl--loop-accum-var))))

(defun cl--loop-build-ands (clauses)
  "Return various representations of (and . CLAUSES).
CLAUSES is a list of Elisp expressions, where clauses of the form
\(progn E1 E2 E3 .. t) are the focus of particular optimizations.
The return value has shape (COND BODY COMBO)
such that COMBO is equivalent to (and . CLAUSES)."
  (let ((ands nil)
	(body nil))
    ;; Look through `clauses', trying to optimize (progn ,@A t) (progn ,@B) ,@C
    ;; into (progn ,@A ,@B) ,@C.
    (while clauses
      (if (and (eq (car-safe (car clauses)) 'progn)
	       (eq (car (last (car clauses))) t))
	  (if (cdr clauses)
	      (setq clauses (cons (nconc (butlast (car clauses))
					 (if (eq (car-safe (cadr clauses))
						 'progn)
                                             (cdadr clauses)
					   (list (cadr clauses))))
				  (cddr clauses)))
            ;; A final (progn ,@A t) is moved outside of the `and'.
	    (setq body (cdr (butlast (pop clauses)))))
	(push (pop clauses) ands)))
    (setq ands (or (nreverse ands) (list t)))
    (list (if (cdr ands) (cons 'and ands) (car ands))
	  body
	  (let ((full (if body
			  (append ands (list (cons 'progn (append body '(t)))))
			ands)))
	    (if (cdr full) (cons 'and full) (car full))))))


;;; Other iteration control structures.

;;;###autoload
(defmacro cl-do (steps endtest &rest body)
  "Bind variables and run BODY forms until END-TEST returns non-nil.
First, each VAR is bound to the associated INIT value as if by a `let' form.
Then, in each iteration of the loop, the END-TEST is evaluated; if true,
the loop is finished.  Otherwise, the BODY forms are evaluated, then each
VAR is set to the associated STEP expression (as if by a `cl-psetq' form)
and the next iteration begins.

Once the END-TEST becomes true, the RESULT forms are evaluated (with
the VARs still bound to their values) to produce the result
returned by `cl-do'.

Note that the entire loop is enclosed in an implicit nil block, so
that you can use `cl-return' to exit at any time.

Also note that END-TEST is checked before evaluating BODY.  If END-TEST
is initially non-nil, `cl-do' will exit without running BODY.

For more details, see `cl-do' description in Info node `(cl) Iteration'.

\(fn ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)"
  (declare (indent 2)
           (debug
            ((&rest &or symbolp (symbolp &optional form form))
             (form body)
             cl-declarations body)))
  (cl--expand-do-loop steps endtest body nil))

;;;###autoload
(defmacro cl-do* (steps endtest &rest body)
  "Bind variables and run BODY forms until END-TEST returns non-nil.
First, each VAR is bound to the associated INIT value as if by a `let*' form.
Then, in each iteration of the loop, the END-TEST is evaluated; if true,
the loop is finished.  Otherwise, the BODY forms are evaluated, then each
VAR is set to the associated STEP expression (as if by a `setq'
form) and the next iteration begins.

Once the END-TEST becomes true, the RESULT forms are evaluated (with
the VARs still bound to their values) to produce the result
returned by `cl-do*'.

Note that the entire loop is enclosed in an implicit nil block, so
that you can use `cl-return' to exit at any time.

Also note that END-TEST is checked before evaluating BODY.  If END-TEST
is initially non-nil, `cl-do*' will exit without running BODY.

This is to `cl-do' what `let*' is to `let'.
For more details, see `cl-do*' description in Info node `(cl) Iteration'.

\(fn ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)"
  (declare (indent 2) (debug cl-do))
  (cl--expand-do-loop steps endtest body t))

(defun cl--expand-do-loop (steps endtest body star)
  `(cl-block nil
     (,(if star 'let* 'let)
      ,(mapcar (lambda (c) (if (consp c) (list (car c) (nth 1 c)) c))
               steps)
      (while (not ,(car endtest))
        ,@body
        ,@(let ((sets (mapcar (lambda (c)
                                (and (consp c) (cdr (cdr c))
                                     (list (car c) (nth 2 c))))
                              steps)))
            (setq sets (delq nil sets))
            (and sets
                 (list (cons (if (or star (not (cdr sets)))
                                 'setq 'cl-psetq)
                             (apply #'append sets))))))
      ,@(or (cdr endtest) '(nil)))))

;;;###autoload
(defmacro cl-dolist (spec &rest body)
  "Loop over a list.
Evaluate BODY with VAR bound to each `car' from LIST, in turn.
Then evaluate RESULT to get return value, default nil.
An implicit nil block is established around the loop.

\(fn (VAR LIST [RESULT]) BODY...)"
  (declare (debug ((symbolp form &optional form) cl-declarations body))
           (indent 1))
  (let ((loop `(dolist ,spec ,@body)))
    (if (advice-member-p 'cl--wrap-in-nil-block 'dolist)
        loop `(cl-block nil ,loop))))

;;;###autoload
(defmacro cl-dotimes (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY with VAR bound to successive integers from 0, inclusive,
to COUNT, exclusive.  Then evaluate RESULT to get return value, default
nil.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (debug cl-dolist) (indent 1))
  (let ((loop `(dotimes ,spec ,@body)))
    (if (advice-member-p 'cl--wrap-in-nil-block 'dotimes)
        loop `(cl-block nil ,loop))))

(defvar cl--tagbody-alist nil)

;;;###autoload
(defmacro cl-tagbody (&rest labels-or-stmts)
  "Execute statements while providing for control transfers to labels.
Each element of LABELS-OR-STMTS can be either a label (integer or symbol)
or a `cons' cell, in which case it's taken to be a statement.
This distinction is made before performing macroexpansion.
Statements are executed in sequence left to right, discarding any return value,
stopping only when reaching the end of LABELS-OR-STMTS.
Any statement can transfer control at any time to the statements that follow
one of the labels with the special form (go LABEL).
Labels have lexical scope and dynamic extent."
  (let ((blocks '())
        (first-label (if (consp (car labels-or-stmts))
                       'cl--preamble (pop labels-or-stmts))))
    (let ((block (list first-label)))
      (dolist (label-or-stmt labels-or-stmts)
        (if (consp label-or-stmt) (push label-or-stmt block)
          ;; Add a "go to next block" to implement the fallthrough.
          (unless (eq 'go (car-safe (car-safe block)))
            (push `(go ,label-or-stmt) block))
          (push (nreverse block) blocks)
          (setq block (list label-or-stmt))))
      (unless (eq 'go (car-safe (car-safe block)))
        (push '(go cl--exit) block))
      (push (nreverse block) blocks))
    (let ((catch-tag (make-symbol "cl--tagbody-tag"))
          (cl--tagbody-alist cl--tagbody-alist))
      (push (cons 'cl--exit catch-tag) cl--tagbody-alist)
      (dolist (block blocks)
        (push (cons (car block) catch-tag) cl--tagbody-alist))
      (macroexpand-all
       `(let ((next-label ',first-label))
          (while
              (not (eq (setq next-label
                             (catch ',catch-tag
                               (cl-case next-label
                                 ,@blocks)))
                       'cl--exit))))
       `((go . ,(lambda (label)
                  (let ((catch-tag (cdr (assq label cl--tagbody-alist))))
                    (unless catch-tag
                      (error "Unknown cl-tagbody go label `%S'" label))
                    `(throw ',catch-tag ',label))))
         ,@macroexpand-all-environment)))))

(defun cl--prog (binder bindings body)
  (let (decls)
    (while (eq 'declare (car-safe (car body)))
      (push (pop body) decls))
    `(cl-block nil
       (,binder ,bindings
         ,@(nreverse decls)
         (cl-tagbody . ,body)))))

;;;###autoload
(defmacro cl-prog (bindings &rest body)
  "Run BODY like a `cl-tagbody' after setting up the BINDINGS.
Shorthand for (cl-block nil (let BINDINGS (cl-tagbody BODY)))"
  (cl--prog 'let bindings body))

;;;###autoload
(defmacro cl-prog* (bindings &rest body)
  "Run BODY like a `cl-tagbody' after setting up the BINDINGS.
Shorthand for (cl-block nil (let* BINDINGS (cl-tagbody BODY)))"
  (cl--prog 'let* bindings body))

;;;###autoload
(defmacro cl-do-symbols (spec &rest body)
  "Loop over all symbols.
Evaluate BODY with VAR bound to each interned symbol, or to each symbol
from OBARRAY.

\(fn (VAR [OBARRAY [RESULT]]) BODY...)"
  (declare (indent 1)
           (debug ((symbolp &optional form form) cl-declarations
                   def-body)))
  ;; Apparently this doesn't have an implicit block.
  `(cl-block nil
     (let (,(car spec))
       (mapatoms #'(lambda (,(car spec)) ,@body)
                 ,@(and (cadr spec) (list (cadr spec))))
       ,(nth 2 spec))))

;;;###autoload
(defmacro cl-do-all-symbols (spec &rest body)
  "Like `cl-do-symbols', but use the default obarray.

\(fn (VAR [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp &optional form) cl-declarations body)))
  `(cl-do-symbols (,(car spec) nil ,(cadr spec)) ,@body))


;;; Assignments.

;;;###autoload
(defmacro cl-psetq (&rest args)
  "Set SYMs to the values VALs in parallel.
This is like `setq', except that all VAL forms are evaluated (in order)
before assigning any symbols SYM to the corresponding values.

\(fn SYM VAL SYM VAL ...)"
  (declare (debug setq))
  (cons 'cl-psetf args))


;;; Binding control structures.

;;;###autoload
(defmacro cl-progv (symbols values &rest body)
  "Bind SYMBOLS to VALUES dynamically in BODY.
The forms SYMBOLS and VALUES are evaluated, and must evaluate to lists.
Each symbol in the first list is bound to the corresponding value in the
second list (or to nil if VALUES is shorter than SYMBOLS); then the
BODY forms are executed and their result is returned.  This is much like
a `let' form, except that the list of symbols can be computed at run-time."
  (declare (indent 2) (debug (form form def-body)))
  (let ((bodyfun (make-symbol "body"))
        (binds (make-symbol "binds"))
        (syms (make-symbol "syms"))
        (vals (make-symbol "vals")))
    `(progn
       (let* ((,syms ,symbols)
              (,vals ,values)
              (,bodyfun (lambda () ,@body))
              (,binds ()))
         (while ,syms
           (push (list (pop ,syms) (list 'quote (pop ,vals))) ,binds))
         (eval (list 'let (nreverse ,binds)
                     (list 'funcall (list 'quote ,bodyfun))))))))

(defconst cl--labels-magic (make-symbol "cl--labels-magic"))

(defvar cl--labels-convert-cache nil)

(defun cl--labels-convert (f)
  "Special macro-expander to rename (function F) references in `cl-labels'."
  (cond
   ;; ¡¡Big Ugly Hack!! We can't use a compiler-macro because those are checked
   ;; *after* handling `function', but we want to stop macroexpansion from
   ;; being applied infinitely, so we use a cache to return the exact `form'
   ;; being expanded even though we don't receive it.
   ;; In Common Lisp, we'd use the `&whole' arg instead (see
   ;; "Macro Lambda Lists" in the CLHS).
   ((let ((symbols-with-pos-enabled nil)) ;Don't rewrite #'<X@5> => #'<X@3>
      (eq f (car cl--labels-convert-cache)))
    ;; This value should be `eq' to the `&whole' form.
    ;; If this is not the case, we have a bug.
    (prog1 (cdr cl--labels-convert-cache)
      ;; Drop it, so it can't accidentally interfere with some
      ;; unrelated subsequent use of `function' with the same symbol.
      (setq cl--labels-convert-cache nil)))
   (t
    (let* ((found (assq f macroexpand-all-environment))
           (replacement (and found
                             (ignore-errors
                               (funcall (cdr found) cl--labels-magic)))))
      (if (and replacement (eq cl--labels-magic (car replacement)))
          (nth 1 replacement)
        ;; FIXME: Here, we'd like to return the `&whole' form, but since Elisp
        ;; doesn't have that, we approximate it via `cl--labels-convert-cache'.
        (let ((res `(function ,f)))
          (setq cl--labels-convert-cache (cons f res))
          res))))))

;;;###autoload
(defmacro cl-flet (bindings &rest body)
  "Make local function definitions.
Each definition can take the form (FUNC EXP) where FUNC is the function
name, and EXP is an expression that returns the function value to which
it should be bound, or it can take the more common form (FUNC ARGLIST
BODY...) which is a shorthand for (FUNC (lambda ARGLIST BODY))
where BODY is wrapped in a `cl-block' named FUNC.

FUNC is defined only within FORM, not BODY, so you can't write recursive
function definitions.  Use `cl-labels' for that.  See Info node
`(cl) Function Bindings' for details.

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)"
  (declare (indent 1)
           ;; The first (symbolp form) case doesn't use `&name' because
           ;; it's hard to associate this name with the body of the function
           ;; that `form' will return (bug#65344).
           ;; We could try and use a `&name' for those cases where the
           ;; body of the function can be found, (e.g. the form wraps
           ;; some `prog1/progn/let' around the final `lambda'), but it's
           ;; not clear it's worth the trouble.
           (debug ((&rest [&or (symbolp form)
                               (&define [&name symbolp "@cl-flet@"]
                                        [&name [] gensym] ;Make it unique!
                                        cl-lambda-list
                                        cl-declarations-or-string
                                        [&optional ("interactive" interactive)]
                                        def-body)])
                   cl-declarations body)))
  (let ((binds ()) (newenv macroexpand-all-environment))
    (dolist (binding bindings)
      (let* ((var (make-symbol (format "--cl-%s--" (car binding))))
             (args-and-body (cdr binding))
             (args (car args-and-body))
             (body (cdr args-and-body)))
        (if (and (null body)
                 (macroexp-copyable-p args))
            ;; Optimize (cl-flet ((fun var)) body).
            (setq var args)
          (push (list var (if (null body)
                              args
                            (let ((parsed-body (macroexp-parse-body body)))
                              `(cl-function
                                (lambda ,args
                                  ,@(car parsed-body)
                                  (cl-block ,(car binding)
                                    ,@(cdr parsed-body)))))))
                binds))
	(push (cons (car binding)
                    (lambda (&rest args)
                      (if (eq (car args) cl--labels-magic)
                          (list cl--labels-magic var)
                        `(funcall ,var ,@args))))
              newenv)))
    ;; FIXME: Eliminate those functions which aren't referenced.
    (macroexp-let* (nreverse binds)
                   (macroexpand-all
                    `(progn ,@body)
                    ;; Don't override lexical-let's macro-expander.
                    (if (assq 'function newenv) newenv
                      (cons (cons 'function #'cl--labels-convert) newenv))))))

;;;###autoload
(defmacro cl-flet* (bindings &rest body)
  "Make local function definitions.
Like `cl-flet' but the definitions can refer to previous ones.

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)"
  (declare (indent 1) (debug cl-flet))
  (cond
   ((null bindings) (macroexp-progn body))
   ((null (cdr bindings)) `(cl-flet ,bindings ,@body))
   (t `(cl-flet (,(pop bindings)) (cl-flet* ,bindings ,@body)))))

(defun cl--self-tco (var fargs body)
  ;; This tries to "optimize" tail calls for the specific case
  ;; of recursive self-calls by replacing them with a `while' loop.
  ;; It is quite far from a general tail-call optimization, since it doesn't
  ;; even handle mutually recursive functions.
  (letrec
      ((done nil) ;; Non-nil if some TCO happened.
       ;; This var always holds the value nil until (just before) we
       ;; exit the loop.
       (retvar (make-symbol "retval"))
       (ofargs (mapcar (lambda (s) (if (memq s cl--lambda-list-keywords) s
                                (make-symbol (symbol-name s))))
                       fargs))
       (opt-exps (lambda (exps) ;; `exps' is in tail position!
                   (append (butlast exps)
                           (list (funcall opt (car (last exps)))))))
       (opt
        (lambda (exp) ;; `exp' is in tail position!
          (pcase exp
            ;; FIXME: Optimize `apply'?
            (`(funcall ,(pred (eq var)) . ,aargs)
             ;; This is a self-recursive call in tail position.
             (let ((sets nil)
                   (fargs ofargs))
               (while fargs
                 (pcase (pop fargs)
                   ('&rest
                    (push (pop fargs) sets)
                    (push `(list . ,aargs) sets)
                    ;; (cl-assert (null fargs))
                    )
                   ('&optional nil)
                   (farg
                    (push farg sets)
                    (push (pop aargs) sets))))
               (setq done t)
               `(progn (setq . ,(nreverse sets))
                       :recurse)))
            (`(progn . ,exps) `(progn . ,(funcall opt-exps exps)))
            (`(if ,cond ,then . ,else)
             `(if ,cond ,(funcall opt then) . ,(funcall opt-exps else)))
            (`(and  . ,exps) `(and . ,(funcall opt-exps exps)))
            (`(or ,arg) (funcall opt arg))
            (`(or ,arg . ,args)
             (let ((val (make-symbol "val")))
               `(let ((,val ,arg))
                  (if ,val ,(funcall opt val) ,(funcall opt `(or . ,args))))))
            (`(cond . ,conds)
             (let ((cs '()))
               (while conds
                 (pcase (pop conds)
                   (`(,exp)
                    (push (if conds
                              ;; This returns the value of `exp' but it's
                              ;; only in tail position if it's the
                              ;; last condition.
                              ;; Note: This may set the var before we
                              ;; actually exit the loop, but luckily it's
                              ;; only the case if we set the var to nil,
                              ;; so it does preserve the invariant that
                              ;; the var is nil until we exit the loop.
                              `((setq ,retvar ,exp) nil)
                            `(,(funcall opt exp)))
                          cs))
                   (exps
                    (push (funcall opt-exps exps) cs))))
               ;; No need to set `retvar' to return nil.
               `(cond . ,(nreverse cs))))
            ((and `(,(or 'let 'let*) ,bindings . ,exps)
                  (guard
                   ;; Note: it's OK for this `let' to shadow any
                   ;; of the formal arguments since we will only
                   ;; setq the fresh new `ofargs' vars instead ;-)
                   (let ((shadowings
                          (mapcar (lambda (b) (if (consp b) (car b) b)) bindings)))
                     (and
                      ;; If `var' is shadowed, then it clearly can't be
                      ;; tail-called any more.
                      (not (memq var shadowings))
                      ;; If any of the new bindings is a dynamic
                      ;; variable, the body is not in tail position.
                      (not (delq nil (mapcar #'macroexp--dynamic-variable-p
                                             shadowings)))))))
             `(,(car exp) ,bindings . ,(funcall opt-exps exps)))
            ((and `(condition-case ,err-var ,bodyform . ,handlers)
                  (guard (not (eq err-var var))))
             `(condition-case ,err-var
                  ,(if (assq :success handlers)
                       bodyform
                     `(progn (setq ,retvar ,bodyform) nil))
                . ,(mapcar (lambda (h)
                             (cons (car h) (funcall opt-exps (cdr h))))
                           handlers)))
            ('nil nil)  ;No need to set `retvar' to return nil.
            (_ `(progn (setq ,retvar ,exp) nil))))))

    (let ((optimized-body (funcall opt-exps body)))
      (if (not done)
          (cons fargs body)
        ;; We use two sets of vars: `ofargs' and `fargs' because we need
        ;; to be careful that if a closure captures a formal argument
        ;; in one iteration, it needs to capture a different binding
        ;; then that of other iterations, e.g.
        (cons
         ofargs
         `((let (,retvar)
             (while (let ,(delq nil
                                (cl-mapcar
                                 (lambda (a oa)
                                   (unless (memq a cl--lambda-list-keywords)
                                     (list a oa)))
                                 fargs ofargs))
                      . ,optimized-body))
             ,retvar)))))))

(defun cl--self-tco-on-form (var form)
  ;; Apply self-tco to the function returned by FORM, assuming that
  ;; it will be bound to VAR.
  (pcase form
    (`(function (lambda ,fargs . ,ebody)) form
     (pcase-let* ((`(,decls . ,body) (macroexp-parse-body ebody))
                  (`(,ofargs . ,obody) (cl--self-tco var fargs body)))
       `(function (lambda ,ofargs ,@decls . ,obody))))
    (`(let ,bindings ,form)
     `(let ,bindings ,(cl--self-tco-on-form var form)))
    (`(if ,cond ,exp1 ,exp2)
     `(if ,cond ,(cl--self-tco-on-form var exp1)
        ,(cl--self-tco-on-form var exp2)))
    (`(oclosure--fix-type ,exp1 ,exp2)
     `(oclosure--fix-type ,exp1 ,(cl--self-tco-on-form var exp2)))
    (_ form)))

;;;###autoload
(defmacro cl-labels (bindings &rest body)
  "Make local (recursive) function definitions.

Each definition can take the form (FUNC EXP) where FUNC is the function
name, and EXP is an expression that returns the function value to which
it should be bound, or it can take the more common form (FUNC ARGLIST
BODY...) which is a shorthand for (FUNC (lambda ARGLIST BODY))
where BODY is wrapped in a `cl-block' named FUNC.

FUNC is in scope in any BODY or EXP, as well as in FORM, so you can write
recursive and mutually recursive function definitions, with the caveat
that EXPs are evaluated in sequence and you cannot call a FUNC before its
EXP has been evaluated.
See Info node `(cl) Function Bindings' for details.

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)"
  (declare (indent 1) (debug cl-flet))
  (let ((binds ()) (newenv macroexpand-all-environment))
    (dolist (binding bindings)
      (let ((var (make-symbol (format "--cl-%s--" (car binding)))))
	(push (cons var binding) binds)
	(push (cons (car binding)
                    (lambda (&rest args)
                      (if (eq (car args) cl--labels-magic)
                          (list cl--labels-magic var)
                        (cl-list* 'funcall var args))))
              newenv)))
    ;; Don't override lexical-let's macro-expander.
    (unless (assq 'function newenv)
      (push (cons 'function #'cl--labels-convert) newenv))
    ;; Perform self-tail call elimination.
    `(letrec ,(mapcar
               (lambda (bind)
                 (pcase-let* ((`(,var ,fun ,sargs . ,sbody) bind))
                   `(,var ,(cl--self-tco-on-form
                            var (macroexpand-all
                                 (if (null sbody)
                                     sargs ;A (FUNC EXP) definition.
                                   (let ((parsed-body
                                          (macroexp-parse-body sbody)))
                                     `(cl-function
                                       (lambda ,sargs
                                         ,@(car parsed-body)
                                         (cl-block ,fun
                                           ,@(cdr parsed-body))))))
                                 newenv)))))
               (nreverse binds))
       . ,(macroexp-unprogn
           (macroexpand-all
            (macroexp-progn body)
            newenv)))))

(defvar edebug-lexical-macro-ctx)

(defun cl--edebug-macrolet-interposer (bindings pf &rest specs)
  ;; (cl-assert (null (cdr bindings)))
  (setq bindings (car bindings))
  (let ((edebug-lexical-macro-ctx
         (nconc (mapcar (lambda (binding)
                          (cons (car binding)
                                (when (eq 'declare (car-safe (nth 2 binding)))
                                  (nth 1 (assq 'debug (cdr (nth 2 binding)))))))
                        bindings)
                edebug-lexical-macro-ctx)))
    (funcall pf specs)))

;; The following ought to have a better definition for use with newer
;; byte compilers.
;;;###autoload
(defmacro cl-macrolet (bindings &rest body)
  "Make temporary macro definitions.
This is like `cl-flet', but for macros instead of functions.

\(fn ((NAME ARGLIST BODY...) ...) FORM...)"
  (declare (indent 1)
           (debug (&interpose (&rest (&define [&name symbolp "@cl-macrolet@"]
                                              [&name [] gensym] ;Make it unique!
                                              cl-macro-list
                                              cl-declarations-or-string
                                              def-body))
                              cl--edebug-macrolet-interposer
                              cl-declarations body)))
  (if (cdr bindings)
      `(cl-macrolet (,(car bindings)) (cl-macrolet ,(cdr bindings) ,@body))
    (if (null bindings) (macroexp-progn body)
      (let* ((name (caar bindings))
	     (res (cl--transform-lambda (cdar bindings) name)))
	(macroexpand-all (macroexp-progn body)
			 (cons (cons name
                                     (eval `(function (lambda ,@res)) t))
			       macroexpand-all-environment))))))

(defun cl--sm-macroexpand (exp &optional env)
  "Special macro expander used inside `cl-symbol-macrolet'."
  ;; FIXME: Arguably, this should be the official definition of `macroexpand'.
  (while (not (eq exp (setq exp (macroexpand-1 exp env)))))
  exp)

(defun cl--sm-macroexpand-1 (orig-fun exp &optional env)
  "Special macro expander advice used inside `cl-symbol-macrolet'.
This function extends `macroexpand-1' during macro expansion
of `cl-symbol-macrolet' to additionally expand symbol macros."
  (let ((exp (funcall orig-fun exp env))
        (venv (alist-get :cl-symbol-macros env)))
    (pcase exp
      ((pred symbolp)
       ;; Try symbol-macro expansion.
       (let ((symval (assq exp venv)))
         (if symval (cadr symval) exp)))
      (`(setq . ,args)
       ;; Convert setq to setf if required by symbol-macro expansion.
       (let ((convert nil))
         (while args
           (let* ((place (pop args))
                  ;; Here, we know `place' should be a symbol.
                  (symval (assq place venv)))
             (pop args)
             (when symval
               (setq convert t))))
         (if convert
             (cons 'setf (cdr exp))
           exp)))
      ;; CL's symbol-macrolet used to treat re-bindings as candidates for
      ;; expansion (turning the let into a letf if needed), contrary to
      ;; Common-Lisp where such re-bindings hide the symbol-macro.
      ;; Not sure if there actually is code out there which depends
      ;; on this behavior (haven't found any yet).
      ;; Such code should explicitly use `cl-letf' instead, I think.
      ;;
      ;; (`(,(or `let `let*) . ,(or `(,bindings . ,body) pcase--dontcare))
      ;;  (let ((letf nil) (found nil) (nbs ()))
      ;;    (dolist (binding bindings)
      ;;      (let* ((var (if (symbolp binding) binding (car binding)))
      ;;             (sm (assq var venv)))
      ;;        (push (if (not (cdr sm))
      ;;                  binding
      ;;                (let ((nexp (cadr sm)))
      ;;                  (setq found t)
      ;;                  (unless (symbolp nexp) (setq letf t))
      ;;                  (cons nexp (cdr-safe binding))))
      ;;              nbs)))
      ;;    (when found
      ;;      (setq exp `(,(if letf
      ;;                       (if (eq (car exp) 'let) 'cl-letf 'cl-letf*)
      ;;                     (car exp))
      ;;                  ,(nreverse nbs)
      ;;                  ,@body)))))
      ;;
      ;; We implement the Common-Lisp behavior, instead (see bug#26073):
      ;; The behavior of CL made sense in a dynamically scoped
      ;; language, but nowadays, lexical scoping semantics is more often
      ;; expected.
      (`(,(or 'let 'let*) . ,(or `(,bindings . ,body) pcase--dontcare))
       (let ((nbs ()) (found nil))
         (dolist (binding bindings)
           (let* ((var (if (symbolp binding) binding (car binding)))
                  (val (and found (consp binding) (eq 'let* (car exp))
                            (list (macroexpand-all (cadr binding)
                                                   env)))))
             (push (if (assq var venv)
                       ;; This binding should hide "its" surrounding
                       ;; symbol-macro, but given the way macroexpand-all
                       ;; works (i.e. the `env' we receive as input will
                       ;; be (re)applied to the code we return), we can't
                       ;; prevent application of `env' to the
                       ;; sub-expressions, so we need to α-rename this
                       ;; variable instead.
                       (let ((nvar (make-symbol (symbol-name var))))
                         (setq found t)
                         (push (list var nvar) venv)
                         (push (cons :cl-symbol-macros venv) env)
                         (cons nvar (or val (cdr-safe binding))))
                     (if val (cons var val) binding))
                   nbs)))
         (if found
             `(,(car exp)
               ,(nreverse nbs)
               ,@(macroexp-unprogn
                  (macroexpand-all (macroexp-progn body)
                                   env)))
           exp)))
      ;; Do the same as for `let' but for variables introduced
      ;; via other means, such as `lambda' and `condition-case'.
      (`(function (lambda ,args . ,body))
       (let ((nargs ()) (found nil))
         (dolist (var args)
           (push (cond
                  ((memq var '(&optional &rest)) var)
                  ((assq var venv)
                   (let ((nvar (make-symbol (symbol-name var))))
                     (setq found t)
                     (push (list var nvar) venv)
                     (push (cons :cl-symbol-macros venv) env)
                     nvar))
                  (t var))
                 nargs))
         (if found
             `(function
               (lambda ,(nreverse nargs)
                 . ,(mapcar (lambda (exp)
                              (macroexpand-all exp env))
                            body)))
           exp)))
      ((and `(condition-case ,var ,exp . ,clauses)
            (guard (assq var venv)))
       (let ((nvar (make-symbol (symbol-name var))))
         (push (list var nvar) venv)
         (push (cons :cl-symbol-macros venv) env)
         `(condition-case ,nvar ,(macroexpand-all exp env)
            . ,(mapcar
                (lambda (clause)
                  `(,(car clause)
                    . ,(mapcar (lambda (exp)
                                 (macroexpand-all exp env))
                               (cdr clause))))
                clauses))))
      (_ exp))))

;;;###autoload
(defmacro cl-symbol-macrolet (bindings &rest body)
  "Make symbol macro definitions.
Within the body FORMs, references to the variable NAME will be replaced
by EXPANSION, and (setq NAME ...) will act like (setf EXPANSION ...).

\(fn ((NAME EXPANSION) ...) FORM...)"
  (declare (indent 1) (debug ((&rest (symbolp sexp)) cl-declarations body)))
  (let ((malformed-bindings nil)
        (advised (advice-member-p #'cl--sm-macroexpand 'macroexpand)))
    (dolist (binding bindings)
      (unless (and (consp binding) (symbolp (car binding))
                   (consp (cdr binding)) (null (cddr binding)))
        (push binding malformed-bindings)))
    (unwind-protect
        (progn
          (unless advised
            (advice-add 'macroexpand :override #'cl--sm-macroexpand)
            (advice-add 'macroexpand-1 :around #'cl--sm-macroexpand-1))
          (let* ((venv (cdr (assq :cl-symbol-macros
                                  macroexpand-all-environment)))
                 (expansion
                  (macroexpand-all (macroexp-progn body)
                                   (cons (cons :cl-symbol-macros
                                               (append bindings venv))
                                         macroexpand-all-environment))))
            (if malformed-bindings
                (let ((rev-malformed-bindings (nreverse malformed-bindings)))
                  (macroexp-warn-and-return
                   (format-message "Malformed `cl-symbol-macrolet' binding(s): %S"
                                   rev-malformed-bindings)
                   expansion nil nil rev-malformed-bindings))
              expansion)))
      (unless advised
        (advice-remove 'macroexpand   #'cl--sm-macroexpand)
        (advice-remove 'macroexpand-1 #'cl--sm-macroexpand-1)))))

;;;###autoload
(defmacro cl-with-gensyms (names &rest body)
  "Bind each of NAMES to an uninterned symbol and evaluate BODY."
  (declare (debug (sexp body)) (indent 1))
  `(let ,(cl-loop for name in names collect
                  `(,name (gensym (symbol-name ',name))))
     ,@body))

;;;###autoload
(defmacro cl-once-only (names &rest body)
  "Generate code to evaluate each of NAMES just once in BODY.

This macro helps with writing other macros.  Each of NAMES is
either (NAME FORM) or NAME, which latter means (NAME NAME).
During macroexpansion, each NAME is bound to an uninterned
symbol.  The expansion evaluates each FORM and binds it to the
corresponding uninterned symbol.

For example, consider this macro:

    (defmacro my-cons (x)
      (cl-once-only (x)
        \\=`(cons ,x ,x)))

The call (my-cons (pop y)) will expand to something like this:

    (let ((g1 (pop y)))
      (cons g1 g1))

The use of `cl-once-only' ensures that the pop is performed only
once, as intended.

See also `macroexp-let2'."
  (declare (debug (sexp body)) (indent 1))
  (setq names (mapcar #'ensure-list names))
  (let ((our-gensyms (cl-loop for _ in names collect (gensym))))
    ;; During macroexpansion, obtain a gensym for each NAME.
    `(let ,(cl-loop for sym in our-gensyms collect `(,sym (gensym)))
       ;; Evaluate each FORM and bind to the corresponding gensym.
       ;;
       ;; We require this explicit call to `list' rather than using
       ;; (,,@(cl-loop ...)) due to a limitation of Elisp's backquote.
       `(let ,(list
               ,@(cl-loop for name in names for gensym in our-gensyms
                          for to-eval = (or (cadr name) (car name))
                          collect ``(,,gensym ,,to-eval)))
          ;; During macroexpansion, bind each NAME to its gensym.
          ,(let ,(cl-loop for name in names for gensym in our-gensyms
                          collect `(,(car name) ,gensym))
             ,@body)))))

;;;###autoload
(defmacro cl-with-accessors (bindings instance &rest body)
  "Use BINDINGS as function calls on INSTANCE inside BODY.

This macro helps when writing code that makes repeated use of the
accessor functions of a structure or object instance, such as those
created by `cl-defstruct' and `defclass'.

BINDINGS is a list of (NAME ACCESSOR) pairs.  Inside BODY, NAME is
treated as the function call (ACCESSOR INSTANCE) using
`cl-symbol-macrolet'.  NAME can be used with `setf' and `setq' as a
generalized variable.  Because of how the accessor is used,
`cl-with-accessors' can be used with any generalized variable that can
take a single argument, such as `car' and `cdr'.

See also the macro `with-slots' described in the Info
node `(eieio)Accessing Slots', which is similar, but uses slot names
instead of accessor functions.

\(fn ((NAME ACCESSOR) ...) INSTANCE &rest BODY)"
  (declare (debug [(&rest (symbolp symbolp)) form body])
           (indent 2))
  (cond ((null body)
         (macroexp-warn-and-return "`cl-with-accessors' used with empty body"
                                   nil 'empty-body))
        ((null bindings)
         (macroexp-warn-and-return "`cl-with-accessors' used without accessors"
                                   (macroexp-progn body)
                                   'suspicious))
        (t
         (cl-once-only (instance)
           (let ((symbol-macros))
             (dolist (b bindings)
               (pcase b
                 (`(,(and (pred symbolp) var)
                    ,(and (pred symbolp) accessor))
                  (push `(,var (,accessor ,instance))
                        symbol-macros))
                 (_
                  (error "Malformed `cl-with-accessors' binding: %S" b))))
             `(cl-symbol-macrolet
                  ,symbol-macros
                ,@body))))))

;;; Multiple values.

;;;###autoload
(defmacro cl-multiple-value-bind (vars form &rest body)
  "Collect multiple return values.
FORM must return a list; the BODY is then executed with the first N elements
of this list bound (`let'-style) to each of the symbols SYM in turn.  This
is analogous to the Common Lisp `multiple-value-bind' macro, using lists to
simulate true multiple return values.  For compatibility, (cl-values A B C) is
a synonym for (list A B C).

\(fn (SYM...) FORM BODY)"
  (declare (indent 2) (debug ((&rest symbolp) form body)))
  (let ((temp (make-symbol "--cl-var--")) (n -1))
    `(let* ((,temp ,form)
            ,@(mapcar (lambda (v)
                        (list v `(nth ,(setq n (1+ n)) ,temp)))
                      vars))
       ,@body)))

;;;###autoload
(defmacro cl-multiple-value-setq (vars form)
  "Collect multiple return values.
FORM must return a list; the first N elements of this list are stored in
each of the symbols SYM in turn.  This is analogous to the Common Lisp
`multiple-value-setq' macro, using lists to simulate true multiple return
values.  For compatibility, (cl-values A B C) is a synonym for (list A B C).

\(fn (SYM...) FORM)"
  (declare (indent 1) (debug ((&rest symbolp) form)))
  (cond ((null vars) `(progn ,form nil))
	((null (cdr vars)) `(setq ,(car vars) (car ,form)))
	(t
	 (let* ((temp (make-symbol "--cl-var--")) (n 0))
	   `(let ((,temp ,form))
              (prog1 (setq ,(pop vars) (car ,temp))
                (setq ,@(apply #'nconc
                               (mapcar (lambda (v)
                                         (list v `(nth ,(setq n (1+ n))
                                                       ,temp)))
                                       vars)))))))))


;;; Declarations.

;;;###autoload
(define-obsolete-function-alias 'cl-locally #'progn "31.1")

;;;###autoload
(defmacro cl-the (type form)
  "Return FORM.  If type-checking is enabled, assert that it is of TYPE."
  (declare (indent 1) (debug (cl-type-spec form)))
  ;; When native compiling possibly add the appropriate type hint.
  (when (and (boundp 'byte-native-compiling)
             byte-native-compiling)
    (setf form
          (cl-case type
            (fixnum `(comp-hint-fixnum ,form))
            (cons `(comp-hint-cons ,form))
            (otherwise form))))
  (if (not (or (not (macroexp-compiling-p))
               (< cl--optimize-speed 3)
               (= cl--optimize-safety 3)))
      form
    (macroexp-let2 macroexp-copyable-p temp form
      `(progn (unless (cl-typep ,temp ',type)
                (signal 'wrong-type-argument
                        (list ',type ,temp ',form)))
              ,temp))))

;;;###autoload
(or (assq 'cl-optimize defun-declarations-alist)
    (let ((x (list 'cl-optimize #'cl--optimize)))
      (push x macro-declarations-alist)
      (push x defun-declarations-alist)))

;;;###cl-autoload
(defun cl--optimize (f _args &rest qualities)
  "Serve `cl-optimize' in function declarations.
Example:
  (defun foo (x)
    (declare (cl-optimize (speed 3) (safety 0)))
    x)"
  ;; FIXME this should make use of `cl--declare-stack' but I suspect
  ;; this mechanism should be reviewed first.
  (cl-loop for (qly val) in qualities
           do (cl-ecase qly
                (speed
                 (setf cl--optimize-speed val)
                 (byte-run--set-speed f nil val))
                (safety
                 (setf cl--optimize-safety val)))))

(defvar cl--proclaim-history t)    ; for future compilers
(defvar cl--declare-stack t)       ; for future compilers

(defun cl--do-proclaim (spec hist)
  (and hist (listp cl--proclaim-history) (push spec cl--proclaim-history))
  (cond ((eq (car-safe spec) 'special)
	 (if (boundp 'byte-compile-bound-variables)
	     (setq byte-compile-bound-variables
		   (append (cdr spec) byte-compile-bound-variables))))

	((eq (car-safe spec) 'inline)
	 (while (setq spec (cdr spec))
	   (or (memq (get (car spec) 'byte-optimizer)
		     '(nil byte-compile-inline-expand))
	       (error "%s already has a byte-optimizer, can't make it inline"
		      (car spec)))
	   (put (car spec) 'byte-optimizer #'byte-compile-inline-expand)))

	((eq (car-safe spec) 'notinline)
	 (while (setq spec (cdr spec))
	   (if (eq (get (car spec) 'byte-optimizer)
		   #'byte-compile-inline-expand)
	       (put (car spec) 'byte-optimizer nil))))

	((eq (car-safe spec) 'optimize)
	 (let ((speed (assq (nth 1 (assq 'speed (cdr spec)))
			    '((0 nil) (1 t) (2 t) (3 t))))
	       (safety (assq (nth 1 (assq 'safety (cdr spec)))
			     '((0 t) (1 nil) (2 nil) (3 nil)))))
	   (if speed (setq cl--optimize-speed (car speed)
			   byte-optimize (nth 1 speed)))
	   (if safety (setq cl--optimize-safety (car safety)
			    byte-compile-delete-errors (nth 1 safety)))))

	((and (eq (car-safe spec) 'warn) (boundp 'byte-compile-warnings))
	 (while (setq spec (cdr spec))
	   (if (consp (car spec))
               (if (eq (cadar spec) 0)
                   (byte-compile-disable-warning (caar spec))
                 (byte-compile-enable-warning (caar spec)))))))
  nil)

;;; Process any proclamations made before cl-macs was loaded.
(defvar cl--proclaims-deferred)
(let ((p (reverse cl--proclaims-deferred)))
  (while p (cl--do-proclaim (pop p) t))
  (setq cl--proclaims-deferred nil))

;;;###autoload
(defmacro cl-declare (&rest specs)
  "Declare SPECS about the current function while compiling.
For instance

  (cl-declare (warn 0))

will turn off byte-compile warnings in the function.
See Info node `(cl)Declarations' for details."
  (declare (obsolete defvar "31.1"))
  (if (macroexp-compiling-p)
      (while specs
	(if (listp cl--declare-stack) (push (car specs) cl--declare-stack))
	(cl--do-proclaim (pop specs) nil)))
  nil)

;;; The standard modify macros.

;; `setf' is now part of core Elisp, defined in gv.el.

;;;###autoload
(defmacro cl-psetf (&rest args)
  "Set PLACEs to the values VALs in parallel.
This is like `setf', except that all VAL forms are evaluated (in order)
before assigning any PLACEs to the corresponding values.

\(fn PLACE VAL PLACE VAL ...)"
  (declare (debug setf))
  (let ((p args) (simple t) (vars nil))
    (while p
      (if (or (not (symbolp (car p))) (cl--expr-depends-p (nth 1 p) vars))
	  (setq simple nil))
      (if (memq (car p) vars)
	  (error "Destination duplicated in psetf: %s" (car p)))
      (push (pop p) vars)
      (or p (error "Odd number of arguments to cl-psetf"))
      (pop p))
    (if simple
	`(progn (setq ,@args) nil)
      (setq args (reverse args))
      (let ((expr `(setf ,(cadr args) ,(car args))))
	(while (setq args (cddr args))
	  (setq expr `(setf ,(cadr args) (prog1 ,(car args) ,expr))))
	`(progn ,expr nil)))))

;;;###autoload
(defmacro cl-remf (place tag)
  "Remove TAG from property list PLACE.
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The form returns true if TAG was found and removed, nil otherwise."
  (declare (debug (place form)))
  (gv-letplace (tval setter) place
    (macroexp-let2 macroexp-copyable-p ttag tag
      `(if (eq ,ttag (car ,tval))
           (progn ,(funcall setter `(cddr ,tval))
                  t)
         (cl--do-remf ,tval ,ttag)))))

;;;###autoload
(defmacro cl-shiftf (place &rest args)
  "Shift left among PLACEs.
Example: (cl-shiftf A B C) sets A to B, B to C, and returns the old A.
Each PLACE may be a symbol, or any generalized variable allowed by `setf'.

\(fn PLACE... VAL)"
  (declare (debug (&rest place)))
  (cond
   ((null args) place)
   ((symbolp place) `(prog1 ,place (setq ,place (cl-shiftf ,@args))))
   (t
    (gv-letplace (getter setter) place
      `(prog1 ,getter
         ,(funcall setter `(cl-shiftf ,@args)))))))

;;;###autoload
(defmacro cl-rotatef (&rest args)
  "Rotate left among PLACEs.
Example: (cl-rotatef A B C) sets A to B, B to C, and C to A.  It returns nil.
Each PLACE may be a symbol, or any generalized variable allowed by `setf'.

\(fn PLACE...)"
  (declare (debug (&rest place)))
  (if (not (memq nil (mapcar #'symbolp args)))
      (and (cdr args)
	   (let ((sets nil)
		 (first (car args)))
	     (while (cdr args)
	       (setq sets (nconc sets (list (pop args) (car args)))))
	     `(cl-psetf ,@sets ,(car args) ,first)))
    (let* ((places (reverse args))
	   (temp (make-symbol "--cl-rotatef--"))
	   (form temp))
      (while (cdr places)
        (setq form
              (gv-letplace (getter setter) (pop places)
                `(prog1 ,getter ,(funcall setter form)))))
      (gv-letplace (getter setter) (car places)
	(macroexp-let* `((,temp ,getter))
                       `(progn ,(funcall setter form) nil))))))

;; FIXME: `letf' is unsatisfactory because it does not really "restore" the
;; previous state.  If the getter/setter loses information, that info is
;; not recovered.

(defun cl--letf (bindings simplebinds binds body)
  ;; It's not quite clear what the semantics of cl-letf should be.
  ;; E.g. in (cl-letf ((PLACE1 VAL1) (PLACE2 VAL2)) BODY), while it's clear
  ;; that the actual assignments ("bindings") should only happen after
  ;; evaluating VAL1 and VAL2, it's not clear when the sub-expressions of
  ;; PLACE1 and PLACE2 should be evaluated.  Should we have
  ;;    PLACE1; VAL1; PLACE2; VAL2; bind1; bind2
  ;; or
  ;;    VAL1; VAL2; PLACE1; PLACE2; bind1; bind2
  ;; or
  ;;    VAL1; VAL2; PLACE1; bind1; PLACE2; bind2
  ;; Common-Lisp's `psetf' does the first, so we'll do the same.
  (if (null bindings)
      (if (and (null binds) (null simplebinds)) (macroexp-progn body)
        (let ((body-form
               (macroexp-progn
                (append
                 (delq nil
                       (mapcar (lambda (x)
                                 (pcase x
                                   ;; If there's no vnew, do nothing.
                                   (`(,_vold ,_getter ,setter ,vnew)
                                    (funcall setter vnew))))
                               binds))
                 body))))
        `(let* (,@(mapcar (lambda (x)
                            (pcase-let ((`(,vold ,getter ,_setter ,_vnew) x))
                              (list vold getter)))
                          binds)
                ,@simplebinds)
           ,(if binds
                `(unwind-protect ,body-form
                   ,@(mapcar (lambda (x)
                               (pcase-let ((`(,vold ,_getter ,setter ,_vnew) x))
                                 (funcall setter vold)))
                             binds))
              body-form))))
    (let* ((binding (car bindings))
           (place (car binding)))
      (gv-letplace (getter setter) place
        (macroexp-let2 nil vnew (cadr binding)
          (if (and (symbolp place)
                   ;; `place' could be some symbol-macro.
                   (eq place getter))
              ;; Special-case for simple variables.
              ;; FIXME: We currently only use this special case when `place'
              ;; is a simple var.  Should we also use it when the
              ;; macroexpansion of `place' is a simple var (i.e. when
              ;; getter+setter is the same as that of a simple var)?
              (cl--letf (cdr bindings)
                        (cons `(,getter ,(if (cdr binding) vnew getter))
                              simplebinds)
                        binds body)
            (cl--letf (cdr bindings) simplebinds
                      (cons `(,(make-symbol "old") ,getter ,setter
                              ,@(if (cdr binding) (list vnew)))
                            binds)
                      body)))))))

;;;###autoload
(defmacro cl-letf (bindings &rest body)
  "Temporarily bind to PLACEs.
This is the analogue of `let', but with generalized variables (in the
sense of `setf') for the PLACEs.  Each PLACE is set to the corresponding
VALUE, then the BODY forms are executed.  On exit, either normally or
because of a `throw' or error, the PLACEs are set back to their original
values.  Note that this macro is *not* available in Common Lisp.
As a special case, if `(PLACE)' is used instead of `(PLACE VALUE)',
the PLACE is not modified before executing BODY.

See info node `(cl) Modify Macros' for details.

\(fn ((PLACE VALUE) ...) BODY...)"
  (declare (indent 1) (debug ((&rest [&or (symbolp form)
                                          (gate gv-place &optional form)])
                              body)))
  (if (and (not (cdr bindings)) (cdar bindings) (symbolp (caar bindings))
           (not (assq (caar bindings)
                      (alist-get :cl-symbol-macros macroexpand-all-environment))))
      `(let ,bindings ,@body)
    (cl--letf bindings () () body)))

;;;###autoload
(defmacro cl-letf* (bindings &rest body)
  "Temporarily bind to PLACEs.
Like `cl-letf' but where the bindings are performed one at a time,
rather than all at the end (i.e. like `let*' rather than like `let')."
  (declare (indent 1) (debug cl-letf))
  (dolist (binding (reverse bindings))
    (setq body (list `(cl-letf (,binding) ,@body))))
  (macroexp-progn body))

;;;###autoload
(defmacro cl-callf (func place &rest args)
  "Set PLACE to (FUNC PLACE ARGS...).
FUNC should be an unquoted function name or a lambda expression.
PLACE may be a symbol, or any generalized variable allowed by
`setf'."
  (declare (indent 2) (debug (cl-function place &rest form)))
  (gv-letplace (getter setter) place
    (let* ((rargs (cons getter args)))
      (funcall setter
               (if (symbolp func) (cons func rargs)
                 `(funcall #',func ,@rargs))))))

;;;###autoload
(defmacro cl-callf2 (func arg1 place &rest args)
  "Set PLACE to (FUNC ARG1 PLACE ARGS...).
Like `cl-callf', but PLACE is the second argument of FUNC, not the first.

\(fn FUNC ARG1 PLACE ARGS...)"
  (declare (indent 3) (debug (cl-function form place &rest form)))
  (if (and (cl--safe-expr-p arg1) (cl--simple-expr-p place) (symbolp func))
      `(setf ,place (,func ,arg1 ,place ,@args))
    (macroexp-let2 nil a1 arg1
      (gv-letplace (getter setter) place
        (let* ((rargs (cl-list* a1 getter args)))
          (funcall setter
                   (if (symbolp func) (cons func rargs)
                     `(funcall #',func ,@rargs))))))))

;;;###autoload
(defmacro cl-defsubst (name args &rest body)
  "Define NAME as a function.
Like `defun', except the function is automatically declared `inline' and
the arguments are immutable.
ARGLIST allows full Common Lisp conventions, and BODY is implicitly
surrounded by (cl-block NAME ...).
The function's arguments should be treated as immutable.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (debug cl-defun) (indent 2))
  (let* ((argns (cl--arglist-args args))
	 (real-args (if (eq '&cl-defs (car args)) (cddr args) args))
         (p argns)
         ;; (pbody (cons 'progn body))
         )
    (while (and p (eq (cl--expr-contains real-args (car p)) 1)) (pop p))
    `(progn
       ,(if p nil   ; give up if defaults refer to earlier args
          `(cl-define-compiler-macro ,name
             ,(if (memq '&key args)
                  `(&whole cl-whole &cl-quote ,@args)
                (cons '&cl-quote args))
             ;; NB.  This will produce incorrect results in some
             ;; cases, as our coding conventions says that the first
             ;; line must be a full sentence.  However, if we don't
             ;; word wrap we will have byte-compiler warnings about
             ;; overly long docstrings.  So we can't have a perfect
             ;; result here, and choose to avoid the byte-compiler
             ;; warnings.
             ,(internal--format-docstring-line "compiler-macro for `%s'." name)
             (cl--defsubst-expand
              ',argns '(cl-block ,name ,@(cdr (macroexp-parse-body body)))
              nil
              ,(and (memq '&key args) 'cl-whole) nil ,@argns)))
       (cl-defun ,name ,args ,@body))))

(defun cl--defsubst-expand (argns body _simple whole _unsafe &rest argvs)
  (if (and whole (not (cl--safe-expr-p (macroexp-progn argvs))))
      whole
    ;; Function arguments are unconditionally statically scoped (bug#47552).
    (cl--slet (cl-mapcar #'list argns argvs) body 'nowarn)))

;;; Structures.

(defmacro cl--find-class (type)
  `(get ,type 'cl--class))

;; Rather than hard code cl-structure-object, we indirect through this variable
;; for bootstrapping reasons.
(defvar cl--struct-default-parent nil)

(defvar cl--struct-inline t
  "If non-nil, `cl-defstruct' will define inlinable functions.")

;;;###autoload
(defmacro cl-defstruct (struct &rest descs)
  "Define a struct type.
This macro defines a new data type called NAME that stores data
in SLOTs.  It defines a `make-NAME' constructor, a `copy-NAME'
copier, a `NAME-p' predicate, and slot accessors named `NAME-SLOT'.
You can use the accessors to set the corresponding slots, via `setf'.

NAME is usually a symbol, but may instead take the form (NAME
OPTIONS...), where each OPTION is either a single keyword
or (KEYWORD VALUE) where KEYWORD can be one of `:conc-name',
`:constructor', `:copier', `:predicate', `:type', `:named',
`:initial-offset', `:print-function', `:noinline', or `:include'.
See Info node `(cl)Structures' for the description of the
options.

The first element in SLOTS can be a doc string.

The rest of the elements in SLOTS is a list of SLOT elements,
each of which should either be a symbol, or take the form (SNAME
SDEFAULT SOPTIONS...), where SDEFAULT is the default value of
that slot and SOPTIONS are keyword-value pairs for that slot.

Supported keywords for slots are:
- `:read-only': If this has a non-nil value, that slot cannot be set via `setf'.
- `:documentation': this is a docstring describing the slot.
- `:type': the type of the field; currently only used for documentation.

To see the documentation for a defined struct type, use
\\[describe-symbol] or \\[cl-describe-type].

\(fn NAME &optional DOCSTRING &rest SLOTS)"
  (declare (doc-string 2) (indent 1)
           (debug
            (&define                    ;Makes top-level form not be wrapped.
             [&or symbolp
                  (gate ;; FIXME: Why?
                   symbolp &rest
                   [&or (":constructor" &define name &optional cl-lambda-list)
                        sexp])]
             [&optional stringp]
             ;; All the above is for the following def-form.
             &rest &or symbolp (symbolp &optional def-form &rest sexp))))
  (let* ((name (if (consp struct) (car struct) struct))
	 (warning nil)
	 (opts (cdr-safe struct))
	 (slots nil)
	 (defaults nil)
	 (conc-name (concat (symbol-name name) "-"))
	 (constructor (intern (format "make-%s" name)))
	 (constrs nil)
	 (copier (intern (format "copy-%s" name)))
	 (predicate (intern (format "%s-p" name)))
	 (print-func nil) (print-auto nil)
	 (safety (if (macroexp-compiling-p) cl--optimize-safety 3))
	 (include nil)
         ;; There are 4 types of structs:
         ;; - `vector' type: means we should use a vector, which can come
         ;;   with or without a tag `name', which is usually in slot 0
         ;;   but obeys :initial-offset.
         ;; - `list' type: same as `vector' but using lists.
         ;; - `record' type: means we should use a record, which necessarily
         ;;   comes tagged in slot 0.  Currently we'll use the `name' as
         ;;   the tag, but we may want to change it so that the class object
         ;;   is used as the tag.
         ;; - nil type: this is the "pre-record default", which uses a vector
         ;;   with a tag in slot 0 which is a symbol of the form
         ;;   `cl-struct-NAME'.  We need to still support this for backward
         ;;   compatibility with old .elc files.
	 (tag name)
	 (tag-symbol (intern (format "cl-struct-%s-tags" name)))
	 (include-descs nil)
	 (include-name nil)
	 (type nil)         ;nil here means not specified explicitly.
	 (named nil)
         (cldefsym (if cl--struct-inline 'cl-defsubst 'cl-defun))
         (defsym (if cl--struct-inline 'cl-defsubst 'defun))
	 (forms nil)
         (docstring (if (stringp (car descs)) (pop descs)))
         (dynbound-slotnames '())
	 pred-form pred-check)
    ;; Can't use `cl-check-type' yet.
    (unless (cl--struct-name-p name)
      (signal 'wrong-type-argument (list 'cl-struct-name-p name 'name)))
    (setq descs (cons '(cl-tag-slot)
                      (mapcar (lambda (x) (if (consp x) x (list x)))
			      descs)))
    (while opts
      (let ((opt (if (consp (car opts)) (caar opts) (car opts)))
	    (args (cdr-safe (pop opts))))
	(cond ((eq opt :conc-name)
	       (if args
		   (setq conc-name (if (car args)
				       (symbol-name (car args)) ""))))
	      ((eq opt :constructor)
	       (if (cdr args)
                   (progn
                     ;; If this defines a constructor of the same name as
                     ;; the default one, don't define the default.
                     (if (eq (car args) constructor)
                         (setq constructor nil))
                     (push args constrs))
		 (if args (setq constructor (car args)))))
	      ((eq opt :copier)
	       (if args (setq copier (car args))))
	      ((eq opt :predicate)
	       (if args (setq predicate (car args))))
	      ((eq opt :include)
               ;; FIXME: Actually, we can include more than once as long as
               ;; we include EIEIO classes rather than cl-structs!
               (when include-name (error "Can't :include more than once"))
               (setq include-name (car args))
               (setq include-descs (mapcar (lambda (x)
                                             (if (consp x) x (list x)))
                                           (cdr args))))
	      ((eq opt :print-function)
	       (setq print-func (car args)))
	      ((eq opt :type)
	       (setq type (car args))
               (unless (memq type '(vector list))
                 (error "Invalid :type specifier: %s" type)))
	      ((eq opt :named)
	       (setq named t))
	      ((eq opt :noinline)
	       (setq defsym 'defun) (setq cldefsym 'cl-defun))
	      ((eq opt :initial-offset)
	       (setq descs (nconc (make-list (car args) '(cl-skip-slot))
				  descs)))
	      (t
	       (setq warning
	             (macroexp-warn-and-return
	              (format "Structure option %S unrecognized" opt)
	              warning nil nil (list opt struct)))))))
    (unless (or include-name type
                ;; Don't create a bogus parent to `cl-structure-object'
                ;; while compiling the (cl-defstruct cl-structure-object ..)
                ;; in `cl-preloaded.el'.
                (eq name cl--struct-default-parent))
      (setq include-name cl--struct-default-parent))
    (when include-name (setq include (cl--struct-get-class include-name)))
    (if print-func
	(setq print-func
              `(progn (funcall #',print-func cl-x cl-s cl-n) t))
      (or type (and include (not (cl--struct-class-print include)))
	  (setq print-auto t
		print-func (and (or (not (or include type)) (null print-func))
				`(progn
                                   (princ ,(format "#S(%s" name) cl-s))))))
    (if include
	(let* ((inc-type (cl--struct-class-type include))
               (old-descs (cl-struct-slot-info include)))
	  (and type (not (eq inc-type type))
	       (error ":type disagrees with :include for %s" name))
	  (while include-descs
	    (setcar (memq (or (assq (caar include-descs) old-descs)
			      (error "No slot %s in included struct %s"
				     (caar include-descs) include))
			  old-descs)
		    (pop include-descs)))
	  (setq descs (append old-descs (delq (assq 'cl-tag-slot descs) descs))
		type inc-type
		named (if (memq type '(vector list))
                          (assq 'cl-tag-slot descs)
                        'true))
	  (if (cl--struct-class-named include) (setq named t)))
      (unless type
	(setq named 'true)))
    (or named (setq descs (delq (assq 'cl-tag-slot descs) descs)))
    (when (and (null predicate) named)
      (setq predicate (intern (format "cl--struct-%s-p" name))))
    (setq pred-form (and named
			 (let ((pos (- (length descs)
				       (length (memq (assq 'cl-tag-slot descs)
						     descs)))))
			   (cond
                            ((null type) ;Record type.
                             `(memq (type-of cl-x) ,tag-symbol))
                            ((eq type 'vector)
                             `(and (vectorp cl-x)
                                   (>= (length cl-x) ,(length descs))
                                   (memq (aref cl-x ,pos) ,tag-symbol)))
                            ((= pos 0) `(memq (car-safe cl-x) ,tag-symbol))
                            (t `(and (consp cl-x)
				     (memq (nth ,pos cl-x) ,tag-symbol))))))
	  pred-check (and pred-form (> safety 0)
                          (if (and (eq (caadr pred-form) 'vectorp)
				   (= safety 1))
                              (cons 'and (cdddr pred-form))
                            `(,predicate cl-x))))
    (when pred-form
      (push `(eval-and-compile
               ;; Define the predicate to be effective at compile time
               ;; as native comp relies on `cl-typep' that relies on
               ;; predicates to be defined as they are registered in
               ;; cl-deftype-satisfies.
               (,defsym ,predicate (cl-x)
               (declare (side-effect-free error-free) (pure t))
               ,(if (eq (car pred-form) 'and)
                    (append pred-form '(t))
                  `(and ,pred-form t)))
               (define-symbol-prop ',name 'cl-deftype-satisfies ',predicate))
            forms))
    (let ((pos 0) (descp descs))
      (while descp
	(let* ((desc (pop descp))
	       (slot (pop desc)))
	  (when (macroexp--dynamic-variable-p slot)
	    (push slot dynbound-slotnames))
	  (if (memq slot '(cl-tag-slot cl-skip-slot))
	      (progn
		(push nil slots)
		(push (and (eq slot 'cl-tag-slot) `',tag)
			 defaults))
	    (if (assq slot descp)
		(error "Duplicate slots named %s in %s" slot name))
	    (let ((accessor (intern (format "%s%s" conc-name slot)))
                  (default-value (pop desc))
                  (doc (plist-get desc :documentation))
                  (access-body
                   `(progn
                      ,@(and pred-check
			     (list `(or ,pred-check
                                        (signal 'wrong-type-argument
                                                (list ',name cl-x)))))
                      ,(if (memq type '(nil vector)) `(aref cl-x ,pos)
                         (if (= pos 0) '(car cl-x)
                           `(nth ,pos cl-x))))))
	      (push slot slots)
	      (push default-value defaults)
              ;; The arg "cl-x" is referenced by name in e.g. pred-form
	      ;; and pred-check, so changing it is not straightforward.
	      (push `(,defsym ,accessor (cl-x)
                       ,(let ((long-docstring
                               (format "Access slot \"%s\" of `%s' struct X."
                                       slot name)))
                          (concat
                           ;; NB.  This will produce incorrect results
                           ;; in some cases, as our coding conventions
                           ;; says that the first line must be a full
                           ;; sentence.  However, if we don't word
                           ;; wrap we will have byte-compiler warnings
                           ;; about overly long docstrings.  So we
                           ;; can't have a perfect result here, and
                           ;; choose to avoid the byte-compiler
                           ;; warnings.
                           (if (>= (length long-docstring)
                                   (or (bound-and-true-p
                                        byte-compile-docstring-max-column)
                                       80))
                               (concat
                                (internal--format-docstring-line
                                 "Access slot \"%s\" of X." slot)
                                "\n"
                                (internal--format-docstring-line
                                 "Struct X is a `%s'." name))
                             (internal--format-docstring-line long-docstring))
                           (if doc (concat "\n" doc) "")
                           "\n"
                           (format "\n\n(fn %s X)" accessor)))
                       (declare (side-effect-free t))
                       ,access-body)
                    forms)
              ;; FIXME: This hack is to document this as a generalized
              ;; variable, despite it not having the `gv-expander'
              ;; property.  See `help-fns--generalized-variable'.
              (push `(function-put ',accessor 'document-generalized-variable t)
                    forms)
              (when (oddp (length desc))
                (push
                 (macroexp-warn-and-return
                  (format-message
                   "Missing value for option `%S' of slot `%s' in struct %s!"
                   (car (last desc)) slot name)
                  nil nil nil (car (last desc)))
                 forms)
                (when (and (keywordp (car defaults))
                           (not (keywordp (car desc))))
                  (let ((kw (car defaults)))
                    (push
                     (macroexp-warn-and-return
                      (format-message
                       "  I'll take `%s' to be an option rather than a default value."
                       kw)
                      nil nil nil kw)
                     forms)
                    (push kw desc)
                    (setcar defaults nil))))
              (cond
               ((eq defsym 'defun)
                (unless (plist-get desc ':read-only)
                  (push `(defun ,(gv-setter accessor) (val cl-x)
                           (setf ,access-body val))
                        forms)))
               ((plist-get desc ':read-only)
                (push `(gv-define-expander ,accessor
                         (lambda (_cl-do _cl-x)
                           (error "%s is a read-only slot" ',accessor)))
                      forms))
               (t
                ;; For normal slots, we don't need to define a setf-expander,
                ;; since gv-get can use the compiler macro to get the
                ;; same result.
                ;; (push `(gv-define-setter ,accessor (cl-val cl-x)
                ;;          ;; If cl is loaded only for compilation,
                ;;          ;; the call to cl--struct-setf-expander would
                ;;          ;; cause a warning because it may not be
                ;;          ;; defined at run time.  Suppress that warning.
                ;;          (progn
                ;;            (declare-function
                ;;             cl--struct-setf-expander "cl-macs"
                ;;             (x name accessor pred-form pos))
                ;;            (cl--struct-setf-expander
                ;;             cl-val cl-x ',name ',accessor
                ;;             ,(and pred-check `',pred-check)
                ;;             ,pos)))
                ;;       forms)
                ))
	      (if print-auto
		  (nconc print-func
			 (list `(princ ,(format " %s" slot) cl-s)
			       `(prin1 (,accessor cl-x) cl-s)))))))
	(setq pos (1+ pos))))
    (setq slots (nreverse slots)
	  defaults (nreverse defaults))
    (and copier
         (push `(defalias ',copier #'copy-sequence)
               forms))
    (if constructor
	(push (list constructor
                    (cons '&key (delq nil (copy-sequence slots))))
              constrs))
    (pcase-dolist (`(,cname ,args ,doc) constrs)
      (let* ((anames (cl--arglist-args args))
             (make (cl-mapcar (lambda (s d) (if (memq s anames) s d))
			      slots defaults))
	     (con-fun (or type #'record)))
	(push `(,cldefsym ,cname
                   (&cl-defs (nil ,@descs) ,@args)
                 ,(if (stringp doc) doc
                    ;; NB.  This will produce incorrect results in
                    ;; some cases, as our coding conventions says that
                    ;; the first line must be a full sentence.
                    ;; However, if we don't word wrap we will have
                    ;; byte-compiler warnings about overly long
                    ;; docstrings.  So we can't have a perfect result
                    ;; here, and choose to avoid the byte-compiler
                    ;; warnings.
                    (internal--format-docstring-line
                     "Constructor for objects of type `%s'." name))
                 ,@(if (cl--safe-expr-p `(progn ,@(mapcar #'cl-second descs)))
                       '((declare (side-effect-free t))))
                 (,con-fun ,@make))
              forms)))
    (if print-auto (nconc print-func (list '(princ ")" cl-s) t)))
    ;; Don't bother adding to cl-custom-print-functions since it's not used
    ;; by anything anyway!
    ;;(if print-func
    ;;    (push `(if (boundp 'cl-custom-print-functions)
    ;;               (push
    ;;                ;; The auto-generated function does not pay attention to
    ;;                ;; the depth argument cl-n.
    ;;                (lambda (cl-x cl-s ,(if print-auto '_cl-n 'cl-n))
    ;;                  (and ,pred-form ,print-func))
    ;;                cl-custom-print-functions))
    ;;          forms))
    `(progn
       (defvar ,tag-symbol)
       ,@(if (null dynbound-slotnames)
             (nreverse forms)
           `((with-suppressed-warnings ((lexical . ,dynbound-slotnames))
               ,@(nreverse forms))))
       :autoload-end
       ;; Call cl-struct-define during compilation as well, so that
       ;; a subsequent cl-defstruct in the same file can correctly include this
       ;; struct as a parent.
       (eval-and-compile
         (cl-struct-define ',name ,docstring ',include-name
                           ',(or type 'record) ,(eq named t) ',descs
                           ',tag-symbol ',tag ',print-auto))
       ,warning
       ',name)))

;;; Add cl-struct support to pcase

;;;###autoload
(pcase-defmacro cl-struct (type &rest fields)
  "Pcase patterns that match cl-struct EXPVAL of type TYPE.
Elements of FIELDS can be of the form (NAME PAT) in which case the
contents of field NAME is matched against PAT, or they can be of
the form NAME which is a shorthand for (NAME NAME)."
  (declare (debug (sexp &rest [&or (sexp pcase-PAT) sexp])))
  `(and (pred (cl-typep _ ',type))
        ,@(mapcar
           (lambda (field)
             (let* ((name (if (consp field) (car field) field))
                    (pat (if (consp field) (cadr field) field)))
               `(app ,(if (eq (cl-struct-sequence-type type) 'list)
                          `(nth ,(cl-struct-slot-offset type name))
                        `(aref _ ,(cl-struct-slot-offset type name)))
                     ,pat)))
           fields)))

(defun cl--defstruct-predicate (type)
  (let ((cons (assq (cl-struct-sequence-type type)
                    '((list . consp)
                      (vector . vectorp)
                      (nil . recordp)))))
    (if cons
        (cdr cons)
      'recordp)))

(defun cl--pcase-mutually-exclusive-p (orig pred1 pred2)
  "Extra special cases for `cl-typep' predicates."
  (let* ((x1 pred1) (x2 pred2)
         (t1
          (and (eq 'cl-typep (car-safe x1))    (setq x1 (cdr x1))
               (eq '_ (car-safe x1))           (setq x1 (cdr x1))
               (null (cdr-safe x1))            (setq x1 (car x1))
               (eq 'quote (car-safe x1))       (cadr x1)))
         (t2
          (and (eq 'cl-typep (car-safe x2))    (setq x2 (cdr x2))
               (eq '_ (car-safe x2))           (setq x2 (cdr x2))
               (null (cdr-safe x2))            (setq x2 (car x2))
               (eq 'quote (car-safe x2))       (cadr x2))))
    (or
     (and (symbolp t1) (symbolp t2)
          (let ((c1 (cl--find-class t1))
                (c2 (cl--find-class t2)))
            (and c1 c2
                 (not (or (memq t1 (cl--class-allparents c2))
                          (memq t2 (cl--class-allparents c1)))))))
     (let ((c1 (and (symbolp t1) (cl--find-class t1))))
       (and c1 (cl--struct-class-p c1)
            (funcall orig (cl--defstruct-predicate t1)
                     pred2)))
     (let ((c2 (and (symbolp t2) (cl--find-class t2))))
       (and c2 (cl--struct-class-p c2)
            (funcall orig pred1
                     (cl--defstruct-predicate t2))))
     (funcall orig pred1 pred2))))
(when (fboundp 'advice-add)           ;Not available during bootstrap.
  (advice-add 'pcase--mutually-exclusive-p
              :around #'cl--pcase-mutually-exclusive-p))


;;;###cl-autoload
(defun cl-struct-sequence-type (struct-type)
  "Return the sequence used to build STRUCT-TYPE.
STRUCT-TYPE is a symbol naming a struct type.  Return values are
either `vector', `list' or nil (and the latter indicates a
`record' struct type."
  (declare (side-effect-free t) (pure t))
  (cl--struct-class-type (cl--struct-get-class struct-type)))

(defun cl--alist-to-plist (alist)
  (let ((res '()))
    (dolist (x alist)
      (push (car x) res)
      (push (cdr x) res))
    (nreverse res)))

;;;###autoload
(defun cl-struct-slot-info (struct-type)
  "Return a list of slot names of struct STRUCT-TYPE.
Each entry is a list (SLOT-NAME . OPTS), where SLOT-NAME is a
slot name symbol and OPTS is a list of slot options given to
`cl-defstruct'.  Dummy slots that represent the struct name and
slots skipped by :initial-offset may appear in the list."
  (declare (side-effect-free t) (pure t))
  (let* ((class (cl--struct-get-class struct-type))
         (slots (cl--struct-class-slots class))
         (type (cl--struct-class-type class))
         (descs (if type () (list '(cl-tag-slot)))))
    (dotimes (i (length slots))
      (let ((slot (aref slots i)))
        (push `(,(cl--slot-descriptor-name slot)
                ,(cl--slot-descriptor-initform slot)
                ,@(if (not (eq (cl--slot-descriptor-type slot) t))
                      `(:type ,(cl--slot-descriptor-type slot)))
                ,@(cl--alist-to-plist (cl--slot-descriptor-props slot)))
              descs)))
    (nreverse descs)))

(define-error 'cl-struct-unknown-slot "struct has no slot")

;;;###cl-autoload
(defun cl-struct-slot-offset (struct-type slot-name)
  "Return the offset of slot SLOT-NAME in STRUCT-TYPE.
The returned zero-based slot index is relative to the start of
the structure data type and is adjusted for any structure name
and :initial-offset slots.  Signal error if struct STRUCT-TYPE
does not contain SLOT-NAME."
  (declare (side-effect-free t) (pure t))
  (or (gethash slot-name
               (cl--class-index-table (cl--struct-get-class struct-type)))
      (signal 'cl-struct-unknown-slot (list struct-type slot-name))))

(defvar byte-compile-function-environment)

(defun cl--macroexp-fboundp (sym)
  "Return non-nil if SYM will be bound when we run the code.
Of course, we really can't know that for sure, so it's just a heuristic."
  (or (fboundp sym)
      (and (macroexp-compiling-p)
           (or (cdr (assq sym byte-compile-function-environment))
               (cdr (assq sym macroexpand-all-environment))))))

;;;###autoload
(define-inline cl-typep (val type)
  "Return t if VAL is of type TYPE, nil otherwise."
  (inline-letevals (val)
    (pcase (inline-const-val type)
      ((and (or (and type (pred symbolp)) `(,type))
            (guard (get type 'cl-deftype-satisfies)))
       (inline-quote (funcall #',(get type 'cl-deftype-satisfies) ,val)))
      ((and `(,name . ,args) (guard (get name 'cl-deftype-handler)))
       (inline-quote
        (cl-typep ,val ',(apply (get name 'cl-deftype-handler) args))))
      ;; FIXME: Move this to a `cl-deftype'.  The problem being that these
      ;; types are hybrid "built-in and derived".
      (`(,(and name (or 'integer 'float 'number))
         . ,(or `(,min ,max) pcase--dontcare))
       (inline-quote
        (and (cl-typep ,val ',name)
             ,(if (memq min '(* nil)) t
                (if (consp min)
                    (inline-quote (> ,val ',(car min)))
                  (inline-quote (>= ,val ',min))))
             ,(if (memq max '(* nil)) t
                (if (consp max)
                    (inline-quote (< ,val ',(car max)))
                  (inline-quote (<= ,val ',max)))))))
      (`(not ,type) (inline-quote (not (cl-typep ,val ',type))))
      (`(,(and name (or 'and 'or)) . ,types)
       (cond
        ((null types) (inline-quote ',(eq name 'and)))
        ((null (cdr types))
         (inline-quote (cl-typep ,val ',(car types))))
        (t
         (let ((head (car types))
               (rest `(,name . ,(cdr types))))
           (cond
            ((eq name 'and)
             (inline-quote (and (cl-typep ,val ',head)
                             (cl-typep ,val ',rest))))
            (t
             (inline-quote (or (cl-typep ,val ',head)
                            (cl-typep ,val ',rest)))))))))
      (`(eql ,v)          (inline-quote (and (eql ,val ',v) t)))
      (`(member . ,args)  (inline-quote (and (memql ,val ',args) t)))
      (`(satisfies ,pred) (inline-quote (funcall #',pred ,val)))
      ((and (pred symbolp) type (guard (get type 'cl-deftype-handler)))
       (inline-quote
        (cl-typep ,val ',(funcall (get type 'cl-deftype-handler)))))
      ((and (or 'nil 't) type) (inline-quote ',type))
      ((and (pred symbolp) type)
       (macroexp-warn-and-return
        (format-message "Unknown type: %S" type)
        (let* ((name (symbol-name type))
               (namep (intern (concat name "p"))))
          (cond
           ((cl--macroexp-fboundp namep) (inline-quote (funcall #',namep ,val)))
           ((cl--macroexp-fboundp
             (setq namep (intern (concat name "-p"))))
            (inline-quote (funcall #',namep ,val)))
           ((cl--macroexp-fboundp type) (inline-quote (funcall #',type ,val)))
           (t (error "Unknown type %S" type))))
        nil nil type))
      (type (error "Bad type spec: %S" type)))))


;;;###autoload
(defmacro cl-check-type (form type &optional string)
  "Verify that FORM is of type TYPE; signal an error if not.
STRING is an optional description of the desired type.

Hint: To check the type of an object, use `cl-type-of'.
To define new types, see `cl-deftype'."
  (declare (debug (place cl-type-spec &optional stringp)))
  (and (or (not (macroexp-compiling-p))
	   (< cl--optimize-speed 3) (= cl--optimize-safety 3))
       (macroexp-let2 macroexp-copyable-p temp form
         `(progn (or (cl-typep ,temp ',type)
                     (signal 'wrong-type-argument
                             (list ,(or string `',(if (eq 'satisfies
                                                          (car-safe type))
                                                      (cadr type) type))
                                   ,temp ',form)))
                 nil))))

;;;###autoload
(defmacro cl-assert (form &optional show-args string &rest args)
  ;; FIXME: This is actually not compatible with Common-Lisp's `assert'.
  "Verify that FORM returns non-nil; signal an error if not.
Second arg SHOW-ARGS means to include arguments of FORM in message.
Other args STRING and ARGS... are arguments to be passed to `error'.
They are not evaluated unless the assertion fails.  If STRING is
omitted, a default message listing FORM itself is used."
  (declare (debug (form &rest form)))
  (and (or (not (macroexp-compiling-p))
	   (< cl--optimize-speed 3) (= cl--optimize-safety 3))
       (let ((sargs (and show-args
                         (delq nil (mapcar (lambda (x)
                                             (unless (macroexp-const-p x)
                                               x))
                                           (cdr-safe form))))))
	 `(progn
            (or ,form
                (cl--assertion-failed
                 ',form ,@(if (or string sargs args)
                              `(,string (list ,@sargs) (list ,@args)))))
            nil))))

;;; Compiler macros.

;;;###autoload
(defmacro cl-define-compiler-macro (func args &rest body)
  "Define a compiler-only macro.
This is like `defmacro', but macro expansion occurs only if the call to
FUNC is compiled (i.e., not interpreted).  Compiler macros should be used
for optimizing the way calls to FUNC are compiled; the form returned by
BODY should do the same thing as a call to the normal function called
FUNC, though possibly more efficiently.  Note that, like regular macros,
compiler macros are expanded repeatedly until no further expansions are
possible.  Unlike regular macros, BODY can decide to \"punt\" and leave the
original function call alone by declaring an initial `&whole foo' parameter
and then returning foo."
  ;; Like `cl-defmacro', but with the `&whole' special case.
  (declare (debug (&define [&name symbolp "@cl-compiler-macro"]
                           cl-macro-list
                           cl-declarations-or-string def-body))
           (indent 2))
  (let ((p args) (res nil))
    (while (consp p) (push (pop p) res))
    (setq args (nconc (nreverse res) (and p (list '&rest p)))))
  ;; FIXME: The code in bytecomp mishandles top-level expressions that define
  ;; uninterned functions.  E.g. it would generate code like:
  ;;    (defalias '#1=#:foo--cmacro #[514 ...])
  ;;    (put 'foo 'compiler-macro '#:foo--cmacro)
  ;; So we circumvent this by using an interned name.
  (let ((fname (intern (concat (symbol-name func) "--cmacro"))))
    `(eval-and-compile
       ;; Name the compiler-macro function, so that `symbol-file' can find it.
       (cl-defun ,fname ,(if (memq '&whole args) (delq '&whole args)
                           (cons '_cl-whole-arg args))
         ,@body)
       (define-symbol-prop ',func 'compiler-macro #',fname))))

;;;###autoload
(defun cl-compiler-macroexpand (form)
  "Like `macroexpand', but for compiler macros.
Expands FORM repeatedly until no further expansion is possible.
Returns FORM unchanged if it has no compiler macro, or if it has a
macro that returns its `&whole' argument."
  (while
      (let ((func (car-safe form)) (handler nil))
	(while (and (symbolp func)
		    (not (setq handler (get func 'compiler-macro)))
		    (fboundp func)
		    (or (not (autoloadp (symbol-function func)))
			(autoload-do-load (symbol-function func) func)))
	  (setq func (symbol-function func)))
	(and handler
	     (not (eq form (setq form (apply handler form (cdr form))))))))
  form)

;; Optimize away unused block-wrappers.

(defvar cl--active-block-names nil)

(cl-define-compiler-macro cl--block-wrapper (form)
  (pcase form
    (`(let ((,var . ,val)) (catch ,var . ,body))
     (let* ((cl-entry (cons var nil))
            (cl--active-block-names (cons cl-entry cl--active-block-names))
            (cl-body (macroexpand-all      ;Performs compiler-macro expansions.
                      (macroexp-progn body)
                      macroexpand-all-environment)))
       ;; FIXME: To avoid re-applying macroexpand-all, we'd like to be able
       ;; to indicate that this return value is already fully expanded.
       (if (cdr cl-entry)
           `(let ((,var . ,val)) (catch ,var ,@(macroexp-unprogn cl-body)))
           cl-body)))
    ;; `form' was somehow mangled, god knows what happened, let's not touch it.
    (_ form)))

(cl-define-compiler-macro cl--block-throw (cl-tag cl-value)
  (let ((cl-found (and (symbolp cl-tag) (assq cl-tag cl--active-block-names))))
    (if cl-found (setcdr cl-found t)))
  `(throw ,cl-tag ,cl-value))

;; Compile-time optimizations for some functions defined in this package.

(defun cl--compiler-macro-member (form a list &rest keys)
  (let ((test (and (= (length keys) 2) (eq (car keys) :test)
		   (cl--const-expr-val (nth 1 keys)))))
    (cond ((eq test 'eq) `(memq ,a ,list))
	  ((eq test 'equal) `(member ,a ,list))
	  ((or (null keys) (eq test 'eql)) `(memql ,a ,list))
	  (t form))))

(defun cl--compiler-macro-assoc (form a list &rest keys)
  (let ((test (and (= (length keys) 2) (eq (car keys) :test)
		   (cl--const-expr-val (nth 1 keys)))))
    (cond ((eq test 'eq) `(assq ,a ,list))
	  ((eq test 'equal) `(assoc ,a ,list))
	  ((and (macroexp-const-p a) (or (null keys) (eq test 'eql)))
	   (if (floatp (cl--const-expr-val a))
	       `(assoc ,a ,list) `(assq ,a ,list)))
	  (t form))))

;;;###autoload
(defun cl--compiler-macro-adjoin (form a list &rest keys)
  (if (memq :key keys) form
    (macroexp-let2* macroexp-copyable-p ((va a) (vlist list))
      `(if (cl-member ,va ,vlist ,@keys) ,vlist (cons ,va ,vlist)))))

(defun cl--compiler-macro-get (_form sym prop &optional def)
  (if def
      `(cl-getf (symbol-plist ,sym) ,prop ,def)
    `(get ,sym ,prop)))

;;; Types and assertions.

;;;###autoload
(defmacro cl-deftype (name arglist &rest body)
  "Define NAME as a new, so-called derived type.
The type NAME can then be used in `cl-typecase', `cl-check-type',
etc., and to some extent, as method specializer.

ARGLIST is a Common Lisp argument list of the sort accepted by
`cl-defmacro'.  BODY forms should return a type specifier that is equivalent
to the type (see the Info node `(cl)Type Predicates').

If there is a `declare' form in BODY, the spec (parents . PARENTS)
can specify a list of types NAME is a subtype of.
The list of PARENTS types determines the order of methods invocation,
and missing PARENTS may cause incorrect ordering of methods, while
extraneous PARENTS may cause use of extraneous methods.

If PARENTS is non-nil, ARGLIST must be nil."
  (declare (debug cl-defmacro) (doc-string 3) (indent 2))
  (pcase-let*
      ((`(,decls . ,forms) (macroexp-parse-body body))
       (declares (assq 'declare decls))
       (parent-decl (assq 'parents (cdr declares)))
       (parents (cdr parent-decl)))
    (when parent-decl
      ;; "Consume" the `parents' declaration.
      (cl-callf (lambda (x) (delq parent-decl x)) (cdr declares))
      (when (equal declares '(declare))
        (cl-callf (lambda (x) (delq declares x)) decls)))
    (and parents arglist
         (error "Parents specified, but arglist not empty"))
    (let* ((expander
            `(cl-function (lambda (&cl-defs ('*) ,@arglist) ,@decls ,@forms)))
           ;; FIXME: Pass a better lexical context.
           (specifier (ignore-errors (funcall (eval expander t))))
           (predicate
            (pcase specifier
              (`(satisfies ,f) `#',f)
              ('nil nil)
              (type `(lambda (x) (ignore x) (cl-typep x ',type))))))
      `(eval-and-compile
         (cl--define-derived-type
          ',name ,expander ,predicate ',parents)))))

;; This one is redundant, but we keep it to silence a
;; warning during the early bootstrap when `cl-seq.el' gets
;; loaded before `cl-preloaded.el' is defined.
(put 'list 'cl-deftype-satisfies #'listp)

;; Thanks to `eval-and-compile', `cl--define-derived-type' is needed
;; both at compile-time and at runtime, so we need to double-check.
(static-if (not (fboundp 'cl--define-derived-type)) nil
  (when (fboundp 'cl--define-derived-type)
    (cl-deftype natnum () (declare (parents integer)) '(satisfies natnump))
    (cl-deftype character () (declare (parents fixnum natnum))
                '(and fixnum natnum))
    (cl-deftype base-char () (declare (parents character))
                '(satisfies characterp))
    (cl-deftype extended-char () (declare (parents character))
                '(and character (not base-char)))
    (cl-deftype keyword () (declare (parents symbol)) '(satisfies keywordp))
    (cl-deftype command ()
      ;; FIXME: Can't use `function' as parent because of arrays as
      ;; keyboard macros, which are redundant since `kmacro.el'!!
      ;;(declare (parents function))
      '(satisfies commandp))

    (eval-when-compile
      (defmacro cl--defnumtype (type base)
        `(cl-deftype ,type (&optional min max)
           (list 'and ',base
                 (if (memq min '(* nil)) t
                   (if (consp min)
                       `(satisfies . ,(lambda (val) (> val (car min))))
                     `(satisfies . ,(lambda (val) (>= val min)))))
                 (if (memq max '(* nil)) t
                   (if (consp max)
                       `(satisfies . ,(lambda (val) (< val (car max))))
                     `(satisfies . ,(lambda (val) (<= val max)))))))))
    ;;(cl--defnumtype integer ??)
    ;;(cl--defnumtype float ??)
    ;;(cl--defnumtype number ??)
    (cl--defnumtype real number)))

;; Additional functions that we can now define because we've defined
;; `cl-defsubst' and `cl-typep'.

(define-inline cl-struct-slot-value (struct-type slot-name inst)
  "Return the value of slot SLOT-NAME in INST of STRUCT-TYPE.
STRUCT-TYPE and SLOT-NAME are symbols.  INST is a structure instance."
  (declare (side-effect-free t))
  (inline-letevals (struct-type slot-name inst)
    (inline-quote
     (progn
       (unless (cl-typep ,inst ,struct-type)
         (signal 'wrong-type-argument (list ,struct-type ,inst)))
       ;; We could use `elt', but since the byte compiler will resolve the
       ;; branch below at compile time, it's more efficient to use the
       ;; type-specific accessor.
       (if (eq (cl-struct-sequence-type ,struct-type) 'list)
           (nth (cl-struct-slot-offset ,struct-type ,slot-name) ,inst)
         (aref ,inst (cl-struct-slot-offset ,struct-type ,slot-name)))))))

(make-obsolete-variable 'cl-macs-load-hook
                        "use `with-eval-after-load' instead." "28.1")
(run-hooks 'cl-macs-load-hook)

;;; Pcase type pattern.

;;;###autoload
(pcase-defmacro cl-type (type)
  "Pcase pattern that matches objects of TYPE.
TYPE is a type descriptor as accepted by `cl-typep', which see."
  `(pred (cl-typep _ ',type)))


;; Local variables:
;; generated-autoload-file: "cl-loaddefs.el"
;; End:

(provide 'cl-macs)

;;; cl-macs.el ends here
