;; -*- lexical-binding: t; symbol-packages: t -*-
;;; tm.el --- A terminal menu implementation in Lisp

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

;;; Todo

;; - Customization, including Unicode use.

;;; Commentary:

;; This is a menu implementation for ttys. It uses two changes in the C
;; code:
;;
;; - a new hook `x-popup-menu-function'
;; - hiding the cursor in frames with (cursor-type . nil) parameter.
;;
;; The code uses nothing else of the C code, including parsing menu
;; keymaps. This would makes it possible to delete portions of the C
;; code in the future.
;;
;; The behavior of the menu is as close as possible like in the macOS
;; GUI, which is of course because I am a macOS user.

;; Some diagrams, rough overview. Add symbol prefixes in your
;; imagination.
;;
;; CLOS class diagram, inheritance.
;;
;;                  +---------------+
;;        +-------->|    element    |
;;        |         +---------------+
;;        |                 ^
;; +-------------+          |
;; |    pane     |          |
;; +-------------+          |
;;                          |
;;                  +---------------+
;;           +----->|      item     |<----+
;;           |      +---------------+     |
;;           |                            |
;; +-------------------+         +-----------------+
;; |     separator     |    +---->      button     |<--+
;; +-------------------+    |    +-----------------+   |
;;                          |                          |
;;                +----------------+        +------------------+
;;                |     radio      |        |     checkbox     |
;;                +----------------+        +------------------+
;;
;;
;; More class diagram:
;;
;;   +-parent-+
;;   | child  |  +-----selected--------+
;;   v        v  |                     v
;; +---------------+             +---------------+
;; |     pane      |<---pane-----|    item       |
;; +---------------+             |               |
;;  |   ^   |    |              *|               |
;;  |buffer |    +----items----->|               |----+
;;  |   |   |                    |               | draw-start
;;  |   |   +---invoking-item--->|               | draw-end
;;  |   |                        |               |    |
;;  |   |                        +---------------+    |
;;  |pane-drawn     +---------------------------------+
;;  |   |           v
;;  |   |      +--------+   +--------+      +-------+
;;  |   +----->| buffer |<--| window |<---->| frame |
;;  |          +--------+   +--------+      +-------+
;; frame            ^                           | ^
;;  |               +------------------buffer---+ |
;;  +---------------------------------------------+
;;
;;
;; Event loop:
;;
;; +-------+         +-------+         +---------+
;; | loop  |         | loop-1|         | command |
;; |       |         |       |           +after
;; +-------+         +-------+         +---------+
;;     |                 |                  |
;;     |---------------->|----------------->|
;;     |                 |                  |
;;     |                 |<--throw close----|
;;     |                 |------loop------->|
;;     |                 |                  |
;;     |<---------throw to-top-level--------|
;;     |                 |                  |
;;     |                 |<-----recursive---|
;;     |                 |----------------->|
;;     |                 |                  |
;;
;;

;;; Code:

(require 'cl-lib)
;; Required for with emacs -Q, adding autoloads doesn't cut it.
(require 'eieio)

(defgroup tm nil
  "Group for terminal menus implemented in Lisp."
  :group 'menu)

(defface tm-face
  '((((class color) (min-colors 1000000))
     :background "#252525" :foreground "#d0d0d0")
    (((class color) (min-colors 256))
     :background "#212121" :foreground "while")
    (t :background "white"))
  "Face for TTY menus."
  :group 'tm
  :version "31.1")

(defface tm-face-selected
  '((((class color) (min-colors 1000000) (background dark))
     :inherit tm-face :background "#2f3f4f")
    (((class color) (min-colors 256) (background dark))
     :inherit tm-face :background "#00007f")
    (t :inherit tm-face :foreground "black" :background "white"))
  "Face for selected menu items."
  :group 'tm
  :version "31.1")

(defface tm-face-selected-inactive
  '((t :inherit tm-face :background "grey20"))
  "Face for inactive selected menu items."
  :group 'tm
  :version "31.1")

(defface tm-face-disabled
  '((((class color) (min-colors 1000000) (background dark))
     :inherit tm-face :foreground "grey5")
    (((class color) (min-colors 256) (background dark))
     :inherit tm-face :foreground "#5f5f5f")
    (t :inherit tm-face :foreground "gray5"))
  "Face for disabled menu items."
  :group 'tm
  :version "31.1")

(defclass tm-element () ())

(defclass tm-pane (tm-element)
  ((items :initarg :items :initform nil :type t)
   (buffer :initarg :buffer :type buffer)
   (overlay :initarg nil :type overlay)
   (frame :initarg :frame :type frame)
   (layout :type list)
   (keymap :initarg :keymap)
   (invoking-item :initarg :invoking-item :type (or null tm-item))
   (parent-pane :initform nil :type (or null tm-pane))
   (child-pane :initform nil :type (or null tm-pane))
   (selected-item :initform nil :type (or null tm-item))))

;; Type t in many places because even the name can be a form that needs
;; to be evaluated to get the name (Redo is an example in the context
;; menu).
(defclass tm-item (tm-element)
  ((name :initarg :name :type t)
   (enable :initarg :enable :initform t :type t)
   (help :initarg :help :initform nil :type t)
   (visible :initarg :visible :initform t :type t)
   (key-sequence :initarg :key-sequence :initform nil :type t)
   (keys :initarg :keys :initform nil :type t)
   (filter :initarg :filter :initform nil :type t)
   (button :initarg :button :initform nil :type t)
   (binding :initarg :binding :initform nil :type t)
   (key-code :initarg :key-code :initform nil :type t)
   (pane :initarg :pane :type tm-pane)
   (next-item :initform nil :type (or null tm-item))
   (prev-item :initform nil :type (or null tm-item))
   (draw-start :initform nil :type (or null number))
   (draw-end :initform nil :type (or null number))))

(defclass tm-button (tm-item) ())
(defclass tm-radio (tm-button) ())
(defclass tm-checkbox (tm-button) ())
(defclass tm-separator (tm-item)
  ((sep :initform "-" :type string :reader tm-sep)))

(defcustom tm-sub-menu-offset-x -3
  "Open sub-menus this delta to the left or right."
  :type 'integer)

(defcustom tm-sub-menu-offset-y -1
  "Open sub-menus this delta up or down."
  :type 'integer)

(defvar tm--updating-buffer nil
  "Dynamically bound to the current buffer when a menu is invoked.")

(defvar tm--from-menu-bar nil
  "Non-nil means menu is invoked for a menu-bar.
Bound by an around advice for `popup-menu' if called for a menu-bar
menu.  If non-nil, it is a cons (X . Y) of the menu-item.")

(defun tm--eval (form)
  "Evaluate FORM in the context of the menu.
The context consists of the buffer that was current when the menu
was invoked.  This buffer must be current when evaluating various things
in the menu because of local variables."
  (with-current-buffer tm--updating-buffer
    (eval form)))

(defun tm--get-separator-string (name)
  "Determine which separator char to use.
NAME is a separator label, which which is a kind of separator type.
Value is a string of length 1 for the separator char."
  (cl-multiple-value-bind (ch disp)
      (pcase name
	("--space" (cl-values ?\s nil))
	("--double-line" (cl-values ?= 'box-double-horizontal))
	(_ (cl-values ?- 'box-horizontal)))
    (let ((sep ch)
	  (slot (when disp
		  (display-table-slot standard-display-table disp))))
      (when (and slot (characterp slot))
	(setq sep slot))
      (make-string 1 sep))))

(cl-defmethod initialize-instance
  :after ((item tm-separator) &rest)
  "Constructor for separator ITEM.
Computes the actual separator char to use for a separator."
  (with-slots (name sep enable) item
    (setf enable nil)
    (setf sep (tm--get-separator-string name))))

(defun tm--resolve-keymap (obj)
  (let ((def (indirect-function obj)))
    (pcase def
      ('nil nil)
      (`(keymap . ,_) def)
      ((and (guard (symbolp obj))
            `(autoload ,_ ,_ ,_ ,_ keymap . ,_))
       (autoload-do-load def obj nil)
       (tm--resolve-keymap obj))
      (_ nil))))

(cl-defmethod initialize-instance
  :after ((item tm-item) &rest)
  "Constructor for menu-item ITEM.
If a menu-item's binding is a keymap with 0 elements, disable it."
  (with-slots (binding filter enable) item
    (when filter
      (setf binding (tm--eval `(,filter (quote ,binding)))))

    ;; If binding is a symbol whose function definition is an autoload
    ;; keymap, resolve that.
    (when (symbolp binding)
      (when-let* ((keymap (tm--resolve-keymap binding))
                  ((keymapp keymap)))
        (setf binding keymap)))

    ;; We cannot do the following because a symbol may have a
    ;; a function definition of the form (autoload ...), which is
    ;; itself not callable, and `call-interactively' barfs.
    ;; (setf binding (indirect-function binding))

    (when (and (keymapp binding)
               (zerop (cl-loop for b being the key-codes of binding
                               count b)))
      (setf enable nil))))

(cl-defmethod initialize-instance
  :after ((pane tm-pane) &rest)
  "Constructor for menu pane PANE."
  (with-slots (invoking-item parent-pane buffer overlay) pane
    (setf overlay (make-overlay 1 1 buffer))
    (when invoking-item
      (let ((invoking-pane (slot-value invoking-item 'pane)))
        (setf (slot-value invoking-pane 'child-pane) pane)
        (setf parent-pane invoking-pane)))))

;; Various format strings for the elements of a menu-item:
;;
;;    | left border | button | name | key | right border |
;;
;; These format strings specify left or right alignment of elements and
;; a minimum width.
(defcustom tm-left-border-format "%1s"
  "Format string for the left border of menus."
  :type 'string)
(defcustom tm-right-border-format "%1s"
  "Format string for the right border of menus."
  :type 'string)
(defcustom tm-button-format "%-2s"
  "Format string for the button part of menu-items."
  :type 'string)
(defcustom tm-key-format "%10s"
  "Format string for the key part of menu-items."
  :type 'string)
(defcustom tm-name-format "%-20s"
  "Format string for the name part of menu-items."
  :type 'string)

(defcustom tm-triangle "▶"
  "What to display as a sub-menu indicator."
  :type 'string)
(defcustom tm-radio-on "●"
  "What to display for a radio button in on state."
  :type 'string)
(defcustom tm-radio-off "◯"
  "What to display for a radio button in off state."
  :type 'string)
(defcustom tm-checkbox-on "✔"
  "What to display for a checkbox in on state."
  :type 'string)
(defcustom tm-checkbox-off "□"
  "What to display for a checkbox in off state."
  :type 'string)

(defun tm--call-interactively (fn)
  "Call FN interactively in `tm--updating-buffer'."
  (with-current-buffer tm--updating-buffer
    (call-interactively fn)))

(defun tm--visible-p (item)
  "Value is non-nil if ITEM is visible."
  (tm--eval (slot-value item 'visible)))

(defun tm--enabled-p (item)
  "Value is non-nil if ITEM is enabled."
  (tm--eval (slot-value item 'enable)))

(defun tm--selectable-p (item)
  "Value is non-nil if ITEM is selectable by the user."
  (and (tm--visible-p item)
       (tm--enabled-p item)))

(defun tm--name (item)
  "Value is the name of ITEM."
  (tm--eval (slot-value item 'name)))

(defun tm-button-selected-p (item)
  "Value is non-nil if button ITEM is selected (on)."
  (with-slots (button) item
    (cl-destructuring-bind (_ . form) button
      (tm--eval form))))

(defun tm-ninsert (n x)
  "Insert N times X at point."
  (cl-loop repeat n do (insert x)))

(cl-defgeneric tm-button-string (item)
  "Value is a string for the button part of ITEM."
  ( :method ((_ tm-item))
    "")
  ( :method ((r tm-radio))
    (format tm-button-format
            (if (tm-button-selected-p r)
		tm-radio-on
	      tm-radio-off)))
  ( :method ((c tm-checkbox))
    (format tm-button-format
            (if (tm-button-selected-p c)
		tm-checkbox-on
	      tm-checkbox-off)))
  ( :method ((_ tm-separator))
    ""))

(cl-defgeneric tm--name-string (item)
  "Value is a string for the name part of ITEM."
  ( :method ((item tm-item))
    (format tm-name-format (tm--name item)))
  ( :method ((_ tm-separator))
    ""))

(defun tm--key-description (binding)
  "Get the key description of the binding of ITEM."
  (with-current-buffer tm--updating-buffer
    (key-description
     (where-is-internal binding nil t))))

(defun tm--key-binding (key)
  (with-current-buffer tm--updating-buffer
    (key-binding key)))

(defun tm--funcall (fn)
  (with-current-buffer tm--updating-buffer
    (funcall fn)))

(cl-defgeneric tm--key-string (item)
  "Value is a string for the key part of ITEM."
  ( :method ((item tm-item))
    (format
     tm-key-format
     (with-slots (name binding keys key-sequence) item
       (cond
        (key-sequence
         (tm--key-description (tm--key-binding key-sequence)))
        (keys
         ;; Undocumented: :keys X can specify a function
         ;; which is called with no arguments. Magit uses
         ;; that.
         (let ((s (cond ((functionp keys)
                         (tm--funcall keys))
                        ((stringp keys) keys)
                        (t "??"))))
           (if (stringp s)
               (substitute-command-keys s)
             "??")))
        ((null binding) "")
	((keymapp binding) tm-triangle)
        (t (tm--key-description binding))))))
  ( :method ((_ tm-separator)) ""))

;; Menu items look like this:
;;
;; +--------+--------+-------------+-----+--------+
;; | left   | button | name        | key | right  |
;; | border |        |             |     | border |
;; +--------+--------+-------------+-----+--------+
;;
;; Left and right border consist of spaces. BUTTON is either empty or
;; display a button in on or off state for radio buttons or
;; checkboxes. NAME is the string to display for the menu item. KEY is
;; the key combination to display.
;;
;; All these elements can be customized with format strings.

(cl-defgeneric tm--draw-button (item pane)
  "Draw the button part of ITEM on PANE."
  ( :method ((_item tm-item) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (_ button-width _ _ _) layout
	(tm-ninsert button-width ?\s))))
  ( :method ((item tm-button) _pane)
    (insert (tm-button-string item)))
  ( :method ((item tm-separator) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (_ button-width _ _ _) layout
	(tm-ninsert button-width (tm-sep item))))))

(cl-defgeneric tm--draw-name (item pane)
  "Draw the name part of ITEM on PANE."
  ( :method ((item tm-item) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (left-border button name-width _ _) layout
	(with-slots (name) item
	  (insert (tm--name-string item))
	  (indent-to (+ left-border button name-width))))))
  ( :method ((item tm-separator) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (_ _ name-width _ _) layout
	(tm-ninsert name-width (tm-sep item))))))

(cl-defgeneric tm--draw-key (item pane)
  "Draw the key part of ITEM on PANE."
  ( :method ((item tm-item) pane)
    (cl-flet ((right-aligned-p (fmt)
                (not (string-match-p "%-" fmt))))
      (with-slots (layout) pane
        (cl-destructuring-bind (left-border button name-width key-width _)
            layout
          (let ((key (tm--key-string item)))
            (when (and (< (length key) key-width)
                       (right-aligned-p tm-key-format))
              (tm-ninsert (- key-width (length key)) " "))
	    (insert key))
	  (indent-to (+ left-border button name-width key-width))))))
  ( :method ((item tm-separator) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (_ _ _ key-width _) layout
	(tm-ninsert key-width (tm-sep item))))))

(cl-defgeneric tm--draw-finish (item pane)
  "Finish drawing ITEM on PANE."
  ( :method ((item tm-item) _)
    (let* ((enabled (tm--enabled-p item))
	   (face (if enabled 'tm-face
                   'tm-face-disabled)))
      (put-text-property (pos-bol) (pos-eol) 'face face))
    (when-let* ((help (slot-value item 'help)))
      (put-text-property (pos-bol) (pos-eol) 'help-echo help)))
  ( :method ((_item tm-separator) _)
    (put-text-property (pos-bol) (pos-eol) 'face 'tm-face)))

(cl-defgeneric tm--layout (pane)
  "Compute the layout of PANE for drawing items on it.
This sets the `layout' of PANE to a list (LEFT-BORDER BUTTON NAME KEY
RIGHT-BORDER), all elements giving the widths to use for the
corresponding columns of a menu item."
  ( :method ((pane tm-pane))
    (with-slots (items layout) pane
      (cl-loop
       with left-border
       = (string-width (format tm-left-border-format ""))
       with right-border
       = (string-width (format tm-right-border-format ""))
       for i in items
       maximize (string-width (tm-button-string i)) into button
       maximize (string-width (tm--name-string i)) into name
       maximize (string-width (tm--key-string i)) into key
       finally (setq layout `(,left-border ,button ,name ,key
                                           ,right-border))))))

(cl-defgeneric tm--frame (thing)
  "Value is the frame associated with THING."
  ( :method ((pane tm-pane))
    (slot-value pane 'frame))
  ( :method ((item tm-item))
    (tm--frame (slot-value item 'pane))))

;; When redrawing a pane, we try to arrange things so that the selection
;; is retained. At least theoretically, it can happen that in the
;; redrawn menu the old selection is no longer selectable. This function
;; finds an alternative selection in this case.
;;
;; Should be removed.
(defun tm--try-place-point (selectable old-line)
  (goto-char (point-min))
  (if (nth old-line selectable)
      (forward-line old-line)
    (let ((next (cl-loop for i from (1+ old-line)
			 to (1- (length selectable))
			 when (nth i selectable) return i))
	  (prev (cl-loop for i downfrom (1- old-line) to 0
			 when (nth i selectable) return i)))
      (cond (next (forward-line next))
	    (prev (forward-line prev))))))

(cl-defgeneric tm--draw (item pane)
  "Draw ITEM on PANE."
  ( :method ((pane tm-pane) line)
    (with-slots (buffer items) pane
      (with-current-buffer buffer
	(let ((old-line (or line (1- (line-number-at-pos)))))
	  (erase-buffer)
	  (tm--layout pane)
	  (let ((selectable
		 (cl-loop for i in items
			  when (tm--visible-p i)
			  do (tm--draw i pane)
			  and collect (tm--selectable-p i))))
	    (tm--try-place-point selectable old-line))))))
  ( :method :around ((item tm-item) pane)
    (with-slots (draw-start draw-end) item
      (setf draw-start (point))
      (insert (format tm-left-border-format ""))
      (cl-call-next-method)
      (insert (format tm-right-border-format ""))
      (tm--draw-finish item pane)
      (insert ?\n)
      (setf draw-end (point))
      ;; For move movement
      (put-text-property draw-start draw-end 'tm-item item)))
  ( :method ((item tm-item) pane)
    (tm--draw-button item pane)
    (tm--draw-name item pane)
    (tm--draw-key item pane)))

(defvar-local tm--pane-drawn nil
  "The tm-pane drawn in a buffer.")

(cl-defgeneric tm--add (pane item)
  "Add ITEM to PANE."
  ( :method ((pane tm-pane) (item tm-item))
    (with-slots (items) pane
      (let ((last (last items)))
        (when last
          (setf (slot-value (car last) 'next-item) item))
        (setf (slot-value item 'prev-item) (car last))
        (if last
            (setf (cdr last) (list item))
          (setf items (list item)))))))

(defun tm--root-pane (pane)
  "Find the root pane of PANE."
  (while (slot-value pane 'parent-pane)
    (setq pane (slot-value pane 'parent-pane)))
  pane)

(defun tm--command-p (cmd)
  (or (commandp cmd)
      (and-let* ((def (indirect-function cmd))
                 ((autoloadp def))))))

(defun tm--set-overlay-face (pane)
  (cl-loop
   for p = pane then (slot-value p 'parent-pane)
   while p do
   (with-slots (overlay) p
     (overlay-put overlay 'face
                  (if (eq p tm--pane-drawn)
                      'tm-face-selected
                    'tm-face-selected-inactive)))))

(cl-defgeneric tm--select (item how)
  "Select ITEM on its pane."
  ( :method ((item tm-item) _how)
    (tm--select (slot-value item 'pane) item))
  ( :method ((pane tm-pane) (item tm-item))
    (setf (slot-value pane 'selected-item) item)
    (with-slots (overlay) pane
      (tm--set-overlay-face pane)
      (with-slots (draw-start draw-end) item
        (move-overlay overlay draw-start draw-end)))))

(cl-defgeneric tm--act (item how)
  "Perform the action associated with ITEM."
  ( :method ((item tm-item) how)
    (when-let* ((enabled (tm--enabled-p item)))
      (with-slots (binding) item
        (if (keymapp binding)
            (throw 'tm-leave (cons item how))
          (throw 'tm-to-top-level (cons item how))))))
  ( :method ((_item tm-separator) _))
  ( :method ((item tm-button) how)
    (with-slots (binding) item
      (when (tm--command-p binding)
	(tm--call-interactively binding))
      (tm--draw tm--pane-drawn nil)
      (tm--select item how))))

(cl-defgeneric tm--delete (thing)
  "Delete THING."
  ( :method ((pane tm-pane))
    (with-slots (buffer frame parent-pane child-pane) pane
      (when child-pane
        (tm--delete child-pane))
      (kill-buffer buffer)
      (delete-frame frame)
      (when parent-pane
        (setf (slot-value parent-pane 'child-pane) nil))))
  ( :method ((frame frame))
    (when-let* ((buffer (frame-parameter frame 'tm-buffer))
                (pane (with-current-buffer buffer tm--pane-drawn)))
      (tm--delete pane))))

(defun tm--binding-type (item)
  "Determine what kind of binding ITEM has.
Value is nil if ITEM has no binding.  Value is `command' if the ITEM has
a command as binding. It is `keymap' is the item's binding is a keymap. "
  (with-slots (binding) item
    (cond ((null binding) nil)
          ((tm--command-p binding) 'command)
          ((keymapp binding) 'keymap)
          (t (error "unknown binding %S" binding)))))

(defun tm--make-element (pane code item)
  "Construct a new menu element and add it to PANE.
PANE is the pane the menu-element is constructed for.  CODE and ITEM are
key-code and menu-item definition from a keymap.  Value is the menu
element constructed."
  (cl-labels ((separator? (name)
                (let ((name (tm--eval name)))
                  (and (stringp name)
                       (string-prefix-p "--" name))))
	      (button? (props)
                (plist-get props :button))
	      (radio? (props)
                (eq (car (button? props)) :radio))
	      (toggle? (props)
                (eq (car (button? props)) :toggle))
              (make (class props)
                (tm--add pane (apply #'make-instance class
                                    (cl-list*  :pane pane
                                               :key-code code props)))))
    (pcase-exhaustive item
      ;; NIL -> ignore
      ('nil)

      ;; Code = remap with item for example (keymap (FROM . TO))
      ((guard (eq code 'remap)))

      ;; (menu-item SEPARATOR-NAME ...)
      (`(menu-item ,(and (pred separator?) name) ,_ . ,props)
      (make 'tm-separator (cl-list* :name name props)))

     ;; (menu-item NAME)
     (`(menu-item ,name)
      (make 'tm-item (list :name name :enable nil)))

     ;; (menu-item NAME BINDING ... :button (:radio ...) ...)
     (`(menu-item ,name ,binding . ,(and (pred radio?) props))
      (make 'tm-radio (cl-list* :name name :binding binding props)))

     ;; (menu-item NAME BINDING ... :button (:toggle ...) ...)
     (`(menu-item ,name ,binding . ,(and (pred toggle?) props))
      (make 'tm-checkbox (cl-list* :name name :binding binding props)))

     ;; (menu-item NAME BINDING ...)
     (`(menu-item ,name ,binding . ,props)
      (make 'tm-item (cl-list* :name name :binding binding props)))

     ;; (SEPARATOR-NAME ...)
     (`(,(and (pred separator?) name) . ,_)
      (make 'tm-separator (list :name name)))

     ;; (NAME KEYMAP)
     (`(,name ,(and (pred keymapp) keymap))
      (make 'tm-item (list :name name :binding keymap)))

     ;; (NAME HELP BINDING)
     (`(,name ,help ,binding)
      (make 'tm-item (list :name name :binding binding :help help)))

     ;; (NAME . BINDING)
     (`(,name . ,binding)
      (make 'tm-item (list :name name :binding binding))))))

(defun tm--create-buffer (pane)
  "Create a buffer named BUFFER for DRAW to fill.
DRAW is called with no arguments and with current buffer being the
buffer created. Value is (BUFFER WIDTH HEIGHT), where BUFFER is
the buffer that was used, WIDTH is the maximum line width in the
buffer, and HEIGHT is the number of lines in the buffer. "
  (with-slots (buffer) pane
    (with-current-buffer buffer
      (dolist (var '((mode-line-format . nil)
                     (header-line-format . nil)
                     (tab-line-format . nil)
                     (tab-bar-format . nil)
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
                     ;; > 100 means don't ever recenter.
                     (scroll-conservatively . 101)))
	(set (make-local-variable (car var)) (cdr var)))
      (let ((inhibit-modification-hooks t)
            (inhibit-read-only t)
	    (indent-tabs-mode nil))
	(setq tm--pane-drawn pane)
	(tm--draw pane 0)
	(cl-flet ((line-width ()
		    (save-excursion
		      (goto-char (point-min))
		      (goto-char (pos-eol))
		      (current-column))))
	  (list (current-buffer)
		(line-width)
		(count-lines (point-min) (point-max))))))))

(defun tm--keymap-name (keymap)
  "Return the name of KEYMAP, if any."
  (when (symbolp keymap)
    (setq keymap (indirect-function keymap)))
  (let ((name (last keymap)))
    (and (stringp (car name)) (car name))))

(defconst tm--buffer-name-prefix " *tm--")

(defun tm--pane-buffer-name (keymap)
  "Make a buffer name for KEYMAP."
  (if-let* ((name (tm--keymap-name keymap)))
      (format "%s%s*" tm--buffer-name-prefix name)
    (generate-new-buffer-name tm--buffer-name-prefix)))

(defun tm--make-pane (keymap invoking-item frame)
  "Create a `tm-pane'.
KEYMAP is the menu keymap for the pane. INVOLING-ITEM if non-nil is the
menu-item that invoked this menu.  FRAME Is the frame to display the
menu in."
  (let ((pane (make-instance
               'tm-pane
               :keymap keymap
               :invoking-item invoking-item
               :frame frame
	       :buffer (get-buffer-create
                        (tm--pane-buffer-name keymap)))))
    (cl-loop for binding being the key-bindings of keymap
             using (key-codes code)
             do (tm--make-element pane code binding))
    pane))

(defvar tm--frame-parameters
  `((visibility . nil)
    (background-color . "grey20")
    (foreground-color . "white")
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
    (tm-bar-lines . 0)
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

(defun tm--frame-parameters ()
  "Return the frame parameters to use for menu child frames."
  (let ((params (copy-sequence tm--frame-parameters)))
    (when-let* ((fg (face-attribute 'tm-face :foreground))
                ((stringp fg)))
      (setf (alist-get 'foreground-color params) fg))
    (when-let* ((bg (face-attribute 'tm-face :background))
                ((stringp bg)))
      (setf (alist-get 'background-color params) bg))
    params))

(defun tm--make-fully-visible (f1 f2 x y)
  "Make frame F2 fully visible in F1,
Don't obscure point (X, Y) if possible.  Change size and position of F2
as needed."
  (let* ((f1-edges (frame-edges f1))
	 (f1-width (frame-width f1))
         (f1-height (frame-height f1))
         (f1-x (nth 0 f1-edges))
         (f1-y (nth 1 f1-edges))
         (f2-width (frame-width f2))
         (f2-height (frame-height f2))
         (new-x (max f1-x (min (+ f1-x f1-width (- f2-width)) x)))
         (new-y (max f1-y (min (+ f1-y f1-height (- f2-height)) y))))
    ;; Ensure F2 is fully contained within F1
    (setq new-x (min new-x (+ f1-x f1-width (- f2-width))))
    (setq new-y (min new-y (+ f1-y f1-height (- f2-height))))
    ;; Move and resize F2
    (set-frame-position f2 new-x new-y)
    (set-frame-size f2 f2-width f2-height)))

(defun tm--frame-absolute-position (frame x y)
  "Translate (X, Y) in FRAME to absolute coordinates."
  (let ((current-frame frame)
        (abs-x x)
        (abs-y y))
    (while (frame-parent current-frame)
      (let ((edges (frame-edges current-frame)))
	(setq abs-x (+ abs-x (nth 0 edges)))
	(setq abs-y (+ abs-y (nth 1 edges)))
	(setq current-frame (frame-parent current-frame))))
    (cons abs-x abs-y)))

(defun tm--create-frame (keymap where invoking-item)
  (cl-destructuring-bind (parent-frame x y) where
    (when (frame-parent parent-frame)
      (cl-destructuring-bind (ax . ay)
	  (tm--frame-absolute-position parent-frame x y)
	(setq x ax y ay)
	(setq parent-frame (frame-root-frame parent-frame))))
    (let* ((minibuffer (minibuffer-window parent-frame))
           (window-min-height 1)
           (window-min-width 1)
           (after-make-frame-functions nil)
	   (frame (make-frame `((parent-frame . ,parent-frame)
				(minibuffer . ,minibuffer)
				,@(tm--frame-parameters))))
	   (win (frame-root-window frame)))
      (let ((pane (tm--make-pane keymap invoking-item frame)))
        (cl-destructuring-bind (buffer width height)
            (tm--create-buffer pane)
	  (modify-frame-parameters frame `((name . ,(buffer-name buffer))
				           (tm-buffer . ,buffer)))
	  (set-window-buffer win buffer)
	  (set-window-dedicated-p win t)
          ;; Don't make the frame absurdly large.
          (setq height (min height
			    (round (/ (frame-height parent-frame) 1.6))))
	  (set-frame-size frame width height)
	  (set-frame-position frame x y)
	  (tm--make-fully-visible parent-frame frame x y)
	  (make-frame-visible frame)
	  (raise-frame frame)
	  frame)))))

;; Debugging aid. Delete all menu frames. Don't delete the buffers, we
;; might want to inspect them.
(defun tm--delete-menu-frames ()
  (interactive)
  (cl-flet ((frame-name (frame)
	      (frame-parameter frame 'name)))
    (cl-loop for frame in (frame-list)
	     when (string-prefix-p " *tm-" (frame-name frame))
	     do (tm--delete frame))))

(defun tm--childp (child parent)
  "Return t if pane CHILD is a child of PARENT."
  (cl-loop for c = (slot-value parent 'child-pane)
           then (slot-value c 'child-pane)
           while c
           when (eq c child) return t))

(defun tm-cmd-mouse-moved (event)
  "Handle mouse movement in a menu."
  (interactive "e")
  ;; If we moved the mouse in the menu-bar, and we are displaying a menu
  ;; for a menu-bar, and the menu-bar item moved to is different from
  ;; the one we are displaying, close the current menu, and display the
  ;; new one.
  (when (and tm--from-menu-bar
             (eq (posn-area (event-end event)) 'menu-bar))
    (cl-destructuring-bind (x . y) (posn-x-y (event-end event))
      ;; Set current buffer so that we compute things with the right
      ;; menu-bar for that buffer.
      (with-current-buffer tm--updating-buffer
        (when-let* ((new (menu-bar-menu-at-x-y x y menu-updating-frame))
                  (old (menu-bar-menu-at-x-y (car tm--from-menu-bar)
                                             (cdr tm--from-menu-bar)
                                             menu-updating-frame))
                  ((and new (not (eq old new))))
                  (start-x (cdr (menu-bar-item-at-x x))))
          (setq tm--from-menu-bar (posn-x-y (event-end event)))
          (throw 'tm-to-top-level `(menu-bar ,start-x ,y))))))

  (when-let* ((end (event-end event))
	      (win (posn-window end))
              (item (mouse-posn-property end 'tm-item))
	      ((tm--selectable-p item))
              (pane (slot-value item 'pane))
              (frame (slot-value pane 'frame)))
    (tm--select item 'mouse)
    (unless (eq pane tm--pane-drawn)
      (when-let* ((sel (slot-value tm--pane-drawn 'selected-item)))
        (if (tm--childp pane tm--pane-drawn)
            ;; Move to child -> activate child.
            (tm--act sel 'mouse)
          (throw 'tm-leave nil))))))

(defun tm-cmd-mouse-act (event)
  "Perform action on selected menu-item."
  (interactive "e")
  (when-let* ((end (event-end event))
	      (win (posn-window end))
              ((or (eq win (selected-window))
                   (throw 'tm-to-top-level nil)))
              (item (mouse-posn-property end 'tm-item))
	      ((tm--selectable-p item)))
    (goto-char (posn-point end))
    (tm--act item 'mouse)))

(defun tm-cmd-key-act ()
  "Perform the action associated with a menu-item."
  (interactive)
  (let* ((selected (slot-value tm--pane-drawn 'selected-item)))
    (if selected
        (tm--act selected 'key)
      ;; macOS closes the menu if no menu-item is selected.
      (throw 'tm-leave nil))))

(defun tm-cmd-next-item ()
  "Move to next selectable menu-item."
  (interactive)
  (let* ((pane tm--pane-drawn)
         (items (slot-value pane 'items))
         (selected (slot-value pane 'selected-item))
         (next (if selected
                   (slot-value selected 'next-item)
                 (cl-first items))))
    (while next
      (if (tm--selectable-p next)
          (progn
            (tm--select next nil)
            (setq next nil))
        (setq next (slot-value next 'next-item))))))

(defun tm-cmd-previous-item ()
  "Move to previous selectable menu-item."
  (interactive)
  (let* ((pane tm--pane-drawn)
         (selected (slot-value pane 'selected-item))
         (prev (and selected (slot-value selected 'prev-item))))
    (while prev
      (if (tm--selectable-p prev)
          (progn
            (tm--select prev nil)
            (setq prev nil))
        (setq prev (slot-value prev 'prev-item))))))

(cl-defstruct (tm--bar-menu
               (:constructor nil)
               (:constructor tm--make-bar-menu (name key cmd)))
  name key cmd x0 x1)

(defun tm--bar-info-2 ()
  "Get keys and bindings of the menu-bar.
Value is a list of (KEY . BINDING) where duplicate keys and definitions
to `undefined' have been removed.  See function `menu_bar_item' in C why
that is done."
  (cl-loop with keys-seen = () and result = ()
           for key being the key-codes of (menu-bar-keymap)
           using (key-bindings binding) do
           (if (eq binding 'undefined)
               (setq result (cl-remove key result :key #'car))
             (unless (memq key keys-seen)
               (push (cons key binding) result)))
           (push key keys-seen)
           finally return (nreverse result)))

(defun tm--bar-info-1 ()
  "Compute the layout of the menu-bar of buffer BUFFER.
Value is a list of `tm--bar-menu' structures containing the needed info."
  (with-current-buffer tm--updating-buffer
    (when-let* ((menu-bar (tm--bar-info-2)))
      (cl-loop
       with layout = ()
       for (key . binding) in menu-bar do
       (pcase binding
         ((or `(,(and (pred stringp) name) . ,cmd)
              `(menu-item
                ,name ,cmd
                . ,(and props
                        (guard (let ((visible (plist-get props :visible)))
                                 (or (null visible) (eval visible)))))))
          (push (tm--make-bar-menu name key cmd) layout)))
       finally return (nreverse layout)))))

(defun tm--bar-info ()
  "Compute the layout of the menu-bar of buffer BUFFER.
Value is a list of `tm--bar-menu' structures containing the needed info."
  (let ((info (tm--bar-info-1))
        (column 0))
    (dolist (i info info)
      (setf (tm--bar-menu-x0 i) column)
      (incf column (1+ (length (tm--bar-menu-name i))))
      (setf (tm--bar-menu-x1 i) column))))

(defun tm--bar-find-pane (menu-bar pane)
  "Find PANE in the menu-bar MENU-BAR.
Return the index of of its entry in LAYOUT. Value is nil if not found,
but that should not happen."
  (cl-position-if (lambda (m)
                    (eq (slot-value pane 'keymap)
                        (tm--bar-menu-cmd m)))
                  menu-bar))

(defun tm--move-in-menu-bar (move-left)
  "Arrange to move to another item in the menu-bar.
MOVE-LEFT non-nil means move to the previous item.  This
gets the selected tm-item and determines the pane it is
in. If that is a top-level pane, not a sub-menu, see where that pane
is in the menu-bar. Then determine the next/previous menu-bar item,
and make us display that menu."
  (when-let* (((not (null tm--from-menu-bar)))
              (root-pane (tm--root-pane tm--pane-drawn))
              (menu-bar (tm--bar-info))
              (index (tm--bar-find-pane menu-bar root-pane))
              (n (length menu-bar)))
    (cond (move-left
           (decf index)
           (when (< index 0)
             (setq index (1- n))))
          (t
           (incf index)
           (when (>= index n)
             (setq index 0))))
    (let ((x0 (tm--bar-menu-x0 (nth index menu-bar))))
      (throw 'tm-to-top-level `(menu-bar ,x0 0)))))

(defun tm--selected-item ()
  (slot-value tm--pane-drawn 'selected-item))

(defun tm-cmd-open ()
  "Select a menu-item with <right> if it is for a sub-menu."
  (interactive)
  (when-let* ((item (tm--selected-item)))
    (with-slots (binding) item
      (when (keymapp binding)
	(tm--act item 'key))))
  (tm--move-in-menu-bar nil))

(defun tm-cmd-close ()
  "Close current menu pane with <left>."
  (interactive)
  ;; If this is not a top-level pane, close it.
  (when-let* ((item (tm--selected-item))
              (pane (slot-value item 'pane))
              ((slot-value pane 'invoking-item)))
    (throw 'tm-leave nil))
  ;; If it is a top-level plane, either move left in the menu-bar or
  ;; close it.
  (tm--move-in-menu-bar 'left)
  (throw 'tm-leave nil))

(defun tm--isearch (forward)
  "Isearch in a menu, FORWARD t means search forward."
  (isearch-mode forward nil nil)
  (while isearch-mode
    (let* ((key (read-key-sequence nil nil t nil nil nil)))
      (when-let* ((cmd (lookup-key isearch-mode-map key)))
	(if (and (eq cmd #'isearch-printing-char)
		 (stringp key))
	    (isearch-printing-char (aref key 0))
	  (call-interactively cmd)))))
  (let* ((items (slot-value tm--pane-drawn 'items))
         (i (1- (line-number-at-pos (point) t)))
         (sel (nth i items)))
    (while (and sel (not (tm--selectable-p sel)))
      (setf sel (if forward
                    (slot-value sel 'next-item)
                  (slot-value sel 'prev-item))))
    (when sel
      (tm--select sel 'key))))

(defun tm-cmd-isearch-forward ()
  "Isearch forward in a menu."
  (interactive)
  (tm--isearch t))

(defun tm-cmd-isearch-backward ()
  "Isearch backward in a menu."
  (interactive)
  (tm--isearch nil))

(defun tm-cmd-menu-bar-click (_event)
  "Handle click in a menu-bar while a menu is open."
  (interactive "e")
  (throw 'tm-leave nil))

(defun tm-cmd-close-on-click (_event)
  "Close one menu-pane."
  (interactive "e")
  (throw 'tm-to-top-level nil))

(defvar-keymap tm-keymap
  :doc "Keymap for menu interaction."
  "<up>" #'tm-cmd-previous-item
  "<down>" #'tm-cmd-next-item
  "<left>" #'tm-cmd-close
  "<right>" #'tm-cmd-open
  "C-b" #'tm-cmd-close
  "C-f" #'tm-cmd-open
  "C-g" #'keyboard-quit
  "C-j" #'tm-cmd-key-act
  "C-n" #'tm-cmd-next-item
  "C-p" #'tm-cmd-previous-item
  "C-r" #'tm-cmd-isearch-backward
  "C-s" #'tm-cmd-isearch-forward
  "RET" #'tm-cmd-key-act
  "SPC" #'tm-cmd-key-act
  "<return>" #'tm-cmd-key-act
  "<mouse-movement>" #'tm-cmd-mouse-moved
  "<menu-bar> <mouse-1>" #'tm-cmd-menu-bar-click
  "<tab-bar> <mouse-1>" #'tm-cmd-close-on-click
  "<vertical-line> <mouse-1>" #'tm-cmd-close-on-click
  "<mode-line> <mouse-1>" #'tm-cmd-close-on-click
  "<mouse-1>" #'tm-cmd-mouse-act)

(defun tm--position (pos)
  "Translate position POS to (FRAME X Y)."
  (interactive)
  (pcase-exhaustive pos
    ;; nil
    ('nil nil)

    ;; t
    ('t
     (cl-destructuring-bind (frame x . y) (mouse-position)
       ;; mouse-position sometimes returns nil for x and y which I
       ;; think should not happen.
       (list frame (or x 10) (or y 10))))

    ;; EVENT
    ((and e (guard (eventp e)))
     (let* ((end (event-end e))
	    (win (posn-window end))
	    (x (car (posn-x-y end)))
	    (y (cdr (posn-x-y end))))
       ;; posn-window returns a frame when the event is not on a window,
       ;; for example when clicking on a menu bar in a tty frame.
       (if (windowp win)
	   (cl-destructuring-bind (wx wy _ _) (window-edges win nil t)
	     (list (window-frame win) (+ wx x) (+ wy y)))
	 (let ((menu-bar-lines (frame-parameter win 'menu-bar-lines)))
	   (list win x (+ y menu-bar-lines))))))

    ;; ((X . Y) WINDOW)
    ((and `((,x . ,y) ,win)
          (guard (and (numberp x) (numberp y) (windowp win))))
     (cl-destructuring-bind (wx wy _ _) (window-edges win nil t)
       (list (window-frame win) (+ wx x) (+ wy y))))

    ;; ((X Y) WINDOW)
    ((and `((,x ,y) ,win)
          (guard (and (numberp x) (numberp y) (windowp win))))
     (cl-destructuring-bind (wx wy _ _) (window-edges win nil t)
       (list (window-frame win) (+ wx x) (+ wy y))))

    ;; ((X . Y) FRAME)
    ((and `((,x . ,y) ,frame)
          (guard (and (numberp x) (numberp y) (framep frame))))
     (list frame x y))

    ;; ((X Y) FRAME
    ((and `((,x ,y) ,frame)
          (guard (and (numberp x) (numberp y) (framep frame))))
     (list frame x y))

    ;; (X . Y)
    ((and `(,x . ,y)
          (guard (numberp x) (numberp y)))
     (list (selected-frame) x y))))

(defun tm--where (selected how)
  "Determine where to display SELECTED,
HOW is either `key' or `mouse' and specifies how the menu
invocation takes place."
  (cl-ecase how
    (key
     (let* ((frame (tm--frame selected))
            (end (slot-value selected 'draw-end))
	    (win (frame-root-window frame))
            (posn (posn-at-point (1- end) win)))
       (cl-destructuring-bind (x . y) (posn-x-y posn)
         (list frame
               (+ x tm-sub-menu-offset-x)
               (+ y tm-sub-menu-offset-y)))))
     (mouse
      (let* ((posn (posn-at-point (pos-eol)))
	    (win (posn-window posn)))
       (cl-destructuring-bind (x . y) (posn-x-y posn)
         (tm--position (list (cons (- x 3) y) win)))))))

(defun tm--select-menu-bar-item-by-name (key)
  "Switch to another menu in the menu-bar.
KEY is a string determining what menu to look for."
  (when-let* ((pos tm--from-menu-bar)
              ((char-uppercase-p (aref key 0)))
              (menu-bar (tm--bar-info))
              (start (cl-position-if
                      (lambda (m)
                        (and (>= (tm--bar-menu-x0 m) (car pos))
                             (< (car pos) (tm--bar-menu-x1 m))))
                      menu-bar)))
    (cl-loop with n = (length menu-bar)
             repeat n
             for i = (mod (1+ start) n) then (mod (1+ i) n)
             for m = (nth i menu-bar)
             for name = (tm--bar-menu-name m)
             for x = (tm--bar-menu-x0 m)
             when (string-prefix-p key name t) do
             (setq tm--from-menu-bar (cons x 0))
             (throw 'tm-to-top-level `(menu-bar ,x 0)))))

(defun tm--select-item-by-name ()
  "Select a menu-item from `this-command-keys'."
  (when-let* ((key (this-command-keys))
              ((stringp key))
              (pane tm--pane-drawn)
              (items (slot-value pane 'items)))
    (cl-flet ((matchp (i)
                (when (tm--selectable-p i)
                  (let ((name (string-trim-left (slot-value i 'name))))
                    (string-prefix-p key name t)))))
      (unless (tm--select-menu-bar-item-by-name key)
        (let* ((current (slot-value pane 'selected-item))
               (pos (if current (cl-position current items) 0)))
          (cl-loop with n = (length items)
                   repeat n
                   for i = (mod (1+ pos) n) then (mod (1+ i) n)
                   for item = (nth i items)
                   when (matchp item)
                   do (tm--select item 'key)
                   and return t))))))

(defun tm--open-on-pane (item)
  "Return t if ITEM's pane has a sub-menu open."
  (when-let* ((pane (slot-value item 'pane))
              (open (slot-value pane 'child-pane)))
    (slot-value open 'invoking-item)))

(defun tm--open/close-menu-on-selection (previous selected)
  "Open or close a sub-menu on selection.

PREVIOUS is the previously selected menu-item, SELECTED is the currently
selected menu-item.  This function either returns normally, in which
case nothing has to be done, or it throws `tm--leave' with a value.
Throwing `tm-leave' leaves the innermost command loop.

If the value thrown is (SELECTED . key), this means to open the sub-menu
of SELECTED.  If the value thrown is `close-sub-menu' this means to
close a previously open sub-menu."
  (let ((prev (and previous (tm--binding-type previous)))
        (sel (and selected (tm--binding-type selected))))
    (pcase-exhaustive (cons prev sel)
      ;; nil -> nil
      (`(nil . nil))

      ;; nil -> command
      (`(nil . command))

      ;; nil -> keymap
      (`(nil . keymap)
       (throw 'tm-leave (cons selected 'key)))

      ;; command -> nil
      (`(command . nil))

      ;; command -> command
      (`(command . command))

      ;; command -> keymap
      (`(command . keymap)
       (throw 'tm-leave (cons selected 'key)))

      ;; keymap -> nil
      (`(keymap . nil)
       (throw 'tm-leave 'close-sub-menu))

      ;; keymap -> command
      (`(keymap . command)
       (throw 'tm-leave 'close-sub-menu))

      ;; keymap -> keymap
      (`(keymap . keymap)
       (throw 'tm-leave (cons selected 'key))))))

(defun tm--command-loop ()
  "Innermost command loop.
This reads events with `read-key-sequence' until something uses `throw'
to leave the loop."
  (catch 'tm-leave
    (let ((previous-selected (tm--selected-item)))
      (while t
        ;; Open or close sub-menus on selection change.  This throws out
        ;; of this loop if needed to open or close a sub-menu.
        (let* ((selected (tm--selected-item)))
          (unless (eq selected previous-selected)
            (tm--open/close-menu-on-selection previous-selected selected)
            (setq previous-selected selected)))

        (let* ((track-mouse t)
	       (key (read-key-sequence nil))
	       (cmd (lookup-key tm-keymap key)))
          ;; Entering a character that is self-inserting in global-map
          ;; searches for a menu-item beginning with that character.
          (when-let* (((not (tm--command-p cmd)))
                      (cmd (lookup-key global-map key))
                      ((eq cmd 'self-insert-command)))
            (tm--select-item-by-name))
          ;; Otherwise execute a command, if any.  This is for toggling
          ;; buttons, moving on the menu and so on.
	  (when (tm--command-p cmd)
	    (call-interactively cmd)))))))

(defun tm--after-command-loop (res frame)
  "Perform actions after `tm--command-loop' has been left.
RES is what `tm--command-loop' returned.  FRAME is current menu frame
being used.  Value is the menu frame to use in the future. "
  (let ((use-frame frame))
    (pcase-exhaustive res
      ;; (SELECTED . HOW) - we want to open the sub-menu of SELECTED
      ;; if not already open, or we want to activate it.
      (`(,selected . ,(and (pred symbolp) how))
       (with-slots (binding) selected
         (cond
          ((keymapp binding)
           (let ((open (tm--open-on-pane selected)))
             (cond
              ;; Is already open -> set the focus to it.  FIXME:
              ;; if it lands on a menu-item that has a sub-menu,
              ;; open that sub-menu.
              ((eq open selected)
               (select-frame-set-input-focus frame))
              ;; Not open -> open it and process events recursively.
              ((null open)
	       (tm--loop-1 binding
                           (tm--where selected how)
                           :invoking-item selected
                           :focus nil))
              ;; We are on the same level.  Close our current menu, and
              ;; replace it with the new one.
              (t
               (tm--delete frame)
               (setq use-frame (tm--create-frame
                                binding
                                (tm--where selected how)
                                selected))))))

          ;; We want to close the current menu.
          (t (throw 'tm-item-close selected)))))

      ;; close-sub-menu - we want to close a currently open sub-menu.
      (`close-sub-menu
       (when-let* ((child (slot-value tm--pane-drawn 'child-pane)))
         (tm--delete child)))

      ;; We want to close the current menu.
      ('nil
       (throw 'tm-item-close nil)))
    use-frame))

(cl-defun tm--loop-1 (keymap where &key invoking-item (focus t))
  "Second level of the event loop.
KEYMAP is the keymap to open as a menu. WHERE is where on the display to
open the menu. INVOKING-ITEM, if non-nil is a menu item for which to
display a sub-menu.  FOCUS non-nil means to set the input focus to the
new menu."
  (let ((frame (tm--create-frame keymap where invoking-item)))
    (unwind-protect
        (catch 'tm-item-close
          (when focus
            (select-frame-set-input-focus frame))
          (while t
            (let* ((res (tm--command-loop)))
              (setq frame (tm--after-command-loop res frame))
              (tm--set-overlay-face tm--pane-drawn))))
      (tm--delete frame))))

(defun tm--loop (keymap where)
  "Event loop for tty menus.
KEYMAP is a menu keymap, and WHERE specifies where to open the menu."
  (let ((res (catch 'tm-to-top-level
               (save-selected-window
                 (tm--loop-1 keymap where :focus t)))))
    (pcase-exhaustive res
      ('nil nil)
      (`(menu-bar ,x ,y) (cons x y))
      (`(,selected . ,_) selected))))

(cl-defun tm--popup-menu (position menu)
  "This is the replacement for `x-popup-menu'.
It is installed as `x-popup-menu-function' when using `tm-mode'."
  (when-let* ((where (tm--position position)))
    (cl-destructuring-bind (menu-updating-frame _ _) where
      (let ((tm--updating-buffer (current-buffer)))
        (cond ((keymapp menu)
               ;; Run the event loop, and at the end, collect bindings
               ;; for nested menus involved to form the result event.
               (cl-loop for i = (tm--loop menu where)
                        then (with-slots (pane) i
                               (with-slots (invoking-item) pane
                                 invoking-item))
                        while i
                        when (consp i) return i
                        collect (slot-value i 'key-code) into codes
                        finally return (nreverse codes)))
	      ((consp menu)
	       (cl-loop with outer = (make-sparse-keymap "outer")
		        for keymap in menu
		        for name = (tm--keymap-name keymap)
		        do (define-key outer (vector (intern name)) keymap)
		        finally (tm--loop outer where)))
	      (t (error "Not a menu: %S" menu)))))))

(defun tm--around-mouse-set-point (old-fun &rest args)
  "Around-advice for `mouse-set-point',
A mouse-click in a menu can lead to one or more frames for menu panes
being deleted. Somehow, such a click event survives the frame deletions,
and lands in `mouse-set-point' with the event containing a window on such
a deleted frame. I couldn't find the reason for that.  So this is an
advice added to handle that."
  (let* ((event (car args))
         (win (posn-window (event-start event))))
    (when (or (not (windowp win)) (window-live-p win))
      (apply old-fun args))))

(defun tm--around-popup-menu (old-fun &rest args)
  "Around-advice for `popup-menu'.
This around-advice for `popup-menu' binds `tm--from-menu-bar' to
(X . Y) in the menu-bar when invoked for a menu-bar menu, and nil
otherwise. This makes it possible to behave differently for menus in
the menu-bar and others."
  (let ((tm--from-menu-bar
         (pcase args
           (`(,_ ,pos ,_ ,(and menu-bar (guard menu-bar)))
            (posn-x-y pos)))))
    (apply old-fun args)))

(defun tm--around-frame-list (old-fun &rest args)
  "Around-advice for `frame-list'.
This removes frames used for menus from the result value."
  (cl-remove-if (lambda (f)
                  (let ((name (frame-parameter f 'name)))
                    (string-prefix-p tm--buffer-name-prefix name)))
                (apply old-fun args)))

(defvar tm--yank-menu-index 0)
(defcustom tm--yank-menu-keys "1234567890abcdefghijklmnopqrstuvwxyz"
  "Characters to use for yank menu entries."
  :type 'string)

(defun tm--around-menu-bar-update-yank-menu (old-fun &rest args)
  "Around-advice for `menu-bar-update-yank-menu'.
Display that a little bit nicer."
  (cl-destructuring-bind (string old) args
    (when (string-match "\n" string)
      (setq string (string-replace "\n" "\\n" string)))
    (setq string (format "%c: %s" (aref tm--yank-menu-keys
                                        tm--yank-menu-index)
                         string))
    (setq tm--yank-menu-index
          (mod (1+ tm--yank-menu-index)
               (length tm--yank-menu-keys)))
    (funcall old-fun string old)))

(defun tm-buffer-menu-open ()
  "Override for `buffer-menu-open'.
Opens the menu at point instead of at (0, 0)."
  (popup-menu (mouse-buffer-menu-keymap) (posn-at-point)))

;;;###autoload
(define-minor-mode tm-menu-mode
  "Global minor mode for displaying menus with tty child frames.
\\{tm-keymap}
Entering a self-inserting character goes to the next menu-item starting
with that character.  When no next items is found, start searching from
the start."
  :global t :group 'menu
  (unless (display-graphic-p)
    (cond (tm-menu-mode
           (advice-add 'menu-bar-update-yank-menu :around
                       #'tm--around-menu-bar-update-yank-menu)
           (advice-add 'buffer-menu-open :override #'tm-buffer-menu-open)
           (advice-add 'mouse-set-point :around #'tm--around-mouse-set-point)
           (advice-add 'popup-menu :around #'tm--around-popup-menu)
           (advice-add 'frame-list :around #'tm--around-frame-list)
           (setq x-popup-menu-function #'tm--popup-menu))
          (t
           (advice-remove 'menu-bar-update-yank-menu
                          #'tm--around-menu-bar-update-yank-menu)
           (advice-remove 'buffer-menu-open #'tm-buffer-menu-open)
           (advice-remove 'mouse-set-point #'tm--around-mouse-set-point)
           (advice-remove 'popup-menu #'tm--around-popup-menu)
           (advice-remove 'frame-list #'tm--around-frame-list)
           (setq x-popup-menu-function nil)))))

(provide 'tm)
