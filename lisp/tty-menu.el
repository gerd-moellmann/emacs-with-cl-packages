;; -*- lexical-binding: t; symbol-packages: t -*-
;;; tty-menu.el --- A tty menu implementation in Lisp

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
;; | outer |         | inner |         | command |
;; +-------+         +-------+         +---------+
;;     |                 |                  |
;;     |---------------->|----------------->|
;;     |                 |                  |
;;     |                 |<--throw leave----|
;;     |                 |<----return-------|
;;     |                 |----------------->|
;;     |                 |                  |
;;     |<---------throw to-top-level--------|
;;     |                 |                  |
;;     |                 |--recursion--+    |
;;     |                 |             |    |
;;     |                 |<------------+    |
;;     |<----return------|                  |
;;     |                 |                  |
;;
;;
;; - Use of slot-value and with-slots in the code:
;;
;; There is a reason for that. Accessors have to obey naming rules,
;; which means they should use the `tty-menu-' prefix. That's
;; unreadable, and the reason I prefer with-slots and slot-value. It's a
;; sad state of affairs.

;;; Code:

(require 'cl-lib)

(defgroup tty-menu nil
  "Group for tty-menus implemented in Lisp."
  :group 'menu)

(defface tty-menu-face
  '((t :inherit menu))
  "Face for TTY menus."
  :group 'tty-menu
  :version "31.1")

(defface tty-menu-face-selected
  '((t :inherit tty-menu-face :background "darkblue"))
  "Face for selected menu items."
  :group 'tty-menu
  :version "31.1")

(defface tty-menu-face-selected-inactive
  '((t :inherit tty-menu-face :background "grey20"))
  "Face for inactive selected menu items."
  :group 'tty-menu
  :version "31.1")

(defface tty-menu-face-disabled
  '((t :inherit tty-menu-face :foreground "gray5"))
  "Face for disabled menu items."
  :group 'tty-menu
  :version "31.1")

(defclass tty-menu-element () ())

(defclass tty-menu-pane (tty-menu-element)
  ((items :initarg :items :initform nil :type t)
   (buffer :initarg :buffer :type buffer)
   (overlay :initarg nil :type overlay)
   (frame :initarg :frame :type frame)
   (layout :type list)
   (keymap :initarg :keymap)
   (invoking-item :initarg :invoking-item :type (or null tty-menu-item))
   (parent-pane :initform nil :type (or null tty-menu-pane))
   (child-pane :initform nil :type (or null tty-menu-pane))
   (selected-item :initform nil :type (or null tty-menu-item))))

;; Type t in many places because even the name can be a form that needs
;; to be evaluated to get the name (Redo is an example in the context
;; menu).
(defclass tty-menu-item (tty-menu-element)
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
   (pane :initarg :pane :type tty-menu-pane)
   (next-item :initform nil :type (or null tty-menu-item))
   (prev-item :initform nil :type (or null tty-menu-item))
   (draw-start :initform nil :type (or null number))
   (draw-end :initform nil :type (or null number))))

(defclass tty-menu-button (tty-menu-item) ())
(defclass tty-menu-radio (tty-menu-button) ())
(defclass tty-menu-checkbox (tty-menu-button) ())
(defclass tty-menu-separator (tty-menu-item)
  ((sep :initform "-" :type string :reader tty-menu-sep)))

(defvar tty-menu-updating-buffer nil
  "Dynamically bound to the current buffer when a menu is invoked.")

(defvar tty-menu-from-menu-bar nil
  "Non-nil means menu is invoked for a menu-bar.
Bound by an around advice for `popup-menu' if called for a menu-bar
menu.  If non-nil, it is a cons (X . Y) of the menu-item.")

(defun tty-menu-eval (form)
  "Evaluate FORM in the context of the menu.
The context consists of the buffer that was current when the menu
was invoked.  This buffer must be current when evaluating various things
in the menu because of local variables."
  (with-current-buffer tty-menu-updating-buffer
    (eval form)))

(defun tty-menu-get-separator-string (name)
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
  :after ((item tty-menu-separator) &rest)
  "Constructor for separator ITEM.
Computes the actual separator char to use for a separator."
  (with-slots (name sep enable) item
    (setf enable nil)
    (setf sep (tty-menu-get-separator-string name))))

(cl-defmethod initialize-instance
  :after ((item tty-menu-item) &rest)
  "Constructor for menu-item ITEM.
If a menu-item's binding is a keymap with 0 elements, disable it."
  (with-slots (binding filter enable) item
    (when filter
      (setf binding (tty-menu-eval `(,filter (quote ,binding)))))
    (when (symbolp binding)
      (setf binding (indirect-function binding)))
    (when (and (keymapp binding)
               (zerop (cl-loop for b being the key-codes of binding
                               count b)))
      (setf enable nil))))

(cl-defmethod initialize-instance
  :after ((pane tty-menu-pane) &rest)
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
(defvar tty-menu-left-border-format "%1s")
(defvar tty-menu-right-border-format "%1s")
(defvar tty-menu-button-format "%-2s")
(defvar tty-menu-key-format "%10s")
(defvar tty-menu-name-format "%-20s")

;; Characters to use for radio buttons and checkboxes.
(defvar tty-menu-triangle "▶")
(defvar tty-menu-radio-on "●")
(defvar tty-menu-radio-off "◯")
(defvar tty-menu-checkbox-on "✔")
(defvar tty-menu-checkbox-off "□")

(defun tty-menu-call-interactively (fn)
  "Call FN interactively in `tty-menu-updating-buffer'."
  (with-current-buffer tty-menu-updating-buffer
    (call-interactively fn)))

(defun tty-menu-visible-p (item)
  "Value is non-nil if ITEM is visible."
  (tty-menu-eval (slot-value item 'visible)))

(defun tty-menu-enabled-p (item)
  "Value is non-nil if ITEM is enabled."
  (tty-menu-eval (slot-value item 'enable)))

(defun tty-menu-selectable-p (item)
  "Value is non-nil if ITEM is selectable by the user."
  (and (tty-menu-visible-p item)
       (tty-menu-enabled-p item)))

(defun tty-menu-name (item)
  "Value is the name of ITEM."
  (tty-menu-eval (slot-value item 'name)))

(defun tty-menu-button-selected-p (item)
  "Value is non-nil if button ITEM is selected (on)."
  (with-slots (button) item
    (cl-destructuring-bind (_ . form) button
      (tty-menu-eval form))))

(defun tty-menu-ninsert (n x)
  "Insert N times X at point."
  (cl-loop repeat n do (insert x)))

(cl-defgeneric tty-menu-button-string (item)
  "Value is a string for the button part of ITEM."
  ( :method ((_ tty-menu-item))
    "")
  ( :method ((r tty-menu-radio))
    (format tty-menu-button-format
            (if (tty-menu-button-selected-p r)
		tty-menu-radio-on
	      tty-menu-radio-off)))
  ( :method ((c tty-menu-checkbox))
    (format tty-menu-button-format
            (if (tty-menu-button-selected-p c)
		tty-menu-checkbox-on
	      tty-menu-checkbox-off)))
  ( :method ((_ tty-menu-separator))
    ""))

(cl-defgeneric tty-menu-name-string (item)
  "Value is a string for the name part of ITEM."
  ( :method ((item tty-menu-item))
    (format tty-menu-name-format (tty-menu-name item)))
  ( :method ((_ tty-menu-separator))
    ""))

(cl-defgeneric tty-menu-key-string (item)
  "Value is a string for the key part of ITEM."
  ( :method ((item tty-menu-item))
    (format tty-menu-key-format
	    (with-slots (binding) item
	      (cond ((commandp binding)
		     (key-description
		      (where-is-internal binding nil t)))
		    ((keymapp binding)
		     tty-menu-triangle)
		    (t "")))))
  ( :method ((_ tty-menu-separator)) ""))

(cl-defgeneric tty-menu-draw-button (item pane)
  "Draw the button part of ITEM on PANE."
  ( :method ((_item tty-menu-item) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (_ button-width _ _ _) layout
	(tty-menu-ninsert button-width ?\s))))
  ( :method ((item tty-menu-button) _pane)
    (insert (tty-menu-button-string item)))
  ( :method ((item tty-menu-separator) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (_ button-width _ _ _) layout
	(tty-menu-ninsert button-width (tty-menu-sep item))))))

(cl-defgeneric tty-menu-draw-name (item pane)
  "Draw the name part of ITEM on PANE."
  ( :method ((item tty-menu-item) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (left-border button name-width _ _) layout
	(with-slots (name) item
	  (insert (tty-menu-name-string item))
	  (indent-to (+ left-border button name-width))))))
  ( :method ((item tty-menu-separator) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (_ _ name-width _ _) layout
	(tty-menu-ninsert name-width (tty-menu-sep item))))))

(cl-defgeneric tty-menu-draw-key (item pane)
  "Draw the key part of ITEM on PANE."
  ( :method ((item tty-menu-item) pane)
    (cl-flet ((right-aligned-p (fmt)
                (not (string-match-p "%-" fmt))))
      (with-slots (layout) pane
        (cl-destructuring-bind (left-border button name-width key-width _)
            layout
          (let ((key (tty-menu-key-string item)))
            (when (and (< (length key) key-width)
                       (right-aligned-p tty-menu-key-format))
              (tty-menu-ninsert (- key-width (length key)) " "))
	    (insert key))
	  (indent-to (+ left-border button name-width key-width))))))
  ( :method ((item tty-menu-separator) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (_ _ _ key-width _) layout
	(tty-menu-ninsert key-width (tty-menu-sep item))))))

(cl-defgeneric tty-menu-draw-finish (item pane)
  "Finish drawing ITEM on PANE."
  ( :method ((item tty-menu-item) _)
    (let* ((enabled (tty-menu-enabled-p item))
	   (face (if enabled 'tty-menu-face
                   'tty-menu-face-disabled)))
      (put-text-property (pos-bol) (pos-eol) 'face face))
    (when-let* ((help (slot-value item 'help)))
      (put-text-property (pos-bol) (pos-eol) 'help-echo help)))
  ( :method ((_item tty-menu-separator) _)
    (put-text-property (pos-bol) (pos-eol) 'face 'tty-menu-face)))

(cl-defgeneric tty-menu-layout (pane)
  "Compute the layout of PANE for drawing items on it.
This set the `layout' of PANE to a list (LEFT-BORDER BUTTON NAME KEY
RIGHT-BORDER), all elements giving the widths to use for the
corresponding columns of a menu item."
  ( :method ((pane tty-menu-pane))
    (with-slots (items layout) pane
      (cl-loop
       with left-border
       = (string-width (format tty-menu-left-border-format ""))
       with right-border
       = (string-width (format tty-menu-right-border-format ""))
       for i in items
       maximize (string-width (tty-menu-button-string i)) into button
       maximize (string-width (tty-menu-name-string i)) into name
       maximize (string-width (tty-menu-key-string i)) into key
       finally (setq layout `(,left-border ,button ,name ,key
                                           ,right-border))))))

(cl-defgeneric tty-menu-frame (thing)
  "Value is the frame associated with THING."
  ( :method ((pane tty-menu-pane))
    (slot-value pane 'frame))
  ( :method ((item tty-menu-item))
    (tty-menu-frame (slot-value item 'pane))))

;; When redrawing a pane, we try to arrange things so that the selection
;; is retained. At least theoretically, it can happen that in the
;; redrawn menu the old selection is no longer selectable. This function
;; finds an alternative selection in this case.
;;
;; Should be removed.
(defun tty-menu-try-place-point (selectable old-line)
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

(cl-defgeneric tty-menu-draw (item pane)
  "Draw ITEM on PANE."
  ( :method ((pane tty-menu-pane) line)
    (with-slots (buffer items) pane
      (with-current-buffer buffer
	(let ((old-line (or line (1- (line-number-at-pos)))))
	  (erase-buffer)
	  (tty-menu-layout pane)
	  (let ((selectable
		 (cl-loop for i in items
			  when (tty-menu-visible-p i)
			  do (tty-menu-draw i pane)
			  and collect (tty-menu-selectable-p i))))
	    (tty-menu-try-place-point selectable old-line))))))
  ( :method :around ((item tty-menu-item) pane)
    (with-slots (draw-start draw-end) item
      (setf draw-start (point))
      (insert (format tty-menu-left-border-format ""))
      (cl-call-next-method)
      (insert (format tty-menu-right-border-format ""))
      (tty-menu-draw-finish item pane)
      (insert ?\n)
      (setf draw-end (point))
      ;; For move movement
      (put-text-property draw-start draw-end 'tty-menu-item item)))
  ( :method ((item tty-menu-item) pane)
    (tty-menu-draw-button item pane)
    (tty-menu-draw-name item pane)
    (tty-menu-draw-key item pane)))

(defvar-local tty-menu-pane-drawn nil
  "The tty-menu-pane drawn in a buffer.")

(cl-defgeneric tty-menu-add (pane item)
  "Add ITEM to PANE."
  ( :method ((pane tty-menu-pane) (item tty-menu-item))
    (with-slots (items) pane
      (let ((last (last items)))
        (when last
          (setf (slot-value (car last) 'next-item) item))
        (setf (slot-value item 'prev-item) (car last))
        (if last
            (setf (cdr last) (list item))
          (setf items (list item)))))))

(defun tty-menu-root-pane (pane)
  "Find the root pane of PANE."
  (while (slot-value pane 'parent-pane)
    (setq pane (slot-value pane 'parent-pane)))
  pane)

(defun tty-menu-set-overlay-face (pane)
  (cl-loop
   for p = pane then (slot-value p 'parent-pane)
   while p do
   (with-slots (overlay) p
     (overlay-put overlay 'face
                  (if (eq p tty-menu-pane-drawn)
                      'tty-menu-face-selected
                    'tty-menu-face-selected-inactive)))))

(cl-defgeneric tty-menu-select (item how)
  "Select ITEM on its pane."
  ( :method ((item tty-menu-item) _how)
    (tty-menu-select (slot-value item 'pane) item))
  ( :method ((pane tty-menu-pane) (item tty-menu-item))
    (setf (slot-value pane 'selected-item) item)
    (with-slots (overlay) pane
      (tty-menu-set-overlay-face pane)
      (with-slots (draw-start draw-end) item
        (move-overlay overlay draw-start draw-end)))))

(cl-defgeneric tty-menu-act (item how)
  "Perform the action associated with ITEM."
  ( :method ((item tty-menu-item) how)
    (when-let* ((enabled (tty-menu-enabled-p item)))
      (with-slots (binding) item
        (if (keymapp binding)
            (throw 'tty-menu-leave (cons item how))
          (throw 'tty-menu-to-top-level (cons item how))))))
  ( :method ((_item tty-menu-separator) _))
  ( :method ((item tty-menu-button) _)
    (with-slots (binding) item
      (when (commandp binding)
	(tty-menu-call-interactively binding))
      (tty-menu-draw tty-menu-pane-drawn nil))))

(cl-defgeneric tty-menu-delete (thing)
  "Delete THING."
  ( :method ((pane tty-menu-pane))
    (with-slots (buffer frame parent-pane child-pane) pane
      (when child-pane
        (tty-menu-delete child-pane))
      (kill-buffer buffer)
      (delete-frame frame)
      (when parent-pane
        (setf (slot-value parent-pane 'child-pane) nil))))
  ( :method ((frame frame))
    (when-let* ((buffer (frame-parameter frame 'tty-menu-buffer))
                (pane (with-current-buffer buffer tty-menu-pane-drawn)))
      (tty-menu-delete pane))))

(defun tty-menu-make-element (pane code item)
  "Construct a new menu element.
PANE is the pane the menu-element is constructed for.  CODE and ITEM are
key-code and menu-item definition from a keymap.  Value is the menu
element constructed."
  (cl-labels ((separator? (name)
                (let ((name (tty-menu-eval name)))
                  (and (stringp name)
                       (string-prefix-p "--" name))))
	      (button? (props)
                (plist-get props :button))
	      (radio? (props)
                (eq (car (button? props)) :radio))
	      (toggle? (props)
                (eq (car (button? props)) :toggle))
              (make (class props)
                (apply #'make-instance class
                       (cl-list*  :pane pane :key-code code props))))
    (pcase-exhaustive item
     ;; (menu-item SEPARATOR-NAME ...)
      (`(menu-item ,(and (pred separator?) name) ,_ . ,props)
      (make 'tty-menu-separator (cl-list* :name name props)))

     ;; (menu-item NAME)
     (`(menu-item ,name)
      (make 'tty-menu-item (list :name name :enable nil)))

     ;; (menu-item NAME BINDING ... :button (:radio ...) ...)
     (`(menu-item ,name ,binding . ,(and (pred radio?) props))
      (make 'tty-menu-radio (cl-list* :name name :binding binding props)))

     ;; (menu-item NAME BINDING ... :button (:toggle ...) ...)
     (`(menu-item ,name ,binding . ,(and (pred toggle?) props))
      (make 'tty-menu-checkbox (cl-list* :name name :binding binding props)))

     ;; (menu-item NAME BINDING ...)
     (`(menu-item ,name ,binding . ,props)
      (make 'tty-menu-item (cl-list* :name name :binding binding props)))

     ;; (SEPARATOR-NAME ...)
     (`(,(and (pred separator?) name) . ,_)
      (make 'tty-menu-separator (list :name name)))

     ;; (NAME KEYMAP)
     (`(,name ,(and (pred keymapp) keymap))
      (make 'tty-menu-item (list :name name :binding keymap)))

     ;; (NAME HELP BINDING)
     (`(,name ,help ,binding)
      (make 'tty-menu-item (list :name name :binding binding :help help)))

     ;; (NAME . BINDING)
     (`(,name . ,binding)
      (make 'tty-menu-item (list :name name :binding binding))))))

(defun tty-menu-create-buffer (pane)
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
	(setq tty-menu-pane-drawn pane)
	(tty-menu-draw pane 0)
	(cl-flet ((line-width ()
		    (save-excursion
		      (goto-char (point-min))
		      (goto-char (pos-eol))
		      (current-column))))
	  (list (current-buffer)
		(line-width)
		(count-lines (point-min) (point-max))))))))

(defun tty-menu-keymap-name (keymap)
  "Return the name of KEYMAP, if any."
  (when (symbolp keymap)
    (setq keymap (indirect-function keymap)))
  (let ((name (last keymap)))
    (and (stringp (car name)) (car name))))

(defun tty-menu-pane-buffer-name (keymap)
  "Make a buffer name for KEYMAP."
  (if-let* ((name (tty-menu-keymap-name keymap)))
      (format " *tty-menu-%s*" name)
    (generate-new-buffer-name " *tty-menu--")))

(defun tty-menu-make-pane (keymap invoking-item frame)
  "Create a `tty-menu-pane'.
KEYMAP is the menu keymap for the pane. INVOLING-ITEM if non-nil is the
menu-item that invoked this menu.  FRAME Is the frame to display the
menu in."
  (let ((pane (make-instance
               'tty-menu-pane
               :keymap keymap
               :invoking-item invoking-item
               :frame frame
	       :buffer (get-buffer-create
                        (tty-menu-pane-buffer-name keymap)))))
    (cl-loop for binding being the key-bindings of keymap
             using (key-codes code)
             for i = (tty-menu-make-element pane code binding)
             do (tty-menu-add pane i))
    pane))

(defvar tty-menu-frame-parameters
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
    (tty-menu-bar-lines . 0)
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

(defun tty-menu-frame-parameters ()
  "Return the frame parameters to use for menu child frames."
  (let ((params (copy-sequence tty-menu-frame-parameters)))
    (when-let* ((fg (face-attribute 'tty-menu-face :foreground))
                ((stringp fg)))
      (setf (alist-get 'foreground-color params) fg))
    (when-let* ((bg (face-attribute 'tty-menu-face :background))
                ((stringp bg)))
      (setf (alist-get 'background-color params) bg))
    params))

(defun tty-menu-make-fully-visible (f1 f2 x y)
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

(defun tty-menu-frame-absolute-position (frame x y)
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

(defun tty-menu-create-frame (keymap where invoking-item)
  (cl-destructuring-bind (parent-frame x y) where
    ;; We want to show the menu using a root frame as parent because
    ;; that doesn't clip the frame. Means that we have to translate
    ;; coordinates to absolute.
    (when (frame-parent parent-frame)
      (cl-destructuring-bind (ax . ay)
	  (tty-menu-frame-absolute-position parent-frame x y)
	(setq x ax y ay)
	(setq parent-frame (frame-root-frame parent-frame))))
    (let* ((minibuffer (minibuffer-window parent-frame))
           (window-min-height 1)
           (window-min-width 1)
           (after-make-frame-functions nil)
	   (frame (make-frame `((parent-frame . ,parent-frame)
				(minibuffer . ,minibuffer)
				,@(tty-menu-frame-parameters))))
	   (win (frame-root-window frame)))
      (let ((pane (tty-menu-make-pane keymap invoking-item frame)))
        (cl-destructuring-bind (buffer width height)
            (tty-menu-create-buffer pane)
	  (modify-frame-parameters frame `((name . ,(buffer-name buffer))
				           (tty-menu-buffer . ,buffer)))
	  (set-window-buffer win buffer)
	  (set-window-dedicated-p win t)
          ;; Don't make the frame absurdly large.
          (setq height (min height
			    (round (/ (frame-height parent-frame) 1.6))))
	  (set-frame-size frame width height)
	  (set-frame-position frame x y)
	  (tty-menu-make-fully-visible parent-frame frame x y)
	  (make-frame-visible frame)
	  (raise-frame frame)
	  frame)))))

;; Debugging aid. Delete all menu frames. Don't delete the buffers, we
;; might want to inspect them.
(defun tty-menu-delete-menu-frames ()
  (interactive)
  (cl-flet ((frame-name (frame)
	      (frame-parameter frame 'name)))
    (cl-loop for frame in (frame-list)
	     when (string-prefix-p " *tty-menu-" (frame-name frame))
	     do (tty-menu-delete frame))))

(defun tty-menu-is-child (child parent)
  "Return t if pane CHILD is a child of PARENT."
  (cl-loop for c = (slot-value parent 'child-pane)
           then (slot-value c 'child-pane)
           while c
           when (eq c child) return t))

(defun tty-menu-cmd-mouse-moved (event)
  "Handle mouse movement in a menu."
  (interactive "e")
  ;; If we moved the mouse in the menu-bar, and we are displaying a menu
  ;; for a menu-bar, and the menu-bar item moved to is different from
  ;; the one we are displaying, close the current menu, and display the
  ;; new one.
  (when (and tty-menu-from-menu-bar
             (eq (posn-area (event-end event)) 'menu-bar))
    (cl-destructuring-bind (x . y) (posn-x-y (event-end event))
      ;; Set current buffer so that we compute things with the right
      ;; menu-bar for that buffer.
      (with-current-buffer tty-menu-updating-buffer
        (when-let* ((new (menu-bar-menu-at-x-y x y menu-updating-frame))
                  (old (menu-bar-menu-at-x-y (car tty-menu-from-menu-bar)
                                             (cdr tty-menu-from-menu-bar)
                                             menu-updating-frame))
                  ((and new (not (eq old new))))
                  (start-x (cdr (menu-bar-item-at-x x))))
          (setq tty-menu-from-menu-bar (posn-x-y (event-end event)))
          (throw 'tty-menu-to-top-level `(menu-bar ,start-x ,y))))))

  (when-let* ((end (event-end event))
	      (win (posn-window end))
              (item (mouse-posn-property end 'tty-menu-item))
	      ((tty-menu-selectable-p item))
              (pane (slot-value item 'pane))
              (frame (slot-value pane 'frame)))
    (tty-menu-select item 'mouse)
    (unless (eq pane tty-menu-pane-drawn)
      (when-let* ((sel (slot-value tty-menu-pane-drawn 'selected-item)))
        (if (tty-menu-is-child pane tty-menu-pane-drawn)
            ;; Move to child -> activate child.
            (tty-menu-act sel 'mouse)
          (throw 'tty-menu-leave nil))))))

(defun tty-menu-cmd-mouse-act (event)
  "Perform action on selected menu-item."
  (interactive "e")
  (when-let* ((end (event-end event))
	      (win (posn-window end))
              ((or (eq win (selected-window))
                   (throw 'tty-menu-to-top-level nil)))
              (item (mouse-posn-property end 'tty-menu-item))
	      ((tty-menu-selectable-p item)))
    (goto-char (posn-point end))
    (tty-menu-act item 'mouse)))

(defun tty-menu-cmd-key-act ()
  "Perform the action associated with a menu-item."
  (interactive)
  (let* ((selected (slot-value tty-menu-pane-drawn 'selected-item)))
    (if selected
        (tty-menu-act selected 'key)
      ;; macOS closes the menu if no menu-item is selected.
      (throw 'tty-menu-leave nil))))

(defun tty-menu-cmd-next-item ()
  "Move to next selectable menu-item."
  (interactive)
  (let* ((pane tty-menu-pane-drawn)
         (items (slot-value pane 'items))
         (selected (slot-value pane 'selected-item))
         (next (if selected
                   (slot-value selected 'next-item)
                 (cl-first items))))
    (while next
      (if (tty-menu-selectable-p next)
          (progn
            (tty-menu-select next nil)
            (setq next nil))
        (setq next (slot-value next 'next-item))))))

(defun tty-menu-cmd-previous-item ()
  "Move to previous selectable menu-item."
  (interactive)
  (let* ((pane tty-menu-pane-drawn)
         (selected (slot-value pane 'selected-item))
         (prev (and selected (slot-value selected 'prev-item))))
    (while prev
      (if (tty-menu-selectable-p prev)
          (progn
            (tty-menu-select prev nil)
            (setq prev nil))
        (setq prev (slot-value prev 'prev-item))))))

(defun tty-menu-bar-layout (buffer)
  "Compute the layout of the menu-bar of buffer BUFFER.
Value is a list of (KEY-CODE KEYMAP X0 X1) where KEY-CODE Is the
KEY-CODE of the menu-item, e.g. `edit', and KEYMAP is the associated
menu keymap. X0 and X1 are the start end end column of the menu-item."
  (with-current-buffer buffer
    (when-let* ((menu-bar (menu-bar-keymap)))
      (cl-loop
       with column = 0 and layout = nil
       for code being the key-codes of menu-bar
       using (key-bindings binding) do
       ;; This is something like ("Edit" . KEYMAP)
       (pcase binding
         ((or `(,(and (pred stringp) name) . ,cmd)
              `(menu-item
                ,name ,cmd
                . ,(and props
                        (guard (let ((visible (plist-get props :visible)))
                                 (or (null visible) (eval visible)))))))
          (let ((start-column column))
            (cl-incf column (1+ (length name)))
            (push (list code cmd start-column column) layout))))
       finally return (nreverse layout)))))

(defun tty-menu-bar-find-pane (layout pane)
  "Find PANE in the menu-bar layout LAYOUT.
Return the index of of its entry in LAYOUT. Value is nil if not found,
but that should not happen."
  (cl-loop
   with keymap = (slot-value pane 'keymap)
   for index from 0
   for elem in layout
   for (_code binding _x0 _x1) = elem
   when (eq binding keymap) return index))

(defun tty-menu-move-in-menu-bar (move-left)
  "Arrange to move to another item in the menu-bar.
MOVE-LEFT non-nil means move to the previous item.  This
gets the selected tty-menu-item and determines the pane it is
in. If that is a top-level pane, not a sub-menu, see where that pane
is in the menu-bar. Then determine the next/previous menu-bar item,
and make us display that menu."
  (when-let* (((not (null tty-menu-from-menu-bar)))
              (root-pane (tty-menu-root-pane tty-menu-pane-drawn))
              (layout (tty-menu-bar-layout tty-menu-updating-buffer))
              (index (tty-menu-bar-find-pane layout root-pane))
              (n (length layout)))
    (cond (move-left
           (cl-decf index)
           (when (< index 0)
             (setq index (1- n))))
          (t
           (cl-incf index)
           (when (>= index n)
             (setq index 0))))
    (cl-destructuring-bind (_ _ x0 _) (nth index layout)
      (throw 'tty-menu-to-top-level `(menu-bar ,x0 0)))))

(defun tty-menu-selected-item ()
  (slot-value tty-menu-pane-drawn 'selected-item))

(defun tty-menu-cmd-open ()
  "Select a menu-item with <right> if it is for a sub-menu."
  (interactive)
  (when-let* ((item (tty-menu-selected-item)))
    (with-slots (binding) item
      (when (keymapp binding)
	(tty-menu-act item 'key))))
  (tty-menu-move-in-menu-bar nil))

(defun tty-menu-cmd-close ()
  "Close current menu pane with <left>."
  (interactive)
  ;; If this is not a top-level pane, close it.
  (when-let* ((item (tty-menu-selected-item))
              (pane (slot-value item 'pane))
              ((slot-value pane 'invoking-item)))
    (throw 'tty-menu-leave nil))
  ;; If it is a top-level plane, either move left in the menu-bar or
  ;; close it.
  (tty-menu-move-in-menu-bar 'left)
  (throw 'tty-menu-leave nil))

(defun tty-menu-isearch (forward)
  "Isearch in a menu, FORWARD t means search forward."
  (isearch-mode forward nil nil)
  (while isearch-mode
    (let* ((key (read-key-sequence nil nil t nil nil nil)))
      (when-let* ((cmd (lookup-key isearch-mode-map key)))
	(if (and (eq cmd #'isearch-printing-char)
		 (stringp key))
	    (isearch-printing-char (aref key 0))
	  (call-interactively cmd)))))
  (let* ((items (slot-value tty-menu-pane-drawn 'items))
         (i (1- (line-number-at-pos (point) t)))
         (sel (nth i items)))
    (while (and sel (not (tty-menu-selectable-p sel)))
      (setf sel (if forward
                    (slot-value sel 'next-item)
                  (slot-value sel 'prev-item))))
    (when sel
      (tty-menu-select sel 'key))))

(defun tty-menu-cmd-isearch-forward ()
  "Isearch forward in a menu."
  (interactive)
  (tty-menu-isearch t))

(defun tty-menu-cmd-isearch-backward ()
  "Isearch backward in a menu."
  (interactive)
  (tty-menu-isearch nil))

(defun tty-menu-cmd-menu-bar-click (_event)
  "Handle click in a menu-bar while a menu is open."
  (interactive "e")
  (throw 'tty-menu-leave nil))

(defun tty-menu-cmd-close-on-click (_event)
  "Close one menu-pane."
  (interactive "e")
  (throw 'tty-menu-to-top-level nil))

(defvar-keymap tty-menu-keymap
  :doc "Keymap for menu interaction."
  "<up>" #'tty-menu-cmd-previous-item
  "<down>" #'tty-menu-cmd-next-item
  "<left>" #'tty-menu-cmd-close
  "<right>" #'tty-menu-cmd-open
  "C-b" #'tty-menu-cmd-close
  "C-f" #'tty-menu-cmd-open
  "C-g" #'keyboard-quit
  "C-j" #'tty-menu-cmd-key-act
  "C-n" #'tty-menu-cmd-next-item
  "C-p" #'tty-menu-cmd-previous-item
  "C-r" #'tty-menu-cmd-isearch-backward
  "C-s" #'tty-menu-cmd-isearch-forward
  "RET" #'tty-menu-cmd-key-act
  "SPC" #'tty-menu-cmd-key-act
  "<return>" #'tty-menu-cmd-key-act
  "<mouse-movement>" #'tty-menu-cmd-mouse-moved
  "<menu-bar> <mouse-1>" #'tty-menu-cmd-menu-bar-click
  "<tab-bar> <mouse-1>" #'tty-menu-cmd-close-on-click
  "<vertical-line> <mouse-1>" #'tty-menu-cmd-close-on-click
  "<mode-line> <mouse-1>" #'tty-menu-cmd-close-on-click
  "<mouse-1>" #'tty-menu-cmd-mouse-act)

(defun tty-menu-position (pos)
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

(defun tty-menu-where (selected how)
  "Determine where to display SELECTED,
HOW is either `key' or `mouse' and specifies how the menu
invocation takes place."
  (cl-ecase how
    (key
     (let* ((frame (tty-menu-frame selected))
            (end (slot-value selected 'draw-end))
	    (win (frame-root-window frame))
            (posn (posn-at-point (1- end) win)))
       (cl-destructuring-bind (x . y) (posn-x-y posn)
         (list frame (- x 3) y))))
     (mouse
      (let* ((posn (posn-at-point (pos-eol)))
	    (win (posn-window posn)))
       (cl-destructuring-bind (x . y) (posn-x-y posn)
         (tty-menu-position (list (cons (- x 3) y) win)))))))

(defun tty-menu-select-item-by-name ()
  "Select a menu-item from `this-command-keys'."
  (when-let* ((key (this-command-keys))
              ((stringp key))
              (pane tty-menu-pane-drawn)
              (items (slot-value pane 'items)))
    (cl-flet ((matchp (i)
                (when (tty-menu-selectable-p i)
                  (let ((name (string-trim-left (slot-value i 'name))))
                    (string-prefix-p key name t)))))
      (let* ((current (slot-value pane 'selected-item))
             (pos (if current (cl-position current items) 0)))
        (cl-loop with n = (length items)
                 repeat n
                 for i = (mod (1+ pos) n) then (mod (1+ i) n)
                 for item = (nth i items)
                 when (matchp item)
                 do (tty-menu-select item 'key)
                 and return t)))))

(defun tty-menu-open-on-pane (item)
  "Return t if ITEM's pane has a sub-menu open."
  (when-let* ((pane (slot-value item 'pane))
              (open (slot-value pane 'child-pane)))
    (slot-value open 'invoking-item)))

(defun tty-menu-open-sub-menus-on-selection (previous selected)
  ;; (message "sel %S -> %S"
  ;;          (when previous
  ;;            (slot-value previous 'name))
  ;;          (when selected
  ;;            (slot-value selected 'name)))
  (let ((prev (and previous (slot-value previous 'binding)))
        (sel (and selected (slot-value selected 'binding))))
    (pcase-exhaustive (cons prev sel)
      ;; nil -> nil
      (`(nil . nil))

      ;; nil -> command
      (`(nil . ,(pred commandp _)))

      ;; nil -> keymap
      (`(nil . ,(pred keymapp _))
       (throw 'tty-menu-leave (cons selected 'key)))

      ;; command -> nil
      (`(,(pred commandp _) . nil) nil)

      ;; command -> command
      (`(,(pred commandp _) . ,(pred commandp _)))

      ;; command -> keymap
      (`(,(pred commandp _) . ,(pred keymapp _))
       (throw 'tty-menu-leave (cons selected 'key)))

      ;; keymap -> nil
      (`(,(pred keymapp _) . nil)
       (throw 'tty-menu-leave 'close-sub-menu))

      ;; keymap -> command
      (`(,(pred keymapp _) . ,(pred commandp _))
       (throw 'tty-menu-leave 'close-sub-menu))

      ;; keymap -> keymap
      (`(,(pred keymapp _) . ,(pred keymapp _))
       (throw 'tty-menu-leave (cons selected 'key))))))

(defun tty-menu-command-loop ()
  (catch 'tty-menu-leave
    (let ((previous-selected (tty-menu-selected-item)))
      (while t
        (let* ((selected (tty-menu-selected-item)))
          (unless (eq selected previous-selected)
            (tty-menu-open-sub-menus-on-selection previous-selected selected)
            (setq previous-selected selected)))

        (let* ((track-mouse t)
	       (key (read-key-sequence nil))
	       (cmd (lookup-key tty-menu-keymap key)))
          ;; Entering a character that is self-inserting
          ;; in global-map searches for a menu-item
          ;; beginning with that character.
          (when-let* (((not (commandp cmd)))
                      (cmd (lookup-key global-map key))
                      ((eq cmd 'self-insert-command)))
            (tty-menu-select-item-by-name))
          ;; Otherwise execute a command, if any.
          ;; This is for toggling buttons, moving on
          ;; the menu and so on.
	  (when (commandp cmd)
	    (call-interactively cmd)))))))

(defun tty-menu-after-command-loop (res frame)
  (let ((use-frame frame))
    (pcase-exhaustive res
      (`(,selected . ,(and (pred symbolp) how))
       (with-slots (binding) selected
         (cond
          ((keymapp binding)
           ;;(message "  open %S" (slot-value selected 'name))
           (let ((open (tty-menu-open-on-pane selected)))
             (cond
              ((eq open selected)
               (select-frame-set-input-focus frame))
              ((null open)
	       (tty-menu-loop-1 binding
                                (tty-menu-where selected how)
                                :invoking-item selected
                                :focus nil))
              (t
               (tty-menu-delete frame)
               (setq use-frame
                     (tty-menu-create-frame
                      binding
                      (tty-menu-where selected how)
                      selected))))))
          (t (throw 'tty-menu-item-close selected)))))
      (`close-sub-menu
       (when-let* ((child (slot-value tty-menu-pane-drawn 'child-pane)))
         (tty-menu-delete child)))
      ('nil
       (throw 'tty-menu-item-close nil)))
    use-frame))

(cl-defun tty-menu-loop-1 (keymap where &key invoking-item (focus t))
  (let ((frame (tty-menu-create-frame keymap where invoking-item)))
    (unwind-protect
        (catch 'tty-menu-item-close
          (when focus
            (select-frame-set-input-focus frame))
          (while t
            (let* ((res (tty-menu-command-loop)))
              (setq frame (tty-menu-after-command-loop res frame))
              (tty-menu-set-overlay-face tty-menu-pane-drawn))))
      (tty-menu-delete frame))))

(defun tty-menu-loop (keymap where)
  "Event loop for tty menus.
KEYMAP is a menu keymap, and WHERE specifies where to open the menu."
  (let ((res (catch 'tty-menu-to-top-level
               (save-selected-window
                 (tty-menu-loop-1 keymap where :focus t)))))
    (pcase-exhaustive res
      ('nil nil)
      (`(menu-bar ,x ,y) (cons x y))
      (`(,selected . ,_) selected))))

(cl-defun tty-menu-popup-menu (position menu)
  "This is the replacement for `x-popup-menu'.
It is installed as `x-popup-menu-function' when using
`tty-menu-mode'."
  (when-let* ((where (tty-menu-position position)))
    (cl-destructuring-bind (menu-updating-frame _ _) where
      (let ((tty-menu-updating-buffer (current-buffer)))
        (cond ((keymapp menu)
               ;; Run the event loop, and at the end, collect bindings
               ;; for nested menus involved to form the result event.
               (cl-loop for i = (tty-menu-loop menu where)
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
		        for name = (tty-menu-keymap-name keymap)
		        do (define-key outer (vector (intern name)) keymap)
		        finally (tty-menu-loop outer where)))
	      (t (error "Not a menu: %S" menu)))))))

(defun tty-menu-around-mouse-set-point (old-fun &rest args)
  "Around advice for `mouse-set-point',
A mouse-click in a menu can lead to one or more frames for menu panes
being deleted. Somehow, such a click event survives the frame deletions,
and lands in mouse-set-point with the event containing a window on such
a deleted frame. I couldn't find the reason for that.  So this is an
advice added to handle that."
  (let* ((event (car args))
         (win (posn-window (event-start event))))
    (when (or (not (windowp win)) (window-live-p win))
      (apply old-fun args))))

(defun tty-menu-around-popup-menu (old-fun &rest args)
  "Around advice for `popup-menu'.
This around advice for `popup-menu' binds `tty-menu-from-menu-bar' to
(X . Y) in the menu-bar when invoked for a menu-bar menu, and nil
otherwise. This makes it possible to behave differently for menus in
the menu-bar and others."
  (let ((tty-menu-from-menu-bar
         (pcase args
           (`(,_ ,pos ,_ ,(and menu-bar (guard menu-bar)))
            (posn-x-y pos)))))
    (apply old-fun args)))

(defun tty-menu-buffer-menu-open ()
  "Override for `buffer-menu-open'.
Opens the menu at point instead of at (0, 0)."
  (popup-menu (mouse-buffer-menu-keymap) (posn-at-point)))

;;;###autoload
(define-minor-mode tty-menu-mode
  "Global minor mode for displaying menus with tty child frames.
\\{tty-menu-keymap}
Entering a self-inserting character goes to the next menu-item starting
with that character.  When no next items is found, start searching from
the start."
  :global t :group 'menu
  (unless (display-graphic-p)
    (cond (tty-menu-mode
           (advice-add 'buffer-menu-open :override #'tty-menu-buffer-menu-open)
           (advice-add 'mouse-set-point :around #'tty-menu-around-mouse-set-point)
           (advice-add 'popup-menu :around #'tty-menu-around-popup-menu)
           (setq x-popup-menu-function #'tty-menu-popup-menu))
          (t
           (advice-remove 'buffer-menu-open #'tty-menu-buffer-menu-open)
           (advice-remove 'mouse-set-point #'tty-menu-around-mouse-set-point)
           (advice-remove 'popup-menu #'tty-menu-around-popup-menu)
           (setq x-popup-menu-function nil)))))

(provide 'tty-menu)
