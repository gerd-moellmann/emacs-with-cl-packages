;; -*- lexical-binding: t; symbol-packages: t -*-
;;; tty-menu.el --- A menu implementation in Lisp

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

;;; Code:

;;; Todo

;; A mode: Open a sub-menu by moving the selection into the menu-item for the
;; sub-menu.  Close it by moving the selection out.

;; Make cursor invisible somehow (is cursor-type not supposed to work?)

;; menu-bar-menu doesn't work (list of keymaps?)

(require 'cl-lib)

(defclass tty-menu-element () ()
  :documentation "Base class for elements of a menu.")

(defclass tty-menu-pane (tty-menu-element)
  ((items :initarg :items :initform nil :type t)
   (buffer :initarg :buffer :type buffer)
   (layout :type list)
   (invoking-item :initarg :invoking-item :type (or null tty-menu-item)))
  :documentation "Class for menu panes.")

(defclass tty-menu-item (tty-menu-element)
  (;; type t because even the name can be a form that needs to be
   ;; evaluated to get the name (Redo is an example in the context
   ;; menu).
   (name :initarg :name :type t)
   (enable :initarg :enable :initform t :type t)
   (help :initarg :help :initform nil :type t)
   (visible :initarg :visible :initform t :type t)
   (key-sequence :initarg :key-sequence :initform nil :type t)
   (keys :initarg :keys :initform nil :type t)
   (filter :initarg :filter :initform nil :type t)
   (button :initarg :button :initform nil :type t)
   (selected :initarg :selected :initform nil :type t)
   (binding :initarg :binding :initform nil :type t)
   (key-code :initarg :key-code :initform nil :type t)
   (pane :initarg :pane :type tty-menu-pane)))

(defclass tty-menu-button (tty-menu-item) ())
(defclass tty-menu-radio (tty-menu-button) ())
(defclass tty-menu-checkbox (tty-menu-button) ())

(defclass tty-menu-separator (tty-menu-item)
  ((sep :initform "-" :type string :reader tty-menu-sep)))

(defun tty-menu-get-separator-string (name)
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

(cl-defmethod initialize-instance :after ((item tty-menu-separator)
                                          &rest)
  (with-slots (name sep enable) item
    (setf enable nil)
    (setf sep (tty-menu-get-separator-string name))))

(cl-defmethod initialize-instance :after ((item tty-menu-item)
                                          &rest)
  (with-slots (binding enable) item
    (when (and (keymapp binding)
               (zerop (cl-loop for b being the key-codes of binding
                               count b)))
      (setf enable nil))))

(defvar tty-menu-left-border-format "%1s")
(defvar tty-menu-right-border-format "%1s")
(defvar tty-menu-button-format "%-4s")
(defvar tty-menu-key-format "%10s")
(defvar tty-menu-name-format "%-20s")

(defvar tty-menu-triangle "▶")
(defvar tty-menu-radio-on "●")
(defvar tty-menu-radio-off "◯")
(defvar tty-menu-checkbox-on "✔")
(defvar tty-menu-checkbox-off "□")

(defun tty-menu-selectable-p (item)
  (eval (slot-value item 'enable)))

(defun tty-menu-visible-p (item)
  (eval (slot-value item 'visible)))

(defun tty-menu-enabled-p (item)
  (eval (slot-value item 'enable)))

(defun tty-menu-ninsert (n x)
  (cl-loop repeat n do (insert x)))

(cl-defgeneric tty-menu-button-string (item)
  ( :method ((_ tty-menu-item))
    "")
  ( :method ((r tty-menu-radio))
    (format tty-menu-button-format
	    (if (slot-value r 'selected)
		tty-menu-radio-on
	      tty-menu-radio-off)))
  ( :method ((c tty-menu-checkbox))
    (format tty-menu-button-format
	    (if (slot-value c 'selected)
		tty-menu-checkbox-on
	      tty-menu-checkbox-off)))
  ( :method ((_ tty-menu-separator))
    ""))

(cl-defgeneric tty-menu-name-string (item)
  ( :method ((item tty-menu-item))
    (with-slots (name) item
      (format tty-menu-name-format (eval name))))
  ( :method ((_ tty-menu-separator))
    ""))

(cl-defgeneric tty-menu-key-string (item)
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
  ( :method ((_item tty-menu-item) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (_ button-width _ _ _) layout
	(tty-menu-ninsert button-width ?\s))))
  ( :method ((item tty-menu-button) _pane)
    (with-slots (selected button) item
      (cl-destructuring-bind (_ . form) button
	(setf selected (eval form))
	(insert (tty-menu-button-string item)))))
  ( :method ((item tty-menu-separator) pane)
    (with-slots (layout) pane
      (cl-destructuring-bind (_ button-width _ _ _) layout
	(tty-menu-ninsert button-width (tty-menu-sep item))))))

(cl-defgeneric tty-menu-draw-name (item pane)
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
  ( :method ((item tty-menu-item) _)
    (let* ((enabled (tty-menu-enabled-p item))
	   (face (if enabled 'tty-menu-enabled-face
                   'tty-menu-disabled-face)))
      (put-text-property (pos-bol) (pos-eol) 'tty-menu-selectable
                         enabled)
      (put-text-property (pos-bol) (pos-eol) 'face face))
    (when-let* ((help (slot-value item 'help)))
      (put-text-property (pos-bol) (pos-eol) 'help-echo help)))
  ( :method ((_item tty-menu-separator) _)
    (put-text-property (pos-bol) (pos-eol) 'tty-menu-selectable nil)
    (put-text-property (pos-bol) (pos-eol) 'face 'tty-menu-enabled-face)))

(cl-defgeneric tty-menu-layout (pane)
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
    (let ((start (point)))
      (insert (format tty-menu-left-border-format ""))
      (cl-call-next-method)
      (insert (format tty-menu-right-border-format ""))
      (tty-menu-draw-finish item pane)
      (insert ?\n)
      (put-text-property start (point) 'tty-menu-item item)))
  ( :method ((item tty-menu-item) pane)
    (tty-menu-draw-button item pane)
    (tty-menu-draw-name item pane)
    (tty-menu-draw-key item pane)))

(defvar-local tty-menu-selection-ov nil)
(defvar-local tty-menu-pane-drawn nil)

(cl-defgeneric tty-menu-select-item (item how)
  ( :method ((item tty-menu-item) how)
    (when-let* ((enabled (tty-menu-enabled-p item)))
      (with-slots (binding) item
        (if (keymapp binding)
            (throw 'tty-menu-item-selected (cons item how))
          (throw 'tty-menu-final-item-selected (cons item how))))))
  ( :method ((_item tty-menu-separator) _))
  ( :method ((item tty-menu-button) _)
    (with-slots (binding) item
      (when (commandp binding)
	(call-interactively binding))
      (tty-menu-draw tty-menu-pane-drawn nil))))

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
	(setq tty-menu-selection-ov nil tty-menu-pane-drawn pane)
	(tty-menu-draw pane 0)
	(cl-flet ((line-width ()
		    (save-excursion
		      (goto-char (point-min))
		      (goto-char (line-end-position))
		      (current-column))))
	  (list (current-buffer)
		(line-width)
		(count-lines (point-min) (point-max))))))))

(defun tty-menu-make-element (pane code item)
  (cl-labels ((separator? (name)
                (let ((name (eval name)))
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
      (`(menu-item ,(and (pred separator?) name) . ,props)
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

(defun tty-menu-keymap-name (keymap)
  (when (symbolp keymap)
    (setq keymap (symbol-function keymap)))
  (let ((name (last keymap)))
    (and (stringp (car name)) (car name))))

(defun tty-menu-make-pane-buffer (keymap invoking-item)
  (cl-labels ((pane-buffer-name ()
	        (if-let* ((name (tty-menu-keymap-name keymap)))
	            (format " *tty-menu-%s*" name)
	          (generate-new-buffer-name " *tty-menu--")))
              (make-pane (keymap)
                (let ((pane (make-instance
                             'tty-menu-pane
                             :invoking-item invoking-item
	                     :buffer (get-buffer-create (pane-buffer-name)))))
                  (with-slots (items) pane
                    (setq items
                          (cl-loop for binding being the key-bindings of keymap
                                   using (key-codes code)
		                   collect (tty-menu-make-element
                                            pane code binding))))
                  pane)))
    (tty-menu-create-buffer (make-pane keymap))))

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
  (let ((params (copy-sequence tty-menu-frame-parameters)))
    (when-let* ((fg (face-attribute 'tty-menu-enabled-face :foreground))
                ((stringp fg)))
      (setf (alist-get 'foreground-color params) fg))
    (when-let* ((bg (face-attribute 'tty-menu-enabled-face :background))
                ((stringp bg)))
      (setf (alist-get 'background-color params) bg))
    params))

(defun tty-menu-make-fully-visible (f1 f2 x y)
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
  "Translate (X, Y) in FRAME to absolute coordinates relative to the root frame."
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
           (menu-updating-frame frame)
	   (win (frame-root-window frame)))
      (cl-destructuring-bind (buffer width height)
          (tty-menu-make-pane-buffer keymap invoking-item)
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
	(select-frame-set-input-focus frame)
	frame))))

(defun tty-menu-delete-frame (frame)
  ;; During development, sometimes the frame can be null.
  (when frame
    (kill-buffer (frame-parameter frame 'tty-menu-buffer))
    (delete-frame frame)))

(defun tty-menu-mouse-moved (event)
  (interactive "e")
  (when-let* ((end (event-end event))
	      (win (posn-window end))
              ((eq win (selected-window)))
              (item (mouse-posn-property end 'tty-menu-item))
	      ((tty-menu-selectable-p item)))
    (goto-char (posn-point end))
    item))

(defun tty-menu-mouse-select-item (event)
  (interactive "e")
  (when-let* ((end (event-end event))
	      (win (posn-window end))
              ((or (eq win (selected-window))
                   (throw 'tty-menu-item-selected nil)))
              (item (mouse-posn-property end 'tty-menu-item))
	      ((tty-menu-selectable-p item)))
    (goto-char (posn-point end))
    (tty-menu-select-item item 'mouse)))

(defun tty-menu-key-select-item ()
  (interactive)
  (when-let* ((item (get-text-property (point) 'tty-menu-item)))
    (tty-menu-select-item item 'key)))

(defun tty-menu-key-select-item-if-subpane ()
  (interactive)
  (when-let* ((item (get-text-property (point) 'tty-menu-item)))
    (with-slots (binding) item
      (when (keymapp binding)
	(tty-menu-select-item item 'key)))))

(defun tty-menu-next-line ()
  (interactive)
  (cl-loop for next = (next-single-property-change (point) 'tty-menu-item)
	   then (next-single-property-change next 'tty-menu-item)
	   while next
	   for item = (get-text-property next 'tty-menu-item)
	   until (tty-menu-selectable-p item)
	   finally (when next (goto-char next))))

(defun tty-menu-previous-line ()
  (interactive)
  (cl-loop for prev = (previous-single-property-change
		       (point) 'tty-menu-item nil (point-min))
	   then (previous-single-property-change
		 prev 'tty-menu-item nil (point-min))
	   while prev
	   for item = (get-text-property prev 'tty-menu-item)
	   if (tty-menu-selectable-p item) do (goto-char prev) and return t
	   else if (eq prev (point-min)) return t))

(defun tty-menu-close-pane ()
  (interactive)
  (throw 'tty-menu-item-selected nil))

(defun tty-menu-isearch (forward)
  (isearch-mode forward nil nil)
  (while isearch-mode
    (let* ((key (read-key-sequence nil nil t nil nil nil)))
      (when-let* ((cmd (lookup-key isearch-mode-map key)))
	(if (and (eq cmd #'isearch-printing-char)
		 (stringp key))
	    (isearch-printing-char (aref key 0))
	  (call-interactively cmd))))))

(defun tty-menu-isearch-forward ()
  (interactive)
  (tty-menu-isearch t))

(defun tty-menu-isearch-backward ()
  (interactive)
  (tty-menu-isearch nil))

(defun tty-menu-menu-bar-click (_event)
  (interactive "e")
  (throw 'tty-menu-item-selected nil))

(defun tty-menu-close-on-click (_event)
  (interactive "e")
  (tty-menu-close-pane))

(defvar-keymap tty-menu-keymap
  :doc "Keymap for menu interaction."
  "<up>" #'tty-menu-previous-line
  "<down>" #'tty-menu-next-line
  "<left>" #'tty-menu-close-pane
  "<right>" #'tty-menu-key-select-item-if-subpane
  "C-b" #'tty-menu-close-pane
  "C-g" #'keyboard-quit
  "C-j" #'tty-menu-key-select-item
  "C-n" #'tty-menu-next-line
  "C-p" #'tty-menu-previous-line
  "C-r" #'tty-menu-isearch-backward
  "C-s" #'tty-menu-isearch-forward
  "RET" #'tty-menu-key-select-item
  "SPC" #'tty-menu-key-select-item
  "<return>" #'tty-menu-key-select-item
  "<mouse-movement>" #'tty-menu-mouse-moved
  "<menu-bar> <mouse-1>" #'tty-menu-menu-bar-click
  "<tab-bar> <mouse-1>" #'tty-menu-close-on-click
  "<vertical-line> <mouse-1>" #'tty-menu-close-on-click
  "<mode-line> <mouse-1>" #'tty-menu-close-on-click
  "<mouse-1>" #'tty-menu-mouse-select-item)

(defun tty-menu-show-selected-item ()
  (unless tty-menu-selection-ov
    (setq tty-menu-selection-ov (make-overlay 1 1))
    (overlay-put tty-menu-selection-ov 'face 'tty-menu-selected-face))
  (move-overlay tty-menu-selection-ov (line-beginning-position) (line-end-position)))

(defun tty-menu-global-menu ()
  (keymap-lookup global-map "<menu-bar>"))

(defun tty-menu-position (pos)
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

(defun tty-menu-where (how)
  (cl-ecase how
    ((mouse key)
     (let* ((posn (posn-at-point (line-end-position)))
	    (win (posn-window posn)))
       (cl-destructuring-bind (x . y) (posn-x-y posn)
         (tty-menu-position (list (cons (- x 3) y) win)))))))

(defun tty-menu-loop-1 (keymap where invoking-item)
  (let ((frame (tty-menu-create-frame keymap where invoking-item)))
    (unwind-protect
	;; Inner loop handling mouse movement over the pane, moving with
	;; the keyboard on the pane. The loop is left by a throw when a
	;; menu-item is selected.
	(cl-loop
         named outer-loop
	 while t
	 for res = (catch 'tty-menu-item-selected
		     (while t
		       (tty-menu-show-selected-item)
		       (let* ((track-mouse t)
			      (key (read-key-sequence nil))
			      (cmd (lookup-key tty-menu-keymap key)))
			 (when (commandp cmd)
			   (call-interactively cmd)))))
	 do
	 ;; If the selected item was for a sub-pane, call ourselves
	 ;; recursively with the sub-pane.
	 (pcase-exhaustive res
           (`(,selected . ,(and (pred symbolp) how))
	    (with-slots (binding) selected
	      (if (keymapp binding)
		  (tty-menu-loop-1 binding (tty-menu-where how) selected)
		(cl-return-from outer-loop selected))))
           ('nil
	    (cl-return-from outer-loop nil))))
      (tty-menu-delete-frame frame))))

(defun tty-menu-loop (keymap where)
  (let ((res (catch 'tty-menu-final-item-selected
               (save-selected-window
                 (tty-menu-loop-1 keymap where nil)))))
    (pcase-exhaustive res
      ('nil nil)
      (`(,selected . ,_) selected))))

(defun tty-menu-delete-menu-frames ()
  (cl-flet ((frame-name (frame)
	      (frame-parameter frame 'name)))
    (cl-loop for frame in (frame-list)
	     when (string-prefix-p " *tty-menu-" (frame-name frame))
	     do (delete-frame frame))))

(cl-defun tty-menu-popup-menu (position menu)
  (when-let* ((where (tty-menu-position position)))
    (cond ((keymapp menu)
           (cl-loop for i = (tty-menu-loop menu where)
                    then (with-slots (pane) i
                           (with-slots (invoking-item) pane
                             invoking-item))
                    while i
                    collect (slot-value i 'key-code) into codes
                    finally return (nreverse codes)))
	  ((consp menu)
	   (cl-loop with outer = (make-sparse-keymap "outer")
		    for keymap in menu
		    for name = (tty-menu-keymap-name keymap)
		    do (define-key outer (vector (intern name)) keymap)
		    finally (tty-menu-loop outer where)))
	  (t (error "Not a menu: %S" menu)))))

(defun tty-menu-around-mouse-set-point (old-fun &rest args)
  (let* ((start (event-start (car args)))
         (win (posn-window start)))
    (when (or (not (windowp win)) (window-live-p win))
      (apply old-fun args))))

;;;###autoload
(define-minor-mode tty-menu-mode
  "Global minor mode for displaying menus with tty child frames."
  :global t :group 'menu
  (unless (display-graphic-p)
    (cond (tty-menu-mode
           (advice-add 'mouse-set-point :around #'tty-menu-around-mouse-set-point)
           (setq x-popup-menu-function #'tty-menu-popup-menu))
          (t
           (advice-remove 'mouse-set-point #'tty-menu-around-mouse-set-point)
           (setq x-popup-menu-function nil)))))

(provide 'tty-menu)
