;;; mac-transparent-titlebar-tab-bar-tests.el --- Tests for interactions between tranparent title bar and native tab-bar -*- lexical-binding:t -*-

;;; Commentary:
;;
;; This tests are designed to run with a visible frame (a GUI window),
;; as they verify native macOS tab-bar functionality while
;; `mac-transparent-titlebar' is enabled.  As such there's not
;; implemeted as ert style tests per se, i.e. not a series of
;; `deftest's, but rather this file should be evaled as a script, e.g.,
;;
;;   emacs -Q -load mac-transparent-titlebar-tab-bar-tests.el

;;; Code:

(require 'ert)

(defun wait-for (secs)
  "Wait for SECS and force redisplaying the window."
  (let ((until (+ (float-time) secs))
        (interval (/ secs 20.0)))
    (while (< (float-time) until)
      (sleep-for interval)
      (force-window-update)
      (redisplay))))

;; Need some time for the frame to be fully drawn in the screen.
(wait-for 5)

(setq debug-on-error t)

(defvar frame-stack nil)

;; (0) Enable transparency on current frame and all new frames
(set-frame-parameter nil 'mac-transparent-titlebar t)
(add-to-list 'default-frame-alist '(mac-transparent-titlebar . t))

;; (1) Test the initial frame
(push (should
       (mac-frame-tab-group-property nil :selected-frame))
      frame-stack)
(should (frame-parameter nil 'mac-transparent-titlebar))
(should (eql 1
             (length (mac-frame-tab-group-property nil :frames))))
(should-not (mac-frame-tab-group-property nil :tab-bar-visible-p))
(should-not (mac-frame-tab-group-property nil :overview-visible-p))

;; toggle tab-bar on and off
(mac-set-frame-tab-group-property nil :tab-bar-visible-p t)
(should (mac-frame-tab-group-property nil :tab-bar-visible-p))
(mac-set-frame-tab-group-property nil :tab-bar-visible-p nil)
(should-not (mac-frame-tab-group-property nil :tab-bar-visible-p))

;; TODO: need to find a way to exit overview programatically
;; ;; toggle overview on and off
;; (mac-set-frame-tab-group-property nil :tab-bar-visible-p t)
;; (should (mac-frame-tab-group-property nil :tab-bar-visible-p))
;; (should
;;  (progn
;;    (mac-set-frame-tab-group-property nil :overview-visible-p t)
;;    (mac-frame-tab-group-property nil :overview-visible-p)))
;; ;; when one tab is present tab-bar is hidden after exiting overview
;; (should-not (mac-frame-tab-group-property nil :tab-bar-visible-p))

;; (2) Open a new frame in a new tab and test it
(mac-set-frame-tab-group-property nil :tab-bar-visible-p t)
(should (mac-frame-tab-group-property nil :tab-bar-visible-p))
;; Need some time for tab bar to be drawn in the screen
(wait-for 2)
(mac-handle-new-window-for-tab nil)
;; Need some time for the new fame to be drawn in the screen
(wait-for 2)

(push (should
       (mac-frame-tab-group-property nil :selected-frame))
      frame-stack)
;; the :selecte-frame is a truly unique frame
(should-not (equal (car frame-stack) (cadr frame-stack)))
(let ((frames (mac-frame-tab-group-property nil :frames)))
  (should (eql 2 (length frames)))
  (should (member (car frame-stack) frames))
  (should (member (cadr frame-stack) frames)))

(should (frame-parameter nil 'mac-transparent-titlebar))
(should-not (mac-frame-tab-group-property nil :overview-visible-p))

;; tab-bar cannot be swiched off when there's more than one tab
(should-error (mac-set-frame-tab-group-property nil :tab-bar-visible-p nil))
(should (mac-frame-tab-group-property nil :tab-bar-visible-p))

;; TODO: need to find a way to exit overview programatically
;; ;; toggle overview on and off
;; (should (mac-frame-tab-group-property nil :tab-bar-visible-p))
;; (should-not (mac-frame-tab-group-property nil :overview-visible-p))
;; (should
;;  (progn
;;    (mac-set-frame-tab-group-property nil :overview-visible-p t)
;;    (mac-frame-tab-group-property nil :overview-visible-p)))
;; ;; tab-bar is visible after exiting overview when there's more than one tab-bar
;; (should (mac-frame-tab-group-property nil :tab-bar-visible-p))


;; select previously selected frame
(mac-set-frame-tab-group-property nil :selected-frame (cadr frame-stack))
(should (equal (cadr frame-stack)
               (mac-frame-tab-group-property nil :selected-frame)))

;; (3) Open a new frame in a new window and test it
(let ((f (make-frame)))
  (select-frame f))

(push (should
       (mac-frame-tab-group-property nil :selected-frame))
      frame-stack)
;; the :selecte-frame is a truly unique frame
(should (eql 3 (length frame-stack)))
(should-not (equal (car frame-stack) (cadr frame-stack)))
(should-not (equal (car frame-stack) (caddr frame-stack)))

(let ((frames (mac-frame-tab-group-property nil :frames)))
  (should (eql 1 (length frames)))
  (should (member (car frame-stack) frames)))

(should (frame-parameter nil 'mac-transparent-titlebar))
(should-not (mac-frame-tab-group-property nil :overview-visible-p))
(should-not (mac-frame-tab-group-property nil :tab-bar-visible-p))

;; toggle tab-bar on and off
(mac-set-frame-tab-group-property nil :tab-bar-visible-p t)
(should (mac-frame-tab-group-property nil :tab-bar-visible-p))
(mac-set-frame-tab-group-property nil :tab-bar-visible-p nil)
(should-not (mac-frame-tab-group-property nil :tab-bar-visible-p))

;; (4) Transfer one frame from the first window to the newly opened window
(mac-set-frame-tab-group-property
 nil :frames
 (list (car frame-stack)
       (cadr frame-stack)))

(let ((frames (mac-frame-tab-group-property nil :frames)))
  (should (eql 2 (length frames)))
  (should (member (car frame-stack) frames))
  (should (member (cadr frame-stack) frames)))
(should (mac-frame-tab-group-property nil :tab-bar-visible-p))

(let* ((f (caddr frame-stack))
       (frames (mac-frame-tab-group-property f :frames)))
  (should (eql 1 (length frames)))
  (should (member f frames))
  (should-not (mac-frame-tab-group-property f :tab-bar-visible-p)))


;; (5) Test the overview visible
(should
 (progn
   (mac-set-frame-tab-group-property nil :overview-visible-p t)
   (mac-frame-tab-group-property nil :overview-visible-p)))

;; All done - test pass!
(kill-emacs 0)

(provide 'mac-transparent-titlebar-tab-bar-tests)

;;; mac-transparent-titlebar-tab-bar-tests.el ends here
