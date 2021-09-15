;;; mac-win.el --- parse switches controlling interface with Mac window system -*- lexical-binding:t -*-

;; Copyright (C) 1999-2008  Free Software Foundation, Inc.
;; Copyright (C) 2009-2021  YAMAMOTO Mitsuharu

;; Author: Andrew Choi <akochoi@mac.com>
;;	YAMAMOTO Mitsuharu <mituharu@math.s.chiba-u.ac.jp>
;; Keywords: terminals

;; This file is part of GNU Emacs Mac port.

;; GNU Emacs Mac port is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs Mac port is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs Mac port.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Mac-win.el:  this file is loaded from ../lisp/startup.el when it recognizes
;; that Mac windows are to be used.  Command line switches are parsed and those
;; pertaining to Mac are processed and removed from the command line.  The
;; Mac display is opened and hooks are set for popping up the initial window.

;; Beginning in Emacs 23, the act of loading this file should not have
;; the side effect of initializing the window system or processing
;; command line arguments (this file is now loaded in loadup.el).  See
;; `handle-args-function' and `window-system-initialization' for more details.

;; startup.el will then examine startup files, and eventually call the hooks
;; which create the first window(s).

;;; Code:

;; These are the standard X switches from the Xt Initialize.c file of
;; Release 4.

;; Command line		Resource Manager string

;; +rv			*reverseVideo
;; +synchronous		*synchronous
;; -background		*background
;; -bd			*borderColor
;; -bg			*background
;; -bordercolor		*borderColor
;; -borderwidth		.borderWidth
;; -bw			.borderWidth
;; -display		.display
;; -fg			*foreground
;; -fn			*font
;; -font		*font
;; -foreground		*foreground
;; -geometry		.geometry
;; -iconic		.iconic
;; -name		.name
;; -reverse		*reverseVideo
;; -rv			*reverseVideo
;; -selectionTimeout    .selectionTimeout
;; -synchronous		*synchronous
;; -xrm

;; An alist of X options and the function which handles them.  See
;; ../startup.el.

(eval-when-compile (require 'cl-lib))

;; (if (not (eq window-system 'mac))
;;     (error "%s: Loading mac-win.el but not compiled for Mac" invocation-name))

(require 'term/common-win)
(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'select)
(require 'menu-bar)
(require 'fontset)
(require 'dnd)

(defvar mac-service-selection)
(defvar mac-system-script-code)
(defvar mac-apple-event-map)
(defvar mac-ts-active-input-overlay)
(defvar mac-frame-tabbing)


;;
;; Standard Mac cursor shapes
;;

(defconst mac-pointer-arrow 0)
(defconst mac-pointer-copy-arrow 1)
(defconst mac-pointer-alias-arrow 2)
(defconst mac-pointer-contextual-menu-arrow 3)
(defconst mac-pointer-I-beam 4)
(defconst mac-pointer-cross 5)
(defconst mac-pointer-plus 6)
(defconst mac-pointer-watch 7)
(defconst mac-pointer-closed-hand 8)
(defconst mac-pointer-open-hand 9)
(defconst mac-pointer-pointing-hand 10)
(defconst mac-pointer-counting-up-hand 11)
(defconst mac-pointer-counting-down-hand 12)
(defconst mac-pointer-counting-up-and-down-hand 13)
(defconst mac-pointer-spinning 14)
(defconst mac-pointer-resize-left 15)
(defconst mac-pointer-resize-right 16)
(defconst mac-pointer-resize-left-right 17)
;; Mac OS X 10.2 and later
(defconst mac-pointer-not-allowed 18)
;; Mac OS X 10.3 and later
(defconst mac-pointer-resize-up 19)
(defconst mac-pointer-resize-down 20)
(defconst mac-pointer-resize-up-down 21)
(defconst mac-pointer-poof 22)
;; Mac OS X 10.7 and later
(defconst mac-pointer-resize-northwest-southeast 23)
(defconst mac-pointer-resize-northeast-southwest 24)
(defconst mac-pointer-resize-north-south 25)
(defconst mac-pointer-resize-north 26)
(defconst mac-pointer-resize-south 27)
(defconst mac-pointer-resize-east-west 28)
(defconst mac-pointer-resize-east 29)
(defconst mac-pointer-resize-west 30)
(defconst mac-pointer-resize-northwest 31)
(defconst mac-pointer-resize-northeast 32)
(defconst mac-pointer-resize-southwest 33)
(defconst mac-pointer-resize-southeast 34)

;;
;; Standard X cursor shapes that have Mac counterparts
;;

(defconst x-pointer-left-ptr mac-pointer-arrow)
(defconst x-pointer-xterm mac-pointer-I-beam)
(defconst x-pointer-crosshair mac-pointer-cross)
(defconst x-pointer-plus mac-pointer-plus)
(defconst x-pointer-watch mac-pointer-watch)
(defconst x-pointer-hand2 mac-pointer-pointing-hand)
(defconst x-pointer-left-side mac-pointer-resize-left)
(defconst x-pointer-right-side mac-pointer-resize-right)
(defconst x-pointer-sb-h-double-arrow mac-pointer-resize-left-right)
(defconst x-pointer-top-side mac-pointer-resize-up)
(defconst x-pointer-bottom-side mac-pointer-resize-down)
(defconst x-pointer-sb-v-double-arrow mac-pointer-resize-up-down)


;;;; Utility functions
(declare-function mac-get-preference "mac.c"
                  (key &optional application format hash-bound))

(defun mac-possibly-use-high-resolution-monitors-p ()
  "Return non-nil if high-resolution monitors can possibly be used.
Namely, either a Retina display is connected or HiDPI display
modes have been enabled with Quartz Debug.app."
  (or
   ;; HiDPI display modes have been enabled with Quartz Debug.app.
   (mac-get-preference "DisplayResolutionEnabled" "com.apple.windowserver")
   (cl-loop for attributes in (display-monitor-attributes-list)
	    if (eq (cdr (assq 'backing-scale-factor attributes)) 2) return t)))

(defun mac-high-resolution-image-file-name (filename &optional scale)
  "Return the name of high-resolution image file for FILENAME.
The optional arg SCALE is the scale factor, and defaults to 2."
  (let ((pos (or (string-match "\\.[^./]*\\'" filename) (length filename))))
    (format "%s@%dx%s" (substring filename 0 pos) (or scale 2)
	    (substring filename pos))))


;;;; Modifier keys

;; Modifier name `ctrl' is an alias of `control'.
(put 'ctrl 'modifier-value (get 'control 'modifier-value))


;;;; Script codes and coding systems
(defconst mac-script-code-coding-systems
  '((0 . mac-roman)			; smRoman
    (1 . japanese-shift-jis)		; smJapanese
    (2 . chinese-big5)			; smTradChinese
    (3 . korean-iso-8bit)		; smKorean
    (7 . mac-cyrillic)			; smCyrillic
    (25 . chinese-iso-8bit)		; smSimpChinese
    (29 . mac-centraleurroman)		; smCentralEuroRoman
    )
  "Alist of Mac script codes vs Emacs coding systems.")

(define-charset 'mac-centraleurroman
  "Mac Central European Roman"
  :short-name "Mac CE"
  :ascii-compatible-p t
  :code-space [0 255]
  :map
  (let ((tbl
	 [?\Ä ?\Ā ?\ā ?\É ?\Ą ?\Ö ?\Ü ?\á ?\ą ?\Č ?\ä ?\č ?\Ć ?\ć ?\é ?\Ź
	  ?\ź ?\Ď ?\í ?\ď ?\Ē ?\ē ?\Ė ?\ó ?\ė ?\ô ?\ö ?\õ ?\ú ?\Ě ?\ě ?\ü
	  ?\† ?\° ?\Ę ?\£ ?\§ ?\• ?\¶ ?\ß ?\® ?\© ?\™ ?\ę ?\¨ ?\≠ ?\ģ ?\Į
	  ?\į ?\Ī ?\≤ ?\≥ ?\ī ?\Ķ ?\∂ ?\∑ ?\ł ?\Ļ ?\ļ ?\Ľ ?\ľ ?\Ĺ ?\ĺ ?\Ņ
	  ?\ņ ?\Ń ?\¬ ?\√ ?\ń ?\Ň ?\∆ ?\« ?\» ?\… ?\  ?\ň ?\Ő ?\Õ ?\ő ?\Ō
	  ?\– ?\— ?\“ ?\” ?\‘ ?\’ ?\÷ ?\◊ ?\ō ?\Ŕ ?\ŕ ?\Ř ?\‹ ?\› ?\ř ?\Ŗ
	  ?\ŗ ?\Š ?\‚ ?\„ ?\š ?\Ś ?\ś ?\Á ?\Ť ?\ť ?\Í ?\Ž ?\ž ?\Ū ?\Ó ?\Ô
	  ?\ū ?\Ů ?\Ú ?\ů ?\Ű ?\ű ?\Ų ?\ų ?\Ý ?\ý ?\ķ ?\Ż ?\Ł ?\ż ?\Ģ ?\ˇ])
	(map (make-vector 512 nil)))
    (or (= (length tbl) 128)
	(error "Invalid vector length: %d" (length tbl)))
    (dotimes (i 128)
      (aset map (* i 2) i)
      (aset map (1+ (* i 2)) i))
    (dotimes (i 128)
      (aset map (+ 256 (* i 2)) (+ 128 i))
      (aset map (+ 256 (1+ (* i 2))) (aref tbl i)))
    map))

(define-coding-system 'mac-centraleurroman
  "Mac Central European Roman Encoding (MIME:x-mac-centraleurroman)."
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(mac-centraleurroman)
  :mime-charset 'x-mac-centraleurroman)

(define-charset 'mac-cyrillic
  "Mac Cyrillic"
  :short-name "Mac CYRILLIC"
  :ascii-compatible-p t
  :code-space [0 255]
  :map
  (let ((tbl
	 [?\А ?\Б ?\В ?\Г ?\Д ?\Е ?\Ж ?\З ?\И ?\Й ?\К ?\Л ?\М ?\Н ?\О ?\П
	  ?\Р ?\С ?\Т ?\У ?\Ф ?\Х ?\Ц ?\Ч ?\Ш ?\Щ ?\Ъ ?\Ы ?\Ь ?\Э ?\Ю ?\Я
	  ?\† ?\° ?\Ґ ?\£ ?\§ ?\• ?\¶ ?\І ?\® ?\© ?\™ ?\Ђ ?\ђ ?\≠ ?\Ѓ ?\ѓ
	  ?\∞ ?\± ?\≤ ?\≥ ?\і ?\µ ?\ґ ?\Ј ?\Є ?\є ?\Ї ?\ї ?\Љ ?\љ ?\Њ ?\њ
	  ?\ј ?\Ѕ ?\¬ ?\√ ?\ƒ ?\≈ ?\∆ ?\« ?\» ?\… ?\  ?\Ћ ?\ћ ?\Ќ ?\ќ ?\ѕ
	  ?\– ?\— ?\“ ?\” ?\‘ ?\’ ?\÷ ?\„ ?\Ў ?\ў ?\Џ ?\џ ?\№ ?\Ё ?\ё ?\я
	  ?\а ?\б ?\в ?\г ?\д ?\е ?\ж ?\з ?\и ?\й ?\к ?\л ?\м ?\н ?\о ?\п
	  ?\р ?\с ?\т ?\у ?\ф ?\х ?\ц ?\ч ?\ш ?\щ ?\ъ ?\ы ?\ь ?\э ?\ю ?\€])
	(map (make-vector 512 nil)))
    (or (= (length tbl) 128)
	(error "Invalid vector length: %d" (length tbl)))
    (dotimes (i 128)
      (aset map (* i 2) i)
      (aset map (1+ (* i 2)) i))
    (dotimes (i 128)
      (aset map (+ 256 (* i 2)) (+ 128 i))
      (aset map (+ 256 (1+ (* i 2))) (aref tbl i)))
    map))

(define-coding-system 'mac-cyrillic
  "Mac Cyrillic Encoding (MIME:x-mac-cyrillic)."
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(mac-cyrillic)
  :mime-charset 'x-mac-cyrillic)

(define-charset 'mac-symbol
  "Mac Symbol"
  :short-name "Mac SYMBOL"
  :code-space [32 254]
  :map
  (let ((tbl-32-126
	 [?\  ?\! ?\∀ ?\# ?\∃ ?\% ?\& ?\∍ ?\( ?\) ?\∗ ?\+ ?\, ?\− ?\. ?\/
	  ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\: ?\; ?\< ?\= ?\> ?\?
	  ?\≅ ?\Α ?\Β ?\Χ ?\Δ ?\Ε ?\Φ ?\Γ ?\Η ?\Ι ?\ϑ ?\Κ ?\Λ ?\Μ ?\Ν ?\Ο
	  ?\Π ?\Θ ?\Ρ ?\Σ ?\Τ ?\Υ ?\ς ?\Ω ?\Ξ ?\Ψ ?\Ζ ?\[ ?\∴ ?\] ?\⊥ ?\_
	  ?\ ?\α ?\β ?\χ ?\δ ?\ε ?\φ ?\γ ?\η ?\ι ?\ϕ ?\κ ?\λ ?\μ ?\ν ?\ο
	  ?\π ?\θ ?\ρ ?\σ ?\τ ?\υ ?\ϖ ?\ω ?\ξ ?\ψ ?\ζ ?\{ ?\| ?\} ?\∼])
	(map-32-126 (make-vector (* (1+ (- 126 32)) 2) nil))
	(tbl-160-254
	 ;; Mapping of the following characters are changed from the
	 ;; original one:
	 ;; 0xE2 0x00AE+0xF87F->0x00AE # REGISTERED SIGN, alternate: sans serif
	 ;; 0xE3 0x00A9+0xF87F->0x00A9 # COPYRIGHT SIGN, alternate: sans serif
	 ;; 0xE4 0x2122+0xF87F->0x2122 # TRADE MARK SIGN, alternate: sans serif
	 [?\€ ?\ϒ ?\′ ?\≤ ?\⁄ ?\∞ ?\ƒ ?\♣ ?\♦ ?\♥ ?\♠ ?\↔ ?\← ?\↑ ?\→ ?\↓
	  ?\° ?\± ?\″ ?\≥ ?\× ?\∝ ?\∂ ?\• ?\÷ ?\≠ ?\≡ ?\≈ ?\… ?\⏐ ?\⎯ ?\↵
	  ?\ℵ ?\ℑ ?\ℜ ?\℘ ?\⊗ ?\⊕ ?\∅ ?\∩ ?\∪ ?\⊃ ?\⊇ ?\⊄ ?\⊂ ?\⊆ ?\∈ ?\∉
	  ?\∠ ?\∇ ?\® ?\© ?\™ ?\∏ ?\√ ?\⋅ ?\¬ ?\∧ ?\∨ ?\⇔ ?\⇐ ?\⇑ ?\⇒ ?\⇓
	  ?\◊ ?\〈 ?\® ?\© ?\™ ?\∑ ?\⎛ ?\⎜ ?\⎝ ?\⎡ ?\⎢ ?\⎣ ?\⎧ ?\⎨ ?\⎩ ?\⎪
	  ?\ ?\〉 ?\∫ ?\⌠ ?\⎮ ?\⌡ ?\⎞ ?\⎟ ?\⎠ ?\⎤ ?\⎥ ?\⎦ ?\⎫ ?\⎬ ?\⎭])
	(map-160-254 (make-vector (* (1+ (- 254 160)) 2) nil)))
    (dotimes (i (1+ (- 126 32)))
      (aset map-32-126 (* i 2) (+ 32 i))
      (aset map-32-126 (1+ (* i 2)) (aref tbl-32-126 i)))
    (dotimes (i (1+ (- 254 160)))
      (aset map-160-254 (* i 2) (+ 160 i))
      (aset map-160-254 (1+ (* i 2)) (aref tbl-160-254 i)))
    (vconcat map-32-126 map-160-254)))

(define-charset 'mac-dingbats
  "Mac Dingbats"
  :short-name "Mac Dingbats"
  :code-space [32 254]
  :map
  (let ((tbl-32-126
	 [?\  ?\✁ ?\✂ ?\✃ ?\✄ ?\☎ ?\✆ ?\✇ ?\✈ ?\✉ ?\☛ ?\☞ ?\✌ ?\✍ ?\✎ ?\✏
	  ?\✐ ?\✑ ?\✒ ?\✓ ?\✔ ?\✕ ?\✖ ?\✗ ?\✘ ?\✙ ?\✚ ?\✛ ?\✜ ?\✝ ?\✞ ?\✟
	  ?\✠ ?\✡ ?\✢ ?\✣ ?\✤ ?\✥ ?\✦ ?\✧ ?\★ ?\✩ ?\✪ ?\✫ ?\✬ ?\✭ ?\✮ ?\✯
	  ?\✰ ?\✱ ?\✲ ?\✳ ?\✴ ?\✵ ?\✶ ?\✷ ?\✸ ?\✹ ?\✺ ?\✻ ?\✼ ?\✽ ?\✾ ?\✿
	  ?\❀ ?\❁ ?\❂ ?\❃ ?\❄ ?\❅ ?\❆ ?\❇ ?\❈ ?\❉ ?\❊ ?\❋ ?\● ?\❍ ?\■ ?\❏
	  ?\❐ ?\❑ ?\❒ ?\▲ ?\▼ ?\◆ ?\❖ ?\◗ ?\❘ ?\❙ ?\❚ ?\❛ ?\❜ ?\❝ ?\❞])
	(map-32-126 (make-vector (* (1+ (- 126 32)) 2) nil))
	(tbl-128-141
	 [?\❨ ?\❩ ?\❪ ?\❫ ?\❬ ?\❭ ?\❮ ?\❯ ?\❰ ?\❱ ?\❲ ?\❳ ?\❴ ?\❵])
	(map-128-141 (make-vector (* (1+ (- 141 128)) 2) nil))
	(tbl-161-239
	 [?\❡ ?\❢ ?\❣ ?\❤ ?\❥ ?\❦ ?\❧ ?\♣ ?\♦ ?\♥ ?\♠ ?\① ?\② ?\③ ?\④
	  ?\⑤ ?\⑥ ?\⑦ ?\⑧ ?\⑨ ?\⑩ ?\❶ ?\❷ ?\❸ ?\❹ ?\❺ ?\❻ ?\❼ ?\❽ ?\❾ ?\❿
	  ?\➀ ?\➁ ?\➂ ?\➃ ?\➄ ?\➅ ?\➆ ?\➇ ?\➈ ?\➉ ?\➊ ?\➋ ?\➌ ?\➍ ?\➎ ?\➏
	  ?\➐ ?\➑ ?\➒ ?\➓ ?\➔ ?\→ ?\↔ ?\↕ ?\➘ ?\➙ ?\➚ ?\➛ ?\➜ ?\➝ ?\➞ ?\➟
	  ?\➠ ?\➡ ?\➢ ?\➣ ?\➤ ?\➥ ?\➦ ?\➧ ?\➨ ?\➩ ?\➪ ?\➫ ?\➬ ?\➭ ?\➮ ?\➯])
	(map-161-239 (make-vector (* (1+ (- 239 161)) 2) nil))
	(tbl-241-254
	 [?\➱ ?\➲ ?\➳ ?\➴ ?\➵ ?\➶ ?\➷ ?\➸ ?\➹ ?\➺ ?\➻ ?\➼ ?\➽ ?\➾])
	(map-241-254 (make-vector (* (1+ (- 254 241)) 2) nil)))
    (dotimes (i (1+ (- 126 32)))
      (aset map-32-126 (* i 2) (+ 32 i))
      (aset map-32-126 (1+ (* i 2)) (aref tbl-32-126 i)))
    (dotimes (i (1+ (- 141 128)))
      (aset map-128-141 (* i 2) (+ 128 i))
      (aset map-128-141 (1+ (* i 2)) (aref tbl-128-141 i)))
    (dotimes (i (1+ (- 239 161)))
      (aset map-161-239 (* i 2) (+ 161 i))
      (aset map-161-239 (1+ (* i 2)) (aref tbl-161-239 i)))
    (dotimes (i (1+ (- 254 241)))
      (aset map-241-254 (* i 2) (+ 241 i))
      (aset map-241-254 (1+ (* i 2)) (aref tbl-241-254 i)))
    (vconcat map-32-126 map-128-141 map-161-239 map-241-254)))

(defvar mac-system-coding-system nil
  "Coding system derived from the system script code.")

(defun mac-setup-system-coding-system ()
  (setq mac-system-coding-system
	(or (cdr (assq mac-system-script-code mac-script-code-coding-systems))
	    'mac-roman))
  (set-selection-coding-system mac-system-coding-system))


;;;; Composition
(defconst mac-emoji-variation-characters-alist
  '((keycap . "#*0123456789")
    (non-keycap . "\u00A9\u00AE\u203C\u2049\u2122\u2139\u2194\u2195\
\u2196\u2197\u2198\u2199\u21A9\u21AA\u231A\u231B\u2328\u23CF\u23E9\u23EA\
\u23ED\u23EE\u23EF\u23F1\u23F2\u23F3\u23F8\u23F9\u23FA\u24C2\u25AA\u25AB\
\u25B6\u25C0\u25FB\u25FC\u25FD\u25FE\u2600\u2601\u2602\u2603\u2604\u260E\
\u2611\u2614\u2615\u2618\u261D\u2620\u2622\u2623\u2626\u262A\u262E\u262F\
\u2638\u2639\u263A\u2640\u2642\u2648\u2649\u264A\u264B\u264C\u264D\u264E\
\u264F\u2650\u2651\u2652\u2653\u265F\u2660\u2663\u2665\u2666\u2668\u267B\
\u267E\u267F\u2692\u2693\u2694\u2695\u2696\u2697\u2699\u269B\u269C\u26A0\
\u26A1\u26A7\u26AA\u26AB\u26B0\u26B1\u26BD\u26BE\u26C4\u26C5\u26C8\u26CF\
\u26D1\u26D3\u26D4\u26E9\u26EA\u26F0\u26F1\u26F2\u26F3\u26F4\u26F5\u26F7\
\u26F8\u26F9\u26FA\u26FD\u2702\u2708\u2709\u270C\u270D\u270F\u2712\u2714\
\u2716\u271D\u2721\u2733\u2734\u2744\u2747\u2753\u2757\u2763\u2764\u27A1\
\u2934\u2935\u2B05\u2B06\u2B07\u2B1B\u2B1C\u2B50\u2B55\u3030\u303D\u3297\
\u3299\
\U0001F004\U0001F170\U0001F171\U0001F17E\U0001F17F\U0001F202\U0001F21A\
\U0001F22F\U0001F237\U0001F30D\U0001F30E\U0001F30F\U0001F315\U0001F31C\
\U0001F321\U0001F324\U0001F325\U0001F326\U0001F327\U0001F328\U0001F329\
\U0001F32A\U0001F32B\U0001F32C\U0001F336\U0001F378\U0001F37D\U0001F393\
\U0001F396\U0001F397\U0001F399\U0001F39A\U0001F39B\U0001F39E\U0001F39F\
\U0001F3A7\U0001F3AC\U0001F3AD\U0001F3AE\U0001F3C2\U0001F3C4\U0001F3C6\
\U0001F3CA\U0001F3CB\U0001F3CC\U0001F3CD\U0001F3CE\U0001F3D4\U0001F3D5\
\U0001F3D6\U0001F3D7\U0001F3D8\U0001F3D9\U0001F3DA\U0001F3DB\U0001F3DC\
\U0001F3DD\U0001F3DE\U0001F3DF\U0001F3E0\U0001F3ED\U0001F3F3\U0001F3F5\
\U0001F3F7\U0001F408\U0001F415\U0001F41F\U0001F426\U0001F43F\U0001F441\
\U0001F442\U0001F446\U0001F447\U0001F448\U0001F449\U0001F44D\U0001F44E\
\U0001F453\U0001F46A\U0001F47D\U0001F4A3\U0001F4B0\U0001F4B3\U0001F4BB\
\U0001F4BF\U0001F4CB\U0001F4DA\U0001F4DF\U0001F4E4\U0001F4E5\U0001F4E6\
\U0001F4EA\U0001F4EB\U0001F4EC\U0001F4ED\U0001F4F7\U0001F4F9\U0001F4FA\
\U0001F4FB\U0001F4FD\U0001F508\U0001F50D\U0001F512\U0001F513\U0001F549\
\U0001F54A\U0001F550\U0001F551\U0001F552\U0001F553\U0001F554\U0001F555\
\U0001F556\U0001F557\U0001F558\U0001F559\U0001F55A\U0001F55B\U0001F55C\
\U0001F55D\U0001F55E\U0001F55F\U0001F560\U0001F561\U0001F562\U0001F563\
\U0001F564\U0001F565\U0001F566\U0001F567\U0001F56F\U0001F570\U0001F573\
\U0001F574\U0001F575\U0001F576\U0001F577\U0001F578\U0001F579\U0001F587\
\U0001F58A\U0001F58B\U0001F58C\U0001F58D\U0001F590\U0001F5A5\U0001F5A8\
\U0001F5B1\U0001F5B2\U0001F5BC\U0001F5C2\U0001F5C3\U0001F5C4\U0001F5D1\
\U0001F5D2\U0001F5D3\U0001F5DC\U0001F5DD\U0001F5DE\U0001F5E1\U0001F5E3\
\U0001F5E8\U0001F5EF\U0001F5F3\U0001F5FA\U0001F610\U0001F687\U0001F68D\
\U0001F691\U0001F694\U0001F698\U0001F6AD\U0001F6B2\U0001F6B9\U0001F6BA\
\U0001F6BC\U0001F6CB\U0001F6CD\U0001F6CE\U0001F6CF\U0001F6E0\U0001F6E1\
\U0001F6E2\U0001F6E3\U0001F6E4\U0001F6E5\U0001F6E9\U0001F6F0\U0001F6F3"))
  "Groups of characters that are sensitive to variation selectors 15 and 16.
It is an alist of label symbols vs sequences of characters.
The entries are currently based on emoji-variation-sequences.txt 14.0.")

(defconst mac-emoji-modifier-base-characters-alist
  '((t . "\u261D\u26F9\u270A\u270B\u270C\u270D\
\U0001F385\U0001F3C2\U0001F3C3\U0001F3C4\U0001F3C7\U0001F3CA\U0001F3CB\
\U0001F3CC\U0001F442\U0001F443\U0001F446\U0001F447\U0001F448\U0001F449\
\U0001F44A\U0001F44B\U0001F44C\U0001F44D\U0001F44E\U0001F44F\U0001F450\
\U0001F466\U0001F467\U0001F468\U0001F469\U0001F46B\U0001F46C\U0001F46D\
\U0001F46E\U0001F470\U0001F471\U0001F472\U0001F473\U0001F474\U0001F475\
\U0001F476\U0001F477\U0001F478\U0001F47C\U0001F481\U0001F482\U0001F483\
\U0001F485\U0001F486\U0001F487\U0001F48F\U0001F491\U0001F4AA\U0001F574\
\U0001F575\U0001F57A\U0001F590\U0001F595\U0001F596\U0001F645\U0001F646\
\U0001F647\U0001F64B\U0001F64C\U0001F64D\U0001F64E\U0001F64F\U0001F6A3\
\U0001F6B4\U0001F6B5\U0001F6B6\U0001F6C0\U0001F6CC\U0001F90C\U0001F90F\
\U0001F918\U0001F919\U0001F91A\U0001F91B\U0001F91C\U0001F91E\U0001F91F\
\U0001F926\U0001F930\U0001F931\U0001F932\U0001F933\U0001F934\U0001F935\
\U0001F936\U0001F937\U0001F938\U0001F939\U0001F93D\U0001F93E\U0001F977\
\U0001F9B5\U0001F9B6\U0001F9B8\U0001F9B9\U0001F9BB\U0001F9CD\U0001F9CE\
\U0001F9CF\U0001F9D1\U0001F9D2\U0001F9D3\U0001F9D4\U0001F9D5\U0001F9D6\
\U0001F9D7\U0001F9D8\U0001F9D9\U0001F9DA\U0001F9DB\U0001F9DC\U0001F9DD\
\U0001FAC3\U0001FAC4\U0001FAC5\U0001FAF0\U0001FAF1\U0001FAF2\U0001FAF3\
\U0001FAF4\U0001FAF5\U0001FAF6"))
  "Groups of characters that are sensitive to emoji modifiers.
It is an alist of label symbols vs sequences of characters.
The entries are currently based on emoji-sequences.txt 14.0.")

(defconst mac-emoji-gendered-zwj-characters-alist
  '((role . "\u2695\u2696\u2708\
\U0001F33E\U0001F373\U0001F37C\U0001F393\U0001F3A4\U0001F3A8\U0001F3EB\
\U0001F3ED\U0001F4BB\U0001F4BC\U0001F527\U0001F52C\U0001F680\U0001F692\
\U0001F9AF\U0001F9BC\U0001F9BD")
    (hair . "\U0001F9B0\U0001F9B1\U0001F9B2\U0001F9B3")
    (gendered . "\u26F9\U0001F3C3\U0001F3C4\U0001F3CA\U0001F3CB\U0001F3CC\
\U0001F46E\U0001F46F\U0001F470\U0001F471\U0001F473\U0001F477\U0001F481\
\U0001F482\U0001F486\U0001F487\U0001F575\U0001F645\U0001F646\U0001F647\
\U0001F64B\U0001F64D\U0001F64E\U0001F6A3\U0001F6B4\U0001F6B5\U0001F6B6\
\U0001F926\U0001F935\U0001F937\U0001F938\U0001F939\U0001F93C\U0001F93D\
\U0001F93E\U0001F9B8\U0001F9B9\U0001F9CD\U0001F9CE\U0001F9CF\U0001F9D4\
\U0001F9D6\U0001F9D7\U0001F9D8\U0001F9D9\U0001F9DA\U0001F9DB\U0001F9DC\
\U0001F9DD\U0001F9DE\U0001F9DF"))
  "Groups of characters that are parts of the gendered zwj sequences.
It is an alist of label symbols vs sequences of characters.
The entries are currently based on emoji-zwj-sequences.txt 14.0.")

(defconst mac-emoji-tag-base-characters-alist
  '((flag "\U0001F3F4" "[\U000E0030-\U000E0039\U000E0061-\U000E007A]+"))
  "Groups of emoji characters that are sensitive to tags.
It is an alist of label symbols vs lists whose elements are of
the form (CHARS SPEC) where CHARS is a string of tag base
characters and SPEC is a regexp for the corresponding tag spec.
The entries are currently based on emoji-sequences.txt 14.0.")

(defun mac-emoji-multistyles-unistyles (sequence)
  "Split emoji SEQUENCE into a cons of multistyles and unistyles."
  (let ((variations-regexp
         (concat "["
                 (mapconcat 'cdr mac-emoji-variation-characters-alist "")
                 "]"))
        multistyles unistyles)
    (mapc (lambda (c) (if (string-match variations-regexp (string c))
                          (push c multistyles)
                        (push c unistyles)))
          sequence)
    (cons multistyles unistyles)))

(defun mac-compose-gstring-for-variation-with-trailer (gstring direction)
  "Compose glyph-string GSTRING for graphic display.
GSTRING must have three glyphs; the first is a character that is
sensitive to the variation selectors 15 (U+FE0E, text-style) and
16 (U+FE0F, emoji-style), the second is a glyph for one of the
variation selectors, and the third is a \"trailer\" character
that participates in the whole composition, e.g., the combining
enclosing keycap character (U+20E3) or the emoji modifiers for
skin tones (U+1F3FB - U+1F3FF)."
  (let ((saved-header (lgstring-header gstring))
	(g2 (lgstring-glyph gstring 2))
	(i 0)
	glyph from to)
    (lgstring-set-header gstring (vector (lgstring-font gstring)
					 (lgstring-char gstring 0)
					 (lgstring-char gstring 2)))
    (lglyph-set-from-to g2 (1- (lglyph-from g2)) (1- (lglyph-to g2)))
    (lgstring-set-glyph gstring 1 g2)
    (lgstring-set-glyph gstring 2 nil)
    (setq gstring (copy-sequence (font-shape-gstring gstring direction)))
    (when gstring
      (lgstring-set-header gstring saved-header)
      (lgstring-set-id gstring nil)
      (while (and (< i (lgstring-glyph-len gstring))
		  (setq glyph (copy-sequence (lgstring-glyph gstring i))))
	(setq from (lglyph-from glyph)
	      to (lglyph-to glyph))
	(if (>= from 1) (setq from (1+ from)))
	(if (>= to 1) (setq to (1+ to)))
	(lglyph-set-from-to glyph from to)
	(lgstring-set-glyph gstring i glyph)
	(setq i (1+ i)))))
  gstring)

(defun mac-compose-gstring-for-text-style-variation (gstring _direction)
  "Compose glyph-string GSTRING in text style for graphic display.
GSTRING must have two glyphs; the first is a character that is
sensitive to the text/emoji-style variation selector, and the
second is a glyph for the variation selector 15 (U+FE0E)."
  (let ((g0 (lgstring-glyph gstring 0))
	(g1 (lgstring-glyph gstring 1)))
    (lglyph-set-from-to g0 (lglyph-from g0) (lglyph-to g1))
    (lglyph-set-char g0 0)
    (lgstring-set-glyph gstring 1 nil))
  gstring)

(defun mac-compose-gstring-for-emoji-style-variation (gstring direction)
  "Compose glyph-string GSTRING in emoji style for graphic display.
GSTRING must have two glyphs; the first is a character that is
sensitive to the text/emoji-style variation selector, and the
second is a glyph for the variation selector 16 (U+FE0F)."
  (let ((saved-header (lgstring-header gstring))
	glyph)
    (lgstring-set-header gstring (vector (lgstring-font gstring)
					 (lgstring-char gstring 0)))
    (lgstring-set-glyph gstring 1 nil)
    (setq gstring (copy-sequence (font-shape-gstring gstring direction)))
    (when gstring
      (lgstring-set-header gstring saved-header)
      (lgstring-set-id gstring nil)
      (setq glyph (copy-sequence (lgstring-glyph gstring 0)))
      (lglyph-set-from-to glyph (lglyph-from glyph) (1+ (lglyph-to glyph)))
      (lglyph-set-char glyph 0)
      (lgstring-set-glyph gstring 0 glyph)))
  gstring)

(defun mac-setup-composition-function-table ()
  ;; Regional Indicator Symbols
  (set-char-table-range composition-function-table '(#x1F1E6 . #x1F1FF)
			'([".[\x1F1E6-\x1F1FF]" 0 font-shape-gstring]))
  (let* ((variations
          (mapconcat 'cdr mac-emoji-variation-characters-alist ""))
         (modifications
          (mapconcat 'cdr mac-emoji-modifier-base-characters-alist ""))
         (modification-multistyles
          (car (mac-emoji-multistyles-unistyles modifications))))
    ;; Variation Selectors 15 and 16
    (let ((regexp-keycap
	   (concat "[" (cdr (assq 'keycap mac-emoji-variation-characters-alist))
		   "].\u20E3"))
	  (regexp-all (concat "[" variations "]."))
	  (regexp-modified
           (concat "[" modification-multistyles "].[\x1F3FB-\x1F3FF]")))
      (set-char-table-range
       composition-function-table ?\uFE0E
       `([,regexp-keycap 1 mac-compose-gstring-for-variation-with-trailer 1]
	 [,regexp-modified 1 mac-compose-gstring-for-variation-with-trailer -1]
	 [,regexp-all 1 mac-compose-gstring-for-text-style-variation -1]))
      (set-char-table-range
       composition-function-table ?\uFE0F
       `([,regexp-keycap 1 mac-compose-gstring-for-variation-with-trailer 0]
	 [,regexp-modified 1 mac-compose-gstring-for-variation-with-trailer 0]
	 [,regexp-all 1 mac-compose-gstring-for-emoji-style-variation 0])))
    ;; Emoji Modifiers
    (set-char-table-range
     composition-function-table '(#x1F3FB . #x1F3FF)
     `([,(concat "[" modifications "].") 1 font-shape-gstring 0]))
    ;; Emoji Tag Sequences
    (let* ((tag-term "\U000E007F")
           (modifications-regexp (concat "[" modifications "]"))
           rules)
      (dolist (entry mac-emoji-tag-base-characters-alist)
        (let* ((base-chars (nth 1 entry))
               (tag-spec (nth 2 entry))
               (base-multistyles
                (car (mac-emoji-multistyles-unistyles base-chars)))
               base-modifications)
          (push `[,(concat "[" base-chars "]\\(?:" tag-spec "\\)" tag-term)
                  1 font-shape-gstring -1] rules)
          (when (/= (length base-multistyles) 0)
            (push `[,(concat "[" base-multistyles "]\uFE0E\\(?:"
                             tag-spec "\\)" tag-term)
                    2 font-shape-gstring -2] rules)
            (push `[,(concat "[" base-multistyles "]\uFE0F\\(?:"
                             tag-spec "\\)" tag-term)
                    2 font-shape-gstring -1] rules))
          (mapc (lambda (c) (if (string-match modifications-regexp (string c))
                                (push c base-modifications)))
                base-chars)
          (when (/= (length base-modifications) 0)
            (push `[,(concat "[" base-modifications "][\x1F3FB-\x1F3FF]\\(?:"
                             tag-spec "\\)" tag-term)
                    2 font-shape-gstring -1] rules)
            (let ((base-modification-multistyles
                   (car (mac-emoji-multistyles-unistyles base-modifications))))
              (if (/= (length base-modification-multistyles) 0)
                  (push `[,(concat "[" base-modification-multistyles
                                   "]\uFE0F[\x1F3FB-\x1F3FF]\\(?:"
                                   tag-spec "\\)" tag-term)
                          3 font-shape-gstring -2] rules))))))
      (set-char-table-range
       composition-function-table '(#xE0020 . #xE007E) (nreverse rules))))
  ;; Emoji ZWJ Sequences.
  (let* ((zwj "\u200D") (vs16 "\uFE0F") (modifiers "[\x1F3FB-\x1F3FF]")
         (man "\U0001F468") (woman "\U0001F469") (person "\U0001F9D1")
	 (girl "\U0001F467") (boy "\U0001F466")
	 (red-heart "\u2764") (kiss-mark "\U0001F48B") (handshake "\U0001F91D")
         (rightwards-hand "\U0001FAF1") (leftwards-hand "\U0001FAF2")
         (fire "\U0001F525") (adhesive-bandage "\U0001FA79")
         (white-flag "\U0001F3F3") (transgender "\u26A7") (rainbow "\U0001F308")
         (black-flag "\U0001F3F4") (skull-and-crossbones "\u2620")
         (cat "\U0001F408") (black-large-square "\u2B1B")
         (dog "\U0001F415") (safety-vest "\U0001F9BA")
         (bear "\U0001F43B") (snowflake "\u2744")
         (eye "\U0001F441") (left-speech-bubble "\U0001F5E8")
         (face-with-open-mouth "\U0001F62E") (dashing-away "\U0001F4A8")
         (knocked-out-face "\U0001F635") (dizzy "\U0001F4AB")
         (face-without-mouth "\U0001F636") (fog "\U0001F32B")
         (christmas-tree "\U0001F384")
         (female "\u2640") (male "\u2642")
         (vs16? (concat vs16 "?"))
	 (man-or-woman (concat "[" man woman "]"))
         (man-woman-or-person (concat "[" man woman person "]"))
	 (girl-or-boy (concat "[" girl boy "]"))
	 (female-or-male (concat "[" female male "]"))
	 (children (concat "\\(?:" girl "\\(?:" zwj girl-or-boy "\\)?"
			   "\\|" boy "\\(?:" zwj boy "\\)?\\)"))
         (object-multis-unis
          (mac-emoji-multistyles-unistyles
           (mapconcat (lambda (symbol)
                        (cdr (assq symbol
                                   mac-emoji-gendered-zwj-characters-alist)))
                      '(role hair) "")))
         (object-multistyles (car object-multis-unis))
         (object-unistyles (cdr object-multis-unis))
         (signs (cdr (assq 'gendered mac-emoji-gendered-zwj-characters-alist)))
         (sign-multistyles (car (mac-emoji-multistyles-unistyles signs))))
    (set-char-table-range
     composition-function-table (string-to-char zwj)
     `([,(concat man ".\\(?:\\(?:" man-or-woman zwj "\\)?" children
                 "\\|" red-heart vs16? zwj
                 "\\(?:" kiss-mark zwj "\\)?" man "\\)")
	1 font-shape-gstring -1]
       [,(concat woman ".\\(?:\\(?:" woman zwj "\\)?" children
                 "\\|" red-heart vs16? zwj
		 "\\(?:" kiss-mark zwj "\\)?" man-or-woman "\\)")
	1 font-shape-gstring -1]
       [,(concat man-woman-or-person ".\\(?:" handshake zwj person
                 "\\|[" object-multistyles "]" vs16?
                 "\\|[" object-unistyles "]\\)")
	1 font-shape-gstring -1]
       [,(concat rightwards-hand "." leftwards-hand)
        1 font-shape-gstring -1]
       [,(concat red-heart zwj "\\(?:" fire "\\|" adhesive-bandage "\\)")
	1 font-shape-gstring -1]
       [,(concat white-flag zwj "\\(?:" transgender vs16? "\\|" rainbow "\\)")
	1 font-shape-gstring -1]
       [,(concat black-flag zwj skull-and-crossbones vs16?)
	1 font-shape-gstring -1]
       [,(concat cat zwj black-large-square)
	1 font-shape-gstring -1]
       [,(concat dog zwj safety-vest)
	1 font-shape-gstring -1]
       [,(concat bear zwj snowflake vs16?)
	1 font-shape-gstring -1]
       [,(concat eye zwj left-speech-bubble vs16?)
	1 font-shape-gstring -1]
       [,(concat face-with-open-mouth zwj dashing-away)
	1 font-shape-gstring -1]
       [,(concat knocked-out-face zwj dizzy)
	1 font-shape-gstring -1]
       [,(concat face-without-mouth zwj fog vs16?)
	1 font-shape-gstring -1]
       [,(concat person zwj christmas-tree)
	1 font-shape-gstring -1]
       [,(concat "[" signs "]." female-or-male vs16?)
	1 font-shape-gstring -1]))
    (set-char-table-range
     composition-function-table ?\uFE0F
     `([,(concat red-heart vs16 zwj "\\(?:" fire "\\|" adhesive-bandage "\\)")
	1 font-shape-gstring 0]
       [,(concat white-flag vs16 zwj
                 "\\(?:" transgender vs16? "\\|" rainbow "\\)")
        1 font-shape-gstring 0]
       [,(concat eye vs16 zwj left-speech-bubble vs16?)
	1 font-shape-gstring 0]
       [,(concat "[" sign-multistyles "]." zwj female-or-male vs16?)
	1 font-shape-gstring 0]
       ,@(aref composition-function-table ?\uFE0F)))
    (set-char-table-range
     composition-function-table '(#x1F3FB . #x1F3FF)
     `([,(concat man-woman-or-person "." zwj
                 "\\(?:[" object-multistyles "]" vs16?
                 "\\|[" object-unistyles "]\\)")
	1 font-shape-gstring 0]
       [,(concat "[" signs "]." zwj female-or-male vs16?)
	1 font-shape-gstring 0]
       [,(concat man "." zwj "\\(?:" red-heart vs16? zwj
                 "\\(?:" kiss-mark zwj "\\)?" man modifiers "\\)")
	1 font-shape-gstring 0]
       [,(concat woman "." zwj "\\(?:" red-heart vs16? zwj
		 "\\(?:" kiss-mark zwj "\\)?" man-or-woman modifiers "\\)")
	1 font-shape-gstring 0]
       [,(concat person "." zwj "\\(?:" red-heart vs16? zwj
		 "\\(?:" kiss-mark zwj "\\)?" person modifiers "\\)")
	1 font-shape-gstring 0]
       [,(concat "\\(" man "\\|" person "\\)." zwj handshake zwj
                 "\\1" modifiers)
	1 font-shape-gstring 0]
       [,(concat woman "." zwj handshake zwj man-or-woman modifiers)
	1 font-shape-gstring 0]
       [,(concat rightwards-hand "." zwj leftwards-hand modifiers)
	1 font-shape-gstring 0]
       [,(concat person "." zwj christmas-tree)
	1 font-shape-gstring 0]
       ,@(aref composition-function-table #x1F3FB)))))

(defcustom mac-auto-operator-composition-characters "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  "Sequence of characters used in automatic operator composition."
  :package-version '(Mac\ port . "5.10")
  :type 'string
  :group 'mac)

(defcustom mac-auto-operator-composition-max-length 4
  "Maximum length of strings used in automatic operator composition."
  :package-version '(Mac\ port . "5.14")
  :type 'integer
  :group 'mac)

(declare-function mac-font-shape-gstring-nocache "macfont.m"
                  (gstring direction))

(defvar mac-auto-operator-composition-cache (make-hash-table :test 'equal))

(defun mac-subgstring (gstring &optional from to)
  (let ((size (lgstring-char-len gstring))
        (header (lgstring-header gstring))
        (new-gstring (make-vector (length gstring) nil))
        (j 0))
    (cond ((null from) (setq from 0))
          ((< from 0) (setq from (+ from size))))
    (cond ((null to) (setq to size))
          ((< to 0) (setq to (+ to size))))
    (lgstring-set-header new-gstring
                         (vconcat (list (aref header 0))
                                  (substring header (1+ from) (1+ to))))
    (dotimes (i (lgstring-glyph-len gstring))
      (let ((lglyph (lgstring-glyph gstring i)))
        (when (and lglyph (<= from (lglyph-to lglyph))
                   (< (lglyph-from lglyph) to))
          (setq lglyph (lglyph-copy lglyph))
          (lglyph-set-from-to lglyph (- (lglyph-from lglyph) from)
                              (- (lglyph-to lglyph) from))
          (lgstring-set-glyph new-gstring j lglyph)
          (setq j (1+ j)))))
    new-gstring))

(defun mac-gstring-prefix-p (gstring1 gstring2)
  (and (eq (lgstring-font gstring1) (lgstring-font gstring2))
       (let* ((len1 (lgstring-glyph-len gstring1))
	      (minlen (min len1 (lgstring-glyph-len gstring2)))
	      (i 0))
	 (while (and (< i minlen)
		     (equal (lgstring-glyph gstring1 i)
			    (lgstring-glyph gstring2 i))
		     (lgstring-glyph gstring1 i))
	   (setq i (1+ i)))
	 (or (= i len1)
	     (and (< i len1) (null (lgstring-glyph gstring1 i)))))))

(defun mac-auto-operator-composition-shape-gstring (gstring direction)
  "Like `font-shape-gstring', but return nil unless GSTRING is minimal.
GSTRING is minimal if and only if the shaped GSTRING does not
coincide with the concatenation of the shaped ones of any proper
prefix of GSTRING and the corresponding suffix."
  (if (gethash (lgstring-header gstring) mac-auto-operator-composition-cache)
      nil
    (let ((full (mac-font-shape-gstring-nocache
                 (mac-subgstring gstring) direction))
	  (char-len (lgstring-char-len gstring))
	  (i 1))
      (while (and full (< i char-len))
	(let ((partial (mac-font-shape-gstring-nocache
			(mac-subgstring gstring 0 i) direction)))
	  (when (and partial (mac-gstring-prefix-p partial full))
            (setq partial (mac-font-shape-gstring-nocache
                           (mac-subgstring gstring i) direction))
            (if (and partial (mac-gstring-prefix-p partial
                                                   (mac-subgstring full i)))
                (setq full nil))))
	(setq i (1+ i)))
      (if full
	  (font-shape-gstring gstring direction)
	(puthash (copy-sequence (lgstring-header gstring)) t
		 mac-auto-operator-composition-cache)
	nil))))

(define-minor-mode mac-auto-operator-composition-mode
  "Toggle Mac Auto Operator Composition mode.

Mac Auto Operator Composition mode automatically composes
consecutive occurrences of characters consisting of the elements
of `mac-auto-operator-composition-characters' if the font
supports such a composition.  Some fonts provide ligatures for
several combinations of symbolic characters so such a combination
looks like a single unit of an operator symbol in a programming
language."
  :init-value nil
  :global t
  :group 'mac
  :package-version '(Mac\ port . "5.10")
  (if mac-auto-operator-composition-mode
      (when (eq (terminal-live-p (frame-terminal)) 'mac)
	(let* ((regexp (regexp-opt
			(mapcar 'char-to-string
				mac-auto-operator-composition-characters)))
	       (rules `([,(concat "." regexp) 0
			 mac-auto-operator-composition-shape-gstring]))
	       (last-old-rules (list nil))
	       last-new-rules)
	  (let ((i 2))
	    (while (< i mac-auto-operator-composition-max-length)
	      (push `[,(format ".%s\\{%d\\}" regexp i) 0
		      mac-auto-operator-composition-shape-gstring]
		    rules)
	      (setq i (1+ i))))
	  (mapc (lambda (c)
		  (let ((old-rules (aref composition-function-table c)))
		    (when (listp old-rules)
		      (unless (eq old-rules last-old-rules)
			(setq last-old-rules old-rules)
			(setq last-new-rules (append rules old-rules)))
		      (set-char-table-range composition-function-table c
					    last-new-rules))))
		mac-auto-operator-composition-characters))
	(global-auto-composition-mode 1))
    (map-char-table
     (lambda (c rules)
       (when (consp rules)
	 (let (new-rules removed-p)
	   (dolist (rule rules)
	     (if (eq (aref rule 2) 'mac-auto-operator-composition-shape-gstring)
		 (setq removed-p t)
	       (push rule new-rules)))
	   (if removed-p
	       (set-char-table-range composition-function-table c
				     (nreverse new-rules))))))
     composition-function-table)
    (clrhash mac-auto-operator-composition-cache)))


;;;; Conversion between common flavors and Lisp string.

(defconst mac-text-encoding-ascii #x600
  "ASCII text encoding.")

(defconst mac-text-encoding-mac-japanese-basic-variant #x20001
  "MacJapanese text encoding without Apple double-byte extensions.")

(declare-function mac-code-convert-string "mac.c"
		  (string source target &optional normalization-form))
(declare-function mac-convert-property-list "mac.c"
                  (property-list &optional format hash-bound))

(defun mac-utxt-to-string (data &optional coding-system source-encoding)
  (or coding-system (setq coding-system mac-system-coding-system))
  (let* ((encoding
	  (and (eq (coding-system-base coding-system) 'japanese-shift-jis)
	       mac-text-encoding-mac-japanese-basic-variant))
	 (str (let (bytes data1 data-normalized data1-normalized)
		(and (setq bytes (mac-code-convert-string
				  data source-encoding
				  (or encoding coding-system)))
		     ;; Check if round-trip conversion gives an
		     ;; equivalent result.  We use HFS+D instead of
		     ;; NFD so OHM SIGN and GREEK CAPITAL LETTER OMEGA
		     ;; may not be considered equivalent.
		     (setq data1 (mac-code-convert-string
				  bytes (or encoding coding-system)
				  source-encoding))
		     (or (string= data1 data)
			 (and (setq data-normalized (mac-code-convert-string
						     data source-encoding
						     source-encoding 'HFS+D))
			      (setq data1-normalized (mac-code-convert-string
						      data1 source-encoding
						      source-encoding 'HFS+D))
			      (string= data1-normalized data-normalized)))
                     (decode-coding-string bytes coding-system)))))
    (when str
      (cond ((eq coding-system 'mac-roman)
             ;; mac-roman, being based on glibc charmaps data, maps
             ;; APPLE LOGO to U+E01E.
             (subst-char-in-string ?\xe01e ? str t))
            ((eq encoding mac-text-encoding-mac-japanese-basic-variant)
             ;; Does it contain Apple one-byte extensions other than
             ;; reverse solidus?
             (if (string-match
                  ;; Character alternatives in multibyte eight-bit is
                  ;; unreliable.  (Bug#3687)
                  ;; (string-to-multibyte "[\xa0\xfd-\xff]")
                  (string-to-multibyte "\xa0\\|\xfd\\|\xfe\\|\xff") str)
                 (setq str nil)
               ;; ASCII-only?
               (unless (mac-code-convert-string data source-encoding
                                                mac-text-encoding-ascii)
                 (subst-char-in-string ?\x5c ?\¥ str t)
                 (subst-char-in-string (unibyte-char-to-multibyte ?\x80) ?\\
                                       str t))))))
    (or str (decode-coding-string data
				  (or source-encoding
				      (if (eq (byteorder) ?B)
					  'utf-16be 'utf-16le))))))

(defun mac-TIFF-to-string (data &optional text)
  (let* ((image (create-image data 'image-io t))
         (metadata (image-metadata image)))
    (unless (plist-get metadata 'count)
      (let* ((props (plist-get metadata 'image-io-properties-at-index))
             ;; We used to use kCGImagePropertyTIFFResolutionUnit, but
             ;; this is not correct for screenshots taken with
             ;; mirroring a Retina display to a non-Retina one.
             (dpi-width (cdr (assoc "DPIWidth" props)))
             (dpi-height (cdr (assoc "DPIHeight" props))))
        (if (numberp dpi-width)
            (setq image `(,@image
                          :width ,(round (/ (cdr (assoc "PixelWidth" props))
                                            (/ dpi-width 72.0))))))
        (if (numberp dpi-height)
            (setq image `(,@image
                          :height ,(round (/ (cdr (assoc "PixelHeight" props))
                                             (/ dpi-height 72.0))))))))
    (propertize (or text " ") 'display image)))

(defun mac-PDF-to-string (data &optional text)
  (propertize (or text " ") 'display (create-image data 'image-io t)))

(defun mac-pasteboard-string-to-string (data &optional coding-system)
  (mac-utxt-to-string data coding-system 'utf-8))

(defun mac-string-to-pasteboard-string (string &optional coding-system)
  (or coding-system (setq coding-system mac-system-coding-system))
  (let (data encoding)
    (when (memq (coding-system-base coding-system)
		(find-coding-systems-string string))
      (let ((str string))
	(cond ((eq coding-system 'mac-roman)
               ;; mac-roman, being based on glibc charmaps data, maps
               ;; APPLE LOGO to U+E01E.
               (if (string-match "\xe01e" str)
                   (setq str nil)))
              ((eq coding-system 'japanese-shift-jis)
               (setq encoding mac-text-encoding-mac-japanese-basic-variant)
               (setq str
                     (subst-char-in-string ?\\ (unibyte-char-to-multibyte ?\x80)
                                           str))
               (subst-char-in-string ?\¥ ?\x5c str t)
               ;; ASCII-only?
               (if (string-match "\\`[\x00-\x7f]*\\'" str)
                   (setq str nil))))
	(and str
	     (setq data (mac-code-convert-string
			 (encode-coding-string str coding-system)
			 (or encoding coding-system) 'utf-8)))))
    (or data (encode-coding-string string 'utf-8))))

(defun mac-local-file-name-to-file-url (filename)
  (concat "file://"
          (mapconcat 'url-hexify-string (split-string filename "/") "/")))

(defun mac-pasteboard-filenames-to-file-urls (data)
  (setq data (mac-convert-property-list data))
  (when (vectorp data)
    (mapcar (lambda (filename)
              (when (stringp filename)
                (mac-local-file-name-to-file-url filename)))
            data)))


;;;; Selections

(defun mac-selection-value-internal (type)
  (let ((request-type 'NSStringPboardType)
	text)
    (with-demoted-errors "mac-selection-value-internal: %S"
      (if (consp request-type)
          (while (and request-type (not text))
            (setq text (gui-get-selection type (car request-type)))
            (setq request-type (cdr request-type)))
        (setq text (gui-get-selection type request-type))))
    (if text
	(remove-text-properties 0 (length text) '(foreign-selection nil) text))
    (with-demoted-errors "mac-selection-value-internal: %S"
      (let ((image-type-converters
             '(;; For the "Scan Documents" context menu via Continuity
               ;; Camera on macOS 10.14.
               (NSPasteboardTypePDF . mac-PDF-to-string)
               (NSTIFFPboardType . mac-TIFF-to-string)))
            image-data)
        (while (and image-type-converters (null image-data))
          (setq image-data
                (gui-get-selection type (caar image-type-converters)))
          (when image-data
            (remove-text-properties 0 (length image-data)
                                    '(foreign-selection nil) image-data)
            (if text
                (setq text (list text (funcall (cdar image-type-converters)
                                               image-data text)))
              (setq text (funcall (cdar image-type-converters) image-data))))
          (setq image-type-converters (cdr image-type-converters)))))
    text))

(define-obsolete-function-alias 'x-cut-buffer-or-selection-value
  'x-selection-value "24.1")

;; Arrange for the kill and yank functions to set and check the clipboard.

(defun mac-setup-selection-properties ()
  (put 'CLIPBOARD 'mac-pasteboard-name
       "Apple CFPasteboard general")	; NSGeneralPboard
  (put 'FIND 'mac-pasteboard-name
       "Apple CFPasteboard find")	; NSFindPboard
  (put 'PRIMARY 'mac-pasteboard-name
       (format "GNU Emacs CFPasteboard primary %d" (emacs-pid)))
  (put 'NSStringPboardType 'mac-pasteboard-data-type "NSStringPboardType")
  (put 'NSTIFFPboardType 'mac-pasteboard-data-type
       "NeXT TIFF v4.0 pasteboard type")
  (put 'NSFilenamesPboardType 'mac-pasteboard-data-type
       "NSFilenamesPboardType")
  (put 'NSPasteboardTypePDF 'mac-pasteboard-data-type "com.adobe.pdf"))

(defun mac-select-convert-to-string (selection type value)
  (let ((str (xselect-convert-to-string selection nil value))
	(coding (or next-selection-coding-system selection-coding-system)))
    (when str
      ;; If TYPE is nil, this is a local request; return STR as-is.
      (if (null type)
	  str
	;; Otherwise, encode STR.
	(let ((inhibit-read-only t))
	  (remove-text-properties 0 (length str) '(composition nil) str)
	  (cond
	   ((eq type 'NSStringPboardType)
	    (setq str (mac-string-to-pasteboard-string str coding)))
	   (t
	    (error "Unknown selection type: %S" type))))

	(setq next-selection-coding-system nil)
	(cons type str)))))

(defun mac-select-convert-to-pasteboard-filenames (selection type value)
  (let ((filename (xselect-convert-to-filename selection type value)))
    (and filename
	 (cons type (mac-convert-property-list `(array . [(string . ,filename)])
                                               'xml1)))))

(setq selection-converter-alist
      (nconc
       '((NSStringPboardType . mac-select-convert-to-string)
	 (NSTIFFPboardType . nil)
	 (NSFilenamesPboardType . mac-select-convert-to-pasteboard-filenames)
         (NSPasteboardTypePDF . nil))
       selection-converter-alist))

;;;; Apple events, Action events, and Services menu

(defvar mac-startup-options)
(declare-function mac-send-apple-event-internal "macselect.c"
		  (apple-event &optional send-mode))
(declare-function mac-start-animation "macfns.c"
		  (frame-or-window &rest properties))

;;; Event classes
(put 'core-event     'mac-apple-event-class "aevt") ; kCoreEventClass
(put 'internet-event 'mac-apple-event-class "GURL") ; kAEInternetEventClass
(put 'odb-editor-suite  'mac-apple-event-class "R*ch") ; kODBEditorSuite

;;; Event IDs
;; kCoreEventClass
(put 'open-application     'mac-apple-event-id "oapp") ; kAEOpenApplication
(put 'reopen-application   'mac-apple-event-id "rapp") ; kAEReopenApplication
(put 'open-documents       'mac-apple-event-id "odoc") ; kAEOpenDocuments
(put 'print-documents      'mac-apple-event-id "pdoc") ; kAEPrintDocuments
(put 'open-contents        'mac-apple-event-id "ocon") ; kAEOpenContents
(put 'quit-application     'mac-apple-event-id "quit") ; kAEQuitApplication
(put 'application-died     'mac-apple-event-id "obit") ; kAEApplicationDied
(put 'show-preferences     'mac-apple-event-id "pref") ; kAEShowPreferences
(put 'autosave-now         'mac-apple-event-id "asav") ; kAEAutosaveNow
(put 'answer               'mac-apple-event-id "ansr") ; kAEAnswer
;; kAEInternetEventClass
(put 'get-url              'mac-apple-event-id "GURL") ; kAEGetURL
;; kODBEditorSuite
(put 'modified-file        'mac-apple-event-id "FMod") ; kAEModifiedFile
(put 'closed-file          'mac-apple-event-id "FCls") ; kAEClosedFile

(defmacro mac-event-spec (event)
  `(nth 1 ,event))

(defmacro mac-event-ae (event)
  `(nth 2 ,event))

(declare-function mac-coerce-ae-data "mac.c" (src-type src-data dst-type))

(defun mac-ae-parameter (ae &optional keyword type)
  (or keyword (setq keyword "----")) ;; Direct object.
  (if (not (and (consp ae) (equal (car ae) "aevt")))
      (error "Not an Apple event: %S" ae)
    (let ((type-data (cdr (assoc keyword (cdr ae))))
	  data)
      (when (and type type-data (not (equal type (car type-data))))
	(setq data (mac-coerce-ae-data (car type-data) (cdr type-data) type))
	(setq type-data (if data (cons type data) nil)))
      type-data)))

(defun mac-ae-list (ae &optional keyword type)
  (or keyword (setq keyword "----")) ;; Direct object.
  (let ((desc (mac-ae-parameter ae keyword "list")))
    (cond ((null desc)
	   nil)
	  ((not (equal (car desc) "list"))
	   (error "Parameter for \"%s\" is not a list" keyword))
	  (t
	   (if (null type)
	       (cdr desc)
	     (mapcar
	      (lambda (type-data)
		(mac-coerce-ae-data (car type-data) (cdr type-data) type))
	      (cdr desc)))))))

(defun mac-ae-number (ae keyword)
  (let ((type-data (mac-ae-parameter ae keyword))
	str)
    (if (and type-data
	     (setq str (mac-coerce-ae-data (car type-data)
					   (cdr type-data) "TEXT")))
	(let ((num (string-to-number str)))
	  ;; Mac OS Classic may return "0e+0" as the coerced value for
	  ;; the type "magn" and the data "\000\000\000\000".
	  (if (= num 0.0) 0 num))
      nil)))

(defun mac-bytes-to-integer (bytes &optional from to)
  (or from (setq from 0))
  (or to (setq to (length bytes)))
  (let* ((len (- to from))
	 (extended-sign-len (- (1+ (ceiling (log most-positive-fixnum 2)))
			       (* 8 len)))
	 (result 0))
    (dotimes (i len)
      (setq result (logior (lsh result 8)
			   (aref bytes (+ from (if (eq (byteorder) ?B) i
						 (- len i 1)))))))
    (if (> extended-sign-len 0)
	(ash (lsh result extended-sign-len) (- extended-sign-len))
      result)))

(defun mac-ae-selection-range (ae)
;; #pragma options align=mac68k
;; typedef struct SelectionRange {
;;   short unused1; // 0 (not used)
;;   short lineNum; // line to select (<0 to specify range)
;;   long startRange; // start of selection range (if line < 0)
;;   long endRange; // end of selection range (if line < 0)
;;   long unused2; // 0 (not used)
;;   long theDate; // modification date/time
;; } SelectionRange;
;; #pragma options align=reset
  (let ((range-bytes (cdr (mac-ae-parameter ae "kpos" "TEXT"))))
    (and range-bytes
	 (list (mac-bytes-to-integer range-bytes 2 4)
	       (mac-bytes-to-integer range-bytes 4 8)
	       (mac-bytes-to-integer range-bytes 8 12)
	       (mac-bytes-to-integer range-bytes 16 20)))))

;; On Mac OS X 10.4 and later, the `open-document' event contains an
;; optional parameter keyAESearchText from the Spotlight search.
(defun mac-ae-text-for-search (ae)
  (let ((utf8-text (cdr (mac-ae-parameter ae "stxt" "utf8"))))
    (and utf8-text
	 (decode-coding-string utf8-text 'utf-8))))

(defun mac-ae-text (ae)
  (or (cdr (mac-ae-parameter ae nil "TEXT"))
      (error "No text in Apple event.")))

(defun mac-ae-script-language (ae keyword)
;; struct WritingCode {
;;   ScriptCode          theScriptCode;
;;   LangCode            theLangCode;
;; };
  (let ((bytes (cdr (mac-ae-parameter ae keyword "intl"))))
    (and bytes
	 (cons (mac-bytes-to-integer bytes 0 2)
	       (mac-bytes-to-integer bytes 2 4)))))

(defconst mac-keyboard-modifier-mask-alist
  (mapcar
   (lambda (modifier-bit)
     (cons (car modifier-bit) (lsh 1 (cdr modifier-bit))))
   '((command  . 8)			; cmdKeyBit
     (shift    . 9)			; shiftKeyBit
     (option   . 11)			; optionKeyBit
     (control  . 12)			; controlKeyBit
     (function . 17)))			; kEventKeyModifierFnBit
  "Alist of Mac keyboard modifier symbols vs masks.")

(defun mac-ae-keyboard-modifiers (ae)
  (let ((modifiers-value (mac-ae-number ae "kmod"))
	modifiers)
    (if modifiers-value
	(dolist (modifier-mask mac-keyboard-modifier-mask-alist)
	  (if (/= (logand modifiers-value (cdr modifier-mask)) 0)
	      (setq modifiers (cons (car modifier-mask) modifiers)))))
    modifiers))

(defun mac-ae-type-string (ae keyword)
  (let ((bytes (cdr (mac-ae-parameter ae keyword "type"))))
    (and bytes
	 (if (eq (byteorder) ?B)
	     bytes
	   (apply 'unibyte-string (nreverse (append bytes '())))))))

(defun mac-ae-open-application (event)
  "Open the application Emacs with the Apple event EVENT.
It records the Apple event in `mac-startup-options' as a value
for the key symbol `apple-event' so it can be inspected later."
  (interactive "e")
  (push (cons 'apple-event (mac-event-ae event)) mac-startup-options))

(declare-function mac-application-state "macfns.c" ())

(defun mac-ae-reopen-application (_event)
  "Show some frame in response to the Apple event EVENT.
The frame to be shown is chosen from visible or iconified frames
if possible.  If there's no such frame, a new frame is created."
  (interactive "e")
  ;; OS X 10.10 sometimes makes hidden frames visible after the call
  ;; to this function.
  (let ((count 6))
    (while (and (> count 0)
		(plist-get (mac-application-state) :hidden-p))
      (sit-for 0.017)
      (setq count (1- count))))
  (unless (frame-visible-p (selected-frame))
    (let ((frame (or (car (visible-frame-list))
		     (car (filtered-frame-list 'frame-visible-p)))))
      (if frame
	  (select-frame frame)
	(switch-to-buffer-other-frame "*scratch*"))))
  (select-frame-set-input-focus (selected-frame)))

(defun mac-ae-open-documents (event)
  "Open the documents specified by the Apple event EVENT."
  (interactive "e")
  (let ((ae (mac-event-ae event)))
    (dolist (filename (mac-ae-list ae nil 'undecoded-file-name))
      (when filename
        (dnd-open-local-file (mac-local-file-name-to-file-url filename) nil)))
    (let ((selection-range (mac-ae-selection-range ae))
	  (search-text (mac-ae-text-for-search ae)))
      (cond (selection-range
	     (let ((line (car selection-range))
		   (start (cadr selection-range))
		   (end (nth 2 selection-range)))
	       (if (>= line 0)
		   (progn
		     (goto-char (point-min))
		     (forward-line line)) ; (1- (1+ line))
		 (if (and (>= start 0) (>= end 0))
		     (progn (set-mark (1+ start))
			    (goto-char (1+ end)))))))
	    ((stringp search-text)
	     (re-search-forward
	      (mapconcat 'regexp-quote (split-string search-text) "\\|")
	      nil t))))
    (mac-odb-setup-buffer ae))
  (select-frame-set-input-focus (selected-frame)))

(declare-function mac-resume-apple-event "macselect.c"
		  (apple-event &optional error-code))

(defun mac-ae-quit-application (event)
  "Quit the application Emacs with the Apple event EVENT."
  (interactive "e")
  (let ((ae (mac-event-ae event)))
    (unwind-protect
	(save-buffers-kill-emacs)
      ;; Reaches here if the user has canceled the quit.
      (mac-resume-apple-event ae -128)))) ; userCanceledErr

(defun mac-type-string-to-bytes (type-string)
  (unless (and (stringp type-string) (= (string-bytes type-string) 4))
    (error "Invalid type string (not a 4-byte string): %S" type-string))
  (if (eq (byteorder) ?l)
      (setq type-string
	    (apply 'unibyte-string (nreverse (append type-string '())))))
  type-string)

(defun mac-create-apple-event (event-class event-id target)
  "Create a Lisp representation of an Apple event.
EVENT-CLASS (or EVENT-ID) represents the Apple event class (or
ID, resp.).  It should be either a 4-byte string or a symbol
whose `mac-apple-event-class' (or `mac-apple-event-id', resp.)
property is a 4-byte string.

TARGET specifies the target application for the event.  It should
be either a Lisp representation of the target address Apple event
descriptor (e.g., (cons \"sign\" (mac-type-string-to-bytes
\"MACS\")) for Finder), an integer specifying the process ID, or
a string specifying bundle ID on Mac OS X 10.3 and later (e.g.,
\"com.apple.Finder\") or an application URL with the `eppc'
scheme."
  (if (symbolp event-class)
      (setq event-class (get event-class 'mac-apple-event-class)))
  (if (symbolp event-id)
      (setq event-id (get event-id 'mac-apple-event-id)))
  (cond ((numberp target)
	 (setq target (cons "kpid"	; typeKernelProcessID
			    (mac-coerce-ae-data
			     "TEXT" (number-to-string target) "long"))))
	((stringp target)
	 (if (string-match "\\`eppc://" target)
	     (setq target (cons "aprl"	; typeApplicationURL
				(encode-coding-string target 'utf-8)))
	   (setq target (cons "bund"	; typeApplicationBundleID
			      (encode-coding-string target 'utf-8)))))
	((not (and (consp target) (stringp (car target))
		   (= (string-bytes (car target)) 4)))
	 (error "Not an address descriptor: %S" target)))
  `("aevt" . ((event-class . ("type" . ,(mac-type-string-to-bytes event-class)))
	      (event-id . ("type" . ,(mac-type-string-to-bytes event-id)))
	      (address . ,target))))

(defun mac-ae-set-parameter (apple-event keyword descriptor)
  "Set parameter KEYWORD to DESCRIPTOR on APPLE-EVENT.
APPLE-EVENT is a Lisp representation of an Apple event,
preferably created with `mac-create-apple-event'.  See
`mac-ae-set-reply-parameter' for the formats of KEYWORD and
DESCRIPTOR.  If KEYWORD is a symbol starting with `:', then
DESCRIPTOR can be any Lisp value.  In that case, the value is not
sent as an Apple event, but you can use it in the reply handler.
See `mac-send-apple-event' for such usage.  This function
modifies APPLE-EVENT as a side effect and returns APPLE-EVENT."
  (if (not (and (consp apple-event) (equal (car apple-event) "aevt")))
      (error "Not an Apple event: %S" apple-event)
    (setcdr apple-event
	    (cons (cons keyword descriptor)
		  (delete keyword (cdr apple-event))))
    apple-event))

(defvar mac-sent-apple-events nil
  "List of previously sent Apple events.")

(defun mac-send-apple-event (apple-event &optional callback)
  "Send APPLE-EVENT.  CALLBACK is called when its reply arrives.
APPLE-EVENT is a Lisp representation of an Apple event,
preferably created with `mac-create-apple-event' and
`mac-ae-set-parameter'.

If CALLBACK is nil, then the reply is not requested.  If CALLBACK
is t, then the function does not return until the reply arrives.
Otherwise, the function returns without waiting for the arrival
of the reply, and afterwards CALLBACK is called with one
argument, the reply event, when the reply arrives.

The function returns an integer if some error has occurred in
sending, the Lisp representation of the sent Apple event if
CALLBACK is not t, and the Lisp representation of the reply Apple
event if CALLBACK is t.

With the reply event, which is given either as the argument to
CALLBACK or as the return value in the case that CALLBACK is t,
one can examine the originally sent Apple event via the
`original-apple-event' parameter.

Each event parameter whose keyword is a symbol starting with `:'
is not sent as an Apple event, but given as a parameter of the
original event to CALLBACK.  So, you can pass context information
through this without \"emulated closures\".  For example,

  (let ((apple-event (mac-create-apple-event ...)))
    (mac-ae-set-parameter apple-event :context1 SOME-LISP-VALUE1)
    (mac-ae-set-parameter apple-event :context2 SOME-LISP-VALUE2)
    (mac-send-apple-event
     apple-event
     (lambda (reply-ae)
       (let* ((orig-ae (mac-ae-parameter reply-ae 'original-apple-event))
              (context1-value (mac-ae-parameter orig-ae :context1))
	      (context2-value (mac-ae-parameter orig-ae :context2)))
	 ...))))"
  (let ((ae (mac-send-apple-event-internal apple-event (and callback t))))
    (when (consp ae)
      (let ((params (cdr ae)))
	(dolist (param (cdr apple-event))
	  (if (not (assoc (car param) params))
	      (setcdr ae (cons param (cdr ae))))))
      (cond ((null callback))		; no reply
	    ((not (eq callback t))	; asynchronous
	     (push (mac-ae-set-parameter ae 'callback callback)
		   mac-sent-apple-events))
	    (t				; synchronous
	     (push (mac-ae-set-parameter ae 'callback
					 (lambda (reply-ae)
					   (throw 'mac-send-apple-event
						  reply-ae)))
		   mac-sent-apple-events)
	     (let ((orig-ae ae))
	       (unwind-protect
		   (let (reply-ae events)
		     (while (null (setq reply-ae (catch 'mac-send-apple-event
						   (push (read-event) events)
						   nil))))
		     (setq unread-command-events (nreverse events))
		     (setq ae reply-ae))
		 (mac-ae-set-parameter orig-ae 'callback 'ignore))))))
    ae))

(defun mac-ae-answer (event)
  "Handle the reply EVENT for a previously sent Apple event."
  (interactive "e")
  (let* ((reply-ae (mac-event-ae event))
	 (return-id (mac-ae-parameter reply-ae 'return-id "shor"))
	 (rest mac-sent-apple-events)
	 prev)
    (while (and rest
		(not (equal (mac-ae-parameter (car rest) 'return-id "shor")
			    return-id)))
      (setq rest (cdr (setq prev rest))))
    (when rest
      (if prev
	  (setcdr prev (cdr rest))
	(setq mac-sent-apple-events (cdr mac-sent-apple-events)))
      (funcall (mac-ae-parameter (car rest) 'callback)
	       (mac-ae-set-parameter reply-ae 'original-apple-event
				     (car rest))))))

;; url-generic-parse-url is autoloaded from url-parse.
(declare-function url-type "url-parse" t t) ; defstruct
(declare-function org-protocol-check-filename-for-protocol "org-protocol.el"
		  (fname restoffiles client))

(defun mac-ae-get-url (event)
  "Open the URL specified by the Apple event EVENT.
Currently the `mailto' and `org-protocol' schemes are supported."
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (url (mac-ae-text ae)))
    (cond ((string-match "\\`mailto:" url)
	   (url-mailto (url-generic-parse-url url))
	   (select-frame-set-input-focus (selected-frame)))
	  ((string-match "\\`org-protocol:" url)
	   (require 'org-protocol)
	   ;; URL of the form `org-protocol://sub-protocol:/...' might
	   ;; have been standardized to
	   ;; `org-protocol://sub-protocol/...' .
	   (if (and (string-match "\\`org-protocol:/+[^/]+/" url)
		    (not (eq (aref url (- (match-end 0) 2)) ?:)))
	       (setq url (concat (substring url 0 (1- (match-end 0)))
				 ":" (substring url (1- (match-end 0))))))
           (let ((file-path nil))
             (condition-case err
                 (setq file-path (org-protocol-check-filename-for-protocol url (list url) nil))
	       (error
	        (message "%s" (error-message-string err))))
             (if (stringp file-path) (switch-to-buffer (find-file-noselect file-path))))
	   (select-frame-set-input-focus (selected-frame)))
	  (t
	   (mac-resume-apple-event ae t)))))

(setq mac-apple-event-map (make-sparse-keymap))

;; Received when Emacs is launched without associated documents.
;; We record the received Apple event in `mac-startup-options' as a
;; value for the key symbol `apple-event' so we can inspect some
;; parameters such as keyAEPropData ("prdt") that may contain
;; keyAELaunchedAsLogInItem ("lgit") or keyAELaunchedAsServiceItem
;; ("svit") on Mac OS X 10.4 and later.  You should use
;; `mac-ae-type-string' for extracting data for keyAEPropData in order
;; to take byte order into account.
(define-key mac-apple-event-map [core-event open-application]
  'mac-ae-open-application)

;; Received when a dock or application icon is clicked and Emacs is
;; already running.
(define-key mac-apple-event-map [core-event reopen-application]
  'mac-ae-reopen-application)

(define-key mac-apple-event-map [core-event open-documents]
  'mac-ae-open-documents)
(define-key mac-apple-event-map [core-event show-preferences] 'customize)
(define-key mac-apple-event-map [core-event quit-application]
  'mac-ae-quit-application)
(define-key mac-apple-event-map [core-event answer] 'mac-ae-answer)

(define-key mac-apple-event-map [internet-event get-url] 'mac-ae-get-url)

;;; ODB Editor Suite
(defvar mac-odb-received-apple-events nil
  "List of received Apple Events containing ODB Editor Suite parameters.")
(put 'mac-odb-received-apple-events 'permanent-local t)

(defun mac-odb-setup-buffer (apple-event)
  (when (or (mac-ae-parameter apple-event "FSnd") ; keyFileSender
	    ;; (mac-ae-parameter apple-event "FSfd") ; keyFileSenderDesc
	    )
    (make-local-variable 'mac-odb-received-apple-events)
    (let ((custom-path (mac-ae-parameter apple-event "Burl" "utxt")))
      (if custom-path
	  (rename-buffer (mac-utxt-to-string (cdr custom-path)) t)))
    (mac-ae-set-parameter apple-event :original-file-name buffer-file-name)
    (push apple-event mac-odb-received-apple-events)
    (add-hook 'after-save-hook 'mac-odb-send-modified-file-events nil t)
    (add-hook 'kill-buffer-hook 'mac-odb-send-closed-file-events nil t)
    (add-hook 'kill-emacs-hook 'mac-odb-send-closed-file-events-all-buffers)
    (run-hooks 'mac-odb-setup-buffer-hook)))

(defun mac-odb-cleanup-buffer ()
  (run-hooks 'mac-odb-cleanup-buffer-hook)
  (remove-hook 'after-save-hook 'mac-odb-send-modified-file-events t)
  (remove-hook 'kill-buffer-hook 'mac-odb-send-closed-file-events t)
  (kill-local-variable 'mac-odb-received-apple-events))

(defun mac-odb-file-sender-desc (apple-event)
  (or ;; (mac-ae-parameter apple-event "FSfd")
      (let ((sign-bytes (cdr (mac-ae-parameter apple-event "FSnd" "type"))))
	(and sign-bytes (cons "sign" sign-bytes)))))

(defun mac-odb-send-modified-file-events ()
  (dolist (orig-ae mac-odb-received-apple-events)
    (let ((original-file (mac-ae-parameter orig-ae))
	  (sender-token (mac-ae-parameter orig-ae "FTok"))
	  (original-file-name (mac-ae-parameter orig-ae :original-file-name))
	  (apple-event
	   (mac-create-apple-event 'odb-editor-suite 'modified-file
				   (mac-odb-file-sender-desc orig-ae))))
      (mac-ae-set-parameter apple-event "----" original-file)
      (if (not (equal original-file-name buffer-file-name))
	  (let* ((coding (or file-name-coding-system
			     default-file-name-coding-system))
		 (file-desc-type (car original-file))
		 (new-location
		  (cons file-desc-type
			(mac-coerce-ae-data 'undecoded-file-name
					    (encode-coding-string
					     buffer-file-name coding)
					    file-desc-type))))
	    (mac-ae-set-parameter apple-event "New?" new-location)))
      (if sender-token
	  (mac-ae-set-parameter apple-event "FTok" sender-token))
      (mac-send-apple-event apple-event))))

(defun mac-odb-send-closed-file-events ()
  (dolist (orig-ae mac-odb-received-apple-events)
    (let ((original-file (mac-ae-parameter orig-ae))
	  (sender-token (mac-ae-parameter orig-ae "FTok"))
	  (apple-event
	   (mac-create-apple-event 'odb-editor-suite 'closed-file
				   (mac-odb-file-sender-desc orig-ae))))
      (mac-ae-set-parameter apple-event "----" original-file)
      (if sender-token
	  (mac-ae-set-parameter apple-event "FTok" sender-token))
      (mac-send-apple-event apple-event)))
  (mac-odb-cleanup-buffer))

(defun mac-odb-send-closed-file-events-all-buffers ()
  (save-excursion
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (if mac-odb-received-apple-events
	  (mac-odb-send-closed-file-events)))))

;;; AppleScript
(declare-function mac-osa-script "mac.c"
		  (code-or-file &optional compiled-p-or-language file-p
				value-form handler-call &rest args))

(defvar mac-do-applescript-use-legacy-encoding nil
  "Non-nil means `mac-do-applescript' uses legacy encoding for unibyte scripts.
If this value is nil or the script given to `mac-do-applescript'
is a multibyte string, it is regarded as a Unicode text.
Otherwise, the script is regarded as a byte sequence in a Mac
traditional encoding specified by `mac-system-script-code', just
as in the Carbon(+AppKit) ports of Emacs 22 or the Mac port of
Emacs 23-24.4.  In the latter case, the return value or the error
message of `mac-do-applescript' is also a unibyte string in the
Mac traditional encoding.

Note that if this value is non-nil, a unibyte ASCII-only script
does not always have the same meaning as the multibyte
counterpart.  For example, `\\x5c' in a unibyte script is
interpreted as a yen sign when the value of
`mac-system-script-code' is 1 (smJapanese), but the same
character in a multibyte script is interpreted as a reverse
solidus.  You may want to apply `string-to-multibyte' to the
script if it is given as an ASCII-only string literal.")

(defun mac-do-applescript (script)
  "Compile and execute AppleScript SCRIPT and return the result.
If compilation and execution are successful, the resulting script
value is returned as a string.  Otherwise the function aborts and
displays the error message returned by the AppleScript scripting
component.

Set `mac-do-applescript-use-legacy-encoding' to t if you want
strict compatibility with the Carbon(+AppKit) ports of Emacs 22
or the Mac port of Emacs 23-24.4 about encoding of SCRIPT."
  (let ((use-legacy-encoding (and mac-do-applescript-use-legacy-encoding
				  (not (multibyte-string-p script)))))
    (condition-case err
	(if (not use-legacy-encoding)
	    (mac-osa-script script)
	  (setq script (decode-coding-string
			(mac-coerce-ae-data "TEXT" script "utf8") 'utf-8))
	  (mac-coerce-ae-data "utf8" (encode-coding-string
				      (mac-osa-script script) 'utf-8) "TEXT"))
      (error
       (if (equal (cadr err) "OSA script error")
	   (error "AppleScript error %d" (cdr (assq 'number (cddr err))))
	 (error "%s" (if use-legacy-encoding
			 (mac-coerce-ae-data "utf8" (encode-coding-string
						     (cadr err) 'utf-8) "TEXT")
		       (cadr err))))))))

(defalias 'do-applescript 'mac-do-applescript)

;;; Font panel
(declare-function mac-set-font-panel-visible-p "macfns.c" (flag))

(define-minor-mode mac-font-panel-mode
  "Toggle font panel display (Mac Font Panel mode)."
  :init-value nil
  :global t
  :group 'mac
  (mac-set-font-panel-visible-p mac-font-panel-mode))

(defun mac-handle-font-panel-closed (_event)
  "Update internal status in response to font panel closed EVENT."
  (interactive "e")
  ;; Synchronize with the minor mode variable.
  (mac-font-panel-mode 0))

(defun mac-handle-font-selection (event)
  "Change default face attributes according to font selection EVENT."
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (font-spec (cdr (mac-ae-parameter ae 'font-spec))))
    (if font-spec
	(set-face-attribute 'default (selected-frame) :font font-spec))))

;; kEventClassFont/kEventFontPanelClosed
(define-key mac-apple-event-map [font panel-closed]
  'mac-handle-font-panel-closed)
;; kEventClassFont/kEventFontSelection
(define-key mac-apple-event-map [font selection] 'mac-handle-font-selection)

(define-key-after menu-bar-showhide-menu [mac-font-panel-mode]
  (menu-bar-make-mm-toggle mac-font-panel-mode
			   "Font Panel"
			   "Show the font panel as a floating dialog")
  'showhide-speedbar)

;;; Text Services
(declare-function mac-select-input-source "macfns.c"
		  (source &optional set-keyboard-layout-override-p))

(setq mac-ts-active-input-overlay (make-overlay 1 1))
(overlay-put mac-ts-active-input-overlay 'display "")

(defvar mac-ts-active-input-string ""
  "String of text in Mac TSM active input area.")

(defface mac-ts-caret-position
  '((t :inverse-video t))
  "Face for caret position in Mac TSM active input area.
This is used when the active input area is displayed either in
the echo area or in a buffer where the cursor is not displayed."
  :group 'mac)

(defface mac-ts-raw-text
  '((t :underline t))
  "Face for raw text in Mac TSM active input area."
  :group 'mac)

(defface mac-ts-converted-text
  '((((background dark)) :underline "gray60")
    (t :underline "gray80"))
  "Face for converted text in Mac TSM active input area."
  :group 'mac)

(defface mac-ts-selected-converted-text
  '((t :underline t))
  "Face for selected converted text in Mac TSM active input area."
  :group 'mac)

(defun mac-split-string-by-property-change (string)
  (let ((tail (length string))
	head result)
    (unless (= tail 0)
      (while (setq head (previous-property-change tail string)
		   result (cons (substring string (or head 0) tail) result)
		   tail head)))
    result))

(defun mac-replace-untranslated-utf-8-chars (string &optional to-string)
  (or to-string (setq to-string "�"))
  (mapconcat
   (lambda (str)
     (if (get-text-property 0 'untranslated-utf-8 str) to-string str))
   (mac-split-string-by-property-change string)
   ""))

(defun mac-keyboard-translate-char (ch)
  (if (and (characterp ch)
	   (or (char-table-p keyboard-translate-table)
	       (and (or (stringp keyboard-translate-table)
			(vectorp keyboard-translate-table))
		    (> (length keyboard-translate-table) ch))))
      (or (aref keyboard-translate-table ch) ch)
    ch))

(defun mac-unread-string (string)
  ;; The commit 30a6b1f8141 obviates store-kbd-macro-event calls that
  ;; had been made via isearch-unread.
  (setq unread-command-events
        (nconc (mapcar 'mac-keyboard-translate-char
                       (mac-replace-untranslated-utf-8-chars string))
               unread-command-events)))

(defconst mac-marked-text-underline-style-faces
  '((0 . mac-ts-raw-text)		  ; NSUnderlineStyleNone
    (1 . mac-ts-converted-text)		  ; NSUnderlineStyleSingle
    (2 . mac-ts-selected-converted-text)) ; NSUnderlineStyleThick
  "Alist of NSUnderlineStyle vs Emacs face in marked text.")

(defun mac-text-input-set-marked-text (event)
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (text (cdr (mac-ae-parameter ae)))
	 (selected-range (cdr (mac-ae-parameter ae "selectedRange")))
	 (replacement-range (cdr (mac-ae-parameter ae "replacementRange")))
	 (script-language (mac-ae-script-language ae "tssl"))
	 (coding (and script-language
		      (or (cdr (assq (car script-language)
				     mac-script-code-coding-systems))
			  'mac-roman))))
    (let ((use-echo-area
	   (or isearch-mode
	       (and cursor-in-echo-area (current-message))
	       ;; Overlay strings are not shown in some cases.
	       (get-char-property (point) 'invisible)
	       (and (not (bobp))
		    (or (and (get-char-property (point) 'display)
			     (eq (get-char-property (1- (point)) 'display)
				 (get-char-property (point) 'display)))
			(and (get-char-property (point) 'composition)
			     (eq (get-char-property (1- (point)) 'composition)
				 (get-char-property (point) 'composition)))))))
	  active-input-string caret-seen)
      ;; Decode the active input area text with inheriting faces and
      ;; the caret position.
      (put-text-property (* (car selected-range) 2) (length text)
			 'cursor t text)
      (setq active-input-string
	    (mapconcat
	     (lambda (str)
	       (let* ((decoded (mac-utxt-to-string str coding))
		      (underline-style
		       (or (cdr (get-text-property 0 'NSUnderline str)) 0))
		      (face
		       (cdr (assq underline-style
				  mac-marked-text-underline-style-faces))))
		 (put-text-property 0 (length decoded) 'face face decoded)
		 (when (and (not caret-seen)
			    (get-text-property 0 'cursor str))
		   (setq caret-seen t)
		   (if (or use-echo-area (null cursor-type))
		       (put-text-property 0 1 'face 'mac-ts-caret-position
					  decoded)
		     (put-text-property 0 1 'cursor t decoded)))
		 decoded))
	     (mac-split-string-by-property-change text)
	     ""))
      (put-text-property 0 (length active-input-string)
			 'mac-ts-active-input-string t active-input-string)
      (if use-echo-area
	  (let ((msg (current-message))
		message-log-max)
	    (if (and msg
		     ;; Don't get confused by previously displayed
		     ;; `active-input-string'.
		     (null (get-text-property 0 'mac-ts-active-input-string
					      msg)))
		(setq msg (propertize msg 'display
				      (concat msg active-input-string)))
	      (setq msg active-input-string))
	    (message "%s" msg)
	    (move-overlay mac-ts-active-input-overlay 1 1)
	    (overlay-put mac-ts-active-input-overlay 'before-string nil))
	(move-overlay mac-ts-active-input-overlay
		      (point)
		      (if (and delete-selection-mode transient-mark-mode
			       mark-active (not buffer-read-only))
			  (mark)
			(point))
		      (current-buffer))
	(overlay-put mac-ts-active-input-overlay 'before-string
		     active-input-string)
	(if replacement-range
	    (condition-case nil
		;; Strictly speaking, the replacement range can be out
		;; of sync.
		(delete-region (+ (point-min) (car replacement-range))
			       (+ (point-min) (car replacement-range)
				  (cdr replacement-range)))
	      (error nil))))
      (setq mac-ts-active-input-string active-input-string))))

(defvar mac-emoji-font-regexp "\\<emoji\\>"
  "Regexp matching font names for emoji.
This is used for complementing the variation selector
16 (emoji-style) when inserting emoji characters that are
sensitive to the variation selector.")

(defun mac-complement-emoji-by-variation-selector (string)
  (let ((len (length string))
	base)
    (cond ((and (= len 1)
		(string-match
		 (regexp-quote string)
		 (mapconcat 'cdr mac-emoji-variation-characters-alist "")))
	   (concat string "\uFE0F"))
	  ((and (= len 2)
		(eq (aref string 1) #x20E3)
		(string-match
		 (regexp-quote (setq base (substring string 0 1)))
		 (cdr (assq 'keycap mac-emoji-variation-characters-alist))))
	   (concat base "\uFE0F\u20E3"))
	  (t
	   string))))

(defun mac-text-input-insert-text (event)
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (text (cdr (mac-ae-parameter ae)))
	 (replacement-range (cdr (mac-ae-parameter ae "replacementRange")))
	 (script-language (mac-ae-script-language ae "tssl"))
	 (coding (and script-language
		      (or (cdr (assq (car script-language)
				     mac-script-code-coding-systems))
			  'mac-roman))))
    (move-overlay mac-ts-active-input-overlay 1 1)
    (overlay-put mac-ts-active-input-overlay 'before-string nil)
    (let ((msg (current-message))
	  message-log-max)
      (when msg
	(if (get-text-property 0 'mac-ts-active-input-string msg)
	    (message nil)
	  (let ((disp-prop (get-text-property 0 'display msg)))
	    (when (and (stringp disp-prop)
		       (> (length disp-prop) 1)
		       (get-text-property (1- (length disp-prop))
					  'mac-ts-active-input-string
                                          disp-prop))
	      (remove-text-properties 0 (length msg) '(display nil) msg)
	      (message "%s" msg))))))
    (setq mac-ts-active-input-string "")
    (if replacement-range
	(condition-case nil
	    ;; Strictly speaking, the replacement range can be out of
	    ;; sync.
	    (delete-region (+ (point-min) (car replacement-range))
			   (+ (point-min) (car replacement-range)
			      (cdr replacement-range)))
	  (error nil)))
    (let ((string (mac-utxt-to-string text coding))
	  (font (cdr (get-text-property 0 'NSFont text)))) ; NSFontAttributeName
      (if (and (fontp font)
	       (not (next-single-property-change 0 'NSFont text))
	       (string-match mac-emoji-font-regexp
			     (symbol-name (font-get font :family))))
	  (setq string (mac-complement-emoji-by-variation-selector string)))
      (mac-unread-string string))))

(defcustom mac-selected-keyboard-input-source-change-hook nil
  "Hook run for a change to the selected keyboard input source.
The hook functions are not called when Emacs is in the background
even if the selected keyboard input source is changed outside
Emacs."
  :package-version '(Mac\ port . "5.2")
  :type 'hook
  :group 'mac)

(defcustom mac-enabled-keyboard-input-sources-change-hook nil
  "Hook run for a change to the set of enabled keyboard input sources."
  :package-version '(Mac\ port . "5.2")
  :type 'hook
  :group 'mac)

(defun mac-text-input-handle-notification (event)
  (interactive "e")
  (let ((ae (mac-event-ae event)))
    (let ((name (cdr (mac-ae-parameter ae 'name))))
      (cond ((string= name (concat "com.apple.Carbon.TISNotify"
				   "SelectedKeyboardInputSourceChanged"))
	     (run-hooks 'mac-selected-keyboard-input-source-change-hook))
	    ((string= name (concat "com.apple.Carbon.TISNotify"
				   "EnabledKeyboardInputSourcesChanged"))
	     (run-hooks 'mac-enabled-keyboard-input-sources-change-hook))))))

(define-key mac-apple-event-map [text-input set-marked-text]
  'mac-text-input-set-marked-text)
(define-key mac-apple-event-map [text-input insert-text]
  'mac-text-input-insert-text)
(define-key mac-apple-event-map [text-input notification]
  'mac-text-input-handle-notification)

(defun mac-auto-ascii-select-input-source ()
  "Select the most-recently-used ASCII-capable keyboard input source.
Expects to be added to normal hooks."
  (if (= (length mac-ts-active-input-string) 0)
      (mac-select-input-source 'ascii-capable-keyboard)))

(defun mac-auto-ascii-setup-input-source (&optional _prompt)
  "Set up the most-recently-used ASCII-capable keyboard input source.
Expects to be bound to global keymap's prefix keys in
`input-decode-map'."
  (mac-auto-ascii-select-input-source)
  (vector last-input-event))

(define-minor-mode mac-auto-ascii-mode
  "Toggle Mac Auto ASCII mode.

Mac Auto ASCII mode automatically selects the most-recently-used
ASCII-capable keyboard input source on some occasions: after
prefix key (bound in the global keymap) press such as C-x and
M-g, and at the start of minibuffer input.

Strictly speaking, its implementation has a timing issue: the
Lisp event queue may already have some input events that have
been processed by some previous keyboard input source but yet to
be processed by the Lisp interpreter."
  :init-value nil
  :global t
  :group 'mac
  :package-version '(Mac\ port . "5.2")
  (if mac-auto-ascii-mode
      (when (eq (terminal-live-p (frame-terminal)) 'mac)
	(map-keymap (lambda (event definition)
		      (if (and (keymapp definition) (integerp event)
			       (not (eq event ?\e)))
			  (define-key input-decode-map (vector event)
			    'mac-auto-ascii-setup-input-source)))
		    global-map)
	(map-keymap (lambda (event definition)
		      (if (and (keymapp definition) (integerp event)
			       (not (eq event ?\e)))
			  (define-key input-decode-map (vector ?\e event)
			    'mac-auto-ascii-setup-input-source)))
		    esc-map)
	(add-hook 'minibuffer-setup-hook 'mac-auto-ascii-select-input-source))
    (map-keymap (lambda (event definition)
		  (if (eq definition 'mac-auto-ascii-setup-input-source)
		      (define-key input-decode-map (vector event) nil)))
		input-decode-map)
    (let ((input-decode-esc-map (lookup-key input-decode-map "\e")))
      (if (keymapp input-decode-esc-map)
	  (map-keymap
	   (lambda (event definition)
	     (if (eq definition 'mac-auto-ascii-setup-input-source)
		 (define-key input-decode-map (vector ?\e event) nil)))
	   input-decode-esc-map)))
    (remove-hook 'minibuffer-setup-hook 'mac-auto-ascii-select-input-source)))

;;; Converted Actions
(defun mac-handle-about (_event)
  "Display the *About GNU Emacs* buffer in response to EVENT."
  (interactive "e")
  (if (use-fancy-splash-screens-p)
      (mac-start-animation (fancy-splash-frame) :type 'ripple :duration 0.5))
  ;; Convert a event bound in special-event-map to a normal event.
  (setq unread-command-events
	(append '(menu-bar help-menu about-emacs) unread-command-events)))

(defun mac-handle-copy (_event)
  "Copy the selected text to the clipboard.
This is used in response to \"Speak selected text.\""
  (interactive "e")
  (let ((string
	 (condition-case nil
	     (buffer-substring-no-properties (region-beginning) (region-end))
	   (error ""))))
    (gui-set-selection 'CLIPBOARD string)))

(defun mac-handle-synthetic-undo (_event)
  "Push undo event to the command input list."
  (interactive "e")
  (push 'undo unread-command-events))

(defun mac-handle-synthetic-redo (_event)
  "Push redo event to the command input list."
  (interactive "e")
  (push 'redo unread-command-events))

(defun mac-handle-synthetic-cut (_event)
  "Push cut event to the command input list."
  (interactive "e")
  (push 'cut unread-command-events))

(defun mac-handle-synthetic-copy (_event)
  "Push cut event to the command input list."
  (interactive "e")
  (push 'copy unread-command-events))

(defun mac-handle-synthetic-paste (_event)
  "Push cut event to the command input list."
  (interactive "e")
  (push 'paste unread-command-events))

(defun mac-handle-preferences (_event)
  "Display the `Mac' customization group in response to EVENT."
  (interactive "e")
  (mac-start-animation (selected-window) :type 'swipe :duration 0.5)
  (customize-group 'mac))

(defun mac-handle-toolbar-pill-button-clicked (event)
  "Toggle visibility of tool-bars in response to EVENT.
With no keyboard modifiers, it toggles the visibility of the
frame where the tool-bar toggle button was pressed.  With some
modifiers, it changes the global tool-bar visibility setting."
  (interactive "e")
  (let ((ae (mac-event-ae event)))
    (if (mac-ae-keyboard-modifiers ae)
	;; Globally toggle tool-bar-mode if some modifier key is pressed.
	(tool-bar-mode 'toggle)
      (let ((frame (cdr (mac-ae-parameter ae 'frame))))
	(select-frame-set-input-focus frame)
	(set-frame-parameter frame 'tool-bar-lines
			     (if (= (frame-parameter frame 'tool-bar-lines) 0)
				 1 0))))))

(defun mac-handle-change-toolbar-display-mode (event)
  "Change tool bar style in response to EVENT."
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (tag-data (cdr (mac-ae-parameter ae "tag")))
	 style)
    (when (eq (car tag-data) 'number)
      (setq style (cond ((= (cdr tag-data) 1) ; NSToolbarDisplayModeIconAndLabel
			 'both)
			((= (cdr tag-data) 2) ; NSToolbarDisplayModeIconOnly
			 'image)
			((= (cdr tag-data) 3) ; NSToolbarDisplayModeLabelOnly
			 'text)))
      (setq tool-bar-style style)
      (force-mode-line-update t))))

;; Currently, this is called only when the zoom button is clicked
;; while the frame is in fullwidth/fullheight/maximized.
(defun mac-handle-zoom (event)
  "Cancel frame fullscreen status in response to EVENT."
  (interactive "e")
  (let ((ae (mac-event-ae event)))
    (let ((frame (cdr (mac-ae-parameter ae 'frame))))
      (set-frame-parameter frame 'fullscreen nil))))

(defun mac-handle-new-window-for-tab (_event)
  "Create a new frame for tab in response to EVENT."
  (interactive "e")
  (let ((mac-frame-tabbing t))
    (make-frame)))

(define-key mac-apple-event-map [action about] 'mac-handle-about)
(define-key mac-apple-event-map [action copy] 'mac-handle-copy)
(define-key mac-apple-event-map [action preferences] 'mac-handle-preferences)
(define-key mac-apple-event-map [action toolbar-pill-button-clicked]
 'mac-handle-toolbar-pill-button-clicked)
(define-key mac-apple-event-map [action change-toolbar-display-mode]
 'mac-handle-change-toolbar-display-mode)
(put 'change-toolbar-display-mode 'mac-action-key-paths '("tag"))
(define-key mac-apple-event-map [action zoom] 'mac-handle-zoom)
(define-key mac-apple-event-map [action newWindowForTab]
  'mac-handle-new-window-for-tab)
(define-key mac-apple-event-map [action synthetic-undo]
  'mac-handle-synthetic-undo)
(define-key mac-apple-event-map [action synthetic-redo]
  'mac-handle-synthetic-redo)
(define-key mac-apple-event-map [action synthetic-cut]
  'mac-handle-synthetic-cut)
(define-key mac-apple-event-map [action synthetic-copy]
  'mac-handle-synthetic-copy)
(define-key mac-apple-event-map [action synthetic-paste]
  'mac-handle-synthetic-paste)
(define-key mac-apple-event-map [action change-mode] 'ignore)

;;; Spotlight for Help

(declare-function info-initialize "info" ())
(declare-function Info-find-file "info" (filename &optional noerror))
(declare-function Info-toc-build "info" (file))
(declare-function Info-virtual-index "info" (topic))

(defvar mac-help-topics)

(defun mac-setup-help-topics ()
  (unless (or mac-help-topics inhibit-menubar-update)
    (let ((current-message (current-message)))
      (unwind-protect
	  (progn
	    (require 'info)
	    (info-initialize)
	    (let ((filename (Info-find-file "Emacs" t)))
	      (if (null filename)
		  (setq mac-help-topics t)
		(setq mac-help-topics
		      (mapcar (lambda (node-info)
				(encode-coding-string (car node-info) 'utf-8))
			      (Info-toc-build filename))))))
	(unless (equal (current-message) current-message)
	  (if current-message
	      (message "%s" current-message)
	    (message nil)))))))

(defun mac-handle-select-help-topic (event)
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (tag-data (cdr (mac-ae-parameter ae "selectedHelpTopic"))))
    (if (eq (car tag-data) 'string)
	(info (format "(Emacs)%s"
		      (decode-coding-string (cdr tag-data) 'utf-8))))))

(define-key mac-apple-event-map [action select-help-topic]
 'mac-handle-select-help-topic)
(put 'select-help-topic 'mac-action-key-paths '("selectedHelpTopic"))

(defun mac-handle-show-all-help-topics (event)
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (tag-data (cdr (mac-ae-parameter ae "searchStringForAllHelpTopics"))))
    (when (eq (car tag-data) 'string)
      (info "Emacs")
      (Info-virtual-index (decode-coding-string (cdr tag-data) 'utf-8)))))

(define-key mac-apple-event-map [action show-all-help-topics]
 'mac-handle-show-all-help-topics)
(put 'show-all-help-topics 'mac-action-key-paths
     '("searchStringForAllHelpTopics"))

;;; Frame events
(defun mac-handle-modify-frame-parameters-event (event)
  "Modify frame parameters according to EVENT."
  (interactive "e")
  (let ((ae (mac-event-ae event)))
    (let ((frame (cdr (mac-ae-parameter ae 'frame)))
	  (alist (cdr (mac-ae-parameter ae 'alist))))
      ;; macOS 10.12 sends this event for a dead frame when a tab in a
      ;; full screen space is closed.
      (if (frame-live-p frame)
          (modify-frame-parameters frame alist)))))

(define-key mac-apple-event-map [frame modify-frame-parameters]
 'mac-handle-modify-frame-parameters-event)

;;; Accessibility
(defun mac-ax-set-selected-text-range (event)
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (tag-data (cdr (mac-ae-parameter ae)))
	 (window (cdr (mac-ae-parameter ae 'window))))
    (if (and (eq window (posn-window (event-start event)))
	     (eq (car tag-data) 'range))
	(goto-char (+ (point-min) (car (cdr tag-data)))))))

(defun mac-ax-set-visible-character-range (event)
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (tag-data (cdr (mac-ae-parameter ae)))
	 (window (cdr (mac-ae-parameter ae 'window))))
    (if (and (eq window (posn-window (event-start event)))
	     (eq (car tag-data) 'range))
	(set-window-start window (+ (point-min) (car (cdr tag-data)))))))

(defun mac-ax-show-menu (event)
  "Pop up major mode menu near the point in response to accessibility EVENT."
  (interactive "e")
  (let* ((ae (mac-event-ae event))
	 (window (cdr (mac-ae-parameter ae 'window))))
    ;; Check if a different window is selected after the event
    ;; occurred.
    (if (eq window (posn-window (event-start event)))
	(let* ((posn (posn-at-point))
	       (x-y (posn-x-y posn))
	       (width-height (posn-object-width-height posn))
	       (edges (window-pixel-edges))
	       (inside-edges (window-inside-pixel-edges))
	       (xoffset (+ (- (car inside-edges) (car edges))
			   (car x-y)
			   (min (car width-height) (frame-char-width))))
	       (yoffset (+ (cdr x-y)
			   (min (cdr width-height) (frame-char-height)))))
	  (popup-menu (mouse-menu-major-mode-map)
		      (list (list xoffset yoffset) window))))))

(define-key mac-apple-event-map [accessibility set-selected-text-range]
  'mac-ax-set-selected-text-range)
(define-key mac-apple-event-map [accessibility set-visible-character-range]
  'mac-ax-set-visible-character-range)
(define-key mac-apple-event-map [accessibility show-menu]
  'mac-ax-show-menu)

;;; Services
(defun mac-service-open-file ()
  "Open the file specified by the selection value for Services."
  (interactive)
  ;; The selection seems not to contain the file name as
  ;; NSStringPboardType data on Mac OS X 10.4, when the selected items
  ;; are file icons.
  (let (data file-urls)
    (setq data
	  (condition-case nil
	      (gui-get-selection mac-service-selection 'NSFilenamesPboardType)
	    (error nil)))
    (if data
	(setq file-urls
	      (mac-pasteboard-filenames-to-file-urls data)))
    (when (null file-urls)
      (setq data (condition-case nil
		     (gui-get-selection mac-service-selection
                                        'NSStringPboardType)
		   (error nil)))
      (when data
	(if (string-match "\\`[[:space:]\n]*[^[:space:]\n]" data)
	    (setq data (substring data (1- (match-end 0)))))
	(if (string-match "[^[:space:]\n][[:space:]\n]*\\'" data)
	    (setq data (substring data 0 (1+ (match-beginning 0)))))
	(when (file-name-absolute-p data)
	  (let ((filename (expand-file-name data "/")))
	    (unless (and (eq (aref data 0) ?~)
			 (string-match "\\`/~" filename))
	      (setq file-urls
		    (list (mac-local-file-name-to-file-url filename))))))))
    (when file-urls
      (dolist (file-url file-urls)
	(dnd-open-local-file file-url nil))
      (select-frame-set-input-focus (selected-frame)))))

(defun mac-service-open-selection ()
  "Create a new buffer containing the selection value for Services."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*untitled*"))
  (insert (gui-get-selection mac-service-selection 'NSStringPboardType))
  (sit-for 0)
  (save-buffer) ; It pops up the save dialog.
  )

(defun mac-service-mail-selection ()
  "Prepare a mail buffer containing the selection value for Services."
  (interactive)
  (compose-mail)
  (rfc822-goto-eoh)
  (forward-line 1)
  (insert (gui-get-selection mac-service-selection 'NSStringPboardType) "\n"))

(defun mac-service-mail-to ()
  "Prepare a mail buffer to be sent to the selection value for Services."
  (interactive)
  (compose-mail (gui-get-selection mac-service-selection 'NSStringPboardType)))

(defun mac-service-insert-text ()
  "Insert the selection value for Services."
  (interactive)
  (let ((text (mac-selection-value-internal mac-service-selection)))
    (if (null text)
	(message "Emacs does not understand the output from the Services menu.")
      (if (not buffer-read-only)
	  (progn
	    (if (use-region-p)
		(delete-region (region-beginning) (region-end)))
	    (insert text))
	(kill-new text)
	(message "%s"
	 (substitute-command-keys
	  "The text from the Services menu can be accessed with \\[yank]"))))))

;; kEventClassService/kEventServicePaste
(define-key mac-apple-event-map [service paste] 'mac-service-insert-text)
;; kEventClassService/kEventServicePerform
(define-key mac-apple-event-map [service perform open-file]
  'mac-service-open-file)
(define-key mac-apple-event-map [service perform open-selection]
  'mac-service-open-selection)
(define-key mac-apple-event-map [service perform mail-selection]
  'mac-service-mail-selection)
(define-key mac-apple-event-map [service perform mail-to]
  'mac-service-mail-to)

(declare-function mac-ae-set-reply-parameter "macselect.c"
		  (apple-event keyword descriptor))

(defun mac-dispatch-apple-event (event)
  "Dispatch EVENT according to the keymap `mac-apple-event-map'."
  (interactive "e")
  (let* ((binding (lookup-key mac-apple-event-map (mac-event-spec event)))
	 (ae (mac-event-ae event))
	 (service-message (and (keymapp binding)
			       (cdr (mac-ae-parameter ae "svmg")))))
    (when service-message
      (setq service-message
	    (intern (decode-coding-string service-message 'utf-8)))
      (setq binding (lookup-key binding (vector service-message))))
    ;; Replace (cadr event) with a dummy position so that event-start
    ;; returns it.
    (setcar (cdr event) (list (selected-window) (point) '(0 . 0) 0))
    (if (null (mac-ae-parameter ae 'emacs-suspension-id))
	(command-execute binding nil (vector event) t)
      (condition-case err
	  (progn
	    (command-execute binding nil (vector event) t)
	    (mac-resume-apple-event ae))
	(error
	 (mac-ae-set-reply-parameter ae "errs"
				     (cons "TEXT" (error-message-string err)))
	 (mac-resume-apple-event ae -10000)))))) ; errAEEventFailed

(define-key special-event-map [mac-apple-event] 'mac-dispatch-apple-event)


;;;; Drag and drop

(defcustom mac-dnd-types-alist
  '(("public.file-url" ; kUTTypeFileURL (which confirms to kUTTypeURL)
     . mac-dnd-handle-file-url)
    ("public.url"                       ; kUTTypeURL
     . dnd-handle-one-url)
    ("public.utf8-plain-text"           ; NSPasteboardTypeString
     . mac-dnd-insert-pasteboard-string)
    ("public.tiff"                      ; NSPasteboardTypeTIFF
     . mac-dnd-insert-TIFF))
  "Which function to call to handle a drop of that type.
The function takes three arguments, WINDOW, ACTION and DATA.
WINDOW is where the drop occurred, ACTION is always `private' on
Mac.  DATA is the drop data.  Unlike the x-dnd counterpart, the
return value of the function is not significant.

See also `mac-dnd-known-types'."
  :version "25.2"
  :type 'alist
  :group 'mac)

(defun mac-dnd-handle-file-url (window action data)
  "Like dnd-handle-one-url, but accepts a file reference URL as DATA.
On OS X 10.10, drag-and-dropping file icons produces file
reference URLs of the form \"file:///.file/id=...\"."
  (let ((filename (mac-coerce-ae-data "furl" data 'undecoded-file-name)))
    (if filename
        (let ((file-url (mac-local-file-name-to-file-url filename)))
          (dnd-handle-one-url window action file-url)))))

(defun mac-dnd-insert-TIFF (window action data)
  (dnd-insert-text window action (mac-TIFF-to-string data)))

(defun mac-dnd-insert-pasteboard-string (window action data)
  (dnd-insert-text window action (mac-pasteboard-string-to-string data)))

(defun mac-dnd-drop-data (event frame window data type &optional action)
  (or action (setq action 'private))
  (let* ((type-info (assoc type mac-dnd-types-alist))
	 (handler (cdr type-info))
	 (w (posn-window (event-start event))))
    (when handler
      (if (and (window-live-p w)
	       (not (window-minibuffer-p w))
	       (not (window-dedicated-p w)))
	  ;; If dropping in an ordinary window which we could use,
	  ;; let dnd-open-file-other-window specify what to do.
	  (progn
	    (when (not mouse-yank-at-point)
	      (goto-char (posn-point (event-start event))))
	    (funcall handler window action data))
	;; If we can't display the file here,
	;; make a new window for it.
	(let ((dnd-open-file-other-window t))
	  (select-frame frame)
	  (funcall handler window action data))))))

(defun mac-dnd-handle-drag-n-drop-event (event)
  "Receive drag and drop events."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (plist (nth 2 event))
         ;; The second element of a drag-n-drop event is of the form
         ;; (:actions ACTION-LIST :items (ITEM0 ITEM1 ...)) where
         ;; ITEMi is either (TYPE-STRING . DATA-STRING) or nil.
         (actions (plist-get plist :actions))
         (action (and (not (memq 'generic actions)) (memq 'copy actions)
                      'copy)))
    (when (windowp window) (select-window window))
    (dolist (type-data (plist-get plist :items))
      (if type-data
          (mac-dnd-drop-data event (selected-frame) window
                             (cdr type-data) (car type-data) action)))))


;;;; Key-value observing for application

(defcustom mac-effective-appearance-change-hook nil
  "Hook run when the macOS global appearance changes; either manually or
automatically.

A hook function can determine the current appearance by checking the
:appearance property of (mac-application-state)."
  :package-version '(Mac\ port . "8.2")
  :type 'hook
  :group 'mac)

(defun mac-handle-application-effective-appearance-change (_event)
  (interactive "e")
  (run-hooks 'mac-effective-appearance-change-hook)
  (clear-face-cache)
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'background-color
			 (frame-parameter frame 'background-color))))

(define-key mac-apple-event-map [application-kvo effectiveAppearance]
  'mac-handle-application-effective-appearance-change)


(defvar mac-popup-menu-add-contextual-menu)

(declare-function accelerate-menu "macmenu.c" (&optional frame) t)

(defun mac-menu-bar-open (&optional frame)
  "Open the menu bar if it is shown.
`popup-menu' is used if it is off."
  (interactive "i")
  (cond
   ((and (not (zerop (or (frame-parameter nil 'menu-bar-lines) 0)))
	 (fboundp 'accelerate-menu))
    (accelerate-menu frame))
   (t
    (popup-menu (mouse-menu-bar-map) last-nonmenu-event))))

(defun mac-mouse-buffer-menu (event)
  "Like 'mouse-buffer-menu', but contextual menu is added if possible."
  (interactive "e")
  (let ((mac-popup-menu-add-contextual-menu t))
    (mouse-buffer-menu event)))


;;; Mouse wheel smooth scroll

(defcustom mac-mouse-wheel-smooth-scroll t
  "Non-nil means the mouse wheel should scroll by pixel unit if possible."
  :group 'mac
  :type 'boolean)

(defvar mac-ignore-momentum-wheel-events)
(defvar mac-redisplay-dont-reset-vscroll)

(defun mac-mwheel-scroll (event)
  "Scroll up or down according to the EVENT.
Mostly like `mwheel-scroll', but try scrolling by pixel unit if
EVENT has no modifier keys, `mac-mouse-wheel-smooth-scroll' is
non-nil, and the input device supports it."
  (interactive (list last-input-event))
  (setq mac-ignore-momentum-wheel-events nil)
  ;; (nth 3 event) is a plist that may contain the following keys:
  ;; :direction-inverted-from-device-p		(boolean)
  ;; :delta-x, :delta-y, :delta-z		(floats)
  ;; :scrolling-delta-x, :scrolling-delta-y	(floats)
  ;; :phase, :momentum-phase			(symbols)
  ;;	possible value: `none', `began', `stationary', `changed',
  ;;			`ended', `cancelled', or `may-begin'
  ;; :swipe-tracking-from-scroll-events-enabled-p (boolean)
  (if (not (memq (event-basic-type event) '(wheel-up wheel-down)))
      (when (memq (event-basic-type event) '(wheel-left wheel-right))
        (if mouse-wheel-tilt-scroll
            (if (null (plist-get (nth 3 event) :delta-x))
                (mwheel-scroll event)
              (setf (nth 3 event)
                    (round (abs (plist-get (nth 3 event) :delta-x))))
              (when (> (nth 3 event) 0)
                (let ((mouse-wheel-scroll-amount
                       '(1 ((shift) . 5) ((control))))
                      (mouse-wheel-progressive-speed nil))
                  (mwheel-scroll event))))
          (cond ((and
                  ;; "Swipe between pages" enabled.
                  (plist-get (nth 3 event)
                             :swipe-tracking-from-scroll-events-enabled-p)
                  (eq (plist-get (nth 3 event) :momentum-phase) 'began))
                 ;; Post a swipe event when the momentum phase begins
                 ;; for horizontal wheel events.
                 (setq mac-ignore-momentum-wheel-events t)
                 (push (cons
                        (event-convert-list
                         (nconc (delq 'click
                                      (delq 'double
                                            (delq 'triple
                                                  (event-modifiers event))))
                                (if (eq (event-basic-type event) 'wheel-left)
                                    '(swipe-left) '(swipe-right))))
                        (cdr event))
                       unread-command-events)))))
    (if (or (not mac-mouse-wheel-smooth-scroll)
	    (delq 'click (delq 'double (delq 'triple (event-modifiers event))))
	    (null (plist-get (nth 3 event) :scrolling-delta-y)))
	(if (or (null (nth 3 event))
                (/= (plist-get (nth 3 event) :delta-y) 0.0))
            (if (null (plist-get (nth 3 event) :delta-y))
                (mwheel-scroll event)
              (setf (nth 3 event)
                    (round (abs (plist-get (nth 3 event) :delta-y))))
              (when (> (nth 3 event) 0)
                (let ((mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
                      (mouse-wheel-progressive-speed nil))
                  (mwheel-scroll event)))))
      ;; TODO: ignore momentum scroll events after buffer switch.
      (let* ((window-to-scroll (if mouse-wheel-follow-mouse
				   (posn-window (event-start event))))
	     (edges (window-inside-pixel-edges window-to-scroll))
	     (window-inside-pixel-height (- (nth 3 edges) (nth 1 edges)))
	     ;; Do redisplay and measure line heights before selecting
	     ;; the window to scroll.
	     (point-height
	      (or (window-line-height nil window-to-scroll)
		  (progn
		    (redisplay t)
		    (window-line-height nil window-to-scroll))
		  ;; The above still sometimes return nil.
		  (progn
		    (redisplay t)
		    (window-line-height nil window-to-scroll))))
	     (tab-line-height
	      (window-line-height 'tab-line window-to-scroll))
	     (header-line-height
	      (window-line-height 'header-line window-to-scroll))
	     (first-height (window-line-height 0 window-to-scroll))
	     (last-height (window-line-height -1 window-to-scroll))
	     (first-y (+ (or (car tab-line-height) 0)
                         (or (car header-line-height) 0)))
	     (first-height-sans-hidden-top
	      (cond ((= (car first-height) 0) ; completely invisible.
		     0)
		    ((not (or tab-line-height
                              header-line-height)) ; no tab or header line.
		     (+ (car first-height) (nth 3 first-height)))
		    (t	  ; might be partly hidden by the header line.
		     (+ (- (car first-height)
			   (- first-y (max (nth 2 first-height) 0)))
			(nth 3 first-height)))))
	     (delta-y
              (let ((dy (plist-get (nth 3 event) :scrolling-delta-y))
                    pending-events)
                ;; Coalesce vertical mouse wheel events.
                (while (setq event (read-event nil nil 1e-5))
                  (if (and (memq (event-basic-type event)
                                 '(wheel-up wheel-down))
                           (eq window-to-scroll
                               (if mouse-wheel-follow-mouse
                                   (posn-window (event-start event)))))
                      (setq dy
                            (+ dy (plist-get (nth 3 event) :scrolling-delta-y)))
                    (push event pending-events)))
                (if pending-events
                    (setq unread-command-events (nconc (nreverse pending-events)
                                                       unread-command-events)))
                (round (- dy))))
	     ;; Now select the window to scroll.
	     (curwin (if window-to-scroll
			 (prog1
			     (selected-window)
			   (select-window window-to-scroll))))
	     (buffer (window-buffer curwin))
	     (opoint (with-current-buffer buffer
		       (when (eq (car-safe transient-mark-mode) 'only)
			 (point))))
	     (scroll-conservatively 0)
	     scroll-preserve-screen-position
	     auto-window-vscroll
	     redisplay-needed)
	(unwind-protect
	    ;; Check if it is sufficient to adjust window's vscroll
	    ;; value.  Because the vscroll value might be non-zero at
	    ;; this stage, `window-start' might return an invisible
	    ;; position (e.g., "*About GNU Emacs*" buffer.), and thus
	    ;; we cannot rely on `pos-visible-in-window-p' until we
	    ;; reset the vscroll value.  That's why we used
	    ;; `redisplay' followed by `window-line-height' above.
	    (if (cond ((< delta-y 0)	; scroll down
		       (and
			(> first-height-sans-hidden-top 0)
			;; First row has enough room?
			(<= (- (nth 2 first-height) first-y)
			    delta-y)
			(or
			 ;; Cursor is still fully visible if scrolled?
			 (<= (+ (car point-height) (nth 2 point-height))
			     (+ first-y window-inside-pixel-height delta-y))
			 ;; Window has the only one row?
			 (= (nth 2 first-height) (nth 2 last-height)))))
		      ((> delta-y 0) ; scroll up
		       (and
			(> first-height-sans-hidden-top 0)
			;; First row has enough room?
			(< delta-y first-height-sans-hidden-top)
			(or
			 (and
			  ;; Cursor is still fully visible if scrolled?
			  (< (nth 2 first-height) (nth 2 point-height))
			  (not (and (eobp) (bolp)
				    ;; Cursor is at the second row?
				    (= (+ first-y first-height-sans-hidden-top)
				       (nth 2 point-height)))))
			 (and (>= (- first-height-sans-hidden-top delta-y)
				  (frame-char-height))
			      (> (window-vscroll nil t) 0))
			 ;; Window has the only one row?
			 (= (nth 2 first-height) (nth 2 last-height)))))
		      (t
		       t))
		(if (zerop (set-window-vscroll nil (+ (window-vscroll nil t)
						      delta-y) t))
		    ;; XXX: Why is this necessary?
		    (set-window-start nil (window-start)))
	      (when (> (window-vscroll nil t) 0)
		(setq delta-y (- delta-y first-height-sans-hidden-top))
		(condition-case nil
		    ;; XXX: may recenter around the end of buffer.
		    (scroll-up 1)
		  (end-of-buffer
		   (set-window-vscroll nil 0 t)
		   (condition-case nil
		       (funcall mwheel-scroll-up-function 1)
		     (end-of-buffer))
		   (setq delta-y 0)))
		;; XXX: Why is this necessary?
		(set-window-start nil (window-start))
		(set-window-vscroll nil 0 t))
	      (condition-case nil
		  (if (< delta-y 0)
		      ;; Scroll down
		      (let (prev-first prev-point prev-first-y prev-first-row)
			(while (null prev-first-row)
			  ;; It might be the case that `point-min' is
			  ;; visible but `window-start' < `point-min'.
			  (if (pos-visible-in-window-p (point-min) nil t)
			      (signal 'beginning-of-buffer nil))
			  (setq prev-first (window-start))
			  (setq prev-point (point))
			  (let ((prev-first-prev-y
				 (cadr (pos-visible-in-window-p prev-first
								nil t))))
			    (scroll-down)
			    ;; Sometimes the point gets invisible.
			    (if (< (point) (window-start))
				(goto-char (window-start))
			      (while (not (pos-visible-in-window-p nil nil t))
				(vertical-motion -1)))
			    (while (null (setq prev-first-y
					       (cadr (pos-visible-in-window-p
						      prev-first nil t))))
			      ;; If the previous first position gets
			      ;; invisible after scroll down, scroll
			      ;; up by line and try again.
			      (scroll-up 1))
			    (if (/= prev-first-y prev-first-prev-y)
				(progn
				  (setq delta-y
					(+ delta-y
					   (- prev-first-y prev-first-prev-y)))
				  (if (>= delta-y 0)
				      (setq prev-first-row
					    (cdr
					     (posn-actual-col-row
					      (posn-at-x-y 0 prev-first-y))))))
			      ;; It might be the case that `point-min'
			      ;; is visible but `window-start' <
			      ;; `point-min'.
			      (if (pos-visible-in-window-p (point-min) nil t)
				  (signal 'beginning-of-buffer nil))
			      ;; We came back to the original
			      ;; position.  We were at a line that is
			      ;; taller than the window height before
			      ;; the last scroll up.
			      (scroll-down 1)
			      (let (prev-first-vscrolled-y)
				(while (not (setq prev-first-vscrolled-y
						  (cadr (pos-visible-in-window-p
							 prev-first nil t))))
				  (set-window-vscroll
				   nil (+ (window-vscroll nil t)
					  window-inside-pixel-height) t))
				(setq prev-first-y
				      (+ (window-vscroll nil t)
					 prev-first-vscrolled-y))
				(setq delta-y
				      (+ delta-y
					 (- prev-first-y prev-first-prev-y)))
				(if (>= delta-y 0)
				    (setq prev-first-row
					  (cdr (posn-actual-col-row
						(posn-at-x-y
						 0 prev-first-vscrolled-y))))))
			      (set-window-vscroll nil 0 t))))
			(let* ((target-posn (posn-at-x-y 0 (+ first-y delta-y)))
			       target-coord target-row
			       target-y scrolled-pixel-height)
			  (if (and (eq (posn-window target-posn)
				       (selected-window))
				   (null (posn-area target-posn)))
			      (progn
				(setq target-coord
				      (pos-visible-in-window-p
				       (posn-point target-posn) nil t))
				(setq target-y (cadr target-coord)))
			    ;; The target row is below the visible
			    ;; area.  Temporarily set vscroll so the
			    ;; target row comes at the top and
			    ;; re-measure the position.
			    (set-window-vscroll nil delta-y t)
			    (setq target-posn (posn-at-x-y 0 first-y))
			    (setq target-coord (pos-visible-in-window-p
						(posn-point target-posn) nil t))
			    (set-window-vscroll nil 0 t)
			    (setq target-y (cadr target-coord))
			    (cond ((null target-y)
				   ;; Image is completely invisible
				   ;; but its descent is visible in
				   ;; the first row.
				   (setq target-y
					 (- first-y (cdr (posn-object-x-y
							  target-posn)))))
				  ((= target-y first-y)
				   ;; Image is partly invisible in the
				   ;; first row.
				   (setq target-y
					 (- first-y (or (nth 2 target-coord)
							0)))))
			    (setq target-y (+ target-y delta-y)))
			  (setq target-row
				(cdr (posn-actual-col-row target-posn)))
			  (setq scrolled-pixel-height (- target-y first-y))
			  ;; Cancel the last scroll.
			  (goto-char prev-point)
			  (set-window-start nil prev-first)
			  (scroll-down (- prev-first-row target-row
					  (if (= delta-y
						 scrolled-pixel-height)
					      0 1)))
			  (setq delta-y (- delta-y scrolled-pixel-height))))
		    ;; Scroll up
		    (let (found)
		      (while (and (not found)
				  (>= delta-y window-inside-pixel-height))
			(let* ((prev-last-prev-coord
				(pos-visible-in-window-p t nil t))
			       (prev-last-prev-posn
				(posn-at-x-y (car prev-last-prev-coord)
					     (max (cadr prev-last-prev-coord)
						  first-y)))
			       (prev-last
				(posn-point prev-last-prev-posn))
			       (prev-last-prev-y
				(cadr prev-last-prev-coord))
			       prev-last-y prev-first)
			  (scroll-up)
			  (while (null (setq prev-last-y
					     (cadr (pos-visible-in-window-p
						    prev-last nil t))))
			    ;; If the previous last position gets
			    ;; invisible after scroll up, scroll down
			    ;; by line and try again.
			    (setq prev-first (window-start))
			    (scroll-down 1))
			  (if (/= prev-last-y prev-last-prev-y)
			      (setq delta-y
				    (- delta-y
				       (- prev-last-prev-y prev-last-y)))
			    ;; We came back to the original position.
			    ;; We are at a line that is taller than
			    ;; the window height.
			    (while (and (>= (- delta-y (window-vscroll nil t))
					    window-inside-pixel-height)
					(not (pos-visible-in-window-p
					      prev-first nil t)))
			      (set-window-vscroll
			       nil (+ (window-vscroll nil t)
				      window-inside-pixel-height)
			       t))
			    (let ((prev-first-y (cadr (pos-visible-in-window-p
						       prev-first nil t))))
			      (if (not (and prev-first-y
					    (>= (- delta-y
						   (window-vscroll nil t))
						(- prev-first-y first-y))))
				  (progn
				    (setq found t)
				    (set-window-vscroll nil 0 t))
				(setq delta-y
				      (- delta-y
					 (+ (window-vscroll nil t)
					    (- prev-first-y first-y))))
				(set-window-vscroll nil 0 t)
				(scroll-up 1)))))))
		    (if (< delta-y window-inside-pixel-height)
			(let* ((target-posn
				(posn-at-x-y 0 (+ first-y delta-y)))
			       (target-row
				(cdr (posn-actual-col-row target-posn)))
			       (target-y
				(cadr (pos-visible-in-window-p
				       (posn-point target-posn) nil t)))
			       (scrolled-pixel-height (- target-y first-y)))
			  ;; Emacs 23 -> 24 incompatibility: the
			  ;; actual row part of POSITION now counts
			  ;; the header line.
			  ;;
			  ;; Emacs 24.3 -> 24.4 incompatibility again:
			  ;; the actual row part of POSITION no longer
			  ;; counts the header line (see Bug#18384).
			  ;; (if header-line-height
			  ;;     (setq target-row (1- target-row)))
			  (scroll-up (if (= delta-y scrolled-pixel-height)
					 target-row
				       (1+ target-row)))
			  (setq delta-y (- delta-y scrolled-pixel-height)))))
		(beginning-of-buffer
		 (condition-case nil
		     (funcall mwheel-scroll-down-function 1)
		   (beginning-of-buffer))
		 (setq delta-y 0))
		(end-of-buffer
		 (condition-case nil
		     (funcall mwheel-scroll-up-function 1)
		   (end-of-buffer))
		 (setq delta-y 0)))
	      (when (> delta-y 0)
		(scroll-down 1)
		(set-window-vscroll nil delta-y t)
		(setq redisplay-needed t))
	      (if (> (count-screen-lines (window-start) (window-end nil t)) 1)
		  ;; Make sure that the cursor is fully visible.
		  (while (and (< (window-start) (point))
			      (not (pos-visible-in-window-p)))
		    (vertical-motion -1))))
	  (if curwin (select-window curwin))
	  (if redisplay-needed
	      (let ((mac-redisplay-dont-reset-vscroll t))
		(redisplay))))
	;; If there is a temporarily active region, deactivate it if
	;; scrolling moves point.
	(when opoint
	  (with-current-buffer buffer
	    (when (/= opoint (point))
	      ;; Call `deactivate-mark' at the original position, so
	      ;; that the original region is saved to the selection.
	      (let ((newpoint (point)))
		(goto-char opoint)
		(deactivate-mark)
		(goto-char newpoint)))))))))

(defvar mac-mwheel-installed-bindings nil)

(define-minor-mode mac-mouse-wheel-mode
  "Toggle mouse wheel support with smooth scroll (Mac Mouse Wheel mode)."
  :init-value nil
  :global t
  :group 'mac
  ;; Remove previous bindings, if any.
  (while mac-mwheel-installed-bindings
    (let ((key (pop mac-mwheel-installed-bindings)))
      (when (eq (lookup-key (current-global-map) key) 'mac-mwheel-scroll)
        (global-unset-key key))))
  ;; Setup bindings as needed.
  (when mac-mouse-wheel-mode
    (dolist (event '(wheel-down wheel-up wheel-left wheel-right))
      (dolist (key (mapcar (lambda (amt) `[(,@(if (consp amt) (car amt)) ,event)])
                           mouse-wheel-scroll-amount))
        (global-set-key key 'mac-mwheel-scroll)
	(push key mac-mwheel-installed-bindings)))))


;;; Swipe events
(defun mac-previous-buffer (event)
  "Like `previous-buffer', but operate on the window where EVENT occurred."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (mac-start-animation window :type 'move-out :direction 'right)
    (with-selected-window window
      (previous-buffer))))

(defun mac-next-buffer (event)
  "Like `next-buffer', but operate on the window where EVENT occurred."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (mac-start-animation window :type 'none)
    (with-selected-window window
      (next-buffer))
    (mac-start-animation window :type 'move-in :direction 'left)))

(global-set-key [swipe-left] 'mac-previous-buffer)
(global-set-key [swipe-right] 'mac-next-buffer)


;;; Trackpad events
(defvar text-scale-mode-step) ;; in face-remap.el
(defvar text-scale-mode-amount) ;; in face-remap.el
(declare-function mac-set-frame-tab-group-property "macfns.c"
                  (frame prop value))
(declare-function mac-frame-tab-group-property "macfns.c"
                  (&optional frame prop))

(defvar mac-text-scale-magnification 1.0
  "Magnification value for text scaling.
The variable `text-scale-mode-amount' is set to the rounded
logarithm of this value using the value of `text-scale-mode-step'
as base.")
(make-variable-buffer-local 'mac-text-scale-magnification)

(defvar mac-ignore-magnify-events nil
  "Non-nil means magnify events are ignored.")

(defcustom mac-text-scale-magnification-range '(0.1 . 20.0)
  "Pair of minimum and maximum values for `mac-text-scale-magnification'."
  :type '(cons number number)
  :group 'mac)

(defcustom mac-text-scale-standard-width 80
  "Number of columns window occupies in standard buffer text scaling.
On Mac OS X 10.7 and later, you can change buffer text scaling to
the standard state by double-tapping either a touch-sensitive
mouse with one finger or a trackpad with two fingers, if the
buffer is previously unscaled and the place that mouse pointer is
pointing to is before the indentation or after the end of line.
In this standard buffer text scaling state, the default font is
scaled so at least this number of columns can be shown in the
tapped window."
  :type 'integer
  :group 'mac)

(defun mac-scroll-point-to-y (target-point target-y)
  ;; Without this, pos-visible-in-window-p after text-scale-increase
  ;; may return an outdated value.
  (scroll-up 0)
  (condition-case nil
      (let* ((last-point (point))
	     (current-y (cadr (pos-visible-in-window-p target-point nil t)))
	     (direction (if (if current-y (< target-y current-y)
			      (< (window-start) target-point))
			    -1 1)))	; Negative if upward.
	(while (< 0 (* direction (if current-y
				     (- target-y current-y)
				   (- (window-start) target-point))))
	  (scroll-down direction)
	  (setq last-point (point))
	  (setq current-y (cadr (pos-visible-in-window-p target-point nil t))))
	(unless (and (< direction 0) current-y)
	  ;; Cancel the last scroll.
	  (scroll-up direction)
	  (goto-char last-point)))
    ((beginning-of-buffer end-of-buffer) nil)))

(defun mac-magnify-text-scale (event)
  "Magnify the height of the default face in the buffer where EVENT happened.
The actual magnification is performed by `text-scale-mode'."
  (interactive "e")
  (require 'face-remap)
  (let ((original-selected-window (selected-window)))
    (with-selected-window (posn-window (event-start event))
      (let ((magnification (plist-get (nth 3 event) :magnification))
	    (level
	     (round (log mac-text-scale-magnification text-scale-mode-step))))
	;; Sync with text-scale-mode-amount.
	(if (/= level text-scale-mode-amount)
	    (setq mac-text-scale-magnification
		  (expt text-scale-mode-step text-scale-mode-amount)))
	(if (null (plist-get (nth 3 event) :phase))
	    ;; This is double-tapping a mouse with one finger or
	    ;; double-tapping a trackpad with two fingers on Mac OS X
	    ;; 10.7
	    (if (/= text-scale-mode-amount 0)
		(setq mac-text-scale-magnification 1.0)
	      (let ((event-point (posn-point (event-start event))))
		(if (and (not (memq (char-after event-point) '(?\n nil)))
			 (save-excursion
			   (goto-char event-point)
			   (back-to-indentation)
			   (>= event-point (point))))
		    ;; Between indentation and eol.  Scale to 150%.
		    (setq mac-text-scale-magnification 1.5)
		  ;; Before indentation or after eol.  Scale so the
		  ;; number of columns becomes at least the value of
		  ;; mac-text-scale-standard-width.
		  (setq mac-text-scale-magnification
			(expt text-scale-mode-step
			      (floor (log (/ (window-width)
					     (float
					      mac-text-scale-standard-width))
					  text-scale-mode-step)))))))
	  (setq mac-text-scale-magnification
		(* mac-text-scale-magnification (+ 1.0 magnification))))
	(setq mac-text-scale-magnification
	      (min (max mac-text-scale-magnification
			(car mac-text-scale-magnification-range))
		   (cdr mac-text-scale-magnification-range)))
	(setq level
	      (round (log mac-text-scale-magnification text-scale-mode-step)))
	(if (/= level text-scale-mode-amount)
	    (let ((inc (if (< level text-scale-mode-amount) -1 1))
		  (target-point (posn-point (event-start event)))
		  (target-y (cdr (posn-x-y (event-start event)))))
	      (unwind-protect
		  (while (and (not (input-pending-p))
			      (/= text-scale-mode-amount level))
		    (text-scale-increase inc)
		    (mac-scroll-point-to-y target-point target-y)
		    (with-selected-window original-selected-window
		      (sit-for 0.017)))
		(when (/= text-scale-mode-amount level)
		  (text-scale-set level)
		  (mac-scroll-point-to-y target-point target-y)))))))))

(defun mac-magnify-text-scale-or-overview-tab-group (event)
  "Trigger tab group overview or forward EVENT to `mac-magnify-text-scale'.
Tab group overview, which is available on macOS 10.13 and later,
is triggered by the pinch close gesture on a trackpad unless the
text is magnified, and the subsequent pinch gestures are ignored
until you release the fingers so they do not cause unwanted text
scaling.  If you don't want to trigger tab group overview by the
pinch close gesture, then remap this command to
`mac-magnify-text-scale'."
  (interactive "e")
  (if (not (or (> (car (x-server-version)) 10)
               (>= (cadr (x-server-version)) 13)))
      (mac-magnify-text-scale event)
    (let ((phase (plist-get (nth 3 event) :phase)))
      (if (or (null phase) (eq phase 'began))
          (setq mac-ignore-magnify-events nil))
      (when (not mac-ignore-magnify-events)
        (let ((frame (window-frame (posn-window (event-start event)))))
          (if (and (eq phase 'began)
                   (eq (event-basic-type event) 'magnify-down)
                   (mac-frame-tab-group-property frame :selected-frame)
                   (not (mac-frame-tab-group-property frame
                                                      :overview-visible-p))
                   (with-selected-window (posn-window (event-start event))
                     (not (and (boundp 'text-scale-mode-amount)
                               (> text-scale-mode-amount 0)))))
              (progn
                (setq mac-ignore-magnify-events t)
                (mac-set-frame-tab-group-property frame :overview-visible-p t)))
          (mac-magnify-text-scale event))))))

(defun mac-mouse-turn-on-fullscreen (event)
  "Turn on fullscreen in response to the mouse event EVENT."
  (interactive "e")
  (if (not (eq (plist-get (nth 3 event) :phase) 'ended))
      (let ((frame (window-frame (posn-window (event-start event)))))
        (if (not (eq (frame-parameter frame 'fullscreen) 'fullboth))
            (set-frame-parameter frame 'fullscreen 'fullboth)))))

(defun mac-mouse-turn-off-fullscreen (event)
  "Turn off fullscreen in response to the mouse event EVENT."
  (interactive "e")
  (if (not (eq (plist-get (nth 3 event) :phase) 'ended))
      (let ((frame (window-frame (posn-window (event-start event)))))
        (if (frame-parameter frame 'fullscreen)
            (set-frame-parameter frame 'fullscreen nil)))))

(global-set-key [magnify-up] 'mac-magnify-text-scale-or-overview-tab-group)
(global-set-key [magnify-down] 'mac-magnify-text-scale-or-overview-tab-group)
(global-set-key [S-magnify-up] 'mac-mouse-turn-on-fullscreen)
(global-set-key [S-magnify-down] 'mac-mouse-turn-off-fullscreen)
(global-set-key [rotate-left] 'ignore)
(global-set-key [rotate-right] 'ignore)
(global-set-key [S-rotate-left] 'ignore)
(global-set-key [S-rotate-right] 'ignore)


;;; Frame tabbing (macOS 10.12 and later)
(declare-function mac-send-action "macfns.c" (action &optional dry-run-p))

(defun mac-ctl-x-5-revolve-frame-tabbing (arg)
  "Behave as C-x 5 typed, but revolve frame tabbing setting temporarily.
It is changed as automatic -> inverted -> preferred -> disallowed
-> automatic -> ... as you repeat the last key (usually `5').
See also `mac-frame-tabbing'."
  (interactive "P")
  (let* ((command-keys (this-command-keys))
         (mac-frame-tabbing
          (cadr (memq mac-frame-tabbing '(nil automatic inverted t))))
         (seq (let ((overriding-local-map ctl-x-5-map))
                (read-key-sequence
                 (format "with frame tabbing %s:"
                         (cond ((null mac-frame-tabbing) "disallowed")
                               ((eq mac-frame-tabbing t) "preferred")
                               (t (symbol-name mac-frame-tabbing)))) t)))
         (command (if (memq (nth 3 (current-input-mode)) (append seq '()))
                      (signal 'quit nil)
                    (or (lookup-key ctl-x-5-map seq) 'undefined))))
    (setq prefix-arg arg)
    (command-execute command nil (vconcat command-keys (this-command-keys)))))

(defun mac-frame-single-tab-p (&optional frame)
  "Return t if FRAME belongs to a tab group having a single frame."
  (= (length (mac-frame-tab-group-property frame :frames)) 1))

(defun mac-frame-multiple-tabs-p (&optional frame)
  "Return t if FRAME belongs to a tab group having multiple frames."
  (> (length (mac-frame-tab-group-property frame :frames)) 1))

(defun mac-toggle-tab-bar (&optional frame)
  "Toggle tab bar visibility of the tab group for FRAME."
  (interactive)
  (mac-set-frame-tab-group-property
   frame :tab-bar-visible-p
   (not (mac-frame-tab-group-property frame :tab-bar-visible-p))))

(defun mac-next-tab (arg)
  "Select the ARGth next tab on the current tab group."
  (interactive "p")
  (let ((frames (mac-frame-tab-group-property nil :frames))
        tail)
    (when (< arg 0)
      (setq frames (nreverse frames))
      (setq arg (- arg)))
    (setq tail (memq (selected-frame) frames))
    (dotimes (_ arg)
      (setq tail (or (cdr tail) frames)))
    (mac-set-frame-tab-group-property nil :selected-frame (car tail))))

(defun mac-previous-tab (arg)
  "Select the ARGth previous tab on the current tab group."
  (interactive "p")
  (mac-next-tab (- arg)))

(defun mac-next-tab-or-toggle-tab-bar (arg)
  "Select the ARGth next tab or toggle tab bar visibility.
Do the former if the current tab group has multiple tabs, and
otherwise do the latter.  If you don't want to togggle tab bar
visibility, then remap this command to `mac-next-tab'."
  (interactive "p")
  (if (mac-frame-single-tab-p)
      (mac-toggle-tab-bar)
    (mac-next-tab arg)))

(defun mac-previous-tab-or-toggle-tab-bar (arg)
  "Select the ARGth previous tab or toggle tab bar visibility.
Do the former if the current tab group has multiple tabs, and
otherwise do the latter.  If you don't want to togggle tab bar
visibility, then remap this command to `mac-previous-tab'."
  (interactive "p")
  (if (mac-frame-single-tab-p)
      (mac-toggle-tab-bar)
    (mac-previous-tab arg)))

(defun mac-move-tab-to-new-frame (&optional frame)
  "Move the tab for FRAME to a new tab group."
  (interactive)
  (mac-set-frame-tab-group-property
   frame :frames (delq (or frame (selected-frame))
                       (mac-frame-tab-group-property frame :frames))))

(defun mac-toggle-tab-group-overview (&optional frame)
  "Toggle tab overview visibility of the tab group for FRAME."
  (interactive)
  (mac-set-frame-tab-group-property
   frame :overview-visible-p
   (not (mac-frame-tab-group-property frame :overview-visible-p))))

(defun mac-merge-all-frame-tabs ()
  "Merge all frame tabs into the current tab group."
  (interactive)
  (mac-send-action 'mergeAllWindows))


;;; Window system initialization.

(defun mac-win-suspend-error ()
  "Report an error when a suspend is attempted.
This returns an error if any Emacs frames are Mac frames."
  ;; Don't allow suspending if any of the frames are Mac frames.
  (if (memq 'mac (mapcar #'window-system (frame-list)))
      (error "Cannot suspend Emacs while a Mac GUI frame exists")))

(defvar mac-initialized nil
  "Non-nil if the Mac window system has been initialized.")

(declare-function x-open-connection "macfns.c"
		  (display &optional xrm-string must-succeed))
(declare-function x-get-resource "frame.c"
		  (attribute class &optional component subclass))
(declare-function x-parse-geometry "frame.c" (string))

(defun mac-handle-args (args)
  "Process the Mac-related command line options in ARGS.
It records Mac-specific options in `mac-startup-options' as a
value for the key symbol `command-line' after processing the
standard ones in `x-handle-args'."
  (setq args (x-handle-args args))
  (let ((invocation-args args)
	mac-specific-args)
    (setq args nil)
    (while (and invocation-args
		(not (equal (car invocation-args) "--")))
      (let ((this-switch (car invocation-args))
	    case-fold-search)
	(setq invocation-args (cdr invocation-args))
	(cond ((string-match "\\`-psn_" this-switch)
	       (push this-switch mac-specific-args))
	      ;; Cocoa applications let you set temporary preferences
	      ;; on the command line.  We regard option names starting
	      ;; with a capital letter and containing at least 2
	      ;; letters as such preference settings.
	      ((and (string-match "\\`-[A-Z]." this-switch)
		    invocation-args
		    (not (string-match "\\`-" (car invocation-args))))
	       (setq mac-specific-args
		     (cons (car invocation-args)
			   (cons this-switch mac-specific-args)))
	       (setq invocation-args (cdr invocation-args)))
	      (t
	       (setq args (cons this-switch args))))))
    (when mac-specific-args
      (setq mac-specific-args (nreverse mac-specific-args))
      (push (cons 'command-line mac-specific-args) mac-startup-options))
    (nconc (nreverse args) invocation-args)))

(defun mac-handle-startup-keyboard-modifiers ()
  (let ((modifiers-value (cdr (assq 'keyboard-modifiers mac-startup-options)))
	(shift-mask (cdr (assq 'shift mac-keyboard-modifier-mask-alist))))
    (if (and modifiers-value (/= (logand modifiers-value shift-mask) 0))
	;; Shift modifier at startup means "minimum customizations"
	;; like `-Q' command-line option.
	(setq init-file-user nil
	      site-run-file nil
	      inhibit-x-resources t))))

(defun mac-exit-splash-screen ()
  "Stop displaying the splash screen buffer with possibly an animation."
  (interactive)
  (mac-start-animation (selected-window) :type 'fade-out)
  (exit-splash-screen))

(cl-defmethod window-system-initialization (&context (window-system mac)
                                            &optional _display)
  "Initialize Emacs for Mac GUI frames."
  (cl-assert (not mac-initialized))

  ;; Make sure we have a valid resource name.
  (or (stringp x-resource-name)
      (let (i)
	(setq x-resource-name (copy-sequence invocation-name))

	;; Change any . or * characters in x-resource-name to hyphens,
	;; so as not to choke when we use it in X resource queries.
	(while (setq i (string-match "[.*]" x-resource-name))
	  (aset x-resource-name i ?-))))

  (x-open-connection "Mac"
		     x-command-line-resources
		     ;; Exit Emacs with fatal error if this fails.
		     t)

  (mac-handle-startup-keyboard-modifiers)

  ;; Create the default fontset.
  (create-default-fontset)

  (set-fontset-font t nil (font-spec :family "Apple Symbols"
				     :registry "iso10646-1") nil 'prepend)
  (when (and (string-match "darwin\\([0-9]+\\)" system-configuration)
	     (>= (string-to-number (match-string 1 system-configuration)) 11))
    ;; Built on Mac OS X 10.7 or later.
    (let ((spec (font-spec :family "Apple Color Emoji" :registry "iso10646-1")))
      (set-fontset-font t nil spec nil 'append)
      ;; Work around lots of font lookups in emoji compositions.
      (set-fontset-font t #xFE0F spec)	; Variation Selector 16
      (set-fontset-font t '(#x1F1E6 . #x1F1FF) spec) ; Regional Indicator Syms
      (set-fontset-font t '(#x1F3FB . #x1F3FF) spec))) ; Emoji Modifiers
  (mac-setup-composition-function-table)
  ;; (set-fontset-font t nil (font-spec :family "LastResort") nil 'append)
  (set-fontset-font t '(#x20000 . #x2FFFF)
		    '("HanaMinB" . "unicode-sip") nil 'append)

  ;; Create fontset specified in X resources "Fontset-N" (N is 0, 1, ...).
  (create-fontset-from-x-resource)

  ;; Apply a geometry resource to the initial frame.  Put it at the end
  ;; of the alist, so that anything specified on the command line takes
  ;; precedence.
  (let* ((res-geometry (x-get-resource "geometry" "Geometry"))
	 parsed)
    (if res-geometry
	(progn
	  (setq parsed (x-parse-geometry res-geometry))
	  ;; If the resource specifies a position,
	  ;; call the position and size "user-specified".
	  (if (or (assq 'top parsed) (assq 'left parsed))
	      (setq parsed (cons '(user-position . t)
				 (cons '(user-size . t) parsed))))
	  ;; All geometry parms apply to the initial frame.
	  (setq initial-frame-alist (append initial-frame-alist parsed))
	  ;; The size parms apply to all frames.  Don't set it if there are
	  ;; sizes there already (from command line).
	  (if (and (assq 'height parsed)
		   (not (assq 'height default-frame-alist)))
	      (setq default-frame-alist
		    (cons (cons 'height (cdr (assq 'height parsed)))
			  default-frame-alist)))
	  (if (and (assq 'width parsed)
		   (not (assq 'width default-frame-alist)))
	      (setq default-frame-alist
		    (cons (cons 'width (cdr (assq 'width parsed)))
			  default-frame-alist))))))

  ;; Check the reverseVideo resource.
  (let ((case-fold-search t))
    (let ((rv (x-get-resource "reverseVideo" "ReverseVideo")))
      (if (and rv
	       (string-match "^\\(true\\|yes\\|on\\)$" rv))
	  (setq default-frame-alist
		(cons '(reverse . t) default-frame-alist)))))

  ;; Don't let Emacs suspend under Mac.
  (add-hook 'suspend-hook 'mac-win-suspend-error)

  ;; Turn off window-splitting optimization; Mac is usually fast enough
  ;; that this is only annoying.
  (setq split-window-keep-point t)

  ;; Enable CLIPBOARD copy/paste through menu bar commands.
  (menu-bar-enable-clipboard)

  (mac-setup-system-coding-system)
  (mac-setup-selection-properties)

  ;; Processing of Apple events are deferred at the startup time.  For
  ;; example, files dropped onto the Emacs application icon can only
  ;; be processed when the initial frame has been created: this is
  ;; where the files should be opened.
  (add-hook 'after-init-hook 'mac-process-deferred-apple-events)
  (run-with-idle-timer 5 t 'mac-cleanup-expired-apple-events)

  ;; If Emacs is invoked from the command line, the initial frame
  ;; doesn't get focused.
  (add-hook 'after-init-hook
	    (lambda () (if (and (let ((first-option
				       (cadr (assq 'command-line
						   mac-startup-options))))
				  (not (and first-option
					    (string-match "\\`-psn_"
							  first-option))))
				(eq (frame-visible-p (selected-frame)) t))
			   (x-focus-frame (selected-frame)))))

  (add-hook 'after-init-hook
	    (lambda ()
	      (mouse-wheel-mode 0)
	      (mac-mouse-wheel-mode 1)))

  (add-hook 'menu-bar-update-hook 'mac-setup-help-topics)
  (run-with-idle-timer 0.1 nil 'mac-setup-help-topics)

  (substitute-key-definition 'exit-splash-screen 'mac-exit-splash-screen
			     splash-screen-keymap)

  ;; Running on macOS 10.10 or later.
  (when (or (> (car (x-server-version)) 10) (>= (cadr (x-server-version)) 10))
    (advice-add 'fancy-startup-screen :after
                (lambda (&optional _concise)
                  (set (make-local-variable 'face-remapping-alist)
                       '((default :stipple "alpha:50%"))))))

  (if (eq (lookup-key global-map [C-down-mouse-1]) 'mouse-buffer-menu)
      (global-set-key [C-down-mouse-1] 'mac-mouse-buffer-menu))
  (push 'mac-mouse-buffer-menu selection-inhibit-update-commands)

  ;; Running on macOS 10.12 or later.
  (when (or (> (car (x-server-version)) 10) (>= (cadr (x-server-version)) 12))
    (define-key ctl-x-5-map "5" 'mac-ctl-x-5-revolve-frame-tabbing)
    (define-key-after menu-bar-showhide-menu [mac-toggle-tab-bar]
      '(menu-item "Tab-bar" mac-toggle-tab-bar
                  :enable (mac-frame-single-tab-p)
                  :button (:toggle . (mac-frame-tab-group-property
                                      nil :tab-bar-visible-p)))
      'menu-bar-mode)
    (when (or (> (car (x-server-version)) 10) (>= (cadr (x-server-version)) 13))
      (define-key-after menu-bar-showhide-menu [mac-toggle-tab-group-overview]
        '(menu-item "Tab Group Overview" mac-toggle-tab-group-overview
                    :enable (mac-frame-tab-group-property nil :selected-frame)
                    :button (:toggle . (mac-frame-tab-group-property
                                        nil :overview-visible-p)))
        'mac-toggle-tab-bar))
    (define-key-after global-buffers-menu-map [mac-separator-tab]
      menu-bar-separator)
    (define-key-after global-buffers-menu-map [mac-next-tab]
      '(menu-item "Show Next Tab" mac-next-tab-or-toggle-tab-bar
                  :enable (mac-frame-multiple-tabs-p)))
    (global-set-key [(control tab)] 'mac-next-tab-or-toggle-tab-bar)
    (define-key-after global-buffers-menu-map [mac-previous-tab]
      '(menu-item "Show Previous Tab" mac-previous-tab-or-toggle-tab-bar
                  :enable (mac-frame-multiple-tabs-p)))
    (global-set-key [(control shift tab)] 'mac-previous-tab-or-toggle-tab-bar)
    (define-key-after global-buffers-menu-map [mac-move-tab-to-new-frame]
      '(menu-item "Move Tab to New Frame" mac-move-tab-to-new-frame
                  :enable (mac-frame-multiple-tabs-p)))
    (define-key-after global-buffers-menu-map [mac-merge-all-frame-tabs]
      '(menu-item "Merge All Frames" mac-merge-all-frame-tabs
                  :enable (mac-send-action 'mergeAllWindows t))))

  (x-apply-session-resources)
  (add-to-list 'display-format-alist '("\\`Mac\\'" . mac))
  (setq mac-initialized t))

(declare-function mac-own-selection-internal "macselect.c"
		  (selection value &optional frame))
(declare-function mac-disown-selection-internal "macselect.c"
		  (selection &optional time-object terminal))
(declare-function mac-selection-owner-p "macselect.c"
		  (&optional selection terminal))
(declare-function mac-selection-exists-p "macselect.c"
		  (&optional selection terminal))
(declare-function mac-get-selection-internal "macselect.c"
		  (selection-symbol target-type &optional time-stamp terminal))

(cl-defmethod handle-args-function (args &context (window-system mac))
  (mac-handle-args args))

(cl-defmethod frame-creation-function (params &context (window-system mac))
  (x-create-frame-with-faces params))

(cl-defmethod gui-backend-set-selection (selection value
                                         &context (window-system mac))
  (if value (mac-own-selection-internal selection value)
    (mac-disown-selection-internal selection)))

(cl-defmethod gui-backend-selection-owner-p (selection
                                             &context (window-system mac))
  (mac-selection-owner-p selection))

(cl-defmethod gui-backend-selection-exists-p (selection
                                              &context (window-system mac))
  (mac-selection-exists-p selection))

(cl-defmethod gui-backend-get-selection (selection-symbol target-type
                                         &context (window-system mac)
                                         &optional time-stamp terminal)
  (mac-get-selection-internal selection-symbol target-type time-stamp terminal))

;; Initiate drag and drop
(define-key special-event-map [drag-n-drop] 'mac-dnd-handle-drag-n-drop-event)

(defcustom mac-system-symbol-map
  (mapcar (lambda (arg)
	    (cons (purecopy (car arg)) (purecopy (cdr arg))))
  '(
    ("etc/images/attach" . "paperclip")
    ("etc/images/back-arrow". "arrowshape.turn.up.backward")
    ("etc/images/bookmark_add" . "bookmark")
    ("etc/images/cancel" . "xmark.circle")
    ("etc/images/checked" . "checkmark.square")
    ("etc/images/close" . "xmark")
    ("etc/images/connect" . "rectangle.connected.to.line.below")
    ("etc/images/contact" . "person.crop.square.fill.and.at.rectangle")
    ("etc/images/copy" . "doc.on.doc")
    ("etc/images/cut" . "scissors")
    ("etc/images/data-save" . "tray.and.arrow.down")
    ("etc/images/delete" . "trash")
    ("etc/images/describe" . "doc.badge.gearshape")
    ("etc/images/diropen" . "folder")
    ("etc/images/disconnect" . "slider.horizontal.below.rectangle")
    ("etc/images/exit" . "pip.exit")
    ("etc/images/fwd-arrow". "arrowshape.turn.up.forward")
    ("etc/images/help" . "lifepreserver")
    ("etc/images/home" . "house")
    ("etc/images/index" . "list.bullet.rectangle")
    ("etc/images/info" . "lightbulb")
    ("etc/images/jump-to" . "arrow.turn.right.down")
    ;; ("etc/images/letter")
    ("etc/images/left-arrow" . "chevron.backward")
    ("etc/images/lock" . "lock")
    ("etc/images/lock-broken" . "lock.slash")
    ("etc/images/lock-ok" . "lock.circle")
    ;; ("etc/images/mh-logo")
    ("etc/images/new" . "doc.badge.plus")
    ("etc/images/next-node" . "chevron.forward.square")
    ("etc/images/next-page" . "book")
    ("etc/images/open" . "doc")
    ("etc/images/paste" . "doc.on.clipboard")
    ("etc/images/preferences" . "wrench.and.screwdriver")
    ("etc/images/prev-node" . "chevron.backward.square")
    ("etc/images/print" . "printer")
    ("etc/images/redo" . "arrow.uturn.forward")
    ("etc/images/refresh" . "arrow.clockwise")
    ("etc/images/right-arrow" . "chevron.forward")
    ("etc/images/save" . "square.and.arrow.down")
    ("etc/images/saveas" . "square.and.arrow.down.on.square")
    ("etc/images/search" . "magnifyingglass")
    ("etc/images/search-replace" . "text.magnifyingglass")
    ;; ("etc/images/separator")
    ("etc/images/show" . "eye")
    ;; ("etc/images/sort-ascending")
    ;; ("etc/images/sort-column-ascending")
    ;; ("etc/images/sort-criteria")
    ;; ("etc/images/sort-descending")
    ;; ("etc/images/sort-row-ascending")
    ("etc/images/spell" . "textformat.abc.dottedunderline")
    ;; ("etc/images/splash")
    ("etc/images/unchecked" . "square")
    ("etc/images/undo" . "arrow.uturn.backward")
    ("etc/images/up-arrow" . "chevron.up")
    ("etc/images/up-node" . "chevron.up.square")
    ("etc/images/zoom-in" . "plus.magnifyingglass")
    ("etc/images/zoom-out" . "minus.magnifyingglass")
    ))
  "How icons for tool bars are mapped to macOS system symbols.
Emacs must be run on macOS 11 and later for this to have any effect."
  :package-version '(Mac\ port . "7.10")
  :type '(alist :key-type (string :tag "Emacs icon")
                :value-type (choice (repeat (string :tag "Symbol name"))
				    (string :tag "Symbol name")))
  :group 'mac)

(defcustom mac-icon-map-list '(mac-system-symbol-map)
  "A list of alists that map icon file names to system symbols.
The alists are searched in the order they appear.  The first match is used.
The keys in the alists are file names without extension and with two directory
components.  For example, to map /usr/local/share/emacs/26.3/etc/images/new.xpm
to system symbol doc.badge.plus, use:

  (\"etc/images/new\" . \"doc.badge.plus\")

The list elements are either the symbol name for the alist or the
alist itself.

If you don't want system symbols, set the variable to nil."
  :package-version '(Mac\ port . "7.10")
  :type '(choice (const :tag "Don't use system symbols" nil)
		 (repeat
                  (choice symbol
			  (alist :key-type (string :tag "Emacs icon")
                                 :value-type
                                 (choice (repeat (string :tag "Symbol name"))
				         (string :tag "Symbol name"))))))
  :group 'mac)

(defconst mac-system-symbol-cache (make-hash-table :weakness t :test 'equal))

(defun mac-map-system-symbol (file)
  "Map icon with file name FILE to a macOS system symbol name.
This uses `mac-icon-map-list' to map icon file names to macOS
system symbol names."
  (when (stringp file)
    (or (gethash file mac-system-symbol-cache)
	(puthash
	 file
	 (save-match-data
	   (let* ((file-sans (file-name-sans-extension file))
		  (key (and (string-match "/\\([^/]+/[^/]+/[^/]+$\\)"
					  file-sans)
			    (match-string 1 file-sans)))
		  (icon-map mac-icon-map-list)
		  elem value)
	     (while (and (null value) icon-map)
	       (setq elem (car icon-map)
		     value (assoc-string (or key file-sans)
					 (if (symbolp elem)
					     (symbol-value elem)
					   elem))
		     icon-map (cdr icon-map)))
	     (and value (cdr value))))
	 mac-system-symbol-cache))))

(provide 'mac-win)
(provide 'term/mac-win)

;;; mac-win.el ends here
