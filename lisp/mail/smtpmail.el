;;; smtpmail.el --- simple SMTP protocol (RFC 821) for sending mail  -*- lexical-binding:t -*-

;; Copyright (C) 1995-1996, 2001-2025 Free Software Foundation, Inc.

;; Author: Tomoji Kagatani <kagatani@rbc.ncl.omron.co.jp>
;; Maintainer: emacs-devel@gnu.org
;; w32 Maintainer: Brian D. Carlstrom <bdc@ai.mit.edu>
;; ESMTP support: Simon Leinen <simon@switch.ch>
;; Hacked by Mike Taylor, 11th October 1999 to add support for
;; automatically appending a domain to RCPT TO: addresses.
;; AUTH=LOGIN support: Stephen Cranefield <scranefield@infoscience.otago.ac.nz>
;; Keywords: mail

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

;; Send Mail to smtp host from smtpmail temp buffer.

;; Please add these lines in your .emacs(_emacs) or use customize.
;;
;;(setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
;;(setq message-send-mail-function 'smtpmail-send-it) ; if you use message/Gnus
;;(setq smtpmail-smtp-server "YOUR SMTP HOST")
;;(setq smtpmail-local-domain "YOUR DOMAIN NAME")
;;(setq smtpmail-sendto-domain "YOUR DOMAIN NAME")
;;(setq smtpmail-debug-info t) ; only to debug problems

;; To queue mail, set `smtpmail-queue-mail' to t and use
;; `smtpmail-send-queued-mail' to send.

;; Modified by Stephen Cranefield <scranefield@infoscience.otago.ac.nz>,
;; 22/6/99, to support SMTP Authentication by the AUTH=LOGIN mechanism.
;; See http://help.netscape.com/products/server/messaging/3x/info/smtpauth.html
;; Rewritten by Simon Josefsson to use same credential variable as AUTH
;; support below.

;; Modified by Simon Josefsson <jas@pdc.kth.se>, 22/2/99, to support SMTP
;; Authentication by the AUTH mechanism.
;; See https://www.ietf.org/rfc/rfc2554.txt

;;; Code:
;;; Dependencies

(require 'sendmail)
(require 'auth-source)
(autoload 'mail-strip-quoted-names "mail-utils")
(autoload 'message-make-date "message")
(autoload 'message-make-message-id "message")
(autoload 'rfc2104-hash "rfc2104")

;;; Options

(defgroup smtpmail nil
  "SMTP protocol for sending mail."
  :group 'mail)

(defcustom smtpmail-default-smtp-server nil
  "Specify default SMTP server.
This only has effect if you specify it before loading the smtpmail library."
  :type '(choice (const nil) string))

(defcustom smtpmail-smtp-server
  (or (getenv "SMTPSERVER") smtpmail-default-smtp-server)
  "The name of the host running SMTP server."
  :type '(choice (const nil) string))

(defcustom smtpmail-smtp-service 25
  "SMTP service port number.
The default value would be \"smtp\" or 25."
  :type '(choice (integer :tag "Port") (string :tag "Service")))

(defcustom smtpmail-smtp-user nil
  "User name to use when looking up credentials in the authinfo file.
If non-nil, only consider credentials for the specified user."
  :version "24.1"
  :type '(choice (const nil) string))

(defcustom smtpmail-local-domain nil
  "Local domain name without a host name.
If the function `system-name' returns the full internet address,
don't define this value."
  :type '(choice (const nil) string))

(defcustom smtpmail-stream-type nil
  "Type of SMTP connections to use.
This may be either nil (upgrade with STARTTLS if possible),
`starttls' (refuse to send if STARTTLS isn't available),
`plain' (never use STARTTLS), or `ssl' (to use TLS/SSL)."
  :version "24.1"
  :type '(choice (const :tag "Possibly upgrade to STARTTLS" nil)
		 (const :tag "Always use STARTTLS" starttls)
		 (const :tag "Never use STARTTLS" plain)
		 (const :tag "Use TLS/SSL" ssl)))

(defcustom smtpmail-sendto-domain nil
  "Local domain name without a host name.
This is appended (with an @-sign) to any specified recipients which do
not include an @-sign, so that each RCPT TO address is fully qualified.
\(Some configurations of sendmail require this.)

Don't bother to set this unless you have get an error like:
	Sending failed; 501 <someone>: recipient address must contain a domain."
  :type '(choice (const nil) string))

(defcustom smtpmail-debug-info nil
  "Whether to print info in buffer *trace of SMTP session to <somewhere>*.
See also `smtpmail-debug-verb' which determines if the SMTP protocol should
be verbose as well."
  :type 'boolean)

(defcustom smtpmail-debug-verb nil
  "Whether this library sends the SMTP VERB command or not.
The commands enables verbose information from the SMTP server."
  :type 'boolean)

(defcustom smtpmail-code-conv-from nil
  "Coding system for encoding outgoing mail.
Used for the value of `sendmail-coding-system' when
`select-message-coding-system' is called."
  :type 'coding-system)

(defcustom smtpmail-queue-mail nil
  "Non-nil means mail is queued; otherwise it is sent immediately.
If queued, it is stored in the directory `smtpmail-queue-dir' and
sent with `smtpmail-send-queued-mail'.  Also see
`smtpmail-store-queue-variables'."
  :type 'boolean)

(defcustom smtpmail-queue-dir "~/Mail/queued-mail/"
  "Directory where `smtpmail.el' stores queued mail.
This directory should not be writable by other users."
  :type 'directory)

(defcustom smtpmail-warn-about-unknown-extensions nil
  "If set, print warnings about unknown SMTP extensions.
This is mainly useful for development purposes, to learn about
new SMTP extensions that might be useful to support."
  :type 'boolean
  :version "21.1")

(defcustom smtpmail-queue-index-file "index"
  "File name of queued mail index.
This is relative to `smtpmail-queue-dir'."
  :type 'string)

(defcustom smtpmail-servers-requiring-authorization nil
  "Regexp matching servers that require authorization.
Normally smtpmail will try first to send emails via SMTP without
user/password credentials, and then retry using credentials if
the server says that it requires it.  If the server name matches
this regexp, smtpmail will send over the credentials on the first
attempt."
  :type '(choice regexp (const :tag "None" nil))
  :version "27.1")

(defcustom smtpmail-retries 10
  "The number of times smtpmail will retry sending when getting transient errors.
These are errors with a code of 4xx from the SMTP server, which
mean \"try again\"."
  :type 'natnum
  :version "27.1")

(defcustom smtpmail-store-queue-variables nil
  "If non-nil, store SMTP variables when queueing mail.
These will then be used when sending the queue."
  :type 'boolean
  :version "28.1")

;;; Variables

(defvar smtpmail-recipient-address-list nil)
(defvar smtpmail--stored-queue-variables
  '(smtpmail-smtp-server
    smtpmail-stream-type
    smtpmail-smtp-service
    smtpmail-smtp-user))

(defvar smtpmail-queue-counter 0)

;; Buffer-local variable.
(defvar smtpmail-read-point)

(defvar smtpmail-auth-supported '(cram-md5 plain login)
  "List of supported SMTP AUTH mechanisms.
The list is in preference order.
Every element should have a matching `cl-defmethod'
for `smtpmail-try-auth-method'.")

(defvar smtpmail-mail-address nil
  "Value to use for envelope-from address for mail from ambient buffer.")

;;; Functions

;;;###autoload
(defun smtpmail-send-it ()
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " smtpmail errors")
		  0))
	(tembuf (generate-new-buffer " smtpmail temp"))
	(case-fold-search nil)
	delimline
	result
	(mailbuf (current-buffer))
        ;; Examine this variable now, so that
	;; local binding in the mail buffer will take effect.
	(smtpmail-mail-address
         (save-restriction
           ;; Only look at the headers when fetching the
           ;; envelope address.
           (message-narrow-to-headers)
           (or (and mail-specify-envelope-from (mail-envelope-from))
               (let ((from (mail-fetch-field "from")))
	         (and from
		      (cadr (mail-extract-address-components from))))
	       (smtpmail-user-mail-address))))
	(smtpmail-code-conv-from
	 (if enable-multibyte-characters
	     (let ((sendmail-coding-system smtpmail-code-conv-from))
	       (select-message-coding-system)))))
    (unwind-protect
	(with-current-buffer tembuf
	  (erase-buffer)
	  ;; Use the same `buffer-file-coding-system' as in the mail
	  ;; buffer, otherwise any `write-region' invocations (e.g., in
	  ;; mail-do-fcc below) will annoy with asking for a suitable
	  ;; encoding.
	  (set-buffer-file-coding-system smtpmail-code-conv-from nil t)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (mail-sendmail-undelimit-header)
	  (setq delimline (point-marker))
          ;; (sendmail-synch-aliases)
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    ;; We used to process Resent-... headers here,
	    ;; but it was not done properly, and the job
	    ;; is done correctly in `smtpmail-deduce-address-list'.
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:\\([ \t]*\n\\)+\\b" delimline t)
		(replace-match "")
	      ;; This one matches a Subject just before the header delimiter.
	      (if (and (re-search-forward "^Subject:\\([ \t]*\n\\)+" delimline t)
		       (= (match-end 0) delimline))
		  (replace-match "")))
	    ;; Put the "From:" field in unless for some odd reason
	    ;; they put one in themselves.
	    (goto-char (point-min))
	    (if (not (re-search-forward "^From:" delimline t))
		(let* ((login smtpmail-mail-address)
		       (fullname (user-full-name)))
		  (cond ((eq mail-from-style 'angles)
			 (insert "From: " fullname)
			 (let ((fullname-start (+ (point-min) 6))
			       (fullname-end (point-marker)))
			   (goto-char fullname-start)
			   ;; Look for a character that cannot appear unquoted
			   ;; according to RFC 822 or its successors.
			   (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
						  fullname-end 1)
			       (progn
				 ;; Quote fullname, escaping specials.
				 (goto-char fullname-start)
				 (insert "\"")
				 (while (re-search-forward "[\"\\]"
							   fullname-end 1)
				   (replace-match "\\\\\\&" t))
				 (insert "\""))))
			 (insert " <" login ">\n"))
			((eq mail-from-style 'parens)
			 (insert "From: " login " (")
			 (let ((fullname-start (point)))
			   (insert fullname)
			   (let ((fullname-end (point-marker)))
			     (goto-char fullname-start)
			     ;; RFC 822 and its successors say \ and
			     ;; nonmatching parentheses must be
			     ;; escaped in comments.
			     ;; Escape every instance of ()\ ...
			     (while (re-search-forward "[()\\]" fullname-end 1)
			       (replace-match "\\\\\\&" t))
			     ;; ... then undo escaping of matching parentheses,
			     ;; including matching nested parentheses.
			     (goto-char fullname-start)
			     (while (re-search-forward
				     "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
				     fullname-end 1)
			       (replace-match "\\1(\\3)" t)
			       (goto-char fullname-start))))
			 (insert ")\n"))
			((null mail-from-style)
			 (insert "From: " login "\n")))))
	    ;; Insert a `Message-Id:' field if there isn't one yet.
	    (goto-char (point-min))
	    (unless (re-search-forward "^Message-Id:" delimline t)
	      (insert "Message-Id: " (message-make-message-id) "\n"))
	    ;; Insert a `Date:' field if there isn't one yet.
	    (goto-char (point-min))
	    (unless (re-search-forward "^Date:" delimline t)
	      (insert "Date: " (message-make-date) "\n"))
	    ;; Possibly add a MIME header for the current coding system
	    (let (charset)
	      (goto-char (point-min))
	      (and (eq mail-send-nonascii 'mime)
		   (not (re-search-forward "^MIME-version:" delimline t))
		   (progn (skip-chars-forward "\0-\177")
			  (/= (point) (point-max)))
		   smtpmail-code-conv-from
		   (setq charset
			 (coding-system-get smtpmail-code-conv-from
					    'mime-charset))
		   (goto-char delimline)
		   (insert "MIME-version: 1.0\n"
			   "Content-type: text/plain; charset="
			   (symbol-name charset)
			   "\nContent-Transfer-Encoding: 8bit\n")))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    ;; Find and handle any Fcc fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^Fcc:" delimline t)
		;; Force `mail-do-fcc' to use the encoding of the mail
		;; buffer to encode outgoing messages on Fcc files.
		(let ((coding-system-for-write
		       ;; mbox files must have Unix EOLs.
		       (coding-system-change-eol-conversion
			smtpmail-code-conv-from 'unix)))
		  (mail-do-fcc delimline)))
	    (if mail-interactive
		(with-current-buffer errbuf
		  (erase-buffer))))
	  ;; Encode the header according to RFC2047.
	  (mail-encode-header (point-min) delimline)
	  ;; Get recipients' addresses
	  (setq smtpmail-recipient-address-list
                (smtpmail-deduce-address-list tembuf (point-min) delimline))

	  (smtpmail-do-bcc delimline)
          ;; Send or queue
	  (if (not smtpmail-queue-mail)
	      (if (not (null smtpmail-recipient-address-list))
		  (when (setq result
			      (smtpmail-via-smtp
			       smtpmail-recipient-address-list tembuf))
		    (error "Sending failed: %s"
                           (smtpmail--sanitize-error-message result)))
		(error "Sending failed; no recipients"))
	    (let* ((file-data
		    (expand-file-name
		     (format "%s_%i"
			     (format-time-string "%Y-%m-%d_%H:%M:%S")
			     (setq smtpmail-queue-counter
				   (1+ smtpmail-queue-counter)))
		     smtpmail-queue-dir))
		   (file-data (convert-standard-filename file-data))
		   (file-elisp (concat file-data ".el"))
		   (buffer-data (create-file-buffer file-data)))
	      (unless (file-exists-p smtpmail-queue-dir)
		(make-directory smtpmail-queue-dir t))
	      (with-current-buffer buffer-data
		(erase-buffer)
		(set-buffer-file-coding-system
		 ;; We will be reading the file with no-conversion in
		 ;; smtpmail-send-queued-mail below, so write it out
		 ;; with Unix EOLs.
		 (coding-system-change-eol-conversion
		  (or smtpmail-code-conv-from 'undecided)
		  'unix)
		 nil t)
		(insert-buffer-substring tembuf)
		(write-file file-data)
                (let ((coding-system-for-write 'utf-8))
                  (with-temp-buffer
                    (insert "(setq ")
                    (dolist (var (cons 'smtpmail-recipient-address-list
                                       ;; Perhaps store the server etc.
                                       (and smtpmail-store-queue-variables
                                            smtpmail--stored-queue-variables)))
                      (insert (format "     %s %S\n" var (symbol-value var))))
                    (insert ")\n")
                    (write-region (point-min) (point-max) file-elisp
                                  nil 'silent)))
		(write-region (concat file-data "\n") nil
                              (expand-file-name smtpmail-queue-index-file
                                                smtpmail-queue-dir)
                              t 'silent))
	      (kill-buffer buffer-data))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf)))))

;;;###autoload
(defun smtpmail-send-queued-mail ()
  "Send mail that was queued as a result of setting `smtpmail-queue-mail'."
  (interactive)
  (with-temp-buffer
    ;; Get index, get first mail, send it, update index, get second
    ;; mail, send it, etc...
    (let (file-data file-elisp
          (qfile (expand-file-name smtpmail-queue-index-file
                                   smtpmail-queue-dir))
          (stored (cons 'smtpmail-recipient-address-list
                        smtpmail--stored-queue-variables))
          smtpmail-recipient-address-list
          (smtpmail-smtp-server smtpmail-smtp-server)
          (smtpmail-stream-type smtpmail-stream-type)
          (smtpmail-smtp-service smtpmail-smtp-service)
          (smtpmail-smtp-user smtpmail-smtp-user)
	  result)
      (insert-file-contents qfile)
      (goto-char (point-min))
      (while (not (eobp))
	(setq file-data (buffer-substring (point) (line-end-position)))
	(setq file-elisp (concat file-data ".el"))
        (let ((coding-system-for-read 'utf-8))
          (with-temp-buffer
            (insert-file-contents file-elisp)
            (let ((form (read (current-buffer))))
              (when (or (not (consp form))
                        (not (eq (car form) 'setq))
                        (not (consp (cdr form))))
                (error "Unexpected code in %S: %S" file-elisp form))
              (cl-loop for (var val) on (cdr form) by #'cddr
                       when (memq var stored)
                       do (set var val)))))
	;; Insert the message literally: it is already encoded as per
	;; the MIME headers, and code conversions might guess the
	;; encoding wrongly.
	(with-temp-buffer
	  (let ((coding-system-for-read 'no-conversion))
	    (insert-file-contents file-data))
          (let ((smtpmail-mail-address
                 (or (and mail-specify-envelope-from
                          (save-restriction
                            ;; Only look at the headers when fetching the
                            ;; envelope address.
                            (message-narrow-to-headers)
                            (mail-envelope-from)))
                     user-mail-address)))
            (if (not smtpmail-recipient-address-list)
                (error "Sending failed; no recipients")
              (when (setq result (smtpmail-via-smtp
				  smtpmail-recipient-address-list
				  (current-buffer)))
		(error "Sending failed: %s"
                       (smtpmail--sanitize-error-message result))))))
	(delete-file file-data)
	(delete-file file-elisp)
        (delete-region (line-beginning-position) (line-beginning-position 2)))
      (write-region (point-min) (point-max) qfile))))

(defun smtpmail--sanitize-error-message (string)
  "Try to remove passwords and the like from SMTP error messages."
  (replace-regexp-in-string "\\bAUTH\\b.*" "AUTH" string))

(defun smtpmail-fqdn ()
  (if smtpmail-local-domain
      (concat (system-name) "." smtpmail-local-domain)
    (system-name)))

(defsubst smtpmail-cred-server (cred)
  (nth 0 cred))

(defsubst smtpmail-cred-port (cred)
  (nth 1 cred))

(defsubst smtpmail-cred-key (cred)
  (nth 2 cred))

(defsubst smtpmail-cred-user (cred)
  (nth 2 cred))

(defsubst smtpmail-cred-cert (cred)
  (nth 3 cred))

(defsubst smtpmail-cred-passwd (cred)
  (nth 3 cred))

(defun smtpmail-find-credentials (cred server port)
  (catch 'done
    (let ((l cred) el)
      (while (setq el (pop l))
	(when (and (equal server (smtpmail-cred-server el))
		   (equal port (smtpmail-cred-port el)))
	  (throw 'done el))))))

(defun smtpmail-maybe-append-domain (recipient)
  (if (or (not smtpmail-sendto-domain)
	  (string-search "@" recipient))
      recipient
    (concat recipient "@" smtpmail-sendto-domain)))

(defun smtpmail-command-or-throw (process string &optional code)
  (let (ret)
    (smtpmail-send-command process string)
    (unless (smtpmail-ok-p (setq ret (smtpmail-read-response process))
			   code)
      (throw 'done (format "%s in response to %s"
			   (smtpmail-response-text ret)
			   string)))
    ret))

(defun smtpmail-try-auth-methods (process supported-extensions host port
					  &optional ask-for-password)
  (setq port
	(if port
	    (format "%s" port)
	  "smtp"))
  (let* ((mechs (seq-intersection
                 smtpmail-auth-supported
                 (cdr-safe (assoc 'auth supported-extensions))
                 #'eq))
	 (auth-source-creation-prompts
          '((user  . "SMTP user name for %h: ")
            (secret . "SMTP password for %u@%h: ")))
         (auth-info (car
		     (auth-source-search
		      :host host
		      :port port
		      :user smtpmail-smtp-user
		      :max 1
		      :require (and ask-for-password
				    '(:user :secret))
		      :create ask-for-password)))
         (user (plist-get auth-info :user))
         (password (auth-info-password auth-info))
	 (save-function (and ask-for-password
			     (plist-get auth-info :save-function))))
    (when (and user
	       (not password))
      ;; The user has stored the user name, but not the password, so
      ;; ask for the password, even if we're not forcing that through
      ;; `ask-for-password'.
      (setq auth-info
	    (car
	     (auth-source-search
	      :max 1
	      :host host
	      :port port
	      :user smtpmail-smtp-user
	      :require '(:user :secret)
	      :create t))
	    password (auth-info-password auth-info)))
    (let ((mechs (or (ensure-list (plist-get auth-info :smtp-auth))
                     mechs))
          (result ""))
      (when (and mechs user password)
        (while (and mechs
                    (stringp result))
          (setq result (catch 'done
		         (smtpmail-try-auth-method
                          process (intern-soft (pop mechs)) user password))))
        ;; A string result is an error.
        (if (stringp result)
            (progn
              ;; All methods failed.
              ;; Forget the credentials.
	      (auth-source-forget+ :host host :port port)
              (throw 'done result))
          ;; Success.
	  (when save-function
	    (funcall save-function))
          result)))))

(cl-defgeneric smtpmail-try-auth-method (_process mech _user _password)
  "Perform authentication of type MECH for USER with PASSWORD.
MECH should be one of the values in `smtpmail-auth-supported'.
USER and PASSWORD should be non-nil."
  (error "Mechanism %S not implemented" mech))

(cl-defmethod smtpmail-try-auth-method
  (process (_mech (eql 'cram-md5)) user password)
  (let ((ret (smtpmail-command-or-throw process "AUTH CRAM-MD5")))
    (when (eq (car ret) 334)
      (let* ((challenge (substring (cadr ret) 4))
	     (decoded (base64-decode-string challenge))
	     (password (encode-coding-string password 'utf-8))
	     (user (encode-coding-string user 'utf-8))
	     (hash (rfc2104-hash 'md5 64 16 password decoded))
	     (response (concat user " " hash))
	     ;; Osamu Yamane <yamane@green.ocn.ne.jp>:
	     ;; SMTP auth fails because the SMTP server identifies
	     ;; only the first part of the string (delimited by
	     ;; new line characters) as a response from the
	     ;; client, and the rest as distinct commands.

	     ;; In my case, the response string is 80 characters
	     ;; long.  Without the no-line-break option for
	     ;; `base64-encode-string', only the first 76 characters
	     ;; are taken as a response to the server, and the
	     ;; authentication fails.
	     (encoded (base64-encode-string response t)))
	(smtpmail-command-or-throw process encoded)))))

(cl-defmethod smtpmail-try-auth-method
  (process (_mech (eql 'login)) user password)
  (smtpmail-command-or-throw process "AUTH LOGIN")
  (let ((password (encode-coding-string password 'utf-8))
        (user (encode-coding-string user 'utf-8)))
    (smtpmail-command-or-throw process (base64-encode-string user t))
    (smtpmail-command-or-throw process (base64-encode-string password t))))

(cl-defmethod smtpmail-try-auth-method
  (process (_mech (eql 'plain)) user password)
  ;; We used to send an empty initial request, and wait for an
  ;; empty response, and then send the password, but this
  ;; violate a SHOULD in RFC 2222 paragraph 5.1.  Note that this
  ;; is not sent if the server did not advertise AUTH PLAIN in
  ;; the EHLO response.  See RFC 2554 for more info.
  (let ((password (encode-coding-string password 'utf-8))
        (user (encode-coding-string user 'utf-8)))
    (smtpmail-command-or-throw
     process
     (concat "AUTH PLAIN "
             (base64-encode-string (concat "\0" user "\0" password) t))
     235)))

(cl-defmethod smtpmail-try-auth-method
  (process (_mech (eql 'xoauth2)) user password)
  (let ((ret (smtpmail-command-or-throw
              process
              (concat "AUTH XOAUTH2 "
                      (base64-encode-string
                       (concat "user=" user "\1auth=Bearer " password "\1\1")
                       t)))))
    (if (eq (car ret) 334)
        ;; When a server returns 334 server challenge, it usually means
        ;; the credentials it received were wrong (e.g. was an actual
        ;; password instead of an access token).  In such a case, we
        ;; should return a string with 535 to indicate a failure so that
        ;; smtpmail will try other authentication mechanisms.  See also
        ;; https://debbugs.gnu.org/78366.
        (throw 'done "535 5.7.8 Authentication credentials invalid")
      ret)))

(defun smtpmail-response-code (string)
  (when string
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (and (re-search-forward "^\\([0-9]+\\) " nil t)
	   (string-to-number (match-string 1))))))

(defun smtpmail-ok-p (response &optional code)
  (and (car response)
       (integerp (car response))
       (< (car response) 400)
       (or (null code)
	   (= code (car response)))))

(defun smtpmail-response-text (response)
  (mapconcat #'identity (cdr response) "\n"))

(defun smtpmail-query-smtp-server ()
  "Query for an SMTP server and try to contact it.
If the contact succeeds, customizes and saves `smtpmail-smtp-server'
and `smtpmail-smtp-service'.  This tries standard SMTP ports, and if
none works asks you to supply one.  If you know that you need to use
a non-standard port, you can set `smtpmail-smtp-service' in advance.
Returns an error if the server cannot be contacted."
  (let ((server (read-string "Outgoing SMTP mail server: "))
	(ports '(25 587))
	stream port prompted)
    (when (and smtpmail-smtp-service
	       (not (member smtpmail-smtp-service ports)))
      (push smtpmail-smtp-service ports))
    (while (and (not smtpmail-smtp-server)
		(setq port (pop ports)))
      (if (not (setq stream (condition-case ()
				(open-network-stream "smtp" nil server port)
			      (quit nil)
			      (error nil))))
	  ;; We've used up the list of default ports, so query the user.
	  (when (and (not ports)
		     (not prompted))
	    (push (read-number (format "Port number to use when contacting %s? "
				       server))
		  ports)
	    (setq prompted t))
	(customize-save-variable 'smtpmail-smtp-server server)
	(customize-save-variable 'smtpmail-smtp-service port)
	(delete-process stream)))
    (unless smtpmail-smtp-server
      (error "Couldn't contact an SMTP server"))))

(defun smtpmail-user-mail-address ()
  "Return `user-mail-address' if it's a valid email address."
  (and user-mail-address
       (let ((parts (split-string user-mail-address "@")))
	 (and (= (length parts) 2)
	      ;; There's a dot in the domain name.
	      (string-search "." (cadr parts))
	      user-mail-address))))

(defun smtpmail-via-smtp (recipient smtpmail-text-buffer
				    &optional ask-for-password
                                    send-attempts)
  (unless smtpmail-smtp-server
    (smtpmail-query-smtp-server))
  (let ((process nil)
        (send-attempts (or send-attempts 1))
	(host (or smtpmail-smtp-server
		  (error "`smtpmail-smtp-server' not defined")))
	(port smtpmail-smtp-service)
        ;; `smtpmail-mail-address' should be set to the appropriate
        ;; buffer-local value by the caller, but in case not:
        (envelope-from
         (save-restriction
           ;; Only look at the headers when fetching the
           ;; envelope address.
           (message-narrow-to-headers)
	   (or smtpmail-mail-address
	       (and mail-specify-envelope-from
		    (mail-envelope-from))
	       (let ((from (mail-fetch-field "from")))
	         (and from
		      (cadr (mail-extract-address-components from))))
	       (smtpmail-user-mail-address))))
	process-buffer
	result
	auth-mechanisms
	(supported-extensions '()))

    (when (and smtpmail-servers-requiring-authorization
               (string-match-p smtpmail-servers-requiring-authorization
                               smtpmail-smtp-server))
      (setq ask-for-password t))

    (unwind-protect
	(catch 'done
	  ;; get or create the trace buffer
	  (setq process-buffer
		(get-buffer-create
		 (format "*trace of SMTP session to %s*" host)))

	  ;; clear the trace buffer of old output
	  (with-current-buffer process-buffer
	    (setq buffer-undo-list t)
	    (erase-buffer))

	  ;; Open the connection to the server.
          ;; FIXME: Should we use raw-text-dos coding system to handle the r\n
          ;; for us?
	  (let ((coding-system-for-read 'binary)
		(coding-system-for-write 'binary))
	    (setq result
		  (open-network-stream
		   "smtpmail" process-buffer host port
		   :type smtpmail-stream-type
		   :return-list t
		   :warn-unless-encrypted ask-for-password
		   :capability-command (format "EHLO %s\r\n" (smtpmail-fqdn))
		   :end-of-command "^[0-9]+ .*\r\n"
		   :success "^2.*\n"
		   :always-query-capabilities t
		   :starttls-function
		   (lambda (capabilities)
		     (and (string-match "[ -]STARTTLS" capabilities)
			  "STARTTLS\r\n"))
		   :client-certificate t
		   :use-starttls-if-possible t)))

	  ;; If we couldn't access the server at all, we give up.
	  (unless (setq process (car result))
	    (throw 'done (if (plist-get (cdr result) :error)
			     (plist-get (cdr result) :error)
			   "Unable to contact server")))

	  ;; set the send-filter
	  (set-process-filter process #'smtpmail-process-filter)

	  (let* ((greeting (plist-get (cdr result) :greeting))
		 (code (smtpmail-response-code greeting)))
	    (unless code
	      (throw 'done (format "No greeting: %s" greeting)))
	    (when (>= code 400)
	      (throw 'done (format "Connection not allowed: %s" greeting))))

	  (with-current-buffer process-buffer
            (set-process-coding-system process 'raw-text-unix 'raw-text-unix)
	    (setq-local smtpmail-read-point (point-min))

	    (let* ((capabilities (plist-get (cdr result) :capabilities))
		   (code (smtpmail-response-code capabilities)))
	      (if (or (null code)
		      (>= code 400))
		  ;; The server didn't accept EHLO, so we fall back on HELO.
		  (smtpmail-command-or-throw
		   process (format "HELO %s" (smtpmail-fqdn)))
		;; EHLO was successful, so we parse the extensions.
		(dolist (line (delete "" (split-string capabilities "\r\n")))
		  (let ((name
                         ;; Use ASCII case-table to prevent I
                         ;; downcasing to a dotless i under some
                         ;; language environments.  See
                         ;; https://lists.gnu.org/archive/html/emacs-devel/2007-03/msg01760.html.
			 (with-case-table ascii-case-table
			   (mapcar (lambda (s) (intern (downcase s)))
				   (split-string (substring line 4) "[ ]")))))
		    (when (= (length name) 1)
		      (setq name (car name)))
		    (when name
		      (cond ((memq (if (consp name) (car name) name)
				   '(verb xvrb 8bitmime onex xone
					  expn size dsn etrn
					  enhancedstatuscodes
					  help xusr
					  auth=login auth starttls))
			     (setq supported-extensions
				   (cons name supported-extensions)))
			    (smtpmail-warn-about-unknown-extensions
			     (message "Unknown extension %s" name))))))))

	    (setq auth-mechanisms
		  (smtpmail-try-auth-methods
		   process supported-extensions host port
		   ask-for-password))

	    (when (or (member 'onex supported-extensions)
		      (member 'xone supported-extensions))
	      (smtpmail-command-or-throw process "ONEX"))

	    (when (and smtpmail-debug-verb
		       (or (member 'verb supported-extensions)
			   (member 'xvrb supported-extensions)))
	      (smtpmail-command-or-throw process "VERB"))

	    (when (member 'xusr supported-extensions)
	      (smtpmail-command-or-throw process "XUSR"))

	    ;; MAIL FROM:<sender>
	    (let ((size-part
		   (if (or (member 'size supported-extensions)
			   (assoc 'size supported-extensions))
		       (format " SIZE=%d"
			       (with-current-buffer smtpmail-text-buffer
				 ;; size estimate:
				 (+ (- (point-max) (point-min))
				    ;; Add one byte for each change-of-line
				    ;; because of CR-LF representation:
				    (count-lines (point-min) (point-max)))))
		     ""))
		  (body-part
		   (if (member '8bitmime supported-extensions)
		       ;; FIXME:
		       ;; Code should be added here that transforms
		       ;; the contents of the message buffer into
		       ;; something the receiving SMTP can handle.
		       ;; For a receiver that supports 8BITMIME, this
		       ;; may mean converting BINARY to BASE64, or
		       ;; adding Content-Transfer-Encoding and the
		       ;; other MIME headers.  The code should also
		       ;; return an indication of what encoding the
		       ;; message buffer is now, i.e. ASCII or
		       ;; 8BITMIME.
		       (if nil
			   " BODY=8BITMIME"
			 "")
		     "")))
	      (smtpmail-send-command
	       process (format "MAIL FROM:<%s>%s%s"
			       envelope-from size-part body-part))
	      (cond
	       ((smtpmail-ok-p (setq result (smtpmail-read-response process)))
		;; Success.
		)
               ((and (numberp (car result))
                     (<= 400 (car result) 499)
                     (< send-attempts smtpmail-retries))
                (message "Got transient error code %s when sending; retrying attempt %d..."
                         (car result) send-attempts)
                ;; Retry on getting a transient 4xx code; see
                ;; https://tools.ietf.org/html/rfc5321#section-4.2.1
                (ignore-errors
		  (smtpmail-send-command process "QUIT")
		  (smtpmail-read-response process))
		(delete-process process)
                (sleep-for 1)
		(setq process nil)
		(throw 'done
		       (smtpmail-via-smtp recipient smtpmail-text-buffer
                                          ask-for-password
                                          (1+ send-attempts))))
	       ((and auth-mechanisms
		     (not ask-for-password)
		     (eq (car result) 530))
		;; We got a "530 auth required", so we close and try
		;; again, this time asking the user for a password.
		;; We ignore any errors here, because some MTAs just
		;; close the connection immediately after giving the
		;; error message.
		(ignore-errors
		  (smtpmail-send-command process "QUIT")
		  (smtpmail-read-response process))
		(delete-process process)
		(setq process nil)
		(throw 'done
		       (smtpmail-via-smtp recipient smtpmail-text-buffer t)))
	       (t
		;; Return the error code.
		(throw 'done
		       (smtpmail-response-text result)))))

	    ;; RCPT TO:<recipient>
	    (let ((n 0))
	      (while (not (null (nth n recipient)))
		(smtpmail-send-command
		 process (format "RCPT TO:<%s>"
				 (smtpmail-maybe-append-domain
				  (nth n recipient))))
		(cond
		 ((smtpmail-ok-p (setq result (smtpmail-read-response process)))
		  ;; Success.
		  nil)
		 ((and auth-mechanisms
		       (not ask-for-password)
		       (integerp (car result))
		       (>= (car result) 550)
		       (<= (car result) 554))
		  ;; We got a "550 relay not permitted" (or the like),
		  ;; and the server accepts credentials, so we try
		  ;; again, but ask for a password first.
		  (smtpmail-send-command process "QUIT")
		  (smtpmail-read-response process)
		  (delete-process process)
		  (setq process nil)
		  (throw 'done
			 (smtpmail-via-smtp recipient smtpmail-text-buffer t)))
		 (t
		  ;; Return the error code.
		  (throw 'done
			 (smtpmail-response-text result))))
		(setq n (1+ n))))

	    ;; Send the contents.
	    (smtpmail-command-or-throw process "DATA")
	    (smtpmail-send-data process smtpmail-text-buffer)
	    ;; Return success.
	    nil))
      (when (and process
		 (buffer-live-p process-buffer))
	(with-current-buffer (process-buffer process)
	  (smtpmail-send-command process "QUIT")
	  (smtpmail-read-response process)
	  (delete-process process)
	  (unless smtpmail-debug-info
	    (kill-buffer process-buffer)))))))


(defun smtpmail-process-filter (process output)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output)
    (set-marker (process-mark process) (point))))

(defun smtpmail-read-response (process)
  (let ((case-fold-search nil)
	(response-strings nil)
	(response-continue t)
	(return-value '(nil ()))
	match-end)
    (catch 'done
      (while response-continue
	(goto-char smtpmail-read-point)
	(while (not (search-forward "\r\n" nil t))
	  (unless (memq (process-status process) '(open run))
	    (throw 'done nil))
	  (accept-process-output process)
	  (goto-char smtpmail-read-point))

	(setq match-end (point))
	(setq response-strings
	      (cons (buffer-substring smtpmail-read-point (- match-end 2))
		    response-strings))

	(goto-char smtpmail-read-point)
	(if (looking-at "[0-9]+ ")
	    (let ((begin (match-beginning 0))
		  (end (match-end 0)))
	      (if smtpmail-debug-info
		  (message "%s" (car response-strings)))

	      (setq smtpmail-read-point match-end)

	      ;; ignore lines that start with "0"
	      (if (looking-at "0[0-9]+ ")
		  nil
		(setq response-continue nil)
		(setq return-value
		      (cons (string-to-number
			     (buffer-substring begin end))
			    (nreverse response-strings)))))

	  (if (looking-at "[0-9]+-")
	      (progn (if smtpmail-debug-info
			 (message "%s" (car response-strings)))
		     (setq smtpmail-read-point match-end)
		     (setq response-continue t))
	    (progn
	      (setq smtpmail-read-point match-end)
	      (setq response-continue nil)
	      (setq return-value
		    (cons nil (nreverse response-strings)))))))
      (setq smtpmail-read-point match-end))
    return-value))


(defun smtpmail-send-command (process command)
  (goto-char (point-max))
  (if (string-match "\\`AUTH [A-Z]+ " command)
      (insert (match-string 0 command) "<omitted>\r\n")
    (insert command "\r\n"))
  (setq smtpmail-read-point (point))
  (process-send-string process (concat command "\r\n")))

(defun smtpmail-send-data-1 (process data)
  (goto-char (point-max))

  (if (and (multibyte-string-p data)
	   smtpmail-code-conv-from)
      (setq data (encode-coding-string data smtpmail-code-conv-from)))

  (if smtpmail-debug-info
      (insert data "\r\n"))

  (setq smtpmail-read-point (point))
  ;; Escape "." at start of a line
  (if (eq (string-to-char data) ?.)
      (process-send-string process "."))
  (process-send-string process data)
  (process-send-string process "\r\n"))

(defun smtpmail-send-data (process buffer)
  (let ((data-continue t)
        (pr (with-current-buffer buffer
              (make-progress-reporter "Sending email "
                                      (point-min) (point-max))))
        sending-data)
    (with-current-buffer buffer
      (goto-char (point-min)))
    (while data-continue
      (with-current-buffer buffer
        (progress-reporter-update pr (point))
        (setq sending-data (buffer-substring-no-properties (line-beginning-position)
                                                           (line-end-position)))
	(end-of-line 2)
        (setq data-continue (not (eobp))))
      (smtpmail-send-data-1 process sending-data))
    ;; DATA end "."
    (smtpmail-command-or-throw process ".")
    (progress-reporter-done pr)))

(defun smtpmail-deduce-address-list (smtpmail-text-buffer header-start header-end)
  "Get address list suitable for smtp RCPT TO: <address>."
  (with-temp-buffer
    (let ((case-fold-search t)
          (simple-address-list "")
          this-line
          this-line-end
          addr-regexp)
      (insert-buffer-substring smtpmail-text-buffer header-start header-end)
      (goto-char (point-min))
      ;; RESENT-* fields should stop processing of regular fields.
      (save-excursion
	(setq addr-regexp
	      (if (re-search-forward "^Resent-\\(To\\|Cc\\|Bcc\\):"
				     header-end t)
		  "^Resent-\\(To\\|Cc\\|Bcc\\):"
		"^\\(To:\\|Cc:\\|Bcc:\\)")))

      (while (re-search-forward addr-regexp header-end t)
	(replace-match "")
	(setq this-line (match-beginning 0))
	(forward-line 1)
	;; get any continuation lines
	(while (and (looking-at "^[ \t]+") (< (point) header-end))
	  (forward-line 1))
	(setq this-line-end (point-marker))
	(setq simple-address-list
	      (concat simple-address-list " "
		      (mail-strip-quoted-names (buffer-substring this-line this-line-end)))))
      (erase-buffer)
      (insert " " simple-address-list "\n")
      (subst-char-in-region (point-min) (point-max) 10 ?  t) ; newline --> blank
      (subst-char-in-region (point-min) (point-max) ?, ?  t) ; comma   --> blank
      (subst-char-in-region (point-min) (point-max)  9 ?  t) ; tab     --> blank

      (goto-char (point-min))
      ;; tidiness in case hook is not robust when it looks at this
      (while (re-search-forward "[ \t]+" header-end t) (replace-match " "))

      (goto-char (point-min))
      (let (recipient-address-list)
	(while (re-search-forward " \\([^ ]+\\) " (point-max) t)
	  (backward-char 1)
	  (setq recipient-address-list (cons (buffer-substring (match-beginning 1) (match-end 1))
					     recipient-address-list)))
        recipient-address-list))))

(defun smtpmail-do-bcc (header-end)
  "Delete [Resent-]Bcc: and their continuation lines from the header area.
There may be multiple Bcc: lines, and each may have arbitrarily
many continuation lines."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      ;; iterate over all Bcc: lines
      (while (re-search-forward "^\\(RESENT-\\)?Bcc:" header-end t)
	(delete-region (match-beginning 0)
		       (progn (forward-line 1) (point)))
	;; get rid of any continuation lines
	(while (and (looking-at "^[ \t].*\n") (< (point) header-end))
	  (replace-match ""))))))

;; Obsolete.

(defun smtpmail-intersection (list1 list2)
  (declare (obsolete seq-intersection "28.1"))
  (seq-intersection list2 list1 #'eq))

(provide 'smtpmail)

;;; smtpmail.el ends here
