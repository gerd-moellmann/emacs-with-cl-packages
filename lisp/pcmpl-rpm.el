;;; pcmpl-rpm.el --- functions for dealing with rpm completions  -*- lexical-binding: t -*-

;; Copyright (C) 1999-2025 Free Software Foundation, Inc.

;; Package: pcomplete

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

;; These functions provide completion rules for the `rpm' command and
;; related tools.

;;; Code:

(require 'pcomplete)

(defgroup pcmpl-rpm nil
  "Options for rpm completion."
  :group 'pcomplete
  :prefix "pcmpl-rpm-")

;; rpm -qa can be slow.  Adding --nodigest --nosignature is MUCH faster.
(defcustom pcmpl-rpm-query-options
  (let (opts)
    (with-temp-buffer
      (when (ignore-errors (call-process "rpm" nil t nil "--help"))
        (if (search-backward "--nodigest " nil 'move)
            (setq opts '("--nodigest")))
        (goto-char (point-min))
        (if (search-forward "--nosignature " nil t)
            (push "--nosignature" opts))))
    opts)
  "String, or list of strings, with extra options for an rpm query command."
  :version "24.3"
  :type '(choice (const :tag "No options" nil)
                 (string :tag "Single option")
                 (repeat :tag "List of options" string)))

(defcustom pcmpl-rpm-cache t
  "Whether to cache the list of installed packages."
  :version "24.3"
  :type 'boolean)

(defconst pcmpl-rpm-cache-stamp-file "/var/lib/rpm/Packages"
  "File used to check that the list of installed packages is up-to-date.")

(defvar pcmpl-rpm-cache-time nil
  "Time at which the list of installed packages was updated.")

(defvar pcmpl-rpm-packages nil
  "List of installed packages.")

;; Functions:

(defun pcmpl-rpm-packages ()
  "Return a list of all installed rpm packages."
  (if (and pcmpl-rpm-cache
           pcmpl-rpm-cache-time
           (let ((mtime (file-attribute-modification-time
                         (file-attributes pcmpl-rpm-cache-stamp-file))))
             (and mtime (not (time-less-p pcmpl-rpm-cache-time mtime)))))
      pcmpl-rpm-packages
    (message "Getting list of installed rpms...")
    (setq pcmpl-rpm-cache-time (current-time)
          pcmpl-rpm-packages
          (split-string (apply #'pcomplete-process-result "rpm"
                               (append '("-q" "-a")
                                       (if (stringp pcmpl-rpm-query-options)
                                           (list pcmpl-rpm-query-options)
                                         pcmpl-rpm-query-options)))))
    (message "Getting list of installed rpms...done")
    pcmpl-rpm-packages))

;; Should this use pcmpl-rpm-query-options?
;; I don't think it would speed it up at all (?).
(defun pcmpl-rpm-all-query (flag)
  (message "Querying all packages with `%s'..." flag)
  (let ((pkgs (pcmpl-rpm-packages))
	(provs (list t)))
    (while pkgs
      (nconc provs (split-string
		    (pcomplete-process-result
		     "rpm" "-q" (car pkgs) flag)))
      (setq pkgs (cdr pkgs)))
    (pcomplete-uniquify-list (cdr provs))))

(defsubst pcmpl-rpm-files ()
  (pcomplete-dirs-or-entries "\\.rpm\\'"))

;;;###autoload
(defun pcomplete/rpm ()
  "Completion for the `rpm' command."
  ;; Originally taken from the output of `rpm --help' on a Red Hat 6.1 system.
  (let (mode)
    (while (<= pcomplete-index pcomplete-last)
      (unless mode
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (pcomplete-here*
	     '("--addsign"
	       "--checksig"
	       "--erase"
	       "--help"
	       "--initdb"
	       "--install"
	       "--pipe"
	       "--querytags"
	       "--rebuild"
	       "--rebuilddb"
	       "--recompile"
	       "--resign"
	       "--rmsource"
	       "--setperms"
	       "--setugids"
	       "--upgrade"
	       "--verify"
	       "--version"))
	  (pcomplete-opt "vqVyiUebtK")))
;     -b<stage> <spec>
;     -t<stage> <tarball>    - build package, where <stage> is one of:
;	  p                - prep (unpack sources and apply patches)
;	  l                - list check (do some cursory checks on %files)
;	  c                - compile (prep and compile)
;	  i                - install (prep, compile, install)
;	  b                - binary package (prep, compile, install, package)
;	  a                - bin/src package (prep, compile, install, package)
      (cond
       ((or (eq mode 'query)
	    (pcomplete-match "-[^-]*q"))
	(setq mode 'query)
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--changelog"
		 "--dbpath"
		 "--dump"
		 "--file"
		 "--ftpport"            ;nyi for the next four
		 "--ftpproxy"
		 "--httpport"
		 "--httpproxy"
		 "--provides"
		 "--queryformat"
		 "--rcfile"
		 "--requires"
		 "--root"
		 "--scripts"
		 "--triggeredby"
		 "--whatprovides"
		 "--whatrequires"))
	      (cond
	       ((pcomplete-test "--dbpath")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--queryformat")
		(pcomplete-here*))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--file")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--root")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--scripts")
		(if (pcomplete-match "^--\\(.*\\)" 0)
		    (pcomplete-here* '("--triggers"))))
	       ((pcomplete-test "--triggeredby")
		(pcomplete-here* (pcmpl-rpm-packages)))
	       ((pcomplete-test "--whatprovides")
		(pcomplete-here*
		 (pcmpl-rpm-all-query "--provides")))
	       ((pcomplete-test "--whatrequires")
		(pcomplete-here*
		 (pcmpl-rpm-all-query "--requires")))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "af.p(pcmpl-rpm-files)ilsdcvR")
	    (if (pcomplete-test "-[^-]*p" 'first 1)
		(pcomplete-here (pcmpl-rpm-files))
              (if (pcomplete-test "-[^-]*f" 'first 1)
                  (pcomplete-here* (pcomplete-entries))
                (pcomplete-here (pcmpl-rpm-packages)))))))
       ((pcomplete-test "--pipe")
	(pcomplete-here* (funcall pcomplete-command-completion-function)))
       ((pcomplete-test "--rmsource")
	(pcomplete-here* (pcomplete-entries))
	(throw 'pcomplete-completions nil))
       ((pcomplete-match "\\`--re\\(build\\|compile\\)\\'")
	(pcomplete-here (pcmpl-rpm-files))
	(throw 'pcomplete-completions nil))
       ((pcomplete-match "\\`--\\(resign\\|addsign\\)\\'")
	(while (pcomplete-here (pcmpl-rpm-files))))
       ((or (eq mode 'checksig)
	    (pcomplete-test "--checksig"))
	(setq mode 'checksig)
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--nopgp"
		 "--nogpg"
		 "--nomd5"
		 "--rcfile"))
	      (cond
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "v")
	    (pcomplete-here (pcmpl-rpm-files)))))
       ((or (eq mode 'rebuilddb)
	    (pcomplete-test "--rebuilddb"))
	(setq mode 'rebuilddb)
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--dbpath"
		 "--root"
		 "--rcfile"))
	      (cond
	       ((pcomplete-test "--dbpath")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--root")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "v")
	    (pcomplete-here))))
       ((memq mode '(install upgrade))
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       (append
		'("--allfiles"
		  "--badreloc"
		  "--dbpath"
		  "--excludedocs"
		  "--excludepath"
		  "--force"
		  "--hash"
		  "--ignorearch"
		  "--ignoreos"
		  "--ignoresize"
		  "--includedocs"
		  "--justdb"
		  "--nodeps"
		  "--noorder"
		  "--noscripts"
		  "--notriggers")
		(if (eq mode 'upgrade)
		    '("--oldpackage"))
		'("--percent"
		  "--prefix"
		  "--rcfile"
		  "--relocate"
		  "--replacefiles"
		  "--replacepkgs"
		  "--root")))
	      (cond
	       ((pcomplete-test "--dbpath")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--relocate")
		(pcomplete-here*))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--excludepath")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--root")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--prefix")
		(pcomplete-here* (pcomplete-dirs)))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "vh")
	    (pcomplete-here (pcmpl-rpm-files)))))
       ((or (pcomplete-test "--install")
	    (pcomplete-match "-[^-]*i"))
	(setq mode 'install))
       ((or (pcomplete-test "--upgrade")
	    (pcomplete-match "-[^-]*U"))
	(setq mode 'upgrade))
       ((or (eq mode 'erase)
	    (pcomplete-test "--erase")
	    (pcomplete-match "-[^-]*e"))
	(setq mode 'erase)
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--allmatches"
		 "--dbpath"
		 "--justdb"
		 "--nodeps"
		 "--noorder"
		 "--noscripts"
		 "--notriggers"
		 "--rcfile"
		 "--root"))
	      (cond
	       ((pcomplete-test "--dbpath")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--root")
		(pcomplete-here* (pcomplete-dirs)))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "v")
	    (pcomplete-here (pcmpl-rpm-packages)))))
       ((or (eq mode 'verify)
	    (pcomplete-test "--verify"))
	(setq mode 'verify)
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--dbpath"
		 "--nodeps"
		 "--nofiles"
		 "--nomd5"
		 "--rcfile"
		 "--root"
		 "--triggeredby"
		 "--whatprovides"
		 "--whatrequires"))
	      (cond
	       ((pcomplete-test "--dbpath")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--root")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--triggeredby")
		(pcomplete-here* (pcmpl-rpm-packages)))
	       ((pcomplete-test "--whatprovides")
		(pcomplete-here*
		 (pcmpl-rpm-all-query "--provides")))
	       ((pcomplete-test "--whatrequires")
		(pcomplete-here*
		 (pcmpl-rpm-all-query "--requires")))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "af.p(pcmpl-rpm-files)v")
	    (pcomplete-here (pcmpl-rpm-packages)))))
       ((or (memq mode '(build test))
	    (pcomplete-match "\\`-[bt]"))
	(setq mode (if (pcomplete-match "\\`-b")
		       'build
		     'test))
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--buildroot"
		 "--clean"
		 "--nobuild"
		 "--rcfile"
		 "--rmsource"
		 "--short-circuit"
		 "--sign"
		 "--target"
		 "--timecheck"))
	      (cond
	       ((pcomplete-test "--buildroot")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--timecheck")
		(pcomplete-here*))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "v")
	    (pcomplete-here
	     (pcomplete-dirs-or-entries (if (eq mode 'test)
                                            "\\.tar\\'"
                                          "\\.spec\\'"))))))
       (t
	(error "You must select a mode: -q, -i, -U, --verify, etc"))))))

;;; DNF

(defun pcmpl-rpm--dnf-packages (status)
  "Return packages matching STATUS.
STATUS should be one of --available or --installed."
  (with-temp-message
      "Getting list of packages..."
    (process-lines "dnf" "--cacheonly" "repoquery" "--queryformat=%{name}\\n"
                   status)))

;;;###autoload
(defun pcomplete/dnf ()
  "Completion for the `dnf' command."
  (let ((subcmds (pcomplete-from-help "dnf --help"
                                      :margin (rx bol (group (* " "))
                                                  (one-or-more (any "a-z" "-")) "  ")
                                      :argument (rx (not "-") (1+ (any "a-z" "-"))))))
    (while (not (member (pcomplete-arg 1) subcmds))
      (pcomplete-here (completion-table-merge
                       subcmds
                       (pcomplete-from-help "dnf --help"))))
    (let ((subcmd (pcomplete-arg 1)))
      (while (pcase subcmd
               ((guard (pcomplete-match "\\`-" 0))
                (if-let* (((pcomplete-match (rx bos "--what" (* (not "=")) "="
                                                (group (* any)) eos)
                                            0))
                          (stub (pcomplete-match-string 1 0)))
                    (pcomplete-here (pcmpl-rpm--dnf-packages "--available") stub)
                  (pcomplete-here
                   (pcomplete-from-help `("dnf" ,subcmd "--help")))))
               ((or "downgrade" "dg" "upgrade" "up" "update" "reinstall" "rei"
                    "remove" "rm")
                (pcomplete-here (pcmpl-rpm--dnf-packages "--installed")))
               ((or "builddep" "changelog" "info" "if" "install" "in" "list" "ls"
                    "mark" "repoquery" "rq" "advisory" "updateinfo")
                (pcomplete-here (pcmpl-rpm--dnf-packages "--available"))))))))

(provide 'pcmpl-rpm)

;;; pcmpl-rpm.el ends here
