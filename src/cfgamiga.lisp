#0Y UTF-8 ;;;  This file is Unicode/UTF-8 encoded.  -*- coding: utf-8 -*-

;;; Site specific definitions, to be modified on installation

(in-package "EXT")
(mapcar #'fmakunbound '(machine-type machine-version machine-instance
                        short-site-name long-site-name))

(defun machine-type () "Amiga")
(defun machine-version () "Amiga, OS 2.04-3.1")
(defun machine-instance () (or (sys::getenv "HOSTNAME") "edit config.lisp"))

(defun short-site-name () (or (sys::getenv "ORGANIZATION") "edit config.lisp"))

(defun long-site-name () (or (sys::getenv "ORGANIZATION") "edit config.lisp"))

(defparameter *editor* "emacs" "The name of the editor.")
(defun editor-name () (or (sys::getenv "EDITOR") *editor*))

(defun editor-tempfile ()
  "The temporary file LISP creates for editing."
  "T:lisptemp.lisp")

(defparameter *load-paths*
  '(#"**/"       ; erst in allen Directories unterhalb von hier
    #"LISP:**/") ; dann in allen Directories unterhalb von LISP:
  "The list of directories where programs are searched on LOAD etc.
if device and directory are unspecified.")

;; This makes screen output both faster and prettier:
(setq *print-pretty* t)

;; This perhaps makes pathname parsing more intuitive:
;;  ".clisprc" --> #S(pathname :name ".clisprc" :type nil)
(setq *parse-namestring-dot-file* :name)

;; Also set the variable *default-time-zone* in TIMEZONE.LISP according
;; to your time zone.
;; (setq *default-time-zone* 0)

;; Common Lisp HyperSpec access
(defvar *clhs-root-default*)
(defun clhs-root ()
  "This returns the root URL for the Common Lisp HyperSpec.
You can set the environment variable `CLHSROOT' or redefine this function
in ~/.clisprc.  On win32 you can also use the Registry."
  (or (sys::getenv "CLHSROOT") *clhs-root-default*))
(setq *clhs-root-default* "http://www.lisp.org/HyperSpec/")
