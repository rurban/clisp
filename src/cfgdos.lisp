#0Y UTF-8 ;;;  This file is Unicode/UTF-8 encoded.  -*- coding: utf-8 -*-

;;; Site specific definitions, to be modified on installation

(in-package "EXT")
(mapcar #'fmakunbound '(machine-type machine-version machine-instance
                        short-site-name long-site-name
                        editor-name editor-tempfile edit-file))

(defun machine-type () "PC/486")
(defun machine-version () "486/33")
(defun machine-instance () (or (getenv "HOSTNAME") "edit config.lisp"))

(defun short-site-name () (or (getenv "ORGANIZATION") "edit config.lisp"))
(defun long-site-name () (or (getenv "ORGANIZATION") "edit config.lisp"))

(defparameter *editor* "C:\\UTIL\\PRODIT.EXE"
  "The name of the editor.")
(defun editor-name () (or (getenv "EDITOR") *editor*))

(defun editor-tempfile ()
  "The temporary file LISP creates for editing."
  "lisptemp.lisp")

(defun edit-file (file)
  "(edit-file file) edits a file."
  ;; The function EXECUTE apparently crashes on batch files. Work around.
  (let ((editor (editor-name))
        (filename (namestring file t)))
    (if (string-equal (pathname-type editor) "cmd")
      (shell (format nil "~A ~A" editor filename))
      (execute editor filename))))

;; Treat Ctrl-Z in files as whitespace. Some losing middle-age
;; editors insist on appending this to files.
(eval-when (load eval compile)
  (set-syntax-from-char #\Code26 #\Space))

(defparameter *load-paths*
  '(#"C:"                ; erst im Current-Directory von Laufwerk C:
    #"C:\\CLISP\\...\\") ; dann in allen Directories unterhalb C:\CLISP
  "The list of directories where programs are searched on LOAD etc.
if device and directory are unspecified.")

;; This makes screen output prettier:
(setq *print-pretty* t)

;; This perhaps makes pathname parsing more intuitive:
;;  ".clisprc" --> #S(pathname :name ".clisprc" :type nil)
(setq *parse-namestring-dot-file* :name)

;; Also set the variable *default-time-zone* in TIMEZONE.LISP according
;; to your time zone.
;; (setq *default-time-zone* 0)

;; which browser do you use? (see `*browsers*' in clhs.lisp)
;; (setq *browser* :mozilla-remote)

;; Common Lisp HyperSpec access
(defvar *clhs-root-default*)
(defun clhs-root ()
  "This returns the root URL for the Common Lisp HyperSpec.
You can set the environment variable `CLHSROOT' or redefine this function
in ~/.clisprc.  On win32 you can also use the Registry."
  (or (getenv "CLHSROOT") *clhs-root-default*))
(setq *clhs-root-default* "http://www.lisp.org/HyperSpec/")
