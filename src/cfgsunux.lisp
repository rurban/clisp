#0Y UTF-8 ;;;  This file is Unicode/UTF-8 encoded.  -*- coding: utf-8 -*-

;;; Site specific definitions, to be modified on installation

(in-package "EXT")
(mapcar #'fmakunbound '(short-site-name long-site-name))

(defun short-site-name () (or (sys::getenv "ORGANIZATION") "edit config.lisp"))
(defun long-site-name () (or (sys::getenv "ORGANIZATION") "edit config.lisp"))

(defparameter *editor* "vi" "The name of the editor.")
(defun editor-name () (or (sys::getenv "EDITOR") *editor*))

(defun edit-file (file)
  "(edit-file file) edits a file."
  (open file :direction :probe :if-does-not-exist :create)
  (shell
    (format nil "~A ~A"
                (if (sys::getenv "WINDOW_PARENT") ; Suntools active?
                  "textedit"
                  (editor-name))              ; sonst: Default-Editor
                (truename file))))

(defun editor-tempfile ()
  "The temporary file LISP creates for editing."
  (merge-pathnames "lisptemp.lisp" (user-homedir-pathname)))

(defparameter *load-paths*
  '(#"./"           ; in the current directory
    "~/lisp/**/")   ; in all directories below $HOME/lisp
  "The list of directories where programs are searched on LOAD etc.")

;; This makes screen output prettier:
(setq *print-pretty* t)

;; This perhaps makes pathname parsing more intuitive:
;;  ".clisprc" --> #S(pathname :name ".clisprc" :type nil)
(setq *parse-namestring-dot-file* :name)

;; which browser do you use? (see `*browsers*' in clhs.lisp)
;; (setq *browser* :mozilla-remote)

;; Common Lisp HyperSpec access
(defun clhs-root ()
  "This returns the root URL for the Common Lisp HyperSpec.
You can set the environment variable `CLHSROOT' or redefine this function
in ~/.clisprc.  On win32 you can also use the Registry."
  (or (sys::getenv "CLHSROOT") *clhs-root-default*))
