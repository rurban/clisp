#0Y UTF-8 ;;;  This file is Unicode/UTF-8 encoded.  -*- coding: utf-8 -*-

;;; Site specific definitions, to be modified on installation

(in-package "EXT")
(mapcar #'fmakunbound '(machine-type machine-version machine-instance
                        short-site-name long-site-name))

(defun machine-type () "Acorn")
(defun machine-version () "Risc PC, OS 3.7")
(defun machine-instance () (or (getenv "HOSTNAME") "edit config.lisp"))

(defun short-site-name () (or (getenv "ORGANIZATION") "edit config.lisp"))
(defun long-site-name () (or (getenv "ORGANIZATION") "edit config.lisp"))

(defparameter *editor* "filer_run" "The name of the editor.")
(defun editor-name () (or (getenv "EDITOR") *editor*))

(defun edit-file (file)
  "(edit-file file) edits a file."
  (open file :direction :probe :if-does-not-exist :create)
  (let ((pathname (truename file)))
    (shell
      (format nil "~A ~A"
                  (editor-name)
                  (if (pathname-type pathname)
                    ;; swap pathname's name and type
                    (merge-pathnames
                      (make-pathname :name (pathname-type pathname)
                                     :type (pathname-name pathname))
                      pathname)
                    pathname)))))

(defun editor-tempfile ()
  "The temporary file LISP creates for editing."
  ;; We write this instead of "<Wimp$ScrapDir>.lisptemp" in order to
  ;; make sure that all the components of (getenv "Wimp$ScrapDir")
  ;; are treated as directory components.
  (merge-pathnames "lisptemp" "<Wimp$ScrapDir>."))

(defparameter *load-paths* '(#"@.")  ; may add #"@.***." when this will be implemented
  "The list of directories where programs are searched on LOAD etc.")

;; This makes screen output prettier:
(setq *print-pretty* t)

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
