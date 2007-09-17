;; Module for GDBM / CLISP
;; <http://www.gnu.org/gdbm/>
;; Masayuki Onjo 2007

(defpackage "GDBM"
  (:documentation
   "GDBM - The GNU database manager - <http://www.gnu.org/software/gdbm/>")
  (:use "LISP")
  (:export "GDBM"
           "GDBM-OPEN" "GDBM-CLOSE"
           "GDBM-STORE" "GDBM-FETCH" "GDBM-DELETE" "GDBM-EXISTS"
           "GDBM-FIRSTKEY" "GDBM-NEXTKEY"
           "GDBM-REORGANIZE" "GDBM-SYNC"  "GDBM-SETOPT"))
(in-package "GDBM")

(pushnew :gdbm *features*)
(pushnew "GDBM" custom:*system-package-list* :test #'string=)
(setf (documentation (find-package "GDBM") 'sys::impnotes) "gdbm")

(defstruct (gdbm (:constructor make-gdbm (dbf)))
  (dbf nil))

(define-condition gdbm-error (simple-error)
  ((message :reader gdbm-error-message :initarg :message))
  (:report (lambda (condition stream)
	     (format stream "~A."
		     (gdbm-error-message condition)))))