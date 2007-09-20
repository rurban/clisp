;; Module for GDBM / CLISP
;; <http://www.gnu.org/gdbm/>
;; Masayuki Onjo 2007

(defpackage #:gdbm
  (:documentation
   "GDBM - The GNU database manager - <http://www.gnu.org/software/gdbm/>")
  (:use #:lisp)
  (:export #:gdbm #:gdbm-version
           #:gdbm-open #:gdbm-close #:do-db #:with-db
           #:gdbm-store #:gdbm-fetch #:gdbm-delete #:gdbm-exists
           #:gdbm-firstkey #:gdbm-nextkey
           #:gdbm-reorganize #:gdbm-sync #:gdbm-setopt))
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

(defmacro do-db ((key-var gdbm) &body body)
  "Iterate over the GDBM keys in LOOP."
  (let ((db (gensym "DO-DB")))
    `(loop :with ,db = ,gdbm
       :for ,key-var = (gdbm:gdbm-firstkey ,db)
       :then (gdbm:gdbm-nextkey ,db ,key-var)
       :while ,key-var ,@body)))
