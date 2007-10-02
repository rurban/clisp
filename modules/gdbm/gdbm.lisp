;; Module for GDBM / CLISP
;; <http://www.gnu.org/gdbm/>
;; (C) 2007 Masayuki Onjo
;; Released under GNU GPL2

(defpackage #:gdbm
  (:documentation
   "GDBM - The GNU database manager - <http://www.gnu.org/software/gdbm/>")
  (:use #:lisp)
  (:export #:gdbm #:gdbm-version
           #:gdbm-open #:gdbm-close #:do-db #:with-open-db
           #:gdbm-store #:gdbm-fetch #:gdbm-delete #:gdbm-exists
           #:gdbm-firstkey #:gdbm-nextkey #:gdbm-file-size
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
	     (princ (gdbm-error-message condition) stream))))

(defmacro do-db ((key-var gdbm &rest options) &body body)
  "Iterate over the GDBM keys in LOOP."
  (let ((db (gensym "DO-DB")))
    `(loop :with ,db = ,gdbm
       :for ,key-var = (gdbm:gdbm-firstkey ,db ,@options)
       :then (gdbm:gdbm-nextkey ,db ,key-var ,@options)
       :while ,key-var ,@body)))

(defmacro with-open-db ((db filename &rest options) &body body)
  "Open a GDBM database, execute BODY, ensure that the DB is closed."
  (multiple-value-bind (body-rest declarations) (system::parse-body body)
    `(let ((,db (gdbm-open ,filename ,@options)))
       (declare (read-only ,db) ,@declarations)
       (unwind-protect (multiple-value-prog1 (progn ,@body-rest)
                         (when ,db (gdbm-close ,db)))
         (when ,db (gdbm-close ,db))))))
