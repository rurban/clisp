;;; PostgreSQL demo
;;; Based on the examples distributed with PostgreSQL (man libpq)
;;;
;;; Copyright (C) 1999-2003 by Sam Steingold
;;; Distributed under the GNU GPL2 <http://www.gnu.org/copyleft/gpl.html>:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.

;; for your cut&paste convenience:
;; (load "postgresql/sql.lisp")
;; (sql-demo::sql-test-1)

(require "postgresql")

(defpackage "SQL-DEMO"
  (:use "CL" "EXT" "FFI" "SQL"))

(in-package "SQL-DEMO")

;;;
;;; Helper Functions
;;;

(defvar *sql-log* *standard-output*)

(define-condition sql-error (error)
  ((type :type symbol :reader sql-type :initarg :type)
   (mesg :type simple-string :reader sql-mesg :initarg :mesg))
  (:report (lambda (cc stream)
             (format stream "[~a] ~a" (sql-type cc) (sql-mesg cc)))))

(defun sql-error (conn res &rest mesgs)
  ;; if you do `PQfinish' twice on the same object, you will get segfault!
  (when conn (sql::PQfinish conn))
  ;; if you do `PQclear' twice on the same object, you will get segfault!
  (when res (sql::PQclear res))
  (error 'sql-error :mesg (apply #'concatenate 'string mesgs)
                    :type (if res :request :connection)))

(defun sql-connect (host port opts tty name login passwd)
  (let ((conn (sql::PQsetdbLogin host port opts tty name login passwd)))
    (if (= (sql::PQstatus conn) sql::CONNECTION_OK)
        (format *sql-log* "~&Connection OK:~% db name: `~a'
 host:port[tty]: ~a:~a[~a]~% options: `~a'~%"
                (sql::PQdb conn) (sql::PQhost conn) (sql::PQport conn)
                (sql::PQtty conn) (sql::PQoptions conn))
        (sql-error conn nil "PQconnectdb/template1: "
                   (sql::PQerrorMessage conn)))
    conn))

(defmacro with-sql-connection ((conn host port opts tty name login passwd)
                               &body body)
  `(let ((,conn (sql-connect ,host ,port ,opts ,tty ,name ,login ,passwd)))
    (unwind-protect (progn ,@body)
      ;; close the connection to the database and cleanup
      (when ,conn (sql::PQfinish ,conn)))))

(defun sql-transaction (conn command status &optional clear-p)
  (let ((res (sql::PQexec conn command)))
    (unless (= status (sql::PQresultStatus res))
      (sql-error conn res command ": " (sql::PQresultErrorMessage res)))
    (when clear-p (sql::PQclear res))
    (format *sql-log* " * OK: ~a~%" command)
    res))

(defmacro with-sql-transaction ((res conn command status) &body body)
  `(let ((,res (sql-transaction ,conn ,command ,status)))
    (unwind-protect (progn ,@body)
      ;; avoid memory leaks
      (when ,res (sql::PQclear ,res)))))

;;;
;;; Simple Test
;;;

(defun sql-test-1 ()
  (with-sql-connection (conn nil nil nil nil "template1" nil nil)
    (sql-transaction conn "BEGIN" sql::PGRES_COMMAND_OK t)
    ;; fetch instances from the pg_database, the system catalog of databases
    (sql-transaction conn
                     "DECLARE mycursor CURSOR FOR select * from pg_database"
                     sql::PGRES_COMMAND_OK t)

    ;; FETCH ALL
    (with-sql-transaction (res conn "FETCH ALL in mycursor"
                               sql::PGRES_TUPLES_OK)
      (let ((nfields (sql::PQnfields res))
            (ntuples (sql::PQntuples res)))
        (format t " + ~d fields; ~d ntuples~%" nfields ntuples)

        ;; first, print out the attribute names
        (dotimes (ii nfields (format t "~2%"))
          (format t "~15s" (sql::PQfname res ii)))

        ;; next, print out the instances
        (dotimes (ii (sql::PQntuples res))
          (dotimes (jj nfields (terpri))
            (format t "~15s" (sql::PQgetvalue res ii jj))))))

    ;; close the cursor
    (sql-transaction conn "CLOSE mycursor" sql::PGRES_COMMAND_OK t)

    ;; commit the transaction
    (sql-transaction conn "COMMIT" sql::PGRES_COMMAND_OK t)))

;;;
;;; asynchronous notification interface
;;;
;;; populate a database with the following:
;;; CREATE TABLE TBL1 (i int4);
;;; CREATE TABLE TBL2 (i int4);
;;; CREATE RULE r1 AS ON INSERT TO TBL1 DO [INSERT INTO TBL2 values (new.i); NOTIFY TBL2];
;;;
;;; *** psql barfs on this:
;;; *** ERROR:  parser: parse error at or near ""
;;; *** ERROR:  parser: parse error at or near "]"
;;;
;;;  Then start up this program
;;;  After the program has begun, do
;;; INSERT INTO TBL1 values (10);

(defun sql-test-2 ()
  (with-sql-connection (conn nil nil nil nil (sys::getenv "USER") nil nil)
    (sql-transaction conn "LISTEN TBL2" sql::PGRES_COMMAND_OK t)

    (loop (sql::PQconsumeInput conn)
          (loop :for notify = (sql::PQnotifies conn)
                :while (ffi:validp notify) :do
                ;; unfortunately, (FFI:VALIDP #<FOREIGN-ADDRESS #x00000000>)
                ;; ==> T, so this won't work!
                ;;(lisp:finalize notify ; will `notify' be GCed?! YES!!!
                ;;               (lambda (obj)
                ;;                 (format t "~s is being collected~%" obj)))
                (format t "ASYNC NOTIFY: ~a~%" notify)
                (break))
          (sleep 1))))

;;;
;;; test the binary cursor interface
;;;
;;; *** this is not supported by CLISP at the moment:
;;; *** need to include geo_decls.h
;;;
;;; populate a database by doing the following:
;;;
;;;       CREATE TABLE test1 (i int4, d float4, p polygon);
;;;
;;;       INSERT INTO test1 values (1, 3.567, '(3.0, 4.0, 1.0, 2.0)'::polygon);
;;;
;;;       INSERT INTO test1 values (2, 89.05, '(4.0, 3.0, 2.0, 1.0)'::polygon);
;;;
;;;        the expected output is:
;;;
;;;       tuple 0: got
;;;        i = (4 bytes) 1,
;;;        d = (4 bytes) 3.567000,
;;;        p = (4 bytes) 2 points         boundbox = (hi=3.000000/4.000000, lo = 1.000000,2.000000)
;;;       tuple 1: got
;;;        i = (4 bytes) 2,
;;;        d = (4 bytes) 89.050003,
;;;        p = (4 bytes) 2 points         boundbox = (hi=4.000000/3.000000, lo = 2.000000,1.000000)
;;;

(defun sql-test-3 ()
  (with-sql-connection (conn nil nil nil nil (sys::getenv "USER") nil nil)
    (sql-transaction conn "BEGIN" sql::PGRES_COMMAND_OK t)

    (sql-transaction conn
                     "DECLARE mycursor BINARY CURSOR FOR select * from test1"
                     sql::PGRES_COMMAND_OK t)

    (with-sql-transaction (res conn "FETCH ALL in mycursor"
                               sql::PGRES_TUPLES_OK)
      (let ((i-fnum (sql::PQfnumber res "i"))
            (d-fnum (sql::PQfnumber res "d"))
            (p-fnum (sql::PQfnumber res "p"))
            (nfields (sql::PQnfields res))
            (ntuples (sql::PQntuples res)))
        (format t " + ~d fields; ~d ntuples; i: ~d; d: ~d; p: ~d~%"
                nfields ntuples i-fnum d-fnum p-fnum)
        (dotimes (ii 3)
          (format t "type[~d] = ~d, size[~d] = ~d~%"
                  ii (sql::PQftype res ii) ii (sql::PQfsize res ii)))
        (dotimes (ii ntuples)
          (let ((plen (sql::PQgetlength res ii p-fnum))
                (ival (sql::PQgetvalue res ii i-fnum))
                (dval (sql::PQgetvalue res ii d-fnum)))
            (format t " ++ plen: ~d; ival: ~d; dval: ~f~%" plen ival dval)))))

    (sql-transaction conn "CLOSE mycursor" sql::PGRES_COMMAND_OK t)
    (sql-transaction conn "COMMIT" sql::PGRES_COMMAND_OK t)))

;;; file sql.lisp ends here
