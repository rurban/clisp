;;; PostgreSQL higher level functions and demo
;;; Based on the examples distributed with PostgreSQL (man libpq)
;;;
;;; Copyright (C) 1999-2005 by Sam Steingold
;;; Distributed under the GNU GPL2 <http://www.gnu.org/copyleft/gpl.html>:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.

;; for your cut&paste convenience:
;; (load "postgresql/sql.lisp")
;; (sql::sql-test-1)

(require "postgresql")

(in-package "SQL")

;;;
;;; Helper Functions
;;;

(defvar *sql-log* *standard-output*)

(define-condition sql-error (error)
  ((type :type symbol :reader sql-type :initarg :type)
   (mesg :type simple-string :reader sql-mesg :initarg :mesg))
  (:report (lambda (cc stream)
             (format stream "[~a] ~a" (sql-type cc) (sql-mesg cc)))))

(defun pq-finish (conn)
  "if you do `PQfinish' twice on the same object, you will get segfault!"
  (when (and conn (validp conn))
    (PQfinish conn)
    (setf (validp conn) nil)))

(defun pq-clear (res)
  "if you do `PQclear' twice on the same object, you will get segfault!"
  (when (and res (validp res))
    (PQclear res)
    (setf (validp res) nil)))

(defun sql-error (conn res format-string &rest args)
  (pq-clear res) (pq-finish conn)
  (error 'sql-error :mesg (apply #'format nil format-string args)
         :type (if res :request :connection)))

(defun sql-connect (&key host port options tty name
                    (login "postgres") (password "postgres"))
  (let ((conn (PQsetdbLogin host port options tty name login password)))
    (when conn (set-foreign-pointer conn :copy))
    (unless (and conn (= (PQstatus conn) CONNECTION_OK))
      (sql-error conn nil "~S(~S,~S,~S,~S,~S,~S,~S): ~S"
                 'sql-connect host port options tty name login password
                 (PQerrorMessage conn)))
    (when *sql-log*
      (format *sql-log* "~&Connection(~S) OK:~% db name: ~S
 host:port[tty]: ~S:~S[~S]~% options: ~S~%"
              conn (PQdb conn) (PQhost conn) (PQport conn)
              (PQtty conn) (PQoptions conn)))
    conn))

(defmacro with-sql-connection ((conn &rest options) &body body)
  `(let ((,conn (sql-connect ,@options)))
    (unwind-protect (progn ,@body)
      ;; close the connection to the database and cleanup
      (pq-finish ,conn))))

(defun sql-transaction (conn command status &optional (clear-p t))
  (let ((res (PQexec conn command)))
    (when res (set-foreign-pointer res :copy))
    (unless (and res (= status (PQresultStatus res)))
      (sql-error conn res command "~S(~S,~S): ~S" 'sql-transaction
                 conn command (PQresultErrorMessage res)))
    (when *sql-log*
      (format *sql-log* " * OK: ~a~%" command))
    (when clear-p (pq-clear res))
    res))

(defmacro with-sql-transaction ((res conn command status) &body body)
  `(let ((,res (sql-transaction ,conn ,command ,status nil)))
    (unwind-protect (progn ,@body)
      ;; avoid memory leaks
      (pq-clear ,res))))

(pushnew "SQL" custom:*system-package-list* :test #'string=)

;;; file sql.lisp ends here

;;; tests follow
#+(or) (progn

;;;
;;; Simple Test
;;;

(defun sql-test-1 ()
  (with-sql-connection (conn :name "template1")
    (sql-transaction conn "BEGIN" PGRES_COMMAND_OK)
    ;; fetch instances from the pg_database, the system catalog of databases
    (sql-transaction conn
                     "DECLARE mycursor CURSOR FOR select * from pg_database"
                     PGRES_COMMAND_OK)

    ;; FETCH ALL
    (with-sql-transaction (res conn "FETCH ALL in mycursor" PGRES_TUPLES_OK)
      (let* ((nfields (PQnfields res)) (ntuples (PQntuples res))
             (names (make-array nfields)))
        (format t " + ~D field~:P; ~D ntuple~:P~%" nfields ntuples)

        ;; first, print out the attribute names
        (dotimes (ii nfields)
          (format t "~3:D: ~S~%" ii (setf (aref names ii) (PQfname res ii))))

        ;; next, print out the instances
        (dotimes (ii ntuples)
          (format t "~%<<~D>>~%" ii)
          (dotimes (jj nfields (terpri))
            (format t "~3:D ~15@S = ~S~%"
                    jj (aref names jj) (PQgetvalue res ii jj))))))

    ;; close the cursor
    (sql-transaction conn "CLOSE mycursor" PGRES_COMMAND_OK)

    ;; commit the transaction
    (sql-transaction conn "COMMIT" PGRES_COMMAND_OK)))

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
  (with-sql-connection (conn)
    (sql-transaction conn "LISTEN TBL2" PGRES_COMMAND_OK)

    (loop (PQconsumeInput conn)
      (loop :for notify = (PQnotifies conn)
        :while notify :do (format t "ASYNC NOTIFY: ~a~%" notify)
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
  (with-sql-connection (conn)
    (sql-transaction conn "BEGIN" PGRES_COMMAND_OK)

    (sql-transaction conn
                     "DECLARE mycursor BINARY CURSOR FOR select * from test1"
                     PGRES_COMMAND_OK)

    (with-sql-transaction (res conn "FETCH ALL in mycursor" PGRES_TUPLES_OK)
      (let ((i-fnum (PQfnumber res "i"))
            (d-fnum (PQfnumber res "d"))
            (p-fnum (PQfnumber res "p"))
            (nfields (PQnfields res))
            (ntuples (PQntuples res)))
        (format t " + ~d fields; ~d ntuples; i: ~d; d: ~d; p: ~d~%"
                nfields ntuples i-fnum d-fnum p-fnum)
        (dotimes (ii 3)
          (format t "type[~d] = ~d, size[~d] = ~d~%"
                  ii (PQftype res ii) ii (PQfsize res ii)))
        (dotimes (ii ntuples)
          (let ((plen (PQgetlength res ii p-fnum))
                (ival (PQgetvalue res ii i-fnum))
                (dval (PQgetvalue res ii d-fnum)))
            (format t " ++ plen: ~d; ival: ~d; dval: ~f~%" plen ival dval)))))

    (sql-transaction conn "CLOSE mycursor" PGRES_COMMAND_OK)
    (sql-transaction conn "COMMIT" PGRES_COMMAND_OK)))

)
