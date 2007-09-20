;; -*- Lisp -*-
;; some tests for GDBM
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "gdbm/test")'

(listp (show (multiple-value-list (ext:module-info "gdbm" t)) :pretty t)) T

(defvar *db* nil) *DB*

(stringp (show (gdbm:gdbm-version))) T

(handler-case
    (gdbm::gdbm-p (setf *db* (gdbm:gdbm-open "///" :read-write :newdb)))
  (gdbm::gdbm-error (condition)
    (gdbm::gdbm-error-message condition))) "File open error"

(gdbm::gdbm-p (setf *db* (gdbm:gdbm-open "test.db" :read-write :newdb))) T

(gdbm:gdbm-setopt *db* :cachesize 1024) T

(gdbm:do-db (key *db*) :count key) 0

(handler-case (gdbm:gdbm-setopt *db* :cachesize 1024)
  (gdbm::gdbm-error (condition)
    (gdbm::gdbm-error-message condition))) "Option already set"

(gdbm:gdbm-store *db* "key1" "value1") T

(gdbm:gdbm-fetch *db* "key1") "value1"

(gdbm:gdbm-store *db* "key1" "value2" :flag :insert) NIL

(gdbm:gdbm-fetch *db* "key1") "value1"

(gdbm:gdbm-store *db* "key1" "value2" :flag :replace) T

(gdbm:gdbm-fetch *db* "key1") "value2"

(gdbm:gdbm-store *db* "key1" "value3") T

(gdbm:gdbm-fetch *db* "key1") "value3"

(gdbm:gdbm-store *db* "key2" "test2") T

(gdbm:gdbm-fetch *db* "key2") "test2"

(gdbm:gdbm-fetch *db* "key2" :binary T) #(116 101 115 116 50)

(gdbm:gdbm-store *db* "key3" "test3") T

(gdbm:gdbm-store *db* "key4" #(0 0 0 0 0)) T

(gdbm:gdbm-fetch *db* "key4" :binary T) #(0 0 0 0 0)

(gdbm:gdbm-close *db*) T

(gdbm:gdbm-close *db*) NIL

(gdbm::gdbm-p (setf *db* (gdbm:gdbm-open "test.db"))) T

(gdbm:do-db (key *db*) :count key) 4

(gdbm:gdbm-delete *db* "key1") T

(gdbm:do-db (key *db*) :count key) 3

(gdbm:gdbm-sync *db*) T

(let ((bsize (with-open-file (s "test.db" :direction :input) (file-length s)))
      (asize 0))
  (loop :for i :from 0 :to 1000 :do
    (gdbm:gdbm-store *db* (format nil "key~A" i) (format nil "value~A" i)))
  (gdbm:gdbm-sync *db*)
  (setf asize (with-open-file (s "test.db" :direction :input)
                (file-length s)))
  (> asize bsize)) T

(let ((bsize (with-open-file (s "test.db" :direction :input) (file-length s)))
      (asize 0))
  (loop for i from 0 to 500 do (gdbm:gdbm-delete *db* (format nil "key~A" i)))
  (gdbm:gdbm-sync *db*)
  (gdbm:gdbm-reorganize *db*)
  (setf asize (with-open-file (s "test.db" :direction :input)
                (file-length s)))
  (< asize bsize)) T

(gdbm:gdbm-close *db*) T

(pathnamep (delete-file "test.db")) T
