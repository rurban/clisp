;; -*- Lisp -*- vim:filetype=lisp
;; some tests for GDBM
;; clisp -E 1:1 -q -norc -i ../tests/tests -x '(run-test "../modules/gdbm/test" :logname "gdbm/test")'

(list (null (require "gdbm"))) (#-GDBM NIL #+GDBM T)
(listp (show (multiple-value-list (ext:module-info "gdbm" t)) :pretty t)) T

(stringp (show (gdbm:gdbm-version))) T

(handler-case (gdbm:gdbm-open "///" :read-write :newdb)
  (gdbm:gdbm-error (condition)
    (list (gdbm:gdbm-error-code condition)
          (gdbm:gdbm-error-message condition))))
(:FILE-OPEN-ERROR "File open error")

(defparameter *db*
  (gdbm:gdbm-open "test.db" :read-write :newdb
                  :mode (os:convert-mode '(:RUSR :WUSR))
                  :options '(:sync))) *DB*
(gdbm:gdbm-p (show *db* :pretty t)) T
(gdbm:gdbm-open-p *db*) T

(stringp (show (gdbm:gdbm-path *db*))) T
(integerp (show (gdbm:gdbm-file-size *db*))) T

(handler-case (setf (gdbm:gdbm-opt *db* :cachesize) 1024)
  (type-error (c) (princ-error c) 1024)) 1024
(handler-case (gdbm:gdbm-opt *db* :cachesize)
  (type-error (c) (princ-error c) 1024)) 1024

(handler-case (gdbm:gdbm-opt *db* :flags)
  (type-error (c) (princ-error c) '(:NEWDB :SYNC))) (:NEWDB :SYNC)
(handler-case (setf (gdbm:gdbm-opt *db* :syncmode) nil)
  (type-error (c) (princ-error c) nil)) NIL
(handler-case (gdbm:gdbm-opt *db* :syncmode)
  (type-error (c) (princ-error c) nil)) NIL
(handler-case (gdbm:gdbm-opt *db* :flags)
  (type-error (c) (princ-error c) '(:NEWDB))) (:NEWDB)
(handler-case (setf (gdbm:gdbm-opt *db* :syncmode) t)
  (type-error (c) (princ-error c) t)) T
(handler-case (gdbm:gdbm-opt *db* :syncmode)
  (type-error (c) (princ-error c) t)) T

(handler-case (gdbm:gdbm-opt *db* :flags)
  (type-error (c) (princ-error c) '(:NEWDB :SYNC))) (:NEWDB :SYNC)
(handler-case (gdbm:gdbm-opt *db* :mmap)
  (type-error (c) (princ-error c) t)) T
(handler-case (gdbm:gdbm-opt *db* :centfree)
  (type-error (c) (princ-error c) t)) T
(handler-case (gdbm:gdbm-opt *db* :coalesceblks)
  (type-error (c) (princ-error c) nil)) NIL
(handler-case (integerp (show (gdbm:gdbm-opt *db* :maxmapsize)))
  (type-error (c) (princ-error c) t)) T
(handler-case (equal (namestring (truename (gdbm:gdbm-path *db*)))
                     (gdbm:gdbm-opt *db* :dbname))
  (type-error (c) (princ-error c) t)) T
(handler-case (gdbm:gdbm-opt *db* :blocksize)
  (type-error (c) (princ-error c) 512)) 512

(gdbm:do-db (key *db*) :count key) 0
(if (fboundp 'gdbm:gdbm-count) (gdbm:gdbm-count *db*) 0) 0

(handler-case (setf (gdbm:gdbm-opt *db* :cachesize) 1024)
  (gdbm:gdbm-error (condition)
    (list (gdbm:gdbm-error-code condition)
          (gdbm:gdbm-error-message condition)))
  (type-error (c) (princ-error c) '(:OPT-ALREADY-SET "Option already set")))
(:OPT-ALREADY-SET "Option already set")

(multiple-value-list (gdbm:gdbm-store *db* "key1" "value1")) NIL

(handler-bind ((type-error (lambda (c) (princ-error c) (use-value *db*))))
  (setf (gdbm:gdbm-opt nil :default-value-type) 'integer)) INTEGER
(gdbm:gdbm-opt *db* :default-value-type) INTEGER
(setf (gdbm:gdbm-opt *db* :default-value-type) 'string) STRING
(gdbm:gdbm-opt *db* :default-value-type) STRING
(gdbm:gdbm-opt *db* :default-key-type) NIL

(gdbm:gdbm-fetch *db* "key1") "value1"

(gdbm:gdbm-fetch *db* "key1" :type 'string) "value1"

(handler-case (gdbm:gdbm-store *db* "key1" "value2" :flag :insert)
  (gdbm:gdbm-error (condition)
    (list (gdbm:gdbm-error-code condition)
          (gdbm:gdbm-error-message condition))))
(:CANNOT-REPLACE "Cannot replace")

(gdbm:gdbm-fetch *db* "key1") "value1"

(multiple-value-list (gdbm:gdbm-store *db* "key1" "value2" :flag :replace)) NIL

(gdbm:gdbm-fetch *db* "key1") "value2"

(gdbm:gdbm-exists *db* #(107 101 121 48)) NIL

(gdbm:gdbm-exists *db* #(107 101 121 49)) T

(gdbm:gdbm-fetch *db* #(107 101 121 49)) "value2"

(multiple-value-list (gdbm:gdbm-store *db* "key1" "value3")) NIL

(gdbm:gdbm-fetch *db* "key1") "value3"

(gdbm:gdbm-exists *db* #(107 101 121 50)) NIL

(multiple-value-list (gdbm:gdbm-store *db* "key2" "test2")) NIL

(gdbm:gdbm-exists *db* #(107 101 121 50)) T

(gdbm:gdbm-fetch *db* "key2") "test2"

(gdbm:gdbm-fetch *db* "key2" :type 'vector) #(116 101 115 116 50)

(multiple-value-list (gdbm:gdbm-store *db* "key3" "test3")) NIL

(multiple-value-list (gdbm:gdbm-store *db* "key4" #(0 0 0 0 0))) NIL

(gdbm:gdbm-fetch *db* "key4" :type 'vector) #(0 0 0 0 0)

(gdbm:gdbm-open-p *db*) T

(gdbm:gdbm-close *db*) T

(gdbm:gdbm-open-p *db*) NIL

(gdbm:gdbm-close *db*) NIL

(gdbm:gdbm-p (setf *db* (gdbm:gdbm-open *db* :default-key-type 'string))) T

(gdbm:do-db (key *db*) :count key) 4
(if (fboundp 'gdbm:gdbm-count) (gdbm:gdbm-count *db*) 4) 4

(multiple-value-list (gdbm:gdbm-delete *db* "key1")) NIL

(gdbm:do-db (key *db*) :count key) 3
(if (fboundp 'gdbm:gdbm-count) (gdbm:gdbm-count *db*) 3) 3

(gdbm:gdbm-sync *db*) NIL

(listp (show (gdbm:do-db (key *db*) :collect (gdbm:gdbm-file-size *db*)))) T

(let ((bsize (gdbm:gdbm-file-size *db*)) (asize 0)
      (bnum (and (fboundp 'gdbm:gdbm-count) (gdbm:gdbm-count *db*))) (anum 0))
  (loop :for i :from 0 :to 1000 :do
    (gdbm:gdbm-store *db* (format nil "key~A" i) (format nil "value~A" i)))
  (gdbm:gdbm-sync *db*)
  (setf asize (gdbm:gdbm-file-size *db*)
        anum (and (fboundp 'gdbm:gdbm-count) (gdbm:gdbm-count *db*)))
  (format t "~&File size: ~:DB/~:D --> ~:DB/~:D~%" bsize bnum asize anum)
  (> asize bsize)) T

(let ((bsize (gdbm:gdbm-file-size *db*)) (asize 0)
      (bnum (and (fboundp 'gdbm:gdbm-count) (gdbm:gdbm-count *db*))) (anum 0))
  (loop :for i :from 0 :to 500 :do
      (gdbm:gdbm-delete *db* (format nil "key~A" i)))
  (gdbm:gdbm-sync *db*)
  #-:CYGWIN (gdbm:gdbm-reorganize *db*)
  (setf asize (gdbm:gdbm-file-size *db*)
        anum (and (fboundp 'gdbm:gdbm-count) (gdbm:gdbm-count *db*)))
  (format t "~&File size: ~:DB/~:D --> ~:DB/~:D~%" bsize bnum asize anum)
  (< asize bsize)) T
#-:CYGWIN T
#+:CYGWIN NIL

(gdbm:gdbm-close *db*) T

(gdbm:gdbm-close *db*) NIL

(gdbm:with-open-db (db *db* :read-write :reader :default-key-type 'string)
  (gdbm:do-db (key db)
    :sum (length (gdbm:gdbm-fetch db key :type 'vector)))) 4001

(gdbm:with-open-db (db *db* :read-write :writer)
  (gdbm:gdbm-store db #(0 0 0 0) #(1 1 1 1))
  (gdbm:do-db (key db :type 'vector)
    :sum (length (gdbm:gdbm-fetch db key :type 'vector)))) 4005

(handler-case (gdbm:with-open-db (db *db* :read-write :reader)
                (gdbm:gdbm-store db #(0 0 0 0) #(1 1 1 1)))
  (gdbm:gdbm-error (condition)
    (list (gdbm:gdbm-error-code condition)
          (gdbm:gdbm-error-message condition))))
(:READER-CANT-STORE "Reader can't store")

(handler-case (gdbm:with-open-db (db *db* :read-write :reader)
                (gdbm:gdbm-delete db #(0 0 0 0)))
  (gdbm:gdbm-error (condition)
    (list (gdbm:gdbm-error-code condition)
          (gdbm:gdbm-error-message condition))))
(:READER-CANT-DELETE "Reader can't delete")

(gdbm:with-open-db (db *db* :read-write :reader)
  (gdbm:gdbm-fetch db #(0 0 0 0) :type 'vector)) #(1 1 1 1)

(gdbm:with-open-db (db *db* :read-write :reader)
  (type-of (gdbm:gdbm-fetch db #(0 0 0 0) :type 'vector)))
(simple-array (unsigned-byte 8) (4))

(gdbm:with-open-db (db *db* :read-write :reader
                       :default-value-type '32bit-vector)
  (gdbm:gdbm-fetch db #(0 0 0 0)))
#(16843009)

(gdbm:with-open-db (db *db* :read-write :writer)
  (gdbm:gdbm-store db 1 17)
  (gdbm:gdbm-fetch db 1 :type 'integer)) 17

(gdbm:with-open-db (db *db* :read-write :writer)
  (loop :for i :from 1 :to 100 :for i! = (! i) :for l = (integer-length i!)
    :do (gdbm:gdbm-store db i i!)
    (when (zerop (mod (1+ l) 32)) ; 32bit-vector conversion possible
      (show (list i i! l (gdbm:gdbm-fetch db i :type 'vector)
                  (gdbm:gdbm-fetch db i :type '32bit-vector))
            :pretty t))
    :always (= (gdbm:gdbm-fetch db i :type 'integer) i!))) T

(gdbm:with-open-db (db *db* :read-write :writer)
  (gdbm:gdbm-store db 2 2.0f0)
  (gdbm:gdbm-fetch db 2 :type 'single-float)) 2.0f0

(gdbm:with-open-db (db *db* :read-write :writer)
  (gdbm:gdbm-store db 3 4.0d0)
  (gdbm:gdbm-fetch db 3 :type 'double-float)) 4.0d0

(gdbm:with-open-db (db *db* :read-write :writer)
  (gdbm:gdbm-store db 1.0f0 2.0f0)
  (gdbm:gdbm-fetch db 1.0f0 :type 'single-float)) 2.0f0

(gdbm:with-open-db (db *db* :read-write :writer)
  (gdbm:gdbm-store db 2.5d0 4.0d0)
  (gdbm:gdbm-fetch db 2.5d0 :type 'double-float)) 4.0d0

(gdbm:with-open-db (db *db* :read-write :writer)
  (let ((v (make-array 3 :element-type '(unsigned-byte 32)
                       :initial-contents '(123 456 789))))
    (gdbm:gdbm-store db v v)
    (gdbm:gdbm-fetch db v :type '32bit-vector))) #(123 456 789)

(gdbm:gdbm-close *db*) NIL

(pathnamep (delete-file (gdbm:gdbm-path *db*))) T

(unintern '*db*) T
