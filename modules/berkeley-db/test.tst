;; -*- Lisp -*-
;; some tests for Berkeley-DB
;; clisp -K full -E utf-8 -q -norc -i ../tests/tests -x '(run-test "berkeley-db/test")'

(multiple-value-bind (ve ma mi pa) (bdb:db-version)
  (format t "~&Version: ~S (~D.~D.~D)~%" ve ma mi pa))
NIL

;;; --- helpers ---
(defun kill-down (name)
  (dolist (f (directory (ext:string-concat name "**")))
    (format t "~&removing ~S~%" f)
    (if (pathname-name f)
        (delete-file f)
        (ext:delete-dir f))))
kill-down
(defun rmrf (name)
  (ext:dir (ext:string-concat name "**"))
  (kill-down name)
  (format t "~&removing ~S~%" name)
  (ext:delete-dir name))
rmrf
(defun prepare-dir (name)
  (if (ext:probe-directory name)
      (kill-down name)
      (ext:make-dir name))
  NIL)
prepare-dir
(defun show-db (db)
  (let ((*print-pretty* t))
    (print (list db (bdb:db-fd db) (bdb:db-stat db)
                 (bdb:db-get-options db))))
  nil)
show-db
(defun show-dbe (dbe)
  (let ((*print-pretty* t))
    (print (list dbe :archive (bdb:log-archive dbe)
                 (bdb:txn-stat dbe) (bdb:lock-stat dbe) (bdb:log-stat dbe)
                 (bdb:dbe-get-options dbe))))
  nil)
show-dbe
(defun finish-file (file)
  (when (probe-file file)
    (with-open-file (st file :direction :input)
      (format t "~&~S: ~:D byte~:P:~%" file (file-length st))
      (loop :for l = (read-line st nil nil) :while l
        :do (format t "--> ~S~%" l))))
  (delete-file file)
  (probe-file file))
finish-file
(progn
  (defmethod close :before ((h bdb:bdb-handle) &key abort)
    (declare (ignore abort))
    (let ((*print-pretty* t))
      (format t "~&Closing ~S~%" h)))
  (defmethod close :after ((h bdb:bdb-handle) &key abort)
    (declare (ignore abort))
    (let ((*print-pretty* t))
      (format t "~&Closed ~S~%" h)))
  nil)
nil

;;; preparations

(prepare-dir "bdb-home/") NIL
(prepare-dir "bdb-data/") NIL
(progn (delete-file "bdb-errors") NIL) NIL

;;; creation

(defvar *dbe* (print (bdb:dbe-create))) *dbe*

(bdb:dbe-set-options *dbe* :errfile "bdb-errors" :verbose t
                     :data_dir "bdb-data/")
NIL

(bdb:dbe-open *dbe* :home "bdb-home/" :create t
              :init_mpool t :init_txn t :init_lock t :init_log t)
NIL

(show-dbe *dbe*) NIL

(defvar *db* (let ((*print-pretty* t)) (print (bdb:db-create *dbe*)))) *db*

;; the actual file goes to ./bdb-data/bazonk.db !
(bdb:db-open *db* "bazonk.db" :type :BTREE :create t) NIL

(null (probe-file "./bdb-data/bazonk.db")) NIL

(bdb:db-put *db* "foo" "bar")
NIL
(bdb:db-put *db* "fep" "blicket")
NIL

(bdb:db-sync *db*) NIL
(show-db *db*) NIL
(close *db*)   T

;;; recno with underlying text file
(with-open-file (s "bdb-data/recno-source.txt" :direction :output
                   :external-format :unix)
  (write-line "foo" s)
  (write-line "bar" s)
  (write-line "foobar" s))
"foobar"

(let ((db (bdb:db-create *dbe*)))
  (bdb:db-set-options db :RE_SOURCE "recno-source.txt")
  (bdb:db-open db "recno-source.db" :type :RECNO :create t)
  (unwind-protect
       (list (bdb:db-get db 1 :type :string)
             (bdb:db-get db 2 :type :string)
             (bdb:db-get db 3 :type :string)
             (bdb:db-get db 4 :error nil))
    (close db)))
("foo" "bar" "foobar" :NOTFOUND)

(let ((db (bdb:db-create *dbe*)))
  (bdb:db-set-options db :RE_SOURCE "recno-source.txt")
  (bdb:db-open db "recno-source.db")
  (unwind-protect
       (bdb:db-put db 5 "bazonk")
    (close db)))
NIL

(bdb:with-db (db *dbe* "recno-source.db" :rdonly t)
  (show-db db)
  (bdb:with-dbc (cu db)
    (list
     (loop :with key :and val
       :do (setf (values key val)
                 (bdb:dbc-get cu :INTEGER :STRING :DB_NEXT :error nil))
       :until (eq key :notfound)
       :collect (list key val))
     (bdb:db-get db 4 :error nil))))
(((1 "foo") (2 "bar") (3 "foobar") (5 "bazonk")) :KEYEMPTY)

(with-open-file (s "bdb-data/recno-source.txt" :direction :input)
  (loop :for l = (read-line s nil nil) :while l :collect l))
("foo" "bar" "foobar" "" "bazonk")

;;; write factorials into (:BTREE :HASH)
(dolist (type '(:btree :hash))
  (print type)
  (bdb:with-db (db *dbe* (format nil "test-~A.db" type)
                        :type type :create t)
    (show-db db)
    (print (loop :repeat 20 :for x = (random 30) :for x! = (! x)
             :collect (list x x! (bdb:db-put db x x!))))))
NIL

;;; write factorials into (:QUEUE :RECNO)
(dolist (type '(:queue :recno))
  (print type)
  (let ((db (bdb:db-create *dbe*)) (max 30))
    ;; :RE_LEN must be set before DB-OPEN
    (bdb:db-set-options
     db :RE_LEN (print (* 4 (ceiling (integer-length (! max)) 32))) :RE_PAD 0)
    (bdb:db-open db (format nil "test-~A.db" type) :type type :create t)
    (show-db db)
    (unwind-protect
         (print (loop :repeat 20 :for x = (random max) :collect
                  (list (bdb:db-put db nil x :action :DB_APPEND) x
                        (bdb:db-put db nil (! x) :action :DB_APPEND) (! x))))
      (close db))
    (print db)))
NIL

;; locks - will NOT be automatically closed by DBE-CLOSE
(defparameter *locker* (print (bdb:lock-id *dbe*))) *locker*
(defparameter *lock* (print (bdb:lock-get *dbe* "foo" *locker* :DB_LOCK_READ)))
*lock*

(close *dbe*) T

(close *lock*) ERROR

(ext:dir "bdb-home/**") NIL
(ext:dir "bdb-data/**") NIL
(finish-file "bdb-errors") NIL

;;; access

(let ((*print-pretty* t)) (setq *dbe* (print (bdb:dbe-create))) nil) NIL

(bdb:dbe-set-options *dbe* :errfile "bdb-errors" :verbose t
                     :data_dir "bdb-data/")
NIL

(let ((arr #A((unsigned-byte 8) (6 6)
              ((0 0 0 0 0 0)
               (0 0 1 1 0 1)
               (0 1 1 1 1 1)
               (0 1 1 0 0 0)
               (0 0 1 0 0 0)
               (0 1 1 0 0 0)))))
  (bdb:dbe-set-options *dbe* :lk_conflicts arr)
  (equalp arr (bdb:dbe-get-options *dbe* :lk_conflicts)))
T

(bdb:dbe-open *dbe* :home "bdb-home/" :create t
              :init_mpool t :init_txn t :init_lock t :init_log t)
NIL

(show-dbe *dbe*) NIL

(let ((*print-pretty* t)) (setq *db* (print (bdb:db-create *dbe*))) nil) NIL

(bdb:db-open *db* "bazonk.db" :rdonly t) NIL

(show-db *db*) NIL

(defvar *cursor* (print (bdb:make-dbc *db*))) *cursor*

(let ((li ()))
  (loop (multiple-value-bind (key val)
            (bdb:dbc-get *cursor* :STRING :STRING :DB_NEXT :error nil)
          (when (eq key :notfound) (return li))
          (format t "~&=[count=~D]=> ~S -> ~S~%"
                  (bdb:dbc-count *cursor*) key val)
          (push (list key val) li))))
(("foo" "bar") ("fep" "blicket"))

(bdb:db-get *db* "bar" :error nil :type :raw)
:NOTFOUND

(bdb:db-get *db* "foo")
#(98 97 114)                    ; "bar"

(close *cursor*) T
(close *db*)         T

(let ((*print-pretty* t)) (setq *db* (print (bdb:db-create *dbe*))) nil) NIL
(bdb:db-open *db* "bazonk.db") NIL
(bdb:db-truncate *db*)      2   ; the number of records discarded
(close *db*)         T

;;; read factorials from (:BTREE :HASH)
(dolist (type '(:btree :hash))
  (print type)
  (bdb:with-db (db *dbe* (format nil "test-~A.db" type) :rdonly t)
    (show-db db)
    (bdb:with-dbc (cu db)
      (loop (multiple-value-bind (key val)
                (bdb:dbc-get cu :INTEGER :INTEGER :DB_NEXT :error nil)
              (when (eq key :notfound) (return))
              (format t "~&=[count=~D]=> ~S -> ~S~%"
                      (bdb:dbc-count cu) key val)
              (assert (= (! key) val)))))))
NIL

;;; read factorials from (:QUEUE :RECNO)
(dolist (type '(:queue :recno))
  (print type)
  (bdb:with-db (db *dbe* (format nil "test-~A.db" type) :rdonly t)
    (show-db db)
    (bdb:with-dbc (cu db)
      (loop (multiple-value-bind (key val)
                (bdb:dbc-get cu :INTEGER :INTEGER :DB_NEXT :error nil)
              (when (eq key :notfound) (return))
              (format t "~&=[count=~D]=> ~S -> ~S~%"
                      (bdb:dbc-count cu) key val)
              (multiple-value-bind (key1 val1)
                  (bdb:dbc-get cu :INTEGER :INTEGER :DB_NEXT)
                (format t "~&=[count=~D]=> ~S -> ~S~%"
                        (bdb:dbc-count cu) key1 val1)
                (assert (= (! val) val1))))))))
NIL

;; transactions - will be automatically closed (committed) by DBE-CLOSE
(let ((txn (bdb:txn-begin *dbe*)) (*print-pretty* t))
  (print (list txn (bdb:txn-begin *dbe* :parent txn) *dbe*))
  nil)
nil

;; *locker* & *lock* come from a previous incarnation of *dbe*
(bdb:lock-put *dbe* (print *lock*)) NIL
(bdb:lock-id-free *dbe* *locker*) NIL

(show-dbe *dbe*) NIL
(close *dbe*)    T

(finish-file "bdb-errors") NIL
(rmrf "bdb-home/") T
(rmrf "bdb-data/") T
