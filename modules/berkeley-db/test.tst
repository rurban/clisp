;; -*- Lisp -*-
;; some tests for Berkeley-DB
;; clisp -K full -E utf-8 -q -norc -i ../tests/tests -x '(run-test "berkeley-db/test")'

(multiple-value-bind (ve ma mi pa) (bdb:db-version)
  (format t "~&Version: ~S (~D.~D.~D)~%" ve ma mi pa))
NIL

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
(defun finish-file (file)
  (when (probe-file file)
    (with-open-file (st file :direction :input)
      (format t "~&~S: ~:D byte~:P:~%" file (file-length st))
      (loop :for l = (read-line st nil nil) :while l
        :do (format t "--> ~S~%" l))))
  (null (delete-file file)))
finish-file
(prepare-dir "bdb-home/") NIL
(prepare-dir "bdb-data/") NIL
(progn (delete-file "bdb-errors") NIL) NIL

;;; creation

(defvar *dbe* (print (bdb:env-create))) *dbe*

(bdb:env-set-options *dbe* :errfile "bdb-errors" :verbose t
                     :data_dir "bdb-data/")
NIL

(progn (print (bdb:env-get-options *dbe*)) nil) NIL

(bdb:env-open *dbe* :home "bdb-home/" :create t :init_mpool t) NIL

(progn (print (bdb:env-get-options *dbe*)) nil) NIL

(defvar *db* (print (bdb:db-create *dbe*))) *db*

;; the actual file goes to ./bdb-data/bazonk.db !
(bdb:db-open *db* "bazonk.db" :type :BTREE :create t) NIL

(null (probe-file "./bdb-data/bazonk.db")) NIL

(bdb:db-put *db* "foo" "bar")
NIL
(bdb:db-put *db* "fep" "blicket")
NIL

(bdb:db-sync *db*) NIL
(show-db *db*) NIL
(bdb:db-close *db*)   T

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
    (bdb:db-close db)))
("foo" "bar" "foobar" :NOTFOUND)

(let ((db (bdb:db-create *dbe*)))
  (bdb:db-set-options db :RE_SOURCE "recno-source.txt")
  (bdb:db-open db "recno-source.db")
  (unwind-protect
       (bdb:db-put db 5 "bazonk")
    (bdb:db-close db)))
NIL

(bdb:with-open-db (db *dbe* "recno-source.db" :rdonly t)
  (show-db db)
  (bdb:with-cursor (cu db)
    (list
     (loop :with key :and val
       :do (setf (values key val)
                 (bdb:cursor-get cu :INTEGER :STRING :DB_NEXT :error nil))
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
  (bdb:with-open-db (db *dbe* (format nil "test-~A.db" type)
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
      (bdb:db-close db))
    (print db)))
NIL

(bdb:env-close *dbe*) T

(ext:dir "bdb-home/**") NIL
(ext:dir "bdb-data/**") NIL
(finish-file "bdb-errors") NIL

;;; access

(progn (setq *dbe* (print (bdb:env-create))) nil) NIL

(bdb:env-set-options *dbe* :errfile "bdb-errors" :verbose t
                     :data_dir "bdb-data/")
NIL

(bdb:env-open *dbe* :home "bdb-home/" :create t :init_mpool t)  NIL

(progn (print (bdb:env-get-options *dbe*)) nil) NIL

(progn (setq *db* (print (bdb:db-create *dbe*))) nil) NIL

(bdb:db-open *db* "bazonk.db" :rdonly t) NIL

(show-db *db*) NIL

(defvar *cursor* (print (bdb:make-cursor *db*))) *cursor*

(let ((li ()))
  (loop (multiple-value-bind (key val)
            (bdb:cursor-get *cursor* :STRING :STRING :DB_NEXT :error nil)
          (when (eq key :notfound) (return li))
          (format t "~&=[count=~D]=> ~S -> ~S~%"
                  (bdb:cursor-count *cursor*) key val)
          (push (list key val) li))))
(("foo" "bar") ("fep" "blicket"))

(bdb:db-get *db* "bar" :error nil :type :raw)
:NOTFOUND

(bdb:db-get *db* "foo")
#(98 97 114)                    ; "bar"

(bdb:cursor-close *cursor*) T
(bdb:db-close *db*)         T

(progn (setq *db* (print (bdb:db-create *dbe*))) nil) NIL
(bdb:db-open *db* "bazonk.db") NIL
(bdb:db-truncate *db*)      2   ; the number of records discarded
(bdb:db-close *db*)         T

;;; read factorials from (:BTREE :HASH)
(dolist (type '(:btree :hash))
  (print type)
  (bdb:with-open-db (db *dbe* (format nil "test-~A.db" type) :rdonly t)
    (show-db db)
    (bdb:with-cursor (cu db)
      (loop (multiple-value-bind (key val)
                (bdb:cursor-get cu :INTEGER :INTEGER :DB_NEXT :error nil)
              (when (eq key :notfound) (return))
              (format t "~&=[count=~D]=> ~S -> ~S~%"
                      (bdb:cursor-count cu) key val)
              (assert (= (! key) val)))))))
NIL

;;; read factorials from (:QUEUE :RECNO)
(dolist (type '(:queue :recno))
  (print type)
  (bdb:with-open-db (db *dbe* (format nil "test-~A.db" type) :rdonly t)
    (show-db db)
    (bdb:with-cursor (cu db)
      (loop (multiple-value-bind (key val)
                (bdb:cursor-get cu :INTEGER :INTEGER :DB_NEXT :error nil)
              (when (eq key :notfound) (return))
              (format t "~&=[count=~D]=> ~S -> ~S~%"
                      (bdb:cursor-count cu) key val)
              (multiple-value-bind (key1 val1)
                  (bdb:cursor-get cu :INTEGER :INTEGER :DB_NEXT)
                (format t "~&=[count=~D]=> ~S -> ~S~%"
                        (bdb:cursor-count cu) key1 val1)
                (assert (= (! val) val1))))))))
NIL

(bdb:env-close *dbe*)       T

(finish-file "bdb-errors") T    ; no errors, bdb-errors does not exist
(rmrf "bdb-home/") T
(rmrf "bdb-data/") T
