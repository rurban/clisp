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

(prepare-dir "bdb-home/") NIL
(prepare-dir "bdb-data/") NIL
(null (delete-file "bdb-errors")) NIL

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

(dolist (type '(:btree :hash))
  (print type)
  (bdb:with-open-db (db *dbe* (format nil "test-~A.db" type)
                        :type type :create t)
    (show-db db)
    (dotimes (i 25) (bdb:db-put db i (! i)))))
NIL

(dolist (type '(:queue :recno))
  (print type)
  (let ((db (bdb:db-create *dbe*)))
    ;; :RE_LEN must be set before DB-OPEN
    (bdb:db-set-options db :RE_LEN (print (integer-length (! 25))))
    (bdb:db-open db (format nil "test-~A.db" type) :type type :create t)
    (show-db db)
    (unwind-protect
         (dotimes (i 25) (bdb:db-put db i (! i) :flag :DB_APPEND))
      (bdb:db-close db))
    (print db)))
NIL

(bdb:env-close *dbe*) T

(ext:dir "bdb-home/*") NIL
(ext:dir "bdb-data/*") NIL

(with-open-file (e "bdb-errors" :direction :input)
  (format t "~&errors: ~:D byte~:P:~%" (file-length e))
  (loop :for s = (read-line e nil nil) :while s :do (format t "--> ~S~%" s)))
NIL

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

(dolist (type '(:btree :hash :queue :recno))
  (print type)
  (bdb:with-open-db (db *dbe* (format nil "test-~A.db" type))
    (show-db db)
    (bdb:with-cursor (cu db)
      (loop (multiple-value-bind (key val)
                (bdb:cursor-get cu :INTEGER :INTEGER :DB_NEXT :error nil)
              (when (eq key :notfound) (return))
              (format t "~&=[count=~D]=> ~S -> ~S~%"
                      (bdb:cursor-count cu) key val))))))
NIL

(bdb:env-close *dbe*)       T

(with-open-file (e "bdb-errors" :direction :input)
  (format t "~&errors: ~:D byte~:P:~%" (file-length e))
  (loop :for s = (read-line e nil nil) :while s :do (format t "--> ~S~%" s)))
NIL

(rmrf "bdb-home/") T
(rmrf "bdb-data/") T
(null (delete-file "bdb-errors")) NIL
