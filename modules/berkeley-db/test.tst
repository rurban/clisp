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

(prepare-dir "bdb-home/") NIL
(prepare-dir "bdb-data/") NIL

;;; creation

(defvar *dbe* (print (bdb:env-create))) *dbe*

(bdb:env-set-options *dbe* :errfile "bdb-errors" :verbose t
                     :data_dir "bdb-data/")
NIL

(progn (print (multiple-value-list (bdb:env-get-options *dbe*))) nil) NIL

(bdb:env-open *dbe* :home "bdb-home/" :create t :init_mpool t) NIL

(progn (print (multiple-value-list (bdb:env-get-options *dbe*))) nil) NIL

(defvar *db* (print (bdb:db-create *dbe*))) *db*

(bdb:db-open *db* "bdb-data/bazonk.db" :type :BTREE :create t) NIL

(bdb:db-put *db* (ext:convert-string-to-bytes "foo" charset:utf-8)
            (ext:convert-string-to-bytes "bar" charset:utf-8))
NIL
(bdb:db-put *db* (ext:convert-string-to-bytes "fep" charset:utf-8)
            (ext:convert-string-to-bytes "blicket" charset:utf-8))
NIL

(bdb:db-sync *db*) NIL

(integerp (print (bdb:db-fd *db*))) T
(progn (print (multiple-value-list (bdb:db-get-options *db*))) nil) NIL
(progn (print (bdb:db-stat *db*)) nil) nil

(bdb:db-close *db*)   T
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

(progn (print (multiple-value-list (bdb:env-get-options *dbe*))) nil) NIL

(progn (setq *db* (print (bdb:db-create *dbe*))) nil) NIL

(bdb:db-open *db* "bdb-data/bazonk.db" :rdonly t) NIL

(integerp (print (bdb:db-fd *db*))) T
(progn (print (multiple-value-list (bdb:db-get-options *db*))) nil) NIL
(progn (print (bdb:db-stat *db*)) nil) nil

(defvar *cursor* (print (bdb:make-cursor *db*))) *cursor*

(let ((li ()))
  (loop (multiple-value-bind (key val)
            (bdb:cursor-get *cursor* nil nil :DB_NEXT :error nil)
          (when (eq key :notfound) (return li))
          (setq key (ext:convert-string-from-bytes key charset:utf-8)
                val (ext:convert-string-from-bytes val charset:utf-8))
          (format t "~&=[count=~D]=> ~S -> ~S~%"
                  (bdb:cursor-count *cursor*) key val)
          (push (list key val) li))))
(("foo" "bar") ("fep" "blicket"))

(bdb:db-get *db* (ext:convert-string-to-bytes "bar" charset:utf-8) :error nil)
:NOTFOUND

(ext:convert-string-from-bytes
 (bdb:db-get *db* (ext:convert-string-to-bytes "foo" charset:utf-8))
 charset:utf-8)
"bar"

(bdb:cursor-close *cursor*) T
(bdb:db-close *db*)         T
(bdb:env-close *dbe*)       T

(with-open-file (e "bdb-errors" :direction :input)
  (format t "~&errors: ~:D byte~:P:~%" (file-length e))
  (loop :for s = (read-line e nil nil) :while s :do (format t "--> ~S~%" s)))
NIL

(rmrf "bdb-home/") T
(rmrf "bdb-data/") T
(null (delete-file "bdb-errors")) NIL
