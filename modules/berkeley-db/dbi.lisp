;;; Copyright (C) 2003-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See <http://www.gnu.org/copyleft/gpl.html>

(defpackage "BDB"
  (:use "COMMON-LISP" "EXT")
  (:nicknames "BERKELEY-DB" "BERKDB")
  (:export "DB-VERSION"
           "ENV-CREATE" "ENV-CLOSE" "ENV-DBREMOVE" "ENV-DBRENAME" "ENV-OPEN"
           "ENV-REMOVE" "ENV-SET-OPTIONS" "ENV-GET-OPTIONS"
           "DB-CREATE" "DB-CLOSE" "DB-DEL" "DB-FD" "DB-GET" "DB-STAT"
           "DB-OPEN" "DB-SYNC" "DB-TRUNCATE" "DB-UPGRADE" "DB-REMOVE"
           "DB-RENAME" "DB-PUT" "DB-JOIN" "DB-KEY-RANGE" "DB-VERIFY"
           "DB-SET-OPTIONS" "DB-GET-OPTIONS"
           "MAKE-CURSOR" "CURSOR-CLOSE" "CURSOR-COUNT" "CURSOR-DEL"
           "CURSOR-DUP" "CURSOR-GET" "CURSOR-PUT"
           "TXN-BEGIN" "TXN-ABORT" "TXN-COMMIT" "TXN-DISCARD" "TXN-ID"
           "TXN-CHECKPOINT" "TXN-PREPARE" "TXN-RECOVER" "TXN-SET-TIMEOUT"
           "TXN-STAT"
           "BDB-ERROR" "BDB-ERROR-NUMBER"
           "WITH-OPEN-DB"))

(setf (package-lock "EXT") nil)
(use-package '("BDB") "EXT")
(ext:re-export "BDB" "EXT")
(pushnew :berkeley-db *features*)
(in-package "BDB")

;;; objects
(defstruct (env (:constructor mkenv (handle)))
  (handle nil :read-only t))
(defstruct (db (:constructor mkdb (handle)))
  (handle nil :read-only t))
(defstruct (cursor (:constructor mkcursor (handle)))
  (handle nil :read-only t))
(defstruct (txn (:constructor mktxn (handle)))
  (handle nil :read-only t))
(defstruct (lsn (:constructor mklsn (file offset)))
  (file 0 :type (unsigned-byte 32) :read-only t)
  (offset 0 :type (unsigned-byte 32) :read-only t))

(defstruct (db-stat (:constructor nil))
  (byte-swapped nil :read-only t)
  (magic nil :read-only t)
  (version nil :read-only t)
  (num-keys nil :read-only t)
  (num-data nil :read-only t)
  (page-size nil :read-only t))
(defstruct (db-stat-hash (:include db-stat)
                         (:constructor mkdbstat-hash
                                       (byte-swapped magic version num-keys
                                        num-data page-size
                                        fill-factor num-buckets free bfree
                                        big-pages big-bfree overflows
                                        overflows-free dup dup-free)))
  (fill-factor nil :read-only t)
  (num-buckets nil :read-only t)
  (free nil :read-only t)
  (bfree nil :read-only t)
  (big-pages nil :read-only t)
  (big-bfree nil :read-only t)
  (overflows nil :read-only t)
  (overflows-free nil :read-only t)
  (dup nil :read-only t)
  (dup-free nil :read-only t))

(defstruct (db-stat-btree (:include db-stat)
                          (:constructor mkdbstat-btree
                                        (byte-swapped magic version num-keys
                                         num-data page-size
                                         min-key re-len re-pad levels
                                         internal-pages leaf-pages dup over
                                         free int-pgfree leaf-pgfree dup-pgfree
                                         over-pgfree)))
  (min-key nil :read-only t)
  (re-len nil :read-only t)
  (re-pad nil :read-only t)
  (levels nil :read-only t)
  (internal-pages nil :read-only t)
  (leaf-pages nil :read-only t)
  (dup nil :read-only t)
  (over nil :read-only t)
  (free nil :read-only t)
  (int-pgfree nil :read-only t)
  (leaf-pgfree nil :read-only t)
  (dup-pgfree nil :read-only t)
  (over-pgfree nil :read-only t))
(defstruct (db-stat-queue (:include db-stat)
                          (:constructor mkdbstat-queue
                                        (byte-swapped magic version num-keys
                                         num-data page-size
                                         extent-size pages re-len re-pad
                                         pg-free first-recno curr-recno)))
  (extent-size nil :read-only t)
  (pages nil :read-only t)
  (re-len nil :read-only t)
  (re-pad nil :read-only t)
  (pg-free nil :read-only t)
  (first-recno nil :read-only t)
  (curr-recno nil :read-only t))

(defstruct (db-txn-active (:constructor mktxnactive
                                        (txnid parentid lsn xa_status xid)))
  ;; The transaction ID of the transaction.
  (txnid 0 :type (unsigned-byte 32) :read-only t)
  ;; The transaction ID of the parent transaction (or 0, if no parent).
  (parentid 0 :type (unsigned-byte 32) :read-only t)
  ;; The current log sequence number when the transaction was begun.
  (lsn nil :type lsn :read-only t)
  ;; If the transaction is an XA transaction, the status of the
  ;; transaction, otherwise 0.
  (xa_status 0 :type (unsigned-byte 32) :read-only t)
  ;; If the transaction is an XA transaction, the transaction's XA ID.
  (xid nil :type (vector (unsigned-byte 8)
                         #,(env-get-options nil :DB_XIDDATASIZE))
       :read-only t))

(defstruct (db-txn-stat (:constructor mktxnstat
                                      (last_ckp time_ckp last_txnid maxtxns
                                       nactive maxnactive nbegins naborts
                                       ncommits nrestores regsize region_wait
                                       region_nowait txnarray)))
  ;; The LSN of the last checkpoint.
  (last_ckp nil :type lsn :read-only t)
  ;; The time the last completed checkpoint finished
  (time_ckp 0 :type integer :read-only t)
  ;; The last transaction ID allocated.
  (last_txnid 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of active transactions configured.
  (maxtxns 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that are currently active.
  (nactive 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of active transactions at any one time.
  (maxnactive 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that have begun.
  (nbegins 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that have aborted.
  (naborts 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that have committed.
  (ncommits 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that have been restored.
  (nrestores 0 :type (unsigned-byte 32) :read-only t)
  ;; The size of the region.
  (regsize 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times that a thread of control was forced to wait
  ;; before obtaining the region lock.
  (region_wait 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times that a thread of control was able to obtain the
  ;; region lock without waiting.
  (region_nowait 0 :type (unsigned-byte 32) :read-only t)
  ;; an array of NACTIVE DB-TXN-ACTIVE structures, describing the
  ;; currently active transactions.
  (txnarray nil :type vector :read-only t))

;;; macros (see macros2.lisp for `with-open-file')
(defmacro with-open-db ((var &rest options) &body forms)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY forms)
    `(LET ((,var (BDB:DB-OPEN ,@options)))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       (UNWIND-PROTECT (PROGN ,@body-rest)
         (WHEN ,var (BDB:DB-CLOSE ,var))))))

(ext:without-package-lock ("CL")
(defmethod close ((dbe env) &key abort)
  (declare (ignore abort))
  (env-close dbe))
(defmethod close ((db db) &key abort)
  (declare (ignore abort))
  (db-close db))
(defmethod close ((cu cursor) &key abort)
  (declare (ignore abort))
  (cursor-close cu))
)

(define-condition bdb-error (simple-error)
  (($errno :initarg :errno :reader bdb-error-number)))

#+(or)
(progn ;; sample
 (bdb:db-version)
 (setq dbe (bdb:env-create))
 (bdb:env-set-options dbe :errfile "bdb-errors" :verbose t
                      :data_dir "d:/sds/work/eeld/Test BDBs/35_teth_db/")
 (bdb:env-get-options dbe)
 (bdb:env-open dbe :home "berkeley-db/" :create t :init_mpool t)

 (setq db (bdb:db-create dbe))
 (bdb:db-open db "admin.db" :rdonly t)
 (bdb:db-open db "d:/sds/work/eeld/Test BDBs/35_teth_db/admin.db" :rdonly t)
 (bdb:db-open db "d:/sds/work/eeld/Test BDBs/35_teth_db/index.db" :rdonly t)
 (bdb:db-get-options db)
 (bdb:db-stat db)
 (bdb:db-fd db)

 (setq cu (bdb:make-cursor db))
 (loop (multiple-value-bind (key val)
           (bdb:cursor-get cu nil nil :DB_NEXT :error nil)
         (when (eq key :notfound) (return))
         (format t "=> ~S~%-> ~S~%" key
                 (ext:convert-string-from-bytes val charset:utf-8))))
 (bdb:db-get db (ext:convert-string-to-bytes "foo" *misc-encoding*) :error nil)

 (bdb:env-set-options dbe :errfile nil)
 (bdb:env-close dbe)
 (bdb:db-close db)
 (bdb:cursor-close cu)
)

;;; restore locks
(push "BDB" *system-package-list*)
(setf (package-lock *system-package-list*) t)
