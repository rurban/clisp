;;; Copyright (C) 2003-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See <http://www.gnu.org/copyleft/gpl.html>

(defpackage "BDB"
  (:use "COMMON-LISP" "EXT")
  (:nicknames "BERKELEY-DB" "BERKDB")
  (:export "DB-VERSION"
           "BDB-HANDLE" "BDB-HANDLE-PARENTS" "BDB-HANDLE-DEPENDENTS"
           "DBE" "DB" "TXN" "DBC" "LOGC" "MPOOLFILE" "DBLOCK"
           "DBE-CREATE" "DBE-CLOSE" "DBE-DBREMOVE" "DBE-DBRENAME" "DBE-OPEN"
           "DBE-REMOVE" "DBE-SET-OPTIONS" "DBE-GET-OPTIONS"
           "DB-CREATE" "DB-CLOSE" "DB-DEL" "DB-FD" "DB-GET" "DB-STAT"
           "DB-OPEN" "DB-SYNC" "DB-TRUNCATE" "DB-UPGRADE" "DB-REMOVE"
           "DB-RENAME" "DB-PUT" "DB-JOIN" "DB-KEY-RANGE" "DB-VERIFY"
           "DB-SET-OPTIONS" "DB-GET-OPTIONS"
           "MAKE-DBC" "DBC-CLOSE" "DBC-COUNT" "DBC-DEL"
           "DBC-DUP" "DBC-GET" "DBC-PUT"
           "LOCK-DETECT" "LOCK-GET" "LOCK-ID" "LOCK-ID-FREE" "LOCK-PUT"
           "LOCK-CLOSE" "LOCK-STAT"
           "TXN-BEGIN" "TXN-ABORT" "TXN-COMMIT" "TXN-DISCARD" "TXN-ID"
           "TXN-CHECKPOINT" "TXN-PREPARE" "TXN-RECOVER" "TXN-SET-TIMEOUT"
           "TXN-STAT"
           "BDB-ERROR" "BDB-ERROR-NUMBER"
           "WITH-DB" "WITH-DBC"))

(setf (package-lock "EXT") nil)
(use-package '("BDB") "EXT")
(ext:re-export "BDB" "EXT")
(pushnew :berkeley-db *features*)
(in-package "BDB")

;;; objects
(defstruct (bdb-handle (:constructor nil) (:copier nil))
  (handle nil :read-only t)
  (parents nil)       ; parents cannot be closed until this is closed
  (dependents nil))  ; cannot close this until all dependents are closed
(defstruct (dbe (:include bdb-handle) (:copier nil)
                (:constructor mkdbe (handle parents))))
(defstruct (db (:include bdb-handle) (:copier nil)
               (:constructor mkdb (handle parents))))
(defstruct (dbc (:include bdb-handle) (:copier nil)
                   (:constructor mkdbc (handle parents))))
(defstruct (txn (:include bdb-handle) (:copier nil)
                (:constructor mktxn (handle parents))))
(defstruct (logc (:include bdb-handle) (:copier nil)
                 (:constructor mklogc (handle parents))))
(defstruct (mpoolfile (:include bdb-handle) (:copier nil)
                      (:constructor mkmpoolfile (handle parents))))
(defstruct (dblock (:copier nil) (:constructor mkdblock (handle parent)))
  (handle nil :read-only t) parent)

(defun mkhandle (maker parents closer handle)
  "make BDB-HANDLE, add it to the DEPENDETS of its PARENT, call FINALIZE"
  (unless (listp parents) (setq parents (list parents)))
  (let ((bdb-handle (funcall maker handle parents)))
    (dolist (parent parents)
      (push bdb-handle (bdb-handle-dependents parent)))
    (finalize bdb-handle closer)
    bdb-handle))
(defun kill-handle (handle)
  "close all dependents, remove from parents' dependents"
  (mapc #'close (bdb-handle-dependents handle))
  (dolist (p (bdb-handle-parents handle))
    (setf (bdb-handle-dependents p)
          (delete handle (bdb-handle-dependents p)))))

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
                                         internal-pages leaf-pages dup-pages
                                         overflow-pager
                                         free int-pgfree leaf-pgfree dup-pgfree
                                         over-pgfree)))
  (min-key nil :read-only t)
  (re-len nil :read-only t)
  (re-pad nil :read-only t)
  (levels nil :read-only t)
  (internal-pages nil :read-only t)
  (leaf-pages nil :read-only t)
  (dup-pages nil :read-only t)
  (overflow-pager nil :read-only t)
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

(defstruct (db-lock-stat (:constructor
                          mklockstat
                          (st_id st_cur_maxid st_nmodes st_maxlocks
                           st_maxlockers st_maxobjects st_nlocks st_maxnlocks
                           st_nlockers st_maxnlockers st_nobjects st_maxnobjects
                           st_nrequests st_nreleases st_nnowaits st_nconflicts
                           st_ndeadlocks st_locktimeout st_nlocktimeouts
                           st_txntimeout st_ntxntimeouts st_regsize
                           st_region_wait st_region_nowait)))
  ;; The last allocated locker ID.
  (st_id 0 :type (unsigned-byte 32) :read-only t)
  ;; The current maximum unused locker ID.
  (st_cur_maxid 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of lock modes.
  (st_nmodes 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of locks possible.
  (st_maxlocks 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of lockers possible.
  (st_maxlockers 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of lock objects possible.
  (st_maxobjects 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of current locks.
  (st_nlocks 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of locks at any one time.
  (st_maxnlocks 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of current lockers.
  (st_nlockers 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of lockers at any one time.
  (st_maxnlockers 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of current lock objects.
  (st_nobjects 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of lock objects at any one time.
  (st_maxnobjects 0 :type (unsigned-byte 32) :read-only t)
  ;; The total number of locks requested.
  (st_nrequests 0 :type (unsigned-byte 32) :read-only t)
  ;; The total number of locks released.
  (st_nreleases 0 :type (unsigned-byte 32) :read-only t)
  ;; The total number of lock requests failing because DB_LOCK_NOWAIT was set.
  (st_nnowaits 0 :type (unsigned-byte 32) :read-only t)
  ;; The total number of locks not immediately available due to conflicts.
  (st_nconflicts 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of deadlocks.
  (st_ndeadlocks 0 :type (unsigned-byte 32) :read-only t)
  ;; Lock timeout value.
  (st_locktimeout 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of lock requests that have timed out.
  (st_nlocktimeouts 0 :type (unsigned-byte 32) :read-only t)
  ;; Transaction timeout value.
  (st_txntimeout 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that have timed out. This value is also
  ;; a component of st_ndeadlocks, the total number of deadlocks detected.
  (st_ntxntimeouts 0 :type (unsigned-byte 32) :read-only t)
  ;; The size of the lock region.
  (st_regsize 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times that a thread of control was forced to wait
  ;; before obtaining the region lock.
  (st_region_wait 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times that a thread of control was able to obtain the
  ;; region lock without waiting.
  (st_region_nowait 0 :type (unsigned-byte 32) :read-only t))


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
                         #,(dbe-get-options nil :DB_XIDDATASIZE))
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
(defmacro with-db ((var dbe file &rest options &key xa &allow-other-keys)
                   &body forms)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY forms)
    (remf options :xa)
    `(LET ((,var (BDB:DB-CREATE ,dbe :xa ,xa)))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       (BDB:DB-OPEN ,var ,file ,@options)
       (UNWIND-PROTECT (PROGN ,@body-rest)
         (WHEN ,var (BDB:DB-CLOSE ,var))))))
(defmacro with-dbc ((var &rest options) &body forms)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY forms)
    `(LET ((,var (BDB:MAKE-DBC ,@options)))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       (UNWIND-PROTECT (PROGN ,@body-rest)
         (WHEN ,var (BDB:DBC-CLOSE ,var))))))

(ext:without-package-lock ("CL")
(defmethod close ((dbe dbe) &key abort)
  (declare (ignore abort))
  (dbe-close dbe))
(defmethod close ((db db) &key abort)
  (declare (ignore abort))
  (db-close db))
(defmethod close ((cu dbc) &key abort)
  (declare (ignore abort))
  (dbc-close cu))
(defmethod close ((lock dblock) &key abort)
  (declare (ignore abort))
  (lock-close lock))
(defmethod close ((tx txn) &key abort)
  (if abort (txn-abort tx) (txn-commit tx)))
)

(define-condition bdb-error (simple-error)
  (($errno :initarg :errno :reader bdb-error-number)))

;;; restore locks
(push "BDB" *system-package-list*)
(setf (package-lock *system-package-list*) t)
