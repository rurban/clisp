;;; Copyright (C) 2003 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See <http://www.gnu.org/copyleft/gpl.html>

(defpackage "BDB"
  (:use "COMMON-LISP" "EXT")
  (:nicknames "BERKELEY-DB" "BERKDB")
  (:export "DB-VERSION"
           "ENV-CREATE" "ENV-CLOSE" "ENV-DBREMOVE" "ENV-DBRENAME" "ENV-OPEN"
           "ENV-REMOVE" "ENV-SET-OPTIONS"
           "DB-CREATE" "DB-CLOSE" "DB-DEL" "DB-FD" "DB-GET" "DB-STAT"
           "DB-OPEN" "DB-SYNC" "DB-TRUNCATE" "DB-UPGRADE" "DB-REMOVE"
           "DB-RENAME" "DB-PUT"
           "TXN-BEGIN" "TXN-ABORT" "TXN-COMMIT" "TXN-DISCARD"
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
(defstruct (txn (:constructor mktxn (handle)))
  (handle nil :read-only t))

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


;;; macros (see macros1.lisp for `with-open-file')
(defmacro with-open-db ((var &rest options) &body forms)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY forms)
    `(LET ((,var (BDB:DB-OPEN ,@options)))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       (UNWIND-PROTECT
            (MULTIPLE-VALUE-PROG1 (PROGN ,@body-rest)
              (WHEN ,var (BDB:DB-CLOSE ,var) (setq ,var nil)))
         (WHEN ,var (BDB:DB-CLOSE ,var))))))

#| sample
 (bdb:db-version)
 (setq dbe (bdb:env-create))
 (bdb:env-set-options dbe :data-dir "/etc/setup/")
 (bdb:env-open dbe)
|#

;;; restore locks
(push "BDB" *system-package-list*)
(setf (package-lock *system-package-list*) t)
