;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(in-package "LISP")
(export
 '(dir-key-single-value with-dir-key-open dir-key-copy
   with-dir-key-search dir-key-dump-tree
   dir-key-info))

;;; utilities

(defmacro with-dir-key-open ((var key subkey &rest opts) &body body)
  `(let ((,var (dir-key-open ,key ,subkey ,@opts)))
    (unwind-protect (progn ,@body)
      (dir-key-close ,var))))

(defun dir-key-single-value (type path name)
  (with-dir-key-open (dk type path)
    (dir-key-value dk name)))

(defun dir-key-copy (dkey)
  (dir-key-open (dir-key-type dkey) (dir-key-path dkey)
                :direction (dir-key-direction dkey)))

;;; iterations

(defmacro with-dir-key-search ((key-iter att-iter dkey path
                                &key (scope :level))
                               &body body)
  (unless (symbolp key-iter)
    (error (ENGLISH "~S: macro name should be a symbol, not ~S")
           'with-dir-key-search key-iter))
  (let ((k-it (gensym "WDKS-")))
    `(let ((,k-it (sys::dkey-search-iterator ,dkey ,path ,scope)))
      (macrolet ((,key-iter () '(sys::dkey-search-next-key ,k-it)) .
                 ,(if att-iter
                      `((,att-iter () '(sys::dkey-search-next-att ,k-it)))))
        ,@body))))

;; the following two functions are re-implementations of
;; `dir-key-attributes' and `dir-key-subkeys' respectively, using
;; `with-dir-key-search'.
(defun dir-key-values (dkey path)
  (with-dir-key-search (k-iter a-iter dkey path :scope :self)
    (let ((kk (k-iter)) vals)
      (loop (multiple-value-bind (att val) (a-iter)
              (unless att (return))
              (push (cons att val) vals)))
    (cons kk (nreverse vals)))))

(defun dir-key-children (dkey path)
  (with-dir-key-search (k-iter nil dkey path :scope :level)
    (do* ((kk (k-iter) (k-iter)) res)
         ((null kk) (nreverse res))
      (push kk res))))

(defun dir-key-dump-tree (dkey path &key (out *standard-output*) (collect t))
  "Dump the whole subtree to OUT.
If collect is non-nil, collect all the keys into an a-list."
  (with-dir-key-search (k-iter v-iter dkey path :scope :tree)
    (do ((kk (k-iter) (k-iter)) keys (vals nil nil))
        ((null kk) (nreverse keys))
      (when out (format out "~%[~s]~2%" kk))
      (loop (multiple-value-bind (att val) (v-iter)
              (unless att (return))
              (when collect (push (cons att val) vals))
              (when out (format out "~s=~s~%" att val))))
      (when out (terpri out))
      (when collect (push (cons kk vals) keys)))))

;;; info

(defstruct dir-key-info
  type path
  class-name
  n-sub-keys max-sub-key-len max-sub-key-class-len
  n-values max-value-name-len max-value-data-len
  security
  write-time)

(defun dir-key-info (dkey)
  (multiple-value-bind
        (class-name n-sub-keys max-sub-key-len max-sub-key-class-len
         n-values max-value-name-len max-value-data-len
         security write-time)
      (sys::dkey-info dkey)
    (make-dir-key-info
     :type (dir-key-type dkey) :path (dir-key-path dkey) :class-name class-name
     :n-sub-keys n-sub-keys :max-sub-key-len max-sub-key-len
     :max-sub-key-class-len max-sub-key-class-len
     :n-values n-values :max-value-name-len max-value-name-len
     :max-value-data-len max-value-data-len
     :security security :write-time write-time)))
