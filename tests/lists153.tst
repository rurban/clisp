;;; -*- Lisp -*-

;; crashes
(unless (and (= (logand (sys::address-of nil) #xffffff) 0) ; SPVW_PURE_BLOCKS ?
             (<= (integer-length most-positive-fixnum) 26)) ; 32-bit machine ?
  (let ((z (make-list 5000000)))
    (gc) (mapcar #'null z) (gc) (setq z nil) (gc) nil))
NIL
