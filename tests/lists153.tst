;;; -*- Lisp -*-

;; crashes
(let ((z (make-list 5000000)))
  (gc) (mapcar #'null z) (gc) (setq z nil) (gc) nil)
NIL
