;;; -*- Lisp -*-

;; crashes
#+:enable-risky-tests
(let ((z (make-list 10000000)))
  (gc) (setq z nil) (gc) nil)
#+:enable-risky-tests NIL
