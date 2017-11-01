;;; -*- Lisp -*- vim:filetype=lisp

#+LISPWORKS
(progn
  (defun gc () (mark-and-sweep 3))
  t)
#+LISPWORKS
T

;; This test allocates 10 million cons cells.
(unless #-CLISP nil
        #+CLISP
        (and (search " TYPECODES " (software-type)) ; TYPECODES ?
             (<= (integer-length most-positive-fixnum) 26)) ; 32-bit machine ?
  (let ((z (make-list 5000000)))
    (gc) (mapcar #'null z) (gc) (setq z nil) (gc) nil))
NIL
