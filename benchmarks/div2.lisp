;;; DIV2 -- Benchmark which divides by 2 using lists of n ()'s.
;;; This file contains a recursive as well as an iterative test.

(defun create-n (n)
  (declare (type fixnum n))
  (do ((n n (the fixnum (1- n)))
       (a () (push () a)))
      ((= (the fixnum n) 0) a)
    (declare (type fixnum n))))

(defparameter **l** (create-n 200))

(defun iterative-div2 (l)
  (do ((l l (cddr l))
       (a () (push (car l) a)))
      ((null l) a)))

(defun recursive-div2 (l)
  (cond ((null l) ())
        (t (cons (car l) (recursive-div2 (cddr l))))))

(defun test-div2-iterative (&optional (l **l**))
  (do ((i 300 (the fixnum (1- i))))
      ((= (the fixnum i) 0))
    (declare (type fixnum i))
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)))

(defun test-div2-recursive (&optional (l **l**))
  (do ((i 300 (the fixnum (1- i))))
      ((= (the fixnum i) 0))
    (declare (type fixnum i))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))
