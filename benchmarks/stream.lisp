;; Benchmark for Common Lisp
;; Bernhard Degel, Bruno Haible 1989

;; "Stream" = NIL or List (x1 x2 ... xn . y),
;; with n>0 and y either NIL or a function,
;; which returns a stream (xn+1 ...).

;; first element of a non-empty stream:
(defun stream-head (s) (car s))

;; rest of a non-empty stream:
(defun stream-tail (s)
  (if (listp (cdr s)) (cdr s) (setf (cdr s) (funcall (cdr s)))))

;; return non-empty stream with first element x and rest-list y,
(defmacro stream-cons (x y)
  `(cons ,x #'(lambda () ,y)))

;; return a stream with elements from s that satisfy pred
(defun stream-filter (pred s)
  (cond ((null s) nil)
        ((funcall pred (stream-head s))
         (stream-cons (stream-head s) (stream-filter pred (stream-tail s))))
        (t (stream-filter pred (stream-tail s)))))

;; return the n-th element (n>=0) of a stream.
(defun stream-nth (n s)
  (if (zerop n)
    (stream-head s)
    (stream-nth (1- n) (stream-tail s))))

;; return the stream of all integers from n
(defun integers (n)
  (stream-cons n (integers (1+ n))))

;; sieve out of a stream all elements that are divisible by the first element
(defun sieve (s)
  (if (null s)
    nil
    (let ((p (stream-head s)))
      (stream-cons
        p
        (stream-filter #'(lambda (n) (not (zerop (mod n p))))
                       (sieve (stream-tail s)))))))

;; return the n-th prime number (n>=1).
(defun nth-prime (n)
  (stream-nth (1- n) (sieve (integers 2))))

;;(time (nth-prime 10))
;;(time (nth-prime 20))
;;(time (nth-prime 50))

