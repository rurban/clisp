;-*-mode:lisp-*-
; a fib benchmark that uses binary arithmetic.
; 16-DEC-92 George Carrette. GJC@MITECH.COM
; Wanted a benchmark that avoided built-in numbers
; and that did more work.
;
; Benchmark: (b->number (b-fib '(A A B A B))) => 6765
;
; Note: (b->number '(A A B A B)) => 20
;
; (C) Copyright 1992 George Carrette. As described in SLIB.C

(defun b-fib (x)
  (if (b< x '(A B))
      x
      (b+ (b-fib (b- x '(B)))
          (b-fib (b- x '(A B))))))

(defun number->b (n)
  (if (zerop n)
      '()
      (cons (if (oddp n) 'B 'A)
            (number->b (floor n 2)))))

(defun b->number (x)
  (if (null x)
      0
    (+ (if (eq (car x) 'B) 1 0)
       (* 2 (b->number (cdr x))))))

(defun b+ (x y)
  (if (null x)
      y
    (if (null y)
        x
      (if (eq (car x) 'A)
          (cons (car y) (b+ (cdr x) (cdr y)))
        (if (eq (car y) 'A)
            (cons 'B (b+ (cdr x) (cdr y)))
          (cons 'A (b+ (b+ '(B) (cdr x)) (cdr y))))))))

(defun b- (x y)
  (if (null y)
      x
    (if (null x)
        (error "negative number")
      (if (eq (car y) 'A)
          (bA-cons (car x) (b- (cdr x) (cdr y)))
        (if (eq (car x) 'B)
            (bA-cons (if (eq (car y) 'B) 'A 'B) (b- (cdr x) (cdr y)))
          (cons 'B (b- (b- (cdr x) '(B)) (cdr y))))))))

(defun bA-cons (x l)
  (if (and (null l) (eq x 'A))
      '()
    (cons x l)))

(defun b< (x y)
  (and (not (null y))
       (or (null x)
           (b< (cdr x) (cdr y))
           (and (equal (cdr x) (cdr y))
                (eq (car x) 'A)
                (eq (car y) 'B)))))

(defun bfib-test ()
  (b->number (b-fib '(A A B A B))))
