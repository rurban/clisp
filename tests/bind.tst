;;; -*- Lisp -*-
;; test non-global special bindings

;; variable being declared special is bound - a "bound" declaration
(let ((x 5)) (let ((x (1+ x))) (declare (special x)) x))
6
(let ((x 5)) (let* ((x (1+ x))) (declare (special x)) x))
6
(let ((x 5)) (multiple-value-bind (x) (1+ x) (declare (special x)) x))
6
(let ((x 5)) ((lambda (x) (declare (special x)) x)  (1+ x)))
6

;; similarly, without enclosing lexical binding
(block foo
  (handler-bind ((unbound-variable
                  (lambda (c) (princ-error c) (return-from foo 'good))))
    (let ((x (1+ x))) (declare (special x)) x)))
GOOD
(block foo
  (handler-bind ((unbound-variable
                  (lambda (c) (princ-error c) (return-from foo 'good))))
    (let* ((x (1+ x))) (declare (special x)) x)))
GOOD
(block foo
  (handler-bind ((unbound-variable
                  (lambda (c) (princ-error c) (return-from foo 'good))))
    (multiple-value-bind (x) (1+ x) (declare (special x)) x)))
GOOD
(block foo
  (handler-bind ((unbound-variable
                  (lambda (c) (princ-error c) (return-from foo 'good))))
    ((lambda (x) (declare (special x)) x)  (1+ x))))
GOOD

;; variable being declared special is not being bound - a "free" declaration
(let ((x 5)) (let ((y (1+ x))) (declare (special x)) y))
6
(let ((x 5)) (let* ((y (1+ x))) (declare (special x)) y))
6
(let ((x 5)) (multiple-value-bind (y) (1+ x) (declare (special x)) y))
6
(let ((x 5)) ((lambda (y) (declare (special x)) y)  (1+ x)))
6

;; variable is not being declared special
(let ((x 5)) (let ((x (1+ x))) x))
6
(let ((x 5)) (let* ((x (1+ x))) x))
6
(let ((x 5)) (multiple-value-bind (x) (1+ x) x))
6
(let ((x 5)) ((lambda (x) x)  (1+ x)))
6

;; CLHS 3.3.4 - a "bound" declaration
(LET ((X 5))
  (PROGV '(X) '(20)
    (LET ((X (1+ X)) (Z (1+ X)))
      (DECLARE (SPECIAL X))
      Z)))
6
(LET ((X 5))
  (PROGV '(X) '(20)
    (LET* ((X (1+ X)) (Z (1+ X)))
      (DECLARE (SPECIAL X))
      Z)))
7
(LET ((X 5))
  (PROGV '(X) '(20)
    (MULTIPLE-VALUE-BIND (X Z) (VALUES (1+ X) (1+ X))
      (DECLARE (SPECIAL X))
      Z)))
6
(LET ((X 5))
  (PROGV '(X) '(20)
    ((LAMBDA (&OPTIONAL (X (1+ X)) (Z (1+ X)))
       (DECLARE (SPECIAL X))
       Z))))
7

;; same as above, with LOCALLY
(let ((x 5))
  (progv '(x y) '(20 120)
    (let ((x (1+ x)) (y (1+ x)) (z (1+ x)))
      (declare (special x))
      (list z (locally (declare (special y)) y) x y))))
(6 120 6 6)
(let ((x 5))
  (progv '(x y) '(20 120)
    (let* ((x (1+ x)) (y (1+ x)) (z (1+ x)))
      (declare (special x))
      (list z (locally (declare (special y)) y) x y))))
(7 120 6 7)
(let ((x 5))
  (progv '(x y) '(20 120)
    (multiple-value-bind (x y z) (values (1+ x) (1+ x) (1+ x))
      (declare (special x))
      (list z (locally (declare (special y)) y) x y))))
(6 120 6 6)
(let ((x 5))
  (progv '(x y) '(20 120)
    ((lambda (&optional (x (1+ x)) (y (1+ x)) (z (1+ x)))
       (declare (special x))
       (list z (locally (declare (special y)) y) x y)))))
(7 120 6 7)

;; CLHS 3.3.4 - a "free" declaration
(LET ((X 5))
  (PROGV '(X) '(20)
    (LET ((Y (1+ X)) (Z (1+ X)))
      (DECLARE (SPECIAL X))
      Z)))
6
(LET ((X 5))
  (PROGV '(X) '(20)
    (LET* ((Y (1+ X)) (Z (1+ X)))
      (DECLARE (SPECIAL X))
      Z)))
6
(LET ((X 5))
  (PROGV '(X) '(20)
    (MULTIPLE-VALUE-BIND (Y Z) (VALUES (1+ X) (1+ X))
      (DECLARE (SPECIAL X))
      Z)))
6
(LET ((X 5))
  (PROGV '(X) '(20)
    ((LAMBDA (&OPTIONAL (Y (1+ X)) (Z (1+ X)))
       (DECLARE (SPECIAL X))
       Z))))
6

;; global special variable with extra redundant special declaration
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (let ((*global-var-for-bind.tst* (1+ *global-var-for-bind.tst*)))
        (declare (special *global-var-for-bind.tst*))
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (let* ((*global-var-for-bind.tst* (1+ *global-var-for-bind.tst*)))
        (declare (special *global-var-for-bind.tst*))
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (multiple-value-bind (*global-var-for-bind.tst*)
          (1+ *global-var-for-bind.tst*)
        (declare (special *global-var-for-bind.tst*))
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      ((lambda (*global-var-for-bind.tst*)
         (declare (special *global-var-for-bind.tst*))
         *global-var-for-bind.tst*)
       (1+ *global-var-for-bind.tst*))
      *global-var-for-bind.tst*)))
(6 5)

;; global special variable without special declaration
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (let ((*global-var-for-bind.tst* (1+ *global-var-for-bind.tst*)))
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (let* ((*global-var-for-bind.tst* (1+ *global-var-for-bind.tst*)))
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (multiple-value-bind (*global-var-for-bind.tst*)
          (1+ *global-var-for-bind.tst*)
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      ((lambda (*global-var-for-bind.tst*)
         *global-var-for-bind.tst*)
       (1+ *global-var-for-bind.tst*))
      *global-var-for-bind.tst*)))
(6 5)
