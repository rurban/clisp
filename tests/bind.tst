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

;; interaction with global specials
(progn (defvar *global-var-for-bind.tst* 123)
 (let ((*global-var-for-bind.tst* 5))
   (let ((*global-var-for-bind.tst* (1+ *global-var-for-bind.tst*)))
     (declare (special *global-var-for-bind.tst*))
     *global-var-for-bind.tst*)))
6
(progn (defvar *global-var-for-bind.tst* 123)
 (let ((*global-var-for-bind.tst* 5))
   (let* ((*global-var-for-bind.tst* (1+ *global-var-for-bind.tst*)))
     (declare (special *global-var-for-bind.tst*))
     *global-var-for-bind.tst*)))
6
(progn (defvar *global-var-for-bind.tst* 123)
 (let ((*global-var-for-bind.tst* 5))
   (multiple-value-bind (*global-var-for-bind.tst*)
       (1+ *global-var-for-bind.tst*)
     (declare (special *global-var-for-bind.tst*))
     *global-var-for-bind.tst*)))
6
(progn (defvar *global-var-for-bind.tst* 123)
 (let ((*global-var-for-bind.tst* 5))
   ((lambda (*global-var-for-bind.tst*)
      (declare (special *global-var-for-bind.tst*))
      *global-var-for-bind.tst*)
    (1+ *global-var-for-bind.tst*))))
6
