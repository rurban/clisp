;;; -*- Lisp -*-
;; test non-global special bindings

;; variable being declared special is bound
(let ((x 5)) (let ((x (1+ x))) (declare (special x)) x))
6
(let ((x 5)) (let* ((x (1+ x))) (declare (special x)) x))
6
(let ((x 5)) (multiple-value-bind (x) (1+ x) (declare (special x)) x))
6
(let ((x 5)) ((lambda (x) (declare (special x)) x)  (1+ x)))
6

;; variable being declared special is unbound
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

;; variable is not being declared special
(let ((x 5)) (let ((x (1+ x))) x))
6
(let ((x 5)) (let* ((x (1+ x))) x))
6
(let ((x 5)) (multiple-value-bind (x) (1+ x) x))
6
(let ((x 5)) ((lambda (x) x)  (1+ x)))
6
