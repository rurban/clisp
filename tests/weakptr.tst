;; -*- Lisp -*-

(progn (setq co (cons 1 2) wp (make-weak-pointer co))
       (multiple-value-list (weak-pointer-value wp)))
((1 . 2) T)

(progn (gc) (multiple-value-list (weak-pointer-value wp)))
((1 . 2) T)

(progn (setq co nil) (gc) (multiple-value-list (weak-pointer-value wp)))
(NIL NIL)

(progn (setq co (cons 1 2) wp (make-weak-pointer 1))
       (multiple-value-list (weak-pointer-value wp)))
(1 T)

(progn (setf (weak-pointer-value wp) co) (gc)
       (multiple-value-list (weak-pointer-value wp)))
((1 . 2) T)

(progn (setf (weak-pointer-value wp) 2) (gc)
       (multiple-value-list (weak-pointer-value wp)))
(2 T)

(progn (setf (weak-pointer-value wp) co) (gc)
       (multiple-value-list (weak-pointer-value wp)))
((1 . 2) T)

(progn (setf (weak-pointer-value wp) 3 co nil) (gc)
       (multiple-value-list (weak-pointer-value wp)))
(3 T)

(progn (setf (weak-pointer-value wp) (cons 1 2)) (gc)
       (multiple-value-list (weak-pointer-value wp)))
(NIL NIL)
