;; -*- Lisp -*-

(defmacro weakptr-test (&body body)
  `(progn (make-list 100) ,@body (make-array 200)
          (list (eq co (weak-pointer-value wp))
                (multiple-value-list (weak-pointer-value wp))
                (multiple-value-bind (v p) (weak-pointer-value wpp)
                  (list (type-of v) p)))))
weakptr-test

(weakptr-test (setq co (cons 1 2) wp (make-weak-pointer co)
                    wpp (make-weak-pointer wp)))
(T ((1 . 2) T) (WEAK-POINTER T))

(weakptr-test (gc))
(T ((1 . 2) T) (WEAK-POINTER T))

(weakptr-test (setq co nil) (gc))
(T (NIL NIL) (WEAK-POINTER T))

(weakptr-test (setq co (cons 1 2) wp (make-weak-pointer 1)))
(NIL (1 T) (WEAK-POINTER T))

(weakptr-test (setf (weak-pointer-value wp) co) (gc))
(T ((1 . 2) T) (NULL NIL))

(weakptr-test (setf (weak-pointer-value wp) 2 (weak-pointer-value wpp) co)
              (gc))
(NIL (2 T) (CONS T))

(weakptr-test (setf (weak-pointer-value wp) co (weak-pointer-value wpp) wp)
              (gc))
(T ((1 . 2) T) (WEAK-POINTER T))

(weakptr-test (setf (weak-pointer-value wp) 3 co nil) (gc))
(NIL (3 T) (WEAK-POINTER T))

(weakptr-test (setf (weak-pointer-value wp) (cons 1 2)) (gc))
(T (NIL NIL) (WEAK-POINTER T))

(let ((*print-circle* t))
  (setf (weak-pointer-value wp) wpp)
  (prin1-to-string wp))
"#1=#<WEAK-POINTER #<WEAK-POINTER #1#>>"

(progn (makunbound 'co) (makunbound 'wp) (makunbound 'wpp) (gc)
       (fmakunbound 'weakptr-test))
weakptr-test
