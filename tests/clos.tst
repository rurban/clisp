;; -*- Lisp -*-

#-CMU
(use-package "CLOS")
#-CMU
T

(unintern '<C1>)
T

(progn
(defclass <C1> ()
  ((x :initform 0 :accessor x-val :reader get-x :writer set-x :initarg :x)
   (y :initform 1 :accessor y-val :reader get-y :writer set-y :initarg :y)))
())
NIL

(progn
(defclass <C2> (<C1>)
  ((z :initform 0 :accessor z-val :reader get-z :writer set-z :initarg :z)))
())
NIL

(defparameter a (make-instance (find-class '<C1>) :x 10))
A

(x-val a)
10

(y-val a)
1

(setf (x-val a) 20)
20

(x-val a)
20

(get-x a)
20

(set-x 10 a)
10

(x-val a)
10

(with-slots (x y) a (+ x y))
11

(defun foo (z) (with-slots (x y) z (+ x y)))
foo

(foo a)
11

(compile 'foo)
foo

(foo a)
11

(fmakunbound 'foo)
foo

(x-val (reinitialize-instance a :x 20))
20

(x-val (reinitialize-instance a :x 30))
30

(x-val (reinitialize-instance a :x 50))
50

(x-val (reinitialize-instance a :x 80))
80

(x-val (reinitialize-instance a :y 20))
80

(y-val (reinitialize-instance a :x 30))
20

(x-val (reinitialize-instance a :y 50))
30

(y-val (reinitialize-instance a :x 80))
50

(defparameter b (make-instance (find-class '<C2>) :x 10 :y 20 :z 30))
B

(x-val b)
10

(y-val b)
20

(z-val b)
30

(progn
(defgeneric f (x y)
  (:method ((x t) (y t))
    (list x y)))
(defmethod f ((i integer) (j number))
  (+ i j))
(defmethod f ((s1 string) (s2 string))
  (concatenate 'string s1 s2))
(lambda () (defmethod f ((x list) (y list)) (append x y)))
())
NIL

(f t t)
(T T)

(f 2 3)
5

(f 2 3.0)
5.0

(f 2.0 3)
(2.0 3)

(f "ab" "cd")
"abcd"

(f 1 "abc")
(1 "abc")

(progn
(defgeneric f (x y)
  (:method ((x t) (y t))
    (list x y))
  (:method ((i number) (j integer))
    (list (call-next-method) (- i j)))
  (:method ((i integer) (j number))
    (list (call-next-method) (+ i j))))
())
NIL

(f 'x 'y)
(X Y)

(f 1 2)
(((1 2) -1) 3)

(f 1 2.0)
((1 2.0) 3.0)

(f 1.0 2)
((1.0 2) -1.0)

(progn
(defgeneric g (x)
  (:method ((x null))
    (cons 'null (call-next-method)))
  (:method ((x list))
    (if (next-method-p) (cons 'list (call-next-method)) '(list$)))
  (:method ((x symbol))
    (if (next-method-p) (cons 'symbol (call-next-method)) '(symbol$))))
())
NIL

(g 'x)
(SYMBOL$)

(g '(x))
(LIST$)

(g '())
(NULL SYMBOL LIST$)

(defvar hl)
HL

(progn
(defgeneric hgen (x)
  (:method ((x integer))
    (setf hl (cons 'i-primary-1 hl))
    (call-next-method)
    (setf hl (cons 'i-primary-2 hl)))
  (:method :before ((x integer))
    (setf hl (cons 'i-before hl)))
  (:method :after ((x integer))
    (setf hl (cons 'i-after hl)))
  (:method :around ((x integer))
    (setf hl (cons 'i-around-1 hl))
    (call-next-method)
    (setf hl (cons 'i-around-2 hl)))
  (:method ((x number))
    (setf hl (cons 'n-primary-1 hl))
    (call-next-method)
    (setf hl (cons 'n-primary-2 hl)))
  (:method :before ((x number))
    (setf hl (cons 'n-before hl)))
  (:method :after ((x number))
    (setf hl (cons 'n-after hl)))
  (:method :around ((x number))
    (setf hl (cons 'n-around-1 hl))
    (call-next-method)
    (setf hl (cons 'n-around-2 hl)))
  (:method ((x t))
    (setf hl (cons 'innermost hl))))
(defun h (x)
  (setf hl '()) (hgen x) (reverse hl))
)
H

(h 'abc)
(INNERMOST)

(h 3.14)
(N-AROUND-1 N-BEFORE N-PRIMARY-1 INNERMOST N-PRIMARY-2 N-AFTER N-AROUND-2)

(h 3)
(I-AROUND-1 N-AROUND-1 I-BEFORE N-BEFORE I-PRIMARY-1 N-PRIMARY-1 INNERMOST
  N-PRIMARY-2 I-PRIMARY-2 N-AFTER I-AFTER N-AROUND-2 I-AROUND-2
)

(unintern '<C1>)
T

(progn
(defclass <C1> ()
  ((x :initform 0 :accessor x-val :initarg :x)
   (y :initform 1 :accessor y-val :initarg :y)))
())
NIL

(defparameter a (make-instance (find-class '<C1>) :x 10))
A

(defparameter b (make-instance (find-class '<C1>) :y 20 :x 10))
B

(defparameter c (make-instance (find-class '<C1>)))
C

(x-val a)
10

(y-val a)
1

(x-val b)
10

(y-val b)
20

(x-val c)
0

(y-val c)
1

(unintern '<C1>)
T

(progn
(defclass <C1> ()
  ((x :initform 0 :accessor x-val :initarg :x)
   (y :initform 1 :accessor y-val :initarg :y)))
(defmethod initialize-instance :after ((instance <C1>) &rest initvalues)
  (if (= (x-val instance) 0)
    (setf (x-val instance) (y-val instance))))
())
NIL

(x-val (make-instance (find-class '<C1>)))
1

(x-val (make-instance (find-class '<C1>) :x 10))
10

(x-val (make-instance (find-class '<C1>) :y 20))
20

(x-val (make-instance (find-class '<C1>) :x 10 :y 20))
10

(progn
(defmethod initialize-instance ((inst <C1>) &rest ignore)
  (call-next-method)
  123)
nil)
nil

(x-val (make-instance (find-class '<C1>) :x 101 :y 120))
101

(unintern '<C1>)
T

(eq (class-of ())               (find-class 'null))
T

(eq (class-of t)                (find-class 'symbol))
T

(eq (class-of 10)               (find-class #+(or ALLEGRO CMU) 'fixnum #-(or ALLEGRO CMU) 'integer))
T

(eq (class-of 10.0)             (find-class #+(or ALLEGRO CMU) 'single-float #-(or ALLEGRO CMU) 'float))
T

(eq (class-of '(a b))           (find-class 'cons))
T

(eq (class-of "abc")            (find-class #+CMU 'simple-string #-CMU 'string))
T

(eq (class-of '#(1 2))          (find-class #+CMU 'simple-vector #-CMU 'vector))
T

(eq (class-of #'car)            (find-class 'function))
T

(eq (class-of #'make-instance)  (find-class 'standard-generic-function))
T

(eq (class-of '#2a((a) (b)))    (find-class #+CMU 'simple-array #-CMU 'array))
T

(eq (class-of *standard-input*) (find-class 'stream))
NIL

(eq (class-of (lambda (x) x))   (find-class 'function))
T

(eq (class-of (find-class 't)) (find-class 'built-in-class))
T

(typep "abc" (find-class 't))
T

(typep "abc" (find-class 'array))
T

(typep "abc" (find-class 'vector))
T

(typep "abc" (find-class 'string))
T

(typep "abc" (find-class 'integer))
NIL

(typep 3 (find-class 't))
T

(typep 3 (find-class 'number))
T

(typep 3 (find-class 'float))
NIL

(typep 3 (find-class 'integer))
T

(typep 3 (find-class 'string))
NIL

(typep *standard-input* (find-class 'stream))
T

#+CLISP
(defun subclassp (class1 class2)
  (clos::subclassp class1 class2)
)
#+ALLEGRO
(defun subclassp (class1 class2)
  (finalize-inheritance class1)
  (not (null (member class2 (class-precedence-list class1))))
)
#+CMU
(defun subclassp (class1 class2)
  (not (null (member (car (pcl:class-precedence-list class2))
                     (pcl:class-precedence-list class1)
) )    )     )
#+(or CLISP ALLEGRO CMU) SUBCLASSP

(subclassp (find-class 'number)           (find-class 't))
T

(subclassp (find-class 'integer)          (find-class 'number))
T

(subclassp (find-class 'float)            (find-class 'number))
T

;; make-load-form
;; from kmp
(progn
  (defclass test-class1 () ((foo :initarg :foo :accessor foo :initform 0)))
  (defclass test-class2 () ((foo :initarg :foo :accessor foo :initform 0)))
  (defmethod make-load-form ((obj test-class1) &optional environment)
    `(make-instance 'test-class1 :foo ',(foo obj)))
  (defparameter *t-list*
    (list (make-instance 'test-class1 :foo 100)
          (make-instance 'test-class2 :foo 200)))
  (let* ((lisp-file "make-load-form-demo.lisp")
         (compiled-file
          (compile-file
           (with-open-file (stream lisp-file :direction :output
                                   :if-exists :supersede)
             (format stream "(in-package \"CL-USER\")~
                             ~%(defparameter *t-list* '#.*t-list*)~%")
             (truename stream)))))
    (setq *t-list* '())
    (load compiled-file)
    (delete-file compiled-file)
    (delete-file lisp-file)
    #+clisp (delete-file (make-pathname :type "lib" :defaults lisp-file))
    (mapcar #'foo *t-list*)))
(100 200)

;; change-class
;; <http://www.lisp.org/HyperSpec/Body/stagenfun_change-class.html>
(progn
  (defclass position () ())
  (defclass x-y-position (position)
    ((name :initarg :name)
     (x :initform 0 :initarg :x)
     (y :initform 0 :initarg :y)))
  (defclass rho-theta-position (position)
    ((name :initarg :name)
     (rho :initform 0)
     (theta :initform 0)))
  (defmethod update-instance-for-different-class :before
      ((old x-y-position) (new rho-theta-position) &key)
    ;; Copy the position information from old to new to make new
    ;; be a rho-theta-position at the same position as old.
    (let ((x (slot-value old 'x))
          (y (slot-value old 'y)))
      (setf (slot-value new 'rho) (sqrt (+ (* x x) (* y y)))
            (slot-value new 'theta) (atan y x))))
  (setq p1 (make-instance 'x-y-position :name 'foo :x 2 :y 0)
        p2 (make-instance 'x-y-position :name 'bar :x 1 :y 1))
  (change-class p1 'rho-theta-position)
  (change-class p2 'rho-theta-position)
  (list (slot-value p1 'name) (slot-value p1 'rho) (slot-value p1 'theta)
        (slot-value p2 'name) (slot-value p2 'rho) (slot-value p2 'theta)))
(FOO 2 0 BAR 1.4142135 0.7853981)

(progn
  (defclass c0 () (a b c))
  (defclass c1 () (b c a))
  (setq i (make-instance 'c0))
  (setf (slot-value i 'a) 1 (slot-value i 'b) 2 (slot-value i 'c) 3)
  (change-class i 'c1)
  (list (slot-value i 'a) (slot-value i 'b) (slot-value i 'c)))
(1 2 3)

;; update-instance-for-redefined-class
;; <http://www.lisp.org/HyperSpec/Body/stagenfun_upd_efined-class.html>
(progn
  (defclass position () ())
  (defclass x-y-position (position)
    ((x :initform 0 :accessor position-x)
     (y :initform 0 :accessor position-y)))
  (setf i (make-instance 'x-y-position)
        (position-x i) 1d0
        (position-y i) 1d0)
  (type-of i))
x-y-position

(progn
  ;; It turns out polar coordinates are used more than Cartesian
  ;; coordinates, so the representation is altered and some new
  ;; accessor methods are added.
  (defmethod update-instance-for-redefined-class :before
      ((pos x-y-position) added deleted plist &key)
    ;; Transform the x-y coordinates to polar coordinates
    ;; and store into the new slots.
    (let ((x (getf plist 'x))
          (y (getf plist 'y)))
      (setf (position-rho pos) (sqrt (+ (* x x) (* y y)))
            (position-theta pos) (atan y x))))
  (defclass x-y-position (position)
    ((rho :initform 0 :accessor position-rho)
     (theta :initform 0 :accessor position-theta)))
  ;; All instances of the old x-y-position class will be updated
  ;; automatically.
  ;; The new representation is given the look and feel of the old one.
  (defmethod position-x ((pos x-y-position))
    (with-slots (rho theta) pos (* rho (cos theta))))
  (defmethod (setf position-x) (new-x (pos x-y-position))
    (with-slots (rho theta) pos
      (let ((y (position-y pos)))
        (setq rho (sqrt (+ (* new-x new-x) (* y y)))
              theta (atan y new-x))
        new-x)))
  (defmethod position-y ((pos x-y-position))
    (with-slots (rho theta) pos (* rho (sin theta))))
  (defmethod (setf position-y) (new-y (pos x-y-position))
    (with-slots (rho theta) pos
      (let ((x (position-x pos)))
        (setq rho (sqrt (+ (* x x) (* new-y new-y)))
              theta (atan new-y x))
        new-y)))
  (list (type-of i) (position-x i) (position-y i)
        (position-rho i) (position-theta i)))
(X-Y-POSITION 1.0000000000000002d0 1.0d0
              1.4142135623730951d0 0.7853981633974483d0)

;;; ensure-generic-function
;;; <http://www.lisp.org/HyperSpec/Body/fun_ensure-ge_ric-function.html>
(ensure-generic-function 'car) error
(ensure-generic-function 'defclass) error
(ensure-generic-function 'tagbody) error

(let ((f 'egf-fun))
  (when (fboundp f) (fmakunbound f))
  (list
   (fboundp f)
   (typep (ensure-generic-function f) 'generic-function)
   (typep (ensure-generic-function f) 'generic-function)
   (typep (symbol-function f) 'generic-function)))
(nil t t t)

(let ((f 'egf-fun))
  (when (fboundp f) (fmakunbound f))
  (list
   (fboundp f)
   (typep (ensure-generic-function f :lambda-list '(a b c))
          'generic-function)
   ;; Test of incongruent generic function lambda list when no
   ;; methods exist
   (typep (ensure-generic-function f :lambda-list '(x y))
          'generic-function)
   (typep (symbol-function f) 'generic-function)))
(nil t t t)

(let ((f 'egf-fun))
  (when (fboundp f) (fmakunbound f))
  (list
   (fboundp f)
   (typep (ensure-generic-function f :lambda-list '(a b c))
          'generic-function)
   (typep (eval `(defmethod ,f ((a t)(b t)(c t)) (list a b c)))
          'standard-method)))
(nil t t)

;; Test of incongruent generic function lambda list when
;; some methods do exist
(ensure-generic-function egf-fun :lambda-list '(x y))
error

;; forward reference (GCL ansi test)
(let ((c1 (gensym)) (c2 (gensym)))
  (let ((class1 (eval `(defclass ,c1 (,c2) nil))))
    (if (not (typep class1 'class))
        1
        (let ((class2 (eval `(defclass ,c2 nil nil))))
          (if (not (typep class2 'class))
              2
              (let ((i1 (make-instance c1))
                    (i2 (make-instance c2)))
                (cond
                  ((not (typep i1 c1))     3)
                  ((not (typep i1 class1)) 4)
                  ((not (typep i1 c2))     5)
                  ((not (typep i1 class2)) 6)
                  ((typep i2 c1)           7)
                  ((typep i2 class1)       8)
                  ((not (typep i2 c2))     9)
                  ((not (typep i2 class2)) 10)
                  (t 'good))))))))
good

(let ((c1 (gensym)) (c2 (gensym)) (c3 (gensym)))
  (let ((class1 (eval `(defclass ,c1 (,c2 ,c3) nil))))
    (if (not (typep class1 'class))
        1
        (let ((class2 (eval `(defclass ,c2 nil nil))))
          (if (not (typep class2 'class))
              2
              (let ((class3 (eval `(defclass ,c3 nil nil))))
                (if (not (typep class3 'class))
                    3
                    (let ((i1 (make-instance c1))
                          (i2 (make-instance c2))
                          (i3 (make-instance c3)))
                      (cond
                        ((not (typep i1 c1))     4)
                        ((not (typep i1 class1)) 5)
                        ((not (typep i1 c2))     6)
                        ((not (typep i1 class2)) 7)
                        ((not (typep i1 c3))     8)
                        ((not (typep i1 class3)) 9)
                        ((typep i2 c1)           10)
                        ((typep i2 class1)       11)
                        ((typep i3 c1)           12)
                        ((typep i3 class1)       13)
                        ((not (typep i2 c2))     14)
                        ((not (typep i2 class2)) 15)
                        ((not (typep i3 c3))     16)
                        ((not (typep i3 class3)) 17)
                        ((typep i2 c3)           18)
                        ((typep i2 class3)       19)
                        ((typep i3 c2)           20)
                        ((typep i3 class2)       21)
                        (t 'good))))))))))
good

(let ((c1 (gensym)) (c2 (gensym)) (c3 (gensym)))
  (let ((class1 (eval `(defclass ,c1 (,c2) nil))))
    (if (not (typep class1 'class))
        1
        (let ((class2 (eval `(defclass ,c2 (,c3) nil))))
          (if (not (typep class2 'class))
              2
              (let ((class3 (eval `(defclass ,c3 nil nil))))
                (if (not (typep class3 'class))
                    3
                    (let ((i1 (make-instance c1))
                          (i2 (make-instance c2))
                          (i3 (make-instance c3)))
                      (cond
                        ((not (typep i1 c1))     4)
                        ((not (typep i1 class1)) 5)
                        ((not (typep i1 c2))     6)
                        ((not (typep i1 class2)) 7)
                        ((not (typep i1 c3))     8)
                        ((not (typep i1 class3)) 9)
                        ((typep i2 c1)           10)
                        ((typep i2 class1)       11)
                        ((typep i3 c1)           12)
                        ((typep i3 class1)       13)
                        ((not (typep i2 c2))     14)
                        ((not (typep i2 class2)) 15)
                        ((not (typep i3 c3))     16)
                        ((not (typep i3 class3)) 17)
                        ((not (typep i2 c3))     18)
                        ((not (typep i2 class3)) 19)
                        ((typep i3 c2)           20)
                        ((typep i3 class2)       21)
                        (t 'good))))))))))
good

(block nil
  (let ((c1 (gensym)) (c2 (gensym)) (c3 (gensym)) (c4 (gensym)) (c5 (gensym)))
    (unless (typep (eval `(defclass ,c4 nil nil)) 'class)
      (return 1))
    (unless (typep (eval `(defclass ,c5 nil nil)) 'class)
      (return 2))
    (unless (typep (eval `(defclass ,c1 (,c2 ,c3) nil)) 'class)
      (return 3))
    (unless (typep (eval `(defclass ,c2 (,c4 ,c5) nil)) 'class)
      (return 4))
    (eval `(progn
             (defclass ,c3 (,c5 ,c4) nil)
             (make-instance ',c1)))))
error
