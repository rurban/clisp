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

(let (cache)
  (defmethod slot-missing ((class t) (obj <C1>)
                           (slot-name t) (operation t)
                           &optional (new-value nil new-value-p))
    (setf cache
          (list slot-name operation new-value new-value-p)))
  (list (slot-boundp a 'abcd) cache
        (slot-value a 'abcd) cache))
(T (ABCD SLOT-BOUNDP NIL NIL)
 (ABCD SLOT-VALUE NIL NIL) (ABCD SLOT-VALUE NIL NIL))

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

(let* ((fn (defgeneric f (x y)
             (:method ((x t) (y t))
               (list x y))))
       (meth1 (defmethod f ((i integer) (j number))
                (+ i j)))
       (meth2 (defmethod f ((s1 string) (s2 string))
                (concatenate 'string s1 s2))))
  (lambda () (defmethod f ((x list) (y list)) (append x y)))
  (list (eq meth1 (find-method #'f nil (list (find-class 'integer)
                                             (find-class 'number))))
        (eq meth2 (find-method #'f nil (list (find-class 'string)
                                             (find-class 'string))))))
(T T)

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

(defparameter *hl* nil)
*HL*

(progn
(defgeneric hgen (x)
  (:method ((x integer))
    (setf *hl* (cons 'i-primary-1 *hl*))
    (call-next-method)
    (setf *hl* (cons 'i-primary-2 *hl*)))
  (:method :before ((x integer))
    (setf *hl* (cons 'i-before *hl*)))
  (:method :after ((x integer))
    (setf *hl* (cons 'i-after *hl*)))
  (:method :around ((x integer))
    (setf *hl* (cons 'i-around-1 *hl*))
    (call-next-method)
    (setf *hl* (cons 'i-around-2 *hl*)))
  (:method ((x number))
    (setf *hl* (cons 'n-primary-1 *hl*))
    (call-next-method)
    (setf *hl* (cons 'n-primary-2 *hl*)))
  (:method :before ((x number))
    (setf *hl* (cons 'n-before *hl*)))
  (:method :after ((x number))
    (setf *hl* (cons 'n-after *hl*)))
  (:method :around ((x number))
    (setf *hl* (cons 'n-around-1 *hl*))
    (call-next-method)
    (setf *hl* (cons 'n-around-2 *hl*)))
  (:method ((x t))
    (setf *hl* (cons 'innermost *hl*))))
(defun h (x)
  (setf *hl* '()) (hgen x) (reverse *hl*))
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

(let* ((c (defclass <C1> ()
            ((x :initform 0 :accessor x-val :initarg :x)
             (y :initform 1 :accessor y-val :initarg :y))))
       (m (defmethod initialize-instance :after ((instance <C1>)
                                                 &rest initvalues)
            (if (= (x-val instance) 0)
                (setf (x-val instance) (y-val instance))))))
  (eq m (find-method #'initialize-instance '(:after) (list c))))
T

(x-val (make-instance (find-class '<C1>)))
1

(x-val (make-instance (find-class '<C1>) :x 10))
10

(x-val (make-instance (find-class '<C1>) :y 20))
20

(x-val (make-instance (find-class '<C1>) :x 10 :y 20))
10

(let ((m (defmethod initialize-instance ((inst <C1>) &rest ignore)
           (call-next-method)
           123)))
  (eq m (find-method #'initialize-instance nil (list (find-class '<C1>)))))
T

(x-val (make-instance (find-class '<C1>) :x 101 :y 120))
101

(setf (find-class '<C1>) nil)
nil

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

(eq (class-of (make-array nil)) (find-class 'array))  T
(eq (class-of (make-array nil :element-type nil)) (find-class 'array)) T
(eq (class-of (make-array 10 :element-type nil)) (find-class 'vector)) T

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
(defun mlf-tester (symbol &optional (lisp-file "make-load-form-demo.lisp")
                   &aux (compiled-file (compile-file-pathname lisp-file)))
  (unwind-protect
       (progn
         (with-open-file (stream lisp-file :direction :output
                                 :if-exists :supersede)
           (format stream "(in-package ~s)~%(defparameter ~S '#.~S)~%"
                   (package-name (symbol-package symbol))
                   symbol symbol))
         (compile-file lisp-file)
         (setf (symbol-value symbol) nil)
         (load compiled-file)
         (symbol-value symbol))
    (delete-file compiled-file)
    (delete-file lisp-file)
    #+clisp (delete-file (make-pathname :type "lib" :defaults lisp-file))))
MLF-TESTER

(defun mlf-kill (type)
  (let ((m (find-method #'make-load-form nil (list (find-class type)) nil)))
    (when m (remove-method #'make-load-form m)))
  (setf (find-class type) nil))
mlf-kill

;; from kmp
(progn
  (defclass test-class1 () ((foo :initarg :foo :accessor foo :initform 0)))
  (defclass test-class2 () ((foo :initarg :foo :accessor foo :initform 0)))
  (defmethod make-load-form ((obj test-class1) &optional environment)
    (declare (ignore environment))
    `(make-instance 'test-class1 :foo ',(foo obj)))
  (defmethod make-load-form ((obj test-class2) &optional environment)
    (declare (ignore environment))
    `(make-instance 'test-class2 :foo ',(foo obj)))
  (defparameter *t-list*
    (list (make-instance 'test-class1 :foo 100)
          (make-instance 'test-class2 :foo 200)))
  (mlf-tester '*t-list*)
  (mapcar #'foo *t-list*))
(100 200)

;; form Christophe Rhodes <csr21@cam.ac.uk>
(defstruct foo a)
FOO

(progn
  (defmethod make-load-form ((x foo) &optional env)
    (make-load-form-saving-slots x :environment env))
  (defparameter *tmp-file* "mlf-tmp.lisp")
  (with-open-file (s *tmp-file* :direction :output)
    (format s "(defparameter *foo* '#S(FOO :A BAR-CONST))~%"))
  (load (compile-file *tmp-file*))
  *foo*)
#S(FOO :A BAR-CONST)

(progn
  (makunbound '*foo*)
  (defconstant bar-const 1)
  (unwind-protect (progn (load (compile-file *tmp-file*)) *foo*)
    (delete-file *tmp-file*)
    (delete-file (compile-file-pathname *tmp-file*))
    #+clisp (delete-file (make-pathname :type "lib" :defaults *tmp-file*))
    (mlf-kill 'foo)))
#S(FOO :A BAR-CONST)

;; <http://www.lisp.org/HyperSpec/Issues/iss215-writeup.html>
(progn
  (defclass pos ()
    ((x :initarg :x :reader pos-x)
     (y :initarg :y :reader pos-y)
     (r :accessor pos-r)))
  (defmethod shared-initialize :after ((self pos) ignore1 &rest ignore2)
    (declare (ignore ignore1 ignore2))
    (unless (slot-boundp self 'r)
      (setf (pos-r self) (sqrt (+ (* (pos-x self) (pos-x self))
                                  (* (pos-y self) (pos-y self)))))))
  (defmethod make-load-form ((self pos) &optional environment)
    (declare (ignore environment))
    `(make-instance ',(class-name (class-of self))
                    :x ',(pos-x self) :y ',(pos-y self)))
  (setq *foo* (make-instance 'pos :x 3.0 :y 4.0))
  (mlf-tester '*foo*)
  (list (pos-x *foo*) (pos-y *foo*) (pos-r *foo*)))
(3.0 4.0 5.0)

(progn
  (defclass tree-with-parent ()
    ((parent :accessor tree-parent)
     (children :initarg :children)))
  (defmethod make-load-form ((x tree-with-parent) &optional environment)
    (declare (ignore environment))
    (values
     ;; creation form
     `(make-instance ',(class-name (class-of x)))
     ;; initialization form
     `(setf (tree-parent ',x) ',(slot-value x 'parent)
            (slot-value ',x 'children) ',(slot-value x 'children))))
  (setq *foo* (make-instance 'tree-with-parent :children
                             (list (make-instance 'tree-with-parent
                                                  :children nil)
                                   (make-instance 'tree-with-parent
                                                  :children nil))))
  (setf (tree-parent *foo*) *foo*)
  (dolist (ch (slot-value *foo* 'children))
    (setf (tree-parent ch) *foo*))
  (mlf-tester '*foo*)
  (list (eq *foo* (tree-parent *foo*))
        (every (lambda (x) (eq x *foo*))
               (mapcar #'tree-parent (slot-value *foo* 'children)))
        (every #'null
               (mapcar (lambda (x) (slot-value x 'children))
                       (slot-value *foo* 'children)))))
(T T T)

;; <http://www.lisp.org/HyperSpec/Issues/iss237-writeup.html>
(progn
  (defparameter *initform-executed-counter* 0)
  (defstruct foo (slot-1 (incf *initform-executed-counter*)))
  (defparameter *foo* (make-foo)))
*FOO*
*foo*                           #S(FOO :SLOT-1 1)
*initform-executed-counter*     1
(progn
  (mapc #'eval (multiple-value-list (make-load-form-saving-slots *foo*)))
  *initform-executed-counter*)
1
(progn
  (defmethod print-object ((f foo) (o stream))
    (format o "~1t<~a>" (foo-slot-1 f)))
  (prin1-to-string (make-foo)))
" <2>"

(progn (mlf-kill 'foo) (defstruct foo slot))
FOO

;; From: Kaz Kylheku <kaz@ashi.footprints.net>
;; Date: Sat, 3 Jan 2004 14:47:25 -0800 (PST)
;; <http://article.gmane.org/gmane.lisp.clisp.general:7853>
;; this is insane but necessary:
;;   WITH-IGNORED-ERRORS in tests.lisp binds *ERROR-HANDLER* which
;;   disables CLCS processing and thus compiled output breaks in
;;   MAKE-INIT-FORM - NO-APPLICABLE-METHOD for MAKE-LOAD-FORM,
;;   therefore we have to bind *ERROR-HANDLER* to NIL ourselves here
(let* ((*ERROR-HANDLER* NIL)
       (file "foo.lisp") c)
  (unwind-protect
       (progn
         (makunbound '*foo*)
         (with-open-file (f file :direction :output)
           (format f "(defstruct foo slot)~@
                      (defparameter *foo* #.(make-foo))~%"))
         (load (setq c (compile-file file)))
         *foo*)
    (delete-file file)
    (delete-file c)
    #+clisp (delete-file (make-pathname :type "lib" :defaults file))))
#S(FOO :SLOT NIL)

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

;; <https://sourceforge.net/tracker/?func=detail&aid=869187&group_id=1355&atid=101355>
(progn
  (defclass c1 () ())
  (defclass c2 () ())
  (list
   (let ((c (make-instance 'c1)))
     (list (type-of (change-class c 'c2))
           (type-of (change-class c 'c1))))
   (let ((c (make-instance 'c1)))
     (list (type-of (change-class c 'c1))
           (type-of (change-class c 'c1))))))
((C2 C1) (C1 C1))

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
(ensure-generic-function 'egf-fun :lambda-list '(x y))
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

(progn
  (defclass class-0203 () ((a :allocation :class) (b :allocation :instance)))
  (defclass class-0204 (class-0203) (c d))
  (let ((c1 (make-instance 'class-0203)) (c2 (make-instance 'class-0204)))
    (list
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     (slot-boundp c2 'c) (slot-boundp c2 'd)
     (setf (slot-value c1 'a) 'x)
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     (slot-boundp c2 'c) (slot-boundp c2 'd)
     (slot-value c1 'a)
     (slot-value c2 'a)
     (eq (slot-makunbound c1 'a) c1)
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     (slot-boundp c2 'c) (slot-boundp c2 'd))))
(:bound nil nil nil nil nil nil
 x
 :bound t nil t nil nil nil
 x x
 t
 :bound nil nil nil nil nil nil)

(progn
  (defclass class-0206a () ((a :allocation :instance) (b :allocation :class)))
  (defclass class-0206b (class-0206a)
    ((a :allocation :class) (b :allocation :instance)))
  (let ((c1 (make-instance 'class-0206a)) (c2 (make-instance 'class-0206b)))
    (list
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     (setf (slot-value c1 'a) 'x)
     (setf (slot-value c1 'b) 'y)
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     :value-1
     (slot-value c1 'a) (slot-value c1 'b)
     (progn (slot-makunbound c1 'a)
            (slot-makunbound c1 'b)
            (setf (slot-value c2 'a) 'x))
     (setf (slot-value c2 'b) 'y)
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     :value-2
     (slot-value c2 'a) (slot-value c2 'b)
     (progn (slot-makunbound c2 'a)
            (slot-makunbound c2 'b)
            nil))))
(:bound nil nil nil nil
 x y
 :bound t t nil nil
 :value-1 x y
 x y
 :bound nil nil t t
 :value-2 x y
 nil)

(let* ((c (defclass reinit-class-01 ()
            ((a :initarg :a) (b :initarg :b))))
       (m (defmethod reinitialize-instance :after ((instance reinit-class-01)
                                                   &rest initargs
                                                   &key (x nil x-p))
            (declare (ignore initargs))
            (when x-p (setf (slot-value instance 'a) x))
            instance)))
  (eq m (find-method #'reinitialize-instance '(:after) (list c))))
T

(let* ((obj (make-instance 'reinit-class-01))
       (obj2 (reinitialize-instance obj :a 1 :b 3)))
  (list (eq obj obj2) (slot-value obj2 'a) (slot-value obj2 'b)))
(t 1 3)

(let* ((obj (make-instance 'reinit-class-01 :a 10 :b 20))
       (obj2 (reinitialize-instance obj :x 3)))
  (list (eq obj obj2) (slot-value obj2 'a) (slot-value obj2 'b)))
(t 3 20)

(let* ((obj (make-instance 'reinit-class-01 :a 10 :b 20))
       (obj2 (reinitialize-instance obj :x 3 :x 100)))
  (list (eq obj obj2) (slot-value obj2 'a) (slot-value obj2 'b)))
(t 3 20)

(let* ((obj (make-instance 'reinit-class-01 :a 10 :b 20))
       (obj2 (reinitialize-instance obj :x 3 :garbage 100)))
  (list (eq obj obj2) (slot-value obj2 'a) (slot-value obj2 'b)))
error

(let ((gf1 (defgeneric no-app-meth-gf-01 ()))
      (gf2 (defgeneric no-app-meth-gf-02 (x)))
      (gf3 (defgeneric no-app-meth-gf-03 (x y))))
  (defmethod no-applicable-method ((x (eql gf1)) &rest args)
    (list 'no-applicable-method args))
  (defmethod no-applicable-method ((x (eql gf2)) &rest args)
    (list 'no-applicable-method args))
  (defmethod no-applicable-method ((x (eql gf3)) &rest args)
    (list 'no-applicable-method args))
  (list (no-app-meth-gf-01)
        (no-app-meth-gf-02 (cons 'a 'b))
        (no-app-meth-gf-03 (cons 'a 'b) (cons 'c 'd))))
((NO-APPLICABLE-METHOD nil)
 (NO-APPLICABLE-METHOD ((A . B)))
 (NO-APPLICABLE-METHOD ((A . B) (C . D))))

(let ((gf1 (defgeneric no-prim-meth-gf-01 ()))
      (gf2 (defgeneric no-prim-meth-gf-02 (x)))
      (gf3 (defgeneric no-prim-meth-gf-03 (x y))))
  (defmethod no-prim-meth-gf-01 :around ()
    (list :around (call-next-method)))
  (defmethod no-primary-method ((x (eql gf1)) &rest args)
    (list 'no-primary-method args))
  (defmethod no-prim-meth-gf-02 :around ((x t))
    (list :around x (call-next-method)))
  (defmethod no-primary-method ((x (eql gf2)) &rest args)
    (list 'no-primary-method args))
  (defmethod no-prim-meth-gf-03 :around ((x t) (y t))
    (list :around x y (call-next-method)))
  (defmethod no-primary-method ((x (eql gf3)) &rest args)
    (list 'no-primary-method args))
  (list (no-prim-meth-gf-01)
        (no-prim-meth-gf-02 (cons 'a 'b))
        (no-prim-meth-gf-03 (cons 'a 'b) (cons 'c 'd))))
((NO-PRIMARY-METHOD nil)
 (NO-PRIMARY-METHOD ((A . B)))
 (NO-PRIMARY-METHOD ((A . B) (C . D))))


;;; method combinations
(progn
  (defgeneric test-mc-standard (x)
    (:method ((x string)) (cons 'string (call-next-method)))
    (:method ((x t)) x))
  (list (test-mc-standard 1)
        (test-mc-standard "a")))
(1 (STRING . "a"))

(progn
  (defgeneric test-mc-standard-bad-qualifiers (x y))
  (defmethod test-mc-standard-bad-qualifiers ((x integer) (y integer)) (+ x y))
  (defmethod test-mc-standard-bad-qualifiers ((x float) (y float)) (+ x y))
  (defmethod test-mc-standard-bad-qualifiers :beffor ((x float) (y float))
    (format t "x = ~S, y = ~S~%" x y)))
ERROR

(progn
  (defgeneric test-mc-progn (x s)
    (:method-combination progn)
    (:method progn ((x string) s) (vector-push-extend 'string s))
    (:method progn ((x t) s) (vector-push-extend 't s))
    (:method :around ((x number) s)
             (vector-push-extend 'number s) (call-next-method)))
  (list (let ((s (make-array 10 :adjustable t :fill-pointer 0)))
          (test-mc-progn 1 s)
          s)
        (let ((s (make-array 10 :adjustable t :fill-pointer 0)))
          (test-mc-progn "a" s)
          s)))
(#(NUMBER T) #(STRING T))

(progn
  (defun positive-integer-qualifier-p (method-qualifiers)
    (and (= (length method-qualifiers) 1)
         (typep (first method-qualifiers) '(integer 0 *))))
  (define-method-combination example-method-combination ()
    ((method-list positive-integer-qualifier-p))
    `(progn ,@(mapcar #'(lambda (method) `(call-method ,method))
                      (stable-sort method-list #'<
                                   :key #'(lambda (method)
                                            (first (method-qualifiers
                                                    method)))))))
  (defgeneric mc-test-piq (p1 p2 s)
    (:method-combination example-method-combination)
    (:method 1 ((p1 t) (p2 t) s) (vector-push-extend (list 1 p1 p2) s))
    (:method 2 ((p1 t) (p2 t) s) (vector-push-extend (list 2 p1 p2) s))
    (:method 3 ((p1 t) (p2 t) s) (vector-push-extend (list 3 p1 p2) s)))
  (let ((s (make-array 10 :adjustable t :fill-pointer 0)))
    (mc-test-piq 1 2 s)
    s))
#((1 1 2) (2 1 2) (3 1 2))

(progn
  (define-method-combination w-args ()
    ((method-list *))
    (:arguments arg1 arg2 &aux (extra :extra))
    `(progn ,@(mapcar #'(lambda (method) `(call-method ,method)) method-list)))
  (defgeneric mc-test-w-args (p1 p2 s)
    (:method-combination w-args)
    (:method ((p1 number) (p2 t) s)
      (vector-push-extend (list 'number p1 p2) s))
    (:method ((p1 string) (p2 t) s)
      (vector-push-extend (list 'string p1 p2) s))
    (:method ((p1 t) (p2 t) s) (vector-push-extend (list t p1 p2) s)))
  (let ((s (make-array 10 :adjustable t :fill-pointer 0)))
    (mc-test-w-args 1 2 s)
    s))
#((NUMBER 1 2) (T 1 2))
