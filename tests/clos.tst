;; -*- Lisp -*-

#-(or CMU SBCL)
(use-package "CLOS")
#-(or CMU SBCL)
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
(#+CMU (ABCD SLOT-BOUNDP NIL NIL) #-CMU T
 (ABCD SLOT-BOUNDP NIL NIL) (ABCD SLOT-VALUE NIL NIL) (ABCD SLOT-VALUE NIL NIL))

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

(eq (class-of 10)               (find-class #+(or ALLEGRO CMU SBCL) 'fixnum #-(or ALLEGRO CMU SBCL) 'integer))
T

(eq (class-of 10.0)             (find-class #+(or ALLEGRO CMU SBCL) 'single-float #-(or ALLEGRO CMU SBCL) 'float))
T

(eq (class-of '(a b))           (find-class 'cons))
T

(eq (class-of "abc")            (find-class #+CMU 'simple-string #+SBCL 'simple-base-string #-(or CMU SBCL) 'string))
T

(eq (class-of '#(1 2))          (find-class #+(or CMU SBCL) 'simple-vector #-(or CMU SBCL) 'vector))
T

(eq (class-of #'car)            (find-class 'function))
T

(eq (class-of #'make-instance)  (find-class 'standard-generic-function))
T

(eq (class-of '#2a((a) (b)))    (find-class #+(or CMU SBCL) 'simple-array #-(or CMU SBCL) 'array))
T

(eq (class-of *standard-input*) (find-class 'stream))
NIL

(eq (class-of (lambda (x) x))   (find-class 'function))
T

(eq (class-of (find-class 't)) (find-class 'built-in-class))
T

(eq (class-of (make-array nil)) (find-class #+(or CMU SBCL) 'simple-array #-(or CMU SBCL) 'array))  T
(eq (class-of (make-array nil :element-type nil)) (find-class #+(or CMU SBCL) 'simple-array #-(or CMU SBCL) 'array)) T
(eq (class-of (make-array 10 :element-type nil)) (find-class #+CMU 'simple-string #+SBCL 'sb-kernel::simple-array-nil #-(or CMU SBCL) 'string)) T

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
#+SBCL
(defun subclassp (class1 class2)
  (not (null (member (car (sb-pcl:class-precedence-list class2))
                     (sb-pcl:class-precedence-list class1)
) )    )     )
#+(or CLISP ALLEGRO CMU SBCL) SUBCLASSP

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
         (with-open-file (stream lisp-file :direction :output #+SBCL :if-exists #+SBCL :supersede)
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
  (with-open-file (s *tmp-file* :direction :output #+SBCL :if-exists #+SBCL :supersede)
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

#+SBCL (unintern 'foo) #+SBCL t
#+SBCL (unintern 'copy-foo) #+SBCL t
#+SBCL (unintern 'make-foo) #+SBCL t

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

(progn (mlf-kill 'foo) nil)
nil

#+SBCL (unintern 'foo) #+SBCL t
#+SBCL (unintern 'copy-foo) #+SBCL t
#+SBCL (unintern 'make-foo) #+SBCL t

(defstruct foo slot)
FOO

;; From: Kaz Kylheku <kaz@ashi.footprints.net>
;; Date: Sat, 3 Jan 2004 14:47:25 -0800 (PST)
;; <http://article.gmane.org/gmane.lisp.clisp.general:7853>
(let ((file "foo.lisp") c)
  (unwind-protect
       (progn
         (makunbound '*foo*)
         (with-open-file (f file :direction :output #+SBCL :if-exists #+SBCL :supersede)
           (format f "(eval-when (compile load eval) (defstruct foo slot))~@
                      (defparameter *foo* #.(make-foo))~%"))
         (load (setq c (compile-file file)))
         *foo*)
    (delete-file file)
    (delete-file c)
    #+clisp (delete-file (make-pathname :type "lib" :defaults file))))
#+CLISP #S(FOO :SLOT NIL)
#+(or CMU SBCL) ERROR
#-(or CLISP CMU SBCL) UNKNOWN

;; The direct-subclasses list must be weak.
#+clisp
(let (old1-weakpointers-count old-subclasses-count old2-weakpointers-count
      new-subclasses-count new-weakpointers-count)
  (defclass foo64a () ())
  (defclass foo64b (foo64a) ())
  (let ((usymbol (gensym)))
    (eval `(defclass ,usymbol (foo64a) ()))
    (setq old1-weakpointers-count (length (clos::class-direct-subclasses (find-class 'foo64a))))
    (setf (symbol-value usymbol) (1- (length (clos::list-all-subclasses (find-class 'foo64a)))))
    (setq old2-weakpointers-count (length (clos::class-direct-subclasses (find-class 'foo64a))))
    (setq old-subclasses-count (symbol-value usymbol)))
  (gc)
  (setq new-subclasses-count (1- (length (clos::list-all-subclasses (find-class 'foo64a)))))
  (setq new-weakpointers-count (length (clos::class-direct-subclasses (find-class 'foo64a))))
  (list old1-weakpointers-count old-subclasses-count old2-weakpointers-count
        new-subclasses-count new-weakpointers-count))
#+clisp
(2 2 2 1 1)

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
#+CLISP (FOO 2 0 BAR 1.4142135 0.7853981)
#+(or CMU SBCL) (FOO 2.0 0.0 BAR 1.4142135 0.7853982)
#-(or CLISP CMU SBCL) UNKNOWN

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

;; Check that a GC collects the forward pointer left over by change-class.
#+CLISP
(progn
  (defclass c3 () (a b c))
  (defclass c4 () (b c d e))
  (let* ((i (make-instance 'c3))
         (nslots-before (sys::%record-length i)))
    (change-class i 'c4)
    (gc)
    (< nslots-before (sys::%record-length i))))
#+CLISP
T
  
;; Redefining a finalized class must not change its identity.
(let (c1 c2)
  (defclass foo60-b () ())
  (defclass foo60-a (foo60-b) ())
  (make-instance 'foo60-b)
  (setq c1 (find-class 'foo60-a))
  (defclass foo60-a () ())
  (setq c2 (find-class 'foo60-a))
  (eq c1 c2))
T

;; Redefining a non-finalized class must not change its identity.
(let (c1 c2)
  (defclass foo61-a (foo61-b) ())
  (setq c1 (find-class 'foo61-a))
  (defclass foo61-a () ())
  (setq c2 (find-class 'foo61-a))
  (eq c1 c2))
T

;; SUBTYPEP must work on finalized classes.
(progn
  (defclass foo62-b (foo62-a) ())
  (defclass foo62-c (foo62-b) ())
  (defclass foo62-a () ())
  (make-instance 'foo62-c)
  (list (subtypep 'foo62-b 'foo62-b)
        (subtypep 'foo62-c 'foo62-b)
        (subtypep 'foo62-b 'foo62-c)))
(T T NIL)

;; SUBTYPEP must work on non-finalized classes.
(progn
  (defclass foo63-b (foo63-a) ())
  (defclass foo63-c (foo63-b) ())
  (defclass foo63-a () ())
  (list (subtypep 'foo63-b 'foo63-b)
        (subtypep 'foo63-c 'foo63-b)
        (subtypep 'foo63-b 'foo63-c)))
(T T NIL)

;; Redefining a class can make it (and also its subclasses) non-finalized.
#+CLISP
(let (fa fb fc)
  (defclass foo65a () ())
  (defclass foo65b (foo65a) ())
  (defclass foo65c (foo65b) ())
  (setq fa (clos:class-finalized-p (find-class 'foo65a))
        fb (clos:class-finalized-p (find-class 'foo65b))
        fc (clos:class-finalized-p (find-class 'foo65c)))
  (defclass foo65b (foo65a foo65other) ())
  (list fa fb fc
        (clos:class-finalized-p (find-class 'foo65a))
        (clos:class-finalized-p (find-class 'foo65b))
        (clos:class-finalized-p (find-class 'foo65c))))
#+CLISP
(T T T T NIL NIL)

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


;; 4.3.6. Redefining Classes

;; Newly added local slot.
;; 4.3.6.1.: "Local slots specified by the new class definition that are not
;;            specified as either local or shared by the old class are added."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo70 () ())
      (setq i (make-instance 'foo70))
      (defclass foo70 () ((size :initarg :size :initform 1) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(1 NULL)

;; Newly added shared slot.
;; 4.3.6.: "Newly added shared slots are initialized."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo71 () ())
      (setq i (make-instance 'foo71))
      (defclass foo71 () ((size :initarg :size :initform 1 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(1 NULL)

;; Discarded local slot.
;; 4.3.6.1.: "Slots not specified as either local or shared by the new class
;;            definition that are specified as local by the old class are
;;            discarded."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo72 () ((size :initarg :size :initform 1)))
      (setq i (make-instance 'foo72 :size 5))
      (defclass foo72 () ((other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(NIL SIMPLE-ERROR)

;; Discarded shared slot.
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo73 () ((size :initarg :size :initform 1 :allocation :class)))
      (setq i (make-instance 'foo73))
      (defclass foo73 () ((other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(NIL SIMPLE-ERROR)

;; Shared slot remains shared.
;; 4.3.6.: "The value of a slot that is specified as shared both in the old
;;          class and in the new class is retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo74 () ((size :initarg :size :initform 1 :allocation :class)))
      (setq i (make-instance 'foo74))
      (defclass foo74 () ((size :initarg :size :initform 2 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(1 NULL)

;; Shared slot becomes local.
;; 4.3.6.1.: "The value of a slot that is specified as shared in the old class
;;            and as local in the new class is retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo75 () ((size :initarg :size :initform 1 :allocation :class)))
      (setq i (make-instance 'foo75))
      (defclass foo75 () ((size :initarg :size :initform 2) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(1 NULL)

;; Local slot remains local.
;; 4.3.6.1.: "The values of local slots specified by both the new and old
;;            classes are retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo76 () ((size :initarg :size :initform 1)))
      (setq i (make-instance 'foo76 :size 5))
      (defclass foo76 () ((size :initarg :size :initform 2) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(5 NULL)

;; Local slot becomes shared.
;; 4.3.6.: "Slots that were local in the old class and that are shared in the
;;          new class are initialized."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo77 () ((size :initarg :size :initform 1)))
      (setq i (make-instance 'foo77 :size 5))
      (defclass foo77 () ((size :initarg :size :initform 2 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(2 NULL)


;; Redefining the superclass of an instance

;; Newly added local slot.
;; 4.3.6.1.: "Local slots specified by the new class definition that are not
;;            specified as either local or shared by the old class are added."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo80a () ())
      (defclass foo80b (foo80a) ())
      (setq i (make-instance 'foo80b))
      (defclass foo80a () ((size :initarg :size :initform 1) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(1 NULL)

;; Newly added shared slot.
;; 4.3.6.: "Newly added shared slots are initialized."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo81a () ())
      (defclass foo81b (foo81a) ())
      (setq i (make-instance 'foo81b))
      (defclass foo81a () ((size :initarg :size :initform 1 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(1 NULL)

;; Discarded local slot.
;; 4.3.6.1.: "Slots not specified as either local or shared by the new class
;;            definition that are specified as local by the old class are
;;            discarded."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo82a () ((size :initarg :size :initform 1)))
      (defclass foo82b (foo82a) ())
      (setq i (make-instance 'foo82b :size 5))
      (defclass foo82a () ((other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(NIL SIMPLE-ERROR)

;; Discarded shared slot.
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo83a () ((size :initarg :size :initform 1 :allocation :class)))
      (defclass foo83b (foo83a) ())
      (setq i (make-instance 'foo83b))
      (defclass foo83a () ((other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(NIL SIMPLE-ERROR)

;; Shared slot remains shared.
;; 4.3.6.: "The value of a slot that is specified as shared both in the old
;;          class and in the new class is retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo84a () ((size :initarg :size :initform 1 :allocation :class)))
      (defclass foo84b (foo84a) ())
      (setq i (make-instance 'foo84b))
      (defclass foo84a () ((size :initarg :size :initform 2 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(1 NULL)

;; Shared slot becomes local.
;; 4.3.6.1.: "The value of a slot that is specified as shared in the old class
;;            and as local in the new class is retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo85a () ((size :initarg :size :initform 1 :allocation :class)))
      (defclass foo85b (foo85a) ())
      (setq i (make-instance 'foo85b))
      (defclass foo85a () ((size :initarg :size :initform 2) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(1 NULL)

;; Local slot remains local.
;; 4.3.6.1.: "The values of local slots specified by both the new and old
;;            classes are retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo86a () ((size :initarg :size :initform 1)))
      (defclass foo86b (foo86a) ())
      (setq i (make-instance 'foo86b :size 5))
      (defclass foo86a () ((size :initarg :size :initform 2) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(5 NULL)

;; Local slot becomes shared.
;; 4.3.6.: "Slots that were local in the old class and that are shared in the
;;          new class are initialized."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo87a () ((size :initarg :size :initform 1)))
      (defclass foo87b (foo87a) ())
      (setq i (make-instance 'foo87b :size 5))
      (defclass foo87a () ((size :initarg :size :initform 2 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (type-of condition)))
(2 NULL)


;; The clos::list-direct-subclasses function lists only non-finalized direct
;; subclasses.
#+CLISP
(progn
  (defclass foo88b (foo88a) ((s :initarg :s)))
  (defclass foo88c (b) ())
  (defclass foo88a () ())
  ; Here foo88a is finalized, foo88b and foo88c are not.
  (list
    (length (clos::list-direct-subclasses (find-class 'foo88a)))
    (length (clos::list-direct-subclasses (find-class 'foo88b)))
    (length (clos::list-direct-subclasses (find-class 'foo88c)))))
#+CLISP
(0 0 0)
#+CLISP
(progn
  (defclass foo89b (foo89a) ((s :initarg :s)))
  (defclass foo89c (b) ())
  (defclass foo89a () ())
  (let ((x (make-instance 'foo89b :s 5)))
    ; Here foo89a and foo89b are finalized, foo89c is not.
    (list
      (length (clos::list-direct-subclasses (find-class 'foo89a)))
      (length (clos::list-direct-subclasses (find-class 'foo89b)))
      (length (clos::list-direct-subclasses (find-class 'foo89c))))))
#+CLISP
(1 0 0)

;; The clos::list-direct-subclasses function must notice when a finalized
;; direct subclass is redefined in such a way that it is no longer a subclass.
#+CLISP
(progn
  (defclass foo90b (foo90a) ((s :initarg :s)))
  (defclass foo90c (foo90b) ())
  (defclass foo90a () ())
  (let ((x (make-instance 'foo90b :s 5)))
    ; Here foo90a and foo90b are finalized, foo90c is not.
    (defclass foo90b () (s))
    ; Now foo90b is no longer direct subclass of foo90a.
    (list
      (length (clos::list-direct-subclasses (find-class 'foo90a)))
      (length (clos::list-direct-subclasses (find-class 'foo90b)))
      (length (clos::list-direct-subclasses (find-class 'foo90c))))))
#+CLISP
(0 0 0)

;; The clos::list-direct-subclasses function must notice when a finalized
;; direct subclass is redefined in such a way that it is no longer finalized.
#+CLISP
(progn
  (defclass foo91a () ())
  (defclass foo91b (foo91a) ())
  (defclass foo91c (foo91b) ())
  (defclass foo91b (foo91a foo91other) ())
  (list
    (length (clos::list-direct-subclasses (find-class 'foo91a)))
    (length (clos::list-direct-subclasses (find-class 'foo91b)))
    (length (clos::list-direct-subclasses (find-class 'foo91c)))))
#+CLISP
(0 0 0)

;; make-instances-obsolete causes update-instance-for-redefined-class to
;; be called on instances of current subclasses.
(progn
  (defclass foo92b (foo92a) ((s :initarg :s)))
  (defclass foo92a () ())
  (let ((x (make-instance 'foo92b :s 5)) (update-counter 0))
    (defclass foo92b (foo92a) ((s) (s1) (s2))) ; still subclass of foo92a
    (slot-value x 's)
    (defmethod update-instance-for-redefined-class ((object foo92b) added-slots discarded-slots property-list &rest initargs)
      (incf update-counter))
    (make-instances-obsolete 'foo92a)
    (slot-value x 's)
    update-counter))
1

;; make-instances-obsolete does not cause update-instance-for-redefined-class
;; to be called on instances of ancient subclasses.
(progn
  (defclass foo93b (foo93a) ((s :initarg :s)))
  (defclass foo93a () ())
  (let ((x (make-instance 'foo93b :s 5)) (update-counter 0))
    (defclass foo93b () ((s) (s1) (s2))) ; no longer a subclass of foo93a
    (slot-value x 's)
    (defmethod update-instance-for-redefined-class ((object foo93b) added-slots discarded-slots property-list &rest initargs)
      (incf update-counter))
    (make-instances-obsolete 'foo93a)
    (slot-value x 's)
    update-counter))
0

;; Redefining a class removes the slot accessors installed on behalf of the
;; old class.
(progn
  (defclass foo94 () ((a :reader foo94-get-a :writer foo94-set-a)
                      (b :reader foo94-get-b :writer foo94-set-b)
                      (c :accessor foo94-c)
                      (d :accessor foo94-d)
                      (e :accessor foo94-e)))
  (list* (not (null (find-method #'foo94-get-a '() (list (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-set-a '() (list (find-class 't) (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-get-b '() (list (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-set-b '() (list (find-class 't) (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-c '() (list (find-class 'foo94)) nil)))
         (not (null (find-method #'(setf foo94-c) '() (list (find-class 't) (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-d '() (list (find-class 'foo94)) nil)))
         (not (null (find-method #'(setf foo94-d) '() (list (find-class 't) (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-e '() (list (find-class 'foo94)) nil)))
         (not (null (find-method #'(setf foo94-e) '() (list (find-class 't) (find-class 'foo94)) nil)))
         (progn
           (defclass foo94 () ((a :reader foo94-get-a :writer foo94-set-a)
                               (b)
                               (c :accessor foo94-c)
                               (e :accessor foo94-other-e)))
           (list (not (null (find-method #'foo94-get-a '() (list (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-set-a '() (list (find-class 't) (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-get-b '() (list (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-set-b '() (list (find-class 't) (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-c '() (list (find-class 'foo94)) nil)))
                 (not (null (find-method #'(setf foo94-c) '() (list (find-class 't) (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-d '() (list (find-class 'foo94)) nil)))
                 (not (null (find-method #'(setf foo94-d) '() (list (find-class 't) (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-e '() (list (find-class 'foo94)) nil)))
                 (not (null (find-method #'(setf foo94-e) '() (list (find-class 't) (find-class 'foo94)) nil)))))))
(T T T T T T T T T T
 T T NIL NIL T T NIL NIL NIL NIL)

;; It is possible to redefine a class in a way that makes it non-finalized,
;; if it was not yet instantiated. Fetching the class-prototype doesn't count
;; as an instantiation.
(progn
  (defclass foo95b () ((s :initarg :s :accessor foo95b-s)))
  (class-prototype (find-class 'foo95b))
  (defclass foo95b (foo95a) ((s :accessor foo95b-s)))
  t)
T

;; When redefining a class in a way that makes it non-finalized, and it was
;; already instantiated, an error is signalled, and the instances survive it.
(let ((notes '()))
  (flet ((note (o) (setq notes (append notes (list o)))))
    (defclass foo96b () ((s :initarg :s :accessor foo96b-s)))
    (let ((x (make-instance 'foo96b :s 5)))
      (note (foo96b-s x))
      (note
        (type-of
          (second
            (multiple-value-list
              (ignore-errors
                (defclass foo96b (foo96a) ((s :accessor foo96b-s))))))))
      (note (foo96b-s x))
      (note (slot-value x 's))
      (defclass foo96a () ((r :accessor foo96b-r)))
      (note (foo96b-s x))
      (note (slot-value x 's))
      (note (subtypep 'foo96b 'foo96a))
      notes)))
(5 SIMPLE-ERROR 5 5 5 5 NIL)
(let ((notes '()))
  (flet ((note (o) (setq notes (append notes (list o)))))
    (defclass foo97b () ((s :initarg :s :accessor foo97b-s)))
    (let ((x (make-instance 'foo97b :s 5)))
      (note (foo97b-s x))
      (note
        (type-of
          (second
            (multiple-value-list
              (ignore-errors
                (defclass foo97b (foo97a) ((s :accessor foo97b-s))))))))
      (note (foo97b-s x))
      (note (slot-value x 's))
      (defclass foo97a () ((r :accessor foo97b-r)))
      (note (foo97b-s x))
      (note (slot-value x 's))
      (note (subtypep 'foo97b 'foo97a))
      notes)))
(5 SIMPLE-ERROR 5 5 5 5 NIL)


;; Test the :fixed-slot-location option.

; Single class.
(progn
  (defclass foo100 () (a b c) (:fixed-slot-locations))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo100))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(a b c)))
(1 2 3)

; Simple subclass.
(progn
  (defclass foo101a () (a b c) (:fixed-slot-locations))
  (defclass foo101b (foo101a) (d e f) (:fixed-slot-locations))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo101b))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(a b c d e f)))
(1 2 3 4 5 6)

; Subclass with multiple inheritance.
(progn
  (defclass foo102a () (a b c) (:fixed-slot-locations))
  (defclass foo102b () (d e f))
  (defclass foo102c (foo102a foo102b) (g h i))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo102c))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(a b c d e f g h i)))
(1 2 3 4 5 6 7 8 9)

; Subclass with multiple inheritance.
(progn
  (defclass foo103a () (a b c))
  (defclass foo103b () (d e f) (:fixed-slot-locations))
  (defclass foo103c (foo103a foo103b) (g h i))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo103c))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(a b c d e f g h i)))
(4 5 6 1 2 3 7 8 9)

; Subclass with multiple inheritance and collision.
(progn
  (defclass foo104a () (a b c) (:fixed-slot-locations))
  (defclass foo104b () (d e f) (:fixed-slot-locations))
  (defclass foo104c (foo104a foo104b) (g h i))
  t)
ERROR

; Subclass with multiple inheritance and no collision.
(progn
  (defclass foo105a () (a b c) (:fixed-slot-locations))
  (defclass foo105b () () (:fixed-slot-locations))
  (defclass foo105c (foo105a foo105b) (g h i))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo105c))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(a b c g h i)))
(1 2 3 4 5 6)

; Subclass with multiple inheritance and no collision.
(progn
  (defclass foo106a () () (:fixed-slot-locations))
  (defclass foo106b () (d e f) (:fixed-slot-locations))
  (defclass foo106c (foo106a foo106b) (g h i))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo106c))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(d e f g h i)))
(1 2 3 4 5 6)

; Subclass with shared slots.
(progn
  (defclass foo107a ()
    ((a :allocation :instance)
     (b :allocation :instance)
     (c :allocation :class)
     (d :allocation :class)
     (e :allocation :class))
    (:fixed-slot-locations))
  (defclass foo107b (foo107a)
    ((b :allocation :class)))
  t)
ERROR

; Subclass with shared slots and no collision.
(progn
  (defclass foo108a ()
    ((a :allocation :instance)
     (b :allocation :instance)
     (c :allocation :class)
     (d :allocation :class)
     (e :allocation :class))
    (:fixed-slot-locations))
  (defclass foo108b (foo108a)
    (; (b :allocation :class) ; gives error, see above
     (c :allocation :instance)
     (d :allocation :class)
     (f :allocation :instance)
     (g :allocation :class)))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo108b))
                                :key #'clos:slot-definition-name)))
                (let ((location (clos:slot-definition-location slot)))
                  (if (consp location)
                    (class-name (clos::cv-newest-class (car location)))
                    location))))
          '(a b c d e f g)))
(1 2 3 foo108b foo108a 4 foo108b)


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

#+CLISP
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
#+CLISP
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
    (format t "x = ~S, y = ~S~%" x y))
  t)
#+(or CLISP CMU) ERROR #+SBCL T #-(or CLISP CMU SBCL) UNKNOWN

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
