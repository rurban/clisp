;; -*- Lisp -*-
;; test the macro functions; chapter 8
;; -----------------------------------


;; 8.1
;macro-function | defmacro


(and (macro-function 'push) T)
T

(and (macro-function 'member) T)
NIL

(defmacro arithmetic-if (test neg-form zero-form pos-form)
  (let ((var (gensym)))
    `(let ((,var ,test))
       (cond ((< ,var 0) ,neg-form)
             ((= ,var 0) ,zero-form)
             (T ,pos-form)))))
arithmetic-if


(and (macro-function 'arithmetic-if) T)
T

(setf x 8)
8

(arithmetic-if (- x 4)(- x)(LIST "ZERO") x)
8


(setf x 4)
4

(arithmetic-if (- x 4)(- x)(LIST "ZERO")x)
("ZERO")

(setf x 3)
3

(arithmetic-if (- x 4)(- x)(LIST "ZERO")x)
-3

(defmacro arithmetic-if (test neg-form &optional zero-form pos-form)
  (let ((var (gensym)))
    `(let ((,var ,test))
       (cond ((< ,var 0) ,neg-form)
             ((= ,var 0) ,zero-form)
             (T ,pos-form)))))
arithmetic-if

(setf x 8)
8

(arithmetic-if (- x 4)(- x))
nil

(setf x 4)
4

(arithmetic-if (- x 4)(- x))
NIL

(setf x 3)
3

(arithmetic-if (- x 4)(- x))
-3

(defmacro halibut ((mouth eye1 eye2)
                   ((fin1 length1)(fin2 length2))
                   tail)
  `(list ,mouth ,eye1 ,eye2 ,fin1 ,length1 ,fin2 ,length2 ,tail))
halibut

(setf m 'red-mouth
      eyes '(left-eye . right-eye)
      f1 '(1 2 3 4 5)
      f2 '(6 7 8 9 0)
      my-favorite-tail '(list of all parts of tail))
(list of all parts of tail)

(halibut (m (car eyes)(cdr eyes))
         ((f1 (length f1))(f2 (length f2)))
         my-favorite-tail)
(RED-MOUTH LEFT-EYE RIGHT-EYE (1 2 3 4 5) 5 (6 7 8 9 0) 5
(LIST OF ALL PARTS OF TAIL))

;; 8.2
; macroexpand | macroexpand-1

(ecase 'otherwise
  (otherwise 4))
4

;; Issue MACRO-FUNCTION-ENVIRONMENT:YES
(macrolet ((foo (&environment env)
             (if (macro-function 'bar env)
                 ''yes
                 ''no)))
  (list (foo)
        (macrolet ((bar () :beep))
          (foo))))
(no yes)

;; 3.2.2.1 Compiler Macros
(define-compiler-macro testp () '(progn 2))
TESTP

(defun testp () 'B)
TESTP

(defun test11 () (testp))
TEST11

(test11)
B

(compile 'test11)
TEST11

(test11)
2

(define-compiler-macro testc () ''A)
testc

(defun testc () 'b)
testc

(defun test6 () (testc))
test6

(test6)
B

(compile 'test6)
test6

(test6)
A

(define-compiler-macro testw () ''#(a 3))
testw

(defun testw () 'b)
testw

(defun test9 () (testw))
test9

(test9)
B

(compile 'test9)
test9

(test9)
#(a 3)

(define-compiler-macro testf () '(FUNCTION print))
testf

(defun testf () 'b)
testf

(defun test10 () (testf))
test10

(test10)
B

(compile 'test10)
test10

(test10)
#.#'print

(define-compiler-macro testp () '(progn (print 'a) 2))
testp

(defun testp () 'b)
testp

(defun test11 () (testp))
test11

(test11)
B

(compile 'test11)
test11

(test11)
2

;; http://sf.net/tracker/index.php?func=detail&aid=550864&group_id=1355&atid=101355
(defun test12 ()
  (flet ((test12-o ()
           (flet ((test12-i () (return-from test12 nil)))
             (test12-i))))
    (test12-o)))
test12

(test12)
nil

(compile 'test12)
test12

(test12)
nil

;; a crash compiling sbcl, reported by Christophe Rhodes
;; (Corrupted STACK in #<COMPILED-CLOSURE STEM> at byte 45)
;; the bug was fixed by bruno in compiler.lisp 1.80
(progn
  (defun stem (&key (obj (error "missing OBJ")))
    (with-open-file (stream obj :direction :output)
      (truename stream)))
  (compile 'stem)
  (delete-file (stem :obj "foo-bar-zot"))
  t)
t

;; bug in compiled repeated keywords
;; fixed by sds in compiler.lisp 1.92
(defparameter x 1)
x

(defun test-key () (find 1 #(0 1 2 3) :test #'= :test (incf x)))
test-key

(test-key)
1

x
2

(compile 'test-key)
test-key

(test-key)
1

x
3

(destructuring-bind ((a &optional (b 'bee)) one two three)
    `((alpha) 1 2 3)
  (list a b three two one))
(ALPHA BEE 3 2 1)

(defmacro m (&key (x x)) `,x)
m

(m)
3

(destructuring-bind (&key (x x)) nil x)
3

(destructuring-bind (x . y) '(1 . 10) (list x y))
(1 10)

(destructuring-bind (&whole (a  . b) c . d) '(1 . 2) (list a b c d))
(1 2 1 2)

(macrolet ((%m (&whole (m a b) c d) `'(,m ,a ,b ,c ,d))) (%m 1 2))
(%M 1 2 1 2)

(macrolet ((%m (&key ((:a (b c)))) `'(,c ,b))) (%m :a (1 2)))
(2 1)

(macrolet ((%m (&key ((:a (b c)) '(3 4))) `'(,c ,b)))
  (list (%m :a (1 2)) (%m :a (1 2) :a (10 11)) (%m)))
((2 1) (2 1) (4 3))

(macrolet ((%m (&key ((:a (b c)) '(3 4) a-p)) `'(,a-p ,c ,b)))
 (list (%m :a (1 2)) (%m :a (1 2) :a (10 11)) (%m)))
((T 2 1) (T 2 1) (NIL 4 3))

(macrolet ((%m (&key a b c) `'(,a ,b ,c)))
  (list (%m :allow-other-keys nil)
        (%m :a 1 :allow-other-keys nil)
        (%m :allow-other-keys t)
        (%m :allow-other-keys t :allow-other-keys nil :foo t)
        (%m :allow-other-keys t :c 1 :b 2 :a 3)
        (%m :c 1 :b 2 :a 3 :allow-other-keys t)
        (%m :allow-other-keys nil :c 1 :b 2 :a 3)))
((NIL NIL NIL) (1 NIL NIL) (NIL NIL NIL) (NIL NIL NIL) (3 2 1) (3 2 1) (3 2 1))

;;; <http://www.lisp.org/HyperSpec/Body/fun_macroexpa_acroexpand-1.html>
(defmacro alpha (x y) `(beta ,x ,y))   ALPHA
(defmacro beta (x y) `(gamma ,x ,y))   BETA
(defmacro delta (x y) `(gamma ,x ,y))  DELTA
(defmacro mexpand (form &environment env)
  (multiple-value-bind (expansion expanded-p)
      (macroexpand form env)
    `(list ',expansion ',expanded-p)))
MEXPAND
(defmacro mexpand-1 (form &environment env)
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1 form env)
    `(list ',expansion ',expanded-p)))
MEXPAND-1
(defun fexpand (form &optional env)
  (multiple-value-list (macroexpand form env)))
FEXPAND
(defun fexpand-1 (form &optional env)
  (multiple-value-list (macroexpand-1 form env)))
FEXPAND-1

;; Simple examples involving just the global environment
(fexpand-1 '(alpha a b))
((BETA A B) T)
(mexpand-1 (alpha a b))
((BETA A B) T)
(fexpand '(alpha a b))
((GAMMA A B) T)
(mexpand (alpha a b))
((GAMMA A B) T)
(fexpand-1 'not-a-macro)
(NOT-A-MACRO NIL)
(mexpand-1 not-a-macro)
(NOT-A-MACRO NIL)
(fexpand '(not-a-macro a b))
((NOT-A-MACRO A B) NIL)
(mexpand (not-a-macro a b))
((NOT-A-MACRO A B) NIL)

;; Examples involving lexical environments
(macrolet ((alpha (x y) `(delta ,x ,y)))
  (fexpand-1 '(alpha a b)))
((BETA A B) T)
(macrolet ((alpha (x y) `(delta ,x ,y)))
  (mexpand-1 (alpha a b)))
((DELTA A B) T)
(macrolet ((alpha (x y) `(delta ,x ,y)))
  (fexpand '(alpha a b)))
((GAMMA A B) T)
(macrolet ((alpha (x y) `(delta ,x ,y)))
  (mexpand (alpha a b)))
((GAMMA A B) T)
(macrolet ((beta (x y) `(epsilon ,x ,y)))
  (mexpand (alpha a b)))
((EPSILON A B) T)
(let ((x (list 1 2 3)))
  (symbol-macrolet ((a-sm (first x)))
    (mexpand a-sm)))
((FIRST X) T)
(let ((x (list 1 2 3)))
  (symbol-macrolet ((a-sm (first x)))
    (fexpand 'a-sm)))
(A-SM NIL)
(symbol-macrolet ((b-sm (alpha x y)))
  (mexpand-1 b-sm))
((ALPHA X Y) T)
(symbol-macrolet ((b-sm (alpha x y)))
  (mexpand b-sm))
((GAMMA X Y) T)
(symbol-macrolet ((b-sm (alpha x y))
                  (a-sm b-sm))
  (mexpand-1 a-sm))
(B-SM T)
(symbol-macrolet ((b-sm (alpha x y))
                  (a-sm b-sm))
  (mexpand a-sm))
((GAMMA X Y) T)

;; Examples of shadowing behavior
(flet ((beta (x y) (+ x y)))
  (mexpand (alpha a b)))
((BETA A B) T)
(macrolet ((alpha (x y) `(delta ,x ,y)))
  (flet ((alpha (x y) (+ x y)))
    (mexpand (alpha a b))))
((ALPHA A B) NIL)
(let ((x (list 1 2 3)))
  (symbol-macrolet ((a-sm (first x)))
    (let ((a-sm x))
      (mexpand a-sm))))
(A-SM NIL)

;; <http://www.lisp.org/HyperSpec/Body/speope_fletcm_scm_macrolet.html>
;; The macro-expansion functions defined by macrolet are defined in the
;; lexical environment in which the macrolet form appears
(symbol-macrolet ((foo 12))
  (macrolet ((bar (x) `(+ ,x ,(1+ foo))))
    (bar 10)))
23

(symbol-macrolet ((foo 12))
  (macrolet ((bar (x) (+ x foo)))
    (bar 10)))
22

(let ((f (gensym "FUNC-")) (a (gensym "A-")) (b (gensym "B-")))
  (eval
   `(defun ,f ()
      (let ((,a 1) (,b 2))
        (symbol-macrolet ((,a 5))
          (symbol-macrolet ((,b ,a)) ,b)))))
  (funcall f))
5

;; <https://sourceforge.net/tracker/index.php?func=detail&aid=678194&group_id=1355&atid=101355>
(defvar *my-typeof-counter* 0)
*my-typeof-counter*
(defmacro my-typeof (place &environment env)
  (let ((exp-place (macroexpand place env)))
    (unless (and (consp exp-place) (eq (car exp-place) 'FOREIGN-VALUE))
      (error "MY-TYPEOF not upon a place: ~S" exp-place))
    (incf *my-typeof-counter*)
    (second exp-place)))
my-typeof

(defmacro with-var ((var fvar) &body body)
  (let ((fv (gensym (symbol-name var))))
    `(LET ((,fv ,fvar))
       (SYMBOL-MACROLET ((,var (FOREIGN-VALUE ,fv)))
         ,@body))))
with-var

(with-var (my-var "fake variable") (my-typeof my-var))
"fake variable"

*my-typeof-counter*
1

(funcall (lambda ()
           (declare (compile))
           (with-var (my-var "fake variable")
             (my-typeof my-var))))
"fake variable"

*my-typeof-counter*
2

;; from Christophe Rhodes <csr21@cam.ac.uk>
(defmacro my-mac (&optional (x (error "missing arg"))
                  &key (y (error "missing arg")))
  `'(,x ,y))
MY-MAC
(my-mac 1 :y 10)           (1 10)
(defmacro my-mac (&key (b t)) (if b 'c 'd)) MY-MAC
(macroexpand '(my-mac))        C
(macroexpand '(my-mac :b nil)) D
(defmacro my-mac (&key (a t b)) `(,a ,b))   MY-MAC
(macroexpand '(my-mac :a 1))   (1 T)
(macroexpand '(my-mac))        (T NIL)

;; <http://www.lisp.org/HyperSpec/Body/mac_defmacro.html>
(defmacro dm1a (&whole x) `',x) dm1a
(macroexpand '(dm1a))           '(DM1A)

(defmacro dm1b (&whole x a &optional b) `'(,x ,a ,b)) dm1b
(macroexpand '(dm1b q))   '((DM1B Q) Q NIL)
(macroexpand '(dm1b q r)) '((DM1B Q R) Q R)

(defmacro dm2a (&whole form a b) `'(form ,form a ,a b ,b)) dm2a
(macroexpand '(dm2a x y)) '(FORM (DM2A X Y) A X B Y)
(dm2a x y)                (FORM (DM2A X Y) A X B Y)

(defmacro dm2b (&whole form a (&whole b (c . d) &optional (e 5))
                &body f &environment env)
  ``(,',form ,,a ,',b ,',(macroexpand c env) ,',d ,',e ,',f))
dm2b
(dm2b :x1 (((incf x2) x3 x4)) x5 x6)
((DM2B :X1 (((INCF X2) X3 X4)) X5 X6) :X1 (((INCF X2) X3 X4))
 (SETQ X2 (+ X2 1)) (X3 X4) 5 (X5 X6))

(let ((x1 5))
  (macrolet ((segundo (x) `(cadr ,x)))
    (dm2b x1 (((segundo x2) x3 x4)) x5 x6)))
((DM2B X1 (((SEGUNDO X2) X3 X4)) X5 X6)
 5 (((SEGUNDO X2) X3 X4)) (CADR X2) (X3 X4) 5 (X5 X6))

;; -C test
;; <http://article.gmane.org/gmane.lisp.clisp.general/7393>
#+CLISP
(loop :for a :in
  (funcall
   (sys::compile-form-in-toplevel-environment
    '(list (list #'equal 2 2) (list #'equal 2 3))))
  :collect (funcall (car a) (cadr a) (caddr a)))
#+CLISP (T NIL)

#+CLISP
(progn
  (defclass t1 () ((foo :accessor foo :initform :foo)))
  (list
   (funcall
    (compile nil (lambda () (typep (make-instance 't1) 't1))))
   (funcall
    (sys::compile-form-in-toplevel-environment
     '(typep (make-instance 't1) 't1)))))
#+CLISP (T T)

(progn
  ;; the first definition of NOTINLINE-TEST-FUNC-1 is side-effect-free,
  ;; so the compiler could have eliminated the call to it in
  ;; NOTINLINE-TEST-FUNC-2,
  ;; except that the NOTINLINE declaration should prevent that
  (declaim (notinline notinline-test-func-1))
  (defun notinline-test-func-1 (x) x)
  (compile 'notinline-test-func-1)
  (defun notinline-test-func-2 (x) (notinline-test-func-1 x) x)
  (compile 'notinline-test-func-2)
  (defvar *notinline-test-var* 10)
  (defun notinline-test-func-1 (x) (incf *notinline-test-var* x))
  (list (notinline-test-func-2 12) *notinline-test-var*))
(12 22)

(let ((file "tmp.lisp"))
  (with-open-file (o file :direction :output)
    (write-line "(defun caller (a b) (foo a b))" o)
    (write-line "(defun foo (a b c) (list a b c))" o))
  (unwind-protect (progn (load file #+CLISP :compiling #+CLISP t)
                         (foo 1 2 3))
    (delete-file file)))
(1 2 3)

(let ((file1 "tmp1.lisp") (file2 "tmp2.lisp"))
  (with-open-file (o file1 :direction :output)
    (write-line "(defun foo (a b c) (cons b c a))" o)
    (format o "(load ~S)~%" file2))
  (with-open-file (o file2 :direction :output)
    (write-line "(defun bar (a b) (sin (1+ a) (1- b a)))" o))
  (unwind-protect (progn (load file1 #+CLISP :compiling #+CLISP t)
                         (list (fboundp 'foo) (fboundp 'bar)))
    (delete-file file1) (delete-file file2)))
(T T)

;; the following 3 tests are generated
;; by the random tester in the GCL ANSI CL testsuite
;; <https://sourceforge.net/tracker/?func=detail&atid=101355&aid=813119&group_id=1355>
(funcall
 (compile nil '(lambda (a)
                (if (and (if a t nil) nil) a (min (block b5 -1) a))))
 123)
-1

(funcall
 (compile nil '(lambda (a b c)
                (if (or (not (and a nil))
                        (and (or b (ldb-test (byte 26 31) c)) t))
                    b b)))
 123 144 532)
144

(funcall
 (compile nil '(lambda (c)
		(if (or (not (if c nil nil))
			(and (and (ldb-test (byte 13 25) -707966251)
				  (logbitp 5 c))
			     (ldb-test (byte 13 26) -396394270089)))
		    513972305 19641756)))
 125)
513972305

;; <http://article.gmane.org/gmane.lisp.clisp.devel/10566>
(let ((file "tmp.lisp"))
  (with-open-file (out file :direction :output)
    (write '(eval-when (load compile eval)
             (+ (funcall (compile nil (lambda () (load-time-value (+ 2 3)))))
              120))
           :stream out))
  (unwind-protect (compile-file file)
    (delete-file file)
    (delete-file (compile-file-pathname file))
    #+clisp (delete-file (make-pathname :type "lib" :defaults file)))
  nil)
nil

(FUNCALL
 (COMPILE NIL (LAMBDA (A) (UNWIND-PROTECT (BLOCK B2 (RETURN-FROM B2 A)))))
 77759)
77759

;; <https://sourceforge.net/tracker/index.php?func=detail&aid=860052&group_id=1355&atid=101355>
(funcall
 (compile
  nil
  (lambda ()
    (labels ((%f17 (f17-1 f17-2)
               (multiple-value-prog1 f17-1 f17-2 100 (return-from %f17 12))))
      (%f17 1 2)))))
12

(funcall (compile nil (lambda (a)
                        (block b6
                          (multiple-value-prog1 a (return-from b6 100)))))
         :wrong)
100

(funcall (compile nil (lambda ()
                        (block b3
                          (return-from b3 (multiple-value-prog1 10
                                            (return-from b3 100)))))))
100

;; <https://sourceforge.net/tracker/index.php?func=detail&aid=842912&group_id=1355&atid=101355>
(FUNCALL
 (COMPILE NIL
          (LAMBDA (A B)
            (UNWIND-PROTECT
                 (BLOCK B2
                   (FLET ((%F1 NIL B))
                     (LOGIOR (IF A (IF (LDB-TEST (BYTE 23 1) 253966182)
                                       (RETURN-FROM B2 A) -103275090)
                                 62410)
                             (IF (NOT (NOT (IF (NOT NIL) T
                                               (LDB-TEST (BYTE 2 27)
                                                         253671809))))
                                 (RETURN-FROM B2 -22)
                                 (%F1)))))
              (setq a (+ a b)))))
 777595384624 -1510893868)
777595384624

;;<https://sourceforge.net/tracker/index.php?func=detail&aid=842913&group_id=1355&atid=101355>
(FUNCALL
 (COMPILE NIL
          (LAMBDA (A C)
            (FLET ((%F10 () 10))
              (FLET ((%F4 (&OPTIONAL
                           (F4-1 (SETQ C (%F10)))
                           (F4-2 (SETQ A 0)))
                       (+ F4-1 F4-2)
                       123))
                (%F4 -5)))))
 13 17)
123

;;<https://sourceforge.net/tracker/index.php?func=detail&aid=842910&group_id=1355&atid=101355>
(funcall
 (COMPILE NIL
          (LAMBDA (A C)
            (IF (OR (LDB-TEST (BYTE 12 18) A)
                    (NOT (AND T (NOT (IF (NOT (AND C T)) NIL NIL)))))
                170
                -110730)))
 123 456)
-110730
