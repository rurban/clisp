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
