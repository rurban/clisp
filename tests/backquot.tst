;;; -*- Lisp -*-
;;; tests for backquote

(setf x '(a b c))
(a b c)

`(x ,x ,@x foo ,(cadr  x) bar ,(cdr x) baz ,@(cdr x) ,. x)
(X (A B C) A B C FOO B BAR (B C) BAZ B C A B C)

(read-from-string "`,@x")
ERROR

`(,x . ,x)      ; = (append (list x) x)
((a b c) a b c)

(read-from-string "`(,x . ,@x)")
ERROR

(read-from-string ",x")
ERROR

(read-from-string "`#1A(1 2 ,(+ 2 2) 4)")
ERROR

(defstruct foo (a b))
FOO

(read-from-string "`#S(FOO :A ,'A :B ,'B)")
ERROR

(read-from-string "``(,,,x)")
ERROR

(let ((list '(a b c d)))
  `(foo `(bar ,@',(mapcar #'(lambda (sym) `(baz ',sym ,sym)) list))))
(foo '(bar (baz 'a a) (baz 'b b) (baz 'c c) (baz 'd d)))

`#(1 2 3 4)
#(1 2 3 4)

`#(,@x)
#(a b c)

(setf a 10 b 20 c 30)
30

`(,a ,b ,c)
(10 20 30)

(eval ``(,,@x))
(10 20 30)

(eval ``(,,@(mapcar #'(lambda (z) `(list ',z)) x)))
((A) (B) (C))

(eval ``(,@,@(mapcar #'(lambda (z) `(list ',z)) x)))
(A B C)

(eval (eval ```(,,@,@(mapcar #'(lambda (z) `(list ',z)) x))))
(10 20 30)

(setf aa (list 10) bb (list 20) cc (list 30))
(30)

(setf xx '(aa bb cc))
(AA BB CC)

(eval (eval ```(,,@,@(mapcar #'(lambda (z) `(list ',z)) xx))))
((10) (20) (30))

(eval (eval ```(,@,@,@(mapcar #'(lambda (z) `(list ',z)) xx))))
(10 20 30)

(eval (eval ```(alpha ,@,@,@(mapcar #'(lambda (z) `(list ',z)) xx) omega)))
(ALPHA 10 20 30 OMEGA)

(eval (eval ```(alpha ,.,.,.(mapcar #'(lambda (z) `(list ',z)) xx) omega)))
(ALPHA 10 20 30 OMEGA)

;; CLISP-specific behavior: ,. splice is truly destructive when
;; backquote optimizer turned on (which is the default).

aa
(10 20 30 OMEGA)

bb
(20 30 OMEGA)

cc
(30 OMEGA)

;; CLISP-specific behavior: repeated macro expansion cancels
;; all commas and backquotes.

``````````,,,,,,,,,,'x
X

``````````,',',',',',',',',','x
'''''''''X

(let ((x 3)) `````(,(,(,(,(,(incf x)))))) x)
4

(format nil "~a" ``,,`,3)
"3"

(format nil "~a" '``,,`,3)
"``,,`,3"

(format nil "~a" '``,(,.alpha ,`,`(,@42) . ,omega))
"``,(,.ALPHA ,`,`(,@42) . ,OMEGA)"

;; Common extension: if the backquote form is based on an improper list,
;; allow unquotes in the terminating atom.
`(2 3 . #(,(+ 2 2) ,@(list 5)))
(2 3 . #(4 5))

(let ((o 1))
  (declare (special o))
  (eval (let ((a 2) (b 3))
          (declare (special a b))
          ``(,o ,@',(mapcar #'symbol-value '(a b))))))
(1 2 3)

(let ((env 1))
  (eval
   (let ((get-code '(:a 12 :b 45 :double (* %buffer 2))))
     `(defun get-macro (display event-key variable)
        `(let ((%buffer ,display))
           (declare (ignorable %buffer))
           ,(getf `(:display (the t ,display) :event-key (the t ,event-key)
                    ,@',(mapcar #'(lambda (form) (incf env env) form)
                                get-code))
                  variable)))))
  (list (eval (get-macro 1234 5678 :display))
        (eval (get-macro 1234 5678 :event-key))
        (eval (get-macro 1234 5678 :a))
        (eval (get-macro 1234 5678 :double))
        env))
(1234 5678 12 2468 64)

(progn
  (defmacro define-setf (var &rest values)
  "define a setf function name (setf <var>) that will
  set the variable `var' to the sum of the given values
  plus the one given when setf'ed."
  `(defsetf ,(intern (symbol-name var)) () (value)
     `(setf ,',var (+ ,value ,@',values))))
  (defvar *avar* nil)
  (define-setf *avar* 1 2 3)
  (list (setf (*avar*) 4)
        *avar*))
(10 10)

(let ((a 12)) (macrolet ((b () (let ((c 19)) ``(,a ,@',@(list c))))) (b)))
(12 . 19)
