;; -*- Lisp -*-
;; CLtL2 20; ANSI CL 3.1

;; eval

(eval (list 'cdr (car '((quote (a . b)) c))))
b

(makunbound 'x)
x

(eval 'x)
ERROR

(setf x 3)
3

(eval 'x)
3

;; eval-when
(let ((ff "eval-when-test.lisp"))
  (with-open-file (foo ff :direction :output)
    (format foo "~%(eval-when (compile eval)
  ;; note that LAMBDA is not externalizable
  (defvar *junk* #.(lambda (x) (+ 15 x))))~%"))
  (delete-file (compile-file ff))
  (delete-file ff)
  #+clisp (delete-file (make-pathname :type "lib" :defaults ff))
  nil)
nil

;; constantp

(constantp 2)
T

(constantp #\r)
T

(constantp "max")
T

(constantp '#(110))
T

(constantp :max)
T

(constantp T)
T

(constantp NIL)
T

(constantp 'PI)
#-CLISP T #+CLISP NIL

(constantp '(quote foo))
T
