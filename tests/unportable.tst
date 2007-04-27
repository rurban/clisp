;; -*- Lisp -*-
;; Test unportable code of which some implementations specify the behaviour
;; These fall into 3 categories in ANSI-CL:
;; - "implementation defined",
;; - "implementation dependent" or
;; - "undefined consequences"
;; Use #-(or ...) to avoid implementations where the unportable form may drop the bomb

;;;;  Implementation-defined

;; 13.1.4.6 & 13.6
#+(and clisp unicode)
(digit-char-p #\KHMER_DIGIT_ZERO) ; #\U17e0
#+(and clisp unicode) 0

;; In CLISP, there are unicode digits beside 0-9 a-z yet they form no numbers
#+(and clisp unicode)
(type-of (read-from-string (string #\KHMER_DIGIT_ZERO)))
#+(and clisp unicode) SYMBOL


;;;;  Implementation-dependent

;; 6.1.1.4 Whether initial value forms of for/as variables include
;; lexical environment of all loop variables or only preceding ones.
#-(or clisp)
(let ((vars '(1 2))) (loop for vars on vars collect (length vars)))
#+(or cmu sbcl) (2 1)

#-(or clisp)
(let ((vars '(1 2 3))) (loop for i from 0 below 5 for vars on vars collect (length vars)))
#+(or cmu sbcl) (3 2 1)

#|
;; 1.4.1.5 Coerce designator to function once or every time
(defun add-some (x) ; TODO not reproducible past first run
  (defun add-some (x) (+ x 2))
  (+ x 1))
add-some
;; ffi callback: coerce immediately, IIRC don't even accept name

#-(or)
(mapcar 'add-some '(1 2 3 4))
#+(or clisp) (2 4 5 6)
|#

#|
;; 3.2.5 error handler for type error in compile[-file]
(with-simple-restart (error "test handler")
  (compile '(macrolet ((foo () (error "at compile-time")))
              (foo))))
|#

;; 5.1.1.2 setf expander vs. setf function of standardized accessors
(fboundp '(setf car))
#+(or clisp) NIL
#+(or sbcl) T


;;;; Undefined Consequences

;; 2.4.8.3
#-(or) (read-from-string "#3()")
#+(or CLISP) ERROR
#+(or sbcl cmucl) #(nil nil nil)

;; 5.2
;; "The consequences are undefined if an attempt is made to transfer
;; control to an exit point whose dynamic extent has ended."


;; Paul Dietz' ANSI testsuite (part of gcl) checks some border cases
;; http://cvs.savannah.gnu.org/viewcvs/gcl/ansi-tests/beyond-ansi/?root=gcl
