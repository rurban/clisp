;; -*- Lisp -*-
;; Test unportable code of which some implementations specify the behaviour
;; These fall into 3 categories in ANSI-CL:
;; - "implementation defined",
;; - "implementation dependent" or
;; - "undefined consequences"
;; - none of the above, simply a gap in the spec
;; Use #-(or ...) to avoid implementations where the unportable form
;; may drop the bomb

;;;;
;;;;  Implementation-defined
;;;;

;; 13.1.4.6 & 13.6
#+(and clisp unicode)
(digit-char-p #\KHMER_DIGIT_ZERO) ; #\U17e0
#+(and clisp unicode) 0

;; In CLISP, there are unicode digits beside 0-9 a-Z yet they form no numbers
#+(and clisp unicode)
(type-of (read-from-string (string #\KHMER_DIGIT_ZERO)))
#+(and clisp unicode) SYMBOL


;;;;
;;;;  Implementation-dependent
;;;;

;; 6.1.1.4 Whether initial value forms of for/as variables include
;; lexical environment of all loop variables or only preceding ones.
;; CLISP changed its behaviour across versions

#-(or clisp)
(let ((vars '(1 2))) (loop for vars on vars collect (length vars)))
#+(or cmu sbcl cormanlisp) (2 1)

#-(or clisp)
(let ((vars '(1 2 3))) (loop for i from 0 below 5 for vars on vars collect (length vars)))
#+(or cmu sbcl cormanlisp) (3 2 1)

;; Whether the iteration constructs establish a new binding of var on
;; each iteration or whether it establishes a binding for var once at
;; the beginning and then assigns it on any subsequent iterations

;; Actually, CLISP does not guarantee the (3 3 3) or (2 2 2) result,
;; it just guarantees that it won't be (2 1 0), but rather (x x x),
;; because a single binding is assigned on each iteration.
;; CMUCL 19d switched dotimes behaviour from (3 3 3) to (2 1 0)

(let (a)
  (dotimes (i 3) (push (lambda () i) a))
  (loop for x in a collect (funcall x)))
#+(or clisp cmu sbcl cormanlisp) (3 3 3)
#+(or) (2 1 0)

(let (a)
  (dolist (i '(0 1 2)) (push (lambda () i) a))
  (loop for x in a collect (funcall x)))
#+(or clisp) (2 2 2)
#+(or cmu sbcl) (2 1 0)
#+(or cormanlisp) (nil nil nil)


#|
;; 1.4.1.5 Coerce designator to function once or every time
(defun add-some (x) ; TODO not reproducible past first run
  (defun add-some (x) (+ x 2))
  (+ x 1))
add-some
;; ffi callback: coerce immediately, IIRC don't even accept name

#-(or)
(mapcar 'add-some '(1 2 3 4))
#+(or clisp cormanlisp) (2 4 5 6)
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
#+(or cmu sbcl cormanlisp) T


;;;;
;;;; Undefined Consequences
;;;;

;; 2.4.8.3
#-(or) (read-from-string "#3()")
#+(or clisp) ERROR
#+(or sbcl cmu cormanlisp) #(nil nil nil)

;; 5.2
;; "The consequences are undefined if an attempt is made to transfer
;; control to an exit point whose dynamic extent has ended."


;;;;
;;;; Simply unspecified
;;;;

;; CLHS has a note on PROG where it is "explained" as (BLOCK (LET (TAGBODY ...))
;; whereas Mario Latendresse's paper on list comprehensions shows a macroexpansion
;; with the nesting (LET (BLOCK (TAGBODY ...))) -- what implementation was used?
;; Note that notes in ANSI-CL are not a binding part of the standard.

(block nil (prog ((x (return :outer-let))) (return :never)) (return :clhs))
#-(or cormanlisp) :clhs
#+(or cormanlisp) :outer-let

(dolist (i '(1 2 . 3) i))
ERROR

(loop for i in '(1 2 . 3) count t)      ; for comparison, well-defined
ERROR                                   ; 6.1.2.1.2 via ENDP

;; http://www.cliki.net/Issue%20BUTLAST-DOTTED-LIST
(butlast '(1 2 . 3) 0)
#+(or clisp cmu) (1 2)
#+(or sbcl Emacs) (1 2 . 3)
#+(or cormanlisp) ERROR

(list 1 #.(values) 2)
#+(or clisp) (1 nil 2)
#+(or sbcl cmu cormanlisp) (1 2)


;;;;
;;;; Portable code, but care to depend on this?
;;;;

;; 6.1.9
;; "The clause repeat n is roughly equivalent to a clause such as
;; for downfrom (- n 1) to 0"
;; Iterate differs and uses ceiling instead.
(loop repeat 3.5 count t)
#+(or cmu) 3
#+(or clisp sbcl cormanlisp) 4

(loop for i downfrom (- 3.5 1) to 0 count t) ; for comparison, well-defined
3

;; "the concept [of length] does not make sense for dotted lists",
;; says ANSI-CL issue DOTTED-LIST-ARGUMENTS
(length '(1 2 . 3))                     ; dotted list is not a sequence
ERROR                                   ; how annoying

(list-length '(1 2 . 3))
ERROR                                   ; agreed


;; Paul Dietz' ANSI testsuite (part of gcl) checks some border cases
;; http://cvs.savannah.gnu.org/viewcvs/gcl/ansi-tests/beyond-ansi/?root=gcl
