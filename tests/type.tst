;; -*- Lisp -*-

(TYPEP (QUOTE A) (QUOTE SYMBOL))
T

(TYPEP (QUOTE NIL) (QUOTE SYMBOL))
T

(TYPEP (QUOTE (NIL)) (QUOTE SYMBOL))
NIL

(TYPEP 3 (QUOTE INTEGER))
T

(TYPEP 3 (QUOTE (INTEGER 0 4)))
T

(TYPEP 3 (QUOTE (INTEGER 0 3)))
T

(TYPEP 3 (QUOTE (INTEGER 0 2)))
NIL

(TYPEP 3 (QUOTE (FLOAT 0.0 2.0)))
NIL

(TYPEP 3 (QUOTE (FLOAT 0.0 2.0)))
NIL

(TYPEP 3 (QUOTE (FLOAT 0.0 4.0)))
NIL

(TYPEP 3.2 (QUOTE (FLOAT 0.0 4.0)))
T

(TYPEP 3.2 (QUOTE (FLOAT 0.0 3.2)))
T

(TYPEP 3.2 (QUOTE (FLOAT 0.0 (3.2))))
NIL

(TYPEP 3.2 (QUOTE (SHORT-FLOAT 0.0S0 3.2S0)))
#+(or ALLEGRO CMU ECL) T #-(or ALLEGRO CMU ECL) NIL

(TYPEP 3.2 (QUOTE (SINGLE-FLOAT 0.0F0 3.2F0)))
T

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2S0 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (FLOAT 0.0 3.2)))
T

(TYPEP 3.2S0 (QUOTE (FLOAT 0.0S0 3.2S0)))
T

(TYPEP 2.0S0 (QUOTE (SHORT-FLOAT 0.0S0 3.0S0)))
T

(TYPEP 2.0S0 (QUOTE (SINGLE-FLOAT 0.0F0 3.0F0)))
#+(or ALLEGRO CMU ECL) T #-(or ALLEGRO CMU ECL) NIL

(TYPEP 2.0 (QUOTE (SINGLE-FLOAT 0.0F0 3.0F0)))
T

(TYPEP 2.0D0 (QUOTE (DOUBLE-FLOAT 0.0D0 3.0D0)))
T

(TYPEP 3.0D0 (QUOTE (DOUBLE-FLOAT 0.0D0 3.0D0)))
T

(TYPEP 3.0D0 (QUOTE (DOUBLE-FLOAT 0.0D0 (3.0D0))))
NIL

(TYPEP 4 (QUOTE (MOD 4)))
NIL

(TYPEP 4 (QUOTE (MOD 5)))
T

(TYPEP 4 (QUOTE (RATIONAL 2 5)))
T

(TYPEP 4 (QUOTE (RATIONAL 2 7/2)))
NIL

(TYPEP 4 (QUOTE (RATIONAL 2 9/2)))
T

(TYPEP 4 (QUOTE (RATIONAL 2 4)))
T

(TYPEP 4/3 (QUOTE (RATIONAL 2 4)))
NIL

(TYPEP 2 (QUOTE (RATIONAL 2 4)))
T

(TYPEP "abcd" (QUOTE STRING))
T

(TYPEP "abcd" (QUOTE (STRING 4)))
T

(TYPEP "abcd" (QUOTE (STRING 43)))
NIL

(TYPEP '#(2 3) (QUOTE (COMPLEX INTEGER)))
NIL

(TYPEP '#(2 3) (QUOTE COMPLEX))
NIL

(TYPEP #C(2 3) (QUOTE COMPLEX))
T

(TYPEP #C(2 3) (QUOTE (COMPLEX INTEGER)))
T

;; depends on (UPGRADED-COMPLEX-PART-TYPE FLOAT)
(TYPEP #C(2.2 3) (QUOTE (COMPLEX FLOAT)))
T

(TYPEP #C(2 3) (QUOTE (COMPLEX SYMBOL)))
ERROR

(TYPEP '#(A B C D) (QUOTE VECTOR))
T

(TYPEP '#(A B C D) (QUOTE (VECTOR * 4)))
T

;; depends on (UPGRADED-COMPLEX-PART-TYPE '(EQL 0)) ?
(TYPEP #C(0 1) '(COMPLEX (EQL 0)))
T

#| ; depends on (upgraded-array-element-type 'SYMBOL) !
 (TYPEP '#(A B C D) (QUOTE (VECTOR SYMBOL 4)))
 NIL
|#

(TYPEP (QUOTE A) (QUOTE (SYMBOL CONS)))
ERROR

(TYPEP (QUOTE A) (QUOTE (OR CONS SYMBOL)))
T

(TYPEP (QUOTE A) (QUOTE (OR CONS NUMBER)))
NIL

(TYPEP (QUOTE A) (QUOTE (OR ATOM NUMBER)))
T

(TYPEP (QUOTE A) (QUOTE (AND ATOM NUMBER)))
NIL

(TYPEP (QUOTE 2) (QUOTE (AND ATOM NUMBER)))
T

(TYPEP (QUOTE 2) (QUOTE (MEMBER 1 2 3)))
T

(TYPEP (QUOTE 2) (QUOTE (MEMBER 1 3)))
NIL

(TYPEP (QUOTE 2) (QUOTE (NOT (MEMBER 1 3))))
T

(TYPEP (QUOTE 2) (QUOTE (NOT (MEMBER 1 2 3))))
NIL

(TYPEP 2 (QUOTE (AND NUMBER (NOT SYMBOL))))
T

(TYPEP 2 (QUOTE (AND STRING (NOT SYMBOL))))
NIL

(TYPEP 2 (QUOTE (OR STRING (NOT SYMBOL))))
T

(TYPEP (QUOTE CONS) (QUOTE FUNCTION))
#-(or CLtL2 ANSI-CL CLISP) T #+(or CLtL2 ANSI-CL CLISP) NIL

(TYPEP (QUOTE CONS) (QUOTE (SATISFIES FUNCTIONP)))
#-(or CLtL2 ANSI-CL CLISP) T #+(or CLtL2 ANSI-CL CLISP) NIL

(TYPEP (QUOTE CONS) (QUOTE (SATISFIES NOT)))
NIL

(TYPEP (QUOTE NIL) (QUOTE (SATISFIES NOT)))
T

(TYPEP (QUOTE NIL) NIL)
NIL

(TYPEP (QUOTE T) NIL)
NIL

(SUBTYPEP (QUOTE CONS) T)
T

(SUBTYPEP NIL (QUOTE CONS))
T

(SUBTYPEP (QUOTE CONS) (QUOTE LIST))
T

(SUBTYPEP (QUOTE CONS) (QUOTE (OR ATOM CONS)))
T

(SUBTYPEP (QUOTE CONS) (QUOTE (AND ATOM CONS)))
NIL

(SUBTYPEP (QUOTE CONS) (QUOTE (NOT ATOM)))
#-(or AKCL ALLEGRO CMU) T #+(or AKCL ALLEGRO CMU) NIL

(SUBTYPEP (QUOTE LIST) (QUOTE (NOT ATOM)))
NIL

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (INTEGER 0 7)))
T

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (INTEGER 0 (5))))
NIL

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (INTEGER 0 5)))
T

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (MOD 5)))
NIL

(SUBTYPEP (QUOTE (INTEGER 1 (5))) (QUOTE (MOD 5)))
T

(SUBTYPEP (QUOTE (OR (INTEGER 1 (5) FLOAT)))
          (QUOTE (OR FLOAT (MOD 5))))
#+(or XCL CLISP ECL) T
#+(or ALLEGRO CMU) ERROR
#-(or XCL CLISP ALLEGRO CMU ECL) UNKNOWN

(SUBTYPEP (QUOTE (OR (INTEGER 1 (5)) FLOAT)) (QUOTE (OR FLOAT (MOD 5))))
T

(SUBTYPEP (QUOTE (AND NUMBER (FLOAT 1.0 (5.0)))) (QUOTE (OR FLOAT (MOD 5))))
T

(SUBTYPEP (QUOTE (AND NUMBER (NOT (FLOAT 1.0 (5.0)))))
          (QUOTE (OR FLOAT (MOD 5))))
NIL

(SUBTYPEP (QUOTE (AND FLOAT (NOT (FLOAT 1.0 (5.0)))))
          (QUOTE (OR FLOAT (MOD 5))))
T

(SUBTYPEP (QUOTE (AND FLOAT (NOT (FLOAT 1.0 (5.0)))))
          (QUOTE (OR (FLOAT * 1.0) (FLOAT * 5.0))))
NIL

(SUBTYPEP (QUOTE (SATISFIES CONSP)) (QUOTE LIST))
NIL

(SUBTYPEP (QUOTE SIMPLE-STRING) (QUOTE ARRAY))
T

(DEFTYPE MOD1 (N) `(AND NUMBER (FLOAT 0.0 (,N))))
MOD1

(TYPEP 4.1 (QUOTE (MOD1 5.0)))
T

(TYPEP 4.1 (QUOTE (MOD1 4.1)))
NIL

(SUBTYPEP (QUOTE (FLOAT 2.3 6.7)) (QUOTE (MOD1 6.8)))
T

(SUBTYPEP (QUOTE (FLOAT 2.3 6.7)) (QUOTE (MOD1 6.7)))
NIL

(DEFUN BELIEBIGER-TEST (A) (MEMBER A (QUOTE (U I V X))))
BELIEBIGER-TEST

(NOT (NULL (TYPEP (QUOTE U) (QUOTE (SATISFIES BELIEBIGER-TEST)))))
T

(TYPEP (QUOTE A) (QUOTE (SATISFIES BELIEBIGER-TEST)))
NIL

(SUBTYPEP (QUOTE (MEMBER U I)) (QUOTE (SATISFIES BELIEBIGER-TEST)))
T

(SUBTYPEP (QUOTE (OR (MEMBER U I))) (QUOTE (SATISFIES BELIEBIGER-TEST)))
T

(SUBTYPEP (QUOTE (OR (MEMBER U I A))) (QUOTE (SATISFIES BELIEBIGER-TEST)))
NIL

(SUBTYPEP (QUOTE (SATISFIES BELIEBIGER-TEST)) (QUOTE (MEMBER U I V X Y)))
NIL

(DEFTYPE BELIEBIGER-TYP () (QUOTE (SATISFIES BELIEBIGER-TEST)))
BELIEBIGER-TYP

(NOT (NULL (TYPEP (QUOTE U) (QUOTE BELIEBIGER-TYP))))
T

(TYPEP (QUOTE A) (QUOTE BELIEBIGER-TYP))
NIL

#+(and CLISP FFI)
(TYPEP #\A 'FFI:FOREIGN-ADDRESS)
#+(and CLISP FFI)
NIL

(SUBTYPEP (QUOTE (MEMBER U I)) (QUOTE BELIEBIGER-TYP))
T

(SUBTYPEP (QUOTE BELIEBIGER-TYP) (QUOTE (MEMBER U I V X Y)))
NIL
(subtypep nil 'fixnum) t
(subtypep 'short-float 'float ) t
(subtypep 'single-float 'float ) t
(subtypep 'double-float 'float ) t
(subtypep 'long-float 'float ) t

(subtypep 'null 'symbol) t
(subtypep 'null 'list) t
(subtypep 'cons 'list) t

(subtypep 'standard-char 'string-char) t

(subtypep 'string-char 'character) t

(subtypep 'string 'vector) t

(subtypep 'bit-vector 'vector) t
(subtypep 'vector 'array) t

(subtypep 'simple-array 'array) t

(subtypep 'simple-vector 'simple-array) t
(subtypep 'simple-vector 'vector) t
(subtypep 'simple-string 'simple-array) t
(subtypep 'simple-bit-vector 'simple-array) t

(subtypep 'simple-string 'string) t
(subtypep 'simple-string 'vector) t
(subtypep 'simple-string 'simple-vector) nil
(subtypep 'simple-bit-vector 'bit-vector) t
(subtypep 'bit-vector 'vector) t
(subtypep 'simple-bit-vector 'simple-vector) nil

(subtypep 'unsigned-byte 'integer) t
(subtypep 'signed-byte 'integer) t

(type-of (coerce '(1 2 3 4) '(simple-array (unsigned-byte 8))))
(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (4))

(type-of (coerce '(1 2 3 4) '(simple-array *)))
(SIMPLE-VECTOR 4)

(type-of (coerce '(1 2 3 4) '(simple-array * (4))))
(SIMPLE-VECTOR 4)

;; these must be recognized correctly (see CLHS SUBTYPE and Figure 4-2)
(multiple-value-list (subtypep 'atom 'cons)) (nil t)
(multiple-value-list (subtypep 'atom 'list)) (nil t)
(multiple-value-list (subtypep 'cons 'atom)) (nil t)
(multiple-value-list (subtypep 'list 'atom)) (nil t)
(multiple-value-list (subtypep 'stream 'atom)) (t t)
(multiple-value-list (subtypep 'string 'atom)) (t t)
(multiple-value-list (subtypep 'vector 'atom)) (t t)
(multiple-value-list (subtypep nil nil))     (t t)
(multiple-value-list (subtypep 'extended-char 'character))     (t t)
;; yuk!!!
;;(multiple-value-list (subtypep '(vector nil) 'string)) (T T)
#+(and CLISP FFI)
(multiple-value-list (subtypep 'ffi:foreign-function 'function))
#+(and CLISP FFI)
(T T)

(multiple-value-list
 (subtypep '(and (not boolean) standard-char) 'standard-char))
(t t)

(let ((x 1)) (ctypecase x (t 'a)))  a
(let ((x 1)) (etypecase x (t 'a)))  a

(multiple-value-list (subtypep '(AND CONS (NOT (EQL 0))) 'CONS))  (t t)

(multiple-value-list (subtypep '(integer 1 2) '(real 1 2)))  (t t)
(multiple-value-list (subtypep '(integer 1 2) '(real (1) 2)))  (nil t)
(multiple-value-list (subtypep '(mod 10) '(or (mod 10) (mod 10))))  (t t)

;; from GCL ansi-test
(progn
  (setq *DISJOINT-TYPES-LIST*
        '(CONS SYMBOL ARRAY NUMBER CHARACTER HASH-TABLE FUNCTION READTABLE
          PACKAGE PATHNAME STREAM RANDOM-STATE CONDITION RESTART))
  (defclass bar () ())
  (defstruct foo))
foo

(loop for type in *disjoint-types-list*
  unless (and (equal (multiple-value-list (subtypep type 'bar)) '(nil t))
              (equal (multiple-value-list (subtypep 'bar type)) '(nil t)))
  collect type)
nil

(loop for type in *disjoint-types-list*
  unless (and (equal (multiple-value-list (subtypep type 'foo)) '(nil t))
              (equal (multiple-value-list (subtypep 'foo type)) '(nil t)))
  collect type)
nil

(loop :with class = (find-class 'vector) :for x :in '((1 0) #(1 0) #*10)
  :for y = (coerce x class) :always (and (equalp y #(1 0)) (vectorp y)))
t

(coerce 1.0 'complex)
#C(1.0 0.0)

(deftype otherwise () nil)
otherwise

(typecase 'foo (otherwise :wrong) (t :right))
:right

(typecase 'foo (otherwise :wrong) (symbol :right) (t :wrong2))
:right

;; <http://www.lisp.org/HyperSpec/Body/speope_the.html>
(the fixnum (+ 5 7)) 12
(multiple-value-list (the (values) (truncate 3.2 2))) (1 1.2)
(multiple-value-list (the integer (truncate 3.2 2)))  (1 1.2)
(multiple-value-list (the (values integer) (truncate 3.2 2)))       (1 1.2)
(multiple-value-list (the (values integer float) (truncate 3.2 2))) (1 1.2)
(multiple-value-list (the (values integer float symbol) (truncate 3.2 2)))
(1 1.2)
(multiple-value-list (the (values integer float symbol t null list)
                       (truncate 3.2 2)))
(1 1.2)
(let ((i 100)) (declare (fixnum i)) (the fixnum (1+ i)))
101
(let* ((x (list 'a 'b 'c)) (y 5))
  (setf (the fixnum (car x)) y)
  x)
(5 B C)
(the (values) 'a) A
(multiple-value-list (the (values &rest symbol) (values 'a 'b))) (A B)

(type-of (make-array '(10 3) :element-type nil))
(SIMPLE-ARRAY NIL (10 3))
(type-of (make-array 10 :element-type nil))
(SIMPLE-ARRAY NIL (10))
