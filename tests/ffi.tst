;; -*- lisp -*-
;; (ext:cd "../tests/") (load "tests") (run-test "ffi")
;; ./clisp -norc -i suite/tests -x '(run-test "suite/ffi")'

(progn (defpackage "FTEST" (:use "FFI" "COMMON-LISP")) (in-package "FTEST") T)
T

(multiple-value-list (sizeof 'uint8))
(1 1)

(bitsizeof 'sint32)
32

(foreign-address-unsigned (unsigned-foreign-address 3))
3

(def-call-out c-self
  (:name "ffi_identity")
  (:arguments (obj c-pointer))
  (:return-type c-pointer) (:language :stdc))
C-SELF

(typep #'c-self 'function)
T

(typep #'c-self 'foreign-function)
T

(subtypep 'foreign-function 'function)
T

(check-type #'c-self foreign-function)
nil

(functionp (setq parse-c-type-optimizer
                 (compiler-macro-function 'parse-c-type)))
T

(funcall parse-c-type-optimizer '(parse-c-type 'c-pointer) nil)
'C-POINTER

(funcall parse-c-type-optimizer '(parse-c-type 'c-pointer 'opaque) nil)
(PARSE-C-TYPE 'C-POINTER 'OPAQUE)

(def-c-type opaque c-pointer)
OPAQUE

(funcall parse-c-type-optimizer '(parse-c-type 'opaque) nil)
'C-POINTER

(funcall parse-c-type-optimizer '(parse-c-type '(c-ptr uint8)) nil)
(PARSE-C-TYPE '(c-ptr uint8))

(car (funcall parse-c-type-optimizer '(parse-c-type `(c-array uint8 ,l)) nil))
VECTOR

(let () (declare (compile)) (with-c-var (place 'long -12345678) place))
-12345678

(let () (declare (compile))
  (with-foreign-object (fv 'long -12345678) (foreign-value fv)))
-12345678

(with-c-var (place '(c-array sint8 (2 3))
                   #2a((-1 -2 -3) (-9 -8 -7)))
  place)
#2A((-1 -2 -3) (-9 -8 -7))

(with-c-var (place '(c-array sint8 (2 3))
                   #(#(-1 -2 -3) #(-9 -8 -7)))
  place)
ERROR

(with-c-var (place '(c-array sint8 (2 3))
                   #2a((-1 -2 -3) (-9 -8 -7)))
  (cast place '(c-array sint8 (3 2))))
#2A((-1 -2) (-3 -9) (-8 -7))

;; <http://sourceforge.net/tracker/index.php?func=detail&aid=679661&group_id=1355&atid=101355>
(def-c-struct triv (i int))
TRIV

(def-call-out trigger (:arguments (struct_array (c-array-ptr (c-ptr triv))))
  (:name "ffi_identity") (:language :stdc)
  (:return-type (c-array-ptr (c-ptr triv))))
TRIGGER

(trigger (vector (make-triv :i 0) (make-triv :i 1) (make-triv :i 3)
                 (make-triv :i 4) (make-triv :i 5) (make-triv :i 6)))
#(#S(TRIV :I 0) #S(TRIV :I 1) #S(TRIV :I 3)
  #S(TRIV :I 4) #S(TRIV :I 5) #S(TRIV :I 6))

(with-foreign-object (x '(c-array-ptr int) (vector -4 6 7))
  (foreign-value x))
#(-4 6 7)

(let ((v (allocate-deep 'triv (make-triv :i 42))))
  (prog1
      (list (typeof (foreign-value v))
            (slot (foreign-value v) 'i))
    (foreign-free v)))
((C-STRUCT TRIV (I INT)) 42)

#+UNICODE
(type-of (setq orig-encoding custom:*foreign-encoding*))
#+UNICODE EXT:ENCODING

#+UNICODE
(typep (setf custom:*foreign-encoding*
             (ext:make-encoding :charset 'charset:iso-8859-1)) 'ext:encoding)
#+UNICODE T

#+UNICODE
(setf custom:*foreign-encoding* (ext:make-encoding :charset 'charset:utf-8))
#+UNICODE ERROR ;  not a 1:1 encoding

(typep (ffi::lookup-foreign-variable
        "ffi_user_pointer" (ffi::parse-c-type 'ffi:c-pointer))
       'foreign-variable)
T

(ffi::lookup-foreign-variable "ffi_user_pointer" (parse-c-type 'uint64))
ERROR

(typep (ffi::lookup-foreign-variable
        "ffi_user_pointer"
        (parse-c-type '(c-array-ptr sint8)))
       'foreign-variable)
T

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr-null (c-array character 3))))
    (:return-type (c-ptr (c-array uint8 3))) (:language :stdc))
  (c-self "@A0"))
#(64 65 48)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr-null (c-array sint8 4))))
    (:return-type (c-array-ptr uint8)) (:language :stdc))
  (list (c-self #(127 64 63 0)) (c-self nil)))
(#(127 64 63) NIL)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr-null (c-array uint8 5))))
    (:return-type (c-array-ptr sint8)) (:language :stdc))
  (c-self (make-array 5 :element-type '(unsigned-byte 8)
                      :initial-contents '(127 63 64 0 6))))
#(127 63 64)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj c-string))
    (:return-type (c-array-ptr uint8)) (:language :stdc))
  (c-self (coerce '(#\@ #\A #\Newline #\2) 'string)))
#(64 65 10 50)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr (c-array-max uint16 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self (make-array 4 :element-type '(unsigned-byte 16)
                      :initial-contents '(128 255 0 127))))
#(128 255)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (a1 (c-ptr (c-array-max uint32 4)))
                (a2 (c-ptr (c-array-max uint8 4)))
                (a3 (c-ptr (c-array-max uint8 4)))
                (a4 (c-ptr (c-array     uint32 2))))
    (:return-type (c-ptr (c-array-max sint32 4))) (:language :stdc))
  (c-self (make-array 3 :element-type '(unsigned-byte 32)
                      :initial-contents '(128 0 127))
          (vector 1 2 3)
          (make-array 2 :element-type '(unsigned-byte 8)
                      :initial-contents '(241 17))
          (make-array 2 :element-type '(unsigned-byte 32)
                      :initial-contents '(1299 192225))))
#(128)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr (c-array sint8 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self #(-128 -99 0 127)))
#(-128 -99 0 127)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr (c-array uint16 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self (make-array 4 :element-type '(unsigned-byte 16)
                      :initial-contents '(128 255 0 127))))
#(128 255 0 127)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first boolean)
                (obj (c-ptr (c-array uint16 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self T #(1000 #xff 0 127)))
#(1000 255 0 127)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr (c-union (c1 character)
                                     (s (c-array-ptr character))))))
    (:return-type (c-ptr character)) (:language :stdc))
  (c-self #\w))
#\w

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first boolean)
                (obj (c-ptr (c-union (c character)
                                     (b boolean)
                                     (p c-pointer)))
                     :in-out))
    (:return-type nil) (:language :stdc))
  (c-self t #\j))
#\j

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first c-string))
    (:return-type (c-ptr (c-array character 4))) (:language :stdc))
  (c-self "zrewp"))
"zrew"

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-ptr (c-array character 4))) (:language :stdc))
  (c-self #(64 65 66 67 68)))
"@ABC"

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first c-string)
                (obj (c-ptr (c-array sint16 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self "abc" #(-32768 -255 0 -256)))
#(-32768 -255 0 -256)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first c-string)
                (obj (c-ptr (c-array uint32 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self nil #(#xffffffff #xffffff 0 127)))
#(#xffffffff #xffffff 0 127)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr (c-array-max sint16 17)) :out))
    (:return-type nil) (:language :stdc))
  (c-self))
#()

(with-foreign-object (fv 'long -12345678) (typep fv 'foreign-variable))
T

(progn
  (defparameter *x* 0)
  (defun callback (x)
    (the (unsigned-byte 16) x)
    (setf *x* x)
    (the (unsigned-byte 16) (1+ (* 2 x))))
  *x*)
0

(def-c-type idfun
 (c-function (:arguments (x uint)) (:return-type uint)
   (:language :stdc)))
IDFUN

;; convert forth and back
(type-of (setq callbackf (with-c-var (x 'idfun #'callback) x)))
FOREIGN-FUNCTION

(list (funcall callbackf 32767) *x*)
(65535 32767)

(with-foreign-object (x '(c-function
    (:arguments (x uint)) (:return-type uint)(:language :stdc)) callbackf))
NIL

(with-foreign-object (x '(c-function
    (:arguments (x int)) (:return-type uint)(:language :stdc)) callbackf))
ERROR

(progn (foreign-free callbackf) (makunbound 'callbackf))
CALLBACKF

(progn
  (defparameter *x* 0)
  (defun pass-float (x)
    (the double-float x)
    (setf *x* x)
    (the single-float (float x 1f0)))
  *x*)
0

(def-c-type idfpfun
 (c-function (:arguments (x double-float)) (:return-type single-float)
   (:language :stdc)))
IDFPFUN

(type-of (setq fpcallback (with-c-var (x 'idfpfun #'pass-float) x)))
FOREIGN-FUNCTION

(list (funcall fpcallback 3.5d0) *x*)
(3.5f0 3.5d0)

(progn (foreign-free fpcallback) 4)
4

(funcall fpcallback -7.5d0)
ERROR

(def-call-out foreign-as-string (:name "ffi_identity")
  (:arguments (obj c-pointer))
  (:return-type c-string) (:language :stdc))
FOREIGN-AS-STRING

(with-foreign-string (fv e b "ABC" #+UNICODE :encoding #+UNICODE charset:ascii
                         :null-terminated-p nil)
  (list e b))
(3 3)

(with-foreign-string (fv e b "ABC" #+UNICODE :encoding #+UNICODE charset:ascii
                         :null-terminated-p t)
  (list e b))
(4 4)

(with-foreign-string (f e b "abc" :start 1 :end 2)
  (foreign-as-string (foreign-address f)))
"b"

(let ((f (with-foreign-string (fv e b "ABC") fv)))
  (validp f))
NIL

(block abort
  (with-foreign-string (fv e b "ABC")
    (setq fm fv) (return-from abort 123)))
123

(validp fm)
NIL

(block abort
  (with-foreign-object (fv 'sint16 -563)
    (setq fm fv) (return-from abort 246)))
246

(validp fm)
NIL

(foreign-value fm)
ERROR

(with-c-var (x '(c-array-max character 32) "") x)
""

(type-of (setq fm (allocate-deep 'c-string "abc" :read-only t)))
FOREIGN-VARIABLE

(foreign-value fm)
"abc"

(with-c-place (x fm) x)
"abc"

(with-c-place (x fm) (setf x "xyz"))
ERROR

(foreign-value (ffi::%cast fm (ffi::parse-c-type
                               '(c-ptr (c-array-max character 20)))))
"abc"

(with-c-place (x fm) (cast x '(c-ptr (c-array-max character 2))))
"ab"

(progn (foreign-free fm) 0)
0

(progn (setf (validp fm) nil) 1)
1

(progn (setf (validp fm) nil) 2)
2

(progn (setq fm (allocate-deep 'character "abc" :count 5)) (type-of fm))
FOREIGN-VARIABLE

(with-c-place (x fm) (identity (typeof x)))
(C-ARRAY-MAX CHARACTER 5)

(with-c-place (x fm) (typeof x))
(C-ARRAY-MAX CHARACTER 5)

(with-foreign-object (fv `(c-array-max character ,5) "abc")
  (with-c-place (x fv) (typeof x)))
(C-ARRAY-MAX CHARACTER 5)

(let () (declare (compile))
  (with-foreign-object (fv `(c-array-max character ,5) "abc")
    (with-c-place (x fv) (typeof x))))
(C-ARRAY-MAX CHARACTER 5)

(let () (declare (compile))
  (with-c-var (x `(c-array uint32 ,1))
    (typeof (cast x `(c-array uint8 ,4)))))
(C-ARRAY UINT8 4)

(let () (declare (compile))
  (with-c-var (x `(c-array-max uint32 ,1) #(#x11111111))
    (cast x `(c-array uint8 ,4))))
#(#x11 #x11 #x11 #x11)

(with-c-place (x fm) (setf (element x 1) #\Z))
#\Z

(foreign-value fm)
"aZc"

(with-c-place (x fm) (cast x '(c-array character 3)))
ERROR

(with-c-place (x fm) (offset x 1 '(c-array character 2)))
"Zc"

(with-c-place (x fm)
  (slot (cast x '(c-union (s (c-array character 5))
                          (c character))) 'c))
#\a

(progn (foreign-free fm) (validp fm))
T

(progn (setf (validp fm) nil) (validp fm))
NIL

(makunbound 'fm)
FM

(with-c-var (place 'long -12345678)  place)
-12345678

(with-c-var (place 'c-string "abc")  place)
"abc"

(with-c-var (place `(c-array character ,6) "abcdef")  place)
"abcdef"

(with-c-var (place `(c-array character ,6) "abcdefghi")  place)
ERROR

(with-c-var (place '(c-array character 7) "abc")  place)
ERROR

(with-c-var (place `(c-array-max character ,6) "abcdefgh")  place)
"abcdef"

(with-c-var (place `(c-array-max character ,7) "abc")  place)
"abc"

(progn (setq fm (allocate-shallow 'character :count 3)) (type-of fm))
FOREIGN-VARIABLE

(foreign-value fm)
""

(progn (foreign-free fm) T)
T

(allocate-shallow 'ulong :count 0)
ERROR

(progn (setq fm (allocate-shallow 'long :count 2)) (type-of fm))
FOREIGN-VARIABLE

(foreign-value fm)
#(0 0)

(progn (foreign-free fm) (makunbound 'fm))
FM

(with-c-var (place '(c-ptr (c-struct vector
                             (a (c-array long 2))
                             (s (c-array character 3))))
                   '#(#(-2000000000 -1000333111) "abc"))
  (slot (deref place) 'a))
#(-2000000000 -1000333111)

(with-c-var (place '(c-ptr (c-struct list
                             (a (c-array long 2))
                             (s (c-array character 3))))
                   '(#(-3 -1) #(#\a #\b #\c)))
  (slot (deref place) 's))
"abc"

(progn (foreign-free (allocate-deep
    '(c-ptr (c-struct list (a (c-array long 2)) (s (c-array character 3))))
    '(#(-2 -3) "abc")) :full t) nil)
NIL

(with-foreign-object (x '(c-array-ptr int) (vector -4 6 7)) (foreign-value x))
#(-4 6 7)

(with-foreign-object (x '(c-struct list
                          (a (c-array-ptr ulong))
                          (b (c-array-ptr ulong))
                          (c (c-array-ptr ulong))
                          (d (c-ptr (c-array ulong 2))))
                        '(#(123456789) #(987654321) #(543235263)
                          #(936272894 1333222444)))
  (foreign-value x))
(#(123456789) #(987654321) #(543235263) #(936272894 1333222444))

;; UTF-16 is not in every CLISP with #+UNICODE
#+UNICODE
(let ((sy (find-symbol "UTF-16" "CHARSET")))
  (if (and sy (boundp sy) (sys::encodingp (symbol-value sy)))
      (with-foreign-string (fv e b "ABC" :encoding (symbol-value sy))
        (list e b))
      '(4 10)))
#+UNICODE (4 10) ; #\uFEFF is added upfront

;; prevent the user from shooting himself in the foot
(setf (validp (unsigned-foreign-address 4)) nil)
ERROR

(with-c-var (place '(c-ptr (c-struct list
                            (a (c-array long 2))
                            (s (c-array character 3))))
                   '(#(-3 -1) #(#\a #\b #\c)))
  (slot (deref place) 's))
"abc"

(with-c-var (place '(c-ptr (c-struct vector
                            (a (c-array long 2))
                            (s (c-array character 3))))
                   '#(#(-3 -1) #(#\a #\b #\c)))
  place)
#(#(-3 -1) "abc")

(def-call-out make-foreign-string
  (:arguments (s c-string :in :malloc-free))
  (:name "ffi_identity") (:language :stdc)
  (:return-type c-pointer))
MAKE-FOREIGN-STRING

(progn (setf *x* (make-foreign-string "abcd"))
       (with-c-var (p 'c-pointer *x*) (cast p '(c-ptr (c-array uint8 4)))))
#(97 98 99 100)

(with-c-var (p 'c-pointer *x*) (cast p '(c-ptr (c-array character 4))))
"abcd"

(progn (foreign-free *x*) (makunbound '*x*))
*x*

#+UNICODE
(type-of (setq custom:*foreign-encoding* orig-encoding))
#+UNICODE EXT:ENCODING

#+win32
(progn
  (def-call-out command-line
      (:name "GetCommandLineA") (:library "kernel32.dll")
      (:arguments) (:return-type ffi:c-string) (:language :stdc))
  (stringp (command-line)))
#+win32 T

(list
 (def-call-out c-malloc (:arguments (l long))
   (:name "malloc") (:language :stdc) (:return-type c-pointer)
   (:library #+(and unix (not cygwin)) :DEFAULT
             #+cygwin "/bin/cygwin1.dll" ; RTLD_DEFAULT not implemented
             #+win32 :DEFAULT))
 (def-call-out c-free (:arguments (p c-pointer))
   (:name "free") (:language :stdc) (:return-type nil)
   (:library #+(and unix (not cygwin)) :DEFAULT
             #+cygwin "/bin/cygwin1.dll" ; RTLD_DEFAULT not implemented
             #+win32 :DEFAULT)))
(c-malloc c-free)

;; this is ugly and inefficient; if you find yourself doing this,
;; you probably want to do it in C
(let ((m (c-malloc 4)) ret)
  (unwind-protect
       (with-c-var (v '(c-ptr (c-array uint8 4)))
         (setf (cast v 'c-pointer) m)
         (with-c-var (i '(c-ptr uint32))
           (setf (cast i 'c-pointer) m)
           (setq i 0)
           (push v ret)
           (setq i (1- (ash 1 32)))
           (push v ret)
           (setq v #A((unsigned-byte 8) (4) (1 2 3 4)))
           ;; I depends on endianness!
           (assert (or (= i (+ (ash 4 24) (ash 3 16) (ash 2 8) 1))
                       (= i (+ (ash 1 24) (ash 2 16) (ash 3 8) 4)))))
         (nreverse ret))
    (c-free m)))
(#A((unsigned-byte 8) (4) (0 0 0 0))
 #A((unsigned-byte 8) (4) (255 255 255 255)))

(progn (in-package "USER") (delete-package "FTEST") T) T
