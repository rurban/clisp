;; -*- Lisp -*-
;; packages-test
;; -------------

;;test file for chapter 11

(packagep  *package*)
T

;; `list-all-packages' and type test
(every #'packagep (list-all-packages))
T

;;11.6 obligate Paketnamen u. deren Abkuerzungen

;;vorhandensein der standardpakete und find-package dafuer

(and (find-package 'lisp) t)
T
(and (find-package 'user) t)
T
(and (find-package 'keyword) t)
T
(and (find-package 'system) t)
T
(and (find-package 'sys) t)
T
(and (find-package "sys") t)
NIL
(and (find-package "sys") t)
NIL
(and (find-package "system") t)
NIL
(and (find-package "SYSTEM") t)
T
(and (find-package "SYS") t)
T

(eq (find-package (find-package "CL")) (find-package "CL"))
t

;nicknames
(find "SYS" (package-nicknames 'sys) :test #'string=)
"SYS"

;package-name
(package-name 'sys)
"SYSTEM"
(package-name 'system)
"SYSTEM"
(package-name "COMMON-LISP-USER")
"COMMON-LISP-USER"
(package-name "SYS")
"SYSTEM"

(let ((p (make-package #\p)))
  (prog1 (eq p (find-package #\p))
    (delete-package p)))
t

;;; 11.7 anlegen von paketen, export import ...

  ;package-funktionen mit nutzerdefinierten paketen

;falls test1 bereits existiert
(and (find-package 'test1)
     (in-package "TEST1")
     (rename-package (find-package 'test1) 'test1-old)
     nil)
nil

;make-package
(package-name (make-package 'test1 :nicknames '(t1 tst1)))
"TEST1"

;package-use-list
;(package-use-list (find-package 'test1))
;("LISP")


(and (in-package "TEST1") T)
T


(export '(test1::test1-y test1::test1-z)
        (find-package '"TEST1"))
T

(export '(test1::test1-a test1::test1-b test1::test1-c)
        (find-package 'test1))
T

(setf test1-a -2
      test1-b -1
      test1-c  0
      test1-x  1
      test1-y  2
      test1-z  3)
3

;falls test2 bereits existiert
(and
        (find-package 'test2)
        (rename-package (find-package 'test2) 'test2-old)
        nil)
nil

(package-name (defpackage test2 (:nicknames "T2" "TST2") (:use test1)))
"TEST2"

(progn (in-package "TEST2") t)
LISP:T

(cl:package-name (cl:find-package 'test2))
"TEST2"

(cl:package-name cl:*package*)
"TEST2"

(cl:import '(cl:error) (cl:find-package 'test2))
CL:T

(cl:and (cl:boundp 'test1-x) test1-x)
CL:NIL

(cl:unintern 'test1-x)
CL:T

(eval (read-from-string "(cl:and (cl:boundp 'test1:test1-x) test1:test1-x)"))
#+XCL 1 #-XCL ERROR

(cl:and (cl:boundp 'test1::test1-x) test1::test1-x)
1

(cl:and (cl:boundp 'test1-y) test1-y)
#+XCL CL:NIL #-XCL 2

(cl:unintern 'test1-y)
#+XCL CL:T #-XCL CL:NIL

(cl:and (cl:boundp 'test1:test1-y) test1:test1-y)
#+XCL ERROR #-XCL 2

(cl:and (cl:boundp 'test1::test1-y) test1::test1-y)
2

(cl:import  '(test1::test1-x test1::test1-y) (cl:find-package 'test2))
CL:T

(cl:and (cl:boundp 'test1-x) test1-x)
1

(eval (read-from-string "(cl:and (cl:boundp 'test1:test1-x) test1:test1-x)"))
#+XCL 1 #-XCL ERROR

(cl:and (cl:boundp 'test1::test1-x) test1::test1-x)
1

(cl:and (cl:boundp 'test1-z) test1-z)
#+XCL CL:NIL #-XCL 3

(cl:unintern 'test1-z (cl:find-package 'test2))
#+XCL CL:T #-XCL CL:NIL

(cl:and (cl:boundp 'test1:test1-z) test1:test1-z)
#+XCL ERROR #-XCL 3

test1::test1-z
3

(cl:unexport  '(test1::test1-x test1::test1-y) (cl:find-package 'test1))
CL:T

(cl:and (cl:boundp 'test1-x) test1-x)
1

(cl:and (cl:boundp 'test1-y) test1-y)
#+XCL CL:NIL #-XCL 2

(cl:unintern 'test1-x (cl:find-package 'test2))
CL:T

(eval (read-from-string "test1:test1-x"))
ERROR

test1::test1-x
1

test1-z
3

(cl:unintern 'test1-z (cl:find-package 'test2))
#+XCL CL:T #-XCL CL:NIL

test1:test1-z
3

test1::test1-z
3

(cl:import 'test1::test1-z (cl:find-package 'test2))
CL:T

test1-z
3

test1:test1-z
3

test1::test1-z
3

test1-c
#+XCL ERROR #-XCL 0

(cl:unintern 'test-c (cl:find-package 'test2))
CL:T

test1:test1-c
0

test1::test1-c
0

(cl:import '(test1::test1-a test1::test1-b test1::test1-c)
             (cl:find-package 'test2))
CL:T

test1-c
0

test1:test1-c
0

test1::test1-c
0

(cl:eq 'test1-c 'test1::test1-c)
CL:T

  ;Ende nutzerdefinierte Pakete

;; test in standardmaessig vorgegebenen paketen

; export | import | unintern

(cl:and (cl:in-package "CL-USER") cl:T)
CL:T

(setf x 1 y 2 z 3)
3

(and (defpackage "EDITOR") T)
T

(and (in-package "EDITOR") T)
T

(unintern 'x)
T

(unintern 'y)
T

(unintern 'z)
T

cl-user::x
1

(eval (read-from-string "cl-user:x"))
ERROR

x
error

(eq 'x 'cl-user::x)
NIL

(unintern 'x)
T

(export '(cl-user::x cl-user::y) (find-package 'cl-user))
T

cl-user::x
1

cl-user:x
1

x
error

(unintern 'x)
T

(import 'cl-user:x (find-package 'editor))
T

x
1

(eq 'x 'cl-user::x)
t

(eq 'x 'cl-user:x)
t

(eq 'editor::x 'cl-user::x)
t

;; unexport

(and (in-package "CL-USER") T)
T

(unexport 'y)
T

(and (in-package "EDITOR") T)
T

y
ERROR

(eval (read-from-string "cl-user:y"))
ERROR

cl-user::y
2

;; shadowing-import -- zunaechst ohne geerbte symbole!!

(and (in-package "CL-USER") (package-name *package*))
"COMMON-LISP-USER"

(setf d 4 e 5 f 6 y 111 x 222)
222

(export '(cl-user::a cl-user::b cl-user::c cl-user::y cl-user::x)
        (find-package 'cl-user))
T

(import '(cl-user::a cl-user::b cl-user::c cl-user::y) (find-package 'editor))
ERROR

(and (make-package 'shadow-test) (in-package "SHADOW-TEST") t)
T

(setf x 'shadow-test)
shadow-test

(shadowing-import '(cl-user::d cl-user::e cl-user::f cl-user::x)
                  (find-package 'shadow-test))
T

x
222

(eq cl-user::x x)
T

; shadow

(shadow '(e f) (find-package 'shadow-test))
t

(setf e 'shadow-test-e)
shadow-test-e

(eq 'e 'cl-user::e)
#+XCL nil #-XCL t

e
shadow-test-e

(eval (read-from-string "cl-user:e"))
error

cl-user::e
#+XCL 5 #-XCL shadow-test-e

; use-package | unuse-package

(and (make-package 'use-test) (in-package "USE-TEST") t)
t

(use-package '(cl-user))
T

cl-user::d
4

(eval (read-from-string "cl-user:d"))
#+XCL 4 #-XCL ERROR

d
ERROR

(unuse-package 'cl-user)
T

cl-user::d
4

(eval (read-from-string "cl-user:d"))
ERROR

d
ERROR

;make-package mit beutzung eines paketes, dass geerbte symbole enthaelt

(and (make-package 'inherit :nicknames '(inh i)) (in-package "INHERIT") T)
T

(setf a 'inherita b 'inheritb)
inheritb

(export '(a b) (find-package 'inherit))
T

(and (make-package 'inherit1 :use '(inherit)) (in-package "INHERIT1") T)
T

a
inherit::inherita

b
inherit::inheritb

(cl:setf c 'inherit1c)
inherit1c

(cl:and (cl:make-package 'inherit2 :use '(inherit1))
        (cl:in-package "INHERIT2") cl:T)
CL:T

a
#+XCL inherita #-XCL CL:ERROR

b
#+XCL inheritb #-XCL CL:ERROR

c
#+XCL inherit1c #-XCL CL:ERROR

(eval (read-from-string "(cl:eq 'c 'inherit1:c)"))
#+XCL CL:T #-XCL CL:ERROR

(eval (read-from-string "(cl:eq 'a 'inherit:a)"))
#+XCL CL:T #-XCL CL:ERROR

(eval (read-from-string "(cl:eq 'b 'inherit:b)"))
#+XCL CL:T #-XCL CL:ERROR

(cl:eq 'c 'inherit1::c)
#+XCL CL:T #-XCL CL:NIL

(cl:eq 'a 'inherit::a)
#+XCL CL:T #-XCL CL:NIL

(cl:eq 'b 'inherit::b)
#+XCL CL:T #-XCL CL:NIL

;find-all-symbols

(cl:and (cl:in-package "CL-USER") cl:T)
CL:T

; find-all-symbols fehlerhaft
(and (member 'cl-user::x (setf s (find-all-symbols 'x)))T)
T

(eval (read-from-string "(and (member 'editor:x s) t)"))
#+XCL T #-XCL ERROR

(and (member 'cl-user::x (setf s1 (find-all-symbols 'x)))T)
T

(set-difference s s1)
nil                              ;Ende Kommentar

;do-symbols | do-external-symbols | do-all-symbols

(setf sym nil
      esym nil
      asym nil
)
nil

(do-symbols (s (find-package 'cl-user))(push (symbol-name s) sym))
nil

(do-external-symbols (s (find-package 'cl-user))(push (symbol-name s) esym))
nil

(do-all-symbols (s)(push (symbol-name s) asym))
nil

(find "ESYM" sym :test #'string=)
"ESYM"

(find "ESYM" esym :test #'string=)
nil

(find "LAMBDA-LIST-KEYWORDS" esym :test #'string=)
#+XCL "LAMBDA-LIST-KEYWORDS" #-XCL NIL

;(count "LAMBDA-LIST-KEYWORDS" asym :test #'string=)
;T                                                  ;viel zu lang

; modules | provide | (require nicht getestet !)

(and *modules* T)
#+(or XCL ECL) T #+CLISP NIL #-(or XCL CLISP ECL) UNKNOWN

(and (provide 'provide-test) t)
t

(find "provide-test" *modules* :test #'string=)
"provide-test"

;; <HS>/Body/mac_with-package-iterator.html
(defun test-package-iterator (package)
  (unless (packagep package)
    (setq package (find-package package)))
  (let ((all-entries '())
        (generated-entries '()))
    (do-symbols (x package)
      (multiple-value-bind (symbol accessibility)
          (find-symbol (symbol-name x) package)
        (push (list symbol accessibility) all-entries)))
    (with-package-iterator (generator-fn package
                                         :internal :external :inherited)
      (loop
        (multiple-value-bind (more? symbol accessibility pkg)
            (generator-fn)
          (unless more? (return))
          (let ((l (multiple-value-list (find-symbol (symbol-name symbol)
                                                     package))))
            (unless (equal l (list symbol accessibility))
              (error "Symbol ~S not found as ~S in package ~A [~S]"
                     symbol accessibility (package-name package) l))
            (push l generated-entries)))))
    (unless (and (subsetp all-entries generated-entries :test #'equal)
                 (subsetp generated-entries all-entries :test #'equal))
      (error "Generated entries and Do-Symbols entries do not correspond"))
    t))
test-package-iterator

(test-package-iterator :common-lisp-user)
t

(test-package-iterator :common-lisp)
t

(format t "End of file")
nil
