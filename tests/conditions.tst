;;;; -*- Lisp -*-
;;;; Test suite for the Common Lisp condition system
;;;; Written by David Gadbois <gadbois@cs.utexas.edu> 30.11.1993

;;;
;;; Helpers
;;;

#+CLISP
(defun my-cpl (class)
  (clos::class-precedence-list (clos:find-class class))
)
#+ALLEGRO
(defun my-cpl (class)
  (clos:finalize-inheritance (find-class class))
  (clos:class-precedence-list (find-class class))
)
#+CMU
(defun my-cpl (class)
  (pcl:class-precedence-list (find-class class))
)
#+SBCL
(defun my-cpl (class)
  (sb-pcl:class-precedence-list (find-class class))
)
MY-CPL

(defun check-superclasses (class expected)
  (let ((expected (list* class 't #+(or CLISP ALLEGRO SBCL) 'standard-object #+CMU 'instance 'condition expected))
        (super (mapcar #' #+(or CLISP ALLEGRO SBCL) class-name #+CMU pcl:class-name (my-cpl class))))
    (list (set-difference super expected)
          (set-difference expected super))))
CHECK-SUPERCLASSES

;;;
;;; IGNORE-ERRORS
;;;
;;; If this does not work, none of the tests that check for getting an error
;;; will.

;;; IGNORE-ERRORS should work.
(multiple-value-bind (value condition)
    (ignore-errors (error "Foo"))
  (list value (type-of condition)))
(nil simple-error)

;;; IGNORE-ERRORS should not interfere with values in non-error situations.
(multiple-value-list
    (ignore-errors (values 23 42)))
(23 42)

;;;
;;; Predefined condition types.
;;;

(check-superclasses 'warning '())
(nil nil)
(check-superclasses 'style-warning '(warning))
(nil nil)
(check-superclasses 'serious-condition '())
(nil nil)
(check-superclasses 'error '(serious-condition))
(nil nil)
(check-superclasses 'cell-error '(error serious-condition))
(nil nil)
(check-superclasses 'parse-error '(error serious-condition))
(nil nil)
(check-superclasses 'storage-condition '(serious-condition))
(nil nil)
(check-superclasses 'simple-error '(simple-condition error serious-condition))
(nil nil)
(check-superclasses 'simple-condition '())
(nil nil)
(check-superclasses 'simple-warning '(simple-condition warning))
(nil nil)
(check-superclasses 'file-error '(error serious-condition))
(nil nil)
(check-superclasses 'control-error '(error serious-condition))
(nil nil)
(check-superclasses 'program-error '(error serious-condition))
(nil nil)
(check-superclasses 'undefined-function '(cell-error error serious-condition))
(nil nil)
(check-superclasses 'arithmetic-error '(error serious-condition))
(nil nil)
(check-superclasses 'division-by-zero '(arithmetic-error error serious-condition))
(nil nil)
(check-superclasses 'floating-point-invalid-operation '(arithmetic-error error serious-condition))
(nil nil)
(check-superclasses 'floating-point-inexact '(arithmetic-error error serious-condition))
(nil nil)
(check-superclasses 'floating-point-overflow '(arithmetic-error error serious-condition))
(nil nil)
(check-superclasses 'floating-point-underflow '(arithmetic-error error serious-condition))
(nil nil)
(check-superclasses 'unbound-slot '(cell-error error serious-condition))
(nil nil)
(check-superclasses 'package-error '(error serious-condition))
(nil nil)
(check-superclasses 'print-not-readable '(error serious-condition))
(nil nil)
(check-superclasses 'reader-error '(parse-error stream-error error serious-condition))
(nil nil)
(check-superclasses 'stream-error '(error serious-condition))
(nil nil)
(check-superclasses 'end-of-file '(stream-error error serious-condition))
(nil nil)
(check-superclasses 'unbound-variable '(cell-error error serious-condition))
(nil nil)
(check-superclasses 'type-error '(error serious-condition))
(nil nil)
(check-superclasses 'simple-type-error '(simple-condition type-error error serious-condition))
(nil nil)

;;;
;;; Defining conditions.
;;;
(define-condition test () ())
TEST

(check-superclasses 'test '())
(nil nil)

(define-condition test2 (test) ())
TEST2

(check-superclasses 'test2 '(test))
(nil nil)

(define-condition test3 (test2 simple-condition) ())
TEST3

(check-superclasses 'test3 '(test2 test simple-condition))
(nil nil)

;;;
;;; Making conditions
;;;
(progn (make-condition 'test) t)
T

(ignore-errors (progn (make-condition 'integer) t))
NIL

;;;
;;; :REPORT option to DEFINE-CONDITION
;;;
(define-condition test4 (test3)
  ()
  (:report (lambda (condition stream)
             (format stream "Yow! -- ~S" (type-of condition)))))
TEST4

(with-output-to-string (s) (princ (make-condition 'test4) s))
"Yow! -- TEST4"

(define-condition test5 (test4) ())
TEST5

(with-output-to-string (s) (princ (make-condition 'test5) s))
"Yow! -- TEST5"

(with-output-to-string (s)
  (princ (make-condition 'test3
           :format-control "And How! -- ~S"
           :format-arguments '(23)) s))
"And How! -- 23"

;;;
;;; Condition slots.
;;;
(define-condition test6 (test4)
  ((foo :initarg :foo :initform 23 :accessor test6-foo))
  (:report (lambda (condition stream)
             (format stream "~S -- ~S"
                     (type-of condition)
                     (test6-foo condition)))))
TEST6

(test6-foo (make-condition 'test6))
23

(test6-foo (make-condition 'test6 :foo 42))
42

(setf (test6-foo (make-condition 'test6 :foo 42)) 17)
17

(with-output-to-string (s) (princ (make-condition 'test6 :foo 42) s))
"TEST6 -- 42"

;;;
;;; HANDLER-BIND
;;;

;;; You do not have to bind handlers.
(ignore-errors
 (handler-bind
     ()
   (error "Foo")))
nil

;;; Handlers should not interfere with values in non-error situations.
(multiple-value-list
    (block foo
      (handler-bind
          ((error #'(lambda (c)
                      (declare (ignore c))
                      (return-from foo 23))))
        (values 42 17))))
(42 17)

;;; Handlers should work.
(multiple-value-list
    (block foo
      (handler-bind
          ((error #'(lambda (c)
                      (declare (ignore c))
                      (return-from foo (values 23 17)))))
        (error "Foo"))))
(23 17)

;;; Only the appropriate handlers should be called.
(ignore-errors
 (block foo
   (handler-bind
       ((type-error #'(lambda (c)
                        (declare (ignore c))
                        (return-from foo 23))))
     (error "Foo"))))
nil

;;; Handlers can be specified type expressions.
(block foo
  (handler-bind
      (((or type-error error)
        #'(lambda (c)
            (declare (ignore c))
            (return-from foo 23))))
    (error "Foo")))
23

;;; Handlers should be undone.
(ignore-errors
 (block foo
   (let ((first-time t))
     (handler-bind
         ((error
           #'(lambda (c)
               (declare (ignore c))
               (if first-time
                   (progn
                     (setq first-time nil)
                     (error "Bar"))
                   (return-from foo 23)))))
       (error "Foo")))))
nil

;;; Handlers should be undone.
(block foo
  (let ((first-time t))
    (handler-bind
        ((error
          #'(lambda (c)
              (declare (ignore c))
              (return-from foo 23))))
      (handler-bind
          ((error
            #'(lambda (c)
                (declare (ignore c))
                (if first-time
                    (progn
                      (setq first-time nil)
                      (error "Bar"))
                    (return-from foo 42)))))
        (error "Foo")))))
23

;;; Handlers in the same cluster should be accessible.
(ignore-errors
 (block foo
   (handler-bind
       ((error
         #'(lambda (c) (declare (ignore c)) nil))
        (error
         #'(lambda (c)
             (declare (ignore c))
             (return-from foo 23))))
     (error "Foo"))))
23

;;; Multiple handlers should work.
(block foo
  (handler-bind
      ((type-error
        #'(lambda (c)
            (declare (ignore c))
            (return-from foo 42)))
       (error
        #'(lambda (c)
            (declare (ignore c))
            (return-from foo 23))))
    (error "Foo")))
23

;;; Handlers should be undone.
(block foo
  (handler-bind
      ((error #'(lambda (c)
                  (declare (ignore c))
                  (return-from foo 23))))
    (block bar
      (handler-bind
          ((error #'(lambda (c)
                      (declare (ignore c))
                      (return-from foo 42))))
        (return-from bar)))
    (error "Foo")))
23

;;;
;;; HANDLER-CASE
;;;

;;; HANDLER-CASE should handle errors.
(multiple-value-list
    (handler-case
        (error "Foo")
      (error (c) (when (typep c 'error) (values 23 42)))))
(23 42)

;;; Except those it doesn't handle.
(ignore-errors
 (handler-case
     (error "Foo")
   (type-error () 23)))
NIL

;;; You don't have to specify handlers.
(ignore-errors
 (handler-case
     (error "Foo")))
NIL

;;; HANDLER-CASE should not interfere with values in non-error situations.
(multiple-value-list
    (handler-case
        (values 42 17)
      (error () 23)))
(42 17)

;;; :NO-ERROR should return values.
(multiple-value-list
    (handler-case
        (values 23 42)
      (:no-error (a b)
        (values b a))))
(42 23)

;;; Except when there is an error.
(handler-case
    (error "Foo")
  (error () 23)
  (:no-error (&rest args) (declare (ignore args)) 42))
23

;;; It does not have to be the last clause.
(handler-case
    23
  (:no-error (v) (1+ v))
  (error () 42))
24

;;; Multiple handlers should be OK.
(handler-case
    (error "Foo")
  (type-error () 23)
  (error () 42))
42

;;; Handlers should get undone.
(ignore-errors
 (progn
   (block foo
     (handler-case
         (return-from foo 23)
       (error () 42)))
   (error "Foo")))
NIL

;;; Ditto.
(ignore-errors
 (block foo
   (let ((first-time t))
     (handler-case
         (error "Foo")
       (error ()
         (if first-time
             (progn
               (setf first-time nil)
               (error "Bar"))
             (return-from foo 23)))))))
NIL

;;; from GCL ansi-test by Paul F. Dietz
(macrolet ((%m (&rest args) (cons 'error args)))
  (handler-bind ((error #'(lambda (c2)
                            (invoke-restart (find-restart 'foo c2)))))
    (handler-bind ((error #'(lambda (c) (declare (ignore c)) (error "blah"))))
      (restart-case (restart-case (%m "boo!")
                      (foo () 'bad))
        (foo () 'good)))))
good

(symbol-macrolet ((%s (error "boo!")))
  (handler-bind ((error #'(lambda (c2)
                            (invoke-restart (find-restart 'foo c2)))))
    (handler-bind ((error #'(lambda (c) (declare (ignore c)) (error "blah"))))
      (restart-case (restart-case %s
                      (foo () 'bad))
        (foo () 'good)))))
good

(macrolet ((%m2 (&rest args) (cons 'error args)))
  (macrolet ((%m (&rest args &environment env)
               (macroexpand (cons '%m2 args) env)))
    (handler-bind ((error #'(lambda (c2)
                              (invoke-restart (find-restart 'foo c2)))))
      (handler-bind ((error #'(lambda (c)
                                (declare (ignore c)) (error "blah"))))
        (restart-case (restart-case (%m "boo!")
                        (foo () 'bad))
          (foo () 'good))))))
good

(macrolet ((%m2 (&rest args) (cons 'error args)))
  (macrolet ((%m (&rest args &environment env)
               (macroexpand (cons '%m2 args) env)))
    (handler-bind ((error #'(lambda (c2)
                              (invoke-restart (find-restart 'foo c2)))))
      (handler-bind ((error #'(lambda (c)
                                (declare (ignore c)) (error "blah"))))
        (restart-case (with-restarts ((foo () 'bad))
                        (%m "boo!"))
          (foo () 'good))))))
good

(multiple-value-list
 (with-simple-restart (foo "zzz")
   (invoke-restart 'foo)))
(nil t)

(multiple-value-list
 (flet ((%f nil (invoke-restart 'foo)))
   (with-simple-restart (foo "zzz") (%f))))
(nil t)

(multiple-value-list
 (with-simple-restart (nil "")
   (invoke-restart (first (compute-restarts)))))
(nil t)

(restart-case
    (invoke-restart 'foo)
  (foo () :test (lambda (c) (declare (ignore c)) nil) 'bad)
  (foo () 'good))
good

;; restarts
(defmacro check-use-value (fun good bad &key (type 'type-error) (test 'eql))
  `(handler-bind ((,type (lambda (c)
                           (princ c) (terpri)
                           (use-value ',good))))
     (,test (,fun ',good) (,fun ',bad))))
check-use-value

(check-use-value char-code #\1 12 :test =) t
(check-use-value symbol-name good "bad" :test string=) t
(check-use-value intern "BAR" bar :test eq) t
(check-use-value fboundp cons "CONS") t
(check-use-value fdefinition cons "CONS") t
(check-use-value string "123" 123) t

(let ((bs (make-broadcast-stream)))
  (handler-bind ((type-error (lambda (c) (princ c) (terpri) (use-value bs))))
    (broadcast-stream-streams 10)))
NIL

(handler-bind ((error (lambda (c) (princ c) (terpri) (use-value #\#))))
  (eq (get-dispatch-macro-character #\a #\()
      (get-dispatch-macro-character #\# #\()))
T

(with-output-to-string (o)
  (handler-bind ((type-error (lambda (c) (princ c) (terpri) (use-value o))))
    (princ "no error!" 123)))
"no error!"

(handler-bind ((type-error (lambda (c) (princ c) (terpri) (use-value 16))))
  (parse-integer "ABC" :radix 'lambda))
2748

(with-input-from-string (s "bazonk")
  (handler-bind ((type-error (lambda (c) (princ c) (terpri) (use-value s))))
    (list (read-char 123) (read-char 1) (read-char 'read-char))))
(#\b #\a #\z)

(handler-bind
    ((type-error
      (lambda (c)
        (princ c) (terpri)
        (use-value
         (case (type-error-datum c)
           (1 *readtable*)
           (2 :upcase)
           (t (error "huh?")))))))
  (setf (readtable-case 1) 2))
:UPCASE

(handler-bind
    ((type-error
      (lambda (c)
        (princ c) (terpri)
        (use-value
         (case (type-error-datum c)
           (1 #\#)
           (2 *readtable*)
           (t (error "huh?")))))))
  (nth-value 1 (get-macro-character 1 2)))
T

(handler-bind ((type-error
                (lambda (c)
                  (princ c) (terpri)
                  (use-value 7))))
  (list (digit-char-p #\3 300)
        (digit-char-p #\8 'digit-char-p)))
(3 NIL)

(handler-bind ((type-error
                (lambda (c)
                  (princ c) (terpri)
                  (use-value (char (type-error-datum c) 0)))))
  (list (char= "abc" "a")
        (char-equal "ABC" "a")))
(T T)

(handler-bind ((type-error
                (lambda (c)
                  (princ c) (terpri)
                  (use-value (string (type-error-datum c))))))
  (ext:string-concat "foo-" 'bar "-baz"))
"foo-BAR-baz"

(handler-bind ((undefined-function
                (lambda (c) (princ c) (terpri)
                        (store-value
                         (lambda (new-car pair)
                           (setf (car pair) new-car))))))
  (let ((a '(1 . 2)))
    (setf (zz a) 12)
    a))
(12 . 2)
(fmakunbound '(setf zz)) (setf zz)

(handler-bind ((undefined-function
                (lambda (c) (princ c) (terpri) (store-value #'car))))
  (zz '(1 . 2)))
1
(fmakunbound 'zz) zz

(defun use-value-string->symbol (c)
  (princ c) (terpri)
  (use-value (intern (string-upcase
                      (etypecase c
                        (type-error (type-error-datum c))
                        (cell-error (cell-error-name c)))))))
use-value-string->symbol

(handler-bind ((error (lambda (c) (princ c) (terpri) (use-value '+))))
  (eval '(function "+")))
#.#'+

(handler-bind ((error #'use-value-string->symbol))
  (funcall "+" 1 2 3))
6

;; progv
(handler-bind ((type-error #'use-value-string->symbol))
  (progv '("foo") '(123) foo))
123

(handler-bind ((program-error (lambda (c) (princ c) (terpri) (use-value 'zz))))
  (progv '(:const-var) '(123) zz))
123

(handler-bind ((type-error #'use-value-string->symbol))
  (multiple-value-setq (a "foo") (values 123 321))
  (list foo a))
(321 123)

(handler-bind ((program-error (lambda (c) (princ c) (terpri) (use-value 'zz))))
  (setq :const-var 125)
  zz)
125

(handler-bind ((program-error
                (lambda (c) (princ c) (terpri) (use-value '(zz 48)))))
  (let (("foo" 32)) zz))
48

;; This test reflects only the current CLISP behaviour:
;; - It can be argued that zz should be bound statically (since zz
;;   is not declared special) or should be bound dynamically (since :const-var
;;   would be bound dynamically and zz replaces just the symbol).
;; - It can be argued that later zz should be evaluated statically (because
;;   that's what normal EVAL in the interpreter would do) or should be
;;   evaluated to lookup (symbol-value 'zz) - since that's what the compiler
;;   would make from the code.
(handler-bind ((program-error
                (lambda (c) (princ c) (terpri) (use-value 'zz))))
  (let ((:const-var 64)) zz))
64

;; either TYPE-ERROR or PROGRAM-ERROR is reasonable here
;; (handler-bind ((program-error #'use-value-string->symbol)
;;                (type-error #'use-value-string->symbol))
;;   ((lambda (x "y") (+ x y)) 1 3))
;; 4

;; (handler-bind ((program-error #'use-value-string->symbol)
;;                (type-error #'use-value-string->symbol))
;;   ((lambda (x &optional ("y" 10)) (+ x y)) 1 3))
;; 4

;; (handler-bind ((program-error #'use-value-string->symbol)
;;                (type-error #'use-value-string->symbol))
;;   ((lambda (x &key ("y" 10)) (+ x y)) 1 :y 3))
;; 4

;; (handler-bind ((program-error #'use-value-string->symbol)
;;                (type-error #'use-value-string->symbol))
;;   ((lambda (x &aux ("y" 10)) (+ x y)) 1))
;; 11

;; (handler-bind ((program-error #'use-value-string->symbol)
;;                (type-error #'use-value-string->symbol))
;;   (let ((f (lambda ("a" &optional "b" ("c" 1) &rest "d"
;;                     &key "e" ("f" 2) ("g" 3 "gp") (("hk" "ha") 4 "hp")
;;                     ("i" 5 "ip")
;;                     &aux ("j" 6))
;;              (list a b c '&rest d 'e e 'f f 'g g gp 'h ha hp 'i i ip 'j j))))
;;     (print f)
;;     (funcall f 11 22 33 :e 44 :g 55 'hk 66)))
;; (11 22 33 &REST (:E 44 :G 55 HK 66) E 44 F 2 G 55 T H 66 T I 5 NIL J 6)

;; make-hash-table
(flet ((mht (test) (make-hash-table :test test)))
  (check-use-value mht eql bazonk :test equalp)) t
(flet ((mht (w) (make-hash-table :weak w)))
  (check-use-value mht nil bazonk :test equalp)) t
(flet ((mht (s) (make-hash-table :size s)))
  (check-use-value mht 10 bazonk :test equalp)) t
(flet ((mht (rs) (make-hash-table :rehash-size rs)))
  (check-use-value mht 2d0 bazonk :test equalp)) t
(flet ((mht (tr) (make-hash-table :rehash-threshold tr)))
  (check-use-value mht 5d-1 bazonk :test equalp)) t

(handler-bind ((program-error (lambda (c) (princ c) (terpri) (use-value '1+)))
               (type-error (lambda (c) (princ c) (terpri) (use-value '1-))))
  (list (eval '(1 10)) (funcall 1 100) (apply 1 '(1000))))
(11 99 999)

(progn (makunbound 'bar)
(handler-bind ((unbound-variable
                (lambda (c) (declare (ignore c)) (store-value 41))))
  (1+ bar)))
42

bar 41

(progn
 (defclass zot () (zot-foo))
 (setq bar (make-instance 'zot))
 (handler-bind ((unbound-slot
                 (lambda (c) (declare (ignore c)) (store-value 41))))
   (1+ (slot-value bar 'zot-foo))))
42

(slot-value bar 'zot-foo) 41

(progn
  (define-condition xyzzy ()
    ((f1 :accessor my-f1 :initarg :f1-is))
    (:report (lambda (c s)
               (format s "~1Txyzzy: My f1 is ~A" (my-f1 c)))))
  (princ-to-string (make-condition 'xyzzy :f1-is "a silly string")))
" xyzzy: My f1 is a silly string"

;; check all invocations of correctable-error in package.d
(let* ((p1 (make-package "PACK-1" :use nil))
       (p2 (make-package "PACK-2" :use nil))
       (p3 (make-package "PACK-3" :use nil))
       (p4 (make-package "PACK-4" :use nil))
       (p5 (make-package "PACK-5" :use nil))
       (bar-name (symbol-name (gensym "BAR-")))
       (foo1 (intern "FOO" p1)) (foo2 (intern "FOO" p2))
       (bar1 (intern bar-name p1)) (bar2 (intern bar-name p2))
       (bar3 (intern bar-name p3)) (bar4 (intern bar-name p4))
       (s12 (intern "SYM-1" p2)) (s22 (intern "SYM-2" p2))
       (s13 (intern "SYM-1" p3)) (s23 (intern "SYM-2" p3))
       (s14 (intern "SYM-1" p4)) (s24 (intern "SYM-2" p4))
       (s15 (intern "SYM-1" p5)) (s25 (intern "SYM-2" p5)))
  (export (list s12 s22) p2)
  (export (list s13 s23) p3)
  (export (list s14 s24) p4)
  (handler-bind ((package-error
                  (lambda (c) (princ c) (terpri) (invoke-restart :pack-3))))
    (use-package (list p2 p3 p4) p1))
  (assert (null (set-exclusive-or (list p2 p3 p4) (package-use-list p1))))
  (assert (eq (find-symbol "SYM-1" p1) s13))
  (assert (eq (find-symbol "SYM-2" p1) s23))
  (handler-bind ((package-error
                  (lambda (c) (princ c) (terpri) (invoke-restart 'import))))
    (export s15 p1))
  (assert (eq (find-symbol "SYM-1" p1) s15))
  (handler-bind ((package-error
                  (lambda (c) (princ c) (terpri) (invoke-restart :pack-2))))
    (export foo2 p2))
  (assert (eq (find-symbol "FOO" p1) foo2))
  (assert (null (set-exclusive-or (list bar1 bar2 bar3 bar4)
                                  (find-all-symbols bar-name))))
  (handler-bind ((package-error
                  (lambda (c) (princ c) (terpri) (invoke-restart :pack-1))))
    (export bar2 p2))
  (assert (eq (find-symbol bar-name p1) bar1))
  (export bar3 p3)
  (export bar4 p4)
  (handler-bind ((package-error
                  (lambda (c) (princ c) (terpri) (invoke-restart :pack-4))))
    (unintern bar1 p1))
  (assert (eq (find-symbol bar-name p1) bar4))
  (delete-package p5)
  (handler-bind ((package-error (lambda (c) (princ c) (terpri) (continue c))))
    (delete-package p2) (delete-package p3) (delete-package p4))
  (delete-package p1))
T

(let ((p1 (make-package "PACK" :use nil)) p2 p3 p4
      (bar-name (symbol-name (gensym "BAR-"))))
  (handler-bind ((package-error
                  (lambda (c) (princ c) (terpri) (invoke-restart 'continue))))
    (assert (eq p1 (make-package "PACK"))))
  (handler-bind ((package-error
                  (lambda (c)
                    (princ c) (terpri)
                    (invoke-restart 'read "KCAP"))))
    (setq p2 (make-package "PACK")))
  (assert (string= "KCAP" (package-name p2)))
  (handler-bind ((package-error
                  (lambda (c) (princ c) (terpri) (invoke-restart 'continue))))
    (setq p3 (make-package "FOO" :nicknames (list "CL" bar-name "KCAP"))))
  (assert (equal (list bar-name) (package-nicknames p3)))
  (handler-bind ((package-error
                  (lambda (c)
                    (princ c) (terpri)
                    (invoke-restart 'read "ZOT"))))
    (setq p4 (make-package "QUUX" :nicknames (list "CL" bar-name "KCAP"))))
  (assert (equal (list "ZOT") (package-nicknames p4)))
  (delete-package p1) (delete-package p2)
  (delete-package p3) (delete-package p4))
T
