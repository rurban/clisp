;;;; Common Lisp Object System for CLISP: Customizable variables
;;;; Bruno Haible 2004

(in-package "EXT")

(progn
  (export #1='(custom::*strict-mop*
               custom::*forward-referenced-class-misdesign*)
              "CUSTOM")
  (export #1# "EXT"))

(in-package "CLOS")

;; ============================================================================

(define-symbol-macro custom:*forward-referenced-class-misdesign*
                     (<forward-referenced-class>-under-<class>))

(defvar *<forward-referenced-class>-under-<class>* nil)

(defun <forward-referenced-class>-under-<class> ()
  *<forward-referenced-class>-under-<class>*)

(defun (setf <forward-referenced-class>-under-<class>) (val)
  (when val (setq val 't))
  (if val
    (unless (eq (find-class 'class) <potential-class>)
      (set-<class>-<potential-class>))
    (unless (eq (find-class 'class) <defined-class>)
      (set-<class>-<defined-class>)))
  (setq *<forward-referenced-class>-under-<class>* val)
  val)

(defun set-<class>-<potential-class> ()
  (setf (class-classname <defined-class>) 'defined-class)
  (setf (class-classname <potential-class>) 'class)
  (setf (find-class 'class) <potential-class>)
  (setf (get 'class 'sys::type-symbol) (get 'potential-class 'sys::type-symbol)))

(defun set-<class>-<defined-class> ()
  (setf (class-classname <potential-class>) 'potential-class)
  (setf (class-classname <defined-class>) 'class)
  (setf (find-class 'class) <defined-class>)
  (setf (get 'class 'sys::type-symbol) (get 'defined-class 'sys::type-symbol)))

; Initial setting:
(set-<class>-<defined-class>)

;; ============================================================================

(define-symbol-macro custom:*strict-mop* (strict-mop))

(defvar *strict-mop* nil)

(defun strict-mop ()
  *strict-mop*)

(defun (setf strict-mop) (val)
  (when val (setq val 't))
  (setf custom:*forward-referenced-class-misdesign* val)
  (setq *strict-mop* val)
  val)
