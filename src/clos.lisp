;;;; Common Lisp Object System for CLISP
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; method combinations: James Anderson 2003

(in-package "CLOS")

;;; preliminary remarks:

;; abbreviations:
;; std = standard
;; gf = generic function
;; <...> = (class ...), mostly = (find-class '...)
;; em = effective method

(load "clos-class1")
; Now DEFCLASS works (except for accessor methods).
(load "clos-slots1")
(load "clos-class2")
(load "clos-method1")
(load "clos-methcomb1")
(load "clos-genfun1")
(load "clos-methcomb2")
(load "clos-genfun2")
(load "clos-methcomb3")
(load "clos-methcomb4")
(load "clos-methcomb5")
(load "clos-method2")
(load "clos-genfun3")
(load "clos-genfun4")
; Now DEFGENERIC, DEFMETHOD work. DEFCLASS works fully.
(load "clos-class3")
(load "clos-genfun5")
(load "clos-method3")
(load "clos-slots2")
(load "clos-class5")
(load "clos-slotdef2")
; Now instance creation works. Instances can be passed to generic functions.
(setq *classes-finished* t)
(load "clos-slotdef3")
(load "clos-print")
(load "documentation")
