;;;; Common Lisp Object System for CLISP
;;;; Generic Functions
;;;; Part n-1: Generic functions specified in the MOP.
;;;; Bruno Haible 2004-06-13

(in-package "CLOS")


;; Make creation of <standard-generic-function> instances customizable.
(setf (fdefinition 'initialize-instance-<standard-generic-function>) #'initialize-instance)
(setf (fdefinition 'make-instance-<standard-generic-function>) #'make-instance)

;; Make creation of generic-function instances customizable.
(setf (fdefinition 'make-generic-function-instance) #'make-instance)
