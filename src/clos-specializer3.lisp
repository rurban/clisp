;;;; Common Lisp Object System for CLISP
;;;; Specializers
;;;; Part n-1: Generic functions specified in the MOP.
;;;; Bruno Haible 2004-05-15

(in-package "CLOS")


;; Make creation of <specializer> instances customizable.
(setf (fdefinition 'make-instance-<eql-specializer>) #'make-instance)


;; Optimized accessors, with type checking.

(defun specializer-direct-generic-functions-table (specializer)
  (assert (typep specializer 'specializer))
  (sys::%record-ref specializer *<specializer>-direct-generic-functions-location*))
(defun (setf specializer-direct-generic-functions-table) (new-value specializer)
  (assert (typep specializer 'specializer))
  (setf (sys::%record-ref specializer *<specializer>-direct-generic-functions-location*) new-value))

(defun specializer-direct-methods-table (specializer)
  (assert (typep specializer 'specializer))
  (sys::%record-ref specializer *<specializer>-direct-methods-location*))
(defun (setf specializer-direct-methods-table) (new-value specializer)
  (assert (typep specializer 'specializer))
  (setf (sys::%record-ref specializer *<specializer>-direct-methods-location*) new-value))

(defun eql-specializer-singleton (specializer)
  (assert (typep specializer 'eql-specializer))
  (sys::%record-ref specializer *<eql-specializer>-singleton-location*))
(defun (setf eql-specializer-singleton) (new-value specializer)
  (assert (typep specializer 'eql-specializer))
  (setf (sys::%record-ref specializer *<eql-specializer>-singleton-location*) new-value))


;; MOP p. 103
(fmakunbound 'specializer-direct-generic-functions)
(defgeneric specializer-direct-generic-functions (specializer)
  (:method ((specializer specializer))
    (list-direct-generic-functions specializer)))

;; MOP p. 103
(fmakunbound 'specializer-direct-methods)
(defgeneric specializer-direct-methods (specializer)
  (:method ((specializer specializer))
    (list-direct-methods specializer)))
