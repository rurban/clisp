;;;; Common Lisp Object System for CLISP
;;;; Specializers
;;;; Part n-2: Final class definitions, make/initialize-instance methods.
;;;; Bruno Haible 2004-05-15

(in-package "CLOS")

;;; ===========================================================================

;; Define the class <specializer>.
(macrolet ((form () *<specializer>-defclass*))
  (form))

(defmethod initialize-instance ((specializer specializer) &rest args)
  (apply #'initialize-instance-<specializer> specializer args))

(defmethod reinitialize-instance ((instance specializer) &rest initargs)
  (declare (ignore initargs))
  (error (TEXT "~S: The MOP does not allow reinitializing ~S")
         'reinitialize-instance instance))

;;; ===========================================================================

;; Define the class <eql-specializer>.
(defparameter <eql-specializer>
  (macrolet ((form () *<eql-specializer>-defclass*))
    (form)))
(replace-class-version (find-class 'eql-specializer)
                       *<eql-specializer>-class-version*)

(defmethod initialize-instance ((specializer eql-specializer) &rest args
                                &key ((singleton singleton) nil)
                                &allow-other-keys)
  (declare (ignore singleton))
  (apply #'initialize-instance-<eql-specializer> specializer args))
(setf (fdefinition 'make-instance-<eql-specializer>) #'make-instance)

;;; ===========================================================================
