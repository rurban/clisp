;;;; Common Lisp Object System for CLISP
;;;; Slot Definition metaobjects
;;;; Part n-2: Final class definitions, make/initialize-instance methods.
;;;; Bruno Haible 2004-04-18

(in-package "CLOS")

;;; ===========================================================================

;; Define the class <slot-definition>.
(macrolet ((form () *<slot-definition>-defclass*))
  (form))

;; Define the class <direct-slot-definition>.
(macrolet ((form () *<direct-slot-definition>-defclass*))
  (form))

;; Define the class <effective-slot-definition>.
(macrolet ((form () *<effective-slot-definition>-defclass*))
  (form))


;;; Lift the initialization protocol.

(defmethod initialize-instance ((slotdef slot-definition) &rest args
                                &key name initform initfunction initargs
                                     type allocation documentation
                                     ((inheritable-initer inheritable-initer))
                                     ((inheritable-doc inheritable-doc)))
  (declare (ignore name initform initfunction initargs type allocation
                   documentation inheritable-initer inheritable-doc))
  (apply #'initialize-instance-<slot-definition> slotdef args))

(defmethod initialize-instance ((slotdef direct-slot-definition) &rest args
                                &key name initform initfunction initargs
                                     type allocation documentation
                                     ((inheritable-initer inheritable-initer))
                                     ((inheritable-doc inheritable-doc))
                                     readers writers)
  (declare (ignore name initform initfunction initargs type allocation
                   documentation inheritable-initer inheritable-doc readers
                   writers))
  (apply #'initialize-instance-<direct-slot-definition> slotdef args))

(defmethod initialize-instance ((slotdef effective-slot-definition) &rest args
                                &key name initform initfunction initargs
                                     type allocation documentation
                                     ((inheritable-initer inheritable-initer))
                                     ((inheritable-doc inheritable-doc)))
  (declare (ignore name initform initfunction initargs type allocation
                   documentation inheritable-initer inheritable-doc))
  (apply #'initialize-instance-<effective-slot-definition> slotdef args))

(defmethod reinitialize-instance ((instance slot-definition) &rest initargs)
  (declare (ignore initargs))
  (error (TEXT "~S: The MOP does not allow reinitializing ~S")
         'reinitialize-instance instance))


;;; ===========================================================================

;;; Now the concrete classes for <standard-class> and <structure-class> slots.

;;; ---------------------------------------------------------------------------

;; Define the class <standard-direct-slot-definition>.
(defparameter <standard-direct-slot-definition>
  (macrolet ((form () *<standard-direct-slot-definition>-defclass*))
    (form)))
(replace-class-version (find-class 'standard-direct-slot-definition)
                       *<standard-direct-slot-definition>-class-version*)
(defmethod initialize-instance ((slotdef standard-direct-slot-definition) &rest args)
  (apply #'initialize-instance-<standard-direct-slot-definition> slotdef args))
(setf (fdefinition 'make-instance-<standard-direct-slot-definition>) #'make-instance)

;;; ---------------------------------------------------------------------------

;; Define the class <standard-effective-slot-definition>.
(defparameter <standard-effective-slot-definition>
  (macrolet ((form () *<standard-effective-slot-definition>-defclass*))
    (form)))
(replace-class-version (find-class 'standard-effective-slot-definition)
                       *<standard-effective-slot-definition>-class-version*)
(defmethod initialize-instance ((slotdef standard-effective-slot-definition) &rest args)
  (apply #'initialize-instance-<standard-effective-slot-definition> slotdef args))
(setf (fdefinition 'make-instance-<standard-effective-slot-definition>) #'make-instance)

;;; ---------------------------------------------------------------------------

;; Define the class <structure-direct-slot-definition>.
(defparameter <structure-direct-slot-definition>
  (macrolet ((form () *<structure-direct-slot-definition>-defclass*))
    (form)))
(replace-class-version (find-class 'structure-direct-slot-definition)
                       *<structure-direct-slot-definition>-class-version*)
(defmethod initialize-instance ((slotdef structure-direct-slot-definition) &rest args)
  (apply #'initialize-instance-<structure-direct-slot-definition> slotdef args))
(setf (fdefinition 'make-instance-<structure-direct-slot-definition>) #'make-instance)

;;; ---------------------------------------------------------------------------

;; Define the class <structure-effective-slot-definition>.
(defparameter <structure-effective-slot-definition>
  (macrolet ((form () *<structure-effective-slot-definition>-defclass*))
    (form)))
(replace-class-version (find-class 'structure-effective-slot-definition)
                       *<structure-effective-slot-definition>-class-version*)
(defun structure-effective-slot-definition-initff (slotdef)
  (slot-value slotdef 'initff))
(defun (setf structure-effective-slot-definition-initff) (new-value slotdef)
  (setf (slot-value slotdef 'initff) new-value))
(defun structure-effective-slot-definition-readonly (slotdef)
  (slot-value slotdef 'readonly))
(defun (setf structure-effective-slot-definition-readonly) (new-value slotdef)
  (setf (slot-value slotdef 'readonly) new-value))
(defmethod initialize-instance ((slotdef structure-effective-slot-definition) &rest args)
  (apply #'initialize-instance-<structure-effective-slot-definition> slotdef args))
(setf (fdefinition 'make-instance-<structure-effective-slot-definition>) #'make-instance)

;;; ===========================================================================

;; Now that all the predefined subclasses of <slot-definition> have been
;; defined, CLASS-OF can work on all existing <slot-definition> instances.
;; Therefore now, not earlier, it's possible to pass these <slot-definition>
;; instances to generic functions.
