;;;; Common Lisp Object System for CLISP
;;;; Class metaobjects
;;;; Part n-1: Generic functions specified in the MOP.
;;;; Bruno Haible 2004-05-25

(in-package "CLOS")

;;; ===========================================================================

;; Make creation of <class> instances customizable.
(setf (fdefinition 'initialize-instance-<built-in-class>) #'initialize-instance)
(setf (fdefinition 'make-instance-<built-in-class>) #'make-instance)
(setf (fdefinition 'initialize-instance-<structure-class>) #'initialize-instance)
(setf (fdefinition 'make-instance-<structure-class>) #'make-instance)
(setf (fdefinition 'initialize-instance-<standard-class>) #'initialize-instance)
(setf (fdefinition 'make-instance-<standard-class>) #'make-instance)

;;; ===========================================================================

;;; Optimized class-xxx accessors.
;;; These are possible thanks to the :fixed-slot-locations class option.

(defun check-class-initialized (class)
  (unless (class-initialized class)
    (error (TEXT "The class ~S has not yet been initialized.")
           class)))

(defun check-class-finalized (class)
  (check-class-initialized class)
  (when (null (%class-precedence-list class))
    (error (TEXT "The class ~S has not yet been finalized.")
           class)))

;; Not in MOP.
(defun class-classname (class)
  (assert (typep class 'class))
  (sys::%record-ref class *<class>-classname-location*))
(defun (setf class-classname) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-classname-location*) new-value))
;; MOP p. 76
(fmakunbound 'class-name)
(defgeneric class-name (class)
  (:method ((class class))
    (check-class-initialized class)
    (class-classname class)))
;; MOP p. 92
(defgeneric (setf class-name) (new-value class)
  (:method (new-value (class class))
    (unless (symbolp new-value)
      (error-of-type 'type-error
        :datum new-value :expected-type 'symbol
        (TEXT "~S: The name of a class must be a symbol, not ~S")
        '(setf class-name) new-value))
    (when (built-in-class-p class)
      (error-of-type 'error
        (TEXT "~S: The name of the built-in class ~S cannot be modified")
        '(setf class-name) class))
    ; TODO: Call (reinitialize-instance class :name new-value) instead.
    (setf (class-classname class) new-value)))

;; MOP p. 76
(fmakunbound 'class-direct-superclasses)
(defgeneric class-direct-superclasses (class)
  (:method ((class class))
    (check-class-initialized class)
    (sys::%record-ref class *<class>-direct-superclasses-location*)))
;; Not in MOP.
(defun (setf class-direct-superclasses) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-direct-superclasses-location*) new-value))

;; Not in MOP.
(defun class-all-superclasses (class)
  (assert (typep class 'class))
  (sys::%record-ref class *<class>-all-superclasses-location*))
(defun (setf class-all-superclasses) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-all-superclasses-location*) new-value))

;; Not in MOP.
(defun %class-precedence-list (class)
  (assert (typep class 'class))
  (sys::%record-ref class *<class>-precedence-list-location*))
(defun (setf class-precedence-list) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-precedence-list-location*) new-value))
;; MOP p. 76
(fmakunbound 'class-precedence-list)
(defgeneric class-precedence-list (class)
  (:method ((class class))
    (check-class-finalized class)
    (sys::%record-ref class *<class>-precedence-list-location*)))

;; Not in MOP.
(defun class-direct-subclasses-table (class)
  (assert (typep class 'class))
  (sys::%record-ref class *<class>-direct-subclasses-location*))
(defun (setf class-direct-subclasses-table) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-direct-subclasses-location*) new-value))
;; MOP p. 76
(defgeneric class-direct-subclasses (class)
  (:method ((class class))
    (check-class-initialized class)
    (list-direct-subclasses class)))

;; MOP p. 75
(fmakunbound 'class-direct-slots)
(defgeneric class-direct-slots (class)
  (:method ((class class))
    (check-class-initialized class)
    (sys::%record-ref class *<class>-direct-slots-location*)))
;; Not in MOP.
(defun (setf class-direct-slots) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-direct-slots-location*) new-value))

;; MOP p. 77
(fmakunbound 'class-slots)
(defgeneric class-slots (class)
  (:method ((class class))
    (check-class-finalized class)
    (sys::%record-ref class *<class>-slots-location*)))
;; Not in MOP.
(defun (setf class-slots) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-slots-location*) new-value))

;; Not in MOP.
(defun class-slot-location-table (class)
  (assert (typep class 'class))
  (sys::%record-ref class *<class>-slot-location-table-location*))
(defun (setf class-slot-location-table) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-slot-location-table-location*) new-value))

;; MOP p. 75
(fmakunbound 'class-direct-default-initargs)
(defgeneric class-direct-default-initargs (class)
  (:method ((class class))
    (check-class-initialized class)
    (sys::%record-ref class *<class>-direct-default-initargs-location*)))
;; Not in MOP.
(defun (setf class-direct-default-initargs) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-direct-default-initargs-location*) new-value))

;; MOP p. 75
(fmakunbound 'class-default-initargs)
(defgeneric class-default-initargs (class)
  (:method ((class class))
    (check-class-finalized class)
    (sys::%record-ref class *<class>-default-initargs-location*)))
;; Not in MOP.
(defun (setf class-default-initargs) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-default-initargs-location*) new-value))

;; Not in MOP.
(defun class-documentation (class)
  (assert (typep class 'class))
  (sys::%record-ref class *<class>-documentation-location*))
(defun (setf class-documentation) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-documentation-location*) new-value))

;; Not in MOP.
(defun class-initialized (class)
  (assert (typep class 'class))
  (sys::%record-ref class *<class>-initialized-location*))
(defun (setf class-initialized) (new-value class)
  (assert (typep class 'class))
  (setf (sys::%record-ref class *<class>-initialized-location*) new-value))

;; Not in MOP.
(defun class-subclass-of-stablehash-p (class)
  (assert (typep class 'slotted-class))
  (sys::%record-ref class *<slotted-class>-subclass-of-stablehash-p-location*))
(defun (setf class-subclass-of-stablehash-p) (new-value class)
  (assert (typep class 'slotted-class))
  (setf (sys::%record-ref class *<slotted-class>-subclass-of-stablehash-p-location*) new-value))

;; Not in MOP.
(defun class-generic-accessors (class)
  (assert (typep class 'slotted-class))
  (sys::%record-ref class *<slotted-class>-generic-accessors-location*))
(defun (setf class-generic-accessors) (new-value class)
  (assert (typep class 'slotted-class))
  (setf (sys::%record-ref class *<slotted-class>-generic-accessors-location*) new-value))

;; Not in MOP.
(defun class-direct-accessors (class)
  (assert (typep class 'slotted-class))
  (sys::%record-ref class *<slotted-class>-direct-accessors-location*))
(defun (setf class-direct-accessors) (new-value class)
  (assert (typep class 'slotted-class))
  (setf (sys::%record-ref class *<slotted-class>-direct-accessors-location*) new-value))

;; Not in MOP.
(defun class-valid-initargs (class)
  (assert (typep class 'slotted-class))
  (sys::%record-ref class *<slotted-class>-valid-initargs-location*))
(defun (setf class-valid-initargs) (new-value class)
  (assert (typep class 'slotted-class))
  (setf (sys::%record-ref class *<slotted-class>-valid-initargs-location*) new-value))

;; Not in MOP.
(defun class-instance-size (class)
  (assert (typep class 'slotted-class))
  (sys::%record-ref class *<slotted-class>-instance-size-location*))
(defun (setf class-instance-size) (new-value class)
  (assert (typep class 'slotted-class))
  (setf (sys::%record-ref class *<slotted-class>-instance-size-location*) new-value))

;; Not in MOP.
(defun class-names (class)
  (assert (typep class 'structure-class))
  (sys::%record-ref class *<structure-class>-names-location*))
(defun (setf class-names) (new-value class)
  (assert (typep class 'structure-class))
  (setf (sys::%record-ref class *<structure-class>-names-location*) new-value))

;; Not in MOP.
(defun class-current-version (class)
  (assert (typep class 'standard-class))
  (sys::%record-ref class *<standard-class>-current-version-location*))
(defun (setf class-current-version) (new-value class)
  (assert (typep class 'standard-class))
  (setf (sys::%record-ref class *<standard-class>-current-version-location*) new-value))

;; Not in MOP.
(defun class-fixed-slot-locations (class)
  (assert (typep class 'standard-class))
  (sys::%record-ref class *<standard-class>-fixed-slot-locations-location*))
(defun (setf class-fixed-slot-locations) (new-value class)
  (assert (typep class 'standard-class))
  (setf (sys::%record-ref class *<standard-class>-fixed-slot-locations-location*) new-value))

;; Not in MOP.
(defun class-instantiated (class)
  (assert (typep class 'standard-class))
  (sys::%record-ref class *<standard-class>-instantiated-location*))
(defun (setf class-instantiated) (new-value class)
  (assert (typep class 'standard-class))
  (setf (sys::%record-ref class *<standard-class>-instantiated-location*) new-value))

;; Not in MOP.
(defun class-finalized-direct-subclasses-table (class)
  (assert (typep class 'standard-class))
  (sys::%record-ref class *<standard-class>-finalized-direct-subclasses-location*))
(defun (setf class-finalized-direct-subclasses-table) (new-value class)
  (assert (typep class 'standard-class))
  (setf (sys::%record-ref class *<standard-class>-finalized-direct-subclasses-location*) new-value))

;; MOP p. 77
(fmakunbound 'class-prototype)
(defgeneric class-prototype (class)
  (:method ((class standard-class))
    (check-class-finalized class)
    (or (sys::%record-ref class *<standard-class>-prototype-location*)
        (setf (sys::%record-ref class *<standard-class>-prototype-location*)
              (let ((old-instantiated (class-instantiated class)))
                (prog1
                  (clos::%allocate-instance class)
                  ;; The allocation of the prototype doesn't need to flag the
                  ;; class as being instantiated, because 1. the prototype is
                  ;; thrown away when the class is redefined, 2. we don't want
                  ;; a redefinition with nonexistent or non-finalized
                  ;; superclasses to succeed despite of the prototype.
                  (setf (class-instantiated class) old-instantiated)))))))
;; Not in MOP.
(defun (setf class-prototype) (new-value class)
  (assert (typep class 'standard-class))
  (setf (sys::%record-ref class *<standard-class>-prototype-location*) new-value))

;;; ===========================================================================

;;; Class Finalization Protocol

;; MOP p. 76
(defgeneric class-finalized-p (class)
  (:method ((class class))
    (and (class-initialized class)
         (not (null (%class-precedence-list class)))))
  ;; CLISP extension: Convenience method on symbols.
  (:method ((name symbol))
    (class-finalized-p (find-class name))))

;; MOP p. 54
(defgeneric finalize-inheritance (class)
  (:method ((class standard-class))
    (finalize-class class t))
  ;; CLISP extension: No-op method on other classes.
  (:method ((class class))
    class)
  ;; CLISP extension: Convenience method on symbols.
  (:method ((name symbol))
    (finalize-inheritance (find-class name))))

;; MOP p. 38
(fmakunbound 'compute-class-precedence-list)
(defgeneric compute-class-precedence-list (class)
  (:method ((class class))
    (compute-class-precedence-list-<class> class)))

;; MOP p. 42
(fmakunbound 'compute-effective-slot-definition)
(defgeneric compute-effective-slot-definition (class slotname direct-slot-definitions)
  (:method ((class class) slotname direct-slot-definitions)
    (compute-effective-slot-definition-<class> class slotname direct-slot-definitions)))

;; MOP p. 43
(fmakunbound 'compute-slots)
(defgeneric compute-slots (class)
  (:method ((class standard-class))
    (compute-slots-<class>-primary class))
  (:method :around ((class standard-class))
    (compute-slots-<slotted-class>-around class
      #'(lambda (c) (call-next-method c)))))

;; MOP p. 39
(fmakunbound 'compute-default-initargs)
(defgeneric compute-default-initargs (class)
  (:method ((class class))
    (compute-default-initargs-<class> class)))

;;; ===========================================================================

;;; Class definition customization

;; MOP p. 47
(fmakunbound 'ensure-class-using-class)
(defgeneric ensure-class-using-class (class name
                                      &key metaclass
                                           direct-superclasses
                                           direct-slots
                                           direct-default-initargs
                                           documentation
                                           ; CLISP specific extension:
                                           fixed-slot-locations
                                      &allow-other-keys)
  (:method ((class class) name &rest args)
    (apply #'ensure-class-using-class-<t> class name args))
  (:method ((class null) name &rest args)
    (apply #'ensure-class-using-class-<t> class name args)))

;; MOP p. 102
(fmakunbound 'validate-superclass)
(defgeneric validate-superclass (class superclass)
  (:method ((class class) (superclass class))
    (or (eq superclass <t>)
        (eq (class-of class) (class-of superclass))
        ;; CLISP specific extension:
        (subclassp (class-of class) (class-of superclass)))))

;;; ===========================================================================

;;; Subclass relationship change notification

;; MOP p. 32
(fmakunbound 'add-direct-subclass)
(defgeneric add-direct-subclass (class subclass)
  (:method ((class class) (subclass class))
    (add-direct-subclass-internal class subclass)))

;; MOP p. 90
(fmakunbound 'remove-direct-subclass)
(defgeneric remove-direct-subclass (class subclass)
  (:method ((class class) (subclass class))
    (remove-direct-subclass-internal class subclass)))

;;; ===========================================================================

;;; Accessor definition customization

;; MOP p. 86
(fmakunbound 'reader-method-class)
(defgeneric reader-method-class (class direct-slot &rest initargs)
  (:method ((class class) direct-slot &rest initargs)
    (declare (ignore direct-slot initargs))
    <standard-reader-method>))

;; MOP p. 103
(fmakunbound 'writer-method-class)
(defgeneric writer-method-class (class direct-slot &rest initargs)
  (:method ((class class) direct-slot &rest initargs)
    (declare (ignore direct-slot initargs))
    <standard-writer-method>))
