;;;; Common Lisp Object System for CLISP
;;;; Class metaobjects
;;;; Part n-2: Final class definitions, make/initialize-instance methods.
;;;; Bruno Haible 2004-05-25

(in-package "CLOS")

;;; ===========================================================================

;;; Lift the initialization protocol.

(defmethod initialize-instance ((class class) &rest args
                                &key name direct-superclasses direct-slots
                                     direct-default-initargs documentation
                                &allow-other-keys)
  (declare (ignore name direct-superclasses direct-slots
                   direct-default-initargs documentation))
  (apply #'initialize-instance-<class> class args))

;;; ===========================================================================

(defmethod initialize-instance ((class built-in-class) &rest args
                                &key name direct-superclasses
                                &allow-other-keys)
  (declare (ignore name direct-superclasses))
  (apply #'initialize-instance-<built-in-class> class args))

;;; ===========================================================================

(defmethod initialize-instance ((class structure-class) &rest args
                                &key name direct-superclasses direct-slots
                                     direct-default-initargs documentation
                                     ((direct-slots direct-slots-as-metaobjects) '())
                                     ((names names) nil)
                                     ((slots slots) '()) ((size size) 1)
                                &allow-other-keys)
  (declare (ignore name direct-superclasses direct-slots
                   direct-default-initargs documentation
                   direct-slots-as-metaobjects names slots size))
  (apply #'initialize-instance-<structure-class> class args))

;;; ===========================================================================

(defmethod initialize-instance ((class standard-class) &rest args
                                &key name direct-superclasses direct-slots
                                     direct-default-initargs documentation
                                &allow-other-keys)
  (declare (ignore name direct-superclasses direct-slots
                   direct-default-initargs documentation))
  (apply #'initialize-instance-<standard-class> class args))

;;; ===========================================================================

;; Now that all the predefined subclasses of <class> have been defined,
;; CLASS-OF can work on all existing <class> instances. Therefore now, not
;; earlier, it's possible to pass these <class> instances to generic
;; functions.
