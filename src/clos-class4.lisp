;;;; Common Lisp Object System for CLISP
;;;; Class metaobjects
;;;; Part n-2: Final class definitions, make/initialize-instance methods.
;;;; Bruno Haible 2004-05-25

(in-package "CLOS")

;;; ===========================================================================

;;; Lift the initialization protocol.

(defmethod shared-initialize ((class class) situation &rest args
                              &key name direct-superclasses direct-slots
                                   direct-default-initargs documentation
                              &allow-other-keys)
  (declare (ignore name direct-superclasses direct-slots
                   direct-default-initargs documentation))
  (apply #'shared-initialize-<class> class situation args))

;;; ===========================================================================

(defmethod shared-initialize ((class built-in-class) situation &rest args
                              &key name direct-superclasses
                              &allow-other-keys)
  (declare (ignore name direct-superclasses))
  (apply #'shared-initialize-<built-in-class> class situation args))

;;; ===========================================================================

(defmethod shared-initialize ((class structure-class) situation &rest args
                              &key name direct-superclasses direct-slots
                                   direct-default-initargs documentation
                                   ((direct-slots direct-slots-as-metaobjects) '())
                                   ((names names) nil)
                                   ((slots slots) '()) ((size size) 1)
                              &allow-other-keys)
  (declare (ignore name direct-superclasses direct-slots
                   direct-default-initargs documentation
                   direct-slots-as-metaobjects names slots size))
  (apply #'shared-initialize-<structure-class> class situation args))

;;; ===========================================================================

(defmethod shared-initialize ((class standard-class) situation &rest args
                              &key name direct-superclasses direct-slots
                                   direct-default-initargs documentation
                              &allow-other-keys)
  (declare (ignore name direct-superclasses direct-slots
                   direct-default-initargs documentation))
  (apply #'shared-initialize-<standard-class> class situation args))

;;; ===========================================================================

;; Now that all the predefined subclasses of <class> have been defined,
;; CLASS-OF can work on all existing <class> instances. Therefore now, not
;; earlier, it's possible to pass these <class> instances to generic
;; functions.
