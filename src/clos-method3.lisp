;;;; Common Lisp Object System for CLISP
;;;; Methods
;;;; Part n-2: make/initialize-instance methods, generic functions.
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;;; Lift the initialization protocol.

(defmethod initialize-instance ((method standard-method) &rest args
                                &key qualifiers
                                     lambda-list
                                     specializers
                                     function
                                     documentation
                                     ((fast-function fast-function) nil)
                                     ((initfunction initfunction) nil)
                                     ((wants-next-method-p wants-next-method-p) nil)
                                     ((signature signature) nil)
                                     ((gf gf) nil)
                                     ((from-defgeneric from-defgeneric) nil)
                                &allow-other-keys)
  (declare (ignore qualifiers lambda-list specializers function documentation
                   fast-function initfunction wants-next-method-p signature gf
                   from-defgeneric))
  (apply #'initialize-instance-<standard-method> method args))

(defmethod initialize-instance ((method standard-accessor-method) &rest args
                                &key slot-definition
                                &allow-other-keys)
  (declare (ignore slot-definition))
  (apply #'initialize-instance-<standard-accessor-method> method args))

(defmethod reinitialize-instance ((instance method) &rest initargs)
  (declare (ignore initargs))
  (error (TEXT "~S: The MOP does not allow reinitializing ~S")
         'reinitialize-instance instance))


;; MOP p. 82
(defgeneric method-function (method)
  (:method ((method standard-method))
    (std-method-function-or-substitute method)))
(initialize-extended-method-check #'method-function)

;; MOP p. 82
(defgeneric method-qualifiers (method)
  (:method ((method standard-method))
    (std-method-qualifiers method)))
; No extended method check because this GF is specified in ANSI CL.
;(initialize-extended-method-check #'method-qualifiers)

;; MOP p. 82
(defgeneric method-lambda-list (method)
  (:method ((method standard-method))
    (std-method-lambda-list method)))
(initialize-extended-method-check #'method-lambda-list)

;; MOP p. 82
(defgeneric method-specializers (method)
  (:method ((method standard-method))
    (std-method-specializers method)))
(initialize-extended-method-check #'method-specializers)

;; MOP p. 82
(defgeneric method-generic-function (method)
  (:method ((method standard-method))
    (std-method-generic-function method)))
(initialize-extended-method-check #'method-generic-function)

(defgeneric function-keywords (method)
  (:method ((method standard-method))
    (let ((sig (std-method-signature method)))
      (values (sig-keywords sig) (sig-allow-p sig)))))

;; MOP p. 82-83
(defgeneric accessor-method-slot-definition (method)
  (:method ((method standard-accessor-method))
    (%accessor-method-slot-definition method)))
(initialize-extended-method-check #'accessor-method-slot-definition)
