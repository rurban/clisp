;;;; Common Lisp Object System for CLISP
;;;; Generic Functions
;;;; Part n-2: make/initialize-instance methods, generic functions.
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; ----------------------------------------------------------------------------

;;; Lift the initialization protocol.

(defmethod shared-initialize ((gf standard-generic-function) situation &rest args
                              &key name
                                   lambda-list
                                   argument-precedence-order
                                   method-class
                                   method-combination
                                   documentation
                                   declarations
                                   &allow-other-keys)
  (declare (ignore name lambda-list argument-precedence-order method-class
                   method-combination documentation declarations))
  (apply #'shared-initialize-<standard-generic-function> gf situation args))

;; ----------------------------------------------------------------------------

;; An argument is called "dispatching" if not all the corresponding parameter
;; specializers are <t>.
(defun dispatching-arg-p (index methods)
  (notevery #'(lambda (method)
                (eq (nth index (std-method-specializers method)) <t>))
            methods))
(defun single-dispatching-arg (reqanz methods)
  (let ((first-dispatching-arg
         (dotimes (i reqanz nil)
           (when (dispatching-arg-p i methods) (return i)))))
    (and first-dispatching-arg
         (do ((i (1+ first-dispatching-arg) (1+ i)))
             ((>= i reqanz) first-dispatching-arg)
           (when (dispatching-arg-p i methods) (return nil))))))
(defun dispatching-arg-type (index methods)
  `(OR ,@(remove-duplicates
           (mapcar #'(lambda (method)
                       (nth index (std-method-specializers method)))
                   methods)
           :test #'same-specializers-p)))

(defgeneric no-applicable-method (gf &rest args)
  (:method ((gf t) &rest args)
    (let* ((reqanz (sig-req-num (std-gf-signature gf)))
           (methods (std-gf-methods gf))
           (dispatching-arg (single-dispatching-arg reqanz methods)))
      (sys::retry-function-call
       (if dispatching-arg
         (make-condition 'method-call-type-error
           :datum (nth dispatching-arg args)
           :expected-type (dispatching-arg-type dispatching-arg methods)
           :generic-function gf :argument-list args
           :format-control (TEXT "~S: When calling ~S with arguments ~S, no method is applicable.")
           :format-arguments (list 'no-applicable-method gf args))
         (make-condition 'method-call-error
           :generic-function gf :argument-list args
           :format-control (TEXT "~S: When calling ~S with arguments ~S, no method is applicable.")
           :format-arguments (list 'no-applicable-method gf args)))
       gf args))))

(defgeneric missing-required-method (gf combination group-name group-filter &rest args)
  (:method ((gf t) (combination method-combination) (group-name symbol) (group-filter function) &rest args)
    (let* ((reqanz (sig-req-num (std-gf-signature gf)))
           (methods (remove-if-not group-filter (std-gf-methods gf)))
           (dispatching-arg (single-dispatching-arg reqanz methods)))
      (if dispatching-arg
        (error-of-type 'method-call-type-error
          :datum (nth dispatching-arg args)
          :expected-type (dispatching-arg-type dispatching-arg methods)
          :generic-function gf :argument-list args
          (TEXT "~S: When calling ~S with arguments ~S, no method of group ~S (from ~S) is applicable.")
          'missing-required-method gf args group-name combination)
        (error-of-type 'method-call-error
          :generic-function gf :argument-list args
          (TEXT "~S: When calling ~S with arguments ~S, no method of group ~S (from ~S) is applicable.")
          'missing-required-method gf args group-name combination)))))

;; Special case of missing-required-method for STANDARD method combination
;; and the PRIMARY method group.
(defgeneric no-primary-method (gf &rest args)
  (:method ((gf t) &rest args)
    (let* ((reqanz (sig-req-num (std-gf-signature gf)))
           (methods (remove-if-not #'null (std-gf-methods gf)
                                   :key #'std-method-qualifiers))
           (dispatching-arg (single-dispatching-arg reqanz methods)))
      (sys::retry-function-call
       (if dispatching-arg
         (make-condition 'method-call-type-error
           :datum (nth dispatching-arg args)
           :expected-type (dispatching-arg-type dispatching-arg methods)
           :generic-function gf :argument-list args
           :format-control (TEXT "~S: When calling ~S with arguments ~S, no primary method is applicable.")
           :format-arguments (list 'no-primary-method gf args))
         (make-condition 'method-call-error
           :generic-function gf :argument-list args
           :format-control (TEXT "~S: When calling ~S with arguments ~S, no primary method is applicable.")
           :format-arguments (list 'no-primary-method gf args)))
       gf args))))

(defun %no-next-method (method &rest args)
  (apply #'no-next-method (std-method-generic-function method) method args))
(defgeneric no-next-method (gf method &rest args)
  (:method ((gf standard-generic-function) (method standard-method) &rest args
            &aux (cont-mesg (format nil (TEXT "ignore ~S") 'CALL-NEXT-METHOD)))
    (if (let ((method-combo (std-gf-method-combination gf)))
          (funcall (method-combination-call-next-method-allowed method-combo)
                   gf method-combo method))
      (cerror cont-mesg 'method-call-error
        :generic-function gf :method method :argument-list args
        :format-control (TEXT "~S: When calling ~S with arguments ~S, there is no next method after ~S, and ~S was called.")
        :format-arguments (list 'no-next-method gf args method
                                '(call-next-method)))
      (let ((qualifiers (std-method-qualifiers method)))
        (if qualifiers
          (cerror cont-mesg 'program-error
            :format-control (TEXT "~S: ~S is invalid within ~{~S~^ ~} methods")
            :format-arguments (list gf 'CALL-NEXT-METHOD qualifiers))
          (cerror cont-mesg 'program-error
            :format-control (TEXT "~S: ~S is invalid within primary methods")
            :format-arguments (list gf 'CALL-NEXT-METHOD)))))))

;; ----------------------------------------------------------------------------

(defun check-generic-function-initialized (gf)
  (unless (std-gf-initialized gf)
    (error (TEXT "The generic function ~S has not yet been initialized.")
           gf)))

;; MOP p. 80
(defgeneric generic-function-name (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (funcallable-name gf)))
;; MOP p. 92
(defgeneric (setf generic-function-name) (new-value generic-function)
  (:method (new-value (gf standard-generic-function))
    (unless (sys::function-name-p new-value)
      (error-of-type 'type-error
        :datum new-value :expected-type '(or symbol (cons (eql setf) (cons symbol null)))
        (TEXT "~S: The name of a generic function must be a function name, not ~S")
        '(setf generic-function-name) new-value))
    (reinitialize-instance gf :name new-value)
    new-value))

;; MOP p. 80
(defgeneric generic-function-methods (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (std-gf-methods gf)))

;; MOP p. 80
(defgeneric generic-function-method-class (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (std-gf-default-method-class gf)))

;; MOP p. 79
(defgeneric generic-function-lambda-list (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (std-gf-lambda-list gf)))

;; MOP p. 80
(defgeneric generic-function-method-combination (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (std-gf-method-combination gf)))

;; MOP p. 79
(defgeneric generic-function-argument-precedence-order (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (let ((argorder (std-gf-argorder gf))
          (lambdalist (std-gf-lambda-list gf)))
      (mapcar #'(lambda (i) (nth i lambdalist)) argorder))))

;; MOP p. 79
(defgeneric generic-function-declarations (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (std-gf-declspecs gf)))

;; ----------------------------------------------------------------------------

(defgeneric find-method (gf qualifiers specializers &optional errorp)
  (:method ((gf standard-generic-function) qualifiers specializers &optional (errorp t))
    (std-find-method gf qualifiers specializers errorp)))

(defgeneric add-method (gf method)
  (:method ((gf standard-generic-function) (method standard-method))
    (std-add-method gf method)))

(defgeneric remove-method (gf method)
  (:method ((gf standard-generic-function) (method standard-method))
    (std-remove-method gf method)))

;; MOP p. 40
(fmakunbound 'compute-discriminating-function)
(defgeneric compute-discriminating-function (gf)
  (:method ((gf standard-generic-function))
    (compute-discriminating-function-<standard-generic-function> gf)))

;; MOP p. 35
(fmakunbound 'compute-applicable-methods)
(defgeneric compute-applicable-methods (gf args)
  (:method ((gf standard-generic-function) args)
    (compute-applicable-methods-<standard-generic-function> gf args)))

;; MOP p. 36
(fmakunbound 'compute-applicable-methods-using-classes)
(defgeneric compute-applicable-methods-using-classes (gf req-arg-classes)
  (:method ((gf standard-generic-function) req-arg-classes)
    (compute-applicable-methods-using-classes-<standard-generic-function> gf req-arg-classes)))

;; MOP p. 41
(fmakunbound 'compute-effective-method)
(defgeneric compute-effective-method (gf combination methods)
  (:method ((gf standard-generic-function) combination methods)
    (compute-effective-method-<standard-generic-function> gf combination methods)))
