;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; An argument is called "dispatching" if not all the corresponding parameter
;; specializers are <t>.
(defun dispatching-arg-p (index methods)
  (notevery #'(lambda (method)
                (eq (nth index (std-method-parameter-specializers method))
                    <t>))
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
                       (nth index (std-method-parameter-specializers method)))
                   methods)
           :test #'same-specializers-p)))

(defgeneric no-applicable-method (gf &rest args)
  (:method ((gf t) &rest args)
    (let* ((reqanz (sig-req-num (gf-signature gf)))
           (methods (gf-methods gf))
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

(defgeneric no-primary-method (gf &rest args)
  (:method ((gf t) &rest args)
    (let* ((reqanz (sig-req-num (gf-signature gf)))
           (methods (remove-if-not #'null (gf-methods gf)
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
  (apply #'no-next-method (std-method-gf method) method args))
(defgeneric no-next-method (gf method &rest args)
  (:method ((gf standard-generic-function) (method standard-method) &rest args
            &aux (cont-mesg (format nil (TEXT "ignore ~S") 'CALL-NEXT-METHOD)))
    (if (let ((method-combo (gf-method-combination gf)))
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

(defgeneric find-method (gf qualifiers specializers &optional errorp)
  (:method ((gf standard-generic-function) qualifiers specializers &optional (errorp t))
    (std-find-method gf qualifiers specializers errorp)))

(defgeneric add-method (gf method)
  (:method ((gf standard-generic-function) (method standard-method))
    (std-add-method gf method)))

(defgeneric remove-method (gf method)
  (:method ((gf standard-generic-function) (method standard-method))
    (std-remove-method gf method)))

(defgeneric compute-applicable-methods (gf args)
  (:method ((gf standard-generic-function) args)
    (let ((reqanz (sig-req-num (gf-signature gf)))
          (methods (gf-methods gf)))
      (if (>= (length args) reqanz)
        (let ((req-args (subseq args 0 reqanz)))
          ;; 1. Select the applicable methods:
          (setq methods
            (remove-if-not
              #'(lambda (method) (method-applicable-p method req-args))
              (the list methods)))
          ;; 2. Sort the applicable methods by precedence order:
          (sort-applicable-methods methods req-args (gf-argorder gf)))
        nil)))) ; rather no error
