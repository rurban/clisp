;;;; Common Lisp Object System for CLISP: Classes
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


(defgeneric print-object (object stream)
  (:method ((object standard-object) stream)
    (if *print-readably*
      (let ((form (make-init-form object)))
        (if form
          (write (sys::make-load-time-eval form) :stream stream)
          (print-unreadable-object (object stream :type t :identity t))))
      (print-unreadable-object (object stream :type t :identity t))))
  (:method ((object structure-object) stream)
    (system::print-structure object stream))
  (:method ((object class) stream)
    (print-object-<class> object stream))
  (:method ((object slot-definition) stream)
    (print-object-<slot-definition> object stream))
  (:method ((object eql-specializer) stream)
    (print-object-<eql-specializer> object stream))
  (:method ((object method-combination) stream)
    (print-object-<method-combination> object stream))
  (:method ((object standard-method) stream)
    (print-object-<standard-method> object stream))
  (:method ((object funcallable-standard-object) stream)
    (print-object-<funcallable-standard-object> object stream)))

;; Another DEFSTRUCT hook.
(defun defstruct-remove-print-object-method (name)
  (let ((method (find-method #'print-object nil
                             (list (find-class name) <t>) nil)))
    (when method (remove-method #'print-object method))))
