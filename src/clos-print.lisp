;;;; Common Lisp Object System for CLISP: Classes
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


(fmakunbound 'print-object)
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
    (if *print-readably*
      (write (sys::make-load-time-eval
               `(FIND-CLASS ',(class-classname object)))
             :stream stream)
      (print-unreadable-object (object stream :type t)
        (write (class-classname object) :stream stream)
        (when (standard-class-p object)
          (if (and (slot-boundp object 'current-version)
                   (class-version-p (class-current-version object))
                   (slot-boundp object 'precedence-list))
            (progn
              (unless (%class-precedence-list object) ; not yet finalized?
                (write-string " " stream)
                (write :incomplete :stream stream))
              ;; FIXME: Overhaul this questionable and confusing feature.
              (let ((serial (cv-serial (class-current-version object))))
                (unless (eql serial 0)
                  (write-string " " stream)
                  (write :version :stream stream)
                  (write-string " " stream)
                  (write serial :stream stream))))
            (progn
              (write-string " " stream)
              (write :uninitialized :stream stream)))))))
  (:method ((object slot-definition) stream)
    (print-unreadable-object (object stream :type t :identity t)
      (write (slot-definition-name object) :stream stream)))
  (:method ((object eql-specializer) stream)
    (print-unreadable-object (object stream :type t)
      (write (eql-specializer-object object) :stream stream)))
  (:method ((object method-combination) stream)
    (print-object-<method-combination> object stream))
  (:method ((object standard-method) stream)
    (print-object-<standard-method> object stream)))

;; Another DEFSTRUCT hook.
(defun defstruct-remove-print-object-method (name)
  (let ((method (find-method #'print-object nil
                             (list (find-class name) <t>) nil)))
    (when method (remove-method #'print-object method))))
