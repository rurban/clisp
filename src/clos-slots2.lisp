;;;; Common Lisp Object System for CLISP: Slots
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


(defgeneric slot-missing (class instance slot-name operation
                          &optional new-value)
  (:method ((class t) instance slot-name operation &optional new-value)
    (declare (ignore instance new-value))
    (error-of-type 'error
      (TEXT "~S: The class ~S has no slot named ~S")
      operation class slot-name)))

(defgeneric slot-unbound (class instance slot-name)
  (:method ((class t) instance slot-name)
    (declare (ignore class))
    (multiple-value-bind (new-value store-p)
        (sys::check-value `(slot-value ,instance ',slot-name)
                          (make-condition 'unbound-slot :name slot-name
                                          :instance instance))
      (when store-p
        (setf (slot-value instance slot-name) new-value))
      new-value)))
