;;;; Common Lisp Object System for CLISP: Methods
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


(defgeneric method-qualifiers (method)
  (:method ((method standard-method))
    (std-method-qualifiers method)))

(defgeneric function-keywords (method)
  (:method ((method standard-method))
    (let ((sig (std-method-signature method)))
      (values (sig-keywords sig) (sig-allow-p sig)))))

;; MOP p. 83
(defgeneric accessor-method-slot-definition (method)
  (:method ((method standard-accessor-method))
    (%accessor-method-slot-definition method)))
