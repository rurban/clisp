;;;; Common Lisp Object System for CLISP
;;;; Objects with stable hash code
;;;; Part 2: Final class definition, make/initialize-instance methods.
;;;; Bruno Haible 2004-05-15

(in-package "CLOS")

;;; ===========================================================================

;; Define the class <standard-stablehash>.
(macrolet ((form () *<standard-stablehash>-defclass*))
  (form))


;;; Lift the initialization protocol.

(defmethod initialize-instance ((object standard-stablehash) &rest args
                                &key)
  (apply #'initialize-instance-<standard-stablehash> object args))

;;; ===========================================================================
