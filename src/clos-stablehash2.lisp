;;;; Common Lisp Object System for CLISP
;;;; Objects with stable hash code
;;;; Part 2: Final class definition, make/initialize-instance methods.
;;;; Bruno Haible 2004-05-15

(in-package "CLOS")

;;; ===========================================================================

;;; Lift the initialization protocol.

(defmethod initialize-instance ((object standard-stablehash) &rest args
                                &key)
  (apply #'initialize-instance-<standard-stablehash> object args))

;;; ===========================================================================

;; Definition of <structure-stablehash>.
;; Used for (make-hash-table :test 'stablehash-eq).
(defstruct (structure-stablehash (:predicate nil) (:copier nil))
  (hashcode (sys::random-posfixnum))) ; GC invariant hash code

;;; ===========================================================================
