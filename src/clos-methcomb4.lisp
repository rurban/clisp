;;;; Common Lisp Object System for CLISP: Method Combination
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; James Anderson 2003

(in-package "CLOS")


;; Converts a method-combination designator, e.g. a method combination name
;; or a list consisting of a method combination name and options, to a
;; method-combination instance.
(defun coerce-to-method-combination (method-combo)
  (flet ((mc (designator)
           (typecase designator
             (symbol (find-method-combination designator))
             (method-combination designator)
             (t (error-of-type 'program-error
                  (TEXT "~S is not a valid a ~S designator")
                  designator 'method-combination)))))
    (if (consp method-combo)
      (let ((clone (copy-method-combination (mc (first method-combo)))))
        (setf (method-combination-options clone)
              (copy-list (rest method-combo)))
        clone)
      (mc method-combo))))
