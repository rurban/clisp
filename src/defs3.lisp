;;; CLtL2-kompatible Definitionen
;;; insbesondere solche, die von CLtL1 abweichen
;;; Bruno Haible 6.12.1993
;;; Sam Steingold, 2001-03-20, LISP/COMMON-LISP merger

;;=============================================================================

(in-package "SYSTEM")

;; These definitions conform to CLtL2.

(defmacro flet (fundefs &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil env)
    ((lambda (main-form)
       (if declarations
         `(LOCALLY (DECLARE ,@declarations) ,main-form)
         main-form
     ) )
     `(SYS::%FLET ,fundefs
        ,@body-rest
      )
) ) )

(defmacro labels (fundefs &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil env)
    ((lambda (main-form)
       (if declarations
         `(LOCALLY (DECLARE ,@declarations) ,main-form)
         main-form
     ) )
     `(SYS::%LABELS ,fundefs
        ,@body-rest
      )
) ) )

#|
;; This would conform to ANSI CL and its broken declaration scope.

(defmacro flet (fundefs &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil env)
    `(SYS::%FLET ,fundefs
       ,@(if declarations
           `((LOCALLY (DECLARE ,@declarations) ,@body-rest))
           body-rest
         )
     )
) )

(defmacro labels (fundefs &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil env)
    `(SYS::%LABELS ,fundefs
       ,@(if declarations
           `((LOCALLY (DECLARE ,@declarations) ,@body-rest))
           body-rest
         )
     )
) )

|#

(defmacro macrolet (macrodefs &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil env)
    `(SYS::%MACROLET ,macrodefs
       ,@(if declarations
           `((LOCALLY (DECLARE ,@declarations) ,@body-rest))
           body-rest
         )
     )
) )
