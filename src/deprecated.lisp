;;; deprecated CLISP functionality
;;; present for now, will be removed later
;;; (except for CLHS and CLtL2 names which will be kept forever because
;;; of the old unmaintained packages we want to keep working).
;;;
;;; Sam Steingold 2001, 2007, 2009, 2017

;; the standard way to deprecate a function is to define a
;; compiler-macro for it which will issue a warning

(in-package "SYSTEM")

(defun deprecate (symbol superseded &optional (def (fdefinition superseded)))
  (export symbol (symbol-package symbol))
  (sys::%putd symbol def)
  (push (list symbol "Use ~S instead." superseded)
        system::*deprecated-functions-alist*)
  symbol)

;; ------------------------------------------------
;; http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Issues/iss321.html
;; `special-form-p' -- renamed to `special-operator-p'

(deprecate 'ext::special-form-p 'special-operator-p)

;; ------------------------------------------------
;; http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Issues/iss308.html
;; `get-setf-method-multiple-value' -- renamed to `get-setf-expansion'
;; `define-setf-method' -- renamed to `define-setf-expander'

(deprecate 'ext::get-setf-method-multiple-value 'get-setf-expansion)
(deprecate 'ext::define-setf-method 'define-setf-expander)
