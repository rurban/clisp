;; deprecated CLISP functionality
;; present for now, will be removed later
;; Sam Steingold 2001

;; the standard way to deprecate a function is to define a
;; compiler-marco for it which will issue a warning

(in-package "SYSTEM")

;; ---------------------------------------------------------
;; `type-expand-1' -- superceded by (type-expand typespec t)

(export '(ext::type-expand-1) "EXT")
(defun type-expand-1 (typespec) (type-expand typespec t))

#+compiler
(define-compiler-macro type-expand-1 (typespec)
  (let ((ret `(type-expand ,typespec t)))
    (c-warn "~s is deprecated and will be removed in a future release.
Use ~s instead"
            'type-expand-1 ret)
    ret))
#+compiler
(push 'type-expand-1 *deprecated-functions-list*)
(setf (get 'type-expand-1 'deprecated) 'type-expand)

;; ------------------------------------------------
;; http://www.lisp.org/HyperSpec/Issues/iss321.html
;; `special-form-p' -- renamed to `special-operator-p'

(export '(ext::special-form-p) "EXT")
(sys::%putd 'special-form-p #'special-operator-p)
#+compiler
(push 'special-form-p *deprecated-functions-list*)
(setf (get 'special-form-p 'deprecated) 'special-operator-p)

;; ------------------------------------------------
;; http://www.lisp.org/HyperSpec/Issues/iss308.html
;; `get-setf-method-multiple-value' -- renamed to `get-setf-expansion'
;; `define-setf-method' -- renamed to `define-setf-expander'

(export '(ext::get-setf-method-multiple-value ext::define-setf-method) "EXT")
(sys::%putd 'get-setf-method-multiple-value #'get-setf-expansion)
#+compiler
(push 'get-setf-method-multiple-value *deprecated-functions-list*)
(setf (get 'get-setf-method-multiple-value 'deprecated) 'get-setf-expansion)

(sys::%putd 'define-setf-method (fdefinition 'define-setf-expander))
#+compiler
(push 'define-setf-method *deprecated-functions-list*)
(setf (get 'define-setf-method 'deprecated) 'define-setf-expander)
