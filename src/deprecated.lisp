;; deprecated CLISP functionality
;; present for now, will be removed later
;; Sam Steingold 2001

;; the standard way to deprecate a function is to define a
;; compiler-marco for it which will issue a warning

;; type-expand-1 -- superceded by (type-expand typespec t)

(in-package "EXT")
(export '(type-expand-1))
(in-package "SYSTEM")
(defun type-expand-1 (typespec) (type-expand typespec t))

#+compiler
(define-compiler-macro type-expand-1 (typespec)
  (let ((ret `(type-expand ,typespec t)))
    (c-warn "~s is deprecated and will be removed in a future release.
Use ~s instead"
            'type-expand-1 ret)
    ret))
