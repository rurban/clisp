;;;; Common Lisp Object System for CLISP: Method Combination
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; James Anderson 2003

(in-package "CLOS")


;; For STANDARD method combination:
;; partition the methods according to qualifiers
(defun partition-method-list (methods)
  (let ((primary-methods '())
        (before-methods '())
        (after-methods '())
        (around-methods '()))
    (dolist (method methods)
      (let ((quals (std-method-qualifiers method)))
        (cond ((equal quals '())        (push method primary-methods))
              ((equal quals '(:before)) (push method before-methods))
              ((equal quals '(:after))  (push method after-methods))
              ((equal quals '(:around)) (push method around-methods)))))
    (values
      (nreverse primary-methods)
      (nreverse before-methods)
      (nreverse after-methods)
      (nreverse around-methods))))
