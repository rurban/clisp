;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; CLtL2 28.1.6.3., ANSI CL 7.6.3. Agreement on Parameter Specializers and
;;                                 Qualifiers
(defun methods-agree-p (method1 method2)
  (and (equal (std-method-qualifiers method1) (std-method-qualifiers method2))
       (specializers-agree-p (std-method-parameter-specializers method1)
                             (std-method-parameter-specializers method2))))
(defun specializers-agree-p (specializers1 specializers2)
  (and (eql (length specializers1) (length specializers2))
       (every #'same-specializers-p specializers1 specializers2)))
(defun same-specializers-p (parspec1 parspec2)
  (or ;; two equal classes?
      (eq parspec1 parspec2)
      ;; two equal EQL-specializers?
      (and (consp parspec1) (consp parspec2)
           (eql (second parspec1) (second parspec2)))))

;; CLtL2 28.1.6.2., ANSI CL 7.6.2. Applicable methods
(defun method-applicable-p (method required-arguments)
  (every #'typep required-arguments
         (std-method-parameter-specializers method)))

;; CLtL2 28.1.7.1., ANSI CL 7.6.6.1. Sorting the applicable methods by
;;                                   precedence order
(defun sort-applicable-methods (methods required-arguments argument-order)
  (sort (copy-list methods)
        #'(lambda (method1 method2) ; method1 < method2 ?
            (let ((specializers1 (std-method-parameter-specializers method1))
                  (specializers2 (std-method-parameter-specializers method2)))
              (dolist (arg-index argument-order nil)
                (let ((arg (nth arg-index required-arguments))
                      (psp1 (nth arg-index specializers1))
                      (psp2 (nth arg-index specializers2)))
                  (if (consp psp1)
                    (if (consp psp2)
                      nil         ; (EQL x) = (EQL x)
                      (return t)) ; (EQL x) < <class>  ==>  method1 < method2
                    (if (consp psp2)
                      (return nil) ; <class> > (EQL x)   ==>  method1 > method2
                      ;; two classes: compare the position in the CPL of arg:
                      (let* ((cpl (class-precedence-list (class-of arg)))
                             (pos1 (position psp1 cpl))
                             (pos2 (position psp2 cpl)))
                        (cond ((< pos1 pos2) (return t)) ; method1 < method2
                              ((> pos1 pos2) (return nil)) ; method1 > method2
                              ))))))))))
