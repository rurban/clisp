;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


(defun make-generic-function (funname signature argorder method-combo
                              &rest methods)
  (let ((gf (make-fast-gf funname signature argorder)))
    (setf (gf-method-combination gf)
          (coerce-to-method-combination method-combo))
    (dolist (method methods) (std-add-method gf method))
    (finalize-fast-gf gf)
    gf))

(defun do-defgeneric (funname signature argorder method-combo &rest methods)
  (if (fboundp funname)
    (let ((gf (fdefinition funname)))
      (if (clos::generic-function-p gf)
        ;; redefinition of a generic function
        (progn
          (warn-if-gf-already-called gf)
          ;; Remove the old defgeneric-originated methods. Instead of calling
          ;; std-remove-method on each such method, while inhibiting warnings,
          ;; we can just as well remove the methods directly.
          (setf (gf-methods gf)
                (remove-if #'(lambda (method)
                               (when (std-method-origin method)
                                 (setf (std-method-gf method) nil)
                                 t))
                           (gf-methods gf)))
          (unless (equalp signature (gf-signature gf))
            (dolist (method (gf-methods gf))
              (check-signature-congruence gf method signature))
            (setf (gf-signature gf) signature))
          (setf (gf-argorder gf) argorder)
          (let ((method-combo (coerce-to-method-combination method-combo)))
            (unless (eq method-combo (gf-method-combination gf))
              (dolist (method (gf-methods gf))
                (check-method-qualifiers gf method method-combo))
              (setf (gf-method-combination gf) method-combo)))
          (dolist (method methods) (std-add-method gf method))
          (finalize-fast-gf gf)
          gf)
        (error-of-type 'program-error
          (TEXT "~S: ~S does not name a generic function")
          'defgeneric funname)))
    (setf (fdefinition funname)
          (apply #'make-generic-function funname signature argorder
                 method-combo methods))))


#||  ;; For GENERIC-FLET, GENERIC-LABELS
;; like make-generic-function, only that the dispatch-code is
;; installed immediately.
 (defun make-generic-function-now (funname signature argorder method-combo
                                   &rest methods)
  (let ((gf (make-fast-gf funname signature argorder)))
    (setf (gf-method-combination gf)
          (coerce-to-method-combination method-combo))
    (dolist (method methods) (std-add-method gf method))
    (install-dispatch gf)
    gf))
||#


;; For GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS

(defun make-generic-function-form (caller funname lambda-list options)
  (multiple-value-bind (signature argorder method-combo method-forms)
      (analyze-defgeneric caller funname lambda-list options)
    `(MAKE-GENERIC-FUNCTION ',funname ',signature ',argorder ',method-combo
                            ,@method-forms)))

#| GENERIC-FUNCTION is a TYPE (and a COMMON-LISP symbol) in ANSI CL,
 but not a macro, so this definition violates the standard
 (defmacro generic-function (lambda-list &rest options)
  (make-generic-function-form 'generic-function 'LAMBDA
                              lambda-list options))
|#

;; For GENERIC-FLET, GENERIC-LABELS
(defun analyze-generic-fundefs (caller fundefs)
  (let ((names '())
        (funforms '()))
    (dolist (fundef fundefs)
      (unless (and (consp fundef) (consp (cdr fundef)))
        (error-of-type 'sys::source-program-error
          (TEXT "~S: ~S is not a generic function specification")
          caller fundef))
      (push (first fundef) names)
      (push (make-generic-function-form
             caller (first fundef) (second fundef) (cddr fundef))
            funforms))
    (values (nreverse names) (nreverse funforms))))


;;; GENERIC-FLET

(defmacro generic-flet (fundefs &body body)
  (multiple-value-bind (funnames funforms)
      (analyze-generic-fundefs 'generic-flet fundefs)
    (let ((varnames (gensym-list funnames)))
      `(LET ,(mapcar #'list varnames funforms)
         (FLET ,(mapcar #'(lambda (varname funname)
                            `(,funname (&rest args) (apply ,varname args)))
                        varnames funnames)
           ,@body)))))

;;; GENERIC-LABELS

(defmacro generic-labels (fundefs &body body)
  (multiple-value-bind (funnames funforms)
      (analyze-generic-fundefs 'generic-labels fundefs)
    (let ((varnames (gensym-list funnames)))
      `(LET ,varnames
         (FLET ,(mapcar #'(lambda (varname funname)
                            `(,funname (&rest args) (apply ,varname args)))
                        varnames funnames)
           ,@(mapcar #'(lambda (varname funform) `(SETQ ,varname ,funform))
                     varnames funforms)
           ,@body)))))


;;; WITH-ADDED-METHODS
;; is screwed up and therefore will not be implemented.
