;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


(defun make-generic-function (generic-function-class funname lambda-list argument-precedence-order method-combo method-class declspecs documentation
                              &rest methods)
  (let ((gf (make-fast-gf generic-function-class funname lambda-list argument-precedence-order method-class declspecs documentation)))
    (setf (std-gf-method-combination gf)
          (coerce-to-method-combination funname method-combo))
    (dolist (method methods) (std-add-method gf method))
    (finalize-fast-gf gf)
    gf))

;; When this is true, it is possible to replace a non-generic function with
;; a generic function through DEFGENERIC.
(defparameter *allow-making-generic* nil)

(defun do-defgeneric (funname generic-function-class lambda-list signature argument-precedence-order method-combo method-class declspecs documentation &rest methods)
  (if (fboundp funname)
    (let ((gf (fdefinition funname)))
      (if (typep-class gf <generic-function>)
        ;; Redefinition of a generic function.
        (progn
          ;; Take into account the new generic-function-class.
          (unless (eq (class-of gf) generic-function-class)
            (change-class gf generic-function-class))
          (warn-if-gf-already-called gf)
          ;; Remove the old defgeneric-originated methods. Instead of calling
          ;; std-remove-method on each such method, while inhibiting warnings,
          ;; we can just as well remove the methods directly.
          (setf (std-gf-methods gf)
                (remove-if #'(lambda (method)
                               (when (std-method-from-defgeneric method)
                                 (setf (std-method-generic-function method) nil)
                                 t))
                           (std-gf-methods gf)))
          (unless (equalp signature (std-gf-signature gf))
            (dolist (method (std-gf-methods gf))
              (check-signature-congruence gf method signature))
            (setf (std-gf-signature gf) signature))
          (shared-initialize-<standard-generic-function> gf nil
            :lambda-list lambda-list
            :argument-precedence-order argument-precedence-order
            :method-class method-class
            :declarations declspecs
            :documentation documentation)
          (let ((method-combo (coerce-to-method-combination funname method-combo)))
            (unless (eq method-combo (std-gf-method-combination gf))
              (dolist (method (std-gf-methods gf))
                (check-method-qualifiers gf method method-combo))
              (setf (std-gf-method-combination gf) method-combo)))
          (dolist (method methods) (std-add-method gf method))
          (finalize-fast-gf gf)
          gf)
        (if (not *allow-making-generic*)
          (error-of-type 'program-error
            (TEXT "~S: ~S does not name a generic function")
            'defgeneric funname)
          (setf (fdefinition funname)
                (apply #'make-generic-function generic-function-class funname lambda-list argument-precedence-order
                       method-combo method-class declspecs documentation methods)))))
    (setf (fdefinition funname)
          (apply #'make-generic-function generic-function-class funname lambda-list argument-precedence-order
                 method-combo method-class declspecs documentation methods))))


#||  ;; For GENERIC-FLET, GENERIC-LABELS
;; like make-generic-function, only that the dispatch-code is
;; installed immediately.
 (defun make-generic-function-now (generic-function-class funname lambda-list argument-precedence-order method-combo method-class declspecs documentation
                                   &rest methods)
  (let ((gf (make-fast-gf generic-function-class funname lambda-list argument-precedence-order method-class declspecs documentation)))
    (setf (std-gf-method-combination gf)
          (coerce-to-method-combination funname method-combo))
    (dolist (method methods) (std-add-method gf method))
    (install-dispatch gf)
    gf))
||#


;; For GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS

(defun make-generic-function-form (caller whole-form funname lambda-list options)
  (multiple-value-bind (generic-function-class-form signature argument-precedence-order method-combo method-class-form declspecs docstring method-forms)
      (analyze-defgeneric caller whole-form funname lambda-list options)
    (declare (ignore signature))
    `(MAKE-GENERIC-FUNCTION ,generic-function-class-form ',funname ',lambda-list ',argument-precedence-order ',method-combo ,method-class-form ',declspecs ',docstring
                            ,@method-forms)))

#| GENERIC-FUNCTION is a TYPE (and a COMMON-LISP symbol) in ANSI CL,
 but not a macro, so this definition violates the standard
 (defmacro generic-function (&whole whole-form
                             lambda-list &rest options)
  (make-generic-function-form 'generic-function whole-form 'LAMBDA
                              lambda-list options))
|#

;; For GENERIC-FLET, GENERIC-LABELS
(defun analyze-generic-fundefs (caller whole-form fundefs)
  (let ((names '())
        (funforms '()))
    (dolist (fundef fundefs)
      (unless (and (consp fundef) (consp (cdr fundef)))
        (error-of-type 'ext:source-program-error
          :form whole-form
          :detail fundef
          (TEXT "~S: ~S is not a generic function specification")
          caller fundef))
      (push (first fundef) names)
      (push (make-generic-function-form
             caller whole-form (first fundef) (second fundef) (cddr fundef))
            funforms))
    (values (nreverse names) (nreverse funforms))))


;;; GENERIC-FLET

(defmacro generic-flet (&whole whole-form
                        fundefs &body body)
  (multiple-value-bind (funnames funforms)
      (analyze-generic-fundefs 'generic-flet whole-form fundefs)
    (let ((varnames (gensym-list funnames)))
      `(LET ,(mapcar #'list varnames funforms)
         (FLET ,(mapcar #'(lambda (varname funname)
                            `(,funname (&rest args) (apply ,varname args)))
                        varnames funnames)
           ,@body)))))

;;; GENERIC-LABELS

(defmacro generic-labels (&whole whole-form
                          fundefs &body body)
  (multiple-value-bind (funnames funforms)
      (analyze-generic-fundefs 'generic-labels whole-form fundefs)
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
