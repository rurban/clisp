;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; ============================= Runtime support =============================

;; Runtime support for CALL-NEXT-METHOD.
(defun %call-next-method (method next-methods original-args new-args)
  (let* ((gf (std-method-generic-function method))
         (emf (sys::generic-function-effective-method-function gf))
         (original-em (apply emf original-args))
         (new-em (apply emf new-args)))
    (if (eq original-em new-em)
      (if next-methods
        (apply next-methods new-args)
        (apply #'%no-next-method method new-args))
      (error-of-type 'error
        (TEXT "~S in ~S: the new arguments ~S have a different effective method than the old arguments ~S")
        'call-next-method gf new-args original-args))))


;; =================== Initialization and Reinitialization ===================

(defun make-generic-function (generic-function-class funname lambda-list argument-precedence-order method-combination method-class declspecs documentation
                              &rest methods)
  (let ((gf (make-generic-function-instance generic-function-class
              :name funname
              :lambda-list lambda-list
              :argument-precedence-order argument-precedence-order
              :method-class method-class
              :method-combination method-combination
              :declarations declspecs
              :documentation documentation)))
    (dolist (method methods) (std-add-method gf method))
    gf))

;; When this is true, it is possible to replace a non-generic function with
;; a generic function through DEFGENERIC or ENSURE-GENERIC-FUNCTION.
(defparameter *allow-making-generic* nil)

(defun ensure-generic-function-using-class-<t> (gf funname &rest all-keys
                                                &key (generic-function-class <standard-generic-function>)
                                                     lambda-list
                                                     argument-precedence-order
                                                     (method-class nil method-class-p)
                                                     method-combination
                                                     documentation
                                                     declarations
                                                     declare
                                                     environment
                                                     ((methods methods) nil) ; from DEFGENERIC
                                                &allow-other-keys)
  (declare (ignore lambda-list argument-precedence-order method-combination
                   documentation declarations declare environment methods))
  ;; Argument checks.
  (unless (function-name-p funname)
    (error-of-type 'program-error
      (TEXT "~S: the name of a function must be a symbol, not ~S")
      'ensure-generic-function-using-class funname))
  (unless (class-p generic-function-class)
    (if (symbolp generic-function-class)
      (setq generic-function-class (find-class generic-function-class))
      (error (TEXT "~S for generic-function ~S: generic-function-class ~S is neither a class or a symbol")
             'ensure-generic-function-using-class funname generic-function-class)))
  (unless (subclassp generic-function-class <generic-function>)
    (error (TEXT "~S for generic-function ~S: generic-function-class ~S is not a subclass of GENERIC-FUNCTION")
           'ensure-generic-function-using-class funname generic-function-class))
  ;; Preparation of initialization arguments.
  (setq all-keys (copy-list all-keys))
  (remf all-keys ':generic-function-class)
  (when method-class-p
    (unless (class-p method-class)
      (if (symbolp method-class)
        (setq method-class (find-class method-class))
        (error (TEXT "~S for generic-function ~S: method-class ~S is neither a class or a symbol")
               'ensure-generic-function-using-class funname method-class)))
    (setf (getf all-keys ':method-class) method-class))
  (if gf
    ;; Redefinition of a generic function.
    (progn
      ;; Take into account the new generic-function-class.
      (unless (eq (class-of gf) generic-function-class)
        ;; MOP p. 51 says that an error should be signalled in this case,
        ;; but ANSI CL says that CHANGE-CLASS is used to modify the GF.
        (change-class gf generic-function-class))
      (warn-if-gf-already-called gf)
      (apply #'reinitialize-instance gf all-keys)
      gf)
    ;; First definition of a generic function.
    (setf (fdefinition funname)
          (apply #'make-generic-function-instance
                 generic-function-class
                 :name funname
                 all-keys))))

;; Preliminary.
(defun ensure-generic-function-using-class (gf funname &rest args
                                            &key generic-function-class
                                                 lambda-list
                                                 argument-precedence-order
                                                 method-class
                                                 method-combination
                                                 documentation
                                                 declarations
                                                 declare
                                                 environment
                                            &allow-other-keys)
  (declare (ignore generic-function-class lambda-list argument-precedence-order
                   method-class method-combination documentation declarations
                   declare environment))
  (apply #'ensure-generic-function-using-class-<t> gf funname args))

;; MOP p. 49
(defun ensure-generic-function (funname &rest args
                                &key generic-function-class
                                     lambda-list
                                     argument-precedence-order
                                     method-class
                                     method-combination
                                     documentation
                                     declarations
                                     declare
                                     environment
                                &allow-other-keys)
  (declare (ignore generic-function-class lambda-list argument-precedence-order
                   method-class method-combination documentation declarations
                   declare environment))
  (unless (function-name-p funname)
    (error-of-type 'program-error
      (TEXT "~S: the name of a function must be a symbol, not ~S")
      'ensure-generic-function funname))
  (let ((result
          (apply #'ensure-generic-function-using-class
                 (if (fboundp funname)
                   (let ((gf (fdefinition funname)))
                     (if (typep-class gf <generic-function>)
                       gf
                       (if (not *allow-making-generic*)
                         (error-of-type 'program-error
                           (TEXT "~S: ~S does not name a generic function")
                           'ensure-generic-function funname)
                         nil)))
                   nil)
                 funname
                 args)))
    ; A check, to verify that user-defined methods on
    ; ensure-generic-function-using-class work as they should.
    (unless (typep-class result <generic-function>)
      (error (TEXT "Wrong ~S result for ~S: not a generic-function: ~S")
             'ensure-generic-function-using-class funname result))
    result))

#||
;; For GENERIC-FLET, GENERIC-LABELS
;; like make-generic-function, only that the dispatch-code is
;; installed immediately.
(defun make-generic-function-now (generic-function-class funname lambda-list argument-precedence-order method-combination method-class declspecs documentation
                                  &rest methods)
  (let ((gf (apply #'make-generic-function generic-function-class funname lambda-list argument-precedence-order method-combination method-class declspecs documentation methods)))
    (install-dispatch gf)
    gf))
||#


;; ================================ DEFMETHOD ================================

(defmacro defmethod (&whole whole-form
                     funname &rest method-description)
  (setq funname (sys::check-function-name funname 'defmethod))
  (multiple-value-bind (method-initargs-forms signature)
      (analyze-method-description 'defmethod whole-form funname method-description)
    `(LET ()
       (COMPILER::EVAL-WHEN-COMPILE
         (COMPILER::C-DEFUN ',funname ,signature nil 'DEFMETHOD))
       (DO-DEFMETHOD ',funname (LIST ,@method-initargs-forms)))))

(defun do-defmethod (funname method-or-initargs)
  (let* ((gf
           (if (fboundp funname)
             (let ((gf (fdefinition funname)))
               (if (typep-class gf <generic-function>)
                 gf
                 (error-of-type 'error
                   (TEXT "~S: ~S does not name a generic function")
                   'defmethod funname)))
             (setf (fdefinition funname)
                   ;; Create a GF compatible with the given method signature.
                   (multiple-value-bind (m-lambdalist m-signature)
                       (if (listp method-or-initargs)
                         (values (getf method-or-initargs ':lambda-list)
                                 (getf method-or-initargs 'signature))
                         (values (std-method-lambda-list method-or-initargs)
                                 (std-method-signature method-or-initargs)))
                     (let ((gf-lambdalist
                             (gf-lambdalist-from-first-method m-lambdalist m-signature)))
                       (make-generic-function-instance <standard-generic-function>
                         :name funname
                         :lambda-list gf-lambdalist
                         :method-class <standard-method>))))))
         (method
           (if (listp method-or-initargs)
             (apply #'make-method-instance (std-gf-default-method-class gf)
                    method-or-initargs)
             method-or-initargs)))
    (std-add-method gf method)
    method))

;;; (DECLAIM-METHOD function-name qualifier* spec-lambda-list)
;; does the compile-time side effects of
;; (DEFMETHOD function-name qualifier* spec-lambda-list ...)

(defmacro declaim-method (&whole whole-form
                          funname &rest method-description)
  (setq funname (sys::check-function-name funname 'declaim-method))
  (multiple-value-bind (method-initargs-forms signature)
      (analyze-method-description 'defmethod whole-form funname method-description)
    (declare (ignore method-initargs-forms))
    `(COMPILER::EVAL-WHEN-COMPILE
       (COMPILER::C-DEFUN ',funname ,signature nil 'DEFMETHOD))))


;; ====================== DEFGENERIC and similar Macros ======================

;;; For DEFGENERIC, GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS,
;;;     WITH-ADDED-METHODS
;; caller: symbol
;; whole-form: whole source form
;; funname: function name, symbol or (SETF symbol)
;; lambdalist: lambdalist of the generic function
;; options: (option*)
;; Returns 8 values:
;; 1. generic-function-class-form,
;; 2. signature,
;; 3. argument-precedence-order,
;; 4. method-combination-lambda, to be applied to the generic-function-class,
;; 5. method-class-form,
;; 6. declspecs,
;; 7. docstring,
;; 8. method-forms.
(defun analyze-defgeneric (caller whole-form funname lambdalist options)
  (setq funname (sys::check-function-name funname caller))
  ;; Parse the lambdalist:
  (analyze-defgeneric-lambdalist caller whole-form funname lambdalist)
  ;; Process the options:
  (let ((generic-function-classes nil)
        (method-forms '())
        (method-combinations nil)
        (method-classes nil)
        (argorders nil)
        (declares nil)
        (docstrings nil))
    (dolist (option options)
      (unless (listp option)
        (error-of-type 'ext:source-program-error
          :form whole-form
          :detail option
          (TEXT "~S ~S: not a ~S option: ~S")
          caller funname 'defgeneric option))
      (case (first option)
        (DECLARE
         ;; The DEFGENERIC description in ANSI CL is inconsistent. According to
         ;; the BNF syntax, multiple DECLARE options cannot be passed in a
         ;; single DEFGENERIC forms. However, it also says explicitly "The
         ;; declare option may be specified more than once..."
         #|
         (when declares
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             (TEXT "~S ~S: ~S may only be specified once.")
             caller funname 'declare))
         |#
         (check-gf-declspecs (rest option) 'declare
           #'(lambda (errorstring &rest arguments)
               (error (TEXT "~S ~S: ~A")
                      caller funname
                      (apply #'format nil errorstring arguments))))
         (setq declares
               (if declares
                 `(DECLARE ,@(append (rest declares) (rest option)))
                 option)))
        (:ARGUMENT-PRECEDENCE-ORDER
         (when argorders
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             (TEXT "~S ~S: ~S may only be specified once.")
             caller funname ':argument-precedence-order))
         (setq argorders option))
        (:DOCUMENTATION
         (unless (and (eql (length option) 2) (stringp (second option)))
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail option
             (TEXT "~S ~S: A string must be specified after ~S : ~S")
             caller funname ':documentation option))
         (when docstrings
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             (TEXT "~S ~S: Only one ~S string is allowed.")
             caller funname ':documentation))
         (setq docstrings (rest option)))
        (:METHOD-COMBINATION
         (when method-combinations
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             (TEXT "~S ~S: ~S may only be specified once.")
             caller funname ':method-combination))
         ;; The syntax for this option is
         ;;   (method-combination-name method-combination-argument*)
         ;; CLHS writes "method-combination" here instead of
         ;; "method-combination-name" but this is most probably a typo.
         ;; To be liberal, we also allow a method-combination instead of a
         ;; method-combination-name.
         (unless (>= (length option) 2)
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail option
             (TEXT "~S ~S: A method combination type name must be specified after ~S : ~S")
             caller funname ':method-combination option))
         (let ((method-combination-name (second option)))
           (unless (or (symbolp method-combination-name)
                       (typep method-combination-name <method-combination>))
             (error-of-type 'ext:source-program-error
               :form whole-form
               :detail method-combination-name
               (TEXT "~S ~S: Invalid method combination specification: ~S")
               caller funname option))
           (setq method-combinations (rest option))))
        (:GENERIC-FUNCTION-CLASS
         (unless (and (eql (length option) 2) (symbolp (second option)))
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail option
             (TEXT "~S ~S: A class name must be specified after ~S : ~S")
             caller funname ':generic-function-class option))
         (when generic-function-classes
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             (TEXT "~S ~S: Only one ~S option is allowed.")
             caller funname ':generic-function-class))
         (setq generic-function-classes (rest option)))
        (:METHOD-CLASS
         (unless (and (eql (length option) 2) (symbolp (second option)))
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail option
             (TEXT "~S ~S: A class name must be specified after ~S : ~S")
             caller funname ':method-class option))
         (when method-classes
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             (TEXT "~S ~S: Only one ~S option is allowed.")
             caller funname ':method-class))
         (setq method-classes (rest option)))
        (:METHOD
         (push (analyze-method-description caller whole-form funname (rest option))
               method-forms))
        (t (error-of-type 'ext:source-program-error
             :form whole-form
             :detail option
             (TEXT "~S ~S: invalid syntax in ~S option: ~S")
             caller funname 'defgeneric option))))
    ;; Check :argument-precedence-order :
    (multiple-value-bind (signature argument-precedence-order argorder)
        (check-gf-lambdalist+argorder lambdalist (rest argorders) argorders
          #'(lambda (detail errorstring &rest arguments)
              (error-of-type 'ext:source-program-error
                :form whole-form
                :detail detail
                (TEXT "~S ~S: ~A")
                caller funname (apply #'format nil errorstring arguments))))
      (declare (ignore argorder))
      ;; Default :method-combination is STANDARD:
      (unless method-combinations
        (setq method-combinations '(STANDARD)))
      (let ((generic-function-class-form
              (if generic-function-classes
                `(FIND-CLASS ',(first generic-function-classes))
                '<STANDARD-GENERIC-FUNCTION>))
            (method-class-form
              (if method-classes
                `(FIND-CLASS ',(first method-classes))
                '<STANDARD-METHOD>)))
        (values generic-function-class-form
                signature
                argument-precedence-order
                (let ((method-combination-name (car method-combinations))
                      (options (cdr method-combinations)))
                  `(LAMBDA (GF-CLASS)
                     ,(if (symbolp method-combination-name)
                        ;; We need a preliminary generic function because the
                        ;; method combination must be passed to
                        ;; ensure-generic-function, and the GF has not yet been
                        ;; created and initialized at this moment.
                        `(FIND-METHOD-COMBINATION
                           (MAKE-GENERIC-FUNCTION-INSTANCE GF-CLASS :NAME ',funname)
                           ',method-combination-name ',options)
                        `(METHOD-COMBINATION-WITH-OPTIONS ',funname ',method-combination-name ',options))))
                method-class-form
                ;; list of declspecs
                (cdr declares)
                ;; docstring or nil
                (car docstrings)
                ;; list of the method-forms
                (mapcar #'(lambda (method-initargs-forms)
                            `(MAKE-METHOD-INSTANCE ,method-class-form
                               ,@method-initargs-forms))
                  (nreverse method-forms)))))))

;; Parse a DEFGENERIC lambdalist:
;; lambdalist --> reqnum, req-vars, optnum, restp, keyp, keywords, allowp
(defun analyze-defgeneric-lambdalist (caller whole-form funname lambdalist)
  (multiple-value-bind (reqvars optvars rest keyp keywords keyvars allowp)
      (sys::analyze-generic-function-lambdalist lambdalist
        #'(lambda (detail errorstring &rest arguments)
            (error-of-type 'ext:source-program-error
              :form whole-form
              :detail detail
              (TEXT "~S ~S: invalid generic function lambda-list: ~A")
              caller funname (apply #'format nil errorstring arguments))))
    (declare (ignore keyvars))
    (values (length reqvars) reqvars (length optvars)
            (or (not (eql rest 0)) keyp) ; &key implies &rest
            keyp keywords allowp)))


;;; DEFGENERIC

(defmacro defgeneric (&whole whole-form
                      funname lambda-list &rest options)
  (multiple-value-bind (generic-function-class-form signature argument-precedence-order method-combination-lambda method-class-form declspecs docstring method-forms)
      (analyze-defgeneric 'defgeneric whole-form funname lambda-list options)
    (let ((generic-function-class-var (gensym)))
      `(LET ()
         (DECLARE (SYS::IN-DEFUN ,funname))
         (COMPILER::EVAL-WHEN-COMPILE
           (COMPILER::C-DEFUN ',funname ',signature nil 'DEFGENERIC))
         ;; NB: no (SYSTEM::REMOVE-OLD-DEFINITIONS ',funname)
         (LET ((,generic-function-class-var ,generic-function-class-form))
           (APPLY #'ENSURE-GENERIC-FUNCTION ',funname
             ;; Here we pass :GENERIC-FUNCTION-CLASS, :ARGUMENT-PRECEDENCE-ORDER,
             ;; :METHOD-CLASS, :DOCUMENTATION also if the corresponding option
             ;; wasn't specified in the DEFGENERIC form, because when a generic
             ;; function is being redefined, :DOCUMENTATION NIL means to erase
             ;; the documentation string, while nothing means to keep it!
             ;; See MOP p. 61.
             :GENERIC-FUNCTION-CLASS ,generic-function-class-var
             :LAMBDA-LIST ',lambda-list
             :ARGUMENT-PRECEDENCE-ORDER ',argument-precedence-order
             :METHOD-CLASS ,method-class-form
             :METHOD-COMBINATION (,method-combination-lambda ,generic-function-class-var)
             :DOCUMENTATION ',docstring
             :DECLARATIONS ',declspecs
             'METHODS (LIST ,@method-forms)
             ;; Pass the default initargs of the generic-function class, in
             ;; order to erase leftovers from the previous definition.
             (MAPCAP #'(LAMBDA (X) (LIST (FIRST X) (FUNCALL (THIRD X))))
                     (CLASS-DEFAULT-INITARGS ,generic-function-class-var))))))))

;; ============================================================================

;; For GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS

;; Transform lambdalist into calling convention for the compiler:
(defun defgeneric-lambdalist-callinfo (caller whole-form funname lambdalist)
  (multiple-value-bind (reqnum req-vars optnum restp keyp keywords allowp)
      (analyze-defgeneric-lambdalist caller whole-form funname lambdalist)
    (declare (ignore req-vars keyp))
    (callinfo reqnum optnum restp keywords allowp)))

(defun make-generic-function-form (caller whole-form funname lambda-list options)
  (multiple-value-bind (generic-function-class-form signature argument-precedence-order method-combination-lambda method-class-form declspecs docstring method-forms)
      (analyze-defgeneric caller whole-form funname lambda-list options)
    (declare (ignore signature))
    (let ((generic-function-class-var (gensym)))
      `(LET ((,generic-function-class-var ,generic-function-class-form))
         (MAKE-GENERIC-FUNCTION ,generic-function-class-var ',funname ',lambda-list ',argument-precedence-order (,method-combination-lambda ,generic-function-class-var) ,method-class-form ',declspecs ',docstring
                                ,@method-forms)))))

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

;; ============================================================================
