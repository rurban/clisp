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


;; ======================== The Set of Methods of a GF ========================

;; Cruel hack (CLtL2 28.1.9.2., ANSI CL 7.1.2.):
;; - MAKE-INSTANCE must be informed about the methods of ALLOCATE-INSTANCE,
;;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - INITIALIZE-INSTANCE must be informed about the methods of
;;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - REINITIALIZE-INSTANCE must be informed about the methods of
;;   REINITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - UPDATE-INSTANCE-FOR-REDEFINED-CLASS must be informed about the methods of
;;   UPDATE-INSTANCE-FOR-REDEFINED-CLASS and SHARED-INITIALIZE.
;; - UPDATE-INSTANCE-FOR-DIFFERENT-CLASS must be informed about the methods of
;;   UPDATE-INSTANCE-FOR-DIFFERENT-CLASS and SHARED-INITIALIZE.
(defvar |#'allocate-instance| nil)
(defvar |#'initialize-instance| nil)
(defvar |#'reinitialize-instance| nil)
(defvar |#'update-instance-for-redefined-class| nil)
(defvar |#'update-instance-for-different-class| nil)
(defvar |#'shared-initialize| nil)
(defvar *gf-warn-on-replacing-method* t)

;; CLtL2 28.1.6.3., ANSI CL 7.6.3.
;; Agreement on Parameter Specializers and Qualifiers
(defun methods-agree-p (method1 method2)
  (and (equal (std-method-qualifiers method1) (std-method-qualifiers method2))
       (specializers-agree-p (std-method-specializers method1)
                             (std-method-specializers method2))))

;; CLtL2 28.1.6.4., ANSI CL 7.6.4. Congruent Lambda-lists
(defun check-signature-congruence (gf method &optional
                                   (gf-sign (std-gf-signature gf))
                                   (m-sign (std-method-signature method)))
  (unless (= (sig-req-num m-sign) (sig-req-num gf-sign))
    (error-of-type 'error
      (TEXT "~S has ~D, but ~S has ~D required parameter~:P")
      method (sig-req-num m-sign) gf (sig-req-num gf-sign)))
  (unless (= (sig-opt-num m-sign) (sig-opt-num gf-sign))
    (error-of-type 'error
      (TEXT "~S has ~D, but ~S has ~D optional parameter~:P")
      method (sig-opt-num m-sign) gf (sig-opt-num gf-sign)))
  (when (and (sig-rest-p m-sign) (not (sig-rest-p gf-sign)))
    (error-of-type 'error
      (TEXT "~S accepts &REST or &KEY, but ~S does not.")
      method gf))
  (when (and (sig-rest-p gf-sign) (not (sig-rest-p m-sign)))
    (error-of-type 'error
      (TEXT "~S accepts &REST or &KEY, but ~S does not.")
      gf method))
  (when (sig-keys-p gf-sign)    ; gf has keywords?
    ;; yes ==> method must accept it
    (unless (if (sig-keys-p m-sign)
              (or (sig-allow-p m-sign) ; keywords match
                  (subsetp (sig-keywords gf-sign) (sig-keywords m-sign)))
              (sig-rest-p m-sign)) ; method must have &rest!
      (error-of-type 'error
        (TEXT "~S does not accept the keywords ~S of ~S")
        method (sig-keywords gf-sign) gf))))

;; CLtL2 28.1.7.2., 28.1.7.4., ANSI CL 7.6.6.2., 7.6.6.4. Method qualifiers
(defun check-method-qualifiers (gf method
                                &optional (method-combo (std-gf-method-combination gf)))
  (funcall (method-combination-check-method-qualifiers method-combo)
           gf method-combo method))
(defun invalid-method-qualifiers-error (gf method)
  (error-of-type 'program-error
    (TEXT "~S method combination, used by ~S, does not allow the method qualifiers ~:S: ~S")
    (method-combination-name (std-gf-method-combination gf)) gf
    (std-method-qualifiers method) method))

;; Add a method to a generic function.
(defun std-add-method (gf method)
  (check-signature-congruence gf method)
  (when (std-method-generic-function method)
    (error-of-type 'error
      "~S: ~S already belongs to ~S, cannot also add it to ~S"
      'std-add-method method (std-method-generic-function method) gf))
  (check-method-qualifiers gf method)
  (setf (std-method-fast-function method) nil
        (std-method-generic-function method) gf)
  ;; Determine function from initfunction:
  (when (and (null (std-method-function method))
             (null (std-method-fast-function method)))
    (let ((h (funcall (std-method-initfunction method) method)))
      (setf (std-method-fast-function method) (car h))
      (when (car (cdr h)) ; could the variable ",cont" be optimized away?
        (setf (std-method-wants-next-method-p method) nil))))
  ;; The method is finished. Now add it:
  (warn-if-gf-already-called gf)
  (let ((old-method (find method (std-gf-methods gf) :test #'methods-agree-p)))
    (cond ((eq gf |#'allocate-instance|) (note-ai-change method))
          ((eq gf |#'initialize-instance|) (note-ii-change method))
          ((eq gf |#'reinitialize-instance|) (note-ri-change method))
          ((eq gf |#'update-instance-for-redefined-class|) (note-uirc-change method))
          ((eq gf |#'update-instance-for-different-class|) (note-uidc-change method))
          ((eq gf |#'shared-initialize|) (note-si-change method)))
    (setf (std-gf-methods gf)
          (cons method
                (if old-method
                  (progn
                    (when *gf-warn-on-replacing-method*
                      (warn (TEXT "Replacing method ~S in ~S")
                            old-method gf))
                    (remove old-method (std-gf-methods gf)))
                  (std-gf-methods gf))))
    (setf (std-gf-effective-method-cache gf) '())
    (finalize-fast-gf gf))
  ;; It's not worth updating the seclass of a generic function, since 1. in
  ;; most cases, it can signal a NO-APPLICABLE-METHOD error and thus has
  ;; *seclass-dirty*, 2. the compiler must assume that the seclass doesn't
  ;; change over time, which we cannot guarantee, since generic functions are
  ;; not sealed.
  gf)

;; Remove a method from a generic function.
(defun std-remove-method (gf method)
  (let ((old-method (find (std-method-initfunction method) (std-gf-methods gf)
                          :key #'std-method-initfunction)))
    (when old-method
      (warn-if-gf-already-called gf)
      (when (need-gf-already-called-warning-p gf)
        (warn (TEXT "Removing method ~S in ~S")
              old-method gf))
      (cond ((eq gf |#'allocate-instance|) (note-ai-change method))
            ((eq gf |#'initialize-instance|) (note-ii-change method))
            ((eq gf |#'reinitialize-instance|) (note-ri-change method))
            ((eq gf |#'update-instance-for-redefined-class|) (note-uirc-change method))
            ((eq gf |#'update-instance-for-different-class|) (note-uidc-change method))
            ((eq gf |#'shared-initialize|) (note-si-change method)))
      (setf (std-gf-methods gf) (remove old-method (std-gf-methods gf))
            (std-method-generic-function old-method) nil
            (std-method-from-defgeneric old-method) nil)
      (setf (std-gf-effective-method-cache gf) '())
      (finalize-fast-gf gf)))
  gf)

;; Find a method in a generic function.
(defun std-find-method (gf qualifiers specializers &optional (errorp t))
  (let ((n (sig-req-num (std-gf-signature gf))))
    (unless (listp specializers)
      (error-of-type 'error
        (TEXT "~S: the specializers argument is not a list: ~S")
        'find-method specializers))
    (unless (eql (length specializers) n)
      (error-of-type 'error
        (TEXT "~S: the specializers argument has length ~D, but ~S has ~D required parameter~:P")
        'find-method (length specializers) gf n))
    ; Convert (EQL object) -> #<EQL-SPECIALIZER object>:
    (setq specializers
          (mapcar #'(lambda (specializer)
                      (if (and (consp specializer) (eq (car specializer) 'EQL)
                               (consp (cdr specializer)) (null (cddr specializer)))
                        (intern-eql-specializer (second specializer))
                        specializer))
                  specializers)))
  ;; Simulate
  ;;   (find hypothetical-method (std-gf-methods gf) :test #'methods-agree-p)
  ;; cf. methods-agree-p
  (dolist (method (std-gf-methods gf))
    (when (and (equal (std-method-qualifiers method) qualifiers)
               (specializers-agree-p (std-method-specializers method)
                                     specializers))
      (return-from std-find-method method)))
  (if errorp
    (error-of-type 'error
      (TEXT "~S has no method with qualifiers ~:S and specializers ~:S")
      gf qualifiers specializers)
    nil))


;; =================== Initialization and Reinitialization ===================

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
                     (let* ((req-num (sig-req-num m-signature))
                            (opt-num (sig-opt-num m-signature))
                            (rest-p (sig-rest-p m-signature))
                            (gf-lambdalist (append (subseq m-lambdalist 0 req-num)
                                                   (if (> opt-num 0)
                                                     (cons '&OPTIONAL
                                                       (mapcar #'(lambda (item) (if (consp item) (first item) item))
                                                               (subseq m-lambdalist (+ req-num 1) (+ req-num 1 opt-num))))
                                                     '())
                                                   (if rest-p
                                                     (list '&REST
                                                           (let ((i (position '&REST m-lambdalist)))
                                                             (if i (nth (+ i 1) m-lambdalist) (gensym))))
                                                     '()))))
                         ; gf-lambdalist's signature is
                         ; (make-signature :req-num req-num :opt-num opt-num :rest-p rest-p).
                         (make-fast-gf <standard-generic-function> funname gf-lambdalist (subseq m-lambdalist 0 req-num) <standard-method> '() nil))))))
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
;; --> generic-function-class-form, signature, argument-precedence-order, method combination, method-class-form, declspecs, docstring, method-forms
(defun analyze-defgeneric (caller whole-form funname lambdalist options)
  (setq funname (sys::check-function-name funname caller))
  ;; Parse the lambdalist:
  (analyze-defgeneric-lambdalist caller whole-form funname lambdalist)
  ;; Process the options:
  (let ((generic-function-classes nil)
        (method-forms '())
        (method-combination 'STANDARD)
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
         ;; The method combination designator must be present and may not be
         ;; null. Allowed are STANDARD without options, a symbol with options,
         ;; or, in the case of invocation from ensure-generic-function,
         ;; a method-combination object with options.
         (let ((designator (cadr option)))
           (if (or (typep designator <method-combination>)
                   (and designator (symbolp designator)))
             (setq method-combination (rest option))
             (error-of-type 'ext:source-program-error
               :form whole-form
               :detail designator
               (TEXT "~S ~S: Invalid method combination: ~S")
               caller funname option))))
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
                method-combination
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
  (multiple-value-bind (generic-function-class-form signature argument-precedence-order method-combo method-class-form declspecs docstring method-forms)
      (analyze-defgeneric 'defgeneric whole-form funname lambda-list options)
    `(LET ()
       (DECLARE (SYS::IN-DEFUN ,funname))
       (COMPILER::EVAL-WHEN-COMPILE
         (COMPILER::C-DEFUN ',funname ',signature nil 'DEFGENERIC))
       ;; NB: no (SYSTEM::REMOVE-OLD-DEFINITIONS ',funname)
       (DO-DEFGENERIC ',funname ,generic-function-class-form ',lambda-list ',signature ',argument-precedence-order ',method-combo ,method-class-form ',declspecs ',docstring
                      ,@method-forms))))

(defun ensure-generic-function (function-name &key argument-precedence-order
                                declare declarations documentation environment
                                generic-function-class lambda-list
                                method-class method-combination)
  (declare (ignore environment))
  (multiple-value-bind (generic-function-class-form signature argument-precedence-order method-combo method-class-form declspecs docstring method-forms)
      (analyze-defgeneric
       'defgeneric nil function-name lambda-list
       `(,@(if (or declare declarations) `((declare ,@(append declare declarations))))
         ,@(if documentation `((:documentation ,documentation)))
         ,@(if argument-precedence-order
               `((:argument-precedence-order ,argument-precedence-order)))
         ,@(if generic-function-class
              `((:generic-function-class
                 ,(if (class-p generic-function-class)
                      (class-name generic-function-class)
                      generic-function-class))))
         ,@(if method-combination `((:method-combination ,method-combination)))
         ,@(if method-class `((:method-class ,method-class)))))
    (declare (ignore declspecs docstring method-forms))
    (do-defgeneric function-name (eval generic-function-class-form) lambda-list signature argument-precedence-order method-combo (eval method-class-form) (append declare declarations) documentation)))

;; ============================================================================

;; For GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS

;; Transform lambdalist into calling convention for the compiler:
(defun defgeneric-lambdalist-callinfo (caller whole-form funname lambdalist)
  (multiple-value-bind (reqnum req-vars optnum restp keyp keywords allowp)
      (analyze-defgeneric-lambdalist caller whole-form funname lambdalist)
    (declare (ignore req-vars keyp))
    (callinfo reqnum optnum restp keywords allowp)))

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

;; ============================================================================
