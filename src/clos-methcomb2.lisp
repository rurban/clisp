;;;; Common Lisp Object System for CLISP: Method Combination
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; James Anderson 2003

(in-package "CLOS")


;;; ---------------------------- Method Selection ----------------------------

;; CLtL2 28.1.6.2., ANSI CL 7.6.2. Applicable methods
(defun method-applicable-p (method required-arguments)
  (every #'typep required-arguments
         (std-method-parameter-specializers method)))

;; CLtL2 28.1.7.1., ANSI CL 7.6.6.1.2.
;; Sorting the applicable methods by precedence order
;; > methods: A list of methods from the same generic function that are
;;            already known to be applicable for the given required-arguments.
;; > required-arguments: The list of required arguments.
;; > argument-order: A list of indices in the range 0..req-num-1 that
;;                   determines the argument order.
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


;;; ----------------------- General Method Combination -----------------------

(defvar *method-combination-arguments* nil
  "The actual generic function call arguments (in compute-effective-method)" )
(defvar *method-combination-generic-function* nil
  "The generic function applied (in compute-effective-method)")
(defvar *method-combination* nil
  "The generic function's method combination (in compute-effective-method)")

;; Error about a method whose qualifiers don't fit with a method-combination.
;; This is specified to be a function, not a condition type, because it is
;; meant to be called from a DEFINE-METHOD-COMBINATION's body.
(defun invalid-method-error (method format-string &rest args)
  (error
    (TEXT "For function ~S applied to argument list ~S:~%While computing the effective method through ~S:~%Invalid method: ~S~%~?")
    *method-combination-generic-function* *method-combination-arguments*
    *method-combination*
    method
    format-string args))

;; Other error during method combination, not tied to a particular method.
;; This is specified to be a function, not a condition type, because it is
;; meant to be called from a DEFINE-METHOD-COMBINATION's body.
;; The fact that MISSING-REQUIRED-METHOD and NO-PRIMARY-METHOD don't call this
;; function is not a problem, because the user is not supposed to redefine or
;; customize this function.
(defun method-combination-error (format-string &rest args)
  (error
    (TEXT "For function ~S applied to argument list ~S:~%While computing the effective method through ~S:~%Impossible to combine the methods:~%~?")
    *method-combination-generic-function* *method-combination-arguments*
    *method-combination*
    format-string args))

(defun invalid-method-sort-order-error (order-form order-value)
  (method-combination-error
    (TEXT "The value of ~S is ~S, should be :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST.")
    order-form order-value))

(defun invalid-sort-order-error (order-form order-value)
  (error-of-type 'sys::source-program-error
    (TEXT "The value of ~S is ~S, should be :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST.")
    order-form order-value))

(defun any-method-combination-check-options (gf-name combination options checker)
  (locally (declare (compile))
    (sys::%handler-bind
        ((program-error
           #'(lambda (err)
               (error-of-type 'sys::source-program-error
                 (TEXT "~S ~S: Invalid method-combination options ~S for ~S: ~A")
                 'defgeneric gf-name options combination err))))
      (apply checker options))))

;; Adds the function-macro definitions of CALL-NEXT-METHOD and NEXT-METHOD-P.
(defun add-next-method-local-functions (self cont req-dummies rest-dummy body)
  `(SYSTEM::FUNCTION-MACRO-LET
     ((CALL-NEXT-METHOD
        ((&REST NEW-ARGS)
         (IF NEW-ARGS
           ;; argument checking in the interpreter only
           (IF (EVAL-WHEN (EVAL) T)
             (%CALL-NEXT-METHOD
               ,self
               ,cont
               ,(if rest-dummy
                  `(LIST* ,@req-dummies ,rest-dummy)
                  `(LIST ,@req-dummies))
               NEW-ARGS)
             (IF ,cont
               (APPLY ,cont NEW-ARGS)
               (APPLY (FUNCTION %NO-NEXT-METHOD) ,self NEW-ARGS)))
           ,(if rest-dummy
              `(IF ,cont
                 (APPLY ,cont ,@req-dummies ,rest-dummy)
                 (APPLY (FUNCTION %NO-NEXT-METHOD) ,self
                        ,@req-dummies ,rest-dummy))
              `(IF ,cont
                 (FUNCALL ,cont ,@req-dummies)
                 (%NO-NEXT-METHOD ,self ,@req-dummies)))))
        ((&REST NEW-ARG-EXPRS)
         (IF NEW-ARG-EXPRS
           ;; argument checking in the interpreter only
           (LIST 'IF '(EVAL-WHEN (EVAL) T)
             (LIST '%CALL-NEXT-METHOD
               ',self
               ',cont
               (LIST ',(if rest-dummy 'LIST* 'LIST)
                 ,@(mapcar #'(lambda (x) `',x) req-dummies)
                 ,@(if rest-dummy `(',rest-dummy) '()))
               (CONS 'LIST NEW-ARG-EXPRS))
             (LIST 'IF ',cont
               (LIST* 'FUNCALL ',cont NEW-ARG-EXPRS)
               (LIST* '%NO-NEXT-METHOD ',self NEW-ARG-EXPRS)))
           ,(if rest-dummy
              `(LIST 'IF ',cont
                 (LIST 'APPLY ',cont
                   ,@(mapcar #'(lambda (x) `',x) req-dummies)
                   ',rest-dummy)
                 (LIST 'APPLY '(FUNCTION %NO-NEXT-METHOD)
                   ',self
                   ,@(mapcar #'(lambda (x) `',x) req-dummies)
                   ',rest-dummy))
              `(LIST 'IF ',cont
                 (LIST 'FUNCALL ',cont
                   ,@(mapcar #'(lambda (x) `',x) req-dummies))
                 (LIST '%NO-NEXT-METHOD
                   ',self
                   ,@(mapcar #'(lambda (x) `',x) req-dummies)))))))
      (NEXT-METHOD-P
        (() ,cont)
        (() ',cont)))
     ,@body))

(defmacro call-method (method &optional next-methods-list)
  (declare (ignore method next-methods-list))
  (error-of-type 'sys::source-program-error
    (TEXT "~S is possible only from within the context of an effective method function. See ~S.")
    'call-method 'define-method-combination))

(defmacro make-method (form)
  (declare (ignore form))
  (error-of-type 'sys::source-program-error
    (TEXT "~S is possible only at particular places from within the context of an effective method function. See ~S.")
    'make-method 'define-method-combination))

(defun make-method-error (whole-form)
  (error-of-type 'sys::source-program-error
    (TEXT "~S cannot be used here: ~S")
    'make-method whole-form))

(defun call-method-arg1-error (whole-form)
  (error-of-type 'sys::source-program-error
    (TEXT "~S: The first argument is neither a method nor a (MAKE-METHOD ...) form: ~S")
    'call-method whole-form))

(defun call-method-arg2-error (whole-form)
  (error-of-type 'sys::source-program-error
    (TEXT "~S: The second argument is not a list: ~S")
    'call-method whole-form))

;; Returns pieces of code to be used in the expansion of the effective-method.
;; 1. the lambda-list of the effective-method.
;; 2. the part of the lambda-list responsible for keyword checking.
;; 3. a declarations/forms list to use right after the lambda-list.
;; 4. an application primitive to use with argument lists for the methods.
;; 5. a list of forms representing the arguments to pass to methods.
;; 6. a set of macro definitions that defines local macros.
(defun effective-method-code-bricks (gf methods)
  (let* ((signature (gf-signature gf))
         (req-num (sig-req-num signature))
         (req-vars (gensym-list req-num))
         (restp (gf-sig-restp signature))
         (rest-var (if restp (gensym)))
         (apply-fun (if restp 'APPLY 'FUNCALL))
         (apply-args `(,@req-vars ,@(if restp `(,rest-var) '())))
         (lambdalist `(,@req-vars ,@(if restp `(&REST ,rest-var) '()))))
    (multiple-value-bind (opt-vars key-vars lambdalist-keypart)
        (gf-keyword-arguments restp signature methods)
      (values
        ;; 1. lambda-list
        (if (null opt-vars)
          (append lambdalist lambdalist-keypart)
          lambdalist)
        ;; 2. lambda-list &key part
        lambdalist-keypart
        ;; 3. declarations and first forms
        (if (null opt-vars)
          (if key-vars `((DECLARE (IGNORE ,@key-vars))) '())
          `((APPLY #'(LAMBDA (&OPTIONAL ,@opt-vars ,@lambdalist-keypart)
                       (DECLARE (IGNORE ,@opt-vars ,@key-vars)))
                   ,rest-var)))
        ;; 4. application primitive
        apply-fun
        ;; 5. list of forms representing the argument
        apply-args
        ;; 6. macro definitions
        `((MAKE-METHOD (&WHOLE WHOLE FORM)
            (DECLARE (IGNORE FORM))
            (MAKE-METHOD-ERROR WHOLE))
          (CALL-METHOD (&WHOLE WHOLE METHOD &OPTIONAL NEXT-METHODS-LIST)
            (UNLESS (OR (TYPEP METHOD <METHOD>)
                        (AND (CONSP METHOD) (EQ (CAR METHOD) 'MAKE-METHOD)
                             (CONSP (CDR METHOD)) (NULL (CDDR METHOD))))
              (CALL-METHOD-ARG1-ERROR WHOLE))
            (UNLESS (LISTP NEXT-METHODS-LIST)
              (CALL-METHOD-ARG2-ERROR WHOLE))
            (LET ((NEXT-METHODS-EM-FORM
                    (IF NEXT-METHODS-LIST
                      (LIST 'FUNCTION
                        (LIST 'LAMBDA ',lambdalist
                          (LIST 'CALL-METHOD (CAR NEXT-METHODS-LIST)
                            (CDR NEXT-METHODS-LIST))))
                      'NIL)))
              (IF (TYPEP METHOD <METHOD>)
                (IF (STD-METHOD-WANTS-NEXT-METHOD-P METHOD)
                  (LIST* ',apply-fun (LIST 'QUOTE (STD-METHOD-FUNCTION METHOD))
                         NEXT-METHODS-EM-FORM ',apply-args)
                  ; Some methods are known a-priori to not use the next-method list.
                  (LIST* ',apply-fun (LIST 'QUOTE (STD-METHOD-FUNCTION METHOD))
                         ',apply-args))
                (LET ((CONT (GENSYM)))
                  (LIST 'LET (LIST (LIST CONT NEXT-METHODS-EM-FORM))
                    (LIST 'DECLARE (LIST 'IGNORABLE CONT))
                    (ADD-NEXT-METHOD-LOCAL-FUNCTIONS 'NIL CONT ',req-vars ',rest-var
                      (CDR METHOD))))))))))))

(defun build-effective-method-function-form (generic-function combination methods
                                             effective-method-form
                                             combination-arguments-lambda-list)
  "Given the generic function, its combination, and the effective method form
and the arguments-lambda-list specifying variables for it, constructs the
function form for the effective method, including correct arguments and with
the next-method support."
  (multiple-value-bind (lambdalist lambdalist-keypart firstforms apply-fun apply-args macrodefs)
      (effective-method-code-bricks generic-function methods)
    (declare (ignore lambdalist-keypart))
    (let* ((declarations (method-combination-declarations combination))
           (ef-fun
             (if (and (consp effective-method-form)
                      (eq (first effective-method-form) 'CALL-METHOD)
                      (consp (cdr effective-method-form))
                      (typep (second effective-method-form) <method>)
                      (not (std-method-wants-next-method-p (second effective-method-form))))
               (std-method-function (second effective-method-form))
               (let ((wrapped-ef-form
                       `(MACROLET ,macrodefs
                          ,effective-method-form)))
                 (when combination-arguments-lambda-list
                   ;; Use an inline lambda to assign values to the variables
                   ;; of the combination-arguments-lambda-list.
                   (let ((whole-var nil)
                         (whole-form nil))
                     (when (eq (first combination-arguments-lambda-list) '&WHOLE)
                       (setq whole-var (second combination-arguments-lambda-list))
                       (setq whole-form (list* (ecase apply-fun
                                                 (APPLY 'LIST*)
                                                 (FUNCALL 'LIST))
                                               apply-args))
                       (setq combination-arguments-lambda-list
                             (cddr combination-arguments-lambda-list)))
                     (multiple-value-bind (reqvars optvars optinits optsvars rest
                                           keyp keywords keyvars keyinits keysvars
                                           allowp auxvars auxinits)
                         (analyze-lambdalist combination-arguments-lambda-list
                           #'(lambda (errorstring &rest arguments)
                               (error (TEXT "In ~S ~S lambda list: ~A")
                                      combination ':arguments
                                      (apply #'format nil errorstring arguments))))
                       (declare (ignore optinits optsvars 
                                        keywords keyvars keyinits keysvars
                                        allowp auxvars auxinits))
                       ;; The combination-arguments-lambda-list has an implicit
                       ;; &ALLOW-OTHER-KEYS.
                       (when (and (memq '&KEY combination-arguments-lambda-list)
                                  (not (memq '&ALLOW-OTHER-KEYS combination-arguments-lambda-list)))
                         (let ((i (or (position '&AUX combination-arguments-lambda-list)
                                      (length combination-arguments-lambda-list))))
                           (setq combination-arguments-lambda-list
                                 (append (subseq combination-arguments-lambda-list 0 i)
                                         '(&ALLOW-OTHER-KEYS)
                                         (subseq combination-arguments-lambda-list i)))))
                       (let* ((ll-req-num (length reqvars))
                              (ll-opt-num (length optvars))
                              (signature (gf-signature generic-function))
                              (gf-req-num (sig-req-num signature))
                              (gf-opt-num (sig-opt-num signature)))
                         ;; "If the section of the :arguments lambda-list is
                         ;;  shorter, extra arguments are ignored."
                         (when (< ll-req-num gf-req-num)
                           (setq apply-args (append (subseq apply-args 0 ll-req-num)
                                                    (subseq apply-args gf-req-num))))
                         ;; "If the section of the :arguments lambda-list is
                         ;;  longer, excess required parameters are bound to
                         ;;  forms that evaluate to nil and excess optional
                         ;;  parameters are bound to their initforms."
                         (when (> ll-req-num gf-req-num)
                           (setq apply-args (append (subseq apply-args 0 gf-req-num)
                                                    (make-list (- ll-req-num gf-req-num)
                                                               :initial-element 'NIL)
                                                    (subseq apply-args gf-req-num))))
                         ;; Now the required parameters section of apply-args
                         ;; has length ll-req-num.
                         ;; Likewise for the &optional section.
                         (when (< ll-opt-num gf-opt-num)
                           (let* ((has-&optional (eq (nth ll-req-num combination-arguments-lambda-list) '&OPTIONAL))
                                  (i (+ ll-req-num (if has-&optional 1 0) ll-opt-num)))
                             (setq combination-arguments-lambda-list
                                   (append (subseq combination-arguments-lambda-list 0 i)
                                           (if has-&optional '() '(&OPTIONAL))
                                           (gensym-list (- gf-opt-num ll-opt-num))
                                           (subseq combination-arguments-lambda-list i)))))
                         (when (> ll-opt-num gf-opt-num)
                           ;; In this case we have to split the one lambda into
                           ;; two or three ones.
                           ;; Outermost lambda: the required and present optional
                           ;;                   variables.
                           ;; Inner lambda: The missing optional variables.
                           ;; Innermost lambda: The &rest/&key variables.
                           (let ((combination-arguments-rest
                                   (subseq combination-arguments-lambda-list (+ ll-req-num 1 ll-opt-num)))
                                 (apply-args-rest (subseq apply-args ll-req-num)))
                             (when (memq (first combination-arguments-rest) '(&REST &KEY))
                               (setq wrapped-ef-form
                                     `(,apply-fun #'(LAMBDA ,(append (if (> gf-opt-num 0) '(&OPTIONAL) '())
                                                                     (gensym-list gf-opt-num)
                                                                     combination-arguments-rest)
                                                      ,@declarations
                                                      ,wrapped-ef-form)
                                                  ,@apply-args-rest))
                               (setq combination-arguments-lambda-list
                                     (subseq combination-arguments-lambda-list 0 (+ ll-req-num 1 ll-opt-num))))
                             (setq wrapped-ef-form
                                   `(FUNCALL #'(LAMBDA (&OPTIONAL ,@(subseq combination-arguments-lambda-list (+ ll-req-num 1 gf-opt-num)))
                                                 ,@declarations
                                                 ,wrapped-ef-form)))
                             (setq combination-arguments-lambda-list
                                   (subseq combination-arguments-lambda-list 0 (+ ll-req-num 1 gf-opt-num)))
                             (when (memq (first combination-arguments-rest) '(&REST &KEY))
                               (setq combination-arguments-lambda-list
                                     (append combination-arguments-lambda-list `(&REST ,(gensym)))))))
                         ;; When lambdalist has &rest or &key but combination-arguments-lambda-list
                         ;; doesn't, add a dummy &rest variable to it.
                         (when (and (eq apply-fun 'APPLY)
                                    (not (or (not (eql rest 0)) keyp)))
                           (let ((i (or (position '&AUX combination-arguments-lambda-list)
                                        (length combination-arguments-lambda-list))))
                             (setq combination-arguments-lambda-list
                                   (append (subseq combination-arguments-lambda-list 0 i)
                                           `(&REST ,(gensym))
                                           (subseq combination-arguments-lambda-list i)))))
                         ;; "&whole var can be placed first in the :arguments lambda-list."
                         (when whole-form
                           (setq combination-arguments-lambda-list
                                 (cons whole-var combination-arguments-lambda-list))
                           (setq apply-args (cons whole-form apply-args)))
                         (setq wrapped-ef-form
                               `(,apply-fun #'(LAMBDA ,combination-arguments-lambda-list
                                                ,@declarations
                                                ,wrapped-ef-form)
                                            ,@apply-args))))))
                 `#'(LAMBDA ,lambdalist
                      ,@declarations
                      ,@firstforms
                      ,wrapped-ef-form)))))
      ef-fun)))

(defun compute-effective-method-as-function-form (gf combination methods)
  ;; Apply method combination:
  (multiple-value-bind (effective-method-form effective-method-options)
      (funcall (method-combination-expander combination)
               gf combination (method-combination-options combination) methods)
    ;; Build a function form around the inner form:
    (build-effective-method-function-form gf combination methods
      effective-method-form
      (cdr (assoc ':ARGUMENTS effective-method-options)))))

;;; ----------------------- Standard Method Combination -----------------------

(defun standard-method-combination-check-options (gf-name combination options)
  (declare (ignore combination))
  (unless (null options)
    (error-of-type 'sys::source-program-error
      (TEXT "~S ~S: The ~S method combination permits no options: ~S")
      'defgeneric gf-name 'standard options)))

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

(defun standard-method-combination-expander (gf combination options methods)
  (declare (ignore combination))
  (declare (ignore options)) ; already checked in check-options
  ;; Split up into individual method types.
  (multiple-value-bind (primary-methods before-methods after-methods around-methods)
      (partition-method-list methods)
    (when (null primary-methods)
      (return-from standard-method-combination-expander
        (let ((rest-variable (gensym)))
          (values `(APPLY #'NO-PRIMARY-METHOD ',gf ,rest-variable)
                  `((:ARGUMENTS &WHOLE ,rest-variable))))))
    ;; Combine methods into an "effective method":
    (labels ((ef-1 (primary-methods before-methods after-methods
                    around-methods)
               (if (null around-methods)
                 (ef-2 primary-methods before-methods after-methods)
                 (let ((next-ef
                         (ef-1 primary-methods before-methods
                               after-methods (rest around-methods))))
                   `(CALL-METHOD ,(first around-methods)
                      ,(list `(MAKE-METHOD ,next-ef))))))
             (forms-for-invoking-sequentially (methods)
               (mapcar #'(lambda (method)
                           `(CALL-METHOD ,method))
                       methods))
             (ef-2 (primary-methods before-methods after-methods)
               (let ((next-ef (ef-3 primary-methods after-methods)))
                 (if (null before-methods)
                   next-ef
                   `(PROGN
                      ; most-specific-first:
                      ,@(forms-for-invoking-sequentially before-methods)
                      ,next-ef))))
             (ef-3 (primary-methods after-methods)
               (let ((next-ef (ef-4 primary-methods)))
                 (if (null after-methods)
                   next-ef
                   `(MULTIPLE-VALUE-PROG1
                      ,next-ef
                      ; most-specific-last:
                      ,@(forms-for-invoking-sequentially (reverse after-methods))))))
             (ef-4 (primary-methods)
               `(CALL-METHOD ,(first primary-methods) ,(rest primary-methods))))
      (values
        (ef-1 primary-methods before-methods after-methods around-methods)
        '()))))

(defun standard-method-combination-check-method-qualifiers (gf method-combo method)
  ;; CLtL2 28.1.7.2., 28.1.7.4., ANSI CL 7.6.6.2., 7.6.6.4. Method qualifiers
  (let ((qualifiers (std-method-qualifiers method)))
    (when qualifiers
      (let ((allowed-qualifiers (method-combination-qualifiers method-combo)))
        (if allowed-qualifiers
          (dolist (q qualifiers)
            (unless (member q allowed-qualifiers)
              (error-of-type 'sys::source-program-error
                (TEXT "~S method combination, used by ~S, allows no method qualifiers except ~S: ~S")
                (method-combination-name method-combo) gf allowed-qualifiers method)))
          (error-of-type 'sys::source-program-error
            (TEXT "~S method combination, used by ~S, does not allow method qualifiers: ~S")
            (method-combination-name method-combo) gf method))
        (when (> (length qualifiers) 1)
          (error-of-type 'sys::source-program-error
            (TEXT "~S method combination, used by ~S, does not allow more than one method qualifier on a method: ~S")
            (method-combination-name method-combo) gf method))))))

(defun standard-method-combination-call-next-method-allowed (gf method-combo method)
  (declare (ignore gf method-combo))
  (let ((qualifiers (std-method-qualifiers method)))
    (or (equal qualifiers '()) (equal qualifiers '(:around)))))

(setf (get-method-combination 'standard)
      (make-method-combination
        :name 'standard
        :documentation "the STANDARD METHOD-COMBINATION object"
        :qualifiers '(:before :after :around)
        :check-options #'standard-method-combination-check-options
        :expander #'standard-method-combination-expander
        :check-method-qualifiers #'standard-method-combination-check-method-qualifiers
        :call-next-method-allowed #'standard-method-combination-call-next-method-allowed))

;;; ---------------------- Short-Form Method Combination ----------------------

(defun short-form-method-combination-check-options (gf-name combination options)
  (any-method-combination-check-options gf-name combination options
    (function method-combination-option-checker
      (lambda (&optional (order ':most-specific-first))
        (unless (memq order '(:most-specific-first :most-specific-last))
          (invalid-sort-order-error 'order order))))))

(defun compute-short-form-effective-method-form (gf combination options methods)
  (destructuring-bind (&optional (order ':most-specific-first)) options
    (let ((operator (method-combination-operator combination)))
      (multiple-value-bind (primary around)
           (let ((primary-methods '())
                 (around-methods '())
                 (qualifier (method-combination-name combination)))
             (dolist (method methods)
               (let ((quals (std-method-qualifiers method)))
                 (if (equal quals '(:around))
                   (push method around-methods)
                   (push method primary-methods))))
             (when (null primary-methods)
               (return-from compute-short-form-effective-method-form
                 (let ((rest-variable (gensym)))
                   (values `(APPLY #'NO-PRIMARY-METHOD ',gf ,rest-variable)
                           `((:ARGUMENTS &WHOLE ,rest-variable))))))
             ;; check that all qualifiers are singular and correct
             ;; FIXME: move this check to check-method-qualifiers
             (dolist (method primary-methods)
               (let ((qualifiers (std-method-qualifiers method)))
                 (unless (and (null (rest qualifiers))
                              (eq (first qualifiers) qualifier))
                   (invalid-method-error
                    method (TEXT "qualifiers ~s not permitted for combination ~s.")
                    qualifiers qualifier))))
             (values
               (ecase order
                 (:most-specific-first (nreverse primary-methods))
                 (:most-specific-last primary-methods))
               (nreverse around-methods)))
        (let ((form
                (if (and (null (rest primary))
                         (method-combination-identity-with-one-argument combination))
                  `(CALL-METHOD ,(first primary))
                  `(,operator ,@(mapcar #'(lambda (method) `(CALL-METHOD ,method)) primary)))))
          (when around
            (setq form `(CALL-METHOD ,(first around)
                                     (,@(rest around) (make-method ,form)))))
          (values form '()))))))

(defun short-form-method-combination-expander
    (*method-combination-generic-function* *method-combination* options methods)
  (compute-short-form-effective-method-form
    *method-combination-generic-function* *method-combination* options methods))

(defun short-form-method-combination-check-method-qualifiers (gf method-combo method)
  (standard-method-combination-check-method-qualifiers gf method-combo method)
  (let ((qualifiers (std-method-qualifiers method)))
    (when (null qualifiers)
      (error-of-type 'sys::source-program-error
        (TEXT "~S method combination, used by ~S, does not allow less than one method qualifier on a method: ~S")
        (method-combination-name method-combo) gf method))))

(defun short-form-method-combination-call-next-method-allowed (gf method-combo method)
  (declare (ignore gf method-combo))
  (let ((qualifiers (std-method-qualifiers method)))
    (equal qualifiers '(:around))))

;;; Predefined method combinations.
(dolist (name '(+ and append list max min nconc or progn))
  (setf (get-method-combination name)
        (make-method-combination
          :name name :operator name
          :qualifiers (list name ':around)
          :identity-with-one-argument (not (eq name 'list))
          :documentation (format nil "the ~A ~A object"
                                 name 'method-combination)
          :check-options #'short-form-method-combination-check-options
          :expander #'short-form-method-combination-expander
          :check-method-qualifiers #'short-form-method-combination-check-method-qualifiers
          :call-next-method-allowed #'short-form-method-combination-call-next-method-allowed)))

;;; ---------------------- Long-Form Method Combination ----------------------

(defun long-form-method-combination-expander
    (*method-combination-generic-function* *method-combination* methods options
     long-expander)
  (values
    (apply long-expander *method-combination-generic-function* methods options)
    `((:ARGUMENTS ,@(method-combination-arguments-lambda-list *method-combination*)))))

(defun long-form-method-combination-call-next-method-allowed (gf method-combo method)
  (declare (ignore gf method-combo method))
  t)

;;; ------------------------ DEFINE-METHOD-COMBINATION ------------------------

(defun parse-method-groups (name method-groups)
  (labels ((group-error (group message &rest message-args)
             (error-of-type 'sys::source-program-error
               (TEXT "~S ~S: invalid method group specifier ~S: ~A")
               'define-method-combination name group
               (apply #'format nil message message-args)))
           ;; Performs the syntax check of a method-group-specifier and
           ;; returns a simple-vector
           ;;   #(name patterns/predicate orderform required-p description)
           ;; The second element can be a non-empty list of patterns, or a
           ;; non-null symbol naming a predicate.
           (normalize-group (group)
             (unless (and (consp group) (consp (cdr group)))
               (group-error group (TEXT "Not a list of at least length 2")))
             (let ((variable (car group))
                   (groupr (cdr group))
                   (patterns '())
                   (predicate nil)
                   (orderforms '())
                   (requireds '())
                   (description nil))
               (unless (symbolp variable)
                 (group-error group (TEXT "Not a variable name: ~S") variable))
               ; Parse the {qualifier-pattern+ | predicate} part:
               (do ()
                   ((atom groupr))
                 (let ((qp (car groupr)))
                   (cond ((or (eq qp '*)
                              (and (listp qp)
                                   (memq (cdr (last qp)) '(nil *))))
                          ; A qualifier pattern.
                          (when predicate
                            (group-error group (TEXT "In method group ~S: Cannot specify both qualifier patterns and a predicate.") variable))
                          (push qp patterns))
                         ((memq qp '(:DESCRIPTION :ORDER :REQUIRED))
                          ; End of the {qualifier-pattern+ | predicate} part.
                          (return))
                         ((symbolp qp)
                          ; A predicate.
                          (when predicate
                            (group-error group (TEXT "In method group ~S: Cannot specify more than one predicate.") variable))
                          (when patterns
                            (group-error group (TEXT "In method group ~S: Cannot specify both qualifier patterns and a predicate.") variable))
                          (setq predicate qp))
                         (t
                           (group-error group (TEXT "In method group ~S: Neither a qualifier pattern nor a predicate: ~S") variable qp))))
                 (setq groupr (cdr groupr)))
               (do ()
                   ((atom groupr))
                 (when (atom (cdr groupr))
                   (group-error group (TEXT "In method group ~S: options must come in pairs") variable))
                 (let ((optionkey (first groupr))
                       (argument (second groupr)))
                   (case optionkey
                     (:ORDER
                      (when orderforms
                        (group-error group (TEXT "In method group ~S: option ~S may only be given once") variable ':order))
                      (setq orderforms (list argument)))
                     (:REQUIRED
                      (when requireds
                        (group-error group (TEXT "In method group ~S: option ~S may only be given once") variable ':required))
                      (setq requireds (list (not (null argument)))))
                     (:DESCRIPTION
                      (when description
                        (group-error group (TEXT "In method group ~S: option ~S may only be given once") variable ':description))
                      (unless (stringp argument)
                        (group-error group (TEXT "In method group ~S: ~S is not a string") variable argument))
                      (setq description argument))
                     (t
                      (group-error group (TEXT "In method group ~S: Invalid option ~S") variable optionkey))))
                 (setq groupr (cddr groupr)))
               (unless (or patterns predicate)
                 (group-error group (TEXT "In method group ~S: Missing pattern or predicate.") variable))
               (vector variable
                       (or predicate (nreverse patterns))
                       (if orderforms (first orderforms) '':MOST-SPECIFIC-FIRST)
                       (if requireds (first requireds) 'NIL)
                       (or description
                           (concatenate 'string
                             (sys::format-quote (format nil "~A" variable))
                             "~@{ ~S~}"))))))
    (mapcar #'normalize-group method-groups)))

(defun compute-method-partition-lambdas (method-groups body)
  "Given the normalized method group specifiers, computes
1. a function without arguments, that checks the options,
2. a function to be applied to a list of methods to produce the effective
method function's body. The group variables are bound in the body.
3. a function to be applied to a single method to produce a qualifiers check."
  (let ((order-bindings nil))
    (labels (;; Returns a form that tests whether a list of qualifiers, assumed
             ;; to be present in the variable QUALIFIERS, matches the given pattern.
             (compute-match-predicate-1 (pattern)
               ; Already checked above.
               (assert (or (eq pattern '*)
                           (and (listp pattern)
                                (memq (cdr (last pattern)) '(nil *)))))
               (cond ((null pattern) `(NULL QUALIFIERS))
                     ((eq pattern '*) `T)
                     ((null (cdr (last pattern))) `(EQUAL QUALIFIERS ',pattern))
                     (t (let* ((required-part (ldiff pattern '*))
                               (n (length required-part)))
                          `(AND (SYS::CONSES-P ,n QUALIFIERS)
                                (EQUAL (LDIFF QUALIFIERS (NTHCDR ,n QUALIFIERS))
                                       ',required-part))))))
             ;; Returns a form that tests whether a list of qualifiers, assumed
             ;; to be present in the variable QUALIFIERS, satisfies the test
             ;; for the given normalized method group description.
             (compute-match-predicate (ngroup)
               (let ((patterns (svref ngroup 1)))
                 ; Already checked above.
                 (assert (and (or (listp patterns) (symbolp patterns))
                              (not (null patterns))))
                 (if (listp patterns)
                   `(OR ,@(mapcar #'compute-match-predicate-1 patterns))
                   `(,patterns QUALIFIERS))))
             ;; Returns the variable binding for the given normalized method
             ;; group description.
             (compute-variable-binding (ngroup)
               (let ((variable (svref ngroup 0)))
                 `(,variable NIL)))
             ;; Returns a form that performs the :required check for the given
             ;; normalized method group description.
             (compute-required-form (ngroup)
               (let ((variable (svref ngroup 0))
                     (required-p (svref ngroup 3)))
                 (when required-p
                   `(UNLESS ,variable
                      (APPLY #'MISSING-REQUIRED-METHOD
                        *METHOD-COMBINATION-GENERIC-FUNCTION*
                        *METHOD-COMBINATION*
                        ',variable
                        #'(LAMBDA (METH)
                            (LET ((QUALIFIERS (METHOD-QUALIFIERS METH)))
                              (DECLARE (IGNORABLE QUALIFIERS))
                              ,(compute-match-predicate ngroup)))
                        *METHOD-COMBINATION-ARGUMENTS*)))))
             ;; Returns a form that reorders the list of methods in the method
             ;; group that originates from the given normalized method group
             ;; description.
             (compute-reorder-form (ngroup)
               ;; If an order spec is present, make a binding for the
               ;; shared value and use that to decide whether to reverse.
               ;; If the order is :most-positive-first, we have to reverse,
               ;; to undo the reversal done by the previous PUSH operations.
               (let ((variable (svref ngroup 0))
                     (order-form (svref ngroup 2)))
                 (if (or (equal order-form '':MOST-SPECIFIC-FIRST)
                         (equal order-form ':MOST-SPECIFIC-FIRST))
                   `(SETQ ,variable (NREVERSE ,variable))
                   (let ((order-variable
                           (first (find order-form order-bindings :key #'second))))
                     (unless order-variable
                       (setq order-variable (gensym "ORDER-"))
                       (push `(,order-variable ,order-form) order-bindings))
                     `(COND ((EQ ,order-variable ':MOST-SPECIFIC-FIRST)
                             (SETQ ,variable (NREVERSE ,variable)))
                            ((EQ ,order-variable ':MOST-SPECIFIC-LAST))
                            (T (INVALID-METHOD-SORT-ORDER-ERROR ',order-form ,order-variable))))))))
      (let ((match-clauses '())
            (check-forms '()))
        (dolist (ngroup method-groups)
          (let ((variable (svref ngroup 0))
                (qualifier-test-form (compute-match-predicate ngroup)))
            (push `(,qualifier-test-form (PUSH METHD ,variable))
                  match-clauses)
            (push qualifier-test-form check-forms)))
        (setq match-clauses (nreverse match-clauses))
        (setq check-forms (nreverse check-forms))
        (let ((order-forms
                (delete nil (mapcar #'compute-reorder-form method-groups))))
          (values
            `(LAMBDA ()
               (LET (,@order-bindings)
                 ,@(mapcar #'(lambda (order-binding)
                               (let ((order-variable (first order-binding))
                                     (order-form (second order-binding)))
                                 `(UNLESS (MEMQ ,order-variable '(:MOST-SPECIFIC-FIRST :MOST-SPECIFIC-LAST))
                                    (INVALID-SORT-ORDER-ERROR ',order-form ,order-variable))))
                           order-bindings)))
            `(LAMBDA (METHODS)
               (LET (,@(mapcar #'compute-variable-binding method-groups)
                     ,@order-bindings)
                 (DOLIST (METHD METHODS)
                   (LET ((QUALIFIERS (METHOD-QUALIFIERS METHD)))
                     (DECLARE (IGNORABLE QUALIFIERS))
                     (COND ,@match-clauses
                           (T (INVALID-METHOD-QUALIFIERS-ERROR *METHOD-COMBINATION-GENERIC-FUNCTION* METHD)))))
                 ,@order-forms
                 ,@(delete nil (mapcar #'compute-required-form method-groups))
                 (PROGN ,@body)))
            `(LAMBDA (GF METHD)
               (LET ((QUALIFIERS (METHOD-QUALIFIERS METHD)))
                 (DECLARE (IGNORABLE QUALIFIERS))
                 (OR ,@check-forms
                     (INVALID-METHOD-QUALIFIERS-ERROR GF METHD))))))))))

(defmacro define-method-combination (&whole whole-form
                                     name &rest options)
  "The macro define-method-combination defines a new method combination.
Short-form options are :documentation, :identity-with-one-argument,
 and :operator.
Long-form options are a list of method-group specifiers,
 each of which comprises a sequence of qualifier patterns
 followed by respective :description, :order, :required options,
 and optional :generic-function, and :arguments options preceeding
 the definition body."
  (unless (symbolp name)
    (error-of-type 'sys::source-program-error
      (TEXT "~S: method combination name ~S should be a symbol")
      'define-method-combination name))
  (sys::check-redefinition
    name 'define-method-combination
    (and (get-method-combination name nil)
         "method combination"))
  (cond ;; "The short form syntax ... is recognized when the second subform is
        ;;  a non-nil symbol or is not present."
        ((or (null options)
             (and (consp options)
                  (typep (first options) '(and symbol (not null)))))
         ;; Short form.
         (when (oddp (length options))
           (error-of-type 'sys::source-program-error
             (TEXT "~S ~S: options must come in pairs")
             'define-method-combination name))
         (let ((documentation nil)
               (identities '())
               (operators '()))
           (do ((optionsr options (cddr optionsr)))
               ((atom optionsr))
             (when (atom (cdr optionsr))
               (error-of-type 'sys::source-program-error
                 (TEXT "~S ~S: options must come in pairs")
                 'define-method-combination name))
             (let ((optionkey (first optionsr))
                   (argument (second optionsr)))
               (case optionkey
                 (:DOCUMENTATION
                  (when documentation
                    (error-of-type 'sys::source-program-error
                      (TEXT "~S ~S: option ~S may only be given once")
                      'define-method-combination name ':documentation))
                  (unless (stringp argument)
                    (error-of-type 'sys::source-program-error
                      (TEXT "~S ~S: ~S is not a string")
                      'define-method-combination name argument))
                  (setq documentation argument))
                 (:IDENTITY-WITH-ONE-ARGUMENT
                  (when identities
                    (error-of-type 'sys::source-program-error
                      (TEXT "~S ~S: option ~S may only be given once")
                      'define-method-combination name ':identity-with-one-argument))
                  (setq identities (list (not (null argument)))))
                 (:OPERATOR
                  (when operators
                    (error-of-type 'sys::source-program-error
                      (TEXT "~S ~S: option ~S may only be given once")
                      'define-method-combination name ':operator))
                  (unless (symbolp argument)
                    (error-of-type 'sys::source-program-error
                      (TEXT "~S ~S, option ~S: ~S is not a symbol")
                      'define-method-combination name ':operator argument))
                  (setq operators (list argument)))
                 (t
                   (error-of-type 'sys::source-program-error
                     (TEXT "~S ~S: ~S is not a valid short-form option")
                     'define-method-combination name optionkey)))))
           `(DO-DEFINE-METHOD-COMBINATION
              ',name
              ,@(when documentation
                  `(:DOCUMENTATION ',documentation))
              ,@(when identities
                  `(:IDENTITY-WITH-ONE-ARGUMENT ',(first identities)))
              :OPERATOR ',(if operators (first operators) name)
              :QUALIFIERS ',(list name ':around)
              :CHECK-OPTIONS #'SHORT-FORM-METHOD-COMBINATION-CHECK-OPTIONS
              :EXPANDER #'SHORT-FORM-METHOD-COMBINATION-EXPANDER
              :CHECK-METHOD-QUALIFIERS #'SHORT-FORM-METHOD-COMBINATION-CHECK-METHOD-QUALIFIERS
              :CALL-NEXT-METHOD-ALLOWED #'SHORT-FORM-METHOD-COMBINATION-CALL-NEXT-METHOD-ALLOWED)))
        ;; "The long form syntax ... is recognized when the second subform is a
        ;;  list."
        ((and (consp options) (listp (first options)))
         ;; Long form.
         (unless (and (>= (length options) 2) (listp (second options)))
           (error-of-type 'sys::source-program-error
             (TEXT "~S ~S: invalid syntax for long form: ~S")
             'define-method-combination name whole-form))
         (let ((lambda-list (first options))
               (method-group-specifiers (second options))
               (body (cddr options)))
           ; Check the lambda-list.
           (analyze-lambdalist lambda-list
             #'(lambda (errorstring &rest arguments)
                 (error-of-type 'sys::source-program-error
                   (TEXT "~S ~S: invalid lambda-list: ~A")
                   'define-method-combination name
                   (apply #'format nil errorstring arguments))))
           ; Check the method-group-specifiers, then the rest.
           (let ((method-groups
                   (parse-method-groups name method-group-specifiers))
                 (arguments-lambda-list nil)
                 (arguments-variables '())
                 (user-gf-variable nil)
                 (gf-name-variable (gensym "GF-NAME-"))
                 (gf-variable (gensym "GF-"))
                 (combination-variable (gensym "COMBINATION-"))
                 (options-variable (gensym "OPTIONS-"))
                 (methods-variable (gensym "METHODS-"))
                 (method-variable (gensym "METHOD-")))
             (when (and (consp body) (consp (car body))
                        (eq (caar body) ':ARGUMENTS))
               (setq arguments-lambda-list (cdar body))
               (let ((arguments-lambda-list-without-whole
                       (if (and (consp arguments-lambda-list)
                                (eq (car arguments-lambda-list) '&WHOLE)
                                (consp (cdr arguments-lambda-list)))
                         (cddr arguments-lambda-list)
                         arguments-lambda-list)))
                 (multiple-value-bind (reqvars optvars optinits optsvars rest
                                       keyp keywords keyvars keyinits keysvars
                                       allowp auxvars auxinits)
                     (analyze-lambdalist arguments-lambda-list-without-whole
                       #'(lambda (errorstring &rest arguments)
                           (error-of-type 'sys::source-program-error
                             (TEXT "~S ~S: invalid ~S lambda-list: ~A")
                             'define-method-combination name ':arguments
                             (apply #'format nil errorstring arguments))))
                   (declare (ignore optinits keyp keywords keyinits allowp auxinits))
                   (setq arguments-variables
                         (remove 0 (append (if (eq (first arguments-lambda-list) '&WHOLE)
                                             (list (second arguments-lambda-list))
                                             '())
                                           reqvars optvars optsvars (list rest)
                                           keyvars keysvars auxvars)))))
               (setq body (cdr body)))
             (when (and (consp body) (consp (car body))
                        (eq (caar body) ':GENERIC-FUNCTION))
               (let ((option (cdar body)))
                 (unless (and (consp option) (symbolp (car option))
                              (null (cdr option)))
                   (error-of-type 'sys::source-program-error
                     (TEXT "~S ~S: Invalid syntax for ~S option: ~S")
                     'define-method-combination name ':generic-function (car body)))
                 (setq user-gf-variable (car option))
                 (setq body (cdr body))))
             (multiple-value-bind (body-rest declarations documentation)
                 (sys::parse-body body t)
               (when arguments-variables
                 ;; Add bindings so that the effective method function can
                 ;; access the arguments that were passed to generic function.
                 (setq body-rest
                       `((LET ,(mapcan
                                 #'(lambda (variable)
                                     (list `(,variable ',variable)))
                                 arguments-variables)
                           ,@body-rest))))
               (multiple-value-bind (check-options-lambda partition-lambda check-lambda)
                   (compute-method-partition-lambdas method-groups body-rest)
                 `(DO-DEFINE-METHOD-COMBINATION
                    ',name
                    ,@(when documentation
                        `(:DOCUMENTATION ',documentation))
                    ,@(when declarations
                        `(:DECLARATIONS '((DECLARE ,@declarations))))
                    ,@(when arguments-lambda-list
                        `(:ARGUMENTS-LAMBDA-LIST ',arguments-lambda-list))
                    :CHECK-OPTIONS
                      #'(LAMBDA (,gf-name-variable ,combination-variable
                                 ,options-variable)
                          (ANY-METHOD-COMBINATION-CHECK-OPTIONS
                            ,gf-name-variable ,combination-variable
                            ,options-variable
                            (FUNCTION METHOD-COMBINATION-OPTION-CHECKER
                              (LAMBDA (,@lambda-list)
                                (,check-options-lambda)))))
                    :EXPANDER
                      #'(LAMBDA (,gf-variable ,combination-variable
                                 ,options-variable ,methods-variable)
                          (LONG-FORM-METHOD-COMBINATION-EXPANDER
                            ,gf-variable ,combination-variable
                            ,methods-variable ,options-variable
                            #'(LAMBDA (,gf-variable ,methods-variable ,@lambda-list)
                                (LET (,@(when user-gf-variable `(,user-gf-variable ,gf-variable)))
                                  (,partition-lambda ,methods-variable)))))
                    :CHECK-METHOD-QUALIFIERS
                      #'(LAMBDA (,gf-variable ,combination-variable ,method-variable)
                          (DECLARE (IGNORE ,combination-variable))
                          (,check-lambda ,gf-variable ,method-variable))
                    :CALL-NEXT-METHOD-ALLOWED
                      #'LONG-FORM-METHOD-COMBINATION-CALL-NEXT-METHOD-ALLOWED))))))
        (t (error-of-type 'sys::source-program-error
             (TEXT "~S ~S: invalid syntax, neither short form nor long form syntax: ~S")
             'define-method-combination name whole-form))))

;; DEFINE-METHOD-COMBINATION execution
(defun do-define-method-combination (name &rest initargs)
  "Support function for the DEFINE-METHOD-COMBINATION macro,
which performs the instantiation and registration and returns NAME."
  (let ((method-combination
          (apply #'make-method-combination :name name initargs)))
    (setf (get-method-combination name) method-combination)
    name))

;;; ---------------------------------- Misc ----------------------------------

(defun find-method-combination-<generic-function>-<symbol> (gf name options)
  (let ((combination (get-method-combination name 'defgeneric)))
    (funcall (method-combination-check-options combination)
             (sys::closure-name gf) combination options)
    (when options
      (setq combination (copy-method-combination combination))
      (setf (method-combination-options combination) (copy-list options)))
    combination))

;; Preliminary.
(defun find-method-combination (gf name options)
  (find-method-combination-<generic-function>-<symbol> gf name options))

;; Converts a method-combination designator, e.g. a method combination name
;; or a list consisting of a method combination name and options, to a
;; method-combination instance.
(defun coerce-to-method-combination (gf-name method-combo)
  (flet ((mc (designator)
           (typecase designator
             (symbol (get-method-combination designator 'defgeneric))
             (method-combination designator)
             (t (error-of-type 'program-error
                  (TEXT "~S is not a valid a ~S designator")
                  designator 'method-combination)))))
    (if (consp method-combo)
      (let ((combination (mc (first method-combo)))
            (options (rest method-combo)))
        (funcall (method-combination-check-options combination)
                 gf-name combination options)
        (when options
          (setq combination (copy-method-combination combination))
          (setf (method-combination-options combination) (copy-list options)))
        combination)
      (mc method-combo))))
