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

(defun compute-effective-method-function (generic-function combination methods
                                          effective-method-form)
  "Given the generic function, its combination, and the effective method form,
constructs and compiles the lambda form for the correct arguments
and with the next-method support.
This reproduces that aspect of the standard-method-combination-expander
which pertains to the parameter list and extends it for the next-method
support.  In constrast to the original, this is used to post-process the
effective method form, in which case the applicable-method computation
has already transpired."
  (declare (ignore methods))
  (let* ((signature (gf-signature generic-function))
         (req-anz (sig-req-num signature))
         (req-vars (gensym-list req-anz))
         (restp (gf-sig-restp signature))
         (rest-var (if restp (gensym)))
         (apply-fun (if restp 'APPLY 'FUNCALL))
         (apply-args nil)
         (lambdalist nil)
         (declarations (method-combination-declarations combination))
         (combination-arguments
          (method-combination-arguments-lambda-list combination))
         (whole-var (when (eq (first combination-arguments) '&whole)
                      (second combination-arguments)))
         (destructuring-lambda-list nil))
    (multiple-value-bind (opt-vars key-vars lambdalist-keypart)
        (gf-keyword-arguments restp signature methods)
      (declare (ignore opt-vars key-vars lambdalist-keypart)) ; FIXME
      ;; Reconcile the required parameters between the effective method
      ;; and an internal destructuring lambda list if the combination
      ;; specified an argument list. The interface rest parameter comprises
      ;; optional, rest, and key arguments, and is destructured for
      ;; internal bindings, and/or coalesced with the required arguments
      ;; to fabricate a whole binding.
      (when whole-var
        ;; Pick off the initial whole parameter.
        (setf combination-arguments (nthcdr 2 combination-arguments)))
      (multiple-value-bind (positional opt opt-i opt-p rest num-req)
          ;; FIXME: combination-arguments is a 3.4.10 lambda list, not an
          ;; 3.4.1 ordinary lambda list.
          (analyze-lambdalist combination-arguments
            #'(lambda (errorstring &rest arguments)
                (error (TEXT "In ~S ~S lambda list: ~A")
                       combination ':arguments
                       (apply #'format nil errorstring arguments))))
        (declare (ignore opt opt-i opt-p rest))
        (when (> (setq num-req (length positional)) (length req-vars))
          (method-combination-error (TEXT "invalid combination arguments: ~s.")
                                    combination-arguments))
        (setf req-vars (append positional (nthcdr num-req req-vars)))
        ;; Construct analogous interface parameter and application
        ;; argument lists.
        (setf lambdalist `(,@req-vars ,@(when restp `(&rest ,rest-var)))
              apply-args `(,@req-vars ,@(if restp `(,rest-var) '())))
        ;; If a method combination argument list is present, the required
        ;; arguments have already been accommodated, but it remains to
        ;; deconstruct the everything else from the rest arguments.
        (when combination-arguments
          (setf destructuring-lambda-list
                `(,@(nthcdr num-req combination-arguments)
                  ,@(when whole-var
                      `(,@(unless (find '&aux combination-arguments) '(&aux))
                          (,whole-var (list* ,@req-vars ,rest-var))))))))
      ;; Combine the generated lambda list with the effective method form
      ;; to create the method function.
      (let ((ef-fun
              (if (and (eq (first effective-method-form) 'call-method)
                       (= (length effective-method-form) 2)
                       (method-combination-identity-with-one-argument combination))
                (let ((method (second effective-method-form)))
                  `(lambda ,lambdalist
                     (DECLARE (INLINE FUNCALL APPLY))
                     ,(if (or (consp method)
                              (std-method-wants-next-method-p method))
                        `(,apply-fun ,(std-method-function method) nil
                                     ,@apply-args)
                        `(,apply-fun ,(std-method-function method)
                                     ,@apply-args))))
                `(lambda ,lambdalist
                   (DECLARE (INLINE FUNCALL APPLY))
                   (macrolet ((call-method (method &optional next-methods
                                                   &aux (m-function (list 'std-method-function method)))
                              ;; If the method expects a next-method
                              ;; operator, construct a function from
                              ;; successor methods or pass NIL as function
                              ;; (NEXT-METHOD-P reacts on it) and precede
                              ;; the arguments proper with this function.
                              ;; If no next methods are expected, then pass
                              ;; the arguments only.  NB, even though this
                              ;; file changes method generation to _always_
                              ;; expect a next-method context, preexisting
                              ;; methods are not that way.??
                              (if (or (consp method)
                                      (std-method-wants-next-method-p method))
                                (if next-methods
                                  (list* ',apply-fun m-function
                                         (list 'function
                                               (list 'lambda ',lambdalist
                                                     (list 'call-method
                                                           (first next-methods)
                                                           (rest next-methods))))
                                         ',apply-args)
                                  (list* ',apply-fun m-function nil ',apply-args))
                                (list* ',apply-fun m-function ',apply-args)))
                            (make-method (body)
                              ;; make a temporary method
                              (let* ((next-method-parm (gensym "NM-"))
                                     (method-lambda
                                      (list 'function
                                            (list 'lambda
                                                  (cons next-method-parm
                                                        ',lambdalist)
                                                  (list 'declare
                                                        (list 'ignore
                                                              next-method-parm))
                                                  body))))
                                (list 'make-standard-method
                                      :function method-lambda
                                      :wants-next-method-p t
                                      :parameter-specializers nil
                                      :qualifiers nil
                                      :signature ,signature
                                      :gf ,*method-combination-generic-function*
                                      ;; It's never going to be added to a
                                      ;; generic function.
                                      :initfunction nil))))
                     ,@declarations
                     ;; If the combination specified an internal argument list,
                     ;; extract the variable parameters from the rest binding.
                     ,(if destructuring-lambda-list
                        `(destructuring-bind ,destructuring-lambda-list ,rest-var
                           ,effective-method-form)
                        effective-method-form))))))
      ;; (pprint ef-fun)
      ;; (eval ef-fun)           ; interpreted
      (compile nil ; (gensym (string (method-combination-name combination)))
               ef-fun)))))

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

(defun standard-method-combination-expander (gf combination options args)
  (declare (ignore combination))
  (declare (ignore options)) ; already checked in check-options
  (let* ((signature (gf-signature gf))
         (req-anz (sig-req-num signature))
         (req-vars (gensym-list req-anz))
         (req-args (subseq args 0 req-anz))
         (restp (gf-sig-restp signature))
         (rest-var (if restp (gensym)))
         (apply-fun (if restp 'APPLY 'FUNCALL))
         (apply-args `(,@req-vars ,@(if restp `(,rest-var) '())))
         (lambdalist `(,@req-vars ,@(if restp `(&REST ,rest-var) '())))
         (arg-order (gf-argorder gf))
         (methods (gf-methods gf)))
    ;; Determine the effective method:
    ;; 1. Select the applicable methods:
    (setq methods
          (remove-if-not #'(lambda (method)
                             (method-applicable-p method req-args))
                         (the list methods)))
    (when (null methods)
      (return-from standard-method-combination-expander
        (no-method-caller 'no-applicable-method gf)))
    (multiple-value-bind (opt-vars key-vars lambdalist-keypart)
        (gf-keyword-arguments restp signature methods)
      ;; 2. Sort the applicable methods by precedence order:
      (setq methods (sort-applicable-methods methods req-args arg-order))
      ;; 3. Apply method combination:
      ;; Split up into individual method types.
      (multiple-value-bind
            (primary-methods before-methods after-methods around-methods)
          (partition-method-list methods)
        (when (null primary-methods)
          (return-from standard-method-combination-expander
            (no-method-caller 'no-primary-method gf)))
        ;; Combine methods into an "effective method":
        (labels ((ef-1 (primary-methods before-methods after-methods
                        around-methods)
                   (if (null around-methods)
                     (ef-2 primary-methods before-methods after-methods)
                     (let* ((1method (first around-methods))
                            (1function (std-method-function 1method)))
                       (if (std-method-wants-next-method-p 1method)
                         (let ((next-ef
                                (ef-1 primary-methods before-methods
                                      after-methods (rest around-methods))))
                           `(,apply-fun ',1function
                                        #'(LAMBDA ,lambdalist ,next-ef)
                                        ,@apply-args))
                         `(,apply-fun ',1function ,@apply-args)))))
                 (forms-for-invoking-sequentially (methods)
                   (mapcar #'(lambda (method)
                               (if (std-method-wants-next-method-p method)
                                 `(,apply-fun ',(std-method-function method)
                                              nil ,@apply-args)
                                 `(,apply-fun ',(std-method-function method)
                                              ,@apply-args)))
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
                   (let* ((1method (first primary-methods))
                          (1function (std-method-function 1method)))
                     (if (std-method-wants-next-method-p 1method)
                         (let ((next-ef-fun (ef-5 (rest primary-methods))))
                           `(,apply-fun ',1function ,next-ef-fun ,@apply-args))
                         `(,apply-fun ',1function ,@apply-args))))
                 (ef-5 (primary-methods)
                   (if (null primary-methods)
                       'NIL ; no function, NEXT-METHOD-P reacts on it
                       `#'(LAMBDA ,lambdalist ,(ef-4 primary-methods)))))
          (let* ((ef-form (ef-1 primary-methods before-methods after-methods
                                around-methods))
                 (ef-fun (if (and (eq (car ef-form) apply-fun)
                                  (equal (cddr ef-form) apply-args)
                                  (null lambdalist-keypart))
                           (cadr ef-form)
                           `#'(LAMBDA
                                  ,@(if (null opt-vars)
                                      `(,(append lambdalist lambdalist-keypart)
                                        ,@(if key-vars
                                            `((DECLARE (IGNORE ,@key-vars)))))
                                      `(,lambdalist
                                        (APPLY #'(LAMBDA (&OPTIONAL ,@opt-vars
                                                          ,@lambdalist-keypart)
                                                   (DECLARE (IGNORE ,@opt-vars
                                                                    ,@key-vars)))
                                               ,rest-var)))
                                ,ef-form))))
            ;; (eval ef-fun)                                 ; interpreted
            ;; (eval `(LOCALLY (DECLARE (COMPILE)) ,ef-fun)) ; compiled
            (eval `(LET () (DECLARE (COMPILE) (INLINE FUNCALL APPLY))
                        ,ef-fun))))))))

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

(setf (find-method-combination 'standard)
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

(defun compute-short-form-effective-method-form (combination options methods)
  (flet ((partition-short-form-method-list (combination methods order)
           (let ((primary-methods '())
                 (around-methods '())
                 (qualifier (method-combination-name combination)))
             (dolist (method methods)
               (let ((quals (std-method-qualifiers method)))
                 (if (equal quals '(:around))
                     (push method around-methods)
                     (push method primary-methods))))
             (unless primary-methods
               (method-combination-error (TEXT "no applicable primary methods.")))
             ;; check that all qualifiers are singular and correct
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
              (nreverse around-methods)))))
    (destructuring-bind (&optional (order ':most-specific-first)) options
      (let ((operator (method-combination-operator combination)))
        (multiple-value-bind (primary around)
            (partition-short-form-method-list combination methods order)
          (flet ((call-methods (methods)
                   (mapcar #'(lambda (method) `(call-method ,method))
                           methods)))
            (let ((form
                   (if (or (rest primary)
                           (not (method-combination-identity-with-one-argument
                                 combination)))
                     `(,operator ,@(call-methods primary))
                     `(call-method ,(first primary)))))
              (when around
                (setq form
                      `(call-method ,(first around)
                                    (,@(rest around) (make-method ,form)))))
              form)))))))

(defun short-form-method-combination-expander
    (*method-combination-generic-function* *method-combination*
     options *method-combination-arguments*)
  (let* ((methods
          (or (compute-applicable-methods
               *method-combination-generic-function*
               *method-combination-arguments*)
              (no-method-caller 'no-applicable-method
                                *method-combination-generic-function*)))
         (em-form (compute-short-form-effective-method-form
                   *method-combination* options methods)))
    (typecase em-form
      (function em-form)
      (list (compute-effective-method-function
             *method-combination-generic-function*
             *method-combination* methods em-form)))))

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
  (setf (find-method-combination name)
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
    (*method-combination-generic-function* *method-combination*
     options *method-combination-arguments* long-expander)
  (let* ((methods
          (or (compute-applicable-methods
               *method-combination-generic-function*
               *method-combination-arguments*)
              (no-method-caller 'no-applicable-method
                                *method-combination-generic-function*)))
         (em-form (apply long-expander *method-combination-generic-function*
                         methods options)))
    (typecase em-form
      (function em-form)
      (list (compute-effective-method-function
             *method-combination-generic-function*
             *method-combination* methods em-form)))))

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
    (and (find-method-combination name :if-does-not-exist nil)
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
                 (user-gf-variable nil)
                 (gf-name-variable (gensym "GF-NAME-"))
                 (gf-variable (gensym "GF-"))
                 (combination-variable (gensym "COMBINATION-"))
                 (options-variable (gensym "OPTIONS-"))
                 (args-variable (gensym "ARGUMENTS-"))
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
                 (analyze-lambdalist arguments-lambda-list-without-whole
                   #'(lambda (errorstring &rest arguments)
                       (error-of-type 'sys::source-program-error
                         (TEXT "~S ~S: invalid ~S lambda-list: ~A")
                         'define-method-combination name ':arguments
                         (apply #'format nil errorstring arguments)))))
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
               (when arguments-lambda-list
                 ;; Add bindings so that the effective method function can
                 ;; access the arguments that were passed to generic function.
                 (setq body-rest
                       `((LET ,(mapcan
                                 #'(lambda (parameter)
                                     (unless (memq parameter lambda-list-keywords)
                                       (when (consp parameter)
                                         (setq parameter
                                               (if (consp (first parameter))
                                                 (second (first parameter))
                                                 (first parameter))))
                                       (list `(,parameter ',parameter))))
                                 arguments-lambda-list)
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
                    :IDENTITY-WITH-ONE-ARGUMENT T ; really??
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
                                 ,options-variable ,args-variable)
                          (LONG-FORM-METHOD-COMBINATION-EXPANDER
                            ,gf-variable ,combination-variable
                            ,options-variable ,args-variable
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
    (setf (find-method-combination name) method-combination)
    name))

;;; ---------------------------------- Misc ----------------------------------

;; Converts a method-combination designator, e.g. a method combination name
;; or a list consisting of a method combination name and options, to a
;; method-combination instance.
(defun coerce-to-method-combination (gf-name method-combo)
  (flet ((mc (designator)
           (typecase designator
             (symbol (find-method-combination designator))
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
