;;;; Common Lisp Object System for CLISP: Method Combination
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; James Anderson 2003

(in-package "CLOS")


(defvar *method-combination-arguments* nil
  "The actual generic function call arguments (in compute-effective-method)" )
(defvar *method-combination-generic-function* nil
  "The generic function applied (in compute-effective-method)")
(defvar *method-combination* nil
  "The generic function's method combination (in compute-effective-method)")

;;; error functions
(defun invalid-method-error (method format-string &rest args)
  (error-of-type 'sys::source-program-error
    (TEXT "for function ~s applied to ~s:~%while computing the effective method through ~s:~%invalid method: ~s~%~?")
    *method-combination-generic-function*
    *method-combination-arguments*
    *method-combination*
    method format-string args))

(defun method-combination-error (format-string &rest args)
  (error-of-type 'sys::source-program-error
    (TEXT "for function ~s applied to ~s:~%while computing the effective method through ~s:~%invalid method combination: ~s~%~?")
    *method-combination-generic-function*
    *method-combination-arguments*
    *method-combination*
    *method-combination* format-string args))

;;; utility functions
(defun qualifiers-match-p (qualifiers pattern)
  "Returns t if the argument qualifiers for a method match the
specification pattern for a method group. '*' matches anything,
the element * matches any single element, and others match on
equal. A null element is tested here, even though the generated
partition function includes a clause for it."
  (typecase pattern
    (cons (when (or (eq (first pattern) '*)
                    (equal (first pattern) (first qualifiers)))
            (qualifiers-match-p (rest qualifiers) (rest pattern))))
    (null (null qualifiers))
    (symbol
     (or (eq pattern '*)
         (method-combination-error "invalid method group pattern: ~s."
                                   pattern)))
    (t (method-combination-error "invalid method group pattern: ~s."
                                 pattern))))

(defun compute-method-partition-lambdas (method-groups)
  "Given the method group form from the combination definition,
computes 1. a function to be applied to a list of methods to produce a
partitioned plist. The group variables are used as the keys.
Where order specifications are present consolidate them bind them.??
2. a function to be applied to a single method to produce a qualifiers check.
Performs static tests for conflicting patterns components and
generates dynamic tests for unmatched methods and required groups."
  (let ((order-bindings nil) (*-group-variable nil) (order-forms nil))
    (labels ((group-error (group message)
               (error "invalid group: ~s: ~a." group message))
             (normalize-group (group &aux (g group))
               (let ((variable (pop group)) (patterns nil) (description nil)
                     (required nil) (order nil))
                 (loop (unless group (return))
                   (let ((qp (pop group)))
                     (cond ((or (eq qp '*) (consp qp) (null qp))
                            (unless (listp patterns)
                              (group-error qp "duplicate pattern option"))
                            (push qp patterns))
                           ((eq qp :order)
                            (if order
                              (group-error qp "duplicate order option")
                              (setf order (pop group))))
                           ((eq qp :required)
                            (if required
                              (group-error qp "duplicate required option")
                              (setf required (pop group))))
                           ((eq qp :description)
                            (if description
                              (group-error qp "duplicate description option")
                              (setf description (pop group))))
                           ((symbolp qp)
                            (if patterns
                              (group-error qp "duplicate predicate option")
                              (setf patterns qp)))
                           (t (group-error qp "illegal group pattern")))))
                 (typecase patterns
                   (cons (setf patterns (reverse patterns)))
                   (null (group-error g "at least one pattern is required."))
                   (symbol t))
                 (list variable patterns order required
                       (or description (format nil "~s qualifiers ~s"
                                               variable patterns)))))
             (compute-required-form (group)
               (let ((variable (first group))
                     (patterns (second group))
                     (required-form (fourth group)))
                 (when required-form
                   `(unless (getf partitioned-method-plist ',variable)
                      (method-combination-error
                       "no methods match group: ~s ~s."
                       ',variable ',patterns)))))
             (compute-match-predicate-1 (pattern)
               (typecase pattern
                 (symbol (case pattern
                           (* `(qualifiers-match-p qualifiers '*))
                           ((nil) '(null qualifiers))
                           (t `(,pattern qualifiers))))
                 (cons `(qualifiers-match-p qualifiers ',pattern))
                 (t (error "illegal group pattern: ~s." pattern))))
             (compute-match-predicate (group)
               (let ((variable (first group))
                     (patterns (second group)))
                 (cond ((equal patterns '(*))
                        (if *-group-variable
                            (error "duplicate * group: ~s." group)
                            (setf *-group-variable variable))
                        nil)
                       ((symbolp patterns)
                        `(,patterns qualifiers))
                       (t
                        (if (null patterns)
                          '(null qualifiers)
                          `(or ,@(mapcar #'compute-match-predicate-1 patterns)))))))
             (compute-sort-form (group)
               ;; If an order spec is present, make a binding for the
               ;; shared value and use that to decide whether to reverse.
               ;; If no spec if present, then always reverse.
               (let ((variable (first group))
                     (order (third group))
                     (order-variable nil))
                 (cond (order
                        (unless (setf order-variable
                                      (first (find order order-bindings
                                                   :key #'second
                                                   :test #'equalp)))
                          (setf order-variable  (gensym "ORDER-"))
                          (push (list order-variable  order) order-bindings))
                        `(ecase ,order-variable
                           ((nil :most-specific-first)
                            (setf (getf partitioned-method-plist ',variable)
                                  (reverse (getf partitioned-method-plist
                                                 ',variable))))
                           (:most-specific-last )))
                       (t
                        `(setf (getf partitioned-method-plist ',variable)
                               (reverse (getf partitioned-method-plist
                                              ',variable))))))))
      (setq method-groups (mapcar #'normalize-group method-groups))
      (let ((match-forms '()) (check-forms '()))
        (dolist (group method-groups)
          (let ((variable (first group))
                (predicate (compute-match-predicate group)))
            (when predicate
              (push `(when ,predicate
                       (push methd (getf partitioned-method-plist ',variable)))
                    match-forms)
              (push predicate check-forms))))
        (setq match-forms (nreverse match-forms))
        (setq check-forms (nreverse check-forms))
        (let ((order-forms
                (delete nil (mapcar #'compute-sort-form method-groups))))
          (values
            `(lambda (methods)
               (let ((partitioned-method-plist nil) ,@order-bindings)
                 (dolist (methd methods)
                   (let ((qualifiers (method-qualifiers methd)))
                     (declare (ignorable qualifiers))
                     (or ,@match-forms
                         ,(if *-group-variable
                            `(push methd (getf partitioned-method-plist
                                               ',*-group-variable))
                            '(invalid-method-error
                              "method matched no group: ~s." methd)))))
                 ,@order-forms
                 ,@(delete nil (mapcar #'compute-required-form method-groups))
                 partitioned-method-plist))
            `(lambda (gf methd)
               ,(if *-group-variable
                  'nil
                  `(let ((qualifiers (method-qualifiers methd)))
                     (declare (ignorable qualifiers))
                     (or ,@check-forms (invalid-method-qualifiers-error gf methd)))))))))))

;;; definition implementation
(defun %define-method-combination (name &rest initargs)
  "Support function for the DEFINE-METHOD-COMBINATION macro,
which performs the instantiation and registration and returns NAME."
  (declare (dynamic-extent initargs))
  (let ((definition-object (apply #'make-method-combination
                                  :name name initargs)))
    (setf (find-method-combination name) definition-object)
    name))

(defmacro define-method-combination (name &rest options)
  "The macro define-method-combination defines a new method combination.
Short-form options are :documentation, :identity-with-one-argument,
 and :operator.
Long-form options are a list of method-group specifiers,
 each of which comprises a sequence of qualifier patterns
 followed by respective :description, :order, :required options,
 and optional :generic-function, and :arguments options preceeding
 the definition body."
  (sys::check-redefinition
   name 'define-method-combination
   (and (find-method-combination name :if-does-not-exist nil)
        "method combination"))
  (cond ((or (null options)             ; short form
             (typep (first options) '(and symbol (not null))))
         (destructuring-bind            ; reconstruct to ensure constants
               (&key documentation identity-with-one-argument (operator name))
             options
           `(%define-method-combination
             ',name
             ,@(when documentation
                 `(:documentation ',documentation))
             ,@(when identity-with-one-argument
                 `(:identity-with-one-argument ',identity-with-one-argument))
             :operator ',operator
             :qualifiers ',(list name ':around)
             :expander #'short-form-method-combination-expander
             :check-method-qualifiers #'short-form-method-combination-check-method-qualifiers
             :call-next-method-allowed #'short-form-method-combination-call-next-method-allowed)))
        ((listp (first options))        ; long form
         (destructuring-bind (lambda-list qualifier-groups . body) options
           (let ((arguments-lambda-list nil)
                 (gf-variable nil)
                 (declarations nil)
                 (combination-variable (gensym "COMBINATION-"))
                 (options-variable (gensym "OPTIONS-"))
                 (args-variable (gensym "ARGUMENTS-"))
                 (methods-variable (gensym "METHODS-"))
                 (method-variable (gensym "METHOD-"))
                 (ignore-gf nil)
                 (documentation nil))
             (loop
               (typecase (first body)
                 (string (when documentation (return))
                         (setf documentation (pop body)))
                 (cons (destructuring-bind (keyword . rest) (first body)
                         (case keyword
                           (:arguments
                             (when arguments-lambda-list
                               (error "duplicate :arguments option."))
                             (setf arguments-lambda-list rest))
                           (:generic-function
                             (when gf-variable
                               (error "duplicate :generic-function option."))
                             (setf gf-variable (first rest)))
                           (declare
                             (push (first body) declarations))
                           (t (return))))
                       (pop body))
                 (t (return))))
             (unless gf-variable (setf gf-variable (gensym "GF-") ignore-gf t))
             (when arguments-lambda-list
               ;; add reflecive bindings for the planned effective function
               ;; parameters
               (setf body
                     `((let ,(mapcan
                              (lambda (parameter)
                                (unless (memq parameter lambda-list-keywords)
                                  (when (consp parameter)
                                    (setf parameter
                                          (if (consp (first parameter))
                                              (second (first parameter))
                                              (first parameter))))
                                  `((,parameter ',parameter))))
                              arguments-lambda-list)
                         ,@body))))
             (multiple-value-bind (partition-lambda check-lambda)
                 (compute-method-partition-lambdas qualifier-groups)
               `(%define-method-combination
                 ',name
                 ,@(when documentation `(:documentation ,documentation))
                 ,@(when declarations `(:declarations ',(reverse declarations)))
                 ,@(when arguments-lambda-list
                     `(:arguments-lambda-list ',arguments-lambda-list))
                 :qualifiers ',qualifier-groups
                 :identity-with-one-argument t
                 :expander
                   (lambda (,gf-variable ,combination-variable
                            ,options-variable ,args-variable)
                     (long-form-method-combination-expander
                       ,gf-variable ,combination-variable
                       ,options-variable ,args-variable
                       (lambda (,gf-variable ,methods-variable ,@lambda-list)
                         ,@(when ignore-gf `((declare (ignore ,gf-variable))))
                         ;; The partition lambda generates a plist keyed by
                         ;; variable name which is then &key destructured and
                         ;; bound for the body.
                         (destructuring-bind
                           (&key ,@(mapcar (lambda (arg &aux (var (first arg)))
                                             `((,var ,var)))
                                           qualifier-groups))
                           (,partition-lambda ,methods-variable)
                           ,@body))))
                 :check-method-qualifiers
                   (lambda (,gf-variable ,combination-variable ,method-variable)
                     (declare (ignore ,combination-variable))
                     (,check-lambda ,gf-variable ,method-variable))
                 :call-next-method-allowed
                   #'long-form-method-combination-call-next-method-allowed)))))
        (t (error "invalid method combination options: ~s." options))))

;;; Method computation implementation:
;;; - compute-effective-method-function handles the function interface
;;;   and the next-method support for both short and long forms.
;;; - short forms: compute-short-form-effective-method-form
;;;   and short-form-method-combination-expander
;;; - long forms: long-form-method-combination-expander

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
          (analyze-lambdalist combination-arguments)
        (declare (ignore opt opt-i opt-p rest))
        (when (> (setq num-req (length positional)) (length req-vars))
          (method-combination-error "invalid combination arguments: ~s."
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
               (method-combination-error "no applicable primary methods."))
             ;; check that all qualifiers are singular and correct
             (dolist (method primary-methods)
               (let ((qualifiers (std-method-qualifiers method)))
                 (unless (and (null (rest qualifiers))
                              (eq (first qualifiers) qualifier))
                   (invalid-method-error
                    method "qualifiers ~s not permitted for combination ~s."
                    qualifiers qualifier))))
             (values
              (ecase order
                ((nil :most-specific-first)
                 (nreverse primary-methods))
                (:most-specific-last
                 primary-methods))
              (nreverse around-methods)))))
    (destructuring-bind
          (&optional (order (method-combination-order combination)))
        options
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

;;; Predefined method combinations.
(dolist (name '(+ and append list max min nconc or progn))
  (setf (find-method-combination name)
        (make-method-combination
          :name name :operator name
          :qualifiers (list name ':around)
          :identity-with-one-argument (not (eq name 'list))
          :documentation (format nil "the ~A ~A object"
                                 name 'method-combination)
          :expander #'short-form-method-combination-expander
          :check-method-qualifiers #'short-form-method-combination-check-method-qualifiers
          :call-next-method-allowed #'short-form-method-combination-call-next-method-allowed)))

