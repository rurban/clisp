;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")

;; ===================== Generic Function Initialization =====================

; n --> list (0 ... n-1)
(defun countup (n)
  (do* ((count n (1- count))
        (l '() (cons count l)))
       ((eql count 0) l)))

;; Checks a generic-function lambda-list and argument-precedence-order.
;; Returns three values:
;; 1. the lambda-list's signature,
;; 2. the argument-precedence-order, as a list of variables,
;; 3. the argument-precedence-order, as a list of numbers from 0 to reqnum-1.
;; Reports errors through errfunc (a function taking a detail object, an
;; error format string and format string arguments).
(defun check-gf-lambdalist+argorder (lambdalist argument-precedence-order argument-precedence-order-p errfunc)
  ;; Check the lambda-list.
  (multiple-value-bind (reqvars optvars rest keyp keywords keyvars allowp)
      (sys::analyze-generic-function-lambdalist lambdalist
        #'(lambda (detail errorstring &rest arguments)
            (funcall errfunc detail
                     (TEXT "Invalid generic function lambda-list: ~A")
                     (apply #'format nil errorstring arguments))))
    (declare (ignore keyvars))
    (let ((reqnum (length reqvars))
          (optnum (length optvars))
          (restp (or (not (eql rest 0)) keyp))) ; &key implies &rest
      (let ((signature
              (make-signature :req-num reqnum :opt-num optnum
                              :rest-p restp :keys-p keyp
                              :keywords keywords :allow-p allowp)))
        (if argument-precedence-order-p
          ;; Check the argument-precedence-order.
          (progn
            (unless (proper-list-p argument-precedence-order)
              (funcall errfunc argument-precedence-order
                       (TEXT "The ~S argument should be a proper list, not ~S")
                       ':argument-precedence-order argument-precedence-order))
            (let ((indices
                    (mapcar #'(lambda (x)
                                (or (position x reqvars)
                                    (funcall errfunc x
                                             (TEXT "Incorrect ~S argument: ~S is not one of the required parameters: ~S")
                                             ':argument-precedence-order x argument-precedence-order)))
                            argument-precedence-order)))
              ;; Is argument-precedence-order a permutation of reqvars?
              ;; In other words: Is the mapping
              ;;         argument-precedence-order --> reqvars
              ;; resp.   indices                   --> {0, ..., reqnum-1}
              ;; bijective?
              (unless (or (<= (length indices) 1) (apply #'/= indices)) ; injective?
                (funcall errfunc argument-precedence-order
                         (TEXT "Incorrect ~S argument: Some variable occurs twice in ~S")
                         ':argument-precedence-order argument-precedence-order))
              (unless (eql (length indices) reqnum) ; surjective?
                (let ((missing (set-difference
                                reqvars argument-precedence-order)))
                  (funcall errfunc missing
                           (TEXT "Incorrect ~S argument: The variables ~S are missing in ~S")
                           ':argument-precedence-order
                           missing argument-precedence-order)))
              (values signature argument-precedence-order indices)))
          (values signature reqvars (countup reqnum)))))))

;; Initialization of a <standard-generic-function> instance.
(defun shared-initialize-<standard-generic-function>
    (gf situation &rest args
     &key (name nil name-p)
          (lambda-list nil lambda-list-p)
          (argument-precedence-order nil argument-precedence-order-p)
          (method-class nil method-class-p)
          (method-combination nil method-combination-p)
          (documentation nil documentation-p)
          (declarations nil declarations-p)
     &allow-other-keys
     &aux signature argorder)
  (declare (ignore name name-p))
  (apply #'shared-initialize-<generic-function> gf situation args)
  (when (eq situation 't)
    (setf (std-gf-initialized gf) nil))
  (when (and argument-precedence-order-p (not lambda-list-p))
    (error (TEXT "(~S ~S) for generic function ~S: ~S argument specified without a ~S argument.")
           (if (eq situation 't) 'initialize-instance 'shared-initialize)
           'standard-generic-function (funcallable-name gf)
           ':argument-precedence-order ':lambda-list))
  (when lambda-list-p
    ; Check the lambda-list and argument-precedence-order.
    (multiple-value-setq (signature argument-precedence-order argorder)
        (check-gf-lambdalist+argorder lambda-list
          argument-precedence-order argument-precedence-order-p
          #'(lambda (detail errorstring &rest arguments)
              (declare (ignore detail))
              (error (TEXT "(~S ~S) for generic function ~S: ~A")
                     (if (eq situation 't) 'initialize-instance 'shared-initialize)
                     'standard-generic-function (funcallable-name gf)
                     (apply #'format nil errorstring arguments))))))
  (when (or (eq situation 't) method-class-p)
    ; Check the method-class.
    (unless method-class-p
      (setq method-class <standard-method>))
    (unless (class-p method-class)
      (error (TEXT "(~S ~S) for generic function ~S: The ~S argument should be a class, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf)
             ':method-class method-class))
    (unless (subclassp method-class <method>)
      (error (TEXT "(~S ~S) for generic function ~S: The ~S argument should be a subclass of ~S, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf)
             ':method-class 'method method-class)))
  (when (or (eq situation 't) method-combination-p)
    ; Check the method-combination.
    #| ; Not sure whether giving an error here is appropriate.
    (unless method-combination-p
      (error (TEXT "(~S ~S) for generic function ~S: Missing ~S argument.")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf)
             ':method-combination))
    |#
    (unless method-combination-p
      (setq method-combination
            (get-method-combination 'STANDARD '(initialize-instance standard-generic-function))))
    (unless (typep-class method-combination <method-combination>)
      (error (TEXT "(~S ~S) for generic function ~S: The ~S argument should be a ~S object, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf)
             ':method-combination 'method-combination method-combination)))
  (when (or (eq situation 't) documentation-p)
    ; Check the documentation.
    (unless (or (null documentation) (stringp documentation))
      (error (TEXT "(~S ~S) for generic function ~S: The ~S argument should be a string or NIL, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf) ':documentation documentation)))
  (when (or (eq situation 't) declarations-p)
    ; Check the declarations.
    (unless (proper-list-p declarations)
      (error (TEXT "(~S ~S) for generic function ~S: The ~S argument should be a proper list, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'standard-generic-function (funcallable-name gf)
             ':declarations declarations)))
  ; Fill the slots.
  (when lambda-list-p
    (setf (std-gf-lambda-list gf) lambda-list)
    (setf (std-gf-signature gf) signature)
    (setf (std-gf-argorder gf) argorder))
  (when (or (eq situation 't) method-class-p)
    (setf (std-gf-default-method-class gf) method-class))
  (when (or (eq situation 't) method-combination-p)
    (setf (std-gf-method-combination gf) method-combination))
  (when (or (eq situation 't) documentation-p)
    (setf (std-gf-documentation gf) documentation))
  (when (or (eq situation 't) declarations-p)
    (setf (std-gf-declspecs gf) declarations))
  (when (eq situation 't)
    (setf (std-gf-methods gf) '()))
  (when (or (eq situation 't) lambda-list-p method-combination-p)
    (setf (std-gf-effective-method-cache gf) '()))
  ; Now allow the user to call the generic-function-xxx accessor functions.
  (setf (std-gf-initialized gf) t)
  gf)

(defun initialize-instance-<standard-generic-function> (gf &rest args
                                                        &key name
                                                             lambda-list
                                                             argument-precedence-order
                                                             method-class
                                                             method-combination
                                                             documentation
                                                             declarations
                                                        &allow-other-keys)
  (declare (ignore name lambda-list argument-precedence-order method-class
                   method-combination documentation declarations))
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'initialize-instance later.
  (apply #'shared-initialize-<standard-generic-function> gf 't args)
  gf)

(defun make-instance-<standard-generic-function> (class &rest args
                                                  &key name
                                                       lambda-list
                                                       argument-precedence-order
                                                       method-class
                                                       method-combination
                                                       documentation
                                                       declarations
                                                  &allow-other-keys)
  ;; class = <standard-generic-function>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore class name lambda-list argument-precedence-order method-class
                   method-combination documentation declarations))
  (let ((gf (%allocate-instance <standard-generic-function>)))
    (apply #'initialize-instance-<standard-generic-function> gf args)))

;; ======================= Installing the Dispatch Code =======================

;; The dispatch-code for generic functions is formed with
;; `(%GENERIC-FUNCTION-LAMBDA ,@lambdabody)
;; - similar to `(FUNCTION (LAMBDA ,@lambdabody)) -.
;; The following must not occur therein:
;; - access to dynamic variables, binding of dynamic variables,
;; - nontrivial BLOCK, RETURN-FROM, TAGBODY, GO constructions,
;; - invocation of global functions, that are not inline,
;; - formation of non-autonomous functions (closures).
;; So the following is necessary:
;;   (declare (inline case eql eq typep
;;                    arrayp bit-vector-p characterp complexp consp floatp
;;                    functionp hash-table-p integerp listp null numberp
;;                    packagep pathnamep sys::logical-pathname-p random-state-p
;;                    rationalp readtablep realp sys::sequencep
;;                    clos::std-instance-p streamp sys::file-stream-p
;;                    sys::synonym-stream-p sys::broadcast-stream-p
;;                    sys::concatenated-stream-p sys::two-way-stream-p
;;                    sys::echo-stream-p sys::string-stream-p stringp
;;                    clos::structure-object-p symbolp vectorp
;;                    class-of cons gethash funcall apply ...
;;   )        )

;; Returns a generic function without dispatch-code. Not callable!!
(defun %make-gf (name lambda-list argument-precedence-order method-class)
  (make-instance-<standard-generic-function> <standard-generic-function>
    :name name
    :lambda-list lambda-list
    :argument-precedence-order argument-precedence-order
    :method-class method-class))

#||
 (defun make-gf (name lambdabody signature argorder methods)
  (let ((preliminary
         (eval `(LET ()
                  (DECLARE (COMPILE))
                  (%GENERIC-FUNCTION-LAMBDA ,@lambdabody)))))
    (sys::%make-closure
     name (sys::closure-codevec preliminary)
     (list (sys::%record-ref preliminary 2)
           signature argorder methods)
     nil)))
||#


#|| ;; Generic functions with primitive dispatch:

 (defun make-slow-gf (name lambda-list argument-precedence-order method-class methods)
  (let* ((final (%make-gf name lambda-list argument-precedence-order method-class))
         (preliminary
           (eval `(LET ((GF ',final))
                    (DECLARE (COMPILE))
                    (%GENERIC-FUNCTION-LAMBDA (&REST ARGS)
                      (DECLARE (INLINE APPLY))
                      (APPLY 'SLOW-FUNCALL-GF GF ARGS))))))
    (setf (sys::%record-ref final 1) (sys::closure-codevec preliminary))
    (setf (sys::%record-ref final 2) (sys::%record-ref preliminary 2))
    (setf (std-gf-methods final) methods)
    final))

 (let* ((prototype
         (let ((gf 'magic))
           (declare (compile))
           (%generic-function-lambda (&rest args)
             (declare (inline apply))
             (apply 'slow-funcall-gf gf args))))
       (prototype-code (sys::%record-ref prototype 1))
       (prototype-consts (sys::%record-ref prototype 2)))
  (defun finalize-slow-gf (gf)
    (setf (sys::%record-ref gf 1) prototype-code)
    (setf (sys::%record-ref gf 2)
          (let ((v (copy-seq prototype-consts)))
            (setf (svref v 0) (substitute gf 'magic (svref v 0)))
            v)))
  (defun gf-never-called-p (gf) (eq (sys::%record-ref gf 1) prototype-code))
  (defun warn-if-gf-already-called (gf) ))

 ;; call of a generic function
 (defun slow-funcall-gf (gf &rest args)
  (let ((reqanz (sig-req-num (std-gf-signature gf)))
        (arg-order (std-gf-argorder gf))
        (methods (std-gf-methods gf)))
    (unless (>= (length args) reqanz)
      (error-of-type 'program-error
        (TEXT "Too few arguments to ~S: ~S")
        gf args))
    (let ((req-args (subseq args 0 reqanz)))
      ;; Determine the effective method:
      ;; 1. Select the applicable methods:
      (setq methods
        (remove-if-not
         #'(lambda (method) (method-applicable-p method req-args))
         methods))
      (when (null methods)
        (return-from slow-funcall-gf
          (no-method-caller 'no-applicable-method gf)))
      ;; 2. Sort the applicable methods by precedence order:
      (setq methods (sort-applicable-methods methods req-args arg-order))
      ;; 3. Apply method combination:
      ;; Only STANDARD method-combination is implemented.
      ;; partition into the distinct method-types:
      (multiple-value-bind
            (primary-methods before-methods after-methods around-methods)
          (partition-method-list methods)
        (when (null primary-methods)
          (return-from slow-funcall-gf
            (no-method-caller 'no-primary-method gf)))
        ;; combine methods into an "effective method" :
        (labels ((ef-1 (primary-methods before-methods after-methods around-methods)
                   (if (null around-methods)
                     (ef-2 primary-methods before-methods after-methods)
                     (let* ((1method (first around-methods))
                            (1function (std-method-fast-function 1method)))
                       (if (std-method-wants-next-method-p 1method)
                         (let ((next-ef
                                 (ef-1 primary-methods before-methods after-methods (rest around-methods))))
                           #'(lambda (&rest args) (apply 1function next-ef args)))
                         #'(lambda (&rest args) (apply 1function args))))))
                 (ef-2 (primary-methods before-methods after-methods)
                   (if (null after-methods)
                     (ef-3 primary-methods before-methods)
                     (let* ((1method (first after-methods))
                            (1function (std-method-fast-function 1method)))
                       (let ((next-ef (ef-2 primary-methods before-methods (rest after-methods))))
                         #'(lambda (&rest args) (multiple-value-prog1 (apply next-ef args) (apply 1function args)))))))
                 (ef-3 (primary-methods before-methods)
                   (if (null before-methods)
                     (ef-4 primary-methods)
                     (let* ((1method (first before-methods))
                            (1function (std-method-fast-function 1method)))
                       (let ((next-ef (ef-3 primary-methods (rest before-methods))))
                         #'(lambda (&rest args) (progn (apply 1function args) (apply next-ef args)))
                 ) ) ) )
                 (ef-4 (primary-methods)
                   (if (null primary-methods)
                     nil ; no function, NEXT-METHOD-P reacts on it
                     (let* ((1method (first primary-methods))
                            (1function (std-method-fast-function 1method)))
                       (if (std-method-wants-next-method-p 1method)
                         (let ((next-ef (ef-4 (rest primary-methods))))
                           #'(lambda (&rest args) (apply 1function next-ef args))
                         )
                         #'(lambda (&rest args) (apply 1function args))
                )) ) ) )
          (let ((ef (ef-1 primary-methods before-methods after-methods
                          around-methods)))
            ;; keyword-check (CLtL2 28.1.6.4., 28.1.6.5., ANSI CL 7.6.4., 7.6.5.) ??
            ;; return effective method.
            ;; It will then be applied to the arguments:
            ef))))))

||#

(defun gf-sig-restp (sig)
  (or (sig-rest-p sig) (> (sig-opt-num sig) 0)))

;; Generic functions with optimized dispatch:

(defun make-fast-gf (name lambda-list argument-precedence-order method-class)
  (let ((gf (%make-gf name lambda-list argument-precedence-order method-class)))
    (finalize-fast-gf gf)
    gf))

(let ((prototype-table
        (make-hash-table :key-type '(cons fixnum boolean) :value-type '(simple-array (unsigned-byte 8) (*))
                         :test 'ext:stablehash-equal :warn-if-needs-rehash-after-gc t)))
  (defun finalize-fast-gf (gf)
    (let* ((signature (std-gf-signature gf))
           (reqanz (sig-req-num signature))
           (restp (gf-sig-restp signature))
           (hash-key (cons reqanz restp))
           (prototype
            (or (gethash hash-key prototype-table)
                (setf (gethash hash-key prototype-table)
                      (let* ((reqvars (gensym-list reqanz))
                             (proto-gf
                              (eval `(LET ((GF 'MAGIC))
                                       (DECLARE (COMPILE))
                                       (%GENERIC-FUNCTION-LAMBDA
                                        (,@reqvars ,@(if restp '(&REST ARGS) '()))
                                        (DECLARE (INLINE FUNCALL) (IGNORABLE ,@reqvars ,@(if restp '(ARGS) '())))
                                        (FUNCALL 'INITIAL-FUNCALL-GF GF))))))
                        ;; we must keep (sys::%record-ref proto-gf 1) .
                        ;; (sys::%record-ref proto-gf 2) = #(NIL INITIAL-FUNCALL-GF MAGIC)
                        (sys::%record-ref proto-gf 1))))))
      (setf (sys::%record-ref gf 1) prototype)
      (setf (sys::%record-ref gf 2) (vector 'NIL 'INITIAL-FUNCALL-GF gf))))
  (defun gf-never-called-p (gf)
    (let* ((signature (std-gf-signature gf))
           (reqanz (sig-req-num signature))
           (restp (gf-sig-restp signature))
           (hash-key (cons reqanz restp))
           (prototype (gethash hash-key prototype-table)))
      (eq (sys::%record-ref gf 1) prototype)))
  (defvar *dynamically-modifiable-generic-function-names*
    ;; A list of names of functions, which ANSI CL explicitly denotes as
    ;; "Standard Generic Function"s, meaning that the user may add methods.
    '(add-method allocate-instance class-name describe-object find-method
      function-keywords initialize-instance make-instance method-qualifiers
      no-applicable-method no-next-method no-primary-method print-object
      reinitialize-instance remove-method shared-initialize slot-missing
      change-class update-instance-for-different-class
      update-instance-for-redefined-class
      slot-unbound make-load-form
      ;; Similar functions from the MOP.
      validate-superclass))
  (defvar *warn-if-gf-already-called* t)
  (defun need-gf-already-called-warning-p (gf)
    (and *warn-if-gf-already-called* (not (gf-never-called-p gf))
         (not (memq (sys::closure-name gf)
                    *dynamically-modifiable-generic-function-names*))))
  (defun warn-if-gf-already-called (gf)
    (when (need-gf-already-called-warning-p gf)
      (warn (TEXT "The generic function ~S is being modified, but has already been called.")
            gf)))
) ; let


;; The actual dispatch-code is calculated at the first call of the
;; function, in order to make successive method-definitions not too
;; expensive.

;; first call of a generic function:
(defun initial-funcall-gf (gf)
  (install-dispatch gf)
  gf)

;; Installs the final dispatch-code into a generic function.
(defun install-dispatch (gf)
  (multiple-value-bind (bindings lambdabody) (compute-dispatch gf)
    (let ((preliminary
           (eval `(LET ,bindings
                    (DECLARE (COMPILE))
                     (%GENERIC-FUNCTION-LAMBDA ,@lambdabody)))))
      (setf (sys::%record-ref gf 1) (sys::%record-ref preliminary 1))
      (setf (sys::%record-ref gf 2) (sys::%record-ref preliminary 2)))))

;; Calculates the dispatch-code of a generic function.
;; It looks as follows:
;; (LAMBDA (variables) ; the required vars separately, everything else with &rest
;;   (DECLARE (INLINE ...)) ; everything inline because of %GENERIC-FUNCTION-LAMBDA
;;   If-cascades, where EQL-parameter-specializers and most of the
;;   builtin-classes are queried online via TYPEP.
;;   CLASS-OF is called for the other required-parameters, the results
;;   are gathered and inserted into a hash table as index. There, the effective
;;   method is located:
;;   (LET ((EM (GETHASH (CONS (CLASS-OF ...) ...) ht1)))
;;     (WHEN EM (RETURN-FROM block (APPLY EM Arguments))))

;;   If that failed:
;;   (APPLY 'COMPUTE-AND-ADD-EFFECTIVE-METHOD gf Arguments)
;; )
;; One does not need to write (APPLY ... Arguments)
;; it is done by %GENERIC-FUNCTION-LAMBDA automatically.
(defun compute-dispatch (gf)
  (let* ((signature (std-gf-signature gf))
         (req-anz (sig-req-num signature))
         (req-vars (gensym-list req-anz))
         (restp (gf-sig-restp signature))
         (rest-var (if restp (gensym)))
         (apply-fun (if restp 'APPLY 'FUNCALL))
         (apply-args `(,@req-vars ,@(if restp `(,rest-var) '())))
         (arg-order (std-gf-argorder gf))
         (methods (std-gf-methods gf))
         (block-name (gensym))
         (maybe-no-applicable nil)
         (ht-vars '())) ; list of hashtable variables and their inits
    ;; we do a recursion over the arguments.
    (labels
       ((recursion (remaining-args ; an nthcdr of arg-order
                    remaining-methods ; sublist of methods
                    class-of-exprs) ; list of CLASS-OF expressions
          (if (null remaining-methods)
            (progn
              (setq maybe-no-applicable t)
              'NIL) ; nothing to do, call NO-APPLICABLE-METHOD later
            (if (null remaining-args)
              ;; all arguments processed
              #|| ; use GETHASH :
              (let ((ht-var (gensym))
                    (n (length class-of-exprs)) ; index with n-tuples
                    ht-init ; expression for initialization of ht-var
                    ht-key-binding ; binding of a variable to an n-tuple
                    em-expr ; expression for looking up the EM
                    setf-em-expr) ; expression-part for setting the EM
                (if (eql n 0)
                  (setq ht-init 'NIL
                        ht-key-binding '()
                        em-expr ht-var
                        setf-em-expr `(SETQ ,ht-var))
                  (let ((tuple-var (gensym)))
                    (setq ht-init
                          `(MAKE-HASH-TABLE
                             ; :KEY-TYPE '(CONS ... CLASS ...) :VALUE-TYPE 'FUNCTION
                             :TEST ',(if (eql n 1) 'EXT:STABLEHASH-EQ 'EXT:STABLEHASH-EQUAL)
                             :WARN-IF-NEEDS-REHASH-AFTER-GC 'T)
                          ht-key-binding
                          `((,tuple-var
                             ,(let ((tuple-fun (hash-tuple-function n)))
                                (if (member '&rest (second tuple-fun))
                                  `(,tuple-fun ,@(reverse class-of-exprs))
                                  ;; no &rest -> can optimize
                                  ;; (the compiler is not yet too good at that)
                                  (sublis (mapcar #'cons (second tuple-fun) (reverse class-of-exprs))
                                          (third tuple-fun))))))
                          em-expr
                          `(GETHASH ,tuple-var ,ht-var)
                          setf-em-expr
                          ;; `(SETF (GETHASH ,tuple-var ,ht-var)) would also work;
                          ;; but the following spares two temporary variables:
                          `(SYSTEM::PUTHASH ,tuple-var ,ht-var))))
                (push (list ht-var ht-init) ht-vars)
                `(LET ,ht-key-binding
                   (RETURN-FROM ,block-name
                     (OR ,em-expr
                         (,@setf-em-expr
                               (,apply-fun 'COMPUTE-APPLICABLE-METHODS-EFFECTIVE-METHOD
                                           ',gf ,@apply-args))))))
              |# ; use CLASS-GETHASH and CLASS-TUPLE-GETHASH :
              (let ((ht-var (gensym))
                    (n (length class-of-exprs)) ; index with n-tuples
                    ht-init ; expression for initialization of ht-var
                    em-expr ; expression for looking up the EM
                    setf-em-expr) ; expression-part for setting the EM
                (if (eql n 0)
                  (setq ht-init 'NIL
                        em-expr ht-var
                        setf-em-expr `(SETQ ,ht-var))
                  (setq class-of-exprs
                        (reverse class-of-exprs)
                        ht-init
                        `(MAKE-HASH-TABLE
                           ; :KEY-TYPE '(CONS ... CLASS ...) :VALUE-TYPE 'FUNCTION
                           :TEST ',(if (eql n 1) 'EXT:STABLEHASH-EQ 'EXT:STABLEHASH-EQUAL)
                           :WARN-IF-NEEDS-REHASH-AFTER-GC 'T)
                        em-expr
                        (if (eql n 1) ; whatever is faster
                          ;; `(GETHASH ,@class-of-exprs ,ht-var) ==
                          `(CLASS-GETHASH ,ht-var ,(second (first class-of-exprs)))
                          `(CLASS-TUPLE-GETHASH ,ht-var ,@(mapcar #'second class-of-exprs)))
                        setf-em-expr
                        `(SYSTEM::PUTHASH
                          ,(let ((tuple-fun (hash-tuple-function n)))
                             (if (memq '&rest (second tuple-fun))
                               `(,tuple-fun ,@class-of-exprs)
                               ;; no &rest -> can optimize
                               ;; (the compiler is not yet too good at that)
                               (sublis (mapcar #'cons (second tuple-fun) class-of-exprs)
                                       (third tuple-fun))))
                          ,ht-var)))
                (push (list ht-var ht-init) ht-vars)
                `(RETURN-FROM ,block-name
                   (OR ,em-expr
                       (,@setf-em-expr
                             (,apply-fun 'COMPUTE-APPLICABLE-METHODS-EFFECTIVE-METHOD
                                         ',gf ,@apply-args)))))
              ;; process next argument:
              (let* ((arg-index (first remaining-args))
                     (arg-var (nth arg-index req-vars))
                     (eql-cases ; all EQL-specializers for this argument
                       (remove-duplicates
                         (mapcar #'eql-specializer-object
                           (remove-if-not #'eql-specializer-p
                             (mapcar #'(lambda (m)
                                         (nth arg-index
                                           (std-method-specializers m)))
                               remaining-methods)))
                         :test #'eql))
                     (eql-caselist ; case-list for CASE
                       (mapcar
                         #'(lambda (object)
                             `((,object)
                               ,(recursion
                                  (cdr remaining-args)
                                  (remove-if-not
                                    #'(lambda (m)
                                        (typep object
                                          (nth arg-index
                                            (std-method-specializers m))))
                                    (the list remaining-methods))
                                  class-of-exprs)))
                         eql-cases)))
                ;; until further notice we do not need to consider the
                ;; EQL-methods anymore.
                (setq remaining-methods
                      (remove-if
                        #'(lambda (m)
                            (eql-specializer-p
                              (nth arg-index (std-method-specializers m))))
                        (the list remaining-methods)))
                ((lambda (other-cases)
                   (if eql-caselist
                     `(CASE ,arg-var ,@eql-caselist (T ,other-cases))
                     other-cases))
                 (let ((classes
                         (delete <t>
                           (delete-duplicates
                             (mapcar #'(lambda (m)
                                         (nth arg-index
                                           (std-method-specializers m)))
                                     remaining-methods)))))
                   ;; If all classes that are to be tested for are
                   ;; built-in-classes, then we will inline the type-dispatch,
                   ;; because in the hierarchy of the built-in-classes
                   ;; (that does not know multiple inheritance except for NULL
                   ;; and VECTOR) all CPLs are consistent.
                   ;; Hence, we can work with
                   ;; (subclassp (class-of obj) class) == (typep obj class)
                   ;; In the other case a hash-table-access is necessary
                   ;; anyway. Then we spare the test for the built-in-
                   ;; classes and include it into the hash-table.
                   (if (and (every #'bc-p classes)
                            (<= (length classes) 5)) ; too many cases -> hash
                     (labels
                        ((built-in-subtree (class remaining-classes remaining-methods)
                           ;; treats the cases, when the argument belongs to
                           ;; the Class class and affiliation to one of the
                           ;; remaining-classes has to be tested.
                           ;; (One can presume that (bc-and class x) /= nil
                           ;; for all x from remaining-classes.)
                           (if (null remaining-classes)
                             ;; case differentiation is no longer necessary
                             (recursion
                               (cdr remaining-args)
                               (remove-if-not
                                 #'(lambda (m)
                                     (bc-and class
                                       (nth arg-index
                                         (std-method-specializers m))))
                                 (the list remaining-methods))
                               class-of-exprs)
                             ;; case differentiation via TYPEP
                             (let ((test-class (first remaining-classes)))
                               ;; better choose test-class maximal:
                               (loop
                                 (let ((other-class
                                         (find-if
                                           #'(lambda (x)
                                               (and (subclassp test-class x)
                                                    (not (eq test-class x))))
                                           (the list remaining-classes))))
                                   (unless other-class (return))
                                   (setq test-class other-class)))
                               `(IF (TYPEP ,arg-var ',(class-classname test-class))
                                  ,(built-in-subtree
                                     (bc-and class test-class) ; /= nil !
                                     (remove 'nil
                                       (mapcar
                                         #'(lambda (x) (bc-and x test-class))
                                         (remove test-class remaining-classes)))
                                     (remove-if-not
                                       #'(lambda (m)
                                           (bc-and
                                             (nth arg-index
                                               (std-method-specializers m))
                                             test-class))
                                       (the list remaining-methods)))
                                  ,(built-in-subtree
                                     (bc-and-not class test-class) ; /= nil !
                                     (remove 'nil
                                       (mapcar
                                         #'(lambda (x) (bc-and-not x test-class))
                                         remaining-classes))
                                     (remove-if-not
                                       #'(lambda (m)
                                           (bc-and-not
                                             (nth arg-index
                                               (std-method-specializers m))
                                             test-class))
                                       (the list remaining-methods))))))))
                       (built-in-subtree <t> classes remaining-methods))
                     (recursion
                       (cdr remaining-args)
                       remaining-methods
                       (cons `(CLASS-OF ,arg-var) class-of-exprs))))))))))
      (let ((form (recursion arg-order methods '())))
        (values
          ;; bindings
          (nreverse ht-vars)
          ;; lambdabody
          `((,@req-vars ,@(if restp `(&REST ,rest-var) '()))
            (DECLARE
              (INLINE
               ;; for the case differentiations:
               CASE EQL EQ TYPEP
               ;; at the inline-expansion of TYPEP on built-in-classes:
               ARRAYP BIT-VECTOR-P CHARACTERP COMPLEXP CONSP FLOATP
               FUNCTIONP HASH-TABLE-P INTEGERP LISTP NULL NUMBERP
               PACKAGEP PATHNAMEP SYS::LOGICAL-PATHNAME-P RANDOM-STATE-P
               RATIONALP READTABLEP REALP SYS::SEQUENCEP
               CLOS::STD-INSTANCE-P STREAMP SYS::FILE-STREAM-P
               SYS::SYNONYM-STREAM-P SYS::BROADCAST-STREAM-P
               SYS::CONCATENATED-STREAM-P SYS::TWO-WAY-STREAM-P
               SYS::ECHO-STREAM-P SYS::STRING-STREAM-P STRINGP
               CLOS::STRUCTURE-OBJECT-P SYMBOLP VECTORP
               ;; looking up and calling of the effective method:
               CLASS-OF CONS GETHASH CLASS-GETHASH CLASS-TUPLE-GETHASH
               SYS::PUTHASH FUNCALL APPLY))
            (BLOCK ,block-name
              ,form
              ,@(if maybe-no-applicable
                  `((funcall 'no-method-caller 'no-applicable-method
                             ',gf))))))))))

(defun no-method-caller (no-method-name gf)
  (lambda (&rest args) (apply no-method-name gf args)))

;; Our EQUAL hash-function looks into cons-trees only upto depth 4.
;; A tuple of at most 16 elements can be turned into such a tree.
(defun hash-tuple-function (n) ; n>0
  (case n
    (1 '(lambda (t1) t1))
    (2 '(lambda (t1 t2) (cons t1 t2)))
    (3 '(lambda (t1 t2 t3) (cons t1 (cons t2 t3))))
    (4 '(lambda (t1 t2 t3 t4) (cons (cons t1 t2) (cons t3 t4))))
    (5 '(lambda (t1 t2 t3 t4 t5) (cons (cons t1 t2) (cons t3 (cons t4 t5)))))
    (6 '(lambda (t1 t2 t3 t4 t5 t6)
         (cons (cons t1 t2) (cons (cons t3 t4) (cons t5 t6)))))
    (7 '(lambda (t1 t2 t3 t4 t5 t6 t7)
         (cons (cons t1 (cons t2 t3)) (cons (cons t4 t5) (cons t6 t7)))))
    (8 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8)
         (cons (cons (cons t1 t2) (cons t3 t4))
          (cons (cons t5 t6) (cons t7 t8))) ))
    (9 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9)
         (cons (cons (cons t1 t2) (cons t3 t4))
          (cons (cons t5 t6) (cons t7 (cons t8 t9))))))
    (10 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)
          (cons (cons (cons t1 t2) (cons t3 t4))
           (cons (cons t5 t6) (cons (cons t7 t8) (cons t9 t10))))))
    (11 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11)
           (cons (cons (cons t1 t2) (cons t3 t4))
            (cons (cons t5 (cons t6 t7))
             (cons (cons t8 t9) (cons t10 t11))))))
    (12 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12)
           (cons (cons (cons t1 t2) (cons t3 t4))
            (cons (cons (cons t5 t6) (cons t7 t8))
             (cons (cons t9 t10) (cons t11 t12))))))
    (13 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13)
           (cons (cons (cons t1 t2) (cons t3 (cons t4 t5)))
            (cons (cons (cons t6 t7) (cons t8 t9))
             (cons (cons t10 t11) (cons t12 t13))))))
    (14 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14)
           (cons (cons (cons t1 t2) (cons (cons t3 t4) (cons t5 t6)))
            (cons (cons (cons t7 t8) (cons t9 t10))
             (cons (cons t11 t12) (cons t13 t14))))))
    (15 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15)
           (cons (cons (cons t1 (cons t2 t3)) (cons (cons t4 t5) (cons t6 t7)))
            (cons (cons (cons t8 t9) (cons t10 t11))
             (cons (cons t12 t13) (cons t14 t15))))))
    (16 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16)
           (cons (cons (cons (cons t1 t2) (cons t3 t4))
                  (cons (cons t5 t6) (cons t7 t8)))
            (cons (cons (cons t9 t10) (cons t11 t12))
             (cons (cons t13 t14) (cons t15 t16))))))
    (t '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 &rest more-t)
          (cons (cons (cons (cons t1 t2) (cons t3 t4))
                 (cons (cons t5 t6) (cons t7 t8)))
           (cons (cons (cons t9 t10) (cons t11 t12))
            (cons (cons t13 t14) more-t)))))))

;;; Calculate the effective method for the given arguments.
;;; It is actually the effective method for all arguments,
;;; for the same EQL and class restrictions as the given arguments,
;;; therefore compute dispatch is already taken care of.
(defun compute-applicable-methods-effective-method (gf &rest args)
  (let ((req-num (sig-req-num (std-gf-signature gf))))
    (if (>= (length args) req-num)
      (let ((req-args (subseq args 0 req-num)))
        (let ((methods
                ;; compute-applicable-methods would be sufficient, but the MOP
                ;; specifies a two-step process.
                (multiple-value-bind (methods certain)
                    (funcall (cond ((eq gf #'compute-applicable-methods-using-classes)
                                    #'compute-applicable-methods-using-classes-<standard-generic-function>)
                                   (t #'compute-applicable-methods-using-classes))
                             gf (mapcar #'class-of req-args))
                  (if certain
                    methods
                    (funcall (cond ((or (eq gf #'compute-applicable-methods) (eq gf #'compute-effective-method))
                                    #'compute-applicable-methods-<standard-generic-function>)
                                   (t #'compute-applicable-methods))
                             gf args)))))
          (when (null methods)
            (return-from compute-applicable-methods-effective-method
              (no-method-caller 'no-applicable-method gf)))
          ;; Combine the methods to an effective method:
          (or (cdr (assoc methods (std-gf-effective-method-cache gf) :test #'equal))
              (let ((effective-method
                      (let ((*method-combination-arguments* args))
                        (compute-effective-method-as-function gf (std-gf-method-combination gf) methods))))
                (push (cons methods effective-method) (std-gf-effective-method-cache gf))
                effective-method))))
      (error (TEXT "~S: ~S has ~S required argument~:P, but only ~S arguments were passed to ~S: ~S")
             'compute-applicable-methods-effective-method gf req-num (length args)
             'compute-applicable-methods-effective-method args))))

;; Preliminary.
(defun compute-applicable-methods (gf args)
  (compute-applicable-methods-<standard-generic-function> gf args))

(defun compute-applicable-methods-<standard-generic-function> (gf args)
  (let ((req-num (sig-req-num (std-gf-signature gf))))
    (if (>= (length args) req-num)
      ;; 0. Check the method specializers:
      (let ((methods (std-gf-methods gf)))
        (dolist (method methods)
          (check-method-only-standard-specializers gf method
            'compute-applicable-methods))
        ;; 1. Select the applicable methods:
        (let ((req-args (subseq args 0 req-num)))
          (setq methods
                (remove-if-not #'(lambda (method)
                                   (method-applicable-p method req-args))
                               (the list methods)))
          ;; 2. Sort the applicable methods by precedence order:
          (sort-applicable-methods methods (mapcar #'class-of req-args) (std-gf-argorder gf))))
      (error (TEXT "~S: ~S has ~S required argument~:P, but only ~S arguments were passed to ~S: ~S")
             'compute-applicable-methods gf req-num (length args)
             'compute-applicable-methods args))))

;; compute-applicable-methods-using-classes is just plain redundant, and must
;; be a historical relic of the time before CLOS had EQL specializers (or a
;; brain fart of the PCL authors). But the MOP wants it, so we implement it.

;; Preliminary.
(defun compute-applicable-methods-using-classes (gf req-arg-classes)
  (compute-applicable-methods-using-classes-<standard-generic-function> gf req-arg-classes))

(defun compute-applicable-methods-using-classes-<standard-generic-function> (gf req-arg-classes)
  (unless (and (proper-list-p req-arg-classes) (every #'class-p req-arg-classes))
    (error (TEXT "~S: argument should be a proper list of classes, not ~S")
           'compute-applicable-methods-using-classes req-arg-classes))
  (let ((req-num (sig-req-num (std-gf-signature gf))))
    (if (= (length req-arg-classes) req-num)
      ;; 0. Check the method specializers:
      (let ((methods (std-gf-methods gf)))
        (dolist (method methods)
          (check-method-only-standard-specializers gf method
            'compute-applicable-methods-using-classes))
        ;; 1. Select the applicable methods. Note that the arguments are
        ;; assumed to be _direct_ instances of the given classes, i.e.
        ;; classes = (mapcar #'class-of required-arguments).
        (setq methods
              (remove-if-not #'(lambda (method)
                                 (let ((specializers (std-method-specializers method))
                                       (applicable t) (unknown nil))
                                   (mapc #'(lambda (arg-class specializer)
                                             (if (class-p specializer)
                                               ;; For class specializers,
                                               ;; (typep arg specializer) is equivalent to
                                               ;; (subtypep (class-of arg) specializer).
                                               (unless (subclassp arg-class specializer)
                                                 (setq applicable nil))
                                               ;; For EQL specializers,
                                               ;; (typep arg specializer) is certainly false
                                               ;; if (class-of arg) and (class-of (eql-specializer-object specializer))
                                               ;; differ. Otherwise unknown.
                                               (if (eq arg-class (class-of (eql-specializer-object specializer)))
                                                 (setq unknown t)
                                                 (setq applicable nil))))
                                         req-arg-classes specializers)
                                   (when (and applicable unknown)
                                     (return-from compute-applicable-methods-using-classes-<standard-generic-function>
                                       (values nil nil)))
                                   applicable))
                             (the list methods)))
        ;; 2. Sort the applicable methods by precedence order:
        (values (sort-applicable-methods methods req-arg-classes (std-gf-argorder gf)) t))
      (error (TEXT "~S: ~S has ~S required argument~:P, but ~S classes were passed to ~S: ~S")
             'compute-applicable-methods-using-classes gf req-num (length req-arg-classes)
             'compute-applicable-methods-using-classes req-arg-classes))))

;; There's no real reason for checking the method specializers in
;; compute-applicable-methods, rather than in
;; compute-effective-method-as-function, but that's how the MOP specifies it.
(defun check-method-only-standard-specializers (gf method caller)
  (dolist (spec (std-method-specializers method))
    (unless (or (class-p spec) (typep-class spec <eql-specializer>))
      (error (TEXT "~S: Invalid method specializer ~S on ~S in ~S")
        caller spec method gf))))

(defun compute-effective-method-as-function (gf combination methods)
  ;; Apply method combination:
  (let ((ef-fun (compute-effective-method-as-function-form gf combination methods)))
    ;; Evaluate or compile the resulting form:
    (if (constantp ef-fun) ; constant or self-evaluating form?
      ;; No need to invoke the compile for a constant form.
      ef-fun
      ;; For a general form:
      ;; (eval ef-fun)                                 ; interpreted
      ;; (eval `(LOCALLY (DECLARE (COMPILE)) ,ef-fun)) ; compiled
      (eval `(LET () (DECLARE (COMPILE) (INLINE FUNCALL APPLY))
               ,ef-fun)))))

(defun gf-keyword-arguments (restp signature methods)
  ;; CLtL2 28.1.6.4., 28.1.6.5., ANSI CL 7.6.4., 7.6.5.
  ;; Keyword Arguments in Generic Functions
  (when restp
    ;; The generic function has &REST or &KEY, thus try all methods.
    ;; "If the lambda-list of ... the generic function definition
    ;;  contains &allow-other-keys, all keyword arguments are accepted."
    (unless (sig-allow-p signature)
      ;; "The specific set of keyword arguments accepted ...
      ;;  varies according to the applicable methods."
      (let ((signatures (mapcar #'std-method-signature methods)))
        ;; "A method that has &rest but not &key does not affect the
        ;;   set of acceptable keyword arguments."
        (setq signatures (delete-if-not #'sig-keys-p signatures))
        ;; No &key in the generic function, and no method with &key ==>
        ;; no restriction on the arguments.
        (when (or (sig-keys-p signature) signatures)
          ;; "If the lambda-list of any applicable method ... contains
          ;;  &allow-other-keys, all keyword arguments are accepted."
          (unless (some #'sig-allow-p signatures)
            ;; "The set of keyword arguments accepted for a
            ;;  particular call is the union of the keyword
            ;;  arguments accepted by all applicable methods and
            ;;  the keyword arguments mentioned after &key in the
            ;;  generic function definition."
            (let* ((keywords
                    (remove-duplicates
                     (append (sig-keywords signature)
                             (mapcap #'sig-keywords signatures))
                     :from-end t))
                   (opt-vars (gensym-list (sig-opt-num signature)))
                   (key-vars (gensym-list keywords))
                   (lambdalist-keypart
                     `(&KEY    ; lambdalist-keypart
                       ,@(mapcar #'(lambda (kw var) `((,kw ,var)))
                                 keywords key-vars))))
              (values opt-vars key-vars lambdalist-keypart))))))))
