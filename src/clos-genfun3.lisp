;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


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
  (error-of-type 'sys::source-program-error
    (TEXT "~S method combination, used by ~S, does not allow the method qualifiers ~:S: ~S")
    (method-combination-name (std-gf-method-combination gf)) gf
    (std-method-qualifiers method) method))

;; Add a method to a generic function
(defun std-add-method (gf method)
  (check-signature-congruence gf method)
  (when (std-method-generic-function method)
    (error-of-type 'error
      "~S: ~S already belongs to ~S, cannot also add it to ~S"
      'std-add-method method (std-method-generic-function method) gf))
  (check-method-qualifiers gf method)
  (setf (std-method-fast-function method) nil
        (std-method-generic-function method) gf)
  ;; determine function from initfunction:
  (when (and (null (std-method-function method))
             (null (std-method-fast-function method)))
    (let ((h (funcall (std-method-initfunction method) method)))
      (setf (std-method-fast-function method) (car h))
      (when (car (cdr h)) ; could the variable ",cont" be optimized away?
        (setf (std-method-wants-next-method-p method) nil))))
  ;; method is finished. store:
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
    (finalize-fast-gf gf))
  ;;(sys::closure-set-seclass gf
  ;;  (sys::seclass-or (sys::function-side-effect gf)
  ;;                   (sys::seclass-or (sys::function-side-effect (std-method-function method))
  ;;                                    (sys::function-side-effect (std-method-fast-function method)))))
  gf)

;; removal of a method from a generic function:
(defun std-remove-method (gf method)
  (let ((old-method (find (std-method-initfunction method) (std-gf-methods gf)
                          :key #'std-method-initfunction)))
    (when old-method
      (warn-if-gf-already-called gf)
      (when (need-gf-already-called-warning-p gf)
        (warn (TEXT "Removing method ~S in ~S") old-method gf))
      (cond ((eq gf |#'allocate-instance|) (note-ai-change method))
            ((eq gf |#'initialize-instance|) (note-ii-change method))
            ((eq gf |#'reinitialize-instance|) (note-ri-change method))
            ((eq gf |#'update-instance-for-redefined-class|) (note-uirc-change method))
            ((eq gf |#'update-instance-for-different-class|) (note-uidc-change method))
            ((eq gf |#'shared-initialize|) (note-si-change method)))
      (setf (std-gf-methods gf) (remove old-method (std-gf-methods gf))
            (std-method-generic-function old-method) nil
            (std-method-from-defgeneric old-method) nil)
      ;;(sys::closure-set-seclass gf
      ;;  (reduce #'sys::seclass-or (std-gf-methods gf)
      ;;          :key #'(lambda (method)
      ;;                   (sys::seclass-or (sys::function-side-effect (std-method-function method))
      ;;                                    (sys::function-side-effect (std-method-fast-function method))))
      ;;          :initial-value sys::*seclass-foldable*))
      (finalize-fast-gf gf)))
  gf)

;; find a method in a generic function:
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
  ;; so to speak
  ;;   (find hypothetical-method (std-gf-methods gf) :test #'methods-agree-p)
  ;; cf. methods-agree-p
  (dolist (method (std-gf-methods gf))
    (when (and (equal (std-method-qualifiers method) qualifiers)
               (specializers-agree-p (std-method-specializers method)
                                     specializers))
      (return-from std-find-method method)))
  (if errorp
    (error-of-type 'error
      (TEXT "~S has no method with qualifiers ~:S and specializers ~S")
      gf qualifiers specializers)
    nil))


;;; DEFMETHOD

(defmacro defmethod (funname &rest method-description)
  (unless (function-name-p funname)
    (error-of-type 'sys::source-program-error
      (TEXT "~S: the name of a function must be a symbol, not ~S")
      'defmethod funname))
  (multiple-value-bind (method-initargs-forms signature)
      (analyze-method-description 'defmethod funname method-description)
    `(LET ()
      (COMPILER::EVAL-WHEN-COMPILE
       (COMPILER::C-DEFUN ',funname ,signature nil 'defmethod))
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
                         (make-fast-gf funname gf-lambdalist (subseq m-lambdalist 0 req-num)))))))
         (method
           (if (listp method-or-initargs)
             ;; During bootstrap, the only used method-class is <standard-method>.
             ;; After bootstrap, make-instance-<standard-method> is the same
             ;; as the general make-instance.
             (apply #'make-instance-<standard-method> (std-gf-default-method-class gf)
                    method-or-initargs)
             method-or-initargs)))
    (std-add-method gf method)
    method))

;;; (DECLAIM-METHOD function-name qualifier* spec-lambda-list)
;; does the compile-time side effects of
;; (DEFMETHOD function-name qualifier* spec-lambda-list ...)

(defmacro declaim-method (funname &rest method-description)
  (unless (function-name-p funname)
    (error-of-type 'sys::source-program-error
      (TEXT "~S: the name of a function must be a symbol, not ~S")
      'declaim-method funname))
  (multiple-value-bind (method-initargs-forms signature)
      (analyze-method-description 'defmethod funname method-description)
    (declare (ignore method-initargs-forms))
    `(COMPILER::EVAL-WHEN-COMPILE
       (COMPILER::C-DEFUN ',funname ,signature nil 'defmethod))))


;;; For DEFGENERIC, GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS,
;;;     WITH-ADDED-METHODS
;; caller: symbol
;; funname: function name, symbol or (SETF symbol)
;; lambdalist: lambdalist of the generic function
;; options: (option*)
;; --> signature, argument-precedence-order, method combination, method-forms, docstring
(defun analyze-defgeneric (caller funname lambdalist options)
  (unless (function-name-p funname)
    (error-of-type 'sys::source-program-error
      (TEXT "~S: the name of a function must be a symbol, not ~S")
      caller funname lambdalist))
  ;; Parse the lambdalist:
  (analyze-defgeneric-lambdalist caller funname lambdalist)
  ;; Process the options:
  (let ((method-forms '())
        (method-combination 'STANDARD)
        (argorders nil)
        (docstrings nil))
    (dolist (option options)
      (unless (listp option)
        (error-of-type 'sys::source-program-error
          (TEXT "~S ~S: not a ~S option: ~S")
          caller funname 'defgeneric option))
      (case (first option)
        (DECLARE
         ;; The declaration is being ignored.
         ;; The compiler will ignore it in any case.
         (unless (every
                   #'(lambda (x) (and (consp x) (eq (first x) 'OPTIMIZE)))
                   (rest option))
           (warn (TEXT "~S ~S: Only ~S declarations are permitted: ~S")
                 caller funname 'optimize option)))
        (:ARGUMENT-PRECEDENCE-ORDER
         (when argorders
           (error-of-type 'sys::source-program-error
             (TEXT "~S ~S: ~S may only be specified once.")
             caller funname ':argument-precedence-order))
         (setq argorders option))
        (:DOCUMENTATION
         (unless (and (eql (length option) 2) (stringp (second option)))
           (error-of-type 'sys::source-program-error
             (TEXT "~S ~S: A string must be specified after ~S : ~S")
             caller funname ':documentation option))
         (when docstrings
           (error-of-type 'sys::source-program-error
             (TEXT "~S ~S: Only one ~S string is allowed")
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
             (setf method-combination (rest option))
             (error-of-type 'sys::source-program-error
               (TEXT "~S ~S: Invalid method combination: ~S")
               caller funname option))))
        (:GENERIC-FUNCTION-CLASS
         ;; the class of the generic function is being ignored.
         (unless (equal (rest option) '(STANDARD-GENERIC-FUNCTION))
           (error-of-type 'sys::source-program-error
             (TEXT "~S ~S: The only valid generic function class name is ~S : ~S")
             caller funname 'standard-generic-function option)))
        (:METHOD-CLASS
         ;; the class of the methods is being ignored.
         (unless (equal (rest option) '(STANDARD-METHOD))
           (error-of-type 'sys::source-program-error
             (TEXT "~S ~S: The only valid method class name is ~S : ~S")
             caller funname 'standard-method option)))
        (:METHOD
         (let ((method-initargs-forms
                 (analyze-method-description caller funname (rest option))))
           (push `(MAKE-INSTANCE-<STANDARD-METHOD> <STANDARD-METHOD> ,@method-initargs-forms)
                 method-forms)))
        (t (error-of-type 'sys::source-program-error
             (TEXT "~S ~S: invalid syntax in ~S option: ~S")
             caller funname 'defstruct option))))
    ;; Check :argument-precedence-order :
    (multiple-value-bind (signature argument-precedence-order argorder)
        (check-gf-lambdalist+argorder lambdalist (rest argorders) argorders
          #'(lambda (errorstring &rest arguments)
              (error-of-type 'sys::source-program-error
                (TEXT "~S ~S: ~A")
                caller funname (apply #'format nil errorstring arguments))))
      (declare (ignore argorder))
      (values signature
              argument-precedence-order
              method-combination
              ;; list of the method-forms
              (nreverse method-forms)
              ;; docstring or nil
              (car docstrings)))))

;; parse the lambdalist:
;; lambdalist --> reqnum, req-vars, optnum, restp, keyp, keywords, allowp
(defun analyze-defgeneric-lambdalist (caller funname lambdalist)
  (multiple-value-bind (reqvars optvars rest keyp keywords keyvars allowp)
      (sys::analyze-generic-function-lambdalist lambdalist
        #'(lambda (errorstring &rest arguments)
            (error-of-type 'sys::source-program-error
              (TEXT "~S ~S: invalid generic function lambda-list: ~A")
              caller funname (apply #'format nil errorstring arguments))))
    (declare (ignore keyvars))
    (values (length reqvars) reqvars (length optvars)
            (or (not (eql rest 0)) keyp) ; &key implies &rest
            keyp keywords allowp)))

;; transform lambdalist into calling convention:
(defun defgeneric-lambdalist-callinfo (caller funname lambdalist)
  (multiple-value-bind (reqanz req-vars optanz restp keyp keywords allowp)
      (analyze-defgeneric-lambdalist caller funname lambdalist)
    (declare (ignore req-vars keyp))
    (callinfo reqanz optanz restp keywords allowp)))


;;; DEFGENERIC

(defmacro defgeneric (funname lambda-list &rest options)
  (multiple-value-bind (signature argument-precedence-order method-combo method-forms docstring)
      (analyze-defgeneric 'defgeneric funname lambda-list options)
    `(LET ()
       (DECLARE (SYS::IN-DEFUN ,funname))
       (COMPILER::EVAL-WHEN-COMPILE
        (COMPILER::C-DEFUN ',funname ',signature nil 'defgeneric))
       ;; NB: no (SYSTEM::REMOVE-OLD-DEFINITIONS ',funname)
       ,@(if docstring
           (let ((symbolform
                   (if (atom funname)
                     `',funname
                     `(LOAD-TIME-VALUE (SYSTEM::GET-SETF-SYMBOL
                                        ',(second funname))))))
             `((SYSTEM::%SET-DOCUMENTATION ,symbolform 'FUNCTION
                                           ',docstring))))
       (DO-DEFGENERIC ',funname ',lambda-list ',signature ',argument-precedence-order ',method-combo
                      ,@method-forms))))

(defun ensure-generic-function (function-name &key argument-precedence-order
                                declare documentation environment
                                generic-function-class lambda-list
                                method-class method-combination)
  (declare (ignore environment))
  (multiple-value-bind (signature argument-precedence-order method-combo)
      (analyze-defgeneric
       'defgeneric function-name lambda-list
       `(,@(if declare `(:declare ,declare))
         ,@(if documentation `(:documentation ,documentation))
         ,@(if argument-precedence-order
               `(:argument-precedence-order ,argument-precedence-order))
         ,@(if generic-function-class
               `(:generic-function-class
                 ,(if (class-p generic-function-class)
                      (class-name generic-function-class)
                      generic-function-class)))
         ,@(if method-combination `(:method-combination ,method-combination))
         ,@(if method-class `(:method-class ,method-class))))
    (do-defgeneric function-name lambda-list signature argument-precedence-order method-combo)))
