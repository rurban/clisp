;;;; Common Lisp Object System for CLISP: Method Combination
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; James Anderson 2003

(in-package "CLOS")


;;; Standard method combination.

(defun standard-method-combination-expander (gf combination options args)
  (declare (ignore combination))
  (unless (null options)
    (error-of-type 'sys::source-program-error
      (TEXT "~S ~S: The ~S method combination permits no options: ~S")
      'compute-effective-method (sys::closure-name gf) 'standard options))
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
                         methods))
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
  ;; 28.1.7.2, 28.1.7.4 method qualifiers
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
        :expander #'standard-method-combination-expander
        :check-method-qualifiers #'standard-method-combination-check-method-qualifiers
        :call-next-method-allowed #'standard-method-combination-call-next-method-allowed))
