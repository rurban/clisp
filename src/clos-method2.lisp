;;;; Common Lisp Object System for CLISP: Methods
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; auxiliary function: test for lambda-list-marker.
(defun lambda-list-keyword-p (x)
  (memq x lambda-list-keywords))

;;; For DEFMETHOD, DEFGENERIC, GENERIC-FUNCTION, GENERIC-FLET,
;;;     GENERIC-LABELS, WITH-ADDED-METHODS
;; caller: symbol
;; funname: function name, symbol or (SETF symbol)
;; description: (qualifier* spec-lambda-list {declaration|docstring}* form*)
;; ==> method-building-form
(defun analyze-method-description (caller funname description)
  (let ((qualifiers nil))
    (loop
      (when (atom description)
        (error-of-type 'sys::source-program-error
          (TEXT "~S ~S: missing lambda list")
          caller funname))
      (when (listp (car description)) (return))
      (push (pop description) qualifiers))
    (setq qualifiers (nreverse qualifiers))
    ;; build lambdalist, extract parameter-specializer and signature:
    (let ((specialized-lambda-list (car description))
          (body (cdr description)))
      (let ((req-vars '())
            (ignorable-req-vars '())
            (spec-list '())
            (req-specializer-forms '()))
        (do ()
            ((or (atom specialized-lambda-list)
                 (lambda-list-keyword-p (car specialized-lambda-list))))
          (let* ((item (pop specialized-lambda-list))
                 (specializer-name
                   (if (atom item)
                     (progn (push item req-vars) 't)
                     (progn
                       (push (first item) req-vars)
                       (push (first item) ignorable-req-vars) ; CLtL2 p. 840 top
                       (second item)))))
            (push specializer-name spec-list)
            (push (if (class-p specializer-name)
                    `',specializer-name
                    (if (and (consp specializer-name)
                             (eq (car specializer-name) 'EQL))
                      `(LIST 'EQL ,(second specializer-name))
                      `(FIND-CLASS ',specializer-name)))
                  req-specializer-forms )))
        (setq spec-list (nreverse spec-list))
        (sys::check-redefinition
          (list* funname qualifiers spec-list) caller
          ;; do not warn about redefinition when no method was defined
          (and (fboundp 'find-method) (fboundp funname)
               (clos::generic-function-p (fdefinition funname))
               (eql (sig-req-num (gf-signature (fdefinition funname))) (length spec-list))
               (find-method (fdefinition funname) qualifiers spec-list nil)
               "method"))
        (let ((reqanz (length req-vars))
              (lambda-list (nreconc req-vars specialized-lambda-list)))
          (multiple-value-bind (reqs optvars optinits optsvars rest
                                keyp keywords keyvars keyinits keysvars
                                allowp auxvars auxinits)
              (analyze-lambdalist lambda-list
                #'(lambda (errorstring &rest arguments)
                    (error (TEXT "~S ~S: ~A")
                           caller funname
                           (apply #'format nil errorstring arguments))))
            (declare (ignore reqs optinits optsvars keyvars keyinits keysvars
                             auxvars auxinits))
            (let ((optanz (length optvars))
                  (restp (or keyp (not (eql rest 0)))))
              ;; Methods have an implicit &allow-other-keys (CLtL2 28.1.6.4., ANSI CL 7.6.4.):
              (when (and keyp (not allowp))
                (let ((index (+ (position '&KEY lambda-list :test #'eq) 1 (length keywords))))
                  (setq lambda-list
                    `(,@(subseq lambda-list 0 index) &ALLOW-OTHER-KEYS
                      ,@(subseq lambda-list index)))))
              (let* ((self (gensym))
                     (compile nil)
                     (lambdabody
                       (multiple-value-bind (body-rest declarations docstring)
                           (sys::parse-body body t)
                         (declare (ignore docstring))
                         (setq compile (member '(COMPILE) declarations :test #'equal))
                         (when ignorable-req-vars
                           (push `(IGNORABLE ,@(nreverse ignorable-req-vars))
                                 declarations))
                         (let ((lambdabody-part1
                                `(,lambda-list
                                  ,@(if declarations `((DECLARE ,@declarations)))))
                               (lambdabody-part2
                                 (if (eq caller 'generic-function)
                                   body-rest
                                   ;; implicit block
                                   `((BLOCK ,(function-block-name funname)
                                       ,@body-rest)))))
                           (let ((cont (gensym)) ; variable for the continuation
                                 (req-dummies (gensym-list reqanz))
                                 (rest-dummy (if (or restp (> optanz 0)) (gensym)))
                                 (lambda-expr `(LAMBDA ,@lambdabody-part1 ,@lambdabody-part2)))
                             `(; new lambda-list:
                               (,cont
                                ,@req-dummies
                                ,@(if rest-dummy `(&REST ,rest-dummy) '()))
                               ,(add-next-method-local-functions
                                  self cont req-dummies rest-dummy
                                  ;; new body:
                                  (list
                                    (if rest-dummy
                                      `(APPLY (FUNCTION ,lambda-expr)
                                              ,@req-dummies ,rest-dummy)
                                      `(,lambda-expr ,@req-dummies)))))))))
                     (sig (make-signature :req-num reqanz :opt-num optanz
                                          :rest-p restp :keys-p keyp
                                          :keywords keywords :allow-p allowp)))
                (values
                  `(MAKE-STANDARD-METHOD
                     :INITFUNCTION
                       #'(LAMBDA (,self)
                           ,@(if compile '((DECLARE (COMPILE))))
                           (%OPTIMIZE-FUNCTION-LAMBDA (T) ,@lambdabody))
                     :WANTS-NEXT-METHOD-P T
                     :PARAMETER-SPECIALIZERS
                       (LIST ,@(nreverse req-specializer-forms))
                     :QUALIFIERS ',qualifiers
                     :SIGNATURE ,sig
                     ,@(if (eq caller 'DEFGENERIC) `(:ORIGIN T)))
                  sig)))))))))
