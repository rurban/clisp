;; Tracer
;; Bruno Haible 13.2.1990, 15.3.1991, 4.4.1991
;; German comments translated into English: Stefan Kain 2001-12-26

;; (TRACE) returns a list of traced functions
;; (TRACE fun ...) additionally traces the functions fun, ... .
;; Format for fun:
;;  Either a Symbol
;;   symbol
;;  or a List made of a Symbol and a few Keyword-Arguments (pair-wise!)
;;   (symbol
;;    [:suppress-if form]   ; no Trace-Output, as long as form is true
;;    [:step-if form]       ; Trace moves into the Stepper, if form is true
;;    [:pre form]           ; executes form before function call
;;    [:post form]          ; executes form after  function call
;;    [:pre-break-if form]  ; Trace moves into break-loop before function call,
;;                          ; if form is true
;;    [:post-break-if form] ; Trace moves into break-loop after  function call,
;;                          ; if form is true
;;    [:pre-print form]     ; prints the values of form before function call
;;    [:post-print form]    ; prints the values of form after  function call
;;    [:print form]         ; prints the values of form before
;;                          ; and after the function call
;;   )
;;   In all these forms *TRACE-FUNCTION* (the function itself),
;;   *TRACE-ARGS* (the function arguments),
;;   *TRACE-FORM* (the function-/macro-call as form),
;;   and after function call also *TRACE-VALUES* (the list of values
;;   of the function call) can be accessed,
;;   and the function can be left with RETURN with given values.
;; (UNTRACE) returns list of traced functions, discards the all.
;; (UNTRACE symbol ...) discards symbol, ... from the list of traced
;;   functions.
;; TRACE and UNTRACE are also applicable to functions (SETF symbol) and macros,
;;   not however applicable to locally defined functions and macros.

(in-package "COMMON-LISP")
(export '(trace untrace))
(in-package "EXT")
(export '(*trace-function* *trace-args* *trace-form* *trace-values*
          fdefinition-local))
(in-package "SYSTEM")

(proclaim '(special *trace-function* *trace-args* *trace-form* *trace-values*))
(defvar *traced-functions* nil) ; list of currently traced function-names
;; So long as a function-name funname [resp. more exactly: the Symbol
;; symbol = (get-funname-symbol funname)] are traced, the Property
;; sys::traced-definition contains the old content of the function-cell, the
;; Property sys::tracing-definition contains the new content of the
;; function-cell, and the function-name is element of the list
;; *traced-functions*.
;; Meanwhile the content of the function-cell can change, however!
;; At all events the following is true:
;;  (and (fboundp symbol)
;;       (eq (symbol-function symbol) (get symbol 'sys::tracing-definition)))
;; ===>   (member funname *traced-functions* :test #'equal)
;; <==>   (get symbol 'sys::traced-definition)
(defvar *trace-level* 0) ; nesting depth for Trace-Output

(defun fdefinition-local (spec)
  "Return the closure defined locally with LABELS or FLET.
SPEC is a list of (CLOSURE SUB-CLOSURE SUB-SUB-CLOSURE ...)
CLOSURE must be compiled."
  (flet ((subclosure (closure name)
           (let ((consts (closure-consts closure))
                 ;; compiler::symbol-suffix is defined in compiler.lisp
                 (nm (compiler::symbol-suffix (closure-name closure) name)))
             (or (find nm consts :test #'eq :key
                       (lambda (obj) (and (closurep obj) (closure-name obj))))
                 (error (ENGLISH "~s: no local name ~s in ~s")
                        'fdefinition-local name closure)))))
    (let ((closure (fdefinition (car spec))))
      (unless (closurep closure)
        (error-of-type 'type-error
          :datum closure :expected-type 'closure
          (ENGLISH "~S: ~S must name a closure")
          'fdefinition-local (car spec)))
      (unless (compiled-function-p closure)
        (setq closure (compile nil closure)))
      (dolist (spe (cdr spec))
        (setq closure (subclosure closure spe)))
      closure)))

;; Functions, that the Tracer calls at runtime and that the user could
;; trace, must be called in their untraced form.
;; Instead of (fun arg ...) use instead (SYS::%FUNCALL '#,#'fun arg ...)
;; or (SYS::%FUNCALL (LOAD-TIME-VALUE #'fun) arg ...).
;; This applies for all used functions here from #<PACKAGE LISP> except for
;; CAR, CDR, CONS, APPLY, VALUES-LIST (which are all compiled inline).

(defmacro trace (&rest funs)
  (if (null funs)
    '*traced-functions*
    (cons 'append
      (mapcar #'(lambda (fun)
                  (if (or (atom fun) (function-name-p fun))
                    (trace1 fun)
                    (apply #'trace1 fun)))
              funs))))

(defun trace1 (funname &key (suppress-if nil) (step-if nil)
                            (pre nil) (post nil)
                            (pre-break-if nil) (post-break-if nil)
                            (pre-print nil) (post-print nil) (print nil)
                       &aux (old-function (gensym)) (macro-flag (gensym)))
  (unless (function-name-p funname)
    (error-of-type 'source-program-error
      (ENGLISH "~S: function name should be a symbol, not ~S")
      'trace funname))
  (let ((symbolform
          (if (atom funname)
            `',funname
            `(load-time-value (get-setf-symbol ',(second funname))))))
    `(block nil
       (unless (fboundp ,symbolform) ; function defined at all?
         (warn (ENGLISH "~S: undefined function ~S")
               'trace ',funname)
         (return nil))
       (when (special-operator-p ,symbolform) ; Special-Form: not traceable
         (warn (ENGLISH "~S: cannot trace special operator ~S")
               'trace ',funname)
         (return nil))
       (let* ((,old-function (symbol-function ,symbolform))
              (,macro-flag (consp ,old-function)))
         (unless (eq ,old-function (get ,symbolform 'sys::tracing-definition))
           ;; already traced?
           (setf (get ,symbolform 'sys::traced-definition) ,old-function)
           (pushnew ',funname *traced-functions* :test #'equal))
         (format t (ENGLISH "~&;; Tracing ~:[function~;macro~] ~S.")
                   ,macro-flag ',funname)
         (setf (get ,symbolform 'sys::tracing-definition)
           (setf (symbol-function ,symbolform)
             ;; new function, that replaces the original one:
             ,(let ((newname (concat-pnames "TRACED-"
                                            (get-funname-symbol funname)))
                    (body
                     `((declare (compile)
                        (inline car cdr cons apply values-list))
                       (let ((*trace-level* (trace-level-inc)))
                         (block nil
                           (unless ,suppress-if
                             (trace-pre-output))
                           ,@(when pre-print
                               `((trace-print (multiple-value-list
                                               ,pre-print))))
                           ,@(when print
                               `((trace-print (multiple-value-list ,print))))
                           ,pre
                           ,@(when pre-break-if
                               `((when ,pre-break-if (sys::break-loop t))))
                           (let ((*trace-values*
                                  (multiple-value-list
                                   (if ,step-if
                                     (trace-step-apply)
                                     (apply *trace-function* *trace-args*)))))
                             ,@(when post-break-if
                                 `((when ,post-break-if (sys::break-loop t))))
                             ,post
                             ,@(when print
                                 `((trace-print (multiple-value-list ,print))))
                             ,@(when post-print
                                `((trace-print (multiple-value-list
                                                ,post-print))))
                             (unless ,suppress-if
                               (trace-post-output))
                             (values-list *trace-values*)))))))
                `(if (not ,macro-flag)
                   (function ,newname
                     (lambda (&rest *trace-args*
                              &aux (*trace-form*
                                    (make-apply-form ',funname *trace-args*))
                                   (*trace-function*
                                    (get-traced-definition ,symbolform)))
                       ,@body))
                   (make-macro
                     (function ,newname
                       (lambda (&rest *trace-args*
                                &aux (*trace-form* (car *trace-args*))
                                     (*trace-function*
                                      (cdr (get-traced-definition
                                            ,symbolform))))
                         ,@body))))))))
       '(,funname))))

;; auxiliary functions:
;; return next-higher Trace-Level:
(defun trace-level-inc ()
  (%funcall '#,#'1+ *trace-level*))
;; fetch original function definition:
(defun get-traced-definition (symbol)
  (%funcall '#,#'get symbol 'sys::traced-definition))
;; apply, but step by step:
(defun trace-step-apply ()
  ;; (eval `(step (apply ',*trace-function* ',*trace-args*)))
  (%funcall '#,#'eval
    (cons 'step
     (cons
       (cons 'apply
        (cons (cons 'quote (cons *trace-function* nil))
         (cons (cons 'quote (cons *trace-args* nil))
          nil)))
      nil))))
;; build Eval-Form, that corresponds to an Apply (approximately) :
(defun make-apply-form (funname args)
  (declare (inline cons mapcar))
  (cons funname
    (mapcar #'(lambda (arg)
                ;; (list 'quote arg)
                (cons 'quote (cons arg nil)))
            args)))
;; Output before call, uses *trace-level* and *trace-form*
(defun trace-pre-output ()
  (%funcall '#,#'terpri *trace-output*)
  (%funcall '#,#'write *trace-level* :stream *trace-output* :base 10 :radix t)
  (%funcall '#,#'write-string " Trace: " *trace-output*)
  (%funcall '#,#'prin1 *trace-form* *trace-output*))
;; Output after call, uses *trace-level*, *trace-form* and *trace-values*
(defun trace-post-output ()
  (declare (inline car cdr consp atom))
  (%funcall '#,#'terpri *trace-output*)
  (%funcall '#,#'write *trace-level* :stream *trace-output* :base 10 :radix t)
  (%funcall '#,#'write-string " Trace: " *trace-output*)
  (%funcall '#,#'write (car *trace-form*) :stream *trace-output*)
  (%funcall '#,#'write-string " ==> " *trace-output*)
  (trace-print *trace-values* nil))
;; Output of a list of values:
(defun trace-print (vals &optional (nl-flag t))
  (when nl-flag (%funcall '#,#'terpri *trace-output*))
  (when (consp vals)
    (loop
      (let ((val (car vals)))
        (%funcall '#,#'prin1 val *trace-output*))
      (setq vals (cdr vals))
      (when (atom vals) (return))
      (%funcall '#,#'write-string ", " *trace-output*))))

(defmacro untrace (&rest funs)
  `(mapcan #'untrace1
    ,(if (null funs) `(copy-list *traced-functions*) `',funs)))

(defun untrace1 (funname)
  (unless (function-name-p funname)
    (error-of-type 'source-program-error
      (ENGLISH "~S: function name should be a symbol, not ~S")
      'untrace funname))
  (let* ((symbol (get-funname-symbol funname))
         (old-definition (get symbol 'sys::traced-definition)))
    (prog1
      (if old-definition
        ;; symbol was traced
        (progn
          (if (and (fboundp symbol)
                   (eq (symbol-function symbol)
                       (get symbol 'sys::tracing-definition)))
            (setf (symbol-function symbol) old-definition)
            (warn (ENGLISH "~S: ~S was traced and has been redefined!")
                  'untrace funname))
          `(,funname))
        ;; funname was not traced
        '())
      (untrace2 funname))))

(defun untrace2 (funname)
  (let ((symbol (get-funname-symbol funname)))
    (remprop symbol 'sys::traced-definition)
    (remprop symbol 'sys::tracing-definition))
  (setq *traced-functions* (delete funname *traced-functions* :test #'equal)))
