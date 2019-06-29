;;; Utilities for function objects
;;; Sam Steingold 2001-2005, 2008
;;; Bruno Haible 2004

(in-package "COMMON-LISP")
(export '(function-lambda-expression))
(in-package "SYSTEM")

;; The signature of a function object.
(defstruct (signature (:type vector) (:conc-name sig-))
  ;; (name nil     :type (or symbol cons))
  (req-num 0    :type fixnum)
  (opt-num 0    :type fixnum)
  (rest-p nil   :type boolean)
  (keys-p nil   :type boolean)
  (keywords nil :type list)
  (allow-p nil  :type boolean))

;; X3J13 vote <88>
;; function --> lambda expression, CLtL2 p. 682
(defun function-lambda-expression (obj)
  (setq obj (coerce obj 'function))
  (cond #+FFI
        ((eq (type-of obj) 'FFI::FOREIGN-FUNCTION)
         (values nil nil (sys::%record-ref obj 0))) ; ff_name
        ((sys::subr-info obj)
         (values nil nil (sys::subr-info obj)))
        ((sys::%compiled-function-p obj) ; compiled closure?
         (let* ((name (sys::closure-name obj))
                (def (get (if (symbolp name)
                              name (get (second name) 'sys::setf-function))
                          'sys::definition)))
           (values (when def (cons 'LAMBDA (cddar def))) t name)))
        ((sys::closurep obj) ; interpreted closure?
         (values (cons 'LAMBDA (sys::%record-ref obj 1)) ; lambda-expression without docstring (from clos_form)
                 (vector ; environment
                         (sys::%record-ref obj 4) ; venv
                         (sys::%record-ref obj 5) ; fenv
                         (sys::%record-ref obj 6) ; benv
                         (sys::%record-ref obj 7) ; genv
                         (sys::%record-ref obj 8)); denv
                 (sys::closure-name obj))))) ; name

(defun function-name (obj)
  ;; Equivalent to (nth-value 2 (function-lambda-expression obj))
  (setq obj (coerce obj 'function))
  (cond #+FFI
        ((eq (type-of obj) 'FFI::FOREIGN-FUNCTION)
         (sys::%record-ref obj 0)) ; ff_name
        ((sys::subr-info obj))
        ((sys::%compiled-function-p obj) ; compiled closure?
         (sys::closure-name obj))
        ((sys::closurep obj) ; interpreted closure?
         (sys::closure-name obj))))

;; Return extended information about a function object, including
;; the name and the signature.
;; 1. name
;; 2. req-num
;; 3. opt-num
;; 4. rest-p
;; 5. key-p
;; 6. keyword-list
;; 7. allow-other-keys-p
(defun function-info (obj &optional no-error)
  (when (and (function-name-p obj) (fboundp obj))
    (setq obj (fdefinition obj)))
  (if (closurep obj)
    (if (sys::%compiled-function-p obj)
      ;; compiled closure
      (multiple-value-bind (req-num opt-num rest-p key-p keywords allow-p)
          (signature obj)
        (values (sys::closure-name obj)
                req-num opt-num rest-p key-p keywords allow-p))
      ;; interpreted closure
      (let ((clos_keywords (sys::%record-ref obj 16)))
        (values (sys::closure-name obj)
                (sys::%record-ref obj 12) ; req_num
                (sys::%record-ref obj 13) ; opt_num
                (sys::%record-ref obj 19) ; rest_flag
                (not (numberp clos_keywords))
                (if (not (numberp clos_keywords)) (copy-list clos_keywords))
                (sys::%record-ref obj 18)))) ; allow_flag
    (cond #+FFI
          ((eq (type-of obj) 'FFI::FOREIGN-FUNCTION)
           (values (function-name obj)
                   (foreign-function-in-arg-count obj) 0 nil nil nil nil))
          (t
           (multiple-value-bind (name req-num opt-num rest-p keywords allow-p)
               (subr-info obj)
             (if name
               (values name req-num opt-num rest-p keywords keywords allow-p)
               (if no-error
                 (values)
                 (coerce obj 'function)))))))) ; error

;; Returns the function definition of a function name, ignoring wrappers
;; installed by TRACE, profilers etc.
(defun unwrapped-fdefinition (funname)
  (let* ((sym (get-funname-symbol funname))
         (def (or (get sym 'sys::traced-definition)
                  (symbol-function sym))))
    (if (macrop def)
      (macro-expander def)
      def)))
