;;; Utilities for function objects
;;; Sam Steingold 2001-2004
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

;; Check an argument that should be a function name, giving the user the
;; opportunity to correct it if it is not.
(defun check-function-name (caller funname)
  (do () ((function-name-p funname) funname)
    (setq funname
          (check-value nil
            (coerce-to-condition (TEXT "~s: ~s is not a function name")
                                 (list caller funname) 'check-function-name
                                 'simple-source-program-error)))))

;; X3J13 vote <88>
;; function --> lambda expression, CLtL2 p. 682
(defun function-lambda-expression (obj)
  (setq obj (coerce obj 'function))
  (cond #+FFI
        ((eq (type-of obj) 'FFI::FOREIGN-FUNCTION)
         (values nil nil (sys::%record-ref obj 0)))
        ((sys::subr-info obj)
         (values nil nil (sys::subr-info obj)))
        ((sys::%compiled-function-p obj) ; compiled closure?
         (let* ((name (sys::%record-ref obj 0))
                (def (get name 'sys::definition)))
           (values (when def (cons 'LAMBDA (cddar def))) t name)))
        ((sys::closurep obj) ; interpreted closure?
         (values (cons 'LAMBDA (sys::%record-ref obj 1)) ; lambda-expression without docstring
                 (vector ; environment
                         (sys::%record-ref obj 4) ; venv
                         (sys::%record-ref obj 5) ; fenv
                         (sys::%record-ref obj 6) ; benv
                         (sys::%record-ref obj 7) ; genv
                         (sys::%record-ref obj 8)); denv
                 (sys::%record-ref obj 0))))) ; name

(defun function-name (obj)
  ;; Equivalent to (nth-value 2 (function-lambda-expression obj))
  (setq obj (coerce obj 'function))
  (cond #+FFI
        ((eq (type-of obj) 'FFI::FOREIGN-FUNCTION)
         (sys::%record-ref obj 0))
        ((sys::subr-info obj))
        ((sys::%compiled-function-p obj) ; compiled closure?
         (sys::%record-ref obj 0))
        ((sys::closurep obj) ; interpreted closure?
         (sys::%record-ref obj 0))))
