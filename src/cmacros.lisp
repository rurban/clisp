;;; CLISP Compiler Macros
;;; Sam Steingold 2001-05-09
;;; CLHS 3.2.2.1 http://www.lisp.org/HyperSpec/Body/sec_3-2-2-1.html

(in-package "SYSTEM")

(defun compiler-macro-function (name &optional environment)
  (declare (ignore environment))
  (cond ((symbolp name) (get name 'compiler-macro))
        ((function-name-p name) ; (setf name)
         (get (second name) 'compiler-macro-setf))
        (t (error-of-type
            'source-program-error
            (ENGLISH "~S: function name should be a symbol, not ~S")
            'compiler-macro-function name))))

(defun (setf compiler-macro-function) (newf name &optional environment)
  (declare (ignore environment))
  (cond ((symbolp name) (setf (get name 'compiler-macro) newf))
        ((function-name-p name) ; (setf name)
         (setf (get (second name) 'compiler-macro-setf) newf))
        (t (error-of-type
            'source-program-error
            (ENGLISH "~S: function name should be a symbol, not ~S")
            'compiler-macro-function name))))

;; (proclaim '(inline function-form-funform simple-function-form-p))

;; check whether the form is (FUNCTION fun-form) and return the fun-form
(defun function-form-funform (form)
  (and (consp form) (eq (car form) 'FUNCTION)
       (consp (cdr form)) (null (cddr form))
       (second form)))

;; check whether the form is #'symbol
(defun simple-function-form-p (form)
  (let ((ff (function-form-funform form)))
    (and ff (function-name-p ff))))

;; (funcall (function foo) ...) ==> (foo ...)
(defun strip-funcall-form (form)
  (if (and (eq (car form) 'funcall) (simple-function-form-p (second form)))
      (cons (second (second form)) (cddr form))
      form))

(defmacro define-compiler-macro (&whole form name args &body body)
  (declare (ignore name args body))
  (multiple-value-bind (expansion name lambdalist docstring)
      (sys::make-macro-expansion (cdr form) 'strip-funcall-form)
    (declare (ignore lambdalist))
    `(EVAL-WHEN (COMPILE LOAD EVAL)
      ,@(when docstring
         `((SYSTEM::%SET-DOCUMENTATION ',name 'COMPILER-MACRO ,docstring)))
      (setf (compiler-macro-function ',name) ,expansion)
      ',name)))
