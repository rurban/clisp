;;;; Generic documentation
;;;; Sam Steingold 2002 - 2004

(in-package "CLOS")


;;; documentation
(defgeneric documentation (x doc-type)
  (:method ((x function) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (documentation (nth-value 2 (function-lambda-expression x)) 'function))
  (:method ((x function) (doc-type (eql 'function)))
    (declare (ignore doc-type))
    (documentation (nth-value 2 (function-lambda-expression x)) 'function))
  (:method ((x list) (doc-type (eql 'function)))
    (declare (ignore doc-type))
    (documentation (second (check-function-name 'documentation x)) 'setf))
  (:method ((x list) (doc-type (eql 'compiler-macro)))
    (declare (ignore doc-type))
    (documentation (second (check-function-name  'documentation x))
                   'setf-compiler-macro))
  (:method ((x symbol) (doc-type symbol))
    ;; doc-type = `function', `compiler-macro', `setf', `variable', `type',
    ;; `setf-compiler-macro', `structure'
    (getf (gethash x sys::*documentation*) doc-type))
  (:method ((x symbol) (doc-type (eql 'structure))) ; structure --> type
    (declare (ignore doc-type))
    (documentation x 'type))
  (:method ((x symbol) (doc-type (eql 'class))) ; class --> type
    (declare (ignore doc-type))
    (documentation x 'type))
  (:method ((x method-combination) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (method-combination-documentation x))
  (:method ((x method-combination) (doc-type (eql 'method-combination)))
    (declare (ignore doc-type))
    (method-combination-documentation x))
  (:method ((x standard-method) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (getf (gethash x sys::*documentation*) 'standard-method))
  (:method ((x package) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (getf (gethash x sys::*documentation*) 'package))
  (:method ((x standard-class) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (documentation (class-name x) 'type))
  (:method ((x standard-class) (doc-type (eql 'type)))
    (declare (ignore doc-type))
    (documentation (class-name x) 'type))
  (:method ((x structure-class) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (documentation (class-name x) 'type))
  (:method ((x structure-class) (doc-type (eql 'type)))
    (declare (ignore doc-type))
    (documentation (class-name x) 'type))
  (:method ((x standard-object) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (documentation (class-of x) 'type))
  (:method ((x standard-object) (doc-type (eql 'type)))
    (declare (ignore doc-type))
    (documentation (class-of x) 'type))
  (:method ((x structure-object) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (documentation (class-of x) 'type))
  (:method ((x structure-object) (doc-type (eql 'type)))
    (declare (ignore doc-type))
    (documentation (class-of x) 'type)))

(defgeneric (setf documentation) (new-value x doc-type)
  (:method (new-value (x function) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (sys::%set-documentation (nth-value 2 (function-lambda-expression x))
                             'function new-value))
  (:method (new-value (x function) (doc-type (eql 'function)))
    (declare (ignore doc-type))
    (sys::%set-documentation (nth-value 2 (function-lambda-expression x))
                             'function new-value))
  (:method (new-value (x list) (doc-type (eql 'function)))
    (declare (ignore doc-type))
    (sys::%set-documentation
     (second (check-function-name '(setf documentation) x)) 'setf new-value))
  (:method (new-value (x list) (doc-type (eql 'compiler-macro)))
    (declare (ignore doc-type))
    (sys::%set-documentation
     (second (check-function-name '(setf documentation) x))
     'setf-compiler-macro new-value))
  (:method (new-value (x symbol) (doc-type symbol))
    ;; doc-type = `function', `compiler-macro', `setf', `variable', `type',
    ;; `setf-compiler-macro', `structure'
    (sys::%set-documentation x doc-type new-value))
  (:method (new-value (x symbol) (doc-type (eql 'structure)))
    (declare (ignore doc-type))
    (sys::%set-documentation x 'type new-value))
  (:method (new-value (x symbol) (doc-type (eql 'class)))
    (declare (ignore doc-type))
    (sys::%set-documentation x 'type new-value))
  (:method (new-value (x method-combination) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (setf (method-combination-documentation x) new-value))
  (:method (new-value (x method-combination)
            (doc-type (eql 'method-combination)))
    (declare (ignore doc-type))
    (setf (method-combination-documentation x) new-value))
  (:method (new-value (x standard-method) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (sys::%set-documentation x 'standard-method new-value))
  (:method (new-value (x package) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (sys::%set-documentation x 'package new-value))
  (:method (new-value (x standard-class) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (sys::%set-documentation (class-name x) 'type new-value))
  (:method (new-value (x standard-class) (doc-type (eql 'type)))
    (declare (ignore doc-type))
    (sys::%set-documentation (class-name x) 'type new-value))
  (:method (new-value (x structure-class) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (sys::%set-documentation (class-name x) 'type new-value))
  (:method (new-value (x structure-class) (doc-type (eql 'type)))
    (declare (ignore doc-type))
    (sys::%set-documentation (class-name x) 'type new-value))
  (:method (new-value (x standard-object) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (sys::%set-documentation (class-of x) 'type new-value))
  (:method (new-value (x standard-object) (doc-type (eql 'type)))
    (declare (ignore doc-type))
    (sys::%set-documentation (class-of x) 'type new-value))
  (:method (new-value (x structure-object) (doc-type (eql 't)))
    (declare (ignore doc-type))
    (sys::%set-documentation (class-of x) 'type new-value))
  (:method (new-value (x structure-object) (doc-type (eql 'type)))
    (declare (ignore doc-type))
    (sys::%set-documentation (class-of x) 'type new-value)))
