;;;; Common Lisp Object System for CLISP
;;;; Specializers
;;;; Part 1: Class definitions, preliminary accessors, utility functions.
;;;; Bruno Haible 2004-05-15

(in-package "CLOS")

;;; ===========================================================================

;;; The abstract class <specializer> allows specializers for methods in
;;; generic functions (i.e. classes and EQL-specializers) to be treated in a
;;; homogenous way.

(defvar *<specializer>-defclass*
  '(defclass specializer (standard-stablehash metaobject)
     (($direct-generic-functions ; weak-list or weak-hash-table of GFs that use
                                ; this specializer
        :initform nil)
      ($direct-methods          ; weak-list or weak-hash-table of methods that
                                ; use this specializer
        :initform nil))
     (:fixed-slot-locations)))

;; Fixed slot locations.
(defconstant *<specializer>-direct-generic-functions-location* 2)
(defconstant *<specializer>-direct-methods-location* 3)

;; Preliminary accessors.
(defun specializer-direct-generic-functions-table (object)
  (sys::%record-ref object *<specializer>-direct-generic-functions-location*))
(defun (setf specializer-direct-generic-functions-table) (new-value object)
  (setf (sys::%record-ref object *<specializer>-direct-generic-functions-location*) new-value))
(defun specializer-direct-methods-table (object)
  (sys::%record-ref object *<specializer>-direct-methods-location*))
(defun (setf specializer-direct-methods-table) (new-value object)
  (setf (sys::%record-ref object *<specializer>-direct-methods-location*) new-value))

;; Initialization of a <specializer> instance.
(defun shared-initialize-<specializer> (specializer situation &rest args
                                        &key &allow-other-keys)
  (apply #'shared-initialize-<standard-stablehash> specializer situation args)
  (unless *classes-finished*
    ; Bootstrapping: Simulate the effect of #'%shared-initialize.
    (when (eq situation 't) ; called from initialize-instance?
      (setf (specializer-direct-generic-functions-table specializer) nil)
      (setf (specializer-direct-methods-table specializer) nil)))
  specializer)

;;; ===========================================================================

;;; The class <eql-specializer> represents an EQL-specializer.

(defvar <eql-specializer> 'eql-specializer)
(defvar *<eql-specializer>-defclass*
  '(defclass eql-specializer (specializer)
     (($singleton :initarg singleton))
     (:fixed-slot-locations)))
(defvar *<eql-specializer>-class-version* (make-class-version))

;; Fixed slot locations.
(defconstant *<eql-specializer>-singleton-location* 4)

;; Preliminary accessors.
(defun eql-specializer-singleton (object)
  (sys::%record-ref object *<eql-specializer>-singleton-location*))
(defun (setf eql-specializer-singleton) (new-value object)
  (setf (sys::%record-ref object *<eql-specializer>-singleton-location*) new-value))

;; Initialization of an <eql-specializer> instance.
(defun shared-initialize-<eql-specializer> (specializer situation &rest args
                                            &key ((singleton singleton) nil singleton-p)
                                            &allow-other-keys)
  (apply #'shared-initialize-<specializer> specializer situation args)
  (unless *classes-finished*
    ; Bootstrapping: Simulate the effect of #'%shared-initialize.
    (when singleton-p
      (setf (eql-specializer-singleton specializer) singleton)))
  specializer)

(defun initialize-instance-<eql-specializer> (specializer &rest args
                                              &key &allow-other-keys)
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'initialize-instance later.
  (apply #'shared-initialize-<eql-specializer> specializer 't args))

(defun make-instance-<eql-specializer> (class &rest args
                                        &key &allow-other-keys)
  ;; class = <eql-specializer>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore class))
  (let ((specializer (allocate-metaobject-instance *<eql-specializer>-class-version* 5)))
    (apply #'initialize-instance-<eql-specializer> specializer args)))

;; Type test.
(defun eql-specializer-p (object)
  (and (std-instance-p object)
       (let ((cv (sys::%record-ref object 0)))
         ; Treat the most frequent case first, for bootstrapping.
         (or (eq cv *<eql-specializer>-class-version*)
             (gethash <eql-specializer>
                      (class-all-superclasses (class-of object)))))))

;;; ===========================================================================

#|
;; Adds a generic function to the list of direct generic functions.
(defun add-direct-generic-function (specializer gf) ...)
;; Removes a generic function from the list of direct generic functions.
(defun remove-direct-generic-function (specializer gf) ...)
;; Returns the currently existing direct generic functions, as a freshly consed
;; list.
(defun list-direct-generic-functions (specializer) ...)
|#
(def-weak-set-accessors specializer-direct-generic-functions-table generic-function
  add-direct-generic-function-internal
  remove-direct-generic-function-internal
  list-direct-generic-functions)

;; The MOP description of specializer-direct-generic-functions specifies that
;; the list of direct-generic-functions is managed through add-direct-method
;; and remove-direct-method. However, the MOP description of remove-method
;; says that when remove-direct-method is called, the association between the
;; method and the generic function has already been dissolved. (This is a flaw
;; in the MOP: The notification upon removal should be split into a notification
;; before removal and a notification after removal.) Therefore the list of
;; direct-generic-functions cannot be managed incrementally when a method is
;; removed. Instead, we clear the list and mark it as to be recomputed from
;; the direct-methods list.
(fmakunbound 'remove-direct-generic-function-internal)
(defun forget-direct-generic-functions (specializer)
  (setf (specializer-direct-generic-functions-table specializer) 'unknown))
(defun add-direct-generic-function (specializer gf)
  (unless (eq (specializer-direct-generic-functions-table specializer) 'unknown)
    (add-direct-generic-function-internal specializer gf)))
(defun compute-direct-generic-functions (specializer)
  (let* ((methods (specializer-direct-methods specializer))
         (gfs (delete-duplicates (mapcar #'method-generic-function methods) :test #'eq)))
    (when (memq nil gfs)
      (error (TEXT "~S: Some methods have been removed from their generic function, but the list in the ~S specializer was not updated.")
             'specializer-direct-generic-functions specializer))
    (cond ((null gfs) nil)
          ((<= (length gfs) 10) (ext:make-weak-list gfs))
          (t (let ((ht (make-hash-table :key-type 'generic-function :value-type '(eql t)
                                        :test 'ext:stablehash-eq :warn-if-needs-rehash-after-gc t
                                        :weak :key)))
               (dolist (x gfs) (setf (gethash x ht) t))
               ht)))))
(defun update-list-direct-generic-functions (specializer)
  (when (eq (specializer-direct-generic-functions-table specializer) 'unknown)
    (setf (specializer-direct-generic-functions-table specializer)
          (compute-direct-generic-functions specializer)))
  (list-direct-generic-functions specializer))

;; MOP p. 103
(defun specializer-direct-generic-functions (specializer)
  (update-list-direct-generic-functions specializer))

(defun add-direct-method-generic-function (specializer method)
  (let ((gf (method-generic-function method)))
    (unless gf
      (error (TEXT "~S: Attempting to add a method that is not attached to a generic function: ~S")
             'add-direct-method method))
    (add-direct-generic-function specializer gf)))

#|
;; Adds a method to the list of direct methods.
(defun add-direct-method (specializer method) ...)
;; Removes a method from the list of direct methods.
(defun remove-direct-method (specializer method) ...)
;; Returns the currently existing direct methods, as a freshly consed list.
(defun list-direct-methods (specializer) ...)
|#
(def-weak-set-accessors specializer-direct-methods-table method
  add-direct-method-internal
  remove-direct-method-internal
  list-direct-methods)

;; Preliminary.
(defun add-direct-method (specializer method)
  (add-direct-method-internal specializer method)
  (add-direct-method-generic-function specializer method))
(defun remove-direct-method (specializer method)
  (remove-direct-method-internal specializer method)
  (forget-direct-generic-functions specializer))

;; MOP p. 103
(defun specializer-direct-methods (specializer)
  (list-direct-methods specializer))

;;; ===========================================================================

;; EQL-specializers for numbers.
(defvar *eql-specializer-table*
        (make-hash-table :key-type 'number :value-type 'eql-specializer
                         :test 'ext:fasthash-eql :warn-if-needs-rehash-after-gc t))

;; EQL-specializers for other kinds of objects.
(defvar *eq-specializer-table*
        (make-hash-table :key-type '(not number) :value-type 'eql-specializer
                         :test 'ext:stablehash-eq
                         :weak :key))

;; MOP p. 70
(defun intern-eql-specializer (object)
  (let ((table (if (numberp object) *eql-specializer-table* *eq-specializer-table*)))
    (or (gethash object table)
        (setf (gethash object table)
              (make-instance-<eql-specializer> <eql-specializer>
                'singleton object)))))

;; MOP p. 52
(defun eql-specializer-object (specializer)
  (eql-specializer-singleton specializer))

;;; ===========================================================================

;; Converts a specializer to a pretty printing type.
(defun specializer-pretty (specializer)
  (if (eql-specializer-p specializer)
    `(EQL ,(eql-specializer-object specializer))
    specializer))
