;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; ============================================================================

(defparameter <funcallable-standard-class>
  (defclass funcallable-standard-class (semi-standard-class)
    ()
    (:fixed-slot-locations)))
(defparameter *<funcallable-standard-class>-class-version*
  (class-current-version <funcallable-standard-class>))

(defconstant *<funcallable-standard-class>-instance-size* 27)

(defun make-instance-<funcallable-standard-class> (metaclass &rest args
                                                   &key name
                                                        (direct-superclasses '())
                                                        (direct-slots '())
                                                        (direct-default-initargs '())
                                                   &allow-other-keys)
  ;; metaclass = <funcallable-standard-class>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore metaclass name direct-superclasses direct-slots
                   direct-default-initargs))
  (let ((class (allocate-metaobject-instance *<funcallable-standard-class>-class-version*
                                             *<funcallable-standard-class>-instance-size*)))
    (apply #'initialize-instance-<funcallable-standard-class> class args)))

(defun initialize-instance-<funcallable-standard-class> (class &rest args
                                                         &key &allow-other-keys)
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'initialize-instance later.
  (apply #'shared-initialize-<funcallable-standard-class> class 't args)
  (install-class-direct-accessors class)
  class)

(defun shared-initialize-<funcallable-standard-class> (class situation &rest args
                                                       &key (direct-superclasses '() direct-superclasses-p)
                                                            ((:direct-slots direct-slots-as-lists) '() direct-slots-as-lists-p)
                                                            ((direct-slots direct-slots-as-metaobjects) '() direct-slots-as-metaobjects-p)
                                                            (direct-default-initargs '() direct-default-initargs-p)
                                                            (documentation nil documentation-p)
                                                            (generic-accessors t generic-accessors-p)
                                                            (fixed-slot-locations nil fixed-slot-locations-p)
                                                       &allow-other-keys)
  (declare (ignore direct-superclasses direct-superclasses-p
                   direct-slots-as-lists direct-slots-as-lists-p
                   direct-slots-as-metaobjects direct-slots-as-metaobjects-p
                   direct-default-initargs direct-default-initargs-p
                   documentation documentation-p generic-accessors
                   generic-accessors-p fixed-slot-locations
                   fixed-slot-locations-p))
  (apply #'shared-initialize-<semi-standard-class> class situation args)
  class)

;; ----------------------------------------------------------------------------

;; Low-level representation of funcallable instances:
;; Funcallable instances are Closures with a certain bit set in the
;; closure_flags. They always the following shape:
;; - recdata[0] = clos_name_or_class_version is a semi-class-version,
;;   like for instances,
;; - recdata[1] = clos_codevec is a simple-8bit-vector, like for compiled
;;   functions,
;; - recdata[2] = clos_venv is reserved,
;; - recdata[3] is the first slot, the name,
;; - then come additional slots, as described by the class.

(defparameter <funcallable-standard-object>
  (let ((*allow-mixing-metaclasses* t))
    #|
    (defclass funcallable-standard-object (function standard-object)
      ;; The MOP p. 7 specifies a superclass list (standard-object function),
      ;; but then generic-function and standard-generic-function would have a
      ;; class-precedence-list that contains standard-object before function,
      ;; which contradicts the last sentence of ANSI CL 4.2.2. Possible
      ;; workarounds are: 1. reversed order (function standard-object),
      ;; 2. use a twin superclass or subclass of standard-object instead of
      ;; standard-object itself, 3. override compute-class-precedence-list for
      ;; this class. We choose solution 1 because it is the one a user will
      ;; most easily understand.
      (($name ; The function name is present as first CLOS slot. The macro
              ; Closure_name in lispbibl.d refers to it. Therefore this slot
              ; must not be changed after initialization, since this could
              ; interfere with the forwarded-instance mechanism.
         :accessor funcallable-name))
      (:metaclass funcallable-standard-class)
      (:fixed-slot-locations)
      (:generic-accessors nil))
    |#
    (ensure-class 'funcallable-standard-object
      :metaclass <funcallable-standard-class>
      :direct-superclasses `(,<function> ,<standard-object>)
      :direct-slots '((:name $name
                       :readers (funcallable-name)
                       :writers ((setf funcallable-name))))
      :direct-default-initargs '()
      :fixed-slot-locations t
      :generic-accessors nil)))

;; ============================================================================

;; low-level-representation:
;; Compiled functions (Cclosures), for which a certain bit is set in
;; the flag-byte of the code-vector. Additionally behind it:
;; - the signature, a signature struct (see compiler.lisp)
;; - the argument-precedence-order, as list of numbers from 0 to reqanz-1,
;; - the list of all methods.
;; - the method combination object

;; The compiler uses (at GENERIC-FLET, GENERIC-LABELS) and the evaluator
;; presupposes likewise, that a generic function does not change its
;; calling convention.
;; A generic function with signature (reqanz optanz restp keywords allowp)
;; is from the very beginning (!) a compiled function with
;;         reqanz  required parameters
;;         0       optional parameters
;;         &rest if and only if (or (> optanz 0) restp),
;;         without &key.
(defun callinfo (reqanz optanz restp keywords allowp)
  (declare (ignore keywords allowp))
  (list reqanz 0 (or (> optanz 0) restp) nil nil nil))

(defun gf-signature (gf)
  (sys::%record-ref gf 3))
(defun (setf gf-signature) (new gf)
  (setf (sys::%record-ref gf 3) new))

(defun gf-argorder (gf)
  (sys::%record-ref gf 4))
(defun (setf gf-argorder) (new gf)
  (setf (sys::%record-ref gf 4) new))

(defun gf-methods (gf)
  (sys::%record-ref gf 5))
(defun (setf gf-methods) (new gf)
  (setf (sys::%record-ref gf 5) new))

(defun gf-method-combination (gf)
  (sys::%record-ref gf 6))
(defun (setf gf-method-combination) (new gf)
  (setf (sys::%record-ref gf 6) new))
