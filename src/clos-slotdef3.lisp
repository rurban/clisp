;;;; Common Lisp Object System for CLISP
;;;; Slot Definition metaobjects
;;;; Part n-1: Generic functions specified in the MOP.
;;;; Bruno Haible 2004-04-18

(in-package "CLOS")


#| ;;; Unoptimized slot-definition-xxx accessors.

;; MOP p. 84
(fmakunbound 'slot-definition-name)
(defgeneric slot-definition-name (slotdef)
  (:method ((slotdef slot-definition))
    (slot-value slotdef 'name)))
(defun (setf slot-definition-name) (new-value slotdef)
  (setf (slot-value slotdef 'name) new-value))

(defun slot-definition-inheritable-initer (slotdef)
  (slot-value slotdef 'inheritable-initer))

;; MOP p. 84
(fmakunbound 'slot-definition-initform)
(defgeneric slot-definition-initform (slotdef)
  (:method ((slotdef slot-definition))
    (inheritable-slot-definition-initform (slot-value slotdef 'inheritable-initer))))
(defun (setf slot-definition-initform) (new-value slotdef)
  (setf (inheritable-slot-definition-initform (slot-value slotdef 'inheritable-initer)) new-value))

;; MOP p. 84
(fmakunbound 'slot-definition-initfunction)
(defgeneric slot-definition-initfunction (slotdef)
  (:method ((slotdef slot-definition))
    (inheritable-slot-definition-initfunction (slot-value slotdef 'inheritable-initer))))
(defun (setf slot-definition-initfunction) (new-value slotdef)
  (setf (inheritable-slot-definition-initfunction (slot-value slotdef 'inheritable-initer)) new-value))

;; MOP p. 84
(fmakunbound 'slot-definition-initargs)
(defgeneric slot-definition-initargs (slotdef)
  (:method ((slotdef slot-definition))
    (slot-value slotdef 'initargs)))
(defun (setf slot-definition-initargs) (new-value slotdef)
  (setf (slot-value slotdef 'initargs) new-value))

;; MOP p. 85
(fmakunbound 'slot-definition-type)
(defgeneric slot-definition-type (slotdef)
  (:method ((slotdef slot-definition))
    (slot-value slotdef 'type)))
(defun (setf slot-definition-type) (new-value slotdef)
  (setf (slot-value slotdef 'type) new-value))

;; MOP p. 84
(fmakunbound 'slot-definition-allocation)
(defgeneric slot-definition-allocation (slotdef)
  (:method ((slotdef slot-definition))
    (slot-value slotdef 'allocation)))
(defun (setf slot-definition-allocation) (new-value slotdef)
  (setf (slot-value slotdef 'allocation) new-value))

(defun slot-definition-inheritable-doc (slotdef)
  (slot-value slotdef 'inheritable-doc))

(defun slot-definition-documentation (slotdef)
  (inheritable-slot-definition-documentation (slot-value slotdef 'inheritable-doc)))
(defun (setf slot-definition-documentation) (new-value slotdef)
  (setf (inheritable-slot-definition-documentation (slot-value slotdef 'inheritable-doc)) new-value))

;; MOP p. 85
(fmakunbound 'slot-definition-readers)
(defgeneric slot-definition-readers (slotdef)
  (:method ((slotdef direct-slot-definition))
    (slot-value slotdef 'readers)))
(defun (setf slot-definition-readers) (new-value slotdef)
  (setf (slot-value slotdef 'readers) new-value))

;; MOP p. 85
(fmakunbound 'slot-definition-writers)
(defgeneric slot-definition-writers (slotdef)
  (:method ((slotdef direct-slot-definition))
    (slot-value slotdef 'writers)))
(defun (setf slot-definition-writers) (new-value slotdef)
  (setf (slot-value slotdef 'writers) new-value))

;; MOP p. 86
(fmakunbound 'slot-definition-location)
(defgeneric slot-definition-location (slotdef)
  (:method ((slotdef effective-slot-definition))
    (slot-value slotdef 'location)))
(defun (setf slot-definition-location) (new-value slotdef)
  (setf (slot-value slotdef 'location) new-value))

|#

;;; Optimized slot-definition-xxx accessors.
;;; These are possible thanks to the :fixed-slot-locations class option.

;; MOP p. 84
(fmakunbound 'slot-definition-name)
(defgeneric slot-definition-name (slotdef)
  (:method ((slotdef slot-definition))
    (sys::%record-ref slotdef 1)))

;; MOP p. 84
(fmakunbound 'slot-definition-initform)
(defgeneric slot-definition-initform (slotdef)
  (:method ((slotdef slot-definition))
    (inheritable-slot-definition-initform (sys::%record-ref slotdef 5))))

;; MOP p. 84
(fmakunbound 'slot-definition-initfunction)
(defgeneric slot-definition-initfunction (slotdef)
  (:method ((slotdef slot-definition))
    (inheritable-slot-definition-initfunction (sys::%record-ref slotdef 5))))

;; MOP p. 84
(fmakunbound 'slot-definition-initargs)
(defgeneric slot-definition-initargs (slotdef)
  (:method ((slotdef slot-definition))
    (sys::%record-ref slotdef 2)))

;; MOP p. 85
(fmakunbound 'slot-definition-type)
(defgeneric slot-definition-type (slotdef)
  (:method ((slotdef slot-definition))
    (sys::%record-ref slotdef 3)))

;; MOP p. 84
(fmakunbound 'slot-definition-allocation)
(defgeneric slot-definition-allocation (slotdef)
  (:method ((slotdef slot-definition))
    (sys::%record-ref slotdef 4)))

;; MOP p. 85
(fmakunbound 'slot-definition-readers)
(defgeneric slot-definition-readers (slotdef)
  (:method ((slotdef direct-slot-definition))
    (sys::%record-ref slotdef 7)))

;; MOP p. 85
(fmakunbound 'slot-definition-writers)
(defgeneric slot-definition-writers (slotdef)
  (:method ((slotdef direct-slot-definition))
    (sys::%record-ref slotdef 8)))

;; MOP p. 86
(fmakunbound 'slot-definition-location)
(defgeneric slot-definition-location (slotdef)
  (:method ((slotdef effective-slot-definition))
    (sys::%record-ref slotdef 7)))


;; MOP p. 45
(fmakunbound 'direct-slot-definition-class)
(defgeneric direct-slot-definition-class (class &rest initargs)
  (:method ((class standard-class) &rest initargs)
    (declare (ignore initargs))
    <standard-direct-slot-definition>)
  (:method ((class structure-class) &rest initargs)
    (declare (ignore initargs))
    <structure-direct-slot-definition>))

;; MOP p. 45
(fmakunbound 'effective-slot-definition-class)
(defgeneric effective-slot-definition-class (class &rest initargs)
  (:method ((class standard-class) &rest initargs)
    (declare (ignore initargs))
    <standard-effective-slot-definition>)
  #|
  (:method ((class funcallable-standard-class) &rest initargs)
    (declare (ignore initargs))
    <standard-effective-slot-definition>)
  |#
  (:method ((class structure-class) &rest initargs)
    (declare (ignore initargs))
    <structure-effective-slot-definition>))

;; Customizable function used to compare two slots of given objects belonging
;; to the same class.
;; Arguments: class is a class,
;;            (class-of object1) = class,
;;            (class-of object2) = class,
;;            slot is a slot of class,
;;            value1 = (slot-value object1 (slot-definition-name slot)),
;;            value2 = (slot-value object2 (slot-definition-name slot)).
(defgeneric slot-equal-using-class (class object1 object2 slot value1 value2)
  (:method ((class (eql <standard-direct-slot-definition>)) (object1 standard-direct-slot-definition) (object2 standard-direct-slot-definition) slot value1 value2)
    (declare (ignore object1 object2 slot))
    (equal value1 value2)))

;; Test two direct slots for equality, except for the inheritable slots,
;; where only the presence is compared.
(defun equal-direct-slot (slot1 slot2 &aux slot-class)
  (and (eq (setq slot-class (class-of slot1)) (class-of slot2))
       (eq (slot-definition-name slot1) (slot-definition-name slot2))
       (eq (null (slot-definition-initfunction slot1)) (null (slot-definition-initfunction slot2)))
       (eq (null (slot-definition-documentation slot1)) (null (slot-definition-documentation slot2)))
       ;; The MOP doesn't specify an equality method that the user could define,
       ;; therefore we use the generic "compare all slots" approach.
       (dolist (s (class-slots slot-class) t)
         (let ((n (slot-definition-name s)))
           (unless (memq n '(:initform :initfunction :documentation inheritable-initer inheritable-doc))
             (let ((unboundp1 (not (slot-boundp slot1 n)))
                   (unboundp2 (not (slot-boundp slot2 n))))
               (unless (and (eq unboundp1 unboundp2)
                            (or unboundp1
                                (slot-equal-using-class slot-class slot1 slot2 s
                                                        (slot-value slot1 n)
                                                        (slot-value slot2 n))))
                 (return nil))))))))

#|
;; Tell the compiler how to serialize <structure-effective-slot-definition>
;; instances. This is needed for DEFSTRUCT.
(defmethod make-load-form ((object structure-effective-slot-definition) &optional environment)
  (declare (ignore environment))
  (make-load-form-<structure-effective-slot-definition> object))
|#
