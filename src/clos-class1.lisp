;;;; Common Lisp Object System for CLISP
;;;; Class metaobjects
;;;; Part 1: Class definitions, preliminary accessors.
;;;; Bruno Haible 2004-05-25

(in-package "CLOS")


;;; Low-level representation:

;; In the runtime-system, the type "CLOS instance" exists.
;; The first component is the class-version, the rest are the local slot
;; values.

;; Classes are instances of type CLASS,

;; The "value" of a slot that is unbound, is #<UNBOUND> - what else?

;;; see RECORD.D :
;; (STD-INSTANCE-P obj) tests, if an object is a CLOS-instance.
;; (ALLOCATE-STD-INSTANCE class n) returns a CLOS-instance with Class class
;; and n-1 additional slots.

;;; see IO.D :
;; CLOS-instances are printed via (PRINT-OBJECT object stream).

;; (CLASS-OF object) see PREDTYPE.D, uses property CLOSCLASS.

;;; ===========================================================================

;;; Auxiliary stuff.

;; An empty hash table.
(defconstant empty-ht
             (make-hash-table :key-type 'symbol :value-type 't
                              :test 'eq :warn-if-needs-rehash-after-gc t
                              :size 0))

;;; ===========================================================================

;;; The abstract class <class> allows built-in objects, user-defined objects
;;; and proxies to external worlds to be treated in a homogenous way.

(defvar *<class>-defclass*
  '(defclass class (specializer)
     ((classname           ; (class-name class) = (class-classname class),
                           ; a symbol
        :type symbol
        :initarg :name)
      (direct-superclasses ; list of all direct superclasses (or their names,
                           ; while the class is waiting to be finalized)
        :type list
        :initarg :direct-superclasses)
      (all-superclasses    ; hash table of all superclasses (including
                           ; the class itself)
        :type hash-table)
      (precedence-list     ; ordered list of all superclasses (with the class
                           ; itself first), or NIL while the class is waiting
                           ; to be finalized
        :type list)
      (direct-subclasses   ; set of all direct subclasses, as a weak-list or
                           ; weak-hash-table or NIL
        :type (or hash-table weak-list null)
        :initform nil)
      (direct-slots        ; list of all freshly added slots (as
                           ; direct-slot-definition instances)
        :type list
        :initarg :direct-slots)
      (slots               ; list of all slots (as effective-slot-definition
                           ; instances)
        :type list)
      (slot-location-table ; hash table slotname -> location of the slot
        :type hash-table
        :initform empty-ht)
      (direct-default-initargs ; freshly added default-initargs
                           ; (as alist initarg -> (form function))
        :type list
        :initarg :direct-default-initargs)
      (default-initargs    ; default-initargs
                           ; (as alist initarg -> (form function))
        )
      (documentation       ; string or NIL
        :type (or string null)
        :initarg :documentation)
      (initialized         ; set to true when the class is initialized
        :type boolean
        :initform nil))
     (:fixed-slot-locations)))

;; Fixed slot locations.
(defconstant *<class>-classname-location* 4)
(defconstant *<class>-direct-superclasses-location* 5)
(defconstant *<class>-all-superclasses-location* 6)
(defconstant *<class>-precedence-list-location* 7)
(defconstant *<class>-direct-subclasses-location* 8)
(defconstant *<class>-direct-slots-location* 9)
(defconstant *<class>-slots-location* 10)
(defconstant *<class>-slot-location-table-location* 11)
(defconstant *<class>-direct-default-initargs-location* 12)
(defconstant *<class>-default-initargs-location* 13)
(defconstant *<class>-documentation-location* 14)
(defconstant *<class>-initialized-location* 15)

;; Preliminary accessors.
(defun class-classname (object)
  (sys::%record-ref object *<class>-classname-location*))
(defun (setf class-classname) (new-value object)
  (setf (sys::%record-ref object *<class>-classname-location*) new-value))
(defun class-direct-superclasses (object)
  (sys::%record-ref object *<class>-direct-superclasses-location*))
(defun (setf class-direct-superclasses) (new-value object)
  (setf (sys::%record-ref object *<class>-direct-superclasses-location*) new-value))
(defun class-all-superclasses (object)
  (sys::%record-ref object *<class>-all-superclasses-location*))
(defun (setf class-all-superclasses) (new-value object)
  (setf (sys::%record-ref object *<class>-all-superclasses-location*) new-value))
(defun %class-precedence-list (object)
  (sys::%record-ref object *<class>-precedence-list-location*))
(defun class-precedence-list (object)
  (sys::%record-ref object *<class>-precedence-list-location*))
(defun (setf class-precedence-list) (new-value object)
  (setf (sys::%record-ref object *<class>-precedence-list-location*) new-value))
(defun class-direct-subclasses-table (object)
  (sys::%record-ref object *<class>-direct-subclasses-location*))
(defun (setf class-direct-subclasses-table) (new-value object)
  (setf (sys::%record-ref object *<class>-direct-subclasses-location*) new-value))
(defun class-direct-slots (object)
  (sys::%record-ref object *<class>-direct-slots-location*))
(defun (setf class-direct-slots) (new-value object)
  (setf (sys::%record-ref object *<class>-direct-slots-location*) new-value))
(defun class-slots (object)
  (sys::%record-ref object *<class>-slots-location*))
(defun (setf class-slots) (new-value object)
  (setf (sys::%record-ref object *<class>-slots-location*) new-value))
(defun class-slot-location-table (object)
  (sys::%record-ref object *<class>-slot-location-table-location*))
(defun (setf class-slot-location-table) (new-value object)
  (setf (sys::%record-ref object *<class>-slot-location-table-location*) new-value))
(defun class-direct-default-initargs (object)
  (sys::%record-ref object *<class>-direct-default-initargs-location*))
(defun (setf class-direct-default-initargs) (new-value object)
  (setf (sys::%record-ref object *<class>-direct-default-initargs-location*) new-value))
(defun class-default-initargs (object)
  (sys::%record-ref object *<class>-default-initargs-location*))
(defun (setf class-default-initargs) (new-value object)
  (setf (sys::%record-ref object *<class>-default-initargs-location*) new-value))
(defun class-documentation (object)
  (sys::%record-ref object *<class>-documentation-location*))
(defun (setf class-documentation) (new-value object)
  (setf (sys::%record-ref object *<class>-documentation-location*) new-value))
(defun class-initialized (object)
  (sys::%record-ref object *<class>-initialized-location*))
(defun (setf class-initialized) (new-value object)
  (setf (sys::%record-ref object *<class>-initialized-location*) new-value))

(defun canonicalized-slot-p (x)
  ; A "canonicalized slot specification" is a special kind of property list.
  ; See MOP p. 13-15.
  (and (proper-list-p x)
       (evenp (length x))
       (let ((default '#:default))
         (not (eq (getf x ':name default) default)))))

(defun canonicalized-default-initarg-p (x)
  ; A "canonicalized default initarg" is an element of an alist mapping
  ; a slot name (a symbol) to a list of the form (form function).
  ; See MOP p. 16.
  (and (consp x) (symbolp (first x))
       (consp (cdr x)) (consp (cddr x)) (functionp (third x))
       (null (cdddr x))))

;; Initialization of a <class> instance.
(defun shared-initialize-<class> (class situation &rest args
                                  &key (name nil name-p)
                                       (direct-superclasses nil direct-superclasses-p)
                                       ((:direct-slots direct-slots-as-lists) '() direct-slots-as-lists-p)
                                       ((direct-slots direct-slots-as-metaobjects) '() direct-slots-as-metaobjects-p)
                                       (direct-default-initargs nil direct-default-initargs-p)
                                       (documentation nil documentation-p)
                                  &allow-other-keys
                                  &aux old-direct-superclasses)
  (setq old-direct-superclasses
        (if (eq situation 't) ; called from initialize-instance?
          '()
          (sys::%record-ref class *<class>-direct-superclasses-location*)))
  (apply #'shared-initialize-<specializer> class situation args)
  (unless *classes-finished*
    ; Bootstrapping: Simulate the effect of #'%shared-initialize.
    (when (eq situation 't) ; called from initialize-instance?
      (setf (class-direct-subclasses-table class) nil)
      (setf (class-slot-location-table class) empty-ht)
      (setf (class-initialized class) nil)))
  (if (or (eq situation 't) name-p)
    ; No need to check the name: any name is valid.
    (setf (class-classname class) name)
    ; Get the name, for error message purposes.
    (setq name (class-classname class)))
  (when (or (eq situation 't) direct-superclasses-p)
    ; Check the direct-superclasses.
    (unless (proper-list-p direct-superclasses)
      (error (TEXT "(~S ~S) for class ~S: The ~S argument should be a proper list, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name ':direct-superclasses direct-superclasses))
    (unless (every #'(lambda (x) (or (class-p x) (symbolp x))) direct-superclasses)
      (error (TEXT "(~S ~S) for class ~S: The direct-superclasses list should consist of classes and symbols, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name direct-superclasses))
    (when (and (> (length direct-superclasses) 1)
               (typep class <structure-class>))
      (error (TEXT "(~S ~S) for class ~S: The metaclass ~S forbids more than one direct superclass: It does not support multiple inheritance.")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name (class-of class)))
    (dolist (sc direct-superclasses)
      (unless (symbolp sc)
        (check-allowed-superclass class sc)))
    (when (null direct-superclasses)
      (setq direct-superclasses (default-direct-superclasses class))))
  (when (or (eq situation 't) direct-slots-as-lists-p)
    ; Check the direct-slots.
    (unless (proper-list-p direct-slots-as-lists)
      (error (TEXT "(~S ~S) for class ~S: The ~S argument should be a proper list, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name ':direct-slots direct-slots-as-lists))
    (dolist (sl direct-slots-as-lists)
      (unless (canonicalized-slot-p sl)
        (error (TEXT "(~S ~S) for class ~S: The direct slot specification ~S is not in the canonicalized form (slot-name initform initfunction).")
               (if (eq situation 't) 'initialize-instance 'shared-initialize)
               'class name sl))))
  (when (or (eq situation 't) direct-default-initargs-p)
    ; Check the direct-default-initargs.
    (unless (proper-list-p direct-default-initargs)
      (error (TEXT "(~S ~S) for class ~S: The ~S argument should be a proper list, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name ':direct-default-initargs direct-default-initargs))
    (dolist (definitarg direct-default-initargs)
      (unless (canonicalized-default-initarg-p definitarg)
        (error (TEXT "(~S ~S) for class ~S: The direct default initarg ~S is not in canonicalized form (a property list).")
               (if (eq situation 't) 'initialize-instance 'shared-initialize)
               'class name definitarg))))
  (when (or (eq situation 't) documentation-p)
    ; Check the documentation.
    (unless (or (null documentation) (stringp documentation))
      (error (TEXT "(~S ~S) for class ~S: The ~S argument should be a string or NIL, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name :documentation documentation)))
  ; Fill the slots.
  (when (or (eq situation 't) direct-superclasses-p)
    (setf (class-direct-superclasses class) (copy-list direct-superclasses))
    (update-subclasses-sets class old-direct-superclasses direct-superclasses))
  (when (or (eq situation 't) direct-slots-as-lists-p direct-slots-as-metaobjects-p)
    (setf (class-direct-slots class)
          (append direct-slots-as-metaobjects
                  (convert-direct-slots class direct-slots-as-lists))))
  (when (or (eq situation 't) direct-default-initargs-p)
    (setf (class-direct-default-initargs class) direct-default-initargs))
  (when (or (eq situation 't) documentation-p)
    (setf (class-documentation class) documentation))
  ; The following slots are initialized by the subclass' shared-initialize:
  ;   all-superclasses
  ;   precedence-list
  ;   slots
  ;   slot-location-table
  ;   default-initargs
  ; Now allow the user to call the class-xxx accessor functions.
  (setf (class-initialized class) t)
  class)

;;; ===========================================================================

;;; The class <built-in-class> represents those classes for which the user
;;; cannot create subclasses.

(defvar <built-in-class> 'built-in-class)
(defvar *<built-in-class>-defclass*
  '(defclass built-in-class (class)
     ()
     (:fixed-slot-locations)))
(defvar *<built-in-class>-class-version* (make-class-version))

(defconstant *<built-in-class>-instance-size* 16)

;;; ===========================================================================

;;; The class <slotted-class> represents those classes for which the local
;;; slot values are stored in the instance. It also represents common
;;; behaviour of <standard-class> and <structure-class>.

(defvar *<slotted-class>-defclass*
  '(defclass slotted-class (class)
     ((subclass-of-stablehash-p ; true if <standard-stablehash> or
                           ; <structure-stablehash> is among the superclasses
        :type boolean)
      (valid-initargs      ; list of valid initargs
        :type list)
      (instance-size       ; number of local slots of the direct instances + 1
        :type (integer 1 *)))
     (:fixed-slot-locations)))

;; Fixed slot locations.
(defconstant *<slotted-class>-subclass-of-stablehash-p-location* 16)
(defconstant *<slotted-class>-valid-initargs-location* 17)
(defconstant *<slotted-class>-instance-size-location* 18)

;; Preliminary accessors.
(defun class-subclass-of-stablehash-p (object)
  (sys::%record-ref object *<slotted-class>-subclass-of-stablehash-p-location*))
(defun (setf class-subclass-of-stablehash-p) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-subclass-of-stablehash-p-location*) new-value))
(defun class-valid-initargs (object)
  (sys::%record-ref object *<slotted-class>-valid-initargs-location*))
(defun (setf class-valid-initargs) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-valid-initargs-location*) new-value))
(defun class-instance-size (object)
  (sys::%record-ref object *<slotted-class>-instance-size-location*))
(defun (setf class-instance-size) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-instance-size-location*) new-value))

;; Initialization of a <slotted-class> instance.
(defun shared-initialize-<slotted-class> (class situation &rest args
                                          &key &allow-other-keys)
  (apply #'shared-initialize-<class> class situation args)
  ; The following slots are initialized by the subclass' shared-initialize:
  ;   subclass-of-stablehash-p
  ;   valid-initargs
  ;   instance-size
  class)

;;; ===========================================================================

;;; The class <structure-class> represents classes like those defined through
;;; DEFSTRUCT.

(defvar <structure-class> 'structure-class)
(defvar *<structure-class>-defclass*
  '(defclass structure-class (slotted-class)
     ((names               ; encoding of the include-nesting, a list
        :type cons))
     (:fixed-slot-locations)))
(defvar *<structure-class>-class-version* (make-class-version))

;; Fixed slot locations.
(defconstant *<structure-class>-names-location* 19)

;; Preliminary accessors.
(defun class-names (object)
  (sys::%record-ref object *<structure-class>-names-location*))
(defun (setf class-names) (new-value object)
  (setf (sys::%record-ref object *<structure-class>-names-location*) new-value))

(defconstant *<structure-class>-instance-size* 20)

;;; ===========================================================================

;;; The class <standard-class> represents classes with the "default" CLOS
;;; behaviour.

(defvar <standard-class> 'standard-class)
(defvar *<standard-class>-defclass*
  '(defclass standard-class (slotted-class)
     ((current-version     ; most recent class-version, points back to this
                           ; class
        :type simple-vector)
      (direct-accessors    ; automatically generated accessor methods
                           ; (as plist)
        :type list
        :initform '())
      (fixed-slot-locations ; flag whether to guarantee same slot locations
                           ; in all subclasses
        )
      (instantiated        ; true if an instance has already been created
        :type boolean
        :initform nil)
      (finalized-direct-subclasses ; set of all finalized direct subclasses,
                           ; as a weak-list or weak-hash-table or NIL
        :type (or hash-table weak-list null)
        :initform '())
      (prototype           ; class prototype - an instance or NIL
        :type (or standard-object null)))
     (:fixed-slot-locations)))
(defvar *<standard-class>-class-version* (make-class-version))

;; Fixed slot locations.
(defconstant *<standard-class>-current-version-location* 19)
(defconstant *<standard-class>-direct-accessors-location* 20)
(defconstant *<standard-class>-fixed-slot-locations-location* 21)
(defconstant *<standard-class>-instantiated-location* 22)
(defconstant *<standard-class>-finalized-direct-subclasses-location* 23)
(defconstant *<standard-class>-prototype-location* 24)

;; Preliminary accessors.
(defun class-current-version (object)
  (sys::%record-ref object *<standard-class>-current-version-location*))
(defun (setf class-current-version) (new-value object)
  (setf (sys::%record-ref object *<standard-class>-current-version-location*) new-value))
(defun class-direct-accessors (object)
  (sys::%record-ref object *<standard-class>-direct-accessors-location*))
(defun (setf class-direct-accessors) (new-value object)
  (setf (sys::%record-ref object *<standard-class>-direct-accessors-location*) new-value))
(defun class-fixed-slot-locations (object)
  (sys::%record-ref object *<standard-class>-fixed-slot-locations-location*))
(defun (setf class-fixed-slot-locations) (new-value object)
  (setf (sys::%record-ref object *<standard-class>-fixed-slot-locations-location*) new-value))
(defun class-instantiated (object)
  (sys::%record-ref object *<standard-class>-instantiated-location*))
(defun (setf class-instantiated) (new-value object)
  (setf (sys::%record-ref object *<standard-class>-instantiated-location*) new-value))
(defun class-finalized-direct-subclasses-table (object)
  (sys::%record-ref object *<standard-class>-finalized-direct-subclasses-location*))
(defun (setf class-finalized-direct-subclasses-table) (new-value object)
  (setf (sys::%record-ref object *<standard-class>-finalized-direct-subclasses-location*) new-value))
(defun class-prototype (object)
  (sys::%record-ref object *<standard-class>-prototype-location*))
(defun (setf class-prototype) (new-value object)
  (setf (sys::%record-ref object *<standard-class>-prototype-location*) new-value))

(defconstant *<standard-class>-instance-size* 25)

;;; ===========================================================================

;;; Type tests.

(defun built-in-class-p (object)
  (and (std-instance-p object)
       (let ((cv (sys::%record-ref object 0)))
         ; Treat the most frequent cases first, for speed and bootstrapping.
         (cond ((eq cv *<standard-class>-class-version*) nil)
               ((eq cv *<structure-class>-class-version*) nil)
               ((eq cv *<built-in-class>-class-version*) t)
               (t ; Now a slow, but general instanceof test.
                 (gethash <built-in-class>
                          (class-all-superclasses (class-of object))))))))

(defun structure-class-p (object)
  (and (std-instance-p object)
       (let ((cv (sys::%record-ref object 0)))
         ; Treat the most frequent cases first, for speed and bootstrapping.
         (cond ((eq cv *<standard-class>-class-version*) nil)
               ((eq cv *<structure-class>-class-version*) t)
               ((eq cv *<built-in-class>-class-version*) nil)
               (t ; Now a slow, but general instanceof test.
                 (gethash <structure-class>
                          (class-all-superclasses (class-of object))))))))

(defun standard-class-p (object)
  (and (std-instance-p object)
       (let ((cv (sys::%record-ref object 0)))
         ; Treat the most frequent cases first, for speed and bootstrapping.
         (cond ((eq cv *<standard-class>-class-version*) t)
               ((eq cv *<structure-class>-class-version*) nil)
               ((eq cv *<built-in-class>-class-version*) nil)
               (t ; Now a slow, but general instanceof test.
                 (gethash <standard-class>
                          (class-all-superclasses (class-of object))))))))

(sys::def-atomic-type class class-p)
(sys::def-atomic-type built-in-class built-in-class-p)
(sys::def-atomic-type structure-class structure-class-p)
(sys::def-atomic-type standard-class standard-class-p)

;;; ===========================================================================

;;; Copying.
(defun copy-standard-class (class)
  (let* ((n (sys::%record-length class))
         (copy (allocate-metaobject-instance (sys::%record-ref class 0) n)))
    (dotimes (i n) (setf (sys::%record-ref copy i) (sys::%record-ref class i)))
    copy))

;; Preliminary.
;; Now we can at least print classes.
(defun print-object (object stream)
  (cond ((class-p object) (format stream "#<CLASS ~S>" (class-classname object)))
        (t (write-string "#<UNKNOWN>" stream))))
