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
;; (ALLOCATE-STD-INSTANCE class n) returns a non-funcallable CLOS-instance
;; with Class class and n-1 additional slots.
;; (ALLOCATE-FUNCALLABLE-INSTANCE class n) returns a funcallable CLOS-instance
;; with Class class and n-3 additional slots.

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
     (($classname          ; (class-name class) = (class-classname class),
                           ; a symbol
        :type symbol
        :initarg :name)
      ($direct-superclasses ; list of all direct superclasses (or their names,
                           ; while the class is waiting to be finalized)
        :type list
        :initarg :direct-superclasses)
      ($all-superclasses   ; hash table of all superclasses (including
                           ; the class itself)
        :type hash-table)
      ($precedence-list    ; ordered list of all superclasses (with the class
                           ; itself first), or NIL while the class is waiting
                           ; to be finalized
        :type list)
      ($direct-subclasses  ; set of all direct subclasses, as a weak-list or
                           ; weak-hash-table or NIL
        :type (or hash-table weak-list null)
        :initform nil)
      ($direct-slots       ; list of all freshly added slots (as
                           ; direct-slot-definition instances)
        :type list
        :initarg :direct-slots)
      ($slots              ; list of all slots (as effective-slot-definition
                           ; instances)
        :type list)
      ($slot-location-table ; hash table slotname -> descriptor
                           ; where the descriptor is either
                           ; - the location of the slot (a fixnum or cons), or
                           ; - its effective slot definition
        :type hash-table
        :initform empty-ht)
      ($direct-default-initargs ; freshly added default-initargs
                           ; (as alist initarg -> (form function))
        :type list
        :initarg :direct-default-initargs)
      ($default-initargs   ; default-initargs
                           ; (as alist initarg -> (form function))
        )
      ($documentation      ; string or NIL
        :type (or string null)
        :initarg :documentation)
      ($listeners          ; list of objects to be notified upon a change
        :type list
        :initform nil)
      ($initialized        ; describes which parts of the class are initialized
        :type (integer 0 6) ; 0 = nothing
                            ; 1 = name
                            ; 2 = likewise, plus direct-... info
                            ; 3 = likewise, plus class-precedence-list
                            ; 4 = likewise, plus class-all-superclasses
                            ; 5 = likewise, plus class-slots
                            ; 6 = likewise, plus slot-location-table, default-initargs
        :initform 0))
     (:fixed-slot-locations t)))

;; Fixed slot locations.
(defconstant *<class>-classname-location* 3)
(defconstant *<class>-direct-superclasses-location* 4)
(defconstant *<class>-all-superclasses-location* 5)
(defconstant *<class>-precedence-list-location* 6)
(defconstant *<class>-direct-subclasses-location* 7)
(defconstant *<class>-direct-slots-location* 8)
(defconstant *<class>-slots-location* 9)
(defconstant *<class>-slot-location-table-location* 10)
(defconstant *<class>-direct-default-initargs-location* 11)
(defconstant *<class>-default-initargs-location* 12)
(defconstant *<class>-documentation-location* 13)
(defconstant *<class>-listeners-location* 14)
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
(defun class-listeners (object)
  (sys::%record-ref object *<class>-listeners-location*))
(defun (setf class-listeners) (new-value object)
  (setf (sys::%record-ref object *<class>-listeners-location*) new-value))
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
      (setf (class-listeners class) nil)
      (setf (class-initialized class) 0)))
  (if (or (eq situation 't) name-p)
    (progn
      ; No need to check the name: any name is valid.
      (setf (class-classname class) name)
      (when (eq situation 't)
        (setf (class-initialized class) 1)))
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
          (if direct-slots-as-metaobjects-p
            direct-slots-as-metaobjects
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
  ; Now allow the user to call some class-xxx accessor functions.
  (when (eq situation 't)
    (setf (class-initialized class) 2))
  class)

;;; ===========================================================================

;;; The class <built-in-class> represents those classes for which the user
;;; cannot create subclasses.

(defvar <built-in-class> 'built-in-class)
(defvar *<built-in-class>-defclass*
  '(defclass built-in-class (class)
     ()
     (:fixed-slot-locations t)))
(defvar *<built-in-class>-class-version* (make-class-version))

(defconstant *<built-in-class>-instance-size* 16)

;;; ===========================================================================

;;; The class <slotted-class> represents those classes for which the local
;;; slot values are stored in the instance. It also represents common
;;; behaviour of <standard-class> and <structure-class>.

(defvar *<slotted-class>-defclass*
  '(defclass slotted-class (class)
     (($subclass-of-stablehash-p ; true if <standard-stablehash> or
                           ; <structure-stablehash> is among the superclasses
        :type boolean)
      ($generic-accessors  ; flag whether to create the accessors as methods;
                           ; if false, regular functions are used
        :initform t)
      ($direct-accessors   ; automatically generated accessor methods
                           ; (as plist)
        :type list
        :initform '())
      ($valid-initargs-from-slots ; list of valid initargs, computed from slots
        :type list)        ; (not including those declared valid by methods!)
      ($instance-size      ; number of local slots of the direct instances + 1
        :type (integer 1 *)))
     (:fixed-slot-locations t)))

;; Fixed slot locations.
(defconstant *<slotted-class>-subclass-of-stablehash-p-location* 16)
(defconstant *<slotted-class>-generic-accessors-location* 17)
(defconstant *<slotted-class>-direct-accessors-location* 18)
(defconstant *<slotted-class>-valid-initargs-from-slots-location* 19)
(defconstant *<slotted-class>-instance-size-location* 20)

;; Preliminary accessors.
(defun class-subclass-of-stablehash-p (object)
  (sys::%record-ref object *<slotted-class>-subclass-of-stablehash-p-location*))
(defun (setf class-subclass-of-stablehash-p) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-subclass-of-stablehash-p-location*) new-value))
(defun class-generic-accessors (object)
  (sys::%record-ref object *<slotted-class>-generic-accessors-location*))
(defun (setf class-generic-accessors) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-generic-accessors-location*) new-value))
(defun class-direct-accessors (object)
  (sys::%record-ref object *<slotted-class>-direct-accessors-location*))
(defun (setf class-direct-accessors) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-direct-accessors-location*) new-value))
(defun class-valid-initargs-from-slots (object)
  (sys::%record-ref object *<slotted-class>-valid-initargs-from-slots-location*))
(defun (setf class-valid-initargs-from-slots) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-valid-initargs-from-slots-location*) new-value))
(defun class-instance-size (object)
  (sys::%record-ref object *<slotted-class>-instance-size-location*))
(defun (setf class-instance-size) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-instance-size-location*) new-value))

;; Initialization of a <slotted-class> instance.
(defun shared-initialize-<slotted-class> (class situation &rest args
                                          &key (generic-accessors t generic-accessors-p)
                                          &allow-other-keys)
  (apply #'shared-initialize-<class> class situation args)
  (unless *classes-finished*
    ; Bootstrapping: Simulate the effect of #'%shared-initialize.
    (when (eq situation 't) ; called from initialize-instance?
      (setf (class-direct-accessors class) '())))
  (when (or (eq situation 't) generic-accessors-p)
    (setf (class-generic-accessors class) generic-accessors))
  ; The following slots are initialized by the subclass' shared-initialize:
  ;   subclass-of-stablehash-p
  ;   valid-initargs-from-slots
  ;   instance-size
  class)

;;; ===========================================================================

;;; The class <structure-class> represents classes like those defined through
;;; DEFSTRUCT.

(defvar <structure-class> 'structure-class)
(defvar *<structure-class>-defclass*
  '(defclass structure-class (slotted-class)
     (($names              ; encoding of the include-nesting, a list
                           ; (name_1 ... name_i-1 name_i) with name=name_1,
        :type cons)        ; name_1 contains name_2, ..., name_i-1 contains name_i.
      ($kconstructor       ; name of keyword constructor function
        :type symbol))
     (:fixed-slot-locations t)))
(defvar *<structure-class>-class-version* (make-class-version))

;; Fixed slot locations.
(defconstant *<structure-class>-names-location* 21)
(defconstant *<structure-class>-kconstructor-location* 22)

;; Preliminary accessors.
(defun class-names (object)
  (sys::%record-ref object *<structure-class>-names-location*))
(defun (setf class-names) (new-value object)
  (setf (sys::%record-ref object *<structure-class>-names-location*) new-value))
(defun class-kconstructor (object)
  (sys::%record-ref object *<structure-class>-kconstructor-location*))
(defun (setf class-kconstructor) (new-value object)
  (setf (sys::%record-ref object *<structure-class>-kconstructor-location*) new-value))

(defconstant *<structure-class>-instance-size* 23)

;;; ===========================================================================

;;; The class <semi-standard-class> is a common superclass of <standard-class>
;;; and <funcallable-standard-class>. Both implement the "default" CLOS
;;; behaviour.

(defvar <semi-standard-class> 'semi-standard-class)
(defvar *<semi-standard-class>-defclass*
  '(defclass semi-standard-class (slotted-class)
     (($current-version    ; most recent class-version, points back to this
                           ; class
        :type simple-vector)
      ($funcallablep       ; flag whether direct instances are funcallable
        :type boolean)
      ($fixed-slot-locations ; flag whether to guarantee same slot locations
                           ; in all subclasses
        :initarg :fixed-slot-locations
        )
      ($instantiated       ; true if an instance has already been created
        :type boolean
        :initform nil)
      ($finalized-direct-subclasses ; set of all finalized direct subclasses,
                           ; as a weak-list or weak-hash-table or NIL
        :type (or hash-table weak-list null)
        :initform '())
      ($prototype          ; class prototype - an instance or NIL
        :type (or standard-object null)))
     (:default-initargs :fixed-slot-locations nil)
     (:fixed-slot-locations t)))

;; Fixed slot locations.
(defconstant *<semi-standard-class>-current-version-location* 21)
(defconstant *<semi-standard-class>-funcallablep-location* 22)
(defconstant *<semi-standard-class>-fixed-slot-locations-location* 23)
(defconstant *<semi-standard-class>-instantiated-location* 24)
(defconstant *<semi-standard-class>-finalized-direct-subclasses-location* 25)
(defconstant *<semi-standard-class>-prototype-location* 26)

;; Preliminary accessors.
(defun class-current-version (object)
  (sys::%record-ref object *<semi-standard-class>-current-version-location*))
(defun (setf class-current-version) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-current-version-location*) new-value))
(defun class-funcallablep (object)
  (sys::%record-ref object *<semi-standard-class>-funcallablep-location*))
(defun (setf class-funcallablep) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-funcallablep-location*) new-value))
(defun class-fixed-slot-locations (object)
  (sys::%record-ref object *<semi-standard-class>-fixed-slot-locations-location*))
(defun (setf class-fixed-slot-locations) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-fixed-slot-locations-location*) new-value))
(defun class-instantiated (object)
  (sys::%record-ref object *<semi-standard-class>-instantiated-location*))
(defun (setf class-instantiated) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-instantiated-location*) new-value))
(defun class-finalized-direct-subclasses-table (object)
  (sys::%record-ref object *<semi-standard-class>-finalized-direct-subclasses-location*))
(defun (setf class-finalized-direct-subclasses-table) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-finalized-direct-subclasses-location*) new-value))
(defun class-prototype (object)
  (sys::%record-ref object *<semi-standard-class>-prototype-location*))
(defun (setf class-prototype) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-prototype-location*) new-value))

;;; ===========================================================================

;;; The class <standard-class> represents classes with the "default" CLOS
;;; behaviour.

(defvar <standard-class> 'standard-class)
(defvar *<standard-class>-defclass*
  '(defclass standard-class (semi-standard-class)
     ()
     (:fixed-slot-locations t)))
(defvar *<standard-class>-class-version* (make-class-version))

(defconstant *<standard-class>-instance-size* 27)

;; For DEFCLASS macro expansions.
(defconstant *<standard-class>-valid-initialization-keywords*
             '(:name :direct-superclasses :direct-slots :direct-default-initargs
               :documentation :generic-accessors :fixed-slot-locations))
(defconstant *<standard-class>-default-initargs* '(:fixed-slot-locations nil))

;;; ===========================================================================

;;; The classes <funcallable-standard-class> and <funcallable-standard-object>
;;; can be defined later.

(defvar <funcallable-standard-class> nil)
(defvar *<funcallable-standard-class>-class-version* nil)
(defvar <funcallable-standard-object> nil)

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

(defun semi-standard-class-p (object)
  (and (std-instance-p object)
       (let ((cv (sys::%record-ref object 0)))
         ; Treat the most frequent cases first, for speed and bootstrapping.
         (cond ((eq cv *<standard-class>-class-version*) t)
               ((eq cv *<structure-class>-class-version*) nil)
               ((eq cv *<built-in-class>-class-version*) nil)
               (t ; Now a slow, but general instanceof test.
                 (gethash <semi-standard-class>
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

(defun print-object-<class> (object stream)
  (if *print-readably*
    (write (sys::make-load-time-eval `(FIND-CLASS ',(class-classname object)))
           :stream stream)
    (print-unreadable-object (object stream :type t)
      (write (class-classname object) :stream stream)
      (when (semi-standard-class-p object)
        (if (and (slot-boundp object '$current-version)
                 (class-version-p (class-current-version object))
                 (slot-boundp object '$precedence-list))
          (progn
            (when (< (class-initialized object) 3) ; not yet finalized?
              (write-string " " stream)
              (write :incomplete :stream stream))
            ;; FIXME: Overhaul this questionable and confusing feature.
            (let ((serial (cv-serial (class-current-version object))))
              (unless (eql serial 0)
                (write-string " " stream)
                (write :version :stream stream)
                (write-string " " stream)
                (write serial :stream stream))))
          (progn
            (write-string " " stream)
            (write :uninitialized :stream stream)))))))

;; Preliminary.
;; Now we can at least print classes.
(defun print-object (object stream)
  (cond ((class-p object) (format stream "#<CLASS ~S>" (class-classname object)))
        (t (write-string "#<UNKNOWN>" stream))))
