;;;; Common Lisp Object System for CLISP: Classes
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


(defgeneric class-name (class)
  (:method ((class class))
    (class-classname class)))

(defgeneric (setf class-name) (new-value class)
  (:method (new-value (class class))
    (unless (symbolp new-value)
      (error-of-type 'type-error
        :datum new-value :expected-type 'symbol
        (TEXT "~S: The name of a class must be a symbol, not ~S")
        '(setf class-name) new-value))
    (when (built-in-class-p class)
      (error-of-type 'error
        (TEXT "~S: The name of the built-in class ~S cannot be modified")
        '(setf class-name) class))
    (setf (class-classname class) new-value)))


;;; CLtL2 28.1.9., ANSI CL 7.1. Object Creation and Initialization

;; Cruel hack (CLtL2 28.1.9.2., ANSI CL 7.1.2.):
;; - MAKE-INSTANCE must be informed about the methods of ALLOCATE-INSTANCE,
;;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - INITIALIZE-INSTANCE must be informed about the methods of
;;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - REINITIALIZE-INSTANCE must be informed about the methods of
;;   REINITIALIZE-INSTANCE and SHARED-INITIALIZE.
;; - UPDATE-INSTANCE-FOR-REDEFINED-CLASS must be informed about the methods of
;;   UPDATE-INSTANCE-FOR-REDEFINED-CLASS and SHARED-INITIALIZE.
;; - UPDATE-INSTANCE-FOR-DIFFERENT-CLASS must be informed about the methods of
;;   UPDATE-INSTANCE-FOR-DIFFERENT-CLASS and SHARED-INITIALIZE.

(defparameter *make-instance-table*
  (make-hash-table :key-type 'class :value-type '(simple-vector 4)
                   :test 'ext:stablehash-eq :warn-if-needs-rehash-after-gc t))
  ;; Hash table, mapping a class to a simple-vector containing
  ;; - a list of valid keyword arguments,
  ;; - the effective method of allocate-instance,
  ;; - the effective method of initialize-instance,
  ;; - the effective method of shared-initialize.

(defparameter *reinitialize-instance-table*
  (make-hash-table :key-type 'class :value-type 'cons
                   :test 'ext:stablehash-eq :warn-if-needs-rehash-after-gc t))
  ;; Hash table, mapping a class to a cons containing
  ;; - a list of valid keyword arguments,
  ;; - the effective method of shared-initialize.

(defparameter *update-instance-for-redefined-class-table*
  (make-hash-table :key-type 'class :value-type 'list
                   :test 'ext:stablehash-eq :warn-if-needs-rehash-after-gc t))
  ;; Hash table, mapping a class to
  ;; - a list of valid keyword arguments.

(defparameter *update-instance-for-different-class-table*
  (make-hash-table :key-type '(cons class class) :value-type 'list
                   :test 'ext:stablehash-equal :warn-if-needs-rehash-after-gc t))
  ;; Hash table, mapping a cons (old-class . new-class) to
  ;; - a list of valid keyword arguments.

;; FIXME: These tables must also be cleaned when a classed is redefined
;; (because subclassp changes, and for *update-instance-for-redefined-class-table*
;; also because the class-slots change).

(defun note-i-change (specializer table)
  (maphash #'(lambda (class value) (declare (ignore value))
               (when (subclassp class specializer)
                 (remhash class table)))
           table))
(defun note-i-meta-change (meta-specializer table)
  (maphash #'(lambda (class value) (declare (ignore value))
               (when (typep-class class meta-specializer) ; <==> (typep class meta-specializer)
                 (remhash class table)))
           table))

(defun note-ai-change (method)
  (let ((specializer (first (std-method-parameter-specializers method))))
    (if (consp specializer)
      ;; EQL-method for ALLOCATE-INSTANCE:
      ;; object must be a class, else worthless.
      (let ((specialized-object (second specializer)))
        (when (class-p specialized-object)
          ;; Remove the entries from *make-instance-table* for which the
          ;; implied method might be applicable:
          (note-i-change specialized-object *make-instance-table*)))
      ;; remove the entries from *make-instance-table* for which the
      ;; implied method might be applicable:
      (note-i-meta-change specializer *make-instance-table*))))

(defun note-ii-change (method)
  (let ((specializer (first (std-method-parameter-specializers method))))
    ;; EQL-methods for INITIALIZE-INSTANCE are worthless in any case.
    (unless (consp specializer)
      ;; Remove the entries from *make-instance-table* for which the
      ;; implied method might be applicable:
      (note-i-change specializer *make-instance-table*))))

(defun note-ri-change (method)
  (let ((specializer (first (std-method-parameter-specializers method))))
    ;; EQL-methods for REINITIALIZE-INSTANCE are essentially worthless.
    (unless (consp specializer)
      ;; Remove the entries from *reinitialize-instance-table* for which the
      ;; implied method might be applicable:
      (note-i-change specializer *reinitialize-instance-table*))))

(defun note-uirc-change (method)
  (let ((specializer (first (std-method-parameter-specializers method))))
    ;; EQL-methods for UPDATE-INSTANCE-FOR-REDEFINED-CLASS are essentially
    ;; worthless.
    (unless (consp specializer)
      ;; Remove the entries from *update-instance-for-redefined-class-table*
      ;; for which the implied method might be applicable:
      (note-i-change specializer *update-instance-for-redefined-class-table*))))

(defun note-uidc-change (method)
  (let ((specializer1 (first (std-method-parameter-specializers method)))
        (specializer2 (second (std-method-parameter-specializers method))))
    ;; Methods for UPDATE-INSTANCE-FOR-DIFFERENT-CLASS with EQL specializer
    ;; in the first argument are essentially worthless.
    (unless (consp specializer1)
      ;; Methods for UPDATE-INSTANCE-FOR-DIFFERENT-CLASS with EQL specializer
      ;; in the second argument are worthless in any case.
      (unless (consp specializer2)
        ;; Remove the entries from *update-instance-for-different-class-table*
        ;; for which the implied method might be applicable:
        (let ((table *update-instance-for-different-class-table*))
          (maphash #'(lambda (classes value) (declare (ignore value))
                       (let ((class1 (car classes))
                             (class2 (cdr classes)))
                         (when (and (subclassp class1 specializer1)
                                    (subclassp class2 specializer2))
                           (remhash classes table))))
                   table))))))

(defun note-si-change (method)
  (let* ((specializers (std-method-parameter-specializers method))
         (specializer1 (first specializers))
         (specializer2 (second specializers)))
    ;; EQL-methods for SHARED-INITIALIZE are essentially worthless.
    (unless (consp specializer1)
      ;; As second argument, INITIALIZE-INSTANCE passes always T .
      (when (typep 'T specializer2)
        ;; Remove the entries from *make-instance-table* for which the
        ;; implied method might be applicable:
        (note-i-change specializer1 *make-instance-table*))
      ;; As second argument, REINITIALIZE-INSTANCE passes always NIL .
      (when (typep 'NIL specializer2)
        ;; Remove the entries from *reinitialize-instance-table* for which the
        ;; implied method might be applicable:
        (note-i-change specializer1 *reinitialize-instance-table*))
      ;; Remove the entries from *update-instance-for-redefined-class-table*
      ;; for which the implied method might be applicable:
      (note-i-change specializer1 *reinitialize-instance-table*)
      ;; Remove the entries from *update-instance-for-different-class-table*
      ;; for which the implied method might be applicable:
      (let ((table *update-instance-for-different-class-table*))
        (maphash #'(lambda (classes value) (declare (ignore value))
                     (let ((class2 (cdr classes)))
                       (when (subclassp class2 specializer1)
                         (remhash classes table))))
                 table)))))

;;; collect all keywords from a list of applicable methods
(defun valid-initarg-keywords (class methods)
  (let ((signatures (mapcar #'std-method-signature methods)))
    ;; "A method that has &rest but not &key does not affect the set of
    ;;  acceptable keyword srguments."
    (setq signatures (delete-if-not #'sig-keys-p signatures))
    ;; "The presence of &allow-other-keys in the lambda list of an applicable
    ;;  method disables validity checking of initialization arguments."
    ;; (ANSI CL section 7.1.2)
    (if (some #'sig-allow-p signatures)
      't
      ;; "The keyword name of each keyword parameter specified in the method's
      ;;  lambda-list becomes an initialization argument for all classes for
      ;;  which the method is applicable."
      (remove-duplicates
       (append (class-valid-initargs class) (mapcap #'sig-keywords signatures))
       :from-end t))))

;; NB: On calculation of an effective method, the residual
;; arguments do not count.
;; At the first call of INITIALIZE-INSTANCE or MAKE-INSTANCE of each class
;; we memorize the needed information in *make-instance-table*.

;; For MAKE-INSTANCE the following is necessary as keys:
;; - the initargs that are used for the initialization of slots,
;; - the keywords of methods from SHARED-INITIALIZE,
;; - the keywords of methods from INITIALIZE-INSTANCE,
;; - the keywords of methods from ALLOCATE-INSTANCE.
(defun valid-make-instance-keywords (class)
  (valid-initarg-keywords
    class
    (append
     ;; list of all applicable methods from SHARED-INITIALIZE
     (remove-if-not
      #'(lambda (method)
          (let* ((specializers (std-method-parameter-specializers method))
                 (specializer1 (first specializers))
                 (specializer2 (second specializers)))
            (and (atom specializer1) (subclassp class specializer1)
                 (typep 'T specializer2))))
      (the list (gf-methods |#'shared-initialize|)))
     ;; list of all applicable methods from INITIALIZE-INSTANCE
     (remove-if-not
      #'(lambda (method)
          (let ((specializer
                 (first (std-method-parameter-specializers method))))
            (and (atom specializer) (subclassp class specializer))))
      (the list (gf-methods |#'initialize-instance|)))
     ;; list of all applicable methods from ALLOCATE-INSTANCE
     (remove-if-not
      #'(lambda (method)
          (let ((specializer
                 (first (std-method-parameter-specializers method))))
            (if (consp specializer)
              (eql class (second specializer))
              (typep-class class specializer)))) ; <==> (typep class specializer)
      (the list (gf-methods |#'allocate-instance|))))))
(defun make-instance-table-entry1 (class)
  (values (valid-make-instance-keywords class)
          (compute-applicable-methods-effective-method |#'allocate-instance| class)))
(defun make-instance-table-entry2 (instance)
  (values (compute-applicable-methods-effective-method |#'initialize-instance| instance)
          (compute-applicable-methods-effective-method |#'shared-initialize| instance 'T)))

;; For REINITIALIZE-INSTANCE the following is necessary as keys:
;; - the initargs that are used for the initialization of slots,
;; - the keywords of methods from SHARED-INITIALIZE,
;; - the keywords of methods from REINITIALIZE-INSTANCE.
(defun valid-reinitialize-instance-keywords (class)
  (valid-initarg-keywords
    class
    (append
      ;; list of all applicable methods from SHARED-INITIALIZE
      (remove-if-not
        #'(lambda (method)
            (let* ((specializers (std-method-parameter-specializers method))
                   (specializer1 (first specializers))
                   (specializer2 (second specializers)))
              (and (atom specializer1) (subclassp class specializer1)
                   (typep 'NIL specializer2))))
        (the list (gf-methods |#'shared-initialize|)))
      ;; list of all applicable methods from REINITIALIZE-INSTANCE
      (remove-if-not
        #'(lambda (method)
            (let ((specializer
                    (first (std-method-parameter-specializers method))))
              (and (atom specializer) (subclassp class specializer))))
        (the list (gf-methods |#'reinitialize-instance|))))))

;; For UPDATE-INSTANCE-FOR-REDEFINED-CLASS the following is necessary as keys:
;; - the initargs that are used for the initialization of slots,
;; - the keywords of methods from SHARED-INITIALIZE,
;; - the keywords of methods from UPDATE-INSTANCE-FOR-REDEFINED-CLASS.
;; Return a 2nd value that indicates whether the result is independent of
;; added-slots, discarded-slots, property-list.
(defun valid-update-instance-for-redefined-class-keywords (class added-slots discarded-slots property-list)
  (let ((independent t))
    (values
      (valid-initarg-keywords
        class
        (append
          ;; list of all applicable methods from SHARED-INITIALIZE
          (remove-if-not
            #'(lambda (method)
                (let* ((specializers (std-method-parameter-specializers method))
                       (specializer1 (first specializers))
                       (specializer2 (second specializers)))
                  (and (atom specializer1) (subclassp class specializer1)
                       (progn
                         (when (or (consp specializer2) (eq specializer2 <null>))
                           (setq independent nil))
                         (typep added-slots specializer2)))))
            (the list (gf-methods |#'shared-initialize|)))
          ;; list of all applicable methods from UPDATE-INSTANCE-FOR-REDEFINED-CLASS
          (remove-if-not
            #'(lambda (method)
                (let* ((specializers (std-method-parameter-specializers method))
                       (specializer1 (first specializers))
                       (specializer2 (second specializers))
                       (specializer3 (third specializers))
                       (specializer4 (fourth specializers)))
                  (and (atom specializer1) (subclassp class specializer1)
                       (progn
                         (when (or (consp specializer2) (eq specializer2 <null>))
                           (setq independent nil))
                         (when (or (consp specializer3) (eq specializer3 <null>))
                           (setq independent nil))
                         (when (or (consp specializer4) (eq specializer4 <null>))
                           (setq independent nil))
                         (and (typep added-slots specializer2)
                              (typep discarded-slots specializer3)
                              (typep property-list specializer4))))))
            (the list (gf-methods |#'update-instance-for-redefined-class|)))))
      independent)))

;; For UPDATE-INSTANCE-FOR-DIFFERENT-CLASS the following is necessary as keys:
;; - the initargs that are used for the initialization of slots,
;; - the keywords of methods from SHARED-INITIALIZE,
;; - the keywords of methods from UPDATE-INSTANCE-FOR-DIFFERENT-CLASS.
(defun valid-update-instance-for-different-class-keywords (old-class new-class added-slots)
  (valid-initarg-keywords
    new-class
    (append
      ;; list of all applicable methods from SHARED-INITIALIZE
      (remove-if-not
        #'(lambda (method)
            (let* ((specializers (std-method-parameter-specializers method))
                   (specializer1 (first specializers))
                   (specializer2 (second specializers)))
              (and (atom specializer1) (subclassp new-class specializer1)
                   (typep added-slots specializer2))))
        (the list (gf-methods |#'shared-initialize|)))
      ;; list of all applicable methods from UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
      (remove-if-not
        #'(lambda (method)
            (let* ((specializers (std-method-parameter-specializers method))
                   (specializer1 (first specializers))
                   (specializer2 (second specializers)))
              (and (atom specializer1) (subclassp old-class specializer1)
                   (atom specializer2) (subclassp new-class specializer2))))
        (the list (gf-methods |#'update-instance-for-different-class|))))))

;; Also in record.d.
(defun check-initialization-argument-list (initargs caller)
  (do ((l initargs (cddr l)))
      ((endp l))
    (unless (symbolp (car l))
      (error-of-type 'program-error ; ANSI CL 3.5.1.5. wants a PROGRAM-ERROR here.
        "~S: invalid initialization argument ~S"
        caller (car l)))
    (when (endp (cdr l))
      (error-of-type 'program-error ; ANSI CL 3.5.1.6. wants a PROGRAM-ERROR here.
        "~S: keyword arguments in ~S should occur pairwise"
        caller initargs))))

;; CLtL2 28.1.9.5., 28.1.9.4., ANSI CL 7.1.5., 7.1.4.
(defgeneric shared-initialize
    (instance slot-names &rest initargs &key &allow-other-keys))
(setq |#'shared-initialize| #'shared-initialize)
#||
 (defmethod shared-initialize ((instance standard-object) slot-names
                               &rest initargs)
  (check-initialization-argument-list initargs 'shared-initialize)
  (dolist (slot (class-slots (class-of instance)))
    (let ((slotname (slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
          (get-properties initargs (slot-definition-initargs slot))
        (declare (ignore init-key))
        (if foundp
          (setf (slot-value instance slotname) init-value)
          (unless (slot-boundp instance slotname)
            (let ((initfunction (slot-definition-initfunction slot)))
              (when init
                (when (or (eq slot-names 'T) (memq slotname slot-names))
                  (setf (slot-value instance slotname)
                        (funcall initfunction))))))))))
  instance)
||#
;; the main work is done by a SUBR:
(do-defmethod 'shared-initialize
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%shared-initialize '(T)))
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-object) (find-class 't))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 2 :rest-p t)))
(do-defmethod 'shared-initialize
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%shared-initialize '(T)))
    :wants-next-method-p nil
    :parameter-specializers
      (list (find-class 'structure-object) (find-class 't))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 2 :rest-p t)))

;; CLtL2 28.1.12., ANSI CL 7.3.
(defgeneric reinitialize-instance
    (instance &rest initargs &key &allow-other-keys))
(setq |#'reinitialize-instance| #'reinitialize-instance)
#||
 (defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (check-initialization-argument-list initargs 'reinitialize-instance)
  (apply #'shared-initialize instance 'NIL initargs))
||#
#|| ; optimized:
 (defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (check-initialization-argument-list initargs 'reinitialize-instance)
  (let ((h (gethash (class-of instance) *reinitialize-instance-table*)))
    (if h
      (progn
        ;; CLtL2 28.1.9.2., ANSI CL 7.1.2. Validity of initialization arguments
        (let ((valid-keywords (car h)))
          (unless (eq valid-keywords 't)
            (sys::keyword-test initargs valid-keywords)))
        (if (not (eq (cdr h) #'clos::%shared-initialize))
          ;; apply effective method from shared-initialize:
          (apply (cdr h) instance 'NIL initargs)
          ;; clos::%shared-initialize with slot-names=NIL can be simplified:
          (progn
            (dolist (slot (class-slots (class-of instance)))
              (let ((slotname (slot-definition-name slot)))
                (multiple-value-bind (init-key init-value foundp)
                    (get-properties initargs (slot-definition-initargs slot))
                  (declare (ignore init-key))
                  (if foundp
                    (setf (slot-value instance slotname) init-value)))))
            instance)))
      (apply #'initial-reinitialize-instance instance initargs))))
||#
;; the main work is done by a SUBR:
(do-defmethod 'reinitialize-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%reinitialize-instance '(T)))
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-object))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 1 :rest-p t)))
(do-defmethod 'reinitialize-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%reinitialize-instance '(T)))
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'structure-object))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 1 :rest-p t)))
;; At the first call of REINITIALIZE-INSTANCE of each class
;; we memorize the needed information in *reinitialize-instance-table*.
(defun initial-reinitialize-instance (instance &rest initargs)
  (let* ((class (class-of instance))
         (valid-keywords (valid-reinitialize-instance-keywords class)))
    ;; CLtL2 28.1.9.2., ANSI CL 7.1.2. Validity of initialization arguments
    (unless (eq valid-keywords 't)
      (sys::keyword-test initargs valid-keywords))
    (let ((si-ef (compute-applicable-methods-effective-method |#'shared-initialize| instance 'NIL)))
      (setf (gethash class *reinitialize-instance-table*)
            (cons valid-keywords si-ef))
      (apply si-ef instance 'NIL initargs))))

;; CLtL2 28.1.9.6., ANSI CL 7.1.6.
(defgeneric initialize-instance (instance &rest initargs
                                 &key &allow-other-keys))
(setq |#'initialize-instance| #'initialize-instance)
#||
 (defmethod initialize-instance ((instance standard-object) &rest initargs)
  (check-initialization-argument-list initargs 'initialize-instance)
  (apply #'shared-initialize instance 'T initargs))
||#
#|| ; optimized:
 (defmethod initialize-instance ((instance standard-object) &rest initargs)
  (check-initialization-argument-list initargs 'initialize-instance)
  (let ((h (gethash class *make-instance-table*)))
    (if h
      (if (not (eq (svref h 3) #'clos::%shared-initialize))
        ;; apply effective method from shared-initialize:
        (apply (svref h 3) instance 'T initargs)
        ;; clos::%shared-initialize with slot-names=T can be simplified:
        (progn
          (dolist (slot (class-slots (class-of instance)))
            (let ((slotname (slot-definition-name slot)))
              (multiple-value-bind (init-key init-value foundp)
                  (get-properties initargs (slot-definition-initargs slot))
                (declare (ignore init-key))
                (if foundp
                  (setf (slot-value instance slotname) init-value)
                  (unless (slot-boundp instance slotname)
                    (let ((initfunction (slot-definition-initfunction slot)))
                      (when initfunction
                        (setf (slot-value instance slotname)
                              (funcall initfunction)))))))))
          instance))
      (apply #'initial-initialize-instance instance initargs))))
||#
;; the main work is done by a SUBR:
(do-defmethod 'initialize-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%initialize-instance '(T)))
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-object))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 1 :rest-p t)))
(do-defmethod 'initialize-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%initialize-instance '(T)))
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'structure-object))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 1 :rest-p t)))
;; At the first call of MAKE-INSTANCE or INITIALIZE-INSTANCE of each class
;; we memorize the needed information in *make-instance-table*.
(defun initial-initialize-instance (instance &rest initargs)
  (let ((class (class-of instance)))
    (multiple-value-bind (valid-keywords ai-ef)
        (make-instance-table-entry1 class)
      (multiple-value-bind (ii-ef si-ef) (make-instance-table-entry2 instance)
        (setf (gethash class *make-instance-table*)
              (vector valid-keywords ai-ef ii-ef si-ef))
        ;; apply effective method from SHARED-INITIALIZE:
        (apply si-ef instance 'T initargs)))))

;; User-defined methods on allocate-instance are now supported.
(defgeneric allocate-instance
    (instance &rest initargs &key &allow-other-keys))
(setq |#'allocate-instance| #'allocate-instance)
#||
 (defgeneric allocate-instance (class)
  (:method ((class standard-class))
    (unless (class-precedence-list class) (finalize-class class t))
    (allocate-std-instance class (class-instance-size class)))
  (:method ((class structure-class))
    (sys::%make-structure (class-names class) (class-instance-size class)
                          :initial-element unbound)))
||#
#||
 (defun %allocate-instance (class &rest initargs)
  (check-initialization-argument-list initargs 'allocate-instance)
  ;; No need to check the validity of the initargs, because ANSI CL says
  ;; "The caller of allocate-instance is expected to have already checked
  ;;  the initialization arguments."
  ;; Quick and dirty dispatch among <standard-class> and <structure-class>.
  ;; (class-current-version class) is a simple-vector, (class-names class) a cons.
  (if (atom (class-current-version class))
    (progn (unless (class-precedence-list class) (finalize-class class t))
      (allocate-std-instance class (class-instance-size class)))
    (sys::%make-structure (class-names class) (class-instance-size class)
                          :initial-element unbound)))
||#
; the main work is done by a SUBR:
(do-defmethod 'allocate-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%allocate-instance '(T)))
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-class))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 1 :rest-p t)))
(do-defmethod 'allocate-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%allocate-instance '(T)))
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'structure-class))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 1 :rest-p t)))

;; CLtL2 28.1.9.7., ANSI CL 7.1.7.
(defgeneric make-instance (class &rest initargs &key &allow-other-keys)
  (:method ((class symbol) &rest initargs)
    (apply #'make-instance (find-class class) initargs)))
#||
 (defmethod make-instance ((class standard-class) &rest initargs)
  (check-initialization-argument-list initargs 'make-instance)
  ;; CLtL2 28.1.9.3., 28.1.9.4., ANSI CL 7.1.3., 7.1.4.: Take note of
  ;; default-initargs:
  (dolist (default-initarg (class-default-initargs class))
    (let ((nothing default-initarg))
      (when (eq (getf initargs (car default-initarg) nothing) nothing)
        (setq initargs
              (append initargs
                (list (car default-initarg)
                      (funcall (caddr default-initarg))))))))
  #||
  ;; CLtL2 28.1.9.2., ANSI CL 7.1.2. Validity of initialization arguments
  (sys::keyword-test initargs
      (union (class-valid-initargs class)
             (applicable-keywords #'initialize-instance class))) ; ??
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs))
  ||#
  (let ((h (gethash class *make-instance-table*)))
    (if h
      (progn
        ;; CLtL2 28.1.9.2., ANSI CL 7.1.2. Validity of initialization arguments
        (let ((valid-keywords (svref h 0)))
          (unless (eq valid-keywords 't)
            (sys::keyword-test initargs valid-keywords)))
        (let ((instance (apply #'allocate-instance class initargs)))
          (if (not (eq (svref h 2) #'clos::%initialize-instance))
            ;; apply effective method from initialize-instance:
            (apply (svref h 2) instance initargs)
            ;; clos::%initialize-instance can be simplified (one does not need
            ;; to look into *make-instance-table* once again):
            (if (not (eq (svref h 3) #'clos::%shared-initialize))
              ;; apply effective method from shared-initialize:
              (apply (svref h 3) instance 'T initargs)
              ...
            ))))
      (apply #'initial-make-instance class initargs))))
||#
;; the main work is done by a SUBR:
(do-defmethod 'make-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%make-instance '(T)))
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-class))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 1 :rest-p t)))
(do-defmethod 'make-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%make-instance '(T)))
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'structure-class))
    :qualifiers '()
    :signature #s(compiler::signature :req-num 1 :rest-p t)))
;; At the first call of MAKE-INSTANCE or INITIALIZE-INSTANCE of each class
;; we memorize the needed information in *make-instance-table*.
(defun initial-make-instance (class &rest initargs)
  (multiple-value-bind (valid-keywords ai-ef)
      (make-instance-table-entry1 class)
    ;; http://www.lisp.org/HyperSpec/Body/sec_7-1-2.html
    ;; 7.1.2 Declaring the Validity of Initialization Arguments
    (unless (eq valid-keywords 't)
      (sys::keyword-test initargs valid-keywords))
    ;; call the effective method of ALLOCATE-INSTANCE:
    (let ((instance (apply ai-ef class initargs)))
      (unless (eq (class-of instance) class)
        (error-of-type 'error
          (TEXT "~S method for ~S returned ~S")
          'allocate-instance class instance))
      (multiple-value-bind (ii-ef si-ef) (make-instance-table-entry2 instance)
        (setf (gethash class *make-instance-table*)
              (vector valid-keywords ai-ef ii-ef si-ef))
        ;; call the effective method of INITIALIZE-INSTANCE:
        (apply ii-ef instance initargs))
      ;; return the instance
      instance)))

;; Users want to be able to create instances of subclasses of <standard-class>
;; and <structure-class>. So, when creating a class, we now go through
;; MAKE-INSTANCE and INITIALIZE-INSTANCE.
(setf (fdefinition 'make-instance-standard-class) #'make-instance)
(setf (fdefinition 'make-instance-structure-class) #'make-instance)
(defmethod initialize-instance ((new-class-object standard-class) &rest args
                                &key name (metaclass <standard-class>)
                                documentation direct-superclasses direct-slots
                                direct-default-initargs)
  (declare (ignore documentation direct-superclasses direct-slots
                   direct-default-initargs))
  (setf (class-classname new-class-object) name)
  (setf (class-metaclass new-class-object) metaclass) ; = (class-of new-class-object)
  (apply #'initialize-instance-standard-class new-class-object args)
  new-class-object)
(defmethod initialize-instance ((new-class-object structure-class) &rest args
                                &key name (metaclass <structure-class>)
                                documentation direct-superclasses
                                ((:direct-slots direct-slots-as-lists) '())
                                ((direct-slots direct-slots-as-metaobjects) '())
                                direct-default-initargs names slots size)
  (declare (ignore documentation direct-superclasses direct-slots-as-lists
                   direct-slots-as-metaobjects direct-default-initargs names
                   slots size))
  (setf (class-classname new-class-object) name)
  (setf (class-metaclass new-class-object) metaclass) ; = (class-of new-class-object)
  (apply #'initialize-instance-structure-class new-class-object args)
  new-class-object)

;;; change-class
(defgeneric change-class (instance new-class &key &allow-other-keys)
  (:method ((instance standard-object) (new-class standard-class)
            &rest initargs)
    ;; ANSI CL 7.2.1. Modifying the Structure of the Instance.
    (let ((previous (%change-class instance new-class)))
      ;; previous = a copy of instance
      ;; instance = mutated instance, with new class, slots unbound
      ;; Copy identically named slots:
      (let ((old-slots (class-slots (class-of previous)))
            (new-slots (class-slots new-class)))
        (dolist (slot old-slots)
          (let ((name (slot-definition-name slot)))
            (when (slot-boundp previous name)
              ;; Retain the slot's value if it is a local slot in new-class.
              (let ((new-slot (find name new-slots :test #'eq
                                    :key #'slot-definition-name)))
                (when (and new-slot (atom (slot-definition-location new-slot)))
                  (setf (slot-value instance name)
                        (slot-value previous name))))))))
      ;; ANSI CL 7.2.2. Initializing Newly Added Local Slots.
      (apply #'update-instance-for-different-class
             previous instance initargs)))
  (:method ((instance t) (new-class symbol) &rest initargs)
    (apply #'change-class instance (find-class new-class) initargs)))

(defgeneric update-instance-for-different-class (previous current
                                                 &key &allow-other-keys)
  (:method ((previous standard-object) (current standard-object)
            &rest initargs)
    ;; ANSI CL 7.2.2. Initializing Newly Added Local Slots.
    (check-initialization-argument-list initargs 'update-instance-for-different-class)
    (let* ((old-class (class-of previous))
           (old-slots-table (class-slot-location-table old-class))
           (new-class (class-of current))
           (added-slots
             (mapcan #'(lambda (slot)
                         ; Only local slots.
                         (when (atom (slot-definition-location slot))
                           (let ((name (slot-definition-name slot)))
                             ; Only slots for which no slot of the same name
                             ; exists in the previous class.
                             (when (null (gethash name old-slots-table))
                               (list name)))))
                     (class-slots new-class))))
      ;; CLtL2 28.1.9.2., ANSI CL 7.1.2. Validity of initialization arguments
      (multiple-value-bind (valid-keywords found)
          (gethash (cons old-class new-class) *update-instance-for-different-class-table*)
        (unless found
          (setq valid-keywords
                (valid-update-instance-for-different-class-keywords
                  old-class new-class added-slots))
          (setf (gethash (cons old-class new-class) *update-instance-for-different-class-table*)
                valid-keywords))
        (unless (eq valid-keywords 't)
          (sys::keyword-test initargs valid-keywords)))
      (apply #'shared-initialize current added-slots initargs))))
(setq |#'update-instance-for-different-class| #'update-instance-for-different-class)

;; Users want to be able to create subclasses of <standard-class> and write
;; methods on MAKE-INSTANCES-OBSOLETE on them. So, we now go redefine
;; MAKE-INSTANCES-OBSOLETE as a generic function.
(fmakunbound 'make-instances-obsolete)
(defgeneric make-instances-obsolete (class)
  (:method ((class standard-class))
    (make-instances-obsolete-standard-class class)
    class)
  (:method ((class symbol))
    (make-instances-obsolete (find-class class))
    class))

(defgeneric update-instance-for-redefined-class
    (instance added-slots discarded-slots property-list &rest initargs
     &key &allow-other-keys)
  (:method ((instance standard-object) added-slots discarded-slots
            property-list &rest initargs)
    (check-initialization-argument-list initargs 'update-instance-for-redefined-class)
    ;; CLtL2 28.1.9.2., ANSI CL 7.1.2. Validity of initialization arguments
    (let ((class (class-of instance)))
      (multiple-value-bind (valid-keywords found)
          (gethash class *update-instance-for-redefined-class-table*)
        (unless found
          (let (independent)
            (multiple-value-setq (valid-keywords independent)
                (valid-update-instance-for-redefined-class-keywords
                 class added-slots discarded-slots property-list))
            (when independent
              (setf (gethash class *update-instance-for-redefined-class-table*)
                    valid-keywords))))
        (unless (eq valid-keywords 't)
          (sys::keyword-test initargs valid-keywords))))
    (apply #'shared-initialize instance added-slots initargs)))
(setq |#'update-instance-for-redefined-class| #'update-instance-for-redefined-class)

;;; class prototype (MOP)

(defgeneric class-prototype (class)
  (:method ((class standard-class))
    (or (class-proto class)
        (setf (class-proto class)
              (let ((old-instantiated (class-instantiated class)))
                (prog1
                  (clos::%allocate-instance class)
                  ;; The allocation of the prototype doesn't need to flag the
                  ;; class as being instantiated, because 1. the prototype is
                  ;; thrown away when the class is redefined, 2. we don't want
                  ;; a redefinition with nonexistent or non-finalized
                  ;; superclasses to succeed despite of the prototype.
                  (setf (class-instantiated class) old-instantiated))))))
  (:method ((name symbol)) (class-prototype (find-class name))))

;;; class finalization (MOP)

(defgeneric class-finalized-p (class)
  (:method ((class standard-class)) (not (null (class-precedence-list class))))
  (:method ((name symbol)) (class-finalized-p (find-class name))))

(defgeneric finalize-inheritance (class)
  (:method ((class standard-class)) (finalize-class class t))
  (:method ((name symbol)) (finalize-inheritance (find-class name))))

;;; Utility functions

;; Returns the slot names of an instance of a slotted-class
;; (i.e. of a structure-object or standard-object).
(defun slot-names (object)
  (mapcar #'slot-definition-name (class-slots (class-of object))))
