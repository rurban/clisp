;;;; Common Lisp Object System for CLISP
;;;; Class metaobjects
;;;; Part 3: Class definition and redefinition.
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; Wipe out all traces of an earlier loaded CLOS.
(eval-when (load eval)
  (do-all-symbols (s) (remprop s 'CLOSCLASS)))

;; CLtL2 28.1.4., ANSI CL 4.3.7. Integrating Types and Classes
(defun subclassp (class1 class2)
  (unless (>= (class-initialized class1) 4) (finalize-class class1 t))
  (values
    (gethash class2 (class-all-superclasses class1)))) ; T or (default) NIL

;; Continue bootstrapping.
(%defclos
  ;; distinctive marks for CLASS-P
  *<standard-class>-class-version*
  *<structure-class>-class-version*
  *<built-in-class>-class-version*
  'class
  ;; built-in-classes for CLASS-OF
  (vector 'array 'bit-vector 'character 'complex 'cons 'float 'function
          'hash-table 'integer 'null 'package 'pathname
          #+LOGICAL-PATHNAMES 'logical-pathname
          'random-state 'ratio 'readtable
          'stream 'file-stream 'synonym-stream 'broadcast-stream
          'concatenated-stream 'two-way-stream 'echo-stream 'string-stream
          'string 'symbol 't 'vector))

;; Bootstrapping support.
(defun replace-class-version (class class-version)
  (replace class-version (class-current-version class))
  (setf (class-current-version class) class-version))

;;; -------------------------------- DEFCLASS --------------------------------

(defmacro defclass (&whole whole-form
                    name superclass-specs slot-specs &rest options)
  (unless (symbolp name)
    (error-of-type 'ext:source-program-error
      :form whole-form
      :detail name
      (TEXT "~S: class name ~S should be a symbol")
      'defclass name))
  (let* ((superclass-forms
           (progn
             (unless (listp superclass-specs)
               (error-of-type 'ext:source-program-error
                 :form whole-form
                 :detail superclass-specs
                 (TEXT "~S ~S: expecting list of superclasses instead of ~S")
                 'defclass name superclass-specs))
             (mapcar #'(lambda (superclass)
                         (unless (symbolp superclass)
                           (error-of-type 'ext:source-program-error
                             :form whole-form
                             :detail superclass
                             (TEXT "~S ~S: superclass name ~S should be a symbol")
                             'defclass name superclass))
                         `',superclass)
                       superclass-specs)))
         (accessor-method-decl-forms '())
         (accessor-function-decl-forms '())
         (generic-accessors 'T)
         (slot-forms
           (let ((slot-names '()))
             (unless (listp slot-specs)
               (error-of-type 'ext:source-program-error
                 :form whole-form
                 :detail slot-specs
                 (TEXT "~S ~S: expecting list of slot specifications instead of ~S")
                 'defclass name slot-specs))
             (mapcar #'(lambda (slot-spec)
                         (let ((slot-name slot-spec) (slot-options '()))
                           (when (consp slot-spec)
                             (setq slot-name (car slot-spec)
                                   slot-options (cdr slot-spec)))
                           (unless (symbolp slot-name)
                             (error-of-type 'ext:source-program-error
                               :form whole-form
                               :detail slot-name
                               (TEXT "~S ~S: slot name ~S should be a symbol")
                               'defclass name slot-name))
                           (if (memq slot-name slot-names)
                             (error-of-type 'ext:source-program-error
                               :form whole-form
                               :detail slot-names
                               (TEXT "~S ~S: There may be only one direct slot with the name ~S.")
                               'defclass name slot-name)
                             (push slot-name slot-names))
                           (let ((readers '())
                                 (writers '())
                                 (allocation '())
                                 (initargs '())
                                 (initform nil) (initfunction nil)
                                 (types '())
                                 (documentation nil))
                             (when (oddp (length slot-options))
                               (error-of-type 'ext:source-program-error
                                 :form whole-form
                                 :detail slot-options
                                 (TEXT "~S ~S: slot options for slot ~S must come in pairs")
                                 'defclass name slot-name))
                             (do ((optionsr slot-options (cddr optionsr)))
                                 ((atom optionsr))
                               (let ((optionkey (first optionsr))
                                     (argument (second optionsr)))
                                 (case optionkey
                                   (:READER
                                    (unless (and (symbolp argument) argument)
                                      (error-of-type 'ext:source-program-error
                                        :form whole-form
                                        :detail argument
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a non-NIL symbol")
                                        'defclass name slot-name argument))
                                    (push argument readers))
                                   (:WRITER
                                    (unless (function-name-p argument)
                                      (error-of-type 'ext:source-program-error
                                        :form whole-form
                                        :detail argument
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a function name")
                                        'defclass name slot-name argument))
                                    (push argument writers))
                                   (:ACCESSOR
                                    (unless (and (symbolp argument) argument)
                                      (error-of-type 'ext:source-program-error
                                        :form whole-form
                                        :detail argument
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a non-NIL symbol")
                                        'defclass name slot-name argument))
                                    (push argument readers)
                                    (push `(SETF ,argument) writers))
                                   (:ALLOCATION
                                    (when allocation
                                      (error-of-type 'ext:source-program-error
                                        :form whole-form
                                        :detail slot-options
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':allocation slot-name))
                                    (case argument
                                      ((:INSTANCE :CLASS) (setq allocation argument))
                                      (t (error-of-type 'ext:source-program-error
                                           :form whole-form
                                           :detail argument
                                           (TEXT "~S ~S, slot option for slot ~S must have the value ~S or ~S, not ~S")
                                           'defclass name slot-name ':instance ':class argument))))
                                   (:INITARG
                                    (unless (symbolp argument)
                                      (error-of-type 'ext:source-program-error
                                        :form whole-form
                                        :detail argument
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a symbol")
                                        'defclass name slot-name argument))
                                    (push argument initargs))
                                   (:INITFORM
                                    (when initform
                                      (error-of-type 'ext:source-program-error
                                        :form whole-form
                                        :detail slot-options
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':initform slot-name))
                                    (setq initform `(QUOTE ,argument)
                                          initfunction (make-initfunction-form argument slot-name)))
                                   (:TYPE
                                    (when types
                                      (error-of-type 'ext:source-program-error
                                        :form whole-form
                                        :detail slot-options
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':type slot-name))
                                    (setq types (list argument)))
                                   (:DOCUMENTATION
                                    (when documentation
                                      (error-of-type 'ext:source-program-error
                                        :form whole-form
                                        :detail slot-options
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':documentation slot-name))
                                    (unless (stringp argument)
                                      (error-of-type 'ext:source-program-error
                                        :form whole-form
                                        :detail argument
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a string")
                                        'defclass name slot-name argument))
                                    (setq documentation argument))
                                   (t
                                     (error-of-type 'ext:source-program-error
                                       :form whole-form
                                       :detail optionkey
                                       (TEXT "~S ~S, slot option for slot ~S: ~S is not a valid slot option")
                                       'defclass name slot-name optionkey)))))
                             (setq readers (nreverse readers))
                             (setq writers (nreverse writers))
                             (let ((type (if types (first types) 'T)))
                               (dolist (funname readers)
                                 (push `(DECLAIM-METHOD ,funname ((OBJECT ,name)))
                                       accessor-method-decl-forms)
                                 (push `(PROCLAIM '(FUNCTION ,funname (,name) ,type))
                                       accessor-function-decl-forms)
                                 (push `(SYSTEM::EVAL-WHEN-COMPILE (SYSTEM::C-DEFUN ',funname (SYSTEM::LAMBDA-LIST-TO-SIGNATURE '(OBJECT))))
                                       accessor-function-decl-forms))
                               (dolist (funname writers)
                                 (push `(DECLAIM-METHOD ,funname (NEW-VALUE (OBJECT ,name)))
                                       accessor-method-decl-forms)
                                 (push `(PROCLAIM '(FUNCTION ,funname (,type ,name) ,type))
                                       accessor-function-decl-forms)
                                 (push `(SYSTEM::EVAL-WHEN-COMPILE (SYSTEM::C-DEFUN ',funname (SYSTEM::LAMBDA-LIST-TO-SIGNATURE '(NEW-VALUE OBJECT))))
                                       accessor-function-decl-forms)))
                             `(LIST
                                :NAME ',slot-name
                                ,@(when readers `(:READERS ',readers))
                                ,@(when writers `(:WRITERS ',writers))
                                ,@(when (eq allocation ':class) `(:ALLOCATION :CLASS))
                                ,@(when initargs `(:INITARGS ',(nreverse initargs)))
                                ,@(when initform `(:INITFORM ',initform :INITFUNCTION ,initfunction))
                                ,@(when types `(:TYPE ',(first types)))
                                ,@(when documentation `(:DOCUMENTATION ',documentation))))))
                     slot-specs))))
    `(LET ()
       (EVAL-WHEN (COMPILE LOAD EVAL)
         (APPLY #'ENSURE-CLASS
           ',name
           :DIRECT-SUPERCLASSES (LIST ,@superclass-forms)
           :DIRECT-SLOTS (LIST ,@slot-forms)
           ,@(let ((metaclass nil)
                   (direct-default-initargs nil)
                   (documentation nil)
                   (fixed-slot-locations nil))
               (dolist (option options)
                 (block nil
                   (when (listp option)
                     (let ((optionkey (first option)))
                       (when (case optionkey
                               (:METACLASS metaclass)
                               (:DEFAULT-INITARGS direct-default-initargs)
                               (:DOCUMENTATION documentation))
                         (error-of-type 'ext:source-program-error
                           :form whole-form
                           :detail options
                           (TEXT "~S ~S, option ~S may only be given once")
                           'defclass name optionkey))
                       (case optionkey
                         (:METACLASS
                          (when (eql (length option) 2)
                            (let ((argument (second option)))
                              (unless (symbolp argument)
                                (error-of-type 'ext:source-program-error
                                  :form whole-form
                                  :detail argument
                                  (TEXT "~S ~S, option ~S: ~S is not a symbol")
                                  'defclass name option argument))
                              (setq metaclass `(FIND-CLASS ',argument)))
                            (return)))
                         (:DEFAULT-INITARGS
                          (let ((list (rest option)))
                            (when (and (consp list) (null (cdr list)) (listp (car list)))
                              (setq list (car list))
                              (warn (TEXT "~S ~S: option ~S should be written ~S")
                                    'defclass name option (cons ':DEFAULT-INITARGS list)))
                            (when (oddp (length list))
                              (error-of-type 'ext:source-program-error
                                :form whole-form
                                :detail list
                                (TEXT "~S ~S, option ~S: arguments must come in pairs")
                                'defclass name option))
                            (setq direct-default-initargs
                                  `(:DIRECT-DEFAULT-INITARGS
                                    (LIST
                                     ,@(let ((arglist nil) (formlist nil))
                                         (do ((listr list (cddr listr)))
                                             ((atom listr))
                                           (unless (symbolp (first listr))
                                             (error-of-type 'ext:source-program-error
                                               :form whole-form
                                               :detail (first listr)
                                               (TEXT "~S ~S, option ~S: ~S is not a symbol")
                                               'defclass name option (first listr)))
                                           (when (member (first listr) arglist)
                                             (error-of-type 'ext:source-program-error
                                               :form whole-form
                                               :detail list
                                               (TEXT "~S ~S, option ~S: ~S may only be given once")
                                               'defclass name option (first listr)))
                                           (push (first listr) arglist)
                                           (push (second listr) formlist))
                                         (mapcan #'(lambda (arg form)
                                                     `((LIST ',arg ',form ,(make-initfunction-form form arg))))
                                                 (nreverse arglist) (nreverse formlist)))))))
                          (return))
                         (:DOCUMENTATION
                          (when (eql (length option) 2)
                            (let ((argument (second option)))
                              (unless (stringp argument)
                                (error-of-type 'ext:source-program-error
                                  :form whole-form
                                  :detail argument
                                  (TEXT "~S ~S, option ~S: ~S is not a string")
                                  'defclass name option argument))
                              (setq documentation
                                    `(:DOCUMENTATION ',argument)))
                            (return)))
                         (:GENERIC-ACCESSORS
                          (when (eql (length option) 2)
                            (let ((argument (second option)))
                              (setq generic-accessors argument)
                              (return))))
                         (:FIXED-SLOT-LOCATIONS
                          (setq fixed-slot-locations `(:FIXED-SLOT-LOCATIONS 'T))
                          (return)))))
                   (error-of-type 'ext:source-program-error
                     :form whole-form
                     :detail option
                     (TEXT "~S ~S: invalid option ~S")
                     'defclass name option)))
               `(,@(if metaclass `(:METACLASS ,metaclass))
                 ;; Here we use (or ... '(... NIL)) because when a class is
                 ;; being redefined, :DOCUMENTATION NIL means to erase the
                 ;; documentation string, while nothing means to keep it!
                 ;; See MOP p. 57.
                 ,@(or direct-default-initargs '(:DIRECT-DEFAULT-INITARGS NIL))
                 ,@(or documentation '(:DOCUMENTATION NIL))
                 :GENERIC-ACCESSORS ',generic-accessors
                 ,@(or fixed-slot-locations '(:FIXED-SLOT-LOCATIONS NIL))
                 ;; Pass the default initargs of the metaclass, in
                 ;; order to erase leftovers from the previous definition.
                 ,(if metaclass
                    `(MAPCAP #'(LAMBDA (X) (LIST (FIRST X) (FUNCALL (THIRD X))))
                             (CLASS-DEFAULT-INITARGS ,metaclass))
                    `NIL)))))
       ,@(if generic-accessors
           (nreverse accessor-method-decl-forms) ; the DECLAIM-METHODs
           (nreverse accessor-function-decl-forms)) ; the C-DEFUNs
       (FIND-CLASS ',name))))

;; DEFCLASS execution:

;; The function responsible for a MAKE-INSTANCES-OBSOLETE call.
(defvar *make-instances-obsolete-caller* 'make-instances-obsolete)

(defun ensure-class-using-class-<t> (class name &rest all-keys
                                     &key (metaclass <standard-class>)
                                          (direct-superclasses '())
                                          (direct-slots '())
                                          (direct-default-initargs '())
                                          (documentation nil)
                                          (fixed-slot-locations nil)
                                     &allow-other-keys)
  (declare (ignore direct-slots direct-default-initargs documentation
                   fixed-slot-locations))
  ;; Argument checks.
  (unless (symbolp name)
    (error (TEXT "~S: class name ~S should be a symbol")
           'ensure-class-using-class name))
  (unless (class-p metaclass)
    (if (symbolp metaclass)
      (setq metaclass
            (cond ((eq metaclass 'standard-class) <standard-class>) ; for bootstrapping
                  (t (find-class metaclass))))
      (error (TEXT "~S for class ~S: metaclass ~S is neither a class or a symbol")
             'ensure-class-using-class name metaclass)))
  (unless (or (eq metaclass <standard-class>) ; for bootstrapping
              (subclassp metaclass <class>))
    (error (TEXT "~S for class ~S: metaclass ~S is not a subclass of CLASS")
           'ensure-class-using-class name metaclass))
  (unless (proper-list-p direct-superclasses)
    (error (TEXT "~S for class ~S: The ~S argument should be a proper list, not ~S")
           'ensure-class-using-class name ':direct-superclasses direct-superclasses))
  (unless (every #'(lambda (x) (or (class-p x) (symbolp x))) direct-superclasses)
    (error (TEXT "~S for class ~S: The direct-superclasses list should consist of classes and symbols, not ~S")
           'ensure-class-using-class name direct-superclasses))
  ;; Ignore the old class if the given name is not its "proper name".
  ;; (This is an ANSI CL requirement; it's not clear whether it belongs
  ;; here or in ENSURE-CLASS.)
  (when (and class (not (eq (class-name class) name)))
    (return-from ensure-class-using-class-<t>
      (apply #'ensure-class-using-class nil name all-keys)))
  ;; Decide whether to modify the given class or ignore it.
  (let ((a-standard-class-p (or (eq metaclass <standard-class>)
                                (subclassp metaclass <standard-class>))))
    (when class
      (cond ((not (eq metaclass (class-of class)))
             ;; This can occur when mixing DEFSTRUCT and DEFCLASS.
             ;; MOP p. 48 says "If the class of the class argument is not the
             ;; same as the class specified by the :metaclass argument, an
             ;; error is signalled." But we can do better: ignore the old
             ;; class, warn and proceed. The old instances will thus keep
             ;; pointing to the old class.
             (warn (TEXT "Cannot redefine ~S with a different metaclass ~S")
                   class metaclass)
             (setq class nil))
            ((not a-standard-class-p)
             ;; This can occur when redefining a class defined through
             ;; (DEFCLASS ... (:METACLASS STRUCTURE-CLASS)), which is
             ;; equivalent to re-executed DEFSTRUCT.
             ;; Only <standard-class> subclasses support making instances
             ;; obsolete. Ignore the old class and proceed. The old instances
             ;; will thus keep pointing to the old class.
             (setq class nil)))
      (unless class
        (return-from ensure-class-using-class-<t>
          (apply #'ensure-class-using-class nil name all-keys))))
    ;; Preparation of class initialization arguments.
    (setq all-keys (copy-list all-keys))
    (remf all-keys ':metaclass)
    ;; See which direct superclasses are already defined.
    (setq direct-superclasses
          (mapcar #'(lambda (c)
                      (if (class-p c)
                        c
                        (or (find-class c (not a-standard-class-p)) c)))
                  direct-superclasses))
    (if class
      ;; Modify the class and return the modified class.
      (apply #'reinitialize-instance ; => #'reinitialize-instance-<class>
             class
             :direct-superclasses direct-superclasses
             all-keys)
      (setf (find-class name)
            (setq class
              (apply (cond ((eq metaclass <standard-class>)
                            #'make-instance-<standard-class>)
                           ((eq metaclass <funcallable-standard-class>)
                            #'make-instance-<funcallable-standard-class>)
                           ((eq metaclass <built-in-class>)
                            #'make-instance-<built-in-class>)
                           ((eq metaclass <structure-class>)
                            #'make-instance-<structure-class>)
                           (t #'make-instance))
                     metaclass
                     :name name
                     :direct-superclasses direct-superclasses
                     all-keys))))
    class))

;; Preliminary.
(defun ensure-class-using-class (class name &rest args
                                 &key (metaclass <standard-class>)
                                      (direct-superclasses '())
                                      (direct-slots '())
                                      (direct-default-initargs '())
                                      (documentation nil)
                                      (fixed-slot-locations nil)
                                 &allow-other-keys)
  (declare (ignore metaclass direct-superclasses direct-slots
                   direct-default-initargs documentation fixed-slot-locations))
  (apply #'ensure-class-using-class-<t> class name args))

;; MOP p. 46
(defun ensure-class (name &rest args
                     &key (metaclass <standard-class>)
                          (direct-superclasses '())
                          (direct-slots '())
                          (direct-default-initargs '())
                          (documentation nil)
                          (fixed-slot-locations nil)
                     &allow-other-keys)
  (declare (ignore metaclass direct-superclasses direct-slots
                   direct-default-initargs documentation fixed-slot-locations))
  (unless (symbolp name)
    (error (TEXT "~S: class name ~S should be a symbol")
           'ensure-class name))
  (let ((result
          (apply #'ensure-class-using-class (find-class name nil) name args)))
    ; A check, to verify that user-defined methods on ensure-class-using-class
    ; work as they should.
    (unless (class-p result)
      (error (TEXT "Wrong ~S result for ~S: not a class: ~S")
             'ensure-class-using-class name result))
    result))

;; Preliminary.
(defun reader-method-class (class direct-slot &rest initargs)
  (declare (ignore class direct-slot initargs))
  <standard-reader-method>)
(defun writer-method-class (class direct-slot &rest initargs)
  (declare (ignore class direct-slot initargs))
  <standard-writer-method>)

;; ---------------------------- Class redefinition ----------------------------

(defun reinitialize-instance-<class> (class &rest all-keys
                                      &key (name nil name-p)
                                           (direct-superclasses '() direct-superclasses-p)
                                           (direct-slots '() direct-slots-p)
                                           (direct-default-initargs '() direct-default-initargs-p)
                                           (documentation nil documentation-p)
                                           (fixed-slot-locations nil fixed-slot-locations-p)
                                      &allow-other-keys
                                      &aux (metaclass (class-of class)))
  (if (and (>= (class-initialized class) 4) ; already finalized?
           (subclassp class <metaobject>))
    ;; Things would go awry when we try to redefine <class> and similar.
    (warn (TEXT "Redefining metaobject class ~S has no effect.")
          class)
    (progn
      (when direct-superclasses-p
        ;; Normalize the (class-direct-superclasses class) in the same way as
        ;; the direct-superclasses argument, so that we can compare the two
        ;; lists using EQUAL.
        (when (and (subclassp metaclass <standard-class>)
                   (< (class-initialized class) 3))
          (do ((l (class-direct-superclasses class) (cdr l)))
              ((atom l))
            (let ((c (car l)))
              (unless (class-p c)
                (let ((new-c (or (find-class c nil) c)))
                  (unless (symbolp new-c)
                    (check-allowed-superclass class new-c))
                  (setf (car l) new-c)
                  (when (class-p new-c) ; changed from symbol to class
                    (add-direct-subclass new-c class))))))))
      (when direct-slots-p
        ;; Convert the direct-slots to <direct-slot-definition> instances.
        (setq direct-slots (convert-direct-slots class direct-slots)))
      ;; Trivial changes (that can occur when loading the same code twice)
      ;; do not require updating the instances:
      ;; changed slot-options :initform, :documentation,
      ;; changed class-options :name, :default-initargs, :documentation.
      (if (or (and direct-superclasses-p
                   (not (equal (or direct-superclasses (default-direct-superclasses class))
                               (class-direct-superclasses class))))
              (and direct-slots-p
                   (not (equal-direct-slots direct-slots (class-direct-slots class))))
              (and direct-default-initargs-p
                   (not (equal-default-initargs direct-default-initargs
                                                (class-direct-default-initargs class))))
              (and fixed-slot-locations-p
                   (not (eq fixed-slot-locations (class-fixed-slot-locations class)))))
        ;; Instances have to be updated:
        (let* ((was-finalized (>= (class-initialized class) 6))
               (must-be-finalized
                 (and was-finalized
                      (some #'class-instantiated (list-all-finalized-subclasses class))))
               (old-direct-superclasses (class-direct-superclasses class))
               (old-direct-accessors (class-direct-accessors class))
               old-class)
          ;; ANSI CL 4.3.6. Remove accessor methods created by old DEFCLASS.
          (remove-accessor-methods old-direct-accessors)
          (setf (class-direct-accessors class) '())
          ;; Clear the cached prototype.
          (setf (class-prototype class) nil)
          ;; Declare all instances as obsolete, and backup the class object.
          (let ((old-version (class-current-version class))
                (*make-instances-obsolete-caller* 'defclass))
            (make-instances-obsolete class)
            (setq old-class (cv-class old-version)))
          (locally (declare (compile))
            (sys::%handler-bind
                ;; If an error occurs during the class redefinition, switch back
                ;; to the old definition, so that existing instances can continue
                ;; to be used.
                ((ERROR #'(lambda (condition)
                            (declare (ignore condition))
                            ;; Restore the class using the backup copy.
                            (let ((new-version (class-current-version class)))
                              (dotimes (i (sys::%record-length class))
                                (setf (sys::%record-ref class i) (sys::%record-ref old-class i)))
                              (setf (class-current-version class) new-version))
                            ;; Restore the accessor methods.
                            (add-accessor-methods old-direct-accessors)
                            (setf (class-direct-accessors class) old-direct-accessors))))
              (apply #'shared-initialize ; => #'shared-initialize-<built-in-class>
                                         ;    #'shared-initialize-<standard-class>
                                         ;    #'shared-initialize-<structure-class>
                     class
                     nil
                     `(,@(if direct-slots-p (list 'direct-slots direct-slots) '())
                       ,@all-keys))
              (update-subclasses-for-redefined-class class
                was-finalized must-be-finalized old-direct-superclasses)))
          (install-class-direct-accessors class))
        ;; Instances don't need to be updated:
        (progn
          (when name-p
            ;; Store new name:
            (setf (class-classname class) name))
          (when direct-slots-p
            ;; Store new slot-inits:
            (do ((l-old (class-direct-slots class) (cdr l-old))
                 (l-new direct-slots (cdr l-new)))
                ((null l-new))
              (let ((old (car l-old))
                    (new (car l-new)))
                (setf (slot-definition-initform old) (slot-definition-initform new))
                (setf (slot-definition-initfunction old) (slot-definition-initfunction new))
                (setf (slot-definition-documentation old) (slot-definition-documentation new)))))
          (when direct-default-initargs-p
            ;; Store new default-initargs:
            (do ((l-old (class-direct-default-initargs class) (cdr l-old))
                 (l-new direct-default-initargs (cdr l-new)))
                ((null l-new))
              (let ((old (cdar l-old))
                    (new (cdar l-new)))
                ;; Move initform and initfunction from new destructively into
                ;; the old one:
                (setf (car old) (car new))
                (setf (cadr old) (cadr new)))))
          (when documentation-p
            ;; Store new documentation:
            (setf (class-documentation class) documentation))
          ;; NB: These modifications are automatically inherited by the
          ;; subclasses of class! Due to <inheritable-slot-definition-initer>
          ;; and <inheritable-slot-definition-doc>.
          ;; No need to call (install-class-direct-accessors class) here.
      ) )
      ;; Notification of listeners:
      (map-dependents class
        #'(lambda (dependent)
            (apply #'update-dependent class dependent all-keys)))
  ) )
  class)

(defun equal-direct-slots (slots1 slots2)
  (or (and (null slots1) (null slots2))
      (and (consp slots1) (consp slots2)
           (equal-direct-slot (first slots1) (first slots2))
           (equal-direct-slots (rest slots1) (rest slots2)))))
(defun equal-default-initargs (initargs1 initargs2)
  (or (and (null initargs1) (null initargs2))
      (and (consp initargs1) (consp initargs2)
           (eq (car (first initargs1)) (car (first initargs2)))
           (equal-default-initargs (cdr initargs1) (cdr initargs2)))))

(defun map-dependents-<class> (class function)
  (dolist (dependent (class-listeners class))
    (funcall function dependent)))

;; ----------------------- General routines for <class> -----------------------

;; Preliminary.
(defun class-name (class)
  (class-classname class))

;; Returns the list of implicit direct superclasses when none was specified.
(defun default-direct-superclasses (class)
  (cond ((typep class <standard-class>) (list <standard-object>))
        ((typep class <funcallable-standard-class>) (list <funcallable-standard-object>))
        ((typep class <structure-class>) (list <structure-object>))
        (t '())))

;; When this is true, all safety checks about the metaclasses
;; of superclasses are omitted.
(defparameter *allow-mixing-metaclasses* nil)

(defun check-metaclass-mix (name direct-superclasses metaclass-test metaclass)
  (unless *allow-mixing-metaclasses*
    (unless (every metaclass-test direct-superclasses)
      (error-of-type 'error
        (TEXT "(~S ~S): superclass ~S should be of class ~S")
        'DEFCLASS name (find-if-not metaclass-test direct-superclasses)
        metaclass))))

;; Preliminary.
(defun validate-superclass (class superclass)
  (or ;; Green light if class and superclass belong to the same metaclass.
      (eq (sys::%record-ref class 0) (sys::%record-ref superclass 0))
      ;; Green light also if class is a funcallable-standard-class and
      ;; superclass is a standard-class.
      (and (eq (sys::%record-ref class 0) *<funcallable-standard-class>-class-version*)
           (eq (sys::%record-ref superclass 0) *<standard-class>-class-version*))
      ;; Other than that, only <standard-object> and <structure-object> can
      ;; inherit from <t> without belonging to the same metaclass.
      (and (eq superclass <t>)
           (memq (class-classname class) '(standard-object structure-object)))
      ;; And only <funcallable-standard-object> can inherit from <function>
      ;; without belonging to the same metaclass.
      (and (eq superclass <function>)
           (eq (class-classname class) 'funcallable-standard-object))))

(defun check-allowed-superclass (class superclass)
  (unless (validate-superclass class superclass)
    (error (TEXT "(~S ~S) for class ~S: ~S does not allow ~S to become a subclass of ~S. You may define a method on ~S to allow this.")
           'initialize-instance 'class (class-classname class) 'validate-superclass class superclass
           'validate-superclass)))

;;; The direct-subclasses slot can be either
;;; - NIL or a weak-list (for saving memory when there are few subclasses), or
;;; - a weak-hash-table (for speed when there are many subclasses).
#|
;; Adds a class to the list of direct subclasses.
(defun add-direct-subclass (class subclass) ...)
;; Removes a class from the list of direct subclasses.
(defun remove-direct-subclass (class subclass) ...)
;; Returns the currently existing direct subclasses, as a freshly consed list.
(defun list-direct-subclasses (class) ...)
|#
(def-weak-set-accessors class-direct-subclasses-table class
  add-direct-subclass-internal
  remove-direct-subclass-internal
  list-direct-subclasses)

;; Preliminary.
(defun add-direct-subclass (class subclass)
  (add-direct-subclass-internal class subclass))
(defun remove-direct-subclass (class subclass)
  (remove-direct-subclass-internal class subclass))

(defun update-subclasses-sets (class old-direct-superclasses new-direct-superclasses)
  ;; Drop classes that are not yet defined; they have no subclasses list.
  (setq old-direct-superclasses (remove-if #'symbolp old-direct-superclasses))
  (setq new-direct-superclasses (remove-if #'symbolp new-direct-superclasses))
  (unless (equal old-direct-superclasses new-direct-superclasses)
    (let ((removed-direct-superclasses
            (set-difference old-direct-superclasses new-direct-superclasses))
          (added-direct-superclasses
            (set-difference new-direct-superclasses old-direct-superclasses)))
      (dolist (super removed-direct-superclasses)
        (remove-direct-subclass super class))
      (dolist (super added-direct-superclasses)
        (add-direct-subclass super class)))))

;; ----------------------------------------------------------------------------
;; CLtL2 28.1.5., ANSI CL 4.3.5. Determining the Class Precedence List

;; The set of all classes forms a directed graph: Class C is located
;; below the direct superclasses of C. This graph is acyclic, because
;; at the moment of definition of the class C all direct superclasses must
;; already be present.

;; Hence, one can use Noether Induction (Induction from above to below in
;; the class graph) .

;; For a class C let DS(n) be the list of all direct superclasses of C.
;; The set of all superclasses (incl. C itself) is inductively defined as
;; S(C) := {C} union union_{D in DS(C)} S(D).

;; In other words:
;; S(C) = { C_n : C_n in DS(C_{n-1}), ..., C_1 in DS(C_0), C_0 = C }

;; Lemma 1: (a) C in S(C).
;;          (b) DS(C) subset S(C).
;;          (c) D in DS(C) ==> S(D) subset S(C).
;;          (d) D in S(C) ==> S(D) subset S(C).
;; proof:  (a) follows from the definition.
;;         (b) from (a) and from the definition.
;;         (c) from the definition.
;;         (d) from (c) with fixed D via induction over C.

;; The CPL of a class C is one order of set S(C).
;; If CPL(C) = (... D1 ... D2 ...), one writes D1 < D2.
;; The relation introduced by this is a total order upon S(C).
;; The following set of restrictions has to be taken into account:
;; R(C) := union_{D in S(C)} DR(D)  with
;; DR(C) := { C < C1, C1 < C2, ..., C{n-1} < C_n } if DS(C) = (C1, ..., Cn).
;; If R(C) contains a cycle, R(C) cannot be completed into a total order,
;; of course. Then, R(C) is called inconsistent.
;; CPL(C) is constructed as follows:
;;   L := (), R := R(C).
;;   L := (L | C), remove all (C < ..) from R.
;;   while R /= {}, deal with the set M of all minimal elements of R
;;     (those classes, that can be added to L without violating R(C) ).
;;     If M is empty, then there is a cycle in R(C) and
;;     the algorithm is finished. Else, choose that element among the
;;     elements E of M, which has a D being rightmost in L with
;;     E in DS(D) .
;;     L := (L | E), remove all (E < ..) from R.
;;   CPL(C) := L.
;; L is lengthened stepwise by one element, R is shortened stepwise,
;; and R always consists solely of relations between elements
;; of S(C)\L.

;; Lemma 2: (a) CPL(C) = (C ...).
;;          (b) If DS(C) = (C1, ..., Cn), then
;;              CPL(C) = (C ... C1 ... C2 ... ... Cn ...).
;; proof:  (a) obvious by construction.
;;         (b) If Ci is added to the CPL, then the restriction
;;             C{i-1} < Ci can no longer be in R, so C{i-1} must already be
;;             in the CPL.

;; The following statement is wrong:
;; (*) If D is in DS(C) and CPL(D) = (D1, ..., Dn), then
;;     CPL(C) = (C ... D1 ... D2 ... ... Dn ...).
;; Example:
;;     z
;;    /|\             CPL(z) = (z)
;;   / | \            CPL(x) = (x z)
;;  x  |  x           CPL(y) = (y z)
;;  |  |  |           CPL(d) = (d x z)
;;  d  y  e           CPL(e) = (e x z)
;;   \/ \/            CPL(b) = (b d x y z)
;;   b   c            CPL(c) = (c y e x z)
;;    \ /             CPL(a) = (a b d c y e x z)
;;     a
;;                    CPL(a) does not contain CPL(b) !

#||
 (defclass z () ())
 (defclass x (z) ())
 (defclass y (z) ())
 (defclass d (x z) ())
 (defclass e (x z) ())
 (defclass b (d y) ())
 (defclass c (y e) ())
 (defclass a (b c) ())
 (mapcar #'find-class '(z x y d e b c a))
||#

(defun std-compute-cpl (class direct-superclasses)
  (let* ((superclasses ; list of all superclasses in any order
          (remove-duplicates
           (mapcap #'class-precedence-list direct-superclasses)))
         (L '())
         (R1 (list (cons class direct-superclasses)))
         (R2 (mapcar #'(lambda (D) (cons D (class-direct-superclasses D)))
                     superclasses)))
    (loop
      ;; L is the reversed, so far constructed CPL.
      ;; R1 is the list of the so far relevant restrictions, in the form
      ;; R1 = (... (Dj ... Dn) ...) if from DR(D) = (D1 ... Dn) only
      ;; Dj,...,Dn is left over. The order in R1 corresponds to that in L.
      ;; R2 is the list of all so far irrelevant restrictions.
      (when (null R1)
        (return)) ; R1 = R2 = () -> finished
      (let ((M (remove-duplicates (mapcar #'first R1) :from-end t)))
        (setq M (remove-if #'(lambda (E)
                               (or (dolist (r R1 nil)
                                     (when (member E (cdr r)) (return t)))
                                   (dolist (r R2 nil)
                                     (when (member E (cdr r)) (return t)))))
                           (the list M)))
        (when (null M)
          (error-of-type 'error
            (TEXT "~S ~S: inconsistent precedence graph, cycle ~S")
            'defclass (class-classname class)
            ;; find cycle: advance to ever smaller elements
            ;; with aid of the restrictions.
            (let* ((R0 (append R1 R2))
                   (cycle (list (car (first R0)))))
              (loop
                (let* ((last (car cycle))
                       (next (dolist (r R0 nil)
                               (when (member last (cdr r))
                                 (return (nth (position last (cdr r)) r))))))
                  (when (null next)
                    ;; last is now apparently a minimal element, after all!
                    (return '??))
                  (when (member next cycle)
                    (setf (cdr (member next cycle)) nil)
                    (return cycle))
                  (push next cycle))))))
        (let ((E (first M)))
          (push E L)
          (push (assoc E R2) R1)
          (setq R2 (delete E R2 :key #'first))
          (mapl #'(lambda (r) (when (eq (first (car r)) E) (pop (car r)))) R1)
          (setq R1 (delete-if #'null R1)))))
    (setq L (nreverse L))
    ;; Test, if L is compatible with the CPL(D), D in direct-superclasses:
    (mapc #'(lambda (D)
              (unless ; Is (class-precedence-list D) sublist of L ?
                  (do ((CL L)
                       (DL (class-precedence-list D) (cdr DL)))
                      ((null DL) t)
                    (when (null (setq CL (member (car DL) CL))) (return nil)))
                (warn (TEXT "(class-precedence-list ~S) and (class-precedence-list ~S) are inconsistent")
                      class D)))
          direct-superclasses)
    L))

(defun compute-class-precedence-list-<class> (class)
  (std-compute-cpl class (class-direct-superclasses class)))

;; Preliminary.
(defun compute-class-precedence-list (class)
  (compute-class-precedence-list-<class> class))

(defun checked-compute-class-precedence-list (class)
  (let ((cpl (compute-class-precedence-list class))
        (name (class-name class)))
    ; Some checks, to guarantee that user-defined methods on
    ; compute-class-precedence-list don't break our CLOS.
    (unless (proper-list-p cpl)
      (error (TEXT "Wrong ~S result for class ~S: not a proper list: ~S")
             'compute-class-precedence-list name cpl))
    (dolist (c cpl)
      (unless (class-p c)
        (error (TEXT "Wrong ~S result for class ~S: list element is not a class: ~S")
               'compute-class-precedence-list name c)))
    (unless (eq (first cpl) class)
      (error (TEXT "Wrong ~S result for class ~S: list doesn't start with the class itself: ~S")
             'compute-class-precedence-list name cpl))
    (unless (or (eq name 't) ; for bootstrapping
                (eq (car (last cpl)) <t>))
      (error (TEXT "Wrong ~S result for class ~S: list doesn't end with ~S: ~S")
             'compute-class-precedence-list name <t> cpl))
    (unless (= (length cpl) (length (remove-duplicates cpl :test #'eq)))
      (error (TEXT "Wrong ~S result for class ~S: list contains duplicates: ~S")
             'compute-class-precedence-list name cpl))
    (let ((superclasses (reduce #'union
                                (mapcar #'class-precedence-list
                                        (class-direct-superclasses class))
                                :initial-value '())))
      (let ((forgotten (set-difference superclasses cpl)))
        (when forgotten
          (error (TEXT "Wrong ~S result for class ~S: list doesn't contain the superclass~[~;~:;es~] ~{~S~^, ~}.")
                 'compute-class-precedence-list name (length forgotten) forgotten)))
      (let ((extraneous (set-difference (rest cpl) superclasses)))
        (when extraneous
          (error (TEXT "Wrong ~S result for class ~S: list contains elements that are not superclasses: ~{~S~^, ~}")
                 'compute-class-precedence-list name extraneous))))
    ; Now we've checked the CPL is OK.
    cpl))

;; Stuff all superclasses (from the precedence-list) into a hash-table.
(defun std-compute-superclasses (precedence-list)
  (let ((ht (make-hash-table :key-type 'class :value-type '(eql t)
                             :test 'ext:stablehash-eq :warn-if-needs-rehash-after-gc t)))
    (mapc #'(lambda (superclass) (setf (gethash superclass ht) t))
          precedence-list)
    ht))

;; Determine whether a class inherits from <standard-stablehash> or
;; <structure-stablehash>.
(defun std-compute-subclass-of-stablehash-p (class)
  (dolist (superclass (class-precedence-list class) nil)
    (let ((superclassname (class-classname superclass)))
      (when (or (eq superclassname 'standard-stablehash)
                (eq superclassname 'structure-stablehash))
        (return t)))))

;; ----------------------------------------------------------------------------
;; CLtL2 28.1.3.2., ANSI CL 7.5.3. Inheritance of Slots and Slot Options

(defun compute-effective-slot-definition-<class> (class name directslotdefs)
  (let* ((args
           `(:name ,name
             ; "The allocation of a slot is controlled by the most
             ;  specific slot specifier."
             :allocation ,(slot-definition-allocation (first directslotdefs))
             ; "The set of initialization arguments that initialize a
             ;  given slot is the union of the initialization arguments
             ;  declared in the :initarg slot options in all the slot
             ;  specifiers.
             ,@(let ((initargs
                       (remove-duplicates
                         (mapcap #'slot-definition-initargs directslotdefs)
                         :from-end t)))
                 (if initargs `(:initargs ,initargs)))
             ; "The default initial value form for a slot is the value
             ;  of the :initform slot option in the most specific slot
             ;  specifier that contains one."
             ,@(dolist (slot directslotdefs '())
                 (when (slot-definition-initfunction slot)
                   (return `(:initform ,(slot-definition-initform slot)
                             :initfunction ,(slot-definition-initfunction slot)
                             inheritable-initer ,(slot-definition-inheritable-initer slot)))))
             ; "The contents of a slot will always be of type
             ;  (and T1 ... Tn) where T1 ...Tn are the values of the
             ;  :type slot options contained in all of the slot specifiers."
             ,@(let ((types '()))
                 (dolist (slot directslotdefs)
                   (push (slot-definition-type slot) types))
                 `(:type ,(if types `(AND ,@(nreverse types)) 'T)))
             ; "The documentation string for a slot is the value of the
             ;  :documentation slot option in the most specific slot
             ;  specifier that contains one."
             ,@(dolist (slot directslotdefs '())
                 (when (slot-definition-documentation slot)
                   (return `(:documentation ,(slot-definition-documentation slot)
                             inheritable-doc ,(slot-definition-inheritable-doc slot)))))
             #|| ; Commented out because <effective-slot-definition>
                 ; doesn't have readers and writers.
             ,@(let ((readers
                       (mapcap #'slot-definition-readers directslotdefs)))
                 (if readers `(:readers ,readers)))
             ,@(let ((writers
                       (mapcap #'slot-definition-writers directslotdefs)))
                 (if writers `(:writers ,writers)))
             ||#
             ))
         (slot-definition-class
           (apply #'effective-slot-definition-class class args)))
    (cond ((semi-standard-class-p class)
           (unless (or ; for bootstrapping
                       (eq slot-definition-class 'standard-effective-slot-definition)
                       (and (class-p slot-definition-class)
                            (subclassp slot-definition-class <standard-effective-slot-definition>)))
             (error (TEXT "Wrong ~S result for class ~S: not a subclass of ~S: ~S")
                    'effective-slot-definition-class (class-name class)
                    'standard-effective-slot-definition slot-definition-class)))
          ((structure-class-p class)
           (unless (and (class-p slot-definition-class)
                        (subclassp slot-definition-class <structure-effective-slot-definition>))
             (error (TEXT "Wrong ~S result for class ~S: not a subclass of ~S: ~S")
                    'effective-slot-definition-class (class-name class)
                    'structure-effective-slot-definition slot-definition-class))))
    (apply (cond ((eq slot-definition-class 'standard-effective-slot-definition)
                  #'make-instance-<standard-effective-slot-definition>)
                 (t #'make-instance))
           slot-definition-class args)))

;; Preliminary.
(defun compute-effective-slot-definition (class slotname direct-slot-definitions)
  (compute-effective-slot-definition-<class> class slotname direct-slot-definitions))

(defun compute-slots-<class>-primary (class)
  ;; Gather all slot-specifiers, ordered by precedence:
  (let ((all-slots
          (mapcan #'(lambda (c) (nreverse (copy-list (class-direct-slots c))))
                  (class-precedence-list class))))
    ;; Partition by slot-names:
    (setq all-slots
          (let ((ht (make-hash-table :key-type 'symbol :value-type 't
                                     :test 'ext:stablehash-eql :warn-if-needs-rehash-after-gc t)))
            (dolist (slot all-slots)
              (let ((slot-name (slot-definition-name slot)))
                (push slot (gethash slot-name ht nil))))
            (let ((L nil))
              (maphash #'(lambda (name slot-list)
                           (push (cons name (nreverse slot-list)) L))
                       ht)
              L))) ; not (nreverse L), because maphash reverses the order
    ;; Bring the slots into final order: Superclass before subclass, and
    ;; inside each class, keeping the same order as in the direct-slots.
    (setq all-slots (nreverse all-slots))
    ;; all-slots is now a list of lists of the form
    ;; (name most-specific-slot ... least-specific-slot).
    (mapcar
      #'(lambda (slotbag)
          (let ((name (car slotbag))
                (directslotdefs (cdr slotbag)))
            ;; Create the effective slot definition in a way that depends
            ;; only on the class, name, and direct-slot-definitions.
            (let ((eff-slot
                    (compute-effective-slot-definition class name directslotdefs)))
              ; Some checks, to guarantee that user-defined methods on
              ; compute-effective-slot-definition don't break our CLOS.
              (unless (effective-slot-definition-p eff-slot)
                (error (TEXT "Wrong ~S result for class ~S, slot ~S: not an ~S instance: ~S")
                       'compute-effective-slot-definition class name 'effective-slot-definition eff-slot))
              eff-slot)))
      all-slots)))

;; Allocation of local and shared slots.
;; Side effects done by this function: The slot-definition-location of the
;; slots is determined.
(defun compute-slots-<slotted-class>-around (class next-method)
  (let ((cpl (class-precedence-list class))
        (slots (funcall next-method class)))
    ; Some checks, to guarantee that user-defined primary methods on
    ; compute-slots don't break our CLOS.
    (unless (proper-list-p slots)
      (error (TEXT "Wrong ~S result for class ~S: not a proper list: ~S")
             'compute-slots (class-name class) slots))
    (cond ((semi-standard-class-p class)
           (dolist (slot slots)
             (unless (standard-effective-slot-definition-p slot)
               (error (TEXT "Wrong ~S result for class ~S: list element is not a ~S: ~S")
                      'compute-slots (class-name class)
                      'standard-effective-slot-definition slot))))
          ((structure-class-p class)
           (dolist (slot slots)
             (unless (typep slot <structure-effective-slot-definition>)
               (error (TEXT "Wrong ~S result for class ~S: list element is not a ~S: ~S")
                      'compute-slots (class-name class)
                      'structure-effective-slot-definition slot)))))
    (unless (= (length slots)
               (length (delete-duplicates (mapcar #'slot-definition-name slots))))
      (error (TEXT "Wrong ~S result for class ~S: list contains duplicate slot names: ~S")
             'compute-slots (class-name class) slots))
    ;; Implementation of fixed-slot-locations policy.
    (let ((superclasses-with-fixed-slot-locations
            (remove-if-not #'(lambda (c)
                               (and (semi-standard-class-p c)
                                    (class-fixed-slot-locations c)))
                           (cdr (class-precedence-list class)))))
      (when superclasses-with-fixed-slot-locations
        (dolist (slot slots)
          (let ((name (slot-definition-name slot))
                (location nil))
            (dolist (superclass superclasses-with-fixed-slot-locations)
              (let ((slot-in-superclass (find name (class-slots superclass)
                                              :key #'slot-definition-name)))
                (when slot-in-superclass
                  (when (eq (slot-definition-allocation slot-in-superclass) ':instance)
                    (let ((guaranteed-location
                            (slot-definition-location slot-in-superclass)))
                      (assert (integerp guaranteed-location))
                      (if location
                        (unless (equal location guaranteed-location)
                          (error (TEXT "In class ~S, the slot ~S is constrained by incompatible constraints inherited from the superclasses.")
                                 (class-name class) name))
                        (setq location guaranteed-location)))))))
            (when location
              (unless (eq (slot-definition-allocation slot) ':instance)
                (error (TEXT "In class ~S, non-local slot ~S is constrained to be a local slot at offset ~S.")
                       (class-name class) name location))
              (setf (slot-definition-location slot) location))))))
    (let ((constrained-indices
            (let ((constrained-slots (remove-if-not #'slot-definition-location slots)))
              (setq constrained-slots (copy-list constrained-slots))
              (setq constrained-slots (sort constrained-slots #'< :key #'slot-definition-location))
              (do ((l constrained-slots (cdr l)))
                  ((null (cdr l)))
                (when (= (slot-definition-location (car l)) (slot-definition-location (cadr l)))
                  (error (TEXT "In class ~S, the slots ~S and ~S are constrained from the superclasses to both be located at offset ~S.")
                         (class-name class)
                         (slot-definition-name (car l)) (slot-definition-name (cadr l))
                         (slot-definition-location (car l)))))
              (mapcar #'slot-definition-location constrained-slots)))
          (local-index (class-instance-size class))
          (shared-index 0))
      ;; Actually the constrained-indices must form a list of consecutive indices
      ;; (1 2 ... n), but we don't need to make use of this.
      ;; Now determine the location of each slot.
      (when (and constrained-indices (< (first constrained-indices) local-index))
        (error (TEXT "In class ~S, a slot constrained from a superclass wants to be located at offset ~S, which is impossible.")
               (class-name class) (first constrained-indices)))
      (flet ((skip-constrained-indices ()
               (loop
                 (if (and constrained-indices
                          (= (first constrained-indices) local-index))
                   (progn (incf local-index) (pop constrained-indices))
                   (return)))))
        (skip-constrained-indices)
        (dolist (slot slots)
          (let ((name (slot-definition-name slot))
                (allocation (slot-definition-allocation slot)))
            (setf (slot-definition-location slot)
                  (cond ((eq allocation ':instance)
                         ;; Local slot.
                         (or (slot-definition-location slot)
                             (prog1
                               local-index
                               (incf local-index)
                               (skip-constrained-indices))))
                        ((eq allocation ':class)
                         ;; Shared slot.
                         ;; This is a flaw in the compute-slots protocol: the
                         ;; primary compute-slots method returns a list of slots,
                         ;; without information about the class where the slot
                         ;; comes from. So we have to re-scan the direct slots
                         ;; lists.
                         (let ((origin
                                 (dolist (superclass cpl class)
                                   (when (find name (class-direct-slots superclass)
                                               :key #'slot-definition-name)
                                     (return superclass)))))
                           (if (eq origin class)
                             ;; New shared slot.
                             (prog1
                               (cons (class-current-version class) shared-index)
                               (incf shared-index))
                             ;; Inherited shared slot.
                             (let ((inh-descriptor
                                     (gethash name (class-slot-location-table origin))))
                               (if (effective-slot-definition-p inh-descriptor)
                                 (slot-definition-location inh-descriptor)
                                 inh-descriptor)))))
                        (t ;; Don't signal an error for user-defined allocation
                           ;; types. They can be handled by user-defined around
                           ;; methods.
                           nil))))))
      ;; Actually the constrained-indices must already have been emptied by
      ;; the first (skip-constrained-indices) call, but we don't need to make
      ;; use of this. Warn if :fixed-slot-locations would cause a waste of
      ;; space.
      (when constrained-indices
        (setq local-index (1+ (car (last constrained-indices))))
        (warn (TEXT "In class ~S, constrained slot locations cause holes to appear.")
              (class-name class)))
      slots)))

;; Preliminary.
(defun compute-slots (class)
  (compute-slots-<slotted-class>-around class #'compute-slots-<class>-primary))

(defun checked-compute-slots (class)
  (let ((slots (compute-slots class)))
    ; Some checks, to guarantee that user-defined around methods on
    ; compute-slots don't break our CLOS.
    (unless (proper-list-p slots)
      (error (TEXT "Wrong ~S result for class ~S: not a proper list: ~S")
             'compute-slots (class-name class) slots))
    (dolist (slot slots)
      (unless (standard-effective-slot-definition-p slot)
        (error (TEXT "Wrong ~S result for class ~S: list element is not a ~S: ~S")
               'compute-slots (class-name class)
               'standard-effective-slot-definition slot)))
    (unless (= (length slots)
               (length (delete-duplicates (mapcar #'slot-definition-name slots))))
      (error (TEXT "Wrong ~S result for class ~S: list contains duplicate slot names: ~S")
             'compute-slots (class-name class) slots))
    (dolist (slot slots)
      (unless (slot-definition-location slot)
        (error (TEXT "Wrong ~S result for class ~S: not slot location has been assigned to ~S")
               'compute-slots (class-name class) slot)))
    slots))

;; The MOP lacks a way to customize the instance size as a function of the
;; slots. This becomes an issue when you have slots which occupy more than one
;; word, and such a slot is the last local slot.
(defun compute-instance-size (class)
  (let ((size (class-instance-size class))) ; initial size depends on the metaclass
    (dolist (slot (class-slots class))
      (when (eq (slot-definition-allocation slot) ':instance)
        (let ((location (slot-definition-location slot)))
          (assert (integerp location))
          (setq size (max size (+ location 1))))))
    size))

;; Similarly, the MOP lacks a way to customize the shared slot values vector's
;; size as a function of the slots.
(defun compute-shared-size (class)
  (let ((shared-size 0))
    (dolist (slot (class-slots class))
      (let ((location (slot-definition-location slot)))
        (when (and (consp location) (eq (cv-newest-class (car location)) class))
          (let ((shared-index (cdr location)))
            (setq shared-size (max shared-size (+ shared-index 1)))))))
    shared-size))

;; Creates the shared slot values vector for a class.
(defun create-shared-slots-vector (class shared-size old-slot-location-table)
  (let ((v (make-array shared-size :initial-element 'DEADBEEF)))
    (dolist (slot (class-slots class))
      (let ((location (slot-definition-location slot)))
        (when (and (consp location)
                   (eq (cv-newest-class (car location)) class))
          (let ((shared-index (cdr location)))
            (setf (svref v shared-index)
                  (let* ((old-slot-descriptor
                           (gethash (slot-definition-name slot) old-slot-location-table))
                         (old-slot-location
                           (if (effective-slot-definition-p old-slot-descriptor)
                             (slot-definition-location old-slot-descriptor)
                             old-slot-descriptor)))
                    (if (and (consp old-slot-location)
                             (eq (cv-newest-class (car old-slot-location)) class))
                      ;; The slot was already shared. Retain its value.
                      (svref (cv-shared-slots (car old-slot-location))
                             (cdr old-slot-location))
                      ;; A new shared slot.
                      (let ((initfunction (slot-definition-initfunction slot)))
                        (if initfunction
                          (funcall initfunction)
                          (sys::%unbound))))))))))
    v))

(defun compute-slot-location-table (class)
  (let ((slots (class-slots class)))
    (if slots
      (make-hash-table
        :key-type 'symbol :value-type 't
        :test 'ext:stablehash-eq :warn-if-needs-rehash-after-gc t
        :initial-contents
          (mapcar #'(lambda (slot)
                      (cons (slot-definition-name slot)
                            (compute-slot-location-table-entry class slot)))
                  slots))
      empty-ht)))

(defun compute-slot-location-table-entry (class slot)
  (let ((location (slot-definition-location slot))
        ;; Compute the effective methods of SLOT-VALUE-USING-CLASS etc.
        ;; Note that we cannot use (class-prototype class) yet.
        ;; Also, SLOT-VALUE-USING-CLASS etc. are not defined on STRUCTURE-CLASS.
        (efm-svuc
          (if (and (semi-standard-class-p class) *classes-finished*)
            (compute-applicable-methods-effective-method-for-set
              |#'slot-value-using-class|
              (list `(EQL ,class) `(INSTANCE-OF-P ,class) `(EQL ,slot))
              (list class '`(CLASS-PROTOTYPE ,class) slot))
            #'%slot-value-using-class))
        (efm-ssvuc
          (if (and (semi-standard-class-p class) *classes-finished*)
            (compute-applicable-methods-effective-method-for-set
              |#'(setf slot-value-using-class)|
              (list `(TYPEP ,<t>) `(EQL ,class) `(INSTANCE-OF-P ,class) `(EQL ,slot))
              (list 'ANY-VALUE class '`(CLASS-PROTOTYPE ,class) slot))
            #'%set-slot-value-using-class))
        (efm-sbuc
          (if (and (semi-standard-class-p class) *classes-finished*)
            (compute-applicable-methods-effective-method-for-set
              |#'slot-boundp-using-class|
              (list `(EQL ,class) `(INSTANCE-OF-P ,class) `(EQL ,slot))
              (list class '`(CLASS-PROTOTYPE ,class) slot))
            #'%slot-boundp-using-class))
        (efm-smuc
          (if (and (semi-standard-class-p class) *classes-finished*)
            (compute-applicable-methods-effective-method-for-set
              |#'slot-makunbound-using-class|
              (list `(EQL ,class) `(INSTANCE-OF-P ,class) `(EQL ,slot))
              (list class '`(CLASS-PROTOTYPE ,class) slot))
            #'%slot-makunbound-using-class)))
    (setf (slot-definition-efm-svuc slot) efm-svuc)
    (setf (slot-definition-efm-ssvuc slot) efm-ssvuc)
    (setf (slot-definition-efm-sbuc slot) efm-sbuc)
    (setf (slot-definition-efm-smuc slot) efm-smuc)
    (if (and (eq efm-svuc #'%slot-value-using-class)
             (eq efm-ssvuc #'%set-slot-value-using-class)
             (eq efm-sbuc #'%slot-boundp-using-class)
             (eq efm-smuc #'%slot-makunbound-using-class))
      location
      slot)))

;; ----------------------------------------------------------------------------
;; CLtL2 28.1.3.3., ANSI CL 4.3.4.2. Inheritance of Default-Initargs

(defun compute-default-initargs-<class> (class)
  (remove-duplicates
    (mapcap #'class-direct-default-initargs (class-precedence-list class))
    :key #'car
    :from-end t))

;; Preliminary.
(defun compute-default-initargs (class)
  (compute-default-initargs-<class> class))

(defun checked-compute-default-initargs (class)
  (let ((default-initargs (compute-default-initargs class)))
    ; Some checks, to guarantee that user-defined methods on
    ; compute-default-initargs don't break our CLOS.
    (unless (proper-list-p default-initargs)
      (error (TEXT "Wrong ~S result for class ~S: not a proper list: ~S")
             'compute-default-initargs (class-name class) default-initargs))
    (dolist (di default-initargs)
      (unless (canonicalized-default-initarg-p di)
        (error (TEXT "Wrong ~S result for class ~S: list element is not a canonicalized default initarg: ~S")
               'compute-default-initargs (class-name class) di)))
    (unless (= (length default-initargs)
               (length (delete-duplicates (mapcar #'first default-initargs))))
      (error (TEXT "Wrong ~S result for class ~S: list contains duplicate initarg names: ~S")
             'compute-default-initargs (class-name class) default-initargs))
    default-initargs))

;; ----------------------------- Accessor Methods -----------------------------

;; Flag to avoid bootstrapping issues with the compiler.
(defvar *compile-accessor-functions* nil)

;; Install the accessor methods corresponding to the direct slots of a class.
(defun install-class-direct-accessors (class)
  (dolist (slot (class-direct-slots class))
    (let ((slot-name (slot-definition-name slot))
          (readers (slot-definition-readers slot))
          (writers (slot-definition-writers slot)))
      (when (or readers writers)
        (let ((generic-p (class-generic-accessors class))
              (access-place
                (let (effective-slot)
                  (if (and (semi-standard-class-p class)
                           (class-fixed-slot-locations class)
                           (setq effective-slot
                                 (find slot-name (class-slots class)
                                       :key #'slot-definition-name))
                           (eq (slot-definition-allocation effective-slot)
                               ':instance))
                    (progn
                      (assert (typep (slot-definition-location effective-slot) 'integer))
                      `(STANDARD-INSTANCE-ACCESS OBJECT ,(slot-definition-location effective-slot)))
                    `(SLOT-VALUE OBJECT ',slot-name)))))
          ; Generic accessors are defined as methods and listed in the
          ; direct-accessors list, so they can be removed upon class redefinition.
          ; Non-generic accessors are defined as plain functions.
          (dolist (funname readers)
            (if generic-p
              (setf (class-direct-accessors class)
                    (list* funname
                           (do-defmethod funname
                             (let* ((args
                                      (list
                                        :specializers (list class)
                                        'initfunction
                                          (eval
                                            `#'(LAMBDA (#:SELF)
                                                 (DECLARE (COMPILE))
                                                 (%OPTIMIZE-FUNCTION-LAMBDA (T) (#:CONTINUATION OBJECT)
                                                   (DECLARE (COMPILE))
                                                   ,access-place)))
                                        'wants-next-method-p t
                                        :qualifiers nil
                                        :lambda-list '(OBJECT)
                                        'signature (sys::memoized (make-signature :req-num 1))
                                        :slot-definition slot))
                                    (method-class
                                      (apply #'reader-method-class class slot args)))
                               (unless (and (class-p method-class)
                                            (subclassp method-class <standard-reader-method>))
                                 (error (TEXT "Wrong ~S result for class ~S: not a subclass of ~S: ~S")
                                        'reader-method-class (class-name class) 'standard-reader-method method-class))
                               (apply #'make-instance method-class args)))
                           (class-direct-accessors class)))
              (setf (fdefinition funname)
                    (eval `(FUNCTION ,funname
                             (LAMBDA (OBJECT)
                               ,@(if *compile-accessor-functions* '((DECLARE (COMPILE))))
                               (UNLESS (TYPEP OBJECT ',class)
                                 (ERROR-ACCESSOR-TYPECHECK ',funname OBJECT ',class))
                               ,access-place))))))
          (dolist (funname writers)
            (if generic-p
              (setf (class-direct-accessors class)
                    (list* funname
                           (do-defmethod funname
                             (let* ((args
                                      (list
                                        :specializers (list <t> class)
                                        'initfunction
                                          (eval
                                            `#'(LAMBDA (#:SELF)
                                                 (DECLARE (COMPILE))
                                                 (%OPTIMIZE-FUNCTION-LAMBDA (T) (#:CONTINUATION NEW-VALUE OBJECT)
                                                   (DECLARE (COMPILE))
                                                   (SETF ,access-place NEW-VALUE))))
                                        'wants-next-method-p t
                                        :qualifiers nil
                                        :lambda-list '(NEW-VALUE OBJECT)
                                        'signature (sys::memoized (make-signature :req-num 2))
                                        :slot-definition slot))
                                    (method-class
                                      (apply #'writer-method-class class slot args)))
                               (unless (and (class-p method-class)
                                            (subclassp method-class <standard-writer-method>))
                                 (error (TEXT "Wrong ~S result for class ~S: not a subclass of ~S: ~S")
                                        'writer-method-class (class-name class) 'standard-writer-method method-class))
                               (apply #'make-instance method-class args)))
                           (class-direct-accessors class)))
              (setf (fdefinition funname)
                    (eval `(FUNCTION ,funname
                             (LAMBDA (NEW-VALUE OBJECT)
                               ,@(if *compile-accessor-functions* '((DECLARE (COMPILE))))
                               (UNLESS (TYPEP OBJECT ',class)
                                 (ERROR-ACCESSOR-TYPECHECK ',funname OBJECT ',class))
                               (SETF ,access-place NEW-VALUE))))))))))))

;; Remove a set of accessor methods given as a plist.
(defun remove-accessor-methods (plist)
  (do ((l plist (cddr l)))
      ((endp l))
    (let ((funname (car l))
          (method (cadr l)))
      (remove-method (fdefinition funname) method))))

;; Add a set of accessor methods given as a plist.
(defun add-accessor-methods (plist)
  (do ((l plist (cddr l)))
      ((endp l))
    (let ((funname (car l))
          (method (cadr l)))
      (add-method (fdefinition funname) method))))

;; --------------- Creation of an instance of <built-in-class> ---------------

(defun make-instance-<built-in-class> (metaclass &rest args
                                       &key name (direct-superclasses '())
                                       &allow-other-keys)
  ;; metaclass = <built-in-class>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore metaclass name direct-superclasses))
  (let ((class (allocate-metaobject-instance *<built-in-class>-class-version*
                                             *<built-in-class>-instance-size*)))
    (apply #'initialize-instance-<built-in-class> class args)))

(defun initialize-instance-<built-in-class> (class &rest args
                                             &key &allow-other-keys)
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'initialize-instance later.
  (apply #'shared-initialize-<built-in-class> class 't args)
  (install-class-direct-accessors class)
  class)

(defun shared-initialize-<built-in-class> (class situation &rest args
                                           &key (name nil name-p)
                                                (direct-superclasses '() direct-superclasses-p)
                                           &allow-other-keys)
  (when (or (eq situation 't) direct-superclasses-p)
    (check-metaclass-mix (if name-p name (class-classname class))
                         direct-superclasses
                         #'built-in-class-p 'built-in-class))
  (apply #'shared-initialize-<class> class situation args)
  ; Initialize the remaining <class> slots:
  (when (or (eq situation 't) direct-superclasses-p)
    (setf (class-precedence-list class)
          (checked-compute-class-precedence-list class))
    (when (eq situation 't)
      (setf (class-initialized class) 3))
    (setf (class-all-superclasses class)
          (std-compute-superclasses (class-precedence-list class)))
    (when (eq situation 't)
      (setf (class-initialized class) 4)))
  (when (eq situation 't)
    (setf (class-slots class) '())
    (setf (class-initialized class) 5)
    (setf (class-default-initargs class) '())
    (setf (class-initialized class) 6))
  ; Done.
  class)

;; --------------- Creation of an instance of <structure-class> ---------------

(defun make-instance-<structure-class> (metaclass &rest args
                                        &key name (direct-superclasses '())
                                             ;; The following keys come from ENSURE-CLASS.
                                             ((:direct-slots direct-slots-as-lists) '())
                                             (direct-default-initargs '()) (documentation nil)
                                             ;; The following keys come from DEFINE-STRUCTURE-CLASS.
                                             ((names names) nil)
                                             ((direct-slots direct-slots-as-metaobjects) '())
                                             ((slots slots) '()) ((size size) 1)
                                        &allow-other-keys)
  ;; metaclass = <structure-class>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore metaclass name direct-superclasses direct-slots-as-lists
                   direct-default-initargs documentation
                   names direct-slots-as-metaobjects slots size))
  (let ((class (allocate-metaobject-instance *<structure-class>-class-version*
                                             *<structure-class>-instance-size*)))
    (apply #'initialize-instance-<structure-class> class args)))

(defun initialize-instance-<structure-class> (class &rest args
                                              &key &allow-other-keys)
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'initialize-instance later.
  (apply #'shared-initialize-<structure-class> class 't args)
  (install-class-direct-accessors class)
  class)

(defun shared-initialize-<structure-class> (class situation &rest args
                                            &key (name nil name-p)
                                                 (direct-superclasses '() direct-superclasses-p)
                                                 (generic-accessors t generic-accessors-p)
                                                 ;; The following keys come from ENSURE-CLASS.
                                                 ((:direct-slots direct-slots-as-lists) '() direct-slots-as-lists-p)
                                                 (direct-default-initargs '() direct-default-initargs-p)
                                                 (documentation nil documentation-p)
                                                 ;; The following keys come from DEFINE-STRUCTURE-CLASS.
                                                 ((names names) nil names-p)
                                                 ((direct-slots direct-slots-as-metaobjects) '() direct-slots-as-metaobjects-p)
                                                 ((slots slots) '())
                                                 ((size size) 1)
                                            &allow-other-keys)
  ;; metaclass ⊆ <structure-class>
  (declare (ignore generic-accessors generic-accessors-p direct-slots-as-lists
                   direct-slots-as-metaobjects direct-default-initargs
                   documentation documentation-p))
  (when (or (eq situation 't) direct-superclasses-p)
    (check-metaclass-mix (if name-p name (class-classname class))
                         direct-superclasses
                         #'structure-class-p 'STRUCTURE-CLASS))
  (apply #'shared-initialize-<slotted-class> class situation args)
  (setq direct-superclasses (class-direct-superclasses class)) ; augmented
  ; Initialize the remaining <class> slots:
  (when (or (eq situation 't) direct-superclasses-p)
    (setf (class-precedence-list class)
          (checked-compute-class-precedence-list class))
    (when (eq situation 't)
      (setf (class-initialized class) 3))
    (setf (class-all-superclasses class)
          (std-compute-superclasses (class-precedence-list class)))
    (when (eq situation 't)
      (setf (class-initialized class) 4)))
  (when (or (eq situation 't) direct-superclasses-p
            direct-slots-as-lists-p direct-slots-as-metaobjects-p)
    (unless names
      ;; When called via ENSURE-CLASS, we have to do inheritance of slots.
      (when direct-superclasses
        (setq slots (class-slots (first direct-superclasses)))
        (setq size (class-instance-size (first direct-superclasses)))))
    (setf (class-slots class) slots)
    (when (eq situation 't)
      (setf (class-initialized class) 5))
    (setf (class-slot-location-table class) (compute-slot-location-table class))
    (setf (class-instance-size class) size)
    (unless names
      (setf (class-instance-size class) 1)
      (setf (class-slots class)
            (compute-slots-<slotted-class>-around class
              #'(lambda (c)
                  (append slots
                    (remove-if (let ((slotnames (mapcar #'slot-definition-name slots)))
                                 #'(lambda (slot) (member (slot-definition-name slot) slotnames)))
                               (compute-slots-<class>-primary c))))))
      (setf (class-instance-size class) (max size (compute-instance-size class)))
      (let ((ht (class-slot-location-table class)))
        (dolist (slot (class-slots class))
          (setf (gethash (slot-definition-name slot) ht)
                (slot-definition-location slot))))
      (when (plusp (compute-shared-size class))
        (error-of-type 'error
          (TEXT "(~S ~S): metaclass ~S does not support shared slots")
                'DEFCLASS name 'STRUCTURE-CLASS))))
  (when (or (eq situation 't) direct-superclasses-p direct-default-initargs-p)
    (setf (class-default-initargs class)
          (checked-compute-default-initargs class)))
  (when (eq situation 't)
    (setf (class-initialized class) 6))
  ; Initialize the remaining <slotted-class> slots:
  (when (or (eq situation 't) direct-superclasses-p)
    (setf (class-subclass-of-stablehash-p class)
          (std-compute-subclass-of-stablehash-p class)))
  (when (or (eq situation 't) direct-superclasses-p
            direct-slots-as-lists-p direct-slots-as-metaobjects-p)
    (setf (class-valid-initargs class)
          (remove-duplicates (mapcap #'slot-definition-initargs (class-slots class)))))
  ; Initialize the remaining <structure-class> slots:
  (when (or (eq situation 't) direct-superclasses-p names-p)
    (unless names
      (setq names
            (cons name
                  (if direct-superclasses
                     (class-names (first direct-superclasses))
                     '()))))
    (setf (class-names class) names))
  ; Done.
  (when (eq situation 't)
    (system::note-new-structure-class))
  class)

;; DEFSTRUCT-Hook
(eval-when (compile load eval) (fmakunbound 'define-structure-class))
(defun define-structure-class (name)
  (let ((descr (get name 'sys::defstruct-description)))
    (when descr
      (let* ((names (svref descr 0))
             (all-slots (svref descr 4))
             (slots (remove-if-not #'slot-definition-initargs ; means #'sys::ds-real-slot-p
                                   all-slots))
             (direct-slots (svref descr 5)))
        (setf (find-class name)
              (make-instance-<structure-class> <structure-class>
                :name name
                :direct-superclasses
                  (if (cdr names) (list (find-class (second names))) '())
                'names names
                'direct-slots direct-slots
                'slots slots
                'size (if all-slots
                        (1+ (slot-definition-location (car (last all-slots))))
                        1)
                :generic-accessors nil))))))
(defun undefine-structure-class (name)
  (setf (find-class name) nil))

;; ------------- Creation of an instance of <semi-standard-class> -------------

(defun shared-initialize-<semi-standard-class> (class situation &rest args
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
                   generic-accessors-p))
  (apply #'shared-initialize-<slotted-class> class situation args)
  (when (eq situation 't)
    (setf (class-current-version class)
          (make-class-version :newest-class class
                              :class class
                              :serial 0))
    (unless *classes-finished*
      ; Bootstrapping: Simulate the effect of #'%shared-initialize.
      (setf (class-instantiated class) nil)
      (setf (class-finalized-direct-subclasses-table class) '())))
  ; Initialize the remaining <class> slots:
  (setf (class-initialized class) 2) ; mark as not yet finalized
  (setf (class-precedence-list class) nil) ; mark as not yet finalized
  (setf (class-all-superclasses class) nil) ; mark as not yet finalized
  ; Initialize the remaining <slotted-class> slots:
  ; Initialize the remaining <semi-standard-class> slots:
  (when (or (eq situation 't) fixed-slot-locations-p)
    (setf (class-fixed-slot-locations class) fixed-slot-locations))
  (setf (class-prototype class) nil)
  ; Try to finalize it.
  (finalize-class class nil)
  ; Done.
  class)

;; ------------- Finalizing an instance of <semi-standard-class> -------------

;; Try to finalize a given class, given as a class name or class object.
;; Return the finalized class object on success, or nil when the class could
;; not yet be finalized.
;; When force-p is non-nil, signal an error when finalization is impossible.
;; As a side effect of finalization, symbols in (class-direct-superclasses) are
;; replaced with class objects, and the (class-precedence-list class) is
;; computed.
(defun finalize-class (class
                       &optional force-p
                                 ; The stack of classes being finalized now:
                                 (finalizing-now nil))
  (when (or (class-p class) (setq class (find-class class force-p)))
    (if (>= (class-initialized class) 6) ; already finalized?
      class
      (progn
        ;; Here we get only for instances of STANDARD-CLASS, since instances
        ;; of BUILT-IN-CLASS and STRUCTURE-CLASS are already finalized when
        ;; they are constructed.
        (when (memq class finalizing-now)
          (error-of-type 'program-error
            (TEXT "~S: class definition circularity: ~S depends on itself")
            'defclass class))
        (let ((finalizing-now (cons class finalizing-now)))
          (do ((superclassesr (class-direct-superclasses class) (cdr superclassesr)))
              ((endp superclassesr))
            (let* ((superclass (car superclassesr))
                   (finalized-superclass
                     (finalize-class superclass force-p finalizing-now)))
              (unless finalized-superclass
                ;; Finalization of a superclass was impossible. force-p must
                ;; be nil here, otherwise an error was signaled already. So we
                ;; have to return nil as well.
                (return-from finalize-class nil))
              (when (symbolp superclass) ; changed from symbol to class
                (check-allowed-superclass class finalized-superclass))
              (setf (car superclassesr) finalized-superclass)
              (when (symbolp superclass) ; changed from symbol to class
                (add-direct-subclass finalized-superclass class)))))
        ;; Now compute the class-precedence-list.
        (finalize-instance-semi-standard-class class)
        class))))

(defun finalize-instance-semi-standard-class (class
       &aux (direct-superclasses (class-direct-superclasses class))
            (name (class-name class))
            (old-slot-location-table (class-slot-location-table class)))
  ;; metaclass ⊆ <semi-standard-class>
  (if (standard-class-p class)
    (check-metaclass-mix name direct-superclasses
                         #'standard-class-p 'STANDARD-CLASS)
    (check-metaclass-mix name direct-superclasses
                         #'semi-standard-class-p 'SEMI-STANDARD-CLASS))
  (setf (class-precedence-list class)
        (checked-compute-class-precedence-list class))
  (when (< (class-initialized class) 3)
    (setf (class-initialized class) 3))
  (setf (class-all-superclasses class)
        (std-compute-superclasses (class-precedence-list class)))
  (when (< (class-initialized class) 4)
    (setf (class-initialized class) 4))
  (dolist (super direct-superclasses)
    (when (semi-standard-class-p super)
      (add-finalized-direct-subclass super class)))
  (setf (class-subclass-of-stablehash-p class)
        (std-compute-subclass-of-stablehash-p class))
  (setf (class-funcallablep class)
        ; <funcallable-standard-object> or a subclass of it?
        (if (gethash <function> (class-all-superclasses class)) t nil))
  (setf (class-instance-size class)
        (if (class-funcallablep class)
          3  ; see comments in clos-genfun1.lisp
          1)) ; slot 0 is the class_version pointer
  (setf (class-slots class) (checked-compute-slots class))
  (when (< (class-initialized class) 5)
    (setf (class-initialized class) 5))
  (setf (class-instance-size class) (compute-instance-size class))
  (setf (class-slot-location-table class) (compute-slot-location-table class))
  (let ((shared-size (compute-shared-size class)))
    (when (plusp shared-size)
      (setf (cv-shared-slots (class-current-version class))
            (create-shared-slots-vector class shared-size old-slot-location-table))))
  ;; CLtL2 28.1.3.3., ANSI CL 4.3.4.2. Inheritance of Class Options
  (setf (class-default-initargs class) (checked-compute-default-initargs class))
  (setf (class-valid-initargs class)
        (remove-duplicates (mapcap #'slot-definition-initargs (class-slots class))))
  (when (< (class-initialized class) 6)
    (setf (class-initialized class) 6))
  (system::note-new-standard-class))

;; ------------- Redefining an instance of <semi-standard-class> -------------

;; Preliminary definition.
(defun make-instances-obsolete (class)
  (make-instances-obsolete-<semi-standard-class> class))

(defun make-instances-obsolete-<semi-standard-class> (class)
  (when (>= (class-initialized class) 6) ; nothing to do if not yet finalized
    ;; Recurse to the subclasses. (Even if there are no direct instances of
    ;; this class: the subclasses may have instances.)
    (mapc #'make-instances-obsolete-<semi-standard-class>-nonrecursive
          (list-all-finalized-subclasses class))))

(defun make-instances-obsolete-<semi-standard-class>-nonrecursive (class)
  (if (and (>= (class-initialized class) 4) ; already finalized?
           (subclassp class <metaobject>))
    ; Don't obsolete metaobject instances.
    (let ((name (class-name class))
          (caller *make-instances-obsolete-caller*)
          ;; Rebind *make-instances-obsolete-caller* because WARN may enter a
          ;; nested REP-loop.
          (*make-instances-obsolete-caller* 'make-instances-obsolete))
      (warn (TEXT "~S: Class ~S (or one of its ancestors) is being redefined, but its instances cannot be made obsolete")
            caller name))
    (progn
      (when (class-instantiated class) ; don't warn if there are no instances
        (let ((name (class-name class))
              (caller *make-instances-obsolete-caller*)
              ;; Rebind *make-instances-obsolete-caller* because WARN may enter a
              ;; nested REP-loop.
              (*make-instances-obsolete-caller* 'make-instances-obsolete))
          (if (eq caller 'defclass)
            (warn (TEXT "~S: Class ~S (or one of its ancestors) is being redefined, instances are obsolete")
                  caller name)
            (warn (TEXT "~S: instances of class ~S are made obsolete")
                  caller name))))
      ;; Create a new class-version. (Even if there are no instances: the
      ;; shared-slots may need change.)
      (let* ((copy (copy-standard-class class))
             (old-version (class-current-version copy))
             (new-version
               (make-class-version :newest-class class
                                   :class class
                                   :serial (1+ (cv-serial old-version)))))
        (setf (cv-class old-version) copy)
        (setf (cv-next old-version) new-version)
        (setf (class-current-version class) new-version)))))

;; After a class redefinition, finalize the subclasses so that the instances
;; can be updated.
(defun update-subclasses-for-redefined-class (class was-finalized must-be-finalized old-direct-superclasses)
  (when was-finalized ; nothing to do if not finalized before the redefinition
    ;; Handle the class itself specially, because its superclasses list now is
    ;; not the same as before.
    (setf (class-initialized class) 2) ; mark as not yet finalized
    (setf (class-precedence-list class) nil) ; mark as not yet finalized
    (setf (class-all-superclasses class) nil) ; mark as not yet finalized
    (if must-be-finalized
      ;; The class remains finalized.
      (progn
        (finalize-class class t)
        (let ((new-direct-superclasses (class-direct-superclasses class)))
          (unless (equal old-direct-superclasses new-direct-superclasses)
            (let ((removed-direct-superclasses
                    (set-difference old-direct-superclasses new-direct-superclasses))
                  (added-direct-superclasses
                    (set-difference new-direct-superclasses old-direct-superclasses)))
              (dolist (super removed-direct-superclasses)
                (when (semi-standard-class-p super)
                  (remove-finalized-direct-subclass super class)))
              (dolist (super added-direct-superclasses)
                (when (semi-standard-class-p super)
                  (add-finalized-direct-subclass super class)))))))
      ;; The class becomes unfinalized.
      (dolist (super old-direct-superclasses)
        (when (semi-standard-class-p super)
          (remove-finalized-direct-subclass super class))))
    ;; Now handle the true subclasses.
    (mapc #'update-subclasses-for-redefined-class-nonrecursive
          (rest (list-all-finalized-subclasses class)))))

(defun update-subclasses-for-redefined-class-nonrecursive (class)
  (when (>= (class-initialized class) 6) ; nothing to do if not yet finalized
    (setf (class-initialized class) 2) ; mark as not yet finalized
    (setf (class-precedence-list class) nil) ; mark as not yet finalized
    (setf (class-all-superclasses class) nil) ; mark as not yet finalized
    (if (class-instantiated class)
      ;; The class remains finalized.
      (finalize-class class t)
      ;; The class becomes unfinalized. If it has an instantiated subclass, the
      ;; subclass' finalize-class invocation will re-finalize this one.
      (dolist (super (class-direct-superclasses class))
        (when (semi-standard-class-p super)
          (remove-finalized-direct-subclass super class))))))

;; Store the information needed by the update of obsolete instances in a
;; class-version object. Invoked when an instance needs to be updated.
(defun class-version-compute-slotlists (old-version)
  (let ((old-class (cv-class old-version))
        (new-class (cv-class (cv-next old-version)))
        ; old-class is already finalized - otherwise no instance could exist.
        ; new-class is already finalized, because ensure-class guarantees it.
        (kept2 '())
        (added '())
        (discarded '())
        (discarded2 '()))
    (dolist (old-slot (class-slots old-class))
      (let* ((name (slot-definition-name old-slot))
             (new-slot (find name (class-slots new-class)
                             :test #'eq :key #'slot-definition-name)))
        (if (and new-slot (atom (slot-definition-location new-slot)))
          ;; Local slot remains local, or shared slot becomes local.
          (setq kept2 (list* (slot-definition-location old-slot)
                             (slot-definition-location new-slot)
                             kept2))
          (if (atom (slot-definition-location old-slot))
            ;; Local slot is discarded or becomes shared.
            (setq discarded (cons name discarded)
                  discarded2 (list* name (slot-definition-location old-slot) discarded2))))))
    (dolist (new-slot (class-slots new-class))
      (let* ((name (slot-definition-name new-slot))
             (old-slot (find name (class-slots old-class)
                             :test #'eq :key #'slot-definition-name)))
        (unless old-slot
          ;; Newly added local slot.
          (setq added (cons name added)))))
    (setf (cv-kept-slot-locations old-version) kept2)
    (setf (cv-added-slots old-version) added)
    (setf (cv-discarded-slots old-version) discarded)
    (setf (cv-discarded-slot-locations old-version) discarded2)
    (setf (cv-slotlists-valid-p old-version) t)))

;; -------------- Auxiliary functions for <semi-standard-class> --------------

;;; Maintaining the weak references to the finalized direct subclasses.
;;; (We need only the finalized subclasses, because:
;;;  - The only use of these references is for make-instances-obsolete and for
;;;    update-subclasses-for-redefined-class.
;;;  - A non-finalized class cannot have instances.
;;;  - Without an instance one cannot even access the shared slots.)

;;; The finalized-direct-subclasses slot can be either
;;; - NIL or a weak-list (for saving memory when there are few subclasses), or
;;; - a weak-hash-table (for speed when there are many subclasses).

#|
;; Adds a class to the list of direct subclasses.
(defun add-finalized-direct-subclass (class subclass) ...)
;; Removes a class from the list of direct subclasses.
(defun remove-finalized-direct-subclass (class subclass) ...)
;; Returns the currently existing direct subclasses, as a freshly consed list.
(defun list-finalized-direct-subclasses (class) ...)
|#
(def-weak-set-accessors class-finalized-direct-subclasses-table class
  add-finalized-direct-subclass
  remove-finalized-direct-subclass
  list-finalized-direct-subclasses)

;; Returns the currently existing finalized subclasses, in top-down order,
;; including the class itself as first element.
(defun list-all-finalized-subclasses (class)
  ; Use a breadth-first search which removes duplicates.
  (let ((as-list '())
        (as-set (make-hash-table :key-type 'class :value-type '(eql t)
                                 :test 'ext:stablehash-eq :warn-if-needs-rehash-after-gc t
                                 :rehash-size 2s0))
        (pending (list class)))
    (loop
      (unless pending (return))
      (let ((new-pending '()))
        (dolist (class pending)
          (unless (gethash class as-set)
            (push class as-list)
            (setf (gethash class as-set) t)
            (setq new-pending
              (nreconc (if (semi-standard-class-p class)
                         ; <semi-standard-class> stores the finalized direct-subclasses.
                         (list-finalized-direct-subclasses class)
                         ; <class> stores only the complete direct-subclasses list.
                         (remove-if-not #'(lambda (c) (= (class-initialized c) 6))
                                        (list-direct-subclasses class)))
                       new-pending))))
        (setq pending (nreverse new-pending))))
    ;; Now reorder the list so that superclasses come before, not after, a
    ;; class. This is needed by update-subclasses-for-redefined-class. (It's
    ;; a "topological sorting" algorithm w.r.t. to the superclass relation.)
    (let ((tsorted-list '()))
      (labels ((add-with-superclasses-first (cls)
                 (when (gethash cls as-set)
                   (remhash cls as-set)
                   (dolist (supercls (class-direct-superclasses cls))
                     (add-with-superclasses-first supercls))
                   (push cls tsorted-list))))
        (mapc #'add-with-superclasses-first as-list))
      (setq tsorted-list (nreverse tsorted-list))
      (assert (eq (first tsorted-list) class))
      tsorted-list)))

;; --------------- Creation of an instance of <standard-class> ---------------

(defun make-instance-<standard-class> (metaclass &rest args
                                       &key name
                                            (direct-superclasses '())
                                            (direct-slots '())
                                            (direct-default-initargs '())
                                       &allow-other-keys)
  ;; metaclass = <standard-class>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore metaclass name direct-superclasses direct-slots
                   direct-default-initargs))
  (let ((class (allocate-metaobject-instance *<standard-class>-class-version*
                                             *<standard-class>-instance-size*)))
    (apply #'initialize-instance-<standard-class> class args)))

(defun initialize-instance-<standard-class> (class &rest args
                                             &key &allow-other-keys)
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'initialize-instance later.
  (apply #'shared-initialize-<standard-class> class 't args)
  (install-class-direct-accessors class)
  class)

(defun shared-initialize-<standard-class> (class situation &rest args
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

;; ---------------------------------------------------------------------------

;; Bootstrapping
(progn
  (setq <function> nil)

  ;; 1. Define the class <t>.
  (setq <t>
        (make-instance-<built-in-class> nil
          :name 't
          :direct-superclasses '()))
  (setf (find-class 't) <t>)

  ;; 2. Define the class <standard-object>.
  (setq <standard-object>
        (let ((*allow-mixing-metaclasses* t))
          (make-instance-<standard-class> nil
            :name 'standard-object
            :direct-superclasses `(,<t>)
            :direct-slots '()
            :slots '()
            :slot-location-table empty-ht
            :instance-size 1
            :direct-default-initargs '()
            :default-initargs '())))
  (setf (find-class 'standard-object) <standard-object>)

  ;; 3. Define the class <metaobject>.
  (setq <metaobject>
        (macrolet ((form () *<metaobject>-defclass*))
          (form)))

  ;; 4. Define the class <standard-stablehash>.
  (macrolet ((form () *<standard-stablehash>-defclass*))
    (form))

  ;; 5. Define the class <specializer>.
  (macrolet ((form () *<specializer>-defclass*))
    (form))

  ;; 6. Define the class <class>.
  (setq <class>
        (macrolet ((form () *<class>-defclass*))
          (form)))

  ;; 7. Define the class <built-in-class>.
  (setq <built-in-class>
        (macrolet ((form () *<built-in-class>-defclass*))
          (form)))
  (replace-class-version <built-in-class>
                         *<built-in-class>-class-version*)

  ;; 8. Define the classes <slotted-class>, <semi-standard-class>,
  ;; <standard-class>, <structure-class>.
  (macrolet ((form () *<slotted-class>-defclass*))
    (form))
  (setq <semi-standard-class>
    (macrolet ((form () *<semi-standard-class>-defclass*))
      (form)))
  (setq <standard-class>
    (macrolet ((form () *<standard-class>-defclass*))
      (form)))
  (replace-class-version <standard-class>
                         *<standard-class>-class-version*)
  (setq <structure-class>
    (macrolet ((form () *<structure-class>-defclass*))
      (form)))
  (replace-class-version <structure-class>
                         *<structure-class>-class-version*)

  ;; 9. Define the class <structure-object>.
  (setq <structure-object>
        (let ((*allow-mixing-metaclasses* t))
          (make-instance-<structure-class> <structure-class>
            :name 'structure-object
            :direct-superclasses `(,<t>)
            :direct-slots '()
            :direct-default-initargs '()
            'names (list 'structure-object))))
  (setf (find-class 'structure-object) <structure-object>)
  (setf (get 'structure-object 'sys::defstruct-description)
        (vector (class-names <structure-object>)
                'T
                (class-instance-size <structure-object>)
                nil
                '()
                '()))

  ;; 10. Define other classes whose definition was delayed.

  ;; Define the class <slot-definition>.
  (macrolet ((form () *<slot-definition>-defclass*))
    (form))

  ;; Define the class <direct-slot-definition>.
  (macrolet ((form () *<direct-slot-definition>-defclass*))
    (form))

  ;; Define the class <effective-slot-definition>.
  (setq <effective-slot-definition>
        (macrolet ((form () *<effective-slot-definition>-defclass*))
          (form)))

  ;; Define the class <standard-slot-definition>.
  (macrolet ((form () *<standard-slot-definition>-defclass*))
    (form))

  ;; Define the class <standard-direct-slot-definition>.
  (setq <standard-direct-slot-definition>
        (macrolet ((form () *<standard-direct-slot-definition>-defclass*))
          (form)))
  (replace-class-version (find-class 'standard-direct-slot-definition)
                         *<standard-direct-slot-definition>-class-version*)

  ;; Define the class <standard-effective-slot-definition>.
  (setq <standard-effective-slot-definition>
        (macrolet ((form () *<standard-effective-slot-definition>-defclass*))
          (form)))
  (replace-class-version (find-class 'standard-effective-slot-definition)
                         *<standard-effective-slot-definition>-class-version*)

  ;; Define the class <structure-direct-slot-definition>.
  (setq <structure-direct-slot-definition>
        (macrolet ((form () *<structure-direct-slot-definition>-defclass*))
          (form)))
  (replace-class-version (find-class 'structure-direct-slot-definition)
                         *<structure-direct-slot-definition>-class-version*)

  ;; Define the class <structure-effective-slot-definition>.
  (setq <structure-effective-slot-definition>
        (macrolet ((form () *<structure-effective-slot-definition>-defclass*))
          (form)))
  (replace-class-version (find-class 'structure-effective-slot-definition)
                         *<structure-effective-slot-definition>-class-version*)

  ;; Define the class <eql-specializer>.
  (setq <eql-specializer>
        (macrolet ((form () *<eql-specializer>-defclass*))
          (form)))
  (replace-class-version (find-class 'eql-specializer)
                         *<eql-specializer>-class-version*)

);progn

;;; Install built-in classes:
;; See CLtL2 p. 783 table 28-1, ANSI CL 4.3.7.
(macrolet ((def (&rest classes &aux (new (car (last classes))))
             (let ((name (intern (string-trim "<>" (symbol-name new)))))
               `(setf (find-class ',name)
                  (setq ,new
                    (make-instance-<built-in-class> <built-in-class>
                      :name ',name
                      :direct-superclasses
                        (list ,@(cdr (reverse classes)))))))))
 ;(def <t>)
  (def <t> <character>)
  (def <t> <function>)
  (def <t> <hash-table>)
  (def <t> <package>)
  (def <t> <pathname>)
  #+LOGICAL-PATHNAMES
  (def     <pathname> <logical-pathname>)
  (def <t> <random-state>)
  (def <t> <readtable>)
  (def <t> <stream>)
  (def     <stream> <file-stream>)
  (def     <stream> <synonym-stream>)
  (def     <stream> <broadcast-stream>)
  (def     <stream> <concatenated-stream>)
  (def     <stream> <two-way-stream>)
  (def     <stream> <echo-stream>)
  (def     <stream> <string-stream>)
  (def <t> <symbol>)
  (def <t> <sequence>)
  (def     <sequence> <list>)
  (def                <list> <cons>)
  (def                <list> <symbol> <null>)
  (def <t>            <array>)
  (def     <sequence> <array> <vector>)
  (def                        <vector> <bit-vector>)
  (def                        <vector> <string>)
  (def <t> <number>)
  (def     <number> <complex>)
  (def     <number> <real>)
  (def              <real> <float>)
  (def              <real> <rational>)
  (def                     <rational> <ratio>)
  (def                     <rational> <integer>)
)

;; Continue bootstrapping.
(%defclos
  ;; distinctive marks for CLASS-P
  *<standard-class>-class-version*
  *<structure-class>-class-version*
  *<built-in-class>-class-version*
  <class>
  ;; built-in-classes for CLASS-OF
  (vector <array> <bit-vector> <character> <complex> <cons> <float> <function>
          <hash-table> <integer> <null> <package> <pathname>
          #+LOGICAL-PATHNAMES <logical-pathname>
          <random-state> <ratio> <readtable>
          <stream> <file-stream> <synonym-stream> <broadcast-stream>
          <concatenated-stream> <two-way-stream> <echo-stream> <string-stream>
          <string> <symbol> <t> <vector>))

;;; Intersection of two built-in-classes:
;; Deviations from the single-inheritance are only
;; (AND <sequence> <array>) = <vector> and (AND <list> <symbol>) = <null>.
(defun bc-p (class)
  (or (built-in-class-p class)
      (eq class <standard-object>)
      (eq class <structure-object>)))
(defun bc-and (class1 class2) ; returns (AND class1 class2)
  (cond ((subclassp class1 class2) class1)
        ((subclassp class2 class1) class2)
        ((or (and (subclassp <sequence> class1) (subclassp <array> class2))
             (and (subclassp <sequence> class2) (subclassp <array> class1)))
         <vector>)
        ((or (and (subclassp <list> class1) (subclassp <symbol> class2))
             (and (subclassp <list> class2) (subclassp <symbol> class1)))
         <null>)
        (t nil)))
(defun bc-and-not (class1 class2) ; returns a class c with
                                  ; (AND class1 (NOT class2)) <= c <= class1
  (cond ((subclassp class1 class2) nil)
        ((and (eq class1 <sequence>) (subclassp <vector> class2)) <list>)
        ((and (eq class1 <sequence>) (subclassp <list> class2)) <vector>)
        ((and (eq class1 <list>) (subclassp <null> class2)) <cons>)
        (t class1)))
