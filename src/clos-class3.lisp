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
  (unless (%class-precedence-list class1) (finalize-class class1 t))
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
          'random-state 'ratio 'readtable 'standard-generic-function
          'stream 'file-stream 'synonym-stream 'broadcast-stream
          'concatenated-stream 'two-way-stream 'echo-stream 'string-stream
          'string 'symbol 't 'vector))

;; Bootstrapping support.
(defun replace-class-version (class class-version)
  (replace class-version (class-current-version class))
  (setf (class-current-version class) class-version))

;;; -------------------------------- DEFCLASS --------------------------------

(defmacro defclass (name superclass-specs slot-specs &rest options)
  (unless (symbolp name)
    (error-of-type 'sys::source-program-error
      (TEXT "~S: class name ~S should be a symbol")
      'defclass name))
  (let* ((superclass-forms
           (progn
             (unless (listp superclass-specs)
               (error-of-type 'sys::source-program-error
                 (TEXT "~S ~S: expecting list of superclasses instead of ~S")
                 'defclass name superclass-specs))
             (mapcar #'(lambda (superclass)
                         (unless (symbolp superclass)
                           (error-of-type 'sys::source-program-error
                             (TEXT "~S ~S: superclass name ~S should be a symbol")
                             'defclass name superclass))
                         `',superclass)
                       superclass-specs)))
         (classvar (gensym))
         (accessor-decl-forms '())
         (accessor-def-forms '())
         (slot-forms
           (let ((slot-names '()))
             (unless (listp slot-specs)
               (error-of-type 'sys::source-program-error
                 (TEXT "~S ~S: expecting list of slot specifications instead of ~S")
                 'defclass name slot-specs))
             (mapcar #'(lambda (slot-spec)
                         (let ((slot-name slot-spec) (slot-options '()))
                           (when (consp slot-spec)
                             (setq slot-name (car slot-spec)
                                   slot-options (cdr slot-spec)))
                           (unless (symbolp slot-name)
                             (error-of-type 'sys::source-program-error
                               (TEXT "~S ~S: slot name ~S should be a symbol")
                               'defclass name slot-name))
                           (if (memq slot-name slot-names)
                             (error-of-type 'sys::source-program-error
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
                               (error-of-type 'sys::source-program-error
                                 (TEXT "~S ~S: slot options for slot ~S must come in pairs")
                                 'defclass name slot-name))
                             (do ((optionsr slot-options (cddr optionsr)))
                                 ((atom optionsr))
                               (let ((optionkey (first optionsr))
                                     (argument (second optionsr)))
                                 (case optionkey
                                   ((:READER :WRITER)
                                    (unless (function-name-p argument)
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a function name")
                                        'defclass name slot-name argument))
                                    (case optionkey
                                      (:READER (push argument readers))
                                      (:WRITER (push argument writers))))
                                   (:ACCESSOR
                                    (unless (symbolp argument)
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a symbol")
                                        'defclass name slot-name argument))
                                    (push argument readers)
                                    (push `(SETF ,argument) writers))
                                   (:ALLOCATION
                                    (when allocation
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':allocation slot-name))
                                    (case argument
                                      ((:INSTANCE :CLASS) (setq allocation argument))
                                      (t (error-of-type 'sys::source-program-error
                                           (TEXT "~S ~S, slot option for slot ~S must have the value ~S or ~S, not ~S")
                                           'defclass name slot-name ':instance ':class argument))))
                                   (:INITARG
                                    (unless (symbolp argument)
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a symbol")
                                        'defclass name slot-name argument))
                                    (push argument initargs))
                                   (:INITFORM
                                    (when initform
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':initform slot-name))
                                    (setq initform `(QUOTE ,argument)
                                          initfunction (make-initfunction-form argument slot-name)))
                                   (:TYPE
                                    (when types
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':type slot-name))
                                    (setq types (list argument)))
                                   (:DOCUMENTATION
                                    (when documentation
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':documentation slot-name))
                                    (unless (stringp argument)
                                      (error-of-type 'sys::source-program-error
                                        (TEXT "~S ~S, slot option for slot ~S: ~S is not a string")
                                        'defclass name slot-name argument))
                                    (setq documentation argument))
                                   (t
                                     (error-of-type 'sys::source-program-error
                                       (TEXT "~S ~S, slot option for slot ~S: ~S is not a valid slot option")
                                       'defclass name slot-name optionkey)))))
                             (setq readers (nreverse readers))
                             (setq writers (nreverse writers))
                             (dolist (funname readers)
                               (push `(DECLAIM-METHOD ,funname ((OBJECT ,name)))
                                     accessor-decl-forms)
                               (push `(SETF (CLASS-DIRECT-ACCESSORS ,classvar)
                                            (LIST* ',funname
                                                   #|
                                                   (DEFMETHOD ,funname ((OBJECT ,name))
                                                     ; (:METHOD-CLASS STANDARD-READER-METHOD)
                                                     (DECLARE (COMPILE))
                                                     (SLOT-VALUE OBJECT ',slot-name))
                                                   |#
                                                   (DO-DEFMETHOD ',funname
                                                     (MAKE-STANDARD-READER-METHOD
                                                       :INITFUNCTION
                                                         #'(LAMBDA (#:SELF)
                                                             (DECLARE (COMPILE))
                                                             (%OPTIMIZE-FUNCTION-LAMBDA (T) (#:CONTINUATION OBJECT)
                                                               (DECLARE (COMPILE))
                                                               (SLOT-VALUE OBJECT ',slot-name)))
                                                       :WANTS-NEXT-METHOD-P T
                                                       :PARAMETER-SPECIALIZERS (LIST (FIND-CLASS ',name))
                                                       :QUALIFIERS 'NIL
                                                       :SIGNATURE ,(make-signature :req-num 1)))
                                                   (CLASS-DIRECT-ACCESSORS ,classvar)))
                                     accessor-def-forms))
                             (dolist (funname writers)
                               (push `(DECLAIM-METHOD ,funname (NEW-VALUE (OBJECT ,name)))
                                     accessor-decl-forms)
                               (push `(SETF (CLASS-DIRECT-ACCESSORS ,classvar)
                                            (LIST* ',funname
                                                   #|
                                                   (DEFMETHOD ,funname (NEW-VALUE (OBJECT ,name))
                                                     ; (:METHOD-CLASS STANDARD-WRITER-METHOD)
                                                     (DECLARE (COMPILE))
                                                     (SETF (SLOT-VALUE OBJECT ',slot-name) NEW-VALUE))
                                                   |#
                                                   (DO-DEFMETHOD ',funname
                                                     (MAKE-STANDARD-WRITER-METHOD
                                                       :INITFUNCTION
                                                         #'(LAMBDA (#:SELF)
                                                             (DECLARE (COMPILE))
                                                             (%OPTIMIZE-FUNCTION-LAMBDA (T) (#:CONTINUATION NEW-VALUE OBJECT)
                                                               (DECLARE (COMPILE))
                                                               (SETF (SLOT-VALUE OBJECT ',slot-name) NEW-VALUE)))
                                                       :WANTS-NEXT-METHOD-P T
                                                       :PARAMETER-SPECIALIZERS (LIST (FIND-CLASS 'T) (FIND-CLASS ',name))
                                                       :QUALIFIERS 'NIL
                                                       :SIGNATURE ,(make-signature :req-num 2)))
                                                   (CLASS-DIRECT-ACCESSORS ,classvar)))
                                     accessor-def-forms))
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
         (ENSURE-CLASS
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
                         (error-of-type 'sys::source-program-error
                           (TEXT "~S ~S, option ~S may only be given once")
                           'defclass name optionkey))
                       (case optionkey
                         (:METACLASS
                          (when (eql (length option) 2)
                            (let ((argument (second option)))
                              (unless (symbolp argument)
                                (error-of-type 'sys::source-program-error
                                  (TEXT "~S ~S, option ~S: ~S is not a symbol")
                                  'defclass name option argument))
                              (setq metaclass `(:METACLASS (FIND-CLASS ',argument))))
                            (return)))
                         (:DEFAULT-INITARGS
                          (let ((list (rest option)))
                            (when (and (consp list) (null (cdr list)) (listp (car list)))
                              (setq list (car list))
                              (warn (TEXT "~S ~S: option ~S should be written ~S")
                                    'defclass name option (cons ':DEFAULT-INITARGS list)))
                            (when (oddp (length list))
                              (error-of-type 'sys::source-program-error
                                (TEXT "~S ~S, option ~S: arguments must come in pairs")
                                'defclass name option))
                            (setq direct-default-initargs
                                  `(:DIRECT-DEFAULT-INITARGS
                                    (LIST
                                     ,@(let ((arglist nil) (formlist nil))
                                         (do ((list list (cddr list)))
                                             ((atom list))
                                           (unless (symbolp (first list))
                                             (error-of-type 'sys::source-program-error
                                               (TEXT "~S ~S, option ~S: ~S is not a symbol")
                                               'defclass name option (first list)))
                                           (when (member (first list) arglist)
                                             (error-of-type 'sys::source-program-error
                                               (TEXT "~S ~S, option ~S: ~S may only be given once")
                                               'defclass name option (first list)))
                                           (push (first list) arglist)
                                           (push (second list) formlist))
                                         (mapcan #'(lambda (arg form)
                                                     `((LIST ',arg ',form ,(make-initfunction-form form arg))))
                                                 (nreverse arglist) (nreverse formlist)))))))
                          (return))
                         (:DOCUMENTATION
                          (when (eql (length option) 2)
                            (let ((argument (second option)))
                              (unless (stringp argument)
                                (error-of-type 'sys::source-program-error
                                  (TEXT "~S ~S, option ~S: ~S is not a string")
                                  'defclass name option argument))
                              (setq documentation
                                    `(:DOCUMENTATION ',argument)))
                            (return)))
                         (:FIXED-SLOT-LOCATIONS
                          (setq fixed-slot-locations `(:FIXED-SLOT-LOCATIONS 'T))
                          (return)))))
                   (error-of-type 'sys::source-program-error
                     (TEXT "~S ~S: invalid option ~S")
                     'defclass name option)))
               `(,@metaclass ,@direct-default-initargs ,@documentation ,@fixed-slot-locations))))
       ,@(nreverse accessor-decl-forms) ; the DECLAIM-METHODs
       (LET ((,classvar (FIND-CLASS ',name)))
         ,@(nreverse accessor-def-forms) ; the DEFMETHODs
         ,classvar))))

;; DEFCLASS execution:

;; The function responsible for a MAKE-INSTANCES-OBSOLETE call.
(defvar *make-instances-obsolete-caller* 'make-instances-obsolete)

(defun ensure-class (name &rest all-keys
                          &key (metaclass <standard-class>)
                               (direct-superclasses '())
                               (direct-slots '())
                               (direct-default-initargs '())
                               (documentation nil)
                               (fixed-slot-locations nil)
                          &allow-other-keys)
  (let ((a-standard-class-p (or (eq metaclass <standard-class>)
                                (subclassp metaclass <standard-class>)))
        (class (find-class name nil)))
    (when (and class (not (eq (class-name class) name)))
      ;; Ignore the old class if the given name is not its "proper name".
      (setq class nil))
    (when (and class (not (and a-standard-class-p
                               (eq metaclass (class-of class)))))
      (unless (eq metaclass (class-of class)) ; mixing DEFSTRUCT & DEFCLASS
        (warn (TEXT "Cannot redefine ~S with a different metaclass ~S")
              class metaclass))
      ;; DEFSTRUCT -> (DEFCLASS ... (:METACLASS STRUCTURE-CLASS))
      ;; ==> no warning, just discard the old definition, like with DEFSTRUCT
      (setq class nil))
    ;; See which direct superclasses are already defined.
    (setq direct-superclasses
          (mapcar #'(lambda (c)
                      (if (class-p c)
                        c
                        (or (find-class c (not a-standard-class-p)) c)))
                  direct-superclasses))
    (if class
      (progn
        (if (and (%class-precedence-list class) ; already finalized?
                 (subclassp class <metaobject>))
          ;; Things would go awry when we try to redefine <class> and similar.
          (warn (TEXT "Redefining metaobject class ~S has no effect.")
                class)
          (progn
            ;; Normalize the (class-direct-superclasses class) in the same way as
            ;; the direct-superclasses argument, so that we can compare the two
            ;; lists using EQUAL.
            (when (and a-standard-class-p (null (%class-precedence-list class)))
              (do ((l (class-direct-superclasses class) (cdr l)))
                  ((atom l))
                (let ((c (car l)))
                  (unless (class-p c)
                    (let ((new-c (or (find-class c nil) c)))
                      (unless (symbolp new-c)
                        (check-allowed-superclass class new-c))
                      (setf (car l) new-c)
                      (when (class-p new-c) ; changed from symbol to class
                        (add-direct-subclass new-c class)))))))
            ;; Convert the direct-slots to <direct-slot-definition> instances.
            (setq direct-slots (convert-direct-slots class direct-slots))
            ;; Trivial changes (that can occur when loading the same code twice)
            ;; do not require updating the instances:
            ;; changed slot-options :initform, :documentation,
            ;; changed class-options :default-initargs, :documentation.
            (if (and (equal (or direct-superclasses (default-direct-superclasses class))
                            (class-direct-superclasses class))
                     (equal-direct-slots direct-slots (class-direct-slots class))
                     (equal-default-initargs direct-default-initargs
                                             (class-direct-default-initargs class))
                     (eq fixed-slot-locations (class-fixed-slot-locations class)))
              (progn
                ;; Store new slot-inits:
                (do ((l-old (class-direct-slots class) (cdr l-old))
                     (l-new direct-slots (cdr l-new)))
                    ((null l-new))
                  (let ((old (car l-old))
                        (new (car l-new)))
                    (setf (slot-definition-initform old) (slot-definition-initform new))
                    (setf (slot-definition-initfunction old) (slot-definition-initfunction new))
                    (setf (slot-definition-documentation old) (slot-definition-documentation new))))
                ;; Store new default-initargs:
                (do ((l-old (class-direct-default-initargs class) (cdr l-old))
                     (l-new direct-default-initargs (cdr l-new)))
                    ((null l-new))
                  (let ((old (cdar l-old))
                        (new (cdar l-new)))
                    ;; Move initform and initfunction from new destructively into
                    ;; the old one:
                    (setf (car old) (car new))
                    (setf (cadr old) (cadr new))))
                ;; Store new documentation:
                (setf (class-documentation class) documentation)
                ;; NB: These modifications are automatically inherited by the
                ;; subclasses of class! Due to <inheritable-slot-definition-initer>
                ;; and <inheritable-slot-definition-doc>.
              )
              ;; Instances have to be updated:
              (let* ((was-finalized (%class-precedence-list class))
                     (must-be-finalized
                       (and was-finalized
                            (some #'class-instantiated (list-all-finalized-subclasses class))))
                     (old-direct-superclasses (class-direct-superclasses class))
                     (old-direct-accessors (class-direct-accessors class))
                     old-class)
                ;; ANSI CL 4.3.6. Remove accessor methods created by old DEFCLASS.
                (do ((l old-direct-accessors (cddr l)))
                    ((endp l))
                  (let ((funname (car l))
                        (method (cadr l)))
                    (remove-method (fdefinition funname) method)))
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
                                  (do ((l old-direct-accessors (cddr l)))
                                      ((endp l))
                                    (let ((funname (car l))
                                          (method (cadr l)))
                                      (add-method (fdefinition funname) method)))
                                  (setf (class-direct-accessors class) old-direct-accessors))))
                    (apply (cond ((eq metaclass <standard-class>)
                                  #'initialize-instance-<standard-class>)
                                 ((eq metaclass <built-in-class>)
                                  #'initialize-instance-<built-in-class>)
                                 ((eq metaclass <structure-class>)
                                  #'initialize-instance-<structure-class>)
                                 (t #'initialize-instance))
                           class
                           :name name
                           :direct-superclasses direct-superclasses
                           'direct-slots direct-slots
                           all-keys)
                    ;; FIXME: Need to handle changes of shared slots here?
                    (update-subclasses-for-redefined-class class
                      was-finalized must-be-finalized old-direct-superclasses)))))))
        ;; Modified class as value:
        class)
      (setf (find-class name)
            (apply (cond ((eq metaclass <standard-class>)
                          #'make-instance-<standard-class>)
                         ((eq metaclass <built-in-class>)
                          #'make-instance-<built-in-class>)
                         ((eq metaclass <structure-class>)
                          #'make-instance-<structure-class>)
                         (t #'make-instance))
                   metaclass
                   :name name
                   :direct-superclasses direct-superclasses
                   all-keys)))))
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

;; ----------------------- General routines for <class> -----------------------

;; Preliminary.
(defun class-name (class)
  (class-classname class))

;; Returns the list of implicit direct superclasses when none was specified.
(defun default-direct-superclasses (class)
  (cond ((typep class <standard-class>) (list <standard-object>))
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
      ;; Other than that, only <standard-object> and <structure-object> can
      ;; inherit from <t> without belonging to the same metaclass.
      (and (eq superclass <t>)
           (memq (class-classname class) '(standard-object structure-object)))))

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
  add-direct-subclass
  remove-direct-subclass
  list-direct-subclasses)

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

;;; CLtL2 28.1.5., ANSI CL 4.3.5. Determining the Class Precedence List

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
  (dolist (superclass (%class-precedence-list class) nil)
    (let ((superclassname (class-classname superclass)))
      (when (or (eq superclassname 'standard-stablehash)
                (eq superclassname 'structure-stablehash))
        (return t)))))

;; CLtL2 28.1.3.2., ANSI CL 7.5.3. Inheritance of Slots and Slot Options

(defun std-compute-slots (class)
  ;; Gather all slot-specifiers, ordered by precedence:
  (let ((all-slots
          (mapcan
            #'(lambda (c)
                (mapcar #'(lambda (slot)
                            (let ((alloc (slot-definition-allocation slot))
                                  (loc nil))
                              (if (eq alloc ':class)
                                (setq alloc c)
                                (when (and (not (eq c class))
                                           (standard-class-p c)
                                           (class-fixed-slot-locations c))
                                  (let* ((name (slot-definition-name slot))
                                         (slot-in-c (find name (class-slots c) :key #'slot-definition-name)))
                                    (when slot-in-c
                                      (let ((location (slot-definition-location slot-in-c)))
                                        (assert (or (null location) (integerp location)))
                                        (when location
                                          (setq loc location)))))))
                              (list* slot alloc loc)))
                        (reverse (class-direct-slots c))))
            (%class-precedence-list class))))
    ;; Partition by slot-names:
    (setq all-slots
          (let ((ht (make-hash-table :key-type 'symbol :value-type 't
                                     :test 'ext:stablehash-eql :warn-if-needs-rehash-after-gc t)))
            (dolist (slot+alloc all-slots)
              (let ((slot-name (slot-definition-name (car slot+alloc))))
                (push slot+alloc (gethash slot-name ht nil))))
            (let ((L nil))
              (maphash #'(lambda (name slot+alloc-list)
                           (push (cons name (nreverse slot+alloc-list)) L))
                       ht)
              L))) ; not (nreverse L), because maphash reverses the order
    ;; Bring the slots into final order: Superclass before subclass, and
    ;; inside each class, keeping the same order as in the direct-slots.
    (setq all-slots (nreverse all-slots))
    ;; all-slots is now a list of lists of the form
    ;; (name most-specific-slot+alloc ... least-specific-slot+alloc).
    (mapcar
      #'(lambda (slotbag)
          (let* ((name (car slotbag))
                 (slotspecs (cdr slotbag))
                 (args
                   `(:name ,name
                     ; "The allocation of a slot is controlled by the most
                     ;  specific slot specifier."
                     :allocation ,(cadr (first slotspecs))
                     ; "The set of initialization arguments that initialize a
                     ;  given slot is the union of the initialization arguments
                     ;  declared in the :initarg slot options in all the slot
                     ;  specifiers.
                     ,@(let ((initargs
                               (remove-duplicates
                                 (mapcap #'(lambda (slot+alloc)
                                             (slot-definition-initargs (car slot+alloc)))
                                         slotspecs)
                                 :from-end t)))
                         (if initargs `(:initargs ,initargs)))
                     ; "The default initial value form for a slot is the value
                     ;  of the :initform slot option in the most specific slot
                     ;  specifier that contains one."
                     ,@(dolist (slot+alloc slotspecs '())
                         (when (slot-definition-initfunction (car slot+alloc))
                           (return `(:initform ,(slot-definition-initform (car slot+alloc))
                                     :initfunction ,(slot-definition-initfunction (car slot+alloc))
                                     inheritable-initer ,(slot-definition-inheritable-initer (car slot+alloc))))))
                     ; "The contents of a slot will always be of type
                     ;  (and T1 ... Tn) where T1 ...Tn are the values of the
                     ;  :type slot options contained in all of the slot specifiers."
                     ,@(let ((types '()))
                         (dolist (slot+alloc slotspecs)
                           (push (slot-definition-type (car slot+alloc)) types))
                         `(:type ,(if types `(AND ,@(nreverse types)) 'T)))
                     ; "The documentation string for a slot is the value of the
                     ;  :documentation slot option in the most specific slot
                     ;  specifier that contains one."
                     ,@(dolist (slot+alloc slotspecs '())
                         (when (slot-definition-documentation (car slot+alloc))
                           (return `(:documentation ,(slot-definition-documentation (car slot+alloc))
                                     inheritable-doc ,(slot-definition-inheritable-doc (car slot+alloc))))))
                     #|| ; Commented out because <effective-slot-definition>
                         ; doesn't have readers and writers.
                     ,@(let ((readers
                               (mapcap #'(lambda (slot+alloc) (slot-definition-readers (car slot+alloc)))
                                       slotspecs)))
                         (if readers `(:readers ,readers)))
                     ,@(let ((writers
                               (mapcap #'(lambda (slot+alloc) (slot-definition-writers (car slot+alloc)))
                                       slotspecs)))
                         (if writers `(:writers ,writers)))
                     ||#
                     ,@(let ((location nil))
                         ;; Implementation of fixed-slot-locations policy, part 1.
                         (dolist (slot+alloc slotspecs)
                           (let ((guaranteed-location (cddr slot+alloc)))
                             (when guaranteed-location
                               (if location
                                 (unless (equal location guaranteed-location)
                                   (error (TEXT "In class ~S, the slot ~S is constrained by incompatible constraints inherited from the superclasses.")
                                          (class-name class) name))
                                 (setq location guaranteed-location)))))
                         (if location `(location ,location)))))
                 (slot-definition-class
                   (apply #'effective-slot-definition-class class args)))
            (apply (cond ((eq slot-definition-class 'standard-effective-slot-definition)
                          #'make-instance-<standard-effective-slot-definition>)
                         (t #'make-instance))
                   slot-definition-class args)))
      all-slots)))

;; Allocation of local and shared slots

;; Add the local and shared slots to the slot-location-table ht,
;; incrementing the instance-size, and return the new shared-size.
(defun std-layout-slots (class slots)
  ;; Implementation of fixed-slot-locations policy, part 2.
  (let (constrained-indices)
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
      (setq constrained-indices (mapcar #'slot-definition-location constrained-slots)))
    ;; Actually the constrained-indices must form a list of consecutive indices
    ;; (1 2 ... n), but we don't need to make use of this.
    ;; Now determine the location of each slot.
    (let ((ht (class-slot-location-table class))
          (local-index (class-instance-size class))
          (shared-index 0))
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
          (let* ((name (slot-definition-name slot))
                 (allocation (slot-definition-allocation slot))
                 (location
                   (if (eq allocation ':instance)
                     ; local slot
                     (or (slot-definition-location slot)
                         (prog1
                           local-index
                           (incf local-index)
                           (skip-constrained-indices)))
                     ; shared slot
                     (if (slot-definition-location slot)
                       (error (TEXT "In class ~S, shared slot ~S is constrained to be a local slot at offset ~S.")
                              (class-name class) name (slot-definition-location slot))
                       (if (eq allocation class)
                         ; new shared slot
                         (prog1
                           (cons (class-current-version class) shared-index)
                           (incf shared-index))
                         ; inherited shared slot
                         (gethash name (class-slot-location-table allocation)))))))
            (setf (slot-definition-location slot) location)
            (setf (gethash name ht) location)))
        ;; Actually the constrained-indices must already have been emptied by
        ;; the first (skip-constrained-indices) call, but we don't need to make
        ;; use of this. Warn if :fixed-slot-locations would cause a waste of
        ;; space.
        (when constrained-indices
          (setq local-index (1+ (car (last constrained-indices))))
          (warn (TEXT "In class ~S, constrained slot locations cause holes to appear.")
                (class-name class))))
      (setf (class-instance-size class) local-index)
      shared-index)))

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
                                             &key name
                                                  (direct-superclasses '())
                                             &allow-other-keys)
  (check-metaclass-mix name direct-superclasses
                       #'built-in-class-p 'built-in-class)
  (apply #'initialize-instance-<class> class args)
  ; Initialize the remaining <class> slots:
  (setf (class-precedence-list class)
        (std-compute-cpl class direct-superclasses))
  (setf (class-all-superclasses class)
        (std-compute-superclasses (%class-precedence-list class)))
  (setf (class-slots class) '())
  (setf (class-default-initargs class) '())
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
                                              &key name (direct-superclasses '())
                                                   ;; The following keys come from ENSURE-CLASS.
                                                   ((:direct-slots direct-slots-as-lists) '())
                                                   (direct-default-initargs '()) (documentation nil)
                                                   ;; The following keys come from DEFINE-STRUCTURE-CLASS.
                                                   ((names names) nil)
                                                   ((direct-slots direct-slots-as-metaobjects) '())
                                                   ((slots slots) '()) ((size size) 1)
                                              &allow-other-keys)
  ;; metaclass <= <structure-class>
  (declare (ignore direct-slots-as-lists direct-slots-as-metaobjects
                   documentation))
  (check-metaclass-mix name direct-superclasses
                       #'structure-class-p 'STRUCTURE-CLASS)
  (apply #'initialize-instance-<slotted-class> class args)
  (setq direct-superclasses (class-direct-superclasses class)) ; augmented
  ; Initialize the remaining <class> slots:
  (setf (class-precedence-list class)
        (std-compute-cpl class direct-superclasses))
  (setf (class-all-superclasses class)
        (std-compute-superclasses (%class-precedence-list class)))
  (unless names
    ;; When called via ENSURE-CLASS, we have to do inheritance of slots.
    (when direct-superclasses
      (setq slots (class-slots (first direct-superclasses)))
      (setq size (class-instance-size (first direct-superclasses)))))
  (setf (class-slot-location-table class)
        (make-hash-table
          :key-type 'symbol :value-type 't
          :test 'ext:stablehash-eq :warn-if-needs-rehash-after-gc t
          :initial-contents
            (mapcar #'(lambda (slot)
                        (cons (slot-definition-name slot) (slot-definition-location slot)))
                    slots)))
  (setf (class-instance-size class) size)
  (unless names
    (let* ((more-slots (std-compute-slots class))
           (shared-index (std-layout-slots class more-slots)))
      (when (plusp shared-index)
        (error-of-type 'error
          (TEXT "(~S ~S): metaclass ~S does not support shared slots")
                'DEFCLASS name 'STRUCTURE-CLASS))
      (setq slots (append slots more-slots))))
  (setf (class-slots class) slots)
  (setf (class-default-initargs class)
        (remove-duplicates
          (append direct-default-initargs
                  (mapcap #'class-default-initargs
                    (cdr (%class-precedence-list class))))
          :key #'car
          :from-end t))
  ; Initialize the remaining <slotted-class> slots:
  (setf (class-subclass-of-stablehash-p class)
        (std-compute-subclass-of-stablehash-p class))
  (setf (class-valid-initargs class)
        (remove-duplicates (mapcap #'slot-definition-initargs (class-slots class))))
  (setf (class-instance-size class) size)
  ; Initialize the remaining <structure-class> slots:
  (unless names
    (setq names
          (cons name
                (if direct-superclasses
                   (class-names (first direct-superclasses))
                   '()))))
  (setf (class-names class) names)
  ; Done.
  (system::note-new-structure-class)
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
                        1)))))))
(defun undefine-structure-class (name)
  (setf (find-class name) nil))

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
                                             &key (direct-superclasses '())
                                                  ((:direct-slots direct-slots-as-lists) '())
                                                  ((direct-slots direct-slots-as-metaobjects) '())
                                                  (direct-default-initargs '()) (documentation nil)
                                                  (fixed-slot-locations nil)
                                             &allow-other-keys)
  (declare (ignore direct-superclasses direct-slots-as-lists
                   direct-slots-as-metaobjects direct-default-initargs
                   documentation))
  (apply #'initialize-instance-<slotted-class> class args)
  (when (eq (sys::%record-ref class *<standard-class>-current-version-location*)
            (sys::%unbound))
    (setf (class-current-version class)
          (make-class-version :newest-class class
                              :class class
                              :serial 0))
    (unless *classes-finished*
      ; Bootstrapping: Simulate the effect of #'%initialize-instance.
      (setf (class-direct-accessors class) '())
      (setf (class-instantiated class) nil)
      (setf (class-finalized-direct-subclasses-table class) '())))
  ; Initialize the remaining <class> slots:
  (setf (class-precedence-list class) nil) ; mark as not yet finalized
  (setf (class-all-superclasses class) nil) ; mark as not yet finalized
  ; Initialize the remaining <slotted-class> slots:
  ; Initialize the remaining <standard-class> slots:
  (setf (class-fixed-slot-locations class) fixed-slot-locations)
  (setf (class-prototype class) nil)
  ; Try to finalize it.
  (finalize-class class nil)
  ; Done.
  class)

;; ---------------- Finalizing an instance of <standard-class> ----------------

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
    (if (%class-precedence-list class) ; already finalized?
      class
      (progn
        ;; Here we get only for instances of STANDARD-CLASS, since instances
        ;; of BUILT-IN-CLASS and STRUCTURE-CLASS are already finalized when
        ;; they are constructed.
        (when (memq class finalizing-now)
          (error-of-type 'sys::source-program-error
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
        (finalize-instance-standard-class class)
        class))))

(defun finalize-instance-standard-class (class
       &aux (direct-superclasses (class-direct-superclasses class))
            (name (class-name class))
            (old-slot-location-table (class-slot-location-table class)))
  ;; metaclass <= <standard-class>
  (check-metaclass-mix name direct-superclasses
                       #'standard-class-p 'STANDARD-CLASS)
  (setf (class-precedence-list class)
        (std-compute-cpl class direct-superclasses))
  (setf (class-all-superclasses class)
        (std-compute-superclasses (%class-precedence-list class)))
  (dolist (super direct-superclasses)
    (when (standard-class-p super)
      (add-finalized-direct-subclass super class)))
  (setf (class-subclass-of-stablehash-p class)
        (std-compute-subclass-of-stablehash-p class))
  (setf (class-slots class) (std-compute-slots class))
  (setf (class-slot-location-table class)
        (make-hash-table :key-type 'symbol :value-type 't
                         :test 'ext:stablehash-eq :warn-if-needs-rehash-after-gc t))
  (setf (class-instance-size class) 1) ; slot 0 is the class_version pointer
  (let ((shared-index (std-layout-slots class (class-slots class))))
    (when (plusp shared-index)
      (setf (cv-shared-slots (class-current-version class))
            (let ((v (make-array shared-index))
                  (i 0))
              (dolist (slot (class-slots class) v)
                (when (eq (slot-definition-allocation slot) class)
                  (setf (svref v i)
                        (let ((old-slot-location
                                (gethash (slot-definition-name slot) old-slot-location-table)))
                          (if (and (consp old-slot-location)
                                   (eq (cv-newest-class (car old-slot-location)) class))
                            ;; The slot was already shared. Retain its value.
                            (svref (cv-shared-slots (car old-slot-location))
                                   (cdr old-slot-location))
                            ;; A new shared slot.
                            (let ((initfunction (slot-definition-initfunction slot)))
                              (if initfunction
                                (funcall initfunction)
                                (sys::%unbound))))))
                  (incf i)))))))
  ;; CLtL2 28.1.3.3., ANSI CL 4.3.4.2. Inheritance of Class Options
  (setf (class-default-initargs class)
        (remove-duplicates
          (mapcap #'class-direct-default-initargs (%class-precedence-list class))
          :key #'car
          :from-end t))
  (setf (class-valid-initargs class)
        (remove-duplicates (mapcap #'slot-definition-initargs (class-slots class))))
  (system::note-new-standard-class))

;; --------------- Redefining an instance of <standard-class> ---------------

;; Preliminary definition.
(defun make-instances-obsolete (class)
  (make-instances-obsolete-standard-class class))

(defun make-instances-obsolete-standard-class (class)
  (when (%class-precedence-list class) ; nothing to do if not yet finalized
    ;; Recurse to the subclasses. (Even if there are no direct instances of
    ;; this class: the subclasses may have instances.)
    (mapc #'make-instances-obsolete-standard-class-nonrecursive
          (list-all-finalized-subclasses class))))

(defun make-instances-obsolete-standard-class-nonrecursive (class)
  (if (and (%class-precedence-list class) ; already finalized?
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
                (when (standard-class-p super)
                  (remove-finalized-direct-subclass super class)))
              (dolist (super added-direct-superclasses)
                (when (standard-class-p super)
                  (add-finalized-direct-subclass super class)))))))
      ;; The class becomes unfinalized.
      (dolist (super old-direct-superclasses)
        (when (standard-class-p super)
          (remove-finalized-direct-subclass super class))))
    ;; Now handle the true subclasses.
    (mapc #'update-subclasses-for-redefined-class-nonrecursive
          (rest (list-all-finalized-subclasses class)))))

(defun update-subclasses-for-redefined-class-nonrecursive (class)
  (when (%class-precedence-list class) ; nothing to do if not yet finalized
    (setf (class-precedence-list class) nil) ; mark as not yet finalized
    (setf (class-all-superclasses class) nil) ; mark as not yet finalized
    (if (class-instantiated class)
      ;; The class remains finalized.
      (finalize-class class t)
      ;; The class becomes unfinalized. If it has an instantiated subclass, the
      ;; subclass' finalize-class invocation will re-finalize this one.
      (dolist (super (class-direct-superclasses class))
        (when (standard-class-p super)
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

;; --------------- Auxiliary functions for <standard-class> ---------------

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
              (nreconc (list-finalized-direct-subclasses class) new-pending))))
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

;; ---------------------------------------------------------------------------

;; Bootstrapping
(progn

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

  ;; 8. Define the classes <slotted-class>, <standard-class>, <structure-class>.
  (macrolet ((form () *<slotted-class>-defclass*))
    (form))
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
  (def     <function> <generic-function>)
  (def                <generic-function> <standard-generic-function>)
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
          <random-state> <ratio> <readtable> <standard-generic-function>
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
