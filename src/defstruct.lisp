;;; Sources for CLISP DEFSTRUCT macro
;;; Bruno Haible 1988-2003
;;; Sam Steingold 1998-2003
;;; German comments translated into English: Stefan Kain 2003-01-14

(in-package "SYSTEM")

(defsetf %structure-ref %structure-store)

#| Explanation of the appearing data types:

   (get name 'DEFSTRUCT-DESCRIPTION) =
     #(names type keyword-constructor slotlist defaultfun0 defaultfun1 ...)

   names is a coding of the INCLUDE-nesting for Structure name:
   names = (name_1 ... name_i-1 name_i) with name=name_1,
     name_1 contains name_2, ..., name_i-1 contains name_i.

   type (if the type of the whole structure is meant):
      = T                      storage as a normal structure
      = LIST                   storage as list
      = VECTOR                 storage as (simple-)vector
      = (VECTOR element-type)  storage as vector with element-type

   keyword-constructor = NIL or the name of the keyword-constructor

   slotlist is a packed description of the slots of a structure:
   slotlist = ({slot}*)
   slot = #(name initargs offset initer default type readonly var)
   with name being the slotname,
              (NIL for the slot, that contains the structure-name)
         default is the default value:
              either a constant, that evaluates to the default value,
              or a form (a symbol or a list (SVREF ...)), that yields
              upon evaluation in an arbitrary environment a function,
              that returns the default value, when called.
         type being the declared type for this slot,
         readonly = NIL or = T specifying, if this slot is readonly, i.e.
              after the construction of the Structure the slot cannot be
               changed with (setf ...) anymore.
   if type = T, the Structure-Name occupies the slot 0, but is not itemized
     in the slotlist, because there is nothing to do for its initialization.

|#

(defun make-ds-slot (name initargs offset initer default type readonly)
  (vector name initargs offset initer default type readonly
          (and name (make-symbol (symbol-name name)))))
(proclaim '(inline ds-slot-name ds-slot-var))
(defun ds-slot-name (slot) (svref slot 0))
;(defun ds-slot-initargs (slot) (svref slot 1)) ; only used in clos.lisp
(defmacro ds-slot-offset (slot) `(svref ,slot 2))
(defmacro ds-slot-initer (slot) `(svref ,slot 3)) ; used in clos.lisp
(defmacro ds-slot-default (slot) `(svref ,slot 4))
(defmacro ds-slot-type (slot) `(svref ,slot 5))
(defmacro ds-slot-readonly (slot) `(svref ,slot 6))
(defun ds-slot-var (slot) (svref slot 7))
(defun copy-ds-slot (slot) (sys::%copy-simple-vector slot))

#| auxiliary function for both constructors:
   (ds-arg-default arg slot)
   returns for an argument arg (part of the argument list) the part of
   the argument list, that binds this argument with the default for slot.
|#

(defun ds-arg-default (arg slot)
  (let ((default (ds-slot-default slot)))
    ;; default is either constant or function or symbol
    (if (constantp default)
        `(,arg ,default)
        `(,arg (FUNCALL ,default)))))

#| auxiliary function for both constructors:
   (ds-make-constructor-body type name names size slotlist get-var)
   returns the expression, that creates and fills a structure
   of given type.
|#
(defun ds-make-constructor-body (type name names size slotlist get-var)
  (if (and (or (eq type 'VECTOR) (eq type 'LIST))
           (do ((slotlistr slotlist (cdr slotlistr))
                (index 0 (1+ index)))
               ((null slotlistr) t)
             (unless (eq (ds-slot-offset (car slotlistr)) index)
               (return nil))))
    ;; optimize the simple case
    `(,type ,@(mapcar #'(lambda (slot)
                          (if (ds-slot-name slot)
                            `(THE ,(ds-slot-type slot) ,(funcall get-var slot))
                            `(QUOTE ,(ds-slot-default slot))))
                       slotlist))
    `(LET ((OBJECT
             ,(cond ((eq type 'T) `(%MAKE-STRUCTURE ,names ,size))
                    ((eq type 'LIST) `(MAKE-LIST ,size))
                    ((consp type)
                     `(MAKE-ARRAY ,size :ELEMENT-TYPE ',(second type)))
                    (t `(MAKE-ARRAY ,size)))))
       ,@(mapcar
          #'(lambda (slot &aux (offset (ds-slot-offset slot)))
              `(SETF
                ,(cond ((eq type 'T)
                        `(%STRUCTURE-REF ',name OBJECT ,offset) )
                       ((eq type 'LIST)
                        `(NTH ,offset OBJECT) )
                       ((eq type 'VECTOR)
                        `(SVREF OBJECT ,offset) )
                       (t `(AREF OBJECT ,offset) ))
                ,(if (ds-slot-name slot)
                   `(THE ,(ds-slot-type slot) ,(funcall get-var slot))
                   `(QUOTE ,(ds-slot-default slot)))))
           slotlist)
       OBJECT)))

#| auxiliary function for ds-make-boa-constructor:

   (ds-arg-with-default arg slotlist)
   returns for an argument arg (part of the argument list) the part of
   the argument list, that binds this argument with the correct default value.
|#

(defun ds-arg-with-default (arg slotlist)
  (if (and (listp arg) (consp (cdr arg)))
    ;; default value is already supplied
    arg
    ;; no default value in the lambda-list
    (let* ((var (if (listp arg) (first arg) arg))
           (slot (find (if (consp var) (second var) var) slotlist
                       :key #'ds-slot-name :test #'eq)))
      (if slot
        ;; slot found -> take its default value
        (ds-arg-default var slot)
        ;; slot not found, no default value
        arg))))

#| (ds-make-boa-constructor descriptor type name names size slotlist)
   returns the form, that defines the BOA-constructor.
|#
(defun ds-make-boa-constructor (descriptor type name names size slotlist)
  (let ((constructorname (first descriptor))
        (arglist (second descriptor)))
    ;; gather supplied arguments:
    (let* ((argnames
             (let ((L nil))
               (dolist (arg arglist)
                 (unless (memq arg lambda-list-keywords)
                   (let ((var (if (listp arg) (first arg) arg)))
                     (push (if (consp var) (second var) var) L))))
               (nreverse L)))
           ;; argnames is the list of all arguments that are already
           ;; supplied with values in the parameter list.
           (new-arglist ; new argument list
            `(;; required args:
              ,@(do ((arglistr arglist (cdr arglistr))
                     (arg)
                     (required-args nil))
                    ((or (endp arglistr)
                         (memq (setq arg (car arglistr))
                               lambda-list-keywords))
                     (nreverse required-args))
                    (push arg required-args))
              ;; optional args:
              ,@(do ((arglistr (cdr (memq '&optional arglist)) (cdr arglistr))
                     (arg)
                     (optionals nil))
                    ((or (endp arglistr)
                         (memq (setq arg (car arglistr))
                               lambda-list-keywords))
                     (if (null optionals) nil
                         (cons '&optional (nreverse optionals))))
                    (push (ds-arg-with-default arg slotlist) optionals))
              ;; rest arg:
              ,@(let ((arglistr (memq '&rest arglist)))
                  (if arglistr `(&rest ,(second arglistr)) '()))
              ;; key args:
              ,@(do ((arglistr (cdr (memq '&key arglist)) (cdr arglistr))
                     (arg)
                     (keys nil))
                    ((or (endp arglistr)
                         (memq (setq arg (car arglistr))
                               lambda-list-keywords))
                     (setq keys (nreverse keys))
                     (if (and (consp arglistr)
                              (eq (car arglistr) '&allow-other-keys))
                       (progn (pop arglistr) `(&key ,@keys &allow-other-keys))
                       (if (null keys) nil `(&key ,@keys))))
                    (push (ds-arg-with-default arg slotlist) keys))
              ;; aux args:
              &aux
              ,@(do ((aux-args-r (cdr (memq '&aux arglist)) (cdr aux-args-r))
                     (aux-arg)
                     (new-aux-args nil))
                    ((or (null aux-args-r)
                         (memq (setq aux-arg (car aux-args-r))
                               lambda-list-keywords))
                     (nreverse new-aux-args))
                    (push (ds-arg-with-default aux-arg slotlist) new-aux-args))
              ,@(let ((slotinitlist nil))
                  (dolist (slot slotlist)
                    (when (ds-slot-name slot)
                      (unless (memq (ds-slot-name slot) argnames)
                        (push (ds-arg-with-default
                               (ds-slot-name slot) slotlist)
                              slotinitlist))))
                  (nreverse slotinitlist)))))
      `(DEFUN ,constructorname ,new-arglist
         ,(ds-make-constructor-body type name names size slotlist
                                    #'ds-slot-name)))))

#| (ds-make-keyword-constructor descriptor type name names size slotlist)
   returns the form, that defines the keyword-constructor. |#
(defun ds-make-keyword-constructor (descriptor type name names size slotlist)
  `(DEFUN ,descriptor
     (&KEY
      ,@(mapcap
          #'(lambda (slot)
              (if (ds-slot-name slot)
                (list (ds-arg-default (ds-slot-var slot) slot))
                '()))
          slotlist))
     ,(ds-make-constructor-body type name names size slotlist #'ds-slot-var)))

#| (ds-make-pred predname type name name-offset)
   returns the form, that creates the type-test-predicate for
   the structure name.

   type         the type of the structure,
   name         the name of the structure,
   predname     the name of the type-test-predicate,
   name-offset  (only possible with type /= T )
                the position, where the name is stored.
|#
(defun ds-make-pred (predname type name name-offset)
  `(,@(if (eq type 'T) `((PROCLAIM '(INLINE ,predname))) '())
    (DEFUN ,predname (OBJECT)
      ,(if (eq type 'T)
         `(%STRUCTURE-TYPE-P ',name OBJECT)
         (if (eq type 'LIST)
           `(AND (CONSP OBJECT)
                 ,@(if (eql name-offset 0)
                     `((EQ (CAR OBJECT) ',name))
                     `((> (LENGTH OBJECT) ,name-offset)
                       (EQ (NTH ,name-offset OBJECT) ',name))))
           `(AND (SIMPLE-VECTOR-P OBJECT)
                 (> (LENGTH OBJECT) ,name-offset)
                 (EQ (SVREF OBJECT ,name-offset) ',name)))))))

(defun ds-make-copier (copiername name type)
  (declare (ignore name))
  `(,@(if (or (eq type 'T) (eq type 'LIST))
        `((PROCLAIM '(INLINE ,copiername)))
        '())
    (DEFUN ,copiername (STRUCTURE)
      ,(if (eq type 'T)
         '(COPY-STRUCTURE STRUCTURE)
         (if (eq type 'LIST)
           '(COPY-LIST STRUCTURE)
           (if (consp type)
             `(LET* ((OBJ-LENGTH (ARRAY-TOTAL-SIZE STRUCTURE))
                     (OBJECT (MAKE-ARRAY OBJ-LENGTH :ELEMENT-TYPE
                                         (QUOTE ,(second type)))))
                (DOTIMES (I OBJ-LENGTH OBJECT)
                  (SETF (AREF OBJECT I) (AREF STRUCTURE I))))
             '(%COPY-SIMPLE-VECTOR STRUCTURE)))))))

(defun ds-make-accessors (name names type concname slotlist)
  (mapcap
    #'(lambda (slot)
        (if (ds-slot-name slot)
          (let ((accessorname
                 (if concname
                     (concat-pnames concname (ds-slot-name slot))
                     (ds-slot-name slot)))
                (offset (ds-slot-offset slot))
                (slottype (ds-slot-type slot)))
            ;; This makes the macroexpansion depend on the current state
            ;; of the compilation environment, but it doesn't hurt because
            ;; the included structure's definition must already be
            ;; present in the compilation environment anyway. We don't expect
            ;; people to re-DEFUN defstruct accessors.
            (if (memq (get accessorname 'SYSTEM::DEFSTRUCT-READER name)
                      (cdr names))
              '()
              `((PROCLAIM '(FUNCTION ,accessorname (,name) ,slottype))
                (PROCLAIM '(INLINE ,accessorname))
                (DEFUN ,accessorname (OBJECT)
                  (THE ,slottype
                    ,(cond ((eq type 'T)
                            `(%STRUCTURE-REF ',name OBJECT ,offset))
                           ((eq type 'LIST) `(NTH ,offset OBJECT))
                           ((consp type) `(AREF OBJECT ,offset))
                           (t `(SVREF OBJECT ,offset)))))
                (SYSTEM::%PUT ',accessorname 'SYSTEM::DEFSTRUCT-READER
                              ',name))))
          '()))
    slotlist))

(defun ds-make-defsetfs (name names type concname slotlist)
  (mapcap
    #'(lambda (slot)
        (if (and (ds-slot-name slot) (not (ds-slot-readonly slot)))
          (let ((accessorname
                 (if concname
                     (concat-pnames concname (ds-slot-name slot))
                     (ds-slot-name slot)))
                (offset (ds-slot-offset slot))
                (slottype (ds-slot-type slot)))
            ;; This makes the macroexpansion depend on the current state
            ;; of the compilation environment, but it doesn't hurt because
            ;; the included structure's definition must already be
            ;; present in the compilation environment anyway. We don't expect
            ;; people to re-DEFSETF defstruct accessors.
            (if (memq (get accessorname 'SYSTEM::DEFSTRUCT-WRITER name)
                      (cdr names))
              '()
              `((DEFSETF ,accessorname (STRUCT) (VALUE)
                  ,(if (eq type 'T)
                     `(LIST '%STRUCTURE-STORE '',name
                        STRUCT
                        ,offset
                        ,(if (eq 'T slottype)
                           `VALUE
                           `(LIST 'THE ',slottype VALUE)))
                     (if (eq type 'LIST)
                       `(LIST 'SETF (LIST 'NTH ,offset STRUCT) VALUE)
                       (if (consp type)
                         `(LIST 'SETF (LIST 'AREF STRUCT ,offset) VALUE)
                         `(LIST 'SETF (LIST 'SVREF STRUCT ,offset) VALUE)))))
                (eval-when (compile eval load)
                  (SYSTEM::%PUT ',accessorname 'SYSTEM::DEFSTRUCT-WRITER
                                ',name)))))))
    slotlist))

;; Two hooks for CLOS
(defun clos::define-structure-class (name) ; preliminary
  (declare (ignore name))
  (system::note-new-structure-class))
(defun clos::defstruct-remove-print-object-method (name) ; preliminary
  (declare (ignore name))
  nil)

(defmacro defstruct (name-and-options . docstring-and-slotargs)
  (let ((name                              name-and-options)
        (options                           nil)
        (conc-name-option                  t)
        (constructor-option-list           nil)
        (keyword-constructor               nil)
        (copier-option                     t)
        (predicate-option                  0)
        (include-option                    nil)
         names
         namesform
        (namesbinding                      nil)
        (print-object-option               nil)
        (type-option                       t)
        (named-option                      0)
        (initial-offset-option             0)
        (initial-offset                    0)
        (docstring                         nil)
        (slotargs                          docstring-and-slotargs)
         size
        (include-skip                      0)
        (inherited-slot-count              0)
        (slotlist                          nil)
        (slotdefaultvars                   nil)
        (slotdefaultfuns                   nil)
         constructor-forms                      )
    ;; check name-and-options:
    (when (listp name-and-options)
      (setq name (first name-and-options))
      (setq options (rest name-and-options)))
    ;; otherwise, name and options are already correct.
    (unless (symbolp name)
      (error-of-type 'source-program-error
        (TEXT "~S: invalid syntax for name and options: ~S")
        'defstruct name-and-options))
    ;; name is a symbol, options is the list of options.
    ;; processing the options:
    (dolist (option options)
      (when (keywordp option) (setq option (list option))) ; option without arguments
      (if (listp option)
        (if (keywordp (car option))
          (case (first option)
            (:CONC-NAME
             (setq conc-name-option (second option)))
            (:CONSTRUCTOR
               (if (atom (cdr option))
                 ;; default-keyword-constructor
                 (push (concat-pnames "MAKE-" name) constructor-option-list)
                 (let ((arg (second option)))
                   (check-symbol 'defstruct arg)
                   (push
                     (if (atom (cddr option))
                       arg ; keyword-constructor
                       (if (not (listp (third option)))
                         (error-of-type 'source-program-error
                           (TEXT "~S ~S: argument list should be a list: ~S")
                           'defstruct name (third option))
                         (rest option))) ; BOA-constructor
                     constructor-option-list))))
            (:COPIER
               (when (consp (cdr option))
                 (let ((arg (second option)))
                   (check-symbol 'defstruct arg)
                   (setq copier-option arg))))
            (:PREDICATE
               (when (consp (cdr option))
                 (let ((arg (second option)))
                   (check-symbol 'defstruct arg)
                   (setq predicate-option arg))))
            ((:INCLUDE :INHERIT)
               (if (null include-option)
                 (setq include-option option)
                 (error-of-type 'source-program-error
                   (TEXT "~S ~S: At most one :INCLUDE argument may be specified: ~S")
                   'defstruct name options)))
            ((:PRINT-FUNCTION :PRINT-OBJECT)
               (if (null (cdr option))
                 (setq print-object-option '(PRINT-STRUCTURE STRUCT STREAM))
                 (let ((arg (second option)))
                   (when (and (consp arg) (eq (first arg) 'FUNCTION))
                     (warn (TEXT "~S: Use of ~S implicitly applies FUNCTION.~@
                                     Therefore using ~S instead of ~S.")
                           'defstruct (first option) (second arg) arg)
                     (setq arg (second arg)))
                   (setq print-object-option
                         `(,arg STRUCT STREAM
                           ,@(if (eq (first option) ':PRINT-FUNCTION)
                                 '(*PRIN-LEVEL*) '()))))))
            (:TYPE (setq type-option (second option)))
            (:NAMED (setq named-option t))
            (:INITIAL-OFFSET (setq initial-offset-option (or (second option) 0)))
            (T (error-of-type 'source-program-error
                 (TEXT "~S ~S: unknown option ~S")
                 'defstruct name (first option))))
          (error-of-type 'source-program-error
            (TEXT "~S ~S: invalid syntax in ~S option: ~S")
            'defstruct name 'defstruct option))
        (error-of-type 'source-program-error
          (TEXT "~S ~S: not a ~S option: ~S")
          'defstruct name 'defstruct option)))
    ;;; conc-name-option is either T or NIL or the :CONC-NAME argument.
    ;; constructor-option-list is a list of all :CONSTRUCTOR-arguments,
    ;;   each in the form  symbol  or  (symbol arglist . ...).
    ;; copier-option is either T or the :COPIER-argument.
    ;; predicate-option is either 0 or the :PREDICATE-argument.
    ;; include-option is either NIL or the entire
    ;;   :INCLUDE/:INHERIT-option.
    ;; print-object-option is NIL or a form for the body of the PRINT-OBJECT
    ;;   method.
    ;; type-option is either T or the :TYPE-argument.
    ;; named-option is either 0 or T.
    ;; initial-offset-option is either 0 or the :INITIAL-OFFSET-argument.
    ;;; inspection of the options:
    (setq named-option (or (eq type-option 'T) (eq named-option 'T)))
    ;; named-option (NIL or T) specifies, if the name is in the structure.
    (if named-option
      (when (eql predicate-option 0)
        (setq predicate-option (concat-pnames name "-P"))) ; defaultname
      (unless (or (eql predicate-option 0) (eq predicate-option 'NIL))
        (error-of-type 'source-program-error
          (TEXT "~S ~S: There is no :PREDICATE on unnamed structures.")
          'defstruct name)))
    ;; predicate-option is
    ;;   if named-option=T: either NIL or the name of the type-test-predicate,
    ;;   if named-option=NIL meaningless.
    (when (eq conc-name-option 'T)
      (setq conc-name-option (string-concat (string name) "-")))
    ;; conc-name-option is the name prefix.
    (if constructor-option-list
      (setq constructor-option-list (remove 'NIL constructor-option-list))
      (setq constructor-option-list (list (concat-pnames "MAKE-" name))))
    ;; constructor-option-list is a list of all constructors that have to be
    ;; created, each in the form  symbol  or  (symbol arglist . ...).
    (if (eq copier-option 'T)
      (setq copier-option (concat-pnames "COPY-" name)))
    ;; copier-option is either NIL or the name of the copy function.
    (unless (or (eq type-option 'T)
                (eq type-option 'VECTOR)
                (eq type-option 'LIST)
                (and (consp type-option) (eq (first type-option) 'VECTOR)))
      (error-of-type 'source-program-error
        (TEXT "~S ~S: invalid :TYPE option ~S")
        'defstruct name type-option))
    ;; type-option is either T or LIST or VECTOR or (VECTOR ...)
    (unless (and (integerp initial-offset-option) (>= initial-offset-option 0))
      (error-of-type 'source-program-error
        (TEXT "~S ~S: The :INITIAL-OFFSET must be a nonnegative integer, not ~S")
        'defstruct name initial-offset-option))
    ;; initial-offset-option is an Integer >=0.
    (when (and (plusp initial-offset-option) (eq type-option 'T))
      (error-of-type 'source-program-error
        (TEXT "~S ~S: :INITIAL-OFFSET must not be specified without :TYPE : ~S")
        'defstruct name options))
    ;; if type-option=T, then initial-offset-option=0.
    (when (eq type-option 'T) (setq include-skip 1))
    ;; if type-option=T, include-skip is 1, else 0.
    (when (stringp (first docstring-and-slotargs))
      (setq docstring (first docstring-and-slotargs))
      (setq slotargs (rest docstring-and-slotargs)))
    ;; else, docstring and slotargs are already correct.
    ;; docstring is either NIL or a String.
    ;; slotargs are the remaining arguments.
    (if include-option
      (let* ((option (rest include-option))
             (subname (first option))
             (incl-desc (get subname 'DEFSTRUCT-DESCRIPTION)))
        (when (null incl-desc)
          (error-of-type 'source-program-error
            (TEXT "~S ~S: included structure ~S has not been defined.")
            'defstruct name subname))
        (setq names (cons name (svref incl-desc 0)))
        (setq namesbinding
              (list
               (list
                (setq namesform (gensym))
                `(CONS ',name (SVREF (GET ',subname 'DEFSTRUCT-DESCRIPTION)
                                     0)))))
        (unless (equalp (svref incl-desc 1) type-option)
          (error-of-type 'source-program-error
            (TEXT "~S ~S: included structure ~S must be of the same type ~S.")
            'defstruct name subname type-option))
        (setq slotlist
          (nreverse
            (mapcar #'(lambda (slot)
                        (setq slot (copy-ds-slot slot))
                        (when (car (ds-slot-initer slot))
                          (setf (ds-slot-initer slot)
                                (cons (add-unquote (ds-slot-default slot))
                                      'NIL)))
                        slot)
                    (svref incl-desc 3))))
        ;; slotlist is the reversed list of the inherited slots
        (when slotlist
          (setq include-skip (1+ (ds-slot-offset (first slotlist)))))
        ;; include-skip >=0 is the number of slots that are already consumend
        ;;    by the substructure, the "size" of the substructure.
        ;; process further arguments of the :INCLUDE-option:
        (dolist (slotarg (rest option))
          (let* ((slotname (if (atom slotarg) slotarg (first slotarg)))
                 (slot (find slotname slotlist :key #'ds-slot-name
                             :test #'eq)))
            (when (null slot)
              (error-of-type 'source-program-error
                (TEXT "~S ~S: included structure ~S has no component with name ~S.")
                'defstruct name subname slotname))
            (if (atom slotarg)
              (setf (ds-slot-default slot) 'NIL) ; overwrite default to NIL
              (progn
                (let ((default (second slotarg)))
                  (unless (constantp default)
                    (push
                      `(FUNCTION ,(concat-pnames "DEFAULT-" slotname)
                         (LAMBDA () ,default))
                      slotdefaultfuns)
                    (setq default (gensym))
                    (push default slotdefaultvars))
                  (setf (ds-slot-default slot) default))
                ;; process the slot-options of this Slot-Specifier:
                (do ((slot-arglistr (cddr slotarg) (cddr slot-arglistr)))
                    ((endp slot-arglistr))
                  (let ((slot-keyword (first slot-arglistr))
                        (slot-key-value (second slot-arglistr)))
                    (cond ((eq slot-keyword ':READ-ONLY)
                           (if slot-key-value
                             (setf (ds-slot-readonly slot) t)
                             (if (ds-slot-readonly slot)
                               (error-of-type 'source-program-error
                                 (TEXT "~S ~S: The READ-ONLY slot ~S of the included structure ~S must remain READ-ONLY in ~S.")
                                 'defstruct name slotname subname name)
                               (setf (ds-slot-readonly slot) nil))))
                          ((eq slot-keyword ':TYPE)
                           (unless (subtypep (type-for-discrimination
                                              slot-key-value)
                                             (type-for-discrimination
                                              (ds-slot-type slot)))
                             (error-of-type 'source-program-error
                               (TEXT "~S ~S: The type ~S of slot ~S should be a subtype of the type defined for the included strucure ~S, namely ~S.")
                               'defstruct name slot-key-value slotname subname
                               (ds-slot-type slot)))
                           (setf (ds-slot-type slot) slot-key-value))
                          (t (error-of-type 'source-program-error
                               (TEXT "~S ~S: ~S is not a slot option.")
                               'defstruct name slot-keyword)))))))))
        (when (eq (first include-option) ':INHERIT)
          (setq inherited-slot-count (length slotlist))))
      (if (eq name 'STRUCTURE-OBJECT)
        (setq names (list name)
              namesform `',names)
        (setq names (cons name (svref (get 'STRUCTURE-OBJECT
                                           'DEFSTRUCT-DESCRIPTION) 0))
              namesbinding
              (list
               (list
                (setq namesform (gensym))
                `(CONS ',name (SVREF (GET 'STRUCTURE-OBJECT
                                          'DEFSTRUCT-DESCRIPTION) 0)))))))
    ;; names is the include-nesting, namesform is the form belonging to it.
    ;; slotlist is the former slot list, reversed.
    ;; inherited-slot-count is the number of slots, that have to be ignored
    ;; when the accessors are created.
    (when (and named-option ; named structure
               (consp type-option) ; of type (VECTOR ...)
               ;; must be able to contain the name(s):
               (not (typep names (type-for-discrimination
                                  (second type-option)))))
      (error-of-type 'source-program-error
        (TEXT "~S ~S: structure of type ~S cannot hold the name.")
        'defstruct name type-option))
    ;; layout of the structure:
    ;; names, poss. include-slots, initial-offset-option times NIL, slots.
    ;; layout of vector or list:
    ;; include-part, initial-offset-option times NIL, poss. name, slots.
    (setq initial-offset (+ include-skip initial-offset-option))
    (unless (eq type-option 'T)
      (when named-option
        (push
          (make-ds-slot nil ; tag for the type recognition slot
                        '()
                        (setq initial-offset-option initial-offset)
                        (cons 'NIL name) name ; "default value" = name
                        'SYMBOL ; type = symbol
                        T) ; read-only
          slotlist)
        (incf initial-offset)))
    ;; the slots are situated behind initial-offset.
    ;; If type/=T (i.e vector or list) and named-option, the name is situated
    ;;   in Slot number  initial-offset-option = (1- initial-offset).
    ;; processing the slots:
    (let ((offset initial-offset))
      (dolist (slotarg slotargs)
        (let (slotname
              default)
          (if (atom slotarg)
            (setq slotname slotarg  default nil)
            (setq slotname (first slotarg)  default (second slotarg)))
          (unless (constantp default)
            (push
              `(FUNCTION ,(concat-pnames "DEFAULT-" slotname)
                 (LAMBDA () ,default))
              slotdefaultfuns)
            (setq default (gensym))
            (push default slotdefaultvars))
          ;; Here we compare slot names through their symbol-names, not through
          ;; #'eq, because if we have two slots P::X and Q::X, the two accessor
          ;; functions would have the same name FOO-X.
          (when (find (symbol-name slotname) slotlist
                      :key #'(lambda (slot) (symbol-name (ds-slot-name slot)))
                      :test #'string=)
            (error-of-type 'source-program-error
              (TEXT "~S ~S: There may be only one slot with the name ~S.")
              'defstruct name slotname))
          (when (string= "P" slotname)
            (warn
             (TEXT "~S ~S: Slot ~S accessor will shadow the predicate.")
             'defstruct name slotname))
          (let ((type t) (read-only nil))
            (when (consp slotarg)
              (do ((slot-arglistr (cddr slotarg) (cddr slot-arglistr)))
                  ((endp slot-arglistr))
                (let ((slot-keyword (first slot-arglistr))
                      (slot-key-value (second slot-arglistr)))
                  (cond ((eq slot-keyword ':READ-ONLY)
                         (setq read-only (if slot-key-value t nil)))
                        ((eq slot-keyword ':TYPE) (setq type slot-key-value))
                        (t (error-of-type 'source-program-error
                             (TEXT "~S ~S: ~S is not a slot option.")
                             'defstruct name slot-keyword))))))
            (push (make-ds-slot slotname
                                (if slotname
                                  (list (intern (symbol-name slotname)
                                                *keyword-package*))
                                  '()) ; initargs
                                offset ; location
                                (if (constantp default)
                                  ;; default is a constant
                                  (cons 'NIL (eval default))
                                  ;; default is a gensym
                                  (cons (add-unquote default) 'NIL))
                                default type read-only) ; defstruct specific
                  slotlist)))
        (incf offset))
      (setq size offset))
    ;; size = total length of the structure
    (setq slotlist (nreverse slotlist))
    (setq slotdefaultfuns (nreverse slotdefaultfuns))
    (setq slotdefaultvars (nreverse slotdefaultvars))
    ;; the slots in slotlist are now sorted in ascending order again.
    (setq constructor-forms
      (mapcar
        #'(lambda (constructor-option)
            (if (consp constructor-option)
              (ds-make-boa-constructor
                constructor-option type-option name namesform size slotlist)
              (progn
                (if (null keyword-constructor)
                  (setq keyword-constructor constructor-option))
                (ds-make-keyword-constructor
                 constructor-option type-option name namesform size
                 slotlist))))
        constructor-option-list))
    ;; constructor-forms = list of forms, that define the constructors.
    (let ((index 4))
      (dolist (defaultvar slotdefaultvars)
        (setf (ds-slot-default (find defaultvar slotlist :test #'eq
                                     :key #'(lambda (x) (ds-slot-default x))))
              `(SVREF (GET ',name 'DEFSTRUCT-DESCRIPTION) ,index))
        (incf index)))
    ;; now, slotlist contains no more slotdefaultvars.
    `(EVAL-WHEN (LOAD COMPILE EVAL)
       (LET ()
         (LET ,(append namesbinding (mapcar #'list slotdefaultvars
                                            slotdefaultfuns))
           ,@constructor-forms
           (%PUT ',name 'DEFSTRUCT-DESCRIPTION
                 (VECTOR ,namesform ',type-option ',keyword-constructor
                         ,(add-backquote slotlist)
                         ,@slotdefaultvars)))
         ,@(if (eq type-option 'T) `((CLOS::DEFINE-STRUCTURE-CLASS ',name)))
         ,@(if (and named-option predicate-option)
             (ds-make-pred predicate-option type-option name
                           initial-offset-option))
         ,@(if copier-option (ds-make-copier copier-option name type-option))
         ,@(let ((directslotlist (nthcdr inherited-slot-count slotlist)))
             `(,@(ds-make-accessors name names type-option conc-name-option
                                    directslotlist)
               ,@(ds-make-defsetfs name names type-option conc-name-option
                                   directslotlist)))
         (sys::%set-documentation ',name 'STRUCTURE ,docstring)
         ,@(when (eq type-option 'T)
             (list
               (if print-object-option
                 `(CLOS:DEFMETHOD CLOS:PRINT-OBJECT ((STRUCT ,name) STREAM)
                    (PROGN ,print-object-option))
                 `(CLOS::DEFSTRUCT-REMOVE-PRINT-OBJECT-METHOD ',name))))
         ',name))))

(defstruct (structure-object (:predicate nil) (:copier nil) (:constructor nil)))
