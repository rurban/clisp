;;; Sources for CLISP DEFSTRUCT macro
;;; Bruno Haible 1988-2004
;;; Sam Steingold 1998-2004
;;; German comments translated into English: Stefan Kain 2003-01-14

(in-package "SYSTEM")

#| Explanation of the appearing data types:

   (get name 'DEFSTRUCT-DESCRIPTION) =
     #(names type size keyword-constructor slotlist defaultfun0 defaultfun1 ...)

   names is a coding of the INCLUDE-nesting for Structure name:
   names = (name_1 ... name_i-1 name_i) with name=name_1,
     name_1 contains name_2, ..., name_i-1 contains name_i.

   type (if the type of the whole structure is meant):
      = T                      storage as a normal structure
      = LIST                   storage as list
      = VECTOR                 storage as (simple-)vector
      = (VECTOR element-type)  storage as vector with element-type

   size is the structure size / list length / vector length.

   keyword-constructor = NIL or the name of the keyword-constructor

   slotlist is a packed description of the slots of a structure:
   slotlist = ({slot}*)
   slot = an instance of structure-direct-slot-definition, containing:
         name - the slotname,
         initargs - a list containing the initialization argument,
              or NIL for the pseudo-slot containing the structure name in
              named structures,
         offset - the location of the slot in any instance,
         initer = (initform . initfunction) - as usual,
         init-function-form -
              a form (a symbol or a list (SVREF ...)), that yields
              upon evaluation in an arbitrary environment a function,
              that returns the default value, when called.
         type - the declared type for this slot,
         readonly = NIL or = T specifying, if this slot is readonly, i.e.
              after the construction of the Structure the slot cannot be
              changed with (setf ...) anymore.
         (See also pr_structure_default() in io.d.)
   The initializations are specified as follows:
     - not real slot (i.e. initargs = ()):
       initform           = `(QUOTE ,name)
       initfunction       = a constant-initfunction for name
       init-function-form = `(MAKE-CONSTANT-INITFUNCTION ',name)
     - real slot with constant initform:
       initform           = as specified by the user
       initfunction       = a constant-initfunction for the initform's value
       init-function-form = `(MAKE-CONSTANT-INITFUNCTION ,initform)
     - real slot with non-constant initform:
       initform           = as specified by the user
       initfunction       = a closure taking 0 arguments, or nil
       init-function-form = for inherited slots: `(SVREF ...)
                            for direct slots: `(FUNCTION (LAMBDA () ,initform))
                            In both cases, after some processing: a gensym
                            referring to a binding.

   if type = T, the Structure-Name occupies the slot 0, but is not mentioned
     in the slotlist, because there is nothing to do for its initialization.
|#

(defun make-ds-slot (name initargs offset initer initff type readonly)
  (clos::make-instance-<structure-effective-slot-definition>
    clos::<structure-effective-slot-definition>
    :name name
    :initargs initargs
    :initform (car initer) :initfunction (cdr initer) 'clos::inheritable-initer initer
    'clos::initff initff
    :type type
    'clos::readonly readonly
    'clos::location offset))
(defun copy-<structure-effective-slot-definition> (slot)
  (make-ds-slot
    (clos::slot-definition-name slot)
    (clos::slot-definition-initargs slot)
    (clos::slot-definition-location slot)
    (clos::slot-definition-inheritable-initer slot)
    (clos::structure-effective-slot-definition-initff slot)
    (clos::slot-definition-type slot)
    (clos::structure-effective-slot-definition-readonly slot)))
(defmacro ds-real-slot-p (slot)
  `(not (null (clos::slot-definition-initargs ,slot))))
(defmacro ds-pseudo-slot-default (slot)
  ;; The pseudo-slots have an initform = (QUOTE name) and an initfunction which
  ;; returns the name.
  `(funcall (clos::slot-definition-initfunction ,slot)))

#| The type test comes in 4 variants. Keep them in sync! |#

#| Type test, for TYPEP.
   Must be equivalent to (typep object (ds-canonicalize-type symbol)).
|#
(defun ds-typep (object symbol desc)
  (let ((type (svref desc 1)))
    (if (eq type 'T)
      (%STRUCTURE-TYPE-P symbol object)
      (let ((size (svref desc 2)))
        (if (eq type 'LIST)
          (and (conses-p size object)
               (dolist (slot (svref desc 4) t)
                 (unless (ds-real-slot-p slot)
                   (unless (eq (nth (clos::slot-definition-location slot) object)
                               (ds-pseudo-slot-default slot))
                     (return nil)))))
          (and (vectorp object) (simple-array-p object)
               (>= (length object) size)
               (equal (array-element-type object)
                      (if (consp type)
                        (upgraded-array-element-type (second type))
                        'T))
               (dolist (slot (svref desc 4) t)
                 (unless (ds-real-slot-p slot)
                   (unless (and (simple-vector-p object)
                                (eq (svref object (clos::slot-definition-location slot))
                                    (ds-pseudo-slot-default slot)))
                     (return nil))))))))))

#| Type test expansion, for TYPEP compiler macro. |#
(defun ds-typep-expansion (objform symbol desc)
  (let ((type (svref desc 1)))
    (if (eq type 'T)
      `(%STRUCTURE-TYPE-P ',symbol ,objform)
      (let ((size (svref desc 2))
            (tmp (gensym)))
        `(LET ((,tmp ,objform))
           ,(if (eq type 'LIST)
              `(AND ,@(case size
                        (0 '())
                        (1 `((CONSP ,tmp)))
                        (t `((CONSES-P ,size ,tmp))))
                    ,@(mapcan #'(lambda (slot)
                                  (unless (ds-real-slot-p slot)
                                    `((EQ (NTH ,(clos::slot-definition-location slot) ,tmp)
                                          ',(ds-pseudo-slot-default slot)))))
                              (svref desc 4)))
              (let ((eltype (if (consp type)
                              (upgraded-array-element-type (second type))
                              'T)))
                `(AND ,@(if (eq eltype 'T)
                          `((SIMPLE-VECTOR-P ,tmp))
                          `((VECTORP ,tmp)
                            (SIMPLE-ARRAY-P ,tmp)
                            (EQUAL (ARRAY-ELEMENT-TYPE ,tmp) ',eltype)))
                      ,(case size
                         (0 'T)
                         (t `(>= (LENGTH ,tmp) ,size)))
                      ,@(mapcan #'(lambda (slot)
                                    (unless (ds-real-slot-p slot)
                                      `((EQ (SVREF ,tmp ,(clos::slot-definition-location slot))
                                            ',(ds-pseudo-slot-default slot)))))
                                (svref desc 4))))))))))

#| Type canonicalization, for SUBTYPEP. |#
(defun ds-canonicalize-type (symbol)
  (let ((desc (get symbol 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (let ((type (svref desc 1)))
        (if (eq type 'T)
          symbol
          (let ((size (svref desc 2))
                (slotlist (svref desc 4)))
            (if (eq type 'LIST)
              (let ((resulttype 'T))
                ;; Start with T, not (MEMBER NIL), because of the possibility
                ;; of subclasses.
                (dotimes (i size) (setq resulttype (list 'CONS 'T resulttype)))
                (dolist (slot slotlist)
                  (unless (ds-real-slot-p slot)
                    (let ((resttype resulttype))
                      (dotimes (j (clos::slot-definition-location slot))
                        (setq resttype (third resttype)))
                      (setf (second resttype) `(EQL ,(ds-pseudo-slot-default slot))))))
                resulttype)
              `(AND (SIMPLE-ARRAY ,(if (consp type) (second type) 'T) (*))
                    ;; Constraints that cannot be represented through ANSI CL
                    ;; type specifiers. We use SATISFIES types with uninterned
                    ;; symbols. This is possible because this function is only
                    ;; used for SUBTYPEP.
                    ,@(when (or (plusp size)
                                (some #'(lambda (slot) (not (ds-real-slot-p slot)))
                                      slotlist))
                        (let ((constraint-name (gensym)))
                          (setf (symbol-function constraint-name)
                                #'(lambda (x) (typep x symbol)))
                          `((SATISFIES ,constraint-name)))))))))
      ; The DEFSTRUCT-DESCRIPTION was lost.
      'NIL)))

#| (ds-make-pred predname type name slotlist size)
   returns the form, that creates the type-test-predicate for
   the structure name.

   type         the type of the structure,
   name         the name of the structure,
   predname     the name of the type-test-predicate,
   slotlist     (only used when type /= T) list of slots
   size         instance size
|#
(defun ds-make-pred (predname type name slotlist size)
  `(,@(if (eq type 'T) `((PROCLAIM '(INLINE ,predname))) '())
    (DEFUN ,predname (OBJECT)
      ,(if (eq type 'T)
         `(%STRUCTURE-TYPE-P ',name OBJECT)
         (let ((max-offset -1)
               (max-name-offset -1))
           (dolist (slot slotlist)
             (setq max-offset (max max-offset (clos::slot-definition-location slot)))
             (unless (ds-real-slot-p slot)
               (setq max-name-offset (max max-name-offset (clos::slot-definition-location slot)))))
           ; This code is only used when there is at least one named slot.
           (assert (<= 0 max-name-offset max-offset))
           (assert (< max-offset size))
           (if (eq type 'LIST)
             `(AND ,@(case size
                       (0 '())
                       (1 `((CONSP OBJECT)))
                       (t `((CONSES-P ,size OBJECT))))
                   ,@(mapcan #'(lambda (slot)
                                 (unless (ds-real-slot-p slot)
                                   `((EQ (NTH ,(clos::slot-definition-location slot) OBJECT)
                                         ',(ds-pseudo-slot-default slot)))))
                             slotlist))
             ; This code is only used when there is at least one named slot.
             ; Therefore the vector's upgraded element type must contain
             ; SYMBOL, i.e. it must be a general vector.
             `(AND (SIMPLE-VECTOR-P OBJECT)
                   (>= (LENGTH OBJECT) ,size)
                   ,@(mapcan #'(lambda (slot)
                                 (unless (ds-real-slot-p slot)
                                   `((EQ (SVREF OBJECT ,(clos::slot-definition-location slot))
                                         ',(ds-pseudo-slot-default slot)))))
                             slotlist))))))))

#| auxiliary function for both constructors:
   (ds-arg-default arg slot)
   returns for an argument arg (part of the argument list) the part of
   the argument list, that binds this argument with the default for slot.
|#

(defun ds-arg-default (arg slot)
  (let ((initer (clos::slot-definition-inheritable-initer slot)))
    `(,arg
      ,(if (constant-initfunction-p (cdr initer)) ; equivalent to (constantp (car initer))
         (car initer) ; initform
         `(FUNCALL ,(clos::structure-effective-slot-definition-initff slot))))))

#| auxiliary function for both constructors:
   (ds-make-constructor-body type name names size slotlist get-var)
   returns the expression, that creates and fills a structure
   of given type.
|#
(defun ds-make-constructor-body (type name names size slotlist varlist)
  (if (and (or (eq type 'VECTOR) (eq type 'LIST))
           (do ((slotlistr slotlist (cdr slotlistr))
                (index 0 (1+ index)))
               ((null slotlistr) (eql index size))
             (unless (eq (clos::slot-definition-location (car slotlistr)) index)
               (return nil))))
    ;; optimize the simple case
    `(,type ,@(mapcar #'(lambda (slot var)
                          (if (ds-real-slot-p slot)
                            `(THE ,(clos::slot-definition-type slot) ,var)
                            `(QUOTE ,(ds-pseudo-slot-default slot))))
                       slotlist varlist))
    `(LET ((OBJECT
             ,(cond ((eq type 'T) `(%MAKE-STRUCTURE ,names ,size))
                    ((eq type 'LIST) `(MAKE-LIST ,size))
                    ((consp type)
                     `(MAKE-ARRAY ,size :ELEMENT-TYPE ',(second type)))
                    (t `(MAKE-ARRAY ,size)))))
       ,@(mapcar
          #'(lambda (slot var &aux (offset (clos::slot-definition-location slot)))
              `(SETF
                ,(cond ((eq type 'T)
                        `(%STRUCTURE-REF ',name OBJECT ,offset) )
                       ((eq type 'LIST)
                        `(NTH ,offset OBJECT) )
                       ((eq type 'VECTOR)
                        `(SVREF OBJECT ,offset) )
                       (t `(AREF OBJECT ,offset) ))
                ,(if (ds-real-slot-p slot)
                   `(THE ,(clos::slot-definition-type slot) ,var)
                   `(QUOTE ,(ds-pseudo-slot-default slot)))))
           slotlist varlist)
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
                       :key #'clos::slot-definition-name :test #'eq)))
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
                    (when (ds-real-slot-p slot)
                      (unless (memq (clos::slot-definition-name slot) argnames)
                        (push (ds-arg-with-default
                                (clos::slot-definition-name slot) slotlist)
                              slotinitlist))))
                  (nreverse slotinitlist)))))
      `(DEFUN ,constructorname ,new-arglist
         ,(ds-make-constructor-body type name names size slotlist
                                    (mapcar #'clos::slot-definition-name slotlist))))))

#| (ds-make-keyword-constructor descriptor type name names size slotlist)
   returns the form, that defines the keyword-constructor. |#
(defun ds-make-keyword-constructor (descriptor type name names size slotlist)
  (let ((varlist
          (mapcar #'(lambda (slot)
                      (if (ds-real-slot-p slot)
                        (make-symbol
                          (symbol-name (clos::slot-definition-name slot)))
                        nil))
                  slotlist)))
    `(DEFUN ,descriptor
       (&KEY
        ,@(mapcan
            #'(lambda (slot var)
                (if (ds-real-slot-p slot)
                  (list (ds-arg-default var slot))
                  '()))
            slotlist varlist))
       ,(ds-make-constructor-body type name names size slotlist varlist))))

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
        (if (ds-real-slot-p slot)
          (let ((accessorname
                  (if concname
                    (concat-pnames concname (clos::slot-definition-name slot))
                    (clos::slot-definition-name slot)))
                (offset (clos::slot-definition-location slot))
                (slottype (clos::slot-definition-type slot)))
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
        (if (and (ds-real-slot-p slot) (not (clos::structure-effective-slot-definition-readonly slot)))
          (let ((accessorname
                  (if concname
                    (concat-pnames concname (clos::slot-definition-name slot))
                    (clos::slot-definition-name slot)))
                (offset (clos::slot-definition-location slot))
                (slottype (clos::slot-definition-type slot)))
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
(defun clos::undefine-structure-class (name) ; preliminary
  (declare (ignore name)))
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
        (slotdefaultslots                  nil)
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
            (mapcar #'copy-<structure-effective-slot-definition> (svref incl-desc 4))))
        ;; slotlist is the reversed list of the inherited slots.
        (setq include-skip (svref incl-desc 2))
        (when slotlist
          (assert (> include-skip (clos::slot-definition-location (first slotlist)))))
        ;; include-skip >=0 is the number of slots that are already consumend
        ;;    by the substructure, the "size" of the substructure.
        ;; process further arguments of the :INCLUDE-option:
        (dolist (slotarg (rest option))
          (let* ((slotname (if (atom slotarg) slotarg (first slotarg)))
                 (slot (find slotname slotlist :key #'clos::slot-definition-name
                             :test #'eq)))
            (when (null slot)
              (error-of-type 'source-program-error
                (TEXT "~S ~S: included structure ~S has no component with name ~S.")
                'defstruct name subname slotname))
            (if (atom slotarg)
              ; overwrite default to NIL
              (progn
                (setf (clos::slot-definition-inheritable-initer slot)
                      (cons 'NIL (make-constant-initfunction 'NIL)))
                (setf (clos::structure-effective-slot-definition-initff slot)
                      `(MAKE-CONSTANT-INITFUNCTION NIL)))
              (progn
                (let ((initform (second slotarg)))
                  (if (constantp initform)
                    (progn
                      (setf (clos::slot-definition-inheritable-initer slot)
                            (cons initform (make-constant-initfunction (eval initform))))
                      (setf (clos::structure-effective-slot-definition-initff slot)
                            `(MAKE-CONSTANT-INITFUNCTION ,initform)))
                    (let ((variable (gensym)))
                      (push
                        `(FUNCTION ,(concat-pnames "DEFAULT-" slotname)
                           (LAMBDA () ,initform))
                        slotdefaultfuns)
                      (push variable slotdefaultvars)
                      (push slot slotdefaultslots)
                      (setf (clos::slot-definition-inheritable-initer slot)
                            (cons initform nil)) ; FIXME
                      (setf (clos::structure-effective-slot-definition-initff slot)
                            variable))))
                ;; process the slot-options of this Slot-Specifier:
                (do ((slot-arglistr (cddr slotarg) (cddr slot-arglistr)))
                    ((endp slot-arglistr))
                  (let ((slot-keyword (first slot-arglistr))
                        (slot-key-value (second slot-arglistr)))
                    (cond ((eq slot-keyword ':READ-ONLY)
                           (if slot-key-value
                             (setf (clos::structure-effective-slot-definition-readonly slot) t)
                             (if (clos::structure-effective-slot-definition-readonly slot)
                               (error-of-type 'source-program-error
                                 (TEXT "~S ~S: The READ-ONLY slot ~S of the included structure ~S must remain READ-ONLY in ~S.")
                                 'defstruct name slotname subname name)
                               (setf (clos::structure-effective-slot-definition-readonly slot) nil))))
                          ((eq slot-keyword ':TYPE)
                           (unless
                               (subtypep
                                 (type-for-discrimination slot-key-value)
                                 (type-for-discrimination (clos::slot-definition-type slot)))
                             (error-of-type 'source-program-error
                               (TEXT "~S ~S: The type ~S of slot ~S should be a subtype of the type defined for the included strucure ~S, namely ~S.")
                               'defstruct name slot-key-value slotname subname
                               (clos::slot-definition-type slot)))
                           (setf (clos::slot-definition-type slot) slot-key-value))
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
          ; the type recognition pseudo-slot
          (make-ds-slot nil
                        '()
                        initial-offset
                        (cons `(QUOTE ,name) (make-constant-initfunction name))
                        `(MAKE-CONSTANT-INITFUNCTION ',name)
                        'SYMBOL ; type = symbol
                        T)      ; read-only
          slotlist)
        (incf initial-offset)))
    ;; the slots are situated behind initial-offset.
    ;; If type/=T (i.e vector or list) and named-option, the name is situated
    ;;   in Slot number  (1- initial-offset).
    ;; processing the slots:
    (let ((offset initial-offset))
      (dolist (slotarg slotargs)
        (let (slotname
              initform
              initfunction
              initfunctionform)
          (if (atom slotarg)
            (setq slotname slotarg  initform nil)
            (setq slotname (first slotarg)  initform (second slotarg)))
          ;; Here we compare slot names through their symbol-names, not through
          ;; #'eq, because if we have two slots P::X and Q::X, the two accessor
          ;; functions would have the same name FOO-X.
          (when (find (symbol-name slotname) slotlist
                      :test #'(lambda (name slot)
                                (and (ds-real-slot-p slot)
                                     (string= (clos::slot-definition-name slot) name))))
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
            (if (constantp initform)
              (setq initfunction (make-constant-initfunction (eval initform))
                    initfunctionform `(MAKE-CONSTANT-INITFUNCTION ,initform))
              (let ((variable (gensym)))
                (push
                  `(FUNCTION ,(concat-pnames "DEFAULT-" slotname)
                     (LAMBDA () ,initform))
                  slotdefaultfuns)
                (push variable slotdefaultvars)
                (setq initfunction nil ; FIXME
                      initfunctionform variable)))
            (push (make-ds-slot slotname
                                (list (intern (symbol-name slotname)
                                              *keyword-package*)) ; initargs
                                offset ; location
                                (cons initform initfunction)
                                initfunctionform
                                ;; The following are defstruct specific.
                                type read-only)
                  slotlist)
            (unless (constantp initform)
              (push (car slotlist) slotdefaultslots))))
        (incf offset))
      (setq size offset))
    ;; size = total length of the structure
    (setq slotlist (nreverse slotlist))
    (setq slotdefaultfuns (nreverse slotdefaultfuns))
    (setq slotdefaultvars (nreverse slotdefaultvars))
    (setq slotdefaultslots (nreverse slotdefaultslots))
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
    (let ((index 5))
      (mapc #'(lambda (defaultvar slot)
                (setf (clos::structure-effective-slot-definition-initff slot)
                      `(SVREF (GET ',name 'DEFSTRUCT-DESCRIPTION) ,index))
                (incf index))
            slotdefaultvars slotdefaultslots))
    ;; now, slotlist contains no more slotdefaultvars.
    `(EVAL-WHEN (LOAD COMPILE EVAL)
       (LET ()
         (LET ,(append namesbinding (mapcar #'list slotdefaultvars slotdefaultfuns))
           ,@constructor-forms
           (%PUT ',name 'DEFSTRUCT-DESCRIPTION
                 (VECTOR ,namesform ',type-option ,size ',keyword-constructor
                         (LIST
                           ,@(mapcar #'(lambda (slot)
                                         (clos::make-load-form-<structure-effective-slot-definition>
                                           slot
                                           (let ((i (position slot slotdefaultslots)))
                                             (if i (nth i slotdefaultvars) nil))))
                                     slotlist))
                         ,@slotdefaultvars)))
         ,(if (eq type-option 'T)
            `(CLOS::DEFINE-STRUCTURE-CLASS ',name)
            `(CLOS::UNDEFINE-STRUCTURE-CLASS ',name))
         ,@(if (and named-option predicate-option)
             (ds-make-pred predicate-option type-option name slotlist size))
         ,@(if copier-option (ds-make-copier copier-option name type-option))
         ,@(let ((directslotlist (nthcdr inherited-slot-count slotlist)))
             `(,@(ds-make-accessors name names type-option conc-name-option
                                    directslotlist)
               ,@(ds-make-defsetfs name names type-option conc-name-option
                                   directslotlist)))
         ;; see documentation.lisp: we map STRUCTURE to TYPE
         (sys::%set-documentation ',name 'TYPE ,docstring)
         ,@(when (eq type-option 'T)
             (list
               (if print-object-option
                 `(CLOS:DEFMETHOD CLOS:PRINT-OBJECT ((STRUCT ,name) STREAM)
                    (PROGN ,print-object-option))
                 `(CLOS::DEFSTRUCT-REMOVE-PRINT-OBJECT-METHOD ',name))))
         ',name))))

(defstruct (structure-object (:predicate nil) (:copier nil) (:constructor nil)))
