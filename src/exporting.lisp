;;; Macros that export their definiendum
;;; Bruno Haible 2004-12-15

(defpackage "EXPORTING"
  (:use "COMMON-LISP")
  (:shadow . #1=(defconstant defparameter defvar define-symbol-macro
                 defun defgeneric defmethod define-compiler-macro defsetf
                 define-setf-expander defmacro define-modify-macro
                 deftype defstruct defclass define-condition
                 define-method-combination
                 #+FFI def-c-type #+FFI def-c-enum #+FFI def-c-struct
                 #+FFI def-c-var
                 #+FFI def-c-call-out #+FFI def-call-out
                 #+AFFI def-lib-call-out))
  (:export . #1#))

(in-package "EXPORTING")

;; Macros for the variable namespace.

(cl:defmacro defconstant (&whole whole
                          name initial-value &optional documentation)
  (declare (ignore initial-value documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFCONSTANT ,name ,@(cddr whole))))

(cl:defmacro defparameter (&whole whole
                           name initial-value &optional documentation)
  (declare (ignore initial-value documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFPARAMETER ,name ,@(cddr whole))))

(cl:defmacro defvar (&whole whole
                     name &optional initial-value documentation)
  (declare (ignore initial-value documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFVAR ,name ,@(cddr whole))))

(cl:defmacro define-symbol-macro (symbol expansion)
  `(PROGN
     (EXPORT ',(or symbol '(NIL)))
     (CL:DEFINE-SYMBOL-MACRO ,symbol ,expansion)))

;; Macros for the function namespace.

(cl:defmacro defun (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or (sys::function-block-name name) '(NIL)))
     (CL:DEFUN ,name ,lambda-list ,@body)))

(cl:defmacro defgeneric (name lambda-list &rest options)
  `(PROGN
     (EXPORT ',(or (sys::function-block-name name) '(NIL)))
     (CL:DEFGENERIC ,name ,lambda-list ,@options)))

(cl:defmacro defmethod (name &rest definition)
  `(PROGN
     (EXPORT ',(or (sys::function-block-name name) '(NIL)))
     (CL:DEFMETHOD ,name ,@definition)))

(cl:defmacro define-compiler-macro (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or (sys::function-block-name name) '(NIL)))
     (CL:DEFINE-COMPILER-MACRO ,name ,lambda-list ,@body)))

(cl:defmacro defsetf (name &rest definition)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFSETF ,name ,@definition)))

(cl:defmacro define-setf-expander (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFINE-SETF-EXPANDER ,name ,lambda-list ,@body)))

(cl:defmacro defmacro (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFMACRO ,name ,lambda-list ,@body)))

(cl:defmacro define-modify-macro (&whole whole
                                  name lambda-list function &optional documentation)
  (declare (ignore lambda-list function documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFINE-MODIFY-MACRO ,name ,@(cddr whole))))

;; Macros for the type namespace.

(cl:defmacro deftype (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFTYPE ,name ,lambda-list ,@body)))

(cl:defmacro defstruct (name+options &rest slots)
  (let ((name (if (consp name+options) (first name+options) name+options)))
    `(PROGN
       (EXPORT '(,name
                 ,@(let ((constructor-option-list nil)
                         (copier-option 0)
                         (predicate-option 0))
                     (when (consp name+options)
                       (dolist (option (rest name+options))
                         (if (or (eq option ':CONSTRUCTOR) (equal option '(:CONSTRUCTOR)))
                           (push (sys::concat-pnames "MAKE-" name) constructor-option-list)
                           (when (and (consp option) (consp (cdr option)))
                             (case (first option)
                               (:CONSTRUCTOR (push (second option) constructor-option-list))
                               (:COPIER (setq copier-option (second option)))
                               (:PREDICATE (setq predicate-option (second option))))))))
                     (nconc (if constructor-option-list
                              (delete 'NIL constructor-option-list)
                              (list (sys::concat-pnames "MAKE-" name)))
                            (when copier-option
                              (list (if (eql copier-option 0)
                                      (sys::concat-pnames "COPY-" name)
                                      copier-option)))
                            (when predicate-option
                              (list (if (eql predicate-option 0)
                                      (sys::concat-pnames name "-P")
                                      predicate-option)))))
                 ,@(let ((conc-name-option 0))
                     (when (consp name+options)
                       (dolist (option (rest name+options))
                         (when (and (consp option) (consp (cdr option))
                                    (eq (first option) ':CONC-NAME))
                           (setq conc-name-option (second option)))))
                     (when (eql conc-name-option 0)
                       (setq conc-name-option (sys::string-concat (string name) "-")))
                     (mapcar #'(lambda (slot-spec)
                                 (sys::ds-accessor-name
                                   (if (consp slot-spec) (first slot-spec) slot-spec)
                                   conc-name-option))
                             slots))))
       (CL:DEFSTRUCT ,name+options ,@slots))))

(cl:defun slot-definition-accessor-symbols (slot)
  (mapcar #'sys::function-block-name
          (append (clos:slot-definition-readers slot)
                  (clos:slot-definition-writers slot))))

(cl:defun all-accessor-symbols (direct-slot-list)
  (mapcan #'slot-definition-accessor-symbols direct-slot-list))

(cl:defun class-accessor-symbols (class) ; ABI
  (all-accessor-symbols (clos:class-direct-slots class)))

(cl:defmacro defclass (name superclasses slot-specs &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (LET ((C (CL:DEFCLASS ,name ,superclasses ,slot-specs ,@options)))
       (EXPORT (CLASS-ACCESSOR-SYMBOLS C))
       C)))

(cl:defmacro define-condition (name parent-types slot-specs &rest options)
  `(PROGN
     (EXPORT '(,name
               ,@(mapcan #'(lambda (slot-spec)
                             (when (consp slot-spec)
                               (let ((symbols '()))
                                 (do ((slot-options (cdr slot-spec) (cddr slot-options)))
                                     ((endp slot-options))
                                   (when (sys::memq (first slot-options)
                                                    '(:READER :WRITER :ACCESSOR))
                                     (push (sys::function-block-name (second slot-options))
                                           symbols)))
                                 (nreverse symbols))))
                         slot-specs)))
     (CL:DEFINE-CONDITION ,name ,parent-types ,slot-specs ,@options)))

;; Macros for the method-combination namespace.

(cl:defmacro define-method-combination (name &rest definition)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFINE-METHOD-COMBINATION ,name ,@definition)))

;; FFI.

#+FFI
(cl:defmacro def-c-type (name typespec)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-C-TYPE ,name ,typespec)))

#+FFI
(cl:defmacro def-c-enum (name &rest items)
  `(PROGN
     (EXPORT '(,name ,@(mapcar #'(lambda (item) (if (consp item) (first item) item))
                               items)))
     (FFI:DEF-C-ENUM ,name ,@items)))

#+FFI
(cl:defmacro def-c-struct (name+options &rest slots)
  (let ((name (if (consp name+options) (first name+options) name+options)))
    `(PROGN
       (EXPORT '(,name
                 ,(sys::concat-pnames "MAKE-" name)
                 ,(sys::concat-pnames "COPY-" name)
                 ,(sys::concat-pnames "-P" name)
                 ,@(let ((concname (sys::string-concat (string name) "-")))
                     (mapcar #'(lambda (slot)
                                 (let ((slotname (first slot)))
                                   (sys::concat-pnames concname slotname)))
                             slots))))
       (FFI:DEF-C-STRUCT ,name+options ,@slots))))

#+FFI
(cl:defmacro def-c-var (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-C-VAR ,name ,@options)))

#+FFI
(cl:defmacro def-c-call-out (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-C-CALL-OUT ,name ,@options)))

#+FFI
(cl:defmacro def-call-out (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-CALL-OUT ,name ,@options)))

#+AFFI
(cl:defmacro def-lib-call-out (name library &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-LIB-CALL-OUT ,name ,library ,@options)))

#| ;; def-c-call-in and def-call-in don't actually define anything;
   ;; they are more like declarations.

#+FFI
 (cl:defmacro def-c-call-in (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-C-CALL-IN ,name ,@options)))

#+FFI
 (cl:defmacro def-call-in (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-CALL-IN ,name ,@options)))

|#
