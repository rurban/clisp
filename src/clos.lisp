;;;; Common Lisp Object System für CLISP
;;;; Bruno Haible 21.8.1993

; Zur Benutzung reicht ein einfaches (USE-PACKAGE "CLOS").


(in-package "LISP")
(pushnew ':clos *features*)


(in-package "SYSTEM") ; Trotz DEFPACKAGE nötig!

(defpackage "CLOS"

(:import-from "SYSTEM"
  ;; Import:
  sys::error-of-type                                 ; in error.d definiert
  sys::function-name-p                               ; in control.d definiert
  sys::function-block-name                           ; in init.lsp definiert
; clos::generic-function-p                           ; in predtype.d definiert
; clos::class-p clos:class-of clos:find-class        ; in predtype.d definiert
; clos::structure-object-p                           ; in record.d definiert
; clos::std-instance-p clos::allocate-std-instance   ; in record.d definiert
; clos::%allocate-instance                           ; in record.d definiert
; clos:slot-value clos::set-slot-value               ; in record.d definiert
; clos:slot-boundp clos:slot-makunbound              ; in record.d definiert
; clos:slot-exists-p                                 ; in record.d definiert
; clos::class-gethash clos::class-tuple-gethash      ; in hashtabl.d definiert
  compiler::memq compiler::*keyword-package*         ; in compiler.lsp definiert
  compiler::%generic-function-lambda                 ; in compiler.lsp definiert
  compiler::%optimize-function-lambda                ; in compiler.lsp definiert
; clos:generic-flet clos:generic-labels              ; in compiler.lsp behandelt
  ;; Export:
; clos::closclass   ; als Property in predtype.d, type.lsp, compiler.lsp benutzt
; clos:class                     ; in record.d benutzt
; clos:generic-function          ; in type.lsp, compiler.lsp benutzt
; clos:standard-generic-function ; in predtype.d, type.lsp, compiler.lsp benutzt
; clos:slot-missing clos:slot-unbound  ; von record.d aufgerufen
; clos::*make-instance-table*          ; von record.d benutzt
; clos::*reinitialize-instance-table*  ; von record.d benutzt
; clos::initial-reinitialize-instance  ; von record.d aufgerufen
; clos::initial-initialize-instance    ; von record.d aufgerufen
; clos::initial-make-instance          ; von record.d aufgerufen
; clos:print-object                    ; von io.d aufgerufen
; clos:describe-object                 ; von user2.lsp aufgerufen
; clos::define-structure-class         ; von defstruc.lsp aufgerufen
; clos::defstruct-remove-print-object-method ; von defstruc.lsp aufgerufen
; clos::built-in-class-p               ; von type.lsp aufgerufen
; clos::subclassp                      ; von type.lsp aufgerufen, in compiler.lsp benutzt
; clos:class-name                      ; in type.lsp, compiler.lsp benutzt
; clos:find-class                      ; in compiler.lsp benutzt
; clos::defgeneric-lambdalist-callinfo ; von compiler.lsp aufgerufen
; clos::make-generic-function-form     ; von compiler.lsp aufgerufen
)

) ; defpackage

(in-package "CLOS")

;;; Exportierungen: ** auch in init.lsp ** !
(export '(
  ;; Namen von Funktionen und Macros:
  slot-value slot-boundp slot-makunbound slot-exists-p with-slots with-accessors
  find-class class-of defclass defmethod call-next-method next-method-p
  defgeneric generic-function generic-flet generic-labels
  class-name
  no-applicable-method no-primary-method no-next-method
  find-method add-method remove-method
  compute-applicable-methods method-qualifiers function-keywords
  slot-missing slot-unbound
  print-object describe-object
  make-instance allocate-instance initialize-instance reinitialize-instance
  shared-initialize
  ;; Namen von Klassen:
  standard-class structure-class built-in-class
  standard-object
  generic-function standard-generic-function method standard-method
  ;; andere Symbole:
  standard ; Methoden-Kombination
))


;;; Vorbemerkungen:

;; Abkürzungen:
;; std = standard
;; gf = generic function
;; <...> = (class ...), meist = (find-class '...)
;; em = effective method


;;; Vordefinierte Klassen:
; Metaklassen:
(defvar <class>)                       ; hier <structure-class>
(defvar <standard-class>)              ; hier <structure-class>
(defvar <structure-class>)             ; hier <structure-class>
(defvar <built-in-class>)              ; hier <structure-class>
; Klassen:
(defvar <standard-object>)             ; <standard-class>
(defvar <structure-object>)            ; <structure-class>
(defvar <generic-function>)            ; <built-in-class>
(defvar <standard-generic-function>)   ; <built-in-class>
;(defvar <method>)                     ; hier <structure-class>
;(defvar <standard-method>)            ; hier <structure-class>
(defvar <array>)                       ; <built-in-class>
(defvar <bit-vector>)                  ; <built-in-class>
(defvar <character>)                   ; <built-in-class>
(defvar <complex>)                     ; <built-in-class>
(defvar <cons>)                        ; <built-in-class>
(defvar <float>)                       ; <built-in-class>
(defvar <function>)                    ; <built-in-class>
(defvar <hash-table>)                  ; <built-in-class>
(defvar <integer>)                     ; <built-in-class>
(defvar <list>)                        ; <built-in-class>
(defvar <null>)                        ; <built-in-class>
(defvar <number>)                      ; <built-in-class>
(defvar <package>)                     ; <built-in-class>
(defvar <pathname>)                    ; <built-in-class>
#+LOGICAL-PATHNAMES
(defvar <logical-pathname>)            ; <built-in-class>
(defvar <random-state>)                ; <built-in-class>
(defvar <ratio>)                       ; <built-in-class>
(defvar <rational>)                    ; <built-in-class>
(defvar <readtable>)                   ; <built-in-class>
(defvar <real>)                        ; <built-in-class>
(defvar <sequence>)                    ; <built-in-class>
(defvar <stream>)                      ; <built-in-class>
(defvar <file-stream>)                 ; <built-in-class>
(defvar <synonym-stream>)              ; <built-in-class>
(defvar <broadcast-stream>)            ; <built-in-class>
(defvar <concatenated-stream>)         ; <built-in-class>
(defvar <two-way-stream>)              ; <built-in-class>
(defvar <echo-stream>)                 ; <built-in-class>
(defvar <string-stream>)               ; <built-in-class>
(defvar <string>)                      ; <built-in-class>
(defvar <symbol>)                      ; <built-in-class>
(defvar <t>)                           ; <built-in-class>
(defvar <vector>)                      ; <built-in-class>


;;; Low-Level-Repräsentation:

;; Im Runtime-System gibt es den Typ "CLOS-Instanz".
;; Erste Komponente ist die Klasse.

;; Klassen sind Structures vom Typ CLASS,
;;   erste Komponente ist die Metaklasse, zweite Komponente der Name.

;; Der "Wert" eines Slots, der unbound ist, ist #<UNBOUND> - was sonst?

;; siehe RECORD.D :
; (STD-INSTANCE-P obj) testet, ob ein Objekt eine CLOS-Instanz ist.
; (ALLOCATE-STD-INSTANCE class n) liefert eine CLOS-Instanz mit Klasse class
; und n-1 zusätzlichen Slots.
;; siehe IO.D :
; CLOS-Instanzen werden via (PRINT-OBJECT object stream) ausgegeben.


;;; globale Verwaltung von Klassen und ihren Namen:

#| ; siehe PREDTYPE.D
(defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment)) ; was sollte das Environment bedeuten?
  (unless (symbolp symbol)
    (error-of-type 'type-error
      :datum symbol :expected-type 'symbol
      (ENGLISH "~S: argument ~S is not a symbol")
      'find-class symbol
  ) )
  (let ((class (get symbol 'CLOSCLASS)))
    (if (not (class-p class))
      (if errorp
        (error-of-type 'error
          (ENGLISH "~S: ~S does not name a class")
          'find-class symbol
        )
        nil
      )
      class
) ) )
|#

(defun (setf find-class) (new-value symbol &optional errorp environment)
  (declare (ignore errorp environment)) ; was sollte das Environment bedeuten?
  (unless (symbolp symbol)
    (error-of-type 'type-error
      :datum symbol :expected-type 'symbol
      (ENGLISH "~S: argument ~S is not a symbol")
      '(setf find-class) symbol
  ) )
  (unless (class-p new-value)
    (error-of-type 'type-error
      :datum new-value :expected-type 'class
      (ENGLISH "~S: ~S is not a class")
      '(setf find-class) new-value
  ) )
  (let ((h (get symbol 'CLOSCLASS)))
    (when (class-p h)
      (when (and (built-in-class-p h) (eq (class-name h) symbol)) ; auch Structure-Klassen schützen??
        (error-of-type 'error
          (ENGLISH "~S: cannot redefine built-in class ~S")
          '(setf find-class) h
      ) )
      (when (sys::exported-lisp-symbol-p symbol)
        (cerror (ENGLISH "The old definition will be lost")
                (ENGLISH "~S: Redefining the COMMON LISP class ~S")
                '(setf find-class) symbol
      ) )
      ; Sollte man (setf (class-name h) nil) machen??
  ) )
  (setf (get symbol 'CLOSCLASS) new-value)
)

; (CLASS-OF object) siehe PREDTYPE.D, benutzt Property CLASS.


;;; Slots:

#|
;; So könnten die Zugriffsfunktionen aussehen, wenn man SLOT-VALUE-USING-CLASS
;; verwendet.

; Zugriff auf Slots von Objekten der Metaklasse <standard-class>:
(defun std-slot-value (instance slot-name)
  (declare (compile))
  (let* ((class (class-of instance))
         (slot-location (gethash slot-name (class-slot-location-table class))))
    ((lambda (value)
       (if (eq value unbound)
         (slot-unbound class instance slot-name)
         value
     ) )
     (cond ((null slot-location)
            (slot-missing class instance slot-name 'slot-value)
           )
           ((atom slot-location)
            ; access local slot
            (sys::%record-ref instance slot-location)
           )
           (t
            ; access shared slot
            (svref (class-shared-slots (car slot-location)) (cdr slot-location))
           )
    ))
) )
(defun std-setf-slot-value (instance slot-name new-value)
  (let* ((class (class-of instance))
         (slot-location (gethash slot-name (class-slot-location-table class))))
    (cond ((null slot-location)
           (slot-missing class instance slot-name 'setf new-value)
          )
          ((atom slot-location)
           ; access local slot
           (sys::%record-store instance slot-location new-value)
          )
          (t
           ; access shared slot
           (setf (svref (class-shared-slots (car slot-location)) (cdr slot-location))
                 new-value
          ))
) ) )
(defun std-slot-boundp (instance slot-name)
  (declare (compile))
  (let* ((class (class-of instance))
         (slot-location (gethash slot-name (class-slot-location-table class))))
    (cond ((null slot-location)
           (slot-missing class instance slot-name 'slot-boundp)
          )
          ((atom slot-location)
           ; access local slot
           (not (eq (sys::%record-ref instance slot-location) unbound))
          )
          (t
           ; access shared slot
           (not (eq (svref (class-shared-slots (car slot-location)) (cdr slot-location)) unbound))
          )
) ) )
(defun std-slot-makunbound (instance slot-name)
  (declare (compile))
  (let* ((class (class-of instance))
         (slot-location (gethash slot-name (class-slot-location-table class))))
    (cond ((null slot-location)
           (slot-missing class instance slot-name 'slot-makunbound)
          )
          ((atom slot-location)
           ; access local slot
           (sys::%record-store instance slot-location unbound)
          )
          (t
           ; access shared slot
           (setf (svref (class-shared-slots (car slot-location)) (cdr slot-location))
                 unbound
          ))
) ) )
(defun std-slot-exists-p (instance slot-name)
  (and (gethash slot-name (class-slot-location-table (class-of instance))) t)
)

;; Zugriff auf Slots allgemein:
(defun slot-value (object slot-name)
  (let ((class (class-of object)))
    ; Metaklasse <standard-class> gesondert betrachten
    ; aus Effizienzgründen und wegen Bootstrapping
    (if (eq (class-of class) <standard-class>)
      (std-slot-value object slot-name)
      (slot-value-using-class class object slot-name)
) ) )
(defun (setf slot-value) (new-value object slot-name)
  (let ((class (class-of object)))
    ; Metaklasse <standard-class> gesondert betrachten
    ; aus Effizienzgründen und wegen Bootstrapping
    (if (eq (class-of class) <standard-class>)
      (std-setf-slot-value object slot-name new-value)
      (setf-slot-value-using-class new-value class object slot-name)
) ) )
(defun slot-boundp (object slot-name)
  (let ((class (class-of object)))
    ; Metaklasse <standard-class> gesondert betrachten
    ; aus Effizienzgründen und wegen Bootstrapping
    (if (eq (class-of class) <standard-class>)
      (std-slot-boundp object slot-name)
      (slot-boundp-using-class class object slot-name)
) ) )
(defun slot-makunbound (object slot-name)
  (let ((class (class-of object)))
    ; Metaklasse <standard-class> gesondert betrachten
    ; aus Effizienzgründen und wegen Bootstrapping
    (if (eq (class-of class) <standard-class>)
      (std-slot-makunbound object slot-name)
      (slot-makunbound-using-class class object slot-name)
) ) )
(defun slot-exists-p (object slot-name)
  (let ((class (class-of object)))
    ; Metaklasse <standard-class> gesondert betrachten
    ; aus Effizienzgründen und wegen Bootstrapping
    (if (eq (class-of class) <standard-class>)
      (std-slot-exists-p object slot-name)
      (slot-exists-p-using-class class object slot-name)
) ) )

(defun slot-value-using-class (class object slot-name)
  (no-slot-error class object slot-name)
)
(defun setf-slot-value-using-class (new-value class object slot-name)
  (declare (ignore new-value))
  (no-slot-error class object slot-name)
)
(defun slot-boundp-using-class (class object slot-name)
  (no-slot-error class object slot-name)
)
(defun slot-makunbound-using-class (class object slot-name)
  (no-slot-error class object slot-name)
)
(defun slot-exists-p-using-class (class object slot-name)
  (no-slot-error class object slot-name)
)

(defun no-slot-error (class object slot-name)
  (declare (ignore slot-name))
  (error-of-type 'error
    (ENGLISH "instance ~S of class ~S has no slots (wrong metaclass)")
    object class
) )
|#

;; Der Effizienz halber - wir wollen den Test auf <standard-class> umgehen -
;; bekommen alle Klassen (egal ob standard- oder built-in-) eine
;; slot-location-table. Außerdem können wir hier mit unbound schlecht umgehen.
;; Daher sind
;;   slot-value, set-slot-value, slot-boundp, slot-makunbound, slot-exists-p
;; nun bereits in RECORD.D enthalten.

(defsetf slot-value set-slot-value)

;; WITH-SLOTS

(defmacro with-slots (slot-entries instance-form &body body &environment env)
  (let ((vars '())
        (slots '()))
    (unless (listp slot-entries)
      (error-of-type 'sys::source-program-error
        (ENGLISH "~S: not a list of slots: ~S")
        'with-slots slot-entries
    ) )
    (dolist (slot slot-entries)
      (let ((var slot))
        (when (consp slot)
          (unless (eql (length slot) 2)
            (error-of-type 'sys::source-program-error
              (ENGLISH "~S: invalid slot and variable specification ~S")
              'with-slots slot
          ) )
          (setq var (first slot) slot (second slot))
          (unless (symbolp var)
            (error-of-type 'sys::source-program-error
              (ENGLISH "~S: variable ~S should be a symbol")
              'with-slots var
          ) )
        )
        (unless (symbolp slot)
          (error-of-type 'sys::source-program-error
            (ENGLISH "~S: slot name ~S should be a symbol")
            'with-slots slot
        ) )
        (push var vars)
        (push slot slots)
    ) )
    (multiple-value-bind (body-rest declarations) (sys::parse-body body nil env)
      (let ((instance-var (gensym)))
        `(LET ((,instance-var ,instance-form))
           (SYMBOL-MACROLET
             ,(mapcar #'(lambda (var slot)
                          `(,var (SLOT-VALUE ,instance-var ',slot))
                        )
                      (nreverse vars) (nreverse slots)
              )
             ,@(if declarations `((DECLARE ,@declarations)))
             ,@body-rest
         ) )
) ) ) )

;; WITH-ACCESSORS

(defmacro with-accessors (slot-entries instance-form &body body &environment env)
  (unless (listp slot-entries)
    (error-of-type 'sys::source-program-error
      (ENGLISH "~S: not a list of slots: ~S")
      'with-accessors slot-entries
  ) )
  (dolist (slot-entry slot-entries)
    (unless (and (consp slot-entry) (eql (length slot-entry) 2))
      (error-of-type 'sys::source-program-error
        (ENGLISH "~S: invalid slot and accessor specification ~S")
        'with-accessors slot-entry
    ) )
    (unless (symbolp (first slot-entry))
      (error-of-type 'sys::source-program-error
        (ENGLISH "~S: variable ~S should be a symbol")
        'with-accessors (first slot-entry)
    ) )
    (unless (symbolp (second slot-entry))
      (error-of-type 'sys::source-program-error
        (ENGLISH "~S: accessor name ~S should be a symbol")
        'with-accessors (second slot-entry)
    ) )
  )
  (multiple-value-bind (body-rest declarations) (sys::parse-body body nil env)
    (let ((instance-var (gensym)))
      `(LET ((,instance-var ,instance-form))
         (SYMBOL-MACROLET
           ,(mapcar #'(lambda (slot-entry)
                        `(,(first slot-entry) (,(second slot-entry) ,instance-var))
                      )
                    slot-entries
            )
           ,@(if declarations `((DECLARE ,@declarations)))
           ,@body-rest
       ) )
) ) )


;;; Klassen

; zum Bootstrappen
(eval-when (compile load eval)
  (defun define-structure-class (name) (declare (ignore name)) ) ; vorläufig
  (defun defstruct-remove-print-object-method (name) (declare (ignore name)) ) ; vorläufig
)
; alle Spuren eines früher geladenen CLOS ausmerzen
(eval-when (load eval)
  (do-all-symbols (s) (remprop s 'CLOSCLASS))
)

(defconstant empty-ht (make-hash-table :test #'eq :size 0))

(defstruct (class (:predicate nil)) ; (:print-object print-class) s.u.
  metaclass ; (class-of class) = (class-metaclass class), eine Klasse
  classname ; (class-name class) = (class-classname class), ein Symbol
  direct-superclasses ; Liste aller direkten Oberklassen
  all-superclasses ; Hash-Tabelle aller Oberklassen (inkl. der Klasse selbst)
  precedence-list ; angeordnete Liste aller Oberklassen (Klasse selbst zuerst)
  (slot-location-table empty-ht) ; Hashtabelle Slotname -> wo der Slot sitzt
)

(defstruct (built-in-class (:inherit class) (:conc-name "CLASS-"))
)
(proclaim '(notinline built-in-class-p))

(defstruct (slotted-class (:inherit class) (:predicate nil) (:copier nil) (:conc-name "CLASS-"))
  slots                    ; Liste aller Slots (als Slot-Definitionen)
  default-initargs         ; Default-Initargs (als Aliste Initarg -> Initer)
  valid-initargs           ; Liste der gültigen Initargs
  instance-size            ; Anzahl der Slots der direkten Instanzen + 1
)

(defstruct (structure-class (:inherit slotted-class) (:conc-name "CLASS-"))
  names                    ; Codierung der include-Verschachtelung, eine Liste
)

(defstruct (standard-class (:inherit slotted-class) (:conc-name "CLASS-"))
  shared-slots             ; Simple-Vector mit den Werten aller Shared Slots, oder NIL
  direct-slots             ; Liste der neu hinzugekommenen Slots (als Plisten)
  direct-default-initargs  ; Neu hinzugekommene Default-Initargs (als Pliste)
)

; Zugriff auf Slots von Instanzen der Klasse <class> mittels der
; defstruct-Accessoren, daher hier keine Bootstrapping-Probleme.

; Weiter Bootstrapping
(%defclos
  ; Erkennungszeichen für CLASS-P
  (svref (get 'class 'sys::defstruct-description) 0)
  ; Built-In-Klassen für CLASS-OF
  (vector 'array 'bit-vector 'character 'complex 'cons 'float 'function
          'hash-table 'integer 'null 'package 'pathname
          #+LOGICAL-PATHNAMES 'logical-pathname
          'random-state 'ratio 'readtable 'standard-generic-function
          'stream 'file-stream 'synonym-stream 'broadcast-stream
          'concatenated-stream 'two-way-stream 'echo-stream 'string-stream
          'string 'symbol 't 'vector
) )

(defun print-class (class stream)
  (print-unreadable-object (class stream :type t)
    (write (class-classname class) :stream stream)
) )


;;; DEFCLASS

(defmacro defclass (name superclass-specs slot-specs &rest options)
  (unless (symbolp name)
    (error-of-type 'sys::source-program-error
      (ENGLISH "~S: class name ~S should be a symbol")
      'defclass name
  ) )
  (let* ((superclass-forms
           (progn
             (unless (listp superclass-specs)
               (error-of-type 'sys::source-program-error
                 (ENGLISH "~S ~S: expecting list of superclasses instead of ~S")
                 'defclass name superclass-specs
             ) )
             (mapcar #'(lambda (superclass)
                         (unless (symbolp superclass)
                           (error-of-type 'sys::source-program-error
                             (ENGLISH "~S ~S: superclass name ~S should be a symbol")
                             'defclass name superclass
                         ) )
                         `(FIND-CLASS ',superclass)
                       )
                     superclass-specs
         ) ) )
         (accessor-def-forms '())
         (slot-forms
           (let ((slot-names '()))
             (unless (listp slot-specs)
               (error-of-type 'sys::source-program-error
                 (ENGLISH "~S ~S: expecting list of slot specifications instead of ~S")
                 'defclass name slot-specs
             ) )
             (mapcar #'(lambda (slot-spec)
                         (let ((slot-name slot-spec) (slot-options '()))
                           (when (consp slot-spec)
                             (setq slot-name (car slot-spec) slot-options (cdr slot-spec))
                           )
                           (unless (symbolp slot-name)
                             (error-of-type 'sys::source-program-error
                               (ENGLISH "~S ~S: slot name ~S should be a symbol")
                               'defclass name slot-name
                           ) )
                           (if (member slot-name slot-names :test #'eq)
                             (error-of-type 'sys::source-program-error
                               (ENGLISH "~S ~S: There may be only one direct slot with the name ~S.")
                               'defclass name slot-name
                             )
                             (push slot-name slot-names)
                           )
                           (let ((accessors '())
                                 (readers '())
                                 (writers '())
                                 (allocation '())
                                 (initargs '())
                                 (initform nil) (initer nil)
                                 (types '())
                                 (documentation nil))
                             (when (oddp (length slot-options))
                               (error-of-type 'sys::source-program-error
                                 (ENGLISH "~S ~S: slot options for slot ~S don't come in pairs")
                                 'defclass name slot-name
                             ) )
                             (do ((optionsr slot-options (cddr optionsr)))
                                 ((atom optionsr))
                               (let ((optionkey (first optionsr))
                                     (argument (second optionsr)))
                                 (case optionkey
                                   ((:READER :WRITER)
                                    (unless (function-name-p argument)
                                      (error-of-type 'sys::source-program-error
                                        (ENGLISH "~S ~S, slot option for slot ~S: ~S is not a function name")
                                        'defclass name slot-name argument
                                    ) )
                                    (case optionkey
                                      (:READER (push argument readers))
                                      (:WRITER (push argument writers))
                                   ))
                                   (:ACCESSOR
                                    (unless (symbolp argument)
                                      (error-of-type 'sys::source-program-error
                                        (ENGLISH "~S ~S, slot option for slot ~S: ~S is not a symbol")
                                        'defclass name slot-name argument
                                    ) )
                                    (push argument accessors)
                                    (push argument readers)
                                    (push `(SETF ,argument) writers)
                                   )
                                   (:ALLOCATION
                                    (when allocation
                                      (error-of-type 'sys::source-program-error
                                        (ENGLISH "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':allocation slot-name
                                    ) )
                                    (case argument
                                      ((:INSTANCE :CLASS) (setq allocation argument))
                                      (t (error-of-type 'sys::source-program-error
                                           (ENGLISH "~S ~S, slot option for slot ~S must have the value ~S or ~S, not ~S")
                                           'defclass name slot-name ':instance ':class argument
                                   )) )  )
                                   (:INITARG
                                    (unless (symbolp argument)
                                      (error-of-type 'sys::source-program-error
                                        (ENGLISH "~S ~S, slot option for slot ~S: ~S is not a symbol")
                                        'defclass name slot-name argument
                                    ) )
                                    (push argument initargs)
                                   )
                                   (:INITFORM
                                    (when initform
                                      (error-of-type 'sys::source-program-error
                                        (ENGLISH "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':initform slot-name
                                    ) )
                                    (setq initform `(QUOTE ,argument)
                                          initer (make-initer argument)
                                   ))
                                   (:TYPE
                                    (when types
                                      (error-of-type 'sys::source-program-error
                                        (ENGLISH "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':type slot-name
                                    ) )
                                    (setq types (list argument))
                                   )
                                   (:DOCUMENTATION
                                    (when documentation
                                      (error-of-type 'sys::source-program-error
                                        (ENGLISH "~S ~S, slot option ~S for slot ~S may only be given once")
                                        'defclass name ':documentation slot-name
                                    ) )
                                    (unless (stringp argument)
                                      (error-of-type 'sys::source-program-error
                                        (ENGLISH "~S ~S, slot option for slot ~S: ~S is not a string")
                                        'defclass name slot-name argument
                                    ) )
                                    (setq documentation argument)
                                   )
                                   (t
                                     (error-of-type 'sys::source-program-error
                                       (ENGLISH "~S ~S, slot option for slot ~S: ~S is not a valid slot option")
                                       'defclass name slot-name optionkey
                                   ) )
                             ) ) )
                             (setq readers (nreverse readers))
                             (setq writers (nreverse writers))
                             (dolist (funname readers)
                               (push `(DEFMETHOD ,funname ((OBJECT ,name))
                                        (SLOT-VALUE OBJECT ',slot-name)
                                      )
                                     accessor-def-forms
                             ) )
                             (dolist (funname writers)
                               (push `(DEFMETHOD ,funname (NEW-VALUE (OBJECT ,name))
                                        (SETF (SLOT-VALUE OBJECT ',slot-name) NEW-VALUE)
                                      )
                                     accessor-def-forms
                             ) )
                             `(LIST
                                :NAME ',slot-name
                                ,@(when accessors `(:ACCESSORS ',(nreverse accessors)))
                                ,@(when readers `(:READERS ',readers))
                                ,@(when writers `(:WRITERS ',writers))
                                ,@(when (eq allocation ':class) `(:ALLOCATION :CLASS))
                                ,@(when initargs `(:INITARGS ',(nreverse initargs)))
                                ,@(when initform `(#| :INITFORM ,initform |# :INITER ,initer))
                                ,@(when types `(:TYPE ',(first types)))
                                ,@(when documentation `(:DOCUMENTATION ',documentation))
                              )
                       ) ) )
                     slot-specs
        )) ) )
    `(LET ()
       (EVAL-WHEN (COMPILE LOAD EVAL)
         (ENSURE-CLASS
           ',name
           :DIRECT-SUPERCLASSES (LIST ,@superclass-forms)
           :DIRECT-SLOTS (LIST ,@slot-forms)
           ,@(let ((metaclass nil)
                   (direct-default-initargs nil)
                   (documentation nil))
               (dolist (option options)
                 (block nil
                   (when (listp option)
                     (let ((optionkey (first option)))
                       (when (case optionkey
                               (:METACLASS metaclass)
                               (:DEFAULT-INITARGS direct-default-initargs)
                               (:DOCUMENTATION documentation)
                             )
                         (error-of-type 'sys::source-program-error
                           (ENGLISH "~S ~S, option ~S may only be given once")
                           'defclass name optionkey
                       ) )
                       (case optionkey
                         (:METACLASS
                          (when (eql (length option) 2)
                            (let ((argument (second option)))
                              (unless (symbolp argument)
                                (error-of-type 'sys::source-program-error
                                  (ENGLISH "~S ~S, option ~S: ~S is not a symbol")
                                  'defclass name option argument
                              ) )
                              (setq metaclass `(:METACLASS (FIND-CLASS ',argument)))
                            )
                            (return)
                         ))
                         (:DEFAULT-INITARGS
                          (let ((list (rest option)))
                            (when (and (consp list) (null (cdr list)) (listp (car list)))
                              (setq list (car list))
                              (warn (ENGLISH "~S ~S: option ~S should be written ~S")
                                    'defclass name option (cons ':DEFAULT-INITARGS list)
                            ) )
                            (when (oddp (length list))
                              (error-of-type 'sys::source-program-error
                                (ENGLISH "~S ~S, option ~S: arguments don't come in pairs")
                                'defclass name option
                            ) )
                            (setq direct-default-initargs
                                  `(:DIRECT-DEFAULT-INITARGS
                                    (LIST
                                     ,@(let ((arglist nil) (formlist nil))
                                         (do ((list list (cddr list)))
                                             ((atom list))
                                           (unless (symbolp (first list))
                                             (error-of-type 'sys::source-program-error
                                               (ENGLISH "~S ~S, option ~S: ~S is not a symbol")
                                               'defclass name option (first list)
                                           ) )
                                           (when (member (first list) arglist)
                                             (error-of-type 'sys::source-program-error
                                               (ENGLISH "~S ~S, option ~S: ~S may only be given once")
                                               'defclass name option (first list)
                                           ) )
                                           (push (first list) arglist)
                                           (push (second list) formlist)
                                         )
                                         (mapcan #'(lambda (arg form)
                                                     `(',arg ,(make-initer form))
                                                   )
                                                 (nreverse arglist) (nreverse formlist)
                                       ) )
                                   ))
                          ) )
                          (return)
                         )
                         (:DOCUMENTATION
                          (when (eql (length option) 2)
                            (let ((argument (second option)))
                              (unless (stringp argument)
                                (error-of-type 'sys::source-program-error
                                  (ENGLISH "~S ~S, option ~S: ~S is not a string")
                                  'defclass name option argument
                              ) )
                              (setq documentation `(:DOCUMENTATION ',argument))
                            )
                            (return)
                         ))
                   ) ) )
                   (error-of-type 'sys::source-program-error
                     (ENGLISH "~S ~S: invalid option ~S")
                     'defclass name option
               ) ) )
               `(,@metaclass ,@direct-default-initargs ,@documentation)
             )
       ) )
       ,@(nreverse accessor-def-forms) ; die DEFMETHODs
       (FIND-CLASS ',name)
     )
) )
; Ein Initer zur Laufzeit ist - um Funktionsaufrufe zu sparen -
; i.a. ein Cons (init-function . nil), bei Konstanten aber (nil . init-value).
(defun make-initer (form)
  (if (constantp form)
    `(CONS 'NIL ,form)
    `(CONS (FUNCTION (LAMBDA () ,form)) 'NIL)
) )

; DEFCLASS-Ausführung:

; Zur Laufzeit noch bedeutsame Information eines Slots:
(defstruct (slot-definition
            (:conc-name "SLOTDEF-")
            (:type vector) (:predicate nil) (:copier nil) (:constructor nil))
  (name nil :type symbol)
  (initargs '() :type list)
  (location nil :type (or null integer cons))
  (initer nil :type (or null cons))
)
(defstruct (standard-slot-definition (:inherit slot-definition)
            (:conc-name "SLOTDEF-")
            (:type vector) (:predicate nil)
            (:constructor make-standard-slot-definition (name allocation initargs location initer)))
  (allocation :instance :type (or (member :class :instance) class))
)

(defun make-slotdef (&key name (allocation ':instance) (initargs '()) location (initer nil) (initform nil) (accessors '()) (readers '()) (writers '()) type documentation)
  (declare (ignore initform accessors readers writers type documentation))
  (make-standard-slot-definition name allocation initargs location initer)
)

#| ; In defstruc.lsp ist im wesentlichen das Folgende enthalten.
; In record.d und hier wird benutzt, dass die ersten 4 Attribute übereinstimmen!
(defstruct (structure-slot-definition (:include slot-definition)
            (:conc-name "DS-SLOT-")
            (:type vector) (:predicate nil)
            (:constructor make-ds-slot (name offset location initer default type readonly)))
  ;(name nil :type symbol)              ; ds-slot-name = slotdef-name !!
  ;(initargs '() :type list)            ; ds-slot-initargs = slotdef-initargs !!
  ;(offset nil :type (or null integer)) ; ds-slot-offset = slotdef-location !!
  ;(initer nil :type (or null cons))    ; ds-slot-initer = slotdef-initer !!
  (default nil)                         ; ds-slot-default
  (type nil)                            ; ds-slot-type
  (readonly nil)                        ; ds-slot-readonly
)
|#

(defun ensure-class (name &rest all-keys
                          &key (metaclass <standard-class>)
                               (direct-superclasses '())
                               (direct-slots '())
                               (direct-default-initargs '())
                               (documentation nil)
                          &allow-other-keys
                    )
  (let ((class (find-class name nil)))
    (when class
      ; Die einzige Modifikationen, die wir bei Klassen zulassen, sind die,
      ; die bei doppeltem Laden desselben Codes auftreten können:
      ; veränderte Slot-Optionen :initform, :documentation,
      ; veränderte Klassen-Optionen :default-initargs, :documentation.
      (if (and (eq metaclass <standard-class>)
               (eq metaclass (class-of class))
               (equal direct-superclasses (class-direct-superclasses class))
               (equal-slots direct-slots (class-direct-slots class))
               (equal-default-initargs direct-default-initargs (class-direct-default-initargs class))
          )
        (progn
          ; neue Slot-Inits eintragen:
          (do ((l-old (class-direct-slots class) (cdr l-old))
               (l-new direct-slots (cdr l-new)))
              ((null l-new))
            (let ((old (getf (car l-old) ':initer))
                  (new (getf (car l-new) ':initer)))
              (when old
                ; Slot-Initer new destruktiv in den Slot-Initer old umfüllen:
                (setf (car old) (car new))
                (setf (cdr old) (cdr new))
          ) ) )
          ; neue Default-Initargs eintragen:
          (do ((l-old (class-direct-default-initargs class) (cddr l-old))
               (l-new direct-default-initargs (cddr l-new)))
              ((null l-new))
            (let ((old (second l-old))
                  (new (second l-new)))
              ; Initer new destruktiv in den Initer old umfüllen:
              (setf (car old) (car new))
              (setf (cdr old) (cdr new))
          ) )
          ; NB: Diese Modifikationen vererben sich auch automatisch auf die
          ; Unterklassen von class!
          ; neue Dokumentation eintragen:
          (when documentation (setf (documentation name 'TYPE) documentation))
          ; modifizierte Klasse als Wert:
          (return-from ensure-class class)
        )
        (progn
          (warn (ENGLISH "~S: Class ~S is being redefined, instances are obsolete")
                'defclass name
          )
          ; Durch alle Symbole durchlaufen, um die Unterklassen abzugrasen.
          ; Sollte eleganter gehen??
          (let ((subclass-names (list name)))
            (do-all-symbols (sym)
              (let ((c (get sym 'CLOSCLASS)))
                (when (and c (subclassp c class))
                  (pushnew sym subclass-names)
            ) ) )
            (dolist (sym subclass-names)
              (let ((c (get sym 'CLOSCLASS)))
                (setf (class-name c) (gensym "OBSOLETE-CLASS-"))
                (remprop sym 'CLOSCLASS)
                (setf (documentation sym 'TYPE) '())
          ) ) )
    ) ) )
    (when documentation (setf (documentation name 'TYPE) documentation))
    (setf (find-class name)
          (apply (cond ((eq metaclass <standard-class>) #'make-instance-standard-class)
                       ((eq metaclass <built-in-class>) #'make-instance-built-in-class)
                       ((eq metaclass <structure-class>) #'make-instance-structure-class)
                       (t #'make-instance)
                 )
                 metaclass
                 :name name
                 all-keys
) ) )     )
(defun equal-slots (slots1 slots2)
  (or (and (null slots1) (null slots2))
      (and (consp slots1) (consp slots2)
           (equal-slot (first slots1) (first slots2))
           (equal-slots (rest slots1) (rest slots2))
) )   )
(defun equal-slot (slot1 slot2) ; slot1, slot2 Plisten
  (or (and (null slot1) (null slot2))
      (and #| (consp slot1) (consp slot2) |#
           (eq (first slot1) (first slot2))
           (or (memq (first slot1) '(#| :initform |# :initer #| :documentation |# ))
               (equal (second slot1) (second slot2))
           )
           (equal-slot (cddr slot1) (cddr slot2))
) )   )
(defun equal-default-initargs (initargs1 initargs2)
  (or (and (null initargs1) (null initargs2))
      (and (consp initargs1) (consp initargs2)
           (eq (first initargs1) (first initargs2))
           (equal-default-initargs (cddr initargs1) (cddr initargs2))
) )   )

(defun add-default-superclass (direct-superclasses default-superclass)
  ; Manchmal will man eine bestimmte Oberklasse erzwingen.
  ; Sie darf aber nicht zweimal angegeben werden.
  (if (member default-superclass direct-superclasses :test #'eq)
    direct-superclasses
    (append direct-superclasses (list default-superclass))
) )

; When this is true, all safety checks about the metaclasses of superclasses
; are omitted.
(defparameter *allow-mixing-metaclasses* nil)

; Erzeugung einer Instanz von <standard-class>:

(let (unbound) (declare (compile)) ; unbound = #<unbound>
(defun def-unbound (x) (declare (compile)) (setq unbound x))
(defun make-instance-standard-class
       (metaclass &rest args
                  &key name (direct-superclasses '()) (direct-slots '())
                            (direct-default-initargs '())
                  &allow-other-keys
       )
  ; metaclass = <standard-class>
  (declare (ignore direct-superclasses direct-slots direct-default-initargs))
  (let ((class (make-standard-class :classname name :metaclass metaclass)))
    (apply #'initialize-instance-standard-class class args)
) )
(defun initialize-instance-standard-class
           (class &key name (direct-superclasses '()) (direct-slots '())
                            (direct-default-initargs '())
                  &allow-other-keys
           )
  ; metaclass <= <standard-class>
  (unless *allow-mixing-metaclasses*
    (unless (every #'standard-class-p direct-superclasses)
      (error-of-type 'error
        (ENGLISH "~S ~S: superclass ~S should belong to class STANDARD-CLASS")
        'defclass name (find-if-not #'standard-class-p direct-superclasses)
  ) ) )
  (setf (class-direct-superclasses class) (copy-list direct-superclasses))
  (setf (class-precedence-list class)
        (std-compute-cpl class
          (add-default-superclass direct-superclasses <standard-object>)
  )     )
  (setf (class-all-superclasses class)
        (std-compute-superclasses (class-precedence-list class))
  )
  (setf (class-direct-slots class) direct-slots)
  (setf (class-slots class) (std-compute-slots class))
  (setf (class-slot-location-table class) (make-hash-table :test #'eq))
  (setf (class-instance-size class) 1) ; Index 0 wird von der Klasse belegt
  (let ((shared-index (std-layout-slots class (class-slots class))))
    (when (plusp shared-index)
      (setf (class-shared-slots class)
            (let ((v (make-array shared-index))
                  (i 0))
              (mapc #'(lambda (slot)
                        (when (eq (slotdef-allocation slot) class)
                          (setf (svref v i)
                            (let ((init (slotdef-initer slot)))
                              (if init
                                (if (car init) (funcall (car init)) (cdr init))
                                unbound
                          ) ) )
                          (incf i)
                      ) )
                    (class-slots class)
              )
              v
  ) ) )     )
  (setf (class-direct-default-initargs class) direct-default-initargs)
  (setf (class-default-initargs class) ; 28.1.3.3.
        (remove-duplicates
          (mapcan
            #'(lambda (c)
                (when (standard-class-p c)
                  (plist-to-alist (class-direct-default-initargs c))
              ) )
            (class-precedence-list class)
          )
          :key #'car
          :from-end t
  )     )
  (setf (class-valid-initargs class)
        (remove-duplicates (mapcap #'slotdef-initargs (class-slots class)))
  )
  (system::note-new-standard-class)
  class
)
) ; let

;; 28.1.5. Determining the Class Precedence List
;
; Die Menge aller Klassen bildet einen gerichteten Graphen: Klasse C sitzt
; unterhalb der direkten Oberklassen von C. Dieser Graph ist azyklisch, weil
; zum Zeitpunkt Definition der Klasse C alle direkten Oberklassen bereits
; vorhanden sein müssen.
;
; Man kann daher noethersche Induktion (Induktion von oben nach unten im
; Klassengraphen) verwenden.
;
; Zu einer Klasse C sei DS(n) die Liste aller direkten Oberklassen von C.
; Die Menge aller Oberklassen (inkl. C selbst) ist induktiv definiert als
; S(C) := {C} union union_{D in DS(C)} S(D).
;
; Anders ausgedrückt:
; S(C) = { C_n : C_n in DS(C_{n-1}), ..., C_1 in DS(C_0), C_0 = C }
;
; Lemma 1: (a) C in S(C).
;          (b) DS(C) subset S(C).
;          (c) D in DS(C) ==> S(D) subset S(C).
;          (d) D in S(C) ==> S(D) subset S(C).
; Beweis: (a) Aus der Definition.
;         (b) Aus (a) und der Definition.
;         (c) Aus der Definition.
;         (d) Aus (c) bei festem D mit Induktion über C.
;
; Die CPL einer Klasse C ist eine Anordnung der Menge S(C).
; Falls CPL(C) = (... D1 ... D2 ...), schreibt man D1 < D2. Die so eingeführte
; Relation ist eine Totalordnung auf S(C).
; Dabei ist die folgende Menge von Restriktionen zu berücksichtigen:
; R(C) := union_{D in S(C)} DR(D)  mit
; DR(C) := { C < C1, C1 < C2, ..., C{n-1} < C_n } falls DS(C) = (C1, ..., Cn).
; Falls R(C) einen Zyklus enthält, kann natürlich R(C) nicht zu einer
; Totalordnung vervollständigt werden. Dann heißt R(C) inkonsistent.
; CPL(C) wird folgendermaßen konstruiert:
;   L := (), R := R(C).
;   L := (L | C), entferne alle (C < ..) aus R.
;   Solange R /= {}, betrachte die Menge M aller minimalen Elemente von R
;     (das sind diejenigen Klassen, die man, ohne R(C) zu verletzen, zu L
;     hinzufügen könnte). Ist M leer, so hat man einen Zyklus in R(C) und
;     bricht den Algorithmus ab. Sonst wähle unter den Elementen E von M
;     dasjenige aus, das ein möglichst weit rechts in L gelegenes D mit
;     E in DS(D) besitzt.
;     L := (L | E), entferne alle (E < ..) aus R.
;   CPL(C) := L.
; L wird schrittweise um ein Element verlängert, R wird schrittweise
; verkleinert, und R besteht immer nur aus Relationen zwischen Elementen
; von S(C)\L.
;
; Lemma 2: (a) CPL(C) = (C ...).
;          (b) Ist DS(C) = (C1, ..., Cn), so ist
;              CPL(C) = (C ... C1 ... C2 ... ... Cn ...).
; Beweis: (a) Klar nach Konstruktion.
;         (b) Wenn Ci in die CPL aufgenommen wird, kann die Restriktion
;             C{i-1} < Ci nicht mehr in R sein, also muss C{i-1} schon in
;             der CPL sein.
;
; Folgende Aussage ist falsch:
; (*) Ist D in DS(C) und CPL(D) = (D1, ..., Dn), so ist
;     CPL(C) = (C ... D1 ... D2 ... ... Dn ...).
; Beispiel:
;     z
;    /|\             CPL(z) = (z)
;   / | \            CPL(x) = (x z)
;  x  |  x           CPL(y) = (y z)
;  |  |  |           CPL(d) = (d x z)
;  d  y  e           CPL(e) = (e x z)
;   \/ \/            CPL(b) = (b d x y z)
;   b   c            CPL(c) = (c y e x z)
;    \ /             CPL(a) = (a b d c y e x z)
;     a
;                    CPL(a) enthält CPL(b) nicht!
;
#|
(defclass z () ())
(defclass x (z) ())
(defclass y (z) ())
(defclass d (x z) ())
(defclass e (x z) ())
(defclass b (d y) ())
(defclass c (y e) ())
(defclass a (b c) ())
(mapcar #'find-class '(z x y d e b c a))
|#

(defun std-compute-cpl (class direct-superclasses)
  (let* ((superclasses ; Liste aller Oberklassen in irgendeiner Reihenfolge
           (remove-duplicates
             (mapcap #'class-precedence-list direct-superclasses)
         ) )
         (L '())
         (R1 (list (cons class direct-superclasses)))
         (R2 (mapcar #'(lambda (D) (cons D (class-direct-superclasses D)))
                     superclasses
        ))   )
    (loop
      ; L ist die umgedrehte bisher konstruierte CPL.
      ; R1 ist die Liste der bisher relevanten Restriktionen, in der Form
      ; R1 = (... (Dj ... Dn) ...) wenn aus DR(D) = (D1 ... Dn) nur noch
      ; Dj,...,Dn übrig sind. Die Reihenfolge in R1 entspricht der in L.
      ; R2 ist die Liste der bisher irrelevanten Restriktionen.
      (when (null R1)
        (return) ; R1 = R2 = () -> fertig
      )
      (let ((M (remove-duplicates (mapcar #'first R1) :from-end t)))
        (setq M
          (remove-if
            #'(lambda (E)
                (or (dolist (r R1 nil) (when (member E (cdr r)) (return t)))
                    (dolist (r R2 nil) (when (member E (cdr r)) (return t)))
              ) )
            M
        ) )
        (when (null M)
          (error-of-type 'error
            (ENGLISH "~S ~S: inconsistent precedence graph, cycle ~S")
            'defclass (class-classname class)
            ; Zyklus finden: mit Hilfe der Restriktionen zu immer
            ; kleineren Elementen voranschreiten.
            (let* ((R0 (append R1 R2))
                   (cycle (list (car (first R0)))))
              (loop
                (let* ((last (car cycle))
                       (next (dolist (r R0 nil)
                               (when (member last (cdr r))
                                 (return (nth (position last (cdr r)) r))
                      ))     ) )
                  (when (null next)
                    ; Offenbar ist last nun doch ein minimales Element!
                    (return '??)
                  )
                  (when (member next cycle)
                    (setf (cdr (member next cycle)) nil)
                    (return cycle)
                  )
                  (push next cycle)
            ) ) )
        ) )
        (let ((E (first M)))
          (push E L)
          (push (assoc E R2) R1)
          (setq R2 (delete E R2 :key #'first))
          (mapl #'(lambda (r) (when (eq (first (car r)) E) (pop (car r)))) R1)
          (setq R1 (delete-if #'null R1))
    ) ) )
    (setq L (nreverse L))
    ; Teste, ob L mit den CPL(D), D in direct-superclasses, verträglich ist:
    (mapc #'(lambda (D)
              (unless ; Ist (class-precedence-list D) Teil-Liste von L ?
                (do ((CL L)
                     (DL (class-precedence-list D) (cdr DL)))
                    ((null DL) t)
                  (when (null (setq CL (member (car DL) CL))) (return nil))
                )
                (warn (ENGLISH "(class-precedence-list ~S) and (class-precedence-list ~S) are inconsistent")
                      class D
            ) ) )
          direct-superclasses
    )
    L
) )

; Stopft alle Oberklassen (aus der precedence-list) in eine Hash-Tabelle.
(defun std-compute-superclasses (precedence-list)
  (let ((ht (make-hash-table :test #'eq)))
    (mapc #'(lambda (superclass) (setf (gethash superclass ht) t))
          precedence-list
    )
    ht
) )

; Hilfsfunktion (p1 v1 ... pn vn) -> ((p1 . v1) ... (pn . vn))
(defun plist-to-alist (pl &aux (al '()))
  (loop
    (when (null pl) (return))
    (setq al (acons (first pl) (second pl) al))
    (setq pl (cddr pl))
  )
  (nreverse al)
)

; Hilfsfunktion ((p1 . v1) ... (pn . vn)) -> (p1 v1 ... pn vn)
(defun alist-to-plist (al)
  (mapcan #'(lambda (pv) (list (car pv) (cdr pv))) al)
)

;; 28.1.3.2. Inheritance of Slots and Slot Options

(defun std-compute-slots (class &optional (more-direct-slots '()))
  ; Alle Slot-Specifier sammeln, geordnet nach Präzedenz:
  (let ((all-slots
          (mapcan
            #'(lambda (c)
                (mapcar #'(lambda (slot)
                            (setq slot (plist-to-alist slot))
                            (when (eq (cdr (assoc ':allocation slot)) ':class)
                              (setf (cdr (assoc ':allocation slot)) c)
                            )
                            slot
                          )
                  (append
                    (if (standard-class-p c) (class-direct-slots c))
                    (if (eq c class) more-direct-slots)
              ) ) )
            (class-precedence-list class)
       )) )
    ; Aufspalten nach Slot-Namen:
    (setq all-slots
      (let ((ht (make-hash-table :test #'eq)))
        (dolist (slot all-slots)
          (assert (eq (caar slot) ':name))
          (push (cdr slot) (gethash (cdar slot) ht nil))
        )
        (let ((L nil))
          (maphash #'(lambda (name slots) (push (cons name (nreverse slots)) L)) ht)
          L ; nicht (nreverse L), da maphash die Reihenfolge umdreht
    ) ) )
    ; all-slots ist nun eine Liste von Listen der Form
    ; (name most-specific-slotspec ... least-specific-slotspec).
    (mapcar #'(lambda (slot)
                (let ((name (car slot))
                      (slotspecs (cdr slot)))
                  (apply #'make-slotdef
                    :name name
                    (alist-to-plist
                      `(,(or (assoc ':allocation (first slotspecs))
                             `(:allocation . :instance)
                         )
                        #|
                        ,@(let ((accessors
                                  (mapcap #'(lambda (slotspec) (cdr (assoc ':accessors slotspec)))
                                          slotspecs
                               )) )
                            (if accessors `((:accessors . ,accessors)))
                          )
                        |#
                        ,@(let ((initargs
                                  (remove-duplicates
                                    (mapcap #'(lambda (slotspec) (cdr (assoc ':initargs slotspec)))
                                            slotspecs
                                    )
                                    :from-end t
                               )) )
                            (if initargs `((:initargs . ,initargs)))
                          )
                        ,@(dolist (slotspec slotspecs '())
                            (when (assoc ':initer slotspec)
                              (return `(#| ,(assoc ':initform slotspec) |# ,(assoc ':initer slotspec)))
                          ) )
                        #|
                        ,(let ((types '()))
                           (dolist (slotspec slotspecs)
                             (when (assoc ':type slotspec)
                               (push (cdr (assoc ':type slotspec)) types)
                           ) )
                           `(:type . ,(if types `(AND ,@(nreverse types)) 'T))
                         )
                        |#
                        #|
                        ,@(dolist (slotspec slotspecs '())
                            (when (assoc ':documentation slotspec)
                              (return `(,(assoc ':documentation slotspec)))
                          ) )
                        |#
                       )
              ) ) ) )
            all-slots
    )
) )

;; Allocation of local and shared slots

; Add the local and shared slots to the slot-location-table ht,
; incrementing the instance-size, and return the new shared-size.
(defun std-layout-slots (class slots)
  (let ((ht (class-slot-location-table class))
        (local-index (class-instance-size class))
        (shared-index 0))
    (mapc #'(lambda (slot)
              (let* ((name (slotdef-name slot))
                     (allocation (slotdef-allocation slot))
                     (location
                       (cond ((eq allocation ':instance) ; local slot
                              (prog1 local-index (incf local-index))
                             )
                             ((eq allocation class) ; new shared slot
                              (prog1 (cons class shared-index) (incf shared-index))
                             )
                             (t ; inherited shared slot
                              (gethash name (class-slot-location-table allocation))
                    )) )     )
                (setf (slotdef-location slot) location)
                (setf (gethash name ht) location)
            ) )
          slots
    )
    (setf (class-instance-size class) local-index)
    shared-index
) )


; Erzeugung einer Instanz von <built-in-class>:

(defun make-instance-built-in-class
       (metaclass &key name (direct-superclasses '())
                  &allow-other-keys
       )
  ; metaclass = <built-in-class>
  (unless *allow-mixing-metaclasses*
    (unless (every #'built-in-class-p direct-superclasses)
      (error-of-type 'error
        (ENGLISH "~S: superclass ~S should belong to class BUILT-IN-CLASS")
        name (find-if-not #'built-in-class-p direct-superclasses)
  ) ) )
  (let ((class (make-built-in-class :classname name :metaclass metaclass)))
    (setf (class-direct-superclasses class) (copy-list direct-superclasses))
    (setf (class-precedence-list class)
          (std-compute-cpl class direct-superclasses)
    )
    (setf (class-all-superclasses class)
          (std-compute-superclasses (class-precedence-list class))
    )
    class
) )


; Erzeugung einer Instanz von <structure-class>:

(defun make-instance-structure-class
       (metaclass &rest args
                  &key name (direct-superclasses '())
                       ; The following keys come from ENSURE-CLASS.
                       (direct-slots '()) (direct-default-initargs '())
                       ; The following keys come from DEFINE-STRUCTURE-CLASS.
                       names (slots '()) (size 1)
                  &allow-other-keys
       )
  ; metaclass = <structure-class>
  (declare (ignore direct-superclasses direct-slots direct-default-initargs names slots size))
  (let ((class (make-structure-class :classname name :metaclass metaclass)))
    (apply #'initialize-instance-structure-class class args)
) )
(defun initialize-instance-structure-class
           (class &key name (direct-superclasses '())
                       ; The following keys come from ENSURE-CLASS.
                       (direct-slots '()) (direct-default-initargs '())
                       ; The following keys come from DEFINE-STRUCTURE-CLASS.
                       names (slots '()) (size 1)
                  &allow-other-keys
       )
  ; metaclass <= <structure-class>
  (unless (null (cdr direct-superclasses))
    (error-of-type 'error
      (ENGLISH "~S: metaclass STRUCTURE-CLASS forbids more than one direct superclass")
      name
  ) )
  (unless *allow-mixing-metaclasses*
    (unless (every #'structure-class-p direct-superclasses)
      (error-of-type 'error
        (ENGLISH "~S: superclass ~S should belong to class STRUCTURE-CLASS")
        name (first direct-superclasses)
  ) ) )
  (setf (class-direct-superclasses class) (copy-list direct-superclasses))
  (setf (class-precedence-list class)
        (std-compute-cpl class
          (add-default-superclass
            (add-default-superclass direct-superclasses
                                    <structure-object>)
                                    <t>)
  )     )
  (setf (class-all-superclasses class)
        (std-compute-superclasses (class-precedence-list class))
  )
  ; When called via ENSURE-CLASS, we have to do inheritance of slots.
  (unless names
    (setq names
          (cons name
                (if direct-superclasses (class-names (first direct-superclasses)) '())
    )     )
    (when direct-superclasses
      (setq slots (class-slots (first direct-superclasses)))
      (setq size (class-instance-size (first direct-superclasses)))
  ) )
  (setf (class-slot-location-table class)
        (make-hash-table :test #'eq
          :initial-contents
            (mapcar #'(lambda (slot)
                        (cons (slotdef-name slot) (slotdef-location slot))
                      )
                    slots
  )     )   )
  (setf (class-instance-size class) size)
  (setf (class-slots class) slots)
  ; When called via ENSURE-CLASS, we may have to treat additional direct slots.
  (when direct-slots
    (let* ((more-slots (std-compute-slots class direct-slots))
           (shared-index (std-layout-slots class more-slots)))
      (when (plusp shared-index)
        (error-of-type 'error
          (ENGLISH "~S: metaclass STRUCTURE-CLASS does not support shared slots")
          name
      ) )
      (setf (class-slots class) (append (class-slots class) more-slots))
  ) )
  (setf (class-default-initargs class)
        (remove-duplicates
          (append (plist-to-alist direct-default-initargs)
                  (mapcap
                    #'(lambda (c)
                        (when (structure-class-p c)
                          (class-default-initargs c)
                      ) )
                    (cdr (class-precedence-list class))
          )       )
          :key #'car
          :from-end t
  )     )
  (setf (class-valid-initargs class)
        (remove-duplicates (mapcap #'slotdef-initargs (class-slots class)))
  )
  (setf (class-names class) names)
  (system::note-new-structure-class)
  class
)

; DEFSTRUCT-Hook
(defun define-structure-class (name)
  (let ((descr (get name 'sys::defstruct-description)))
    (when descr
      (let* ((names (svref descr 0))
             (all-slots (svref descr 3))
             (slots (remove-if-not #'sys::ds-slot-name all-slots)))
        (setf (find-class name)
              (make-instance-structure-class <structure-class>
                :name name
                :direct-superclasses
                  (if (cdr names) (list (find-class (second names))) '())
                :names names
                :slots slots
                :size (if all-slots (1+ (sys::ds-slot-offset (car (last all-slots)))) 1)
) ) ) ) )     )

;; Bootstrapping
(progn
  ; 1. Klasse <t>
  (setq <t>
        (make-instance-built-in-class nil :name 't :direct-superclasses '())
  )
  ; 2. Klassen <structure-class> und <structure-object>
  (setq <structure-class> (make-structure-class)) ; Dummy, damit (setf find-class) geht
  (setq <structure-object> <t>)
  (setq <structure-object>
        (make-instance-structure-class <structure-class>
          :name 'structure-object
          :direct-superclasses '()
          :names (svref (get 'structure-object 'sys::defstruct-description) 0)
  )     )
  (setf (find-class 'structure-object) <structure-object>)
  (setq <class> (define-structure-class 'class))
  (let ((<slotted-class> (define-structure-class 'slotted-class)))
    (setq <structure-class> (define-structure-class 'structure-class))
    (setf (class-metaclass <structure-object>) <structure-class>)
    (setf (class-metaclass <class>) <structure-class>)
    (setf (class-metaclass <slotted-class>) <structure-class>)
    (setf (class-metaclass <structure-class>) <structure-class>)
  )
  ; 3. Alle structure-Klassen
  (labels ((define-structure-class-with-includes (name)
             (when (get name 'sys::defstruct-description)
               (unless (find-class name nil)
                 (let ((names (svref (get name 'sys::defstruct-description) 0)))
                   (when (cdr names)
                     (define-structure-class-with-includes (second names))
                 ) )
                 (define-structure-class name)
          )) ) )
    (do-all-symbols (s) (define-structure-class-with-includes s))
  )
  ; 4. Klassen <standard-class>, <built-in-class>
  (setq <standard-class> (find-class 'standard-class))
  (setq <built-in-class> (find-class 'built-in-class))
  ; 5. Klasse <t> zu Ende
  (setf (class-metaclass <t>) <built-in-class>)
  (setf (find-class 't) <t>)
  ; 6. Klasse <standard-object>
  (setq <standard-object>
        (make-standard-class
          :classname 'standard-object
          :metaclass <standard-class>
          :direct-superclasses `(,<t>)
          :direct-slots '()
          :slots '()
          :slot-location-table empty-ht
          :instance-size 1
          :direct-default-initargs nil
          :default-initargs nil
  )     )
  (setf (class-all-superclasses <standard-object>)
        (std-compute-superclasses
          (setf (class-precedence-list <standard-object>)
                `(,<standard-object> ,<t>)
  )     ) )
  (setf (find-class 'standard-object) <standard-object>)
  (system::note-new-standard-class)
  ; 7. Wert #<unbound>
  (def-unbound
    (sys::%record-ref (allocate-std-instance <standard-object> 2) 1)
  )
)


;; 28.1.4. Integrating Types and Classes
(defun subclassp (class1 class2)
  (values
    (gethash class2 (class-all-superclasses class1)) ; T oder (Default) NIL
) )

;; Built-In-Klassen installieren
; Table 28-1, CLtL2 p. 783
(macrolet ((def (&rest classes &aux (new (car (last classes))))
             (let ((name (intern (string-trim "<>" (symbol-name new)))))
               `(setf (find-class ',name)
                  (setq ,new
                    (make-instance-built-in-class <built-in-class>
                      :name ',name
                      :direct-superclasses (list ,@(cdr (reverse classes)))
                ) ) )
          )) )
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

; Weiter Bootstrapping
(%defclos
  ; Erkennungszeichen für CLASS-P
  (svref (get 'class 'sys::defstruct-description) 0)
  ; Built-In-Klassen für CLASS-OF
  (vector <array> <bit-vector> <character> <complex> <cons> <float> <function>
          <hash-table> <integer> <null> <package> <pathname>
          #+LOGICAL-PATHNAMES <logical-pathname>
          <random-state> <ratio> <readtable> <standard-generic-function>
          <stream> <file-stream> <synonym-stream> <broadcast-stream>
          <concatenated-stream> <two-way-stream> <echo-stream> <string-stream>
          <string> <symbol> <t> <vector>
) )

;; Schnitt zweier Built-In-Klassen:
; Abweichungen von der Single-Inheritance sind nur
; (AND <sequence> <array>) = <vector> und (AND <list> <symbol>) = <null>.
(defun bc-p (class)
  (or (built-in-class-p class)
      (eq class <standard-object>)
      (eq class <structure-object>)
) )
(defun bc-and (class1 class2) ; liefert (AND class1 class2)
  (cond ((subclassp class1 class2) class1)
        ((subclassp class2 class1) class2)
        ((or (and (subclassp <sequence> class1) (subclassp <array> class2))
             (and (subclassp <sequence> class2) (subclassp <array> class1))
         )
         <vector>
        )
        ((or (and (subclassp <list> class1) (subclassp <symbol> class2))
             (and (subclassp <list> class2) (subclassp <symbol> class1))
         )
         <null>
        )
        (t nil)
) )
(defun bc-and-not (class1 class2) ; liefert eine Klasse c mit
                                  ; (AND class1 (NOT class2)) <= c <= class1
  (cond ((subclassp class1 class2) nil)
        ((and (eq class1 <sequence>) (subclassp <vector> class2)) <list>)
        ((and (eq class1 <sequence>) (subclassp <list> class2)) <vector>)
        ((and (eq class1 <list>) (subclassp <null> class2)) <cons>)
        (t class1)
) )


;;; Methoden

(defstruct (method (:predicate nil) (:copier nil) (:constructor nil)))

(defstruct (standard-method (:include method) (:conc-name "STD-METHOD-")) ; (:print-object print-std-method)
  function               ; die Funktion
  wants-next-method-p    ; Flag, ob als erstes Argument die NEXT-METHOD (als
                         ; Funktion mit allen Argumenten) bzw. NIL übergeben
                         ; werden soll (= NIL bei :BEFORE- und :AFTER-Methoden)
  parameter-specializers ; Liste ({class | (EQL object)}*)
  qualifiers             ; Liste von Symbolen, z.B. (:before)
  signature              ; Liste (reqanz optanz restp keyp keywords allowp)
  gf                     ; die generische Funktion, zu der diese Methode
                         ; gehört (nur für den Bedarf von CALL-NEXT-METHOD und
                         ; NO-NEXT-METHOD)
  initfunction           ; liefert, wenn aufgerufen, die Funktion
                         ; (nur für den Bedarf von ADD-METHOD)
)

; Bei CALL-NEXT-METHOD und NO-NEXT-METHOD muss die generische Funktion bekannt
; sein. Da allerdings im Prinzip Methoden nicht bestimmten generischen
; Funktionen zugehörig sind (wegen ADD-METHOD), müssen wir die Methode bei
; ADD-METHOD kopieren. Die Identität zweier Kopien derselben Methode stellen
; wir durch Blick auf std-method-initfunction fest. (Man könnte stattdessen
; auch die generische Funktion bei jedem Aufruf mitgeben, als erstes Argument
; an die effektive Methode, aber das ist sicher ineffizienter.)

(defun print-std-method (method stream)
  (print-unreadable-object (method stream :type t)
    (dolist (q (std-method-qualifiers method))
      (write q :stream stream)
      (write-char #\Space stream)
    )
    (write (std-method-parameter-specializers method) :stream stream)
) )

; Hilfsfunktion: Liefert eine Liste von n Gensyms.
(defun n-gensyms (n)
  (do ((l '() (cons (gensym) l))
       (i n (1- i)))
      ((eql i 0) l)
) )

; Hilfsfunktion: Testet auf Lambda-Listen-Marker.
(defun lambda-list-keyword-p (x)
  (memq x lambda-list-keywords)
)

;; Für DEFMETHOD, DEFGENERIC, GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS,
;; WITH-ADDED-METHODS
  ; caller: Symbol
  ; funname: Funktionsname, Symbol oder (SETF symbol)
  ; description: (qualifier* spec-lambda-list {declaration|docstring}* form*)
  ; ==> method-building-form
(defun analyze-method-description (caller funname description env)
  (let ((qualifiers nil))
    (loop
      (when (atom description)
        (error-of-type 'sys::source-program-error
          (ENGLISH "~S ~S: missing lambda list")
          caller funname
      ) )
      (when (listp (car description)) (return))
      (push (pop description) qualifiers)
    )
    ; Nur STANDARD Methodenkombination ist implementiert.
    (cond ((equal qualifiers '()))
          ((equal qualifiers '(:before)))
          ((equal qualifiers '(:after)))
          ((equal qualifiers '(:around)))
          (t (error-of-type 'sys::source-program-error
               (ENGLISH "STANDARD method combination doesn't allow the method qualifiers to be ~S")
               (nreverse qualifiers)
    )     )  )
    ; Lambdaliste bilden, Parameter-Specializer und Signatur extrahieren:
    (let ((specialized-lambda-list (car description))
          (body (cdr description)))
      (let ((req-vars '())
            (ignorable-req-vars '())
            (req-specializer-forms '()))
        (do ()
            ((or (atom specialized-lambda-list)
                 (lambda-list-keyword-p (car specialized-lambda-list))
            ))
          (let* ((item (pop specialized-lambda-list))
                 (specializer-name
                   (if (atom item)
                     (progn (push item req-vars) 't)
                     (progn
                       (push (first item) req-vars)
                       (push (first item) ignorable-req-vars) ; CLtL2 S. 840 oben
                       (second item)
                )) ) )
            (push (if (class-p specializer-name)
                    `',specializer-name
                    (if (and (consp specializer-name)
                             (eq (car specializer-name) 'EQL)
                        )
                      `(LIST 'EQL ,(second specializer-name))
                      `(FIND-CLASS ',specializer-name)
                  ) )
                  req-specializer-forms
        ) ) )
        (let* ((reqanz (length req-vars))
               (lambda-list (nreconc req-vars specialized-lambda-list))
               (optanz
                 (let ((h (cdr (member '&OPTIONAL lambda-list :test #'eq))))
                   (or (position-if #'lambda-list-keyword-p h) (length h))
               ) )
               (keyp (not (null (member '&KEY lambda-list :test #'eq))))
               (restp (or keyp (not (null (member '&REST lambda-list :test #'eq)))))
               (keywords
                 (mapcar
                   #'(lambda (item)
                       (when (consp item) (setq item (first item)))
                       (if (consp item)
                         (first item)
                         (intern (symbol-name item) *keyword-package*)
                     ) )
                   (let ((h (cdr (member '&KEY lambda-list :test #'eq))))
                     (subseq h 0 (position-if #'lambda-list-keyword-p h))
               ) ) )
               (allowp (and keyp (not (null (member '&ALLOW-OTHER-KEYS lambda-list :test #'eq)))))
              )
          ; Methoden haben ein implizites &allow-other-keys (28.1.6.4.):
          (when (and keyp (not allowp))
            (let ((index (+ (position '&KEY lambda-list :test #'eq) 1 (length keywords))))
              (setq lambda-list
                `(,@(subseq lambda-list 0 index) &ALLOW-OTHER-KEYS
                  ,@(subseq lambda-list index)
                 )
          ) ) )
          (let* ((self (gensym))
                 (wants-next-method-p
                   (or (equal qualifiers '()) (equal qualifiers '(:around)))
                 )
                 (compile nil)
                 (lambdabody
                   (multiple-value-bind (body-rest declarations docstring)
                       (sys::parse-body body t env)
                     (declare (ignore docstring))
                     (setq compile (member '(COMPILE) declarations :test #'equal))
                     (when ignorable-req-vars
                       (push `(IGNORABLE ,@(nreverse ignorable-req-vars))
                             declarations
                     ) )
                     (let ((lambdabody-part1
                             `(,lambda-list
                               ,@(if declarations `((DECLARE ,@declarations)))
                              )
                           )
                           (lambdabody-part2
                             (if (eq caller 'generic-function)
                               body-rest
                               ; impliziter Block
                               `((BLOCK ,(function-block-name funname) ,@body-rest))
                          )) )
                       (if wants-next-method-p
                         (let ((cont (gensym)) ; Variable für die Continuation
                               (req-dummies ; Liste von reqanz Dummies
                                 (n-gensyms reqanz)
                               )
                               (rest-dummy (if (or restp (> optanz 0)) (gensym)))
                               (lambda-expr `(LAMBDA ,@lambdabody-part1 ,@lambdabody-part2)))
                           `(; neue Lambda-Liste:
                             (,cont
                              ,@req-dummies
                              ,@(if rest-dummy `(&REST ,rest-dummy) '())
                             )
                             (MACROLET
                               ((CALL-NEXT-METHOD (&REST NEW-ARG-EXPRS)
                                  (IF NEW-ARG-EXPRS
                                    (LIST 'IF ',cont
                                      ; Let's do argument checking in the interpreter only
                                      (LIST 'IF '(EVAL-WHEN (EVAL) T)
                                        (LIST '%CALL-NEXT-METHOD
                                          ',self
                                          ',cont
                                          (LIST ',(if rest-dummy 'LIST* 'LIST)
                                            ,@(mapcar #'(lambda (x) `',x) req-dummies)
                                            ,@(if rest-dummy `(',rest-dummy) '())
                                          )
                                          (CONS 'LIST NEW-ARG-EXPRS)
                                        )
                                        (LIST* 'FUNCALL ',cont NEW-ARG-EXPRS)
                                      )
                                      (LIST* '%NO-NEXT-METHOD ',self NEW-ARG-EXPRS)
                                    )
                                    ,(if rest-dummy
                                       `(LIST 'IF ',cont
                                          (LIST 'APPLY ',cont
                                            ,@(mapcar #'(lambda (x) `',x) req-dummies)
                                            ',rest-dummy
                                          )
                                          (LIST 'APPLY '(FUNCTION %NO-NEXT-METHOD)
                                            ',self
                                            ,@(mapcar #'(lambda (x) `',x) req-dummies)
                                            ',rest-dummy
                                        ) )
                                       `(LIST 'IF ',cont
                                          (LIST 'FUNCALL ',cont
                                            ,@(mapcar #'(lambda (x) `',x) req-dummies)
                                          )
                                          (LIST '%NO-NEXT-METHOD
                                            ',self
                                            ,@(mapcar #'(lambda (x) `',x) req-dummies)
                                        ) )
                                     )
                                ) )
                                (NEXT-METHOD-P () ',cont)
                               )
                               ; neuer Body:
                               ,(if rest-dummy
                                  `(APPLY (FUNCTION ,lambda-expr)
                                          ,@req-dummies ,rest-dummy
                                   )
                                  `(,lambda-expr ,@req-dummies)
                                )
                            ))
                         )
                         `(,@lambdabody-part1
                           (MACROLET
                             ((CALL-NEXT-METHOD ()
                                (ERROR-OF-TYPE 'PROGRAM-ERROR
                                  (ENGLISH "~S ~S: ~S is invalid within ~S methods")
                                  ',caller ',funname 'CALL-NEXT-METHOD ',(first qualifiers)
                              ) )
                              (NEXT-METHOD-P ()
                                (ERROR-OF-TYPE 'PROGRAM-ERROR
                                  (ENGLISH "~S ~S: ~S is invalid within ~S methods")
                                  ',caller ',funname 'NEXT-METHOD-P ',(first qualifiers)
                             )) )
                             ,@lambdabody-part2
                          ))
                )) ) ) )
            `(MAKE-STANDARD-METHOD
               :INITFUNCTION
                 #'(LAMBDA (,self)
                     ,@(if compile '((DECLARE (COMPILE))))
                     (%OPTIMIZE-FUNCTION-LAMBDA
                       ,(if wants-next-method-p `(T) `())
                       ,@lambdabody
                   ) )
               :WANTS-NEXT-METHOD-P ',wants-next-method-p
               :PARAMETER-SPECIALIZERS (LIST ,@(nreverse req-specializer-forms))
               :QUALIFIERS ',qualifiers
               :SIGNATURE '(,reqanz ,optanz ,restp ,keyp ,keywords ,allowp)
             )
) ) ) ) ) )

;; 28.1.6.3. agreement on parameter specializers and qualifiers
(defun methods-agree-p (method1 method2)
  (and (equal (std-method-qualifiers method1) (std-method-qualifiers method2))
       (specializers-agree-p (std-method-parameter-specializers method1)
                             (std-method-parameter-specializers method2)
) )    )
(defun specializers-agree-p (specializers1 specializers2)
  (and (eql (length specializers1) (length specializers2))
       (every #'same-specializers-p specializers1 specializers2)
) )
(defun same-specializers-p (parspec1 parspec2)
  (or ; zwei gleiche Klassen?
      (eq parspec1 parspec2)
      ; zwei gleiche EQL-Specializer?
      (and (consp parspec1) (consp parspec2)
           (eql (second parspec1) (second parspec2))
) )   )

;; 28.1.6.2. applicable methods
(defun method-applicable-p (method required-arguments)
  (every #'typep required-arguments (std-method-parameter-specializers method))
)

;; 28.1.7.1. sorting the applicable methods by precedence order
(defun sort-applicable-methods (methods required-arguments argument-order)
  (sort (copy-list methods)
        #'(lambda (method1 method2) ; method1 < method2 ?
            (let ((specializers1 (std-method-parameter-specializers method1))
                  (specializers2 (std-method-parameter-specializers method2)))
              (dolist (arg-index argument-order nil)
                (let ((arg (nth arg-index required-arguments))
                      (psp1 (nth arg-index specializers1))
                      (psp2 (nth arg-index specializers2)))
                  (if (consp psp1)
                    (if (consp psp2)
                      nil        ; (EQL x) = (EQL x)
                      (return t) ; (EQL x) < <class>  ==>  method1 < method2
                    )
                    (if (consp psp2)
                      (return nil) ; <class> > (EQL x)   ==>  method1 > method2
                      ; Zwei Klassen: vergleiche die Position in der CPL von arg:
                      (let* ((cpl (class-precedence-list (class-of arg)))
                             (pos1 (position psp1 cpl))
                             (pos2 (position psp2 cpl)))
                        (cond ((< pos1 pos2) (return t)) ; method1 < method2
                              ((> pos1 pos2) (return nil)) ; method1 > method2
                      ) )
          ) ) ) ) ) )
) )

; Für STANDARD Methodenkombination: Aufspalten der Methoden nach Qualifiern
(defun partition-method-list (methods)
  (let ((primary-methods '())
        (before-methods '())
        (after-methods '())
        (around-methods '()))
    (dolist (method methods)
      (let ((quals (std-method-qualifiers method)))
        (cond ((equal quals '())        (push method primary-methods))
              ((equal quals '(:before)) (push method before-methods))
              ((equal quals '(:after))  (push method after-methods))
              ((equal quals '(:around)) (push method around-methods))
    ) ) )
    (values
      (nreverse primary-methods)
      (nreverse before-methods)
      (nreverse after-methods)
      (nreverse around-methods)
) ) )


;;; Generische Funktionen

; Low-Level-Repräsentation:
; Compilierte Funktionen (Cclosures), bei denen im Flag-Byte des Code-Vektors
; ein bestimmtes Bit gesetzt ist. Hintendran zusätzlich:
; - die Signatur, eine Liste (reqanz optanz restp keywords allowp),
; - die Argument-Precedence-Order, als Liste der Zahlen von 0 bis reqanz-1,
; - die Liste aller Methoden.

; Der Compiler benutzt (bei GENERIC-FLET, GENERIC-LABELS) und der Evaluator
; setzt ebenfalls voraus, dass eine generische Funktion ihre Aufrufkonvention
; nicht ändert.
; Eine generische Funktion mit Signatur (reqanz optanz restp keywords allowp)
; ist von Anfang an (!) eine compilierte Funktion mit
;         reqanz  required-Parametern
;         0       optionalen Parametern
;         &rest genau dann wenn (or (> optanz 0) restp),
;         ohne &key.
(defun callinfo (reqanz optanz restp keywords allowp)
  (declare (ignore keywords allowp))
  (list reqanz 0 (or (> optanz 0) restp) nil nil nil)
)

(defun gf-signature (gf)
  (sys::%record-ref gf 3)
)
(defun (setf gf-signature) (new gf)
  (setf (sys::%record-ref gf 3) new)
)

(defun gf-argorder (gf)
  (sys::%record-ref gf 4)
)
(defun (setf gf-argorder) (new gf)
  (setf (sys::%record-ref gf 4) new)
)

(defun gf-methods (gf)
  (sys::%record-ref gf 5)
)
(defun (setf gf-methods) (new gf)
  (setf (sys::%record-ref gf 5) new)
)

; Der Dispatch-Code für generische Funktionen wird mit
; `(%GENERIC-FUNCTION-LAMBDA ,@lambdabody)
; - ähnlich zu `(FUNCTION (LAMBDA ,@lambdabody)) - gebildet.
; Es dürfen darin nicht vorkommen:
; - Zugriff auf dynamische Variablen, Binden von dynamischen Variablen,
; - nichttriviale BLOCK, RETURN-FROM, TAGBODY, GO Konstrukte,
; - Aufruf globaler Funktionen, die nicht inline sind,
; - Bildung von nicht-autonomen Funktionen (Closures).
; Nötig ist also:
;   (declare (inline case eql eq typep
;                    arrayp bit-vector-p characterp complexp consp floatp
;                    functionp clos::generic-function-p hash-table-p integerp
;                    listp null numberp packagep pathnamep sys::logical-pathname-p
;                    random-state-p rationalp readtablep realp sys::sequencep
;                    clos::std-instance-p streamp sys::file-stream-p
;                    sys::synonym-stream-p sys::broadcast-stream-p
;                    sys::concatenated-stream-p sys::two-way-stream-p
;                    sys::echo-stream-p sys::string-stream-p stringp
;                    clos::structure-object-p symbolp vectorp
;                    class-of cons gethash funcall apply ...
;   )        )

; Liefert eine generische Funktion ohne Dispatch-Code. Nicht aufrufbar!!
(let* ((prototype ; eine sinnlose Funktion
         #'(lambda (&rest args) (declare (compile) (ignore args))
             (tagbody 1 (go 1))
           )
       )
       (prototype-code (sys::%record-ref prototype 1)))
  (defun %make-gf (name signature argorder methods)
    (sys::%make-closure name prototype-code
                        (list nil signature argorder methods)
  ) )
)

#|
(defun make-gf (name lambdabody signature argorder methods)
  (let ((preliminary
          (eval `(LET ()
                   (DECLARE (COMPILE))
                   (%GENERIC-FUNCTION-LAMBDA ,@lambdabody)
                 )
       )) )
    (sys::%make-closure
      name
      (sys::closure-codevec preliminary)
      (list
        (sys::%record-ref preliminary 2)
        signature
        argorder
        methods
) ) ) )
|#


#|

;; Generische Funktionen mit primitivem Dispatch:

(defun make-slow-gf (name signature argorder methods)
  (let* ((final (%make-gf name signature argorder methods))
         (preliminary
           (eval `(LET ((GF ',final))
                    (DECLARE (COMPILE))
                    (%GENERIC-FUNCTION-LAMBDA (&REST ARGS)
                      (DECLARE (INLINE APPLY))
                      (APPLY 'SLOW-FUNCALL-GF GF ARGS)
                  ) )
        )) )
    (setf (sys::%record-ref final 1) (sys::closure-codevec preliminary))
    (setf (sys::%record-ref final 2) (sys::%record-ref preliminary 2))
    final
) )

(let* ((prototype
         (let ((gf 'magic))
           (declare (compile))
           (%generic-function-lambda (&rest args)
             (declare (inline apply))
             (apply 'slow-funcall-gf gf args)
       ) ) )
       (prototype-code (sys::%record-ref prototype 1))
       (prototype-consts (sys::%record-ref prototype 2)))
  (defun finalize-slow-gf (gf)
    (setf (sys::%record-ref gf 1) prototype-code)
    (setf (sys::%record-ref gf 2)
          (let ((v (copy-seq prototype-consts)))
            (setf (svref v 0) (substitute gf 'magic (svref v 0)))
            v
    )     )
  )
  (defun gf-never-called-p (gf) (eq (sys::%record-ref gf 1) prototype-code))
  (defun warn-if-gf-already-called (gf) )
)

; Aufruf einer generischen Funktion
(defun slow-funcall-gf (gf &rest args)
  (let ((reqanz (first (gf-signature gf)))
        (arg-order (gf-argorder gf))
        (methods (gf-methods gf)))
    (unless (>= (length args) reqanz)
      (error-of-type 'program-error
        (ENGLISH "Too few arguments to ~S: ~S")
        gf args
    ) )
    (let ((req-args (subseq args 0 reqanz)))
      ; Determine the effective method:
      ; 1. Select the applicable methods:
      (setq methods
        (remove-if-not #'(lambda (method) (method-applicable-p method req-args))
                       methods
      ) )
      (when (null methods)
        (return-from slow-funcall-gf (apply #'no-applicable-method gf args))
      )
      ; 2. Sort the applicable methods by precedence order:
      (setq methods (sort-applicable-methods methods req-args arg-order))
      ; 3. Apply method combination:
      ; Nur STANDARD Methoden-Kombination ist implementiert.
      ; Aufspalten in einzelne Methoden-Typen:
      (multiple-value-bind (primary-methods before-methods after-methods around-methods)
          (partition-method-list methods)
        (when (null primary-methods)
          (return-from slow-funcall-gf (apply #'no-primary-method gf args))
        )
        ; Methoden zu einer "effektiven Methode" kombinieren:
        (labels ((ef-1 (primary-methods before-methods after-methods around-methods)
                   (if (null around-methods)
                     (ef-2 primary-methods before-methods after-methods)
                     (let* ((1method (first around-methods))
                            (1function (std-method-function 1method)))
                       (if (std-method-wants-next-method-p 1method)
                         (let ((next-ef
                                 (ef-1 primary-methods before-methods after-methods (rest around-methods))
                              ))
                           #'(lambda (&rest args) (apply 1function next-ef args))
                         )
                         #'(lambda (&rest args) (apply 1function args))
                 ) ) ) )
                 (ef-2 (primary-methods before-methods after-methods)
                   (if (null after-methods)
                     (ef-3 primary-methods before-methods)
                     (let* ((1method (first after-methods))
                            (1function (std-method-function 1method)))
                       (let ((next-ef (ef-2 primary-methods before-methods (rest after-methods))))
                         #'(lambda (&rest args) (multiple-value-prog1 (apply next-ef args) (apply 1function args)))
                 ) ) ) )
                 (ef-3 (primary-methods before-methods)
                   (if (null before-methods)
                     (ef-4 primary-methods)
                     (let* ((1method (first before-methods))
                            (1function (std-method-function 1method)))
                       (let ((next-ef (ef-3 primary-methods (rest before-methods))))
                         #'(lambda (&rest args) (progn (apply 1function args) (apply next-ef args)))
                 ) ) ) )
                 (ef-4 (primary-methods)
                   (if (null primary-methods)
                     nil ; keine Funktion, NEXT-METHOD-P reagiert darauf
                     (let* ((1method (first primary-methods))
                            (1function (std-method-function 1method)))
                       (if (std-method-wants-next-method-p 1method)
                         (let ((next-ef (ef-4 (rest primary-methods))))
                           #'(lambda (&rest args) (apply 1function next-ef args))
                         )
                         #'(lambda (&rest args) (apply 1function args))
                )) ) ) )
          (let ((ef (ef-1 primary-methods before-methods after-methods around-methods)))
            ; Keyword-Check (28.1.6.4., 28.1.6.5.) ??
            ; Effektive Methode liefern. Sie wird dann auf die Argumente angewandt:
            ef
) ) ) ) ) )

|#


;; Generische Funktionen mit optimiertem Dispatch:

(defun make-fast-gf (name signature argorder)
  (let ((gf (%make-gf name signature argorder '())))
    (finalize-fast-gf gf)
    gf
) )

(let ((prototype-table (make-hash-table :test #'equal)))
  (defun finalize-fast-gf (gf)
    (let* ((signature (gf-signature gf))
           (reqanz (first signature))
           (restp (or (third signature) (> (second signature) 0)))
           (hash-key (cons reqanz restp))
           (prototype
             (or (gethash hash-key prototype-table)
                 (setf (gethash hash-key prototype-table)
                       (let* ((reqvars (n-gensyms reqanz))
                              (proto-gf
                                (eval `(LET ((GF 'MAGIC))
                                         (DECLARE (COMPILE))
                                         (%GENERIC-FUNCTION-LAMBDA (,@reqvars ,@(if restp '(&REST ARGS) '()))
                                           (DECLARE (INLINE FUNCALL) (IGNORABLE ,@reqvars ,@(if restp '(ARGS) '())))
                                           (FUNCALL 'INITIAL-FUNCALL-GF GF)
                                       ) )
                             )) )
                         ; (sys::%record-ref proto-gf 1) müssen wir aufbewahren.
                         ; (sys::%record-ref proto-gf 2) = #(NIL INITIAL-FUNCALL-GF MAGIC)
                         (sys::%record-ref proto-gf 1)
          )) )   )     )
      (setf (sys::%record-ref gf 1) prototype)
      (setf (sys::%record-ref gf 2) (vector 'NIL 'INITIAL-FUNCALL-GF gf))
  ) )
  (defun gf-never-called-p (gf)
    (let* ((signature (gf-signature gf))
           (reqanz (first signature))
           (restp (or (third signature) (> (second signature) 0)))
           (hash-key (cons reqanz restp))
           (prototype (gethash hash-key prototype-table)))
      (eq (sys::%record-ref gf 1) prototype)
  ) )
  (defvar *dynamically-modifiable-generic-function-names*
    ; A list of names of functions, which ANSI CL explicitly denotes as
    ; "Standard Generic Function"s, meaning that the user may add methods.
    '(add-method allocate-instance class-name describe-object find-method
      function-keywords initialize-instance make-instance method-qualifiers
      no-applicable-method no-next-method no-primary-method print-object
      reinitialize-instance remove-method shared-initialize slot-missing
      slot-unbound
  )  )
  (defvar *warn-if-gf-already-called* t)
  (defun warn-if-gf-already-called (gf)
    (when (and *warn-if-gf-already-called* (not (gf-never-called-p gf))
               (not (member (sys::%record-ref gf 0)
                            *dynamically-modifiable-generic-function-names*
                            :test #'eq)))
      (warn (ENGLISH "The generic function ~S is being modified, but has already been called.")
            gf
  ) ) )
)

; Der eigentliche Dispatch-Code wird erst beim ersten Aufruf der Funktion
; berechnet, um aufeinanderfolgende Methoden-Definitionen nicht zu teuer
; zu machen.

; Erster Aufruf einer generischen Funktion:
(defun initial-funcall-gf (gf)
  (install-dispatch gf)
  gf
)

; Installiert den endgültigen Dispatch-Code in eine generische Funktion.
(defun install-dispatch (gf)
  (multiple-value-bind (bindings lambdabody) (compute-dispatch gf)
    (let ((preliminary
            (eval `(LET ,bindings
                     (DECLARE (COMPILE))
                     (%GENERIC-FUNCTION-LAMBDA ,@lambdabody)
                   )
         )) )
      (setf (sys::%record-ref gf 1) (sys::%record-ref preliminary 1))
      (setf (sys::%record-ref gf 2) (sys::%record-ref preliminary 2))
) ) )

; Berechnet den Dispatch-Code einer generischen Funktion.
; Er hat folgendes Aussehen:
; (LAMBDA (variablen)      ; die required einzeln, alles andere mit &rest
;   (DECLARE (INLINE ...)) ; alles inline wegen %GENERIC-FUNCTION-LAMBDA
;   If-Kaskaden, dabei werden EQL-Parameter-Specializer und die meisten
;   Builtin-Klassen per TYPEP inline abgefragt.
;   Für die anderen required-Parameter wird CLASS-OF aufgerufen, die Ergebnisse
;   gesammelt und als Index in eine Hash-Tabelle genommen. Dort steht die
;   effektive Methode:
;   (LET ((EM (GETHASH (CONS (CLASS-OF ...) ...) ht1)))
;     (WHEN EM (RETURN-FROM block (APPLY EM Argumente)))
;   )
;   Wenn das nicht gelungen ist:
;   (APPLY 'COMPUTE-AND-ADD-EFFECTIVE-METHOD gf Argumente)
; )
; Das (APPLY ... Argumente) braucht man nicht hinzuschreiben, das macht
; %GENERIC-FUNCTION-LAMBDA automatisch.
(defun compute-dispatch (gf)
  (let* ((signature (gf-signature gf))
         (req-anz (first signature))
         (req-vars (n-gensyms req-anz))
         (restp (or (third signature) (> (second signature) 0)))
         (rest-var (if restp (gensym)))
         (apply-fun (if restp 'APPLY 'FUNCALL))
         (apply-args `(,@req-vars ,@(if restp `(,rest-var) '())))
         (arg-order (gf-argorder gf))
         (methods (gf-methods gf))
         (block-name (gensym))
         (maybe-no-applicable nil)
         (ht-vars '())) ; Liste von Hashtabellen-Variablen und ihren Inits
    ; Wir machen eine Rekursion über die Argumente.
    (labels
       ((recursion (remaining-args ; ein nthcdr von arg-order
                    remaining-methods ; Teilliste von methods
                    class-of-exprs ; Liste von CLASS-OF Expressions
                   )
          (if (null remaining-methods)
            (progn
              (setq maybe-no-applicable t)
              'NIL ; nichts tun, später NO-APPLICABLE-METHOD aufrufen
            )
            (if (null remaining-args)
              ; alle Argumente abgearbeitet
              #| ; benutze GETHASH :
              (let ((ht-var (gensym))
                    (n (length class-of-exprs)) ; indiziere mit n-Tupeln
                    ht-init ; Expression zum Initialisieren von ht-var
                    ht-key-binding ; Bindung einer Variablen an ein n-Tupel
                    em-expr ; Expression zum Auffinden der EM
                    setf-em-expr ; Expression-Teil zum Setzen der EM
                   )
                (if (eql n 0)
                  (setq ht-init 'NIL
                        ht-key-binding '()
                        em-expr ht-var
                        setf-em-expr `(SETQ ,ht-var)
                  )
                  (let ((tuple-var (gensym)))
                    (setq ht-init
                          `(MAKE-HASH-TABLE
                             :TEST (FUNCTION ,(if (eql n 1) 'EQ 'EQUAL))
                           )
                          ht-key-binding
                          `((,tuple-var
                             ,(let ((tuple-fun (hash-tuple-function n)))
                                (if (member '&rest (second tuple-fun))
                                  `(,tuple-fun ,@(reverse class-of-exprs))
                                  ; kein &rest -> kann optimieren
                                  ; (der Compiler kann's noch nicht so gut)
                                  (sublis (mapcar #'cons (second tuple-fun) (reverse class-of-exprs))
                                          (third tuple-fun)
                              ) ) )
                           ))
                          em-expr
                          `(GETHASH ,tuple-var ,ht-var)
                          setf-em-expr
                          ; `(SETF (GETHASH ,tuple-var ,ht-var)) ginge auch;
                          ; das Folgende spart aber zwei temporäre Variablen:
                          `(SYSTEM::PUTHASH ,tuple-var ,ht-var)
                ) ) )
                (push (list ht-var ht-init) ht-vars)
                `(LET ,ht-key-binding
                   (RETURN-FROM ,block-name
                     (OR ,em-expr
                         (,@setf-em-expr
                               (,apply-fun 'COMPUTE-EFFECTIVE-METHOD ',gf
                                           ,@apply-args
                     )   )     )
                 ) )
              )
              |# ; benutze CLASS-GETHASH und CLASS-TUPLE-GETHASH :
              (let ((ht-var (gensym))
                    (n (length class-of-exprs)) ; indiziere mit n-Tupeln
                    ht-init ; Expression zum Initialisieren von ht-var
                    em-expr ; Expression zum Auffinden der EM
                    setf-em-expr ; Expression-Teil zum Setzen der EM
                   )
                (if (eql n 0)
                  (setq ht-init 'NIL
                        em-expr ht-var
                        setf-em-expr `(SETQ ,ht-var)
                  )
                  (setq class-of-exprs
                        (reverse class-of-exprs)
                        ht-init
                        `(MAKE-HASH-TABLE
                           :TEST (FUNCTION ,(if (eql n 1) 'EQ 'EQUAL))
                         )
                        em-expr
                        (if (eql n 1) ; je nachdem welches schneller ist
                          ; `(GETHASH ,@class-of-exprs ,ht-var) ==
                          `(CLASS-GETHASH ,ht-var ,(second (first class-of-exprs)))
                          `(CLASS-TUPLE-GETHASH ,ht-var ,@(mapcar #'second class-of-exprs))
                        )
                        setf-em-expr
                        `(SYSTEM::PUTHASH
                          ,(let ((tuple-fun (hash-tuple-function n)))
                             (if (member '&rest (second tuple-fun))
                               `(,tuple-fun ,@class-of-exprs)
                               ; kein &rest -> kann optimieren
                               ; (der Compiler kann's noch nicht so gut)
                               (sublis (mapcar #'cons (second tuple-fun) class-of-exprs)
                                       (third tuple-fun)
                           ) ) )
                          ,ht-var
                         )
                ) )
                (push (list ht-var ht-init) ht-vars)
                `(RETURN-FROM ,block-name
                   (OR ,em-expr
                       (,@setf-em-expr
                             (,apply-fun 'COMPUTE-EFFECTIVE-METHOD ',gf
                                         ,@apply-args
                   )   )     )
                 )
              )
              ; nächstes Argument abarbeiten:
              (let* ((arg-index (first remaining-args))
                     (arg-var (nth arg-index req-vars))
                     (eql-cases ; alle EQL-Specializer für dieses Argument
                       (remove-duplicates
                         (mapcar #'second
                           (remove-if-not #'consp
                             (mapcar #'(lambda (m)
                                         (nth arg-index
                                           (std-method-parameter-specializers m)
                                       ) )
                               remaining-methods
                         ) ) )
                         :test #'eql
                     ) )
                     (eql-caselist ; Fall-Liste für CASE
                       (mapcar
                         #'(lambda (object)
                             `((,object)
                               ,(recursion
                                  (cdr remaining-args)
                                  (remove-if-not
                                    #'(lambda (m)
                                        (typep object
                                          (nth arg-index
                                            (std-method-parameter-specializers m)
                                      ) ) )
                                    remaining-methods
                                  )
                                  class-of-exprs
                                )
                              )
                           )
                         eql-cases
                    )) )
                ; Fürs weitere brauchen wir die EQL-Methoden nicht mehr zu
                ; betrachten.
                (setq remaining-methods
                      (remove-if
                        #'(lambda (m)
                            (consp
                              (nth arg-index
                                (std-method-parameter-specializers m)
                          ) ) )
                        remaining-methods
                )     )
                ((lambda (other-cases)
                   (if eql-caselist
                     `(CASE ,arg-var ,@eql-caselist (T ,other-cases))
                     other-cases
                 ) )
                 (let ((classes
                         (delete <t>
                           (delete-duplicates
                             (mapcar #'(lambda (m)
                                         (nth arg-index
                                           (std-method-parameter-specializers m)
                                       ) )
                                     remaining-methods
                      )) ) ) )
                   ; Falls alle Klassen, auf die zu testen ist,
                   ; Built-In-Klassen sind, machen wir den Typ-Dispatch
                   ; inline. Denn in der Hierarchie der Built-In-Klassen
                   ; (die außer NULL und VECTOR keine mehrfache Vererbung
                   ; kennt) sind alle CPLs konsistent. Man kann daher mit
                   ; (subclassp (class-of obj) class) == (typep obj class)
                   ; arbeiten.
                   ; Im anderen Fall ist sowieso ein Hash-Tabellen-Zugriff
                   ; nötig, dann sparen wir uns den Test auf die Built-In-
                   ; Klassen und beziehen ihn in die Hash-Tabelle ein.
                   (if (and (every #'bc-p classes)
                            (<= (length classes) 5) ; zu viele Fälle -> hashen
                       )
                     (labels
                        ((built-in-subtree (class remaining-classes remaining-methods)
                           ; behandelt die Fälle, dass das Argument der Klasse
                           ; class angehört und auf Zugehörigkeit zu einer der
                           ; remaining-classes abgeprüft werden muss.
                           ; (Man kann voraussetzen, dass (bc-and class x) /= nil
                           ; für alle x aus remaining-classes.)
                           (if (null remaining-classes)
                             ; Keine Fallunterscheidung mehr nötig
                             (recursion
                               (cdr remaining-args)
                               (remove-if-not
                                 #'(lambda (m)
                                     (bc-and class
                                       (nth arg-index
                                         (std-method-parameter-specializers m)
                                   ) ) )
                                 remaining-methods
                               )
                               class-of-exprs
                             )
                             ; Fallunterscheidung mittels TYPEP
                             (let ((test-class (first remaining-classes)))
                               ; besser test-class maximal wählen:
                               (loop
                                 (let ((other-class
                                         (find-if
                                           #'(lambda (x)
                                               (and (subclassp test-class x)
                                                    (not (eq test-class x))
                                             ) )
                                           remaining-classes
                                      )) )
                                   (unless other-class (return))
                                   (setq test-class other-class)
                               ) )
                               `(IF (TYPEP ,arg-var ',(class-classname test-class))
                                  ,(built-in-subtree
                                     (bc-and class test-class) ; /= nil !
                                     (remove 'nil
                                       (mapcar
                                         #'(lambda (x) (bc-and x test-class))
                                         (remove test-class remaining-classes)
                                     ) )
                                     (remove-if-not
                                       #'(lambda (m)
                                           (bc-and
                                             (nth arg-index
                                               (std-method-parameter-specializers m)
                                             )
                                             test-class
                                         ) )
                                       remaining-methods
                                   ) )
                                  ,(built-in-subtree
                                     (bc-and-not class test-class) ; /= nil !
                                     (remove 'nil
                                       (mapcar
                                         #'(lambda (x) (bc-and-not x test-class))
                                         remaining-classes
                                     ) )
                                     (remove-if-not
                                       #'(lambda (m)
                                           (bc-and-not
                                             (nth arg-index
                                               (std-method-parameter-specializers m)
                                             )
                                             test-class
                                         ) )
                                       remaining-methods
                                   ) )
                                )
                        )) ) )
                       (built-in-subtree <t> classes remaining-methods)
                     )
                     (recursion
                       (cdr remaining-args)
                       remaining-methods
                       (cons `(CLASS-OF ,arg-var) class-of-exprs)
                )) ) )
       )) ) ) )
      (let ((form (recursion arg-order methods '())))
        (values
          ; bindings
          (nreverse ht-vars)
          ; lambdabody
          `((,@req-vars ,@(if restp `(&REST ,rest-var) '()))
            (DECLARE
              (INLINE ; für die Fallunterscheidungen:
                      CASE EQL EQ TYPEP
                      ; bei der Inline-Expansion von TYPEP auf Built-In-Klassen:
                      ARRAYP BIT-VECTOR-P CHARACTERP COMPLEXP CONSP FLOATP
                      FUNCTIONP CLOS::GENERIC-FUNCTION-P HASH-TABLE-P INTEGERP
                      LISTP NULL NUMBERP PACKAGEP PATHNAMEP SYS::LOGICAL-PATHNAME-P
                      RANDOM-STATE-P RATIONALP READTABLEP REALP SYS::SEQUENCEP
                      CLOS::STD-INSTANCE-P STREAMP SYS::FILE-STREAM-P
                      SYS::SYNONYM-STREAM-P SYS::BROADCAST-STREAM-P
                      SYS::CONCATENATED-STREAM-P SYS::TWO-WAY-STREAM-P
                      SYS::ECHO-STREAM-P SYS::STRING-STREAM-P STRINGP
                      CLOS::STRUCTURE-OBJECT-P SYMBOLP VECTORP
                      ; Finden und Aufruf der effektiven Methode:
                      CLASS-OF CONS GETHASH CLASS-GETHASH CLASS-TUPLE-GETHASH
                      SYS::PUTHASH FUNCALL APPLY
            ) )
            (BLOCK ,block-name
              ,form
              ,@(if maybe-no-applicable
                  `((,apply-fun 'NO-APPLICABLE-METHOD ',gf ,@apply-args))
                )
           ))
) ) ) ) )

; Unsere EQUAL-Hashfunktion schaut in Cons-Bäume nur bis Tiefe 4 hinein.
; Ein Tupel aus maximal 16 Elementen kann zu einem solchen Baum gemacht werden.
(defun hash-tuple-function (n) ; n>0
  (case n
    (1 '(lambda (t1) t1))
    (2 '(lambda (t1 t2) (cons t1 t2)))
    (3 '(lambda (t1 t2 t3) (cons t1 (cons t2 t3))))
    (4 '(lambda (t1 t2 t3 t4) (cons (cons t1 t2) (cons t3 t4))))
    (5 '(lambda (t1 t2 t3 t4 t5) (cons (cons t1 t2) (cons t3 (cons t4 t5)))))
    (6 '(lambda (t1 t2 t3 t4 t5 t6)
          (cons (cons t1 t2) (cons (cons t3 t4) (cons t5 t6))) ))
    (7 '(lambda (t1 t2 t3 t4 t5 t6 t7)
          (cons (cons t1 (cons t2 t3)) (cons (cons t4 t5) (cons t6 t7))) ))
    (8 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8)
          (cons (cons (cons t1 t2) (cons t3 t4)) (cons (cons t5 t6) (cons t7 t8))) ))
    (9 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9)
          (cons (cons (cons t1 t2) (cons t3 t4)) (cons (cons t5 t6) (cons t7 (cons t8 t9)))) ))
    (10 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)
           (cons (cons (cons t1 t2) (cons t3 t4)) (cons (cons t5 t6) (cons (cons t7 t8) (cons t9 t10)))) ))
    (11 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11)
           (cons (cons (cons t1 t2) (cons t3 t4)) (cons (cons t5 (cons t6 t7)) (cons (cons t8 t9) (cons t10 t11)))) ))
    (12 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12)
           (cons (cons (cons t1 t2) (cons t3 t4)) (cons (cons (cons t5 t6) (cons t7 t8)) (cons (cons t9 t10) (cons t11 t12)))) ))
    (13 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13)
           (cons (cons (cons t1 t2) (cons t3 (cons t4 t5))) (cons (cons (cons t6 t7) (cons t8 t9)) (cons (cons t10 t11) (cons t12 t13)))) ))
    (14 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14)
           (cons (cons (cons t1 t2) (cons (cons t3 t4) (cons t5 t6))) (cons (cons (cons t7 t8) (cons t9 t10)) (cons (cons t11 t12) (cons t13 t14)))) ))
    (15 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15)
           (cons (cons (cons t1 (cons t2 t3)) (cons (cons t4 t5) (cons t6 t7))) (cons (cons (cons t8 t9) (cons t10 t11)) (cons (cons t12 t13) (cons t14 t15)))) ))
    (16 '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16)
           (cons (cons (cons (cons t1 t2) (cons t3 t4)) (cons (cons t5 t6) (cons t7 t8))) (cons (cons (cons t9 t10) (cons t11 t12)) (cons (cons t13 t14) (cons t15 t16)))) ))
    (t '(lambda (t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 &rest more-t)
          (cons (cons (cons (cons t1 t2) (cons t3 t4)) (cons (cons t5 t6) (cons t7 t8))) (cons (cons (cons t9 t10) (cons t11 t12)) (cons (cons t13 t14) more-t))) ))
) )

; Berechnet die effektive Methode zu gegebenen Argumenten.
; Es ist eigentlich die effektive Methode zu allen Argumenten, die dieselben
; EQL- und Klassen-Einschränkungen haben wie die gegebenen Argumente, aber
; darum hat sich compute-dispatch schon gekümmert.
(defun compute-effective-method (gf &rest args)
  (tagbody restart-compute
    (return-from compute-effective-method
      (let* ((signature (gf-signature gf))
             (req-anz (first signature))
             (req-vars (n-gensyms req-anz))
             (req-args (subseq args 0 req-anz))
             (restp (or (third signature) (> (second signature) 0)))
             (rest-var (if restp (gensym)))
             (apply-fun (if restp 'APPLY 'FUNCALL))
             (apply-args `(,@req-vars ,@(if restp `(,rest-var) '())))
             (lambdalist `(,@req-vars ,@(if restp `(&REST ,rest-var) '())))
             (opt-vars '())
             (key-vars '())
             (lambdalist-keypart '())
             (arg-order (gf-argorder gf))
             (methods (gf-methods gf)))
        ; Determine the effective method:
        ; 1. Select the applicable methods:
        (setq methods
          (remove-if-not #'(lambda (method) (method-applicable-p method req-args))
                         methods
        ) )
        (when (null methods)
          (apply #'no-applicable-method gf args)
          (go restart-compute)
        )
        ; 28.1.6.4., 28.1.6.5.: Keyword arguments in generic functions
        (when restp
          ; Die generische Funktion hat &REST oder &KEY, also auch alle Methoden.
          ; "If the lambda-list of ... the generic function definition contains
          ;  &allow-other-keys, all keyword arguments are accepted."
          (unless (fifth signature)
            ; "The specific set of keyword arguments accepted ... varies according
            ;  to the applicable methods."
            (let ((signatures (mapcar #'std-method-signature methods)))
              ; "A method that has &rest but not &key does not affect the set of
              ;  acceptable keyword arguments."
              (setq signatures (delete-if-not #'fourth signatures))
              ; Keine Methode mit &key -> keine Einschränkung der Argumente.
              (unless (null signatures)
                ; "If the lambda-list of any applicable method ... contains
                ;  &allow-other-keys, all keyword arguments are accepted."
                (unless (some #'sixth signatures)
                  ; "The set of keyword arguments accepted for a particular call
                  ;  is the union of the keyword arguments accepted by all
                  ;  applicable methods and the keyword arguments mentioned after
                  ;  &key in the generic function definition."
                  (let ((keywords
                          (remove-duplicates
                            (append (fourth signature) (mapcap #'fifth signatures))
                            :from-end t
                       )) )
                    (setq opt-vars (n-gensyms (second signature)))
                    (setq key-vars (n-gensyms (length keywords)))
                    (setq lambdalist-keypart
                          `(&KEY
                            ,@(mapcar #'(lambda (kw var) `((,kw ,var)))
                                      keywords key-vars
                              )
                           )
        ) ) ) ) ) ) )
        ; 2. Sort the applicable methods by precedence order:
        (setq methods (sort-applicable-methods methods req-args arg-order))
        ; 3. Apply method combination:
        ; Nur STANDARD Methoden-Kombination ist implementiert.
        ; Aufspalten in einzelne Methoden-Typen:
        (multiple-value-bind (primary-methods before-methods after-methods around-methods)
            (partition-method-list methods)
          (when (null primary-methods)
            (apply #'no-primary-method gf args)
            (go restart-compute)
          )
          ; Methoden zu einer "effektiven Methode" kombinieren:
          (labels ((ef-1 (primary-methods before-methods after-methods around-methods)
                     (if (null around-methods)
                       (ef-2 primary-methods before-methods after-methods)
                       (let* ((1method (first around-methods))
                              (1function (std-method-function 1method)))
                         (if (std-method-wants-next-method-p 1method)
                           (let ((next-ef
                                     (ef-1 primary-methods before-methods after-methods (rest around-methods))
                                ))
                             `(,apply-fun ',1function
                                          #'(LAMBDA ,lambdalist ,next-ef)
                                          ,@apply-args
                              )
                           )
                           `(,apply-fun ',1function ,@apply-args)
                   ) ) ) )
                   (ef-2 (primary-methods before-methods after-methods)
                     (let ((next-ef (ef-3 primary-methods after-methods)))
                       (if (null before-methods)
                         next-ef
                         `(PROGN
                            ,@(mapcar
                                #'(lambda (method)
                                    `(,apply-fun ',(std-method-function method)
                                                 ,@apply-args
                                     )
                                  )
                                before-methods ; most-specific-first
                              )
                            ,next-ef
                          )
                   ) ) )
                   (ef-3 (primary-methods after-methods)
                     (let ((next-ef (ef-4 primary-methods)))
                       (if (null after-methods)
                         next-ef
                         `(MULTIPLE-VALUE-PROG1
                            ,next-ef
                            ,@(mapcar
                                #'(lambda (method)
                                    `(,apply-fun ',(std-method-function method)
                                                 ,@apply-args
                                     )
                                  )
                                (reverse after-methods) ; most-specific-last
                              )
                          )
                   ) ) )
                   (ef-4 (primary-methods)
                     (let* ((1method (first primary-methods))
                            (1function (std-method-function 1method)))
                       (if (std-method-wants-next-method-p 1method)
                         (let ((next-ef-fun (ef-5 (rest primary-methods))))
                           `(,apply-fun ',1function ,next-ef-fun ,@apply-args)
                         )
                         `(,apply-fun ',1function ,@apply-args)
                   ) ) )
                   (ef-5 (primary-methods)
                     (if (null primary-methods)
                       'NIL ; keine Funktion, NEXT-METHOD-P reagiert darauf
                       `#'(LAMBDA ,lambdalist ,(ef-4 primary-methods))
                  )) )
            (let* ((ef-form (ef-1 primary-methods before-methods after-methods around-methods))
                   (ef-fun (if (and (eq (car ef-form) apply-fun)
                                    (equal (cddr ef-form) apply-args)
                                    (null lambdalist-keypart)
                               )
                             (cadr ef-form)
                             `#'(LAMBDA
                                  ,@(if (null opt-vars)
                                      `(,(append lambdalist lambdalist-keypart)
                                        ,@(if key-vars `((DECLARE (IGNORE ,@key-vars))))
                                       )
                                      `(,lambdalist
                                        (APPLY #'(LAMBDA (&OPTIONAL ,@opt-vars ,@lambdalist-keypart)
                                                   (DECLARE (IGNORE ,@opt-vars ,@key-vars))
                                                 )
                                               ,rest-var
                                       ))
                                    )
                                  ,ef-form
                                )
                  ))       )
              ; (eval ef-fun)                                 ; interpretiert
              ; (eval `(LOCALLY (DECLARE (COMPILE)) ,ef-fun)) ; compiliert
              (eval `(LET () (DECLARE (COMPILE) (INLINE FUNCALL APPLY)) ,ef-fun))
) ) ) ) ) ) )

; Runtime-Support für CALL-NEXT-METHOD.
(defun %call-next-method (method next-methods original-args new-args)
  (let* ((gf (std-method-gf method))
         (emf (sys::generic-function-effective-method-function gf))
         (original-em (apply emf original-args))
         (new-em (apply emf new-args)))
    (if (eq original-em new-em)
      (apply next-methods new-args)
      (error-of-type 'error
        (ENGLISH "~S in ~S: the new arguments ~S have a different effective method than the old arguments ~S")
        'call-next-method gf new-args original-args
) ) ) )


; Cruel hack (CLtL2 28.1.9.2., ANSI CL 7.1.2.):
; - MAKE-INSTANCE must be informed about the methods of ALLOCATE-INSTANCE,
;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
; - INITIALIZE-INSTANCE must be informed about the methods of
;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
; - REINITIALIZE-INSTANCE must be informed about the methods of
;   REINITIALIZE-INSTANCE and SHARED-INITIALIZE.
(defvar |#'allocate-instance| nil)
(defvar |#'initialize-instance| nil)
(defvar |#'reinitialize-instance| nil)
(defvar |#'shared-initialize| nil)
(defvar *gf-warn-on-replacing-method* t)

; Hinzufügen einer Methode zu einer generischen Funktion:
(defun std-add-method (gf method)
  ; 28.1.6.4. congruent lambda lists
  (let ((gf-sign (gf-signature gf))             ; (reqanz optanz restp keywords allowp)
        (m-sign (std-method-signature method))) ; (reqanz optanz restp keyp keywords allowp)
    (unless (= (first m-sign) (first gf-sign))
      (error-of-type 'error
        (ENGLISH "~S has ~S, but ~S has ~S required parameters")
        method (first m-sign) gf (first gf-sign)
    ) )
    (unless (= (second m-sign) (second gf-sign))
      (error-of-type 'error
        (ENGLISH "~S has ~S, but ~S has ~S optional parameters")
        method (second m-sign) gf (second gf-sign)
    ) )
    (when (and (third m-sign) (not (third gf-sign)))
      (error-of-type 'error
        (ENGLISH "~S has &REST or &KEY, but ~S hasn't.")
        method gf
    ) )
    (when (and (third gf-sign) (not (third m-sign)))
      (error-of-type 'error
        (ENGLISH "~S has &REST or &KEY, but ~S hasn't.")
        gf method
    ) )
    (when (fourth gf-sign) ; gf hat Keywords?
      ; ja -> Methode muss sie akzeptieren:
      (unless (if (fourth m-sign) ; Methode hat &key ?
                (or (sixth m-sign) ; Methode muss &allow-other-keys haben oder
                    (subsetp (fourth gf-sign) (fifth m-sign)) ; die Keywords aufzählen
                )
                (third m-sign) ; Methode muss &rest haben!
              )
        (error-of-type 'error
          (ENGLISH "~S doesn't accept the keywords ~S of ~S")
          method (fourth gf-sign) gf
    ) ) )
  )
  ; method kopieren, damit man gf eintragen kann:
  (when (std-method-wants-next-method-p method)
    (setq method (copy-standard-method method))
    (setf (std-method-function method) nil)
    (setf (std-method-gf method) gf)
  )
  ; function aus initfunction bestimmen:
  (when (null (std-method-function method))
    (let ((h (funcall (std-method-initfunction method) method)))
      (setf (std-method-function method) (car h))
      (when (car (cdr h)) ; konnte die Variable ",cont" wegoptimiert werden?
        (setf (std-method-wants-next-method-p method) nil)
  ) ) )
  ; Methode ist fertig. Eintragen:
  (warn-if-gf-already-called gf)
  (let ((old-method (find method (gf-methods gf) :test #'methods-agree-p)))
    (cond ((eq gf |#'allocate-instance|) (note-ai-change method))
          ((eq gf |#'initialize-instance|) (note-ii-change method))
          ((eq gf |#'reinitialize-instance|) (note-ri-change method))
          ((eq gf |#'shared-initialize|) (note-si-change method))
    )
    (setf (gf-methods gf)
          (cons method
                (if old-method
                  (progn
                    (when *gf-warn-on-replacing-method*
                      (warn (ENGLISH "Replacing method ~S in ~S")
                            old-method gf
                    ) )
                    (remove old-method (gf-methods gf))
                  )
                  (gf-methods gf)
    )     )     )
    (finalize-fast-gf gf)
  )
  gf
)

; Entfernen einer Methode von einer generischen Funktion:
(defun std-remove-method (gf method)
  (let ((old-method (find (std-method-initfunction method) (gf-methods gf)
                          :key #'std-method-initfunction)))
    (when old-method
      (warn-if-gf-already-called gf)
      (warn (ENGLISH "Removing method ~S in ~S")
            old-method gf
      )
      (cond ((eq gf |#'allocate-instance|) (note-ai-change method))
            ((eq gf |#'initialize-instance|) (note-ii-change method))
            ((eq gf |#'reinitialize-instance|) (note-ri-change method))
            ((eq gf |#'shared-initialize|) (note-si-change method))
      )
      (setf (gf-methods gf) (remove old-method (gf-methods gf)))
      (finalize-fast-gf gf)
  ) )
  gf
)

; Aufsuchen einer Methode in einer generischen Funktion:
(defun std-find-method (gf qualifiers specializers &optional (errorp t))
  ; sozusagen
  ;   (find hypothetical-method (gf-methods gf) :test #'methods-agree-p)
  ; vgl. methods-agree-p
  (dolist (method (gf-methods gf))
    (when (and (equal (std-method-qualifiers method) qualifiers)
               (specializers-agree-p (std-method-parameter-specializers method)
                                     specializers
          )    )
      (return-from std-find-method method)
  ) )
  (if errorp
    (error-of-type 'error
      (ENGLISH "~S has no method with qualifiers ~:S and specializers ~S")
      gf qualifiers specializers
    )
    nil
) )


;;; DEFMETHOD

(defmacro defmethod (funname &rest method-description &environment env)
  (unless (function-name-p funname)
    (error-of-type 'sys::source-program-error
      (ENGLISH "~S: the name of a function must be a symbol, not ~S")
      'defmethod funname
  ) )
  `(LET ()
     (EVAL-WHEN (COMPILE) (COMPILER::C-DEFUN ',funname))
     (DO-DEFMETHOD ',funname
       ,(analyze-method-description 'defmethod funname method-description env)
   ) )
)

(defun do-defmethod (funname method)
  (std-add-method
    (if (fboundp funname)
      (let ((gf (fdefinition funname)))
        (if (clos::generic-function-p gf)
          gf
          (error-of-type 'error
            (ENGLISH "~S doesn't name a generic function")
            funname
      ) ) )
      (setf (fdefinition funname)
            (let ((signature (std-method-signature method)))
              (make-fast-gf funname
                            ; GF-Signatur aus der Methoden-Signatur bestimmen:
                            (list (first signature) ; reqanz
                                  (second signature) ; optanz
                                  (third signature) ; restp
                                  '() ; keywords
                                  nil ; allowp
                            )
                            ; argorder := (0 ... reqanz-1)
                            (countup (first signature))
      )     ) )
    )
    method
  )
  method
)

; n --> Liste (0 ... n-1)
(defun countup (n)
  (do* ((count n (1- count))
        (l '() (cons count l)))
       ((eql count 0) l)
) )


;; Für DEFGENERIC, GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS,
;; WITH-ADDED-METHODS
  ; caller: Symbol
  ; funname: Funktionsname, Symbol oder (SETF symbol)
  ; lambdalist: Lambdaliste der generischen Funktion
  ; options: (option*)
  ; --> signature, argorder, method-forms, docstring
(defun analyze-defgeneric (caller funname lambdalist options env)
  (unless (function-name-p funname)
    (error-of-type 'sys::source-program-error
      (ENGLISH "~S: the name of a function must be a symbol, not ~S")
      caller funname lambdalist
  ) )
  ; Lambdaliste parsen:
  (multiple-value-bind (reqanz req-vars optanz restp keywords allowp)
      (analyze-defgeneric-lambdalist caller funname lambdalist)
    ; Optionen abarbeiten:
    (let ((method-forms '())
          (argorders nil)
          (docstrings nil))
      (dolist (option options)
        (unless (listp option)
          (error-of-type 'sys::source-program-error
            (ENGLISH "~S ~S: not a ~S option: ~S")
            caller funname 'defgeneric option
        ) )
        (case (first option)
          (DECLARE
            (unless (every
                       #'(lambda (x) (and (consp x) (eq (first x) 'OPTIMIZE)))
                       (rest option)
                    )
              (warn (ENGLISH "~S ~S: Only ~S declarations are permitted: ~S")
                    caller funname 'optimize option
            ) )
            ; Die Deklaration wird ignoriert.
            ; Der Compiler ignoriert sie sowieso.
          )
          (:ARGUMENT-PRECEDENCE-ORDER
            (when argorders
              (error-of-type 'sys::source-program-error
                (ENGLISH "~S ~S: ~S may only be specified once.")
                caller funname ':argument-precedence-order
            ) )
            (setq argorders option)
          )
          (:DOCUMENTATION
            (unless (and (eql (length option) 2) (stringp (second option)))
              (error-of-type 'sys::source-program-error
                (ENGLISH "~S ~S: A string must be specified after ~S : ~S")
                caller funname ':documentation option
            ) )
            (when docstrings
              (error-of-type 'sys::source-program-error
                (ENGLISH "~S ~S: Only one ~S string is allowed")
                caller funname ':documentation
            ) )
            (setq docstrings (rest option))
          )
          (:METHOD-COMBINATION
            (unless (equal (rest option) '(STANDARD))
              (error-of-type 'sys::source-program-error
                (ENGLISH "~S ~S: The only valid method combination is ~S : ~S")
                caller funname 'standard option
            ) )
            ; Die Methodenkombination wird ignoriert.
          )
          (:GENERIC-FUNCTION-CLASS
            (unless (equal (rest option) '(STANDARD-GENERIC-FUNCTION))
              (error-of-type 'sys::source-program-error
                (ENGLISH "~S ~S: The only valid generic function class name is ~S : ~S")
                caller funname 'standard-generic-function option
            ) )
            ; Die Klasse der generischen Funktion wird ignoriert.
          )
          (:METHOD-CLASS
            (unless (equal (rest option) '(STANDARD-METHOD))
              (error-of-type 'sys::source-program-error
                (ENGLISH "~S ~S: The only valid method class name is ~S : ~S")
                caller funname 'standard-method option
            ) )
            ; Die Klasse der Methoden wird ignoriert.
          )
          (:METHOD
            (push (analyze-method-description caller funname (rest option) env)
                  method-forms
          ) )
          (t (error-of-type 'sys::source-program-error
               (ENGLISH "~S ~S: invalid syntax in ~S option: ~S")
               caller funname 'defstruct option
      ) ) )  )
      ; :argument-precedence-order überprüfen:
      (let ((argorder
              (if argorders
                (let ((l (mapcar #'(lambda (x)
                                     (or (position x req-vars)
                                         (error-of-type 'sys::source-program-error
                                           (ENGLISH "~S ~S: ~S is not one of the required parameters: ~S")
                                           caller funname x argorders
                                   ) )   )
                                 (rest argorders)
                     ))  )
                  ; Ist (rest argorders) eine Permutation von req-vars ?
                  ; Anders ausgedrückt: Ist die Abbildung
                  ;        (rest argorders)  -->  req-vars
                  ; bzw.   l --> {0, ..., reqanz-1}
                  ; bijektiv?
                  (unless (apply #'/= l) ; injektiv?
                    (error-of-type 'sys::source-program-error
                      (ENGLISH "~S ~S: some variable occurs twice in ~S")
                      caller funname argorders
                  ) )
                  (unless (eql (length l) reqanz) ; surjektiv?
                    (error-of-type 'sys::source-program-error
                      (ENGLISH "~S ~S: ~S is missing some required parameter")
                      caller funname argorders
                  ) )
                  l
                )
                (countup reqanz)
           )) )
        (values ; Signatur
                `(,reqanz ,optanz ,restp ,keywords ,allowp)
                ; argorder
                argorder
                ; Liste der Methoden-Formen
                (nreverse method-forms)
                ; docstring oder nil
                (car docstrings)
        )
) ) ) )

; Lambdaliste parsen:
; lambdalist --> reqanz, req-vars, optanz, restp, keywords, allowp
(defun analyze-defgeneric-lambdalist (caller funname lambdalist)
  (let ((req-vars '())
        (optanz 0)
        (restp nil)
        (keyp nil)
        (keywords '())
        (allowp nil))
    (when (some #'(lambda (item) (and (consp item) (cdr item))) lambdalist)
      (error-of-type 'sys::source-program-error
        (ENGLISH "~S ~S: No initializations are allowed in a generic function lambda-list: ~S")
        caller funname lambdalist
    ) )
    (flet ((check-varname (var)
             (unless (symbolp var)
               (error-of-type 'sys::source-program-error
                 (ENGLISH "~S ~S: variable name ~S should be a symbol")
                 caller funname var
             ) )
             (when (member var req-vars :test #'eq)
               (error-of-type 'sys::source-program-error
                 (ENGLISH "~S ~S: duplicate variable name ~S")
                 caller funname var
             ) )
             var
          ))
      (loop
        (when (or (atom lambdalist) (lambda-list-keyword-p (first lambdalist)))
          (return)
        )
        (push (check-varname (pop lambdalist)) req-vars)
      )
      (when (and (consp lambdalist) (eq (first lambdalist) '&optional))
        (pop lambdalist)
        (loop
          (when (or (atom lambdalist) (lambda-list-keyword-p (first lambdalist)))
            (return)
          )
          (let ((item (pop lambdalist)))
            (check-varname (if (consp item) (first item) item))
            (incf optanz)
      ) ) )
      (when (and (consp lambdalist) (eq (first lambdalist) '&rest)
                 (consp (rest lambdalist))
            )
        (pop lambdalist)
        (check-varname (pop lambdalist))
        (setq restp t)
      )
      (when (and (consp lambdalist) (eq (first lambdalist) '&key))
        (pop lambdalist)
        (setq restp t) ; &key impliziert &rest
        (loop
          (when (or (atom lambdalist) (lambda-list-keyword-p (first lambdalist)))
            (return)
          )
          (let ((item (pop lambdalist)))
            (when (consp item) (setq item (first item)))
            (check-varname (if (consp item) (second item) item))
            (push (if (consp item)
                    (first item)
                    (intern (symbol-name item) *keyword-package*)
                  )
                  keywords
        ) ) )
        (when (and (consp lambdalist) (eq (first lambdalist) '&allow-other-keys))
          (pop lambdalist)
          (setq allowp t)
      ) )
    )
    (when lambdalist
      (error-of-type 'sys::source-program-error
        (ENGLISH "~S ~S: invalid lambda list portion: ~S")
        caller funname lambdalist
    ) )
    (values (length req-vars) (nreverse req-vars) optanz
            (or restp keyp) keywords allowp
) ) )

; Lambdaliste in Aufrufkonvention umrechnen:
(defun defgeneric-lambdalist-callinfo (caller funname lambdalist)
  (multiple-value-bind (reqanz req-vars optanz restp keywords allowp)
      (analyze-defgeneric-lambdalist caller funname lambdalist)
    (declare (ignore req-vars))
    (callinfo reqanz optanz restp keywords allowp)
) )


;;; DEFGENERIC

(defmacro defgeneric (funname lambda-list &rest options &environment env)
  (multiple-value-bind (signature argorder method-forms docstring)
      (analyze-defgeneric 'defgeneric funname lambda-list options env)
    `(LET ()
       (EVAL-WHEN (COMPILE) (COMPILER::C-DEFUN ',funname))
       ; NB: Kein (SYSTEM::REMOVE-OLD-DEFINITIONS ',funname)
       ,@(if docstring
           (let ((symbolform
                   (if (atom funname)
                     `',funname
                     `(LOAD-TIME-VALUE (SYSTEM::GET-SETF-SYMBOL ',(second funname)))
                )) )
             `((SYSTEM::%SET-DOCUMENTATION ,symbolform 'FUNCTION ',docstring))
         ) )
       (DO-DEFGENERIC ',funname ',signature ',argorder ,@method-forms)
     )
) )

(defun make-generic-function (funname signature argorder &rest methods)
  (let ((gf (make-fast-gf funname signature argorder)))
    (dolist (method methods) (std-add-method gf method))
    (finalize-fast-gf gf)
    gf
) )

(defvar *gf-warn-on-removing-all-methods* t)

(defun do-defgeneric (funname signature argorder &rest methods)
  (if (fboundp funname)
    (let ((gf (fdefinition funname)))
      (if (clos::generic-function-p gf)
        ; Umdefinition einer generischen Funktion
        (progn
          (warn-if-gf-already-called gf)
          (when (and *gf-warn-on-removing-all-methods* (gf-methods gf))
            (warn (ENGLISH "Removing all methods of ~S")
                  gf
            )
            (setf (gf-methods gf) nil)
          )
          (unless (and (equal signature (gf-signature gf))
                       (equal argorder (gf-argorder gf))
                  )
            (warn (ENGLISH "Modifying the parameter profile of ~S")
                  gf
            )
            (setf (gf-signature gf) signature)
            (setf (gf-argorder gf) argorder)
          )
          (dolist (method methods) (std-add-method gf method))
          (finalize-fast-gf gf)
          gf
        )
        (error-of-type 'program-error
          (ENGLISH "~S doesn't name a generic function")
          funname
    ) ) )
    (setf (fdefinition funname)
          (apply #'make-generic-function funname signature argorder methods)
) ) )


#|
;; Für GENERIC-FLET, GENERIC-LABELS

; Wie make-generic-function, nur dass der Dispatch-Code gleich installiert wird.
(defun make-generic-function-now (funname signature argorder &rest methods)
  (let ((gf (make-fast-gf funname signature argorder)))
    (dolist (method methods) (std-add-method gf method))
    (install-dispatch gf)
    gf
) )
|#


;; Für GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS

(defun make-generic-function-form (caller funname lambda-list options env)
  (multiple-value-bind (signature argorder method-forms docstring)
      (analyze-defgeneric caller funname lambda-list options env)
    (declare (ignore docstring))
    `(MAKE-GENERIC-FUNCTION ',funname ',signature ',argorder ,@method-forms)
) )


;;; GENERIC-FUNCTION

(defmacro generic-function (lambda-list &rest options &environment env)
  (make-generic-function-form 'generic-function 'LAMBDA lambda-list options env)
)


;; Für GENERIC-FLET, GENERIC-LABELS
(defun analyze-generic-fundefs (caller fundefs env)
  (let ((names '())
        (funforms '()))
    (dolist (fundef fundefs)
      (unless (and (consp fundef) (consp (cdr fundef)))
        (error-of-type 'sys::source-program-error
          (ENGLISH "~S: ~S is not a generic function specification")
          caller fundef
      ) )
      (push (first fundef) names)
      (push (make-generic-function-form caller (first fundef) (second fundef) (cddr fundef) env) funforms)
    )
    (values (nreverse names) (nreverse funforms))
) )


;;; GENERIC-FLET

(defmacro generic-flet (fundefs &body body &environment env)
  (multiple-value-bind (funnames funforms)
      (analyze-generic-fundefs 'generic-flet fundefs env)
    (let ((varnames (n-gensyms (length funnames))))
      `(LET ,(mapcar #'list varnames funforms)
         (FLET ,(mapcar #'(lambda (varname funname)
                            `(,funname (&rest args) (apply ,varname args))
                          )
                        varnames funnames
                )
           ,@body
       ) )
) ) )


;;; GENERIC-LABELS

(defmacro generic-labels (fundefs &body body &environment env)
  (multiple-value-bind (funnames funforms)
      (analyze-generic-fundefs 'generic-labels fundefs env)
    (let ((varnames (n-gensyms (length funnames))))
      `(LET ,varnames
         (FLET ,(mapcar #'(lambda (varname funname)
                            `(,funname (&rest args) (apply ,varname args))
                          )
                        varnames funnames
                )
           ,@(mapcar #'(lambda (varname funform) `(SETQ ,varname ,funform))
                     varnames funforms
             )
           ,@body
       ) )
) ) )


;;; WITH-ADDED-METHODS
; ist vermurkst und wird deshalb nicht implementiert.


;;; Verschiedene generische Funktionen, die wir bis jetzt hinausgezögert haben:

(defgeneric class-name (class)
  (:method ((class class))
    (class-classname class)
) )

(defgeneric (setf class-name) (new-value class)
  (:method (new-value (class class))
    (unless (symbolp new-value)
      (error-of-type 'type-error
        :datum new-value :expected-type 'symbol
        (ENGLISH "~S: The name of a class must be a symbol, not ~S")
        '(setf class-name) new-value
    ) )
    (when (built-in-class-p class)
      (error-of-type 'error
        (ENGLISH "~S: The name of the built-in class ~S cannot be modified")
        '(setf class-name) class
    ) )
    (setf (class-classname class) new-value)
) )

; An argument is called "dispatching" if not all the corresponding parameter
; specializers are <t>.
(defun dispatching-arg-p (index methods)
  (notevery #'(lambda (method)
                (eq (nth index (std-method-parameter-specializers method)) <t>)
              )
            methods
) )
(defun single-dispatching-arg (reqanz methods)
  (let ((first-dispatching-arg
          (dotimes (i reqanz nil) (when (dispatching-arg-p i methods) (return i)))
       ))
    (and first-dispatching-arg
         (do ((i (1+ first-dispatching-arg) (1+ i)))
             ((>= i reqanz) first-dispatching-arg)
           (when (dispatching-arg-p i methods) (return nil))
) ) )    )
(defun dispatching-arg-type (index methods)
  `(OR ,@(remove-duplicates
           (mapcar #'(lambda (method)
                       (nth index (std-method-parameter-specializers method))
                     )
                   methods
           )
           :test #'same-specializers-p
   )     )
)

(defgeneric no-applicable-method (gf &rest args)
  (:method ((gf t) &rest args)
    (let* ((reqanz (first (gf-signature gf)))
           (methods (gf-methods gf))
           (dispatching-arg (single-dispatching-arg reqanz methods)))
      (if dispatching-arg
        (error-of-type 'type-error
          :datum (first args)
          :expected-type (dispatching-arg-type dispatching-arg methods)
          (ENGLISH "~S: When calling ~S with arguments ~S, no method is applicable.")
          'no-applicable-method gf args
        )
        (error-of-type 'error
          (ENGLISH "~S: When calling ~S with arguments ~S, no method is applicable.")
          'no-applicable-method gf args
) ) ) ) )

(defgeneric no-primary-method (gf &rest args)
  (:method ((gf t) &rest args)
    (let* ((reqanz (first (gf-signature gf)))
           (methods (mapcan #'(lambda (method)
                                (when (equal (std-method-qualifiers method) '())
                                  (list method)
                              ) )
                            (gf-methods gf)
           )        )
           (dispatching-arg (single-dispatching-arg reqanz methods)))
      (if dispatching-arg
        (error-of-type 'type-error
          :datum (first args)
          :expected-type (dispatching-arg-type dispatching-arg methods)
          (ENGLISH "~S: When calling ~S with arguments ~S, no primary method is applicable.")
          'no-primary-method gf args
        )
        (error-of-type 'error
          (ENGLISH "~S: When calling ~S with arguments ~S, no primary method is applicable.")
          'no-primary-method gf args
) ) ) ) )

(defun %no-next-method (method &rest args)
  (apply #'no-next-method (std-method-gf method) method args)
)
(defgeneric no-next-method (gf method &rest args)
  (:method ((gf standard-generic-function) (method standard-method) &rest args)
    (error-of-type 'error
      (ENGLISH "~S: When calling ~S with arguments ~S, there is no next method after ~S, and ~S was called.")
      'no-next-method gf args method '(call-next-method)
) ) )

(defgeneric find-method (gf qualifiers specializers &optional errorp)
  (:method ((gf standard-generic-function) qualifiers specializers &optional (errorp t))
     (std-find-method gf qualifiers specializers errorp)
) )

(defgeneric add-method (gf method)
  (:method ((gf standard-generic-function) (method standard-method))
    (std-add-method gf method)
) )

(defgeneric remove-method (gf method)
  (:method ((gf standard-generic-function) (method standard-method))
    (std-remove-method gf method)
) )

(defgeneric compute-applicable-methods (gf args)
  (:method ((gf standard-generic-function) args)
    (let ((reqanz (first (gf-signature gf)))
          (methods (gf-methods gf)))
      (if (>= (length args) reqanz)
        (let ((req-args (subseq args 0 reqanz)))
          ; 1. Select the applicable methods:
          (setq methods
            (remove-if-not
              #'(lambda (method) (method-applicable-p method req-args))
              methods
          ) )
          ; 2. Sort the applicable methods by precedence order:
          (sort-applicable-methods methods req-args (gf-argorder gf))
        )
        nil ; lieber kein Error
) ) ) )

(defgeneric method-qualifiers (method)
  (:method ((method standard-method))
    (std-method-qualifiers method)
) )

(defgeneric function-keywords (method)
  (:method ((method standard-method))
    (values-list (cddddr (std-method-signature method)))
) )

(defgeneric slot-missing (class instance slot-name operation &optional new-value)
  (:method ((class t) instance slot-name operation &optional new-value)
    (declare (ignore instance new-value))
    (error-of-type 'error
      (ENGLISH "~S: The class ~S has no slot named ~S")
      operation class slot-name
) ) )

(defgeneric slot-unbound (class instance slot-name)
  (:method ((class t) instance slot-name)
    (declare (ignore class))
    (error-of-type 'unbound-slot
      :name slot-name
      :instance instance
      (ENGLISH "~S: The slot ~S of ~S has no value")
      'slot-value slot-name instance
) ) )

(defgeneric print-object (object stream)
  (:method ((object standard-object) stream)
    (print-unreadable-object (object stream :type t :identity t))
  )
  (:method ((object structure-object) stream)
    (system::print-structure object stream)
  )
  (:method ((object class) stream)
    (print-class object stream)
  )
  (:method ((object standard-method) stream)
    (print-std-method object stream)
) )

; Noch ein DEFSTRUCT-Hook
(defun defstruct-remove-print-object-method (name)
  (let ((method (find-method #'print-object nil (list (find-class name) <t>) nil)))
    (when method (remove-method #'print-object method))
) )

;; 28.1.9. Object creation and initialization

; Cruel hack (CLtL2 28.1.9.2., ANSI CL 7.1.2.):
; - MAKE-INSTANCE must be informed about the methods of ALLOCATE-INSTANCE,
;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
; - INITIALIZE-INSTANCE must be informed about the methods of
;   INITIALIZE-INSTANCE and SHARED-INITIALIZE.
; - REINITIALIZE-INSTANCE must be informed about the methods of
;   REINITIALIZE-INSTANCE and SHARED-INITIALIZE.

(defparameter *make-instance-table* (make-hash-table :test #'eq))
  ; Hash table, mapping a class to a simple-vector containing
  ; - a list of valid keyword arguments,
  ; - the effective method of allocate-instance,
  ; - the effective method of initialize-instance,
  ; - the effective method of shared-initialize.

(defparameter *reinitialize-instance-table* (make-hash-table :test #'eq))
  ; Hash table, mapping a class to a cons containing
  ; - a list of valid keyword arguments,
  ; - the effective method of shared-initialize.

(defun note-i-change (specializer table)
  (maphash #'(lambda (class value) (declare (ignore value))
               (when (subclassp class specializer)
                 (remhash class table)
             ) )
           table
) )
(defun note-i-meta-change (meta-specializer table)
  (maphash #'(lambda (class value) (declare (ignore value))
               (when (subclassp (class-of class) meta-specializer) ; <==> (typep class meta-specializer)
                 (remhash class table)
             ) )
           table
) )

(defun note-ai-change (method)
  (let ((specializer (first (std-method-parameter-specializers method))))
    (if (consp specializer)
      ; EQL-Methode auf ALLOCATE-INSTANCE:
      ; Objekt muss Klasse sein, sonst wertlos
      (let ((specialized-object (second specializer)))
        (when (class-p specialized-object)
          ; Entferne die Einträge von *make-instance-table*, für welche die
          ; besagte Methode anwendbar wäre:
          (note-i-change specialized-object *make-instance-table*)
      ) )
      ; Entferne die Einträge von *make-instance-table*, für welche die
      ; besagte Methode anwendbar wäre:
      (note-i-meta-change specializer *make-instance-table*)
) ) )

(defun note-ii-change (method)
  (let ((specializer (first (std-method-parameter-specializers method))))
    ; EQL-Methoden auf INITIALIZE-INSTANCE sind eh wertlos
    (unless (consp specializer)
      ; Entferne die Einträge von *make-instance-table*, für welche die
      ; besagte Methode anwendbar wäre:
      (note-i-change specializer *make-instance-table*)
) ) )

(defun note-ri-change (method)
  (let ((specializer (first (std-method-parameter-specializers method))))
    ; EQL-Methoden auf REINITIALIZE-INSTANCE sind im wesentlichen wertlos
    (unless (consp specializer)
      ; Entferne die Einträge von *reinitialize-instance-table*, für welche die
      ; besagte Methode anwendbar wäre:
      (note-i-change specializer *reinitialize-instance-table*)
) ) )

(defun note-si-change (method)
  (let* ((specializers (std-method-parameter-specializers method))
         (specializer1 (first specializers))
         (specializer2 (second specializers)))
    ; EQL-Methoden auf SHARED-INITIALIZE sind im wesentlichen wertlos
    (unless (consp specializer1)
      ; Als zweites Argument wird von INITIALIZE-INSTANCE immer nur T übergeben.
      (when (typep 'T specializer2)
        ; Entferne die Einträge von *make-instance-table*, für welche die
        ; besagte Methode anwendbar wäre:
        (note-i-change specializer1 *make-instance-table*)
      )
      ; Als zweites Argument wird von REINITIALIZE-INSTANCE nur NIL übergeben.
      (when (typep 'NIL specializer2)
        ; Entferne die Einträge von *reinitialize-instance-table*, für welche die
        ; besagte Methode anwendbar wäre:
        (note-i-change specializer1 *reinitialize-instance-table*)
      )
) ) )

; Aus einer Liste von anwendbaren Methoden alle Keywords sammeln:
(defun valid-initarg-keywords (class methods)
  (let ((signatures (mapcar #'std-method-signature methods)))
    ; "A method that has &rest but not &key does not affect the set of
    ;  acceptable keyword srguments."
    (setq signatures (delete-if-not #'fourth signatures))
    ; "The presence of &allow-other-keys in the lambda list of an applicable
    ;  method disables validity checking of initialization arguments."
    ; (ANSI CL section 7.1.2)
    (if (some #'sixth signatures)
      't
      ; "The keyword name of each keyword parameter specified in the method's
      ;  lambda-list becomes an initialization argument for all classes for
      ;  which the method is applicable."
      (remove-duplicates
        (append (class-valid-initargs class) (mapcap #'fifth signatures))
        :from-end t
) ) ) )

; NB: Beim Berechnen einer effektiven Methode kommt es auf die restlichen
; Argumente nicht an.
; Beim ersten INITIALIZE-INSTANCE- oder MAKE-INSTANCE-Aufruf einer jeden Klasse
; merkt man sich die benötigte Information in *make-instance-table*.

; Bei MAKE-INSTANCE sind als Keys gültig:
; - die Initargs, die zur Initialisierung von Slots benutzt werden,
; - die Keywords von Methoden von SHARED-INITIALIZE,
; - die Keywords von Methoden von INITIALIZE-INSTANCE,
; - die Keywords von Methoden von ALLOCATE-INSTANCE.
(defun valid-make-instance-keywords (class)
  (valid-initarg-keywords
    class
    (append
      ; Liste aller anwendbaren Methoden von SHARED-INITIALIZE
      (remove-if-not
        #'(lambda (method)
            (let* ((specializers (std-method-parameter-specializers method))
                   (specializer1 (first specializers))
                   (specializer2 (second specializers)))
              (and (atom specializer1) (subclassp class specializer1)
                   (typep 'T specializer2)
          ) ) )
        (gf-methods |#'shared-initialize|)
      )
      ; Liste aller anwendbaren Methoden von INITIALIZE-INSTANCE
      (remove-if-not
        #'(lambda (method)
            (let ((specializer (first (std-method-parameter-specializers method))))
              (and (atom specializer) (subclassp class specializer))
          ) )
        (gf-methods |#'initialize-instance|)
      )
      ; Liste aller anwendbaren Methoden von ALLOCATE-INSTANCE
      (remove-if-not
        #'(lambda (method)
            (let ((specializer (first (std-method-parameter-specializers method))))
              (if (consp specializer)
                (eql class (second specializer))
                (subclassp (class-of class) specializer) ; <==> (typep class specializer)
          ) ) )
        (gf-methods |#'allocate-instance|)
      )
) ) )
(defun make-instance-table-entry1 (class)
  (values (valid-make-instance-keywords class)
          (compute-effective-method |#'allocate-instance| class)
) )
(defun make-instance-table-entry2 (instance)
  (values (compute-effective-method |#'initialize-instance| instance)
          (compute-effective-method |#'shared-initialize| instance 'T)
) )

; 28.1.9.5., 28.1.9.4.
(defgeneric shared-initialize (instance slot-names &rest initargs))
(setq |#'shared-initialize| #'shared-initialize)
#|
(defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
  (dolist (slot (class-slots (class-of instance)))
    (let ((slotname (slotdef-name slot)))
      (multiple-value-bind (init-key init-value foundp)
          (get-properties initargs (slotdef-initargs slot))
        (declare (ignore init-key))
        (if foundp
          (setf (slot-value instance slotname) init-value)
          (unless (slot-boundp instance slotname)
            (let ((init (slotdef-initer slot)))
              (when init
                (when (or (eq slot-names 'T) (member slotname slot-names :test #'eq))
                  (setf (slot-value instance slotname)
                        (if (car init) (funcall (car init)) (cdr init))
  ) ) ) ) ) ) ) ) )
  instance
)
|#
; die Haupt-Arbeit erledigt ein SUBR:
(do-defmethod 'shared-initialize
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%shared-initialize '(T))
                    )
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-object) (find-class 't))
    :qualifiers '()
    :signature '(2 0 t () () ())
) )
(do-defmethod 'shared-initialize
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%shared-initialize '(T))
                    )
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'structure-object) (find-class 't))
    :qualifiers '()
    :signature '(2 0 t () () ())
) )

; 28.1.12.
(defgeneric reinitialize-instance (instance &rest initargs))
(setq |#'reinitialize-instance| #'reinitialize-instance)
#|
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance 'NIL initargs)
)
|#
#|
; optimiert:
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (let ((h (gethash (class-of instance) *reinitialize-instance-table*)))
    (if h
      (progn
        ; 28.1.9.2. validity of initialization arguments
        (let ((valid-keywords (car h)))
          (unless (eq valid-keyword 't)
            (sys::keyword-test initargs valid-keywords)
        ) )
        (if (not (eq (cdr h) #'clos::%shared-initialize))
          ; effektive Methode von shared-initialize anwenden:
          (apply (cdr h) instance 'NIL initargs)
          ; clos::%shared-initialize mit slot-names=NIL lässt sich vereinfachen:
          (progn
            (dolist (slot (class-slots (class-of instance)))
              (let ((slotname (slotdef-name slot)))
                (multiple-value-bind (init-key init-value foundp)
                    (get-properties initargs (slotdef-initargs slot))
                  (declare (ignore init-key))
                  (if foundp
                    (setf (slot-value instance slotname) init-value)
            ) ) ) )
            instance
      ) ) )
      (apply #'initial-reinitialize-instance instance initargs)
) ) )
|#
; die Haupt-Arbeit erledigt ein SUBR:
(do-defmethod 'reinitialize-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%reinitialize-instance '(T))
                    )
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-object))
    :qualifiers '()
    :signature '(1 0 t () () ())
) )
(do-defmethod 'reinitialize-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%reinitialize-instance '(T))
                    )
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'structure-object))
    :qualifiers '()
    :signature '(1 0 t () () ())
) )
; Beim ersten REINITIALIZE-INSTANCE-Aufruf einer jeden Klasse merkt man sich die
; benötigte Information in *reinitialize-instance-table*.
(defun initial-reinitialize-instance (instance &rest initargs)
  (let* ((class (class-of instance))
         (valid-keywords
           (valid-initarg-keywords
             class
             ; Liste aller anwendbaren Methoden von SHARED-INITIALIZE
             (remove-if-not
               #'(lambda (method)
                   (let* ((specializers (std-method-parameter-specializers method))
                          (specializer1 (first specializers))
                          (specializer2 (second specializers)))
                     (and (atom specializer1) (subclassp class specializer1)
                          (typep 'NIL specializer2)
                 ) ) )
               (gf-methods |#'shared-initialize|)
        )) ) )
    ; 28.1.9.2. validity of initialization arguments
    (unless (eq valid-keywords 't)
      (sys::keyword-test initargs valid-keywords)
    )
    (let ((si-ef (compute-effective-method |#'shared-initialize| instance 'NIL)))
      (setf (gethash class *reinitialize-instance-table*) (cons valid-keywords si-ef))
      (apply si-ef instance 'NIL initargs)
) ) )

; 28.1.9.6.
(defgeneric initialize-instance (instance &rest initargs))
(setq |#'initialize-instance| #'initialize-instance)
#|
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance 'T initargs)
)
|#
#|
; optimiert:
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (let ((h (gethash class *make-instance-table*)))
    (if h
      (if (not (eq (svref h 3) #'clos::%shared-initialize))
        ; effektive Methode von shared-initialize anwenden:
        (apply (svref h 3) instance 'T initargs)
        ; clos::%shared-initialize mit slot-names=T lässt sich vereinfachen:
        (progn
          (dolist (slot (class-slots (class-of instance)))
            (let ((slotname (slotdef-name slot)))
              (multiple-value-bind (init-key init-value foundp)
                  (get-properties initargs (slotdef-initargs slot))
                (declare (ignore init-key))
                (if foundp
                  (setf (slot-value instance slotname) init-value)
                  (unless (slot-boundp instance slotname)
                    (let ((init (slotdef-initer slot)))
                      (when init
                        (setf (slot-value instance slotname)
                              (if (car init) (funcall (car init)) (cdr init))
          ) ) ) ) ) ) ) )
          instance
      ) )
      (apply #'initial-initialize-instance instance initargs)
) ) )
|#
; die Haupt-Arbeit erledigt ein SUBR:
(do-defmethod 'initialize-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%initialize-instance '(T))
                    )
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-object))
    :qualifiers '()
    :signature '(1 0 t () () ())
) )
(do-defmethod 'initialize-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%initialize-instance '(T))
                    )
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'structure-object))
    :qualifiers '()
    :signature '(1 0 t () () ())
) )
(defun initial-initialize-instance (instance &rest initargs)
  (let ((class (class-of instance)))
    (multiple-value-bind (valid-keywords ai-ef) (make-instance-table-entry1 class)
      (multiple-value-bind (ii-ef si-ef) (make-instance-table-entry2 instance)
        (setf (gethash class *make-instance-table*) (vector valid-keywords ai-ef ii-ef si-ef))
        ; effektive Methode von SHARED-INITIALIZE anwenden:
        (apply si-ef instance 'T initargs)
) ) ) )

; User-defined methods on allocate-instance are now supported.
(defgeneric allocate-instance (instance &rest initargs))
(setq |#'allocate-instance| #'allocate-instance)
#|
(defgeneric allocate-instance (class)
  (:method ((class standard-class))
    (allocate-std-instance class (class-instance-size class))
  )
  (:method ((class structure-class))
    (sys::%make-structure (class-names class) (class-instance-size class) :initial-element unbound)
) )
|#
#|
(defun %allocate-instance (class &rest initargs)
  (declare (ignore initargs))
  ; Quick and dirty dispatch among <standard-class> and <structure-class>.
  ; (class-shared-slots class) is a simple-vector, (class-names class) a cons.
  (if (atom (class-shared-slots class))
    (allocate-std-instance class (class-instance-size class))
    (sys::%make-structure (class-names class) (class-instance-size class) :initial-element unbound)
) )
|#
; die Haupt-Arbeit erledigt ein SUBR:
(do-defmethod 'allocate-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%allocate-instance '(T))
                    )
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-class))
    :qualifiers '()
    :signature '(1 0 t () () ())
) )
(do-defmethod 'allocate-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%allocate-instance '(T))
                    )
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'structure-class))
    :qualifiers '()
    :signature '(1 0 t () () ())
) )

; 28.1.9.7.
(defgeneric make-instance (class &rest initargs)
  (:method ((class symbol) &rest initargs)
    (apply #'make-instance (find-class class) initargs)
  )
)
#|
(defmethod make-instance ((class standard-class) &rest initargs)
  ; 28.1.9.3., 28.1.9.4. default-initargs zur Kenntnis nehmen:
  (dolist (default-initarg (class-default-initargs class))
    (let ((nothing default-initarg))
      (when (eq (getf initargs (car default-initarg) nothing) nothing)
        (setq initargs
              (append initargs
                (list (car default-initarg)
                      (let ((init (cdr default-initarg)))
                        (if (car init) (funcall (car init)) (cdr init))
  ) ) ) )     ) )     )
  #|
  ; 28.1.9.2. validity of initialization arguments
  (sys::keyword-test initargs
                     (union (class-valid-initargs class)
                            (applicable-keywords #'initialize-instance class) ; ??
  )                  )
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
  )
  |#
  (let ((h (gethash class *make-instance-table*)))
    (if h
      (progn
        ; 28.1.9.2. validity of initialization arguments
        (let ((valid-keywords (svref h 0)))
          (unless (eq valid-keywords 't)
            (sys::keyword-test initargs valid-keywords)
        ) )
        (let ((instance (apply #'allocate-instance class initargs)))
          (if (not (eq (svref h 2) #'clos::%initialize-instance))
            ; effektive Methode von initialize-instance anwenden:
            (apply (svref h 2) instance initargs)
            ; clos::%initialize-instance lässt sich vereinfachen (man braucht
            ; nicht nochmal in *make-instance-table* nachzusehen):
            (if (not (eq (svref h 3) #'clos::%shared-initialize))
              ; effektive Methode von shared-initialize anwenden:
              (apply (svref h 3) instance 'T initargs)
              ...
            )
      ) ) )
      (apply #'initial-make-instance class initargs)
) ) )
|#
; die Haupt-Arbeit erledigt ein SUBR:
(do-defmethod 'make-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%make-instance '(T))
                    )
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'standard-class))
    :qualifiers '()
    :signature '(1 0 t () () ())
) )
(do-defmethod 'make-instance
  (make-standard-method
    :initfunction #'(lambda (gf) (declare (ignore gf))
                      (cons #'clos::%make-instance '(T))
                    )
    :wants-next-method-p nil
    :parameter-specializers (list (find-class 'structure-class))
    :qualifiers '()
    :signature '(1 0 t () () ())
) )
(defun initial-make-instance (class &rest initargs)
  (multiple-value-bind (valid-keywords ai-ef) (make-instance-table-entry1 class)
    ; 28.1.9.2. validity of initialization arguments
    (unless (eq valid-keywords 't)
      (sys::keyword-test initargs valid-keywords)
    )
    ; effektive Methode von ALLOCATE-INSTANCE anwenden:
    (let ((instance (apply ai-ef class initargs)))
      (unless (eq (class-of instance) class)
        (error-of-type 'error
          (ENGLISH "~S method for ~S returned ~S")
          'allocate-instance class instance
      ) )
      (multiple-value-bind (ii-ef si-ef) (make-instance-table-entry2 instance)
        (setf (gethash class *make-instance-table*) (vector valid-keywords ai-ef ii-ef si-ef))
        ; effektive Methode von INITIALIZE-INSTANCE anwenden:
        (apply ii-ef instance initargs)
) ) ) )


;; Users want to be able to create instances of subclasses of <standard-class>
;; and <structure-class>. So, when creating a class, we now go through
;; MAKE-INSTANCE and INITIALIZE-INSTANCE.
(defun make-instance-standard-class (&rest args)
  (apply #'make-instance args)
)
(defun make-instance-structure-class (&rest args)
  (apply #'make-instance args)
)
(defmethod initialize-instance ((new-class-object standard-class) &rest args
                  &key name (metaclass <standard-class>) documentation
                       direct-superclasses direct-slots
                       direct-default-initargs
                               )
  (declare (ignore documentation direct-superclasses direct-slots direct-default-initargs))
  (setf (class-classname new-class-object) name)
  (setf (class-metaclass new-class-object) metaclass) ; = (class-of new-class-object)
  (apply #'initialize-instance-standard-class new-class-object args)
  (call-next-method)
  new-class-object
)
(defmethod initialize-instance ((new-class-object structure-class) &rest args
                  &key name (metaclass <structure-class>) documentation
                       direct-superclasses direct-slots
                       direct-default-initargs
                       names slots size
                               )
  (declare (ignore documentation direct-superclasses direct-slots direct-default-initargs names slots size))
  (setf (class-classname new-class-object) name)
  (setf (class-metaclass new-class-object) metaclass) ; = (class-of new-class-object)
  (apply #'initialize-instance-structure-class new-class-object args)
  (call-next-method)
  new-class-object
)


;;; Utility functions

;; Returns the slot names of an instance of a slotted-class
;; (i.e. of a structure-object or standard-object).
(defun slot-names (object)
  (mapcar #'slotdef-name (class-slots (class-of object))))

