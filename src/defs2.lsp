;;; CLtL2-kompatible Definitionen
;;; Bruno Haible 21.7.1994

;===============================================================================

(in-package "LISP")
(export '(nth-value function-lambda-expression defpackage define-symbol-macro
          print-unreadable-object declaim destructuring-bind complement
          constantly with-standard-io-syntax with-hash-table-iterator
          read-sequence write-sequence designator
)        )
(in-package "SYSTEM")

;-------------------------------------------------------------------------------

;; X3J13 vote <123>

;; Macro (nth-value n form) == (nth n (multiple-value-list form)), CLtL2 S. 184
(defmacro nth-value (n form)
  (if (and (integerp n) (>= n 0))
    (if (< n (1- multiple-values-limit))
      (if (= n 0)
        `(PROG1 ,form)
        (let ((resultvar (gensym)))
          (do ((vars (list resultvar))
               (ignores nil)
               (i n (1- i)))
              ((zerop i)
               `(MULTIPLE-VALUE-BIND ,vars ,form
                  (DECLARE (IGNORE ,@ignores))
                  ,resultvar
              ) )
            (let ((g (gensym))) (push g vars) (push g ignores))
      ) ) )
      `(PROGN ,form NIL)
    )
    `(NTH ,n (MULTIPLE-VALUE-LIST ,form))
) )

;-------------------------------------------------------------------------------

;; X3J13 vote <88>

;; Interpretierte Funktion in Lambda-Ausdruck umwandeln, CLtL2 S. 682
(defun function-lambda-expression (obj)
  (cond ((and (compiled-function-p obj) (functionp obj)) ; SUBR oder compilierte Closure?
         (values nil t nil)
        )
        ((sys::closurep obj) ; interpretierte Closure?
         (values (cons 'LAMBDA (sys::%record-ref obj 1)) ; Lambda-Ausdruck ohne Docstring
                 (vector ; Environment
                         (sys::%record-ref obj 4) ; venv
                         (sys::%record-ref obj 5) ; fenv
                         (sys::%record-ref obj 6) ; benv
                         (sys::%record-ref obj 7) ; genv
                         (sys::%record-ref obj 8) ; denv
                 )
                 (sys::%record-ref obj 0) ; Name
        ))
        (t
         (error-of-type 'type-error
           :datum obj :expected-type 'function
           (DEUTSCH "~S: ~S ist keine Funktion."
            ENGLISH "~S: ~S is not a function"
            FRANCAIS "~S : ~S n'est pas une fonction.")
           'function-lambda-expression obj
) )     ))

;-------------------------------------------------------------------------------

;; X3J13 vote <52>

;; Package-Definition und -Installation, CLtL2 S. 270
(defmacro defpackage (packname &rest options)
  (flet ((check-packname (name)
           (cond ((stringp name) name)
                 ((symbolp name) (symbol-name name))
                 (t (error-of-type 'source-program-error
                      (DEUTSCH "~S: Package-Name muß ein String oder Symbol sein, nicht ~S."
                       ENGLISH "~S: package name ~S should be a string or a symbol"
                       FRANCAIS "~S : Le nom d'un paquetage doit être une chaîne ou un symbole et non ~S.")
                      'defpackage name
         ) )     )  )
         (check-symname (name)
           (cond ((stringp name) name)
                 ((symbolp name) (symbol-name name))
                 (t (error-of-type 'source-program-error
                      (DEUTSCH "~S ~A: Symbol-Name muß ein String oder Symbol sein, nicht ~S."
                       ENGLISH "~S ~A: symbol name ~S should be a string or a symbol"
                       FRANCAIS "~S ~A : Le nom d'un symbole doit être une chaîne ou un symbole et non ~S.")
                      'defpackage packname name
        )) )     )  )
    (setq packname (check-packname packname))
    ; Optionen abarbeiten:
    (let ((size nil) ; Flag ob :SIZE schon da war
          (documentation nil) ; Flag, ob :DOCUMENTATION schon da war
          (nickname-list '()) ; Liste von Nicknames
          (shadow-list '()) ; Liste von Symbolnamen für shadow
          (shadowing-list '()) ; Listen von Paaren (Symbolname . Paketname) für shadowing-import
          (use-list '()) ; Liste von Paketnamen für use-package
          (use-default '("LISP")) ; Default-Wert für use-list
          (case-sensitive nil) ; Flag für :CASE-SENSITIVE
          (import-list '()) ; Listen von Paaren (Symbolname . Paketname) für import
          (intern-list '()) ; Liste von Symbolnamen für intern
          (symname-list '()) ; Liste aller bisher aufgeführten Symbolnamen
          (export-list '())) ; Liste von Symbolnamen für export
      (flet ((record-symname (name)
               (if (member name symname-list :test #'string=)
                 (error-of-type 'source-program-error
                   (DEUTSCH "~S ~A: Symbol ~A darf nur einmal aufgeführt werden."
                    ENGLISH "~S ~A: the symbol ~A must not be specified more than once"
                    FRANCAIS "~S ~A : Le symbole ~A ne peut être mentionné qu'une seule fois.")
                   'defpackage packname name
                 )
                 (push name symname-list)
               )
               name
            ))
        (dolist (option options)
          (if (listp option)
            (if (keywordp (car option))
              (case (first option)
                (:SIZE
                  (if size
                    (error-of-type 'source-program-error
                      (DEUTSCH "~S ~A: Die Option ~S darf nur einmal angegeben werden."
                       ENGLISH "~S ~A: the ~S option must not be given more than once"
                       FRANCAIS "~S ~A : L'option ~S ne doit apparaître qu'une seule fois.")
                      'defpackage packname ':SIZE
                    )
                    (setq size t) ; Argument wird ignoriert
                ) )
                (:DOCUMENTATION ; ANSI-CL
                  (if documentation
                    (error-of-type 'source-program-error
                      (DEUTSCH "~S ~A: Die Option ~S darf nur einmal angegeben werden."
                       ENGLISH "~S ~A: the ~S option must not be given more than once"
                       FRANCAIS "~S ~A : L'option ~S ne doit apparaître qu'une seule fois.")
                      'defpackage packname ':DOCUMENTATION
                    )
                    (setq documentation t) ; Argument wird ignoriert
                ) )
                (:NICKNAMES
                  (dolist (name (rest option))
                    (push (check-packname name) nickname-list)
                ) )
                (:SHADOW
                  (dolist (name (rest option))
                    (push (record-symname (check-symname name)) shadow-list)
                ) )
                (:SHADOWING-IMPORT-FROM
                  (let ((pack (check-packname (second option))))
                    (dolist (name (cddr option))
                      (push (cons (record-symname (check-symname name)) pack)
                            shadowing-list
                ) ) ) )
                (:USE
                  (dolist (name (rest option))
                    (push (check-packname name) use-list)
                  )
                  (setq use-default nil)
                )
                (:IMPORT-FROM
                  (let ((pack (check-packname (second option))))
                    (dolist (name (cddr option))
                      (push (cons (record-symname (check-symname name)) pack)
                            import-list
                ) ) ) )
                (:INTERN
                  (dolist (name (rest option))
                    (push (record-symname (check-symname name)) intern-list)
                ) )
                (:EXPORT
                  (dolist (name (rest option))
                    (push (check-symname name) export-list)
                ) )
                (:CASE-SENSITIVE ; CLISP extension
                  (when (not (null (second option)))
                    (setq case-sensitive t)
                ) )
                (T (error-of-type 'source-program-error
                     (DEUTSCH "~S ~A: Die Option ~S gibt es nicht."
                      ENGLISH "~S ~A: unknown option ~S"
                      FRANCAIS "~S ~A : Option ~S non reconnue.")
                     'defpackage packname (first option)
              ) )  )
              (error-of-type 'source-program-error
                (DEUTSCH "~S ~A: Falsche Syntax in ~S-Option: ~S"
                 ENGLISH "~S ~A: invalid syntax in ~S option: ~S"
                 FRANCAIS "~S ~A : Mauvaise syntaxe dans l'option ~S: ~S")
                'defpackage packname 'defpackage option
            ) )
            (error-of-type 'source-program-error
              (DEUTSCH "~S ~A: Das ist keine ~S-Option: ~S"
               ENGLISH "~S ~A: not a ~S option: ~S"
               FRANCAIS "~S ~A : Ceci n'est pas une option ~S: ~S")
              'defpackage packname 'defpackage option
        ) ) )
        ; Auf Überschneidungen zwischen intern-list und export-list prüfen:
        (setq symname-list intern-list)
        (mapc #'record-symname export-list)
      )
      ; Listen umdrehen und Default-Werte eintragen:
      (setq nickname-list (nreverse nickname-list))
      (setq shadow-list (nreverse shadow-list))
      (setq shadowing-list (nreverse shadowing-list))
      (setq use-list (or use-default (nreverse use-list)))
      (setq import-list (nreverse import-list))
      (setq intern-list (nreverse intern-list))
      (setq export-list (nreverse export-list))
      ; Expansion produzieren:
      `(EVAL-WHEN (LOAD COMPILE EVAL)
         (SYSTEM::%IN-PACKAGE ,packname :NICKNAMES ',nickname-list
                                        :USE '()
                                        ,@(when case-sensitive `(:CASE-SENSITIVE T))
         )
         ; Schritt 1
         ,@(if shadow-list
             `((SHADOW ',(mapcar #'make-symbol shadow-list) ,packname))
           )
         ,@(mapcar
             #'(lambda (pair)
                 `(SHADOWING-IMPORT-CERROR ,(car pair) ,(cdr pair) ,packname)
               )
             shadowing-list
           )
         ; Schritt 2
         ,@(if use-list `((USE-PACKAGE ',use-list ,packname)))
         ; Schritt 3
         ,@(mapcar
             #'(lambda (pair)
                 `(IMPORT-CERROR ,(car pair) ,(cdr pair) ,packname)
               )
             import-list
           )
         ,@(mapcar
             #'(lambda (symname) `(INTERN ,symname ,packname))
             intern-list
           )
         ; Schritt 4
         ,@(if export-list
             `((INTERN-EXPORT ',export-list ,packname))
           )
         (FIND-PACKAGE ,packname)
       )
) ) )
; Hilfsfunktionen:
(defun find-symbol-cerror (string packname calling-packname)
  (multiple-value-bind (sym found) (find-symbol string packname)
    (unless found
      (cerror ; 'package-error ??
              (DEUTSCH "Dieses Symbol wird erzeugt."
               ENGLISH "This symbol will be created."
               FRANCAIS "Ce symbole sera créé.")
              (DEUTSCH "~S ~A: Es gibt kein Symbol ~A::~A ."
               ENGLISH "~S ~A: There is no symbol ~A::~A ."
               FRANCAIS "~S ~A : Il n'y a pas de symbole ~A::~A .")
              'defpackage calling-packname packname string
      )
      (setq sym (intern string packname))
    )
    sym
) )
(defun shadowing-import-cerror (string packname calling-packname)
  (shadowing-import (find-symbol-cerror string packname calling-packname)
                    calling-packname
) )
(defun import-cerror (string packname calling-packname)
  (import (find-symbol-cerror string packname calling-packname)
          calling-packname
) )
(defun intern-export (string-list packname)
  (export (mapcar #'(lambda (string) (intern string packname)) string-list)
          packname
) )

;-------------------------------------------------------------------------------

;; cf. X3J13 vote <173>

;; Definition globaler Symbol-Macros
(defmacro define-symbol-macro (symbol expansion)
  (unless (symbolp symbol)
    (error-of-type 'source-program-error
      (DEUTSCH "~S: Der Name eines Symbol-Macros muß ein Symbol sein, nicht: ~S"
       ENGLISH "~S: the name of a symbol macro must be a symbol, not ~S"
       FRANCAIS "~S : Le nom d'un macro symbole doit être un symbole et non ~S")
      'define-symbol-macro symbol
  ) )
  `(LET ()
     (EVAL-WHEN (COMPILE LOAD EVAL)
       (CHECK-NOT-SPECIAL-VARIABLE-P ',symbol)
       (MAKUNBOUND ',symbol)
       (SYSTEM::SET-SYMBOL-VALUE ',symbol (SYSTEM::MAKE-SYMBOL-MACRO ',expansion))
     )
     ',symbol
   )
)

(defun check-not-special-variable-p (symbol)
  (when (special-variable-p symbol)
    (error-of-type 'program-error
      (DEUTSCH "~S: Das Symbol ~S benennt eine globale Variable."
       ENGLISH "~S: the symbol ~S names a global variable"
       FRANCAIS "~S : Le symbole ~S est le nom d'une variable globale.")
      'define-symbol-macro symbol
) ) )

;-------------------------------------------------------------------------------

;; X3J13 vote <40>

(defmacro print-unreadable-object
    ((&whole args object stream &key type identity) &body body)
  (declare (ignore object stream type identity))
  `(SYSTEM::WRITE-UNREADABLE
     ,(if body `(FUNCTION (LAMBDA () ,@body)) 'NIL)
     ,@args
   )
)

;-------------------------------------------------------------------------------

;; X3J13 vote <144>

(defmacro declaim (&rest decl-specs)
  `(PROGN
     ,@(mapcar #'(lambda (decl-spec) `(PROCLAIM (QUOTE ,decl-spec))) decl-specs)
   )
)

;-------------------------------------------------------------------------------

;; X3J13 vote <64>

(defmacro destructuring-bind (lambdalist form &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (if declarations (setq declarations `((DECLARE ,@declarations))))
    (let ((%arg-count 0) (%min-args 0) (%restp nil)
          (%let-list nil) (%keyword-tests nil) (%default-form nil))
      (analyze1 lambdalist '<DESTRUCTURING-FORM> 'destructuring-bind '<DESTRUCTURING-FORM>)
      (let ((lengthtest (make-length-test '<DESTRUCTURING-FORM> 0))
            (mainform `(LET* ,(nreverse %let-list)
                         ,@declarations
                         ,@(nreverse %keyword-tests)
                         ,@body-rest
           ))          )
        (if lengthtest
          (setq mainform
            `(IF ,lengthtest
               (DESTRUCTURING-ERROR <DESTRUCTURING-FORM>
                                    '(,%min-args . ,(if %restp nil %arg-count))
               )
               ,mainform
        ) )  )
        `(LET ((<DESTRUCTURING-FORM> ,form)) ,mainform)
) ) ) )

(defun destructuring-error (destructuring-form min.max)
  (let ((min (car min.max))
        (max (cdr min.max)))
    (error-of-type 'error
      (DEUTSCH "Das zu zerlegende Objekt sollte eine Liste mit ~:[mindestens ~*~S~;~:[~S bis ~S~;~S~]~] Elementen sein, nicht ~4@*~S."
       ENGLISH "The object to be destructured should be a list with ~:[at least ~*~S~;~:[from ~S to ~S~;~S~]~] elements, not ~4@*~S."
       FRANCAIS "L'objet à démonter devrait être une liste ~:[d'au moins ~*~S~;de ~:[~S à ~S~;~S~]~] éléments et non ~4@*~S.")
      max (eql min max) min max destructuring-form
) ) )

;-------------------------------------------------------------------------------

;; X3J13 vote <87>

(defun complement (fun)
  #'(lambda (&rest arguments) (not (apply fun arguments)))
)

;; ANSI-CL

(defun constantly (object)
  #'(lambda (&rest arguments) (declare (ignore arguments)) object)
)

;-------------------------------------------------------------------------------

;; part of X3J13 vote <40>

(defconstant *common-lisp-user-package* (find-package "COMMON-LISP-USER"))

(defmacro with-standard-io-syntax (&body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
    ; It would be possible to put all these bindings into a single function,
    ; but this would force variables into closures.
    `(LET (; printer/reader variables:
           (*PACKAGE*                   *COMMON-LISP-USER-PACKAGE*)
           ; printer variables:
           (*PRINT-ARRAY*               T)
           (*PRINT-BASE*                10)
           (*PRINT-CASE*                ':UPCASE)
           (*PRINT-CIRCLE*              NIL)
           (*PRINT-ESCAPE*              T)
           (*PRINT-GENSYM*              T)
           (*PRINT-LENGTH*              NIL)
           (*PRINT-LEVEL*               NIL)
          ;(*PRINT-LINES*               NIL) ; XP variable not present in CLISP
          ;(*PRINT-MISER-WIDTH*         NIL) ; XP variable not present in CLISP
          ;(*PRINT-PPRINT-DISPATCH*     NIL) ; XP variable not present in CLISP
           (*PRINT-PRETTY*              NIL)
           (*PRINT-RADIX*               NIL)
           (*PRINT-READABLY*            T)
          ;(*PRINT-RIGHT-MARGIN*        NIL) ; XP variable not present in CLISP
           (*PRINT-CLOSURE*             NIL) ; CLISP specific
           (*PRINT-RPARS*               T) ; CLISP specific
           (*PRINT-INDENT-LISTS*        2) ; CLISP specific
           (SYSTEM::*PRIN-STREAM*       NIL) ; CLISP specific
           (SYSTEM::*PRIN-LINELENGTH*   79) ; CLISP specific
           ; reader variables:
           (*READ-BASE*                 10)
           (*READ-DEFAULT-FLOAT-FORMAT* 'SINGLE-FLOAT)
           (*READ-EVAL*                 T)
           (*READ-SUPPRESS*             NIL)
           (*READTABLE*                 (COPY-READTABLE NIL))
          )
       ,@(if declarations `((DECLARE ,@declarations)))
       ,@body-rest
     )
) )

;-------------------------------------------------------------------------------

;; part of X3J13 vote <98>

(defmacro with-hash-table-iterator ((macroname hashtable) &body body)
  (unless (symbolp macroname)
    (error (DEUTSCH "~S: Macroname muß ein Symbol sein, nicht ~S"
            ENGLISH "~S: macro name should be a symbol, not ~S"
            FRANCAIS "~S : le nom de macro n'est pas un symbole: ~S")
           'with-hash-table-iterator macroname
  ) )
  (let ((var (gensym)))
    `(LET ((,var (SYS::HASH-TABLE-ITERATOR ,hashtable)))
       (MACROLET ((,macroname () '(SYS::HASH-TABLE-ITERATE ,var) ))
         ,@body
     ) )
) )

;-------------------------------------------------------------------------------

;; ANSI-CL

(defmacro lambda (&whole whole
                  lambdalist &body body)
  (declare (ignore lambdalist body))
  `(FUNCTION ,whole)
)

;-------------------------------------------------------------------------------

;; Make GET-MACRO-CHARACTER work on dispatch macro characters.
(let ((vector '#()))
  (declare (compile))
  ; This code must be in accordance with io.d:read_macro().
  (defun dispatch-reader (stream ch)
    (let ((arg 0)
          subch)
      (let ((flag nil))
        (loop
          (let ((nextch (read-char stream nil nil)))
            (unless nextch
              (error-of-type 'end-of-file
                :stream stream
                (DEUTSCH "~S: Eingabestream ~S endet innerhalb eines Read-Macro zu ~S"
                 ENGLISH "~S: input stream ~S ends within read macro beginning to ~S"
                 FRANCAIS "~S : Le «stream» d'entrée se termine à l'intérieur d'un macro de lecture en ~S")
                'read stream ch
            ) )
            (unless (string-char-p nextch)
              (error-of-type 'stream-error
                :stream stream
                (DEUTSCH "~S von ~S: Gelesenes Zeichen ist kein String-Char: ~S"
                 ENGLISH "~S from ~S: character read should be a string-char: ~S"
                 FRANCAIS "~S de ~S : le caractère lu n'est pas de type STRING-CHAR.")
                'read stream ch
            ) )
            (unless (char<= #\0 nextch #\9)
              (setq subch nextch)
              (return)
            )
            (setq arg (+ (* 10 arg) (digit-char-p nextch)))
            (setq flag t)
        ) )
        (unless flag (setq arg nil))
      )
      (let ((macrodef (svref vector (char-int (char-upcase subch)))))
        (unless macrodef
          (error-of-type 'stream-error
            :stream stream
            (DEUTSCH "~S von ~S: Nach ~S ist ~S als Dispatch-Macrozeichen undefiniert."
             ENGLISH "~S from ~S: After ~S is ~S an undefined dispatch macro character"
             FRANCAIS "~S de ~S : Après ~S ~S n'est plus défini comme macro caractère de «dispatch».")
            'read stream ch subch
        ) )
        (funcall macrodef stream subch arg)
  ) ) )
  (let ((vector-index
          (do ((i 0 (1+ i)))
               (nil)
            (when (eq (%record-ref #'dispatch-reader i) vector) (return i)))))
    (%defio #'dispatch-reader vector-index)
  )
)

;-------------------------------------------------------------------------------

; READ-SEQUENCE and WRITE-SEQUENCE are badly specified because they assume
; that the stream has a unique element type, either subtype of CHARACTER or
; subtype of INTEGER. But some streams (esp. generic-streams) have a type
; of (OR CHARACTER INTEGER).

; This is a little hack to get the non-ambigouous cases right.

(defun stream-input-element-type (stream)
  (loop
    (typecase stream
      (SYNONYM-STREAM
        (setq stream (symbol-value (synonym-stream-symbol stream)))
      )
      (ECHO-STREAM
        (setq stream (echo-stream-input-stream stream))
      )
      (TWO-WAY-STREAM
        (setq stream (two-way-stream-input-stream stream))
      )
      (T (return))
  ) )
  (stream-element-type stream)
)

(defun stream-output-element-type (stream)
  (loop
    (typecase stream
      (SYNONYM-STREAM
        (setq stream (symbol-value (synonym-stream-symbol stream)))
      )
      (ECHO-STREAM
        (setq stream (echo-stream-output-stream stream))
      )
      (TWO-WAY-STREAM
        (setq stream (two-way-stream-output-stream stream))
      )
      (T (return))
  ) )
  (stream-element-type stream)
)

(defun read-sequence (sequence stream &rest rest &key (start 0) (end nil))
  (declare (ignore start end))
  (let ((eltype (stream-input-element-type stream)))
    (cond ((or (eq eltype 'NIL) (eq eltype 'STRING-CHAR) (eq eltype 'CHARACTER))
           (apply #'read-char-sequence sequence stream rest)
          )
          ((subtypep eltype 'INTEGER)
           (apply #'read-byte-sequence sequence stream rest)
          )
          (t
           (error (DEUTSCH "~S: ~S von ~S ist nicht eindeutig. Benutzen Sie ~S oder ~S."
                   ENGLISH "~S: ~S of ~S is ambiguous. Please use ~S or ~S."
                   FRANCAIS "~S : ~S de ~S est ambigu. Utilisez ~S ou ~S.")
                  'read-sequence 'stream-element-type stream
                  'read-char-sequence 'read-byte-sequence
) ) )     ))

(defun write-sequence (sequence stream &rest rest &key (start 0) (end nil))
  (declare (ignore start end))
  (let ((eltype (stream-output-element-type stream)))
    (cond ((or (eq eltype 'NIL) (eq eltype 'STRING-CHAR) (eq eltype 'CHARACTER))
           (apply #'write-char-sequence sequence stream rest)
          )
          ((subtypep eltype 'INTEGER)
           (apply #'write-byte-sequence sequence stream rest)
          )
          (t
           (error (DEUTSCH "~S: ~S von ~S ist nicht eindeutig. Benutzen Sie ~S oder ~S."
                   ENGLISH "~S: ~S of ~S is ambiguous. Please use ~S or ~S."
                   FRANCAIS "~S : ~S de ~S est ambigu. Utilisez ~S ou ~S.")
                  'write-sequence 'stream-element-type stream
                  'write-char-sequence 'write-byte-sequence
) ) )     ))

;-------------------------------------------------------------------------------

; ANSI-CL specifies TYPE-ERRORs in many places.
; Here are the corresponding types.

; (DESIGNATOR thing) is an abbreviation for many terms seen in the CLHS
; glossary.
;
; bounding index sequence    (START-INDEX sequence), (END-INDEX sequence)
; character                  CHARACTER
; class                      CLASS
; condition                  ---
; extended function          EXTENDED-FUNCTION
; external file format       ---
; file position              FILE-POSITION
; function                   FUNCTION
; interval                   ---
; list                       LIST
; logical-host               LOGICAL-HOST
; package                    PACKAGE
; pathname                   PATHNAME
; readtable                  READTABLE
; restart                    RESTART
; spreadable argument list   ---
; stream                     STREAM
; stream variable            ---
; string                     STRING, (STRING length)
;                            STRING-CHAR

(deftype designator (thing)
  (cond ((symbolp thing)
         (case thing
;          (STRING
;            `(OR CHARACTER STRING SYMBOL)
;          )
           (CHARACTER
             `(OR CHARACTER
                  ,@(if (not *ansi*) `((INTEGER 0 ,(1- char-int-limit))))
                  (DESIGNATOR (STRING 1))
           )  )
;          (CLASS `(OR CLOS:CLASS (AND SYMBOL (SATISFIES CLASS-DESIGNATOR-P))))
;          (EXTENDED-FUNCTION
;            `(OR (AND (OR SYMBOL CONS) (SATISFIES FUNCTION-NAME-P)) FUNCTION)
;          )
;          (FILE-POSITION
;            `(OR (MEMBER :START :END) (INTEGER 0 *))
;          )
;          (FUNCTION
;            `(OR SYMBOL FUNCTION)
;          )
;          (LIST
;            `T
;          )
;          (LOGICAL-HOST
;            #-LOGICAL-PATHNAMES `NIL
;            #+LOGICAL-PATHNAMES `(OR STRING LOGICAL-PATHNAME)
;          )
;          (PACKAGE
;            `(OR (DESIGNATOR STRING) PACKAGE)
;          )
;          (PATHNAME
;            `(OR STRING FILE-STREAM PATHNAME)
;          )
;          (READTABLE
;            `(OR NULL READTABLE)
;          )
;          (RESTART
;            `(OR (AND SYMBOL (NOT NULL)) RESTART)
;          )
;          (STREAM
;            `(OR BOOLEAN STREAM)
;          )
           (STRING-CHAR
             `(OR STRING-CHAR
                  ,@(if (not *ansi*) `((INTEGER 0 ,(1- char-code-limit))))
                  (DESIGNATOR (STRING 1))
           )  )
           (t thing)
        ))
        ((consp thing)
         (case (first thing)
;          (START-INDEX
;            (let ((seq (second thing)))
;              (assert (typep seq 'SEQUENCE))
;              `(INTEGER 0 ,(length seq))
;          ) )
;          (END-INDEX
;            (let ((seq (second thing)))
;              (assert (typep seq 'SEQUENCE))
;              `(OR (INTEGER 0 ,(length (second thing))) NULL)
;          ) )
           (STRING
             (let ((n (second thing)))
               (assert (typep n '(INTEGER 0 *)))
               (let ((fun (intern (format nil "SYMBOL-OF-LENGTH-~D" n)
                                  (find-package "SYSTEM"))))
                 (unless (fboundp fun)
                   (setf (symbol-function fun)
                         #'(lambda (s)
                             (and (symbolp s) (eql (length (symbol-name s)) n))
                           )
                 ) )
                 `(OR ,@(if (eql n 1) '(STRING-CHAR) '())
                      (STRING ,n)
                      (AND SYMBOL (SATISFIES ,fun))
                  )
           ) ) )
           (t thing)
        ))
        (t (typespec-error 'designator thing))
) )

;(defun class-designator-p (sym &aux f)
;  (and (setq f (get sym 'CLOS::CLOSCLASS))
;       (clos::class-p f)
;       (eq (clos:class-name f) sym)
;) )

(defun recognizable-sequence-type-p (typespec)
  (or (subtypep typespec 'LIST) (subtypep typespec 'VECTOR))
)

;-------------------------------------------------------------------------------

