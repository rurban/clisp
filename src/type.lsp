;;;; TYPEP und Verwandtes
;;;; Michael Stoll, 21. 10. 1988
;;;; Bruno Haible, 10.6.1989

;;; Datenstrukturen für TYPEP:
;;; - Ein Type-Specifier-Symbol hat auf seiner Propertyliste unter dem
;;;   Indikator SYS::TYPE-SYMBOL eine Funktion von einem Argument, die
;;;   testet, ob ein Objekt vom richtigen Typ ist.
;;; - Ein Symbol, das eine Type-Specifier-Liste beginnen kann, hat auf seiner
;;;   Propertyliste unter dem Indikator SYS::TYPE-LIST eine Funktion von
;;;   einem Argument für das zu testende Objekt und zusätzlichen Argumenten
;;;   für die Listenelemente.
;;; - Ein Symbol, das als Typmacro definiert wurde, hat auf seiner Property-
;;;   liste unter dem Indikator SYSTEM::DEFTYPE-EXPANDER den zugehörigen
;;;   Expander: eine Funktion, die den zu expandierenden Type-Specifier (eine
;;;   mindestens einelementige Liste) als Argument bekommt.

(in-package "LISP")
(export '(type-expand-1 type-expand))
(in-package "SYSTEM")

; vorläufig, solange bis clos.lsp geladen wird:
(unless (fboundp 'clos::built-in-class-p)
  (defun clos::built-in-class-p (object) (declare (ignore object)) nil)
  (defun clos::subclassp (class1 class2) (declare (ignore class1 class2)) nil)
)

(defun typespec-error (fun type)
  (error-of-type 'error
    (DEUTSCH "~S: ~S ist keine zugelassene Typspezifikation."
     ENGLISH "~S: invalid type specification ~S"
     FRANCAIS "~S : ~S n'est pas une spécification de type légale.")
    fun type
) )

;-------------------------------------------------------------------------------

;;; TYPEP, CLTL S. 72, S. 42-51
(defun typep (x y &aux f) ; x = Objekt, y = Typ
  (cond
    ((symbolp y)
       (cond ((setq f (get y 'TYPE-SYMBOL)) (funcall f x))
             ((setq f (get y 'TYPE-LIST)) (funcall f x))
             ((setq f (get y 'DEFTYPE-EXPANDER)) (typep x (funcall f (list y))))
             ((get y 'DEFSTRUCT-DESCRIPTION) (%STRUCTURE-TYPE-P y x))
             ((and (setf f (get y 'CLOS::CLOSCLASS))
                   (clos::class-p f)
                   (eq (clos:class-name f) y)
              )
              (clos::subclassp (clos:class-of x) f)
             )
             (t (typespec-error 'typep y))
    )  )
    ((and (consp y) (symbolp (first y)))
       (cond
         ((and (eq (first y) 'SATISFIES) (eql (length y) 2))
            (unless (symbolp (second y))
              (error-of-type 'error
                (DEUTSCH "~S: Argument zu SATISFIES muss Symbol sein: ~S"
                 ENGLISH "~S: argument to SATISFIES must be a symbol: ~S"
                 FRANCAIS "~S : L'argument de SATISFIES doit être un symbole: ~S")
                'typep (second y)
            ) )
            (if (funcall (symbol-function (second y)) x) t nil)
         )
         ((eq (first y) 'MEMBER)
            (if (member x (rest y)) t nil)
         )
         ((and (eq (first y) 'EQL) (eql (length y) 2))
            (eql x (second y))
         )
         ((and (eq (first y) 'NOT) (eql (length y) 2))
            (not (typep x (second y)))
         )
         ((eq (first y) 'AND)
            (dolist (type (rest y) t)
              (unless (typep x type) (return nil))
         )  )
         ((eq (first y) 'OR)
            (dolist (type (rest y) nil)
              (when (typep x type) (return t))
         )  )
         ((setq f (get (first y) 'TYPE-LIST)) (apply f x (rest y)))
         ((setq f (get (first y) 'DEFTYPE-EXPANDER)) (typep x (funcall f y)))
         (t (typespec-error 'typep y))
    )  )
    ((clos::class-p y) (clos::subclassp (clos:class-of x) y))
    ((encodingp y) (charset-typep x y))
    (t (typespec-error 'typep y))
) )

; CLTL S. 43
(%put 'ARRAY 'TYPE-SYMBOL #'arrayp)
(%put 'ATOM 'TYPE-SYMBOL #'atom)
(%put 'BASE-CHAR 'TYPE-SYMBOL
  #+BASE-CHAR=CHARACTER
  #'characterp
  #-BASE-CHAR=CHARACTER
  (function type-symbol-base-char
    (lambda (x) (and (characterp x) (base-char-p x)))
) )
(%put 'BASE-STRING 'TYPE-SYMBOL #'stringp)
(%put 'BIGNUM 'TYPE-SYMBOL
  (function type-symbol-bignum
    (lambda (x) (and (integerp x) (not (fixnump x))))
) )
(%put 'BIT 'TYPE-SYMBOL
  (function type-symbol-bit
    (lambda (x) (or (eql x 0) (eql x 1)))
) )
(%put 'BIT-VECTOR 'TYPE-SYMBOL #'bit-vector-p)
(%put 'BOOLEAN 'TYPE-SYMBOL
  (function type-symbol-boolean
    (lambda (x) (or (eq x 'nil) (eq x 't)))
) )
(%put 'CHARACTER 'TYPE-SYMBOL #'characterp)
(%put 'COMMON 'TYPE-SYMBOL #'commonp)
(%put 'COMPILED-FUNCTION 'TYPE-SYMBOL #'compiled-function-p)
(%put 'COMPLEX 'TYPE-SYMBOL #'complexp)
(%put 'CONS 'TYPE-SYMBOL #'consp)
(%put 'DOUBLE-FLOAT 'TYPE-SYMBOL #'double-float-p)
(%put 'ENCODING 'TYPE-SYMBOL #'encodingp)
(%put 'EXTENDED-CHAR 'TYPE-SYMBOL
  (function type-symbol-extended-char
    #+BASE-CHAR=CHARACTER
    (lambda (x) (declare (ignore x)) nil)
    #-BASE-CHAR=CHARACTER
    (lambda (x) (and (characterp x) (not (base-char-p x))))
) )
(%put 'FIXNUM 'TYPE-SYMBOL #'fixnump)
(%put 'FLOAT 'TYPE-SYMBOL #'floatp)
(%put 'FUNCTION 'TYPE-SYMBOL #'functionp)
(%put 'CLOS:GENERIC-FUNCTION 'TYPE-SYMBOL #'clos::generic-function-p)
(%put 'HASH-TABLE 'TYPE-SYMBOL #'hash-table-p)
(%put 'INTEGER 'TYPE-SYMBOL #'integerp)
(%put 'KEYWORD 'TYPE-SYMBOL #'keywordp)
(%put 'LIST 'TYPE-SYMBOL #'listp)
#+LOGICAL-PATHNAMES
(%put 'LOGICAL-PATHNAME 'TYPE-SYMBOL #'logical-pathname-p)
(%put 'LONG-FLOAT 'TYPE-SYMBOL #'long-float-p)
(%put 'NIL 'TYPE-SYMBOL
  (function type-symbol-nil
    (lambda (x) (declare (ignore x)) nil)
) )
(%put 'NULL 'TYPE-SYMBOL #'null)
(%put 'NUMBER 'TYPE-SYMBOL #'numberp)
(%put 'PACKAGE 'TYPE-SYMBOL #'packagep)
(%put 'PATHNAME 'TYPE-SYMBOL #'pathnamep)
(%put 'RANDOM-STATE 'TYPE-SYMBOL #'random-state-p)
(%put 'RATIO 'TYPE-SYMBOL
  (function type-symbol-ratio
    (lambda (x) (and (rationalp x) (not (integerp x))))
) )
(%put 'RATIONAL 'TYPE-SYMBOL #'rationalp)
(%put 'READTABLE 'TYPE-SYMBOL #'readtablep)
(%put 'REAL 'TYPE-SYMBOL #'realp)
(%put 'SEQUENCE 'TYPE-SYMBOL #'sequencep)
(%put 'SHORT-FLOAT 'TYPE-SYMBOL #'short-float-p)
(%put 'SIMPLE-ARRAY 'TYPE-SYMBOL #'simple-array-p)
(%put 'SIMPLE-BASE-STRING 'TYPE-SYMBOL #'simple-string-p)
(%put 'SIMPLE-BIT-VECTOR 'TYPE-SYMBOL #'simple-bit-vector-p)
(%put 'SIMPLE-STRING 'TYPE-SYMBOL #'simple-string-p)
(%put 'SIMPLE-VECTOR 'TYPE-SYMBOL #'simple-vector-p)
(%put 'SINGLE-FLOAT 'TYPE-SYMBOL #'single-float-p)
(%put 'STANDARD-CHAR 'TYPE-SYMBOL
  (function type-symbol-standard-char
    (lambda (x) (and (characterp x) (standard-char-p x)))
) )
(%put 'CLOS:STANDARD-GENERIC-FUNCTION 'TYPE-SYMBOL #'clos::generic-function-p)
(%put 'CLOS:STANDARD-OBJECT 'TYPE-SYMBOL #'clos::std-instance-p)
(%put 'STREAM 'TYPE-SYMBOL #'streamp)
(%put 'FILE-STREAM 'TYPE-SYMBOL #'file-stream-p)
(%put 'SYNONYM-STREAM 'TYPE-SYMBOL #'synonym-stream-p)
(%put 'BROADCAST-STREAM 'TYPE-SYMBOL #'broadcast-stream-p)
(%put 'CONCATENATED-STREAM 'TYPE-SYMBOL #'concatenated-stream-p)
(%put 'TWO-WAY-STREAM 'TYPE-SYMBOL #'two-way-stream-p)
(%put 'ECHO-STREAM 'TYPE-SYMBOL #'echo-stream-p)
(%put 'STRING-STREAM 'TYPE-SYMBOL #'string-stream-p)
(%put 'STRING 'TYPE-SYMBOL #'stringp)
(%put 'STRING-CHAR 'TYPE-SYMBOL #'characterp)
(%put 'CLOS:STRUCTURE-OBJECT 'TYPE-SYMBOL #'clos::structure-object-p)
(%put 'SYMBOL 'TYPE-SYMBOL #'symbolp)
(%put 'T 'TYPE-SYMBOL
  (function type-symbol-t
    (lambda (x) (declare (ignore x)) t)
) )
(%put 'VECTOR 'TYPE-SYMBOL #'vectorp)

; CLTL S. 46-50
(defun upgraded-array-element-type (type)
  ; siehe array.d
  (case type
    ((BIT) 'BIT)
    ((CHARACTER) 'CHARACTER)
    ((T) 'T)
    (t (multiple-value-bind (low high) (sys::subtype-integer type)
         ; Es gilt (or (null low) (subtypep type `(INTEGER ,low ,high)))
         (if (and (integerp low) (not (minusp low)) (integerp high))
           (let ((l (integer-length high)))
             ; Es gilt (subtypep type `(UNSIGNED-BYTE ,l))
             (cond ((<= l 1) 'BIT)
                   ((<= l 2) '(UNSIGNED-BYTE 2))
                   ((<= l 4) '(UNSIGNED-BYTE 4))
                   ((<= l 8) '(UNSIGNED-BYTE 8))
                   ((<= l 16) '(UNSIGNED-BYTE 16))
                   ((<= l 32) '(UNSIGNED-BYTE 32))
                   (t 'T)
           ) )
           (if (subtypep type 'CHARACTER)
             'CHARACTER
             'T
  ) )  ) ) )
)
(%put 'ARRAY 'TYPE-LIST
  (function type-list-array
    (lambda (x &optional (el-type '*) (dims '*))
      (and (arrayp x)
           (or (eq el-type '*)
               (equal (array-element-type x) (upgraded-array-element-type el-type))
           )
           (or (eq dims '*)
               (if (numberp dims)
                 (eql dims (array-rank x))
                 (and (eql (length dims) (array-rank x))
                      (every #'(lambda (a b) (or (eq a '*) (eql a b)))
                             dims (array-dimensions x)
  ) ) )    )   ) )    )
)
(%put 'SIMPLE-ARRAY 'TYPE-LIST
  (function type-list-simple-array
    (lambda (x &optional (el-type '*) (dims '*))
      (and (simple-array-p x)
           (or (eq el-type '*)
               (equal (array-element-type x) (upgraded-array-element-type el-type))
           )
           (or (eq dims '*)
               (if (numberp dims)
                 (eql dims (array-rank x))
                 (and (eql (length dims) (array-rank x))
                      (every #'(lambda (a b) (or (eq a '*) (eql a b)))
                             dims (array-dimensions x)
  ) ) )    )   ) )    )
)
(%put 'VECTOR 'TYPE-LIST
  (function type-list-vector
    (lambda (x &optional (el-type '*) (size '*))
      (and (vectorp x)
           (or (eq el-type '*)
               (equal (array-element-type x) (upgraded-array-element-type el-type))
           )
           (or (eq size '*) (eql (array-dimension x 0) size))
  ) ) )
)
(%put 'SIMPLE-VECTOR 'TYPE-LIST
  (function type-list-simple-vector
    (lambda (x &optional (size '*))
      (and (simple-vector-p x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'COMPLEX 'TYPE-LIST
  (function type-list-complex
    (lambda (x &optional (rtype '*) (itype rtype))
      (and (complexp x)
           (or (eq rtype '*) (typep (realpart x) rtype))
           (or (eq itype '*) (typep (imagpart x) itype))
  ) ) )
)
(%put 'INTEGER 'TYPE-LIST
  (function type-list-integer
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'integerp 'INTEGER)
  ) )
)
(defun typep-number-test (x low high test type)
  (and (funcall test x)
       (cond ((eq low '*))
             ((funcall test low) (<= low x))
             ((and (consp low) (null (rest low)) (funcall test (first low)))
                (< (first low) x)
             )
             (t (error-of-type 'error
                  (DEUTSCH "~S: Argument zu ~S muss *, ~S oder eine Liste von ~S sein: ~S"
                   ENGLISH "~S: argument to ~S must be *, ~S or a list of ~S: ~S"
                   FRANCAIS "~S : L'argument de ~S doit être *, ~S ou une liste de ~S: ~S")
                  'typep type type type low
       )     )  )
       (cond ((eq high '*))
             ((funcall test high) (>= high x))
             ((and (consp high) (null (rest high)) (funcall test (first high)))
                (> (first high) x)
             )
             (t (error-of-type 'error
                  (DEUTSCH "~S: Argument zu ~S muss *, ~S oder eine Liste von ~S sein: ~S"
                   ENGLISH "~S: argument to ~S must be *, ~S or a list of ~S: ~S"
                   FRANCAIS "~S : L'argument de ~S doit être *, ~S ou une liste de ~S: ~S")
                  'typep type type type high
) )    )     )  )
(%put 'MOD 'TYPE-LIST
  (function type-list-mod
    (lambda (x n)
      (unless (integerp n)
        (error-of-type 'error
          (DEUTSCH "~S: Argument zu MOD muss ganze Zahl sein: ~S"
           ENGLISH "~S: argument to MOD must be an integer: ~S"
           FRANCAIS "~S : L'argument de MOD doit être un entier: ~S")
          'typep n
      ) )
      (and (integerp x) (<= 0 x) (< x n))
  ) )
)
(%put 'SIGNED-BYTE 'TYPE-LIST
  (function type-list-signed-byte
    (lambda (x &optional (n '*))
      (unless (or (eq n '*) (integerp n))
        (error-of-type 'error
          (DEUTSCH "~S: Argument zu SIGNED-BYTE muss ganze Zahl oder * sein: ~S"
           ENGLISH "~S: argument to SIGNED-BYTE must be an integer or * : ~S"
           FRANCAIS "~S : L'argument de SIGNED-BYTE doit être un entier ou bien * : ~S")
          'typep n
      ) )
      (and (integerp x) (or (eq n '*) (< (integer-length x) n)))
  ) )
)
(%put 'UNSIGNED-BYTE 'TYPE-LIST
  (function type-list-unsigned-byte
    (lambda (x &optional (n '*))
      (unless (or (eq n '*) (integerp n))
        (error-of-type 'error
          (DEUTSCH "~S: Argument zu UNSIGNED-BYTE muss ganze Zahl oder * sein: ~S"
           ENGLISH "~S: argument to UNSIGNED-BYTE must be an integer or * : ~S"
           FRANCAIS "~S : L'argument de UNSIGNED-BYTE doit être un entier ou bien * : ~S")
          'typep n
      ) )
      (and (integerp x)
           (not (minusp x))
           (or (eq n '*) (<= (integer-length x) n))
  ) ) )
)
(%put 'REAL 'TYPE-LIST
  (function type-list-real
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'realp 'REAL)
  ) )
)
(%put 'RATIONAL 'TYPE-LIST
  (function type-list-rational
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'rationalp 'RATIONAL)
  ) )
)
(%put 'FLOAT 'TYPE-LIST
  (function type-list-float
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'floatp 'FLOAT)
  ) )
)
(%put 'SHORT-FLOAT 'TYPE-LIST
  (function type-list-short-float
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'short-float-p 'SHORT-FLOAT)
  ) )
)
(%put 'SINGLE-FLOAT 'TYPE-LIST
  (function type-list-single-float
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'single-float-p 'SINGLE-FLOAT)
  ) )
)
(%put 'DOUBLE-FLOAT 'TYPE-LIST
  (function type-list-double-float
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'double-float-p 'DOUBLE-FLOAT)
  ) )
)
(%put 'LONG-FLOAT 'TYPE-LIST
  (function type-list-long-float
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'long-float-p 'LONG-FLOAT)
  ) )
)
(%put 'STRING 'TYPE-LIST
  (function type-list-string
    (lambda (x &optional (size '*))
      (and (stringp x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'SIMPLE-STRING 'TYPE-LIST
  (function type-list-simple-string
    (lambda (x &optional (size '*))
      (and (simple-string-p x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'BASE-STRING 'TYPE-LIST
  (function type-list-base-string
    (lambda (x &optional (size '*))
      (and (stringp x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'SIMPLE-BASE-STRING 'TYPE-LIST
  (function type-list-simple-base-string
    (lambda (x &optional (size '*))
      (and (simple-string-p x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'BIT-VECTOR 'TYPE-LIST
  (function type-list-bit-vector
    (lambda (x &optional (size '*))
      (and (bit-vector-p x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'SIMPLE-BIT-VECTOR 'TYPE-LIST
  (function type-list-simple-bit-vector
    (lambda (x &optional (size '*))
      (and (simple-bit-vector-p x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'CONS 'TYPE-LIST
  (function type-cons
    (lambda (x &optional (car-type '*) (cdr-type '*))
      (and (consp x)
           (or (eq car-type '*) (typep (car x) car-type))
           (or (eq cdr-type '*) (typep (cdr x) cdr-type))
  ) ) )
)

; Typtest ohne Gefahr einer Fehlermeldung. Für SIGNAL und HANDLER-BIND.
(defun safe-typep (x y)
  (let ((*error-handler*
          #'(lambda (&rest error-args)
              (declare (ignore error-args))
              (return-from safe-typep nil)
       ))   )
    (typep x y)
) )

; Umwandlung eines "type for declaration" in einen "type for discrimination".
(defun type-for-discrimination (y &optional (notp nil) &aux f)
  (cond ((symbolp y)
           (cond ((get y 'TYPE-SYMBOL) y)
                 ((get y 'TYPE-LIST) y)
                 ((setq f (get y 'DEFTYPE-EXPANDER))
                  (let* ((z (funcall f (list y)))
                         (zx (type-for-discrimination z notp)))
                    (if (eql zx z) y zx)
                 ))
                 (t y)
        )  )
        ((and (consp y) (symbolp (first y)))
           (case (first y)
             ((SATISFIES MEMBER EQL) y)
             (NOT
              (let* ((z (second y))
                     (zx (type-for-discrimination z (not notp))))
                (if (eql zx z) y `(NOT ,zx))
             ))
             ((AND OR COMPLEX VALUES)
              (let* ((z (rest y))
                     (zx (mapcar #'(lambda (x) (type-for-discrimination x notp)) z)))
                (if (every #'eql z zx) y (cons (first y) zx))
             ))
             (FUNCTION
              ;; (FUNCTION arg-types res-type) is somewhere between
              ;; NIL and FUNCTION, but undecidable.
              (if notp 'NIL 'FUNCTION)
             )
             (t (cond ((get (first y) 'TYPE-LIST) y)
                      ((setq f (get (first y) 'DEFTYPE-EXPANDER))
                       (let* ((z (funcall f y))
                              (zx (type-for-discrimination z notp)))
                         (if (eql zx z) y zx)
                      ))
                      (t y)
        )  ) )  )
        (t y)
) )

; Testet eine Liste von Werten auf Erfüllen eines Type-Specifiers. Für THE.
(defun %the (values type)
  (macrolet ((near-typep (objform typform)
               ;; near-typep ist wie typep, nur dass das Objekt auch ein
               ;; Read-Label sein darf. Das tritt z.B. auf bei
               ;; (read-from-string "#1=#S(FOO :X #1#)")
               ;; im Konstruktor MAKE-FOO. Die Implementation ist aber
               ;; nicht gezwungen, bei fehlerhaftem THE zwingend einen
               ;; Fehler zu melden, darum ist ein lascherer Typcheck hier
               ;; erlaubt.
               (let ((g (gensym)))
                 `(let ((,g ,objform))
                    (or (typep ,g ,typform) (eq (type-of ,g) 'READ-LABEL))
                  )
            )) )
    (if (and (consp type) (eq (car type) 'VALUES))
      (macrolet ((typespec-error ()
                   '(error-of-type 'error
                      (DEUTSCH "Falsch aufgebauter Type-Specifier: ~S"
                       ENGLISH "Invalid type specifier ~S"
                       FRANCAIS "Spécificateur de type mal formé : ~S")
                      type
                ))  )
        (let ((vals values)
              (types (cdr type)))
          ; required-Werte:
          (loop
            (when (or (atom types) (member (car types) lambda-list-keywords :test #'eq))
              (return)
            )
            (unless (and (consp vals) (near-typep (car vals) (car types)))
              (return-from %the nil)
            )
            (setq vals (cdr vals))
            (setq types (cdr types))
          )
          ; optionale Werte:
          (when (and (consp types) (eq (car types) '&optional))
            (setq types (cdr types))
            (loop
              (when (or (atom types) (member (car types) lambda-list-keywords :test #'eq))
                (return)
              )
              (when (consp vals)
                (unless (near-typep (car vals) (car types)) (return-from %the nil))
                (setq vals (cdr vals))
              )
              (setq types (cdr types))
          ) )
          ; restliche Werte:
          (if (atom types)
            (when (consp vals) (return-from %the nil))
            (case (car types)
              (&rest
                (setq types (cdr types))
                (when (atom types) (typespec-error))
                (unless (near-typep vals (car types)) (return-from %the nil))
                (setq types (cdr types))
              )
              (&key)
              (t (typespec-error))
          ) )
          ; Keyword-Werte:
          (when (consp types)
            (if (eq (car types) '&key)
              (progn
                (setq types (cdr types))
                (when (oddp (length vals)) (return-from %the nil))
                (let ((keywords nil))
                  (loop
                    (when (or (atom types) (member (car types) lambda-list-keywords :test #'eq))
                      (return)
                    )
                    (let ((item (car types)))
                      (unless (and (listp item) (eql (length item) 2) (symbolp (first item)))
                        (typespec-error)
                      )
                      (let ((kw (intern (symbol-name (first item)) *keyword-package*)))
                        (unless (near-typep (getf vals kw) (second item))
                          (return-from %the nil)
                        )
                        (push kw keywords)
                    ) )
                    (setq types (cdr types))
                  )
                  (if (and (consp types) (eq (car types) '&allow-other-keys))
                    (setq types (cdr types))
                    (unless (getf vals ':allow-other-keys)
                      (do ((L vals (cddr L)))
                          ((atom L))
                        (unless (member (car L) keywords :test #'eq)
                          (return-from %the nil)
                  ) ) ) )
              ) )
              (when (consp types) (typespec-error))
          ) )
          t
      ) )
      (near-typep (if (consp values) (car values) nil) type) ; 1. Wert abtesten
) ) )

;-------------------------------------------------------------------------------

;;; SUBTYPEP, vorläufige Version
(defun canonicalize-type (type) ; type ein wenig vereinfachen, nicht rekursiv
  (cond ((symbolp type)
         (let ((f (get type 'DEFTYPE-EXPANDER)))
           (if f
             (canonicalize-type (funcall f (list type))) ; macroexpandieren
             (case type
               (ATOM '(NOT CONS))
               (BASE-CHAR #+BASE-CHAR=CHARACTER 'CHARACTER
                          #-BASE-CHAR=CHARACTER '(AND CHARACTER (SATISFIES BASE-CHAR-P))
               )
               (BIGNUM '(AND INTEGER (NOT FIXNUM)))
               (BIT '(INTEGER 0 1))
               (BOOLEAN '(MEMBER NIL T))
               (COMMON '(OR CONS SYMBOL NUMBER ARRAY STANDARD-CHAR
                         STREAM PACKAGE HASH-TABLE READTABLE PATHNAME RANDOM-STATE
                         STRUCTURE
               )        )
               (EXTENDED-CHAR #+BASE-CHAR=CHARACTER 'NIL
                              #-BASE-CHAR=CHARACTER '(AND CHARACTER (NOT (SATISFIES BASE-CHAR-P)))
               )
               (FIXNUM '(INTEGER #,most-negative-fixnum #,most-positive-fixnum))
               (KEYWORD '(AND SYMBOL (SATISFIES KEYWORDP)))
               (LIST '(OR CONS (MEMBER NIL)))
               ((NIL) '(OR))
               (NULL '(MEMBER NIL))
               (NUMBER '(OR REAL COMPLEX))
               (RATIO '(AND RATIONAL (NOT INTEGER)))
               (SEQUENCE '(OR LIST VECTOR)) ; user-defined sequences??
               (SIGNED-BYTE 'INTEGER)
               (STANDARD-CHAR '(AND CHARACTER #-BASE-CHAR=CHARACTER (SATISFIES BASE-CHAR-P) (SATISFIES STANDARD-CHAR-P)))
               (STRING-CHAR 'CHARACTER)
               ((T) '(AND))
               (UNSIGNED-BYTE '(INTEGER 0 *))
               ((ARRAY SIMPLE-ARRAY BIT-VECTOR SIMPLE-BIT-VECTOR
                 STRING SIMPLE-STRING BASE-STRING SIMPLE-BASE-STRING
                 VECTOR SIMPLE-VECTOR
                 COMPLEX REAL INTEGER RATIONAL FLOAT
                 SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT
                )
                 (canonicalize-type (list type))
               )
               (t (if (and (setq f (get type 'CLOS::CLOSCLASS))
                           (clos::class-p f) (not (clos::built-in-class-p f))
                           (eq (clos:class-name f) type)
                      )
                    f
                    type
        )) ) ) )  )
        ((and (consp type) (symbolp (first type)))
         (let ((f (get (first type) 'DEFTYPE-EXPANDER)))
           (if f
             (canonicalize-type (funcall f type)) ; macroexpandieren
             (case (first type)
               (MEMBER ; (MEMBER &rest objects)
                 (if (null (rest type)) '(OR) type)
               )
               (EQL ; (EQL object)
                 `(MEMBER ,(second type))
               )
               ((AND OR) ; (AND type*), (OR type*) kann man evtl. vereinfachen
                 (if (and (consp (cdr type)) (null (cddr type)))
                   (canonicalize-type (second type))
                   type
               ) )
               (MOD ; (MOD n)
                 (let ((n (second type)))
                   (unless (and (integerp n) (>= n 0)) (typespec-error 'subtypep type))
                   `(INTEGER 0 (,n))
               ) )
               (SIGNED-BYTE ; (SIGNED-BYTE &optional s)
                 (let ((s (or (second type) '*)))
                   (if (eq s '*)
                     'INTEGER
                     (progn
                       (unless (and (integerp s) (plusp s)) (typespec-error 'subtypep type))
                       (let ((n (expt 2 (1- s))))
                         `(INTEGER ,(- n) (,n))
               ) ) ) ) )
               (UNSIGNED-BYTE ; (UNSIGNED-BYTE &optional s)
                 (let ((s (or (second type) '*)))
                   (if (eq s '*)
                     '(INTEGER 0 *)
                     (progn
                       (unless (and (integerp s) (>= s 0)) (typespec-error 'subtypep type))
                       (let ((n (expt 2 s)))
                         `(INTEGER 0 (,n))
               ) ) ) ) )
               (SIMPLE-BIT-VECTOR ; (SIMPLE-BIT-VECTOR &optional size)
                 (let ((size (or (second type) '*)))
                   `(SIMPLE-ARRAY BIT (,size))
               ) )
               (SIMPLE-STRING ; (SIMPLE-STRING &optional size)
                 (let ((size (or (second type) '*)))
                   `(SIMPLE-ARRAY CHARACTER (,size))
               ) )
               (SIMPLE-BASE-STRING ; (SIMPLE-BASE-STRING &optional size)
                 (let ((size (or (second type) '*)))
                   `(SIMPLE-ARRAY BASE-CHAR (,size))
               ) )
               (SIMPLE-VECTOR ; (SIMPLE-VECTOR &optional size)
                 (let ((size (or (second type) '*)))
                   `(SIMPLE-ARRAY T (,size))
               ) )
               (BIT-VECTOR ; (BIT-VECTOR &optional size)
                 (let ((size (or (second type) '*)))
                   `(ARRAY BIT (,size))
               ) )
               (STRING ; (STRING &optional size)
                 (let ((size (or (second type) '*)))
                   `(ARRAY CHARACTER (,size))
               ) )
               (BASE-STRING ; (BASE-STRING &optional size)
                 (let ((size (or (second type) '*)))
                   `(ARRAY BASE-CHAR (,size))
               ) )
               (VECTOR ; (VECTOR &optional el-type size)
                 (let ((el-type (or (second type) '*))
                       (size (or (third type) '*)))
                   `(ARRAY ,el-type (,size))
               ) )
               (t type)
        )) ) )
        ((clos::class-p type)
         (if (and (clos::built-in-class-p type)
                  (eq (get (clos:class-name type) 'CLOS::CLOSCLASS) type)
             )
           (canonicalize-type (clos:class-name type))
           type
        ))
        ((encodingp type)
         #+UNICODE
           (case (sys::%record-ref type 1) ; encoding-charset
             ((charset:unicode-16-big-endian charset:unicode-16-little-endian
               charset:unicode-32-big-endian charset:unicode-32-little-endian
               charset:utf-8 charset:java)
              'CHARACTER
             )
             (t type)
           )
         #-UNICODE 'CHARACTER
        )
) )
(defun subtypep (type1 type2)
  (macrolet ((yes () '(return-from subtypep (values t t)))
             (no () '(return-from subtypep (values nil t)))
             (unknown () '(return-from subtypep (values nil nil))))
    (setq type1 (canonicalize-type type1))
    (setq type2 (canonicalize-type type2))
    (when (equal type1 type2) (yes)) ; (subtypep type type) stimmt immer
                                     ; equal auf MEMBER und EQL verboten!!??
    (when (consp type1)
      (cond ;; über SATISFIES-Typen kann man nichts aussagen
            ;((and (eq (first type1) 'SATISFIES) (eql (length type1) 2))
            ; (unknown)
            ;)
            ;; MEMBER: alle Elemente müssen vom Typ type2 sein
            ((eq (first type1) 'MEMBER)
             (dolist (x (rest type1) (yes))
               (unless (typep x type2) (return (no)))
            ))
            ;; NOT: (subtypep `(NOT ,type1) `(NOT ,type2)) ist äquivalent
            ;; zu (subtypep type2 type1), sonst ist Entscheidung schwierig
            ((and (eq (first type1) 'NOT) (eql (length type1) 2))
             (return-from subtypep
               (if (and (consp type2) (eq (first type2) 'NOT) (eql (length type2) 2))
                 (subtypep (second type2) (second type1))
                 (unknown)
            )) )
            ;; OR: Jeder Typ muss Subtyp von type2 sein
            ((eq (first type1) 'OR)
             (dolist (type (rest type1) (yes))
               (multiple-value-bind (is known) (subtypep type type2)
                 (unless is (return-from subtypep (values nil known)))
            )) )
    ) )
    (when (consp type2)
      (cond ;; über SATISFIES-Typen kann man nichts aussagen
            ;((and (eq (first type2) 'SATISFIES) (eql (length type2) 2))
            ; (unknown)
            ;)
            ;; NOT: siehe oben
            ((and (eq (first type2) 'NOT) (eql (length type2) 2))
             (unknown)
            )
            ;; AND: type1 muss Subtyp jedes der Typen sein
            ((eq (first type2) 'AND)
             (dolist (type (rest type2) (yes))
               (multiple-value-bind (is known) (subtypep type1 type)
                 (unless is (return-from subtypep (values nil known)))
            )) )
            ;; OR: Falls type1 Subtyp eines der Typen ist, sonst nicht bekannt
            ((eq (first type2) 'OR)
             (if (rest type2)
               (dolist (type (rest type2) (unknown))
                 (when (subtypep type1 type) (return (yes)))
               )
               (setq type2 'NIL) ; wird später besser behandelt
            ))
    ) )
    (when (consp type1)
      (cond ;; AND: Falls ein Typ Subtyp von type2 ist, sonst nicht bekannt
            ((eq (first type1) 'AND)
             (dolist (type (rest type1) (unknown))
               (when (subtypep type type2) (return (yes)))
            ))
    ) )
    (when (and (symbolp type1) (get type1 'DEFSTRUCT-DESCRIPTION)
               (symbolp type2) (get type2 'DEFSTRUCT-DESCRIPTION)
          )
      (let ((inclist1 (svref (get type1 'DEFSTRUCT-DESCRIPTION) 0))
            (inclist2 (svref (get type2 'DEFSTRUCT-DESCRIPTION) 0)))
        (loop
          (when (eq inclist1 inclist2) (return (yes)))
          (when (atom inclist1) (return))
          (setq inclist1 (cdr inclist1))
      ) )
    )
    (when (or (clos::class-p type1) (clos::class-p type2))
      (if (and (clos::class-p type1) (clos::class-p type2) (clos::subclassp type1 type2))
        (yes)
        (no)
    ) )
    (when (atom type1) (setq type1 (list type1)))
    (case (first type1)
      ((ARRAY SIMPLE-ARRAY)
        (macrolet ((array-p (type)
                     `(or (eq ,type 'ARRAY) (eq ,type (first type1)))
                  ))
          (let ((el-type1 (if (rest type1) (second type1) '*))
                (dims1 (if (cddr type1) (third type1) '*)))
            (values
              (cond ((array-p type2) t)
                    ((and (consp type2) (array-p (first type2)))
                     (let ((el-type2 (if (rest type2) (second type2) '*))
                           (dims2 (if (cddr type2) (third type2) '*)))
                       (and (or (eq el-type2 '*)
                                (and (not (eq el-type1 '*))
                                     (equal (upgraded-array-element-type el-type1)
                                            (upgraded-array-element-type el-type2)
                            )   )    )
                            (or (eq dims2 '*)
                                (if (listp dims1)
                                  (if (listp dims2)
                                    (and (eql (length dims1) (length dims2))
                                         (every #'(lambda (a b) (or (eq b '*) (eql a b)))
                                                dims1 dims2
                                    )    )
                                    (eql (length dims1) dims2)
                                  )
                                  (if (listp dims2)
                                    (and (eql dims1 (length dims2))
                                         (every #'(lambda (b) (eq b '*)) dims2)
                                    )
                                    (eql dims1 dims2)
                    )) )    )   ) )
                    (t nil)
              )
              t
      ) ) ) )
      (COMPLEX
        (let* ((rtype1 (if (rest type1) (second type1) '*))
               (itype1 (if (cddr type1) (third type1) rtype1)))
          (values
            (cond ((or (eq type2 'COMPLEX) (eq type2 'NUMBER)) t)
                  ((and (consp type2) (eq (first type2) 'COMPLEX))
                   (let* ((rtype2 (if (rest type2) (second type2) '*))
                          (itype2 (if (cddr type2) (third type2) rtype2)))
                     (and (or (eq rtype2 '*)
                              (and (not (eq rtype1 '*))
                                   (subtypep rtype1 rtype2)
                          )   )
                          (or (eq itype2 '*)
                              (and (not (eq itype1 '*))
                                   (subtypep itype1 itype2)
                  )) )    )   )
                  (t nil)
            )
            t
      ) ) )
      ((REAL INTEGER RATIONAL FLOAT SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
        (let ((typelist
                (cons (first type1)
                  (case (first type1)
                    (REAL '(NUMBER))
                    (INTEGER '(RATIONAL REAL NUMBER))
                    ((RATIONAL FLOAT) '(REAL NUMBER))
                    ((SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT) '(FLOAT REAL NUMBER))
              ) ) )
              (low1 (if (rest type1) (second type1) '*))
              (high1 (if (cddr type1) (third type1) '*))
              (integer-flag1 (eq (first type1) 'INTEGER))
              (efl t)
              (efh t))
          (when (consp low1)
            (setq low1 (first low1))
            (if integer-flag1 (when (numberp low1) (incf low1)) (setq efl nil))
          )
          (when (consp high1)
            (setq high1 (first high1))
            (if integer-flag1 (when (numberp high1) (decf high1)) (setq efh nil))
          )
          ; efl gibt an, ob low1 zu type1 dazugehört.
          ; efh gibt an, ob high1 zu type1 dazugehört.
          (cond ((and (numberp low1) (numberp high1)
                      (not (or (< low1 high1) (and (= low1 high1) efl efh)))
                 ) ; type1 leer?
                 (yes)
                )
                ((member type2 typelist) (yes))
                ((and (consp type2) (member (first type2) typelist))
                 (let ((low2 (if (rest type2) (second type2) '*))
                       (high2 (if (cddr type2) (third type2) '*))
                       (integer-flag2 (eq (first type2) 'INTEGER)))
                   (if (consp low2)
                     (progn (setq low2 (first low2))
                            (when integer-flag2 (when (numberp low2) (incf low2)) (setq efl nil))
                     )
                     (setq efl nil)
                   )
                   (if (consp high2)
                     (progn (setq high2 (first high2))
                            (when integer-flag2 (when (numberp high2) (decf high2)) (setq efh nil))
                     )
                     (setq efh nil)
                   )
                   ; efl gibt an, ob low1 zu type1 dazugehört und low2 zu type2 nicht dazugehört.
                   ; efh gibt an, ob high1 zu type1 dazugehört und high2 zu type2 nicht dazugehört.
                   (values
                     (and (or (eq low2 '*)
                              (and (numberp low1)
                                   (if efl (> low1 low2) (>= low1 low2))
                          )   )
                          (or (eq high2 '*)
                              (and (numberp high1)
                                   (if efh (< high1 high2) (<= high1 high2))
                     )    )   )
                     t
                )) )
                ((not integer-flag1) (no))
                ((and (symbolp type2) (not (member type2 '(COMPLEX REAL INTEGER RATIONAL FLOAT SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)))) (no))
                (t (unknown))
      ) ) )
      (CONS
       (let ((cartype1 (if (rest type1) (second type1) '*))
             (cdrtype1 (if (cddr type1) (third type1) '*)))
         (if (eq type2 'CONS)
           (yes)
           (multiple-value-bind (caremptyis caremptyknown)
               (if (eq cartype1 '*) (values nil t) (subtypep cartype1 'NIL))
             (when (and caremptyis caremptyknown) (yes))
             (multiple-value-bind (cdremptyis cdremptyknown)
                 (if (eq cdrtype1 '*) (values nil t) (subtypep cdrtype1 'NIL))
               (when (and cdremptyis cdremptyknown) (yes))
               (if (and (consp type2) (eq (car type2) 'CONS))
                 (let ((cartype2 (if (rest type2) (second type2) '*))
                       (cdrtype2 (if (cddr type2) (third type2) '*)))
                   (multiple-value-bind (caris carknown)
                       (if (eq cartype2 '*)
                         (values t t)
                         (subtypep (if (eq cartype1 '*) 'T cartype1) cartype2)
                       )
                     (when (and carknown (not caris) cdremptyknown) (no))
                     (multiple-value-bind (cdris cdrknown)
                         (if (eq cdrtype2 '*)
                           (values t t)
                           (subtypep (if (eq cdrtype1 '*) 'T cdrtype1) cdrtype2)
                         )
                       (when (and cdrknown (not cdris) caremptyknown) (no))
                       (when (and caris carknown cdris cdrknown) (yes))
                       (unknown)
                 ) ) )
                 (if (and caremptyknown cdremptyknown) (no) (unknown))
      )) ) ) ) )
      ((CHARACTER ENCODING FUNCTION HASH-TABLE PACKAGE PATHNAME RANDOM-STATE
        READTABLE STREAM SYMBOL)
       (no)
      )
      (CLOS:GENERIC-FUNCTION
       (if (eq type2 'FUNCTION) (yes) (no))
      )
      (CLOS:STANDARD-GENERIC-FUNCTION
       (if (or (eq type2 'CLOS:GENERIC-FUNCTION) (eq type2 'FUNCTION)) (yes) (no))
      )
      #+LOGICAL-PATHNAMES
      (LOGICAL-PATHNAME (if (eq type2 'PATHNAME) (yes) (no)))
      ((FILE-STREAM SYNONYM-STREAM BROADCAST-STREAM CONCATENATED-STREAM
        TWO-WAY-STREAM ECHO-STREAM STRING-STREAM)
       (if (eq type2 'STREAM) (yes) (no))
      )
      (t
       (if (encodingp (first type1))
         (cond ((eq type2 'CHARACTER) (yes))
               ((encodingp type2) (if (charset-subtypep (first type1) type2) (yes) (no)))
               (t (no))
         )
         (unknown)
      ))
) ) )

#-UNICODE
(defun charset-subtypep (encoding1 encoding2)
  t
)
#+UNICODE
(let ((table (make-hash-table :test #'equal)))
     ; cache: charset name -> list of intervals #(start1 end1 ... startm endm)
  #| ; Now in C and much more efficient.
  (defun charset-range (encoding start end)
    (setq start (char-code start))
    (setq end (char-code end))
    (let ((intervals '())   ; finished intervals
          (i1 nil) (i2 nil) ; interval being built
          (i start))
      (loop
        (if (charset-typep (code-char i) encoding)
          (if i2 (setq i2 i) (setq i1 i i2 i))
          (if i2 (setq intervals (list* i2 i1 intervals) i1 nil i2 nil))
        )
        (when (eql i end) (return))
        (incf i)
      )
      (when i2 (setq intervals (list* i2 i1 intervals)))
      (map 'simple-string #'code-char (nreverse intervals))
  ) )
  |#
  ; Return the definition range of a character set. If necessary, compute it
  ; and store it in the cache.
  (defun get-charset-range (charset)
    (or (gethash charset table)
        (setf (gethash charset table)
              (charset-range (make-encoding :charset charset) (code-char 0) (code-char (1- char-code-limit)))
  ) )   )
  ; Return the character set of an encoding (a symbol or string).
  (defun encoding-charset (encoding) (sys::%record-ref encoding 1))
  ; Fill the cache.
  (do-external-symbols (sym (find-package "CHARSET"))
    (get-charset-range (encoding-charset (symbol-value sym)))
  )
  ; Test whether all characters encodable in encoding1 are also encodable in
  ; encoding2.
  (defun charset-subtypep (encoding1 encoding2)
    (let* ((intervals1 (get-charset-range (encoding-charset encoding1)))
           (intervals2 (get-charset-range (encoding-charset encoding2)))
           (n1 (length intervals1))
           (n2 (length intervals2))
           (j1 0)  ; grows by 2 from 0 to n1
           (j2 0)) ; grows by 2 from 0 to n2
      (loop
        ; Get next interval from intervals1.
        (when (eql j1 n1) (return-from charset-subtypep t))
        (let ((i1 (schar intervals1 j1)) (i2 (schar intervals1 (+ j1 1))))
          ; Test whether [i1,i2] is contained in intervals2.
          (let (i3 i4)
            (loop
              (when (eql j2 n2)
                ; [i1,i2] not contained in intervals2.
                (return-from charset-subtypep nil)
              )
              (setq i3 (schar intervals2 j2))
              (setq i4 (schar intervals2 (+ j2 1)))
              ; If i3 <= i4 < i1 <= i2, skip the interval [i3,i4].
              (when (char>= i4 i1) (return))
              (incf j2 2)
            )
            (when (char< i1 i3)
              ; i1 not contained in intervals2.
              (return-from charset-subtypep nil)
            )
            (when (char< i4 i2)
              ; i4+1 (in [i1,i2]) not contained in intervals2.
              (return-from charset-subtypep nil)
            )
            ; Now (<= i3 i1) and (<= i2 i4), hence [i1,i2] contained in intervals2.
            (incf j1 2)
  ) ) ) ) )
)

;; Bestimmt zwei Werte low,high so, dass (subtypep type `(INTEGER ,low ,high))
;; gilt und low möglichst groß und high möglichst klein ist.
;; low = * bedeutet -unendlich, high = * bedeutet unendlich.
;; Werte sind NIL,NIL falls (subtypep type 'INTEGER) falsch ist.
;; Wir brauchen diese Funktion nur für MAKE-ARRAY, UPGRADED-ARRAY-ELEMENT-TYPE
;; und OPEN, dürfen also oBdA  type  durch  `(OR ,type (MEMBER 0))  ersetzen.
(defun subtype-integer (type)
  (macrolet ((yes () '(return-from subtype-integer (values low high)))
             (no () '(return-from subtype-integer nil))
             (unknown () '(return-from subtype-integer nil)))
    (setq type (canonicalize-type type))
    (if (consp type)
      (macrolet ((min* (x y) `(if (or (eq ,x '*) (eq ,y '*)) '* (min ,x ,y)))
                 (max* (x y) `(if (or (eq ,x '*) (eq ,y '*)) '* (max ,x ,y))))
        (case (first type)
          (MEMBER ;; MEMBER: alle Elemente müssen vom Typ INTEGER sein
            (let ((low 0) (high 0)) ; oBdA!
              (dolist (x (rest type) (yes))
                (unless (typep x 'INTEGER) (return (no)))
                (setq low (min low x) high (max high x))
          ) ) )
          (OR ;; OR: Jeder Typ muss Subtyp von INTEGER sein
            (let ((low 0) (high 0)) ; oBdA!
              (dolist (type1 (rest type) (yes))
                (multiple-value-bind (low1 high1) (subtype-integer type1)
                  (unless low1 (return (no)))
                  (setq low (min* low low1) high (max* high high1))
          ) ) ) )
          (AND ;; AND: Falls ein Typ Subtyp von INTEGER ist, sonst nicht bekannt
            ;; Hier könnte man die verschiedenen Integer-Subtypen schneiden.
            (dolist (type1 (rest type) (unknown))
              (multiple-value-bind (low high) (subtype-integer type1)
                (when low (return (yes)))
          ) ) )
      ) )
      (setq type (list type))
    )
    (if (eq (first type) 'INTEGER)
      (let ((low (if (rest type) (second type) '*))
            (high (if (cddr type) (third type) '*)))
        (when (consp low)
          (setq low (first low))
          (when (numberp low) (incf low))
        )
        (when (consp high)
          (setq high (first high))
          (when (numberp high) (decf high))
        )
        (when (and (numberp low) (numberp high) (not (<= low high))) ; type leer?
          (setq low 0 high 0)
        )
        (yes)
      )
      (unknown)
) ) )

#| Zu tun:
SUBTYPEP so verbessern, dass
(let ((l '(ARRAY BASE-CHAR BASE-STRING BIT-VECTOR BOOLEAN CHARACTER COMPLEX
           CONS FLOAT FUNCTION CLOS:GENERIC-FUNCTION HASH-TABLE INTEGER LIST
           NULL NUMBER PACKAGE PATHNAME #+LOGICAL-PATHNAMES LOGICAL-PATHNAME
           RANDOM-STATE RATIONAL READTABLE REAL SEQUENCE
           CLOS:STANDARD-GENERIC-FUNCTION STREAM STRING SYMBOL VECTOR
     ))   )
  (dolist (a l)
    (dolist (b l)
      (unless (or (subtypep a b) (subtypep b a))
        (unless (equal (multiple-value-list (subtypep `(AND ,a ,b) 'NIL))
                       '(nil t)
                )
          (print (list a b))
) ) ) ) )
möglichst wenig ausgibt.

Henry Baker:
(defun type-null (x)
  (values (and (eq 'bit (upgraded-array-element-type `(or bit ,x)))
               (not (typep 0 x))
               (not (typep 1 x)))
          t))
(type-null '(and symbol number))
(type-null '(and integer symbol))
(type-null '(and integer character))
(subtypep '(and symbol number) 'nil)
(subtypep '(array t (2 5)) '(or (array t (2 3 4)) (array t (2 4))))
(subtypep '(array t (2 5)) '(not (or (array t (2 3 4)) (array t (2 5)))))
(subtypep '(array t (2 5)) '(not (or (array t (2 3 4)) (array t (2 4)))))
(subtypep '(ratio 2/3 1) '(ratio 4/5 1))
(subtypep '(rational 4/5 1) '(or (member 1) (rational 2/3 (1))))

Calling (equal type1 type2) is wrong: they may be MEMBER or EQL specifiers
referring to circular lists.
|#

;-------------------------------------------------------------------------------

(defun type-expand-1 (typespec &aux f)
  (cond ((symbolp typespec)
         (cond ((or (get typespec 'TYPE-SYMBOL) (get typespec 'TYPE-LIST))
                (values typespec nil)
               )
               ((setq f (get typespec 'DEFTYPE-EXPANDER))
                (values (funcall f (list typespec)) t)
               )
               ((or (get typespec 'DEFSTRUCT-DESCRIPTION)
                    (and (setq f (get typespec 'CLOS::CLOSCLASS))
                         (clos::class-p f)
                         (eq (clos:class-name f) typespec)
                )   )
                (values typespec nil)
               )
               (t (typespec-error 'type-expand-1 typespec))
        ))
        ((and (consp typespec) (symbolp (first typespec)))
         (case (first typespec)
           ((SATISFIES MEMBER EQL NOT AND OR) (values typespec nil))
           (t (cond ((get (first typespec) 'TYPE-LIST) (values typespec nil))
                    ((setq f (get (first typespec) 'DEFTYPE-EXPANDER))
                     (values (funcall f typespec) t)
                    )
                    (t (typespec-error 'type-expand-1 typespec))
        )) )  )
        ((clos::class-p typespec) (values typespec nil))
        (t (typespec-error 'type-expand-1 typespec))
) )

(defun type-expand (typespec)
  (multiple-value-bind (a b) (type-expand-1 typespec)
    (if b
      (loop
        (multiple-value-setq (a b) (type-expand-1 a))
        (unless b (return (values a b)))
      )
      (values typespec nil)
) ) )

;-------------------------------------------------------------------------------
