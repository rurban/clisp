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
    (ENGLISH "~S: invalid type specification ~S")
    fun type
) )

;===============================================================================

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
                (ENGLISH "~S: argument to SATISFIES must be a symbol: ~S")
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

;-------------------------------------------------------------------------------

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

;-------------------------------------------------------------------------------

;; Macros for defining the various built-in "atomic type specifier"s and
;; "compound type specifier"s. The following macros add information for both
;; the TYPEP function above and the c-TYPEP in the compiler.

; Alist symbol -> funname, used by the compiler.
(defparameter c-typep-alist1 '())
; Alist symbol -> lambdabody, used by the compiler.
(defparameter c-typep-alist2 '())
; Alist symbol -> expander function, used by the compiler.
(defparameter c-typep-alist3 '())

; (def-atomic-type symbol function-name)
; defines an atomic type. The function-name designates a function taking one
; argument and returning a generalized boolean value. It can be either a
; symbol or a lambda expression.
(defmacro def-atomic-type (symbol funname)
  (let ((lambdap (and (consp funname) (eq (car funname) 'LAMBDA))))
    `(PROGN
       (SETF (GET ',symbol 'TYPE-SYMBOL)
             ,(if lambdap
                `(FUNCTION ,(concat-pnames "TYPE-SYMBOL-" symbol) ,funname)
                `(FUNCTION ,funname)
              )
       )
       ,(if lambdap
          `(SETQ C-TYPEP-ALIST2
                 (NCONC C-TYPEP-ALIST2 (LIST (CONS ',symbol ',(cdr funname))))
           )
          `(SETQ C-TYPEP-ALIST1
                 (NCONC C-TYPEP-ALIST1 (LIST (CONS ',symbol ',funname)))
           )
        )
       ',symbol
     )
) )

; (def-compound-type symbol lambda-list (x) check-form typep-form c-typep-form)
; defines a compound type. The lambda-list is of the form (&optional ...)
; where the arguments come from the CDR of the type specifier.
; For typep-form, x is an object.
; For c-typep-form, x is a multiply evaluatable form (actually a gensym).
; check-form is a form performing error checking, may call `error'.
; typep-form should return a generalized boolean value.
; c-typep-form should produce a form returning a generalized boolean value.
(defmacro def-compound-type (symbol lambdalist (var) check-form typep-form c-typep-form)
  `(PROGN
     (SETF (GET ',symbol 'TYPE-LIST)
           (FUNCTION ,(concat-pnames "TYPE-LIST-" symbol)
             (LAMBDA (,var ,@lambdalist)
               ,@(if check-form
                   `((MACROLET ((ERROR (&REST ERROR-ARGS)
                                  (LIST* 'ERROR-OF-TYPE ''ERROR ERROR-ARGS)
                               ))
                       ,check-form
                    ))
                 )
               ,typep-form
     )     ) )
     (SETQ C-TYPEP-ALIST3
           (NCONC C-TYPEP-ALIST3
                  (LIST (CONS ',symbol
                              #'(LAMBDA (,var ,@lambdalist &REST ILLEGAL-ARGS)
                                  (DECLARE (IGNORE ILLEGAL-ARGS))
                                  ,@(if check-form
                                      `((MACROLET ((ERROR (&REST ERROR-ARGS)
                                                     (LIST 'PROGN
                                                           (LIST* 'C-WARN ERROR-ARGS)
                                                           '(THROW 'C-TYPEP NIL)
                                                  )) )
                                          ,check-form
                                       ))
                                    )
                                  ,c-typep-form
                                )
     )     )      )     )
     ',symbol
   )
)

; CLtL1 p. 43
(def-atomic-type ARRAY arrayp)
(def-atomic-type ATOM atom)
(def-atomic-type BASE-CHAR
  #+BASE-CHAR=CHARACTER
  characterp
  #-BASE-CHAR=CHARACTER
  (lambda (x) (and (characterp x) (base-char-p x)))
)
(def-atomic-type BASE-STRING stringp)
(def-atomic-type BIGNUM
  (lambda (x) (and (integerp x) (not (fixnump x))))
)
(def-atomic-type BIT
  (lambda (x) (or (eql x 0) (eql x 1)))
)
(def-atomic-type BIT-VECTOR bit-vector-p)
(def-atomic-type BOOLEAN
  (lambda (x) (or (eq x 'nil) (eq x 't)))
)
(def-atomic-type CHARACTER characterp)
(def-atomic-type COMMON commonp)
(def-atomic-type COMPILED-FUNCTION compiled-function-p)
(def-atomic-type COMPLEX complexp)
(def-atomic-type CONS consp)
(def-atomic-type DOUBLE-FLOAT double-float-p)
(def-atomic-type ENCODING encodingp)
(def-atomic-type EXTENDED-CHAR
  #+BASE-CHAR=CHARACTER
  (lambda (x) (declare (ignore x)) nil)
  #-BASE-CHAR=CHARACTER
  (lambda (x) (and (characterp x) (not (base-char-p x))))
)
(def-atomic-type FIXNUM fixnump)
(def-atomic-type FLOAT floatp)
(def-atomic-type FUNCTION functionp)
(def-atomic-type CLOS:GENERIC-FUNCTION clos::generic-function-p)
(def-atomic-type HASH-TABLE hash-table-p)
(def-atomic-type INTEGER integerp)
(def-atomic-type KEYWORD keywordp)
(def-atomic-type LIST listp)
#+LOGICAL-PATHNAMES
(def-atomic-type LOGICAL-PATHNAME logical-pathname-p)
(def-atomic-type LONG-FLOAT long-float-p)
(def-atomic-type NIL
  (lambda (x) (declare (ignore x)) nil)
)
(def-atomic-type NULL null)
(def-atomic-type NUMBER numberp)
(def-atomic-type PACKAGE packagep)
(def-atomic-type PATHNAME pathnamep)
(def-atomic-type RANDOM-STATE random-state-p)
(def-atomic-type RATIO
  (lambda (x) (and (rationalp x) (not (integerp x))))
)
(def-atomic-type RATIONAL rationalp)
(def-atomic-type READTABLE readtablep)
(def-atomic-type REAL realp)
(def-atomic-type SEQUENCE sequencep)
(def-atomic-type SHORT-FLOAT short-float-p)
(def-atomic-type SIMPLE-ARRAY simple-array-p)
(def-atomic-type SIMPLE-BASE-STRING simple-string-p)
(def-atomic-type SIMPLE-BIT-VECTOR simple-bit-vector-p)
(def-atomic-type SIMPLE-STRING simple-string-p)
(def-atomic-type SIMPLE-VECTOR simple-vector-p)
(def-atomic-type SINGLE-FLOAT single-float-p)
(def-atomic-type STANDARD-CHAR
  (lambda (x) (and (characterp x) (standard-char-p x)))
)
(def-atomic-type CLOS:STANDARD-GENERIC-FUNCTION clos::generic-function-p)
(def-atomic-type CLOS:STANDARD-OBJECT clos::std-instance-p)
(def-atomic-type STREAM streamp)
(def-atomic-type FILE-STREAM file-stream-p)
(def-atomic-type SYNONYM-STREAM synonym-stream-p)
(def-atomic-type BROADCAST-STREAM broadcast-stream-p)
(def-atomic-type CONCATENATED-STREAM concatenated-stream-p)
(def-atomic-type TWO-WAY-STREAM two-way-stream-p)
(def-atomic-type ECHO-STREAM echo-stream-p)
(def-atomic-type STRING-STREAM string-stream-p)
(def-atomic-type STRING stringp)
(def-atomic-type STRING-CHAR characterp)
(def-atomic-type CLOS:STRUCTURE-OBJECT clos::structure-object-p)
(def-atomic-type SYMBOL symbolp)
(def-atomic-type T
  (lambda (x) (declare (ignore x)) t)
)
(def-atomic-type VECTOR vectorp)

; CLtL1 p. 46-50
(defun c-typep-array (tester el-type dims x)
  `(AND (,tester ,x)
        ,@(if (eq el-type '*)
            '()
            `((EQUAL (ARRAY-ELEMENT-TYPE ,x) ',(upgraded-array-element-type el-type)))
          )
        ,@(if (eq dims '*)
            '()
            (if (numberp dims)
              `((EQL ,dims (ARRAY-RANK ,x)))
              `((EQL ,(length dims) (ARRAY-RANK ,x))
                ,@(let ((i 0))
                    (mapcap #'(lambda (dim)
                                (prog1
                                  (if (eq dim '*)
                                    '()
                                    `((EQL ',dim (ARRAY-DIMENSION ,x ,i)))
                                  )
                                  (incf i)
                              ) )
                            dims
                  ) )
               )
          ) )
   )
)
(defun c-typep-vector (tester size x)
  `(AND (,tester ,x)
        ,@(if (eq size '*)
            '()
            `((EQL (ARRAY-DIMENSION ,x 0) ',size))
          )
   )
)
(defun typep-number-test (x low high test type)
  (and (funcall test x)
       (cond ((eq low '*))
             ((funcall test low) (<= low x))
             ((and (consp low) (null (rest low)) (funcall test (first low)))
                (< (first low) x)
             )
             (t (error-of-type 'error
                  (ENGLISH "~S: argument to ~S must be *, ~S or a list of ~S: ~S")
                  'typep type type type low
       )     )  )
       (cond ((eq high '*))
             ((funcall test high) (>= high x))
             ((and (consp high) (null (rest high)) (funcall test (first high)))
                (> (first high) x)
             )
             (t (error-of-type 'error
                  (ENGLISH "~S: argument to ~S must be *, ~S or a list of ~S: ~S")
                  'typep type type type high
) )    )     )  )
(defun c-typep-number (caller tester low high x)
  `(AND (,tester ,x)
        ,@(cond ((eq low '*) '())
                ((funcall tester low) `((<= ,low ,x)))
                ((and (consp low) (null (rest low)) (funcall tester (first low)))
                 `((< ,(first low) ,x))
                )
                (t (c-warn (ENGLISH "~S: argument to ~S must be *, ~S or a list of ~S: ~S")
                           'typep caller caller caller low
                   )
                   (throw 'c-TYPEP nil)
          )     )
        ,@(cond ((eq high '*) '())
                ((funcall tester high) `((>= ,high ,x)))
                ((and (consp high) (null (rest high)) (funcall tester (first high)))
                 `((> ,(first high) ,x))
                )
                (t (c-warn (ENGLISH "~S: argument to ~S must be *, ~S or a list of ~S: ~S")
                           'typep caller caller caller high
                   )
                   (throw 'c-TYPEP nil)
          )     )
   )
)
(def-compound-type ARRAY (&optional (el-type '*) (dims '*)) (x)
  nil
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
  )    )   ) )    )
  (c-typep-array 'ARRAYP el-type dims x)
)
(def-compound-type SIMPLE-ARRAY (&optional (el-type '*) (dims '*)) (x)
  nil
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
  )    )   ) )    )
  (c-typep-array 'SIMPLE-ARRAY-P el-type dims x)
)
(def-compound-type VECTOR (&optional (el-type '*) (size '*)) (x)
  nil
  (and (vectorp x)
       (or (eq el-type '*)
           (equal (array-element-type x) (upgraded-array-element-type el-type))
       )
       (or (eq size '*) (eql (array-dimension x 0) size))
  )
  `(AND (VECTORP ,x)
        ,@(if (eq el-type '*)
            '()
            `((EQUAL (ARRAY-ELEMENT-TYPE ,x) ',(upgraded-array-element-type el-type)))
          )
        ,@(if (eq size '*)
            '()
            `((EQL (ARRAY-DIMENSION ,x 0) ',size))
          )
   )
)
(def-compound-type SIMPLE-VECTOR (&optional (size '*)) (x)
  nil
  (and (simple-vector-p x)
       (or (eq size '*) (eql size (array-dimension x 0)))
  )
  (c-typep-vector 'SIMPLE-VECTOR-P size x)
)
(def-compound-type COMPLEX (&optional (rtype '*) (itype rtype)) (x)
  nil
  (and (complexp x)
       (or (eq rtype '*) (typep (realpart x) rtype))
       (or (eq itype '*) (typep (imagpart x) itype))
  )
  `(AND (COMPLEXP ,x)
        ,@(if (eq rtype '*)
            '()
            `((TYPEP (REALPART ,x) ',rtype))
          )
        ,@(if (eq itype '*)
            '()
            `((TYPEP (IMAGPART ,x) ',itype))
          )
   )
)
(def-compound-type INTEGER (&optional (low '*) (high '*)) (x)
  nil
  (typep-number-test x low high #'integerp 'INTEGER)
  (c-typep-number 'INTEGER 'INTEGERP low high x)
)
(def-compound-type MOD (n) (x)
  (unless (integerp n)
    (error (ENGLISH "~S: argument to MOD must be an integer: ~S")
           'typep n
  ) )
  (and (integerp x) (<= 0 x) (< x n))
  `(AND (INTEGERP ,x) (NOT (MINUSP ,x)) (< ,x ,n))
)
(def-compound-type SIGNED-BYTE (&optional (n '*)) (x)
  (unless (or (eq n '*) (integerp n))
    (error (ENGLISH "~S: argument to SIGNED-BYTE must be an integer or * : ~S")
           'typep n
  ) )
  (and (integerp x) (or (eq n '*) (< (integer-length x) n)))
  `(AND (INTEGERP ,x)
        ,@(if (eq n '*) '() `((< (INTEGER-LENGTH ,x) ,n)))
   )
)
(def-compound-type UNSIGNED-BYTE (&optional (n '*)) (x)
  (unless (or (eq n '*) (integerp n))
    (error (ENGLISH "~S: argument to UNSIGNED-BYTE must be an integer or * : ~S")
           'typep n
  ) )
  (and (integerp x)
       (not (minusp x))
       (or (eq n '*) (<= (integer-length x) n))
  )
  `(AND (INTEGERP ,x) (NOT (MINUSP ,x))
        ,@(if (eq n '*) '() `((<= (INTEGER-LENGTH ,x) ,n)))
   )
)
(def-compound-type REAL (&optional (low '*) (high '*)) (x)
  nil
  (typep-number-test x low high #'realp 'REAL)
  (c-typep-number 'REAL 'REALP low high x)
)
(def-compound-type RATIONAL (&optional (low '*) (high '*)) (x)
  nil
  (typep-number-test x low high #'rationalp 'RATIONAL)
  (c-typep-number 'RATIONAL 'RATIONALP low high x)
)
(def-compound-type FLOAT (&optional (low '*) (high '*)) (x)
  nil
  (typep-number-test x low high #'floatp 'FLOAT)
  (c-typep-number 'FLOAT 'FLOATP low high x)
)
(def-compound-type SHORT-FLOAT (&optional (low '*) (high '*)) (x)
  nil
  (typep-number-test x low high #'short-float-p 'SHORT-FLOAT)
  (c-typep-number 'SHORT-FLOAT 'SHORT-FLOAT-P low high x)
)
(def-compound-type SINGLE-FLOAT (&optional (low '*) (high '*)) (x)
  nil
  (typep-number-test x low high #'single-float-p 'SINGLE-FLOAT)
  (c-typep-number 'SINGLE-FLOAT 'SINGLE-FLOAT-P low high x)
)
(def-compound-type DOUBLE-FLOAT (&optional (low '*) (high '*)) (x)
  nil
  (typep-number-test x low high #'double-float-p 'DOUBLE-FLOAT)
  (c-typep-number 'DOUBLE-FLOAT 'DOUBLE-FLOAT-P low high x)
)
(def-compound-type LONG-FLOAT (&optional (low '*) (high '*)) (x)
  nil
  (typep-number-test x low high #'long-float-p 'LONG-FLOAT)
  (c-typep-number 'LONG-FLOAT 'LONG-FLOAT-P low high x)
)
(def-compound-type STRING (&optional (size '*)) (x)
  nil
  (and (stringp x)
       (or (eq size '*) (eql size (array-dimension x 0)))
  )
  (c-typep-vector 'STRINGP size x)
)
(def-compound-type SIMPLE-STRING (&optional (size '*)) (x)
  nil
  (and (simple-string-p x)
       (or (eq size '*) (eql size (array-dimension x 0)))
  )
  (c-typep-vector 'SIMPLE-STRING-P size x)
)
(def-compound-type BASE-STRING (&optional (size '*)) (x)
  nil
  (and (stringp x)
       (or (eq size '*) (eql size (array-dimension x 0)))
  )
  (c-typep-vector 'STRINGP size x)
)
(def-compound-type SIMPLE-BASE-STRING (&optional (size '*)) (x)
  nil
  (and (simple-string-p x)
       (or (eq size '*) (eql size (array-dimension x 0)))
  )
  (c-typep-vector 'SIMPLE-STRING-P size x)
)
(def-compound-type BIT-VECTOR (&optional (size '*)) (x)
  nil
  (and (bit-vector-p x)
       (or (eq size '*) (eql size (array-dimension x 0)))
  )
  (c-typep-vector 'BIT-VECTOR-P size x)
)
(def-compound-type SIMPLE-BIT-VECTOR (&optional (size '*)) (x)
  nil
  (and (simple-bit-vector-p x)
       (or (eq size '*) (eql size (array-dimension x 0)))
  )
  (c-typep-vector 'SIMPLE-BIT-VECTOR-P size x)
)
(def-compound-type CONS (&optional (car-type '*) (cdr-type '*)) (x)
  nil
  (and (consp x)
       (or (eq car-type '*) (typep (car x) car-type))
       (or (eq cdr-type '*) (typep (cdr x) cdr-type))
  )
  `(AND (CONSP ,x)
        ,@(if (eq car-type '*) '() `((TYPEP (CAR ,x) ',car-type)))
        ,@(if (eq cdr-type '*) '() `((TYPEP (CDR ,x) ',cdr-type)))
   )
)

(fmakunbound 'def-atomic-type)
(fmakunbound 'def-compound-type)

;-------------------------------------------------------------------------------

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
                      (ENGLISH "Invalid type specifier ~S")
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

;===============================================================================

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
           (case (sys::%record-ref type 3) ; encoding-charset
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
  (defun encoding-charset (encoding) (sys::%record-ref encoding 3))
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

;===============================================================================

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
        (unless b (return (values a t)))
      )
      (values typespec nil)
) ) )

;===============================================================================
