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

(in-package "EXT")
(export '(type-expand))
(in-package "SYSTEM")

; vorläufig, solange bis clos.lisp geladen wird:
(eval-when (eval)
  (defun clos::built-in-class-p (object) (declare (ignore object)) nil))
(unless (fboundp 'clos::subsclassp)
  (defun clos::subclassp (class1 class2) (declare (ignore class1 class2)) nil)
  (defun clos::class-name (c) (declare (ignore c)) nil)
)

(defun typespec-error (fun type)
  (error-of-type 'error
    (TEXT "~S: invalid type specification ~S")
    fun type
) )

;; ============================================================================

;; return the CLOS class named by TYPESPEC or NIL
(defun clos-class (typespec)
  (let ((cc (get typespec 'CLOS::CLOSCLASS)))
    (when (and cc (clos::class-p cc) (eq (clos:class-name cc) typespec))
      cc)))

;;; TYPEP, CLTL S. 72, S. 42-51
(defun typep (x y &optional env &aux f) ; x = Objekt, y = Typ
  (declare (ignore env))
  (setq y (expand-deftype y))
  (cond
    ((symbolp y)
       (cond ((setq f (get y 'TYPE-SYMBOL)) (funcall f x))
             ((setq f (get y 'TYPE-LIST)) (funcall f x))
             ((setq f (get y 'DEFSTRUCT-DESCRIPTION)) (ds-typep x y f))
             ((setq f (clos-class y)) (clos::subclassp (clos:class-of x) f))
             (t (typespec-error 'typep y))
    )  )
    ((and (consp y) (symbolp (first y)))
       (cond
         ((and (eq (first y) 'SATISFIES) (eql (length y) 2))
            (unless (symbolp (second y))
              (error-of-type 'error
                (TEXT "~S: argument to SATISFIES must be a symbol: ~S")
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
         (t (typespec-error 'typep y))
    )  )
    ((clos::class-p y) (clos::subclassp (clos:class-of x) y))
    ((encodingp y) (charset-typep x y))
    (t (typespec-error 'typep y))
) )

;; ----------------------------------------------------------------------------

(defun upgraded-array-element-type (type &optional environment)
  (declare (ignore environment))
  ;; see array.d
  (case type
    ((BIT) 'BIT)
    ((CHARACTER) 'CHARACTER)
    ((T) 'T)
    ((NIL) 'NIL)
    (t (if (subtypep type 'NIL)
         'NIL
         (multiple-value-bind (low high) (sys::subtype-integer type)
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
                     (t 'T)))
             (if (subtypep type 'CHARACTER)
               'CHARACTER
               'T)))))))

;; ----------------------------------------------------------------------------

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
(def-atomic-type BASE-STRING
  (lambda (x)
    (and (stringp x)
         (eq (array-element-type x)
             #+BASE-CHAR=CHARACTER 'CHARACTER #-BASE-CHAR=CHARACTER 'BASE-CHAR
) ) )    )
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
(def-atomic-type SIMPLE-BASE-STRING
  (lambda (x)
    (and (simple-string-p x)
         (eq (array-element-type x)
             #+BASE-CHAR=CHARACTER 'CHARACTER #-BASE-CHAR=CHARACTER 'BASE-CHAR
) ) )    )
(def-atomic-type SIMPLE-BIT-VECTOR simple-bit-vector-p)
(def-atomic-type SIMPLE-STRING simple-string-p)
(def-atomic-type SIMPLE-VECTOR simple-vector-p)
(def-atomic-type SINGLE-FLOAT single-float-p)
(defun %standard-char-p (x) (and (characterp x) (standard-char-p x)))
(def-atomic-type STANDARD-CHAR %standard-char-p)
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
(def-atomic-type T (lambda (x) (declare (ignore x)) t))
;; foreign1.lisp is loaded after this file,
;; so these symbols are not external yet
#+ffi
(def-atomic-type ffi::foreign-function
  (lambda (x) (eq 'ffi::foreign-function (type-of x))))
#+ffi
(def-atomic-type ffi::foreign-variable
  (lambda (x) (eq 'ffi::foreign-variable (type-of x))))
#+ffi
(def-atomic-type ffi::foreign-address
  (lambda (x) (eq 'ffi::foreign-address (type-of x))))
;; see lispbibl.d (#define FOREIGN) and predtype.d (TYPE-OF):
#+(or unix ffi affi win32)
(def-atomic-type foreign-pointer
  (lambda (x) (eq 'foreign-pointer (type-of x))))
(def-atomic-type VECTOR vectorp)
(def-atomic-type PLIST (lambda (x) (and (listp x) (evenp (length x)))))

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
                  (TEXT "~S: argument to ~S must be *, ~S or a list of ~S: ~S")
                  'typep type type type low
       )     )  )
       (cond ((eq high '*))
             ((funcall test high) (>= high x))
             ((and (consp high) (null (rest high)) (funcall test (first high)))
                (> (first high) x)
             )
             (t (error-of-type 'error
                  (TEXT "~S: argument to ~S must be *, ~S or a list of ~S: ~S")
                  'typep type type type high
) )    )     )  )
(defun c-typep-number (caller tester low high x)
  `(AND (,tester ,x)
        ,@(cond ((eq low '*) '())
                ((funcall tester low) `((<= ,low ,x)))
                ((and (consp low) (null (rest low)) (funcall tester (first low)))
                 `((< ,(first low) ,x))
                )
                (t (c-warn (TEXT "~S: argument to ~S must be *, ~S or a list of ~S: ~S")
                           'typep caller caller caller low
                   )
                   (throw 'c-TYPEP nil)
          )     )
        ,@(cond ((eq high '*) '())
                ((funcall tester high) `((>= ,high ,x)))
                ((and (consp high) (null (rest high)) (funcall tester (first high)))
                 `((> ,(first high) ,x))
                )
                (t (c-warn (TEXT "~S: argument to ~S must be *, ~S or a list of ~S: ~S")
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
       (or (eq rtype '*)
           (typep (realpart x) (upgraded-complex-part-type rtype)))
       (or (eq itype '*)
           (typep (imagpart x) (upgraded-complex-part-type itype))))
  `(AND (COMPLEXP ,x)
        ,@(if (eq rtype '*)
            '()
            `((TYPEP (REALPART ,x) ',(upgraded-complex-part-type rtype))))
        ,@(if (eq itype '*)
            '()
            `((TYPEP (IMAGPART ,x) ',(upgraded-complex-part-type itype))))))
(def-compound-type INTEGER (&optional (low '*) (high '*)) (x)
  nil
  (typep-number-test x low high #'integerp 'INTEGER)
  (c-typep-number 'INTEGER 'INTEGERP low high x)
)
(def-compound-type MOD (n) (x)
  (unless (integerp n)
    (error (TEXT "~S: argument to MOD must be an integer: ~S")
           'typep n
  ) )
  (and (integerp x) (<= 0 x) (< x n))
  `(AND (INTEGERP ,x) (NOT (MINUSP ,x)) (< ,x ,n))
)
(def-compound-type SIGNED-BYTE (&optional (n '*)) (x)
  (unless (or (eq n '*) (integerp n))
    (error (TEXT "~S: argument to SIGNED-BYTE must be an integer or * : ~S")
           'typep n
  ) )
  (and (integerp x) (or (eq n '*) (< (integer-length x) n)))
  `(AND (INTEGERP ,x)
        ,@(if (eq n '*) '() `((< (INTEGER-LENGTH ,x) ,n)))
   )
)
(def-compound-type UNSIGNED-BYTE (&optional (n '*)) (x)
  (unless (or (eq n '*) (integerp n))
    (error (TEXT "~S: argument to UNSIGNED-BYTE must be an integer or * : ~S")
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

;; ----------------------------------------------------------------------------

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
                    (or (typep ,g ,typform) (eq (type-of ,g) 'READ-LABEL))))))
    (if (and (consp type) (eq (car type) 'VALUES))
      (let ((vals values) (types (cdr type)))
        ;; required:
        (loop
          (when (or (atom types) (atom vals)) (return-from %the t))
          (when (memq (car types) lambda-list-keywords) (return))
          (unless (near-typep (pop vals) (pop types))
            (return-from %the nil)))
        ;; &optional:
        (when (and (consp types) (eq (car types) '&optional))
          (setq types (cdr types))
          (loop
            (when (or (atom types) (atom vals)) (return-from %the t))
            (when (memq (car types) lambda-list-keywords) (return))
            (unless (near-typep (pop vals) (pop types))
              (return-from %the nil))))
        ;; &rest &key:
        (case (car types)
          (&rest
           (setq types (cdr types))
           (when (atom types) (typespec-error 'the type))
           (unless (near-typep (pop vals) (pop types))
             (return-from %the nil)))
          (&key)
          (t (typespec-error 'the type)))
        (if (eq (car types) '&key)
          (progn
            (setq types (cdr types))
            (when (oddp (length vals)) (return-from %the nil))
            (let ((keywords nil))
              (loop
                (when (or (atom types) (atom vals)) (return-from %the t))
                (when (memq (car types) lambda-list-keywords) (return))
                (let ((item (pop types)))
                  (unless (and (listp item) (eql (length item) 2)
                               (symbolp (first item)))
                    (typespec-error 'the type))
                  (let ((kw (intern (symbol-name (first item))
                                    *keyword-package*)))
                    (unless (near-typep (getf vals kw) (second item))
                      (return-from %the nil))
                    (push kw keywords))))
              (if (and (consp types) (eq (car types) '&allow-other-keys))
                (setq types (cdr types))
                (unless (getf vals ':allow-other-keys)
                  (do ((L vals (cddr L)))
                      ((atom L))
                    (unless (memq (car L) keywords)
                      (return-from %the nil)))))))
          (when (consp types) (typespec-error 'the type)))
        t)
      (near-typep (if (consp values) (car values) nil) type))))

;;; ===========================================================================

;; SUBTYPEP
(load "subtypep")

;; this function does not return anything useful,
;; since CLISP complex numbers can always hold any number
(defun upgraded-complex-part-type (spec)
  (if (subtypep spec 'real)
    'real
    (error-of-type 'error
      (TEXT "~S: type ~S is not a subtype of ~S")
      'upgraded-complex-part-type spec 'real)))


;; Returns the number of bytes that are needed to represent #\Null in a
;; given encoding.
(defun encoding-zeroes (encoding)
  #+UNICODE
  ;; this should use min_bytes_per_char for cache, not the hash table
  (let ((name (ext:encoding-charset encoding))
        (table #.(make-hash-table :test #'equal
                                  :initial-contents '(("UTF-7" . 1))))
        (tester #.(make-string 2 :initial-element (code-char 0))))
    (or (gethash name table)
        (setf (gethash name table)
              (- (length (ext:convert-string-to-bytes tester encoding))
                 (length (ext:convert-string-to-bytes tester encoding
                                                      :end 1))))))
  #-UNICODE 1)

;; Determines two values low,high such that
;;   (subtypep type `(INTEGER ,low ,high))
;; holds and low is as large as possible and high is as small as possible.
;; low = * means -infinity, high = * means infinity.
;; When (subtypep type 'INTEGER) is false, the values NIL,NIL are returned.
;; We need this function only for MAKE-ARRAY, UPGRADED-ARRAY-ELEMENT-TYPE and
;; OPEN and can therefore w.l.o.g. replace
;;   type  with  `(OR ,type (MEMBER 0))
#| ;; The original implementation calls canonicalize-type and then applies
   ;; a particular SUBTYPE variant:
 (defun subtype-integer (type)
  (macrolet ((yes () '(return-from subtype-integer (values low high)))
             (no () '(return-from subtype-integer nil))
             (unknown () '(return-from subtype-integer nil)))
    (setq type (canonicalize-type type))
    (if (consp type)
      (case (first type)
        (MEMBER ; (MEMBER &rest objects)
          ;; All elements must be of type INTEGER.
          (let ((low 0) (high 0)) ; wlog!
            (dolist (x (rest type) (yes))
              (unless (typep x 'INTEGER) (return (no)))
              (setq low (min low x) high (max high x)))))
        (OR ; (OR type*)
          ;; Every type must be subtype of INTEGER.
          (let ((low 0) (high 0)) ; wlog!
            (dolist (type1 (rest type) (yes))
              (multiple-value-bind (low1 high1) (subtype-integer type1)
                (unless low1 (return (no)))
                (setq low (if (or (eq low '*) (eq low1 '*)) '* (min low low1))
                      high (if (or (eq high '*) (eq high1 '*))
                               '* (max high high1)))))))
        (AND ; (AND type*)
          ;; If one of the types is subtype of INTEGER, then yes,
          ;; otherwise unknown.
          (let ((low nil) (high nil))
            (dolist (type1 (rest type))
              (multiple-value-bind (low1 high1) (subtype-integer type1)
                (when low1
                  (if low
                    (setq low (if (eq low '*) low1 (if (eq low1 '*) low (max low low1)))
                          high (if (eq high '*) high1 (if (eq high1 '*) high (min high high1))))
                    (setq low low1 high high1)))))
            (if low
              (progn
                (when (and (numberp low) (numberp high) (not (<= low high)))
                  (setq low 0 high 0) ; type equivalent to NIL)
                (yes))
              (unknown)))))
      (setq type (list type)))
    (if (eq (first type) 'INTEGER)
      (let ((low (if (rest type) (second type) '*))
            (high (if (cddr type) (third type) '*)))
        (when (consp low)
          (setq low (first low))
          (when (numberp low) (incf low)))
        (when (consp high)
          (setq high (first high))
          (when (numberp high) (decf high)))
        (when (and (numberp low) (numberp high) (not (<= low high))) ; type leer?
          (setq low 0 high 0))
        (yes))
      (if (and (eq (first type) 'INTERVALS) (eq (second type) 'INTEGER))
        (let ((low (third type))
              (high (car (last type))))
          (when (consp low)
            (setq low (first low))
            (when (numberp low) (incf low)))
          (when (consp high)
            (setq high (first high))
            (when (numberp high) (decf high)))
          (yes))
        (unknown)))))
|# ;; This implementation inlines the (tail-recursive) canonicalize-type
   ;; function. Its advantage is that it doesn't cons as much.
   ;; (For example, (subtype-integer '(UNSIGNED-BYTE 8)) doesn't cons.)
(defun subtype-integer (type)
  (macrolet ((yes () '(return-from subtype-integer (values low high)))
             (no () '(return-from subtype-integer nil))
             (unknown () '(return-from subtype-integer nil)))
    (setq type (expand-deftype type))
    (cond ((symbolp type)
           (case type
             (BIT (let ((low 0) (high 1)) (yes)))
             (FIXNUM
              (let ((low '#,most-negative-fixnum)
                    (high '#,most-positive-fixnum))
                (yes)))
             ((INTEGER BIGNUM SIGNED-BYTE)
              (let ((low '*) (high '*)) (yes)))
             (UNSIGNED-BYTE
              (let ((low 0) (high '*)) (yes)))
             ((NIL)
              (let ((low 0) (high 0)) (yes))) ; wlog!
             (t (no))))
          ((and (consp type) (symbolp (first type)))
           (case (first type)
             (MEMBER ; (MEMBER &rest objects)
              ;; All elements must be of type INTEGER.
              (let ((low 0) (high 0)) ; wlog!
                (dolist (x (rest type) (yes))
                  (unless (typep x 'INTEGER) (return (no)))
                  (setq low (min low x) high (max high x)))))
             (EQL ; (EQL object)
              (let ((x (second type)))
                (if (typep x 'INTEGER)
                  (let ((low (min 0 x)) (high (max 0 x))) (yes))
                  (no))))
             (OR ; (OR type*)
              ;; Every type must be subtype of INTEGER.
              (let ((low 0) (high 0)) ; wlog!
                (dolist (type1 (rest type) (yes))
                  (multiple-value-bind (low1 high1) (subtype-integer type1)
                    (unless low1 (return (no)))
                    (setq low (if (or (eq low '*) (eq low1 '*))
                                  '* (min low low1))
                          high (if (or (eq high '*) (eq high1 '*))
                                   '* (max high high1)))))))
             (AND ; (AND type*)
              ;; If one of the types is subtype of INTEGER, then yes,
              ;; otherwise unknown.
              (let ((low nil) (high nil))
                (dolist (type1 (rest type))
                  (multiple-value-bind (low1 high1) (subtype-integer type1)
                    (when low1
                      (if low
                        (setq low (if (eq low '*) low1
                                      (if (eq low1 '*) low
                                          (max low low1)))
                              high (if (eq high '*) high1
                                       (if (eq high1 '*) high
                                           (min high high1))))
                        (setq low low1
                              high high1)))))
                (if low
                  (progn
                    (when (and (numberp low) (numberp high)
                               (not (<= low high)))
                      (setq low 0 high 0)) ; type equivalent to NIL
                    (yes))
                  (unknown))))
             (INTEGER
              (let ((low (if (rest type) (second type) '*))
                    (high (if (cddr type) (third type) '*)))
                (when (consp low)
                  (setq low (first low))
                  (when (numberp low) (incf low)))
                (when (consp high)
                  (setq high (first high))
                  (when (numberp high) (decf high)))
                (when (and (numberp low) (numberp high) (not (<= low high)))
                  (setq low 0 high 0)) ; type equivalent to NIL
                (yes)))
             (INTERVALS
              (if (eq (second type) 'INTEGER)
                (let ((low (third type))
                      (high (car (last type))))
                  (when (consp low)
                    (setq low (first low))
                    (when (numberp low) (incf low)))
                  (when (consp high)
                    (setq high (first high))
                    (when (numberp high) (decf high)))
                  (yes))
                (unknown)))
             (MOD ; (MOD n)
              (let ((n (second type)))
                (unless (and (integerp n) (>= n 0))
                  (typespec-error 'subtypep type))
                (if (eql n 0)
                  (no)
                  (let ((low 0) (high (1- n)))
                    (yes)))))
             (SIGNED-BYTE ; (SIGNED-BYTE &optional s)
              (let ((s (if (cdr type) (second type) '*)))
                (if (eq s '*)
                  (let ((low '*) (high '*)) (yes))
                  (progn
                    (unless (and (integerp s) (plusp s))
                      (typespec-error 'subtypep type))
                    (let ((n (ash 1 (1- s)))) ; (ash 1 *) == (expt 2 *)
                      (let ((low (- n)) (high (1- n)))
                        (yes)))))))
             (UNSIGNED-BYTE ; (UNSIGNED-BYTE &optional s)
              (let ((s (if (cdr type) (second type) '*)))
                (if (eq s '*)
                    (let ((low 0) (high '*)) (yes))
                    (progn
                      (unless (and (integerp s) (>= s 0))
                        (typespec-error 'subtypep type))
                      (let ((n (ash 1 s))) ; (ash 1 *) == (expt 2 *)
                        (let ((low 0) (high (1- n)))
                          (yes)))))))
             (t (no))))
          ((clos::class-p type)
           (if (and (clos::built-in-class-p type)
                    (eq (get (clos:class-name type) 'CLOS::CLOSCLASS) type))
             (return-from subtype-integer
               (subtype-integer (clos:class-name type)))
             (no)))
          ((encodingp type) (no))
          (t (typespec-error 'subtypep type)))))

#| TODO: Fix subtype-integer such that this works.
Henry Baker:
 (defun type-null (x)
  (values (and (eq 'bit (upgraded-array-element-type `(or bit ,x)))
               (not (typep 0 x))
               (not (typep 1 x)))
          t))
 (type-null '(and symbol number))
 (type-null '(and integer symbol))
 (type-null '(and integer character))
|#

;; ============================================================================

(defun type-expand (typespec &optional once-p)
  (multiple-value-bind (expanded user-defined-p)
      (expand-deftype typespec once-p)
    (if user-defined-p (values expanded user-defined-p)
      (cond ((symbolp typespec)
             (cond ((or (get typespec 'TYPE-SYMBOL) (get typespec 'TYPE-LIST))
                    (values typespec nil))
                   ((or (get typespec 'DEFSTRUCT-DESCRIPTION)
                        (clos-class typespec))
                    (values typespec nil))
                   (t (typespec-error 'type-expand typespec))))
            ((and (consp typespec) (symbolp (first typespec)))
             (case (first typespec)
               ((SATISFIES MEMBER EQL NOT AND OR) (values typespec nil))
               (t (cond ((get (first typespec) 'TYPE-LIST)
                         (values typespec nil))
                        (t (typespec-error 'type-expand typespec))))))
            ((clos::class-p typespec) (values typespec nil))
            (t (typespec-error 'type-expand typespec))))))

;; ============================================================================

(unless (clos::generic-function-p #'clos::class-name)
  (fmakunbound 'clos::class-name))
