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

;; find the smallest CLOS class containing this TYPESPEC
(defun type-class (typespec)
  (or (cond ((symbolp typespec) (clos-class typespec))
            ((and (consp typespec) (symbolp (car typespec)))
             (case (car typespec)
               ((SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
                (clos-class 'float))
               ((ARRAY SIMPLE-ARRAY)
                (let ((eltype (if (consp (cdr typespec)) (cadr typespec) '*))
                      (dims (if (consp (cddr typespec)) (caddr typespec) '*)))
                  (if (and (consp dims) (null (cdr dims))) ; vector
                      (case eltype
                        ((BIT) (clos-class 'bit-vector))
                        ((CHARACTER BASE-CHAR) (clos-class 'string))
                        (T (clos-class 'vector)))
                      (clos-class 'array))))
               (t ; handle some "canonicalizations"
                (multiple-value-bind (type foundp)
                    (gethash typespec
                             #.(make-hash-table
                                :test 'equal :initial-contents
                                '(((OR CONS (MEMBER NIL)) . LIST)
                                  ((OR REAL COMPLEX) . NUMBER)
                                  ((OR LIST VECTOR) . SEQUENCE)
                                  ((AND RATIONAL (NOT INTEGER)) . RATIO)
                                  ((MEMBER NIL) . NULL))))
                  (if foundp (get type 'CLOS::CLOSCLASS)
                      (clos-class (car typespec))))))))
      typespec))

;;; TYPEP, CLTL S. 72, S. 42-51
(defun typep (x y &optional env &aux f) ; x = Objekt, y = Typ
  (declare (ignore env))
  (setq y (expand-deftype y))
  (cond
    ((symbolp y)
       (cond ((setq f (get y 'TYPE-SYMBOL)) (funcall f x))
             ((setq f (get y 'TYPE-LIST)) (funcall f x))
             ((get y 'DEFSTRUCT-DESCRIPTION) (%STRUCTURE-TYPE-P y x))
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

(defun valid-interval-designator-p (hilo test)
  "check whether the first argument is a valid interval designator"
  (or (eq hilo '*) (funcall test hilo)
      (and (consp hilo) (null (cdr hilo))
           (funcall test (car hilo)))))

(defmacro with-interval-designators ((&rest vars) &body body)
  "(with-interval-designators ((interval exclusive-p number) ...) ...)"
  `(let* ,(mapcan #'(lambda (var)
                      (let* ((i-d (pop var)) (excl-p (pop var)) (num (pop var))
                             (v-i-d (gensym "INTERVAL-")))
                        `((,v-i-d ,i-d) (,excl-p (consp ,v-i-d))
                          (,num (if ,excl-p (car ,v-i-d) ,v-i-d)))))
                  vars)
     ,@body))

(defun interval-mix (lo1 hi1 lo2 hi2 type head)
  "join (and/or) intervals; when disjoint or not intersecting, return nil
head: AND/OR: (head (real lo1 hi1) (real lo2 hi2))"
  ;; (int [lo1 hi1] [lo2 hi2]) ==
  ;; (int [lo1 *] [* hi1] [lo2 *] [* hi2]) ==
  ;; (int [(max lo1 lo2) *] [* (min hi1 h2)]) ==
  ;; [(max lo1 lo2) (min hi1 hi2)]
  (when (eq type 'INTEGER)
    (case head
      ((AND) ; make boundaries inclusive
       (when (consp lo1) (setq lo1 (1+ (car lo1))))
       (when (consp hi1) (setq hi1 (1- (car hi1))))
       (when (consp lo2) (setq lo2 (1+ (car lo2))))
       (when (consp hi2) (setq hi2 (1- (car hi2)))))
      ((OR) ; make boundaries exclusive
       (when (integerp lo1) (setq lo1 (list (1- lo1))))
       (when (integerp hi1) (setq hi1 (list (1+ hi1))))
       (when (integerp lo2) (setq lo2 (list (1- lo2))))
       (when (integerp hi2) (setq hi2 (list (1+ hi2)))))))
  (with-interval-designators ((lo1 le1 ln1) (lo2 le2 ln2)
                              (hi1 he1 hn1) (hi2 he2 hn2))
    ;; BOT   LO   HI  TOP
    (multiple-value-bind (b-lo n-lo e-lo bot)
        (cond ((eq lo1 '*) (values lo2 ln2 le2 lo1))
              ((eq lo2 '*) (values lo1 ln1 le1 lo2))
              ((< ln1 ln2) (values lo2 ln2 le2 lo1))
              ((< ln2 ln1) (values lo1 ln1 le1 lo2))
              (le1 (values lo1 ln1 le1 lo2))
              (t (values lo2 ln2 le2 lo1)))
      (multiple-value-bind (b-hi n-hi e-hi top)
          (cond ((eq hi1 '*) (values hi2 hn2 he2 hi1))
                ((eq hi2 '*) (values hi1 hn1 he1 hi2))
                ((< hn1 hn2) (values hi1 hn1 he1 hi2))
                ((< hn2 hn1) (values hi2 hn2 he2 hi1))
                (he1 (values hi2 hn2 he2 hi1))
                (t (values hi1 hn1 he1 hi2)))
        (case head
          ((AND)
           (unless (and n-lo n-hi ; no *
                        (or (> n-lo n-hi) ; strict
                            (and (= n-lo n-hi) ; same
                                 (or e-hi e-lo)))) ; at least one exclusive
             (values b-lo b-hi)))
          ((OR)
           (unless (and n-lo n-hi ; no *
                        (or (> n-lo n-hi) ; strict
                            (and (= n-lo n-hi) ; same
                                 e-hi e-lo))) ; both exclusive
             ;; the interval union is an interval too
             (values bot top))))))))

(defun interval-mix-not (yy nl nh)
  "(and yy (not (real nl nh)))"
  (with-interval-designators ((nl nle nln) (nh nhe nhn))
    (let ((n1 (if (eq nln '*) nil `(,(car yy) * ,(if nle nln `(,nln)))))
          (n2 (if (eq nhn '*) nil `(,(car yy) ,(if nhe nhn `(,nhn)) *))))
      (canonicalize-type `(or (and ,yy ,n1) (and ,yy ,n2))))))

(defun simplify-and-or (head ta1 ta2 &aux (t1 (car ta1)) (t2 (car ta2)))
  "simplify (head t1 t2)"
  (when (and (eq head 'AND)
             (or (subtypep t1 `(not ,t2))
                 (subtypep t2 `(not ,t1))))
    ;; (AND disjoint types) ==> NIL
    (return-from simplify-and-or nil))
  (when (and (eq head 'OR)
             (or (subtypep `(not ,t2) t1)
                 (subtypep `(not ,t1) t2)))
    ;; (OR complementary types) ==> T
    (return-from simplify-and-or t))
  ;; eliminate redredundant redundancies
  (cond ((subtypep t1 t2)
         (case head
           (AND (setf (car ta2) t))
           (OR (setf (car ta1) nil)))
         (return-from simplify-and-or head))
        ((subtypep t2 t1)
         (case head
           (AND (setf (car ta1) t))
           (OR (setf (car ta2) nil)))
         (return-from simplify-and-or head))
        ((and (consp t1) (consp t2)
              (eq (car t1) (car t2)))
         ;; what about (and (integer 1 10) (real 1.5 6.4)) ?!
         (case (car t1)
           ((REAL INTEGER RATIONAL FLOAT SHORT-FLOAT
             SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
            (multiple-value-bind (top bot)
                (interval-mix (second t1) (third t1) (second t2) (third t2)
                              (car t1) head)
              (if top
                (progn
                  (setf (car ta1) `(,(car t1) ,top ,bot)
                        (car ta2) `(,head))
                  (return-from simplify-and-or head))
                (when (eq head 'and)
                  (return-from simplify-and-or nil))))))))
  (when (and (consp t1) (eq (car t1) 'MEMBER)
             (consp t2) (eq (car t2) 'MEMBER))
    (case head
      ((AND) (setf (car ta1) (cons 'member (intersection (cdr t1) (cdr t2)))))
      ((OR) (setf (car ta1) (cons 'member (union (cdr t1) (cdr t2))))))
    (setf (car ta2) `(,head))
    (return-from simplify-and-or head))
  ;; wrong!
  ;;(when (and (consp t1) (eq (car t1) 'CONS)
  ;;           (consp t2) (eq (car t2) 'CONS))
  ;;  (setf (car ta1)
  ;;        `(CONS ,(canonicalize-type `(,head ,(second t1) ,(second t2)))
  ;;               ,(canonicalize-type `(,head ,(third t1) ,(third t2))))
  ;;        (car ta2) `(,head))
  ;;  (return-from simplify-and-or head))
  (let (yy nn hh)
    (when (and (eq head 'and) (consp t1) (consp t2)
               (or (and (eq (car t1) 'NOT)
                        (consp (second t1))
                        (eq (car (second t1)) (car t2))
                        (setq yy t2 nn (second t1)
                              hh (car t2)))
                   (and (eq (car t2) 'NOT)
                        (consp (second t2))
                        (eq (car (second t2)) (car t1))
                        (setq yy t1 nn (second t2)
                              hh (car t1))))
               (memq hh '(REAL INTEGER RATIONAL FLOAT SHORT-FLOAT
                          SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)))
      ;; (and (real ? ?) (not (real ? ?))) ==>
      ;; (or (real ? ?) (real ? ?))
      (setf (car ta1) (interval-mix-not yy (second nn) (third nn))
            (car ta2) `(,head))
      (return-from simplify-and-or head)))
  (when (eq head 'and)
    ;; (and class1 class2) ==>
    ;; (or common subclasses of class1 and class2)
    (if (and (clos::class-p t1) (clos::class-p t2))
        `(or ,(clos::class-and t1 t2))
        (let ((c1 (type-class t1)) (c2 (type-class t2)))
          (when (or (and (clos::class-p c1) (clos::class-p c2)
                         (null (clos::class-and c1 c2)))
                    (and (clos::class-p c1) (symbolp c2))
                    (and (clos::class-p c2) (symbolp c1)))
            (return-from simplify-and-or nil)))))
  ;; can do nothing!
  head)

;;; SUBTYPEP, the provisional version
(defvar *canonicalize-type-prefer-clos* nil)
(defun canonicalize-type (type &optional (*canonicalize-type-prefer-clos*
                                          *canonicalize-type-prefer-clos*)
                          &aux head)
  (setq type (expand-deftype type))
  ;; small, non-recursive simplifications
  (cond ((symbolp type)
         (case type
           (ATOM '(NOT CONS))
           (BASE-CHAR #+BASE-CHAR=CHARACTER 'CHARACTER
                      #-BASE-CHAR=CHARACTER '(AND CHARACTER (SATISFIES BASE-CHAR-P)))
           (BIGNUM  '(AND INTEGER (NOT FIXNUM)))
           (BIT '(INTEGER 0 1))
           (BOOLEAN '(MEMBER NIL T))
           (EXTENDED-CHAR #+BASE-CHAR=CHARACTER '(OR) ; NIL
                          #-BASE-CHAR=CHARACTER '(AND CHARACTER (NOT (SATISFIES BASE-CHAR-P))))
           (FIXNUM '(INTEGER #,most-negative-fixnum #,most-positive-fixnum))
           (KEYWORD '(AND SYMBOL (SATISFIES KEYWORDP)))
           (LIST (if *canonicalize-type-prefer-clos*
                     (get 'LIST 'CLOS::CLOSCLASS) '(OR CONS (MEMBER NIL))))
           ((NIL) '(OR))
           (NULL (if *canonicalize-type-prefer-clos*
                     (get 'NULL 'CLOS::CLOSCLASS) '(MEMBER NIL)))
           (NUMBER (if *canonicalize-type-prefer-clos*
                       (get 'NUMBER 'CLOS::CLOSCLASS) '(OR REAL COMPLEX)))
           (RATIO (if *canonicalize-type-prefer-clos*
                      (get 'RATIO 'CLOS::CLOSCLASS)
                      '(AND RATIONAL (NOT INTEGER))))
           (SEQUENCE (if *canonicalize-type-prefer-clos*
                         (get 'SEQUENCE 'CLOS::CLOSCLASS)
                         '(OR LIST VECTOR))) ; user-defined sequences??
           (SIGNED-BYTE '(INTEGER * *))
           (STANDARD-CHAR '(AND CHARACTER #-BASE-CHAR=CHARACTER (SATISFIES BASE-CHAR-P) (SATISFIES %STANDARD-CHAR-P)))
           (STRING-CHAR 'CHARACTER)
           ((T *) '(AND))
           (UNSIGNED-BYTE '(INTEGER 0 *))
           ((ARRAY SIMPLE-ARRAY BIT-VECTOR SIMPLE-BIT-VECTOR
             STRING SIMPLE-STRING BASE-STRING SIMPLE-BASE-STRING
             VECTOR SIMPLE-VECTOR
             COMPLEX REAL INTEGER RATIONAL FLOAT
             SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
            (canonicalize-type (list type)))
           ((STREAM FILE-STREAM SYNONYM-STREAM BROADCAST-STREAM
             CONCATENATED-STREAM TWO-WAY-STREAM ECHO-STREAM STRING-STREAM)
            ;; We treat STREAM and subclasses like CLOS classes, so that
            ;; (subtypep 'FUNDAMENTAL-STREAM 'STREAM) can return T.
            (or (clos-class type) type))
           (t
            (let ((f (clos-class type)))
              (if f (if (clos::built-in-class-p f) type f)
                  (let ((d (get type 'DEFSTRUCT-DESCRIPTION)))
                    ;; if TYPE names a normal structure (i.e., (svref d 1)=T),
                    ;; it is also a CLOS class, so the above would apply
                    (if d (canonicalize-type (svref d 1))
                        ;; by now we know that type does not name anything
                        type)))))))
        ((and (consp type) (symbolp (setq head (first type))))
         (case head
           (MEMBER ; (MEMBER &rest objects)
            (if (null (rest type)) '(OR)
              (let* ((new-rest (remove-duplicates (rest type)))
                     (same-p (eq new-rest (rest type))))
                (if (and (cdr new-rest) (every #'integerp new-rest))
                  ;; (member 1 2 3 4 5) ==> (integer 1 5)
                  (do* ((sorted (sort (if same-p (copy-list new-rest) new-rest)
                                      #'<))
                        (ta sorted (cdr ta)) (pos (car sorted) (1+ pos)))
                       ((endp ta) (list 'INTEGER (car sorted) (1- pos)))
                    (unless (= pos (car ta)) (return (cons 'MEMBER sorted))))
                  (if same-p type (cons 'MEMBER new-rest))))))
           (EQL ; (EQL object)
            `(MEMBER ,(second type)))
           ((AND OR) ; (AND type*), (OR type*) - may be able to simplify
            (if (null (cdr type)) type ; itself: (or), (and)
              (let ((terms (delete-duplicates
                            (mapcar #'canonicalize-type (rest type)))))
                (cond ((null (cdr terms)) (car terms)) ; (or type), (and type)
                      ((and (eq head 'and) ; (and ... nil ...) => nil
                            (member '(or) terms :test #'equal))
                       '(or))   ; nil
                      ((and (eq head 'or) ; (or ... t ...) => t
                            (member '(and) terms) :test #'equal)
                       '(and))  ; t
                      ((let ((new-rest
                              (delete-duplicates
                               ;; splice (OR (OR foo) bar) into (OR foo bar)
                               (mapcap (lambda (ty)
                                         (if (and (consp ty)
                                                  (eq (first ty) head))
                                             (rest ty) (list ty)))
                                       terms))))
                         (do ((ta1 new-rest (cdr ta1))) ((endp ta1))
                           (do ((ta2 (cdr ta1) (cdr ta2))) ((endp ta2))
                             (case (simplify-and-or head ta1 ta2)
                               ((NIL) (return-from canonicalize-type '(OR)))
                               ((T) (return-from canonicalize-type '(AND))))))
                         (if (equal new-rest (rest type)) type
                           (canonicalize-type
                            (cons head new-rest)))))))))
           (NOT                 ; (NOT type)
            (when (cddr type) (typespec-error 'subtypep type))
            (let ((not-type (canonicalize-type (second type))))
              (cond ((and (consp not-type) (eq (car not-type) 'NOT))
                     (second not-type)) ; (NOT (NOT A)) ==> A
                    ((and (consp not-type) (memq (car not-type) '(AND OR))
                          (null (cdr not-type))) ; i.e., '(AND) or '(OR)
                     ;; this might not be good
                     ;; (NOT (AND A B)) ==> (OR (NOT A) (NOT B))
                     ;;(canonicalize-type
                     ;; (cons (case (car not-type) (AND 'OR) (OR 'AND))
                     ;;       (mapcar (lambda (ty) (list 'NOT ty))
                     ;;               (rest not-type)))))
                     (list (case (car not-type) (AND 'OR) (OR 'AND))))
                    (t (list 'NOT not-type)))))
           (MOD ; (MOD n)
            (let ((n (second type)))
              (unless (and (integerp n) (>= n 0))
                (typespec-error 'subtypep type))
              `(INTEGER 0 ,(1- n))))
           (SIGNED-BYTE ; (SIGNED-BYTE &optional s)
            (let ((s (or (second type) '*)))
              (if (eq s '*)
                'INTEGER
                (progn
                  (unless (and (integerp s) (plusp s))
                    (typespec-error 'subtypep type))
                  (let ((n (ash 1 (1- s)))) ; (ash 1 *) == (expt 2 *)
                    `(INTEGER ,(- n) ,(1- n)))))))
           (UNSIGNED-BYTE ; (UNSIGNED-BYTE &optional s)
            (let ((s (or (second type) '*)))
              (if (eq s '*)
                '(INTEGER 0 *)
                (progn
                  (unless (and (integerp s) (>= s 0))
                    (typespec-error 'subtypep type))
                  `(INTEGER 0 ,(1- (ash 1 s))))))) ; (ash 1 *) == (expt 2 *)
           (SIMPLE-BIT-VECTOR ; (SIMPLE-BIT-VECTOR &optional size)
            (let ((size (or (second type) '*)))
              `(SIMPLE-ARRAY BIT (,size))))
           (SIMPLE-STRING ; (SIMPLE-STRING &optional size)
            (let ((size (or (second type) '*)))
              `(OR (SIMPLE-ARRAY CHARACTER (,size))
                   #-BASE-CHAR=CHARACTER (SIMPLE-ARRAY BASE-CHAR (,size))
                   (SIMPLE-ARRAY NIL (,size)))))
           (SIMPLE-BASE-STRING ; (SIMPLE-BASE-STRING &optional size)
            (let ((size (or (second type) '*)))
              `(SIMPLE-ARRAY BASE-CHAR (,size))))
           (SIMPLE-VECTOR ; (SIMPLE-VECTOR &optional size)
            (let ((size (or (second type) '*)))
              `(SIMPLE-ARRAY T (,size))))
           (BIT-VECTOR ; (BIT-VECTOR &optional size)
            (let ((size (or (second type) '*)))
              `(ARRAY BIT (,size))))
           (STRING ; (STRING &optional size)
            (let ((size (or (second type) '*)))
              `(OR (ARRAY CHARACTER (,size))
                   #-BASE-CHAR=CHARACTER (ARRAY BASE-CHAR (,size))
                   (ARRAY NIL (,size)))))
           (BASE-STRING ; (BASE-STRING &optional size)
            (let ((size (or (second type) '*)))
              `(ARRAY BASE-CHAR (,size))))
           (VECTOR ; (VECTOR &optional el-type size)
            (let ((el-type (if (cdr type) (second type) '*))
                  (size (if (cddr type) (third type) '*)))
              `(ARRAY ,el-type (,size))))
           ((REAL INTEGER RATIONAL FLOAT SHORT-FLOAT
             SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
            (let ((lo (if (consp (cdr type)) (cadr type) '*))
                  (hi (if (consp (cddr type)) (caddr type) '*))
                  (test (get head 'SYS::TYPE-SYMBOL)))
              (unless (and (valid-interval-designator-p lo test)
                           (valid-interval-designator-p hi test))
                (typespec-error 'subtypep type))
              (with-interval-designators ((lo le ln) (hi he hn))
                (when (eq head 'INTEGER) ; ==> (integer low high)
                  (when he (setq hi (1- (car hi)) he nil hn hi))
                  (when le (setq lo (1+ (car lo)) le nil ln lo)))
                (if (and (numberp ln) (numberp hn) (<= hn ln))
                  (if (or (< hn ln) he le)
                    '(OR) `(MEMBER ,ln))
                  `(,head ,lo ,hi)))))
           ((CONS)
            (let ((car (if (consp (cdr type)) (canonicalize-type (cadr type))
                           t))
                  (cdr (if (consp (cddr type)) (canonicalize-type (caddr type))
                           t)))
              (when (cdddr type)
                (typespec-error 'subtypep type))
              (cond ((or (equal '(OR) car) (equal '(OR) cdr)) '(OR))
                    ((and (eq t car) (eq t cdr)) 'cons)
                    (`(cons ,car ,cdr)))))
           (t type)))
        ((clos::class-p type)
         (if (and (clos::built-in-class-p type)
                  (eq (get (clos:class-name type) 'CLOS::CLOSCLASS) type))
           (canonicalize-type (clos:class-name type))
           type))
        ((encodingp type)
         #+UNICODE
         (let ((charset (encoding-charset type)))
           (case charset
             ((charset:unicode-16-big-endian charset:unicode-16-little-endian
               charset:unicode-32-big-endian charset:unicode-32-little-endian
               charset:utf-8 charset:java)
              'CHARACTER)
             (t
              (if (and (stringp charset)
                       (or (string= charset "UTF-16")
                           (string= charset "UTF-7")))
                'CHARACTER
                type))))
         #-UNICODE 'CHARACTER)
        (t (typespec-error 'subtypep type))))
(defun subtypep (type1 type2 &optional env &aux head1)
  (macrolet ((yes () '(return-from subtypep (values t t)))
             (no () '(return-from subtypep (values nil t)))
             (unknown () '(return-from subtypep (values nil nil))))
    (let ((ct1 (canonicalize-type type1 t))
          (ct2 (canonicalize-type type2 t)))
      (when (and (clos::class-p ct1) (clos::class-p ct2))
        (if (clos::subclassp ct1 ct2) (yes) (no))))
    (setq type1 (canonicalize-type type1))
    (setq type2 (canonicalize-type type2))
    ;; canonicalize-type: T ==> (AND),  NIL ==> (OR)
    (when (or (equal '(OR) type1) (equal '(AND) type2)) (yes))
    (when (eql type1 type2) (yes)) ; (subtypep type type) always true
    (when (equal '(OR) type2)
      (if (and (consp type1) (memq (car type1) '(AND MEMBER NOT OR SATISFIES)))
          (unknown) (no)))
    (when (consp type1)
      (setq head1 (first type1))
      (cond ;; über SATISFIES-Typen kann man nichts aussagen
            ;((and (eq head1 'SATISFIES) (eql (length type1) 2))
            ; (unknown)
            ;)
            ;; MEMBER: alle Elemente müssen vom Typ type2 sein
            ((eq head1 'MEMBER)
             (dolist (x (rest type1) (yes))
               (unless (typep x type2 env) (return (no)))))
            ;; NOT: (subtypep `(NOT ,type1) `(NOT ,type2)) ist äquivalent
            ;; zu (subtypep type2 type1), sonst ist Entscheidung schwierig
            ((and (eq head1 'NOT) (eql (length type1) 2))
             (return-from subtypep
               (cond ((and (consp type2) (eq (first type2) 'NOT)
                           (eql (length type2) 2))
                      (subtypep (second type2) (second type1)))
                     ((subtypep (second type1) type2)
                      (no))     ; assumes type2 != T
                     (t (unknown)))))
            ;; OR: Jeder Typ muss Subtyp von type2 sein
            ((eq head1 'OR)
             (dolist (type (rest type1) (yes))
               (multiple-value-bind (is known) (subtypep type type2)
                 (unless is (return-from subtypep (values nil known))))))))
    (when (consp type2)
      (cond ;; über SATISFIES-Typen kann man nichts aussagen
            ;((and (eq (first type2) 'SATISFIES) (eql (length type2) 2))
            ; (unknown)
            ;)
            ;; NOT: siehe oben
            ((and (eq (first type2) 'NOT) (eql (length type2) 2))
             (if (eq (second type2) 'CONS) ; (NOT CONS) == ATOM
                 (if (or (eq type1 'CONS) ; CONS or (CONS ...)
                         (and (consp type1) (eq head1 'CONS)))
                     (no)
                     (if (clos::class-p type1)
                         (yes)
                         (setq type2 'atom)))
                 (unknown)))
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
                 (when (subtypep type1 type) (return (yes))))
               (setq type2 'NIL))))) ; wird später besser behandelt
    (when (consp type1)
      (cond ;; AND: Falls ein Typ Subtyp von type2 ist, sonst nicht bekannt
            ((eq head1 'AND)
             (dolist (type (rest type1) (unknown))
               (when (subtypep type type2) (return (yes)))))))
    (when (and (symbolp type1) (get type1 'DEFSTRUCT-DESCRIPTION)
               (symbolp type2) (get type2 'DEFSTRUCT-DESCRIPTION))
      (let ((inclist1 (svref (get type1 'DEFSTRUCT-DESCRIPTION) 0))
            (inclist2 (svref (get type2 'DEFSTRUCT-DESCRIPTION) 0)))
        (loop
          (when (eq inclist1 inclist2) (return (yes)))
          (when (atom inclist1) (return))
          (setq inclist1 (cdr inclist1)))))
    (when (or (clos::class-p type1) (clos::class-p type2))
      (if (and (clos::class-p type1) (clos::class-p type2)
               (clos::subclassp type1 type2))
        (yes) (no)))
    (when (atom type1) (setq head1 type1 type1 (list type1)))
    (case head1
      ((ARRAY SIMPLE-ARRAY)
       (when (eq type2 'atom) (yes))
        (macrolet ((array-p (type) `(or (eq ,type 'ARRAY) (eq ,type head1))))
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
       (when (eq type2 'atom) (yes))
       (let* ((rtype1 (if (rest type1) (second type1) '*))
              (itype1 (if (cddr type1) (third type1) rtype1)))
         (values
          (cond ((or (eq type2 'COMPLEX) (eq type2 'NUMBER)) t)
                ((and (consp type2) (eq (first type2) 'COMPLEX))
                 (let* ((rtype2 (if (rest type2) (second type2) '*))
                        (itype2 (if (cddr type2) (third type2) rtype2)))
                   (and (or (eq rtype2 '*)
                            (and (not (eq rtype1 '*))
                                 (subtypep rtype1 rtype2)))
                        (or (eq itype2 '*)
                            (and (not (eq itype1 '*))
                                 (subtypep itype1 itype2))))))
                (t nil))
          t)))
      ((REAL INTEGER RATIONAL FLOAT SHORT-FLOAT SINGLE-FLOAT
        DOUBLE-FLOAT LONG-FLOAT)
       (when (eq type2 'atom) (yes))
       (with-interval-designators (((second type1) low1-ex low1)
                                   ((third type1) high1-ex high1))
         (cond ((and (consp type2)
                     (or (eq head1 (first type2))
                         (memq (first type2)
                               (case head1
                                 (REAL '(NUMBER))
                                 (INTEGER '(RATIONAL REAL NUMBER))
                                 ((RATIONAL FLOAT) '(REAL NUMBER))
                                 ((SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT
                                   LONG-FLOAT) '(FLOAT REAL NUMBER))))))
                (with-interval-designators (((second type2) low2-ex low2)
                                            ((third type2) high2-ex high2))
                  (values
                   (and (or (eq low2 '*)
                            (and (numberp low1)
                                 (if (and low2-ex (not low1-ex))
                                     (> low1 low2) (>= low1 low2))))
                        (or (eq high2 '*)
                            (and (numberp high1)
                                 (if (and high2-ex (not high1-ex))
                                     (< high1 high2) (<= high1 high2)))))
                   t)))
               ((not (eq head1 'INTEGER)) (no))
               (t (unknown)))))
      (CONS
       (let ((cartype1 (if (rest type1) (second type1) '*))
             (cdrtype1 (if (cddr type1) (third type1) '*)))
         (if (eq type2 'CONS)
           (yes)
           (multiple-value-bind (caremptyis caremptyknown)
               ;; actually, we went through `canonicalize-type',
               ;; so we know that both `caremptyis' and `cdremptyis' are NIL
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
                         (subtypep (if (eq cartype1 '*) 'T cartype1) cartype2))
                     (when (and carknown (not caris) cdremptyknown) (no))
                     (multiple-value-bind (cdris cdrknown)
                         (if (eq cdrtype2 '*)
                           (values t t)
                           (subtypep (if (eq cdrtype1 '*) 'T cdrtype1)
                                     cdrtype2))
                       (when (and cdrknown (not cdris) caremptyknown) (no))
                       (when (and caris carknown cdris cdrknown) (yes))
                       (unknown))))
                 (if (and caremptyknown cdremptyknown) (no) (unknown))))))))
      ((CHARACTER ENCODING FUNCTION HASH-TABLE PACKAGE PATHNAME RANDOM-STATE
        READTABLE SYMBOL)
       (if (eq type2 'ATOM) (yes) (no)))
      ((CLOS:GENERIC-FUNCTION COMPILED-FUNCTION #+ffi FFI::FOREIGN-FUNCTION)
       (if (or (eq type2 'FUNCTION) (eq type2 'ATOM)) (yes) (no)))
      (CLOS:STANDARD-GENERIC-FUNCTION
       (if (or (eq type2 'CLOS:GENERIC-FUNCTION) (eq type2 'FUNCTION)
               (eq type2 'ATOM))
           (yes) (no)))
      #+LOGICAL-PATHNAMES
      (LOGICAL-PATHNAME
       (if (or (eq type2 'PATHNAME) (eq type2 'ATOM)) (yes) (no)))
      (t
       (if (encodingp head1)
         (cond ((or (eq type2 'CHARACTER) (eq type2 'atom)) (yes))
               ((encodingp type2)
                (if (charset-subtypep head1 type2) (yes) (no)))
               (t (no)))
         (unknown))))))

;; this function does not return anything useful,
;; since CLISP complex numbers can always hold any number
(defun upgraded-complex-part-type (spec)
  (if (subtypep spec 'real) 'real
      (error-of-type 'error
        (TEXT "~S: type ~S is not a subtype of ~S")
        'upgraded-complex-part-type spec 'real)))

#+UNICODE
(let ((table (make-hash-table :test #'equal)))
  ;; cache: charset name -> list of intervals #(start1 end1 ... startm endm)
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
          (if i2 (setq intervals (list* i2 i1 intervals) i1 nil i2 nil)))
        (when (eql i end) (return))
        (incf i))
      (when i2 (setq intervals (list* i2 i1 intervals)))
      (map 'simple-string #'code-char (nreverse intervals))))
  |#
  ;; Return the definition range of a character set. If necessary, compute it
  ;; and store it in the cache.
  (defun get-charset-range (charset &optional maxintervals)
    (or (gethash charset table)
        (setf (gethash charset table)
              (charset-range (make-encoding :charset charset)
                             (code-char 0) (code-char (1- char-code-limit))
                             maxintervals))))
  ;; Fill the cache, but cache only the results with small lists of intervals.
  ;; Some iconv based encodings have large lists of intervals (up to 5844
  ;; intervals for ISO-2022-JP-2) which are rarely used and not worth caching.
  (do-external-symbols (sym (find-package "CHARSET"))
    (let* ((charset (encoding-charset (symbol-value sym)))
           (computed-range (get-charset-range charset 100))
           (intervals (/ (length computed-range) 2)))
      (when (>= intervals 100) (remhash charset table)))))
;; Test whether all characters encodable in encoding1 are also encodable in
;; encoding2.
(defun charset-subtypep (encoding1 encoding2)
  #-UNICODE (declare (ignore encoding1 encoding2)) #-UNICODE t
  #+UNICODE
  (let* ((intervals1 (get-charset-range (encoding-charset encoding1)))
         (intervals2 (get-charset-range (encoding-charset encoding2)))
         (n1 (length intervals1))
         (n2 (length intervals2))
         (jj1 0)  ; grows by 2 from 0 to n1
         (jj2 0)) ; grows by 2 from 0 to n2
    (loop
      ;; Get next interval from intervals1.
      (when (eql jj1 n1) (return-from charset-subtypep t))
      (let ((i1 (schar intervals1 jj1)) (i2 (schar intervals1 (+ jj1 1))))
        ;; Test whether [i1,i2] is contained in intervals2.
        (let (i3 i4)
          (loop
            (when (eql jj2 n2) ; [i1,i2] not contained in intervals2.
              (return-from charset-subtypep nil))
            (setq i3 (schar intervals2 jj2))
            (setq i4 (schar intervals2 (+ jj2 1)))
            ;; If i3 <= i4 < i1 <= i2, skip the interval [i3,i4].
            (when (char>= i4 i1) (return))
            (incf jj2 2))
          (when (char< i1 i3) ; i1 not contained in intervals2.
            (return-from charset-subtypep nil))
          (when (char< i4 i2) ; i4+1 (in [i1,i2]) not contained in intervals2.
            (return-from charset-subtypep nil))
          ;; Now (<= i3 i1) and (<= i2 i4),
          ;; hence [i1,i2] is contained in intervals2.
          (incf jj1 2))))))
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
      (unknown))))
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
             (MOD ; (MOD n)
              (let ((n (second type)))
                (unless (and (integerp n) (>= n 0))
                  (typespec-error 'subtypep type))
                (let ((low 0) (high (max 0 (1- n))))
                  (yes))))
             (SIGNED-BYTE ; (SIGNED-BYTE &optional s)
              (let ((s (or (second type) '*)))
                (if (eq s '*)
                  (let ((low '*) (high '*)) (yes))
                  (progn
                    (unless (and (integerp s) (plusp s))
                      (typespec-error 'subtypep type))
                    (let ((n (ash 1 (1- s)))) ; (ash 1 *) == (expt 2 *)
                      (let ((low (- n)) (high (1- n)))
                        (yes)))))))
             (UNSIGNED-BYTE ; (UNSIGNED-BYTE &optional s)
              (let ((s (or (second type) '*)))
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

#| TODO:
improve SUBTYPEP so that
 (let ((l '(ARRAY BASE-CHAR BASE-STRING BIT-VECTOR BOOLEAN CHARACTER COMPLEX
           CONS FLOAT FUNCTION CLOS:GENERIC-FUNCTION HASH-TABLE INTEGER LIST
           NULL NUMBER PACKAGE PATHNAME #+LOGICAL-PATHNAMES LOGICAL-PATHNAME
           RANDOM-STATE RATIONAL READTABLE REAL SEQUENCE
           CLOS:STANDARD-GENERIC-FUNCTION STREAM STRING SYMBOL VECTOR)))
  (dolist (a l)
    (dolist (b l)
      (unless (or (subtypep a b) (subtypep b a))
        (unless (equal (multiple-value-list (subtypep `(AND ,a ,b) 'NIL))
                       '(nil t))
          (print (list a b)))))))
prints as little as possible

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
