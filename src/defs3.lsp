;;; CLtL2-kompatible Definitionen
;;; insbesondere solche, die von CLtL1 abweichen
;;; Bruno Haible 6.12.1993

;===============================================================================

(defpackage "COMMON-LISP"
  (:nicknames "CL")
  (:use "LISP" "CLOS")
  (:shadow "MAKE-PACKAGE" "FLET" "LABELS" "MACROLET")
)

(in-package "COMMON-LISP")

;;; Exportierungen:
;; Nur in ANSI Common Lisp (CLtL2 bzw. ANSI-CL) explizit erwähnte Symbole!
(export '(
;; Typen:
array atom bignum bit bit-vector boolean character common compiled-function
complex cons double-float fixnum float function hash-table integer keyword
list #+LOGICAL-PATHNAMES logical-pathname long-float nil null number package
pathname random-state ratio rational readtable real sequence short-float
simple-array simple-bit-vector simple-string simple-vector single-float
standard-char stream file-stream synonym-stream broadcast-stream
concatenated-stream two-way-stream echo-stream string-stream string
string-char symbol t vector satisfies values mod signed-byte unsigned-byte
restart condition warning serious-condition error simple-condition
simple-warning simple-error storage-condition type-error simple-type-error
program-error control-error package-error print-not-readable stream-error
end-of-file file-error cell-error unbound-variable undefined-function
arithmetic-error division-by-zero floating-point-overflow
floating-point-underflow floating-point-inexact
floating-point-invalid-operation
;; Klassen:
class built-in-class standard-class generic-function standard-generic-function
method standard-method standard-object structure-class structure-object
;; Konstanten:
lambda-list-keywords lambda-parameters-limit nil t call-arguments-limit
multiple-values-limit pi boole-clr boole-set boole-1 boole-2 boole-c1 boole-c2
boole-and boole-ior boole-xor boole-eqv boole-nand boole-nor boole-andc1
boole-andc2 boole-orc1 boole-orc2 most-positive-fixnum most-negative-fixnum
most-positive-short-float least-positive-short-float least-negative-short-float
most-negative-short-float most-positive-single-float
least-positive-single-float least-negative-single-float
most-negative-single-float most-positive-double-float
least-positive-double-float least-negative-double-float
most-negative-double-float most-positive-long-float least-positive-long-float
least-negative-long-float most-negative-long-float
least-positive-normalized-short-float least-negative-normalized-short-float
least-positive-normalized-single-float least-negative-normalized-single-float
least-positive-normalized-double-float least-negative-normalized-double-float
least-positive-normalized-long-float least-negative-normalized-long-float
short-float-epsilon single-float-epsilon double-float-epsilon
long-float-epsilon short-float-negative-epsilon single-float-negative-epsilon
double-float-negative-epsilon long-float-negative-epsilon
char-code-limit char-font-limit char-bits-limit char-control-bit char-meta-bit
char-super-bit char-hyper-bit array-rank-limit array-dimension-limit
array-total-size-limit internal-time-units-per-second
;; Variablen:
*macroexpand-hook* *gensym-counter* *package* *modules* *random-state*
+ ++ +++ - * ** *** / // /// *standard-input*
*standard-output* *error-output* *query-io* *debug-io* *terminal-io*
*trace-output* *read-base* *read-suppress* *read-eval* *readtable*
*print-readably* *print-escape* *print-pretty* *print-circle* *print-base*
*print-radix* *print-case* *print-gensym* *print-level* *print-length*
*print-array* *read-default-float-format* *default-pathname-defaults*
*load-verbose* *load-print* *load-pathname* *load-truename* *break-on-warnings*
*compile-verbose* *compile-print* *compile-file-pathname*
*compile-file-truename* *features* *break-on-signals* *debugger-hook*
;; Funktionen:
coerce type-of upgraded-array-element-type typep subtypep null symbolp
atom consp listp numberp integerp rationalp floatp realp complexp characterp
stringp bit-vector-p vectorp simple-vector-p simple-string-p
simple-bit-vector-p arrayp packagep functionp compiled-function-p commonp eq
eql equal equalp not symbol-value symbol-function fdefinition boundp fboundp
special-operator-p set makunbound fmakunbound get-setf-method
get-setf-method-multiple-value apply funcall mapcar maplist mapc mapl mapcan
mapcon values values-list macro-function macroexpand macroexpand-1 proclaim
get remprop symbol-plist getf get-properties symbol-name make-symbol
copy-symbol gensym gentemp symbol-package keywordp make-package in-package
find-package package-name package-nicknames rename-package package-use-list
package-used-by-list package-shadowing-symbols list-all-packages delete-package
intern find-symbol unintern export unexport import shadowing-import shadow
use-package unuse-package find-all-symbols provide require zerop plusp minusp
oddp evenp = /= < > <= >= max min + - * / 1+ 1- conjugate gcd lcm exp expt
log sqrt isqrt abs phase signum sin cos tan cis asin acos atan sinh cosh tanh
asinh acosh atanh float rational rationalize numerator denominator floor
ceiling truncate round mod rem ffloor fceiling ftruncate fround decode-float
scale-float float-radix float-sign float-digits float-precision
integer-decode-float complex realpart imagpart logior logxor logand logeqv
lognand lognor logandc1 logandc2 logorc1 logorc2 boole lognot logtest logbitp
ash logcount integer-length byte byte-size byte-position ldb ldb-test mask-field
dpb deposit-field random make-random-state random-state-p standard-char-p
graphic-char-p string-char-p alpha-char-p upper-case-p lower-case-p
both-case-p digit-char-p alphanumericp char= char/= char< char> char<= char>=
char-equal char-not-equal char-lessp char-greaterp char-not-greaterp
char-not-lessp char-code char-bits char-font code-char make-char character
char-upcase char-downcase digit-char char-int int-char char-name name-char
char-bit set-char-bit complement constantly elt subseq copy-seq length reverse
nreverse make-sequence concatenate map map-into some every notany notevery
reduce fill replace remove remove-if remove-if-not delete delete-if
delete-if-not remove-duplicates delete-duplicates substitute substitute-if
substitute-if-not nsubstitute nsubstitute-if nsubstitute-if-not find find-if
find-if-not position position-if position-if-not count count-if count-if-not
mismatch search sort stable-sort merge car cdr caar cadr cdar cddr caaar
caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
cons tree-equal endp list-length nth first second third fourth fifth sixth
seventh eighth ninth tenth rest nthcdr last list list* make-list append
copy-list copy-alist copy-tree revappend nconc nreconc butlast nbutlast ldiff
rplaca rplacd subst subst-if subst-if-not nsubst nsubst-if nsubst-if-not sublis
nsublis member member-if member-if-not tailp adjoin union nunion intersection
nintersection set-difference nset-difference set-exclusive-or
nset-exclusive-or subsetp acons pairlis assoc assoc-if assoc-if-not rassoc
rassoc-if rassoc-if-not make-hash-table hash-table-p gethash remhash maphash
clrhash hash-table-count hash-table-rehash-size hash-table-rehash-threshold
hash-table-size hash-table-test sxhash make-array vector aref svref
array-element-type array-rank array-dimension array-dimensions array-total-size
array-in-bounds-p array-row-major-index row-major-aref adjustable-array-p
array-displacement
bit sbit bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2
bit-orc1 bit-orc2 bit-not array-has-fill-pointer-p fill-pointer vector-push
vector-push-extend vector-pop adjust-array char schar string= string-equal
string< string> string<= string>= string/= string-lessp string-greaterp
string-not-greaterp string-not-lessp string-not-equal make-string string-trim
string-left-trim string-right-trim string-upcase string-downcase
string-capitalize nstring-upcase nstring-downcase nstring-capitalize string
copy-structure
eval constantp make-synonym-stream make-broadcast-stream
make-concatenated-stream make-two-way-stream make-echo-stream
make-string-input-stream make-string-output-stream get-output-stream-string
streamp open-stream-p input-stream-p output-stream-p stream-element-type close
broadcast-stream-streams concatenated-stream-streams echo-stream-input-stream
echo-stream-output-stream synonym-stream-symbol two-way-stream-input-stream
two-way-stream-output-stream interactive-stream-p
copy-readtable readtablep set-syntax-from-char set-macro-character
get-macro-character make-dispatch-macro-character
set-dispatch-macro-character get-dispatch-macro-character readtable-case
read read-preserving-whitespace read-delimited-list read-line read-char
unread-char peek-char listen read-char-no-hang clear-input read-from-string
parse-integer read-byte write prin1 print pprint princ write-to-string
prin1-to-string princ-to-string write-char write-string write-line terpri
fresh-line finish-output force-output clear-output write-byte format y-or-n-p
yes-or-no-p wild-pathname-p pathname-match-p translate-pathname
#+LOGICAL-PATHNAMES logical-pathname
#+LOGICAL-PATHNAMES translate-logical-pathname
#+LOGICAL-PATHNAMES logical-pathname-translations
#+LOGICAL-PATHNAMES load-logical-pathname-translations
compile-file-pathname pathname truename parse-namestring merge-pathnames
make-pathname pathnamep pathname-host pathname-device pathname-directory
pathname-name pathname-type pathname-version namestring file-namestring
directory-namestring host-namestring enough-namestring user-homedir-pathname
open rename-file delete-file probe-file file-write-date file-author
file-position file-length load directory ensure-directories-exist
error cerror warn break compile compile-file disassemble
function-lambda-expression
documentation  variable structure type ; drei Dokumentations-Typen
describe describe-object inspect room ed dribble apropos apropos-list
get-decoded-time get-universal-time decode-universal-time encode-universal-time
get-internal-run-time get-internal-real-time sleep lisp-implementation-type
lisp-implementation-version machine-type machine-version machine-instance
software-type software-version short-site-name long-site-name identity
add-method call-next-method class-name class-of compute-applicable-methods
find-class find-method function-keywords initialize-instance make-instance
method-qualifiers next-method-p no-applicable-method no-next-method
print-object reinitialize-instance remove-method shared-initialize slot-boundp
slot-exists-p slot-makunbound slot-missing slot-unbound slot-value
signal make-condition compute-restarts restart-name find-restart invoke-restart
invoke-restart-interactively abort continue muffle-warning store-value
use-value invoke-debugger simple-condition-format-string
simple-condition-format-arguments type-error-datum type-error-expected-type
package-error-package print-not-readable-object stream-error-stream
file-error-pathname cell-error-name arithmetic-error-operation
arithmetic-error-operands
;; Special-forms:
eval-when quote function setq progn let let* locally compiler-let progv flet
labels macrolet symbol-macrolet if block return-from tagbody go
multiple-value-call multiple-value-prog1 catch unwind-protect throw declare
the load-time-value
;; Macros:
deftype defun defvar defparameter defconstant and or psetq setf psetf shiftf
rotatef define-modify-macro defsetf define-setf-method prog1 prog2
when unless cond
case typecase  otherwise ; otherwise als Marker für die catchall-clause
return loop do do* dolist dotimes prog prog* multiple-value-list
multiple-value-bind multiple-value-setq nth-value defmacro destructuring-bind
declaim remf defpackage do-symbols do-external-symbols do-all-symbols incf decf
push pushnew pop with-hash-table-iterator defstruct with-open-stream
with-input-from-string with-output-to-string with-standard-io-syntax
print-unreadable-object with-open-file define-symbol-macro
check-type assert etypecase ctypecase ecase ccase trace untrace
step time
loop-finish
formatter
defclass defgeneric defmethod generic-flet generic-function generic-labels
with-accessors with-slots
check-type assert etypecase ctypecase ecase ccase handler-case ignore-errors
handler-bind define-condition with-simple-restart restart-case restart-bind
with-condition-restarts
;; sonstige Markierer:
lambda
; Lambda-Listen-Markierer:
&optional &rest &key &allow-other-keys &aux &body &whole &environment
; EVAL-WHEN-Situationen:
eval load compile
; DECLARE-Specifier:
special type ftype function inline notinline ignore ignorable optimize speed
space safety compilation-speed debug declaration dynamic-extent
; Methoden-Kombination:
standard
))

;===============================================================================

(in-package "SYSTEM")

(defun common-lisp:make-package (package-name &key (nicknames '()) (use '("COMMON-LISP")))
  (lisp:make-package package-name :nicknames nicknames :use use)
)

;; These definitions conform to CLtL2.

(defmacro common-lisp:flet (fundefs &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil env)
    ((lambda (main-form)
       (if declarations
         `(LOCALLY (DECLARE ,@declarations) ,main-form)
         main-form
     ) )
     `(LISP:FLET ,fundefs
        ,@body-rest
      )
) ) )

(defmacro common-lisp:labels (fundefs &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil env)
    ((lambda (main-form)
       (if declarations
         `(LOCALLY (DECLARE ,@declarations) ,main-form)
         main-form
     ) )
     `(LISP:LABELS ,fundefs
        ,@body-rest
      )
) ) )

#|
;; This would conform to ANSI CL and its broken declaration scope.

(defmacro common-lisp:flet (fundefs &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil env)
    `(LISP:FLET ,fundefs
       ,@(if declarations
           `((LOCALLY (DECLARE ,@declarations) ,@body-rest))
           body-rest
         )
     )
) )

(defmacro common-lisp:labels (fundefs &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil env)
    `(LISP:LABELS ,fundefs
       ,@(if declarations
           `((LOCALLY (DECLARE ,@declarations) ,@body-rest))
           body-rest
         )
     )
) )

|#

(defmacro common-lisp:macrolet (macrodefs &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil env)
    `(LISP:MACROLET ,macrodefs
       ,@(if declarations
           `((LOCALLY (DECLARE ,@declarations) ,@body-rest))
           body-rest
         )
     )
) )

;===============================================================================

(defpackage "COMMON-LISP-USER" (:nicknames "CL-USER") (:use "COMMON-LISP"))

(sys::%proclaim-constant 'system::*common-lisp-user-package*
  (find-package "COMMON-LISP-USER")
)

;===============================================================================

