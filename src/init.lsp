;;;;   INITIALISIERUNGS-FILE

(in-package "LISP")

(shadow 'system::debug (find-package "SYSTEM"))

;;; Exportierungen:
(export '(
;; Typen:
array atom base-char base-string bignum bit bit-vector boolean character
common compiled-function complex cons double-float extended-char fixnum float
function hash-table integer keyword list #+LOGICAL-PATHNAMES logical-pathname
long-float nil null number package pathname random-state ratio rational
readtable real sequence short-float simple-array simple-base-string
simple-bit-vector simple-string simple-vector single-float standard-char
stream file-stream synonym-stream broadcast-stream concatenated-stream
two-way-stream echo-stream string-stream string string-char symbol t vector
satisfies values mod signed-byte unsigned-byte
; Pseudo-Typen:
simple-2bit-vector 2bit-vector simple-4bit-vector 4bit-vector
simple-8bit-vector 8bit-vector simple-16bit-vector 16bit-vector
simple-32bit-vector 32bit-vector special-form system-function
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
*evalhook* *applyhook* + ++ +++ - * ** *** / // /// *standard-input*
*standard-output* *error-output* *query-io* *debug-io* *terminal-io*
*trace-output* *read-base* *read-suppress* *read-eval* *readtable*
*print-readably* *print-escape* *print-pretty* *print-circle* *print-base*
*print-radix* *print-case* *print-gensym* *print-level* *print-length*
*print-array* *print-right-margin*
*read-default-float-format* *default-pathname-defaults*
*load-paths* *load-verbose* *load-print* *load-echo* *load-compiling*
*load-pathname* *load-truename* *break-on-warnings* *compile-warnings*
*compile-verbose* *compile-print* *compile-file-pathname*
*compile-file-truename* *features*
;; Funktionen:
coerce type-of upgraded-array-element-type typep subtypep null symbolp
atom consp listp numberp integerp rationalp floatp realp complexp characterp
stringp bit-vector-p vectorp simple-vector-p simple-string-p
simple-bit-vector-p arrayp packagep functionp compiled-function-p commonp eq
eql equal equalp not symbol-value symbol-function fdefinition boundp fboundp
special-operator-p special-form-p set makunbound fmakunbound
get-setf-expansion get-setf-method get-setf-method-multiple-value
apply funcall mapcar maplist mapc mapl mapcan
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
eval evalhook applyhook constantp make-synonym-stream make-broadcast-stream
make-concatenated-stream make-two-way-stream make-echo-stream
make-string-input-stream make-string-output-stream get-output-stream-string
streamp open-stream-p input-stream-p output-stream-p stream-element-type
stream-external-format close broadcast-stream-streams
concatenated-stream-streams echo-stream-input-stream echo-stream-output-stream
synonym-stream-symbol two-way-stream-input-stream two-way-stream-output-stream
interactive-stream-p
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
file-position file-length file-string-length load directory
ensure-directories-exist
error cerror warn break compile compile-file disassemble
documentation  variable structure type ; drei Dokumentations-Typen
describe inspect room ed dribble apropos apropos-list get-decoded-time
get-universal-time decode-universal-time encode-universal-time
get-internal-run-time get-internal-real-time sleep lisp-implementation-type
lisp-implementation-version machine-type machine-version machine-instance
software-type software-version short-site-name long-site-name identity
;; Special-forms:
eval-when quote function setq progn let let* locally compiler-let progv flet
labels macrolet symbol-macrolet if block return-from tagbody go
multiple-value-call multiple-value-prog1 catch unwind-protect throw declare
the load-time-value
;; Macros:
deftype defun defvar defparameter defconstant and or psetq setf psetf shiftf
rotatef define-modify-macro defsetf define-setf-expander define-setf-method
prog1 prog2 when unless cond
case typecase  otherwise ; otherwise als Marker für die catchall-clause
return loop do do* dolist dotimes prog prog* multiple-value-list
multiple-value-bind multiple-value-setq defmacro remf do-symbols
do-external-symbols do-all-symbols with-package-iterator incf decf
push pushnew pop defstruct
with-open-stream with-input-from-string with-output-to-string
with-standard-io-syntax with-open-file define-symbol-macro
check-type assert etypecase ctypecase ecase ccase trace untrace step time space
formatter
english deutsch francais
;; Symbol-Macros:
*ansi*
*default-file-encoding*
#+UNICODE *pathname-encoding*
#+UNICODE *terminal-encoding*
#+UNICODE *misc-encoding*
;; sonstige Markierer:
; EVAL-WHEN-Situationen:
eval load compile
; DECLARE-Specifier:
special type ftype function inline notinline ignore ignorable optimize speed
space safety compilation-speed debug declaration dynamic-extent compile
constant-inline constant-notinline
; Features:
interpreter compiler
))

(proclaim
  '(constant-notinline
    ;; These constants are platform dependent and therefore shouldn't be
    ;; inlined in compiled bytecode files.
    lambda-parameters-limit call-arguments-limit
    system::*jmpbuf-size* system::*big-endian*
    most-positive-fixnum most-negative-fixnum
    most-positive-short-float most-negative-short-float
    least-positive-short-float least-negative-short-float
    most-positive-single-float most-negative-single-float
    least-positive-single-float least-negative-single-float
    most-positive-double-float most-negative-double-float
    least-positive-double-float least-negative-double-float
    short-float-epsilon short-float-negative-epsilon
    single-float-epsilon single-float-negative-epsilon
    double-float-epsilon double-float-negative-epsilon
    char-code-limit
    array-total-size-limit array-dimension-limit array-rank-limit
    internal-time-units-per-second
)  )

(sys::%proclaim-constant 'lambda-list-keywords
  '(&optional &rest &key &allow-other-keys &aux &body &whole &environment)
)
(export lambda-list-keywords)

(sys::%putd 'exit #'sys::%exit)
(sys::%putd 'quit #'sys::%exit)
(sys::%putd 'bye #'sys::%exit)
(export '(exit quit bye))

(export 'the-environment)

(proclaim '(special *features*))

(in-package "SYSTEM" :nicknames '("SYS" "COMPILER") :use '("LISP"))
(proclaim '(special compiler::*compiling*))
(setq compiler::*compiling* nil)

(in-package "CLOS" :use '("LISP"))
;;; Exportierungen:
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
  class standard-class structure-class built-in-class
  standard-object structure-object
  generic-function standard-generic-function method standard-method
  ;; andere Symbole:
  standard ; Methoden-Kombination
))

(in-package "LISP")
; Exportierungen von conditio.lsp
(export '(
  handler-bind                  ; vorgezogen für compiler.lsp
  find-restart compute-restarts ; vorgezogen für user1.lsp
  invoke-restart-interactively  ; dito
  restart                       ; vermeide Konflikt mit user1.lsp
  continue                      ; vermeide Konflikt mit user1.lsp
  end-of-file                   ; vermeide Konflikt mit init.lsp, user2.lsp
  ; Typen für error-of-type:
  condition warning serious-condition error storage-condition type-error
  program-error control-error package-error print-not-readable parse-error
  stream-error end-of-file reader-error file-error cell-error unbound-variable
  undefined-function unbound-slot arithmetic-error division-by-zero
  floating-point-overflow floating-point-underflow floating-point-inexact
  floating-point-invalid-operation
))

(in-package "USER" :use '("LISP" "CLOS"))

; Optionale Files wie macros3.lsp, defs2.lsp, loop.lsp, defs3.lsp machen ihre
; Exportierungen selber.


(in-package "SYSTEM")

#-COMPILER ; nur beim Bootstrappen
(progn

; vorläufig soll bei GET_CLOSURE nicht expandiert werden:
(sys::%putd '%expand-lambdabody-main
  (function %expand-lambdabody-main
    (lambda (lambdabody venv fenv)
      (declare (source nil) (ignore venv fenv))
      lambdabody
) ) )

; vorläufig soll defun ganz trivial expandiert werden:
(sys::%putd 'defun
  (cons 'sys::macro
    (function defun
      (lambda (form env)
        (declare (ignore env))
        #|
        (let ((name (cadr form))
              (lambdalist (caddr form))
              (body (cdddr form)))
          `(SYS::%PUTD ',name (FUNCTION ,name (LAMBDA ,lambdalist ,@body)))
        )
        |#
        (let ((name (cadr form)))
          (list 'sys::%putd (list 'quote name)
            (list 'function name (cons 'lambda (cddr form)))
        ) )
    ) )
) )

)

;; Handling for internationalized strings. New translations are not defined
;; through `deflocalized'; instead, they are produced using GNU gettext and
;; retrieved using SYS::LANGUAGE, which calls the C function gettext().
(let ((h (cons 'sys::macro
           (function
             (lambda (form env)
               (declare (ignore env))
               (apply #'(lambda (&key &allow-other-keys)) form)
               (list 'SYS::LANGUAGE
                     (getf form 'ENGLISH)
                     (getf form 'DEUTSCH)
                     (getf form 'FRANCAIS)
      )) ) ) ) )
  (sys::%putd 'ENGLISH h)
  (sys::%putd 'DEUTSCH h)
  (sys::%putd 'FRANCAIS h)
)

(sys::%putd 'sys::exported-lisp-symbol-p
  (function sys::exported-lisp-symbol-p
    (lambda (symbol)
      (let ((string (symbol-name symbol)))
        (or (let ((p (find-package "LISP")))
              (and p
                (multiple-value-bind (s f) (find-symbol string p)
                  (and (eq s symbol) (eq f ':external))
            ) ) )
            (let ((p (find-package "COMMON-LISP")))
              (and p
                (multiple-value-bind (s f) (find-symbol string p)
                  (and (eq s symbol) (eq f ':external))
    ) ) )   ) ) )
) )

(sys::%putd 'sys::remove-old-definitions
  (function sys::remove-old-definitions
    (lambda (symbol) ; entfernt die alten Funktionsdefinitionen eines Symbols
      (if (special-operator-p symbol)
        (error-of-type 'error
          (ENGLISH "~S is a special form and may not be redefined.")
          symbol
      ) )
      (if (and (or (fboundp symbol) (macro-function symbol))
               (sys::exported-lisp-symbol-p symbol)
          )
        (cerror (ENGLISH "The old definition will be lost")
                (ENGLISH "Redefining the COMMON LISP ~A ~S")
                (fbound-string symbol) ; "Funktion" bzw. "Macro"
                symbol
                (macro-function symbol)
      ) )
      (fmakunbound symbol) ; Funktions-/Macro-Definition streichen
      ; Property sys::definition wird nicht entfernt, da sie sowieso
      ; bald neu gesetzt wird.
      (remprop symbol 'sys::macro) ; Macro-Definition streichen
      (remprop symbol 'sys::defstruct-reader) ; DEFSTRUCT-Information streichen
      (when (get symbol 'sys::documentation-strings) ; Dokumentation streichen
        (sys::%set-documentation symbol 'FUNCTION nil)
      )
      (when (get symbol 'sys::inline-expansion)
        (sys::%put symbol 'sys::inline-expansion t)
      )
      (when (get symbol 'sys::traced-definition) ; Trace streichen
        (warn (ENGLISH "DEFUN/DEFMACRO: redefining ~S; it was traced!")
              symbol
        )
        (untrace2 symbol)
    ) )
) )

; THE-ENVIRONMENT as in SCHEME
(sys::%putd '%the-environment
  (function %the-environment
    (lambda (form env)
      (declare (ignore form))
      (sys::svstore env 0 (svref (svref env 0) 2)) ; nuke *evalhook* binding
      env
    )
) )
(sys::%putd '%the-environment-error
  (function %the-environment-error
    (lambda ()
      (error-of-type 'source-program-error
        (ENGLISH "~S is impossible in compiled code")
        'the-environment
    ) )
) )
(sys::%putd 'the-environment
  (cons 'sys::macro
    (function the-environment
      (lambda (form env)
        (declare (ignore form env))
        '(progn
           (eval-when ((not eval)) (%the-environment-error))
           (let ((*evalhook* #'%the-environment)) 0)
         )
) ) ) )
; The toplevel environment
(proclaim '(special *toplevel-environment*))
(setq *toplevel-environment* (eval '(the-environment)))
(proclaim '(special *toplevel-denv*))
(setq *toplevel-denv* (svref *toplevel-environment* 4))

; liefert den Namen des impliziten Blocks zu einem Funktionsnamen
(defun function-block-name (funname)
  (if (atom funname) funname (second funname))
)

; schiebt einen impliziten Block in einen Body ein.
; benutzt *venv* und *fenv*.
(defun add-implicit-block (name body)
  (multiple-value-bind (body-rest declarations docstring)
      (sys::parse-body body t (vector *venv* *fenv*))
    (append (if declarations (list (cons 'DECLARE declarations)))
            (if docstring (list docstring))
            (list (list* 'BLOCK (function-block-name name) body-rest))
) ) )

;;; Funktionen zum Expandieren von Macros innerhalb eines Codestückes
;;;
;;; Insgesamt wird der gesamte Code (einer Funktion) durchgegangen und
;;; globale und lokale Macros expandiert.
;;; Aus       #'(lambda lambdalist . body)
;;; wird so   #'(lambda expanded-lambdalist
;;;               (declare (source (lambdalist . body))) . expanded-body
;;;             )
;;; Durch diese Deklaration ist gewährleistet, dass eine bereits einmal
;;; durchlaufene Funktion als solche erkannt und nicht unnötigerweise ein
;;; zweites Mal durchlaufen wird.

; Vorsicht! Fürs Bootstrappen (erkennbar an #-COMPILER) müssen manche der
; Funktionen in primitiverem Lisp (ohne do, do*, case) geschrieben werden.

(PROGN

(proclaim '(special *keyword-package*))
(setq *keyword-package* (find-package "KEYWORD"))

(proclaim '(special *fenv*))
; *fenv* = Das aktuelle Function-Environment während der Expansion
; einer Form. Struktur: NIL oder ein 2n+1-elementiger Vektor
; (n1 f1 ... nn fn next), wo die ni Funktionsnamen sind, die fi ihre funktionale
; Bedeutung sind (Closure oder (MACRO . Closure) oder noch NIL); bei next
; geht's ebenso weiter.

; (fenv-assoc s fenv) sucht Symbol s in Function-Environment fenv.
(defun fenv-assoc (s fenv)
  (if fenv
    (if (simple-vector-p fenv)
      #+COMPILER
      (do ((l (1- (length fenv)))
           (i 0 (+ i 2)))
          ((= i l) (fenv-assoc s (svref fenv i)))
        (if (equal s (svref fenv i))
          (return (svref fenv (1+ i)))
      ) )
      #-COMPILER
      (let ((l (1- (length fenv)))
            (i 0))
        (block nil
          (tagbody
            1 (if (= i l) (return-from nil (fenv-assoc s (svref fenv i))))
              (if (equal s (svref fenv i))
                (return-from nil (svref fenv (1+ i)))
              )
              (setq i (+ i 2))
              (go 1)
      ) ) )
      (error-of-type 'type-error
        :datum fenv :expected-type '(or null simple-vector)
        (ENGLISH "~S is an invalid function environment")
        fenv
    ) )
    'T ; nicht gefunden
) )
; Stellt fest, ob ein Funktionsname im Function-Environment fenv nicht
; definiert ist und daher auf die globale Funktion verweist.
(defun global-in-fenv-p (s fenv) ; vorläufig
  (eq (fenv-assoc s fenv) 'T)
)

(proclaim '(special *venv*))
; *venv* = Das aktuelle Variablen-Environment während der Expansion
; einer Form. Struktur: NIL oder ein 2n+1-elementiger Vektor
; (n1 v1 ... nn vn next), wo die ni Symbole sind, die vi ihre
; syntaktische Bedeutung (Symbol-Macro-Objekt oder sonstiges); bei next
; geht's ebenso weiter.

; (venv-assoc s venv) sucht Symbol s in Variablen-Environment venv.
; Liefert den Wert (oder NIL falls kein Wert).
; Vorsicht: Der Wert kann #<SPECDECL> oder #<SYMBOL-MACRO ...> sein, darf
; daher in interpretiertem Code nicht in einer Variablen zwischengespeichert
; werden.
(defun venv-assoc (s venv)
  (if venv
    (if (simple-vector-p venv)
      #+COMPILER
      (do ((l (1- (length venv)))
           (i 0 (+ i 2)))
          ((= i l) (venv-assoc s (svref venv i)))
        (if (eq s (svref venv i))
          (return (svref venv (1+ i)))
      ) )
      #-COMPILER
      (let ((l (1- (length venv)))
            (i 0))
        (block nil
          (tagbody
            1 (if (= i l) (return-from nil (venv-assoc s (svref venv i))))
              (if (eq s (svref venv i))
                (return-from nil (svref venv (1+ i)))
              )
              (setq i (+ i 2))
              (go 1)
      ) ) )
      (error-of-type 'type-error
        :datum venv :expected-type '(or null simple-vector)
        (ENGLISH "~S is an invalid variable environment")
        venv
    ) )
    (and (boundp s) (%symbol-value s)) ; nicht gefunden
) )

; Die meisten Expansionsfunktionen liefern zwei Werte: Das Expansions-
; ergebnis, der zweite Wert (NIL oder T) zeigt an, ob darin etwas verändert
; wurde.

; (%expand-cons ...) setzt ein cons zusammen. 2 Werte.
; form=alte Form,
; expf,flagf = Expansion des First-Teils,
; expr,flagr = Expansion des Rest-Teils.
(defun %expand-cons (form expf flagf expr flagr)
  (if (or flagf flagr)
    (values (cons expf expr) t)
    (values form nil)
) )

; (%expand-form form) expandiert eine ganze Form. 2 Werte.
(defun %expand-form (form)
  (if (atom form)
    #+COMPILER
    (let (h)
      (if (and (symbolp form) (symbol-macro-p (setq h (venv-assoc form *venv*))))
        (values (sys::%record-ref h 0) t)
        (values form nil)
    ) )
    #-COMPILER
    (if (and (symbolp form) (symbol-macro-p (venv-assoc form *venv*)))
      (values (sys::%record-ref (venv-assoc form *venv*) 0) t)
      (values form nil)
    )
    ; form ist CONS
    (let ((f (first form)))
      (if (function-name-p f)
        (let ((h (fenv-assoc f *fenv*)))
          ; f ist in *fenv* assoziiert zu h
          (if (eq h 'T)
            ; f hat keine lokale Definition
            ; Nun die einzelnen Expander für die Special-forms:
            (case f
              ((RETURN-FROM THE)
                ; 1. Argument lassen, alle weiteren expandieren
                (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (multiple-value-call #'%expand-cons (rest form)
                    (second form) nil
                    (%expand-list (cddr form))
              ) ) )
              ((QUOTE GO DECLARE LOAD-TIME-VALUE) ; nichts expandieren
                (values form nil)
              )
              (FUNCTION
                ; Falls erstes bzw. zweites Argument Liste,
                ; als Lambda-Ausdruck expandieren.
                (multiple-value-call #'%expand-cons form
                  'FUNCTION nil
                  (if (atom (cddr form))
                    (if (function-name-p (second form))
                      (let ((h (fenv-assoc (second form) *fenv*)))
                        (cond ((or (eq h 'T) (closurep h) (null h)) (values (rest form) nil))
                              ((and (consp h) (eq (first h) 'MACRO))
                               (error-of-type 'source-program-error
                                 (ENGLISH "~S: ~S is illegal since ~S is a local macro")
                                 '%expand form (second form)
                              ))
                              (t (error-of-type 'error
                                   (ENGLISH "~S: invalid function environment ~S")
                                   '%expand *fenv*
                              )  )
                      ) )
                      (if (atom (second form))
                        (error-of-type 'source-program-error
                          (ENGLISH "~S: ~S is invalid since ~S is not a symbol")
                          '%expand form (second form)
                        )
                        (multiple-value-call #'%expand-cons (rest form)
                          (%expand-lambda (second form))
                          (cddr form) nil
                    ) ) )
                    (multiple-value-call #'%expand-cons (rest form)
                      (second form) nil
                      (multiple-value-call #'%expand-cons (cddr form)
                        (%expand-lambda (third form))
                        (cdddr form) nil
              ) ) ) ) )
              (EVAL-WHEN
                ; Falls die Situation COMPILE angegeben ist, führe den Body
                ; als PROGN aus, gib eine Form zurück, die ohne Seiteneffekte
                ; dieselben Werte liefert.
                ; Sonst expandiere alle Argumente ab dem zweiten als Formen.
                (if (or (member 'COMPILE (second form))
                        (member :COMPILE-TOPLEVEL (second form)))
                  (values
                    (list 'values-list
                      (list 'quote
                        (multiple-value-list (eval (cons 'PROGN (cddr form))))
                    ) )
                    t
                  )
                  (multiple-value-call #'%expand-cons form
                    (first form) nil
                    (multiple-value-call #'%expand-cons (rest form)
                      (second form) nil
                      (%expand-list (cddr form))
              ) ) ) )
              (LET ; Variablenliste und Body expandieren
                (let ((*venv* *venv*))
                  (%expand-special-declarations (cddr form))
                  (multiple-value-call #'%expand-cons form
                    (first form) nil
                    (multiple-value-call #'%expand-cons (rest form)
                      (%expand-varspez (second form))
                      (%expand-list (cddr form))
              ) ) ) )
              (LET* ; Variablenliste und Body expandieren
                (let ((*venv* *venv*))
                  (%expand-special-declarations (cddr form))
                  (multiple-value-call #'%expand-cons form
                    (first form) nil
                    (multiple-value-call #'%expand-cons (rest form)
                      (%expand-varspez* (second form))
                      (%expand-list (cddr form))
              ) ) ) )
              (LOCALLY ; Body expandieren
                (let ((*venv* *venv*))
                  (%expand-special-declarations (cdr form))
                  (multiple-value-call #'%expand-cons form
                    (first form) nil
                    (%expand-list (cdr form))
              ) ) )
              (MULTIPLE-VALUE-BIND ; Form und Body getrennt expandieren
                (let ((*venv* *venv*))
                  (%expand-special-declarations (cdddr form))
                  (multiple-value-call #'%expand-cons form
                    'MULTIPLE-VALUE-BIND nil
                    (multiple-value-call #'%expand-cons (rest form)
                      (second form) nil
                      (multiple-value-call #'%expand-cons (cddr form)
                        (%expand-form (third form))
                        (progn
                          (%expand-lexical-variables (second form))
                          (%expand-list (cdddr form))
              ) ) ) ) ) )
              (COMPILER-LET
                ; Variablenliste im leeren Environment und Body expandieren
                (progv
                  (mapcar #'%expand-varspec-var (second form))
                  (mapcar #'%expand-varspec-val (second form))
                  (values (%expand-form (cons 'PROGN (cddr form))) t)
              ) )
              (COND ; Alle Teilformen der Klauseln expandieren:
                (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (%expand-cond (rest form))
              ) )
              (CASE ; 1. Argument und alle Teilformen der Klauseln expandieren:
                (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (multiple-value-call #'%expand-cons (rest form)
                    (%expand-form (second form))
                    (%expand-case (cddr form))
              ) ) )
              (BLOCK
                ; Body expandieren. Falls darin ein RETURN-FROM auf diesen
                ; Block vorkommt, behalte BLOCK. Sonst mache ein PROGN daraus.
                (multiple-value-bind (body flagb) (%expand-list (cddr form))
                  (if (%return-p (second form) body)
                    (multiple-value-call #'%expand-cons form
                      (first form) nil
                      (multiple-value-call #'%expand-cons (rest form)
                        (second form) nil
                        body flagb
                    ) )
                    (values
                      (cond ((atom body) body)
                            ((null (cdr body)) (car body))
                            (t (cons 'progn body))
                      )
                      t
              ) ) ) )
              ((SETQ PSETQ) ; jedes zweite Argument expandieren
                (if (%expand-setqlist-macrop (rest form))
                  (let ((new (if (eq (first form) 'SETQ) 'SETF 'PSETF)))
                    (values
                      (%expand-form
                        (funcall (macro-function new) (cons new (rest form)) (vector *venv* *fenv*))
                      )
                      t
                  ) )
                  (multiple-value-call #'%expand-cons form
                    (first form) nil
                    (%expand-setqlist (rest form))
              ) ) )
              (MULTIPLE-VALUE-SETQ ; 1. Argument lassen, alle weiteren expandieren
                (if (%expand-varlist-macrop (second form))
                  (values (%expand-form (cons 'MULTIPLE-VALUE-SETF (rest form))) t)
                  (multiple-value-call #'%expand-cons form
                    'MULTIPLE-VALUE-SETQ nil
                    (multiple-value-call #'%expand-cons (rest form)
                      (second form) nil
                      (%expand-list (cddr form))
              ) ) ) )
              (TAGBODY
                ; alle Argumente expandieren, dabei entstehende Atome weglassen
                (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (%expand-tagbody (rest form))
              ) )
              (PROGN ; alle Argumente expandieren, evtl. vereinfachen.
                (if (null (rest form))
                  (values nil t)
                  (if (null (cddr form))
                    (values (%expand-form (second form)) t)
                    (multiple-value-call #'%expand-cons form
                      (first form) nil
                      (%expand-list (rest form))
              ) ) ) )
              (FLET ; Funktionsdefinitionen expandieren,
                    ; Body im erweiterten Environment expandieren
                (if (null (second form))
                  (values (%expand-form (cons 'PROGN (cddr form))) t)
                  (let ((newfenv (%expand-fundefs-1 (second form))))
                    (multiple-value-call #'%expand-cons form
                      (first form) nil
                      (multiple-value-call #'%expand-cons (rest form)
                        (%expand-fundefs-2 (second form))
                        (let ((*fenv* (apply #'vector newfenv)))
                          (%expand-list (cddr form))
              ) ) ) ) ) )
              (LABELS ; Funktionsdefinitionen und Body im erweiterten Environment expandieren
                (if (null (second form))
                  (values (%expand-form (cons 'PROGN (cddr form))) t)
                  (let ((newfenv (%expand-fundefs-1 (second form))))
                    (let ((*fenv* (apply #'vector newfenv)))
                      (multiple-value-call #'%expand-cons form
                        (first form) nil
                        (multiple-value-call #'%expand-cons (rest form)
                          (%expand-fundefs-2 (second form))
                          (%expand-list (cddr form))
              ) ) ) ) ) )
              (MACROLET ; Body im erweiterten Environment expandieren
                (do ((L1 (second form) (cdr L1))
                     (L2 nil))
                    ((atom L1)
                     (if L1
                       (error-of-type 'source-program-error
                         (ENGLISH "code after MACROLET contains a dotted list, ending with ~S")
                         L1
                       )
                       (let ((*fenv* (apply #'vector (nreverse (cons *fenv* L2)))))
                         (values (%expand-form (cons 'PROGN (cddr form))) t)
                    )) )
                  (let ((macrodef (car L1)))
                    (if (and (consp macrodef)
                             (symbolp (car macrodef))
                             (consp (cdr macrodef))
                        )
                      (setq L2
                        (cons (make-macro-expandercons macrodef)
                              (cons (car macrodef) L2)
                      ) )
                      (error-of-type 'source-program-error
                        (ENGLISH "illegal syntax in MACROLET: ~S")
                        macrodef
              ) ) ) ) )
              (SYMBOL-MACROLET ; Body im erweiterten Environment expandieren
                (do ((L1 (second form) (cdr L1))
                     (L2 nil))
                    ((atom L1)
                     (if L1
                       (error-of-type 'source-program-error
                         (ENGLISH "code after SYMBOL-MACROLET contains a dotted list, ending with ~S")
                         L1
                       )
                       (let ((*venv* (apply #'vector (nreverse (cons *venv* L2)))))
                         (let ((specials (%expand-special-declarations (cddr form))))
                           (do ((L3 (second form) (cdr L3)))
                               ((atom L3))
                             (if (member (caar L3) specials :test #'eq)
                               (error-of-type 'source-program-error
                                 (ENGLISH "~S: symbol ~S must not be declared SPECIAL and a macro at the same time")
                                 'symbol-macrolet (caar L3)
                         ) ) ) )
                         (values (%expand-form (cons 'LOCALLY (cddr form))) t)
                    )) )
                  (let ((symdef (car L1)))
                    (if (and (consp symdef)
                             (symbolp (car symdef))
                             (consp (cdr symdef))
                             (null (cddr symdef))
                        )
                      (let ((symbol (car symdef))
                            (expansion (cadr symdef)))
                        (if (special-variable-p symbol)
                          (error-of-type 'program-error
                            (ENGLISH "~S: symbol ~S is declared special and must not be declared a macro")
                            'symbol-macrolet symbol
                          )
                          (setq L2
                            (cons (make-symbol-macro expansion) (cons symbol L2))
                      ) ) )
                      (error-of-type 'source-program-error
                        (ENGLISH "illegal syntax in SYMBOL-MACROLET: ~S")
                        symdef
              ) ) ) ) )
              (%HANDLER-BIND ; Handlerliste und Body expandieren
                (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (multiple-value-call #'%expand-cons (rest form)
                    (%expand-handlers (second form))
                    (%expand-list (cddr form))
              ) ) )
              (t
                (cond ((and (symbolp f) (special-operator-p f))
                       ; sonstige Special-forms,
                       ; z.B. IF, CATCH, THROW, PROGV, UNWIND-PROTECT, PROGN,
                       ; PROG1, PROG2, WHEN, UNLESS, MULTIPLE-VALUE-LIST,
                       ; MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1, AND, OR:
                       (multiple-value-call #'%expand-cons form
                         f nil
                         (%expand-list (rest form))
                      ))
                      ((and (symbolp f) (setq h (macro-function f))) ; globale Macro-Definition
                       (values (%expand-form (funcall h form (vector *venv* *fenv*))) t)
                      )
                      (t ; normaler Funktionsaufruf
                       (multiple-value-call #'%expand-cons form
                         f nil
                         (%expand-list (rest form))
            ) ) )     ))
            ; f hat eine lokale Definition
            (cond ((or (closurep h) (null h)); aufzurufende Funktion
                   (multiple-value-call #'%expand-cons form
                     f nil
                     (%expand-list (rest form))
                  ))
                  ((and (consp h) (eq (car h) 'MACRO)) ; zu expandierender Macro
                   (values (%expand-form (funcall (cdr h) form *fenv*)) t)
                  ) ; Expander aufrufen
                  (t (error-of-type 'error
                       (ENGLISH "bad function environment occurred in ~S: ~S")
                       '%expand-form *fenv*
        ) ) )     )  )
        (if (consp f)
          (multiple-value-call #'%expand-cons form
            (%expand-lambda f)
            (%expand-list (rest form))
          )
          (error-of-type 'source-program-error
            (ENGLISH "~S: invalid form ~S")
            '%expand-form form
) ) ) ) ) )

; Hilfsfunktionen für die Expansion:

; expandiert eine Liste von Formen. 2 Werte.
(defun %expand-list (l)
  (if (atom l)
    (if l
      (error-of-type 'source-program-error
        (ENGLISH "code contains a dotted list, ending with ~S")
        l
      )
      (values nil nil)
    )
    (multiple-value-call #'%expand-cons l
                         (%expand-form (first l))
                         (%expand-list (rest l))
) ) )

; Fügt lexikalische Variablen zu *venv* hinzu.
; (Wird nur dazu benutzt, um Symbol-Macros zu überdecken.)
(defun %expand-lexical-variables (vars)
  (if vars
    (setq *venv*
      (apply #'vector
        (nconc (mapcan #'(lambda (v) (list v nil)) vars) (list *venv*))
) ) ) )

; Fügt SPECIAL-Deklarationen am Anfang eines Body zu *venv* hinzu.
(defun %expand-special-declarations (body)
  (multiple-value-bind (body-rest declarations)
      (sys::parse-body body nil (vector *venv* *fenv*))
    (declare (ignore body-rest)) ; Deklarationen nicht wegwerfen!
    (let ((specials nil))
      (mapc #'(lambda (declspec)
                (if (and (consp declspec) (null (cdr (last declspec))))
                  (if (eq (car declspec) 'SPECIAL)
                    (mapc #'(lambda (x) (if (symbolp x) (setq specials (cons x specials))))
                          (cdr declspec)
              ) ) ) )
            (nreverse declarations)
      )
      (setq specials (nreverse specials))
      (%expand-lexical-variables specials) ; auf specdecl kommt es hier nicht an
      specials
) ) )

; expandiert einen Funktionsnamen, der ein Cons ist (das muss ein
; Lambda-Ausdruck sein). 2 Werte.
(defun %expand-lambda (l)
  (unless (eq (first l) 'lambda)
    (error-of-type 'source-program-error
      (ENGLISH "~S: ~S should be a lambda expression")
      '%expand-form l
  ) )
  (multiple-value-call #'%expand-cons l
      'lambda nil ; LAMBDA
      (%expand-lambdabody (rest l))
) )

; expandiert den CDR eines Lambda-Ausdrucks, ein (lambdalist . body). 2 Werte.
(defun %expand-lambdabody (lambdabody &optional name blockp)
  (let ((body (rest lambdabody)))
    (if (and (consp body)
             (let ((form (car body)))
               (and (consp form)
                    (eq (car form) 'DECLARE)
                    (let ((declspecs (cdr form)))
                      (and (consp declspecs)
                           (let ((declspec (car declspecs)))
                             (and (consp declspec)
                                  (eq (car declspec) 'SOURCE)
        )    ) )    ) )    ) )
      (values lambdabody nil) ; bereits expandiert -> unberührt lassen
      (let ((*venv* *venv*))
        (if blockp
          (setq lambdabody
                (cons (first lambdabody)
                      (add-implicit-block name (rest lambdabody))
        ) )     )
        (values (list*
                  (%expand-lambdalist (first lambdabody))
                  (list 'DECLARE (list 'SOURCE lambdabody))
                  (%expand-list (rest lambdabody))
                )
                t
) ) ) ) )

; expandiert eine Lambdaliste. 2 Werte.
(defun %expand-lambdalist (ll)
  (if (atom ll)
    (if ll
      (error-of-type 'source-program-error
        (ENGLISH "lambda list must not end with the atom ~S")
        ll
      )
      (values nil nil)
    )
    (multiple-value-call #'%expand-cons ll
        (%expand-parspez (first ll))
        (progn
          (let ((v (first ll)))
            (if (not (member v lambda-list-keywords :test #'eq))
              (setq *venv* (vector (%expand-varspec-var v) nil *venv*))
          ) )
          (%expand-lambdalist (rest ll))
) ) )   )

; expandiert ein Element einer Lambdaliste. 2 Werte.
; (Expandiert dabei nur bei Listen, und dann auch nur das zweite Element.)
(defun %expand-parspez (ps)
  (if (or (atom ps) (atom (rest ps)))
    (values ps nil)
    (multiple-value-call #'%expand-cons ps
        (first ps) nil
        (multiple-value-call #'%expand-cons (rest ps)
            (%expand-form (second ps))
            (cddr ps) nil
) ) )   )

; expandiert eine Variablenliste für LET. 2 Werte.
(defun %expand-varspez (vs &optional (nvenv nil))
  (if (atom vs)
    (if vs
      (error-of-type 'source-program-error
        (ENGLISH "~S: variable list ends with the atom ~S")
        'let vs
      )
      (progn
        (setq *venv* (apply #'vector (nreverse (cons *venv* nvenv))))
        (values nil nil)
    ) )
    (multiple-value-call #'%expand-cons vs
        (%expand-parspez (first vs)) ; Bei Liste 2. Element expandieren
        (%expand-varspez (rest vs) (list* nil (%expand-varspec-var (first vs)) nvenv))
) ) )

; expandiert eine Variablenliste für LET*. 2 Werte.
(defun %expand-varspez* (vs)
  (if (atom vs)
    (if vs
      (error-of-type 'source-program-error
        (ENGLISH "~S: variable list ends with the atom ~S")
        'let* vs
      )
      (values nil nil)
    )
    (multiple-value-call #'%expand-cons vs
        (%expand-parspez (first vs)) ; Bei Liste 2. Element expandieren
        (progn
          (setq *venv* (vector (%expand-varspec-var (first vs)) nil *venv*))
          (%expand-varspez* (rest vs))
) ) )   )

(defun %expand-varspec-var (varspec)
  (if (atom varspec) varspec (first varspec))
)

(defun %expand-varspec-val (varspec)
  (if (atom varspec) nil (eval (second varspec)))
)

; Expandiert eine Cond-Klausel-Liste. 2 Werte.
(defun %expand-cond (clauses)
  (if (atom clauses)
    (values clauses nil)
    (multiple-value-call #'%expand-cons clauses
        (%expand-list (first clauses))
        (%expand-cond (rest clauses))
) ) )

; Expandiert eine Case-Klausel-Liste. 2 Werte.
(defun %expand-case (clauses)
  (if (atom clauses)
    (values clauses nil)
    (multiple-value-call #'%expand-cons clauses
      (multiple-value-call #'%expand-cons (first clauses)
        (caar clauses) nil
        (%expand-list (cdar clauses))
      )
      (%expand-case (rest clauses))
) ) )

; Auf den bereits expandierten Body wird folgendes angewandt:
; (%return-p name list) stellt fest, ob die Formenliste list irgendwo ein
; (RETURN-FROM name ...) enthält.
(defun %return-p (name body)
  (block return-p
    (tagbody 1
      (if (atom body) (return-from return-p nil))
      (let ((form (car body)))
        (if
          ; stelle fest, ob form ein (RETURN-FROM name ...) enthält:
          (and (consp form)
               (or (and (eq (first form) 'return-from) ; (RETURN-FROM name ...)
                        (eq (second form) name)
                   )
                   (and (consp (first form))           ; Lambdaliste
                        (%return-p name (first form))
                   )
                   (and (not ; keine neue Definition desselben Blocks ?
                          (and (eq (first form) 'block) (eq (second form) name))
                        )
                        (%return-p name (rest form)) ; Funktionsaufruf
          )    )   )
          (return-from return-p t)
      ) )
      (setq body (cdr body))
      (go 1)
) ) )

(defun %expand-varlist-macrop (l)
  (and (consp l)
       (or (and (symbolp (car l)) (symbol-macro-p (venv-assoc (car l) *venv*)))
           (%expand-varlist-macrop (cdr l))
) )    )

(defun %expand-setqlist-macrop (l)
  (and (consp l) (consp (cdr l))
       (or (and (symbolp (car l)) (symbol-macro-p (venv-assoc (car l) *venv*)))
           (%expand-setqlist-macrop (cddr l))
) )    )

(defun %expand-setqlist (l)
  (if (or (atom l) (atom (cdr l)))
    (values l nil)
    (multiple-value-call #'%expand-cons l
        (first l) nil
        (multiple-value-call #'%expand-cons (rest l)
            (%expand-form (second l))
            (%expand-setqlist (cddr l))
) ) )   )

; (%expand-tagbody list) expandiert die Elemente einer Liste und lässt dabei
; entstehende Atome fest (damit keine neuen Tags entstehen, die andere Tags
; verdecken könnten). 2 Werte.
(defun %expand-tagbody (body)
  (cond ((atom body) (values body nil))
        ((atom (first body))
         (multiple-value-call #'%expand-cons body
             (first body) nil
             (%expand-tagbody (rest body))
        ))
        (t (multiple-value-bind (exp flag) (%expand-form (first body))
             (if (atom exp)
               (values (%expand-tagbody (rest body)) t) ; weglassen
               (multiple-value-call #'%expand-cons body
                   exp flag
                   (%expand-tagbody (rest body))
) )     )  ) ) )
; (%expand-fundefs-1 fundefs) liefert eine Liste (name1 nil ... namek nil *fenv*)
(defun %expand-fundefs-1 (fundefs)
  (if (atom fundefs)
    (if fundefs
      (error-of-type 'source-program-error
        (ENGLISH "FLET/LABELS: code contains a dotted list, ending with ~S")
        fundefs
      )
      (list *fenv*)
    )
    (let ((fundef (car fundefs)))
      (if (and (consp fundef) (function-name-p (car fundef)) (consp (cdr fundef)))
        (list* (car fundef) nil (%expand-fundefs-1 (cdr fundefs)))
        (error-of-type 'source-program-error
          (ENGLISH "illegal syntax in FLET/LABELS: ~S")
          fundef
) ) ) ) )
; (%expand-fundefs-2 fundefs) expandiert eine Funktionsdefinitionenliste,
; wie in FLET, LABELS. 2 Werte.
(defun %expand-fundefs-2 (fundefs)
  (if (atom fundefs)
    (values fundefs nil)
    (let ((fundef (car fundefs)))
      (multiple-value-call #'%expand-cons fundefs
             (multiple-value-call #'%expand-cons fundef
                     (car fundef) nil
                     (%expand-lambdabody (cdr fundef) (car fundef) t)
             )
             (%expand-fundefs-2 (rest fundefs))
) ) ) )
; (%expand-handlers handlers) expandiert eine Typ/Handler-Liste
; wie in %HANDLER-BIND. 2 Werte.
(defun %expand-handlers (handlers)
  (if (atom handlers)
    (values handlers nil)
    (let ((handler (car handlers)))
      (multiple-value-call #'%expand-cons handlers
        (multiple-value-call #'%expand-cons handler
          (car handler) nil
          (%expand-list (cdr handler))
        )
        (%expand-handlers (cdr handlers))
) ) ) )

#|
; expandiert eine Form in einem gegebenen Function-Environment
; Kann bei Bedarf von EVAL aufgerufen werden.
(defun %expand-form-main (form *fenv*)
  (%expand-form form)
)
|#

; expandiert (lambdalist . body) in einem gegebenen Function-Environment.
; Wird von GET_CLOSURE aufgerufen.
(defun %expand-lambdabody-main (lambdabody *venv* *fenv*)
  (%expand-lambdabody lambdabody)
)

(VALUES) )

;; ab hier ist FUNCTION funktionsfähig, soweit kein MACROLET darin vorkommt.

(PROGN

(proclaim '(special *load-paths*))
(setq *load-paths* nil)
(proclaim '(special *source-file-types*))
(setq *source-file-types* '(#".lsp"))
(proclaim '(special *compiled-file-types*))
(setq *compiled-file-types* '(#".fas"))

; vorläufig brauchen die Files nicht gesucht zu werden:
(defun search-file (filename extensions)
  (mapcan #'(lambda (extension)
              (let ((filename (merge-pathnames filename extension)))
                (if (probe-file filename) (list filename) '())
            ) )
          (reverse extensions)
) )

(proclaim '(special *load-verbose*))
(setq *load-verbose* t)
(proclaim '(special *load-print*))
(setq *load-print* nil)
(proclaim '(special *load-echo*))
(setq *load-echo* nil)
(proclaim '(special *load-compiling*))
(setq *load-compiling* nil)
(proclaim '(special *load-pathname*))
(setq *load-pathname* nil)
(proclaim '(special *load-truename*))
(setq *load-truename* nil)
(proclaim '(special *load-input-stream*))
(setq *load-input-stream* nil)
(proclaim '(special *load-level*))
(setq *load-level* 0)

; (LOAD filename [:verbose] [:print] [:if-does-not-exist] [:echo] [:compiling]),
; CLTL S. 426
(fmakunbound 'load)
(defun load (filename
             &key (verbose *load-verbose*) (print *load-print*) (if-does-not-exist t)
                  (echo *load-echo*) (compiling *load-compiling*))
  (let ((stream
          (if (streamp filename)
            filename
            (or (open (setq filename (pathname filename))
                  :direction :input-immutable
                  :element-type 'character
                  :if-does-not-exist nil
                )
                ; Datei mit genau diesem Namen nicht vorhanden.
                ; Suche unter den Dateien mit demselben Namen und den
                ; Extensions "LSP", "FAS" die neueste:
                (let ((present-files
                        (search-file filename
                          (append *source-file-types* *compiled-file-types*)
                     )) )
                  (if (endp present-files)
                    nil
                    (open (setq filename (first present-files))
                          :direction :input-immutable
                          :element-type 'character
       )) ) )   ) ) )
    (if stream
      (let* ((input-stream
               (if echo
                 (make-echo-stream stream *standard-output*)
                 stream
             ) )
             (*load-level* (1+ *load-level*))
             (indent (if (null verbose) ""
                         (make-string *load-level* :initial-element #\Space)))
             (*load-input-stream* input-stream)
             ; :verbose, :print, :echo und :compiling wirken nicht rekursiv -
             ; dazu hat man ja gerade die Special-Variablen *load-verbose* etc.
             ;(*load-verbose* verbose)
             ;(*load-print* print)
             ;(*load-echo* echo)
             ;(*load-compiling* compiling)
             (*load-pathname* (if (pathnamep filename) filename nil))
             (*load-truename* (if (pathnamep filename) (truename filename) nil))
             (*package* *package*) ; *PACKAGE* binden
             (*readtable* *readtable*) ; *READTABLE* binden
             (end-of-file "EOF")) ; einmaliges Objekt
        (when verbose
          (fresh-line)
          (write-string ";;")
          (write-string indent)
          (write-string (ENGLISH "Loading file "))
          (princ filename)
          (write-string (ENGLISH " ..."))
        )
        (sys::allow-read-eval input-stream t)
        (block nil
          (unwind-protect
            (tagbody weiter
              (when echo (fresh-line))
              (let ((obj (read input-stream nil end-of-file)))
                (when (eql obj end-of-file) (return-from nil))
                (setq obj
                  (multiple-value-list
                    (cond ((compiled-function-p obj) (funcall obj))
                          (compiling (funcall (compile-form-in-toplevel-environment obj)))
                          (t (eval obj))
                ) ) )
                (when print (when obj (print (first obj))))
              )
              (go weiter)
            )
            (or (eq input-stream stream) (sys::built-in-stream-close input-stream))
            (or (eq stream filename) (sys::built-in-stream-close stream))
        ) )
        (when verbose
          (fresh-line)
          (write-string ";;")
          (write-string indent)
          (write-string (ENGLISH "Loading of file "))
          (princ filename)
          (write-string (ENGLISH " is finished."))
        )
        t
      )
      (if if-does-not-exist
        (error-of-type 'file-error
          :pathname filename
          (ENGLISH "A file with name ~A does not exist")
          filename
        )
        nil
      )
) ) )

; vorläufig:
(sys::%putd 'defun
  (cons 'sys::macro
    (function defun
      (lambda (form env)
        (unless (and (consp (cdr form)) (consp (cddr form)))
          (error-of-type 'source-program-error
            (ENGLISH "~S: missing function name and/or parameter list")
            'defun
        ) )
        (let ((name (cadr form))
              (lambdalist (caddr form))
              (body (cdddr form)))
          (unless (symbolp name)
            (error-of-type 'source-program-error
              (ENGLISH "~S: ~S is not a symbol.")
              'defun name
          ) )
          (when (special-operator-p name)
            (error-of-type 'source-program-error
              (ENGLISH "~S: special form ~S cannot be redefined.")
              'defun name
          ) )
          (multiple-value-bind (body-rest declarations docstring)
                               (sys::parse-body body t env)
            (declare (ignore docstring))
            #|
            `(PROGN
               (SYS::%PUT ',name 'SYS::DEFINITION
                 (CONS ',form (THE-ENVIRONMENT))
               )
               (SYS::%PUTD ',name
                 (FUNCTION ,name
                   (LAMBDA ,lambdalist
                     (DECLARE (SYS::IN-DEFUN ,name) ,@declarations)
                     (BLOCK ,name ,@body-rest)
               ) ) )
               ',name
             )
            |#
            (list 'progn
              (list 'sys::%put (list 'quote name) ''sys::definition
                    (list 'cons (list 'quote form) '(the-environment))
              )
              (list 'sys::%putd (list 'quote name)
                (list 'FUNCTION name
                  (list 'LAMBDA lambdalist
                        (list* 'DECLARE (list 'SYS::IN-DEFUN name) declarations)
                        (list* 'BLOCK name body-rest)
              ) ) )
              (list 'quote name)
            )
    ) ) ) )
) )

; vorläufige Definition des Macros DO :
(sys::%putd 'do
  (cons 'sys::macro
    (function do
      (lambda (form env)
        (let ((varclauselist (second form))
              (exitclause (third form))
              (body (cdddr form)))
          (when (atom exitclause)
            (error-of-type 'source-program-error
              (ENGLISH "exit clause in ~S must be a list")
              'do
          ) )
          (let ((bindlist nil)
                (reinitlist nil)
                (bodytag (gensym))
                (exittag (gensym)))
            (multiple-value-bind (body-rest declarations)
                                 (sys::parse-body body nil env)
              (block do
                (tagbody 1
                  (when (atom varclauselist)
                    (return-from do
                      #|
                      `(block nil
                         (let ,(nreverse bindlist)
                           (declare ,@declarations)
                           (tagbody
                             (go ,exittag)
                             ,bodytag
                             ,@body-rest
                             (psetq ,@(nreverse reinitlist))
                             ,exittag
                             (or ,(first exitclause) (go ,bodytag))
                             (return-from nil (progn ,@(rest exitclause)))
                       ) ) )
                      |#
                      (list 'block 'nil
                        (list 'let (nreverse bindlist)
                          (cons 'declare declarations)
                          (list* 'tagbody
                            (list 'go exittag)
                            bodytag
                            (append body-rest
                              (list
                                (cons 'psetq (nreverse reinitlist))
                                exittag
                                (list 'or (first exitclause) (list 'go bodytag))
                                (list 'return-from 'nil
                                  (cons 'progn (rest exitclause))
                      ) ) ) ) ) )
                  ) )
                  (let ( (varclause (first varclauselist)) )
                       (setq varclauselist (rest varclauselist))
                       (cond ( (atom varclause)
                                  (setq bindlist
                                        (cons varclause bindlist)) )
                             ( (atom (cdr varclause))
                                  (setq bindlist
                                        (cons (first varclause) bindlist)) )
                             ( (atom (cddr varclause))
                                  (setq bindlist
                                        (cons varclause bindlist)) )
                             ( t (setq bindlist
                                       (cons (list (first varclause)
                                                   (second varclause))
                                             bindlist))
                                 (setq reinitlist
                                       (list* (third varclause)
                                              (first varclause)
                                              reinitlist)) )))
                  (go 1)
    ) ) ) ) ) ) )
) )

; vorläufige Definition des Macros DOTIMES :
(sys::%putd 'dotimes
  (cons 'sys::macro
    (function dotimes
      (lambda (form env)
        (let ((var (first (second form)))
              (countform (second (second form)))
              (resultform (third (second form)))
              (body (cddr form)))
          (multiple-value-bind (body-rest declarations)
                               (sys::parse-body body nil env)
            (let ((g (gensym)))
              #|
              `(DO ((,var 0 (1+ ,var))
                    (,g ,countform))
                   ((>= ,var ,g) ,resultform)
                 (declare ,@declarations)
                 ,@body-rest
               )
              |#
              (list* 'do (list (list var '0 (list '1+ var)) (list g countform))
                         (list (list '>= var g) resultform)
                     (cons 'declare declarations)
                     body-rest
              )
    ) ) ) ) )
) )

(VALUES) )

;; ab hier sind LOAD, DEFUN, DO, DOTIMES (eingeschränkt) funktionsfähig.

(LOAD "defseq")   ;; Definitionen von Standard-Sequences

(LOAD "backquot") ;; Backquote-Readmacro

(PROGN

(sys::%putd 'sys::backquote
  (cons 'sys::macro
    (function sys::backquote
      (lambda (form &optional env) (declare (ignore env)) (third form))
) ) )

(VALUES) )

;; ab hier ist Backquote funktionsfähig

(LOAD "defmacro")

;; ab hier ist FUNCTION (uneingeschränkt) funktionsfähig.

(PROGN

(sys::%putd 'defmacro
  (cons 'sys::macro
    (function defmacro
      (lambda (form &optional env)
        (declare (ignore env))
        (multiple-value-bind (expansion name lambdalist docstring)
                             (sys::make-macro-expansion (cdr form))
          (declare (ignore lambdalist))
          `(LET ()
             (EVAL-WHEN (COMPILE LOAD EVAL)
               (SYSTEM::REMOVE-OLD-DEFINITIONS ',name)
               ,@(if docstring
                   `((SYSTEM::%SET-DOCUMENTATION ',name 'FUNCTION ',docstring))
                   '()
                 )
               (SYSTEM::%PUTD ',name (CONS 'SYSTEM::MACRO ,expansion))
             )
             (EVAL-WHEN (EVAL)
               (SYSTEM::%PUT ',name 'SYSTEM::DEFINITION
                 (CONS ',form (THE-ENVIRONMENT))
             ) )
             ',name
           )
    ) ) )
) )

(sys::%putd 'defun
  (cons 'sys::macro
    (function defun
      (lambda (form env)
        (if (atom (cdr form))
          (error-of-type 'source-program-error
            (ENGLISH "~S: cannot define a function from that: ~S")
            'defun (cdr form)
        ) )
        (unless (function-name-p (cadr form))
          (error-of-type 'source-program-error
            (ENGLISH "~S: the name of a function must be a symbol, not ~S")
            'defun (cadr form)
        ) )
        (if (atom (cddr form))
          (error-of-type 'source-program-error
            (ENGLISH "~S: function ~S is missing a lambda list")
            'defun (cadr form)
        ) )
        (let ((name (cadr form))
              (lambdalist (caddr form))
              (body (cdddr form)))
          (multiple-value-bind (body-rest declarations docstring)
                               (sys::parse-body body t env)
            (let ((symbolform
                    (if (atom name)
                      `',name
                      `(LOAD-TIME-VALUE (GET-SETF-SYMBOL ',(second name)))
                  ) )
                  (lambdabody
                    `(,lambdalist (DECLARE (SYS::IN-DEFUN ,name) ,@declarations)
                       (BLOCK ,(function-block-name name) ,@body-rest)
                     )
                 ))
              `(LET ()
                 (SYSTEM::REMOVE-OLD-DEFINITIONS ,symbolform)
                 ,@(if ; Is name declared inline?
                       (if (and compiler::*compiling* compiler::*compiling-from-file*)
                         (member name compiler::*inline-functions* :test #'equal)
                         (eq (get (if (atom name) name (get-setf-symbol (second name))) 'inlinable) 'inline)
                       )
                     ; Is the lexical environment the top-level environment?
                     ; If yes, save the lambdabody for inline compilation.
                     (if compiler::*compiling*
                       (if (and (null compiler::*venv*)
                                (null compiler::*fenv*)
                                (null compiler::*benv*)
                                (null compiler::*genv*)
                                (eql compiler::*denv* *toplevel-denv*)
                           )
                         `((EVAL-WHEN (COMPILE) (COMPILER::C-DEFUN ',name ',lambdabody))
                           (EVAL-WHEN (LOAD)
                             (SYSTEM::%PUT ,symbolform 'SYSTEM::INLINE-EXPANSION ',lambdabody)
                          ))
                         `((EVAL-WHEN (COMPILE) (COMPILER::C-DEFUN ',name)))
                       )
                       (if (and (null (svref env 0)) ; venv
                                (null (svref env 1)) ; fenv
                           )
                         `((EVAL-WHEN (EVAL)
                             (LET ((%ENV (THE-ENVIRONMENT)))
                               (IF (AND (NULL (SVREF %ENV 0)) ; venv
                                        (NULL (SVREF %ENV 1)) ; fenv
                                        (NULL (SVREF %ENV 2)) ; benv
                                        (NULL (SVREF %ENV 3)) ; genv
                                        (EQL (SVREF %ENV 4) *TOPLEVEL-DENV*) ; denv
                                   )
                                 (SYSTEM::%PUT ,symbolform 'SYSTEM::INLINE-EXPANSION ',lambdabody)
                          )) ) )
                         '()
                     ) )
                     '()
                   )
                 ,@(if docstring
                     `((SYSTEM::%SET-DOCUMENTATION ,symbolform 'FUNCTION ',docstring))
                     '()
                   )
                 (SYSTEM::%PUTD ,symbolform
                   (FUNCTION ,name (LAMBDA ,@lambdabody))
                 )
                 (EVAL-WHEN (EVAL)
                   (SYSTEM::%PUT ,symbolform 'SYSTEM::DEFINITION
                     (CONS ',form (THE-ENVIRONMENT))
                 ) )
                 ',name
               )
    ) ) ) ) )
) )

(VALUES) )

;; ab hier sind DEFMACRO und DEFUN funktionsfähig.

; (MACRO-EXPANDER . macrodef)                                         [Macro]
; expandiert zum Macro-Expander als Programmtext (FUNCTION ... (LAMBDA ...)).
(defmacro MACRO-EXPANDER (&body macrodef)
  (make-macro-expansion macrodef)
)

(LOAD "macros1")  ;; Kontrollstrukturen - Macros
(LOAD "macros2")  ;; weitere Macros

(LOAD "defs1")    ;; Definitionen zu Symbolen, Zahlen, Characters, Zeit
#-(or UNIX WIN32)
(LOAD "timezone") ;; Definition der Zeitzone

(LOAD "places")   ;; SETF-Places: Definitionen und Macros

;; ab hier ist SETF u.ä. funktionsfähig.

(LOAD "floatpri") ;; Ausgabe von Floating-Points

(LOAD "type")     ;; TYPEP

(LOAD "defstruc") ;; DEFSTRUCT-Macro

(LOAD "format")   ;; FORMAT

;; ab hier ist FORMATTER funktionsfähig.

(in-package "LISP")
(export '(default-directory dir))
(in-package "SYSTEM")

; (default-directory) ist ein Synonym für (cd).
(defun default-directory () (cd))

; (setf (default-directory) dir) ist ein Synonym für (cd dir).
(defsetf default-directory () (value)
  `(PROGN (CD ,value) ,value)
)

; FORMAT-Control-String zur Datumsausgabe,
; anwendbar auf eine Liste (sec min hour day month year ...),
; belegt 17-19 Zeichen
(definternational date-format
  (t ENGLISH)
)
(deflocalized date-format ENGLISH
  (formatter "~1{~5@*~D-~4@*~2,'0D-~3@*~2,'0D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}")
)
(defun date-format ()
  (localized 'date-format)
)

; zeigt ein Directory an.
(defun dir (&optional (pathnames #+(or DOS) '("*.*\\" "*.*")
                                 #+(or AMIGA UNIX OS/2 WIN32) '("*/" "*")
                                 #+ACORN-RISCOS '("*." "*" "*.*")
           )          )
  (flet ((onedir (pathname)
           (let ((pathname-list (directory pathname :full t :circle t)))
             (if (every #'atom pathname-list)
               (format t "~{~%~A~}"
                 (sort pathname-list #'string< :key #'namestring)
               )
               (let ((date-format (date-format)))
                 (dolist (l (sort pathname-list #'string< :key #'(lambda (l) (namestring (first l)))))
                   (format t "~%~A~40T~7D~52T~21<~@?~>"
                             (first l) (fourth l) date-format (third l)
               ) ) )
        )) ) )
    (if (listp pathnames) (mapc #'onedir pathnames) (onedir pathnames))
  )
  (values)
)

; A piece of "DO-WHAT-I-MEAN":
; Searches a program file.
; We search in the current directory and then in the directories
; listed in *load-paths*.
; If an extension is specified in the filename, we search only for
; files with this extension. If no extension is specified, we search
; only for files with an extension from the given list.
; The return value is a list of all matching files from the first directory
; containing any matching file, sorted according to decreasing FILE-WRITE-DATE
; (i.e. from new to old), or NIL if no matching file was found.
(defun search-file (filename extensions
                    &aux (use-extensions (null (pathname-type filename))) )
  (when use-extensions
    (setq extensions ; Case-Konversionen auf den Extensions durchführen
      (mapcar #'pathname-type extensions)
  ) )
  ; Defaults einmergen:
  (setq filename (merge-pathnames filename '#".*"))
  ; Suchen:
  (let ((already-searched nil))
    (dolist (dir (cons '#""
                       ; Wenn filename ".." enthält, zählt *load-paths* nicht
                       ; (um Errors wegen ".../../foo" z.B. auf DOS zu vermeiden):
                       (if (member #+(or DOS AMIGA ACORN-RISCOS) :PARENT
                                   #+(or UNIX OS/2 WIN32) ".."
                                   (pathname-directory filename)
                                   :test #'equal
                           )
                         '()
                         (mapcar #'pathname *load-paths*)
            )    )     )
      (let ((search-filename
              (merge-pathnames (merge-pathnames filename dir))
           ))
        (unless (member search-filename already-searched :test #'equal)
          (let ((xpathnames (directory search-filename :full t :circle t)))
            (when use-extensions
              ; nach passenden Extensions filtern:
              (setq xpathnames
                (delete-if-not ; hat xpathname eine der gegebenen Extensions?
                  #'(lambda (xpathname)
                      (member (pathname-type (first xpathname)) extensions
                              :test #-(or AMIGA OS/2 WIN32) #'string=
                                    #+(or AMIGA OS/2 WIN32) #'string-equal
                    ) )
                  xpathnames
            ) ) )
            (when xpathnames
              ; nach Datum sortiert, zurückgeben:
              (dolist (xpathname xpathnames)
                (setf (rest xpathname)
                      (apply #'encode-universal-time (third xpathname))
              ) )
              (return (mapcar #'first (sort xpathnames #'> :key #'rest)))
          ) )
          (push search-filename already-searched)
    ) ) )
) )

(LOAD "room")     ;; room, space

(LOAD "savemem")  ;; saveinitmem

;; At this point saveinitmem works.

; preliminary definition of CERROR, CLtL2 p. 887
(defun cerror (continue-format-string error-format-string &rest args)
  (if *error-handler*
    (apply *error-handler*
           (or continue-format-string t) error-format-string args
    )
    (progn
      (terpri *error-output*)
      (write-string "** - Continuable Error" *error-output*)
      (terpri *error-output*)
      (apply #'format *error-output* error-format-string args)
      (terpri *debug-io*)
      (if (and (interactive-stream-p *debug-io*) *break-driver*)
        (progn
          (write-string (ENGLISH "If you continue (by typing 'continue'): ")
                        *debug-io*
          )
          (apply #'format *debug-io* continue-format-string args)
          (funcall *break-driver* t)
        )
        (apply #'format *debug-io* continue-format-string args)
    ) )
) )

;; this should come before `compiler'
#+syscalls
(in-package "POSIX" :use '("LISP" "CLOS"))
#+syscalls
(export '(resolve-host-ipaddr hostent user-data file-stat sysinfo bogomips
          resource-usage-limits
          erf erfc j0 j1 jn y0 y1 yn gamma lgamma))
#+syscalls
(in-package "SYSTEM")

(LOAD "trace")     ;; TRACE

(LOAD "compiler")  ;; Compiler

(LOAD "disassem")  ;; Disassembler

(LOAD "defs2")     ;; CLtL2-Definitionen, optional

(LOAD "loop")      ;; CLtL2/ANSI-CL-LOOP, optional

(LOAD "clos")      ;; CLOS

(LOAD "conditio")  ;; Conditions

;; At this point the core Common Lisp is complete.


;; Fancy streams:

(LOAD "gstream")   ;; generic streams, optional

(LOAD "xcharin")   ;; extended character input, optional

(LOAD "keyboard")  ;; keyboard stream, optional

(when (or #+AMIGA t (find-package "SCREEN"))
  (LOAD "screen")  ;; Screen-Paket, optional
)


;; Environmental facilities:

#+AMIGA
(LOAD "amigasock") ;; sockets, optional

(LOAD "runprog")   ;; run-program and friends, optional


;; User interface:

(LOAD "query")     ;; querying the user

(LOAD "reploop")   ;; prompt, debugger, stepper

(LOAD "dribble")   ;; dribble

(LOAD "complete")  ;; completion

(LOAD "describe")  ;; apropos, describe

(LOAD "edit")      ;; edit-file, ed, uncompile


;; Random extensions:

;(LOAD "macros3")  ;; more macros, optional

#+FFI ; when (find-package "FFI")
(LOAD "foreign1")  ;; foreign function interface, optional

#+AMIGA
(when (find-symbol "%LIBCALL" "SYSTEM")
  (LOAD "affi1")   ;; simple FFI, optional
)

#+AMIGA (LOAD "rexx1") ;; Rexx-Schnittstelle, optional

#+syscalls
(LOAD "posix")     ;; POSIX/SUSV2 system calls and library functions, optional

(LOAD "defs3")     ;; the COMMON-LISP package

#+GETTEXT (LOAD "german") ;; Deutsche Meldungen
#+GETTEXT (LOAD "french") ;; Französische Meldungen
#+GETTEXT (LOAD "spanish") ;; Spanische Meldungen
#+GETTEXT (LOAD "dutch")  ;; Holländische Meldungen

(LOAD "config")    ;; configuration parameters to be adjusted by the user


(in-package "USER") ;; make the default package the current one

