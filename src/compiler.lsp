; CLISP - Compiler
; Bruno Haible 20.-30.09.1988, 05.-07.10.1988, 10.10.1988, 16.12.1988
;   Version für KCL 27.06.1989, 05.-07.07.1989
;   c-VALUES erweitert am 14.07.1989
;   label-operand in assemble-LAP korrigiert am 14.07.1989
;   ANODE-Komponenten SOURCE, STACKZ eliminiert am 14.07.1989
;     (konditionell von #+COMPILER-DEBUG abhängig)
;   Peephole-Optimierung-Protokoll konditionell von #+PEEPHOLE-DEBUG abhängig
;   Version für CLISP 28.07.1989-11.08.1989
;   Variablen-Optimierungen 10.03.1991
; Michael Stoll, September-Dezember 1991:
;   - Bytecode überarbeitet
;   - Code-Optimierung bzgl. Labels/Sprüngen verbessert
;   - kleine Verbesserung bei c-plus/c-minus,
;     Compilation von CxxxR in Folge von (CAR) und (CDR)
;   - SUBR-Aufrufe ohne Argument-Check zur Laufzeit,
;     SUBRs als Konstanten (via #.#'name)
;   - Aufrufe lokaler Funktionen ohne Argument-Check zur Laufzeit
;   - Rekursive Aufrufe durch Unterprogrammaufruf JSR, bei Endrekursion
;     JMPTAIL (entspricht PSETQ mit anschließendem Sprung)
;   - Verbesserung bei Aufruf einer Funktion mit Rest-Parametern via APPLY
; Bruno Haible, Februar-März 1992:
;   - detailliertere seclass, besseres PSETQ
;   - besseres Constant Folding
;   - Cross-Compilation
; Bruno Haible, 03.06.1992:
;   - Inline-Compilation von Aufrufen globaler Funktionen
; Bruno Haible, August 1993:
;   - Unterstützung für CLOS: generische Funktionen %GENERIC-FUNCTION-LAMBDA,
;     Optimierung unbenutzter Required-Parameter %OPTIMIZE-FUNCTION-LAMBDA
;   - GENERIC-FLET, GENERIC-LABELS
;   - Inline-Compilation von (APPLY (FUNCTION ...) ...)
; Bruno Haible, 09.06.1996:
;   - Bytecode plattform-unabhängig
; Bruno Haible, 04.12.1998:
;   - Optimierung generischer Funktionen durch RETGF
; Weitere Vorhaben:
;   - Variablen-Environments so verändern, dass Aufruf von lokalen Funktionen
;     mittels JSR/JMPTAIL möglich wird (d.h. nachträgliche Entscheidung, ob
;     Aufruf durch CALLC oder JSR)
;   - evtl. bessere Optimierung durch Datenflussanalyse
;   - Inline-Compilation von Aufrufen lokaler Funktionen

; Zur Cross-Compilation (wahlweise mit #+CLISP oder #-CLISP):
; CROSS, die Sprache und den Maschinenbezeichner in die Liste *features*
; aufnehmen, andere Maschinenbezeichner aus *features* herausnehmen.
; Dann den Compiler laden (evtl. compilieren und laden).
; Dann CROSS wieder aus der Liste *features* herausnehmen, und
; mit (cross:compile-file ...) Files compilieren.

; #-CROSS impliziert #+CLISP.

#-CROSS (in-package "LISP")
#-CROSS (export '(eval-env compile compile-file disassemble))
#-CROSS (pushnew ':compiler *features*)

#-CROSS (in-package "COMPILER")
#+CROSS (in-package "CROSS" :nicknames '("CLISP"))
#-CLISP (defmacro ENGLISH (x) x)
;; Konvention: Schreibe SYSTEM::PNAME für ein Symbol, das "zufällig" in
;; #<PACKAGE SYSTEM> sitzt, wir das Symbol aber nicht weiter benutzen.
;; Schreibe SYS::PNAME, wenn wir von dem Symbol irgendwelche Eigenschaften
;; voraussetzen. Schreibe COMPILER::PNAME, wenn der Compiler das Symbol
;; deklariert und es von anderen Programmteilen benutzt wird.
#+CLISP (import '(sys::function-name-p sys::parse-body sys::add-implicit-block
                  sys::make-load-time-eval
                  sys::closure-name sys::closure-codevec sys::closure-consts
                  sys::fixnump sys::short-float-p sys::single-float-p
                  sys::double-float-p sys::long-float-p
                  sys::search-file sys::date-format sys::line-number
                  sys::%funtabref sys::inlinable sys::constant-inlinable
                  sys::*compiling* sys::*compiling-from-file* sys::*inline-functions*
                  sys::*venv* sys::*fenv* sys::*benv* sys::*genv* sys::*denv*
                  sys::*toplevel-environment* sys::*toplevel-denv*
                  COMPILER::C-PROCLAIM COMPILER::C-PROCLAIM-CONSTANT
                  COMPILER::C-DEFUN COMPILER::C-PROVIDE COMPILER::C-REQUIRE
        )        )
#-CROSS (import '(sys::version sys::subr-info))

#+CROSS (shadow '(compile-file))
#+CROSS (export '(compile-file))

#-CLISP (shadow '(macroexpand-1 macroexpand))
#-CLISP
(progn
  (defun function-name-p (form)
    (or (symbolp form)
        (and (consp form) (eq (car form) 'SETF)
             (consp (setq form (cdr form))) (null (cdr form))
             (symbolp (car form))
  ) )   )
  (defun macroexpand-1 (form &optional (env (vector nil nil)))
    (if (and (consp form) (symbolp (car form)))
      (multiple-value-bind (a b c) (fenv-search (car form) (svref env 1))
        (declare (ignore c))
        (cond ((eq a 'system::macro) (values (funcall b form env) t))
              ((macro-function (car form))
               (values (funcall (macro-function (car form)) form env) t)
              )
              (t (values form nil))
      ) )
      (if (symbolp form)
        (multiple-value-bind (macrop expansion)
            (venv-search-macro form (svref env 0))
          (if macrop
            (values expansion t)
            (values form nil)
        ) )
        (values form nil)
  ) ) )
  (defun macroexpand (form &optional (env (vector nil nil)))
    (multiple-value-bind (a b) (macroexpand-1 form env)
      (if b
        (loop
          (multiple-value-setq (a b) (macroexpand-1 a env))
          (unless b (return (values a t)))
        )
        (values form nil)
  ) ) )
  (defun parse-body (body &optional docstring-allowed env)
    (do ((bodyr body (cdr bodyr))
         (declarations nil)
         (docstring nil)
         (form nil))
        ((null bodyr) (values bodyr declarations docstring))
      (cond ((and (stringp (car bodyr)) (cdr bodyr) (null docstring) docstring-allowed)
             (setq docstring (car bodyr))
            )
            ((not (listp (setq form (macroexpand (car bodyr) env))))
             (return (values bodyr declarations docstring))
            )
            ((eq (car form) 'DECLARE)
             (dolist (decl (cdr form)) (push decl declarations))
            )
            (t (return (values bodyr declarations docstring)))
  ) ) )
  (defun function-block-name (funname)
    (if (atom funname) funname (second funname))
  )
  (defun add-implicit-block (name body)
    (multiple-value-bind (body-rest declarations docstring)
        (parse-body body t (vector *venv* *fenv*))
      (append (if declarations (cons 'DECLARE declarations))
              (if docstring (list docstring))
              (list (list* 'BLOCK (function-block-name name) body-rest))
  ) ) )
  (defstruct (load-time-eval
              (:print-function
                (lambda (object stream depth)
                  (declare (ignore depth))
                  (write-string "#." stream)
                  (write (load-time-eval-form object) :stream stream)
              ) )
              (:constructor make-load-time-eval (form))
             )
    form
  )
  (defstruct (symbol-macro (:constructor make-symbol-macro (expansion)))
    expansion
  )
  (defun symbol-macro-expand (v)
    (and (boundp v) (symbol-macro-p (symbol-value v))
         (values t (symbol-macro-expansion (symbol-value v)))
  ) )
  (defparameter c-typep-alist1 nil)
  (defparameter c-typep-alist2 nil)
  (defparameter c-typep-alist3 nil)
  ; Sucht ein Programm-File. Siehe INIT.LSP :
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
      (dolist (dir (cons '#"" '()))
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
                                :test #'string=
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
  (defun make-macro-expander (macrodef)
    (let ((dummysym (make-symbol (symbol-name (car macrodef)))))
      (eval `(DEFMACRO ,dummysym ,@(cdr macrodef)))
      #'(lambda (form &rest env)
          (apply #'lisp:macroexpand-1 (cons dummysym (cdr form)) env)
        )
  ) )
  ; siehe DEFS1.LSP :
  (defun date-format ()
    (ENGLISH "~1{~5@*~D/~4@*~D/~3@*~D ~2@*~2,'0D.~1@*~2,'0D.~0@*~2,'0D~:}")
  )
  (defun sys::line-number (stream) nil)
)


; Version des Evaluators:
#+CROSS
(defconstant *big-endian*
  ;; When cross-compiling within CLISP, we generate compiled closures
  ;; in memory with CLISP's endianness. They will be written out to file
  ;; as little-endian.
  #+CLISP system::*big-endian*
  ;; When cross-compiling outside CLISP, we have no endianness reversion code
  ;; in the #Y printer. So let's generate little-endian compiled closures.
  #-CLISP nil
)
#+CROSS
(defun version ()
  (list '19071996)
)

(defconstant *keyword-package* (find-package "KEYWORD"))
(defconstant *lisp-package* (find-package "LISP"))

; Variablen für Top-Level-Aufruf:
(defvar *compiling* nil) ; gibt an, ob gerade beim Compilieren
; (defvar *error-count*) ; Anzahl der aufgetretenen Errors
; (defvar *warning-count*) ; Anzahl der aufgetretenen Warnungen
; (defvar *style-warning-count*) ; Anzahl der aufgetretenen Stil-Warnungen
(defvar *compile-warnings* t) ; ob Compiler-Warnungen ausgegeben werden
(defvar *compile-verbose* t) ; ob Compiler-Kommentare ausgegeben werden
(defvar *compile-print* nil) ; ob der Compiler ausgibt, wo er gerade ist
(defvar *compiling-from-file*) ; NIL oder T wenn von COMPILE-FILE aufgerufen
(defvar *compile-file-pathname* nil) ; CLtL2 S. 680
(defvar *compile-file-truename* nil) ; CLtL2 S. 680
(defvar *compile-file-lineno1* nil)
(defvar *compile-file-lineno2* nil)
(defvar *c-listing-output*) ; Compiler-Listing-Stream oder nil
(defvar *c-error-output*) ; Compiler-Error-Stream
; Es ist im wesentlichen
; *c-error-output* = (make-broadcast-stream *error-output* *c-listing-output*)
(defvar *known-special-vars*) ; Namen von deklarierten dynamischen Variablen
(defvar *constant-special-vars*) ; Namen und Werte von konstanten Variablen

; Variablen für COMPILE-FILE:
(defvar *fasoutput-stream* nil) ; Compiler-Output-Stream oder nil
(defvar *liboutput-stream* nil) ; Compiler-Library-Stream oder nil
(defvar *coutput-file* nil) ; Compiler-C-Output-File oder nil
(defvar *coutput-stream* nil) ; Compiler-C-Output-Stream oder nil
(defvar *functions-with-errors* nil) ; Namen der Funktionen, wo es Fehler gab
(defvar *known-functions*) ; Namen der bisher bekannten Funktionen,
                           ; wird vom Macroexpander von DEFUN verändert
(defvar *unknown-functions*) ; Namen der bisher unbekannten Funktionen
(defvar *unknown-free-vars*) ; Namen von undeklarierten dynamischen Variablen
(defvar *deprecated-functions*) ; Namen der bisher bisherigen unguten Funktionen
(defvar *inline-functions*) ; global inline-deklarierte Funktionssymbole
(defvar *notinline-functions*) ; global notinline-deklarierte Funktionssymbole
(defvar *inline-definitions*) ; Aliste globaler inlinebarer Funktionsdefinitionen
(defvar *inline-constants*) ; constant-inline-deklarierte Konstantensymbole
(defvar *notinline-constants*) ; constant-notinline-deklarierte Konstantensymbole
(defvar *user-declaration-types*) ; global definierte zusätzliche Deklarationen
(defvar *compiled-modules*) ; bereits "geladene" (compilierte) Modulnamen
(defvar *package-tasks*) ; noch durchzuführende Package-Anforderungen
(defvar *ffi-module* nil) ; Daten, die das FFI ansammelt

#|
The compiler's target is the virtual machine described in bytecode.html.

1. Pass des Compilers:
Macro-Expansion, Codegenerierung (symbolisch), Allokation von Variablen auf
dem STACK oder in Closures, Optimierung auf LISP-Ebene.
Danach steht für jede beteiligte Funktion das Stack-Layout fest.
Die Information steckt in einem Netz von ANODEs.
2. Pass des Compilers:
Auflösung der Variablenbezüge, Optimierung auf Code-Ebene
(Peephole-Optimierung), Kreation compilierter funktionaler Objekte.
3. Pass des Compilers:
Auflösung von Bezügen zwischen den einzelnen funktionalen Objekten.

|#

; externe Repräsentation einer Closure:
; #Y(name
;    #LängeY(Byte in Hex ... Byte in Hex)
;    weitere Konstanten
;   )

#-CLISP
(progn
  (defstruct (closure (:print-function print-closure))
    name    ; der Name der Closure
    codevec ; Liste der Bytes des Codevektor
    consts  ; Liste der Konstanten
  )
  (defun print-closure (closure stream depth)
    (declare (ignore depth))
    (write-string "#Y(" stream)
    (write (closure-name closure) :stream stream)
    (write-char #\space stream)
    (write-char #\# stream)
    (write (length (closure-codevec closure)) :stream stream :base 10. :radix nil :readably nil)
    (write-char #\Y stream)
    ;(write (closure-codevec closure) :stream stream :base 16.) ; stattdessen:
    (write-char #\( stream)
    (do ((i 0 (1- i))
         (L (closure-codevec closure) (cdr L)))
        ((endp L))
      (when (zerop i) (write-char #\newline stream) (setq i 25))
      (write-char #\space stream)
      (write (car L) :stream stream :base 16. :radix nil :readably nil)
    )
    (write-char #\) stream)
    (write-char #\newline stream)
    (dolist (x (closure-consts closure))
      (write-char #\space stream)
      (write x :stream stream)
    )
    (write-char #\) stream)
  )
)

#+CLISP
(progn
  (defsetf sys::%record-ref sys::%record-store)
  (defsetf closure-name (closure) (new-name)
    `(sys::%record-store ,closure 0 ,new-name)
  )
  (defun make-closure (&key name codevec consts)
    (sys::%make-closure name (sys::make-code-vector codevec) consts)
  )
)

#-CLISP
(set-dispatch-macro-character #\# #\Y
  #'(lambda (stream subchar arg)
      (declare (ignore subchar))
      (if arg
        ; Codevector lesen
        (let ((obj (let ((*read-base* 16.)) (read stream t nil t))))
          (unless (= (length obj) arg)
            (error (ENGLISH "Bad length of closure vector: ~S")
                   arg
          ) )
          obj
        )
        ; Closure lesen
        (let ((obj (read stream t nil t)))
          (make-closure :name (first obj) :codevec (second obj) :consts (cddr obj))
    ) ) )
)

; The instruction list is in bytecode.html.

; Instruktionen-Klassifikation:
; O = Instruktion ohne Operand
; K = numerischer Operand oder
;     Kurz-Operand (dann ist das Byte = short-code-ops[x] + Operand)
; N = numerischer Operand
; B = Byte-Operand
; L = Label-Operand
; NH = numerischer Operand, der eine Hashtable referenziert
; NC = numerischer Operand, der ein Handler-Cons referenziert
; LX = so viele Label-Operanden, wie der vorangehende Operand angibt

; Die Position in der Instruction-Table liefert den eigentlichen Code der
; Instruktion (>= 0, < short-code-base), Codes >= short-code-base werden
; von den K-Instruktionen belegt.
(defconstant instruction-table
  '#(; (1) Konstanten
     (NIL O) (PUSH-NIL N) (T O) (CONST K)
     ; (2) statische Variablen
     (LOAD K) (LOADI NNN) (LOADC NN) (LOADV NN) (LOADIC NNNN)
     (STORE K) (STOREI NNN) (STOREC NN) (STOREV NN) (STOREIC NNNN)
     ; (3) dynamische Variablen
     (GETVALUE N) (SETVALUE N) (BIND N) (UNBIND1 O) (UNBIND N) (PROGV O)
     ; (4) Stackoperationen
     (PUSH O) (POP O) (SKIP N) (SKIPI NNN) (SKIPSP NN)
     ; (5) Programmfluss und Sprünge
     (SKIP&RET N) (SKIP&RETGF N)
     (JMP L) (JMPIF L) (JMPIFNOT L) (JMPIF1 L) (JMPIFNOT1 L)
     (JMPIFATOM L) (JMPIFCONSP L) (JMPIFEQ L) (JMPIFNOTEQ L)
     (JMPIFEQTO NL) (JMPIFNOTEQTO NL) (JMPHASH NHL) (JMPHASHV NHL) (JSR L)
     (JMPTAIL NNL)
     ; (6) Environments und Closures
     (VENV O) (MAKE-VECTOR1&PUSH N) (COPY-CLOSURE NN)
     ; (7) Funktionsaufrufe
     (CALL NN) (CALL0 N) (CALL1 N) (CALL2 N)
     (CALLS1 B) (CALLS2 B) (CALLSR NB) (CALLC O) (CALLCKEY O)
     (FUNCALL N) (APPLY N)
     ; (8) optionale und Keyword-Argumente
     (PUSH-UNBOUND N) (UNLIST NN) (UNLIST* NN) (JMPIFBOUNDP NL) (BOUNDP N)
     (UNBOUND->NIL N)
     ; (9) Behandlung mehrerer Werte
     (VALUES0 O) (VALUES1 O) (STACK-TO-MV N) (MV-TO-STACK O) (NV-TO-STACK N)
     (MV-TO-LIST O) (LIST-TO-MV O) (MVCALLP O) (MVCALL O)
     ; (10) BLOCK
     (BLOCK-OPEN NL) (BLOCK-CLOSE O) (RETURN-FROM N) (RETURN-FROM-I NNN)
     ; (11) TAGBODY
     (TAGBODY-OPEN NLX) (TAGBODY-CLOSE-NIL O) (TAGBODY-CLOSE O) (GO NN)
     (GO-I NNNN)
     ; (12) CATCH und THROW
     (CATCH-OPEN L) (CATCH-CLOSE O) (THROW O)
     ; (13) UNWIND-PROTECT
     (UNWIND-PROTECT-OPEN L) (UNWIND-PROTECT-NORMAL-EXIT O)
     (UNWIND-PROTECT-CLOSE O) (UNWIND-PROTECT-CLEANUP O)
     ; (14) HANDLER
     (HANDLER-OPEN NC) (HANDLER-BEGIN&PUSH O)
     ; (15) einige Funktionen
     (NOT O) (EQ O) (CAR O) (CDR O) (CONS O) (SYMBOL-FUNCTION O) (SVREF O)
     (SVSET O) (LIST N) (LIST* N)
     ; (16) kombinierte Operationen
     (NIL&PUSH O) (T&PUSH O) (CONST&PUSH K)
     (LOAD&PUSH K) (LOADI&PUSH NNN) (LOADC&PUSH NN) (LOADV&PUSH NN) (POP&STORE N)
     (GETVALUE&PUSH N)
     (JSR&PUSH L)
     (COPY-CLOSURE&PUSH NN)
     (CALL&PUSH NN) (CALL1&PUSH N) (CALL2&PUSH N)
     (CALLS1&PUSH B) (CALLS2&PUSH B) (CALLSR&PUSH NB)
     (CALLC&PUSH O) (CALLCKEY&PUSH O)
     (FUNCALL&PUSH N) (APPLY&PUSH N)
     (CAR&PUSH O) (CDR&PUSH O) (CONS&PUSH O)
     (LIST&PUSH N) (LIST*&PUSH N)
     (NIL&STORE N) (T&STORE N) (LOAD&STOREC NNN)
     (CALLS1&STORE BN) (CALLS2&STORE BN) (CALLSR&STORE NBN)
     (LOAD&CDR&STORE N) (LOAD&CONS&STORE N) (LOAD&INC&STORE N) (LOAD&DEC&STORE N)
     (LOAD&CAR&STORE NN)
     (CALL1&JMPIF NL) (CALL1&JMPIFNOT NL)
     (CALL2&JMPIF NL) (CALL2&JMPIFNOT NL)
     (CALLS1&JMPIF BL) (CALLS1&JMPIFNOT BL)
     (CALLS2&JMPIF BL) (CALLS2&JMPIFNOT BL)
     (CALLSR&JMPIF NBL) (CALLSR&JMPIFNOT NBL)
     (LOAD&JMPIF NL) (LOAD&JMPIFNOT NL)
     (LOAD&CAR&PUSH N) (LOAD&CDR&PUSH N) (LOAD&INC&PUSH N) (LOAD&DEC&PUSH N)
     (CONST&SYMBOL-FUNCTION N) (CONST&SYMBOL-FUNCTION&PUSH N)
     (CONST&SYMBOL-FUNCTION&STORE NN)
     (APPLY&SKIP&RET NN) (FUNCALL&SKIP&RETGF NN)
)   )
(dotimes (i (length instruction-table))
  (setf (get (first (svref instruction-table i)) 'INSTRUCTION) i)
)
(defconstant instruction-codes
  (let ((hashtable (make-hash-table :test #'eq)))
    (dotimes (i (length instruction-table))
      (setf (gethash (first (svref instruction-table i)) hashtable) i)
    )
    hashtable
) )

; K-Instruktionen:
(defconstant instruction-table-K
  '#(LOAD LOAD&PUSH CONST CONST&PUSH STORE)
)
(defconstant short-code-base 157)
(defconstant short-code-opsize '#(15   25   21   30    8))
(defconstant short-code-ops '#(157  172  197  218  248));256


#|

Zwischensprache nach dem 1. Pass:
=================================

1. Konstanten:

(NIL)                            A0 := NIL, 1 Wert

(PUSH-NIL n)                     n-mal: -(STACK) := NIL, undefinierte Werte

(T)                              A0 := T, 1 Wert

(CONST const)                    A0 := 'const, 1 Wert

(FCONST fnode)                   A0 := das Compilat des fnode, 1 Wert

(BCONST block)                   A0 := das Block-Cons dieses Blockes (eine
                                 Konstante aus FUNC), 1 Wert

(GCONST tagbody)                 A0 := das Tagbody-Cons dieses Tagbody (eine
                                 Konstante aus FUNC), 1 Wert

2.,3. Variablen:

(GET var venvc stackz)           A0 := var, 1 Wert
                                 (venvc ist das aktuelle Closure-Venv,
                                  stackz der aktuelle Stackzustand)

(SET var venvc stackz)           var := A0, 1 Wert
                                 (venvc ist das aktuelle Closure-Venv,
                                  stackz der aktuelle Stackzustand)

(STORE n)                        (STACK+4*n) := A0, 1 Wert

(GETVALUE symbol)                A0 := (symbol-value 'symbol), 1 Wert

(SETVALUE symbol)                (setf (symbol-value 'symbol) A0), 1 Wert

(BIND const)                     bindet const (ein Symbol) dynamisch an A0.
                                 Undefinierte Werte.

(UNBIND1)                        löst einen Bindungsframe auf

(PROGV)                          bindet dynamisch die Symbole in der Liste
                                 (STACK)+ an die Werte in der Liste A0 und
                                 baut dabei genau einen Bindungsframe auf,
                                 undefinierte Werte

4. Stackoperationen:

(PUSH)                           -(STACK) := A0, undefinierte Werte

(POP)                            A0 := (STACK)+, 1 Wert

(UNWIND stackz1 stackz2 for-value) Führt ein Unwind binnen einer Funktion aus:
                                 Bereinigt den Stack, um vom Stackzustand
                                 stackz1 zum Stackzustand stackz2 zu kommen.
                                 Löst dazwischen liegende Frames auf. for-value
                                 gibt an, ob dabei die Werte A0/... gerettet
                                 werden müssen.

(UNWINDSP stackz1 stackz2)       modifiziert den SP, um vom Stackzustand
                                 stackz1 zum Stackzustand stackz2 zu kommen.
                                 STACK und die Werte A0/... bleiben unverändert.

5. Programmfluss und Sprünge:

(RET)                            beendet die Funktion mit den Werten A0/...

(RETGF)                          beendet die Funktion mit 1 Wert A0 und ruft
                                 evtl. A0 als Funktion auf, mit denselben
                                 Argumenten wie die aktuelle Funktion

(JMP label)                      Sprung zu label

(JMPIF label)                    falls A0 /= NIL : Sprung zu label.

(JMPIFNOT label)                 falls A0 = NIL : Sprung zu label.

(JMPIF1 label)                   falls A0 /= NIL : 1 Wert, Sprung zu label.

(JMPIFNOT1 label)                falls A0 = NIL : 1 Wert, Sprung zu label.

(JMPHASH test ((obj1 . label1) ... (objm . labelm)) label . labels)
                                 Sprung zu labeli, falls A0 = obji (im Sinne
                                 des angegebenen Vergleichs), sonst zu label.
                                 Undefinierte Werte.

(JSR m label)                    ruft den Code ab label als Unterprogramm auf,
                                 mit m Argumenten auf dem Stack

(BARRIER)                        wird nie erreicht, zählt als Wegsprung

6. Environments und Closures:

(VENV venvc stackz)              A0 := das Venv, das venvc entspricht
                                 (aus dem Stack, als Konstante aus
                                 FUNC, oder NIL, falls in FUNC nicht vorhanden),
                                 1 Wert
                                 (stackz ist der aktuelle Stackzustand)

(MAKE-VECTOR1&PUSH n)            kreiert einen simple-vector mit n+1 (n>=0)
                                 Komponenten und steckt A0 als Komponente 0
                                 hinein. -(STACK) := der neue Vektor.
                                 Undefinierte Werte.

(COPY-CLOSURE fnode n)           kopiert die Closure, die dem fnode entspricht
                                 und ersetzt in der Kopie für i=0,...,n-1 (n>0)
                                 die Komponente (CONST i) durch (STACK+4*(n-1-i)).
                                 STACK := STACK+4*n. A0 := Closure-Kopie, 1 Wert

7. Funktionsaufrufe:

(CALLP)                          beginnt den Aufbau eines Funktionsaufruf-Frames
                                 (wird im 2. Pass ersatzlos gestrichen)

(CALL k const)                   ruft die Funktion const mit k Argumenten
                                 (STACK+4*(k-1)),...,(STACK+4*0) auf,
                                 STACK:=STACK+4*k, Ergebnis kommt nach A0/...

(CALL0 const)                    ruft die Funktion const mit 0 Argumenten auf,
                                 Ergebnis kommt nach A0/...

(CALL1 const)                    ruft die Funktion const mit 1 Argument A0 auf,
                                 Ergebnis kommt nach A0/...

(CALL2 const)                    ruft die Funktion const mit 2 Argumenten (STACK)
                                 und A0 auf, STACK:=STACK+4,
                                 Ergebnis kommt nach A0/...

(CALLS1 n)                       ruft die Funktion (FUNTAB n)
(CALLS2 n)                       bzw. (FUNTAB 256+n)
                                 (ein SUBR ohne Rest-Parameter) auf,
                                 mit der korrekten Argumentezahl auf dem STACK.
                                 STACK wird bereinigt, Ergebnis kommt nach A0/...

(CALLSR m n)                     ruft die Funktion (FUNTABR n)
                                 (ein SUBR mit Rest-Parameter) auf,
                                 mit der korrekten Argumentezahl und zusätzlich
                                 m restlichen Argumenten auf dem STACK.
                                 STACK wird bereinigt, Ergebnis kommt nach A0/...

(CALLC)                          ruft die Funktion A0 (eine compilierte Closure
                                 ohne Keyword-Parameter) auf. Argumente
                                 sind schon im richtigen Format auf dem STACK,
                                 STACK wird bereinigt, Ergebnis kommt nach A0/...

(CALLCKEY)                       ruft die Funktion A0 (eine compilierte Closure
                                 mit Keyword-Parameter) auf. Argumente
                                 sind schon im richtigen Format auf dem STACK,
                                 STACK wird bereinigt, Ergebnis kommt nach A0/...

(FUNCALLP)                       fängt den Aufbau eines FUNCALL-Frames an,
                                 auszuführende Funktion ist in A0

(FUNCALL n)                      ruft die angegebene Funktion mit n (n>=0)
                                 Argumenten (alle auf dem Stack) auf,
                                 beseitigt den FUNCALL-Frame,
                                 Ergebnis kommt nach A0/...

(APPLYP)                         fängt den Aufbau eines APPLY-Frames an,
                                 auszuführende Funktion ist in A0

(APPLY n)                        ruft die angegebene Funktion mit n (n>=0)
                                 Argumenten (alle auf dem Stack) und weiteren
                                 Argumenten (Liste in A0) auf,
                                 beseitigt den APPLY-Frame,
                                 Ergebnis kommt nach A0/...

8. optionale und Keyword-Argumente:

(PUSH-UNBOUND n)                 n-mal: -(STACK) := #<UNBOUND>, undefinierte Werte

(UNLIST n m)                     Liste A0 n mal verkürzen: -(STACK) := (car A0),
                                 A0 := (cdr A0). Bei den letzten m Mal darf A0
                                 schon zu Ende sein, dann -(STACK) := #<UNBOUND>
                                 stattdessen. Am Schluss muss A0 = NIL sein,
                                 undefinierte Werte. 0 <= m <= n.

(UNLIST* n m)                    Liste A0 n mal verkürzen: -(STACK) := (car A0),
                                 A0 := (cdr A0). Bei den letzten m Mal darf A0
                                 schon zu Ende sein, dann -(STACK) := #<UNBOUND>.
                                 stattdessen. Dann -(STACK) := (nthcdr n A0),
                                 undefinierte Werte. 0 <= m <= n, n > 0.

(JMPIFBOUNDP var venvc stackz label)
                                 falls Variable /= #<UNBOUND> :
                                   Sprung zu label, A0 := Variable, 1 Wert.
                                 Sonst undefinierte Werte.
                                 (stackz ist der aktuelle Stackzustand)

(BOUNDP var venvc stackz)        A0 := (NIL falls Variable=#<UNBOUND>, T sonst),
                                 1 Wert
                                 (stackz ist der aktuelle Stackzustand)

9. Behandlung mehrerer Werte:

(VALUES0)                        A0 := NIL, 0 Werte

(VALUES1)                        A0 := A0, 1 Wert

(STACK-TO-MV n)                  holt n Werte von (STACK)+ herab,
                                 STACK:=STACK+4*n, n>1

(MV-TO-STACK)                    Multiple Values A0/A1/... auf -(STACK),
                                 1. Wert zuoberst, STACK:=STACK-4*D7.W,
                                 danach undefinierte Werte

(NV-TO-STACK n)                  die ersten n Werte (n>=0) auf -(STACK),
                                 1. Wert zuoberst, STACK:=STACK-4*n,
                                 undefinierte Werte

(MV-TO-LIST)                     Multiple Values A0/... als Liste nach A0,
                                 1 Wert

(LIST-TO-MV)                     A0/... := (values-list A0)

(MVCALLP)                        bereitet einen MULTIPLE-VALUE-CALL auf die
                                 Funktion in A0 vor

(MVCALL)                         führt einen MULTIPLE-VALUE-CALL mit den im
                                 Stack liegenden Argumenten aus

10. BLOCK:

(BLOCK-OPEN const label)         Legt einen Block-Cons (mit CAR=const und CDR=
                                 Framepointer) auf -(STACK) ab, baut einen
                                 Block-Frame auf. Bei einem RETURN auf diesen
                                 Frame wird zu label gesprungen.

(BLOCK-CLOSE)                    Verlasse den Block und baue dabei einen Block-
                                 Frame ab (inklusive der Block-Cons-Variablen)

(RETURN-FROM const)              Verlasse den Block, dessen Block-Cons angegeben
                                 ist, mit den Werten A0/...
(RETURN-FROM block)              Verlasse den angegebenen Block (sein Block-Cons
                                 kommt unter den BlockConsts von FUNC vor) mit
                                 den Werten A0/...
(RETURN-FROM block stackz)       Verlasse den angegebenen Block (sein Block-Cons
                                 kommt im Stack vor) mit den Werten A0/...

11. TAGBODY:

(TAGBODY-OPEN const label1 ... labelm)
                                 Legt einen Tagbody-Cons (mit CAR=const
                                 und CDR=Framepointer) auf -(STACK) ab, baut einen
                                 Tagbody-Frame auf. Bei einem GO mit Nummer l
                                 wird zu labell gesprungen.

(TAGBODY-CLOSE-NIL)              Verlasse den Tagbody und baue dabei einen
                                 Tagbody-Frame ab (inklusive der Tagbody-Cons-
                                 Variablen). A0 := NIL, 1 Wert

(TAGBODY-CLOSE)                  Verlasse den Tagbody und baue dabei einen
                                 Tagbody-Frame ab (inklusive der Tagbody-Cons-
                                 Variablen).

(GO const l)                     Springe im Tagbody, dessen Tagbody-Cons
                                 angegeben ist, an Tag (svref (car const) l)
(GO tagbody l)                   Springe im angegebenen Tagbody an Tag Nummer l
                                 in (tagbody-used-far tagbody)
(GO tagbody l stackz)            Springe im angegebenen Tagbody an Tag Nummer l
                                 in (tagbody-used-far tagbody), sein Tagbody-
                                 Cons liegt im Stack

12. CATCH und THROW:

(CATCH-OPEN label)               baut einen CATCH-Frame auf mit A0 als Tag;
                                 bei einem THROW auf dieses Tag wird zu label
                                 gesprungen

(CATCH-CLOSE)                    löst einen CATCH-Frame auf

(THROW)                          führt ein THROW auf den Catch-Tag (STACK)+
                                 aus, mit den Werten A0/...

13. UNWIND-PROTECT:

(UNWIND-PROTECT-OPEN label)      baut einen UNWIND-PROTECT-Frame auf; bei einem
                                 Unwind wird unter Rettung der Werte zu label
                                 gesprungen

(UNWIND-PROTECT-NORMAL-EXIT)     löst einen Unwind-Protect-Frame auf, schreibt
                                 eine Weitermach-Adresse auf SP, rettet die
                                 Werte und fängt an, den folgenden Cleanup-Code
                                 auszuführen

(UNWIND-PROTECT-CLOSE label)     beendet den Cleanup-Code: schreibt die
                                 geretteten Werte zurück, führt ein RTS aus.
                                 Der Cleanup-Code fängt bei label an.

(UNWIND-PROTECT-CLEANUP)         löst einen Unwind-Protect-Frame auf, schreibt
                                 eine Weitermach-Adresse und den PC auf SP,
                                 rettet die Werte und fängt an, den Cleanup-
                                 Code auszuführen

14. HANDLER:

(HANDLER-OPEN const stackz label1 ... labelm)
                                 baut einen HANDLER-Frame auf; const enthält
                                 die Condition-Typen; die entsprechenden
                                 Handler beginnen bei labeli

(HANDLER-BEGIN)                  beginnt einen Handler: stellt den SP-Zustand
                                 wie beim HANDLER-OPEN her,
                                 A0 := dem Handler übergebene Condition, 1 Wert

15. einige Funktionen:

(NOT)                            = (CALL1 #'NOT)

(EQ)                             = (CALL2 #'EQ)

(CAR)                            = (CALL1 #'CAR)

(CDR)                            = (CALL1 #'CDR)

(CONS)                           = (CALL2 #'CONS)

(ATOM)                           = (CALL1 #'ATOM)

(CONSP)                          = (CALL1 #'CONSP)

(SYMBOL-FUNCTION)                = (CALL1 #'SYMBOL-FUNCTION)

(SVREF)                          = (CALL2 #'SVREF)

(SVSET)                          (setf (svref (STACK) A0) (STACK+4)),
                                 A0 := (STACK+4), 1 Wert, STACK:=STACK+8

(LIST n)                         = (CALL n #'LIST), n>0

(LIST* n)                        = (CALL n+1 #'LIST*), n>0


Dabei bedeuten jeweils:

n, m, k     eine ganze Zahl >=0

stackz      einen Stackzustand (siehe STACK-VERWALTUNG).
            Das Stack-Layout steht nach dem 1. Pass fest.

venvc       das Environment der Closure-Variablen (siehe VARIABLEN-VERWALTUNG).
            Dies steht nach dem 1. Pass auch fest.

var         eine Variable (siehe VARIABLEN-VERWALTUNG). Ob sie
            special/konstant/lexikalisch ist, steht nach dem 1. Pass fest.

const       eine Konstante

symbol      ein Symbol

fun         entweder (CONST const) eine Konstante, die ein Symbol ist,
            oder (FUNTAB index) eine Indizierung in die feste Funktionentabelle.

fnode       ein fnode (siehe FUNKTIONEN-VERWALTUNG)

label       ein Label (uninterniertes Symbol)

block       ein Block-Descriptor (siehe BLOCK-VERWALTUNG)

test        EQ oder EQL oder EQUAL

for-value   NIL oder T

|#

#-CLISP ; Die Funktionentabelle steckt in EVAL.
(eval-when (compile load eval)
  ; die Funktionstabelle mit max. 3*256 Funktionen (spart Konstanten in FUNC) :
  (defconstant funtab
    '#(system::%funtabref system::subr-info
       sys::%copy-simple-vector #| svref system::%svstore |# row-major-aref
       system::row-major-store array-element-type array-rank array-dimension
       array-dimensions array-total-size adjustable-array-p bit-and bit-ior
       bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2 bit-orc1 bit-orc2
       bit-not array-has-fill-pointer-p fill-pointer system::set-fill-pointer
       vector-push vector-pop vector-push-extend make-array adjust-array
       standard-char-p graphic-char-p string-char-p alpha-char-p upper-case-p
       lower-case-p both-case-p digit-char-p alphanumericp char-code code-char
       character char-upcase char-downcase digit-char char-int int-char
       char-name char schar system::store-char system::store-schar string=
       string/= string< string> string<= string>= string-equal string-not-equal
       string-lessp string-greaterp string-not-greaterp string-not-lessp
       system::search-string= system::search-string-equal make-string
       system::string-both-trim nstring-upcase string-upcase nstring-downcase
       string-downcase nstring-capitalize string-capitalize string name-char
       substring
       symbol-value #| symbol-function |# boundp fboundp special-operator-p system::set-symbol-value makunbound
       fmakunbound #| values-list |# system::driver system::unwind-to-driver
       system::old-macro-function macroexpand macroexpand-1 proclaim eval evalhook applyhook
       constantp system::parse-body system::keyword-test
       invoke-debugger
       make-hash-table gethash system::puthash remhash maphash clrhash
       hash-table-count system::hash-table-iterator system::hash-table-iterate
       clos::class-gethash sxhash
       copy-readtable set-syntax-from-char set-macro-character
       get-macro-character make-dispatch-macro-character
       set-dispatch-macro-character get-dispatch-macro-character read
       read-preserving-whitespace read-delimited-list read-line read-char
       unread-char peek-char listen read-char-no-hang clear-input
       read-from-string parse-integer write prin1 print pprint princ
       write-to-string prin1-to-string princ-to-string write-char write-string
       write-line terpri fresh-line finish-output force-output clear-output
       system::line-position
       #| car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar
       cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar
       cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr cons |# tree-equal endp
       list-length nth #| first second third fourth |# fifth sixth seventh eighth
       ninth tenth #| rest |# nthcdr last make-list copy-list copy-alist copy-tree
       revappend nreconc system::list-nreverse butlast nbutlast ldiff rplaca
       system::%rplaca rplacd system::%rplacd subst subst-if subst-if-not nsubst
       nsubst-if nsubst-if-not sublis nsublis member member-if member-if-not
       tailp adjoin acons pairlis assoc assoc-if assoc-if-not rassoc rassoc-if
       rassoc-if-not
       lisp-implementation-type lisp-implementation-version software-type
       software-version identity get-universal-time get-internal-run-time
       get-internal-real-time system::%sleep system::%%time
       make-symbol find-package package-name package-nicknames rename-package
       package-use-list package-used-by-list package-shadowing-symbols
       list-all-packages intern find-symbol unintern export unexport import
       shadowing-import shadow use-package unuse-package
       make-package system::%in-package in-package
       find-all-symbols system::map-symbols
       system::map-external-symbols system::map-all-symbols
       parse-namestring pathname pathname-host pathname-device
       pathname-directory pathname-name pathname-type pathname-version
       file-namestring directory-namestring host-namestring merge-pathnames
       enough-namestring make-pathname namestring truename probe-file
       delete-file rename-file system::old-open directory cd make-dir delete-dir
       file-write-date file-author savemem
       #| eq |# eql equal equalp consp atom symbolp stringp numberp
       compiled-function-p #| null not |# system::closurep listp integerp
       system::fixnump rationalp floatp system::short-float-p
       system::single-float-p system::double-float-p system::long-float-p
       realp complexp streamp random-state-p readtablep hash-table-p pathnamep
       system::logical-pathname-p characterp functionp clos::generic-function-p
       packagep arrayp system::simple-array-p bit-vector-p vectorp
       simple-vector-p simple-string-p simple-bit-vector-p commonp type-of
       clos:class-of clos:find-class coerce
       system::%record-ref system::%record-store system::%record-length
       system::%structure-ref system::%structure-store system::%make-structure
       copy-structure system::%structure-type-p system::closure-name
       system::closure-codevec system::closure-consts system::make-code-vector
       system::%make-closure system::make-load-time-eval
       clos::structure-object-p clos::std-instance-p
       clos::old-%allocate-instance clos:slot-value clos::set-slot-value
       clos:slot-boundp clos:slot-makunbound clos:slot-exists-p
       system::sequencep elt system::%setelt subseq copy-seq length reverse
       nreverse make-sequence reduce fill replace remove remove-if remove-if-not
       delete delete-if delete-if-not remove-duplicates delete-duplicates
       substitute substitute-if substitute-if-not nsubstitute nsubstitute-if
       nsubstitute-if-not find find-if find-if-not position position-if
       position-if-not count count-if count-if-not mismatch search sort
       stable-sort merge
       system::file-stream-p make-synonym-stream system::synonym-stream-p
       system::broadcast-stream-p system::concatenated-stream-p
       make-two-way-stream system::two-way-stream-p make-echo-stream
       system::echo-stream-p make-string-input-stream
       system::string-input-stream-index make-string-output-stream
       get-output-stream-string system::make-string-push-stream
       system::string-stream-p input-stream-p output-stream-p
       system::built-in-stream-element-type stream-external-format
       system::built-in-stream-close read-byte write-byte file-position
       file-length
       system::%putd system::%proclaim-constant get getf get-properties
       system::%putplist system::%put remprop symbol-package symbol-plist
       symbol-name keywordp gensym system::special-variable-p gensym
       system::decimal-string zerop plusp minusp oddp evenp 1+ 1- conjugate exp
       expt log sqrt isqrt abs phase signum sin cos tan cis asin acos atan sinh
       cosh tanh asinh acosh atanh float rational rationalize numerator
       denominator floor ceiling truncate round mod rem ffloor fceiling
       ftruncate fround decode-float scale-float float-radix float-sign
       float-digits float-precision integer-decode-float complex realpart
       imagpart lognand lognor logandc1 logandc2 logorc1 logorc2 boole lognot
       logtest logbitp ash logcount integer-length byte byte-size byte-position
       ldb ldb-test mask-field dpb deposit-field random make-random-state !
       exquo long-float-digits system::%set-long-float-digits system::log2
       system::log10
       system::%copy-generic-function
       vector aref system::store array-in-bounds-p array-row-major-index bit
       sbit char= char/= char< char> char<= char>= char-equal char-not-equal
       char-lessp char-greaterp char-not-greaterp char-not-lessp string-concat
       apply system::%funcall funcall mapcar maplist mapc mapl mapcan mapcon
       values error system::error-of-type clos::class-tuple-gethash list list*
       append nconc concatenate map some every notany notevery
       make-broadcast-stream make-concatenated-stream = /= < > <= >= max min
       + - * / gcd lcm logior logxor logand logeqv
  )   )
  (defun %funtabref (index)
    (if (and (<= 0 index) (< index (length funtab))) (svref funtab index) nil)
  )
)
#+CROSS
(eval-when (compile load eval)
  (defun subr-info (sym)
    (values-list
      (assoc sym
        '(; Das ist die Tabelle aller SUBRs, wie in SUBR.D.
          ; SUBRs, die in verschiedenen Implementationen verschiedene
          ; Signaturen haben und/oder deren Spezifikation sich noch ändern
          ; könnte, sind dabei allerdings auskommentiert.
          (! 1 0 nil nil nil)
          (system::%%time 0 0 nil nil nil)
          (system::%defseq 1 0 nil nil nil)
          (system::%exit 0 1 nil nil nil)
          (system::%funcall 1 0 t nil nil)
          (system::%funtabref 1 0 nil nil nil)
          (system::%in-package 1 0 nil (:nicknames :use :case-sensitive) nil)
          (system::%make-closure 3 0 nil nil nil)
          (system::%make-structure 2 0 nil nil nil)
          (system::%proclaim-constant 2 0 nil nil nil)
          (system::%put 3 0 nil nil nil)
          (system::%putd 2 0 nil nil nil)
          (system::%putplist 2 0 nil nil nil)
          (system::%record-length 1 0 nil nil nil)
          (system::%record-ref 2 0 nil nil nil)
          (system::%record-store 3 0 nil nil nil)
          (system::%rplaca 2 0 nil nil nil)
          (system::%rplacd 2 0 nil nil nil)
          (system::%set-long-float-digits 1 0 nil nil nil)
          (system::%setelt 3 0 nil nil nil)
          ;(system::%sleep 1 0 nil nil nil)
          ;(system::%sleep 2 0 nil nil nil)
          (system::%structure-ref 3 0 nil nil nil)
          (system::%structure-store 4 0 nil nil nil)
          (system::%structure-type-p 2 0 nil nil nil)
          (system::%svstore 3 0 nil nil nil)
          (* 0 0 t nil nil)
          (+ 0 0 t nil nil)
          (- 1 0 t nil nil)
          (/ 1 0 t nil nil)
          (/= 1 0 t nil nil)
          (1+ 1 0 nil nil nil)
          (1- 1 0 nil nil nil)
          (< 1 0 t nil nil)
          (<= 1 0 t nil nil)
          (= 1 0 t nil nil)
          (> 1 0 t nil nil)
          (>= 1 0 t nil nil)
          (abs 1 0 nil nil nil)
          (acons 3 0 nil nil nil)
          (acos 1 0 nil nil nil)
          (acosh 1 0 nil nil nil)
          (adjoin 2 0 nil (:test :test-not :key) nil)
          (adjust-array 2 0 nil (:element-type :initial-element :initial-contents :fill-pointer :displaced-to :displaced-index-offset) nil)
          (adjustable-array-p 1 0 nil nil nil)
          (alpha-char-p 1 0 nil nil nil)
          (alphanumericp 1 0 nil nil nil)
          (append 0 0 t nil nil)
          (apply 2 0 t nil nil)
          (applyhook 4 1 nil nil nil)
          (aref 1 0 t nil nil)
          (array-dimension 2 0 nil nil nil)
          (array-dimensions 1 0 nil nil nil)
          (array-element-type 1 0 nil nil nil)
          (array-has-fill-pointer-p 1 0 nil nil nil)
          (array-in-bounds-p 1 0 t nil nil)
          (array-rank 1 0 nil nil nil)
          (system::array-reader 3 0 nil nil nil)
          (array-row-major-index 1 0 t nil nil)
          (array-total-size 1 0 nil nil nil)
          (arrayp 1 0 nil nil nil)
          (ash 2 0 nil nil nil)
          (asin 1 0 nil nil nil)
          (asinh 1 0 nil nil nil)
          (assoc 2 0 nil (:test :test-not :key) nil)
          (assoc-if 2 0 nil (:key) nil)
          (assoc-if-not 2 0 nil (:key) nil)
          (atan 1 1 nil nil nil)
          (atanh 1 0 nil nil nil)
          (atom 1 0 nil nil nil)
          (system::binary-reader 3 0 nil nil nil)
          (bit 1 0 t nil nil)
          (bit-and 2 1 nil nil nil)
          (bit-andc1 2 1 nil nil nil)
          (bit-andc2 2 1 nil nil nil)
          (bit-eqv 2 1 nil nil nil)
          (bit-ior 2 1 nil nil nil)
          (bit-nand 2 1 nil nil nil)
          (bit-nor 2 1 nil nil nil)
          (bit-not 1 1 nil nil nil)
          (bit-orc1 2 1 nil nil nil)
          (bit-orc2 2 1 nil nil nil)
          (bit-vector-p 1 0 nil nil nil)
          (system::bit-vector-reader 3 0 nil nil nil)
          (bit-xor 2 1 nil nil nil)
          (boole 3 0 nil nil nil)
          (both-case-p 1 0 nil nil nil)
          (boundp 1 0 nil nil nil)
          (system::broadcast-stream-p 1 0 nil nil nil)
          (system::built-in-stream-close 1 0 nil (:abort) nil)
          (system::built-in-stream-element-type 1 0 nil nil nil)
          (butlast 1 1 nil nil nil)
          (byte 2 0 nil nil nil)
          (byte-position 1 0 nil nil nil)
          (byte-size 1 0 nil nil nil)
          (caaaar 1 0 nil nil nil)
          (caaadr 1 0 nil nil nil)
          (caaar 1 0 nil nil nil)
          (caadar 1 0 nil nil nil)
          (caaddr 1 0 nil nil nil)
          (caadr 1 0 nil nil nil)
          (caar 1 0 nil nil nil)
          (cadaar 1 0 nil nil nil)
          (cadadr 1 0 nil nil nil)
          (cadar 1 0 nil nil nil)
          (caddar 1 0 nil nil nil)
          (cadddr 1 0 nil nil nil)
          (caddr 1 0 nil nil nil)
          (cadr 1 0 nil nil nil)
          (car 1 0 nil nil nil)
          (cd 0 1 nil nil nil)
          (cdaaar 1 0 nil nil nil)
          (cdaadr 1 0 nil nil nil)
          (cdaar 1 0 nil nil nil)
          (cdadar 1 0 nil nil nil)
          (cdaddr 1 0 nil nil nil)
          (cdadr 1 0 nil nil nil)
          (cdar 1 0 nil nil nil)
          (cddaar 1 0 nil nil nil)
          (cddadr 1 0 nil nil nil)
          (cddar 1 0 nil nil nil)
          (cdddar 1 0 nil nil nil)
          (cddddr 1 0 nil nil nil)
          (cdddr 1 0 nil nil nil)
          (cddr 1 0 nil nil nil)
          (cdr 1 0 nil nil nil)
          (ceiling 1 1 nil nil nil)
          (char 2 0 nil nil nil)
          (char-code 1 0 nil nil nil)
          (char-downcase 1 0 nil nil nil)
          (char-equal 1 0 t nil nil)
          (char-greaterp 1 0 t nil nil)
          (char-int 1 0 nil nil nil)
          (char-lessp 1 0 t nil nil)
          (char-name 1 0 nil nil nil)
          (char-not-equal 1 0 t nil nil)
          (char-not-greaterp 1 0 t nil nil)
          (char-not-lessp 1 0 t nil nil)
          (system::char-reader 3 0 nil nil nil)
          (char-upcase 1 0 nil nil nil)
          (char/= 1 0 t nil nil)
          (char< 1 0 t nil nil)
          (char<= 1 0 t nil nil)
          (char= 1 0 t nil nil)
          (char> 1 0 t nil nil)
          (char>= 1 0 t nil nil)
          (character 1 0 nil nil nil)
          (characterp 1 0 nil nil nil)
          (cis 1 0 nil nil nil)
          (clos::class-gethash 2 0 nil nil nil)
          (clos:class-of 1 0 nil nil nil)
          (clos::class-p 1 0 nil nil nil)
          (clos::class-tuple-gethash 2 0 t nil nil)
          (clear-input 0 1 nil nil nil)
          (clear-output 0 1 nil nil nil)
          (system::closure-codevec 1 0 nil nil nil)
          (system::closure-consts 1 0 nil nil nil)
          (system::closure-name 1 0 nil nil nil)
          (system::closure-reader 3 0 nil nil nil)
          (system::closurep 1 0 nil nil nil)
          (clrhash 1 0 nil nil nil)
          (code-char 1 0 nil nil nil)
          (coerce 2 0 nil nil nil)
          (system::comment-reader 3 0 nil nil nil)
          (commonp 1 0 nil nil nil)
          (compiled-function-p 1 0 nil nil nil)
          (complex 1 1 nil nil nil)
          (system::complex-reader 3 0 nil nil nil)
          (complexp 1 0 nil nil nil)
          (concatenate 1 0 t nil nil)
          (system::concatenated-stream-p 1 0 nil nil nil)
          (conjugate 1 0 nil nil nil)
          (cons 2 0 nil nil nil)
          (consp 1 0 nil nil nil)
          (constantp 1 0 nil nil nil)
          (copy-alist 1 0 nil nil nil)
          (system::%copy-generic-function 2 0 nil nil nil)
          (copy-list 1 0 nil nil nil)
          (copy-readtable 0 2 nil nil nil)
          (copy-seq 1 0 nil nil nil)
          (system::%copy-simple-vector 1 0 nil nil nil)
          (copy-structure 1 0 nil nil nil)
          (copy-tree 1 0 nil nil nil)
          (cos 1 0 nil nil nil)
          (cosh 1 0 nil nil nil)
          (count 2 0 nil (:from-end :start :end :key :test :test-not) nil)
          (count-if 2 0 nil (:from-end :start :end :key) nil)
          (count-if-not 2 0 nil (:from-end :start :end :key) nil)
          (system::debug 0 0 nil nil nil)
          (system::decimal-string 1 0 nil nil nil)
          (decode-float 1 0 nil nil nil)
          (delete 2 0 nil (:from-end :start :end :key :test :test-not :count) nil)
          (delete-dir 1 0 nil nil nil)
          (delete-duplicates 1 0 nil (:from-end :start :end :key :test :test-not) nil)
          (delete-file 1 0 nil nil nil)
          (delete-if 2 0 nil (:from-end :start :end :key :count) nil)
          (delete-if-not 2 0 nil (:from-end :start :end :key :count) nil)
          (denominator 1 0 nil nil nil)
          (deposit-field 3 0 nil nil nil)
          (system::describe-frame 2 0 nil nil nil)
          (digit-char 1 1 nil nil nil)
          (digit-char-p 1 1 nil nil nil)
          (directory 0 1 nil (:circle :full) nil)
          (directory-namestring 1 0 nil nil nil)
          (system::double-float-p 1 0 nil nil nil)
          (dpb 3 0 nil nil nil)
          (system::driver 1 0 nil nil nil)
          (system::echo-stream-p 1 0 nil nil nil)
          (eighth 1 0 nil nil nil)
          (elt 2 0 nil nil nil)
          (endp 1 0 nil nil nil)
          (enough-namestring 1 1 nil nil nil)
          (eq 2 0 nil nil nil)
          (eql 2 0 nil nil nil)
          (equal 2 0 nil nil nil)
          (equalp 2 0 nil nil nil)
          (error 1 0 t nil nil)
          (system::error-of-type 2 0 t nil nil)
          (eval 1 0 nil nil nil)
          (system::eval-at 2 0 nil nil nil)
          (system::eval-frame-p 1 0 nil nil nil)
          (evalhook 3 1 nil nil nil)
          (evenp 1 0 nil nil nil)
          (every 2 0 t nil nil)
          ;(execute 1 2 nil nil nil)
          ;(execute 1 0 t nil nil)
          (exp 1 0 nil nil nil)
          (export 1 1 nil nil nil)
          (expt 2 0 nil nil nil)
          (exquo 2 0 nil nil nil)
          (fboundp 1 0 nil nil nil)
          (fceiling 1 1 nil nil nil)
          (system::feature-reader 3 0 nil nil nil)
          (ffloor 1 1 nil nil nil)
          (fifth 1 0 nil nil nil)
          (file-author 1 0 nil nil nil)
          (file-length 1 0 nil nil nil)
          (file-namestring 1 0 nil nil nil)
          (file-position 1 1 nil nil nil)
          (system::file-stream-p 1 0 nil nil nil)
          (file-string-length 2 0 nil nil nil)
          (file-write-date 1 0 nil nil nil)
          (fill 2 0 nil (:start :end) nil)
          (fill-pointer 1 0 nil nil nil)
          (find 2 0 nil (:from-end :start :end :key :test :test-not) nil)
          (find-all-symbols 1 0 nil nil nil)
          (clos:find-class 1 2 nil nil nil)
          (find-if 2 0 nil (:from-end :start :end :key) nil)
          (find-if-not 2 0 nil (:from-end :start :end :key) nil)
          (find-package 1 0 nil nil nil)
          (find-symbol 1 1 nil nil nil)
          (finish-output 0 1 nil nil nil)
          (first 1 0 nil nil nil)
          (system::fixnump 1 0 nil nil nil)
          (float 1 1 nil nil nil)
          (float-digits 1 1 nil nil nil)
          (float-precision 1 0 nil nil nil)
          (float-radix 1 0 nil nil nil)
          (float-sign 1 1 nil nil nil)
          (floatp 1 0 nil nil nil)
          (floor 1 1 nil nil nil)
          (fmakunbound 1 0 nil nil nil)
          (force-output 0 1 nil nil nil)
          (fourth 1 0 nil nil nil)
          (system::frame-down 2 0 nil nil nil)
          (system::frame-down-1 2 0 nil nil nil)
          (system::frame-up 2 0 nil nil nil)
          (system::frame-up-1 2 0 nil nil nil)
          (fresh-line 0 1 nil nil nil)
          (fround 1 1 nil nil nil)
          (ftruncate 1 1 nil nil nil)
          (funcall 1 0 t nil nil)
          (system::function-reader 3 0 nil nil nil)
          (functionp 1 0 nil nil nil)
          (gc 0 0 nil nil nil)
          (gcd 0 0 t nil nil)
          (clos::generic-function-p 1 0 nil nil nil)
          (gensym 0 1 nil nil nil)
          (get 2 1 nil nil nil)
          (get-dispatch-macro-character 2 1 nil nil nil)
          (get-internal-real-time 0 0 nil nil nil)
          (get-internal-run-time 0 0 nil nil nil)
          (get-macro-character 1 1 nil nil nil)
          (get-output-stream-string 1 0 nil nil nil)
          (get-properties 2 0 nil nil nil)
          (get-universal-time 0 0 nil nil nil)
          (getf 2 1 nil nil nil)
          (gethash 2 1 nil nil nil)
          (graphic-char-p 1 0 nil nil nil)
          (hash-table-count 1 0 nil nil nil)
          (hash-table-rehash-size 1 0 nil nil nil)
          (hash-table-rehash-threshold 1 0 nil nil nil)
          (hash-table-size 1 0 nil nil nil)
          (hash-table-test 1 0 nil nil nil)
          (system::hash-table-iterate 1 0 nil nil nil)
          (system::hash-table-iterator 1 0 nil nil nil)
          (hash-table-p 1 0 nil nil nil)
          (system::hexadecimal-reader 3 0 nil nil nil)
          (host-namestring 1 0 nil nil nil)
          (identity 1 0 nil nil nil)
          (imagpart 1 0 nil nil nil)
          (import 1 1 nil nil nil)
          (in-package 1 0 nil (:nicknames :use :case-sensitive) nil)
          (system::initial-contents-aux 1 0 nil nil nil)
          (input-stream-p 1 0 nil nil nil)
          (int-char 1 0 nil nil nil)
          (integer-decode-float 1 0 nil nil nil)
          (integer-length 1 0 nil nil nil)
          (integerp 1 0 nil nil nil)
          (intern 1 1 nil nil nil)
          (invoke-debugger 1 0 nil nil nil)
          (isqrt 1 0 nil nil nil)
          (system::keyword-test 2 0 nil nil nil)
          (keywordp 1 0 nil nil nil)
          (system::label-definiion-reader 3 0 nil nil nil)
          (system::label-reference-reader 3 0 nil nil nil)
          (last 1 1 nil nil nil)
          (lcm 0 0 t nil nil)
          (ldb 2 0 nil nil nil)
          (ldb-test 2 0 nil nil nil)
          (ldiff 2 0 nil nil nil)
          (length 1 0 nil nil nil)
          (system::line-comment-reader 2 0 nil nil nil)
          (system::line-number 1 0 nil nil nil)
          (system::line-position 0 1 nil nil nil)
          (lisp-implementation-type 0 0 nil nil nil)
          (lisp-implementation-version 0 0 nil nil nil)
          (list 0 0 t nil nil)
          (list* 1 0 t nil nil)
          (system::list-access 2 0 nil nil nil)
          (system::list-access-set 3 0 nil nil nil)
          (list-all-packages 0 0 nil nil nil)
          (system::list-elt 2 0 nil nil nil)
          (system::list-endtest 2 0 nil nil nil)
          (system::list-fe-init 1 0 nil nil nil)
          (system::list-fe-init-end 2 0 nil nil nil)
          (system::list-init-start 2 0 nil nil nil)
          (list-length 1 0 nil nil nil)
          (system::list-llength 1 0 nil nil nil)
          (system::list-nreverse 1 0 nil nil nil)
          (system::list-set-elt 3 0 nil nil nil)
          (system::list-upd 2 0 nil nil nil)
          (listen 0 1 nil nil nil)
          (listp 1 0 nil nil nil)
          (system::load-eval-reader 3 0 nil nil nil)
          (log 1 1 nil nil nil)
          (system::log10 1 0 nil nil nil)
          (system::log2 1 0 nil nil nil)
          (logand 0 0 t nil nil)
          (logandc1 2 0 nil nil nil)
          (logandc2 2 0 nil nil nil)
          (logbitp 2 0 nil nil nil)
          (logcount 1 0 nil nil nil)
          (logeqv 0 0 t nil nil)
          (system::logical-pathname-p 1 0 nil nil nil)
          (logior 0 0 t nil nil)
          (lognand 2 0 nil nil nil)
          (lognor 2 0 nil nil nil)
          (lognot 1 0 nil nil nil)
          (logorc1 2 0 nil nil nil)
          (logorc2 2 0 nil nil nil)
          (logtest 2 0 nil nil nil)
          (logxor 0 0 t nil nil)
          (long-float-digits 0 0 nil nil nil)
          (system::long-float-p 1 0 nil nil nil)
          (lower-case-p 1 0 nil nil nil)
          (system::lpar-reader 2 0 nil nil nil)
          ;(machine-instance 0 0 nil nil nil)
          ;(machine-type 0 0 nil nil nil)
          ;(machine-version 0 0 nil nil nil)
          (macro-function 1 1 nil nil nil)
          (macroexpand 1 1 nil nil nil)
          (macroexpand-1 1 1 nil nil nil)
          (make-array 1 0 nil (:adjustable :element-type :initial-element :initial-contents :fill-pointer :displaced-to :displaced-index-offset) nil)
          (system::make-bit-vector 1 0 nil nil nil)
          (make-broadcast-stream 0 0 t nil nil)
          (make-buffered-input-stream 2 0 nil nil nil)
          (make-buffered-output-stream 1 0 nil nil nil)
          (system::make-code-vector 1 0 nil nil nil)
          (make-concatenated-stream 0 0 t nil nil)
          (make-dir 1 0 nil nil nil)
          (make-dispatch-macro-character 1 2 nil nil nil)
          (make-echo-stream 2 0 nil nil nil)
          (make-hash-table 0 0 nil (:initial-contents :test :size :rehash-size :rehash-threshold) nil)
          (make-list 1 0 nil (:initial-element) nil)
          (system::make-load-time-eval 1 0 nil nil nil)
          (make-package 1 0 nil (:nicknames :use :case-sensitive) nil)
          (make-pathname 0 0 nil (:defaults :case :host :device :directory :name :type :version) nil)
          #+(or UNIX OS/2 WIN32) (make-pipe-input-stream 1 0 nil (:element-type :external-format :buffered) nil)
          #+(or UNIX OS/2 WIN32) (make-pipe-output-stream 1 0 nil (:element-type :external-format :buffered) nil)
          #+(or UNIX OS/2 WIN32) (make-pipe-io-stream 1 0 nil (:element-type :external-format :buffered) nil)
          (make-random-state 0 1 nil nil nil)
          (make-sequence 2 0 nil (:initial-element :update) nil)
          (make-string 1 0 nil (:initial-element :element-type) nil)
          (make-string-input-stream 1 2 nil nil nil)
          (make-string-output-stream 0 0 nil (:element-type :line-position) nil)
          (system::make-string-push-stream 1 0 nil nil nil)
          (make-symbol 1 0 nil nil nil)
          (make-synonym-stream 1 0 nil nil nil)
          (make-two-way-stream 2 0 nil nil nil)
          (make-weak-pointer 1 0 nil nil nil)
          (makunbound 1 0 nil nil nil)
          (map 3 0 t nil nil)
          (system::map-all-symbols 1 0 nil nil nil)
          (system::map-external-symbols 2 0 nil nil nil)
          (system::map-symbols 2 0 nil nil nil)
          (mapc 2 0 t nil nil)
          (mapcan 2 0 t nil nil)
          (mapcar 2 0 t nil nil)
          (mapcon 2 0 t nil nil)
          (maphash 2 0 nil nil nil)
          (mapl 2 0 t nil nil)
          (maplist 2 0 t nil nil)
          (mask-field 2 0 nil nil nil)
          (max 1 0 t nil nil)
          (member 2 0 nil (:test :test-not :key) nil)
          (member-if 2 0 nil (:key) nil)
          (member-if-not 2 0 nil (:key) nil)
          (merge 4 0 nil (:key) nil)
          (merge-pathnames 1 2 nil (:wild) nil)
          (min 1 0 t nil nil)
          (minusp 1 0 nil nil nil)
          (mismatch 2 0 nil (:from-end :start1 :end1 :start2 :end2 :key :test :test-not) nil)
          (mod 2 0 nil nil nil)
          (name-char 1 0 nil nil nil)
          (namestring 1 1 nil nil nil)
          (nbutlast 1 1 nil nil nil)
          (nconc 0 0 t nil nil)
          (ninth 1 0 nil nil nil)
          (not 1 0 nil nil nil)
          (system::not-feature-reader 3 0 nil nil nil)
          (system::not-readable-reader 3 0 nil nil nil)
          (notany 2 0 t nil nil)
          (notevery 2 0 t nil nil)
          (nreconc 2 0 nil nil nil)
          (nreverse 1 0 nil nil nil)
          (nstring-capitalize 1 0 nil (:start :end) nil)
          (nstring-downcase 1 0 nil (:start :end) nil)
          (nstring-upcase 1 0 nil (:start :end) nil)
          (nsublis 2 0 nil (:test :test-not :key) nil)
          (nsubst 3 0 nil (:test :test-not :key) nil)
          (nsubst-if 3 0 nil (:key) nil)
          (nsubst-if-not 3 0 nil (:key) nil)
          (nsubstitute 3 0 nil (:from-end :start :end :key :test :test-not :count) nil)
          (nsubstitute-if 3 0 nil (:from-end :start :end :key :count) nil)
          (nsubstitute-if-not 3 0 nil (:from-end :start :end :key :count) nil)
          (nth 2 0 nil nil nil)
          (nthcdr 2 0 nil nil nil)
          (null 1 0 nil nil nil)
          (numberp 1 0 nil nil nil)
          (numerator 1 0 nil nil nil)
          (system::octal-reader 3 0 nil nil nil)
          (oddp 1 0 nil nil nil)
          (open 1 0 nil (:direction :element-type :if-exists :if-does-not-exist :external-format :buffered) nil)
          (output-stream-p 1 0 nil nil nil)
          (package-name 1 0 nil nil nil)
          (package-nicknames 1 0 nil nil nil)
          (package-shadowing-symbols 1 0 nil nil nil)
          (package-use-list 1 0 nil nil nil)
          (package-used-by-list 1 0 nil nil nil)
          (packagep 1 0 nil nil nil)
          (pairlis 2 1 nil nil nil)
          (system::parse-body 1 2 nil nil nil)
          (parse-integer 1 0 nil (:start :end :radix :junk-allowed) nil)
          (parse-namestring 1 2 nil (:start :end :junk-allowed) nil)
          (pathname 1 0 nil nil nil)
          (pathname-device 1 0 nil (:case) nil)
          (pathname-directory 1 0 nil (:case) nil)
          (pathname-host 1 0 nil (:case) nil)
          (pathname-match-p 2 0 nil nil nil)
          (pathname-name 1 0 nil (:case) nil)
          (system::pathname-reader 3 0 nil nil nil)
          (pathname-type 1 0 nil (:case) nil)
          (pathname-version 1 0 nil nil nil)
          (pathnamep 1 0 nil nil nil)
          (peek-char 0 5 nil nil nil)
          (phase 1 0 nil nil nil)
          (plusp 1 0 nil nil nil)
          (position 2 0 nil (:from-end :start :end :key :test :test-not) nil)
          (position-if 2 0 nil (:from-end :start :end :key) nil)
          (position-if-not 2 0 nil (:from-end :start :end :key) nil)
          (pprint 1 1 nil nil nil)
          (prin1 1 1 nil nil nil)
          (prin1-to-string 1 0 nil nil nil)
          (princ 1 1 nil nil nil)
          (princ-to-string 1 0 nil nil nil)
          (print 1 1 nil nil nil)
          (probe-file 1 0 nil nil nil)
          (proclaim 1 0 nil nil nil)
          (system::puthash 3 0 nil nil nil)
          (system::quote-reader 2 0 nil nil nil)
          (system::radix-reader 3 0 nil nil nil)
          (random 1 1 nil nil nil)
          (random-state-p 1 0 nil nil nil)
          (rassoc 2 0 nil (:test :test-not :key) nil)
          (rassoc-if 2 0 nil (:key) nil)
          (rassoc-if-not 2 0 nil (:key) nil)
          (rational 1 0 nil nil nil)
          (rationalize 1 0 nil nil nil)
          (rationalp 1 0 nil nil nil)
          (read 0 4 nil nil nil)
          (read-byte 1 2 nil nil nil)
          (read-char 0 4 nil nil nil)
          (read-char-no-hang 0 4 nil nil nil)
          (read-delimited-list 1 2 nil nil nil)
          (system::read-eval-print 1 1 nil nil nil)
          (system::read-eval-reader 3 0 nil nil nil)
          (system::read-form 1 1 nil nil nil)
          (read-from-string 1 2 nil (:preserve-whitespace :start :end) nil)
          (read-integer 2 3 nil nil nil)
          (read-line 0 4 nil nil nil)
          (read-preserving-whitespace 0 4 nil nil nil)
          (readtablep 1 0 nil nil nil)
          (realp 1 0 nil nil nil)
          (realpart 1 0 nil nil nil)
          (system::redo-eval-frame 1 0 nil nil nil)
          (reduce 2 0 nil (:from-end :start :end :key :initial-value) nil)
          (rem 2 0 nil nil nil)
          (remhash 2 0 nil nil nil)
          (remove 2 0 nil (:from-end :start :end :key :test :test-not :count) nil)
          (remove-duplicates 1 0 nil (:from-end :start :end :key :test :test-not) nil)
          (remove-if 2 0 nil (:from-end :start :end :key :count) nil)
          (remove-if-not 2 0 nil (:from-end :start :end :key :count) nil)
          (remprop 2 0 nil nil nil)
          (rename-file 2 0 nil nil nil)
          (rename-package 2 1 nil nil nil)
          (replace 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (rest 1 0 nil nil nil)
          (system::return-from-eval-frame 2 0 nil nil nil)
          (revappend 2 0 nil nil nil)
          (reverse 1 0 nil nil nil)
          (round 1 1 nil nil nil)
          (row-major-aref 2 0 nil nil nil)
          (system::row-major-store 3 0 nil nil nil)
          (system::rpar-reader 2 0 nil nil nil)
          (rplaca 2 0 nil nil nil)
          (rplacd 2 0 nil nil nil)
          (system::same-env-as 2 0 nil nil nil)
          (savemem 1 0 nil nil nil)
          (sbit 1 0 t nil nil)
          (scale-float 2 0 nil nil nil)
          (schar 2 0 nil nil nil)
          (search 2 0 nil (:from-end :start1 :end1 :start2 :end2 :key :test :test-not) nil)
          (system::search-string-equal 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (system::search-string= 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (second 1 0 nil nil nil)
          (system::sequencep 1 0 nil nil nil)
          (set 2 0 nil nil nil)
          (set-dispatch-macro-character 3 1 nil nil nil)
          (system::set-fill-pointer 2 0 nil nil nil)
          (set-macro-character 2 2 nil nil nil)
          (system::set-symbol-value 2 0 nil nil nil)
          (set-syntax-from-char 2 2 nil nil nil)
          (seventh 1 0 nil nil nil)
          (shadow 1 1 nil nil nil)
          (shadowing-import 1 1 nil nil nil)
          ;(shell 0 1 nil nil nil)
          (system::short-float-p 1 0 nil nil nil)
          (show-stack 0 0 nil nil nil)
          (signum 1 0 nil nil nil)
          (system::simple-array-p 1 0 nil nil nil)
          (simple-bit-vector-p 1 0 nil nil nil)
          (simple-string-p 1 0 nil nil nil)
          (simple-vector-p 1 0 nil nil nil)
          (sin 1 0 nil nil nil)
          (system::single-float-p 1 0 nil nil nil)
          (sinh 1 0 nil nil nil)
          (sixth 1 0 nil nil nil)
          (clos:slot-value 2 0 nil nil nil)
          (clos::set-slot-value 3 0 nil nil nil)
          (clos:slot-boundp 2 0 nil nil nil)
          (clos:slot-makunbound 2 0 nil nil nil)
          (clos:slot-exists-p 2 0 nil nil nil)
          (software-type 0 0 nil nil nil)
          (software-version 0 0 nil nil nil)
          (some 2 0 t nil nil)
          (sort 2 0 nil (:key :start :end) nil)
          (special-operator-p 1 0 nil nil nil)
          (system::special-variable-p 1 0 nil nil nil)
          (sqrt 1 0 nil nil nil)
          (stable-sort 2 0 nil (:key :start :end) nil)
          (standard-char-p 1 0 nil nil nil)
          (clos::std-instance-p 1 0 nil nil nil)
          (system::store 2 0 t nil nil)
          (system::store-char 3 0 nil nil nil)
          (system::store-schar 3 0 nil nil nil)
          (stream-external-format 1 0 nil nil nil)
          (streamp 1 0 nil nil nil)
          (string 1 0 nil nil nil)
          (system::string-both-trim 3 0 nil nil nil)
          (string-capitalize 1 0 nil (:start :end) nil)
          (string-char-p 1 0 nil nil nil)
          (string-concat 0 0 t nil nil)
          (string-downcase 1 0 nil (:start :end) nil)
          (string-equal 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string-greaterp 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (system::string-input-stream-index 1 0 nil nil nil)
          (string-lessp 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string-not-equal 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string-not-greaterp 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string-not-lessp 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (system::string-reader 2 0 nil nil nil)
          (system::string-stream-p 1 0 nil nil nil)
          (string-upcase 1 0 nil (:start :end) nil)
          (string/= 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string< 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string<= 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string= 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string> 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string>= 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (stringp 1 0 nil nil nil)
          (clos::structure-object-p 1 0 nil nil nil)
          (system::structure-reader 3 0 nil nil nil)
          (sublis 2 0 nil (:test :test-not :key) nil)
          (system::subr-info 1 0 nil nil nil)
          (subseq 2 1 nil nil nil)
          (subst 3 0 nil (:test :test-not :key) nil)
          (subst-if 3 0 nil (:key) nil)
          (subst-if-not 3 0 nil (:key) nil)
          (substitute 3 0 nil (:from-end :start :end :key :test :test-not :count) nil)
          (substitute-if 3 0 nil (:from-end :start :end :key :count) nil)
          (substitute-if-not 3 0 nil (:from-end :start :end :key :count) nil)
          (substring 2 1 nil nil nil)
          (svref 2 0 nil nil nil)
          (system::svstore 3 0 nil nil nil)
          (sxhash 1 0 nil nil nil)
          (symbol-function 1 0 nil nil nil)
          (symbol-name 1 0 nil nil nil)
          (symbol-package 1 0 nil nil nil)
          (symbol-plist 1 0 nil nil nil)
          (symbol-value 1 0 nil nil nil)
          (symbolp 1 0 nil nil nil)
          (system::synonym-stream-p 1 0 nil nil nil)
          (system::syntax-error-reader 3 0 nil nil nil)
          (tailp 2 0 nil nil nil)
          (tan 1 0 nil nil nil)
          (tanh 1 0 nil nil nil)
          (tenth 1 0 nil nil nil)
          (terpri 0 1 nil nil nil)
          (system::the-frame 0 0 nil nil nil)
          (third 1 0 nil nil nil)
          (translate-pathname 3 0 nil (:all :merge) nil)
          (tree-equal 2 0 nil (:test :test-not) nil)
          (truename 1 0 nil nil nil)
          (truncate 1 1 nil nil nil)
          (system::two-way-stream-p 1 0 nil nil nil)
          (type-of 1 0 nil nil nil)
          (unexport 1 1 nil nil nil)
          (unintern 1 1 nil nil nil)
          (system::uninterned-reader 3 0 nil nil nil)
          (unread-char 1 1 nil nil nil)
          (unuse-package 1 1 nil nil nil)
          (system::unwind-to-driver 0 0 nil nil nil)
          (upper-case-p 1 0 nil nil nil)
          (use-package 1 1 nil nil nil)
          #+(or UNIX ACORN-RISCOS WIN32) (user-homedir-pathname 0 1 nil nil nil)
          (values 0 0 t nil nil)
          (values-list 1 0 nil nil nil)
          (vector 0 0 t nil nil)
          (system::vector-endtest 2 0 nil nil nil)
          (system::vector-fe-endtest 2 0 nil nil nil)
          (system::vector-fe-init 1 0 nil nil nil)
          (system::vector-fe-init-end 2 0 nil nil nil)
          (system::vector-fe-upd 2 0 nil nil nil)
          (system::vector-init 1 0 nil nil nil)
          (system::vector-init-start 2 0 nil nil nil)
          (system::vector-length 1 0 nil nil nil)
          (vector-pop 1 0 nil nil nil)
          (vector-push 2 0 nil nil nil)
          (vector-push-extend 2 1 nil nil nil)
          (system::vector-reader 3 0 nil nil nil)
          (system::vector-upd 2 0 nil nil nil)
          (vectorp 1 0 nil nil nil)
          (system::version 0 1 nil nil nil)
          (weak-pointer-p 1 0 nil nil nil)
          (weak-pointer-value 1 0 nil nil nil)
          (wild-pathname-p 1 1 nil nil nil)
          (write 1 0 nil (:case :level :length :gensym :escape :radix :base :array :circle :pretty :closure :readably :right-margin :stream) nil)
          (write-byte 2 0 nil nil nil)
          (write-char 1 1 nil nil nil)
          (write-integer 3 1 nil nil nil)
          (write-line 1 1 nil (:start :end) nil)
          (write-string 1 1 nil (:start :end) nil)
          (write-to-string 1 0 nil (:case :level :length :gensym :escape :radix :base :array :circle :pretty :closure :readably :right-margin) nil)
          (xgcd 0 0 t nil nil)
          (zerop 1 0 nil nil nil)
) ) ) )  )
(defconstant function-codes
  (let ((hashtable (make-hash-table :test #'eq)))
    (dotimes (i (* 3 256))
      (let ((sym (%funtabref i))) ; Name der Funktion FUNTAB[i]
        (when sym (setf (gethash sym hashtable) i))
    ) )
    hashtable
) )
(defconstant funtabR-index ; Startindex der FUNTABR bzgl. FUNTAB
  (dotimes (i (* 3 256))
    (let ((sym (%funtabref i)))
      (multiple-value-bind (name req opt rest-p) (subr-info sym)
        (declare (ignore name req opt))
        (when rest-p (return i))
) ) ) )
(defun CALLS-code (funtab-index)
  (if (< funtab-index 256)
    `(CALLS1 ,funtab-index)
    `(CALLS2 ,(- funtab-index 256))
) )

; Hilfsfunktion: mapcan, aber mit append statt nconc:
#|
#-CLISP
(defun mapcap (fun &rest lists &aux (L nil))
  (loop
    (setq L
      (nconc
        (reverse
          (apply fun
            (maplist #'(lambda (listsr)
                         (if (atom (car listsr))
                           (return)
                           (pop (car listsr))
                       ) )
                     lists
        ) ) )
        L
      )
  ) )
  (nreverse L)
)
|#
#-CLISP
(defun mapcap (fun &rest lists)
  (apply #'append (apply #'mapcar fun lists))
)

; Hilfsfunktion: mapcon, aber mit append statt nconc:
#|
#-CLISP
(defun maplap (fun &rest lists &aux (L nil))
  (loop
    (setq L
      (nconc
        (reverse
          (apply fun
            (maplist #'(lambda (listsr)
                         (if (atom (car listsr))
                           (return)
                           (prog1
                             (car listsr)
                             (setf (car listsr) (cdr (car listsr)))
                       ) ) )
                     lists
        ) ) )
        L
      )
  ) )
  (nreverse L)
)
|#
#-CLISP
(defun maplap (fun &rest lists)
  (apply #'append (apply #'maplist fun lists))
)

; (memq item const-symbollist) == (member item const-symbollist :test #'eq),
; nur der boolesche Wert.
(defmacro memq (item list)
  (if (and (constantp list) (listp (eval list)))
    `(case ,item (,(eval list) t) (t nil))
    `(member ,item ,list :test #'eq)
) )

; Fehlermeldungsfunktion
(defun compiler-error (caller &optional where)
  (error (ENGLISH "Compiler bug!! Occurred in ~A~@[ at ~A~].")
         caller where
) )



;                      S T A C K - V E R W A L T U N G

; Ein Stackzustand beschreibt, was sich zur Laufzeit alles auf den beiden
; Stacks befinden wird.
; Genaue Struktur:
; (item1 ... itemk . fun)
; Das ist im Speicher in Wirklichkeit eine Baumstruktur!
; Es bedeuten hierbei:
;  fun = FNODE der Funktion, in der gezählt wird.
;  item = eines der folgenden:
;    n (Integer >=0) : n Lisp-Objekte auf dem STACK
;                      belegt n STACK-Einträge
;    (BIND n)        : einen Bindungsframe für n Variablen,
;                      belegt 1+2*n STACK-Einträge und 0 SP-Einträge
;                      Muss bei Unwind explizit aufgelöst werden
;    PROGV           : ein Bindungsframe für beliebig viele Variablen,
;                      belegt ? STACK-Einträge und 1 SP-Eintrag (Pointer über
;                      den Frame = alter STACK)
;                      Muss bei Unwind explizit aufgelöst werden
;    CATCH           : ein CATCH-Frame
;                      belegt 3 STACK-Einträge und 2+jmpbufsize SP-Einträge
;    UNWIND-PROTECT  : ein Unwind-Protect-Frame
;                      belegt 2 STACK-Einträge und 2+jmpbufsize SP-Einträge
;                      Muss bei Unwind aufgelöst und der Cleanup ausgeführt
;                      werden
;    CLEANUP         : während der Cleanup-Phase eines UNWIND-PROTECT
;                      belegt ? STACK-Einträge und 3 SP-Einträge
;                      (der untere ist Pointer über den Frame = alter STACK)
;    BLOCK           : ein BLOCK-Frame
;                      belegt 3 STACK-Einträge und 2+jmpbufsize SP-Einträge
;                      Muss bei Unwind explizit aufgelöst werden
;    (TAGBODY n)     : ein TAGBODY-Frame, der n Tags aufhebt
;                      belegt 3+n STACK-Einträge und 1+jmpbufsize SP-Einträge
;                      Muss bei Unwind explizit aufgelöst werden
;    MVCALLP         : Vorbereitung für MVCALL
;                      belegt 1 STACK-Eintrag und 1 SP-Eintrag (Pointer über
;                      FRAME = STACK)
;    MVCALL          : viele Lisp-Objekte
;                      belegt ? STACK-Einträge und 1 SP-Eintrag (Pointer über
;                      Frame = alter STACK)
;    ANYTHING        : viele Lisp-Objekte und Frames
;                      belegt ? STACK-Einträge und 1 SP-Eintrag (Pointer über
;                      Frame = alter STACK)

(defvar *stackz*)    ; der aktuelle Stackzustand

; Eine SP-Tiefe k ist ein Cons (k1 . k2) und bedeutet k1+jmpbufsize*k2.
(defmacro spd (k1 k2) `(cons ,k1 ,k2))
(defun spd+ (k kd)
  (cons (+ (car k) (car kd))
        (+ (cdr k) (cdr kd))
) )
(defun spd- (k kd)
  (cons (- (car k) (car kd))
        (- (cdr k) (cdr kd))
) )
(defun spd<= (k kk)
  (and (<= (car k) (car kk))
       (<= (cdr k) (cdr kk))
) )
(defun spdmax (k kk)
  (cons (max (car k) (car kk))
        (max (cdr k) (cdr kk))
) )
#|
; We cannot simply take the maximum of two depths, have to work with lists.
; Is depth covered by some of the depths in the list?
(defun some-spd<= (depth list-of-depths)
  (dolist (x list-of-depths nil)
    (when (spd<= depth x) (return t))
) )
|#

; (stackz-fun stackz) extrahiert aus einem Stackzustand die Funktion, in der
; gerade gearbeitet wird.
#|
(defun stackz-fun (stackz)
  (loop (when (atom stackz) (return)) (setq stackz (cdr stackz)))
  stackz
)
|#
; äquivalent, aber schneller:
(defun stackz-fun (stackz)
  (if (atom stackz) stackz (cdr (last stackz)))
)

; (in-same-function-p stackz1 stackz2) stellt fest, ob in beiden Stackzuständen
; in derselben Funktion gearbeitet wird.
(defun in-same-function-p (stackz1 stackz2)
  (eq (stackz-fun stackz1) (stackz-fun stackz2))
)

; (zugriff-in-stack stackz1 stackz2)
; Für den Zugriff auf lokale Variablen im Stack:
; ergibt zu zwei Stackzuständen stackz1 und stackz2, die beide innerhalb
; derselben Funktion liegen und wo stackz1 "tiefer" ist als stackz2:
; 2 Werte: NIL und n, falls (stackz2) = (STACK+4*n) von stackz1 aus,
;          k und n, falls (stackz2) = ((SP+4*k)+4*n) von stackz1 aus.
; (Falls stackz2 mit BLOCK oder TAGBODY beginnt, ist immer der Zugriff auf die
;  consvar eines Block- bzw. Tagbody-Frames gemeint.)
(defun zugriff-in-stack (stackz1 stackz2 &aux (k nil) (n 0) (kd (spd 0 0)))
  (loop ; beim Durchlaufen der Stacks nach oben:
    ; momentanes STACK ist STACK+4*n (bei k=NIL) bzw. (SP+4*k)+4*n,
    ; momentanes SP ist SP+4*kd (bei k=NIL) bzw. SP+4*(k+kd).
    (when (eq stackz1 stackz2) (return))
    (when (atom stackz1) (compiler-error 'zugriff-in-stack "STACKZ-END"))
    (let ((item (car stackz1)))
      (cond ((integerp item) (setq n (+ n item)))
            ((consp item)
             (case (first item)
               (BIND    (setq n (+ n (+ 1 (* 2 (second item))))))
               (TAGBODY (setq kd (spd+ kd (spd 1 1))
                              n (+ n (+ 3 (second item)))
               )        )
               (t (compiler-error 'zugriff-in-stack "STACKZ-LISTITEM"))
            ))
            (t
             (case item
               (PROGV          (setq k (if k (spd+ k kd) kd) kd (spd 1 0) n 0))
               (CATCH          (setq kd (spd+ kd (spd 2 1)) n (+ n 3)))
               (UNWIND-PROTECT (setq kd (spd+ kd (spd 2 1)) n (+ n 2)))
               (CLEANUP        (setq k (if k (spd+ k kd) kd) kd (spd 3 0) n 0))
               (BLOCK          (setq kd (spd+ kd (spd 2 1)) n (+ n 3)))
               (MVCALLP        (setq kd (spd+ kd (spd 1 0)) n (+ n 1)))
               ((MVCALL ANYTHING)
                               (setq k (if k (spd+ k kd) kd) kd (spd 1 0) n 0))
               (t (compiler-error 'zugriff-in-stack "STACKZ-ITEM"))
    ) )     ))
    (setq stackz1 (cdr stackz1))
  )
  (when (and (consp stackz2) ; beim Zugriff auf BLOCK- bzw. TAGBODY-consvar:
             (or (eq (car stackz2) 'BLOCK)
                 (and (consp (car stackz2)) (eq (first (car stackz2)) 'TAGBODY))
        )    )
    (setq n (+ n 2)) ; consvar liegt genau 2 Einträge höher als Frameanfang
  )
  (values k n)
)

; (may-UNWIND stackz1 stackz2)
; stellt fest, ob (UNWIND stackz1 stackz2 for-value) legal ist. Dazu ist
; notwendig, dass der Compiler über die Frames zwischen stackz1 und stackz2
; genau Bescheid weiß.
(defun may-UNWIND (stackz1 stackz2)
  (loop
    (when (eq stackz1 stackz2) (return t))
    (when (atom stackz1) (compiler-error 'may-UNWIND "STACKZ-END"))
    (when (eq (car stackz1) 'ANYTHING) (return nil))
    (setq stackz1 (cdr stackz1))
) )

; (expand-UNWIND stackz1 stackz2 for-value)
; liefert ein zu (UNWIND stackz1 stackz2 for-value) äquivalentes Codestück,
; bestehend aus
; (SKIP n), (SKIPI k1 k2 n), (SKIPSP k1 k2), (VALUES0),
; (UNWIND-PROTECT-CLEANUP), (UNBIND1), (BLOCK-CLOSE), (TAGBODY-CLOSE).
; Es muss - ausgehend von stackz1 - den Stack so bereinigen, dass danach der
; Stackzustand stackz2 vorliegt. Bei for-value=NIL können die Werte dabei
; weggeworfen werden.
(defun expand-UNWIND (stackz1 stackz2 for-value
                      &aux (k nil) (n 0) (kd (spd 0 0)) (codelist nil))
  (flet ((here () ; bis hierher erst einmal die Stacks hochsetzen
           (if k
             (progn
               (push `(SKIPI ,(car k) ,(cdr k) ,n) codelist)
               (unless (> (car kd) 0) (compiler-error 'expand-UNWIND "SP-depth"))
               (when (or (> (car kd) 1) (> (cdr kd) 0))
                 (push `(SKIPSP ,(- (car kd) 1) ,(cdr kd)) codelist)
             ) )
             (progn
               (when (> n 0) (push `(SKIP ,n) codelist))
               (when (or (> (car kd) 0) (> (cdr kd) 0))
                 (push `(SKIPSP ,(car kd) ,(cdr kd)) codelist)
           ) ) )
           (setq k nil n 0 kd (spd 0 0))
        ))
    (loop ; beim Durchlaufen der Stacks nach oben:
      ; momentanes STACK ist STACK+4*n (bei k=NIL) bzw. (SP+4*k)+4*n,
      ; momentanes SP ist SP+4*kd (bei k=NIL) bzw. SP+4*(k+kd).
      (when (eq stackz1 stackz2) (here) (return))
      (when (atom stackz1) (compiler-error 'expand-UNWIND "STACKZ-END"))
      (let ((item (car stackz1)))
        (cond ((integerp item) (setq n (+ n item)))
              ((consp item)
               (case (first item)
                 (BIND    (here) (push '(UNBIND1) codelist))
                 (TAGBODY (here) (push '(TAGBODY-CLOSE) codelist))
                 (t (compiler-error 'expand-UNWIND "STACKZ-LISTITEM"))
              ))
              (t
               (case item
                 (PROGV (here) (push '(UNBIND1) codelist) (setq kd (spd 1 0)))
                 (CATCH (setq kd (spd+ kd (spd 2 1)) n (+ n 3)))
                 (UNWIND-PROTECT
                   (here)
                   (unless for-value
                      ; bei for-value=NIL wird beim ersten auftretenden
                      ; UNWIND-PROTECT-Frame ein '(VALUES0) eingefügt.
                     (setq for-value t)
                     (push '(VALUES0) codelist)
                   )
                   (push '(UNWIND-PROTECT-CLEANUP) codelist)
                 )
                 (CLEANUP (setq k (if k (spd+ k kd) kd) kd (spd 3 0) n 0))
                 (BLOCK (here) (push '(BLOCK-CLOSE) codelist))
                 (MVCALLP (setq kd (spd+ kd (spd 1 0)) n (+ n 1)))
                 (MVCALL (setq k (if k (spd+ k kd) kd) kd (spd 1 0) n 0))
                 (t (compiler-error 'expand-UNWIND "STACKZ-ITEM"))
      ) )     ))
      (setq stackz1 (cdr stackz1))
    )
    (nreverse codelist)
) )

; (spdepth-difference stackz1 stackz2)
; liefert den Unterschied k von SP bei stackz1 und SP bei stackz2.
; Um den SP von stackz1 zu stackz2 hochzusetzen, reicht also ein (SKIPSP k1 k2).
(defun spdepth-difference (stackz1 stackz2 &aux (k (spd 0 0)))
  (loop
    (when (eq stackz1 stackz2) (return))
    (when (atom stackz1) (compiler-error 'spdepth-difference "STACKZ-END"))
    (let ((item (car stackz1)))
      (if (consp item)
        (case (first item)
          (TAGBODY (setq k (spd+ k (spd 1 1))))
        )
        (case item
          ((PROGV MVCALLP MVCALL ANYTHING) (setq k (spd+ k (spd 1 0))))
          ((CATCH UNWIND-PROTECT BLOCK) (setq k (spd+ k (spd 2 1))))
          (CLEANUP (setq k (spd+ k (spd 3 0))))
    ) ) )
    (setq stackz1 (cdr stackz1))
  )
  k
)



;        F U N C T I O N - E N V I R O N M E N T - V E R W A L T U N G

; mitgegeben vom Interpreter: %fenv%

; Interpreter-Funktions-Environment hat die Gestalt
; %fenv% = NIL oder #(f1 def1 ... fn defn NEXT-ENV), NEXT-ENV von derselben
; Gestalt.
; Damit ist eine Abbildung fi --> defi realisiert.
; defi = (SYSTEM::MACRO . expander)  bedeutet einen lokalen Macro.
; defi = Closure                     bedeutet, dass defi die lokale
;                                    Funktionsdefinition von fi ist
; defi = NIL                         bedeutet, dass eine lokale Funktions-
;                                    definition noch hineinkommt (vgl. LABELS)

; neu konstruiert:
(defvar *fenv*)
; enthält die neuen lexikalischen Funktionsbindungen.
; *fenv* hat dieselbe Gestalt wie %fenv% und endet mit %fenv%:
; #(f1 def1 ... fn defn NEXT-ENV), was eine Abbildung fi --> defi
; realisiert.
; defi = (SYSTEM::MACRO expander)  bedeutet einen lokalen Makro.
; defi = (fdescr . var)            bedeutet, dass die lokale Funktionsdefinition
;           von fi zur Laufzeit in der lexikalischen Variablen var steckt.
;           fnode ist der zu fi gehörige fnode, anfangs noch NIL.
; defi = (fdescr . const)          bedeutet, dass die lokale Funktionsdefinition
;           von fi autonom ist und in der Konstanten const steckt.
;           fnode ist der zu fi gehörige fnode, anfangs noch NIL.
; Dabei ist fdescr ein Cons (fnode . lambdadescr),
;           fnode der zu fi gehörige fnode oder NIL,
;           lambdadescr = (LABELS . Liste der Werte von analyze-lambdalist)
;           oder lambdadescr = (GENERIC . Signature) oder NIL.

; Suche die lokale Funktionsdefinition des Symbols f in fenv :
; Ergebnis ist:
; SYSTEM::MACRO, expander           bei einem lokalen Macro,
; GLOBAL, Vektor, Index             wenn defi = (svref Vektor Index)
;                                   (also in %fenv% gefunden)
; LOCAL, def, fdescr                wenn defi = def eine Variable oder Konstante
;                                   (also in *fenv* ohne %fenv% gefunden)
; NIL                               falls nicht lokal definiert.
(defun fenv-search (f &optional (fenv *fenv*))
  (loop
    (when (null fenv) (return-from fenv-search 'NIL))
    (unless (simple-vector-p fenv) (compiler-error 'fenv-search))
    (do ((l (1- (length fenv)))
         (i 0 (+ i 2)))
        ((= i l) (setq fenv (svref fenv i)))
      (if (equal f (svref fenv i))
        (let ((def (svref fenv (1+ i))))
          (return-from fenv-search
            (if (consp def)
              (if (eq (car def) 'SYSTEM::MACRO)
                (values 'SYSTEM::MACRO (cdr def))
                (values 'LOCAL (cdr def) (car def))
              )
              (values 'GLOBAL fenv (1+ i))
  ) ) ) ) ) )
)
; Stellt fest, ob ein Funktionsname im Function-Environment fenv nicht
; definiert ist und daher auf die globale Funktion verweist.
(defun global-in-fenv-p (s fenv)
  (eq (fenv-search s fenv) 'NIL)
)

; Mit einem Vektor aus
; - einem solchen Variablen-Environment (verkettete Vektoren, mit
;   defi = #<SYMBOL-MACRO expansion> für Symbol-Macro-Definitionen),
; - einem solchen Funktions-Environment (verkettete Vektoren, mit
;   defi = (SYSTEM::MACRO . expander) für Macro-Definitionen zu fi)
; arbeiten die Funktionen
; MACROEXPAND-1, MACROEXPAND, PARSE-BODY:
#|
(MACROEXPAND-1 form env) expandiert die gegebene Form im Macroexpansions-
Environment env und liefert die 1 mal expandierte Form und T
(oder form und NIL, falls nicht expandierbar).

(MACROEXPAND form env) expandiert die gegebene Form im Macroexpansions-
Environment env und liefert die sooft wie möglich expandierte Form und T
(oder form und NIL, falls nicht expandierbar).

(PARSE-BODY body docstring-allowed env) analysiert den body und spaltet von
ihm die Deklarationen und den Docstring (falls erlaubt und vorhanden) ab.
3 Werte: der übrige body-rest, eine Liste der vorgekommenen declspecs,
der Docstring (oder NIL).
|#


;           B L O C K - E N V I R O N M E N T - V E R W A L T U N G

; mitgegeben vom Interpreter: %benv%

; Interpreter-Block-Environment hat die Gestalt
; %benv% = ((name1 . status1) ... (namen . statusn))
; wobei namei ein Symbol und statusi der Status dieses lexikalisch umfassenden
; Blocks ist: #<DISABLED> falls der Block bereits verlassen wurde, sonst ein
; Pointer in den Stack auf den zugehörigen Block-Frame.

; neu konstruiert:
(defvar *benv*)

; *benv* hat die Gestalt
; ((name1 . block1) ... (namen . blockn) . %benv%)
; wobei blocki der Descriptor des Blocks mit Namen namei ist:
(defstruct (block (:copier nil))
  fnode                 ; Funktion, in der dieser Block definiert ist, ein FNODE
  label                 ; label, an dem dieser Block zu Ende ist
  stackz                ; Stackzustand nach dem Aufbau des Block-Frames
  consvar               ; Variable, die im Stack im Block-Frame liegt und den
                        ; Block-Cons enthält (dessen CDR beim Verlassen des
                        ; Blockes auf #<DISABLED> gesetzt wird)
  used-far              ; Flag, gibt an, ob dieser Block aus einer anderen
                        ; Funktion heraus mit RETURN-FROM verlassen wird.
  for-value             ; gibt an, ob das gesamte Block-Konstrukt Werte
                        ; zurückliefern soll.
)
#+CLISP (remprop 'block 'sys::defstruct-description)

; Sucht nach einem Block mit dem Namen name und liefert:
; NIL                          falls nicht gefunden,
; Block-Descriptor             falls in *benv* gefunden,
; Block-Cons (name . status)   falls in %benv% gefunden.
(defun benv-search (name &optional (benv *benv*))
  (loop
    (when (atom benv) (return nil))
    (when (eq (caar benv) name)
      (if (block-p (cdar benv))
        (return (cdar benv))
        (return (car benv))
    ) )
    (setq benv (cdr benv))
) )


;         T A G B O D Y - E N V I R O N M E N T - V E R W A L T U N G

; mitgegeben vom Interpreter: %genv%

; Interpreter-Tagbody-Environment hat die Gestalt
; %genv% = ((Tagvektor1 . status1) ... (Tagvektorn . statusn))
; wobei Tagvektori ein simple-vector ist, der die anspringbaren Tags enthält,
; statusi der Status dieses lexikalisch umfassenden Tagbodys
; ist: #<DISABLED> falls der Tagbody bereits verlassen wurde, sonst ein
; Pointer in den Stack auf den zugehörigen Tagbody-Frame.

; neu konstruiert:
(defvar *genv*)

; *genv* hat die Gestalt
; ((Tagvektor1 . tagbody1) ... (Tagvektorn . tagbodyn) . %genv%)
; wobei tagbodyi der Descriptor des Tagbodys i ist:
(defstruct (tagbody (:copier nil))
  fnode               ; Funktion, in der dieser Tagbody definiert ist, ein FNODE
  labellist           ; Liste der Labels, parallel zum Tagvektor
  stackz              ; Stackzustand nach dem Aufbau des Tagbody-Frames
  consvar             ; Variable, die im Stack im Tagbody-Frame liegt und den
                      ; Tagbody-Cons enthält (dessen CDR beim Verlassen des
                      ; Tagbodys auf #<DISABLED> gesetzt wird)
  used-far            ; Vektor mit Fill-Pointer, enthält all die Tags, die
                      ; aus einer anderen Funktion heraus mit GO angesprungen
                      ; werden.
)
#+CLISP (remprop 'tagbody 'sys::defstruct-description)

; Sucht nach einem Tag mit dem Namen name und liefert:
; NIL                                         falls nicht gefunden,
; Tagbody-Descriptor, Index                   falls in *genv* gefunden,
; Tagbody-Cons (Tagvektor . status), Index    falls in %genv% gefunden.
(defun genv-search (name &optional (genv *genv*))
  (loop
    (when (atom genv) (return nil))
    (do* ((v (caar genv))
          (l (length v))
          (i 0 (1+ i)))
         ((= i l))
      (when (eql (svref v i) name)
        (return-from genv-search
          (values (if (tagbody-p (cdar genv)) (cdar genv) (car genv)) i)
    ) ) )
    (setq genv (cdr genv))
) )


;       V A R I A B L E N - E N V I R O N M E N T - V E R W A L T U N G

; mitgegeben vom Interpreter: %venv%

; Interpreter-Variablen-Environment hat die Gestalt
; %venv% = NIL oder #(v1 val1 ... vn valn NEXT-ENV), NEXT-ENV von derselben
; Gestalt.
(defparameter specdecl
  #+CLISP (eval
            '(let ((*evalhook*
                     #'(lambda (form env) (declare (ignore form))
                         (svref (svref env 0) 1)
                         ; Der Evalhook-Mechanismus übergibt das Environment.
                         ; (svref...0) davon ist das Variablen-Environment,
                         ; (svref...1) davon ist von der *evalhook*-Bindung
                         ; der assoziierte "Wert" #<SPECIAL REFERENCE>.
                  ))   )
               0
          )  )
  #-CLISP (cons nil nil)
)
; stellt fest, ob das Symbol var eine Special-Variable darstellt
#+CLISP
(defun proclaimed-special-p (var)
  (or (sys::special-variable-p var)
      (not (null (member var *known-special-vars* :test #'eq)))
) )
#-CLISP
(defun proclaimed-special-p (var)
  (or
    (eq var '*evalhook*)
    (eq var '*applyhook*)
    (eq var '*macroexpand-hook*)
    (let ((obj (cons nil nil)))
      (eval
        `(let ((,var ',obj))
           (and (boundp ',var) (eq (symbol-value ',var) ',obj))
    ) )  )
    (not (null (member var *known-special-vars* :test #'eq)))
) )

; neu konstruiert:
(defvar *venv*)                  ; Variablen-Environment, Feinstruktur
(defvar *venvc*)                 ; Variablen-Environment, Grobstruktur

; *venv* hat dieselbe Gestalt wie %venv% und endet mit %venv%:
; #(v1 var1 ... vn varn NEXT_ENV), wo vari Variablen-Konstrukte oder
; Symbolmacros oder Interpreter-Werte sind und NEXT-ENV von derselben Gestalt.

; *venvc* simuliert das Laufzeit-Variablen-Environment zur Laufzeit, soweit
; es sich um Closure-Variablen handelt.
; *venvc* hat die Gestalt
; (item1 ... itemn)
; jedes item ist
;   NIL :            ein LET/LET*/MULTIPLE-VALUE-BIND/Funktionseintritt/
;                    FLET/LABELS, der keine Closure aufmacht
;   fnode :          eine neue Funktion
;   ((var1 ... vark) . stackz) : durch ein LET/LET*/MULTIPLE-VALUE-BIND/
;                    Funktionseintritt/FLET/LABELS kommen die Variablen
;                    Var1, ..., Vark in eine Closure.
;                    Diese Closure liegt im Stack; angegeben der
;                    Stackzustand, an der sie erreichbar ist.

; Eine Variable wird beschrieben dadurch, dass sie entweder special ist oder
; - falls lexikalisch - der Stackaufbau nach dem Anlegen der Variablen im Stack
; bzw. der Ort in der Closure festliegt.
(defstruct (var (:copier nil))
  (name nil :read-only t)     ; Symbol
  (specialp nil :read-only t) ; special deklariert (oder lexikalisch gebunden) ?
  constantp                   ; Konstante ?
  constant                    ; wenn Konstante: Wert und Herkunft der Konstanten
                              ;   (der Wert ist zur Compile-Zeit bekannt)
  usedp                       ; falls lexikalisch:
                              ;   wurde die Variable jemals abgefragt ?
                              ;   (Eine durch NIL oder T beendete Liste der
                              ;    Referenzen auf die Variable)
  for-value-usedp             ; falls lexikalisch:
                              ;   wurde die Variable jemals for-value abgefragt?
  really-usedp                ; falls lexikalisch:
                              ;   wurde die Variable jemals wirklich
                              ;   (um den Wert zu wissen) abgefragt ?
  (assignedp nil)             ; falls lexikalisch:
                              ;   wurde der Variablen jemals ein Wert zugewiesen?
  (modified-list '())         ; falls lexikalisch: zu jedem SET auf die Variable
                              ;   eine Liste (value-anode set-anode . for-value)
  (replaceable-list '())      ; falls lexikalisch:
                              ;   zu jeder movable-Variablen, die während ihrer
                              ;   gesamten Existenz denselben Wert wie diese
                              ;   hat und deswegen ersetzbar ist, jeweils eine
                              ;   Liste (var init-anode . bind-anode)
  closurep                    ; falls lexikalisch:
                              ;   NIL falls im Stack, T falls in der Closure
  (stackz nil :read-only t)   ; falls lexikalisch:
                              ;   Stackzustand nach dem Anlegen der Variablen
                              ;   (falls Variable im Stack: ihr Ort im Stack)
  (venvc nil :read-only t)    ; falls lexikalisch und in der Closure:
                              ;   das *venvc*, in dessen erstem Item diese
                              ;   Variable vorkommt.
)
#+CLISP (remprop 'var 'sys::defstruct-description)

; (venv-search v) sucht in *venv* nach einer Variablen mit dem Symbol v.
; Ergebnis ist:
; NIL                   falls nicht gefunden
; SPECIAL               falls als Special-deklarierte Variable gefunden
; LOCAL, vector, index  falls interpretativ lexikalisch gebunden, Wert im Vektor
; T, var                falls lexikalisch gebunden, im Stack oder in der Closure
(defun venv-search (v &optional (venv *venv*))
  (when (or (constantp v) (proclaimed-special-p v))
    (return-from venv-search 'SPECIAL)
  )
  (loop
    (cond ((null venv) (return-from venv-search 'NIL))
          ((simple-vector-p venv)
           (do ((l (1- (length venv)))
                (i 0 (+ i 2)))
               ((= i l) (setq venv (svref venv i)))
             (if (eq v (svref venv i))
               (let ((val (svref venv (1+ i))))
                 (return-from venv-search
                   (if (and (var-p val) #| (eq (var-name val) v) |# )
                     (if (var-specialp val) 'SPECIAL (values T val))
                     (if (eq val specdecl) 'SPECIAL (values 'LOCAL venv (1+ i)))
          )) ) ) ) )
          (t (compiler-error 'venv-search))
  ) )
)

; (venv-search-macro v) sucht in *venv* nach einer Variablen mit dem Symbol v.
; Ergebnis ist:
;   Wenn v ein Symbol-Macro darstellt:  T, Expansion.
;   Sonst:                              NIL.
(defun venv-search-macro (v &optional (venv *venv*))
  (multiple-value-bind (a b c) (venv-search v venv)
    (case a
      ((NIL) (symbol-macro-expand v))
      ((LOCAL) (and (symbol-macro-p (svref b c))
                    (values t (sys::%record-ref (svref b c) 0))
      )        )
      (t nil)
) ) )

; (push-*venv* var1 ... varn) erweitert *venv* um var1, ..., varn,
; sozusagen wie durch  (dolist (v (list var1 ... varn)) (push v *venv*)).
(defun push-*venv* (&rest varlist)
  (when varlist
    (let ((l (list *venv*)))
      (dolist (var varlist) (setq l (list* (var-name var) var l)))
      (setq *venv* (apply #'vector l))
) ) )

; (zugriff-in-closure var venvc stackz)
; liefert zu einer Closure-Variablen var, wie man auf sie zugreifen kann
; (von einem Ort aus, an der Stack und das Closure-Environment durch stackz und
;  venvc beschrieben werden):
; 3 Werte k, n, m; die Variable sitzt in (svref ... 1+m) von
;     nil, n, m  : (STACK+4*n)
;     k, nil, m  : (svref ... 0)^k VenvConst
;     k, n,   m  : ((SP+4*k)+4*n)
(defun zugriff-in-closure (var venvc stackz &aux (k nil) n)
  ; Grobschleife, stellt die Closure-Tiefe k ab VenvConst fest:
  (loop
    (when (eq venvc (var-venvc var)) (return))
    (let ((item (car venvc)))
      (if (null k)
        (when (not (listp item)) (setq k 0)) ; Zählanfang, (not (listp item)) == (fnode-p item)
        (when (consp item) (incf k)) ; zählen
    ) )
    (setq venvc (cdr venvc))
  )
  (if k
    (setq n nil)
    (multiple-value-setq (k n) (zugriff-in-stack stackz (cdr (first venvc))))
  )
  (let ((m (do ((L (car (first venvc)) (cdr L))
                (i 0 (1+ i)))
               ((eq (car L) var) i)
       ))  )
    (values k n m)
) )


;             K O N S T A N T E N - V E R W A L T U N G

; Eine Konstante ist eine Box mit dem Wert der Konstanten:
(defstruct (const (:copier nil))
  value               ; Wert der Konstanten
  form                ; Form, die bei Auswertung value ergibt
  horizont            ; Gültigkeitsbereich von value und form:
                      ; :VALUE  -  nur value ist gültig
                      ;            (dann ist implizit form = `(QUOTE ,value) )
                      ; :ALL    -  value und form beide gültig
                      ; :FORM   -  nur form gültig
    ; Bei *compiling-from-file* = nil ist nur :VALUE und :ALL möglich.
    ; Was im 3. Pass in den Fnode eingetragen wird, ist:
    ;   Bei *compiling-from-file* = nil: nur value.
    ;   Bei *compiling-from-file* /= nil:
    ;     Falls (eq horizont ':value), value, sonst form.
)
#+CLISP (remprop 'const 'sys::defstruct-description)
; Im 2. Pass werden auch Variablen mit constantp=T als Konstanten behandelt.


;           D E K L A R A T I O N E N - V E R W A L T U N G

(defparameter *declaration-types*
  '(special ; Bindungen
    type ftype function ; Typen
    inline notinline ; Funktionen-Compilation
    ignore optimize dynamic-extent ; Compiler-Hinweise
    declaration ; Zusatzdeklarationen
    ; Typen nach Tabelle 4-1 :
    array atom base-char base-string bignum bit bit-vector boolean character
    common compiled-function complex cons double-float extended-char fixnum
    float function hash-table integer keyword list long-float nil null number
    package pathname random-state ratio rational readtable real sequence
    short-float simple-array simple-base-string simple-bit-vector
    simple-string simple-vector single-float standard-char stream string
    string-char symbol t vector
    ; zusätzliche Deklarationen:
    compile ; Anweisung, dass die Form bzw. Funktion zu compilieren ist
    sys::source ; der Source-Lambdabody (unexpandiert) innerhalb eines Lambdabody
    sys::in-defun ; zeigt an, zu welcher globalen Funktion der Code gehört
    ignorable ; markiert Variablen als vielleicht ignorierbar
              ; (NB: Gensym-Variablen sind immer automatisch ignorable.)
    sys::read-only ; markiert Variablen als nicht zugewiesen
)  )

; mitgegeben vom Interpreter: %denv%

; neu konstruiert:
(defvar *denv*)
; *denv* hat dieselbe Gestalt wie %denv% und endet mit %denv%.
; *denv* hat die Gestalt (item1 ... itemn), wo jedes item die Bauart
; (declaration-type argument ...) hat.
; Sonderbehandlung von
;   SPECIAL : wird weggelassen, stattdessen in *venv* notiert.
;   IGNORE, IGNORABLE : wird weggelassen, stattdessen bei der
;                       verarbeitenden Form selber verarbeitet.
; Zusätzliche Deklaration (INLINING symbol) gegen rekursives Inlining.

; (process-declarations declspeclist) pusht die Deklarationen (wie sie von
; PARSE-BODY kommen) auf *denv* und liefert:
; eine Liste der Special-deklarierten Symbole,
; eine Liste der Ignore-deklarierten Symbole,
; eine Liste der Ignorable-deklarierten Symbole,
; eine Liste der Read-Only-deklarierten Symbole.
(defun process-declarations (declspeclist &aux (specials nil) (ignores nil) (ignorables nil) (readonlys nil))
  (setq declspeclist (nreverse declspeclist))
  (dolist (declspec declspeclist)
    (if (or (atom declspec) (cdr (last declspec)))
      (c-warn (ENGLISH "Bad declaration syntax: ~S~%Will be ignored.")
              declspec
      )
      (let ((declspectype (car declspec)))
        (if (and (symbolp declspectype)
                 (or (member declspectype *declaration-types* :test #'eq)
                     (do ((L *denv* (cdr L)))
                         ((null L) nil)
                       (if (and (eq (first (car L)) 'DECLARATION)
                                (member declspectype (rest (car L)) :test #'eq)
                           )
                         (return t)
                     ) )
                     (and *compiling-from-file*
                       (member declspectype *user-declaration-types* :test #'eq)
            )    )   )
          (cond ((eq declspectype 'SPECIAL)
                 (dolist (x (cdr declspec))
                   (if (symbolp x)
                     (push x specials)
                     (c-warn (ENGLISH "Non-symbol ~S may not be declared SPECIAL.")
                             x
                )) ) )
                ((eq declspectype 'IGNORE)
                 (dolist (x (cdr declspec))
                   (if (symbolp x)
                     (push x ignores)
                     (c-warn (ENGLISH "Non-symbol ~S may not be declared IGNORE.")
                             x
                )) ) )
                ((eq declspectype 'IGNORABLE)
                 (dolist (x (cdr declspec))
                   (if (symbolp x)
                     (push x ignorables)
                     (c-warn (ENGLISH "Non-symbol ~S may not be declared IGNORABLE.")
                             x
                )) ) )
                ((eq declspectype 'SYS::READ-ONLY)
                 (dolist (x (cdr declspec))
                   (if (symbolp x)
                     (push x readonlys)
                     (c-warn (ENGLISH "Non-symbol ~S may not be declared READ-ONLY.")
                             x
                )) ) )
                (t (push declspec *denv*))
          )
          (c-warn (ENGLISH "Unknown declaration ~S.~%The whole declaration will be ignored.")
                  declspectype declspec
  ) ) ) ) )
  (values specials ignores ignorables readonlys)
)

; (declared-notinline fun denv) stellt fest, ob fun - ein Symbol, das eine
; globale Funktion, die nicht durch eine lokale Funktionsdefinition verdeckt
; ist, benennt - in denv als NOTINLINE deklariert ist.
; Was ist mit lokalen Funktionen ??
(defun declared-notinline (fun &optional (denv *denv*))
  (when (member `(INLINING ,fun) *denv* :test #'equal)
    (return-from declared-notinline t) ; keine Funktion rekursiv inline expandieren!
  )
  (loop
    (when (atom denv)
      (when *compiling-from-file*
        (when (member fun *notinline-functions* :test #'equal) (return t))
        (when (member fun *inline-functions* :test #'equal) (return nil))
      )
      (return (eq (get (get-funname-symbol fun) 'inlinable) 'notinline))
    )
    (let ((declspec (car denv)))
      (when (and (eq (car declspec) 'INLINE) (member fun (cdr declspec) :test #'equal))
        (return nil)
      )
      (when (and (eq (car declspec) 'NOTINLINE) (member fun (cdr declspec) :test #'equal))
        (return t)
    ) )
    (setq denv (cdr denv))
) )

; (declared-constant-notinline sym denv) stellt fest, ob sym - ein Symbol, das
; eine globale Konstante, die nicht durch eine lokale Variablendefinition
; verdeckt ist, benennt - in denv als CONSTANT-NOTINLINE deklariert ist.
(defun declared-constant-notinline (sym &optional (denv *denv*))
  (loop
    (when (atom denv)
      (when *compiling-from-file*
        (when (member sym *notinline-constants*) (return t))
        (when (member sym *inline-constants*) (return nil))
      )
      (return (eq (get sym 'constant-inlinable) 'constant-notinline))
    )
    (let ((declspec (car denv)))
      (when (and (eq (car declspec) 'CONSTANT-INLINE) (member sym (cdr declspec)))
        (return nil)
      )
      (when (and (eq (car declspec) 'CONSTANT-NOTINLINE) (member sym (cdr declspec)))
        (return t)
    ) )
    (setq denv (cdr denv))
) )


;             F U N K T I O N E N - V E R W A L T U N G

; Ein FNODE enthält die nötige Information für eine Funktion:
(defstruct (fnode (:copier nil))
  name            ; Name, ein Symbol oder (SETF symbol)
  code            ; Code dieser Funktion (zuerst nichts, dann ein ANODE,
                  ; dann eine Closure)
  ; Ab hier Beschreibungen für die kommende Closure:
  venvconst       ; Flag, ob das Venv dieser Funktion explizit beim Aufbau
                  ; mitgegeben werden muss (oder immer NIL ist)
  venvc           ; Aussehen des Venv, das dieser Funktion beim Aufbau
                  ; mitgegeben werden muss (wenn überhaupt)
  Blocks-Offset   ; Anzahl der Konstanten bis hierher
  (Blocks nil)    ; Liste der Block-Konstrukte, die dieser Funktion beim Aufbau
                  ; mitgegeben werden müssen
  Tagbodys-Offset ; Anzahl der Konstanten bis hierher
  (Tagbodys nil)  ; Liste der Tagbody-Konstrukte, die dieser Funktion beim
                  ; Aufbau mitgegeben werden müssen
  Keyword-Offset  ; Anzahl der lokalen Konstanten bis hierher
                  ; = Anfangsoffset der Keywords in FUNC
                  ; (also =0 genau dann, wenn die Funktion autonom ist)
  (req-anz 0)     ; Anzahl der required parameter
  (opt-anz 0)     ; Anzahl der optionalen Parameter
  (rest-flag nil) ; Flag, ob &REST - Parameter angegeben.
  (keyword-flag nil) ; Flag, ob &KEY - Parameter angegeben.
  (keywords nil)  ; Liste der Keyword-Konstanten (in der richtigen Reihenfolge)
  allow-other-keys-flag ; &ALLOW-OTHER-KEYS-Flag
  Consts-Offset   ; Anzahl der lokalen Konstanten bis hierher
  (consts nil)    ; Liste der sonstigen Konstanten dieser Funktion
                  ; Diese Liste wird erst im 2. Pass aufgebaut.
  (consts-forms nil) ; Liste der evtl. Formen, die diese Konstanten ergeben
  enclosing       ; lexikalisch nächste darüberliegende Funktion (oder NIL)
  gf-p            ; Flag, ob eine generische Funktion produziert wird
                  ; (impliziert Blocks-Offset = Tagbodys-Offset = Keyword-Offset = 0 oder 1)
)
#+CLISP (remprop 'fnode 'sys::defstruct-description)

; die aktuelle Funktion, ein FNODE:
(defvar *func*)
; das Label am Beginn des Codes der aktuellen Funktion:
(defvar *func-start-label*)

; Anzahl der bisher in der aktuellen Funktion aufgetretenen anonymen
; Funktionen (Lambda-Ausdrücke):
(defvar *anonymous-count*)

; *no-code* = T besagt, dass kein Code produziert werden soll:
(defvar *no-code*)
; Dies verhindert, dass Variablen unnötigerweise in die Closure gesteckt oder
; Optimierungen unnötigerweise unterlassen werden.


;                 F O R M E N - V E R W A L T U N G

; Bei jeder Rekursion werden folgende Variablen dynamisch gebunden:
(defvar *form*)      ; die aktuelle Form
(defvar *for-value*) ; ob und welche Werte der Form von Belang sind:
                     ; NIL : Werte sind irrelevant
                     ; ONE : nur der erste Wert ist relevant
                     ; ALL : alle Werte sind relevant

; Ein ANODE ist die Codierung der Information, die beim Compilieren einer Form
; gebraucht wird.
(defstruct (anode
            (:constructor mk-anode (#+COMPILER-DEBUG source
                                    type
                                    #+COMPILER-DEBUG sub-anodes
                                    seclass
                                    code
                                    #+COMPILER-DEBUG stackz
            )                      )
            (:copier nil)
           )
  #+COMPILER-DEBUG
  source              ; die zu dieser Form gehörige Source, meist eine Form
                      ; (nur zu Debugzwecken erforderlich)
  type                ; Typ des ANODE (CALL, PRIMOP, VAR, LET, SETQ, ...)
  #+COMPILER-DEBUG
  sub-anodes          ; alle ANODEs der Unterformen
  seclass             ; Seiteneffekt-Klassifikation
  code                ; erzeuger LAP-Code, eine Liste aus LAP-Anweisungen
                      ; und ANODEs
  #+COMPILER-DEBUG
  stackz              ; Zustand der Stacks beim Eintritt in den zugehörigen
                      ; LAP-Code
)
#+CLISP (remprop 'anode 'sys::defstruct-description)
; (make-anode ...) ist dasselbe wie mk-anode, nur dass dabei die Argumente
; mit Keywords markiert werden und wegen #+COMPILER-DEBUG unnötige
; Komponenten trotzdem dastehen dürfen.
(eval-when (compile eval)
  (defmacro make-anode (&key
                        (source `*form*)
                        type
                        (sub-anodes `'())
                        seclass
                        code
                        (stackz `*stackz*)
                       )
    `(mk-anode #+COMPILER-DEBUG ,source
               ,type
               #+COMPILER-DEBUG ,sub-anodes
               ,seclass
               ,code
               #+COMPILER-DEBUG ,stackz
     )
) )

#|
; Eine Seiteneffekt-Klasse (SECLASS) ist ein Indikator:
; NIL : dieses ANODE produziert keine Seiteneffekte,
;       sein Wert ist nicht von Seiteneffekten beeinflussbar.
; VAL : dieses ANODE produziert keine Seiteneffekte,
;       sein Wert ist aber von Seiteneffekten beeinflussbar.
; T   : dieses ANODE kann Seiteneffekte produzieren.
; Somit:
;   Falls der Wert uninteressant ist, kann ein ANODE mit SECLASS = NIL/VAL
;   weggelassen werden.
;   In der Reihenfolge der Auswertung dürfen vertauscht werden ANODEs mit
;   SECLASS     NIL-NIL, NIL-VAL, NIL-T, VAL-VAL.

; (seclass-or class1 ... classk) bestimmt die Gesamtklasse der Ausführung
; aller Klassen.
(defun seclass-or (&rest args)
  (cond ((member 'T args :test #'eq) 'T)
        ((member 'VAL args :test #'eq) 'VAL)
        (t 'NIL)
) )
; Dito, mit nur 2 Argumenten
(defun seclass-or-2 (seclass1 seclass2)
  (or (eq seclass1 'T) seclass2 seclass1)
)
; Damit die Liste der sub-anodes nicht gebildet werden muss, aber dennoch
; der zu dieser Liste gehörige Seiteneffektklasse berechnet werden kann:
(eval-when (compile eval)
  (defmacro anodes-seclass-or (&rest anodeforms)
    (reduce #'(lambda (form1 form2) `(SECLASS-OR-2 ,form1 ,form2))
            (mapcar #'(lambda (anodeform) `(ANODE-SECLASS ,anodeform))
                    anodeforms
  ) )       )
  (define-modify-macro seclass-or-f (anode) seclass-or-anode)
  (defmacro seclass-or-anode (seclass anode)
    `(SECLASS-OR-2 ,seclass (ANODE-SECLASS ,anode))
  )
)
(defun anodelist-seclass-or (anodelist)
  (apply #'seclass-or (mapcar #'anode-seclass anodelist))
)

; Stellt fest, ob zwei Anodes in der Reihenfolge ihrer Auswertung vertauscht
; werden können - vorausgesetzt, die Stackzustände lassen das zu.
(defun anodes-commute (anode1 anode2)
  (let ((seclass1 (anode-seclass anode1))
        (seclass2 (anode-seclass anode2)))
    (or (eq seclass1 'NIL) (eq seclass2 'NIL)
        (and (eq seclass1 'VAL) (eq seclass2 'VAL))
) ) )
|#

; Eine Seiteneffekt-Klasse (SECLASS) ist ein Indikator (uses . modifies):
; uses = NIL : dieses Anode ist nicht von Seiteneffekten beeinflussbar,
;        Liste : dieses Anode ist vom Wert der Variablen in der Liste abhängig,
;        T : dieses Anode ist möglicherweise von jedem Seiteneffekt beeinflussbar.
; modifies = NIL : dieses Anode produziert keine Seiteneffekte
;            Liste : ... produziert Seiteneffekte nur auf die Werte der
;                    Variablen in der Liste
;            T : ... produziert Seiteneffekte unbekannten Ausmaßes.
; (Variablen sind hier VAR-Structures für lexikalische und Symbole für
; dynamische Variablen.)
; Somit:
;   Falls der Wert uninteressant ist, kann ein ANODE mit SECLASS-modifies=NIL
;   weggelassen werden.
;   In der Reihenfolge der Auswertung dürfen vertauscht werden ANODEs mit
;   SECLASS, deren uses- und modifies-Teil über Kreuz disjunkt sind.

; (seclass-or class1 ... classk) bestimmt die Gesamtklasse der Ausführung
; aller Klassen.
(defun seclass-or (&rest args)
  (if (null args) '(NIL . NIL) (reduce #'seclass-or-2 args))
)
; Dito, mit nur 2 Argumenten
(defun seclass-or-2 (seclass1 seclass2)
  (cons (if (or (eq (car seclass1) 'T) (eq (car seclass2) 'T))
          'T
          (union (car seclass1) (car seclass2))
        )
        (if (or (eq (cdr seclass1) 'T) (eq (cdr seclass2) 'T))
          'T
          (union (cdr seclass1) (cdr seclass2))
) )     )

; Damit die Liste der sub-anodes nicht gebildet werden muss, aber dennoch
; der zu dieser Liste gehörige Seiteneffektklasse berechnet werden kann:
(eval-when (compile eval)
  (defmacro anodes-seclass-or (&rest anodeforms)
    (reduce #'(lambda (form1 form2) `(SECLASS-OR-2 ,form1 ,form2))
            (mapcar #'(lambda (anodeform) `(ANODE-SECLASS ,anodeform))
                    anodeforms
  ) )       )
  (define-modify-macro seclass-or-f (anode) seclass-or-anode)
  (defmacro seclass-or-anode (seclass anode)
    `(SECLASS-OR-2 ,seclass (ANODE-SECLASS ,anode))
  )
)
(defun anodelist-seclass-or (anodelist)
  (apply #'seclass-or (mapcar #'anode-seclass anodelist))
)

; Seiteneffekte auf weiter innen gebundene lexikalische Variablen zählen
; nicht und werden deshalb eliminiert:
(defun seclass-without (seclass varlist)
  (flet ((bound (var) (member var varlist))) ; testet, ob var gebunden wird
    ; (Dynamische Variablen werden nicht eliminiert; sie sind in varlist
    ; als VAR-Structures und in seclass als Symbole enthalten.)
    (cons (if (eq (car seclass) 'T) 'T (remove-if #'bound (car seclass)))
          (if (eq (cdr seclass) 'T) 'T (remove-if #'bound (cdr seclass)))
) ) )

; Stellt fest, ob zwei Anodes in der Reihenfolge ihrer Auswertung vertauscht
; werden können - vorausgesetzt, die Stackzustände lassen das zu.
(defun anodes-commute (anode1 anode2)
  (seclasses-commute (anode-seclass anode1) (anode-seclass anode2))
)
(defun seclasses-commute (seclass1 seclass2)
  (flet ((disjoint-p (uses modifies)
           (or (null uses) (null modifies)
               (and (not (eq uses 'T)) (not (eq modifies 'T))
                    (null (intersection uses modifies))
        )) )   )
    (and (disjoint-p (car seclass1) (cdr seclass2))
         (disjoint-p (car seclass2) (cdr seclass1))
) ) )


;            H I L F S F U N K T I O N E N

; Zerlegt einen Funktionsnamen in Package und String.
(defun get-funname-string+pack (funname)
  (if (atom funname)
    (values (symbol-name funname) (symbol-package funname))
    (values (concatenate 'string "(" (symbol-name (first funname)) " "
                                     (symbol-name (second funname)) ")"
            )
            (symbol-package (second funname))
) ) )

; Liefert einen Funktionsnamen, der sich aus der Package und dem Printname eines
; gegebenen Funktionsnamen, einem Bindestrich und einem Suffix zusammensetzt.
(defun symbol-suffix (funname suffix)
  (if (and (symbolp funname) (null (symbol-package funname))
           (function-name-p suffix)
      )
    suffix
    (multiple-value-bind (name pack) (get-funname-string+pack funname)
      ; suffix in einen String umwandeln:
      (cond ((symbolp suffix) (setq suffix (symbol-name suffix)))
            ((not (stringp suffix))
             (setq suffix (write-to-string suffix :escape nil :base 10 :radix nil :readably nil))
      )     )
      ; neues Symbol bilden:
      (let ((new-name (concatenate 'string name "-" suffix)))
        (if pack (intern new-name pack) (make-symbol new-name))
) ) ) )

; (C-COMMENT controlstring . args)
; gibt eine Zusatzinformation des Compilers aus (mittels FORMAT).
(defun c-comment (cstring &rest args)
  (let ((dest (if *compile-verbose* *c-error-output* *c-listing-output*)))
    (when dest (apply #'format dest cstring args))
) )

; (C-SOURCE-LOCATION)
; liefert eine Beschreibung, an welcher Source-Stelle man sich befindet.
(defun c-source-location ()
  (if (and *compiling-from-file* *compile-file-lineno1* *compile-file-lineno2*)
    (format nil
            (if (= *compile-file-lineno1* *compile-file-lineno2*)
              (ENGLISH " in line ~D")
              (ENGLISH " in lines ~D..~D")
            )
            *compile-file-lineno1* *compile-file-lineno2*
    )
    ""
) )

(defvar *warning-count*)
; (C-WARN controlstring . args)
; gibt eine Compiler-Warnung aus (mittels FORMAT).
(defun c-warn (cstring &rest args)
  (setq cstring
    (concatenate 'string (ENGLISH "~%WARNING~@[ in function ~S~]~A :~%")
                         cstring
  ) )
  (incf *warning-count*)
  (let ((dest (if *compile-warnings* *c-error-output* *c-listing-output*)))
    (when dest
      (apply #'format dest cstring
             (and (boundp '*func*) (fnode-p *func*) (fnode-name *func*))
             (c-source-location)
             args
) ) ) )

(defvar *style-warning-count*)
; (C-STYLE-WARN controlstring . args)
; gibt eine Stil-Warnung aus (mittels FORMAT).
(defun c-style-warn (cstring &rest args)
  (incf *style-warning-count*)
  (apply #'c-warn cstring args)
)

(defvar *error-count*)
; (C-ERROR controlstring . args)
; gibt einen Compiler-Error aus (mittels FORMAT) und beendet das laufende C-FORM.
(defun c-error (cstring &rest args)
  (incf *error-count*)
  (let ((in-function
          (and (boundp '*func*) (fnode-p *func*) (fnode-name *func*))
       ))
    (when in-function
      (when *compiling-from-file* (pushnew in-function *functions-with-errors*))
    )
    (format *c-error-output*
            (ENGLISH "~%ERROR~@[ in function ~S~]~A :~%~?")
            in-function (c-source-location)
            cstring args
  ) )
  (throw 'c-error
    (make-anode :source NIL
                :type 'ERROR
                :sub-anodes '()
                :seclass '(NIL . NIL)
                :code '((NIL))
) ) )

; (c-eval-when-compile form) führt eine Form zur Compile-Zeit aus.
(defun c-eval-when-compile (form)
  (when (and *compiling-from-file* *liboutput-stream*)
    ; Form auf den Liboutput-Stream schreiben:
    (terpri *liboutput-stream*)
    (write form :stream *liboutput-stream* :pretty t
                :readably t :right-margin 79
                ; :closure t :circle t :array t :gensym t
                ; :escape t :level nil :length nil :radix t
  ) )
  ; Form evaluieren:
  (eval form)
)

; (l-constantp form) stellt fest, ob form im Compiler als Load-Time-Konstante
; gehandhabt werden darf.
(defun l-constantp (form)
  (if (atom form)
    (or (numberp form) (characterp form) (arrayp form)
        (and (symbolp form)
             (cond ((keywordp form) t)
                   ((eq (symbol-package form) *lisp-package*)
                    (constantp form)
                   )
                   (t (not (null (assoc form *constant-special-vars*))))
    )   )    )
    (and (eq (first form) 'QUOTE) (consp (cdr form)) (null (cddr form)))
) )

; (c-constantp form) stellt fest, ob form im Compiler als Compile-Time-
; Konstante gehandhabt werden darf, und der Wert bekannt ist und inline
; eingesetzt werden darf.
; Bei *compiling-from-file* = nil ist das mit (l-constantp form) identisch.
(defun c-constantp (form)
  (if (atom form)
    (or (numberp form) (characterp form) (arrayp form)
        (and (symbolp form)
             (cond ((keywordp form) t)
                   ((and *compiling-from-file*
                         (declared-constant-notinline form)
                    )
                    nil
                   )
                   ((eq (symbol-package form) *lisp-package*)
                    (constantp form)
                   )
                   (t (not (null (assoc form *constant-special-vars*))))
    )   )    )
    (and (eq (first form) 'QUOTE) (consp (cdr form)) (null (cddr form)))
) )

; (c-constant-value form) liefert den Wert einer Konstanten.
; (c-constantp form) wird vorausgesetzt.
(defun c-constant-value (form)
  (if (atom form)
    (cond ((numberp form) form)
          ((characterp form) form)
          ((arrayp form) form)
          ((symbolp form)
           (cond ((keywordp form) form)
                 ((eq (symbol-package form) *lisp-package*)
                  (symbol-value form)
                 )
                 (t (cdr (assoc form *constant-special-vars*)))
    )     ))
    (second form)
) )

; (anode-constantp anode) stellt fest, ob der Anode einen konstanten (und
; zur Compile-Zeit bekannten) Wert liefert.
(defun anode-constantp (anode)
  ; Anode liefert konstanten Wert jedenfalls dann, wenn sein Code
  ; (nach TRAVERSE-ANODE) genau aus ((CONST ...)) bestehen würde.
  (let ((code (anode-code anode)))
    (and (consp code) (null (cdr code)) ; Liste der Länge 1
         (let ((item (car code)))
            (cond ((consp item)
                   (and (eq (first item) 'CONST)
                        (not (eq (const-horizont (second item)) ':form))
                  ))
                  ((anode-p item) (anode-constantp item))
) ) )    )  )

; (anode-constant-value anode) liefert den Wert eines konstanten Anode.
(defun anode-constant (anode)
  (let ((item (car (anode-code anode))))
    (cond ((consp item) (second item))
          (t #|(anode-p item)|# (anode-constant item))
) ) )
(defun anode-constant-value (anode)
  (const-value (anode-constant anode))
)

; (new-const value) liefert eine Konstante in *func* mit dem Wert value
; im 1. Pass
(defun new-const (value)
  (make-const :horizont ':value :value value)
)

; (make-label for-value) liefert ein neues Label. for-value (NIL/ONE/ALL)
; gibt an, welche der Werte nach dem Label gebraucht werden.
(defun make-label (for-value)
  (let ((label (gensym)))
    (setf (symbol-value label) '()) ; Referenzliste für 2. Pass := leer
    (setf (get label 'for-value) for-value)
    label
) )

; liefert eine Special-Variable
(defun make-special-var (symbol)
  (make-var :name symbol :specialp t
            :constantp (l-constantp symbol)
            :constant (if (l-constantp symbol)
                        (if (c-constantp symbol)
                          (make-const :horizont ':all
                                      :value (c-constant-value symbol)
                                      :form symbol
                          )
                          (make-const :horizont ':form
                                      :form symbol
) )                   ) ) )


;                     E R S T E R   P A S S

; (test-list L) stellt fest, ob L eine echte Liste ist, die mit NIL endet
; und mindestens l1, höchstens aber l2 Elemente hat. Sonst Error.
(defun test-list (L &optional (l1 0) (l2 nil))
  (unless (and (listp L) (null (cdr (last L))))
    (c-error (ENGLISH "Code contains dotted list ~S")
             L
  ) )
  (unless (>= (length L) l1)
    (c-error (ENGLISH "Form too short, too few arguments: ~S")
             L
  ) )
  (when l2
    (unless (<= (length L) l2)
      (c-error (ENGLISH "Form too long, too many arguments: ~S")
               L
  ) ) )
)

; c-form-table enthält zu allen Funktionen/Specialforms/Macros, die speziell
; behandelt werden müssen, die Behandlungsfunktion (ohne Argumente aufzurufen).
(defconstant c-form-table
  (let ((hashtable (make-hash-table :test #'eq)))
    (mapc
      #'(lambda (acons) (setf (gethash (car acons) hashtable) (cdr acons)))
      `(; Special forms:
          (QUOTE . c-QUOTE)
          (PROGN . c-PROGN)
          (LET . ,#'(lambda () (c-LET/LET* nil)))
          (LET* . ,#'(lambda () (c-LET/LET* t)))
          (IF . c-IF)
          (SETQ . c-SETQ)
          (BLOCK . c-BLOCK)
          (RETURN-FROM . c-RETURN-FROM)
          (TAGBODY . c-TAGBODY)
          (GO . c-GO)
          (FUNCTION . c-FUNCTION)
          (MULTIPLE-VALUE-BIND . c-MULTIPLE-VALUE-BIND)
          (MULTIPLE-VALUE-SETQ . c-MULTIPLE-VALUE-SETQ)
          (AND . c-AND)
          (OR . c-OR)
          (WHEN . c-WHEN)
          (UNLESS . c-UNLESS)
          (COND . c-COND)
          (CASE . c-CASE)
          (PSETQ . c-PSETQ)
          (MULTIPLE-VALUE-CALL . c-MULTIPLE-VALUE-CALL)
          (PROG1 . c-PROG1)
          (PROG2 . c-PROG2)
          (THE . c-THE)
          (CATCH . c-CATCH)
          (THROW . c-THROW)
          (UNWIND-PROTECT . c-UNWIND-PROTECT)
          (PROGV . c-PROGV)
          (MULTIPLE-VALUE-LIST . c-MULTIPLE-VALUE-LIST)
          (MULTIPLE-VALUE-PROG1 . c-MULTIPLE-VALUE-PROG1)
          (FLET . c-FLET)
          (LABELS . c-LABELS)
          (MACROLET . c-MACROLET)
          (SYMBOL-MACROLET . c-SYMBOL-MACROLET)
          (COMPILER-LET . c-COMPILER-LET)
          (EVAL-WHEN . c-EVAL-WHEN)
          (DECLARE . c-DECLARE)
          (LOAD-TIME-VALUE . c-LOAD-TIME-VALUE)
          (LOCALLY . c-LOCALLY)
        ; Macros:
          (%GENERIC-FUNCTION-LAMBDA . c-%GENERIC-FUNCTION-LAMBDA)
          (%OPTIMIZE-FUNCTION-LAMBDA . c-%OPTIMIZE-FUNCTION-LAMBDA)
          (CLOS:GENERIC-FLET . c-GENERIC-FLET)
          (CLOS:GENERIC-LABELS . c-GENERIC-LABELS)
          (HANDLER-BIND . c-HANDLER-BIND)
          (SYS::%HANDLER-BIND . c-HANDLER-BIND)
          (SYS::CONSTANT-EQL . c-CONSTANT-EQL)
        ; Inline-compilierte Funktionen:
          (FUNCALL . c-FUNCALL)
          (SYS::%FUNCALL . c-FUNCALL)
          (APPLY . c-APPLY)
          (+ . c-PLUS)
          (- . c-MINUS)
          (SYS::SVSTORE . c-SVSTORE)
          (EQ . c-EQ)
          (EQL . c-EQL)
          (EQUAL . c-EQUAL)
          (MAPCAR . c-MAPCAR)
          (MAPLIST . c-MAPLIST)
          (MAPC . c-MAPC)
          (MAPL . c-MAPL)
          (MAPCAN . c-MAPCAN)
          (MAPCON . c-MAPCON)
          (MAPCAP . c-MAPCAP)
          (MAPLAP . c-MAPLAP)
          (TYPEP . c-TYPEP)
          (FORMAT . c-FORMAT)
          (REMOVE-IF . c-REMOVE-IF)
          (REMOVE-IF-NOT . c-REMOVE-IF-NOT)
          (DELETE-IF . c-DELETE-IF)
          (DELETE-IF-NOT . c-DELETE-IF-NOT)
          (SUBSTITUTE-IF . c-SUBSTITUTE-IF)
          (SUBSTITUTE-IF-NOT . c-SUBSTITUTE-IF-NOT)
          (NSUBSTITUTE-IF . c-NSUBSTITUTE-IF)
          (NSUBSTITUTE-IF-NOT . c-NSUBSTITUTE-IF-NOT)
          (FIND-IF . c-FIND-IF)
          (FIND-IF-NOT . c-FIND-IF-NOT)
          (POSITION-IF . c-POSITION-IF)
          (POSITION-IF-NOT . c-POSITION-IF-NOT)
          (COUNT-IF . c-COUNT-IF)
          (COUNT-IF-NOT . c-COUNT-IF-NOT)
          (SUBST-IF . c-SUBST-IF)
          (SUBST-IF-NOT . c-SUBST-IF-NOT)
          (NSUBST-IF . c-NSUBST-IF)
          (NSUBST-IF-NOT . c-NSUBST-IF-NOT)
          (MEMBER-IF . c-MEMBER-IF)
          (MEMBER-IF-NOT . c-MEMBER-IF-NOT)
          (ASSOC-IF . c-ASSOC-IF)
          (ASSOC-IF-NOT . c-ASSOC-IF-NOT)
          (RASSOC-IF . c-RASSOC-IF)
          (RASSOC-IF-NOT . c-RASSOC-IF-NOT)
          (LDB . c-LDB)
          (LDB-TEST . c-LDB-TEST)
          (MASK-FIELD . c-MASK-FIELD)
          (DPB . c-DPB)
          (DEPOSIT-FIELD . c-DEPOSIT-FIELD)
    )  )
    hashtable
) )
; Diese Tabelle muss alle Special-Forms enthalten:
(do-all-symbols (sym)
  (when (and (special-operator-p sym) (not (gethash sym c-form-table)))
    (compiler-error 'c-form-table)
) )

; compiliert eine Form.
; Dabei ergibt sich kein Code, falls keine Werte gebraucht werden und die Form
; keine Seiteneffekte produziert.
(defun c-form (*form* &optional (*for-value* *for-value*))
 (let
  ((anode
    (catch 'c-error
      (if (atom *form*)
        (cond ((symbolp *form*)
               (multiple-value-bind (macrop expansion)
                   (venv-search-macro *form* *venv*)
                 (if macrop ; Symbol-Macro ?
                   (c-form expansion) ; -> expandieren
                   (c-VAR *form*)
              )) )
              ((or (numberp *form*) (characterp *form*) (stringp *form*)
                   (bit-vector-p *form*)
                   ;; X3J13 vote <72> conditionally implemented: check *package*
                   (member (find-package "COMMON-LISP") (package-use-list *package*))
               )
               (c-CONST)
              )
              (t (c-error (ENGLISH "Invalid form: ~S")
                          *form*
        )     )  )
        (let ((fun (first *form*)))
          (if (function-name-p fun)
            (multiple-value-bind (a b c) (fenv-search fun)
              (declare (ignore b))
              (if (null a)
                ; nicht lokal definiert
                (let ((handler (gethash fun c-form-table)))
                  (if handler ; Behandlungsfunktion gefunden?
                    ; also (symbolp fun)
                    (if (or (and (special-operator-p fun)
                                 (not (macro-function fun)))
                            (not (declared-notinline fun))
                        )
                      (funcall handler) ; ja -> aufrufen
                      (if (macro-function fun)
                        (c-form (macroexpand-1 *form* (vector *venv* *fenv*))) ; -> expandieren
                        ; normaler Aufruf globaler Funktion
                        (c-GLOBAL-FUNCTION-CALL fun)
                    ) )
                    ; nein -> jedenfalls keine Special-Form (die sind ja
                    ; alle in der Tabelle).
                    (if (and (symbolp fun) (macro-function fun)) ; globaler Macro ?
                      (c-form (macroexpand-1 *form* (vector *venv* *fenv*))) ; -> expandieren
                      ; globale Funktion
                      (if (and (equal fun (fnode-name *func*))
                               (not (declared-notinline fun))
                               (member `(SYS::IN-DEFUN ,fun) *denv* :test #'equal)
                          )
                        ; rekursiver Aufruf der aktuellen globalen Funktion
                        (c-LOCAL-FUNCTION-CALL fun (cons *func* nil) (cdr *form*))
                        ; normaler Aufruf globaler Funktion
                        (c-GLOBAL-FUNCTION-CALL fun)
                ) ) ) )
                (case a
                  (SYSTEM::MACRO ; lokaler Macro
                    (c-form (macroexpand-1 *form* (vector *venv* *fenv*))) ; -> expandieren
                  )
                  (GLOBAL ; Funktion im Interpreter-Environment %fenv% gefunden
                    ; (c-form `(SYS::%FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
                    (c-FUNCALL-NOTINLINE `(FUNCTION ,fun) (cdr *form*))
                  )
                  (LOCAL ; lokale Funktion (in *fenv* gefunden)
                    ; (c-form `(SYS::%FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
                    (c-LOCAL-FUNCTION-CALL fun c (cdr *form*))
                  )
                  (t (compiler-error 'c-form))
            ) ) )
            (if (and (consp fun) (eq (car fun) 'LAMBDA))
              (c-form `(SYS::%FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
              #| nicht: (c-LAMBDA-FUNCTION-CALL fun (cdr *form*)) |#
              (c-error (ENGLISH "Not the name of a function: ~S")
                       fun
    ) ) ) ) ) )
  ))
  #+COMPILER-DEBUG (setf (anode-source anode) *form*)
  ; Falls keine Werte gebraucht werden und keine Seiteneffekte produziert
  ; werden, kann der dazugehörige Code ganz gestrichen werden:
  (when (and (null *for-value*) (null (cdr (anode-seclass anode))))
    (setf (anode-code anode) '())
    (setf (anode-seclass anode) '(NIL . NIL))
  )
  anode
))

; macroexpandiere eine Form.
; Das ist genau das, was c-form später sowieso macht.
; (c-form (macroexpand-form form)) == (c-form form).
(defun macroexpand-form (form)
  ; Der Unterschied zu (values (macroexpand form (vector *venv* *fenv*)))
  ; ist, dass wir hier Macros, die in c-form-table aufgeführt sind, nicht
  ; als Macros expandieren.
  (tagbody
    reexpand
    (if (atom form)
      (if (symbolp form)
        (multiple-value-bind (macrop expansion) (venv-search-macro form *venv*)
          (if macrop
            (progn (setq form expansion) (go reexpand))
            (go done)
        ) )
        (go done)
      )
      (let ((fun (first form)))
        (if (function-name-p fun)
          (let ((a (fenv-search fun)))
            (if (or (and (null a)
                         ; nicht lokal definiert
                         (symbolp fun) (macro-function fun) ; globaler Macro?
                         (not (gethash fun c-form-table))
                    )
                    (eq a 'SYSTEM::MACRO) ; lokaler Macro?
                )
              (progn
                (setq form (macroexpand-1 form (vector *venv* *fenv*))) ; -> expandieren
                (go reexpand)
              )
              (go done)
          ) )
          (go done)
    ) ) )
    done
    (return-from macroexpand-form form)
) )

; compiliere NIL (eine Art Notausgang)
(defun c-NIL ()
  (make-anode :type 'NIL
              :sub-anodes '()
              :seclass '(NIL . NIL)
              :code '((NIL)) )
)

; Konstante als Form:
(defun c-CONST ()
  (make-anode :type 'const
              :sub-anodes '()
              :seclass '(NIL . NIL)
              :code `((CONST ,(new-const *form*)))
) )

; Variable als Form:
(defun c-VAR (symbol)
  ; Suche die Variable in *venv* :
  (multiple-value-bind (a b c) (venv-search symbol)
    (when (eq a 'NIL)
      (c-warn (ENGLISH "~S is neither declared nor bound,~@
                        it will be treated as if it were declared SPECIAL.")
              symbol
      )
      (when *compiling-from-file*
        (pushnew symbol *unknown-free-vars* :test #'eq)
      )
      (setq a 'SPECIAL)
    )
    (case a
      (SPECIAL ; Special-Variable
        (let ((var (make-special-var symbol)))
          (make-anode
            :type 'VAR
            :sub-anodes '()
            :seclass (cons
                       (if (and *for-value* (not (var-constantp var))) (list symbol) 'NIL)
                       'NIL
                     )
            :code (if *for-value*
                    (if (var-constantp var)
                      `((CONST ,(make-const
                                  :horizont (if (keywordp symbol) ':value ':all) ; Keywords braucht man nicht in #.-Syntax
                                  :value (c-constant-value symbol)
                                  :form symbol
                       ))       )
                      `((GETVALUE ,symbol))
                    )
                    '()
      ) ) )       )
      (LOCAL ; interpretativ lexikalisch
        (make-anode
          :type 'VAR
          :sub-anodes '()
          :seclass (cons (if *for-value* 'T 'NIL) 'NIL)
          :code (if *for-value*
                  `((CONST ,(new-const b)) ; Vektor
                    (PUSH)
                    (CONST ,(new-const c)) ; Index
                    (SVREF)
                   )
                  '()
      ) )       )
      ((T) ; lexikalisch in Stack oder Closure
        (let* ((var b)
               (get-anode
                 (make-anode
                   :type 'VAR
                   :sub-anodes '()
                   :seclass (cons (if *for-value* (list var) 'NIL) 'NIL)
                   :code (if *for-value*
                           `((GET ,var ,*venvc* ,*stackz*))
                           '()
              )) )       )
          (push get-anode (var-usedp var))
          (when *for-value*
            (setf (var-for-value-usedp var) t)
            (unless *no-code*
              (setf (var-really-usedp var) t)
              (unless (eq (stackz-fun (var-stackz var)) *func*)
                (setf (var-closurep var) t)
              )
              (when (var-closurep var)
                ; aktiviere Venvconst in allen dazwischenliegenden Funktionen
                (do ((venvc *venvc* (cdr venvc)))
                    ((null venvc) (compiler-error 'c-VAR "INVISIBLE"))
                  (when (eq venvc (var-venvc var)) (return))
                  (when (fnode-p (car venvc))
                    (setf (fnode-Venvconst (car venvc)) t)
          ) ) ) ) )
          get-anode
      ) )
      (t (compiler-error 'c-VAR 'venv-search))
) ) )

; Variablenzuweisung:
(defun c-VARSET (symbol value-anode for-value)
  ; Suche die Variable in *venv* :
  (multiple-value-bind (a b c) (venv-search symbol)
    (when (eq a 'NIL)
      (c-warn (ENGLISH "~S is neither declared nor bound,~@
                        it will be treated as if it were declared SPECIAL.")
              symbol
      )
      (setq a 'SPECIAL)
    )
    (case a
      (SPECIAL ; Special-Variable
        (let ((var (make-special-var symbol)))
          (make-anode :type 'VARSET
                      :sub-anodes '()
                      :seclass (cons
                                 'NIL
                                 (if (var-constantp var) 'NIL (list symbol))
                               )
                      :code (if (var-constantp var)
                              (progn
                                (c-warn (ENGLISH "The constant ~S may not be assigned to.~@
                                                  The assignment will be ignored.")
                                        symbol
                                )
                                '((VALUES1))
                              )
                              `((SETVALUE , symbol))
      ) ) )                 )
      (LOCAL ; interpretativ lexikalisch
        (make-anode :type 'VARSET
                    :sub-anodes '()
                    :seclass (cons 'NIL 'T)
                    :code `((PUSH)
                            (CONST ,(new-const b)) ; Vektor
                            (PUSH)
                            (CONST ,(new-const c)) ; Index
                            (SVSET)
      ) )                  )
      ((T) ; lexikalisch in Stack oder Closure
        (let* ((var b)
               (set-anode
                 (make-anode :type 'VARSET
                             :sub-anodes '()
                             :seclass (cons 'NIL (list var))
                             :code `((SET ,var ,*venvc* ,*stackz*))
              )) )
          (unless (var-usedp var) (setf (var-usedp var) t)) ; Zuweisung "benutzt" die Variable
          (setf (var-assignedp var) t)
          (unless *no-code*
            (setf (var-constantp var) nil) ; nicht mehr konstant wegen Zuweisung
            (push (list* value-anode set-anode for-value) (var-modified-list var))
            (unless (eq (stackz-fun (var-stackz var)) *func*)
              (setf (var-closurep var) t)
              ; aktiviere Venvconst in allen dazwischenliegenden Funktionen
              (do ((venvc *venvc* (cdr venvc)))
                  ((null venvc) (compiler-error 'c-VARSET "INVISIBLE"))
                (when (eq venvc (var-venvc var)) (return))
                (when (fnode-p (car venvc))
                  (setf (fnode-Venvconst (car venvc)) t)
            ) ) )
            ; Das Ersetzen einer Variablen innervar durch var ist dann
            ; nicht erlaubt, wenn während der Existenzdauer von innervar
            ; an var ein Wert zugewiesen wird.
            (setf (var-replaceable-list var)
              (delete-if #'(lambda (innervar-info) ; innervar gerade aktiv?
                             (let ((innervar (first innervar-info)))
                               (tailp (var-stackz innervar) *stackz*)
                           ) )
                         (var-replaceable-list var)
            ) )
          )
          set-anode
      ) )
      (t (compiler-error 'c-VARSET 'venv-search))
) ) )

;; Funktionsaufrufe, bei denen die Funktion ein Symbol oder (SETF symbol) ist:

(defun make-funname-const (name)
  (if (atom name)
    (new-const name)
    (let ((symbol (second name)))
      (make-const :horizont ':all
                  :value (system::get-setf-symbol symbol)
                  :form `(SYSTEM::GET-SETF-SYMBOL ',symbol)
) ) ) )

; Global function call, normal (notinline): (fun {form}*)
(defun c-NORMAL-FUNCTION-CALL (fun) ; fun ist ein Symbol oder (SETF symbol)
  (test-list *form* 1)
  (let* ((n (length (cdr *form*)))
         #+COMPILER-DEBUG (oldstackz *stackz*)
         (*stackz* *stackz*))
    (do ((formlist (cdr *form*))
         #+COMPILER-DEBUG (anodelist '())
         (codelist (list '(CALLP))))
        ((null formlist)
         (push
           `(,@(case n
                 (0 `(CALL0)) (1 `(CALL1)) (2 `(CALL2)) (t `(CALL ,n))
               )
             ,(make-funname-const fun)
            )
           codelist
         )
         (make-anode
           :type 'CALL
           :sub-anodes (nreverse anodelist)
           :seclass '(T . T)
           :code (nreverse codelist)
           :stackz oldstackz
        ))
      (let* ((formi (pop formlist))
             (anodei (c-form formi 'ONE)))
        #+COMPILER-DEBUG (push anodei anodelist)
        (push anodei codelist)
        (push '(PUSH) codelist)
        (push 1 *stackz*)
) ) ) )

; Liefert die Signatur einer Funktion aus dem fdescr
(defun fdescr-signature (fdescr)
  (if (cdr fdescr)
    (if (eq (cadr fdescr) 'LABELS)
      ; bei LABELS: aus der Lambdalisten-Information
      (multiple-value-bind (reqvar  optvar optinit optsvar  restvar
                            keyflag keyword keyvar keyinit keysvar allow-other-keys
                            auxvar auxinit)
          (values-list (cddr fdescr))
        (declare (ignore optinit optsvar keyvar keyinit keysvar auxvar auxinit))
        (values (length reqvar) (length optvar)
                (not (eql restvar 0)) keyflag
                keyword allow-other-keys
      ) )
      ; bei GENERIC-FLET oder GENERIC-LABELS: aus der Signatur
      (values-list (cddr fdescr))
    )
    ; bei FLET oder IN-DEFUN: aus dem fnode
    (let ((fnode (car fdescr)))
      (values (fnode-req-anz fnode) (fnode-opt-anz fnode)
              (fnode-rest-flag fnode) (fnode-keyword-flag fnode)
              (fnode-keywords fnode) (fnode-allow-other-keys-flag fnode)
) ) ) )

; (test-argument-syntax args applyargs fun req opt rest-p key-p keylist allow-p)
; überprüft, ob die Argumentliste args (und evtl. weitere Argumente applyargs)
; als Argumentliste zu fun (Symbol) geeignet ist, d.h. ob sie der gegebenen
; Spezifikation, gegeben durch req,opt,rest-p,keylist,allow-p, genügt.
; Gegebenenfalls wird eine Warnung ausgegeben.
; Liefert:
;   NO-KEYS           bei korrekter Syntax, ohne Keywords,
;   STATIC-KEYS       bei korrekter Syntax mit konstanten Keywords,
;   DYNAMIC-KEYS      bei (vermutlich) korrekter Syntax,
;                       mit nicht-konstanten Keywords.
;   NIL               bei fehlerhafter Syntax,
; In den ersten beiden Fällen ist
; falls (not applyargs):
;   req <= (length args) <= (req+opt oder, falls rest-p oder key-p, unendlich)
; bzw. falls applyargs:
;   (length args) <= (req+opt oder, falls rest-p oder key-p, unendlich).
(defun test-argument-syntax (args applyargs fun req opt rest-p key-p keylist allow-p)
  (unless (and (listp args) (null (cdr (last args))))
    (c-error (ENGLISH "argument list to function ~S is dotted: ~S")
             fun args
  ) )
  (let ((n (length args))
        (reqopt (+ req opt)))
    (unless (and (or applyargs (<= req n)) (or rest-p key-p (<= n reqopt)))
      (c-warn (ENGLISH "~S called with ~S~:[~; or more~] arguments, but it requires ~
                        ~:[~:[from ~S to ~S~;~S~]~;at least ~*~S~] arguments.")
              fun n applyargs
              (or rest-p key-p)  (eql req reqopt) req reqopt
      )
      (return-from test-argument-syntax 'NIL)
    )
    (unless key-p (return-from test-argument-syntax 'NO-KEYS))
    ; Mit Keywords.
    (when (<= n reqopt) (return-from test-argument-syntax 'STATIC-KEYS))
    (when rest-p (return-from test-argument-syntax 'DYNAMIC-KEYS))
    (setq n (- n reqopt) args (nthcdr reqopt args))
    (unless (evenp n)
      (c-warn (ENGLISH "keyword arguments to function ~S should occur pairwise: ~S")
              fun args
      )
      (return-from test-argument-syntax 'NIL)
    )
    (do ((keyargs args (cddr keyargs))
         (allow-flag allow-p)
         (wrong-key nil)
        )
        ((null keyargs)
         (if wrong-key
           (c-error (ENGLISH "keyword ~S is not allowed for function ~S.~
                              ~%The only allowed keyword~:[s are ~{~S~#[~; and ~S~:;, ~]~}~; is ~{~S~}~].")
                    wrong-key fun (eql (length keylist) 1) keylist
           )
           'STATIC-KEYS
        ))
      (let ((key (first keyargs)))
        (unless (c-constantp key)
          (return-from test-argument-syntax 'DYNAMIC-KEYS)
        )
        (setq key (c-constant-value key))
        (unless (symbolp key)
          (c-warn (ENGLISH "argument ~S to function ~S is not a symbol")
                  (first keyargs) fun
          )
          (return-from test-argument-syntax 'DYNAMIC-KEYS)
        )
        (when (eq key ':ALLOW-OTHER-KEYS)
          (unless (c-constantp (second keyargs))
            (return-from test-argument-syntax 'DYNAMIC-KEYS)
          )
          (when (c-constant-value (second keyargs)) (setq allow-flag t))
        )
        (unless (or allow-flag (member key keylist :test #'eq))
          (setq wrong-key key)
    ) ) )
) )

; (c-DIRECT-FUNCTION-CALL args applyargs fun req opt rest-p key-p keylist
;                         subr-flag call-code-producer)
; compiliert die Abarbeitung der Argumente für den Direktaufruf einer
; Funktion (d.h. ohne Argument-Check zur Laufzeit).
; (test-argument-syntax ...) muss die Argumente bereits erfolgreich (d.h.
; mit Ergebnis NO-KEYS oder STATIC-KEYS) überprüft haben.
; args : Liste der Argumentformen,
; applyargs : falls angegeben, Liste einer Form für die weiteren Argumente,
; fun : Name der aufzurufenden Funktion (Symbol),
; req,opt,rest-p,key-p,keylist,allow-p : Information über die Lambdaliste von fun
; subr-flag : Flag, ob fun ein SUBR oder aber eine compilierte Closure ist,
;             (Obacht: applyargs nur bei compilierten Closures verwenden!),
; call-code-producer : Funktion, die den Code liefert, der am Ende anzufügen
;                      ist und den Aufruf ausführt.
(defun c-DIRECT-FUNCTION-CALL (args applyargs fun req opt rest-p key-p keylist
                               subr-flag call-code-producer)
  (let* ((foldable nil)
         (sideeffects ; Seiteneffektklasse des Funktionsaufrufs selbst
           (if (not subr-flag)
             '(T . T) ; kein SUBR -> kann nichts aussagen
             (case fun ; fun ein SUBR
               (; Seiteneffektklasse (NIL . NIL) haben diejenigen Funktionen,
                ; die ihre Argumente nur anschauen (Pointer, Inhalt nur bei
                ; Zahlen oder ähnlichen unmodifizierbaren Datenstrukturen)
                ; und auf keine globalen Variablen zugreifen.
                ; Eine Funktion, die, zweimal mit denselben Argumenten auf-
                ; gerufen, stets dasselbe Ergebnis liefert (im EQL-Sinne),
                ; erlaubt Constant-Folding: Sind alle Argumente Konstanten
                ; und der Funktionsaufruf durchführbar, so darf der Funktions-
                ; aufruf durch das konstante Funktionsergebnis ersetzt werden.
                ;
                ; This is the list of SUBRs which have no side effects,
                ; don't depend on global variables or such, don't even look
                ; "into" their arguments, and are "foldable" (two calls with
                ; identical arguments give the same result, and calls with
                ; constant arguments can be evaluated at compile time).
                (SYSTEM::%FUNTABREF
                 ARRAY-ELEMENT-TYPE ARRAY-RANK ADJUSTABLE-ARRAY-P
                 STANDARD-CHAR-P GRAPHIC-CHAR-P STRING-CHAR-P ALPHA-CHAR-P UPPER-CASE-P
                 LOWER-CASE-P BOTH-CASE-P DIGIT-CHAR-P ALPHANUMERICP CHAR= CHAR/= CHAR< CHAR>
                 CHAR<= CHAR>= CHAR-EQUAL CHAR-NOT-EQUAL CHAR-LESSP CHAR-GREATERP
                 CHAR-NOT-GREATERP CHAR-NOT-LESSP CHAR-CODE CODE-CHAR
                 CHAR-UPCASE CHAR-DOWNCASE DIGIT-CHAR CHAR-INT INT-CHAR
                 CHAR-NAME
                 SPECIAL-OPERATOR-P
                 ENDP
                 IDENTITY
                 EQ EQL CONSP ATOM SYMBOLP STRINGP NUMBERP
                 NULL NOT SYSTEM::CLOSUREP LISTP INTEGERP SYSTEM::FIXNUMP RATIONALP FLOATP
                 SYSTEM::SHORT-FLOAT-P SYSTEM::SINGLE-FLOAT-P SYSTEM::DOUBLE-FLOAT-P SYSTEM::LONG-FLOAT-P
                 REALP COMPLEXP STREAMP SYSTEM::FILE-STREAM-P SYSTEM::SYNONYM-STREAM-P
                 SYSTEM::BROADCAST-STREAM-P SYSTEM::CONCATENATED-STREAM-P SYSTEM::TWO-WAY-STREAM-P
                 SYSTEM::ECHO-STREAM-P SYSTEM::STRING-STREAM-P
                 RANDOM-STATE-P READTABLEP HASH-TABLE-P PATHNAMEP
                 HASH-TABLE-TEST
                 SYSTEM::LOGICAL-PATHNAME-P CHARACTERP FUNCTIONP PACKAGEP ARRAYP SIMPLE-ARRAY-P
                 BIT-VECTOR-P VECTORP SIMPLE-VECTOR-P SIMPLE-STRING-P SIMPLE-BIT-VECTOR-P
                 SYSTEM::SYMBOL-MACRO-P CLOS::STRUCTURE-OBJECT-P CLOS::STD-INSTANCE-P
                 ZEROP PLUSP MINUSP ODDP EVENP = /= < > <= >= MAX MIN
                 + - * / 1+ 1- CONJUGATE GCD LCM ISQRT
                 RATIONAL RATIONALIZE NUMERATOR DENOMINATOR FLOOR CEILING TRUNCATE
                 ROUND MOD REM DECODE-FLOAT SCALE-FLOAT
                 FLOAT-RADIX FLOAT-SIGN FLOAT-DIGITS FLOAT-PRECISION INTEGER-DECODE-FLOAT
                 COMPLEX REALPART IMAGPART LOGIOR LOGXOR LOGAND LOGEQV LOGNAND LOGNOR
                 LOGANDC1 LOGANDC2 LOGORC1 LOGORC2 BOOLE LOGNOT LOGTEST LOGBITP ASH LOGCOUNT
                 INTEGER-LENGTH LDB LDB-TEST MASK-FIELD DPB DEPOSIT-FIELD ! EXQUO
                ) ; alle diese sind SUBRs ohne Keyword-Parameter
                (setq foldable t)
                '(NIL . NIL)
               )
               (;
                ; This is the list of SUBRs which have no side effects,
                ; don't depend on global variables or such, don't even look
                ; "into" their arguments, but are not "foldable".
                (VECTOR MAKE-STRING
                 VALUES ; nicht foldable, um Endlosschleife zu verhindern!
                 CONS LIST LIST* MAKE-LIST ACONS
                 LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-VERSION SOFTWARE-TYPE
                 SOFTWARE-VERSION
                 SYSTEM::MAKE-LOAD-TIME-EVAL SYSTEM::MAKE-SYMBOL-MACRO
                 SYMBOL-NAME
                 SYSTEM::DECIMAL-STRING
                )
                '(NIL . NIL)
               )
               (;
                ; This is the list of SUBRs which have no side effects,
                ; but depend on global variables or look "into" their arguments.
                (SYSTEM::SUBR-INFO
                 SYSTEM::%COPY-SIMPLE-VECTOR AREF SVREF ROW-MAJOR-AREF
                 ARRAY-DIMENSION ARRAY-DIMENSIONS ARRAY-TOTAL-SIZE
                 ARRAY-IN-BOUNDS-P ARRAY-ROW-MAJOR-INDEX BIT SBIT
                 ARRAY-HAS-FILL-POINTER-P FILL-POINTER MAKE-ARRAY
                 CHARACTER CHAR SCHAR STRING= STRING/= STRING< STRING> STRING<=
                 STRING>= STRING-EQUAL STRING-NOT-EQUAL STRING-LESSP STRING-GREATERP
                 STRING-NOT-GREATERP STRING-NOT-LESSP SYSTEM::SEARCH-STRING=
                 SYSTEM::SEARCH-STRING-EQUAL SYSTEM::STRING-BOTH-TRIM STRING-UPCASE
                 STRING-DOWNCASE STRING-CAPITALIZE STRING NAME-CHAR SUBSTRING STRING-CONCAT
                 MAKE-SYMBOL SYMBOL-VALUE SYMBOL-FUNCTION BOUNDP FBOUNDP
                 VALUES-LIST MACRO-FUNCTION CONSTANTP
                 MAKE-HASH-TABLE GETHASH HASH-TABLE-COUNT HASH-TABLE-REHASH-SIZE
                 HASH-TABLE-REHASH-THRESHOLD HASH-TABLE-SIZE SYSTEM::HASH-TABLE-ITERATOR SXHASH
                 GET-MACRO-CHARACTER GET-DISPATCH-MACRO-CHARACTER
                 CAR CDR CAAR CADR CDAR CDDR CAAAR CAADR CADAR CADDR CDAAR CDADR CDDAR CDDDR
                 CAAAAR CAAADR CAADAR CAADDR CADAAR CADADR CADDAR CADDDR
                 CDAAAR CDAADR CDADAR CDADDR CDDAAR CDDADR CDDDAR CDDDDR
                 LIST-LENGTH NTH FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH
                 EIGHTH NINTH TENTH REST NTHCDR LAST APPEND COPY-LIST
                 COPY-ALIST COPY-TREE REVAPPEND BUTLAST LDIFF TAILP PAIRLIS
                 GET-UNIVERSAL-TIME GET-INTERNAL-RUN-TIME
                 GET-INTERNAL-REAL-TIME SYSTEM::%%TIME
                 FIND-PACKAGE PACKAGE-NAME PACKAGE-NICKNAMES PACKAGE-USE-LIST
                 PACKAGE-USED-BY-LIST PACKAGE-SHADOWING-SYMBOLS LIST-ALL-PACKAGES FIND-SYMBOL
                 FIND-ALL-SYMBOLS
                 PARSE-NAMESTRING PATHNAME PATHNAME-HOST PATHNAME-DEVICE PATHNAME-DIRECTORY
                 PATHNAME-NAME PATHNAME-TYPE PATHNAME-VERSION FILE-NAMESTRING
                 DIRECTORY-NAMESTRING HOST-NAMESTRING MERGE-PATHNAMES ENOUGH-NAMESTRING
                 MAKE-PATHNAME NAMESTRING TRUENAME PROBE-FILE DIRECTORY FILE-WRITE-DATE
                 FILE-AUTHOR
                 EQUAL EQUALP COMPILED-FUNCTION-P CLOS::GENERIC-FUNCTION-P COMMONP
                 TYPE-OF CLOS::CLASS-P CLOS:CLASS-OF COERCE
                 SYSTEM::%RECORD-REF SYSTEM::%RECORD-LENGTH SYSTEM::%STRUCTURE-REF SYSTEM::%MAKE-STRUCTURE
                 COPY-STRUCTURE SYSTEM::%STRUCTURE-TYPE-P SYSTEM::CLOSURE-NAME
                 SYSTEM::CLOSURE-CODEVEC SYSTEM::CLOSURE-CONSTS SYSTEM::MAKE-CODE-VECTOR
                 SYSTEM::%MAKE-CLOSURE CLOS::OLD-%ALLOCATE-INSTANCE CLOS:SLOT-EXISTS-P
                 SYSTEM::SEQUENCEP ELT SUBSEQ COPY-SEQ LENGTH REVERSE CONCATENATE
                 MAKE-SYNONYM-STREAM SYNONYM-STREAM-SYMBOL MAKE-BROADCAST-STREAM
                 BROADCAST-STREAM-STREAMS MAKE-CONCATENATED-STREAM
                 CONCATENATED-STREAM-STREAMS MAKE-TWO-WAY-STREAM
                 TWO-WAY-STREAM-INPUT-STREAM TWO-WAY-STREAM-OUTPUT-STREAM
                 MAKE-ECHO-STREAM ECHO-STREAM-INPUT-STREAM
                 ECHO-STREAM-OUTPUT-STREAM MAKE-STRING-INPUT-STREAM
                 SYSTEM::STRING-INPUT-STREAM-INDEX MAKE-STRING-OUTPUT-STREAM
                 SYSTEM::MAKE-STRING-PUSH-STREAM MAKE-BUFFERED-INPUT-STREAM
                 MAKE-BUFFERED-OUTPUT-STREAM SYSTEM::BUILT-IN-STREAM-OPEN-P
                 INPUT-STREAM-P OUTPUT-STREAM-P SYSTEM::BUILT-IN-STREAM-ELEMENT-TYPE
                 FILE-LENGTH
                 GET GETF GET-PROPERTIES SYMBOL-PACKAGE SYMBOL-PLIST KEYWORDP
                 SYSTEM::SPECIAL-VARIABLE-P GENSYM
                 FFLOOR FCEILING FTRUNCATE FROUND
                 EXP EXPT LOG SQRT ABS PHASE SIGNUM SIN COS TAN CIS ASIN ACOS ATAN
                 SINH COSH TANH ASINH ACOSH ATANH FLOAT BYTE BYTE-SIZE BYTE-POSITION
                 SYSTEM::LOG2 SYSTEM::LOG10
                )
                '(T . NIL)
               )
               ; All other SUBRs (which may have side effects) are subsumed here.
               (t '(T . T)) ; vielleicht Seiteneffekte
        )) ) )
    (if (and (null *for-value*) (null (cdr sideeffects)))
      ; Brauche die Funktion nicht aufzurufen, nur die Argumente auswerten
      (progn
        (let ((*no-code* t) (*for-value* 'NIL))
          (funcall call-code-producer)
        )
        (c-form `(PROGN ,@args ,@applyargs))
      )
      (let ((n (length args))
            (reqopt (+ req opt))
            (seclass sideeffects)
            (codelist '()))
        (let ((*stackz* *stackz*))
          ; required und angegebene optionale Parameter:
          (dotimes (i (min n reqopt))
            (let* ((formi (pop args))
                   (anodei (c-form formi 'ONE)))
              (seclass-or-f seclass anodei)
              (push anodei codelist)
            )
            (push '(PUSH) codelist)
            (push 1 *stackz*)
          )
          (if applyargs
            (progn
              (when subr-flag (compiler-error 'c-DIRECT-FUNCTION-CALL "APPLY-SUBR"))
              (when key-p (compiler-error 'c-DIRECT-FUNCTION-CALL "APPLY-KEY"))
              (if (>= reqopt n)
                ; fehlende optionale Parameter werden aus der Liste initialisiert:
                (let* ((anz (- reqopt n))
                       (anode1 (c-form (first applyargs) 'ONE))
                       (anode2 (progn
                                 (push (if rest-p (+ anz 1) anz) *stackz*)
                                 (c-unlist rest-p anz (min opt anz))
                      ))       )
                  (seclass-or-f seclass anode1)
                  (push anode1 codelist)
                  (seclass-or-f seclass anode2)
                  (push anode2 codelist)
                )
                ; n > reqopt, impliziert rest-p.
                ; Übergabe von restlichen Argumenten an eine compilierte Closure:
                ; als Liste.
                ; Liste aus allen weiteren Argumenten:
                (progn
                  (let ((*stackz* *stackz*)
                        (rest-args args))
                    (loop
                      (when (null rest-args) (return))
                      (let ((anode (c-form (pop rest-args) 'ONE)))
                        (seclass-or-f seclass anode)
                        (push anode codelist)
                      )
                      (push '(PUSH) codelist)
                      (push 1 *stackz*)
                    )
                    (let ((anode (c-form (first applyargs) 'ONE)))
                      (seclass-or-f seclass anode)
                      (push anode codelist)
                    )
                    (push `(LIST* ,(- n reqopt)) codelist)
                  )
                  (push '(PUSH) codelist)
                  (push 1 *stackz*)
            ) ) )
            (progn
              ; fehlende optionale Parameter werden mit #<UNBOUND> initialisiert:
              (when (> reqopt n)
                (let ((anz (- reqopt n)))
                  (push `(PUSH-UNBOUND ,anz) codelist)
                  (push anz *stackz*)
              ) )
              ; &rest-Parameter:
              (when rest-p
                (if subr-flag
                  ; Übergabe von restlichen Argumenten an ein SUBR: einzeln
                  (loop
                    (when (null args) (return))
                    (let ((anode (c-form (pop args) 'ONE)))
                      (seclass-or-f seclass anode)
                      (push anode codelist)
                    )
                    (push '(PUSH) codelist)
                    (push 1 *stackz*)
                  )
                  ; Übergabe von restlichen Argumenten an eine compilierte Closure:
                  ; als Liste
                  (if (null args)
                    ; leere Liste
                    (progn
                      (push '(NIL) codelist)
                      (push '(PUSH) codelist)
                      (push 1 *stackz*)
                    )
                    ; Liste aus allen weiteren Argumenten:
                    (progn
                      (let ((*stackz* *stackz*)
                            (rest-args args))
                        (loop
                          (when (null rest-args) (return))
                          (let ((anode (c-form (pop rest-args) 'ONE)))
                            (seclass-or-f seclass anode)
                            (push anode codelist)
                          )
                          (push '(PUSH) codelist)
                          (push 1 *stackz*)
                        )
                        (push `(LIST ,(- n reqopt)) codelist)
                      )
                      (push '(PUSH) codelist)
                      (push 1 *stackz*)
            ) ) ) ) )
          )
          ; &key-Parameter:
          (when key-p
            ; Nur dann gleichzeitig rest-p und key-p, wenn n <= reqopt, da
            ; test-argument-syntax (ergab STATIC-KEYS) den anderen Fall
            ; bereits ausgeschlossen hat.
            (let ((keyanz (length keylist)))
              ; Erst alle Keys mit #<UNBOUND> vorbelegen, dann die Argumente
              ; in der angegebenen Reihenfolge auswerten und zuordnen?
              ; Das ist uns zu einfach. Wir lassen die Argumente kommutieren,
              ; damit möglichst viele der (STORE ...) durch (PUSH) ersetzt
              ; werden können: Die Argumente zu den ersten Keys werden nach
              ; Möglichkeit zuerst ausgewertet, die zu den letzten Keys
              ; zuletzt. Wir lassen es allerdings bei einem einzigen
              ; (PUSH-UNBOUND ...).
              (let* ((key-positions ; Liste von Tripeln (key stack-depth free-p),
                                    ; wobei stack-depth = keyanz-1...0 läuft und
                                    ; free-p angibt, ob der Slot schon gefüllt ist.
                       (let ((i keyanz))
                         (mapcar #'(lambda (key) (list key (decf i) t)) keylist)
                     ) )
                     (anodes ; Liste von Quadrupeln
                             ; (needed key-position anode stackz), wobei
                             ; key-position die stack-depth des Keyword-Slots
                             ; oder NIL ist, anode der Anode zu diesem Argument.
                             ; Die Liste wird in derselben Reihenfolge gehalten,
                             ; wie sie die Argumentliste vorgibt.
                             ; Ausnahme: needed = NIL bei anodes, deren
                             ; Berechnung man vorgezogen oder verschoben hat.
                       (let ((L '()))
                         (loop
                           (when (null args) (return))
                           (let* ((key (c-constant-value (pop args)))
                                  (tripel (assoc key key-positions :test #'eq)) ; kann =NIL sein!
                                  (for-value (third tripel))
                                  (arg (pop args)))
                             ; for-value /= NIL: Existentes Keyword, und der Slot ist noch leer
                             ; for-value = NIL: ALLOW-erlaubtes Keyword oder Slot schon gefüllt
                             (let* ((*stackz* (cons 0 *stackz*)) ; 0 wird später ersetzt
                                    (anode (c-form arg (if for-value 'ONE 'NIL))))
                               (seclass-or-f seclass anode)
                               (push (list t (second tripel) anode *stackz*) L)
                             )
                             (setf (third tripel) nil)
                         ) )
                         (nreverse L)
                    )) )
                (let ((depth1 0)
                      (depth2 0)
                      (codelist-from-end '()))
                  ; Möglichst viel nach vorne ziehen:
                  (do ((anodesr anodes (cdr anodesr)))
                      ((null anodesr))
                    (let ((anodeetc (car anodesr))) ; nächstes Quadrupel
                      (when (first anodeetc) ; noch was zu tun?
                        (if (and
                              (or ; kein Keyword, d.h. kein (STORE ...) nötig?
                                  (null (second anodeetc))
                                  ; oberstes Keyword?
                                  (= (second anodeetc) (- keyanz depth1 1))
                              )
                              ; kommutiert anodeetc mit allen vorigen anodes?
                              (let ((anode (third anodeetc)))
                                (do ((anodesr2 anodes (cdr anodesr2)))
                                    ((eq anodesr2 anodesr) t)
                                  (unless (anodes-commute anode (third (car anodesr2)))
                                    (return nil)
                              ) ) )
                            )
                          ; vorziehen:
                          (progn
                            (setf (first (fourth anodeetc)) depth1) ; korrekte Stacktiefe
                            (push (third anodeetc) codelist) ; in die Codeliste
                            (when (second anodeetc)
                              (push '(PUSH) codelist)
                              (incf depth1)
                            )
                            (setf (first anodeetc) nil) ; diesen brauchen wir nicht mehr
                          )
                          ; sonst machen wir nichts.
                  ) ) ) )
                  ; Möglichst viel nach hinten ziehen:
                  (setq anodes (nreverse anodes))
                  (do ((anodesr anodes (cdr anodesr)))
                      ((null anodesr))
                    (let ((anodeetc (car anodesr))) ; nächstes Quadrupel
                      (when (first anodeetc) ; noch was zu tun?
                        (if (and
                              (or ; kein Keyword, d.h. kein (STORE ...) nötig?
                                  (null (second anodeetc))
                                  ; unterstes Keyword?
                                  (= (second anodeetc) depth2)
                              )
                              ; kommutiert anodeetc mit allen späteren anodes?
                              (let ((anode (third anodeetc)))
                                (do ((anodesr2 anodes (cdr anodesr2)))
                                    ((eq anodesr2 anodesr) t)
                                  (unless (anodes-commute anode (third (car anodesr2)))
                                    (return nil)
                              ) ) )
                            )
                          ; ans Ende verschieben:
                          (progn
                            (when (second anodeetc)
                              (push '(PUSH) codelist-from-end)
                              (incf depth2)
                            )
                            (setf (first (fourth anodeetc)) (- keyanz depth2)) ; korrekte Stacktiefe
                            (push (third anodeetc) codelist-from-end) ; in die Codeliste
                            (setf (first anodeetc) nil) ; diesen brauchen wir nicht mehr
                          )
                          ; sonst machen wir nichts.
                  ) ) ) )
                  (setq anodes (nreverse anodes))
                  (let ((depth-now (- keyanz depth2))) ; codelist-from-end erniedrigt den Stack um depth2
                    (when (> depth-now depth1)
                      (push `(PUSH-UNBOUND ,(- depth-now depth1)) codelist)
                    )
                    ; In codelist herrscht jetzt Stacktiefe depth-now.
                    (dolist (anodeetc anodes)
                      (when (first anodeetc)
                        (setf (first (fourth anodeetc)) depth-now) ; korrekte Stacktiefe
                        (push (third anodeetc) codelist)
                        (when (second anodeetc)
                          (push `(STORE ,(- (second anodeetc) depth2)) codelist)
                  ) ) ) )
                  ; Nun codelist-from-end:
                  (setq codelist (nreconc codelist-from-end codelist))
              ) )
              ; Jetzt sind alle Key-Argumente auf dem Stack.
              (push keyanz *stackz*)
          ) )
          (setq codelist (nreconc codelist (funcall call-code-producer)))
        )
        ; Constant-Folding: Ist fun foldable (also subr-flag = T und
        ; key-flag = NIL) und besteht codelist außer den (PUSH)s und dem
        ; Call-Code am Schluss nur aus Anodes mit code = ((CONST ...)) ?
        (when (and foldable
                   (every #'(lambda (code)
                              (or (not (anode-p code)) (anode-constantp code))
                            )
                          codelist
              )    )
          ; Funktion aufzurufen versuchen:
          (let ((args (let ((L '())) ; Liste der (konstanten) Argumente
                        (dolist (code codelist)
                          (when (anode-p code)
                            (push (anode-constant-value code) L)
                        ) )
                        (nreverse L)
                )     )
                resulting-values)
            (when (block try-eval
                    (setq resulting-values
                      (let ((*error-handler*
                              #'(lambda (&rest error-args)
                                  (declare (ignore error-args))
                                  (return-from try-eval nil)
                           ))   )
                        (multiple-value-list (apply fun args))
                    ) )
                    t
                  )
              ; Funktion erfolgreich aufgerufen, Constant-Folding durchführen:
              (return-from c-DIRECT-FUNCTION-CALL
                (c-GLOBAL-FUNCTION-CALL-form
                  `(VALUES ,@(mapcar #'(lambda (x) `(QUOTE ,x)) resulting-values))
        ) ) ) ) )
        (make-anode
          :type `(DIRECT-CALL ,fun)
          :sub-anodes (remove-if-not #'anode-p codelist)
          :seclass seclass
          :code codelist
        )
) ) ) )
(defun c-unlist (rest-p n m)
  (if rest-p
    (if (eql n 0)
      (make-anode :type 'UNLIST*
                  :sub-anodes '()
                  :seclass '(NIL . NIL)
                  :code '((PUSH))
      )
      (make-anode :type 'UNLIST*
                  :sub-anodes '()
                  :seclass '(T . T) ; kann Error melden
                  :code `((UNLIST* ,n ,m))
    ) )
    (make-anode :type 'UNLIST
                :sub-anodes '()
                :seclass '(T . T) ; kann Error melden
                :code `((UNLIST ,n ,m))
) ) )
(defun cclosure-call-code-producer (fun fnode req opt rest-flag key-flag keylist)
  (if (eq fnode *func*)
    ; rekursiver Aufruf der eigenen Funktion
    (let ((call-code
            `((JSR ,(+ req opt (if rest-flag 1 0) (length keylist)) ; Zahl der Stack-Einträge
                   ,*func-start-label*
             ))
         ))
      #'(lambda () call-code)
    )
    ; eine andere Cclosure aufrufen
    #'(lambda ()
        (list
          (c-form `(FUNCTION ,fun) 'ONE)
          (if key-flag '(CALLCKEY) '(CALLC))
      ) )
) )

; Global function call: (fun {form}*)
(defun c-GLOBAL-FUNCTION-CALL-form (*form*)
  (c-GLOBAL-FUNCTION-CALL (first *form*))
)
(defun c-GLOBAL-FUNCTION-CALL (fun) ; fun ist ein Symbol oder (SETF symbol)
  (test-list *form* 1)
  (when *compiling-from-file* ; von COMPILE-FILE aufgerufen?
    (note-function-used fun)
    ; PROCLAIM-Deklarationen zur Kenntnis nehmen:
    (when (and (eq fun 'PROCLAIM) (= (length *form*) 2))
      (let ((h (second *form*)))
        (when (c-constantp h)
          (c-form
            `(EVAL-WHEN (COMPILE) (c-PROCLAIM ',(c-constant-value h)))
    ) ) ) )
    ; Modul-Anforderungen zur Kenntnis nehmen:
    (when (and (memq fun '(PROVIDE REQUIRE))
               (every #'c-constantp (rest *form*))
          )
      (c-form
        `(EVAL-WHEN (COMPILE)
           (,(case fun
               (PROVIDE 'c-PROVIDE) ; c-PROVIDE statt PROVIDE
               (REQUIRE 'c-REQUIRE) ; c-REQUIRE statt REQUIRE
             )
            ,@(mapcar
                #'(lambda (x) (list 'QUOTE (c-constant-value x))) ; Argumente quotieren
                (rest *form*)
         ) )  )
    ) )
    ; Package-Anforderungen zur Kenntnis nehmen:
    (when (and (memq fun '(MAKE-PACKAGE SYSTEM::%IN-PACKAGE IN-PACKAGE
                           SHADOW SHADOWING-IMPORT EXPORT UNEXPORT
                           USE-PACKAGE UNUSE-PACKAGE IMPORT
               )          )
               (every #'c-constantp (rest *form*))
          )
      (push
        `(,fun
          ,@(mapcar
              #'(lambda (x) (list 'QUOTE (c-constant-value x))) ; Argumente quotieren
              (rest *form*)
         )  )
        *package-tasks*
  ) ) )
  (let* ((args (cdr *form*)) ; Argumente
         (n (length args))) ; Anzahl der Argumente
    (if (not (declared-notinline fun)) ; darf fun INLINE genommen werden?
      (multiple-value-bind (name req opt rest-p keylist allow-p) (subr-info fun)
        ; Ist fun ein SUBR, so sollte name = fun sein, und das SUBR hat die
        ; Spezifikation req, opt, rest-p, key-p = (not (null keylist)), allow-p.
        ; Sonst ist name = NIL.
        (if (and name (eq fun name)) ; beschreibt fun ein gültiges SUBR?
          (case fun
            ((CAR CDR FIRST REST NOT NULL CONS SVREF VALUES
              CAAR CADR CDAR CDDR CAAAR CAADR CADAR CADDR CDAAR CDADR
              CDDAR CDDDR SECOND THIRD FOURTH CAAAAR CAAADR CAADAR CAADDR
              CADAAR CADADR CADDAR CADDDR CDAAAR CDAADR CDADAR CDADDR
              CDDAAR CDDADR CDDDAR CDDDDR ATOM CONSP
              VALUES-LIST SYS::%SVSTORE EQ SYMBOL-FUNCTION LIST LIST*
             )
             ; Diese hier haben keylist=NIL, allow-p=NIL und
             ; (was aber nicht verwendet wird) opt=0.
             (if (and (<= req n) (or rest-p (<= n (+ req opt))))
               ; Wir machen den Aufruf INLINE.
               (let ((sideeffects ; Seiteneffektklasse der Funktionsausführung
                       (case fun
                         ((NOT NULL CONS VALUES ATOM CONSP EQ LIST LIST*)
                           '(NIL . NIL)
                         )
                         ((CAR CDR FIRST REST CAAR CADR
                           CDAR CDDR CAAAR CAADR CADAR CADDR CDAAR CDADR CDDAR
                           CDDDR SECOND THIRD FOURTH CAAAAR CAAADR CAADAR CAADDR
                           CADAAR CADADR CADDAR CADDDR CDAAAR CDAADR CDADAR CDADDR
                           CDDAAR CDDADR CDDDAR CDDDDR VALUES-LIST
                           SVREF SYMBOL-FUNCTION
                          )
                           '(T . NIL)
                         )
                         (t '(T . T))
                    )) )
                 (if (and (null *for-value*) (null (cdr sideeffects)))
                   ; Brauche die Funktion nicht aufzurufen, nur die Argumente auswerten
                   (c-form `(PROGN ,@args))
                   (if (and (eq fun 'VALUES) (eq *for-value* 'ONE))
                     (if (= n 0) (c-NIL) (c-form `(PROG1 ,@args)))
                     (let ((seclass sideeffects)
                           (codelist '()))
                       (let ((*stackz* *stackz*))
                         ; Argumente auswerten und bis auf das letzte auf den Stack
                         ; (denn das letzte Argument wird in A0 erwartet):
                         (loop
                           (when (null args) (return))
                           (let ((anode (c-form (pop args) 'ONE)))
                             (seclass-or-f seclass anode)
                             (push anode codelist)
                           )
                           (when args ; nicht am Schluss
                             (push '(PUSH) codelist)
                             (push 1 *stackz*)
                         ) )
                         (setq codelist
                           (nreconc codelist
                             (case fun
                               ((CAR FIRST) '((CAR)))
                               ((CDR REST) '((CDR)))
                               (CAAR '((CAR) (CAR)))
                               ((CADR SECOND) '((CDR) (CAR)))
                               (CDAR '((CAR) (CDR)))
                               (CDDR '((CDR) (CDR)))
                               (CAAAR '((CAR) (CAR) (CAR)))
                               (CAADR '((CDR) (CAR) (CAR)))
                               (CADAR '((CAR) (CDR) (CAR)))
                               ((CADDR THIRD) '((CDR) (CDR) (CAR)))
                               (CDAAR '((CAR) (CAR) (CDR)))
                               (CDADR '((CDR) (CAR) (CDR)))
                               (CDDAR '((CAR) (CDR) (CDR)))
                               (CDDDR '((CDR) (CDR) (CDR)))
                               (CAAAAR '((CAR) (CAR) (CAR) (CAR)))
                               (CAAADR '((CDR) (CAR) (CAR) (CAR)))
                               (CAADAR '((CAR) (CDR) (CAR) (CAR)))
                               (CAADDR '((CDR) (CDR) (CAR) (CAR)))
                               (CADAAR '((CAR) (CAR) (CDR) (CAR)))
                               (CADADR '((CDR) (CAR) (CDR) (CAR)))
                               (CADDAR '((CAR) (CDR) (CDR) (CAR)))
                               ((CADDDR FOURTH) '((CDR) (CDR) (CDR) (CAR)))
                               (CDAAAR '((CAR) (CAR) (CAR) (CDR)))
                               (CDAADR '((CDR) (CAR) (CAR) (CDR)))
                               (CDADAR '((CAR) (CDR) (CAR) (CDR)))
                               (CDADDR '((CDR) (CDR) (CAR) (CDR)))
                               (CDDAAR '((CAR) (CAR) (CDR) (CDR)))
                               (CDDADR '((CDR) (CAR) (CDR) (CDR)))
                               (CDDDAR '((CAR) (CDR) (CDR) (CDR)))
                               (CDDDDR '((CDR) (CDR) (CDR) (CDR)))
                               (ATOM '((ATOM)))
                               (CONSP '((CONSP)))
                               ((NOT NULL) '((NOT)))
                               (CONS '((CONS)))
                               (SVREF '((SVREF)))
                               (SYS::%SVSTORE '((SVSET)))
                               (EQ '((EQ)))
                               (VALUES (case n
                                         (0 '((VALUES0)) )
                                         (1 '((VALUES1)) )
                                         (t `((PUSH) ; letztes Argument auch noch in den Stack
                                              (STACK-TO-MV ,n)
                                             )
                               )       ) )
                               (VALUES-LIST '((LIST-TO-MV)))
                               (SYMBOL-FUNCTION '((SYMBOL-FUNCTION)))
                               (LIST (if (plusp n)
                                       `((PUSH) (LIST ,n))
                                       '((NIL))
                               )     )
                               (LIST* (case n
                                        (1 '((VALUES1)) )
                                        (t `((LIST* ,(1- n))) )
                               )      )
                               (t (compiler-error 'c-GLOBAL-FUNCTION-CALL))
                       ) ) ) )
                       (make-anode
                         :type `(PRIMOP ,fun)
                         :sub-anodes (remove-if-not #'anode-p codelist)
                         :seclass seclass
                         :code codelist
                       )
               ) ) ) )
               ; falsche Argumentezahl -> doch nicht INLINE:
               (progn
                 (c-warn (ENGLISH "~S called with ~S arguments, but it requires ~
                                   ~:[~:[from ~S to ~S~;~S~]~;at least ~*~S~] arguments.")
                         fun n
                         rest-p  (eql opt 0) req (+ req opt)
                 )
                 (c-NORMAL-FUNCTION-CALL fun)
            )) )
            (t ; Ist das SUBR fun in der FUNTAB enthalten?
             (let ((index (gethash fun function-codes)))
               (if index
                 (case (test-argument-syntax args nil
                                    fun req opt rest-p keylist keylist allow-p
                       )
                   ((NO-KEYS STATIC-KEYS)
                    ; korrekte Syntax, Stack-Layout zur Compilezeit vorhersehbar
                    ; -> INLINE
                    (c-DIRECT-FUNCTION-CALL
                      args nil fun req opt rest-p keylist keylist
                      t ; es handelt sich um ein SUBR
                      (let ((call-code
                              ; Aufruf mit Hilfe der FUNTAB:
                              (cons
                                (if (not rest-p)
                                  (CALLS-code index)
                                  `(CALLSR ,(max 0 (- n req opt)) ; Bei n<req+opt kommt noch ein (PUSH-UNBOUND ...)
                                           ,(- index funtabR-index)
                                   )
                                )
                                (case fun
                                  (; Funktionen, die nicht zurückkehren:
                                   (; control.d:
                                    SYS::DRIVER SYS::UNWIND-TO-DRIVER
                                    ; debug.d:
                                    ; SYS::REDO-EVAL-FRAME SYS::RETURN-FROM-EVAL-FRAME
                                    ; error.d:
                                    ERROR SYSTEM::ERROR-OF-TYPE INVOKE-DEBUGGER
                                   )
                                   '((BARRIER))
                                  )
                                  (t '())
                           )) ) )
                        #'(lambda () call-code)
                   )) )
                   (t (c-NORMAL-FUNCTION-CALL fun))
                 )
                 (c-NORMAL-FUNCTION-CALL fun)
          ) )) )
          (let ((inline-lambdabody
                  (or (and *compiling-from-file*
                           (cdr (assoc fun *inline-definitions* :test #'equal))
                      )
                      (get (get-funname-symbol fun) 'sys::inline-expansion)
               )) )
            (if (and #| inline-lambdabody |#
                     (consp inline-lambdabody)
                     (inline-callable-function-lambda-p `(FUNCTION (LAMBDA ,@inline-lambdabody)) n)
                )
              ; Aufruf einer globalen Funktion INLINE möglich
              (c-FUNCALL-INLINE fun args nil inline-lambdabody nil)
              (c-NORMAL-FUNCTION-CALL fun)
      ) ) ) )
      (c-NORMAL-FUNCTION-CALL fun)
) ) )

(defvar *deprecated-functions-list*
  '(GENTEMP SET SPECIAL-FORM-P RESOLVE-HOST-IPADDR FILE-STAT USER-DATA))

; Hilfsfunktion: Notiere, dass eine globale Funktionsdefinition benutzt wird.
(defun note-function-used (name)
  (unless (or (fboundp name) (member name *known-functions* :test #'equal))
    (pushnew name *unknown-functions* :test #'equal)
  )
  (when (memq name *deprecated-functions-list*)
    (pushnew name *deprecated-functions* :test #'eq)
) )

; Hilfsfunktion: PROCLAIM beim Compilieren vom File, vgl. Funktion PROCLAIM
(defun c-PROCLAIM (declspec)
  (when (consp declspec)
    (case (car declspec)
      (SPECIAL
        (dolist (var (cdr declspec))
          (when (symbolp var) (pushnew var *known-special-vars* :test #'eq))
      ) )
      (INLINE
        (dolist (var (cdr declspec))
          (when (function-name-p var)
            (pushnew var *inline-functions* :test #'equal)
            (setq *notinline-functions* (delete var *notinline-functions* :test #'equal))
      ) ) )
      (NOTINLINE
        (dolist (var (cdr declspec))
          (when (function-name-p var)
            (pushnew var *notinline-functions* :test #'equal)
            (setq *inline-functions* (delete var *inline-functions* :test #'equal))
      ) ) )
      (CONSTANT-INLINE
        (dolist (var (cdr declspec))
          (when (symbolp var)
            (pushnew var *inline-constants*)
            (setq *notinline-constants* (delete var *notinline-constants*))
      ) ) )
      (CONSTANT-NOTINLINE
        (dolist (var (cdr declspec))
          (when (symbolp var)
            (pushnew var *notinline-constants*)
            (setq *inline-constants* (delete var *inline-constants*))
      ) ) )
      (DECLARATION
        (dolist (var (cdr declspec))
          (when (symbolp var) (pushnew var *user-declaration-types* :test #'eq))
      ) )
) ) )

; Hilfsfunktion: DEFCONSTANT beim Compilieren
(defun c-PROCLAIM-CONSTANT (symbol initial-value-form)
  (when *compiling-from-file*
    (pushnew symbol *known-special-vars* :test #'eq)
    (when (c-constantp initial-value-form)
      (push (cons symbol (c-constant-value initial-value-form))
            *constant-special-vars*
) ) ) )

; Hilfsfunktion: DEFUN beim Compilieren
(defun c-DEFUN (symbol &optional lambdabody)
  (when *compiling* ; c-DEFUN kann auch vom Expander aus aufgerufen werden!
    (when *compiling-from-file*
      (pushnew symbol *known-functions* :test #'equal)
      (when lambdabody ; Lambdabody angegeben ->
        ; Funktionsdefinition erfolgt im Top-Level-Environment und ist inlinebar.
        (push (cons symbol lambdabody) *inline-definitions*)
) ) ) )

; Hilfsfunktion: PROVIDE beim Compilieren vom File, vgl. Funktion PROVIDE
(defun c-PROVIDE (module-name)
  (pushnew (string module-name) *compiled-modules* :test #'string=)
)

; Hilfsfunktion: REQUIRE beim Compilieren vom File, vgl. Funktion REQUIRE
(defun c-REQUIRE (module-name &optional (pathname nil p-given))
  (unless (member (string module-name) *compiled-modules* :test #'string-equal)
    (unless p-given (setq pathname (pathname module-name)))
    (flet ((load-lib (file)
             (let* ((present-files
                      (search-file file (append *source-file-types* '(#".lib")))
                    )
                    (newest-file (first present-files)))
               ; Falls das libfile unter den gefundenen Files vorkommt
               ; und das neueste ist:
               (if (and (consp present-files)
                        (string= (pathname-type newest-file)
                                 '#,(pathname-type '#".lib")
                   )    )
                 (load newest-file :verbose nil :print nil :echo nil) ; libfile laden
                 (compile-file (or newest-file file)) ; file compilieren
          )) ) )
      (if (atom pathname) (load-lib pathname) (mapcar #'load-lib pathname))
) ) )

;;; Hilfsfunktionen für
;;; LET/LET*/MULTIPLE-VALUE-BIND/Lambda-Ausdruck/FLET/LABELS:

;; Syntaxanalyse:

; analysiert eine Parameterliste von LET/LET*, liefert:
; die Liste der Symbole,
; die Liste der Formen.
(defun analyze-letlist (parameters)
  (do ((L parameters (cdr L))
       (symbols nil)
       (forms nil))
      ((null L) (values (nreverse symbols) (nreverse forms)))
    (cond ((symbolp (car L)) (push (car L) symbols) (push nil forms))
          ((and (consp (car L)) (symbolp (caar L))
                (or (null (cdar L))
                    (and (consp (cdar L)) (null (cddar L)))
           )    )
           (push (caar L) symbols) (push (cadar L) forms)
          )
          (t (catch 'c-error
               (c-error (ENGLISH "Illegal syntax in LET/LET*: ~S")
                        (car L)
    )     )  ) )
) )

; analysiert eine Lambdaliste einer Funktion (CLTL S. 60), liefert 13 Werte:
; 1. Liste der required Parameter
; 2. Liste der optionalen Parameter
; 3. Liste der Initformen der optionalen Parameter
; 4. Liste der Svars zu den optionalen Parametern (0 für die fehlenden)
; 5. Rest-Parameter oder 0
; 6. Flag, ob Keywords erlaubt sind
; 7. Liste der Keywords
; 8. Liste der Keyword-Parameter
; 9. Liste der Initformen der Keyword-Parameter
; 10. Liste der Svars zu den Keyword-Parametern (0 für die fehlenden)
; 11. Flag, ob andere Keywords erlaubt sind
; 12. Liste der Aux-Variablen
; 13. Liste der Initformen der Aux-Variablen
(defun analyze-lambdalist (lambdalist)
  (let ((L lambdalist) ; Rest der Lambdaliste
        (req nil)
        (optvar nil)
        (optinit nil)
        (optsvar nil)
        (rest 0)
        (keyflag nil)
        (keyword nil)
        (keyvar nil)
        (keyinit nil)
        (keysvar nil)
        (allow-other-keys nil)
        (auxvar nil)
        (auxinit nil))
       ; alle in umgedrehter Reihenfolge
    (macrolet ((err-illegal (item)
                 `(catch 'c-error
                    (c-error (ENGLISH "Lambda list marker ~S not allowed here.")
                             ,item
                  ) )
               )
               (err-norest ()
                 `(catch 'c-error
                    (c-error (ENGLISH "Missing &REST parameter in lambda list ~S")
                             lambdalist
                  ) )
               )
               (err-superflu (item)
                 `(catch 'c-error
                    (c-error (ENGLISH "Lambda list element ~S is superfluous.")
                             ,item
                  ) )
              ))
      ; Required Parameter:
      (loop
        (if (atom L) (return))
        (let ((item (car L)))
          (if (symbolp item)
            (if (memq item lambda-list-keywords)
              (if (memq item '(&optional &rest &key &aux))
                (return)
                (err-illegal item)
              )
              (push item req)
            )
            (lambdalist-error item)
        ) )
        (setq L (cdr L))
      )
      ; Hier gilt (or (atom L) (member (car L) '(&optional &rest &key &aux))).
      ; Optionale Parameter:
      (when (and (consp L) (eq (car L) '&optional))
        (setq L (cdr L))
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (memq item lambda-list-keywords)
                (if (memq item '(&rest &key &aux))
                  (return)
                  (err-illegal item)
                )
                (progn (push item optvar) (push nil optinit) (push 0 optsvar))
              )
              (if (and (consp item) (symbolp (car item)))
                (if (null (cdr item))
                  (progn (push (car item) optvar) (push nil optinit) (push 0 optsvar))
                  (if (consp (cdr item))
                    (if (null (cddr item))
                      (progn (push (car item) optvar) (push (cadr item) optinit) (push 0 optsvar))
                      (if (and (consp (cddr item)) (symbolp (caddr item)) (null (cdddr item)))
                        (progn (push (car item) optvar) (push (cadr item) optinit) (push (caddr item) optsvar))
                        (lambdalist-error item)
                    ) )
                    (lambdalist-error item)
                ) )
                (lambdalist-error item)
          ) ) )
          (setq L (cdr L))
      ) )
      ; Hier gilt (or (atom L) (member (car L) '(&rest &key &aux))).
      ; Rest-Parameter:
      (when (and (consp L) (eq (car L) '&rest))
        (setq L (cdr L))
        (if (atom L)
          (err-norest)
          (prog ((item (car L)))
            (if (symbolp item)
              (if (memq item lambda-list-keywords)
                (progn (err-norest) (return))
                (setq rest item)
              )
              (lambdalist-error item)
            )
            (setq L (cdr L))
      ) ) )
      ; Vorrücken bis zum nächsten &key oder &aux :
      (loop
        (when (atom L) (return))
        (let ((item (car L)))
          (if (memq item lambda-list-keywords)
            (if (memq item '(&key &aux))
              (return)
              (err-illegal item)
            )
            (err-superflu item)
        ) )
        (setq L (cdr L))
      )
      ; Hier gilt (or (atom L) (member (car L) '(&key &aux))).
      ; Keyword-Parameter:
      (when (and (consp L) (eq (car L) '&key))
        (setq L (cdr L))
        (setq keyflag t)
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (memq item lambda-list-keywords)
                (if (memq item '(&allow-other-keys &aux))
                  (return)
                  (err-illegal item)
                )
                (progn
                  (push (intern (symbol-name item) *keyword-package*) keyword)
                  (push item keyvar) (push nil keyinit) (push 0 keysvar)
              ) )
              (if (and
                    (consp item)
                    (or
                      (symbolp (car item))
                      (and (consp (car item))
                           (symbolp (caar item))
                           (consp (cdar item))
                           (symbolp (cadar item))
                           (null (cddar item))
                    ) )
                    (or (null (cdr item))
                        (and (consp (cdr item))
                             (or (null (cddr item))
                                 (and (consp (cddr item)) (symbolp (caddr item)) (null (cdddr item)))
                  ) )   )    )
                (progn
                  (if (consp (car item))
                    (progn (push (caar item) keyword) (push (cadar item) keyvar))
                    (progn (push (intern (symbol-name (car item)) *keyword-package*) keyword) (push (car item) keyvar))
                  )
                  (if (consp (cdr item))
                    (progn
                      (push (cadr item) keyinit)
                      (if (consp (cddr item))
                        (push (caddr item) keysvar)
                        (push 0 keysvar)
                    ) )
                    (progn (push nil keyinit) (push 0 keysvar))
                ) )
                (lambdalist-error item)
          ) ) )
          (setq L (cdr L))
        )
        ; Hier gilt (or (atom L) (member (car L) '(&allow-other-keys &aux))).
        (when (and (consp L) (eq (car L) '&allow-other-keys))
          (setq allow-other-keys t)
          (setq L (cdr L))
      ) )
      ; Vorrücken bis zum nächsten &AUX :
      (loop
        (when (atom L) (return))
        (let ((item (car L)))
          (if (memq item lambda-list-keywords)
            (if (memq item '(&aux))
              (return)
              (err-illegal item)
            )
            (err-superflu item)
        ) )
        (setq L (cdr L))
      )
      ; Hier gilt (or (atom L) (member (car L) '(&aux))).
      ; &AUX-Variablen:
      (when (and (consp L) (eq (car L) '&aux))
        (setq L (cdr L))
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (memq item lambda-list-keywords)
                (err-illegal item)
                (progn (push item auxvar) (push nil auxinit))
              )
              (if (and (consp item) (symbolp (car item)))
                (if (null (cdr item))
                  (progn (push (car item) auxvar) (push nil auxinit))
                  (if (and (consp (cdr item)) (null (cddr item)))
                    (progn (push (car item) auxvar) (push (cadr item) auxinit))
                    (lambdalist-error item)
                ) )
                (lambdalist-error item)
          ) ) )
          (setq L (cdr L))
      ) )
      ; Hier gilt (atom L).
      (if L
        (catch 'c-error
          (c-error (ENGLISH "Lambda lists with dots are only allowed in macros, not here: ~S")
                   lambdalist
      ) ) )
    )
    (values
      (nreverse req)
      (nreverse optvar) (nreverse optinit) (nreverse optsvar)
      rest
      keyflag
      (nreverse keyword) (nreverse keyvar) (nreverse keyinit) (nreverse keysvar)
      allow-other-keys
      (nreverse auxvar) (nreverse auxinit)
) ) )

(defun lambdalist-error (item)
  (catch 'c-error
    (c-error (ENGLISH "Illegal lambda list element ~S")
             item
) ) )

; (inline-callable-function-lambda-p form n) bzw.
; (inline-callable-function-p form n) stellt fest, ob form eine Form ist, die
; eine Funktion liefert, die mit n (und evtl. mehr) Argumenten Inline
; aufgerufen werden kann. (vorbehaltlich Syntax-Errors in der Lambdaliste)
; form sollte bereits macroexpandiert sein.
(defun inline-callable-function-lambda-p (form n &optional (more nil))
  ; muss von der Bauart (FUNCTION funname) sein
  (and (consp form) (eq (first form) 'FUNCTION)
       (consp (cdr form)) (null (cddr form))
       (let ((funname (second form)))
         ; funname muss von der Bauart (LAMBDA lambdalist ...) sein
         (and (consp funname) (eq (first funname) 'LAMBDA) (consp (cdr funname))
              (let ((lambdalist (second funname)))
                ; lambdalist muss eine Liste sein, die kein &KEY enthält
                ; (Funktionen mit &KEY werden nicht INLINE-expandiert, weil die
                ; Zuordnung von den Argumenten zu den Variablen nur dynamisch,
                ; mit GETF, möglich ist, und das kann die in Assembler
                ; geschriebene APPLY-Routine schneller.)
                (and (listp lambdalist)
                     (not (position '&KEY lambdalist))
                     (not (position '&ALLOW-OTHER-KEYS lambdalist))
                     (let ((&opt-pos (position '&OPTIONAL lambdalist))
                           (&rest-pos (position '&REST lambdalist))
                           (&aux-pos (or (position '&AUX lambdalist)
                                         (length lambdalist)
                          ))         )
                       (if &rest-pos
                         ; &rest angegeben
                         (or more (>= n (or &opt-pos &rest-pos)))
                         ; &rest nicht angegeben
                         (if more
                           (<= n (if &opt-pos (- &aux-pos 1) &aux-pos))
                           (if &opt-pos
                             (<= &opt-pos n (- &aux-pos 1))
                             (= n &aux-pos)
                     ) ) ) )
              ) )
       ) )
) )
(defun inline-callable-function-p (form n)
  (or (inline-callable-function-lambda-p form n)
      (and (consp form) (eq (first form) 'FUNCTION)
           (consp (cdr form)) (null (cddr form))
           (let ((fun (second form)))
             ; fun muss ein Funktionsname mit Inline-Definition sein,
             ; dann wird (FUNCALL form ...) später zu (fun ...)
             ; umgewandelt und inline compiliert werden.
             ; Siehe c-FUNCALL, c-FUNCTION-CALL, c-GLOBAL-FUNCTION-CALL.
             (and (function-name-p fun)
                  (null (fenv-search fun))
                  (not (and (symbolp fun) (or (special-operator-p fun) (macro-function fun))))
                  (not (declared-notinline fun))
                  (or #| ;; Lohnt sich wohl nicht
                      (and (equal fun (fnode-name *func*))
                           (member `(SYS::IN-DEFUN ,fun) *denv* :test #'equal)
                           (multiple-value-bind (req opt rest-flag key-flag keylist allow-flag)
                               (fdescr-signature (cons *func* nil))
                             (declare (ignore keylist allow-flag))
                             (and (<= req n) (or rest-flag (<= n (+ req opt))) (not key-flag))
                      )    )
                      |#
                      (let ((inline-lambdabody
                              (or (and *compiling-from-file*
                                       (cdr (assoc fun *inline-definitions* :test #'equal))
                                  )
                                  (get (get-funname-symbol fun) 'sys::inline-expansion)
                           )) )
                        (and #| inline-lambdabody |#
                             (consp inline-lambdabody)
                             (inline-callable-function-lambda-p `(FUNCTION (LAMBDA ,@inline-lambdabody)) n)
                       ) )
) )   )    ) )    )


;; Special-deklarierte Symbole:

(defvar *specials*) ; Liste aller zuletzt special deklarierten Symbole
(defvar *ignores*) ; Liste aller zuletzt ignore deklarierten Symbole
(defvar *ignorables*) ; Liste aller zuletzt ignorable deklarierten Symbole
(defvar *readonlys*) ; Liste aller zuletzt read-only deklarierten Symbole

; pusht alle Symbole von specials als Variablen auf *venv* :
(defun push-specials ()
  (apply #'push-*venv* (mapcar #'make-special-var *specials*))
)

; Überprüft eine Variable, ob sie zu Recht ignore-deklariert ist oder nicht...
(defun ignore-check (var)
  (let ((sym (var-name var)))
    (if (member sym *ignores* :test #'eq)
      ; var ignore-deklariert
      (if (var-specialp var)
        (c-warn (ENGLISH "Binding variable ~S can cause side effects despite of IGNORE declaration~%since it is declared SPECIAL.")
                sym
        )
        (if (var-for-value-usedp var)
          (c-style-warn (ENGLISH "variable ~S is used despite of IGNORE declaration.")
                        sym
      ) ) )
      ; var nicht ignore-deklariert
      (unless (member sym *ignorables* :test #'eq)
        ; var auch nicht ignorable-deklariert
        (unless (or (var-specialp var) (var-usedp var))
          ; var lexikalisch und unbenutzt
          (unless (null (symbol-package sym)) ; sym ein (gensym) ?
            ; (Symbole ohne Home-Package kommen nicht vom Benutzer, die Warnung
            ; würde nur verwirren).
            (c-style-warn (ENGLISH "variable ~S is not used.~%Misspelled or missing IGNORE declaration?")
                          sym
    ) ) ) ) )
    (when (member sym *readonlys* :test #'eq)
      (unless (var-specialp var)
        (when (var-assignedp var)
          (c-warn (ENGLISH "The variable ~S is assigned to, despite of READ-ONLY declaration.")
                  sym
    ) ) ) )
) )

; liefert den Code, der zum neuen Aufbau einer Closure und ihrer Unterbringung
; im Stack nötig ist:
; Dieser Code erweitert das von (cdr venvc) beschriebene Venv um closurevars,
; (cdr stackz) ist der aktuelle Stackzustand.
; Nach Aufbau der Closure sind venvc bzw. stackz die aktuellen Zustände.
(defun c-MAKE-CLOSURE (closurevars venvc stackz)
  (if closurevars
    `((VENV ,(cdr venvc) ,(cdr stackz))
      (MAKE-VECTOR1&PUSH ,(length closurevars))
     )
    '()
) )

;; Es gibt zwei Arten von Variablen-Bindungs-Vorgehensweisen:
; 1. fixed-var: die Variable hat eine Position im Stack, darf nicht wegoptimiert
;               werden. Ist die Variable dann doch in der Closure, so muss ihr
;               Wert dorthin übertragen werden; ist die Variable dynamisch, so
;               muss ein Bindungsframe aufgemacht werden.
;               Auftreten: MULTIPLE-VALUE-BIND, Lambda-Ausdruck (required,
;               optional, rest, keyword - Parameter)
; 2. movable-var: die Variable darf wegoptimiert werden, falls sie konstant ist
;                 (sie entweder dynamisch und konstant ist oder lexikalisch
;                  und an eine Konstante gebunden und nie geSETQed wird). Hier
;                 spielt also der Init-Wert eine Rolle.
;                 Auftreten: LET, LET*, Lambda-Ausdruck (optional-svar,
;                 keyword-svar, aux-Variablen)

;; 1. fixed-var

; Bindung einer fixed-var:
; symbol --> Variable
; Lässt *stackz* unverändert.
(defun bind-fixed-var-1 (symbol)
  (if (or (constantp symbol)
          (proclaimed-special-p symbol)
          (member symbol *specials* :test #'eq)
      )
    ; muss symbol dynamisch binden:
    (progn
      (when (l-constantp symbol)
        (catch 'c-error
          (c-error (ENGLISH "Constant ~S cannot be bound.")
                   symbol
      ) ) )
      (make-special-var symbol)
    )
    ; muss symbol lexikalisch binden:
    (make-var :name symbol :specialp nil :constantp nil
              :usedp nil :for-value-usedp nil :really-usedp nil
              :closurep nil
              :stackz *stackz* :venvc *venvc*
    )
) )

; registriert in *stackz*, dass eine fixed-var gebunden wird
(defun bind-fixed-var-2 (var)
  (when (and (var-specialp var) (not (var-constantp var)))
    (push '(BIND 1) *stackz*)
) )

; liefert den Code, der die Variable var an den Inhalt von stackdummyvar
; bindet. stackz ist der Stackzustand vor dem Binden dieser Variablen.
(defun c-bind-fixed-var (var stackdummyvar stackz)
  (if (var-specialp var)
    (if (var-constantp var)
      '() ; Konstante kann nicht gebunden werden
      `((GET ,stackdummyvar ,*venvc* ,stackz)
        (BIND ,(new-const (var-name var)))
       )
    )
    ; var lexikalisch, nach Definition nicht konstant
    (if (var-closurep var)
      `((GET ,stackdummyvar ,*venvc* ,stackz)
        (SET ,var ,*venvc* ,stackz)
       )
      '() ; var und stackdummyvar identisch
) ) )

; Kreiert je eine Stackvariable und eine Fixed-Variable zu jedem Symbol aus der
; Variablenliste symbols und liefert beide Listen als Werte.
(defun process-fixed-var-list (symbols &optional optimflags)
  (do ((symbolsr symbols (cdr symbolsr))
       (optimflagsr optimflags (cdr optimflagsr))
       (varlist nil) ; Liste der Variablen
       (stackvarlist nil)) ; Liste der Stackvariablen (teils Dummys)
      ((null symbolsr) (values (nreverse varlist) (nreverse stackvarlist)))
    (push 1 *stackz*)
    ; (mit constantp=nil und really-usedp=t, um eine Wegoptimierung zu vermeiden)
    (push (make-var :name (gensym) :specialp nil :constantp nil
                    :usedp nil :for-value-usedp nil :really-usedp (null (car optimflagsr))
                    :closurep nil :stackz *stackz* :venvc *venvc*
          )
          stackvarlist
    )
    (push (bind-fixed-var-1 (car symbolsr)) varlist)
) )

; Eliminiert alle Zuweisungen auf eine unbenutzte Variable.
(defun unmodify-unused-var (var)
  (dolist (modified (var-modified-list var))
    (if (cddr modified)
      ; Wert der Zuweisung wird gebraucht
      (let ((set-anode (second modified))) ; Anode der Zuweisung selbst
        (setf (anode-code set-anode) '((VALUES1))) ; Zuweisung entfernen
      )
      ; Wert der Zuweisung wird nicht gebraucht
      (progn
        (let ((value-anode (first modified))) ; Anode für zugewiesenen Wert
          (when (null (cdr (anode-seclass value-anode)))
            (setf (anode-code value-anode) '()) ; evtl. Wert-Form entfernen
        ) )
        (let ((set-anode (second modified))) ; Anode der Zuweisung selbst
          (setf (anode-code set-anode) '()) ; Zuweisung entfernen
) ) ) ) )

; Überprüft und optimiert die Variablen
; und liefert die Liste der Closure-Variablen (in der richtigen Reihenfolge).
(defun checking-fixed-var-list (varlist &optional optimflaglist)
  (let ((closurevarlist '()))
    (dolist (var varlist (nreverse closurevarlist))
      ; 1. Schritt: eventuelle Warnungen ausgeben
      (ignore-check var)
      ; 2. Schritt: Variablen-Ort (Stack oder Closure) endgültig bestimmen,
      ; evtl. optimieren
      (unless (var-specialp var)
        ; nur lexikalische Variablen können in der Closure liegen,
        ; nur bei lexikalischen Variablen kann optimiert werden
        (if (not (var-really-usedp var))
          ; Variable lexikalisch und unbenutzt
          (progn ; Variable eliminieren
            (setf (var-closurep var) nil)
            (when (car optimflaglist) ; optimierbare fixed-var?
              (setf (first (var-stackz var)) 0) ; aus dem Stack entfernen
              (setf (car optimflaglist) 'GONE) ; als gestrichen vermerken
            )
            (unmodify-unused-var var) ; Zuweisungen auf var eliminieren
          )
          (when (var-closurep var)
            ; Variable muss in der Closure liegen
            (push var closurevarlist)
      ) ) )
      (setq optimflaglist (cdr optimflaglist))
) ) )

;; 2. movable-var

; Beim Binden einer Variablen var an einen Anode anode:
; Wird eine lexikalische Variable an den Wert an einer lexikalischen Variablen
; gebunden? Wenn ja, an welche Variable?
(defun bound-to-var-p (var anode)
  (if (var-specialp var)
    nil
    ; var lexikalisch
    (loop
      (unless (eql (length (anode-code anode)) 1) (return nil))
      (setq anode (first (anode-code anode)))
      (unless (anode-p anode)
        (if (and (consp anode) (eq (first anode) 'GET))
          ; Code zum Anode besteht genau aus ((GET outervar ...)).
          (return (second anode))
          (return nil)
    ) ) )
) )

; Bindung einer movable-var:
; symbol form-anode --> Variable
; erweitert *stackz* um genau einen Eintrag
(defun bind-movable-var (symbol form-anode)
  (if (or (constantp symbol)
          (proclaimed-special-p symbol)
          (member symbol *specials* :test #'eq)
      )
    ; muss symbol dynamisch binden:
    (progn
      (if (l-constantp symbol)
        (progn
          (catch 'c-error
            (c-error (ENGLISH "Constant ~S cannot be bound.")
                     symbol
          ) )
          (push 0 *stackz*)
        )
        (push '(BIND 1) *stackz*)
      )
      (make-special-var symbol)
    )
    ; muss symbol lexikalisch binden:
    (let ((var
            (progn
              (push 1 *stackz*) ; vorläufig: 1 Platz auf dem Stack
              (make-var :name symbol :specialp nil
                :constantp (anode-constantp form-anode) ; wird bei Zuweisungen auf NIL gesetzt
                :constant (if (anode-constantp form-anode) (anode-constant form-anode))
                :usedp nil :for-value-usedp nil :really-usedp nil
                :closurep nil ; wird evtl. auf T gesetzt
                :stackz *stackz* :venvc *venvc*
         )) ) )
      (let ((outervar (bound-to-var-p var form-anode)))
        (when outervar ; Wird var an eine Variable outervar gebunden, so
                       ; darf später evtl. jede Referenz zu var in eine
                       ; Referenz zu outervar umgewandelt werden.
          (push (list var form-anode) (var-replaceable-list outervar))
      ) )
      var
) ) )

; liefert den Code, der die Variable var an A0 bindet:
(defun c-bind-movable-var (var)
  (if (var-specialp var)
    (if (var-constantp var)
      '() ; dynamische Konstanten können nicht gebunden werden
      `((BIND ,(new-const (var-name var))))
    )
    (if (var-closurep var)
      ; Closure-Variable schreiben:
      ; (var-stackz var) = (0 . ...) ist der aktuelle Stackzustand.
      `((SET ,var ,*venvc* ,(var-stackz var)))
      ; lexikalische Variable: wurde eventuell aus dem Stack eliminiert
      (if (zerop (first (var-stackz var)))
        '()
        `((PUSH)) ; im Stack: in die nächstuntere Stacklocation schreiben
) ) ) )

; liefert den Code, der die Variable var an das Ergebnis des ANODEs anode bindet
(defun c-bind-movable-var-anode (var anode)
  (let ((binding-anode
          (make-anode :type 'BIND-MOVABLE
                      :sub-anodes '()
                      :seclass '(NIL . NIL)
                      :code (c-bind-movable-var var)
       )) )
    (let ((outervar (bound-to-var-p var anode)))
      (when outervar ; Wird var an eine Variable outervar gebunden, so
                     ; darf später evtl. jede Referenz zu var in eine
                     ; Referenz zu outervar umgewandelt werden.
        (dolist (innervar-info (var-replaceable-list outervar))
          (when (eq (first innervar-info) var)
            (setf (cddr innervar-info) binding-anode) ; binding-anode nachtragen
    ) ) ) )
    (list anode binding-anode)
) )

; (process-movable-var-list symbols initforms *-flag) compiliert die initforms
; (wie bei LET/LET*) und assoziiert sie mit den Variablen zu symbols.
; Verändert *venv* (bei *-flag : incrementell, sonst auf einmal).
; Liefert drei Werte:
; 1. Liste der Variablen,
; 2. Liste der ANODEs zu den initforms,
; 3. Liste der Stackzustände nach dem Binden der Variablen.
(defun process-movable-var-list (symbols initforms *-flag)
  (do ((symbolsr symbols (cdr symbolsr))
       (initformsr initforms (cdr initformsr))
       (varlist '())
       (anodelist '())
       (stackzlist '()))
      ((null symbolsr)
       (unless *-flag (apply #'push-*venv* varlist)) ; Binden bei LET
       (values (nreverse varlist) (nreverse anodelist) (nreverse stackzlist))
      )
    (let* ((initform (car initformsr))
           (anode (c-form initform 'ONE)) ; initform compilieren
           (var (bind-movable-var (car symbolsr) anode)))
      (push anode anodelist)
      (push var varlist)
      (push *stackz* stackzlist)
      (when *-flag (push-*venv* var)) ; Binden bei LET*
) ) )

; Überprüft und optimiert die Variablen (wie bei LET/LET*)
; und liefert die Liste der Closure-Variablen (in der richtigen Reihenfolge).
(defun checking-movable-var-list (varlist anodelist)
  (do ((varlistr varlist (cdr varlistr))
       (anodelistr anodelist (cdr anodelistr))
       (closurevarlist '()))
      ((null varlistr) (nreverse closurevarlist))
    (let ((var (car varlistr)))
      (when var
        ; 1. Schritt: eventuelle Warnungen ausgeben
        (ignore-check var)
        ; 2. Schritt: Variablen-Ort (Stack oder Closure oder eliminiert)
        ; endgültig bestimmen
        (unless (var-specialp var)
          ; nur bei lexikalischen Variablen kann optimiert werden
          (if (var-constantp var)
            ; Variable lexikalisch und konstant
            (progn ; Variable eliminieren
              (setf (var-closurep var) nil)
              (setf (first (var-stackz var)) 0) ; aus dem Stack entfernen
              (when (null (cdr (anode-seclass (car anodelistr))))
                (setf (anode-code (car anodelistr)) '()) ; evtl. initform entfernen
            ) )
            (if (not (var-really-usedp var))
              ; Variable lexikalisch und unbenutzt
              (progn ; Variable eliminieren
                (setf (var-closurep var) nil)
                (setf (first (var-stackz var)) 0) ; aus dem Stack entfernen
                (when (null (cdr (anode-seclass (car anodelistr))))
                  (setf (anode-code (car anodelistr)) '()) ; evtl. initform entfernen
                )
                (unmodify-unused-var var) ; Zuweisungen auf var eliminieren
              )
              (when (var-closurep var)
                ; Variable muss in der Closure liegen
                (setf (first (var-stackz var)) 0) ; belegt 0 Stack-Einträge
                (push var closurevarlist)
        ) ) ) )
) ) ) )

; Optimiert eine Liste von Variablen.
; (In der Liste müssen die lexikalisch inneren Variablen zuletzt kommen.)
(defun optimize-var-list (vars)
  (unless *no-code*
    (dolist (var (reverse vars))
      (when var
        ; Optimierung (innere Variablen zuerst):
        ; Wird eine Variable innervar an den Wert von var gebunden, wird
        ; während der Lebensdauer von innervar weder innervar noch var verändert
        ; (um dies sicherstellen zu können, müssen beide lexikalisch und im Stack
        ; sein), so kann innervar durch var ersetzt werden.
        (unless (or (var-specialp var) (var-closurep var))
          ; var ist lexikalisch und im Stack
          (dolist (innervar-info (var-replaceable-list var))
            (let ((innervar (first innervar-info)))
              ; innervar ist eine movable-var, die mit var initialisiert wird.
              ; Während der Lebensdauer von innervar wird var nichts zugewiesen.
              (unless (or (var-specialp innervar) (var-closurep innervar))
                ; innervar ist lexikalisch und im Stack
                (when (null (var-modified-list innervar))
                  ; Während der Lebensdauer von innervar wird auch innervar
                  ; nichts zugewiesen.
                  (unless (eql (first (var-stackz innervar)) 0) ; innervar noch nicht wegoptimiert?
                    (when (cddr innervar-info) ; und innervar-info korrekt dreigliedrig?
                      ; Variable innervar eliminieren:
                      (setf (first (var-stackz innervar)) 0) ; aus dem Stack entfernen
                      ; Initialisierung und Binden von innervar eliminieren:
                      (setf (anode-code (second innervar-info)) '())
                      (setf (anode-code (cddr innervar-info)) '())
                      ; Die Referenzen auf die Variable innervar werden
                      ; in Referenzen auf die Variable var umgewandelt:
                      (let ((using-var (var-usedp var)))
                        (do ((using-innervar (var-usedp innervar) (cdr using-innervar)))
                            ((atom using-innervar))
                          (let* ((anode (car using-innervar)) ; ein Anode vom Typ VAR
                                 (code (anode-code anode))) ; sein Code, () oder ((GET ...))
                            (unless (null code)
                              ; (anode-code anode) ist von der Gestalt ((GET innervar ...))
                              (setf (second (car code)) var)
                              (push anode using-var)
                        ) ) )
                        (setf (var-usedp var) using-var)
                      )
        ) ) ) ) ) ) )
) ) ) )

; Bildet den Code, der eine Liste von Variablen, zusammen mit ihren svars,
; bindet (wie bei Lambdabody- Optional/Key - Variablen).
(defun c-bind-with-svars (-vars -dummys s-vars -anodes s-anodes -stackzs)
  (do ((-varsr -vars (cdr -varsr)) ; fixed-vars
       (-dummysr -dummys (cdr -dummysr))
       (s-varsr s-vars (cdr s-varsr)) ; movable-vars
       (-anodesr -anodes (cdr -anodesr))
       (s-anodesr s-anodes (cdr s-anodesr))
       (-stackzsr -stackzs (cdr -stackzsr))
       (L '()))
      ((null -varsr) (nreverse L))
    (when (car s-varsr)
      (setq L
        (revappend
          (c-bind-movable-var-anode (car s-varsr) (car s-anodesr))
          L
    ) ) )
    (setq L
      (revappend
        (let* ((var (car -varsr))
               (stackdummyvar (car -dummysr))
               (anode (car -anodesr))
               (stackz (car -stackzsr))
               (label (make-label 'ONE)))
          (if (var-specialp var)
            `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
              ,anode
              ,label
              ,@(if (var-constantp var)
                  '() ; Konstante kann nicht gebunden werden
                  `((BIND ,(new-const (var-name var))))
                )
             )
            ; var lexikalisch, nach Definition nicht konstant
            (if (var-closurep var)
              `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
                ,anode
                ,label
                (SET ,var ,*venvc* ,stackz)
               )
              (if (not (var-really-usedp var))
                ; Variable wurde in checking-fixed-var-list wegoptimiert
                (if (cdr (anode-seclass anode))
                  `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
                    ,anode
                    ,label
                   )
                  '()
                )
                ; im Stack vorhandene Variable
                `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
                  ,anode
                  (SET ,var ,*venvc* ,stackz)
                  ,label
                 )
        ) ) ) )
        L
    ) )
) )

; compiliere (name lambdalist {declaration|docstring}* {form}*), liefere FNODE
(defun c-LAMBDABODY (name lambdabody &optional fenv-cons gf-p reqoptimflags)
  (test-list lambdabody 1)
  (let* ((*func* (make-fnode :name name :enclosing *func* :venvc *venvc*))
         (*stackz* *func*) ; leerer Stack
         (*venvc* (cons *func* *venvc*))
         (*func-start-label* (make-label 'NIL))
         (*anonymous-count* 0)
         (anode (catch 'c-error
    ; ab hier wird's kompliziert
    (multiple-value-bind (reqvar  optvar optinit optsvar  restvar
                          keyflag keyword keyvar keyinit keysvar allow-other-keys
                          auxvar auxinit)
        (if fenv-cons
          (values-list (cddar fenv-cons)) ; Bei c-LABELS wurde analyze-lambdalist schon aufgerufen
          (analyze-lambdalist (car lambdabody))
        )
      (setf (fnode-req-anz *func*) (length reqvar)
            (fnode-opt-anz *func*) (length optvar)
            (fnode-rest-flag *func*) (not (eql restvar 0))
            (fnode-keyword-flag *func*) keyflag
            (fnode-keywords *func*) keyword
            (fnode-allow-other-keys-flag *func*) allow-other-keys
      )
      (when fenv-cons (setf (caar fenv-cons) *func*)) ; Fixup für c-LABELS
      (multiple-value-bind (body-rest declarations)
          (parse-body (cdr lambdabody) t (vector *venv* *fenv*))
        (let ((oldstackz *stackz*)
              (*stackz* *stackz*)
              (*denv* *denv*)
              (*venv* *venv*)
              (*venvc* *venvc*)
              *specials* *ignores* *ignorables* *readonlys*
              req-vars req-dummys req-stackzs
              opt-vars opt-dummys opt-anodes opts-vars opts-anodes opt-stackzs
              rest-vars rest-dummys rest-stackzs
              key-vars key-dummys key-anodes keys-vars keys-anodes key-stackzs
              aux-vars aux-anodes
              closuredummy-stackz closuredummy-venvc
             )
          (multiple-value-setq (*specials* *ignores* *ignorables* *readonlys*)
            (process-declarations declarations)
          )
          ; Special-Variable auf *venv* pushen:
          (push-specials)
          ; Sichtbarkeit von Closure-Dummyvar:
          (push nil *venvc*)
          (setq closuredummy-venvc *venvc*)
          ; Stack-Dummy-Variable für die reqvar,optvar,restvar,keyvar bilden:
          (multiple-value-setq (req-vars req-dummys)
            (process-fixed-var-list reqvar reqoptimflags)
          )
          (multiple-value-setq (opt-vars opt-dummys)
            (process-fixed-var-list optvar)
          )
          (multiple-value-setq (rest-vars rest-dummys)
            (if (eql restvar 0)
              (values '() '())
              (process-fixed-var-list (list restvar))
          ) )
          (multiple-value-setq (key-vars key-dummys)
            (process-fixed-var-list keyvar)
          )
          ; Platz für die Funktion selbst (unter den Argumenten):
          (push 1 *stackz*)
          ; Platz für Closure-Dummyvar:
          (push 0 *stackz*)
          (setq closuredummy-stackz *stackz*)
          ; Bindungen der required-Parameter aktivieren:
          (setq req-stackzs (bind-req-vars req-vars))
          ; Bindungen der optional-Parameter/svar aktivieren:
          (multiple-value-setq (opt-anodes opt-stackzs opts-vars opts-anodes)
            (bind-opt-vars opt-vars opt-dummys optinit optsvar)
          )
          ; Bindung des rest-Parameters aktivieren:
          (unless (eql restvar 0)
            (setq rest-stackzs (bind-rest-vars rest-vars))
          )
          ; Bindungen der keyword-Parameter/svar aktivieren:
          (multiple-value-setq (key-anodes key-stackzs keys-vars keys-anodes)
            (bind-opt-vars key-vars key-dummys keyinit keysvar)
          )
          ; Bindungen der Aux-Variablen aktivieren:
          (multiple-value-setq (aux-vars aux-anodes)
            (bind-aux-vars auxvar auxinit)
          )
          (let* ((body-anode (c-form `(PROGN ,@body-rest) (if gf-p 'ONE 'ALL)))
                 ; Überprüfen der Variablen:
                 (closurevars
                   (append
                     (checking-fixed-var-list req-vars reqoptimflags)
                     (checking-fixed-var-list opt-vars)
                     (checking-movable-var-list opts-vars opts-anodes)
                     (checking-fixed-var-list rest-vars)
                     (checking-fixed-var-list key-vars)
                     (checking-movable-var-list keys-vars keys-anodes)
                     (checking-movable-var-list aux-vars aux-anodes)
                 ) )
                 (codelist
                   `(,*func-start-label*
                     ,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                     ,@(mapcap #'c-bind-fixed-var req-vars req-dummys req-stackzs)
                     ,@(c-bind-with-svars opt-vars opt-dummys opts-vars opt-anodes opts-anodes opt-stackzs)
                     ,@(mapcap #'c-bind-fixed-var rest-vars rest-dummys rest-stackzs)
                     ,@(c-bind-with-svars key-vars key-dummys keys-vars key-anodes keys-anodes key-stackzs)
                     ,@(mapcap #'c-bind-movable-var-anode aux-vars aux-anodes)
                     ,body-anode
                     (UNWIND ,*stackz* ,oldstackz t)
                     ,(if gf-p '(RETGF) '(RET))
                 )  )
                 (anode
                   (make-anode
                     :type 'LAMBDABODY
                     :source lambdabody
                     :sub-anodes `(,@opt-anodes ,@(remove nil opts-anodes)
                                   ,@key-anodes ,@(remove nil keys-anodes)
                                   ,@aux-anodes ,body-anode
                                  )
                     :seclass '(T . T) ; die Seiteneffektklasse dieses Anode ist irrelevant
                     :stackz oldstackz
                     :code codelist
                )) )
            (when closurevars
              (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
              (setf (first closuredummy-venvc)
                (cons closurevars closuredummy-stackz)
            ) )
            (optimize-var-list (append req-vars opt-vars opts-vars rest-vars key-vars keys-vars aux-vars))
            anode
    ) ) ) )
    ; das war die Produktion des Anode
        ))      )
    (setf (fnode-code *func*) anode)
    (when reqoptimflags (decf (fnode-req-anz *func*) (count 'GONE reqoptimflags)))
    (when (eq (anode-type anode) 'ERROR)
      ; korrekte, aber nichtstuende Funktion daraus machen
      (setf (fnode-req-anz *func*) 0
            (fnode-opt-anz *func*) 0
            (fnode-rest-flag *func*) t
            (fnode-keyword-flag *func*) nil
            (fnode-keywords *func*) '()
            (fnode-allow-other-keys-flag *func*) nil
            (anode-code (fnode-code *func*)) `((NIL) (SKIP 2) (RET))
    ) )
    (setf (fnode-gf-p *func*) gf-p)
    (setf (fnode-Consts-Offset *func*)
      (+ (setf (fnode-Keyword-Offset *func*)
           (+ (setf (fnode-Tagbodys-Offset *func*)
                (+ (setf (fnode-Blocks-Offset *func*)
                     (if (fnode-venvconst *func*) 1 0)
                   )
                   (length (fnode-Blocks *func*))
              ) )
              (length (fnode-Tagbodys *func*))
         ) )
         (length (fnode-Keywords *func*))
    ) )
    (when gf-p
      ; Der Dispatch generischer Funktionen kann nicht auf externe Blocks und
      ; Tagbodys verweisen. Die Keywords allerdings werden notgedrungen verlagert.
      (when (or (fnode-Blocks *func*) (fnode-Tagbodys *func*))
        (compiler-error 'c-LAMBDABODY "GF")
      )
      ; Nun ist (fnode-Keyword-Offset *func*) = (fnode-Tagbodys-Offset *func*) =
      ;       = (fnode-Blocks-Offset *func*) = (if (fnode-venvconst *func*) 1 0)
    )
    *func*
) )
(defun bind-req-vars (req-vars)
  (let ((req-stackzs '()))
    (dolist (var req-vars)
      (push-*venv* var)
      (push *stackz* req-stackzs)
      (bind-fixed-var-2 var)
    )
    (nreverse req-stackzs)
) )
(defun bind-opt-vars (opt-vars opt-dummys optinit optsvar)
  (let ((opt-anodes '())
        (opt-stackzs '())
        (opts-vars '())
        (opts-anodes '()))
    (do ((opt-varsr opt-vars (cdr opt-varsr))
         (opt-dummysr opt-dummys (cdr opt-dummysr))
         (optinitr optinit (cdr optinitr))
         (optsvarr optsvar (cdr optsvarr)))
        ((null opt-varsr))
      (if (eql (car optsvarr) 0)
        (progn (push nil opts-vars) (push nil opts-anodes))
        (let* ((anode
                 (make-anode
                   :type 'OPTIONAL-SVAR
                   :sub-anodes '()
                   :seclass (cons (list (car opt-dummysr)) 'NIL)
                   :code `((BOUNDP ,(car opt-dummysr) ,*venvc* ,*stackz*))
               ) )
               (var (bind-movable-var (car optsvarr) anode))
              )
          (push anode opts-anodes)
          (push var opts-vars)
      ) )
      (push (c-form (car optinitr) 'ONE) opt-anodes)
      (push-*venv* (car opt-varsr))
      (push *stackz* opt-stackzs) (bind-fixed-var-2 (car opt-varsr))
      (unless (eql (car optsvarr) 0) (push-*venv* (car opts-vars)))
    )
    (values
      (nreverse opt-anodes) (nreverse opt-stackzs)
      (nreverse opts-vars) (nreverse opts-anodes)
    )
) )
(defun bind-rest-vars (rest-vars)
  (let ((rest-stackzs '()))
    (push-*venv* (car rest-vars))
    (push *stackz* rest-stackzs)
    (bind-fixed-var-2 (car rest-vars))
    rest-stackzs ; (nreverse rest-stackzs) unnötig
) )
(defun bind-aux-vars (auxvar auxinit)
  (let ((aux-vars '())
        (aux-anodes '()))
    (do ((auxvarr auxvar (cdr auxvarr))
         (auxinitr auxinit (cdr auxinitr)))
        ((null auxvarr))
      (let* ((initform (car auxinitr))
             (anode (c-form initform 'ONE))
             (var (bind-movable-var (car auxvarr) anode)))
        (push anode aux-anodes)
        (push var aux-vars)
        (push-*venv* var)
    ) )
    (values (nreverse aux-vars) (nreverse aux-anodes))
) )

; liefert den ANODE, der (bei gegebenem aktuellem Stackzustand)
; die zu einem FNODE gehörende Funktion als Wert liefert.
(defun c-FNODE-FUNCTION (fnode &optional (*stackz* *stackz*))
  (make-anode
    :type 'FUNCTION
    :sub-anodes '()
    :seclass '(NIL . NIL)
    :code (if (zerop (fnode-keyword-offset fnode))
            `((FCONST ,fnode))
            `(,@(if (fnode-Venvconst fnode)
                  (prog1 ; beim Aufbau mitzugebendes Venv
                    `((VENV ,(fnode-venvc fnode) ,*stackz*)
                      (PUSH)
                     )
                    (setq *stackz* (cons 1 *stackz*))
                ) )
              ,@(mapcap ; beim Aufbau mitzugebende Block-Conses
                  #'(lambda (block)
                      (prog1
                        `(,(if (member block (fnode-Blocks *func*) :test #'eq)
                             `(BCONST ,block)
                             `(GET ,(block-consvar block) ,*venvc* ,*stackz*)
                           )
                           (PUSH)
                         )
                        (setq *stackz* (cons 1 *stackz*))
                    ) )
                  (fnode-Blocks fnode)
                )
              ,@(mapcap ; beim Aufbau mitzugebende Tagbody-Conses
                  #'(lambda (tagbody)
                      (prog1
                        `(,(if (member tagbody (fnode-Tagbodys *func*) :test #'eq)
                             `(GCONST ,tagbody)
                             `(GET ,(tagbody-consvar tagbody) ,*venvc* ,*stackz*)
                           )
                           (PUSH)
                         )
                        (setq *stackz* (cons 1 *stackz*))
                    ) )
                  (fnode-Tagbodys fnode)
                )
              ,@(if (fnode-gf-p fnode)
                  (progn
                    (assert (= (fnode-keyword-offset fnode) 1))
                    `((FCONST ,fnode)
                      (PUSH)
                      ,(CALLS-code (gethash 'SYSTEM::%COPY-GENERIC-FUNCTION function-codes))
                     )
                  )
                  `((COPY-CLOSURE ,fnode ,(fnode-keyword-offset fnode)))
                )
             )
          )
) )


;        ERSTER PASS :   S P E C I A L   F O R M S

; compiliere (PROGN {form}*)
; keine Formen -> NIL, genau eine Form -> diese Form,
; mindestens zwei Formen -> alle der Reihe nach, nur bei der letzten kommt es
; auf die Werte an.
(defun c-PROGN ()
  (test-list *form* 1)
  (let ((L (cdr *form*))) ; Liste der Formen
    (cond ((null L) (c-NIL)) ; keine Form -> NIL
          ((null (cdr L)) (c-form (car L))) ; genau eine Form
          (t (do (#+COMPILER-DEBUG (anodelist '())
                  (seclass '(NIL . NIL))
                  (codelist '())
                  (Lr L)) ; restliche Formenliste
                 ((null Lr)
                  (make-anode
                    :type 'PROGN
                    :sub-anodes (nreverse anodelist)
                    :seclass seclass
                    :code (nreverse codelist)
                 ))
               (let* ((formi (pop Lr)) ; i-te Form
                      (anodei (c-form formi (if (null Lr) *for-value* 'NIL))))
                 #+COMPILER-DEBUG (push anodei anodelist)
                 (seclass-or-f seclass anodei)
                 (push anodei codelist)
) ) )     )  ) )

; compiliere (PROG1 form1 {form}*)
; bei *for-value* muss der Wert von form1 im Stack gerettet werden
(defun c-PROG1 ()
  (test-list *form* 2)
  (if (or (null *for-value*) (and (eq *for-value* 'ONE) (null (cddr *form*))))
    (c-form `(PROGN ,@(cdr *form*)))
    (let ((anode1 (c-form (second *form*) 'ONE))
          (anode2 (let ((*stackz* (cons 1 *stackz*)))
                    (c-form `(PROGN ,@(cddr *form*)) 'NIL)
         ))       )
      (make-anode
        :type 'PROG1
        :sub-anodes (list anode1 anode2)
        :seclass (anodes-seclass-or anode1 anode2)
        :code `(,anode1 (PUSH) ,anode2 (POP))
) ) ) )

; compiliere (PROG2 form1 form2 {form}*)
(defun c-PROG2 ()
  (test-list *form* 3)
  (c-form `(PROGN ,(second *form*) (PROG1 ,(third *form*) ,@(cdddr *form*))))
)

; compiliere (IF form1 form2 [form3])
; ist form1 eine Konstante, so kann der Compiler die Fallunterscheidung treffen.
(defun c-IF ()
  (test-list *form* 3 4)
  (let ((form1 (second *form*))
        (form2 (third *form*))
        (form3 (fourth *form*))) ; = NIL, falls *form* nur 3 lang ist
    (let ((anode1 (c-form form1 'ONE)))
      (if (anode-constantp anode1)
        (if (anode-constant-value anode1)
          (prog1 (c-form form2) (let ((*no-code* t)) (c-form form3 'NIL)))
          (prog2 (let ((*no-code* t)) (c-form form2 'NIL)) (c-form form3))
        )
        (let ((anode2 (c-form form2))
              (label1 (make-label *for-value*)))
          (if form3
            (let ((anode3 (c-form form3))
                  (label2 (make-label 'NIL)))
              (make-anode
                :type 'IF
                :sub-anodes (list anode1 anode2 anode3)
                :seclass (anodes-seclass-or anode1 anode2 anode3)
                :code
                  `(,anode1
                    (JMPIFNOT ,label2)
                    ,anode2
                    (JMP ,label1)
                    ,label2
                    ,anode3
                    ,label1
                   )
            ) )
            ; save one jump on if without else
            (make-anode
              :type 'IF
              :sub-anodes (list anode1 anode2)
              :seclass (anodes-seclass-or anode1 anode2)
              :code
                `(,anode1
                  (,(if *for-value* 'JMPIFNOT1 'JMPIFNOT) ,label1)
                  ,anode2
                  ,label1
                 )
) ) ) ) ) ) )

; compiliere (WHEN form1 {form}*)
(defun c-WHEN ()
  (test-list *form* 2)
  (c-form `(IF ,(second *form*) (PROGN ,@(cddr *form*))))
)

; compiliere (UNLESS form1 {form}*)
(defun c-UNLESS ()
  (test-list *form* 2)
  (c-form `(IF ,(second *form*) NIL (PROGN ,@(cddr *form*))))
)

; compiliere (AND {form}*)
(defun c-AND ()
  (test-list *form* 1)
  (cond ((null (cdr *form*)) ; keine Formen
         (make-anode
           :type 'AND
           :sub-anodes '()
           :seclass '(NIL . NIL)
           :code '((T))
        ))
        ((null (cddr *form*)) (c-form (second *form*))) ; genau eine Form
        (t (do (#+COMPILER-DEBUG (anodelist '())
                (seclass '(NIL . NIL))
                (codelist '())
                (Lr (cdr *form*))
                (label (make-label *for-value*))) ; Label am Ende
               ((null Lr)
                (push label codelist)
                (make-anode
                  :type 'AND
                  :sub-anodes (nreverse anodelist)
                  :seclass seclass
                  :code (nreverse codelist)
               ))
             (let* ((formi (pop Lr))
                    (anodei (c-form formi (if (null Lr) *for-value* 'ONE))))
               #+COMPILER-DEBUG (push anodei anodelist)
               (seclass-or-f seclass anodei)
               (if (null Lr)
                 ; letzte Form -> direkt übernehmen
                 (push anodei codelist)
                 ; nicht letzte Form -> Test kreieren
                 (if (anode-constantp anodei)
                   ; Konstante /= NIL -> weglassen, Konstante NIL -> fertig
                   (unless (anode-constant-value anodei)
                     (if *for-value* (push '(NIL) codelist))
                     (let ((*no-code* t)) (dolist (form Lr) (c-form form 'NIL)))
                     (setq Lr nil)
                   )
                   (progn ; normaler Test
                     (push anodei codelist)
                     (push `(,(if *for-value* 'JMPIFNOT1 'JMPIFNOT) ,label)
                           codelist
             ) ) ) ) )
) )     )  )

; compiliere (OR {form}*)
(defun c-OR ()
  (test-list *form* 1)
  (cond ((null (cdr *form*)) ; keine Formen
         (make-anode
           :type 'OR
           :sub-anodes '()
           :seclass '(NIL . NIL)
           :code '((NIL))
        ))
        ((null (cddr *form*)) (c-form (second *form*))) ; genau eine Form
        (t (do (#+COMPILER-DEBUG (anodelist '())
                (seclass '(NIL . NIL))
                (codelist '())
                (Lr (cdr *form*))
                (label (make-label *for-value*))) ; Label am Ende
               ((null Lr)
                (push label codelist)
                (make-anode
                  :type 'OR
                  :sub-anodes (nreverse anodelist)
                  :seclass seclass
                  :code (nreverse codelist)
               ))
             (let* ((formi (pop Lr))
                    (anodei (c-form formi (if (null Lr) *for-value* 'ONE))))
               #+COMPILER-DEBUG (push anodei anodelist)
               (seclass-or-f seclass anodei)
               (if (null Lr)
                 ; letzte Form -> direkt übernehmen
                 (push anodei codelist)
                 ; nicht letzte Form -> Test kreieren
                 (if (anode-constantp anodei)
                   ; Konstante NIL -> weglassen, Konstante /= NIL -> fertig
                   (when (anode-constant-value anodei)
                     (if *for-value* (push anodei codelist))
                     (let ((*no-code* t)) (dolist (form Lr) (c-form form 'NIL)))
                     (setq Lr nil)
                   )
                   (progn ; normaler Test
                     (push anodei codelist)
                     (push `(,(if *for-value* 'JMPIF1 'JMPIF) ,label)
                           codelist
             ) ) ) ) )
) )     )  )

; compiliere (QUOTE object)
(defun c-QUOTE ()
  (test-list *form* 2 2)
  (let ((value (second *form*)))
    (make-anode :type 'QUOTE
                :sub-anodes '()
                :seclass '(NIL . NIL)
                :code (if *for-value* `((CONST ,(new-const value))) '() )
) ) )

; compiliere (THE type form)
(defun c-THE ()
  (test-list *form* 3 3)
  (c-form (third *form*)) ; ignoriere einfach die Typdeklaration
)

; compiliere (DECLARE {declspec}*)
(defun c-DECLARE ()
  (test-list *form* 1)
  (c-error (ENGLISH "Misplaced declaration: ~S")
           *form*
) )

; compiliere (LOAD-TIME-VALUE form [read-only-p])
(defun c-LOAD-TIME-VALUE ()
  (test-list *form* 2 3)
  (let ((form (second *form*))) ; ignoriere read-only-p
    (make-anode :type 'LOAD-TIME-VALUE
                :sub-anodes '()
                :seclass '(NIL . NIL)
                :code (if *for-value*
                        `((CONST ,(if *compiling-from-file*
                                    (if (and (symbolp form) (c-constantp form))
                                      (make-const :horizont ':all :value (c-constant-value form) :form form)
                                      (make-const :horizont ':form :form form)
                                    )
                                    (make-const :horizont ':all :value (eval form) :form form)
                                  )
                         ))
                        '()
                      )
) ) )

; compiliere (CATCH tag {form}*)
(defun c-CATCH ()
  (test-list *form* 2)
  (let* ((anode1 (c-form (second *form*) 'ONE))
         (anode2 (let ((*stackz* (cons 'CATCH *stackz*)))
                   (c-form `(PROGN ,@(cddr *form*)))
         )       )
         (label (make-label *for-value*)))
    (make-anode :type 'CATCH
                :sub-anodes (list anode1 anode2)
                :seclass (anodes-seclass-or anode1 anode2)
                :code `(,anode1
                        (CATCH-OPEN ,label)
                        ,anode2
                        (CATCH-CLOSE)
                        ,label
) ) )                  )

; compiliere (THROW tag form)
(defun c-THROW ()
  (test-list *form* 3 3)
  (let* ((anode1 (c-form (second *form*) 'ONE))
         (anode2 (let ((*stackz* (cons 1 *stackz*)))
                   (c-form (third *form*) 'ALL)
        ))       )
    (make-anode :type 'THROW
                :sub-anodes (list anode1 anode2)
                :seclass (cons (car (anodes-seclass-or anode1 anode2)) 'T)
                :code `(,anode1 (PUSH) ,anode2 (THROW))
) ) )

; compiliere (UNWIND-PROTECT form1 {form}*)
(defun c-UNWIND-PROTECT ()
  (test-list *form* 2)
  (let* ((anode1 (let ((*stackz* (cons 'UNWIND-PROTECT *stackz*)))
                   (c-form (second *form*))
         )       )
         (anode2 (let ((*stackz* (cons 'CLEANUP *stackz*)))
                   (c-form `(PROGN ,@(cddr *form*)) 'NIL)
         )       )
         (label (make-label 'NIL)))
    (make-anode :type 'UNWIND-PROTECT
                :sub-anodes (list anode1 anode2)
                :seclass (anodes-seclass-or anode1 anode2)
                :code `((UNWIND-PROTECT-OPEN ,label)
                        ,anode1
                        ,@(case *for-value*
                            ((NIL) '((VALUES0)))
                            (ONE '((VALUES1)))
                            ((T) '())
                          )
                        (UNWIND-PROTECT-NORMAL-EXIT)
                        ,label
                        ,anode2
                        (UNWIND-PROTECT-CLOSE ,label)
) ) )                  )

; compiliere (PROGV form1 form2 {form}*)
(defun c-PROGV ()
  (test-list *form* 3)
  (let ((anode1 (c-form (second *form*) 'ONE)))
    ; falls form1 konstant=NIL ist, kann man sich das Binden sparen:
    (if (and (anode-constantp anode1) (null (anode-constant-value anode1)))
      (c-form `(PROGN ,(third *form*) (PROGN ,@(cdddr *form*))))
      (let* ((stackz2 (cons 1 *stackz*))
             (anode2 (let ((*stackz* stackz2))
                       (c-form (third *form*) 'ONE)
             )       )
             (stackz3 (cons 'PROGV *stackz*))
             (anode3 (let ((*stackz* stackz3))
                       (c-form `(PROGN ,@(cdddr *form*)))
             )       )
             (flag t))
        ; falls anode3 von keinen Seiteneffekten abhängig ist, kann man sich das
        ; Binden sparen:
        (when (null (car (anode-seclass anode3)))
          (setf (first stackz2) 0)
          (setf (first stackz3) 0)
          (setq flag nil)
        )
        (make-anode :type 'PROGV
                    :sub-anodes (list anode1 anode2 anode3)
                    :seclass (anodes-seclass-or anode1 anode2 anode3)
                    :code `(,anode1
                            ,@(if flag '((PUSH)))
                            ,anode2
                            ,@(if flag '((PROGV)))
                            ,anode3
                            ,@(if flag
                                `((UNWIND ,stackz3 ,*stackz* ,*for-value*))
                                ; wird expandiert zu '((UNBIND1) (SKIPSP 1 0))
                           )  )
) ) ) ) )

; compiliere (MULTIPLE-VALUE-PROG1 form1 {form}*)
; falls Werte nicht gebraucht werden: einfaches PROGN. Sonst: falls {form}*
; seiteneffektfrei, nur form1, sonst: Werte von form1 auf den Stack legen und
; nachher mit Funktion VALUES wieder einsammeln.
(defun c-MULTIPLE-VALUE-PROG1 ()
  (test-list *form* 2)
  (case *for-value*
    (ALL
     (let* ((stackz1 (cons 'MVCALLP *stackz*))
            (anode1 (let ((*stackz* stackz1))
                      (c-form (second *form*))
            )       )
            (anode2 (let ((*stackz* (cons 'MVCALL *stackz*)))
                      (c-form `(PROGN ,@(cddr *form*)) 'NIL)
           ))       )
       (make-anode :type 'MULTIPLE-VALUE-PROG1
                   :sub-anodes (list anode1 anode2)
                   :seclass (anodes-seclass-or anode1 anode2)
                   :code
                      (if (cdr (anode-seclass anode2))
                        `((CONST , #+CLISP (make-const :horizont ':all
                                                       :value #'values
                                                       :form '(function values)
                                           )
                                   #-CLISP (new-const 'values)
                          )
                          (MVCALLP)
                          ,anode1
                          (MV-TO-STACK)
                          ,anode2
                          (MVCALL))
                        (prog2 (setf (first stackz1) 0) `(,anode1))
                      )
    )) )
    (ONE (c-form `(PROG1 ,@(cdr *form*))))
    ((NIL) (c-form `(PROGN ,@(cdr *form*))))
) )

; compiliere (MULTIPLE-VALUE-CALL form1 {form}*)
(defun c-MULTIPLE-VALUE-CALL ()
  (test-list *form* 2)
  (if (null (cddr *form*))
    ; (c-form `(SYS::%FUNCALL ,(second *form*))) ; 0 Argumente zu form1
    (c-FUNCTION-CALL (second *form*) '())
    (let* ((anode1 (c-form (second *form*) 'ONE))
           #+COMPILER-DEBUG (anodelist (list anode1))
           (codelist '()))
      (push anode1 codelist)
      (push '(MVCALLP) codelist)
      (do ((Lr (cddr *form*))
           (i 0 (1+ i)))
          ((null Lr))
        (let* ((formi (pop Lr))
               (anodei
                 (let ((*stackz* (cons (if (zerop i) 'MVCALLP 'MVCALL) *stackz*)))
                   (c-form formi 'ALL)
              )) )
          #+COMPILER-DEBUG (push anodei anodelist)
          (push anodei codelist)
          (push '(MV-TO-STACK) codelist)
      ) )
      (push '(MVCALL) codelist)
      (make-anode :type 'MULTIPLE-VALUE-CALL
                  :sub-anodes (nreverse anodelist)
                  :seclass '(T . T)
                  :code (nreverse codelist)
) ) ) )

; compiliere (MULTIPLE-VALUE-LIST form)
(defun c-MULTIPLE-VALUE-LIST ()
  (test-list *form* 2 2)
  (if *for-value*
    (let ((anode1 (c-form (second *form*) 'ALL)))
      (make-anode :type 'MULTIPLE-VALUE-LIST
                  :sub-anodes (list anode1)
                  :seclass (anodes-seclass-or anode1)
                  :code `(,anode1 (MV-TO-LIST))
    ) )
    (c-form (second *form*))
) )

; Stellt fest, ob eine SETQ-Argumentliste Symbol-Macros zuweist.
(defun setqlist-macrop (l)
  (do ((l l (cddr l)))
      ((null l) nil)
    (let ((s (car l)))
      (when (and (symbolp s) (venv-search-macro s)) (return t))
) ) )

; compiliere (SETQ {symbol form}*)
; alle Zuweisungen nacheinander durchführen
(defun c-SETQ ()
  (test-list *form* 1)
  (when (evenp (length *form*))
    (c-error (ENGLISH "Odd number of arguments to SETQ: ~S")
             *form*
  ) )
  (if (null (cdr *form*))
    (c-NIL) ; (SETQ) == (PROGN) == NIL
    (if (setqlist-macrop (cdr *form*))
      (c-form ; (SETF ...) statt (SETQ ...), macroexpandieren
        (funcall (macro-function 'SETF) (cons 'SETF (cdr *form*))
                 (vector *venv* *fenv*)
      ) )
      (do ((L (cdr *form*) (cddr L))
           #+COMPILER-DEBUG (anodelist '())
           (seclass '(NIL . NIL))
           (codelist '()))
          ((null L)
           (make-anode
             :type 'SETQ
             :sub-anodes (nreverse anodelist)
             :seclass seclass
             :code (nreverse codelist)
          ))
        (let* ((symboli (first L))
               (formi (second L))
               (anodei (c-form formi 'ONE)))
          #+COMPILER-DEBUG (push anodei anodelist)
          (if (symbolp symboli)
            (progn
              (push anodei codelist)
              (seclass-or-f seclass anodei)
              (let ((setteri (c-VARSET symboli anodei
                                       (and *for-value* (null (cddr L)))
                   ))        )
                (push setteri codelist)
                (seclass-or-f seclass setteri)
            ) )
            (progn
              (catch 'c-error
                (c-error (ENGLISH "Cannot assign to non-symbol ~S.")
                         symboli
              ) )
              (push '(VALUES1) codelist)
      ) ) ) )
) ) )

; compiliere (PSETQ {symbol form}*)
; alle Zwischenwerte auf dem Stack retten, erst dann zuweisen
(defun c-PSETQ ()
  (test-list *form* 1)
  (when (evenp (length *form*))
    (c-error (ENGLISH "Odd number of arguments to PSETQ: ~S")
             *form*
  ) )
  (if (null (cdr *form*))
    (c-NIL) ; (PSETQ) == (PROGN) == NIL
    (if (setqlist-macrop (cdr *form*))
      (c-form ; (PSETF ...) statt (PSETQ ...), macroexpandieren
        (funcall (macro-function 'PSETF) (cons 'PSETF (cdr *form*))
                 (vector *venv* *fenv*)
      ) )
      (let ((anodelist '())
            (setterlist '()))
        ; Formen und Zuweisungen compilieren:
        (do ((L (cdr *form*)))
            ((null L))
          (let* ((symboli (pop L))
                 (formi (pop L))
                 (anodei (c-form formi 'ONE)))
            (if (symbolp symboli)
              (progn
                (push anodei anodelist)
                (push (c-VARSET symboli anodei nil) setterlist)
                (push 0 *stackz*)
              )
              (catch 'c-error
                (c-error (ENGLISH "Cannot assign to non-symbol ~S.")
                         symboli
        ) ) ) ) )
        ; Versuche, sie so zu reorganisieren, dass möglichst wenige (PUSH)
        ; und (POP) nötig werden:
        (let ((codelist1 '())
              (codelist2 '())
              ; baue codelist = (nconc codelist1 (nreverse codelist2)) zusammen
              (seclass '(NIL . NIL))) ; Seiteneffektklasse von codelist insgesamt
          (do ((anodelistr anodelist (cdr anodelistr))
               (setterlistr setterlist (cdr setterlistr)))
              ((null anodelistr))
            (let ((anode (car anodelistr))
                  (setter (car setterlistr)))
              ; Normalerweise wäre vor codelist der anode und ein (PUSH)
              ; und nach codelist ein (POP) und der setter anzuhängen.
              ; Dies versuchen wir zu vereinfachen:
              (cond ((seclasses-commute (anode-seclass setter) seclass)
                     ; Ziehe den setter nach vorne:
                     (push setter codelist1)
                     (push anode codelist1)
                    )
                    ((seclasses-commute (anode-seclass anode) seclass)
                     ; Ziehe den anode nach hinten:
                     (push anode codelist2)
                     (push setter codelist2)
                    )
                    (t ; keine Vereinfachung möglich
                     (push '(PUSH) codelist1)
                     (push anode codelist1)
                     (push '(POP) codelist2)
                     (push setter codelist2)
                     (setf (car *stackz*) 1) ; brauche eine Variable im Stack
              )     )
              (setq seclass
                (seclass-or-2 seclass
                  (seclass-or-2 (anode-seclass anode) (anode-seclass setter))
              ) )
              (setf *stackz* (cdr *stackz*))
          ) )
          ; *stackz* ist nun wieder auf dem alten Niveau.
          (when *for-value* (push '(NIL) codelist2))
          (make-anode
            :type 'PSETQ
            :sub-anodes (nreverse anodelist)
            :seclass seclass
            :code (nconc codelist1 (nreverse codelist2))
) ) ) ) ) )

; compiliere (MULTIPLE-VALUE-SETQ ({symbol}*) form)
; alle gewünschten Werte auf den Stack, dann einzeln herunternehmen und
; zuweisen.
(defun c-MULTIPLE-VALUE-SETQ ()
  (test-list *form* 3 3)
  (test-list (second *form*) 0)
  (if (dolist (s (second *form*) nil)
        (when (and (symbolp s) (venv-search-macro s)) (return t))
      )
    (c-form `(SYSTEM::MULTIPLE-VALUE-SETF ,@(cdr *form*)))
    (let* ((n (length (second *form*)))
           (anode1 (c-form (third *form*) 'ALL))
           (*stackz* *stackz*))
      (if (zerop n)
        (make-anode :type 'MULTIPLE-VALUE-SETQ
                    :sub-anodes (list anode1)
                    :seclass (anodes-seclass-or anode1)
                    :code `(,anode1
                            ,@(if (eq *for-value* 'ALL) '((VALUES1)) '())
        )                  )
        (do ((L (second *form*) (cdr L))
             #+COMPILER-DEBUG (anodelist (list anode1))
             (seclass (anode-seclass anode1))
             (codelist '()))
            ((null L)
             (if (= n 1)
               (setq codelist (cdr codelist)) ; letztes (POP) streichen
               (setq codelist (cons `(NV-TO-STACK ,n) codelist))
             )
             (make-anode
               :type 'MULTIPLE-VALUE-SETQ
               :sub-anodes (nreverse anodelist)
               :seclass seclass
               :code (cons anode1 codelist)
            ))
          (let ((symbol (car L)))
            (if (symbolp symbol)
              (let ((setter (c-VARSET symbol
                              (make-anode :type 'NOP
                                          :sub-anodes '()
                                          :seclass '(NIL . NIL)
                                          :code '()
                              )
                              (and *for-value* (null codelist))
                   ))       )
                (push setter codelist)
                (seclass-or-f seclass setter)
              )
              (catch 'c-error
                (c-error (ENGLISH "Cannot assign to non-symbol ~S.")
                         symbol
          ) ) ) )
          (push '(POP) codelist)
          (push 1 *stackz*)
) ) ) ) )

; Liefert den Code für das parallele Binden von Variablen.
; (car *stackz*) sollte = 0 sein, (cdr *stackz*) wird evtl. erweitert.
(defun c-parallel-bind-movable-var-anode (varlist anodelist stackzlist
                                          &optional (other-anodes '())
                                         )
  ; Variable darf erst am Schluss gebunden werden, falls sie SPECIAL ist
  ; und nachfolgende Anodes von ihrem Wert abhängen können.
  (let ((bind-afterwards nil))
    (append
      (maplap
        #'(lambda (varlistr anodelistr stackzlistr)
            (let ((var (car varlistr))
                  (anode (car anodelistr)))
              (if (and (var-specialp var)
                       (let ((symbol (var-name var)))
                         (some
                           #'(lambda (other-anode)
                               ; hängt der Wert von other-anode möglicherweise
                               ; vom Wert von var ab?
                               (let ((uses (car (anode-seclass other-anode))))
                                 (or (eq uses 'T) (member symbol uses))
                             ) )
                           (cdr anodelistr)
                  )    ) )
                (let* ((stackz (car stackzlistr))
                       (dummyvar ; Hilfsvariable im Stack
                         (make-var :name (gensym) :specialp nil
                                   :closurep nil :stackz stackz
                      )) )
                  (push (list dummyvar var (cdr *stackz*)) bind-afterwards)
                  (push (car stackz) (cdr *stackz*)) ; Platz für 1 Schluss-Bindung mehr
                  (setf (car stackz) 1) ; Platz für Hilfsvariable im Stack merken
                  (c-bind-movable-var-anode dummyvar anode)
                )
                (c-bind-movable-var-anode var anode)
          ) ) )
        varlist (append anodelist other-anodes) stackzlist
      )
      other-anodes
      (mapcap
        #'(lambda (bind)
            (let ((dummyvar (first bind)) ; Hilfsvariable im Stack
                  (var (second bind)) ; SPECIAL-Variable
                  (stackz (third bind))) ; Stackzustand vor Aufbau der Schluss-Bindung
              `((GET ,dummyvar ,*venvc* ,stackz)
                ,@(c-bind-movable-var var)
               )
          ) )
        (nreverse bind-afterwards)
      )
    )
) )

; compiliere (LET/LET* ({var|(var value)}*) {declaration}* {form}*)
(defun c-LET/LET* (*-flag)
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (multiple-value-bind (body-rest declarations)
      (parse-body (cddr *form*) nil (vector *venv* *fenv*))
    (let ((oldstackz *stackz*)
          (*stackz* *stackz*)
          (*denv* *denv*)
          (*venv* *venv*)
          (*venvc* *venvc*))
      (multiple-value-bind (*specials* *ignores* *ignorables* *readonlys*)
          (process-declarations declarations)
        ; Special-Variable auf *venv* pushen:
        (push-specials)
        ; Syntaxtest der Parameterliste:
        (multiple-value-bind (symbols initforms) (analyze-letlist (second *form*))
          (push 0 *stackz*) (push nil *venvc*) ; Platz für Closure-Dummyvar
          (let ((closuredummy-stackz *stackz*)
                (closuredummy-venvc *venvc*))
            (multiple-value-bind (varlist anodelist stackzlist)
                (process-movable-var-list symbols initforms *-flag)
              (unless *-flag (push 0 *stackz*)) ; Platz für Schluss-Bindungen
              (let ((body-anode (c-form `(PROGN ,@body-rest)))) ; Body compilieren
                ; Überprüfen der Variablen:
                (let* ((closurevars (checking-movable-var-list varlist anodelist))
                       (codelist
                         `(,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                           ,@(if *-flag
                               ; sequentielles Binden der Variablen
                               (mapcap #'c-bind-movable-var-anode varlist anodelist)
                               ; paralleles Binden der Variablen
                               (c-parallel-bind-movable-var-anode varlist anodelist stackzlist)
                             )
                           ,body-anode
                           (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                       )  )
                       (anode
                         (make-anode
                           :type (if *-flag 'LET* 'LET)
                           :sub-anodes `(,@anodelist ,body-anode)
                           :seclass (seclass-without
                                      (anodelist-seclass-or `(,@anodelist ,body-anode))
                                      varlist
                                    )
                           :stackz oldstackz
                           :code codelist
                      )) )
                  (when closurevars
                    (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
                    (setf (first closuredummy-venvc)
                      (cons closurevars closuredummy-stackz)
                  ) )
                  (optimize-var-list varlist)
                  anode
) ) ) ) ) ) ) ) )

; compiliere (LOCALLY {declaration}* {form}*)
(defun c-LOCALLY (&optional (c #'c-form)) ; vgl. c-LET/LET*
  (test-list *form* 1)
  (multiple-value-bind (body-rest declarations)
      (parse-body (cdr *form*) nil (vector *venv* *fenv*))
    (let ((*venv* *venv*))
      (multiple-value-bind (*specials* ignores ignorables readonlys)
          (process-declarations declarations)
        (declare (ignore ignores ignorables readonlys))
        ; Special-Variable auf *venv* pushen:
        (push-specials)
        (funcall c `(PROGN ,@body-rest))
) ) ) )

; compiliere (MULTIPLE-VALUE-BIND ({var}*) form1 {declaration}* {form}*)
(defun c-MULTIPLE-VALUE-BIND ()
  (test-list *form* 3)
  (test-list (second *form*) 0)
  (let ((symbols (second *form*)))
    (dolist (sym symbols)
      (unless (symbolp sym)
        (c-error (ENGLISH "Only symbols may be used as variables, not ~S")
                 sym
    ) ) )
    (if (= (length symbols) 1)
      (c-form `(LET ((,(first symbols) ,(third *form*))) ,@(cdddr *form*)))
      (multiple-value-bind (body-rest declarations)
          (parse-body (cdddr *form*) nil (vector *venv* *fenv*))
        (let ((oldstackz *stackz*)
              (*stackz* *stackz*)
              (*denv* *denv*)
              (*venv* *venv*)
              (*venvc* *venvc*))
          (multiple-value-bind (*specials* *ignores* *ignorables* *readonlys*)
              (process-declarations declarations)
            ; Special-Variable auf *venv* pushen:
            (push-specials)
            (if (null symbols) ; leere Variablenliste -> gar nichts binden
              (let* ((anode1 (c-form (third *form*) 'NIL))
                     (anode2 (c-form `(PROGN ,@(cdddr *form*)))))
                (make-anode :type 'MULTIPLE-VALUE-BIND
                  :sub-anodes (list anode1 anode2)
                  :seclass (anodes-seclass-or anode1 anode2)
                  :code `(,anode1 ,anode2)
              ) )
              (let ((anode1 (c-form (third *form*) 'ALL)))
                (push nil *venvc*) ; Sichtbarkeit von Closure-Dummyvar
                (multiple-value-bind (varlist stackvarlist)
                    (process-fixed-var-list symbols)
                  (push 0 *stackz*) ; Platz für Closure-Dummyvar
                  (let* ((closuredummy-stackz *stackz*)
                         (closuredummy-venvc *venvc*)
                         (stackzlist
                           (do* ((varlistr varlist (cdr varlistr))
                                 (L '()))
                                ((null varlistr) (nreverse L))
                             (let ((var (car varlistr)))
                               (push-*venv* var)
                               (push *stackz* L) (bind-fixed-var-2 var)
                         ) ) )
                         (body-anode ; Body compilieren
                           (c-form `(PROGN ,@body-rest))
                         )
                         ; Überprüfen der Variablen:
                         (closurevars (checking-fixed-var-list varlist))
                         (codelist ; Code generieren
                           `(,anode1
                             (NV-TO-STACK ,(length symbols))
                             ,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                             ,@ ; Binden von special- oder Closure-Variablen:
                               (do ((stackvarlistr stackvarlist (cdr stackvarlistr))
                                    (stackzlistr stackzlist (cdr stackzlistr))
                                    (varlistr varlist (cdr varlistr))
                                    (L '()))
                                   ((null varlistr) (nreverse L))
                                 (setq L
                                   (append
                                     (reverse
                                       (c-bind-fixed-var
                                         (car varlistr)
                                         (car stackvarlistr)
                                         (car stackzlistr)
                                     ) )
                                     L
                               ) ) )
                             ,body-anode
                             (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                         )  )
                         (anode
                           (make-anode
                             :type 'MULTIPLE-VALUE-BIND
                             :sub-anodes (list anode1 body-anode)
                             :seclass (seclass-without
                                        (anodes-seclass-or anode1 body-anode)
                                        varlist
                                      )
                             :stackz oldstackz
                             :code codelist
                        )) )
                    (when closurevars
                      (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
                      (setf (first closuredummy-venvc)
                        (cons closurevars closuredummy-stackz)
                    ) )
                    (optimize-var-list varlist)
                    anode
) ) ) ) ) ) ) ) ) )

; compiliere (COMPILER-LET ({var|(var value)}*) {form}*)
(defun c-COMPILER-LET (&optional (c #'c-form))
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (do ((L (second *form*) (cdr L))
       (varlist '())
       (valueslist '()))
      ((null L)
       (progv (nreverse varlist) (nreverse valueslist)
         (funcall c `(PROGN ,@(cddr *form*)) )
      ))
    (cond ((symbolp (car L)) (push (car L) varlist) (push nil valueslist))
          ((and (consp (car L)) (symbolp (caar L))
                (or (null (cdar L))
                    (and (consp (cdar L)) (null (cddar L)))
           )    )
           (push (caar L) varlist) (push (eval (cadar L)) valueslist))
          (t (catch 'c-error
               (c-error (ENGLISH "Illegal syntax in COMPILER-LET: ~S")
                        (car L)
    )     )  ) )
) )

(macrolet ((check-blockname (name)
             `(unless (symbolp ,name)
                (catch 'c-error
                  (c-error (ENGLISH "Block name must be a symbol, not ~S")
                           ,name
                ) )
                (setq ,name NIL) ; Default-Blockname
              )
          ))

; compiliere (BLOCK name {form}*)
(defun c-BLOCK ()
  (test-list *form* 2)
  (let ((name (second *form*)))
    (check-blockname name)
    (let* ((*stackz* (cons 'BLOCK *stackz*)) ; Block-Frame
           (label (make-label *for-value*))
           (block (make-block :fnode *func* :label label
                    :consvar (make-var :name (gensym) :specialp nil
                                       :closurep nil :stackz *stackz*
                             )
                    :stackz *stackz* :used-far nil :for-value *for-value*
           )      )
           (*benv* (cons (cons name block) *benv*)) ; Block aktivieren
           (anode (c-form `(PROGN ,@(cddr *form*))))
          )
      (if (block-used-far block)
        (make-anode :type 'BLOCK
                    :sub-anodes (list anode)
                    :seclass (anodes-seclass-or anode)
                    :code `((BLOCK-OPEN ,(new-const (and (symbol-package name) name)) ; (gensym) zu nil machen
                                        ,label
                            )
                            ,anode
                            (BLOCK-CLOSE)
                            ,label
        )                  )
        (progn
          (setf (first *stackz*) 0) ; brauche keinen Blockframe
          (make-anode :type 'BLOCK
                      :sub-anodes (list anode)
                      :seclass (anodes-seclass-or anode)
                      :code `(,anode ,label)
) ) ) ) ) )

; compiliere (RETURN-FROM name [form])
(defun c-RETURN-FROM ()
  (test-list *form* 2 3)
  (let ((name (second *form*)))
    (check-blockname name)
    (let ((a (benv-search name)))
      (cond ((null a) ; dieser Blockname ist unsichtbar
             (c-error (ENGLISH "RETURN-FROM block ~S is impossible from here.")
                      name
            ))
            ((block-p a) ; in *benv* ohne %benv% sichtbar
             (let ((anode (c-form (third *form*) (block-for-value a))))
               (if (and (eq (block-fnode a) *func*)
                        (may-UNWIND *stackz* (cdr (block-stackz a)))
                   )
                 ; selbe Funktionen
                 (make-anode
                   :type 'RETURN-FROM
                   :sub-anodes (list anode)
                   :seclass '(T . T)
                   :code `(,anode
                           (UNWIND ,*stackz* ,(cdr (block-stackz a)) ,(block-for-value a))
                           (JMP ,(block-label a))
                 )        )
                 ; verschiedene Funktionen oder unbekannte Frames auf dem Stack
                 (progn
                   (unless *no-code*
                     ; in alle dazwischenliegenden Funktionen diesen Block eintragen:
                     (do ((fnode *func* (fnode-enclosing fnode)))
                         ((eq fnode (block-fnode a)))
                       (pushnew a (fnode-blocks fnode))
                     )
                     (setf (block-used-far a) t)
                   )
                   (make-anode
                     :type 'RETURN-FROM
                     :sub-anodes (list anode)
                     :seclass '(T . T)
                     :code `(,anode
                             ,@(if (not (block-for-value a)) '((VALUES0)))
                             (RETURN-FROM ,a
                              ,@(if (eq (block-fnode a) *func*) `(,*stackz*) '())
                   )        ))
            )) ) )
            ((consp a) ; in %benv% sichtbar
             (let ((anode (c-form (third *form*) 'ALL)))
               (make-anode
                 :type 'RETURN-FROM
                 :sub-anodes (list anode)
                 :seclass '(T . T)
                 :code `(,anode
                         (RETURN-FROM ,(new-const a))
            )) )        )
            (t (compiler-error 'c-RETURN-FROM))
) ) ) )

) ; macrolet

; compiliere (TAGBODY {tag|form}*)
(defun c-TAGBODY ()
  (test-list *form* 1)
  (multiple-value-bind (taglist labellist)
    (do ((L (cdr *form*) (cdr L))
         (taglist '())
         (labellist '()))
        ((null L) (values (nreverse taglist) (nreverse labellist)))
      (let ((item (car L)))
        (if (atom item)
          (if (or (symbolp item) (numberp item))
            ; Symbol NIL wird zugelassen, weil es in ANSI CL nicht mehr
            ; zweideutig ist.
            ; Andere Zahlen werden zugelassen, damit - ebenso wie 3.3.2 - auch
            ; 3.3 ein zulässiges Sprungziel ist.
            (progn
              (push item taglist)
              (push (make-label 'NIL) labellist)
            )
            (catch 'c-error
              (c-error (ENGLISH "Only numbers and symbols are valid tags, not ~S")
                       item
    ) ) ) ) ) )
    (let* ((*stackz* (cons 0 *stackz*)) ; evtl. TAGBODY-Frame
           (tagbody (make-tagbody :fnode *func* :labellist labellist
                      :consvar (make-var :name (gensym) :specialp nil
                                         :closurep nil :stackz *stackz*
                               )
                      :stackz *stackz*
                      :used-far (make-array (length taglist) :fill-pointer 0)
           )        )
           (*genv* (cons (cons (apply #'vector taglist) tagbody) *genv*))
             ; Tagbody aktivieren
           (codelist '())
           #+COMPILER-DEBUG (anodelist '())
           (seclass '(NIL . NIL)))
      ; Inneres des Tagbody compilieren:
      (do ((formlistr (cdr *form*) (cdr formlistr))
           (taglistr taglist)
           (labellistr labellist))
          ((null formlistr)
           #+COMPILER-DEBUG (setq anodelist (nreverse anodelist))
           (setq codelist (nreverse codelist))
          )
        (let ((formi (car formlistr)))
          (if (atom formi)
            (when (and (consp taglistr) (eql formi (car taglistr)))
              ; Tag wiedergefunden
              (pop taglistr) (push (pop labellistr) codelist)
            )
            (let ((anodei (c-form formi 'NIL)))
              #+COMPILER-DEBUG (push anodei anodelist)
              (seclass-or-f seclass anodei)
              (push anodei codelist)
      ) ) ) )
      (if (> (length (tagbody-used-far tagbody)) 0)
        (let* ((used-tags (tagbody-used-far tagbody))
               (l (length used-tags))
               (used-label-list
                 (do ((i 0 (1+ i))
                      (l1 '()))
                     ((= i l) (nreverse l1))
                   (push
                     (elt labellist (position (aref used-tags i) taglist :test #'eql))
                     l1
              )) ) )
          (setf (first *stackz*) `(TAGBODY ,l))
          (setq codelist
            `((TAGBODY-OPEN
                ,(new-const (map 'simple-vector
                                 #'(lambda (tag) (and (symbol-package tag) tag)) ; (gensym)s zu nil machen
                                 used-tags
                 )          )
                ,@used-label-list
              )
              ,@codelist
              (TAGBODY-CLOSE-NIL)
        ) )  )
        (when *for-value* (setq codelist `(,@codelist (NIL))))
      )
      (make-anode :type 'TAGBODY
                  :sub-anodes anodelist
                  :seclass seclass
                  :code codelist
) ) ) )

; compiliere (GO tag)
(defun c-GO ()
  (test-list *form* 2 2)
  (let ((tag (second *form*)))
    (unless (or (symbolp tag) (numberp tag))
      (c-error (ENGLISH "Tag must be a symbol or a number, not ~S")
               tag
    ) )
    (multiple-value-bind (a b) (genv-search tag)
      (cond ((null a) ; dieser Tag ist unsichtbar
             (c-error (ENGLISH "GO to tag ~S is impossible from here.")
                      tag
            ))
            ((tagbody-p a) ; in *genv* ohne %genv% sichtbar
             (if (and (eq (tagbody-fnode a) *func*)
                      (may-UNWIND *stackz* (tagbody-stackz a))
                 )
               ; selbe Funktionen
               (make-anode
                 :type 'GO
                 :sub-anodes '()
                 :seclass '(T . T)
                 :code `((UNWIND ,*stackz* ,(tagbody-stackz a) nil)
                         (JMP ,(nth b (tagbody-labellist a)))
               )        )
               ; verschiedene Funktionen oder unbekannte Frames auf dem Stack
               (let ((index 0))
                 (unless *no-code*
                   (setq index
                     (do* ((v (tagbody-used-far a))
                           (l (length v))
                           (i 0 (1+ i)))
                          ((= i l) (vector-push tag v) l)
                       (if (eql (aref v i) tag) (return i))
                   ) )
                   ; (aref (tagbody-used-far a) index) = tag
                   ; in alle dazwischenliegenden Funktionen diesen Tagbody eintragen:
                   (do ((fnode *func* (fnode-enclosing fnode)))
                       ((eq fnode (tagbody-fnode a)))
                     (pushnew a (fnode-tagbodys fnode))
                 ) )
                 (make-anode
                   :type 'GO
                   :sub-anodes '()
                   :seclass '(T . T)
                   :code `((VALUES0)
                           (GO ,a ,index
                            ,@(if (eq (tagbody-fnode a) *func*) `(,*stackz*) '())
                          ))
                 )
            )) )
            ((consp a) ; in %genv% sichtbar
             (make-anode
               :type 'GO
               :sub-anodes '()
               :seclass '(T . T)
               :code `((GO ,(new-const a) ,b))
            ))
            (t (compiler-error 'c-GO))
) ) ) )

; compiliere (FUNCTION funname)
(defun c-FUNCTION ()
  (test-list *form* 2 3)
  (let* ((longp (cddr *form*)) ; Flag, ob Langform (FUNCTION name funname)
         (name (second *form*)))
    (if (and (not longp) (function-name-p name))
      (multiple-value-bind (a b c) (fenv-search name)
        (case a
          ((NIL)
           (when *compiling-from-file* ; von COMPILE-FILE aufgerufen?
             (note-function-used name)
           )
           (make-anode
             :type 'FUNCTION
             :sub-anodes '()
             :seclass '(T . NIL)
             :code (if (and (subr-info name) (not (declared-notinline name)))
                     `((CONST ,(make-const :horizont ':all
                                           :value (symbol-function name)
                                           :form `(FUNCTION ,name)
                      ))       )
                     `((CONST ,(make-funname-const name)) (SYMBOL-FUNCTION))
          ))       )
          (SYSTEM::MACRO
           (c-error (ENGLISH "~S is not a function. It is a locally defined macro.")
                    name
          ))
          (GLOBAL ; gefunden in %fenv%
           (make-anode
             :type 'FUNCTION
             :sub-anodes '()
             :seclass '(T . NIL)
             :code `((CONST ,(new-const b))
                     (PUSH)
                     (CONST ,(new-const c))
                     (SVREF)
          ))        )
          (LOCAL ; gefunden in *fenv* ohne %fenv%
           (if (const-p b)
             (make-anode
               :type 'FUNCTION
               :sub-anodes '()
               :seclass '(NIL . NIL)
               :code `((FCONST ,(const-value b)))
             )
             (c-VAR (var-name b))
          ))
          (t (compiler-error 'c-FUNCTION))
      ) )
      (let ((funname (car (last *form*))))
        (if (and (consp funname) (eq (car funname) 'LAMBDA) (consp (cdr funname)))
          (let ((*no-code* (or *no-code* (null *for-value*))))
            (c-fnode-function
              (c-lambdabody
                (if (and longp (function-name-p name))
                  name ; angegebener Funktionsname
                  (symbol-suffix (fnode-name *func*) (incf *anonymous-count*))
                )
                (cdr funname)
          ) ) )
          (c-error (ENGLISH "Only symbols and lambda expressions are function names, not ~S")
                   funname
) ) ) ) ) )

; compiliere (%GENERIC-FUNCTION-LAMBDA . lambdabody)
(defun c-%GENERIC-FUNCTION-LAMBDA ()
  (test-list *form* 1)
  (let ((*no-code* (or *no-code* (null *for-value*))))
    (c-fnode-function
      (c-lambdabody
        (symbol-suffix (fnode-name *func*) (incf *anonymous-count*))
        (cdr *form*)
        nil
        t ; gf-p = T, Code für generische Funktion bauen
) ) ) )

; compiliere (%OPTIMIZE-FUNCTION-LAMBDA reqoptimflags . lambdabody)
; reqoptimflags ist eine Liste von Flags, welche Required-Parameter des
; lambdabody wegoptimiert werden dürfen. Zu jedem Required-Parameter:
; NIL: normal,
; T: darf wegoptimiert werden, dann wird daraus GONE gemacht.
; NILs am Schluss der Liste dürfen weggelassen werden.
; Die Ausgabe enthält zusätzlich zur Funktion die Liste der Wegoptimierten.
(defmacro %OPTIMIZE-FUNCTION-LAMBDA (reqoptimflags &rest lambdabody)
  (declare (ignore reqoptimflags))
  `(CONS (FUNCTION (LAMBDA ,@lambdabody)) NIL) ; ohne Compiler: nicht optimieren
)
(defun c-%OPTIMIZE-FUNCTION-LAMBDA ()
  (test-list *form* 2)
  (let ((*no-code* (or *no-code* (null *for-value*))))
    (let* ((reqoptimflags (copy-list (second *form*)))
           (anode1
             (c-fnode-function
               (c-lambdabody
                 (symbol-suffix (fnode-name *func*) (incf *anonymous-count*))
                 (cddr *form*)
                 nil nil reqoptimflags
           ) ) )
           (resultflags (mapcar #'(lambda (x) (eq x 'GONE)) reqoptimflags))
           (anode2 (let ((*stackz* (cons 1 *stackz*))
                         (*form* `(QUOTE ,resultflags)))
                     (c-QUOTE)
          ))       )
      (make-anode :type '%OPTIMIZE-FUNCTION-LAMBDA
                  :sub-anodes (list anode1 anode2)
                  :seclass (anodes-seclass-or anode1 anode2)
                  :code `(,anode1 (PUSH) ,anode2 (CONS))
) ) ) )

(macrolet ((err-syntax (specform fdef)
             `(catch 'c-error
                (c-error (ENGLISH "Illegal function definition syntax in ~S: ~S")
                         ,specform ,fdef
              ) )
          ))

; compiliere (FLET ({fundef}*) {form}*)
(defun c-FLET ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (multiple-value-bind (namelist fnodelist)
      (do ((fdefsr (second *form*) (cdr fdefsr))
           (L1 '())
           (L2 '()))
          ((null fdefsr) (values (nreverse L1) (nreverse L2)))
        (let ((fdef (car fdefsr)))
          (if (and (consp fdef) (function-name-p (car fdef)) (consp (cdr fdef)))
            (let* ((name (car fdef))
                   (fnode (c-lambdabody
                            (symbol-suffix (fnode-name *func*) name)
                            (cons (cadr fdef)
                                  (add-implicit-block name (cddr fdef))
                  ))      ) )
              (push name L1)
              (push fnode L2)
            )
            (err-syntax 'FLET fdef)
      ) ) )
    ; namelist = Liste der Namen, fnodelist = Liste der fnodes der Funktionen
    (let ((oldstackz *stackz*)
          (*stackz* *stackz*)
          (*venvc* *venvc*)
          (*venv* *venv*))
      (push 0 *stackz*) (push nil *venvc*) ; Platz für Closure-Dummyvar
      (let ((closuredummy-stackz *stackz*)
            (closuredummy-venvc *venvc*))
        (multiple-value-bind (varlist anodelist *fenv*)
            (do ((namelistr namelist (cdr namelistr))
                 (fnodelistr fnodelist (cdr fnodelistr))
                 (varlist '())
                 (anodelist '())
                 (fenv '()))
                ((null namelistr)
                 (values (nreverse varlist) (nreverse anodelist)
                         (apply #'vector (nreverse (cons *fenv* fenv)))
                ))
              (push (car namelistr) fenv)
              (let ((fnode (car fnodelistr)))
                (if (zerop (fnode-keyword-offset fnode))
                  ; Funktionsdefinition ist autonom
                  (push (cons (list fnode) (make-const :horizont ':value :value fnode)) fenv)
                  (progn
                    (push (c-fnode-function fnode) anodelist)
                    (push 1 *stackz*)
                    (let ((var (make-var :name (gensym) :specialp nil
                                 :constantp nil
                                 :usedp t :for-value-usedp t :really-usedp nil
                                 :closurep nil ; später evtl. auf T gesetzt
                                 :stackz *stackz* :venvc *venvc*
                         ))    )
                      (push (cons (list fnode) var) fenv)
                      (push var varlist)
            ) ) ) ) )
          (apply #'push-*venv* varlist) ; Hilfsvariablen aktivieren
          (let* ((body-anode ; restliche Formen compilieren
                   (c-form `(PROGN ,@(cddr *form*)))
                 )
                 (closurevars (checking-movable-var-list varlist anodelist))
                 (anode
                   (make-anode
                     :type 'FLET
                     :sub-anodes `(,@anodelist ,body-anode)
                     :seclass (seclass-without
                                (anodelist-seclass-or `(,@anodelist ,body-anode))
                                varlist
                              )
                     :code `(,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                             ,@(mapcap #'c-bind-movable-var-anode varlist anodelist)
                             ,body-anode
                             (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                   )        )
                ))
            (when closurevars
              (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
              (setf (first closuredummy-venvc)
                (cons closurevars closuredummy-stackz)
            ) )
            (optimize-var-list varlist)
            anode
) ) ) ) ) )

; compiliere (LABELS ({fundef}*) {form}*)
(defun c-LABELS ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (let ((oldstackz *stackz*)
        (*stackz* *stackz*)
        (*venvc* *venvc*)
        (*venv* *venv*))
    (push 0 *stackz*) (push nil *venvc*) ; Platz für Closure-Dummyvar
    (let ((closuredummy-stackz *stackz*)
          (closuredummy-venvc *venvc*))
      (multiple-value-bind (namelist varlist lambdanamelist lambdabodylist fenvconslist)
          (do ((fdefsr (second *form*) (cdr fdefsr))
               (L1 '())
               (L2 '())
               (L3 '())
               (L4 '())
               (L5 '()))
              ((null fdefsr)
               (values (nreverse L1) (nreverse L2) (nreverse L3) (nreverse L4) (nreverse L5))
              )
            (let ((fdef (car fdefsr)))
              (if (and (consp fdef) (function-name-p (car fdef)) (consp (cdr fdef)))
                (let ((name (car fdef)))
                  (push name L1)
                  (push 1 *stackz*)
                  (push (make-var :name (gensym) :specialp nil
                                  :constantp nil
                                  :usedp t :for-value-usedp t :really-usedp nil
                                  :closurep nil ; später evtl. auf T gesetzt
                                  :stackz *stackz* :venvc *venvc*
                        )
                        L2
                  )
                  (push (symbol-suffix (fnode-name *func*) name) L3)
                  (push (cdr fdef) L4)
                  (push
                    (cons
                      ; fdescr, bestehend aus:
                      (cons nil ; Platz für den FNODE
                        (cons 'LABELS
                          (multiple-value-list ; Werten von analyze-lambdalist
                            (analyze-lambdalist (cadr fdef))
                      ) ) )
                      ; Variable
                      (car L2)
                    )
                    L5
                ) )
                (err-syntax 'LABELS fdef)
          ) ) )
        ; namelist = Liste der Namen, varlist = Liste der Variablen,
        ; lambdanamelist = Liste der Dummynamen der Funktionen,
        ; lambdabodylist = Liste der Lambdabodys der Funktionen,
        ; fenvconslist = Liste der Conses (fdescr . var) für *fenv*
        ; (jeweils fdescr noch ohne den fnode, der kommt erst später hinein).
        (let ((*fenv* ; Funktionsnamen aktivieren
                (do ((namelistr namelist (cdr namelistr))
                     (fenvconslistr fenvconslist (cdr fenvconslistr))
                     (L nil))
                    ((null namelistr)
                     (push *fenv* L)
                     (apply #'vector (nreverse L))
                    )
                  (push (car namelistr) L)
                  (push (car fenvconslistr) L)
             )) )
          (apply #'push-*venv* varlist) ; Hilfsvariablen aktivieren
          (let* ((fnodelist ; Funktionen compilieren
                   (mapcar #'(lambda (name lambdaname lambdabody fenvcons)
                               (c-lambdabody
                                 lambdaname
                                 (cons (car lambdabody)
                                       (add-implicit-block name (cdr lambdabody))
                                 )
                                 fenvcons
                             ) )
                           namelist lambdanamelist lambdabodylist fenvconslist
                 ) )
                 (anodelist
                   (mapcar #'(lambda (fnode var)
                               (c-fnode-function fnode (cdr (var-stackz var)))
                             )
                           fnodelist varlist
                 ) )
                 (body-anode ; restliche Formen compilieren
                   (c-form `(PROGN ,@(cddr *form*)))
                ))
            ; die Variablen, zu denen die Funktion autonom war, werden nach-
            ; träglich zu Konstanten erklärt:
            (do ((varlistr varlist (cdr varlistr))
                 (fnodelistr fnodelist (cdr fnodelistr)))
                ((null varlistr))
              (let ((var (car varlistr))
                    (fnode (car fnodelistr)))
                (when (zerop (fnode-keyword-offset fnode))
                  ; Funktionsdefinition ist autonom
                  (setf (var-constantp var) t)
                  (setf (var-constant var) (new-const fnode))
            ) ) )
            (let* ((closurevars (checking-movable-var-list varlist anodelist))
                   (anode
                     (make-anode
                       :type 'LABELS
                       :sub-anodes `(,@anodelist ,body-anode)
                       :seclass (seclass-without
                                  (anodelist-seclass-or `(,@anodelist ,body-anode))
                                  varlist
                                )
                       :code `(,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                               ,@(mapcap #'c-bind-movable-var-anode varlist anodelist)
                               ,body-anode
                               (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                     )        )
                  ))
              (when closurevars
                (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
                (setf (first closuredummy-venvc)
                  (cons closurevars closuredummy-stackz)
              ) )
              (optimize-var-list varlist)
              anode
) ) ) ) ) ) )

; compiliere (CLOS:GENERIC-FLET ({genfundefs}*) {form}*)
(defun c-GENERIC-FLET ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (multiple-value-bind (namelist signlist formlist)
      (do ((fdefsr (second *form*) (cdr fdefsr))
           (L1 '())
           (L2 '())
           (L3 '()))
          ((null fdefsr) (values (nreverse L1) (nreverse L2) (nreverse L3)))
        (let ((fdef (car fdefsr)))
          (if (and (consp fdef) (function-name-p (car fdef)) (consp (cdr fdef)))
            (let ((name (first fdef)))
              (push name L1)
              (push (clos::defgeneric-lambdalist-callinfo 'clos:generic-flet name (second fdef))
                    L2
              )
              (push (clos::make-generic-function-form 'clos:generic-flet
                      name (second fdef) (cddr fdef) (vector *venv* *fenv*)
                    )
                    L3
            ) )
            (err-syntax 'CLOS:GENERIC-FLET fdef)
      ) ) )
    ; namelist = Liste der Namen,
    ; signlist = Liste der Signaturen der generischen Funktionen,
    ; formlist = Liste der Konstruktor-Formen der generischen Funktionen.
    (let ((oldstackz *stackz*)
          (*stackz* *stackz*)
          (*venvc* *venvc*)
          (*venv* *venv*))
      (push 0 *stackz*) (push nil *venvc*) ; Platz für Closure-Dummyvar
      (let ((closuredummy-stackz *stackz*)
            (closuredummy-venvc *venvc*))
        (multiple-value-bind (varlist anodelist *fenv*)
            (do ((namelistr namelist (cdr namelistr))
                 (signlistr signlist (cdr signlistr))
                 (formlistr formlist (cdr formlistr))
                 (varlist '())
                 (anodelist '())
                 (fenv '()))
                ((null namelistr)
                 (values (nreverse varlist) (nreverse anodelist)
                         (apply #'vector (nreverse (cons *fenv* fenv)))
                ))
              (push (car namelistr) fenv)
              (push (c-form (car formlistr) 'ONE) anodelist)
              (push 1 *stackz*)
              (let ((var (make-var :name (gensym) :specialp nil
                           :constantp nil
                           :usedp t :for-value-usedp t :really-usedp nil
                           :closurep nil ; später evtl. auf T gesetzt
                           :stackz *stackz* :venvc *venvc*
                   ))    )
                (push (cons (list* nil 'GENERIC (car signlistr)) var) fenv)
                (push var varlist)
            ) )
          (apply #'push-*venv* varlist) ; Hilfsvariablen aktivieren
          (let* ((body-anode ; restliche Formen compilieren
                   (c-form `(PROGN ,@(cddr *form*)))
                 )
                 (closurevars (checking-movable-var-list varlist anodelist))
                 (anode
                   (make-anode
                     :type 'CLOS:GENERIC-FLET
                     :sub-anodes `(,@anodelist ,body-anode)
                     :seclass (seclass-without
                                (anodelist-seclass-or `(,@anodelist ,body-anode))
                                varlist
                              )
                     :code `(,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                             ,@(mapcap #'c-bind-movable-var-anode varlist anodelist)
                             ,body-anode
                             (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                   )        )
                ))
            (when closurevars
              (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
              (setf (first closuredummy-venvc)
                (cons closurevars closuredummy-stackz)
            ) )
            (optimize-var-list varlist)
            anode
) ) ) ) ) )

; compiliere (CLOS:GENERIC-LABELS ({genfundefs}*) {form}*)
(defun c-GENERIC-LABELS ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (let ((oldstackz *stackz*)
        (*stackz* *stackz*)
        (*venvc* *venvc*)
        (*venv* *venv*))
    (push 0 *stackz*) (push nil *venvc*) ; Platz für Closure-Dummyvar
    (let ((closuredummy-stackz *stackz*)
          (closuredummy-venvc *venvc*))
      (multiple-value-bind (namelist varlist fenvconslist formlist)
          (do ((fdefsr (second *form*) (cdr fdefsr))
               (L1 '())
               (L2 '())
               (L3 '())
               (L4 '()))
              ((null fdefsr)
               (values (nreverse L1) (nreverse L2) (nreverse L3) (nreverse L4))
              )
            (let ((fdef (car fdefsr)))
              (if (and (consp fdef) (function-name-p (car fdef)) (consp (cdr fdef)))
                (let ((name (first fdef)))
                  (push name L1)
                  (push 1 *stackz*)
                  (push (make-var :name (gensym) :specialp nil
                                  :constantp nil
                                  :usedp t :for-value-usedp t :really-usedp nil
                                  :closurep nil ; später evtl. auf T gesetzt
                                  :stackz *stackz* :venvc *venvc*
                        )
                        L2
                  )
                  (push (cons
                          ; fdescr
                          (list* nil 'GENERIC
                                 (clos::defgeneric-lambdalist-callinfo 'clos:generic-labels name (second fdef))
                          )
                          ; Variable
                          (car L2)
                        )
                        L3
                  )
                  (push (clos::make-generic-function-form 'clos:generic-labels
                          name (second fdef) (cddr fdef) (vector *venv* *fenv*)
                        )
                        L4
                ) )
                (err-syntax 'CLOS:GENERIC-LABELS fdef)
          ) ) )
        ; namelist = Liste der Namen, varlist = Liste der Variablen,
        ; fenvconslist = Liste der Conses (fdescr . var) für *fenv*,
        ; formlist = Liste der Konstruktor-Formen der generischen Funktionen.
        (let ((*fenv* ; Funktionsnamen aktivieren
                (do ((namelistr namelist (cdr namelistr))
                     (fenvconslistr fenvconslist (cdr fenvconslistr))
                     (L nil))
                    ((null namelistr)
                     (push *fenv* L)
                     (apply #'vector (nreverse L))
                    )
                  (push (car namelistr) L)
                  (push (car fenvconslistr) L)
             )) )
          (apply #'push-*venv* varlist) ; Hilfsvariablen aktivieren
          (let* ((anodelist
                   (mapcar #'(lambda (form) (c-form form 'ONE)) formlist)
                 )
                 (body-anode ; restliche Formen compilieren
                   (c-form `(PROGN ,@(cddr *form*)))
                 )
                 (closurevars (checking-movable-var-list varlist anodelist))
                 (anode
                   (make-anode
                     :type 'CLOS:GENERIC-LABELS
                     :sub-anodes `(,@anodelist ,body-anode)
                     :seclass (seclass-without
                                (anodelist-seclass-or `(,@anodelist ,body-anode))
                                varlist
                              )
                     :code `(,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                             ,@(mapcap #'c-bind-movable-var-anode varlist anodelist)
                             ,body-anode
                             (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                   )        )
                ))
            (when closurevars
              (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
              (setf (first closuredummy-venvc)
                (cons closurevars closuredummy-stackz)
            ) )
            (optimize-var-list varlist)
            anode
) ) ) ) ) )

) ; macrolet

; compiliere (MACROLET ({macrodef}*) {form}*)
(defun c-MACROLET (&optional (c #'c-form))
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (do ((L1 (second *form*) (cdr L1))
       (L2 '()))
      ((null L1)
       (push *fenv* L2)
       (let ((*fenv* (apply #'vector (nreverse L2)))) ; *fenv* erweitern
         (funcall c `(PROGN ,@(cddr *form*))) ; restliche Formen compilieren
      ))
    (let* ((macrodef (car L1))
           (name (car macrodef)))
      (push name L2)
      (push #+CLISP (sys::make-macro-expandercons macrodef)
            #-CLISP (cons 'SYSTEM::MACRO (make-macro-expander macrodef))
            L2
  ) ) )
)

; compiliere (SYMBOL-MACROLET ({symdef}*) {declaration}* {form}*)
(defun c-SYMBOL-MACROLET (&optional (c #'c-form))
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (multiple-value-bind (body-rest declarations)
      (parse-body (cddr *form*) nil (vector *venv* *fenv*))
    (let ((*denv* *denv*)
          (*venv* *venv*))
      (multiple-value-bind (*specials* *ignores* *ignorables* *readonlys*)
          (process-declarations declarations)
        ; Special-Variable auf *venv* pushen:
        (push-specials)
        ; Syntaxtest der Parameterliste:
        (multiple-value-bind (symbols expansions)
            (do ((L (second *form*) (cdr L))
                 (symbols nil)
                 (expansions nil))
                ((null L) (values (nreverse symbols) (nreverse expansions)))
              (let ((symdef (car L)))
                (if (and (consp symdef) (symbolp (car symdef))
                         (consp (cdr symdef)) (null (cddr symdef))
                    )
                  (progn (push (first symdef) symbols) (push (second symdef) expansions))
                  (catch 'c-error
                    (c-error (ENGLISH "Illegal syntax in SYMBOL-MACROLET: ~S")
                             symdef
            ) ) ) ) )
          (dolist (symbol symbols)
            (if (or (constantp symbol) (proclaimed-special-p symbol))
              (catch 'c-error
                (c-error (ENGLISH "~S: symbol ~S is declared special and must not be declared a macro")
                         'symbol-macrolet symbol
              ) )
              (if (member symbol *specials* :test #'eq)
                (catch 'c-error
                  (c-error (ENGLISH "~S: symbol ~S must not be declared SPECIAL and a macro at the same time")
                           'symbol-macrolet symbol
          ) ) ) ) )
          (setq *venv* ; *venv* erweitern
            (apply #'vector
              (nconc (mapcan #'(lambda (sym expansion) (list sym (make-symbol-macro expansion)))
                             symbols expansions
                     )
                     (list *venv*)
          ) ) )
          (funcall c `(PROGN ,@body-rest)) ; restliche Formen compilieren
) ) ) ) )

; compiliere (EVAL-WHEN ({situation}*) {form}*)
(defun c-EVAL-WHEN (&optional (c #'c-form))
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (let ((load-flag nil)
        (compile-flag nil)
        (compile-once-only nil))
    (dolist (situation (second *form*))
      (case situation
        ((LOAD :LOAD-TOPLEVEL) (setq load-flag t))
        ((COMPILE :COMPILE-TOPLEVEL) (setq compile-flag t))
        ((EVAL :EXECUTE))
        (COMPILE-ONCE-ONLY (setq compile-once-only t))
        (T (cond ((equal situation '(NOT EVAL)) (setq load-flag t compile-flag t))
                 ((or (equal situation '(NOT COMPILE))
                      (equal situation '(NOT :COMPILE-TOPLEVEL)))
                  (setq load-flag t))
                 (t (c-error (ENGLISH "EVAL-WHEN situation must be EVAL or LOAD or COMPILE, but not ~S")
                             situation
    ) ) )  )     )  )
    (let ((form `(PROGN ,@(cddr *form*))))
      (if compile-flag
        (c-eval-when-compile form) ; ausführen und ins Lib-File schreiben
        (if compile-once-only
          (eval form) ; nur jetzt ausführen, nicht ins Lib-File schreiben
      ) )
      (funcall c (if load-flag form 'NIL))
) ) )

; compiliere (COND {clause}*)
(defun c-COND ()
  (test-list *form* 1)
  (c-form
    (let ((clauses (cdr *form*))) ; (COND . clauses) macroexpandieren
      (if (null clauses)
        'NIL
        (let ((clause (car clauses)))
          (if (atom clause)
            (c-error (ENGLISH "COND clause without test: ~S")
                     clause
            )
            (let ((test (car clause)))
              (if (cdr clause)
                `(IF ,test (PROGN ,@(cdr clause)) (COND ,@(cdr clauses)))
                `(OR ,test (COND ,@(cdr clauses)))
) ) ) ) ) ) ) )

; compiliere (CASE keyform {clause}*)
(defun c-CASE ()
  (test-list *form* 1)
  (let ((keyform (second *form*))
        (clauses (cddr *form*))
        ; clauses vereinfachen:
        (newclauses '())
        (allkeys '()))
    (let ((default-passed nil))
      (do ((clauses clauses))
          ((endp clauses))
        (let ((clause (pop clauses)))
          (if (atom clause)
            (c-error (ENGLISH "CASE clause without objects: ~S")
                     clause
            )
            (let ((keys (car clause)))
              (if default-passed ; war der Default schon da?
                (setq keys nil)
                (if (or (eq keys 'T) (eq keys 'OTHERWISE))
                  (progn
                    (when clauses
                      (catch 'c-error
                        (c-error (ENGLISH "~S: the ~S clause must be the last one: ~S")
                                 'case keys *form*
                    ) ) )
                    (setq keys 'T)
                    (setq default-passed t)
                  )
                  (let ((newkeys '()))
                    (dolist (key (if (listp keys) keys (list keys)))
                      (if (not (member key allkeys :test #'eql)) ; remove-duplicates
                        (progn (push key allkeys) (push key newkeys))
                        (c-style-warn (ENGLISH "Duplicate ~S label ~S : ~S")
                                      'case key *form*
                    ) ) )
                    (setq keys (nreverse newkeys))
              ) ) )
              (push (cons keys (cdr clause)) newclauses)
      ) ) ) )
      (unless default-passed (push '(T NIL) newclauses))
      (setq newclauses (nreverse newclauses))
      (setq allkeys (nreverse allkeys))
    )
    ; newclauses enthält jetzt keine doppelten keys, genau einmal T als keys,
    ; und allkeys ist die Menge aller Keys.
    (if (<= (length allkeys) 2) ; wenige Keys -> direkt EQL verwenden
      (let ((keyvar (gensym)))
        (labels ((ifify (clauses)
                   (if (null clauses)
                     'NIL
                     `(IF ,(let ((keys (caar clauses)))
                             (if (listp keys)
                               `(OR ,@(mapcar
                                        #'(lambda (key) `(EQL ,keyvar ',key))
                                        keys
                                )     )
                               'T ; keys = T, der Default-Fall
                           ) )
                        (PROGN ,@(cdar clauses))
                        ,(ifify (cdr clauses))
                      )
                )) )
          (c-form
            `(LET ((,keyvar ,keyform)) (PROGN ,keyvar ,(ifify newclauses)))
      ) ) )
      (let ((keyform-anode (c-form keyform 'ONE))
            (default-anode nil)
            (cases '())) ; Liste von Tripeln (keylist label anode)
        (dolist (clause newclauses)
          (if (car clause)
            (let ((anode (c-form `(PROGN ,@(cdr clause)))))
              (if (atom (car clause))
                (setq default-anode anode)
                (push (list (car clause) (make-label 'NIL) anode) cases)
            ) )
            (let ((*no-code* t)) (c-form `(PROGN ,@(cdr clause)) 'NIL))
        ) )
        (setq cases (nreverse cases))
        (if (anode-constantp keyform-anode)
          (let ((value (anode-constant-value keyform-anode)))
            (dolist (case cases default-anode)
              (when (member value (first case) :test #'eql)
                (return (third case))
          ) ) )
          (let ((default-label (make-label 'NIL))
                (end-label (make-label *for-value*))
                (test (if (every #'EQL=EQ allkeys) 'EQ 'EQL)))
            (make-anode
              :type 'CASE
              :sub-anodes `(,keyform-anode ,@(mapcar #'third cases) ,default-anode)
              :seclass
                (anodelist-seclass-or
                  `(,keyform-anode ,@(mapcar #'third cases) ,default-anode)
                )
              :code
                `(,keyform-anode
                  (JMPHASH
                    ,test
                    ,(mapcap ; Aliste (obji -> labeli)
                       #'(lambda (case)
                           (let ((label (second case)))
                             (mapcar #'(lambda (obj) (cons obj label))
                                     (first case)
                         ) ) )
                       cases
                     )
                    ,default-label
                    ,@(mapcar #'second cases) ; alle Labels, ohne Doppelte
                  )
                  ,@(mapcap
                      #'(lambda (case)
                          `(,(second case) ; Label
                            ,(third case) ; Anode
                            (JMP ,end-label)
                           )
                        )
                      cases
                    )
                  ,default-label
                  ,default-anode
                  ,end-label
                 )
          ) )
) ) ) ) )


;               ERSTER PASS :   M A C R O S

; compiliere (HANDLER-BIND ({(typespec handler)}*) {form}*)
; und  (SYS::%HANDLER-BIND ({(typespec handler)}*) {form}*)
(defun c-HANDLER-BIND ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (let ((body (cddr *form*))
        (types '())
        (handler-labels '())
        (handler-anodes '()))
    (dolist (clause (second *form*))
      (test-list clause 2 2)
      (let ((type (first clause))
            (handler (second clause)))
        (if (block try-subtypep
              (let ((*error-handler*
                      #'(lambda (&rest error-args)
                          (declare (ignore error-args))
                          (return-from try-subtypep nil)
                   ))   )
                (subtypep type `(OR ,@types))
            ) )
          ; Brauche diesen Handler nicht zu berücksichtigen
          (let ((*no-code* t) (*for-value* 'NIL)) (c-form handler))
          ; Der Handler ist eine Funktion mit dynamischem Extent.
          (let ((label (make-label 'ONE)))
            (push type types)
            (push label handler-labels)
            (push
              (let* ((*stackz* (cons 'ANYTHING *stackz*))
                     (oldstackz *stackz*)
                     (*venv* *venv*))
                ; Platz für die Funktion selbst:
                (push 1 *stackz*)
                (let* ((condition-sym (gensym))
                       (condition-anode
                         (make-anode :type 'CONDITION
                                     :sub-anodes '()
                                     :seclass '(T . NIL)
                                     :code '() ; vorher kommt (HANDLER-BEGIN)
                       ) )
                       (condition-var (bind-movable-var condition-sym condition-anode)))
                  (push-*venv* condition-var)
                  (let ((body-anode
                          (c-form `(SYS::%FUNCALL ,handler ,condition-sym) 'NIL)
                       ))
                    ; Überprüfen der Variablen (muss nicht in die Closure):
                    (checking-movable-var-list (list condition-var) (list condition-anode))
                    (let* ((codelist
                             `(,label
                               (HANDLER-BEGIN)
                               ,@(c-bind-movable-var-anode condition-var condition-anode)
                               ,body-anode
                               (UNWINDSP ,*stackz* ,*func*) ; ein (SKIPSP k1 k2)
                               (UNWIND ,*stackz* ,oldstackz NIL) ; ein (SKIP 2)
                               (RET)
                              )
                           )
                           (anode
                             (make-anode
                               :type 'HANDLER
                               :sub-anodes `(,body-anode)
                               :seclass '(T . T) ; eigentlich irrelevant
                               :stackz oldstackz
                               :code codelist
                          )) )
                      (optimize-var-list (list condition-var))
                      anode
              ) ) ) )
              handler-anodes
            )
    ) ) ) )
    (if (null types)
      (c-form `(PROGN ,@body))
      (progn
        (setq types (nreverse types))
        (setq handler-labels (nreverse handler-labels))
        (setq handler-anodes (nreverse handler-anodes))
        (let* ((label (make-label 'NIL))
               (oldstackz *stackz*)
               (*stackz* (cons 4 *stackz*)) ; HANDLER-Frame
               (body-anode (c-form `(PROGN ,@body))))
          (make-anode
            :type 'HANDLER-BIND
            :sub-anodes `(,body-anode ,@handler-anodes)
            :seclass (anodelist-seclass-or `(,body-anode ,@handler-anodes))
            :stackz oldstackz
            :code `((HANDLER-OPEN ,(new-const (coerce types 'vector)) ,*stackz* ,@handler-labels)
                    (JMP ,label)
                    ,@handler-anodes
                    ,label
                    ,body-anode
                    (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                   )
    ) ) ) )
) )

; compiliere (SYS::CONSTANT-EQL form1 form2 form3)
(defun c-CONSTANT-EQL ()
  (test-list *form* 4 4)
  (let ((form1 (second *form*))
        (form23 (cddr *form*)))
    (if (and *compiling-from-file*
             (c-constantp form1)
             (let ((value (c-constant-value form1)))
               (or (stringp value) (bit-vector-p value))
        )    )
      (c-form `(SYS::LOOSE-CONSTANT-EQL ,@form23))
      (c-form `(EQL ,@form23))
) ) )


;   ERSTER PASS :   I N L I N E - F U N K T I O N E N   (PRIMOPS)

; Funktionsaufrufe, die wie special forms behandelt werden:

; Erst FUNCALL bzw. SYS::%FUNCALL.

; (c-FUNCALL-NOTINLINE funform args) compiliert einen Funktionsaufruf
; (SYS::%FUNCALL funform . args),
; für den das STACK-Layout der Argumente nicht zur Compile-Zeit bestimmt
; werden kann.
(defun c-FUNCALL-NOTINLINE (funform args)
  (test-list args 0)
  (let* ((anode1 (c-form funform 'ONE))
         (*stackz* (cons 1 *stackz*)))
    (do ((formlistr args (cdr formlistr))
         #+COMPILER-DEBUG (anodelist (list anode1))
         (codelist (list '(FUNCALLP) anode1)))
        ((null formlistr)
         (push `(FUNCALL ,(length args)) codelist)
         (make-anode
           :type 'FUNCALL
           :sub-anodes (nreverse anodelist)
           :seclass '(T . T)
           :code (nreverse codelist)
        ))
      (let ((anode (c-form (car formlistr) 'ONE)))
        #+COMPILER-DEBUG (push anode anodelist)
        (push anode codelist)
      )
      (push '(PUSH) codelist)
      (push 1 *stackz*)
) ) )

; (c-FUNCALL-INLINE funform args applyargs lambdabody sameenv) compiliert einen
; Funktionsaufruf (SYS::%FUNCALL funform . args) bzw.
; (APPLY funform . args applyargs) [applyargs eine Liste aus einer Form],
; für den das STACK-Layout der Argumente zur Compile-Zeit bestimmt werden kann.
; sameenv gibt an, ob lambdabody im selben Environment oder im
; Top-Level-Environment zu betrachten ist.
(defun c-FUNCALL-INLINE (funform arglist applyarglist lambdabody sameenv)
  (test-list lambdabody 1)
  (multiple-value-bind (reqvar  optvar optinit optsvar  restvar
                        keyflag keyword keyvar keyinit keysvar allow-other-keys
                        auxvar auxinit)
      (analyze-lambdalist (pop lambdabody))
    (when (or keyflag keyword keyvar keyinit keysvar allow-other-keys)
      (compiler-error 'c-FUNCALL-INLINE)
    )
    (let ((r (length reqvar)) ; Anzahl der required-Argumente
          (s (length optvar)) ; Anzahl der optionalen Argumente
          (|t| (length arglist))) ; Anzahl der angegebenen Argumente
      (when (and (null restvar) (> |t| (+ r s)))
        ; zu viele Argumente angegeben. Wird beseitigt durch Einführung
        ; mehrerer zusätzlicher optionaler Argumente:
        (catch 'c-error
          (c-error (ENGLISH "Too many arguments to ~S")
                   funform
        ) )
        (dotimes (i (- |t| (+ r s)))
          (let ((var (gensym)))
            (setq optvar (append optvar (list var)))
            (setq optinit (append optinit (list nil)))
            (setq optsvar (append optsvar (list nil)))
            (incf s)
            (push `(DECLARE (IGNORE ,var)) lambdabody)
      ) ) )
      (when (and (null applyarglist) (< |t| r))
        ; zu wenige Argumente angegeben. Wird beseitigt durch Einführung
        ; zusätzlicher Argumente:
        (catch 'c-error
          (c-error (ENGLISH "Too few arguments to ~S")
                   funform
        ) )
        (setq arglist (append arglist (make-list (- r |t|) :initial-element nil)))
        (setq |t| r)
      )
      ; Nun ist (t>=r oder apply-arg da) und (t<=r+s oder &rest-Parameter da).
      (let ((oldstackz *stackz*)
            (oldvenv *venv*)
            (oldfenv *fenv*)
            (oldbenv *benv*)
            (oldgenv *genv*)
            (olddenv *denv*)
            (*stackz* *stackz*)
            (*venv* (and sameenv *venv*))
            (*venvc* *venvc*)
            (*fenv* (and sameenv *fenv*))
            (*benv* (and sameenv *benv*))
            (*genv* (and sameenv *genv*))
            (*denv* (if sameenv
                      *denv*
                      (cons `(INLINING ,funform)
                            (remove-if-not #'(lambda (declspec)
                                               (case (car declspec)
                                                 ((DECLARATION SYS::IN-DEFUN INLINING) t)
                                                 (t nil)
                                             ) )
                                           *denv*
           ))       ) )     )
        (multiple-value-bind (body-rest declarations)
            (parse-body lambdabody t (vector *venv* *fenv*))
          (let (*specials* *ignores* *ignorables* *readonlys*
                req-vars req-anodes req-stackzs
                opt-vars opt-anodes opt-stackzs ; optionale und svar zusammen!
                rest-vars rest-anodes rest-stackzs
                fixed-anodes fixed-stackz
                reqfixed-vars reqfixed-dummys reqfixed-stackzs
                optfixed-vars optfixed-dummys optfixed-anodes
                optsfixed-vars optsfixed-anodes optfixed-stackzs
                restfixed-vars restfixed-dummys restfixed-stackzs
                aux-vars aux-anodes
                closuredummy-stackz closuredummy-venvc
               )
            (multiple-value-setq (*specials* *ignores* *ignorables* *readonlys*)
              (process-declarations declarations)
            )
            ; Special-Variable auf *venv* pushen:
            (push-specials)
            (push 0 *stackz*) (push nil *venvc*) ; Platz für Closure-Dummyvar
            (setq closuredummy-stackz *stackz* closuredummy-venvc *venvc*)
            (flet ((finish-using-applyarg (reqvar optvar optinit optsvar restvar)
                     ; reqvar und optvar/optinit/optsvar sowie arglist sind schon
                     ; teilweise verkürzt. Zerlegen der weiteren Argumentliste
                     ; mittels UNLIST bzw. UNLIST*. Daher ein Stackaufbau mit
                     ; festem Aussehen, vgl. c-LAMBDABODY.
                     (setq fixed-anodes
                           (list
                             (let ((anode1 (let ((*venv* oldvenv)
                                                 (*fenv* oldfenv)
                                                 (*benv* oldbenv)
                                                 (*genv* oldgenv)
                                                 (*denv* olddenv))
                                             (c-form (first applyarglist) 'ONE)
                                   )       )
                                   (anode2 (c-unlist (not (eql restvar 0))
                                                     (+ (length reqvar) (length optvar))
                                                     (length optvar)
                                  ))       )
                               (make-anode
                                 :type 'APPLY-UNLIST
                                 :sub-anodes (list anode1 anode2)
                                 :seclass (anodes-seclass-or anode1 anode2)
                                 :code `(,anode1 ,anode2)
                     )     ) ) )
                     ; Stack-Dummy-Variable für die reqvar,optvar,restvar bilden:
                     (multiple-value-setq (reqfixed-vars reqfixed-dummys)
                       (process-fixed-var-list reqvar)
                     )
                     (multiple-value-setq (optfixed-vars optfixed-dummys)
                       (process-fixed-var-list optvar)
                     )
                     (multiple-value-setq (restfixed-vars restfixed-dummys)
                       (if (eql restvar 0)
                         (values '() '())
                         (process-fixed-var-list (list restvar))
                     ) )
                     (push 0 *stackz*) (setq fixed-stackz *stackz*)
                     ; Bindungen der required-Parameter aktivieren:
                     (setq reqfixed-stackzs (bind-req-vars reqfixed-vars))
                     ; Bindungen der optional-Parameter/svar aktivieren:
                     (multiple-value-setq (optfixed-anodes optfixed-stackzs optsfixed-vars optsfixed-anodes)
                       (bind-opt-vars optfixed-vars optfixed-dummys optinit optsvar)
                     )
                     ; Bindung des rest-Parameters aktivieren:
                     (unless (eql restvar 0)
                       (setq restfixed-stackzs (bind-rest-vars restfixed-vars))
                     )
                  ))
              (block main-args
                ; required-Parameter binden:
                (do ((reqvarr reqvar (cdr reqvarr)))
                    ((null reqvarr))
                  (if (null arglist) ; impliziert, dass apply-arg da
                    (return-from main-args
                      (finish-using-applyarg reqvarr optvar optinit optsvar restvar)
                    )
                    (let* ((form (pop arglist))
                           (anode (let ((*venv* oldvenv)
                                        (*fenv* oldfenv)
                                        (*benv* oldbenv)
                                        (*genv* oldgenv)
                                        (*denv* olddenv))
                                    (c-form form 'ONE)
                           )      )
                           (var (bind-movable-var (car reqvarr) anode)))
                      (push anode req-anodes)
                      (push var req-vars)
                      (push *stackz* req-stackzs)
                      (push-*venv* var)
                ) ) )
                ; optionale Parameter und Svars binden:
                (do ((optvarr optvar (cdr optvarr))
                     (optinitr optinit (cdr optinitr))
                     (optsvarr optsvar (cdr optsvarr)))
                    ((null optvarr))
                  (if (and applyarglist (null arglist))
                    (return-from main-args
                      (finish-using-applyarg '() optvarr optinitr optsvarr restvar)
                    )
                    (let* ((svar-init (not (null arglist))) ; = NIL oder T
                           (anode (if svar-init
                                    (progn
                                      (let ((*no-code* t))
                                        (c-form (car optinitr) 'NIL)
                                      )
                                      (let ((*venv* oldvenv)
                                            (*fenv* oldfenv)
                                            (*benv* oldbenv)
                                            (*genv* oldgenv)
                                            (*denv* olddenv))
                                        (c-form (pop arglist) 'ONE)
                                    ) )
                                    (c-form (car optinitr) 'ONE)
                           )      )
                           (var (bind-movable-var (car optvarr) anode)))
                      (push anode opt-anodes)
                      (push var opt-vars)
                      (push *stackz* opt-stackzs)
                      (push-*venv* var)
                      (unless (eql (car optsvarr) 0)
                        (let* ((anode (c-form svar-init 'ONE))
                               (var (bind-movable-var (car optsvarr) anode)))
                          (push anode opt-anodes)
                          (push var opt-vars)
                          (push *stackz* opt-stackzs)
                          (push-*venv* var)
                      ) )
                ) ) )
                (if (eql restvar 0)
                  ; weitere Argumente verbrauchen:
                  (when applyarglist
                    (return-from main-args
                      (finish-using-applyarg '() '() '() '() restvar)
                  ) )
                  ; Rest-Parameter binden:
                  (let* ((form (if applyarglist
                                 (if arglist `(LIST* ,@arglist ,@applyarglist) (first applyarglist))
                                 (if arglist `(LIST ,@arglist) 'NIL)
                         )     )
                         (anode (let ((*venv* oldvenv)
                                      (*fenv* oldfenv)
                                      (*benv* oldbenv)
                                      (*genv* oldgenv)
                                      (*denv* olddenv))
                                  (c-form form 'ONE)
                         )      )
                         (var (bind-movable-var restvar anode)))
                    (push anode rest-anodes)
                    (push var rest-vars)
                    (push *stackz* rest-stackzs)
                    (push-*venv* var)
                ) )
                (push 0 *stackz*) (setq fixed-stackz *stackz*)
            ) )
            (setq req-vars (nreverse req-vars))
            (setq req-anodes (nreverse req-anodes))
            (setq req-stackzs (nreverse req-stackzs))
            (setq opt-vars (nreverse opt-vars))
            (setq opt-anodes (nreverse opt-anodes))
            (setq opt-stackzs (nreverse opt-stackzs))
            ; Bindungen der Aux-Variablen aktivieren:
            (multiple-value-setq (aux-vars aux-anodes)
              (bind-aux-vars auxvar auxinit)
            )
            (let* ((body-anode (c-form `(PROGN ,@body-rest)))
                   ; Überprüfen der Variablen:
                   (varlist
                     (append req-vars opt-vars rest-vars
                             reqfixed-vars optfixed-vars optsfixed-vars restfixed-vars
                             aux-vars
                   ) )
                   (closurevars
                     (append
                       (checking-movable-var-list req-vars req-anodes)
                       (checking-movable-var-list opt-vars opt-anodes)
                       (checking-movable-var-list rest-vars rest-anodes)
                       (checking-fixed-var-list reqfixed-vars)
                       (checking-fixed-var-list optfixed-vars)
                       (checking-movable-var-list optsfixed-vars optsfixed-anodes)
                       (checking-fixed-var-list restfixed-vars)
                       (checking-movable-var-list aux-vars aux-anodes)
                   ) )
                   (codelist
                     `(,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                       ,@(let ((*stackz* fixed-stackz))
                           (c-parallel-bind-movable-var-anode
                             (append req-vars    opt-vars    rest-vars   )
                             (append req-anodes  opt-anodes  rest-anodes )
                             (append req-stackzs opt-stackzs rest-stackzs)
                             fixed-anodes
                         ) )
                       ,@(mapcap #'c-bind-fixed-var reqfixed-vars reqfixed-dummys reqfixed-stackzs)
                       ,@(c-bind-with-svars optfixed-vars optfixed-dummys optsfixed-vars optfixed-anodes optsfixed-anodes optfixed-stackzs)
                       ,@(mapcap #'c-bind-fixed-var restfixed-vars restfixed-dummys restfixed-stackzs)
                       ,@(mapcap #'c-bind-movable-var-anode aux-vars aux-anodes)
                       ,body-anode
                       (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                   )  )
                   (anode
                     (make-anode
                       :type 'FUNCALL
                       :sub-anodes
                         `(,@req-anodes ,@opt-anodes ,@rest-anodes
                           ,@fixed-anodes ,@optfixed-anodes ,@(remove nil optsfixed-anodes)
                           ,@aux-anodes ,body-anode
                          )
                       :seclass
                         (seclass-without
                           (anodelist-seclass-or
                             `(,@req-anodes ,@opt-anodes ,@rest-anodes
                               ,@fixed-anodes ,@optfixed-anodes ,@(remove nil optsfixed-anodes)
                               ,@aux-anodes ,body-anode
                           )  )
                           varlist
                         )
                       :stackz oldstackz
                       :code codelist
                  )) )
              (when closurevars
                (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
                (setf (first closuredummy-venvc)
                  (cons closurevars closuredummy-stackz)
              ) )
              (optimize-var-list varlist)
              anode
) ) ) ) ) ) )

; compiliert (fun {form}*), wobei fun eine lokale Funktion ist.
; fdescr die zugehörige Information aus *fenv*.
(defun c-LOCAL-FUNCTION-CALL (fun fdescr args)
  ; (test-list args 0) ; das erledigt gleich (test-argument-syntax ...)
  ; Aufruf-Spezifikation holen:
  (multiple-value-bind (req opt rest-flag key-flag keylist allow-flag)
      (fdescr-signature fdescr)
    (case (test-argument-syntax
            args nil fun req opt rest-flag key-flag keylist allow-flag
          )
      ((NO-KEYS STATIC-KEYS)
       ; Aufruf INLINE
       (c-DIRECT-FUNCTION-CALL
         args nil fun req opt rest-flag key-flag keylist
         nil ; kein SUBR-, sondern Cclosure-Aufruf
         (cclosure-call-code-producer fun (car fdescr) req opt rest-flag key-flag keylist)
      ))
      (t (c-FUNCALL-NOTINLINE `(FUNCTION ,fun) args))
) ) )

; (c-FUNCTION-CALL funform arglist) compiliert einen Funktionsaufruf
; (SYS::%FUNCALL funform . arglist).
(defun c-FUNCTION-CALL (funform arglist)
  (setq funform (macroexpand-form funform))
  (when (inline-callable-function-lambda-p funform (length arglist))
    ; Aufruf eines Lambda-Ausdrucks INLINE möglich
    (return-from c-FUNCTION-CALL
      (c-FUNCALL-INLINE funform arglist nil (cdr (second funform)) t)
  ) )
  (when (and (consp funform) (eq (first funform) 'COMPLEMENT)
             (consp (rest funform)) (null (cddr funform))
             (not (fenv-search 'COMPLEMENT)) (not (declared-notinline 'COMPLEMENT))
             (not (fenv-search 'NOT))
        )
    ; (complement fn) --> (let ((f fn)) ... #'(lambda (&rest args) (not (apply f args))) ...)
    (return-from c-FUNCTION-CALL
      (c-form `(NOT (SYS::%FUNCALL ,(second funform) ,@arglist)))
  ) )
  (when (and (consp funform) (eq (first funform) 'CONSTANTLY)
             (consp (rest funform)) (null (cddr funform))
             (not (fenv-search 'CONSTANTLY)) (not (declared-notinline 'CONSTANTLY))
        )
    ; (constantly obj) --> (let ((o obj)) ... #'(lambda (&rest args) (declare (ignore args)) o) ...)
    (return-from c-FUNCTION-CALL
      (c-form `(PROG1 ,(second funform) ,@arglist))
  ) )
  (when (and (consp funform) (eq (first funform) 'FUNCTION)
             ; Ausdrücke der Form (FUNCTION ...) dürfen zu beliebigem
             ; Zeitpunkt ausgewertet werden, also ist
             ; (SYS::%FUNCALL (FUNCTION fun) . arglist)  äquivalent zu
             ; (fun . arglist).
             (consp (rest funform)) (function-name-p (second funform)) ; vorerst nur #'sym, sonst Endlosschleife!
        )
    (return-from c-FUNCTION-CALL
      (progn
        (test-list funform 2 2)
        (c-form `(,(second funform) ,@arglist)) ; genauer aufschlüsseln, vgl. c-FUNCTION ??
  ) ) )
  ; Aufruf NOTINLINE
  (c-FUNCALL-NOTINLINE funform arglist)
)

(defun c-FUNCALL ()
  (test-list *form* 2)
  (c-FUNCTION-CALL (second *form*) (cddr *form*))
)

(defun c-APPLY ()
  (test-list *form* 3)
  (let* ((funform (second *form*))
         (arglist (cddr *form*))
         (n (1- (length arglist)))) ; Mindestanzahl Argumente
    (setq funform (macroexpand-form funform))
    (when (inline-callable-function-lambda-p funform n t)
      ; Aufruf eines Lambda-Ausdrucks INLINE möglich
      (return-from c-APPLY
        (c-FUNCALL-INLINE funform (butlast arglist) (last arglist) (cdr (second funform)) t)
    ) )
    (when (and (consp funform) (eq (first funform) 'COMPLEMENT)
               (consp (rest funform)) (null (cddr funform))
               (not (fenv-search 'COMPLEMENT)) (not (declared-notinline 'COMPLEMENT))
               (not (fenv-search 'NOT))
          )
      ; (complement fn) --> (let ((f fn)) ... #'(lambda (&rest args) (not (apply f args))) ...)
      (return-from c-APPLY
        (c-form `(NOT (APPLY ,(second funform) ,@arglist)))
    ) )
    (when (and (consp funform) (eq (first funform) 'CONSTANTLY)
               (consp (rest funform)) (null (cddr funform))
               (not (fenv-search 'CONSTANTLY)) (not (declared-notinline 'CONSTANTLY))
          )
      ; (constantly obj) --> (let ((o obj)) ... #'(lambda (&rest args) (declare (ignore args)) o) ...)
      (return-from c-APPLY
        (c-form `(PROG1 ,(second funform) ,@arglist))
    ) )
    (when (and (consp funform) (eq (first funform) 'FUNCTION)
               ; Ausdrücke der Form (FUNCTION ...) dürfen zu beliebigem
               ; Zeitpunkt ausgewertet werden.
               (consp (rest funform)) (function-name-p (second funform))
          )
      (let ((fun (second funform)))
        (test-list funform 2 2)
        (unless (declared-notinline fun) ; darf fun INLINE genommen werden?
          (flet ((c-LOCAL-APPLY (fdescr)
                   (multiple-value-bind (req opt rest-flag key-flag keylist allow-flag)
                       (fdescr-signature fdescr)
                     (unless key-flag
                       ; ohne Keyword-Argumente
                       (when (eq (test-argument-syntax (butlast arglist) (last arglist)
                                   fun req opt rest-flag key-flag keylist allow-flag
                                 )
                               'NO-KEYS
                             )
                         ; Syntax stimmt -> Aufruf INLINE
                         (return-from c-APPLY
                           (c-DIRECT-FUNCTION-CALL (butlast arglist) (last arglist)
                             fun req opt rest-flag key-flag keylist
                             nil ; kein SUBR-, sondern Cclosure-Aufruf
                             (cclosure-call-code-producer fun (car fdescr) req opt rest-flag key-flag keylist)
                )) ) ) ) ) )
            (multiple-value-bind (a b c) (fenv-search fun)
              (declare (ignore b))
              ; (APPLY #'fun . args) kann evtl. vereinfacht werden
              (case a
                ((NIL) ; globale Funktion
                  (unless (and (symbolp fun) (or (special-operator-p fun) (macro-function fun))) ; Special-Form oder globaler Macro ?
                    (when (and (equal fun (fnode-name *func*))
                               (member `(SYS::IN-DEFUN ,fun) *denv* :test #'equal)
                          )
                      ; rekursiver Aufruf der aktuellen globalen Funktion
                      (c-LOCAL-APPLY (cons *func* nil))
                    )
                    (let ((inline-lambdabody
                            (or (and *compiling-from-file*
                                     (cdr (assoc fun *inline-definitions* :test #'equal))
                                )
                                (get (get-funname-symbol fun) 'sys::inline-expansion)
                         )) )
                      (if (and #| inline-lambdabody |#
                               (consp inline-lambdabody)
                               (inline-callable-function-lambda-p `(FUNCTION (LAMBDA ,@inline-lambdabody)) n t)
                          )
                        ; Aufruf einer globalen Funktion INLINE möglich
                        (return-from c-APPLY
                          (c-FUNCALL-INLINE fun (butlast arglist) (last arglist) inline-lambdabody nil)
                ) ) ) ) )
                (LOCAL ; lokale Funktion
                  (c-LOCAL-APPLY c)
              ) )
    ) ) ) ) )
    ; Wenn keine der Optimierungen möglich war:
    (let* ((anode1 (c-form funform 'ONE))
           (*stackz* (cons 1 *stackz*)))
      (do ((formlistr arglist (cdr formlistr))
           #+COMPILER-DEBUG (anodelist (list anode1))
           (codelist (list '(APPLYP) anode1)))
          ((null formlistr)
           (push `(APPLY ,n) codelist)
           (make-anode
             :type 'APPLY
             :sub-anodes (nreverse anodelist)
             :seclass '(T . T)
             :code (nreverse codelist)
          ))
        (let ((anode (c-form (car formlistr) 'ONE)))
          #+COMPILER-DEBUG (push anode anodelist)
          (push anode codelist)
          (when (cdr formlistr)
            (push 1 *stackz*) (push '(PUSH) codelist)
    ) ) ) )
) )

(defun c-PLUS ()
  (test-list *form* 1)
  ; bilde Teilsumme der konstanten Argumente, Rest dann dazu:
  (let ((const-sum 0)
        (other-parts '())
        val
       )
    (dolist (form (cdr *form*))
      (setq form (macroexpand-form form))
      (if (and (c-constantp form) (numberp (setq val (c-constant-value form))))
        (setq const-sum (+ const-sum val))
        (push form other-parts)
    ) )
    (case (length other-parts)
      (0 ; nur konstante Summanden
         (c-form const-sum) ; Zahl const-sum wertet zu sich selbst aus
      )
      (1 ; nur ein variabler Summand
         (case const-sum
           (0 (c-form (first other-parts))) ; keine Addition nötig
           (+1 (c-form `(1+ ,(first other-parts))))
           (-1 (c-form `(1- ,(first other-parts))))
           (t (c-GLOBAL-FUNCTION-CALL-form `(+ ,const-sum ,@other-parts)))
      )  )
      (t (setq other-parts (nreverse other-parts))
         (unless (eql const-sum 0) (push const-sum other-parts))
         (c-GLOBAL-FUNCTION-CALL-form `(+ ,@other-parts))
) ) ) )

(defun c-MINUS ()
  (test-list *form* 2)
  (let ((unary-p (= (length *form*) 2)) ; unäres Minus oder nicht?
        (const-sum 0) ; Summe der konstanten Teile
        (first-part 0) ; zu addierende Form
        (other-parts '()) ; abzuziehende Formen
        val
       )
    (unless unary-p
      (let ((form (macroexpand-form (second *form*))))
        (if (and (c-constantp form) (numberp (setq val (c-constant-value form))))
          (setq const-sum val)
          (setq first-part form)
    ) ) )
    (dolist (form (if unary-p (cdr *form*) (cddr *form*)))
      (setq form (macroexpand-form form))
      (if (and (c-constantp form) (numberp (setq val (c-constant-value form))))
        (setq const-sum (- const-sum val))
        (push form other-parts)
    ) )
    (if (null other-parts)
      ; nichts zu subtrahieren
      (let ((*form* `(+ ,const-sum ,first-part))) (c-PLUS))
      ; etwas zu subtrahieren
      (c-GLOBAL-FUNCTION-CALL-form
        `(-
          ,@(if (eql first-part 0) ; variable zu addierende Form?
              (if (and (eql const-sum 0) (null (cdr other-parts)))
                '()
                `(,const-sum)
              )
              (if (eql const-sum 0)
                `(,first-part)
                `(,first-part ,(- const-sum))
            ) )
          ,@(nreverse other-parts)
         )
) ) ) )

(defun c-SVSTORE ()
  (test-list *form* 4 4)
  ; (sys::svstore arg1 arg2 arg3) -> (sys::%svstore arg3 arg1 arg2)
  (let ((arg1 (second *form*)) (arg2 (third *form*)) (arg3 (fourth *form*))
        (argvar1 (gensym)) (argvar2 (gensym)))
    (c-form
      `(LET* ((,argvar1 ,arg1) (,argvar2 ,arg2))
         (sys::%svstore ,arg3 ,argvar1 ,argvar2)
       )
) ) )

(defun c-EQ ()
  (test-list *form* 3 3)
  (let ((arg1 (macroexpand-form (second *form*)))
        (arg2 (macroexpand-form (third *form*))))
    (if (and (c-constantp arg1) (c-constantp arg2))
      (c-form `(QUOTE ,(eq (c-constant-value arg1) (c-constant-value arg2))))
      (progn
        (when (c-constantp arg1)
          (rotatef arg1 arg2) ; Besser arg2 konstant, damit JMPIFEQTO geht
        )
        (if (and (c-constantp arg2) (eq (c-constant-value arg2) 'NIL))
          (c-GLOBAL-FUNCTION-CALL-form `(NULL ,arg1))
          (c-GLOBAL-FUNCTION-CALL-form `(EQ ,arg1 ,arg2))
) ) ) ) )

; bei Symbolen, Fixnums und Characters ist EQL mit EQ gleichbedeutend
(defun EQL=EQ (x)
  (or (symbolp x)
      (and (integerp x) (<= (integer-length x) 24))
      ; Note: Using (fixnump x) here would not generate portable code.
      ; The minimum fixnum length across architectures in clisp is 24 bits.
      (characterp x)
) )

(defun c-EQL ()
  (test-list *form* 3 3)
  (let ((arg1 (macroexpand-form (second *form*)))
        (arg2 (macroexpand-form (third *form*))))
    (cond ((and (c-constantp arg1) (c-constantp arg2))
           (c-form `(QUOTE ,(eql (c-constant-value arg1) (c-constant-value arg2))))
          )
          ((or (and (c-constantp arg1) (EQL=EQ (c-constant-value arg1)))
               (and (c-constantp arg2) (EQL=EQ (c-constant-value arg2)))
           )
           (let ((*form* `(EQ ,arg1 ,arg2))) (c-EQ))
          )
          (t (c-GLOBAL-FUNCTION-CALL-form `(EQL ,arg1 ,arg2)))
) ) )

; bei Symbolen, Zahlen und Characters ist EQUAL mit EQL gleichbedeutend
(defun EQUAL=EQL (x) (or (symbolp x) (numberp x) (characterp x)))

(defun c-EQUAL ()
  (test-list *form* 3 3)
  (let ((arg1 (macroexpand-form (second *form*)))
        (arg2 (macroexpand-form (third *form*))))
    (cond ((or (and (c-constantp arg1) (EQUAL=EQL (c-constant-value arg1)))
               (and (c-constantp arg2) (EQUAL=EQL (c-constant-value arg2)))
           )
           (let ((*form* `(EQL ,arg1 ,arg2))) (c-EQL))
          )
          (t (c-GLOBAL-FUNCTION-CALL-form `(EQUAL ,arg1 ,arg2)))
) ) )

; Bildet den inneren Teil einer MAPCAR/MAPC/MAPCAN/MAPCAP-Expansion
(defun c-MAP-on-CARs-inner (innerst-fun blockname restvars &optional (itemvars '()))
  (if (null restvars)
    (funcall innerst-fun (nreverse itemvars))
    (let ((restvar (car restvars))
          (itemvar (gensym)))
      `(IF (CONSP ,restvar)
         (LET ((,itemvar (CAR ,restvar)))
           ,(c-MAP-on-CARs-inner innerst-fun blockname (cdr restvars) (cons itemvar itemvars))
         )
         (RETURN-FROM ,blockname)
) ) )  )

; Bildet eine MAPCAR/MAPCAN/MAPCAP-Expansion
(defun c-MAP-on-CARs (adjoin-fun funform forms)
  (let ((erg (gensym))
        (blockname (gensym))
        (restvars
          (mapcar #'(lambda (form) (declare (ignore form)) (gensym)) forms)
        )
        (tag (gensym)))
    `(LET ((,erg NIL))
       (BLOCK ,blockname
         (LET* ,(mapcar #'list restvars forms)
           (TAGBODY ,tag
             ,(c-MAP-on-CARs-inner
                #'(lambda (itemvars)
                    `(SETQ ,erg (,adjoin-fun (SYS::%FUNCALL ,funform ,@itemvars) ,erg))
                  )
                blockname
                restvars
              )
             (SETQ ,@(mapcap #'(lambda (restvar)
                                 `(,restvar (CDR ,restvar))
                               )
                             restvars
             )       )
             (GO ,tag)
       ) ) )
       (SYS::LIST-NREVERSE ,erg)
) )  )

; Bildet eine MAPLIST/MAPCON/MAPLAP-Expansion
(defun c-MAP-on-LISTs (adjoin-fun funform forms)
  (let ((erg (gensym))
        (blockname (gensym))
        (restvars
          (mapcar #'(lambda (form) (declare (ignore form)) (gensym)) forms)
        )
        (tag (gensym)))
    `(LET ((,erg NIL))
       (BLOCK ,blockname
         (LET* ,(mapcar #'list restvars forms)
           (TAGBODY ,tag
             (IF (OR ,@(mapcar #'(lambda (restvar) `(ATOM ,restvar)) restvars))
               (RETURN-FROM ,blockname)
             )
             (SETQ ,erg (,adjoin-fun (SYS::%FUNCALL ,funform ,@restvars) ,erg))
             (SETQ ,@(mapcap #'(lambda (restvar)
                                 `(,restvar (CDR ,restvar))
                               )
                             restvars
             )       )
             (GO ,tag)
       ) ) )
       (SYS::LIST-NREVERSE ,erg)
) )  )

(defun c-MAPC ()
  (test-list *form* 3)
  (let ((funform (macroexpand-form (second *form*))))
    (if (inline-callable-function-p funform (length (cddr *form*)))
      (c-form
        (let* ((tempvar (gensym))
               (forms (cons tempvar (cdddr *form*)))
               (blockname (gensym))
               (restvars
                 (mapcar #'(lambda (form) (declare (ignore form)) (gensym)) forms)
               )
               (tag (gensym)))
          `(LET ((,tempvar ,(third *form*)))
             (BLOCK ,blockname
               (LET* ,(mapcar #'list restvars forms)
                 (TAGBODY ,tag
                   ,(c-MAP-on-CARs-inner
                      #'(lambda (itemvars) `(SYS::%FUNCALL ,funform ,@itemvars))
                      blockname
                      restvars
                    )
                   (SETQ ,@(mapcap #'(lambda (restvar)
                                       `(,restvar (CDR ,restvar))
                                     )
                                   restvars
                   )       )
                   (GO ,tag)
             ) ) )
             ,tempvar
      ) )  )
      (c-GLOBAL-FUNCTION-CALL-form `(MAPC ,funform ,@(cddr *form*)))
) ) )

(defun c-MAPL ()
  (test-list *form* 3)
  (let ((funform (macroexpand-form (second *form*))))
    (if (inline-callable-function-p funform (length (cddr *form*)))
      (c-form
        (let* ((tempvar (gensym))
               (forms (cons tempvar (cdddr *form*)))
               (blockname (gensym))
               (restvars
                 (mapcar #'(lambda (form) (declare (ignore form)) (gensym)) forms)
               )
               (tag (gensym)))
          `(LET ((,tempvar ,(third *form*)))
             (BLOCK ,blockname
               (LET* ,(mapcar #'list restvars forms)
                 (TAGBODY ,tag
                   (IF (OR ,@(mapcar #'(lambda (restvar) `(ATOM ,restvar)) restvars))
                     (RETURN-FROM ,blockname)
                   )
                   (SYS::%FUNCALL ,funform ,@restvars)
                   (SETQ ,@(mapcap #'(lambda (restvar)
                                       `(,restvar (CDR ,restvar))
                                     )
                                   restvars
                   )       )
                   (GO ,tag)
             ) ) )
             ,tempvar
      ) )  )
      (c-GLOBAL-FUNCTION-CALL-form `(MAPL ,funform ,@(cddr *form*)))
) ) )

(defun c-MAPCAR ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPC ,@(cdr *form*)))) (c-MAPC))
    (let ((funform (macroexpand-form (second *form*)))
          (forms (cddr *form*)))
      (if (inline-callable-function-p funform (length forms))
        (c-form (c-MAP-on-CARs 'CONS funform forms))
        (c-GLOBAL-FUNCTION-CALL-form `(MAPCAR ,funform ,@forms))
) ) ) )

(defun c-MAPLIST ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPL ,@(cdr *form*)))) (c-MAPL))
    (let ((funform (macroexpand-form (second *form*)))
          (forms (cddr *form*)))
      (if (inline-callable-function-p funform (length forms))
        (c-form (c-MAP-on-LISTs 'CONS funform forms))
        (c-GLOBAL-FUNCTION-CALL-form `(MAPLIST ,funform ,@forms))
) ) ) )

(defun c-MAPCAN ()
  (test-list *form* 3)
  (let ((funform (macroexpand-form (second *form*)))
        (forms (cddr *form*)))
    (if (inline-callable-function-p funform (length forms))
      (c-form (c-MAP-on-CARs 'NRECONC funform forms))
      (c-GLOBAL-FUNCTION-CALL-form `(MAPCAN ,funform ,@forms))
) ) )

(defun c-MAPCON ()
  (test-list *form* 3)
  (let ((funform (macroexpand-form (second *form*)))
        (forms (cddr *form*)))
    (if (inline-callable-function-p funform (length forms))
      (c-form (c-MAP-on-LISTs 'NRECONC funform forms))
      (c-GLOBAL-FUNCTION-CALL-form `(MAPCON ,funform ,@forms))
) ) )

(defun c-MAPCAP ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPC ,@(cdr *form*)))) (c-MAPC))
    (let ((funform (macroexpand-form (second *form*)))
          (forms (cddr *form*)))
      (if (inline-callable-function-p funform (length forms))
        (c-form (c-MAP-on-CARs 'REVAPPEND funform forms))
        (c-GLOBAL-FUNCTION-CALL-form `(MAPCAP ,funform ,@forms))
) ) ) )

(defun c-MAPLAP ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPL ,@(cdr *form*)))) (c-MAPL))
    (let ((funform (macroexpand-form (second *form*)))
          (forms (cddr *form*)))
      (if (inline-callable-function-p funform (length forms))
        (c-form (c-MAP-on-LISTs 'REVAPPEND funform forms))
        (c-GLOBAL-FUNCTION-CALL-form `(MAPLAP ,funform ,@forms))
) ) ) )

;; c-TYPEP vgl. TYPEP in type.lsp
(defun c-TYPEP () ; vgl. TYPEP in type.lsp
  (test-list *form* 3 3)
  (let ((objform (second *form*))
        (typeform (macroexpand-form (third *form*))))
    (when (c-constantp typeform)
      (let ((type (c-constant-value typeform)) h)
        (cond ((symbolp type)
                (cond ; Test auf Property TYPE-SYMBOL:
                      ((setq h (assoc type c-typep-alist1))
                        (setq h (cdr h))
                        (return-from c-TYPEP
                          (c-GLOBAL-FUNCTION-CALL-form `(,h ,objform))
                      ) )
                      ((setq h (assoc type c-typep-alist2))
                        (setq h (cdr h))
                        (return-from c-TYPEP
                          (let ((*form* `(,h ,objform)))
                            (c-FUNCALL-INLINE
                              (symbol-suffix '#:TYPEP (symbol-name type))
                              (list objform)
                              nil
                              h
                              nil
                      ) ) ) )
                      ; Test auf Property TYPE-LIST:
                      ((setq h (assoc type c-typep-alist3))
                        (setq h (cdr h))
                        (let* ((objvar (gensym))
                               (testform (funcall h objvar))
                               (lambdabody `((,objvar) ,testform)))
                          (return-from c-TYPEP
                            (let ((*form* `((lambda ,@lambdabody) ,objform)))
                              (c-FUNCALL-INLINE
                                (symbol-suffix '#:TYPEP (symbol-name type))
                                (list objform)
                                nil
                                lambdabody
                                nil
                      ) ) ) ) )
                      #+CLISP ; Test auf Property DEFTYPE-EXPANDER:
                      ((setq h (get type 'SYS::DEFTYPE-EXPANDER))
                        (return-from c-TYPEP
                          (c-form `(TYPEP ,objform ',(funcall h (list type))))
                      ) )
                      #+CLISP ; Test auf Property DEFSTRUCT-DESCRIPTION:
                      ((get type 'SYS::DEFSTRUCT-DESCRIPTION)
                        (return-from c-TYPEP
                          (c-form `(SYS::%STRUCTURE-TYPE-P ',type ,objform))
                      ) )
                      #+CLISP ; Test auf Property CLOS::CLOSCLASS:
                      ((and (setq h (get type 'CLOS::CLOSCLASS)) (clos::class-p h)
                            (eq (clos:class-name h) type)
                       )
                        (return-from c-TYPEP
                          (c-form `(CLOS::SUBCLASSP (CLOS:CLASS-OF ,objform)
                                     (LOAD-TIME-VALUE (CLOS:FIND-CLASS ',type))
                                   )
                      ) ) )
              ) )
              ((and (consp type) (symbolp (first type)))
                (catch 'c-TYPEP
                  (cond ((and (eq (first type) 'SATISFIES) (eql (length type) 2))
                          (let ((fun (second type)))
                            (unless (symbolp (second type))
                              (c-warn (ENGLISH "~S: argument to SATISFIES must be a symbol: ~S")
                                      'typep (second type)
                              )
                              (throw 'c-TYPEP nil)
                            )
                            (return-from c-TYPEP
                              (c-GLOBAL-FUNCTION-CALL-form `(,fun ,objform))
                        ) ) )
                        ((eq (first type) 'MEMBER)
                          (return-from c-TYPEP
                            (let ((*form* `(CASE ,objform (,(rest type) T) (t NIL))))
                              (c-CASE)
                        ) ) )
                        ((and (eq (first type) 'EQL) (eql (length type) 2))
                          (return-from c-TYPEP
                            (let ((*form* `(EQL ,objform ',(second type))))
                              (c-EQL)
                        ) ) )
                        ((and (eq (first type) 'NOT) (eql (length type) 2))
                          (return-from c-TYPEP
                            (c-GLOBAL-FUNCTION-CALL-form
                              `(NOT (TYPEP ,objform ',(second type)))
                        ) ) )
                        ((or (eq (first type) 'AND) (eq (first type) 'OR))
                          (return-from c-TYPEP
                            (c-form
                              (let ((objvar (gensym)))
                                `(LET ((,objvar ,objform))
                                   (,(first type) ; AND oder OR
                                    ,@(mapcar #'(lambda (typei) `(TYPEP ,objvar ',typei)) (rest type))
                                 ) )
                        ) ) ) )
                        ((setq h (assoc (first type) c-typep-alist3))
                          (setq h (cdr h))
                          (let* ((objvar (gensym))
                                 (testform (apply h objvar (rest type)))
                                 (lambdabody `((,objvar) ,testform)))
                            (return-from c-TYPEP
                              (let ((*form* `((lambda ,@lambdabody) ,objform)))
                                (c-FUNCALL-INLINE
                                  (symbol-suffix '#:TYPEP (symbol-name (first type)))
                                  (list objform)
                                  nil
                                  lambdabody
                                  nil
                        ) ) ) ) )
              ) ) )
              ((and (clos::class-p type) (eq (get (clos:class-name type) 'CLOS::CLOSCLASS) type))
                (return-from c-TYPEP
                  (c-form `(CLOS::SUBCLASSP (CLOS:CLASS-OF ,objform)
                             (LOAD-TIME-VALUE (CLOS:FIND-CLASS ',(clos:class-name type)))
                           )
              ) ) )
             ;((sys::encodingp type) ...) ; not worth optimizing
    ) ) )
    (c-GLOBAL-FUNCTION-CALL-form `(TYPEP ,objform ,typeform))
) )

;; c-FORMAT vgl. FORMAT in format.lsp
(defun c-FORMAT ()
  (test-list *form* 3)
  ; Give a warning for the common error of forgotten destination.
  (let ((destination (second *form*)))
    (when (c-constantp destination)
      (let ((destination (c-constant-value destination)))
        (unless (or (null destination) (eq destination 'T)
                    (streamp destination)
                    (and (stringp destination)
                         (array-has-fill-pointer-p destination)))
          (c-error (ENGLISH "The ~S destination is invalid (not NIL or T or a stream or a string with fill-pointer): ~S")
                   (car *form*) destination)))))
  (if (and (stringp (third *form*)) (not (fenv-search 'FORMATTER)))
    ; Format-String zur Compile-Zeit vorkompilieren.
    (c-GLOBAL-FUNCTION-CALL-form
      `(FORMAT ,(second *form*) (FORMATTER ,(third *form*)) ,@(cdddr *form*))
    )
    (c-GLOBAL-FUNCTION-CALL 'FORMAT)
) )

;; c-REMOVE-IF, c-REMOVE-IF-NOT usw.
(macrolet ((c-seqop (op n)
             (let ((op-if (intern (concatenate 'string (string op) "-IF") *lisp-package*))
                   (op-if-not (intern (concatenate 'string (string op) "-IF-NOT") *lisp-package*))
                   (c-op-if (intern (concatenate 'string "C-" (string op) "-IF")))
                   (c-op-if-not (intern (concatenate 'string "C-" (string op) "-IF-NOT"))))
               `(progn
                  (defun ,c-op-if ()
                    (test-list *form* ,(+ 1 n))
                    (let ((pred-arg (macroexpand-form
                                      ,(case n (2 `(second *form*))
                                               (3 `(third *form*))
                                       )
                         ))         )
                      (if (and (consp pred-arg) (eq (first pred-arg) 'COMPLEMENT)
                               (consp (rest pred-arg)) (null (cddr pred-arg))
                               ; (op-if (complement fn) ...) --> (op-if-not fn ...)
                               (not (fenv-search 'COMPLEMENT)) (not (declared-notinline 'COMPLEMENT))
                               (not (fenv-search 'NOT))
                          )
                        (c-form ,(case n (2 `(list* ',op-if-not (second pred-arg) (cddr *form*)))
                                         (3 `(list* ',op-if-not (second *form*) (second pred-arg) (cdddr *form*)))
                                 )
                        )
                        (c-GLOBAL-FUNCTION-CALL ',op-if)
                  ) ) )
                  (defun ,c-op-if-not ()
                    (test-list *form* ,(+ 1 n))
                    (let ((pred-arg (macroexpand-form
                                      ,(case n (2 `(second *form*))
                                               (3 `(third *form*))
                                       )
                         ))         )
                      (if (and (consp pred-arg) (eq (first pred-arg) 'COMPLEMENT)
                               (consp (rest pred-arg)) (null (cddr pred-arg))
                               ; (op-if-not (complement fn) ...) --> (op-if fn ...)
                               (not (fenv-search 'COMPLEMENT))
                               (not (fenv-search 'NOT))
                          )
                        (c-form ,(case n (2 `(list* ',op-if (second pred-arg) (cddr *form*)))
                                         (3 `(list* ',op-if (second *form*) (second pred-arg) (cdddr *form*)))
                                 )
                        )
                        (c-GLOBAL-FUNCTION-CALL ',op-if-not)
                  ) ) )
               )
          )) )
  (c-seqop REMOVE 2)
  (c-seqop DELETE 2)
  (c-seqop SUBSTITUTE 3)
  (c-seqop NSUBSTITUTE 3)
  (c-seqop FIND 2)
  (c-seqop POSITION 2)
  (c-seqop COUNT 2)
  (c-seqop SUBST 3)
  (c-seqop NSUBST 3)
  (c-seqop MEMBER 2)
  (c-seqop ASSOC 2)
  (c-seqop RASSOC 2)
)

; Recognizes a constant byte specifier and returns it, or NIL.
(defun c-constant-byte-p (form)
  (cond ((c-constantp form)
          (setq form (c-constant-value form))
          (if (eq (type-of form) 'BYTE) form nil)
        )
        ((and (consp form)
              (eq (first form) 'BYTE)
              (consp (cdr form))
              (typep (second form) '(AND (INTEGER 0 *) FIXNUM))
              (consp (cddr form))
              (typep (third form) '(AND (INTEGER 0 *) FIXNUM))
              (null (cdddr form))
              (not (fenv-search 'BYTE))
              (not (declared-notinline 'BYTE))
         )
          ; no need to ignore errors, we have checked the arguments
          (byte (second form) (third form))
        )
        (t nil)
) )

(defun c-LDB ()
  (test-list *form* 3 3)
  (let ((arg1 (c-constant-byte-p (macroexpand-form (second *form*)))))
    (if arg1
      ; We optimize (ldb (byte size position) integer) only when position = 0,
      ; because when position > 0, the expression
      ; `(logand ,(1- (ash 1 size)) (ash integer ,(- position))
      ; is not better and causes more heap allocations than the original
      ; expression. The expression
      ; `(ash (logand ,(ash (1- (ash 1 size)) position) integer) ,(- position))
      ; is even worse.
      ; The "24" below is an arbitrary limit, to avoid huge integers in the code
      ; [e.g. for (ldb (byte 30000 0) x)]. In particular, we know that for
      ; size <= 24, (1- (ash 1 size)) is a posfixnum, which makes the LOGAND
      ; operation particularly efficient.
      (if (and (= (byte-position arg1) 0) (<= (byte-size arg1) 24))
        (c-GLOBAL-FUNCTION-CALL-form
          `(LOGAND ,(1- (ash 1 (byte-size arg1))) ,(third *form*))
        )
        (c-GLOBAL-FUNCTION-CALL-form
          `(LDB (QUOTE ,arg1) ,(third *form*))
        )
      )
      (c-GLOBAL-FUNCTION-CALL 'LDB)
) ) )

(defun c-LDB-TEST ()
  (test-list *form* 3 3)
  (let ((arg1 (c-constant-byte-p (macroexpand-form (second *form*)))))
    (if arg1
      ; The "24" below is an arbitrary limit, to avoid huge integers in the code
      ; [e.g. for (ldb-test (byte 30000 0) x)].
      (if (<= (+ (byte-size arg1) (byte-position arg1)) 24)
        (c-GLOBAL-FUNCTION-CALL-form
          `(LOGTEST ,(ash (1- (ash 1 (byte-size arg1))) (byte-position arg1)) ,(third *form*))
        )
        (c-GLOBAL-FUNCTION-CALL-form
          `(LDB-TEST (QUOTE ,arg1) ,(third *form*))
        )
      )
      (c-GLOBAL-FUNCTION-CALL 'LDB-TEST)
) ) )

(defun c-MASK-FIELD ()
  (test-list *form* 3 3)
  (let ((arg1 (c-constant-byte-p (macroexpand-form (second *form*)))))
    (if arg1
      ; We know that for size+position <= 24, (ash (1- (ash 1 size)) position)
      ; is a posfixnum, which makes the LOGAND operation more efficient than
      ; the MASK-FIELD operation.
      (if (<= (+ (byte-size arg1) (byte-position arg1)) 24)
        (c-GLOBAL-FUNCTION-CALL-form
          `(LOGAND ,(ash (1- (ash 1 (byte-size arg1))) (byte-position arg1)) ,(third *form*))
        )
        (c-GLOBAL-FUNCTION-CALL-form
          `(MASK-FIELD (QUOTE ,arg1) ,(third *form*))
        )
      )
      (c-GLOBAL-FUNCTION-CALL 'MASK-FIELD)
) ) )

(defun c-DPB ()
  (test-list *form* 4 4)
  (let ((arg2 (c-constant-byte-p (macroexpand-form (third *form*)))))
    (if arg2
      (c-GLOBAL-FUNCTION-CALL-form
        `(DPB ,(second *form*) (QUOTE ,arg2) ,(fourth *form*))
      )
      (c-GLOBAL-FUNCTION-CALL 'DPB)
) ) )

(defun c-DEPOSIT-FIELD ()
  (test-list *form* 4 4)
  (let ((arg2 (c-constant-byte-p (macroexpand-form (third *form*)))))
    (if arg2
      (c-GLOBAL-FUNCTION-CALL-form
        `(DEPOSIT-FIELD ,(second *form*) (QUOTE ,arg2) ,(fourth *form*))
      )
      (c-GLOBAL-FUNCTION-CALL 'DEPOSIT-FIELD)
) ) )



;                     Z W E I T E R   P A S S

; eine Tabelle von Paaren (fnode n).
; Jedes Paar zeigt an, dass im 3. Pass in der Konstanten Nummer n des
; funktionalen Objektes von fnode der dort stehende fnode durch das durch ihn
; erzeugte funktionale Objekt zu ersetzen ist.
(defvar *fnode-fixup-table*)

; macht aus dem ANODE-Baum zum fnode *func* ein funktionales Objekt:
(defun pass2 (*func*)
  (when (anode-p (fnode-code *func*)) ; falls 2. Pass noch nicht durchgeführt:
    ; erst den Code flachklopfen, optimieren und assemblieren:
    (let ((code-list (compile-to-LAP))) ; Code flachklopfen und in Stücke zerteilen,
                                        ; optimieren und zu einer Liste machen
      (when (fnode-gf-p *func*) (setq code-list (CONST-to-LOADV code-list))) ; evtl. CONSTs umwandeln
      (let ((SPdepth (SP-depth code-list))) ; Stackbedarf bestimmen
        (setq code-list (insert-combined-LAPs code-list)) ; kombinierte Operationen einführen
        (create-fun-obj *func* (assemble-LAP code-list) SPdepth) ; assemblieren und funkt. Objekt
    ) )
    ; dann die Sub-Funktionen durch den 2. Pass jagen
    (dolist (x (fnode-Consts *func*)) (if (fnode-p x) (pass2 x)))
) )

#|

pass2 ruft den 1. Schritt auf.

Nach dem 1. Schritt ist der Code in kleine Stücke aufgeteilt, jeweils von
einem Label bis zu einem Wegsprung (JMP, JMPCASE, JMPCASE1-TRUE, JMPCASE1-FALSE,
JMPHASH, RETURN-FROM, GO, RET, RETGF, THROW, BARRIER). Die Teile stecken
(jeweils als Liste in umgekehrter Reihenfolge, mit dem Label als letztem CDR)
im Vektor *code-parts*.
(symbol-value label) enthält eine Liste der Referenzen von label, und zwar in
der Form:
 - Index in *code-parts*, wenn die Referenz der entsprechende Wegsprung ist;
 - opcode sonst, wobei opcode der Befehl ist, in dem label auftritt.
Nach dem 1. Schritt enthält der Code nur noch Tags (Symbole) und Listen aus
Symbolen und Zahlen. Es darf daher mit SUBST und EQUAL gearbeitet werden.

Der 1. Schritt ruft, sobald er mit einem Stück fertig ist, den 2. Schritt
auf.

Dann ruft pass2 den 3. Schritt auf. Es handelt sich hier um Optimierungen,
die, wenn sie erfolgreich waren, weitere dieser Optimierungen aufrufen.

|#

#|
                             1. Schritt:
          Expansion von Code-Teilen, Aufteilen des Codes in Stücke

Verändert werden:

vorher                           nachher

(CONST const)                    (CONST n const)
(FCONST fnode)                   (CONST n), Fixup für 3. Pass merken
(BCONST block)                   (CONST n)
(GCONST tagbody)                 (CONST n)
(GET var venvc stackz)           (LOAD n) oder (LOADI k1 k2 n)
                                 oder (LOADC n m) oder (LOADIC k1 k2 n m)
                                 oder (LOADV k m) oder (GETVALUE n)
                                 oder (CONST n) oder (CONST n const)
(SET var venvc stackz)           (STORE n) oder (STOREI k1 k2 n)
                                 oder (STOREC n m) oder (STOREIC k1 k2 n m)
                                 oder (STOREV k m) oder (SETVALUE n)
(SETVALUE symbol)                (SETVALUE n)
(GETVALUE symbol)                (GETVALUE n)
(BIND const)                     (BIND n)
(UNWIND stackz1 stackz2 for-value) eine Folge von
                                 (SKIP n), (SKIPI k1 k2 n), (SKIPSP k1 k2),
                                 (VALUES0), (UNWIND-PROTECT-CLEANUP), (UNBIND1),
                                 (BLOCK-CLOSE), (TAGBODY-CLOSE)
(UNWINDSP stackz1 stackz2)       eine Folge von (SKIPSP k1 k2)
(JMPIF label)                    (JMPCASE label new-label) new-label
(JMPIFNOT label)                 (JMPCASE new-label label) new-label
(JMPIF1 label)                   (JMPCASE1-TRUE label new-label) new-label
(JMPIFNOT1 label)                (JMPCASE1-FALSE new-label label) new-label
(JMPHASH test ((obj1 . label1) ... (objm . labelm)) label . labels)
                                 (JMPHASH n ht label . labels)
                                 wobei ht = Hash-Tabelle (obji -> labeli) ist
(VENV venvc stackz)              (VENV) oder (NIL)
                                 oder (LOAD n) oder (LOADI k1 k2 n)
(COPY-CLOSURE fnode n)           (COPY-CLOSURE m n), Fixup für 3. Pass merken
(CALLP)                          gestrichen
(CALL k fun)                     (CALL k n)
(CALL0 fun)                      (CALL0 n)
(CALL1 fun)                      (CALL1 n)
(CALL2 fun)                      (CALL2 n)
(FUNCALLP)                       (PUSH)
(APPLYP)                         (PUSH)
(JMPIFBOUNDP var venvc stackz label)
                                 (JMPIFBOUNDP n label)
(BOUNDP var venvc stackz)        (BOUNDP n)
(BLOCK-OPEN const label)         (BLOCK-OPEN n label)
(RETURN-FROM const)              (RETURN-FROM n)
(RETURN-FROM block)              (RETURN-FROM n)
(RETURN-FROM block stackz)       (RETURN-FROM-I k1 k2 n)
(TAGBODY-OPEN const label1 ... labelm)
                                 (TAGBODY-OPEN n label1 ... labelm)
(GO const l)                     (GO n l)
(GO tagbody l)                   (GO n l)
(GO tagbody l stackz)            (GO-I k1 k2 n l)
(HANDLER-OPEN const stackz label1 ... labelm)
                                 (HANDLER-OPEN n v k label1 ... labelm)


unverändert bleiben:
(NIL)
(PUSH-NIL n)
(T)
(STORE n)
(UNBIND1)
(PROGV)
(PUSH)
(POP)
(RET)
(RETGF)
(JMP label)
(JSR m label)
(BARRIER)
(MAKE-VECTOR1&PUSH n)
(CALLS1 n)
(CALLS2 n)
(CALLSR m n)
(CALLC)
(CALLCKEY)
(FUNCALL n)
(APPLY n)
(PUSH-UNBOUND n)
(UNLIST n m)
(UNLIST* n m)
(VALUES0)
(VALUES1)
(STACK-TO-MV n)
(MV-TO-STACK)
(NV-TO-STACK n)
(MV-TO-LIST)
(LIST-TO-MV)
(MVCALLP)
(MVCALL)
(BLOCK-CLOSE)
(TAGBODY-CLOSE-NIL)
(TAGBODY-CLOSE)
(CATCH-OPEN label)
(CATCH-CLOSE)
(THROW)
(UNWIND-PROTECT-OPEN label)
(UNWIND-PROTECT-NORMAL-EXIT)
(UNWIND-PROTECT-CLOSE label)
(UNWIND-PROTECT-CLEANUP)
(HANDLER-BEGIN)
(NOT)
(EQ)
(CAR)
(CDR)
(CONS)
(ATOM)
(CONSP)
(SYMBOL-FUNCTION)
(SVREF)
(SVSET)
(LIST n)
(LIST* n)

Neue Operationen:

(JMP label boolvalue)            Sprung zu label, boolvalue beschreibt den 1.
                                 Wert: FALSE falls =NIL, TRUE falls /=NIL,
                                 NIL falls unbekannt.

(JMPCASE label1 label2)          Sprung zu label1, falls A0 /= NIL,
                                 bzw. zu label2, falls A0 = NIL.

(JMPCASE1-TRUE label1 label2)    Falls A0 /= NIL: Sprung nach label1, 1 Wert.
                                 Falls A0 = NIL: Sprung nach label2.

(JMPCASE1-FALSE label1 label2)   Falls A0 /= NIL: Sprung nach label1.
                                 Falls A0 = NIL: Sprung nach label2, 1 Wert.

(JMPTAIL m n label)              Verkleinerung des Stack-Frames von n auf m,
                                 dann Sprung zu label mit undefinierten Werten.

|#

; Ein Vektor mit Fill-Pointer, der die Codestücke enthält:
(defvar *code-parts*)

; Ein gleichlanger Vektor mit Fill-Pointer, der zu jedem Codestück eine
; "Position" enthält, wo das Stück am Ende landen soll (0 = ganz am Anfang,
; je höher, desto weiter hinten).
(defvar *code-positions*)

; Trägt eine Konstante in (fnode-consts *func*) ein und liefert deren Index n.
; value ist der Wert der Konstanten,
; form eine Form mit diesem Wert oder NIL,
; horizont = :value (dann ist form = NIL) oder :all oder :form.
(defun value-form-index (value form horizont &optional (func *func*))
  (let ((const-list (fnode-consts func))
        (forms-list (fnode-consts-forms func))
        (n (fnode-Consts-Offset func)))
    (if (null const-list)
      (progn
        (setf (fnode-consts func) (list value))
        (setf (fnode-consts-forms func) (list form))
        n
      )
      (loop
        (when (if (eq horizont ':form)
                (eql (car forms-list) form)
                ; Bei horizont = :value oder :all vergleichen wir nur value.
                (eql (car const-list) value)
              )
          (return n)
        )
        (incf n)
        (when (null (cdr const-list))
          (setf (cdr const-list) (list value))
          (setf (cdr forms-list) (list form))
          (return n)
        )
        (setq const-list (cdr const-list))
        (setq forms-list (cdr forms-list))
) ) ) )
(defun constvalue-index (value)
  (value-form-index value nil ':value)
)

; sucht eine Konstante in (fnode-Keywords *func*) und in (fnode-Consts *func*),
; trägt sie eventuell in (fnode-Consts *func*) ein. Liefert ihren Index n.
(defun kvalue-form-index (value form horizont &optional (func *func*))
  (when (and (not (eq horizont ':form)) (symbolp value)) ; nur bei Symbolen (früher: Keywords) lohnt sich die Suche
    (do ((n (fnode-Keyword-Offset func) (1+ n))
         (L (fnode-Keywords func) (cdr L)))
        ((null L))
      (if (eq (car L) value) (return-from kvalue-form-index n))
  ) )
  (value-form-index value form horizont func)
)
(defun kconstvalue-index (value)
  (kvalue-form-index value nil ':value)
)
(defun const-index (const)
  (if (and *compiling-from-file* (not (eq (const-horizont const) ':value)))
    (kvalue-form-index (const-value const) (const-form const) (const-horizont const))
    (kvalue-form-index (const-value const) nil ':value)
) )

; (make-const-code const) liefert den Code, der den Wert der Konstanten
; als 1 Wert nach A0 bringt.
(defun make-const-code (const)
  (unless (eq (const-horizont const) ':form)
    (let ((value (const-value const)))
      (cond ((eq value 'nil) (return-from make-const-code '(NIL) ))
            ((eq value 't) (return-from make-const-code '(T) ))
  ) ) )
  `(CONST ,(const-index const) ,const)
)

; (bconst-index block) liefert den Index in FUNC, an dem dieser Block steht.
(defun bconst-index (block &optional (func *func*))
; (+ (fnode-Blocks-Offset func)
;    (position block (fnode-Blocks func) :test #'eq)
; )
  (do ((n (fnode-Blocks-Offset func) (1+ n))
       (L (fnode-Blocks func) (cdr L)))
      ((eq (car L) block) n)
) )

; (gconst-index tagbody) liefert den Index in FUNC, an dem dieser Tagbody steht.
(defun gconst-index (tagbody &optional (func *func*))
; (+ (fnode-Tagbodys-Offset func)
;    (position tagbody (fnode-Tagbodys func) :test #'eq)
; )
  (do ((n (fnode-Tagbodys-Offset func) (1+ n))
       (L (fnode-Tagbodys func) (cdr L)))
      ((eq (car L) tagbody) n)
) )

; (fconst-index fnode) liefert den Index in FUNC, an dem dieser fnode in den
; Konstanten steht. Wenn nötig, wird er eingefügt und in *fnode-fixup-table*
; vermerkt.
(defun fconst-index (fnode &optional (func *func*))
  (if (member fnode (fnode-Consts func))
    (constvalue-index fnode)
    (let ((n (constvalue-index fnode)))
      (push (list func n) *fnode-fixup-table*)
      n
) ) )

; Hilfsvariablen beim rekursiven Aufruf von traverse-anode:

; Das aktuelle Codestück, eine umgedrehte Liste von Instruktionen, die
; mit dem Start-Label als letztem nthcdr endet.
(defvar *code-part*)

; und seine Nummer (Index in *code-parts*)
(defvar *code-index*)

; Flag, ob "toter Code" (d.h. Code, der nicht erreichbar ist) vorliegt
(defvar *dead-code*)

; Für Sprungkettenverkürzung in traverse-anode: Liste aller bereits
; durchgeführten Label-Substitutionen ((old-label . new-label) ...)
(defvar *label-subst*)

; Der aktuelle Wert, interpretiert als boolescher Wert:
; FALSE falls =NIL, TRUE falls /=NIL, NIL falls unbekannt.
; (Keine Einschränkung an die Anzahl der Werte!)
(defvar *current-value*)

; Liste der Variablen/Konstanten, deren Wert mit dem aktuellen übereinstimmt
; (lexikalische Variablen als VARIABLE-Structures, dynamische Variablen als
; Symbole, Konstanten als CONST-Structures mit horizont = :value oder :all).
; Ist diese Liste nichtleer, so liegt auch genau 1 Wert vor.
(defvar *current-vars*)

; Jedes Label (ein Gensym-Symbol) hat als Wert eine Liste aller Referenzen
; auf label, und zwar jeweils entweder als Index i in *code-parts*, wenn es
; sich um den Wegsprung (das Ende) von (aref *code-parts* i) handelt, oder
; als Instruktion (einer Liste) in allen anderen Fällen. Falls das Label
; ein Codestück beginnt, steht unter (get label 'code-part) der Index in
; *code-part* des Codestücks, das mit diesem Label anfängt. Unter
; (get label 'for-value) steht, wieviele Werte bei einem möglichen Sprung
; auf das Label von Bedeutung sind (NIL/ONE/ALL).
; Eine Ausnahme stellt das "Label" NIL dar, das den Einsprungpunkt darstellt.

; Ersetzt alle Referenzen auf old-label durch Referenzen auf new-label.
(defun label-subst (old-label new-label)
  ; alle Referenzen auf old-label verändern:
  (dolist (ref (symbol-value old-label))
    (nsubst new-label old-label
            (rest (if (integerp ref) (first (aref *code-parts* ref)) ref))
  ) )
  ; und als Referenzen auf new-label eintragen:
  (setf (symbol-value new-label)
    (nconc (symbol-value old-label) (symbol-value new-label))
  )
  (setf (symbol-value old-label) '())
  ; Mit old-label fängt kein Codestück mehr an:
  (remprop old-label 'code-part)
)

; Aktuelles Codestück beenden und ein neues Codestück anfangen:
(defun finish-code-part ()
  ; das aktuelle Codestück vereinfachen:
  (simplify *code-part*)
  ; *code-part* in *code-parts* unterbringen:
  (vector-push-extend *code-part* *code-parts*)
  (vector-push-extend (incf *code-index*) *code-positions*)
)

; Einen Wegsprung auf Label label emittieren.
; Dadurch wird ein neues Codestück angefangen.
(defun emit-jmp (label)
  ; mit einem Wegsprung:
  (push `(JMP ,label ,*current-value*) *code-part*)
  (push *code-index* (symbol-value label))
  (finish-code-part)
)

; Läuft durch den Code eines Anode durch, expandiert den Code und baut dabei
; *code-part* weiter. Adjustiert die Variablen *current-value* usw. passend.
(defun traverse-anode (code)
  (dolist (item code)
    (if (atom item)
      (cond ((symbolp item) ; Label
             (if *dead-code*
               ; Code kann angesprungen werden, ist ab jetzt nicht mehr tot
               (setq *dead-code* nil)
               (if (symbolp *code-part*)
                 ; Label item sofort nach Label *code-part*
                 ; -> können identifiziert werden
                 (let ((old-label *code-part*) (new-label item))
                   ; substituiere *code-parts* -> item
                   (label-subst old-label new-label)
                   (setq *label-subst*
                     (acons old-label new-label
                       (nsubst new-label old-label *label-subst*)
                 ) ) )
                 ; Label mitten im Codestück -> aktuelles Codestück beenden
                 (emit-jmp item)
             ) )
             ; jetzt geht das aktuelle Codestück erst richtig los,
             ; mit dem Label item:
             (setq *code-part* item)
             (setf (get item 'code-part) (fill-pointer *code-parts*))
             ; Da noch Sprünge auf dieses Label kommen können, wissen wir
             ; nicht, was A0 enthält:
             (setq *current-value* nil *current-vars* '())
            )
            ((anode-p item) (traverse-anode (anode-code item))) ; Anode -> rekursiv
            (t (compiler-error 'traverse-anode "ITEM"))
      )
      ; item ist eine normale Instruktion
      (unless *dead-code* ; nur erreichbarer Code braucht verarbeitet zu werden
        (nsublis *label-subst* (rest item)) ; bisherige Substitutionen durchführen
        (case (first item)
          (CONST
            (let ((const (second item)))
              (if (eq (const-horizont const) ':form)
                (progn
                  (push (make-const-code const) *code-part*)
                  (setq *current-value* nil *current-vars* '())
                )
                (let ((cv (const-value const)))
                  (unless ; ein (CONST cv) schon in *current-vars* enthalten?
                      (dolist (v *current-vars* nil)
                        (when (and (const-p v) (eq (const-value v) cv)) (return t))
                      )
                    (push (make-const-code const) *code-part*)
                    (setq *current-value* (if (null cv) 'FALSE 'TRUE)
                          *current-vars* (list const)
          ) ) ) ) ) )
          (FCONST
            (push `(CONST ,(fconst-index (second item))) *code-part*)
            (setq *current-value* 'TRUE *current-vars* '())
          )
          (BCONST
            (push `(CONST ,(bconst-index (second item))) *code-part*)
            (setq *current-value* 'TRUE *current-vars* '())
          )
          (GCONST
            (push `(CONST ,(gconst-index (second item))) *code-part*)
            (setq *current-value* 'TRUE *current-vars* '())
          )
          (GET
            (let ((var (second item))
                  (venvc (third item))
                  (stackz (fourth item)))
              (unless (member var *current-vars* :test #'eq) ; Ist bereits der aktuelle Wert = var ?
                (push
                  (if (var-constantp var)
                    (let* ((const (var-constant var))
                           (val (const-value const)))
                      (setq *current-value* (if (null val) 'FALSE 'TRUE))
                      (if (fnode-p val)
                        ; FNODEs als Werte können (fast) nur von LABELS stammen
                        `(CONST ,(fconst-index val))
                        (make-const-code const)
                    ) )
                    (progn
                      (setq *current-value* nil)
                      (if (var-specialp var)
                        `(GETVALUE ,(kconstvalue-index (setq var (var-name var))))
                        (if (var-closurep var)
                          (multiple-value-bind (k n m)
                              (zugriff-in-closure var venvc stackz)
                            (if n
                              (if k `(LOADIC ,(car k) ,(cdr k) ,n ,m) `(LOADC ,n ,m))
                              `(LOADV ,k ,(1+ m))
                          ) )
                          ; lexikalisch und im Stack, also in derselben Funktion
                          (multiple-value-bind (k n)
                              (zugriff-in-stack stackz (var-stackz var))
                            (if k `(LOADI ,(car k) ,(cdr k) ,n) `(LOAD ,n) )
                  ) ) ) ) )
                  *code-part*
                )
                (setq *current-vars* (list var))
          ) ) )
          (SET
            (let ((var (second item))
                  (venvc (third item))
                  (stackz (fourth item)))
              (unless (member var *current-vars* :test #'eq) ; Ist bereits der aktuelle Wert = var ?
                (push
                  (if (var-specialp var)
                    `(SETVALUE ,(kconstvalue-index (setq var (var-name var))))
                    (if (var-closurep var)
                      (multiple-value-bind (k n m)
                          (zugriff-in-closure var venvc stackz)
                        (if n
                          (if k `(STOREIC ,(car k) ,(cdr k) ,n ,m) `(STOREC ,n ,m))
                          `(STOREV ,k ,(1+ m))
                      ) )
                      ; lexikalisch und im Stack, also in derselben Funktion
                      (multiple-value-bind (k n)
                          (zugriff-in-stack stackz (var-stackz var))
                        (if k `(STOREI ,(car k) ,(cdr k) ,n) `(STORE ,n) )
                  ) ) )
                  *code-part*
                )
                (push var *current-vars*) ; *current-value* bleibt unverändert
          ) ) )
          (GETVALUE
            (let ((symbol (second item)))
              (unless (member symbol *current-vars* :test #'eq)
                (push `(GETVALUE ,(kconstvalue-index symbol)) *code-part*)
                (setq *current-value* nil *current-vars* (list symbol))
          ) ) )
          (SETVALUE
            (let ((symbol (second item)))
              (unless (member symbol *current-vars* :test #'eq)
                (push `(SETVALUE ,(kconstvalue-index symbol)) *code-part*)
                (push symbol *current-vars*) ; *current-value* bleibt unverändert
          ) ) )
          (BIND
            (push `(BIND ,(const-index (second item))) *code-part*)
            (setq *current-value* nil *current-vars* '()) ; undefinierte Werte
          )
          (UNWIND ; mehrzeilige Umwandlung
            (traverse-anode
              (expand-UNWIND (second item) (third item) (fourth item))
          ) )
          (UNWINDSP ; mehrzeilige Umwandlung
            (let ((k (spdepth-difference (second item) (third item))))
              (when (or (> (car k) 0) (> (cdr k) 0))
                (push `(SKIPSP ,(car k) ,(cdr k)) *code-part*)
          ) ) )
          ((JMPIF JMPIFNOT JMPIF1 JMPIFNOT1)
            (if (null *current-value*)
              (let ((label (second item))
                    (new-label (make-label 'NIL)))
                (push
                  (case (first item)
                    (JMPIF `(JMPCASE ,label ,new-label))
                    (JMPIFNOT `(JMPCASE ,new-label ,label))
                    (JMPIF1 `(JMPCASE1-TRUE ,label ,new-label))
                    (JMPIFNOT1 `(JMPCASE1-FALSE ,new-label ,label))
                  )
                  *code-part*
                )
                (push *code-index* (symbol-value (second item)))
                (push *code-index* (symbol-value new-label))
                (finish-code-part)
                (setf (get new-label 'code-part) (fill-pointer *code-parts*))
                (setq *code-part* new-label)
                ; *current-value* und *current-vars* bleiben unverändert.
              )
              ; boolescher Wert beim Wegsprung bekannt
              (if (if (eq *current-value* 'FALSE)
                    (memq (first item) '(JMPIF JMPIF1)) ; Wert=NIL -> JMPIF weglassen
                    (memq (first item) '(JMPIFNOT JMPIFNOT1)) ; Wert/=NIL -> JMPIFNOT weglassen
                  )
                ; Sprung weglassen
                nil
                ; in JMP umwandeln:
                (progn
                  (when (memq (first item) '(JMPIF1 JMPIFNOT1))
                    (push '(VALUES1) *code-part*) ; genau 1 Wert erzwingen
                  )
                  (emit-jmp (second item))
                  (setq *dead-code* t)
          ) ) ) )
          (JMPHASH
            (let ((hashtable (make-hash-table :test (second item)))
                  (labels (cddddr item)))
              (dolist (acons (third item))
                (setf (gethash (car acons) hashtable)
                      (position (cdr acons) labels)
              ) )
              (push `(JMPHASH ,(constvalue-index hashtable) ,hashtable
                              ,@(cdddr item)
                     )
                    *code-part*
            ) )
            ; Referenzen vermerken:
            (dolist (label (cdddr item))
              (push *code-index* (symbol-value label))
            )
            (finish-code-part)
            (setq *dead-code* t)
          )
          (VENV
            (let ((venvc (second item))
                  (stackz (third item)))
              (loop ; in venvc die NILs übergehen
                (when (car venvc) (return))
                (setq venvc (cdr venvc))
              )
              (push
                (if (consp (car venvc)) ; aus dem Stack holen
                  (multiple-value-bind (k n)
                      (zugriff-in-stack stackz (cdr (car venvc)))
                    (if k `(LOADI ,(car k) ,(cdr k) ,n) `(LOAD ,n) )
                  )
                  (if (eq (car venvc) *func*)
                    (if (fnode-Venvconst *func*) '(VENV) '(NIL))
                    (compiler-error 'traverse-anode 'VENV)
                ) )
                *code-part*
              )
              (if (equal (car *code-part*) '(NIL))
                (setq *current-value* 'FALSE *current-vars* (list (make-const :horizont ':value :value 'NIL)))
                (setq *current-value* nil *current-vars* '())
              )
          ) )
          (COPY-CLOSURE
            (push `(COPY-CLOSURE ,(fconst-index (second item)) ,(third item))
                   *code-part*
            )
            (setq *current-value* 'TRUE *current-vars* '())
          )
          (CALLP) ; wird gestrichen
          (CALL
            (push `(CALL ,(second item) ,(const-index (third item)))
                   *code-part*
            )
            (setq *current-value* nil *current-vars* '())
          )
          ((CALL0 CALL1 CALL2)
            (push `(,(first item) ,(const-index (second item)))
                  *code-part*
            )
            (setq *current-value* nil *current-vars* '())
          )
          ((FUNCALLP APPLYP)
            (push '(PUSH) *code-part*)
            (setq *current-value* nil *current-vars* '())
          )
          ((JMPIFBOUNDP BOUNDP)
            (let ((var (second item))
                  (stackz (fourth item))
                 )
              (when (var-closurep var)
                (compiler-error 'traverse-anode 'var-closurep)
              )
              (multiple-value-bind (k n)
                  (zugriff-in-stack stackz (var-stackz var))
                (when k (compiler-error 'traverse-anode 'var-stackz))
                (push `(,(first item) ,n ,@(cddddr item)) *code-part*)
                (when (eq (first item) 'JMPIFBOUNDP)
                  (push (first *code-part*) (symbol-value (fifth item)))
                )
                (setq *current-value* nil *current-vars* '()) ; undefinierte Werte
          ) ) )
          (BLOCK-OPEN
            (let ((label (third item)))
              (push `(BLOCK-OPEN ,(const-index (second item)) ,label)
                     *code-part*
              )
              (push (first *code-part*) (symbol-value label))
          ) )
          (RETURN-FROM
            (push
              (if (cddr item)
                (multiple-value-bind (k n)
                    (zugriff-in-stack (third item) (block-stackz (second item)))
                  `(RETURN-FROM-I ,(car k) ,(cdr k) ,n)
                )
                (if (block-p (second item))
                  `(RETURN-FROM ,(bconst-index (second item)))
                  `(RETURN-FROM ,(const-index (second item)))
              ) )
              *code-part*
            )
            (finish-code-part)
            (setq *dead-code* t)
          )
          (TAGBODY-OPEN
            (push `(TAGBODY-OPEN ,(const-index (second item)) ,@(cddr item))
                  *code-part*
            )
            (dolist (label (cddr item)) (push item (symbol-value label)))
          )
          (GO
            (push
              (if (cdddr item)
                (multiple-value-bind (k n)
                    (zugriff-in-stack (fourth item) (tagbody-stackz (second item)))
                  `(GO-I ,(car k) ,(cdr k) ,n ,(third item))
                )
                (if (tagbody-p (second item))
                  `(GO ,(gconst-index (second item)) ,(third item))
                  `(GO ,(const-index (second item)) ,(third item))
              ) )
              *code-part*
            )
            (finish-code-part)
            (setq *dead-code* t)
          )
          ((NIL TAGBODY-CLOSE-NIL)
            (push item *code-part*)
            (setq *current-value* 'FALSE *current-vars* (list (make-const :horizont ':value :value 'NIL)))
          )
          (HANDLER-OPEN
            (setq item
              (let ((v (const-value (second item)))
                    (k (spdepth-difference (third item) *func*)))
                ; Aus v = #(type1 ... typem) mache v = #(type1 nil ... typem nil)
                (setq v (coerce (mapcap #'(lambda (x) (list x nil)) (coerce v 'list)) 'vector))
                `(HANDLER-OPEN ,(constvalue-index (cons v k)) ,v ,k ,@(cdddr item))
            ) )
            (push item *code-part*)
            (dolist (label (cddddr item)) (push item (symbol-value label)))
          )
          (VALUES0
            (push item *code-part*)
            (setq *current-value* 'FALSE *current-vars* '())
          )
          ((SKIP SKIPI SKIPSP VALUES1 MVCALLP BLOCK-CLOSE TAGBODY-CLOSE
            CATCH-CLOSE UNWIND-PROTECT-NORMAL-EXIT HANDLER-BEGIN
            STORE ; STORE nur auf Funktionsargumente innerhalb eines
                  ; Funktionsaufrufs, vgl. c-DIRECT-FUNCTION-CALL
           )
            (push item *code-part*)
          )
          ((T)
            (push item *code-part*)
            (setq *current-value* 'TRUE *current-vars* (list (make-const :horizont ':value :value 'T)))
          )
          ((RET RETGF BARRIER THROW)
            (push item *code-part*)
            (finish-code-part)
            (setq *dead-code* t)
          )
          (JMP
            (emit-jmp (second item))
            (setq *dead-code* t)
          )
          (JSR
            (push item *code-part*)
            (push item (symbol-value (third item)))
            (setq *current-value* nil *current-vars* '())
          )
          ((CATCH-OPEN UNWIND-PROTECT-OPEN)
            (push item *code-part*)
            (push item (symbol-value (second item)))
          )
          (UNWIND-PROTECT-CLOSE
            (push item *code-part*)
            (push item (symbol-value (second item)))
            (setq *current-value* nil *current-vars* '()) ; Werte werden weggeworfen
          )
          ((PUSH-NIL PROGV PUSH POP MAKE-VECTOR1&PUSH CALLS1 CALLS2 CALLSR
            CALLC CALLCKEY FUNCALL APPLY PUSH-UNBOUND UNLIST UNLIST*
            STACK-TO-MV MV-TO-STACK NV-TO-STACK MV-TO-LIST LIST-TO-MV MVCALL
            NOT EQ CAR CDR ATOM CONSP SYMBOL-FUNCTION SVREF SVSET
           )
            (push item *code-part*)
            (setq *current-value* nil *current-vars* '())
          )
          ((CONS LIST LIST*)
            (push item *code-part*)
            (setq *current-value* 'TRUE *current-vars* '())
          )
          ((UNWIND-PROTECT-CLEANUP)
            (push item *code-part*)
            (setq *current-vars* '()) ; Kann Variablenwerte zerstören
          )
          ((UNBIND1)
            (push item *code-part*)
            (setq *current-vars* (delete-if #'symbolp *current-vars*)) ; Kann Werte dynamischer Variablen zerstören
          )
          (t (compiler-error 'traverse-anode "LISTITEM"))
) ) ) ) )

; Hilfsfunktionen nach dem 1. Schritt:

; Kommt eine Instruktion item dazu, die vielleicht Label-Referenzen enthält,
; so ist note-references aufzurufen. Dieses notiert die Label-Referenzen in
; item. item gehöre zu (aref *code-parts* index).
; Wird eine Instruktion item entfernt, die vielleicht Label-Referenzen enthält,
; so ist remove-references aufzurufen. Dieses notiert das Wegfallen der
; Label-Referenzen in item. item gehöre zu (aref *code-parts* index).
; Liefert auch die Liste der in item enthaltenen Labels.
(macrolet ((references ()
             `(case (first item)
                (JMP (end-ref (second item)))
                ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                 (end-ref (second item)) (end-ref (third item))
                )
                (JMPHASH (dolist (label (cdddr item)) (end-ref label)))
                ((JMPIFBOUNDP CATCH-OPEN UNWIND-PROTECT-OPEN UNWIND-PROTECT-CLOSE)
                 (mid-ref (second item))
                )
                ((BLOCK-OPEN JSR) (mid-ref (third item)))
                (JMPTAIL (mid-ref (fourth item)))
                (TAGBODY-OPEN (dolist (label (cddr item)) (mid-ref label)))
                (HANDLER-OPEN (dolist (label (cddddr item)) (mid-ref label)))
              )
          ))
  (defun note-references (item &optional index)
    (macrolet ((end-ref (label) `(push index (symbol-value ,label)))
               (mid-ref (label) `(push item (symbol-value ,label))))
      (references)
  ) )
  (defun remove-references (item &optional index &aux (labellist '()))
    (macrolet ((end-ref (label)
                 (let ((labelvar (gensym)))
                   `(let ((,labelvar ,label))
                      (setf (symbol-value ,labelvar) (delete index (symbol-value ,labelvar)))
                      (pushnew ,labelvar labellist)
                    )
               ) )
               (mid-ref (label)
                 (let ((labelvar (gensym)))
                   `(let ((,labelvar ,label))
                      (setf (symbol-value ,labelvar) (delete item (symbol-value ,labelvar)))
                      (pushnew ,labelvar labellist)
                    )
              )) )
      (references)
      labellist
  ) )
)

#|
                              2. Schritt
                Vereinfachung von Folgen von Operationen

Dieses spielt sich auf umgedrehten Codestücken ab; sie werden dabei destruktiv
verändert.

Vereinfachungsregeln für Operationen:

1. (VALUES1) darf nach allen Instruktionen gestrichen werden, die sowieso nur
   einen Wert produzieren, und vor allen, die sowieso nur einen verwenden.

2. (SKIP n1) (SKIP n2)                   --> (SKIP n1+n2)
   (SKIPI k1 k2 n1) (SKIP n2)            --> (SKIPI k1 k2 n1+n2)
   (SKIP n1) (SKIPI k1 k2 n2)            --> (SKIPI k1 k2 n2)
   (SKIPI k11 k21 n1) (SKIPI k21 k22 n2) --> (SKIPI k11+k12+1 k21+k22 n2)
   (SKIPSP k11 k21) (SKIPI k21 k22 n)    --> (SKIPI k11+k12 k21+k22 n)
   (SKIPSP k11 k21) (SKIPSP k21 k22)     --> (SKIPSP k11+k12 k21+k22)

3. (NOT) (NOT) (NOT)                 --> (NOT)
   (ATOM) (NOT)                      --> (CONSP)
   (CONSP) (NOT)                     --> (ATOM)

4. (LOAD 0) (SKIP n)                 --> (POP) (SKIP n-1)  für n>1
   (LOAD 0) (SKIP 1)                 --> (POP)             für n=1
   (PUSH) (SKIP n)                   --> (SKIP n-1)  für n>1
   (PUSH) (SKIP 1)                   -->             für n=1
   (NV-TO-STACK n) (SKIP n)          -->
   (NV-TO-STACK n+m) (SKIP n)        --> (NV-TO-STACK m)
   (NV-TO-STACK n) (SKIP n+m)        --> (SKIP m)
   (STORE m) (SKIP n)                --> (VALUES1) (SKIP n) für n>m
   (STORE 0) (POP)                   --> (VALUES1) (SKIP 1)
   (PUSH) (POP)                      --> (VALUES1)
   (POP) (PUSH)                      -->
   (SKIP n) (PUSH)                   --> (SKIP n-1) (STORE 0) für n>1
   (SKIP 1) (PUSH)                   --> (STORE 0)            für n=1

5. (VALUES1)/... (MV-TO-STACK)       --> (VALUES1)/... (PUSH)
   (VALUES0) (MV-TO-STACK)           -->
   (STACK-TO-MV n) (MV-TO-STACK)     -->
   (STACK-TO-MV m) (NV-TO-STACK n)   --> (PUSH-NIL n-m)  für m<n
                                     -->                 für m=n
                                     --> (SKIP m-n)      für m>n
   (NIL)/(VALUES0) (NV-TO-STACK n)   --> (PUSH-NIL n)
   (VALUES1)/... (NV-TO-STACK n)     --> (VALUES1)/... (PUSH) (PUSH-NIL n-1)

6. (PUSH-UNBOUND n) (PUSH-UNBOUND m) --> (PUSH-UNBOUND n+m)

7. (LIST* 1)                         --> (CONS)

|#

; Die Hash-Tabelle one-value-ops enthält diejenigen Befehle,
; die genau einen Wert erzeugen.
(defconstant one-value-ops
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (op '(NIL T CONST LOAD LOADI LOADC LOADV LOADIC STORE STOREI
                  STOREC STOREV STOREIC GETVALUE SETVALUE POP VENV
                  COPY-CLOSURE BOUNDP VALUES1 MV-TO-LIST TAGBODY-CLOSE-NIL
                  NOT EQ CAR CDR CONS ATOM CONSP SYMBOL-FUNCTION SVREF SVSET
                  LIST LIST*
            )    )
      (setf (gethash op ht) t)
    )
    ht
) )

; Der Wert zu einem Key in dieser Hash-Tabelle gibt an, wieviele Werte bei
; der Ausführung der entsprechenden Operation benötigt werden
; (vgl. *for-value*):
; NIL : Werte werden weggeworfen.
; ONE : Ein Wert wird verwendet, die übrigen weggeworfen.
; ALL : Alle Werte werden verwendet.
; Operationen, die ihre Werte nicht verändern, werden hierin nicht
; aufgeführt.
(defconstant for-value-table
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (op '(NIL PUSH-NIL T CONST LOAD LOADI LOADC LOADV LOADIC
                  GETVALUE POP JSR JMPTAIL BARRIER VENV COPY-CLOSURE CALL
                  CALL0 CALLS1 CALLS2 CALLSR FUNCALL PUSH-UNBOUND JMPIFBOUNDP
                  BOUNDP VALUES0 STACK-TO-MV MVCALL
                  BLOCK-OPEN TAGBODY-OPEN TAGBODY-CLOSE-NIL GO GO-I
                  UNWIND-PROTECT-OPEN UNWIND-PROTECT-CLOSE
                  HANDLER-OPEN HANDLER-BEGIN
                  LIST
            )    )
      (setf (gethash op ht) 'NIL)
    )
    (dolist (op '(STORE STOREI STOREC STOREV STOREIC SETVALUE BIND PROGV PUSH
                  MAKE-VECTOR1&PUSH CALL1 CALL2 CALLC CALLCKEY APPLY UNLIST
                  UNLIST* VALUES1 LIST-TO-MV MVCALLP CATCH-OPEN
                  NOT EQ CAR CDR CONS ATOM CONSP SYMBOL-FUNCTION SVREF SVSET
                  LIST*
            )    )
      (setf (gethash op ht) 'ONE)
    )
    (dolist (op '(MV-TO-STACK NV-TO-STACK MV-TO-LIST RETURN-FROM RETURN-FROM-I
                  THROW UNWIND-PROTECT-NORMAL-EXIT
            )    )
      (setf (gethash op ht) 'ALL)
    )
    ; Nicht in der Tabelle, weil sie die Werte unverändert lassen:
    ;           '(UNBIND1 SKIP SKIPI SKIPSP BLOCK-CLOSE TAGBODY-CLOSE
    ;             CATCH-CLOSE UNWIND-PROTECT-CLEANUP
    ;            )
    ; Nicht in der Tabelle, weil es Wegsprünge sind:
    ;   ONE:    '(RETGF JMPHASH)
    ;   ALL:    '(RET JMP JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
    ht
) )

; Vereinfacht ein Codestück (in umgedrehter Reihenfolge!).
; Obige Vereinfachungsregeln werden durchgeführt, solange es geht.
; Ergebnis ist meist NIL, oder aber (um anzuzeigen, dass weitere Optimierungen
; möglich sind) das Anfangslabel, falls sich dessen Property for-value
; abgeschwächt hat.
(defun simplify (codelist)
  (let ((for-value-at-end
          (let ((item (car codelist)))
            (case (first item)
              (JMP (get (second item) 'for-value))
              ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                (if (or (and (not (eq (first item) 'JMPCASE1-TRUE))
                             (eq (get (second item) 'for-value) 'ALL)
                        )
                        (and (not (eq (first item) 'JMPCASE1-FALSE))
                             (eq (get (third item) 'for-value) 'ALL)
                    )   )
                  'ALL
                  'ONE
              ) )
              ((RETGF JMPHASH) 'ONE)
              ((BARRIER GO GO-I JMPTAIL) 'NIL)
              ((RETURN-FROM RETURN-FROM-I RET THROW) 'ALL)
              (t (compiler-error 'simplify "AT-END"))
        ) ) )
        (result nil)) ; evtl. das Anfangslabel
    ; for-value-at-end zeigt an, welche Werte vor dem Wegsprung benötigt werden.
    (loop
      (let ((modified nil))
        (let* ((links codelist) (mitte (cdr links)) rechts (for-value for-value-at-end))
          ; Es wandern drei Pointer durch die Codeliste: ...links.mitte.rechts...
          ; for-value zeigt an, was für Werte nach Ausführung von (car mitte),
          ; vor Ausführung von (car links), gebraucht werden.
          (loop
            nochmal
            (when (atom mitte) (return))
            (setq rechts (cdr mitte))
            (macrolet ((ersetze1 (new) ; ersetze (car mitte) durch new
                         `(progn
                            (setf (car mitte) ,new)
                            (setq modified t) (go nochmal)
                          )
                       )
                       (ersetze2 (new) ; ersetze (car mitte) und (car rechts) durch new
                         `(progn
                            ,@(unless (equal new '(car mitte))
                                `((setf (car mitte) ,new))
                              )
                            (setf (cdr mitte) (cdr rechts))
                            (setq modified t) (go nochmal)
                          )
                       )
                       (streiche1 () ; streiche (car mitte) ersatzlos
                         `(progn
                            (setf (cdr links) (setq mitte rechts))
                            (setq modified t) (go nochmal)
                          )
                       )
                       (streiche2 () ; streiche (car mitte) und (car rechts) ersatzlos
                         `(progn
                            (setf (cdr links) (setq mitte (cdr rechts)))
                            (setq modified t) (go nochmal)
                          )
                       )
                       (erweitere2 (new1 new2) ; ersetze (car mitte) durch new1 und new2
                         `(progn
                            (setf (car mitte) ,new1)
                            (setf (cdr mitte) (cons ,new2 rechts))
                            (setq modified t) (go nochmal)
                          )
                      ))
              (when (eq for-value 'NIL)
                ; vor einer Operation, die keine Werte braucht:
                (case (first (car mitte))
                  ((NIL T CONST LOAD LOADI LOADC LOADV LOADIC GETVALUE VENV
                    BOUNDP VALUES0 VALUES1 MV-TO-LIST LIST-TO-MV NOT CAR CDR
                    SYMBOL-FUNCTION ATOM CONSP
                   )
                    (streiche1)
                  )
                  ((LIST LIST* STACK-TO-MV) ; (LIST n) --> (SKIP n), n>0
                                            ; (LIST* n) --> (SKIP n), n>0
                                            ; (STACK-TO-MV n) --> (SKIP n), n>0
                    (ersetze1 `(SKIP ,(second (car mitte))))
                  )
                  ((POP EQ CONS SVREF) (ersetze1 '(SKIP 1)))
              ) )
              (when (eq for-value 'ONE)
                ; vor einer Operation, die nur einen Wert braucht:
                (case (first (car mitte))
                  (VALUES1 (streiche1))
                  (VALUES0 (ersetze1 '(NIL)))
                  (LIST-TO-MV (ersetze1 '(CAR)))
                  (STACK-TO-MV ; (STACK-TO-MV n) --> (SKIP n-1) (POP) für n>1
                    (let ((n (second (car mitte))))
                      (erweitere2 '(POP) `(SKIP ,(- n 1)))
              ) ) ) )
              (when (consp rechts)
                ; Gucklock umfasst (car mitte) und (car rechts), evtl. auch mehr.
                (case (first (car mitte))
                  (VALUES1 ; Regel 1
                    (when (gethash (first (car rechts)) one-value-ops nil)
                      ; (op ...) (VALUES1) --> (op ...)
                      (streiche1)
                  ) )
                  (NOT ; Regel 3
                    (case (first (car rechts))
                      (NOT
                        (when (and (consp (cdr rechts))
                                   (equal (cadr rechts) '(NOT))
                              )
                          ; (NOT) (NOT) (NOT) --> (NOT)
                          (streiche2)
                      ) )
                      (ATOM (ersetze2 '(CONSP))) ; (ATOM) (NOT) --> (CONSP)
                      (CONSP (ersetze2 '(ATOM))) ; (CONSP) (NOT) --> (ATOM)
                  ) )
                  (SKIP
                    (let ((n2 (second (car mitte)))) ; n2 > 0
                      (case (first (car rechts))
                        ; Regel 2
                        (SKIP ; (SKIP n1) (SKIP n2) --> (SKIP n1+n2)
                          (let ((n1 (second (car rechts))))
                            (ersetze2 `(SKIP ,(+ n1 n2)))
                        ) )
                        (SKIPI ; (SKIPI k1 k2 n1) (SKIP n2) --> (SKIPI k1 k2 n1+n2)
                          (let ((k1 (second (car rechts)))
                                (k2 (third (car rechts)))
                                (n1 (fourth (car rechts))))
                            (ersetze2 `(SKIPI ,k1 ,k2 ,(+ n1 n2)))
                        ) )
                        ; Regel 4
                        (LOAD ; (LOAD 0) (SKIP n) --> (POP) [(SKIP n-1)]
                          (when (eql (second (car rechts)) 0)
                            (if (eql n2 1)
                              (ersetze2 '(POP))
                              (progn (setf (car rechts) '(POP))
                                     (ersetze1 `(SKIP ,(- n2 1)))
                        ) ) ) )
                        (PUSH ; (PUSH) (SKIP n) --> [(SKIP n-1)]
                          (if (eql n2 1)
                            (streiche2)
                            (ersetze2 `(SKIP ,(- n2 1)))
                        ) )
                        (NV-TO-STACK
                          (let ((n1 (second (car rechts))))
                            (cond ((> n1 n2) (ersetze2 `(NV-TO-STACK ,(- n1 n2))))
                                  ((< n1 n2) (ersetze2 `(SKIP ,(- n2 n1))))
                                  (t (streiche2))
                        ) ) )
                        (STORE ; (STORE m) (SKIP n) --> (VALUES1) (SKIP n) für n>m
                          (let ((m (second (car rechts))))
                            (when (> n2 m)
                              (setf (car rechts) '(VALUES1))
                              (setq modified t) (go nochmal)
                  ) ) ) ) ) )
                  (SKIPI ; Regel 2
                    (case (first (car rechts))
                      (SKIP ; (SKIP n1) (SKIPI k1 k2 n2) --> (SKIPI k1 k2 n2)
                        (ersetze2 (car mitte))
                      )
                      (SKIPI ; (SKIPI k11 k21 n1) (SKIPI k21 k22 n2) --> (SKIPI k11+k12+1 k21+k22 n2)
                        (let ((k11 (second (car rechts)))
                              (k21 (third (car rechts)))
                              (k12 (second (car mitte)))
                              (k22 (third (car mitte)))
                              (n2 (third (car mitte))))
                          (ersetze2 `(SKIPI ,(+ k11 k12 1) ,(+ k21 k22) ,n2))
                      ) )
                      (SKIPSP ; (SKIPSP k11 k21) (SKIPI k21 k22 n) --> (SKIPI k11+k12 k21+k22 n)
                        (let ((k11 (second (car rechts)))
                              (k21 (third (car rechts)))
                              (k12 (second (car mitte)))
                              (k22 (third (car mitte)))
                              (n2 (third (car mitte))))
                          (ersetze2 `(SKIPI ,(+ k11 k12) ,(+ k21 k22) ,n2))
                  ) ) ) )
                  (SKIPSP ; Regel 2
                    (case (first (car rechts))
                      (SKIPSP ; (SKIPSP k11 k21) (SKIPSP k21 k22) --> (SKIPSP k11+k12 k21+k22)
                        (let ((k11 (second (car rechts)))
                              (k21 (third (car rechts)))
                              (k12 (second (car mitte)))
                              (k22 (third (car mitte))))
                          (ersetze2 `(SKIPSP ,(+ k11 k12) ,(+ k21 k22)))
                  ) ) ) )
                  (POP ; Regel 4
                    (cond ((equal (car rechts) '(STORE 0))
                            ; (STORE 0) (POP) --> (VALUES1) (SKIP 1)
                            (setf (car rechts) '(VALUES1))
                            (ersetze1 '(SKIP 1))
                          )
                          ((equal (car rechts) '(PUSH))
                            ; (PUSH) (POP) --> (VALUES1)
                            (ersetze2 '(VALUES1))
                  ) )     )
                  (PUSH ; Regel 4
                    (case (first (car rechts))
                      (POP (streiche2)) ; (POP) (PUSH) streichen
                      (SKIP ; (SKIP n) (PUSH) --> [(SKIP n-1)] (STORE 0)
                        (let ((n (second (car rechts))))
                          (if (eql n 1)
                            (unless (and (consp (cdr rechts)) (equal (cadr rechts) '(LOAD 0)))
                              ; (LOAD 0) (SKIP 1) (PUSH) wird anders behandelt
                              (ersetze2 '(STORE 0))
                            )
                            (progn (setf (car rechts) `(SKIP ,(- n 1)))
                                   (ersetze1 '(STORE 0))
                  ) ) ) ) ) )
                  (MV-TO-STACK ; Regel 5
                    (when (gethash (first (car rechts)) one-value-ops nil)
                      ; (car rechts) liefert nur einen Wert -->
                      ; (MV-TO-STACK) durch (PUSH) ersetzen:
                      (ersetze1 '(PUSH))
                    )
                    (case (first (car rechts))
                      ((VALUES0 STACK-TO-MV) (streiche2))
                  ) )
                  (NV-TO-STACK ; Regel 5
                    (let ((n (second (car mitte))))
                      (case (first (car rechts))
                        (STACK-TO-MV
                          (let ((m (second (car rechts))))
                            (cond ((> n m) (ersetze2 `(PUSH-NIL ,(- n m))))
                                  ((< n m) (ersetze2 `(SKIP ,(- m n))))
                                  (t (streiche2))
                        ) ) )
                        ((VALUES0 NIL) (ersetze2 `(PUSH-NIL ,n)))
                        (t (when (gethash (first (car rechts)) one-value-ops nil)
                             (erweitere2 `(PUSH-NIL ,(- n 1)) `(PUSH))
                  ) ) ) )  )
                  (PUSH-UNBOUND ; Regel 6
                    (case (first (car rechts))
                      (PUSH-UNBOUND ; (PUSH-UNBOUND n) (PUSH-UNBOUND m) --> (PUSH-UNBOUND n+m)
                        (let ((n (second (car rechts)))
                              (m (second (car mitte))))
                          (ersetze2 `(PUSH-UNBOUND ,(+ n m)))
                  ) ) ) )
                  (LIST* ; Regel 7
                    (when (equal (rest (car mitte)) '(1))
                      (ersetze1 '(CONS))
                  ) )
            ) ) )
            (when (atom mitte) (return))
            ; Neues for-value berechnen, in Abhängigkeit von (car mitte):
            (setq for-value
              (gethash (first (car mitte)) for-value-table for-value)
            )
            ; weiterrücken:
            (setq links mitte mitte rechts)
          )
          ; Codestück zu Ende: (atom mitte)
          (when mitte
            ; mitte ist das Anfangslabel
            (let ((old-for-value (get mitte 'for-value)))
              ; Ist for-value besser als old-for-value ?
              (when (and (not (eq for-value old-for-value))
                         (or (eq old-for-value 'ALL) (eq for-value 'NIL))
                    )
                ; ja -> Anfangslabel nachher als Ergebnis bringen:
                (setf (get mitte 'for-value) for-value result mitte)
          ) ) )
        ) ; end let*
        (unless modified (return))
    ) ) ; end let, loop
    (let (codelistr)
      (when (and (eq (first (first codelist)) 'RET)
                 (consp (setq codelistr (cdr codelist)))
                 (or (eq (first (first codelistr)) 'JSR)
                     (and (eq (first (second codelist)) 'SKIP)
                          (consp (setq codelistr (cddr codelist)))
                          (eq (first (first codelistr)) 'JSR)
            )    )   )
        ; (JSR n label) [(SKIP m)] (RET) --> (JMPTAIL n n+m label)
        (let ((n (second (first codelistr)))
              (label (third (first codelistr)))
              (m (if (eq codelistr (cdr codelist)) 0 (second (second codelist)))))
          (setf (first codelist) `(JMPTAIL ,n ,(+ n m) ,label))
        )
        (remove-references (first codelistr)) ; (JSR ...) wird gestrichen
        (note-references (first codelist)) ; (JMPTAIL ...) wird eingefügt
        (setf (cdr codelist) (cdr codelistr)) ; ein bzw. zwei Listenelemente streichen
        (setq for-value-at-end 'NIL) ; JMPTAIL braucht keine Werte
    ) )
    result
) )

#|
                            3. Schritt:
                      Allgemeine Optimierungen

Wird eine Optimierung erfolgreich durchgeführt, so werden alle weiteren
Optimierungen nochmal probiert, die sich deswegen ergeben könnten.

optimize-part    - ruft den 2. Schritt auf:
                   Peephole-Optimierung normaler Operationen.

optimize-label   - Codestücke zu Labels, die nicht (mehr) referenziert werden,
                   werden entfernt.
                 - Wird ein Label nur von einem einzigen JMP referenziert,
                   der nicht vom selben Codestück kommt, können die beiden
                   betroffenen Stücke aneinandergehängt werden.

optimize-short   - Liegt ein Codestück vor, wo auf das Anfangslabel label1
                   sofort ein (JMP label2) folgt, so werden alle Referenzen
                   von label1 durch label2 ersetzt und das Codestück entfernt.
                 - Liegt ein Codestück vor, wo auf das Anfangslabel label
                   sofort ein
                   (JMPCASE/JMPCASE1-TRUE/JMPCASE1-FALSE label_true label_false)
                   folgt, so können Referenzen (JMPCASE1-TRUE label l) und
                   (JMPCASE1-FALSE l label) vereinfacht werden.
                 - Ein kurzes Codestück wird direkt an zugehörige JMPs auf
                   sein Anfangslabel angehängt. (Ein Codestück heißt "kurz",
                   wenn es höchstens 2 Befehle umfasst und nicht mit einem
                   JMPHASH (den man nicht duplizieren sollte) abgeschlossen
                   ist. Auch HANDLER-OPEN sollte man nicht duplizieren.)

optimize-jmpcase - (JMPCASE label label) wird vereinfacht zu (JMP label).
                 - (NOT) [...] (JMPCASE label_true label_false) wird
                   vereinfacht zu [...] (JMPCASE label_false label_true),
                   wobei [...] nur Befehle enthalten darf, die den 1. Wert
                   nicht verändern, und bei label_true und label_false keine
                   Werte gebraucht werden.

optimize-value   - Ein Wegsprung JMPCASE1-TRUE/JMPCASE1-FALSE kann durch
                   JMPCASE ersetzt werden, wenn am Ziel-Label der Wert
                   nicht gebraucht oder nur der 1. Wert gebraucht wird.
                 - Ein Wegsprung JMPCASE/JMPCASE1-TRUE/JMPCASE1-FALSE kann
                   durch ein JMP ersetzt werden, wenn der aktuelle Wert an
                   dieser Stelle als =NIL oder als /=NIL nachgewiesen werden
                   kann.
                 - Ein JMP kann die Information, welcher Wert gerade vorliegt,
                   zu seinem Ziel-Label weitertragen.

coalesce         - Lege Codeteile mit gleichem Ende (mind. 3 Befehle) zusammen.

|#

(defun optimize-part (code)
  (let ((label (simplify code)))
    (when label
      ; Die Property for-value von label wurde verbessert.
      (dolist (ref (symbol-value label))
        (when (integerp ref) (optimize-value ref))
) ) ) )

(defun optimize-label (label &optional (index (get label 'code-part))
                                       (code (aref *code-parts* index))
                                       (lastc (last code))
                      )
  (unless (eq label (cdr lastc)) (compiler-error 'optimize-label))
  (when label
    ; label ist ein Label, es beginnt den Code
    ; code = (aref *code-parts* index), und es ist lastc = (last code).
    (let ((refs (symbol-value label))) ; Liste der Referenzen darauf
      (cond ((null refs)
              ; nicht referenziertes Label: Codestück entfernen,
              ; Referenzen aus diesem Codestück heraus eliminieren.
              (let ((labellist '())) ; Liste von Labels, die Referenzen
                                     ; verloren haben
                (loop
                  (when (atom code) (return))
                  (setq labellist
                    (nreconc labellist (remove-references (pop code) index))
                ) )
                (setf (aref *code-parts* index) nil) ; Codestück entfernen
                ; Bei Labels mit weniger Referenzen weiteroptimieren:
                ; (Vorsicht: Hierdurch kann sich *code-parts* verändern.)
                (dolist (olabel labellist)
                  (let* ((oindex (get olabel 'code-part))
                         (ocode (aref *code-parts* oindex)))
                    (when (and ocode (eq (cdr (last ocode)) olabel))
                      (optimize-label olabel oindex ocode)
                ) ) )
            ) )
            ((null (cdr refs))
              ; Label mit nur einer Referenz, und zwar durch JMP ?
              (let ((ref (first refs)))
                (when (and (integerp ref) ; Ein JMP ist ein Wegsprung
                           (eq (first (car (aref *code-parts* ref))) 'JMP)
                           (not (eql index ref)) ; aus anderem Codestück
                      )
                  ; Anhängen:
                  ; (aref *code-parts* ref) wird in die Schublade
                  ; (aref *code-parts* index) gesteckt.
                  (setf (cdr lastc) (rest (aref *code-parts* ref)))
                  (setf (aref *code-parts* ref) nil)
                  (let ((new-startlabel (cdr (last lastc)))) ; neues Startlabel von (aref *code-parts* index)
                    (when new-startlabel
                      (setf (get new-startlabel 'code-part) index)
                  ) )
                  (setf (symbol-value label) '()) ; altes Startlabel von (aref *code-parts* index) deaktivieren
                  ; neues Codestück vereinfachen:
                  (optimize-part code)
) ) ) )     ) ) )

(defun optimize-short (index &optional (code (aref *code-parts* index))
                             &aux      (lastc (last code))
                                       (label (cdr lastc))
                      )
  (when label
    ; label ist ein Label, es beginnt den Code
    ; code = (aref *code-parts* index), und es ist lastc = (last code).
    (when (eq code lastc)
      ; Eine einzige Operation nach dem Label.
      (let ((item (car code)))
        (case (first item)
          (JMP ; (JMP ...) sofort nach dem Label
            (let ((to-label (second item)))
              (unless (eq label to-label)
                (label-subst label to-label) ; Referenzen umbiegen
                (setf (aref *code-parts* index) nil) ; Codestück entfernen
                (setf (symbol-value to-label)
                      (delete index (symbol-value to-label)) ; Referenz fällt weg
                )
                (optimize-label to-label) ; mögliche Optimierung
                (dolist (refindex (symbol-value to-label))
                  (when (integerp refindex)
                    (let* ((refcode (aref *code-parts* refindex))
                           (ref (car refcode)))
                      (when (and (eq (first ref) 'JMPCASE)
                                 (eq (second ref) to-label)
                                 (eq (third ref) to-label)
                            )
                        (optimize-jmpcase refindex refcode) ; sichere Optimierung
                ) ) ) )
            ) )
            (return-from optimize-short)
          )
          ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
            (let ((true-label (second item))
                  (false-label (third item)))
              (unless (or (eq label true-label) (eq label false-label))
                (macrolet ((err () `(compiler-error 'optimize-short)))
                  ; JMPCASE1-Referenzen auf label vereinfachen:
                  (let ((modified-indices '())) ; Indizes von modifizierten Codestücken
                    (dolist (refindex (symbol-value label))
                      (when (integerp refindex)
                        (let* ((refcode (aref *code-parts* refindex))
                               (ref (car refcode)))
                          (case (first ref)
                            (JMP
                              ; (JMP label) --> (JMPCASE/... true-label false-label)
                              (setf (car refcode) item)
                              ; neue Verweise auf true-label und false-label:
                              (push refindex (symbol-value true-label))
                              (push refindex (symbol-value false-label))
                              (push refindex modified-indices)
                            )
                            ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                              ; (JMPCASE/... label1 label2)
                              (let ((label1 (second ref)) ; im TRUE-Fall: wohin springen
                                    (label2 (third ref)) ; im FALSE-Fall: wohin springen
                                    (1-true (eq (first ref) 'JMPCASE1-TRUE)) ; im TRUE-Fall: mit (VALUES1) ?
                                    (1-false (eq (first ref) 'JMPCASE1-FALSE))) ; im FALSE-Fall: mit (VALUES1) ?
                                (when (eq label label1)
                                  ; Der (JMPCASE/... label ...) wird vereinfacht zu
                                  ; (JMPCASE/... true-label ...).
                                  (setq label1 true-label)
                                  ; neuer Verweis auf true-label:
                                  (push refindex (symbol-value true-label))
                                  (push refindex modified-indices)
                                  (when (eq (first item) 'JMPCASE1-TRUE)
                                    (setq 1-true t)
                                ) )
                                (when (eq label label2)
                                  ; Der (JMPCASE/... ... label) wird vereinfacht zu
                                  ; (JMPCASE/... ... false-label).
                                  (setq label2 false-label)
                                  ; neuer Verweis auf false-label:
                                  (push refindex (symbol-value false-label))
                                  (push refindex modified-indices)
                                  (when (eq (first item) 'JMPCASE1-FALSE)
                                    (setq 1-false t)
                                ) )
                                (unless (eq (get label1 'for-value) 'ALL)
                                  (setq 1-true nil)
                                )
                                (unless (eq (get label2 'for-value) 'ALL)
                                  (setq 1-false nil)
                                )
                                (when (and 1-true 1-false)
                                  (push '(VALUES1) (cdr refcode))
                                  (setq 1-true nil 1-false nil)
                                )
                                (setf (car refcode)
                                  `(,(cond (1-true 'JMPCASE1-TRUE)
                                           (1-false 'JMPCASE1-FALSE)
                                           (t 'JMPCASE)
                                     )
                                    ,label1
                                    ,label2
                                   )
                            ) ) )
                            (JMPHASH (err)) ; JMPHASH hat undefinierte Werte
                        ) )
                        ; später:
                        ; (setf (symbol-value label) (delete refindex (symbol-value label)))
                    ) )
                    (setf (symbol-value label)
                          (delete-if #'integerp (symbol-value label))
                    )
                    ; evtl. Optimierung wegen verringerter Referenzen möglich:
                    (optimize-label label)
                    ; evtl. weitere Optimierung in veränderten Codeteilen:
                    (dolist (refindex modified-indices)
                      (simplify (aref *code-parts* refindex))
                      (optimize-value refindex)
                      (optimize-jmpcase refindex (aref *code-parts* refindex))
                    )
          ) ) ) ) )
    ) ) )
    ; Sonstige "kurze" Codestücke, maximal 2 Operationen lang:
    (when (and (or (eq code lastc) (eq (cdr code) lastc))
               (not (eq (first (car code)) 'JMPHASH))
               (or (eq code lastc) (not (eq (first (cadr code)) 'HANDLER-OPEN)))
          )
      (let ((indices '())) ; Liste der Indizes der Codestücke, an die wir code anhängen
        (setf (cdr lastc) '()) ; code vorläufig ohne das Label am Schluss
        (dolist (refindex (symbol-value label))
          (when (and (integerp refindex) (not (eql refindex index)))
            (let ((refcode (aref *code-parts* refindex)))
              (when (eq (first (car refcode)) 'JMP)
                ; anhängen:
                (let ((new-code (mapcar #'copy-list code)))
                  (dolist (op new-code) (note-references op refindex))
                  (setf (aref *code-parts* refindex) (nconc new-code (cdr refcode)))
                )
                (setf (symbol-value label) (delete refindex (symbol-value label)))
                (push refindex indices)
        ) ) ) )
        (setf (cdr lastc) label) ; wieder das Label ans Listenende setzen
        (when indices
          ; mögliche weitere Optimierungen:
          (dolist (refindex indices)
            (optimize-part (aref *code-parts* refindex))
          )
          (optimize-label label) ; label hat weniger Referenzen -> optimieren
    ) ) )
) )

; get-boolean-value versucht zu einem Anfangsstück eines Codestücks
; (einem (nthcdr n codelist) mit n>=1) zu bestimmen, welcher boolesche Wert
; nach seiner Ausführung vorliegt:
; FALSE     sicher A0 = NIL,
; TRUE      sicher A0 /= NIL,
; NIL       keine Aussage.
(defun get-boolean-value (code)
  (macrolet ((err () `(compiler-error 'get-boolean-value)))
    (let ((invert nil)) ; ob von hier bis zum Ende der boolesche Wert invertiert wird
      ((lambda (value)
         (if invert
           (case value (TRUE 'FALSE) (FALSE 'TRUE) (t NIL))
           value
       ) )
       (block value
         (loop ; Codeliste durchlaufen
           (when (atom code) (return))
           (case (first (car code))
             ((NIL VALUES0 TAGBODY-CLOSE-NIL) ; produzieren Wert NIL
               (return-from value 'FALSE) ; Damit können wir die Schleife abbrechen
             )
             ((T CONS LIST LIST*) ; produzieren Wert /= NIL
               ; (LIST n) und (LIST* n) wegen n>0.
               (return-from value 'TRUE) ; Damit können wir die Schleife abbrechen
             )
             (CONST
               (unless (and (cddr (car code)) (eq (const-horizont (third (car code))) ':form))
                 ; (CONST n) produziert Wert /= NIL, weil der Wert schon zur
                 ; Compile-Zeit bekannt ist und die Konstante NIL in make-const-code
                 ; bereits speziell behandelt wurde.
                 (return-from value 'TRUE) ; Damit können wir die Schleife abbrechen
               )
               (return-from value nil)
             )
             (NOT (setq invert (not invert))) ; invertiere später den booleschen Wert
             ((UNBIND1 SKIP SKIPI SKIPSP STORE STOREI STOREV STOREC STOREIC SETVALUE
               VALUES1 BLOCK-CLOSE TAGBODY-CLOSE CATCH-CLOSE UNWIND-PROTECT-CLEANUP
             )) ; keine Änderung des 1. Werts -> weiter in der Codeliste
             (t (return-from value nil))
           )
           (setq code (cdr code))
         )
         (when code
           ; code ist das Anfangslabel.
           ; Inspiziere alle Sprünge auf das Label code:
           (let ((bisher nil))
             ; bisher = FALSE, falls bisher alle Sprünge den booleschen Wert
             ;                 FALSE mitbringen,
             ; bisher = TRUE, falls bisher alle Sprünge den booleschen Wert
             ;                TRUE mitbringen,
             ; bisher = NIL am Anfang.
             ; Falls ein Sprung einen unbekannten booleschen Wert mitbringt,
             ; kann man die Schleife gleich verlassen.
             (flet ((neu (value)
                      (cond ((null bisher) (setq bisher value))
                            ((not (eq value bisher)) (return-from value nil))
                   )) )
               (dolist (ref (symbol-value code))
                 (if (integerp ref)
                   (let ((refcode (first (aref *code-parts* ref)))) ; der Wegsprung hierher
                     ; Ein Wegsprung mit undefinierten Werten kann das nicht sein.
                     (case (first refcode)
                       (JMP
                         (if (third refcode)
                           ; Wert vor dem Sprung bekannt
                           (neu (third refcode))
                           ; Wert vor dem Sprung unbekannt
                           (return-from value nil)
                       ) )
                       ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                         (when (eq code (second refcode)) (neu 'TRUE))
                         (when (eq code (third refcode)) (neu 'FALSE))
                       )
                       (t (err)) ; JMPHASH hat undefinierte Werte, und die
                                 ; anderen Wegsprünge enthalten keine Labels.
                   ) )
                   (case (first ref)
                     ((JMPIFBOUNDP BLOCK-OPEN CATCH-OPEN)
                       (return-from value nil) ; Da können wir nichts aussagen
                     )
                     (t (err)) ; An den Labels in TAGBODY-OPEN, JSR,
                               ; UNWIND-PROTECT-OPEN, UNWIND-PROTECT-CLOSE
                               ; liegen undefinierte Werte vor.
         ) ) ) ) ) )
         nil ; Default: nichts aussagbar
      ))
) ) )

(defun optimize-jmpcase (index code)
  (when (eq (first (car code)) 'JMPCASE)
    ; Code endet mit (JMPCASE ...)
    (let ((true-label (second (car code)))
          (false-label (third (car code))))
      (if (eq true-label false-label)
        ; (JMPCASE label label) --> (JMP label ..)
        (progn
          (setf (car code) `(JMP ,true-label ,(get-boolean-value (cdr code))))
          ; doppelte Referenz wird zu einer einfachen:
          (setf (symbol-value true-label)
                (delete index (symbol-value true-label) :count 1)
          )
          ; und weiter optimieren:
          (optimize-part code)
          (optimize-short (get true-label 'code-part))
        )
        (when (and (null (get true-label 'for-value))
                   (null (get false-label 'for-value))
              )
          ; Versuche NOTs zu eliminieren:
          (let ((invert 0)
                (cr1 code)
                (cr2 (cdr code))) ; stets cr2 = (cdr cr1)
            (loop
              (when (atom cr2) (return))
              (case (first (car cr2))
                ((UNBIND1 SKIP SKIPI SKIPSP VALUES1 BLOCK-CLOSE TAGBODY-CLOSE
                  CATCH-CLOSE UNWIND-PROTECT-CLEANUP
                 ) ; diese Operationen brauchen keine Werte und lassen
                   ; den 1. Wert unverändert
                 (shiftf cr1 cr2 (cdr cr2))
                )
                (NOT
                  (setf (cdr cr1) (setq cr2 (cdr cr2))) ; (NOT) streichen
                  (incf invert)
                )
                (t (return))
            ) )
            ; invert = Anzahl, wie oft (NOT) gestrichen wurde
            (when (oddp invert)
              ; true-label und false-label vertauschen:
              (setf (car code) `(JMPCASE ,false-label ,true-label))
            )
            (when (plusp invert)
              ; und weiter optimieren:
              (optimize-part code)
              (optimize-short index)
        ) ) )
) ) ) )

(defun optimize-value (index &optional (code (aref *code-parts* index)))
  (let ((item (car code)))
    (case (first item)
      ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
        ; (JMPCASE/... true-label false-label)
        (let ((true-label (second item))
              (false-label (third item)))
          (when (or (and (eq (first item) 'JMPCASE1-TRUE)
                         (not (eq (get true-label 'for-value) 'ALL))
                         ; Wertezahl 1 wird bei true-label nicht gebraucht
                         ; (JMPCASE1-TRUE ...) --> (JMPCASE ...)
                    )
                    (and (eq (first item) 'JMPCASE1-FALSE)
                         (not (eq (get false-label 'for-value) 'ALL))
                         ; Wertezahl 1 wird bei false-label nicht gebraucht
                         ; (JMPCASE1-FALSE ...) --> (JMPCASE ...)
                )   )
            (setq item (setf (car code) `(JMPCASE ,@(rest item))))
            ; Weitere mögliche Optimierungen:
            (optimize-jmpcase index code)
          )
          ; Versuche, den booleschen Wert an dieser Stelle zu ermitteln
          ; und vereinfache gegebenenfalls:
          (case (get-boolean-value (cdr code))
            (TRUE ; Sprung geht immer auf true-label
              ; Referenz auf false-label streichen:
              (setf (symbol-value false-label)
                (delete index (symbol-value false-label))
              )
              (setf (car code) `(JMP ,true-label TRUE))
              (when (eq (first item) 'JMPCASE1-TRUE)
                (push '(VALUES1) (cdr code))
                (simplify code)
              )
              (optimize-part code) ; weitere mögliche Optimierung
              ; weitere mögliche Optimierungen:
              (optimize-label false-label) ; wegen verringerter Referenzen
              (optimize-short index) ; wegen obigem optimize-part
            )
            (FALSE
              ; Referenz auf true-label streichen
              (setf (symbol-value true-label)
                (delete index (symbol-value true-label))
              )
              (setf (car code) `(JMP ,false-label FALSE))
              (when (eq (first item) 'JMPCASE1-FALSE)
                (push '(VALUES1) (cdr code))
                (simplify code)
              )
              (optimize-part code) ; weitere mögliche Optimierung
              ; weitere mögliche Optimierungen:
              (optimize-label true-label) ; wegen verringerter Referenzen
              (optimize-short index) ; wegen obigem optimize-part
      ) ) ) )
      (JMP
        (let ((label (second item)))
          (when (get label 'for-value)
            ; Wert wird benötigt
            (when (null (third item))
              ; aber er ist unbekannt.
              ; Vielleicht lässt sich der Wert herausbekommen ?
              (let ((value (get-boolean-value (cdr code))))
                (when value
                  (setf (car code) `(JMP ,label ,value))
                  ; Wert jetzt bekannt, lässt sich vielleicht verwenden:
                  (optimize-value (get label 'code-part))
) ) ) ) ) ) ) ) )

; coalesce legt gleiche Codeteile in den gegebenen Codestücken soweit wie
; möglich zusammen und liefert als Ergebnis ein Flag, ob etwas geändert wurde.
(defun coalesce (&optional (indexlist
                             ; Liste aller möglichen Indizes
                             (let ((L '()))
                               (dotimes (i (fill-pointer *code-parts*)) (push i L))
                               (nreverse L)
                )          ) )
  (let ((parts-ht ; Eine Hashtabelle, die eine Abbildung realisiert:
                  ; Codeende --> Liste aller Indizes von Codestücken,
                  ;              die damit enden
          (let ((ht (make-hash-table :test #'equal :size (length indexlist))))
            (dolist (index indexlist)
              (let ((code (aref *code-parts* index))) ; ein Codestück
                ; Wegen der Vereinfachungsregel für "kurze" Codestücke werden
                ; nur Teile zusammengelegt, die in mindestens den letzten 3
                ; Operationen übereinstimmen.
                (when (and (consp code) (consp (cdr code)) (consp (cddr code)))
                  (push index
                    (gethash (list* (first code) (second code) (third code))
                             ht '()
                  ) )
            ) ) )
            ht
        ) )
        (modified nil))
    ; Dann über die möglichen Codeenden iterieren:
    (maphash
      #'(lambda (code-beginning indices)
          (declare (ignore code-beginning))
          (when (cdr indices) ; mindestens zwei Indizes mit diesem Codeende?
            ; Versuche, möglichst langes Codestück zusammenzulegen:
            (let ((codes ; Liste der zusammenzulegenden Codestücke
                    (mapcar #'(lambda (i) (aref *code-parts* i)) indices)
                  )
                  (new-code '()) ; hier wird der gemeinsame Code gesammelt
                  (new-index (fill-pointer *code-parts*)) ; Index dafür
                  (new-order ; das gemeinsame Stück wird beim letzten Teil einzusortiert
                    (reduce #'max (mapcar #'(lambda (i) (aref *code-positions* i)) indices))
                 ))
              (loop
                ; stimmen noch alle überein?
                (unless (every #'consp codes) (return))
                (let* ((code1 (first codes)) ; ein beliebiges der Codestücke
                       (code11 (car code1))) ; dessen letzte Operation
                  (unless (every #'(lambda (code) (equal (car code) code11))
                                 (rest codes)
                          )
                    (return)
                  )
                  ; ja. Alle Codestücke aus codes um eine Operation verkürzen:
                  (mapc #'(lambda (code index) ; Referenzen löschen
                            (remove-references (car code) index)
                          )
                        codes indices
                  )
                  ; verkürzen: (setq codes (mapcar #'cdr codes)), oder:
                  (mapl #'(lambda (codesr)
                            (setf (car codesr) (cdr (car codesr)))
                          )
                        codes
                  )
                  (push code11 new-code) ; new-code verlängern
                  (note-references code11 new-index)
              ) )
              (let* ((new-label (make-label 'ALL))
                     ; Alle Codestücke aus codes wurden verkürzt, sie werden
                     ; jetzt verlängert um ein (JMP new-label NIL).
                     (jmpop `(JMP ,new-label NIL)))
                (mapc #'(lambda (code index)
                          (setf (aref *code-parts* index) (cons jmpop code))
                        )
                      codes indices
                )
                (setf (symbol-value new-label) indices) ; Referenzen auf new-label
                (setf (get new-label 'code-part) new-index)
                (vector-push-extend (nreconc new-code new-label) *code-parts*)
                (vector-push-extend new-order *code-positions*)
              )
              ; weitere mögliche Optimierungen:
              (optimize-part (aref *code-parts* new-index))
              (coalesce indices)
              (setq modified t) ; Veränderung hat stattgefunden
        ) ) )
      parts-ht
    )
    modified
) )

; Die Hauptfunktion des 3. Schritts:
; Führt alle Optimierungen durch, und fasst dann alle Codestücke wieder zu
; einer einzigen Codeliste zusammen und liefert diese.
(defun optimize-all ()
  ; Optimierungen:
  (loop
    ; Optimierungen aufrufen:
    ; Wird eine fündig, so ruft sie auch gleich die Optimierungs-
    ; schritte auf, die sich dadurch ergeben könnten. Daher brauchen
    ; sie hier nur einmal aufgeführt zu werden.
    ; Vorsicht hier: durch die Optimierungen können *code-parts* und sein
    ; Inhalt sich völlig verändern.
    (do ((index 0 (1+ index)))
        ((eql index (fill-pointer *code-parts*)))
      (let ((code (aref *code-parts* index)))
        (when code
          (let* ((lastc (last code))
                 (label (cdr lastc)))
            (when label
              (unless (eql index (get label 'code-part))
                (compiler-error 'optimize-all 'code-part)
            ) )
            (optimize-label label index code lastc)
      ) ) )
      (let ((code (aref *code-parts* index)))
        (when code
          (optimize-jmpcase index code)
      ) )
      (let ((code (aref *code-parts* index)))
        (when code
          (optimize-value index code)
      ) )
      (let ((code (aref *code-parts* index)))
        (when code
          (optimize-short index code)
    ) ) )
    (unless (coalesce) (return)) ; (coalesce) tat nichts -> fertig
  )
  ; Zu einer einzigen Codeliste zusammenfassen:
  ; (Dabei werden die Labels nun Listenelemente im Code statt nur NTHCDRs.)
  (let ((start-index 0)) ; Start-"Label" NIL beginnt Codestück Nr. 0
    ; Erst jeweils ein Codestück, das mit label anfängt, wenn möglich an ein
    ; Codestück anhängen, das mit einem JMP oder JMPCASE/... zu label endet.
    (do ((index (fill-pointer *code-parts*)))
        ((eql (decf index) 0)) ; index durchläuft die Indizes von *code-parts*
                               ; von oben nach unten, ausgenommen start-index=0.
      (let ((code (aref *code-parts* index)))
        (when code
          (loop
            ; Betrachte das Label am Ende von code, im Codestück Nr. index:
            (let* ((lastc (last code)) ; letztes Cons von code
                   (label (cdr lastc)) ; Label am Ende von code
                   (refs (symbol-value label)) ; Referenzen darauf
                   (pos (aref *code-positions* index)) ; Position von code
                   (jmp-ref nil) ; bisher beste gefundene JMP-Referenz auf label
                   (jmpcase-ref nil) ; bisher beste gefundene JMPCASE-Referenz auf label
                   (jmpcase1-ref nil)) ; bisher beste gefundene JMPCASE1-...-Referenz auf label
              (if (null label)
                ; Das Start-Code-Stück wurde umgehängt!
                (progn
                  (setq start-index index)
                  (return) ; zum nächsten Index
                )
                (flet ((better (new-ref old-ref)
                         ; Eine Referenz new-ref ist "besser" als eine andere
                         ; old-ref, wenn sie näher dran ist. Dabei haben
                         ; Vorwärtsreferenzen generell Priorität gegenüber
                         ; Rückwärtsreferenzen.
                         (or (null old-ref) ; noch gar kein old-ref?
                             (let ((old-pos (aref *code-positions* old-ref))
                                   (new-pos (aref *code-positions* new-ref)))
                               (if (> old-pos pos) ; Habe bisher nur Rückwärtssprung?
                                 ; ja: new-pos ist besser, falls es
                                 ; < pos (Vorwärtssprung) oder
                                 ; >=pos, <=old-pos (kürzerer Rückwärtssprung) ist.
                                 (<= new-pos old-pos)
                                 ; nein: new-pos ist besser, falls es
                                 ; <=pos, >=old-pos (kürzerer Vorwärtssprung) ist.
                                 (<= old-pos new-pos pos)
                      )) )   ) )
                  (macrolet ((update (old-ref new-ref) ; zur Bestimmung des bisher Besten
                               `(when (better ,new-ref ,old-ref)
                                  (setq ,old-ref ,new-ref)
                                )
                            ))
                    ; Bestimme die beste Referenz, an die das Codestück
                    ; gehängt werden kann:
                    (dolist (refindex refs)
                      (when (and (integerp refindex)
                                 (not (eql refindex index)) ; nicht an sich selber hängen!
                            )
                        (let ((refcode1 (car (aref *code-parts* refindex))))
                          (case (first refcode1)
                            (JMP ; mögliches Anhängen an (JMP label ...)
                              (update jmp-ref refindex)
                            )
                            (JMPCASE ; mögliches Anhängen an (JMPCASE ... label ...)
                              (update jmpcase-ref refindex)
                            )
                            (JMPCASE1-TRUE ; mögliches Anhängen an (JMPCASE1-TRUE ... label)
                              (when (eq label (third refcode1))
                                (update jmpcase1-ref refindex)
                            ) )
                            (JMPCASE1-FALSE ; mögliches Anhängen an (JMPCASE1-FALSE label ...)
                              (when (eq label (second refcode1))
                                (update jmpcase1-ref refindex)
                            ) )
                    ) ) ) )
                    (cond (jmp-ref ; an (JMP label) anhängen
                            (setf (cdr lastc)
                                  (cons label (cdr (aref *code-parts* jmp-ref)))
                            )
                            (setf (aref *code-parts* jmp-ref) nil)
                            (setq code lastc)
                          )
                          (jmpcase1-ref
                            (let* ((refcode (aref *code-parts* jmpcase1-ref))
                                   (refcode1 (car refcode))
                                   (jmpop
                                     (if (eq label (second refcode1))
                                       `(JMPIFNOT1 ,(third refcode1))
                                       `(JMPIF1 ,(second refcode1))
                                  )) )
                              (setf (cdr lastc) (list* label jmpop (cdr refcode)))
                              (setf (aref *code-parts* jmpcase1-ref) nil)
                              (setq code lastc)
                          ) )
                          (jmpcase-ref
                            (let* ((refcode (aref *code-parts* jmpcase-ref))
                                   (refcode1 (car refcode))
                                   (for-value (or (get (second refcode1) 'for-value)
                                                  (get (third refcode1) 'for-value)
                                   )          )
                                   (jmpop
                                     (if (eq label (second refcode1))
                                       `(JMPIFNOT ,(third refcode1) ,for-value)
                                       `(JMPIF ,(second refcode1) ,for-value)
                                  )) )
                              (setf (cdr lastc) (list* label jmpop (cdr refcode)))
                              (setf (aref *code-parts* jmpcase-ref) nil)
                              (setq code lastc)
                          ) )
                          (t ; kein Anhängen möglich
                            (return) ; zum nächsten Index
          ) ) ) ) ) )     )
    ) ) )
    ; Sicherstellen, dass das Anfangs-Stück auch an den Anfang kommt:
    ; (Das würde auch gehen, indem bei jeder der obigen Anhängungen
    ; ein (setf (aref *code-positions* index) (aref *code-positions* jmp..-ref))
    ; gemacht würde. Wieso tun wir das nicht??)
    (setf (aref *code-positions* start-index) 0)
    ; Codeliste zusammensetzen:
    (let ((code-parts (map 'list #'cons *code-parts* *code-positions*)))
      (setq code-parts (delete-if-not #'car code-parts)) ; code=nil bedeutet: gestrichen
      (setq code-parts (sort code-parts #'> :key #'cdr)) ; nach Reihenfolge sortieren
      ; Die Teile sind jetzt in der richtigen Ordnung, nur umgekehrt.
      (let ((codelist '()))
        (dolist (code-part code-parts)
          (let ((code (car code-part)))
            ; code an codelist anhängen, dabei aber den Wegsprung umwandeln:
            (let ((item (car code)))
              (case (first item)
                (JMP (setf (car code) `(JMP ,(second item))))
                (JMPCASE ; (JMPCASE true-label false-label)
                         ; --> (JMPIFNOT false-label fv) (JMP true-label)
                  (setq code
                    (list* `(JMP ,(second item))
                           `(JMPIFNOT ,(third item)
                                      ,(or (get (second item) 'for-value)
                                           (get (third item) 'for-value)
                                       )
                            )
                           (cdr code)
                ) ) )
                (JMPCASE1-TRUE ; (JMPCASE1-TRUE true-label false-label)
                               ; --> (JMPIF1 true-label) (JMP false-label)
                  (setq code
                    (list* `(JMP ,(third item))
                           `(JMPIF1 ,(second item))
                           (cdr code)
                ) ) )
                (JMPCASE1-FALSE ; (JMPCASE1-FALSE true-label false-label)
                                ; --> (JMPIFNOT1 false-label) (JMP true-label)
                  (setq code
                    (list* `(JMP ,(second item))
                           `(JMPIFNOT1 ,(third item))
                           (cdr code)
            ) ) ) ) )
            ; Label zum Listenelement machen:
            (let ((lastc (last code)))
              (when (cdr lastc)
                (setf (cdr lastc) (list (cdr lastc)))
            ) )
            ; Umdrehen und vor codelist hängen (deswegen wurde vorhin
            ; mit #'> statt #'< sortiert):
            (setq codelist (nreconc code codelist))
        ) )
        codelist
) ) ) )

#|
;; Debugging hints:
(in-package "SYSTEM")
(setq *print-circle* t)
(trace compile-to-lap)
(trace (traverse-anode :post-print *code-part*))
(trace (optimize-part    :pre-print *code-parts* :post-print *code-parts*)
       (optimize-label   :pre-print *code-parts* :post-print *code-parts*)
       (optimize-short   :pre-print *code-parts* :post-print *code-parts*)
       (optimize-jmpcase :pre-print *code-parts* :post-print *code-parts*)
       (optimize-value   :pre-print *code-parts* :post-print *code-parts*)
       (coalesce         :pre-print *code-parts* :post-print *code-parts*)
       (optimize-all     :pre-print *code-parts* :post-print *code-parts*)
)
(trace simplify)
;; Move out suspect code to a separate file which you load interpreted.

;; Special debugging checks:
(defun optimize-check ()
  (do ((index 0 (1+ index)))
      ((eql index (fill-pointer *code-parts*)))
    (let ((code (aref *code-parts* index)))
      (when code
        (let* ((lastc (last code))
               (label (cdr lastc)))
          (when label
            (unless (eql index (get label 'code-part))
              (compiler-error 'optimize-check 'code-part)
) ) ) ) ) ) )
(trace
  (optimize-part    :pre (optimize-check) :post (optimize-check) :suppress-if t)
  (optimize-label   :pre (optimize-check) :post (optimize-check) :suppress-if t)
  (optimize-short   :pre (optimize-check) :post (optimize-check) :suppress-if t)
  (optimize-jmpcase :pre (optimize-check) :post (optimize-check) :suppress-if t)
  (optimize-value   :pre (optimize-check) :post (optimize-check) :suppress-if t)
  (coalesce         :pre (optimize-check) :post (optimize-check) :suppress-if t)
  (optimize-all     :pre (optimize-check) :post (optimize-check) :suppress-if t)
)
|#

#| Was ist mit den folgenden möglichen Optimierungen??

10. Kommt vor einem (JMP label) ein (UNWIND-PROTECT-CLEANUP) und vor dem
   label ein (UNWIND-PROTECT-3 cleanup-label), so muss es sich um denselben
   UNWIND-PROTECT-Frame handeln, und man kann (UNWIND-PROTECT-CLEANUP)
   streichen und (JMP label) durch (JMP newlabel) ersetzen, wobei newlabel
   ein neues Label ist, das vor dem (evtl. zu ergänzenden) (UNWIND-PROTECT-2)
   vor cleanup-label sitzt:
   (UNWIND-PROTECT-CLEANUP) (JMP label) ...
   ... [(UNWIND-PROTECT-2)] cleanup-label ... (UNWIND-PROTECT-3 cleanup-label) label
   -->
   (JMP newlabel) ...
   ... newlabel (UNWIND-PROTECT-2) cleanup-label ... (UNWIND-PROTECT-3 cleanup-label) label

11. Kommt nach einem Label label ein (NIL), so darf jeder (JMPIFNOT label)
   und jeder (JMPIFNOT1 label) durch ein (JMPIFNOT1 z) ersetzt werden,
   wo z ein neues Label nach dem (NIL) ist:
          (JMPIFNOT label) ... label (NIL) ...
   -->       (JMPIFNOT1 z) ... label (NIL) z ...

|#

; Führt den 1. und 2.,3. Schritt aus:
(defun compile-to-LAP ()
  (let ((*code-parts* (make-array 10 :adjustable t :fill-pointer 0))
        (*code-positions* (make-array 10 :adjustable t :fill-pointer 0)))
    ; Expandiert den Code des Fnode *func* und teilt ihn in Stücke auf.
    ; Hinterlässt seine Werte in *code-parts* und *code-positions*.
    (let ((*code-part* (list '(START))) ; NIL als Start-"Label"
          (*code-index* 0)
          (*dead-code* nil)
          (*label-subst* '())
          (*current-value* nil)
          (*current-vars* '()))
      (traverse-anode (anode-code (fnode-code *func*)))
    )
    ; Optimiert in *code-parts* und *code-positions*, fasst dann den Code
    ; in einer Liste zusammen und liefert diese:
    (let ((code-list (optimize-all)))
      (unless (equal (pop code-list) '(START))
        (compiler-error 'compile-to-LAP 'start)
      )
      code-list
) ) )


#|
                            4. Schritt:
                      Eliminieren von (CONST n)

Generische Funktionen haben eine feste Länge. Die Konstanten werden im
VENV-Const aufbewahrt. In diesem Schritt werden umgewandelt:
  (LOADV k m)    -->  (LOADV k+1 m)
  (STOREV k m)   -->  (STOREV k+1 m)
  (CONST n [c])  -->  (LOADV 0 n)
  (VENV)         -->  (LOADV 0 0)
  (JMPHASH n ht label . labels)  -->  (JMPHASHV n ht label . labels)
  (GETVALUE n)         -->  illegal
  (SETVALUE n)         -->  illegal
  (BIND n)             -->  illegal
  (COPY-CLOSURE m n)   -->  illegal
  (CALL k n)           -->  illegal
  (CALL0 n)            -->  illegal
  (CALL1 n)            -->  illegal
  (CALL2 n)            -->  illegal
  (BLOCK-OPEN n label) -->  illegal
  (RETURN-FROM n)      -->  illegal
  (TAGBODY-OPEN n ...) -->  illegal
  (GO n l)             -->  illegal
|#

(defun CONST-to-LOADV (code-list)
  (do ((codelistr code-list (cdr codelistr)))
      ((null codelistr))
    (let ((item (car codelistr)))
      (when (consp item)
        (case (first item)
          ((LOADV STOREV)
            (setf (car codelistr)
                  `(,(first item) ,(1+ (second item)) ,@(cddr item))
          ) )
          (CONST
            (setf (car codelistr) `(LOADV 0 ,(second item)))
          )
          (VENV
            (setf (car codelistr) `(LOADV 0 0))
          )
          (JMPHASH
            (setf (car codelistr) `(JMPHASHV ,@(cdr item)))
          )
          ((GETVALUE SETVALUE BIND COPY-CLOSURE CALL CALL0 CALL1 CALL2
            BLOCK-OPEN RETURN-FROM TAGBODY-OPEN GO)
            (compiler-error 'CONST-to-LOADV "Illegal-in-GF")
          )
  ) ) ) )
  code-list
)


#|
                            5. Schritt:
                   Bestimmung des Stackbedarfs

Dieser Schritt bestimmt, wieviel SP-Einträge die Funktion maximal braucht.
|#

(defun SP-depth (code-list)
  ;; We have to compute the maximum SP depth in the two spd dimensions
  ;; separately. Instead of walking through the code twice, we walk only once.
  ;; When we see that a label can be reached with depths (d1 . d2) and (e1 . e2)
  ;; we pretend it can be reached with depth ((max d1 e1) . (max d2 e2)).
  ;; Similarly, when we have tracked a label at depths (d1 . d2) and (e1 . e2),
  ;; we pretend having tracked it at depth ((max d1 e1) . (max d2 e2)).
  ;; This is allowed because we are only interested in the two separate maxima.
  ;; Think of two different machines computing the two maxima in parallel.
  (let ((max-depth-1 0) (max-depth-2 0) ; bisherige Maximal-Tiefe
        (unseen-label-alist '()) ; Labels, ab denen noch verfolgt werden muss
        (seen-label-alist '()) ; Labels, die schon verfolgt wurden
          ; beides Alisten ((label . depth) ...)
          ; Es ist durchaus möglich, dass dasselbe Codestück mit unterschied-
          ; lichen SP-Tiefen durchgeführt werden kann (nämlich dann, wenn es
          ; mit einem Wegsprung THROW, RETURN-FROM, RETURN-FROM-I, GO, GO-I
          ; oder BARRIER endet)!
          ; seen-label-alist enthält zu jedem Label die maximale Tiefe, mit
          ; der ab diesem Label schon verfolgt wurde.
          ; unseen-label-alist enthält zu jedem Label die maximale bisher
          ; notierte Tiefe, mit der ab diesem Label noch verfolgt werden muss.
        (mitte code-list) ; restliche Codeliste
        (depth (spd 0 0)) ; aktuelle Tiefe
       )
    (macrolet ((check-depth (wanted-depth)
                 ; überprüft, ob depth gleich der Tiefe wanted-depth ist
                 `(unless (equal depth ,wanted-depth)
                    (compiler-error 'SP-depth)
                  )
              ))
      (loop
        ; mitte läuft durch die Codeliste, von der aktuellen Position
        ; bis zum nächsten Wegsprung, und zählt die Tiefe mit.
        (loop
          (when (null mitte) (return))
          (let ((item (car mitte)))
            (if (atom item)
              ; Label
              (let ((h (assoc item seen-label-alist)))
                (if h
                  (if (spd<= depth (cdr h))
                    (return)
                    (setf (cdr h) (setf depth (spdmax depth (cdr h))))
                  )
                  (push (cons item depth) seen-label-alist)
              ) )
              ; Instruktion
              (macrolet ((note-label (labelform)
                           ; notiere, dass zu label gesprungen werden kann
                           (let ((label (gensym)))
                             `(let* ((,label ,labelform)
                                     (h (assoc ,label seen-label-alist)))
                                (unless (and h (spd<= depth (cdr h)))
                                  (let ((depth (if h (spdmax depth (cdr h)) depth)))
                                    (setq h (assoc ,label unseen-label-alist))
                                    (if h
                                      (unless (spd<= depth (cdr h))
                                        (setf (cdr h) (spdmax depth (cdr h)))
                                      )
                                      (push (cons ,label depth) unseen-label-alist)
                              ) ) ) )
                         ) )
                         (note-inc (amount)
                           ; notiere, dass depth um amount erhöht wird
                           `(progn
                              (setq depth (spd+ depth ,amount))
                              (setq max-depth-1 (max max-depth-1 (car depth)))
                              (setq max-depth-2 (max max-depth-2 (cdr depth)))
                            )
                         )
                         (note-dec (amount)
                           ; notiere, dass depth um amount erniedrigt wird
                           `(progn
                              (setq depth (spd- depth ,amount))
                              (when (or (minusp (car depth)) (minusp (cdr depth)))
                                (compiler-error 'SP-depth "<0")
                            ) )
                         )
                         (note-jmp ()
                           ; notiere, dass weggesprungen wird
                           `(return)
                        ))
                (case (first item)
                  (JMP ; (JMP label)
                    (note-label (second item))
                    (note-jmp)
                  )
                  ((JMPIF JMPIF1 JMPIFNOT JMPIFNOT1 JMPIFBOUNDP) ; (JMP... label)
                    (note-label (second item))
                  )
                  ((JMPHASH JMPHASHV JMPTAIL) ; (JMPHASH.. n ht label . labels), (JMPTAIL m n label)
                    (dolist (label (cdddr item)) (note-label label))
                    (note-jmp)
                  )
                  (JSR ; (JSR n label)
                    (let ((depth (spd 0 0))) (note-label (third item)))
                  )
                  ((BARRIER THROW RETURN-FROM RETURN-FROM-I GO GO-I) ; (BARRIER), (THROW), (RETURN-FROM n), (RETURN-FROM-I k n), (GO n l), (GO-I k n l)
                    (note-jmp)
                  )
                  ((RET RETGF) ; (RET), (RETGF)
                    (check-depth (spd 0 0))
                    (note-jmp)
                  )
                  (PROGV ; (PROGV)
                    (note-inc (spd 1 0))
                  )
                  (CATCH-OPEN ; (CATCH-OPEN label)
                    (note-label (second item))
                    (note-inc (spd 2 1))
                  )
                  (CATCH-CLOSE ; (CATCH-CLOSE)
                    (note-dec (spd 2 1))
                  )
                  (UNWIND-PROTECT-OPEN ; (UNWIND-PROTECT-OPEN label)
                    ; eigentlich: (note-inc (spd 2 1))
                    (note-inc (spd 3 0)) (note-label (second item)) (note-dec (spd 3 0))
                    (note-inc (spd 2 1))
                  )
                  (UNWIND-PROTECT-NORMAL-EXIT ; (UNWIND-PROTECT-NORMAL-EXIT), danach kommt label
                    (note-dec (spd 2 1)) (note-inc (spd 3 0))
                  )
                  (UNWIND-PROTECT-CLOSE ; (UNWIND-PROTECT-CLOSE label)
                    ; eigentlich: (note-dec (spd 3 0))
                    (note-label (second item)) (note-dec (spd 3 0))
                  )
                  (UNWIND-PROTECT-CLEANUP ; (UNWIND-PROTECT-CLEANUP)
                    ; eigentlich: (note-dec (spd 2 1)) (note-inc (spd 3 0)) ... (note-dec (spd 3 0))
                    (note-dec (spd 2 1))
                  )
                  (BLOCK-OPEN ; (BLOCK-OPEN n label)
                    (note-label (third item))
                    (note-inc (spd 2 1))
                  )
                  (BLOCK-CLOSE ; (BLOCK-CLOSE)
                    (note-dec (spd 2 1))
                  )
                  (TAGBODY-OPEN ; (TAGBODY-OPEN n label1 ... labelm)
                    (note-inc (spd 1 1))
                    (dolist (label (cddr item)) (note-label label))
                  )
                  ((TAGBODY-CLOSE-NIL TAGBODY-CLOSE) ; (TAGBODY-CLOSE-NIL), (TAGBODY-CLOSE)
                    (note-dec (spd 1 1))
                  )
                  (HANDLER-OPEN ; (HANDLER-OPEN n v k label1 ... labelm)
                    (check-depth (fourth item))
                    (dolist (label (cddddr item)) (note-label label))
                  )
                  ((MVCALLP HANDLER-BEGIN) ; (MVCALLP), (HANDLER-BEGIN)
                    (note-inc (spd 1 0))
                  )
                  (MVCALL ; (MVCALL)
                    (note-dec (spd 1 0))
                  )
                  (SKIPSP ; (SKIPSP k1 k2)
                    (note-dec (spd (second item) (third item)))
                  )
                  (SKIPI ; (SKIPI k1 k2 n)
                    (note-dec (spd (+ (second item) 1) (third item)))
                  )
              ) )
          ) )
          (setq mitte (cdr mitte))
        )
        ; Nächstes zu verfolgendes Label suchen:
        (loop
          (when (null unseen-label-alist) ; fertig ?
            (return-from SP-depth (spd max-depth-1 max-depth-2))
          )
          (let* ((unseen (pop unseen-label-alist))
                 (label (car unseen))) ; nächstes zu verfolgendes Label
            (setq depth (cdr unseen))
            (let ((h (assoc label seen-label-alist)))
              (unless (and h (spd<= depth (cdr h)))
                (when h (setq depth (spdmax depth (cdr h))))
                ; Ab diesem Label die Codeliste abarbeiten:
                ; (Dadurch wird (label . depth) in seen-label-alist aufgenommen,
                ; es ist bereits aus unseen-label-alist entfernt.)
                (setq mitte (member label code-list :test #'eq))
                (return)
        ) ) ) )
) ) ) )


#|
                            6. Schritt:
                 Einführung von Kurz-Operationen

Dieser Schritt arbeitet auf der Codeliste und verändert sie dabei destruktiv.

1. (ATOM) (JMPIF label NIL)             --> (JMPIFATOM label)
   (ATOM) (JMPIFNOT label NIL)          --> (JMPIFCONSP label)
   (CONSP) (JMPIF label NIL)            --> (JMPIFCONSP label)
   (CONSP) (JMPIFNOT label NIL)         --> (JMPIFATOM label)
   (ATOM)                               --> (PUSH) (CALLS ATOM)
   (CONSP)                              --> (PUSH) (CALLS CONSP)

2. (NIL) (PUSH)                         --> (NIL&PUSH)
   (NIL) (PUSH) ... (NIL) (PUSH)        --> (PUSH-NIL n)
   (NIL) (STORE n)                      --> (NIL&STORE n)
   (PUSH-NIL 1)                         --> (NIL&PUSH)

3. (T) (PUSH)                           --> (T&PUSH)
   (T) (STORE n)                        --> (T&STORE n)

4. (CONST n c)                          --> (CONST n)
   (CONST n) (PUSH)                     --> (CONST&PUSH n)
   (CONST n) (SYMBOL-FUNCTION) (PUSH)   --> (CONST&SYMBOL-FUNCTION&PUSH n)
   (CONST n) (SYMBOL-FUNCTION) (STORE m)--> (CONST&SYMBOL-FUNCTION&STORE n m)
   (CONST n) (SYMBOL-FUNCTION)          --> (CONST&SYMBOL-FUNCTION n)

5. (COPY-CLOSURE n m) (PUSH)            --> (COPY-CLOSURE&PUSH n m)

6. (LOAD n) (PUSH)                      --> (LOAD&PUSH n)
   (LOAD k) (STOREC n m)                --> (LOAD&STOREC k n m)
   (LOAD n) (JMPIF label fv)            --> (LOAD&JMPIF n label)
   (LOAD n) (JMPIFNOT label fv)         --> (LOAD&JMPIFNOT n label)
   (LOAD n) (CAR) (PUSH)                --> (LOAD&CAR&PUSH n)
   (LOAD n) (CDR) (PUSH)                --> (LOAD&CDR&PUSH n)
   (LOAD n) (CDR) (STORE n)             --> (LOAD&CDR&STORE n)
   (LOAD n+1) (CONS) (STORE n)          --> (LOAD&CONS&STORE n)
   (LOAD n) (PUSH) (CALLS 1+) (STORE n) --> (LOAD&INC&STORE n)
   (LOAD n) (PUSH) (CALLS 1-) (STORE n) --> (LOAD&DEC&STORE n)
   (LOAD n) (PUSH) (CALLS 1+) (PUSH)    --> (LOAD&INC&PUSH n)
   (LOAD n) (PUSH) (CALLS 1-) (PUSH)    --> (LOAD&DEC&PUSH n)
   (LOAD n) (CAR) (STORE m)             --> (LOAD&CAR&STORE n m)

7. (JMPIFBOUNDP n l) (NIL) (STORE n) l  --> (UNBOUND->NIL n) l

8. (LOADI n1 n2 n3) (PUSH)              --> (LOADI&PUSH n1 n2 n3)
   (LOADC n1 n2) (PUSH)                 --> (LOADC&PUSH n1 n2)
   (LOADV n1 n2) (PUSH)                 --> (LOADV&PUSH n1 n2)

9. (GETVALUE n) (PUSH)                  --> (GETVALUE&PUSH n)

10. (UNBIND1) ... (UNBIND1)             --> (UNBIND n)

11. (CAR) (PUSH)                        --> (CAR&PUSH)
    (CDR) (PUSH)                        --> (CDR&PUSH)
    (CONS) (PUSH)                       --> (CONS&PUSH)
    (LIST n) (PUSH)                     --> (LIST&PUSH n)
    (LIST* n) (PUSH)                    --> (LIST*&PUSH n)
    (FUNCALL n) (PUS)                   --> (FUNCALL&PUSH n)
    (APPLY n) (PUSH)                    --> (APPLY&PUSH n)

12. (POP) (STORE n)                     --> (POP&STORE n)

13. (SKIP n) (RET)                      --> (SKIP&RET n)
    (SKIP n) (RETGF)                    --> (SKIP&RETGF n)
    ; (RET)                             --> (SKIP&RET 0)
    ; (RETGF)                           --> (SKIP&RETGF 0)
    ; kommt nicht vor, da im Stack stets noch die Closure selbst sitzt

14. (UNWIND-PROTECT-CLOSE label)        --> (UNWIND-PROTECT-CLOSE)

15. (JMPHASH n ht label . labels)       --> (JMPHASH n ht label)
    (JMPHASHV n ht label . labels)      --> (JMPHASHV n ht label)

16. (JSR n label)                       --> (JSR label)
    (JSR n label) (PUSH)                --> (JSR&PUSH label)

17. (CALL m n) (PUSH)                   --> (CALL&PUSH m n)
    (CALL1 n) (PUSH)                    --> (CALL1&PUSH n)
    (CALL2 n) (PUSH)                    --> (CALL2&PUSH n)
    (CALLS1 n) (PUSH)                   --> (CALLS1&PUSH n)
    (CALLS2 n) (PUSH)                   --> (CALLS2&PUSH n)
    (CALLSR m n) (PUSH)                 --> (CALLSR&PUSH m n)
    (CALLC) (PUSH)                      --> (CALLC&PUSH)
    (CALLCKEY) (PUSH)                   --> (CALLCKEY&PUSH)

18. (CALL1 n) (JMPIF label fv)          --> (CALL1&JMPIF n label)
    (CALL1 n) (JMPIFNOT label fv)       --> (CALL1&JMPIFNOT n label)
    (CALL2 n) (JMPIF label fv)          --> (CALL2&JMPIF n label)
    (CALL2 n) (JMPIFNOT label fv)       --> (CALL2&JMPIFNOT n label)
    (CALLS1 n) (JMPIF label fv)         --> (CALLS1&JMPIF n label)
    (CALLS1 n) (JMPIFNOT label fv)      --> (CALLS1&JMPIFNOT n label)
    (CALLS2 n) (JMPIF label fv)         --> (CALLS2&JMPIF n label)
    (CALLS2 n) (JMPIFNOT label fv)      --> (CALLS2&JMPIFNOT n label)
    (CALLSR m n) (JMPIF label fv)       --> (CALLSR&JMPIF m n label)
    (CALLSR m n) (JMPIFNOT label fv)    --> (CALLSR&JMPIFNOT m n label)

19. (CALLS1 n) (STORE k)                --> (CALLS1&STORE n k)
    (CALLS2 n) (STORE k)                --> (CALLS2&STORE n k)
    (CALLSR m n) (STORE k)              --> (CALLSR&STORE m n k)

20. (EQ) (JMPIF label NIL)              --> (JMPIFEQ label)
    (EQ) (JMPIFNOT label NIL)           --> (JMPIFNOTEQ label)
    (CONST n) (EQ) (JMPIF label NIL)    --> (JMPIFEQTO n label)
    (CONST n) (EQ) (JMPIFNOT label NIL) --> (JMPIFNOTEQTO n label)

21. (APPLY n) (SKIP k) (RET)            --> (APPLY&SKIP&RET n k)
    (FUNCALL n) (SKIP k) (RETGF)        --> (FUNCALL&SKIP&RETGF n k)

22. (HANDLER-BEGIN) (PUSH)              --> (HANDLER-BEGIN&PUSH)

23. (BARRIER)                           -->

|#

(let ((CALLS-1+ (CALLS-code (gethash '1+ function-codes)))
      (CALLS-1- (CALLS-code (gethash '1- function-codes)))
      (CALLS-atom (CALLS-code (gethash 'atom function-codes)))
      (CALLS-consp (CALLS-code (gethash 'consp function-codes))))
  (defun insert-combined-LAPs (code-list)
    ; Zunächst die ATOM/CONSP-Umwandlung, weil diese PUSHs einführen kann:
    (do ((crest code-list (cdr crest)))
        ((null crest))
      (let ((item (car crest)))
        (when (consp item)
          (case (first item)
            (CONST ; (CONST n c) -> (CONST n)
              (setf (cddr item) '())
            )
            ((ATOM CONSP)
              (setq item (first item))
              (if (and #| (consp (cdr crest)) |#
                       (consp (cadr crest))
                       (memq (first (cadr crest)) '(JMPIF JMPIFNOT))
                       (null (third (cadr crest)))
                  )
                ; z.B. (ATOM) (JMPIF label NIL) --> (JMPIFATOM label)
                (setf (car crest)
                      `(,(if (eq (first (cadr crest)) 'JMPIF)
                           (if (eq item 'ATOM) 'JMPIFATOM 'JMPIFCONSP)
                           (if (eq item 'ATOM) 'JMPIFCONSP 'JMPIFATOM)
                         )
                        ,(second (cadr crest))
                       )
                      (cdr crest) (cddr crest)
                )
                ; z.B. (ATOM) --> (PUSH) (CALLS ATOM)
                (setf (car crest) '(PUSH)
                      (cdr crest) (cons (if (eq item 'ATOM) CALLS-atom CALLS-consp)
                                        (cdr crest)
                )                 )
    ) ) ) ) ) )
    ; Nun die sonstigen Umformungen: Ein einziger Durchlauf.
    ; Zwei Pointer laufen durch die Codeliste: ...mitte.rechts...
    (do* ((mitte code-list rechts)
          (rechts (cdr mitte) (cdr rechts)))
         ((null mitte))
      (macrolet ((ersetze (length new-code)
                   ; ersetzt die nächsten length Elemente
                   ; (nth 0 mitte) ... (nth (- length 1) mitte)
                   ; durch ein einziges Element new-code.
                   (assert (typep length '(INTEGER 0 4)))
                   `(progn
                      ,(case length
                         (0 `(setf (cdr mitte) (setq rechts (cons (car mitte) rechts))
                                   (car mitte) ,new-code
                         )   )
                         (1 `(setf (car mitte) ,new-code))
                         (t `(setf (car mitte) ,new-code
                                   (cdr mitte) ,(setq rechts
                                                  (case length
                                                    (2 `(cdr rechts))
                                                    (3 `(cddr rechts))
                                                    (4 `(cdddr rechts))
                                                ) )
                       ) )   )
                      (go weiter)
                    )
                ))
        (let ((item (car mitte)))
          (when (consp item)
            ; Untersuchung des Befehls item und der nachfolgenden:
            (when (and #| (consp rechts) |# (consp (car rechts)))
              ; normale Umwandlungen, mit Aneinanderhängen der Argumente:
              (let ((new-op
                      (cdr (assoc (first item)
                                  (case (first (car rechts))
                                    (PUSH  '((T        . T&PUSH)
                                             (CONST    . CONST&PUSH)
                                             (LOADI    . LOADI&PUSH)
                                             (LOADC    . LOADC&PUSH)
                                             (LOADV    . LOADV&PUSH)
                                             (GETVALUE . GETVALUE&PUSH)
                                             (CALL     . CALL&PUSH)
                                             (CALL1    . CALL1&PUSH)
                                             (CALL2    . CALL2&PUSH)
                                             (CALLS1   . CALLS1&PUSH)
                                             (CALLS2   . CALLS2&PUSH)
                                             (CALLSR   . CALLSR&PUSH)
                                             (CALLC    . CALLC&PUSH)
                                             (CALLCKEY . CALLCKEY&PUSH)
                                             (CAR      . CAR&PUSH)
                                             (CDR      . CDR&PUSH)
                                             (CONS     . CONS&PUSH)
                                             (LIST     . LIST&PUSH)
                                             (LIST*    . LIST*&PUSH)
                                             (FUNCALL  . FUNCALL&PUSH)
                                             (APPLY    . APPLY&PUSH)
                                             (COPY-CLOSURE . COPY-CLOSURE&PUSH)
                                             (HANDLER-BEGIN . HANDLER-BEGIN&PUSH)
                                    )       )
                                    (JMPIF
                                      (let ((alist
                                              '((EQ     . JMPIFEQ)
                                                (LOAD   . LOAD&JMPIF)
                                                (CALL1  . CALL1&JMPIF)
                                                (CALL2  . CALL2&JMPIF)
                                                (CALLS1 . CALLS1&JMPIF)
                                                (CALLS2 . CALLS2&JMPIF)
                                                (CALLSR . CALLSR&JMPIF)
                                               )
                                           ))
                                        (when (third (car rechts))
                                          (setq alist (cdr alist))
                                        )
                                        (setf (cddr (car rechts)) '())
                                        alist
                                    ) )
                                    (JMPIFNOT
                                      (let ((alist
                                              '((EQ     . JMPIFNOTEQ)
                                                (LOAD   . LOAD&JMPIFNOT)
                                                (CALL1  . CALL1&JMPIFNOT)
                                                (CALL2  . CALL2&JMPIFNOT)
                                                (CALLS1 . CALLS1&JMPIFNOT)
                                                (CALLS2 . CALLS2&JMPIFNOT)
                                                (CALLSR . CALLSR&JMPIFNOT)
                                               )
                                           ))
                                        (when (third (car rechts))
                                          (setq alist (cdr alist))
                                        )
                                        (setf (cddr (car rechts)) '())
                                        alist
                                    ) )
                                    (STORE '((NIL    . NIL&STORE)
                                             (T      . T&STORE)
                                             (POP    . POP&STORE)
                                             (CALLS1 . CALLS1&STORE)
                                             (CALLS2 . CALLS2&STORE)
                                             (CALLSR . CALLSR&STORE)
                                    )       )
                                    (STOREC '((LOAD . LOAD&STOREC)))
                                    (RET '((SKIP . SKIP&RET)))
                                    (RETGF '((SKIP . SKIP&RETGF)))
                                  )
                                  :test #'eq
                   )) )    )
                (when new-op
                  (ersetze 2 `(,new-op ,@(rest item) ,@(rest (car rechts))))
            ) ) )
            ; weitere Umwandlungen:
            (case (first item)
              ((NIL PUSH-NIL)
                (flet ((nilpusher-p (coder)
                         ; Kommt (NIL) (PUSH) --> 1,
                         ; kommt (PUSH-NIL n) --> n,
                         ; sonst nil.
                         (and #| (consp coder) |# (consp (car coder))
                              (case (first (car coder))
                                (PUSH-NIL (second (car coder)))
                                ((NIL) (when (equal (cadr coder) '(PUSH))
                                         (setf (cdr coder) (cddr coder))
                                         1
                                )      )
                                (t nil)
                      )) )    )
                  (let ((count (nilpusher-p mitte)))
                    (when count
                      (setq rechts (cdr mitte))
                      (loop
                        (let ((next-count (nilpusher-p rechts)))
                          (unless next-count (return))
                          (incf count next-count)
                        )
                        (setq rechts (cdr rechts))
                      )
                      (setf (car mitte) (if (eql count 1) '(NIL&PUSH) `(PUSH-NIL ,count))
                            (cdr mitte) rechts
                      )
                      (go weiter)
              ) ) ) )
              (CONST
                (when (and #| (consp rechts) |# (consp (car rechts)))
                  (case (first (car rechts))
                    (SYMBOL-FUNCTION
                      (let ((n (second item)))
                        (cond ((and #| (consp (cdr rechts)) |#
                                    (equal (cadr rechts) '(PUSH))
                               )
                               (ersetze 3 `(CONST&SYMBOL-FUNCTION&PUSH ,n))
                              )
                              ((and #| (consp (cdr rechts)) |#
                                    (consp (cadr rechts))
                                    (eq (first (cadr rechts)) 'STORE)
                               )
                               (ersetze 3
                                 `(CONST&SYMBOL-FUNCTION&STORE ,n ,(second (cadr rechts)))
                              ))
                              (t (ersetze 2 `(CONST&SYMBOL-FUNCTION ,n)))
                    ) ) )
                    (EQ
                      (when (and #| (consp (cdr rechts)) |#
                                 (consp (cadr rechts))
                                 (memq (first (cadr rechts)) '(JMPIF JMPIFNOT))
                                 (null (third (cadr rechts)))
                            )
                        (ersetze 3
                          `(,(if (eq (first (cadr rechts)) 'JMPIF)
                               'JMPIFEQTO
                               'JMPIFNOTEQTO
                             )
                            ,(second item)
                            ,(second (cadr rechts))
                           )
              ) ) ) ) ) )
              (LOAD
                (when (and #| (consp rechts) |# (consp (car rechts)))
                  (let ((n (second item)))
                    (case (first (car rechts))
                      (CAR
                        (when (and #| (consp (cdr rechts)) |# (consp (cadr rechts)))
                          (case (first (cadr rechts))
                            (PUSH (ersetze 3 `(LOAD&CAR&PUSH ,n)))
                            (STORE
                              (ersetze 3
                                `(LOAD&CAR&STORE ,n ,(second (cadr rechts)))
                      ) ) ) ) )
                      (CDR
                        (when (and #| (consp (cdr rechts)) |# (consp (cadr rechts)))
                          (case (first (cadr rechts))
                            (PUSH (ersetze 3 `(LOAD&CDR&PUSH ,n)))
                            (STORE
                              (when (eql n (second (cadr rechts)))
                                (ersetze 3 `(LOAD&CDR&STORE ,n))
                      ) ) ) ) )
                      (CONS
                        (when (and #| (consp (cdr rechts)) |# (consp (cadr rechts))
                                   (eq (first (cadr rechts)) 'STORE)
                                   (eql (second (cadr rechts)) (- n 1))
                              )
                          (ersetze 3 `(LOAD&CONS&STORE ,(- n 1)))
                      ) )
                      (PUSH
                        (when (and #| (consp (cdr rechts)) |# (consp (cadr rechts))
                                   (or (equal (cadr rechts) CALLS-1+)
                                       (equal (cadr rechts) CALLS-1-)
                                   )
                                   #| (consp (cddr rechts)) |# (consp (caddr rechts))
                              )
                          (when (equal (caddr rechts) '(PUSH))
                            (ersetze 4
                              `(,(if (equal (cadr rechts) CALLS-1+)
                                   'LOAD&INC&PUSH
                                   'LOAD&DEC&PUSH
                                 )
                                ,n
                               )
                          ) )
                          (when (and (eq (first (caddr rechts)) 'STORE)
                                     (eql (second (caddr rechts)) n)
                                )
                            (ersetze 4
                              `(,(if (equal (cadr rechts) CALLS-1+)
                                   'LOAD&INC&STORE
                                   'LOAD&DEC&STORE
                                 )
                                ,n
                               )
                        ) ) )
                        (ersetze 2 `(LOAD&PUSH ,n))
              ) ) ) ) )
              (JMPIFBOUNDP ; vereinfache (JMPIFBOUNDP n l) (NIL) (STORE n) l
                (when (and #| (consp rechts) |#
                           (equal (car rechts) '(NIL))
                           #| (consp (cdr rechts)) |#
                           (consp (cadr rechts))
                           (eq (first (cadr rechts)) 'STORE)
                           (eql (second (cadr rechts)) (second item))
                           #| (consp (cddr rechts)) |#
                           (eq (caddr rechts) (third item))
                      )
                  (ersetze 3 `(UNBOUND->NIL ,(second item)))
              ) )
              (JSR
                (if (and #| (consp rechts) |# (equal (car rechts) '(PUSH)))
                  (ersetze 2 `(JSR&PUSH ,(third item)))
                  (ersetze 1 `(JSR ,(third item)))
              ) )
              (UNBIND1
                (let ((count 1))
                  (loop
                    (unless (and #| (consp rechts) |#
                                 (equal (car rechts) '(UNBIND1))
                            )
                      (return)
                    )
                    (incf count)
                    (setq rechts (cdr rechts))
                  )
                  (unless (eql count 1)
                    (setf (car mitte) `(UNBIND ,count))
                    (setf (cdr mitte) rechts)
                    (go weiter)
              ) ) )
              ;(RET (ersetze 1 '(SKIP&RET 0))) ; kommt nicht vor!
              ;(RETGF (ersetze 1 '(SKIP&RETGF 0))) ; kommt nicht vor!
              (UNWIND-PROTECT-CLOSE (ersetze 1 '(UNWIND-PROTECT-CLOSE)))
              ((JMPIF JMPIFNOT) (ersetze 1 `(,(first item) ,(second item))))
              ((JMPHASH JMPHASHV)
                (let ((hashtable (third item))
                      (labels (cddddr item)))
                  (maphash
                    #'(lambda (obj index) ; (gethash obj hashtable) = index
                        (setf (gethash obj hashtable) (nth index labels))
                      )
                    hashtable
                ) )
                (setf (cddddr item) '())
              )
              (HANDLER-OPEN
                (do ((v (third item))
                     (labels (cddddr item) (cdr labels))
                     (i 1 (+ i 2)))
                    ((null labels))
                  (setf (svref v i) (car labels))
                )
                (setf (cdddr item) '())
              )
              (APPLY
                (when (and #| (consp rechts) |#
                           (consp (car rechts))
                           (eq (first (car rechts)) 'SKIP)
                           #| (consp (cdr rechts)) |#
                           (equal (cadr rechts) '(RET))
                      )
                  (ersetze 3 `(APPLY&SKIP&RET ,(second item) ,(second (car rechts))))
              ) )
              (FUNCALL
                (when (and #| (consp rechts) |#
                           (consp (car rechts))
                           (eq (first (car rechts)) 'SKIP)
                           #| (consp (cdr rechts)) |#
                           (equal (cadr rechts) '(RETGF))
                      )
                  (ersetze 3 `(FUNCALL&SKIP&RETGF ,(second item) ,(second (car rechts))))
              ) )
      ) ) ) )
      weiter ; Hier ist man mit (car mitte) fertig.
      (when (equal (car rechts) '(BARRIER))
        ; streiche Element (car rechts)
        (setf (cdr mitte) (setq rechts (cdr rechts)))
      )
    )
    code-list
  )
)


#|
                                7. Schritt:
                Umwandlung der Instruktionen in eine Byte-Folge

Erster Teilschritt: jeder Instruktion wird eine Klassifikation der Instruktion
und die Länge der Instruktion (Label-Operanden nicht mitgezählt)
vorangestellt, jedem Label wird sein PC als Wert zugewiesen.
Dabei werden die Operandenlängen - soweit möglich - bestimmt, in Instruktionen
auftretende Labels werden durch (vermutliche Verweislänge . label) ersetzt.
So wird aus (BLOCK-OPEN 2 #:G7) --> (NL 2 . (67 2 (1 . #:G7))) .
Weitere Teilschritte:
Immer wieder wird die Codeliste durchlaufen, dabei werden Sprungverweise
eventuell von 1 auf 2 oder 6 Byte verlängert. Dadurch kann der Code insgesamt
nur länger werden.
Letzter Teilschritt:
Die Sprungverweise werden in Distanzen umgesetzt, und die Codeliste wird
als Liste von Bytes neu aufgebaut.
|#
; gibt an, wieviel Bytes ein numerischer Operand braucht:
(defun num-operand-length (n)
  (cond ((< n 128) 1) ; 7 Bit in 1 Byte
        ((< n 32768) 2) ; 15 Bit in 2 Bytes
        (t 6) ; sonst 6 Bytes
) )
; assembliert eine Code-Liste und liefert eine Bytecode-Liste:
(defun assemble-LAP (code-list)
  ; erster Teilschritt:
  (do ((code-listr code-list (cdr code-listr))
       (PC 0))
      ((null code-listr))
    (let ((item (car code-listr)))
      (if (atom item)
        (setf (symbol-value item) PC)
        (let ((instr-code (gethash (first item) instruction-codes)))
          (unless instr-code (compiler-error 'assemble-LAP "ILLEGAL INSTRUCTION"))
          (let ((instr-class (second (svref instruction-table instr-code)))
                (instr-length 1))
            (if (and (eq instr-class 'K)
                     (< (second item)
                        (svref short-code-opsize (position (first item) instruction-table-K))
                )    )
              (progn
                (setq instr-code
                  (+ (svref short-code-ops
                            (position (first item) instruction-table-K)
                     )
                     (second item)
                ) )
                (setq instr-class 'O)
                (setq item (list (first item)))
              )
              (case instr-class
                (O)
                ((K N NC) (incf instr-length (num-operand-length (second item))))
                (B (incf instr-length 1))
                (L (incf PC 1) (push 1 (second item)))
                (NN (incf instr-length (num-operand-length (second item)))
                    (incf instr-length (num-operand-length (third item))) )
                (NB (incf instr-length (num-operand-length (second item)))
                    (incf instr-length 1) )
                (BN (incf instr-length 1)
                    (incf instr-length (num-operand-length (third item))) )
                (NNN (incf instr-length (num-operand-length (second item)))
                     (incf instr-length (num-operand-length (third item)))
                     (incf instr-length (num-operand-length (fourth item))) )
                (NBN (incf instr-length (num-operand-length (second item)))
                     (incf instr-length 1)
                     (incf instr-length (num-operand-length (fourth item))) )
                (NNNN (incf instr-length (num-operand-length (second item)))
                      (incf instr-length (num-operand-length (third item)))
                      (incf instr-length (num-operand-length (fourth item)))
                      (incf instr-length (num-operand-length (fifth item))) )
                (NL (incf instr-length (num-operand-length (second item)))
                    (incf PC 1) (push 1 (third item)) )
                (BL (incf instr-length 1)
                    (incf PC 1) (push 1 (third item)) )
                (NNL (incf instr-length (num-operand-length (second item)))
                     (incf instr-length (num-operand-length (third item)))
                     (incf PC 1) (push 1 (fourth item)) )
                (NBL (incf instr-length (num-operand-length (second item)))
                     (incf instr-length 1)
                     (incf PC 1) (push 1 (fourth item)) )
                (NHL (incf instr-length (num-operand-length (second item)))
                     (incf PC 1) (push 1 (fourth item)) )
                (NLX (incf instr-length (num-operand-length (second item)))
                     (do ((L (cddr item) (cdr L)))
                         ((null L))
                       (incf PC 1) (push 1 (car L))
                )    )
            ) )
            (incf PC instr-length)
            (setf (car code-listr)
              (list* instr-class instr-length instr-code (cdr item))
            )
  ) ) ) ) )
  ; weitere Teilschritte:
  (loop
    (unless
      (let ((modified nil) (PC 0))
        (dolist (item code-list)
          (if (atom item)
            (setf (symbol-value item) PC)
            (progn
              (incf PC (cadr item))
              (when (memq (car item) '(L NL BL NNL NBL NHL NLX))
                (let ((itemargs (cdddr item)))
                  (dolist (x (case (car item)
                               (L itemargs)
                               ((NL BL NLX) (cdr itemargs))
                               ((NNL NBL NHL) (cddr itemargs))
                          )  )
                    (incf PC (car x))
                    (let ((new-dist (- (symbol-value (cdr x)) PC)))
                      ; bisher angenommene Sprunglänge und neu errechnete abgleichen:
                      (if (<= -64 new-dist 63) ; 7 Bits in 1 Byte
                        () ; Sprunglänge bleibt 1
                        (if (<= -16384 new-dist 16383) ; 15 Bits in 2 Bytes
                          (case (car x)
                            (1 (setf (car x) 2) ; neue Sprunglänge=2
                               (incf PC 1) ; gibt 2-1=1 Bytes Verlängerung
                               (setq modified t)
                          ) )
                          ; 32 Bits in 6 Bytes
                          (case (car x)
                            (1 (setf (car x) 6) ; neue Sprunglänge=6
                               (incf PC 5) ; gibt 6-1=5 Bytes Verlängerung
                               (setq modified t)
                            )
                            (2 (setf (car x) 6) ; neue Sprunglänge=6
                               (incf PC 4) ; gibt 6-2=4 Bytes Verlängerung
                               (setq modified t)
                      ) ) ) )
              ) ) ) )
        ) ) )
        modified
      )
      (return) ; nichts mehr verändert -> alle Sprunglängen optimal
  ) )
  ; letzter Teilschritt:
  (let ((byte-list '()) (PC 0))
    (flet ((new-byte (n) (push n byte-list)))
      (flet ((num-operand (n)
               (cond ((< n 128) (new-byte n))
                     ((< n 32768) (new-byte (+ 128 (ldb (byte 7 8) n)))
                                  (new-byte (ldb (byte 8 0) n))
                     )
                     (t (compiler-error 'assemble-LAP "15 BIT"))
             ) )
             (label-operand (x)
               (incf PC (car x))
               (let ((dist (- (symbol-value (cdr x)) PC)))
                 (case (car x)
                   (1 (new-byte (ldb (byte 7 0) dist)))
                   (2 (new-byte (+ 128 (ldb (byte 7 8) dist)))
                      (new-byte (ldb (byte 8 0) dist))
                   )
                   (6 (new-byte 128) (new-byte 0)
                      (new-byte (ldb (byte 8 24) dist))
                      (new-byte (ldb (byte 8 16) dist))
                      (new-byte (ldb (byte 8 8) dist))
                      (new-byte (ldb (byte 8 0) dist))
                 ) )
            )) )
        (dolist (item code-list)
          (when (consp item)
            (incf PC (cadr item))
            (new-byte (caddr item))
            (case (car item)
              (O) ; darin fallen auch die 1-Byte-Befehle vom Typ K
              ((K N) (num-operand (second (cddr item))))
              (B (new-byte (second (cddr item))))
              (L (label-operand (second (cddr item))))
              (NN (num-operand (second (cddr item)))
                  (num-operand (third (cddr item))) )
              (NB (num-operand (second (cddr item)))
                  (new-byte (third (cddr item))) )
              (BN (new-byte (second (cddr item)))
                  (num-operand (third (cddr item))) )
              (NNN (num-operand (second (cddr item)))
                   (num-operand (third (cddr item)))
                   (num-operand (fourth (cddr item))) )
              (NBN (num-operand (second (cddr item)))
                   (new-byte (third (cddr item)))
                   (num-operand (fourth (cddr item))) )
              (NNNN (num-operand (second (cddr item)))
                    (num-operand (third (cddr item)))
                    (num-operand (fourth (cddr item)))
                    (num-operand (fifth (cddr item))) )
              (NL (num-operand (second (cddr item)))
                  (label-operand (third (cddr item))) )
              (BL (new-byte (second (cddr item)))
                  (label-operand (third (cddr item))) )
              (NNL (num-operand (second (cddr item)))
                   (num-operand (third (cddr item)))
                   (label-operand (fourth (cddr item))) )
              (NBL (num-operand (second (cddr item)))
                   (new-byte (third (cddr item)))
                   (label-operand (fourth (cddr item))) )
              (NHL (num-operand (second (cddr item)))
                   (let ((ht (third (cddr item))))
                     (maphash
                       #'(lambda (obj x) ; x = (gethash obj ht)
                           (setf (gethash obj ht) (- (symbol-value x) PC))
                         )
                       ht
                   ) )
                   (label-operand (fourth (cddr item)))
              )
              (NC (num-operand (second (cddr item)))
                  (let* ((v (third (cddr item)))
                         (m (length v)))
                    (do ((i 1 (+ i 2)))
                        ((>= i m))
                      (setf (svref v i) (symbol-value (svref v i)))
              )   ) )
              (NLX (num-operand (second (cddr item)))
                   (dolist (x (cddr (cddr item))) (label-operand x)) )
            )
        ) )
    ) )
    (nreverse byte-list)
) )

; die Umkehrung zu assemble-LAP : liefert zu einer Bytecode-Liste die dazu
; gehörige Codeliste. In dieser steht allerdings vor jedem Item noch der PC.
(defun disassemble-LAP (byte-list const-list)
  (let ((code-list '()) (PC 0) instr-PC (label-alist '()))
    ; label-alist ist eine Liste von Conses (PC . label), in der die PCs streng
    ; fallend geordnet sind.
    (flet ((PC->label-a (PC)
             (cons PC (make-symbol
                        (concatenate 'string "L" (prin1-to-string PC))
           ) )        )
           (next-byte () (incf PC) (pop byte-list))
          )
      (flet ((num-operand ()
               (let ((a (next-byte)))
                 (cond ((< a 128) a)
                       (t (+ (* 256 (- a 128)) (next-byte)))
             ) ) )
             (label-operand
                  (&optional
                    (dist
                      (let ((a (next-byte)))
                        (cond ((< a 128) (if (< a 64) a (- a 128)))
                              (t (setq a (- a 128))
                                 (unless (< a 64) (setq a (- a 128)))
                                 (setq a (+ (* 256 a) (next-byte)))
                                 (if (zerop a)
                                   (+ (* 256 (+ (* 256 (+ (* 256 (next-byte))
                                                          (next-byte)
                                                )      )
                                                (next-byte)
                                      )      )
                                      (next-byte)
                                   )
                                   a
                    ) ) )     )  )
                    (label-PC (+ PC dist))
                  )
               ; Suche label-PC in label-alist:
               (do* ((L1 nil L2)
                     (L2 label-alist (cdr L2))) ; L1 = nil oder L2 = (cdr L1)
                    ((cond
                       ((or (null L2) (> label-PC (caar L2))) ; einfügen
                        (setq L2 (cons (PC->label-a label-PC) L2))
                        (if L1 (setf (cdr L1) L2) (setq label-alist L2))
                        t)
                       ((= label-PC (caar L2)) t)
                       (t nil)
                     )
                     (cdar L2)
            )) )    )
        (loop
          (when (null byte-list) (return))
          (setq instr-PC PC) ; PC beim Start der Instruktion
          (let ((instruction
                  (let ((instr-code (next-byte)))
                    (if (>= instr-code short-code-base)
                      (let* ((q (position instr-code short-code-ops :test #'>= :from-end t))
                             (r (- instr-code (svref short-code-ops q))))
                        (list (svref instruction-table-K q) r)
                      )
                      (let* ((table-entry (svref instruction-table instr-code))
                             (instr-name (first table-entry)))
                        (case (second table-entry)
                          (O (list instr-name))
                          ((K N) (list instr-name (num-operand)))
                          (B (list instr-name (next-byte)))
                          (L (list instr-name (label-operand)))
                          (NN (list instr-name (num-operand) (num-operand)))
                          (NB (list instr-name (num-operand) (next-byte)))
                          (BN (list instr-name (next-byte) (num-operand)))
                          (NNN (list instr-name (num-operand) (num-operand) (num-operand)))
                          (NBN (list instr-name (num-operand) (next-byte) (num-operand)))
                          (NNNN (list instr-name (num-operand) (num-operand) (num-operand) (num-operand)))
                          (NL (list instr-name (num-operand) (label-operand)))
                          (BL (list instr-name (next-byte) (label-operand)))
                          (NNL (list instr-name (num-operand) (num-operand) (label-operand)))
                          (NBL (list instr-name (num-operand) (next-byte) (label-operand)))
                          (NHL (let* ((n (num-operand))
                                      (ht (if (eq instr-name 'JMPHASH)
                                            (nth n const-list)           ; JMPHASH
                                            (svref (first const-list) n) ; JMPHASHV
                                      )   )
                                      (labels '()))
                                 (maphash
                                   #'(lambda (obj dist)
                                       (declare (ignore obj))
                                       (push (label-operand dist) labels)
                                     )
                                   ht
                                 )
                                 (list* instr-name n (label-operand) labels)
                          )    )
                          (NC (let* ((n (num-operand))
                                     (v (car (nth n const-list)))
                                     (m (length v))
                                     (labels '()))
                                (do ((i 1 (+ i 2)))
                                    ((>= i m))
                                  (push (label-operand nil (svref v i)) labels)
                                )
                                (list* instr-name n (nreverse labels))
                          )   )
                          (NLX (let* ((n (num-operand))
                                      (m (length (nth n const-list)))
                                      (L '()))
                                 (dotimes (i m) (push (label-operand) L))
                                 (list* instr-name n (nreverse L))
                          )    )
               )) ) ) ) )
            (push (cons instr-PC instruction) code-list)
        ) )
    ) )
    ; (setq label-alist (sort label-alist #'> :key #'car))
    ; code-list umdrehen und dabei die Labels einfügen:
    (let ((new-code-list '()))
      (loop
        (when (and new-code-list label-alist
                   (= (caar new-code-list) (caar label-alist))
              )
          (push (car label-alist) new-code-list)
          (setq label-alist (cdr label-alist))
        )
        (when (null code-list) (return))
        ; eine Instruktion von code-list in new-code-list übernehmen:
        (psetq code-list (cdr code-list)
               new-code-list (rplacd code-list new-code-list)
      ) )
      new-code-list
) ) )


#|
                           8. Schritt:
                    funktionales Objekt bilden

Die Funktion make-closure wird dazu vorausgesetzt.
|#
; trägt eine Byteliste als Code in fnode ein.
(defun create-fun-obj (fnode byte-list SPdepth)
  (setf (fnode-code fnode)
    (make-closure
      :name (fnode-name fnode)
      :codevec
        (macrolet ((as-word (anz)
                     (if *big-endian*
                       ; BIG-ENDIAN-Prozessor
                       `(floor ,anz 256)
                       ; LITTLE-ENDIAN-Prozessor
                       `(multiple-value-bind (q r) (floor ,anz 256) (values r q))
                  )) )
          (multiple-value-call #'list*
            (as-word (car SPdepth))
            (as-word (cdr SPdepth))
            (as-word (fnode-req-anz fnode))
            (as-word (fnode-opt-anz fnode))
            (+ (if (fnode-rest-flag fnode) 1 0)
               (if (fnode-gf-p fnode) 16 0)
               (if (fnode-keyword-flag fnode)
                 (+ 128 (if (fnode-allow-other-keys-flag fnode) 64 0))
                 0
            )  )
            (values ; Argumenttyp-Kürzel
              (let ((req-anz (fnode-req-anz fnode))
                    (opt-anz (fnode-opt-anz fnode))
                    (rest (fnode-rest-flag fnode))
                    (key (fnode-keyword-flag fnode)))
                (cond ((and (not rest) (not key) (< (+ req-anz opt-anz) 6))
                       (+ (svref '#(1 7 12 16 19 21) opt-anz) req-anz)
                      )
                      ((and rest (not key) (zerop opt-anz) (< req-anz 5))
                       (+ 22 req-anz)
                      )
                      ((and (not rest) key (< (+ req-anz opt-anz) 5))
                       (+ (svref '#(27 32 36 39 41) opt-anz) req-anz)
                      )
                      (t 0)
            ) ) )
            (if (fnode-keyword-flag fnode)
              (multiple-value-call #'values
                (as-word (length (fnode-keywords fnode)))
                (as-word (fnode-Keyword-Offset fnode))
              )
              (values)
            )
            byte-list
        ) )
      :consts
        (let ((l (append
                   (make-list (fnode-Keyword-Offset fnode))
                   (fnode-keywords fnode)
                   (if *compiling-from-file*
                     (mapcar #'(lambda (value form)
                                 (if form (make-load-time-eval form) value)
                               )
                             (fnode-Consts fnode) (fnode-Consts-forms fnode)
                     )
                     (fnode-Consts fnode)
             ))  ) )
          (if (fnode-gf-p fnode)
            (list (coerce l 'simple-vector))
            l
        ) )
  ) )
  fnode
)

; Liefert die Signatur eines funktionalen Objekts,
; als Werte:
; 1. req-anz
; 2. opt-anz
; 3. rest-p
; 4. key-p
; 5. keyword-list
; 6. allow-other-keys-p
; und zusätzlich
; 7. byte-list
; 8. const-list
(defun signature (closure)
  (let ((const-list (closure-consts closure))
        (byte-list (closure-codevec closure)))
    (macrolet ((pop2 (listvar)
                 (if *big-endian*
                   ; BIG-ENDIAN-Prozessor
                   `(+ (* 256 (pop ,listvar)) (pop ,listvar))
                   ; LITTLE-ENDIAN-Prozessor
                   `(+ (pop ,listvar) (* 256 (pop ,listvar)))
              )) )
      (pop byte-list) (pop byte-list)
      (pop byte-list) (pop byte-list)
      (let* ((req-anz (pop2 byte-list))
             (opt-anz (pop2 byte-list))
             (h (pop byte-list))
             (key-p (logbitp 7 h)))
        (pop byte-list)
        (values
          req-anz
          opt-anz
          (logbitp 0 h)
          key-p
          (when key-p
            (let ((kw-count (pop2 byte-list))
                  (kw-offset (pop2 byte-list)))
              (subseq (if (logbitp 4 h) ; generische Funktion?
                        (coerce (first const-list) 'list)
                        const-list
                      )
                      kw-offset (+ kw-offset kw-count)
          ) ) )
          (logbitp 6 h)
          byte-list
          const-list
) ) ) ) )


;                  D R I T T E R   P A S S

(defun pass3 ()
  (dolist (pair *fnode-fixup-table*)
    (let ((code (fnode-code (first pair))) (n (second pair)))
      (macrolet ((closure-const (code n)
                   #-CLISP `(nth ,n (closure-consts ,code))
                   #+CLISP `(sys::%record-ref ,code (+ 2 ,n))
                ))
        (setf (closure-const code n) (fnode-code (closure-const code n)))
) ) ) )


;             T O P - L E V E L - A U F R U F

; compiliert einen Lambdabody und liefert seinen Code.
(defun compile-lambdabody (name lambdabody)
  (let ((fnode (c-lambdabody name lambdabody)))
    (unless *no-code*
      (let ((*fnode-fixup-table* '()))
        (pass2 fnode)
        (pass3)
      )
      (fnode-code fnode)
) ) )

; wird bei (lambda (...) (declare (compile)) ...) aufgerufen und liefert ein
; zu diesem Lambda-Ausdruck äquivalentes funktionales Objekt.
(defun compile-lambda (name lambdabody %venv% %fenv% %benv% %genv% %denv%)
  (let ((*compiling* t)
        (*compiling-from-file* nil)
        (*c-listing-output* nil)
        (*c-error-output* *error-output*)
        (*known-special-vars* '())
        (*constant-special-vars* '())
        (*func* nil)
        (*fenv* %fenv%)
        (*benv* %benv%)
        (*genv* %genv%)
        (*venv* %venv%)
        (*venvc* nil)
        (*denv* %denv%)
        (*error-count* 0) (*warning-count* 0) (*style-warning-count* 0)
        (*no-code* nil)
       )
    (let ((funobj (compile-lambdabody name lambdabody)))
      (unless (zerop *error-count*)
        (return-from compile-lambda (compile-lambdabody name '(() NIL)))
      )
      funobj
) ) )

; wird bei (let/let*/multiple-value-bind ... (declare (compile)) ...) aufgerufen
; und liefert ein funktionales Objekt, das - mit 0 Argumenten aufgerufen - diese
; Form ausführt.
(let ((form-count 0))
  (defun compile-form (form %venv% %fenv% %benv% %genv% %denv%)
    (compile-lambda (symbol-suffix '#:COMPILED-FORM (incf form-count))
                    `(() ,form)
                    %venv% %fenv% %benv% %genv% %denv%
  ) )
)

#+CLISP
(progn
  ; Evaluiert eine Form in einem Environment
  (defun eval-env (form &optional (env *toplevel-environment*))
    (evalhook form nil nil env)
  )
  ; Compiliert eine Form im Toplevel-Environment
  (defun compile-form-in-toplevel-environment (form &aux (env *toplevel-environment*))
    (compile-form form
                  (svref env 0) ; %venv%
                  (svref env 1) ; %fenv%
                  (svref env 2) ; %benv%
                  (svref env 3) ; %genv%
                  (svref env 4) ; %denv%
  ) )
)

; Common-Lisp-Funktion COMPILE
#-CROSS
(defun compile (name &optional (definition nil svar)
                     &aux (macro-flag nil) (trace-flag nil) (save-flag nil))
  (unless (function-name-p name)
    (error-of-type 'error
      (ENGLISH "Name of function to be compiled must be a symbol, not ~S")
      name
  ) )
  (let ((symbol (get-funname-symbol name)))
    (if svar
      ; Neudefinition von name als Funktion.
      (progn
        ; Ist name getraced -> falls vorher Macro, erst untracen.
        (when (and name (setq svar (get symbol 'sys::traced-definition)))
          (if (consp svar)
            (progn
              (warn (ENGLISH "~S: redefining ~S; it was traced!")
                    'compile name
              )
              (sys::untrace2 name)
            )
            (setq trace-flag t)
        ) )
        (when (compiled-function-p definition)
          (warn (ENGLISH "~S is already compiled.")
                definition
          )
          (when name
            (if trace-flag
              (setf (get symbol 'sys::traced-definition) definition)
              (setf (symbol-function symbol) definition)
          ) )
          (return-from compile name)
        )
        (when name
          (setq save-flag
                (cons `(SETF (FDEFINITION ',name) ',definition)
                      sys::*toplevel-environment*
        ) )     )
      )
      ; Compilierung der vorhandenen Funktions-/Macro-Definition.
      (progn
        (unless (fboundp symbol)
          (error-of-type 'undefined-function
            :name name
            (ENGLISH "Undefined function ~S")
            name
        ) )
        (if (setq definition (get symbol 'sys::traced-definition))
          (setq trace-flag t)
          (setq definition (symbol-function symbol))
        )
        (when (and (consp definition) (eq (car definition) 'system::macro))
          (setq macro-flag t)
          (setq definition (cdr definition))
        )
        (when (compiled-function-p definition)
          (warn (ENGLISH "~S is already compiled.")
                name
          )
          (return-from compile name)
    ) ) )
    (unless (or (and (consp definition) (eq (car definition) 'lambda))
                (sys::closurep definition)
            )
      (error-of-type 'error
        (ENGLISH "Not a lambda expression nor a function: ~S")
        definition
    ) )
    (let ((*compiling* t)
          (*error-count* 0)
          (*warning-count* 0)
          (*style-warning-count* 0)
          (*compiling-from-file* nil)
          (*c-listing-output* nil)
          (*c-error-output* *error-output*)
          (*known-special-vars* '())
          (*constant-special-vars* '())
          (*func* nil)
          (*fenv* (if (sys::closurep definition)
                    (sys::%record-ref definition 5)
                    nil
          )       )
          (*benv* (if (sys::closurep definition)
                    (sys::%record-ref definition 6)
                    nil
          )       )
          (*genv* (if (sys::closurep definition)
                    (sys::%record-ref definition 7)
                    nil
          )       )
          (*venv* (if (sys::closurep definition)
                    (sys::%record-ref definition 4)
                    nil
          )       )
          (*venvc* nil)
          (*denv* (if (sys::closurep definition)
                    (sys::%record-ref definition 8)
                    *toplevel-denv*
          )       )
          (*no-code* nil))
      (let ((lambdabody (if (sys::closurep definition)
                          (sys::%record-ref definition 1)
                          (cdr definition)
           ))           )
        (let ((funobj (compile-lambdabody name lambdabody)))
          (values
            (if (zerop *error-count*)
              (if name
                (progn
                  (when macro-flag (setq funobj (cons 'system::macro funobj)))
                  (if trace-flag
                    (setf (get symbol 'sys::traced-definition) funobj)
                    (setf (symbol-function symbol) funobj)
                  )
                  (when save-flag
                    (setf (get symbol 'sys::definition) save-flag)
                  )
                  name
                )
                funobj
              )
              nil
            )
            (let ((count (+ *error-count* *warning-count*)))
              (if (zerop count) nil count)
            )
            (let ((count (+ *error-count* (- *warning-count* *style-warning-count*))))
              (if (zerop count) nil count)
            )
) ) ) ) ) )

; Top-Level-Formen müssen einzeln aufs .fas-File rausgeschrieben werden,
; wegen der Semantik von EVAL-WHEN und LOAD-TIME-VALUE.
; Da Top-Level-Formen bei EVAL-WHEN, PROGN und LOCALLY auseinandergebrochen
; werden können, muss man LET () verwenden, wenn man dies umgehen will.

; Compiliert eine Top-Level-Form für COMPILE-FILE. Der *toplevel-name* wird
; meist unverändert durchgereicht. *toplevel-for-value* gibt an, ob der Wert
; gebraucht wird (für LOAD :PRINT T) oder nicht.
(defvar *toplevel-for-value*)
(defun compile-toplevel-form (form &optional (*toplevel-name* *toplevel-name*))
  (declare (special *toplevel-name*))
  (catch 'c-error
    ; CLtL2 S. 90: "Processing of top-level forms in the file compiler ..."
    ; 1. Schritt: Macroexpandieren
    (if (atom form)
      (when (symbolp form)
        (multiple-value-bind (macrop expansion) (venv-search-macro form *venv*)
          (when macrop ; Symbol-Macro ?
            (return-from compile-toplevel-form
              (compile-toplevel-form expansion) ; -> expandieren
      ) ) ) )
      (let ((fun (first form)))
        (when (symbolp fun)
          (multiple-value-bind (a b c) (fenv-search fun)
            (declare (ignore b c))
            (if (null a)
              ; nicht lokal definiert
              (case fun
                (PROGN ; vgl. c-PROGN
                  (test-list form 1)
                  (let ((L (cdr form))) ; Liste der Formen
                    (cond ((null L) (compile-toplevel-form 'NIL)) ; keine Form
                          ((null (cdr L)) (compile-toplevel-form (car L))) ; genau eine Form
                          (t (let ((subform-count 0))
                               (do ((Lr L))
                                   ((null Lr))
                                 (let* ((subform (pop Lr))
                                        (*toplevel-for-value* (and *toplevel-for-value* (null Lr))))
                                   (compile-toplevel-form subform
                                     (symbol-suffix *toplevel-name* (incf subform-count))
                  ) )     )  ) ) ) )
                  (return-from compile-toplevel-form)
                )
                ((LOCALLY EVAL-WHEN COMPILER-LET MACROLET SYMBOL-MACROLET)
                  (let ((*form* form))
                    ; c-LOCALLY bzw. c-EVAL-WHEN bzw. c-COMPILER-LET bzw.
                    ; c-MACROLET bzw. c-SYMBOL-MACROLET aufrufen:
                    (funcall (gethash fun c-form-table) #'compile-toplevel-form)
                  )
                  (return-from compile-toplevel-form)
                )
                (t (when (macro-function fun) ; globaler Macro ?
                     (return-from compile-toplevel-form
                       (compile-toplevel-form (macroexpand-1 form (vector *venv* *fenv*))) ; -> expandieren
              ) )  ) )
              ; lokal definiert
              (when (eq a 'SYSTEM::MACRO) ; lokaler Macro
                (return-from compile-toplevel-form
                  (compile-toplevel-form (macroexpand-1 form (vector *venv* *fenv*))) ; -> expandieren
              ) )
    ) ) ) ) )
    ; 2. Schritt: compilieren und rausschreiben
    (when (and (not *toplevel-for-value*) (l-constantp form))
      (return-from compile-toplevel-form)
    )
    (let ((*package-tasks* '()))
      (setq form
        (compile-lambdabody *toplevel-name*
          `(() ,form ,@(if *toplevel-for-value* '() '((VALUES)) ) )
      ) )
      (when *c-listing-output*
        (disassemble-closures form *c-listing-output*)
      )
      (when *fasoutput-stream*
        (write form :stream *fasoutput-stream* :pretty t
                    :readably t :right-margin 79
                    ; :closure t :circle t :array t :gensym t
                    ; :escape t :level nil :length nil :radix t
        )
        (terpri *fasoutput-stream*)
      )
      (when *package-tasks*
        (c-eval-when-compile `(PROGN ,@(nreverse *package-tasks*)))
      )
) ) )

; C-Output-File öffnen, falls noch nicht offen:
(defun prepare-coutput-file ()
  (if (and *compiling-from-file* *coutput-file*)
    (progn
      (unless *coutput-stream*
        (setq *coutput-stream* (open *coutput-file* :direction :output))
        (format *coutput-stream* "#include \"clisp.h\"~%~%")
      )
      t
    )
    nil
) )
; Hook fürs FFI:
(defun finalize-coutput-file ())

; Common-Lisp-Funktion COMPILE-FILE
; file          sollte ein Pathname/String/Symbol sein.
; :output-file  sollte nil oder t oder ein Pathname/String/Symbol oder
;               ein Output-Stream sein. Default: t.
; :listing      sollte nil oder t oder ein Pathname/String/Symbol oder
;               ein Output-Stream sein. Default: nil.
; :warnings     gibt an, ob die Warnings auch auf dem Bildschirm erscheinen
;               sollen.
; :verbose      gibt an, ob die Errors auch auf dem Bildschirm erscheinen
;               sollen.
(defun compile-file (file &key (output-file 'T) listing
                               ((:warnings *compile-warnings*) *compile-warnings*)
                               ((:verbose *compile-verbose*) *compile-verbose*)
                               ((:print *compile-print*) *compile-print*)
                          &aux (top-call nil) liboutput-file (*coutput-file* nil)
                               (new-output-stream nil) (new-listing-stream nil)
                    )
  (setq file (or (first (search-file file *source-file-types*))
                 (merge-pathnames file (merge-pathnames '#".lsp"))
  )          )
  (when (and output-file (not (streamp output-file)))
    (setq output-file
      (if (eq output-file 'T)
        (merge-pathnames '#".fas" file)
        (merge-pathnames output-file (merge-pathnames '#".fas" file))
    ) )
    (setq liboutput-file (merge-pathnames '#".lib" output-file))
    (setq *coutput-file* (merge-pathnames '#".c" output-file))
    (setq new-output-stream t)
  )
  (when (and listing (not (streamp listing)))
    (setq listing (if (eq listing 'T)
                    (merge-pathnames '#".lis" file)
                    (merge-pathnames listing)
    )             )
    (setq new-listing-stream t)
  )
  (with-open-file (istream file :direction :input-immutable)
    (let ((listing-stream (if new-listing-stream
                            (open listing :direction :output)
                            (if (streamp listing) listing nil)
         ))               ) ; ein Stream oder NIL
      (unwind-protect
        (let ((*compile-file-pathname* file)
              (*compile-file-truename* (truename file))
              (*compile-file-lineno1* nil)
              (*compile-file-lineno2* nil)
              (*fasoutput-stream* (if new-output-stream
                                    (open output-file :direction :output)
                                    (if (streamp output-file) output-file nil)
              )                   ) ; ein Stream oder NIL
              (*liboutput-stream* (if new-output-stream
                                    (open liboutput-file :direction :output)
                                    nil
              )                   ) ; ein Stream oder NIL
              (*coutput-stream* nil) ; ein Stream oder vorerst NIL
              (*ffi-module* nil) ; vorerst NIL
              (compilation-successful nil))
          (when *fasoutput-stream* (sys::allow-read-eval *fasoutput-stream* t))
          (when *liboutput-stream* (sys::allow-read-eval *liboutput-stream* t))
          (unwind-protect
            (progn
              (when listing-stream
                (format listing-stream
                  (ENGLISH "~&Listing of compilation of file ~A~%on ~@? by ~A, version ~A")
                  file
                  (date-format)
                  (multiple-value-list (get-decoded-time))
                    ; Liste (sec min hour day month year ...)
                  (lisp-implementation-type) (lisp-implementation-version)
              ) )
              (unless *compiling* ; Variablen setzen, nicht binden!
                (setq *functions-with-errors* '())
                (setq *known-special-vars* '()) (setq *unknown-free-vars* '())
                (setq *constant-special-vars* '())
                (setq *known-functions* '()) (setq *unknown-functions* '())
                (setq *deprecated-functions* '())
                (setq *inline-functions* '()) (setq *notinline-functions* '())
                (setq *inline-definitions* '())
                (setq *inline-constants* '()) (setq *notinline-constants* '())
                (setq *user-declaration-types* '())
                (setq *compiled-modules* '())
                (setq top-call t)
              )
              (let ((*compiling* t)
                    (*compiling-from-file* t)
                    (*package* *package*)
                    (*readtable* *readtable*)
                    (*c-listing-output* listing-stream)
                    (*c-error-output*
                      (if listing-stream
                        (make-broadcast-stream *error-output* listing-stream)
                        *error-output*
                    ) )
                    (*func* nil)
                    (*fenv* nil)
                    (*benv* nil)
                    (*genv* nil)
                    (*venv* nil)
                    (*venvc* nil)
                    (*denv* *toplevel-denv*)
                    (*error-count* 0) (*warning-count* 0) (*style-warning-count* 0)
                    (*no-code* (and (null *fasoutput-stream*) (null listing-stream)))
                    (*toplevel-for-value* t)
                    (eof-value "EOF")
                    (form-count 0)
                   )
                (c-comment (ENGLISH "~%Compiling file ~A ...")
                           file
                )
                (when *fasoutput-stream*
                  (let ((*package* *keyword-package*))
                    (write `(SYSTEM::VERSION ',(version)) :stream *fasoutput-stream*
                           :readably t :right-margin 79 :case ':upcase
                           ; :escape t :level nil :length nil :radix t
                  ) )
                  (terpri *fasoutput-stream*)
                )
                #+UNICODE
                (flet ((set-utf-8 (stream)
                         ; Set the stream's encoding to UTF-8, if it supports it.
                         (block try
                           (let ((*error-handler*
                                   #'(lambda (&rest error-args)
                                       (declare (ignore error-args))
                                       (return-from try nil)
                                 )   )
                                 (encoding 'charset:utf-8))
                             (setf (stream-external-format stream) encoding)
                             (write-string "#0Y " stream)
                             (let ((*package* (find-package "CHARSET")))
                               (write encoding :stream stream :readably t)
                             )
                             (terpri stream)
                      )) ) )
                  (when new-output-stream
                    (when *fasoutput-stream*
                      (set-utf-8 *fasoutput-stream*)
                    )
                    (when *liboutput-stream*
                      (set-utf-8 *liboutput-stream*)
                ) ) )
                (loop
                  (peek-char t istream nil eof-value)
                  (setq *compile-file-lineno1* (line-number istream))
                  (let ((form (read istream nil eof-value)))
                    (setq *compile-file-lineno2* (line-number istream))
                    (when (eql form eof-value) (return))
                    (when *compile-print*
                      (format t "~%; ~A"
                                (sys::write-to-short-string form
                                  (- (or *print-right-margin* sys::*prin-linelength*) 2)
                    ) )         )
                    (compile-toplevel-form form
                      (symbol-suffix '#:TOP-LEVEL-FORM (incf form-count))
                ) ) )
                (finalize-coutput-file)
                (c-comment (ENGLISH "~&~%Compilation of file ~A is finished.")
                           file
                )
                (c-comment (ENGLISH "~%~D error~:P, ~D warning~:P")
                           *error-count* *warning-count* (eql *warning-count* 1)
                )
                (when top-call
                  (when *functions-with-errors*
                    (c-comment (ENGLISH "~%There were errors in the following functions:~%~{~<~%~:; ~S~>~^~}")
                               (nreverse *functions-with-errors*)
                  ) )
                  (setq *unknown-functions*
                    (nset-difference *unknown-functions* *known-functions* :test #'equal)
                  )
                  (when *unknown-functions*
                    (c-comment (ENGLISH "~%The following functions were used but not defined:~%~{~<~%~:; ~S~>~^~}")
                               (nreverse *unknown-functions*)
                  ) )
                  (let ((unknown-vars (set-difference *unknown-free-vars* *known-special-vars*))
                        (too-late-vars (intersection *unknown-free-vars* *known-special-vars*)))
                    (when unknown-vars
                      (c-comment (ENGLISH "~%The following special variables were not defined:~%~{~<~%~:; ~S~>~^~}")
                                 (nreverse unknown-vars)
                    ) )
                    (when too-late-vars
                      (c-comment (ENGLISH "~%The following special variables were defined too late:~%~{~<~%~:; ~S~>~^~}")
                                 (nreverse too-late-vars)
                  ) ) )
                  (when *deprecated-functions*
                    (c-comment (ENGLISH "~%The following functions were used but are deprecated:~%~{~<~%~:; ~S~>~^~}")
                               (nreverse *deprecated-functions*)
                  ) )
                )
                (c-comment "~%")
                (setq compilation-successful (zerop *error-count*))
                (values (if compilation-successful output-file nil)
                        (let ((count (+ *error-count* *warning-count*)))
                          (if (zerop count) nil count)
                        )
                        (let ((count (+ *error-count* (- *warning-count* *style-warning-count*))))
                          (if (zerop count) nil count)
                        )
            ) ) )
            (when new-output-stream
              (close *fasoutput-stream*)
              (close *liboutput-stream*)
              (if *coutput-stream*
                (close *coutput-stream*)
                (when (probe-file *coutput-file*) (delete-file *coutput-file*))
              )
              (unless compilation-successful
                (delete-file output-file) (delete-file liboutput-file)
                (when (probe-file *coutput-file*) (delete-file *coutput-file*))
            ) )
        ) )
        (when new-listing-stream (close listing-stream))
) ) ) )

; Das muss mit compile-file (s.o.) konsistent sein!
(defun compile-file-pathname (file &key (output-file 'T) &allow-other-keys)
  (setq file (or (first (search-file file *source-file-types*))
                 (merge-pathnames file (merge-pathnames '#".lsp"))
  )          )
  (when (and output-file (not (streamp output-file)))
    (setq output-file
      (if (eq output-file 'T)
        (merge-pathnames '#".fas" file)
        (merge-pathnames output-file (merge-pathnames '#".fas" file))
    ) )
  )
  output-file
)

(defun disassemble-closures (closure stream)
  (let ((closures '()))
    (labels ((mark (cl) ; trägt eine Closure cl (rekursiv) in closures ein.
               (push cl closures) ; cl markieren
               (dolist (c (closure-consts cl)) ; und alle Teil-Closures
                 (when #+CLISP (and (sys::closurep c) (compiled-function-p c))
                       #-CLISP (closure-p c)
                   (unless (member c closures) (mark c)) ; ebenfalls markieren
            )) ) )
      (mark closure) ; Haupt-Closure markieren
    )
    (dolist (c (nreverse closures)) ; alle Closures disassemblieren
      (disassemble-closure c stream)
) ) )

#-CLISP
(defun disassemble-closure (closure &optional (stream *standard-output*))
  (format stream (ENGLISH "~%~%Disassembly of function ~S")
                 (closure-name closure)
  )
  (multiple-value-bind (req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p
                        byte-list const-list)
      (signature closure)
    (do ((L const-list (cdr L))
         (i 0 (1+ i)))
        ((null L))
      (format stream "~%(CONST ~S) = ~S" i (car L))
    )
    (format stream (ENGLISH "~%~S required arguments")
                   req-anz
    )
    (format stream (ENGLISH "~%~S optional arguments")
                   opt-anz
    )
    (format stream (ENGLISH "~%~:[No rest parameter~;Rest parameter~]")
                   rest-p
    )
    (if key-p
      (let ((kw-count (length keyword-list)))
        (format stream (ENGLISH "~%~S keyword parameter~:P: ~{~S~^, ~}.")
                       kw-count keyword-list
        )
        (when allow-other-keys-p
          (format stream (ENGLISH "~%Other keywords are allowed."))
      ) )
      (format stream (ENGLISH "~%No keyword parameters"))
    )
    (let ((const-string-list (mapcar #'write-to-string const-list)))
      (do ((L (disassemble-LAP byte-list const-list) (cdr L)))
          ((null L))
        (let ((PC (caar L))
              (instr (cdar L)))
          (format stream "~%~S~6T~A" PC instr)
          (multiple-value-bind ... ; siehe unten
            ...
    ) ) ) )
    (format stream "~%")
) )
#+CLISP
(defun disassemble-closure (closure &optional (stream *standard-output*))
  (terpri stream)
  (terpri stream)
  (write-string (ENGLISH "Disassembly of function ")
                stream
  )
  (prin1 (closure-name closure) stream)
  (multiple-value-bind (req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p
                        byte-list const-list)
      (signature closure)
    (do ((L const-list (cdr L))
         (i 0 (1+ i)))
        ((null L))
      (terpri stream)
      (write-string "(CONST " stream)
      (prin1 i stream)
      (write-string ") = " stream)
      (prin1 (car L) stream)
    )
    (terpri stream)
    (prin1 req-anz stream)
    (write-string (ENGLISH " required arguments")
                  stream
    )
    (terpri stream)
    (prin1 opt-anz stream)
    (write-string (ENGLISH " optional arguments")
                  stream
    )
    (terpri stream)
    (if rest-p
      (write-string (ENGLISH "Rest parameter")
                    stream
      )
      (write-string (ENGLISH "No rest parameter")
                    stream
    ) )
    (if key-p
      (let ((kw-count (length keyword-list)))
        (terpri stream)
        (prin1 kw-count stream)
        (format stream (ENGLISH " keyword parameter~P: ")
                       kw-count
        )
        (do ((L keyword-list))
            ((endp L))
          (prin1 (pop L) stream)
          (if (endp L) (write-string "." stream) (write-string ", " stream))
        )
        (when allow-other-keys-p
          (terpri stream)
          (write-string (ENGLISH "Other keywords are allowed.")
                        stream
      ) ) )
      (progn
        (terpri stream)
        (write-string (ENGLISH "No keyword parameters")
                      stream
    ) ) )
    (let ((const-string-list
            (mapcar #'(lambda (x) (sys::write-to-short-string x 35)) const-list)
         ))
      (do ((L (disassemble-LAP byte-list const-list) (cdr L)))
          ((null L))
        (let ((PC (caar L))
              (instr (cdar L)))
          (terpri stream)
          (prin1 PC stream)
          (dotimes (i (let ((pos (sys::line-position stream))) (if pos (max 1 (- 6 pos)) 2)))
            (write-char #\Space stream) ; Tab 6
          )
          (princ instr stream) ; instr ausgeben, Symbole ohne Package-Marker!
          (multiple-value-bind (commentp comment)
            (when (consp instr)
              (case (first instr)
                ((CALLS1 CALLS1&PUSH CALLS1&STORE CALLS1&JMPIFNOT CALLS1&JMPIF)
                  (values t (%funtabref (second instr)))
                )
                ((CALLS2 CALLS2&PUSH CALLS2&STORE CALLS2&JMPIFNOT CALLS2&JMPIF)
                  (values t (%funtabref (+ 256 (second instr))))
                )
                ((CALLSR CALLSR&PUSH CALLSR&STORE CALLSR&JMPIFNOT CALLSR&JMPIF)
                  (values t (%funtabref (+ funtabR-index (third instr))))
                )
                ((CALL CALL&PUSH)
                  (values 'string (nth (third instr) const-string-list))
                )
                ((CALL0 CALL1 CALL1&PUSH CALL1&JMPIFNOT CALL1&JMPIF
                  CALL2 CALL2&PUSH CALL2&JMPIFNOT CALL2&JMPIF
                  JMPIFEQTO JMPIFNOTEQTO CONST CONST&PUSH SETVALUE GETVALUE
                  GETVALUE&PUSH BIND CONST&STORE CONST&SYMBOL-FUNCTION&PUSH
                  CONST&SYMBOL-FUNCTION COPY-CLOSURE&PUSH COPY-CLOSURE
                  CONST&SYMBOL-FUNCTION&STORE TAGBODY-OPEN HANDLER-OPEN
                 )
                  (values 'string (nth (second instr) const-string-list))
            ) ) )
            (when commentp
              (dotimes (i (let ((pos (sys::line-position stream))) (if pos (max 1 (- 42 pos)) 2)))
                (write-char #\Space stream) ; Tab 42
              )
              (write-string "; " stream)
              (if (eq commentp 'string)
                (write-string comment stream)
                (prin1 comment stream)
    ) ) ) ) ) )
    (terpri stream)
) )

#-CROSS
(defun disassemble (object &aux name)
  (when (function-name-p object)
    (unless (fboundp object)
      (error-of-type 'undefined-function
        :name object
        (ENGLISH "Undefined function ~S")
        object
    ) )
    (setq name object)
    (setq object (get-funname-symbol object))
    (setq object (or (get object 'sys::traced-definition)
                     (symbol-function object)
  ) )            )
  (when (and (consp object) (eq (car object) 'system::macro))
    (setq object (cdr object))
  )
  #+UNIX (when (stringp object)
           (return-from disassemble
             (disassemble-machine-code (sys::program-name) (sys::program-id)
                          object
         ) ) )
  #+UNIX (when (sys::code-address-of object)
           (return-from disassemble
             (disassemble-machine-code (sys::program-name) (sys::program-id)
                          (format nil "0x~X" (sys::code-address-of object))
         ) ) )
  (unless (sys::closurep object)
    (error-of-type 'error
      (ENGLISH "Cannot disassemble ~S")
      object
  ) )
  ; object ist eine Closure.
  (unless (compiled-function-p object)
    (setq object
      (compile-lambda (sys::%record-ref object 0) ; name
                      (sys::%record-ref object 1) ; lambdabody
                      (sys::%record-ref object 4) ; venv
                      (sys::%record-ref object 5) ; fenv
                      (sys::%record-ref object 6) ; benv
                      (sys::%record-ref object 7) ; genv
                      (sys::%record-ref object 8) ; denv
  ) ) )
  ; object ist eine compilierte Closure.
  (disassemble-closure object) ; Disassemblieren
  object ; compilierte Closure als Wert
)
