;; LOOP-Facility nach CLTL2
;; (LOOP {loop-clause}*), CLTL2 S. 163,709-747
;; Bruno Haible 19.10.1991-20.10.1991, 22.10.1991, 6.6.1993, 28.6.1994, 16.6.1996
;; Sam Steingold 1999-03-11

(in-package "LISP")
(export '(loop loop-finish))
(pushnew ':loop *features*)

(in-package "SYSTEM")

;; Parser-Hilfsfunktionen:

(eval-when (compile load eval)

; (loop-keywordp obj) stellt fest, ob obj ein Loop-Keyword ist,
; und liefert dann das entsprechende Symbol (eindeutig), sonst NIL.
(defun loop-keywordp (obj)
  (and (symbolp obj)
       (gethash (symbol-name obj)
         (load-time-value
           (make-hash-table :test #'equal
             :initial-contents
               (mapcar #'(lambda (s) (cons (symbol-name s) s))
                 '(named
                   for as and from downfrom upfrom to downto upto below
                   above by in on = then across being each the hash-key
                   hash-keys hash-value hash-values of using symbol
                   present-symbol internal-symbol external-symbol symbols
                   present-symbols internal-symbols external-symbols
                   repeat
                   while until always never thereis
                   collect collecting append appending nconc nconcing
                   count counting sum summing maximize maximizing
                   minimize minimizing into
                   with
                   if when unless else end it
                   do doing return
                   of-type
                   initially finally
         ) )   )  )
) )    )

(defvar *whole*) ; die gesamte Form (LOOP ...)

; (loop-syntax-error loop-keyword) meldet einen Syntaxfehler
(defun loop-syntax-error (loop-keyword)
  (error (ENGLISH "~S: syntax error after ~A in ~S")
         'loop (symbol-name loop-keyword) *whole*
) )

;; Destructuring:

; (destructure-vars pattern) liefert die Liste der Variablen,
; die in pattern vorkommen.
(defun destructure-vars (pattern)
  (cond ((null pattern) nil)
        ((atom pattern) (list pattern))
        (t (nconc (destructure-vars (car pattern))
                  (destructure-vars (cdr pattern))
) )     )  )

; (empty-tree-p pattern) stellt fest, ob in pattern
; überhaupt keine Variablen vorkommen.
(defun empty-tree-p (pattern)
  (cond ((null pattern) t)
        ((atom pattern) nil)
        (t (and (empty-tree-p (car pattern)) (empty-tree-p (cdr pattern))))
) )

; (destructure-type pattern type) liefert eine Liste von Declaration-Specifiern,
; die die Variablen aus pattern zu den Typen aus type deklarieren.
(defun destructure-type (pattern type)
  (cond ((null pattern) nil)
        ((atom pattern) (list `(TYPE ,type ,pattern)))
        ((consp type)
         (nconc (destructure-type (car pattern) (car type))
                (destructure-type (cdr pattern) (cdr type))
        ))
        (t (let ((vars (destructure-vars pattern)))
             (if vars (list `(TYPE ,type ,@vars)) nil)
) )     )  )

; (simple-type-p type) stellt fest, ob der Typ type nach Destructuring nur
; aus NIL, T, FIXNUM, FLOAT besteht (und damit ein OF-TYPE überflüssig macht).
(defun simple-type-p (type)
  (if (atom type)
    (case type
      ((NIL T FIXNUM FLOAT) t)
      (t nil)
    )
    (and (simple-type-p (car type))
         (simple-type-p (cdr type))
) ) )

(defvar *helpvars*) ; Vektor mit Hilfsvariablen fürs Destructuring

; (helpvar n) liefert die (n+1)-te Hilfsvariable (n>=0). Es müssen schon
; mindestens n Hilfvariablen gebraucht worden sein.
; Evtl. wird eine neue Hilfvariable erzeugt.
(defun helpvar (n)
  (when (= n (fill-pointer *helpvars*))
    (vector-push-extend (gensym) *helpvars*)
  )
  (aref *helpvars* n)
)

; (destructure pattern form) liefert eine Liste von Listen
; (Variable Form). Das erste ist eine Variable aus pattern, das
; zweite eine Form, an die die Variable zu binden ist bzw. die
; der Variablen zuzuweisen ist. Auf die Reihenfolge der Bindungen
; bzw. Zuweisungen kommt es nicht an (d.h. es sind sowohl LET
; als auch LET* bzw. sowohl PSETQ als auch SETQ möglich).
(defun destructure (pattern form)
  (labels ((destructure-tree (pattern form helpvar-count)
             ; helpvar-count = Anzahl der belegten Hilfsvariablen
             (cond ((empty-tree-p pattern) nil)
                   ((atom pattern) (list (list pattern form)))
                   ((empty-tree-p (car pattern))
                    (destructure-tree (cdr pattern) `(CDR ,form) helpvar-count)
                   )
                   ((empty-tree-p (cdr pattern))
                    (destructure-tree (car pattern) `(CAR ,form) helpvar-count)
                   )
                   (t ; muss form zwischendurch einer Hilfsvariablen zuweisen
                     (let ((helpvar (helpvar helpvar-count)))
                       (nconc (destructure-tree (car pattern) `(CAR (SETQ ,helpvar ,form)) (1+ helpvar-count))
                              (destructure-tree (cdr pattern) `(CDR ,helpvar) helpvar-count)
          )) )     ) ) )
    (or (destructure-tree pattern form 0)
        ; keine Variablen -> muss trotzdem form auswerten!
        (list (list (helpvar 0) form))
) ) )

; Liefert zu einer Liste (var ...) von Variablen ohne Initialisierungsformen
; die Bindungsliste ((var var-init) ...), wobei var-init mit den declspecs
; verträglich ist.
(defun default-bindings (vars declspecs)
  ; Verwende NIL oder 0 oder 0.0 - falls das passt -
  ; oder verwende NIL und erweitere die Typdeklaration.
  (let ((bindings (mapcar #'(lambda (var) (list var 'NIL)) vars)))
    (dolist (declspec declspecs)
      (when (eq (first declspec) 'TYPE)
        ; declspec hat die Form (TYPE type . vars)
        (let* ((type (second declspec))
               (dtype (type-for-discrimination type))
               h)
          (cond ((typep 'NIL dtype) ) ; OK
                ((or (typep (setq h '0) dtype) (typep (setq h '0.0) dtype))
                 (dolist (var (cddr declspec))
                   (setf (second (find var bindings :key #'first)) h)
                ))
                (t (setf (second declspec) `(OR NULL ,type)))
    ) ) ) )
    bindings
) )

;; A loop-initialisation describes at macro expansion time the task
;; to initialise one or more variables. The initialisation may end up
;; generating code in the prologue or in the inner loop.
(defstruct (loop-initialisation
             (:copier nil)
             (:conc-name "LI-")
             (:predicate nil)
             (:constructor make-loop-init)
           )
  ;; How to generate the Lisp code.
  specform           ; special form: LET or MULTIPLE-VALUE-BIND or PROGN
  bindings           ; for LET: list of bindings, for MULTIPLE-VALUE-BIND: varlist and form
  declspecs          ; list of declspecs
  (endtest-forms nil) ; more forms to be inserted after the declarations, within the tagbody.
  ;; Properties of this initialisation.
  everytime          ; If the assignment has to be evaluated in the prologue only: NIL.
                     ; If the assignment has to be evaluated once for each iteration:
                     ; a cons, pointing at the right place in the stepafter-code.
  (requires-stepbefore nil) ; True if the variables can get their values only in the stepbefore-code or initially-code,
                     ; false if the first assignment can be merged with the initial binding.
  (depends-preceding nil) ; True if everytime=NIL and the values may depend on preceding variables,
                     ; so that these preceding variables must get their values no later than in
                     ; the initially-code.
  (later-depend nil) ; True if some later variables depend on these values, so that these values
                     ; must be computed no later than in the initially-code.
)
#+CLISP (remprop 'loop-initialisation 'sys::defstruct-description)

; (wrap-initialisations initialisations form) wickelt eine (umgedrehte!)
; Liste von Initialisierungen um form herum und liefert die neue Form.
(defun wrap-initialisations (initialisations form)
  (dolist (initialisation initialisations)
    (let ((name (li-specform initialisation))
          (bindings (li-bindings initialisation))
          (declarations (li-declspecs initialisation)))
      (setq form
        `(,name
          ,@(case name (MULTIPLE-VALUE-BIND bindings) (LET `(,bindings)))
          ,@(if declarations `((DECLARE ,@declarations)))
          ,@(li-endtest-forms initialisation)
          ,form
         )
  ) ) )
  form
)

(defvar *last-it*) ; Variable, die das letzte Test-Ergebnis ("it") enthält
(defvar *used-it*) ; Flag, ob diese Variable benutzt wird

; Das Gros des Expanders:
(defun expand-loop (*whole* body)
  (let ((body-rest body) ; alle Parse-Funktionen verkürzen body-rest
        (block-name 'NIL) ; Name des umgebenden BLOCKs
        (already-within-main nil) ; im zweiten Teil von {variables}* {main}* ?
        (*helpvars* (make-array 1 :fill-pointer 0 :adjustable t)) ; Vektor
                                   ; mit Hilfsvariablen fürs Destructuring
        (*last-it* nil) ; Variable, die das letzte Test-Ergebnis ("it") enthält
        (acculist-var nil) ; Akkumulationsvariable für collect, append etc.
        (accuvar-tailvar-alist nil) ; alist of (accu-var . tail-var)
        (accunum-var nil) ; Akkumulationsvariable für count, sum etc.
        (accu-vars-nil nil) ; Akkumulationsvariablen mit Initialwert NIL
        (accu-vars-0 nil) ; Akkumulationsvariablen mit Initialwert 0
        (accu-declarations nil) ; Typdeklarationen (umgedrehte Liste von declspecs)
        (initialisations nil) ; Bindungen: (init ...) (umgedrehte Liste)
        (seen-for-as-= nil) ; schon eine FOR-AS-= Klausel gesehen?
        (seen-endtest nil) ; schon eine FOR-AS Klausel mit Abbruchbedingung gesehen?
        (initially-code nil) ; initially-Code (umgedrehte Liste)
        (stepbefore-code nil) ; Code zum Abbruch vor dem Schleifendurchlauf (umgedrehte Liste)
        (main-code nil) ; Code im Hauptteil der Schleife (umgedrehte Liste)
        (stepafter-code nil) ; Code zur Vorbereitung des nächsten Schleifendurchlaufs (umgedrehte Liste)
        (accu-vars-nreverse nil) ; Akkumulationsvariablen, die am Schluss umzudrehen sind
        (finally-code nil) ; finally-Code (umgedrehte Liste)
        (results nil) ; Liste von Ergebnisformen (höchstens eine!)
       )
    (labels
      ((next-kw () ; Schaut, ob als nächstes ein Keyword kommt.
                   ; Wenn ja, wird es geliefert. Wenn nein, Ergebnis NIL.
         (and (consp body-rest) (loop-keywordp (first body-rest)))
       )
       (parse-kw-p (kw) ; Schaut, ob als nächstes das Keyword kw kommt.
                        ; Wenn ja, wird es übergangen. Wenn nein, Ergebnis NIL.
         (and (consp body-rest) (eq (loop-keywordp (first body-rest)) kw)
              (progn (pop body-rest) t)
       ) )
       (parse-form (kw) ; Nach kw: parst expr
         (unless (consp body-rest) (loop-syntax-error kw))
         (pop body-rest)
       )
       (parse-form-or-it (kw) ; Nach kw: parst expr, das auch 'it' sein kann
         (unless (consp body-rest) (loop-syntax-error kw))
         (let ((form (pop body-rest)))
           (if (eq (loop-keywordp form) 'it)
             (if *last-it*
               (progn (setq *used-it* t) *last-it*)
               (loop-syntax-error 'it)
             )
             form
       ) ) )
       (parse-var-typespec () ; parst var [typespec]
         ; Liefert das Variablen-Pattern und eine Liste von declspecs.
         (unless (consp body-rest)
           (error (ENGLISH "~S: missing variable.")
                  'loop
         ) )
         (let ((pattern (pop body-rest))
               (typedecl nil))
           (block nil
             (unless (consp body-rest) (return))
             (case (loop-keywordp (first body-rest))
               ((NIL) ; kein Loop-Keyword -> als Typespec interpretieren
                (setq typedecl (pop body-rest))
                (unless (simple-type-p typedecl)
                  (warn (ENGLISH "~S: After ~S, ~S is interpreted as a type specification")
                        'loop pattern typedecl
               )) )
               ((OF-TYPE) ; OF-TYPE -> danach kommt ein Typespec
                (pop body-rest)
                (setq typedecl (parse-form 'of-type))
               )
               (T (return)) ; sonstiges
             )
             (setq typedecl (destructure-type pattern typedecl))
           )
           (values pattern typedecl)
       ) )
       (parse-progn () ; parst: {expr}*
                       ; und liefert die Liste der Formen
         (let ((list nil))
           (loop
             (unless (and (consp body-rest)
                          (not (loop-keywordp (first body-rest)))
                     )
               (return)
             )
             (push (pop body-rest) list)
           )
           (nreverse list)
       ) )
       (parse-unconditional () ; parst ein Unconditional
         ; unconditional ::= {do | doing} {expr}*
         ; unconditional ::= return expr
         ; Liefert eine Lisp-Form oder NIL wenn's kein Unconditional war.
         (let ((kw (next-kw)))
           (case kw
             ((DO DOING)
              (pop body-rest)
              `(PROGN ,@(parse-progn))
             )
             ((RETURN)
              (pop body-rest)
              `(RETURN-FROM ,block-name ,(parse-form-or-it kw))
             )
             (t 'NIL)
       ) ) )
       (parse-clause () ; parst eine Clause
         ; clause ::= accumulation | conditional | unconditional
         ; accumulation ::= {collect | collecting | append | appending |
         ;                   nconc | nconcing} expr [into var]
         ; accumulation ::= {count | counting | sum | summing |
         ;                   maximize | maximizing | minimize |
         ;                   minimizing} expr [into var] [typespec]
         ; conditional ::= {if | when | unless} expr clause {and clause}*
         ;                 [else clause {and clause}*] [end]
         ; Liefert eine Lisp-Form oder NIL wenn's keine Clause war.
         (or (parse-unconditional)
             (let ((kw (next-kw)))
               (case kw
                 ((COLLECT COLLECTING APPEND APPENDING NCONC NCONCING)
                  (pop body-rest)
                  ; It seems permitted to write
                  ;   (loop ...  collect i into c  collect (copy-list c))
                  ; Therefore we must use forward-consing collection
                  ; (keeping the tail in a separate variable) if the accumulation
                  ; variable is named, and can use the more efficient backward-
                  ; consing (with nreverse at the end) only for unnamed accumulation.
                  (let ((form (parse-form-or-it kw))
                        (accuvar nil)
                        (accufuncsym
                          (case kw
                            ((COLLECT COLLECTING) 'CONS)
                            ((APPEND APPENDING) 'REVAPPEND)
                            ((NCONC NCONCING) 'NRECONC))))
                    (when (parse-kw-p 'into)
                      (unless (and (consp body-rest)
                                   (symbolp (setq accuvar (pop body-rest))))
                        (loop-syntax-error 'into)))
                    (if accuvar
                      ; Named accumulation variable -> forward-consing.
                      (let ((tailvar
                              (cdr (or (assoc accuvar accuvar-tailvar-alist)
                                       (car (setq accuvar-tailvar-alist
                                                  (acons accuvar (gensym (symbol-name accuvar))
                                                         accuvar-tailvar-alist))))))
                            (incrementvar (gensym)))
                        (push accuvar accu-vars-nil)
                        (push tailvar accu-vars-nil)
                        `(LET ((,incrementvar
                                ,(ecase accufuncsym
                                   (CONS `(LIST ,form))
                                   (REVAPPEND `(COPY-LIST ,form))
                                   (NRECONC `,form))))
                           (IF ,accuvar
                             ,(case accufuncsym
                                (CONS `(SETF ,tailvar (SETF (CDR ,tailvar) ,incrementvar)))
                                (t `(SETF ,tailvar (LAST (RPLACD ,tailvar ,incrementvar))))
                              )
                             ,(case accufuncsym
                                (CONS `(SETF ,tailvar (SETF ,accuvar ,incrementvar)))
                                (t `(SETF ,tailvar (LAST (SETF ,accuvar ,incrementvar))))
                              )
                         ) )
                      )
                      ; Unnamed accumulation variable -> backward-consing.
                      (progn
                        (setq accuvar
                              (or acculist-var (setq acculist-var (gensym)))
                        )
                        (push accuvar accu-vars-nil)
                        (push `(SYS::LIST-NREVERSE ,accuvar) results)
                        `(SETQ ,accuvar (,accufuncsym ,form ,accuvar))
                      )
                 )) )
                 ((COUNT COUNTING SUM SUMMING MAXIMIZE MAXIMIZING
                   MINIMIZE MINIMIZING)
                  (pop body-rest)
                  (let ((form (parse-form-or-it kw))
                        (accuvar nil))
                    (when (parse-kw-p 'into)
                      (unless (and (consp body-rest)
                                   (symbolp (setq accuvar (pop body-rest)))
                              )
                        (loop-syntax-error 'into)
                    ) )
                    (unless accuvar
                      (setq accuvar
                        (or accunum-var (setq accunum-var (gensym)))
                      )
                      (push accuvar results)
                    )
                    (when (consp body-rest)
                      (let ((kw2 (loop-keywordp (first body-rest))))
                        (when (or (not kw2) (eq kw2 'of-type))
                          (let ((type
                                  (if (not kw2)
                                    (pop body-rest)
                                    (progn (pop body-rest) (parse-form 'of-type))
                               )) )
                            (case kw
                              ((MAXIMIZE MAXIMIZING MINIMIZE MINIMIZING)
                               (setq type `(OR NULL ,type)))) ; wegen Startwert NIL
                            (push `(TYPE ,type ,accuvar) accu-declarations)
                    ) ) ) )
                    (case kw
                      ((MAXIMIZE MAXIMIZING MINIMIZE MINIMIZING)
                       (push accuvar accu-vars-nil))
                      ((COUNT COUNTING SUM SUMMING)
                       (push accuvar accu-vars-0)))
                    (case kw
                      ((COUNT COUNTING) `(WHEN ,form (INCF ,accuvar)))
                      ((SUM SUMMING) `(SETQ ,accuvar (+ ,accuvar ,form)))
                      ((MAXIMIZE MAXIMIZING) `(SETQ ,accuvar (MAX-IF ,form ,accuvar)))
                      ((MINIMIZE MINIMIZING) `(SETQ ,accuvar (MIN-IF ,form ,accuvar)))
                 )) )
                 ((IF WHEN UNLESS)
                  (pop body-rest)
                  (let* ((condition (parse-form kw))
                         (it-var (gensym))
                         used-it
                         (true-form
                           (let ((*last-it* it-var) (*used-it* nil))
                             (prog1
                               (parse-clauses kw)
                               (setq used-it *used-it*)
                         ) ) )
                         (false-form 'NIL))
                    (when (parse-kw-p 'else)
                      (setq false-form
                        (let ((*last-it* it-var) (*used-it* nil))
                          (prog1
                            (parse-clauses 'else)
                            (setq used-it (or used-it *used-it*))
                    ) ) ) )
                    (parse-kw-p 'end)
                    (when used-it
                      (psetq it-var `((,it-var ,condition))
                             condition it-var
                    ) )
                    (let ((form
                            `(IF ,(if (eq kw 'UNLESS)
                                    `(NOT ,condition) ; UNLESS
                                    `,condition ; IF, WHEN
                                  )
                               ,true-form
                               ,false-form
                             )
                         ))
                      (if used-it `(LET ,it-var ,form) `,form)
                 )) )
                 (t 'NIL)
       ) )   ) )
       (parse-clauses (kw) ; Nach kw: parst  clause {and clause}*
                           ; oder kurz       {clause}+{and}
         ; Liefert eine Lisp-Form.
         (let ((clauses nil))
           (loop
             (let ((clause (parse-clause)))
               (unless clause (loop-syntax-error kw))
               (push clause clauses)
             )
             (unless (parse-kw-p 'and) (return))
             (setq kw 'and)
             (setq *last-it* nil) ; 'it' ist nur in der ersten Klausel gültig
           )
           `(PROGN ,@(nreverse clauses))
       ) )
       ; Binden und Initialisieren von Variablen:
       ; Nach ANSI-CL 6.1.1.4 gelten zwei Grundregeln:
       ; - Beim Initialisieren von FOR-AS Variablen (außer FOR-AS-=) sind
       ;   mindestens alle vorherigen FOR-AS Variablen sichtbar.
       ; - Beim Initialisieren von FOR-AS-= Variablen sind alle FOR-AS Variablen
       ;   sichtbar.
       ; Zusätzlich ist die folgende Grundregel wünschenswert:
       ; - Beim Initialisieren von FOR-AS-= Variablen sind mindestens alle
       ;   vorherigen FOR-AS Variablen initialisiert und deren Abbruch-
       ;   bedingungen abgeprüft.
       ; Man könnte erst alle Variablen binden und dann im initially-code
       ; die Initialisierungen durchführen. Wir führen demgegenüber zwei
       ; Optimierungen durch:
       ; - Falls vor der FOR-AS Variablen keine FOR-AS-= Klausel kommt,
       ;   braucht die Variable zum Zeitpunkt ihrer Initialisierung nicht
       ;   sichtbar zu sein, und wir verlagern ihre Initialisierung nach
       ;   vorne, zur Bindung. Das geht aber nur, wenn vor der FOR-AS Variablen
       ;   keine FOR-AS Klausel mit Abbruchbedingung kommt.
       ; - Falls eine Variable gar nicht sichtbar zu sein braucht, weil keine
       ;   FOR-AS-= Klausel vorkommt und hinter ihr auch keine andere FOR-AS
       ;   Klausel stört, können die Bindung und die Initialiserung der
       ;   Variablen ins Schleifeninnere verschoben werden.
       (note-initialisation (initialisation)
         (when (or (li-bindings initialisation)
                   (li-declspecs initialisation)
                   (li-endtest-forms initialisation)
               )
           (when seen-for-as-= (setf (li-requires-stepbefore initialisation) t))
           (when (li-endtest-forms initialisation) (setq seen-endtest t))
           (push initialisation initialisations)
       ) )
       (make-endtest (endtest-form)
         (make-loop-init
           :specform 'PROGN
           :bindings nil
           :declspecs nil
           :endtest-forms (list endtest-form)
           :everytime (setq stepafter-code (cons 'NIL stepafter-code))
           :requires-stepbefore seen-endtest
       ) )
      )
      ;; Los geht's!
      ; parst: [named name]
      (when (parse-kw-p 'named)
        (unless (and (consp body-rest) (symbolp (first body-rest)))
          (loop-syntax-error 'named)
        )
        (setq block-name (pop body-rest))
      )
      (loop
        ; main ::= clause | termination | initially | finally |
        ;          with | for-as | repeat
        ; termination ::= {while | until | always | never | thereis} expr
        ; initially ::= initially {expr}*
        ; finally ::= finally { unconditional | {expr}* }
        ; with ::= with {var-typespec [= expr]}+{and}
        ; for-as ::= {for | as} {var-typespec ...}+{and}
        ; repeat ::= repeat expr
        (unless (consp body-rest) (return))
        (let ((clause (parse-clause)))
          (if clause
            (progn (setq already-within-main t) (push clause main-code))
            (let ((kw (loop-keywordp (first body-rest))))
              (case kw
                ((WHILE UNTIL ALWAYS NEVER THEREIS)
                 (pop body-rest)
                 (setq already-within-main t)
                 (let ((form (parse-form kw)))
                   (push (case kw
                           (WHILE `(UNLESS ,form (LOOP-FINISH)) )
                           (UNTIL `(WHEN ,form (LOOP-FINISH)) )
                           (ALWAYS
                             (push 'T results)
                             `(UNLESS ,form (RETURN-FROM ,block-name 'NIL))
                           )
                           (NEVER
                             (push 'T results)
                             `(WHEN ,form (RETURN-FROM ,block-name 'NIL))
                           )
                           (THEREIS
                             (let ((dummy (gensym)))
                               `(BLOCK ,dummy
                                  (RETURN-FROM ,block-name
                                    (OR ,form (RETURN-FROM ,dummy NIL))
                                ) )
                           ) )
                         )
                         main-code
                )) )
                ((INITIALLY)
                 (pop body-rest)
                 (push `(PROGN ,@(parse-progn)) initially-code)
                )
                ((FINALLY)
                 (pop body-rest)
                 (push (or (parse-unconditional) `(PROGN ,@(parse-progn)))
                       finally-code
                ))
                ((WITH FOR AS REPEAT)
                 (pop body-rest)
                 (when already-within-main
                   (warn (ENGLISH "~S: ~A clauses should occur before the loop's main body")
                         'loop (symbol-name kw)
                 ) )
                 (case kw
                   ((WITH)
                    (let ((bindings nil)
                          (declspecs nil))
                      (loop
                        (let (new-bindings)
                          (multiple-value-bind (pattern new-declspecs) (parse-var-typespec)
                            (if (parse-kw-p '=)
                              ; Initialisierungsform angegeben.
                              (let ((form (parse-form '=)))
                                (setq new-bindings (destructure pattern form))
                              )
                              ; keine Initialisierungsform angegeben.
                              (setq new-bindings (default-bindings (destructure-vars pattern) new-declspecs))
                            )
                            (setq bindings (revappend new-bindings bindings))
                            (setq declspecs (revappend new-declspecs declspecs))
                        ) )
                        (unless (parse-kw-p 'and) (return))
                        (setq kw 'and)
                      )
                      (note-initialisation
                        (make-loop-init
                          :specform 'LET
                          :bindings (nreverse bindings)
                          :declspecs (nreverse declspecs)
                          :everytime nil
                          :requires-stepbefore seen-endtest
                          :depends-preceding t
                      ) )
                   ))
                   ((FOR AS)
                    ; for-as ::= {for | as} for-as-clause {and [{for | as}] for-as-clause}*
                    ; for-as-clause ::= var-typespec
                    ;                   [{from | downfrom | upfrom} expr]
                    ;                   [{to | downto | upto | below | above} expr]
                    ;                   [by expr]
                    ; for-as-clause ::= var-typespec {in | on} expr [by expr]
                    ; for-as-clause ::= var-typespec = expr [then expr]
                    ; for-as-clause ::= var-typespec across expr
                    ; for-as-clause ::= var-typespec being {each | the}
                    ;                   {hash-key[s] | hash-value[s]}
                    ;                   {in | of} expr
                    ;                   [using ( {hash-value | hash-key} var ) ]
                    ; for-as-clause ::= var-typespec being {each | the}
                    ;                   {symbol[s] | present-symbol[s] | internal-symbol[s] | external-symbol[s]}
                    ;                   {in | of} expr
                    (let ((bindings nil)
                          (declspecs nil)
                          (initialisations nil)
                          (stepafter nil)
                          (old-seen-endtest seen-endtest)
                          (depends-preceding nil))
                      (flet ((note-initialisation (initialisation)
                               ; Aufrufe von note-initialisation müssen temporär aufgehoben werden.
                               (when (li-endtest-forms initialisation) (setq seen-endtest t))
                               (push initialisation initialisations)
                            ))
                        (loop
                          (multiple-value-bind (pattern new-declspecs) (parse-var-typespec)
                            (let ((preposition (next-kw)))
                              (case preposition
                                ((IN ON)
                                 (pop body-rest)
                                 (let ((start-form (parse-form preposition))
                                       (step-function-form '(FUNCTION CDR))
                                       (step-function-var nil))
                                   (when (parse-kw-p 'by)
                                     (setq step-function-form (parse-form 'by))
                                   )
                                   (unless (and (consp step-function-form)
                                                (eq (first step-function-form) 'FUNCTION)
                                                (consp (cdr step-function-form))
                                                (null (cddr step-function-form))
                                                (symbolp (second step-function-form))
                                           )
                                     (setq step-function-var (gensym))
                                   )
                                   (let ((var (gensym))) ; Hilfsvariable
                                     (push `(,var ,start-form) bindings)
                                     (when step-function-var
                                       (push `(,step-function-var ,step-function-form) bindings)
                                     )
                                     (note-initialisation
                                       (make-endtest `(WHEN (ENDP ,var) (LOOP-FINISH)))
                                     )
                                     (note-initialisation
                                       (make-loop-init
                                         :specform 'LET
                                         :bindings (destructure pattern (if (eq preposition 'IN) `(CAR ,var) `,var))
                                         :declspecs new-declspecs
                                         :everytime t
                                         :requires-stepbefore seen-endtest
                                     ) )
                                     (push
                                       (list var
                                             (if step-function-var
                                               `(FUNCALL ,step-function-var ,var)
                                               `(,(second step-function-form) ,var)
                                       )     )
                                       stepafter
                                )) ) )
                                (=
                                 (pop body-rest)
                                 (let* ((first-form (parse-form 'preposition))
                                        (then-form first-form))
                                   (when (parse-kw-p 'then)
                                     (setq then-form (parse-form 'then))
                                   )
                                   (setq bindings
                                     (revappend (destructure pattern first-form)
                                                bindings
                                   ) )
                                   (setq declspecs (revappend new-declspecs declspecs))
                                   (unless (and (constantp first-form) (constantp then-form))
                                     (setq seen-for-as-= t)
                                     ; Even when first-form is constant but then-form is not,
                                     ; we must set depends-preceding, because the stepafter-code
                                     ; depends on the order of the steppings, which forbids
                                     ; moving some code from initially-code + stepafter-code
                                     ; to stepbefore-code.
                                     (setq depends-preceding t)
                                   )
                                   (setq stepafter (revappend (destructure pattern then-form) stepafter))
                                ))
                                (ACROSS
                                 (pop body-rest)
                                 (let ((vector-form (parse-form preposition))
                                       (vector-var (gensym))
                                       (index-var (gensym)))
                                   (push `(,vector-var ,vector-form) bindings)
                                   (push `(,index-var 0) bindings)
                                   (note-initialisation
                                     (make-endtest `(WHEN (>= ,index-var (LENGTH ,vector-var)) (LOOP-FINISH)))
                                   )
                                   (note-initialisation
                                     (make-loop-init
                                       :specform 'LET
                                       :bindings (destructure pattern `(AREF ,vector-var ,index-var))
                                       :declspecs new-declspecs
                                       :everytime t
                                       :requires-stepbefore seen-endtest
                                   ) )
                                   (push (list index-var `(1+ ,index-var)) stepafter)
                                ))
                                (BEING
                                 (pop body-rest)
                                 (let ((plural (next-kw)))
                                   (case plural
                                     ((EACH THE) )
                                     (t (loop-syntax-error 'being))
                                   )
                                   (pop body-rest)
                                   (let ((preposition (next-kw)))
                                     (case preposition
                                       ((HASH-KEY HASH-VALUE
                                         SYMBOL PRESENT-SYMBOL INTERNAL-SYMBOL EXTERNAL-SYMBOL
                                        )
                                        (when (eq plural 'THE)
                                          (warn (ENGLISH "~S: After ~S a plural loop keyword is required, not ~A")
                                                'loop plural (symbol-name preposition)
                                       )) )
                                       ((HASH-KEYS HASH-VALUES
                                         SYMBOLS PRESENT-SYMBOLS INTERNAL-SYMBOLS EXTERNAL-SYMBOLS
                                        )
                                        (when (eq plural 'EACH)
                                          (warn (ENGLISH "~S: After ~S a singular loop keyword is required, not ~A")
                                                'loop plural (symbol-name preposition)
                                       )) )
                                       (t (loop-syntax-error plural))
                                     )
                                     (pop body-rest)
                                     (case (next-kw)
                                       ((IN OF) )
                                       (t (loop-syntax-error preposition))
                                     )
                                     (pop body-rest)
                                     (let ((form (parse-form preposition)))
                                       (case preposition
                                         ((HASH-KEY HASH-KEYS HASH-VALUE HASH-VALUES)
                                          (let ((other-pattern nil))
                                            (when (parse-kw-p 'using)
                                              (unless (and (consp body-rest)
                                                           (consp (car body-rest))
                                                           (consp (cdar body-rest))
                                                           (null (cddar body-rest))
                                                           (case (loop-keywordp (caar body-rest))
                                                             ((HASH-KEY HASH-KEYS)
                                                              (case preposition
                                                                ((HASH-VALUE HASH-VALUES) t) (t nil)
                                                             ))
                                                             ((HASH-VALUE HASH-VALUES)
                                                              (case preposition
                                                                ((HASH-KEY HASH-KEYS) t) (t nil)
                                                             ))
                                                      )    )
                                                (loop-syntax-error 'using)
                                              )
                                              (setq other-pattern (second (pop body-rest)))
                                            )
                                            (let ((state-var (gensym))
                                                  (nextp-var (gensym))
                                                  (nextkey-var (gensym))
                                                  (nextvalue-var (gensym)))
                                              (multiple-value-bind (nextmain-var nextother-var)
                                                (case preposition
                                                  ((HASH-KEY HASH-KEYS) (values nextkey-var nextvalue-var))
                                                  ((HASH-VALUE HASH-VALUES) (values nextvalue-var nextkey-var))
                                                )
                                                (push `(,state-var (SYS::HASH-TABLE-ITERATOR ,form)) bindings)
                                                (note-initialisation
                                                  (make-loop-init
                                                    :specform 'MULTIPLE-VALUE-BIND
                                                    :bindings `((,nextp-var ,nextkey-var ,nextvalue-var)
                                                                (SYS::HASH-TABLE-ITERATE ,state-var)
                                                               )
                                                    :declspecs (unless other-pattern `((IGNORE ,nextother-var)))
                                                    :endtest-forms `((UNLESS ,nextp-var (LOOP-FINISH)))
                                                    :everytime t
                                                    :requires-stepbefore seen-endtest
                                                ) )
                                                (note-initialisation
                                                  (make-loop-init
                                                    :specform 'LET
                                                    :bindings (destructure pattern nextmain-var)
                                                    :declspecs new-declspecs
                                                    :everytime t
                                                    :requires-stepbefore seen-endtest
                                                ) )
                                                (when other-pattern
                                                  (note-initialisation
                                                    (make-loop-init
                                                      :specform 'LET
                                                      :bindings (destructure other-pattern nextother-var)
                                                      :declspecs nil
                                                      :everytime t
                                                      :requires-stepbefore seen-endtest
                                                ) ) )
                                         )) ) )
                                         ((SYMBOL SYMBOLS PRESENT-SYMBOL PRESENT-SYMBOLS
                                           INTERNAL-SYMBOL INTERNAL-SYMBOLS EXTERNAL-SYMBOL EXTERNAL-SYMBOLS
                                          )
                                          (let ((flags (case preposition
                                                         ((SYMBOL SYMBOLS) '(:internal :external :inherited))
                                                         ((PRESENT-SYMBOL PRESENT-SYMBOLS) '(:internal :external))
                                                         ((INTERNAL-SYMBOL INTERNAL-SYMBOLS) '(:internal))
                                                         ((EXTERNAL-SYMBOL EXTERNAL-SYMBOLS) '(:external))
                                                )      )
                                                (state-var (gensym))
                                                (nextp-var (gensym))
                                                (nextsym-var (gensym)))
                                            (push `(,state-var (SYS::PACKAGE-ITERATOR ,form ',flags))
                                                  bindings
                                            )
                                            (note-initialisation
                                              (make-loop-init
                                                :specform 'MULTIPLE-VALUE-BIND
                                                :bindings `((,nextp-var ,nextsym-var)
                                                            (SYS::PACKAGE-ITERATE ,state-var)
                                                           )
                                                :declspecs nil
                                                :endtest-forms `((UNLESS ,nextp-var (LOOP-FINISH)))
                                                :everytime t
                                                :requires-stepbefore seen-endtest
                                            ) )
                                            (note-initialisation
                                              (make-loop-init
                                                :specform 'LET
                                                :bindings (destructure pattern nextsym-var)
                                                :declspecs new-declspecs
                                                :everytime t
                                                :requires-stepbefore seen-endtest
                                            ) )
                                         ))
                                )) ) ) )
                                (t
                                 (unless (symbolp pattern) (loop-syntax-error kw))
                                 ;; ANSI CL 6.1.2.1.1 implies that the start/end/by
                                 ;; clauses can come in any order, but only one of
                                 ;; each kind.
                                 (let ((step-start-p nil)
                                       (step-end-p nil)
                                       (step-by-p nil)
                                       step-start-form
                                       step-end-form
                                       step-by-form
                                       step-end-preposition
                                       dir)
                                   (loop
                                     (cond ((and (not step-start-p)
                                                 (setq dir (case preposition
                                                             (FROM 't)
                                                             (UPFROM 'up)
                                                             (DOWNFROM 'down)
                                                             (t nil))))
                                            (setq step-start-p dir)
                                            (pop body-rest)
                                            (setq step-start-form (parse-form preposition))
                                            (push `(,pattern ,step-start-form) bindings)
                                           )
                                           ((and (not step-end-p)
                                                 (setq dir (case preposition
                                                             (TO 't)
                                                             ((UPTO BELOW) 'up)
                                                             ((DOWNTO ABOVE) 'down)
                                                             (t nil))))
                                            (setq step-end-p dir)
                                            (setq step-end-preposition preposition)
                                            (pop body-rest)
                                            (setq step-end-form (parse-form preposition))
                                            (unless (constantp step-end-form)
                                              (let ((step-end-var (gensym)))
                                                (push `(,step-end-var ,step-end-form) bindings)
                                                (setq step-end-form step-end-var)
                                           )) )
                                           ((and (not step-by-p)
                                                 (eq preposition 'BY))
                                            (setq step-by-p t)
                                            (pop body-rest)
                                            (setq step-by-form (parse-form 'by))
                                            (unless (constantp step-by-form)
                                              (let ((step-by-var (gensym)))
                                                (push `(,step-by-var ,step-by-form) bindings)
                                                (setq step-by-form step-by-var)
                                           )) )
                                           (t (return))
                                     )
                                     (setq preposition (next-kw))
                                   )
                                   ;; All parsing done, gather the declarations:
                                   (setq declspecs (revappend new-declspecs declspecs))
                                   ;; Determine the direction of iteration:
                                   (let ((step-direction
                                           (if (or (eq step-start-p 'down) (eq step-end-p 'down))
                                             (if (or (eq step-start-p 'up) (eq step-end-p 'up))
                                               (error (ENGLISH "~S: questionable iteration direction after ~A")
                                                      'loop (symbol-name kw)
                                               )
                                               'down
                                             )
                                             'up
                                        )) )
                                     ;; Determine start, unless given:
                                     (unless step-start-p
                                       (when (eq step-direction 'down)
                                         ; Abwärtsiteration ohne Startwert ist nicht erlaubt.
                                         ; Die zweite optionale Klausel (d.h. preposition) muss abwärts zeigen.
                                         (error (ENGLISH "~S: specifying ~A requires FROM or DOWNFROM")
                                                'loop (symbol-name preposition)
                                       ) )
                                       ; Aufwärtsiteration -> Startwert 0
                                       (setq step-start-form '0)
                                       (push `(,pattern ,step-start-form) bindings)
                                     )
                                     ; Determine step, unless given:
                                     (unless step-by-p (setq step-by-form '1))
                                     ; Determine end test:
                                     (when step-end-p
                                       (let* ((compfun
                                                (if (eq step-direction 'up)
                                                  (if (eq step-end-preposition 'below) '>= '>) ; up
                                                  (if (eq step-end-preposition 'above) '<= '<) ; down
                                              ) )
                                              (endtest
                                                (if (and (constantp step-end-form) (zerop (eval step-end-form)))
                                                  (case compfun
                                                    (>= `(NOT (MINUSP ,pattern)) )
                                                    (> `(PLUSP ,pattern) )
                                                    (<= `(NOT (PLUSP ,pattern)) )
                                                    (< `(MINUSP ,pattern) )
                                                  )
                                                  `(,compfun ,pattern ,step-end-form)
                                             )) )
                                         (note-initialisation
                                           (make-endtest `(WHEN ,endtest (LOOP-FINISH)))
                                     ) ) )
                                     (push
                                       (list pattern `(,(if (eq step-direction 'up) '+ '-) ,pattern ,step-by-form))
                                       stepafter
                                )) ) )
                          ) ) )
                          (unless (parse-kw-p 'and) (return))
                          (setq kw 'and)
                          (case (next-kw) ((FOR AS) (pop body-rest)))
                      ) )
                      (when (setq stepafter (apply #'append (nreverse stepafter)))
                        (push `(PSETQ ,@stepafter) stepafter-code)
                      )
                      (push 'NIL stepafter-code) ; Markierung für spätere Initialisierungen
                      (note-initialisation
                        (make-loop-init
                          :specform 'LET
                          :bindings (nreverse bindings)
                          :declspecs (nreverse declspecs)
                          :everytime nil
                          :requires-stepbefore old-seen-endtest
                          :depends-preceding depends-preceding
                      ) )
                      (dolist (initialisation (nreverse initialisations))
                        (when (li-everytime initialisation)
                          (setf (li-everytime initialisation) stepafter-code)
                        )
                        (note-initialisation initialisation)
                      )
                   ))
                   ((REPEAT)
                    (let ((form (parse-form kw))
                          (var (gensym)))
                      (note-initialisation
                        (make-loop-init
                          :specform 'LET
                          :bindings `((,var ,form))
                          :declspecs nil
                          :everytime nil
                          :requires-stepbefore seen-endtest
                          :depends-preceding t
                      ) )
                      (push `(SETQ ,var (1- ,var)) stepafter-code)
                      (note-initialisation
                        (make-endtest `(UNLESS (PLUSP ,var) (LOOP-FINISH)))
                      )
                   ))
                ))
                (t (error (ENGLISH "~S: illegal syntax near ~S in ~S")
                          'loop (first body-rest) *whole*
                )  )
      ) ) ) ) )
      ; Noch einige semantische Tests:
      (setq results (delete-duplicates results :test #'equal))
      (when (> (length results) 1)
        (error (ENGLISH "~S: ambiguous result of loop ~S")
               'loop *whole*
      ) )
      (unless (null results)
        (push `(RETURN-FROM ,block-name ,@results) finally-code)
      )
      ; Initialisierungen abarbeiten und optimieren:
      (let ((initialisations1 nil)
            (initialisations2 nil))
        (unless seen-for-as-=
          (loop
            (when (null initialisations) (return))
            (let ((initialisation (first initialisations)))
              (unless (li-everytime initialisation) (return))
              ; letzte Initialiserungsklausel nach initialisations2 verschieben:
              (pop initialisations)
              (push initialisation initialisations2)
        ) ) )
        ; `depends-preceding' backpropagation:
        (let ((later-depend nil))
          (dolist (initialisation initialisations)
            (when later-depend (setf (li-later-depend initialisation) t))
            (when (li-depends-preceding initialisation) (setq later-depend t))
        ) )
        (setq initialisations (nreverse initialisations))
        (loop
          (when (null initialisations) (return))
          (let* ((initialisation (pop initialisations))
                 (everytime (li-everytime initialisation))
                 (requires-stepbefore (li-requires-stepbefore initialisation))
                 (name (li-specform initialisation))
                 (bindings (li-bindings initialisation))
                 (declarations (li-declspecs initialisation))
                 (vars (case name (MULTIPLE-VALUE-BIND (first bindings)) (LET (mapcar #'first bindings))))
                 (initforms
                   (case name
                     (MULTIPLE-VALUE-BIND `((MULTIPLE-VALUE-SETQ ,@bindings)))
                     (LET `((SETQ ,@(apply #'append bindings))))
                     (t '())
                 ) )
                 (endtest-forms (li-endtest-forms initialisation)))
            (if requires-stepbefore
              ; wegen seen-for-as-= oder AREF nicht optimierbar
              (progn
                (push
                  (make-loop-init
                    :specform 'LET
                    :bindings (default-bindings vars declarations)
                    :declspecs declarations
                  )
                  initialisations1
                )
                (if everytime
                  (if (li-later-depend initialisation)
                    (progn ; double code: initially-code and stepafter-code
                      (setq initially-code (revappend endtest-forms (revappend initforms initially-code)))
                      (setf (cdr everytime) (revappend endtest-forms (revappend initforms (cdr everytime))))
                    )
                    (setq stepbefore-code (revappend endtest-forms (revappend initforms stepbefore-code)))
                  )
                  (setq initially-code (revappend endtest-forms (revappend initforms initially-code)))
              ) )
              ; Initialisierungsklausel nach initialisations1 schaffen:
              (progn
                (push
                  (make-loop-init
                    :specform name
                    :bindings bindings
                    :declspecs declarations
                  )
                  initialisations1
                )
                (if everytime
                  (progn
                    ; put the initforms into the stepafter-code only.
                    (setf (cdr everytime) (revappend initforms (cdr everytime)))
                    ; handle the endtest-forms.
                    (if (li-later-depend initialisation)
                      (progn ; double endtest: initially-code and stepafter-code
                        (setq initially-code (revappend endtest-forms initially-code))
                        (setf (cdr everytime) (revappend endtest-forms (cdr everytime)))
                      )
                      (setq stepbefore-code (revappend endtest-forms stepbefore-code))
                  ) )
                  (setq initially-code (revappend endtest-forms initially-code))
            ) ) )
        ) )
        (setq initialisations1 (nreverse initialisations1))
        (push
          (make-loop-init
            :specform 'LET
            :bindings
              `(,@(map 'list #'(lambda (var) `(,var NIL)) *helpvars*)
                ,@(mapcar #'(lambda (var) `(,var NIL)) (delete-duplicates accu-vars-nil))
                ,@(mapcar #'(lambda (var) `(,var 0)) (delete-duplicates accu-vars-0))
               )
            :declspecs
              (nreverse accu-declarations)
          )
          initialisations1
        )
        ;; Remove the NIL placeholders in stepafter-code.
        (setq stepafter-code (delete 'NIL stepafter-code))
        ;; If initially-code and stepafter-code both end in the same
        ;; forms, drag these forms across the label to stepbefore-code.
        (flet ((form-eq (form1 form2) ; Calling EQUAL on user-given forms would be wrong
                 (or (eql form1 form2)
                     (and (consp form1) (consp form2)
                          (eql (length form1) (length form2))
                          (or (eq (car form1) (car form2))
                              (and (case (length form1) ((1 3) t))
                                   (case (car form1) ((SETQ PSETQ) t))
                                   (case (car form2) ((SETQ PSETQ) t))
                          )   )
                          (every #'eq (cdr form1) (cdr form2))
              )) )   )
          (loop
            (unless (and (consp initially-code) (consp stepafter-code)
                         (form-eq (car initially-code) (car stepafter-code))
                    )
              (return)
            )
            (setq stepbefore-code (nconc stepbefore-code (list (pop stepafter-code))))
            (pop initially-code)
        ) )
        ;; Final macroexpansion.
        `(MACROLET ((LOOP-FINISH () (LOOP-FINISH-ERROR)))
           (BLOCK ,block-name
             ,(wrap-initialisations (nreverse initialisations1)
                `(MACROLET ((LOOP-FINISH () '(GO END-LOOP)))
                   (TAGBODY
                     ,@(if initially-code `((PROGN ,@(nreverse initially-code))))
                     BEGIN-LOOP
                     ,@(if stepbefore-code `((PROGN ,@(nreverse stepbefore-code))))
                     ,(wrap-initialisations (nreverse initialisations2)
                        `(PROGN ,@(nreverse main-code))
                      )
                     ,@(if stepafter-code `((PROGN ,@(nreverse stepafter-code))))
                     (GO BEGIN-LOOP)
                     END-LOOP
                     ,@(mapcar #'(lambda (var) `(SETQ ,var (SYS::LIST-NREVERSE ,var)))
                               accu-vars-nreverse
                       )
                     (MACROLET ((LOOP-FINISH () (LOOP-FINISH-WARN) '(GO END-LOOP)))
                       ,@(nreverse finally-code)
                 ) ) )
              )
         ) )
) ) ) )

;; Der eigentliche Macro:

(defmacro loop (&whole whole &body body)
  (if (some #'loop-keywordp body)
    ; neue Form von LOOP
    (expand-loop whole body)
    ; alte Form von LOOP
    (let ((tag (gensym)))
      `(BLOCK NIL (TAGBODY ,tag ,@body (GO ,tag)))
) ) )
(defmacro loop-finish (&whole whole)
  (error (ENGLISH "~S is possible only from within ~S")
         whole 'loop
) )
(defun loop-finish-warn ()
  (warn (ENGLISH "Use of ~S in FINALLY clauses is deprecated because it can lead to infinite loops.")
        '(loop-finish)
) )
(defun loop-finish-error ()
  (error (ENGLISH "~S is not possible here")
         '(loop-finish)
) )

)

;; Run-Time-Support:

(defun max-if (x y)
  (if y (max x y) x)
)
(defun min-if (x y)
  (if y (min x y) x)
)

