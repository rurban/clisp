;; Tracer
;; Bruno Haible 13.2.1990, 15.3.1991, 4.4.1991

; (TRACE) liefert Liste der getraceten Funktionen
; (TRACE fun ...) tracet die Funktionen fun, ... zusätzlich.
; Format für fun:
;   Entweder ein Symbol
;        symbol
;   oder eine Liste aus einem Symbol und einigen Keyword-Argumenten (paarig!)
;        (symbol
;          [:suppress-if form]   ; kein Trace-Output, solange form erfüllt ist
;          [:step-if form]       ; Trace geht in den Stepper, falls form erfüllt
;          [:pre form]           ; führt vor Funktionsaufruf form aus
;          [:post form]          ; führt nach Funktionsaufruf form aus
;          [:pre-break-if form]  ; Trace geht vor Funktionsaufruf in die Break-Loop,
;                                ; falls form erfüllt
;          [:post-break-if form] ; Trace geht nach Funktionsaufruf in die Break-Loop,
;                                ; falls form erfüllt
;          [:pre-print form]     ; gibt die Werte von form vor Funktionsaufruf aus
;          [:post-print form]    ; gibt die Werte von form nach Funktionsaufruf aus
;          [:print form]         ; gibt die Werte von form vor und nach Funktionsaufruf aus
;        )
;   In all diesen Formen kann auf *TRACE-FUNCTION* (die Funktion selbst)
;   und *TRACE-ARGS* (die Argumente an die Funktion)
;   und *TRACE-FORM* (der Funktions-/Macro-Aufruf als Form)
;   und nach Funktionsaufruf auch auf *TRACE-VALUES* (die Liste der Werte
;   des Funktionsaufrufs) zugegriffen werden,
;   und mit RETURN kann der Aufruf mit gegebenen Werten verlassen werden.
; (UNTRACE) liefert Liste der getraceten Funktionen, streicht sie alle.
; (UNTRACE symbol ...) streicht symbol, ... aus der Liste der getraceten
;   Funktionen.
; TRACE und UNTRACE sind auch auf Funktionen (SETF symbol) und auf Macros anwendbar,
;   nicht jedoch auf lokal definierte Funktionen und Macros.

(in-package "LISP")
(export '(trace untrace
          *trace-function* *trace-args* *trace-form* *trace-values*
)        )
(in-package "SYSTEM")

(proclaim '(special *trace-function* *trace-args* *trace-form* *trace-values*))
(defvar *traced-functions* nil) ; Liste der momentan getraceden Funktionsnamen
  ; Solange ein Funktionsname funname [bzw. genauer: das Symbol
  ; symbol = (get-funname-symbol funname)] getraced ist, enthält
  ; die Property sys::traced-definition den alten Inhalt der Funktionszelle,
  ; die Property sys::tracing-definition den neuen Inhalt der Funktionszelle,
  ; und ist der Funktionsname Element der Liste *traced-functions*.
  ; Währenddessen kann sich der Inhalt der Funktionszelle jedoch ändern!
  ; Jedenfalls gilt stets:
  ;        (and (fboundp symbol)
  ;             (eq (symbol-function symbol) (get symbol 'sys::tracing-definition))
  ;        )
  ; ===>   (member funname *traced-functions* :test #'equal)
  ; <==>   (get symbol 'sys::traced-definition)
(defvar *trace-level* 0) ; Verschachtelungstiefe bei der Trace-Ausgabe

; Funktionen, die der Tracer zur Laufzeit aufruft und die der Benutzer
; tracen könnte, müssen in ihrer ungetraceden Form aufgerufen werden.
; Statt (fun arg ...) verwende daher (SYS::%FUNCALL '#,#'fun arg ...)
; oder (SYS::%FUNCALL (LOAD-TIME-VALUE #'fun) arg ...).
; Dies gilt für alle hier verwendeten Funktionen von #<PACKAGE LISP> außer
; CAR, CDR, CONS, APPLY, VALUES-LIST (die alle inline compiliert werden).

(defmacro trace (&rest funs)
  (if (null funs)
    '*traced-functions*
    (cons 'append
      (mapcar #'(lambda (fun)
                  (if (or (atom fun) (function-name-p fun))
                    (trace1 fun)
                    (apply #'trace1 fun)
                ) )
              funs
    ) )
) )

(defun trace1 (funname &key (suppress-if nil) (step-if nil)
                            (pre nil) (post nil)
                            (pre-break-if nil) (post-break-if nil)
                            (pre-print nil) (post-print nil) (print nil)
                       &aux (old-function (gensym)) (macro-flag (gensym))
              )
  (unless (function-name-p funname)
    (error-of-type 'source-program-error
      (ENGLISH "~S: function name should be a symbol, not ~S")
      'trace funname
  ) )
  (let ((symbolform
          (if (atom funname)
            `',funname
            `(load-time-value (get-setf-symbol ',(second funname)))
       )) )
    `(block nil
       (unless (fboundp ,symbolform) ; Funktion überhaupt definiert?
         (warn (ENGLISH "~S: undefined function ~S")
               'trace ',funname
         )
         (return nil)
       )
       (when (special-operator-p ,symbolform) ; Special-Form: nicht tracebar
         (warn (ENGLISH "~S: cannot trace special form ~S")
               'trace ',funname
         )
         (return nil)
       )
       (let* ((,old-function (symbol-function ,symbolform))
              (,macro-flag (consp ,old-function)))
         (unless (eq ,old-function (get ,symbolform 'sys::tracing-definition)) ; schon getraced?
           (setf (get ,symbolform 'sys::traced-definition) ,old-function)
           (pushnew ',funname *traced-functions* :test #'equal)
         )
         (format t (ENGLISH "~&;; Tracing ~:[function~;macro~] ~S.")
                   ,macro-flag ',funname
         )
         (setf (get ,symbolform 'sys::tracing-definition)
           (setf (symbol-function ,symbolform)
             ; neue Funktion, die die ursprüngliche ersetzt:
             ,(let ((newname (concat-pnames "TRACED-" (get-funname-symbol funname)))
                    (body
                      `((declare (compile) (inline car cdr cons apply values-list))
                        (let ((*trace-level* (trace-level-inc)))
                          (block nil
                            (unless ,suppress-if
                              (trace-pre-output)
                            )
                            ,@(when pre-print
                                `((trace-print (multiple-value-list ,pre-print)))
                              )
                            ,@(when print
                                `((trace-print (multiple-value-list ,print)))
                              )
                            ,pre
                            ,@(when pre-break-if
                                `((when ,pre-break-if (sys::break-loop t)))
                              )
                            (let ((*trace-values*
                                    (multiple-value-list
                                      (if ,step-if
                                        (trace-step-apply)
                                        (apply *trace-function* *trace-args*)
                                 )) ) )
                              ,@(when post-break-if
                                  `((when ,post-break-if (sys::break-loop t)))
                                )
                              ,post
                              ,@(when print
                                  `((trace-print (multiple-value-list ,print)))
                                )
                              ,@(when post-print
                                  `((trace-print (multiple-value-list ,post-print)))
                                )
                              (unless ,suppress-if
                                (trace-post-output)
                              )
                              (values-list *trace-values*)
                       )) ) )
                   ))
                `(if (not ,macro-flag)
                   (function ,newname
                     (lambda (&rest *trace-args*
                              &aux (*trace-form* (make-apply-form ',funname *trace-args*))
                                   (*trace-function* (get-traced-definition ,symbolform))
                             )
                       ,@body
                   ) )
                   (make-macro
                     (function ,newname
                       (lambda (&rest *trace-args*
                                &aux (*trace-form* (car *trace-args*))
                                     (*trace-function* (cdr (get-traced-definition ,symbolform)))
                               )
                         ,@body
                   ) ) )
                 )
              )
       ) ) )
       '(,funname)
     )
) )

;; Hilfsfunktionen:
; Nächsthöheres Trace-Level liefern:
(defun trace-level-inc ()
  (%funcall '#,#'1+ *trace-level*)
)
; Ursprüngliche Funktionsdefinition holen:
(defun get-traced-definition (symbol)
  (%funcall '#,#'get symbol 'sys::traced-definition)
)
; Anwenden, aber durchsteppen:
(defun trace-step-apply ()
  ;(eval `(step (apply ',*trace-function* ',*trace-args*)))
  (%funcall '#,#'eval
    (cons 'step
     (cons
       (cons 'apply
        (cons (cons 'quote (cons *trace-function* nil))
         (cons (cons 'quote (cons *trace-args* nil))
          nil
       )))
      nil
    ))
  )
)
; Eval-Form bauen, die einem Apply (näherungsweise) entspricht:
(defun make-apply-form (funname args)
  (declare (inline cons mapcar))
  (cons funname
    (mapcar #'(lambda (arg)
                ;(list 'quote arg)
                (cons 'quote (cons arg nil))
              )
            args
  ) )
)
; Output vor Aufruf, benutzt *trace-level* und *trace-form*
(defun trace-pre-output ()
  (%funcall '#,#'terpri *trace-output*)
  (%funcall '#,#'write *trace-level* :stream *trace-output* :base 10 :radix t)
  (%funcall '#,#'write-string " Trace: " *trace-output*)
  (%funcall '#,#'prin1 *trace-form* *trace-output*)
)
; Output nach Aufruf, benutzt *trace-level*, *trace-form* und *trace-values*
(defun trace-post-output ()
  (declare (inline car cdr consp atom))
  (%funcall '#,#'terpri *trace-output*)
  (%funcall '#,#'write *trace-level* :stream *trace-output* :base 10 :radix t)
  (%funcall '#,#'write-string " Trace: " *trace-output*)
  (%funcall '#,#'write (car *trace-form*) :stream *trace-output*)
  (%funcall '#,#'write-string " ==> " *trace-output*)
  (trace-print *trace-values* nil)
)
; Output einer Liste von Werten:
(defun trace-print (vals &optional (nl-flag t))
  (when nl-flag (%funcall '#,#'terpri *trace-output*))
  (when (consp vals)
    (loop
      (let ((val (car vals)))
        (%funcall '#,#'prin1 val *trace-output*)
      )
      (setq vals (cdr vals))
      (when (atom vals) (return))
      (%funcall '#,#'write-string ", " *trace-output*)
) ) )

(defmacro untrace (&rest funs)
  `(mapcan #'untrace1 ,(if (null funs) `(copy-list *traced-functions*) `',funs))
)

(defun untrace1 (funname)
  (unless (function-name-p funname)
    (error-of-type 'source-program-error
      (ENGLISH "~S: function name should be a symbol, not ~S")
      'untrace funname
  ) )
  (let* ((symbol (get-funname-symbol funname))
         (old-definition (get symbol 'sys::traced-definition)))
    (prog1
      (if old-definition
        ; symbol war getraced
        (progn
          (if (and (fboundp symbol)
                   (eq (symbol-function symbol) (get symbol 'sys::tracing-definition))
              )
            (setf (symbol-function symbol) old-definition)
            (warn (ENGLISH "~S: ~S was traced and has been redefined!")
                  'untrace funname
          ) )
          `(,funname)
        )
        ; funname war nicht getraced
        '()
      )
      (untrace2 funname)
) ) )

(defun untrace2 (funname)
  (let ((symbol (get-funname-symbol funname)))
    (remprop symbol 'sys::traced-definition)
    (remprop symbol 'sys::tracing-definition)
  )
  (setq *traced-functions* (delete funname *traced-functions* :test #'equal))
)

