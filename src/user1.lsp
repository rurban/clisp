;;;; User-Interface, Teil 1
;;;; Eval-Env, Debugger, Stepper, Errors, Query-User
;;;; Bruno Haible 4.2.1990, 4.11.1991

(in-package "LISP")
(export '(eval-env with-keyboard *keyboard-input* *prompt-with-package*))
(in-package "SYSTEM")

;-------------------------------------------------------------------------------
;;                                 EVAL-ENV

; Das Toplevel-Environment
(defparameter *toplevel-environment* (eval '(the-environment)))
(defparameter *toplevel-denv* (svref *toplevel-environment* 4))

; Evaluiert eine Form in einem Environment
(defun eval-env (form &optional (env *toplevel-environment*))
  (evalhook form nil nil env)
)

;-------------------------------------------------------------------------------
;;                                 Debugger

(defvar *break-count* 0) ; Anzahl der aktiven Break-Schleifen (Fixnum >=0)

; Zähler zum Vermeiden von Endlosrekursionen wegen *error-output*
(defvar *recurse-count-error-output* 0)
; Zähler zum Vermeiden von Endlosrekursionen wegen *debug-io*
(defvar *recurse-count-debug-io* 0)

; Hauptschleife:
; (driver
;   #'(lambda () (read-eval-print "> "))
; )

(defvar *prompt-with-package* t)

(defvar *home-package* nil)

(defun prompt-string-package ()
  (unless *home-package*
    (setf *home-package* *package*))
  (if (and (packagep *package*) (package-name *package*))
    (if (or (not (find-symbol "T" *package*)) ; Ist *package* eine Package ohne Lisp-Syntax?
            (and *prompt-with-package*
                 (not (eq *package* *home-package*)) ) )
        (string-concat "[" (package-name *package*) "]")
        "" )
    (DEUTSCH "[*package* ungültig]"
     ENGLISH "[*package* invalid]"
     FRANCAIS "[*package* invalide]")
) )
; Vom Prompt der erste Teil:
(defun prompt-string1 () "")
; Vom Prompt der zweite Teil:
(defun prompt-string2 () (prompt-string-package))
; Vom Prompt der letzte Teil:
(defun prompt-string3 () "> ")

; Help-Funktion:
(defvar *key-bindings* nil) ; Liste von Tasten-Bindungen und Helpstrings
(defun help ()
  (dolist (s (reverse (remove-if-not #'stringp *key-bindings*)))
    (write-string s #|*debug-io*|#)
) )

; Bausteine der Break-Schleife:
(defvar *debug-frame*)
(defvar *debug-mode*)
(defvar *frame-limit1* nil) ; untere Grenze für frame-down und frame-down-1
(defvar *frame-limit2* nil) ; obere Grenze für frame-up und frame-up-1
(defun frame-limit1 (frames-to-skip)
  (let ((frame (the-frame)))
    (let ((*frame-limit1* nil)
          (*frame-limit2* nil))
      (dotimes (i frames-to-skip) (setq frame (frame-up-1 frame 1)))
    )
    frame
) )
(defun frame-limit2 ()
  (let ((frame (the-frame)))
    (let ((*frame-limit1* nil)
          (*frame-limit2* nil))
      (loop
        (let ((nextframe (frame-up-1 frame 1)))
          (when (or (eq nextframe frame) (driver-frame-p nextframe)) (return))
          (setq frame nextframe)
      ) )
      (dotimes (i 2) (setq frame (frame-down-1 frame 1)))
    )
    frame
) )
(defun debug-help () (help) (throw 'debug 'continue))
(defun debug-unwind () (throw 'debug 'unwind))
(defun debug-mode-1 () (setq *debug-mode* 1) (throw 'debug 'continue))
(defun debug-mode-2 () (setq *debug-mode* 2) (throw 'debug 'continue))
(defun debug-mode-3 () (setq *debug-mode* 3) (throw 'debug 'continue))
(defun debug-mode-4 () (setq *debug-mode* 4) (throw 'debug 'continue))
(defun debug-mode-5 () (setq *debug-mode* 5) (throw 'debug 'continue))
(defun debug-where ()
  (describe-frame *standard-output* *debug-frame*)
  (throw 'debug 'continue)
)
(defun debug-up ()
  (describe-frame *standard-output*
    (setq *debug-frame* (frame-up-1 *debug-frame* *debug-mode*))
  )
  (throw 'debug 'continue)
)
(defun debug-top ()
  (describe-frame *standard-output*
    (setq *debug-frame* (frame-up *debug-frame* *debug-mode*))
  )
  (throw 'debug 'continue)
)
(defun debug-down ()
  (describe-frame *standard-output*
    (setq *debug-frame* (frame-down-1 *debug-frame* *debug-mode*))
  )
  (throw 'debug 'continue)
)
(defun debug-bottom ()
  (describe-frame *standard-output*
    (setq *debug-frame* (frame-down *debug-frame* *debug-mode*))
  )
  (throw 'debug 'continue)
)
(defun debug-backtrace (&optional (mode *debug-mode*))
  (let ((frame (frame-down-1 (frame-up-1 *frame-limit1* mode) mode)))
    (loop
      (describe-frame *standard-output* frame)
      (when (eq frame (setq frame (frame-up-1 frame mode))) (return))
  ) )
  (throw 'debug 'continue)
)
(defun debug-backtrace-1 () (debug-backtrace 1))
(defun debug-backtrace-2 () (debug-backtrace 2))
(defun debug-backtrace-3 () (debug-backtrace 3))
(defun debug-backtrace-4 () (debug-backtrace 4))
(defun debug-backtrace-5 () (debug-backtrace 5))
(defun debug-trap-on ()
  (trap-eval-frame *debug-frame* t)
  (throw 'debug 'continue)
)
(defun debug-trap-off ()
  (trap-eval-frame *debug-frame* nil)
  (throw 'debug 'continue)
)
(defun debug-redo ()
  (redo-eval-frame *debug-frame*)
  (throw 'debug 'continue)
)
(defun debug-return ()
  (return-from-eval-frame *debug-frame*
    (read-form (DEUTSCH "Werte: "
                ENGLISH "values: "
                FRANCAIS "Valeurs : ")
  ) )
  (throw 'debug 'continue)
)
(defun debug-continue () (throw 'debug 'quit))

(defun commands0 ()
             (list
               (DEUTSCH "
Help = diese Liste
Benutzen Sie die üblichen Editiermöglichkeiten."
                ENGLISH "
Help = this list
Use the usual editing capabilities."
                FRANCAIS "
Help = cette liste
Éditez de la façon habituelle."
               )
               (cons "Help"   #'debug-help  )
)            )
(defun commands1 ()
             (list
               (DEUTSCH "
Help   = dieses Menü
Abort  = Abbruch, Rücksprung zur nächsthöheren Eingabeschleife
Unwind = Abbruch, Rücksprung zur nächsthöheren Eingabeschleife
Mode-1 = alle Stack-Elemente inspizieren
Mode-2 = alle Frames inspizieren
Mode-3 = nur lexikalische Frames inspizieren
Mode-4 = nur EVAL- und APPLY-Frames inspizieren (Default)
Mode-5 = nur APPLY-Frames inspizieren
Where  = diesen Frame inspizieren
Up     = nächsthöheren Frame inspizieren
Top    = obersten Frame inspizieren
Down   = nächstneueren Frame inspizieren
Bottom = neuesten Frame inspizieren
Backtrace-1 = alle Stack-Elemente auflisten
Backtrace-2 = alle Frames auflisten
Backtrace-3 = alle lexikalische Frames auflisten
Backtrace-4 = alle EVAL- und APPLY-Frames auflisten
Backtrace-5 = alle APPLY-Frames auflisten
Backtrace   = Stack auflisten im aktuellen Mode
Break+ = Breakpoint im EVAL-Frame setzen
Break- = Breakpoint im EVAL-Frame löschen
Redo   = Form im EVAL-Frame erneut auswerten
Return = EVAL-Frame mit gegebenen Werten verlassen"
                ENGLISH "
Help   = this command list
Abort  = abort to the next recent input loop
Unwind = abort to the next recent input loop
Mode-1 = inspect all the stack elements
Mode-2 = inspect all the frames
Mode-3 = inspect only lexical frames
Mode-4 = inspect only EVAL and APPLY frames (default)
Mode-5 = inspect only APPLY frames
Where  = inspect this frame
Up     = go up one frame, inspect it
Top    = go to top frame, inspect it
Down   = go down one frame, inspect it
Bottom = go to bottom (most recent) frame, inspect it
Backtrace-1 = list all stack elements
Backtrace-2 = list all frames
Backtrace-3 = list all lexical frames
Backtrace-4 = list all EVAL and APPLY frames
Backtrace-5 = list all APPLY frames
Backtrace   = list stack in current mode
Break+ = set breakpoint in EVAL frame
Break- = disable breakpoint in EVAL frame
Redo   = re-evaluate form in EVAL frame
Return = leave EVAL frame, prescribing the return values"
                FRANCAIS "
Help   = ce menu-ci
Abort  = arrêt, retour au niveau supérieur
Unwind = arrêt, retour au niveau supérieur
Mode-1 = examiner tous les éléments de la pile
Mode-2 = examiner tous les «frames»
Mode-3 = examiner uniquement les «frames» lexicaux
Mode-4 = examiner uniquement les «frames» EVAL et APPLY (par défaut)
Mode-5 = examiner uniquement les «frames» APPLY
Where  = examiner ce «frame»
Up     = examiner un «frame» supérieur
Top    = examiner le «frame» le plus élevé
Down   = examiner un prochain «frame» plus récent (inférieur)
Bottom = examiner le «frame» le plus récent (le plus bas)
Backtrace-1 = montrer tous les éléments de la pile
Backtrace-2 = montrer tous les «frames»
Backtrace-3 = montrer tous les «frames» lexicaux
Backtrace-4 = montrer tous les «frames» EVAL et APPLY
Backtrace-5 = montrer tous les «frames» APPLY
Backtrace   = montrer la pile en mode actuel
Break+ = placer un point d'interception dans le «frame» EVAL
Break- = enlever le point d'interception du «frame» EVAL
Redo   = réévaluer la forme dans le «frame» EVAL
Return = quitter le «frame» EVAL avec certaines valeurs"
               )
               (cons "Help"   #'debug-help  )
               (cons "?"      #'debug-help  )
               (cons "Abort"  #'debug-unwind)
               (cons "Unwind" #'debug-unwind)
               (cons "Mode-1" #'debug-mode-1)
               (cons "Mode-2" #'debug-mode-2)
               (cons "Mode-3" #'debug-mode-3)
               (cons "Mode-4" #'debug-mode-4)
               (cons "Mode-5" #'debug-mode-5)
               (cons "Where"  #'debug-where )
               (cons "Up"     #'debug-up    )
               (cons "Top"    #'debug-top   )
               (cons "Down"   #'debug-down  )
               (cons "Bottom" #'debug-bottom)
               (cons "Backtrace-1" #'debug-backtrace-1)
               (cons "Backtrace-2" #'debug-backtrace-2)
               (cons "Backtrace-3" #'debug-backtrace-3)
               (cons "Backtrace-4" #'debug-backtrace-4)
               (cons "Backtrace-5" #'debug-backtrace-5)
               (cons "Backtrace"   #'debug-backtrace  )
)            )
(defun commands2 ()
             (list
               (cons "Break+" #'debug-trap-on )
               (cons "Break-" #'debug-trap-off)
               (cons "Redo"   #'debug-redo  )
               (cons "Return" #'debug-return)
)            )
(defun commands3 ()
             (list
               (DEUTSCH "
Continue = Rest weiter abarbeiten"
                ENGLISH "
Continue = continue evaluation"
                FRANCAIS "
Continue = continuer l'évaluation"
               )
               (cons "Continue" #'debug-continue)
)            )

;; um Help-Kommando erweiterte Hauptschleife.
(defun main-loop ()
  (setq *break-count* 0)
  (driver ; Driver-Frame aufbauen und folgende Funktion (endlos) ausführen:
    #'(lambda ()
        (catch 'debug ; die (throw 'debug ...) abfangen
          (if ; Eingabezeile verlangen
              (read-eval-print (string-concat (prompt-string1) (prompt-string2) (prompt-string3))
                               (copy-list (commands0))
              )
            ; T -> #<EOF>
            (exit)
            ; NIL -> Form bereits ausgewertet und ausgegeben
) )   ) ) )
(setq *driver* #'main-loop)

;; komfortable Break-Schleife. (Läuft nur in compiliertem Zustand!)
(defun break-loop (continuable &optional (condition nil) (print-it nil)
                   &aux (may-continue
                          (or continuable
                              (and condition (find-restart 'continue condition))
                        ) )
                        (interactive-p (interactive-stream-p *debug-io*))
                        (commandsr '())
                  )
  (when (and print-it (typep condition (clos:find-class 'condition)))
    (symbol-stream '*error-output* :output)
    ; Ein Zeichen auf *error-output* ausgeben, mit Abfangen von Endlosrekursion:
    (let ((*recurse-count-error-output* (1+ *recurse-count-error-output*)))
      (when (> *recurse-count-error-output* 3)
        (setq *recurse-count-error-output* 0)
        (makunbound '*error-output*)
        (let ((*recurse-count-debug-io* (1+ *recurse-count-debug-io*)))
          (when (> *recurse-count-debug-io* 3)
            (setq *recurse-count-debug-io* 0)
            (makunbound '*debug-io*)
            (symbol-stream '*debug-io* :io)
          )
          (symbol-stream '*error-output* :output)
      ) )
      (terpri *error-output*)
    )
    (if may-continue
      (progn (write-string "** - Continuable Error" *error-output*) (terpri *error-output*))
      (write-string "*** - " *error-output*)
    )
    ;; Output the error message, but don't trap into recursive errors.
    (let ((*recursive-error-count* (1+ *recursive-error-count*)))
      (if (> *recursive-error-count* 3)
        (progn
          (setq *recursive-error-count* 0)
          (write-string (DEUTSCH "Unausgebbare Fehlermeldung"
                         ENGLISH "Unprintable error message"
                         FRANCAIS "Message inimprimable")
                        *error-output*
        ) )
        (sys::print-condition condition *error-output*)
    ) )
    (symbol-stream '*debug-io* :io)
    (when may-continue
      (if continuable
        (when interactive-p
          (terpri *debug-io*)
          (write-string (DEUTSCH "Sie können (mit Continue) fortfahren."
                         ENGLISH "You can continue (by typing 'continue')."
                         FRANCAIS "Vous pouvez continuer (tapez «continue» pour cela).")
                        *debug-io*
          )
        )
        (progn
          (terpri *debug-io*)
          (when interactive-p
            (write-string (DEUTSCH "Wenn Sie (mit Continue) fortfahren: "
                           ENGLISH "If you continue (by typing 'continue'): "
                           FRANCAIS "Si vous continuez (en tapant «continue»): ")
                          *debug-io*
            )
          )
          (princ may-continue *debug-io*)
  ) ) ) )
  (when condition
    (let ((restarts (remove may-continue (compute-restarts condition))))
      (when restarts
        (when interactive-p
          (terpri *debug-io*)
          (write-string (if may-continue
                          (DEUTSCH "Weitere mögliche Optionen:"
                           ENGLISH "The following restarts are available too:"
                           FRANCAIS "D'autres rentrées possibles:")
                          (DEUTSCH "Mögliche Optionen:"
                           ENGLISH "The following restarts are available:"
                           FRANCAIS "Rentrées possibles:")
                        )
                        *debug-io*
        ) )
        (let ((counter 0))
          (dolist (restart restarts)
            (let* ((command (string-concat "R" (sys::decimal-string (incf counter))))
                   (helpstring (string-concat "
" command " = " (princ-to-string restart))))
              ; Restart-Möglichkeit ausgeben:
              (when interactive-p
                (write-string helpstring *debug-io*)
              )
              (push helpstring commandsr)
              ; und in die Liste commandsr aufnehmen:
              (push (cons command
                          (let ((restart restart))
                            #'(lambda () (invoke-restart-interactively restart))
                    )     )
                    commandsr
          ) ) )
          (setq commandsr (nreverse commandsr))
  ) ) ) )
  (tagbody
    (clear-input *debug-io*) ; because the user didn't expect a break loop
    (let* ((*break-count* (1+ *break-count*))
           (stream (make-synonym-stream '*debug-io*))
           (*standard-input* stream)
           (*standard-output* stream)
           (prompt (with-output-to-string (s)
                      (write-string (prompt-string1) s)
                      (write *break-count* :stream s)
                      (write-string ". Break" s)
                      (write-string (prompt-string2) s)
                      (write-string (prompt-string3) s)
           )       )
           (*frame-limit1* (frame-limit1 13))
           (*frame-limit2* (frame-limit2))
           (*debug-mode* 4)
           (*debug-frame* (frame-down-1 (frame-up-1 *frame-limit1* *debug-mode*) *debug-mode*))
          )
      (driver ; Driver-Frame aufbauen und folgende Funktion (endlos) ausführen:
        #'(lambda ()
            (case
                (catch 'debug ; die (throw 'debug ...) abfangen und analysieren
                  (same-env-as *debug-frame* ; bei *debug-frame* gültiges Environment aufbauen
                    #'(lambda ()
                        (if ; Eingabezeile verlangen
                            (read-eval-print prompt
                              (nconc (copy-list (commands1))
                                     (when (eval-frame-p *debug-frame*) (copy-list (commands2)))
                                     (when may-continue (copy-list (commands3)))
                                     commandsr
                            ) )
                          ; T -> #<EOF>
                          (throw 'debug (if may-continue 'quit 'unwind))
                          ; NIL -> Form bereits ausgewertet und ausgegeben
                          #|(throw 'debug 'continue)|#
                ) )   ) )
              (unwind (go unwind))
              (quit ; nur erreicht, falls may-continue
                (if continuable
                  (go quit)
                  (invoke-restart-interactively may-continue)
              ) )
              (t ) ; alles andere, insbesondere continue
    ) )   ) )
    unwind (unwind-to-driver)
    quit
) )
(setq *break-driver* #'break-loop)

;-------------------------------------------------------------------------------
;;        komfortabler Stepper. (Läuft nur in compiliertem Zustand!)

(defvar *step-level* 0) ; momentane Step-Tiefe
(defvar *step-quit* most-positive-fixnum) ; kritische Step-Tiefe:
  ; sobald diese unterschritten wird, wacht der Stepper wieder auf.
(defvar *step-watch* nil) ; Abbruchbedingung

; (STEP form), CLTL S. 441
(defmacro step (form)
  `(let* ((*step-level* 0)
          (*step-quit* most-positive-fixnum)
          (*step-watch* nil)
          (*evalhook* #'step-hook-fn))
     ,form
   )
)

(defun commands4 ()
             (list
               (DEUTSCH "
Step     = Step into form: diese Form im Einzelschrittmodus ausführen
Next     = Step over form: diese Form auf einmal ausführen
Over     = Step over this level: bis zum Aufrufer auf einmal ausführen
Continue = Einzelschrittmodus abschalten, Rest ausführen
Step-until, Next-until, Over-until, Continue-until:
           dito, jedoch mit Angabe einer Abbruchbedingung"
                ENGLISH "
Step     = step into form: evaluate this form in single step mode
Next     = step over form: evaluate this form at once
Over     = step over this level: evaluate at once up to the next return
Continue = switch off single step mode, continue evaluation
Step-until, Next-until, Over-until, Continue-until:
           same as above, specify a condition when to stop"
                FRANCAIS "
Step     = step into form: évaluer cette forme petit à petit
Next     = step over form: évaluer cette forme en bloc
Over     = step over this level: évaluer tout le reste jusqu'au prochain retour
Continue = continue: évaluer tout le reste en bloc
Step-until, Next-until, Over-until, Continue-until:
           de même, avec spécification d'une condition d'arrêt"
               )
               (cons "Step"     #'(lambda () (throw 'stepper 'into)))
               (cons "Next"     #'(lambda () (throw 'stepper 'over)))
               (cons "Over"     #'(lambda () (throw 'stepper 'over-this-level)))
               (cons "Continue" #'(lambda () (throw 'stepper 'continue)))
               (cons "Step-until"     #'(lambda () (throw 'stepper (values 'into t))))
               (cons "Next-until"     #'(lambda () (throw 'stepper (values 'over t))))
               (cons "Over-until"     #'(lambda () (throw 'stepper (values 'over-this-level t))))
               (cons "Continue-until" #'(lambda () (throw 'stepper (values 'continue t))))
)            )

(defun step-values (values)
  (let ((*standard-output* *debug-io*))
    (terpri #|*debug-io*|#)
    (write-string (DEUTSCH "Step "
                   ENGLISH "step "
                   FRANCAIS "Step ")
                  #|*debug-io*|#
    )
    (write *step-level* #|:stream *debug-io*|#)
    (write-string " ==> " #|*debug-io*|#)
    (case (length values)
      (0 (write-string (DEUTSCH "Keine Werte"
                        ENGLISH "no values"
                        FRANCAIS "Aucune valeur")
                       #|*debug-io*|#
      )  )
      (1 (write-string (DEUTSCH "Wert: "
                        ENGLISH "value: "
                        FRANCAIS "Valeur : ")
                       #|*debug-io*|#
         )
         (write (car values) #|:stream *debug-io*|#)
      )
      (t (write (length values) #|:stream *debug-io*|#)
         (write-string (DEUTSCH " Werte: "
                        ENGLISH " values: "
                        FRANCAIS " Valeurs : ")
                       #|*debug-io*|#
         )
         (do ((L values))
             ((endp L))
           (write (pop L) #|:stream *debug-io*|#)
           (unless (endp L) (write-string ", " #|*debug-io*|#))
      )  )
  ) )
  (values-list values)
)

(defun step-hook-fn (form &optional (env *toplevel-environment*))
  (let ((*step-level* (1+ *step-level*)))
    (when (>= *step-level* *step-quit*) ; Solange *step-level* >= *step-quit*
      (if (and *step-watch* (funcall *step-watch*)) ; und kein Breakpoint,
        (setq *step-quit* most-positive-fixnum)
        (return-from step-hook-fn ; ist der Stepper passiv
          (evalhook form nil nil env) ; (d.h. er evaluiert die Form einfach)
    ) ) )
    (tagbody
      (let* ((stream (make-synonym-stream '*debug-io*))
             (*standard-input* stream)
             (*standard-output* stream)
             (prompt (with-output-to-string (s)
                       (write-string (prompt-string1) s)
                       (write-string "Step " s)
                       (write *step-level* :stream s)
                       (write-string (prompt-string2) s)
                       (write-string (prompt-string3) s)
             )       )
             (*frame-limit1* (frame-limit1 11))
             (*frame-limit2* (frame-limit2))
             (*debug-mode* 4)
             (*debug-frame* (frame-down-1 (frame-up-1 *frame-limit1* *debug-mode*) *debug-mode*))
            )
        (fresh-line #|*debug-io*|#)
        (write-string (DEUTSCH "Step "
                       ENGLISH "step "
                       FRANCAIS "Step ")
                      #|*debug-io*|#
        )
        (write *step-level* #|:stream *debug-io*|#)
        (write-string " --> " #|*debug-io*|#)
        (write form #|:stream *debug-io*|# :length 4 :level 3)
        (loop
          (multiple-value-bind (what watchp)
            (catch 'stepper ; die (throw 'stepper ...) abfangen und analysieren
              (driver ; Driver-Frame aufbauen und folgende Funktion endlos ausführen:
                #'(lambda ()
                    (case
                        (catch 'debug ; die (throw 'debug ...) abfangen und analysieren
                          (same-env-as *debug-frame* ; bei *debug-frame* gültiges Environment aufbauen
                            #'(lambda ()
                                (if ; Eingabezeile verlangen
                                    (read-eval-print prompt
                                      (nconc (copy-list (commands1))
                                             (when (eval-frame-p *debug-frame*) (copy-list (commands2)))
                                             (copy-list (commands4))
                                    ) )
                                  ; T -> #<EOF>
                                  (go continue)
                                  ; NIL -> Form bereits ausgewertet und ausgegeben
                                  #|(throw 'debug 'continue)|#
                        ) )   ) )
                      (unwind (go unwind))
                      (t ) ; alles andere, insbesondere continue
            ) )   ) )
            (when watchp
              (let ((form (read-form (DEUTSCH "Abbruchbedingung: "
                                      ENGLISH "condition when to stop: "
                                      FRANCAIS "condition d'arrêt : ")
                   ))     )
                (setq *step-watch* ; Funktion, die 'form' bei *debug-frame* auswertet
                  (eval-at *debug-frame* `(function (lambda () ,form)))
            ) ) )
            (case what
              (into (go into))
              (over (go over))
              (over-this-level (go over-this-level))
              (continue (go continue))
            )
      ) ) )
      unwind
        (unwind-to-driver)
      into
        (return-from step-hook-fn
          (step-values
            (multiple-value-list (evalhook form #'step-hook-fn nil env))
        ) )
      over-this-level
        (setq *step-quit* *step-level*) ; Stepper in Schlafzustand schalten
      over
        (return-from step-hook-fn
          (step-values
            (multiple-value-list (evalhook form nil nil env))
        ) )
      continue
        (setq *step-quit* 0)
        (go over)
) ) )

;-------------------------------------------------------------------------------
;;                                  Errors

; *ERROR-HANDLER* sollte NIL oder eine Funktion sein, die übergeben bekommt:
; - NIL (bei ERROR) bzw. continue-format-string (bei CERROR),
; - error-format-string,
; - Argumente dazu,
; und die nur zurückkehren sollte, falls das erstere /=NIL ist.
(defvar *error-handler* nil)

; (CERROR continue-format-string error-format-string {arg}*), CLTL S. 430
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
      (if (interactive-stream-p *debug-io*)
        (progn
          (write-string (DEUTSCH "Wenn Sie (mit Continue) fortfahren: "
                         ENGLISH "If you continue (by typing 'continue'): "
                         FRANCAIS "Si vous continuez (en tapant «continue»): ")
                        *debug-io*
          )
          (apply #'format *debug-io* continue-format-string args)
          (funcall *break-driver* t)
        )
        (apply #'format *debug-io* continue-format-string args)
  ) ) )
  nil
)

(defvar *break-on-warnings* nil)
; (WARN format-string {arg}*), CLTL S. 432
(defun warn (format-string &rest args)
  (terpri *error-output*)
  (write-string (DEUTSCH "WARNUNG:"
                 ENGLISH "WARNING:"
                 FRANCAIS "AVERTISSEMENT :")
                *error-output*
  )
  (terpri *error-output*)
  (apply #'format *error-output* format-string args)
  (when *break-on-warnings* (funcall *break-driver* t))
  nil
)

; (BREAK [format-string {arg}*]), CLTL S. 432
(defun break (&optional (format-string "*** - Break") &rest args)
  (terpri *error-output*)
  (apply #'format *error-output* format-string args)
  (funcall *break-driver* t)
  nil
)

; (SYSTEM::BATCHMODE-ERRORS {form}*) executes the forms, but handles errors
; just as a batch program should do: continuable errors are signalled as
; warnings, non-continuable errors and Ctrl-C interrupts cause Lisp to exit.
(defmacro batchmode-errors (&body body)
  `(LET ((*ERROR-HANDLER* #'BATCHMODE-ERROR-HANDLER)
         (*BREAK-DRIVER* #'BATCHMODE-BREAK-DRIVER))
     (PROGN ,@body)
   )
)
(defun batchmode-error-handler (continue errorstring &rest args)
  (if continue
    (warn "~A~%~A" (apply #'format nil errorstring args)
                   (apply #'format nil continue args)
    )
    (progn
      (terpri *error-output*)
      (write-string "*** - " *error-output*)
      (apply #'format *error-output* errorstring args)
      (exit t) ; exit Lisp with error
) ) )
; Need to bind *break-driver* as well, because Ctrl-C interrupts are
; most often signalled directly, without passing through ERROR or SIGNAL.
(defun batchmode-break-driver (continuable &optional (condition nil) (print-it nil))
  (declare (ignore continuable condition print-it))
  (exit t)
)

;-------------------------------------------------------------------------------
;;                            Querying the user

; (Y-OR-N-P [format-string {arg}*]), CLTL S. 407
(defun y-or-n-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    (write-string (DEUTSCH " (j/n) "
                   ENGLISH " (y/n) "
                   FRANCAIS " (o/n) ")
                  *query-io*
  ) )
  (let ((localinfo (localized 'y-or-n)))
    (loop
      (let ((line (string-left-trim " " (read-line *query-io*))))
        (when (plusp (length line))
          (let ((first-char (char-upcase (char line 0))))
            (when (member first-char (car localinfo)) (return nil))
            (when (member first-char (cdr localinfo)) (return t))
      ) ) )
      (terpri *query-io*)
      (write-string (DEUTSCH "Bitte mit j oder n antworten: "
                     ENGLISH "Please answer with y or n : "
                     FRANCAIS "Répondez par o ou n : ")
                    *query-io*
) ) ) )
(definternational y-or-n (t ENGLISH))
(deflocalized y-or-n ENGLISH '((#\N) . (#\Y)))
(deflocalized y-or-n DEUTSCH '((#\N) . (#\J #\Y)))
(deflocalized y-or-n FRANCAIS '((#\N) . (#\O #\Y)))

; (YES-OR-NO-P [format-string {arg}*]), CLTL S. 408
(defun yes-or-no-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    (write-string (DEUTSCH " (ja/nein) "
                   ENGLISH " (yes/no) "
                   FRANCAIS " (oui/non) ")
                  *query-io*
  ) )
  (let ((localinfo (localized 'yes-or-no)))
    (loop
      (clear-input *query-io*)
      (let ((line (string-trim " " (read-line *query-io*))))
        (when (member line (car localinfo) :test #'string-equal) (return nil))
        (when (member line (cdr localinfo) :test #'string-equal) (return t))
      )
      (terpri *query-io*)
      (write-string (DEUTSCH "Bitte mit ja oder nein antworten: "
                     ENGLISH "Please answer with yes or no : "
                     FRANCAIS "Répondez par oui ou non : ")
                    *query-io*
) ) ) )
(definternational yes-or-no (t ENGLISH))
(deflocalized yes-or-no ENGLISH '(("no") . ("yes" "yup")))
(deflocalized yes-or-no DEUTSCH '(("nein" "nee" "nö") . ("ja")))
(deflocalized yes-or-no FRANCAIS '(("non") . ("oui")))

(defvar *keyboard-input*)
(defmacro with-keyboard (&body body)
  `(SYS::EXEC-WITH-KEYBOARD (FUNCTION (LAMBDA () (PROGN ,@body))))
)
(defun exec-with-keyboard (fun)
  #+(or DOS OS/2 WIN32) ; *keyboard-input* existiert schon
    (funcall fun)
  #+(or UNIX ACORN-RISCOS)
    (let ((mode nil))
      (unwind-protect
        (progn
          (unless *keyboard-input*
            (setq *keyboard-input* (sys::make-keyboard-stream))
          )
          (setq mode (sys::terminal-raw *terminal-io* t))
          (funcall fun)
        )
        (sys::terminal-raw *terminal-io* mode)
    ) )
  #+AMIGA
    (let ((*keyboard-input* *terminal-io*))
      (unwind-protect
        (progn (sys::terminal-raw *terminal-io* t) (funcall fun))
        (sys::terminal-raw *terminal-io* nil)
    ) )
    #| ;; redefined after SCREEN is loaded:
    (let ((*keyboard-input* (screen::make-generic-stream ...)))
      (unwind-protect
        (funcall fun)
        (close *keyboard-input*)
    ) )
    |#
)

