;;;; Debugger, Stepper, Errors

(in-package "LISP")
(export '(*prompt*))
(in-package "SYSTEM")

;;;--------------------------------------------------------------------------
;;;                                 Debugger

(defvar *break-count* 0) ; Anzahl der aktiven Break-Schleifen (Fixnum >=0)

; Zähler zum Vermeiden von Endlosrekursionen wegen *error-output*
(defvar *recurse-count-error-output* 0)
; Zähler zum Vermeiden von Endlosrekursionen wegen *debug-io*
(defvar *recurse-count-debug-io* 0)

; Hauptschleife:
; (driver
;   #'(lambda () (read-eval-print "> "))
; )

(defun package-short-name (pkg)
  "Return the shortest (nick)name of the package."
  (declare (type package pkg))
  (let ((name (reduce (lambda (st0 st1)
                        (declare (simple-string st0 st1))
                        (if (> (length st0) (length st1)) st1 st0))
                      (package-nicknames pkg) :initial-value
                      (package-name pkg))))
    (case *print-case*
      (:upcase (string-upcase name))
      (:downcase (string-downcase name))
      (:capitalize (string-capitalize name))
      (t name))))

(defvar *command-index* 0 "The number of commands received so far.")
(defvar *home-package* nil "The starting package of this session.")
(defun prompt-new-package ()
  "Return the current package or NIL if it never changed."
  (unless *home-package* (setq *home-package* *package*))
  (unless (eq *home-package* *package*) *package*))

(defvar *prompt*
  #'(lambda ()
      ;; prompt with *package* when it is different from the initial one
      ;; or when it doesn't contain standard LISP symbols, like T.
      (if (and (packagep *package*) (package-name *package*))
          (format nil "~@[~a~][~:d]"
                  (if (or (not (find-symbol "T" *package*))
                          (prompt-new-package))
                      (package-short-name *package*))
                  (incf *command-index*))
          (ENGLISH "[*package* invalid]")))
  "The top level prompt.  If a function, the return value is used.
If anything else, printed.")

; Vom Prompt der erste Teil:
(defun prompt-string1 () "")
; Vom Prompt der zweite Teil:
(defun prompt-string2 ()
  (princ-to-string (if (functionp *prompt*)
                       (lisp::ignore-errors (funcall *prompt*))
                       *prompt*)))
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
    (read-form (ENGLISH "Values: "))
  )
  (throw 'debug 'continue)
)
(defun debug-continue () (throw 'debug 'quit))

(defun commands0 ()
             (list
               (ENGLISH "
Help = this list
Use the usual editing capabilities.
(quit) or (exit) leaves CLISP."
               )
               (cons "Help"   #'debug-help  )
)            )
(defun commands1 ()
             (list
               (ENGLISH "
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
               (ENGLISH "
Continue = continue evaluation"
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
          (write-string (ENGLISH "Unprintable error message")
                        *error-output*
        ) )
        (sys::print-condition condition *error-output*)
    ) )
    (symbol-stream '*debug-io* :io)
    (when may-continue
      (if continuable
        (when interactive-p
          (terpri *debug-io*)
          (write-string (ENGLISH "You can continue (by typing 'continue').")
                        *debug-io*
          )
        )
        (progn
          (terpri *debug-io*)
          (when interactive-p
            (write-string (ENGLISH "If you continue (by typing 'continue'): ")
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
                          (ENGLISH "The following restarts are available too:")
                          (ENGLISH "The following restarts are available:")
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
                      (write-string ". Break " s)
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

;;;--------------------------------------------------------------------------
;;;        komfortabler Stepper. (Läuft nur in compiliertem Zustand!)

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
               (ENGLISH "
Step     = step into form: evaluate this form in single step mode
Next     = step over form: evaluate this form at once
Over     = step over this level: evaluate at once up to the next return
Continue = switch off single step mode, continue evaluation
Step-until, Next-until, Over-until, Continue-until:
           same as above, specify a condition when to stop"
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
    (write-string (ENGLISH "step ") #|*debug-io*|#)
    (write *step-level* #|:stream *debug-io*|#)
    (write-string " ==> " #|*debug-io*|#)
    (case (length values)
      (0 (write-string (ENGLISH "no values") #|*debug-io*|#))
      (1 (write-string (ENGLISH "value: ") #|*debug-io*|#)
         (write (car values) #|:stream *debug-io*|#)
      )
      (t (write (length values) #|:stream *debug-io*|#)
         (write-string (ENGLISH " values: ") #|*debug-io*|#)
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
                       (write-char #\Space s)
                       (write-string (prompt-string2) s)
                       (write-string (prompt-string3) s)
             )       )
             (*frame-limit1* (frame-limit1 11))
             (*frame-limit2* (frame-limit2))
             (*debug-mode* 4)
             (*debug-frame* (frame-down-1 (frame-up-1 *frame-limit1* *debug-mode*) *debug-mode*))
            )
        (fresh-line #|*debug-io*|#)
        (write-string (ENGLISH "step ") #|*debug-io*|#)
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
              (let ((form (read-form (ENGLISH "condition when to stop: "))))
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
