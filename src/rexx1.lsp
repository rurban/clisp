;;;; Rexx Funktionen für CLISP
;;;; Jörg Höhle 15.4.1997

(in-package "LISP")
(export '(rexx-run-command rexx-send-command rexx-wait-sent-command rexx-do-command
          rexx-loop *rexx-ignore-errors*
)        )

;;;; Interface:
;;;
;;; (rexx-loop)
;;;
;;; (rexx-run-command name) -> null
;;;
;;; (rexx-do-command name) -> (rc result)
;;;
;;; (rexx-send-command name) -> handle
;;; (rexx-wait-sent-command handle) -> (rc result)
;;;
;;; name kann ein String (Kommandos in einem String)
;;;      oder ein Pathname (File mit Kommandos) sein.
;;; rc ist der ARexx return Code.
;;; result ist der ARexx return String, nur wenn rc gleich 0.

;;;; ===========================================================================
;;;; Implementation:

(in-package "SYSTEM")

;;; Wir benutzen folgende Funktionen aus REXX.D:
;;; (system::%rexx-wait-input) -> boolean
;;; (system::%rexx-get) -> (handle string) für eine neue Nachricht oder
;;;  (handle rc [result]) für ein Reply auf eine unserer Nachrichten
;;; (system::%rexx-reply handle rc result) -> null
;;; (system::%rexx-put name :result :string :token :host :io) -> handle
;;; Keyword-Argumente result, string, token, io sind Flags:
;;;  result: Antwort merken
;;;  string: Argument als Befehle statt 1. Token als Dateiname verstehen
;;;  token: Tokens erzeugen
;;;  io: E/A Kanäle übernehmen
;;; host: ARexx Portname bzw. NIL für "REXX" oder T für "AREXX" (asynchrone
;;; Bearbeitung)

;; Wir verwalten eine Aliste  msg -> reply  aller weggeschickten und noch
;; unbearbeiteten Messages und ihrer Antworten (Listen (Code String);
;; NIL für noch unbeantwortete Messages). Beim Abschicken einer Message
;; bekommen wir ein "handle" (FOREIGN-POINTER) als Erkennungszeichen
;; (diese werden mit EQUALP verglichen).

(defvar *rexx-outmsg-list* '())

(defun rexx-add-index (handle &optional (value nil))
  (push (cons handle value) *rexx-outmsg-list*)
)
(defun rexx-find-index (handle)
  (assoc handle *rexx-outmsg-list* :test #'equalp)
)
(defun rexx-delete-entry (acons)
  (setq *rexx-outmsg-list* (delete acons *rexx-outmsg-list* :test #'eq))
)

;; Startet ein REXX-Kommando, ohne jedoch jetzt auf dessen Beendigung zu warten.
;; Liefert das Handle, damit man später noch auf seine Beendigung warten kann,
;; jedoch NIL, falls das Kommando nicht erfolgreich abgeschickt werden konnte.
(defun rexx-send-command (name &rest keys &key result string token host io)
  (declare (ignore result string token host io))
  "Starts execution of a rexx command asynchronously."
  (let ((handle (apply #'%rexx-put name keys)))
    (when handle
      (rexx-add-index handle)
      handle
) ) )

;; Wartet auf die nächste Nachricht und liefert ihr Handle.
(defun rexx-next-event ()
  (loop ; es fehlt derzeit die Möglichkeit, parallel *STANDARD-INPUT* zu lesen
    ; nächste Nachricht lesen und auswerten, falls vorhanden:
    (let ((event (%rexx-get)))
      (when event (return event))
    )
    ; auf die nächste Nachricht warten:
    (%rexx-wait-input)
) )


(defvar *rexx-ignore-errors* t
  "If T silently ignore errors, if NIL invoke normal *error-handler*,
  otherwise must be a function that is bound to *error-handler*")


;; "Hauptschleife": Wartet auf Nachrichten, interpretiert diese als Fragen,
;; wertet sie aus und schickt die Antwort zurück (oder Return-Code 5 im Falle
;; eines Fehlers). Die Schleife wird beendet, wenn eine Antwort auf Handle
;; wait-for kommt.
;; Wir möchten: dass rexx-loop in eine Endlosschleife geht und möglicherweise
;; in den Debugger springt. unwind/abort soll dabei zurück in die Schleife
;; springen (oder doch rexx-loop verlassen? (dann loop statt driver
;; benutzen)), damit falsche ARexx-Eingaben nicht zum Abbruch von rexx-loop
;; führen.
;; Ferner soll rexx-loop auch ganz normal beendet werden, z.Z. springt es
;; über rexx:exit-loop.cl in eine Eingabeschleife, deswegen geht (progn
;; (rexx-loop) (print "Over")) nicht.
(defun rexx-loop (&optional wait-for)
  "Rexx driver loop. Optional message to wait for."
  (driver ; driver oder einfaches loop ??
    #'(lambda ()
        (let ((event (rexx-next-event))) ; nächste Nachricht
          (cond ((numberp (second event)) ; ein Reply (handle rc [result])
                 (let ((index (rexx-find-index (first event))))
                   (when index (setf (cdr index) (rest event))) ; Antwort abspeichern
                 )
                 (when (equalp (first event) wait-for)
                   (return-from rexx-loop (rest event)) ; evtl. Schleife beenden
                ))
                (t ; ein Befehl (handle string)
                 (let ((result nil))
                   ; warum funktioniert (catch 'debug ...) nicht??
                   (unwind-protect
                     (block try-rep ; Fehlerbehandlung
                       (setq result
                         (with-output-to-string (stream)
                           (let ((*error-handler*
                                  (cond ((functionp *rexx-ignore-errors*)
                                         *rexx-ignore-errors*)
                                        ((not *rexx-ignore-errors*)
                                         *error-handler*)
                                        (t #'(lambda (&rest error-args)
                                               (declare (ignore error-args))
                                               (return-from try-rep nil)))
                                )))
                             ; primitives READ-EVAL-PRINT :
                             (princ (eval (read-from-string (second event)))
                                    stream
                     ) ) ) ) )
                     (%rexx-reply (first event) (if result 0 5) result) ; portabler machen!??
                )) )
) )   ) ) )

;; Wartet auf die Beendigung eines REXX-Kommandos.
;; Liefert die Antwort (eine Liste (Code [String])).
(defun rexx-wait-sent-command (handle)
  "Waits for command termination."
  (let ((index (rexx-find-index handle)))
    (unless index
      (error-of-type 'error
        (ENGLISH "No waiting for ~S possible.")
         handle
    ) )
    (loop
      (when (cdr index) (rexx-delete-entry index) (return (cdr index)))
      (rexx-loop handle) ; auf die Antwort warten, Aussprung oben
) ) )

;; Startet ein REXX-Kommando und wartet, bis es beendet ist.
;; Liefert die Antwort (eine Liste (Code String)),
;; jedoch NIL, falls das Kommando nicht erfolgreich abgeschickt werden konnte.
(defun rexx-do-command (name &rest keys &key &allow-other-keys)
  "Executes command, waiting for result."
  (let ((handle (apply #'rexx-send-command name keys)))
    (when handle
      (rexx-wait-sent-command handle)
) ) )

;; Startet ein REXX-Kommando, ohne jedoch auf dessen Beendigung zu warten
;; (asynchron).
;; Liefert /=NIL, falls das Kommando erfolgreich abgeschickt wurde.
(defun rexx-run-command (name &key string token)
  "Runs a rexx command asynchronously, no return code."
  (if (rexx-do-command name :string string :token token :host t) t nil)
)

