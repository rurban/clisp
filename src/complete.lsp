;;;; Command-line completion hook

(in-package "SYSTEM")

;;-----------------------------------------------------------------------------

; Vervollständigungs-Routine in Verbindung mit der GNU Readline-Library:
; Input: string die Eingabezeile, (subseq string start end) das zu vervoll-
; ständigende Textstück.
; Output: eine Liste von Simple-Strings. Leer, falls keine sinnvolle Vervoll-
; ständigung. Sonst CDR = Liste aller sinnvollen Vervollständigungen, CAR =
; sofortige Ersetzung.
#+(or UNIX OS/2)
(defun completion (string start end)
  ; quotiert vervollständigen?
  (let ((start1 start) (quoted nil))
    (when (and (>= start 1) (member (char string (- start 1)) '(#\" #\|)))
      (decf start1) (setq quoted t)
    )
    (let (; Hilfsvariablen beim Sammeln der Symbole:
          knownpart ; Anfangsstück
          knownlen  ; dessen Länge
          (L '())   ; sammelnde Liste
         )
      (let* ((functionalp1
               (and (>= start1 1)
                    (equal (subseq string (- start1 1) start1) "(")
             ) )
             (functionalp2
               (and (>= start1 2)
                    (equal (subseq string (- start1 2) start1) "#'")
             ) )
             (functionalp ; Vervollständigung in funktionaler Position?
               (or functionalp1 functionalp2)
             )
             (gatherer
               (if functionalp
                 #'(lambda (sym)
                     (when (fboundp sym)
                       (let ((name (symbol-name sym)))
                         (when (and (>= (length name) knownlen) (string-equal name knownpart :end1 knownlen))
                           (push name L)
                   ) ) ) )
                 #'(lambda (sym)
                     (let ((name (symbol-name sym)))
                       (when (and (>= (length name) knownlen) (string-equal name knownpart :end1 knownlen))
                         (push name L)
                   ) ) )
             ) )
             (package *package*)
             (mapfun #'sys::map-symbols)
             (prefix nil))
        ; Evtl. Packagenamen abspalten:
        (unless quoted
          (let ((colon (position #\: string :start start :end end)))
            (when colon
              (unless (setq package (find-package (string-upcase (subseq string start colon))))
                (return-from completion nil)
              )
              (incf colon)
              (if (and (< colon end) (eql (char string colon) #\:))
                (incf colon)
                (setq mapfun #'sys::map-external-symbols)
              )
              (setq prefix (subseq string start colon))
              (setq start colon)
        ) ) )
        (setq knownpart (subseq string start end))
        (setq knownlen (length knownpart))
        (funcall mapfun gatherer package)
        (when (null L) (return-from completion nil))
        ; Bei einer Funktion ohne Argumente ergänze die schließende Klammer:
        (when (and functionalp1
                   (null (cdr L))
                   (let ((sym (find-symbol (car L) package)))
                     (and #| sym |#
                          (fboundp sym)
                          (functionp (symbol-function sym))
                          (multiple-value-bind (req-anz opt-anz rest-p key-p)
                              (function-signature (symbol-function sym))
                            (and (eql req-anz 0) (eql opt-anz 0) (not rest-p) (not key-p))
              )    ) )    )
          (setf (car L) (string-concat (car L) ")"))
        )
        ; Kleinbuchstaben:
        (unless quoted
          (setq L (mapcar #'string-downcase L))
        )
        ; sortieren:
        (setq L (sort L #'string<))
        ; größtes gemeinsames Anfangsstück suchen:
        (let ((imax ; (reduce #'min (mapcar #'length L))
                (let ((i (length (first L))))
                  (dolist (s (rest L)) (setq i (min i (length s))))
                  i
             )) )
          (do ((i 0 (1+ i)))
              ((or (eql i imax)
                   (let ((c (char (first L) i)))
                     (dolist (s (rest L) nil) (unless (eql (char s i) c) (return t)))
               )   )
               (push (subseq (first L) 0 i) L)
        ) )   )
        ; Präfix wieder ankleben:
        (when prefix
          (mapl #'(lambda (l)
                    (setf (car l) (string-concat prefix (car l)))
                  )
                L
        ) )
        L
) ) ) )

