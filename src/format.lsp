; FORMAT - und was dazugehört.
; Bruno Haible 22.06.1988
; CLISP-Version 16.08.1988, 03.09.1988, 04.08.1989
; Groß umgearbeitet von Bruno Haible am 14.02.1990-15.02.1990
; Weiter umgearbeitet und FORMATTER geschrieben am 9.4.1995-11.4.1995

; FORMAT is a mechanism for producing string output conveniently by, basically,
; taking a pre-determined string with placeholders and substituting computed
; values or strings for those placeholders -- though it became much more
; complex than this because the placeholders included iteration primitives for
; producing lists of results, plurals, and other such exotica. It may be
; loosely characterized as FORTRAN FORMAT statements gone berserk.
; -- Guy L. Steele Jr. and Richard P. Gabriel in "The Evolution of Lisp"

(in-package "SYSTEM")

;-------------------------------------------------------------------------------

; Datenstruktur der Kontrollstring-Direktive:
(defstruct (control-string-directive
             (:copier nil)
             (:conc-name "CSD-")
             (:predicate nil)
             (:constructor make-csd ())
           )
  (type         0 :type fixnum)
  (cs-index     0 :type fixnum)
  (parm-list    nil :type list)
  (v-or-#-p     nil :type symbol)
  (colon-p      nil :type symbol)
  (atsign-p     nil :type symbol)
  (data         nil)
  (clause-chain nil)
)
#+CLISP (remprop 'control-string-directive 'sys::defstruct-description)
; Erläuterung:
; type=0 : Direktive ~<Newline>, nichts auszugeben.
;          Weitere Komponenten bedeutungslos
; type=1 : String auszugeben,
;          von *FORMAT-CS* die Portion :START cs-index :END data.
;          Weitere Komponenten bedeutungslos
; type=2 : Formatier-Direktive auszuführen.
;          data = Name der Direktive (Symbol),
;          colon-p gibt an, ob ein ':' da war,
;          atsign-p gibt an, ob ein '@' da war,
;          parm-list = Parameterliste an die Direktive,
;          v-or-#-p gibt an, ob parm-list vor dem Aufruf noch zu behandeln ist.
;          clause-chain ist eine Verzeigerung: z.B. bei ~[...~;...~;...~]
;          von der ~[-Direktive auf die Liste ab der ersten ~;-Direktive,
;          von da auf die Liste ab der nächsten ~;-Direktive usw.
;          bis schließlich auf die Liste ab der ~]-Direktive.

; Zeigt an, ob ein Character ein Whitespace-Character ist.
(defun whitespacep (char)
  (member char '(#\Space #\Newline #\Linefeed #\Tab #\Return #\Page))
)

; (FORMAT-PARSE-CS control-string startindex csdl stop-at)
; parst einen Kontrollstring (genauer: (subseq control-string startindex))
; und legt die sich ergebende Control-String-Directive-Liste in (cdr csdl) ab.
; Das Parsen muss mit der Direktive stop-at enden (ein Character, oder NIL
; für Stringende).
; Falls stop-at /= NIL, ist in (csd-clause-chain (car csdl)) ein Pointer auf
; die Teilliste ab dem nächsten Separator einzutragen. Diese Pointer bilden
; eine einfach verkettete Liste innerhalb csdl: von einem Separator zum
; nächsten, zum Schluss zum Ende der Clause.
(defun format-parse-cs (control-string startindex csdl stop-at)
  (declare (fixnum startindex))
  (macrolet ((errorstring ()
               (ENGLISH "The control string terminates within a directive.")
            ))
    (prog* ((index startindex) ; cs-index des nächsten Zeichens
            ch ; current character
            intparam ; Integer-Parameter
            newcsd ; aktuelle CSD
            (last-separator-csd (car csdl))
           )
      (declare (type simple-string control-string) (type fixnum index))
      (loop ; neue Direktive insgesamt
        (tagbody
          (when (>= index (length control-string))
            (go string-ended)
          )
          (setq ch (schar control-string index))
          (unless (eql ch #\~)
            ; eventuell noch Stringstück zu einer eingenen Direktive machen
            (setq csdl (setf (cdr csdl) (list (setq newcsd (make-csd)))))
            (setf (csd-type     newcsd) 1)
            (setf (csd-cs-index newcsd) index)
            (setq index (position #\~ control-string :start index))
            (unless index
              (setf (csd-data newcsd) (setq index (length control-string)))
              (go string-ended)
            )
            (setf (csd-data newcsd) index)
          )
          (setq csdl (setf (cdr csdl) (list (setq newcsd (make-csd)))))
          (setf (csd-type         newcsd) 2)
          (setf (csd-cs-index     newcsd) index)
          (setf (csd-parm-list    newcsd) nil)
          (setf (csd-v-or-#-p     newcsd) nil)
          (setf (csd-colon-p      newcsd) nil)
          (setf (csd-atsign-p     newcsd) nil)
          (setf (csd-data         newcsd) nil)
          (setf (csd-clause-chain newcsd) nil)

          param ; Parameter einer Direktive kann beginnen
          (incf index)
          (when (>= index (length control-string))
            (format-error control-string index (errorstring))
            (go string-ended)
          )
          (setq ch (schar control-string index))
          (when (digit-char-p ch) (go num-param))
          (case ch
            ((#\+ #\-) (go num-param))
            (#\' (go quote-param))
            ((#\V #\v #\#)
             (push (if (eql ch #\#) ':ARG-COUNT ':NEXT-ARG)
                   (csd-parm-list newcsd)
             )
             (setf (csd-v-or-#-p newcsd) T)
             (go param-ok-1)
            )
            (#\, (push nil (csd-parm-list newcsd)) (go param))
            (#\: (go colon-modifier))
            (#\@ (go atsign-modifier))
            (T (go directive))
          )

          num-param ; numerischer Parameter
          (multiple-value-setq (intparam index)
            (parse-integer control-string :start index :junk-allowed t)
          )
          (unless intparam
            (format-error control-string index
                          (ENGLISH "~A must introduce a number.")
                          ch
          ) )
          (push intparam (csd-parm-list newcsd))
          (go param-ok-2)

          quote-param ; Quote-Parameter-Behandlung
          (incf index)
          (when (>= index (length control-string))
            (format-error control-string index
              (ENGLISH "The control string terminates in the middle of a parameter.")
            )
            (go string-ended)
          )
          (setq ch (schar control-string index))
          (push ch (csd-parm-list newcsd))

          param-ok-1 ; Parameter OK
          (incf index)
          param-ok-2 ; Parameter OK
          (when (>= index (length control-string))
            (format-error control-string index (errorstring))
            (go string-ended)
          )
          (setq ch (schar control-string index))
          (case ch
            (#\, (go param))
            (#\: (go colon-modifier))
            (#\@ (go atsign-modifier))
            (T (go directive))
          )

          colon-modifier ; nach :
          (setf (csd-colon-p newcsd) T)
          (go passed-modifier)

          atsign-modifier ; nach @
          (setf (csd-atsign-p newcsd) T)
          (go passed-modifier)

          passed-modifier ; nach : oder @
          (incf index)
          (when (>= index (length control-string))
            (format-error control-string index (errorstring))
            (go string-ended)
          )
          (setq ch (schar control-string index))
          (case ch
            (#\: (go colon-modifier))
            (#\@ (go atsign-modifier))
            (T (go directive))
          )

          directive ; Direktive (ihr Name) erreicht
          (setf (csd-parm-list newcsd) (nreverse (csd-parm-list newcsd)))
          (let ((directive-name
                  (cdr (assoc (char-upcase ch)
                         '((#\A . FORMAT-ASCII)
                           (#\S . FORMAT-S-EXPRESSION)
                           (#\W . FORMAT-WRITE)
                           (#\D . FORMAT-DECIMAL)
                           (#\B . FORMAT-BINARY)
                           (#\O . FORMAT-OCTAL)
                           (#\X . FORMAT-HEXADECIMAL)
                           (#\R . FORMAT-RADIX)
                           (#\P . FORMAT-PLURAL)
                           (#\C . FORMAT-CHARACTER)
                           (#\F . FORMAT-FIXED-FLOAT)
                           (#\E . FORMAT-EXPONENTIAL-FLOAT)
                           (#\G . FORMAT-GENERAL-FLOAT)
                           (#\$ . FORMAT-DOLLARS-FLOAT)
                           (#\% . FORMAT-TERPRI)
                           (#\& . FORMAT-FRESH-LINE)      (#\Newline . #\Newline)
                           (#\| . FORMAT-PAGE)
                           (#\~ . FORMAT-TILDE)
                           (#\T . FORMAT-TABULATE)
                           (#\* . FORMAT-GOTO)
                           (#\? . FORMAT-INDIRECTION)
                           (#\/ . FORMAT-CALL-USER-FUNCTION)
                           (#\( . FORMAT-CASE-CONVERSION) (#\) . FORMAT-CASE-CONVERSION-END)
                           (#\[ . FORMAT-CONDITIONAL)     (#\] . FORMAT-CONDITIONAL-END)
                           (#\{ . FORMAT-ITERATION)       (#\} . FORMAT-ITERATION-END)
                           (#\< . FORMAT-JUSTIFICATION)   (#\> . FORMAT-JUSTIFICATION-END)
                           (#\^ . FORMAT-UP-AND-OUT)      (#\; . FORMAT-SEPARATOR)
                           (#\! . FORMAT-CALL)
                           ; mit Funktionsdefinition      ; ohne Funktionsdefinition
               )) )    )  )
            (if directive-name
              (setf (csd-data newcsd) directive-name)
              (format-error control-string index
                (ENGLISH "Non-existent directive")
          ) ) )
          (incf index)
          (case ch
            (#\/
             (let* ((start index)
                    (end (or (position #\/ control-string :start start)
                             (format-error control-string index
                               (ENGLISH "Closing '/' is missing")
                    )    )   )
                    (pos (position #\: control-string :start start :end end))
                    (name (string-upcase
                            (subseq control-string
                                    (if pos
                                      (if (char= #\: (char control-string (1+ pos))) (+ 2 pos) (1+ pos))
                                      start )
                                    end )))
                    (pack (if pos
                            (let ((packname (string-upcase (subseq control-string start pos))))
                              (or (find-package packname)
                                  (format-error control-string index
                                    (ENGLISH "There is no package with name ~S")
                                    packname )))
                            *common-lisp-user-package* )))
               (push (list (intern name pack)) (csd-parm-list newcsd))
               (setq index (1+ end))
            ))
            (( #\( #\[ #\{ #\< )
             (multiple-value-setq (index csdl)
               (format-parse-cs control-string index csdl
                 (case ch (#\( #\)) (#\[ #\]) (#\{ #\}) (#\< #\>) )
             ) )
            )
            (( #\) #\] #\} #\> )
             (unless stop-at
               (format-error control-string index
                 (ENGLISH "The closing directive '~A' does not have a corresponding opening one.")
                 ch
             ) )
             (unless (eql ch stop-at)
               (format-error control-string index
                 (ENGLISH "The closing directive '~A' does not match the corresponding opening one. It should read '~A'.")
                 ch stop-at
             ) )
             (setf (csd-clause-chain last-separator-csd) csdl)
             (go end)
            )
            (#\;
             (unless (or (eql stop-at #\]) (eql stop-at #\>))
               (format-error control-string index
                 (ENGLISH "The ~~; directive is not allowed at this point.")
             ) )
             (setf (csd-clause-chain last-separator-csd) csdl)
             (setq last-separator-csd newcsd)
            )
            (#\Newline
             (setf (csd-type newcsd) 0)
             (if (csd-colon-p newcsd)
               (if (csd-atsign-p newcsd)
                 (format-error control-string index
                   (ENGLISH "The ~~newline directive cannot take both modifiers.")
                 )
                 nil ; ~:<newline> -> Newline ignorieren, Whitespace dalassen
               )
               (progn
                 (when (csd-atsign-p newcsd)
                   ; ~@<newline> -> Stringstück mit Newline zum Ausgeben
                   (setf (csd-type newcsd) 1)
                   (setf (csd-cs-index newcsd) (1- index))
                   (setf (csd-data newcsd) index)
                 )
                 (setq index
                   (or (position-if-not #'whitespacep control-string :start index)
                       (length control-string)
          ) )) ) ) )
        ) ; tagbody zu Ende
      ) ; loop zu Ende

      string-ended
      (when stop-at
        (format-error control-string index
          (ENGLISH "An opening directive is never closed; expecting '~A'.")
          stop-at
      ) )

      end
      (return (values index csdl))
) ) )

;-------------------------------------------------------------------------------

(defvar *FORMAT-CS*) ; control-string
(defvar *FORMAT-CSDL*) ; control-string directive list
(defvar *FORMAT-ARG-LIST*) ; argument-list
(defvar *FORMAT-NEXT-ARG*) ; pointer to next argument in argument-list
(defvar *FORMAT-NEXT-ARGLIST*) ; pointer to next sublist in ~:{ iteration
(defvar *FORMAT-UP-AND-OUT* nil) ; reason for up-and-out

; (format-error controlstring errorpos errorcode . arguments)
; signalisiert einen Error, der bei FORMAT aufgetreten ist. Die Stelle im
; Control-string wird mit einem Pfeil markiert.
(defun format-error (controlstring errorpos errorstring &rest arguments)
  (when controlstring
    (unless errorpos (setq errorpos (csd-cs-index (car *FORMAT-CSDL*))))
    (setq errorstring
      (string-concat errorstring
        (ENGLISH "~%Current point in control string:")
    ) )
    (let ((pos1 0) (pos2 0))
      (declare (simple-string errorstring) (fixnum pos1 pos2))
      (loop
        (setq pos2 (or (position #\Newline controlstring :start pos1)
                       (length controlstring)
        )          )
        (setq errorstring (string-concat errorstring "~%  ~A"))
        (setq arguments
          (nconc arguments (list (substring controlstring pos1 pos2)))
        )
        (when (<= pos1 errorpos pos2)
          (setq errorstring
            (string-concat errorstring "~%~VT"
                           #+(or DOS OS/2) "" #-(or DOS OS/2) "|"
          ) )
          (setq arguments (nconc arguments (list (+ (- errorpos pos1) 2))))
        )
        (when (= pos2 (length controlstring)) (return))
        (setq pos1 (+ pos2 1))
  ) ) )
  (apply #'error-of-type 'error errorstring arguments)
)

;-------------------------------------------------------------------------------

(defun format (destination control-string &rest arguments)
  (unless (or (stringp control-string) (functionp control-string))
    (format-cs-error control-string)
  )
  (cond ((null destination)
         (let ((stream (make-string-output-stream)))
           (format-apply stream control-string arguments)
           (get-output-stream-string stream)
        ))
        ((eq destination 'T)
         (format-apply *standard-output* control-string arguments)
         nil
        )
        ((streamp destination)
         (format-apply destination control-string arguments)
         nil
        )
        ((stringp destination)
         (if (array-has-fill-pointer-p destination)
           (let ((stream (sys::make-string-push-stream destination)))
             (format-apply stream control-string arguments)
           )
           (error-of-type 'error
             (ENGLISH "The destination string ~S should have a fill pointer.")
             destination
         ) )
         nil
        )
        (t (error-of-type 'type-error
             :datum destination :expected-type '(or boolean stream string)
             (ENGLISH "The destination argument ~S is invalid (not NIL or T or a stream or a string).")
             destination
        )  )
) )

(defun format-apply (stream control-string arguments &optional (whole-arguments arguments))
  (cond ((stringp control-string)
         ; evtl. noch control-string zu einem Simple-String machen ??
         (let ((node (list control-string)))
           (format-parse-cs control-string 0 node nil)
           (let* ((*FORMAT-CS*         (car node))
                  (*FORMAT-CSDL*       (cdr node))
                  (*FORMAT-ARG-LIST*   whole-arguments)
                  (*FORMAT-NEXT-ARG*   arguments)
                  (*FORMAT-NEXT-ARGLIST* nil)
                  (*FORMAT-UP-AND-OUT* nil))
             (format-interpret stream)
             *FORMAT-NEXT-ARG*
        )) )
        ((functionp control-string)
         (let ((*FORMAT-CS* nil)) ; format-error kann nicht mehr auf die Stelle zeigen
           (apply control-string stream arguments)
        ))
        (t (format-cs-error control-string))
) )

(defun format-cs-error (control-string)
  (error-of-type 'type-error
    :datum control-string :expected-type '(or string function)
    (ENGLISH "~S: The control-string must be a string, not ~S")
    'format control-string
) )

;-------------------------------------------------------------------------------

; (next-arg) liefert (und verbraucht) das nächste Argument aus der Argument-
; liste *FORMAT-NEXT-ARG*.
(defun next-arg ()
  (if (atom *FORMAT-NEXT-ARG*)
    (format-error *FORMAT-CS* nil
      (ENGLISH "There are not enough arguments left for this directive.")
    )
    (pop *FORMAT-NEXT-ARG*)
) )

; (format-interpret stream [endmarker]) interpretiert *FORMAT-CSDL* ab.
; Fluid vars:
;   *FORMAT-ARG-LIST*
;   *FORMAT-NEXT-ARG*
;   *FORMAT-NEXT-ARGLIST*
;   *FORMAT-CS*
;   *FORMAT-CSDL*
;   *FORMAT-UP-AND-OUT*
; Abbruch des Interpretierens bei Antreffen der Direktive endmarker
; oder der Direktive ~; .
(defun format-interpret (stream &optional (endmarker nil))
  (loop
    (when *FORMAT-UP-AND-OUT* (return))
    (when (endp *FORMAT-CSDL*) (return))
    (let ((csd (car *FORMAT-CSDL*)))
      (case (csd-type csd)
        (0 )
        (1 (write-string *FORMAT-CS* stream
             :start (csd-cs-index csd) :end (csd-data csd)
        )  )
        (2 (let ((directive-name (csd-data csd)))
             (if (eq directive-name endmarker) (return))
             (if (eq directive-name 'FORMAT-SEPARATOR) (return))
             (apply directive-name
               stream
               (csd-colon-p csd)
               (csd-atsign-p csd)
               (format-resolve-parms csd)
        )  ) )
    ) )
    (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
) )

; liefert die korrekte Argumentliste einer CSD, evtl. mit eingesetzten
; Parametern: V (als :NEXT-ARG) und # (als :ARG-COUNT) werden aufgelöst.
(defun format-resolve-parms (csd)
  (let ((arglist (csd-parm-list csd)))
    (if (csd-v-or-#-p csd)
      (mapcar #'(lambda (arg)
                  (case arg
                    (:NEXT-ARG (next-arg))
                    (:ARG-COUNT (list-length *FORMAT-NEXT-ARG*))
                    (T arg)
                ) )
              arglist
      )
      arglist
) ) )

; Definiert eine einfache FORMAT-Unterfunktion, d.i. eine, die genau ein
; Argument verbraucht.
(defmacro defformat-simple (name (stream colon atsign . optionals-with-defaults)
                                 (arg) &body body
                            &environment env)
  (multiple-value-bind (body-rest declarations) (sys::parse-body body nil env)
    (let ((name2 (concat-pnames "DO-" name)) ; in #<PACKAGE SYSTEM>
          (optionals (mapcar #'(lambda (opt) (if (consp opt) (first opt) opt))
                             optionals-with-defaults
         ))          )
      `(PROGN
         (DEFUN ,name (,stream ,colon ,atsign &OPTIONAL ,@optionals)
           (,name2 ,stream ,colon ,atsign ,@optionals (next-arg))
         )
         (DEFUN ,name2 (,stream ,colon ,atsign ,@optionals ,arg)
           ,@(if declarations `((DECLARE ,@declarations)))
           ,@(mapcap #'(lambda (opt)
                         (if (and (consp opt) (not (null (second opt))))
                           `((IF (NULL ,(first opt)) (SETQ ,(first opt) ,(second opt))))
                           '()
                       ) )
                     optionals-with-defaults
             )
           ,@body-rest
       ) )
) ) )

; Bewegt den Stand des "Pointers in die Argumentliste" in eine Richtung.
(defun format-goto-new-arg (backwardp index)
  (if backwardp
    ; rückwärts
    (setq *FORMAT-NEXT-ARG*
      (nthcdr
        (max (- (list-length *FORMAT-ARG-LIST*) (list-length *FORMAT-NEXT-ARG*) index) 0)
        *FORMAT-ARG-LIST*
    ) )
    ; vorwärts ist einfacher:
    (setq *FORMAT-NEXT-ARG* (nthcdr index *FORMAT-NEXT-ARG*))
) )

; gibt arg als römische Zahl auf stream aus, z.B. 4 als IIII.
(defun format-old-roman (arg stream)
  (unless (and (integerp arg) (<= 1 arg 4999))
    (format-error *FORMAT-CS* nil
      (ENGLISH "The ~~:@R directive requires an integer in the range 1 - 4999, not ~S")
      arg
  ) )
  (do ((charlistr  '(#\M  #\D #\C #\L #\X #\V #\I) (cdr charlistr))
       (valuelistr '(1000 500 100 50  10   5   1) (cdr valuelistr))
       (value arg (multiple-value-bind (multiplicity restvalue)
                      (floor value (first valuelistr))
                    (dotimes (i multiplicity)
                      (write-char (first charlistr) stream)
                    )
                    restvalue
      ))          )
      ((zerop value))
) )

; gibt arg als römische Zahl auf stream aus, z.B. 4 als IV.
(defun format-new-roman (arg stream)
  (unless (and (integerp arg) (<= 1 arg 3999))
    (format-error *FORMAT-CS* nil
      (ENGLISH "The ~~@R directive requires an integer in the range 1 - 3999, not ~S")
      arg
  ) )
  (do ((charlistr       '(#\M #\D #\C #\L #\X #\V #\I) (cdr charlistr))
       (valuelistr     '(1000 500 100 50  10   5   1 ) (cdr valuelistr))
       (lowercharlistr  '(#\C #\C #\X #\X #\I #\I    ) (cdr lowercharlistr))
       (lowervaluelistr '(100 100 10  10   1   1   0 ) (cdr lowervaluelistr))
       (value arg
         (multiple-value-bind (multiplicity restvalue)
             (floor value (first valuelistr))
           (dotimes (i multiplicity) (write-char (first charlistr) stream))
           (let ((loweredvalue (- (first valuelistr) (first lowervaluelistr))))
             (if (>= restvalue loweredvalue)
               (progn
                 (write-char (first lowercharlistr) stream)
                 (write-char (first charlistr) stream)
                 (- restvalue loweredvalue)
               )
               restvalue
      )) ) ) )
      ((zerop value))
) )

(defconstant FORMAT-CARDINAL-ONES
  '#(NIL "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
     "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen"
     "seventeen" "eighteen" "nineteen"
)   )

(defconstant FORMAT-CARDINAL-TENS
  '#(NIL NIL "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety")
)

; (format-small-cardinal arg stream) gibt eine ganze Zahl >0, <1000 im
; Klartext auf englisch auf den stream aus. (arg=0 -> gibt nichts aus.)
(defun format-small-cardinal (arg stream)
  (multiple-value-bind (hundreds tens-and-ones) (truncate arg 100)
    (when (> hundreds 0)
      (write-string (svref FORMAT-CARDINAL-ONES hundreds) stream)
      (write-string " hundred" stream)
    )
    (when (> tens-and-ones 0)
      (when (> hundreds 0) (write-string " and " stream))
      (multiple-value-bind (tens ones) (truncate tens-and-ones 10)
        (if (< tens 2)
          (write-string (svref FORMAT-CARDINAL-ONES tens-and-ones) stream)
          (progn
            (write-string (svref FORMAT-CARDINAL-TENS tens) stream)
            (when (> ones 0)
              (write-char #\- stream)
              (write-string (svref FORMAT-CARDINAL-ONES ones) stream)
) ) ) ) ) ) )

; (format-cardinal arg stream) gibt die ganze Zahl arg im Klartext auf englisch
; auf den Stream aus.
(defun format-cardinal (arg stream) ; arg Integer
  (if (zerop arg)
    (write-string "zero" stream)
    (progn
      (when (minusp arg) (write-string "minus " stream) (setq arg (- arg)))
      (labels
        ((blocks1000 (illions-list arg) ; Zerlegung in 1000er-Blöcke
           (when (null illions-list)
             (format-error *FORMAT-CS* nil
               (ENGLISH "The argument for the ~~R directive is too large.")
           ) )
           (multiple-value-bind (thousands small) (truncate arg 1000)
             (when (> thousands 0) (blocks1000 (cdr illions-list) thousands))
             (when (> small 0)
               (when (> thousands 0) (write-string ", " stream))
               (format-small-cardinal small stream)
               (write-string (car illions-list) stream)
        )) ) )
        (blocks1000
          ; amerikanisch (billion=10^9)
          '("" " thousand" " million" " billion" " trillion" " quadrillion"
            " quintillion" " sextillion" " septillion" " octillion" " nonillion"
            " decillion" " undecillion" " duodecillion" " tredecillion"
            " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
            " octodecillion" " novemdecillion" " vigintillion")
          arg
) ) ) ) )

(defconstant FORMAT-ORDINAL-ONES
  '#(NIL "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth"
     "ninth" "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
     "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"
)   )

; (format-ordinal arg stream) gibt eine ganze Zahl arg als Abzählnummer im
; Klartext auf englisch auf den stream aus.
(defun format-ordinal (arg stream) ; arg Integer
  (if (zerop arg)
    (write-string "zeroth" stream)
    (progn
      (when (minusp arg) (write-string "minus " stream) (setq arg (- arg)))
      (multiple-value-bind (hundreds tens-and-ones) (floor arg 100)
        (when (> hundreds 0) (format-cardinal (* hundreds 100) stream))
        (if (zerop tens-and-ones)
          (write-string "th" stream)
          (multiple-value-bind (tens ones) (floor tens-and-ones 10)
            (when (> hundreds 0) (write-char #\Space stream))
            (cond ((< tens 2)
                   (write-string (svref FORMAT-ORDINAL-ONES tens-and-ones) stream)
                  )
                  ((zerop ones)
                   (write-string
                     (svref '#(NIL "tenth" "twentieth" "thirtieth" "fortieth" "fiftieth"
                               "sixtieth" "seventieth" "eightieth" "ninetieth")
                            tens
                     )
                     stream
                  ))
                  (t (write-string (svref FORMAT-CARDINAL-TENS tens) stream)
                     (write-char #\- stream)
                     (write-string (svref FORMAT-ORDINAL-ONES ones) stream)
) ) ) ) ) ) )     )

; (format-padding count char stream) gibt count (ein Fixnum >=0) Zeichen char
; auf stream aus.
(defun format-padding (count char stream)
  (dotimes (i count) (write-char char stream))
)

; gibt auf den Stream stream aus:
; den String str, eventuell aufgefüllt mit Padding characters padchar.
; Und zwar so, dass die Breite mindestens mincol ist. Um das zu erreichen,
; werden mindestens minpad Zeichen eingefügt, eventuelle weitere dann in
; Blöcken à colinc Zeichen. Falls padleftflag, werden sie links eingefügt,
; sonst rechts vom String.
(defun format-padded-string
       (mincol colinc minpad padchar padleftflag str stream)
  (let* ((need (+ (length str) minpad)) ; so viele Zeichen mindestens
         (auxpad (if (< need mincol)
                   (* (ceiling (- mincol need) colinc) colinc)
                   0
        ))       ) ; so viele Zeichen zusätzlich
    (unless padleftflag (write-string str stream))
    (format-padding (+ minpad auxpad) padchar stream)
    (when padleftflag (write-string str stream))
) )

; gibt den Integer arg auf den Stream aus:
; in Zahlenbasis base, mit Vorzeichen (+ nur falls >=0 und positive-sign-flag),
; bei commaflag alle drei Stellen unterbrochen durch ein Zeichen commachar.
; Das Ganze links aufgefüllt mit padchar's, so dass die Gesamtbreite mindestens
; mincol ist.
(defun format-integer (base
                       mincol
                       padchar
                       commachar
                       commainterval
                       commaflag
                       positive-sign-flag
                       arg
                       stream
                      )
  (let* ((*print-base* base)
         (*print-radix* nil)
         (*print-readably* nil))
    (if (and (zerop mincol) (not commaflag) (not positive-sign-flag))
      (princ arg stream) ; normale Ausgabe tut's
      (let* ((oldstring (princ-to-string arg))
             (oldstring-length (length oldstring))
             (number-of-digits
               (if (minusp arg) (1- oldstring-length) oldstring-length) )
             (number-of-commas
               (if commaflag (floor (1- number-of-digits) commainterval) 0) )
             (positive-sign (and positive-sign-flag (>= arg 0)))
             (newstring-length
               (+ (if positive-sign 1 0) ; Vorzeichen
                  oldstring-length number-of-commas ; Ziffern, Kommas
             ) )
             (newstring (make-string newstring-length)) )
        ; Erst Vorzeichen +:
        (when positive-sign (setf (schar newstring 0) #\+))
        ; Dann oldstring in newstring übertragen, dabei Kommata überspringen:
        (let ((oldpos oldstring-length) (newpos newstring-length))
          (loop
            (decf oldpos)
            (when (minusp oldpos) (return))
            (decf newpos)
            (setf (schar newstring newpos) (schar oldstring oldpos))
            (when (and (plusp number-of-commas)
                       (zerop (mod (- oldstring-length oldpos) commainterval))
                  ) ; noch ein Komma einzufügen?
              (decf newpos)
              (setf (schar newstring newpos) commachar)
              (decf number-of-commas)
        ) ) )
        (if (zerop mincol)
          (write-string newstring stream) ; schneller
          (format-padded-string mincol 1 0 padchar t newstring stream)
) ) ) ) )

; was ~D bei non-Integer-Argument tut: Argument mit ~A, aber dezimal ausgeben
(defun format-ascii-decimal (arg stream)
  (let ((*print-base* 10.)
        (*print-radix* nil)
        (*print-readably* nil))
    (princ arg stream)
) )

; Unterprogramm für ~D, ~B, ~O, ~X:
(defun format-base (base stream colon-modifier atsign-modifier
                    mincol padchar commachar commainterval
                    arg)
  (if (or (and (zerop mincol) (not colon-modifier) (not atsign-modifier))
          (not (integerp arg))
      )
    (let ((*print-base* base)
          (*print-radix* nil)
          (*print-readably* nil))
      (princ arg stream)
    )
    (format-integer base mincol padchar commachar commainterval
                    colon-modifier atsign-modifier arg stream
) ) )

; (format-scale-exponent-aux arg null eins zehn zehntel lg2)
; liefert zur Floating-Point-Zahl arg >= 0 und
; null = 0.0, eins = 1.0, zehn = 10.0, zehntel = 0.1, lg2 = log(2)/log(10)
; (erste vier in derselben Floating-Point-Precision wie arg)
; zwei Werte: mantissa und n, mit
; ganzem n und mantissa floating-point, 0.1 <= mantissa < 1,
; arg = mantissa * 10^n (also 10^(n-1) <= arg < 10^n ).
; (Bei arg=null: null und n=0.)
(defun format-scale-exponent-aux (arg null eins zehn zehntel lg2)
  (multiple-value-bind (significand expon) (decode-float arg)
    (declare (ignore significand))
    (if (zerop arg)
      (values null 0)
      (let* ((expon10a (truncate (* expon lg2))) ; nicht round, um Überlauf zu vermeiden
             (signif10a (/ arg (expt zehn expon10a))))
        (do ((zehnpot zehn (* zehnpot zehn))
             (signif10b signif10a (/ signif10a zehnpot))
             (expon10b expon10a (1+ expon10b)))
            ((< signif10b eins)
             (do ((zehnpot zehn (* zehnpot zehn))
                  (signif10c signif10b (* signif10b zehnpot))
                  (expon10c expon10b (1- expon10c)))
                 ((>= signif10c zehntel)
                  (values signif10c expon10c)
             )   )
        )   )
) ) ) )

; (format-scale-exponent arg) liefert zur Floating-Point-Zahl arg >= 0
; zwei Werte: mantissa und n, mit
; ganzem n und mantissa floating-point, 0.1 <= mantissa < 1,
; arg = mantissa * 10^n (also 10^(n-1) <= arg < 10^n ).
; (Bei arg=null: 0.0 und n=0.)
(defun format-scale-exponent (arg)
  (cond ((short-float-p arg)
         (format-scale-exponent-aux arg 0.0s0 1.0s0 10.0s0 0.1s0 0.30103s0)
        )
        ((single-float-p arg)
         (format-scale-exponent-aux arg 0.0f0 1.0f0 10.0f0 0.1f0 0.30103s0)
        )
        ((double-float-p arg)
         (format-scale-exponent-aux arg 0.0d0 1.0d0 10.0d0 0.1d0 0.30103s0)
        )
        ((long-float-p arg)
         (format-scale-exponent-aux arg
           (float 0 arg) (float 1 arg) (float 10 arg) (float 1/10 arg)
           0.30102999566d0 ; lg2 wird mit 32 Bit Genauigkeit gebraucht
) )     ))

; (format-float-to-string arg width d k dmin)
; ergibt einen String zum Floating-point arg:
; er hat den Wert von (* (abs arg) (expt 10 k)), dabei mind. d Nachkommastellen
; und höchstens die Länge width (width=nil -> keine Einschränkung).
; Trotzdem wird nicht auf weniger als dmin Stellen gerundet.
(let ((digit-string
        (make-array 20 :element-type 'character :adjustable t :fill-pointer t)
     ))
(defun format-float-to-string (arg width d k dmin)
  (if (zerop arg)
    (let ((places (max (or d 0) (or dmin 0))))
      (when width ; width angegeben -> places := (min places (1- width))
        (when (>= places width) (setq places (1- width)))
      )
      (values
        (let ((str (make-string (1+ places) :initial-element #\0)))
          (setf (schar str 0) #\.)
          str          ; ein Punkt und places Nullen
        )
        (1+ places)    ; Stellenzahl
        t              ; Punkt ganz vorne
        (zerop places) ; Punkt ganz hinten ?
        0              ; Position des Punktes
    ) )
    (multiple-value-bind (significand expon) (integer-decode-float arg)
; significand : Integer >0
; expon : Integer
; mantprec : Anzahl der echten Mantissenbits von significand
; (also 2^mantprec <= significand < 2^(mantprec+1))
; width : Anzahl Stellen, die die Zahl (inklusive Punkt) nicht überschreiten
;         soll, oder NIL
; d : Mindestanzahl Nachkommastellen oder NIL
; k : Skalierungsfaktor (siehe CLTL S.394)
; dmin : Mindestanzahl von Dezimaltellen, die (trotz Angabe von width oder d)
;        nicht gerundet werden dürfen.
;        (Nur interessant, falls d <= dmin <= (precision der Zahl).)
; wandelt die Zahl significand*2^expon um in einen Dezimalstring um.
; Es ist kein Exponent dabei.
      (let* ((mantprec (1- (float-digits arg)))
             (numerator significand)
             (denominator 1)
             (abrund-einh 1) ; Abrundungseinheit:
               ; Abrunden um 1 in der letzten abrundbaren Stelle entspricht
               ; einer Erniedrigung von numerator um abrund-einh.
             (aufrund-einh 1) ; Aufrundungseinheit:
               ; Aufrunden um 1 in der letzten aufrundbaren Stelle entspricht
               ; einer Erhöhung von numerator um aufrund-einh.
             ; Stellen: 0 = 1. Stelle vor dem Punkt, -1 = 1. Stelle nach dem Punkt.
             (stelle 0) ; Stelle der als nächstes auszugebenden Ziffer
             (digit-count 0) ; Zahl der bisher in digit-string ausgegebenen
                             ; Ziffern (exklusive den Punkt)
             (point-pos 0) ; Punkt-Position = Zahl führender Stellen
                           ; = Zahl der Ziffern vor dem Punkt
             (letzte-stelle nil) ; NIL oder (falls d oder width angegeben waren)
                           ; Stelle der letzten signifikanten Ziffer
             (halbzahlig nil) ; zeigt an, ob hinten genau ein 0.500000 wegfällt
             digit ; die laufende Ziffer, >=0, <10
             (abrunden nil) ; T falls letzte Ziffer abzurunden ist
             (aufrunden nil) ; T falls letzte Ziffer aufzurunden ist
            )
        (setf (fill-pointer digit-string) 0) ; digit-string leeren
        (cond
          ((> expon 0)
           (setq numerator (ash significand expon))
           (setq aufrund-einh (setq abrund-einh (ash 1 expon)))
          )
          ((< expon 0)
           (setq denominator (ash 1 (- expon))) ; aufrund-einh = abrund-einh = 1
        ) )
        ; Zahl = numerator/denominator
        (when (= significand (ash 1 mantprec))
          ; Ist der Significand=2^mantprec, so ist abrund-einh zu halbieren.
          ; Man kann stattdessen auch alle 3 anderen Grössen verdoppeln:
          (setq aufrund-einh (ash aufrund-einh 1))
          (setq numerator (ash numerator 1))
          (setq denominator (ash denominator 1))
        )
        ; Defaultmäßig: Auf-/Abrunde-Einheit = eine Einheit in der letzten
        ; BINÄRstelle.
        ; Zahl = numerator/denominator
        ; Skalierungsfaktor k in die Zahl mit einbeziehen (vgl. CLTL S.394)
        ; k<0 -> Mantisse durch 10^(abs k) dividieren
        ; k>0 -> Mantisse mit 10^k multiplizieren
        ; Dabei aufrund-einh, abrund-einh im Verhältnis zu numerator beibehalten.
        (when k
          (if (< k 0)
            (let ((skal-faktor (expt 10 (- k))))
              (setq denominator (* denominator skal-faktor))
            )
            (let ((skal-faktor (expt 10 k)))
              (setq numerator (* numerator skal-faktor))
              (setq aufrund-einh (* aufrund-einh skal-faktor))
              (setq abrund-einh (* abrund-einh skal-faktor))
            )
        ) )
        ; auf >= 1/10 adjustieren:
        ; (jeweils numerator mit 10 multiplizieren, eine führende 0 mehr vorsehen)
        (do ()
            ((>= (* numerator 10) denominator))
          (setq stelle (1- stelle))
          (setq numerator (* numerator 10))
          (setq abrund-einh (* abrund-einh 10))
          (setq aufrund-einh (* aufrund-einh 10))
        )
        ; stelle = Stelle der letzten führenden 0
        ;        = 1 + Stelle der 1. signifikanten Ziffer
        ;        oder =0, falls k>=0
        ; Ausführung der Rundung:
        (loop
          ; Solange das Ergebnis auch nach Aufrundung >= 1 bliebe,
          ; eine Vorkommastelle mehr einplanen:
          (do ()
              ((< (+ (ash numerator 1) aufrund-einh) (ash denominator 1)))
            (setq denominator (* denominator 10))
            (setq stelle (1+ stelle))
          )
          ; Falls d oder width angegeben:
          ; letzte-stelle ausrechnen
          (if d
            ; Falls dmin angegeben: (min (- d) (- dmin)) = (- (max d dmin)).
            ; Sonst (- d).
            (progn
              (setq letzte-stelle (- d))
              (when (and dmin (> letzte-stelle (- dmin)))
                (setq letzte-stelle (- dmin))
            ) )
            ; Falls nicht d, nur width angegeben:
            (when width
              (if (< stelle 0)
                ; Es kommen führende Nullen nach dem Punkt -> d:=(1- width)
                (setq letzte-stelle (- 1 width))
                ; Es kommen keine führenden Nullen nach dem Punkt ->
                ; Es wird stelle Vorkommaziffern geben, d:=(- (1- width) stelle)
                (setq letzte-stelle (1+ (- stelle width)))
              )
              ; also letzte-stelle = (- (- (1- width) (max stelle 0)))
              ; wieder dmin berücksichtigen:
              (when (and dmin (> letzte-stelle (- dmin)))
                (setq letzte-stelle (- dmin))
          ) ) )
          (when (or d width)
            (let* ((ziffernzahl (- letzte-stelle stelle))
                   ; ziffernzahl = - Zahl signifikanter Stellen oder >=0.
                   (dezimal-einh denominator))
              ; dezimal-einh := (ceiling (* dezimal-einh (expt 10 ziffernzahl)))
              (if (>= ziffernzahl 0)
                (dotimes (i ziffernzahl)
                  (setq dezimal-einh (* dezimal-einh 10))
                )
                (dotimes (i (- ziffernzahl))
                  (setq dezimal-einh (ceiling dezimal-einh 10))
                )
              )
              ; dezimal-einh = Um wieviel numerator erhöht bzw. erniedigt werden
              ; müsste, damit sich die Dezimaldarstellung um genau 1 an der
              ; Position letzte-stelle verändert.
              (setq abrund-einh (max dezimal-einh abrund-einh))
              (setq aufrund-einh (max dezimal-einh aufrund-einh))
              ; Jetzt darf auch um eine (halbe) DEZIMAL-Einheit gerundet werden.
              (when (= aufrund-einh dezimal-einh) (setq halbzahlig T))
          ) )
          (when (< (+ (ash numerator 1) aufrund-einh) (ash denominator 1))
            (return)
        ) )
        ; stelle = Position der ersten signifikanten Stelle + 1
        ; Führenden Punkt und nachfolgende Nullen ausgeben:
        (when (< stelle 0)
          (setq point-pos digit-count)
          (vector-push-extend #\. digit-string)
          (dotimes (i (- stelle))
            (incf digit-count)
            (vector-push-extend #\0 digit-string)
        ) )
        ; Ziffern der Mantisse ausgeben:
        (loop
          (when (zerop stelle)
            (vector-push-extend #\. digit-string)
            (setq point-pos digit-count)
          )
          (decf stelle)
          (multiple-value-setq (digit numerator)
            (truncate (* numerator 10) denominator)
          )
          (setq abrund-einh (* abrund-einh 10))
          (setq aufrund-einh (* aufrund-einh 10))
          (setq abrunden (< (ash numerator 1) abrund-einh))
          (if halbzahlig
            (setq aufrunden
              (>= (ash numerator 1) (- (ash denominator 1) aufrund-einh))
            )
            (setq aufrunden
              (> (ash numerator 1) (- (ash denominator 1) aufrund-einh))
            )
          )
          (when (or abrunden aufrunden
                    (and letzte-stelle (<= stelle letzte-stelle))
                )
            (return)
          )
          (vector-push-extend (schar "0123456789" digit) digit-string)
          (incf digit-count)
        )
        ; letzte signifikante Ziffer ausgeben:
        (when (or (null letzte-stelle) (>= stelle letzte-stelle))
          (vector-push-extend
            (schar "0123456789"
              (cond
                ((and abrunden (not aufrunden)) digit)
                ((and aufrunden (not abrunden)) (1+ digit))
                ((<= (ash numerator 1) denominator) digit)
                (t (1+ digit))
            ) )
            digit-string
          )
          (incf digit-count)
        )
        ; Nachfolgende Nullen und Punkt ausgeben
        (when (>= stelle 0)
          (dotimes (i stelle)
            (incf digit-count)
            (vector-push-extend #\0 digit-string)
          )
          (vector-push-extend #\. digit-string)
          (setq point-pos digit-count)
        )
        (when d
          (dotimes (i (- d (- digit-count point-pos)))
            (incf digit-count)
            (vector-push-extend #\0 digit-string)
        ) )
        (values
                  digit-string               ; Ziffern
                  (1+ digit-count)           ; Anzahl der Ziffern
                  (= point-pos 0)            ; Punkt ganz vorne?
                  (= point-pos digit-count)  ; Punkt ganz hinten?
                  point-pos                  ; Position des Punktes
        ) ; 5 Werte
) ) ) )
)

; (format-float-for-f w d k overflowchar padchar plus-sign-flag arg stream)
; gibt die Floating-Point-Zahl arg in Festkommadarstellung auf stream aus.
(defun format-float-for-f (w d k overflowchar padchar plus-sign-flag arg stream)
  (let ((width (if w (if (or plus-sign-flag (minusp arg)) (1- w) w) nil)))
    ; width = zur Verfügung stehende Zeichen ohne Vorzeichen
    (multiple-value-bind (digits digitslength leadingpoint trailingpoint)
        (format-float-to-string arg width d k nil)
      (when (eql d 0) (setq trailingpoint nil)) ; d=0 -> keine Zusatz-Null hinten
      (when w
        (setq width (- width digitslength))
        (when leadingpoint ; evtl. Zusatz-Null vorne einplanen
          (if (> width 0) (setq width (1- width)) (setq leadingpoint nil))
        )
        (when trailingpoint ; evtl. Zusatz-Null hinten einplanen
          (if (> width 0) (setq width (1- width)) (setq trailingpoint nil))
        )
      )
      ; Es bleiben noch width Zeichen übrig.
      (if (and overflowchar w (minusp width))
        (format-padding w overflowchar stream) ; Zu wenig Platz -> overflow
        (progn
          (when (and w (> width 0)) (format-padding width padchar stream))
          (if (minusp arg)
            (write-char #\- stream)
            (if plus-sign-flag (write-char #\+ stream))
          )
          (when leadingpoint (write-char #\0 stream))
          (write-string digits stream)
          (when trailingpoint (write-char #\0 stream))
      ) )
) ) )

; (format-float-for-e w d e k overflowchar padchar exponentchar plus-sign-flag
;                     arg stream)
; gibt die Floating-point-Zahl arg in Exponentialdarstellung auf den stream aus.
; (vgl. CLTL S.392-394)
; Aufteilung der Mantisse:
;   Falls k<=0, erst 1 Null (falls von der Breite her passend), dann der Punkt,
;               dann |k| Nullen, dann d-|k| signifikante Stellen;
;               zusammen also d Nachkommastellen.
;   Falls k>0,  erst k signifikante Stellen, dann der Punkt,
;               dann weitere d-k+1 signifikante Stellen;
;               zusammen also d+1 signifikante Stellen. Keine Nullen vorne.
;   (Der Defaultwert in FORMAT-EXPONENTIAL-FLOAT ist k=1.)
; Vor der Mantisse das Vorzeichen (ein + nur falls arg>=0 und plus-sign-flag).
; Dann der Exponent, eingeleitet durch exponentchar, dann Vorzeichen des
; Exponenten (stets + oder -), dann e Stellen für den Exponenten.
; Dann wird das Ganze mit padchars auf w Zeichen Breite aufgefüllt.
; Sollte das (auch nach evtl. Unterdrückung einer führenden Null) mehr als
; w Zeichen ergeben, so werden statt dessen w overflowchars ausgegeben, oder
; (falls overflowchar = nil) die Zahl mit so vielen Stellen wie nötig
; ausgegeben.
(defun format-float-for-e (w d e k
       overflowchar padchar exponentchar plus-sign-flag arg stream)
  (multiple-value-bind (mantissa oldexponent) (format-scale-exponent (abs arg))
    (let* ((exponent (if (zerop arg) 0 (- oldexponent k))) ; auszugebender Exponent
           (expdigits (write-to-string (abs exponent) :base 10. :radix nil :readably nil))
           (expdigitsneed (if e (max (length expdigits) e) (length expdigits)))
           ; expdigitsneed = Anzahl der Stellen, die für die Ziffern des
           ; Exponenten nötig sind.
           (mantd (if d (if (> k 0) (1+ (- d k)) d) nil))
           ; mantd = Anzahl der Mantissenstellen hinter dem Punkt
           (dmin (if (minusp k) (- 1 k) nil)) ; nachher: fordere, dass
           ; nicht in die ersten (+ 1 (abs k)) Stellen hineingerundet wird.
           (mantwidth (if w (- w 2 expdigitsneed) nil))
           ; mantwidth = Anzahl der für die Mantisse (inkl. Vorzeichen, Punkt)
           ; zur Verfügung stehenden Zeichen (oder nil)
          )
      (declare (simple-string expdigits) (fixnum exponent expdigitsneed))
      (if (and overflowchar w e (> expdigitsneed e))
        ; Falls Overflowchar und w und e angegeben, Exponent mehr braucht:
        (format-padding w overflowchar stream)
        (progn
          (if w
            (if (or plus-sign-flag (minusp arg)) (setq mantwidth (1- mantwidth)))
          )
          ; mantwidth = Anzahl der für die Mantisse (ohne Vorzeichen,
          ; inklusive Punkt) zur Verfügung stehenden Zeichen (oder nil)
          (multiple-value-bind (mantdigits mantdigitslength
                                leadingpoint trailingpoint)
              (format-float-to-string mantissa mantwidth mantd k dmin)
            (when w
              (setq mantwidth (- mantwidth mantdigitslength))
              (if trailingpoint
                (if (or (null mantd) (> mantd 0))
                  (setq mantwidth (- mantwidth 1))
                  (setq trailingpoint nil)
              ) )
              (if leadingpoint
                (if (> mantwidth 0)
                  (setq mantwidth (- mantwidth 1))
                  (setq leadingpoint nil)
              ) )
            )
            ; Es bleiben noch mantwidth Zeichen übrig.
            (if (and overflowchar w (minusp mantwidth))
              (format-padding w overflowchar stream) ; Zu wenig Platz -> overflow
              (progn
                (when (and w (> mantwidth 0))
                  (format-padding mantwidth padchar stream)
                )
                (if (minusp arg)
                  (write-char #\- stream)
                  (if plus-sign-flag (write-char #\+ stream))
                )
                (if leadingpoint (write-char #\0 stream))
                (write-string mantdigits stream)
                (if trailingpoint (write-char #\0 stream))
                (write-char
                  (cond (exponentchar)
                        ((and (not *PRINT-READABLY*)
                              (typep arg *READ-DEFAULT-FLOAT-FORMAT*)
                         )
                         #\E
                        )
                        ((short-float-p arg) #\s)
                        ((single-float-p arg) #\f)
                        ((double-float-p arg) #\d)
                        ((long-float-p arg) #\L)
                  )
                  stream
                )
                (write-char (if (minusp exponent) #\- #\+) stream)
                (when (and e (> e (length expdigits)))
                  (format-padding (- e (length expdigits)) #\0 stream)
                )
                (write-string expdigits stream)
          ) ) )
    ) ) )
) )

; Rückt *FORMAT-CSDL* vor bis zum Ende des momentanen ~[ bzw. ~{ bzw. ~< .
(defun format-skip-to-end ()
  (do ()
      ((null (csd-clause-chain (car *FORMAT-CSDL*))))
    (setq *FORMAT-CSDL* (csd-clause-chain (car *FORMAT-CSDL*)))
) )

; (format-justified-segments mincol colinc minpad justify-left justify-right
;   piecelist) berechnet, an welchen Stellen zwischen den einzelnen Strings in
; piecelist wieviele Leerstellen zu setzen sind.
; Zwischen die einzelnen Strings aus piecelist (auch vorher, falls justify-left;
; auch nachher, falls justify-right) werden mindestens minpad padding-characters
; eingefügt. Dann werden nochmals weitere padding-characters dazugenommen,
; damit die Gesamtbreite >= mincol wird. Ist die Breite > mincol, werden weitere
; padding-characters dazugenommen, so dass die Breite von der Form
; mincol + k * colinc wird. Diese padding-characters werden auf die einzelnen
; Stellen gleichmäßig verteilt.
; 1. Wert: Ein Vektor, der zu jeder Stelle angibt, wieviele padding-characters
; einzufügen sind (NIL = keine).
; Erstes Element: ganz links, zweites: nach 1. String, ..., letztes: rechts.
; 2. Wert: Die sich ergebende Gesamtbreite.
(defun format-justified-segments
       (mincol colinc minpad justify-left justify-right piecelist)
  (declare (fixnum mincol colinc minpad))
  (let ((piecesnumber 0)
        (pieceswidth 0))
    (dolist (piece piecelist)
      (declare (simple-string piece))
      (incf piecesnumber)
      (incf pieceswidth (length piece))
    )
    (let* ((new-justify-left
             (or justify-left (and (= piecesnumber 1) (not justify-right))))
           (padblocks (+ piecesnumber -1       ; Anzahl der Einfüge-Stellen
                         (if new-justify-left 1 0) (if justify-right 1 0)
           )          )
           (width-need (+ pieceswidth (* padblocks minpad)))
           (width (+ mincol
                     (if (<= width-need mincol)
                         0
                         (* (ceiling (- width-need mincol) colinc) colinc)
          ))      )  )
      (declare (fixnum piecesnumber pieceswidth padblocks width-need width))
      (multiple-value-bind (padwidth rest) (floor (- width pieceswidth) padblocks)
        (let ((padblock-lengths
                (make-array (1+ piecesnumber) :initial-element padwidth)
             ))
          (unless new-justify-left (setf (svref padblock-lengths 0) nil))
          (unless justify-right (setf (svref padblock-lengths piecesnumber) nil))
          (do ((i 0 (1+ i)))
              ((zerop rest))
            (when (svref padblock-lengths i)
              (incf (svref padblock-lengths i))
              (decf rest)
          ) )
          (values padblock-lengths width)
) ) ) ) )

;-------------------------------------------------------------------------------

; ~A, CLTL S.387-388, CLtL2 S. 584
(defformat-simple format-ascii (stream colon-modifier atsign-modifier
                  (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
                  (arg)
  (when (and colon-modifier (null arg)) (setq arg "()"))
  (if (and (zerop mincol) (zerop minpad))
    (princ arg stream)
    (format-padded-string mincol colinc minpad padchar
      atsign-modifier ; =: padleftflag
      (princ-to-string arg)
      stream
) ) )

; ~S, CLTL S.388, CLtL2 S. 584
(defformat-simple format-s-expression (stream colon-modifier atsign-modifier
                  (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
                  (arg)
  (if (and (zerop mincol) (zerop minpad))
    (if (and colon-modifier (null arg))
      (write-string "()" stream)
      (prin1 arg stream)
    )
    (format-padded-string mincol colinc minpad padchar
      atsign-modifier ; =: padleftflag
      (if (and colon-modifier (null arg)) "()" (prin1-to-string arg))
      stream
) ) )

; ~W
(defformat-simple format-write (stream colon-modifier atsign-modifier
                  (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
                  (arg)
  (declare (ignore colon-modifier))
  (if (and (zerop mincol) (zerop minpad))
    (write arg :stream stream)
    (format-padded-string mincol colinc minpad padchar
      atsign-modifier ; =: padleftflag
      (write-to-string arg)
      stream
) ) )

; ~D, CLTL S.388, CLtL2 S. 585
(defformat-simple format-decimal (stream colon-modifier atsign-modifier
                  (mincol 0) (padchar #\Space) (commachar #\,) (commainterval 3))
                  (arg)
  (format-base 10 stream colon-modifier atsign-modifier mincol padchar commachar commainterval arg)
)

; ~B, CLTL S.388, CLtL2 S. 585
(defformat-simple format-binary (stream colon-modifier atsign-modifier
                  (mincol 0) (padchar #\Space) (commachar #\,) (commainterval 3))
                  (arg)
  (format-base 2 stream colon-modifier atsign-modifier mincol padchar commachar commainterval arg)
)

; ~O, CLTL S.388, CLtL2 S. 585
(defformat-simple format-octal (stream colon-modifier atsign-modifier
                  (mincol 0) (padchar #\Space) (commachar #\,) (commainterval 3))
                  (arg)
  (format-base 8 stream colon-modifier atsign-modifier mincol padchar commachar commainterval arg)
)

; ~X, CLTL S.388-389, CLtL2 S. 586
(defformat-simple format-hexadecimal (stream colon-modifier atsign-modifier
                  (mincol 0) (padchar #\Space) (commachar #\,) (commainterval 3))
                  (arg)
  (format-base 16 stream colon-modifier atsign-modifier mincol padchar commachar commainterval arg)
)

; ~R, CLTL S.389, CLtL2 S. 586-587
(defformat-simple format-radix (stream colon-modifier atsign-modifier
                  (radix nil) (mincol 0) (padchar #\Space) (commachar #\,) (commainterval 3))
                  (arg)
  (if radix
    (format-integer radix mincol padchar commachar commainterval
                    colon-modifier atsign-modifier
                    arg stream
    )
    (if atsign-modifier
      (if (integerp arg)
        (if colon-modifier
          (format-old-roman arg stream)
          (format-new-roman arg stream)
        )
        (format-error *FORMAT-CS* nil
          (ENGLISH "The ~~R and ~~:R directives require an integer argument, not ~S")
          arg
      ) )
      (if colon-modifier
        (format-ordinal arg stream)
        (format-cardinal arg stream)
) ) ) )

; ~P, CLTL S. 389, CLtL2 S. 587-588
(defun format-plural (stream colon-modifier atsign-modifier)
  (when colon-modifier (format-goto-new-arg t 1))
  (let ((singular (eql (next-arg) 1)))
    (if atsign-modifier
      (write-string (if singular "y" "ies") stream)
      (unless singular (write-char #\s stream))
) ) )

; ~C, CLTL S.389-390, CLtL2 S. 588
(defformat-simple format-character (stream colon-modifier atsign-modifier)
                  (arg)
  (unless (characterp arg)
    (format-error *FORMAT-CS* nil
      (ENGLISH "The ~~C directive requires a character argument, not ~S")
      arg
  ) )
  (if (not colon-modifier)
    (if (not atsign-modifier)
      ; ~C
      (write-char arg stream)
      ; ~@C
      (prin1 arg stream)
    )
    ; ~:C prints the name of non-printing characters.
    ; ~:@C prints instructions how to type the character.
    ; Since characters don't have attributes any more, both do the same.
    (if (and (graphic-char-p arg) (not (eql arg #\Space)))
      (write-char arg stream)
      (let ((name (char-name arg)))
        (if name
          (write-string (string-capitalize name) stream)
          (write-char arg stream)
    ) ) )
) )

; ~F, CLTL S.390-392, CLtL2 S. 588-590
(defformat-simple format-fixed-float (stream colon-modifier atsign-modifier
                  (w nil) (d nil) (k 0) (overflowchar nil) (padchar #\Space))
                  (arg)
  (declare (ignore colon-modifier))
  (when (rationalp arg) (setq arg (float arg)))
  (if (floatp arg)
    (format-float-for-f w d k overflowchar padchar atsign-modifier arg stream)
    (format-ascii-decimal arg stream)
) )

; ~E, CLTL S.392-395, CLtL2 S. 590-593
(defformat-simple format-exponential-float (stream colon-modifier atsign-modifier
                  (w nil) (d nil) (e nil) (k 1)
                  (overflowchar nil) (padchar #\Space) (exponentchar nil))
                  (arg)
  (declare (ignore colon-modifier))
  (when (rationalp arg) (setq arg (float arg)))
  (if (floatp arg)
    (format-float-for-e w d e k overflowchar padchar exponentchar
                        atsign-modifier arg stream
    )
    (format-ascii-decimal arg stream)
) )

; ~G, CLTL S.395-396, CLtL2 S. 594-595
(defformat-simple format-general-float (stream colon-modifier atsign-modifier
                  (w nil) (d nil) (e nil) (k 1)
                  (overflowchar nil) (padchar #\Space) (exponentchar nil))
                  (arg)
  (declare (ignore colon-modifier))
  (if (rationalp arg) (setq arg (float arg)))
  (if (floatp arg)
    (multiple-value-bind (mantissa n) (format-scale-exponent (abs arg))
      (declare (ignore mantissa))
      (if (null d)
        (setq d
          (multiple-value-bind (digits digitslength)
            (format-float-to-string (abs arg) nil nil nil nil)
            (declare (ignore digits))
            (max (max (1- digitslength) 1) (min n 7))
      ) ) )
      (let* ((ee (if e (+ 2 e) 4))
             (dd (- d n)))
        (if (<= 0 dd d)
          (progn
            (format-float-for-f
              (if w (- w ee) nil)
              dd 0
              overflowchar padchar atsign-modifier arg stream
            )
            (format-padding ee #\Space stream)
          )
          (format-float-for-e w d e k overflowchar padchar exponentchar
                              atsign-modifier arg stream
    ) ) ) )
    (format-ascii-decimal arg stream)
) )

; ~$, CLTL S.396-397, CLtL2 S. 595-596
(defformat-simple format-dollars-float (stream colon-modifier atsign-modifier
                  (d 2) (n 1) (w 0) (padchar #\Space))
                  (arg)
  (when (rationalp arg) (setq arg (float arg)))
  (if (floatp arg)
    (multiple-value-bind (digits digitslength
                          leadingpoint trailingpoint leadings)
      (format-float-to-string arg nil d 0 nil)
      (declare (ignore digitslength leadingpoint trailingpoint))
      (let* ((lefts (max leadings n))
             (totalwidth (+ (if (or atsign-modifier (minusp arg)) 1 0)
                            lefts 1 d
             )           )
             (padcount (max (- w totalwidth) 0)))
        (if (not colon-modifier) (format-padding padcount padchar stream))
        (if (minusp arg)
          (write-char #\- stream)
          (if atsign-modifier (write-char #\+ stream))
        )
        (if colon-modifier (format-padding padcount padchar stream))
        (format-padding (- lefts leadings) #\0 stream)
        (write-string digits stream)
    ) )
    (format-ascii-decimal arg stream)
) )

; ~%, CLTL S.397, CLtL2 S. 596
(defun format-terpri (stream colon-modifier atsign-modifier &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (dotimes (i count) (terpri stream))
)

; ~&, CLTL S.397, CLtL2 S. 596
(defun format-fresh-line (stream colon-modifier atsign-modifier
                          &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (when (plusp count)
    (fresh-line stream)
    (dotimes (i (1- count)) (terpri stream))
) )

; ~|, CLTL S.397, CLtL2 S. 596
(defun format-page (stream colon-modifier atsign-modifier &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (dotimes (i count) (write-char #\Page stream))
)

; ~~, CLTL S.397, CLtL2 S. 596
(defun format-tilde (stream colon-modifier atsign-modifier &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (dotimes (i count) (write-char #\~ stream))
)

; ~T, CLTL S.398-399, CLtL2 S. 597-598
(defun format-tabulate (stream colon-modifier atsign-modifier
                        &optional (colnum 1) (colinc 1))
  (declare (ignore colon-modifier))
  (if (null colnum) (setq colnum 1))
  (if (null colinc) (setq colinc 1))
  (let* ((new-colnum (max colnum 0))
         (new-colinc (max colinc 1)) ; >0
         (pos (sys::line-position stream))) ; aktuelle Position, Fixnum >=0 oder NIL
    (if atsign-modifier
      (format-padding
        (if pos (+ new-colnum (mod (- (+ pos new-colnum)) new-colinc)) new-colnum)
        #\Space stream
      )
      (if pos
        (if (< pos new-colnum)
          (format-padding (- new-colnum pos) #\Space stream)
          (unless (zerop colinc)
            (format-padding (+ colinc (mod (- new-colnum pos) (- colinc)))
                            #\Space stream
        ) ) )
        (format-padding 2 #\Space stream)
) ) ) )

; ~*, CLTL S.399, CLtL2 S. 598
(defun format-goto (stream colon-modifier atsign-modifier &optional (index nil))
  (declare (ignore stream))
  (if atsign-modifier
    (setq *FORMAT-NEXT-ARG* (nthcdr (or index 0) *FORMAT-ARG-LIST*))
    (format-goto-new-arg colon-modifier (or index 1))
) )

; ~?, CLTL S.399-401, CLtL2 S. 598-599
(defun format-indirection (stream colon-modifier atsign-modifier)
  (declare (ignore colon-modifier))
  (let* ((csarg (next-arg))
         (node (do-format-indirection-1 csarg)))
    (if atsign-modifier
      (if (consp node)
        (let ((*FORMAT-CS* (car node))
              (*FORMAT-CSDL* (cdr node))
             ;(*FORMAT-ARG-LIST* *FORMAT-NEXT-ARG*) ; ??
              (*FORMAT-UP-AND-OUT* nil))
          (format-interpret stream)
        )
        (setq *FORMAT-NEXT-ARG*
          (let ((*FORMAT-CS* nil))
            (apply node stream *FORMAT-NEXT-ARG*)
      ) ) )
      (let ((arglistarg (next-arg)))
        (do-format-indirection-2 stream node arglistarg arglistarg)
) ) ) )
(defun do-format-indirection (stream csarg arguments)
  (unless (or (stringp csarg) (functionp csarg))
    (format-indirection-cserror csarg)
  )
  (unless (listp arguments) (format-indirection-lerror arguments))
  (format-apply stream csarg arguments)
)
(defun do-format-indirection-1 (csarg)
  (cond ((stringp csarg)
         (let ((node (list csarg)))
           (format-parse-cs csarg 0 node nil)
           node
        ))
        ((functionp csarg)
         csarg
        )
        (t (format-indirection-cserror csarg))
) )
(defun do-format-indirection-2 (stream node arglistarg wholelistarg)
  (unless (listp arglistarg) (format-indirection-lerror arglistarg))
  (if (consp node)
    (let* ((*FORMAT-CS*         (car node))
           (*FORMAT-CSDL*       (cdr node))
           (*FORMAT-ARG-LIST*   wholelistarg)
           (*FORMAT-NEXT-ARG*   arglistarg)
           (*FORMAT-NEXT-ARGLIST* nil)
           (*FORMAT-UP-AND-OUT* nil))
      (format-interpret stream)
      *FORMAT-NEXT-ARG*
    )
    (let ((*FORMAT-CS* nil))
      (apply node stream arglistarg) ; wholelistarg??
) ) )
(defun format-indirection-cserror (csarg)
  (format-error *FORMAT-CS* nil
    (ENGLISH "The control string argument for the ~~? directive is invalid: ~S")
    csarg
) )
(defun format-indirection-lerror (arguments)
  (format-error *FORMAT-CS* nil
    (ENGLISH "The argument list argument for the ~~? directive is invalid: ~S")
    arguments
) )

;;; ~// ANSI CL 22.3.5.4 Tilde Slash: Call Function
(defun FORMAT-CALL-USER-FUNCTION (stream colon-p atsign-p symbol-list &rest more-args)
  (apply (car symbol-list) stream (next-arg) colon-p atsign-p more-args))

; ~(, CLTL S.401, CLtL2 S. 600-601
(defun format-case-conversion (stream colon-modifier atsign-modifier)
  (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
  (let ((tempstr
          (let ((tempstream (make-string-output-stream :line-position (sys::line-position stream))))
            (format-interpret tempstream 'FORMAT-CASE-CONVERSION-END)
            ; Was bewirkt UP-AND-OUT in ~{...~(...~^...~)...~} ??
            (get-output-stream-string tempstream)
       )) )
    (if colon-modifier
      (if atsign-modifier
        (write-string (nstring-upcase tempstr) stream)
        (write-string (nstring-capitalize tempstr) stream)
      )
      (if atsign-modifier
        (write-string (nstring-capitalize1 tempstr) stream)
        (write-string (nstring-downcase tempstr) stream)
) ) ) )
(defun nstring-capitalize1 (string)
  (setq string (nstring-downcase string))
  (dotimes (i (length string)) ; erstes Zeichen zum Upcase machen
    (when (both-case-p (schar string i))
      (setf (schar string i) (char-upcase (schar string i)))
      (return)
  ) )
  string
)

; ~[, CLTL S.402-403, CLtL2 S. 601-602
(defun format-conditional (stream colon-modifier atsign-modifier
                           &optional (prefix nil))
  (if colon-modifier
    (if atsign-modifier
      (format-conditional-error)
      (progn
        (when (next-arg)
          (setq *FORMAT-CSDL* (csd-clause-chain (car *FORMAT-CSDL*)))
        )
        (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
        (format-interpret stream 'FORMAT-CONDITIONAL-END)
      )
    )
    (if atsign-modifier
      (when (next-arg)
        (format-goto-new-arg t 1)
        (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
        (format-interpret stream 'FORMAT-CONDITIONAL-END)
        (unless (null (csd-clause-chain (car *FORMAT-CSDL*)))
          (format-error *FORMAT-CS* nil
            (ENGLISH "The ~~; directive is not allowed at this point.")
      ) ) )
      (let ((index (or prefix (next-arg))))
        (unless (integerp index)
          (format-error *FORMAT-CS* nil
            (ENGLISH "The ~~[ parameter must be an integer, not ~S")
            index
        ) )
        (dotimes (i (if (minusp index) most-positive-fixnum index))
          (when (eq (csd-data (car *FORMAT-CSDL*)) 'FORMAT-CONDITIONAL-END)
            (return)
          )
          (setq *FORMAT-CSDL* (csd-clause-chain (car *FORMAT-CSDL*)))
          (when (csd-colon-p (car *FORMAT-CSDL*)) (return))
        )
        (unless (eq (csd-data (car *FORMAT-CSDL*)) 'FORMAT-CONDITIONAL-END)
          (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
        )
        (format-interpret stream 'FORMAT-CONDITIONAL-END)
  ) ) )
  (format-skip-to-end) ; Weiterrücken bis ans Ende der ~[...~]-Direktive
)
(defun format-conditional-error ()
  (format-error *FORMAT-CS* nil
    (ENGLISH "The ~~[ directive cannot take both modifiers.")
) )

; ~{, CLTL S.403-404, CLtL2 S. 602-604
(defun format-iteration (stream colon-modifier atsign-modifier
                         &optional (prefix nil))
  (let* ((total-csdl *FORMAT-CSDL*)
         (max-iteration-count prefix))
    (format-skip-to-end) ; Weiterrücken bis ans Ende der ~{...~}-Direktive
    (let* ((min-1-iteration (csd-colon-p (car *FORMAT-CSDL*)))
           (inner-cs (if (eq (cdr total-csdl) *FORMAT-CSDL*)
                       (next-arg)
                       *FORMAT-CS*
           )         )
           (inner-csdl (if (stringp inner-cs)
                         (if (eq (cdr total-csdl) *FORMAT-CSDL*)
                           (let ((node (list inner-cs)))
                             (format-parse-cs inner-cs 0 node nil)
                             (cdr node)
                           )
                           (cdr total-csdl)
           )           ) )
           (arg-list-rest (if (not atsign-modifier)
                            (let ((arg (next-arg)))
                              (unless (listp arg)
                                (format-error *FORMAT-CS* nil
                                  (ENGLISH "The ~~{ directive requires a list argument, not ~S")
                                  arg
                              ) )
                              arg
          ))              ) )
      (do* ((iteration-count 0 (1+ iteration-count)))
           ((or (and max-iteration-count
                     (>= iteration-count max-iteration-count)
                )
                (let ((remaining (if atsign-modifier
                                   *FORMAT-NEXT-ARG*
                                   arg-list-rest
                     ))          )
                  (if min-1-iteration
                    (and (plusp iteration-count) (null remaining))
                    (null remaining)
           ))   ) )
        (if (stringp inner-cs)
          (if colon-modifier
            (let* ((*FORMAT-ARG-LIST*
                     (if atsign-modifier (next-arg) (pop arg-list-rest))
                   )
                   (*FORMAT-NEXT-ARGLIST* ; für ~:^
                     (if atsign-modifier *FORMAT-NEXT-ARG* arg-list-rest)
                   )
                   (*FORMAT-NEXT-ARG* *FORMAT-ARG-LIST*)
                   (*FORMAT-CS* inner-cs)
                   (*FORMAT-CSDL* inner-csdl)
                   (*FORMAT-UP-AND-OUT* nil))
              (format-interpret stream 'FORMAT-ITERATION-END)
              (when (eq *FORMAT-UP-AND-OUT* ':TERMINATE-ALL) (return))
            )
            (if atsign-modifier
              (let* (; CLtL2 S. 598: "When within a ~{ construct, the "goto" is
                     ; relative to the list of arguments being processed by the
                     ; iteration." Soll das heißen, dass man bei ~@{ zu Beginn
                     ; jeder Iteration *FORMAT-ARG-LIST* neu binden muss ??
                     ; (*FORMAT-ARG-LIST* *FORMAT-NEXT-ARG*) ??
                     (*FORMAT-CS* inner-cs)
                     (*FORMAT-CSDL* inner-csdl)
                     (*FORMAT-UP-AND-OUT* nil))
                (format-interpret stream 'FORMAT-ITERATION-END)
                (when (eq *FORMAT-UP-AND-OUT* ':TERMINATE-ALL) (return))
              )
              (let* ((*FORMAT-ARG-LIST* arg-list-rest)
                     (*FORMAT-NEXT-ARG* *FORMAT-ARG-LIST*)
                     (*FORMAT-CS* inner-cs)
                     (*FORMAT-CSDL* inner-csdl)
                     (*FORMAT-UP-AND-OUT* nil))
                (format-interpret stream 'FORMAT-ITERATION-END)
                (setq arg-list-rest *FORMAT-NEXT-ARG*)
                (when (eq *FORMAT-UP-AND-OUT* ':TERMINATE-ALL) (return))
          ) ) )
          ; inner-cs may be a function in the ~{~} case
          (if (functionp inner-cs)
            (if colon-modifier
              (let* ((arglist (if atsign-modifier (next-arg) (pop arg-list-rest)))
                     (*FORMAT-CS* nil))
                (apply inner-cs stream arglist)
              )
              (if atsign-modifier
                (setq *FORMAT-NEXT-ARG*
                  (let ((*FORMAT-CS* nil))
                    (apply inner-cs stream *FORMAT-NEXT-ARG*)
                ) )
                (setq arg-list-rest
                  (let ((*FORMAT-CS* nil))
                    (apply inner-cs stream arg-list-rest)
            ) ) ) )
            (format-indirection-cserror inner-cs)
) ) ) ) ) )

; ~<, CLTL S.404-406, CLtL2 S. 604-605
(defun format-justification (stream colon-modifier atsign-modifier
       &optional (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (let* ((saved-csdl *FORMAT-CSDL*)
         (pos (sys::line-position stream))
         (tempstream (make-string-output-stream :line-position pos))
         (check-on-line-overflow nil)
         supplementary-need
         line-length
         (old-piecelist
           (let ((pieces nil))
             (do ((first-piece-flag t nil))
                 ((eq (csd-data (car *FORMAT-CSDL*)) 'FORMAT-JUSTIFICATION-END))
               (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
               (let ((*FORMAT-UP-AND-OUT* nil))
                 (format-interpret tempstream 'FORMAT-JUSTIFICATION-END)
                 (when (and first-piece-flag (eq (csd-data (car *FORMAT-CSDL*)) 'FORMAT-SEPARATOR))
                   (when (setq check-on-line-overflow (csd-colon-p (car *FORMAT-CSDL*)))
                     (multiple-value-setq (supplementary-need line-length)
                       (values-list (format-resolve-parms (car *FORMAT-CSDL*)))
                 ) ) )
                 (when *FORMAT-UP-AND-OUT*
                   (setq *FORMAT-CSDL* saved-csdl)
                   (format-skip-to-end)
                   (return)
                 )
                 (push (get-output-stream-string tempstream) pieces)
             ) )
             (nreverse pieces)
        )) )
    (do-format-justification stream colon-modifier atsign-modifier
                             mincol colinc minpad padchar
                             pos check-on-line-overflow
                             (if check-on-line-overflow (car old-piecelist))
                             supplementary-need line-length
                             (if check-on-line-overflow (cdr old-piecelist) old-piecelist)
) ) )
(defun do-format-justification (stream colon-modifier atsign-modifier
                                mincol colinc minpad padchar
                                pos check-on-line-overflow firstpiece
                                supplementary-need line-length piecelist)
  (if (null mincol) (setq mincol 0))
  (if (null colinc) (setq colinc 1))
  (if (null minpad) (setq minpad 0))
  (if (null padchar) (setq padchar #\Space))
  (if piecelist
    (multiple-value-bind (padblocklengths width)
      (format-justified-segments mincol colinc minpad
        colon-modifier atsign-modifier piecelist)
      (when (and check-on-line-overflow
                 (> (+ (or pos 0) width (or supplementary-need 0))
                    (or line-length #|(sys::line-length stream)|# 72)
            )    )
        (write-string firstpiece stream)
      )
      (do ((i 0 (1+ i)))
          (nil)
        (when (svref padblocklengths i)
          (format-padding (svref padblocklengths i) padchar stream)
        )
        (when (null piecelist) (return))
        (write-string (pop piecelist) stream)
    ) )
    (format-padding mincol padchar stream)
) )

; ~^, CLTL S.406-407, CLtL2 S. 605-606
(defun format-up-and-out (stream colon-modifier atsign-modifier
                          &optional (a nil) (b nil) (c nil))
  (declare (ignore stream atsign-modifier))
  (if (up-and-out-p a b c
        (if colon-modifier *FORMAT-NEXT-ARGLIST* *FORMAT-NEXT-ARG*)
      )
    (setq *FORMAT-UP-AND-OUT* (if colon-modifier ':TERMINATE-ALL ':TERMINATE))
) )
(defun up-and-out-p (a b c &optional args)
  (cond ((and (null a) (null b) (null c)) ; keine Parameter
         (null args)
        )
        ((and (null b) (null c)) (eql a 0)) ; ein Parameter
        ((null c) (eql a b)) ; zwei Parameter
        ((and (integerp a) (integerp b) (integerp c)) (<= a b c))
        ((and (characterp a) (characterp b) (characterp c)) (char<= a b c))
) )

; ~!, a CLISP extension as a replacement for badly designed ~/.../
(defun format-call (stream colon-modifier atsign-modifier &rest more-args)
  (apply (next-arg) stream (next-arg) colon-modifier atsign-modifier more-args)
)

;-------------------------------------------------------------------------------

;; FORMATTER - Compilation von FORMAT-Strings.


; Fall-back function if control-string cannot be compiled.
(defun formatter-hairy (control-string)
  ; control-string is known to be a string
  #'(lambda (stream &rest args)
      (let ((node (list control-string)))
        (format-parse-cs control-string 0 node nil)
        (let* ((*FORMAT-CS*         (car node))
               (*FORMAT-CSDL*       (cdr node))
               (*FORMAT-ARG-LIST*   args)
               (*FORMAT-NEXT-ARG*   *FORMAT-ARG-LIST*)
               (*FORMAT-NEXT-ARGLIST* nil)
               (*FORMAT-UP-AND-OUT* nil))
          (format-interpret stream)
          *FORMAT-NEXT-ARG*
    ) ) )
)


; Block für ~^
(defvar *format-terminate*)
; Block für ~:^
(defvar *format-terminate-all*)

; Der Block wird nur bei Bedarf bereitgestellt.
; Um unnötige UNWIND-PROTECTs zu vermeiden, wird eine Liste der anhängigen
; UNWIND-PROTECTs geführt. Jeder Blockname (ein Gensym) enthält einen Verweis
; auf diese Liste zum Zeitpunkt seiner Bildung.

; Liste der anhängigen UNWIND-PROTECTs
(defvar *format-uwps*)

(defun formatter-block (prefix)
  (let ((sym (gensym prefix)))
    (setf (get sym 'uwps) *format-uwps*)
    sym
) )

(flet ((mark-used (blockname)
         ; Markiere den Block, so dass er nicht wegoptimiert wird.
         (setf (get blockname 'used) t)
         ; Markiere alle übersprungenen UNWIND-PROTECTs, so dass sie nicht
         ; wegoptimiert werden.
         (do ((L1 *format-uwps* (cdr L1))
              (L2 (get blockname 'uwps)))
             ((eq L1 L2))
           (setf (car L1) 'T)
         )
         blockname
      ))
  (defun formatter-terminate ()
    (mark-used *format-terminate*)
  )
  (defun formatter-terminate-all ()
    (mark-used *format-terminate-all*)
  )
)

(defmacro formatter-bind-terminator (&body body)
  `(let ((*format-terminate* (formatter-block "TERMINATE-")))
     (formatter-bind-terminator-1 (progn ,@body))
   )
)
(defun formatter-bind-terminator-1 (forms)
  (when (get *format-terminate* 'used)
    (setq forms `((BLOCK ,*format-terminate* ,@forms)))
  )
  forms
)

(defmacro formatter-bind-terminators (&body body)
  `(let ((*format-terminate* (formatter-block "TERMINATE-"))
         (*format-terminate-all* (formatter-block "TERMINATE-ALL-")))
     (formatter-bind-terminators-1 (progn ,@body))
   )
)
(defun formatter-bind-terminators-1 (forms)
  (when (get *format-terminate* 'used)
    (setq forms `((BLOCK ,*format-terminate* ,@forms)))
  )
  (when (get *format-terminate-all* 'used)
    (setq forms `((BLOCK ,*format-terminate-all* ,@forms)))
  )
  forms
)


; Flag, ob innerhalb von ~(...~)
(defvar *format-case*)


; Wegen ~:^ kann die Argumentliste nicht immer denselben Namen ARGS haben.
; Ihr Name.
(defvar *args*)

; Name der Argumentliste der umschließenden ~:{ Iteration.
(defvar *iterargs*)


; Zugriff auf die normale Argumentliste:
; Normalfall:
;   Argumentliste &REST ARGS,
;   Zugriff auf das nächste Element ist (POP ARGS),
;   ~# ist (LENGTH ARGS),
;   Gesamtliste für ~:* ist WHOLE-ARGS.
; Optimiert, falls kein (LENGTH ARGS) und kein WHOLE-ARGS nötig ist:
;   Argumentliste #:ARG1 #:ARG2 ... &REST ARGS
;   Zugriff auf das nächste Element ist #:ARGi oder (POP ARGS).

; Flag, das anzeigt, ob man sich noch in der linearen Abarbeitungsphase der
; Argumente befindet (jedes genau einmal, bekannte Position).
(defvar *formatter-linear-args*)

; Anzahl der Argumente, die bisher zur linearen Abarbeitungsphase gehören.
; Wichtig: Diese kann hinterher erniedrigt werden!!
(defvar *formatter-linear-argcount*)

; Position in der Argumentliste während der linearen Abarbeitungsphase.
; Stets <= *formatter-linear-argcount*.
(defvar *formatter-linear-position*)

; Flag, ob WHOLE-ARGS gebunden werden soll.
(defvar *formatter-whole-args*)

; Beginnt eine Iteration, die ARGS und evtl. WHOLE-ARGS bindet.
(defmacro formatter-bind-args (&body body)
  `(let ((*args* (gensym "ARGS"))
         (*formatter-linear-args* t)
         (*formatter-linear-argcount* 0)
         (*formatter-linear-position* 0)
         (*formatter-whole-args* nil))
     (formatter-bind-args-1 (progn ,@body))
   )
)
(defun formatter-bind-args-1 (forms)
  (when *formatter-whole-args*
    (subst-if-then #'(lambda (x) ; x = `(WHOLE-ARGS ,i)
                       (setq *formatter-linear-argcount*
                             (min *formatter-linear-argcount* (second x))
                     ) )
                   #'(lambda (x) ; x = `(WHOLE-ARGS ,i) ?
                       (and (consp x) (eq (car x) 'WHOLE-ARGS)
                            (consp (cdr x)) (numberp (cadr x)) (null (cddr x))
                     ) )
                   forms
  ) )
  (let ((argsyms nil))
    (dotimes (i *formatter-linear-argcount*) (push (gensym "ARG") argsyms))
    (setq argsyms (nreverse argsyms))
    (setq forms
      (subst-if-then #'(lambda (x) ; x = `(ARG ,i)
                         (if (< (second x) *formatter-linear-argcount*)
                           (nth (second x) argsyms)
                           `(POP ,*args*)
                       ) )
                     #'(lambda (x) ; x = `(ARG ,i) ?
                         (and (consp x) (eq (car x) 'ARG) (consp (cdr x)) (null (cddr x)))
                       )
                     forms
    ) )
    (setq forms
      (subst-if-then #'(lambda (x) ; x = `(SETQ-ARGS-WHOLE-ARGS ,old-pos ,new-pos)
                         (let ((old-pos (second x)) (new-pos (third x)))
                           (if (<= old-pos *formatter-linear-argcount*)
                             ; no need for WHOLE-ARGS since ARGS = WHOLE-ARGS at this point
                             (if (<= new-pos *formatter-linear-argcount*)
                               `(PROGN)
                               `(SETQ ,*args* (NTHCDR ,(- new-pos *formatter-linear-argcount*) ,*args*))
                             )
                             (progn
                               (setq *formatter-whole-args* t)
                               `(SETQ ,*args* (WHOLE-ARGS ,(max new-pos *formatter-linear-argcount*)))
                       ) ) ) )
                     #'(lambda (x) ; x = `(SETQ-ARGS-WHOLE-ARGS ,i ,j) ?
                         (and (consp x) (eq (car x) 'SETQ-ARGS-WHOLE-ARGS)
                              (consp (cdr x)) (consp (cddr x)) (null (cdddr x)))
                       )
                     forms
    ) )
    (when *formatter-whole-args*
      (setq forms
        (subst-if-then #'(lambda (x) ; x = `(WHOLE-ARGS ,i)
                           (let ((i (- (second x) *formatter-linear-argcount*)))
                             (if (zerop i)
                               `WHOLE-ARGS
                               `(NTHCDR ,i WHOLE-ARGS)
                         ) ) )
                       #'(lambda (x) ; x = `(WHOLE-ARGS ,i) ?
                           (and (consp x) (eq (car x) 'WHOLE-ARGS)
                                (consp (cdr x)) (numberp (cadr x)) (null (cddr x))
                         ) )
                       forms
      ) )
      (setq forms `((LET ((WHOLE-ARGS ,*args*)) ,@forms)))
    )
    (values `(,@argsyms &REST ,*args*)
            `((DECLARE (IGNORABLE ,@argsyms ,*args*)) ,@forms)
) ) )

; Beendet den linearen Modus. Ab jetzt kann auf die Argumentliste
; als ARGS zugegriffen werden.
(defun formatter-stop-linear ()
  (when *formatter-linear-args*
    (setq *formatter-linear-argcount*
          (min *formatter-linear-argcount* *formatter-linear-position*)
    )
    ; Jetzt ist *formatter-linear-argcount* = *formatter-linear-position*.
    (setq *formatter-linear-args* nil)
) )

; Holt eine Form, die die Länge der Argumentliste liefert.
(defun formatter-length-args ()
  (formatter-stop-linear)
  `(LENGTH ,*args*)
)

; Holt eine Form für das nächste Argument.
; Diese Form muss nachher mit SUBST ersetzt werden.
(defun formatter-next-arg ()
  (if *formatter-linear-args*
    (prog1
      `(ARG ,*formatter-linear-position*)
      (incf *formatter-linear-position*)
      (setq *formatter-linear-argcount*
            (max *formatter-linear-argcount* *formatter-linear-position*)
    ) )
    `(POP ,*args*)
) )

; Holt eine Form, die ein nthcdr der ganzen Argumentliste liefert.
; Diese Form muss nachher mit SUBST ersetzt werden.
(defun formatter-whole-args (n)
  (formatter-stop-linear)
  (setq *formatter-whole-args* t)
  `(WHOLE-ARGS ,n)
)

; Holt eine Formenliste zum Überspringen (vor/zurück) von Argumenten.
(defun formatter-goto-arg (absolute-p backward-p n)
  (if absolute-p
    ; im einfachsten Fall: (setq args (nthcdr n whole-args))
    (if (numberp n)
      (progn
        (setq n (max n 0))
        (if *formatter-linear-args*
          (if (< n *formatter-linear-position*)
            (prog1
              `((SETQ-ARGS-WHOLE-ARGS ,*formatter-linear-position* ,n))
              (setq *formatter-linear-position* n)
            )
            ; n >= *formatter-linear-position*
            (formatter-goto-arg nil nil (- n *formatter-linear-position*))
          )
          (progn
            (formatter-stop-linear)
            `((SETQ ,*args* ,(formatter-whole-args n)))
      ) ) )
      (progn
        (formatter-stop-linear)
        `((SETQ ,*args* (NTHCDR ,n ,(formatter-whole-args 0))))
    ) )
    (if backward-p
      ; im einfachsten Fall:
      ; (setq args (nthcdr (max (- (length whole-args) (length args) n) 0) whole-args))
      (if (and (numberp n) *formatter-linear-args*)
        (formatter-goto-arg t nil (- *formatter-linear-position* n))
        (progn
          (formatter-stop-linear)
          `((SETQ ,*args* ,(if *formatter-linear-args*
                             `(NTHCDR (MAX (- ,*formatter-linear-position* ,n) 0) ,(formatter-whole-args 0))
                             `(LIST-BACKWARD ,n ; n zuerst auswerten, da es (POP ARGS) enthalten kann
                                ,(formatter-whole-args 0) ,*args*
                              )
                           )
           ))
      ) )
      ; im einfachsten Fall: (setq args (nthcdr n args))
      (if (and (numberp n) (<= n 100) *formatter-linear-args*)
        (do ((l '() (cons (formatter-next-arg) l)) (i 0 (1+ i)))
            ((>= i n) (nreverse l))
        )
        (progn
          (formatter-stop-linear)
          `((SETQ ,*args* (NTHCDR ,n ,*args*)))
      ) )
) ) )
(defun list-backward (n whole-list list)
  (nthcdr (max (- (length whole-list) (length list) n) 0) whole-list)
)

; Holt eine Form, der ein Direktiven-Argument liefert.
(defun formatter-arg (arg)
  (case arg
    (:NEXT-ARG (formatter-next-arg))
    (:ARG-COUNT (formatter-length-args))
    (T ; arg ist NIL oder Integer oder Character, braucht nicht quotiert zu werden.
       arg
) ) )


; Haupt-Compilier-Funktion. Liefert eine Formenliste.
; Fluid übergeben: *format-cs* und *format-csdl* (wird weitergerückt).
(defun formatter-main-1 (&optional (endmarker nil))
  (let ((forms '()))
    (loop
      (when (endp *format-csdl*) (return))
      (let ((csd (car *format-csdl*)))
        (case (csd-type csd)
          (0 )
          (1 (push (subseq *format-cs* (csd-cs-index csd) (csd-data csd))
                   forms
          )  )
          (2 (let ((directive-name (csd-data csd)))
               (if (eq directive-name endmarker) (return))
               (if (eq directive-name 'FORMAT-SEPARATOR) (return))
               (let ((colon-p (csd-colon-p csd))
                     (atsign-p (csd-atsign-p csd))
                     (arglist (mapcar #'formatter-arg (csd-parm-list csd)))
                    )
                 (labels ((simple-arglist (n)
                            (unless (<= (length arglist) n)
                              (format-error *format-cs* nil
                                (ENGLISH "Too many arguments for this directive")
                            ) )
                            (setq arglist
                                  (append arglist
                                          (make-list (- n (length arglist))
                                                     :initial-element 'NIL
                          ) )     )       )
                          (trivial-call ()
                            (push `(,directive-name
                                    STREAM
                                    ,colon-p
                                    ,atsign-p
                                    ,@arglist
                                   )
                                  forms
                          ) )
                          (trivial (n)
                            (simple-arglist n)
                            (trivial-call)
                          )
                          (simple-call ()
                            (push `(,(intern (string-concat "DO-" (string directive-name))
                                             (find-package "SYSTEM")
                                     )
                                    STREAM
                                    ,colon-p
                                    ,atsign-p
                                    ,@arglist
                                    ; Pass the actual argument last because
                                    ; ,@arglist may contain `(POP ,*args*) as well.
                                    ,(formatter-next-arg)
                                   )
                                  forms
                          ) )
                          (simple (n)
                            (simple-arglist n)
                            (simple-call)
                         ))
                   (case directive-name
                     (FORMAT-ASCII                  ; #\A
                       (simple-arglist 4)
                       (if (and (member (first arglist) '(nil 0)) ; mincol
                                (member (third arglist) '(nil 0)) ; minpad
                           )
                         (progn
                           (setq forms (revappend (remove 'NIL arglist) forms))
                           (push `(PRINC ,(if colon-p
                                            `(OR ,(formatter-next-arg) "()")
                                            (formatter-next-arg)
                                          )
                                         STREAM
                                  )
                                 forms
                         ) )
                         (simple-call)
                     ) )
                     (FORMAT-S-EXPRESSION           ; #\S
                       (simple-arglist 4)
                       (if (and (member (first arglist) '(nil 0)) ; mincol
                                (member (third arglist) '(nil 0)) ; minpad
                                (not colon-p)
                           )
                         (progn
                           (setq forms (revappend (remove 'NIL arglist) forms))
                           (push `(PRIN1 ,(formatter-next-arg) STREAM) forms)
                         )
                         (simple-call)
                     ) )
                     (FORMAT-WRITE                  ; #\W
                       (simple-arglist 4)
                       (if (and (member (first arglist) '(nil 0)) ; mincol
                                (member (third arglist) '(nil 0)) ; minpad
                           )
                         (progn
                           (setq forms (revappend (remove 'NIL arglist) forms))
                           (push `(WRITE ,(formatter-next-arg) :STREAM STREAM) forms)
                         )
                         (simple-call)
                     ) )
                     (FORMAT-DECIMAL                ; #\D
                       (simple 4) )
                     (FORMAT-BINARY                 ; #\B
                       (simple 4) )
                     (FORMAT-OCTAL                  ; #\O
                       (simple 4) )
                     (FORMAT-HEXADECIMAL            ; #\X
                       (simple 4) )
                     (FORMAT-RADIX                  ; #\R
                       (simple-arglist 5)
                       (if (and (null (first arglist)) (not atsign-p))
                         (progn
                           (setq forms (revappend (remove 'NIL arglist) forms))
                           (push `(,(if colon-p 'FORMAT-ORDINAL 'FORMAT-CARDINAL)
                                   ,(formatter-next-arg) STREAM
                                  )
                                 forms
                         ) )
                         (simple-call)
                     ) )
                     (FORMAT-PLURAL                 ; #\P
                       (simple-arglist 0)
                       (when colon-p
                         (setq forms (revappend (formatter-goto-arg nil t 1) forms))
                       )
                       (push (if atsign-p
                               `(WRITE-STRING
                                  (IF (EQL ,(formatter-next-arg) 1) "y" "ies")
                                  STREAM
                                )
                               `(UNLESS (EQL ,(formatter-next-arg) 1)
                                  (WRITE-CHAR #\s STREAM)
                                )
                             )
                             forms
                     ) )
                     (FORMAT-CHARACTER              ; #\C
                       (simple 0) )
                     (FORMAT-FIXED-FLOAT            ; #\F
                       (simple 5) )
                     (FORMAT-EXPONENTIAL-FLOAT      ; #\E
                       (simple 7) )
                     (FORMAT-GENERAL-FLOAT          ; #\G
                       (simple 7) )
                     (FORMAT-DOLLARS-FLOAT          ; #\$
                       (simple 4) )
                     (FORMAT-TERPRI                 ; #\%
                       (simple-arglist 1)
                       (if (member (first arglist) '(nil 1))
                         (push #\Newline forms) ; equiv. to `(TERPRI STREAM)
                         (trivial-call)
                     ) )
                     (FORMAT-FRESH-LINE             ; #\&
                       (simple-arglist 1)
                       (if (member (first arglist) '(nil 1))
                         (push `(FRESH-LINE STREAM) forms)
                         (trivial-call)
                     ) )
                     (FORMAT-PAGE                   ; #\|
                       (simple-arglist 1)
                       (if (member (first arglist) '(nil 1))
                         (push #\Page forms)
                         (trivial-call)
                     ) )
                     (FORMAT-TILDE                  ; #\~
                       (simple-arglist 1)
                       (if (member (first arglist) '(nil 1))
                         (push #\~ forms)
                         (trivial-call)
                     ) )
                     (FORMAT-TABULATE               ; #\T
                       (trivial 2) )
                     (FORMAT-GOTO                   ; #\*
                       (simple-arglist 1)
                       (setq forms
                             (revappend
                               (formatter-goto-arg atsign-p colon-p
                                 (or (first arglist) (if atsign-p 0 1))
                               )
                               forms
                     ) )     )
                     (FORMAT-INDIRECTION            ; #\?
                       (simple-arglist 0)
                       (if atsign-p
                         (push `(SETQ ,*args*
                                  (DO-FORMAT-INDIRECTION STREAM
                                    ,(formatter-next-arg)
                                    ,(progn (formatter-stop-linear) `,*args*)
                                ) )
                               forms
                         )
                         (push `(DO-FORMAT-INDIRECTION STREAM ,(formatter-next-arg) ,(formatter-next-arg))
                               forms
                     ) ) )
                     (FORMAT-CALL-USER-FUNCTION     ; #\/
                       (let* ((func (car (pop arglist)))
                              (argsvars
                               (mapcar #'(lambda (arg) (declare (ignore arg)) (gensym))
                                       arglist))
                              (inner-form
                               `(,func STREAM ,(formatter-next-arg) ,colon-p ,atsign-p
                                       ,@argsvars
                                )
                             ))
                         (push (if argsvars
                                 `(LET ,(mapcar #'list argsvars arglist) ,inner-form)
                                 inner-form)
                               forms
                     ) ) )
                     (FORMAT-CASE-CONVERSION        ; #\(
                       (simple-arglist 0)
                       (setq *format-csdl* (cdr *format-csdl*))
                       (if *format-case*
                         ; Richard Waters notes: It is possible for ~(...~) to
                         ; be nested in a format string, but note that inner
                         ; nested modes never have any effect. You can just
                         ; ignore them.
                         (let ((inner-forms
                                 ; no need to bind *format-case* to t here
                                 (formatter-main-1 'FORMAT-CASE-CONVERSION-END)
                              ))
                           (setq forms (revappend inner-forms forms))
                         )
                         (push `(LET ((ORIG-STREAM STREAM)
                                      (STREAM (MAKE-STRING-OUTPUT-STREAM :LINE-POSITION (SYS::LINE-POSITION STREAM))))
                                  ,@(let* ((*format-uwps* (cons 'NIL *format-uwps*))
                                           (inner-forms
                                             (let ((*format-case* t))
                                               (formatter-main 'FORMAT-CASE-CONVERSION-END)
                                           ) )
                                           (cleanup-forms
                                             `((WRITE-STRING
                                                 (,(if colon-p
                                                     (if atsign-p
                                                       'NSTRING-UPCASE
                                                       'NSTRING-CAPITALIZE
                                                     )
                                                     (if atsign-p
                                                       'SYS::NSTRING-CAPITALIZE1
                                                       'NSTRING-DOWNCASE
                                                   ) )
                                                  (GET-OUTPUT-STREAM-STRING STREAM)
                                                 )
                                                 ORIG-STREAM
                                              ))
                                          ))
                                      (if (car *format-uwps*)
                                        `((UNWIND-PROTECT (PROGN ,@inner-forms) ,@cleanup-forms))
                                        `(,@inner-forms ,@cleanup-forms)
                                    ) )
                                )
                               forms
                     ) ) )
                     (FORMAT-CONDITIONAL            ; #\[
                       (if colon-p
                         (if atsign-p
                           (format-conditional-error)
                           (progn
                             (simple-arglist 0)
                             (push `(IF (NOT ,(formatter-next-arg))
                                      (PROGN ,@(progn
                                                 (formatter-stop-linear)
                                                 (setq *format-csdl* (cdr *format-csdl*))
                                                 (formatter-main 'FORMAT-CONDITIONAL-END)
                                      )        )
                                      (PROGN ,@(progn
                                                 (formatter-stop-linear)
                                                 (setq *format-csdl* (cdr *format-csdl*))
                                                 (formatter-main 'FORMAT-CONDITIONAL-END)
                                    ) )        )
                                   forms
                         ) ) )
                         (if atsign-p
                           (progn
                             (simple-arglist 0)
                             (formatter-stop-linear)
                             (push `(IF (CAR ,*args*)
                                      (PROGN ,@(progn
                                                 (setq *format-csdl* (cdr *format-csdl*))
                                                 (formatter-main 'FORMAT-CONDITIONAL-END)
                                      )        )
                                      (SETQ ,*args* (CDR ,*args*))
                                    )
                                   forms
                             )
                             (unless (null (csd-clause-chain (car *format-csdl*)))
                               (format-error *format-cs* nil
                                 (ENGLISH "The ~~; directive is not allowed at this point.")
                           ) ) )
                           (progn
                             (simple-arglist 1)
                             (push `(CASE ,(or (first arglist) (formatter-next-arg))
                                      ,@(let ((index 0) (cases '()))
                                          (formatter-stop-linear)
                                          (loop
                                            (when (null (csd-clause-chain (car *format-csdl*)))
                                              (return)
                                            )
                                            (when (csd-colon-p (car *format-csdl*))
                                              (setq index 'T)
                                            )
                                            (setq *format-csdl* (cdr *format-csdl*))
                                            (push `(,index ,@(formatter-main 'FORMAT-CONDITIONAL-END))
                                                  cases
                                            )
                                            (if (eq index 'T) (return) (incf index))
                                          )
                                          (nreverse cases)
                                        )
                                    )
                                   forms
                     ) ) ) ) )
                     (FORMAT-ITERATION              ; #\{
                       (simple-arglist 1)
                       (setq *format-csdl* (cdr *format-csdl*))
                       (let ((max-n-iterations (first arglist))
                             (min-1-iteration (csd-colon-p (car (csd-clause-chain csd))))
                             (indirect (eq (csd-clause-chain csd) *format-csdl*)))
                         (flet ((compute-innermost ()
                                  (if indirect
                                    (progn
                                      (formatter-stop-linear)
                                      `((SETQ ,*args*
                                          (DO-FORMAT-INDIRECTION-2 STREAM NODE
                                                                   ,*args* ,(formatter-whole-args 0)
                                       )) )
                                    )
                                    (formatter-main 'FORMAT-ITERATION-END)
                               )) )
                           (flet ((compute-inner ()
                                    (if colon-p
                                      (let ((*iterargs* *args*))
                                        (formatter-bind-terminator
                                          (multiple-value-bind (lambdalist innermost)
                                              (formatter-bind-args (compute-innermost))
                                            `((APPLY #'(LAMBDA ,lambdalist ,@innermost)
                                               ,(formatter-next-arg)
                                             ))
                                      ) ) )
                                      (let ((*iterargs* nil))
                                        ; CLtL2 S. 598: "When within a ~{ construct, the "goto" is
                                        ; relative to the list of arguments being processed by the
                                        ; iteration." Soll das heißen, dass man bei ~@{ zu Beginn
                                        ; jeder Iteration WHOLE-ARGS neu an ARGS binden muss ??
                                        ; (if atsign-p
                                        ;   (progn (formatter-stop-linear)
                                        ;     `((LET ((WHOLE-ARGS ,*args*)) ,@(compute-innermost)))
                                        ;   )
                                        ;   (compute-innermost)
                                        ; )
                                        (compute-innermost)
                                 )) ) )
                             (flet ((compute-middle ()
                                      (if (eql max-n-iterations 0)
                                        '()
                                        (progn
                                          (unless (and (eql max-n-iterations 1) min-1-iteration)
                                            (formatter-stop-linear)
                                          )
                                          (if (eql max-n-iterations 1)
                                            (if min-1-iteration
                                              (compute-inner)
                                              `((UNLESS (ENDP ,*args*) ,@(compute-inner)))
                                            )
                                            `((BLOCK NIL
                                                (TAGBODY
                                                  L
                                                  ,@(if max-n-iterations
                                                      `((WHEN (>= I N) (RETURN)) (INCF I))
                                                    )
                                                  ,@(if (not min-1-iteration)
                                                      `((WHEN (ENDP ,*args*) (RETURN)))
                                                    )
                                                  ,@(compute-inner)
                                                  ,@(if min-1-iteration
                                                      `((WHEN (ENDP ,*args*) (RETURN)))
                                                    )
                                                  (GO L)
                                             )) )
                                   )) ) ) )
                               (flet ((compute-outer ()
                                        (formatter-bind-terminators
                                          ; *format-terminate-all* und *format-terminate* werden
                                          ; gebunden, aber falls colon-p, wird *format-terminate*
                                          ; weiter innen verschattet (s.o.).
                                          (if atsign-p
                                            (compute-middle)
                                            (multiple-value-bind (lambdalist inner-forms)
                                                (formatter-bind-args (compute-middle))
                                              `((APPLY #'(LAMBDA ,lambdalist ,@inner-forms)
                                                 ,(formatter-next-arg)
                                               ))
                                     )) ) ) )
                                 (flet ((compute-outermost ()
                                          (if indirect
                                            `((LET ((NODE (DO-FORMAT-INDIRECTION-1 ,(formatter-next-arg))))
                                                ,@(compute-outer)
                                             ))
                                            (compute-outer)
                                       )) )
                                   (let ((new-forms
                                           (if (and max-n-iterations (not (member max-n-iterations '(0 1))))
                                             `((LET ((N ,(first arglist)) (I 0))
                                                 ,@(compute-outermost)
                                              ))
                                              (compute-outermost)
                                        )) )
                                     (setq forms (revappend new-forms forms))
                     ) ) ) ) ) ) ) )
                     (FORMAT-JUSTIFICATION          ; #\<
                       (simple-arglist 4)
                       (let* ((firstseparator (car (csd-clause-chain csd)))
                              (check-on-line-overflow
                                (and (eq (csd-data firstseparator) 'FORMAT-SEPARATOR)
                                     (csd-colon-p firstseparator)
                              ) )
                              (bindings
                                `((POS (SYS::LINE-POSITION STREAM))
                                  (ORIG-STREAM STREAM)
                                  (STREAM (MAKE-STRING-OUTPUT-STREAM :LINE-POSITION POS))
                                 )
                              )
                              (justify-args
                                `(ORIG-STREAM
                                  ,colon-p
                                  ,atsign-p
                                  ,@arglist
                                  POS
                                  ,check-on-line-overflow
                                  ,(when check-on-line-overflow
                                     (setq *format-csdl* (cdr *format-csdl*))
                                     `(PROGN
                                        ,@(formatter-main 'FORMAT-JUSTIFICATION-END)
                                        (GET-OUTPUT-STREAM-STRING STREAM)
                                      )
                                   )
                                  ,(when check-on-line-overflow
                                     (formatter-arg (first (csd-parm-list firstseparator)))
                                   )
                                  ,(when check-on-line-overflow
                                     (formatter-arg (second (csd-parm-list firstseparator)))
                                   )
                                 )
                              )
                              (*format-uwps* (cons 'NIL *format-uwps*))
                              (pieces-forms '())
                             )
                         (loop
                           (when (null (csd-clause-chain (car *format-csdl*))) (return))
                           (setq *format-csdl* (cdr *format-csdl*))
                           (push (formatter-main 'FORMAT-JUSTIFICATION-END) pieces-forms)
                         )
                         (setq pieces-forms (nreverse pieces-forms))
                         (push
                           (if (car *format-uwps*)
                             `(LET* (,@bindings
                                     (JARGS (LIST ,@justify-args))
                                     (PIECES '()))
                                (UNWIND-PROTECT
                                  (PROGN
                                    ,@(mapcap #'(lambda (piece-forms)
                                                  `(,@piece-forms
                                                    (PUSH (GET-OUTPUT-STREAM-STRING STREAM) PIECES)
                                                   )
                                                )
                                              pieces-forms
                                      )
                                  )
                                  (APPLY #'DO-FORMAT-JUSTIFICATION
                                         (NCONC JARGS (LIST (SYS::LIST-NREVERSE PIECES)))
                              ) ) )
                             `(LET* (,@bindings)
                                (DO-FORMAT-JUSTIFICATION
                                  ,@justify-args
                                  (LIST
                                    ,@(mapcar #'(lambda (piece-forms)
                                                  `(PROGN ,@piece-forms (GET-OUTPUT-STREAM-STRING STREAM))
                                                )
                                              pieces-forms
                                      )
                              ) ) )
                           )
                           forms
                     ) ) )
                     (FORMAT-UP-AND-OUT             ; #\^
                       (simple-arglist 3)
                       (formatter-stop-linear)
                       (let ((argsvar (if colon-p *iterargs* *args*)))
                         (push `(IF ,(if (some #'(lambda (x) (and (constantp x) x)) arglist)
                                       `(UP-AND-OUT-P ,@arglist)
                                       (if (and (null (second arglist)) (null (third arglist)))
                                         (let ((first-arg (first arglist)))
                                           (if (null first-arg)
                                             `(ENDP ,argsvar)
                                             (if (and (consp first-arg) (eq (car first-arg) 'LENGTH))
                                               `(ENDP ,(second first-arg)) ; (EQL (LENGTH x) 0) == (ENDP x)
                                               `(CASE ,first-arg ((NIL) (ENDP ,argsvar)) ((0) T) (T NIL))
                                         ) ) )
                                         `(UP-AND-OUT-P ,@arglist ,argsvar)
                                     ) )
                                  (RETURN-FROM ,(if colon-p (formatter-terminate-all) (formatter-terminate)))
                                )
                               forms
                     ) ) )
                     (FORMAT-CALL                   ; #\!
                       (let* ((argsvars (mapcar #'(lambda (arg) (declare (ignore arg)) (gensym)) arglist))
                              (inner-form
                                `(FUNCALL ,(formatter-next-arg)
                                          STREAM ,(formatter-next-arg) ,colon-p ,atsign-p
                                          ,@argsvars
                                 )
                             ))
                         (push (if argsvars
                                 `(LET ,(mapcar #'list argsvars arglist) ,inner-form)
                                 inner-form
                               )
                               forms
                     ) ) )
                     (t ; Huh? Someone implemented a new format directive,
                        ; but forgot it here! Bail out.
                        (throw 'formatter-hairy nil)
                     )
          )  ) ) ) )
      ) )
      (setq *format-csdl* (cdr *format-csdl*))
    )
    ; Combine adjacent strings:
    (let ((new-forms '()))
      (dolist (form forms)
        (when (characterp form) (setq form (string form)))
        (if (and (consp new-forms) (stringp (car new-forms)) (stringp form))
          (setf (car new-forms) (string-concat form (car new-forms)))
          (push form new-forms)
      ) )
      new-forms
) ) )
(defun formatter-main (&optional (endmarker nil))
  (let ((new-forms (formatter-main-1 endmarker)))
    ; Convert strings to WRITE-STRING forms:
    (mapcap #'(lambda (form)
                (if (stringp form)
                  (case (length form)
                    (0 )
                    (1 (setq form (char form 0))
                       `(,(if (eq form #\Newline)
                            `(TERPRI STREAM)
                            `(WRITE-CHAR ,form STREAM)
                    )   ) )
                    (t `((WRITE-STRING ,form STREAM)))
                  )
                  (list form)
              ) )
            new-forms
) ) )

;; FORMATTER, CLtL2 S. 764
(defmacro formatter (control-string)
  (unless (stringp control-string)
    (error-of-type 'type-error
      :datum control-string :expected-type 'string
      (ENGLISH "The control-string must be a string, not ~S")
      control-string
  ) )
  ; evtl. noch control-string zu einem Simple-String machen ??
  (or
    (catch 'formatter-hairy
      (let ((node (list control-string)))
        (format-parse-cs control-string 0 node nil)
        (let ((*FORMAT-CS* (car node))
              (*FORMAT-CSDL* (cdr node))
              (*format-case* nil)
              (*format-uwps* '())
              (*iterargs* nil))
          (multiple-value-bind (lambdalist forms)
              (formatter-bind-args
                `(,@(formatter-bind-terminators
                      (formatter-main)
                    )
                  ,(progn (formatter-stop-linear) `,*args*)
                 )
              )
            `(FUNCTION
               (LAMBDA (STREAM ,@lambdalist)
                 (DECLARE (IGNORABLE STREAM))
                 ,@forms
             ) )
    ) ) ) )
    `(FORMATTER-HAIRY ,(coerce control-string 'simple-string))
) )

;-------------------------------------------------------------------------------

