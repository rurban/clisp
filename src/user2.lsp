;;;; User-Interface, Teil 2
;;;; Funktionen fürs Debugging (Kapitel 25.3)
;;;; Apropos, Describe, Room, Dribble, Ed
;;;; 27.6.1992

(in-package "LISP")
(export '(*editor* editor-name editor-tempfile edit-file uncompile saveinitmem))
#+WIN32 (export '(execute))
#+(or UNIX OS/2 WIN32) (export '(run-shell-command run-program))
(in-package "SYSTEM")

;-------------------------------------------------------------------------------
;; APROPOS

(defun apropos-list (string &optional (package nil))
  (let* ((L nil)
         (fun #'(lambda (sym)
                  (when
                      #| (search string (symbol-name sym) :test #'char-equal) |#
                      (sys::search-string-equal string sym) ; 15 mal schneller!
                    (push sym L)
                ) )
        ))
    (if package
      (system::map-symbols fun package)
      (system::map-all-symbols fun)
    )
    (stable-sort (delete-duplicates L :test #'eq :from-end t)
                 #'string< :key #'symbol-name
    )
) )

(defun fbound-string (sym) ; liefert den Typ eines Symbols sym mit (fboundp sym)
  (cond ((special-operator-p sym)
         (DEUTSCH "Spezialform"
          ENGLISH "special form"
          FRANCAIS "forme spéciale")
        )
        ((functionp (symbol-function sym))
         (DEUTSCH "Funktion"
          ENGLISH "function"
          FRANCAIS "fonction")
        )
        (t (DEUTSCH "Macro"
            ENGLISH "macro"
            FRANCAIS "macro")
) )     )

(defun apropos (string &optional (package nil))
  (dolist (sym (apropos-list string package))
    (print sym)
    (when (fboundp sym)
      (write-string "   ")
      (write-string (fbound-string sym))
    )
    (when (boundp sym)
      (write-string "   ")
      (if (constantp sym)
        (write-string (DEUTSCH "Konstante"
                       ENGLISH "constant"
                       FRANCAIS "constante")
        )
        (write-string (DEUTSCH "Variable"
                       ENGLISH "variable"
                       FRANCAIS "variable")
    ) ) )
    (when (or (get sym 'system::type-symbol)
              (get sym 'system::defstruct-description)
          )
      (write-string "   ")
      (write-string (DEUTSCH "Typ"
                     ENGLISH "type"
                     FRANCAIS "type")
    ) )
    (when (get sym 'clos::closclass)
      (write-string "   ")
      (write-string (DEUTSCH "Klasse"
                     ENGLISH "class"
                     FRANCAIS "classe")
    ) )
  )
  (values)
)

;-------------------------------------------------------------------------------
;; DESCRIBE

(defun describe (obj &optional s &aux (more '()))
  (cond ((eq s 'nil) (setq s *standard-output*))
        ((eq s 't) (setq s *terminal-io*))
  )
  (format s (DEUTSCH "~%Beschreibung von~%"
             ENGLISH "~%Description of~%"
             FRANCAIS "~%Description de~%")
  )
  (format s "~A" (write-to-short-string obj sys::*prin-linelength*))
  (format s (DEUTSCH "~%Das ist "
             ENGLISH "~%This is "
             FRANCAIS "~%Ceci est ")
  )
  (let ((type (type-of obj)))
    ; Dispatch nach den möglichen Resultaten von TYPE-OF:
    (if (atom type)
      (case type
        (CONS
          (flet ((list-length (list)  ; vgl. CLTL, S. 265
                   (do ((n 0 (+ n 2))
                        (fast list (cddr fast))
                        (slow list (cdr slow))
                       )
                       (nil)
                     (when (atom fast) (return n))
                     (when (atom (cdr fast)) (return (1+ n)))
                     (when (eq (cdr fast) slow) (return nil))
                )) )
            (let ((len (list-length obj)))
              (if len
                (if (null (nthcdr len obj))
                  (format s (DEUTSCH "eine Liste der Länge ~S."
                             ENGLISH "a list of length ~S."
                             FRANCAIS "une liste de longueur ~S.")
                            len
                  )
                  (if (> len 1)
                    (format s (DEUTSCH "eine punktierte Liste der Länge ~S."
                               ENGLISH "a dotted list of length ~S."
                               FRANCAIS "une liste pointée de longueur ~S.")
                              len
                    )
                    (format s (DEUTSCH "ein Cons."
                               ENGLISH "a cons."
                               FRANCAIS "un «cons».")
                ) ) )
                (format s (DEUTSCH "eine zyklische Liste."
                           ENGLISH "a cyclic list."
                           FRANCAIS "une liste circulaire.")
        ) ) ) ) )
        ((SYMBOL NULL BOOLEAN)
          (when (null obj)
            (format s (DEUTSCH "die leere Liste, "
                       ENGLISH "the empty list, "
                       FRANCAIS "la liste vide, ")
          ) )
          (format s (DEUTSCH "das Symbol ~S"
                     ENGLISH "the symbol ~S"
                     FRANCAIS "le symbole ~S")
                    obj
          )
          (when (keywordp obj)
            (format s (DEUTSCH ", ein Keyword"
                       ENGLISH ", a keyword"
                       FRANCAIS ", un mot-clé")
          ) )
          (when (boundp obj)
            (if (constantp obj)
              (format s (DEUTSCH ", eine Konstante"
                         ENGLISH ", a constant"
                         FRANCAIS ", une constante")
              )
              (if (sys::special-variable-p obj)
                (format s (DEUTSCH ", eine SPECIAL-deklarierte Variable"
                           ENGLISH ", a variable declared SPECIAL"
                           FRANCAIS ", une variable declarée SPECIAL")
                )
                (format s (DEUTSCH ", eine Variable"
                           ENGLISH ", a variable"
                           FRANCAIS ", une variable")
            ) ) )
            (when (symbol-macro-expand obj)
              (format s (DEUTSCH " (Macro)"
                         ENGLISH " (macro)"
                         FRANCAIS " (macro)")
              )
              (push `(MACROEXPAND-1 ',obj) more)
            )
            (push `,obj more)
            (push `(SYMBOL-VALUE ',obj) more)
          )
          (when (fboundp obj)
            (format s (DEUTSCH ", benennt "
                       ENGLISH ", names "
                       FRANCAIS ", le nom ")
            )
            (cond ((special-operator-p obj)
                   (format s (DEUTSCH "eine Special-Form"
                              ENGLISH "a special form"
                              FRANCAIS "d'une forme spéciale")
                   )
                   (when (macro-function obj)
                     (format s (DEUTSCH " mit Macro-Definition"
                                ENGLISH " with macro definition"
                                FRANCAIS ", aussi d'un macro")
                  )) )
                  ((functionp (symbol-function obj))
                   (format s (DEUTSCH "eine Funktion"
                              ENGLISH "a function"
                              FRANCAIS "d'une fonction")
                   )
                   (push `#',obj more)
                   (push `(SYMBOL-FUNCTION ',obj) more)
                  )
                  (t ; (macro-function obj)
                   (format s (DEUTSCH "einen Macro"
                              ENGLISH "a macro"
                              FRANCAIS "d'un macro")
                  ))
          ) )
          (when (or (get obj 'system::type-symbol)
                    (get obj 'system::defstruct-description)
                    (get obj 'system::deftype-expander)
                )
            (format s (DEUTSCH ", benennt einen Typ"
                       ENGLISH ", names a type"
                       FRANCAIS ", le nom d'un type")
            )
            (when (get obj 'system::deftype-expander)
              (push `(TYPE-EXPAND-1 ',obj) more)
          ) )
          (when (get obj 'clos::closclass)
            (format s (DEUTSCH ", benennt eine Klasse"
                       ENGLISH ", names a class"
                       FRANCAIS ", le nom d'une classe")
          ) )
          (when (symbol-plist obj)
            (let ((properties
                    (do ((l nil)
                         (pl (symbol-plist obj) (cddr pl)))
                        ((null pl) (nreverse l))
                      (push (car pl) l)
                 )) )
              (format s (DEUTSCH ", hat die Propert~@P ~{~S~^, ~}"
                         ENGLISH ", has the propert~@P ~{~S~^, ~}"
                         FRANCAIS ", a ~[~;la propriété~:;les propriétés~] ~{~S~^, ~}")
                        (length properties) properties
            ) )
            (push `(SYMBOL-PLIST ',obj) more)
          )
          (format s (DEUTSCH "."
                     ENGLISH "."
                     FRANCAIS ".")
          )
          (format s (DEUTSCH "~%Das Symbol "
                     ENGLISH "~%The symbol "
                     FRANCAIS "~%Le symbole ")
          )
          (let ((home (symbol-package obj)))
            (if home
              (format s (DEUTSCH "liegt in ~S"
                         ENGLISH "lies in ~S"
                         FRANCAIS "est situé dans ~S")
                        home
              )
              (format s (DEUTSCH "ist uninterniert"
                         ENGLISH "is uninterned"
                         FRANCAIS "n'appartient à aucun paquetage")
            ) )
            (let ((accessible-packs nil))
              (let ((*print-escape* t)
                    (*print-readably* nil))
                (let ((normal-printout ; externe Repräsentation ohne Package-Marker
                        (if home
                          (let ((*package* home)) (prin1-to-string obj))
                          (let ((*print-gensym* nil)) (prin1-to-string obj))
                     )) )
                  (dolist (pack (list-all-packages))
                    (when ; obj in pack accessible?
                          (string=
                            (let ((*package* pack)) (prin1-to-string obj))
                            normal-printout
                          )
                      (push pack accessible-packs)
              ) ) ) )
              (when accessible-packs
                (format s (DEUTSCH " und ist in ~:[der Package~;den Packages~] ~{~A~^, ~} accessible"
                           ENGLISH " and is accessible in the package~:[~;s~] ~{~A~^, ~}"
                           FRANCAIS " et est visible dans le~:[ paquetage~;s paquetages~] ~{~A~^, ~}")
                          (cdr accessible-packs)
                          (sort (mapcar #'package-name accessible-packs) #'string<)
          ) ) ) )
          (format s (DEUTSCH "."
                     ENGLISH "."
                     FRANCAIS ".")
        ) )
        ((FIXNUM BIGNUM)
          (format s (DEUTSCH "eine ganze Zahl, belegt ~S Bits, ist als ~:(~A~) repräsentiert."
                     ENGLISH "an integer, uses ~S bits, is represented as a ~(~A~)."
                     FRANCAIS "un nombre entier, occupant ~S bits, est représenté comme ~(~A~).")
                    (integer-length obj) type
        ) )
        (RATIO
          (format s (DEUTSCH "eine rationale, nicht ganze Zahl."
                     ENGLISH "a rational, not integral number."
                     FRANCAIS "un nombre rationnel mais pas entier.")
        ) )
        ((SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
          (format s (DEUTSCH "eine Fließkommazahl mit ~S Mantissenbits (~:(~A~))."
                     ENGLISH "a float with ~S bits of mantissa (~(~A~))."
                     FRANCAIS "un nombre à virgule flottante avec une précision de ~S bits (un ~(~A~)).")
                    (float-digits obj) type
        ) )
        (COMPLEX
          (format s (DEUTSCH "eine komplexe Zahl "
                     ENGLISH "a complex number "
                     FRANCAIS "un nombre complexe ")
          )
          (let ((x (realpart obj))
                (y (imagpart obj)))
            (if (zerop y)
              (if (zerop x)
                (format s (DEUTSCH "im Ursprung"
                           ENGLISH "at the origin"
                           FRANCAIS "à l'origine")
                )
                (format s (DEUTSCH "auf der ~:[posi~;nega~]tiven reellen Achse"
                           ENGLISH "on the ~:[posi~;nega~]tive real axis"
                           FRANCAIS "sur la partie ~:[posi~;nega~]tive de l'axe réelle")
                          (minusp x)
              ) )
              (if (zerop x)
                (format s (DEUTSCH "auf der ~:[posi~;nega~]tiven imaginären Achse"
                           ENGLISH "on the ~:[posi~;nega~]tive imaginary axis"
                           FRANCAIS "sur la partie ~:[posi~;nega~]tive de l'axe imaginaire")
                          (minusp y)
                )
                (format s (DEUTSCH "im ~:[~:[ers~;vier~]~;~:[zwei~;drit~]~]ten Quadranten"
                           ENGLISH "in ~:[~:[first~;fourth~]~;~:[second~;third~]~] the quadrant"
                           FRANCAIS "dans le ~:[~:[premier~;quatrième~]~;~:[deuxième~;troisième~]~] quartier")
                          (minusp x) (minusp y)
          ) ) ) )
          (format s (DEUTSCH " der Gaußschen Zahlenebene."
                     ENGLISH " of the Gaussian number plane."
                     FRANCAIS " du plan Gaussien.")
        ) )
        (CHARACTER
          (format s (DEUTSCH "ein Zeichen"
                     ENGLISH "a character"
                     FRANCAIS "un caractère")
          )
          (unless (zerop (char-bits obj))
            (format s (DEUTSCH " mit Zusatzbits"
                       ENGLISH " with additional bits"
                       FRANCAIS " avec des bits supplémentaires")
          ) )
          (unless (zerop (char-font obj))
            (format s (DEUTSCH " aus Zeichensatz ~S"
                       ENGLISH " from font ~S"
                       FRANCAIS " de la police («font») ~S")
                      (char-font obj)
          ) )
          (format s (DEUTSCH "."
                     ENGLISH "."
                     FRANCAIS ".")
          )
          (format s (DEUTSCH "~%Es ist ein ~:[nicht ~;~]druckbares Zeichen."
                     ENGLISH "~%It is a ~:[non-~;~]printable character."
                     FRANCAIS "~%C'est un caractère ~:[non ~;~]imprimable.")
                    (graphic-char-p obj)
          )
          (unless (standard-char-p obj)
            (format s (DEUTSCH "~%Seine Verwendung ist nicht portabel."
                       ENGLISH "~%Its use is non-portable."
                       FRANCAIS "~%Il n'est pas portable de l'utiliser.")
          ) )
        )
        (FUNCTION ; (SYS::CLOSUREP obj) ist erfüllt
          (let ((compiledp (compiled-function-p obj)))
            (format s (DEUTSCH "eine ~:[interpret~;compil~]ierte Funktion."
                       ENGLISH "a~:[n interpret~; compil~]ed function."
                       FRANCAIS "une fonction ~:[interprét~;compil~]ée.")
                      compiledp
            )
            (if compiledp
              (multiple-value-bind (req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p)
                  (sys::signature obj)
                (describe-signature s req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p)
                (push `(DISASSEMBLE #',(sys::closure-name obj)) more)
                (push `(DISASSEMBLE ',obj) more)
              )
              (progn
                (format s (DEUTSCH "~%Argumentliste: ~S"
                           ENGLISH "~%argument list: ~S"
                           FRANCAIS "~%Liste des arguments: ~S")
                          (car (sys::%record-ref obj 1))
                )
                (let ((doc (sys::%record-ref obj 2)))
                  (when doc
                    (format s (DEUTSCH "~%Dokumentation: ~A"
                               ENGLISH "~%documentation: ~A"
                               FRANCAIS "~%Documentation: ~A")
                              doc
              ) ) ) )
        ) ) )
        (COMPILED-FUNCTION ; nur SUBRs und FSUBRs
          (if (functionp obj)
            ; SUBR
            (progn
              (format s (DEUTSCH "eine eingebaute System-Funktion."
                         ENGLISH "a built-in system function."
                         FRANCAIS "une fonction prédéfinie du système.")
              )
              (multiple-value-bind (name req-anz opt-anz rest-p keywords allow-other-keys)
                  (sys::subr-info obj)
                (when name
                  (describe-signature s req-anz opt-anz rest-p keywords keywords allow-other-keys)
            ) ) )
            ; FSUBR
            (format s (DEUTSCH "ein Special-Form-Handler."
                       ENGLISH "a special form handler."
                       FRANCAIS "un interpréteur de forme spéciale.")
        ) ) )
        #+(or AMIGA FFI)
        (FOREIGN-POINTER
          (format s (DEUTSCH "ein Foreign-Pointer."
                     ENGLISH "a foreign pointer"
                     FRANCAIS "un pointeur étranger.")
        ) )
        #+FFI
        (FOREIGN-ADDRESS
          (format s (DEUTSCH "eine Foreign-Adresse."
                     ENGLISH "a foreign address"
                     FRANCAIS "une addresse étrangère.")
        ) )
        #+FFI
        (FOREIGN-VARIABLE
          (format s (DEUTSCH "eine Foreign-Variable vom Foreign-Typ ~S."
                     ENGLISH "a foreign variable of foreign type ~S."
                     FRANCAIS "une variable étrangère de type étranger ~S.")
                    (deparse-c-type (sys::%record-ref obj 3))
        ) )
        #+FFI
        (FOREIGN-FUNCTION
          (format s (DEUTSCH "eine Foreign-Funktion."
                     ENGLISH "a foreign function."
                     FRANCAIS "une fonction étrangère.")
        ) )
        ((STREAM FILE-STREAM SYNONYM-STREAM BROADCAST-STREAM
          CONCATENATED-STREAM TWO-WAY-STREAM ECHO-STREAM STRING-STREAM
         )
          (format s (DEUTSCH "ein ~:[~:[geschlossener ~;Output-~]~;~:[Input-~;bidirektionaler ~]~]Stream."
                     ENGLISH "a~:[~:[ closed ~;n output-~]~;~:[n input-~;n input/output-~]~]stream."
                     FRANCAIS "un «stream» ~:[~:[fermé~;de sortie~]~;~:[d'entrée~;d'entrée/sortie~]~].")
                    (input-stream-p obj) (output-stream-p obj)
        ) )
        (PACKAGE
          (if (package-name obj)
            (progn
              (format s (DEUTSCH "die Package mit Namen ~A"
                         ENGLISH "the package named ~A"
                         FRANCAIS "le paquetage de nom ~A")
                        (package-name obj)
              )
              (let ((nicknames (package-nicknames obj)))
                (when nicknames
                  (format s (DEUTSCH " und zusätzlichen Namen ~{~A~^, ~}"
                             ENGLISH ". It has the nicknames ~{~A~^, ~}"
                             FRANCAIS ". Il porte aussi les noms ~{~A~^, ~}")
                            nicknames
              ) ) )
              (format s (DEUTSCH "."
                         ENGLISH "."
                         FRANCAIS ".")
              )
              (let ((use-list (package-use-list obj))
                    (used-by-list (package-used-by-list obj)))
                (format s (DEUTSCH "~%Sie "
                           ENGLISH "~%It "
                           FRANCAIS "~%Il ")
                )
                (when use-list
                  (format s (DEUTSCH "importiert die externen Symbole der Package~:[~;s~] ~{~A~^, ~} und "
                             ENGLISH "imports the external symbols of the package~:[~;s~] ~{~A~^, ~} and "
                             FRANCAIS "importe les symboles externes d~:[u paquetage~;es paquetages~] ~{~A~^, ~} et ")
                            (cdr use-list) (mapcar #'package-name use-list)
                ) )
                (format s (DEUTSCH "exportiert ~:[keine Symbole~;die Symbole~:*~{~<~%~:; ~S~>~^~}~]"
                           ENGLISH "exports ~:[no symbols~;the symbols~:*~{~<~%~:; ~S~>~^~}~]"
                           FRANCAIS "~:[n'exporte pas de symboles~;exporte les symboles~:*~{~<~%~:; ~S~>~^~}~]")
                          ; Liste aller exportierten Symbole:
                          (let ((L nil))
                            (do-external-symbols (s obj) (push s L))
                            (sort L #'string< :key #'symbol-name)
                )         )
                (when used-by-list
                  (format s (DEUTSCH " an die Package~:[~;s~] ~{~A~^, ~}"
                             ENGLISH " to the package~:[~;s~] ~{~A~^, ~}"
                             FRANCAIS " vers le~:[ paquetage~;s paquetages~] ~{~A~^, ~}")
                            (cdr used-by-list) (mapcar #'package-name used-by-list)
                ) )
                (format s (DEUTSCH "."
                           ENGLISH "."
                           FRANCAIS ".")
            ) ) )
            (format s (DEUTSCH "eine gelöschte Package."
                       ENGLISH "a deleted package."
                       FRANCAIS "un paquetage éliminé.")
        ) ) )
        (HASH-TABLE
          (format s (DEUTSCH "eine Hash-Tabelle mit ~S Eintr~:*~[ägen~;ag~:;ägen~]."
                     ENGLISH "a hash table with ~S entr~:@P."
                     FRANCAIS "un tableau de hachage avec ~S entrée~:*~[s~;~:;s~].")
                    (hash-table-count obj)
        ) )
        (READTABLE
          (format s (DEUTSCH "~:[eine ~;die Common-Lisp-~]Readtable."
                     ENGLISH "~:[a~;the Common Lisp~] readtable."
                     FRANCAIS "~:[un~;le~] tableau de lecture~:*~:[~; de Common Lisp~].")
                    (equalp obj (copy-readtable))
        ) )
        ((PATHNAME #+LOGICAL-PATHNAMES LOGICAL-PATHNAME)
          (format s (DEUTSCH "ein ~:[~;portabler ~]Pathname~:[.~;~:*, aufgebaut aus:~{~A~}~]"
                     ENGLISH "a ~:[~;portable ~]pathname~:[.~;~:*, with the following components:~{~A~}~]"
                     FRANCAIS "un «pathname»~:[~; portable~]~:[.~;~:*, composé de:~{~A~}~]")
                    (sys::logical-pathname-p obj)
                    (mapcan #'(lambda (kw component)
                                (when component
                                  (list (format nil "~%~A = ~A"
                                                    (symbol-name kw)
                                                    (make-pathname kw component)
                              ) ) )     )
                      '(:host :device :directory :name :type :version)
                      (list
                        (pathname-host obj)
                        (pathname-device obj)
                        (pathname-directory obj)
                        (pathname-name obj)
                        (pathname-type obj)
                        (pathname-version obj)
        ) )         ) )
        (RANDOM-STATE
          (format s (DEUTSCH "ein Random-State."
                     ENGLISH "a random-state."
                     FRANCAIS "un «random-state».")
        ) )
        (BYTE
          (format s (DEUTSCH "ein Byte-Specifier, bezeichnet die ~S Bits ab Bitposition ~S eines Integers."
                     ENGLISH "a byte specifier, denoting the ~S bits starting at bit position ~S of an integer."
                     FRANCAIS "un intervalle de bits, comportant ~S bits à partir de la position ~S d'un entier.")
                    (byte-size obj) (byte-position obj)
        ) )
        (LOAD-TIME-EVAL
          (format s (DEUTSCH "eine Absicht der Evaluierung zur Ladezeit." ; ??
                     ENGLISH "a load-time evaluation promise." ; ??
                     FRANCAIS "une promesse d'évaluation au moment du chargement.") ; ??
        ) )
        (READ-LABEL
          (format s (DEUTSCH "eine Markierung zur Auflösung von #~D#-Verweisen bei READ."
                     ENGLISH "a label used for resolving #~D# references during READ."
                     FRANCAIS "une marque destinée à résoudre #~D# au cours de READ.")
                    (logand (sys::address-of obj) '#,(ash most-positive-fixnum -1))
        ) )
        (FRAME-POINTER
          (format s (DEUTSCH "ein Pointer in den Stack. Er zeigt auf:"
                     ENGLISH "a pointer into the stack. It points to:"
                     FRANCAIS "un pointeur dans la pile. Il pointe vers :")
          )
          (sys::describe-frame s obj)
        )
        (SYSTEM-INTERNAL
          (format s (DEUTSCH "ein Objekt mit besonderen Eigenschaften."
                     ENGLISH "a special-purpose object."
                     FRANCAIS "un objet distingué.")
        ) )
        (ADDRESS
          (format s (DEUTSCH "eine Maschinen-Adresse."
                     ENGLISH "a machine address."
                     FRANCAIS "une addresse au niveau de la machine.")
        ) )
        (t
         (if (and (symbolp type) (sys::%structure-type-p type obj))
           ; Structure
           (progn
             (format s (DEUTSCH "eine Structure vom Typ ~S."
                        ENGLISH "a structure of type ~S."
                        FRANCAIS "une structure de type ~S.")
                       type
             )
             (let ((type (sys::%record-ref obj 0)))
               (when (cdr type)
                 (format s (DEUTSCH "~%Als solche ist sie auch eine Structure vom Typ ~{~S~^, ~}."
                            ENGLISH "~%As such, it is also a structure of type ~{~S~^, ~}."
                            FRANCAIS "~%En tant que telle, c'est aussi une structure de type ~{~S~^, ~}.")
                           (cdr type)
             ) ) )
             (clos:describe-object obj s)
           )
           ; CLOS-Instanz
           (progn
             (format s (DEUTSCH "eine Instanz der CLOS-Klasse ~S."
                        ENGLISH "an instance of the CLOS class ~S."
                        FRANCAIS "un objet appartenant à la classe ~S de CLOS.")
                       (clos:class-of obj)
             )
             (clos:describe-object obj s)
         ) )
      ) )
      ; Array-Typen
      (let ((rank (array-rank obj))
            (eltype (array-element-type obj)))
        (format s (DEUTSCH "ein~:[~; einfacher~] ~A-dimensionaler Array"
                   ENGLISH "a~:[~; simple~] ~A dimensional array"
                   FRANCAIS "une matrice~:[~; simple~] à ~A dimension~:P")
                  (simple-array-p obj) rank
        )
        (when (eql rank 1)
          (format s (DEUTSCH " (Vektor)"
                     ENGLISH " (vector)"
                     FRANCAIS " (vecteur)")
        ) )
        (unless (eq eltype 'T)
          (format s (DEUTSCH " von ~:(~A~)s"
                     ENGLISH " of ~(~A~)s"
                     FRANCAIS " de ~(~A~)s")
                    eltype
        ) )
        (when (adjustable-array-p obj)
          (format s (DEUTSCH ", adjustierbar"
                     ENGLISH ", adjustable"
                     FRANCAIS ", ajustable")
        ) )
        (when (plusp rank)
          (format s (DEUTSCH ", der Größe ~{~S~^ x ~}"
                     ENGLISH ", of size ~{~S~^ x ~}"
                     FRANCAIS ", de grandeur ~{~S~^ x ~}")
                    (array-dimensions obj)
          )
          (when (array-has-fill-pointer-p obj)
            (format s (DEUTSCH " und der momentanen Länge (Fill-Pointer) ~S"
                       ENGLISH " and current length (fill-pointer) ~S"
                       FRANCAIS " et longueur courante (fill-pointer) ~S")
                      (fill-pointer obj)
        ) ) )
        (format s (DEUTSCH "."
                   ENGLISH "."
                   FRANCAIS ".")
      ) )
  ) )
  (when more
    (format s (DEUTSCH "~%Mehr Information durch Auswerten von ~{~S~^ oder ~}."
               ENGLISH "~%For more information, evaluate ~{~S~^ or ~}."
               FRANCAIS "~%Pour obtenir davantage d'information, évaluez ~{~S~^ ou ~}.")
              (nreverse more)
  ) )
  (values)
)

; Liefert die Signatur eines funktionalen Objekts, als Werte:
; 1. req-anz
; 2. opt-anz
; 3. rest-p
; 4. key-p
; 5. keyword-list
; 6. allow-other-keys-p
(defun function-signature (obj)
  (if (sys::closurep obj)
    (if (compiled-function-p obj)
      ; compilierte Closure
      (multiple-value-bind (req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p)
          (sys::signature obj) ; siehe compiler.lsp
        (values req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p)
      )
      ; interpretierte Closure
      (let ((clos_keywords (sys::%record-ref obj 16)))
        (values (sys::%record-ref obj 12) ; req_anz
                (sys::%record-ref obj 13) ; opt_anz
                (sys::%record-ref obj 19) ; rest_flag
                (not (numberp clos_keywords))
                (if (not (numberp clos_keywords)) (copy-list clos_keywords))
                (sys::%record-ref obj 18) ; allow_flag
      ) )
    )
    (cond #+FFI
          ((eq (type-of obj) 'FOREIGN-FUNCTION)
           (values (sys::foreign-function-signature obj) 0 nil nil nil nil)
          )
          (t
           (multiple-value-bind (name req-anz opt-anz rest-p keywords allow-other-keys)
               (sys::subr-info obj)
             (if name
               (values req-anz opt-anz rest-p keywords keywords allow-other-keys)
               (error (DEUTSCH "~S: ~S ist keine Funktion."
                       ENGLISH "~S: ~S is not a function."
                       FRANCAIS "~S : ~S n'est pas une fonction.")
                      'function-signature obj
               )
) ) )     )) )

(defun describe-signature (s req-anz opt-anz rest-p keyword-p keywords allow-other-keys)
  (format s (DEUTSCH "~%Argumentliste: "
             ENGLISH "~%argument list: "
             FRANCAIS "~%Liste des arguments : ")
  )
  (format s "(~{~A~^ ~})"
    (let ((args '()) (count 0))
      (dotimes (i req-anz)
        (incf count)
        (push (format nil "ARG~D" count) args)
      )
      (when (plusp opt-anz)
        (push '&OPTIONAL args)
        (dotimes (i opt-anz)
          (incf count)
          (push (format nil "ARG~D" count) args)
      ) )
      (when rest-p
        (push '&REST args)
        (push "OTHER-ARGS" args)
      )
      (when keyword-p
        (push '&KEY args)
        (dolist (kw keywords) (push (prin1-to-string kw) args))
        (when allow-other-keys (push '&ALLOW-OTHER-KEYS args))
      )
      (nreverse args)
) ) )
;; DOCUMENTATION mit abfragen und ausgeben??
;; function, variable, type, structure, setf

; Gibt object in einen String aus, der nach Möglichkeit höchstens max Zeichen
; lang sein soll.
(defun write-to-short-string (object max)
  ; Methode: probiere
  ; level = 0: length = 0,1,2
  ; level = 1: length = 1,2,3,4
  ; level = 2: length = 2,...,6
  ; usw. bis maximal level = 16.
  ; Dabei level möglichst groß, und bei festem level length möglichst groß.
  (if (or (numberp object) (symbolp object)) ; von length und level unbeeinflußt?
    (write-to-string object)
    (macrolet ((minlength (level) `,level)
               (maxlength (level) `(* 2 (+ ,level 1))))
      ; Um level möglist groß zu bekommen, dabei length = minlength wählen.
      (let* ((level ; Binärsuche nach dem richtigen level
               (let ((level1 0) (level2 16))
                 (loop
                   (when (= (- level2 level1) 1) (return))
                   (let ((levelm (floor (+ level1 level2) 2)))
                     (if (<= (length (write-to-string object :level levelm :length (minlength levelm))) max)
                       (setq level1 levelm) ; levelm paßt, probiere größere
                       (setq level2 levelm) ; levelm paßt nicht, probiere kleinere
                 ) ) )
                 level1
             ) )
             (length ; Binärsuche nach dem richtigen length
               (let ((length1 (minlength level)) (length2 (maxlength level)))
                 (loop
                   (when (= (- length2 length1) 1) (return))
                   (let ((lengthm (floor (+ length1 length2) 2)))
                     (if (<= (length (write-to-string object :level level :length lengthm)) max)
                       (setq length1 lengthm) ; lengthm paßt, probiere größere
                       (setq length2 lengthm) ; lengthm paßt nicht, probiere kleinere
                 ) ) )
                 length1
            )) )
        (write-to-string object :level level :length length)
) ) ) )

;-------------------------------------------------------------------------------
;; ROOM

(definternational room-format (t ENGLISH))
(deflocalized room-format DEUTSCH
  (list (formatter "Klasse~VT Instanzen   Größe (Bytes)   ø-Größe~%")
        (formatter "------~VT ---------   -------------  ---------~%")
        (formatter       "~VT~8D     ~9D  ~13,3F~%")
) )
(deflocalized room-format ENGLISH
  (list (formatter "Class~VT# Instances  Size (bytes)  Average size~%")
        (formatter "-----~VT-----------  ------------  ------------~%")
        (formatter      "~VT~8D     ~9D  ~13,3F~%")
) )
(deflocalized room-format FRANCAIS
  (list (formatter "Classe~VT instances  taille (octets)  t. moyenne~%")
        (formatter "------~VT ---------  ---------------  ----------~%")
        (formatter       "~VT~8D     ~9D  ~13,3F~%")
) )

(defun room (&optional (kind :default))
  (unless (or (eq kind 'nil) (eq kind ':default) (eq kind 't))
    (error (DEUTSCH "~S: Argument muss ~S, ~S oder ~S sein, nicht ~S"
            ENGLISH "~S: argument must be ~S, ~S or ~S, not ~S"
            FRANCAIS "~S : L'argument doit être ~S, ~S ou ~S et non ~S")
           'room 'nil 't ':default kind
  ) )
  ; Get the figures now, because (heap-statistics) causes heap allocation.
  (multiple-value-bind (used room static) (sys::%room)
    (when (eq kind 't)
      (let ((stat (heap-statistics)))
        ; stat = #( ... (classname num-instances . num-bytes) ...)
        (setq stat (sort stat #'> :key #'cddr))
        (let* ((localinfo (localized 'room-format))
               (header-line (first localinfo))
               (separator-line (second localinfo))
               (data-line (third localinfo))
               (midcol (- sys::*prin-linelength* 39))
               (total-instances 0)
               (total-bytes 0))
          (fresh-line)
          (format t header-line midcol)
          (format t separator-line midcol)
          (dotimes (i (length stat))
            (let* ((stat-record (svref stat i))
                   (classname (car stat-record))
                   (instances (cadr stat-record))
                   (bytes (cddr stat-record)))
              (when (plusp instances)
                (prin1 classname)
                (format t data-line midcol
                          instances bytes
                          (/ (float bytes 0d0) instances)
                )
                (incf total-instances instances)
                (incf total-bytes bytes)
          ) ) )
          (format t separator-line midcol)
          (write-string (DEUTSCH "Gesamt"
                         ENGLISH "Total"
                         FRANCAIS "total")
          )
          (format t data-line midcol
                    total-instances total-bytes
                    (/ (float total-bytes 0d0) total-instances)
          )
    ) ) )
    (unless (eq kind 'nil)
      (format t (DEUTSCH "~%Dauerhaft allozierte Bytes:    ~9D~%Gegenwärtig benutzte Bytes:    ~9D~%Bis zur nächsten GC verfügbar: ~9D~%"
                 ENGLISH "~%Bytes permanently allocated:   ~9D~%Bytes currently in use:        ~9D~%Bytes available until next GC: ~9D~%"
                 FRANCAIS "~%Allocation permanente:                   ~9D~%Octets couramment utilisés:              ~9D~%Octets disponibles jusqu'au prochain GC: ~9D~%")
                static used room
    ) )
    (values used room)
) )

;-------------------------------------------------------------------------------
;; SPACE

;; Recall the macro SPACE, making (space form) roughly equivalent to
;
; (multiple-value-bind (var1 var2 var3 var4) (%space1)
;   (let ((*gc-statistics* (1+ (max *gc-statistics* 0))))
;     (setq var3 (multiple-value-list form))
;     (setq var4 (%space2))
;   )
;   (%space var1 var2 var3 var4)
;   (values-list var3)
; )

;; It works like this:
;; 1. Do a GC, to eliminate unused objects, and call (heap-statistics)
;;    to get a statistics of all live objects.
;; 2. Enable the GC reclaimage statistics: Call (gc-statistics),
;;    then bind *gc-statistics* to at least 1.
;; 3. Evaluate the form.
;; 4. Convert the form's values to a list.
;; 5. Call (gc), so that the GC eliminates temporarily allocated objects.
;; 6. Call (gc-statistics) again. We can now unbind *gc-statistics*.
;; 7. Call (heap-statistics) again, to get a statistics of all now live
;;    objects.
;; ==> The difference of the two (gc-statistics) calls will comprise exactly
;;     the temporary allocations of the form.
;; ==> The difference of the two (heap-statistics) calls will comprise
;;     - the resulting data structure of the first call to (heap-statistics),
;;     - the permanent allocations of the form,
;;     - the list allocated to hold the form's values,
;;     - the data structures which form the ldifference between the two
;;       calls to (gc-statistics).

(defun %space1 ()
  (gc)
  (values
    (heap-statistics)
    (gc-statistics)
) )

; %space1 returns two values, so var3 and var4 are initially bound to nil.

(defun %space2 ()
  (gc)
  (gc-statistics)
)

(defun %space (heap-stat-1 gc-stat-1 vallist gc-stat-2)
  (let ((heap-stat-2 (heap-statistics)))
    ;; Now we have all the statistics, and are free to do any kind
    ;; of allocations.
    (let ((ht (make-hash-table :test #'eq)))
      ;; For each type, (gethash type ht) contains a cons
      ;;   (heap-stat-record . gc-stat-record),
      ;; where both records are conses (n-instances . n-bytes).
      (flet ((add (statv accessor incrementer)
               ;; Adds the contents of statv to the hash table.
               ;; accessor = #'car or #'cdr, incrementer = #'+ or #'-.
               (dotimes (i (length statv))
                 (let ((stat-record (svref statv i)))
                   (let ((name (car stat-record))
                         (n-instances (cadr stat-record))
                         (n-bytes (cddr stat-record)))
                     (unless (and (zerop n-instances) (zerop n-bytes))
                       (let ((htx (gethash name ht)))
                         (unless htx
                           (setf (gethash name ht)
                                 (setf htx (cons (cons 0 0) (cons 0 0)))
                         ) )
                         (let ((record (funcall accessor htx)))
                           (setf (car record)
                                 (funcall incrementer (car record) n-instances)
                           )
                           (setf (cdr record)
                                 (funcall incrementer (cdr record) n-bytes)
                           )
            )) ) ) ) ) ) )
        ;; The difference between the two (gc-statistics) calls.
        (do ((l gc-stat-2 (cdr l)))
            ((eq l gc-stat-1))
          (add (car l) #'cdr #'+)
        )
        ;; The difference between the two (heap-statistics) calls.
        (add heap-stat-2 #'car #'+)
        (add heap-stat-1 #'car #'-)
        ;; Adjust for the statistics data structures themselves.
        (add (heap-statistics-statistics heap-stat-1) #'car #'-)
        (add (list-statistics vallist) #'car #'-)
        (add (gc-statistics-statistics gc-stat-1 gc-stat-2) #'car #'-)
      )
      ;; Now transform the hash table into a list, and sort it.
      (let ((statlist '()))
        (maphash #'(lambda (name htx) (push (cons name htx) statlist)) ht)
        ;; statlist is now a list of (name heap-stat-record . gc-stat-record).
        (setq statlist
              (sort statlist #'>
                    :key #'(lambda (r) (+ (cdr (cadr r)) (cdr (cddr r))))
        )     )
        ;; Display the statistics.
        (space-tabulate statlist)
        ;; Done. The return value is ignored.
) ) ) )

(definternational space-format (t ENGLISH))
(deflocalized space-format DEUTSCH
  (list (formatter       "~VT     dauerhaft             temporär~%")
        (formatter "Klasse~VTInstanzen   Bytes    Instanzen   Bytes~%")
        (formatter "------~VT--------- ---------  --------- ---------~%")
        (formatter       "~VT~9D ~9D  ~9D ~9D~%")
) )
(deflocalized space-format ENGLISH
  (list (formatter      "~VT     Permanent            Temporary~%")
        (formatter "Class~VTinstances   bytes    instances   bytes~%")
        (formatter "-----~VT--------- ---------  --------- ---------~%")
        (formatter      "~VT~9D ~9D  ~9D ~9D~%")
) )
(deflocalized space-format FRANCAIS
  (list (formatter       "~VT     permanent            temporaire~%")
        (formatter "Classe~VTinstances   octets   instances   octets~%")
        (formatter "------~VT--------- ---------  --------- ---------~%")
        (formatter       "~VT~9D ~9D  ~9D ~9D~%")
) )

(defun space-tabulate (statlist)
  (let* ((localinfo (localized 'space-format))
         (header-line1 (first localinfo))
         (header-line2 (second localinfo))
         (separator-line (third localinfo))
         (data-line (fourth localinfo))
         (midcol (- sys::*prin-linelength* 40))
         (total-perm-instances 0)
         (total-perm-bytes 0)
         (total-temp-instances 0)
         (total-temp-bytes 0)
         (stream *trace-output*))
    (fresh-line stream)
    (format stream header-line1 midcol)
    (format stream header-line2 midcol)
    (format stream separator-line midcol)
    (dolist (stat-record statlist)
      (let* ((classname (car stat-record))
             (permstat (cadr stat-record))
             (tempstat (cddr stat-record))
             (perm-instances (car permstat))
             (perm-bytes (cdr permstat))
             (temp-instances (car tempstat))
             (temp-bytes (cdr tempstat)))
        (unless (and (zerop perm-instances) (zerop perm-bytes)
                     (zerop temp-instances) (zerop temp-bytes)
                )
          (prin1 classname stream)
          (format stream data-line midcol
                  perm-instances perm-bytes
                  temp-instances temp-bytes
          )
          (incf total-perm-instances perm-instances)
          (incf total-perm-bytes perm-bytes)
          (incf total-temp-instances temp-instances)
          (incf total-temp-bytes temp-bytes)
    ) ) )
    (format stream separator-line midcol)
    (write-string (DEUTSCH "Gesamt"
                   ENGLISH "Total"
                   FRANCAIS "total")
                  stream
    )
    (format stream data-line midcol
            total-perm-instances total-perm-bytes
            total-temp-instances total-temp-bytes
    )
) )

;-------------------------------------------------------------------------------
;; DRIBBLE

; The use of an intermediate synonym-stream is for robustness.
; (Just try dribbling to a file on a full disk partition...)
(defvar *dribble-stream* nil)

(let ((dribble-file nil) (dribbled-input nil) (dribbled-output nil)
      (dribbled-error-output nil) (dribbled-trace-output nil)
      (dribbled-query-io nil) (dribbled-debug-io nil))
  (defun dribble (&optional file)
    (if file
      (progn
        (if dribble-file
          (warn (DEUTSCH "Es wird bereits auf ~S protokolliert."
                 ENGLISH "Already dribbling to ~S"
                 FRANCAIS "Le protocole est déjà écrit sur ~S.")
                dribble-file
          )
          ;; Dribbling means to redirect all screen output to the file.
          ;; We redirect all standard streams. More precisely, those
          ;; which are #<SYNONYM-STREAM *TERMINAL-IO*>. Those which are
          ;; synonyms to other standard streams indirectly referring
          ;; to #<SYNONYM-STREAM *TERMINAL-IO*> are not redirected,
          ;; because that would cause each output to this stream to
          ;; be written twice to the dribble-file.
          (labels ((goes-to-terminal (stream) ; this is a hack
                     (and (typep stream 'synonym-stream)
                          (eq (synonym-stream-symbol stream) '*terminal-io*)
                   ) )
                   (goes-indirectly-to-terminal (stream) ; an even bigger hack
                     (and (typep stream 'synonym-stream)
                          (let ((sym (synonym-stream-symbol stream)))
                            (and (boundp sym)
                                 (let ((stream (symbol-value sym)))
                                   (or (goes-to-terminal stream)
                                       (goes-indirectly-to-terminal stream)
                  )) )    ) )    ) )
            (setq *dribble-stream* (open file :direction :output
                                              :if-exists :append
                                              :if-does-not-exist :create)
                  dribble-file (make-synonym-stream '*dribble-stream*)
                  dribbled-input nil
                  dribbled-output nil
                  dribbled-error-output nil
                  dribbled-trace-output nil
                  dribbled-query-io nil
                  dribbled-debug-io nil
            )
            (unless (goes-indirectly-to-terminal *standard-input*)
              (setq dribbled-input *standard-input*)
              (setq *standard-input* (make-echo-stream *standard-input* dribble-file))
            )
            (unless (goes-indirectly-to-terminal *standard-output*)
              (setq dribbled-output *standard-output*)
              (setq *standard-output* (make-broadcast-stream *standard-output* dribble-file))
            )
            (when (goes-to-terminal *error-output*)
              (setq dribbled-error-output *error-output*)
              (setq *error-output* (make-broadcast-stream *error-output* dribble-file))
            )
            (when (goes-to-terminal *trace-output*)
              (setq dribbled-trace-output *trace-output*)
              (setq *trace-output* (make-broadcast-stream *trace-output* dribble-file))
            )
            (when (goes-to-terminal *query-io*)
              (setq dribbled-query-io *query-io*)
              (setq *query-io*
                    (make-two-way-stream
                          (make-echo-stream *query-io* dribble-file)
                          (make-broadcast-stream *query-io* dribble-file)
            ) )     )
            (when (goes-to-terminal *debug-io*)
              (setq dribbled-debug-io *debug-io*)
              (setq *debug-io*
                    (make-two-way-stream
                          (make-echo-stream *debug-io* dribble-file)
                          (make-broadcast-stream *debug-io* dribble-file)
            ) )     )
        ) )
        *dribble-stream*
      )
      (if dribble-file
        (progn
          (when dribbled-input (setq *standard-input* dribbled-input))
          (when dribbled-output (setq *standard-output* dribbled-output))
          (when dribbled-error-output (setq *error-output* dribbled-error-output))
          (when dribbled-trace-output (setq *trace-output* dribbled-trace-output))
          (when dribbled-query-io (setq *query-io* dribbled-query-io))
          (when dribbled-debug-io (setq *debug-io* dribbled-debug-io))
          (setq dribble-file nil)
          (setq dribbled-input nil)
          (setq dribbled-output nil)
          (setq dribbled-error-output nil)
          (setq dribbled-trace-output nil)
          (setq dribbled-query-io nil)
          (setq dribbled-debug-io nil)
          (prog1
            *dribble-stream*
            (close *dribble-stream*)
            (setq *dribble-stream* (make-broadcast-stream))
        ) )
        (warn (DEUTSCH "Es wird zur Zeit nicht protokolliert."
               ENGLISH "Currently not dribbling."
               FRANCAIS "Aucun protocole n'est couramment écrit.")
) ) ) ) )

;-------------------------------------------------------------------------------
;; ED

;; *editor*, editor-name und editor-tempfile sind in CONFIG.LSP definiert.
;; Hier stehen nur die Defaults.

;; Der Name des Editors:
(defparameter *editor* nil)

;; Liefert den Namen des Editors:
(defun editor-name () *editor*)

;; Das temporäre File, das LISP beim Editieren anlegt:
(defun editor-tempfile ()
  #+DOS "LISPTEMP.LSP"
  #+OS/2 "lisptemp.lsp"
  #+AMIGA "T:lisptemp.lsp"
  #+(or UNIX WIN32) (merge-pathnames "lisptemp.lsp" (user-homedir-pathname))
)

;; (edit-file file) editiert ein File.
(defun edit-file (file)
  (unless (editor-name)
    (error-of-type 'error
      (DEUTSCH "Kein externer Editor installiert."
       ENGLISH "No external editor installed."
       FRANCAIS "Un éditeur externe n'est pas installé.")
  ) )
  ; Damit TRUENAME keinen Fehler liefert, wenn das File noch nicht existiert,
  ; stellen wir sicher, daß das File existiert:
  #+(or UNIX AMIGA ACORN-RISCOS)
  (unless (probe-file file)
    (close (open file :direction :output))
  )
  #+(or DOS OS/2 WIN32)
    (execute (editor-name) ; das ist der Name des Editors
             (namestring file t) ; file als String
    )
  #+UNIX
    (shell (format nil "~A ~A" (editor-name) (truename file)))
  #+AMIGA
    (shell (format nil "~A \"~A\"" (editor-name) (truename file)))
  #+ACORN-RISCOS
    (let ((pathname (truename file)))
      (shell
        (format nil "~A ~A"
                    (editor-name)
                    (if (pathname-type pathname)
                      ; swap pathname's name and type
                      (merge-pathnames
                        (make-pathname :name (pathname-type pathname)
                                       :type (pathname-name pathname)
                        )
                        pathname
                      )
                      pathname
                    )
    ) ) )
)

(defun ed (&optional arg &aux funname sym fun def)
  (if (null arg)
    (edit-file "")
    (if (or (pathnamep arg) (stringp arg))
      (edit-file arg)
      (if (and (cond ((function-name-p arg) (setq funname arg) t)
                     ((functionp arg) (function-name-p (setq funname (sys::%record-ref arg 0))))
                     (t nil)
               )
               (fboundp (setq sym (get-funname-symbol funname)))
               (or (setq fun (macro-function sym))
                   (setq fun (symbol-function sym))
               )
               (functionp fun)
               (or (function-name-p arg) (eql fun arg))
               (setq def (get sym 'sys::definition))
          )
        (let ((tempfile (editor-tempfile)))
          (with-open-file (f tempfile :direction :output)
            (pprint (car def) f)
            (terpri f) (terpri f)
          )
          (let ((date (file-write-date tempfile)))
            (edit-file tempfile)
            (when (> (file-write-date tempfile) date)
              (with-open-file (f tempfile :direction :input)
                (let ((*package* *package*) ; *PACKAGE* binden
                      (end-of-file "EOF")) ; einmaliges Objekt
                  (loop
                    (let ((obj (read f nil end-of-file)))
                      (when (eql obj end-of-file) (return))
                      (print (evalhook obj nil nil (cdr def)))
              ) ) ) )
              (when (compiled-function-p fun) (compile funname))
          ) )
          funname
        )
        (error-of-type 'error
          (DEUTSCH "~S ist nicht editierbar."
           ENGLISH "~S cannot be edited."
           FRANCAIS "~S ne peut pas être édité.")
          arg
) ) ) ) )

(defun uncompile (arg &aux funname sym fun def)
  (if (and (cond ((function-name-p arg) (setq funname arg) t)
                 ((functionp arg) (function-name-p (setq funname (sys::%record-ref arg 0))))
                 (t nil)
           )
           (fboundp (setq sym (get-funname-symbol funname)))
           (or (setq fun (macro-function sym))
               (setq fun (symbol-function sym))
           )
           (functionp fun)
           (or (function-name-p arg) (eql fun arg))
           (setq def (get sym 'sys::definition))
      )
    (evalhook (car def) nil nil (cdr def))
    (error-of-type 'error
      (DEUTSCH "~S: Quellcode zu ~S nicht verfügbar."
       ENGLISH "~S: source code for ~S not available."
       FRANCAIS "~S : Les sources de ~S ne sont pas présentes.")
      'uncompile funname
    )
) )

;-------------------------------------------------------------------------------

; Speichert den momentanen Speicherinhalt unter Weglassen überflüssiger
; Objekte ab als LISPIMAG.MEM.
; Diese Funktion bekommt keine Argumente und hat keine lokalen Variablen, da
; sonst in interpretiertem Zustand die Variablenwerte mit abgespeichert würden.
(defun %saveinitmem ()
  (do-all-symbols (sym) (remprop sym 'sys::definition))
  (when (fboundp 'clos::install-dispatch)
    (do-all-symbols (sym)
      (when (and (fboundp sym) (clos::generic-function-p (symbol-function sym)))
        (let ((gf (symbol-function sym)))
          (when (clos::gf-never-called-p gf)
            (clos::install-dispatch gf)
  ) ) ) ) )
  (setq - nil + nil ++ nil +++ nil * nil ** nil *** nil / nil // nil /// nil)
  (savemem "lispimag.mem")
  (room nil)
)

; Speichert den momentanen Speicherinhalt ab.
; Läuft nur in compiliertem Zustand!
(defun saveinitmem (&optional (filename "lispinit.mem")
                    &key ((:quiet *quiet*) nil) init-function)
  (setq - nil + nil ++ nil +++ nil * nil ** nil *** nil / nil // nil /// nil)
  (if init-function
    (let* ((old-driver *driver*)
           (*driver* #'(lambda ()
                         (setq *driver* old-driver)
                         (funcall init-function)
                         (funcall *driver*)
          ))           )
      (savemem filename)
    )
    (savemem filename)
  )
  (room nil)
)

;-------------------------------------------------------------------------------

; Vervollständigungs-Routine in Verbindung mit der GNU Readline-Library:
; Input: string die Eingabezeile, (subseq string start end) das zu vervoll-
; ständigende Textstück.
; Output: eine Liste von Simple-Strings. Leer, falls keine sinnvolle Vervoll-
; ständigung. Sonst CDR = Liste aller sinnvollen Vervollständigungen, CAR =
; sofortige Ersetzung.
#+(or UNIX DOS OS/2)
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

;-------------------------------------------------------------------------------

#+(or UNIX OS/2 WIN32)
;; UNIX:
; Must quote the program name and arguments since Unix shells interpret
; characters like #\Space, #\', #\<, #\>, #\$ etc. in a special way. This
; kind of quoting should work unless the string contains #\Newline and we
; call csh. But we are lucky: only /bin/sh will be used.
;; WIN32:
; Must quote program name and arguments since Win32 interprets characters
; like #\Space, #\Tab, #\\, #\" (but not #\< and #\>) in a special way:
; - Space and Tab are interpreted as delimiters. They are not treated as
;   delimiters if they are surrounded by double quotes: "...".
; - Unescaped double quotes are removed from the input. Their only effect is
;   that within double quotes, space and tab are treated like normal characters.
; - Backslashes not followed by double quotes are not special.
; - But 2*n+1 backslashes followed by a double quote become
;   n backslashes followed by a double quote (n >= 0):
;     \" -> "
;     \\\" -> \"
;     \\\\\" -> \\"
; The high-level Win32 command interpreter cmd.exe (but not the low-level
; function CreateProcess()) also interprets #\&, #\<, #\>, #\| as special
; delimiters and makes #\^ disappear. To avoid this, quote them like spaces.
(labels (#+UNIX
         (shell-simple-quote (string)
           (shell-quote string)
         )
         #+UNIX
         (shell-quote (string) ; surround a string by single quotes
           (let ((qchar nil) ; last quote character: nil or #\' or #\"
                 (qstring (make-array 10 :element-type 'string-char
                                         :adjustable t :fill-pointer 0)))
             (map nil #'(lambda (c)
                          (let ((q (if (eql c #\') #\" #\')))
                            (unless (eql qchar q)
                              (when qchar (vector-push-extend qchar qstring))
                              (vector-push-extend (setq qchar q) qstring)
                            )
                            (vector-push-extend c qstring)))
                      string
             )
             (when qchar (vector-push-extend qchar qstring))
             qstring
         ) )
         #+(or DOS OS/2)
         (shell-simple-quote (string)
           string
         )
         #+(or DOS OS/2)
         (shell-quote (string) ; surround a string by double quotes
           ; I have tested Turbo C compiled programs and EMX compiled programs.
           ; 1. Special characters (space, tab, <, >, ...) lose their effect if
           ;    they are inside double quotes. To get a double quote, write \".
           ; 2. Separate the strings by spaces. Turbo C compiled programs don't
           ;    require this, but EMX programs merge adjacent strings.
           ; 3. You cannot pass an empty string or a string terminated by \ to
           ;    Turbo C compiled programs. To pass an empty string to EMX
           ;    programs, write "". You shouldn't pass a string terminated by \
           ;    or containing \" to EMX programs.
           ; Quick and dirty: assume none of these cases occur.
           (let ((qstring (make-array 10 :element-type 'string-char
                                         :adjustable t :fill-pointer 0)))
             (vector-push-extend #\" qstring)
             (map nil #'(lambda (c)
                          (when (eql c #\") (vector-push-extend #\\ qstring))
                          (vector-push-extend c qstring)
                        )
                      string
             )
             (vector-push-extend #\" qstring)
             qstring
         ) )
         #+WIN32
         (shell-simple-quote (string) ; protect against spaces only
           ; Also protect the characters which are special for the command
           ; interpreter. This is needed only if the command interpreter
           ; will be called, but doesn't hurt if CreateProcess() will be
           ; called directly.
           (if (or (eql (length string) 0)
                   (some #'(lambda (c)
                             (or ; space?
                                 (<= (char-code c) 32)
                                 ; special delimiter?
                                 (eql c #\&)
                                 (eql c #\<)
                                 (eql c #\>)
                                 (eql c #\|)
                                 (eql c #\^)
                           ) )
                         string
               )   )
             (string-concat "\"" string "\"")
             string
         ) )
         #+WIN32
         (shell-quote (string) ; full protection
           (let ((qstring (make-array 10 :element-type 'string-char
                                         :adjustable t :fill-pointer 0))
                 (backslashes 0))
             (map nil #'(lambda (c)
                          (when (eql c #\")
                            (dotimes (i (+ backslashes 1))
                              (vector-push-extend #\\ qstring)
                          ) )
                          (vector-push-extend c qstring)
                          (if (eql c #\\)
                            (incf backslashes)
                            (setq backslashes 0)
                        ) )
                      string
             )
             (shell-simple-quote qstring)
         ) )
         ; conversion to a string that works for a pathname as well
         (xstring (object)
           (if (pathnamep object) (namestring object t) (string object))
        ))
  #+WIN32
  (defun execute (programfile &rest arguments)
    (shell
      (apply #'string-concat
             (shell-simple-quote (xstring programfile))
             (mapcan #'(lambda (argument)
                         (list " " (shell-quote (xstring argument)))
                       )
                     arguments
  ) ) )      )
  (defun run-shell-command (command &key (input ':terminal) (output ':terminal)
                                         (if-output-exists ':overwrite)
                                         #+UNIX (may-exec nil)
                                         #+WIN32 (indirectp nil)
                           )
    (case input
      ((:TERMINAL :STREAM) )
      (t (if (eq input 'NIL)
           (setq input #+UNIX "/dev/null" #+(or DOS OS/2 WIN32) "nul")
           (setq input (xstring input))
         )
         (setq command (string-concat command " < " (shell-quote input)))
         #+WIN32 (setq indirectp t)
    ) )
    (case output
      ((:TERMINAL :STREAM) )
      (t (if (eq output 'NIL)
           (setq output #+UNIX "/dev/null" #+(or DOS OS/2 WIN32) "nul"
                 if-output-exists ':OVERWRITE
           )
           (progn
             (setq output (xstring output))
             (when (and (eq if-output-exists ':ERROR) (probe-file output))
               (setq output (pathname output))
               (error-of-type 'file-error
                 :pathname output
                 (DEUTSCH "~S: Eine Datei ~S existiert bereits."
                  ENGLISH "~S: File ~S already exists"
                  FRANCAIS "~S : Le fichier ~S existe déjà.")
                 'run-shell-command output
         ) ) ) )
         (setq command
               (string-concat command
                 (ecase if-output-exists
                   ((:OVERWRITE :ERROR) " > ")
                   (:APPEND " >> ")
                 )
                 (shell-quote output)
         )     )
         #+WIN32 (setq indirectp t)
    ) )
    #+UNIX
    (when may-exec
      ; Wenn die ausführende Shell die "/bin/sh" ist und command eine
      ; "simple command" im Sinne von sh(1), können wir ein wenig optimieren:
      (setq command (string-concat "exec " command))
    )
    #+WIN32
    (when indirectp
      (setq command (string-concat (shell-name) " /c " command))
    )
    (if (eq input ':STREAM)
      (if (eq output ':STREAM)
        (make-pipe-io-stream command)
        (make-pipe-output-stream command)
      )
      (if (eq output ':STREAM)
        (make-pipe-input-stream command)
        (shell command) ; unter UNIX evtl. " &" anfügen, um Hintergrund-Prozeß zu bekommen
    ) )
  )
  (defun run-program (program &key (arguments '())
                                   (input ':terminal) (output ':terminal)
                                   (if-output-exists ':overwrite)
                                   #+WIN32 (indirectp nil)
                     )
    (run-shell-command
      (apply #'string-concat
             (shell-simple-quote (xstring program))
             (mapcan #'(lambda (argument)
                         (list " " (shell-quote (xstring argument)))
                       )
                     arguments
      )      )
      #+UNIX :may-exec #+UNIX t
      #+WIN32 :indirectp #+WIN32 indirectp
      :input input :output output :if-output-exists if-output-exists
  ) )
)

