;;;; Apropos, Describe

(in-package "CLOS")

;;-----------------------------------------------------------------------------
;; DESCRIBE

(defvar *describe-nesting* 0)
(defvar *describe-done* nil)

(defun describe-slotted-object (object s)
  (let ((slotnames (mapcar #'slotdef-name (class-slots (class-of object)))))
    (if slotnames
        (let* ((slotstrings (mapcar #'write-to-string slotnames)) more
               (tabpos (+ 4 (reduce #'max (mapcar #'length slotstrings)))))
          (format s (DEUTSCH "~%~v,vtSlots:"
                     ENGLISH "~%~v,vtSlots:"
                     FRANCAIS "~%~v,vtComposants:")
                  *describe-nesting* lisp:*print-indent-lists*)
          (mapc #'(lambda (slotname slotstring)
                    (format s "~%~v,vt  ~A~VT" (1+ *describe-nesting*)
                            lisp:*print-indent-lists* slotstring tabpos)
                    (cond ((slot-boundp object slotname)
                           (format s "=  ~S" (slot-value object slotname))
                           (pushnew (slot-value object slotname) more))
                          ((format s (DEUTSCH "ohne Wert"
                                      ENGLISH "unbound"
                                      FRANCAIS "aucune valeur")))))
                slotnames slotstrings)
          (dolist (vv (nreverse more)) (describe vv)))
        (format s (DEUTSCH "~%~v,vtKeine Slots."
                   ENGLISH "~%~v,vtNo slots."
                   FRANCAIS "~%~v,vtAucun composant.")
                *describe-nesting* lisp:*print-indent-lists*))))

(defgeneric describe-object (obj stream)
  (:method
   ((obj t) (stream stream))
   (ecase (sys::type-of obj)
     #+(or AMIGA FFI)
     (foreign-pointer
      (format stream (DEUTSCH "ein Foreign-Pointer."
                      ENGLISH "a foreign pointer"
                      FRANCAIS "un pointeur étranger.")))
     #+FFI
     (foreign-address
      (format stream (DEUTSCH "eine Foreign-Adresse."
                      ENGLISH "a foreign address"
                      FRANCAIS "une addresse étrangère.")))
     #+FFI
     (foreign-variable
      (format stream
              (DEUTSCH "eine Foreign-Variable vom Foreign-Typ ~S."
               ENGLISH "a foreign variable of foreign type ~S."
               FRANCAIS "une variable étrangère de type étranger ~S.")
              (sys::deparse-c-type (sys::%record-ref obj 3))))
     #+FFI
     (foreign-function
      (format stream (DEUTSCH "eine Foreign-Funktion."
                      ENGLISH "a foreign function."
                      FRANCAIS "une fonction étrangère.")))
     (byte
      (format stream (DEUTSCH "ein Byte-Specifier, bezeichnet die ~S Bits ab Bitposition ~S eines Integers."
                      ENGLISH "a byte specifier, denoting the ~S bits starting at bit position ~S of an integer."
                      FRANCAIS "un intervalle de bits, comportant ~S bits à partir de la position ~S d'un entier.")
              (byte-size obj) (byte-position obj)))
     (load-time-eval
      (format stream
              (DEUTSCH "eine Absicht der Evaluierung zur Ladezeit." ; ??
               ENGLISH "a load-time evaluation promise." ; ??
               FRANCAIS "une promesse d'évaluation au moment du chargement.")))
     (weak-pointer
      (multiple-value-bind (value validp) (weak-pointer-value obj)
        (if validp
            (progn
              (format stream (DEUTSCH "ein für die GC unsichtbarer Pointer auf ~S."
                              ENGLISH "a GC-invisible pointer to ~S."
                              FRANCAIS "un pointeur, invisible pour le GC, sur ~S.")
                      value)
              (describe value))
            (format stream (DEUTSCH "ein für die GC unsichtbarer Pointer auf ein nicht mehr existierendes Objekt."
                            ENGLISH "a GC-invisible pointer to a now defunct object."
                            FRANCAIS "un pointeur, invisible pour le GC, sur un objet qui n'existe plus.")))))
     (read-label
      (format stream (DEUTSCH "eine Markierung zur Auflösung von #~D#-Verweisen bei READ."
                      ENGLISH "a label used for resolving #~D# references during READ."
                      FRANCAIS "une marque destinée à résoudre #~D# au cours de READ.")
              (logand (sys::address-of obj)
                      '#,(ash most-positive-fixnum -1))))
     (frame-pointer
      (format stream (DEUTSCH "ein Pointer in den Stack. Er zeigt auf:"
                      ENGLISH "a pointer into the stack. It points to:"
                      FRANCAIS "un pointeur dans la pile. Il pointe vers :"))
      (sys::describe-frame stream obj))
     (system-internal
      (format stream (DEUTSCH "ein Objekt mit besonderen Eigenschaften."
                      ENGLISH "a special-purpose object."
                      FRANCAIS "un objet distingué.")))
     (address
      (format stream (DEUTSCH "eine Maschinen-Adresse."
                      ENGLISH "a machine address."
                      FRANCAIS "une addresse au niveau de la machine.")))))
  (:method
   ((obj standard-object) (stream stream))
      (format stream (DEUTSCH "eine Instanz der CLOS-Klasse ~S."
                      ENGLISH "an instance of the CLOS class ~S."
                      FRANCAIS "un objet appartenant à la classe ~S de CLOS.")
              (clos:class-of obj))
   (describe-slotted-object obj stream))
  (:method ((obj structure-object) (stream stream)) ; CLISP specific
           (format stream (DEUTSCH "eine Structure vom Typ ~S."
                           ENGLISH "a structure of type ~S."
                           FRANCAIS "une structure de type ~S.")
                   (type-of obj))
           (let ((types (butlast (cdr (sys::%record-ref obj 0)))))
             (when types
               (format stream (DEUTSCH "~%Als solche ist sie auch eine Structure vom Typ ~{~S~^, ~}."
                               ENGLISH "~%As such, it is also a structure of type ~{~S~^, ~}."
                               FRANCAIS "~%En tant que telle, c'est aussi une structure de type ~{~S~^, ~}.")
                       types)))
           (describe-slotted-object obj stream))
  (:method ((obj cons) (stream stream))
           (let ((len
                  (do ((n 0 (+ n 2)) (f obj (cddr f)) (s obj (cdr s))) (nil)
                    (when (atom f) (return n))
                    (when (atom (cdr f)) (return (1+ n)))
                    (when (eq (cdr f) s) (return nil)))))
             (if len
                 (if (null (nthcdr len obj))
                     (format stream (DEUTSCH "eine Liste der Länge ~S."
                                     ENGLISH "a list of length ~S."
                                     FRANCAIS "une liste de longueur ~S.")
                             len)
                     (if (> len 1)
                         (format stream
                                 (DEUTSCH "eine punktierte Liste der Länge ~S."
                                  ENGLISH "a dotted list of length ~S."
                                  FRANCAIS "une liste pointée de longueur ~S.")
                                 len)
                         (format stream (DEUTSCH "ein Cons."
                                         ENGLISH "a cons."
                                         FRANCAIS "un «cons»."))))
                 (format stream (DEUTSCH "eine zyklische Liste."
                                 ENGLISH "a cyclic list."
                                 FRANCAIS "une liste circulaire.")))))
  (:method ((obj null) (stream stream))
           (format stream (DEUTSCH "die leere Liste, "
                           ENGLISH "the empty list, "
                           FRANCAIS "la liste vide, "))
           (call-next-method))
  (:method ((obj symbol) (stream stream))
           (format stream (DEUTSCH "das Symbol ~S, "
                           ENGLISH "the symbol ~S, "
                           FRANCAIS "le symbole ~S, ")
                   obj)
           (let ((home (symbol-package obj)) mored moree)
             (cond (home
                    (format stream (DEUTSCH "liegt in ~S"
                                    ENGLISH "lies in ~S"
                                    FRANCAIS "est situé dans ~S")
                            home)
                    (pushnew home mored))
                   (t (format stream
                              (DEUTSCH "ist uninterniert"
                               ENGLISH "is uninterned"
                               FRANCAIS "n'appartient à aucun paquetage"))))
             (let ((accessible-packs nil)
                   (*print-escape* t) (*print-readably* nil))
               (let ((normal-printout
                      (if home
                          (let ((*package* home)) (prin1-to-string obj))
                          (let ((*print-gensym* nil)) (prin1-to-string obj)))))
                 (dolist (pack (list-all-packages))
                   (when ; obj in pack accessible?
                       (string=
                        (let ((*package* pack)) (prin1-to-string obj))
                        normal-printout)
                     (push pack accessible-packs))))
               (when accessible-packs
                 (format stream (DEUTSCH ", ist in ~:[der Package~;den Packages~] ~{~A~^, ~} accessible"
                                 ENGLISH ", is accessible in the package~:[~;s~] ~{~A~^, ~}"
                                 FRANCAIS ", est visible dans le~:[ paquetage~;s paquetages~] ~{~A~^, ~}")
                         (cdr accessible-packs)
                         (sort (mapcar #'package-name accessible-packs)
                               #'string<))))
             (when (keywordp obj)
               (format stream (DEUTSCH ", ist ein Keyword"
                               ENGLISH ", is a keyword"
                               FRANCAIS ", est un mot-clé")))
             (when (boundp obj)
               (if (constantp obj)
                   (format stream (DEUTSCH ", eine Konstante"
                                   ENGLISH ", a constant"
                                   FRANCAIS ", une constante"))
                   (if (sys::special-variable-p obj)
                       (format stream
                               (DEUTSCH ", eine SPECIAL-deklarierte Variable"
                                ENGLISH ", a variable declared SPECIAL"
                                FRANCAIS ", une variable declarée SPECIAL"))
                       (format stream (DEUTSCH ", eine Variable"
                                       ENGLISH ", a variable"
                                       FRANCAIS ", une variable"))))
               (when (symbol-macro-expand obj)
                 (format stream (DEUTSCH " (Macro: ~s)"
                                 ENGLISH " (macro: ~s)"
                                 FRANCAIS " (macro: ~s)")
                         (macroexpand-1 obj))
                 (push `(macroexpand-1 ',obj) moree))
               (format stream (DEUTSCH ", Wert: ~s"
                               ENGLISH ", value: ~s"
                               FRANCAIS ", valeur : ~s")
                       (symbol-value obj))
               (pushnew (symbol-value obj) mored))
             (when (fboundp obj)
               (format stream (DEUTSCH ", benennt "
                               ENGLISH ", names "
                               FRANCAIS ", le nom "))
               (cond ((special-operator-p obj)
                      (format stream (DEUTSCH "eine Special-Form"
                                      ENGLISH "a special form"
                                      FRANCAIS "d'une forme spéciale"))
                      (when (macro-function obj)
                        (format stream (DEUTSCH " mit Macro-Definition"
                                        ENGLISH " with macro definition"
                                        FRANCAIS ", aussi d'un macro"))))
                     ((functionp (symbol-function obj))
                      (format stream (DEUTSCH "eine Funktion"
                                      ENGLISH "a function"
                                      FRANCAIS "d'une fonction")))
                     (t ; (macro-function obj)
                      (format stream (DEUTSCH "einen Macro"
                                      ENGLISH "a macro"
                                      FRANCAIS "d'un macro"))))
               (pushnew (symbol-function obj) mored))
             (when (or (get obj 'system::type-symbol)
                       (get obj 'system::defstruct-description)
                       (get obj 'system::deftype-expander))
               (format stream (DEUTSCH ", benennt einen Typ"
                               ENGLISH ", names a type"
                               FRANCAIS ", le nom d'un type"))
               (when (get obj 'system::deftype-expander)
                 (push `(type-expand-1 ',obj) moree)))
             (when (get obj 'clos::closclass)
               (format stream (DEUTSCH ", benennt eine Klasse"
                               ENGLISH ", names a class"
                               FRANCAIS ", le nom d'une classe")))
             (when (symbol-plist obj)
               (let ((properties
                      (do ((l nil) (pl (symbol-plist obj) (cddr pl)))
                          ((null pl) (nreverse l))
                        (push (car pl) l))))
                 (format stream (DEUTSCH ", hat die Propert~@P ~{~S~^, ~}"
                                 ENGLISH ", has the propert~@P ~{~S~^, ~}"
                                 FRANCAIS ", a ~[~;la propriété~:;les propriétés~] ~{~S~^, ~}")
                         (length properties) properties))
               (push `(symbol-plist ',obj) moree))
             (when moree
               (format stream (DEUTSCH "~%~v,vtMehr Information durch Auswerten von ~{~S~^ oder ~}."
                               ENGLISH "~%~v,vtFor more information, evaluate ~{~S~^ or ~}."
                               FRANCAIS "~%~v,vtPour obtenir davantage d'information, évaluez ~{~S~^ ou ~}.")
                       *describe-nesting* lisp:*print-indent-lists* moree))
             (dolist (zz (nreverse mored)) (describe zz stream))))
  (:method ((obj integer) (stream stream))
           (format stream (DEUTSCH "eine ganze Zahl, belegt ~S Bit~:p, ist als ~:(~A~) repräsentiert."
                           ENGLISH "an integer, uses ~S bit~:p, is represented as a ~(~A~)."
                           FRANCAIS "un nombre entier, occupant ~S bit~:p, est représenté comme ~(~A~).")
                   (integer-length obj) (type-of obj)))
  (:method ((obj ratio) (stream stream))
           (format stream (DEUTSCH "eine rationale, nicht ganze Zahl."
                           ENGLISH "a rational, not integral number."
                           FRANCAIS "un nombre rationnel mais pas entier.")))
  (:method ((obj float) (stream stream))
           (format stream (DEUTSCH "eine Fließkommazahl mit ~S Mantissenbits (~:(~A~))."
                           ENGLISH "a float with ~S bits of mantissa (~(~A~))."
                           FRANCAIS "un nombre à virgule flottante avec une précision de ~S bits (un ~(~A~)).")
                   (float-digits obj) (type-of obj)))
  (:method ((obj complex) (stream stream))
           (format stream (DEUTSCH "eine komplexe Zahl "
                           ENGLISH "a complex number "
                           FRANCAIS "un nombre complexe "))
           (let ((x (realpart obj))
                 (y (imagpart obj)))
             (if (zerop y)
                 (if (zerop x)
                     (format stream (DEUTSCH "im Ursprung"
                                     ENGLISH "at the origin"
                                     FRANCAIS "à l'origine"))
                     (format stream
                             (DEUTSCH "auf der ~:[posi~;nega~]tiven reellen Achse"
                              ENGLISH "on the ~:[posi~;nega~]tive real axis"
                              FRANCAIS "sur la partie ~:[posi~;nega~]tive de l'axe réelle")
                             (minusp x)))
                 (if (zerop x)
                     (format stream (DEUTSCH "auf der ~:[posi~;nega~]tiven imaginären Achse"
                                     ENGLISH "on the ~:[posi~;nega~]tive imaginary axis"
                                     FRANCAIS "sur la partie ~:[posi~;nega~]tive de l'axe imaginaire")
                             (minusp y))
                     (format stream (DEUTSCH "im ~:[~:[ers~;vier~]~;~:[zwei~;drit~]~]ten Quadranten"
                                     ENGLISH "in ~:[~:[first~;fourth~]~;~:[second~;third~]~] the quadrant"
                                     FRANCAIS "dans le ~:[~:[premier~;quatrième~]~;~:[deuxième~;troisième~]~] quartier")
                             (minusp x) (minusp y)))))
           (format stream (DEUTSCH " der Gaußschen Zahlenebene."
                           ENGLISH " of the Gaussian number plane."
                           FRANCAIS " du plan Gaussien.")))
  (:method ((obj character) (stream stream))
           (format stream (DEUTSCH "ein Zeichen"
                           ENGLISH "a character"
                           FRANCAIS "un caractère"))
           (format stream (DEUTSCH "."
                           ENGLISH "."
                           FRANCAIS "."))
           (format stream
                   (DEUTSCH "~%Es ist ein ~:[nicht ~;~]druckbares Zeichen."
                    ENGLISH "~%It is a ~:[non-~;~]printable character."
                    FRANCAIS "~%C'est un caractère ~:[non ~;~]imprimable.")
                   (graphic-char-p obj))
           (unless (standard-char-p obj)
             (format stream
                     (DEUTSCH "~%Seine Verwendung ist nicht portabel."
                      ENGLISH "~%Its use is non-portable."
                      FRANCAIS "~%Il n'est pas portable de l'utiliser."))))
  (:method ((obj stream) (stream stream))
           (format stream (DEUTSCH "ein ~:[~:[geschlossener ~;Output-~]~;~:[Input-~;bidirektionaler ~]~]Stream."
                           ENGLISH "a~:[~:[ closed ~;n output-~]~;~:[n input-~;n input/output-~]~]stream."
                           FRANCAIS "un «stream» ~:[~:[fermé~;de sortie~]~;~:[d'entrée~;d'entrée/sortie~]~].")
                   (input-stream-p obj) (output-stream-p obj)))
  (:method ((obj package) (stream stream))
           (if (package-name obj)
               (progn
                 (format stream (DEUTSCH "die Package mit Namen ~A"
                                 ENGLISH "the package named ~A"
                                 FRANCAIS "le paquetage de nom ~A")
                         (package-name obj))
                 (let ((nicknames (package-nicknames obj)))
                   (when nicknames
                     (format stream
                             (DEUTSCH " und zusätzlichen Namen ~{~A~^, ~}"
                              ENGLISH ". It has the nicknames ~{~A~^, ~}"
                              FRANCAIS ". Il porte aussi les noms ~{~A~^, ~}")
                             nicknames)))
                 (format stream ".")
                 (let ((use-list (package-use-list obj))
                       (used-by-list (package-used-by-list obj)))
                   (format stream (DEUTSCH "~%~v,vtSie "
                                   ENGLISH "~%~v,vtIt "
                                   FRANCAIS "~%~v,vtIl ")
                           *describe-nesting* lisp:*print-indent-lists*)
                   (when use-list
                     (format stream (DEUTSCH "importiert die externen Symbole der Package~:[~;s~] ~{~A~^, ~} und "
                                     ENGLISH "imports the external symbols of the package~:[~;s~] ~{~A~^, ~} and "
                                     FRANCAIS "importe les symboles externes d~:[u paquetage~;es paquetages~] ~{~A~^, ~} et ")
                             (cdr use-list) (mapcar #'package-name use-list)))
                   (let ((L nil)) ; maybe list all exported symbols
                     (do-external-symbols (s obj) (push s L))
                     (setq L (sort L #'string< :key #'symbol-name))
                     (if (= 1 *describe-nesting*)
                         (format stream (DEUTSCH "exportiert ~:[keine Symbole~;die Symbole~:*~{~<~%~:; ~S~>~^~}~%~]"
                                         ENGLISH "exports ~:[no symbols~;the symbols~:*~{~<~%~:; ~S~>~^~}~%~]"
                                         FRANCAIS "~:[n'exporte pas de symboles~;exporte les symboles~:*~{~<~%~:; ~S~>~^~}~%~]")
                                 L)
                         (format stream (DEUTSCH "exportiert ~:[keine Symbole~:;~:*~:d Symbole ~]"
                                         ENGLISH "exports ~[no symbols~:;~:*~:d symbols ~]"
                                         FRANCAIS "~[n'exporte pas de symboles~:;exporte~:* ~:d symboles ~]")
                                 (length L))))
                   (when used-by-list
                     (format stream
                             (DEUTSCH "an die Package~:[~;s~] ~{~A~^, ~}"
                              ENGLISH " to the package~:[~;s~] ~{~A~^, ~}"
                              FRANCAIS " vers le~:[ paquetage~;s paquetages~] ~{~A~^, ~}")
                             (cdr used-by-list)
                             (mapcar #'package-name used-by-list)))
                   (format stream ".")))
               (format stream (DEUTSCH "eine gelöschte Package."
                               ENGLISH "a deleted package."
                               FRANCAIS "un paquetage éliminé."))))
  (:method ((obj hash-table) (stream stream))
           (format stream (DEUTSCH "eine Hash-Tabelle mit ~S Eintr~:*~[ägen~;ag~:;ägen~]."
                           ENGLISH "a hash table with ~S entr~:@P."
                           FRANCAIS "un tableau de hachage avec ~S entrée~:*~[s~;~:;s~].")
                   (hash-table-count obj)))
  (:method ((obj readtable) (stream stream))
           (format stream (DEUTSCH "~:[eine ~;die Common-Lisp-~]Readtable."
                           ENGLISH "~:[a~;the Common Lisp~] readtable."
                           FRANCAIS "~:[un~;le~] tableau de lecture~:*~:[~; de Common Lisp~].")
                   (equalp obj (copy-readtable))))
  (:method ((obj pathname) (stream stream))
           (format stream (DEUTSCH "ein ~:[~;portabler ~]Pathname~:[.~;~:*, aufgebaut aus:~{~A~}~]"
                           ENGLISH "a ~:[~;portable ~]pathname~:[.~;~:*, with the following components:~{~A~}~]"
                           FRANCAIS "un «pathname»~:[~; portable~]~:[.~;~:*, composé de:~{~A~}~]")
                   (sys::logical-pathname-p obj)
                   (mapcan #'(lambda (kw component)
                               (when component
                                 (list (format nil "~%~A = ~A"
                                               (symbol-name kw)
                                               (make-pathname kw component)))))
                           '(:host :device :directory :name :type :version)
                           (list (pathname-host obj)
                                 (pathname-device obj)
                                 (pathname-directory obj)
                                 (pathname-name obj)
                                 (pathname-type obj)
                                 (pathname-version obj)))))
  (:method ((obj random-state) (stream stream))
           (format stream (DEUTSCH "ein Random-State."
                           ENGLISH "a random-state."
                           FRANCAIS "un «random-state».")))
  (:method ((obj array) (stream stream))
           (let ((rank (array-rank obj)) (eltype (array-element-type obj)))
             (format stream
                     (DEUTSCH "ein~:[~; einfacher~] ~A-dimensionaler Array"
                      ENGLISH "a~:[~; simple~] ~A dimensional array"
                      FRANCAIS "une matrice~:[~; simple~] à ~A dimension~:P")
                     (sys::simple-array-p obj) rank)
             (when (eql rank 1)
               (format stream (DEUTSCH " (Vektor)"
                               ENGLISH " (vector)"
                               FRANCAIS " (vecteur)")))
             (unless (eq eltype 'T)
               (format stream (DEUTSCH " von ~:(~A~)s"
                               ENGLISH " of ~(~A~)s"
                               FRANCAIS " de ~(~A~)s")
                       eltype))
             (when (adjustable-array-p obj)
               (format stream (DEUTSCH ", adjustierbar"
                               ENGLISH ", adjustable"
                               FRANCAIS ", ajustable")))
             (when (plusp rank)
               (format stream (DEUTSCH ", der Größe ~{~S~^ x ~}"
                               ENGLISH ", of size ~{~S~^ x ~}"
                               FRANCAIS ", de grandeur ~{~S~^ x ~}")
                       (array-dimensions obj))
               (when (array-has-fill-pointer-p obj)
                 (format stream
                         (DEUTSCH " und der momentanen Länge (Fill-Pointer) ~S"
                          ENGLISH " and current length (fill-pointer) ~S"
                          FRANCAIS " et longueur courante (fill-pointer) ~S")
                         (fill-pointer obj))))
             (format stream "."))))

(defmethod describe-object ((obj function) (stream stream))
  (if (eq 'compiled-function (type-of obj))
      (if (functionp obj)
          (progn                ; SUBR
            (format stream (DEUTSCH "eine eingebaute System-Funktion."
                            ENGLISH "a built-in system function."
                            FRANCAIS "une fonction prédéfinie du système."))
            (multiple-value-bind (name req opt rest-p keywords other-keys)
                (sys::subr-info obj)
              (when name
                (sys::describe-signature stream req opt rest-p
                                         keywords keywords other-keys))))
                                ; FSUBR
          (format stream (DEUTSCH "ein Special-Form-Handler."
                          ENGLISH "a special form handler."
                          FRANCAIS "un interpréteur de forme spéciale.")))
      (let ((compiledp (compiled-function-p obj)))
        (format stream
                (DEUTSCH "eine ~:[interpret~;compil~]ierte Funktion."
                 ENGLISH "a~:[n interpret~; compil~]ed function."
                 FRANCAIS "une fonction ~:[interprét~;compil~]ée.")
                compiledp)
        (if compiledp
            (multiple-value-bind (req opt rest-p key-p keywords other-keys-p)
                (sys::signature obj)
              (sys::describe-signature stream req opt rest-p key-p keywords
                                       other-keys-p)
              (format stream (DEUTSCH "~%~v,vtMehr Information durch Auswerten von ~{~S~^ oder ~}."
                              ENGLISH "~%~v,vtFor more information, evaluate ~{~S~^ or ~}."
                              FRANCAIS "~%~v,vtPour obtenir davantage d'information, évaluez ~{~S~^ ou ~}.")
                      *describe-nesting* lisp:*print-indent-lists*
                      `((DISASSEMBLE #',(sys::closure-name obj)))))
            (progn
              (format stream (DEUTSCH "~%~v,vtArgumentliste: ~S"
                              ENGLISH "~%~v,vtargument list: ~S"
                              FRANCAIS "~%~v,vtListe des arguments: ~S")
                      *describe-nesting* lisp:*print-indent-lists*
                      (car (sys::%record-ref obj 1)))
              (let ((doc (sys::%record-ref obj 2)))
                (when doc
                  (format stream (DEUTSCH "~%~v,vtDokumentation: ~A"
                                  ENGLISH "~%~v,vtdocumentation: ~A"
                                  FRANCAIS "~%~v,vtDocumentation: ~A")
                          *describe-nesting* lisp:*print-indent-lists*
                          doc))))))))

(defun describe (obj &optional stream)
  (cond ((eq stream 'nil) (setq stream *standard-output*))
        ((eq stream 't) (setq stream *terminal-io*)))
  (if (member obj *describe-done* :test #'eq)
      (format stream (DEUTSCH "~%~v,vt~a"
                      ENGLISH "~%~v,vt~a [see above]"
                      FRANCAIS "~%~v,vt~a")
              (1+ *describe-nesting*) lisp:*print-indent-lists* obj)
      (let ((*describe-nesting* (1+ *describe-nesting*))
            (*describe-done* (cons obj *describe-done*))
            (*print-circle* t))
        (format stream
                (DEUTSCH "~%~v,vt~a ist "
                 ENGLISH "~%~v,vt~a is "
                 FRANCAIS "~%~v,vt~a est ")
                *describe-nesting* lisp:*print-indent-lists*
                (sys::write-to-short-string obj sys::*prin-linelength*))
        (describe-object obj stream)))
  (values))

;;-----------------------------------------------------------------------------
(in-package "SYSTEM")

;;-----------------------------------------------------------------------------
;; APROPOS

(defun apropos-list (string &optional (package nil))
  (let* ((L nil)
         (fun #'(lambda (sym)
                  #| (search string (symbol-name sym) :test #'char-equal) |#
                  (when (sys::search-string-equal string sym)
                    (push sym L)))))
    (if package
        (system::map-symbols fun package)
        (system::map-all-symbols fun))
    (stable-sort (delete-duplicates L :test #'eq :from-end t)
                 #'string< :key #'symbol-name)))

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

(defun signature-to-list (req-anz opt-anz rest-p keyword-p keywords
                          allow-other-keys)
  (let ((args '()) (count -1))
      (dotimes (i req-anz)
      (push (intern (format nil "ARG~D" (incf count)) :sys) args))
      (when (plusp opt-anz)
        (push '&OPTIONAL args)
        (dotimes (i opt-anz)
        (push (intern (format nil "ARG~D" (incf count)) :sys) args)))
      (when rest-p
        (push '&REST args)
      (push 'other-args args))
      (when keyword-p
        (push '&KEY args)
      (dolist (kw keywords) (push kw args))
      (when allow-other-keys (push '&ALLOW-OTHER-KEYS args)))
    (nreverse args)))

(defun arglist (func)
  (multiple-value-call #'signature-to-list (function-signature func)))

(defun describe-signature (s req-anz opt-anz rest-p keyword-p keywords
                           allow-other-keys)
  (when s
    (format s (DEUTSCH "~%Argumentliste: "
               ENGLISH "~%argument list: "
               FRANCAIS "~%Liste des arguments : ")))
  (format s "(~{~A~^ ~})"
          (signature-to-list req-anz opt-anz rest-p keyword-p keywords
                             allow-other-keys)))

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
  (if (or (numberp object) (symbolp object)) ; von length und level unbeeinflusst?
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
                       (setq level1 levelm) ; levelm passt, probiere größere
                       (setq level2 levelm) ; levelm passt nicht, probiere kleinere
                 ) ) )
                 level1
             ) )
             (length ; Binärsuche nach dem richtigen length
               (let ((length1 (minlength level)) (length2 (maxlength level)))
                 (loop
                   (when (= (- length2 length1) 1) (return))
                   (let ((lengthm (floor (+ length1 length2) 2)))
                     (if (<= (length (write-to-string object :level level :length lengthm)) max)
                       (setq length1 lengthm) ; lengthm passt, probiere größere
                       (setq length2 lengthm) ; lengthm passt nicht, probiere kleinere
                 ) ) )
                 length1
            )) )
        (write-to-string object :level level :length length)
) ) ) )
