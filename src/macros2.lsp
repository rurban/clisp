(in-package "SYSTEM")
;-------------------------------------------------------------------------------
;; Languages and Internationalization:
;; (sys::current-language)
;;   returns the current language, a symbol.
;; (deflanguage lang [parent-lang])
;;   defines a language, being a descendant of parent-lang.
;; (sys::assert-language lang)
;;   asserts that lang is a valid language.
;; (definternational symbol
;;   { (lang value-form) }*
;;   [ (t lang) | (t (var) value-form*) ]
;; )
;;   defines an internationalized object, with some predefined values in
;;   the specified languages, and a default language or default function for
;;   unspecified languages.
;; (deflocalized symbol lang value-form)
;;   enhances an internationalized object.
;; (localized symbol [lang])
;;   looks up the value of an internationalized object in lang, which defaults
;;   to the current language.
;;
;; There is an analogy between
;;         deflanguage        --   defclass
;;         definternational   --   defgeneric
;;         deflocalized       --   defmethod
;;         localized          --   funcall
;; If you need something like "definternational with arguments", then use
;; defgeneric with EQL methods for the language argument. (Well, language
;; inheritance doesn't work with EQL methods.)
;;
(in-package "LISP")
(export '(deflanguage definternational deflocalized localized))
(in-package "SYSTEM")
(defvar *all-languages* nil)
(defun assert-language (lang)
  (let ((h (assoc lang *all-languages*)))
    (unless h
      (error-of-type 'error
        (DEUTSCH "Sprache ~S ist nicht definiert."
         ENGLISH "Language ~S is not defined"
         FRANCAIS "La langue ~S n'est pas définie.")
        lang
    ) )
    (cdr h)
) )
(defun ensure-language (lang parent-lang)
  (let ((h (assoc lang *all-languages*)))
    (if h
      (unless (eq (cdr h) parent-lang)
        (error-of-type 'error
          (DEUTSCH "Sprache ~S erbt von ~S."
           ENGLISH "Language ~S inherits from ~S"
           FRANCAIS "La langue ~S hérite de ~S.")
          lang (cdr h)
      ) )
      (progn
        (or (null parent-lang) (assert-language parent-lang))
        (setq *all-languages*
              (nconc *all-languages* (list (cons lang parent-lang)))
  ) ) ) )
  lang
)
(defmacro deflanguage (lang &optional parent-lang)
  `(SYSTEM::ENSURE-LANGUAGE ',lang ',parent-lang)
)
(deflanguage ENGLISH)
(deflanguage DEUTSCH)
(deflanguage FRANCAIS)
(defmacro definternational (symbol &rest options)
  `(PROGN
     ,@(mapcap #'(lambda (option)
                   (let ((lang (first option)))
                     (if (eq lang 'T)
                       `((SYS::%PUT ',symbol 'SYS::OTHER-LANGUAGE
                           ,(if (listp (second option))
                              `(FUNCTION (LAMBDA ,@(cdr option)))
                              `(SYSTEM::DEFINTERNATIONAL-DEFAULT
                                 ',symbol ',(second option)
                               )
                            )
                        ))
                       `((ASSERT-LANGUAGE ',lang)
                         (SYS::%PUT ',symbol ',lang ,(second option))
                        )
                 ) ) )
               options
       )
     ',symbol
   )
)
(defmacro deflocalized (symbol lang form)
  `(PROGN
     (ASSERT-LANGUAGE ',lang)
     (SYS::%PUT ',symbol ',lang ,form)
     ',symbol
   )
)
(defun localized (symbol &optional (language (sys::current-language)))
  (let ((notfound '#:notfound)
        (lang language))
    (loop
      (let ((h (assoc lang *all-languages*)))
        (unless h
          (error-of-type 'error
            (DEUTSCH "~S: Sprache ~S ist nicht definiert."
             ENGLISH "~S: Language ~S is not defined"
             FRANCAIS "~S : La langue ~S n'est pas définie.")
            'localized lang
        ) )
        (let ((value (get symbol lang notfound)))
          (unless (eq value notfound) (return-from localized value))
        )
        ; not found -> search parent language
        (setq lang (cdr h))
        ; no parent language -> lookup default function
        (unless lang (return))
  ) ) )
  (let ((h (get symbol 'SYS::OTHER-LANGUAGE)))
    (if h
      (funcall h language)
      ; no more parent language and no lookup default -> return nil
      nil
) ) )
;; Default defaulter: Look up for another language.
(defvar *localized-recursion* nil)
(defun definternational-default (symbol default-language)
  #'(lambda (language)
      (if (eq *localized-recursion* symbol) ; catch endless recursion
        (error-of-type 'error
          (DEUTSCH "~S ~S: kein Wert für Default-Sprache ~S."
           ENGLISH "~S ~S: no value for default language ~S"
           FRANCAIS "~S ~S : valeur pour la langue par défaut ~S manque.")
          'localized symbol language
        )
        (let ((*localized-recursion* symbol))
          (localized symbol default-language)
    ) ) )
)
;-------------------------------------------------------------------------------
(defmacro typecase (keyform &rest typeclauselist)
  (let* ((tempvar (gensym))
         (condclauselist nil))
    (do ((typeclauselistr typeclauselist (cdr typeclauselistr)))
        ((atom typeclauselistr))
      (cond ((atom (car typeclauselistr))
             (error-of-type 'source-program-error
               (DEUTSCH "Unzulässige Klausel in ~S: ~S"
                ENGLISH "Invalid clause in ~S: ~S"
                FRANCAIS "Clause inadmissible dans ~S : ~S")
               'typecase (car typeclauselistr)
            ))
            ((let ((type (caar typeclauselistr)))
               (or (eq type T) (eq type 'OTHERWISE))
             )
             (push `(T ,@(or (cdar typeclauselistr) '(NIL))) condclauselist)
             (return)
            )
            (t (push `((TYPEP ,tempvar (QUOTE ,(caar typeclauselistr)))
                       ,@(or (cdar typeclauselistr) '(NIL))
                      )
                     condclauselist
            )  )
    ) )
    `(LET ((,tempvar ,keyform)) (COND ,@(nreverse condclauselist)))
) )
;-------------------------------------------------------------------------------
(defun type-error-string ()
  (DEUTSCH "~A~%Der Wert ist: ~S"
   ENGLISH "~A~%The value is: ~S"
   FRANCAIS "~A~%La valeur est : ~S")
)
(defun check-type-error-string (place string typespec)
  (format nil
    (DEUTSCH "Der Wert von ~S sollte ~:[vom Typ ~S~;~:*~A~] sein."
     ENGLISH "The value of ~S should be ~:[of type ~S~;~:*~A~]."
     FRANCAIS "La valeur de ~S devrait être ~:[de type ~S~;~:*~A~].")
    place string typespec
) )
(defun report-one-new-value-string ()
  (DEUTSCH "Sie dürfen einen neuen Wert eingeben."
   ENGLISH "You may input a new value."
   FRANCAIS "Vous pouvez entrer une nouvelle valeur.")
)
(defun prompt-for-new-value-string ()
  (DEUTSCH "~%Neues ~S: "
   ENGLISH "~%New ~S: "
   FRANCAIS "~%Nouveau ~S : ")
)
(defmacro check-type (place typespec &optional (string nil))
  (let ((tag1 (gensym))
        (tag2 (gensym)))
    `(TAGBODY
       ,tag1
       (WHEN (TYPEP ,place ',typespec) (GO ,tag2))
       (CERROR (REPORT-ONE-NEW-VALUE-STRING)
         (TYPE-ERROR-STRING)
         (CHECK-TYPE-ERROR-STRING ',place ',string ',typespec)
         ,place
       )
       (FORMAT *QUERY-IO* (PROMPT-FOR-NEW-VALUE-STRING) ',place)
       (SETF ,place (READ *QUERY-IO*))
       (GO ,tag1)
       ,tag2
     )
) )
;-------------------------------------------------------------------------------
(defun report-no-new-value-string ()
  (DEUTSCH "Neuer Anlauf"
   ENGLISH "Retry"
   FRANCAIS "Reéssayer")
)
(defun report-new-values-string ()
  (DEUTSCH "Sie dürfen neue Werte eingeben."
   ENGLISH "You may input new values."
   FRANCAIS "Vous pouvez entrer de nouvelles valeurs.")
)
(defun assert-error-string (test-form)
  (format nil
    (DEUTSCH "Der Wert von ~S darf nicht NIL sein."
     ENGLISH "~S must evaluate to a non-NIL value."
     FRANCAIS "La valeur de ~S ne peut pas être NIL.")
    test-form
) )
(defmacro assert (test-form &optional (place-list nil) (string nil) &rest args)
  (let ((tag1 (gensym))
        (tag2 (gensym)))
    `(TAGBODY
       ,tag1
       (WHEN ,test-form (GO ,tag2))
       (CERROR ,(case (length place-list)
                  (0 `(REPORT-NO-NEW-VALUE-STRING)
                  )
                  (1 `(REPORT-ONE-NEW-VALUE-STRING)
                  )
                  (t `(REPORT-NEW-VALUES-STRING))
                )
               ',(or string "~A")
               ,@(if string
                   args
                   (list `(ASSERT-ERROR-STRING ',test-form))
                 )
       )
       ,@(mapcan
           #'(lambda (place)
               (list `(FORMAT *QUERY-IO* (PROMPT-FOR-NEW-VALUE-STRING) ',place)
                     `(SETF ,place (READ *QUERY-IO*))
             ) )
           place-list
         )
       (GO ,tag1)
       ,tag2
     )
) )
;-------------------------------------------------------------------------------
(defun typecase-error-string (keyform typelist)
  (format nil
    (DEUTSCH "Der Wert von ~S muß einem der Typen ~{~S~^, ~} angehören."
     ENGLISH "The value of ~S must be of one of the types ~{~S~^, ~}"
     FRANCAIS "La valeur de ~S doit appartenir à l'un des types ~{~S~^, ~}.")
    keyform typelist
) )
(defun case-error-string (keyform caselist)
  (format nil
    (DEUTSCH "Der Wert von ~S muß einer der folgenden sein: ~{~S~^, ~}"
     ENGLISH "The value of ~S must be one of ~{~S~^, ~}"
     FRANCAIS "La valeur de ~S doit être l'une des suivantes : ~{~S~^, ~}")
    keyform caselist
) )

;;; Exhaustive Case Analysis

;; These macros are superseded by the corresponding ones from conditio.lsp
;; when the condition system is available.

(flet ((parenthesize-keys (clauses)
         ;; PARENTHESIZE-KEYS is necessary to avoid confusing
         ;; the symbols OTHERWISE and T used as keys, with the same
         ;; symbols used in the syntax of the non exhaustive CASE.
         (mapcar #'(lambda (c)
                     (cond ((or (eq (car c) 't)
                                (eq (car c) 'otherwise))
                            (warn (DEUTSCH "Schlüssel ~S in Fallunterscheidung sollte lieber in Klammern gesetzt werden: ~S"
                                   ENGLISH "~S used as a key in ~S, it would be better to use parentheses."
                                   FRANCAIS "La clé ~S dans ~S devrait être mise entre parenthèses.")
                                  (car c) c)
                            (cons (list (car c)) (cdr c)))
                           (t c)))
                 clauses)))
  (flet ((typecase-errorstring (keyform keyclauselist)
           (let ((typelist (mapcar #'first keyclauselist)))
             `(TYPECASE-ERROR-STRING ',keyform ',typelist)
             ) )
         (typecase-expected-type (keyclauselist)
           `(OR ,@(mapcar #'first keyclauselist))
           )
         (case-errorstring (keyform keyclauselist)
           (let ((caselist
                  (mapcap #'(lambda (keyclause)
                              (setq keyclause (car keyclause))
                              (if (listp keyclause) keyclause (list keyclause))
                              )
                          keyclauselist
                          )) )
             `(CASE-ERROR-STRING ',keyform ',caselist)
             ) )
         (case-expected-type (keyclauselist)
           `(MEMBER ,@(mapcap #'(lambda (keyclause)
                                  (setq keyclause (car keyclause))
                                  (if (listp keyclause) keyclause (list keyclause))
                                  )
                              keyclauselist
                              )         )
           )
         (simply-error (casename form clauselist errorstring expected-type)
           (let ((var (gensym)))
             `(LET ((,var ,form))
               (,casename ,var
                ,@(parenthesize-keys clauselist)
                (OTHERWISE
                 (ERROR-OF-TYPE 'TYPE-ERROR
                                :DATUM ,var :EXPECTED-TYPE ',expected-type
                                (TYPE-ERROR-STRING)
                                ,errorstring ,var
                                ) ) ) )
             ) )
         (retry-loop (casename place clauselist errorstring)
           (let ((g (gensym))
                 (h (gensym)))
             `(BLOCK ,g
               (TAGBODY
                  ,h
                  (RETURN-FROM ,g
                    (,casename ,place
                               ,@(parenthesize-keys clauselist)
                               (OTHERWISE
                                (CERROR (REPORT-ONE-NEW-VALUE-STRING)
                                        (TYPE-ERROR-STRING)
                                        ,errorstring
                                        ,place
                                        )
                                (FORMAT *QUERY-IO* (PROMPT-FOR-NEW-VALUE-STRING) ',place)
                                (SETF ,place (READ *QUERY-IO*))
                                (GO ,h)
                                ) ) ) ) )
             )) )
    (defmacro etypecase (keyform &rest keyclauselist)
      (simply-error 'TYPECASE keyform keyclauselist
                    (typecase-errorstring keyform keyclauselist)
                    (typecase-expected-type keyclauselist)
                    ) )
    (defmacro ctypecase (keyplace &rest keyclauselist)
      (retry-loop 'TYPECASE keyplace keyclauselist
                  (typecase-errorstring keyplace keyclauselist)
                  ) )
    (defmacro ecase (keyform &rest keyclauselist)
      (simply-error 'CASE keyform keyclauselist
                    (case-errorstring keyform keyclauselist)
                    (case-expected-type keyclauselist)
                    ) )
    (defmacro ccase (keyform &rest keyclauselist)
      (retry-loop 'CASE keyform keyclauselist
                  (case-errorstring keyform keyclauselist)
                  ) )
    ) )

;-------------------------------------------------------------------------------
(defmacro deftype (name lambdalist &body body &environment env)
  (unless (symbolp name)
    (error-of-type 'source-program-error
      (DEUTSCH "Typname muß ein Symbol sein, nicht ~S"
       ENGLISH "type name should be a symbol, not ~S"
       FRANCAIS "Le type doit être un symbole et non ~S")
      name
  ) )
  (if (or (get name 'TYPE-SYMBOL) (get name 'TYPE-LIST))
    (error-of-type 'source-program-error
      (DEUTSCH "~S ist ein eingebauter Typ und darf nicht umdefiniert werden."
       ENGLISH "~S is a built-in type and may not be redefined."
       FRANCAIS "~S est un type prédéfini et ne peut pas être redéfini.")
      name
  ) )
  (multiple-value-bind (body-rest declarations docstring)
      (SYSTEM::PARSE-BODY body t env)
    (if declarations (setq declarations (list (cons 'DECLARE declarations))))
    (let ((%arg-count 0) (%min-args 0) (%restp nil)
          (%let-list nil) (%keyword-tests nil) (%default-form '(QUOTE *)))
      (analyze1 lambdalist '(CDR <DEFTYPE-FORM>) name '<DEFTYPE-FORM>)
      (let ((lengthtest (make-length-test '<DEFTYPE-FORM>))
            (mainform `(LET* ,(nreverse %let-list)
                         ,@declarations
                         ,@(nreverse %keyword-tests)
                         (BLOCK ,name ,@body-rest)
           ))          )
        (if lengthtest
          (setq mainform
            `(IF ,lengthtest
               (TYPE-CALL-ERROR <DEFTYPE-FORM>)
               ,mainform
        ) )  )
        `(EVAL-WHEN (COMPILE LOAD EVAL)
           (LET ()
             (%PUT ',name 'DEFTYPE-EXPANDER
               (FUNCTION ,(make-symbol (string-concat "DEFTYPE-" (string name)))
                 (LAMBDA (<DEFTYPE-FORM>) ,mainform)
             ) )
             (SETF (DOCUMENTATION ',name 'TYPE) ',docstring)
             ',name
         ) )
) ) ) )
(defun type-call-error (deftype-form)
  (error-of-type 'error
    (DEUTSCH "Der Deftype-Expander für ~S kann nicht mit ~S Argumenten aufgerufen werden."
     ENGLISH "The deftype expander for ~S may not be called with ~S arguments."
     FRANCAIS "L'«expandeur» de DEFTYPE pour ~S ne peut pas être appelé avec ~S arguments.")
    (car deftype-form) (1- (length deftype-form))
) )
;-------------------------------------------------------------------------------
(defmacro time (form)
  (let ((vars (list (gensym) (gensym) (gensym) (gensym) (gensym) (gensym)
                    (gensym) (gensym) (gensym)
       ))     )
    `(MULTIPLE-VALUE-BIND ,vars (%%TIME)
       (UNWIND-PROTECT ,form (MULTIPLE-VALUE-CALL #'%TIME (%%TIME) ,@vars))
     ) ; Diese Konstruktion verbraucht zur Laufzeit nur Stackplatz!
) )
;-------------------------------------------------------------------------------
(defmacro space (form)
  (let ((var1 (gensym))
        (var2 (gensym))
        (var3 (gensym))
        (var4 (gensym)))
    `(MULTIPLE-VALUE-BIND (,var1 ,var2 ,var3 ,var4) (%SPACE1)
       (UNWIND-PROTECT
         (LET ((*GC-STATISTICS* (1+ (MAX *GC-STATISTICS* 0))))
           (UNWIND-PROTECT
             (SETQ ,var3 (MULTIPLE-VALUE-LIST ,form))
             (SETQ ,var4 (%SPACE2))
         ) )
         (%SPACE ,var1 ,var2 ,var3 ,var4)
       ) ; Diese Konstruktion verbraucht zur Laufzeit nur Stackplatz!
       (VALUES-LIST ,var3)
     )
) )
;-------------------------------------------------------------------------------
(defmacro with-input-from-string
    ((var string &key (index nil sindex) (start '0 sstart) (end 'NIL send))
     &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
    `(LET ((,var (MAKE-STRING-INPUT-STREAM ,string
                   ,@(if (or sstart send)
                       `(,start ,@(if send `(,end) '()))
                       '()
          ))     )   )
       (DECLARE (READ-ONLY ,var) ,@declarations)
       (UNWIND-PROTECT
         (PROGN ,@body-rest)
         ,@(if sindex `((SETF ,index (SYSTEM::STRING-INPUT-STREAM-INDEX ,var))) '())
         (CLOSE ,var)
     ) )
) )
;-------------------------------------------------------------------------------
(defmacro with-open-file ((stream &rest options) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
    `(LET ((,stream (OPEN ,@options)))
       (DECLARE (READ-ONLY ,stream) ,@declarations)
       (UNWIND-PROTECT
         (MULTIPLE-VALUE-PROG1 (PROGN ,@body-rest)
           (WHEN ,stream (CLOSE ,stream))
         )
         (WHEN ,stream (CLOSE ,stream :ABORT T))
     ) )
) )
;-------------------------------------------------------------------------------
(defmacro with-open-stream ((var stream) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
    `(LET ((,var ,stream))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       (UNWIND-PROTECT
         (MULTIPLE-VALUE-PROG1 (PROGN ,@body-rest) (CLOSE ,var))
         (CLOSE ,var :ABORT T)
     ) )
) )
;-------------------------------------------------------------------------------
(defmacro with-output-to-string
    ((var &optional (string nil)) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
    (if string
      `(LET ((,var (SYS::MAKE-STRING-PUSH-STREAM ,string)))
         (DECLARE (READ-ONLY ,var) ,@declarations)
         (UNWIND-PROTECT
           (PROGN ,@body-rest)
           (CLOSE ,var)
       ) )
      `(LET ((,var (MAKE-STRING-OUTPUT-STREAM)))
         ,@declarations
         (UNWIND-PROTECT
           (PROGN ,@body-rest (GET-OUTPUT-STREAM-STRING ,var))
           (CLOSE ,var)
       ) )
) ) )
;-------------------------------------------------------------------------------
(in-package "LISP")
(export 'with-output-to-printer)
(in-package "SYSTEM")
(defmacro with-output-to-printer ((var) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations)))
    )
    `(LET ((,var (SYS::MAKE-PRINTER-STREAM)))
       ,@declarations
       (UNWIND-PROTECT
         (PROGN ,@body-rest)
         (CLOSE ,var)
     ) )
) )
#+UNIX
(defun make-printer-stream () (make-pipe-output-stream "lpr"))
#+(or DOS OS/2 WIN32)
(defun make-printer-stream () (open "prn" :direction :output))
;-------------------------------------------------------------------------------
(in-package "LISP")
(export 'without-floating-point-underflow)
(in-package "SYSTEM")
(defmacro without-floating-point-underflow (&body body)
  `(LET ((SYS::*INHIBIT-FLOATING-POINT-UNDERFLOW* T))
     (PROGN ,@body)
   )
)
;-------------------------------------------------------------------------------

