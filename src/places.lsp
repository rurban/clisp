;;; CLISP - PLACES.LSP
;;; CLISP-spezifisch: string-concat, %rplaca, %rplacd, store, %setelt, ...

(in-package "SYSTEM")
;;;----------------------------------------------------------------------------
;;; Funktionen zur Definition und zum Ausnutzen von places:
;;;----------------------------------------------------------------------------
(defun setf-symbol (symbol)
  ;; Return an un-interned symbol for SYSTEM::SETF-FUNCTION
  (make-symbol
    (string-concat
      "(SETF "
      (let ((pack (symbol-package symbol))) (if pack (package-name pack) "#"))
      ":"
      (symbol-name symbol)
      ")"
) ) )
;;;----------------------------------------------------------------------------
(defun get-setf-symbol (symbol) ; return a symbol for SYSTEM::SETF-FUNCTION
  (or (get symbol 'SYSTEM::SETF-FUNCTION)
      (progn
        (when (get symbol 'SYSTEM::SETF-EXPANDER)
          (warn (DEUTSCH "Die Funktion (~S ~S) ist durch einen SETF-Expander verborgen."
                 ENGLISH "The function (~S ~S) is hidden by a SETF expander."
                 FRANCAIS "La fonction (~S ~S) est cach��e par une m��thode SETF.")
                'setf symbol
        ) )
        (setf (get symbol 'SYSTEM::SETF-FUNCTION) (setf-symbol symbol))
) )   )
;;;----------------------------------------------------------------------------
(defun get-funname-symbol (funname) ; Abbildung Funktionsname --> Symbol
  (if (atom funname)
    funname
    (get-setf-symbol (second funname))
) )
;;;----------------------------------------------------------------------------
(defun get-setf-expansion (form &optional (env (vector nil nil)))
  (loop
    ; 1. Schritt: nach globalen SETF-Definitionen suchen:
    (when (and (consp form) (symbolp (car form)))
      (when (global-in-fenv-p (car form) (svref env 1))
        ; Operator nicht lokal definiert
        (let ((plist-info (get (first form) 'SYSTEM::SETF-EXPANDER)))
          (when plist-info
            (return-from get-setf-expansion
              (if (symbolp plist-info) ; Symbol kommt von kurzem DEFSETF
                (do* ((storevar (gensym))
                      (tempvars nil (cons (gensym) tempvars))
                      (tempforms nil (cons (car formr) tempforms))
                      (formr (cdr form) (cdr formr)))
                     ((atom formr)
                      (setq tempforms (nreverse tempforms))
                      (values tempvars
                              tempforms
                              `(,storevar)
                              `(,plist-info ,@tempvars ,storevar)
                              `(,(first form) ,@tempvars)
                     ))
                )
                (let ((argcount (car plist-info)))
                  (if (eql argcount -5)
                    ; (-5 . fun) kommt von DEFINE-SETF-METHOD
                    (funcall (cdr plist-info) form env)
                    ; (argcount . fun) kommt von langem DEFSETF
                    (let ((access-form form)
                          (tempvars '())
                          (tempforms '())
                          (new-access-form '()))
                      (let ((i 0)) ; Argumente-Z��hler
                        ; argcount = -1 falls keine Keyword-Argumente existieren
                        ; bzw.     = Anzahl der einzelnen Argumente vor &KEY,
                        ;          = nil nachdem diese abgearbeitet sind.
                        (dolist (argform (cdr access-form))
                          (when (eql i argcount) (setq argcount nil i 0))
                          (if (and (null argcount) (evenp i))
                            (if (keywordp argform)
                              (push argform new-access-form)
                              (error-of-type 'source-program-error
                                (DEUTSCH "Das Argument ~S zu ~S sollte ein Keyword sein."
                                 ENGLISH "The argument ~S to ~S should be a keyword."
                                 FRANCAIS "L'argument ~S de ~S doit ��tre un mot-cl��.")
                                argform (car access-form)
                            ) )
                            (let ((tempvar (gensym)))
                              (push tempvar tempvars)
                              (push argform tempforms)
                              (push tempvar new-access-form)
                          ) )
                          (incf i)
                      ) )
                      (setq new-access-form
                        (cons (car access-form) (nreverse new-access-form))
                      )
                      (let ((newval-var (map-into (make-list
                                                   (1- (function-signature
                                                        (cdr plist-info))))
                                                  #'gensym)))
                        (values
                          (nreverse tempvars)
                          (nreverse tempforms)
                         newval-var
                         (apply (cdr plist-info) new-access-form newval-var)
                          new-access-form
                ) ) ) ) )
            ) )
    ) ) ) )
    ; 2. Schritt: macroexpandieren
    (when (eq form (setq form (macroexpand-1 form env)))
      (return)
  ) )
  ; 3. Schritt: Default-SETF-Methoden
  (cond ((symbolp form)
         (return-from get-setf-expansion
           (let ((storevar (gensym)))
             (values nil
                     nil
                     `(,storevar)
                     `(SETQ ,form ,storevar)
                     `,form
        )) ) )
        ((and (consp form) (symbolp (car form)))
         (return-from get-setf-expansion
           (do* ((storevar (gensym))
                 (tempvars nil (cons (gensym) tempvars))
                 (tempforms nil (cons (car formr) tempforms))
                 (formr (cdr form) (cdr formr)))
                ((atom formr)
                 (setq tempforms (nreverse tempforms))
                 (values tempvars
                         tempforms
                         `(,storevar)
                         `((SETF ,(first form)) ,storevar ,@tempvars)
                         `(,(first form) ,@tempvars)
                ))
        )) )
        (t (error-of-type 'source-program-error
             (DEUTSCH "Das Argument mu�� eine 'SETF-place' sein, ist aber keine: ~S"
              ENGLISH "Argument ~S is not a SETF place."
              FRANCAIS "L'argument ~S doit repr��senter une place modifiable.")
             form
  )     )  )
)
(defun get-setf-method-multiple-value (&rest args) ; backward compatibility
  (apply #'get-setf-expansion args))
;;;----------------------------------------------------------------------------
(defun get-setf-method (form &optional (env (vector nil nil)))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion form env)
    (unless (and (consp stores) (null (cdr stores)))
      (error-of-type 'source-program-error
        (DEUTSCH "Diese 'SETF-place' produziert mehrere 'Store-Variable': ~S"
         ENGLISH "SETF place ~S produces more than one store variable."
         FRANCAIS "La place modifiable ~S produit plusieurs variables de r��sultat.")
        form
    ) )
    (values vars vals stores store-form access-form)
) )
;;;----------------------------------------------------------------------------
; In einfachen Zuweisungen wie (SETQ foo #:G0) darf #:G0 direkt ersetzt werden.
(defun simple-assignment-p (store-form stores)
  (and (= (length stores) 1)
       (consp store-form)
       (eq (first store-form) 'SETQ)
       (= (length store-form) 3)
       (symbolp (second store-form))
       (simple-use-p (third store-form) (first stores))
) )
(defun simple-use-p (form var)
  (or (eq form var)
      (and (consp form) (eq (first form) 'THE) (= (length form) 3)
           (simple-use-p (third form) var)
) )   )
;;;----------------------------------------------------------------------------
(defun documentation (symbol doctype)
  (unless (function-name-p symbol)
    (error-of-type 'error
      (DEUTSCH "~S: Das ist als erstes Argument unzul��ssig, da kein Symbol: ~S"
       ENGLISH "~S: first argument ~S is illegal, not a symbol"
       FRANCAIS "~S : Le premier argument ~S est invalide car ce n'est pas un symbole.")
      'documentation symbol
  ) )
  (getf (get (get-funname-symbol symbol) 'SYSTEM::DOCUMENTATION-STRINGS) doctype)
)
(defun SYSTEM::%SET-DOCUMENTATION (symbol doctype value)
  (unless (function-name-p symbol)
    (error-of-type 'error
      (DEUTSCH "~S: Das ist als erstes Argument unzul��ssig, da kein Symbol: ~S"
       ENGLISH "~S: first argument ~S is illegal, not a symbol"
       FRANCAIS "~S : Le premier argument ~S est invalide car ce n'est pas un symbole.")
      'documentation symbol
  ) )
  (setq symbol (get-funname-symbol symbol))
  (if (null value)
    (when (getf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype)
      (remf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype)
      nil
    )
    (setf (getf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype) value)
) )
;;;----------------------------------------------------------------------------
(defmacro push (item place &environment env)
  (multiple-value-bind (du va ne se ge) (get-setf-expansion place env)
    (if (simple-assignment-p se ne)
        (subst `(cons ,item ,ge) (car ne) se)
        (do ((bindlist (mapcar #'list du va)) (ns (copy-tree se))
             (nn ne (cdr nn)) its)
            ((null nn)
             `(let* ,bindlist
               (multiple-value-bind ,ne ,item
                 (multiple-value-bind ,(nreverse its) ,ge ,ns))))
          (setq its (cons (gensym) its)
                ns (nsubst `(cons ,(car nn) ,(car its)) (car nn) ns))))))
;;;----------------------------------------------------------------------------
(defmacro define-setf-expander (accessfn lambdalist &body body
                                &environment env)
  (unless (symbolp accessfn)
    (error-of-type 'source-program-error
      (DEUTSCH "Der Name der Access-Function mu�� ein Symbol sein und nicht ~S."
       ENGLISH "The name of the access function must be a symbol, not ~S"
       FRANCAIS "Le nom de la fonction d'acc��s doit ��tre un symbole et non ~S.")
      accessfn
  ) )
  (multiple-value-bind (body-rest declarations docstring)
      (system::parse-body body t env)
    (if (null body-rest) (setq body-rest '(NIL)))
    (let ((name (make-symbol (string-concat "SETF-" (symbol-name accessfn)))))
      (multiple-value-bind (newlambdalist envvar) (remove-env-arg lambdalist name)
        (let ((SYSTEM::%ARG-COUNT 0)
              (SYSTEM::%MIN-ARGS 0)
              (SYSTEM::%RESTP nil)
              (SYSTEM::%LET-LIST nil)
              (SYSTEM::%KEYWORD-TESTS nil)
              (SYSTEM::%DEFAULT-FORM nil)
             )
          (SYSTEM::ANALYZE1 newlambdalist '(CDR SYSTEM::%LAMBDA-LIST)
                            name 'SYSTEM::%LAMBDA-LIST
          )
          (if (null newlambdalist)
            (push `(IGNORE SYSTEM::%LAMBDA-LIST) declarations)
          )
          (let ((lengthtest (sys::make-length-test 'SYSTEM::%LAMBDA-LIST))
                (mainform
                  `(LET* ,(nreverse SYSTEM::%LET-LIST)
                     ,@(if declarations `(,(cons 'DECLARE declarations)))
                     ,@SYSTEM::%KEYWORD-TESTS
                     (BLOCK ,accessfn ,@body-rest)
                   )
               ))
            (if lengthtest
              (setq mainform
                `(IF ,lengthtest
                   (ERROR-OF-TYPE 'PROGRAM-ERROR
                     (DEUTSCH "Der SETF-Expander f��r ~S kann nicht mit ~S Argumenten aufgerufen werden."
                      ENGLISH "The SETF expander for ~S may not be called with ~S arguments."
                      FRANCAIS "L'��expandeur�� SETF pour ~S ne peut pas ��tre appel�� avec ~S arguments.")
                     (QUOTE ,accessfn) (1- (LENGTH SYSTEM::%LAMBDA-LIST))
                   )
                   ,mainform
              )  )
            )
            `(EVAL-WHEN (LOAD COMPILE EVAL)
               (LET ()
                 (DEFUN ,name (SYSTEM::%LAMBDA-LIST ,(or envvar 'SYSTEM::ENV))
                   ,@(if envvar '() '((DECLARE (IGNORE SYSTEM::ENV))))
                   ,mainform
                 )
                 (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER
                   (CONS -5 (FUNCTION ,name))
                 )
                 (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF ',docstring)
                 ',accessfn
             ) )
) ) ) ) ) )
(defmacro define-setf-method (&rest args) ; backward compatibility
  `(define-setf-expander ,@ args))
;;;----------------------------------------------------------------------------
(defmacro defsetf (accessfn &rest args &environment env)
  (cond ((and (consp args) (not (listp (first args))) (symbolp (first args)))
         `(EVAL-WHEN (LOAD COMPILE EVAL)
            (LET ()
              (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER ',(first args))
              (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF
                ,(if (and (null (cddr args))
                          (or (null (second args)) (stringp (second args)))
                     )
                   (second args)
                   (if (cddr args)
                     (error-of-type 'source-program-error
                       (DEUTSCH "Zu viele Argumente f��r DEFSETF: ~S"
                        ENGLISH "Too many arguments to DEFSETF: ~S"
                        FRANCAIS "Trop d'arguments pour DEFSETF : ~S")
                       (cdr args)
                     )
                     (error-of-type 'source-program-error
                       (DEUTSCH "Der Dok.-String zu DEFSETF mu�� ein String sein: ~S"
                        ENGLISH "The doc string to DEFSETF must be a string: ~S"
                        FRANCAIS "La documentation pour DEFSETF doit ��tre un cha��ne : ~S")
                       (second args)
                 ) ) )
              )
              ',accessfn
          ) )
        )
        ((and (consp args) (listp (first args)) (consp (cdr args)) (listp (second args)))
         (when (null (second args))
                (error-of-type 'source-program-error
              (DEUTSCH "Bei DEFSETF mu�� genau eine 'Store-Variable' angegeben werden."
                   ENGLISH "Missing store variable in DEFSETF."
               FRANCAIS "Une variable de r��sultat doit ��tre pr��cis��e dans DEFSETF.")))
         (multiple-value-bind (body-rest declarations docstring)
             (system::parse-body (cddr args) t env)
           (let* (arg-count
                  (setter
                    (let* ((lambdalist (first args))
                           (storevars (second args))
                           (SYSTEM::%ARG-COUNT 0)
                           (SYSTEM::%MIN-ARGS 0)
                           (SYSTEM::%RESTP nil)
                           (SYSTEM::%LET-LIST nil)
                           (SYSTEM::%KEYWORD-TESTS nil)
                           (SYSTEM::%DEFAULT-FORM nil))
                      (SYSTEM::ANALYZE1 lambdalist '(CDR SYSTEM::%ACCESS-ARGLIST)
                                        accessfn 'SYSTEM::%ACCESS-ARGLIST
                      )
                      (setq arg-count (if (member '&KEY lambdalist) SYSTEM::%ARG-COUNT -1))
                      (when declarations (setq declarations `((DECLARE ,@declarations))))
                      `(LAMBDA (SYSTEM::%ACCESS-ARGLIST ,@storevars)
                         ,@(if (null lambdalist)
                             `((DECLARE (IGNORE SYSTEM::%ACCESS-ARGLIST)))
                           )
                         ,@declarations
                         (LET* ,(nreverse SYSTEM::%LET-LIST)
                           ,@declarations
                           ,@SYSTEM::%KEYWORD-TESTS
                           (BLOCK ,accessfn ,@body-rest)
                       ) )
                 )) )
             `(EVAL-WHEN (LOAD COMPILE EVAL)
                (LET ()
                  (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER
                    (CONS ,arg-count
                          (FUNCTION ,(concat-pnames "SETF-" accessfn) ,setter)
                  ) )
                  (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF ,docstring)
                  ',accessfn
              ) )
        )) )
        (t (error-of-type 'source-program-error
             (DEUTSCH "DEFSETF-Aufruf f��r ~S ist falsch aufgebaut."
              ENGLISH "Illegal syntax in DEFSETF for ~S"
              FRANCAIS "Le DEFSETF ~S est mal form��.")
             accessfn
) )     )  )
;;;----------------------------------------------------------------------------
;;; Definition of places:
;;;----------------------------------------------------------------------------
(defsetf aref (array &rest indices) (value)
  `(SYSTEM::STORE ,array ,@indices ,value))
;;;----------------------------------------------------------------------------
(defun SYSTEM::%SETNTH (index list value)
  (let ((pointer (nthcdr index list)))
    (if (null pointer)
      (error-of-type 'error
        (DEUTSCH "(SETF (NTH ...) ...) : Index ~S ist zu gro�� f��r ~S."
         ENGLISH "(SETF (NTH ...) ...) : index ~S is too large for ~S"
         FRANCAIS "(SETF (NTH ...) ...) : L'index ~S est trop grand pour ~S.")
        index list
                            )
      (rplaca pointer value)
                        )
    value
                ))
(defsetf nth SYSTEM::%SETNTH)
;;;----------------------------------------------------------------------------
(defsetf elt SYSTEM::%SETELT)
;;;----------------------------------------------------------------------------
(defsetf rest SYSTEM::%RPLACD)
(defsetf first SYSTEM::%RPLACA)
(defsetf second (list) (value) `(SYSTEM::%RPLACA (CDR ,list) ,value))
(defsetf third (list) (value) `(SYSTEM::%RPLACA (CDDR ,list) ,value))
(defsetf fourth (list) (value) `(SYSTEM::%RPLACA (CDDDR ,list) ,value))
(defsetf fifth (list) (value) `(SYSTEM::%RPLACA (CDDDDR ,list) ,value))
(defsetf sixth (list) (value) `(SYSTEM::%RPLACA (CDR (CDDDDR ,list)) ,value))
(defsetf seventh (list) (value) `(SYSTEM::%RPLACA (CDDR (CDDDDR ,list)) ,value))
(defsetf eighth (list) (value) `(SYSTEM::%RPLACA (CDDDR (CDDDDR ,list)) ,value))
(defsetf ninth (list) (value) `(SYSTEM::%RPLACA (CDDDDR (CDDDDR ,list)) ,value))
(defsetf tenth (list) (value) `(SYSTEM::%RPLACA (CDR (CDDDDR (CDDDDR ,list))) ,value))

(defsetf car SYSTEM::%RPLACA)
(defsetf cdr SYSTEM::%RPLACD)
(defsetf caar (list) (value) `(SYSTEM::%RPLACA (CAR ,list) ,value))
(defsetf cadr (list) (value) `(SYSTEM::%RPLACA (CDR ,list) ,value))
(defsetf cdar (list) (value) `(SYSTEM::%RPLACD (CAR ,list) ,value))
(defsetf cddr (list) (value) `(SYSTEM::%RPLACD (CDR ,list) ,value))
(defsetf caaar (list) (value) `(SYSTEM::%RPLACA (CAAR ,list) ,value))
(defsetf caadr (list) (value) `(SYSTEM::%RPLACA (CADR ,list) ,value))
(defsetf cadar (list) (value) `(SYSTEM::%RPLACA (CDAR ,list) ,value))
(defsetf caddr (list) (value) `(SYSTEM::%RPLACA (CDDR ,list) ,value))
(defsetf cdaar (list) (value) `(SYSTEM::%RPLACD (CAAR ,list) ,value))
(defsetf cdadr (list) (value) `(SYSTEM::%RPLACD (CADR ,list) ,value))
(defsetf cddar (list) (value) `(SYSTEM::%RPLACD (CDAR ,list) ,value))
(defsetf cdddr (list) (value) `(SYSTEM::%RPLACD (CDDR ,list) ,value))
(defsetf caaaar (list) (value) `(SYSTEM::%RPLACA (CAAAR ,list) ,value))
(defsetf caaadr (list) (value) `(SYSTEM::%RPLACA (CAADR ,list) ,value))
(defsetf caadar (list) (value) `(SYSTEM::%RPLACA (CADAR ,list) ,value))
(defsetf caaddr (list) (value) `(SYSTEM::%RPLACA (CADDR ,list) ,value))
(defsetf cadaar (list) (value) `(SYSTEM::%RPLACA (CDAAR ,list) ,value))
(defsetf cadadr (list) (value) `(SYSTEM::%RPLACA (CDADR ,list) ,value))
(defsetf caddar (list) (value) `(SYSTEM::%RPLACA (CDDAR ,list) ,value))
(defsetf cadddr (list) (value) `(SYSTEM::%RPLACA (CDDDR ,list) ,value))
(defsetf cdaaar (list) (value) `(SYSTEM::%RPLACD (CAAAR ,list) ,value))
(defsetf cdaadr (list) (value) `(SYSTEM::%RPLACD (CAADR ,list) ,value))
(defsetf cdadar (list) (value) `(SYSTEM::%RPLACD (CADAR ,list) ,value))
(defsetf cdaddr (list) (value) `(SYSTEM::%RPLACD (CADDR ,list) ,value))
(defsetf cddaar (list) (value) `(SYSTEM::%RPLACD (CDAAR ,list) ,value))
(defsetf cddadr (list) (value) `(SYSTEM::%RPLACD (CDADR ,list) ,value))
(defsetf cdddar (list) (value) `(SYSTEM::%RPLACD (CDDAR ,list) ,value))
(defsetf cddddr (list) (value) `(SYSTEM::%RPLACD (CDDDR ,list) ,value))
;;;----------------------------------------------------------------------------
(defsetf svref SYSTEM::SVSTORE)
(defsetf row-major-aref system::row-major-store)
;;;----------------------------------------------------------------------------
(defun devalue-form (form)      ; (values x y z) ==> (x y z)
  (if (eq (car form) 'values) (cdr form) (list form)))
;;;----------------------------------------------------------------------------
(defmacro pop (place &environment env)
  (multiple-value-bind (du va ne se ge) (get-setf-expansion place env)
    (if (simple-assignment-p se ne)
        `(prog1 (car ,ge) ,(subst `(cdr ,ge) (car ne) se))
        (do ((bindlist (mapcar #'list du va)) (ns (copy-tree se))
             (nn ne (cdr nn)))
            ((null nn)
             `(let* ,bindlist
               (multiple-value-bind ,ne ,ge
                 ,@(devalue-form ns)
                 (values ,@(mapcar (lambda (nn) `(car ,nn)) ne)))))
          (setq ns (nsubst `(cdr ,(car nn)) (car nn) ns))))))
;;;----------------------------------------------------------------------------
(defmacro psetf (&whole form &rest args &environment env)
  (labels ((ps (args)
             (multiple-value-bind (du va ne se ge)
                 (get-setf-expansion (car args) env)
               (declare (ignore ge))
               (when (atom (cdr args))
                 (error-of-type 'source-program-error
                    (DEUTSCH "PSETF mit einer ungeraden Anzahl von Argumenten aufgerufen: ~S"
                     ENGLISH "PSETF called with an odd number of arguments: ~S"
                     FRANCAIS "PSETF fut appel�� avec un nombre impair d'arguments : ~S")
                    form))
               `(let* ,(mapcar #'list du va)
                 (multiple-value-bind ,ne ,(second args)
                   ,@(when (cddr args) (list (ps (cddr args))))
                   ,@(devalue-form se))))))
    (nconc (ps args) (list nil))))
;;;----------------------------------------------------------------------------
(defmacro pushnew (item place &rest keylist &environment env)
  (multiple-value-bind (du va ne se ge) (get-setf-expansion place env)
    (if (simple-assignment-p se ne)
        (subst `(adjoin ,item ,ge ,@keylist) (car ne) se)
        (do ((bindlist (mapcar #'list du va)) (ns (copy-tree se))
             (nn ne (cdr nn)) its)
            ((null nn)
             `(let* ,bindlist
               (multiple-value-bind ,ne ,item
                 (multiple-value-bind ,(nreverse its) ,ge ,ns))))
          (setq its (cons (gensym) its)
                ns (nsubst `(adjoin ,(car nn) ,(car its) ,@keylist)
                           (car nn) ns))))))
;;;----------------------------------------------------------------------------
(defmacro remf (place indicator &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place env)
    (let* ((indicatorvar (gensym))
           (bindlist (nconc (mapcar #'list SM1 SM2)
                            `((,(first SM3) ,SM5) (,indicatorvar ,indicator))))
           (var1 (gensym)) (var2 (gensym)))
          `(LET* ,(nreverse bindlist)
             (DO ((,var1 ,(first SM3) (CDDR ,var1))
                  (,var2 NIL ,var1))
                 ((ATOM ,var1) NIL)
               (COND ((ATOM (CDR ,var1))
                      (ERROR-OF-TYPE 'ERROR
                   (DEUTSCH "REMF: Property-Liste ungerader L��nge aufgetreten."
                         ENGLISH "REMF: property list with an odd length"
                    FRANCAIS "REMF : Occurence d'une liste de propri��t��s de longueur impaire.")))
                     ((EQ (CAR ,var1) ,indicatorvar)
                      (IF ,var2
                        (RPLACD (CDR ,var2) (CDDR ,var1))
                        ,(let ((newvalform `(CDDR ,(first SM3))))
                           (if (simple-assignment-p SM4 SM3)
                             (subst newvalform (first SM3) SM4)
                               `(PROGN (SETQ ,(first SM3) ,newvalform) ,SM4))))
                 (RETURN T))))))))
;;;----------------------------------------------------------------------------
(defmacro rotatef (&rest args &environment env)
  (when (null args) (return-from rotatef nil))
  (when (null (cdr args)) (return-from rotatef `(progn ,(car args) nil)))
  (do* ((arglist args (cdr arglist))
        (res (list 'let* nil nil)) lf
        (tail (cdr res)) bindlist stores lv fv)
       ((null arglist)
        (setf (second res) (nreverse bindlist)
              (second (third res)) lv
              (cdr tail) (nconc (nreverse stores) (devalue-form lf))
              (cdr (last res)) (list nil))
        res)
    (multiple-value-bind (du va ne se ge)
        (get-setf-expansion (first arglist) env)
      (setq bindlist (nconc (nreverse (mapcar #'list du va)) bindlist))
      (setf (cadr tail) (list 'multiple-value-bind lv ge nil))
      (setq tail (cddadr tail))
      (if (null fv) (setq fv ne)
          (setq stores (nconc (nreverse (devalue-form lf)) stores)))
      (setq lv ne lf se))))
;;;----------------------------------------------------------------------------
(defmacro define-modify-macro (name lambdalist function &optional docstring)
  (let* ((varlist nil)
         (restvar nil))
    (do* ((lambdalistr lambdalist (cdr lambdalistr))
          (next))
         ((null lambdalistr))
      (setq next (first lambdalistr))
      (cond ((eq next '&OPTIONAL))
            ((eq next '&REST)
             (if (symbolp (second lambdalistr))
               (setq restvar (second lambdalistr))
               (error-of-type 'source-program-error
                 (DEUTSCH "In der Definition von ~S ist die &REST-Variable kein Symbol: ~S"
                  ENGLISH "In the definition of ~S: &REST variable ~S should be a symbol."
                  FRANCAIS "Dans la d��finition de ~S la variable pour &REST n'est pas un symbole : ~S.")
                 name (second lambdalistr)
             ) )
             (if (null (cddr lambdalistr))
               (return)
               (error-of-type 'source-program-error
                 (DEUTSCH "Nach &REST ist nur eine Variable erlaubt; es kam: ~S"
                  ENGLISH "Only one variable is allowed after &REST, not ~S"
                  FRANCAIS "Une seule variable est permise pour &REST et non ~S.")
                 lambdalistr
            )) )
            ((or (eq next '&KEY) (eq next '&ALLOW-OTHER-KEYS) (eq next '&AUX))
             (error-of-type 'source-program-error
               (DEUTSCH "In einer DEFINE-MODIFY-MACRO-Lambdaliste ist ~S unzul��ssig."
                ENGLISH "Illegal in a DEFINE-MODIFY-MACRO lambda list: ~S"
                FRANCAIS "~S n'est pas permis dans une lambda-liste pour DEFINE-MODIFY-MACRO.")
               next
            ))
            ((symbolp next) (push next varlist))
            ((and (listp next) (symbolp (first next)))
             (push (first next) varlist)
            )
            (t (error-of-type 'source-program-error
                 (DEUTSCH "Lambdalisten d��rfen nur Symbole und Listen enthalten, nicht aber ~S"
                  ENGLISH "lambda list may only contain symbols and lists, not ~S"
                  FRANCAIS "Les lambda-listes ne peuvent contenir que des symboles et des listes et non ~S.")
                 next
            )  )
    ) )
    (setq varlist (nreverse varlist))
    `(DEFMACRO ,name (%REFERENCE ,@lambdalist &ENVIRONMENT ENV) ,docstring
       (MULTIPLE-VALUE-BIND (DUMMIES VALS NEWVAL SETTER GETTER)
           (GET-SETF-METHOD %REFERENCE ENV)
         (DO ((D DUMMIES (CDR D))
              (V VALS (CDR V))
              (LET-LIST NIL (CONS (LIST (CAR D) (CAR V)) LET-LIST)))
             ((NULL D)
              (WHEN (SYMBOLP GETTER)
                (RETURN
                  (SUBST
                    (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)
                    (CAR NEWVAL)
                    SETTER
              ) ) )
              (PUSH
                (LIST
                  (CAR NEWVAL)
                  (IF (AND (LISTP %REFERENCE) (EQ (CAR %REFERENCE) 'THE))
                    (LIST 'THE (CADR %REFERENCE)
                      (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)
                    )
                    (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)
                ) )
                LET-LIST
              )
              (LIST 'LET* (NREVERSE LET-LIST) SETTER)
     ) ) ) )
) )
;;;----------------------------------------------------------------------------
(define-modify-macro decf (&optional (delta 1)) -)
;;;----------------------------------------------------------------------------
(define-modify-macro incf (&optional (delta 1)) +)
;;;----------------------------------------------------------------------------
(defmacro setf (&whole form &rest args &environment env)
  (let ((argcount (length args)))
    (cond ((eql argcount 2)
           (let* ((place (first args))
                  (value (second args)))
             (loop
               ; 1. Schritt: nach globalen SETF-Definitionen suchen:
               (when (and (consp place) (symbolp (car place)))
                 (when (global-in-fenv-p (car place) (svref env 1))
                   ; Operator nicht lokal definiert
                   (let ((plist-info (get (first place) 'SYSTEM::SETF-EXPANDER)))
                     (when plist-info
                       (return-from setf
                         (cond ((symbolp plist-info) ; Symbol kommt von kurzem DEFSETF
                                `(,plist-info ,@(cdr place) ,value)
                               )
                               ((and (eq (first place) 'THE) (eql (length place) 3))
                                `(SETF ,(third place) (THE ,(second place) ,value))
                               )
                               ((and (eq (first place) 'VALUES-LIST) (eql (length place) 2))
                                `(VALUES-LIST
                                   (SETF ,(second place)
                                         (MULTIPLE-VALUE-LIST ,value)
                               ) ) )
                               (t
                                (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
                                    (get-setf-expansion place env)
                                  (declare (ignore SM5))
                                  (let ((bindlist (mapcar #'list SM1 SM2)))
                                    (if (eql (length SM3) 1) ; store variable
                                          `(LET* ,(nreverse
                                                    (cons `(,(first SM3) ,value)
                                                       bindlist))
                                          ,SM4)
                                        ;; mehrere Store-Variable
                                        (if ;; Hat SM4 die Gestalt
                                         ;; (VALUES (SETQ v1 store1) ...) ?
                                         (and (consp SM4)
                                              (eq (car SM4) 'VALUES)
                                              (do ((SM3r SM3 (cdr SM3r))
                                                   (SM4r (cdr SM4) (cdr SM4r)))
                                                  ((or (null SM3r) (null SM4r))
                                                   (and (null SM3r) (null SM4r)))
                                                (unless (simple-assignment-p (car SM4r) (list (car SM3r)))
                                                  (return nil))))
                                            (let ((vlist (mapcar #'second (rest SM4))))
                                              `(LET* ,(nreverse bindlist)
                                                 (MULTIPLE-VALUE-SETQ ,vlist ,value)
                                             (VALUES ,@vlist)))
                                            `(LET* ,(nreverse bindlist)
                                               (MULTIPLE-VALUE-BIND ,SM3 ,value
                                             ,SM4)))))))))))))
               ;; 2. Schritt: macroexpandieren
               (when (eq place (setq place (macroexpand-1 place env)))
                 (return)
             ) )
             ; 3. Schritt: Default-SETF-Methoden
             (cond ((symbolp place)
                    `(SETQ ,place ,value)
                   )
                   ((and (consp form) (symbolp (car form)))
                    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
                        (get-setf-expansion place env)
                      (declare (ignore SM5))
                      ; SM4 hat die Gestalt `((SETF ,(first place)) ,@SM3 ,@SM1).
                      ; SM3 ist ��berfl��ssig.
                      `(LET* ,(mapcar #'list SM1 SM2)
                         ,(subst value (first SM3) SM4)
                       )
                   ))
                   (t (error-of-type 'source-program-error
                        (DEUTSCH "Das ist keine erlaubte 'SETF-Place' : ~S"
                         ENGLISH "Illegal SETF place: ~S"
                         FRANCAIS "Ceci n'est pas une place modifiable valide : ~S")
                        (first args)
             )     )  )
          ))
          ((oddp argcount)
           (error-of-type 'source-program-error
             (DEUTSCH "~S mit einer ungeraden Anzahl von Argumenten aufgerufen: ~S"
              ENGLISH "~S called with an odd number of arguments: ~S"
              FRANCAIS "~S fut appel�� avec un nombre impair d'arguments : ~S")
             'setf form
          ))
          (t (do* ((arglist args (cddr arglist))
                   (L nil))
                  ((null arglist) `(LET () (PROGN ,@(nreverse L))))
               (push `(SETF ,(first arglist) ,(second arglist)) L)
          )  )
) ) )
;;;----------------------------------------------------------------------------
(defmacro shiftf (&whole form &rest args &environment env)
  (when (< (length args) 2)
    (error-of-type 'source-program-error
      (DEUTSCH "SHIFTF mit zu wenig Argumenten aufgerufen: ~S"
       ENGLISH "SHIFTF called with too few arguments: ~S"
       FRANCAIS "SHIFTF fut appel�� avec trop peu d'arguments : ~S")
      form))
  (do* ((arglist args (cdr arglist))
        (res (list 'let* nil nil)) lf ff
        (tail (cdr res)) bindlist stores lv fv)
       ((null (cdr arglist))
        (setf (second res) (nreverse bindlist)
              (cadr tail)
              (list 'multiple-value-bind lv (car (last args)) nil)
              tail (cddadr tail)
              (cdr tail) (nconc (nreverse stores) (devalue-form lf))
              (cdr (last res)) (list ff))
        res)
    (multiple-value-bind (du va ne se ge)
        (get-setf-expansion (first arglist) env)
      (setq bindlist (nconc (nreverse (mapcar #'list du va)) bindlist))
      (if (null fv) (setq fv ne ff ge)
          (setf stores (nconc (nreverse (devalue-form lf)) stores)
                (cadr tail) (list 'multiple-value-bind lv ge nil)
                tail (cddadr tail)))
      (setq lv ne lf se))))
;;;----------------------------------------------------------------------------
;;; more places
;;;----------------------------------------------------------------------------
(defsetf GET (symbol indicator &optional default) (value)
  (let ((storeform `(SYSTEM::%PUT ,symbol ,indicator ,value)))
    (if default
      `(PROGN ,default ,storeform) ; default wird nur zum Schein ausgewertet
      `,storeform
) ) )
;;;----------------------------------------------------------------------------
;;; Schreibt zu einem bestimmten Indicator einen Wert in eine gegebene
;;; Propertyliste. Wert ist NIL falls erfolgreich getan oder die neue
;;; (erweiterte) Propertyliste.
(defun sys::%putf (plist indicator value)
  (do ((plistr plist (cddr plistr)))
      ((atom plistr) (list* indicator value plist))
    (when (atom (cdr plistr))
      (error-of-type 'error
        (DEUTSCH "(SETF (GETF ...) ...) : Property-Liste ungerader L��nge aufgetaucht."
         ENGLISH "(SETF (GETF ...) ...) : property list with an odd length"
         FRANCAIS "(SETF (GETF ...) ...) : Occurence d'une liste de propri��t��s de longueur impaire.")
    ))
    (when (eq (car plistr) indicator)
      (rplaca (cdr plistr) value)
      (return nil)
) ) )
(define-setf-expander getf (place indicator &optional default &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place env)
    (let* ((storevar (gensym))
           (indicatorvar (gensym))
           (defaultvar-list (if default (list (gensym)) `()))
          )
      (values
        `(,@SM1 ,indicatorvar ,@defaultvar-list)
        `(,@SM2 ,indicator    ,@(if default `(,default) `()))
        `(,storevar)
        `(LET ((,(first SM3) (SYS::%PUTF ,SM5 ,indicatorvar ,storevar)))
           ,@defaultvar-list ; defaultvar zum Schein auswerten
           (WHEN ,(first SM3) ,SM4)
           ,storevar
         )
        `(GETF ,SM5 ,indicatorvar ,@defaultvar-list)
) ) ) )
;;;----------------------------------------------------------------------------
(defsetf GETHASH (key hashtable &optional default) (value)
  (let ((storeform `(SYSTEM::PUTHASH ,key ,hashtable ,value)))
    (if default
      `(PROGN ,default ,storeform) ; default wird nur zum Schein ausgewertet
      `,storeform
) ) )
;;;----------------------------------------------------------------------------
#| ; siehe oben:
(defun SYSTEM::%SET-DOCUMENTATION (symbol doctype value)
  (unless (function-name-p symbol)
    (error-of-type 'error
      (DEUTSCH "Das ist als erstes Argument unzul��ssig, da kein Symbol: ~S"
       ENGLISH "first argument ~S is illegal, not a symbol"
       FRANCAIS "Le premier argument ~S est invalide car ce n'est pas un symbole.")
      symbol
  ) )
  (setq symbol (get-funname-symbol symbol))
  (if (null value)
    (progn (remf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype) nil)
    (setf (getf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype) value)
) )
|#
(defsetf documentation SYSTEM::%SET-DOCUMENTATION)
;;;----------------------------------------------------------------------------
(defsetf fill-pointer SYSTEM::SET-FILL-POINTER)
;;;----------------------------------------------------------------------------
(defsetf readtable-case SYSTEM::SET-READTABLE-CASE)
;;;----------------------------------------------------------------------------
(defsetf SYMBOL-VALUE SYSTEM::SET-SYMBOL-VALUE)
(sys::%putd 'SET #'SYSTEM::SET-SYMBOL-VALUE) ; deprecated alias
;;;----------------------------------------------------------------------------
(defsetf SYMBOL-FUNCTION SYSTEM::%PUTD)
;;;----------------------------------------------------------------------------
(defsetf SYMBOL-PLIST SYSTEM::%PUTPLIST)
;;;----------------------------------------------------------------------------
(defun SYSTEM::SET-FDEFINITION (name value)
  (setf (symbol-function (get-funname-symbol name)) value)
)
(defsetf FDEFINITION SYSTEM::SET-FDEFINITION)
;;;----------------------------------------------------------------------------
(defsetf MACRO-FUNCTION (symbol) (value)
  `(PROGN
     (SETF (SYMBOL-FUNCTION ,symbol) (CONS 'SYSTEM::MACRO ,value))
     (REMPROP ,symbol 'SYSTEM::MACRO)
     ,value
   )
)
;;;----------------------------------------------------------------------------
(defsetf CHAR SYSTEM::STORE-CHAR)
(defsetf SCHAR SYSTEM::STORE-SCHAR)
(defsetf BIT SYSTEM::STORE)
(defsetf SBIT SYSTEM::STORE)
(defsetf SUBSEQ (sequence start &optional end) (value)
  `(PROGN (REPLACE ,sequence ,value :START1 ,start :END1 ,end) ,value)
)
;;;----------------------------------------------------------------------------
(define-setf-expander char-bit (char name &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method char env)
    (let* ((namevar (gensym))
           (storevar (gensym)))
      (values `(,@SM1 ,namevar)
              `(,@SM2 ,name)
              `(,storevar)
              `(LET ((,(first SM3) (SET-CHAR-BIT ,SM5 ,namevar ,storevar)))
                 ,SM4
                 ,storevar
               )
              `(CHAR-BIT ,SM5 ,namevar)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander LDB (bytespec integer &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method integer env)
    (let* ((bytespecvar (gensym))
           (storevar (gensym)))
      (values (cons bytespecvar SM1)
              (cons bytespec SM2)
              `(,storevar)
              `(LET ((,(first SM3) (DPB ,storevar ,bytespecvar ,SM5)))
                 ,SM4
                 ,storevar
               )
              `(LDB ,bytespecvar ,SM5)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander MASK-FIELD (bytespec integer &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method integer env)
    (let* ((bytespecvar (gensym))
           (storevar (gensym)))
      (values (cons bytespecvar SM1)
              (cons bytespec SM2)
              `(,storevar)
              `(LET ((,(first SM3) (DEPOSIT-FIELD ,storevar ,bytespecvar ,SM5)))
                 ,SM4
                 ,storevar
               )
              `(MASK-FIELD ,bytespecvar ,SM5)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander THE (type place &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-expansion place env)
    (values SM1 SM2 SM3
            (let ((ne (copy-tree SM4)))
              (dolist (vv SM3 ne) (nsubst `(THE ,type ,vv) vv ne)))
            `(THE ,type ,SM5)
) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander APPLY (fun &rest args &environment env)
  (if (and (listp fun)
           (eq (list-length fun) 2)
           (eq (first fun) 'FUNCTION)
           (symbolp (second fun))
      )
    (setq fun (second fun))
    (error-of-type 'source-program-error
      (DEUTSCH "SETF von APPLY ist nur f��r Funktionen der Form #'symbol als Argument definiert."
       ENGLISH "SETF APPLY is only defined for functions of the form #'symbol."
       FRANCAIS "Un SETF de APPLY n'est d��fini que pour les fonctions de la forme #'symbole.")
  ) )
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
      (get-setf-expansion (cons fun args) env)
    (unless (eq (car (last args)) (car (last SM2)))
      (error-of-type 'source-program-error
        (DEUTSCH "APPLY von ~S kann nicht als 'SETF-Place' aufgefa��t werden."
         ENGLISH "APPLY on ~S is not a SETF place."
         FRANCAIS "APPLY de ~S ne peux pas ��tre consid��r�� comme une place modifiable.")
        fun
    ) )
    (let ((item (car (last SM1)))) ; 'item' steht f��r eine Argumentliste!
      (labels ((splice (arglist)
                 ; W��rde man in (LIST . arglist) das 'item' nicht als 1 Element,
                 ; sondern gespliced, sozusagen als ',@item', haben wollen, so
                 ; br��uchte man die Form, die (splice arglist) liefert.
                 (if (endp arglist)
                   'NIL
                   (let ((rest (splice (cdr arglist))))
                     (if (eql (car arglist) item)
                       ; ein (APPEND item ...) davorh��ngen, wie bei Backquote
                       (backquote-append item rest)
                       ; ein (CONS (car arglist) ...) davorh��ngen, wie bei Backquote
                       (backquote-cons (car arglist) rest)
              )) ) ) )
        (flet ((call-splicing (form)
                 ; ersetzt einen Funktionsaufruf form durch einen, bei dem
                 ; 'item' nicht 1 Argument, sondern eine Argumentliste liefert
                 (let ((fun (first form))
                       (argform (splice (rest form))))
                   ; (APPLY #'fun argform) vereinfachen:
                   ; (APPLY #'fun NIL) --> (fun)
                   ; (APPLY #'fun (LIST ...)) --> (fun ...)
                   ; (APPLY #'fun (CONS x y)) --> (APPLY #'fun x y)
                   ; (APPLY #'fun (LIST* ... z)) --> (APPLY #'fun ... z)
                   (if (or (null argform)
                           (and (consp argform) (eq (car argform) 'LIST))
                       )
                     (cons fun (cdr argform))
                     (list* 'APPLY
                            (list 'FUNCTION fun)
                            (if (and (consp argform)
                                     (or (eq (car argform) 'LIST*)
                                         (eq (car argform) 'CONS)
                                )    )
                              (cdr argform)
                              (list argform)
              )) ) ) )      )
          (values SM1 SM2 SM3 (call-splicing SM4) (call-splicing SM5))
) ) ) ) )
;;;----------------------------------------------------------------------------
;;; Zus��tzliche Definitionen von places
;;;----------------------------------------------------------------------------
(define-setf-expander funcall (fun &rest args &environment env)
  (unless (and (listp fun)
               (eq (list-length fun) 2)
               (let ((fun1 (first fun)))
                 (or (eq fun1 'FUNCTION) (eq fun1 'QUOTE))
               )
               (symbolp (second fun))
               (setq fun (second fun))
          )
    (error-of-type 'source-program-error
      (DEUTSCH "SETF von FUNCALL ist nur f��r Funktionen der Form #'symbol definiert."
       ENGLISH "SETF FUNCALL is only defined for functions of the form #'symbol."
       FRANCAIS "Un SETF de FUNCALL n'est d��fini que pour les fonctions de la forme #'symbole.")
  ) )
  (get-setf-expansion (cons fun args) env)
)
;;;----------------------------------------------------------------------------
(defsetf GET-DISPATCH-MACRO-CHARACTER
         (disp-char sub-char &optional (readtable '*READTABLE*)) (value)
  `(PROGN (SET-DISPATCH-MACRO-CHARACTER ,disp-char ,sub-char ,value ,readtable) ,value)
)
;;;----------------------------------------------------------------------------
(defsetf long-float-digits SYSTEM::%SET-LONG-FLOAT-DIGITS)
;;;----------------------------------------------------------------------------
#+LOGICAL-PATHNAMES
(defsetf logical-pathname-translations set-logical-pathname-translations)
;;;----------------------------------------------------------------------------
;;; Handhabung von (SETF (VALUES place1 ... placek) form)
;;; --> (MULTIPLE-VALUE-BIND (dummy1 ... dummyk) form
;;;       (SETF place1 dummy1 ... placek dummyk)
;;;       (VALUES dummy1 ... dummyk)
;;;     )
(define-setf-expander values (&rest subplaces &environment env)
  (multiple-value-bind (temps vals stores storeforms accessforms)
      (setf-VALUES-aux subplaces env)
    (values temps
            vals
            stores
            `(VALUES ,@storeforms)
            `(VALUES ,@accessforms)
) ) )
(defun setf-VALUES-aux (places env)
  (do ((temps nil)
       (vals nil)
       (stores nil)
       (storeforms nil)
       (accessforms nil)
       (placesr places))
      ((atom placesr)
       (setq temps (nreverse temps))
       (setq vals (nreverse vals))
       (setq stores (nreverse stores))
       (setq storeforms (nreverse storeforms))
       (setq accessforms (nreverse accessforms))
       (values temps vals stores storeforms accessforms)
      )
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
        (get-setf-method (pop placesr) env)
      (setq temps (revappend SM1 temps))
      (setq vals (revappend SM2 vals))
      (setq stores (revappend SM3 stores))
      (setq storeforms (cons SM4 storeforms))
      (setq accessforms (cons SM5 accessforms))
) ) )
;;;----------------------------------------------------------------------------
;;; Analog zu (MULTIPLE-VALUE-SETQ (var1 ... vark) form) :
;;; (MULTIPLE-VALUE-SETF (place1 ... placek) form)
;;; --> (VALUES (SETF (VALUES place1 ... placek) form))
;;; --> (MULTIPLE-VALUE-BIND (dummy1 ... dummyk) form
;;;       (SETF place1 dummy1 ... placek dummyk)
;;;       dummy1
;;;     )
(defmacro multiple-value-setf (places form &environment env)
  (multiple-value-bind (temps vals stores storeforms accessforms)
      (setf-VALUES-aux places env)
    (declare (ignore accessforms))
    `(LET* ,(mapcar #'list temps vals)
       (MULTIPLE-VALUE-BIND ,stores ,form
         ,@storeforms
         ,(first stores) ; (null stores) -> NIL -> Wert NIL
     ) )
) )
;;;----------------------------------------------------------------------------
