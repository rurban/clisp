;;; backquote read-macro
;;; Michael Stoll
;;; Rewritten in July/August by Bruno Haible.
;;; Recursive backquote 1989-08-16/17
;;; Adapted to the standard semantics for recursive backquote on 1992-05-24
;;; German comments translated by Mirian Lennox <mirian@cosmic.com> 2003-01-18

(in-package "SYSTEM")

(proclaim '(special *backquote-level*))
;; Either NIL or the number of nested backquote expressions permitted.
;; It is bound in the reader at top level.

(proclaim '(special *nsplice-fun*))
(setq *nsplice-fun* 'NCONC) ; Function which NSPLICE calls
;; (Bound to 'APPEND for the production of the output form in
;; nested backquotes.)

;; Bug: With nested backquotes some partial forms are evaluated several
;; times (e.g. in the primary evaluation forms, which are needed for
;; the interpretation of secondary evaluation forms) and should
;; therefore be free from side-effects.

(defun \`-reader (stream char)
  (declare (ignore char))
  (let* ((*backquote-level* (1+ (or *backquote-level* 0)))
         (skel (read stream t nil t))
         (form (list 'BACKQUOTE
                     (remove-backquote-third skel)
                     (backquote-1 (unquote-level skel)))))
    (when (= *backquote-level* 1) (setq form (elim-unquote-dummy form)))
    form))

(defun \,-reader (stream char &aux (c (peek-char nil stream)))
  (declare (ignore char))
  (cond ((null *backquote-level*)
         (error-of-type 'error
           (TEXT "~S: comma is illegal outside of backquote")
           'read))
        ((zerop *backquote-level*)
         (error-of-type 'error
           (TEXT "~S: more commas out than backquotes in, is illegal")
           'read))
        (t (let ((*backquote-level* (1- *backquote-level*)))
             (cond ((eql c #\@)
                    (read-char stream)
                    (list 'SPLICE (list 'UNQUOTE (read stream t nil t))))
                   ((eql c #\.)
                    (read-char stream)
                    (list 'NSPLICE (list 'UNQUOTE (read stream t nil t))))
                   (t (list 'UNQUOTE (read stream t nil t))))))))

;;(set-macro-character #\` #'\`-reader)
;;(set-macro-character #\, #'\,-reader)

;; Helper functions for macros, which are expanded in backquote forms.
;; (They work only with a single level of backquote nesting.)

(defun add-backquote (skel)
  (list 'BACKQUOTE
        (remove-backquote-third skel)
        (backquote-1 (unquote-level skel))))
(defun add-unquote (skel)
  (list 'UNQUOTE skel))

;; Interpret ...                                as ...
;;  (backquote original-form [expanded-form])    `original-form
;;  (splice (unquote form))                      ,@form
;;  (splice form)                                ,@'form
;;  (nsplice (unquote form))                     ,.form
;;  (nsplice form)                               ,.'form
;;  (unquote form)                               ,form

;;(defmacro backquote (original-form expanded-form)
;;  (declare (ignore original-form))
;;  expanded-form)

(defun remove-backquote-third (skel)
  (cond ((atom skel)
         (if (simple-vector-p skel)
           (map 'vector #'remove-backquote-third skel)
           skel))
        ((and (eq (car skel) 'BACKQUOTE) (consp (cdr skel)))
         (list 'BACKQUOTE (second skel))) ; no third element in the list
        (t (cons (remove-backquote-third (car skel))
                 (remove-backquote-third (cdr skel))))))

;; replace UNQUOTE-DUMMY with UNQUOTE.
(defun elim-unquote-dummy (skel)
  (if (atom skel)
    (cond ((eq skel 'UNQUOTE-DUMMY) 'UNQUOTE)
          ((simple-vector-p skel) (map 'vector #'elim-unquote-dummy skel))
          (t skel))
    (let* ((car (car skel)) (newcar (elim-unquote-dummy car))
           (cdr (cdr skel)) (newcdr (elim-unquote-dummy cdr)))
      (if (and (eq car newcar) (eq cdr newcdr))
        skel
        (cons newcar newcdr)))))

;; converts all UNQUOTEs in "skeleton" skel at level "level+1"
;; (i.e. inside each UNQUOTE at this level) in UNQUOTE-VALUE.

(defun unquote-level (skel &optional (level 0))
  (if (atom skel)
    (if (simple-vector-p skel)
      (map 'vector #'(lambda (subskel) (unquote-level subskel level)) skel)
      skel)
    ;; skel is a cons
    (cond ((and (eq (first skel) 'UNQUOTE) (consp (rest skel)))
           (if (zerop level)
             (list 'UNQUOTE-VALUE (second skel))
             (let ((following (unquote-level (second skel) (1- level))))
               ;; Simplify (UNQUOTE following):
               (if (and (consp following) (eq (car following) 'QUOTE)
                        (consp (second following))
                        (eq (car (second following)) 'UNQUOTE-VALUE))
                 ;;(UNQUOTE (QUOTE (UNQUOTE-VALUE ...))) -> (UNQUOTE-VALUE ...)
                 (second following)
                 (list 'UNQUOTE following)))))
          ((and (eq (first skel) 'BACKQUOTE) (consp (rest skel)))
           (list* 'BACKQUOTE
                  (unquote-level (second skel) (1+ level))
                  (if (consp (cddr skel))
                    (list (unquote-level (third skel) level))
                    nil)))
          (t ; CAR-CDR recursion
            (cons (unquote-level (car skel) level)
                  (unquote-level (cdr skel) level))))))

;; Determines whether a form can expand to several forms.
(defun splicing-p (skel)
  (and (consp skel)
       (let ((h (first skel))) (or (eq h 'splice) (eq h 'nsplice)))))

;; Replaces "skeleton" skel (with UNQUOTE-VALUEs etc.) in suitable code.
(defun backquote-1 (skel)
  (if (atom skel)
    (cond ((or (and (symbolp skel) (constantp skel)
                    (eq skel (symbol-value skel)))
               (numberp skel)
               (stringp skel)
               (bit-vector-p skel))
           ;; Constants which evaluate to themselves remain unchanged.
           skel)
          ((simple-vector-p skel)
           ;; Vectors
           ;; #(... item ...) -> (VECTOR ... item ...)
           ;; #(... ,@form ...) ->
           ;;   (MULTIPLE-VALUE-CALL #'VECTOR ... (VALUES-LIST form) ...)
           (if (some #'splicing-p skel)
             (list* 'MULTIPLE-VALUE-CALL
                    '(FUNCTION VECTOR)
                    (map 'list
                         #'(lambda (subskel)
                             (if (splicing-p subskel)
                               (if (and (consp (second subskel))
                                        (eq (first (second subskel))
                                            'UNQUOTE-VALUE))
                                 (list 'VALUES-LIST (backquote-1 (second subskel)))
                                 ;; SPLICE and/or NSPLICE to return later
                                 (backquote-cons (backquote-1 (first subskel))
                                                 (backquote-1 (rest subskel))))
                               (list 'VALUES (backquote-1 subskel))))
                         skel))
             (let ((einzelne (map 'list #'backquote-1 skel)))
               (if (every #'constantp einzelne)
                 ;; all components are contant -> concatenate immediately
                 (list 'QUOTE (map 'vector #'eval einzelne))
                 (cons 'VECTOR einzelne)))))
          (t                    ; convert other atoms A into 'A
           (list 'QUOTE skel)))
    (cond ((eq (first skel) 'unquote-value)
           ;; ,form at the correct level becomes form
           (second skel))
          ((and (eq (first skel) 'splice) (consp (rest skel)))
           ;; ,@form is forbidden
           (error-of-type 'error
             (TEXT "The syntax ,@form is valid only in lists")))
          ((and (eq (first skel) 'nsplice) (consp (rest skel)))
           ;; ,.form is forbidden
           (error-of-type 'error
             (TEXT "The syntax ,.form is valid only in lists")))
          ((and (eq (first skel) 'backquote) (consp (rest skel)))
           ;; nested backquotes
           (list* 'LIST
                  ''BACKQUOTE
                  (let ((*nsplice-fun* 'APPEND)) (backquote-1 (second skel)))
                  (if (consp (cddr skel))
                    (list (backquote-1 (third skel)))
                    nil)))
          ((and (consp (first skel))
                (eq (first (first skel)) 'splice))
           ;; (  ... ,@EXPR ...  ) handling
           (if (and (consp (second (first skel)))
                    (eq (first (second (first skel))) 'UNQUOTE-VALUE))
             (backquote-append (backquote-1 (second (first skel)))
                               (backquote-1 (rest skel)))
             ;; save SPLICE for later
             (backquote-cons
               (backquote-cons (backquote-1 (first (first skel)))
                               (backquote-1 (rest (first skel))))
               (backquote-1 (rest skel)))))
          ((and (consp (first skel))
                (eq (first (first skel)) 'nsplice))
           ;; handle (  ... ,.EXPR ...  )
           (if (and (consp (second (first skel)))
                    (eq (first (second (first skel))) 'UNQUOTE-VALUE))
             (let ((first (backquote-1 (second (first skel))))
                   (following (backquote-1 (rest skel))))
               ;; simplify (NCONC first following)
               (cond ((null following)
                      ;; (NCONC expr NIL) -> (NCONC expr) -> expr
                      (if (splicing-p first)
                        (list *nsplice-fun* first)
                        first))
                     ((and (consp following)
                           (eq (first following) *nsplice-fun*))
                      ;; (NCONC expr (NCONC . rest)) -> (NCONC expr . rest)
                      (list* *nsplice-fun* first (rest following)) )
                     (t (list *nsplice-fun* first following))))
             ;; save NSPLICE for later
             (backquote-cons
               (backquote-cons (backquote-1 (first (first skel)))
                               (backquote-1 (rest (first skel))))
               (backquote-1 (rest skel)))))
          (t ; combine CAR and CDR
             (backquote-cons (backquote-1 (first skel))
                             (backquote-1 (rest skel)))))))

;; Returns the form 'first' appended to 'following'.
(defun backquote-append (first following)
  ;; simplify (APPEND first following)
  (cond ((null following)
         ;; (APPEND expr NIL) -> (APPEND expr) -> expr
         (if (splicing-p first)
           (list 'APPEND first)
           first))
        ((and (consp following) (eq (first following) 'append))
         ;; (APPEND expr (APPEND . rest)) -> (APPEND expr . rest)
         (list* 'APPEND first (rest following)))
        (t (list 'APPEND first following))))

;; Returns the form which is the cons of the forms 'first' and 'following'.
(defun backquote-cons (first following)
  ;; simplify (CONS first following)
  (cond ((and (constantp first) (constantp following))
         ;; both parts are constant -> combine immediately
         (setq first (eval first))
         (setq following (eval following))
         (list 'QUOTE
           (cons (if (eq first 'UNQUOTE) 'UNQUOTE-DUMMY first) following)))
        ((null following)
         ;; (CONS expr NIL) -> (LIST expr)
         (list 'LIST first))
        ((atom following)
         (list 'CONS first following)) ; without simplifying
        ((eq (first following) 'LIST)
         ;; (CONS expr (LIST . rest)) -> (LIST expr . rest)
         (list* 'LIST first (rest following)))
        ((or (eq (first following) 'LIST*) (eq (first following) 'CONS))
         ;; (CONS expr (LIST* . rest)) -> (LIST* expr . rest)
         ;; (CONS expr1 (CONS expr2 expr3)) -> (LIST* expr1 expr2 expr3)
         (list* 'LIST* first (rest following)))
        (t (list 'CONS first following)))) ; without simplifying
