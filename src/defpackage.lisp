;;; ANSI-compatible definitions
;;; Bruno Haible 21.7.1994
;;; Sam Steingold 1999-2004

;; ============================================================================

(in-package "COMMON-LISP")
(export '(defpackage))
(in-package "SYSTEM")

;; ----------------------------------------------------------------------------

;; X3J13 vote <52>

;; Package-Definition und -Installation, CLtL2 S. 270
(defmacro defpackage (packname &rest options)
  (setq packname (string packname))
  ;; process options:
  (let ((size nil) ; :SIZE has been supplied
        (documentation nil) ; :DOCUMENTATION string
        (nickname-list '()) ; list of nicknames
        (shadow-list '()) ; list of symbol names to shadow
        (shadowing-list '()) ; list of pairs (symbol-name . package-name) for shadowing-import
        (use-list '()) ; list of package-names for use-package
        (use-default '("COMMON-LISP")) ; default for use-list
        (case-sensitive nil) ; flag for :CASE-SENSITIVE
        (import-list '()) ; list of (symbol-name . package-name) for import
        (intern-list '()) ; list of symbol-names for intern
        (symname-list '()) ; list of all symbol names specified so far
        (export-list '())) ; liste of symbol-names for export
    (flet ((record-symname (name)
             (if (member name symname-list :test #'string=)
               (error-of-type 'source-program-error
                 :form name
                 (TEXT "~S ~A: the symbol ~A must not be specified more than once")
                 'defpackage packname name)
               (push name symname-list))))
      (dolist (option options)
        (if (listp option)
          (if (keywordp (car option))
            (case (first option)
              (:SIZE
               (if size
                 (error-of-type 'source-program-error
                   :form size
                   (TEXT "~S ~A: the ~S option must not be given more than once")
                   'defpackage packname ':SIZE)
                 (setq size t))) ; ignored
              (:DOCUMENTATION ; ANSI-CL
               (if documentation
                 (error-of-type 'source-program-error
                   :form documentation
                   (TEXT "~S ~A: the ~S option must not be given more than once")
                   'defpackage packname ':DOCUMENTATION)
                 (setq documentation (second option))))
              (:NICKNAMES
               (dolist (name (rest option))
                 (push (string name) nickname-list)))
              (:SHADOW
               (dolist (name (rest option))
                 (setq name (string name))
                 (unless (member name shadow-list :test #'string=)
                   (push name shadow-list)
                   (record-symname name))))
              (:SHADOWING-IMPORT-FROM
               (let ((pack (string (second option))))
                 (dolist (name (cddr option))
                   (setq name (string name))
                   (let ((name+pack (cons name pack)))
                     (unless (member name+pack shadowing-list :test #'equal) ; #'string= on car and cdr
                       (push name+pack shadowing-list)
                       (record-symname name))))))
              (:USE
               (dolist (name (rest option))
                 (push (string name) use-list))
               (setq use-default nil))
              (:IMPORT-FROM
               (let ((pack (string (second option))))
                 (dolist (name (cddr option))
                   (setq name (string name))
                   (let ((name+pack (cons name pack)))
                     (unless (member name+pack import-list :test #'equal) ; #'string= on car and cdr
                       (push name+pack import-list)
                       (record-symname name))))))
              (:INTERN
               (dolist (name (rest option))
                 (setq name (string name))
                 (unless (member name intern-list :test #'string=)
                   (push name intern-list)
                   (record-symname name))))
              (:EXPORT
               (dolist (name (rest option))
                 (setq name (string name))
                 (unless (member name export-list :test #'string=)
                   (push name export-list))))
              (:CASE-SENSITIVE ; CLISP extension
               (when (not (null (second option)))
                 (setq case-sensitive t)))
              (T (error-of-type 'source-program-error
                   :form (first option)
                   (TEXT "~S ~A: unknown option ~S")
                   'defpackage packname (first option))))
            (error-of-type 'source-program-error
              :form option
              (TEXT "~S ~A: invalid syntax in ~S option: ~S")
              'defpackage packname 'defpackage option))
          (error-of-type 'source-program-error
            :form option
            (TEXT "~S ~A: not a ~S option: ~S")
            'defpackage packname 'defpackage option)))
      ;; check for overlaps between intern-list and export-list:
      (setq symname-list intern-list)
      (mapc #'record-symname export-list))
    ;; reverse lists and apply default values:
    (setq nickname-list (nreverse nickname-list))
    (setq shadow-list (nreverse shadow-list))
    (setq shadowing-list (nreverse shadowing-list))
    (setq use-list (or use-default (nreverse use-list)))
    (setq import-list (nreverse import-list))
    (setq intern-list (nreverse intern-list))
    (setq export-list (nreverse export-list))
    ;; produce the expansion:
    `(EVAL-WHEN (LOAD COMPILE EVAL)
       (SYSTEM::%IN-PACKAGE ,packname :NICKNAMES ',nickname-list :USE '()
                            ,@(when case-sensitive `(:CASE-SENSITIVE T)))
       ;; step 1
       ,@(if shadow-list
           `((SHADOW ',(mapcar #'make-symbol shadow-list) ,packname)))
       ,@(mapcar
          #'(lambda (pair)
              `(SHADOWING-IMPORT-CERROR ,(car pair) ,(cdr pair) ,packname))
          shadowing-list)
       ;; step 2
       ,@(if use-list `((USE-PACKAGE ',use-list ,packname)))
       ;; step 3
       ,@(mapcar #'(lambda (pair)
                     `(IMPORT-CERROR ,(car pair) ,(cdr pair) ,packname))
                 import-list)
       ,@(mapcar #'(lambda (symname) `(INTERN ,symname ,packname))
                 intern-list)
       ;; step 4
       ,@(if export-list `((INTERN-EXPORT ',export-list ,packname)))
       ;; step 5
       ,@(if documentation
           `((SETF (SYS::PACKAGE-DOCUMENTATION (FIND-PACKAGE ,packname))
                   ,documentation)))
       (FIND-PACKAGE ,packname))))

; Hilfsfunktionen:
(defun find-symbol-cerror (string packname calling-packname)
  (multiple-value-bind (sym found) (find-symbol string packname)
    (unless found
      (cerror ; 'package-error ??
              (TEXT "This symbol will be created.")
              (TEXT "~S ~A: There is no symbol ~A::~A .")
              'defpackage calling-packname packname string
      )
      (setq sym (intern string packname))
    )
    sym
) )
(defun shadowing-import-cerror (string packname calling-packname)
  (let ((sym (find-symbol-cerror string packname calling-packname)))
    (shadowing-import (or sym '(NIL)) calling-packname)
) )
(defun import-cerror (string packname calling-packname)
  (let ((sym (find-symbol-cerror string packname calling-packname)))
    (import (or sym '(NIL)) calling-packname)
) )
(defun intern-export (string-list packname)
  (export (mapcar #'(lambda (string) (intern string packname)) string-list)
          packname
) )
