;;; Backquote Implementation for CLISP
;;; Copyright 2003 Kaz Kylheku <kaz@ashi.footprints.net>
;;; Dedicated to Pei-Yin Lin
;;;
;;; LIBERAL FREEWARE LICENSE: This Lisp source code document may be used
;;; by anyone for any purpose, and freely redistributed alone or in
;;; combination with other software, provided that the license is not
;;; violated.  The only possible way to violate the license is to
;;; redistribute this code in source form, with the copyright notice or
;;; license removed or altered.  This license applies to this document
;;; only, not any other software that it is combined with.

(in-package "SYSTEM")

;;; Handle the transformation of [x1] [x2] ... forms as described
;;; in the HyperSpec. In addition, nested backquotes are handled here.
(defun bq-transform (form)
  (if (consp form)
    (case (first form)
      ((UNQUOTE) (list 'list (second form)))
      ((SPLICE) (second form))
      ;; (BQ-NCONC FORM) serves as a parse tree annotation which
      ;; tells the optimizer that FORM may be destructively manipulated.
      ((NSPLICE) (list 'bq-nconc (second form)))
      ((BACKQUOTE) (list 'list (list 'BACKQUOTE (bq-expand (second form)))))
      (otherwise (list 'list (bq-expand form))))
    (list 'list (bq-expand form))))

;;; Handle the transformation of `(x1 x2 x3 ...) as described by HyperSpec
;;; to produce a list of forms that can be combined into an APPEND form.
(defun bq-expand-list (form)
  (cond
    ((null form) nil)
    ((consp (rest form))
       (case (second form)
         ;; well-defined dotted unquote `( ... . ,form)
         ((UNQUOTE)
            (list (bq-transform (first form)) (third form)))
         ;; undefined dotted splice: `( ... . ,@form)
         ((SPLICE NSPLICE)
            (bq-dotted-splice-error (second form)))
         (otherwise
            (cons (bq-transform (first form))
                  (bq-expand-list (rest form))))))
    ((null (rest form))
       (list (bq-transform (first form))))
    (t (list (bq-transform (first form)) (list 'quote (rest form))))))

;;; Handle base cases as describe by HyperSpec, plus nested backquote:
;;;
;;; `,form     -->  form
;;; `,@form    -->  error
;;; ``form     -->  `form-expanded
;;; list-form  -->  (append f1 f2 f3 ...) where (f1 f2 f3 ...)
;;;                 is the output of (bq-expand-list list-form).

(proclaim '(special *backquote-optimize*))
(setq *backquote-optimize* t)

(defun bq-expand-cons (form)
  (case (first form)
    ((UNQUOTE)
       (second form))
    ((SPLICE NSPLICE)
       (bq-non-list-splice-error (second form)))
    ((BACKQUOTE)
       (list 'BACKQUOTE (bq-expand (second form))))
    (otherwise
       (if *backquote-optimize*
         (bq-optimize (bq-expand-list form))
         (cons 'append (bq-expand-list form))))))

;;; Handle vector expansion, along the lines suggested by HyperSpec.
(defun bq-vec-expand (form)
  (let ((expanded (bq-expand (map 'list #'identity form))))
    (if (constantp expanded)
      (apply #'vector (eval expanded))
      (list 'apply '#'vector expanded))))

;;; Top level cases
;;;
;;; `()        -->  ()
;;; `cons      -->  result of (bq-expand-cons cons)
;;; `#( ... )  -->  result of (bq-vec-expand #( ... ))
;;;  other     -->  'other
(defun bq-expand (form)
  ;; we don't have TYPECASE at this stage
  (cond
    ((null form) nil)
    ((consp form) (bq-expand-cons form))
    ((or (stringp form) (bit-vector-p form)) (list 'quote form))
    ((vectorp form) (bq-vec-expand form))
    (t (list 'quote form))))

;;; *unquote-occured* flips to T when a sub-reader encounters the unquote
;;; syntax. This variable is the basis for a trick by which we diagnose uses of
;;; the unquote syntax inside forms which are not vectors or lists, such as:
;;; #`#s(foo-structure :bar #,z) without an cooperation from the readers of
;;; these forms. In some cases, however, such unquote syntax will cause the
;;; reader of the subform to raise an error before we catch it.
(proclaim '(special *unquote-occured*))

;;; *backquote-level* measures the level of backquote nesting that the reader
;;; is entangled in. It increases by one when reading the backquoted object,
;;; and decreases by one over reading an unquoted form.
(proclaim '(special *backquote-level*))

;;; *reading-array* is normally unbound. When the reader is running, it
;;; is dynamically bound to NIL, and when a #A array is being read,
;;; it is bound to T. This lets the comma-reader signal an error when
;;; unquotes occur in an array.
(proclaim '(special *reading-array*))

;;; *reading-struct* is analogous to *reading-array*, but for structs.
(proclaim '(special *reading-struct*))

;;; Handle the ` read syntax.
(defun backquote-reader (stream char)
  (declare (ignore char))
  (let* ((*unquote-occured* nil)
         (*reading-array* nil)
         (*reading-struct* nil)
         (*backquote-level* (1+ (or *backquote-level* 0)))
         (object (read stream t nil t)))
    (unless (or (and (vectorp object)
                     (not (stringp object))
                     (not (bit-vector-p object)))
                (listp object))
      (when *unquote-occured*
        (error-of-type 'reader-error
          (TEXT "~S: unquotes may occur only in (...) or #(...) forms")
          'read)))
    (when (consp object)
      (let ((head (first object)))
        (when (or (eq head 'SPLICE) (eq head 'NSPLICE))
          (bq-non-list-splice-error head :in-reader t)))
      (when (bq-member 'SPLICE object)
        (bq-dotted-splice-error 'SPLICE :in-reader t))
      (when (bq-member 'NSPLICE object)
        (bq-dotted-splice-error 'NSPLICE :in-reader t)))
    (list 'BACKQUOTE object)))

;;; Handle the read syntax ,
(defun comma-reader (stream char)
  (declare (ignore char))
  (when (null *backquote-level*)
    (error-of-type 'reader-error
      (TEXT "~S: comma is illegal outside of backquote")
      'read))
  (when (zerop *backquote-level*)
    (error-of-type 'reader-error
      (TEXT "~S: more commas out than backquotes in, is illegal")
      'read))
  (when *reading-struct*
    (error-of-type 'reader-error
      (TEXT "~S: unquotes may not occur in structures")
      'read))
  (when *reading-array*
    (error-of-type 'reader-error
      (TEXT "~S: unquotes may not occur in arrays")
      'read))
  (setq *unquote-occured* t)
  (let ((*backquote-level* (1- *backquote-level*))
        (next (peek-char nil stream)))
    (cond ((char= next #\@)
           (read-char stream)
           (list 'SPLICE (read stream t nil t)))
          ((char= next #\.)
           (read-char stream)
           (list 'NSPLICE (read stream t nil t)))
          (t (list 'UNQUOTE (read stream t nil t))))))

;;; Signal error for `,.form or `,@form. If :in-reader is t, then
;;; add the prefix "READ: ", to flag the error as coming from the reader.
(defun bq-non-list-splice-error (sym &key in-reader)
  (if (eq sym 'SPLICE)
    (error-of-type 'reader-error
      (TEXT "~athe syntax `,@form is undefined behavior")
      (if in-reader "READ: " ""))
    (error-of-type 'reader-error
      (TEXT "~athe syntax `,.form is undefined behavior")
      (if in-reader "READ: " ""))))

;;; Signal error for `(... . ,@form) or `(... . ,.form).
(defun bq-dotted-splice-error (sym &key in-reader)
  (if (eq sym 'SPLICE)
    (error-of-type 'reader-error
      (TEXT "~athe syntax `( ... . ,@form) is undefined behavior")
      (if in-reader "READ: " ""))
    (error-of-type 'reader-error
      (TEXT "~athe syntax `( ... . ,.form) is undefined behavior")
      (if in-reader "READ: " ""))))

;;; Like MEMBER but handles improper lists without error.
(defun bq-member (elem list &key (test #'eql))
  (do ((list list (rest list)))
      ((atom list) nil)
   (when (funcall test (first list) elem)
     (return list))))

;;;
;;; Optimizer
;;;

;;; BQ-OPTIMIZE takes as input a list of forms that are intended to be
;;; the argument list of an APPEND call.  It tries to optimize the forms
;;; to generate something more efficient than APPEND, but in the case
;;; that it does no optimizations, it just returns (cons 'append forms)
(defun bq-optimize (forms)
  (bq-reduce-nesting (bq-optimize-append forms)))

;;; Returns true if form evaluates to itself.
(defun eval-self-p (form)
  (or (null form)
      (keywordp form)
      (not (or (symbolp form)
               (consp form)))))
;;; quote if the form does not evaluate to iteself
(defun maybe-quote (form)
  (if (eval-self-p form) form (list 'quote form)))

(defun bq-optimize-append (forms)
  (cond
    ;; () -> ()
    ((null forms) nil)
    ;; ((bq-nconc x1) ... (bq-nconc xn)) -> (bq-nconc x1 .. xn)
    ((and (rest forms)
          (every #'(lambda (form)
                     (and (consp form) (eq (first form) 'bq-nconc)))
                 forms))
     (cons 'nconc (mapcar #'second forms)))
    ;; ((list x1) ... (list xn-1) xn) -> (list* x1 ... xn-1 xn)
    ((every #'(lambda (form)
                (and (consp form) (eq (first form) 'list)))
            (butlast forms))
     (bq-optimize-list* (append (mapcan #'rest (butlast forms))
                                (last forms))))
    ;; ((bq-nconc x) ...) -> (nconc x <recurse (...)>)
    ((and (consp (first forms))
          (eq (first (first forms)) 'bq-nconc))
     (list 'nconc
           (second (first forms))
           (bq-optimize-append (rest forms))))
    ;; ((list ...) ...)
    ((and (consp (first forms))
          (memq (first (first forms)) '(list list*)))
     (let ((form (bq-optimize-list (first forms))))
       (if (and (eq 'quote (first form))
                (= (length (second form)) 1))
         ;; ((list x) ...) -> ('(x) ...) -> (cons x <recurse (...)>)
         (list 'cons
               (maybe-quote (first (second form)))
               (bq-optimize-append (rest forms)))
         ;; ((list x1 x2 ...) ...) -> (append '(x1 x2) <recurse (...)>)
         ;;                     or -> (append (list x1 x2) <recurse (...)>)
         (list 'append
               form
               (bq-optimize-append (rest forms))))))
    ;; (x1 x2 ...) -> (append x1 <recurse (x2 ...)>)
    (t (list 'append
             (first forms)
             (bq-optimize-append (rest forms))))))

;;; BQ-OPTIMIZE-LIST* takes as input a list of forms that are intended
;;; to be the argument list of an LIST* call.  It tries to optimize the
;;; forms to generate something more efficient than the implied LIST*,
;;; but in the case that it does no optimizations, it just returns (cons
;;; 'list* forms).  This has to be careful to watch for (SPLICE ...) and
;;; (NSPLICE ...) forms in the last position of the LIST* argument list.
(defun bq-optimize-list* (forms)
  (if (= (length forms) 1)
    ;; ((list x)) -> (list x) [ -> '(x) ]
    ;; (x) -> x
    ;; ((SPLICE X)) -> (append X)
    (if (and (consp (first forms))
             (memq (first (first forms)) '(SPLICE NSPLICE)))
      (list 'append (first forms))
      (bq-optimize-list (first forms)))
    (let* ((forms (bq-drop-superfluous-quotes forms))
           (last-opt (bq-optimize-list (first (last forms)))))
      (cond
        ;; (... ,@form) -> (list* ... (append form))
        ((and (consp last-opt)
              (memq (first last-opt) '(SPLICE NSPLICE))
          (append '(list*) (butlast forms)
                  (list (list 'append last-opt)))))

        ;; (... '(x1 x2 ...)) -> (list ... 'x1 'x2 ...)
        ((and (consp last-opt)
              (eq (first last-opt) 'quote)
              (listp (second last-opt)))
         (bq-optimize-list
          (append '(list) (butlast forms)
                  (mapcar #'maybe-quote
                          (second last-opt)))))
        ;; (... (list x1 x2 ...)) -> (list ... x1 x2 ...)
        ;;                      [ -> '(... x1 x2 ...) ]
        ((and (consp last-opt)
              (eq (first last-opt) 'list))
           (bq-optimize-list
             (append '(list) (butlast forms) (rest last-opt))))

        ;; (... x . nil) -> (list ... x) [ -> '( ... x) ]
        ((null last-opt)
           (bq-optimize-list
             (append '(list) (butlast forms))))

        ;; (... x) -> (list* ... x) [ -> '(... . x) ]
        (t (bq-optimize-list
             (append '(list*) (butlast forms) (list last-opt))))))))

;;; BQ-DROP-SUPERFLUOUS-QUOTES reduces every element of the input list
;;; that is (QUOTE FORM) to FORM, provided that FORM
;;; evaluates to itself already.
(defun bq-drop-superfluous-quotes (forms)
  (mapcar #'(lambda (form)
              (if (and (consp form)
                       (eq 'quote (first form))
                       (eval-self-p (second form)))
                (second form)
                form))
          forms))

;;; BQ-OPTIMIZE-LIST tries to turn a (list ...) or
;;; (list* ...) form into a '(...) form.
(defun bq-optimize-list (form)
  (if (and (consp form)
           (memq (first form) '(list list*))
           (every #'constantp (rest form)))
    (list 'quote (apply (first form)
                        (mapcar #'eval (rest form))))
    form))

;;; Reduce nested APPEND, NCONC and CONS expressions.
;;; This cleans up after the optimizer.
;;;
;;; (NCONC X (NCONC ...)) -> (NCONC X ...)
;;; (APPEND X (APPEND ...)) -> (APPEND X ...)
;;; (CONS X (CONS Y Z)) -> (LIST* X Y Z)
;;; (CONS X (LIST* Y ...)) -> (LIST* X Y ...)
;;; (CONS X (LIST Y ...)) -> (LIST X Y ...)
(defun bq-reduce-nesting (form)
  (cond
    ;; () -> ()
    ((null form) nil)
    ;; a -> a
    ((atom form) form)
    ;; (cons x y)
    ((and (eq (first form) 'cons)
          (cddr form)
          (not (cdddr form)))
       (let ((third (bq-reduce-nesting (third form)))
             (second (bq-reduce-nesting (second form))))
         (cond
           ((atom third) (list 'cons second third))
           ;; (cons x (list ...)) -> (list x ...)
           ((eq (first third) 'list)
              (list* 'list second (rest third)))
           ;; (cons x (list* y ...)) -> (list* x y ...)
           ;; (cons x (cons y z)) -> (list* x y z)
           ((or (eq (first third) 'list)
                (and (eq (first third) 'list*)
                     (cdr third))
                (and (eq (first third) 'cons)
                     (cddr third)
                     (not (cdddr third))))
              (list* 'list* second (rest third)))
          (t (list 'cons second third)))))
    ;; (append/nconc ...)
    ((memq (first form) '(append nconc))
       (cond
         ;; Four or more argument APPEND or NCONC: don't touch.
         ((cdddr form) form)
         ;; Three argument APPEND or NCONC.
         ((cddr form)
            (let ((third (bq-reduce-nesting (third form)))
                  (second (bq-reduce-nesting (second form))))
              (if (and (consp third)
                       (eq (first third) (first form)))
                (list* (first form) second (rest third))
                (list (first form) second third))))
         ;; Two-argument APPEND or NCONC
         ((cdr form)
            (list (first form) (bq-reduce-nesting (second form))))
         ;; Zero-argument APPEND or NCONC
         (t nil)))
    (t form)))

;;; Interfaces used by other modules within CLISP, and possibly
;;; by CLISP applications.
;;;
;;; Note: to dynamically add a variable number of unquotes to a nested
;;; backquote, consider using the ,,@ syntax:
;;;
;;;   (let ((unquote-these-forms '((list 1 2) (list 3 4)))
;;;     `(outer `(inner ,,@unquote-these-forms))
;;;
;;; rather than ADD-BACKQUOTE and ADD-UNQUOTE:
;;;
;;;   (let ((unquote-these-forms '((list 1 2) (list 3 4))))
;;;     `(outer ,(system::add-backquote
;;;                `(inner ,@(mapcar #'system::add-unquote
;;;                                  unquote-these-forms)))))
;;;
;;; The effect is like `(outer ,(inner ,(list 1 2) ,(list 3 4)))
;;;
;;; If you want the effect `(outer ,(inner ,@(list 1 2) ,@(list 3 4)))
;;; then substitute `(outer `(inner ,@,@unquote-these-forms))
;;;
;;; If you think you need ADD-BACKQUOTE and ADD-UNQUOTE, or even
;;; the nonexistent ADD-SPLICE, it's likely that your requirements
;;; may be satisfied by the ,,@ and ,@,@ syntax. The distributive
;;; rule is simple: the right ,@ splices the forms into the list, and the
;;; left , or ,@ distributes itself over those forms before the next
;;; expansion round.
;;;
;;; There are exceptions, like the more complicated situation in CLISP's
;;; giant defstruct macro, which prepares a list of nested lists each
;;; containing a buried unquote forms, and then later encloses it in a
;;; backquote.

(defun add-backquote (skel)
  (list 'BACKQUOTE skel))

(defun add-unquote (skel)
  (list 'UNQUOTE skel))

(defun backquote-cons (car cdr)
  (if *backquote-optimize*
    (bq-optimize-list* (list car cdr))
    (list 'cons car cdr)))

(defun backquote-append (left right)
  (if *backquote-optimize*
    (bq-optimize (list left right))
    (list 'append left right)))
