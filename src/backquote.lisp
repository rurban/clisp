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

;;; ============================== Reader Macro ==============================

;;; At this level, we do only parsing, not simplification, so that things
;;; like `,A print as they were input. Parsing returns
;;; the following                  for
;;;   (BACKQUOTE x)                `x
;;;   (UNQUOTE x)                  ,x
;;;   (SPLICE x)                   ,@x
;;;   (NSPLICE x)                  ,.x

;;; *unquote-occurred* flips to T when a sub-reader encounters the unquote
;;; syntax. This variable is the basis for a trick by which we diagnose uses of
;;; the unquote syntax inside forms which are not vectors or lists, such as:
;;; #`#s(foo-structure :bar #,z) without an cooperation from the readers of
;;; these forms. In some cases, however, such unquote syntax will cause the
;;; reader of the subform to raise an error before we catch it.
(proclaim '(special *unquote-occurred*))

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
  (let* ((*unquote-occurred* nil)
         (*reading-array* nil)
         (*reading-struct* nil)
         (*backquote-level* (1+ (or *backquote-level* 0)))
         (object (read stream t nil t)))
    (unless (or (and (vectorp object) (eq (array-element-type object) 'T))
                (listp object))
      (when *unquote-occurred*
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
  (setq *unquote-occurred* t)
  (let ((*backquote-level* (1- *backquote-level*))
        (next (peek-char nil stream)))
    (cond ((char= next #\@)
           (read-char stream)
           (list 'SPLICE (read stream t nil t)))
          ((char= next #\.)
           (read-char stream)
           (list 'NSPLICE (read stream t nil t)))
          (t (list 'UNQUOTE (read stream t nil t))))))

;;; Like MEMBER but handles improper lists without error.
(defun bq-member (elem list &key (test #'eql))
  (do ((list list (rest list)))
      ((atom list) nil)
   (when (funcall test (first list) elem)
     (return list))))

;;; ----------------------------- Error Messages -----------------------------

;;; Used by the Reader Macro and the Macroexpander.

;;; Signal error for `,.form or `,@form.
;;; It's undefined behaviour; we signal an error for it.
;;; If :in-reader is t, then add the prefix "READ: ", to flag the error as
;;; coming from the reader.
(defun bq-non-list-splice-error (sym &key in-reader)
  (error-of-type 'reader-error
    (if in-reader (TEXT "READ: ~@?") "~@?")
    (if (eq sym 'SPLICE)
      (TEXT "the syntax `,@form is invalid")
      (TEXT "the syntax `,.form is invalid"))))

;;; Signal error for `(... . ,@form) or `(... . ,.form).
;;; It's undefined behaviour; we signal an error for it.
(defun bq-dotted-splice-error (sym &key in-reader)
  (error-of-type 'reader-error
    (if in-reader (TEXT "READ: ~@?") "~@?")
    (if (eq sym 'SPLICE)
      (TEXT "the syntax `( ... . ,@form) is invalid")
      (TEXT "the syntax `( ... . ,.form) is invalid"))))

;;; ============================== Macroexpander ==============================

;;; ------------------------------ Entry-points ------------------------------

;; The BACKQUOTE macro just calls the backquote expander on its argument.
(sys::%putd 'BACKQUOTE
  (sys::make-macro
    (function BACKQUOTE
      (lambda (form env)
        (declare (ignore env))
        (bq-expand (second form))))))

;; The BQ-NCONC form is a marker used in the backquote-generated code.
;; It tells the optimizer that the enclosed forms may be combined with
;; NCONC. By defining BQ-NCONC as a macro, we take care of any `surviving'
;; occurrences that are not removed and processed by the optimizer.
(sys::%putd 'BQ-NCONC
  (sys::make-macro
   (function BQ-NCONC
     (lambda (form env)
       (declare (ignore env))
       (if (cddr form)
         (cons 'NCONC (rest form))
         (second form))))))

;;; ----------------------- Recursive Expansion Engine -----------------------

;;; Naive expansion produces inefficient construction forms, e.g.
;;;   `(,(foo) ,(bar)) => (APPEND (LIST (foo)) (LIST (bar)))
;;;   instead of          (LIST (foo) (bar))
;;; Backquote expansion optimizers are enabled by default, but can be turned
;;; off for debugging.
(proclaim '(special *backquote-optimize*))
(setq *backquote-optimize* t)

;;; Top level cases
;;;
;;; `()        -->  ()
;;; `cons      -->  result of (bq-expand-cons cons)
;;; `#( ... )  -->  result of (bq-expand-vector #( ... ))
;;;  other     -->  'other
(defun bq-expand (form)
  ;; we don't have TYPECASE at this stage
  (cond
    ((null form) nil)
    ((consp form)
      ;; Handle base cases as described by HyperSpec, plus nested backquote:
      ;;
      ;; `,form     -->  form
      ;; `,@form    -->  error
      ;; ``form     -->  `form-expanded
      ;; list-form  -->  (append f1 f2 f3 ...) where (f1 f2 f3 ...)
      ;;                 is the output of (bq-expand-list list-form).
      (case (first form)
        ((UNQUOTE)
          (second form))
        ((SPLICE NSPLICE)
          (bq-non-list-splice-error (second form)))
        ((BACKQUOTE)
          (list 'BACKQUOTE (bq-expand (second form))))
        (otherwise
          (let ((expansion (bq-expand-list form)))
            (if *backquote-optimize*
              (bq-optimize-for-list expansion)
              (cons 'append expansion))))))
    ((and (vectorp form) (eq (array-element-type form) 'T))
      ;; Handle vector expansion, along the lines suggested by HyperSpec.
      (let ((expansion (bq-expand-list (map 'list #'identity form))))
        (if *backquote-optimize*
          (bq-optimize-for-vector expansion)
          (list 'APPLY '#'VECTOR (cons 'APPEND expansion)))))
    (t (list 'quote form))))

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
;;; There is one deviation from the HyperSpec: namely, in the case
;;; `(x1 x2 ... xn . atom) the atom is translated to (backquote atom)
;;; rather than (quote atom). This allows for the atom to be a vector
;;; with embedded unquotes, an apparently common extension.
(defun bq-expand-list (form)
  (if (null form)
    nil
    (let ((expanded-car (bq-transform (first form)))
          (tail (rest form)))
      (cond ((null tail) (list expanded-car))
            ((consp tail)
             (case (first tail)
               ;; well-defined dotted unquote `( ... . ,form)
               ((UNQUOTE)
                (list expanded-car (second tail)))
               ;; undefined dotted splice: `( ... . ,@form)
               ((SPLICE NSPLICE)
                (bq-dotted-splice-error (first tail)))
               (otherwise
                 (cons expanded-car (bq-expand-list tail)))))
            (t (list expanded-car (list 'BACKQUOTE tail)))))))

;;; --------------------------- Expansion Optimizer ---------------------------

;;; BQ-OPTIMIZE-FOR-LIST takes as input a list of forms that are intended to
;;; be the argument list of an APPEND call.  It tries to optimize the forms
;;; to generate something more efficient than APPEND, but in the case
;;; that it does no optimizations, it just returns (cons 'append forms)
(defun bq-optimize-for-list (forms)
  (bq-reduce-nesting (bq-optimize-append forms)))

;;; BQ-OPTIMIZE-FOR-VECTOR generates a better translation for a backquoted
;;; vector. The vector has been already converted to a list, which
;;; was subject to unoptimized backquote expansion. That resulting
;;; list of append arguments is what is passed to this function.
;;; The expansion is optimized and then converted to a vector
;;; form according to these rules:
;;;
;;; '(...)  -> #(...)
;;; (list ...) -> (vector ...)
;;; (append ...) -> (multiple-value-call #'vector ...)
;;;
;;; The (append ...) case is based on the original unoptimized
;;; append args. The arguments are each treated as follows:
;;;
;;; (list ...) -> (values ...)
;;; (splice ...) -> (values-list (append ...))
;;; (nsplice ...) -> (values-list (nconc ...))
;;; other -> (values-list other)
(defun bq-optimize-for-vector (unoptimized)
  (let ((optimized (bq-optimize-for-list unoptimized)))
    (cond
      ((constantp optimized)
         (apply #'vector (eval optimized)))
      ((not (consp optimized))
         (list 'apply '#'vector optimized))
      ((eq (first optimized) 'list)
         (cons 'vector (rest optimized)))
      (t (list* 'multiple-value-call '#'vector
                (mapcar #'(lambda (apply-arg)
                            (cond
                              ((atom apply-arg)
                                 (list 'values-list apply-arg))
                              ((memq (first apply-arg) '(SPLICE NSPLICE))
                                 (list 'values-list (list 'append apply-arg)))
                              ((eq (first apply-arg) 'list)
                                 (list* 'values (rest apply-arg)))
                              (t (list 'values-list apply-arg))))
                           unoptimized))))))

;;; - - - - - - - - - - - - Expansion Optimizer Step 1 - - - - - - - - - - - -

(defun bq-optimize-append (forms &aux butlast)
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
            (setq butlast (butlast forms)))
     (bq-optimize-list* (nconc (mapcap #'rest butlast)
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
;;; 'list* forms).  This has to be careful to watch for splicing unquote
;;; forms in the last position, and also for a quoted unquote in the
;;; last position.
(defun bq-optimize-list* (forms)
  (if (= (length forms) 1)
    ;; ((list x)) -> (list x) [ -> '(x) ]
    ;; (x) -> x
    ;; ((SPLICE X)) -> (append X)
    (if (and (consp (first forms))
             (memq (first (first forms)) '(SPLICE NSPLICE)))
      (list 'append (first forms))
      (bq-optimize-list (first forms)))
    (let* ((forms (mapcar #'maybe-unquote forms))
           (last-opt (bq-optimize-list (first (last forms)))))
      (cond
        ;; (... ,@form) -> (list* ... (append form))
        ((and (consp last-opt)
              (memq (first last-opt) '(SPLICE NSPLICE))
          (append '(list*) (butlast forms)
                  (list (list 'append last-opt)))))

        ;; (... '(x1 x2 ...)) -> (list ... 'x1 'x2 ...)
        ;; But CAREFUL! Must not rip apart unquotes and splices:
        ;; (... ',form) -> (list ... 'system::unquote 'form)
        ((and (consp last-opt)
              (eq (first last-opt) 'quote)
              (listp (second last-opt))
              (not (quoted-bq-operator-p last-opt)))
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

;;; BQ-OPTIMIZE-LIST tries to turn a (list ...) or
;;; (list* ...) form into a '(...) form.
(defun bq-optimize-list (form)
  (if (and (consp form)
           (memq (first form) '(list list*))
           (every #'bq-constant-p (rest form)))
    (list 'quote (apply (first form)
                        (mapcar #'bq-eval (rest form))))
    form))

;;; BQ-EVAL implements special evaluation rules for reducing constant
;;; expressions at backquote expansion time. Most constant expressions
;;; are reduced by EVAL. But certain ones must be treated specially.
;;; For instance, an expression like (QUOTE (UNQUOTE X)) must not
;;; evaluate to (UNQUOTE X). Rather, the two must cancel to just produce
;;; the value of X. We apply BQ-OPTIMIZE-LIST to create opportunities
;;; for (LIST ...) or (LIST* ...) forms to be reduced to constants.
(defun bq-eval (form)
  (if (quoted-bq-operator-p form)
    (case (first (second form))
      ((UNQUOTE) (bq-eval (bq-optimize-list (second (second form)))))
      (otherwise form))
    (eval form)))

;;; BQ-CONSTANT-P determines whether the expression may be reduced
;;; at expansion time by BQ-EVAL, similarly to how CONSTANTP determines
;;; whether an expression may be reduced by EVAL at compile time.
(defun bq-constant-p (form)
  (if (quoted-bq-operator-p form)
    (case (first (second form))
      ((UNQUOTE) (bq-constant-p (bq-optimize-list (second (second form)))))
      (otherwise nil))
    (constantp form)))

;;; - - - - - - - - - - - Nonrecursive auxiliary functions - - - - - - - - - - -

;;; Returns true if form evaluates to itself.
(defun eval-self-p (form)
  (or (null form) (eq form t)
      (keywordp form)
      (not (or (symbolp form)
               (consp form)))))

;;; quote if the form does not evaluate to itself
(defun maybe-quote (form)
  (if (eval-self-p form) form (list 'quote form)))

;;; unquote if the quoted form evaluates to iteself
(defun maybe-unquote (form)
  (if (and (consp form)
           (eq 'quote (first form))
           (eval-self-p (second form)))
      (second form)
      form))

;;; Test whether the argument is of the form (QUOTE (<oper> ...))
;;; where <oper> is one of the backquote operators.
(defun quoted-bq-operator-p (form)
  (and (consp form)
       (eq (first form) 'quote)
       (consp (second form))
       (memq (first (second form)) '(UNQUOTE SPLICE NSPLICE BACKQUOTE))))

;;; - - - - - - - - - - - - Expansion Optimizer Step 2 - - - - - - - - - - - -

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

;;; =========================== Other Entry-points ===========================

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
    (bq-optimize-for-list (list left right))
    (list 'append left right)))
