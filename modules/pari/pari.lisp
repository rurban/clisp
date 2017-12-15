;; CLISP interface to PARI <http://pari.math.u-bordeaux.fr/>
;; Copyright (C) 1995 Michael Stoll
;; Copyright (C) 2004-2010, 2017 Sam Steingold
;; This is free software, distributed under the GNU GPL v2+

;; See file COMPAT in the top-level PARI distribution for object renamings.
;; http://pari.math.u-bordeaux.fr/cgi-bin/gitweb.cgi?p=pari.git;a=blob_plain;f=COMPAT;hb=HEAD
;; at gp prompt: whatnow(old-function-name), ?function-name
;; https://pari.math.u-bordeaux.fr/dochtml/html-stable/

(defpackage "PARI"
  (:modern t)
  (:use #:cl #:ffi))
(in-package "PARI")
(pushnew "PARI" custom:*system-package-list* :test #'string=)

(setf (documentation (find-package "PARI") 'sys::impnotes) "pari")

(default-foreign-language :stdc)

(pushnew :pari *features*)
(provide "pari")

;; capture error output:
;; + the PARI error message goes though the CLCS instead of directly
;;   to *error-output*.
;; - PARI syntax errors are multiline and exquisitely aligned line-by-line
;;   so going through CLCS will make the printed messages less useful.
;; (defvar *pari-error-output* (make-string-output-stream))
;; (defun pari-err-putc (c) (write-char c *pari-error-output*))
;; (defun pari-err-puts (s) (write-string s *pari-error-output*))
;; (defun pari-err-flush ()
;;   (write-string (get-output-stream-string *pari-error-output*) *error-output*))
;; (def-call-in pari-err-putc (:name "clisp_err_putc")
;;   (:return-type nil) (:arguments (c character)))
;; (def-call-in pari-err-puts (:name "clisp_err_puts")
;;   (:return-type nil) (:arguments (s c-string :in :malloc-free)))
;; (def-call-in pari-err-flush (:name "clisp_err_flush")
;;   (:return-type nil) (:arguments))

;;; Declare all the pari types, variables, functions, ...
(c-lines "#undef T~%#include <pari/pari.h>~%#include <pari/paripriv.h>~%")

(defun pari-byte (bits shift)
  (byte (integer-length (ash bits (- shift))) shift))
(def-c-const SIGNBITS (:type ulong))  ; 0xff000000L or 0xC000000000000000UL
(def-c-const SIGNSHIFT (:type ulong)) ; 24 or 62
(defconstant pari-sign-byte           ; (byte 8 24) or (byte 2 62)
  (pari-byte SIGNBITS SIGNSHIFT))
(def-c-const TYPBITS (:type ulong))  ; 0xff000000L or 0xFE00000000000000UL
(def-c-const TYPSHIFT (:type ulong)) ; 24 or 57
(defconstant pari-type-byte          ; (byte 8 24) or (byte 7 57)
  (pari-byte TYPBITS TYPSHIFT))
(def-c-const LGBITS (:type ulong)) ; 0xffffL or 0xFFFFFFFFFFFFFFUL
(defconstant pari-length-byte      ; (byte 16 0) or (byte 56 0)
  (pari-byte LGBITS 0))
(def-c-const EXPOBITS (:type ulong)) ; 0xffffffL or 0x3FFFFFFFFFFFFFFFUL
(defconstant pari-exponent-byte ; (byte 24 0) or (byte 62 0)
  (pari-byte EXPOBITS 0))
(def-c-const HIGHEXPOBIT (:type ulong)) ; 0x800000L or 0x2000000000000000UL
(defconstant pari-exponent-offset HIGHEXPOBIT)
(def-c-const VALPBITS (:type ulong)) ; 0xffffL or 0x3FFFFFFFFFFFUL
(defconstant pari-valuation-byte     ; (byte 16 0) or (byte 46 0)
  (pari-byte VALPBITS 0))
(def-c-const HIGHVALPBIT (:type ulong)) ; 0x8000L or 0x200000000000UL
(defconstant pari-valuation-offset HIGHVALPBIT)
(def-c-const PRECPBITS (:type ulong))  ; 0xffff0000L or 0xFFFFC00000000000UL
(def-c-const PRECPSHIFT (:type ulong)) ; 16 or 46
(defconstant pari-precision-byte ; (byte 16 16) or (byte 18 46)
  (pari-byte PRECPBITS PRECPSHIFT))
(def-c-const VARNBITS (:type ulong))  ; 0xff0000L or 0x3FFFC00000000000UL
(def-c-const VARNSHIFT (:type ulong)) ; 16 or 46
(defconstant pari-varno-byte          ; (byte 8 16) or (byte 16 46)
  (pari-byte VARNBITS VARNSHIFT))
(def-c-const CLONEBIT (:type ulong)) ; ??? or 0x100000000000000UL

;; <paritype.h>

(exporting:def-c-enum pari-typecode
  (INT 1)
  (REAL 2)
  (INTMOD 3)
  (FRAC 4)
  (FFELT 5)
  (COMPLEX 6)
  (PADIC 7)
  (QUAD 8)
  (POLMOD 9)
  (POL 10)
  (SER 11)
  (RFRAC 13)
  (QFR 15)
  (QFI 16)
  (VEC 17)
  (COL 18)
  (MAT 19)
  (LIST 20)
  (STR 21)
  (VECSMALL 22)
  (CLOSURE 23)
  (ERROR 24)
  (INFINITY 25))

;; <parigen.h>

;;; The pari object type:
;;;   typedef long    *GEN;
;;; To prevent CLISP from thinking we want to do something with the long
;;; such a pointer points to, we replace long* by void*:
(def-c-type pari-gen c-pointer)

;; <paristio.h>

;; typedef struct entree {
;;   const char *name;
;;   ulong valence;
;;   void *value;
;;   long menu;
;;   const char *code;
;;   const char *help;
;;   void *pvalue;
;;   long arity;
;;   ulong hash;
;;   struct entree *next;
;; } entree;
(def-c-struct entree
  (name c-string)
  (valence ulong)
  (value c-pointer)
  (menu long)
  (code c-string)
  (help c-string)
  (pvalue c-pointer)
  (arity long)
  (hash ulong)
  (next (c-pointer entree)))    ; (c-ptr-null entree)

(exporting:defun next-entree (e) (foreign-value (entree-next e)))

;; typedef unsigned char *byteptr;
(def-c-type byteptr (c-ptr uchar))

;; typedef ulong pari_sp;
(def-c-type pari_sp ulong)

;; struct pari_mainstack {
;;   pari_sp top, bot, vbot;
;;   size_t size, rsize, vsize, memused;
;; };
(def-c-struct mainstack
  (top pari_sp)
  (bot pari_sp)
  (vbot pari_sp)
  (size size_t)
  (rsize size_t)
  (vsize size_t)
  (memused size_t))
(def-c-var pari-mainstack (:name "pari_mainstack") (:type (c-pointer mainstack)))

(defun bits2digits (bits) (values (floor (* #.(log 2 10) bits))))
(defun digits2bits (digits) (values (ceiling (* #.(log 10 2) digits))))
(defvar pari-real-precision-words
  (+ 2 (ceiling (ext:long-float-digits) #,(bitsizeof 'ulong)))
  "The default precision argument for PARI functions which accept it.
This is always equal to (+ 2 (length (pari-mantissa (%foo PRPD)))),
t.e., this is the memory size for the real return value in ulong words.")
(defun pari-real-precision-words (digits)
  (+ 2 (ceiling (digits2bits digits) #,(bitsizeof 'ulong))))
(defun pari-real-precision (&optional (words pari-real-precision-words))
  "The real PARI precision in decimal digits."
  (bits2digits (* #,(bitsizeof 'ulong) (- words 2))))
(defun (setf pari-real-precision) (digits)
  (let ((bits (digits2bits digits)))
    (setf (ext:long-float-digits) bits
          pari-real-precision-words (+ 2 (ceiling bits #,(bitsizeof 'ulong)))))
  digits)
(define-symbol-macro pari-real-precision (pari-real-precision))
(def-c-var pari-series-precision (:name "precdl") (:type ulong))
(export '(pari-series-precision pari-real-precision))

;;; http://pari.math.u-bordeaux.fr/dochtml/html-stable/usersch4.html#Garbage-collection
;; extern THREAD pari_sp avma;
(exporting:def-c-var pari-avma (:name "avma") (:type pari_sp))
;; GEN gerepile(pari_sp ltop, pari_sp lbot, GEN q);
(exporting:def-call-out gerepile (:return-type pari-gen)
  (:arguments (ltop pari_sp) (lbot pari_sp) (q pari-gen)))


;; <paricom.h>
(def-c-var pari-bernzone (:name "bernzone") (:type pari-gen) (:read-only t))

(def-c-var pari-1    (:name "gen_1") (:type pari-gen) (:read-only t))
(def-c-var pari-2    (:name "gen_2") (:type pari-gen) (:read-only t))
(def-c-var pari-1/2  (:name "ghalf") (:type pari-gen) (:read-only t))
(def-c-var pari-0    (:name "gen_0") (:type pari-gen) (:read-only t))
(def-c-var pari--1   (:name "gen_m1") (:type pari-gen) (:read-only t))
(def-c-var pari--2   (:name "gen_m2") (:type pari-gen) (:read-only t))
(def-c-var pari-nil  (:name "gnil")  (:type pari-gen) (:read-only t))

;; extern  GEN primetab;
(def-c-var primetab (:type pari-gen) (:read-only t))

;; extern  byteptr diffptr;
(def-c-var diffptr (:type byteptr) (:read-only t))

(def-c-const MAXVARN)
;; extern entree **varentries;
;?(def-c-var varentries (:type (c-pointer (c-ptr-null entree))) (:read-only t))
;; (defun varentry (i)
;;   (assert (< i MAXVARN) (i)
;;           "~S: index ~:D is too large (max ~:D)" 'varentry i MAXVARN)
;;   (let ((e (offset varentries (* i #.(sizeof '(c-pointer (c-ptr-null entree))))
;;                    '(c-pointer (c-ptr-null entree)))))
;;     (and e (foreign-value e))))

;; extern int new_galois_format;
(def-c-var new_galois_format (:type int))
;; extern int factor_add_primes;
(def-c-var factor_add_primes (:type int))

;; extern ulong DEBUGFILES, DEBUGLEVEL, DEBUGMEM
(def-c-var debugfiles (:name "DEBUGFILES") (:type ulong))
(def-c-var debuglevel (:name "DEBUGLEVEL") (:type ulong))
(def-c-var debugmem (:name "DEBUGMEM") (:type ulong))

;; entree *is_entry(char *s);
(def-call-out is_entry (:arguments (s c-string))
  (:return-type (c-ptr-null entree)))

;; this optimization is not necessary, it just saves some memory.
;; also, get_entry_doc is called while loading pari.fas,
;; so it cannot be moved to cpari.c
(c-lines "char* get_entry_doc (char* s) { entree *e = is_entry(s); return e==NULL?NULL:(char*)e->help; }~%")
(def-call-out get_entry_doc (:arguments (s c-string)) (:return-type c-string))

(defun get-pari-docstring (str name)
  (let ((doc (get_entry_doc str)))
    (and doc
         (format nil "~A corresponds to the gp function ~A:~%~A"
                 name str doc))))

(def-c-const PARIVERSION (:type c-string))
(def-c-const PARI_VERSION_CODE)
(def-c-const PARI_VERSION_SHIFT)
(def-c-const PARIINFO (:type c-string))
(defconstant pari-version
  (list PARIVERSION
        (ldb (byte PARI_VERSION_SHIFT (* 2 PARI_VERSION_SHIFT))
             PARI_VERSION_CODE)
        (ldb (byte PARI_VERSION_SHIFT PARI_VERSION_SHIFT) PARI_VERSION_CODE)
        (ldb (byte PARI_VERSION_SHIFT 0) PARI_VERSION_CODE)
        PARIINFO))
(export 'pari-version)

;; --- should we really interface to these?
;; void paristack_newrsize(ulong newsize);
(def-call-out paristack_newrsize (:arguments (newsize ulong)) (:return-type nil))
;; void paristack_resize(ulong newsize);
(def-call-out paristack_resize (:arguments (newsize ulong)) (:return-type nil))
;; void paristack_setsize(size_t rsize, size_t vsize);
(def-call-out paristack_setsize (:arguments (rsize size_t) (vsize size_t))
  (:return-type nil))
;; void parivstack_resize(ulong newsize);
(def-call-out parivstack_resize (:arguments (newsize ulong)) (:return-type nil))
;; void parivstack_reset(void);
(def-call-out parivstack_reset (:arguments) (:return-type nil))

(c-lines "#include \"cpari.h\"~%")
(exporting:def-call-out pari-init (:name "init_for_clisp")
  (:arguments (parisize long) (maxprime long)) (:return-type nil))
(exporting:def-call-out pari-fini (:name "pari_close")
  (:arguments) (:return-type nil))
;; ulong maxprime(void);
(exporting:def-call-out maxprime (:return-type ulong) (:arguments))

(c-lines :init-always "init_for_clisp(8000000,500000);~%")
(c-lines :fini "pari_close();~%")

;;; define pari-call-out
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-arg-spec (arg)
    (if (symbolp arg)
      `(,arg pari-gen :in :none)
      (case (length arg)
        (1 `(,@arg pari-gen :in :none))
        (2 `(,@arg :in :none))
        (3 `(,@arg :none))
        (4 arg)
        (t `(,(first arg) ,(second arg) ,(third arg) ,(fourth arg))))))
  (defun make-pari-name (sym)
    (intern (ext:string-concat "%" (symbol-name sym)) (find-package "PARI")))
  (defun convert-to-lambdalist (args)
    (let ((flag nil))
      (mapcan #'(lambda (arg)
                  (if (symbolp arg)
                    (list arg)
                    (case (length arg)
                      ((1 2) (list (first arg)))
                      ((3 4) (if (eq (third arg) :out) '() (list (first arg))))
                      (t `(,@(if flag '() (progn (setq flag t) '(&key)))
                           (,(first arg) ,(fifth arg)
                            ,@(and (sixth arg) `(,(sixth arg)))))))))
              args)))
  (defun arg-preprocessing (args)
    (mapcan (lambda (arg)
              (let ((p (and (consp arg) (seventh arg))))
                (and p (list p))))
            args))
  (defun convert-to-arglist (args)
    (mapcan #'(lambda (arg)
                (if (symbolp arg)
                  `((convert-to-pari ,arg))
                  (case (length arg)
                    (1 `((convert-to-pari ,(first arg))))
                    (2 (if (eq (second arg) 'pari-gen)
                         `((convert-to-pari ,(first arg)))
                         `(,(first arg))))
                    (t (if (eq (third arg) :out) '()
                         (if (eq (second arg) 'pari-gen)
                           `((convert-to-pari ,(first arg)))
                           `(,(first arg))))))))
            args))
  (defun get-additional-return-spec (args)
    (mapcan #'(lambda (arg)
                (if (and (listp arg) (eq (third arg) :out))
                  (if (and (consp (second arg))
                           (eq (first (second arg)) 'c-ptr))
                    `((,(first arg) ,(second (second arg))))
                    (error "~S: :OUT parameter in ~S is not a pointer."
                           'get-additional-return-spec arg))
                  '()))
            args))
  (defun make-defun (name pari-name type args)
    (let ((add-values (get-additional-return-spec args)))
      (if (null add-values)
        `(progn
           (export ',name)
           (defun ,name ,(convert-to-lambdalist args)
             ,@(arg-preprocessing args)
             ,(case type
                (pari-gen
                   `(make-internal-pari-object
                      (,pari-name ,@(convert-to-arglist args))))
                (pari-bool
                   `(convert-to-boolean
                      (,pari-name ,@(convert-to-arglist args))))
                (t `(,pari-name ,@(convert-to-arglist args))))))
        (let* ((all-values (cons (list name type) add-values))
               (temp-vars (mapcar #'(lambda (v) (declare (ignore v)) (gensym))
                                  all-values)))
          `(progn
             (export ',name)
             (defun ,name ,(convert-to-lambdalist args)
               ,@(arg-preprocessing args)
               (multiple-value-bind ,temp-vars
                   (,pari-name ,@(convert-to-arglist args))
                 (values
                   ,@(mapcar #'(lambda (vs tv)
                                 (case (second vs)
                                   (pari-gen `(make-internal-pari-object ,tv))
                                   (pari-bool `(convert-to-boolean ,tv))
                                   (t tv)))
                             all-values temp-vars)))))))))
  (defun make-documentation (name gp-name args)
    `(let ((docstring (get-pari-docstring ,gp-name ',name))
           (docstr2 ,(format nil "Syntax: (~S~{ ~S~})~%"
                             name (convert-to-lambdalist args))))
       (setf (documentation ',name 'function)
             (if docstring
                 (format nil "~A~%~A" docstring docstr2)
                 docstr2)
             (documentation ',name 'pari::gp)
             ,gp-name))))

;;; The macro pari-call-out declares the pari functions to CLISP.
;;; Its syntax is
;;; (pari-call-out <fun-spec> <pari-name> (<arg-spec>*) [<gp-name>])
;;;  <fun-spec> ::= <name> | (<name> [<type> [<allocation>]])
;;;  <arg-spec> ::=   <name>
;;;                 | (<name> [<type> [<mode> [<allocation> [<default>]]]])
;;;  <pari-name>, <gp-name> ::= <string>
;;;  <name> ::= <symbol>
;;;  <type> ::= pari-gen | pari-bool | <c-type>
;;;  <mode> ::= :in | :out
;;;  <allocation> ::= :none | :malloc-free | :alloca
;;;  <default> ::= <expression>
;;; <gp-name> defaults to <pari-name>, <type> defaults to pari-gen,
;;; <mode> defaults to :in, and <allocation> defaults to :none.
;;; This defines a foreign function <pari-name> with arguments and return
;;; type as specified. Moreover, if <gp-name> is non-nil, a function
;;; <name> is defined and <name> is exported from the PARI package.
;;; Arguments and return value are converted as necessary.
;;; <arg-spec>s with <default> present must occur consecutively at the
;;; end of the <arg-spec> list (with :out parameters removed);
;;; the corresponding arguments are made
;;; into keyword arguments to <name> with defaults as given.
;;; If the return-type given was pari-bool, the result should be a pari
;;; zero or one and is converted to nil or t, respectively.
;;; A documentation string is provided.
(defmacro pari-call-out (fun lib-name args &optional (gp-name lib-name))
  (let* ((name (if (symbolp fun) fun (first fun)))
         (type (if (symbolp fun) 'pari-gen (second fun)))
         (rtype-spec (if (or (symbolp fun) (eq type 'pari-bool))
                      '(pari-gen)
                       (rest fun)))
         (pari-name (make-pari-name name)))
    `(progn
       (def-call-out ,pari-name
         (:name ,lib-name)
         (:return-type ,@rtype-spec)
         (:arguments ,@(mapcar #'make-arg-spec args)))
       ,@(when gp-name
           `(,(make-defun name pari-name type args)
             ,(make-documentation name gp-name args)))
       ',pari-name)))

;;; pari-call-out-prec has the same syntax as pari-call-out; it additionally
;;; provides a keyword argument prec defaulting to pari-real-precision-words.
(defmacro pari-call-out-prec (fun lib-name args &optional (gp-name lib-name))
  `(pari-call-out ,fun ,lib-name
     (,@args (prec long :in :none pari-real-precision-words prec-p
                   (when prec-p (setq prec (pari-real-precision-words prec)))))
     ,gp-name))

;; GEN gp_read_str(char *t);
(def-call-out %read-from-string (:name "gp_read_str")
  (:return-type pari-gen) (:arguments (str c-string)))
;; char* GENtostr(GEN x); --- uses f_PRETTYMAT, cannot be read back
(pari-call-out (write-to-string-pretty c-string :malloc-free) "GENtostr" (x) nil)
;; char* GENtostr_raw(GEN x);
(pari-call-out (write-to-string c-string :none) "GENtostr_raw" (x) nil)
;; char* GENtoTeXstr(GEN x);
(pari-call-out (write-to-string-TeX c-string :malloc-free) "GENtoTeXstr" (x) nil)

;; GEN variables_vecsmall(GEN x);
(pari-call-out variable-numbers "variables_vecsmall" (x) "variables")

;; long gvar(GEN x);
(pari-call-out (varno long) "gvar" (x))
;; INLINE int is_bigint(GEN n);
(pari-call-out (bigint? boolean) "is_bigint" (n))

(defun get-varno (x)
  (let ((vn (%varno (convert-to-pari x))))
    (if (bigint? vn) 0 vn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; low-level functionality

(defmacro extract0 ((var x) &body body)
  `(symbol-macrolet ((,var (memory-as ,x 'ulong 0)))
     ,@body))
(defmacro extract1 ((var x) &body body)
  `(symbol-macrolet ((,var (memory-as ,x 'ulong #,(sizeof 'ulong))))
     ,@body))

;; #define signe(x)          (((long)((GEN)(x))[1])>>SIGNSHIFT)
(defun pari-sign-raw (x)
  (extract1 (elt1 x)
    (ecase (ldb pari-sign-byte elt1)
      (0 0) (1 1) (#,(ash SIGNBITS (- SIGNSHIFT)) -1))))

;; #define setsigne(x,s)     (((GEN)(x))[1]=(((GEN)(x))[1]&(~SIGNBITS))+(((long)(s))<<SIGNSHIFT))
(defun (setf pari-sign-raw) (x s)
  (extract1 (elt1 x)
    (dpb s pari-sign-byte elt1)))

;; #define typ(x)            (((ulong)((GEN)(x))[0])>>TYPSHIFT)
(defun pari-type-raw (x)
  (extract0 (elt0 x)
    (ldb pari-type-byte elt0)))

;; #define settyp(x,s)       (((GEN)(x))[0]=(((GEN)(x))[0]&(~TYPBITS))+(((ulong)(s))<<TYPSHIFT))
(defun (setf pari-type-raw) (x s)
  (extract0 (elt0 x)
    (dpb s pari-type-byte elt0)))

;; #define lg(x)             ((long)(((GEN)(x))[0]&LGBITS))
(defun pari-length-raw (x)
  (extract0 (elt0 x)
    (ldb pari-length-byte elt0)))

;; #define setlg(x,s)        (((GEN)(x))[0]=(((GEN)(x))[0]&(~LGBITS))+(s))
;; #define lgefint(x)        ((long)(((ulong*)(x))[1]&LGBITS))
(defun pari-effective-length-raw (x)
  (extract1 (elt1 x)
    (ldb pari-length-byte elt1)))

;; #define setlgef(x,s)      (((GEN)(x))[1]=(((GEN)(x))[1]&(~LGBITS))+(s))
;; #define expo(x)           ((long)((((GEN)(x))[1]&EXPOBITS)-HIGHEXPOBIT))
(defun pari-exponent-raw (x)
  (extract1 (elt1 x)
    (- (ldb pari-exponent-byte elt1) pari-exponent-offset)))

;; #define setexpo(x,s)      (((GEN)(x))[1]=(((GEN)(x))[1]&(~EXPOBITS))+(HIGHEXPOBIT+(s)))
(defun (setf pari-exponent-raw) (x s)
  (extract1 (elt1 x)
    (dpb (+ s pari-exponent-offset) pari-exponent-byte elt1)))

;; #define valp(x)           ((long)((((GEN)(x))[1]&VALPBITS)-HIGHVALPBIT))
(defun pari-valuation-raw (x)
  (extract1 (elt1 x)
    (- (ldb pari-valuation-byte elt1) pari-valuation-offset)))

;; #define setvalp(x,s)      (((GEN)(x))[1]=(((GEN)(x))[1]&(~VALPBITS))+(HIGHVALPBIT+(s)))
(defun (setf pari-valuation-raw) (x s)
  (extract1 (elt1 x)
    (dpb (+ s pari-valuation-offset) pari-valuation-byte elt1)))

;; #define precp(x)          ((long)(((ulong)((GEN)(x))[1])>>PRECPSHIFT))
(defun pari-precision-raw (x)
  (extract1 (elt1 x)
    (ldb pari-precision-byte elt1)))

;; #define setprecp(x,s)     (((GEN)(x))[1]=(((GEN)(x))[1]&(~PRECPBITS))+(((long)(s))<<PRECPSHIFT))


;; #define varn(x)           ((long)((((GEN)(x))[1]&VARNBITS)>>VARNSHIFT))
(defun pari-varno-raw (x)
  (extract1 (elt1 x)
    (ldb pari-varno-byte elt1)))

;; #define setvarn(x,s)      (((GEN)(x))[1]=(((GEN)(x))[1]&(~VARNBITS))+(((ulong)(s))<<VARNSHIFT))
(defun (setf pari-varno-raw) (x s)
  (extract1 (elt1 x)
    (dpb s pari-varno-byte elt1)))

;; #define mant(x,i)         ((((GEN)(x))[1]&SIGNBITS)?((GEN)(x))[i+1]:0)
(defun pari-mantissa (x)
  (memory-as x (parse-c-type `(c-array ulong ,(- (pari-length-raw x) 2)))
             #,(* 2 (sizeof 'c-pointer))))

;; life sucks: the order of words in the data segment of integers depend on
;; whether pari is build with gmp (low bytes first) or not (high bytes first).
;; <http://article.gmane.org/gmane.comp.mathematics.pari.user/1574>
;; <http://pari.math.u-bordeaux.fr/archives/pari-users-1005/msg00008.html>
(c-lines "
void get_integer_data (GEN x, ulong len, ulong *data)
{ ulong i; for (i=0; i < len; i++) data[len-i-1] = *int_W(x,i); }
void set_integer_data (GEN x, ulong len, ulong *data) {
  /* 1st data element is the header, skip it */
  ulong i; len--; data++;
  for (i=0; i < len; i++) *int_W(x,i) = data[len-i-1];
}~%")
(def-call-out get_integer_data (:return-type nil)
  (:arguments (x pari-gen) (len ulong) (data c-pointer)))
(defun pari-get-integer-data (x)
  (let ((len (- (pari-effective-length-raw x) 2)))
    (with-foreign-object (data `(c-array ulong ,len))
      (get_integer_data x len data)
      (foreign-value data))))
(def-call-out set_integer_data (:return-type nil)
  (:arguments (x pari-gen) (len ulong) (data c-pointer)))
(defun pari-set-integer-data (x data)
  (let ((len (length data)))
    (with-foreign-object (data `(c-array ulong ,len) data)
      (set_integer_data x len data))))

;; #define setmant(x,i,s)    (((GEN)(x))[i+1]=s)

(defun pari-set-component (obj i ptr)
  (setf (memory-as obj #,(parse-c-type 'pari-gen)
                   (* #,(sizeof 'c-pointer) i)) ptr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conversion CLISP --> pari and pari --> CLISP

;; INLINE GEN cgetg(long x, long y);
(def-call-out pari-cgetg (:name "cgetg")
  (:return-type pari-gen)
  (:arguments (x long) (y long)))

;; Make a vector of ulongs into a pari object (including the second codeword,
;; which is element 0 of the vector). typecode is the pari type code.
(defun pari-make-object (vec typecode)
  (let* ((len (length vec)) (obj (pari-cgetg (1+ len) typecode)))
    (setf (memory-as obj (parse-c-type `(c-array ulong ,len))
                     #,(sizeof 'c-pointer))
          vec)
    obj))

;;; Define some CLISP analogs for pari types

(export '(pari-object pari-object-internal))

(defstruct pari-object
  "An abstract class for CLISP equivalents of pari objects")

(defgeneric convert-to-pari (x)
  (:documentation
   "Converts suitable CLISP objects into internal pari objects")
  (:method ((x null)) nil))

(defstruct (pari-object-internal (:include pari-object))
  "Pari object as a pointer into the pari stack"
  pointer)

(defun make-internal-pari-object (ptr)
  (and ptr (make-pari-object-internal :pointer ptr)))

(defmethod convert-to-pari ((x pari-object-internal))
  (pari-object-internal-pointer x))

;;; Make internal pari objects printable and readable

(defmethod print-object ((x pari-object-internal) stream)
  (format stream "#Z\"~A\"" (%write-to-string (pari-object-internal-pointer x)))
  x)

(defun pari-reader (stream subchar arg)
  (declare (ignore subchar))
  (when arg (error "~S: Between # and Z no number is allowed." 'read))
  (let ((str (read stream t nil t)))
    (unless (stringp str)
      (error "~S: After #Z a string must follow." 'read))
    (make-internal-pari-object (%read-from-string str))))

(set-dispatch-macro-character #\# #\Z #'pari-reader)

;;; Some helper macros for defining classes for pari objects

(defmacro define-pari-class (name slots)
  `(exporting:defstruct (,name (:include pari-object)) ,@slots))

(defmacro define-pari-converters (typecode name)
  (let ((readers (mapcar #'mop:slot-definition-readers
                         (ext:structure-direct-slots name))))
    `(progn
       (defmethod convert-to-pari ((x ,name))
         (let ((obj (pari-cgetg ,(+ 1 (length readers)) ,typecode)))
           ,@(let ((count 0))
                (mapcar (lambda (readerl)
                          `(pari-set-component obj ,(incf count)
                             (convert-to-pari (,(first readerl) x))))
                        readers))
           obj))
       (defun ,(intern (ext:string-concat "convert-from-pari-"
                                          (symbol-name typecode)) "PARI") (ptr)
         (,(ext:structure-keyword-constructor name)
           ,@(let ((count 0))
               (mapcan #'(lambda (dsd)
                           `(,(car (mop:slot-definition-initargs dsd))
                              (convert-from-pari
                               (%component ptr ,(incf count)))))
                       (ext:structure-direct-slots name))))))))

(defmacro define-pari (typecode name slots)
  `(progn (define-pari-class ,name ,slots)
          (define-pari-converters ,typecode ,name)))

;; INT=1: integers -- represented by CLISP integers

(defmethod convert-to-pari ((x (eql 0)))
  pari-0)

(defmethod convert-to-pari ((x (eql 1)))
  pari-1)

(defmethod convert-to-pari ((x (eql 2)))
  pari-2)

(defmethod convert-to-pari ((x (eql -1)))
  pari--1)

(defmethod convert-to-pari ((x (eql -2)))
  pari--2)

(defun extract-mantissa (vec len val)
  (do ((i len (1- i))
       (y val (ash y #,(- (bitsizeof 'ulong)))))
      ((eql i 0))
    (setf (svref vec i) (logand y #,(1- (ash 1 (bitsizeof 'ulong)))))))

(defmethod convert-to-pari ((x integer))
  (let* ((sign (signum x))
         (val (abs x))
         (len (ceiling (integer-length val) #,(bitsizeof 'ulong)))
         (vec (make-array (1+ len))))
    (setf (svref vec 0)
          (dpb sign pari-sign-byte
               (dpb (+ len 2) pari-length-byte 0)))
    (extract-mantissa vec len val)
    (let ((ptr (pari-make-object vec 1)))
      (pari-set-integer-data ptr vec)
      ptr)))

(defun collect-mantissa (mantissa)
  (let ((result 0))
    (dotimes (i (length mantissa) result)
      (setq result (+ (ash result #,(bitsizeof 'ulong)) (svref mantissa i))))))

(defun convert-from-pari-INT (ptr)
  (* (pari-sign-raw ptr) (collect-mantissa (pari-get-integer-data ptr))))

;; REAL=2: real numbers -- represented by CLISP floats

(defmethod convert-to-pari ((x float))
  (if (= x 0)
    (pari-make-object
      (vector (- pari-exponent-offset (* #,(bitsizeof 'ulong)
                                         pari-real-precision-words) -61) 0) 2)
    (multiple-value-bind (signif expo sign) (integer-decode-float x)
      (let ((pr (float-precision x)))
        ;; need ceil(pr/32) mantissa words,
        ;; signif has to be scaled by 2^(32*ceil(pr/32)-pr)
        ;; and the exponent will be pr+expo-1  (32 <-> (bitsizeof 'ulong))
        (multiple-value-bind (q r) (ceiling pr #,(bitsizeof 'ulong))
          (setq signif (ash signif (- r)))
          (let ((vec (make-array (1+ q))))
            (setf (svref vec 0)
              (dpb sign pari-sign-byte
                   (dpb (+ pari-exponent-offset pr expo -1)
                        pari-exponent-byte 0)))
            (extract-mantissa vec q signif)
            (pari-make-object vec 2)))))))

(defun convert-from-pari-REAL (ptr)
  (let* ((sign (pari-sign-raw ptr))
         (expo (pari-exponent-raw ptr))
         (mant (pari-mantissa ptr))
         (signif (collect-mantissa mant))
         (mant-bits (* #,(bitsizeof 'ulong) (length mant))))
    (* sign (if (zerop mant-bits) 0 ; no signed 0 in CLISP
                (scale-float (float signif (float-digits 1 mant-bits))
                             (- expo mant-bits -1))))))

;; INTMOD=3: integermods

(define-pari INTMOD pari-integermod (modulus rep))

;; FRAC=4: rational numbers -- represented by CLISP ratios

(defmethod convert-to-pari ((x (eql 1/2)))
  pari-1/2)

(defmethod convert-to-pari ((x ratio))
  (let ((obj (pari-cgetg 3 FRAC)))
    (pari-set-component obj 1 (convert-to-pari (numerator x)))
    (pari-set-component obj 2 (convert-to-pari (denominator x)))
    obj))

(defun convert-from-pari-FRAC (ptr)
  (/ (convert-from-pari (%component ptr 1))
     (convert-from-pari (%component ptr 2))))

;; COMPLEX=6: complex numbers -- represented by CLISP complex if possible

(define-pari-class pari-complex (realpart imagpart))

(defmethod convert-to-pari ((x (eql #C(0 1))))
  (pari::%I))

(defmethod convert-to-pari ((x complex))
  (let ((obj (pari-cgetg 3 COMPLEX)))
    (pari-set-component obj 1 (convert-to-pari (realpart x)))
    (pari-set-component obj 2 (convert-to-pari (imagpart x)))
    obj))

(defun convert-from-pari-COMPLEX (ptr)
  (if (and (member (pari-type-raw (%component ptr 1)) '(1 2 4 5))
           (member (pari-type-raw (%component ptr 2)) '(1 2 4 5)))
    ;; CLISP complex is possible
    (complex (convert-from-pari (%component ptr 1))
             (convert-from-pari (%component ptr 2)))
    ;; must construct pari-complex
    (make-pari-complex
     :realpart (convert-from-pari (%component ptr 1))
     :imagpart (convert-from-pari (%component ptr 2)))))

;; PADIC=7: p-adic numbers

(define-pari-class pari-padic (precp valp prime prpow rep))

(defmethod convert-to-pari ((x pari-padic))
  (let ((obj (pari-cgetg 5 PADIC)))
    (extract1 (elt1 obj)
      (setf elt1 (dpb (pari-padic-precp x) pari-precision-byte
                      (dpb (+ (pari-padic-valp x) pari-valuation-offset)
                           pari-valuation-byte 0))))
    (pari-set-component obj 2 (convert-to-pari (pari-padic-prime x)))
    (pari-set-component obj 3 (convert-to-pari (pari-padic-prpow x)))
    (pari-set-component obj 4 (convert-to-pari (pari-padic-rep x)))
    obj))

(defun convert-from-pari-PADIC (ptr)
  (make-pari-padic
   :precp (pari-precision-raw ptr)
   :valp  (pari-valuation-raw ptr)
   :prime (convert-from-pari (%component ptr 1))
   :prpow (convert-from-pari (%component ptr 2))
   :rep   (convert-from-pari (%component ptr 3))))

;; QUAD=8: quadratic numbers

(define-pari QUAD pari-quadratic (poly realpart imagpart))

;; POLMOD=9: polymods

(define-pari POLMOD pari-polymod (modulus rep))

;; POL=10: polynomials

(define-pari-class pari-poly (s varno coeffs))

(defmethod convert-to-pari ((x pari-poly))
  (let* ((coeffs (pari-poly-coeffs x))
         (obj (pari-cgetg (+ 2 (length coeffs)) POL)))
    (extract1 (elt1 obj)
      (setf elt1
            (dpb (pari-poly-s x) pari-sign-byte
                 (dpb (pari-poly-varno x) pari-varno-byte
                      (dpb (+ 2 (length coeffs))
                           pari-length-byte 0)))))
    (dotimes (i (length coeffs) obj)
      (pari-set-component obj (+ i 2) (convert-to-pari (svref coeffs i))))))

(defun convert-from-pari-POL (ptr)
  (let* ((s (pari-sign-raw ptr))
         (varno (pari-varno-raw ptr))
         (len (- (pari-length-raw ptr) 2))
         (coeffs (make-array len)))
    (dotimes (i len)
      (setf (svref coeffs i) (convert-from-pari (%component ptr (1+ i)))))
    (make-pari-poly :s s :varno varno :coeffs coeffs)))

;; SER=11: power series
(define-pari-class pari-pws (s varno expo coeffs))

(defmethod convert-to-pari ((x pari-pws))
  (let* ((coeffs (pari-pws-coeffs x))
         (obj (pari-cgetg (+ 2 (length coeffs)) SER)))
    (extract1 (elt1 obj)
      (setf elt1
            (dpb (pari-pws-s x) pari-sign-byte
                 (dpb (pari-pws-varno x) pari-varno-byte
                      (dpb (+ (pari-pws-expo x) pari-valuation-offset)
                           pari-valuation-byte 0)))))
    (dotimes (i (length coeffs) obj)
      (pari-set-component obj (+ i 2) (convert-to-pari (svref coeffs i))))))

(defun convert-from-pari-SER (ptr)
  (let* ((s (pari-sign-raw ptr))
         (varno (pari-varno-raw ptr))
         (expo (pari-valuation-raw ptr))
         (len (- (pari-length-raw ptr) 2))
         (coeffs (make-array len)))
    (dotimes (i len)
      (setf (svref coeffs i) (convert-from-pari (%component ptr (1+ i)))))
    (make-pari-pws :s s :varno varno :expo expo :coeffs coeffs)))

;; RFRAC=13: rational functions
(define-pari RFRAC pari-ratfun (numer denom))

;; QFR=15: indefinite binary quadratic forms

(define-pari QFR pari-real-qf (a b c d))

;; QFI=16: definite binary quadratic forms

(define-pari QFI pari-imag-qf (a b c))

;; VEC=17, COL=18, VECSMALL=22: vectors -- represented by CLISP vectors
;; #(:row v1 v2 ... vn) <---> row vector
;; #(:col v1 v2 ... vn) <---> column vector
;; #(v1 v2 ... vn)      <---> vector of small (long) integers
(defmethod convert-to-pari ((x vector))
  (let* ((len (length x)) (vecsmall-p nil)
         (obj (case (or (zerop len) (svref x 0))
                (:row (pari-cgetg len VEC))
                (:col (pari-cgetg len COL))
                (t (setq vecsmall-p t)
                   (pari-cgetg (1+ len) VECSMALL)))))
    (if vecsmall-p
        (setf (memory-as obj (parse-c-type `(c-array long ,len))
                         (sizeof 'c-pointer))
              x)
        (do ((i 1 (1+ i)))
            ((= i len))
          (pari-set-component obj i (convert-to-pari (svref x i)))))
    obj))

(defun convert-from-pari-vector (ptr &optional type)
  (let ((len (1- (pari-length-raw ptr))))
    (if type                    ; row or col
        (let ((vec (make-array (1+ len))))
          (setf (svref vec 0) type)
          (do ((i len (1- i)))
              ((eql i 0) vec)
            (setf (svref vec i) (convert-from-pari (%component ptr i)))))
        (memory-as ptr (parse-c-type `(c-array long ,len))
                   (sizeof 'c-pointer)))))

;; MAT=19: matrices -- represented by CLISP 2-dim arrays

(defmethod convert-to-pari ((x array))
  (unless (eql (array-rank x) 2)
    (error "~S: Array ~S is not 2-dimensional." 'convert-to-pari x))
  (let ((obj (pari-cgetg (1+ (array-dimension x 1)) MAT)))
    (dotimes (j (array-dimension x 1) obj)
      (let ((col (pari-cgetg (1+ (array-dimension x 0)) COL)))
        (dotimes (i (array-dimension x 0))
          (pari-set-component col (1+ i) (convert-to-pari (aref x i j))))
        (pari-set-component obj (1+ j) col)))))

(defun convert-from-pari-MAT (ptr)
  (let ((cols (1- (pari-length-raw ptr))))
    (if (eql cols 0)
      (make-array '()) ; probably shouldn't happen...
      (let* ((rows (1- (pari-length-raw (%component ptr 1))))
             (arr (make-array (list rows cols))))
        (dotimes (j cols arr)
          (let ((col (%component ptr (1+ j))))
            (unless (eql (1- (pari-length-raw col)) rows)
              (error "~S: Pari matrix has columns of unequal length."
                     'convert-from-pari))
            (dotimes (i rows)
              (setf (aref arr i j)
                    (convert-from-pari (%component col (1+ i)))))))))))

;;; Conversion from pari -- dispatch

(defun pari-type-raw-to-symbol (type-code)
  (gethash type-code (get 'pari-typecode 'ffi:def-c-enum)))

(defun convert-from-pari (ptr)
  "Converts an internal pari object to a CLISP object"
  (case (pari-type-raw ptr)
    (1 (convert-from-pari-INT ptr))
    (2 (convert-from-pari-REAL ptr))
    (3 (convert-from-pari-INTMOD ptr))
    (4 (convert-from-pari-FRAC ptr))
    (6 (convert-from-pari-COMPLEX ptr))
    (7 (convert-from-pari-PADIC ptr))
    (8 (convert-from-pari-QUAD ptr))
    (9 (convert-from-pari-POLMOD ptr))
    (10 (convert-from-pari-POL ptr))
    (11 (convert-from-pari-SER ptr))
    (13 (convert-from-pari-RFRAC ptr))
    (15 (convert-from-pari-QFR ptr))
    (16 (convert-from-pari-QFI ptr))
    (17 (convert-from-pari-vector ptr :row))
    (18 (convert-from-pari-vector ptr :col))
    (19 (convert-from-pari-MAT ptr))
    (22 (convert-from-pari-vector ptr))
    (t (error "~S: Pari type ~D(~S) is not yet implemented as a CLISP type."
              'convert-from-pari (pari-type-raw ptr)
              (pari-type-raw-to-symbol (pari-type-raw ptr))))))

(defun convert-to-boolean (ptr)
  (case (convert-from-pari ptr)
    (0 nil)
    (1 t)
    (t (error "Pari predicate returned ~S instead of 0 or 1."
              (convert-from-pari ptr)))))

(export '(pari-to-lisp pari-type))
(defgeneric pari-to-lisp (x))

(defmethod pari-to-lisp ((x pari-object)) x)
(defmethod pari-to-lisp ((x pari-object-internal))
  (convert-from-pari (pari-object-internal-pointer x)))
(defmethod pari-to-lisp ((x number)) x)
(defmethod pari-to-lisp ((x array)) x)
(defmethod pari-to-lisp ((x null)) x)

(defun pari-type (obj)
  (let ((type-code
         (pari-type-raw
          (if (pari-object-p obj)
              (pari-object-internal-pointer obj)
              (foreign-address obj)))))
    (or (pari-type-raw-to-symbol type-code)
        (format nil "unknown-typecode-~S" type-code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generate bindings at compile time from /usr/share/pari/pari.desc
;;; "/usr/share" == DATADIR comes from *ARGS* by way of configure
(defmacro process-desc-file ()
  `(progn
     ,@(mapcar #'desc-to-ffi
               (read-pari-desc-file
                (ext:string-concat (first ext:*args*) "/pari/pari.desc")))))
(process-desc-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARI functions not present in GP (pari.desc)

;;; https://pari.math.u-bordeaux.fr/dochtml/html-stable/usersch5.html

;; #Miscellaneous-Boolean-functions
;; int iscomplex(GEN x);
(pari-call-out (iscomplex boolean) "iscomplex" (x))
;; int isexactzero(GEN g);
(pari-call-out (isexactzero boolean) "isexactzero" (g))
;; int isrationalzeroscalar(GEN g);
(pari-call-out (isrationalzeroscalar boolean) "isrationalzeroscalar" (g))
;; int isinexact(GEN x);
(pari-call-out (isinexact boolean) "isinexact" (x))
;; int isinexactreal(GEN x);
(pari-call-out (isinexactreal boolean) "isinexactreal" (x))
;; int isint(GEN n, GEN *ptk);
(pari-call-out (isint boolean) "isint" (n (ptk (c-ptr pari-gen) :out :alloca)))
;; int isrationalzero(GEN g);
(pari-call-out (isrationalzero boolean) "isrationalzero" (g))
;; int issmall(GEN n, long *ptk);
(pari-call-out (issmall boolean) "issmall" (n (ptk (c-ptr pari-gen) :out :alloca)))

;; #GCD-content-and-primitive-part
;; GEN primpart(GEN x);
(pari-call-out primpart "primpart" (x))
;; GEN primitive_part (GEN x, GEN *c);
(pari-call-out primitive-part "primitive_part"
  (x (c (c-ptr pari-gen) :out :alloca)))

;; #Constructors
;; INLINE GEN pol_0(long v);
(pari-call-out pol_0 "pol_0" ((v long)))
;; INLINE GEN pol_1(long v);
(pari-call-out pol_1 "pol_1" ((v long)))
;; INLINE GEN pol_x(long v);
(pari-call-out pol_x "pol_x" ((v long)))
;; GEN scalarcol(GEN x, long n);
(pari-call-out scalarcol "scalarcol" (x (n long)))
;; GEN scalarmat(GEN x, long n);
(pari-call-out scalarmat "scalarmat" (x (n long)))

;; #Small-groups
;; GEN galois_group(GEN gal);
(pari-call-out galois_group "galois_group" (gal))

;;; https://pari.math.u-bordeaux.fr/dochtml/html-stable/usersch6.html

;; GEN idealaddmultoone(GEN nf, GEN list);
(pari-call-out idealaddmultoone "idealaddmultoone" (nf list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; variants mentioned in pari.desc without their own entry

;; GEN caradj(GEN x, long v, GEN *py);
(pari-call-out caradj "caradj"
  (x (varno long :in :none 0) (pt (c-ptr pari-gen) :out :alloca)) "charpoly")
;; GEN ellrandom(GEN e);
(pari-call-out ellrandom "ellrandom" (e) "random")
;; GEN ffrandom(GEN ff);
(pari-call-out ffrandom "ffrandom" (ff) "random")
;; GEN ginv(GEN x);
(pari-call-out ginv "ginv" (x) "_/_")
;; GEN nfinv(GEN nf, GEN x);
(pari-call-out nfinv "nfinv" (nf x) "nfeltpow")

;; NB: _==_ returns pari-gen, not a boolean!
;; int gequal0(GEN x);
(pari-call-out (gequal0 boolean) "gequal0" (x) "_==_")
;; int gequal1(GEN x);
(pari-call-out (gequal1 boolean) "gequal1" (x) "_==_")
;; int gequalm1(GEN x);
(pari-call-out (gequalm1 boolean) "gequalm1" (x) "_==_")
;; int gequal(GEN x, GEN y);
(pari-call-out (gequal boolean) "gequal" (x y) "_==_")

;; long gsizeword(GEN x);
(pari-call-out (sizeword long) "gsizeword" (x) "sizebyte")



;; local variables:
;; eval: (put 'pari-call-out 'common-lisp-indent-function 'defun)
;; eval: (put 'pari-call-out-prec 'common-lisp-indent-function 'defun)
;; eval: (font-lock-add-keywords nil '(("(\\(pari-call-out\\(-prec\\)?\\)\\s *\\(\\(\\s_\\|\\sw\\)*\\)" (1 font-lock-keyword-face) (3 font-lock-function-name-face)) ("(\\(pari-call-out\\(-prec\\)?\\)\\s *(\\(\\(\\s_\\|\\sw\\)*\\)\\s *\\(\\(\\s_\\|\\sw\\)*\\).*)" (1 font-lock-keyword-face) (3 font-lock-function-name-face) (5 font-lock-type-face))))
;; end:
