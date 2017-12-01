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
(c-lines "#undef T~%#include <pari/pari.h>~%")

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

(defun next-entree (e) (foreign-value (entree-next e)))
(export 'next-entree)

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

;; extern THREAD pari_sp avma;
(def-c-var pari-avma (:name "avma") (:type pari_sp))

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

;;; /* init.c */

;; --- should we really export these?
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
(def-call-out pari-init (:name "init_for_clisp")
  (:arguments (parisize long) (maxprime long)) (:return-type nil))
(def-call-out pari-fini (:name "pari_close")
  (:arguments) (:return-type nil))
(export '(pari-init pari-fini))
(c-lines :init-always "init_for_clisp(8000000,500000);~%")
(c-lines :fini "pari_close();~%")


;; #define gval(x,v) ggval(x,pol_x[v])
;; #define gvar9(x) ((typ(x)==9)?gvar2(x):gvar(x))

;; #define coeff(a,i,j)      (*((long*)(*(a+(j)))+(i)))
;; #define gcoeff(a,i,j)     (GEN)coeff(a,i,j)
;; #define bern(i)           (GEN)(bernzone + (i)*(*(bernzone + 2)) + 3)

(eval-when (compile load eval)
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
                 docstr2)))))

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


;;; /* alglin.c */

;; GEN gtrans(GEN x);
(pari-call-out matrix-transpose "gtrans" (x) "trans")
;; GEN scalarmat(GEN x, long n);
(pari-call-out scalar-matrix "scalarmat" (x (n long)) "?")
;; GEN gaddmat(GEN x, GEN y);
;; GEN gaddsmat(long s, GEN y);
;; GEN inverseimage(GEN mat, GEN y);
(pari-call-out matrix-inverse-image "inverseimage" (mat y))

;; GEN ker(GEN x);
(pari-call-out matrix-kernel "ker" (x))
;; GEN keri(GEN x);
(pari-call-out matrix-kernel-integral "keri" (x))
;; GEN kerreel(GEN x, long prec);
;;(pari-call-out-prec matrix-kernel-inexact "kerreel" (x) "kerr")

;; GEN image(GEN x);
(pari-call-out matrix-image "image" (x))
;; GEN imagereel(GEN x, long prec);
;;(pari-call-out-prec matrix-image-inexact "imagereel" (x) "imager")
;; GEN imagecompl(GEN x);
(pari-call-out matrix-image-complement "imagecompl" (x))
;; GEN image2(GEN x);
;; GEN suppl(GEN x);
(pari-call-out matrix-supplement "suppl" (x) "supplement")
;; GEN eigen(GEN x, long prec);
(pari-call-out-prec matrix-eigenvectors "eigen" (x))
;; GEN hess(GEN x);
(pari-call-out matrix-to-hessenberg-form "hess" (x))

;; GEN carhess(GEN x, long v);
(pari-call-out characteristic-polynomial-hessenberg "carhess"
  (x (varno long)) "char2")

;; GEN gauss(GEN a, GEN b);
(pari-call-out matrix-solve "gauss" (a b))
;; GEN invmat(GEN a);
;; GEN det(GEN a);
(pari-call-out matrix-determinant "det" (a))
;; GEN detreel(GEN a);
;; GEN det2(GEN a);

;; GEN caract(GEN x, int v);
;; GEN caradj(GEN x, long v, GEN *py);
(pari-call-out characteristic-polynomial-and-adjoint-matrix "caradj"
  (x (varno long :in :none 0) (py (c-ptr pari-gen) :out :alloca)) "?")
;; GEN adj(GEN x);
(pari-call-out adjoint-matrix "adj" (x))
;; GEN charpoly0(GEN x, long v,long flag);
(pari-call-out characteristic-polynomial "charpoly0"
  (x (varno long :in :none 0) (flag long :in :none 5)))
;; GEN gtrace(GEN x);
(pari-call-out pari-trace "gtrace" (x))
;; GEN quicktrace(GEN x,GEN sym);
;;(pari-call-out pari-quicktrace "quicktrace" (x sym))

;; GEN assmat(GEN x);
;; GEN gnorm(GEN x);
(pari-call-out norm "gnorm" (x) "norm")
;; GEN gnorml2(GEN x);
(pari-call-out l2-norm "gnorml2" (x) "norml2")
;; GEN gconj(GEN x);
(pari-call-out pari-conjugate "gconj" (x) "conj")
;; GEN conjvec(GEN x,long prec);
(pari-call-out-prec vector-of-conjugates "conjvec" (x))
;; GEN matid(long n);
(pari-call-out identity-matrix "matid" ((n long)))
;; GEN concat(GEN x, GEN y);
(pari-call-out pari-concatenate "concat" (x y))

;; GEN diagonal(GEN x);
(pari-call-out diagonal "diagonal" (x))
;; GEN extract0(GEN x, GEN l1, GEN l2);
(pari-call-out matrix-extract "extract0" (x l1 l2))
(sys::deprecate 'vector-extract 'matrix-extract
                (lambda (x l) (matrix-extract x l nil)))
;; GEN gtomat(GEN x);
(pari-call-out convert-to-matrix "gtomat" (x) "mat")
;; GEN invmulmat(GEN a, GEN b);
;; GEN invmulmatreel(GEN a, GEN b);
;;(pari-call-out matrix-invert-and-multiply-inexact "invmulmatreel" (a b) "?")
;; GEN invmatreel(GEN a);
;;(pari-call-out matrix-invert-inexact "invmatreel" (a) "matinvr")

;; GEN qfgaussred(GEN a);
(pari-call-out symmetric-matrix-sqred "qfgaussred" (a))
;; GEN qfsign(GEN a);
(pari-call-out symmetric-matrix-signature "qfsign" (a))
;; GEN jacobi(GEN a, long prec);
(pari-call-out-prec symmetric-matrix-eigenstuff "jacobi" (a))
;; GEN matrixqz0(GEN x, GEN pp);
(pari-call-out matrix-qz "matrixqz0" (x pp))
;; GEN QM_minors_coprime(GEN x, GEN pp);
(pari-call-out QM-minors-coprime "QM_minors_coprime" (x pp))
;; GEN QM_ImZ_hnf(GEN x);
(pari-call-out QM-ImZ-hnf "QM_ImZ_hnf" (x))
;; GEN QM_ImQ_hnf(GEN x);
(pari-call-out QM-ImQ-hnf "QM_ImQ_hnf" (x))

;; GEN indexrank(GEN x);
(pari-call-out matrix-indexrank "indexrank" (x))
;; GEN matkerint0(GEN x,long flag);
(pari-call-out matrix-kernel-integral-reduced "matkerint0" (x (flag long)))
;; GEN intersect(GEN x, GEN y);
(pari-call-out matrix-subspace-intersection "intersect" (x y))
;; GEN deplin(GEN x);
(pari-call-out linear-dependence "deplin" (x))
;; GEN detint(GEN x);
(pari-call-out matrix-determinant-multiple "detint" (x))


;; GEN hnfspec(long** mat,GEN* ptdep,GEN* ptmatc,long* vperm,GEN* ptmatalpha,long co,long li,long k0,long* ptnlze,long* ptcol);

;; GEN hnffinal(GEN matgen,GEN* ptpdep,GEN* ptmatc,long* vperm,GEN* ptmatalpha,long lnz,long co,long li,long col,long lig,long nlze,long* ptcol);

;; GEN hnfadd(GEN mit,GEN* ptpdep,GEN* ptmatc,long* vperm,GEN* ptmatalpha,long co,long li,long col,long* ptnlze,GEN extramat,GEN extramatc);

;; long    rank(GEN x);
(pari-call-out (matrix-rank long) "rank" (x))
;; GEN perf(GEN a);
(pari-call-out symmetric-matrix-perfection "perf" (a))

;;; /* anal.c */

;; GEN readexpr(char *t);
;; GEN gp_read_str(char *t);
(def-call-out %read-from-string (:name "gp_read_str")
  (:return-type pari-gen) (:arguments (str c-string)))

;; void switchin(char *name);
;; void switchout(char *name);

;;; /* arith.c */

;; GEN sqrtint(GEN a);
(pari-call-out pari-isqrt "sqrtint" (a) "isqrt")
;; GEN mpfact(long n);
(pari-call-out factorial-integer "mpfact" ((n long)) "!")
;; GEN mpfactr(long n, long prec);
(pari-call-out-prec factorial-real "mpfactr" ((n long)) "fact")

;; GEN sfcont(GEN x, GEN x1, long k);
;; GEN sfcont2(GEN b, GEN x);
;; GEN gcf(GEN x);
(pari-call-out continued-fraction "gcf" (x) "cf")
;; GEN gcf2(GEN b, GEN x);
(pari-call-out continued-fraction-2 "gcf2" (b x) "cf2")
;; GEN pnqn(GEN x);
(pari-call-out continued-fraction-convergent "pnqn" (x))
;; GEN gboundcf(GEN x, long k);
(pari-call-out bounded-continued-fraction "gboundcf" (x (k long)) "boundcf")

;; GEN bestappr(GEN x, GEN k);
(pari-call-out best-rational-approximation "bestappr" (x k))
;; GEN addprimes(GEN primes);
(pari-call-out add-primes "addprimes" (primes) "addprimes")

;; GEN bezout(GEN a, GEN b, GEN *u, GEN *v); -- ext:xgcd is better!
;; (pari-call-out bezout "bezout" (a b (u (c-ptr pari-gen) :out :alloca)
;;                                   (v (c-ptr pari-gen) :out :alloca)))
;; GEN chinese(GEN x, GEN y);
(pari-call-out chinese-lift "chinese" (x y))
;; GEN Fp_inv(GEN a, GEN m);
;; GEN puissmodulo(GEN a, GEN n, GEN m);
;; GEN fibo(long n);
(pari-call-out fibonacci "fibo" ((n long)))
;; GEN nextprime(GEN n);
(pari-call-out next-prime "nextprime" (n) "nextprime")
;; GEN prime(long n);
(pari-call-out nth-prime "prime" ((n long)))

;; GEN primes(long n);
(pari-call-out first-n-primes "primes" ((n long)))
;; GEN eulerphi(GEN n);
(pari-call-out euler-phi "eulerphi" (n))
;; GEN Z_factor(GEN n);
;; GEN auxdecomp(GEN n, long all);
;; GEN factor(GEN n, long lim);
(pari-call-out factor-bounded "factor" (n (lim long)))

;; GEN sumdiv(GEN n);
(pari-call-out sum-divisors "sumdiv" (n) "sigma")
;; GEN sumdivk(long k, GEN n);
(pari-call-out sum-divisor-powers "sumdivk" ((k long) n) "sigmak")
;; GEN numbdiv(GEN n);
(pari-call-out count-divisors "numbdiv" (n) "numdiv")
;; GEN binaire(GEN x);
(pari-call-out binaire "binaire" (x) "binaire")
;; GEN order(GEN x);
(pari-call-out order "order" (x))
;; GEN znprimroot(GEN m);
(pari-call-out primitive-root "znprimroot" (m) "primroot")
;; GEN znstar(GEN x);
(pari-call-out structure-of-z/n* "znstar" (x))
;; GEN divisors(GEN n);
(pari-call-out divisors "divisors" (n))

;; GEN classno(GEN x);
(pari-call-out quadratic-class-number "classno" (x))
;; GEN classno2(GEN x);
;; GEN hclassno(GEN x);
;; GEN quadunit(GEN x);
(pari-call-out quadratic-unit "quadunit" (x) "unit")
;; GEN quadregulator(GEN x, long prec);
(pari-call-out-prec quadratic-regulator "quadregulator" (x))

;; GEN qfi(GEN x, GEN y, GEN z);
(pari-call-out make-imag-qf "qfi" (x y z))
;; GEN qficomp(GEN x, GEN y);
(pari-call-out compose-imag-qf "qficomp" (x y))
;; GEN redimag(GEN x);
(pari-call-out reduce-imag-qf "redimag" (x))
;; GEN qfr(GEN x, GEN y, GEN z, GEN d);
(pari-call-out make-real-qf "qfr" (x y z d))
;; GEN qfrcomp(GEN x, GEN y);
(pari-call-out compose-real-qf "qfrcomp" (x y))
;; GEN redreal(GEN x);
(pari-call-out reduce-real-qf "redreal" (x))

;; GEN rhoreal(GEN x);
(pari-call-out reduce-real-qf-one-step "rhoreal" (x))
;; GEN rhorealnod(GEN x, GEN isqrtD);
(pari-call-out reduce-real-qf-no-d-one-step "rhorealnod" (x isqrtD))
;; GEN redrealnod(GEN x, GEN isqrtD);
(pari-call-out reduce-real-qf-no-d "redrealnod" (x isqrtD))

;; GEN primeform(GEN x, GEN p, long prec);
(pari-call-out-prec prime-form "primeform" (x p) "pf")

;; GEN nucomp(GEN x, GEN y, GEN l);
(pari-call-out shanks-compose-imag-qf "nucomp" (x y l))
;; GEN nudupl(GEN x, GEN l);
(pari-call-out shanks-double-imag-qf "nudupl" (x l))
;; GEN nupow(GEN x, GEN n);
(pari-call-out shanks-power-imag-qf "nupow" (x n))

;; GEN qfbpowraw(GEN x, long n);
(pari-call-out power-real-qf-raw "qfbpowraw" (x (n long)))

;; GEN gkronecker(GEN x, GEN y);
;; GEN gkrogs(GEN x, long y);
;; GEN gissquare(GEN x);
(pari-call-out (gsquare? pari-bool) "gissquare" (x) "issquare")
;; long issquare(GEN x);
(pari-call-out (square? boolean) "issquare" (x))
;; GEN gissquarerem(GEN x, GEN *pt);

;; GEN gisprime(GEN x, long flag);
(pari-call-out (prime? pari-bool) "gisprime" (x (flag long)) "isprime")
;; GEN gispseudoprime(GEN x);z,
(pari-call-out (pseudo-prime? pari-bool) "gispseudoprime" (x (flag long)) "ispsp")
;; GEN gbittest(GEN x, GEN n);

;; GEN gpseudopremier(GEN n, GEN a);
;; GEN gmu(GEN n);

;; GEN gomega(GEN n);
;; GEN gbigomega(GEN n);

;; long kronecker(GEN x, GEN y);
(pari-call-out (kronecker-symbol long) "kronecker" (x y) "kro")
;; GEN krosg(long s, GEN x);
;; GEN krogs(GEN x, long y);
;; GEN kross(long x, long y);
;; GEN kro8(GEN x, GEN y);

;; long moebius(GEN n);
(pari-call-out (moebius-mu long) "moebius" (n))
;; long omega(GEN n);
(pari-call-out (omega long) "omega" (n))
;; long bigomega(GEN n);
(pari-call-out (bigomega long) "bigomega" (n))
;; GEN hilbert(GEN x, GEN y, GEN p);
(pari-call-out (hilbert-symbol long) "hilbert" (x y p) "hilbert symbol")

;; int carreparfait(GEN x);
;; GEN carrecomplet(GEN x, GEN *pt);
;; GEN bittest(GEN x, long n);

;; long isprime(GEN x);
;; GEN ispsp(GEN x);
;; long issquarefree(GEN x);
(pari-call-out (square-free? boolean) "issquarefree" (x))
;; GEN isfundamental(GEN x);
;; GEN Fp_sqrt(GEN a, GEN p, GEN *pr);

;; int millerrabin(GEN n, long k);
;; GEN pseudopremier(GEN n, GEN a);
;; GEN inversemodulo(GEN a, GEN b, GEN *res);

;; byteptr initprimes(long maxnum);

;; ulong maxprime(void);
(pari-call-out (maxprime ulong) "maxprime" ())

;; void lucas(long n, GEN *ln, GEN *ln1);

;;; /* base.c */

;; GEN nfbasis(GEN x, GEN *y, GEN p);
(pari-call-out nf-basis "nfbasis" (x (y (c-ptr pari-gen) :out :alloca) p) "basis")
;; GEN nfdisc(GEN x);
(pari-call-out nf-field-discriminant "nfdisc" (x))

;; GEN hnf(GEN x);
(pari-call-out matrix-to-hnf "hnf" (x) "hermite")
;; GEN hnfhavas(GEN x);
;; GEN hnfnew(GEN x);
;; GEN hnfperm(GEN x);

;; GEN cleanmod(GEN x,long lim,GEN detmat,GEN detmatsur2);

;; GEN hnfmod(GEN x, GEN detmat);
(pari-call-out matrix-to-hnf-mod "hnfmod" (x detmat) "hermitemod")
;; GEN hnfmodid(GEN x,GEN p);
;; GEN smith(GEN x);
(pari-call-out matrix-elementary-divisors "smith" (x))
;; GEN smith2(GEN x);
(pari-call-out matrix-elementary-divisors-transforms "smith2" (x))

;; GEN allbase(GEN x, long code, GEN *y);
;; GEN galois_group(GEN gal);
(pari-call-out nf-galois-group "galois_group" (gal))
;; GEN nfinit(GEN x, long prec);
(pari-call-out-prec nf-init "nfinit" (x))
;; GEN nfinitred(GEN x, long prec);
(pari-call-out-prec nf-init-reduced "nfinitred" (x))
;; GEN nfinitred2(GEN x, long prec);
(pari-call-out-prec nf-init-reduced-2 "nfinitred2" (x))

;; GEN tschirnhaus(GEN x);
(pari-call-out nf-tschirnhausen-transformation "tschirnhaus" (x))
;; GEN galoisapply(GEN nf, GEN aut, GEN x);
(pari-call-out nf-apply-galois "galoisapply" (nf aut x))
;; GEN galoisconj(GEN nf);
(pari-call-out-prec nf-galois-conjugates "galoisconj" (nf))
;; GEN galoisconj0(GEN nf, long flag, GEN d, long prec);
(pari-call-out-prec nf-galois-conjugates-0 "galoisconj0" (nf (flag long) d))

;; GEN idealprimedec(GEN nf,GEN p);
(pari-call-out nf-prime-decomposition "idealprimedec" (nf p))
;; GEN idealmul(GEN nf,GEN ix,GEN iy);
(pari-call-out ideal-multiply "idealmul" (nf ix iy))
;; GEN idealmulred(GEN nf, GEN ix, GEN iy, long prec);
(pari-call-out-prec ideal-multiply-reduced "idealmulred" (nf ix iy))
;; GEN ideal_two_elt(GEN nf, GEN ix);
(pari-call-out ideal-2-generators "ideal_two_elt" (nf ix) "idealtwoelt")

;; GEN idealmulh(GEN nf, GEN ix, GEN iy);
;; GEN element_mulh(GEN nf, long limi, long limj, GEN x, GEN y);

;; GEN idealmulprime(GEN nf,GEN ix,GEN vp);
;; GEN minideal(GEN nf,GEN ix,GEN vdir,long prec);

;; GEN idealmulelt(GEN nf, GEN elt, GEN x);
;; GEN idealmullll(GEN nf, GEN x, GEN y);

;; GEN ideallllredall(GEN nf, GEN ix, GEN vdir, long prec, long precint);

;; GEN ideallllred(GEN nf,GEN ix,GEN vdir,long prec);
(pari-call-out-prec ideal-lll-reduction "ideallllred" (nf ix vdir))

;; GEN ideallllredpart1(GEN nf,GEN x,GEN vdir, long flprem, long prec);

;; GEN ideallllredpart1spec(GEN nf, GEN x, GEN matt2, long flprem, long prec);

;; GEN ideallllredpart2(GEN nf,GEN arch,GEN z,long prec);

;; GEN nfmul(GEN nf,GEN x,GEN y);
(pari-call-out nf-element-multiply "nfmul" (nf x y) "nfmul")
;; GEN nfsqr(GEN nf,GEN x);
(pari-call-out nf-element-sqr "nfsqr" (nf x) "nfsqr")
;; GEN nfpow(GEN nf,GEN x,GEN k);
(pari-call-out nf-element-power "nfpow" (nf x k) "nfpow")

;; GEN rootsof1(GEN x);
(pari-call-out nf-roots-of-unity "rootsof1" (x))
;; GEN idealinv(GEN nf, GEN ix);
(pari-call-out ideal-invert "idealinv" (nf x))

;; GEN idealpow(GEN nf, GEN ix, GEN n);
(pari-call-out ideal-power "idealpow" (nf iX n))
;; GEN idealpowred(GEN nf, GEN ix, GEN n, long prec);
(pari-call-out-prec ideal-power-reduced "idealpowred" (nf ix n))
;; GEN idealpows(GEN nf, GEN ideal, long iexp);

;; GEN idealpowprime(GEN nf, GEN vp, GEN n,long prec);
;; GEN idealfactor(GEN nf, GEN x);
(pari-call-out ideal-factor "idealfactor" (nf x))

;; GEN idealhermite(GEN nf, GEN x);
(pari-call-out ideal-to-hnf "idealhermite" (nf x))
;; GEN idealhnf0(GEN nf, GEN a, GEN b);
(pari-call-out ideal-2-generators-to-hnf "idealhnf0" (nf a b))
;; GEN idealadd(GEN nf, GEN x, GEN y);
(pari-call-out ideal-add "idealadd" (nf x y))
;; GEN idealaddtoone(GEN nf, GEN x, GEN y);
(pari-call-out ideal-split-one-2 "idealaddtoone" (nf x y))
;; GEN idealaddmultoone(GEN nf, GEN list);
(pari-call-out ideal-split-one-n "idealaddmultoone" (nf l))
;; GEN idealdiv(GEN nf, GEN x, GEN y);
(pari-call-out ideal-divide "idealdiv" (nf x y))

;; GEN idealintersect(GEN nf, GEN x, GEN y);
(pari-call-out ideal-intersection "idealintersect" (nf x y))

;; GEN idealdivexact(GEN nf, GEN x, GEN y);
(pari-call-out ideal-divide-exact "idealdivexact" (nf x y))
;; GEN idealnorm(GEN nf, GEN x);
(pari-call-out ideal-norm "idealnorm" (nf x))

;; GEN idealappr(GEN nf, GEN x);
(pari-call-out ideal-approximate "idealappr" (nf x))
;; GEN idealapprfact(GEN nf, GEN x);
(pari-call-out ideal-approximate-factored "idealapprfact" (nf x))
;; GEN idealapprall(GEN nf, GEN x, long fl);
;; GEN idealchinese(GEN nf, GEN x, GEN y);

;; GEN idealcoprime(GEN nf, GEN x, GEN y);
(pari-call-out ideal-coprime "idealcoprime" (nf x y))
;; GEN ideal_two_elt2(GEN nf, GEN x, GEN a);
(pari-call-out ideal-2-generators-2 "ideal_two_elt2" (nf x a) "idealtwoelt2")

;; GEN twototwo(GEN nf, GEN a, GEN b);
;; GEN threetotwo(GEN nf, GEN a, GEN b, GEN c);
;; GEN threetotwo1(GEN nf, GEN a, GEN b, GEN c);
;; GEN threetotwo2(GEN nf, GEN a, GEN b, GEN c);
;;(pari-call-out nf-element-three-to-two "threetotwo" (nf a b c))
;;(pari-call-out nf-element-two-to-two "twototwo" (nf a b))

;; GEN basistoalg(GEN nf, GEN x);
(pari-call-out nf-basis-to-alg "basistoalg" (nf x))
;; GEN algtobasis(GEN nf, GEN x);
(pari-call-out nf-alg-to-basis "algtobasis" (nf x))

;; GEN weakhermite(GEN nf, GEN x);
;; GEN nfhnf(GEN nf, GEN x);
(pari-call-out nf-pseudo-to-hnf "nfhnf" (nf x))
;; GEN nfhnfmod(GEN nf, GEN x, GEN detmat);
(pari-call-out nf-pseudo-to-hnf-mod "nfhnfmod" (nf x detmat))
;; GEN nfsnf(GEN nf, GEN x);
(pari-call-out nf-smith-normal-form "nfsnf" (nf x))

;; GEN nfdiveuc(GEN nf, GEN a, GEN b);
(pari-call-out nf-element-euclidean-divide "nfdiveuc" (nf a b))
;; GEN nfdivrem(GEN nf, GEN a, GEN b);
(pari-call-out nf-element-euclidean-divmod "nfdivrem" (nf a b))
;; GEN nfmod(GEN nf, GEN a, GEN b);
(pari-call-out nf-element-mod "nfmod" (nf a b))
;; GEN nfdiv(GEN nf, GEN x, GEN y);
(pari-call-out nf-element-divide "nfdiv" (nf x y) "nfdiv")
;; GEN nfinv(GEN nf, GEN x);
(pari-call-out nf-element-inverse "nfinv" (nf x) "nfinv")

;; GEN nfdetint(GEN nf,GEN pseudo);
(pari-call-out nf-determinant-multiple "nfdetint" (nf pseudo))

;; GEN nfreduce(GEN nf, GEN x, GEN ideal);
(pari-call-out nf-element-mod-ideal "nfreduce" (nf x ideal) "nfreduce")

;; GEN checknf(GEN nf);
;; GEN differente(GEN nf, GEN premiers);

;; long idealval(GEN nf,GEN ix,GEN vp);
(pari-call-out (ideal-valuation long) "idealval" (nf ix vp))
;; GEN isideal(GEN nf,GEN x);
(pari-call-out (nf-ideal? boolean) "isideal" (nf x))

;; long nfval(GEN nf, GEN x, GEN vp);
(pari-call-out (nf-element-valuation long) "nfval" (nf x vp) "nfval")

;; long    rnfisfree(GEN bnf, GEN order);
(pari-call-out (rnf-free? boolean) "rnfisfree" (bnf order))

;; GEN allbase4(GEN f, long code, GEN *y, GEN *ptw);
;; GEN base2(GEN x, GEN *y);
;; GEN rnfround2all(GEN nf, GEN pol, long all);
;; GEN rnfpseudobasis(GEN nf, GEN pol);
(pari-call-out rnf-pseudobasis "rnfpseudobasis" (bnf order))
;; GEN rnfdiscf(GEN nf, GEN pol);
(pari-call-out rnf-field-discriminant "rnfdiscf" (nf pol))
;; GEN rnfsimplifybasis(GEN bnf, GEN order);
;; GEN rnfsteinitz(GEN nf, GEN order);
(pari-call-out rnf-steinitz-class "rnfsteinitz" (nf order))
;; GEN rnfbasis(GEN bnf, GEN order);
(pari-call-out rnf-basis "rnfbasis" (bnf order))
;; GEN rnfhnfbasis(GEN bnf, GEN order);
(pari-call-out rnf-hermite-basis "rnfhnfbasis" (bnf order))

;; GEN bsrch(GEN p, GEN fa, long Ka, GEN eta, long Ma);
;; GEN setup(GEN p,GEN f,GEN theta,GEN nut);
;; GEN eleval(GEN f,GEN h,GEN a);
;; GEN vstar(GEN p,GEN h);
;; GEN factcp(GEN p,GEN f,GEN beta);
;; GEN bestnu(GEN w);
;; GEN gcdpm(GEN f1,GEN f2,GEN pm);

;; GEN compositum(GEN pol1, GEN pol2);
(pari-call-out nf-compositum "compositum" (pol1 pol2))

;; GEN nfreducemodpr(GEN nf, GEN x, GEN prhall);
;; GEN element_divmodpr(GEN nf, GEN x, GEN y, GEN prhall);
;; GEN element_powmodpr(GEN nf, GEN x, GEN k, GEN prhall);
;; #define element_mulmodpr(nf,x,y,prhall) (nfreducemodpr(nf,element_mul(nf,x,y);
;; GEN prhall))
;; #define element_sqrmodpr(nf,x,prhall) (nfreducemodpr(nf,element_sqr(nf,x);
;; GEN prhall))

;;; /* base1.c */

;; GEN tayl(GEN x, long v, long precdl);
(pari-call-out taylor-expansion "tayl"
  (x (varno long :in :none (get-varno x))
     (precdl long :in :none pari-series-precision))
  "taylor")
;; GEN pollegendre(long n, long v);
(pari-call-out legendre-polynomial "pollegendre"
  ((n long) (varno long :in :none 0)))
;; GEN polchebyshev(long n, long kind, long v);
(pari-call-out chebyshev-polynomial "polchebyshev"
  ((n long) (kind long :in :none 1) (varno long :in :none 0)))
;; GEN mathilbert(long n);
(pari-call-out hilbert-matrix "mathilbert" ((n long)) "mathilbert")
;; GEN matqpascal(long n, GEN q);
(pari-call-out pascal-triangle "matqpascal" ((n long) q) "matqpascal")
;; GEN laplace(GEN x);
(pari-call-out laplace-transform "laplace" (x))

;; GEN gprec(GEN x, long l);
(pari-call-out change-precision "gprec" (x (l long)) "precision")
;; GEN convol(GEN x, GEN y);
(pari-call-out hadamard-product "convol" (x y))
;; GEN ggrando(GEN x, long n);
(pari-call-out pari-o "ggrando" (a (b long)) "O")
;; GEN gconvsp(GEN x);
;; GEN gconvpe(GEN x);

;; GEN lll(GEN x, long prec);
(pari-call-out-prec matrix-lll-reduce "lll" (x))
;; GEN lll1(GEN x, long prec);
;; GEN lllrat(GEN x);
;; GEN lllgram(GEN x, long prec);
(pari-call-out-prec gram-matrix-lll-reduce "lllgram" (x))
;; GEN lllgram1(GEN x, long prec);
;; GEN lllgramint(GEN x);
(pari-call-out gram-matrix-lll-reduce-integral "lllgramint" (x))
;; GEN lllint(GEN x);
(pari-call-out matrix-lll-reduce-integral "lllint" (x))
;; GEN lllintpartial(GEN mat);
(pari-call-out matrix-partial-lll-reduce-integral "lllintpartial" (mat))
;; GEN lllintpartialall(GEN mat, long all);

;; GEN lllgramkerim(GEN x);
(pari-call-out gram-matrix-lll-reduce-kernel-and-image "lllgramkerim" (x))
;; GEN lllkerim(GEN x);
(pari-call-out matrix-lll-reduce-kernel-and-image "lllkerim" (x))
;; GEN lllgramall(GEN x, long all);
;; GEN lllall0(GEN x, long all);

;; GEN lllgen(GEN x);
;; GEN lllkerimgen(GEN x);
;; GEN lllgramgen(GEN x);
;; GEN lllgramkerimgen(GEN x);
;; GEN lllgramallgen(GEN x, long all);

;; GEN binomial(GEN x, long k);
(pari-call-out binomial-coefficient "binomial" (x (k long)))
;; GEN gscal(GEN x, GEN y);
;; GEN polcyclo(long n);
(pari-call-out cyclotomic-polynomial "polcyclo"
  ((n long) (varno long :in :none (get-varno n))) "polcyclo")

;; GEN lindep(GEN x, long prec);
(pari-call-out-prec vector-find-linear-dependence "lindep" (x))
;; GEN lindep2(GEN x, long bit);
;; GEN lindep2bis(GEN x, long bit, long prec);

;; GEN algdep(GEN x, long n, long prec);
(pari-call-out-prec find-algebraic-dependence "algdep" (x (n long)))
;; GEN algdep2(GEN x, long n, long bit);
;; GEN gsubstvec(GEN x, GEN v, GEN y);
(pari-call-out change-variables "gsubstvec" (x v y))
;; GEN polredbest(GEN x, long flag);
(pari-call-out polynomial-reduction "polredbest" (x (flag long)))

;; GEN polrecip(GEN x);
(pari-call-out reciprocal-polynomial "polrecip" (x) "recip")
;; GEN variables_vecsmall(GEN x);
(pari-call-out variable-numbers "variables_vecsmall" (x))
;; GEN variables_vec(GEN x);
(pari-call-out variables "variables_vec" (x))

;; GEN vecsort0(GEN x, GEN k, long flag);
;; Flag:
;; 1: indirect sorting, return the permutation instead of the permuted vector
;; 2: use lexcmp instead of gcmp
;; 4: use descending instead of ascending order
;; 8: remove duplicate entries
(pari-call-out vector-sort "vecsort0"
  (x (cmpf pari-gen :in :none nil) (flag long :in :none 0)) "vecsort")

;; GEN stirling(long n, long m, long flag);
(pari-call-out stirling "stirling" ((n long) (m long) (flag long :in :none 1)))

;; GEN polsym(GEN x, long n);
(pari-call-out symmetric-powers "polsym" (x (n long)))

;; GEN minim(GEN a, long borne, long stockmax);
;; GEN minimprim(GEN a, long borne, long stockmax);
(pari-call-out symmetric-matrix-minimal-vectors "minim" (a (b long) (m long)))

;; GEN polred(GEN x, long prec);
(pari-call-out-prec nf-poly-reduce "polred" (x))
;; GEN factoredpolred(GEN x, GEN p, long prec);
(pari-call-out-prec nf-poly-reduce-factored "factoredpolred" (x p))
;; GEN smallpolred(GEN x, long prec);
(pari-call-out-prec nf-poly-reduce-small "smallpolred" (x))
;; GEN polred2(GEN x, long prec);
(pari-call-out-prec nf-poly-reduce-2 "polred2" (x))
;; GEN factoredpolred2(GEN x, GEN p, long prec);
(pari-call-out-prec nf-poly-reduce-2-factored "factoredpolred2" (x p))
;; GEN polredabs(GEN x, long prec);
(pari-call-out-prec nf-poly-reduce-abs "polredabs" (x))

;; GEN smallpolred2(GEN x, long prec);
(pari-call-out-prec nf-poly-reduce-2-small "smallpolred2" (x))
;; GEN allpolred(GEN x, GEN *pta, long code, long prec);
;; GEN polymodrecip(GEN x);
(pari-call-out polymod-reverse "polymodrecip" (x) "modreverse")
;; GEN genrand(GEN x);
(pari-call-out pari-random "genrand" (x) "random")
;; GEN numtoperm(long n, GEN x);
(pari-call-out permutation "numtoperm" ((n long) x))
;; GEN permtonum(GEN x);
(pari-call-out permutation-number "permtonum" (x))

;; long setprecr(long n);
;(pari-call-out (set-real-precision long) "setprecr" ((n long)) "setprecision")
;; GEN setserieslength(long n);
;(pari-call-out (set-series-precision long) "setserieslength" ((n long)))

;; void setrand(GEN seed);
(pari-call-out (set-random-seed nil) "setrand" (seed))
;; GEN getrand(void);
(pari-call-out get-random-seed "getrand" ())
;; GEN randomi(GEN x);
(pari-call-out get-random-int "randomi" (x))
;; GEN randomr(long prec);
(pari-call-out-prec get-random-real "randomr" ())

;; long getstack(void);
(pari-call-out (getstack long) "getstack" ())
;; long gettime(void);
(pari-call-out (gettime long) "gettime" ())
;; GEN getheap(void);
(pari-call-out getheap "getheap" ())

;;; /* bibli2.c */

;; GEN somme(entree *ep, GEN x, GEN a, GEN b, char *ch);
;; GEN produit(entree *ep, GEN x, GEN a, GEN b, char *ch);
;; GEN suminf(entree *ep, GEN a, char *ch, long prec);
;; GEN prodinf(entree *ep, GEN a, char *ch, long prec);
;; GEN prodinf1(entree *ep, GEN a, char *ch, long prec);
;; GEN prodeuler(entree *ep, GEN a, GEN b, char *ch, long prec);

;; GEN vecteur(entree *ep, GEN nmax, char *ch);
;; GEN vvecteur(entree *ep, GEN nmax, char *ch);
;; GEN matrice(entree *ep1, entree *ep2, GEN nlig, GEN ncol, char *ch);
;; GEN divsomme(entree *ep, GEN num, char *ch);

;; GEN qromb(entree *ep, GEN a, GEN b, char *ch, long prec);
;; GEN qromo(entree *ep, GEN a, GEN b, char *ch, long prec);
;; GEN qromi(entree *ep, GEN a, GEN b, char *ch, long prec);
;; GEN rombint(entree *ep, GEN a, GEN b, char *ch, long prec);

;; GEN polint(GEN xa, GEN ya, GEN x, GEN *dy);
(pari-call-out interpolating-polynomial-value "polint"
  (xa ya x (dy (c-ptr pari-gen) :out :alloca)))
;; GEN plot(entree *ep, GEN a, GEN b, char *ch);
;; GEN ploth(entree *ep, GEN a, GEN b, char *ch, long prec);
;; GEN ploth2(entree *ep, GEN a, GEN b, char *ch, long prec);
;; GEN plothraw(GEN listx, GEN listy);
;; GEN zbrent(entree *ep, GEN a, GEN b, char *ch, long prec);

;; GEN sumalt(entree *ep, GEN a, char *ch, long prec);
;; GEN sumalt1(entree *ep, GEN a, char *ch, long prec);
;; GEN sumalt2(entree *ep, GEN a, char *ch, long prec);
;; GEN sumalt3(entree *ep, GEN a, char *ch, long prec);
;; GEN sumpos(entree *ep, GEN a, char *ch, long prec);
;; GEN sumposold(entree *ep, GEN a, char *ch, long prec);

;; GEN forpari(entree *ep, GEN a, GEN b, char *ch);
;; GEN forstep(entree *ep, GEN a, GEN b, GEN s, char *ch);
;; GEN fordiv(entree *ep, GEN a, char *ch);
;; GEN forprime(entree *ep, GEN a, GEN b, char *ch);
;; GEN forvec(entree *ep, GEN x, char *ch);

;; GEN initrect(long ne, long x, long y);
;; GEN killrect(long ne);
;; GEN rectcursor(long ne);
;; GEN rectmove(long ne, GEN x, GEN y);
;; GEN rectrmove(long ne, GEN x, GEN y);
;; GEN rectpoint(long ne, GEN x, GEN y);

;; GEN rectrpoint(long ne, GEN x, GEN y);
;; GEN rectbox(long ne, GEN gx2, GEN gy2);
;; GEN rectrbox(long ne, GEN gx2, GEN gy2);
;; GEN rectline(long ne, GEN gx2, GEN gy2);
;; GEN rectrline(long ne, GEN gx2, GEN gy2);
;; GEN rectdraw(GEN list);

;; GEN rectpoints(long ne, GEN listx, GEN listy);
;; GEN rectlines(long ne, GEN listx, GEN listy);
;; GEN rectstring(long ne, GEN x);
;; GEN rectscale(long ne, GEN x1, GEN x2, GEN y1, GEN y2);

;; GEN postdraw(GEN list);
;; GEN postploth(entree *ep, GEN a, GEN b, char *ch);
;; GEN postploth2(entree *ep, GEN a, GEN b, char *ch);
;; GEN postplothraw(GEN listx, GEN listy);

;; GEN gtoset(GEN x);
(pari-call-out convert-to-set "gtoset" (x) "set")
;; GEN setunion(GEN x, GEN y);
(pari-call-out pari-set-union "setunion" (x y))
;; GEN setintersect(GEN x, GEN y);
(pari-call-out pari-set-intersection "setintersect" (x y))
;; GEN setminus(GEN x, GEN y);
(pari-call-out pari-set-difference "setminus" (x y))

;; GEN dirmul(GEN x, GEN y);
(pari-call-out dirichlet-multiply "dirmul" (x y))
;; GEN dirdiv(GEN x, GEN y);
(pari-call-out dirichlet-divide "dirdiv" (x y))
;; GEN dirzetak(GEN nf, GEN b);
(pari-call-out nf-dirchlet-zeta-series "dirzetak" (nf b))

;; long isvecset(GEN x);
;; GEN setsearch(GEN x, GEN y);
(pari-call-out (pari-set-position long) "setsearch" (x y))
;;(pari-call-out (set? boolean) "isvecset" (x) "isset")

;;; /* buch1.c et buch2.c */

;; GEN Buchall(GEN P, long flag, long prec);
(pari-call-out nf-buchall "Buchall" (p (flag long)) "Buchall")

;; GEN isprincipal(GEN bignf, GEN x);
(pari-call-out ideal-class "isprincipal" (bignf x))
;; GEN isprincipalgen(GEN bignf, GEN x);
(pari-call-out ideal-class-more "isprincipalgen" (bignf x))

;; GEN isunit(GEN bignf, GEN x);
(pari-call-out nf-unit-in-basis "isunit" (bnf x))
;; GEN signunits(GEN bignf);
(pari-call-out nf-unit-signs "signunits" (bignf))
;; GEN buchnarrow(GEN bignf);
(pari-call-out nf-buchnarrow "buchnarrow" (bignf))

;; int compte(long **mat, long row, long longueur, long *firstnonzero);
;; int compte2(long **mat, long row, long longueur, long *firstnonzero);

;;; /* elliptic.c */

;; GEN ghell(GEN e, GEN a, long prec);
(pari-call-out-prec ell-height "ghell" (e a) "hell")
;; GEN ghell2(GEN e, GEN a, long prec);
;; GEN ghell3(GEN e, GEN a, long prec);

;; GEN ellinit(GEN x, long prec);
(pari-call-out-prec ell-init "ellinit" (x))
;; GEN zell(GEN e, GEN z, long prec);
(pari-call-out-prec ell-xy-to-z "zell" (e z))
;; GEN ellchangecurve(GEN e, GEN ch);
(pari-call-out ell-change-coordinates "ellchangecurve" (e ch) "ellchangecurve")
;; GEN ellchangepoint(GEN x, GEN ch);
(pari-call-out ell-change-point-coordinates "ellchangepoint" (x ch) "ellchangepoint")

;; GEN addell(GEN e, GEN z1, GEN z2);
(pari-call-out ell-add "addell" (e z1 z2))
;; GEN subell(GEN e, GEN z1, GEN z2);
(pari-call-out ell-subtract "subell" (e z1 z2))
;; GEN powell(GEN e, GEN z, GEN n);
(pari-call-out ell-multiply "powell" (e z n))

;; GEN mathell(GEN e, GEN x, long prec);
(pari-call-out-prec ell-height-pairing-gram-matrix "mathell" (e x))
;; GEN bilhell(GEN e, GEN z1, GEN z2, long prec);
(pari-call-out-prec ell-height-pairing "bilhell" (e z1 z2))

;; GEN ellordinate(GEN e, GEN x, long prec);
(pari-call-out-prec ell-y-coordinates "ellordinate" (e x))
;; GEN ellap(GEN e, GEN pl);
(pari-call-out ell-l-series-p "ellap" (e p))

;; GEN anell(GEN e, long n);
(pari-call-out ell-l-series "anell" (e (n long)))
;; GEN akell(GEN e, GEN n);
(pari-call-out ell-l-series-n "akell" (e n))

;; GEN elllocalred(GEN e, GEN p1);
(pari-call-out ell-local-reduction "elllocalred" (e p1))
;; GEN ellglobalred(GEN e1);
(pari-call-out ell-global-reduction "ellglobalred" (e1))

;; GEN elllseries(GEN e, GEN s, GEN N, GEN A, long prec);
(pari-call-out-prec ell-l-series-value "elllseries" (e s N A))

;; GEN pointell(GEN e, GEN z, long prec);
(pari-call-out-prec ell-z-to-xy "pointell" (e z))
;; GEN elltaniyama(GEN e, long prec);
(pari-call-out-prec ell-modular-parametrization "elltaniyama" (e))

;; GEN orderell(GEN e, GEN p);
(pari-call-out ell-order "orderell" (e p))
;; GEN elltors(GEN e);
(pari-call-out ell-torsion-group "elltors" (e))

;; int oncurve(GEN e, GEN z);
(pari-call-out (ell-on-curve? boolean) "oncurve" (e z) "isoncurve")

;; void eulsum(GEN *sum, GEN term, long jterm, GEN *tab, long *dsum, long prec);

;;; /* es.c */

;; void filtre(char *s);
;; GEN pariputc(char c);
;; GEN pariputs(char *s);
;; GEN ecrire(GEN x, char format, long dec, long chmp);
;; GEN voir(GEN x, long nb);
;; GEN sor(GEN g, char fo, long dd, long chmp);
;; void    brute(GEN g, char format, long dec);
;; GEN matbrute(GEN g, char format, long dec);
;; GEN texe(GEN g, char format, long dec);
;; GEN etatpile(unsigned int n);
;; void    outerr(GEN x);
;; GEN bruterr(GEN x,char format,long dec);
;; GEN outbeauterr(GEN x);
;; void bruteall(GEN g, char format, long dec, long flbl);

;; #ifdef CLISP_MODULE
;; void pariflush(void);
;; #endif /* CLISP_MODULE */

;; char* GENtostr(GEN x); --- uses f_PRETTYMAT, cannot be read back
(pari-call-out (write-to-string-pretty c-string :malloc-free) "GENtostr" (x) nil)
;; char* GENtostr_raw(GEN x);
(pari-call-out (write-to-string c-string :none) "GENtostr_raw" (x) nil)
;; char* GENtoTeXstr(GEN x);
(pari-call-out (write-to-string-TeX c-string :malloc-free) "GENtoTeXstr" (x) nil)

;; void fprintferr(char* pat, ...);
;; void flusherr();

;; char *gitoascii(GEN g, char *buf);

;; void printvargp(long);
;; extern void (*printvariable)(long);

;; long timer(void);
;; GEN timer2(void);

;;; /* gen1.c */

;; GEN gadd(GEN x, GEN y);
(pari-call-out pari+ "gadd" (x y) "+")
;; GEN gsub(GEN x, GEN y);
(pari-call-out pari- "gsub" (x y) "-")
;; GEN gmul(GEN x, GEN y);
(pari-call-out pari* "gmul" (x y) "*")
;; GEN gdiv(GEN x, GEN y);
(pari-call-out pari/ "gdiv" (x y) "/")

;;; /* gen2.c gen3.c */

;; GEN gcopy(GEN x);
;(pari-call-out copy "gcopy" (x) nil)
;; GEN gcopy(GEN x);
;; GEN gclone(GEN x);
;; GEN cgetp(GEN x);
;; GEN gaddpex(GEN x, GEN y);

;; GEN greffe(GEN x, long l);
;; GEN gopsg2(GEN (*f) (GEN, GEN), long s, GEN y);
;; GEN gopgs2(GEN (*f) (GEN, GEN), GEN y, long s);
;; GEN co8(GEN x, long l);
;; GEN cvtop(GEN x, GEN p, long l);
;; GEN compo(GEN x, long n);
(pari-call-out component "compo" (x (n long)))
;; GEN gsqr(GEN x);
(pari-call-out square "gsqr" (x) "sqr")

;; GEN gneg(GEN x);
(pari-call-out pari-minus "gneg" (x) "-")
;; GEN gabs(GEN x, long prec);
(pari-call-out-prec pari-abs "gabs" (x) "abs")

;; GEN gpow(GEN x, GEN n, long prec);
(pari-call-out-prec pari-expt "gpow" (x n) "^")
;; GEN gpowgs(GEN x, long n);
(pari-call-out pari-expt-integer "gpowgs" (x (n long)) "^")

;; GEN gmax(GEN x, GEN y);
(pari-call-out pari-max "gmax" (x y) "max")
;; GEN gmin(GEN x, GEN y);
(pari-call-out pari-min "gmin" (x y) "min")
;; GEN ginv(GEN x);
(pari-call-out invert "ginv" (x) "^(-1)")
;; GEN denom(GEN x);
(pari-call-out pari-denominator "denom" (x))
;; GEN numer(GEN x);
(pari-call-out pari-numerator "numer" (x))
;; GEN lift(GEN x);
(pari-call-out lift "lift" (x))
;; GEN centerlift(GEN x);
(pari-call-out centerlift "centerlift" (x))
;; GEN vecmax(GEN x);
(pari-call-out vector-max "vecmax" (x))
;; GEN vecmin(GEN x);
(pari-call-out vector-min "vecmin" (x))

;; GEN gmulsg(long s, GEN y);
;; GEN gdivgs(GEN x, long s);
;; GEN gmodulo(GEN x, GEN y);
(pari-call-out make-mod "gmodulo" (x y) "mod")
;; GEN simplify(GEN x);
(pari-call-out simplify "simplify" (x))

;; GEN gmod(GEN x, GEN y);
(pari-call-out pari-mod "gmod" (x y) "%")
;; GEN gshift(GEN x, long n);
(pari-call-out pari-ash "gshift" (x (n long)) "shift")
;; GEN gmul2n(GEN x, long n);
(pari-call-out multiply-by-2^n "gmul2n" (x (n long)) "shiftmul")

;; GEN gsubst(GEN x, long v, GEN y);
(pari-call-out pari-substitute "gsubst" (x (varno long) y) "subst")
;; GEN deriv(GEN x, long v);
(pari-call-out derivative "deriv" (x (varno long :in :none (get-varno x))))
;; GEN integ(GEN x, long v);
(pari-call-out integral "integ" (x (varno long :in :none (get-varno x))))
;; GEN recip(GEN x);
(pari-call-out pws-reverse "recip" (x) "reverse")
;; GEN ground(GEN x);
(pari-call-out pari-round "ground" (x) "round")
;; GEN gcvtoi(GEN x, long *e);
;; GEN grndtoi(GEN x, long *e);

;; GEN gceil(GEN x);
(pari-call-out pari-ceiling "gceil" (x) "ceil")
;; GEN gfloor(GEN x);
(pari-call-out pari-floor "gfloor" (x) "floor")
;; GEN gfrac(GEN x);
(pari-call-out mod-1 "gfrac" (x) "frac")
;; GEN gtrunc(GEN x);
(pari-call-out pari-truncate "gtrunc" (x) "trunc")
;; GEN gdivent(GEN x, GEN y);
(pari-call-out quotient "gdivent" (x y) "\\")
;; GEN gdiventres(GEN x, GEN y);
(pari-call-out quotient-and-mod "gdiventres" (x y) "divres")

;; GEN gdivmod(GEN x, GEN y, GEN *pr);
;; GEN geval(GEN x);
;; GEN glt(GEN x, GEN y);
(pari-call-out (pari< pari-bool) "glt" (x y) "<")
;; GEN gle(GEN x, GEN y);
(pari-call-out (pari<= pari-bool) "gle" (x y) "<=")
;; GEN ggt(GEN x, GEN y);
(pari-call-out (pari> pari-bool) "ggt" (x y) ">")
;; GEN gge(GEN x, GEN y);
(pari-call-out (pari>= pari-bool) "gge" (x y) ">=")
;; GEN geq(GEN x, GEN y);
(pari-call-out (pari= pari-bool) "geq" (x y) "==")
;; GEN gne(GEN x, GEN y);
(pari-call-out (pari/= pari-bool) "gne" (x y) "!=")

;; GEN glength(GEN x);
(pari-call-out pari-length "glength" (x) "length")
;; GEN matsize(GEN x);
(pari-call-out matrix-size "matsize" (x))
;; GEN truecoeff(GEN x, long n);
(pari-call-out coefficient "truecoeff" (x (n long)) "coeff")
;; GEN gtype(GEN x);
;; GEN gsettype(GEN x,long t);

;; GEN gtopoly(GEN x, long v);
(pari-call-out convert-to-polynomial-reverse "gtopoly"
  (x (varno long :in :none (get-varno x))) "poly")
;; GEN gtopolyrev(GEN x, long v);
(pari-call-out convert-to-polynomial "gtopolyrev"
  (x (varno long :in :none (get-varno x))) "polyrev")
;; GEN gtoser(GEN x, long v);
(pari-call-out convert-to-pws "gtoser"
  (x (varno long :in :none (get-varno x))) "series")
;; GEN gtovec(GEN x);
(pari-call-out convert-to-vector "gtovec" (x) "vec")
;; GEN dbltor(double x);
;; Be consistent and take 'constant term first' as the normal thing ...

;; GEN karamul(GEN x, GEN y, long k);
;; GEN mpkaramul(GEN x, GEN y, long k);

;; GEN gdivround(GEN x, GEN y);
(pari-call-out pari-round2 "gdivround" (x y) "\\/")
;; GEN gpolvar(GEN y);

;; void    gop0z(GEN (*f) (void), GEN x);
;; GEN gop1z(GEN (*f) (GEN), GEN x, GEN y);
;; GEN gop2z(GEN (*f) (GEN, GEN), GEN x, GEN y, GEN z);
;; GEN gops2gsz(GEN (*f) (GEN, long), GEN x, long s, GEN z);
;; GEN gops2sgz(GEN (*f) (long, GEN), long s, GEN y, GEN z);
;; GEN gops2ssz(GEN (*f) (long, long), long s, long y, GEN z);
;; void    gop3z(GEN (*f) (GEN, GEN, GEN), GEN x, GEN y, GEN z, GEN t);
;; GEN gops1z(GEN (*f) (long), long s, GEN y);
;; GEN gopsg2z(GEN (*f) (GEN, GEN), long s, GEN y, GEN z);
;; GEN gopgs2z(GEN (*f) (GEN, GEN), GEN y, long s, GEN z);
;; GEN gaffsg(long s, GEN x);
;; GEN gaffect(GEN x, GEN y);

;; void    normalize(GEN *px);
;; GEN normalizepol(GEN *px);

;; int gcmp0(GEN x);
(pari-call-out (zero? boolean) "gcmp0" (x) "?")
;; GEN gcmp1(GEN x);
(pari-call-out (one? boolean) "gcmp1" (x) "?")
;; GEN gcmp_1(GEN x);
(pari-call-out (minus-one? boolean) "gcmp_1" (x) "?")
;; int gcmp(GEN x, GEN y);
(pari-call-out (compare boolean) "gcmp" (x y) "?")
;; int lexcmp(GEN x, GEN y);
(pari-call-out (compare-lex boolean) "lexcmp" (x y) "lex")
;; GEN gequal(GEN x, GEN y);
(pari-call-out (equal? boolean) "gequal" (x y) "==")
;; GEN polegal(GEN x, GEN y);
;; GEN vecegal(GEN x, GEN y);
;; int gsigne(GEN x);
(pari-call-out (pari-sign int) "gsigne" (x) "sign")

;; long gvar(GEN x);
(pari-call-out (varno long) "gvar" (x) "?")
;; long gvar2(GEN x);
;; long precision(GEN x);
(pari-call-out (precision long) "precision" (x))
;; GEN gprecision(GEN x);
;; GEN ismonome(GEN x);
;; GEN iscomplex(GEN x);
;; int isexactzero(GEN g);
(pari-call-out (eql-0? boolean) "isexactzero" (g))
;; INLINE int is_bigint(GEN n);
(pari-call-out (bigint? boolean) "is_bigint" (n))

(defun get-varno (x)
  (let ((vn (%varno (convert-to-pari x))))
    (if (bigint? vn) 0 vn)))

;; long padicprec(GEN x, GEN p);
(pari-call-out (get-padic-precision long) "padicprec" (x p))

;; long opgs2(int (*f) (GEN, GEN), GEN y, long s);

;; long gsizeword(GEN x);
(pari-call-out (sizeword long) "gsizeword" (x) "?")
;; long gsizebyte(GEN x);
(pari-call-out (sizebyte long) "gsizebyte" (x) "sizebyte")
;; GEN gexpo(GEN x);
;; GEN gtolong(GEN x);
;; GEN ggval(GEN x, GEN p);
(pari-call-out (valuation long) "ggval" (x p) "valuation")
;; GEN rounderror(GEN x);
;;(pari-call-out (rounderror long) "rounderror" (x))
;; GEN gsize(GEN x);
;;(pari-call-out (size long) "gsize" (x) "size")
;; GEN Z_pvalrem(GEN x, GEN p, GEN *py);

;; double  rtodbl(GEN x);
;; GEN gtodouble(GEN x);

;;; /* polarit.c */
;; GEN ginvmod(GEN x, GEN y);
;; GEN gcopy(GEN x);
;; GEN gdeuc(GEN x, GEN y);
;; GEN grem(GEN x, GEN y);
;; GEN poldivrem(GEN x, GEN y, GEN *pr);

;; GEN poleval(GEN x, GEN y);
;; GEN roots(GEN x, long l);
(pari-call-out-prec complex-roots "roots" (x))
;; GEN roots2(GEN pol,long PREC);
;; GEN rootslong(GEN x, long l);
;;(pari-call-out-prec complex-roots-robust "rootslong" (x))
;; GEN ggcd(GEN x, GEN y);
(pari-call-out pari-gcd "ggcd" (x y) "gcd")
;; GEN gbezout(GEN x, GEN y, GEN *u, GEN *v);
;; GEN vecbezout(GEN x, GEN y);
(pari-call-out pari-xgcd "vecbezout" (x y) "bezout")
;; GEN glcm(GEN x, GEN y);
(pari-call-out pari-lcm "glcm" (x y) "lcm")

;; GEN subresext(GEN x, GEN y, GEN *U, GEN *V);
;; (pari-call-out xresultant "subresext"
;;   (x y (u (c-ptr pari-gen) :out :alloca) (v (c-ptr pari-gen) :out :alloca)))
;; GEN vecbezoutres(GEN x, GEN y);
(pari-call-out resultant-vector "vecbezoutres" (x y))

;; GEN polgcd(GEN x, GEN y);
;; GEN srgcd(GEN x, GEN y);
;; GEN polgcdnun(GEN x, GEN y);
;; GEN content(GEN x);
(pari-call-out content "content" (x))
;; GEN primpart(GEN x);
(pari-call-out primpart "primpart" (x))
;; GEN primitive_part (GEN x, GEN *c);
(pari-call-out primitive-part "primitive_part"
  (x (c (c-ptr pari-gen) :out :alloca)))
;; GEN psres(GEN x, GEN y);
;; GEN factorff(GEN f, GEN p, GEN a);
(pari-call-out factor-in-fq "factorff" (f p a))

;; GEN factmod(GEN f, GEN p);
(pari-call-out factor-in-fp "factmod" (f p))
;; GEN rootmod(GEN f, GEN p);
(pari-call-out mod-p-roots "rootmod" (f p))
;; GEN rootmod2(GEN f, GEN p);
(pari-call-out mod-p-roots-small "rootmod2" (f p))
;; GEN decpol(GEN x, long klim);
;; GEN factor(GEN x);
(pari-call-out factor "factor" (x))
;; long isirreducible(GEN x);
(pari-call-out (irreducible? boolean) "isirreducible" (x))

;; GEN simplefactmod(GEN f, GEN p);
(pari-call-out factor-degrees "simplefactmod" (f p))
;; GEN factcantor(GEN x, GEN p);
(pari-call-out factor-cantor-zassenhaus "factcantor" (x p))

;; GEN poldisc0(GEN x, long v);
(pari-call-out discriminant "poldisc0"
  (x (varno long :in :none (get-varno x))) "poldisc")
;; GEN resultant(GEN x, GEN y);
(pari-call-out resultant "resultant" (x y) "resultant")
;; GEN quadpoly(GEN x);
(pari-call-out quad-minimal-polynomial "quadpoly" (x))
;; GEN quadgen(GEN x);
(pari-call-out make-quad "quadgen" (x))
;; GEN quaddisc(GEN x);
(pari-call-out quad-discriminant "quaddisc" (x))
;; GEN bezoutpol(GEN a, GEN b, GEN *u, GEN *v);
;; GEN polinvmod(GEN x, GEN y);

;; GEN resultant2(GEN x, GEN y);
(pari-call-out resultant-sylvester "resultant2" (x y))
;; GEN sylvestermatrix(GEN x,GEN y);
(pari-call-out sylvester-matrix "sylvestermatrix" (x y))
;; GEN polfnf(GEN a, GEN t);
(pari-call-out nf-factor "polfnf" (a b) "factornf")
;; GEN nfiso(GEN a, GEN b);
;(pari-call-out nf-field-isomorphic? "nfiso" (a b) "isisom")
;; GEN nfincl(GEN a, GEN b);
;(pari-call-out nf-field-inclusion? "nfincl" (a b) "isincl")
;; GEN isisomfast(GEN nf1, GEN nf2, long prec);
;; GEN isinclfast(GEN nf1, GEN nf2, long prec);

;; GEN newtonpoly(GEN x, GEN p);
(pari-call-out newton-polygon "newtonpoly" (x p))
;; GEN padicappr(GEN f, GEN a);
(pari-call-out lift-padic-roots "padicappr" (f a))
;; GEN rootpadic(GEN f, GEN p, long r);
(pari-call-out padic-roots "rootpadic" (f p (prec long)))
;; GEN rootpadicfast(GEN f, GEN p, long r, long flall);
;; GEN gcvtop(GEN x, GEN p, long r);
;; GEN factorpadic2(GEN x, GEN p, long r);

;; GEN factorpadic4(GEN x, GEN p, long r);
(pari-call-out factor-padic "factorpadic4"
  (x p (prec long)) "factorpadic")
;; GEN nilordpadic(GEN p,long r,GEN fx,long mf,GEN gx);
;; GEN Decomppadic(GEN p,long r,GEN f,long mf,GEN theta,GEN chi,GEN nu);

;; long sturmpart(GEN x, GEN a, GEN b);
(pari-call-out (count-real-roots-between long) "sturmpart" (x a b))
(defun count-real-roots (x) (count-real-roots-between x nil nil))
(export 'count-real-roots)

;; int poldivis(GEN x, GEN y, GEN *z);
;; GEN gdvd(GEN x, GEN y);

;; void    gredsp(GEN *px);
;; GEN split(long m, GEN *t, long d, long p, GEN q);
;; GEN split9(GEN m, GEN *t, long d, long p, GEN q, GEN unfq, GEN qq, GEN a);
;; GEN splitgen(GEN m, GEN *t,long d,GEN p, GEN q);

;; int issimplefield(GEN x);
;; GEN isinexactfield(GEN x);

;;; /* trans1.c */

;; GEN greal(GEN x);
(pari-call-out pari-realpart "greal" (x) "real")
;; GEN gimag(GEN x);
(pari-call-out pari-imagpart "gimag" (x) "imag")
;; GEN teich(GEN x);
(pari-call-out-prec teichmueller "teich" (x))
;; GEN agm(GEN x, GEN y, long prec);
(pari-call-out-prec arithmetic-geometric-mean "agm" (x y))
;; GEN palog(GEN x);

;; GEN sqrtr(GEN x);
;; GEN gsqrt(GEN x, long prec);
(pari-call-out-prec pari-sqrt "gsqrt" (x) "sqrt")

;; GEN mpeuler(long prec);
(pari-call-out-prec pari-euler "mpeuler" () "euler")

;; GEN mpcatalan(long prec);
(pari-call-out-prec pari-catalan "mpcatalan" () "catalan")

;; GEN gexp(GEN x, long prec);
(pari-call-out-prec pari-exp "gexp" (x) "exp")

;; GEN mpexpm1(GEN x);
;; GEN mpexp(GEN x);

;; GEN mplog(GEN x);
;; GEN glog(GEN x, long prec);
(pari-call-out-prec pari-log "glog" (x) "log")

;; GEN mpsc1(GEN x, long *ptmod8);
;; GEN mpcos(GEN x);
;; GEN gcos(GEN x, long prec);
(pari-call-out-prec pari-cos "gcos" (x) "cos")
;; GEN mpsin(GEN x);
;; GEN gsin(GEN x, long prec);
(pari-call-out-prec pari-sin "gsin" (x) "sin")

;; GEN mpaut(GEN x);
;; GEN mptan(GEN x);
;; GEN gtan(GEN x, long prec);
(pari-call-out-prec pari-tan "gtan" (x) "tan")
;; GEN mpatan(GEN x);
;; GEN gatan(GEN x, long prec);
(pari-call-out-prec pari-arctan "gatan" (x) "atan")
;; GEN mpasin(GEN x);
;; GEN gasin(GEN x, long prec);
(pari-call-out-prec pari-arcsin "gasin" (x) "asin")

;; GEN mpacos(GEN x);
;; GEN gacos(GEN x, long prec);
(pari-call-out-prec pari-arccos "gacos" (x) "acos")
;; GEN mparg(GEN x, GEN y);
;; GEN mpch(GEN x);
;; GEN gch(GEN x, long prec);
(pari-call-out-prec pari-cosh "gch" (x) "cosh")
;; GEN mpsh(GEN x);
;; GEN gsh(GEN x, long prec);
(pari-call-out-prec pari-sinh "gsh" (x) "sinh")

;; GEN mpth(GEN x);
;; GEN gth(GEN x, long prec);
(pari-call-out-prec pari-tanh "gth" (x) "tanh")
;; GEN mpath(GEN x);
;; GEN gath(GEN x, long prec);
(pari-call-out-prec pari-artanh "gath" (x) "atanh")
;; GEN mpash(GEN x);
;; GEN gash(GEN x, long prec);
(pari-call-out-prec pari-arsinh "gash" (x) "asinh")

;; GEN garg(GEN x, long prec);
(pari-call-out-prec pari-argument "garg" (x) "arg")
;; GEN sarg(GEN x, GEN y, long prec);
;; GEN mppsi(GEN z);
;; GEN gpsi(GEN x, long prec);
(pari-call-out-prec psi "gpsi" (x) "psi")
;; GEN transc(GEN (*f) (GEN, long), GEN x, long prec);
;; GEN kbessel(GEN nu, GEN gx, long prec);
(pari-call-out-prec bessel-k "kbessel" (nu x))
;; GEN hyperu(GEN a, GEN b, GEN gx, long prec);
(pari-call-out-prec hypergeometric-u "hyperu" (a b x))

;; GEN cxpsi(GEN z, long prec);
;; GEN jbesselh(GEN n, GEN z, long prec);
(pari-call-out-prec bessel-j-half "jbesselh" (n z))
;; GEN gzeta(GEN x, long prec);
(pari-call-out-prec riemann-zeta "gzeta" (x) "zeta")

;; GEN kbessel2(GEN nu, GEN x, long prec);
;; GEN eint1(GEN x, long prec);
(pari-call-out-prec exponential-integral-1 "eint1" (x))
;; GEN gerfc(GEN x, long prec);
(pari-call-out-prec erfc "gerfc" (x))
;; GEN eta(GEN x, long prec);
(pari-call-out-prec dedekind-eta "eta" (x))
;; GEN jell(GEN x, long prec);
(pari-call-out-prec elliptic-j "jell" (x))
;; GEN weberf2(GEN x, long prec);
(pari-call-out-prec weber-f2 "weberf2" (x))
;; GEN weberf(GEN x, long prec);
(pari-call-out-prec weber-f "weberf" (x))

;; GEN incgam(GEN a, GEN x, long prec);
(pari-call-out-prec incomplete-gamma "incgam" (a x))
;; GEN incgam1(GEN a, GEN x, long prec);
;; GEN incgam2(GEN a, GEN x, long prec);
;; GEN incgam3(GEN a, GEN x, long prec);
;;(pari-call-out-prec complementary-incomplete-gamma "incgam3" (a x))
;; GEN incgam4(GEN a, GEN x, GEN z, long prec);
;; GEN bernreal(long n, long prec);
(pari-call-out-prec bernoulli-real "bernreal" ((n long)))
;; GEN bernvec(long nomb);
(pari-call-out bernoulli-vector "bernvec" ((nomb long)))

;; GEN mpach(GEN x);
;; GEN gach(GEN x, long prec);
(pari-call-out-prec pari-arcosh "gach" (x) "acosh")
;; GEN mpgamma(GEN x);
;; GEN cxgamma(GEN x, long prec);
;; GEN ggamma(GEN x, long prec);
(pari-call-out-prec gamma "ggamma" (x) "gamma")
;; GEN mpgamd(long x, long prec);
;; GEN ggamd(GEN x, long prec);
(pari-call-out-prec gamma-shift-1/2 "ggamd" (x) "gamh")
;; GEN mppi(long prec);
(pari-call-out-prec pari-pi "mppi" () "pi")

;; GEN mpeuler(long prec);
(pari-call-out-prec euler "mpeuler" () "euler")
;; GEN gpolylog(long m, GEN x, long prec);
(pari-call-out-prec polylog "gpolylog" ((m long) x) "polylog")
;; GEN dilog(GEN x, long prec);
(pari-call-out-prec dilog "dilog" (x))
;; GEN polylog0(long m, GEN x, long flag, long prec);
(pari-call-out-prec polylog0 "polylog0" ((m long) x (flag long)))

;; GEN theta(GEN q, GEN z, long prec);
;; GEN thetanullk(GEN q, long k, long prec);
;; GEN mplngamma(GEN x);
;; GEN cxlngamma(GEN x, long prec);
;; GEN glngamma(GEN x, long prec);
(pari-call-out-prec log-gamma "glngamma" (x) "lngamma")
;; GEN izeta(GEN x, long prec);

;; void constpi(long prec);
;(pari-call-out-prec (precompute-pi nil) "constpi" () "pi")
;; GEN consteuler(long prec);
;(pari-call-out-prec (precompute-euler nil) "consteuler" () "euler")
;; GEN mpbern(long nomb, long prec);
;; GEN gsincos(GEN x, GEN *s, GEN *c, long prec);

;; void gsqrtz(GEN x, GEN y);
;; GEN gexpz(GEN x, GEN y);
;; GEN glogz(GEN x, GEN y);
;; GEN gcosz(GEN x, GEN y);
;; GEN gsinz(GEN x, GEN y);
;; GEN mpsincos(GEN x, GEN *s, GEN *c);
;; GEN gtanz(GEN x, GEN y);

;; void gatanz(GEN x, GEN y);
;; GEN gasinz(GEN x, GEN y);
;; GEN gacosz(GEN x, GEN y);
;; GEN gchz(GEN x, GEN y);
;; GEN gshz(GEN x, GEN y);
;; GEN gthz(GEN x, GEN y);
;; GEN gashz(GEN x, GEN y);
;; GEN gachz(GEN x, GEN y);

;; void gathz(GEN x, GEN y);
;; GEN ggammaz(GEN x, GEN y);
;; GEN glngammaz(GEN x, GEN y);
;; GEN mpgamdz(long s, GEN y);
;; GEN ggamdz(GEN x, GEN y);
;; GEN gpsiz(GEN x, GEN y);
;; GEN gzetaz(GEN x, GEN y);

;; void gpolylogz(long m, GEN x, GEN y);

;;; /* version.c */

;; GEN gerepilc(GEN l, GEN p, GEN q);
;; void gerepilemany(long ltop, GEN* const gptr[], long nptr);

;;; /* pariinl.h */

;; INLINE GEN bnf_get_fu(GEN bnf);
(pari-call-out bnf-get-fu "bnf_get_fu" (bnf))

;; INLINE GEN gen_I(void);
(pari-call-out pari-I "gen_I" () "complex i")

;; INLINE GEN pol_0(long v);
(pari-call-out pari-poly-0 "pol_0" ((v long)))
;; INLINE GEN pol_1(long v);
(pari-call-out pari-poly-1 "pol_1" ((v long)))
;; INLINE GEN pol_x(long v);
(pari-call-out pari-poly-x "pol_x" ((v long)))


;;; mpdefs.h

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
void get_integer_data (GEN x, ulong len, ulong *data);
void get_integer_data (GEN x, ulong len, ulong *data)
{ ulong i; for (i=0; i < len; i++) data[len-i-1] = *int_W(x,i); }
void set_integer_data (GEN x, ulong len, ulong *data);
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

;;; mpansi.h

;; GEN cgeti(long x);
;; GEN cgetr(long x);
;; GEN stoi(long x);

;; GEN cgetg(long x, long y);
;; GEN mpneg(GEN x);
;; GEN mpabs(GEN x);
(def-call-out pari-cgetg
  (:name "cgetg")
  (:return-type pari-gen)
  (:arguments (x long) (y long)))

;;; /* mp.c */

;; GEN gerepile(long l, long p, GEN q);
;; GEN icopy(GEN x);
;; GEN mpcopy(GEN x);
#|
 (def-call-out pari-gerepile
  (:name "gerepile")
  (:return-type pari-gen)
  (:arguments (l long) (p long) (q pari-gen :in :none)))
|#

;;;; Conversion CLISP --> pari and pari --> CLISP

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
  (pari::%pari-I))

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
    (t (error "~S: Pari type ~D is not yet implemented as a CLISP type."
              'convert-from-pari (pari-type-raw ptr)))))

(defun convert-to-boolean (ptr)
  (case (convert-from-pari ptr)
    (0 nil)
    (1 t)
    (t (error "Pari predicate returned ~S instead of 0 or 1."
              (convert-from-pari ptr)))))

(export 'pari-to-lisp)
(defgeneric pari-to-lisp (x))

(defmethod pari-to-lisp ((x pari-object)) x)
(defmethod pari-to-lisp ((x pari-object-internal))
  (convert-from-pari (pari-object-internal-pointer x)))
(defmethod pari-to-lisp ((x number)) x)
(defmethod pari-to-lisp ((x array)) x)
(defmethod pari-to-lisp ((x null)) x)

;; local variables:
;; eval: (put 'pari-call-out 'common-lisp-indent-function 'defun)
;; eval: (put 'pari-call-out-prec 'common-lisp-indent-function 'defun)
;; eval: (font-lock-add-keywords nil '(("(\\(pari-call-out\\(-prec\\)?\\)\\s *\\(\\(\\s_\\|\\sw\\)*\\)" (1 font-lock-keyword-face) (3 font-lock-function-name-face)) ("(\\(pari-call-out\\(-prec\\)?\\)\\s *(\\(\\(\\s_\\|\\sw\\)*\\)\\s *\\(\\(\\s_\\|\\sw\\)*\\).*)" (1 font-lock-keyword-face) (3 font-lock-function-name-face) (5 font-lock-type-face))))
;; end:
