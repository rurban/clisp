;; CLISP interface to PARI <http://pari.math.u-bordeaux.fr/>
;; Copyright (C) 1995 Michael Stoll
;; Copyright (C) 2004 Sam Steingold
;; This is free software, distributed under the GNU GPL

(defpackage #:pari
  (:use #:lisp #:ext #:ffi))
(in-package #:pari)

(pushnew :pari *features*)

(eval-when (compile load eval)
  (defconstant pari-function-count
    492))

;;; Define the functions for output and error output from pari

(defun pari-putc (c) (write-char c *standard-output*))
(defun pari-puts (s) (write-string s *standard-output*))
(defun pari-flush () (finish-output *standard-output*))

(def-call-in pari-putc
  (:name "clispPutc")
  (:return-type nil)
  (:arguments (c character))
  (:language :stdc))
(def-call-in pari-puts
  (:name "clispPuts")
  (:return-type nil)
  (:arguments (s c-string :in :malloc-free))
  (:language :stdc))
(def-call-in pari-flush
  (:name "clispFlush")
  (:return-type nil)
  (:arguments)
  (:language :stdc))

(defun pari-err-putc (c) (write-char c *error-output*))
(defun pari-err-puts (s) (write-string s *error-output*))
(defun pari-err-flush () (finish-output *error-output*))
(defun pari-err-die () (error "Error within pari."))

(def-call-in pari-err-putc
  (:name "clispErrPutc")
  (:return-type nil)
  (:arguments (c character))
  (:language :stdc))
(def-call-in pari-err-puts
  (:name "clispErrPuts")
  (:return-type nil)
  (:arguments (s c-string :in :malloc-free))
  (:language :stdc))
(def-call-in pari-err-flush
  (:name "clispErrFlush")
  (:return-type nil)
  (:arguments)
  (:language :stdc))
(def-call-in pari-err-die
  (:name "clispErrDie")
  (:return-type nil)
  (:arguments)
  (:language :stdc))

;;; Declare all the pari types, variables, functions, ...
(c-lines "#undef T~%#include \"pari.h\"~%")

;; #ifdef LONG_IS_32BIT
;; #define SIGNBITS 0xff000000L
;; #define SIGNSHIFT 24
(defconstant pari-sign-byte (byte 8 24))
;; #define TYPBITS 0xff000000L
;; #define TYPSHIFT 24
(defconstant pari-type-byte (byte 8 24))
  ;; #define PEREBITS 0xff0000L
  ;; #define PERESHIFT 16
;; #define LGBITS 0xffffL
(defconstant pari-length-byte (byte 16 0))
;; #define LGEFBITS 0xffffL
(defconstant pari-effective-length-byte (byte 16 0))
;; #define EXPOBITS 0xffffffL
(defconstant pari-exponent-byte (byte 24 0))
;; #define HIGHEXPOBIT 0x800000L
(defconstant pari-exponent-offset #x800000)
;; #define VALPBITS 0xffffL
(defconstant pari-valuation-byte (byte 16 0))
;; #define HIGHVALPBIT 0x8000L
(defconstant pari-valuation-offset #x8000)
;; #define PRECPBITS 0xffff0000L
;; #define PRECPSHIFT 16
(defconstant pari-precision-byte (byte 16 16))
;; #define VARNBITS 0xff0000L
;; #define VARNSHIFT 16
(defconstant pari-varno-byte (byte 8 16))
;; #endif

;; #ifdef LONG_IS_64BIT
;; ...
;; #endif

;; <parigen.h>

;;; The pari object type:
;;;   typedef long    *GEN;
;;; To prevent CLISP from thinking we want to do something with the long
;;; such a pointer points to, we replace long* by void*:
(def-c-type pari-gen c-pointer)

;; <paristio.h>

;; typedef struct entree {
;;   char *name;
;;   ulong valence;
;;   void *value;
;;   long menu;
;;   char *code;
;;   struct entree *next;
;;   char *help;
;;   void *args;
;; } entree;
(def-c-struct entree
  (name c-string)
  (valence long)
  (value c-pointer)
  (menu long)
  (next (c-ptr entree))
  (help c-string)
  (args c-pointer))

;; typedef unsigned char *byteptr;
(def-c-type byteptr (c-ptr uchar))

;; typedef ulong pari_sp;
(def-c-type pari_sp ulong)

;; extern  long    prec,precdl,defaultpadicprecision;
(def-c-var pari-real-prec-raw    (:name "prec")                  (:type long))
(def-c-var pari-series-precision (:name "precdl")                (:type long))
(def-c-var pari-padic-precision  (:name "defaultpadicprecision") (:type long))

(export '(pari-real-precision pari-series-precision pari-padic-precision))
(define-symbol-macro pari-real-precision (pari-get-real-prec-digits))
(defun pari-get-real-prec-digits ()
  (values (floor (* #.(* 32 (log 2 10)) (- pari-real-prec-raw 2)))))
(defun (setf pari-get-real-prec-digits) (digits)
  (let ((bits (ceiling (* #.(log 10 2) digits))))
    (setf (long-float-digits) bits)
    (setf pari-real-prec-raw (+ (ceiling bits 32) 2))
    digits))

;; extern  GEN     bernzone,gpi,geuler;
#|
 (def-c-var pari-pi    (:name "gpi")    (:type pari-gen) (:read-only t))
 (def-c-var pari-euler (:name "geuler") (:type pari-gen) (:read-only t))
|#
#|
 (def-c-var pari-bernzone (:name "bernzone") (:type pari-gen) (:read-only t))
|#

;; a scratch variable for CLISP: (defined in clisp-interface.c)
(c-lines "extern void* clispTemp;~%")
(def-c-var temp (:name "clispTemp") (:type c-pointer))

;; extern  long    tglobal,paribuffsize,pariecho;
;; extern  long    compact_arrays;
;; extern  long    *ordvar,varchanged;
;; extern  GEN     polvar;
;; extern  GEN     RAVYZARC;

;; extern  long    NUMFUNC;
;; extern  entree  fonctions[],**hashtable;
(def-c-var pari-functions
    (:name "fonctions")
  (:type (c-array entree #.pari-function-count))
  (:read-only t))

;; extern  long    lontyp[],lontyp2[];

;; extern  long    quitting_pari;
;; extern  jmp_buf environnement;
;; extern  FILE    *outfile, *logfile, *infile, *errfile;

;; extern  ulong    avma,bot,top;
(def-c-var pari-avma (:name "avma") (:type pari_sp))
(def-c-var pari-top  (:name "top")  (:type pari_sp))
(def-c-var pari-bot  (:name "bot")  (:type pari_sp))

;; extern  GEN     gnil,gun,gdeux,ghalf,gi,gzero;
(def-c-var pari-nil  (:name "gnil")  (:type pari-gen) (:read-only t))
(def-c-var pari-1    (:name "gun")   (:type pari-gen) (:read-only t))
(def-c-var pari-2    (:name "gdeux") (:type pari-gen) (:read-only t))
(def-c-var pari-1/2  (:name "ghalf") (:type pari-gen) (:read-only t))
(def-c-var pari-i    (:name "gi")    (:type pari-gen) (:read-only t))
(def-c-var pari-0    (:name "gzero") (:type pari-gen) (:read-only t))

;; extern  GEN     *polun,*polx;
#|
 (def-c-var pari-poly-1 (:name "polun") (:type (c-ptr pari-gen)) (:read-only t))
 (def-c-var pari-poly-x (:name "polx")  (:type (c-ptr pari-gen)) (:read-only t))
|#

;; extern  byteptr diffptr;
;; extern  GEN     primetab;

;; extern  GEN     *g;
;; extern  entree  **varentries; /* noms des inconnues actives */
;; extern  GEN     premierbloc;  /* tete de la liste de blocs */
;; extern  long    nvar;         /* numero de la prochaine inconnue */
;; extern  long    glbfmt[];
(def-c-var pari-format (:name "glbfmt") (:type (c-array long 3)))

;; extern  long    **rectgraph;
;; extern  long    pari_randseed;
;; extern  long    DEBUGLEVEL;

;; extern const int STACKSIZE;  /* nombre de gn possibles */
;; extern const int TBLSZ;  /* taille de la table de hashcodes */
;; extern const int NUMPRTBELT; /* taille table de premiers prives */

;; extern  char    *helpmessage[]; /* un message pour chaque fonction predefinie */
(def-c-var pari-helpmessages
  (:name "helpmessage")
  (:type (c-array c-string #.pari-function-count))
  (:read-only t))

(defconstant pari-function-table pari-functions)
(defconstant pari-docstring-table pari-helpmessages)

(defun get-pari-docstring (str name)
  (let ((index (position str pari-function-table
                         :key #'entree-name :test #'string=)))
    (if (null index)
      nil
      (format nil "~A corresponds to the gp function ~A:~%~A"
              name str (aref pari-docstring-table index)))))

(defconstant gp-to-clisp-table
  (make-hash-table :test #'equal :size pari-function-count))

(defun find-pari-function (x)
  (setq x (string-downcase (string x)))
  (let ((found (gethash x gp-to-clisp-table)))
    (dolist (f found)
      (format *standard-output* "~A  <-->  ~S~%" x f)
      (let ((docstr (documentation f 'function)))
	(when docstr
	  (format *standard-output* "~%~A~%" docstr))))
    (values)))

(export 'find-pari-function)

;; extern  char    *errmessage[];  /* un par numero d'erreur */
#|
 (def-c-var pari-errormessages
  (:name "errmessage")
  (:type (c-array c-string 505))
  (:read-only t))

;; extern  char    *pariversion;
 (def-c-var pari-version (:name "pariversion") (:type c-string) (:read-only t))
|#
  ;; #define gval(x,v) ggval(x,polx[v])
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
    (intern (format nil "PARI-~A" sym) (find-package "PARI")))
  (defun convert-to-lambdalist (args)
    (let ((flag nil))
      (mapcan #'(lambda (arg)
                  (if (symbolp arg)
		    (list arg)
		    (case (length arg)
		      ((1 2) (list (first arg)))
		      ((3 4) (if (eq (third arg) :out) '() (list (first arg))))
		      (t `(,@(if flag '() (progn (setq flag t) '(&key)))
		           (,(first arg) ,(fifth arg)))))))
	      args)))
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
       (if docstring
         (progn
	    (setf (documentation ',name 'function)
		    (format nil "~A~%~A" docstring docstr2))
	    (push ',name (gethash ,gp-name gp-to-clisp-table '())))
	 (progn
	   (setf (documentation ',name 'function) docstr2)
	   ,@(if (string= gp-name "?")
	       '()
	       `((push ',name (gethash ,gp-name gp-to-clisp-table '())))))))))

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
;;; This defines a foreign function pari-<name> with arguments and return
;;; type as specified. Moreover, if <gp-name> is non-nil, a function
;;; <name> is defined and <name> is exported from the PARI package.
;;; Arguments and return value are converted as necessary.
;;; <arg-spec>s with <default> present must occur consecutively at the
;;; end of the <arg-spec> list (with :out parameters removed);
;;; the corresponding arguments are made
;;; into keyword arguments to <name> with defaults as given.
;;; If the return-type given was pari-bool, the result should be a pari
;;; zero or one and is converted to nil or t, respectively.
;;; Furthermore, a documentation string is provided and a table is updated,
;;; so that invoking (find-pari-function <gp-name>) prints <name> together
;;; with its documentation.
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
	 (:arguments ,@(mapcar #'make-arg-spec args))
	 (:language :stdc))
       ,@(if gp-name
           `(,(make-defun name pari-name type args)
	     ,(make-documentation name gp-name args))
	   '()))))

;;; pari-call-out-prec has the same syntax as pari-call-out; it additionally
;;; provides a keyword argument prec defaulting to pari-real-prec-raw.
(defmacro pari-call-out-prec (fun lib-name args &optional (gp-name lib-name))
  `(pari-call-out ,fun ,lib-name
     (,@args (prec long :in :none pari-real-prec-raw)) ,gp-name))


;;; /* alglin.c */

;; GEN     gtrans(GEN x),gscalmat(GEN x, long n),gscalsmat(long x, long n),gaddmat(GEN x, GEN y),gaddsmat(long s, GEN y),inverseimage(GEN mat, GEN y);
(pari-call-out matrix-transpose "gtrans" (x) "trans")
(pari-call-out matrix-inverse-image "inverseimage" (mat y))
(pari-call-out scalar-matrix "gscalmat" (x (n long)) "?")

;; GEN     ker(GEN x),keri(GEN x),kerreel(GEN x, long prec);
(pari-call-out matrix-kernel "ker" (x))
(pari-call-out matrix-kernel-integral "keri" (x))
(pari-call-out-prec matrix-kernel-inexact "kerreel" (x) "kerr")

;; GEN     image(GEN x),imagereel(GEN x, long prec),imagecompl(GEN x),image2(GEN x),suppl(GEN x),eigen(GEN x, long prec),hess(GEN x);
(pari-call-out matrix-image "image" (x))
(pari-call-out-prec matrix-image-inexact "imagereel" (x) "imager")
(pari-call-out matrix-image-complement "imagecompl" (x))
(pari-call-out matrix-supplement "suppl" (x) "supplement")
(pari-call-out-prec matrix-eigenvectors "eigen" (x))
(pari-call-out matrix-to-hessenberg-form "hess" (x))

;; GEN     carhess(GEN x, long v);
(pari-call-out characteristic-polynomial-hessenberg "carhess"
  (x (varno long)) "char2")

;; GEN     gauss(GEN a, GEN b),invmat(GEN a),det(GEN a),detreel(GEN a),det2(GEN a);
(pari-call-out matrix-solve "gauss" (a b))
(pari-call-out matrix-determinant "det" (a))

;; GEN     caract(GEN x, int v),caradj(GEN x, long v, GEN *py),adj(GEN x),caradj0(GEN x, long v),trace(GEN x),trace9(GEN x,GEN p1);
(pari-call-out characteristic-polynomial "caradj0"
  (x (varno long :in :none 0)) "char")
(pari-call-out characteristic-polynomial-and-adjoint-matrix "caradj"
  (x (varno long :in :none 0) (py (c-ptr pari-gen) :out :alloca)) "?")
(pari-call-out adjoint-matrix "adj" (x))
(pari-call-out pari-trace "trace" (x))

;; GEN     assmat(GEN x),gnorm(GEN x),gnorml2(GEN x),gconj(GEN x),concat(GEN x, GEN y),idmat(long n),conjvec(GEN x,long prec);
(pari-call-out norm "gnorm" (x) "norm")
(pari-call-out l2-norm "gnorml2" (x) "norml2")
(pari-call-out pari-conjugate "gconj" (x) "conj")
(pari-call-out-prec vector-of-conjugates "conjvec" (x))
(pari-call-out identity-matrix "idmat" ((n long)))
(pari-call-out pari-concatenate "concat" (x y))

;; GEN     extract(GEN x, GEN l),matextract(GEN x, GEN l1, GEN l2),gtomat(GEN x),invmulmat(GEN a, GEN b),invmulmatreel(GEN a, GEN b),invmatreel(GEN a);
(pari-call-out vector-extract "extract" (x l))
(pari-call-out matrix-extract "matextract" (x l1 l2))
(pari-call-out convert-to-matrix "gtomat" (x) "mat")
(pari-call-out matrix-invert-inexact "invmatreel" (a) "matinvr")
(pari-call-out matrix-invert-and-multiply-inexact "invmulmatreel" (a b) "?")

;; GEN     sqred(GEN a),sqred1(GEN a),sqred2(GEN a, long flg),sqred3(GEN a),signat(GEN a),jacobi(GEN a, long prec),matrixqz(GEN x, GEN pp),matrixqz2(GEN x),matrixqz3(GEN x);
(pari-call-out symmetric-matrix-sqred "sqred" (a))
(pari-call-out symmetric-matrix-signature "signat" (a))
(pari-call-out-prec symmetric-matrix-eigenstuff "jacobi" (a))
(pari-call-out matrix-qz "matrixqz" (x pp))
(pari-call-out matrix-qz2 "matrixqz2" (x))
(pari-call-out matrix-qz3 "matrixqz3" (x))

;; GEN     indexrank(GEN x),kerint(GEN x),kerint1(GEN x),kerint2(GEN x),intersect(GEN x, GEN y),deplin(GEN x),detint(GEN x);
(pari-call-out matrix-indexrank "indexrank" (x))
(pari-call-out matrix-kernel-integral-reduced "kerint" (x))
(pari-call-out matrix-kernel-integral-reduced-1 "kerint1" (x))
(pari-call-out matrix-subspace-intersection "intersect" (x y))
(pari-call-out linear-dependence "deplin" (x))
(pari-call-out matrix-determinant-multiple "detint" (x))


;; GEN     hnfspec(long** mat,GEN* ptdep,GEN* ptmatc,long* vperm,GEN* ptmatalpha,long co,long li,long k0,long* ptnlze,long* ptcol);

;; GEN     hnffinal(GEN matgen,GEN* ptpdep,GEN* ptmatc,long* vperm,GEN* ptmatalpha,long lnz,long co,long li,long col,long lig,long nlze,long* ptcol);

;; GEN     hnfadd(GEN mit,GEN* ptpdep,GEN* ptmatc,long* vperm,GEN* ptmatalpha,long co,long li,long col,long* ptnlze,GEN extramat,GEN extramatc);

;; long    rank(GEN x),perf(GEN a);
(pari-call-out (matrix-rank long) "rank" (x))
(pari-call-out (symmetric-matrix-perfection) "perf" (a))

;;; /* anal.c */

;; GEN     lisexpr(char *t),readexpr(char **c),lisseq(char *t),readseq(char **c);
(pari-call-out read-from-string "lisseq" ((str c-string :in :alloca)) nil)

;; void    switchin(char *name), switchout(char *name), fliplog(void);

;;; /* arith.c */

;; GEN     racine(GEN a),mppgcd(GEN a, GEN b),mpfact(long n),mpfactr(long n, long prec);
(pari-call-out pari-isqrt "racine" (a) "isqrt")
(pari-call-out factorial-integer "mpfact" ((n long)) "!")
(pari-call-out-prec factorial-real "mpfactr" ((n long)) "fact")

;; GEN     sfcont(GEN x, GEN x1, long k),sfcont2(GEN b, GEN x),gcf(GEN x),gcf2(GEN b, GEN x),pnqn(GEN x),gboundcf(GEN x, long k);
(pari-call-out bounded-continued-fraction "gboundcf" (x (k long)) "boundcf")
(pari-call-out continued-fraction "gcf" (x) "cf")
(pari-call-out continued-fraction-2 "gcf2" (b x) "cf2")
(pari-call-out continued-fraction-convergent "pnqn" (x))

;; GEN     bestappr(GEN x, GEN k), addprimestotable(GEN primes);
(pari-call-out add-primes "addprimestotable" (primes) "addprimes")
(pari-call-out best-rational-approximation "bestappr" (x k))

;; GEN     bezout(GEN a, GEN b, GEN *u, GEN *v),chinois(GEN x, GEN y),mpinvmod(GEN a, GEN m),puissmodulo(GEN a, GEN n, GEN m),fibo(long n),bigprem(GEN n),prime(long n);
(pari-call-out chinese-lift "chinois" (x y) "chinese")
(pari-call-out fibonacci "fibo" ((n long)))
(pari-call-out next-prime "bigprem" (n) "nextprime")
(pari-call-out nth-prime "prime" ((n long)))

;; GEN     primes(long n),phi(GEN n),decomp(GEN n),auxdecomp(GEN n, long all),smallfact(GEN n),boundfact(GEN n, long lim);
(pari-call-out factor-bounded "boundfact" (n (lim long)))
(pari-call-out euler-phi "phi" (n))
(pari-call-out first-n-primes "primes" ((n long)))
(pari-call-out factor-small "smallfact" (n))

;; GEN     sumdiv(GEN n),sumdivk(long k, GEN n),numbdiv(GEN n),binaire(GEN x),order(GEN x),gener(GEN m),znstar(GEN x),divisors(GEN n);
(pari-call-out divisors "divisors" (n))
(pari-call-out count-divisors "numbdiv" (n) "numdiv")
(pari-call-out order "order" (x))
(pari-call-out primitive-root "gener" (m) "primroot")
(pari-call-out sum-divisors "sumdiv" (n) "sigma")
(pari-call-out sum-divisor-powers "sumdivk" ((k long) n) "sigmak")
(pari-call-out structure-of-z/n* "znstar" (x))

;; GEN     ellfacteur(GEN n1),classno(GEN x),classno2(GEN x),classno3(GEN x),fundunit(GEN x),regula(GEN x, long prec);
(pari-call-out quadratic-class-number "classno" (x))
(pari-call-out-prec quadratic-regulator "regula" (x))
(pari-call-out quadratic-unit "fundunit" (x) "unit")

;; GEN     compimag(GEN x, GEN y),sqcomp(GEN x),qfi(GEN x, GEN y, GEN z),qfr(GEN x, GEN y, GEN z, GEN d),compreal(GEN x, GEN y),redreal(GEN x),sqcompreal(GEN x);
(pari-call-out compose-imag-qf "compimag" (x y))
(pari-call-out make-imag-qf "qfi" (x y z))
(pari-call-out make-real-qf "qfr" (x y z d))
(pari-call-out reduce-real-qf "redreal" (x))

;; GEN     rhoreal(GEN x),rhorealnod(GEN x, GEN isqrtD),redrealnod(GEN x, GEN isqrtD),redimag(GEN x);
(pari-call-out reduce-imag-qf "redimag" (x))
(pari-call-out reduce-real-qf-no-d "redrealnod" (x isqrtD))
(pari-call-out reduce-real-qf-one-step "rhoreal" (x))
(pari-call-out reduce-real-qf-no-d-one-step "rhorealnod" (x isqrtD))

;; GEN     primeform(GEN x, GEN p, long prec);
(pari-call-out-prec prime-form "primeform" (x p) "pf")

;; GEN     nucomp(GEN x, GEN y, GEN l),nudupl(GEN x, GEN l),nupow(GEN x, GEN n);
(pari-call-out shanks-compose-imag-qf "nucomp" (x y l))
(pari-call-out shanks-double-imag-qf "nudupl" (x l))
(pari-call-out shanks-power-imag-qf "nupow" (x n))

;; GEN     comprealraw(GEN x, GEN y),sqcomprealraw(GEN x),powrealraw(GEN x, long n, long prec);
(pari-call-out compose-real-qf-raw "comprealraw" (x y))
(pari-call-out-prec power-real-qf-raw "powrealraw" (x (n long)))

;; GEN     gkronecker(GEN x, GEN y),gkrogs(GEN x, long y),gcarreparfait(GEN x),gcarrecomplet(GEN x, GEN *pt);

;; GEN     gisprime(GEN x),gispsp(GEN x),gissquarefree(GEN x),gisfundamental(GEN x),gbittest(GEN x, GEN n);
(pari-call-out (fundamental-discriminant? pari-bool) "gisfundamental"
  (x) "isfund")
(pari-call-out (prime? pari-bool) "gisprime" (x) "isprime")
(pari-call-out (pseudo-prime? pari-bool) "gispsp" (x) "ispsp")
(pari-call-out (square-free? pari-bool) "gissquarefree" (x) "issqfree")
(pari-call-out (square? pari-bool) "gcarreparfait" (x) "issquare")

;; GEN     gpseudopremier(GEN n, GEN a),gmillerrabin(GEN n, long k),gmu(GEN n),gomega(GEN n),gbigomega(GEN n);

;; long    kronecker(GEN x, GEN y),krosg(long s, GEN x),krogs(GEN x, long y),kross(long x, long y),kro8(GEN x, GEN y);
(pari-call-out (kronecker-symbol long) "kronecker" (x y) "kro")

;; long    mu(GEN n),omega(GEN n),bigomega(GEN n),hil(GEN x, GEN y, GEN p);
(pari-call-out (bigomega long) "bigomega" (n))
(pari-call-out (hilbert-symbol long) "hil" (x y p) "hilb")
(pari-call-out (moebius-mu long) "mu" (n))
(pari-call-out (omega long) "omega" (n))

;; int     carreparfait(GEN x),carrecomplet(GEN x, GEN *pt),bittest(GEN x, long n);

;; int     isprime(GEN x),ispsp(GEN x),issquarefree(GEN x),isfundamental(GEN x),mpsqrtmod(GEN a, GEN p, GEN *pr);

;; int     millerrabin(GEN n, long k),pseudopremier(GEN n, GEN a),inversemodulo(GEN a, GEN b, GEN *res);

;; byteptr initprimes(long maxnum);

;; void    lucas(long n, GEN *ln, GEN *ln1);

;;; /* base.c */

;; GEN     base(GEN x, GEN *y),smallbase(GEN x, GEN *y),discf(GEN x),smalldiscf(GEN x),discf2(GEN x);
(pari-call-out nf-basis "base" (x (y (c-ptr pari-gen) :out :alloca)) "basis")
(pari-call-out nf-field-discriminant "discf" (x))
(pari-call-out nf-basis-small "smallbase"
  (x (y (c-ptr pari-gen) :out :alloca)) "smallbasis")
(pari-call-out nf-field-discriminant-small "smalldiscf" (x))

;; GEN     hnf(GEN x),hnfhavas(GEN x),hnfnew(GEN x),hnfperm(GEN x);
(pari-call-out matrix-to-hnf "hnf" (x) "hermite")

;; GEN     cleanmod(GEN x,long lim,GEN detmat,GEN detmatsur2);

;; GEN     hnfmod(GEN x, GEN detmat),hnfmodid(GEN x,GEN p),smith(GEN x),smith2(GEN x);
(pari-call-out matrix-to-hnf-mod "hnfmod" (x detmat) "hermitemod")
(pari-call-out matrix-elementary-divisors "smith" (x))
(pari-call-out matrix-elementary-divisors-transforms "smith2" (x))

;; GEN     factoredbase(GEN x, GEN p, GEN *y),factoreddiscf(GEN x, GEN p),allbase(GEN x, long code, GEN *y),galois(GEN x, long prec),initalg(GEN x, long prec),initalgred(GEN x, long prec);
(pari-call-out nf-basis-factored "factoredbase"
  (x p (y (c-ptr pari-gen) :out :alloca)) "factoredbasis")
(pari-call-out nf-field-discriminant-factored "factoreddiscf" (x p))
(pari-call-out-prec nf-galois-group "galois" (x))
(pari-call-out-prec nf-initalg "initalg" (x))
(pari-call-out-prec nf-initalg-reduced "initalgred" (x))

;; GEN     tschirnhaus(GEN x),galoisconj(GEN x, long prec),galoisconj1(GEN x, long prec),galoisconj2(GEN x, long prec),galoisconjforce(GEN x, long prec),galoisapply(GEN nf, GEN aut, GEN x),initalgred2(GEN x, long prec);
(pari-call-out nf-apply-galois "galoisapply" (nf aut x))
(pari-call-out-prec nf-galois-conjugates "galoisconj" (x))
(pari-call-out-prec nf-galois-conjugates-1 "galoisconj1" (x))
(pari-call-out-prec nf-galois-conjugates-forced "galoisconjforce" (x))
(pari-call-out-prec nf-initalg-reduced-2 "initalgred2" (x))
(pari-call-out nf-tschirnhausen-transformation "tschirnhaus" (x))

;; GEN     primedec(GEN nf,GEN p),idealmul(GEN nf,GEN ix,GEN iy),idealmulred(GEN nf, GEN ix, GEN iy, long prec), ideal_two_elt(GEN nf, GEN ix);
(pari-call-out ideal-multiply "idealmul" (nf ix iy))
(pari-call-out-prec ideal-multiply-reduced "idealmulred" (nf ix iy))
(pari-call-out ideal-2-generators "ideal_two_elt" (nf ix) "idealtwoelt")
(pari-call-out nf-prime-decomposition "primedec" (nf p))

;; GEN     idealmulh(GEN nf, GEN ix, GEN iy),element_mulh(GEN nf, long limi, long limj, GEN x, GEN y);

;; GEN     idealmulprime(GEN nf,GEN ix,GEN vp),minideal(GEN nf,GEN ix,GEN vdir,long prec);

;; GEN     idealmulelt(GEN nf, GEN elt, GEN x),idealmullll(GEN nf, GEN x, GEN y);

;; GEN     ideallllredall(GEN nf, GEN ix, GEN vdir, long prec, long precint);

;; GEN     ideallllred(GEN nf,GEN ix,GEN vdir,long prec);
(pari-call-out-prec ideal-lll-reduction "ideallllred" (nf ix vdir))

;; GEN     ideallllredpart1(GEN nf,GEN x,GEN vdir, long flprem, long prec);

;; GEN     ideallllredpart1spec(GEN nf, GEN x, GEN matt2, long flprem, long prec);

;; GEN     ideallllredpart2(GEN nf,GEN arch,GEN z,long prec);

;; GEN     element_mul(GEN nf,GEN x,GEN y),element_sqr(GEN nf,GEN x),element_pow(GEN nf,GEN x,GEN k), element_mulvec(GEN nf, GEN x, GEN v);
(pari-call-out nf-element-multiply "element_mul" (nf x y) "nfmul")
(pari-call-out nf-element-power "element_pow" (nf x k) "nfpow")

;; GEN     rootsof1(GEN x),idealinv(GEN nf, GEN ix),oldidealinv(GEN nf, GEN ix);
(pari-call-out ideal-invert "idealinv" (nf x))
(pari-call-out ideal-invert-2 "oldidealinv" (nf x) "idealinv2")
(pari-call-out nf-roots-of-unity "rootsof1" (x))

;; GEN     idealpow(GEN nf, GEN ix, GEN n),idealpowred(GEN nf, GEN ix, GEN n, long prec),idealpows(GEN nf, GEN ideal, long iexp);
(pari-call-out ideal-power "idealpow" (nf iX n))
(pari-call-out-prec ideal-power-reduced "idealpowred" (nf ix n))

;; GEN     idealpowprime(GEN nf, GEN vp, GEN n,long prec),idealfactor(GEN nf, GEN x);
(pari-call-out ideal-factor "idealfactor" (nf x))

;; GEN     idealhermite(GEN nf, GEN x),idealhermite2(GEN nf, GEN a, GEN b),idealadd(GEN nf, GEN x, GEN y), idealaddone(GEN nf, GEN x, GEN y), idealaddmultone(GEN nf, GEN list), idealdiv(GEN nf, GEN x, GEN y);
(pari-call-out ideal-add "idealadd" (nf x y))
(pari-call-out ideal-split-one-2 "idealaddone" (nf x y))
(pari-call-out ideal-split-one-n "idealaddmultone" (nf l))
(pari-call-out ideal-divide "idealdiv" (nf x y))
(pari-call-out ideal-to-hnf "idealhermite" (nf x))
(pari-call-out ideal-2-generators-to-hnf "idealhermite2" (nf a b))

;; GEN     idealintersect(GEN nf, GEN x, GEN y), principalideal(GEN nf, GEN a);
(pari-call-out ideal-intersection "idealintersect" (nf x y))
(pari-call-out nf-principal-ideal "principalideal" (nf a))

;; GEN     principalidele(GEN nf, GEN a),idealdivexact(GEN nf, GEN x, GEN y),idealnorm(GEN nf, GEN x);
(pari-call-out ideal-divide-exact "idealdivexact" (nf x y))
(pari-call-out ideal-norm "idealnorm" (nf x))
(pari-call-out nf-principal-idele "principalidele" (nf a))

;; GEN     idealappr(GEN nf, GEN x),idealapprfact(GEN nf, GEN x), idealapprall(GEN nf, GEN x, long fl), idealchinese(GEN nf, GEN x, GEN y);
(pari-call-out ideal-approximate "idealappr" (nf x))
(pari-call-out ideal-approximate-factored "idealapprfact" (nf x))

;; GEN     idealcoprime(GEN nf, GEN x, GEN y),ideal_two_elt2(GEN nf, GEN x, GEN a);
(pari-call-out ideal-coprime "idealcoprime" (nf x y))
(pari-call-out ideal-2-generators-2 "ideal_two_elt2" (nf x a) "idealtwoelt2")

;; GEN     twototwo(GEN nf, GEN a, GEN b),threetotwo(GEN nf, GEN a, GEN b, GEN c),threetotwo1(GEN nf, GEN a, GEN b, GEN c),threetotwo2(GEN nf, GEN a, GEN b, GEN c);
(pari-call-out nf-element-three-to-two "threetotwo" (nf a b c))
(pari-call-out nf-element-two-to-two "twototwo" (nf a b))

;; GEN     basistoalg(GEN nf, GEN x),algtobasis(GEN nf, GEN x);
(pari-call-out nf-alg-to-basis "algtobasis" (nf x))
(pari-call-out nf-basis-to-alg "basistoalg" (nf x))

;; GEN     weakhermite(GEN nf, GEN x),nfhermite(GEN nf, GEN x),nfhermitemod(GEN nf, GEN x, GEN detmat),nfsmith(GEN nf, GEN x);
(pari-call-out nf-pseudo-to-hnf "nfhermite" (nf x))
(pari-call-out nf-pseudo-to-hnf-mod "nfhermitemod" (nf x detmat))
(pari-call-out nf-smith-normal-form "nfsmith" (nf x))

;; GEN     nfdiveuc(GEN nf, GEN a, GEN b), nfdivres(GEN nf, GEN a, GEN b), nfmod(GEN nf, GEN a, GEN b),element_div(GEN nf, GEN x, GEN y),element_inv(GEN nf, GEN x);
(pari-call-out nf-element-divide "element_div" (nf x y) "nfdiv")
(pari-call-out nf-element-euclidean-divide "nfdiveuc" (nf a b))
(pari-call-out nf-element-euclidean-divmod "nfdivres" (nf a b))
(pari-call-out nf-element-mod "nfmod" (nf a b))

;; GEN     nfdetint(GEN nf,GEN pseudo);
(pari-call-out nf-determinant-multiple "nfdetint" (nf pseudo))

;; GEN     element_reduce(GEN nf, GEN x, GEN ideal);
(pari-call-out nf-element-mod-ideal "element_reduce" (nf x ideal) "nfreduce")

;; GEN     checknf(GEN nf), differente(GEN nf, GEN premiers);

;; long    idealval(GEN nf,GEN ix,GEN vp), isideal(GEN nf,GEN x);
(pari-call-out (ideal-valuation long) "idealval" (nf ix vp))
(pari-call-out (nf-ideal? boolean) "isideal" (nf x))

;; long    element_val(GEN nf, GEN x, GEN vp), element_val2(GEN nf, GEN x, GEN d, GEN vp);
(pari-call-out (nf-element-valuation long) "element_val" (nf x vp) "nfval")

;; long    rnfisfree(GEN bnf, GEN order);
(pari-call-out (rnf-free? boolean) "rnfisfree" (bnf order))

;; GEN     allbase4(GEN f, long code, GEN *y, GEN *ptw),base2(GEN x, GEN *y),rnfround2all(GEN nf, GEN pol, long all),rnfpseudobasis(GEN nf, GEN pol),rnfdiscf(GEN nf, GEN pol),rnfsimplifybasis(GEN bnf, GEN order),rnfsteinitz(GEN nf, GEN order),rnfbasis(GEN bnf, GEN order),rnfhermitebasis(GEN bnf, GEN order);
(pari-call-out rnf-basis "rnfbasis" (bnf order))
(pari-call-out rnf-field-discriminant "rnfdiscf" (nf pol))
(pari-call-out rnf-hermite-basis "rnfhermitebasis" (bnf order))
(pari-call-out rnf-pseudobasis "rnfpseudobasis" (bnf order))
(pari-call-out rnf-steinitz-class "rnfsteinitz" (nf order))

;; GEN     bsrch(GEN p, GEN fa, long Ka, GEN eta, long Ma),setup(GEN p,GEN f,GEN theta,GEN nut),eleval(GEN f,GEN h,GEN a),vstar(GEN p,GEN h),factcp(GEN p,GEN f,GEN beta),bestnu(GEN w),gcdpm(GEN f1,GEN f2,GEN pm);

;; GEN     compositum(GEN pol1, GEN pol2);
(pari-call-out nf-compositum "compositum" (pol1 pol2))

;; GEN     initzeta(GEN pol, long prec),gzetak(GEN nfz, GEN s, long prec),glambdak(GEN nfz, GEN s, long prec),gzetakall(GEN nfz, GEN s, long flag, long prec);
(pari-call-out-prec nf-initzeta "initzeta" (pol))
(pari-call-out-prec nf-lambda-value "glambdak" (nfz s) "lambdak")
(pari-call-out-prec nf-zeta-value "gzetak" (nfz s (flag long)) "zetak")

;; GEN     nfreducemodpr(GEN nf, GEN x, GEN prhall),element_divmodpr(GEN nf, GEN x, GEN y, GEN prhall),element_powmodpr(GEN nf, GEN x, GEN k, GEN prhall);
;; #define element_mulmodpr(nf,x,y,prhall) (nfreducemodpr(nf,element_mul(nf,x,y),prhall))
;; #define element_sqrmodpr(nf,x,prhall) (nfreducemodpr(nf,element_sqr(nf,x),prhall))

;;; /* bibli1.c */

;; GEN     tayl(GEN x, long v, long precdl),legendre(long n),tchebi(long n),hilb(long n),pasc(long n),laplace(GEN x);
(pari-call-out laplace-transform "laplace" (x))
(pari-call-out legendre-polynomial "legendre" ((n long)))
(pari-call-out taylor-expansion "tayl"
  (x (varno long :in :none (get-varno x))
     (precdl long :in :none pari-series-precision))
  "taylor")
(pari-call-out tchebychev-polynomial "tchebi" ((n long)))
(pari-call-out hilbert-matrix "hilb" ((n long)) "hilbert")
(pari-call-out pascal-triangle "pasc" ((n long)) "pascal")

;; GEN     gprec(GEN x, long l),convol(GEN x, GEN y),ggrando(GEN x, long n),ggrandocp(GEN x, long n),gconvsp(GEN x),gconvpe(GEN x);
(pari-call-out change-precision "gprec" (x (l long)) "prec")
(pari-call-out hadamard-product "convol" (x y))
(pari-call-out pari-o "ggrandocp" (a (b long)) "o")

;; GEN     lll(GEN x, long prec),lll1(GEN x, long prec),lllrat(GEN x),lllgram(GEN x, long prec),lllgram1(GEN x, long prec),lllgramint(GEN x),lllint(GEN x),lllintpartial(GEN mat),lllintpartialall(GEN mat, long all);
(pari-call-out-prec matrix-lll-reduce "lll" (x))
(pari-call-out-prec gram-matrix-lll-reduce "lllgram" (x))
(pari-call-out gram-matrix-lll-reduce-integral "lllgramint" (x))
(pari-call-out matrix-lll-reduce-integral "lllint" (x))
(pari-call-out matrix-partial-lll-reduce-integral "lllintpartial" (mat))

;; GEN     lllgramkerim(GEN x),lllkerim(GEN x),lllgramall(GEN x, long all),lllall0(GEN x, long all);
(pari-call-out gram-matrix-lll-reduce-kernel-and-image "lllgramkerim" (x))
(pari-call-out matrix-lll-reduce-kernel-and-image "lllkerim" (x))

;; GEN     lllgen(GEN x),lllkerimgen(GEN x),lllgramgen(GEN x),lllgramkerimgen(GEN x),lllgramallgen(GEN x, long all);

;; GEN     binome(GEN x, long k),gscal(GEN x, GEN y),cyclo(long n),vecsort(GEN x, GEN k);
(pari-call-out binomial-coefficient "binome" (x (k long)) "bin")
(pari-call-out cyclotomic-polynomial "cyclo" ((n long)))
(pari-call-out vector-sort-key "vecsort" (x k))

;; GEN     lindep(GEN x, long prec),lindep2(GEN x, long bit),lindep2bis(GEN x, long bit, long prec);
(pari-call-out-prec vector-find-linear-dependence "lindep" (x))

;; GEN     algdep(GEN x, long n, long prec),algdep2(GEN x, long n, long bit),changevar(GEN x, GEN y),ordred(GEN x, long prec);
(pari-call-out change-variables "changevar" (x y))
(pari-call-out-prec nf-ordred "ordred" (x))
(pari-call-out-prec find-algebraic-dependence "algdep" (x (n long)))

;; GEN     polrecip(GEN x),reorder(GEN x),sort(GEN x),lexsort(GEN x),indexsort(GEN x),polsym(GEN x, long n);
(pari-call-out symmetric-powers "polsym" (x (n long)))
(pari-call-out reciprocal-polynomial "polrecip" (x) "recip")
(pari-call-out vector-index-sort "indexsort" (x) "indsort")
(pari-call-out vector-lexsort "lexsort" (x))
(pari-call-out vector-sort "sort" (x))

;; GEN     minim(GEN a, long borne, long stockmax),minimprim(GEN a, long borne, long stockmax);
(pari-call-out symmetric-matrix-minimal-vectors "minim" (a (b long) (m long)))

;; GEN     polred(GEN x, long prec),factoredpolred(GEN x, GEN p, long prec),smallpolred(GEN x, long prec),polred2(GEN x, long prec),factoredpolred2(GEN x, GEN p, long prec), polredabs(GEN x, long prec);
(pari-call-out-prec nf-poly-reduce-factored "factoredpolred" (x p))
(pari-call-out-prec nf-poly-reduce-2-factored "factoredpolred2" (x p))
(pari-call-out-prec nf-poly-reduce "polred" (x))
(pari-call-out-prec nf-poly-reduce-2 "polred2" (x))
(pari-call-out-prec nf-poly-reduce-abs "polredabs" (x))
(pari-call-out-prec nf-poly-reduce-small "smallpolred" (x))

;; GEN     smallpolred2(GEN x, long prec),allpolred(GEN x, GEN *pta, long code, long prec),polymodrecip(GEN x),genrand(void),permute(long n, GEN x),permuteInv(GEN x);
(pari-call-out permutation "permute" ((n long) x) "permutation")
(pari-call-out permutation-number "permuteInv" (x) "permutation2num")
(pari-call-out pari-random "genrand" () "random")
(pari-call-out polymod-reverse "polymodrecip" (x) "modreverse")
(pari-call-out-prec nf-poly-reduce-2-small "smallpolred2" (x))

;; long    mymyrand();

;; long    setprecr(long n),setserieslength(long n),ccontent(long* x,long n);
#|
(pari-call-out (set-real-precision long) "setprecr" ((n long)) "setprecision")
(pari-call-out (set-series-precision long) "setserieslength" ((n long)))
|#

;; GEN     setrand(long seed),getrand(void),getstack(void),gettime(void),getheap(void);
(pari-call-out getheap "getheap" ())
(pari-call-out getrand "getrand" ())
(pari-call-out getstack "getstack" ())
(pari-call-out setrand "setrand" ((seed long)))

;; void    getheapaux(long* nombre, long* espace);

;;; /* bibli2.c */

;; GEN     somme(entree *ep, GEN x, GEN a, GEN b, char *ch),produit(entree *ep, GEN x, GEN a, GEN b, char *ch),suminf(entree *ep, GEN a, char *ch, long prec),prodinf(entree *ep, GEN a, char *ch, long prec),prodinf1(entree *ep, GEN a, char *ch, long prec),prodeuler(entree *ep, GEN a, GEN b, char *ch, long prec);

;; GEN     vecteur(entree *ep, GEN nmax, char *ch),vvecteur(entree *ep, GEN nmax, char *ch),matrice(entree *ep1, entree *ep2, GEN nlig, GEN ncol, char *ch),divsomme(entree *ep, GEN num, char *ch);

;; GEN     qromb(entree *ep, GEN a, GEN b, char *ch, long prec),qromo(entree *ep, GEN a, GEN b, char *ch, long prec),qromi(entree *ep, GEN a, GEN b, char *ch, long prec),rombint(entree *ep, GEN a, GEN b, char *ch, long prec);

;; GEN     polint(GEN xa, GEN ya, GEN x, GEN *dy),plot(entree *ep, GEN a, GEN b, char *ch),ploth(entree *ep, GEN a, GEN b, char *ch, long prec),ploth2(entree *ep, GEN a, GEN b, char *ch, long prec),plothraw(GEN listx, GEN listy),zbrent(entree *ep, GEN a, GEN b, char *ch, long prec);
(pari-call-out interpolating-polynomial-value "polint"
  (xa ya x (dy (c-ptr pari-gen) :out :alloca)))

;; GEN     sumalt(entree *ep, GEN a, char *ch, long prec),sumalt1(entree *ep, GEN a, char *ch, long prec),sumalt2(entree *ep, GEN a, char *ch, long prec),sumalt3(entree *ep, GEN a, char *ch, long prec),sumpos(entree *ep, GEN a, char *ch, long prec),sumposold(entree *ep, GEN a, char *ch, long prec);

;; GEN     forpari(entree *ep, GEN a, GEN b, char *ch),forstep(entree *ep, GEN a, GEN b, GEN s, char *ch),fordiv(entree *ep, GEN a, char *ch),forprime(entree *ep, GEN a, GEN b, char *ch),forvec(entree *ep, GEN x, char *ch);

;; GEN     initrect(long ne, long x, long y),killrect(long ne),rectcursor(long ne),rectmove(long ne, GEN x, GEN y),rectrmove(long ne, GEN x, GEN y),rectpoint(long ne, GEN x, GEN y);

;; GEN     rectrpoint(long ne, GEN x, GEN y),rectbox(long ne, GEN gx2, GEN gy2),rectrbox(long ne, GEN gx2, GEN gy2),rectline(long ne, GEN gx2, GEN gy2),rectrline(long ne, GEN gx2, GEN gy2),rectdraw(GEN list);

;; GEN     rectpoints(long ne, GEN listx, GEN listy),rectlines(long ne, GEN listx, GEN listy),rectstring(long ne, GEN x),rectscale(long ne, GEN x1, GEN x2, GEN y1, GEN y2);

;; GEN     postdraw(GEN list),postploth(entree *ep, GEN a, GEN b, char *ch),postploth2(entree *ep, GEN a, GEN b, char *ch),postplothraw(GEN listx, GEN listy);

;; GEN     gtoset(GEN x), setunion(GEN x, GEN y), setintersect(GEN x, GEN y), setminus(GEN x, GEN y);
(pari-call-out convert-to-set "gtoset" (x) "set")
(pari-call-out pari-set-intersection "setintersect" (x y))
(pari-call-out pari-set-difference "setminus" (x y))
(pari-call-out pari-set-union "setunion" (x y))

;; GEN     dirmul(GEN x, GEN y), dirdiv(GEN x, GEN y), dirzetak(GEN nf, GEN b);
(pari-call-out dirichlet-divide "dirdiv" (x y))
(pari-call-out dirichlet-multiply "dirmul" (x y))
(pari-call-out nf-dirchlet-zeta-series "dirzetak" (nf b))

;; long    isvecset(GEN x), setsearch(GEN x, GEN y);
(pari-call-out (pari-set-position long) "setsearch" (x y))
(pari-call-out (set? boolean) "isvecset" (x) "isset")

;;; /* buch1.c et buch2.c */

;; GEN     buchimag(GEN D, GEN gcbach, GEN gcbach2, GEN gCO);
(pari-call-out buchimag "buchimag" (d c c2 (co pari-gen :in :none 5)))

;; GEN     buchreal(GEN D, GEN gsens, GEN gcbach, GEN gcbach2, GEN gRELSUP, long prec);
(pari-call-out-prec buchreal "buchreal"
  (d narrow? c c2 (relsup pari-gen :in :none 5)))

;; GEN     buchall(GEN P, GEN gcbach, GEN gcbach2, GEN gRELSUP, GEN gborne, long nbrelpid, long minsfb, long flun, long prec);
(pari-call-out-prec nf-buchall "buchall"
   (p c c2 nrel borne (nrpid long) (minsfb long) (flun long)) nil)

(defmacro def-buch-variant (name flun gp-name)
  (let* ((pari-name (make-pari-name 'nf-buchall))
         (type 'pari-gen)
	 (args `(p (c pari-gen :in :none 0.3) (c2 pari-gen :in :none c)
	           (nrel pari-gen :in :none 5) (borne pari-gen :in :none 1)
		   (nrpid long :in :none 4) (minsfb long :in :none 3)
		   (flun long :in :none ,flun)
		   (prec log :in :none pari-real-prec-raw))))
    `(progn
       ,(make-defun name pari-name type args)
       ,(make-documentation name gp-name args))))

;; #define buchgen(P,gcbach,gcbach2,prec) buchall(P,gcbach,gcbach2,stoi(5),gzero,4,3,0,prec)
(def-buch-variant nf-buchgen 0 "buchgen")

;; #define buchgenfu(P,gcbach,gcbach2,prec) buchall(P,gcbach,gcbach2,stoi(5),gzero,4,3,2,prec)
(def-buch-variant nf-buchgenfu 2 "buchgenfu")

;; #define buchinit(P,gcbach,gcbach2,prec) buchall(P,gcbach,gcbach2,stoi(5),gzero,4,3,-1,prec)
(def-buch-variant nf-buchinit -1 "buchinit")

;; #define buchinitfu(P,gcbach,gcbach2,prec) buchall(P,gcbach,gcbach2,stoi(5),gzero,4,3,-2,prec)
(def-buch-variant nf-buchinitfu -2 "buchinitfu")

;; GEN     isprincipal(GEN bignf, GEN x),isprincipalgen(GEN bignf, GEN x);
(pari-call-out ideal-class "isprincipal" (bignf x))
(pari-call-out ideal-class-more "isprincipalgen" (bignf x))

;; GEN     isunit(GEN bignf, GEN x), signunit(GEN bignf), buchnarrow(GEN bignf), buchfu(GEN bignf);
(pari-call-out nf-buchfu "buchfu" (bignf))
(pari-call-out nf-buchnarrow "buchnarrow" (bignf))
(pari-call-out nf-unit-in-basis "isunit" (bnf x))
(pari-call-out nf-unit-signs "signunit" (bignf))

;; int     compte(long **mat, long row, long longueur, long *firstnonzero);
;; int     compte2(long **mat, long row, long longueur, long *firstnonzero);

;;; /* elliptic.c */

;; GEN     ghell(GEN e, GEN a, long prec),ghell2(GEN e, GEN a, long prec),ghell3(GEN e, GEN a, long prec);
(pari-call-out-prec ell-height "ghell" (e a) "hell")

;; GEN     initell(GEN x, long prec),initell2(GEN x, long prec),smallinitell(GEN x),zell(GEN e, GEN z, long prec),coordch(GEN e, GEN ch),pointch(GEN x, GEN ch);
(pari-call-out ell-change-coordinates "coordch" (e ch) "chell")
(pari-call-out ell-change-point-coordinates "pointch" (x ch) "chptell")
(pari-call-out-prec ell-init "initell" (x))
(pari-call-out ell-init-small "smallinitell" (x))
(pari-call-out-prec ell-xy-to-z "zell" (e z))

;; GEN     addell(GEN e, GEN z1, GEN z2),subell(GEN e, GEN z1, GEN z2),powell(GEN e, GEN z, GEN n);
(pari-call-out ell-add "addell" (e z1 z2))
(pari-call-out ell-multiply "powell" (e z n))
(pari-call-out ell-subtract "subell" (e z1 z2))

;; GEN     mathell(GEN e, GEN x, long prec),bilhell(GEN e, GEN z1, GEN z2, long prec);
(pari-call-out-prec ell-height-pairing "bilhell" (e z1 z2))
(pari-call-out-prec ell-height-pairing-gram-matrix "mathell" (e x))

;; GEN     ordell(GEN e, GEN x, long prec),apell(GEN e, GEN pl),apell1(GEN e, GEN p),apell2(GEN e, GEN p);
(pari-call-out ell-l-series-p "apell" (e p))
(pari-call-out-prec ell-y-coordinates "ordell" (e x))

;; GEN     anell(GEN e, long n),akell(GEN e, GEN n);
(pari-call-out ell-l-series-n "akell" (e n))
(pari-call-out ell-l-series "anell" (e (n long)))

;; GEN     localreduction(GEN e, GEN p1), globalreduction(GEN e1);
(pari-call-out ell-global-reduction "globalreduction" (e1) "globalred")
(pari-call-out ell-local-reduction "localreduction" (e p1) "localred")

;; GEN     lseriesell(GEN e, GEN s, GEN N, GEN A, long prec);
(pari-call-out-prec ell-l-series-value "lseriesell" (e s N A))

;; GEN     pointell(GEN e, GEN z, long prec),taniyama(GEN e);
(pari-call-out-prec ell-z-to-xy "pointell" (e z))
(pari-call-out ell-modular-parametrization "taniyama" (e))

;; GEN     orderell(GEN e, GEN p),torsell(GEN e);
(pari-call-out ell-order "orderell" (e p))
(pari-call-out ell-torsion-group "torsell" (e))

;; int     oncurve(GEN e, GEN z);
(pari-call-out (ell-on-curve? boolean) "oncurve" (e z) "isoncurve")

;; void    eulsum(GEN *sum, GEN term, long jterm, GEN *tab, long *dsum, long prec);

;;; /* es.c */

;; void    filtre(char *s),  pariputc(char c), pariputs(char *s), ecrire(GEN x, char format, long dec, long chmp), voir(GEN x, long nb), sor(GEN g, char fo, long dd, long chmp);
;; void    brute(GEN g, char format, long dec), matbrute(GEN g, char format, long dec), texe(GEN g, char format, long dec), etatpile(unsigned int n);
;; void    outerr(GEN x), bruterr(GEN x,char format,long dec),outbeauterr(GEN x);
;; void bruteall(GEN g, char format, long dec, long flbl);

;; #ifdef CLISP_MODULE
;; void pariflush(void);
;; #endif /* CLISP_MODULE */

;; char* gen2str(GEN x);
(pari-call-out (write-to-string c-string :malloc-free) "gen2str" (x) nil)

;; void fprintferr(char* pat, ...);
;; void flusherr();

;; char *gitoascii(GEN g, char *buf);

;; void printvargp(long);
;; extern void (*printvariable)(long);

;; long timer(void),timer2(void);

;;; /* gen1.c */

;; GEN     gadd(GEN x, GEN y),gsub(GEN x, GEN y),gmul(GEN x, GEN y),gdiv(GEN x, GEN y);
(pari-call-out pari+ "gadd" (x y) "+")
(pari-call-out pari- "gsub" (x y) "-")
(pari-call-out pari* "gmul" (x y) "*")
(pari-call-out pari/ "gdiv" (x y) "/")

;;; /* gen2.c gen3.c */

;; GEN     gcopy(GEN x),forcecopy(GEN x),gclone(GEN x),cgetp(GEN x),gaddpex(GEN x, GEN y);
#|
(pari-call-out copy "gcopy" (x) nil)
|#

;; GEN     greffe(GEN x, long l),gopsg2(GEN (*f) (GEN, GEN), long s, GEN y),gopgs2(GEN (*f) (GEN, GEN), GEN y, long s),co8(GEN x, long l),cvtop(GEN x, GEN p, long l),compo(GEN x, long n),gsqr(GEN x);
(pari-call-out component "compo" (x (n long)))
(pari-call-out square "gsqr" (x) "sqr")

;; GEN     gneg(GEN x),gabs(GEN x, long prec);
(pari-call-out pari-minus "gneg" (x) "-")
(pari-call-out-prec pari-abs "gabs" (x) "abs")

;; GEN     gpui(GEN x, GEN n, long prec), gpuigs(GEN x, long n);
(pari-call-out-prec pari-expt "gpui" (x n) "^")
(pari-call-out pari-expt-integer "gpuigs" (x (n long)) "^")

;; GEN     gmax(GEN x, GEN y),gmin(GEN x, GEN y),ginv(GEN x),denom(GEN x),numer(GEN x),lift(GEN x),centerlift(GEN x),vecmax(GEN x),vecmin(GEN x);
(pari-call-out pari-max "gmax" (x y) "max")
(pari-call-out pari-min "gmin" (x y) "min")
(pari-call-out invert "ginv" (x) "^(-1)")
(pari-call-out centerlift "centerlift" (x))
(pari-call-out pari-denominator "denom" (x))
(pari-call-out lift "lift" (x))
(pari-call-out pari-numerator "numer" (x))
(pari-call-out vector-max "vecmax" (x))
(pari-call-out vector-min "vecmin" (x))

;; GEN     gmulsg(long s, GEN y),gdivgs(GEN x, long s),gmodulo(GEN x, GEN y),gmodulcp(GEN x, GEN y),simplify(GEN x);
(pari-call-out make-mod "gmodulcp" (x y) "mod")
(pari-call-out simplify "simplify" (x))

;; GEN     gmod(GEN x, GEN y),gshift(GEN x, long n),gmul2n(GEN x, long n);
(pari-call-out pari-mod "gmod" (x y) "%")
(pari-call-out pari-ash "gshift" (x (n long)) "shift")
(pari-call-out multiply-by-2^n "gmul2n" (x (n long)) "shiftmul")

;; GEN     gsubst(GEN x, long v, GEN y),deriv(GEN x, long v),integ(GEN x, long v),recip(GEN x),ground(GEN x),gcvtoi(GEN x, long *e),grndtoi(GEN x, long *e);
(pari-call-out derivative "deriv" (x (varno long :in :none (get-varno x))))
(pari-call-out integral "integ" (x (varno long :in :none (get-varno x))))
(pari-call-out pws-reverse "recip" (x) "reverse")
(pari-call-out pari-substitute "gsubst" (x (varno long) y) "subst")

;; GEN     gceil(GEN x),gfloor(GEN x),gfrac(GEN x),gtrunc(GEN x),gdivent(GEN x, GEN y),gdiventres(GEN x, GEN y);
(pari-call-out pari-ceiling "gceil" (x) "ceil")
(pari-call-out pari-floor "gfloor" (x) "floor")
(pari-call-out mod-1 "gfrac" (x) "frac")
(pari-call-out quotient "gdivent" (x y) "\\")
(pari-call-out quotient-and-mod "gdiventres" (x y) "divres")
(pari-call-out pari-round "ground" (x) "round")
(pari-call-out pari-truncate "gtrunc" (x) "trunc")

;; GEN     gdivmod(GEN x, GEN y, GEN *pr),geval(GEN x),glt(GEN x, GEN y),gle(GEN x, GEN y),ggt(GEN x, GEN y),gge(GEN x, GEN y),geq(GEN x, GEN y),gne(GEN x, GEN y);
(pari-call-out (pari< pari-bool) "glt" (x y) "<")
(pari-call-out (pari> pari-bool) "ggt" (x y) ">")
(pari-call-out (pari<= pari-bool) "gle" (x y) "<=")
(pari-call-out (pari>= pari-bool) "gge" (x y) ">=")
(pari-call-out (pari= pari-bool) "geq" (x y) "==")
(pari-call-out (pari/= pari-bool) "gne" (x y) "!=")

;; GEN     gand(GEN x, GEN y),gor(GEN x, GEN y),glength(GEN x),matsize(GEN x),truecoeff(GEN x, long n),gtype(GEN x),gsettype(GEN x,long t);
(pari-call-out (pari-and pari-bool) "gand" (x y) "&&")
(pari-call-out (pari-or pari-bool) "gor" (x y) "||")
(pari-call-out coefficient "truecoeff" (x (n long)) "coeff")
(pari-call-out pari-length "glength" (x) "length")
(pari-call-out matrix-size "matsize" (x))

;; GEN     gtopoly(GEN x, long v),gtopolyrev(GEN x, long v),gtoser(GEN x, long v),gtovec(GEN x),dbltor(double x);
;; Be consistent and take 'constant term first' as the normal thing ...
(pari-call-out convert-to-polynomial "gtopolyrev" (x (varno long :in :none 0)) "polyrev")
(pari-call-out convert-to-polynomial-reverse "gtopoly"
  (x (varno long :in :none 0)) "poly")
(pari-call-out convert-to-pws "gtoser" (x (varno long :in :none 0)) "series")
(pari-call-out convert-to-vector "gtovec" (x) "vec")

;; GEN     karamul(GEN x, GEN y, long k), mpkaramul(GEN x, GEN y, long k);

;; GEN     gdivround(GEN x, GEN y), gpolvar(GEN y);
(pari-call-out pari-round2 "gdivround" (x y) "\\/")

;; void    gop0z(GEN (*f) (void), GEN x),gop1z(GEN (*f) (GEN), GEN x, GEN y),gop2z(GEN (*f) (GEN, GEN), GEN x, GEN y, GEN z),gops2gsz(GEN (*f) (GEN, long), GEN x, long s, GEN z),gops2sgz(GEN (*f) (long, GEN), long s, GEN y, GEN z),gops2ssz(GEN (*f) (long, long), long s, long y, GEN z);

;; void    gop3z(GEN (*f) (GEN, GEN, GEN), GEN x, GEN y, GEN z, GEN t),gops1z(GEN (*f) (long), long s, GEN y),gopsg2z(GEN (*f) (GEN, GEN), long s, GEN y, GEN z),gopgs2z(GEN (*f) (GEN, GEN), GEN y, long s, GEN z),gaffsg(long s, GEN x),gaffect(GEN x, GEN y);

;; void    normalize(GEN *px),normalizepol(GEN *px);

;; int     gcmp0(GEN x),gcmp1(GEN x),gcmp_1(GEN x),gcmp(GEN x, GEN y),lexcmp(GEN x, GEN y),gegal(GEN x, GEN y),polegal(GEN x, GEN y),vecegal(GEN x, GEN y),gsigne(GEN x);
(pari-call-out (compare boolean) "gcmp" (x y) "?")
(pari-call-out (zero? boolean) "gcmp0" (x) "?")
(pari-call-out (one? boolean) "gcmp1" (x) "?")
(pari-call-out (minus-one? boolean) "gcmp_1" (x) "?")
(pari-call-out (equal? boolean) "gegal" (x y) "==")
(pari-call-out (compare-lex boolean) "lexcmp" (x y) "lex")
(pari-call-out (pari-sign int) "gsigne" (x) "sign")

;; int     gvar(GEN x),gvar2(GEN x),tdeg(GEN x),precision(GEN x),gprecision(GEN x),ismonome(GEN x),iscomplex(GEN x),isexactzero(GEN g);
(pari-call-out (eql-0? boolean) "isexactzero" (g))
(pari-call-out (varno int) "gvar" (x) "?")

(defun get-varno (x)
  (let ((vn (pari-varno (convert-to-pari x))))
    (if (< vn 256) vn 0)))

;; long    padicprec(GEN x, GEN p);
(pari-call-out (get-padic-precision long) "padicprec" (x p))

;; long    opgs2(int (*f) (GEN, GEN), GEN y, long s);

;; long    taille(GEN x),taille2(GEN x),gexpo(GEN x),gtolong(GEN x),ggval(GEN x, GEN p),rounderror(GEN x),gsize(GEN x),pvaluation(GEN x, GEN p, GEN *py);
(pari-call-out (pari-bytesize long) "taille2" (x) "bytesize")
(pari-call-out (rounderror long) "rounderror" (x))
(pari-call-out (size long) "gsize" (x) "size")
(pari-call-out (valuation long) "ggval" (x p) "valuation")

;; double  rtodbl(GEN x), gtodouble(GEN x);

;;; /* init.c */

  ;; GEN     newbloc(long n),geni(void);
  ;; GEN     allocatemem(ulong newsize);
  ;; long    marklist(void);
  ;; #ifdef __cplusplus
  ;; void    init(long parisize, long maxprime, void (*printvar)(long)=printvargp);
  ;; #else
;; void    init(long parisize, long maxprime);
(def-call-out pari-init
  (:name "init_for_clisp")
  (:return-type nil)
  (:arguments (parisize long) (maxprime long))
  (:language :stdc))

  ;; #endif
  ;; void    freeall(void), killall(void);
  ;;
  ;; void    killbloc(GEN x),newvalue(entree *ep, GEN val),killvalue(entree *ep);
  ;; #ifdef __cplusplus
  ;; extern "C" void    err(long numerr, ...);
  ;; #else
  ;; extern
  ;; #ifdef __GNUC__
  ;; __volatile__
  ;; #endif
  ;; void    err(long numerr, ...);
  ;; #endif
  ;;
  ;; void    recover(long listloc),changevalue(entree *ep, GEN val),allocatemoremem(ulong newsize);

;;; /* polarit.c */
;; GEN     ginvmod(GEN x, GEN y),gred(GEN x),gdeuc(GEN x, GEN y),gres(GEN x, GEN y),poldivres(GEN x, GEN y, GEN *pr);

;; GEN     poleval(GEN x, GEN y),roots(GEN x, long l),roots2(GEN pol,long PREC),rootslong(GEN x, long l),ggcd(GEN x, GEN y),gbezout(GEN x, GEN y, GEN *u, GEN *v),vecbezout(GEN x, GEN y),glcm(GEN x, GEN y);
(pari-call-out pari-xgcd "vecbezout" (x y) "bezout")
(pari-call-out pari-gcd "ggcd" (x y) "gcd")
(pari-call-out pari-lcm "glcm" (x y) "lcm")
(pari-call-out-prec complex-roots "roots" (x))
(pari-call-out-prec complex-roots-robust "rootslong" (x))

;; GEN     subresext(GEN x, GEN y, GEN *U, GEN *V),vecbezoutres(GEN x, GEN y);

;; GEN     polgcd(GEN x, GEN y),srgcd(GEN x, GEN y),polgcdnun(GEN x, GEN y),content(GEN x),primpart(GEN x),psres(GEN x, GEN y),factmod9(GEN f, GEN p, GEN a);
(pari-call-out content "content" (x))
(pari-call-out factor-in-fq "factmod9" (f p a) "factfq")

;; GEN     factmod(GEN f, GEN p),factmod2(GEN f, GEN p),factmod_gen(GEN f, GEN p),rootmod(GEN f, GEN p),rootmod2(GEN f, GEN p),decpol(GEN x, long klim),factor(GEN x),gisirreducible(GEN x);
(pari-call-out factor-in-fp "factmod" (f p))
(pari-call-out factor "factor" (x))
(pari-call-out (irreducible? pari-bool) "gisirreducible" (x) "isirreducible")
(pari-call-out mod-p-roots "rootmod" (f p))
(pari-call-out mod-p-roots-small "rootmod2" (f p))

;; GEN     factpol(GEN x, long klim, long hint),factpol2(GEN x, long klim),simplefactmod(GEN f, GEN p), factcantor(GEN x, GEN p);
(pari-call-out factor-cantor-zassenhaus "factcantor" (x p))
(pari-call-out factor-degrees "simplefactmod" (f p))
(pari-call-out factor-poly-hensel "factpol" (x (klim long) (hint long)))
(pari-call-out factor-poly-complex "factpol2" (x (klim long)) "?")

;; GEN     subres(GEN x, GEN y),discsr(GEN x),quadpoly(GEN x),quadgen(GEN x),quaddisc(GEN x),bezoutpol(GEN a, GEN b, GEN *u, GEN *v),polinvmod(GEN x, GEN y);
(pari-call-out make-quad "quadgen" (x))
(pari-call-out quad-minimal-polynomial "quadpoly" (x))
(pari-call-out quad-discriminant "quaddisc" (x))
(pari-call-out discriminant "discsr" (x) "disc")
(pari-call-out resultant "subres" (x y) "resultant")

;; GEN     resultant2(GEN x, GEN y),sylvestermatrix(GEN x,GEN y),polfnf(GEN a, GEN t),nfiso(GEN a, GEN b),nfincl(GEN a, GEN b),isisomfast(GEN nf1, GEN nf2, long prec),isinclfast(GEN nf1, GEN nf2, long prec);
(pari-call-out nf-factor "polfnf" (a b) "factornf")
(pari-call-out nf-field-inclusion? "nfincl" (a b) "isincl")
(pari-call-out nf-field-isomorphic? "nfiso" (a b) "isisom")
(pari-call-out resultant-sylvester "resultant2" (x y))
(pari-call-out sylvester-matrix "sylvestermatrix" (x y))

;; GEN     newtonpoly(GEN x, GEN p),apprgen(GEN f, GEN a),apprgen9(GEN f, GEN a),rootpadic(GEN f, GEN p, long r),rootpadicfast(GEN f, GEN p, long r, long flall),gcvtop(GEN x, GEN p, long r),factorpadic2(GEN x, GEN p, long r);
(pari-call-out newton-polygon "newtonpoly" (x p))
(pari-call-out lift-padic-roots "apprgen9" (f a) "apprpadic")
(pari-call-out padic-roots "rootpadic"
  (f p (prec long :in :none pari-padic-precision)))

;; GEN     factorpadic4(GEN x, GEN p, long r),nilordpadic(GEN p,long r,GEN fx,long mf,GEN gx),Decomppadic(GEN p,long r,GEN f,long mf,GEN theta,GEN chi,GEN nu),squarefree(GEN f);
(pari-call-out factor-padic "factorpadic4"
  (x p (prec long :in :none pari-padic-precision)) "factorpadic")

;; long    sturm(GEN x),sturmpart(GEN x, GEN a, GEN b);
(pari-call-out (count-real-roots long) "sturm" (x))
(pari-call-out (count-real-roots-between long) "sturmpart" (x a b))

;; int     poldivis(GEN x, GEN y, GEN *z),gdivise(GEN x, GEN y);

;; void    gredsp(GEN *px),split(long m, GEN *t, long d, long p, GEN q),split9(GEN m, GEN *t, long d, long p, GEN q, GEN unfq, GEN qq, GEN a),splitgen(GEN m, GEN *t,long d,GEN p, GEN q);

;; int     issimplefield(GEN x),isinexactfield(GEN x);

;;; /* trans.c */

;; GEN     greal(GEN x),gimag(GEN x),teich(GEN x),agm(GEN x, GEN y, long prec),palog(GEN x);
(pari-call-out pari-imagpart "gimag" (x) "imag")
(pari-call-out pari-realpart "greal" (x) "real")
(pari-call-out-prec arithmetic-geometric-mean "agm" (x y))
(pari-call-out-prec teichmueller "teich" (x))

;; GEN     mpsqrt(GEN x),gsqrt(GEN x, long prec);
(pari-call-out-prec pari-sqrt "gsqrt" (x) "sqrt")

;; GEN     gexp(GEN x, long prec);
(pari-call-out-prec pari-exp "gexp" (x) "exp")

;; GEN     mplog(GEN x),glog(GEN x, long prec);
(pari-call-out-prec pari-log "glog" (x) "log")

;; GEN     mpexp1(GEN x),mpexp(GEN x);

;; GEN     logagm(GEN q),glogagm(GEN x, long prec);

;; GEN     mpsc1(GEN x, long *ptmod8),mpcos(GEN x),gcos(GEN x, long prec),mpsin(GEN x),gsin(GEN x, long prec);
(pari-call-out-prec pari-cos "gcos" (x) "cos")
(pari-call-out-prec pari-sin "gsin" (x) "sin")

;; GEN     mpaut(GEN x),mptan(GEN x),gtan(GEN x, long prec),mpatan(GEN x),gatan(GEN x, long prec),mpasin(GEN x),gasin(GEN x, long prec);
(pari-call-out-prec pari-arcsin "gasin" (x) "asin")
(pari-call-out-prec pari-arctan "gatan" (x) "atan")
(pari-call-out-prec pari-tan "gtan" (x) "tan")

;; GEN     mpacos(GEN x),gacos(GEN x, long prec),mparg(GEN x, GEN y),mpch(GEN x),gch(GEN x, long prec),mpsh(GEN x),gsh(GEN x, long prec);
(pari-call-out-prec pari-arccos "gacos" (x) "acos")
(pari-call-out-prec pari-cosh "gch" (x) "cosh")
(pari-call-out-prec pari-sinh "gsh" (x) "sinh")

;; GEN     mpth(GEN x),gth(GEN x, long prec),mpath(GEN x),gath(GEN x, long prec),mpash(GEN x),gash(GEN x, long prec);
(pari-call-out-prec pari-arsinh "gash" (x) "asinh")
(pari-call-out-prec pari-artanh "gath" (x) "atanh")
(pari-call-out-prec pari-tanh "gth" (x) "tanh")

;; GEN     garg(GEN x, long prec),sarg(GEN x, GEN y, long prec),mppsi(GEN z),gpsi(GEN x, long prec),transc(GEN (*f) (GEN, long), GEN x, long prec),kbessel(GEN nu, GEN gx, long prec),hyperu(GEN a, GEN b, GEN gx, long prec);
(pari-call-out-prec pari-argument "garg" (x) "arg")
(pari-call-out-prec bessel-k "kbessel" (nu x))
(pari-call-out-prec hypergeometric-u "hyperu" (a b x))
(pari-call-out-prec psi "gpsi" (x) "psi")

;; GEN     cxpsi(GEN z, long prec),jbesselh(GEN n, GEN z, long prec),gzeta(GEN x, long prec);
(pari-call-out-prec bessel-j-half "jbesselh" (n z))
(pari-call-out-prec riemann-zeta "gzeta" (x) "zeta")

;; GEN     kbessel2(GEN nu, GEN x, long prec),eint1(GEN x, long prec),gerfc(GEN x, long prec),eta(GEN x, long prec),jell(GEN x, long prec),wf2(GEN x, long prec),wf(GEN x, long prec);
(pari-call-out-prec exponential-integral-1 "eint1" (x))
(pari-call-out-prec erfc "erfc" (x))
(pari-call-out-prec dedekind-eta "eta" (x))
(pari-call-out-prec elliptic-j "jell" (x))
(pari-call-out-prec weber-f "wf" (x))
(pari-call-out-prec weber-f2 "wf2" (x))

;; GEN     incgam(GEN a, GEN x, long prec),incgam1(GEN a, GEN x, long prec),incgam2(GEN a, GEN x, long prec),incgam3(GEN a, GEN x, long prec),incgam4(GEN a, GEN x, GEN z, long prec),bernreal(long n, long prec),bernvec(long nomb);
(pari-call-out-prec bernoulli-real "bernreal" ((n long)))
(pari-call-out bernoulli-vector "bernvec" ((nomb long)))
(pari-call-out-prec incomplete-gamma "incgam" (a x))
(pari-call-out-prec complementary-incomplete-gamma "incgam3" (a x))

;; GEN     mpach(GEN x),gach(GEN x, long prec),mpgamma(GEN x),cxgamma(GEN x, long prec),ggamma(GEN x, long prec),mpgamd(long x, long prec),ggamd(GEN x, long prec),mppi(long prec);
(pari-call-out-prec pari-arcosh "gach" (x) "acosh")
(pari-call-out-prec gamma-shift-1/2 "ggamd" (x) "gamh")
(pari-call-out-prec gamma "ggamma" (x) "gamma")
(pari-call-out-prec pari-pi "mppi" () "pi")

(define-symbol-macro pari-pi (pari-pi))

;; GEN     mpeuler(long prec),polylog(long m, GEN x, long prec),dilog(GEN x, long prec),polylogd(long m, GEN x, long prec),polylogdold(long m, GEN x, long prec),polylogp(long m, GEN x, long prec),gpolylog(long m, GEN x, long prec);
(pari-call-out-prec dilog "dilog" (x))
(pari-call-out-prec euler "mpeuler" () "euler")
(pari-call-out-prec polylog "gpolylog" ((m long) x) "polylog")
(pari-call-out-prec polylog-d "polylogd" ((m long) x))
(pari-call-out-prec polylog-d-old "polylogdold" ((m long) x))
(pari-call-out-prec polylog-p "polylogp" ((m long) x))

(define-symbol-macro euler (euler))

;; GEN     theta(GEN q, GEN z, long prec),thetanullk(GEN q, long k, long prec),mplngamma(GEN x),cxlngamma(GEN x, long prec),glngamma(GEN x, long prec),izeta(GEN x, long prec);
(pari-call-out-prec log-gamma "glngamma" (x) "lngamma")

;; void    constpi(long prec),consteuler(long prec),mpbern(long nomb, long prec),gsincos(GEN x, GEN *s, GEN *c, long prec);
#|
(pari-call-out-prec (precompute-euler nil) "consteuler" () "euler")
(pari-call-out-prec (precompute-pi nil) "constpi" () "pi")
|#

;; void    gsqrtz(GEN x, GEN y),gexpz(GEN x, GEN y),glogz(GEN x, GEN y),gcosz(GEN x, GEN y),gsinz(GEN x, GEN y),mpsincos(GEN x, GEN *s, GEN *c),gtanz(GEN x, GEN y);

;; void    gatanz(GEN x, GEN y),gasinz(GEN x, GEN y),gacosz(GEN x, GEN y),gchz(GEN x, GEN y),gshz(GEN x, GEN y),gthz(GEN x, GEN y),gashz(GEN x, GEN y),gachz(GEN x, GEN y);

;; void    gathz(GEN x, GEN y),ggammaz(GEN x, GEN y),glngammaz(GEN x, GEN y),mpgamdz(long s, GEN y),ggamdz(GEN x, GEN y),gpsiz(GEN x, GEN y),gzetaz(GEN x, GEN y);

;; void    gpolylogz(long m, GEN x, GEN y);

;;; /* version.c */

;; GEN     gerepilc(GEN l, GEN p, GEN q);
;; void    gerepilemany(long ltop, GEN* const gptr[], long nptr);
  ;; void    printversion(void), printversionno(void);

  ;; typedef struct PariOUT {
  ;;   void (*putc)(char);
  ;;   void (*puts)(char*);
  ;; #ifdef CLISP_MODULE
  ;;   void (*flush)(void);
  ;; #endif /* CLISP_MODULE */
  ;; } PariOUT;

  ;; typedef struct PariERR {
  ;;   void (*putc)(char);
  ;;   void (*puts)(char*);
  ;;   void (*flush)();
  ;;   void (*die)();
  ;; } PariERR;

  ;; extern PariOUT *pariOut;
  ;; extern PariERR *pariErr;

;;; mpdefs.h

(defmacro extract0 ((var x) &body body)
  `(progn
     (setf temp ,x)
     (symbol-macrolet ((,var (deref (cast temp '(c-ptr ulong)))))
       ,@body)))
(defmacro extract1 ((var x) &body body)
  `(progn
     (setf temp ,x)
     (symbol-macrolet
       ((,var (element (deref (cast temp '(c-ptr (c-array ulong 2)))) 1)))
       ,@body)))

;; #define signe(x)          (((long)((GEN)(x))[1])>>SIGNSHIFT)
(defun pari-sign-raw (x)
  (extract1 (elt1 x)
    (ecase (ldb pari-sign-byte elt1)
      (0 0) (1 1) (255 -1))))

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

  ;; #define pere(x)           ((ulong)(((GEN)(x))[0]&PEREBITS)>>PERESHIFT)
  ;; #define setpere(x,s)      (((GEN)(x))[0]=(((GEN)(x))[0]&(~PEREBITS))+(((ulong)(s))<<PERESHIFT))
;; #define lg(x)             ((long)(((GEN)(x))[0]&LGBITS))
(defun pari-length-raw (x)
  (extract0 (elt0 x)
    (ldb pari-length-byte elt0)))

  ;; #define setlg(x,s)        (((GEN)(x))[0]=(((GEN)(x))[0]&(~LGBITS))+(s))
;; #define lgef(x)           ((long)(((GEN)(x))[1]&LGEFBITS))
(defun pari-effective-length-raw (x)
  (extract1 (elt1 x)
    (ldb pari-effective-length-byte elt1)))

  ;; #define setlgef(x,s)      (((GEN)(x))[1]=(((GEN)(x))[1]&(~LGEFBITS))+(s))
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
    (ldb  pari-precision-byte elt1)))

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
(defun pari-mantissa-eff (x)
  (let ((len (pari-effective-length-raw x)))
    ;; x is still in temp here
    (incf (cast temp 'ulong) 8)
    (deref (cast temp `(c-ptr (c-array ulong ,(- len 2)))))))

(defun pari-mantissa (x)
  (let ((len (pari-length-raw x)))
    ;; x is still in temp here
    (incf (cast temp 'ulong) 8)
    (deref (cast temp `(c-ptr (c-array ulong ,(- len 2)))))))

;; #define setmant(x,i,s)    (((GEN)(x))[i+1]=s)

(defun pari-set-component (obj i ptr)
  (setf temp obj)
  (incf (cast temp 'ulong) (* 4 i))
  (setf (deref (cast temp '(c-ptr pari-gen))) ptr))

;;; mpansi.h

;; GEN cgeti(long x),cgetr(long x),stoi(long x);

;; GEN cgetg(long x, long y),negi(GEN x),negr(GEN x),absi(GEN x),absr(GEN x);
(def-call-out pari-cgetg
  (:name "cgetg")
  (:return-type pari-gen)
  (:arguments (x long) (y long))
  (:language :stdc))

;;; /* mp.c ou mp.s */

;; GEN     gerepile(long l, long p, GEN q), icopy(GEN x), rcopy(GEN x);
#|
(def-call-out pari-gerepile
  (:name "gerepile")
  (:return-type pari-gen)
  (:arguments (l long) (p long) (q pari-gen :in :none))
  (:language :stdc))
|#

;;;; Conversion CLISP --> pari and pari --> CLISP

;; Make a vector of ulongs into a pari object (including the second codeword,
;; which is element 0 of the vector). type is the pari type code.
(defun pari-make-object (vec type)
  (let ((obj (pari-cgetg (1+ (length vec)) type)))
    (setf temp obj)
    (incf (cast temp 'ulong) 4)
    (setf (deref (cast temp `(c-ptr (c-array ulong ,(length vec))))) vec)
    obj))

;;; Define some CLISP analogs for pari types

(export '(pari-object internal-pari-object))

(defclass pari-object () ()
  (:documentation "An abstract class for CLISP equivalents of pari objects"))

(defgeneric convert-to-pari (x)
  (:documentation
    "Converts suitable CLISP objects into internal pari objects"))

(defclass internal-pari-object (pari-object)
  ((pointer :accessor pari-class-pointer :initarg :pointer))
  (:documentation "Pari object as a pointer into the pari stack"))

(defun make-internal-pari-object (ptr)
  (make-instance 'internal-pari-object :pointer ptr))

(defmethod convert-to-pari ((x internal-pari-object))
  (pari-class-pointer x))

;;; Make internal pari objects printable and readable

(defmethod print-object ((x internal-pari-object) stream)
  (format stream "#Z\"~A\"" (pari-write-to-string (pari-class-pointer x)))
  x)

(defun pari-reader (stream subchar arg)
  (declare (ignore subchar))
  (when arg (error "~S: Between # and Z no number is allowed." 'read))
  (let ((str (read stream t nil t)))
    (unless (stringp str)
      (error "~S: After #Z a string must follow." 'read))
    (make-instance 'internal-pari-object
      :pointer
      (pari-read-from-string
        (remove-if #'(lambda (c) (member c '(#\Space #\Tab #\Newline)))
	           str)))))

(set-dispatch-macro-character #\# #\Z #'pari-reader)

;;; Some helper macros for defining classes for pari objects

(eval-when (compile load eval)
  (defun make-accessors (slots)
    (let ((pari-package (find-package "PARI")))
      (mapcar #'(lambda (slot)
		  (intern (format nil "PARI-CLASS-~A" slot) pari-package))
	      slots)))
  (defun make-initargs (slots)
    (let ((keyword-package (find-package "KEYWORD")))
      (mapcar #'(lambda (slot)
		  (intern (format nil "~A" slot) keyword-package))
	      slots)))
  (defun dpc-class (name slots accessors initargs)
    `(progn
       (export '(,name ,@accessors))
       (defclass ,name (pari-object)
	         ,(mapcar #'(lambda (slot accessor initarg)
			      `(,slot :accessor ,accessor :initarg ,initarg))
			  slots accessors initargs))
       (defmethod print-object ((x ,name) stream)
         (if *print-readably*
	   (format stream ,(format nil "#.(~~S '~~S~{ ~S '~~S~})" initargs)
	           'make-instance ',name
		   ,@(mapcar #'(lambda (acc) `(,acc x)) accessors))
	   (format stream ,(format nil "#<~~S~{ ~S ~~S~}>" initargs)
	           ',name
		   ,@(mapcar #'(lambda (acc) `(,acc x)) accessors))))))
  (defun dpc-to-pari (typecode name slots accessors)
    `(defmethod convert-to-pari ((x ,name))
	(let ((obj (pari-cgetg ,(+ 1 (length slots)) ,typecode)))
	  ,@(let ((count 0))
	      (mapcar #'(lambda (accessor)
			  `(pari-set-component obj ,(incf count)
			    (convert-to-pari (,accessor x))))
		      accessors))
	  obj)))
  (defun dpc-from-pari (typecode name initargs)
    `(defun ,(intern (format nil "CONVERT-FROM-PARI-~D" typecode)
		      (find-package "PARI"))
	    (ptr)
	(make-instance ',name
	  ,@(let ((count 0))
	      (mapcan #'(lambda (initarg)
			  `(,initarg (convert-from-pari
				      (pari-component ptr ,(incf count)))))
		      initargs))))))

(defmacro define-pari-class-only (name slots)
  (dpc-class name slots (make-accessors slots) (make-initargs slots)))

(defmacro define-pari-class-0 (typecode name slots)
  (let ((accessors (make-accessors slots))
	(initargs (make-initargs slots)))
    `(progn
       ,(dpc-class name slots accessors initargs)
       ,(dpc-to-pari typecode name slots accessors))))

(defmacro define-pari-class (typecode name slots)
  (let ((accessors (make-accessors slots))
	(initargs (make-initargs slots)))
    `(progn
       ,(dpc-class name slots accessors initargs)
       ,(dpc-to-pari typecode name slots accessors)
       ,(dpc-from-pari typecode name initargs))))

;; Type 1: integers -- represented by CLISP integers

(defmethod convert-to-pari ((x (eql 0)))
  pari-0)

(defmethod convert-to-pari ((x (eql 1)))
  pari-1)

(defmethod convert-to-pari ((x (eql 2)))
  pari-2)

(defmethod convert-to-pari ((x integer))
  (let* ((sign (signum x))
         (val (abs x))
	 (len (ceiling (integer-length val) 32))
	 (vec (make-array (1+ len))))
    (setf (svref vec 0)
          (dpb sign pari-sign-byte
	       (dpb (+ len 2) pari-effective-length-byte 0)))
    (do ((i len (1- i))
         (y val (ash y -32)))
        ((eql i 0))
      (setf (svref vec i) (logand y #xFFFFFFFF)))
    (pari-make-object vec 1)))

(defun convert-from-pari-1 (ptr)
  (let* ((sign (pari-sign-raw ptr))
	 (mant (pari-mantissa-eff ptr))
	 (result 0))
    (dotimes (i (length mant) (* sign result))
      (setq result (+ (* result #x100000000) (svref mant i))))))

;; Type 2: real numbers -- represented by CLISP floats

(defmethod convert-to-pari ((x float))
  (if (= x 0)
    (pari-make-object
      (vector (- pari-exponent-offset (* 32 pari-real-prec-raw) -61) 0) 2)
    (multiple-value-bind (signif expo sign) (integer-decode-float x)
      (let ((pr (float-precision x)))
        ;; need ceil(pr/32) mantissa words,
	;; signif has to be scaled by 2^(32*ceil(pr/32)-pr)
	;; and the exponent will be pr+expo-1
	(multiple-value-bind (q r) (ceiling pr 32)
	  (setq signif (ash signif (- r)))
	  (let ((vec (make-array (1+ q))))
	    (setf (svref vec 0)
	      (dpb sign pari-sign-byte
	           (dpb (+ pari-exponent-offset pr expo -1)
		        pari-exponent-byte 0)))
	    (do ((i q (1- i))
	         (y signif (ash y -32)))
		((eql i 0))
	      (setf (svref vec i) (logand y #xFFFFFFFF)))
	    (pari-make-object vec 2)))))))

(defun convert-from-pari-2 (ptr)
  (let* ((sign (pari-sign-raw ptr))
         (expo (pari-exponent-raw ptr))
	 (mant (pari-mantissa ptr))
	 (signif 0))
    (dotimes (i (length mant))
      (setq signif (+ (* signif #x100000000) (svref mant i))))
    (* sign (scale-float (float signif (float-digits 1 (* 32 (length mant))))
                         (- expo (* 32 (length mant)) -1)))))

;; Type 3: integermods

(define-pari-class 3 pari-integermod (modulus rep))

;; Type 4,5: rational numbers -- represented by CLISP ratios

(defmethod convert-to-pari ((x (eql 1/2)))
  pari-1/2)

(defmethod convert-to-pari ((x ratio))
  (let ((obj (pari-cgetg 3 4)))
    (pari-set-component obj 1 (convert-to-pari (numerator x)))
    (pari-set-component obj 2 (convert-to-pari (denominator x)))
    obj))

(defun convert-from-pari-4 (ptr)
  (/ (convert-from-pari (pari-component ptr 1))
     (convert-from-pari (pari-component ptr 2))))

;; Type 6: complex numbers -- represented by CLISP complex if possible

(define-pari-class-0 6 pari-complex (realpart imagpart))

(defmethod convert-to-pari ((x (eql #C(0 1))))
  pari-i)

(defmethod convert-to-pari ((x complex))
  (let ((obj (pari-cgetg 3 6)))
    (pari-set-component obj 1 (convert-to-pari (realpart x)))
    (pari-set-component obj 2 (convert-to-pari (imagpart x)))
    obj))

(defun convert-from-pari-6 (ptr)
  (if (and (member (pari-type-raw (pari-component ptr 1)) '(1 2 4 5))
           (member (pari-type-raw (pari-component ptr 2)) '(1 2 4 5)))
    ;; CLISP complex is possible
    (complex (convert-from-pari (pari-component ptr 1))
             (convert-from-pari (pari-component ptr 2)))
    ;; must construct pari-complex
    (make-instance 'pari-complex
      :realpart (convert-from-pari (pari-component ptr 1))
      :imagpart (convert-from-pari (pari-component ptr 2)))))

;; Type 7: p-adic numbers

(define-pari-class-only pari-padic (precp valp prime prpow rep))

(defmethod convert-to-pari ((x pari-padic))
  (let ((obj (pari-cgetg 5 7)))
    (extract1 (elt1 obj)
      (setf elt1 (dpb (pari-class-precp x) pari-precision-byte
                      (dpb (+ (pari-class-valp x) pari-valuation-offset)
		           pari-valuation-byte 0))))
    (pari-set-component obj 2 (convert-to-pari (pari-class-prime x)))
    (pari-set-component obj 3 (convert-to-pari (pari-class-prpow x)))
    (pari-set-component obj 4 (convert-to-pari (pari-class-rep x)))
    obj))

(defun convert-from-pari-7 (ptr)
  (make-instance 'pari-padic
    :precp (pari-precision-raw ptr)
    :valp  (pari-valuation-raw ptr)
    :prime (convert-from-pari (pari-component ptr 1))
    :prpow (convert-from-pari (pari-component ptr 2))
    :rep   (convert-from-pari (pari-component ptr 3))))

;; Type 8: quadratic numbers

(define-pari-class 8 pari-quadratic (poly realpart imagpart))

;; Type 9: polymods

(define-pari-class 9 pari-polymod (modulus rep))

;; Type 10: polynomials

(define-pari-class-only pari-poly (s varno coeffs))

(defmethod convert-to-pari ((x pari-poly))
  (let* ((coeffs (pari-class-coeffs x))
         (obj (pari-cgetg (+ 2 (length coeffs)) 10)))
    (extract1 (elt1 obj)
      (setf elt1
            (dpb (pari-class-s x) pari-sign-byte
                 (dpb (pari-class-varno x) pari-varno-byte
                      (dpb (+ 2 (length coeffs))
		           pari-effective-length-byte 0)))))
    (dotimes (i (length coeffs) obj)
      (pari-set-component obj (+ i 2) (convert-to-pari (svref coeffs i))))))

(defun convert-from-pari-10 (ptr)
  (let ((s (pari-sign-raw ptr))
        (varno (pari-varno-raw ptr))
	(coeffs (pari-mantissa-eff ptr)))
    (dotimes (i (length coeffs))
      (setf (cast temp 'ulong) (svref coeffs i))
      (setf (svref coeffs i) (convert-from-pari temp)))
    (make-instance 'pari-poly :s s :varno varno :coeffs coeffs)))

;; Type 11: power series

(define-pari-class-only pari-pws (s varno expo coeffs))

(defmethod convert-to-pari ((x pari-pws))
  (let* ((coeffs (pari-class-coeffs x))
         (obj (pari-cgetg (+ 2 (length coeffs)) 11)))
    (extract1 (elt1 obj)
      (setf elt1
            (dpb (pari-class-s x) pari-sign-byte
                 (dpb (pari-class-varno x) pari-varno-byte
                      (dpb (+ (pari-class-expo x) pari-valuation-offset)
		           pari-valuation-byte 0)))))
    (dotimes (i (length coeffs) obj)
      (pari-set-component obj (+ i 2) (convert-to-pari (svref coeffs i))))))

(defun convert-from-pari-11 (ptr)
  (let ((s (pari-sign-raw ptr))
        (varno (pari-varno-raw ptr))
	(expo (pari-valuation-raw ptr))
	(coeffs (pari-mantissa ptr)))
    (dotimes (i (length coeffs))
      (setf (cast temp 'ulong) (svref coeffs i))
      (setf (svref coeffs i) (convert-from-pari temp)))
    (make-instance 'pari-pws :s s :varno varno :expo expo :coeffs coeffs)))

;; Type 13,14: rational functions

(define-pari-class 13 pari-ratfun (numer denom))

;; Type 15: indefinite binary quadratic forms

(define-pari-class 15 pari-real-qf (a b c))

;; Type 16: definite binary quadratic forms

(define-pari-class 16 pari-imag-qf (a b c))

;; Type 17,18: (row and column) vectors -- represented by CLISP vectors
;; #(:row v1 v2 ... vn) <---> row vector
;; #(:col v1 v2 ... vn) <---> column vector
;; #(v1 v2 ... vn)       ---> row vector
(defmethod convert-to-pari ((x vector))
  (if (and (plusp (length x)) (member (svref x 0) '(:row :col) :test #'eq))
    (let ((obj (pari-cgetg (length x) (case (svref x 0) (:row 17) (:col 18)))))
      (do ((i (1- (length x)) (1- i)))
          ((eql i 0) obj)
        (pari-set-component obj i (convert-to-pari (svref x i)))))
    (let ((obj (pari-cgetg (1+ (length x)) 17)))
      (dotimes (i (length x) obj)
        (pari-set-component obj (1+ i) (convert-to-pari (svref x i)))))))

(defun convert-from-pari-17 (ptr)
  (let* ((len (1- (pari-length-raw ptr)))
         (vec (make-array (1+ len))))
    (setf (svref vec 0) :row)
    (do ((i len (1- i)))
        ((eql i 0) vec)
      (setf (svref vec i) (convert-from-pari (pari-component ptr i))))))

(defun convert-from-pari-18 (ptr)
  (let* ((len (1- (pari-length-raw ptr)))
         (vec (make-array (1+ len))))
    (setf (svref vec 0) :col)
    (do ((i len (1- i)))
        ((eql i 0) vec)
      (setf (svref vec i) (convert-from-pari (pari-component ptr i))))))

;; Type 19: matrices -- represented by CLISP 2-dim arrays

(defmethod convert-to-pari ((x array))
  (unless (eql (array-rank x) 2)
    (error "~S: Array ~S is not 2-dimensional." 'convert-to-pari x))
  (let ((obj (pari-cgetg (1+ (array-dimension x 1)) 19)))
    (dotimes (j (array-dimension x 1) obj)
      (let ((col (pari-cgetg (1+ (array-dimension x 0)) 18)))
        (dotimes (i (array-dimension x 0))
	  (pari-set-component col (1+ i) (convert-to-pari (aref x i j))))
	(pari-set-component obj (1+ j) col)))))

(defun convert-from-pari-19 (ptr)
  (let ((cols (1- (pari-length-raw ptr))))
    (if (eql cols 0)
      (make-array '()) ; probably shouldn't happen...
      (let* ((rows (1- (pari-length-raw (pari-component ptr 1))))
             (arr (make-array (list rows cols))))
	(dotimes (j cols arr)
	  (let ((col (pari-component ptr (1+ j))))
	    (unless (eql (1- (pari-length-raw col)) rows)
	      (error "~S: Pari matrix has columns of unequal length."
	             'convert-from-pari))
	    (dotimes (i rows)
	      (setf (aref arr i j)
	            (convert-from-pari (pari-component col (1+ i)))))))))))

;;; Conversion from pari -- dispatch

(defun convert-from-pari (ptr)
    "Converts an internal pari object to a CLISP object"
  (case (pari-type-raw ptr)
    (1 (convert-from-pari-1 ptr))
    (2 (convert-from-pari-2 ptr))
    (3 (convert-from-pari-3 ptr))
    ((4 5) (convert-from-pari-4 ptr))
    (6 (convert-from-pari-6 ptr))
    (7 (convert-from-pari-7 ptr))
    (8 (convert-from-pari-8 ptr))
    (9 (convert-from-pari-9 ptr))
    (10 (convert-from-pari-10 ptr))
    (11 (convert-from-pari-11 ptr))
    ((13 14) (convert-from-pari-13 ptr))
    (15 (convert-from-pari-15 ptr))
    (16 (convert-from-pari-16 ptr))
    (17 (convert-from-pari-17 ptr))
    (18 (convert-from-pari-18 ptr))
    (19 (convert-from-pari-19 ptr))
    (t (error "~S: Pari type ~D not yet implemented as a CLISP type."
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
(defmethod pari-to-lisp ((x internal-pari-object))
  (convert-from-pari (pari-class-pointer x)))
(defmethod pari-to-lisp ((x number)) x)
(defmethod pari-to-lisp ((x array)) x)
