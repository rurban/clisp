;; Load the ansi-tests testsuite into CLISP in *ANSI* mode.
;; Usage:
;;   $ cd ansi-tests
;;   $ clisp -q -ansi -i clispload.lsp

;; First the infrastructure.
(load "gclload1.lsp")

;; Set *package*.
(in-package :cl-test)

;; for ENSURE-DIRECTORIES-EXIST.8
(when (ext:probe-directory "scratch/")
  (mapc #'delete-file (directory "scratch/*"))
  (ext:delete-dir "scratch/"))

;; The expected failures.
(setq regression-test::*expected-failures* '(

  ;; ANSI CL 11.1.2. says that the only nickname of the COMMON-LISP package
  ;; is "CL". In CLISP it also has the nickname "LISP", for backward
  ;; compatibility with older programs.
  COMMON-LISP-PACKAGE-NICKNAMES

  ;; ANSI CL 11.1.2. says that the only nickname of the COMMON-LISP-USER
  ;; package is "CL-USER". In CLISP it also has the nickname "USER", for
  ;; backward compatibility with older programs.
  COMMON-LISP-USER-PACKAGE-NICKNAMES

  ;; The symbols
  ;;   LEAST-NEGATIVE-LONG-FLOAT LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
  ;;   LEAST-POSITIVE-LONG-FLOAT LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
  ;;   LONG-FLOAT-EPSILON LONG-FLOAT-NEGATIVE-EPSILON MOST-NEGATIVE-LONG-FLOAT
  ;;   MOST-POSITIVE-LONG-FLOAT PI
  ;; are variables, not constants, in CLISP, because the long-float precision
  ;; can be changed at run-time, and the values of these symbols change
  ;; accordingly.
  CL-CONSTANT-SYMBOLS.1
  CONSTANTP.10

  ;; Paul Dietz assumes that NIL in place of a variable is equivalent to
  ;; #:IGNORE, i.e. it matches any object and discards it.
  ;; I argue that destructuring works in tree-like manner, where the atoms are
  ;; leaves. NIL being an atom, it is a tree leaf and represents a variable.
  ;; Since NIL as a variable cannot be bound - it is a constant -, NIL in a
  ;; macro argument list is invalid.
  MACROLET.39

  ;; Paul Dietz assumes that passing invalid initialization arguments is a
  ;; PROGRAM-ERROR. In CLISP, it is just an ERROR.
  SHARED-INITIALIZE.ERROR.4

  ;; Paul Dietz assumes that the qualifiers of methods are checked only
  ;; at run time, when the effective method is determined.
  ;; I argue that the description of DEFMETHOD allows qualifier checking to
  ;; occur already at method definition time.
  DEFGENERIC-METHOD-COMBINATION.+.10
  DEFGENERIC-METHOD-COMBINATION.+.11
  DEFGENERIC-METHOD-COMBINATION.APPEND.10
  DEFGENERIC-METHOD-COMBINATION.APPEND.11
  DEFGENERIC-METHOD-COMBINATION.APPEND.13
  DEFGENERIC-METHOD-COMBINATION.NCONC.10
  DEFGENERIC-METHOD-COMBINATION.NCONC.11
  DEFGENERIC-METHOD-COMBINATION.LIST.10
  DEFGENERIC-METHOD-COMBINATION.LIST.11
  DEFGENERIC-METHOD-COMBINATION.MAX.10
  DEFGENERIC-METHOD-COMBINATION.MAX.11
  DEFGENERIC-METHOD-COMBINATION.MIN.10
  DEFGENERIC-METHOD-COMBINATION.MIN.11
  DEFGENERIC-METHOD-COMBINATION.AND.10
  DEFGENERIC-METHOD-COMBINATION.AND.11
  DEFGENERIC-METHOD-COMBINATION.OR.10
  DEFGENERIC-METHOD-COMBINATION.OR.11
  DEFGENERIC-METHOD-COMBINATION.PROGN.10
  DEFGENERIC-METHOD-COMBINATION.PROGN.11

  ;; ANSI CL 12.1.4.4. implies that (ATAN 0 1.0) is 0.0.
  ;; In CLISP it is the exact 0.
  ATAN.4

  ;; In CLISP (atan 1L0) is more than long-float-epsilon apart from (/ pi 4).
  ATAN.11 ATAN.13

  ;; In CLISP rounding errors cause (let ((c #C(97748.0s0 0.0s0))) (/ c c))
  ;; to be different from #C(1.0s0 0.0s0).
  /.8

  ;; ANSI CL 12.1.4.4. implies that (EXPT any-single-float 0) is 1.0.
  ;; In CLISP it is the exact 1.
  EXPT.3 EXPT.4 EXPT.5 EXPT.6

  ;; ANSI CL 12.1.4.4. implies assumes that (EXPT any-complex-single-float 0)
  ;; is #c(1.0 0.0).
  ;; In CLISP it is the exact 1.
  EXPT.8 EXPT.9 EXPT.10 EXPT.11

  ;; CLISP supports complex numbers with realpart and imagpart of different
  ;; type.
  COMPLEX.2 COMPLEX.4 COMPLEX.5

  ;; ANSI CL 12.1.4.4. implies that (PHASE any-nonnegative-real-number) is 0.0.
  ;; In CLISP it is the exact 0.
  PHASE.1 PHASE.2 PHASE.3 PHASE.4 PHASE.5 PHASE.6 PHASE.7

  ;; In CLISP the classes CLASS and METHOD are implemented as structures.
  TYPES.3 BUILT-IN-CLASS-CPL STANDARD-CLASS-CPL STANDARD-METHOD-CPL

  ; To be revisited:
  ; CHANGE-CLASS.1.11 CHANGE-CLASS.3.2 CHANGE-CLASS.ERROR.4
  ; MAKE-INSTANCES-OBSOLETE.2 TYPES.7B TYPES.7C
  ; USER-CLASS-DISJOINTNESS USER-CLASS-DISJOINTNESS-2 TAC-3.16
  ; PRINT.SYMBOL.PREFIX.3 PRINT.SYMBOL.PREFIX.8 PRINT.SYMBOL.PREFIX.10
  ; PRINT.STRING.NIL.1 PRINT.STRING.NIL.2

  ;; test bug: PROBE-FILE on directory:
  ENSURE-DIRECTORIES-EXIST.8
))

;; A few tests call DISASSEMBLE. Make it work without user intervention.
(setf (ext:getenv "PAGER") "cat")

;; w2k exits on (disassemble 'car)
#+(or win32 cygwin)
(ext:without-package-lock ("SYS")
  (defun sys::disassemble-machine-code (a b c)
    (format t "~&<~S ~S ~S>~%" a b c)))

;; Then the tests.
(load "gclload2.lsp")
