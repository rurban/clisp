;;;; Common Lisp Object System for CLISP
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; method combinations: James Anderson 2003

;; to use it: (USE-PACKAGE "CLOS").

(in-package "COMMON-LISP")
(pushnew ':clos *features*)

(in-package "SYSTEM") ; necessary despite DEFPACKAGE!

(defpackage "CLOS"
  (:import-from "EXT" ext:mapcap)
  (:import-from "SYSTEM"
    ;; Import:
    sys::text                   ; for error messages (i18n.d)
    sys::error-of-type          ; defined in error.d
    sys::check-function-name    ; defined in trace.lisp
    sys::function-name-p        ; defined in control.d
    sys::function-block-name    ; defined in eval.d
    sys::memq                   ; defined in list.d
    sys::gensym-list            ; defined in macros2.lisp
    sys::make-signature         ; defined in functions.lisp
    sys::sig-req-num sys::sig-opt-num sys::sig-rest-p ; likewise
    sys::sig-keys-p sys::sig-keywords sys::sig-allow-p ; likewise
    ;; clos::generic-function-p ; defined in predtype.d
    ;; clos::class-p clos:class-of clos:find-class ; defined in predtype.d
    ;; clos::typep-class        ; defined in predtype.d
    ;; clos::structure-object-p ; defined in record.d
    ;; clos::std-instance-p clos::allocate-std-instance ; defined in record.d
    ;; clos::%allocate-instance ; defined in record.d
    ;; clos:slot-value clos::set-slot-value ; defined in record.d
    ;; clos:slot-boundp clos:slot-makunbound ; defined in record.d
    ;; clos:slot-exists-p ; defined in record.d
    ;; clos::class-gethash clos::class-tuple-gethash ; defined in hashtabl.d
    compiler::*keyword-package* ; defined in compiler.lisp
    compiler::%generic-function-lambda ; defined in compiler.lisp
    compiler::%optimize-function-lambda ; defined in compiler.lisp
    compiler::analyze-lambdalist              ; ditto
    ;; clos:generic-flet clos:generic-labels ; treated in compiler.lisp
    ;; Export:
    ;; clos::closclass ; property in predtype.d, type.lisp, compiler.lisp
    ;; clos:class      ; used in record.d
    ;; clos:generic-function ; used in type.lisp, compiler.lisp
    ;; clos:standard-generic-function ; used in predtype.d, type.lisp, compiler.lisp
    ;; clos:slot-missing clos:slot-unbound  ; called by record.d
    ;; clos::*make-instance-table*          ; used in record.d
    ;; clos::*reinitialize-instance-table*  ; used in record.d
    ;; clos::initial-reinitialize-instance  ; called by record.d
    ;; clos::initial-initialize-instance    ; called by record.d
    ;; clos::initial-make-instance          ; called by record.d
    ;; clos:print-object                    ; called by io.d
    ;; clos:describe-object                 ; called by user2.lisp
    ;; clos::define-structure-class         ; called by defstruct.lisp
    ;; clos::defstruct-remove-print-object-method ; called by defstruct.lisp
    ;; clos::built-in-class-p               ; called by type.lisp
    ;; clos::subclassp  ; called by type.lisp, used in compiler.lisp
    ;; clos:class-name                      ; used in type.lisp, compiler.lisp
    ;; clos:find-class                      ; used in compiler.lisp
    ;; clos::defgeneric-lambdalist-callinfo ; called by compiler.lisp
    ;; clos::make-generic-function-form     ; called by compiler.lisp
    )) ; defpackage

(in-package "CLOS")

;;; exports: ** also in init.lisp ** !
(export
 '(;; names of functions and macros:
   slot-value slot-boundp slot-makunbound slot-exists-p with-slots
   with-accessors documentation
   find-class class-of defclass defmethod call-next-method next-method-p
   defgeneric generic-function generic-flet generic-labels
   class-name no-applicable-method no-next-method no-primary-method
   find-method add-method remove-method
   compute-applicable-methods method-qualifiers function-keywords
   slot-missing slot-unbound
   print-object describe-object
   make-instance allocate-instance initialize-instance reinitialize-instance
   shared-initialize ensure-generic-function
   make-load-form make-load-form-saving-slots
   change-class update-instance-for-different-class
   update-instance-for-redefined-class make-instances-obsolete
   ;; names of classes:
   standard-class structure-class built-in-class
   standard-object structure-object
   generic-function standard-generic-function method standard-method
   ;; MOP -- _NOT_ in init.lisp!
   class-prototype class-finalized-p finalize-inheritance
   ;; method combinations
   standard method-combination define-method-combination
   method-combination-error invalid-method-error
   call-method make-method))

;;; preliminary remarks:

;; abbreviations:
;; std = standard
;; gf = generic function
;; <...> = (class ...), mostly = (find-class '...)
;; em = effective method

(load "clos-class1")
(load "clos-slots1")
(load "clos-class2")
(load "clos-method1")
(load "clos-methcomb1")
(load "clos-method2")
(load "clos-genfun1")
(load "clos-methcomb2")
(load "clos-genfun2")
(load "clos-methcomb3")
(load "clos-genfun3")
(load "clos-methcomb4")
(load "clos-genfun4")
(load "clos-class3")
(load "clos-genfun5")
(load "clos-method3")
(load "clos-slots2")
(load "clos-class4")
(load "clos-class5")
(load "documentation")
(load "clos-methcomb5")
