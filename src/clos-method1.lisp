;;;; Common Lisp Object System for CLISP: Methods
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


(defstruct (method (:predicate nil) (:copier nil) (:constructor nil)))

(defparameter <method> 'method)

(defstruct (standard-method (:include method) (:conc-name "STD-METHOD-")
                            (:copier nil)) ; (:print-object print-std-method)
  function               ; the function
  wants-next-method-p    ; flag, if the NEXT-METHOD (as function with all
                         ; arguments) resp. NIL is to be passed as first argument
                         ; (= NIL for :BEFORE- and :AFTER-methods)
  parameter-specializers ; list ({class | (EQL object)}*)
  qualifiers             ; list of symbols, e.g. (:before)
  signature              ; signature struct (see sompiler.lisp)
  gf                     ; the generic function, which this method belongs to
                         ; (only for the demand of CALL-NEXT-METHOD and
                         ; NO-NEXT-METHOD)
  initfunction           ; returns - if called - the function
                         ; (only for the demand of ADD-METHOD)
  origin                 ; flag, if this method comes from a DEFGENERIC
)

;; For CALL-NEXT-METHOD and NO-NEXT-METHOD the generic function must be known.
;; As methods do not belong to certain generic functions in principle
;; (because of ADD-METHOD), we must copy the method at ADD-METHOD.
;; We determine the identity of two copies of the same method
;; by looking at std-method-initfunction. (One could also pass
;; the generic function at each call as first argument to the effective
;; method instead, but this is certainly more inefficient.)

(defun print-std-method (method stream)
  (print-unreadable-object (method stream :type t)
    (dolist (q (std-method-qualifiers method))
      (write q :stream stream)
      (write-char #\Space stream))
    (write (std-method-parameter-specializers method) :stream stream)))

(defstruct (standard-accessor-method (:include standard-method)
                                     (:conc-name "%ACCESSOR-METHOD-")
                                     (:copier nil))
  slot-definition        ; direct slot definition responsible for this method
)

(defstruct (standard-reader-method (:include standard-accessor-method)
                                   (:copier nil))
)

(defstruct (standard-writer-method (:include standard-accessor-method)
                                   (:copier nil))
)
