;;;; Common Lisp Object System for CLISP: Method Combination
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; James Anderson 2003

(in-package "CLOS")


;; Mapping from name, a symbol, to method-combination instance.
(defun find-method-combination (name &key (if-does-not-exist :error))
  (or (get name 'find-method-combination)
      (and if-does-not-exist
           (error "undefined method combination ~s" name))))
(defun (setf find-method-combination) (def name)
  (setf (get name 'find-method-combination) def))

;;; The method-combination class definition.
;; A method-combination is used 1) without options when defined and attached
;; to a symbol, 2) with options when applied to a particular generic function.
;; Strange design... but ANSI CL specifies it this way.
;; A structure definition is to be preferred, otherwise the compiled
;; load fails on type tests as the class can't be defined early enough
;; in the file.
(defstruct (method-combination) ; (:print-object print-method-combination)
  "The method-combination class models all method combination.
The variations are handled by binding the expander function to the instance
and pairing the method-combination definition object with the option list
in the generic function instance."
  name                          ; a symbol naming the method combination
  (operator nil)                ; a symbol operator for short forms
  (order :most-specific-first) ; the order for short form primary methods
  (qualifiers nil)                 ; the allowed list of qualifiers
  (identity-with-one-argument nil) ; true if the short should be so generated
  (documentation nil)              ; an optional documentation string
  (declarations nil) ; list to be prepended to the effective method body
  (expander nil)              ; A function of 4 arguments
                              ; (function method-combination options arguments)
                              ; which computes a combined method function.
  (check-method-qualifiers nil) ; A function of 3 arguments
                                ; (function method-combination method)
                                ; that checks whether the method's qualifiers
                                ; are compatible with the method-combination.
  (call-next-method-allowed nil) ; A function of 3 arguments
                                 ; (function method-combination method)
                                 ; telling whether call-next-method is allowed
                                 ; in the particular method.
  (arguments-lambda-list nil) ; The :arguments option of the defined method
                              ; combination for inclusion in the effective
                              ; method function.
  ;; The following slots depend on the particular generic function.
  (options nil))              ; arguments for the method combination

(defun print-method-combination (object stream)
  (print-unreadable-object (object stream :identity t :type t)
    (write (method-combination-name object) :stream stream)))
