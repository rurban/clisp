;;;; Common Lisp Object System for CLISP: Method Combination
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; James Anderson 2003

(in-package "CLOS")


;;; Global management of method-combinations and their names:

;; Mapping from name, a symbol, to method-combination instance.
;; If the caller is non-nil, an error is signalled if the method-combination
;; does not exist. Otherwise nil is returned.
;; All method-combination objects used here have an empty options list;
;; method-combination objects with options are stored in generic functions.
(defun get-method-combination (name caller)
  (or (get name '%method-combination)
      (and caller
           (error (TEXT "~S: The method combination ~S is not defined.")
                  caller name))))
(defun (setf get-method-combination) (new-value name)
  (setf (get name '%method-combination) new-value))


;;; The method-combination class definition.
;; A method-combination is used 1) without options when defined and attached
;; to a symbol, 2) with options when applied to a particular generic function.
;; Strange design... but ANSI CL specifies it this way.
;; A structure definition is to be preferred, otherwise the compiled
;; load fails on type tests as the class can't be defined early enough
;; in the file.
(defstruct (method-combination) ; (:print-object print-object-<method-combination>)
  "The method-combination class models all method combination.
The variations are handled by binding the expander function to the instance
and pairing the method-combination definition object with the option list
in the generic function instance."

  name                          ; a symbol naming the method combination
  (documentation nil)           ; an optional documentation string
  (check-options nil)           ; A function of 3 arguments
                                ; (function-name method-combination options)
                                ; that checks the syntax of arguments to the
                                ; method combination
  (expander nil)                ; A function of 4 arguments
                                ; (function method-combination options methods)
                                ; which computes two values: 1. the inner body
                                ; of the effective method, as a form containing
                                ; (CALL-METHOD ...) forms, 2. a list of
                                ; options describing the wrapper, such as
                                ; (:ARGUMENTS ...) or (:GENERIC-FUNCTION ...).
  (check-method-qualifiers nil) ; A function of 3 arguments
                                ; (function method-combination method)
                                ; that checks whether the method's qualifiers
                                ; are compatible with the method-combination.
  (call-next-method-allowed nil) ; A function of 3 arguments
                                ; (function method-combination method)
                                ; telling whether call-next-method is allowed
                                ; in the particular method.
  (declarations nil)            ; list to be prepended to the effective method
                                ; body
  (arguments-lambda-list nil)   ; The :arguments option of the defined method
                                ; combination for inclusion in the effective
                                ; method function.

  ;; The following slots apply only to standard and short form
  ;; method-combination.
  (qualifiers nil)              ; the allowed list of qualifiers

  ;; The following slots apply only to short form method-combination.
  (operator nil)                ; a symbol
  (identity-with-one-argument nil) ; true if `(operator ,x) should be replaced
                                ; with x

  ;; The following slots depend on the particular generic function.
  (options nil))                ; arguments for the method combination

(defun print-object-<method-combination> (object stream)
  (print-unreadable-object (object stream :identity t :type t)
    (write (method-combination-name object) :stream stream)))
