;;;; Common Lisp Object System for CLISP: Classes
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;;; predefined classes:
;; metaclasses:
(defvar <class>)                       ; here <structure-class>
(defvar <standard-class>)              ; here <structure-class>
(defvar <structure-class>)             ; here <structure-class>
(defvar <built-in-class>)              ; here <structure-class>
;; classes:
(defvar <standard-object>)             ; <standard-class>
(defvar <structure-object>)            ; <structure-class>
(defvar <generic-function>)            ; <built-in-class>
(defvar <standard-generic-function>)   ; <built-in-class>
;;(defvar <method>)                     ; here <structure-class>
;;(defvar <standard-method>)            ; here <structure-class>
(defvar <array>)                       ; <built-in-class>
(defvar <bit-vector>)                  ; <built-in-class>
(defvar <character>)                   ; <built-in-class>
(defvar <complex>)                     ; <built-in-class>
(defvar <cons>)                        ; <built-in-class>
(defvar <float>)                       ; <built-in-class>
(defvar <function>)                    ; <built-in-class>
(defvar <hash-table>)                  ; <built-in-class>
(defvar <integer>)                     ; <built-in-class>
(defvar <list>)                        ; <built-in-class>
(defvar <null>)                        ; <built-in-class>
(defvar <number>)                      ; <built-in-class>
(defvar <package>)                     ; <built-in-class>
(defvar <pathname>)                    ; <built-in-class>
#+LOGICAL-PATHNAMES
(defvar <logical-pathname>)            ; <built-in-class>
(defvar <random-state>)                ; <built-in-class>
(defvar <ratio>)                       ; <built-in-class>
(defvar <rational>)                    ; <built-in-class>
(defvar <readtable>)                   ; <built-in-class>
(defvar <real>)                        ; <built-in-class>
(defvar <sequence>)                    ; <built-in-class>
(defvar <stream>)                      ; <built-in-class>
(defvar <file-stream>)                 ; <built-in-class>
(defvar <synonym-stream>)              ; <built-in-class>
(defvar <broadcast-stream>)            ; <built-in-class>
(defvar <concatenated-stream>)         ; <built-in-class>
(defvar <two-way-stream>)              ; <built-in-class>
(defvar <echo-stream>)                 ; <built-in-class>
(defvar <string-stream>)               ; <built-in-class>
(defvar <string>)                      ; <built-in-class>
(defvar <symbol>)                      ; <built-in-class>
(defvar <t>)                           ; <built-in-class>
(defvar <vector>)                      ; <built-in-class>

;;; low-level-representation:

;; in the runtime-system, the type "CLOS-instance" exists.
;; first component is the class.

;; classes are structures of type CLASS,
;;   first component is the metaclass, second component is the name.

;; the "value" of a slot that is unbound, is #<UNBOUND> - what else?

;;; see RECORD.D :
;; (STD-INSTANCE-P obj) tests, if an object is a CLOS-instance.
;; (ALLOCATE-STD-INSTANCE class n) returns a CLOS-instance with Class class
;; and n-1 additional slots.
;;; see IO.D :
;; CLOS-instances are printed via (PRINT-OBJECT object stream) .

;;; global management of classes and their names:

#|| ; see PREDTYPE.D
 (defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment)) ; what should be the meaning of the environment?
  (unless (symbolp symbol)
    (error-of-type 'type-error
      :datum symbol :expected-type 'symbol
      (TEXT "~S: argument ~S is not a symbol")
      'find-class symbol))
  (let ((class (get symbol 'CLOSCLASS)))
    (if (not (class-p class))
      (if errorp
        (error-of-type 'error
          (TEXT "~S: ~S does not name a class")
          'find-class symbol)
        nil)
      class)))
||#

(defun (setf find-class) (new-value symbol &optional errorp environment)
  (declare (ignore errorp environment)) ; what should be the meaning of environment?
  (unless (symbolp symbol)
    (error-of-type 'type-error
      :datum symbol :expected-type 'symbol
      (TEXT "~S: argument ~S is not a symbol")
      '(setf find-class) symbol))
  (unless (or (null new-value) (class-p new-value))
    (error-of-type 'type-error
      :datum new-value :expected-type 'class
      (TEXT "~S: ~S is not a class")
      '(setf find-class) new-value))
  (let ((h (get symbol 'CLOSCLASS)))
    (when (class-p h)
      (when (and (built-in-class-p h) (eq (class-name h) symbol)) ; protect structure classes, too??
        (error-of-type 'error
          (TEXT "~S: cannot redefine built-in class ~S")
          '(setf find-class) h)))
    ;; should we do (setf (class-name h) nil) ??
    (sys::check-redefinition symbol '(setf find-class)
                             (and (class-p h) "class")))
  (if new-value
      (setf (get symbol 'CLOSCLASS) new-value)
      (progn (remprop symbol 'CLOSCLASS) nil)))

;; (CLASS-OF object) see PREDTYPE.D, uses property CLASS.
