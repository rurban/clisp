;;;; Common Lisp Object System for CLISP: Classes
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


(defgeneric class-name (class)
  (:method ((class class))
    (class-classname class)))

(defgeneric (setf class-name) (new-value class)
  (:method (new-value (class class))
    (unless (symbolp new-value)
      (error-of-type 'type-error
        :datum new-value :expected-type 'symbol
        (TEXT "~S: The name of a class must be a symbol, not ~S")
        '(setf class-name) new-value))
    (when (built-in-class-p class)
      (error-of-type 'error
        (TEXT "~S: The name of the built-in class ~S cannot be modified")
        '(setf class-name) class))
    (setf (class-classname class) new-value)))
