;; multithreading for CLISP

(defpackage "THREADS"
  (:nicknames "MT" "MP")
  (:use "COMMON-LISP" "EXT")
  (:export "THREAD" "MAKE-THREAD" "THREADP" "THREAD-YIELD"
           "THREAD-INTERRUPT" "THREAD-NAME" "THREAD-ACTIVE-P"
           "CURRENT-THREAD" "LIST-THREADS"
           "MUTEX" "MUTEXP" "MAKE-MUTEX" "MUTEX-LOCK" "MUTEX-UNLOCK"
           "MUTEX-OWNER" "MUTEX-RECURSIVE-P" "WITH-MUTEX-LOCK" "MUTEX-NAME"
           "EXEMPTION" "EXEMPTIONP" "MAKE-EXEMPTION" "EXEMPTION-SIGNAL"
           "EXEMPTION-WAIT" "EXEMPTION-BROADCAST" "EXEMPTION-NAME"
           "Y-OR-N-P-TIMEOUT" "YES-OR-NO-P-TIMEOUT" "WITH-TIMEOUT"
           "SYMBOL-VALUE-THREAD" "*DEFAULT-SPECIAL-BINDINGS*"
           "WITH-DEFERRED-INTERRUPTS"))

(in-package "MT")

(use-package '("MT") "EXT")
(re-export "MT" "EXT")

;; definitions

;; default value (lisp stack) size is very small -  1MB
;; 0 - leaves the OS to decide (basically bad option)
(defvar *DEFAULT-CONTROL-STACK-SIZE* 1048576 "C stack in bytes")
;; the vstack size will be inherited from the parent thread.
;; this is the number of gcv_object_t on the stack
;; 0 - means - inherit from creation thread.
;; the value will be initialized from the runtime
(defvar *DEFAULT-VALUE-STACK-SIZE*)

;; deferred interrupts.
;; in other implementations it is called without-interrupts
(defvar *defer-interrupts* nil)
(defvar *deferred-interrupts* '()) ; list of pending interrupts

;; TODO: add more variables (something should be done about the
;; standartd input/output streams).
(defvar *DEFAULT-SPECIAL-BINDINGS*
  '((*random-state* . *random-state*)
    (*gensym-counter* . *gensym-counter*)
    (ext::*command-index* . ext::*command-index*)
    (*print-base* . *print-base*)
    (*print-length* . *print-length*)
    (*print-level* . *print-level*)
    (*print-circle* . *print-circle*)
    (*print-radix* . *print-radix*)
    (*print-case* . *print-case*)
    (*print-gensym* . *print-gensym*)
    (*print-pretty* . *print-pretty*)
    (*print-readably* . *print-readably*)
    (*read-suppress* . *read-suppress*)
    (*read-default-float-format* . *read-default-float-format*)
    (*readtable* . (copy-readtable))))

(defmacro with-deferred-interrupts (&body body)
  `(let ((*defer-interrupts* t)
         (*deferred-interrupts* '()))
     (unwind-protect (progn ,@body)
       (dolist (i *deferred-interrupts*)
         (apply (car i) (nreverse (cdr i)))))))

(defsetf SYMBOL-VALUE-THREAD MT::SET-SYMBOL-VALUE-THREAD)

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds,
terminate and evaluate TIMEOUT-FORMS."
  `(call-with-timeout ,seconds (lambda () ,@timeout-forms) (lambda () ,@body)))

(defun timeout-message (default localinfo)
  (write-string (SYS::TEXT "[Timed out] "))
  (write-string (string (car (funcall (if default #'cdr #'car) localinfo))))
  (terpri)
  default)

(defun y-or-n-p-timeout (seconds default &rest args)
  "Y-OR-N-P with timeout."
  (declare (ignorable seconds default))
  (with-timeout (seconds (timeout-message default (localized 'sys::y-or-n)))
    (apply #'y-or-n-p args)))

(defun yes-or-no-p-timeout (seconds default &rest args)
  "YES-OR-NO-P with timeout."
  (declare (ignorable seconds default))
  (with-timeout (seconds (timeout-message default (localized 'sys::yes-or-no)))
    (apply #'yes-or-no-p args)))

;;; locks

(defmacro with-mutex-lock ((mutex) &body body)
  "Execute BODY with MUTEX locked."
  (let ((lk (gensym "WL-")))
    `(let ((,lk ,mutex))
      (unwind-protect (progn (mutex-lock ,lk) ,@body)
        (mutex-unlock ,lk)))))

;; helper function for thread interruption
(defun %throw-tag (tag)
  (throw tag tag))
