;; multithreading for CLISP

(defpackage "THREADS"
  (:nicknames "MT" "MP")
  (:use "COMMON-LISP" "EXT")
  (:export "THREAD" "MAKE-THREAD" "THREAD-WAIT"
           "THREADP" "THREAD-YIELD" "THREAD-KILL"
           "THREAD-INTERRUPT" "THREADP" "THREAD-NAME"
           "THREAD-ACTIVE-P" "THREAD-WHOSTATE" "CURRENT-THREAD" "LIST-THREADS"
           "MUTEX" "MUTEXP" "MAKE-MUTEX" "MUTEX-LOCK" "MUTEX-UNLOCK"
           "MUTEX-OWNER" "MUTEX-RECURSIVE-P"
           "EXEMPTION" "EXEMPTIONP" "MAKE-EXEMPTION" "EXEMPTION-SIGNAL"
           "EXEMPTION-WAIT" "EXEMPTION-BROADCAST"
           "MAKE-LOCK" "THREAD-LOCK" "THREAD-UNLOCK" "WITH-LOCK"
           "Y-OR-N-P-TIMEOUT" "YES-OR-NO-P-TIMEOUT" "WITH-TIMEOUT"
           "SYMBOL-VALUE-THREAD" "*DEFAULT-SPECIAL-BINDINGS*"))

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

;; declare special variable for thread's whostate
(defvar *THREAD-WHOSTATE* nil)

;; TODO: add more variables (something should done about the
;; standartd input/output streams.
(defvar *DEFAULT-SPECIAL-BINDINGS*
  '((*random-state* . (make-random-state nil))
    (*gensym-counter* . 0)
    (ext::*command-index* . 0)
    (*thread-whostate* . nil)
    (*print-base* . 10)
    (*print-length* . nil)
    (*print-level* . nil)
    (*print-circle* . nil)
    (*print-radix* . nil)
    (*print-case* . :upcase)
    (*print-gensym* . t)
    (*print-pretty* . t)
    (*print-readably* . nil)
    (*read-suppress* . nil)
    (*read-default-float-format* . 'single-float)
    (*readtable* . (copy-readtable nil))))

(defsetf SYMBOL-VALUE-THREAD MT::SET-SYMBOL-VALUE-THREAD)

(defun thread-whostate (thread)
  (symbol-value-thread *thread-whostate* thread))

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

(defmacro with-lock ((mutex) &body body)
  "Execute BODY with MUTEX locked."
  (let ((lk (gensym "WL-")))
    `(let ((,lk ,mutex))
      (unwind-protect (progn (mutex-lock ,lk) ,@body)
        (mutex-unlock ,lk)))))

;; helper function for thread interruption
(defun %throw-tag (tag)
  (throw tag tag))
