;;; Lisp wrappers for the GLIBC FFI

(in-package "SYS")

(require "linux")

(defpackage "LINUX"
  (:case-sensitive t)
  (:nicknames "UNIX" "GLIBC")
  (:use)
  (:export "real-path" "get-host-name" "get-domain-name"
           "linux-error" "check-res"
           "signal-valid-p" "signal-action-retrieve" "signal-action-install"
           "sa-handler" "sa-flags" "sa-mask" "sigset-empty" "sigset-fill"
           "sigset-add" "sigset-del" "sigset-member-p" "set-sigprocmask"
           "sigset-pending" "set-signal-handler"))

(eval-when (compile load eval)
  (setf (package-lock "SYS") nil))
(push "LINUX" *system-package-list*)

;; if you think you need this, you should use (array character)
;; instead of (array char)
;;(defun vec2string (vec)
;;  ;; Convert a char[] to a lisp STRING.
;;  (convert-string-from-bytes vec *foreign-encoding*
;;                             :end (position 0 vec)))

(defun linux:linux-error (caller)
  (error "~s: ~a" caller (linux::strerror linux::errno)))
(defmacro linux:check-res (res caller)
  `(unless (zerop ,res) (linux:linux-error ,caller)))

(defun linux:real-path (name)
  (multiple-value-bind (success resolved)
      ;; :out or :in-out parameters are returned via multiple values
      (linux::realpath name)
    (unless success (linux:linux-error 'linux:real-path))
    resolved))

(defun linux:get-host-name ()
  (multiple-value-bind (success name)
      ;; :out or :in-out parameters are returned via multiple values
      (linux::gethostname linux::MAXHOSTNAMELEN)
    (linux:check-res success 'linux:get-host-name)
    name))

(defun linux:get-domain-name ()
  (multiple-value-bind (success name)
      ;; :out or :in-out parameters are returned via multiple values
      (linux::getdomainname linux::MAXHOSTNAMELEN)
    (linux:check-res success 'linux:get-domain-name)
    name))

;; convenience functions for ffi sigaction definitions
;; Peter Wood 2002

(defun linux:signal-valid-p (signal)
  "Is SIGNAL valid for this machine?"
  (zerop (linux::sigaction-new signal nil nil)))

(defun linux:signal-action-retrieve (signal)
  "Return the presently installed sigaction structure for SIGNAL"
  (multiple-value-bind (ret act) (linux::sigaction-old signal nil)
    (linux:check-res ret 'linux:signal-action-retrieve)
    act))

(defun linux:signal-action-install (signal newact)
  "Install NEWACT as the sigaction structure for SIGNAL. Error on failure."
  (linux:check-res (linux::sigaction-new signal newact nil)
                   'linux:signal-action-install))

(defun linux:sa-handler (sigact)
  "Returns the signal handler function for SIGACT struct. SETF place."
  (slot-value sigact 'linux::sa_handler))
(defsetf linux:sa-handler (sigact) (handler)
  `(setf (slot-value ,sigact 'linux::sa_handler) ,handler))

(defun linux:sa-flags (sigact)
  "Returns the sa_flags for SIGACT struct. SETF place."
  (slot-value sigact 'linux::sa_flags))
(defsetf linux:sa-flags (sigact) (newflags)
  `(setf (slot-value ,sigact 'linux::sa_flags) ,newflags))

;; e.g.: (setf (sa-flags SIGACT) (logior SA_RESETHAND SA_NOCLDSTOP))

(defun linux:sa-mask (sigact)
  "Returns the sa_mask for SIGACT struct. SETF place."
  (slot-value sigact 'linux::sa_mask))
(defsetf linux:sa-mask (sigact) (mask)
  `(setf (slot-value ,sigact 'linux::sa_mask) ,mask))

(defun linux:sigset-empty ()
  "Return an empty sigset."
  (multiple-value-bind (ret act) (linux::sigemptyset)
    (linux:check-res ret 'linux:sigset-empty)
    act))

(defun linux:sigset-fill ()
  "Return a full sigset"
  (multiple-value-bind (ret set) (linux::sigfillset)
    (linux:check-res ret 'linux:sigset-fill)
    set))

(defun linux:sigset-add (set signal)
  "Return a new set with SIGNAL"
  (multiple-value-bind (ret set) (linux::sigaddset set signal)
    (linux:check-res ret 'linux:sigset-add)
    set))

(defun linux:sigset-del (set signal)
  "Return a new set without SIGNAL"
  (multiple-value-bind (ret set) (linux::sigdelset set signal)
    (linux:check-res ret 'linux:sigset-del)
    set))

(defun linux:sigset-member-p (set signal)
  "T if SIGNAL is a member of SET, otherwise NIL"
  (not (zerop (linux::sigismember set signal))))

(defun linux:set-sigprocmask (act set)
  ;; NB the result of this will not be 'visible' in the sigaction
  ;; struct which contains SET, although the ACT *will* be performed.
  ;; If you want a visible result, see linux:sigprocmask-set-n-save,
  ;; which returns as 2nd value the set structure resulting from ACT.
  "Do ACT on SET. Returns NIL on success and signals an error on failure."
  (linux:check-res (linux::sigprocmask-set act set nil)
                   'linux:set-sigprocmask))

(defun linux:sigset-pending ()
  "Returns the set of pending signals. Nil on failure"
  (multiple-value-bind (ret set) (linux::sigpending)
    (linux:check-res ret 'linux:sigset-pending)
    set))

(defun linux:set-signal-handler (signal fn)
  "Sets FN as signal handler for SIGNAL.  Returns old signal handler."
  (let* ((sigact (linux:signal-action-retrieve signal)) ; the current sigact
         (oh (linux:sa-handler sigact))) ; save the old handler to return
    (setf (linux:sa-handler sigact) fn) ; make fn be the handler in sigact
    (linux:signal-action-install signal sigact) ; install
    oh))                        ; return the old handler

#| signal handling examples:

;;; changing signal handlers:

 (setf oldsigact (linux:signal-action-retrieve linux:SIGINT))
#S(LINUX:sigaction :|sa_handler| #<FOREIGN-FUNCTION #x080711D4>
   :|sa_mask| #S(LINUX:sigset_t :|val| #(2)) :|sa_flags| 335544320
   :|sa_restorer| #<FOREIGN-FUNCTION #x401F1868>)
 (setf savehandler (linux:sa-handler oldsigact))
#<FOREIGN-FUNCTION #x080711D4>
;; this is example is _BAD_ because one cannot do i/o in handlers
;; <https://sourceforge.net/mailarchive/message.php?msg_id=3599878>
 (defun test-handler (s) (format t "~&~s: signal ~d~%" 'test-handler s))
 (setf (linux:sa-handler oldsigact) #'test-handler)
 (linux:signal-action-install linux:SIGINT oldsigact)
;; Now Ctrl-C invokes TEST-HANDLER
 (setf (linux:sa-handler oldsigact) savehandler)
 (linux:signal-action-install linux:SIGINT oldsigact)
;; the standard behavior is restored

;; this is packaged into linux:set-signal-handler:
 (setf savehandler (linux:set-signal-handler linux:SIGINT #'test-handler))
 (linux:raise linux:SIGINT)
;; TEST-HANDLER is called
 (linux:set-signal-handler linux:SIGINT savehandler)
;; the standard behavior is restored

;; Please note that if you use SA_RESETHAND, you reset the handler to
;; the system's notion of default handler, not Clisp's, so if you then
;; hit Ctrl-c, you would exit Clisp!

;;; sigprocmask & sigpending

 (setf sigact (linux:signal-action-retrieve linux:SIGINT))
 (linux:raise linux:SIGINT)
;; ** - Continuable Error/PRINT: User break
 (linux:set-sigprocmask linux:SIG_BLOCK (linux:sa-mask sigact))
 (linux:raise linux:SIGINT)
;; nothing
 (linux:sigset-pending)
#S(LINUX:sigset_t :|val| #(2))
 (linux:set-sigprocmask linux:SIG_UNBLOCK (linux:sa-mask sigact))
;; ** - Continuable Error/EVAL: User break
 (linux:sigset-pending)
#S(LINUX:sigset_t :|val| #())
 (linux:raise linux:SIGINT)
;; ** - Continuable Error/PRINT: User break
;; |#

(eval-when (compile load eval)
  (setf (package-lock *system-package-list*) t))
