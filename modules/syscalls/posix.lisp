;;; handle the posix functions
;;; Sam Steingold 1999-2005

(defpackage #:posix
  (:use #:common-lisp #:ext)
  (:nicknames #:os)
  (:import-from "SYS" sys::process-id)
  (:shadowing-import-from "EXPORTING" #:defstruct)
  (:export
   #:resolve-host-ipaddr #:bogomips #:mkstemp #:mkdtemp
   #:stream-lock #:with-stream-lock #:duplicate-handle #:copy-file
   #:file-owner #:physical-memory
   #+(or :win32 :cygwin) #:file-properties #+unix #:make-xterm-io-stream
   #:priority #:process-id #:openlog #:setlogmask #:syslog #:closelog
   #:getpgid #:setpgrp #:getsid #:setsid #:setpgid #:kill #:sync
   #:erf #:erfc #:j0 #:j1 #:jn #:y0 #:y1 #:yn #:gamma #:lgamma))

(setf (package-lock "EXT") nil)
(use-package '("POSIX") "EXT")
(ext:re-export "POSIX" "EXT")
(pushnew :syscalls *features*)
(in-package "POSIX")

;;; ============================================================
#+unix (progn
(export '(endutxent getutxent getutxid getutxline pututxline setutxent))
(defstruct (utmpx (:constructor make-utmpx (type user id line pid host tv)))
  type user id line pid host tv)
)
;;; ============================================================
(defmacro with-stream-lock ((stream &rest options) &body body)
  "Lock the stream, execute the body, unlock the stream."
  `(unwind-protect (progn (stream-lock ,stream t ,@options) ,@body)
     (stream-lock ,stream nil ,@options)))
;;; ============================================================
(defun syslog (severity facility format &rest args)
  (%syslog severity facility (apply #'format nil format args)))
;; A compiler macro here is better than a simple (declaim (inline syslog))
;; because when the format string is a string literal, the inliner doesn't
;; produce optimal results.
(define-compiler-macro syslog (severity facility format &rest args)
  `(%syslog ,severity ,facility (format nil ,format ,@args)))
;;; ============================================================
(defsetf priority (pid &optional which) (value)
  `(set-priority ,value ,pid ,which))
;;; ============================================================
(defstruct (hostent (:constructor
                     make-hostent (name aliases addr-list addrtype)))
  "see gethostbyname(3) for details"
  (name "" :type simple-string :read-only t)
  (aliases nil :type list :read-only t)
  (addr-list nil :type list :read-only t)
  (addrtype 2 :type fixnum :read-only t))

;;; ============================================================
#+unix (export '(crypt encrypt setkey mknod))

#+unix
(defstruct (user-data (:constructor
                       make-user-data (login-id passwd uid gid full-name
                                       home-dir shell)))
  "see getwnam(2) for details"
  (login-id  "" :type simple-string :read-only t)
  (passwd    "" :type simple-string :read-only t)
  (uid        0 :type (unsigned-byte 32) :read-only t)
  (gid        0 :type (unsigned-byte 32) :read-only t)
  (full-name "" :type simple-string :read-only t)
  (home-dir  "" :type simple-string :read-only t)
  (shell     "" :type simple-string :read-only t))
;;; ============================================================
(export '(set-file-stat convert-mode umask))

(defstruct (file-stat
             (:constructor
              make-file-stat (file dev ino mode nlink uid gid rdev size
                              blksize blocks atime mtime ctime)))
  (file    nil :read-only t)
  (dev       0 :type (unsigned-byte 32) :read-only t)
  (ino       0 :type (unsigned-byte 32) :read-only t)
  (mode    nil :type list :read-only t)
  (nlink     1 :type (unsigned-byte 32) :read-only t)
  (uid     nil :type (or null (unsigned-byte 32)) :read-only t)
  (gid     nil :type (or null (unsigned-byte 32)) :read-only t)
  (rdev    nil :type (or null (unsigned-byte 32)) :read-only t)
  (size      0 :type (unsigned-byte 32) :read-only t)
  (blksize nil :type (or null (unsigned-byte 32)) :read-only t)
  (blocks  nil :type (or null (unsigned-byte 32)) :read-only t)
  (atime     0 :type (integer 0) :read-only t)
  (mtime     0 :type (integer 0) :read-only t)
  (ctime     0 :type (integer 0) :read-only t))

;;; ============================================================
(defstruct (stat-vfs
             (:constructor
              make-stat-vfs (file bsize frsize blocks bfree bavail files
                             ffree favail fsid flag namemax vol-name fs-type)))
  (file    nil :read-only t)
  (bsize   nil :type (or null (unsigned-byte 32)) :read-only t)
  (frsize  nil :type (or null (unsigned-byte 32)) :read-only t)
  (blocks  nil :type (or null (unsigned-byte 32)) :read-only t)
  (bfree   nil :type (or null (unsigned-byte 32)) :read-only t)
  (bavail  nil :type (or null (unsigned-byte 32)) :read-only t)
  (files   nil :type (or null (unsigned-byte 32)) :read-only t)
  (ffree   nil :type (or null (unsigned-byte 32)) :read-only t)
  (favail  nil :type (or null (unsigned-byte 32)) :read-only t)
  (fsid    nil :type (or null (unsigned-byte 32)) :read-only t)
  (flag    nil :type list :read-only t)
  (namemax nil :type (or null (unsigned-byte 32)) :read-only t)
  (vol-name nil :type (or null string) :read-only t)
  (fs-type nil :type (or null string) :read-only t))
;;; ============================================================
#+unix (export '(sysconf confstr))
#+unix
(defstruct (uname (:constructor make-uname (sysname nodename release
                                            version machine)))
  "see uname(2) for details"
  (sysname      "" :type simple-string :read-only t)
  (nodename     "" :type simple-string :read-only t)
  (release      "" :type simple-string :read-only t)
  (version      "" :type simple-string :read-only t)
  (machine      "" :type simple-string :read-only t))
;;; ============================================================
#+unix (progn
(defstruct (rlimit (:constructor make-rlimit (cur max)))
  "see getrlimit(2) for details"
  (cur nil :type (or null (unsigned-byte 32)) :read-only t)
  (max nil :type (or null (unsigned-byte 32)) :read-only t))

(defsetf rlimit (what) (cur max) `(set-rlimit ,what ,cur ,max))

(defstruct (usage (:constructor
                   make-usage (user-time system-time max-rss
                               shared-memory data-memory stack-memory
                               minor-page-faults major-page-faults num-swaps
                               blocks-input blocks-output
                               messages-sent messages-received signals
                               context-switches-voluntary
                               context-switches-involuntary)))
  "see getrusage(3) for details"
  (user-time 0.0d0 :type double-float :read-only t)
  (system-time 0.0d0 :type double-float :read-only t)
  (max-rss 0 :type (signed-byte 32) :read-only t)
  (shared-memory 0 :type (signed-byte 32) :read-only t) ; kB-sec
  (data-memory 0 :type (signed-byte 32) :read-only t) ; kB-sec
  (stack-memory 0 :type (signed-byte 32) :read-only t) ; kB-sec
  (minor-page-faults 0 :type (signed-byte 32) :read-only t)
  (major-page-faults 0 :type (signed-byte 32) :read-only t)
  (num-swaps 0 :type (signed-byte 32) :read-only t)
  (blocks-input 0 :type (signed-byte 32) :read-only t)
  (blocks-output 0 :type (signed-byte 32) :read-only t)
  (messages-sent 0 :type (signed-byte 32) :read-only t)
  (messages-received 0 :type (signed-byte 32) :read-only t)
  (signals 0 :type (signed-byte 32) :read-only t)
  (context-switches-voluntary 0 :type (signed-byte 32) :read-only t)
  (context-switches-involuntary 0 :type (signed-byte 32) :read-only t))
)

;;; ============================================================
(export '(convert-attributes))

(defstruct (file-info (:constructor make-file-info
                                    (attributes ctime atime wtime
                                     size name name-short)))
  (attributes nil :read-only t)
  (ctime nil :read-only t) (atime nil :read-only t) (wtime nil :read-only t)
  (size nil :read-only t) (name nil :read-only t) (name-short nil :read-only t))

#-(or win32 cygwin)
(setf (fdefinition 'convert-attributes) #'convert-mode)
#-(or win32 cygwin)
(defun file-info (path)
  (let ((file-stat (file-stat path)))
    (make-file-info (file-stat-mode file-stat) ; attributes
                    (file-stat-ctime file-stat) ; ctime
                    (file-stat-atime file-stat) ; atime
                    (file-stat-mtime file-stat) ; wtime
                    (file-stat-size file-stat)  ; size
                    (file-stat-file file-stat)  ; name
                    (file-stat-file file-stat)))) ; name-short
;;; ============================================================
#+(or win32 cygwin) (progn
(export '(make-shortcut))

(defstruct (shortcut-info
             (:constructor make-shortcut-info
                           (original path stat working-directory arguments
                            show-command icon description hot-key)))
  (original nil :read-only t)
  (path nil :read-only t)
  (working-directory nil :read-only t)
  (arguments nil :read-only t)
  (show-command nil :read-only t)
  (icon nil :read-only t)
  (description nil :read-only t)
  (hot-key nil :read-only t)
  (stat nil :read-only t))

(defstruct (system-info
             (:constructor make-system-info
                           (processor-architecture page-size
                            minimum-application-address
                            maximum-application-address
                            active-processor-mask number-of-processors
                            allocation-granularity
                            processor-level processor-revision)))
  (processor-architecture nil :read-only t)
  (page-size nil :read-only t)
  (minimum-application-address nil :read-only t)
  (maximum-application-address nil :read-only t)
  (active-processor-mask nil :read-only t)
  (number-of-processors nil :read-only t)
  (allocation-granularity nil :read-only t)
  (processor-level nil :read-only t)
  (processor-revision nil :read-only t))

(defstruct (version
             (:constructor make-version
                           (major minor build platform service-pack
                            service-pack-major service-pack-minor
                            suites product-type)))
  (major nil :read-only t)
  (minor nil :read-only t)
  (build nil :read-only t)
  (platform nil :read-only t)
  (service-pack nil :read-only t)
  (service-pack-major nil :read-only t)
  (service-pack-minor nil :read-only t)
  (suites nil :read-only t)
  (product-type nil :read-only t))

(defstruct (memory-status
             (:conc-name memstat-)
             (:constructor mkmemstat
                           (total-physical avail-physical total-page
                            avail-page total-virtual avail-virtual)))
  (total-physical 0 :type (integer 0) :read-only t)
  (avail-physical 0 :type (integer 0) :read-only t)
  (total-page 0 :type (integer 0) :read-only t)
  (avail-page 0 :type (integer 0) :read-only t)
  (total-virtual 0 :type (integer 0) :read-only t)
  (avail-virtual 0 :type (integer 0) :read-only t))

)

(defun physical-memory ()
  "Return 2 values: TOTAL and AVAILABLE physical memory."
  #+unix (let ((page-size (sysconf :PAGESIZE)))
           (values (* page-size (sysconf :PHYS-PAGES))
                   (* page-size (sysconf :AVPHYS-PAGES))))
  #+win32 (let ((mem-stat (memory-status)))
            (values (memstat-total-physical mem-stat)
                    (memstat-avail-physical mem-stat))))


;;;--------------------------------------------------------------------------
#+unix
(defun make-xterm-io-stream ()
  (let* ((tmps (mkstemp "/tmp/clisp-x-io-XXXXXX"))
         (file (namestring tmps))
         (title "CLISP I/O")
         xio
         (clos::*warn-if-gf-already-called* nil))
    (close tmps) (delete-file tmps)
    (mknod tmps :FIFO :RWXU)
    (shell (format nil "xterm -n ~s -T ~s -e 'tty >> ~a; cat ~a' &"
                   title title file file))
    (setq xio (open file :direction :io))
    ;; we cannot rely on GC closing files because we also need to remove
    ;; the FIFO which is done by the :AFTER method below
    (ext:finalize xio #'close)
    (defmethod close :after ((x (eql xio)) &rest junk)
      (declare (ignore x junk))
      (with-open-file (s file :direction :output)
        (write-line (SYS::TEXT "Bye.") s))
      (delete-file file)
      (let ((clos::*warn-if-gf-already-called* nil))
        (remove-method #'close (find-method #'close '(:after) `((eql ,xio))))))
    xio))

;;; restore locks
(pushnew "POSIX" custom:*system-package-list* :test #'string=)
(setf (package-lock custom:*system-package-list*) t)
