;;; handle the posix functions
;;; Sam Steingold 1999-2005

(defpackage #:posix
  (:use #:common-lisp #:ext)
  (:nicknames #:os)
  (:import-from "SYS" sys::process-id)
  (:export
   #:resolve-host-ipaddr #:bogomips
   #:stream-lock #:with-stream-lock #:duplicate-handle #:copy-file
   #:hostent #:hostent-name #:hostent-aliases #:hostent-addr-list
   #:hostent-addrtype #:file-owner #:physical-memory
   #+(or :win32 :cygwin) #:file-properties
   #:priority #:process-id #:openlog #:setlogmask #:syslog #:closelog
   #:erf #:erfc #:j0 #:j1 #:jn #:y0 #:y1 #:yn #:gamma #:lgamma))

(setf (package-lock "EXT") nil)
(use-package '("POSIX") "EXT")
(ext:re-export "POSIX" "EXT")
(pushnew :syscalls *features*)
(in-package "POSIX")

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
  `(set-priority ,pid ,which ,value))
;;; ============================================================
(defstruct (hostent (:constructor
                     make-hostent (name aliases addr-list addrtype)))
  "see gethostbyname(3) for details"
  (name "" :type simple-string :read-only t)
  (aliases nil :type list :read-only t)
  (addr-list nil :type list :read-only t)
  (addrtype 2 :type fixnum :read-only t))

;;; ============================================================
#+unix (progn
(export
 '(user-data user-data-login-id user-data-passwd user-data-uid user-data-gid
   user-data-full-name user-data-shell
   crypt encrypt setkey mknod))

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

)
;;; ============================================================
(export
 '(file-stat file-stat-file file-stat-dev file-stat-ino file-stat-mode
   file-stat-nlink file-stat-uid file-stat-gid file-stat-rdev
   file-stat-size file-stat-blksize file-stat-blocks file-stat-atime
   file-stat-mtime file-stat-ctime set-file-stat
   convert-mode umask))

(defstruct (file-stat
             (:constructor
              make-file-stat (file dev ino mode nlink uid gid rdev size
                              blksize blocks atime mtime ctime)))
  (file  nil :read-only t)
  (dev     0 :type (unsigned-byte 32) :read-only t)
  (ino     0 :type (unsigned-byte 32) :read-only t)
  (mode    0 :type (unsigned-byte 32) :read-only t)
  (nlink   0 :type (unsigned-byte 32) :read-only t)
  (uid     0 :type (unsigned-byte 32) :read-only t)
  (gid     0 :type (unsigned-byte 32) :read-only t)
  (rdev    0 :type (unsigned-byte 32) :read-only t)
  (size    0 :type (unsigned-byte 32) :read-only t)
  (blksize 0 :type (unsigned-byte 32) :read-only t)
  (blocks  0 :type (unsigned-byte 32) :read-only t)
  (atime   0 :type (integer 0) :read-only t)
  (mtime   0 :type (integer 0) :read-only t)
  (ctime   0 :type (integer 0) :read-only t))

;;; ============================================================
#+unix (progn
(export
 '(stat-vfs stat-vfs-file stat-vfs-bsize stat-vfs-stat-vfs-frsize
   stat-vfs-stat-vfs-blocks stat-vfs-stat-vfs-bfree stat-vfs-stat-vfs-bavail
   stat-vfs-stat-vfs-files stat-vfs-stat-vfs-ffree stat-vfs-stat-vfs-favail
   stat-vfs-stat-vfs-fsid stat-vfs-stat-vfs-flag stat-vfs-namemax))

(defstruct (stat-vfs
             (:constructor
              make-stat-vfs (file bsize frsize blocks bfree bavail files
                             ffree favail fsid flag namemax)))
  (file  nil :read-only t)
  (bsize   0 :type (unsigned-byte 32) :read-only t)
  (frsize  0 :type (unsigned-byte 32) :read-only t)
  (blocks  0 :type (unsigned-byte 32) :read-only t)
  (bfree   0 :type (unsigned-byte 32) :read-only t)
  (bavail  0 :type (unsigned-byte 32) :read-only t)
  (files   0 :type (unsigned-byte 32) :read-only t)
  (ffree   0 :type (unsigned-byte 32) :read-only t)
  (favail  0 :type (unsigned-byte 32) :read-only t)
  (fsid    0 :type (unsigned-byte 32) :read-only t)
  (flag    0 :type (unsigned-byte 32) :read-only t)
  (namemax 0 :type (unsigned-byte 32) :read-only t))

)

;;; ============================================================
#+unix (progn
(export
 '(uname uname-sysname uname-nodename uname-release uname-version uname-machine
   sysconf confstr))

(defstruct (uname (:constructor make-uname (sysname nodename release
                                            version machine)))
  "see uname(2) for details"
  (sysname      "" :type simple-string :read-only t)
  (nodename     "" :type simple-string :read-only t)
  (release      "" :type simple-string :read-only t)
  (version      "" :type simple-string :read-only t)
  (machine      "" :type simple-string :read-only t))
)
;;; ============================================================
#+unix (progn
(export
 '(rlimit rlimit-cur rlimit-max
   usage usage-user-time usage-system-time usage-max-rss
   usage-shared-memory usage-data-memory usage-stack-memory
   usage-minor-page-faults usage-major-page-faults usage-num-swaps
   usage-blocks-input usage-blocks-output usage-messages-sent
   usage-messages-received usage-signals usage-context-switches-voluntary
   usage-context-switches-involuntary))

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
#+(or win32 cygwin) (progn
(export '(file-info file-info-attributes
          file-info-ctime file-info-atime file-info-wtime
          file-info-size-hi file-info-size-lo
          file-info-name file-info-name-short))

(defstruct (file-info (:constructor make-file-info
                                    (attributes ctime atime wtime
                                     size-hi size-lo name name-short)))
  (attributes nil :read-only t)
  (ctime nil :read-only t) (atime nil :read-only t) (wtime nil :read-only t)
  (size-hi nil :read-only t) (size-lo nil :read-only t)
  (name nil :read-only t) (name-short nil :read-only t))

(export '(make-shortcut shortcut-info shortcut-info-working-directory
          shortcut-info-arguments shortcut-info-show-command
          shortcut-info-original shortcut-info-path
          shortcut-info-icon shortcut-info-description shortcut-info-hot-key))

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

(export '(system-info system-info-processor-architecture system-info-page-size
          system-info-minimum-application-address
          system-info-maximum-application-address
          system-info-active-processor-mask system-info-number-of-processors
          system-info-allocation-granularity
          system-info-processor-level system-info-processor-revision))

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

(export '(version version-major version-minor version-build version-platform
          version-service-pack version-service-pack-major
          version-service-pack-minor version-suites version-product-type))

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

(export '(memory-status
          memstat-total-physical memstat-avail-physical memstat-total-page
          memstat-avail-page memstat-total-virtual memstat-avail-virtual))

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
  #+unix (let ((page-size (sysconf :_SC_PAGESIZE)))
           (values (* page-size (sysconf :_SC_PHYS_PAGES))
                   (* page-size (sysconf :_SC_AVPHYS_PAGES))))
  #+win32 (let ((mem-stat (memory-status)))
            (values (memstat-total-physical mem-stat)
                    (memstat-avail-physical mem-stat))))

;;; restore locks
(pushnew "POSIX" *system-package-list* :test #'string=)
(setf (package-lock *system-package-list*) t)
