;;; handle the posix functions
;;; Sam Steingold 1999-2004

(defpackage "POSIX"
  (:use "COMMON-LISP" "EXT")
  (:nicknames "OS")
  (:export
   "RESOLVE-HOST-IPADDR" "BOGOMIPS"
   "STREAM-LOCK" "DUPLICATE-HANDLE" "COPY-FILE"
   "HOSTENT" "HOSTENT-NAME" "HOSTENT-ALIASES" "HOSTENT-ADDR-LIST"
   "HOSTENT-ADDRTYPE"
   "ERF" "ERFC" "J0" "J1" "JN" "Y0" "Y1" "YN" "GAMMA" "LGAMMA"))

(setf (package-lock "EXT") nil)
(use-package '("POSIX") "EXT")
(ext:re-export "POSIX" "EXT")
(pushnew :syscalls *features*)
(in-package "POSIX")

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
   crypt encrypt setkey))

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
#+unix (progn
(export
 '(file-stat file-stat-file file-stat-dev file-stat-ino file-stat-mode
   file-stat-nlink file-stat-uid file-stat-gid file-stat-rdev
   file-stat-size file-stat-blksize file-stat-blocks file-stat-atime
   file-stat-mtime file-stat-ctime set-file-stat mknod convert-mode umask))

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

)

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
   sysconf sysconf-page-size sysconf-physical-pages
   sysconf-physical-pages-available sysconf-num-processor-conf
   sysconf-num-processor-online sysconf-max-threads-per-process))

(defstruct (uname (:constructor make-uname (sysname nodename release
                                            version machine)))
  "see uname(2) for details"
  (sysname      "" :type simple-string :read-only t)
  (nodename     "" :type simple-string :read-only t)
  (release      "" :type simple-string :read-only t)
  (version      "" :type simple-string :read-only t)
  (machine      "" :type simple-string :read-only t))
(defstruct (sysconf (:constructor
                     make-sysconf (page-size physical-pages
                                   physical-pages-available
                                   num-processor-conf num-processor-online
                                   max-threads-per-process)))
  "see sysconf(3c) for details"
  (page-size       nil :type (or null (eq t) (unsigned-byte 32)) :read-only t)
  (physical-pages  nil :type (or null (eq t) (unsigned-byte 32)) :read-only t)
  (physical-pages-available nil :type (or null (eq t) (unsigned-byte 32))
                            :read-only t)
  (num-processor-conf nil :type (or null (eq t) (unsigned-byte 32))
                      :read-only t)
  (num-processor-online nil :type (or null (eq t) (unsigned-byte 32))
                        :read-only t)
  (max-threads-per-process nil :type (or null (eq t) (unsigned-byte 32))
                           :read-only t))

(setf (documentation 'sysconf 'function)
      "Return an instance of the SYSCONF structure.
NIL - no such key; T - sysconf(3c) returned -1.")

(export
 '(confstr confstr-path confstr-ilp32-off32-cflags confstr-ilp32-off32-ldflags
   confstr-ilp32-off32-libs confstr-ilp32-offbig-cflags
   confstr-ilp32-offbig-ldflags confstr-ilp32-offbig-libs
   confstr-lp64-off64-cflags confstr-lp64-off64-ldflags
   confstr-lp64-off64-libs confstr-lpbig-offbig-cflags
   confstr-lpbig-offbig-ldflags confstr-lpbig-offbig-libs
   confstr-width-restricted-envs))
(defstruct (confstr (:constructor
                     make-confstr (path ilp32-off32-cflags ilp32-off32-ldflags
                                   ilp32-off32-libs ilp32-offbig-cflags
                                   ilp32-offbig-ldflags ilp32-offbig-libs
                                   lp64-off64-cflags lp64-off64-ldflags
                                   lp64-off64-libs lpbig-offbig-cflags
                                   lpbig-offbig-ldflags lpbig-offbig-libs
                                   width-restricted-envs)))
  "see confstr(3c) for details"
  (path nil :type (or boolean string) :read-only t)
  (ilp32-off32-cflags nil :type (or boolean string) :read-only t)
  (ilp32-off32-ldflags nil :type (or boolean string) :read-only t)
  (ilp32-off32-libs nil :type (or boolean string) :read-only t)
  (ilp32-offbig-cflags nil :type (or boolean string) :read-only t)
  (ilp32-offbig-ldflags nil :type (or boolean string) :read-only t)
  (ilp32-offbig-libs nil :type (or boolean string) :read-only t)
  (lp64-off64-cflags nil :type (or boolean string) :read-only t)
  (lp64-off64-ldflags nil :type (or boolean string) :read-only t)
  (lp64-off64-libs nil :type (or boolean string) :read-only t)
  (lpbig-offbig-cflags nil :type (or boolean string) :read-only t)
  (lpbig-offbig-ldflags nil :type (or boolean string) :read-only t)
  (lpbig-offbig-libs nil :type (or boolean string) :read-only t)
  (width-restricted-envs nil :type (or boolean string) :read-only t))

(setf (documentation 'confstr 'function)
      "Return an instance of the CONFSTR structure.
NIL - no such key; T - the parameter is not set.")
)
;;; ============================================================
#+unix (progn
(export
 '(rlimit rlimit-soft rlimit-hard
   limits limits-core limits-cpu limits-heap limits-file-size limits-num-files
   limits-stack limits-virt-mem limits-rss limits-memlock
   usage usage-user-time usage-system-time usage-max-rss usage-int-rss
   usage-minor-page-faults usage-major-page-faults usage-num-swaps
   usage-blocks-input usage-blocks-output usage-messages-sent
   usage-messages-received usage-signals usage-context-switches-voluntary
   usage-context-switches-involuntary))

(defstruct (rlimit (:constructor make-rlimit (soft hard)))
  "see getrlimit(2) for details"
  (soft nil :type (or null (unsigned-byte 32)) :read-only t)
  (hard nil :type (or null (unsigned-byte 32)) :read-only t))

(defstruct (limits (:constructor make-limits (core cpu heap file-size num-files
                                              stack virt-mem rss memlock)))
  "see getrlimit(2) for details"
  (core nil :type (or null rlimit) :read-only t)
  (cpu  nil :type (or null rlimit) :read-only t)
  (heap nil :type (or null rlimit) :read-only t)
  (file-size nil :type (or null rlimit) :read-only t)
  (num-files nil :type (or null rlimit) :read-only t)
  (stack nil :type (or null rlimit) :read-only t)
  (virt-mem nil :type (or null rlimit) :read-only t)
  (rss nil :type (or null rlimit) :read-only t)
  (memlock nil :type (or null rlimit) :read-only t))

(defstruct (usage (:constructor
                   make-usage (user-time system-time max-rss int-rss
                               minor-page-faults major-page-faults num-swaps
                               blocks-input blocks-output
                               messages-sent messages-received signals
                               context-switches-voluntary
                               context-switches-involuntary)))
  "see getrusage(3) for details"
  (user-time 0.0d0 :type double-float :read-only t)
  (system-time 0.0d0 :type double-float :read-only t)
  (max-rss 0 :type (signed-byte 32) :read-only t)
  (int-rss 0 :type (signed-byte 32) :read-only t)
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

)

;;; restore locks
(push "POSIX" *system-package-list*)
(setf (package-lock *system-package-list*) t)
