;;; handle the posix functions
;;; Sam Steingold 1999

(in-package "POSIX" :use '("LISP"))

(export '(resolve-host-ipaddr hostent user-data file-stat sysinfo
          erf erfc j0 j1 jn y0 y1 yn gamma lgamma))

;;; ============================================================
(defstruct hostent
  "see gethostbyname(3) for details"
  (name "" :type simple-string)
  (aliases nil :type list)
  (addr-list nil :type list)
  addrtype)

(defun resolve-host-ipaddr (&optional (host :default))
  (if host
      (multiple-value-bind (name aliases addr-list addrtype)
          (resolve-host-ipaddr-internal host)
        (make-hostent :name name :aliases aliases
                      :addr-list addr-list :addrtype addrtype))
      (let ((li (resolve-host-ipaddr-internal nil)))
        (map-into li (lambda (he)
                       (make-hostent
                        :name (svref he 0) :aliases (svref he 1)
                        :addr-list (svref he 2) :addrtype (svref he 3)))
                  li))))

;;; ============================================================
(defstruct user-data
  "see stat(2) for details"
  (login-id  "" :type simple-string)
  (passwd    "" :type simple-string)
  (uid        0 :type (unsigned-byte 32))
  (gid        0 :type (unsigned-byte 32))
  (full-name "" :type simple-string)
  (home-dir  "" :type simple-string)
  (shell     "" :type simple-string))

(defun user-data (&optional (user :default))
  (if user
      (multiple-value-bind (login-id passwd uid gid full-name home-dir shell)
          (user-data-internal user)
        (make-user-data :login-id login-id :passwd passwd :uid uid :gid gid
                        :full-name full-name :home-dir home-dir :shell shell))
      (let ((li (user-data-internal nil)))
        (map-into li (lambda (ud)
                       (make-user-data
                        :login-id (svref ud 0) :passwd (svref ud 1)
                        :uid (svref ud 2) :gid (svref ud 3)
                        :full-name (svref ud 4) :home-dir (svref ud 5)
                        :shell (svref ud 6)))
                  li))))

;;; ============================================================
(defstruct file-stat
  file
  (dev     0 :type (unsigned-byte 32))
  (ino     0 :type (unsigned-byte 32))
  (mode    0 :type (unsigned-byte 32))
  (nlink   0 :type (unsigned-byte 32))
  (uid     0 :type (unsigned-byte 32))
  (gid     0 :type (unsigned-byte 32))
  (rdev    0 :type (unsigned-byte 32))
  (size    0 :type (unsigned-byte 32))
  (blksize 0 :type (unsigned-byte 32))
  (blocks  0 :type (unsigned-byte 32))
  (atime   0 :type (integer 0))
  (mtime   0 :type (integer 0))
  (ctime   0 :type (integer 0)))

(defun file-stat (file &optional link-p)
  (multiple-value-bind (file dev ino mode nlink uid gid rdev size
                        blksize blocks atime mtime ctime)
      (file-stat-internal file link-p)
    (make-file-stat :file file :dev dev :ino ino :mode mode :nlink nlink
                    :uid uid :gid gid :rdev rdev :size size :blksize blksize
                    :blocks blocks :atime atime :mtime mtime :ctime ctime)))

;;; ============================================================
(defstruct sysinfo
  "see sysinfo(2) and sysconf(3c) for details"
  ;; from sysinfo
  (sysname      "" :type simple-string)
  (hostname     "" :type simple-string)
  (release      "" :type simple-string)
  (version      "" :type simple-string)
  (machine      "" :type simple-string)
  (architecture "" :type simple-string)
  (platform     "" :type simple-string)
  (hw-provider  "" :type simple-string)
  (hw-serial    "" :type simple-string)
  (srpc-domain  "" :type simple-string)
  ;; from sysconf
  (page-size       0 :type (unsigned-byte 32))
  (physical-pages  0 :type (unsigned-byte 32))
  (physical-pages-available 0 :type (unsigned-byte 32))
  (num-processor-conf   0 :type (unsigned-byte 32))
  (num-processor-online 0 :type (unsigned-byte 32))
  ;; from linux kernel
  (bogomips    0.0 :type double-float))

(defun sysinfo ()
  "Return an instance of the sysinfo structure, see sysinfo(2)."
  (multiple-value-bind
        (sysname hostname release version machine architecture
         platform hw-provider hw-serial srpc-domain
         page-size physical-pages physical-pages-available
         num-processor-conf num-processor-online
         bogomips)
      (sysinfo-internal)
    (make-sysinfo :sysname sysname :hostname hostname :release release
                  :version version :machine machine :architecture architecture
                  :platform platform :hw-provider hw-provider
                  :hw-serial hw-serial :srpc-domain srpc-domain
                  :page-size page-size :physical-pages physical-pages
                  :physical-pages-available physical-pages-available
                  :num-processor-conf num-processor-conf
                  :num-processor-online num-processor-online
                  :bogomips bogomips)))
