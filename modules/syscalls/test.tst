;; -*- Lisp -*-
;; some tests for SYSCALLS
;; clisp -E 1:1 -q -norc -i ../tests/tests -x '(run-test "syscalls/test")'

(listp (show (multiple-value-list (ext:module-info "syscalls" t)) :pretty t)) T

(os:hostent-p (show (os:resolve-host-ipaddr "localhost")))
T

(listp (show (os:resolve-host-ipaddr) :pretty t)) T
(os:service-p (show (os:service "smtp"))) T
(os:service-p (show (os:service 25))) T
(listp (show (os:service) :pretty t)) T

#+unix ;; (encrypt (encrypt X t) nil) == X
(handler-case
    (let* ((v (make-array 8 :element-type '(unsigned-byte 8))) (u (copy-seq v)))
      (loop :repeat 10 :do
        (dotimes (i 8) (setf (aref v i) (setf (aref u i) (random 256))))
        (os:setkey v) (show (os:encrypt v nil)) (show (os:encrypt v t))
        :never (if (equalp v u) nil (list v u))))
  (system::simple-os-error (err)
    ;; Solaris (sf cf x86-solaris1 & sparc-solaris1) encrypt fails with
    ;;  "UNIX error 89 (ENOSYS): Function not implemented"
    (format t "~S: ~A" 'os:encrypt err)
    T))
#+unix T

#+unix (crypt "foo" "bar") #+unix "ba4TuD1iozTxw"

#+unix
(let* ((fmt "%Y-%m-%d %T") (string (show (os:string-time fmt))))
  (string= string (os:string-time fmt (show (os:string-time fmt string)))))
#+unix T

#+unix
(when (fboundp 'os:getutxent)
  (not (integerp (show (length (loop :for utmpx = (os:getutxent) :while utmpx
                                 :collect (show utmpx :pretty t)))))))
#+unix NIL

(defparameter *tmp1* (os:mkstemp "syscalls-tests-")) *tmp1*
(defparameter *tmp2* (os:mkstemp "syscalls-tests-")) *tmp2*

(let ((*standard-output* (make-broadcast-stream
                          *standard-output* *tmp1* *tmp2*)))
  (show (write *tmp1* :stream *tmp1*)) (terpri *tmp1*)
  (show (write *tmp2* :stream *tmp2*)) (terpri *tmp2*)
  T)
T

#+unix (find :rdwr (show (os:stream-options *tmp1* :fl))) #+unix :RDWR
#+unix (ext:appease-cerrors
        (with-open-file (s *tmp1*)
          (find :rdonly (show (os:stream-options s :fl)))))
#+unix :RDONLY
#+unix (os:stream-options *tmp1* :fd) NIL
#+unix (os:stream-options *tmp1* :fd '(:cloexec)) NIL
#+unix (os:stream-options *tmp1* :fd) #+unix (:cloexec)
#+unix (os:stream-options *tmp1* :fd nil) NIL
#+unix (os:stream-options *tmp1* :fd) NIL

;; may fail with ENOLCK or EOPNOTSUPP - in which case we do not test locking
(handler-case (os:stream-lock *tmp1* t)
  (system::simple-file-error (err)
    (format t "~S: ~A" 'os:stream-lock err)
    (pushnew :no-stream-lock *features*)
    T))
T
#-:no-stream-lock (os:stream-lock *tmp1* nil) #-:no-stream-lock NIL

(typep (show (os:priority (os:process-id))) '(or keyword (integer -20 20))) T

#+unix (let ((id (show (os:getuid)))) (= id (setf (os:getuid) id))) T
#+unix (let ((id (show (os:getgid)))) (= id (setf (os:getgid) id))) T
#+unix (let ((id (show (os:geteuid)))) (= id (setf (os:geteuid) id))) T
#+unix (let ((id (show (os:getegid)))) (= id (setf (os:getegid) id))) T
#+unix (= (os:getuid) (os:geteuid)) T
#+unix (= (os:getgid) (os:getegid)) T

#+unix (listp (show (if (fboundp 'os:sysconf)
                        (os:sysconf) '(no os:sysconf)) :pretty t)) T
#+unix (listp (show (if (fboundp 'os:confstr)
                        (os:confstr) '(no os:confstr)) :pretty t)) T

#+unix (listp (show (if (fboundp 'os:pathconf)
                        (os:pathconf "/") '(no os:pathconf)) :pretty t)) T
#+unix (listp (show (if (fboundp 'os:pathconf)
                        (os:pathconf *tmp1*) '(no os:pathconf)) :pretty t)) T

#+unix
(listp (show (if (fboundp 'os:usage)
                 (multiple-value-list (os:usage)) '(no os:usage)) :pretty t))
T
#+unix
(listp (show (if (fboundp 'os:rlimit)
                 (multiple-value-list (os:rlimit)) '(no os:rlimit)) :pretty t))
T

#+unix (os:uname-p (show (os:uname) :pretty t)) #+unix T

#+unix (os:user-info-p (show (os:user-info :default) :pretty t)) T
#+unix (listp (show (os:user-info) :pretty t)) T
;; some SF CF hosts (solaris, openbsd) are misconfigured:
;; user GID is 100, but there is no group with GID 100
#+unix (os:group-info-p
        (show
         (handler-bind ((error (lambda (c) (princ-error c) (use-value 0))))
           (os:group-info (os:user-info-gid (os:user-info :default)))))) T
#+unix (listp (show (os:group-info) :pretty t)) T
#+unix (= (os:getuid) (os:user-info-uid (os:user-info :default))) T
#+unix (= (os:getgid) (os:user-info-gid (os:user-info :default))) T

(os:file-stat-p (show (os:file-stat *tmp1*) :pretty t)) T
(os:file-stat-p (show (os:file-stat (pathname *tmp1*)) :pretty t)) T

#-:win32 (= (posix:file-stat-ino (os:file-stat *tmp1*))
            (posix:file-stat-ino (os:file-stat *tmp2*)))
#-:win32 NIL

(os:convert-mode #o0666)
#+unix (:RUSR :WUSR :RGRP :WGRP :ROTH :WOTH)
#+win32 (:RUSR :WUSR 54)
#-(or unix win32) ERROR

(os:convert-mode '(:RWXU #+unix :RWXG #+unix :RWXO))
#+unix #o0777
#+win32 #o0700
#-(or unix win32) ERROR

(and (fboundp 'os:stat-vfs)
     (not (os:stat-vfs-p (show (os:stat-vfs *tmp2*) :pretty t))))
NIL

(string= (show #+win32 (ext:string-concat (ext:getenv "USERDOMAIN") "\\"
                                          (ext:getenv "USERNAME"))
               #+unix (ext:getenv "USER")
               #-(or unix win32) ERROR)
         (show (os:file-owner *tmp1*)))
T

(progn (close *tmp1*) (close *tmp2*) T) T

(listp (show (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (show (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (show (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (show (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (show (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (show (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (show (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (show (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (show (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (show (os:copy-file *tmp2* *tmp1* :if-exists :append))) T

(integerp (show (with-open-file (s *tmp1* :direction :input) (file-length s))))
T

(integerp (show (with-open-file (s *tmp2* :direction :input) (file-length s))))
T

;; win32 functions barf on cygwin pathnames
#+win32 (os:file-info-p (show (os:file-info *tmp2*) :pretty t)) T
#+win32 (listp (show (os:file-info (make-pathname :name "syscalls-tests-*"
                                                  :defaults *tmp2*)
                                   t)
                     :pretty t))
T

#+(or win32 cygwin)
(os:system-info-p (show (os:system-info) :pretty t))
#+(or win32 cygwin) T
#+(or win32 cygwin)
(os:version-p (show (os:version) :pretty t))
T
#+(or win32 cygwin)
(os:memory-status-p (show (os:memory-status)))
T

(let ((sysconf #+unix (os:sysconf) #-unix nil))
  ;; guard against broken unixes, like FreeBSD 4.10-BETA
  (if #+unix (and (getf sysconf :PAGESIZE)
                  (getf sysconf :PHYS-PAGES)
                  (getf sysconf :AVPHYS-PAGES))
      #-unix T
      (listp (show (multiple-value-list (os:physical-memory))))
      T))
T

;; test file locking
(let ((buf (make-array 100 :fill-pointer t :adjustable t
                       :element-type 'character))
      #-:win32
      (timeout
       (let* ((uname (os:uname)))
         (cond ((and (string= (os:uname-sysname uname) "Linux")
                     (string= (os:uname-machine uname) "ppc64"))
                (format t "~&~S: increase timeout for openpower-linux1~%"
                        'flush-clisp)
                100)
               (t 1)))))
  (defun flush-clisp (stream)
    (when #-:win32 (socket:socket-status (cons stream :input) timeout)
          ;; select on win32 does not work with pipes
          #+:win32 (progn (sleep 1) (listen stream))
      ;; read from the clisp stream until the next prompt
      (setf (fill-pointer buf) 0)
      (loop :with pos-NL = 0 :for ch = (read-char stream)
        :until (and (char= ch #\Space) (char= #\[ (char buf pos-NL))
                    (let ((pos1 (position #\] buf :start pos-NL)))
                      (and pos1 (char= #\> (char buf (1+ pos1))))))
        :do (when (char= ch #\Newline) (setq pos-NL (1+ (length buf))))
        (vector-push-extend ch buf))
      (show buf))))
FLUSH-CLISP

(defun proc-send (proc fmt &rest args)
  (apply #'format proc fmt args)
  (terpri proc) (force-output proc)
  (flush-clisp proc))
PROC-SEND

(let* ((argv (ext:argv)) (run (aref argv 0))
       (args (list "-M" (aref argv (1+ (position "-M" argv :test #'string=)))
                   "-B" (aref argv (1+ (position "-B" argv :test #'string=)))
                   "-norc" "-q" "-on-error" "abort")))
  (show (cons run args) :pretty t)
  (defparameter *proc1* (ext:run-program run :arguments args
                                         :input :stream :output :stream))
  (defparameter *proc2* (ext:run-program run :arguments args
                                         :input :stream :output :stream))
  (flush-clisp *proc1*)
  (flush-clisp *proc2*)
  t)
T

(stringp
 (proc-send *proc1* "(setq s (open ~S :direction :output :if-exists :append))"
            (truename *tmp1*)))
T
(stringp
 (proc-send *proc2* "(setq s (open ~S :direction :output :if-exists :append))"
            (truename *tmp1*)))
T

#-:no-stream-lock (read-from-string (proc-send *proc1* "(stream-lock s t)"))
#-:no-stream-lock T
#-:no-stream-lock (proc-send *proc2* "(stream-lock s t)")
#-:no-stream-lock NIL           ; blocked

#-:no-stream-lock (read-from-string (proc-send *proc1* "(stream-lock s nil)"))
#-:no-stream-lock NIL           ; released
#-:no-stream-lock (read-from-string (flush-clisp *proc2*))
#-:no-stream-lock T             ; acquired

#-:no-stream-lock (read-from-string (proc-send *proc1* "(stream-lock s t :block nil)"))
#-:no-stream-lock NIL
#-:no-stream-lock (read-from-string (proc-send *proc2* "(stream-lock s nil)"))
#-:no-stream-lock NIL           ; released
#-:no-stream-lock (read-from-string (proc-send *proc1* "(stream-lock s t :block nil)"))
#-:no-stream-lock T
#-:no-stream-lock (read-from-string (proc-send *proc1* "(stream-lock s nil)"))
#-:no-stream-lock NIL           ; released

(multiple-value-list (os:sync)) ()

;; check :rename-and-delete
;; woe32 signals ERROR_SHARING_VIOLATION
;; when renaming a file opened by a different process
#-win32
(let ((inode (show (posix:file-stat-ino (posix:file-stat *tmp1*)))))
  (with-open-stream (s (ext:run-program
                        "tail" :arguments (list "-f" (namestring *tmp1*)
                                                (format nil "--pid=~D"
                                                        (os:process-id)))
                        :output :stream))
    (with-open-file (new *tmp1* :direction :output
                         :if-exists :rename-and-delete)
      (= inode (show (posix:file-stat-ino (posix:file-stat new)))))))
#-win32 NIL

(let ((file "foo.bar") (dates '(3141592653 3279321753)))
  (unwind-protect
       (progn (with-open-file (s file :direction :output) (write s :stream s))
              (loop :for d :in dates :do (posix:set-file-stat file :mtime d)
                :collect (= d (with-open-file (s file) (file-write-date s)))))
    (delete-file file)))
(T T)

(progn (proc-send *proc1* "(close s)(ext:quit)")
       (close (two-way-stream-input-stream *proc1*))
       (close (two-way-stream-output-stream *proc1*))
       (close *proc1*) (makunbound '*proc1*) (unintern '*proc1*)
       (proc-send *proc2* "(close s)(ext:quit)" )
       (close (two-way-stream-input-stream *proc2*))
       (close (two-way-stream-output-stream *proc2*))
       (close *proc2*) (makunbound '*proc2*) (unintern '*proc2*)
       (delete-file *tmp1*) (makunbound '*tmp1*) (unintern '*tmp1*)
       (delete-file *tmp2*) (makunbound '*tmp2*) (unintern '*tmp2*)
       (fmakunbound 'flush-clisp) (unintern 'flush-clisp)
       (fmakunbound 'proc-send) (unintern 'proc-send)
       (setq *features* (delete :no-stream-lock *features*))
       T)
T
