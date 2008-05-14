;; -*- Lisp -*- vim:filetype=lisp
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

(os:erf -6)     -1.0d0
(os:erf -5)     -0.9999999999984626d0
(os:erf -4)     -0.9999999845827421d0
(os:erf -3)     -0.9999779095030014d0
(os:erf -2)     -0.9953222650189527d0
(os:erf -1)     -0.8427007929497149d0
(os:erf 0)      0.0d0
(os:erf 1)      0.8427007929497149d0
(os:erf 2)      0.9953222650189527d0
(os:erf 3)      0.9999779095030014d0
(os:erf 4)      0.9999999845827421d0
(os:erf 5)      0.9999999999984626d0
(os:erf 6)      1.0d0

(os:erfc -6)    2.0d0
(os:erfc -5)    1.9999999999984626d0
(os:erfc -4)    1.999999984582742d0
(os:erfc -3)    1.9999779095030015d0
(os:erfc -2)    1.9953222650189528d0
(os:erfc -1)    1.842700792949715d0
(os:erfc 0)     1.0d0
(os:erfc 1)     0.15729920705028513d0
(os:erfc 2)     0.004677734981047265d0
(os:erfc 3)     2.2090496998585438d-5
(os:erfc 4)     1.541725790028002d-8
(os:erfc 5)     1.5374597944280351d-12
(os:erfc 6)     2.1519736712498916d-17
(os:erfc 7)     4.183825607779414d-23
(os:erfc 8)     1.1224297172982928d-29
(os:erfc 9)     4.13703174651381d-37
(os:erfc 10)    2.088487583762545d-45
(os:erfc 11)    1.4408661379436945d-54
(os:erfc 12)    1.3562611692059042d-64
(os:erfc 13)    1.7395573154667246d-75
(os:erfc 14)    3.037229847750312d-87
(os:erfc 15)    7.212994172451206d-100
(os:erfc 16)    2.3284857515715308d-113
(os:erfc 17)    1.0212280150942608d-127
(os:erfc 18)    6.082369231816399d-143
(os:erfc 19)    4.917722839256475d-159
(os:erfc 20)    5.3958656116079005d-176
(os:erfc 21)    8.032453871022456d-194
(os:erfc 22)    1.6219058609334724d-212
(os:erfc 23)    4.441265948088058d-232
(os:erfc 24)    1.6489825831519335d-252
(os:erfc 25)    8.300172571196522d-274
(os:erfc 26)    5.663192408856143d-296
(os:erfc 30)    0.0d0

(loop :for i :from -10 :to 10 :always (= 1 (+ (os:erf i) (os:erfc i)))) T

(os:j0 0)       1.0d0
(os:j0 1.0)     0.7651976865579666d0
(os:j1 0)       0.0d0
(os:j1 1.0)     0.4400505857449335d0
(os:jN 0 0)     1.0d0
(os:jN 1 1.0)   0.4400505857449335d0
(os:jN 2 1.0)   0.11490348493190049d0
(os:jN 2 1)     0.11490348493190049d0
(os:jN 2 0)     0.0d0
(os:y0 1.0)     0.08825696421567698d0
(os:y0 10.0)    0.055671167283599395d0
(os:y1 1.0)     -0.7812128213002887d0
(os:y1 10.0)    0.2490154242069538d0
(os:yN 2 1.0)   -1.6506826068162543d0
(os:yN 2 10.0)  -0.0058680824422086345d0
(multiple-value-list (os:lgamma 2))   (0.0d0 1)
(mapcar (lambda (n)
          (multiple-value-bind (lg s) (os:lgamma n)
            (list n (float (/ (! (1- n)) (exp lg)) 0f0) s)))
        '(3 5 10 15 30 50 100))
((3 1f0 1) (5 1f0 1) (10 1f0 1) (15 1f0 1) (30 1f0 1) (50 1f0 1) (100 1f0 1))

(loop :for n :upfrom 3 :for lg = (os:lgamma n)
  :unless (= 1 (/ (log (! (1- n))) lg)) :return n)
29                    ; not bad...

(loop :for n :upfrom 3
  :for lg = (handler-case (os:lgamma n)
              (floating-point-overflow () 'floating-point-overflow))
  :for l! = (handler-case (log (float (! (1- n)) lg))
              (floating-point-overflow () 'floating-point-overflow))
  :unless (and (floatp lg) (floatp l!) (= 1 (float (/ l! lg) 0f0)))
  :return (list n lg l!))
(172 711.71472580229d0 FLOATING-POINT-OVERFLOW)

(loop :for n :upfrom 3 :for tg = (os:tgamma n)
  :unless (= 1 (/ (! (1- n)) tg)) :return n)
5               ; OUCH! tgamma is not precize at double precision!

(loop :for n :upfrom 3
  :for tg = (handler-case (os:tgamma n)
              (floating-point-overflow () 'floating-point-overflow))
  :unless (and (floatp tg) (= 1 (float (/ (! (1- n)) tg) 0f0)))
  :return (list n tg))
(172 FLOATING-POINT-OVERFLOW) ; ... but it IS precize at single precision!

#+unix (= (show (os:process-id)) (show (os:getppid))) NIL
#+unix (let ((id (show (os:getuid)))) (= id (setf (os:getuid) id))) T
#+unix (let ((id (show (os:getgid)))) (= id (setf (os:getgid) id))) T
#+unix (let ((id (show (os:geteuid)))) (= id (setf (os:geteuid) id))) T
#+unix (let ((id (show (os:getegid)))) (= id (setf (os:getegid) id))) T
#+unix (let* ((pid (os:process-id))
              (id (show (os:getpgid pid))))
         (= id (setf (os:getpgid pid) id))) #+unix T
#+unix (= (os:getuid) (os:geteuid)) T
#+unix (= (os:getgid) (os:getegid)) T
#+unix (multiple-value-list (os:setreuid (os:getuid) (os:geteuid))) NIL
#+unix (multiple-value-list (os:setregid (os:getgid) (os:getegid))) NIL
#+unix (multiple-value-list (os:setreuid -1 -1)) NIL
#+unix (multiple-value-list (os:setregid -1 -1)) NIL
#+unix (listp (show (if (fboundp 'os:getgroups) (os:getgroups)
                        '(no os:getgroups)) :pretty t)) T
#+unix (if (and (fboundp 'os:getgroups) (fboundp 'os::%setgroups))
           (let ((g (os:getgroups))) ; setgroups may fail with EPERM
	     (eq g (or (ignore-errors (setf (os:getgroups) g)) g)))
           t) T

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

#+unix (listp (show (os:user-info) :pretty t)) T
;; (os:user-info :default) calls getlogin which may fail in cron
#+unix (os:user-info-p
        (handler-case (show (os:user-info :default) :pretty t)
          (:error (c) (princ-error c)
                  (use-value (os:make-user-info "" "" 0 0 "" "" ""))))) T
;; some SF CF hosts (solaris, openbsd) are misconfigured:
;; user GID is 100, but there is no group with GID 100
#+unix (os:group-info-p
        (show
         (handler-bind ((error (lambda (c) (princ-error c) (use-value 0))))
           (os:group-info (os:user-info-gid (os:user-info (os:getuid)))))
         :pretty t)) T
#+unix (listp (show (os:group-info) :pretty t)) T
#+unix (= (os:getuid) (os:user-info-uid (os:user-info (os:getuid)))) T
#+unix (= (os:getgid) (os:user-info-gid (os:user-info (os:getuid)))) T

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

#+ffi (defparameter *foo* (os:fopen "foo" "w")) #+ffi *foo*
;; #+ffi (os:fputc 65 *foo*) #+ffi 65
#+ffi (os:feof *foo*) #+ffi NIL
#+ffi (os:ferror *foo*) #+ffi NIL
#+ffi (os:clearerr *foo*) #+ffi NIL
#+ffi (os:fflush *foo*) #+ffi NIL
#+ffi (os:fclose *foo*) #+ffi NIL
#+ffi (defparameter *foo* (os:fopen "foo" "r")) #+ffi *foo*
;; #+ffi (os:fgetc *foo*) #+ffi 65
;; #+ffi (os:fgetc *foo*) #+ffi -1
#+ffi (os:feof *foo*) #+ffi NIL
#+ffi (os:ferror *foo*) #+ffi NIL
#+ffi (os:clearerr *foo*) #+ffi NIL
#+ffi (os:fclose *foo*) #+ffi NIL
#+ffi (null (delete-file "foo")) #+ffi NIL


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
