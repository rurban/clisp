;; -*- Lisp -*-
;; some tests for SYSCALLS
;; clisp -E utf-8 -q -norc -i ../tests/tests -x '(run-test "syscalls/test")'

(os:hostent-p (princ (os:resolve-host-ipaddr "localhost")))
T

(defparameter *tmp1* (os:mkstemp "syscalls-tests-")) *tmp1*
(defparameter *tmp2* (os:mkstemp "syscalls-tests-")) *tmp2*

(let ((*standard-output* (make-broadcast-stream
                          *standard-output* *tmp1* *tmp2*)))
  (princ (write *tmp1* :stream *tmp1*)) (terpri)
  (princ (write *tmp2* :stream *tmp2*))
  T)
T

(os:stream-lock *tmp1* t) T
(os:stream-lock *tmp1* nil) NIL

(os:priority (os:process-id))
:NORMAL

#+unix
(listp (princ (if (fboundp 'os:sysconf) (os:sysconf) '(no os:sysconf)))) T
#+unix
(listp (princ (if (fboundp 'os:confstr) (os:confstr) '(no os:confstr)))) T

#+unix
(listp (princ (if (fboundp 'os:usage)
                  (multiple-value-list (os:usage)) '(no os:usage))))
T
#+unix
(listp (princ (if (fboundp 'os:rlimit)
                  (multiple-value-list (os:rlimit)) '(no os:rlimit))))
T

#+unix
(os:uname-p (princ (os:uname))) T
#+unix
(os:user-data-p (princ (os:user-data (ext:getenv "USER")))) T

(os:file-stat-p (princ (os:file-stat *tmp1*))) T
(os:file-stat-p (princ (os:file-stat (pathname *tmp1*)))) T
(os:set-file-stat *tmp2* :atime t :mtime t) NIL

(os:convert-mode #o0666)
#+unix (:RUSR :WUSR :RGRP :WGRP :ROTH :WOTH)
#+win32 (:RUSR :WUSR 54)
#-(or unix win32) ERROR

(os:convert-mode '(:RWXU #+unix :RWXG #+unix :RWXO))
#+unix #o0777
#+win32 #o0700
#-(or unix win32) ERROR

(os:stat-vfs-p (princ (os:stat-vfs *tmp2*))) T

(string= #+win32 (ext:string-concat (ext:getenv "USERDOMAIN") "\\"
                                    (ext:getenv "USERNAME"))
         #+unix (ext:getenv "USER")
         #-(or unix win32) ERROR
         (os:file-owner *tmp1*))
T

(progn (close *tmp1*) (close *tmp2*) T) T

(listp (princ (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (princ (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (princ (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (princ (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (princ (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (princ (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (princ (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (princ (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (princ (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (princ (os:copy-file *tmp2* *tmp1* :if-exists :append))) T

(integerp (princ (with-open-file (s *tmp1* :direction :input) (file-length s))))
T

(integerp (princ (with-open-file (s *tmp2* :direction :input) (file-length s))))
T

#+win32                      ; win32 functions barf on cygwin pathnames
(os:file-info-p (princ (os:file-info *tmp2*)))
#+win32 T

#+(or win32 cygwin)
(os:system-info-p (princ (os:system-info)))
#+(or win32 cygwin) T
#+(or win32 cygwin)
(os:version-p (princ (os:version)))
T
#+(or win32 cygwin)
(os:memory-status-p (princ (os:memory-status)))
T

(listp (princ (multiple-value-list (os:physical-memory))))
T

(progn (delete-file *tmp1*) (makunbound '*tmp1*) (unintern '*tmp1*)
       (delete-file *tmp2*) (makunbound '*tmp2*) (unintern '*tmp2*)
       T)
T
