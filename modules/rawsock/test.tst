;; -*- Lisp -*-
;; some tests for RAWSOCK
;; clisp -K full -E utf-8 -q -norc -i ../tests/tests -x '(run-test "rawsock/test")'
;; relies on some functions in the syscalls module

(multiple-value-bind (family total) (rawsock:sockaddr-family-size)
  (defun to-bytes (string) (ext:convert-string-to-bytes string charset:ascii))
  (defun from-bytes (vec &optional size)
    (ext:convert-string-from-bytes vec charset:ascii :end size))
  (defun host->sa (host &optional (port 0))
    ;; note that sockaddr must be of TOTAL size!
    (let* ((he (posix:resolve-host-ipaddr host)) sa
           (ip (first (posix:hostent-addr-list he)))
           (li (read-from-string
                (concatenate 'string "(" (substitute #\Space #\. ip) ")")))
           (ve (make-array (- total family) :element-type '(unsigned-byte 8)
                           :initial-element 0)))
      (show he)
      (setf port (rawsock:htons port)
            (aref ve 0) (ldb #.(byte 8 0) port)
            (aref ve 1) (ldb #.(byte 8 8) port))
      (replace ve li :start1 2)
      (show ve)
      (setq sa (show (rawsock:make-sockaddr :INET ve)))
      (assert (equalp ve (rawsock:sockaddr-data sa)))
      (show (list 'rawsock:sockaddr-family
                  (multiple-value-list (rawsock:sockaddr-family sa))))
      sa))
  (defun local-sa-check (sock sa-local)
    (let* ((sa (rawsock:getsockname sock T))
           (data (rawsock:sockaddr-data sa)))
      (show sa)
      (show (list 'port (+ (aref data 1) (ash (aref data 0) 8))))
      (and (= (rawsock:sockaddr-family sa) (rawsock:sockaddr-family sa-local))
           (equalp (subseq data 2)
                   (subseq (rawsock:sockaddr-data sa-local) 2)))))
  (defvar *sa-remote*) (defvar *sa-local*)
  (defvar *buffer* (make-array 1024 :element-type '(unsigned-byte 8)))
  (defvar *sock*) (defvar *sock1*) (defvar *sock2*)
  (defvar *recv-ret*) #-:win32 (defvar *read-ret*)
  (show (list 'rawsock:sockaddr-family-size
              (multiple-value-list (rawsock:sockaddr-family-size))))
  T) T

(progn (setq *sa-remote* (host->sa "ftp.gnu.org" 21)) T) T
(progn (setq *sa-local* (host->sa :default)) T) T

(integerp (show (setq *sock* (rawsock:socket :INET :STREAM nil)))) T

(unless (equalp #(127 0 0 1) (subseq (rawsock:sockaddr-data *sa-local*) 2 6))
  (rawsock:bind *sock* *sa-local*)
  (not (local-sa-check *sock* *sa-local*)))
NIL
(rawsock:connect *sock* *sa-remote*) NIL
(equalp (rawsock:getpeername *sock* T) *sa-remote*) T

(let ((size (rawsock:recv *sock* *buffer*)))
  (show (setq *recv-ret* (list size (from-bytes *buffer* size))))
  T) T

#+unix
(listp (show (rawsock:socket-option *sock* NIL) :pretty t))
T
#+unix
(listp (show (rawsock:socket-option *sock* NIL :level :ALL) :pretty t))
T

#+unix (setf (rawsock:socket-option *sock* NIL) '(:debug nil))
#+unix (:DEBUG NIL)

#+unix (setf (rawsock:socket-option *sock* NIL :level :all)
             '(:sol-socket (:debug nil)))
#+unix (:SOL-SOCKET (:DEBUG NIL))

#+unix (setf (rawsock:socket-option *sock* :debug) nil)
#+unix NIL

#+unix (setf (rawsock:socket-option *sock* :debug :level :all)
             '(:sol-socket nil))
#+unix (:SOL-SOCKET NIL)

(ext:socket-stream-shutdown *sock* :io) NIL
(rawsock:sock-close *sock*) 0

(let ((so (rawsock:socket :INET :STREAM nil)))
  (show (list so *sock*))
  (= so *sock*))
T

(unless (equalp #(127 0 0 1) (subseq (rawsock:sockaddr-data *sa-local*) 2 6))
  (rawsock:bind *sock* *sa-local*)
  (not (local-sa-check *sock* *sa-local*)))
NIL
(rawsock:connect *sock* *sa-remote*) NIL
(equalp (rawsock:getpeername *sock* T) *sa-remote*) T

#-:win32 ;; on win32, read() cannot be called on a socket!
(let ((size (rawsock:sock-read *sock* *buffer*)))
  (show (setq *read-ret* (list size (from-bytes *buffer* size))))
  T) T

#-:win32 (equal *recv-ret* *read-ret*) T

;; no socketpair() on win32
#-:win32 (progn
  (setf (values *sock1* *sock2*)
        ;; :INET works on cygwin but not on Linux
        (rawsock:socketpair :UNIX :STREAM nil))
  (show (list *sock1* *sock2*))
  T) T

#-:win32
(let ((message "abazonk"))
  (rawsock:sock-write *sock1* (to-bytes message))
  (string= message (from-bytes *buffer* (rawsock:sock-read *sock2* *buffer*))))
T

#-:win32 (rawsock:sock-close *sock1*) 0
#-:win32 (rawsock:sock-close *sock2*) 0

(rawsock:sock-write 1 (to-bytes "foo"))
3

