;; -*- Lisp -*-
;; some tests for RAWSOCK
;; clisp -K full -E utf-8 -q -norc -i ../tests/tests -x '(run-test "rawsock/test")'
;; relies on some functions in the syscalls module

(defun to-bytes (string) (ext:convert-string-to-bytes string charset:ascii))
to-bytes

(let* ((he (posix:resolve-host-ipaddr "ftp.gnu.org"))
       (ip (first (posix:hostent-addr-list he)))
       (li (read-from-string (concatenate
                              'string "(" (substitute #\Space #\. ip) ")")))
       (ve (coerce li '(vector (unsigned-byte 8)))))
  (print he)
  (print li)
  (print ve)
  (setq sa (print (rawsock:make-sockaddr :AF_INET ve)))
  (equalp ve (rawsock:sockaddr-data sa)))
T

(integerp (print (setq sock (rawsock:socket :AF_INET :SOCK_STREAM nil))))
T

(progn
  (setf (values sock1 sock2) (rawsock:socketpair :AF_INET :SOCK_STREAM nil))
  (print (list sock1 sock2))
  T) T

(rawsock:sock-write 1 (to-bytes "foo"))
3

(rawsock:sock-close sock)  0
(rawsock:sock-close sock1) 0
(rawsock:sock-close sock2) 0
