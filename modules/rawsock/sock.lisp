;; Module for Raw Sockets / CLISP
;; Fred Cohen, 2003-2004
;; Don Cohen, 2003-2004
;; Sam Steingold 2004
;; <http://www.opengroup.org/onlinepubs/007908799/xns/syssocket.h.html>

(defpackage "RAWSOCK"
  (:documentation "Raw Socket access")
  (:use "LISP")
  (:export "BUFFER" "OPEN-UNIX-SOCKET"
           "ACCEPT" "BIND" "CONNECT"
           "GETPEERNAME" "GETSOCKNAME"
           "LISTEN" "RECV" "RECVFROM" "RECVMSG"
           "SEND" "SENDMSG" "SENDTO"
           "SHUTDOWN" "SOCKET" "SOCKETPAIR"
           "SOCK-READ" "SOCK-WRITE" "SOCK-CLOSE" "POLL"
           "SOCKADDR" "MAKE-SOCKADDR" "SOCKADDR-FAMILY" "SOCKADDR-DATA"
           "SOCKADDR-FAMILY-SIZE" "MSGHDR" "MAKE-MSGHDR"
           "CONFIGDEV" "IPCSUM" "ICMPCSUM" "TCPCSUM" "UDPCSUM"))

(IN-PACKAGE "RAWSOCK")
(PUSHNEW :RAWSOCK *FEATURES*)
(PUSH "RAWSOCK" EXT:*SYSTEM-PACKAGE-LIST*)

(macrolet ((missing (type) `(error "~S: missing ~S slot" ',type 'data)))
(defstruct (sockaddr (:constructor make-sa (%data)))
  (%data (missing sockaddr) :read-only t :type (vector (unsigned-byte 8))))
(defstruct (msghdr (:constructor make-msghdr (%data)))
  (%data (missing msghdr) :read-only t :type (vector (unsigned-byte 8))))
)

(defconstant sockaddr-family-size (sockaddr-family-size))
(defun sockaddr-data (sa) (subseq (sockaddr-%data sa) sockaddr-family-size))

(defun open-unix-socket (pathname &optional (type :SOCK_STREAM))
  (let* ((socket (socket :AF_UNIX type 0))
         (address (make-sockaddr :AF_UNIX
                                 (ext:convert-string-to-bytes
                                  (namestring pathname)
                                  ext:*pathname-encoding*))))
    (connect socket address)
    (values socket address)))
