;; Module for Raw Sockets / CLISP
;; Fred Cohen, 2003-2004
;; Don Cohen, 2003-2004
;; Sam Steingold 2004
;; <http://www.opengroup.org/onlinepubs/007908799/xns/syssocket.h.html>

(defpackage "RAWSOCK"
  (:documentation "Raw Socket access")
  (:use "LISP")
  (:export "SOCKET" "CLOSESOCK" "BUFFER"
           "ACCEPT" "BIND" "CONNECT"
           "GETPEERNAME" "GETSOCKNAME" "GETSOCKOPT"
           "LISTEN" "RECV" "RECVFROM" "RECVMSG"
           "SEND" "SENDMSG" "SENDTO"
           "SETSOCKOPT" "SHUTDOWN" "SOCKET" "SOCKETPAIR"
           "CONFIGDEV" "IPCSUM" "ICMPCSUM" "TCPCSUM" "UDPCSUM"))

(IN-PACKAGE "RAWSOCK")
(PUSHNEW :RAWSOCK *FEATURES*)
(PUSH "RAWSOCK" EXT:*SYSTEM-PACKAGE-LIST*)

