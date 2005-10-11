;; Module for Raw Sockets / CLISP
;; Fred Cohen, 2003-2004
;; Don Cohen, 2003-2004
;; Sam Steingold 2004-2005
;; <http://www.opengroup.org/onlinepubs/007908799/xns/syssocket.h.html>

(defpackage #:rawsock
  (:documentation "Raw Socket access")
  (:use #:lisp)
  (:shadowing-import-from "EXPORTING" #:defun)
  (:export #:buffer #:resize-buffer #:accept #:bind #:connect
           #:getpeername #:getsockname #:protocol #:protocol-p
           #:protocol-name #:protocol-aliases #:protocol-proto
           #:network #:network-p #:network-name #:network-aliases
           #:network-type #:network-net
           #:sock-listen #:recv #:recvfrom #:recvmsg
           #:send #:sendmsg #:sendto #:socket-option
           #:socket #:socketpair #:sockatmark
           #:sock-read #:sock-write #:sock-close
           #:sockaddr #:make-sockaddr #:sockaddr-family #:sockaddr-p
           #:msghdr #:msghdr-p #:make-msghdr
           #:htonl #:htons #:ntohl #:ntohs #:convert-address
           #:configdev #:ipcsum #:icmpcsum #:tcpcsum #:udpcsum))

(in-package "RAWSOCK")
(pushnew :rawsock *features*)
(pushnew "RAWSOCK" custom:*system-package-list* :test #'string=)

(macrolet ((missing (type) `(error "~S: missing ~S slot" ',type 'data)))
(defstruct (sockaddr (:constructor make-sa (%data)))
  (%data (missing sockaddr) :read-only t :type (vector (unsigned-byte 8))))
(defstruct (msghdr (:constructor make-msghdr (%data)))
  (%data (missing msghdr) :read-only t :type (vector (unsigned-byte 8))))
)

(defstruct (protocol (:constructor make-protocol (name aliases proto)))
  (name "" :type string)
  (aliases nil :type list)
  (proto 0 :type integer))

(defstruct (network (:constructor make-network (name aliases type net)))
  (name "" :type string)
  (aliases nil :type list)
  (type 0 :type integer)
  (net 0 :type integer))

(defsetf socket-option (&rest args) (value) `(set-socket-option ,value ,@args))

(defun sockaddr-data (sa)
  (let ((%data (sockaddr-%data sa)) (offset #,(sockaddr-slot :data)))
    (make-array (- (length %data) offset) :displaced-to %data
                :displaced-index-offset offset
                :element-type '(unsigned-byte 8))))

(defun open-unix-socket (pathname &optional (type :SOCK_STREAM))
  "Return the socket (fixnum) pointing to this UNIX socket special device."
  (let* ((socket (socket :AF_UNIX type 0))
         (address (make-sockaddr :AF_UNIX
                                 (ext:convert-string-to-bytes
                                  (namestring (ext:absolute-pathname pathname))
                                  ext:*pathname-encoding*))))
    (connect socket address)
    (values socket address)))

(defun open-unix-socket-stream (pathname &rest opts &key (type :SOCK_STREAM)
                                &allow-other-keys)
  "Return the lisp STREAM pointing to this UNIX socket special device.
The return value is already FINALIZEd by CLOSE.
Passes :TYPE to SOCKET and all the other options to MAKE-STREAM."
  (multiple-value-bind (sock address) (open-unix-socket pathname type)
    (setq opts (ext:remove-plist opts :type))
    (let ((stream (apply #'ext:make-stream sock opts)))
      (ext:finalize stream #'close)
      (sock-close sock)
      (values stream address))))

(ext:without-package-lock ("CL")
(defmethod close ((sock integer) &key abort)
  (declare (ignore abort))
  (sock-close sock))
)
