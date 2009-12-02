;;; Packet sniffer
;; run as root like this:
;; sudo clisp sniffer.lisp inet 100

(require "rawsock")

(defvar *buffer-len* 1518)

(defun my-rcvfrom (socket buffer device)
  (setf (fill-pointer buffer) 1518)
  (let ((len (rawsock:recvfrom socket buffer device)))
    (setf (fill-pointer buffer) len)))

(defun print-buffer (buffer)
  (format t "len=~:D" (length buffer))
  (loop :for byte :across buffer :do (format t " ~2,'0X"  byte))
  (terpri))

(defun sniff (socket repeat)
  (loop
    :with buffer = (make-array *buffer-len* :element-type '(unsigned-byte 8)
                               :fill-pointer 0)
    :and device = (rawsock:make-sockaddr :UNSPEC)
    :repeat repeat :do
    (my-rcvfrom socket buffer device)
    (format t "~%family ~a " (rawsock:sockaddr-family device))
    (loop :for c :across (rawsock:sockaddr-data device)
      :do (format t "~c" (if (= c 0) #\space (code-char c ))))
    (print-buffer buffer)))

(unless (= 2 (length *args*))
  (error "Expected: ({inet|packet} repeat-count), got ~S" *args*))

(defparameter *socket*
  (let ((arg (pop *args*)))
    (cond ((string= arg "inet")
           (rawsock:socket :inet :packet #x300))
          ((string= arg "packet")
           (rawsock:socket :packet :raw #x300))
          (t (error "invalid socket argument ~S" arg)))))
(unless (plusp *socket*)
  (error "Invalid socket created for ~S: ~S" *args* *socket*))

(sniff *socket* (parse-integer (pop *args*)))
