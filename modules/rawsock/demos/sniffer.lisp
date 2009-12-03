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
  (format t " len=~:D" (length buffer))
  (loop :for byte :across buffer :do (format t " ~2,'0X"  byte))
  (terpri))

(defun print-sockaddr (device)
  (let ((family (rawsock:sockaddr-family device)))
    (format t "family: ~A " family)
    (case family
      (:UNIX
       (loop :for c :across (rawsock:sockaddr-data device)
         :do (format t "~c" (if (= c 0) #\space (code-char c )))))
      (t (prin1 (rawsock:sockaddr-data device))))))

(defun my-open-socket (domain)
  (cond ((string= domain "inet")
         (rawsock:socket :inet :packet #x300))
        ((string= domain "packet")
         (rawsock:socket :packet :raw #x300))
        (t (error "invalid socket domain ~S" domain))))

(defun sniff (domain repeat)
  (let ((socket (my-open-socket domain)))
    (unwind-protect
         (loop
           :with buffer = (make-array *buffer-len* :fill-pointer 0
                                      :element-type '(unsigned-byte 8))
           :and device = (rawsock:make-sockaddr :UNSPEC)
           :repeat repeat :do
           (my-rcvfrom socket buffer device)
           (print-sockaddr device)
           (print-buffer buffer))
      (rawsock:sock-close socket))))

(unless (= 2 (length *args*))
  (error "Expected: ({inet|packet} repeat-count), got ~S" *args*))

(sniff (pop *args*) (parse-integer (pop *args*)))
