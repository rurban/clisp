;;; Packet sniffer
;; run as root like this:
;; $ sudo clisp sniffer.lisp
;; or
;; $ sudo clisp sniffer.lisp -domain packet -type raw

(require "rawsock")

;; parse command line
(defun get-arg-ht ()
  (loop :with ht = (make-hash-table :test 'equal)
    :for (key val) :on *args* :by #'cddr :do
    (unless val (error "Odd number of arguments: ~S" *args*))
    (unless (char= #\- (char key 0)) (error "Non-option argument: ~S" key))
    (let ((v (gethash key ht)))
      (when v (error "Option ~S given more than once: ~S and ~S" key v val)))
    (setf (gethash key ht) val)
    :finally (return ht)))

(defun get-opt (ht opt default pack)
  (let ((val (gethash opt ht)))
    (if val
        (or (parse-integer val :junk-allowed t)
            (and pack (find-symbol (string-upcase val) pack))
            (error "Invalid ~S: ~S" opt val))
        default)))

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

(defun sniff (args)
  (let ((socket (rawsock:socket
                 (get-opt args "-domain" :INET "KEYWORD")
                 (get-opt args "-type" :PACKET "KEYWORD")
                 (get-opt args "-protocol" #x300 "KEYWORD"))))
    (unwind-protect
         (loop
           :with buffer = (make-array *buffer-len* :fill-pointer 0
                                      :element-type '(unsigned-byte 8))
           :and device = (rawsock:make-sockaddr :UNSPEC)
           :repeat (get-opt args "-repeat" 10 nil) :do
           (my-rcvfrom socket buffer device)
           (print-sockaddr device)
           (print-buffer buffer))
      (rawsock:sock-close socket))))

(sniff (get-arg-ht))
