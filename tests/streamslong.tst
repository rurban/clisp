;; -*- lisp -*-

(READ-FROM-STRING "123") 123

(PRIN1-TO-STRING 123) "123"

(SETQ *A* (MAKE-ARRAY 10. :ELEMENT-TYPE #-CMU 'STRING-CHAR #+CMU 'CHARACTER :FILL-POINTER 0)) ""

(FORMAT *A* "XXX") NIL

*A* "XXX"

#+XCL (SYS::CHECK-STREAM-SYSTEM) #+XCL T

(defun bin-stream-test (&key (size (integer-length most-positive-fixnum))
                        (type 'unsigned-byte) (file-name "./foocl")
                        (num-bytes 10)
                        (bytes (if (eq type 'signed-byte)
                                   (loop :repeat num-bytes :collect
                                         (- (random (ash 1 size))
                                            (ash 1 (1- size))))
                                   (loop :repeat num-bytes :collect
                                         (random (ash 1 size))))))
  (with-open-file (foo file-name :direction :output
                       :element-type (list type size))
    (dolist (byte bytes)
      (write-byte byte foo)))
  (unwind-protect
       (with-open-file (foo file-name :direction :input
                            :element-type (list type size))
         (list (stream-element-type foo) (file-length foo) bytes
               (loop :for byte :in bytes :for nb = (read-byte foo) :collect nb
                     :unless (= nb byte) :do
                     (flet ((by-out (sz by)
                              (format nil "~v,'0,' ,4:b"
                                      (+ sz (floor sz 4)) by)))
                       (error "~& * [(~s ~s)] ~a != ~a~%" type size
                              (by-out size byte) (by-out size nb))))))
    (delete-file file-name)))
bin-stream-test

(loop for size from 2 to 40 do (bin-stream-test :size size))
nil

(loop for size from 2 to 40 do (bin-stream-test :size size :type 'signed-byte))
nil
