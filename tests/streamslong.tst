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

;; <http://www.lisp.org/HyperSpec/Body/fun_file-position.html>
(let ((noticed '()) file-written)
  (flet ((notice (x) (push x noticed) x))
    (unwind-protect (progn
         (with-open-file (s "test.bin"
                            :element-type '(unsigned-byte 8)
                            :direction :output
                            :if-exists :error)
           (notice (file-position s)) ;1
           (write-byte 5 s)
           (write-byte 6 s)
           (let ((p (file-position s)))
             (notice p) ;2
             (notice (when p (file-position s (1- p))))) ;3
           (write-byte 7 s)
           (notice (file-position s)) ;4
           (setq file-written (truename s)))
         (with-open-file (s file-written
                            :element-type '(unsigned-byte 8)
                            :direction :input)
           (notice (file-position s)) ;5
           (let ((length (file-length s)))
             (notice length) ;6
             (when length
               (dotimes (i length)
                 (notice (read-byte s)))))) ;7,...
         (nreverse noticed))
      (delete-file file-written))))
(0 2 1 2 0 2 5 7)

(let ((s (make-string-input-stream
          (make-array 10 :element-type 'character
                      :displaced-to "abcdefghijklmnopqrst"
                      :displaced-index-offset 5))))
  (prog1
      (list (read-char s) (read-char s) (file-position s)
            (file-position s 4) (read-char s)
            (file-position s :start) (read-char s)
            (file-position s :end) (file-position s))
    (close s)))
(#\f #\g 2 4 #\j 0 #\f 10 10)

(let ((s (make-string-output-stream)))
  (prog1
      (list (write-char #\a s) (write-char #\b s) (file-position s)
            (get-output-stream-string s)
            (write-string "foo" s) (file-position s 1) (write-char #\z s)
            (get-output-stream-string s)
            (file-position s :start) (write-char #\u s)
            (file-position s :end) (write-char #\w s)
            (get-output-stream-string s))
    (close s)))
(#\a #\b 2 "ab" "foo" 1 #\z "fz" 0 #\u 1 #\w "uw")

(let ((v (make-array 3 :adjustable t :fill-pointer 0
                     :element-type 'character)))
  (with-output-to-string (s v)
    (list (write-string "foo" s) (cons (file-position s) (copy-seq v))
          (file-position s 2) (write-string "bar" s)
          (cons (file-position s) (copy-seq v))
          (file-position s :start) (write-string "zot" s)
          (cons (file-position s) (copy-seq v))
          (file-position s :end) (write-string "plonk" s)
          (cons (file-position s) (copy-seq v)) v)))
("foo" (3 . "foo")
 2 "bar" (5 . "fobar")
 0 "zot" (3 . "zot")
 3 "plonk" (8 . "zotplonk")
 "zotplonk")
