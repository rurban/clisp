;; -*- Lisp -*-
;; some tests for ZLIB
;; clisp -K full -E utf-8 -q -norc -i ../tests/tests -x '(run-test "zlib/test")'

(format t "~&zlib version: ~S~%" (zlib:z-version))
NIL

(let ((v (make-array 1024 :element-type '(unsigned-byte 8))) c)
  (dotimes (i 1024) (setf (aref v i) 0))
  (setq c (zlib:compress v))
  (print (zlib:compress-bound 1024))
  (print (length c))
  (equalp v (zlib:uncompress c 1024)))
T

(let ((v (make-array 1024 :element-type '(unsigned-byte 8))) c
      (cb (zlib:compress-bound 1024)))
  (dotimes (i 1024) (setf (aref v i) (random 256)))
  (setq c (zlib:compress v))
  (print cb)
  (print (length c))
  (unless (= cb (length c)) (warn "zlib compresses random vectors!"))
  (equalp v (zlib:uncompress c 1024)))
T
