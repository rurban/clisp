;; -*- Lisp -*-

(use-package "FFI")  t
(def-c-struct triv (i int))  triv

(def-call-out trigger (:arguments (struct_array (c-array-ptr (c-ptr triv))))
  (:name "ffi_identity") (:language :stdc)
  (:return-type (c-array-ptr (c-ptr triv))))
trigger

(trigger (vector (make-triv :i 0) (make-triv :i 1) (make-triv :i 3)
                 (make-triv :i 4) (make-triv :i 5) (make-triv :i 6)))
#(#S(TRIV :I 0) #S(TRIV :I 1) #S(TRIV :I 3)
  #S(TRIV :I 4) #S(TRIV :I 5) #S(TRIV :I 6))

(with-foreign-object (x '(c-array-ptr int) (vector -4 6 7))
  (ffi::foreign-value x))
#(-4 6 7)
