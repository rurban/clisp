;; -*- Lisp -*-

(use-package "FFI")  t

;; <https://sourceforge.net/tracker/index.php?func=detail&aid=679661&group_id=1355&atid=101355>
(def-c-struct triv (i int))
TRIV

(def-call-out trigger (:arguments (struct_array (c-array-ptr (c-ptr triv))))
  (:name "ffi_identity") (:language :stdc)
  (:return-type (c-array-ptr (c-ptr triv))))
TRIGGER

(trigger (vector (make-triv :i 0) (make-triv :i 1) (make-triv :i 3)
                 (make-triv :i 4) (make-triv :i 5) (make-triv :i 6)))
#(#S(TRIV :I 0) #S(TRIV :I 1) #S(TRIV :I 3)
  #S(TRIV :I 4) #S(TRIV :I 5) #S(TRIV :I 6))

(with-foreign-object (x '(c-array-ptr int) (vector -4 6 7))
  (ffi::foreign-value x))
#(-4 6 7)

(let ((v (ffi:allocate-deep 'triv (make-triv :i 42))))
  (prog1
      (list (typeof (ffi::foreign-value v))
            (slot (ffi::foreign-value v) 'i))
    (foreign-free v)))
((C-STRUCT NIL (I INT)) 42)
