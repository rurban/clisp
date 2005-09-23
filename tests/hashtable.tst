;; -*- Lisp -*-
;; Test :warn-if-needs-rehash-after-gc.

(block nil
  (handler-bind ((WARNING #'(lambda (w) (declare (ignore w)) (return 'WARNING))))
    (let ((x1 (make-instance 'ext:standard-stablehash))
          (x2 (make-instance 'ext:standard-stablehash))
          (ht (make-hash-table :test 'ext:stablehash-eq)))
      (setf (gethash x1 ht) 11)
      (setf (gethash x2 ht) 22)
      (setf (gethash '1000 ht) 11999)
      (gc)
      (gethash x1 ht)
      (setf (gethash '10000000000000000000 ht) 11999999999999)
      (gc)
      (gethash x1 ht))))
11

(block nil
  (handler-bind ((WARNING #'(lambda (w) (declare (ignore w)) (return 'WARNING))))
    (let ((x1 (make-instance 'ext:standard-stablehash))
          (x2 (make-instance 'ext:standard-stablehash))
          (ht (make-hash-table :test 'ext:stablehash-eq
                               :warn-if-needs-rehash-after-gc t)))
      (setf (gethash x1 ht) 11)
      (setf (gethash x2 ht) 22)
      (setf (gethash '1000 ht) 11999)
      (gc)
      (gethash x1 ht)
      (setf (gethash '10000000000000000000 ht) 11999999999999))))
WARNING

;; Test *warn-on-hashtable-needing-rehash-after-gc*.

(block nil
  (handler-bind ((WARNING #'(lambda (w) (declare (ignore w)) (return 'WARNING))))
    (let ((custom:*warn-on-hashtable-needing-rehash-after-gc* t))
      (let ((x1 (make-instance 'ext:standard-stablehash))
            (x2 (make-instance 'ext:standard-stablehash))
            (ht (make-hash-table :test 'ext:stablehash-eq)))
        (setf (gethash x1 ht) 11)
        (setf (gethash x2 ht) 22)
        (setf (gethash '1000 ht) 11999)
        (gc)
        (gethash x1 ht)
        (setf (gethash '10000000000000000000 ht) 11999999999999)
        (gc)
        (gethash x1 ht)))))
WARNING
