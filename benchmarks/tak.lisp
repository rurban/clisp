(defun tak (x y z)
  (declare (fixnum x y z))
  (cond ((not (< y x)) z)
        (t
          (tak
            (tak (the fixnum (1- x)) y z)
            (tak (the fixnum (1- y)) z x)
            (tak (the fixnum (1- z)) x y)))))
  
(defun tak-test ()
    (tak 18 12 6)
    (tak 18 12 6)
    (tak 18 12 6)
    (tak 18 12 6)
    (tak 18 12 6)
    (tak 18 12 6)
    (tak 18 12 6)
    (tak 18 12 6)
    (tak 18 12 6)
    (tak 18 12 6))

