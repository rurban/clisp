;;; TPRINT -- Benchmark to print and read to the terminal.

(defvar tprint-test-atoms '(abc1 cde2 efg3 ghi4 ijk5 klm6 mno7 opq8 qrs9
                            stu0 uvw1 wxy2 \yz3 \123a \234b \345c \456d \567d
                            \678e \789f \890g))

(defun tprint-init (m n atoms)
  (declare (fixnum m n))
  (let ((atoms (subst () () atoms)))
    (do ((a atoms (cdr a)))
        ((null (cdr a)) (rplacd a atoms)))
    (tprint-init-aux m n atoms)))

(defun tprint-init-aux (m n atoms)
  (declare (fixnum m n))
  (cond ((= m 0) (pop atoms))
        (t (do ((i n (the fixnum (- (the fixnum i) 2)))
                (a ()))
               ((< (the fixnum i) 1) a)
             (declare (fixnum i))
             (push (pop atoms) a)
             (push (tprint-init-aux (the fixnum (1- m))
                                    n atoms) a)))))

(defvar tprint-test-pattern (tprint-init 6. 6. tprint-test-atoms))

;;; call:  (print test-pattern)

;;; This will be slower (ha,ha) but won't return that awful
;;; thing that breaks the screen editors.
(defun tprint-test (&optional (repeat 1))
  (let ((out "tprint-out.tmp"))
    (with-open-file (s out :direction :output)
      (loop :repeat repeat :do (print tprint-test-pattern s)))
    (delete-file out)
    t))

