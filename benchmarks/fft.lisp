;;; FFT -- This is an FFT benchmark written by Harry Barrow.
;;; It tests a variety of floating point operations,
;;; including array references.


(defvar **fft-re**
    (make-array 1025. :element-type 'float :initial-element 0.0))
(defvar **fft-im**
    (make-array 1025. :element-type 'float :initial-element 0.0))
(proclaim '(type (simple-array float (*)) **fft-re** **fft-im**))

;; fast fourier transform
(defun fft (areal #|real part|# aimag #|imaginary part|#)
  (declare (type (simple-array float (*)) areal aimag))
  (prog*
    ((ar areal) (ai aimag) (i 1) (j 0) (k 0) (m 0) ;compute m = log(n)
     (n (1- (array-dimension areal 0))) (le 0) (le1 0) (ip 0)
     (nv2 (the (values fixnum fixnum) (floor n 2)))
     (ur 0.0) (ui 0.0) (wr 0.0) (wi 0.0) (tr 0.0) (ti 0.0))
    (declare (type fixnum i j k n nv2 m le le1 ip))
    (declare (type (simple-array float (*)) ar ai))
    (declare (type float ur ui wr wi tr ti))
 l1 (cond ((< i n)
           (setq m (the fixnum (1+ m))
                 i (the fixnum (+ i i)))
           (go l1)))
    (cond ((not (equal n (the fixnum (expt 2 m))))
           (princ "error ... array size not a power of two.")
           (read)
           (return (terpri))))
    (setq j 1                                   ;interchange elements
          i 1)                                  ;in bit-reversed order
 l3 (cond ((< i j)
           (setq tr (aref ar j)
                 ti (aref ai j))
           (setf (aref ar j) (aref ar i))
           (setf (aref ai j) (aref ai i))
           (setf (aref ar i) tr)
           (setf (aref ai i) ti)))
    (setq k nv2)
 l6 (cond ((< k j)
           (setq j (the fixnum (- j k))
                 k (the fixnum (/ k 2)))
           (go l6)))
    (setq j (the fixnum (+ j k))
          i (the fixnum (1+ i)))
    (cond ((< i n)
           (go l3)))
    (do ( (l 1 (the fixnum (1+ (the fixnum l)))))
        ((> (the fixnum l) m))                  ;loop thru stages
      (declare (type fixnum l))
        (setq le (the fixnum (expt 2 l))
              le1 (the (values fixnum fixnum) (floor le 2))
              ur 1.0
              ui 0.0
              wr (cos (/ #.(float pi (float 1)) (float le1)))
              wi (sin (/ #.(float pi (float 1)) (float le1))))
        (do ((j 1 (the fixnum (1+ (the fixnum j)))))
            ((> (the fixnum j) le1))            ;loop thru butterflies
          (declare (type fixnum j))
            (do ( (i j (+ (the fixnum i) le)))
                ((> (the fixnum i) n))          ;do a butterfly
              (declare (type fixnum i))
                (setq ip (the fixnum (+ i le1))
                      tr (- (* (aref ar ip) ur)
                            (* (aref ai ip) ui))
                      ti (+ (* (aref ar ip) ui)
                            (* (aref ai ip) ur)))
                (setf (aref ar ip) (- (aref ar i) tr))
                (setf (aref ai ip) (- (aref ai i) ti))
                (setf (aref ar i) (+ (aref ar i) tr))
                (setf (aref ai i) (+ (aref ai i) ti))))
        (setq tr (- (* ur wr) (* ui wi))
              ti (+ (* ur wi) (* ui wr))
              ur tr
              ui ti))
        (return t)))

;;; the timer which does 10 calls on fft

(defun fft-bench ()
  (dotimes (i 10)
    (fft **fft-re** **fft-im**)))

;;; call:  (fft-bench)

