;; Benchmark for Common Lisp
;; Bruno Haible 1989

;; ACKER

(defun acker (x y)
  (if (zerop x)
    (1+ y)
    (if (zerop y)
      (acker (1- x) 1)
      (acker (1- x) (acker x (1- y))))))
; (acker 0 y) = (+ y 1)
; (acker 1 y) = (+ y 2)
; (acker 2 y) = (+ (* 2 y) 3)
; (acker 3 y) = (- (expt 2 (+ y 3)) 3)

;(time (acker 3 3))
; (acker 3 3)  ==>  61

;(time (acker 3 4))
; (acker 3 4)  ==>  125

