;; Benchmark for Common Lisp: factorial
;; Bruno Haible 1989

(defun fac (n)
  (do* ((i 0 (1+ i))
        (f 1 (* f i)))
       ((= i n) f)))
