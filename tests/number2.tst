;; -*- lisp -*-

(defun check-xgcd (a b)
  (multiple-value-bind (g u v) (xgcd a b)
    (if (= g (+ (* a u) (* b v))) g
        (format t "~& ~d~% ~d~%  ==> ~d~% ~d~% ~d~%" a b g u v))))
check-xgcd

(check-xgcd 2346026393680644703525505657 17293822570713318399)
11

(check-xgcd 77874422 32223899)
1

(check-xgcd 560014183 312839871)
1

(check-xgcd 3 2)
1

(check-xgcd 2 3)
1

(check-xgcd -2 3)
1

(check-xgcd 576561 -5)
1

(check-xgcd 974507656412513757857315037382926980395082974811562770185617915360
           -1539496810360685510909469177732386446833404488164283)
1

(isqrt #x3FFFFFFFC000000000007F)
#x7FFFFFFFBFF

;; transcendental functions

#+clisp (setq *break-on-warnings* t) #+clisp t

(expt -5s0 2s0) #c(25s0 0s0)
(expt -5f0 2f0) #c(25f0 0f0)
(expt -5d0 2d0) #c(25d0 0d0)
(expt -5l0 2l0) #c(25l0 0l0)
(expt -5 2)     25
(expt 5s0 3s0)  125s0
(expt 5f0 3f0)  125f0
(expt 5d0 3d0)  125d0
(expt 5l0 3l0)  125l0
(expt 5 3)      125
(= 1d-1 (setq z #C(1d-1 0d0)))  T
(* z (expt z z)) #C(0.07943282347242815d0 0.0)
z               #C(1d-1 0d0)

(log 8s0 2s0)   3s0
(log 8f0 2f0)   3f0
(log 8d0 2d0)   3d0
(log 8l0 2l0)   3l0
(log -8 -2)     #C(1.0928407f0 -0.42078725f0)
(log -8s0 -2s0) #C(1.09283s0 -0.42078s0)
(log -8f0 -2f0) #C(1.0928407f0 -0.42078725f0)
(log -8d0 -2d0) #C(1.0928406470908163d0 -0.4207872484158604d0)
(log z)         #C(-2.3025850929940455d0 0d0)
z               #C(1d-1 0d0)

(cis 10)    #c(-0.8390715 -0.5440211)
(cis 123)   #c(-0.8879689 -0.45990348)
(zerop (+               (cis 123) (cis -123)  (* -2 (cos 123))))  T
(zerop (+ (* #c(0 1) (- (cis 123) (cis -123))) (* 2 (sin 123))))  T

(exp #c(0 0))      1
(exp #c(0 1))      #C(0.5403023 0.84147096)
(exp #c(1 1))      #C(1.468694 2.2873552)
(exp #c(1 1d0))    #C(1.4686939399158851d0 2.2873552871788423d0)
(exp #c(1d0 1d0))  #C(1.4686939399158851d0 2.2873552871788423d0)
(exp #c(1l0 1))    #C(1.4686939399158851572L0 2.2873552871788423912L0)
(exp #c(0 1d0))    #C(0.5403023058681398d0 0.8414709848078965d0)
(exp 1)            2.7182817
(exp 1s0)          2.7183s0
(exp 1f0)          2.7182817
(exp 1d0)          2.718281828459045d0
(exp 1l0)          2.7182818284590452354L0

(sin 0d0)   0d0
(sinh 0d0)  0d0
(tan 0d0)   0d0
(tanh 0d0)  0d0

(tan 1.57f0) 1255.8483f0
(tan 1.57d0) 1255.7655915007895d0
(tan z)      #C(0.10033467208545055d0 0d0)
(= (tan z) (tan (realpart z)))   T
(tanh z)     #C(0.09966799462495582d0 0d0)
(= (tanh z) (tanh (realpart z))) T
(atan #c(1 2))  #C(1.3389726 0.4023595)
(tan  #c(1 2))  #C(0.033812825 1.0147936)
(tanh #c(20 2)) #C(1.0 0.0)

(tan 0)  0
(tanh 0) 0
(cosh 0) 1
(cos 0)  1
(sin 0)  0
(sinh 0) 0

(sqrt 1)    1
(sqrt 1d0)  1.0d0
(sqrt -1)   #C(0 1)
(sqrt -1d0) #C(0 1.0d0)

(abs (sqrt -1))    1
(phase (sqrt -2))  1.5707964
(signum (sqrt -2)) #C(0 1.0)

(asin 1)  1.5707964
(asin 2)  #C(1.5707964 -1.316958)
(acos 1)  0
(acos 2)  #C(0 1.316958)

(atan 1)  0.7853981
(atan 2)  1.1071488
(atan 2 3) 0.58800256

(sinh 10) 11013.232
(cosh 10) 11013.233

(tanh 10)  1.0
(tanh 3)   0.9950548
(asinh 1)  0.88137364
(acosh 1)  0
(acosh 3)  1.762747
(atanh 3)    #C(0.3465736 -1.5707964)
(atanh 0.9)  1.4722193

;; bits

(loop :for z :from 1 :to 1000
  :for z*z = (* z z) :for 2^z = (ash 1 z) :for z*2^z = (ash z z)
  :for z^z = (expt z z)
  :unless (logtest z z) :collect (list 'logtest z)
  :unless (logtest z*z z*z) :collect (list 'logtest z 'z*z z*z)
  :unless (logtest 2^z 2^z) :collect (list 'logtest z '2^z 2^z)
  :unless (logtest z*2^z z*2^z) :collect (list 'logtest z 'z*2^z z*2^z)
  :unless (logtest z*z z*z) :collect (list 'logtest z 'z*z z*z))
nil

(loop :for z :from 0 :to 1000
  :for z*z = (* z z) :for 2^z = (ash 1 z) :for z*2^z = (ash z z)
  :for z^z = (expt z z)
  :unless (= z (logand z z)) :collect (list 'logand z)
  :unless (= z*z (logand z*z z*z)) :collect (list 'logand z 'z*z z*z)
  :unless (= 2^z (logand 2^z 2^z)) :collect (list 'logand z '2^z 2^z)
  :unless (= z*2^z (logand z*2^z z*2^z)) :collect (list 'logand z 'z*2^z z*2^z)
  :unless (= z*z (logand z*z z*z)) :collect (list 'logand z 'z*z z*z))
nil

(mod-expt 1432 634 3456)
2944

(mod-expt 640785284696442065785559134003308932264708355179002798538113
          671286301850793527622248679786362012411973295201562077406347
          541607700526106309999871171548445806906603126622271198261079)
184927654951560197998922671105024055618160643054333015564836

(defun check-sqrt (num eps)
  (let ((s (sqrt num)))
    (> (* eps 2) (abs (/ (- s (/ num s)) s)))))
check-sqrt

(check-sqrt   MOST-POSITIVE-SHORT-FLOAT SHORT-FLOAT-EPSILON)  T
(check-sqrt  LEAST-POSITIVE-SHORT-FLOAT SHORT-FLOAT-EPSILON)  T
(check-sqrt  LEAST-NEGATIVE-SHORT-FLOAT SHORT-FLOAT-EPSILON)  T
(check-sqrt   MOST-NEGATIVE-SHORT-FLOAT SHORT-FLOAT-EPSILON)  T
(check-sqrt  MOST-POSITIVE-SINGLE-FLOAT SINGLE-FLOAT-EPSILON) T
(check-sqrt LEAST-POSITIVE-SINGLE-FLOAT SINGLE-FLOAT-EPSILON) T
(check-sqrt LEAST-NEGATIVE-SINGLE-FLOAT SINGLE-FLOAT-EPSILON) T
(check-sqrt  MOST-NEGATIVE-SINGLE-FLOAT SINGLE-FLOAT-EPSILON) T
(check-sqrt  MOST-POSITIVE-DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON) T
(check-sqrt LEAST-POSITIVE-DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON) T
(check-sqrt LEAST-NEGATIVE-DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON) T
(check-sqrt  MOST-NEGATIVE-DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON) T
(check-sqrt    MOST-POSITIVE-LONG-FLOAT LONG-FLOAT-EPSILON)   T
(check-sqrt   LEAST-POSITIVE-LONG-FLOAT LONG-FLOAT-EPSILON)   T
(check-sqrt   LEAST-NEGATIVE-LONG-FLOAT LONG-FLOAT-EPSILON)   T
(check-sqrt    MOST-NEGATIVE-LONG-FLOAT LONG-FLOAT-EPSILON)   T
