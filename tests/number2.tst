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

(log 8s0 2s0)   3s0
(log 8f0 2f0)   3f0
(log 8d0 2d0)   3d0
(log 8l0 2l0)   3l0
(log -8 -2)     #C(1.0928407f0 -0.42078725f0)
(log -8s0 -2s0) #C(1.09283s0 -0.42078s0)
(log -8f0 -2f0) #C(1.0928407f0 -0.42078725f0)
(log -8d0 -2d0) #C(1.0928406470908163d0 -0.4207872484158604d0)

(cis 10)    #c(-0.8390715 -0.5440211)
(cis 123)   #c(-0.8879689 -0.45990348)
(zerop (+ (cis 123) (cis -123) (* -2 (cos 123))))  T

(sin 0d0)   0d0
(sinh 0d0)  0d0
(tan 0d0)   0d0
(tanh 0d0)  0d0

(tan 1.57f0) 1255.8483f0
(tan 1.57d0) 1255.7655915007895d0

(atan #c(1 2))  #C(1.3389726 0.4023595)
(tan  #c(1 2))  #C(0.033812825 1.0147936)
(tanh #c(20 2)) #C(1.0 0.0)

(tan 0)  0
(tanh 0) 0
(cosh 0) 1
(cos 0)  1
(sin 0)  0
(sinh 0) 0
