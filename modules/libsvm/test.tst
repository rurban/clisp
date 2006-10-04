;; -*- Lisp -*-
;; some tests for libsvm
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "libsvm/test")'

(defparameter f-parameter (libsvm:make-parameter))
F-PARAMETER

(defparameter v-parameter (ffi:foreign-value f-parameter))
V-PARAMETER

(ffi:validp f-parameter) T
(libsvm:destroy-parameter f-parameter) NIL
(ffi:validp f-parameter) NIL

(equalp v-parameter
        (ffi:foreign-value (setq f-parameter (libsvm:make-parameter
                                              :v v-parameter))))
T

;; create an artificial problem:
;; predict the remainder of dividion by k from n-digits
(defun task (num divisor base)
  (flet ((normalize (x d) (- (/ (* 2 x) (1- d)) 1d0)))
    (values (normalize (rem num divisor) divisor)
            (do ((n num) r (ret ()) (index 0 (1+ index)))
                ((zerop n)
                 (coerce (nreverse (cons (list -1 0d0) ret)) 'vector))
              (multiple-value-setq (n r) (floor n base))
              (let ((value (normalize r base)))
                (unless (zerop value) (push (list index value) ret)))))))
TASK
(defun problem (repeat divisor base)
  (let ((x (make-array repeat)) (y (make-array repeat)))
    (dotimes (i repeat)
      (multiple-value-bind (n v) (task i divisor base)
        (setf (aref y i) n (aref x i) v)))
    (libsvm:make-problem :l repeat :x x :y y)))
PROBLEM

(defparameter f-problem-2-7 (problem 1000 2 7)) F-PROBLEM-2-7
(libsvm:save-problem "svm-problem" f-problem-2-7) NIL
(defparameter l-problem-2-7 (ffi:foreign-value f-problem-2-7)) L-PROBLEM-2-7
(multiple-value-bind (p maxindex) (libsvm:load-problem "svm-problem")
  (setf (ffi:slot (ffi:foreign-value f-parameter) 'libsvm::gamma)
        (float (/ maxindex) 0d0)
        (ffi:slot (ffi:foreign-value f-parameter) 'libsvm::C) 1d0
        (ffi:slot (ffi:foreign-value f-parameter) 'libsvm::kernel_type)
        libsvm::LINEAR
        v-parameter (ffi:foreign-value f-parameter))
  (list (= maxindex (floor (log (1- 1000) 7)))
        (equalp (ffi:foreign-value p) l-problem-2-7)))
(T T)

(let ((vec (libsvm:cross-validation f-problem-2-7 v-parameter 3)))
  (list (length vec) (count 1d0 vec) (count -1d0 vec)))
(1000 543 457)

(defparameter model (libsvm:train l-problem-2-7 v-parameter)) MODEL

(ffi:enum-from-value 'libsvm:svm_type (libsvm:get-svm-type model)) libsvm:C_SVC
(libsvm:get-nr-class model) 2
(libsvm:get-labels model) #(-1 1)
(libsvm:check-probability-model model) 0
(libsvm:get-svr-probability model) 0d0
(libsvm:save-model "svm-model" model) 0
(libsvm:destroy-model (libsvm:load-model "svm-model")) NIL
(libsvm:destroy-model model) NIL
(libsvm:destroy-problem f-problem-2-7) NIL

(defparameter f-problem-3-7 (problem 1000 3 7)) F-PROBLEM-3-7
(libsvm:save-problem "svm-problem" f-problem-3-7) NIL
(defparameter l-problem-3-7 (ffi:foreign-value f-problem-3-7)) L-PROBLEM-3-7
(progn
  (setf f-parameter (libsvm:make-parameter :v v-parameter 'LIBSVM::nu 5d-1
                                           'LIBSVM::svm_type libsvm:NU_SVR)
        v-parameter (ffi:foreign-value f-parameter))
  NIL) NIL

(let ((vec (libsvm:cross-validation f-problem-3-7 v-parameter 3)))
  (list (length vec) (count 1d0 vec) (count -1d0 vec))) NIL

(defparameter model (libsvm:train l-problem-3-7 v-parameter)) MODEL
(ffi:enum-from-value 'libsvm:svm_type (libsvm:get-svm-type model)) libsvm:NU_SVR
(libsvm:get-nr-class model) 3
(libsvm:get-labels model) #(1 -1)
(libsvm:check-probability-model model) 1
(libsvm:get-svr-probability model) 1d0

(destructuring-bind (l y x) l-problem-3-7
  (dotimes (i l)
    (multiple-value-bind (v e)
        (ignore-errors (libsvm:predict-values model (aref x i)))
      (print (list (aref y i) (if e (princ-to-string e) v))))))
NIL

(libsvm:destroy-model model) NIL

(ffi:validp f-problem-3-7) T
(libsvm:destroy-problem f-problem-3-7) NIL
(ffi:validp f-problem-3-7) NIL
(ffi:validp f-parameter) T
(libsvm:destroy-parameter f-parameter) NIL
(ffi:validp f-parameter) NIL

(progn (makunbound 'f-parameter)
       (makunbound 'v-parameter)
       (makunbound 'f-problem)
       (makunbound 'l-problem)
       (makunbound 'model)
       (makunbound 'maxindex)
       (delete-file "svm-model")
       (delete-file "svm-problem")
       T) T
