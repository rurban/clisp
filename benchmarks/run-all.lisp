;;; Driver that runs benchmarks.
;;; This driver uses the following files:
;;;    acker
;;;    bfib
;;;    boyer
;;;    browse
;;;    ctak
;;;    dderiv
;;;    deriv
;;;    destru
;;;    div2
;;;    fac
;;;    fft
;;;    fprint
;;;    fread
;;;    frpoly
;;;    puzzle
;;;    stak
;;;    stream
;;;    tak
;;;    takl
;;;    takr
;;;    tprint
;;;    traverse
;;;    triang

(in-package user)

(defparameter *source-type* "lisp")
(defparameter *compiled-type* (pathname-type (compile-file-pathname "foo")))

(defparameter *file-list*
  '("acker" "bfib" "boyer" "browse" "ctak" "dderiv" "deriv" "destru" "div2"
    "fac" "fft" "fprint" "fread" "frpoly" "puzzle" "stak" "stream"
    "tak" "takl" "takr" "tprint" "traverse" "triang"))

(defparameter *cur-stat* nil)

;; put here your CPU speed in GHz
(defparameter *scale*
  #+CLISP (/ (POSIX:BOGOMIPS) 1000)
  #-CLISP 1d0)

(defvar *benchmark-type* nil)

;; Garbage collection before running a test:
#+KCL (defun gc () (gbc t))
#+CMU (eval-when (compile load eval) (setq ext:*gc-verbose* nil))

;; In POPLOG you must first do:   lib storeutils

; Filename conversion
(defun file (filename)
  (declare (string filename))
  (case *benchmark-type*
    (compiled (concatenate 'string filename "." *compiled-type*))
    (interpreted (concatenate 'string filename "." *source-type*))
    (t filename)))

(defmacro with-file (filename &body body)
  (let ((fi (gensym "WITH-FILE-")))
    `(let ((,fi (file ,filename)))
       (load ,fi :verbose nil)
       (push (list ,fi 0 0) *cur-stat*)
       (format t "~& * file: ~s~%" ,filename)
       ,@body)))

;; Function for benchmarking...
(defun timer-thing (count retval name function &rest args)
  (gc)
  (format t "~& > form: (~a~{ ~s~})~%" name args)
  (let ((start (get-internal-run-time)) elapsed elapsed-s ret)
    (loop :repeat count :do (setq ret (apply function args)))
    (setq elapsed (/ (- (get-internal-run-time) start)
                     (float internal-time-units-per-second 1d0))
          elapsed-s (/ elapsed *scale*))
    (when (car *cur-stat*)
      (incf (second (car *cur-stat*)) elapsed)
      (incf (third (car *cur-stat*)) elapsed-s))
    (if (equalp retval ret)
        (format t "correct: ~s~%" ret)
        (if (eq retval 'none)
            (format t "unchecked: ~s~%" ret)
            (error "(~s~{ ~s~}) failed: ~s  !=  ~s" name args ret retval)))
    (format t "elapsed: ~f sec [scaled: ~f]~%"
            elapsed elapsed-s)))

(defun benchmarks (log)
  (setq *cur-stat* nil)
  ;; repeat counts are selected so that each test takes
  ;; approximately the same time
  (with-file "fac"
    (timer-thing 100 'none "fac" #'fac 1000))
  (with-file "acker"
    (timer-thing 17 253 "acker" #'acker 3 5)
    (timer-thing 9 509 "acker" #'acker 3 6)
    (timer-thing 3 1021 "acker" #'acker 3 7))
  (with-file "bfib"
    (timer-thing 20 6765 "bfib-test" #'bfib-test))
  (with-file "ctak"
    (timer-thing 60 7 "ctak" #'ctak 18 12 6))
  (with-file "stak"
    (timer-thing 60 7 "stak" #'stak 18 12 6))
  (with-file "tak"
    (timer-thing 90 7 "tak" #'tak 18 12 6))
  (with-file "takl"
    (timer-thing 15 '(7 6 5 4 3 2 1) "mas-bench" #'mas-bench))
  (with-file "takr"
    (timer-thing 80 7 "tak0" #'tak0 18 12 6))
  (with-file "boyer"
    (boyer-setup)
    (timer-thing 5 nil "boyer-test" #'boyer-test))
  (with-file "browse"
    (timer-thing 5 nil "browse" #'browse))
  (with-file "dderiv"
    (timer-thing 25 nil "dderiv-run" #'dderiv-run))
  (with-file "deriv"
    (timer-thing 6 nil "deriv-run" #'deriv-run))
  (with-file "destru"
    (timer-thing 30 nil "destructive" #'destructive 600 50))
  (with-file "div2"
    (timer-thing 20 nil "test-div2-iterative" #'test-div2-iterative)
    (timer-thing 20 nil "test-div2-recursive" #'test-div2-recursive))
  (with-file "fft"
    (timer-thing 6 nil "fft-bench" #'fft-bench))
  (with-file "fprint"
    (timer-thing 80 t "fprint" #'fprint))
  (with-file "fread"
    (timer-thing 250 t "fread" #'fread))
  (with-file "frpoly" ; 4 sets of three tests
    (timer-thing 8 nil "pexptsq" #'pexptsq frpoly-r 2)
    (timer-thing 8 nil "pexptsq" #'pexptsq frpoly-r2 2)
    (timer-thing 8 nil "pexptsq" #'pexptsq frpoly-r3 2)
    (timer-thing 8 nil "pexptsq" #'pexptsq frpoly-r 5)
    (timer-thing 8 nil "pexptsq" #'pexptsq frpoly-r2 5)
    (timer-thing 8 nil "pexptsq" #'pexptsq frpoly-r3 5)
    (timer-thing 8 nil "pexptsq" #'pexptsq frpoly-r 10)
    (timer-thing 6 nil "pexptsq" #'pexptsq frpoly-r2 10)
    (timer-thing 6 nil "pexptsq" #'pexptsq frpoly-r3 10)
    (timer-thing 6 nil "pexptsq" #'pexptsq frpoly-r 15)
    (timer-thing 2 nil "pexptsq" #'pexptsq frpoly-r2 15)
    (timer-thing 2 nil "pexptsq" #'pexptsq frpoly-r3 15))
  (with-file "puzzle"
    (timer-thing 4 nil "puzzle-start" #'puzzle-start))
  (with-file "stream"
    (timer-thing 2 229 "nth-prime" #'nth-prime 50)
    (timer-thing 2 541 "nth-prime" #'nth-prime 100)
    (timer-thing 1 863 "nth-prime" #'nth-prime 150))
  (with-file "tprint"
    ;; increasing repeat here will fllod the screen
    (timer-thing 1 t "tprint-test" #'tprint-test))
  (with-file "traverse"
    (timer-thing 1 nil "init-traverse" #'init-traverse)
    (timer-thing 1 nil "run-traverse" #'run-traverse))
  (with-file "triang"
    (timer-thing 1 nil "gogogo" #'gogogo 22))
  (setq *cur-stat* (nreverse *cur-stat*))
  (dribble log)
  (multiple-value-bind (se mi ho da mo ye) (get-decoded-time)
    (format t " * ~s: ~4,'0d:~2,'0d:~2,'0d ~2,'0d:~2,'0d:~2,'0d~% * ~a [~a]~%"
            *benchmark-type* ye mo da ho mi se
            (lisp-implementation-type) (lisp-implementation-version)))
  (format t "~:{~15a ~10,5f sec  ~10,5f scaled~%~}~@
                ~15a ~10,5f sec  ~10,5f scaled~2%"
          *cur-stat* "total"
          (reduce #'+ *cur-stat* :key #'second)
          (reduce #'+ *cur-stat* :key #'third))
  (dribble))

(defun comp-benchmarks (&optional (log "benchmark.log"))
  (timer-thing 1 'none "compile all files" #'mapc #'compile-file *file-list*)
  (let ((*benchmark-type* 'compiled))
    (time (benchmarks log))))

(defun eval-benchmarks (&optional (log "benchmark.log"))
  (let ((*benchmark-type* 'interpreted))
    (time (benchmarks log))))

(format t "~& Running the benchmarks:~@
For running the compiled benchmarks, use (comp-benchmarks)~@
For running the interpreted benchmarks, use (eval-benchmarks)~%")
