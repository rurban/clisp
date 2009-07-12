;; -*- Lisp -*- vim:filetype=lisp

(with-timeout (10 t) nil) NIL
(with-timeout (1 t) (sleep 2)) T
(y-or-n-p-timeout 1 t "y or n") T
(yes-or-no-p-timeout 0.5 nil "yes or no") NIL
(times
 (list (describe (current-thread))
       (describe (make-mutex :name "my lock"))
       (describe (make-exemption :name "my exemption"))))
(NIL NIL NIL)

;; non-recursive mutex
(defparameter *mu1* (make-mutex :name "mu1")) *MU1*
(mutex-owner *mu1*) NIL
(typep *mu1* 'exemption) NIL
(typep *mu1* 'mutex) T
(typep *mu1* 'thread) NIL
(typep *mu1* 'T) T
(mutex-lock *mu1*) T
(mutex-lock *mu1*) ERROR
(eq (mutex-owner *mu1*) (current-thread)) T
(progn (mutex-unlock *mu1*) (mutex-owner *mu1*)) NIL
(mutex-unlock *mu1*) ERROR
;; recursive mutex
(defparameter *mu2* (make-mutex :name "mu2" :recursive-p t)) *MU2*
(mutex-owner *mu2*) NIL
(mutex-lock *mu2*) T
(mutex-lock *mu2*) T
(eq (mutex-owner *mu2*) (current-thread)) T
(progn (mutex-unlock *mu2*) (mutex-unlock *mu2*) (mutex-owner *mu2*)) NIL
(mutex-unlock *mu2*) ERROR

(defparameter *thread-special* 1) *thread-special*

;; thread-interrupt & mutexes
(defparameter *th1*
  (make-thread #'(lambda ()
                   (incf *thread-special*)
                   (let ((*thread-special* 5))
                     (mutex-lock *mu1*)
                     (mutex-lock *mu2*) (mutex-lock *mu2*)
                     (loop (sleep 1))))))
*TH1*
(thread-active-p *th1*) T
(typep *th1* 'exemption) NIL
(typep *th1* 'mutex) NIL
(typep *th1* 'thread) T
(typep *th1* 'T) T

;; wait for the global symbol value to change
(loop until (eql *thread-special* 2) do (sleep 0.1)) NIL
;; just sleep little bit
(sleep 0.5) NIL
(symbol-value-thread '*thread-special* *th1*) 5
(setf (symbol-value-thread '*thread-special* *th1*) 10) 10
(symbol-value-thread '*thread-special* *th1*) 10
*thread-special* 2
;; get global symbol value
(symbol-value-thread '*thread-special* nil) 2
(eq (mutex-owner *mu1*) *th1*) T
;; check timed wait on mutex
(mutex-lock *mu1* :timeout 0.5) NIL
(mutex-lock *mu1* :timeout 0) NIL
;; check thread-interrupt
(thread-active-p (thread-interrupt *th1* :function #'mutex-unlock
                                   :arguments (list *mu1*)))
T
;; thread is interrupted - wait for the mutex *mu1* to be unlocked and grab it
(mutex-lock *mu1*) T
(progn (mutex-unlock *mu1*) (mutex-owner *mu1*)) NIL
(eq (mutex-owner *mu2*) *th1*) T

(defparameter *th2*
  (make-thread #'(lambda ()
                   (mutex-lock *mu2*)
                   (loop (sleep 1)))))
*TH2*
(thread-active-p *th2*)
T

(progn
  (thread-interrupt *th1* :function #'mutex-unlock :arguments (list *mu2*))
  (thread-interrupt *th1* :function #'mutex-unlock :arguments (list *mu2*))
  (sleep 1)
  (eq (mutex-owner *mu2*) *th2*))
T

;; kill *th2* - warning for locked mutex *mu2* will be issued and
;; the mutex will be released
(progn (thread-interrupt *th2* :function t) (sleep 1)) NIL
(mutex-owner *mu2*) NIL
;; multiple times kill on already dead thread
(eq (thread-interrupt
     (thread-interrupt
      (thread-interrupt *th1* :function t)
      :function t)
     :function t) *th1*)
T
(progn (sleep 1) (thread-active-p *th1*)) NIL

(defparameter *exemption*
  (make-exemption :name "test exemption"))
*EXEMPTION*
(defparameter *exemption-state* nil)
*EXEMPTION-STATE*

;; test deferred interrupts, exemption-signal and with-mutex-lock
(defparameter *th1*
  (make-thread (lambda ()
                 (with-deferred-interrupts
                   (with-mutex-lock (*mu1*)
                     (sleep 1)
                     (setf *exemption-state* :signaled)
                     (exemption-signal *exemption*))
                   (loop (sleep 1))))))
*TH1*

(with-mutex-lock (*mu1*)
  (loop until (eq *exemption-state* :signaled)
     do (exemption-wait *exemption* *mu1*)
     finally (return *exemption-state*)))
:SIGNALED

(thread-active-p *th1*) T
(eq (thread-interrupt *th1* :function t) *th1*) T
(sleep 0.5) NIL
(thread-active-p *th1*) T ;; kill is deferred

;; test exemtpion-broadcast and thread-interrupt :override
(with-mutex-lock (*mu1*)
  (thread-interrupt *th1* :function (lambda ()
                                      (with-mutex-lock (*mu1*)
                                        (setf *exemption-state* :broadcasted)
                                        (exemption-broadcast *exemption*)))
                    :override t)
  (loop until (eq *exemption-state* :broadcasted)
     do (exemption-wait *exemption* *mu1*)
     finally (return *exemption-state*)))
:BROADCASTED

(thread-active-p *th1*) T ;; thread should be still running
(eq (thread-interrupt *th1* :function t :override t) *th1*) T
(sleep 0.5) NIL
(thread-active-p *th1*) NIL ;; should be dead

;; create thread with very tiny lisp stack (thus preserving memory)
;; on mac osx lisp heap overlaps lisp stack regions (malloc-ed) when
;; the number of thread with big stacks is very high (will be inspected
;; in details)
;; on all tested platforms (i386 linux, i386 osx, win32 mingw) having
;; more than 500 active threads causes lisp heap to overlap with malloc-ed
;; area (on osx it is less it seems)
;; probably some limit should be set for this

(defun make-test-thread (function)
  #+macos
  (make-thread function :vstack-size 10000)
  #-macos
  (make-thread function))
make-test-thread

(let* ((mu (make-mutex :name "hash-table lock"))
       (ht (make-hash-table))
       (tl (loop :repeat 300 :collect
              (make-test-thread (lambda ()
                                  (mutex-lock mu)
                                  (incf (gethash 1 ht 0))
                                  (mutex-unlock mu))))))
  ;; wait for all threads to finish
  (loop :while (some #'thread-active-p tl) :do (sleep 0.1))
  (gethash 1 ht))
300

(let* ((count 300)
       (pa (make-package (symbol-name (gensym "MT-TEST-")) :use ()))
       (tl (loop :for i :from 1 :to count :collect
             (let ((i i))
               (make-test-thread (lambda ()
                                   (intern (prin1-to-string i) pa)))))))
  ;; wait for all threads to finish
  (loop :while (some #'thread-active-p tl) :do (sleep 0.1))
  (let ((i 0))
    (do-symbols (s pa) (declare (ignore s)) (incf i))
    (assert (= i count)))
  (setq tl (loop :for i :from 1 :to count :collect
             (let ((i i))
               (make-test-thread
                (lambda ()
                  (unintern (find-symbol (prin1-to-string i) pa) pa))))))
  ;; wait for all threads to finish
  (loop :while (some #'thread-active-p tl) :do (sleep 0.1))
  (let ((i 0))
    (do-symbols (s pa) (declare (ignore s)) (incf i))
    (assert (zerop i)))
  (delete-package pa))
T


(progn (symbol-cleanup '*thread-special*)
       (symbol-cleanup '*mu1*)
       (symbol-cleanup '*mu2*)
       (symbol-cleanup '*th1*)
       (symbol-cleanup '*th2*))
T
