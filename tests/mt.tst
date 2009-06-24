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
(mutex-owner (setf m1 (make-mutex :name "m1"))) NIL
(typep m1 'exemption) NIL
(typep m1 'mutex) T
(typep m1 'thread) NIL
(typep m1 'T) T
(mutex-lock m1) T
(mutex-lock m1) ERROR
(eq (mutex-owner m1) (current-thread)) T
(progn (mutex-unlock m1) (mutex-owner m1)) NIL
(mutex-unlock m1) ERROR
;; recursive mutex
(mutex-owner (setf m2 (make-mutex :name "m2" :recursive-p t))) NIL
(mutex-lock m2) T
(mutex-lock m2) T
(eq (mutex-owner m2) (current-thread)) T
(progn (mutex-unlock m2) (mutex-unlock m2) (mutex-owner m2)) NIL
(mutex-unlock m2) ERROR

(defvar *thread-special* 1) *thread-special*

;; thread-interrupt & mutexes
(thread-active-p
 (setf th (make-thread
           #'(lambda ()
               (incf *thread-special*)
               (let ((*thread-special* 5))
                 (mutex-lock m1)
                 (mutex-lock m2) (mutex-lock m2)
                 (loop (sleep 1)))))))
T

(typep th 'exemption) NIL
(typep th 'mutex) NIL
(typep th 'thread) T
(typep th 'T) T

;; wait for the global symbol value to change
(loop until (eql *thread-special* 2) do (sleep 0.1)) NIL
;; just sleep little bit
(sleep 0.5) NIL
(symbol-value-thread '*thread-special* th) 5
(setf (symbol-value-thread '*thread-special* th) 10) 10
(symbol-value-thread '*thread-special* th) 10
*thread-special* 2
;; get global symbol value
(symbol-value-thread '*thread-special* nil) 2
(eq (mutex-owner m1) th) T
;; check timed wait on mutex
(mutex-lock m1 :timeout 0.5) NIL
(mutex-lock m1 :timeout 0) NIL
;; check thread-interrupt
(thread-active-p (thread-interrupt th :function #'mutex-unlock
                                   :arguments (list m1)))
T
;; thread is interrupted - wait for the mutex m1 to be unlocked and grab it
(mutex-lock m1) T
(progn (mutex-unlock m1) (mutex-owner m1)) NIL
(eq (mutex-owner m2) th) T

(thread-active-p
 (setf th2 (make-thread
            #'(lambda ()
                (mutex-lock m2)
                (loop (sleep 1))))))
T

(progn
  (thread-interrupt th :function #'mutex-unlock :arguments (list m2))
  (thread-interrupt th :function #'mutex-unlock :arguments (list m2))
  (sleep 1)
  (eq (mutex-owner m2) th2))
T

;; kill th2 - warning for locked mutex m2 will be issued and
;; the mutex will be released
(progn (thread-kill th2) (sleep 1)) NIL
(mutex-owner m2) NIL
;; multiple times kill on already dead thread
(eq (thread-kill (thread-kill (thread-kill th))) th) T
(progn (sleep 1) (thread-active-p th)) NIL

;; test deferred interrupts
(thread-active-p
 (setf th (make-thread
           (lambda () (with-deferred-interrupts (loop (sleep 1)))))))
T

(eq (thread-interrupt th :function t) th) T
(sleep 0.5) NIL
(thread-active-p th) T ;; kill is deferred
(eq (thread-interrupt th :function t :override t) th) T
(sleep 0.5) NIL
(thread-active-p th) NIL ;; should be dead

(symbol-cleanup '*thread-special*) T
(symbol-cleanup 'm1) T
(symbol-cleanup 'm2) T
(symbol-cleanup 'th) T
(symbol-cleanup 'th2) T
