;; -*- Lisp -*- vim:filetype=lisp

(with-timeout (10 t) nil) NIL
(with-timeout (1 t) (sleep 100)) T
(y-or-n-p-timeout 1 t "y or n") T
(yes-or-no-p-timeout 0.5 nil "yes or no") NIL
(times
 (list (describe (current-thread))
       (describe (make-mutex :name "my lock"))
       (describe (make-exemption :name "my exemption"))))
(NIL NIL NIL)

;; non-recursive mutex
(mutex-owner (setf m1 (make-mutex :name "m1"))) NIL
(eq (mutex-lock m1) m1) T
(mutex-lock m1) ERROR
(eq (mutex-owner m1) (current-thread)) T
(eq (mutex-unlock m1) m1) T
(mutex-unlock m1) ERROR
(mutex-owner m1) NIL
;; recursive mutex
(mutex-owner (setf m2 (make-mutex :name "m2" :recursive-p t))) NIL
(eq (mutex-lock m2) m2) T
(eq (mutex-lock m2) m2) T
(eq (mutex-owner m2) (current-thread)) T
(eq (mutex-unlock (mutex-unlock m2)) m2) T
(mutex-unlock m2) ERROR
(mutex-owner m2) NIL
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
(eq (mutex-owner m1) th) t
;; check thread-interrupt
(thread-active-p (thread-interrupt th #'mutex-unlock m1)) T
;; thread is interrupted - wait for the mutex m1 to be unlocked and grab it
(eq (mutex-owner (mutex-lock m1)) (current-thread)) T
(eq (mutex-unlock m1) m1) T
(eq (mutex-owner m2) th) T

(thread-active-p
 (setf th2 (make-thread
            #'(lambda ()
                (mutex-lock m2)
                (loop (sleep 1))))))
T

(progn
  (thread-interrupt th #'mutex-unlock m2)
  (thread-interrupt th #'mutex-unlock m2)
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

(symbol-cleanup '*thread-special*) T