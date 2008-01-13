;; -*- Lisp -*-

(with-timeout (10 t) nil) NIL
(with-timeout (1 t) (sleep 100)) T
(y-or-n-p-timeout 1 t "y or n") T
(yes-or-no-p-timeout 0.5 nil "yes or no") NIL
