;; -*- Lisp -*-

#+clisp (progn (setf (symbol-function 'setf-gethash)
                     (symbol-function 'sys::puthash)) t)
#+clisp t
#+(or akcl ecl) (progn (setf (symbol-function 'setf-gethash)
                             (symbol-function 'sys:hash-set)) t)
#+(or akcl ecl) t
#+allegro (progn (setf (symbol-function 'setf-gethash)
                       (symbol-function 'excl::%puthash)) t)
#+allegro t
#+cmu (progn (setf (symbol-function 'setf-gethash)
                   (symbol-function 'cl::%puthash)) t)
#+cmu t

(defun symbole ()
  (let ((b 0.)
        (hash-table (make-hash-table :size 20. :rehash-threshold #+xcl 15. #-xcl 0.75))
        (liste (make-list 50.))
        (liste2 (make-list 50.)))
    (rplacd (last liste) liste)
    (rplacd (last liste2) liste2)
    (do-symbols (x (find-package #+xcl 'lisptest #-xcl "LISP"))
      ;; (print x) (finish-output)
      (cond ((car liste)
             (let ((hval (gethash (car liste) hash-table))
                   (lval (car liste2)))
               (unless (eq hval lval)
                 (print "mist, hash-tabelle kaputt")
                 (print (car liste))
                 (print hash-table)
                 (print (hash-table-count hash-table))
                 (print "hval:") (print hval)
                 (print "lval:") (print lval)
                 (return-from symbole 'error))
               (remhash (car liste) hash-table)
               #+xcl (when (< (room) 30000.) (system::%garbage-collection))
               (setf-gethash x hash-table (setq b (+ 1. b)))
               (rplaca liste x)
               (rplaca liste2 b)
               (setq liste (cdr liste))
               (setq liste2 (cdr liste2))))
            (t (setf-gethash x hash-table (setq b (+ 1. b)))
               (rplaca liste x)
               (rplaca liste2 b)
               (setq liste (cdr liste))
               (setq liste2 (cdr liste2)))))))
symbole

(symbole) nil

(progn
  (setf ht1 (make-hash-table :test 'equal)
        ht2 (make-hash-table :test 'equal)
        (gethash ht1 ht1) 1
        (gethash ht2 ht1) 2
        (gethash ht2 ht2) 2
        (gethash ht1 ht2) 1
        (gethash 10 ht1) 11 (gethash 11 ht1) 12 (gethash 12 ht1) 13
        (gethash 12 ht2) 13 (gethash 11 ht2) 12 (gethash 10 ht2) 11)
  (equalp ht1 ht2))
T

(progn (setf (gethash 100 ht1) 101)
       (equalp ht1 ht2))
NIL

(progn (remhash 100 ht1)
       (equalp ht1 ht2))
T
