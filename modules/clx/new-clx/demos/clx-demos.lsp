;;; Common stuff for the demos
;;; Copyright (C) 1999 by Sam Steingold


(defpackage clx-demos
  (:use common-lisp)
  (:export qix sokoban))

(in-package :clx-demos)

(defun getenv (var)
  "Return the value of the environment variable."
  #+cmu (cdr (assoc (string var) *environment-list* :test #'equalp
                    :key #'string))
  #-cmu
  (#+(or allegro clisp) system::getenv #+lispworks lw:environment-variable
   #+lucid lcl:environment-variable #+gcl si:getenv (string var)))

(defun x-host-display (&optional (disp (getenv "DISPLAY")))
  "Parse the DISPLAY environment variable."
  (if disp
      (let ((pos (position #\: disp)))
        (values (subseq disp 0 pos) (parse-integer (subseq disp (1+ pos)))))
      (values "" 0)))

(defun x-open-display ()
  "Open the appropriate X display."
  (multiple-value-bind (host di) (x-host-display)
    (xlib:open-display host :display di)))

(load (merge-pathnames "qix" *load-truename*))
(load (merge-pathnames "sokoban" *load-truename*))
