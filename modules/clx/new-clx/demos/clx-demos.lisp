;;; Common stuff for the demos
;;; Copyright (C) 1999-2005, 2007 by Sam Steingold (sds@gnu.org)
;;; GPL2 is applicable

(defpackage "CLX-DEMOS"
  (:use "COMMON-LISP" "XLIB" "EXT")
  (:shadowing-import-from "XLIB" "CHAR-WIDTH") ; EXT has CHAR-WIDTH
  (:export #:x-open-display #:run-all-demos #:*demos*))

(in-package :clx-demos)

(defun x-host-display (&optional (disp (getenv "DISPLAY")))
  "Parse the DISPLAY environment variable.
Return 3 values: host, server, screen."
  (if disp
      (let* ((pos1 (position #\: disp))
             (pos2 (and pos1 (position #\. disp :start pos1))))
        (values (subseq disp 0 pos1)
                (if pos1 (parse-integer (subseq disp (1+ pos1) pos2)) 0)
                (if pos2 (parse-integer (subseq disp (1+ pos2))) 0)))
      (values "" 0 0)))

(defun x-open-display ()
  "Open the appropriate X display."
  (multiple-value-bind (host di) (x-host-display)
    (xlib:open-display host :display di)))

(defparameter *demos*
  '((koch) (qix) (sokoban #:xpm) (greynetic) (petal) (hanoi) (recurrence)
    (plaid) (bball) (bwindow)))

(defmacro do-demos ((fun-var) &body body)
  (let ((demo (gensym "DO-DEMOS-DEMO-")) (reqs (gensym "DO-DEMOS-REQS-")))
    `(dolist (,demo *demos*)
       (destructuring-bind (,fun-var . ,reqs) ,demo
         (when (every #'find-package ,reqs)
           ,@body)))))

(do-demos (f)
  (let ((n (string-downcase f)))
    (require n (list (make-pathname :name n :defaults *load-truename*)))))
(do-demos (f)
  (export f)
  (format t "~&=== ~S ===~%~A~%~A~%" f (documentation f 'function)
          (cons f (ext:arglist f))))

(defun run-all-demos (&key (sleep 2))
  (do-demos (f)
    (format t "~&=== ~S ===~%" f)
    (funcall f)
    (sleep sleep)))
