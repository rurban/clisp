;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; low-level-representation:
;; Compiled functions (Cclosures), for which a certain bit is set in
;; the flag-byte of the code-vector. Additionally behind it:
;; - the signature, a signature struct (see compiler.lisp)
;; - the argument-precedence-order, as list of numbers from 0 to reqanz-1,
;; - the list of all methods.
;; - the method combination object

;; The compiler uses (at GENERIC-FLET, GENERIC-LABELS) and the evaluator
;; presupposes likewise, that a generic function does not change its
;; calling convention.
;; A generic function with signature (reqanz optanz restp keywords allowp)
;; is from the very beginning (!) a compiled function with
;;         reqanz  required parameters
;;         0       optional parameters
;;         &rest if and only if (or (> optanz 0) restp),
;;         without &key.
(defun callinfo (reqanz optanz restp keywords allowp)
  (declare (ignore keywords allowp))
  (list reqanz 0 (or (> optanz 0) restp) nil nil nil))

(defun gf-signature (gf)
  (sys::%record-ref gf 3))
(defun (setf gf-signature) (new gf)
  (setf (sys::%record-ref gf 3) new))

(defun gf-argorder (gf)
  (sys::%record-ref gf 4))
(defun (setf gf-argorder) (new gf)
  (setf (sys::%record-ref gf 4) new))

(defun gf-methods (gf)
  (sys::%record-ref gf 5))
(defun (setf gf-methods) (new gf)
  (setf (sys::%record-ref gf 5) new))

(defun gf-method-combination (gf)
  (sys::%record-ref gf 6))
(defun (setf gf-method-combination) (new gf)
  (setf (sys::%record-ref gf 6) new))
