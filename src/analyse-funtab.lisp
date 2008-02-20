;;; Copyright (C) 2008 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; Study which functions should be added to eval.d:FUNTAB

(defparameter *closures*
  (let ((ht (make-hash-table)))
    (do-all-symbols (s)
      (when (and (fboundp s) (not (special-operator-p s))
                 (let ((def (fdefinition s)))
                   (and (sys::closurep def)
                        (compiled-function-p def)))
                 (not (sys::subr-info s)))
        (setf (gethash s ht) t)))
    ht)
  "All compiled closures")

(defparameter *notinline-subrs*
  (let ((ht (make-hash-table)))
    (dolist (s (nth-value 2 (module-info "clisp" t)) ht)
      (unless (gethash s sys::function-codes)
        (setf (gethash s ht) 0))))
  "Lisp functions implemented in C but not inlined in FUNTAB")

(defun closure-map-funcalls (closure f)
  (multiple-value-bind (req-num opt-num rest-p
                        key-p keyword-list allow-other-keys-p
                        byte-list const-list)
      (sys::signature closure)
    (declare (ignore req-num opt-num rest-p
                     key-p keyword-list allow-other-keys-p))
    (let ((lap-list (sys::disassemble-LAP byte-list const-list)))
      (dolist (L lap-list)
        (let* ((instr (cdr L))
               (const (when (consp instr)
                        (case (first instr)
                          ((CALL CALL&PUSH) (nth (third instr) const-list))
                          ((CALL0 CALL1 CALL1&PUSH CALL1&JMPIFNOT CALL1&JMPIF
                            CALL2 CALL2&PUSH CALL2&JMPIFNOT CALL2&JMPIF
                            CONST&SYMBOL-FUNCTION&PUSH
                            CONST&SYMBOL-FUNCTION COPY-CLOSURE&PUSH COPY-CLOSURE
                            CONST&SYMBOL-FUNCTION&STORE TAGBODY-OPEN
                            HANDLER-OPEN)
                           (nth (second instr) const-list))))))
          (typecase const
            (null nil)
            (symbol (funcall f const))
            (function (closure-map-funcalls const f))))))))

#+(or)
(maphash (lambda (sym _t)
           (declare (ignore _t))
           (let ((def (fdefinition sym)))
             (closure-map-funcalls
              (if (sys::macrop def) (macro-function sym) def)
              (lambda (s) (incf (gethash s *notinline-subrs*))))))
         *closures*)
