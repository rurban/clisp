;;; filling/indenting stream
;;;
;;; Copyright (C) 2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(in-package "SYSTEM")

(defclass fill-stream (fundamental-character-output-stream)
  ((target-stream :initarg :stream :type stream)
   (buffer :type string :initform
           (make-array (or *print-right-margin* sys::*prin-linelength*)
                       :element-type 'character :fill-pointer 0
                       :adjustable t))
   ;; the indentation level variable or number:
   (indent-var :initarg :indent :initform 0 :type (or symbol integer))
   (current-indent :initform 0 :type integer) ; current line indentation
   (pending-indent :initform nil :type (or null integer))))
;; flush the buffer and print a newline (when NEWLINE-P is non-NIL)
(defun fill-stream-flush-buffer (stream newline-p)
  (with-slots (target-stream buffer pending-indent current-indent indent-var)
      stream
    (flet ((newline ()          ; terpri
             (setq current-indent
                   (* (if (symbolp indent-var)
                          (symbol-value indent-var)
                          indent-var)
                      *print-indent-lists*)
                   pending-indent current-indent)
             (terpri target-stream)))
      ;; fill: if the buffer does not fit on the line, TERPRI
      (let ((pos (sys::line-position target-stream)))
        (when (and pos
                   (<= (or *print-right-margin* sys::*prin-linelength*)
                       (+ (length buffer) pos)))
          (newline)))
      (when pending-indent      ; do the indent
        (sys::write-spaces pending-indent target-stream)
        (setq pending-indent nil))
      (write-char-sequence buffer target-stream)
      (setf (fill-pointer buffer) 0)
      (when newline-p (newline)))))
(defmethod stream-write-char ((stream fill-stream) ch)
  (with-slots (target-stream buffer current-indent pending-indent) stream
    (case ch
      (#\Newline (fill-stream-flush-buffer stream t))
      ((#\Space #\Tab)
       (fill-stream-flush-buffer stream nil)
       (write-char #\Space target-stream))
      (t (vector-push-extend ch buffer)))))
(defmethod stream-line-column ((stream fill-stream))
  (with-slots (target-stream buffer current-indent) stream
    (let ((pos (sys::line-position target-stream)))
      (if pos (max (- (+ (length buffer) pos) current-indent) 0) nil))))
(defmethod stream-start-line-p ((stream fill-stream))
  (with-slots (target-stream buffer current-indent) stream
    (let ((pos (sys::line-position target-stream)))
      (if pos (<= (+ (length buffer) pos) current-indent) nil))))
(defmethod stream-finish-output ((stream fill-stream))
  (fill-stream-flush-buffer stream nil)
  (finish-output (slot-value stream 'target-stream)))
(defmethod stream-force-output ((stream fill-stream))
  (fill-stream-flush-buffer stream nil)
  (force-output (slot-value stream 'target-stream)))
(defmethod stream-clear-output ((stream fill-stream))
  (with-slots (target-stream buffer pending-indent) stream
    (setq pending-indent nil)
    (setf (fill-pointer buffer) 0)
    (clear-output target-stream)))

(defmacro with-fill-stream ((stream-var target-stream &key indent) &body body)
  (multiple-value-bind (body-rest declarations) (parse-body body)
    `(LET ((,stream-var (MAKE-INSTANCE 'fill-stream :STREAM ,target-stream
                                       :INDENT ,indent)))
       (DECLARE (READ-ONLY ,stream-var) ,@declarations)
       (UNWIND-PROTECT (PROGN ,@(or body-rest '(NIL)))
         (FORCE-OUTPUT ,stream-var)))))
