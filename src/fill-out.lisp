;;; filling/indenting stream
;;;
;;; Copyright (C) 2004 by Sam Steingold
;;; Copyright (C) 2004 by Bruno Haible
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(in-package "EXT")
(export '(fill-stream with-fill-stream))
(import '(fill-stream with-fill-stream) "SYS")
(in-package "SYSTEM")

(defclass fill-stream (fundamental-character-output-stream)
  ((target-stream :initarg :stream :type stream)
   (buffer :type string :initform
           (make-array (or *print-right-margin* sys::*prin-linelength*)
                       :element-type 'character :fill-pointer 0
                       :adjustable t))
   ;; the indentation level variable or number:
   (indent-var :initarg :indent :initform 0 :type (or symbol integer))
   (pending-space :initform nil :type boolean)
   (current-indent :initform 0 :type integer) ; current line indentation
   (pending-indent :initform nil :type (or null integer))))
(defun line-pos (fill-stream)
  (with-slots (target-stream buffer pending-space) fill-stream
    (let ((pos (sys::line-position target-stream)))
      (if pos
          (+ pos (if pending-space 1 0) (string-width buffer))
          nil))))
;; flush the buffer and print a newline (when NEWLINE-P is non-NIL)
(defun fill-stream-flush-buffer (stream newline-p)
  (with-slots (target-stream buffer pending-indent current-indent indent-var
               pending-space)
      stream
    (flet ((newline ()          ; terpri
             (setq current-indent
                   (if (symbolp indent-var)
                       (symbol-value indent-var)
                       indent-var)
                   pending-indent current-indent)
             (terpri target-stream)))
      ;; fill: if the buffer does not fit on the line, TERPRI
      (let ((pos (line-pos stream)))
        (when (and pos
                   (<= (or *print-right-margin* sys::*prin-linelength*) pos))
          (newline)))
      (cond (pending-indent      ; do the indent
             (sys::write-spaces pending-indent target-stream)
             (setq pending-indent nil))
            (pending-space
             (write-char #\Space target-stream)))
      (setq pending-space nil)
      (write-char-sequence buffer target-stream)
      (setf (fill-pointer buffer) 0)
      (when newline-p (newline)))))
(progn
  (defmethod stream-write-char ((stream fill-stream) ch)
    (with-slots (buffer pending-space) stream
      #1=
      (case ch
        (#\Newline (fill-stream-flush-buffer stream t))
        ((#\Space #\Tab)
         (when (plusp (length buffer))
           (fill-stream-flush-buffer stream nil))
         (setq pending-space t))
        (t (vector-push-extend ch buffer)))))
  (defmethod stream-write-char-sequence ((stream fill-stream) sequence &optional (start 0) (end nil))
    (if (listp sequence)
      ; Convert list to a vector, to avoid quadratic runtime.
      (setq sequence (if end (subseq sequence start end) (subseq sequence start))
            start 0 end (length sequence))
      (unless end (setq end (length sequence))))
    (when (< start end)
      (with-slots (target-stream buffer current-indent pending-indent) stream
        (do ((pos start (1+ pos)))
            ((>= pos end))
          (let ((ch (elt sequence pos)))
            ; Same body as in stream-write-char.
            #1#))))))
(defmethod stream-line-column ((stream fill-stream))
  (let ((pos (line-pos stream)))
    (if pos (max (- pos (slot-value stream 'current-indent)) 0) nil)))
(defmethod stream-start-line-p ((stream fill-stream))
  (let ((pos (line-pos stream)))
    (if pos (<= pos (slot-value stream 'current-indent)) nil)))
(defmethod stream-finish-output ((stream fill-stream))
  (fill-stream-flush-buffer stream nil)
  (finish-output (slot-value stream 'target-stream)))
(defmethod stream-force-output ((stream fill-stream))
  (fill-stream-flush-buffer stream nil)
  (force-output (slot-value stream 'target-stream)))
(defmethod stream-clear-output ((stream fill-stream))
  (with-slots (target-stream buffer pending-indent pending-space) stream
    (setq pending-indent nil pending-space nil)
    (setf (fill-pointer buffer) 0)
    (clear-output target-stream)))

(defmacro with-fill-stream ((stream-var target-stream &key indent) &body body)
  (multiple-value-bind (body-rest declarations) (parse-body body)
    `(LET ((,stream-var (MAKE-INSTANCE 'fill-stream :STREAM ,target-stream
                                       :INDENT ,indent)))
       (DECLARE (READ-ONLY ,stream-var) ,@declarations)
       (UNWIND-PROTECT (PROGN ,@(or body-rest '(NIL)))
         (FORCE-OUTPUT ,stream-var)))))
