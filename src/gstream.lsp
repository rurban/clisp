;;; Gray streams, following David N. Gray's STREAM-DEFINITION-BY-USER proposal

(in-package "LISP")
(export '(; Classes:
          fundamental-stream
          fundamental-input-stream
          fundamental-output-stream
          fundamental-character-stream
          fundamental-binary-stream
          fundamental-character-input-stream
          fundamental-character-output-stream
          fundamental-binary-input-stream
          fundamental-binary-output-stream
          ; Generic functions for character input:
          stream-read-char
          stream-unread-char
          stream-read-char-no-hang
          stream-peek-char
          stream-listen
          stream-read-char-status
          stream-read-char-sequence
          stream-read-line
          stream-clear-input
          ; Generic functions for character output:
          stream-write-char
          stream-line-column
          stream-start-line-p
          stream-write-char-sequence
          stream-write-string
          stream-terpri
          stream-fresh-line
          stream-finish-output
          stream-force-output
          stream-clear-output
          stream-advance-to-column
          ; Generic functions for binary input:
          stream-read-byte
          stream-read-byte-sequence
          ; Generic functions for binary output:
          stream-write-byte
          stream-write-byte-sequence
          ; Other generic functions:
          close
          open-stream-p
          stream-element-type
)        )

(in-package "SYSTEM")

;; Classes

(eval-when (compile load eval)
  (let ((clos::*allow-mixing-metaclasses* t))
    (clos:defclass fundamental-stream (stream clos:standard-object)
      (($open :type boolean :initform t) ; whether the stream is open
       ($reval :type boolean :initform nil) ; whether read-eval is allowed
) ) ) )

(clos:defclass fundamental-input-stream (fundamental-stream)
  ()
)

(clos:defclass fundamental-output-stream (fundamental-stream)
  ()
)

; Stuff these classes into the runtime system.
(%defgray
  (vector
    (clos:find-class 'fundamental-stream)        ; for STREAMP to work
    (clos:find-class 'fundamental-input-stream)  ; for INPUT-STREAM-P to work
    (clos:find-class 'fundamental-output-stream) ; for OUTPUT-STREAM-P to work
) )

(clos:defclass fundamental-character-stream (fundamental-stream)
  ()
)

(clos:defclass fundamental-binary-stream (fundamental-stream)
  ()
)

(clos:defclass fundamental-character-input-stream (fundamental-input-stream fundamental-character-stream)
  ()
)

(clos:defclass fundamental-character-output-stream (fundamental-output-stream fundamental-character-stream)
  ()
)

(clos:defclass fundamental-binary-input-stream (fundamental-input-stream fundamental-binary-stream)
  ()
)

(clos:defclass fundamental-binary-output-stream (fundamental-output-stream fundamental-binary-stream)
  ()
)

;; General generic functions

(clos:defgeneric close (stream &key abort)
  (:method ((stream stream) &rest args)
    (apply #'sys::built-in-stream-close stream args)
  )
  (:method ((stream fundamental-stream) &rest more-args)
    (declare (ignore more-args))
    (clos:with-slots ($open) stream
      (prog1 $open (setq $open nil))
  ) )
)

(clos:defgeneric open-stream-p (stream)
  (:method ((stream stream))
    (sys::built-in-stream-open-p stream)
  )
  (:method ((stream fundamental-stream))
    (clos:with-slots ($open) stream
      $open
  ) )
)

(clos:defgeneric stream-element-type (stream)
  (:method ((stream stream))
    (sys::built-in-stream-element-type stream)
  )
  (:method ((stream fundamental-stream))
    (clos:no-applicable-method #'stream-element-type stream)
  )
  (:method ((stream fundamental-character-stream))
    'CHARACTER
  )
)
(clos:defgeneric (setf stream-element-type) (new-element-type stream)
  (:method (new-element-type (stream stream))
    (sys::built-in-stream-set-element-type stream new-element-type)
  )
  (:method (new-element-type (stream fundamental-stream))
    (clos:no-applicable-method #'(setf stream-element-type) new-element-type stream)
  )
)

;; Generic functions for character input

; We define the methods on fundamental-input-stream, not
; fundamental-character-input-stream, so that people can use
; (setf stream-element-type).

(clos:defgeneric stream-read-char (stream))

(clos:defgeneric stream-unread-char (stream char))

(clos:defgeneric stream-read-char-no-hang (stream)
  (:method ((stream fundamental-input-stream))
    (stream-read-char stream)
  )
)

(clos:defgeneric stream-peek-char (stream)
  (:method ((stream fundamental-input-stream))
    (let ((c (stream-read-char stream)))
      (unless (eq c ':EOF) (stream-unread-char stream c))
      c
  ) )
)

(clos:defgeneric stream-listen (stream)
  (:method ((stream fundamental-input-stream))
    (let ((c (stream-read-char-no-hang stream)))
      (if (or (eq c 'NIL) (eq c ':EOF))
        nil
        (progn (stream-unread-char stream c) t)
  ) ) )
)

(clos:defgeneric stream-read-char-status (stream)
  (:method ((stream fundamental-input-stream))
    (let ((c (stream-read-char-no-hang stream)))
      (cond ((eq c 'NIL) ':WAIT)
            ((eq c ':EOF) ':EOF)
            (t (stream-unread-char stream c) ':INPUT-AVAILABLE)
  ) ) )
)

(clos:defgeneric stream-read-char-sequence (stream sequence &optional start end)
  (:method ((stream fundamental-input-stream) (sequence string) &optional (start 0) (end nil))
    ; sequence is a simple-string, and start and end are suitable integers.
    (unless end (setq end (length sequence)))
    (do ((index start (1+ index)))
        ((eql index end) index)
      (let ((c (stream-read-char stream)))
        (when (eq c ':EOF) (return index))
        (setf (char sequence index) c)
  ) ) )
)

(clos:defgeneric stream-read-line (stream)
  (:method ((stream fundamental-input-stream))
    (let ((buffer (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))
      (loop
        (let ((c (stream-read-char stream)))
          (cond ((eq c ':EOF) (return (values (coerce buffer 'simple-string) t)))
                ((eql c #\Newline) (return (values (coerce buffer 'simple-string) nil)))
                (t (vector-push-extend c buffer))
  ) ) ) ) )
)

(clos:defgeneric stream-clear-input (stream)
  (:method ((stream fundamental-input-stream))
    nil
  )
)

;; Generic functions for character output

; We define the methods on fundamental-output-stream, not
; fundamental-character-output-stream, so that people can use
; (setf stream-element-type).

(clos:defgeneric stream-write-char (stream character))

(clos:defgeneric stream-line-column (stream))

(clos:defgeneric stream-start-line-p (stream)
  (:method ((stream fundamental-output-stream))
    (eql (stream-line-column stream) 0)
  )
)

(clos:defgeneric stream-write-char-sequence (stream sequence &optional start end)
  (:method ((stream fundamental-output-stream) (sequence string) &optional (start 0) (end nil))
    ; sequence is a simple-string, and start and end are suitable integers.
    (unless end (setq end (length sequence)))
    (do ((index start (1+ index)))
        ((eql index end) nil)
      (stream-write-char stream (char sequence index))
  ) )
)

(clos:defgeneric stream-write-string (stream string &optional start end)
  (:method ((stream fundamental-output-stream) string &optional (start 0) (end nil))
    (stream-write-char-sequence stream string start end)
    string
  )
)

(clos:defgeneric stream-terpri (stream)
  (:method ((stream fundamental-output-stream))
    (stream-write-char stream #\Newline)
    nil
  )
)

(clos:defgeneric stream-fresh-line (stream)
  (:method ((stream fundamental-output-stream))
    (if (stream-start-line-p stream)
      nil
      (progn (stream-terpri stream) t)
  ) )
)

(clos:defgeneric stream-finish-output (stream)
  (:method ((stream fundamental-output-stream))
    nil
  )
)

(clos:defgeneric stream-force-output (stream)
  (:method ((stream fundamental-output-stream))
    nil
  )
)

(clos:defgeneric stream-clear-output (stream)
  (:method ((stream fundamental-output-stream))
    nil
  )
)

(clos:defgeneric stream-advance-to-column (stream column)
  (:method ((stream fundamental-output-stream) (column real))
    (let ((currcol (stream-line-column stream)))
      (if currcol
        (dotimes (i (- column currcol) t) (stream-write-char stream #\Space))
        nil
  ) ) )
)

;; Generic functions for binary input

(clos:defgeneric stream-read-byte (stream))

(clos:defgeneric stream-read-byte-sequence (stream sequence &optional start end)
  (:method ((stream fundamental-input-stream) (sequence vector) &optional (start 0) (end nil))
    ; sequence is a (simple-array (unsigned-byte 8) (*)), and start and end are suitable integers.
    (unless end (setq end (length sequence)))
    (do ((index start (1+ index)))
        ((eql index end) index)
      (let ((x (stream-read-byte stream)))
        (when (eq x ':EOF) (return index))
        (setf (aref sequence index) x)
  ) ) )
)

;; Generic functions for binary output

(clos:defgeneric stream-write-byte (stream integer))

(clos:defgeneric stream-write-byte-sequence (stream sequence &optional start end)
  (:method ((stream fundamental-output-stream) (sequence vector) &optional (start 0) (end nil))
    ; sequence is a (simple-array (unsigned-byte 8) (*)), and start and end are suitable integers.
    (unless end (setq end (length sequence)))
    (do ((index start (1+ index)))
        ((eql index end) nil)
      (stream-write-byte stream (aref sequence index))
  ) )
)

;;; ===========================================================================
;;; generic stream default methods
;;; Marcus Daniels 16.4.1994

(in-package "LISP")
(export '(generic-stream-read-char
          generic-stream-peek-char
          generic-stream-read-char-status
          generic-stream-clear-input
          generic-stream-write-char
          generic-stream-write-string
          generic-stream-finish-output
          generic-stream-force-output
          generic-stream-clear-output
          generic-stream-read-byte
          generic-stream-write-byte
          generic-stream-close
          generic-stream-controller
)        )

(in-package "SYSTEM")

(clos:defclass generic-stream-controller () ())

(clos:defgeneric generic-stream-read-char (controller))
(clos:defgeneric generic-stream-peek-char (controller))
(clos:defgeneric generic-stream-read-char-status (controller))
(clos:defgeneric generic-stream-clear-input (controller))
(clos:defgeneric generic-stream-write-char (controller ch))
(clos:defgeneric generic-stream-write-string (controller string start len))
(clos:defgeneric generic-stream-finish-output (controller))
(clos:defgeneric generic-stream-force-output (controller))
(clos:defgeneric generic-stream-clear-output (controller))
(clos:defgeneric generic-stream-read-byte (controller))
(clos:defgeneric generic-stream-write-byte (controller by))
(clos:defgeneric generic-stream-close (controller))

(clos:defmethod generic-stream-read-char ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-peek-char ((controller generic-stream-controller))
  (values (generic-stream-read-char controller) t)
)

(clos:defmethod generic-stream-read-char-status ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-clear-input ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-write-char ((controller generic-stream-controller) ch)
  (declare (ignore controller ch))
)

(clos:defmethod generic-stream-write-string ((controller generic-stream-controller) string start len)
  (dotimes (i len)
    (generic-stream-write-char controller (schar string (+ start i)))
) )

(clos:defmethod generic-stream-finish-output ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-force-output ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-clear-output ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-read-byte ((controller generic-stream-controller))
  (declare (ignore controller))
)

(clos:defmethod generic-stream-write-byte ((controller generic-stream-controller) by)
  (declare (ignore controller by))
)

(clos:defmethod generic-stream-close ((controller generic-stream-controller))
  (declare (ignore controller))
)

#| ;; Example:
;; Alias streams just perform the required operation on another given stream.
(defclass alias-controller (generic-stream-controller)
  ((orig-stream :initarg :orig-stream))
)
(defun make-alias-stream (orig-stream)
  (make-generic-stream
    (make-instance 'alias-controller :orig-stream orig-stream)
) )
(defmethod generic-stream-read-char ((controller alias-controller))
  (with-slots (orig-stream) controller
    (read-char orig-stream nil nil)
) )
(defmethod generic-stream-peek-char ((controller alias-controller))
  (with-slots (orig-stream) controller
    (values (peek-char nil orig-stream nil nil) nil)
) )
(defmethod generic-stream-read-char-status ((controller alias-controller))
  (with-slots (orig-stream) controller
    (if (listen orig-stream)
      ':INPUT-AVAILABLE
      (let ((ch (read-char-no-hang orig-stream nil t)))
        (cond ((eql ch t) ':EOF)
              ((null ch) ':WAIT) ; nothing available, not EOF
              (t (unread-char ch orig-stream) ':INPUT-AVAILABLE)
) ) ) ) )
(defmethod generic-stream-clear-input ((controller alias-controller))
  (with-slots (orig-stream) controller
    (clear-input orig-stream)
    t
) )
(defmethod generic-stream-write-char ((controller alias-controller) ch)
  (with-slots (orig-stream) controller
    (write-char ch orig-stream)
) )
#| ; not needed, see general method above
(defmethod generic-stream-write-string ((controller alias-controller) string start len)
  (with-slots (orig-stream) controller
    (dotimes (i len)
      (write-char (schar string (+ start i)) orig-stream)
) ) )
|#
(defmethod generic-stream-finish-output ((controller alias-controller))
  (with-slots (orig-stream) controller
    (finish-output orig-stream)
) )
(defmethod generic-stream-force-output ((controller alias-controller))
  (with-slots (orig-stream) controller
    (force-output orig-stream)
) )
(defmethod generic-stream-clear-output ((controller alias-controller))
  (with-slots (orig-stream) controller
    (clear-output orig-stream)
) )
(defmethod generic-stream-read-byte ((controller alias-controller))
  (with-slots (orig-stream) controller
    (read-byte orig-stream nil nil)
) )
(defmethod generic-stream-write-byte (i (controller alias-controller))
  (with-slots (orig-stream) controller
    (write-byte i orig-stream)
) )
(defmethod generic-stream-close ((controller alias-controller))
  ; don't close orig-stream
)
|#

