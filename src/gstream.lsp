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
      (($open :type boolean :initform t)) ; whether the stream is open
) ) )

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
)
(clos:defgeneric (setf stream-element-type) (new-element-type stream)
  (:method (new-element-type (stream stream))
    (sys::built-in-stream-set-element-type stream new-element-type)
  )
  (:method (new-element-type (stream fundamental-stream))
    (clos:no-applicable-method #'(setf stream-element-type) new-element-type stream)
  )
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

