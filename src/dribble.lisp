;;;; Dribble

(in-package "EXT")
(export '(make-dribble-stream dribble-stream-p dribble-stream
          dribble-stream-source dribble-stream-target dribble-toggle))

(in-package "SYSTEM")

(defun make-dribble-stream (source target)
  (make-two-way-stream (make-echo-stream source target)
                       (make-broadcast-stream source target)))
(defun dribble-stream (stream)
  (and (sys::two-way-stream-p stream)
       (let ((in (two-way-stream-input-stream stream))
             (out (two-way-stream-output-stream stream)))
         (and (sys::echo-stream-p in) (sys::broadcast-stream-p out)
              (let ((so (echo-stream-input-stream in))
                    (ta (echo-stream-output-stream in))
                    (bl (broadcast-stream-streams out)))
                (when (and (eq so (pop bl))
                           (eq ta (pop bl)))
                  (values so ta)))))))
(defun dribble-stream-p (obj) (not (null (dribble-stream obj))))
;; should this be integrated into CLOS and the rest of CLISP?
;; right now DRIBBLE-STREAM is not a recognizable subtype of TWO-WAY-STREAM.
;; should it be?  should is be printed specially?
(deftype dribble-stream () '(satisfies dribble-stream-p))
(defun dribble-stream-source (ds)
  (let ((so (dribble-stream ds)))
    (unless so (error 'type-error :datum ds :expected-type 'dribble-stream))
    so))
(defun dribble-stream-target (ds)
  (let ((ta (nth-value 1 (dribble-stream ds))))
    (unless ta (error 'type-error :datum ds :expected-type 'dribble-stream))
    ta))
(defun dribble-toggle (stream &optional file)
  (multiple-value-bind (so ta) (dribble-stream-p stream)
    (if so
      (if file                  ; already dribbling
        (warn (TEXT "Already dribbling ~S to ~S") so ta)
        (progn
          (setf (symbol-value symbol) so)
          (format ta (TEXT ";; Dribble of ~S finished ") so)
          (funcall (date-format) ta (multiple-value-list (get-decoded-time)))
          (terpri ta)
          (values so ta)))
      (if file                    ; not dribbling
        (let ((ta (if (and (streamp ta) (open-stream-p file)
                           (output-stream-p file))
                    file
                    (open file :direction :output
                          :if-exists :append
                          :if-does-not-exist :create))))
          (format ta (TEXT ";; Dribble of ~S started ") stream)
          (funcall (date-format) ta (multiple-value-list (get-decoded-time)))
          (terpri ta)
          (values (make-dribble-stream (symbol-value symbol) ta) ta))
        (warn (TEXT "Currently not dribbling from ~S.") symbol)))))

(defun dribble (&optional file)
  (multiple-value-bind (so ta) (dribble-toggle *terminal-io* file)
    (when (streamp so) (setq *terminal-io* so))
    (close ta)
    ta))
