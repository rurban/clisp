;;;; Dribble

(in-package "SYSTEM")

;;-----------------------------------------------------------------------------
;; DRIBBLE

; The use of an intermediate synonym-stream is for robustness.
; (Just try dribbling to a file on a full disk partition...)
(defvar *dribble-stream* nil)

(let ((dribble-file nil) (dribbled-input nil) (dribbled-output nil)
      (dribbled-error-output nil) (dribbled-trace-output nil)
      (dribbled-query-io nil) (dribbled-debug-io nil))
  (defun dribble (&optional file)
    (if file
      (progn
        (if dribble-file
          (warn (TEXT "Already dribbling to ~S")
                dribble-file
          )
          ;; Dribbling means to redirect all screen output to the file.
          ;; We redirect all standard streams. More precisely, those
          ;; which are #<SYNONYM-STREAM *TERMINAL-IO*>. Those which are
          ;; synonyms to other standard streams indirectly referring
          ;; to #<SYNONYM-STREAM *TERMINAL-IO*> are not redirected,
          ;; because that would cause each output to this stream to
          ;; be written twice to the dribble-file.
          (labels ((goes-to-terminal (stream) ; this is a hack
                     (and (typep stream 'synonym-stream)
                          (eq (synonym-stream-symbol stream) '*terminal-io*)
                   ) )
                   (goes-indirectly-to-terminal (stream) ; an even bigger hack
                     (and (typep stream 'synonym-stream)
                          (let ((sym (synonym-stream-symbol stream)))
                            (and (boundp sym)
                                 (let ((stream (symbol-value sym)))
                                   (or (goes-to-terminal stream)
                                       (goes-indirectly-to-terminal stream)
                  )) )    ) )    ) )
            (setq *dribble-stream* (open file :direction :output
                                              :if-exists :append
                                              :if-does-not-exist :create)
                  dribble-file (make-synonym-stream '*dribble-stream*)
                  dribbled-input nil
                  dribbled-output nil
                  dribbled-error-output nil
                  dribbled-trace-output nil
                  dribbled-query-io nil
                  dribbled-debug-io nil
            )
            (unless (goes-indirectly-to-terminal *standard-input*)
              (setq dribbled-input *standard-input*)
              (setq *standard-input* (make-echo-stream *standard-input* dribble-file))
            )
            (unless (goes-indirectly-to-terminal *standard-output*)
              (setq dribbled-output *standard-output*)
              (setq *standard-output* (make-broadcast-stream *standard-output* dribble-file))
            )
            (when (goes-to-terminal *error-output*)
              (setq dribbled-error-output *error-output*)
              (setq *error-output* (make-broadcast-stream *error-output* dribble-file))
            )
            (when (goes-to-terminal *trace-output*)
              (setq dribbled-trace-output *trace-output*)
              (setq *trace-output* (make-broadcast-stream *trace-output* dribble-file))
            )
            (when (goes-to-terminal *query-io*)
              (setq dribbled-query-io *query-io*)
              (setq *query-io*
                    (make-two-way-stream
                          (make-echo-stream *query-io* dribble-file)
                          (make-broadcast-stream *query-io* dribble-file)
            ) )     )
            (when (goes-to-terminal *debug-io*)
              (setq dribbled-debug-io *debug-io*)
              (setq *debug-io*
                    (make-two-way-stream
                          (make-echo-stream *debug-io* dribble-file)
                          (make-broadcast-stream *debug-io* dribble-file)
            ) )     )
        ) )
        *dribble-stream*
      )
      (if dribble-file
        (progn
          (when dribbled-input (setq *standard-input* dribbled-input))
          (when dribbled-output (setq *standard-output* dribbled-output))
          (when dribbled-error-output (setq *error-output* dribbled-error-output))
          (when dribbled-trace-output (setq *trace-output* dribbled-trace-output))
          (when dribbled-query-io (setq *query-io* dribbled-query-io))
          (when dribbled-debug-io (setq *debug-io* dribbled-debug-io))
          (setq dribble-file nil)
          (setq dribbled-input nil)
          (setq dribbled-output nil)
          (setq dribbled-error-output nil)
          (setq dribbled-trace-output nil)
          (setq dribbled-query-io nil)
          (setq dribbled-debug-io nil)
          (prog1
            *dribble-stream*
            (close *dribble-stream*)
            (setq *dribble-stream* (make-broadcast-stream))
        ) )
        (warn (TEXT "Currently not dribbling."))
) ) ) )
