;;; FREAD -- Benchmark to read from a file.
;;; Pronounced "FRED".  Requires the existence of FPRINT.TST which is created
;;; by FPRINT.

(defun fread ()
  (let ((stream (open "fprint.tst" :direction :input)))
    (read stream)
    (close stream)))

(eval-when (compile load eval) ; Seems dumb to eval at compile and load time.
  (if (not (probe-file "fprint.tst"))
      (format t "~%Define FPRINT.TST by running the FPRINT benchmark!")))

;;; call: (fread))

