;; Test-Suiten ablaufen lassen:

#+CLISP
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(BLOCK ,b
       (LET ((*ERROR-HANDLER*
              #'(LAMBDA (&REST ARGS)
                  (DECLARE (IGNORE ARGS)) (RETURN-FROM ,b 'ERROR))))
         ,@forms))))

#+(or AKCL ECL)
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym))
        (h (gensym)))
    `(BLOCK ,b
       (LET ((,h (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER)))
         (UNWIND-PROTECT
           (PROGN (SETF (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER)
                        #'(LAMBDA (&REST ARGS) (RETURN-FROM ,b 'ERROR))
                  )
                  ,@forms
           )
           (SETF (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER) ,h)
     ) ) )
) )

#+ALLEGRO
(defmacro with-ignored-errors (&rest forms)
  (let ((r (gensym)))
    `(LET ((,r (MULTIPLE-VALUE-LIST (EXCL:ERRORSET (PROGN ,@forms)))))
       (IF (CAR ,r) (VALUES-LIST (CDR ,r)) 'ERROR)
     )
) )

#-(or CLISP AKCL ECL ALLEGRO)
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(BLOCK ,b
       (HANDLER-BIND
         ((ERROR
            #'(LAMBDA (CONDITION) (DECLARE (IGNORE CONDITION))
                (RETURN-FROM ,b 'ERROR)
              )
         ))
         ,@forms
     ) )
) )

#-(or ALLEGRO CMU)
(defun merge-extension (type filename)
  (merge-pathnames type filename)
)
#+(or ALLEGRO CMU)
(defun merge-extension (type filename)
  (merge-pathnames (make-pathname :type (subseq (string type) 1)) filename)
)

(defun do-test (stream log &optional (ignore-errors t))
  (let ((eof "EOF"))
    (loop
      (let ((form (read stream nil eof))
            (result (read stream nil eof)))
        (when (or (eq form eof) (eq result eof)) (return))
        (print form)
        (let ((my-result
                (if ignore-errors
                  (with-ignored-errors (eval form)) ; return ERROR on errors
                  (eval form) ; don't disturb the condition system when testing it!
             )) )
          (cond ((eql result my-result)
                 (format t "~%EQL-OK: ~S" result)
                )
                ((equal result my-result)
                 (format t "~%EQUAL-OK: ~S" result)
                )
                ((equalp result my-result)
                 (format t "~%EQUALP-OK: ~S" result)
                )
                (t
                 (format t "~%ERROR!! ~S should be ~S !" my-result result)
                 (format log "~%Form: ~S~%CORRECT: ~S~%~A: ~S~%"
                             form result
                             #+CLISP "CLISP" #+AKCL "AKCL" #+ECL "ECL" #+ALLEGRO "ALLEGRO" #+CMU "CMUCL"
                             my-result
                ))
) ) ) ) ) )

(defun do-errcheck (stream log &optional ignore-errors)
  (declare (ignore ignore-errors))
  (let ((eof "EOF"))
    (loop
      (let ((form (read stream nil eof))
            (errtype (read stream nil eof)))
        (when (or (eq form eof) (eq errtype eof)) (return))
        (print form)
        (let ((my-result (nth-value 1 (ignore-errors (eval form)))))
          (multiple-value-bind (typep-result typep-error)
              (ignore-errors (typep my-result errtype))
            (cond ((and (not typep-error) typep-result)
                   (format t "~%OK: ~S" errtype)
                  )
                  (t
                   (format t "~%ERROR!! ~S instead of ~S !" my-result errtype)
                   (format log "~%Form: ~S~%CORRECT: ~S~%~A: ~S~%"
                               form errtype
                               #+CLISP "CLISP" #+AKCL "AKCL" #+ECL "ECL" #+ALLEGRO "ALLEGRO" #+CMU "CMUCL"
                               my-result
                  ))
) ) ) ) ) ) )

(defun run-test (testname
                 &optional (tester #'do-test) (ignore-errors t)
                 &aux (logname (merge-extension ".erg" testname))
                      log-empty-p)
  (with-open-file (s (merge-extension ".tst" testname) :direction :input)
    (with-open-file (log logname :direction :output
                                 #+ANSI-CL :if-exists #+ANSI-CL :new-version)
      (let ((*package* *package*)
            (*print-pretty* nil))
        (funcall tester s log ignore-errors))
      #+CMU (finish-output log) ; otherwise (file-length log) may be less than (file-position log)
      (setq log-empty-p (zerop (file-length log)))
  ) )
  (when log-empty-p (delete-file logname))
  (values)
)

(defun run-all-tests ()
  (mapc #'run-test
        '( #-(or AKCL ECL)          "alltest"
                                    "array"
                                    "backquot"
           #-AKCL                   "characters"
           #+(or CLISP ALLEGRO CMU) "clos"
                                    "eval20"
                                    "floeps"
                                    "format"
           #+CLISP                  "genstream"
           #+XCL                    "hash"
                                    "hashlong"
          #+CLISP                   "hashweak"
                                    "iofkts"
                                    "lambda"
                                    "lists151"
                                    "lists152"
                                    "lists153"
                                    "lists154"
                                    "lists155"
                                    "lists156"
           #+(or CLISP ALLEGRO CMU) "loop"
                                    "macro8"
                                    "map"
           #+(or CLISP ALLEGRO CMU) "mop"
                                    "number"
           #+CLISP                  "number2"
           #-(or AKCL ALLEGRO CMU)  "pack11"
           #+(or XCL CLISP)         "path"
           #+XCL                    "readtable"
                                    "setf"
                                    "steele7"
           #-ALLEGRO                "streams"
                                    "streamslong"
                                    "strings"
           #-(or AKCL ECL)          "symbol10"
                                    "symbols"
           #+XCL                    "tprint"
           #+XCL                    "tread"
                                    "type"
  )      )
  #+(or CLISP ALLEGRO CMU)
  (run-test "conditions" #'do-test nil)
  (run-test "excepsit" #'do-errcheck)
  t
)
