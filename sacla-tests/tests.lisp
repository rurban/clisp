;; Run Yuji Minejima's test suite

#+OLD-CLISP
;; Binding *ERROR-HANDLER* is a hammer technique for catching errors. It also
;; disables CLCS processing and thus breaks tests that rely on the condition
;; system, such as:
;;   - the compile-file on structure literal test in clos.lisp
;;   - all tests that use IGNORE-ERRORS
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(BLOCK ,b
       (LET ((*ERROR-HANDLER*
              #'(LAMBDA (&REST ARGS)
                  (with-standard-io-syntax
                    (let* ((*print-readably* nil)
                           (error-message (apply #'format nil (cdr args))))
                      (terpri) (princ error-message)
                      (return-from ,b (values 'error error-message)))))))
         ,@forms))))

#+(or AKCL ECL)
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym))
        (h (gensym)))
    `(BLOCK ,b
       (LET ((,h (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER)))
         (UNWIND-PROTECT
           (PROGN (SETF (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER)
                        #'(LAMBDA (&REST ARGS) (RETURN-FROM ,b 'ERROR)))
                  ,@forms)
           (SETF (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER) ,h))))))

#+ALLEGRO
(defmacro with-ignored-errors (&rest forms)
  (let ((r (gensym)))
    `(LET ((,r (MULTIPLE-VALUE-LIST (EXCL:ERRORSET (PROGN ,@forms)))))
       (IF (CAR ,r) (VALUES-LIST (CDR ,r)) 'ERROR))))

#-(or OLD-CLISP AKCL ECL ALLEGRO)
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(BLOCK ,b
       (HANDLER-BIND
         ((ERROR #'(LAMBDA (CONDITION)
                     (TERPRI) (PRINC CONDITION)
                     (RETURN-FROM ,b (values 'ERROR
                                             (princ-to-string condition))))))
         ,@forms))))

(defun merge-extension (type filename)
  (make-pathname :type type :defaults filename))

;; (lisp-implementation-type) may return something quite long, e.g.,
;; on CMUCL it returns "CMU Common Lisp".
(defvar lisp-implementation-type
  #+CLISP "CLISP" #+AKCL "AKCL" #+ECL "ECL" #+ALLEGRO "ALLEGRO" #+CMU "CMUCL"
  #-(or CLISP AKCL ECL ALLEGRO CMU) (lisp-implementation-type))

(defun do-simple-test (stream log &optional (ignore-errors t))
  (let ((eof "EOF") (error-count 0) (total-count 0))
    (loop
      (let ((form (read stream nil eof)))
        (when (eq form eof) (return))
        (incf total-count)
        (print form)
        (multiple-value-bind (my-result error-message)
            (let ((*print-circle* nil))
              (if ignore-errors
                (with-ignored-errors (eval form)) ; return ERROR on errors
                (eval form))) ; don't disturb the condition system when testing it!
          (if my-result
            (format t "~%OK: true")
            (progn
              (incf error-count)
              (format t "~%ERROR!! ~S should be TRUE !" my-result)
              (format log "~%Form: ~S~%CORRECT: true~%~7A: ~S~%~@[~A~%~]"
                             form lisp-implementation-type
                             my-result error-message))))))
    (values total-count error-count)))

(defvar *run-test-tester* #'do-simple-test)
(defvar *run-test-ignore-errors* t)

(defun run-test (testname
                 &optional (tester *run-test-tester*)
                           (ignore-errors *run-test-ignore-errors*)
                 &aux (logname (merge-extension "erg" testname))
                      error-count total-count)
  (with-open-file (s (merge-extension "lisp" testname) :direction :input)
    (format t "~&~s: started ~s~%" 'run-test s)
    (with-open-file (log logname :direction :output
                                 #+SBCL :if-exists #+SBCL :supersede
                                 #+ANSI-CL :if-exists #+ANSI-CL :new-version)
      (let ((*package* *package*) (*print-circle* t) (*print-pretty* nil))
        (setf (values total-count error-count)
              (funcall tester s log ignore-errors)))))
  (when (zerop error-count) (delete-file logname))
  (format t "~&~s: finished ~s (~:d error~:p out of ~:d test~:p)~%"
          'run-test testname error-count total-count)
  (values total-count error-count))

(defmacro with-accumulating-errors ((error-count total-count) &body body)
  (let ((err (gensym)) (tot (gensym)))
    `(multiple-value-bind (,tot ,err) (progn ,@body)
       (incf ,error-count ,err)
       (incf ,total-count ,tot))))

(defun run-files (files)
  (let ((error-count 0) (total-count 0))
    (dolist (file files)
      (with-accumulating-errors (error-count total-count)
        (run-test file)))
    (format
     t "~&~s: finished ~:d file~:p (~:d error~:p out of ~:d test~:p)~%~S~%"
     'run-files (length files) error-count total-count files)
    (values error-count total-count)))

(defun run-some-tests (&optional (dirlist '("./")))
  (let ((files (mapcan (lambda (dir)
                         (directory (make-pathname :name :wild :type "tst"
                                                   :defaults dir)))
                       dirlist)))
    (if files (run-files files)
        (warn "no TST files in directories ~S" dirlist))))

(defun run-all-tests (&optional (disable-risky t))
  (let ((error-count 0) (total-count 0)
        #+CLISP (custom:*warn-on-floating-point-contagion* nil)
        #+CLISP (custom:*warn-on-floating-point-rational-contagion* nil) )
    (dolist (ff `(                 "must-array.lisp"
                                   "must-character.lisp"
                                   "must-condition.lisp"
                                   "must-cons.lisp"
                                   "must-data-and-control.lisp"
                                   "must-do.lisp"
                                   "must-eval.lisp"
                                   "must-hash-table.lisp"
                                   "must-loop.lisp"
                                   "must-package.lisp"
                                   "must-printer.lisp"
                                   "must-reader.lisp"
                                   "must-sequence.lisp"
                                   "must-string.lisp"
                                   "must-symbol.lisp"
                                   "should-array.lisp"
                                   "should-character.lisp"
                                   "should-cons.lisp"
                                   "should-data-and-control.lisp"
                                   "should-eval.lisp"
                                   "should-hash-table.lisp"
                                   "should-package.lisp"
                                   "should-sequence.lisp"
                                   "should-string.lisp"
                                   "should-symbol.lisp"
                  #-CLISP          "desirable-printer.lisp"
                  #-CLISP          "x-sequence.lisp"))
      (with-accumulating-errors (error-count total-count) (run-test ff)))
    (format t "~s: grand total: ~:d error~:p out of ~:d test~:p~%"
            'run-all-tests error-count total-count)
    (values total-count error-count)))
