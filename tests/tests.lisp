;; run the test suit:

(defun princ-error (c) (format t "~&[~A]: ~A~%" (type-of c) c))

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

#+(or (and AKCL (not GCL)) ECL)
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

#-(or OLD-CLISP (and AKCL (not GCL)) ECL ALLEGRO)
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(BLOCK ,b
       (HANDLER-BIND
         ((ERROR #'(LAMBDA (CONDITION)
                     (PRINC-ERROR CONDITION)
                     (RETURN-FROM ,b (values 'ERROR
                                             (princ-to-string condition))))))
         ,@forms))))

(defun merge-extension (type filename)
  (make-pathname :type type :defaults filename))

;; (lisp-implementation-type) may return something quite long, e.g.,
;; on CMUCL it returns "CMU Common Lisp".
(defvar lisp-implementation
  #+CLISP "CLISP" #+(and AKCL (not GCL)) "AKCL" #+GCL "GCL" #+ECL "ECL" #+ALLEGRO "ALLEGRO" #+CMU "CMUCL"
  #-(or CLISP AKCL GCL ECL ALLEGRO CMU) (lisp-implementation-type))

(defvar *eval-method* :eval)
(defun my-eval (form)
  (ecase *eval-method*
    (:eval (eval form))
    (:compile (funcall (compile nil `(lambda () ,form))))
    (:both (let ((e-value (eval form))
                 (c-value (funcall (compile nil `(lambda () ,form)))))
             (unless (equalp e-value c-value)
               (error "eval: ~S; compile: ~S" e-value c-value))
             e-value))))

(defvar *test-ignore-errors* t)
(defun do-test (stream log)
  (let ((eof "EOF") (error-count 0) (total-count 0))
    (loop
      (let ((form (read stream nil eof))
            (result (read stream nil eof)))
        (when (or (eq form eof) (eq result eof)) (return))
        (incf total-count)
        (fresh-line) (prin1 form) (terpri)
        (multiple-value-bind (my-result error-message)
            (if *test-ignore-errors*
                (with-ignored-errors (my-eval form)) ; return ERROR on errors
                (my-eval form)) ; don't disturb the condition system when testing it!
          (cond ((eql result my-result)
                 (format t "~&EQL-OK: ~S~%" result))
                ((equal result my-result)
                 (format t "~&EQUAL-OK: ~S~%" result))
                ((equalp result my-result)
                 (format t "~&EQUALP-OK: ~S~%" result))
                (t
                 (incf error-count)
                 (format t "~&ERROR!! ~S should be ~S !~%" my-result result)
                 (format log "~&Form: ~S~%CORRECT: ~S~%~7A: ~S~%~@[~A~%~]~%"
                             form result lisp-implementation
                             my-result error-message)
                 (when (and (typep result 'sequence)
                            (typep my-result 'sequence))
                   (let ((pos (mismatch result my-result :test #'equalp)))
                     (let ((*print-length* 10))
                       (flet ((pretty-tail-10 (seq)
                                (if (and (> (length seq) (+ pos 10))
                                         (typep seq 'string))
                                  (concatenate 'string (subseq seq pos (+ pos 10)) "...")
                                  (subseq seq pos))))
                         (format log "~&Differ at position ~:D: ~S vs ~S~%CORRECT: ~S~%~7A: ~S~%"
                                 pos
                                 (if (< pos (length result)) (elt result pos) 'end-of-sequence)
                                 (if (< pos (length my-result)) (elt my-result pos) 'end-of-sequence)
                                 (pretty-tail-10 result)
                                 lisp-implementation
                                 (pretty-tail-10 my-result)))))))))))
    (values total-count error-count)))

(defun do-errcheck (stream log)
  (let ((eof "EOF") (error-count 0) (total-count 0))
    (loop
      (let ((form (read stream nil eof))
            (errtype (read stream nil eof)))
        (when (or (eq form eof) (eq errtype eof)) (return))
        (incf total-count)
        (fresh-line) (prin1 form) (terpri)
        (let ((my-result (nth-value 1 (ignore-errors (my-eval form)))))
          (multiple-value-bind (typep-result typep-error)
              (ignore-errors (typep my-result errtype))
            (cond ((and (not typep-error) typep-result)
                   (format t "~&OK: ~S~%" errtype))
                  (t
                   (incf error-count)
                   (format t "~&ERROR!! ~S instead of ~S !~%" my-result errtype)
                   (format log "~&Form: ~S~%CORRECT: ~S~%~7A: ~S~%"
                               form errtype lisp-implementation
                               my-result)))))))
    (values total-count error-count)))

(defvar *run-test-tester* #'do-test)
(defun run-test (testname
                 &key ((:tester *run-test-tester*) *run-test-tester*)
                      ((:ignore-errors *test-ignore-errors*)
                        *test-ignore-errors*)
                      ((:eval-method *eval-method*) *eval-method*)
                      (logname testname)
                 &aux (logfile (merge-extension "erg" logname))
                      error-count total-count)
  (with-open-file (s (merge-extension "tst" testname) :direction :input)
    (format t "~&~s: started ~s~%" 'run-test s)
    (with-open-file (log logfile :direction :output
                                 #+SBCL :if-exists #+SBCL :supersede
                                 #+ANSI-CL :if-exists #+ANSI-CL :new-version)
      (let ((*package* *package*) (*print-circle* t) (*print-pretty* nil))
        (setf (values total-count error-count)
              (funcall *run-test-tester* s log)))))
  (when (zerop error-count) (delete-file logfile))
  (format t "~&~s: finished ~s (~:d error~:p out of ~:d test~:p)~%"
          'run-test testname error-count total-count)
  (values total-count error-count))

(defmacro with-accumulating-errors ((error-count total-count) &body body)
  (let ((err (gensym)) (tot (gensym)))
    `(multiple-value-bind (,tot ,err) (progn ,@body)
       (incf ,error-count ,err)
       (incf ,total-count ,tot))))

(defun run-files (files)
  (let* ((error-count 0) (total-count 0)
         (here (truename "./"))
         (res (mapcar (lambda (file)
                        (multiple-value-bind (tot err) (run-test file)
                          (incf error-count err)
                          (incf total-count tot)
                          (list file err tot)))
                      files)))
    (format t "~&~s: finished ~3d file~:p: ~3:d error~:p out of ~5:d test~:p~%"
            'run-files (length files) error-count total-count)
    (loop :for rec :in res :for count :upfrom 1 :do
      (format t "~&[~d] ~25@a: ~3:d error~:p out of ~5:d test~:p~%" count
              (enough-namestring (first rec) here) (second rec) (third rec)))
    (values error-count total-count res)))

(defun run-some-tests (&key (dirlist '("./"))
                       ((:eval-method *eval-method*) *eval-method*))
  (let ((files (mapcan (lambda (dir)
                         (directory (make-pathname :name :wild :type "tst"
                                                   :defaults dir)))
                       dirlist)))
    (if files (run-files files)
        (warn "no TST files in directories ~S" dirlist))))

(defun run-all-tests (&key (disable-risky t)
                      ((:eval-method *eval-method*) *eval-method*))
  (let ((error-count 0) (total-count 0)
        #+CLISP (custom:*load-paths* nil)
        #+CLISP (custom:*warn-on-floating-point-contagion* nil)
        #+CLISP (custom:*warn-on-floating-point-rational-contagion* nil)
        (*features* (if disable-risky *features*
                        (cons :enable-risky-tests *features*))))
    (dolist (ff `(#-(or AKCL ECL)   "alltest"
                                    "array"
                  #-OpenMCL         "backquot"
                  #+CLISP           "bin-io"
                  #-(and AKCL (not GCL)) "characters"
                  #+(or CLISP ALLEGRO CMU OpenMCL) "clos"
                  #+CLISP ,@(unless disable-risky '("defhash"))
                  #+(and CLISP UNICODE) "encoding"
                                    "eval20"
                  #+(and CLISP FFI) "ffi"
                                    "floeps"
                  #-OpenMCL         "format"
                  #+CLISP           "genstream"
                  #+XCL             "hash"
                                    "hashlong"
                  #+CLISP           "hashtable"
                                    "iofkts"
                                    "lambda"
                                    "lists151"
                  #-(or GCL OpenMCL) "lists152"
                                    "lists153"
                                    "lists154"
                                    "lists155"
                                    "lists156"
                  #+(or CLISP GCL ALLEGRO CMU SBCL OpenMCL) "loop"
                                    "macro8"
                                    "map"
                  #+(or CLISP ALLEGRO CMU OpenMCL) "mop"
                                    "number"
                  #+CLISP           "number2"
                  #-(or AKCL ALLEGRO CMU OpenMCL) "pack11"
                  #+(or XCL CLISP)  "path"
                  #+XCL             "readtable"
                  #-CMU             "setf"
                                    "steele7"
                  #-ALLEGRO         "streams"
                                    "streamslong"
                                    "strings"
                  #-(or AKCL ECL)   "symbol10"
                                    "symbols"
                  #+XCL             "tprint"
                  #+XCL             "tread"
                                    "type"
                  #+CLISP           "weak"
                  #+CLISP           "hashweak"))
      (with-accumulating-errors (error-count total-count) (run-test ff)))
    (with-accumulating-errors (error-count total-count)
      (run-test "bind" :eval-method :eval :logname "bind-eval"))
    (with-accumulating-errors (error-count total-count)
      (run-test "bind" :eval-method :compile :logname "bind-compile"))
    #+CLISP
    (dotimes (i 20)
      (with-accumulating-errors (error-count total-count)
        (run-test "weakptr")))
    #+(or CLISP ALLEGRO CMU)
    (with-accumulating-errors (error-count total-count)
      (run-test "conditions" :ignore-errors nil))
    (with-accumulating-errors (error-count total-count)
      (run-test "excepsit" :tester #'do-errcheck))
    (format t "~&~s: grand total: ~:d error~:p out of ~:d test~:p~%"
            'run-all-tests error-count total-count)
    (values total-count error-count)))
