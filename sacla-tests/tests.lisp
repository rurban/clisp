;; Run Yuji Minejima's test suite

(load (make-pathname :name "tests" :type nil
                     :defaults (merge-pathnames "../tests/" *load-truename*)))

(setq *run-test-type* "lisp"
      *test-result-in-file* nil)
(defun run-all-tests ()
  (report-results (mapcar #'run-test
                          '("must-array"
                            "must-character"
                            "must-condition"
                            "must-cons"
                            "must-data-and-control"
                            "must-do"
                            "must-eval"
                            "must-hash-table"
                            "must-loop"
                            "must-package"
                            "must-printer"
                            "must-reader"
                            "must-sequence"
                            "must-string"
                            "must-symbol"
                            "should-array"
                            "should-character"
                            "should-cons"
                            "should-data-and-control"
                            "should-eval"
                            "should-hash-table"
                            "should-package"
                            "should-sequence"
                            "should-string"
                            "should-symbol"
                            #-CLISP          "desirable-printer"
                            #-CLISP          "x-sequence"))))
