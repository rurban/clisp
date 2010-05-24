;; -*- Lisp -*- vim:filetype=lisp
;; some tests for GP/PARI CALCULATOR
;; clisp -E 1:1 -q -norc -i ../tests/tests -x '(run-test "pari/test")'

(require "pari") t
(listp (show (multiple-value-list (ext:module-info "pari" t)) :pretty t)) t

(format t "~&Version: ~S~%" pari:pari-version) NIL

(defparameter id (pari:identity-matrix 3)) ID

