;; -*- Lisp -*-
;; clisp -q -norc -K full -i ../tests/tests -x '(run-test "regexp/test")'

(let ((rc (regexp:regexp-compile "a(a)*" :extended t)))
  (prog1 (multiple-value-list (regexp:regexp-exec rc "a"))
    (gc) (gc)))
(#S(REGEXP:MATCH :START 0 :END 1) NIL)
