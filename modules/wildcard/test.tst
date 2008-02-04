;; -*- Lisp -*- vim:filetype=lisp
;; clisp -K full -E utf-8 -q -norc -i ../tests/tests -x '(run-test "wildcard/test")'

(wildcard:match "foo" "bar") NIL

(let ((*apropos-matcher* #'wildcard:wildcard-matcher))
  (apropos-list "wildcard*r"))
(WILDCARD:wildcard-matcher)

(wildcard:match "foo*bar" "foobar") T
(wildcard:match "foo*bar" "foo*bar") T
(wildcard:match "foo*bar" "fooAbar") T
(wildcard:match "foo*bar" "foo/bar") NIL
(wildcard:match "foo*bar" "fooABAR") NIL
(wildcard:match "foo*bar" "fooABAR" :case-insensitive t) T

