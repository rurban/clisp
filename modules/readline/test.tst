;; -*- Lisp -*-
;; some tests for READLINE
;; clisp -q -norc -i ../tests/tests -x '(run-test "readline/test")'

(let ((ver-num (and (boundp 'readline:readline-version)
                    readline:readline-version)))
  (format t "~&readline version ~S (~D=0~O=x~X)~%"
          (and (boundp 'readline:library-version) readline:library-version)
          ver-num ver-num ver-num))
nil

(if (boundp 'readline:editing-mode) readline:editing-mode 1) 1
(if (boundp 'readline:insert-mode)  readline:insert-mode 1)  1
(if (boundp 'readline:readline-name) readline:readline-name "CLISP") "CLISP"
(setq readline:readline-name "abazonk") "abazonk"
readline:readline-name                  "abazonk"

(readline:history-stifled-p)    0
(readline:stifle-history 100) NIL
(readline:history-stifled-p)    1
(abs (readline:unstifle-history)) 100 ; 5=>100, 4.2=>-100
(readline:history-stifled-p)    0

(readline:where-history)       0
(readline:history-total-bytes) 0

(defparameter *history-file* "readline-history-file") *history-file*
(readline:write-history *history-file*)            0
(readline:append-history 1000 *history-file*)      0
(readline:read-history *history-file*)             0
(readline:read-history-range *history-file* 0 -1)  0
(readline:history-truncate-file *history-file* 10) 0
(probe-file (delete-file *history-file*))          NIL
