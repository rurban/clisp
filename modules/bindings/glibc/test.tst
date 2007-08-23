;; -*- Lisp -*-
;; some tests for glibc
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "bindings/glibc/test")'

(defparameter *d* (show (linux:opendir "."))) *D*
(linux:dirent64-d_name (show (linux:readdir64 *d*))) "."
(linux:dirent64-d_name (show (linux:readdir64 *d*))) ".."
(linux:telldir *d*) 2
(linux:seekdir *d* 0) NIL
(linux:telldir *d*) 0
(= linux:DT_DIR (linux:dirent64-d_type (show (linux:readdir64 *d*)))) T
(linux:closedir *d*) 0
