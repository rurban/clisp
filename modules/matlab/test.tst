;; -*- Lisp -*-
;; some tests for Matlab
;; clisp -K full -E utf-8 -q -norc -i ../tests/tests -x '(run-test "matlab/test")'

(matlab:invert-matrix #2a((1 2) (0 2)))
#2A((1d0 -1d0) (0d0 5d-1))

(let ((mx #2a((1 2 3) (4 5 6))))
  (matlab:copy-lisp-to-matlab mx "foo")
  (matlab:copy-matlab-to-lisp "foo" (make-array (array-dimensions mx))))
#2A((1d0 2d0 3d0) (4d0 5d0 6d0))

(let ((mx #2a((1 2 3) (4 5 6))))
  (matlab:copy-lisp-to-matlab mx "foo")
  (matlab:engEvalString matlab:*engine* "bar=foo'")
  (matlab:copy-matlab-to-lisp
   "bar" (make-array (reverse (array-dimensions mx)))))
#2A((1d0 4d0) (2d0 5d0) (3d0 6d0))
