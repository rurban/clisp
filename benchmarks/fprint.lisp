;;; FPRINT -- Benchmark to print to a file.

(defvar **fprint-test-atoms**
  '(abcdef12 cdefgh23 efghij34 ghijkl45 ijklmn56
    klmnop67 mnopqr78 opqrst89 qrstuv90 stuvwx01
    uvwxyz12 wxyzab23 xyzabc34 123456ab 234567bc
    345678cd 456789de 567890ef 678901fg 789012gh
    890123hi))

(defun fprint-init-aux (m n atoms)
  (declare (fixnum n m))
  (cond ((= m 0) (pop atoms))
        (t (do ((i n (the fixnum (- i 2)))
                (a ()))
               ((< i 1) a)
             (declare (fixnum i))
             (push (pop atoms) a)
             (push (fprint-init-aux (the fixnum (1- m)) n atoms) a)))))

(defun fprint-init (m n atoms)
  (declare (fixnum n m))
  (let ((atoms (subst () () atoms)))
    (do ((a atoms (cdr a)))
        ((null (cdr a)) (rplacd a atoms)))
    (fprint-init-aux m n atoms)))

(defvar **fprint-test-pattern** (fprint-init 6. 6. **fprint-test-atoms**))

(defun fprint ()
  (if (probe-file "fprint.tst")
      ;; this seems a little wierd, subsequent calls to FPRINT will be slower
      (delete-file "fprint.tst"))
  (let((stream (open "fprint.tst" :direction :output)))
    ;; defaults to STRING-CHAR
    (print **fprint-test-pattern** stream)
    (close stream)))

(eval-when (compile load eval)
  (if (probe-file "fprint.tst")
      (delete-file "fprint.tst")))

;;; call:  (fprint)

