;;; TAKL benchmark

(defun listn (n)
  (declare (type fixnum n))
   (if (not (= 0 n))
       (cons n (listn (the fixnum (1- n))))))

(defvar ll-18 (listn 18))
(defvar ll-12 (listn 12))
(defvar ll-6 (listn 6))

(defun mas (x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))

 (defun shorterp  (x y)
   (and y (or (null x)
              (shorterp (cdr x) (cdr y)))))

(defun mas-bench () (mas ll-18 ll-12 ll-6))
