;;;BEGIN
;;;FRPOLY
;;; Franz Lisp benchmark from Fateman
;; test from Berkeley based on polynomial arithmetic.

(defvar **v**)
(defvar ***x***)
(defvar **u***)
(defvar frpoly-r)
(defvar frpoly-r2)
(defvar frpoly-r3)

;;(declare (localf pcoefadd pcplus pcplus1 pplus ptimes ptimes1
;;                ptimes2 ptimes3 psimp pctimes pctimes1
;;                pplus1))
;; Franz uses maclisp hackery here; you can rewrite lots of ways.
(defmacro pointergp (x y) `(> (get ,x 'order)(get ,y 'order)))

(defmacro pcoefp (e) `(atom ,e))
(defmacro pzerop (x)
  (let ( (var (gensym)) )
    `(let ((,var ,x))
       (if (numberp ,var) (zerop ,var)))));true for 0 or 0.0
(defmacro pzero () 0)
(defmacro cplus (x y) `(+ ,x ,y))
(defmacro ctimes (x y) `(* ,x ,y))


(defun pcoefadd (e c x) (cond ((pzerop c) x)
                              (t (cons e (cons c x)))))

(defun pcplus (c p) (cond ((pcoefp p) (cplus p c))
                          (t (psimp (car p) (pcplus1 c (cdr p))))))

(defun pcplus1 (c x)
       (cond ((null x)
              (cond ((pzerop c) nil) (t (cons 0 (cons c nil)))))
             ((pzerop (car x)) (pcoefadd 0 (pplus c (cadr x)) nil))
             (t (cons (car x) (cons (cadr x) (pcplus1 c (cddr x)))))))

(defun pctimes (c p) (cond ((pcoefp p) (ctimes c p))
                           (t (psimp (car p) (pctimes1 c (cdr p))))))

(defun pctimes1 (c x)
       (cond ((null x) nil)
             (t (pcoefadd (car x)
                          (ptimes c (cadr x))
                          (pctimes1 c (cddr x))))))

(defun pplus (x y) (cond ((pcoefp x) (pcplus x y))
                         ((pcoefp y) (pcplus y x))
                         ((eq (car x) (car y))
                          (psimp (car x) (pplus1 (cdr y) (cdr x))))
                         ((pointergp (car x) (car y))
                          (psimp (car x) (pcplus1 y (cdr x))))
                         (t (psimp (car y) (pcplus1 x (cdr y))))))

(defun pplus1 (x y)
       (cond ((null x) y)
             ((null y) x)
             ((= (car x) (car y))
              (pcoefadd (car x)
                        (pplus (cadr x) (cadr y))
                        (pplus1 (cddr x) (cddr y))))
             ((> (car x) (car y))
              (cons (car x) (cons (cadr x) (pplus1 (cddr x) y))))
             (t (cons (car y) (cons (cadr y) (pplus1 x (cddr y)))))))

(defun psimp (var x)
       (cond ((null x) 0)
             ((atom x) x)
             ((zerop (car x)) (cadr x))
              (t (cons var x))))

(defun ptimes (x y) (cond ((or (pzerop x) (pzerop y)) (pzero))
                          ((pcoefp x) (pctimes x y))
                          ((pcoefp y) (pctimes y x))
                          ((eq (car x) (car y))
                           (psimp (car x) (ptimes1 (cdr x) (cdr y))))
                          ((pointergp (car x) (car y))
                           (psimp (car x) (pctimes1 y (cdr x))))
                          (t (psimp (car y) (pctimes1 x (cdr y))))))

(defun ptimes1 (***x*** y) (prog (**u*** **v**)
                               (setq **v** (setq **u*** (ptimes2 y)))
                          a    (setq ***x*** (cddr ***x***))
                               (cond ((null ***x***) (return **u***)))
                               (ptimes3 y)
                               (go a)))

(defun ptimes2 (y) (cond ((null y) nil)
                         (t (pcoefadd (+ (car ***x***) (car y))
                                      (ptimes (cadr ***x***) (cadr y))
                                      (ptimes2 (cddr y))))))

(defun ptimes3 (y)
  (prog (e u c)
     a1 (cond ((null y) (return nil)))
        (setq e (+ (car ***x***) (car y)))
        (setq c (ptimes (cadr y) (cadr ***x***) ))
        (cond ((pzerop c) (setq y (cddr y)) (go a1))
              ((or (null **v**) (> e (car **v**)))
               (setq **u*** (setq **v** (pplus1 **u*** (list e c))))
               (setq y (cddr y)) (go a1))
              ((= e (car **v**))
               (setq c (pplus c (cadr **v**)))
               (cond ((pzerop c)
                      (setq **u***
                            (setq **v**
                                  (pdiffer1 **u***
                                            (list (car **v**) (cadr **v**))))))
                     (t (rplaca (cdr **v**) c)))
               (setq y (cddr y))
               (go a1)))
     a  (cond ((and (cddr **v**)
                    (> (caddr **v**) e))
               (setq **v** (cddr **v**)) (go a)))
        (setq u (cdr **v**))
     b  (cond ((or (null (cdr u)) (< (cadr u) e))
               (rplacd u (cons e (cons c (cdr u)))) (go e)))
        (cond ((pzerop (setq c (pplus (caddr u) c)))
                                        (rplacd u (cdddr u)) (go d))
              (t (rplaca (cddr u) c)))
     e  (setq u (cddr u))
     d  (setq y (cddr y))
        (cond ((null y) (return nil)))
        (setq e (+ (car ***x***) (car y)))
        (setq c (ptimes (cadr y) (cadr ***x***)))
     c  (cond ((and (cdr u) (> (cadr u) e)) (setq u (cddr u)) (go c)))
        (go b)))

(defun pexptsq (p n)
        (do ((n (floor n 2) (floor n 2))
             (s (cond ((oddp n) p) (t 1))))
            ;;((zerop n) s)
            ((zerop n) nil) ;;The results make a mess when printed!
            (setq p (ptimes p p))
            (and (oddp n) (setq s (ptimes s p))) ))

(eval-when (load eval)
  (setf (get 'x 'order) 1)
  (setf (get 'y 'order) 2)
  (setf (get 'z 'order) 3)
  (setq
   ;; frpoly-r= x+y+z+1
   frpoly-r (pplus '(x 1 1 0 1) (pplus '(y 1 1) '(z 1 1)))
   ;; frpoly-r2 = 100000*r
   frpoly-r2 (ptimes frpoly-r 100000)
   ;; frpoly-r3 = frpoly-r with floating point coefficients
   frpoly-r3 (ptimes frpoly-r 1.0)))

;;; four sets of three tests, call:
;;; (pexptsq frpoly-r 2) (pexptsq frpoly-r2 2) (pexptsq frpoly-r3 2)
;;; (pexptsq frpoly-r 5) (pexptsq frpoly-r2 5) (pexptsq frpoly-r3 5)
;;; (pexptsq frpoly-r 10) (pexptsq frpoly-r2 10) (pexptsq frpoly-r3 10)
;;; (pexptsq frpoly-r 15) (pexptsq frpoly-r2 15) (pexptsq frpoly-r3 15)

#|

 (defun setup nil
  (putprop 'x 1 'order)
  (putprop 'y 2 'order)
  (putprop 'z 3 'order)
  (setq r (pplus '(x 1 1 0 1) (pplus '(y 1 1) '(z 1 1)))) ; r= x+y+z+1
  (setq r2 (ptimes r 100000)) ;r2 = 100000*r
  (setq r3 (ptimes r 1.0))); r3 = r with floating point coefficients

;; time various computations of powers of polynomials, not counting
;;printing but including gc time ; provide account of g.c. time.

 (include "timer.lsp")
 (timer timit1
  (pexptsq r n) n)
 (timer timit2
  (pexptsq r2 n) n)
 (timer timit3
  (pexptsq r3 n) n)

 (defun bench (n)
 (print 'test1)
 (timit1 n)
 (print 'test2)
 (timit2 n)
 (print 'test3)(timit3 n))

 (setup)
; then (bench 2) ; this should be pretty fast.
; then (bench 5)
; then (bench 10)
; then (bench 15)
;...

;;;END

|#
;;This is a cross-compiler kludge, too. The names are stupid.

(defun pdiffer1 (stupidx stupidy)
  (declare (ignore stupidx stupidy))
  nil)

