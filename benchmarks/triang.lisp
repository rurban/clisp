;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*- ;;;
;;;;TRIANG -- Board game benchmark.

(proclaim '(type simple-vector
                 triang-board triang-sequence triang-a triang-b triang-c))

(proclaim '(special triang-board triang-sequence triang-a triang-b triang-c
                    ))

  (setq triang-board (make-array 16. :initial-element 1))
  (setq triang-sequence (make-array 14. :initial-element 0))
  (setq triang-a
        (make-array 37. :initial-contents '(1 2 4 3 5 6 1 3 6 2 5 4
                                              11 12 13 7 8 4 4 7 11
                                              8 12 13 6 10 15 9 14
                                              13 13 14 15 9 10 6 6
                                              )))
  (setq triang-b
        (make-array 37. :initial-contents  '(2 4 7 5 8 9 3 6 10 5
                                               9 8 12 13 14 8 9 5
                                               2 4 7 5 8 9 3 6 10 5
                                               9 8 12 13 14 8 9
                                               5 5)))
  (setq triang-c
        (make-array 37. :initial-contents  '(4 7 11 8 12 13 6 10
                                               15 9 14 13 13 14
                                               15 9 10 6 1 2 4 3 5 6
                                               1 3 6 2 5 4 11 12 13
                                               7 8 4 4)))
  (defvar triang-answer)
  (defvar triang-final)
  (setf (aref triang-board 5) 0)


(defun triang-last-position ()
  (do ((i 1 (1+ i)))
      ((= i 16.) 0)
    (declare (fixnum i))
    (if (= 1 (the fixnum (aref triang-board i)))
        (return i))))

(defun triang-try (i depth)
  (declare (type fixnum i depth))
  (cond ((= depth 14)
         (let ((lp (triang-last-position)))
           (unless (member lp triang-final)
             (push lp triang-final)))
         (push (cdr (map 'list #'identity triang-sequence)) triang-answer) t)
        ((and (= 1 (the fixnum (aref triang-board (aref triang-a i))))
              (= 1 (the fixnum (aref triang-board (aref triang-b i))))
              (= 0 (the fixnum (aref triang-board (aref triang-c i)))))
         (setf (aref triang-board (aref triang-a i)) 0)
         (setf (aref triang-board (aref triang-b i)) 0)
         (setf (aref triang-board (aref triang-c i)) 1)
         (setf (aref triang-sequence depth) i)
         (do ((j 0 (1+ j))
              (depth (1+ depth)))
             ((or (= j 36.)
                  (triang-try j depth)) ())
           (declare (type fixnum j)))
         (setf (aref triang-board (aref triang-a i)) 1)
         (setf (aref triang-board (aref triang-b i)) 1)
         (setf (aref triang-board (aref triang-c i)) 0) ())))

(defun gogogo (i)
  (declare (type fixnum i))
  (let ((triang-answer ())
        (triang-final ()))
    (triang-try i 1)))

