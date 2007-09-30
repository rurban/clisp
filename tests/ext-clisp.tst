;; -*- Lisp -*- vim:filetype=lisp
;; Test CLISP specific extensions (typically in package EXT)
;; Many tests already in alltest.tst, map.tst etc., but
;; here we avoid using #+clisp all over the place.
;; Jörg Höhle, 2007

;(setf if) 5.1.6
(mapcar (lambda (x &aux a b) (list (setf (if x a b) 2) a b)) '(t nil))
((2 2 nil) (2 nil 2))

(loop with a and b for x below 2
      collect (multiple-value-list
               (setf (if (zerop x) (values a b) (values b a)) (floor 7 5)))
      collect (cons a b))
((1 2) (1 . 2) (1 2) (2 . 1))

(let (x a b c) (setf (if (zerop x) (values a b) c) x))
ERROR ; different number of values in if branches

;(setf progn)

;(setf funcall)

;(setf locally)

;(setf values-list)

;5.1.2 fcase
(ext:fcase string= (let ((foo "two third")) (subseq foo 0 (position #\Space foo)))
  ("first" 1)
  (("second" "two") 2)
  (("true" "yes") t)
  (otherwise nil))
2

;5.1.3 xor
(multiple-value-list (xor nil nil nil))
(nil)

(multiple-value-list (ext:xor nil (block nil (return 'a)) nil))
(a 2)

;5.1.10 custom:*defun-accept-specialized-lambda-list*

;12. long-float-digits + setf long-float-digits

;12.3.2 ! (factorial)
(ext:! 10)
3628800
(ext:! 11)
39916800

(ext:! 0)
1

(ext:! 3/2)
ERROR

;17.1 ext:doseq

;18.1.4 ext:dohash
(defun test-dohash (hash-table)
  (let ((all-entries '())
        (generated-entries '())
        (unique (list nil)))
    (maphash #'(lambda (key value) (push (list key value) all-entries))
             hash-table)
    (dohash (key value hash-table)
      (declare (optimize safety))
      (unless (eql value (gethash key hash-table unique))
        (error "Key ~S not found for value ~S" key value))
      (push (list key value) generated-entries))
    (unless (= (length all-entries)
               (length generated-entries)
               (length (union all-entries generated-entries
                              :key #'car :test (hash-table-test hash-table))))
      (error "MAPHASH and EXT:DOHASH entries don't correspond"))
    t))
test-dohash

(let ((tab (make-hash-table :test #'equal))) ; see alltest.tst
  (setf (gethash "Richard" tab) "Gabriel")
  (setf (gethash "Bruno" tab) "Haible")
  (setf (gethash "Michael" tab) "Stoll")
  (setf (gethash "Linus" tab) "Torvalds")
  (setf (gethash "Richard" tab) "Stallman")
  (test-dohash tab)
)
T

(let ((hash-table (make-hash-table)) (entries '()))
  (setf (gethash 1 hash-table) 100)
  (setf (gethash 2 hash-table) 200)
  (sort (ext:dohash (key value hash-table entries)
          (declare (ignore key))
          (push value entries)) #'<))
(100 200)

(let ((hash-table (make-hash-table
                   :initial-contents '((1 . 100) (2 . 200))))
      (entries '()))
  (sort (ext:dohash (key value hash-table entries)
          (push value entries)
          (go skip)
          (push key entries)
          skip) #'<))
(100 200)

;30.11 ext:letf
(let ((x (list 1))) (list (ext:letf (((car x) 3)) (list x (copy-list x))) x))
(((1) (3)) (1))

(let ((x (list 1))) ; Bug #[ 1731462 ]
  (list (ext:letf (((car x) 3)
                   ((cdr x) (copy-list x)))
          (list x (copy-list x))) x))
(((1) (3 1)) (1))

(let (a b) (list (letf  (((values a b) (values 1 2))) (list a b)) (list a b)))
((1 2) (nil nil))

;ext:letf*
(let ((x (list 2))) (list (ext:letf* (((car x) 3)) (list x (copy-list x))) x))
(((2) (3)) (2))

(let ((x (list 1)))
  (list (ext:letf* (((car x) 3)
                    ((cdr x) (copy-list x)))
          (list x (copy-list x))) x))
(((1) (3 3)) (1))

(let (a b) (list (letf* (((values a b) (values 1 2))) (list a b)) (list a b)))
((1 2) (nil nil))

(let ((x (list 1)))
  (block nil (letf (((car x) 2)
                    ((cdr (progn (return) x)) 3)) x))
  x)
(1)

;letf ldb unsurprisingly restores bitfield bits, not the whole integer value
(let ((x (list #xAAAAAA)))
  (letf (((ldb (byte 5 9) (first x)) -1))
    (setf (first x) 0)) x)
(#x002a00)

(let ((x (list 1))) (letf (((first x) 3))))
nil

;letf/* within macrolet
(let ((x (list 1)))
  (macrolet ((frob () '(first x)))
    (letf  (((frob) 2)) (copy-list x))))
(2)

(let ((x (list 1)))
  (macrolet ((frob () '(first x)))
    (letf* (((frob) 2)) (copy-list x))))
(2)

(symbol-macrolet ((a *print-base*)) (letf  ((a 36)) (princ-to-string 20)))
"K"
(symbol-macrolet ((a *print-base*)) (letf* ((a 36)) (princ-to-string 20)))
"K"

(let (a b c) (symbol-macrolet ((a *print-base*)) (letf  (((values a b c) 36)) (princ-to-string 20))))
"K"

(let (a b c) (symbol-macrolet ((a *print-base*)) (letf* (((values a b c) 36)) (princ-to-string 20))))
"K"

(let ((a (vector 0 0))) (letf  (((values (aref a 0) (aref a 1)) (floor 7 5))) (copy-seq a)))
#(1 2)

(let ((a (vector 0 0))) (letf* (((values (aref a 0) (aref a 1)) (floor 7 5))) (copy-seq a)))
#(1 2)

(letf  (((values) 1)) 2)
2
(letf* (((values) 1)) 2)
2

;letf values-list

(makunbound 'xx)
xx

;letf on symbol-value breaks on unbound variable,
; so it's different from let + special declaration or progv (wishlist)
(letf (((symbol-value 'xx) 3)) (declare (special xx)) xx)
ERROR
;(progv (list 'xx) (list 3) ...)

;letf with gethash leaves default entry, not missing entry in table!
(multiple-value-list
 (gethash 1 (letf* ((h (make-hash-table)) ((gethash 1 h) 'a)) h) 2))
(nil t)

(multiple-value-list
 (gethash 1 (letf* ((h (make-hash-table)) ((gethash 1 h 'b) 'a)) h) 2))
(b t)

;letf on property lists leaves default value!
(let ((a '())) (letf* (((getf a :key) 1))) a)
(:key ())

(let ((a '())) (letf* (((getf a :key :default) 1))) a)
(:key :default)

(let ((a '())) (letf (((getf a :key :default) 1))) a)
(:key :default)

;21.3 read-char-will-hang-p etc.

;make-buffered-input-stream

;24.1.2 featurep
(every #'ext:featurep *features*)
T

(notany (lambda (x) (featurep `(not ,x))) *features*)
T

;30.11 ethe
((lambda (x) (declare (compile)) (ext:ethe integer x)) 3)
3

((lambda (x) (declare (compile)) (ext:ethe integer x)) t)
ERROR

(locally (declare (compile))
  (multiple-value-list (ethe (values integer float) (truncate 3.2 2))))
(1 1.2)

(locally (declare (compile)) (ethe (values float integer) (truncate 3.2 2)))
ERROR
