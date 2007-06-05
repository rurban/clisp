;; -*- Lisp -*-
;; Test CLISP specific extensions (typically in package EXT)
;; Many tests already in alltest.tst, map.tst etc., but
;; here we avoid using #+clisp all over the place.
;; Jörg Höhle, 2007

;(setf if) 5.1.6
(mapcar (lambda (x &aux a b) (list (setf (if x a b) 2) a b)) '(t nil))
((2 2 nil) (2 nil 2))

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

;12. long-float-digits + setf long-float-digits

;12.3.2 ! (factorial)

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

;ext:letf 30.11
(let ((x (list 1))) (list (ext:letf (((car x) 3)) (list x (copy-list x))) x))
(((1) (3)) (1))

(let ((x (list 1))) ; Bug #[ 1731462 ]
  (list (ext:letf (((car x) 3)
                   ((cdr x) (copy-list x)))
          (list x (copy-list x))) x))
(((1) (3 1)) (1))

(let (a b) (list (letf (((values a b) (values 1 2))) (list a b)) (list a b)))
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

;letf/* within macrolet

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
