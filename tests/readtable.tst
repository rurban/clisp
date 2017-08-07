;; -*- Lisp -*- vim:filetype=lisp

(readtablep (setq *readtable* (copy-readtable nil))) T

(setq $ 23)
23

(defun single-dollar-reader (stream char)
  (declare (ignore stream))
  (values (intern (string char))))
SINGLE-DOLLAR-READER

(set-macro-character #\$ #'single-dollar-reader)
T

$
23

(nth-value 2 (function-lambda-expression (get-macro-character #\$)))
SINGLE-DOLLAR-READER

(readtablep (setq *readtable* (copy-readtable nil))) t

(let ((*readtable* (copy-readtable nil)))
  (set-syntax-from-char #\" #\( )
  (multiple-value-list (read-from-string "\"1 2 3)")))
((1 2 3) 7)

"1234"                          ; no lingeting effects from the above
"1234"

(defun inverted-param-reader (stream char)
  (read-delimited-list #\( stream t))
INVERTED-PARAM-READER

(let ((*readtable* (copy-readtable nil)))
  (set-macro-character #\( (get-macro-character #\)))
  (set-macro-character #\) #'inverted-param-reader)
  (multiple-value-list (read-from-string ")a 1(")))
((A 1) 5)

(let ((*readtable* (copy-readtable nil)))
  (set-syntax-from-char #\" #\( )
  (set-macro-character #\( (get-macro-character #\)))
  (set-macro-character #\) #'inverted-param-reader)
  (set-macro-character #\" #'inverted-param-reader)
  (list (multiple-value-list (read-from-string "\"1 2 3("))
        (multiple-value-list (read-from-string ")a 1("))))
(((1 2 3) 7)
 ((A 1) 5))

"1234"
"1234"

'(1 2 3)
(1 2 3)

(defun my-dispatch-macro (stream subchar arg)
  (list 'my-dispatch-macro (streamp stream) subchar arg))
MY-DISPATCH-MACRO

(let ((*readtable* (copy-readtable nil)))
  (list (make-dispatch-macro-character #\x)
        (set-dispatch-macro-character #\x #\. #'my-dispatch-macro)
        (nth-value 2 (function-lambda-expression
                      (get-dispatch-macro-character #\x #\.)))
        (multiple-value-list (read-from-string "\\XS"))
        (multiple-value-list (read-from-string "123x.45"))
        (multiple-value-list (read-from-string "123x.45" t nil :start 3))
        (multiple-value-list (read-from-string "123x.45" t nil :start 5))))
(T T MY-DISPATCH-MACRO (XS 3) (123 3) ((MY-DISPATCH-MACRO T #\. NIL) 5) (45 7))

(get-dispatch-macro-character #\x #\.)
ERROR

(get-dispatch-macro-character #\y #\,)
ERROR

(get-dispatch-macro-character #\y #\.)
ERROR


(defun |#{-reader| (stream char arg)
  (declare (ignore char arg))
  (mapcon #'(lambda (x)
              (mapcar #'(lambda (y) (list (car x) y)) (cdr x)))
          (read-delimited-list #\} stream)))
|#{|-|reader|


(set-dispatch-macro-character #\# #\{ #'|#{-reader|)
T

(set-syntax-from-char #\} #\) ) T

(defparameter *read-st* (make-string-input-stream "#{p q z a} #{a b c d e}"))
*READ-ST*

(read *read-st*)
((P Q) (P Z) (P A) (Q Z) (Q A) (Z A))

(read *read-st*)
((A B) (A C) (A D) (A E) (B C) (B D) (B E) (C D) (C E) (D E))

;; final reset
(readtablep (setq *readtable* (copy-readtable nil nil))) t

(symbols-cleanup
 '($ single-dollar-reader inverted-param-reader my-dispatch-macro
   |#{-reader| *read-st*))
()
