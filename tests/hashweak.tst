;; -*- Lisp -*-

(hash-table-weak-p
 (setq tab (make-hash-table :weak t :test 'equal :initial-contents
                            '((1 . 2) ("foo" . "bar")))))
t

(gethash 1 tab)
2

(gethash "foo" tab)
"bar"

(gethash "zot" tab)
nil

(gethash "bar" tab)
nil

(progn (gc) t)
t

(gethash 1 tab)
2

(gethash "foo" tab)
nil

(gethash "zot" tab)
nil

(gethash "bar" tab)
nil

(setf (hash-table-weak-p tab) nil)
nil

(gethash 1 tab)
2

(gethash "foo" tab)
nil

(setf (gethash "foo" tab) "bar")
"bar"

(gethash "foo" tab)
"bar"

(progn (gc) t)
t

(gethash "foo" tab)
"bar"

(setf (hash-table-weak-p tab) t)
t

(progn (gc) t)
t

(gethash "foo" tab)
nil
