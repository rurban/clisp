;; -*- Lisp -*-

(hash-table-weak-p
 (setq tab (make-hash-table :weak :key :test 'equal :initial-contents
                            '((1 . 2) ("foo" . "bar")))))
:key

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

(setf (hash-table-weak-p tab) :key)
:key

(progn (gc) t)
t

(gethash "foo" tab)
nil

(setf (hash-table-weak-p tab) :value) :value
(setf (gethash "foo" tab) 1) 1
(setf (gethash 1 tab) "bar") "bar"
(setf (gethash "zoo" tab) "zot") "zot"
(progn (gc) t) t
(gethash "foo" tab) 1
(gethash 1 tab) nil
(gethash "zoo" tab) nil

(setf (hash-table-weak-p tab) :either) :either
(setf (gethash "foo" tab) 1) 1
(setf (gethash 1 tab) "bar") "bar"
(setf (gethash "zoo" tab) "zot") "zot"
(progn (gc) t) t
(gethash "foo" tab) nil
(gethash 1 tab) nil
(gethash "zoo" tab) nil

(setf (hash-table-weak-p tab) :both) :both
(setf (gethash "foo" tab) 1) 1
(setf (gethash 1 tab) "bar") "bar"
(setf (gethash "zoo" tab) "zot") "zot"
(progn (gc) t) t
(gethash "foo" tab) 1
(gethash 1 tab) "bar"
(gethash "zoo" tab) nil
