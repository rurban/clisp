;; -*- Lisp -*-

(set-difference '(a b c) (a))
(b c)

(set-difference (loop :for i :from 1 :to 100 :collect i)
                (loop :for i :from 1 :to 99 :collect i)
                :test 'equalp)
(100)
