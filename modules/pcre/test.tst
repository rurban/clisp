;; -*- Lisp -*-
;; some tests for PCRE
;; clisp -q -norc -K full -i ../tests/tests -x '(run-test "pcre/test")'
(multiple-value-bind (ve ma mi) (pcre:pcre-version)
  (format t "~&testing: ~S (~D.~D)~%" ve ma mi))
NIL

(setq d (pcre:pcre-compile "(?P<date> (?P<year>(\\d\\d)?\\d\\d) - (?P<month>\\d\\d) - (?P<day>\\d\\d) )" :extended t :study t)
      s "today is 2003-12-15!"
      v (pcre:pcre-exec d s))
#(#S(PCRE::MATCH :START 9 :END 19) #S(PCRE::MATCH :START 9 :END 19)
  #S(PCRE::MATCH :START 9 :END 13) #S(PCRE::MATCH :START 9 :END 11)
  #S(PCRE::MATCH :START 14 :END 16) #S(PCRE::MATCH :START 17 :END 19))

(pcre:pattern-info d :options)
(:EXTENDED)

(pcre:pattern-info d :nametable)
(("date" . 1) ("day" . 5) ("month" . 4) ("year" . 2))

(pcre:pattern-info d :capturecount)
5

(pcre:pcre-exec d "")
NIL

(pcre:match-string v "year" s d)
"2003"

(pcre:match-string v "month" s d)
"12"

(pcre:match-string v "day" s d)
"15"

(pcre:match-string v "date" s d)
"2003-12-15"


(setq p (pcre:pcre-compile "(a|(z))(bc)")
      r (pcre:pcre-exec p "abc"))
#(#S(PCRE::MATCH :START 0 :END 3) #S(PCRE::MATCH :START 0 :END 1) NIL
  #S(PCRE::MATCH :START 1 :END 3))

(pcre:match-strings r "abc")
#("abc" "a" NIL "bc")

(pcre:pattern-info p :options)
NIL

(let ((cp (pcre:pcre-compile "a(a)*b" :extended t)))
  (pcre:pcre-exec cp "ab"))
#(#S(PCRE:MATCH :START 0 :END 2))

(let ((cp (pcre:pcre-compile "a(a)*(b)" :extended t)))
  (pcre:pcre-exec cp "ab"))
#(#S(PCRE:MATCH :START 0 :END 2) NIL #S(PCRE:MATCH :START 1 :END 2))

(progn (setq d nil s nil v nil p nil r nil)
       (gc)
       nil)
NIL
