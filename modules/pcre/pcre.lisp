;; Module for PCRE / CLISP
;; <http://www.pcre.org/>
;; Sam Steingold 2003

(defpackage "PCRE"
  (:documentation
   "PCRE - Perl Compatible Regular Expressions - <http://www.pcre.org/>")
  (:use "LISP")
  (:export "PCRE-VERSION" "PCRE-CONFIG" "PCRE-COMPILE" "PCRE-EXEC" "PATTERN"
           "PATTERN-INFO" "PCRE-NAME-TO-INDEX" "MATCH-SUBSTRING"
           "MATCH-STRINGS" "MATCH-STRING" "MATCH" "MATCH-START" "MATCH-END"))

(in-package "PCRE")
(push "PCRE" ext:*system-package-list*)

(defstruct (pattern (:constructor make-pat (compiled study)))
  (compiled nil :read-only t)
  (study nil :read-only t))

(defstruct (match (:constructor make-match-boa (start end)) (:constructor))
  (start nil :read-only t)
  (end nil :read-only t))

(defun match-substring (match subject)
  "Return the substring corresponding to the match."
  (subseq subject (match-start match) (match-end match)))

(defun match-strings (ret-vec subject)
  "Return a vector of all substring that match any sub-patterns."
  (map 'vector (lambda (match)
                 (when match
                   (subseq subject (match-start match) (match-end match))))
       ret-vec))

(defun match-string (ret-vec which subject &optional pattern)
  "Return the substring that matches the given sub-pattern.
If which is a name of the sub-pattern, pattern must be supplied."
  (match-substring
   (svref ret-vec (etypecase which
                    (integer which)
                    (string (pcre-name-to-index pattern which))))
   subject))
