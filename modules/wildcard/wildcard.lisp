;; Module for wildcard matching in CLISP
;; Bruno Haible 18.4.1995
;; Sam Steingold 2001-2005

(defpackage "WILDCARD"
  (:modern t)
  (:use "COMMON-LISP" "FFI")
  (:export #:match #:wildcard-matcher))
(in-package "WILDCARD")

(default-foreign-language :stdc)

(c-lines "#include <config.h>~%")
(c-lines "#include <fnmatch.h>~%")

(def-call-out fnmatch
    (:arguments (pattern c-string)
                (string c-string)
                (flags int))
  (:return-type int))

;; flags values
(defconstant FNM_PATHNAME     1)
(defconstant FNM_FILE_NAME    1)
(defconstant FNM_NOESCAPE     2)
(defconstant FNM_PERIOD       4)
(defconstant FNM_LEADING_DIR  8)
(defconstant FNM_CASEFOLD    16)

(defun match (pattern string &key (start 0) (end nil) (case-insensitive nil))
  ;; Prepare the string.
  (unless (and (eql start 0) (null end))
    (unless end (setq end (length string)))
    (setq string (make-array (- end start) :element-type 'character
                                           :displaced-to string
                                           :displaced-index-offset start)))
  ;; Match.
  (zerop
    (fnmatch pattern string
             (logior FNM_PATHNAME (if case-insensitive FNM_CASEFOLD 0)))))

(defun wildcard-matcher (pattern)
  "A valid value for *APROPOS-MATCHER*."
  (lambda (name) (zerop (fnmatch pattern name FNM_CASEFOLD))))
