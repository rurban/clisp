;; -*- Lisp -*-

(defvar *no-iconv-p* (null (ignore-errors (make-encoding :charset "utf-16"))))
*no-iconv-p*

;; http://sourceforge.net/tracker/index.php?func=detail&aid=543072&group_id=1355&atid=101355
(or *no-iconv-p*
    (string=
     (ext:convert-string-from-bytes
      '#(255 254 65 0 13 0)
      (ext:make-encoding :charset "utf-16"))
     (map 'string #'code-char '(65 13))))
t

;; either an error from no iconv, or from invalid string
(ext:convert-string-from-bytes
 '#(255 254 65 0 13) ; missing last 0
 (ext:make-encoding :charset "utf-16" :input-error-action :error))
ERROR

(if *no-iconv-p* "AZ"
    (ext:convert-string-from-bytes
     '#(255 254 65 0 13) ; missing last 0
     (ext:make-encoding :charset "utf-16" :input-error-action #\Z)))
"AZ"

;; http://sourceforge.net/tracker/index.php?func=detail&aid=527380&group_id=1355&atid=101355
(if *no-iconv-p* #(65)
    (ext:convert-string-to-bytes
     (map 'string #'code-char '(129 65))
     (ext:make-encoding :charset "cp1252" :output-error-action :ignore)))
#(65)

;; from Bruno:
(or *no-iconv-p*
    (let ((z #(27 36 40 68 43 35 43 83 43 100 27 40 66))
          (e (make-encoding :charset "ISO-2022-JP-2")))
      (equalp z (convert-string-to-bytes (convert-string-from-bytes z e) e))))
t
