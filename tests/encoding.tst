;; -*- Lisp -*-

;; http://sourceforge.net/tracker/index.php?func=detail&aid=543072&group_id=1355&atid=101355
(string=
 (ext:convert-string-from-bytes
  '#(255 254 65 0 13 0)
  (ext:make-encoding :charset 'charset:utf-16))
 (map 'string #'code-char '(65 13)))
t

(ext:convert-string-from-bytes
 '#(255 254 65 0 13) ; missing last 0
 (ext:make-encoding :charset 'charset:utf-16
                    :input-error-action :error))
ERROR

(ext:convert-string-from-bytes
 '#(255 254 65 0 13) ; missing last 0
 (ext:make-encoding :charset 'charset:utf-16
                    :input-error-action #\Z))
"AZ"

;; http://sourceforge.net/tracker/index.php?func=detail&aid=527380&group_id=1355&atid=101355
(ext:convert-string-to-bytes
 (map 'string #'code-char '(129 65))
 (ext:make-encoding :charset charset:cp1252
                    :output-error-action :ignore))
#(65)

;; from Bruno:
(let ((z #(27 36 40 68 43 35 43 83 43 100 27 40 66)))
  (equalp z (convert-string-to-bytes (convert-string-from-bytes
                                      z charset:ISO-2022-JP-2)
                                     charset:ISO-2022-JP-2)))
t
