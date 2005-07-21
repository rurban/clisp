;; -*- Lisp -*-

;;(with-open-file (f "ucs" :direction :output #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede :element-type '(unsigned-byte 8))
;;  (write-byte-sequence #(0 65 0) f))

;; <http://sourceforge.net/tracker/index.php?func=detail&aid=543072&group_id=1355&atid=101355>
(string=
 (ext:convert-string-from-bytes '#(0 65 0 13) charset:ucs-2)
 (map 'string #'code-char '(65 13)))
t

(ext:convert-string-from-bytes
 '#(0 65 0) ; wrong length
 (ext:make-encoding :charset charset:ucs-2 :input-error-action :error))
ERROR

(ext:convert-string-from-bytes
 '#(0 65 0) ; wrong length
 (ext:make-encoding :charset charset:ucs-2 :input-error-action #\Z))
"AZ"

(string=
 (ext:convert-string-from-bytes '#(0 0 0 65 0 0 0 13) charset:ucs-4)
 (map 'string #'code-char '(65 13)))
t

(ext:convert-string-from-bytes
 '#(0 0 0 65 0 0) ; wrong length
 (ext:make-encoding :charset charset:ucs-4 :input-error-action :error))
ERROR

(ext:convert-string-from-bytes
 '#(0 0 0 65 0 0 0) ; wrong length
 (ext:make-encoding :charset charset:ucs-4 :input-error-action #\Z))
"AZ"

(defparameter *no-iconv-p*
  (with-ignored-errors (not (make-encoding :charset "utf-16"))))
*no-iconv-p*

;; these are broken with glibc 2.2.2, but work with glibc 2.2.5
;; see <http://article.gmane.org/gmane.lisp.clisp.devel/9746>
(if *no-iconv-p* t
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
;; this is broken due to a bug in glibc2.2/3 (works with gnu libiconv)
;(or *no-iconv-p*
;    (let ((z #(27 36 40 68 43 35 43 83 43 100 27 40 66))
;          (e (make-encoding :charset "ISO-2022-JP-2")))
;      (equalp z (convert-string-to-bytes (convert-string-from-bytes z e) e))))
;t

(let ((z (coerce #(97 98 99) '(vector (unsigned-byte 8)))))
  (list (ext:convert-string-from-bytes z charset:ascii :start 0 :end 2)
        (ext:convert-string-from-bytes z charset:ascii :start 0 :end 3)
        (ext:convert-string-from-bytes z charset:ascii :start 1 :end 3)
        (ext:convert-string-from-bytes z charset:ascii :start 1 :end 2)))
("ab" "abc" "bc" "b")

(let ((z "abc"))
  (list (ext:convert-string-to-bytes z charset:ascii :start 0 :end 2)
        (ext:convert-string-to-bytes z charset:ascii :start 0 :end 3)
        (ext:convert-string-to-bytes z charset:ascii :start 1 :end 3)
        (ext:convert-string-to-bytes z charset:ascii :start 1 :end 2)))
(#(97 98) #(97 98 99) #(98 99) #(98))

;;; this would require much more work to unclear end
;;(ext:convert-string-to-bytes (string #\newline) :dos)  #(13 10)
;;(ext:convert-string-to-bytes (string #\newline) :unix) #(10)
;;(ext:convert-string-to-bytes (string #\newline) :mac)  #(13)
;;(coerce (ext:convert-string-from-bytes #(13 10) :dos) 'list) (#\Newline)
;;(coerce (ext:convert-string-from-bytes #(10) :unix) 'list) (#\Newline)
;;(coerce (ext:convert-string-from-bytes #(13) :mac) 'list) (#\Newline)

(let ((vec (make-array 1000 :adjustable t :fill-pointer 0
                            :element-type '(unsigned-byte 8))))
  (map-into vec 'identity (ext:convert-string-to-bytes "Hello" charset:utf-8))
  ;; #(72 101 108 108 111)
  (ext:convert-string-from-bytes vec charset:utf-8))
"Hello"

;; Fill the cache, but cache only the results with small lists of intervals.
;; Some iconv based encodings have large lists of intervals (up to 5844
;; intervals for ISO-2022-JP-2) which are rarely used and not worth caching.
(let ((list ()))
  (do-external-symbols (sym (find-package "CHARSET"))
    (push (list sym (ash (length (sys::get-charset-range
                                  (encoding-charset (symbol-value sym))))
                         -1))
          list))
  (setq list (sort list #'< :key #'second))
  (format t "~& ~:D encoding~:P:~%~:{~25@A: ~5:D~%~}" (length list) list))
NIL
