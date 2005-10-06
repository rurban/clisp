;; -*- Lisp -*-
;; some tests for I18N
;; clisp -q -norc -i ../tests/tests -x '(run-test "i18n/test")'

(i18n:gettext "foo") "foo"
(i18n:ngettext "abazonk" "abazonk" 12) "abazonk"
(typep (show (i18n:textdomain)) '(or null string)) T
(setf (i18n:textdomain) "zot") "zot"
(typep (show (i18n:textdomaindir "foo")) '(or null pathname)) T
(pathnamep (setf (i18n:textdomaindir "foo") (car (directory "./")))) T

(listp (show (i18n:set-locale) :pretty t)) T

(i18n:locale-conv-p (show (i18n:locale-conv) :pretty t)) T

(listp (show (i18n:language-information) :pretty t)) T
