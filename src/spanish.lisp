;;; Spanish translations of DEFINTERNATIONALed values.
;;; Bruno Haible, Carlos Linares

(in-package "LISP")

(export 'ESPAÑOL)

(deflanguage ESPAÑOL)

(in-package "SYSTEM")

(deflocalized y-or-n ESPAÑOL '((#\N) . (#\S #\Y)))
(deflocalized yes-or-no ESPAÑOL '(("no") . ("si")))

