;;; French translations of DEFINTERNATIONALed values.
;;; Bruno Haible, Jörg Höhle

(in-package "I18N")
(common-lisp:export 'FRANCAIS)
(common-lisp:import 'FRANCAIS "EXT")
(common-lisp:export 'FRANCAIS "EXT")

(common-lisp:in-package "SYSTEM")

(deflanguage FRANCAIS)

(deflocalized date-format FRANCAIS
  (formatter "~1{~3@*~D/~4@*~D/~5@*~D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}")
)
(deflocalized room-format FRANCAIS
  (list (formatter "Classe~VT instances  taille (octets)  t. moyenne~%")
        (formatter "------~VT ---------  ---------------  ----------~%")
        (formatter       "~VT~8D     ~9D  ~13,3F~%")
) )
(deflocalized space-format FRANCAIS
  (list (formatter       "~VT     permanent            temporaire~%")
        (formatter "Classe~VTinstances   octets   instances   octets~%")
        (formatter "------~VT--------- ---------  --------- ---------~%")
        (formatter       "~VT~9D ~9D  ~9D ~9D~%")
) )
(deflocalized y-or-n FRANCAIS '((#\N) . (#\O #\Y)))
(deflocalized yes-or-no FRANCAIS '(("non") . ("oui")))
(deflocalized print-condition-format FRANCAIS
  (formatter "Condition exceptionnelle de type ~S.")
)

