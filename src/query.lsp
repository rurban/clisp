;;;; Querying the user

(in-package "SYSTEM")

; -----------------------------------------------------------------------------

; (Y-OR-N-P [format-string {arg}*]), CLTL S. 407
(defun y-or-n-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    (write-string (DEUTSCH " (j/n) "
                   ENGLISH " (y/n) "
                   FRANCAIS " (o/n) ")
                  *query-io*
  ) )
  (let ((localinfo (localized 'y-or-n)))
    (loop
      (let ((line (string-left-trim " " (read-line *query-io*))))
        (when (plusp (length line))
          (let ((first-char (char-upcase (char line 0))))
            (when (member first-char (car localinfo)) (return nil))
            (when (member first-char (cdr localinfo)) (return t))
      ) ) )
      (terpri *query-io*)
      (write-string (DEUTSCH "Bitte mit j oder n antworten: "
                     ENGLISH "Please answer with y or n : "
                     FRANCAIS "Répondez par o ou n : ")
                    *query-io*
) ) ) )
(definternational y-or-n (t ENGLISH))
(deflocalized y-or-n ENGLISH '((#\N) . (#\Y)))
(deflocalized y-or-n DEUTSCH '((#\N) . (#\J #\Y)))
(deflocalized y-or-n FRANCAIS '((#\N) . (#\O #\Y)))

; (YES-OR-NO-P [format-string {arg}*]), CLTL S. 408
(defun yes-or-no-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    (write-string (DEUTSCH " (ja/nein) "
                   ENGLISH " (yes/no) "
                   FRANCAIS " (oui/non) ")
                  *query-io*
  ) )
  (let ((localinfo (localized 'yes-or-no)))
    (loop
      (clear-input *query-io*)
      (let ((line (string-trim " " (read-line *query-io*))))
        (when (member line (car localinfo) :test #'string-equal) (return nil))
        (when (member line (cdr localinfo) :test #'string-equal) (return t))
      )
      (terpri *query-io*)
      (write-string (DEUTSCH "Bitte mit ja oder nein antworten: "
                     ENGLISH "Please answer with yes or no : "
                     FRANCAIS "Répondez par oui ou non : ")
                    *query-io*
) ) ) )
(definternational yes-or-no (t ENGLISH))
(deflocalized yes-or-no ENGLISH '(("no") . ("yes" "yup")))
(deflocalized yes-or-no DEUTSCH '(("nein" "nee" "nö") . ("ja")))
(deflocalized yes-or-no FRANCAIS '(("non") . ("oui")))

