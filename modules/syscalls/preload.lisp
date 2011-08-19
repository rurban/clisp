(make-package "POSIX" :nicknames '("OS"))
(setf (package-lock "SYSTEM") nil) ; for strerror & format-message
;; *SYSTEM-PACKAGE-LIST* is locked in SAVEMEM; restore in posix.lisp
(delete "SYSTEM" custom:*system-package-list* :test #'string=)
