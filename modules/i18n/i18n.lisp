;; Interface to GNU gettext
;;
;; (gettext msgid [domain [category]]) returns the translation of
;; msgid in the given domain, depending on the given category.
;;
;; (ngettext msgid msgid_plural n [domain [category]]) returns the plural
;; form of the translation for of msgid and n in the given domain, depending
;; on the given category.
;;
;; The possible categories are :LC_MESSAGES, :LC_CTYPE, :LC_TIME, :LC_COLLATE,
;; :LC_MONETARY. The default category is :LC_MESSAGES.
;;
;; (textdomain) returns the current default domain.
;;
;; (textdomaindir domain) returns the message catalog directory
;; for the given domain.

(eval-when (compile load eval)
  (setf (package-lock custom:*system-package-list*) nil))

(export
 '(i18n::gettext i18n::ngettext i18n::textdomain i18n::textdomaindir
   i18n::set-locale)
 "I18N")
(ext:re-export "I18N" "EXT")

(defsetf i18n::textdomain i18n::set-textdomain)
(defsetf i18n::textdomaindir i18n::set-textdomaindir)

(pushnew "I18N" custom:*system-package-list* :test #'string=)
(setf (package-lock custom:*system-package-list*) t)
