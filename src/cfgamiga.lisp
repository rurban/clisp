#0Y UTF-8 ;;;  This file is Unicode/UTF-8 encoded.  -*- coding: utf-8 -*-

;;; ENGLISH: Site specific definitions, to be modified on installation
;;; DEUTSCH: Funktionen, die beim Transportieren zu ändern sind
;;; FRANCAIS: Fonctions dépendantes de l'installation

(in-package "LISP")
(export '(clhs-root *clhs-root-default*))
(mapcar #'fmakunbound '(machine-type machine-version machine-instance
                        short-site-name long-site-name editor-tempfile))

(defun machine-type () "Amiga")
(defun machine-version () "Amiga, OS 2.04-3.1")
(defun machine-instance () (or (sys::getenv "HOSTNAME") "edit config.lisp"))

(defun short-site-name () (or (sys::getenv "ORGANIZATION") "edit config.lisp"))

(defun long-site-name () (or (sys::getenv "ORGANIZATION") "edit config.lisp"))

;; ENGLISH: The name of the editor:
;; DEUTSCH: Der Name des Editors:
;; FRANCAIS: Nom de l'éditeur :
(defparameter *editor* "emacs")
(defun editor-name () (or (sys::getenv "EDITOR") *editor*))

;; ENGLISH: The temporary file LISP creates for editing:
;; DEUTSCH: Das temporäre File, das LISP beim Editieren anlegt:
;; FRANCAIS: Fichier temporaire créé par LISP pour l'édition :
(defun editor-tempfile () "T:lisptemp.lisp")

;; ENGLISH: The list of directories where programs are searched on LOAD etc.
;;          if device and directory are unspecified:
;; DEUTSCH: Die Liste von Directories, in denen Programme bei LOAD etc. gesucht
;;          werden, wenn dabei Laufwerk und Directory nicht angegeben ist:
;; FRANCAIS: Liste de répertoires où chercher un fichier lorsqu'un répertoire
;;           particulier n'est pas indiqué :
(defparameter *load-paths*
  '(#"**/"      ; erst in allen Directories unterhalb von hier
    #"LISP:**/" ; dann in allen Directories unterhalb von LISP:
   )
)

;; ENGLISH: This makes screen output both faster and output prettier:
;; DEUTSCH: Dadurch sehen Bildschirmausgaben schneller und besser aus:
;; FRANCAIS: Pour que les sorties sur l'écran soient plus rapides et plus lisibles:
(setq *print-pretty* t)

;; ENGLISH: Also set the variable *default-time-zone* in TIMEZONE.LISP according
;;          to your time zone.
;; DEUTSCH: Setzen Sie auch die Variable *default-time-zone* in TIMEZONE.LISP
;;          auf die bei Ihnen gültige Zeitzone.
;; FRANCAIS: Dans TIMEZONE.LISP, affectez à *default-time-zone* la valeur
;;           correspondante à votre fuseau horaire.
;; (setq *default-time-zone* 0)

;; ENGLISH: Common Lisp HyperSpec access
(defvar *clhs-root-default*)
(defun clhs-root ()
  "This returns the root URL for the Common Lisp HyperSpec.
You can set the environment variable `CLHSROOT' or redefine this function
in ~/.clisprc.  On win32 you can also use the Registry."
  (or (sys::getenv "CLHSROOT") *clhs-root-default*))
(setq *clhs-root-default* "http://www.lisp.org/HyperSpec/")
