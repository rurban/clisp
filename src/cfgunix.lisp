#0Y UTF-8 ;;;  This file is Unicode/UTF-8 encoded.  -*- coding: utf-8 -*-

;;; ENGLISH: Site specific definitions, to be modified on installation
;;; DEUTSCH: Funktionen, die beim Transportieren zu ändern sind
;;; FRANCAIS: Fonctions dépendantes de l'installation

(in-package "EXT")
(mapcar #'fmakunbound '(short-site-name long-site-name))

(defun short-site-name () (or (sys::getenv "ORGANIZATION") "edit config.lisp"))
(defun long-site-name () (or (sys::getenv "ORGANIZATION") "edit config.lisp"))

;; ENGLISH: The name of the editor:
;; DEUTSCH: Der Name des Editors:
;; FRANCAIS: Nom de l'éditeur :
(defparameter *editor* "vi")
(defun editor-name () (or (sys::getenv "EDITOR") *editor*))

;; ENGLISH: (edit-file file) edits a file.
;; DEUTSCH: (edit-file file) editiert eine Datei.
;; FRANCAIS: (edit-file file) permet l'édition d'un fichier.
(defun edit-file (file)
  (open file :direction :probe :if-does-not-exist :create)
  (shell (format nil "~A ~A" (editor-name) (truename file)))
)

;; ENGLISH: The temporary file LISP creates for editing:
;; DEUTSCH: Das temporäre File, das LISP beim Editieren anlegt:
;; FRANCAIS: Fichier temporaire créé par LISP pour l'édition :
(defun editor-tempfile ()
  (merge-pathnames "lisptemp.lisp" (user-homedir-pathname))
)

;; ENGLISH: The list of directories where programs are searched on LOAD etc.:
;; DEUTSCH: Die Liste von Directories, in denen Programme bei LOAD etc. gesucht
;;          werden:
;; FRANCAIS: Liste de répertoires où chercher un fichier programme:
(defparameter *load-paths*
  '(#"./"           ; in the current directory
    "~/lisp/**/"    ; in all directories below $HOME/lisp
)  )

;; ENGLISH: This makes screen output prettier:
;; DEUTSCH: Dadurch sehen Bildschirmausgaben besser aus:
;; FRANCAIS: Pour que les sorties sur l'écran soient plus lisibles:
(setq *print-pretty* t)

;; ENGLISH: Common Lisp HyperSpec access
(defvar *clhs-root-default*)
(defun clhs-root ()
  "This returns the root URL for the Common Lisp HyperSpec.
You can set the environment variable `CLHSROOT' or redefine this function
in ~/.clisprc.  On win32 you can also use the Registry."
  (or (sys::getenv "CLHSROOT") *clhs-root-default*))
