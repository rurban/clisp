#0Y UTF-8 ;;;  This file is Unicode/UTF-8 encoded.  -*- coding: utf-8 -*-

;;; ENGLISH: Site specific definitions, to be modified on installation
;;; DEUTSCH: Funktionen, die beim Transportieren zu ändern sind
;;; FRANCAIS: Fonctions dépendantes de l'installation

(in-package "EXT")
(mapcar #'fmakunbound '(short-site-name long-site-name))

(defun short-site-name ()
  (let ((s (or
             (system::registry "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"
                               "RegisteredOrganization"
             )
             (system::registry "SOFTWARE\\Microsoft\\Windows\\CurrentVersion"
                               "RegisteredOrganization"
             )
       ))  )
    (check-type s string)
    s
) )
(defun long-site-name ()
  (let ((s (or
             (system::registry "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"
                               "RegisteredOwner"
             )
             (system::registry "SOFTWARE\\Microsoft\\Windows\\CurrentVersion"
                               "RegisteredOwner"
             )
       ))  )
    (check-type s string)
    s
) )

;; ENGLISH: The name of the editor:
;; DEUTSCH: Der Name des Editors:
;; FRANCAIS: Nom de l'éditeur :
(defparameter *editor* "notepad.exe")
(defun editor-name () (or (sys::getenv "EDITOR") *editor*))

;; ENGLISH: The temporary file LISP creates for editing:
;; DEUTSCH: Das temporäre File, das LISP beim Editieren anlegt:
;; FRANCAIS: Fichier temporaire créé par LISP pour l'édition :
(defun editor-tempfile ()
  "lisptemp.lisp"
)

;; ENGLISH: (edit-file file) edits a file.
;; DEUTSCH: (edit-file file) editiert eine Datei.
;; FRANCAIS: (edit-file file) permet l'édition d'un fichier.
(defun edit-file (file)
  (execute (editor-name) (namestring file t))
)

;; ENGLISH: Treat Ctrl-Z in files as whitespace. Some losing middle-age
;;          editors insist on appending this to files.
;; DEUTSCH: Behandle Ctrl-Z in Dateien als Leerstelle. Einige dumme
;;          Steinzeit-Editoren bestehen darauf, das an Dateien anzuhängen.
;; FRANCAIS: Traite Ctrl-Z dans les fichiers comme un espace. Quelques
;;           éditeurs du moyen age n'arrêtent pas d'ajouter cela aux fichiers.
(eval-when (load eval compile)
  (set-syntax-from-char #\Code26 #\Space)
)

;; ENGLISH: The list of directories where programs are searched on LOAD etc.
;;          if device and directory are unspecified:
;; DEUTSCH: Die Liste von Directories, in denen Programme bei LOAD etc. gesucht
;;          werden, wenn dabei Laufwerk und Directory nicht angegeben ist:
;; FRANCAIS: Liste de répertoires où chercher un fichier lorsqu'un répertoire
;;           particulier n'est pas indiqué :
(defparameter *load-paths*
  '(#"C:"               ; erst im Current-Directory von Laufwerk C:
    #"C:\\CLISP\\...\\" ; dann in allen Directories unterhalb C:\CLISP
   )
)

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
  (or (sys::getenv "CLHSROOT")
      (let ((s (system::registry "SOFTWARE\\GNU\\CLISP" "CLHSROOT")))
        (check-type s (or null string))
        s)
      *clhs-root-default*))
(setq *clhs-root-default* "http://www.lisp.org/HyperSpec/")
