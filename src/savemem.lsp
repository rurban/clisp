;;;; Saving memory images

(in-package "LISP")
(export '(saveinitmem))
(in-package "SYSTEM")

;;-----------------------------------------------------------------------------

; Speichert den momentanen Speicherinhalt unter Weglassen überflüssiger
; Objekte ab als LISPIMAG.MEM.
; Diese Funktion bekommt keine Argumente und hat keine lokalen Variablen, da
; sonst in interpretiertem Zustand die Variablenwerte mit abgespeichert würden.
(defun %saveinitmem ()
  (do-all-symbols (sym) (remprop sym 'sys::definition))
  (when (fboundp 'clos::install-dispatch)
    (do-all-symbols (sym)
      (when (and (fboundp sym) (clos::generic-function-p (symbol-function sym)))
        (let ((gf (symbol-function sym)))
          (when (clos::gf-never-called-p gf)
            (clos::install-dispatch gf)
  ) ) ) ) )
  (setq - nil + nil ++ nil +++ nil * nil ** nil *** nil / nil // nil /// nil)
  (savemem "lispimag.mem")
  (room nil)
)

; Speichert den momentanen Speicherinhalt ab.
; Läuft nur in compiliertem Zustand!
(defun saveinitmem (&optional (filename "lispinit.mem")
                    &key ((:quiet *quiet*) nil) init-function)
  (setq - nil + nil ++ nil +++ nil * nil ** nil *** nil / nil // nil /// nil)
  (if init-function
    (let* ((old-driver *driver*)
           (*driver* #'(lambda ()
                         (setq *driver* old-driver)
                         (funcall init-function)
                         (funcall *driver*)
          ))           )
      (savemem filename)
    )
    (savemem filename)
  )
  (room nil)
)

