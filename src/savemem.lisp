;;;; Saving memory images

(in-package "LISP")
(export '(saveinitmem))
(in-package "SYSTEM")

;;---------------------------------------------------------------------------
;; Stores the current memory contents as "lispimag.mem", omitting garbage
;; collectible objects.
;; This function does not take arguments and has no local variables, there
;; otherwise in the interpreted mode the values of variables were stored.
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

;; Saves the current memory contents.
;; This function works only when compiled!
(defun saveinitmem (&optional (filename "lispinit.mem")
                    &key ((:quiet *quiet*) nil) init-function)
  (let ((- nil) (+ nil) (++ nil) (+++ nil)
        (* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil)
        (*command-index* 0)
        (*home-package* nil))
    (declare (special *command-index* *home-package*))
    (if init-function
      (let* ((old-driver *driver*)
             (*driver* #'(lambda ()
                           (setq *driver* old-driver)
                           (funcall init-function)
                           (funcall *driver*))))
        (savemem filename)
      )
      (savemem filename)
    )
    (room nil)
) )
