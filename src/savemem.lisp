;;;; Saving memory images

(in-package "LISP")
(export '(saveinitmem))
(in-package "SYSTEM")

;;---------------------------------------------------------------------------
;; Stores the current memory contents as "lispimag.mem", omitting garbage
;; collectible objects.
;; This function does not take arguments and has no local variables, since
;; otherwise in the interpreted mode the values of the variables are stored.
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
  (let* ((old-driver *driver*)
         (*driver*
           #'(lambda ()
               (declare (special *command-index* *home-package*
                                 *active-restarts* *condition-restarts*
               )        )
               ;; Reset a few special variables. This must happen in the
               ;; fresh session, not in the old session, because that would
               ;; temporarily disable error handling in the old session.
               ;; Note: For GC purposes, neither is better: during savemem's
               ;; GC the old values are accessible anyway and thus not garbage
               ;; collected.
               (setq - nil
                     + nil
                     ++ nil
                     +++ nil
                     * nil
                     ** nil
                     *** nil
                     / nil
                     // nil
                     /// nil
                     *active-restarts* nil
                     *condition-restarts* nil
                     *command-index* 0
                     *home-package* nil
               )
               (setq *driver* old-driver)
               (when init-function (funcall init-function))
               (funcall *driver*)
             )
        ))
    (savemem filename)
  )
  (room nil)
)
