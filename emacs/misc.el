;; miscellaneous settings for clisp development in emacs

(eval-after-load "grep"         ; for rgrep
  '(progn
    (add-to-list 'grep-files-aliases
     '("clisp" . "*.[cdfh] *.lisp *.tst *.m4 *.in Makefile* *.xml"))
    (add-to-list 'grep-find-ignored-directories "build*")))

(defun clisp-set-change-log-default-name ()
  "Set `change-log-default-name' appropriately."
  (when (eq (vc-backend buffer-file-name) 'CVS)
    (let* ((dir (file-name-directory buffer-file-name))
           (host (vc-cvs-repository-hostname dir)))
      (when (and host (string-match "^clisp\\." host))
        (let ((cl (expand-file-name "ChangeLog" dir)))
          (set (make-local-variable 'change-log-default-name)
               (cond ((file-exists-p cl) cl)
                     (t
                      (while (not (or (file-exists-p (expand-file-name
                                                      "ANNOUNCE" dir))
                                      (string= dir "/")))
                        (setq dir (expand-file-name ".." dir)))
                      (expand-file-name "src/ChangeLog" dir)))))))))

;; append: must come after vc-find-file-hook
(add-hook 'find-file-hooks 'clisp-set-change-log-default-name t)
