;; miscellaneous settings for clisp development in emacs

(defun clisp-repo-p (dir)
  (let ((host (vc-cvs-repository-hostname dir)))
    (and host (string-match "^clisp\\." host))))

(eval-after-load "grep"         ; for rgrep
  '(progn
    (add-to-list 'grep-files-aliases
     '("clisp" . "*.[cdfh] *.lisp *.tst *.m4 *.in Makefile* *.xml"))
    (add-to-list 'grep-find-ignored-directories
     '(clisp-repo-p . "build*"))))

(defun clisp-set-change-log-default-name ()
  "Set `change-log-default-name' appropriately."
  (when (eq (vc-backend buffer-file-name) 'CVS)
    (let ((dir (file-name-directory buffer-file-name)))
      (when (clisp-repo-p dir)
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

(defun clisp-set-change-log-vc-dir ()
  (when (and (eq vc-dir-backend 'CVS)
             (clisp-repo-p default-directory))
    (set (make-local-variable 'change-log-default-name)
         (expand-file-name "src/ChangeLog" default-directory))))

(add-hook 'vc-dir-mode-hook 'clisp-set-change-log-vc-dir)
