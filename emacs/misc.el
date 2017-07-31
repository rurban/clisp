;; miscellaneous settings for clisp development in emacs

(eval-when-compile
  (defvar vc-dir-backend)
  (defvar change-log-default-name))

(defun clisp-repo-p (dir)
  (let ((root
         (with-temp-buffer
           (and (boundp 'vc-hg-program)
                (zerop (call-process vc-hg-program nil t nil "paths" "default"))
                (buffer-string)))))
    (and root (string-match "/clisp\n$" root))))

(eval-after-load "grep"         ; for rgrep
  '(progn
    (add-to-list 'grep-files-aliases
     '("clisp" . "*.[cdfh] *.lisp *.tst *.m4 *.in Makefile* *.xml"))
    (add-to-list 'grep-find-ignored-directories
     '(clisp-repo-p . "build*"))))

(defun clisp-set-change-log-default-name ()
  "Set `change-log-default-name' appropriately."
  (when (eq (vc-backend buffer-file-name) 'Hg)
    (let ((dir (file-name-directory buffer-file-name)))
      (when (clisp-repo-p dir)
        (let ((cl (expand-file-name "ChangeLog" dir)))
          (set (make-local-variable 'change-log-default-name)
               (if (file-exists-p cl) cl
                   (expand-file-name
                    "src/ChangeLog" (locate-dominating-file
                                     buffer-file-name "ANNOUNCE")))))))))

;; append: must come after vc-find-file-hook
(add-hook 'find-file-hooks 'clisp-set-change-log-default-name t)

(defun clisp-set-compile-command ()
  "Set `compile-command' for CLISP lisp and tst files."
  (when (eq (vc-backend buffer-file-name) 'Hg)
    (let ((dir (file-name-directory buffer-file-name)))
      (when (clisp-repo-p dir)
        (set (make-variable-buffer-local 'compile-command)
             (if (string= "tst" (file-name-extension (buffer-file-name)))
                 (concat "cd ../build && "
                         (if (file-executable-p "../build/lisp.exe")
                             "../build/lisp.exe"
                           "../build/lisp.run")
                         " -q -norc -M lispinit.mem -i tests/tests"
                         " -x '(run-test \"tests/" (file-name-base (buffer-file-name))
                         "\")'")
                 "make -C ../build lispinit.mem"))))))

(add-hook 'lisp-mode-hook 'clisp-set-compile-command)

(defun clisp-set-change-log-vc-dir ()
  (when (and (eq vc-dir-backend 'Hg)
             (clisp-repo-p default-directory))
    (set (make-local-variable 'change-log-default-name)
         (expand-file-name "src/ChangeLog" default-directory))))

(add-hook 'vc-dir-mode-hook 'clisp-set-change-log-vc-dir)

(defun clisp-bug-reference-url-format () ; OLD
  (concat "http://sourceforge.net/tracker/index.php?func=detail&aid="
          (match-string-no-properties 2)
          "&group_id=1355&atid="
          (let ((kind (match-string-no-properties 1)))
            (cond ((string-match "[Bb]ug" kind) "101355")
                  ((string-match "[Pp]atch" kind) "301355")
                  ((string-match "RFE" kind) "351355")
                  (t (error "unknown bug kind [%s]" kind))))))

(put 'clisp-bug-reference-url-format 'bug-reference-url-format t)

(defun clisp-bug-reference-url-format () ; NEW
  (let ((kind (match-string-no-properties 1))
        (bug (match-string-no-properties 2)))
    (format "http://sourceforge.net/p/clisp/%s/%s/"
            (cond ((string-match "[Bb]ug" kind) "bugs")
                  ((string-match "[Pp]atch" kind) "patches")
                  ((string-match "RFE" kind) "feature-requests")
                  (t (error "unknown bug kind [%s]" kind)))
            bug)))

;; see url.el:url-retrieve-synchronously
(defun clisp-sf-transition-url (url)
  ;; find the new URL
  (let* ((retrieval-done nil)
         (new-url nil)
         (asynch-buffer (url-retrieve url (lambda (status)
                                            (setq new-url (plist-get status :redirect)
                                                  retrieval-done t))
                                      nil t t)))
    (while (not retrieval-done)
      (sleep-for 0.1))
    (kill-buffer asynch-buffer)
    (or new-url (error "%s is not redirected" url))))

(defun clisp-sf-convert-url ()
  ;; convert URL at point
  (interactive)
  (let* ((bounds (or (bounds-of-thing-at-point 'url)
                     (progn (re-search-forward "https?://" (line-end-position) t)
                            (bounds-of-thing-at-point 'url))
                     (error "No URL at point!")))
         (url (buffer-substring-no-properties
               (car bounds) (cdr bounds))))
    (kill-region (car bounds) (cdr bounds))
    (insert (clisp-sf-transition-url url))))


(defun clisp-sf-convert-bug-num ()
  ;; convert bug number at point
  (interactive)
  (let* ((bounds (or (bounds-of-thing-at-point 'word)
                     (error "No number at point!")))
         (bug (buffer-substring-no-properties
               (car bounds) (cdr bounds))))
    (kill-region (car bounds) (cdr bounds))
    (insert (clisp-sf-transition-url
             (concat "http://sourceforge.net/tracker/index.php?func=detail&aid="
                     bug "&group_id=1355&atid=101355")))))

(defun clisp-sf-convert-all-references ()
  ;; find and convert reference till EOF
  (interactive)
  (let ((count 0))
    (while (re-search-forward "\\([a-zA-Z]+\\)#\\([0-9]+\\)" nil t)
      (let* ((beg (match-beginning 2)) (end (match-end 2))
             (url (clisp-bug-reference-url-format))
             (new (clisp-sf-transition-url url)))
        (when (string= url new)
          (error "Same URL! %s" url))
        (incf count)
        (message "%d %s --> %s" count url new)
        (goto-char beg)
        (kill-region beg end)
        (insert (car (last (split-string new "/" t))))))
    (message "Converted %d references" count)))

(defun clisp-goto-gmane-url ()
  "Visit the article using Gnus - from Lars."
  (interactive)
  (when (re-search-forward "\\(gmane[^/\n ]+\\)[/:]\\([0-9]+\\)" nil t)
    (let ((group (match-string 1))
	  (article (match-string 2)))
      (message "group=[%s] article=[%s]" group article)
      (gnus-fetch-group
       group ; (format "nntp+news.gmane.org:%s" group)
       (and article
	    (list (string-to-number article)))))))

(defun clisp-get-gmane-article ()
  "Low level: get the article using raw NNTP."
  (interactive)
  (when (re-search-forward "\\(gmane[^/\n ]+\\)[/:]\\([0-9]+\\)" nil t)
    (let* ((group (match-string 1))
           (article (match-string 2))
           (buffer (concat "*" group "-" article "*"))
           (proc (open-network-stream "nntp" buffer "news.gmane.org" 119)))
      (message "group=[%s] article=[%s]" group article)
      (set-process-coding-system proc 'utf-8-dos 'utf-8-dos)
      (process-send-string proc (concat "GROUP " group "\nARTICLE " article "\n"))
      (display-buffer buffer))))

(defun clisp-google-message ()
  "Search for the message in the current buffer
 - created by `clisp-get-gmane-article'."
  (interactive)
  (goto-char (point-min))
  (cl-flet ((get-field (f)
              (save-excursion
                (and (goto-char (point-min))
                     (re-search-forward (concat "^" f ":") nil t)
                     (cl-substitute
                      ?+ ?\s (buffer-substring-no-properties
                              (point) (line-end-position)))))))
    (if (re-search-forward "^Newsgroups: gmane.lisp.clisp" nil t)
        (browse-url (concat "https://sourceforge.net/p/clisp/mailman/search/?q="
                            (get-field "Subject")))
      (browse-url (concat "https://google.com/?q="
                          (get-field "Subject")
                          (or (get-field "List-Id")
                              (get-field "Sender")
                              ""))))))

(defun clisp-check-all-links ()
  "Check all links in the buffer.
Does _NOT_ check anchors!"
  (interactive)
  ;; don't use `save-excursion' because we want to stop at the offending URL
  (let ((start (point)) url (checked ()))
    (while (re-search-forward "https?://" nil t)
      (message "found <%s>" (setq url (thing-at-point 'url)))
      (setq url (first (split-string url "#")))
      (unless (member url checked)
        (push url checked)
        (unless (file-readable-p url)
          (error "%s does not exist" url))))
    (goto-char start)
    (message "Checked %d URLs: %s" (length checked) checked)))


(autoload 'rng-dtd-trivial-p "rng-valid")
(autoload 'nxml-parent-document-set "nxml-mode")
(defun clisp-nxml-mode-hook ()
  "Set `nxml-mode-hook' for clisp impnotes."
  (when (and (null nxml-parent-document)
             (rng-dtd-trivial-p rng-dtd)
             (eq (vc-backend buffer-file-name) 'Hg))
    (let ((dir (file-name-directory buffer-file-name)) parent)
      (when (clisp-repo-p dir)
        (cond ((file-exists-p
                (setq parent (expand-file-name "impnotes.xml.in" dir)))
               (unless (string= parent buffer-file-name)
                 (nxml-parent-document-set parent)))
              ((file-exists-p
                (setq parent (expand-file-name "doc/impnotes.xml.in"
                                               (locate-dominating-file
                                                dir "ANNOUNCE"))))
               (nxml-parent-document-set parent))
              (t (message "Cannot find parent document")))))))
(add-hook 'nxml-mode-hook 'clisp-nxml-mode-hook)
