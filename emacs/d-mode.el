;;; Copyright (C) 2001 by Sam Steingold
;;; released under the GNU GPL <http://www.gnu.org/copyleft/gpl.html>
;;; as a part of CLISP <http://clisp.cons.org>
;;;
;;; the mode for editing CLISP *.d files
;;; add the following to your ~/.emacs.el
;;; (setq auto-mode-alist (cons '("\\.d\\'" . d-mode) auto-mode-alist))
;;; (autoload 'd-mode "/usr/local/src/clisp/emacs/d-mode")

(require 'cc-mode)
(require 'cl)                   ; `subst'

(defun d-mode-beg-of-defun ()
  "A valid value for `beginning-of-defun-function' for `d-mode'."
  (re-search-backward
   (eval-when-compile
    (concat "^" (regexp-opt '("LISPFUN" "local " "global "
                              "nonreturning_function") t)))
   nil 1))                      ; move to the limit

(defun d-mode-convert-function ()
  "Convert from the old-style to ANSI function definition.
The point should be on the prototype and the definition should follow."
  (interactive "")
  (let ((beg (point)))
    (end-of-line 1)
    (delete-region (1- (point)) (- (search-forward "{" nil nil) 2))
    (c-indent-region beg (progn (backward-char 2) (forward-sexp) (point)))))

(defun d-mode-convert-lispfun ()
  "Convert the LISPFUN to the new indentation."
  (interactive "")
  (search-forward "LISPFUN")
  (beginning-of-line)
  (let ((beg (point)))
    (forward-sexp 2) (insert " {")
    (search-forward "{") (delete-region (line-beginning-position) (1+ (point)))
    (goto-char beg)
    (c-indent-region beg (progn (forward-sexp 3) (point)))))

(defun d-mode-indent-sharp (s-element)
  "Check whether a macro or a comment and indent accordingly."
  (save-excursion (back-to-indentation) (if (looking-at "# ") 0 [0])))

(defvar d-font-lock-extra-types
  '(nconc (list "bool" "object" "chart" "[otac]int" "signean"
           "[su]?int[BCL0-9]*" "Values" "fsubr_function" "lisp_function")
    c-font-lock-extra-types)
  "Extra types to be fontified as such.")

(defun d-mode-modify-font-lock (form)
  "Modify the font locking spec appropriately."
  (subst d-font-lock-extra-types 'c-font-lock-extra-types
         ;; `d-mode' should highlight #foo not only at the beginning-of-line
         (if (and (consp form) (stringp (car form))
                  (= ?^ (aref (car form) 0))
                  (= ?# (aref (car form) 1)))
             (cons (concat "^[ \t]*" (substring (car form) 1)) (cdr form))
             form)))

(defvar d-extra-keywords
  (eval-when-compile
   (regexp-opt '("var" "local" "global" "true" "false" "NIL" "T" "loop"
                 "inline" "NULL" "popSTACK" "pushSTACK" "skipSTACK"
                 "dotimespC" "dotimesC" "dotimespL" "dotimesL" "dotimespW"
                 "dotimesW" "nonreturning_function" "return_Values")
               'words)))

(defvar d-font-lock-keywords-1
  (mapcar #'d-mode-modify-font-lock c-font-lock-keywords-1))

(defvar d-font-lock-keywords-2
  (cons d-extra-keywords
        (mapcar #'d-mode-modify-font-lock c-font-lock-keywords-2)))

(defvar d-font-lock-keywords-3
  (cons d-extra-keywords
        (mapcar #'d-mode-modify-font-lock c-font-lock-keywords-3)))

(defvar d-font-lock-keywords d-font-lock-keywords-1)

(defun d-mode-add-font-locking (default)
  (cons (list 'd-font-lock-keywords 'd-font-lock-keywords-1
              'd-font-lock-keywords-2 'd-font-lock-keywords-3)
        (cdr default)))

(defvar d-mode-font-lock-defaults
  (d-mode-add-font-locking
   (if sds-xemacs (get 'c-mode 'font-lock-defaults)
       (cdr (assq 'c-mode font-lock-defaults-alist))))
  "The `font-lock-defaults' for `d-mode'.")

(define-derived-mode d-mode c-mode "D"
  "Major mode for editing CLISP source code.
Special commands:
\\{d-mode-map}
Turning on D mode calls the value of the variable `d-mode-hook',
if that value is non-nil.
If you are using Emacs 20.2 or earlier (including XEmacs) and want to
use fontifications, you have to (require 'font-lock) first.  Sorry.
Beware - this will modify the original C-mode too!"
  (set (make-local-variable 'compile-command)
       (let* ((target (if (eq window-system 'w32) "lisp.exe" "lisp.run"))
              (make (if (eq window-system 'w32) "nmake" "make"))
              (makefile
               (cond ((file-readable-p "Makefile") "Makefile")
                     ((file-readable-p "makefile") "makefile")
                     ((file-readable-p "makefile-msvc") "makefile-msvc")
                     ((file-readable-p "makefile.msvc") "makefile.msvc")
                     ((file-readable-p "makefile.msvc5") "makefile.msvc5")
                     ((file-readable-p "Makefile.msvc5") "Makefile.msvc5")
                     ((file-readable-p "makefile-msvs") "makefile-msvs")
                     ((file-readable-p "makefile-gcc")
                      (setq make "make") "makefile-gcc")
                     (t "<makefile>"))))
         (concat make " -f " makefile " " target)))
  (setf (cdr (assq 'cpp-macro c-offsets-alist)) 'd-mode-indent-sharp)
  ;; (setq defun-prompt-regexp
  ;; "^\\(LISPFUNN?(.*) \\|\\(local\\|global\\|nonreturning_function\\) .*\\)")
  (setq beginning-of-defun-function 'd-mode-beg-of-defun)
  (when (<= 21 emacs-major-version)
    (set (make-local-variable 'font-lock-defaults)
         d-mode-font-lock-defaults)))

(when window-system
  ;; enable font locking
  (if sds-xemacs
      (put 'd-mode 'font-lock-defaults d-mode-font-lock-defaults)
      (when (and (> 21 emacs-major-version)
                 (null (assq 'd-mode font-lock-defaults-alist)))
        (setq font-lock-defaults-alist
              (cons (cons 'd-mode d-mode-font-lock-defaults)
                    font-lock-defaults-alist)))))

(setq c-C-specifier-kwds (concat c-C-specifier-kwds "\\|" d-extra-keywords))

;; enable CLISP "# foo" comments
(modify-syntax-entry ?# ". 1b" d-mode-syntax-table)
(modify-syntax-entry 32 "- 2b" d-mode-syntax-table) ; space
(modify-syntax-entry ?\n "> b" d-mode-syntax-table)
(modify-syntax-entry ?\f "> b" d-mode-syntax-table)
(modify-syntax-entry ?/ "_ 14" d-mode-syntax-table)

;; put D buffers along with the C buffers in the menus
(push '("\\<D\\>" . "C") mouse-buffer-menu-mode-groups)

(provide 'd-mode)
