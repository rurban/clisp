;;; -*- Emacs-Lisp -*-
;;;
;;; indentation style for CLISP sources
;;; byte-compile this file and load it from ~/.emacs.el

;;; general
(setq-default indent-tabs-mode nil)

;;; Common Lisp
(autoload 'common-lisp-indent-function "cl-indent" "Common Lisp indent.")
(setq lisp-indent-function 'common-lisp-indent-function)

(put 'defclass 'common-lisp-indent-function
     '((&whole 4 &rest (&whole 2 &rest 1)) &rest (&whole 2 &rest 1)))
(put 'defconst 'common-lisp-indent-function '(4 2 2 2))
(put 'defgeneric 'common-lisp-indent-function
     (get 'defun 'common-lisp-indent-function))
(put 'define-condition 'common-lisp-indent-function
     '((1 6) (2 6 ((&whole 1))) (3 4 ((&whole 1))) (4 &body)))
(put 'define-condition 'common-lisp-indent-function '(6 4 &body))
(put 'defmethod 'common-lisp-indent-function '(4 4 (&whole 4 &rest 1) &body))
;; (put 'defmethod 'common-lisp-indent-function 'defun)
(put 'generic-flet 'common-lisp-indent-function
     (get 'flet 'common-lisp-indent-function))
(put 'generic-labels 'common-lisp-indent-function
     (get 'labels 'common-lisp-indent-function))
(put 'handler-bind 'common-lisp-indent-function '((&whole 4 &rest 1) 2 &body))
(put 'handler-case 'common-lisp-indent-function
     '((1 4) (&whole 2 ((0 1) (1 3) (2 &body)))))
;; this is against all the code I have ever seen except for the CLISP sources
;; (put 'if 'common-lisp-indent-function 1)
(put 'pprint-logical-block 'common-lisp-indent-function '(4 2))
(put 'restart-bind 'common-lisp-indent-function
     '(((&whole 2 (0 1) (&whole 1))) (2 &body)))
(put 'restart-case 'common-lisp-indent-function
     '((1 4) (&whole 2 ((0 1) (&whole 1)))))
(put 'symbol-macrolet 'common-lisp-indent-function
     (get 'multiple-value-bind 'common-lisp-indent-function))
(put 'with-accessors 'common-lisp-indent-function
     (get 'multiple-value-bind 'common-lisp-indent-function))
(put 'with-added-methods 'common-lisp-indent-function
     '((1 4 ((&whole 1))) (2 &body)))
(put 'with-condition-restarts 'common-lisp-indent-function
     '((1 4 ((&whole 1))) (2 &body)))
(put 'with-output-to-string 'common-lisp-indent-function '(4 2))
(put 'with-simple-restart 'common-lisp-indent-function
     '((1 4 ((&whole 1))) (2 &body)))
(put 'with-slots 'common-lisp-indent-function
     (get 'multiple-value-bind 'common-lisp-indent-function))

;;; D

(setq auto-mode-alist (cons '("\\.d\\'" . d-mode) auto-mode-alist))

(define-derived-mode d-mode c-mode "D"
  "Major mode for editing CLISP source code.
Special commands:
\\{d-mode-map}
Turning on D mode calls the value of the variable `d-mode-hook',
if that value is non-nil.
If you are using Emacs 20.2 or earlier (including XEmacs) and want to
use fontifications, you have to (require 'font-lock) first.  Sorry.")
(when window-system
  (if (boundp 'running-xemacs)
      (put 'd-mode 'font-lock-defaults (get 'c-mode 'font-lock-defaults))
      (unless (assq 'd-mode font-lock-defaults-alist)
        (setq font-lock-defaults-alist
              (cons (cons 'd-mode
                          (cdr (assq 'c-mode font-lock-defaults-alist)))
                    font-lock-defaults-alist)))))
(modify-syntax-entry ?# "<" d-mode-syntax-table)
(modify-syntax-entry 10 ">" d-mode-syntax-table)
