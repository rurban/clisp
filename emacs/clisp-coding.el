;;; Automatic choice of encoding for CLISP sources           -*- Emacs-Lisp -*-
;;; Load this file from ~/.emacs or ~/.emacs.el
;;; Tested with Emacs 20 with Mule-UCS, Emacs 21

(defun clisp-find-file-coding-system (arg-list)
  (and (eq (car arg-list) 'insert-file-contents)
       'utf-8))

; All the *.d sources are in UTF-8 encoding.
(modify-coding-system-alist 'file "\\.d\\'" 'clisp-find-file-coding-system)

; All the *.lisp sources are in UTF-8 encoding.
(modify-coding-system-alist 'file "\\.lisp\\'" 'clisp-find-file-coding-system)
