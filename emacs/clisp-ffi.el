;;; syntax highlighting for CLISP FFI forms            -*- Emacs-Lisp -*-
;;; Load this file from ~/.emacs or ~/.emacs.el

(defconst clisp-ffi-font-lock-keywords
  '("(\\(def-\\(\\(call\\(\\s_\\|\\sw\\)*\\)\\|\\(c-var\\)\\|\\(c-enum\\|c-struct\\|c-type\\)\\)\\)\\s *\\(\\(\\s_\\|\\sw\\)*\\)"
    (1 font-lock-keyword-face)
    (7 (cond ((match-beginning 3) font-lock-function-name-face)
             ((match-beginning 5) font-lock-variable-name-face)
             (t font-lock-type-face)) nil t))
  "Additional lisp-mode keywords for CLISP FFI forms.")

(setq lisp-font-lock-keywords
      (cons clisp-ffi-font-lock-keywords lisp-font-lock-keywords-2))
