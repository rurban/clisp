;;; syntax highlighting for CLISP FFI forms
;;; Load this file from ~/.emacs or ~/.emacs.el

(font-lock-add-keywords
 'lisp-mode
 '(("(\\(def-\\(\\(call\\(\\s_\\|\\sw\\)*\\)\\|\\(c-var\\)\\|\\(c-enum\\|c-struct\\|c-type\\)\\)\\)\\s *\\(\\(\\s_\\|\\sw\\)*\\)"
    (1 font-lock-keyword-face)
    (7 (cond ((match-beginning 3) font-lock-function-name-face)
             ((match-beginning 5) font-lock-variable-name-face)
             (t font-lock-type-face)) nil t))))
