;;; Parsing ordinary lambda lists
;;; Bruno Haible 1988-2004
;;; Sam Steingold 1999-2001

(in-package "SYSTEM")

;;; Analyzes a lambda-list of a function (CLtL2 p. 76, ANSI CL 3.4.1.).
;;; Reports errors through errfunc (a function taking an error format string
;;; format string arguments).
;; Returns 13 values:
;; 1. list of required parameters
;; 2. list of optional parameters
;; 3. list of init-forms of the optional parameters
;; 4. list of supplied-vars for the optional parameters (0 for the missing)
;; 5. &rest parameter or 0
;; 6. flag, if keywords are allowed
;; 7. list of keywords
;; 8. list of keyword parameters
;; 9. list of init-forms of the keyword parameters
;; 10. list of supplied-vars for the keyword parameters (0 for the missing)
;; 11. flag, if other keywords are allowed
;; 12. list of &aux variables
;; 13. list of init-forms of the &aux variables
(defun analyze-lambdalist (lambdalist errfunc)
  (let ((L lambdalist) ; rest of the lambda-list
        (req nil)
        (optvar nil)
        (optinit nil)
        (optsvar nil)
        (rest 0)
        (keyflag nil)
        (keyword nil)
        (keyvar nil)
        (keyinit nil)
        (keysvar nil)
        (allow-other-keys nil)
        (auxvar nil)
        (auxinit nil))
    ;; The lists are all accumulated in reversed order.
    (macrolet ((err-misplaced (item)
                 `(funcall errfunc (TEXT "Lambda list marker ~S not allowed here.")
                                   ,item))
               (err-invalid (item)
                 `(funcall errfunc (TEXT "Invalid lambda list element ~S")
                                   ,item))
               (check-item (item permissible)
                 `(if (memq ,item ,permissible)
                    (return)
                    (err-misplaced ,item)))
               (skip-L (items)
                 `(loop
                    (when (atom L) (return))
                    (let ((item (car L)))
                      (if (memq item lambda-list-keywords)
                        (check-item item ,items)
                        (funcall errfunc (TEXT "Lambda list element ~S is superfluous.")
                                         item)))
                    (setq L (cdr L)))))
      ;; Required parameters:
      (loop
        (if (atom L) (return))
        (let ((item (car L)))
          (if (symbolp item)
            (if (memq item lambda-list-keywords)
              (check-item item '(&optional &rest &key &aux))
              (push item req))
            (err-invalid item)))
        (setq L (cdr L)))
      ;; Now (or (atom L) (member (car L) '(&optional &rest &key &aux))).
      ;; Optional parameters:
      (when (and (consp L) (eq (car L) '&optional))
        (setq L (cdr L))
        (macrolet ((note-optional (var init svar)
                     `(progn
                        (push ,var optvar)
                        (push ,init optinit)
                        (push ,svar optsvar))))
          (loop
            (if (atom L) (return))
            (let ((item (car L)))
              (if (symbolp item)
                (if (memq item lambda-list-keywords)
                  (check-item item '(&rest &key &aux))
                  (note-optional item nil 0))
                (if (and (consp item) (symbolp (car item)))
                  (if (null (cdr item))
                    (note-optional (car item) nil 0)
                    (if (consp (cdr item))
                      (if (null (cddr item))
                        (note-optional (car item) (cadr item) 0)
                        (if (and (consp (cddr item)) (symbolp (caddr item))
                                 (null (cdddr item)))
                          (note-optional (car item) (cadr item) (caddr item))
                          (err-invalid item)))
                      (err-invalid item)))
                  (err-invalid item))))
            (setq L (cdr L)))))
      ;; Now (or (atom L) (member (car L) '(&rest &key &aux))).
      ;; &rest parameters:
      (when (and (consp L) (eq (car L) '&rest))
        (setq L (cdr L))
        (macrolet ((err-norest ()
                     `(funcall errfunc (TEXT "Missing &REST parameter in lambda list ~S")
                                       lambdalist)))
          (if (atom L)
            (err-norest)
            (prog ((item (car L)))
              (if (symbolp item)
                (if (memq item lambda-list-keywords)
                  (progn (err-norest) (return))
                  (setq rest item))
                (err-invalid item))
              (setq L (cdr L))))))
      ;; Move forward to the next &KEY or &AUX:
      (skip-L '(&key &aux))
      ;; Now (or (atom L) (member (car L) '(&key &aux))).
      ;; Keyword parameters:
      (when (and (consp L) (eq (car L) '&key))
        (setq L (cdr L))
        (setq keyflag t)
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (memq item lambda-list-keywords)
                (check-item item '(&allow-other-keys &aux))
                (progn
                  (push (intern (symbol-name item) *keyword-package*) keyword)
                  (push item keyvar) (push nil keyinit) (push 0 keysvar)))
              (if (and (consp item)
                       (or (symbolp (car item))
                           (and (consp (car item))
                                (symbolp (caar item))
                                (consp (cdar item))
                                (symbolp (cadar item))
                                (null (cddar item))))
                       (or (null (cdr item))
                           (and (consp (cdr item))
                                (or (null (cddr item))
                                    (and (consp (cddr item))
                                         (symbolp (caddr item))
                                         (null (cdddr item)))))))
                (progn
                  (if (consp (car item))
                    (progn
                      (push (caar item) keyword)
                      (push (cadar item) keyvar))
                    (progn
                      (push (intern (symbol-name (car item)) *keyword-package*)
                            keyword)
                      (push (car item) keyvar)))
                  (if (consp (cdr item))
                    (progn
                      (push (cadr item) keyinit)
                      (if (consp (cddr item))
                        (push (caddr item) keysvar)
                        (push 0 keysvar)))
                    (progn (push nil keyinit) (push 0 keysvar))))
                (err-invalid item))))
          (setq L (cdr L)))
        ;; Now (or (atom L) (member (car L) '(&allow-other-keys &aux))).
        (when (and (consp L) (eq (car L) '&allow-other-keys))
          (setq allow-other-keys t)
          (setq L (cdr L))))
      ;; Move forward  to the next &AUX:
      (skip-L '(&aux))
      ;; Now (or (atom L) (member (car L) '(&aux))).
      ;; &aux variables:
      (when (and (consp L) (eq (car L) '&aux))
        (setq L (cdr L))
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (memq item lambda-list-keywords)
                (err-misplaced item)
                (progn (push item auxvar) (push nil auxinit)))
              (if (and (consp item) (symbolp (car item)))
                (if (null (cdr item))
                  (progn (push (car item) auxvar) (push nil auxinit))
                  (if (and (consp (cdr item)) (null (cddr item)))
                    (progn (push (car item) auxvar) (push (cadr item) auxinit))
                    (err-invalid item)))
                (err-invalid item))))
          (setq L (cdr L))))
      ;; Now (atom L).
      (if L
        (funcall errfunc (TEXT "Lambda lists with dots are only allowed in macros, not here: ~S")
                         lambdalist)))
    (values
      (nreverse req)
      (nreverse optvar) (nreverse optinit) (nreverse optsvar)
      rest
      keyflag
      (nreverse keyword) (nreverse keyvar) (nreverse keyinit) (nreverse keysvar)
      allow-other-keys
      (nreverse auxvar) (nreverse auxinit))))
