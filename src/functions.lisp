;;; Utilities for function objects
;;; Sam Steingold 2001
;;; Bruno Haible 2004

(in-package "SYSTEM")

;; The signature of a function object.
(defstruct (signature (:type vector) (:conc-name sig-))
  ;; (name nil     :type (or symbol cons))
  (req-num 0    :type fixnum)
  (opt-num 0    :type fixnum)
  (rest-p nil   :type boolean)
  (keys-p nil   :type boolean)
  (keywords nil :type list)
  (allow-p nil  :type boolean))

;; Check an argument that should be a function name, giving the user the
;; opportunity to correct it if it is not.
(defun check-function-name (caller funname)
  (do () ((function-name-p funname) funname)
    (setq funname
          (check-value nil
            (coerce-to-condition (TEXT "~s: ~s is not a function name")
                                 (list caller funname) 'check-function-name
                                 'simple-source-program-error)))))
