;;; MAKE-LOAD-FORM for CLISP
;;; Sam Steingold 2001-07-24

;; this could have been placed in in clos.lisp,
;; but `make-init-form' uses conditions

(in-package "CLOS")

(defun make-load-form-saving-slots
    (object &key environment
     (slot-names (mapcan (lambda (slot)
                           (when (eq :instance
                                     (slotdef-allocation slot))
                             (list (slotdef-name slot))))
                         (class-slots (class-of object)))))
  (declare (ignore environment))
  (values `(allocate-instance (find-class ',(class-name (class-of object))))
          `(progn
            (setf ,@(mapcan (lambda (slot)
                              (when (slot-boundp object slot)
                                `((slot-value ,object ',slot)
                                  ,(slot-value object slot))))
                            slot-names))
            (initialize-instance ,object))))

(defgeneric make-load-form (object &optional environment)
  (:method ((object standard-object) &optional environment)
    (make-load-form-saving-slots object :environment environment)))

(defun make-init-form (object)
  (when compiler::*load-forms*
    (multiple-value-bind (form found-p)
        (gethash object compiler::*load-forms*)
      (if found-p form
          (setf (gethash object compiler::*load-forms*)
                (ignore-errors
                  (multiple-value-bind (cre-form ini-form)
                      (make-load-form object)
                    (list 'funcall
                          (compile nil
                                   (if ini-form
                                       (let ((var (gensym)))
                                         `(lambda ()
                                           (let ((,var ,cre-form))
                                             ,(nsubst var object ini-form)
                                             ,var)))
                                       `(lambda () ,cre-form)))))))))))
