;;; LIBSVM interface
;;; <http://www.csie.ntu.edu.tw/~cjlin/libsvm/>
;;;
;;; Copyright (C) 2006 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(defpackage "LIBSVM"
  (:modern t) (:use "CL" "FFI")
  (:shadowing-import-from "EXPORTING" #:def-c-enum #:def-c-struct
                          #:def-call-out #:def-c-type #:defun))
(in-package "LIBSVM")
(setf (documentation (find-package "LIBSVM") 'sys::impnotes) "libsvm")

(default-foreign-language :stdc)
(defconstant svm-so
  (namestring (merge-pathnames "svm.so" *load-pathname*)))

;;;
;;; types and constants
;;;

(def-c-type node (c-struct list (index int) (value double-float)))

(def-c-type problem (c-struct list
  (l int)                               ; number of records
  (y (c-array-ptr double-float))        ; of length l (targets)
  (x (c-array-ptr (c-array-ptr node))))); of length l (predictors)

(defun problem-l (problem)
  ;; FIXME: horribly inefficient
  (slot (foreign-value problem) 'l))

(def-c-enum svm_type C_SVC NU_SVC ONE_CLASS EPSILON_SVR NU_SVR)

(def-c-enum kernel_type LINEAR POLY RBF SIGMOID PRECOMPUTED)

(def-c-type parameter (c-struct vector
  (svm_type int)
  (kernel_type int)
  (degree int)                  ; for poly
  (gamma double-float)          ; for poly/rbf/sigmoid
  (coef0 double-float)          ; for poly/sigmoid
  ;; these are for training only
  (cache_size double-float)     ; in MB
  (eps double-float)            ; stopping criteria
  (C double-float)              ; for C_SVC, EPSILON_SVR and NU_SVR
  (nr_weight int)               ; for C_SVC
  (weight_label (c-array-ptr int)) ; for C_SVC
  (weight (c-array-ptr double-float)) ; for C_SVC
  (nu double-float)                ; for NU_SVC, ONE_CLASS, and NU_SVR
  (p double-float)                 ; for EPSILON_SVR
  (shrinking int)                  ; use the shrinking heuristics
  (probability int)))              ; do probability estimates

(def-c-type model c-pointer)

;;;
;;; foreign functions and small wrappers
;;;

(ffi:def-call-out svm_destroy_model (:library svm-so)
  (:arguments (model model)) (:return-type nil))
(defun destroy-model (model)
  (when (validp model)
    (svm_destroy_model model)
    (setf (validp model) nil)))
(cl:defun finalize-model (model)
  (ext:finalize (set-foreign-pointer model :copy) #'destroy-model)
  model)

(def-call-out check-parameter (:library svm-so) (:name "svm_check_parameter")
  (:arguments (problem (c-pointer problem)) (param (c-pointer parameter)))
  (:return-type c-string))
(def-call-out svm_train (:library svm-so)
  (:arguments (problem (c-pointer problem)) (param (c-pointer parameter)))
  (:return-type model))
(defun train (problem parameter)
  (let ((check (check-parameter problem parameter)))
    (if check (error "~S: ~A" 'train check)
        (finalize-model (svm_train problem parameter)))))

(ffi:def-call-out svm_cross_validation (:library svm-so)
  (:arguments (problem (c-pointer problem)) (param (c-pointer parameter))
              (nr_fold int) (target c-pointer))
  (:return-type nil))
(defun cross-validation (problem param nr_fold)
  (with-foreign-object (target `(c-array double-float ,(problem-l problem)))
    (svm_cross_validation problem param nr_fold target)
    (foreign-value target)))

(def-call-out save-model (:library svm-so) (:name "svm_save_model")
  (:arguments (model_file_name c-string) (model model))
  (:return-type int))
(ffi:def-call-out svm_load_model (:library svm-so)
  (:arguments (model_file_name c-string))
  (:return-type model))
(defun load-model (model_file_name)
  (finalize-model (svm_load_model model_file_name)))

(def-call-out get-svm-type (:library svm-so) (:name "svm_get_svm_type")
  (:arguments (model model))
  (:return-type int))
(def-call-out get-nr-class (:library svm-so) (:name "svm_get_nr_class")
  (:arguments (model model))
  (:return-type int))
(ffi:def-call-out svm_get_labels (:library svm-so)
  (:arguments (model model) (label c-pointer))
  (:return-type nil))
(defun get-labels (model)
  (with-foreign-object (label `(c-array int ,(get-nr-class model)))
    (svm_get_labels model label)
    (foreign-value label)))
(def-call-out get-svr-probability (:library svm-so)
  (:name "svm_get_svr_probability")
  (:arguments (model model))
  (:return-type double-float))

(ffi:def-call-out svm_predict_values1 (:name "svm_predict_values")
  (:arguments (model model) (x (c-array-ptr node))
              (dec_values (c-ptr double-float) :out))
  (:return-type nil) (:library svm-so))
(ffi:def-call-out svm_predict_values2 (:name "svm_predict_values")
  (:arguments (model model) (x (c-array-ptr node))
              (dec_values c-pointer))
  (:return-type nil) (:library svm-so))
(defun predict-values (model x)
  (case (get-svm-type model)
    ((ONE_CLASS EPSILON_SVR NU_SVR)
     (vector (svm_predict_values1 model x)))
    (t (let* ((nr-class (get-nr-class model))
              (len (/ (* nr-class (1- nr-class)) 2)))
         (with-foreign-object (dec-values `(c-array double-float ,len))
           (svm_predict_values2 model x dec-values)
           (foreign-value dec-values))))))

(def-call-out predict (:library svm-so) (:name "svm_predict")
  (:arguments (model model) (x (c-array-ptr node)))
  (:return-type double-float))
(ffi:def-call-out svm_predict_probability (:library svm-so)
  (:arguments (model model) (x (c-array-ptr node))
              (prob_estimates c-pointer))
  (:return-type double-float))
(defun predict-probability (model x)
  (with-foreign-object (prob_estimates `(c-array int ,(get-nr-class model)))
    (values (svm_predict_probability model x prob_estimates)
            (foreign-value prob_estimates))))

;; not needed!
;; (def-call-out destroy-param (:library svm-so) (:name "svm_destroy_param")
;;   (:arguments (param (c-pointer parameter))) (:return-type nil))

(def-call-out check-probability-model (:library svm-so)
  (:name "svm_check_probability_model")
  (:arguments (model model))
  (:return-type int))

;;;
;;; high-level helpers
;;;

(defun destroy-parameter (parameter)
  (when (validp parameter)
    ;; (destroy-param parameter) -- not needed!
    (foreign-free parameter :full t)
    (setf (validp parameter) nil)))

;; the defaults are the same as those in svm_train (see README)
(defun make-parameter (&key (v #() v-p)
                       ((svm_type svm_type) (if v-p (svref v 0) C_SVC))
                       ((kernel_type kernel_type) (if v-p (svref v 1) RBF))
                       ((degree degree) (if v-p (svref v 2) 3))
                       ((gamma gamma) (if v-p (svref v 3) 0d0)) ; 1/maxindex
                       ((coef0 coef0) (if v-p (svref v 4) 0d0))
                       ((cache_size cache_size) (if v-p (svref v 5) 1d2))
                       ((eps eps) (if v-p (svref v 6) 1d-3))
                       ((C C) (if v-p (svref v 7) 1d0))
                       ((nr_weight nr_weight) (if v-p (svref v 8) 0))
                       ((weight_label weight_label) (if v-p (svref v 9) #()))
                       ((weight weight) (if v-p (svref v 10) #()))
                       ((nu nu) (if v-p (svref v 11) 5d-1))
                       ((p p) (if v-p (svref v 12) 1d-1))
                       ((shrinking shrinking) (if v-p (svref v 13) 1))
                       ((probability probability) (if v-p (svref v 14) 0)))
  (assert (= nr_weight (length weight_label)) (nr_weight weight_label)
          "~S: nr_weight=~:D /= ~:D=(length weight_label)"
          'make-parameter nr_weight (length weight_label))
  (assert (= nr_weight (length weight)) (nr_weight weight)
          "~S: nr_weight=~:D /= ~:D=(length weight)"
          'make-parameter nr_weight (length weight))
  (let ((ret (allocate-deep 'parameter
                            (vector svm_type kernel_type degree
                                    gamma coef0 cache_size eps C
                                    nr_weight weight_label weight
                                    nu p shrinking probability))))
    ;; you DO no have to call (destroy-parameter ret) yourself!
    (ext:finalize ret #'destroy-parameter)
    ret))

(defun make-problem (&key (l 0) y x)
  (assert (= l (length y)) (l y)
          "~S: l=~:D /= ~:D=(length y)" 'make-problem l (length y))
  (assert (= l (length x)) (l x)
          "~S: l=~:D /= ~:D=(length x)" 'make-problem l (length x))
  ;; you must call (destroy-problem ret) yourself!
  ;; -- but remember that `model' returned by `train' uses `problem',
  ;;    so you cannot free `problem' until you free all `model's
  (allocate-deep 'problem (list l (coerce y 'vector) (coerce x 'vector))))

(defun destroy-problem (problem)
  (when (validp problem)
    (foreign-free problem :full t)
    (setf (validp problem) nil)))

(defun load-problem (file &key (log *standard-output*))
  ;; load the `problem' object from a standard libsvm/svmlight problem file:
  ;; target index1:value1 index2:value2 ...
  (let ((len 0) y x (maxindex 0)
        (*read-default-float-format* 'double-float))
    (with-open-file (in file)
      (when log
        (format log "~&;; ~S(~S): ~:D byte~:P..."
                'load-problem file (file-length in))
        (force-output log))
      (loop :for line = (read-line in nil nil) :while line
        :unless (or (zerop (length line)) (char= #\# (aref line 0))) :do
        (incf len)
        (multiple-value-bind (target pos) (read-from-string line)
          (push (coerce target 'double-float) y)
          (push
           (let ((ret ()))
             (loop :with index :and value
               :for colon = (position #\: line :start pos) :while colon :do
               (multiple-value-setq (index pos)
                 (parse-integer line :start pos :end colon))
               (multiple-value-setq (value pos)
                 (read-from-string line t nil :start (1+ colon)))
               (setq maxindex (max maxindex index))
               (push (list (coerce index 'integer)
                           (coerce value 'double-float))
                     ret))
             (coerce (nreverse (cons (list -1 0d0) ret)) 'vector))
           x)))
      (when log (format log "~:D record~:P~%" len)))
    (values (make-problem :l len :y (nreverse y) :x (nreverse x))
            maxindex)))

(defun save-problem (file problem &key (log *standard-output*))
  (with-open-file (out file :direction :output)
    (destructuring-bind (size y x) (foreign-value problem)
      (when log
        (format log "~&;; ~S(~S): ~:D record~:P..." 'save-problem file size)
        (force-output log))
      (dotimes (i size)
        (format out "~G" (aref y i))
        (let ((nodes (aref x i)))
          (loop :for j :upfrom 0 :for node = (aref nodes j)
            :for index = (first node) :for value = (second node)
            :while (/= index -1) :do
            (format out " ~D:~G" index value)))
        (terpri out)))
    (when log (format log "~:D byte~:P~%" (file-length out)))))

(pushnew :libsvm *features*)
(provide "libsvm")
(pushnew "LIBSVM" custom:*system-package-list* :test #'string=)
