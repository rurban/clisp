;;; matlab demo
(defpackage "MATLAB-DEMO" (:use "CL" "EXT" "FFI" "MATLAB"))
(in-package "MATLAB-DEMO")

;;; engine
(defvar *eng-output-buffer* (allocate-shallow '(c-array-max character 1024))
  "pass as the second argument to `engOutputBuffer'")
(defvar *engine* (matlab:engOpen ""))

(matlab:engOutputBuffer *engine* *eng-output-buffer* 1024)
(matlab:engEvalString *engine* "sin(pi)")
(foreign-value *eng-output-buffer*)

;; cleanup
(matlab:engClose *engine*)
(foreign-free *eng-output-buffer*)
(setq *eng-output-buffer* nil *engine* nil)

;;; MATfile
(defvar *matfile*
  (matlab:matOpen "d:/sds/work/isd-cvs/heval/matlab/heval_probs.mat" "r"))
(matlab:matfile-content *matfile*)


;; cleanup
(matlab:matClose *matfile*)
