;;; Lisp wrappers for the Matlab API
;;; <http://www.mathworks.com/access/helpdesk/help/techdoc/apiref/apiref.shtml>

(require "matlab")

(defpackage "MATLAB"
  (:export "MATFILE-CONTENT"))

(in-package "SYS")
(eval-when (compile load) (setf (package-lock "SYS") nil))
(pushnew :matlab *features*)

(defun matfile-content (mf)
  "return the vector of strings naming variables in MATfile"
  (multiple-value-bind (buf num) (matlab::matGetDir mf)
    (unwind-protect
         (ffi:with-c-var (names `(ffi:c-ptr (ffi:c-array ffi:c-string ,num)))
           (setf (ffi:cast names 'ffi:c-pointer) buf)
           names)
      (matlab::mxFree buf))))

(push "MATLAB" ext:*system-package-list*)
(eval-when (compile load)
  (setf (ext:package-lock ext:*system-package-list*) t))
