;;; Lisp wrappers for the Matlab API
;;; <http://www.mathworks.com/access/helpdesk/help/techdoc/apiref/apiref.shtml>

(require "matlab")

(in-package "MATLAB")

(pushnew :matlab *features*)

(defun matfile-content (mf)
  "return the vector of strings naming variables in MATfile"
  (multiple-value-bind (buf num) (matlab::matGetDir mf)
    (unwind-protect
         (ffi:with-c-var (names `(ffi:c-ptr (ffi:c-array ffi:c-string ,num)))
           (setf (ffi:cast names 'ffi:c-pointer) buf)
           names)
      (matlab::mxFree buf))))

(push "MATLAB" custom:*system-package-list*)
