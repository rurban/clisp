;;; Lisp wrappers for the Matlab API
;;; <http://www.mathworks.com/access/helpdesk/help/techdoc/apiref/apiref.shtml>

(require "matlab")

(in-package "MATLAB")

(pushnew :matlab *features*)

(defun matfile-content (mf)
  "return the vector of strings naming variables in MATfile"
  (multiple-value-bind (buf num) (matGetDir mf)
    (unwind-protect
         (with-c-var (names `(c-ptr (c-array c-string ,num)))
           (setf (cast names 'c-pointer) buf)
           names)
      (mxFree buf))))

(push "MATLAB" custom:*system-package-list*)
