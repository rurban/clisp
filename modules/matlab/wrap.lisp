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

(defvar *command* ""
  "The path to pass to engOpen().")
(defvar *engine* nil
  "The current matlab engine.")

(defmacro with-engine ((&optional (eng '*engine*) (cmd '*command*)) &body body)
  "open a matlab engine, do BODY, close the engine"
  `(let ((,eng (engOpen ,cmd)))
     (unwind-protect (progn ,@body)
       (when ,eng (engClose ,eng)))))

(defun engine (&optional (cmd *command*))
  (or *engine* (setq *engine* (engOpen cmd))))

(defmacro with-MATfile ((file name &optional (mode "r")) &body body)
  "open a MAT file, do BODY, close the file"
  `(let ((,file (matOpen ,name ,mode)))
     (unwind-protect (progn ,@body)
       (when ,file (matClose ,file)))))

(defun copy-lisp-to-matlab (lisp-mx &optional mx-array)
  "copy the data in the matrix into the matlab array.
the dimensions should be compatible.
if the destination does not exist, it is created."
  (let ((d0 (array-dimension lisp-mx 0)) (d1 (array-dimension lisp-mx 1)))
    (if mx-array
        (assert (and (= d0 (mxGetM mx-array)) (= d1 (mxGetN mx-array)))
                (mx-array) "Incompatible dimensions: ~Dx~D vs ~Dx~D"
                d0 d1 (mxGetM mx-array) (mxGetN mx-array))
        (setq mx-array (mxCreateDoubleMatrix d0 d1 mxREAL)))
    (loop :for i :from 0 :below d0 :do
      (loop :for j :from 0 :below d1 :do
        (setf (mx-aref mx-array i j d1)
              (coerce (aref lisp-mx i j) 'double-float))))
    mx-array))

(defun copy-matlab-to-lisp (mx-array &optional lisp-mx)
  "copy the data from the matlab array into the matrix.
the dimensions should be compatible.
if the destination does not exist, it is created."
  (let ((d0 (mxGetM mx-array)) (d1 (mxGetN mx-array)))
    (if lisp-mx
        (assert (and (= d0 (array-dimension lisp-mx 0))
                     (= d1 (array-dimension lisp-mx 1)))
                (mx-array) "Incompatible dimensions: ~Dx~D vs ~Dx~D"
                d0 d1 (array-dimension lisp-mx 0) (array-dimension lisp-mx 1))
        (setq lisp-mx (make-array (list d0 d1))))
    (loop :for i :from 0 :below d0 :do
      (loop :for j :from 0 :below d1 :do
        (setf (aref lisp-mx i j)
              (mx-aref mx-array i j d1))))
    lisp-mx))

(defun invert-matrix (mx &optional (eng (engine)))
  "open an engine, compute the matrix inverse (in place), close engine"
  (let ((mmx (copy-lisp-to-matlab mx)))
    (engPutVariable eng "mx" mmx)
    (engEvalString eng "mx=inv(mx);")
    (mxDestroyArray mmx)
    (setq mmx (engGetVariable eng "mx"))
    (copy-matlab-to-lisp mmx mx)
    (mxDestroyArray mmx)
    mx))

(pushnew "MATLAB" custom:*system-package-list* :test #'string=)
