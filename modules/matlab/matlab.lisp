;;; Matlab API interface
;;; <http://www.mathworks.com/access/helpdesk/help/techdoc/apiref/apiref.shtml>
;;;
;;; Copyright (C) 2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(defpackage "MATLAB"
  (:case-sensitive t) (:use))

(eval-when (compile eval)
  (require "exporting" "../exporting")
  (make-exporting "MATLAB"
    cl:compile cl:defconstant cl:eval cl:load
    ffi:cast ffi:char ffi:character ffi:c-array ffi:c-array-max ffi:boolean
    ffi:c-array-ptr ffi:c-function ffi:c-ptr ffi:c-ptr-null ffi:c-pointer
    ffi:c-string ffi:c-struct ffi:deref ffi::foreign-value ffi:double-float
    ffi:element ffi:int ffi:long ffi:nil ffi:short ffi:sint8 ffi:sint16
    ffi:sint32 ffi:sint64 ffi:single-float ffi:sizeof ffi:slot ffi:uchar
    ffi:uint ffi:uint8 ffi:uint16 ffi:uint32 ffi:uint64 ffi:ulong ffi:ushort
    ffi:with-c-var))

(in-package "MATLAB")

;;; types and constants


;;; foreign function definitions

(ffi:default-foreign-language :stdc)

;;; --- Engine ---
(c-lines "#include <engine.h>~%")
(def-c-type Engine c-pointer)
;; int engClose(Engine *ep);
(def-call-out engClose (:arguments (ep Engine)) (:return-type int))
;; Engine *engOpen(const char *startcmd);
(def-call-out engOpen (:arguments (startcmd c-string)) (:return-type Engine))

;; int engEvalString(Engine *ep, const char *string);
(def-call-out engEvalString (:arguments (ep Engine) (cmd c-string))
  (:return-type int))
;; int engOutputBuffer(Engine *ep, char *p, int n);
(def-call-out engOutputBuffer (:arguments (ep Engine) (p c-pointer) (n int))
  (:return-type int))

(def-c-type mxArray c-pointer)
;; mxArray *engGetVariable(Engine *ep, const char *name);
(def-call-out engGetVariable (:arguments (ep Engine) (name c-string))
  (:return-type mxArray))
;; int engPutVariable(Engine *ep, const char *name, const mxArray *mp);
(def-call-out engPutVariable
    (:arguments (ep Engine) (name c-string) (mp mxArray))
  (:return-type int))

;;; windows-only
;; int engGetVisible(Engine *ep, bool *value);
(def-call-out engGetVisible
    (:arguments (ep Engine) (value (c-ptr boolean) :out))
  (:return-type int))
;; int engSetVisible(Engine *ep, bool value);
(def-call-out engSetVisible (:arguments (ep Engine) (value boolean))
  (:return-type int))

;;; windows-only
;; Engine *engOpenSingleUse(const char *startcmd, void *dcom, int *retstatus);
(def-call-out engOpenSingleUse
    (:arguments (startcmd c-string) (dcom c-pointer)
                (retstatus (c-ptr int) :out))
  (:return-type Engine))

;;; --- MAT-File ---
(c-lines "#include <mat.h>~%")
(def-c-type MATFile c-pointer)
;; int matClose(MATFile *mfp);
(def-call-out matClose (:arguments (mfp MATFile)) (:return-type int))
;; MATFile *matOpen(const char *filename, const char *mode);
(def-call-out matOpen (:arguments (filename c-string) (mode c-string))
  (:return-type MATFile))

;; char **matGetDir(MATFile *mfp, int *num);
(def-call-out matGetDir (:arguments (mfp MATFile) (num (c-ptr int) :out))
  (:return-type c-pointer)) ; release with mxFree()

;; FILE *matGetFp(MATFile *mfp);
(def-call-out matGetFp (:arguments (mfp MATFile)) (:return-type c-pointer))

;; mxArray *matGetNextVariable(MATFile *mfp, const char *name);
(def-call-out matGetNextVariable
    (:arguments (mfp MATFile) (name (c-ptr c-string) :out))
  (:return-type mxArray))

;; mxArray *matGetNextVariableInfo(MATFile *mfp, const char *name);
(def-call-out matGetNextVariableInfo
    (:arguments (mfp MATFile) (name (c-ptr c-string) :out))
  (:return-type mxArray))

;; mxArray *matGetVariable(MATFile *mfp, const char *name);
(def-call-out matGetVariable (:arguments (mfp MATFile) (name c-string))
  (:return-type mxArray))
;; mxArray *matGetVariableInfo(MATFile *mfp, const char *name);
(def-call-out matGetVariableInfo (:arguments (mfp MATFile) (name c-string))
  (:return-type mxArray))
;; int matPutVariable(MATFile *mfp, const char *name, const mxArray *mp);
(def-call-out matPutVariable
    (:arguments (mfp MATFile) (name c-string) (mp mxArray))
  (:return-type int))
;; int matDeleteVariable(MATFile *mfp, const char *name);
(def-call-out matDeleteVariable (:arguments (mfp MATFile) (name c-string))
  (:return-type int))
;; int matPutVariableAsGlobal(MATFile*mfp, const char*name, const mxArray*mp);
(def-call-out matPutVariableAsGlobal
    (:arguments (mfp MATFile) (name c-string) (mp mxArray))
  (:return-type int))



;;; --- MEX ---
(c-lines "#include <mex.h>~%")
;; int mexAtExit(void (*ExitFcn)(void));
(def-call-out mexAtExit (:arguments (func (c-function (:arguments))))
  (:return-type int))
;; extern int mexCallMATLAB(
;;     int         nlhs,                   /* number of expected outputs */
;;     mxArray     *plhs[],                /* pointer array to outputs */
;;     int         nrhs,                   /* number of inputs */
;;     mxArray     *prhs[],                /* pointer array to inputs */
;;     const char  *fcn_name               /* name of function to execute */
;;     );
(def-call-out mexCallMATLAB
    (:arguments (nlhs int) (plhs (c-ptr (c-array-max mxArray 50)) :out)
                (nrhs int) (prhs (c-array-max mxArray 50))
                (fcn_name c-string))
  (:return-type int))
;; Issue error message and return to MATLAB prompt
;; extern void mexErrMsgTxt(const char *error_msg);
(def-call-out mexErrMsgTxt (:arguments (error_msg c-string))
  (:return-type nil))
;; Issue formatted error message with corresponding error identifier and
;; return to MATLAB prompt.
;; extern void mexErrMsgIdAndTxt(
;;    const char * identifier, /* string with error message identifier */
;;    const char * err_msg,    /* printf-style format */
;;    ...                      /* any additional arguments */
;;    );
(def-call-out mexErrMsgIdAndTxt
    (:arguments (identifier c-string) (err_msg c-string))
  (:return-type nil))
;; Invoke an unidentified warning. Such warnings can only be affected by
;; the M-code 'warning * all', since they have no specific identifier.
;; extern void mexWarnMsgTxt(const char *warn_msg);
(def-call-out mexWarnMsgTxt (:arguments (warn_msg c-string))
  (:return-type nil))
;; Invoke a warning with message identifier 'identifier' and message
;; derived from 'fmt' and subsequent arguments. The warning may either
;; get printed as is (if it is set to 'on'), or not actually get printed
;; (if set to 'off'). See 'help warning' in MATLAB for more details.
;; extern void mexWarnMsgIdAndTxt(
;;     const char * identifier,    /* string with warning message identifer */
;;     const char * warn_msg,      /* printf-style format */
;;     ...                         /* any additional arguments */
;;     );
(def-call-out mexWarnMsgIdAndTxt
    (:arguments (identifier c-string) (warn_msg c-string))
  (:return-type nil))

;; Parse and execute MATLAB syntax in string.
;; Returns zero if successful, and a non zero value if an error occurs.
;; extern int mexEvalString(const char *str /* matlab command string */);
(def-call-out mexEvalString (:arguments (command c-string))
  (:return-type int))
;; mexFunction is the user defined C routine which is called upon
;; invocation of a mex function.
;; void mexFunction(
;;     int           nlhs,      /* number of expected outputs */
;;     mxArray       *plhs[],   /* array of pointers to output arguments */
;;     int           nrhs,      /* number of inputs */
;;     const mxArray *prhs[]    /* array of pointers to input arguments */
;; );
;(def-call-out mexFunction
;    (:arguments (nlhs int) (plhs (c-ptr (c-array-max mxArray 50)) :out)
;                (nrhs int) (prhs (c-array-max mxArray 50)))
;  (:return-type nil))
;; Return the name of a the MEXfunction currently executing.
;; extern const char *mexFunctionName(void);
(def-call-out mexFunctionName (:arguments) (:return-type c-string))
;; API interface which mimics the "get" function
;; extern const mxArray *mexGet(double handle, const char *property);

(def-call-out mexGet (:arguments (handle double-float) (property c-string))
  (:return-type mxArray))
;; mex equivalent to MATLAB's "set" function
;; extern int mexSet(double handle, const char *property, mxArray *value);
(def-call-out mexSet
    (:arguments (handle double-float) (property c-string) (value mxArray))
  (:return-type int))

;; return a copy of the array value with the specified variable name in
;; the specified workspace
;; extern mxArray *mexGetVariable(const char *workspace, const char *name);
(def-call-out mexGetVariable (:arguments (workspace c-string) (name c-string))
  (:return-type mxArray))
;; return a pointer to the array value with the specified variable name
;; in the specified workspace
;; extern const mxArray *mexGetVariablePtr(const char *workspace, const char *name);
(def-call-out mexGetVariablePtr
    (:arguments (workspace c-string) (name c-string))
  (:return-type mxArray))

;; Tell whether or not a mxArray is in MATLAB's global workspace.
;; extern bool mexIsGlobal(const mxArray *pA);
(def-call-out mexIsGlobal (:arguments (arr mxArray)) (:return-type boolean))

;; Lock a MEX-function so that it cannot be cleared from memory.
;; extern void mexLock(void);
(def-call-out mexLock (:arguments) (:return-type nil))
;; Unlock a locked MEX-function so that it can be cleared from memory.
;; extern void mexUnlock(void);
(def-call-out mexUnlock (:arguments) (:return-type nil))
;; Return true if the MEX-function is currently locked, false otherwise.
;; extern bool mexIsLocked(void);
(def-call-out mexIsLocked (:arguments) (:return-type boolean))

;; Remove all components of an array plus the array header itself
;; from MATLAB's memory allocation list.  The array will now
;; persist between calls to the mex function.  To destroy this
;; array, you will need to explicitly call mxDestroyArray().
;; extern void mexMakeArrayPersistent(mxArray *pa);
(def-call-out mexMakeArrayPersistent (:arguments (arr mxArray))
  (:return-type nil))
;; Remove memory previously allocated via mxCalloc from MATLAB's
;; memory allocation list.  To free this memory, you will need to
;; explicitly call mxFree().
;; extern void mexMakeMemoryPersistent(void *ptr);
(def-call-out mexMakeMemoryPersistent (:arguments (ptr c-pointer))
  (:return-type nil))

;; mex equivalent to MATLAB's "disp" function
;; extern int mexPrintf(const char *fmt, ...);

;; Place a copy of the array value into the specified workspace with the
;; specified name
;; extern int mexPutVariable(const char *workspace,const char *name,const mxArray *parray);
(def-call-out mexPutVariable
    (:arguments (workspace c-string) (name c-string) (arr mxArray))
  (:return-type int))

;; set or clear mexCallMATLAB trap flag (if set then an error in
;; mexCallMATLAB is caught and mexCallMATLAB will return a status value,
;; if not set an error will cause control to revert to MATLAB)
;; extern void mexSetTrapFlag(int flag);
(def-call-out mexSetTrapFlag (:arguments (flag int)) (:return-type nil))

;;; --- MX --- <FIXME:incomplete>
(c-lines "#include <matrix.h>~%")
(defconstant mxMAXNAM 64)

;; extern int mxAddField(mxArray array_ptr, const char *field_name);
(def-call-out mxAddField (:arguments (array_ptr mxArray) (field_name c-string))
  (:return-type int))

;; void mxFree(void *ptr);
(def-call-out mxFree (:arguments (ptr c-pointer)) (:return-type nil))



(cl:in-package "CL-USER")
(provide "matlab")
