;; Foreign functions provided by the win32 system libraries
;; Sam Steingold 2003

(defpackage "WIN32"
  (:case-sensitive t)
  (:nicknames "WOE32" "W32")
  (:use))

(eval-when (compile eval)
  (load "../../exporting")
  (make-exporting "WIN32"
    cl:compile cl:defconstant cl:eval cl:load
    ffi:cast ffi:char ffi:character ffi:c-array ffi:c-array-max
    ffi:c-array-ptr ffi:c-function ffi:c-ptr ffi:c-ptr-null ffi:c-pointer
    ffi:c-string ffi:c-struct ffi:deref ffi::foreign-value ffi:double-float
    ffi:element ffi:int ffi:long ffi:nil ffi:short ffi:sint8 ffi:sint16
    ffi:sint32 ffi:sint64 ffi:single-float ffi:sizeof ffi:slot ffi:uchar
    ffi:uint ffi:uint8 ffi:uint16 ffi:uint32 ffi:uint64 ffi:ulong ffi:ushort
    ffi:boolean ffi:with-c-var))

(ffi:default-foreign-language :stdc)

(in-package "W32")

(def-c-type handle c-pointer)

;; this is not necessary: we are not creating a C file anyway
;;(c-lines "#define WINVER 0x0500~%#include <windows.h>~%")

(def-call-out GetCommandLineA (:library "kernel32.dll")
  (:arguments) (:return-type c-string))

(def-call-out GetLastError (:library "kernel32.dll")
  (:arguments) (:return-type uint32))

(def-call-out GetCurrentProcess (:library "kernel32.dll")
  (:arguments) (:return-type handle))

(def-call-out GetCurrentThread (:library "kernel32.dll")
  (:arguments) (:return-type handle))

(def-call-out GetCurrentProcessId (:library "kernel32.dll")
  (:arguments) (:return-type uint32))

(def-call-out CloseHandle (:library "kernel32.dll")
  (:arguments (handle handle)) (:return-type boolean))

;; (c-lines "<winnt.h>~%")
(eval-when (compile eval load)
(def-c-enum RIGHTS
  (SYNCHRONIZE              #x100000)
  (STANDARD_RIGHTS_REQUIRED  #xF0000)
  (STANDARD_RIGHTS_READ	     #x20000)
  (STANDARD_RIGHTS_WRITE     #x20000)
  (STANDARD_RIGHTS_EXECUTE   #x20000)
  (STANDARD_RIGHTS_ALL      #x1F0000)
  (SPECIFIC_RIGHTS_ALL        #xFFFF)
  (ACCESS_SYSTEM_SECURITY  #x1000000)))

(def-c-enum PROCESS
  (PROCESS_TERMINATE             1)
  (PROCESS_CREATE_THREAD         2)
  (PROCESS_SET_SESSIONID         4)
  (PROCESS_VM_OPERATION          8)
  (PROCESS_VM_READ              16)
  (PROCESS_VM_WRITE             32)
  (PROCESS_DUP_HANDLE           64)
  (PROCESS_CREATE_PROCESS      128)
  (PROCESS_SET_QUOTA           256)
  (PROCESS_SET_INFORMATION     512)
  (PROCESS_QUERY_INFORMATION  1024)
  (PROCESS_ALL_ACCESS
   #.(cl:logior STANDARD_RIGHTS_REQUIRED SYNCHRONIZE #xFFF)))

(def-call-out OpenProcess (:library "kernel32.dll")
  (:arguments (access-flag uint32) ; see PROCESS above
              (inherit-handle boolean)
              (pid uint32))
  (:return-type handle))

;; create an icon
;;(c-lines "<winuser.h>~%")
(def-c-enum image_type
  (IMAGE_BITMAP 0) IMAGE_ICON IMAGE_CURSOR IMAGE_ENHMETAFILE)
(def-c-enum load_options
  (LR_DEFAULTCOLOR 0)
  (LR_MONOCHROME 1)
  (LR_COLOR 2)
  (LR_COPYRETURNORG 4)
  (LR_COPYDELETEORG 8)
  (LR_LOADFROMFILE 16)
  (LR_LOADTRANSPARENT 32)
  (LR_LOADREALSIZE 128)
  (LR_LOADMAP3DCOLORS 4096)
  (LR_CREATEDIBSECTION 8192)
  (LR_COPYFROMRESOURCE #x4000)
  (LR_SHARED 32768))
(def-call-out LoadImageA (:library "User32.dll")
  (:arguments (application-instance-handle handle)
              (image-name c-string)
              (type uint) (width int) (height int)
              (options int))
  (:return-type handle))
#| example:
 (setq icon (win32:LoadImageA nil "d:\\gnu\\clisp\\current\\doc\\clisp.ico"
                              win32:IMAGE_ICON 0 0  win32:LR_LOADFROMFILE))
|#

(eval-when (compile eval load)
  (defconstant MAX_PATH 260))   ; see <windef.h>

(def-call-out GetModuleFileNameA (:library "kernel32.dll")
  (:arguments (application-instance-handle handle)
              (name (c-ptr (c-array-max character #.MAX_PATH)) :out :alloca)
              (size int))  ; always pass MAX_PATH as the second argument
  (:return-type uint32))

(def-call-out GetModuleHandleA (:library "kernel32.dll")
  (:arguments (name c-string))
  (:return-type handle))

;;; ==========================================================================
;;; clean up
(lisp:in-package "CL-USER")
(provide "win32")
