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
    ffi:with-c-var))

(ffi:default-foreign-language :stdc)

(in-package "W32")

;; this is not necessary: we are not creating a C file anyway
;;(c-lines "#define WINVER 0x0500~%#include <windows.h>~%")

(def-call-out GetCommandLineA (:library "kernel32.dll")
  (:arguments) (:return-type c-string))

(def-call-out GetLastError (:library "kernel32.dll")
  (:arguments) (:return-type uint32))

;; create an icon
(c-lines "<winuser.h>~%")
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
  (LR_COPYFROMRESOURCE 0x4000)
  (LR_SHARED 32768))
(def-call-out LoadImageA (:library "User32.dll")
  (:arguments (application-instance-handle c-pointer)
              (image-name c-string)
              (type uint) (width int) (height int)
              (options int))
  (:return-type c-pointer))
#| example:
 (setq icon (win32:LoadImageA nil "d:\\gnu\\clisp\\current\\doc\\clisp.ico"
                             win32:IMAGE_ICON 0 0  win32:LR_LOADFROMFILE))
|#

;;; ==========================================================================
;;; clean up
(lisp:in-package "CL-USER")
(provide "win32")
