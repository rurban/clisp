;; Foreign functions provided by the win32 system libraries
;; Sam Steingold 2003

(defpackage "WIN32"
  (:case-sensitive t)
  (:nicknames "WOE32" "W32")
  (:use))

(eval-when (compile eval)
  (require "exporting" "../../exporting")
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
(def-c-type dword uint32)
(def-c-type word uint16)

;; this is not necessary: we are not creating a C file anyway
;;(c-lines "#define WINVER 0x0500~%#include <windows.h>~%")

(def-call-out GetCommandLineA (:library "kernel32.dll")
  (:arguments) (:return-type c-string))

(def-call-out GetLastError (:library "kernel32.dll")
  (:arguments) (:return-type dword))

(def-call-out GetCurrentProcess (:library "kernel32.dll")
  (:arguments) (:return-type handle))

(def-call-out GetCurrentThread (:library "kernel32.dll")
  (:arguments) (:return-type handle))

(def-call-out GetCurrentProcessId (:library "kernel32.dll")
  (:arguments) (:return-type dword))

(def-call-out CloseHandle (:library "kernel32.dll")
  (:arguments (handle handle)) (:return-type boolean))

;; (c-lines "#include <winnt.h>~%")
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
  (:arguments (access-flag dword) ; PROCESS
              (inherit-handle boolean)
              (pid dword))
  (:return-type handle))

;;(c-lines "#include <winuser.h>~%")
(def-c-enum EWX                 ; shutdown operation
  (EWX_LOGOFF           #x0)
  (EWX_SHUTDOWN         #x1)
  (EWX_REBOOT           #x2)
  (EWX_FORCE            #x4)
  (EWX_POWEROFF         #x8)
  (EWX_FORCEIFHUNG     #x10))
(def-call-out ExitWindowsEx (:library "User32.dll")
  (:arguments (flags uint)      ; EWX
              (reserved dword))
  (:return-type boolean))

(def-c-enum GR_OBJECTS GR_GDIOBJECTS GR_USEROBJECTS)
(def-call-out GetGuiResources (:library "User32.dll")
  (:arguments (process handle)
              (flags dword))    ; GR_OBJECTS
  (:return-type dword))

;; create an icon
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
              (options int))    ; load_options
  (:return-type handle))
#| example:
 (setq icon (win32:LoadImageA nil "d:\\gnu\\clisp\\current\\doc\\clisp.ico"
                              win32:IMAGE_ICON 0 0  win32:LR_LOADFROMFILE))
|#

(defconstant BUFSIZ 4096)       ; <stdio.h>
(defconstant MAX_PATH 260)      ; <windef.h>

(def-call-out GetModuleFileNameA (:library "kernel32.dll")
  (:arguments (application-instance-handle handle)
              (name (c-ptr (c-array-max character #.MAX_PATH)) :out :alloca)
              (size dword))  ; always pass MAX_PATH as the second argument
  (:return-type dword))

(def-call-out GetModuleHandleA (:library "kernel32.dll")
  (:arguments (name c-string))
  (:return-type handle))

;; (c-lines "#include <winicon.h>~%")

(def-call-out GetConsoleTitleA (:library "kernel32.dll")
  (:arguments (buffer (c-ptr (c-array-max character #.BUFSIZ)) :out :alloca)
              (size dword))  ; always pass BUFSIZ as the only argument
  (:return-type dword))

(def-call-out SetConsoleTitleA (:library "kernel32.dll")
  (:arguments (title c-string))
  (:return-type boolean))

;; system information
(def-call-out GetSystemDirectoryA (:library "kernel32.dll")
  (:arguments (buffer (c-ptr (c-array-max character #.MAX_PATH)) :out :alloca)
              (size uint)) ; pass MAX_PATH
  (:return-type uint))
(def-call-out GetWindowsDirectoryA (:library "kernel32.dll")
  (:arguments (buffer (c-ptr (c-array-max character #.MAX_PATH)) :out :alloca)
              (size uint)) ; pass MAX_PATH
  (:return-type uint))
(def-call-out GetCurrentDirectoryA (:library "kernel32.dll")
  (:arguments (size dword) ; pass MAX_PATH
              (buffer (c-ptr (c-array-max character #.MAX_PATH)) :out :alloca))
  (:return-type dword))
(def-call-out GetVersion (:library "kernel32.dll")
  (:arguments) (:return-type dword))

;; user name
(defconstant UNLEN 256)         ; <lmcons.h>
(def-call-out GetUserNameA (:library "advapi32.dll")
  (:arguments (buffer (c-ptr (c-array-max character #.UNLEN)) :out :alloca)
              (size (c-ptr dword) :in-out)) ; pass UNLEN
  (:return-type boolean))

;; declared in <secext.h>, include <security.h>
(def-c-enum EXTENDED_NAME_FORMAT
  (NameUnknown 0)
  (NameFullyQualifiedDN 1)
  (NameSamCompatible 2)
  (NameDisplay 3)
  (NameUniqueId 6)
  (NameCanonical 7)
  (NameUserPrincipal 8)
  (NameCanonicalEx 9)
  (NameServicePrincipal 10))
(def-call-out GetUserNameExA (:library "secur32.dll")
  (:arguments (name-format EXTENDED_NAME_FORMAT)
              (buffer (c-ptr (c-array-max character #.UNLEN)) :out :alloca)
              (size (c-ptr ulong) :in-out)) ; pass UNLEN
  (:return-type boolean))
(def-call-out GetComputerObjectNameA (:library "secur32.dll")
  (:arguments (name-format EXTENDED_NAME_FORMAT)
              (buffer (c-ptr (c-array-max character #.UNLEN)) :out :alloca)
              (size (c-ptr ulong) :in-out)) ; pass UNLEN
  (:return-type boolean))

;; computer name
(defconstant MAX_COMPUTERNAME_LENGTH 16) ; <winbase.h>

(def-call-out GetComputerNameA (:library "kernel32.dll")
  (:arguments (buffer (c-ptr (c-array-max character #.MAX_COMPUTERNAME_LENGTH))
                      :out :alloca)
              (size (c-ptr dword) :in-out)) ; pass MAX_COMPUTERNAME_LENGTH
  (:return-type boolean))

(def-c-enum COMPUTER_NAME_FORMAT
  ComputerNameNetBIOS
  ComputerNameDnsHostname
  ComputerNameDnsDomain
  ComputerNameDnsFullyQualified
  ComputerNamePhysicalNetBIOS
  ComputerNamePhysicalDnsHostname
  ComputerNamePhysicalDnsDomain
  ComputerNamePhysicalDnsFullyQualified
  ComputerNameMax)

(def-call-out GetComputerNameExA (:library "kernel32.dll")
  (:arguments (type-name COMPUTER_NAME_FORMAT)
              (buffer (c-ptr (c-array-max character #.MAX_COMPUTERNAME_LENGTH))
                      :out :alloca)
              (size (c-ptr dword) :in-out)) ; pass MAX_COMPUTERNAME_LENGTH
  (:return-type boolean))


#|

 (defun check-all (enum-type function buf-size)
  (format t "~&;; ~s:~%" function)
  (maphash (lambda (key val)
             (let ((res (multiple-value-list (funcall function key buf-size))))
               (format t " ~S -> ~S~@[ ~S~]~%" val res
                       (unless (car res) (w32:GetLastError)))))
           (ffi::enum-table enum-type)))
 (check-all 'w32:EXTENDED_NAME_FORMAT 'w32:GetUserNameExA w32::UNLEN)
 (check-all 'w32:EXTENDED_NAME_FORMAT 'w32:GetComputerObjectNameA w32::UNLEN)
 (check-all 'w32:COMPUTER_NAME_FORMAT 'w32:GetComputerNameExA
            w32::MAX_COMPUTERNAME_LENGTH)

|#

;;; ==========================================================================
;;; clean up
(lisp:in-package "CL-USER")
(provide "win32")
