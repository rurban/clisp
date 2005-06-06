;; Foreign functions provided by the win32 system libraries
;; Sam Steingold 2003-2005

(defpackage "WIN32"
  (:nicknames "WOE32" "W32")
  (:modern t)
  (:use "COMMON-LISP" "FFI")
  (:shadowing-import-from "EXPORTING"
           #:defconstant #:defun #:defmacro
           #:def-c-type #:def-c-enum #:def-c-struct #:def-c-var #:def-call-out))

(ffi:default-foreign-language :stdc)

(in-package "W32")


(def-c-type handle c-pointer)
(def-c-type dword uint32)
(def-c-type word uint16)

;; this is not necessary: we are not creating a C file anyway
;;(c-lines "#define WINVER 0x0500~%#include <windows.h>~%")

(defconstant system32 (ext:string-concat (ext:getenv "WINDIR") "\\system32\\"))
(defconstant advapi32 (ext:string-concat system32 "advapi32.dll"))
(defconstant kernel32 (ext:string-concat system32 "kernel32.dll"))
(defconstant secur32 (ext:string-concat system32 "secur32.dll"))
(defconstant shell32 (ext:string-concat system32 "shell32.dll"))
(defconstant user32 (ext:string-concat system32 "user32.dll"))

(def-call-out GetCommandLineA (:library kernel32)
  (:arguments) (:return-type c-string))

(def-call-out GetLastError (:library kernel32)
  (:arguments) (:return-type dword))

(def-call-out GetCurrentProcess (:library kernel32)
  (:arguments) (:return-type handle))

(def-call-out GetCurrentThread (:library kernel32)
  (:arguments) (:return-type handle))

(def-call-out GetCurrentProcessId (:library kernel32)
  (:arguments) (:return-type dword))

(def-call-out CloseHandle (:library kernel32)
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

(def-call-out OpenProcess (:library kernel32)
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
(def-call-out ExitWindowsEx (:library user32)
  (:arguments (flags uint)      ; EWX
              (reserved dword))
  (:return-type boolean))

(def-c-enum GR_OBJECTS GR_GDIOBJECTS GR_USEROBJECTS)
(def-call-out GetGuiResources (:library user32)
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
(def-call-out LoadImageA (:library user32)
  (:arguments (application-instance-handle handle)
              (image-name c-string)
              (type uint) (width int) (height int)
              (options int))    ; load_options
  (:return-type handle))
#| example:
 (setq icon (win32:LoadImageA nil "d:\\gnu\\clisp\\current\\doc\\clisp.ico"
                              win32:IMAGE_ICON 0 0  win32:LR_LOADFROMFILE))
|#

(eval-when (compile eval load)
  (defconstant BUFSIZ 4096)     ; <stdio.h>
  (defconstant MAX_PATH 260))   ; <windef.h>

(def-call-out GetModuleFileNameA (:library kernel32)
  (:arguments (application-instance-handle handle)
              (name (c-ptr (c-array-max character #.MAX_PATH)) :out :alloca)
              (size dword))  ; always pass MAX_PATH as the second argument
  (:return-type dword))

(def-call-out GetModuleHandleA (:library kernel32)
  (:arguments (name c-string))
  (:return-type handle))

;; (c-lines "#include <winicon.h>~%")

(def-call-out GetConsoleTitleA (:library kernel32)
  (:arguments (buffer (c-ptr (c-array-max character #.BUFSIZ)) :out :alloca)
              (size dword))  ; always pass BUFSIZ as the only argument
  (:return-type dword))

(def-call-out SetConsoleTitleA (:library kernel32)
  (:arguments (title c-string))
  (:return-type boolean))

;; system information
(def-call-out GetSystemDirectoryA (:library kernel32)
  (:arguments (buffer (c-ptr (c-array-max character #.MAX_PATH)) :out :alloca)
              (size uint)) ; pass MAX_PATH
  (:return-type uint))
(def-call-out GetWindowsDirectoryA (:library kernel32)
  (:arguments (buffer (c-ptr (c-array-max character #.MAX_PATH)) :out :alloca)
              (size uint)) ; pass MAX_PATH
  (:return-type uint))
(def-call-out GetCurrentDirectoryA (:library kernel32)
  (:arguments (size dword) ; pass MAX_PATH
              (buffer (c-ptr (c-array-max character #.MAX_PATH)) :out :alloca))
  (:return-type dword))
(def-call-out GetVersion (:library kernel32)
  (:arguments) (:return-type dword))

;; user name
(eval-when (compile eval load)
  (defconstant UNLEN 256)) ; <lmcons.h>
(def-call-out GetUserNameA (:library advapi32)
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
(def-call-out GetUserNameExA (:library secur32)
  (:arguments (name-format EXTENDED_NAME_FORMAT)
              (buffer (c-ptr (c-array-max character #.UNLEN)) :out :alloca)
              (size (c-ptr ulong) :in-out)) ; pass UNLEN
  (:return-type boolean))
(def-call-out GetComputerObjectNameA (:library secur32)
  (:arguments (name-format EXTENDED_NAME_FORMAT)
              (buffer (c-ptr (c-array-max character #.UNLEN)) :out :alloca)
              (size (c-ptr ulong) :in-out)) ; pass UNLEN
  (:return-type boolean))

;; computer name
(eval-when (compile eval load)
  (defconstant MAX_COMPUTERNAME_LENGTH 16)) ; <winbase.h>

(def-call-out GetComputerNameA (:library kernel32)
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

(def-call-out GetComputerNameExA (:library kernel32)
  (:arguments (type-name COMPUTER_NAME_FORMAT)
              (buffer (c-ptr (c-array-max character #.MAX_COMPUTERNAME_LENGTH))
                      :out :alloca)
              (size (c-ptr dword) :in-out)) ; pass MAX_COMPUTERNAME_LENGTH
  (:return-type boolean))

;; http://msdn.microsoft.com/library/en-us/shellcc/platform/shell/reference/functions/shellexecute.asp
(def-c-enum SE_ERROR            ; <shellapi.h>
  (SE_ERR_FNF 2)
  (SE_ERR_PNF 3)
  (SE_ERR_ACCESSDENIED 5)
  (SE_ERR_OOM 8)
  (SE_ERR_DLLNOTFOUND 32)
  (SE_ERR_SHARE 26)
  (SE_ERR_ASSOCINCOMPLETE 27)
  (SE_ERR_DDETIMEOUT 28)
  (SE_ERR_DDEFAIL 29)
  (SE_ERR_DDEBUSY 30)
  (SE_ERR_NOASSOC 31))
(def-c-enum SHOW_COMMAND        ; <winuser.h>
  (SW_HIDE 0)
  (SW_NORMAL 1)
  (SW_SHOWNORMAL 1)
  (SW_SHOWMINIMIZED 2)
  (SW_MAXIMIZE 3)
  (SW_SHOWMAXIMIZED 3)
  (SW_SHOWNOACTIVATE 4)
  (SW_SHOW 5)
  (SW_MINIMIZE 6)
  (SW_SHOWMINNOACTIVE 7)
  (SW_SHOWNA 8)
  (SW_RESTORE 9)
  (SW_SHOWDEFAULT 10)
  (SW_FORCEMINIMIZE 11)
  (SW_MAX 11))
(def-call-out ShellExecuteA (:library shell32)
  (:arguments (parent handle) (operation c-string) (file c-string)
              (parameters c-string) (directory c-string) (show SHOW_COMMAND))
  (:return-type int))

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

(pushnew "WIN32" custom:*system-package-list* :test #'string=)
(provide "win32")
