;; Foreign functions provided by the Linux C library version 4.5.26
;; Bruno Haible 5.3.1995

(defpackage "LINUX"
  (:case-sensitive t)
  (:nicknames "UNIX")
  (:use)
)

;; This requires linking with NEW_LIBS='linux.o -lm'.

(in-package "LISP")

(eval-when (compile eval)
  ; A temporary package, case-insensitive, so that we don't need to prefix
  ; everything with "lisp:" or "ffi:".
  (defpackage "LINUX-AUX"
    (:use "LISP" "FFI")
  )
)

(eval-when (compile eval)
  (in-package "LINUX-AUX")
)

(eval-when (compile eval)
  ; Symbols to be substituted
  (defconstant substitution
    '((linux::aref . lisp:aref)
      (linux::AREF . lisp:aref)
      (linux::ash . lisp:ash)
      (linux::ASH . lisp:ash)
      (linux::coerce . lisp:coerce)
      (linux::compile . lisp:compile)
      (linux::eval . lisp:eval)
      (linux::fill . lisp:fill)
      (linux::FLOOR . lisp:floor)
      (linux::gensym . lisp:gensym)
      (linux::let . lisp:let)
      (linux::load . lisp:load)
      (linux::load-time-value . lisp:load-time-value)
      (linux::logand . lisp:logand)
      (linux::logbitp . lisp:logbitp)
      (linux::logior . lisp:logior)
      (linux::lognot . lisp:lognot)
      (linux::multiple-value-bind . lisp:multiple-value-bind)
      (linux::MULTIPLE-VALUE-BIND . lisp:multiple-value-bind)
      (linux::progn . lisp:progn)
      (linux::t . lisp:t)
      (linux::+ . lisp:+)
      (linux::- . lisp:-)
      (linux::= . lisp:=)
      (linux::1- . lisp:1-)
      (linux::bitsizeof . ffi:bitsizeof)
      (linux::boolean . ffi:boolean)
      (linux::char . ffi:char)
      (linux::character . ffi:character)
      (linux::c-array . ffi:c-array)
      (linux::c-array-max . ffi:c-array-max)
      (linux::c-array-ptr . ffi:c-array-ptr)
      (linux::c-function . ffi:c-function)
      (linux::c-ptr . ffi:c-ptr)
      (linux::c-pointer . ffi:c-pointer)
      (linux::c-string . ffi:c-string)
      (linux::c-struct . ffi:c-struct)
      (linux::double-float . ffi:double-float)
      (linux::int . ffi:int)
      (linux::long . ffi:long)
      (linux::nil . ffi:nil)
      (linux::short . ffi:short)
      (linux::sint8 . ffi:sint8)
      (linux::sint16 . ffi:sint16)
      (linux::sint32 . ffi:sint32)
      (linux::sint64 . ffi:sint64)
      (linux::single-float . ffi:single-float)
      (linux::uchar . ffi:uchar)
      (linux::uint . ffi:uint)
      (linux::uint8 . ffi:uint8)
      (linux::uint16 . ffi:uint16)
      (linux::uint32 . ffi:uint32)
      (linux::uint64 . ffi:uint64)
      (linux::ulong . ffi:ulong)
      (linux::ushort . ffi:ushort)
  )  )
)

(eval-when (compile eval)
; We want to export all the symbols defined in this file.
  (macrolet ((exporting (defining-macro-name)
               (let ((original-macro-name (intern (string-upcase defining-macro-name) "FFI"))
                     (new-macro-name (intern defining-macro-name "LINUX")))
                 `(progn
                    (defmacro ,new-macro-name (name &rest more)
                      `(progn
                         (export ',name)
                         (,',original-macro-name ,name ,@(sublis substitution more))
                       )
                  ) )
             ) )
             (normal (defining-macro-name)
               (let ((original-macro-name (intern (string-upcase defining-macro-name) "FFI"))
                     (new-macro-name (intern defining-macro-name "LINUX")))
                 `(progn
                    (defmacro ,new-macro-name (&rest more)
                      `(,',original-macro-name ,@(sublis substitution more))
                  ) )
            )) )
    (exporting "defconstant")
    (exporting "defun")
    (exporting "defmacro")
    (exporting "define-modify-macro")
    (exporting "def-c-type")
    (exporting "def-c-enum")
    (exporting "def-c-struct")
    (exporting "def-c-var")
    (exporting "def-c-call-out")
    (normal "c-lines")
    (normal "eval-when")
  )
)

(in-package "LINUX")

(def-c-type longlong sint64)
(def-c-type ulonglong uint64)
(define-modify-macro orf () logior)
(define-modify-macro andf () logand)

; =========================== <sys/types.h> ===================================

(c-lines "#include <sys/types.h>~%")

; --------------------------- <asm/types.h> -----------------------------------

(def-c-type size_t uint)
(def-c-type ssize_t int)
(def-c-type ptrdiff_t int)
(def-c-type s8 sint8)
(def-c-type u8 uint8)
(def-c-type s16 sint16)
(def-c-type u16 uint16)
(def-c-type s32 sint32)
(def-c-type u32 uint32)
(def-c-type s64 sint64)
(def-c-type u64 uint64)

; --------------------------- <linux/types.h> ---------------------------------

(def-c-type time_t long)

(def-c-type clock_t long)

(defconstant null 0)

(def-c-type pid_t int)
(def-c-type uid_t ushort)
(def-c-type gid_t ushort)
(def-c-type dev_t ushort)
(def-c-type ino_t ulong)
(def-c-type mode_t ushort)
(def-c-type umode_t ushort)
(def-c-type nlink_t ushort)
(def-c-type daddr_t int)
(def-c-type off_t long)

(def-c-type loff_t longlong)

;(def-c-type u_char uchar)
;(def-c-type u_short ushort)
;(def-c-type u_int uint)
;(def-c-type u_long ulong)

;(def-c-type unchar uchar)
;(def-c-type unshort ushort)

(def-c-type caddr_t c-pointer)

(def-c-type cc_t uchar)
(def-c-type speed_t uint)
(def-c-type tcflag_t ulong)

(def-c-type fd_set (c-struct vector (fds_bits (c-array ulong 8))))
(defmacro fd_set (fd fdset)
  (let ((i (gensym)) (j (gensym)))
    `(MULTIPLE-VALUE-BIND (,i ,j) (FLOOR ,fd ,(bitsizeof 'ulong))
       (ORF (AREF ,fdset ,i) (ASH 1 ,j))
     )
) )
(defmacro fd_clr (fd fdset)
  (let ((i (gensym)) (j (gensym)))
    `(MULTIPLE-VALUE-BIND (,i ,j) (FLOOR ,fd ,(bitsizeof 'ulong))
       (ANDF (AREF ,fdset ,i) (LOGNOT (ASH 1 ,j)))
     )
) )
(defun fd_isset (fd fdset)
  (multiple-value-bind (i j) (FLOOR fd (load-time-value (bitsizeof 'ulong)))
    (logbitp j (aref fdset i))
) )
(defun fd_zero (fdset)
  (fill fdset 0)
)

#| ;; Present in libc-4, but not libc-5.
(def-c-struct ustat
  (f_tfree daddr_t)
  (f_tinode ino_t)
  (f_fname (c-array character 6))
  (f_fpack (c-array character 6))
)
|#

; --------------------------- <sys/types.h> -----------------------------------

(def-c-type fd_mask ulong)

; =========================== <_G_config.h> ===================================

; This binding is compatible between libc-4.5.26 and libc-5.2.18.
; The version number here corresponds to the first supported version.
(defconstant c_lib_version "4.5.26")
(defconstant c_lib_version_major 4)
(defconstant c_lib_version_minor 5)
(defconstant c_lib_version_subminor 26)
(defconstant g_lib_version "0.63")
; Later, we could write:
;(defconstant c_lib_version "5.2.18")
;(defconstant c_lib_version_major 5)
;(defconstant c_lib_version_minor 2)
;(defconstant c_lib_version_subminor 18)
;(defconstant g_lib_version "2.7.1")

(def-c-type _G_clock_t long)
(def-c-type _G_dev_t ushort)
(def-c-type _G_fpos_t long)
(def-c-type _G_gid_t ushort)
(def-c-type _G_ino_t ulong)
(def-c-type _G_mode_t ushort)
(def-c-type _G_nlink_t ushort)
(def-c-type _G_off_t long)
(def-c-type _G_pid_t int)
(def-c-type _G_ptrdiff_t int)
(def-c-type _G_sigset_t uint)
(def-c-type _G_size_t uint)
(def-c-type _G_time_t long)
(def-c-type _G_uid_t ushort)
(def-c-type _G_wchar_t long)
(def-c-type _G_int32_t sint32)
(def-c-type _G_uint32_t uint32)
(def-c-type _G_ssize_t int)
(def-c-type _G_va_list c-pointer)
(def-c-type _G_signal_return_type nil)
(def-c-type _G_sprintf_return_type int)
(defconstant _G_BUFSIZ 1024)
(defconstant _G_FOPEN_MAX 256)
(defconstant _G_FILENAME_MAX 1024)
(defconstant _G_NULL 0)

; ============================ <ansidecl.h> ===================================

; ============================ <sys/cdefs.h> ==================================

; ============================ <features.h> ===================================

; ============================== <stddef.h> ===================================

(def-c-type wchar_t int)

; ============================== <errno.h> ====================================

; --------------------------- <linux/errno.h> ---------------------------------

(defconstant    EPERM            1)     ; Operation not permitted
(defconstant    ENOENT           2)     ; No such file or directory
(defconstant    ESRCH            3)     ; No such process
(defconstant    EINTR            4)     ; Interrupted system call
(defconstant    EIO              5)     ; I/O error
(defconstant    ENXIO            6)     ; No such device or address
(defconstant    E2BIG            7)     ; Arg list too long
(defconstant    ENOEXEC          8)     ; Exec format error
(defconstant    EBADF            9)     ; Bad file number
(defconstant    ECHILD          10)     ; No child processes
(defconstant    EAGAIN          11)     ; Try again
(defconstant    ENOMEM          12)     ; Out of memory
(defconstant    EACCES          13)     ; Permission denied
(defconstant    EFAULT          14)     ; Bad address
(defconstant    ENOTBLK         15)     ; Block device required
(defconstant    EBUSY           16)     ; Device or resource busy
(defconstant    EEXIST          17)     ; File exists
(defconstant    EXDEV           18)     ; Cross-device link
(defconstant    ENODEV          19)     ; No such device
(defconstant    ENOTDIR         20)     ; Not a directory
(defconstant    EISDIR          21)     ; Is a directory
(defconstant    EINVAL          22)     ; Invalid argument
(defconstant    ENFILE          23)     ; File table overflow
(defconstant    EMFILE          24)     ; Too many open files
(defconstant    ENOTTY          25)     ; Not a typewriter
(defconstant    ETXTBSY         26)     ; Text file busy
(defconstant    EFBIG           27)     ; File too large
(defconstant    ENOSPC          28)     ; No space left on device
(defconstant    ESPIPE          29)     ; Illegal seek
(defconstant    EROFS           30)     ; Read-only file system
(defconstant    EMLINK          31)     ; Too many links
(defconstant    EPIPE           32)     ; Broken pipe
(defconstant    EDOM            33)     ; Math argument out of domain of func
(defconstant    ERANGE          34)     ; Math result not representable
(defconstant    EDEADLK         35)     ; Resource deadlock would occur
(defconstant    ENAMETOOLONG    36)     ; File name too long
(defconstant    ENOLCK          37)     ; No record locks available
(defconstant    ENOSYS          38)     ; Function not implemented
(defconstant    ENOTEMPTY       39)     ; Directory not empty
(defconstant    ELOOP           40)     ; Too many symbolic links encountered
(defconstant    EWOULDBLOCK     EAGAIN) ; Operation would block
(defconstant    ENOMSG          42)     ; No message of desired type
(defconstant    EIDRM           43)     ; Identifier removed
(defconstant    ECHRNG          44)     ; Channel number out of range
(defconstant    EL2NSYNC        45)     ; Level 2 not synchronized
(defconstant    EL3HLT          46)     ; Level 3 halted
(defconstant    EL3RST          47)     ; Level 3 reset
(defconstant    ELNRNG          48)     ; Link number out of range
(defconstant    EUNATCH         49)     ; Protocol driver not attached
(defconstant    ENOCSI          50)     ; No CSI structure available
(defconstant    EL2HLT          51)     ; Level 2 halted
(defconstant    EBADE           52)     ; Invalid exchange
(defconstant    EBADR           53)     ; Invalid request descriptor
(defconstant    EXFULL          54)     ; Exchange full
(defconstant    ENOANO          55)     ; No anode
(defconstant    EBADRQC         56)     ; Invalid request code
(defconstant    EBADSLT         57)     ; Invalid slot
(defconstant    EDEADLOCK       58)     ; File locking deadlock error
(defconstant    EBFONT          59)     ; Bad font file format
(defconstant    ENOSTR          60)     ; Device not a stream
(defconstant    ENODATA         61)     ; No data available
(defconstant    ETIME           62)     ; Timer expired
(defconstant    ENOSR           63)     ; Out of streams resources
(defconstant    ENONET          64)     ; Machine is not on the network
(defconstant    ENOPKG          65)     ; Package not installed
(defconstant    EREMOTE         66)     ; Object is remote
(defconstant    ENOLINK         67)     ; Link has been severed
(defconstant    EADV            68)     ; Advertise error
(defconstant    ESRMNT          69)     ; Srmount error
(defconstant    ECOMM           70)     ; Communication error on send
(defconstant    EPROTO          71)     ; Protocol error
(defconstant    EMULTIHOP       72)     ; Multihop attempted
(defconstant    EDOTDOT         73)     ; RFS specific error
(defconstant    EBADMSG         74)     ; Not a data message
(defconstant    EOVERFLOW       75)     ; Value too large for defined data type
(defconstant    ENOTUNIQ        76)     ; Name not unique on network
(defconstant    EBADFD          77)     ; File descriptor in bad state
(defconstant    EREMCHG         78)     ; Remote address changed
(defconstant    ELIBACC         79)     ; Can not access a needed shared library
(defconstant    ELIBBAD         80)     ; Accessing a corrupted shared library
(defconstant    ELIBSCN         81)     ; .lib section in a.out corrupted
(defconstant    ELIBMAX         82)     ; Attempting to link in too many shared libraries
(defconstant    ELIBEXEC        83)     ; Cannot exec a shared library directly
(defconstant    EILSEQ          84)     ; Illegal byte sequence
(defconstant    ERESTART        85)     ; Interrupted system call should be restarted
(defconstant    ESTRPIPE        86)     ; Streams pipe error
(defconstant    EUSERS          87)     ; Too many users
(defconstant    ENOTSOCK        88)     ; Socket operation on non-socket
(defconstant    EDESTADDRREQ    89)     ; Destination address required
(defconstant    EMSGSIZE        90)     ; Message too long
(defconstant    EPROTOTYPE      91)     ; Protocol wrong type for socket
(defconstant    ENOPROTOOPT     92)     ; Protocol not available
(defconstant    EPROTONOSUPPORT 93)     ; Protocol not supported
(defconstant    ESOCKTNOSUPPORT 94)     ; Socket type not supported
(defconstant    EOPNOTSUPP      95)     ; Operation not supported on transport endpoint
(defconstant    EPFNOSUPPORT    96)     ; Protocol family not supported
(defconstant    EAFNOSUPPORT    97)     ; Address family not supported by protocol
(defconstant    EADDRINUSE      98)     ; Address already in use
(defconstant    EADDRNOTAVAIL   99)     ; Cannot assign requested address
(defconstant    ENETDOWN        100)    ; Network is down
(defconstant    ENETUNREACH     101)    ; Network is unreachable
(defconstant    ENETRESET       102)    ; Network dropped connection because of reset
(defconstant    ECONNABORTED    103)    ; Software caused connection abort
(defconstant    ECONNRESET      104)    ; Connection reset by peer
(defconstant    ENOBUFS         105)    ; No buffer space available
(defconstant    EISCONN         106)    ; Transport endpoint is already connected
(defconstant    ENOTCONN        107)    ; Transport endpoint is not connected
(defconstant    ESHUTDOWN       108)    ; Cannot send after transport endpoint shutdown
(defconstant    ETOOMANYREFS    109)    ; Too many references: cannot splice
(defconstant    ETIMEDOUT       110)    ; Connection timed out
(defconstant    ECONNREFUSED    111)    ; Connection refused
(defconstant    EHOSTDOWN       112)    ; Host is down
(defconstant    EHOSTUNREACH    113)    ; No route to host
(defconstant    EALREADY        114)    ; Operation already in progress
(defconstant    EINPROGRESS     115)    ; Operation now in progress
(defconstant    ESTALE          116)    ; Stale NFS file handle
(defconstant    EUCLEAN         117)    ; Structure needs cleaning
(defconstant    ENOTNAM         118)    ; Not a XENIX named type file
(defconstant    ENAVAIL         119)    ; No XENIX semaphores available
(defconstant    EISNAM          120)    ; Is a named type file
(defconstant    EREMOTEIO       121)    ; Remote I/O error
(defconstant    EDQUOT          122)    ; Quota exceeded

; ------------------------------ <errno.h> ------------------------------------

(def-c-var sys_nerr (:type int) (:read-only t))
(def-c-var sys_errlist (:type (c-array c-string 122)) (:read-only t))

(def-c-var errno (:type int))

(def-c-call-out perror (:arguments (s c-string))
                       (:return-type nil)
)
(def-c-call-out strerror (:arguments (errno int))
                         (:return-type c-string :none)
)

; ============================ <sys/errno.h> ==================================

; ============================== <varargs.h> ==================================
; C compiler dependent

; ============================== <stdarg.h> ===================================
; C compiler dependent

; ============================== <stdlib.h> ===================================

(def-c-struct div_t
  (quot int)
  (rem int)
)
(def-c-struct ldiv_t
  (quot long)
  (rem long)
)

(defconstant rand-max 2147483647)

(defconstant EXIT_FAILURE 1)
(defconstant EXIT_SUCCESS 0)

(defconstant MB_CUR_MAX 1)

(def-c-call-out atof (:arguments (nptr c-string))
                     (:return-type double-float)
)
(def-c-call-out atoi (:arguments (nptr c-string))
                     (:return-type int)
)
(def-c-call-out atol (:arguments (nptr c-string))
                     (:return-type long)
)
(def-c-call-out strtod (:arguments (nptr c-string)
                                   (endptr (c-ptr c-string) :out)
                       )
                       (:return-type double-float)
)
(def-c-call-out strtol (:arguments (nptr c-string)
                                   (endptr (c-ptr c-string) :out)
                                   (base int)
                       )
                       (:return-type long)
)
(def-c-call-out strtoul (:arguments (nptr c-string)
                                    (endptr (c-ptr c-string) :out)
                                    (base int)
                        )
                        (:return-type ulong)
)

(def-c-call-out rand (:arguments)
                     (:return-type int)
)
(def-c-call-out srand (:arguments (seed uint))
                      (:return-type nil)
)

(def-c-call-out random (:arguments)
                       (:return-type long)
)
(def-c-call-out srandom (:arguments (seed uint))
                        (:return-type nil)
)
(def-c-call-out initstate (:arguments (seed uint) (statebuf c-pointer) (statelen size_t))
                          (:return-type c-pointer)
)
(def-c-call-out setstate (:arguments (statebuf c-pointer))
                         (:return-type c-pointer)
)

(def-c-call-out malloc (:arguments (size size_t))
                       (:return-type c-pointer)
)
(def-c-call-out realloc (:arguments (ptr c-pointer) (size size_t))
                        (:return-type c-pointer)
)
(def-c-call-out calloc (:arguments (nmemb size_t) (size size_t))
                       (:return-type c-pointer)
)
(def-c-call-out free (:arguments (ptr c-pointer))
                     (:return-type nil)
)
(def-c-call-out cfree (:arguments (ptr c-pointer))
                      (:return-type nil)
)
(def-c-call-out valloc (:arguments (size size_t))
                       (:return-type c-pointer)
)

(def-c-call-out abort (:arguments)
                      (:return-type nil)
)

(def-c-call-out atexit (:arguments (func (c-function)))
                       (:return-type int)
)

(def-c-call-out on_exit (:arguments (func (c-function (:arguments (status int) (arg c-pointer)))) (arg c-pointer))
                        (:return-type int)
)

(def-c-call-out exit (:arguments (status int))
                     (:return-type nil)
)

(def-c-call-out getenv (:arguments (name c-string))
                       (:return-type c-string)
)

(def-c-call-out putenv (:arguments (string c-string :in :malloc-free))
                       (:return-type int)
)

(def-c-call-out system (:arguments (command c-string))
                       (:return-type int)
)
(def-c-call-out system? (:arguments (null c-string))
                        (:return-type boolean)
                        (:name "system")
)

(def-c-type comparison_fn_t (c-function (:arguments (p1 c-pointer) (p2 c-pointer))
                                        (:return-type int)
)                           )

(def-c-call-out bsearch (:arguments (key c-pointer) (base c-pointer) (nmemb size_t) (size size_t) (compar comparison_fn_t))
                        (:return-type c-pointer)
)

(def-c-call-out qsort (:arguments (base c-pointer) (nmemb size_t) (size size_t) (compar comparison_fn_t))
                      (:return-type nil)
)

(def-c-call-out abs (:arguments (x int))
                    (:return-type int)
)
(def-c-call-out labs (:arguments (x long))
                     (:return-type long)
)

(def-c-call-out div (:arguments (numer int) (denom int))
                    (:return-type div_t)
)
(def-c-call-out ldiv (:arguments (numer long) (denom long))
                     (:return-type ldiv_t)
)

(def-c-call-out mblen (:arguments (s c-string) (n size_t))
                      (:return-type int)
)
(def-c-call-out mbtowc (:arguments (pwc (c-ptr wchar_t) :out) (s c-string) (n size_t))
                       (:return-type int)
)
;(def-c-call-out wctomb (:arguments (s (c-ptr (c-array character 10)) :out) (wchar wchar_t)) ; ??
;                       (:return-type int)
;)

;(def-c-call-out mbstowcs (:arguments (pwcs (c-ptr (c-array wchar_t)) :out) (s c-string) (n size_t)) ; ??
;                         (:return-type size_t)
;)
;(def-c-call-out wcstombs (:arguments (s c-string :out) (pwcs (c-ptr (c-array wchar_t))) (n size_t)) ; ??
;                         (:return-type size_t)
;)

(def-c-var environ (:type (c-array-ptr c-string)) (:read-only t))

;(def-c-call-out memalign (:arguments (alignment size_t) (size size_t))
;                         (:return-type c-pointer)
;) ; non-existent
(def-c-call-out valloc (:arguments (size size_t))
                       (:return-type c-pointer)
)

(def-c-call-out ecvt (:arguments (value double-float) (ndigit size_t) (decpt (c-ptr int) :out) (sign (c-ptr int) :out))
                     (:return-type c-string :none)
)
(def-c-call-out fcvt (:arguments (value double-float) (ndigit size_t) (decpt (c-ptr int) :out) (sign (c-ptr int) :out))
                     (:return-type c-string :none)
)
;(def-c-call-out gcvt (:arguments (value double-float) (ndigit size_t) (buf (c-array character) :out)) ; ??
;                (:return-type c-string)
;)
;(def-c-call-out dtoa (:arguments (d double-float) (mode int) (ndigits int) (decpt (c-ptr int) :out) (sign (c-ptr int) :out) (rve (c-array character) :out)) ; ??
;                (:return-type c-string)
;)

(def-c-call-out drand48 (:arguments)
                        (:return-type double-float)
)
(def-c-call-out erand48 (:arguments (xsubi (c-ptr (c-array ushort 3))))
                        (:return-type double-float)
)
(def-c-call-out lrand48 (:arguments)
                        (:return-type long)
)
(def-c-call-out nrand48 (:arguments (xsubi (c-ptr (c-array ushort 3))))
                        (:return-type long)
)
(def-c-call-out mrand48 (:arguments)
                        (:return-type long)
)
(def-c-call-out jrand48 (:arguments (xsubi (c-ptr (c-array ushort 3))))
                        (:return-type long)
)
(def-c-call-out srand48 (:arguments (seedval long))
                        (:return-type nil)
)
(def-c-call-out seed48 (:arguments (seed16v (c-ptr (c-array ushort 3))))
                       (:return-type (c-ptr (c-array ushort 3)) :none)
)
(def-c-call-out lcong48 (:arguments (param (c-ptr (c-array ushort 7))))
                        (:return-type nil)
)

(def-c-call-out setenv (:arguments (name c-string) (value c-string) (overwrite boolean))
                       (:return-type int)
)
(def-c-call-out unsetenv (:arguments (name c-string))
                         (:return-type int)
)

(def-c-struct qelem
  (q_forw (c-ptr qelem))
  (q_back (c-ptr qelem))
)

(def-c-call-out insque (:arguments (elem qelem) (prev qelem))
                       (:return-type nil)
)
(def-c-call-out remque (:arguments (elem qelem))
                       (:return-type nil)
)

; ============================== <ctype.h> ====================================

(def-c-call-out isalnum (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isalpha (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out iscntrl (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isdigit (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out islower (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isgraph (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isprint (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out ispunct (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isspace (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isupper (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isxdigit (:arguments (c int))
                         (:return-type boolean)
)
(def-c-call-out isblank (:arguments (c int))
                        (:return-type boolean)
)

(def-c-call-out tolower (:arguments (c int))
                        (:return-type int)
)
(def-c-call-out toupper (:arguments (c int))
                        (:return-type int)
)

(def-c-call-out isascii (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out toascii (:arguments (c int))
                        (:return-type int)
)

(def-c-call-out _tolower (:arguments (c int))
                         (:return-type int)
)
(def-c-call-out _toupper (:arguments (c int))
                         (:return-type int)
)

; ============================== <float.h> ====================================

(defconstant FLT_RADIX 2)
(defconstant FLT_MANT_DIG 24)
(defconstant FLT_DIG 6)
(defconstant FLT_ROUNDS 1)
(defconstant FLT_EPSILON 1.19209290f-07)
(defconstant FLT_MIN_EXP -125)
(defconstant FLT_MIN 1.17549435f-38)
(defconstant FLT_MIN_10_EXP -37)
(defconstant FLT_MAX_EXP 128)
(defconstant FLT_MAX 3.40282347f+38)
(defconstant FLT_MAX_10_EXP 38)

(defconstant DBL_MANT_DIG 53)
(defconstant DBL_DIG 15)
(defconstant DBL_EPSILON 2.2204460492503131d-16)
(defconstant DBL_MIN_EXP -1021)
(defconstant DBL_MIN 2.2250738585072014d-308)
(defconstant DBL_MIN_10_EXP -307)
(defconstant DBL_MAX_EXP 1024)
(defconstant DBL_MAX 1.7976931348623157d+308)
(defconstant DBL_MAX_10_EXP 308)

(defconstant LDBL_MANT_DIG 64)
(defconstant LDBL_DIG 18)
(defconstant LDBL_EPSILON 1.084202172485504434L-19) ; ??
(defconstant LDBL_MIN_EXP -16381)
(defconstant LDBL_MIN 3.3621031431120935063L-4932) ; ??
(defconstant LDBL_MIN_10_EXP -4931)
(defconstant LDBL_MAX_EXP 16384)
(defconstant LDBL_MAX 1.189731495357231765L4932) ; ??
(defconstant LDBL_MAX_10_EXP 4932)

; ============================== <limits.h> ===================================

(defconstant CHAR_BIT 8)
(defconstant MB_LEN_MAX 1)
(defconstant SCHAR_MIN -128)
(defconstant SCHAR_MAX 127)
(defconstant UCHAR_MAX 255)
(defconstant CHAR_MIN -128)
(defconstant CHAR_MAX 127)
(defconstant SHRT_MIN -32768)
(defconstant SHRT_MAX 32767)
(defconstant USHRT_MAX 65535)
(defconstant INT_MIN -2147483648)
(defconstant INT_MAX 2147483647)
(defconstant UINT_MAX 4294967295)
(defconstant LONG_MIN -2147483648)
(defconstant LONG_MAX 2147483647)
(defconstant ULONG_MAX 4294967295)
(defconstant LONG_LONG_MIN -9223372036854775808)
(defconstant LONG_LONG_MAX 9223372036854775807)
(defconstant ULONG_LONG_MAX 18446744073709551615)

; ============================== <values.h> ===================================

(defconstant CHARBITS (bitsizeof 'char))
(defconstant SHORTBITS (bitsizeof 'short))
(defconstant INTBITS (bitsizeof 'int))
(defconstant LONGBITS (bitsizeof 'long))
(defconstant PTRBITS (bitsizeof 'c-pointer))
(defconstant DOUBLEBITS (bitsizeof 'double-float))
(defconstant FLOATBITS (bitsizeof 'single-float))
(defconstant MINSHORT -32768)
(defconstant MININT -2147483648)
(defconstant MINLONG -2147483648)
(defconstant MAXSHORT 32767)
(defconstant MAXINT 2147483647)
(defconstant MAXLONG 2147483647)
(defconstant MAXDOUBLE 1.79769313486231570d+308)
(defconstant MAXFLOAT 3.40282347f+38)
(defconstant MINDOUBLE 4.94065645841246544L-324)
(defconstant MINFLOAT 1.40129846d-45)
(defconstant _IEEE 1)
(defconstant _DEXPLEN 11)
(defconstant _FEXPLEN 8)
(defconstant _HIDDENBIT 1)
(defconstant DMAXEXP (+ (1- (ash 1 _DEXPLEN)) -1 _IEEE))
(defconstant FMAXEXP (+ (1- (ash 1 _FEXPLEN)) -1 _IEEE))
(defconstant DMINEXP (- DMAXEXP))
(defconstant FMINEXP (- FMAXEXP))
(defconstant DSIGNIF (+ DOUBLEBITS (- _DEXPLEN) _HIDDENBIT -1))
(defconstant FSIGNIF (+ FLOATBITS (- _FEXPLEN) _HIDDENBIT -1))
(defconstant DMAXPOWTWO (coerce (1- (ash 1 DSIGNIF)) 'double-float))
(defconstant FMAXPOWTWO (coerce (1- (ash 1 FSIGNIF)) 'single-float))

; =============================== <math.h> ====================================

(def-c-call-out acos (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out acosh (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out asin (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out asinh (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out atan (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out atan2 (:arguments (x double-float) (y double-float))
                      (:return-type double-float)
)
(def-c-call-out atanh (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out ceil (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out cos (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out cosh (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out drem (:arguments (x double-float) (y double-float))
                     (:return-type double-float)
)
(def-c-call-out exp (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out fabs (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out floor (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out fmod (:arguments (x double-float) (y double-float))
                     (:return-type double-float)
)
(def-c-call-out hypot (:arguments (x double-float) (y double-float))
                      (:return-type double-float)
)
(def-c-call-out frexp (:arguments (x double-float) (exp (c-ptr int) :out))
                      (:return-type double-float)
)
(def-c-call-out ldexp (:arguments (x double-float) (exp int))
                      (:return-type double-float)
)
(def-c-call-out log (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out log10 (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out modf (:arguments (x double-float) (iptr (c-ptr double-float) :out))
                     (:return-type double-float)
)
(def-c-call-out pow (:arguments (x double-float) (y double-float))
                    (:return-type double-float)
)
(def-c-call-out pow2 (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out pow10 (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out sin (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out sinh (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out sqrt (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out tan (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out tanh (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out erf (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out erfc (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out j0 (:arguments (x double-float))
                   (:return-type double-float)
)
(def-c-call-out j1 (:arguments (x double-float))
                   (:return-type double-float)
)
(def-c-call-out jn (:arguments (n int) (x double-float))
                   (:return-type double-float)
)
(def-c-call-out lgamma (:arguments (x double-float))
                       (:return-type double-float)
)
(def-c-call-out y0 (:arguments (x double-float))
                   (:return-type double-float)
)
(def-c-call-out y1 (:arguments (x double-float))
                   (:return-type double-float)
)
(def-c-call-out yn (:arguments (n int) (x double-float))
                   (:return-type double-float)
)
(def-c-call-out expm1 (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out log1p (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out cbrt (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out rint (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out isinf (:arguments (x double-float))
                      (:return-type boolean)
)
(def-c-call-out isnan (:arguments (x double-float))
                      (:return-type boolean)
)
(def-c-call-out finite (:arguments (x double-float))
                       (:return-type boolean)
)
(def-c-call-out infnan (:arguments (error int))
                       (:return-type double-float)
)
(def-c-call-out copysign (:arguments (x double-float) (y double-float))
                         (:return-type double-float)
)

(defconstant HUGE DBL_MAX)
(defconstant M_E         2.7182818284590452354d0)
(defconstant M_LOG2E     1.4426950408889634074d0)
(defconstant M_LOG10E    0.43429448190325182765d0)
(defconstant M_LN2       0.69314718055994530942d0)
(defconstant M_LN10      2.30258509299404568402d0)
(defconstant M_PI        3.14159265358979323846d0)
(defconstant M_PI_2      1.57079632679489661923d0)
(defconstant M_1_PI      0.31830988618379067154d0)
(defconstant M_PI_4      0.78539816339744830962d0)
(defconstant M_2_PI      0.63661977236758134308d0)
(defconstant M_2_SQRTPI  1.12837916709551257390d0)
(defconstant M_SQRT2     1.41421356237309504880d0)
(defconstant M_SQRT1_2   0.70710678118654752440d0)
(defconstant PI M_PI)
(defconstant PI2 M_PI_2)

; ============================= <posix1_lim.h> ================================

; ----------------------------- <posix1_lim.h> --------------------------------

(defconstant _POSIX_ARG_MAX 4096)
(defconstant _POSIX_CHILD_MAX 6)
(defconstant _POSIX_LINK_MAX 8)
(defconstant _POSIX_MAX_CANON 255)
(defconstant _POSIX_MAX_INPUT 255)
(defconstant _POSIX_NGROUPS_MAX 0)
(defconstant _POSIX_OPEN_MAX 16)
(defconstant _POSIX_NAME_MAX 14)
(defconstant _POSIX_PATH_MAX 255)
(defconstant _POSIX_PIPE_BUF 512)
(defconstant _POSIX_SSIZE_MAX 32767)
(defconstant _POSIX_STREAM_MAX 8)
(defconstant _POSIX_TZNAME_MAX 3)

; ---------------------------- <linux/limits.h> -------------------------------

(eval-when (load compile eval)
(defconstant NR_OPEN 256)
(defconstant NGROUPS_MAX 32)
(defconstant ARG_MAX 131072)
(defconstant CHILD_MAX 999)
(defconstant OPEN_MAX 256)
(defconstant LINK_MAX 127)
(defconstant MAX_CANON 255)
(defconstant MAX_INPUT 255)
(defconstant NAME_MAX 255)
(defconstant PATH_MAX 1024)
(defconstant PIPE_BUF 4096)
)

; ----------------------------- <posix1_lim.h> --------------------------------

(defconstant TZNAME_MAX 50)
(defconstant SSIZE_MAX INT_MAX)
(defconstant STREAM_MAX OPEN_MAX)

; ============================= <posix2_lim.h> ================================

(defconstant _POSIX2_BC_BASE_MAX 99)
(defconstant _POSIX2_BC_DIM_MAX 2048)
(defconstant _POSIX2_BC_SCALE_MAX 99)
(defconstant _POSIX2_BC_STRING_MAX 1000)
(defconstant _POSIX2_EQUIV_CLASS_MAX 2)
(defconstant _POSIX2_EXPR_NEST_MAX 32)
(defconstant _POSIX2_LINE_MAX 2048)
(defconstant _POSIX2_RE_DUP_MAX 255)

(defconstant BC_BASE_MAX _POSIX2_BC_BASE_MAX)
(defconstant BC_DIM_MAX _POSIX2_BC_DIM_MAX)
(defconstant BC_SCALE_MAX _POSIX2_BC_SCALE_MAX)
(defconstant BC_STRING_MAX _POSIX2_BC_STRING_MAX)
(defconstant EQUIV_CLASS_MAX _POSIX2_EQUIV_CLASS_MAX)
(defconstant EXPR_NEST_MAX _POSIX2_EXPR_NEST_MAX)
(defconstant LINE_MAX _POSIX2_LINE_MAX)
(defconstant RE_DUP_MAX 32767)

; ============================= <posix_opt.h> =================================

(defconstant _POSIX_CHOWN_RESTRICTED t)
(defconstant _POSIX_NO_TRUNC t)
(defconstant _POSIX_VDISABLE #\Null)
(defconstant _POSIX_JOB_CONTROL t)
(defconstant _POSIX_SAVED_IDS t)

; ============================== <unistd.h> ===================================

(defconstant _POSIX_VERSION 199009)
(defconstant _POSIX2_C_VERSION 199912)
(defconstant _POSIX2_C_BIND t)
(defconstant _POSIX2_C_DEV t)
(defconstant _POSIX2_SW_DEV t)

(defconstant STDIN_FILENO 0)
(defconstant STDOUT_FILENO 1)
(defconstant STDERR_FILENO 2)

(defconstant R_OK 4)
(defconstant W_OK 2)
(defconstant X_OK 1)
(defconstant F_OK 0)

(def-c-call-out access (:arguments (name c-string) (type int))
                       (:return-type int)
)

(defconstant SEEK_SET 0)
(defconstant SEEK_CUR 1)
(defconstant SEEK_END 2)

(def-c-call-out lseek (:arguments (fd int) (offset off_t) (whence int))
                      (:return-type off_t)
)

(def-c-call-out close (:arguments (fd int))
                      (:return-type int)
)

(def-c-call-out read (:arguments (fd int) (buf c-pointer) (nbytes size_t))
                     (:return-type ssize_t)
                     (:name "full_read")
)
(def-c-call-out write (:arguments (fd int) (buf c-pointer) (nbytes size_t))
                      (:return-type ssize_t)
                      (:name "full_write")
)

(def-c-call-out pipe (:arguments (pipedes (c-ptr (c-array int 2)) :out))
                     (:return-type int)
)

(def-c-call-out alarm (:arguments (seconds uint))
                      (:return-type uint)
)

(def-c-call-out sleep (:arguments (seconds uint))
                      (:return-type uint)
)

(def-c-call-out pause (:arguments)
                      (:return-type int)
)

(def-c-call-out chown (:arguments (file c-string) (owner uid_t) (group gid_t))
                      (:return-type int)
)
(def-c-call-out fchown (:arguments (fd int) (owner uid_t) (group gid_t))
                       (:return-type int)
)

(def-c-call-out chdir (:arguments (path c-string))
                      (:return-type int)
)

;(def-c-call-out getcwd (:arguments (buf c-string :out) (size size_t)) ; ??
;                       (:return-type c-string)
;)

(def-c-call-out get_current_dir_name (:arguments)
                                     (:return-type c-string :malloc-free)
)

;(def-c-call-out getwd (:arguments (buf c-string :out)) ; ??
;                      (:return-type c-string)
;)

(def-c-call-out dup (:arguments (fd int))
                    (:return-type int)
)

(def-c-call-out dup2 (:arguments (fd int) (fd2 int))
                     (:return-type int)
)

(def-c-var environ (:type (c-array-ptr c-string)) (:read-only t))

;(def-c-call-out execve (:arguments (path c-string) (argv c-pointer) (envp c-pointer)) ; ??
;                       (:return-type int)
;)
;(def-c-call-out execv (:arguments (path c-string) (argv c-pointer)) ; ??
;                      (:return-type int)
;)
(def-c-call-out execle0 (:arguments (path c-string) (argv0 c-string) (null c-string) (envp c-pointer))
                        (:return-type int)
                        (:name "execle")
)
(def-c-call-out execle1 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (null c-string) (envp c-pointer))
                        (:return-type int)
                        (:name "execle")
)
(def-c-call-out execle2 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (null c-string) (envp c-pointer))
                        (:return-type int)
                        (:name "execle")
)
(def-c-call-out execle3 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (argv3 c-string) (null c-string) (envp c-pointer))
                        (:return-type int)
                        (:name "execle")
)
(def-c-call-out execl0 (:arguments (path c-string) (argv0 c-string) (null c-string))
                       (:return-type int)
                       (:name "execl")
)
(def-c-call-out execl1 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (null c-string))
                       (:return-type int)
                       (:name "execl")
)
(def-c-call-out execl2 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (null c-string))
                       (:return-type int)
                       (:name "execl")
)
(def-c-call-out execl3 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (argv3 c-string) (null c-string))
                       (:return-type int)
                       (:name "execl")
)
;(def-c-call-out execvp (:arguments (path c-string) (argv c-pointer)) ; ??
;                       (:return-type int)
;)
(def-c-call-out execlp0 (:arguments (path c-string) (argv0 c-string) (null c-string))
                        (:return-type int)
                        (:name "execlp")
)
(def-c-call-out execlp1 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (null c-string))
                        (:return-type int)
                        (:name "execlp")
)
(def-c-call-out execlp2 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (null c-string))
                        (:return-type int)
                        (:name "execlp")
)
(def-c-call-out execlp3 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (argv3 c-string) (null c-string))
                        (:return-type int)
                        (:name "execlp")
)

(def-c-call-out _exit (:arguments (status int))
                      (:return-type nil)
)

(defconstant _PC_LINK_MAX            0)
(defconstant _PC_MAX_CANON           1)
(defconstant _PC_MAX_INPUT           2)
(defconstant _PC_NAME_MAX            3)
(defconstant _PC_PATH_MAX            4)
(defconstant _PC_PIPE_BUF            5)
(defconstant _PC_CHOWN_RESTRICTED    6)
(defconstant _PC_NO_TRUNC            7)
(defconstant _PC_VDISABLE            8)
(def-c-call-out pathconf (:arguments (path c-string) (name int))
                         (:return-type long)
)
(def-c-call-out fpathconf (:arguments (fd int) (name int))
                          (:return-type long)
)

(def-c-enum sysconf-name
  _SC_ARG_MAX
  _SC_CHILD_MAX
  _SC_CLK_TCK
  _SC_NGROUPS_MAX
  _SC_OPEN_MAX
  _SC_STREAM_MAX
  _SC_TZNAME_MAX
  _SC_JOB_CONTROL
  _SC_SAVED_IDS
  _SC_VERSION
  _SC_CLOCKS_PER_SEC
  _SC_BC_BASE_MAX
  _SC_BC_DIM_MAX
  _SC_BC_SCALE_MAX
  _SC_BC_STRING_MAX
  _SC_COLL_WEIGHTS_MAX
  _SC_EQUIV_CLASS_MAX
  _SC_EXPR_NEST_MAX
  _SC_LINE_MAX
  _SC_RE_DUP_MAX
  _SC_2_VERSION
  _SC_2_C_BIND
  _SC_2_C_DEV
  _SC_2_FORT_DEV
  _SC_2_FORT_RUN
  _SC_2_SW_DEV
  _SC_2_LOCALEDEF
)
(def-c-call-out sysconf (:arguments (name int))
                        (:return-type long)
)

(defconstant _CS_PATH 0)
;(def-c-call-out confstr (:arguments (name int) (buf c-pointer) (len size_t)) ; ??
;                        (:return-type size_t)
;)

(def-c-call-out getpid (:arguments)
                       (:return-type pid_t)
)

(def-c-call-out getppid (:arguments)
                        (:return-type pid_t)
)

;(def-c-call-out getpgid (:arguments (pid pid_t))
;                        (:return-type pid_t)
;) ; non-existent

(def-c-call-out getpgrp (:arguments)
                        (:return-type pid_t)
)

(def-c-call-out setpgid (:arguments (pid pid_t) (pgid pid_t))
                        (:return-type int)
)

(def-c-call-out setpgrp (:arguments)
                        (:return-type int)
)

(def-c-call-out setsid (:arguments)
                       (:return-type pid_t)
)

(def-c-call-out getuid (:arguments)
                       (:return-type uid_t)
)

(def-c-call-out geteuid (:arguments)
                        (:return-type uid_t)
)

(def-c-call-out getgid (:arguments)
                       (:return-type gid_t)
)

(def-c-call-out getegid (:arguments)
                        (:return-type gid_t)
)

;(def-c-call-out getgroups (:arguments (size int) (list (c-ptr (c-array gid_t ??)) :out)) ; ??
;                          (:return-type int)
;)

(def-c-call-out setuid (:arguments (uid uid_t))
                       (:return-type int)
)

(def-c-call-out setreuid (:arguments (ruid uid_t) (euid uid_t))
                         (:return-type int)
)

(def-c-call-out seteuid (:arguments (uid uid_t))
                        (:return-type int)
)

(def-c-call-out setgid (:arguments (gid gid_t))
                       (:return-type int)
)

(def-c-call-out setregid (:arguments (rgid gid_t) (egid gid_t))
                         (:return-type int)
)

(def-c-call-out setegid (:arguments (gid gid_t))
                        (:return-type int)
)

(def-c-call-out cuserid (:arguments (null c-string)) ; ??
                        (:return-type c-string)
)

(def-c-call-out ctermid (:arguments (null c-string)) ; ??
                        (:return-type c-string)
)

(def-c-call-out ttyname (:arguments (fd int))
                        (:return-type c-string)
)

(def-c-call-out isatty (:arguments (fd int))
                       (:return-type boolean)
)

(def-c-call-out link (:arguments (from c-string) (to c-string))
                     (:return-type int)
)

(def-c-call-out symlink (:arguments (from c-string) (to c-string))
                        (:return-type int)
)

;(def-c-call-out readlink (:arguments (path c-string) (buf c-pointer) (len size_t)) ; ??
;                         (:return-type int)
;)

(def-c-call-out unlink (:arguments (path c-string))
                       (:return-type int)
)

(def-c-call-out rmdir (:arguments (path c-string))
                      (:return-type int)
)

(def-c-call-out tcgetpgrp (:arguments (fd int))
                          (:return-type pid_t)
)

(def-c-call-out tcsetpgrp (:arguments (fd int) (pgrp_id pid_t))
                          (:return-type int)
)

(def-c-call-out getlogin (:arguments)
                         (:return-type c-string)
)

;(def-c-call-out setlogin (:arguments (name c-string))
;                         (:return-type int)
;) ; non-existent

(def-c-call-out getopt (:arguments (argc int) (argv c-pointer) (opts c-string)) ; ??
                       (:return-type int)
)
(def-c-var opterr (:type int))
(def-c-var optind (:type int))
(def-c-var optopt (:type int))
(def-c-var optarg (:type c-string))

;(def-c-call-out gethostname (:arguments (name c-pointer) (len size_t)) ; ??
;                            (:return-type int)
;)

;(def-c-call-out sethostname (:arguments (name (c-pointer) (len sizet)) ; ??
;                            (:return-type int)
;)

(def-c-call-out gethostid (:arguments)
                          (:return-type long)
)

(def-c-call-out sethostid (:arguments (id long))
                          (:return-type int)
)

(def-c-call-out getpagesize (:arguments)
                            (:return-type size_t)
)

(def-c-call-out getdtablesize (:arguments)
                              (:return-type int)
)

(def-c-call-out fsync (:arguments (fd int))
                      (:return-type int)
)

(def-c-call-out sync (:arguments)
                     (:return-type int)
)

(def-c-call-out vhangup (:arguments)
                        (:return-type int)
)

(def-c-call-out acct (:arguments (name c-string))
                     (:return-type int)
)

(def-c-call-out chroot (:arguments (path c-string))
                       (:return-type int)
)

(def-c-call-out swapon (:arguments (path c-string))
                       (:return-type int)
)

(def-c-call-out getusershell (:arguments)
                             (:return-type c-string)
)
(def-c-call-out setusershell (:arguments)
                             (:return-type nil)
)
(def-c-call-out endusershell (:arguments)
                             (:return-type nil)
)

(def-c-call-out getpass (:arguments (prompt c-string))
                        (:return-type c-string)
)

(def-c-call-out brk (:arguments (end_data_segment c-pointer))
                    (:return-type int)
)
(def-c-call-out sbrk (:arguments (increment ptrdiff_t))
                     (:return-type int)
)

(def-c-call-out crypt (:arguments (key c-string) (salt c-string))
                      (:return-type c-string)
)
(def-c-call-out encrypt (:arguments (block (c-ptr (c-array character 64))) (edflag boolean))
                        (:return-type nil)
)
(def-c-call-out setkey (:arguments (key c-string))
                       (:return-type nil)
)

(def-c-call-out ftruncate (:arguments (fildes int) (length size_t))
                          (:return-type int)
)
(def-c-call-out truncate (:arguments (path c-string) (length size_t))
                         (:return-type int)
)

(def-c-call-out ioperm (:arguments (from ulong) (num ulong) (turn_on boolean))
                       (:return-type int)
)
(def-c-call-out iopl (:arguments (level int))
                     (:return-type int)
)

(def-c-call-out mktemp (:arguments (template c-string :in :alloca)) ; actually :in-out
                       (:return-type c-string)
)
;(def-c-call-out mkstemp (:arguments (template c-string :in-out)); ??
;                        (:return-type int)
;)

(def-c-call-out nice (:arguments (val int))
                     (:return-type int)
)

;(def-c-call-out profil (:arguments (buf c-pointer) (bufsiz int) (offset int) (scale int)) ; ??
;                       (:return-type int)
;)

(def-c-call-out usleep (:arguments (usec ulong))
                       (:return-type nil)
)

#| ;; Present in libc-4, but not libc-5.
(def-c-call-out ustat (:arguments (dev dev_t) (ubuf (c-ptr ustat) :out))
                      (:return-type int)
)
|#

(def-c-call-out idle (:arguments)
                     (:return-type int)
)
(def-c-call-out reboot (:arguments (magic int) (magic_too int) (flag int))
                       (:return-type int)
)

(def-c-call-out swapoff (:arguments (specialfile c-string))
                        (:return-type int)
)

(def-c-call-out uselib (:arguments (filename c-string))
                       (:return-type int)
)

;(def-c-call-out getdomainname (:arguments (name c-pointer) (len size_t)) ; ??
;                              (:return-type int)
;)
;(def-c-call-out setdomainname (:arguments (name c-pointer) (len size_t)) ; ??
;                              (:return-type int)
;)

;(def-c-call-out realpath (:arguments (path c-pointer) (resolved_path c-pointer :out)) ; ??
;                         (:return-type int)
;)

; ============================== <fcntl.h> ====================================

; ---------------------------- <linux/fcntl.h> --------------------------------

(defconstant O_ACCMODE    #o003)
(defconstant O_RDONLY       #o0)
(defconstant O_WRONLY       #o1)
(defconstant O_RDWR         #o2)
(defconstant O_CREAT      #o100)
(defconstant O_EXCL       #o200)
(defconstant O_NOCTTY     #o400)
(defconstant O_TRUNC     #o1000)
(defconstant O_APPEND    #o2000)
(defconstant O_NONBLOCK  #o4000)
(defconstant O_NDELAY    O_NONBLOCK)
(defconstant O_SYNC     #o10000)
(defconstant FASYNC     #o20000)
(defconstant F_DUPFD    0)
(defconstant F_GETFD    1)
(defconstant F_SETFD    2)
(defconstant F_GETFL    3)
(defconstant F_SETFL    4)
(defconstant F_GETLK    5)
(defconstant F_SETLK    6)
(defconstant F_SETLKW   7)
(defconstant F_SETOWN   8)
(defconstant F_GETOWN   9)

(defconstant FD_CLOEXEC 1)

(defconstant F_RDLCK    0)
(defconstant F_WRLCK    1)
(defconstant F_UNLCK    2)

(defconstant F_EXLCK    4)
(defconstant F_SHLCK    8)

(def-c-struct flock
  (l_type short)
  (l_whence short)
  (l_start off_t)
  (l_len off_t)
  (l_pid pid_t)
)

; ------------------------------ <fcntl.h> ------------------------------------

(defconstant FNDELAY O_NDELAY)

(def-c-call-out creat (:arguments (filename c-string) (mode mode_t))
                      (:return-type int)
)

(def-c-call-out fcntl (:arguments (fildes int) (cmd int) (arg c-pointer)) ; ??
                      (:return-type int)
)

(def-c-call-out open (:arguments (filename c-string) (flags int) (mode mode_t))
                     (:return-type int)
)

; ============================= <sys/stat.h> ==================================

(c-lines "#include <sys/stat.h>~%")

; ---------------------------- <linux/stat.h> ---------------------------------

(defconstant S_IFMT  #o0170000)
(defconstant S_IFSOCK #o140000)
(defconstant S_IFLNK  #o120000)
(defconstant S_IFREG  #o100000)
(defconstant S_IFBLK  #o060000)
(defconstant S_IFDIR  #o040000)
(defconstant S_IFCHR  #o020000)
(defconstant S_IFIFO  #o010000)
(defconstant S_ISUID  #o004000)
(defconstant S_ISGID  #o002000)
(defconstant S_ISVTX  #o001000)

(defmacro S_ISLNK (m) `(= (logand ,m S_IFMT) S_IFLNK))
(defmacro S_ISREG (m) `(= (logand ,m S_IFMT) S_IFREG))
(defmacro S_ISDIR (m) `(= (logand ,m S_IFMT) S_IFDIR))
(defmacro S_ISCHR (m) `(= (logand ,m S_IFMT) S_IFCHR))
(defmacro S_ISBLK (m) `(= (logand ,m S_IFMT) S_IFBLK))
(defmacro S_ISFIFO (m) `(= (logand ,m S_IFMT) S_IFFIFO))
(defmacro S_ISSOCK (m) `(= (logand ,m S_IFMT) S_IFSOCK))

(defconstant S_IRWXU #o0700)
(defconstant S_IRUSR #o0400)
(defconstant S_IWUSR #o0200)
(defconstant S_IXUSR #o0100)

(defconstant S_IRWXG #o0070)
(defconstant S_IRGRP #o0040)
(defconstant S_IWGRP #o0020)
(defconstant S_IXGRP #o0010)

(defconstant S_IRWXO #o0007)
(defconstant S_IROTH #o0004)
(defconstant S_IWOTH #o0002)
(defconstant S_IXOTH #o0001)

; ----------------------------- <sys/stat.h> ----------------------------------

(def-c-struct stat
  (st_dev dev_t)
  (__pad1 ushort)
  (st_ino ino_t)
  (st_mode umode_t)
  (st_nlink nlink_t)
  (st_uid uid_t)
  (st_gid gid_t)
  (st_rdev dev_t)
  (__pad2 ushort)
  (st_size off_t)
  (st_blksize ulong)
  (st_blocks ulong)
  (st_atime time_t)
  (__unused1 ulong)
  (st_mtime time_t)
  (__unused2 ulong)
  (st_ctime time_t)
  (__unused3 ulong)
  (__unused4 ulong)
  (__unused5 ulong)
)

(defconstant S_IREAD S_IRUSR)
(defconstant S_IWRITE S_IWUSR)
(defconstant S_IEXEC S_IXUSR)

(def-c-call-out chmod (:arguments (path c-string) (mode mode_t))
                      (:return-type int)
)

(def-c-call-out fchmod (:arguments (fildes int) (mode mode_t))
                       (:return-type int)
)

(def-c-call-out fstat (:arguments (fildes int) (stat_buf (c-ptr stat) :out))
                      (:return-type int)
)

(def-c-call-out mkdir (:arguments (path c-string) (mode mode_t))
                      (:return-type int)
)

(def-c-call-out mknod (:arguments (path c-string) (mode mode_t) (dev dev_t))
                      (:return-type int)
)

(def-c-call-out mkfifo (:arguments (path c-string) (mode mode_t))
                       (:return-type int)
)

(def-c-call-out stat (:arguments (filename c-string) (stat_buf (c-ptr stat) :out))
                     (:return-type int)
)

(def-c-call-out lstat (:arguments (filename c-string) (stat_buf (c-ptr stat) :out))
                      (:return-type int)
)

(def-c-call-out umask (:arguments (mask mode_t))
                      (:return-type mode_t)
)

; ============================== <stdio.h> ====================================

(defconstant EOF -1)

(defconstant _IOFBF 0)
(defconstant _IOLBF 1)
(defconstant _IONBF 2)

(defconstant SEEK_SET 0)
(defconstant SEEK_CUR 1)
(defconstant SEEK_END 2)

(def-c-type FILE
  (c-struct vector
    ; components unknown
) )
(def-c-type fpos_t _G_fpos_t)

(defconstant FOPEN_MAX _G_FOPEN_MAX)
(defconstant FILENAME_MAX _G_FILENAME_MAX)

(defconstant TMP_MAX 238328)

(defconstant L_ctermid     9)
(defconstant L_cuserid     9)
(defconstant P_tmpdir      "/tmp")
(defconstant L_tmpnam      20)

(def-c-call-out clearerr (:arguments (fp c-pointer))
                         (:return-type nil)
)
(def-c-call-out fclose (:arguments (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out feof (:arguments (fp c-pointer))
                     (:return-type int)
)
(def-c-call-out ferror (:arguments (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out fflush (:arguments (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out fgetc (:arguments (fp c-pointer))
                      (:return-type int)
)
(def-c-call-out fgetpos (:arguments (fp c-pointer) (pos (c-ptr fpos_t) :out))
                        (:return-type int)
)
;(def-c-call-out fgets (:arguments (buf c-pointer) (size int) (fp c-pointer)) ; ??
;                      (:return-type c-string)
;)
(def-c-call-out fopen (:arguments (path c-string) (mode c-string))
                      (:return-type c-pointer)
)
(def-c-call-out fprintf0 (:arguments (fp c-pointer) (format c-string))
                         (:return-type int)
                         (:name "fprintf")
)
(def-c-call-out fprintf1i (:arguments (fp c-pointer) (format c-string) (arg int))
                          (:return-type int)
                          (:name "fprintf")
)
(def-c-call-out fprintf1l (:arguments (fp c-pointer) (format c-string) (arg long))
                          (:return-type int)
                          (:name "fprintf")
)
(def-c-call-out fprintf1d (:arguments (fp c-pointer) (format c-string) (arg double-float))
                          (:return-type int)
                          (:name "fprintf")
)
(def-c-call-out fputc (:arguments (c int) (fp c-pointer))
                      (:return-type int)
)
(def-c-call-out fputs (:arguments (str c-string) (fp c-pointer))
                      (:return-type int)
)
;(def-c-call-out fread (:arguments (ptr c-pointer) (size size_t) (nmemb size_t) (fp c-pointer)) ; ??
;                      (:return-type size_t)
;)
;(def-c-call-out freopen (:arguments (path c-string) (mode c-string) (fp c-pointer :in-out)) ; ??
;                        (:return-type c-pointer)
;)
(def-c-call-out fscanf0 (:arguments (fp c-pointer) (format c-string))
                        (:return-type int)
                        (:name "fscanf")
)
(def-c-call-out fscanf1i (:arguments (fp c-pointer) (format c-string) (arg (c-ptr int) :out))
                         (:return-type int)
                         (:name "fscanf")
)
(def-c-call-out fscanf1l (:arguments (fp c-pointer) (format c-string) (arg (c-ptr long) :out))
                         (:return-type int)
                         (:name "fscanf")
)
(def-c-call-out fscanf1d (:arguments (fp c-pointer) (format c-string) (arg (c-ptr double-float) :out))
                         (:return-type int)
                         (:name "fscanf")
)
(def-c-call-out fseek (:arguments (fp c-pointer) (offset long) (whence int))
                      (:return-type int)
)
(def-c-call-out fsetpos (:arguments (fp c-pointer) (pos (c-ptr fpos_t)))
                        (:return-type int)
)
(def-c-call-out ftell (:arguments (fp c-pointer))
                      (:return-type long)
)
;(def-c-call-out fwrite (:arguments (ptr c-pointer) (size size_t) (nmemb size_t) (fp c-pointer)) ; ??
;                       (:return-type size_t)
;)
(def-c-call-out getc (:arguments (fp c-pointer))
                     (:return-type int)
)
(def-c-call-out getchar (:arguments)
                        (:return-type int)
)
;(def-c-call-out gets (:arguments (buf c-pointer)) ; ??
;                     (:return-type c-string)
;)
(def-c-call-out perror (:arguments (s c-string))
                       (:return-type nil)
)
(def-c-call-out printf0 (:arguments (format c-string))
                        (:return-type int)
                        (:name "printf")
)
(def-c-call-out printf1i (:arguments (format c-string) (arg int))
                         (:return-type int)
                         (:name "printf")
)
(def-c-call-out printf1l (:arguments (format c-string) (arg long))
                         (:return-type int)
                         (:name "printf")
)
(def-c-call-out printf1d (:arguments (format c-string) (arg double-float))
                         (:return-type int)
                         (:name "printf")
)
(def-c-call-out putc (:arguments (c int) (fp c-pointer))
                     (:return-type int)
)
(def-c-call-out putchar (:arguments (c int))
                        (:return-type int)
)
(def-c-call-out puts (:arguments (str c-string))
                     (:return-type int)
)
(def-c-call-out remove (:arguments (path c-string))
                       (:return-type int)
)
(def-c-call-out rename (:arguments (old c-string) (new c-string))
                       (:return-type int)
)
(def-c-call-out rewind (:arguments (fp c-pointer))
                       (:return-type nil)
)
(def-c-call-out scanf0 (:arguments (format c-string))
                       (:return-type int)
                       (:name "scanf")
)
(def-c-call-out scanf1i (:arguments (format c-string) (arg (c-ptr int) :out))
                        (:return-type int)
                        (:name "scanf")
)
(def-c-call-out scanf1l (:arguments (format c-string) (arg (c-ptr long) :out))
                        (:return-type int)
                        (:name "scanf")
)
(def-c-call-out scanf1d (:arguments (format c-string) (arg (c-ptr double-float) :out))
                        (:return-type int)
                        (:name "scanf")
)
(def-c-call-out setbuf (:arguments (fp c-pointer) (buf c-pointer))
                       (:return-type nil)
)
(def-c-call-out setlinebuf (:arguments (fp c-pointer))
                           (:return-type nil)
)
(def-c-call-out setbuffer (:arguments (fp c-pointer) (buf c-pointer) (size int))
                          (:return-type nil)
)
(def-c-call-out setvbuf (:arguments (fp c-pointer) (buf c-pointer) (mode int) (size size_t))
                        (:return-type int)
)
;(def-c-call-out sprintf0 (:arguments (str c-pointer :out) (format c-string)) ; ??
;                         (:return-type int)
;                         (:name "sprintf")
;)
(def-c-call-out sscanf0 (:arguments (str c-string) (format c-string))
                        (:return-type int)
                        (:name "sscanf")
)
(def-c-call-out sscanf1i (:arguments (str c-string) (format c-string) (arg (c-ptr int) :out))
                         (:return-type int)
                         (:name "sscanf")
)
(def-c-call-out sscanf1l (:arguments (str c-string) (format c-string) (arg (c-ptr long) :out))
                         (:return-type int)
                         (:name "sscanf")
)
(def-c-call-out sscanf1d (:arguments (str c-string) (format c-string) (arg (c-ptr double-float) :out))
                         (:return-type int)
                         (:name "sscanf")
)
(def-c-call-out tmpfile (:arguments)
                        (:return-type c-pointer)
)
(def-c-call-out tmpnam (:arguments (s c-string :in :alloca)) ; :in-out ??
                       (:return-type c-string)
)
(def-c-call-out ungetc (:arguments (c int) (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out fdopen (:arguments (fildes int) (mode c-string))
                       (:return-type c-pointer)
)
(def-c-call-out fileno (:arguments (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out popen (:arguments (command c-string) (mode c-string))
                      (:return-type c-pointer)
)
(def-c-call-out pclose (:arguments (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out getw (:arguments (fp c-pointer))
                     (:return-type int)
)
(def-c-call-out putw (:arguments (w int) (fp c-pointer))
                     (:return-type int)
)
(def-c-call-out tempnam (:arguments (dir c-string) (prefix c-string))
                        (:return-type c-string :malloc-free)
)

; ============================== <dirent.h> ===================================

; --------------------------- <linux/dirent.h> --------------------------------

(def-c-struct dirent
  (d_ino long)
  (d_off off_t)
  (d_reclen ushort)
  (d_name (c-array-max character #.(lisp:+ NAME_MAX 1)))
)

; ------------------------------ <dirent.h> -----------------------------------

(def-c-struct DIR
  (dd_fd int)
  (dd_loc int)
  (dd_size int)
  (dd_buf (c-ptr dirent))
)

(def-c-call-out opendir (:arguments (name c-string))
                        (:return-type c-pointer)
)

(def-c-call-out closedir (:arguments (dirp c-pointer))
                         (:return-type int)
)

(def-c-call-out readdir (:arguments (dirp c-pointer))
                        (:return-type (c-ptr dirent))
)

(def-c-call-out rewinddir (:arguments (dirp c-pointer))
                          (:return-type nil)
)

(def-c-call-out seekdir (:arguments (dirp c-pointer) (pos off_t))
                        (:return-type nil)
)

(def-c-call-out telldir (:arguments (dirp c-pointer))
                        (:return-type off_t)
)

(def-c-call-out scandir (:arguments (dir c-string)
                                    (namelist (c-ptr (c-ptr (c-ptr dirent))) :out)
                                    (select (c-function (:arguments (d c-string))
                                                        (:return-type boolean)
                                    )       )
                                    (compar (c-function (:arguments (d1 (c-ptr (c-ptr dirent))) (d2 (c-ptr (c-ptr dirent))))
                                                        (:return-type int)
                                    )       )
                        )
                        (:return-type int)
)

(def-c-call-out alphasort (:arguments (d1 (c-ptr (c-ptr dirent))) (d2 (c-ptr (c-ptr dirent))))
                          (:return-type int)
)

;(def-c-call-out getdirentries (:arguments (fd int) (buf c-pointer) (nbytes size_t) (basep (c-ptr off_t) :in-out))
;                              (:return-type ssize_t)
;)

; ================================ <pwd.h> ====================================

(def-c-struct passwd
  (pw_name c-string)
  (pw_passwd c-string)
  (pw_uid uid_t)
  (pw_gid gid_t)
  (pw_gecos c-string)
  (pw_dir c-string)
  (pw_shell c-string)
)

(def-c-call-out __pwdopen (:arguments)
                          (:return-type c-pointer)
)

;(def-c-call-out __pwdread (:arguments (stream c-pointer) (p c-pointer))
;                          (:return-type (c-ptr passwd))
;)

(def-c-call-out __pwdalloc (:arguments)
                           (:return-type c-pointer)
)

(def-c-call-out setpwent (:arguments)
                         (:return-type nil)
)

(def-c-call-out endpwent (:arguments)
                         (:return-type nil)
)

(def-c-call-out getpwent (:arguments)
                         (:return-type (c-ptr passwd))
)

;(def-c-call-out getpw (:arguments (uid uid_t) (buf c-pointer))
;                      (:return-type int)
;)

(def-c-call-out fgetpwent (:arguments (stream c-pointer))
                          (:return-type (c-ptr passwd))
)

(def-c-call-out putpwent (:arguments (c-ptr passwd) (stream c-pointer))
                         (:return-type int)
)

(def-c-call-out getpwuid (:arguments (uid uid_t))
                         (:return-type (c-ptr passwd))
)

(def-c-call-out getpwnam (:arguments (name c-string))
                         (:return-type (c-ptr passwd))
)

; ================================ <grp.h> ====================================

(def-c-struct group
  (gr_name c-string)
  (gr_passwd c-string)
  (gr_gid gid_t)
  (gr_mem (c-ptr c-string)) ; ??
)

(def-c-call-out __grpopen (:arguments)
                          (:return-type c-pointer)
)

;(def-c-call-out __grpread (:arguments (stream c-pointer) (p c-pointer))
;                          (:return-type (c-ptr group))
;)

(def-c-call-out __grpalloc (:arguments)
                           (:return-type c-pointer)
)

(def-c-call-out setgrent (:arguments)
                         (:return-type nil)
)

(def-c-call-out endgrent (:arguments)
                         (:return-type nil)
)

(def-c-call-out getgrent (:arguments)
                         (:return-type (c-ptr group))
)

(def-c-call-out fgetgrent (:arguments (stream c-pointer))
                          (:return-type (c-ptr group))
)

(def-c-call-out getgrgid (:arguments (gid gid_t))
                         (:return-type (c-ptr group))
)

(def-c-call-out getgrnam (:arguments (name c-string))
                         (:return-type (c-ptr group))
)

;(def-c-call-out setgroups (:arguments (n size_t) (groups (c-ptr gid_t)))
;                          (:return-type int)
;)

(def-c-call-out initgroups (:arguments (user c-string) (group gid_t))
                           (:return-type int)
)

; ============================ <sys/utsname.h> ================================

(def-c-struct utsname
  (sysname (c-array-max character 65))
  (nodename (c-array-max character 65))
  (release (c-array-max character 65))
  (version (c-array-max character 65))
  (machine (c-array-max character 65))
  (domainname (c-array-max character 65))
)

(def-c-call-out uname (:arguments (utsbuf (c-ptr utsname) :out))
                      (:return-type int)
)

; ============================= <termios.h> ===================================

; --------------------------- <linux/termios.h> -------------------------------

(defconstant TCGETS     #x5401)
(defconstant TCSETS     #x5402)
(defconstant TCSETSW    #x5403)
(defconstant TCSETSF    #x5404)
(defconstant TCGETA     #x5405)
(defconstant TCSETA     #x5406)
(defconstant TCSETAW    #x5407)
(defconstant TCSETAF    #x5408)
(defconstant TCSBRK     #x5409)
(defconstant TCXONC     #x540A)
(defconstant TCFLSH     #x540B)
(defconstant TIOCEXCL   #x540C)
(defconstant TIOCNXCL   #x540D)
(defconstant TIOCSCTTY  #x540E)
(defconstant TIOCGPGRP  #x540F)
(defconstant TIOCSPGRP   #x5410)
(defconstant TIOCOUTQ    #x5411)
(defconstant TIOCSTI     #x5412)
(defconstant TIOCGWINSZ  #x5413)
(defconstant TIOCSWINSZ  #x5414)
(defconstant TIOCMGET    #x5415)
(defconstant TIOCMBIS    #x5416)
(defconstant TIOCMBIC    #x5417)
(defconstant TIOCMSET    #x5418)
(defconstant TIOCGSOFTCAR #x5419)
(defconstant TIOCSSOFTCAR #x541A)
(defconstant FIONREAD    #x541B)
(defconstant TIOCINQ     FIONREAD)
(defconstant TIOCLINUX   #x541C)
(defconstant TIOCCONS    #x541D)
(defconstant TIOCGSERIAL #x541E)
(defconstant TIOCSSERIAL #x541F)
(defconstant TIOCPKT     #x5420)
(defconstant FIONBIO     #x5421)
(defconstant TIOCNOTTY   #x5422)
(defconstant TIOCSETD    #x5423)
(defconstant TIOCGETD    #x5424)
(defconstant TCSBRKP     #x5425)
(defconstant TIOCTTYGSTRUCT #x5426)
(defconstant FIONCLEX    #x5450)
(defconstant FIOCLEX     #x5451)
(defconstant FIOASYNC    #x5452)
(defconstant TIOCSERCONFIG #x5453)
(defconstant TIOCSERGWILD  #x5454)
(defconstant TIOCSERSWILD  #x5455)
(defconstant TIOCGLCKTRMIOS #x5456)
(defconstant TIOCSLCKTRMIOS #x5457)
(defconstant TIOCSERGSTRUCT #x5458)
(defconstant TIOCSERGETLSR  #x5459)
(defconstant TIOCSERGETMULTI #x545A)
(defconstant TIOCSERSETMULTI #x545B)

(defconstant TIOCPKT_DATA       0)
(defconstant TIOCPKT_FLUSHREAD  1)
(defconstant TIOCPKT_FLUSHWRITE 2)
(defconstant TIOCPKT_STOP       4)
(defconstant TIOCPKT_START      8)
(defconstant TIOCPKT_NOSTOP    16)
(defconstant TIOCPKT_DOSTOP    32)

(def-c-struct winsize
  (ws_row ushort)
  (ws_col ushort)
  (ws_xpixel ushort)
  (ws_ypixel ushort)
)

(eval-when (load compile eval)
  (defconstant NCC 8)
)
(def-c-struct termio
  (c_iflag ushort)
  (c_oflag ushort)
  (c_cflag ushort)
  (c_lflag ushort)
  (c_line uchar)
  (c_cc (c-array uchar #.NCC))
)

(eval-when (load compile eval)
  (defconstant NCCS 19)
)
(def-c-struct termios
  (c_iflag tcflag_t)
  (c_oflag tcflag_t)
  (c_cflag tcflag_t)
  (c_lflag tcflag_t)
  (c_line cc_t)
  (c_cc (c-array cc_t #.NCCS))
)

; c_cc characters
(defconstant VINTR 0)
(defconstant VQUIT 1)
(defconstant VERASE 2)
(defconstant VKILL 3)
(defconstant VEOF 4)
(defconstant VTIME 5)
(defconstant VMIN 6)
(defconstant VSWTC 7)
(defconstant VSTART 8)
(defconstant VSTOP 9)
(defconstant VSUSP 10)
(defconstant VEOL 11)
(defconstant VREPRINT 12)
(defconstant VDISCARD 13)
(defconstant VWERASE 14)
(defconstant VLNEXT 15)
(defconstant VEOL2 16)

; c_iflag bits
(defconstant IGNBRK  #o000001)
(defconstant BRKINT  #o000002)
(defconstant IGNPAR  #o000004)
(defconstant PARMRK  #o000010)
(defconstant INPCK   #o000020)
(defconstant ISTRIP  #o000040)
(defconstant INLCR   #o000100)
(defconstant IGNCR   #o000200)
(defconstant ICRNL   #o000400)
(defconstant IUCLC   #o001000)
(defconstant IXON    #o002000)
(defconstant IXANY   #o004000)
(defconstant IXOFF   #o010000)
(defconstant IMAXBEL #o020000)

; c_oflag bits
(defconstant OPOST   #o000001)
(defconstant OLCUC   #o000002)
(defconstant ONLCR   #o000004)
(defconstant OCRNL   #o000010)
(defconstant ONOCR   #o000020)
(defconstant ONLRET  #o000040)
(defconstant OFILL   #o000100)
(defconstant OFDEL   #o000200)
(defconstant NLDLY   #o000400)
(defconstant   NL0   #o000000)
(defconstant   NL1   #o000400)
(defconstant CRDLY   #o003000)
(defconstant   CR0   #o000000)
(defconstant   CR1   #o001000)
(defconstant   CR2   #o002000)
(defconstant   CR3   #o003000)
(defconstant TABDLY  #o014000)
(defconstant   TAB0  #o000000)
(defconstant   TAB1  #o004000)
(defconstant   TAB2  #o010000)
(defconstant   TAB3  #o014000)
(defconstant   XTABS #o014000)
(defconstant BSDLY   #o020000)
(defconstant   BS0   #o000000)
(defconstant   BS1   #o020000)
(defconstant VTDLY   #o040000)
(defconstant   VT0   #o000000)
(defconstant   VT1   #o040000)
(defconstant FFDLY   #o100000)
(defconstant   FF0   #o000000)
(defconstant   FF1   #o100000)

; c_cflag bit meaning
(defconstant CBAUD   #o010017)
(defconstant  B0     #o000000)
(defconstant  B50    #o000001)
(defconstant  B75    #o000002)
(defconstant  B110   #o000003)
(defconstant  B134   #o000004)
(defconstant  B150   #o000005)
(defconstant  B200   #o000006)
(defconstant  B300   #o000007)
(defconstant  B600   #o000010)
(defconstant  B1200  #o000011)
(defconstant  B1800  #o000012)
(defconstant  B2400  #o000013)
(defconstant  B4800  #o000014)
(defconstant  B9600  #o000015)
(defconstant  B19200 #o000016)
(defconstant  B38400 #o000017)
(defconstant EXTA B19200)
(defconstant EXTB B38400)
(defconstant CSIZE   #o000060)
(defconstant   CS5   #o000000)
(defconstant   CS6   #o000020)
(defconstant   CS7   #o000040)
(defconstant   CS8   #o000060)
(defconstant CSTOPB  #o000100)
(defconstant CREAD   #o000200)
(defconstant PARENB  #o000400)
(defconstant PARODD  #o001000)
(defconstant HUPCL   #o002000)
(defconstant CLOCAL  #o004000)
(defconstant CBAUDEX #o010000)
(defconstant  B57600  #o010001)
(defconstant  B115200 #o010002)
(defconstant  B230400 #o010003)
(defconstant CIBAUD    #o02003600000)
(defconstant CRTSCTS   #o20000000000)

; c_lflag bits
(defconstant ISIG    #o000001)
(defconstant ICANON  #o000002)
(defconstant XCASE   #o000004)
(defconstant ECHO    #o000010)
(defconstant ECHOE   #o000020)
(defconstant ECHOK   #o000040)
(defconstant ECHONL  #o000100)
(defconstant NOFLSH  #o000200)
(defconstant TOSTOP  #o000400)
(defconstant ECHOCTL #o001000)
(defconstant ECHOPRT #o002000)
(defconstant ECHOKE  #o004000)
(defconstant FLUSHO  #o010000)
(defconstant PENDIN  #o040000)
(defconstant IEXTEN  #o100000)

; modem lines
(defconstant TIOCM_LE        #x001)
(defconstant TIOCM_DTR       #x002)
(defconstant TIOCM_RTS       #x004)
(defconstant TIOCM_ST        #x008)
(defconstant TIOCM_SR        #x010)
(defconstant TIOCM_CTS       #x020)
(defconstant TIOCM_CAR       #x040)
(defconstant TIOCM_RNG       #x080)
(defconstant TIOCM_DSR       #x100)
(defconstant TIOCM_CD        TIOCM_CAR)
(defconstant TIOCM_RI        TIOCM_RNG)

(defconstant TIOCSER_TEMT    #x01)

; tcflow() and TCXONC use these
(defconstant TCOOFF          0)
(defconstant TCOON           1)
(defconstant TCIOFF          2)
(defconstant TCION           3)

; tcflush() and TCFLSH use these
(defconstant TCIFLUSH        0)
(defconstant TCOFLUSH        1)
(defconstant TCIOFLUSH       2)

; tcsetattr uses these
(defconstant TCSANOW         0)
(defconstant TCSADRAIN       1)
(defconstant TCSAFLUSH       2)

; line disciplines
(defconstant N_TTY           0)
(defconstant N_SLIP          1)
(defconstant N_MOUSE         2)
(defconstant N_PPP           3)

; ----------------------------- <termios.h> -----------------------------------

(def-c-call-out cfgetispeed (:arguments (termios_p (c-ptr termios)))
                            (:return-type speed_t)
)
(def-c-call-out cfgetospeed (:arguments (termios_p (c-ptr termios)))
                            (:return-type speed_t)
)

(def-c-call-out cfmakeraw (:arguments (t (c-ptr termios) :in-out))
                          (:return-type nil)
)

(def-c-call-out cfsetispeed (:arguments (termios_p (c-ptr termios) :in-out) (speed speed_t))
                            (:return-type int)
)
(def-c-call-out cfsetospeed (:arguments (termios_p (c-ptr termios) :in-out) (speed speed_t))
                            (:return-type int)
)

(def-c-call-out tcdrain (:arguments (fildes int))
                        (:return-type int)
)

(def-c-call-out tcflow (:arguments (fildes int) (action int))
                       (:return-type int)
)

(def-c-call-out tcflush (:arguments (fildes int) (queue_selector int))
                        (:return-type int)
)

(def-c-call-out tcgetpgrp (:arguments (fildes int))
                          (:return-type pid_t)
)

(def-c-call-out tcgetattr (:arguments (fildes int) (termios_p (c-ptr termios) :out))
                          (:return-type int)
)

(def-c-call-out tcsendbreak (:arguments (fildes int) (duration int))
                            (:return-type int)
)

(def-c-call-out tcsetattr (:arguments (fildes int) (optional_actions int) (termios_p (c-ptr termios)))
                          (:return-type int)
)

(def-c-call-out tcsetpgrp (:arguments (fildes int) (pgrp_id pid_t))
                          (:return-type int)
)


#|
; ============================== <.h> ====================================
; =============================================================================

(def-c-call-out (:arguments
                (:return-type
)

(defconstant )
|#
; =============================================================================
#| Not yet converted:

sys:
-rw-r--r--   1 bin      bin           786 Oct 18  1992 file.h
-rw-r--r--   1 bin      bin           261 Nov 17  1992 ioctl.h
-rw-r--r--   1 bin      bin           290 Mar  5  1994 ipc.h
-rw-r--r--   1 bin      bin            83 Jan  3  1993 kd.h
-rw-r--r--   1 bin      bin           533 Feb 15  1994 mman.h
-rw-r--r--   1 bin      bin           331 Jan 25  1993 mount.h
-rw-r--r--   1 bin      bin           495 Jun 19  1993 msg.h
-rw-r--r--   1 bin      bin           363 Nov 17  1992 mtio.h
-rw-r--r--   1 bin      bin           431 May  6  1993 param.h
-rw-r--r--   1 bin      bin           229 Oct 19  1992 ptrace.h
-rw-r--r--   1 bin      bin           676 Aug 17  1993 resource.h
-rw-r--r--   1 bin      bin           632 Jun 19  1993 sem.h
-rw-r--r--   1 bin      bin          2638 Oct  2  1992 serial.h
-rw-r--r--   1 bin      bin           406 Jun 19  1993 shm.h
-rw-r--r--   1 bin      bin            20 Oct  2  1992 signal.h
-rw-r--r--   1 bin      bin          4538 Jan 25  1993 socket.h
-rw-r--r--   1 bin      bin           421 Oct 18  1992 socketcall.h
-rw-r--r--   1 bin      bin            29 Oct 13  1993 socketio.h
-rw-r--r--   1 bin      bin            30 Dec 14  1993 soundcard.h
-rw-r--r--   1 bin      bin          3404 Jul 22  1994 syscall.h
-rw-r--r--   1 bin      bin          6679 Dec 12  1993 syslog.h
-rw-r--r--   1 bin      bin           186 Oct  5  1993 sysmacros.h
-rw-r--r--   1 bin      bin          2315 Oct 31  1993 time.h
-rw-r--r--   1 bin      bin           294 Oct 18  1992 timeb.h
-rw-r--r--   1 bin      bin           266 Oct 31  1993 times.h
-rw-r--r--   1 bin      bin           441 Oct 31  1993 timex.h
-rw-r--r--   1 bin      bin          1995 Jan 25  1993 uio.h
-rw-r--r--   1 bin      bin            30 Dec 14  1993 ultrasound.h
-rw-r--r--   1 bin      bin            22 Oct  2  1992 un.h
-rw-r--r--   1 bin      bin            24 Oct  2  1992 user.h
-rw-r--r--   1 bin      bin           408 Apr 11  1993 vfs.h
-rw-r--r--   1 bin      bin           183 Oct 18  1992 vm86.h
-rw-r--r--   1 bin      bin            83 Jan  3  1993 vt.h
-rw-r--r--   1 bin      bin          4920 Jul 24  1993 wait.h

.:
drwxr-xr-x   2 bin      bin          1024 Apr 18  1993 arpa
-rw-r--r--   1 bin      bin         11300 Aug  8  1993 curses.h
-rw-r--r--   1 bin      bin          1995 Oct  2  1992 dbm.h
-rw-r--r--   1 bin      bin          1755 Aug 14  1993 fnmatch.h
-rw-r--r--   1 bin      bin           255 Jan 11  1994 fpu_control.h
-rw-r--r--   1 bin      bin          1545 Jan 25  1993 ftw.h
-rw-r--r--   1 bin      bin          3571 Nov 29  1992 gdbm.h
-r--r--r--   1 bin      bin          4507 Jul 25  1993 getopt.h
-rw-r--r--   1 bin      bin          3350 Aug 14  1993 glob.h
-rw-r--r--   1 bin      bin          2581 Dec 11  1993 iolibio.h
-rw-r--r--   1 bin      bin          3294 Dec 11  1993 iostdio.h
-rw-r--r--   1 bin      bin           174 Mar 26  1994 lastlog.h
-rw-r--r--   1 bin      bin          7501 Dec 30  1993 libio.h
-rw-r--r--   1 bin      bin          3727 Dec 10  1993 locale.h
-rw-r--r--   1 bin      bin          5054 Feb 14  1994 localeinfo.h
-rw-r--r--   1 bin      bin            20 Oct  2  1992 memory.h
-rw-r--r--   1 bin      bin          1308 Feb 14  1994 mntent.h
-rw-r--r--   1 bin      bin          2462 Oct  2  1992 ndbm.h
drwxr-xr-x   2 bin      bin          1024 Oct 27  1993 net
-rw-r--r--   1 bin      bin          5149 Mar  2  1993 netdb.h
drwxr-xr-x   2 bin      bin          1024 Sep 25 18:45 netinet
-rw-r--r--   1 bin      bin          2039 Mar 23  1994 nl_types.h
-rw-r--r--   1 bin      bin          1425 Oct 12  1992 nlist.h
-rw-r--r--   1 bin      bin         19890 Feb 21  1994 obstack.h
-rw-r--r--   1 bin      bin          3239 Apr  4  1994 paths.h
drwxr-xr-x   2 bin      bin          1024 Apr 18  1993 protocols
-rwxr-xr-x   1 bin      bin         19352 Feb 12  1994 regex.h
-rw-r--r--   1 bin      bin          5128 Nov 16  1993 regexp.h
-rw-r--r--   1 bin      bin          5639 Mar 10  1993 resolv.h
drwxr-xr-x   2 bin      bin          1024 Apr 18  1993 rpc
drwxr-xr-x   2 bin      bin          1024 Mar 12  1993 rpcsvc
-rw-r--r--   1 bin      bin           382 Oct  2  1992 sharedlib.h
-rw-r--r--   1 bin      bin          3374 Nov 15  1993 signal.h
-rw-r--r--   1 bin      bin          7421 Dec 17  1993 string.h
-rw-r--r--   1 bin      bin            20 Oct  2  1992 strings.h
drwxr-xr-x   2 bin      bin          1024 May 15  1994 sys
-rw-r--r--   1 bin      bin           656 Mar  8  1994 syscall.h
-rw-r--r--   1 bin      bin          5458 May 11  1993 sysexits.h
-rw-r--r--   1 bin      bin            24 Nov  5  1992 syslog.h
-rw-r--r--   1 bin      bin          3747 Oct  5  1992 tar.h
-rw-r--r--   1 bin      bin          1831 Feb 10  1994 termcap.h
-rw-r--r--   1 bin      bin          1397 Nov 29  1993 time.h
-rw-r--r--   1 bin      bin           200 Apr 20  1993 ulimit.h
-rw-r--r--   1 bin      bin           264 Nov 19  1993 utime.h
-rw-r--r--   1 bin      bin          1456 Apr  4  1994 utmp.h
-rw-r--r--   1 bin      bin          1156 Mar 10  1994 waitflags.h
-rw-r--r--   1 bin      bin          3724 Jul 24  1993 waitstatus.h

arpa:
-rw-r--r--   1 bin      bin          3658 Mar 12  1993 ftp.h
-rw-r--r--   1 bin      bin          1778 Oct  2  1992 inet.h
-rw-r--r--   1 bin      bin          7802 Aug  6  1993 nameser.h
-rw-r--r--   1 bin      bin          9228 Jan 25  1994 telnet.h
-rw-r--r--   1 bin      bin          1973 Oct  2  1992 tftp.h

net:
-rw-r--r--   1 bin      bin            22 Mar  7  1993 if.h
-rw-r--r--   1 bin      bin            26 Mar  7  1993 if_arp.h
-rw-r--r--   1 bin      bin            28 Mar  7  1993 if_route.h

netinet:
-rw-r--r--   1 bin      bin          2663 May 29  1993 in.h
-rw-r--r--   1 bin      bin            28 Jul 26  1993 in_systm.h
-rw-r--r--   1 bin      bin            22 Jul 26  1993 ip.h
-rw-r--r--   1 bin      bin            24 Jan 25  1994 ip_icmp.h
-rw-r--r--   1 bin      bin          2022 Mar 10  1994 ip_tcp.h
-rw-r--r--   1 bin      bin            23 Jan 25  1994 ip_udp.h
-rw-r--r--   1 bin      bin          1328 Oct  2  1992 protocols.h
lrwxrwxrwx   1 bin      bin             8 Oct 27  1993 tcp.h -> ip_tcp.h
lrwxrwxrwx   1 bin      bin             8 Oct 27  1993 udp.h -> ip_udp.h

protocols:
-rw-r--r--   1 bin      bin          4887 Mar 12  1993 talkd.h
-rw-r--r--   1 bin      bin          3043 Oct  2  1992 timed.h

rpc:
-rw-r--r--   1 bin      bin          5140 Jan 27  1993 auth.h
-rw-r--r--   1 bin      bin          2589 Nov 20  1992 auth_unix.h
-rw-r--r--   1 bin      bin          9484 Mar  5  1993 clnt.h
-rw-r--r--   1 bin      bin          3390 Nov 20  1992 pmap_clnt.h
-rw-r--r--   1 bin      bin          3569 Oct 19  1992 pmap_prot.h
-rw-r--r--   1 bin      bin          2001 Oct 19  1992 pmap_rmt.h
-rw-r--r--   1 bin      bin          2846 Mar  5  1993 rpc.h
-rw-r--r--   1 bin      bin          4487 Oct 19  1992 rpc_msg.h
-rw-r--r--   1 bin      bin          9504 May 11  1993 svc.h
-rw-r--r--   1 bin      bin          1754 Oct 19  1992 svc_auth.h
-rw-r--r--   1 bin      bin          2173 Mar  5  1993 types.h
-rw-r--r--   1 bin      bin         11063 Nov 20  1992 xdr.h

rpcsvc:
-rw-r--r--   1 bin      bin          2823 Sep 21  1992 bootparam_prot
-rw-r--r--   1 bin      bin          2823 Mar  7  1993 bootparam_prot.x
-rw-r--r--   1 bin      bin          1490 Sep 21  1992 klm_prot.h
-rw-r--r--   1 bin      bin          3486 Mar  7  1993 klm_prot.x
-rw-r--r--   1 bin      bin          1512 Sep 21  1992 mount.h
-rw-r--r--   1 bin      bin          4365 Mar  7  1993 mount.x
-rw-r--r--   1 bin      bin          5914 Sep 21  1992 nfs_prot.h
-rw-r--r--   1 bin      bin          7684 Mar  7  1993 nfs_prot.x
-rw-r--r--   1 bin      bin          4049 Sep 21  1992 nlm_prot.h
-rw-r--r--   1 bin      bin          3476 Mar  7  1993 nlm_prot.x
-rw-r--r--   1 bin      bin          3383 Sep 21  1992 rex.h
-rw-r--r--   1 bin      bin          7115 Mar  7  1993 rex.x
-rw-r--r--   1 bin      bin           935 Sep 21  1992 rnusers.h
-rw-r--r--   1 bin      bin          2151 Mar  7  1993 rnusers.x
-rw-r--r--   1 bin      bin           983 Sep 21  1992 rquota.h
-rw-r--r--   1 bin      bin          1569 Mar  7  1993 rquota.x
-rw-r--r--   1 bin      bin          1889 Sep 21  1992 rstat.h
-rw-r--r--   1 bin      bin          3694 Mar  7  1993 rstat.x
-rw-r--r--   1 bin      bin          1269 Sep 21  1992 sm_inter.h
-rw-r--r--   1 bin      bin          3546 Mar  7  1993 sm_inter.x
-rw-r--r--   1 bin      bin           643 Sep 21  1992 spray.h
-rw-r--r--   1 bin      bin          2238 Mar  7  1993 spray.x
-rw-r--r--   1 bin      bin          5444 Sep 21  1992 yp.h
-rw-r--r--   1 bin      bin          6130 Mar  7  1993 yp.x
-rw-r--r--   1 bin      bin         10736 Aug 14  1993 yp_prot.h
-rw-r--r--   1 bin      bin          3222 Mar  5  1993 ypclnt.h
-rw-r--r--   1 bin      bin          3034 Mar  5  1993 yppasswd.h
|#
; =============================================================================

(in-package "LISP")

(eval-when (compile eval)
  (delete-package "LINUX-AUX")
)

