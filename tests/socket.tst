;;; -*- Lisp -*- vim:filetype=lisp
;;; Interactive tests for non-blocking I/O on various kinds of handles

;; On Linux or FreeBSD, use strace to see what system calls are performed.
;; Might need to rebuild clisp without GENERATIONAL_GC for this. Also start
;; clisp|cat, to disable readline.

;; Good tools for testing sockets are:
;;   - telnet,
;;   - socket-1.1 from Jürgen Nickelsen.


;; Preliminaries.

(defun make-byte-array (n)
  (make-array n :element-type '(unsigned-byte 8)))
make-byte-array
(defun coerce-byte-array (l)
  (coerce l '(vector (unsigned-byte 8))))
coerce-byte-array

;; errno values are OS dependent.
;; But we cannot rely on the syscalls module here.
(let* ((os (ext:operating-system-type))
       (cpu (and (equal os "Linux") ; beware: e.g. on Solaris this does not return the CPU
                 ;; For the possible values of 'uname -m', see
                 ;; https://stackoverflow.com/questions/45125516/possible-values-for-uname-m
                 (let ((stream (ext:run-shell-command "uname -m" :output :stream)))
                   (unwind-protect (read-line stream)
                     (close stream))))))
  (setq *os* os)
  (setq +EINVAL+
        (cond ((equal os "GNU/Hurd") #x40000016)
              ((equal os "Haiku") (- 5 #x80000000))
              (t 22)))
  (setq +EPIPE+
        (cond ((equal os "GNU/Hurd") #x40000020)
              ((equal os "Haiku") (- #x600d #x80000000))
              (t 32)))
  (setq +EADDRNOTAVAIL+
        (cond ((equal os "GNU/Hurd") #x40000031)
              ((or (equal os "FreeBSD") (equal os "DragonFly") (equal os "GNU/kFreeBSD")
                   (equal os "NetBSD") (equal os "OpenBSD")
                   (equal os "Darwin")
                   (and (equal os "Linux") (or (eql (search "alpha" cpu) 0) (eql (search "sparc" cpu) 0)))
                   (equal os "Minix"))
               49)
              ((equal os "AIX") 68)
              ((or (equal os "HP-UX") (and (equal os "Linux") (eql (search "parisc" cpu) 0)))
               227)
              ((equal os "IRIX") 236)
              ((or (equal os "SunOS") (and (equal os "Linux") (eql (search "mips" cpu) 0)))
               126)
              ((equal os "Linux") 99)
              ((equal os "Haiku") (- #x7017 #x80000000))
              ((equal os "CYGWIN") 125)
              ((equal os "Windows") 101)
              (t 'unknown)))
  (setq +ECONNRESET+
        (cond ((equal os "GNU/Hurd") #x40000036)
              ((or (equal os "FreeBSD") (equal os "DragonFly") (equal os "GNU/kFreeBSD")
                   (equal os "NetBSD") (equal os "OpenBSD")
                   (equal os "Darwin")
                   (and (equal os "Linux") (or (eql (search "alpha" cpu) 0) (eql (search "sparc" cpu) 0)))
                   (equal os "Minix"))
               54)
              ((equal os "AIX") 73)
              ((or (equal os "HP-UX") (and (equal os "Linux") (eql (search "parisc" cpu) 0)))
               232)
              ((equal os "IRIX") 131)
              ((or (equal os "SunOS") (and (equal os "Linux") (eql (search "mips" cpu) 0)))
               131)
              ((equal os "Linux") 104)
              ((equal os "Haiku") (- #x701c #x80000000))
              ((equal os "CYGWIN") 104)
              ((equal os "Windows") 108)
              (t 'unknown)))
  (setq +ECONNREFUSED+
        (cond ((equal os "GNU/Hurd") #x4000003d)
              ((or (equal os "FreeBSD") (equal os "DragonFly") (equal os "GNU/kFreeBSD")
                   (equal os "NetBSD") (equal os "OpenBSD")
                   (equal os "Darwin")
                   (and (equal os "Linux") (or (eql (search "alpha" cpu) 0) (eql (search "sparc" cpu) 0)))
                   (equal os "Minix"))
               61)
              ((equal os "AIX") 79)
              ((or (equal os "HP-UX") (and (equal os "Linux") (eql (search "parisc" cpu) 0)))
               239)
              ((equal os "IRIX") 146)
              ((or (equal os "SunOS") (and (equal os "Linux") (eql (search "mips" cpu) 0)))
               146)
              ((equal os "Linux") 111)
              ((equal os "Haiku") (- #x7020 #x80000000))
              ((equal os "CYGWIN") 111)
              ((equal os "Windows") 107)
              (t 'unknown)))
  (setq +ETIMEDOUT+
        (cond ((equal os "GNU/Hurd") #x4000003c)
              ((or (equal os "FreeBSD") (equal os "DragonFly") (equal os "GNU/kFreeBSD")
                   (equal os "NetBSD") (equal os "OpenBSD")
                   (equal os "Darwin")
                   (and (equal os "Linux") (or (eql (search "alpha" cpu) 0) (eql (search "sparc" cpu) 0)))
                   (equal os "Minix"))
               60)
              ((equal os "AIX") 78)
              ((or (equal os "HP-UX") (and (equal os "Linux") (eql (search "parisc" cpu) 0)))
               238)
              ((equal os "IRIX") 145)
              ((or (equal os "SunOS") (and (equal os "Linux") (eql (search "mips" cpu) 0)))
               145)
              ((equal os "Linux") 110)
              ((equal os "Haiku") (- #x7020 #x80000000))
              ((equal os "CYGWIN") 116)
              ((equal os "Windows") 138)
              (t 'unknown)))
  nil)
nil

;;; * Reading from files

(defparameter *file* "socket-tst-file.test") *file*
(with-open-file (s *file* :direction :output)
  (loop :repeat 3 :do (write-line "abcdefghijklmnopqrstuvwxyz" s)))
NIL

;;; ** Reading from unbuffered file streams
(defparameter *s*
  (open *file* :element-type '(unsigned-byte 8) :buffered nil))
*s*

(read-byte-sequence (make-byte-array 4) *s*)  4
;; Expected syscalls: 1 x read()

(read-byte-sequence (make-byte-array 4) *s* :no-hang t)   4
;; Expected syscalls: 1 x read()

(read-byte-sequence (make-byte-array 4) *s* :interactive t)   4
;; Expected syscalls: 1 x read()

(close *s*) t

(with-open-file (*s* *file* :element-type '(unsigned-byte 8) :buffered nil)
  (integerp (read-byte-sequence (make-byte-array 10000) *s*))) T
;; Expected: returns without hanging
;; Expected syscalls: 2 x read()

(with-open-file (*s* *file* :element-type '(unsigned-byte 8) :buffered nil)
  (integerp (read-byte-sequence (make-byte-array 10000) *s* :no-hang t))) T
;; Expected: returns without hanging
;; Expected syscalls: 1 x read()

(with-open-file (*s* *file* :element-type '(unsigned-byte 8) :buffered nil)
  (integerp (read-byte-sequence (make-byte-array 10000) *s* :interactive t))) T
;; Expected: returns without hanging
;; Expected syscalls: 1 x read()

;;; ** Reading from buffered file streams

(progn (setq *s* (open *file* :element-type '(unsigned-byte 8) :buffered t))
       (read-byte-sequence (make-byte-array 4) *s*)) 4
;; Expected syscalls: 1 x read()

(read-byte-sequence (make-byte-array 4) *s* :no-hang t) 4
;; Expected syscalls: none

(read-byte-sequence (make-byte-array 4) *s* :interactive t) 4
;; Expected syscalls: none

(close *s*) T

(with-open-file (*s* *file* :element-type '(unsigned-byte 8) :buffered t)
  (integerp (read-byte-sequence (make-byte-array 10000) *s*))) T
;; Expected: returns without hanging
;; Expected syscalls: 2 x read()

(with-open-file (*s* *file* :element-type '(unsigned-byte 8) :buffered t)
  (integerp (read-byte-sequence (make-byte-array 10000) *s* :no-hang t))) T
;; Expected: returns without hanging
;; Expected syscalls: 1 x read()

(with-open-file (*s* *file* :element-type '(unsigned-byte 8) :buffered t)
  (integerp (read-byte-sequence (make-byte-array 10000) *s* :interactive t))) T
;; Expected: returns without hanging
;; Expected syscalls: 1 x read()

;;; * Status:
;; Linux   2004-10-31 OK
;; FreeBSD 2004-10-31 OK
;; BeOS    2004-10-31 OK

;;; * Reading from pipes

;;; ** Reading from unbuffered pipe streams
#+(or) (progn                   ; unix-only, hanging &c
(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered nil))
  (read-byte-sequence (make-byte-array 8) *s*)) 8
;; Expected: hangs then returns 8
;; Expected syscalls: 2 x read()

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered nil))
  (read-byte-sequence (make-byte-array 8) *s* :no-hang t)) 0
;; Expected: 0 without hanging
;; Expected syscalls: 1 x poll()/select()

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered nil))
  (read-byte-sequence (make-byte-array 8) *s* :interactive t)) 4
;; Expected: hangs then returns 4
;; Expected syscalls: 1 x read()

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered nil))
  (read-byte-sequence (make-byte-array 10000) *s*)) 8
;; Expected: hangs then returns 8
;; Expected syscalls: 3 x read()

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered nil))
  (read-byte-sequence (make-byte-array 10000) *s* :no-hang t)) 0
;; Expected: 0 without hanging
;; Expected syscalls: 1 x poll()/select()

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered nil))
  (read-byte-sequence (make-byte-array 10000) *s* :interactive t)) 4
;; Expected: hangs then returns 4
;; Expected syscalls: 1 x read()

;;; ** Reading from buffered pipe streams

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered t))
  (read-byte-sequence (make-byte-array 8) *s*)) 8
;; Expected: hangs then returns 8
;; Expected syscalls: 2 x read()

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered t))
  (read-byte-sequence (make-byte-array 8) *s* :no-hang t)) 0
;; Expected: 0 without hanging
;; Expected syscalls: 1 x poll()/select()

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered t))
  (read-byte-sequence (make-byte-array 8) *s* :interactive t)) 4
;; Expected: hangs then returns 4
;; Expected syscalls: 1 x read()

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered t))
  (read-byte-sequence (make-byte-array 10000) *s*)) 8
;; Expected: hangs then returns 8
;; Expected syscalls: 3 x read()

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered t))
  (read-byte-sequence (make-byte-array 10000) *s* :no-hang t))
;; Expected: 0 without hanging
;; Expected syscalls: 1 x poll()/select()

(with-open-stream (*s* (make-pipe-input-stream
                        "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                        :element-type '(unsigned-byte 8) :buffered t))
  (read-byte-sequence (make-byte-array 10000) *s* :interactive t)) 4
;; Expected: hangs then returns 4
;; Expected syscalls: 1 x read()

;;; * Status:
;; Linux   2004-10-31 OK
;; FreeBSD 2004-10-31 OK
;; BeOS    2004-10-31 OK

)

;;; * Reading from sockets

;;; ** Reading from unbuffered socket streams
#+(or) (progn
(setq *s* (socket-accept (socket-server 1210)
                         :element-type '(unsigned-byte 8) :buffered nil))
;; telnet localhost 1210
(read-byte-sequence (make-byte-array 8) *s*)
;; Expected: hangs then after you type ab<RET>de<RET> it returns 8
;; Expected syscalls: 2 x read()

(setq *s* (socket-accept (socket-server 1211)
                         :element-type '(unsigned-byte 8) :buffered nil))
;; telnet localhost 1211
(read-byte-sequence (make-byte-array 8) *s* :no-hang t)
;; Expected: 0 without hanging
;; Expected syscalls: 1 x poll()/select()

(setq *s* (socket-accept (socket-server 1212)
                         :element-type '(unsigned-byte 8) :buffered nil))
;; telnet localhost 1212
(read-byte-sequence (make-byte-array 8) *s* :interactive t)
;; Expected: hangs then after you type ab<RET> it returns 4
;; Expected syscalls: 1 x read()

(setq *s* (socket-accept (socket-server 1213)
                         :element-type '(unsigned-byte 8) :buffered nil))
;; telnet localhost 1213
(read-byte-sequence (make-byte-array 10000) *s*)
;; Expected: hangs then after you type ab<RET>de<RET>^]quit<RET> it returns 8
;; Expected syscalls: 3 x read()

(setq *s* (socket-accept (socket-server 1214)
                         :element-type '(unsigned-byte 8) :buffered nil))
;; telnet localhost 1214
(read-byte-sequence (make-byte-array 10000) *s* :no-hang t)
;; Expected: 0 without hanging
;; Expected syscalls: 1 x poll()/select()

(setq *s* (socket-accept (socket-server 1215)
                         :element-type '(unsigned-byte 8) :buffered nil))
;; telnet localhost 1215
(read-byte-sequence (make-byte-array 10000) *s* :interactive t)
;; Expected: hangs then after you type ab<RET> it returns 4
;; Expected syscalls: 1 x read()

;;; ** Reading from buffered socket streams

(setq *s* (socket-accept (socket-server 1220)
                         :element-type '(unsigned-byte 8) :buffered t))
;; telnet localhost 1220
(read-byte-sequence (make-byte-array 8) *s*)
;; Expected: hangs then after you type ab<RET>de<RET> it returns 8
;; Expected syscalls: 2 x read()

(setq *s* (socket-accept (socket-server 1221)
                         :element-type '(unsigned-byte 8) :buffered t))
;; telnet localhost 1221
(read-byte-sequence (make-byte-array 8) *s* :no-hang t)
;; Expected: 0 without hanging
;; Expected syscalls: 1 x poll()/select()

(setq *s* (socket-accept (socket-server 1222)
                         :element-type '(unsigned-byte 8) :buffered t))
;; telnet localhost 1222
(read-byte-sequence (make-byte-array 8) *s* :interactive t)
;; Expected: hangs then after you type ab<RET> it returns 4
;; Expected syscalls: 1 x read()

(setq *s* (socket-accept (socket-server 1223)
                         :element-type '(unsigned-byte 8) :buffered t))
;; telnet localhost 1223
(read-byte-sequence (make-byte-array 10000) *s*)
;; Expected: hangs then after you type ab<RET>de<RET>^]quit<RET> it returns 8
;; Expected syscalls: 3 x read()

(setq *s* (socket-accept (socket-server 1224)
                         :element-type '(unsigned-byte 8) :buffered t))
;; telnet localhost 1224
(read-byte-sequence (make-byte-array 10000) *s* :no-hang t)
;; Expected: 0 without hanging
;; Expected syscalls: 1 x poll()/select()

(setq *s* (socket-accept (socket-server 1225)
                       :element-type '(unsigned-byte 8) :buffered t))
;; telnet localhost 1225
(read-byte-sequence (make-byte-array 10000) *s* :interactive t)
;; Expected: hangs then after you type ab<RET> it returns 4
;; Expected syscalls: 1 x read()

;;; * Status:
;; Linux   2004-10-31 OK
;; FreeBSD 2004-10-31 OK
;; BeOS    2004-10-31 OK (except that telnet is not line-buffered)

)

;; Writing to files


(progn (setq *s* (open *file* :direction :io
                       :element-type '(unsigned-byte 8) :buffered nil))
       (length (write-byte-sequence (coerce-byte-array '(65 66 67 68)) *s*))) 4
;; Expected: 4 without hanging
;; Expected syscalls: 1 x write()

(length (write-byte-sequence (coerce-byte-array '(69 70 71 72 10)) *s*)) 5
;; Expected: 5 without hanging
;; Expected syscalls: 1 x write()

(close *s*) T
(with-open-file (*s* *file* :direction :input)
  (multiple-value-list (read-line *s*)))
("ABCDEFGH" NIL)

(progn (setq *s* (open *file* :direction :io
                       :element-type '(unsigned-byte 8) :buffered t))
       (length (write-byte-sequence (coerce-byte-array '(65 66 67 68)) *s*))) 4
;; Expected: 4 without hanging
;; Expected syscalls: 1 x read()

(length (write-byte-sequence (coerce-byte-array '(69 70 71 72 10)) *s*)) 5
;; Expected: 5 without hanging
;; Expected syscalls: none

(close *s*) T
(with-open-file (*s* *file* :direction :input)
  (multiple-value-list (read-line *s*)))
("ABCDEFGH" NIL)

;;; * Status:
;; Linux   2004-10-31 OK
;; FreeBSD 2004-10-31 OK
;; BeOS    2004-10-31 OK


;;; * Writing to pipes
#+(or) (progn
(progn (setq *s* (make-pipe-output-stream
                  (format nil "cat > ~A" *file*)
                  :element-type '(unsigned-byte 8) :buffered nil))
       (length (write-byte-sequence (coerce-byte-array '(65 66 67 68)) *s*))) 4
;; Expected: 4 without hanging and cat file.test shows output
;; Expected syscalls: 1 x write()

(length (write-byte-sequence (coerce-byte-array '(69 70 71 72 10)) *s*)) 5
;; Expected: 5 without hanging and cat file.test shows output
;; Expected syscalls: 1 x write()

(close *s*) T
(with-open-file (*s* *file* :direction :input)
  (multiple-value-list (read-line *s*)))
("ABCDEFGH" NIL)

(progn (setq *s* (make-pipe-output-stream
                  (format nil "cat > ~A" *file*)
                  :element-type '(unsigned-byte 8) :buffered t))
       (length (write-byte-sequence (coerce-byte-array '(65 66 67 68)) *s*))) 4
;; Expected: 4 without hanging, cat file.test shows no output
;; Expected syscalls: none

(force-output *s*) NIL
;; Expected: cat file.test shows ABCD
;; Expected syscalls: 1 x write()

(length (write-byte-sequence (coerce-byte-array '(69 70 71 72 10)) *s*)) 5
;; Expected: 5 without hanging, cat file.test shows ABCD
;; Expected syscalls: none

(close *s*) T
(with-open-file (*s* *file* :direction :input)
  (multiple-value-list (read-line *s*)))
("ABCDEFGH" NIL)

;; Status:
;; Linux   2004-10-31 OK
;; FreeBSD 2004-10-31 OK
;; BeOS    2004-10-31 OK

)

;;; * Writing to sockets
#+(or) (progn
(setq *s* (socket-accept (socket-server 1234)
                         :element-type '(unsigned-byte 8) :buffered nil))
;; telnet localhost 1234
(write-byte-sequence (coerce-byte-array '(65 66 67 68)) *s*)
;; Expected: 4 without hanging, output ABCD
;; Expected syscalls: 1 x write()

(write-byte-sequence (coerce-byte-array '(69 70 71 72 13 10)) *s*)
;; Expected: 6 without hanging, output EFGH
;; Expected syscalls: 1 x write()

(close *s*)
;; Expected: telnet close

(setq *s* (socket-accept (socket-server 1235)
                         :element-type '(unsigned-byte 8) :buffered t))
;; telnet localhost 1235
(write-byte-sequence (coerce-byte-array '(65 66 67 68)) *s*)
;; Expected: 4 without hanging but no output
;; Expected syscalls: none

(force-output *s*)
;; Expected: output ABCD
;; Expected syscalls: 1 x write()

(write-byte-sequence (coerce-byte-array '(69 70 71 72 13 10)) *s*)
;; Expected: 6 without hanging but no output
;; Expected syscalls: none

(close *s*)
;; Expected: output EFGH then telnet close
;; Expected syscalls: 1 x write(), 1 x close()

;;; * Status:
;; Linux   2004-10-31 OK
;; FreeBSD 2004-10-31 OK
;; BeOS    2004-10-31 OK

)

;;; * generic socket servers

(defparameter *server* (show (socket-server))) *server*
(multiple-value-list (socket-status *server* 0)) (NIL 0)

(defparameter *socket-1*
  (show (socket-connect (socket-server-port *server*) "localhost" :timeout 0)))
*socket-1*

(defparameter *status-arg* (list (list *server*) (list *socket-1* :io)))
*status-arg*
(eq (socket-status *status-arg* 0) *status-arg*) T
(cdr (assoc *server* *status-arg*))    T
(cddr (assoc *socket-1* *status-arg*)) :OUTPUT

(defparameter *socket-2* (show (socket-accept *server*))) *socket-2*
(progn (push (list *socket-2* :io) *status-arg*)
       (eq *status-arg* (socket-status *status-arg* 0))) T
(cdr (assoc *server* *status-arg*))    NIL
(cddr (assoc *socket-1* *status-arg*)) :OUTPUT
(cddr (assoc *socket-2* *status-arg*)) :OUTPUT

(write-line "foo" *socket-1*) "foo"
(finish-output *socket-1*) nil

(eq (socket-status *status-arg* 0) *status-arg*) T
(cdr (assoc *server* *status-arg*))    NIL
(cddr (assoc *socket-1* *status-arg*)) :OUTPUT
;; This test frequently fails on Haiku.
(cddr (assoc *socket-2* *status-arg*)) :IO

(multiple-value-list (read-line *socket-2*)) ("foo" NIL)

(close *socket-1*) T
(close *socket-2*) T

(multiple-value-list (socket-status *server* 0)) (NIL 0)

(defparameter *socket-3*
  (show (socket-connect (socket-server-port *server*) "localhost" :timeout 0
                        :element-type '(unsigned-byte 8))))
*socket-3*

(defparameter *status-arg* (list (list *server*) (list *socket-3* :io)))
*status-arg*
(eq (socket-status *status-arg* 0) *status-arg*) T
(cdr (assoc *server* *status-arg*))    T
(cddr (assoc *socket-3* *status-arg*)) :OUTPUT

(defparameter *socket-4*
  (show (socket-accept *server* :element-type '(unsigned-byte 8))))
*socket-4*
(progn (push (list *socket-4* :io) *status-arg*)
       (eq *status-arg* (socket-status *status-arg* 0))) T
(cdr (assoc *server* *status-arg*))    NIL
(cddr (assoc *socket-3* *status-arg*)) :OUTPUT
(cddr (assoc *socket-4* *status-arg*)) :OUTPUT

(read-byte-no-hang *socket-3*) nil
(write-byte 65 *socket-3*) 65
(finish-output *socket-3*) nil

(eq (socket-status *status-arg* 0) *status-arg*) T
(cdr (assoc *server* *status-arg*))    NIL
(cddr (assoc *socket-3* *status-arg*)) :OUTPUT
(cddr (assoc *socket-4* *status-arg*)) :IO

(read-byte *socket-4*) 65

(close *socket-3*) T
(close *socket-4*) T
(socket-server-close *server*) NIL

(progn
  (setq *server* (show (socket-server 9090))
        *socket-1* (show (socket-connect 9090 "localhost" :timeout 0
                                         :buffered nil))
        *socket-2* (show (socket-accept *server* :buffered nil)))
  (write-char #\a *socket-1*))
#\a

(listp
 (show
  (list (multiple-value-list (socket:socket-stream-local *socket-1*))
        (multiple-value-list (socket:socket-stream-peer *socket-1*))
        (multiple-value-list (socket:socket-stream-local *socket-2*))
        (multiple-value-list (socket:socket-stream-peer *socket-2*)))
  :pretty t))
T

(search " (" (socket:socket-stream-local *socket-1* t)) NIL
(search " (" (socket:socket-stream-peer *socket-1* t)) NIL

(socket-status (cons *socket-2* :input) 0) :INPUT
(read-char *socket-2*) #\a
;; This test frequently fails on Haiku.
(socket-status (cons *socket-2* :input) 0) NIL
(close *socket-1*) T
;; This test frequently fails on Haiku.
(socket-status (cons *socket-2* :input) 0) :EOF
(close *socket-2*) T
(multiple-value-list (socket-status *server* 0)) (NIL 0)
(socket-server-close *server*) NIL

;; no one should be listening on 12345
;; https://sourceforge.net/p/clisp/bugs/482/
;; http://article.gmane.org/gmane.lisp.clisp.general/12286
;; https://sourceforge.net/p/clisp/mailman/message/19641749/
(check-os-error (socket:socket-connect 12345 "localhost" :timeout 30)
  #.(let ((os (ext:operating-system-type)))
      (cond ((equal os "Windows") '(:ETIMEDOUT 10060))
            (t `(:ECONNREFUSED ,+ECONNREFUSED+)))))
T
;; :TIMEOUT 0 means to return a useful result or error immediately.
;; https://gitlab.com/gnu-clisp/clisp/-/issues/25
(check-os-error (socket:socket-connect 12345 "localhost" :timeout 0)
  #.(let ((os (ext:operating-system-type)))
      (cond ((equal os "Windows") '(:ETIMEDOUT 10060))
            ((equal os "CYGWIN") `(:ETIMEDOUT ,+ETIMEDOUT+))
            (t `(:ECONNREFUSED ,+ECONNREFUSED+)))))
T
(check-os-error (socket:socket-connect 12345 "localhost" :buffered nil :timeout 0)
  #.(let ((os (ext:operating-system-type)))
      (cond ((equal os "Windows") '(:ETIMEDOUT 10060))
            ((equal os "CYGWIN") `(:ETIMEDOUT ,+ETIMEDOUT+))
            (t `(:ECONNREFUSED ,+ECONNREFUSED+)))))
T

;; https://sourceforge.net/p/clisp/bugs/587/: non-0 timeout
(multiple-value-bind (run args) (cmd-args)
  (let ((is (ext:run-program run :arguments (append args '("-q" "-q" "-x" "
            (let ((se (socket:socket-server)))
              (write-line (princ-to-string (socket:socket-server-port se)))
              (with-open-stream (so (socket:socket-accept se))
                (write-line (lisp-implementation-version) so))
              (socket:socket-server-close se))
"))
                             :input nil :output :stream))
        local remote)
    (loop :until (digit-char-p (peek-char nil is)) :do (read-line is))
    (with-open-stream (so (socket:socket-connect (read is) "localhost"
                                                 :timeout 10))
      (or (string= (setq local (lisp-implementation-version))
                   (setq remote (read-line so)))
          (list :local local :remote remote)))))
T

(let ((interfaces '#.(cond ((equal (ext:operating-system-type) "Minix") '())
                           (t '(nil "localhost" "0.0.0.0" "127.0.0.1")))))

  (mapcar (lambda (i)
            (let ((s (socket-server 0 :interface i)))
              (unwind-protect (socket-server-host (show s))
                (socket-server-close s))))
          interfaces))
#.(cond ((equal (ext:operating-system-type) "Minix") '())
        (t '("0.0.0.0" "127.0.0.1" "0.0.0.0" "127.0.0.1")))

;; Test writing to and reading from a socket after it has encountered ECONNRESET.
(multiple-value-bind (run args) (cmd-args)
  ;; Open a socket server.
  (let ((se (socket:socket-server)))
    ;; Spawn a detached process that connects to it and dies soon afterwards.
    (ext:run-program run
                     :arguments (append args
                                  (list "-q" "-q" "-x"
                                        (format nil "(close (prog1 (socket:socket-connect ~D) (sleep 0.01s0)))" (socket:socket-server-port se))))
                     :wait nil :input nil :output nil)
    (unwind-protect
        (with-open-stream (so (socket:socket-accept se))
          (list
            (socket:socket-status so)
            (write-line "foo" so)
            (sleep 0.02s0)
            (handler-case (socket:socket-status so)
              (os-error (c)
                (princ 'socket-status-2) (princ-error c)
                (case (os-error-code c)
                  ((:ECONNRESET #.+ECONNRESET+) :APPEND)
                  (t (os-error-code c)))))
            (check-os-error
              (progn (write-line "bar" so) t) ; does not signal an error e.g. on macOS, FreeBSD
              (:EPIPE #.+EPIPE+ :ECONNRESET #.+ECONNRESET+)) ; signals ECONNRESET e.g. on Minix
            (handler-case (read-char so)
              (os-error (c)
                (princ 'read-char) (princ-error c)
                (case (os-error-code c)
                  ((:ECONNRESET #.+ECONNRESET+) t)
                  (t (os-error-code c))))
              (end-of-file (c)
                (princ 'read-char) (princ-error c) t))
            (handler-case (socket:socket-status so)
              (os-error (c)
                (princ 'socket-status-3) (princ-error c)
                (case (os-error-code c)
                  ((:ECONNRESET #.+ECONNRESET+) :APPEND) ; signals ECONNRESET e.g. on Cygwin
                  (t (os-error-code c)))))))
      (socket:socket-server-close se))))
(:OUTPUT "foo" NIL :APPEND T T #.(if (equal (ext:operating-system-type) "OpenBSD") ':ERROR ':APPEND))

;; https://sourceforge.net/p/clisp/feature-requests/46/
(check-os-error (socket:socket-connect 0)
  #.(let ((os (ext:operating-system-type)))
      (cond ((or (equal os "FreeBSD") (equal os "DragonFly") (equal os "GNU/kFreeBSD")
                 (equal os "NetBSD") (equal os "OpenBSD")
                 (equal os "Darwin")
                 (equal os "AIX")
                 (equal os "SunOS")
                 (equal os "CYGWIN"))
             `(:EADDRNOTAVAIL ,+EADDRNOTAVAIL+))
            ((equal os "Minix") `(:EADDRNOTAVAIL ,+EADDRNOTAVAIL+ 213))
            ((equal os "Windows") '(:EADDRNOTAVAIL 10049))
            (t `(:ECONNREFUSED ,+ECONNREFUSED+)))))
T

(check-os-error (socket-server 1240 :interface "[/]=") (:EINVAL #.+EINVAL+)) T

;; clean-up
(progn (makunbound '*os*) (unintern '*os*)
       (makunbound '*server*) (unintern '*server*)
       (delete-file *file*) (makunbound '*file*) (unintern '*file*)
       (makunbound '*s*) (unintern '*s*)
       (makunbound '*socket-1*) (unintern '*socket-1*)
       (makunbound '*socket-2*) (unintern '*socket-2*)
       (makunbound '*socket-3*) (unintern '*socket-3*)
       (makunbound '*socket-4*) (unintern '*socket-4*))
T
