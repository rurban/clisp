#| -*- Lisp -*-
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

(defun coerce-byte-array (l)
  (coerce l '(vector (unsigned-byte 8))))


;; Reading from files

; Reading from unbuffered file streams

(setq s (open "/etc/passwd" :element-type '(unsigned-byte 8) :buffered nil))
(read-byte-sequence (make-byte-array 4) s)
Expected: 4 without hanging
Expected syscalls: 1 x read()

(read-byte-sequence (make-byte-array 4) s :no-hang t)
Expected: 4 without hanging
Expected syscalls: 1 x read()

(read-byte-sequence (make-byte-array 4) s :interactive t)
Expected: 4 without hanging
Expected syscalls: 1 x read()

(setq s (open "/etc/passwd" :element-type '(unsigned-byte 8) :buffered nil))
(read-byte-sequence (make-byte-array 10000) s)
Expected: returns without hanging
Expected syscalls: 2 x read()

(setq s (open "/etc/passwd" :element-type '(unsigned-byte 8) :buffered nil))
(read-byte-sequence (make-byte-array 10000) s :no-hang t)
Expected: returns without hanging
Expected syscalls: 1 x read()

(setq s (open "/etc/passwd" :element-type '(unsigned-byte 8) :buffered nil))
(read-byte-sequence (make-byte-array 10000) s :interactive t)
Expected: returns without hanging
Expected syscalls: 1 x read()

; Reading from buffered file streams

(setq s (open "/etc/passwd" :element-type '(unsigned-byte 8) :buffered t))
(read-byte-sequence (make-byte-array 4) s)
Expected: 4 without hanging
Expected syscalls: 1 x read()

(read-byte-sequence (make-byte-array 4) s :no-hang t)
Expected: 4 without hanging
Expected syscalls: none

(read-byte-sequence (make-byte-array 4) s :interactive t)
Expected: 4 without hanging
Expected syscalls: none

(setq s (open "/etc/passwd" :element-type '(unsigned-byte 8) :buffered t))
(read-byte-sequence (make-byte-array 10000) s)
Expected: returns without hanging
Expected syscalls: 2 x read()

(setq s (open "/etc/passwd" :element-type '(unsigned-byte 8) :buffered t))
(read-byte-sequence (make-byte-array 10000) s :no-hang t)
Expected: returns without hanging
Expected syscalls: 1 x read()

(setq s (open "/etc/passwd" :element-type '(unsigned-byte 8) :buffered t))
(read-byte-sequence (make-byte-array 10000) s :interactive t)
Expected: returns without hanging
Expected syscalls: 1 x read()

Status:
Linux   2004-10-31 OK
FreeBSD 2004-10-31 OK
BeOS    2004-10-31 OK


;; Reading from pipes

; Reading from unbuffered pipe streams

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered nil))
(read-byte-sequence (make-byte-array 8) s)
Expected: hangs then returns 8
Expected syscalls: 2 x read()

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered nil))
(read-byte-sequence (make-byte-array 8) s :no-hang t)
Expected: 0 without hanging
Expected syscalls: 1 x poll()/select()

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered nil))
(read-byte-sequence (make-byte-array 8) s :interactive t)
Expected: hangs then returns 4
Expected syscalls: 1 x read()

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered nil))
(read-byte-sequence (make-byte-array 10000) s)
Expected: hangs then returns 8
Expected syscalls: 3 x read()

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered nil))
(read-byte-sequence (make-byte-array 10000) s :no-hang t)
Expected: 0 without hanging
Expected syscalls: 1 x poll()/select()

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered nil))
(read-byte-sequence (make-byte-array 10000) s :interactive t)
Expected: hangs then returns 4
Expected syscalls: 1 x read()

; Reading from buffered pipe streams

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered t))
(read-byte-sequence (make-byte-array 8) s)
Expected: hangs then returns 8
Expected syscalls: 2 x read()

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered t))
(read-byte-sequence (make-byte-array 8) s :no-hang t)
Expected: 0 without hanging
Expected syscalls: 1 x poll()/select()

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered t))
(read-byte-sequence (make-byte-array 8) s :interactive t)
Expected: hangs then returns 4
Expected syscalls: 1 x read()

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered t))
(read-byte-sequence (make-byte-array 10000) s)
Expected: hangs then returns 8
Expected syscalls: 3 x read()

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered t))
(read-byte-sequence (make-byte-array 10000) s :no-hang t)
Expected: 0 without hanging
Expected syscalls: 1 x poll()/select()

(setq s (make-pipe-input-stream "sleep 5; printf 'abc\\n'; sleep 5; printf 'def\\n'"
                                :element-type '(unsigned-byte 8) :buffered t))
(read-byte-sequence (make-byte-array 10000) s :interactive t)
Expected: hangs then returns 4
Expected syscalls: 1 x read()

Status:
Linux   2004-10-31 OK
FreeBSD 2004-10-31 OK
BeOS    2004-10-31 OK


;; Reading from sockets

; Reading from unbuffered socket streams

(setq s (socket-accept (socket-server 1210)
                       :element-type '(unsigned-byte 8) :buffered nil))
telnet localhost 1210
(read-byte-sequence (make-byte-array 8) s)
Expected: hangs then after you type ab<RET>de<RET> it returns 8
Expected syscalls: 2 x read()

(setq s (socket-accept (socket-server 1211)
                       :element-type '(unsigned-byte 8) :buffered nil))
telnet localhost 1211
(read-byte-sequence (make-byte-array 8) s :no-hang t)
Expected: 0 without hanging
Expected syscalls: 1 x poll()/select()

(setq s (socket-accept (socket-server 1212)
                       :element-type '(unsigned-byte 8) :buffered nil))
telnet localhost 1212
(read-byte-sequence (make-byte-array 8) s :interactive t)
Expected: hangs then after you type ab<RET> it returns 4
Expected syscalls: 1 x read()

(setq s (socket-accept (socket-server 1213)
                       :element-type '(unsigned-byte 8) :buffered nil))
telnet localhost 1213
(read-byte-sequence (make-byte-array 10000) s)
Expected: hangs then after you type ab<RET>de<RET>^]quit<RET> it returns 8
Expected syscalls: 3 x read()

(setq s (socket-accept (socket-server 1214)
                       :element-type '(unsigned-byte 8) :buffered nil))
telnet localhost 1214
(read-byte-sequence (make-byte-array 10000) s :no-hang t)
Expected: 0 without hanging
Expected syscalls: 1 x poll()/select()

(setq s (socket-accept (socket-server 1215)
                       :element-type '(unsigned-byte 8) :buffered nil))
telnet localhost 1215
(read-byte-sequence (make-byte-array 10000) s :interactive t)
Expected: hangs then after you type ab<RET> it returns 4
Expected syscalls: 1 x read()

; Reading from buffered socket streams

(setq s (socket-accept (socket-server 1220)
                       :element-type '(unsigned-byte 8) :buffered t))
telnet localhost 1220
(read-byte-sequence (make-byte-array 8) s)
Expected: hangs then after you type ab<RET>de<RET> it returns 8
Expected syscalls: 2 x read()

(setq s (socket-accept (socket-server 1221)
                       :element-type '(unsigned-byte 8) :buffered t))
telnet localhost 1221
(read-byte-sequence (make-byte-array 8) s :no-hang t)
Expected: 0 without hanging
Expected syscalls: 1 x poll()/select()

(setq s (socket-accept (socket-server 1222)
                       :element-type '(unsigned-byte 8) :buffered t))
telnet localhost 1222
(read-byte-sequence (make-byte-array 8) s :interactive t)
Expected: hangs then after you type ab<RET> it returns 4
Expected syscalls: 1 x read()

(setq s (socket-accept (socket-server 1223)
                       :element-type '(unsigned-byte 8) :buffered t))
telnet localhost 1223
(read-byte-sequence (make-byte-array 10000) s)
Expected: hangs then after you type ab<RET>de<RET>^]quit<RET> it returns 8
Expected syscalls: 3 x read()

(setq s (socket-accept (socket-server 1224)
                       :element-type '(unsigned-byte 8) :buffered t))
telnet localhost 1224
(read-byte-sequence (make-byte-array 10000) s :no-hang t)
Expected: 0 without hanging
Expected syscalls: 1 x poll()/select()

(setq s (socket-accept (socket-server 1225)
                       :element-type '(unsigned-byte 8) :buffered t))
telnet localhost 1225
(read-byte-sequence (make-byte-array 10000) s :interactive t)
Expected: hangs then after you type ab<RET> it returns 4
Expected syscalls: 1 x read()

Status:
Linux   2004-10-31 OK
FreeBSD 2004-10-31 OK
BeOS    2004-10-31 OK (except that telnet is not line-buffered)


;; Writing to files

echo blabla > /tmp/testfile

(setq s (open "/tmp/testfile" :direction :io
                              :element-type '(unsigned-byte 8) :buffered nil))
(write-byte-sequence (coerce-byte-array '(65 66 67 68)) s)
Expected: 4 without hanging
Expected syscalls: 1 x write()

(write-byte-sequence (coerce-byte-array '(69 70 71 72 10)) s)
Expected: 5 without hanging
Expected syscalls: 1 x write()

(close s)
cat /tmp/testfile
Expected: ABCDEFGH

echo blabla > /tmp/testfile

(setq s (open "/tmp/testfile" :direction :io
                              :element-type '(unsigned-byte 8) :buffered t))
(write-byte-sequence (coerce-byte-array '(65 66 67 68)) s)
Expected: 4 without hanging
Expected syscalls: 1 x read()

(write-byte-sequence (coerce-byte-array '(69 70 71 72 10)) s)
Expected: 5 without hanging
Expected syscalls: none

(close s)
cat /tmp/testfile
Expected: ABCDEFGH

Status:
Linux   2004-10-31 OK
FreeBSD 2004-10-31 OK
BeOS    2004-10-31 OK


;; Writing to pipes

echo blabla > /tmp/testfile

(setq s (make-pipe-output-stream "cat > /tmp/testfile"
                                 :element-type '(unsigned-byte 8) :buffered nil))
(write-byte-sequence (coerce-byte-array '(65 66 67 68)) s)
Expected: 4 without hanging and cat /tmp/testfile shows output
Expected syscalls: 1 x write()

(write-byte-sequence (coerce-byte-array '(69 70 71 72 10)) s)
Expected: 5 without hanging and cat /tmp/testfile shows output
Expected syscalls: 1 x write()

(close s)
cat /tmp/testfile
Expected: ABCDEFGH

echo blabla > /tmp/testfile

(setq s (make-pipe-output-stream "cat > /tmp/testfile"
                                 :element-type '(unsigned-byte 8) :buffered t))
(write-byte-sequence (coerce-byte-array '(65 66 67 68)) s)
Expected: 4 without hanging, cat /tmp/testfile shows no output
Expected syscalls: none

(force-output s)
Expected: cat /tmp/testfile shows ABCD
Expected syscalls: 1 x write()

(write-byte-sequence (coerce-byte-array '(69 70 71 72 10)) s)
Expected: 5 without hanging, cat /tmp/testfile shows ABCD
Expected syscalls: none

(close s)
cat /tmp/testfile
Expected: ABCDEFGH

Status:
Linux   2004-10-31 OK
FreeBSD 2004-10-31 OK
BeOS    2004-10-31 OK


;; Writing to sockets

(setq s (socket-accept (socket-server 1234)
                       :element-type '(unsigned-byte 8) :buffered nil))
telnet localhost 1234
(write-byte-sequence (coerce-byte-array '(65 66 67 68)) s)
Expected: 4 without hanging, output ABCD
Expected syscalls: 1 x write()

(write-byte-sequence (coerce-byte-array '(69 70 71 72 13 10)) s)
Expected: 6 without hanging, output EFGH
Expected syscalls: 1 x write()

(close s)
Expected: telnet close

(setq s (socket-accept (socket-server 1235)
                       :element-type '(unsigned-byte 8) :buffered t))
telnet localhost 1235
(write-byte-sequence (coerce-byte-array '(65 66 67 68)) s)
Expected: 4 without hanging but no output
Expected syscalls: none

(force-output s)
Expected: output ABCD
Expected syscalls: 1 x write()

(write-byte-sequence (coerce-byte-array '(69 70 71 72 13 10)) s)
Expected: 6 without hanging but no output
Expected syscalls: none

(close s)
Expected: output EFGH then telnet close
Expected syscalls: 1 x write(), 1 x close()

Status:
Linux   2004-10-31 OK
FreeBSD 2004-10-31 OK
BeOS    2004-10-31 OK

|#

(defparameter *server* (socket-server)) *server*
(multiple-value-list (socket-status *server* 0)) (NIL 0)

(defparameter *socket-1*
  (socket-connect (socket-server-port *server*) "localhost" :timeout 0))
*socket-1*

(defparameter *status-arg* (list (list *server*) (list *socket-1* :io)))
*status-arg*
(eq (socket-status *status-arg* 0) *status-arg*) T
(cdr (assoc *server* *status-arg*))    T
(cddr (assoc *socket-1* *status-arg*)) :OUTPUT

(defparameter *socket-2* (socket-accept *server*)) *socket-2*
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
(cddr (assoc *socket-2* *status-arg*)) :IO

(multiple-value-list (read-line *socket-2*)) ("foo" NIL)

(close *socket-1*) T
(close *socket-2*) T
(socket-server-close *server*) NIL

;; clean-up
(progn (makunbound '*server*) (unintern '*server*)
       (makunbound '*socket-1*) (unintern '*socket-1*)
       (makunbound '*socket-2*) (unintern '*socket-2*))
T
