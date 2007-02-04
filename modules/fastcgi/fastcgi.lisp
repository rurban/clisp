;
; CLISP FastCGI interface
;
; Copyright (C) 2003 Alma Mater Software, Inc., Tarrytown, NY, USA
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License version 2 as
; published by the Free Software Foundation; see file GNU-GPL.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software Foundation,
; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;
; $Id$

(defpackage "FASTCGI"
  (:documentation "Minimal bindings for FastCGI use from CLISP")
  (:use "CL" "FFI")
  (:export "GETENV"
           "SLURP-STDIN" "OUT" "WRITE-STDOUT" "WRITE-STDERR" "NL"
           "IS-CGI" "ACCEPT" "FINISH" "WITH-LISTENER")
)

(in-package "FASTCGI")
(pushnew :fastcgi *features*)

(setf (documentation (find-package "FASTCGI") 'sys::impnotes) "fastcgi")

(FFI:default-foreign-language :STDC)

; Global: is request active?
(defvar *fastcgi-request-active* nil)

; -----------   Exported functions

; IS-CGI
(defun is-cgi ()
  "Return T iff this is an old-fashioned CGI request rather than FastCGI mode."
  (fcgi_is_cgi_wrapper))

; ACCEPT
(defun accept ()
  "Place at the top of an explicit FastCGI server loop.  Returns T iff there is a request to do, otherwise consider exiting the application."
  (setf *fastcgi-request-active* t)
  (= 0 (fcgi_accept_wrapper)))

; FINISH
(defun finish ()
  "Place at the bottom of an explicit FastCGI server loop.  Always returns NIL."
  (check-active-request 'finish)
  (setf *fastcgi-request-active* nil)
  (fcgi_finish_wrapper)
  nil)

; GETENV
(defun getenv (&optional var)
  "(FASTCGI:GETENV var) - Gets the value of an environment variable, which should be a string; if called with no argument, returns the entire environment as an alist"
  (if var (fcgi_getenv (to-string var)) (env)))

; ENV -- Return entire environment as an alist
(defun env ()
  "ENV - Returns the entire set of environment variables as an alist"
  (do* ((kv (fcgi_env))
        (result nil)
        (i (- (length kv) 2) (- i 2)))
       ((< i 0) result)
    (push (cons (aref kv i) (aref kv (1+ i))) result)))

; WRITE-STDOUT
(defun write-stdout (data)
  "(FASTCGI:WRITE-STDOUT string) - Write a string to standard output"
  ;; Do it in chunks since there seems to be FFI problems with large buffers
  (do* ((chunksize 65536)
        (s (to-string data))
        (totlen (length s)))
       ((= 0 (length s)) totlen)
    (let ((to-write (min (length s) chunksize)))
      (fcgi_write_stdout (subseq s 0 to-write) to-write)
      (setf s (subseq s to-write)))))

; WRITE-STDERR
(defun write-stderr (data)
  "(FASTCGI:WRITE-STDERR string) - Write a string to standard error"
  (let ((s (to-string data)))
    (fcgi_write_stderr s (length s))))

; SLURP-STDIN
(defun slurp-stdin ()
  "(FASTCGI:SLURP-STDIN)  Reads in the entirety of standard input and returns as a string"
  (check-active-request 'slurp-stdin)
  (do ((result "")
       (eof nil))
      (eof result)
      (let ((buf (byte-array-to-string (hex-to-byte-array (fcgi_read_stdin)))))
        (if (= 0 (length buf))
            (setf eof t)
          (setf result (ext:string-concat result buf))))))

; Output functions

; OUT
(defun out (&rest args)
  "(FASTCGI:OUT args ...) Write arguments to standard output"
  (write-stdout (cat args)))

; NL
; Return newline
(defun nl () "Return a newline" #\Newline)


; ----------------    Internal functions

;; HEX-VALUE -- Get integer value of single upper-case hex digit
(defun hex-value (h)
  (cond ((and (char>= h #\A) (char<= h #\F))
         (+ 10 (- (char-code h) (char-code #\A))))
        ((and (char>= h #\a) (char<= h #\f))
         (+ 10 (- (char-code h) (char-code #\a))))
        ((and (char>= h #\0) (char<= h #\9))
         (- (char-code h) (char-code #\0)))
        (t (error "Invalid hex digit"))))

;; HEX-BYTE-VALUE -- Get byte value of pair of hex digits
(defun hex-byte-value (hh)
  (+ (* 16 (hex-value (elt hh 0)))
     (hex-value (elt hh 1))))

;; HEX-TO-BYTE-ARRAY -- Convert hex string to byte array
(defun hex-to-byte-array (h)
  (let* ((size (/ (length h) 2))
         (result (make-array size :element-type '(unsigned-byte 8))))
    (loop
     for i from 0 to (1- size) do
     (let ((offset (* 2 i)))
       (setf (elt result i)
             (coerce (hex-byte-value (subseq h offset (+ 2 offset)))
                     '(unsigned-byte 8)))))
    result))

;; BYTE-ARRAY-TO-STRING -- Convert byte array to string
(defun byte-array-to-string (b) (map 'string #'code-char b))

; CAT
; Concatenate strings
(defun cat (&rest args)
  (apply #'ext:string-concat (mapcar #'to-string (flatten args))))

; FLATTEN
; Flatten list (lifted from Paul Graham)
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

; TO-STRING
; Convert object to a string; NIL -> ""
(defun to-string (s)
  (cond ((null s) "")
        ((stringp s) s)
        ((symbolp s) (symbol-name s))
        (t (princ-to-string s))))

; CHECK-ACTIVE-REQUEST - Sanity check on use of library function
(defun check-active-request (func)
  (when (not *fastcgi-request-active*)
    (error "Need to call FASTCGI:ACCEPT before using ~S" func)))



;; --------------   "C" functions
(eval-when (compile)
  ;;NB this global affects further compilations in this session
  (setq ffi:*output-c-functions* t))

; Our wrappers
(def-call-out fcgi_getenv       (:arguments (var c-string))               (:return-type c-string))
(def-call-out fcgi_env          (:arguments)                              (:return-type (c-array-ptr c-string) :malloc-free))
(def-call-out fcgi_read_stdin   (:arguments)                              (:return-type c-string))
(def-call-out fcgi_write_stdout (:arguments (data c-string) (length int)) (:return-type int))
(def-call-out fcgi_write_stderr (:arguments (data c-string) (length int)) (:return-type int))

; Direct passthroughs to FCGI library
(def-call-out fcgi_accept_wrapper (:arguments) (:return-type int))
(def-call-out fcgi_finish_wrapper (:arguments) (:return-type nil))
(def-call-out fcgi_is_cgi_wrapper (:arguments) (:return-type boolean))

; End of fastcgi.lisp
