;;; Lisp wrappers for the GLIBC FFI

(in-package "SYS")

(require "linux")

(defpackage "LINUX"
  (:case-sensitive t)
  (:nicknames "UNIX" "GLIBC")
  (:use)
  (:export "real-path" "get-host-name" "get-domain-name"
           "linux-error" "check-res"))

(eval-when (compile load eval)
  (setf (package-lock "SYS") nil))
(push "LINUX" *system-package-list*)

(defun vec2string (vec)
  ;; Convert a char[] to a lisp STRING.
  (convert-string-from-bytes vec *foreign-encoding*
                             :end (position 0 vec)))

(defun linux:linux-error (caller)
  (error "~s: ~a" caller (linux::strerror linux::errno)))
(defmacro linux:check-res (res caller)
  `(unless (zerop ,res) (linux:linux-error ,caller)))

(defun linux:real-path (name)
  (multiple-value-bind (success resolved)
      ;; :out or :in-out parameters are returned via multiple values
      (linux::realpath name)
    (unless success (linux:linux-error 'linux:real-path))
    (vec2string resolved)))

(defun linux:get-host-name ()
  (multiple-value-bind (success name)
      ;; :out or :in-out parameters are returned via multiple values
      (linux::gethostname linux::MAXHOSTNAMELEN)
    (linux:check-res success 'linux:get-host-name)
    (vec2string name)))

(defun linux:get-domain-name ()
  (multiple-value-bind (success name)
      ;; :out or :in-out parameters are returned via multiple values
      (linux::getdomainname linux::MAXHOSTNAMELEN)
    (linux:check-res success 'linux:get-domain-name)
    (vec2string name)))

(eval-when (compile load eval)
  (setf (package-lock *system-package-list*) t))
