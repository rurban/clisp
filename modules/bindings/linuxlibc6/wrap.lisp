;;; Lisp wrappers for the GLIBC FFI

(in-package "SYS")

(require "linux")

(defpackage "LINUX"
  (:case-sensitive t)
  (:nicknames "UNIX" "GLIBC")
  (:use)
  (:export "real-path" "get-host-name" "get-domain-name"))

(eval-when (compile load eval)
  (setf (package-lock "SYS") nil))
(push "LINUX" *system-package-list*)

(defun vec2string (vec)
  ;; Convert a char[] to a lisp STRING.
  (convert-string-from-bytes (subseq vec 0 (position 0 vec))
                             *foreign-encoding*))

(defun linux-error (caller)
  (error "~s: ~a" caller (linux::strerror ; linux::errno
                          (linux::__errno_location))))

(defun linux:real-path (name)
  (multiple-value-bind (success resolved)
      ;; :out or :in-out parameters are returned via multiple values
      (linux::realpath name)
    (if success (vec2string resolved)
        (linux-error 'linux:real-path))))

(defun linux:get-host-name ()
  (multiple-value-bind (success name)
      ;; :out or :in-out parameters are returned via multiple values
      (linux::gethostname linux::MAXHOSTNAMELEN)
    (if (zerop success) (vec2string name)
        (linux-error 'linux:get-host-name))))

(defun linux:get-domain-name ()
  (multiple-value-bind (success name)
      ;; :out or :in-out parameters are returned via multiple values
      (linux::getdomainname linux::MAXHOSTNAMELEN)
    (if (zerop success) (vec2string name)
	(linux-error 'linux:get-domain-name))))

(eval-when (compile load eval)
  (setf (package-lock *system-package-list*) t))
