;;; -*- Mode: Lisp; Package: DSYS; Base: 10; Syntax: Common-Lisp -*-

(in-package "DSYS")

(defsystem clx
    (:pretty-name "CLX")
  (:module clos pcl (:type :system))
  (:module clcs clcs (:type :system))
  #+kcl (:module common-lisp common-lisp (:type :system))
  (:parallel
   #+kcl "kcl-system-patches"
   #+kcl "kcl-compiler-patches"
   clos
   clcs
   #+kcl common-lisp
   (:forms :compile 
	   (progn
	     #+cmu17a
	     (when (find-package "XLIB")
	       (pcl::reset-package "XLIB"))
	     (setq *compile-system-proclamation* *normal-declaration*)
	     (proclaim *compile-system-proclamation*)
	     #+akcl 
	     (progn
	       (setq compiler::*compile-ordinaries* t)
	       (compiler::emit-fn t)
	       (let ((*use-default-pathname-type* nil))
		 (load "sys-package")
		 (load "sys-proclaim"))
	       (load (merge-pathnames "../lsp/sys-proclaim.lisp" 
				      si::*system-directory*)))
	     #+cmu (setq c::*suppress-values-declaration* t)
	     #+(and unix (or excl kcl lucid)) (compile-c "socket")))
   (:parallel
    (:forms :compile (proclaim *fast-declaration*))
    "package"
    #+kcl
    (:file :name "sockcl"
	   #+(and kcl bsd (not mips)) :link 
	   #+(and kcl bsd (not mips)) (format nil #-|NeXT| "~A -lc" 
					          #+|NeXT| "~A -lsys_s"
					      (namestring
					       (merge-pathnames "socket.o"))))
    #+lucid
    (:forms :compile (load-socket)
	    :load (load-socket))
    #+excl "excldep"
    "depdefs"
    "clx"
    "dependent"
    #+excl "exclcmac"			; these are just macros
    "macros"				; these are just macros
    "bufmac"				; these are just macros
    "buffer"
    (:forms :compile (proclaim *normal-declaration*))
    "display"
    "gcontext"
    "input"
    "requests"
    "fonts"
    "graphics"
    "text"
    "attributes"
    "translate"
    "keysyms"
    "manager"
    "image"
    "resource"
    "describe"
    "trace"
    (:forms :compile 
	    (progn #+cmu (setq c::*suppress-values-declaration* nil)
		   #+cmu (purify))
	    :load
	    (progn #+cmu (purify))))))

#+lucid
(progn
(defvar *socket-loaded-p* nil)

(defun load-socket ()
  (unless *socket-loaded-p*
    #+apollo
    (lucid::load-foreign-file
     (namestring (merge-pathnames "socket"))
     :preserve-pathname t)
    #-apollo
    (lucid::load-foreign-files
     (list (namestring (merge-pathnames "socket.o")))
     '("-lc"))
    (setq *socket-loaded-p* t)))
)

#-cmu
(defun compile-c (filename)
  (let* ((c-filename (concatenate 'string filename ".c"))
	 (o-filename (concatenate 'string filename ".o"))
	 (src (merge-pathnames c-filename))
	 (obj  (merge-pathnames o-filename))
	 (args (list "-c" (namestring src)
		     "-o" (namestring obj)
		     #+mips "-G 0"
		     #+(or hp sysv) "-DSYSV"
		     #+(and mips (not dec)) "-I/usr/include/bsd"
		     #-(and mips (not dec)) "-DUNIXCONN")))
    (when (or (null (probe-file obj))
	      (< (file-write-date obj)
		 (file-write-date src)))
      (format t ";;; cc~{ ~A~}~%" args)
      (unless
	  (zerop 
	   #+lucid
	   (multiple-value-bind (iostream estream exitstatus pid)
	       ;; in 2.0, run-program is exported from system:
	       ;; in 3.0, run-program is exported from lcl:
	       ;; system inheirits lcl
	       (system::run-program "cc" :arguments args)
	     (declare (ignore iostream estream pid))
	     exitstatus)
	   #+(or kcl ibcl)
	   (system (format nil "cc~{ ~A~}" args)))
	(error "Compile of ~A failed." src)))))
	 
(defun user::compile-clx-demos ()
  (compile-file (subfile '("clx" "demo") :name "hello" :type "lisp"))
  (compile-file (subfile '("clx" "demo") :name "menu" :type "lisp")))

(defun user::load-clx-demos ()
  (load (subfile '("clx" "demo") :name "hello"))
  (load (subfile '("clx" "demo") :name "menu")))

(defparameter *clx-files*
  '((("systems") "lisp"
     "clx")
    (("clx") "lisp"
     "sysdef"
     "attributes" "buffer" "bufmac" "clx" "defsystem" "depdefs" "dependent"
     "display" "exclcmac" "excldep" "fonts" "gcontext" "generalock"
     "graphics" "image" "input" "keysyms" "macros" "manager" "package"
     "provide" "requests" "resource" "sockcl" "text" "translate"
     "describe" "trace")
    (("clx") "uu"
     "ms-patch")
    (("clx") "c"
     "excldep" "socket")
    (("clx") nil
     "CHANGES" "README" "exclMakefile" "exclREADME")
    (("clx" "debug") "lisp"
     "debug" "describe" "event-test" "keytrans" "trace" "util")
    (("clx" "demo") "lisp"
     "bezier" "beziertest" "hello" "menu" "zoid")
    (("clx" "demo") nil
     "README")
    (("clx" "test") "lisp"
     "image" "trapezoid")
    #||
    (("clx" "doc") nil
     "README")
    (("clx" "doc") "ps"
     "appa" "condition" "contents" "functions" "general" "glossary" 
     "s01" "s02" "s03" "s04" "s05" "s06" "s07" "s08" "s09" "s10"
     "s11" "s12" "s13" "s14" "s15" "s16" "types")
    ||#
    ))
