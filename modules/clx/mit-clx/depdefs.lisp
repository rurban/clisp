;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;; This file contains some of the system dependent code for CLX

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package :xlib)

#+cmu
(eval-when (compile load eval)
  (let ((vs (lisp-implementation-version)))
    (when (and (<= 2 (length vs))
	       (eql #\1 (aref vs 0))
	       (let ((d (digit-char-p (aref vs 1))))
		 (and d (<= 6 d))))
      (pushnew :cmu16 *features*))))

#+CLISP
(eval-when (compile load eval)
  (when (find-symbol "PRINT-UNREADABLE-OBJECT" "LISP")
    (pushnew :have-print-unreadable-object *features*)
  )
  (when (find-symbol "DECLAIM" "LISP")
    (pushnew :have-declaim *features*)
  )
  (when (find-symbol "SIGNAL" "LISP")
    (pushnew :have-clcs *features*)
  )
  (when (find-symbol "WITH-STANDARD-IO-SYNTAX" "LISP")
    (pushnew :have-with-standard-io-syntax *features*)
  )
  (when (find-symbol "READ-BYTE-LOOKAHEAD" "SYSTEM")
    (pushnew :have-read-byte-lookahead *features*)
) )

;;;-------------------------------------------------------------------------
;;; Declarations
;;;-------------------------------------------------------------------------

;;; fix a bug in kcl's RATIONAL...
;;;   redefine both the function and the type.

#+(or kcl ibcl)
(progn
  (defun rational (x)
    (if (rationalp x)
	x
	(lisp:rational x)))
  (deftype rational (&optional l u) `(lisp:rational ,l ,u)))

;;; DECLAIM

#-(or clx-ansi-common-lisp (and CLISP have-declaim))
(defmacro declaim (&rest decl-specs)
  (if (cdr decl-specs)
      `(progn
	 ,@(mapcar #'(lambda (decl-spec) `(proclaim ',decl-spec))
		   decl-specs))
    `(proclaim ',(car decl-specs))))

;;; VALUES value1 value2 ... -- Documents the values returned by the function.

#-lispm
(declaim (declaration values))

;;; ARGLIST arg1 arg2 ... -- Documents the arglist of the function.  Overrides
;;; the documentation that might get generated by the real arglist of the
;;; function.

#-(or lispm lcl3.0)
(declaim (declaration arglist))

;;; DYNAMIC-EXTENT var -- Tells the compiler that the rest arg var has
;;; dynamic extent and therefore can be kept on the stack and not copied to
;;; the heap, even though the value is passed out of the function.

#-(or clx-ansi-common-lisp lcl3.0)
(declaim (declaration dynamic-extent))

;;; IGNORABLE var -- Tells the compiler that the variable might or might not be used.

#-clx-ansi-common-lisp
(declaim (declaration ignorable))

;;; ARRAY-REGISTER var1 var2 ... -- The variables mentioned are locals (not
;;; args) that hold vectors.  

#-Genera 
(declaim (declaration array-register))

;;; INDENTATION argpos1 arginden1 argpos2 arginden2 --- Tells the lisp editor how to
;;; indent calls to the function or macro containing the declaration.  

#-genera
(declaim (declaration indentation))

;;;-------------------------------------------------------------------------
;;; Declaration macros
;;;-------------------------------------------------------------------------

;;; WITH-VECTOR (variable type) &body body --- ensures the variable is a local
;;; and then does a type declaration and array register declaration
(defmacro with-vector ((var type) &body body)
  `(let ((,var ,var))
     (declare (type ,type ,var)
	      (array-register ,var))
     ,@body))

;;; WITHIN-DEFINITION (name type) &body body --- Includes definitions for
;;; Meta-.

#+lispm
(defmacro within-definition ((name type) &body body)
  `(zl:local-declare
     ((sys:function-parent ,name ,type))
     (sys:record-source-file-name ',name ',type)
     ,@body))

#-lispm
(defmacro within-definition ((name type) &body body)
  (declare (ignore name type))
  `(progn ,@body))


;;;-------------------------------------------------------------------------
;;; CLX can maintain a mapping from X server ID's to local data types.  If
;;; one takes the view that CLX objects will be instance variables of
;;; objects at the next higher level, then PROCESS-EVENT will typically map
;;; from resource-id to higher-level object.  In that case, the lower-level
;;; CLX mapping will almost never be used (except in rare cases like
;;; query-tree), and only serve to consume space (which is difficult to
;;; GC), in which case always-consing versions of the make-<mumble>s will
;;; be better.  Even when maps are maintained, it isn't clear they are
;;; useful for much beyond xatoms and windows (since almost nothing else
;;; ever comes back in events).
;;;--------------------------------------------------------------------------
(defconstant *clx-cached-types*
	     '( drawable
		window
		pixmap
;		gcontext
		cursor
		colormap
		font))

(defmacro resource-id-map-test ()
  #+excl '#'equal
  #-excl '#'eql)
					; (eq fixnum fixnum) is not guaranteed.
(defmacro atom-cache-map-test ()
  #+excl '#'equal
  #-excl '#'eq)

(defmacro keysym->character-map-test ()
  #+excl '#'equal
  #-excl '#'eql)

;;; You must define this to match the real byte order.  It is used by
;;; overlapping array and image code.

#+(or lispm vax little-endian i386 Minima)
(eval-when (eval compile load)
  (pushnew :clx-little-endian *features*))

#+lcl3.0
(eval-when (compile eval load)
  (ecase lucid::machine-endian
    (:big nil)
    (:little (pushnew :clx-little-endian *features*))))

#+cmu
(eval-when (compile eval load)
  (ecase #.(c:backend-byte-order c:*backend*)
    (:big-endian)
    (:little-endian (pushnew :clx-little-endian *features*))))

#+CLISP
(eval-when (compile eval load)
  (unless system::*big-endian* (pushnew :clx-little-endian *features*))
)

;;; Steele's Common-Lisp states:  "It is an error if the array specified
;;; as the :displaced-to argument  does not have the same :element-type
;;; as the array being created" If this is the case on your lisp, then
;;; leave the overlapping-arrays feature turned off.  Lisp machines
;;; (Symbolics TI and LMI) don't have this restriction, and allow arrays
;;; with different element types to overlap.  CLX will take advantage of
;;; this to do fast array packing/unpacking when the overlapping-arrays
;;; feature is enabled.

#+(and clx-little-endian lispm)
(eval-when (eval compile load)
  (pushnew :clx-overlapping-arrays *features*))

#+(and clx-overlapping-arrays genera)
(progn
(deftype overlap16 () '(unsigned-byte 16))
(deftype overlap32 () '(signed-byte 32))
)

#+(and clx-overlapping-arrays (or explorer lambda cadr))
(progn
(deftype overlap16 () '(unsigned-byte 16))
(deftype overlap32 () '(unsigned-byte 32))
)

(deftype buffer-bytes () `(simple-array (unsigned-byte 8) (*)))

#+clx-overlapping-arrays
(progn
(deftype buffer-words () `(vector overlap16))
(deftype buffer-longs () `(vector overlap32))
)

;;; This defines a type which is a subtype of the integers.
;;; This type is used to describe all variables that can be array indices.
;;; It is here because it is used below.
;;; This is inclusive because start/end can be 1 past the end.
(deftype array-index () `(integer 0 ,array-dimension-limit))


;; this is the best place to define these?

#-Genera
(progn

(defun make-index-typed (form)
  (if (constantp form) form `(the array-index ,form)))

(defun make-index-op (operator args)
  `(the array-index
	(values 
	  ,(case (length args)
	     (0 `(,operator))
	     (1 `(,operator
		  ,(make-index-typed (first args))))
	     (2 `(,operator
		  ,(make-index-typed (first args))
		  ,(make-index-typed (second args))))
	     (otherwise
	       `(,operator
		 ,(make-index-op operator (subseq args 0 (1- (length args))))
		 ,(make-index-typed (first (last args)))))))))

(defmacro index+ (&rest numbers) (make-index-op '+ numbers))
(defmacro index-logand (&rest numbers) (make-index-op 'logand numbers))
(defmacro index-logior (&rest numbers) (make-index-op 'logior numbers))
(defmacro index- (&rest numbers) (make-index-op '- numbers))
(defmacro index* (&rest numbers) (make-index-op '* numbers))

(defmacro index1+ (number) (make-index-op '1+ (list number)))
(defmacro index1- (number) (make-index-op '1- (list number)))

(defmacro index-incf (place &optional (delta 1))
  (make-index-op 'incf (list place delta)))
(defmacro index-decf (place &optional (delta 1))
  (make-index-op 'decf (list place delta)))

(defmacro index-min (&rest numbers) (make-index-op 'min numbers))
(defmacro index-max (&rest numbers) (make-index-op 'max numbers))

(defmacro index-floor (number divisor)
  (make-index-op 'floor (list number divisor)))
(defmacro index-ceiling (number divisor)
  (make-index-op 'ceiling (list number divisor)))
(defmacro index-truncate (number divisor)
  (make-index-op 'truncate (list number divisor)))

(defmacro index-mod (number divisor)
  (make-index-op 'mod (list number divisor)))

(defmacro index-ash (number count)
  (make-index-op 'ash (list number count)))

(defmacro index-plusp (number) `(plusp (the array-index ,number)))
(defmacro index-zerop (number) `(zerop (the array-index ,number)))
(defmacro index-evenp (number) `(evenp (the array-index ,number)))
(defmacro index-oddp  (number) `(oddp  (the array-index ,number)))

(defmacro index> (&rest numbers)
  `(> ,@(mapcar #'make-index-typed numbers)))
(defmacro index= (&rest numbers)
  `(= ,@(mapcar #'make-index-typed numbers)))
(defmacro index< (&rest numbers)
  `(< ,@(mapcar #'make-index-typed numbers)))
(defmacro index>= (&rest numbers)
  `(>= ,@(mapcar #'make-index-typed numbers)))
(defmacro index<= (&rest numbers)
  `(<= ,@(mapcar #'make-index-typed numbers)))

)

#+Genera
(progn

(defmacro index+ (&rest numbers) `(+ ,@numbers))
(defmacro index-logand (&rest numbers) `(logand ,@numbers))
(defmacro index-logior (&rest numbers) `(logior ,@numbers))
(defmacro index- (&rest numbers) `(- ,@numbers))
(defmacro index* (&rest numbers) `(* ,@numbers))

(defmacro index1+ (number) `(1+ ,number))
(defmacro index1- (number) `(1- ,number))

(defmacro index-incf (place &optional (delta 1)) `(setf ,place (index+ ,place ,delta)))
(defmacro index-decf (place &optional (delta 1)) `(setf ,place (index- ,place ,delta)))

(defmacro index-min (&rest numbers) `(min ,@numbers))
(defmacro index-max (&rest numbers) `(max ,@numbers))

(defun positive-power-of-two-p (x)
  (when (symbolp x)
    (multiple-value-bind (constantp value) (lt:named-constant-p x)
      (when constantp (setq x value))))
  (and (typep x 'fixnum) (plusp x) (zerop (logand x (1- x)))))

(defmacro index-floor (number divisor)
  (cond ((eql divisor 1) number)
	((and (positive-power-of-two-p divisor) (fboundp 'si:%fixnum-floor))
	 `(si:%fixnum-floor ,number ,divisor))
	(t `(floor ,number ,divisor))))

(defmacro index-ceiling (number divisor)
  (cond ((eql divisor 1) number)
	((and (positive-power-of-two-p divisor) (fboundp 'si:%fixnum-ceiling))
	 `(si:%fixnum-ceiling ,number ,divisor))
	(t `(ceiling ,number ,divisor))))

(defmacro index-truncate (number divisor)
  (cond ((eql divisor 1) number)
	((and (positive-power-of-two-p divisor) (fboundp 'si:%fixnum-floor))
	 `(si:%fixnum-floor ,number ,divisor))
	(t `(truncate ,number ,divisor))))

(defmacro index-mod (number divisor)
  (cond ((and (positive-power-of-two-p divisor) (fboundp 'si:%fixnum-mod))
	 `(si:%fixnum-mod ,number ,divisor))
	(t `(mod ,number ,divisor))))

(defmacro index-ash (number count)
  (cond ((eql count 0) number)
	((and (typep count 'fixnum) (minusp count) (fboundp 'si:%fixnum-floor))
	 `(si:%fixnum-floor ,number ,(expt 2 (- count))))
	((and (typep count 'fixnum) (plusp count) (fboundp 'si:%fixnum-multiply))
	 `(si:%fixnum-multiply ,number ,(expt 2 count)))
	(t `(ash ,number ,count))))

(defmacro index-plusp (number) `(plusp ,number))
(defmacro index-zerop (number) `(zerop ,number))
(defmacro index-evenp (number) `(evenp ,number))
(defmacro index-oddp  (number) `(oddp  ,number))

(defmacro index> (&rest numbers) `(> ,@numbers))
(defmacro index= (&rest numbers) `(= ,@numbers))
(defmacro index< (&rest numbers) `(< ,@numbers))
(defmacro index>= (&rest numbers) `(>= ,@numbers))
(defmacro index<= (&rest numbers) `(<= ,@numbers))

)

;;;; Stuff for BUFFER definition

(defconstant *replysize* 32.)

;; used in defstruct initializations to avoid compiler warnings
(defvar *empty-bytes* (make-sequence 'buffer-bytes 0))
(declaim (type buffer-bytes *empty-bytes*))
#+clx-overlapping-arrays
(progn
(defvar *empty-words* (make-sequence 'buffer-words 0))
(declaim (type buffer-words *empty-words*))
)
#+clx-overlapping-arrays
(progn
(defvar *empty-longs* (make-sequence 'buffer-longs 0))
(declaim (type buffer-longs *empty-longs*))
)

(defstruct (reply-buffer (:conc-name reply-) (:constructor make-reply-buffer-internal)
			 (:copier nil) (:predicate nil))
  (size 0 :type array-index)			;Buffer size
  ;; Byte (8 bit) input buffer
  (ibuf8 *empty-bytes* :type buffer-bytes)
  ;; Word (16bit) input buffer
  #+clx-overlapping-arrays
  (ibuf16 *empty-words* :type buffer-words)
  ;; Long (32bit) input buffer
  #+clx-overlapping-arrays
  (ibuf32 *empty-longs* :type buffer-longs)
  (next nil #-explorer :type #-explorer (or null reply-buffer))
  (data-size 0 :type array-index)
  )

(defconstant *buffer-text16-size* 256)
(deftype buffer-text16 () `(simple-array (unsigned-byte 16) (,*buffer-text16-size*)))

;; These are here because.

(defparameter *xlib-package* (find-package :xlib))

(defun xintern (&rest parts)
  (intern (apply #'concatenate 'string (mapcar #'string parts)) *xlib-package*))

(defparameter *keyword-package* (find-package :keyword))

(defun kintern (name)
  (intern (string name) *keyword-package*))

;;; Pseudo-class mechanism.

(eval-when (eval compile load)
(defvar *def-clx-class-use-defclass* #+Genera t #-Genera nil
  "Controls whether DEF-CLX-CLASS uses DEFCLASS.  
   If it is a list, it is interpreted by DEF-CLX-CLASS to be a list of type names
   for which DEFCLASS should be used. 
   If it is not a list, then DEFCLASS is always used.
   If it is NIL, then DEFCLASS is never used, since NIL is the empty list.")

;;************
#-(and (or CLISP AKCL) (not PCL)) ;; You may remove this line for CLISP with native CLOS.
(setq *def-clx-class-use-defclass* '(window drawable pixmap))
#+pcl (setq pcl::*defclass-times*   '(compile load eval))
)

(defmacro def-clx-class ((name &rest options) &body slots)
  (if (or (not (listp *def-clx-class-use-defclass*))
	  (member name *def-clx-class-use-defclass*))
      (let ((clos-package #+clx-ansi-common-lisp
			  (find-package :common-lisp)
			  #-clx-ansi-common-lisp
			  (or (find-package :pcl) ; *** switched ***
			      (find-package :clos)
			      (let ((lisp-pkg (find-package :lisp)))
				(and (find-symbol (string 'defclass) lisp-pkg)
				     lisp-pkg))))
	    (constructor t)
	    (constructor-args t)
	    (include nil)
	    (print-function nil)
	    (copier t)
	    (predicate t))
	(dolist (option options)
	  (ecase (pop option)
	    (:constructor
	      (setf constructor (pop option))
	      (setf constructor-args (if (null option) t (pop option))))
	    (:include
	      (setf include (pop option)))
	    (:print-function
	      (setf print-function (pop option)))
	    (:copier
	      (setf copier (pop option)))
	    (:predicate
	      (setf predicate (pop option)))))
	(flet ((cintern (&rest symbols)
		 (intern (apply #'concatenate 'simple-string
				(mapcar #'symbol-name symbols))
			 *package*))
	       (kintern (symbol)
			(intern (symbol-name symbol) (find-package :keyword)))
	       (closintern (symbol)
		 (intern (symbol-name symbol) clos-package)))
	  (when (eq constructor t)
	    (setf constructor (cintern 'make- name)))
	  (when (eq copier t)
	    (setf copier (cintern 'copy- name)))
	  (when (eq predicate t)
	    (setf predicate (cintern name '-p)))
	  (when include
	    (setf slots (append (get include 'def-clx-class) slots)))
	  (let* ((n-slots (length slots))
		 (slot-names (make-list n-slots))
		 (slot-initforms (make-list n-slots))
		 (slot-types (make-list n-slots)))
	    (dotimes (i n-slots)
	      (let ((slot (elt slots i)))
		(setf (elt slot-names i) (pop slot))
		(setf (elt slot-initforms i) (pop slot))
		(setf (elt slot-types i) (getf slot :type t))))
	    `(progn

	       (eval-when (compile load eval)
		 (setf (get ',name 'def-clx-class) ',slots))

	       ;; From here down are the system-specific expansions:

	       (within-definition (,name def-clx-class)
		 (,(closintern 'defclass)
		  ,name ,(and include `(,include))
		  (,@(map 'list
			  #'(lambda (slot-name slot-initform slot-type)
			      `(,slot-name
				:initform ,slot-initform :type ,slot-type
				:accessor ,(cintern name '- slot-name)
				,@(when (and constructor
					     (or (eq constructor-args t)
						 (member slot-name
							 constructor-args)))
				    `(:initarg ,(kintern slot-name)))
				))
			  slot-names slot-initforms slot-types)))
		 ,(when constructor
		    (if (eq constructor-args t)
			`(defun ,constructor (&rest args)
			   (apply #',(closintern 'make-instance)
				  ',name args))
			`(defun ,constructor ,constructor-args
			   (,(closintern 'make-instance) ',name
			    ,@(mapcan #'(lambda (slot-name)
					  (and (member slot-name slot-names)
					       `(,(kintern slot-name) ,slot-name)))
				      constructor-args)))))
		 ,(when predicate
		    #+(or allegro pcl)
		    `(progn
		       (,(closintern 'defmethod) ,predicate (object)
			 (declare (ignore object))
			 nil)
		       (,(closintern 'defmethod) ,predicate ((object ,name))
			 t))
		    #-(or allegro pcl)
		    `(defun ,predicate (object)
		       (typep object ',name)))
		 ,(when copier
		    `(,(closintern 'defmethod) ,copier ((.object. ,name))
		      (,(closintern 'with-slots) ,slot-names .object.
		       (,(closintern 'make-instance) ',name
			,@(mapcan #'(lambda (slot-name)
				      `(,(kintern slot-name) ,slot-name))
				  slot-names)))))
		 ,(when print-function
		    `(,(closintern 'defmethod)
		      ,(closintern 'print-object)
		      ((object ,name) stream)
		      (,print-function object stream 0))))))))
      `(within-definition (,name def-clx-class)
	 (defstruct (,name ,@options)
	   ,@slots))))

#+Genera
(progn
  (scl:defprop def-clx-class "CLX Class" si:definition-type-name)
  (scl:defprop def-clx-class zwei:defselect-function-spec-finder
	       zwei:definition-function-spec-finder))


;; We need this here so we can define DISPLAY for CLX.
;;
;; This structure is :INCLUDEd in the DISPLAY structure.
;; Overlapping (displaced) arrays are provided for byte
;; half-word and word access on both input and output.
;;
(def-clx-class (buffer (:constructor nil) (:copier nil) (:predicate nil))
  ;; Lock for multi-processing systems
  (lock (make-process-lock "CLX Buffer Lock"))
  #-excl (output-stream nil :type (or null stream))
  #+excl (output-stream -1 :type fixnum)
  ;; Buffer size
  (size 0 :type array-index)
  (request-number 0 :type (unsigned-byte 16))
  ;; Byte position of start of last request
  ;; used for appending requests and error recovery
  (last-request nil :type (or null array-index))
  ;; Byte position of start of last flushed request
  (last-flushed-request nil :type (or null array-index))
  ;; Current byte offset
  (boffset 0 :type array-index)
  ;; Byte (8 bit) output buffer
  (obuf8 *empty-bytes* :type buffer-bytes)
  ;; Word (16bit) output buffer
  #+clx-overlapping-arrays
  (obuf16 *empty-words* :type buffer-words)
  ;; Long (32bit) output buffer
  #+clx-overlapping-arrays
  (obuf32 *empty-longs* :type buffer-longs)
  ;; Holding buffer for 16-bit text
  (tbuf16 (make-sequence 'buffer-text16 *buffer-text16-size* :initial-element 0))
  ;; Probably EQ to Output-Stream
  #-excl (input-stream nil :type (or null stream))
  #+excl (input-stream -1 :type fixnum)
  ;; T when the host connection has gotten errors
  (dead nil :type (or null (not null)))
  ;; T makes buffer-flush a noop.  Manipulated with with-buffer-flush-inhibited.
  (flush-inhibit nil :type (or null (not null)))
  
  ;; Change these functions when using shared memory buffers to the server
  ;; Function to call when writing the buffer
  (write-function 'buffer-write-default)
  ;; Function to call when flushing the buffer
  (force-output-function 'buffer-force-output-default)
  ;; Function to call when closing a connection
  (close-function 'buffer-close-default)
  ;; Function to call when reading the buffer
  (input-function 'buffer-read-default)
  ;; Function to call to wait for data to be input
  (input-wait-function 'buffer-input-wait-default)
  ;; Function to call to listen for input data
  (listen-function 'buffer-listen-default)

  #+Genera (debug-io nil :type (or null stream))
  ) 

;;-----------------------------------------------------------------------------
;; Printing routines.
;;-----------------------------------------------------------------------------

#-(or clx-ansi-common-lisp Genera CMU (and CLISP have-print-unreadable-object))
(defun print-unreadable-object-function (object stream type identity function)
  (declare #+lispm
	   (sys:downward-funarg function))
  (princ "#<" stream)
  (when type
    (let ((type (type-of object))
	  (pcl-package (find-package :pcl)))
      ;; Handle pcl type-of lossage
      (when (and pcl-package
		 (symbolp type)
		 (eq (symbol-package type) pcl-package)
		 (string-equal (symbol-name type) "STD-INSTANCE"))
	(setq type
	      (funcall (intern (symbol-name 'class-name) pcl-package)
		       (funcall (intern (symbol-name 'class-of) pcl-package)
				object))))
      (prin1 type stream)))
  (when (and type function) (princ " " stream))
  (when function (funcall function))
  (when (and (or type function) identity) (princ " " stream))
  (when identity
    #-CLISP (princ "???" stream)
    #+CLISP (format stream "#x~8,'0X" (sys::address-of object))
  )
  (princ ">" stream)
  nil)
  
#-(or clx-ansi-common-lisp Genera CMU (and CLISP have-print-unreadable-object))
(defmacro print-unreadable-object
	  ((object stream &key type identity) &body body)
  (if body
      `(flet ((.print-unreadable-object-body. () ,@body))
	 (print-unreadable-object-function
	   ,object ,stream ,type ,identity #'.print-unreadable-object-body.))
    `(print-unreadable-object-function ,object ,stream ,type ,identity nil)))


;;-----------------------------------------------------------------------------
;; Image stuff
;;-----------------------------------------------------------------------------

(defconstant *image-bit-lsb-first-p*
	     #+clx-little-endian t
	     #-clx-little-endian nil)

(defconstant *image-byte-lsb-first-p*
	     #+clx-little-endian t
	     #-clx-little-endian nil)

(defconstant *image-unit* 32)

(defconstant *image-pad* 32)


;;-----------------------------------------------------------------------------
;; Foreign Functions
;;-----------------------------------------------------------------------------

#+(and lucid apollo (not lcl3.0))
(lucid::define-foreign-function '(connect-to-server "connect_to_server")
  '((:val host    :string)
    (:val display :integer32))
  :integer32)

#+(and lucid (not apollo) (not lcl3.0))
(lucid::define-c-function connect-to-server (host display)
  :result-type :integer)

#+lcl3.0
(lucid::def-foreign-function
    (connect-to-server 
      (:language :c)
      (:return-type :signed-32bit))
  (host :simple-string)
  (display :signed-32bit))

#+(and CMU (not cmu16))
(ext:def-c-routine ("connect_to_server" connect-to-server) (ext:int)
  (host system:null-terminated-string)
  (port ext:int))

#+cmu16
(alien:def-alien-routine ("connect_to_server" xlib::connect-to-server)
			   c-call:int
    (host c-call:c-string)
    (port c-call:int))
