(in-package "XLIB")

#|| ; These are merged into the sources.
(defmacro def-inline (name args return-type flags string)
  (let* ((inline (list args return-type  flags string)))
    `(car (push ',inline (get ',name 'compiler::inline-always)))))

(defmacro defun-inline (name args return-type flags string)
  (let* ((sym (gensym))
         (named-args
          (nthcdr (- 10 (length args)) '(X9 X8 X7 X6 X5 X4 X3 X2 X1 X0)))
         (inline (eval `(def-inline ,sym ,args ,return-type ,flags ,string))))
    `(progn
       (defun ,name  ,named-args
         (declare ,@ (sloop::sloop for v in named-args for w in args
                            when (not (eq t v))
                            collect (list w v)))
         (the ,return-type (,sym ,@ named-args)))
       (push  ',inline (get ',name 'compiler::inline-always)))))

#+helper
`(progn ,@
      (sloop::sloop 
 for v in '(card29 int8 card8  int16 card16 int32)
 for w in '("unsigned long"
	    "char" "unsigned char" "short" "unsigned short" "long")
 for name = (intern (format nil "AREF-~a"v))
 for name-set = (intern (format nil "ASET-~a"v))
 collect
 `(defun-inline  ,name (t fixnum) fixnum #.(compiler::flags compiler::rfa)
	 ,(format nil "(*(~a *)(&((#0)->ust.ust_self[#1])))" w))
 collect
 `(defun-inline ,name-set (fixnum t fixnum) fixnum
    #.(compiler::flags set compiler::rfa)
	 ,(format nil "(*(~a *)(&((#1)->ust.ust_self[#2])))=(~a)(#0)" w w))))
(PROGN
  (DEFUN-INLINE AREF-CARD29 (T FIXNUM) FIXNUM 8
      "(*(unsigned long *)(&((#0)->ust.ust_self[#1])))")
  (DEFUN-INLINE ASET-CARD29 (FIXNUM T FIXNUM) FIXNUM 10
      "(*(unsigned long *)(&((#1)->ust.ust_self[#2])))=(unsigned long)(#0)")
  (DEFUN-INLINE AREF-INT8 (T FIXNUM) FIXNUM 8
      "(*(char *)(&((#0)->ust.ust_self[#1])))")
  (DEFUN-INLINE ASET-INT8 (FIXNUM T FIXNUM) FIXNUM 10
      "(*(char *)(&((#1)->ust.ust_self[#2])))=(char)(#0)")
  (DEFUN-INLINE AREF-CARD8 (T FIXNUM) FIXNUM 8
      "(*(unsigned char *)(&((#0)->ust.ust_self[#1])))")
  (DEFUN-INLINE ASET-CARD8 (FIXNUM T FIXNUM) FIXNUM 10
      "(*(unsigned char *)(&((#1)->ust.ust_self[#2])))=(unsigned char)(#0)")
  (DEFUN-INLINE AREF-INT16 (T FIXNUM) FIXNUM 8
      "(*(short *)(&((#0)->ust.ust_self[#1])))")
  (DEFUN-INLINE ASET-INT16 (FIXNUM T FIXNUM) FIXNUM 10
      "(*(short *)(&((#1)->ust.ust_self[#2])))=(short)(#0)")
  (DEFUN-INLINE AREF-CARD16 (T FIXNUM) FIXNUM 8
      "(*(unsigned short *)(&((#0)->ust.ust_self[#1])))")
  (DEFUN-INLINE ASET-CARD16 (FIXNUM T FIXNUM) FIXNUM 10
      "(*(unsigned short *)(&((#1)->ust.ust_self[#2])))=(unsigned short)(#0)")
  (DEFUN-INLINE AREF-INT32 (T FIXNUM) FIXNUM 8
      "(*(long *)(&((#0)->ust.ust_self[#1])))")
  (DEFUN-INLINE ASET-INT32 (FIXNUM T FIXNUM) FIXNUM 10
      "(*(long *)(&((#1)->ust.ust_self[#2])))=(long)(#0)"))

(defun buffer-read-default (display vector start end timeout)
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end)
	   (type (or null (rational 0 *) (float 0.0 *)) timeout))
; (if *debug-read* (format t "~%doing buffer-read-default ~a(~a)(~a) " display start timeout))
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (let ((tem (and stream (si::fp-input-stream stream))))
	  (if tem (setq stream tem)))
    (or (cond ((null stream))
	      ((listen stream) nil)
	      ((eql timeout 0) :timeout)
	      ((buffer-input-wait-default display timeout)))
	(do* ((index start (index1+ index)))
	     ((index>= index end) nil)
	  (declare (type array-index index))
	  (let ((c (read-byte stream nil -1)))
	    (declare (type fixnum c))
;	    (if *debug-read* (format t "(~d)" c))
	    (if (eql c -1)
		(return t)
	      (setf (aref vector index) (the card8 c))))))))

(defun buffer-new-request-number (buffer)
  (declare (type buffer buffer))
  (setf (buffer-request-number buffer)
	(logand #xffff (the card29 (1+ (the card29(buffer-request-number buffer)))))))




(defun aset-card32 (v a i &aux (h 0))
  (declare (type card32 v)
	   (type buffer-bytes a)
	   (type array-index i)
	   (type int32 h))
  (if (typep v 'int32)
      (setq h v)
    (setq h (card32->int32 v)))
  (aset-card29 h a i)
  ;(or (eql v (aref-card32 a i)) (print (list me (aref-card32 a i))))
  v)
 
(defun card32->int32 (x)
  (declare (type card32 x))
  (declare (values int32))
  (cond ((typep x 'fixnum)
	 x)
	(t  (the int32 (if (logbitp 31 x)
			 (the int32 (- x #x100000000))
			 x)))))
||#
  
