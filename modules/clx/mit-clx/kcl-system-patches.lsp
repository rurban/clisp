
(in-package 'system)


#|| ;This isn't needed for clue anymore, so I have commented it out.
;[The reason it isn't needed is that with this fix, I found that 
;call-arguments-limit was being exceeded, ;and so I added code to 
;remove unwanted keyword arguments first.]

;clue has a call to xlib:create-window in intrinsics.lisp that causes problems.

;It isn't quite right to have a fixed upper argument length limit for 
;functions having keyword arguments because :allow-other-keys might be an
;argument.  The following patch fixes it.  
;(The right way to fix this is to change the appropriate calls to 
;add-init in cmptop.lsp, instead of this patch.)
#+akcl
(progn
(clines "
object siSPinit,siSPmemory;
object MFvfun_key();
#define PADDR(i) ((char *)(siSPinit->s.s_dbind->fixa.fixa_self[fix(i)]))
#define call_MFvfun_key(sym,self,argd,keys) \\
        MFvfun_key(sym,PADDR(self),fix(argd)|0xff00,siSPmemory->s.s_dbind,PADDR(keys));
")

;The "|0xff00" above increases the vfun max args to 255 to allow for
;the :allow-other-keys argument.  When :allow-other-keys is present,
;there might be any number of arguments.

(defentry MFVFUN-KEY (object object object object) (object "call_MFvfun_key"))

;;;;;;;;;

(clines "
object fix_existing_key_vfun(vfun)
   object vfun;
{
  if(type_of(vfun) == t_vfun) 
    {vfun->vfn.vfn_maxargs = 0Xff;}
  return vfun;
}
")

(defentry fix-existing-key-vfun (object) (object "fix_existing_key_vfun"))

(defun fix-existing-key-vfuns (syms)
  (dolist (sym syms)
    (fix-existing-key-vfun (symbol-function sym))))
)
||#

#+akcl
(eval-when (compile load eval)
(when (<= system::*akcl-version* 609)
  (pushnew :pre_akcl_610 *features*))
)

#+pre_akcl_610
(progn

(proclaim '(optimize (safety 2) (space 3)))

;[need this for clx/trace]
;added the call to best-array-element-type
(defun make-sequence (type size &key (initial-element nil iesp)
                                &aux element-type sequence)
  (setq element-type
        (cond ((eq type 'list)
               (return-from make-sequence
                (if iesp
                    (make-list size :initial-element initial-element)
                    (make-list size))))
              ((or (eq type 'simple-string) (eq type 'string)) 'string-char)
              ((or (eq type 'simple-bit-vector) (eq type 'bit-vector)) 'bit)
              ((or (eq type 'simple-vector) (eq type 'vector)) t)
              (t
               (setq type (normalize-type type))
               (when (eq (car type) 'list)
                     (return-from make-sequence
                      (if iesp
                          (make-list size :initial-element initial-element)
                          (make-list size))))
               (unless (or (eq (car type) 'array)
                           (eq (car type) 'simple-array))
                       (error "~S is not a sequence type." type))
               (or (cadr type) t))))
  (setq element-type (best-array-element-type element-type))
  (setq sequence (make-vector element-type size nil nil nil nil nil))
  (when iesp
        (do ((i 0 (1+ i))
             (size size))
            ((>= i size))
          (declare (fixnum i size))
          (setf (elt sequence i) initial-element)))
  sequence)

;The original version (in c/predicate.c) ignores the possibility that 
;arrays and vectors can have non-T element types.
(defun contains-sharp-comma (x)
  (typecase x
    (complex (or (contains-sharp-comma (realpart x))
		 (contains-sharp-comma (imagpart x))))
    (vector  (and (eq 't (array-element-type x))
		  (some #'contains-sharp-comma x)))
    (cons    (or (eq '|#,| (car x))
		 (contains-sharp-comma (car x))
		 (contains-sharp-comma (cdr x))))
    (array   (and (eq 't (array-element-type x))
		  (let* ((rank (array-rank x))
			 (dimensions (make-list rank)))
		    (dotimes (i rank)
		      (setf (nth i dimensions) (array-dimension x i)))
		    (unless (member 0 dimensions)
		      (do ((cursor (make-list rank :initial-element 0)))
			  (nil)
			(declare (:dynamic-extent cursor))
			(when (contains-sharp-comma (apply #'aref x cursor))
			  (return t))
			(when (increment-cursor cursor dimensions)
			  (return nil)))))))
    (t (structurep x))))
		  

)
