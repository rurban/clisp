
(in-package 'compiler)

#+(and akcl (not gcl))
(eval-when (compile load eval)
(when (<= system::*akcl-version* 609)
  (pushnew :pre_akcl_610 *features*))
)

#+pre_akcl_610
(progn

;[without this, xlib:create-window won't work]
;added inline-integer here.
(defun get-inline-loc (ii args &aux (fun (car (cdddr ii))) locs)
  ;;; Those functions that use GET-INLINE-LOC must rebind the variable *VS*.
 (setq locs (inline-args args (car ii) fun))
  (when (and (stringp fun) (char= (char (the string fun) 0) #\@))
    (let ((i 1) (saves nil))
         (declare (fixnum i))
      (do ((char (char (the string fun) i)
                 (char (the string fun) i)))
          ((char= char #\;) (incf i))
          (declare (character char))
          (push (the fixnum (- (char-code char) #.(char-code #\0))) saves)
          (incf i))
      (do ((l locs (cdr l))
           (n 0 (1+ n))
           (locs1 nil))
          ((endp l) (setq locs (reverse locs1)))
          (declare (fixnum n) (object l))
          (if (member n saves)
              (let* ((loc1 (car l)) (loc loc1) (coersion nil))
                    (declare (object loc loc1))
                (when (and (consp loc1)
                           (member (car loc1)
                                   '(FIXNUM-LOC integer-loc CHARACTER-LOC
                                     LONG-FLOAT-LOC SHORT-FLOAT-LOC)))
                      (setq coersion (car loc1))
                      (setq loc (cadr loc1))  ; remove coersion
                      )
                (cond
                 ((and (consp loc)
		       (or
			 (member (car loc) 
                                    '(INLINE INLINE-COND))
			 (and 	 (member (car loc)
					 '(
					   INLINE-FIXNUM inline-integer
					   INLINE-CHARACTER INLINE-LONG-FLOAT
					   INLINE-SHORT-FLOAT))
				 (or (flag-p (cadr loc) allocates-new-storage)
				     (flag-p (cadr loc) side-effect-p))
	                            )))
                  (wt-nl "{")
                  (inc-inline-blocks)
                  (let ((cvar (next-cvar)))
                    (push (list 'CVAR cvar) locs1)
                    (case coersion
                     ((nil) (wt "object V" cvar "= ") (wt-loc loc1))
                     (FIXNUM-LOC (wt "int V" cvar "= ") (wt-fixnum-loc loc))
		     (integer-loc (wt "GEN V" cvar "= ") (wt-integer-loc loc
									 'get-inline-locs))
                     (CHARACTER-LOC
                      (wt "unsigned char V" cvar "= ") (wt-character-loc loc))
                     (LONG-FLOAT-LOC
                      (wt "double V" cvar "= ") (wt-long-float-loc loc))
                     (SHORT-FLOAT-LOC
                      (wt "float V" cvar "= ") (wt-short-float-loc loc))
                     (t (baboon))))
                  (wt ";")
                  )
                 (t (push loc1 locs1))))
              (push (car l) locs1)))))
  (list (inline-type (cadr ii))
        (caddr ii)
        fun
        locs)
  )

;added inline-integer
(defun unwind-exit (loc &optional (jump-p nil) fname
                        &aux (*vs* *vs*) (bds-cvar nil) (bds-bind 0) type.wt)
  (declare (fixnum bds-bind))
  (and *record-call-info* (record-call-info loc fname))
  (when (and (eq loc 'fun-val)
             (not (eq *value-to-go* 'return))
             (not (eq *value-to-go* 'top)))
        (wt-nl) (reset-top))
  (cond ((and (consp *value-to-go*) (eq (car *value-to-go*) 'jump-true))
         (set-jump-true loc (cadr *value-to-go*))
         (when (eq loc t) (return-from unwind-exit)))
        ((and (consp *value-to-go*) (eq (car *value-to-go*) 'jump-false))
         (set-jump-false loc (cadr *value-to-go*))
         (when (null loc) (return-from unwind-exit))))
  (dolist* (ue *unwind-exit* (baboon))
   (cond
    ((consp ue)
     (cond ((eq ue *exit*)
	    (cond ((and (consp *value-to-go*)
			(or (eq (car *value-to-go*) 'jump-true)
			    (eq (car *value-to-go*) 'jump-false)))
		   (unwind-bds bds-cvar bds-bind))
		  (t
		   (if (or bds-cvar   (plusp bds-bind))
                          ;;; Save the value if LOC may possibly refer
                          ;;; to special binding.
		       (if (and (consp loc)
				(or (and (eq (car loc) 'var)
					 (member (var-kind (cadr loc))
						 '(SPECIAL GLOBAL)))
				    (member (car loc)
					    '(SIMPLE-CALL
					      INLINE
					      INLINE-COND INLINE-FIXNUM
					      INLINE-CHARACTER
					      INLINE-INTEGER
					      INLINE-LONG-FLOAT
					      INLINE-SHORT-FLOAT))))
			   (cond ((and (consp *value-to-go*)
				       (eq (car *value-to-go*) 'vs))
				  (set-loc loc)
				  (unwind-bds bds-cvar bds-bind))
				 (t (let
					((temp (list 'cvar (cs-push))))
				      (let ((*value-to-go* temp))
					(set-loc loc))
				      (unwind-bds bds-cvar bds-bind)
				      (set-loc temp))))
			 (progn (unwind-bds bds-cvar bds-bind)
				(set-loc loc)))
		     (set-loc loc))))

	    (when jump-p
	      (when (consp *inline-blocks*) (wt-nl "restore_avma; "))
	      (wt-nl) (wt-go *exit*))
	    (return))
	   (t (setq jump-p t))))
    ((numberp ue) (setq bds-cvar ue bds-bind 0))
    ((eq ue 'bds-bind) (incf bds-bind))
    ((eq ue 'return)
     (when (eq *exit* 'return)
              ;;; *VALUE-TO-GO* must be either *RETURN* or *TRASH*.
       (set-loc loc)
       (unwind-bds bds-cvar bds-bind)
       (wt-nl "return;")
       (return))
        ;;; Never reached
     )
    ((eq ue 'frame)
     (when (and (consp loc)
		(member (car loc)
			'(SIMPLE-CALL INLINE INLINE-COND INLINE-FIXNUM inline-integer
				      INLINE-CHARACTER INLINE-LONG-FLOAT
				      INLINE-SHORT-FLOAT)))
       (cond ((and (consp *value-to-go*)
		   (eq (car *value-to-go*) 'vs))
	      (set-loc loc)
	      (setq loc *value-to-go*))
	     (t (let ((*value-to-go* (if *c-gc* (list 'cvar (cs-push))
				       (list 'vs (vs-push)))))
		  (set-loc loc)
		  (setq loc *value-to-go*)))))
     (wt-nl "frs_pop();"))
    ((eq ue 'tail-recursion-mark))
    ((eq ue 'jump) (setq jump-p t))
    ((setq type.wt
	   (assoc ue
		  '((return-fixnum fixnum .  wt-fixnum-loc)
		    (return-character character . wt-character-loc)
		    (return-short-float short-float . wt-short-float-loc)
		    (return-long-float long-float . wt-long-float-loc)
		    (return-object t . wt-loc))))
     (let ((cvar (next-cvar)))
       (or (eq *exit* (car type.wt)) (wfs-error))
       (setq type.wt (cdr type.wt))
       (wt-nl "{" (rep-type (car type.wt)) "V" cvar " = ")
       (funcall (cdr type.wt) loc)  (wt ";")
       (unwind-bds bds-cvar bds-bind)
       (wt-nl "VMR" *reservation-cmacro* "(V" cvar")}")
       (return)))
		
    (t (baboon))
       ;;; Never reached
    ))
  )

;added inline-integer
(defun set-loc (loc &aux fd)
  (cond ((eq *value-to-go* 'return) (set-return loc))
        ((eq *value-to-go* 'trash)
         (cond ((and (consp loc)
                     (member (car loc)
                             '(INLINE INLINE-COND INLINE-FIXNUM inline-integer
                               INLINE-CHARACTER INLINE-LONG-FLOAT
                               INLINE-SHORT-FLOAT))
                     (cadr loc))
                (wt-nl "(void)(") (wt-inline t (caddr loc) (cadddr loc))
                (wt ");"))
               ((and (consp loc) (eq (car loc) 'SIMPLE-CALL))
                (wt-nl "(void)" loc ";"))))
        ((eq *value-to-go* 'top)
         (unless (eq loc 'fun-val) (set-top loc)))
        ((eq *value-to-go* 'return-fixnum) (set-return-fixnum loc))
        ((eq *value-to-go* 'return-character) (set-return-character loc))
        ((eq *value-to-go* 'return-long-float) (set-return-long-float loc))
        ((eq *value-to-go* 'return-short-float) (set-return-short-float loc))
        ((or (not (consp *value-to-go*))
             (not (symbolp (car *value-to-go*))))
         (baboon))
        ((setq fd (get (car *value-to-go*) 'set-loc))
         (apply fd loc (cdr *value-to-go*)))
        ((setq fd (get (car *value-to-go*) 'wt-loc))
         (wt-nl) (apply fd (cdr *value-to-go*)) (wt "= " loc ";"))
        (t (baboon)))
  )

)
