;;; MODPREP - CLISP module preprocessor
;;;
;;; Copyright (C) 1998 Bruno Haible (20.9.1998, 10.-11.10.1998) [C]
;;; Copyright (C) 2003 by Sam Steingold [lisp]
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
#| This preprocessor generates all necessary tables for a CLISP module.
The input file is normal C code, modified like this:
- It includes "clisp.h".
- There is one (and only one) declaration
    DEFMODULE(module_name,"PACKAGE-NAME")
  module_name is the clisp module name.
  PACKAGE-NAME is the default package name (in upper case) for Lisp
  functions.
- Constant Lisp objects can be referred to using the backquote syntax:
    pushSTACK(`:TEST`);
    value1 = `#()`;
  The backquoted strings are read in at module load time.
- The module SUBRs can be referred to using double backquote syntax:
    pushSTACK(``FOO:BAR``);
  means the same as L(bar) in the CLISP sources
- The definition of Lisp functions is done using the macro
    DEFUN(function_name, lambda_list)
  for example
    DEFUN(foo::bar, x y [&optional z] [&rest foo | &key a b c] )
  &rest and &key cannot be combined (this is a restriction for SUBRs).
  &key requires at least one keyword (this is a restriction for SUBRs too).
- Variables containing Lisp objects (known to the garbage collector) are
  defined using the macro
    DEFVAR(variable_name, initform)
  where initform is a C form. (You can also specify a Lisp initform, by
  using the backquote syntax.) The variable can be referred to as
    O(variable_name)
  These variables are private to the module.

Restrictions and caveats:
- A module should consist of a single file.
- #line lines should not be separated into multiple lines using
  backslash-newline.
- No multi-line comments should start in a preprocessor line.
- #if conditions are assumed to be constant from the DEFMODULE call to
  the end of the file. All necessary #define's should therefore be done
  before the DEFMODULE call.
- DEFUN and DEFVAR statements must take one CPP line
  (can be continued with backslashes):
     DEFUN (FOO:BAR,               ; BAD
             zot quux)
     DEFUN (FOO:BAR,             \
             zot quux)             ; GOOD

|#

(in-package "CL-USER")

(defvar *input-file*)
(defvar *lineno*)               ; current line position in file
(defvar *lines*)                ; list of all lines

(defun next-non-blank (line beg)
  (position-if-not #'sys::whitespacep line :start beg))
(defun prev-non-blank (line end)
  (1+ (position-if-not #'sys::whitespacep line :from-end t :end end)))
(defun next-blank (line beg) (position-if #'sys::whitespacep line :start beg))

(defun decode-directive (line)
  "return :if/:elif/:else/:endif/:line or nil;
 and the start of the first non-blank after the instruction"
  (declare (string line))
  (let ((pos (next-non-blank line 0)))
    (and pos (char= #\# (aref line pos))
         (setq pos (next-non-blank line (1+ pos)))
         (values (intern (subseq line pos (setq pos (next-blank line pos)))
                         #.(find-package "KEYWORD"))
                 (and pos (next-non-blank line pos))))))

(defun decode-line-directive (line)
  "Decode a #line directive. If the line represents a #line directive,
return the line number, else NIL."
  (declare (string line))
  (multiple-value-bind (directive pos) (decode-directive line)
    (when (eq :|line| directive)
      (parse-integer line :start pos :end (next-blank line pos)))))

(defstruct line number contents)

(defun read-all-input (stream)
  (loop :for in = (read-line stream nil nil) :and lineno :upfrom 1
    :while in
    :do (loop :for len = (length in)
          :while (and (/= 0 len) (char= #\\ (aref in (1- len))))
          :do (setf (aref in (1- len)) #\Space ; remove #\\
                    in (ext:string-concat in (read-line stream nil nil)))
          (incf lineno))
    :do (let ((n (decode-line-directive in)))
          (when (and n (/= n lineno))
            ;; (warn "fixed line number: ~S --> ~S" lineno n)
            (setq lineno n)))
    :collect (make-line :number lineno :contents in)))

(defun vector-starts-with (v0 v1)
  (and (>= (length v0) (length v1))
       (every #'string= v0 v1)))

;; Push elt, and optimize: elt can be removed if is starts with an
;; already present string sequence. If another element starts with elt,
;; that one can be removed
(defun stack-push-optimize (stack elt)
  (unless (some (lambda (vec) (vector-starts-with elt vec)) stack)
    (delete-if (lambda (vec) (vector-starts-with vec elt)) stack)
    (vector-push-extend elt stack))
  stack)

;; The #if[def] stack. All the conditions are implicitly combined by &&.
;; For every #if we start a new entry in the stack, which is popped when
;; we see the corresponding #endif. This is a stack of vector of string,
;; not a stack of string, because when a #elif is seen, we add an
;; element to the stack without popping the previous one.
(defvar *if-stack* (make-array 10 :adjustable t :fill-pointer 0))

(defun sharp-if (condition)
  (let ((vec (make-array 5 :adjustable t :fill-pointer 0)))
    (vector-push condition vec)
    (vector-push-extend vec *if-stack*)
    vec))
(defun sharp-else ()
  (let* ((top (aref *if-stack* (1- (length *if-stack*))))
         (pos (1- (length top))))
    (setf (aref top pos) (ext:string-concat "!(" (aref top pos) ")"))
    top))
(defun sharp-elif (condition)
  (vector-push-extend condition (sharp-else)))
(defun sharp-endif () (vector-pop *if-stack*))

(defun current-condition ()
  "Returns the current #if condition.
It is a vector of strings, implicitly combined by &&.
The vector is freshly constructed, but the strings are shared"
  (let ((vec (make-array 5 :adjustable t :fill-pointer 0)))
    (loop :for cc :across *if-stack* :do
      (dotimes (ii (length cc))
        (vector-push-extend (aref cc ii) vec)))
    vec))

(defun string-rest (line pos) (subseq line pos (prev-non-blank line nil)))

(defun if-p (line)
  "#if/#ifdef/#ifndef"
  (declare (string line))
  (multiple-value-bind (directive pos) (decode-directive line)
    (case directive
      ;; FIXME: what about a comment starting on a CPP line?!
      (:|if| (string-rest line pos))
      (:|ifndef| (ext:string-concat "!defined(" (string-rest line pos) ")"))
      (:|ifdef| (ext:string-concat "defined(" (string-rest line pos) ")")))))

(defun else-p (line) (eq :|else| (decode-directive line)))

(defun elif-p (line)
  (multiple-value-bind (directive pos) (decode-directive line)
    (and (eq :|elif| directive) (string-rest line pos))))

(defun endif-p (line) (eq :|endif| (decode-directive line)))

;; DEFMODULE(name,package)
(defvar *module-name*)
(defvar *module-line*)
(defvar *module-package*)
(defvar *module-all-packages*)
(defvar *init-2-name* nil "Did the module define its own init2?")

(defun defmodule-p (line)
  (let* ((pos (next-non-blank line 0))
         (end (and pos (+ pos #.(length "DEFMODULE")))))
    (when (and pos (<= end (length line))
               (string= "DEFMODULE" line :start2 pos :end2 end)
               (char= (aref line (setq pos (next-non-blank line end))) #\())
      (setq *module-name*
            (subseq line (next-non-blank line (1+ pos))
                    (prev-non-blank line (setq pos (position #\, line))))
            *init-2-name* (format nil "module__~A__init_function_2"
                                  *module-name*))
      (assert pos () "no comma in DEFMODULE directive")
      (setq *module-package*
            (subseq line (setq pos (1+ (position #\" line)))
                    (position #\" line :start pos))
            *module-all-packages* (list *module-package*))
      (setq pos (position #\) line))
      (assert pos () "no closing paren in DEFMODULE directive")
      (values *module-name* *module-package*))))

(defstruct objdef
  init tag
  (cond-stack (make-array 5 :adjustable t :fill-pointer 0)))
(defvar *objdefs* (make-array 10 :adjustable t :fill-pointer 0))
(defun tag-to-objdef (tag)
  (find tag *objdefs* :test #'string= :key #'objdef-tag))

(defun init-to-tag (init already-present-p &optional (prefix "object_"))
  (let ((base
         (with-output-to-string (s)
           (when prefix (write-string prefix s))
           (loop :for cc :across init :and ii :upfrom 0 :do
             (cond ((alphanumericp cc) (write-char (char-downcase cc) s))
                   ((char= cc #\:) (write-char (if (zerop ii) #\K #\_) s))
                   ((or (char= cc #\_) (char= cc #\-)) (write-char #\_ s))
                   (t (format s "_~2,'0x" (char-code cc))))))))
    (when (funcall already-present-p base)
      (loop :for ii :upfrom 0 :for new = (format nil "~a_~d" base ii)
        :while (funcall already-present-p new)
        :finally (setq base new)))
    base))

(defun new-objdef (init)
  (unless (every (lambda (cc) (char= cc (char-upcase cc))) init)
    (warn "~S:~D: fixed object case ~S" *input-file* *lineno* init)
    (setq init (string-upcase init)))
  (let ((od (make-objdef :init init :tag (init-to-tag init #'tag-to-objdef))))
    (vector-push-extend od *objdefs*)
    od))

(defun init-to-objdef (init &optional (condition (current-condition)))
  "Looks up or creates an Objdef for a given initstring"
  (let ((odef (or (find init *objdefs* :test #'string= :key #'objdef-init)
                  (new-objdef init))))
    (stack-push-optimize (objdef-cond-stack odef) condition)
    odef))

(defconstant *seclass*
  #("seclass_foldable" "seclass_no_se" "seclass_read"
    "seclass_write" "seclass_default"))
(defstruct signature
  (seclass (1- (length *seclass*)))
  req opt rest-p key-p keywords
  (cond-stack (make-array 5 :adjustable t :fill-pointer 0)))

(defconstant *valid-signatures*
  (vector
   (make-signature :req 0 :opt 0)
   (make-signature :req 1 :opt 0)
   (make-signature :req 2 :opt 0)
   (make-signature :req 3 :opt 0)
   (make-signature :req 4 :opt 0)
   (make-signature :req 5 :opt 0)
   (make-signature :req 6 :opt 0)
   (make-signature :req 0 :opt 1)
   (make-signature :req 1 :opt 1)
   (make-signature :req 2 :opt 1)
   (make-signature :req 3 :opt 1)
   (make-signature :req 4 :opt 1)
   (make-signature :req 0 :opt 2)
   (make-signature :req 1 :opt 2)
   (make-signature :req 2 :opt 2)
   (make-signature :req 3 :opt 2)
   (make-signature :req 0 :opt 3)
   (make-signature :req 1 :opt 3)
   (make-signature :req 2 :opt 3)
   (make-signature :req 0 :opt 4)
   (make-signature :req 0 :opt 5)
   (make-signature :req 0 :opt 0 :rest-p t)
   (make-signature :req 1 :opt 0 :rest-p t)
   (make-signature :req 2 :opt 0 :rest-p t)
   (make-signature :req 3 :opt 0 :rest-p t)
   (make-signature :req 0 :opt 0 :key-p t)
   (make-signature :req 1 :opt 0 :key-p t)
   (make-signature :req 2 :opt 0 :key-p t)
   (make-signature :req 3 :opt 0 :key-p t)
   (make-signature :req 4 :opt 0 :key-p t)
   (make-signature :req 0 :opt 1 :key-p t)
   (make-signature :req 1 :opt 1 :key-p t)
   (make-signature :req 1 :opt 2 :key-p t)))

(defvar *must-close-next-defun* nil
  "set to T when emulating the signature")
(defvar *in-defun* nil "set to T when entering a defun")
(defvar *emulation-count* 0)

(defun parse-signature (fname line &key (start 0) (end (length line)))
  (loop :with seen-opt :and seen-key :and seen-rest :and keys
    :and opt = 0 :and req = 0 :and pos2 = start
    :for pos1 = (next-non-blank line pos2)
    :while (and pos1 (< pos1 end))
    :do (setq pos2 (min end (or (next-blank line pos1) end)))
    (cond ((string-equal line "&optional" :start1 pos1 :end1 pos2)
           (when (or seen-opt seen-key seen-rest)
             (error "~S:~D: bad signature ~S between ~S and ~S"
                    *input-file* *lineno* line pos1 pos2))
           (setq seen-opt t))
          ((string-equal line "&key" :start1 pos1 :end1 pos2)
           (when (or seen-key seen-rest)
             (error "~S:~D: bad signature ~S between ~S and ~S"
                    *input-file* *lineno* line pos1 pos2))
           (setq seen-key t))
          ((string-equal line "&rest" :start1 pos1 :end1 pos2)
           (when (or seen-key seen-rest)
             (error "~S:~D: bad signature ~S between ~S and ~S"
                    *input-file* *lineno* line pos1 pos2))
           (setq seen-rest 0))
          (seen-rest
           (if (zerop seen-rest) (setq seen-rest t)
               (error "~S:~D: bad signature ~S between ~S and ~S"
                      *input-file* *lineno* line pos1 pos2)))
          (seen-key
           (push (init-to-objdef (if (char= (aref line pos1) #\:)
                                     (subseq line pos1 pos2)
                                     (ext:string-concat
                                      ":" (subseq line pos1 pos2))))
                 keys))
          (seen-opt (incf opt))
          ((incf req)))
    :finally (return (check-signature fname
                      (make-signature
                       :req req :opt opt :rest-p seen-rest
                       :key-p seen-key :keywords (nreverse keys))))))

(defun check-signature (fname sig)
  (if (find sig *valid-signatures* :test #'signature-match)
    (values sig nil)
    (values (load-time-value (make-signature :req 0 :opt 0 :rest-p t))
      (with-output-to-string (out) ; emulate signature
        (warn "~S:~D:~A: emulating signature (~A ~A~:[~; &rest~]~:[~; &key~])"
              *input-file* *lineno* fname
              (signature-req sig) (signature-opt sig)
              (signature-rest-p sig) (signature-key-p sig))
        (incf *emulation-count*)
        (when (signature-rest-p sig) ; why?!
          (error "~A: cannot emulate &rest" fname))
        (let* ((min-arg (signature-req sig))
               (req+opt (+ min-arg (signature-opt sig)))
               (max-arg
                (unless (or (signature-rest-p sig) (signature-key-p sig))
                  req+opt))
               (kwds (signature-keywords sig)) (n-kwds (length kwds))
               (kwd-list
                 (with-output-to-string (kwd-s)
                   (write-char #\( kwd-s)
                   (loop :for k :in kwds :and firstp = t :then nil :do
                     (unless firstp (write-char #\Space kwd-s))
                     (write-string (objdef-init k) kwd-s))
                   (write-char #\) kwd-s))))
          (format out "{ if (argcount < ~D) { pushSTACK(TheSubr(subr_self)->name); fehler(source_program_error,(\"EVAL/APPLY: too few arguments given to ~~\")); } " min-arg)
          (when max-arg (format out "if (argcount > ~D) { pushSTACK(TheSubr(subr_self)->name); fehler(source_program_error,(\"EVAL/APPLY: too many arguments given to ~~\")); } " max-arg))
          (unless (zerop (signature-opt sig)) (format out "for (;argcount < ~D; argcount++) pushSTACK(unbound); " req+opt))
          (when (signature-key-p sig)
            (format out "if ((argcount-~D)%2) fehler_key_odd(argcount,TheSubr(subr_self)->name); " req+opt)
            (when (zerop n-kwds) (warn "~A: &key without any keywords" fname))
            (format out "{ uintC i; skipSTACK((-~D)); ~
  for (i = 0; i<argcount-~D; i++) STACK_(i) = STACK_(i+~D); "
                    n-kwds req+opt n-kwds)
            (dotimes (i n-kwds)
              (format out "STACK_(argcount-~D+~D) = unbound; " req+opt i))
            (format out "for (i = argcount-~D; i > 0; i -= 2) " req+opt)
            (loop :for k :in kwds :and i :upfrom 0 :do
              (format out "~[~:;else ~]if (eq (STACK_(i-1),O(~A))) ~
 STACK_(argcount-~D+~D) = STACK_(i-2); "
                      i (objdef-tag k) req+opt (- n-kwds i 1)))
            (format out "else fehler_key_badkw(TheSubr(subr_self)->name,STACK_(i-1),STACK_(i-2),O(~A)); skipSTACK(argcount-~D); }"
                    (objdef-tag (init-to-objdef kwd-list)) req+opt))
          (setq *must-close-next-defun* t))))))

(defun signature-match (s1 s2)
  (and (= (signature-req s1) (signature-req s2))
       (= (signature-opt s1) (signature-opt s2))
       (eq (signature-rest-p s1) (signature-rest-p s2))
       (eq (signature-key-p s1) (signature-key-p s2))))

(defun signature= (s1 s2)
  (and (signature-match s1 s2)
       (= (signature-seclass s1) (signature-seclass s2))
       (equal (signature-keywords s1) (signature-keywords s2))))

(defstruct fundef
  pack      ;; The symbol's package name
  name      ;; The symbol's print name
  tag       ;; The struct tag we use for this function
  ;; The total #if condition of this function.
  (cond-stack (make-array 5 :adjustable t :fill-pointer 0))
  signatures;; The function's possible signatures,
  ;; together with their individual #if conditions.
)
(defvar *fundefs* (make-array 10 :adjustable t :fill-pointer 0))
(defun tag-to-fundef (tag)
  (find tag *fundefs* :test #'string= :key #'fundef-tag))

(defun new-fundef (name pack)
  (let ((fd (make-fundef :pack pack :name name :tag
                         (init-to-tag (ext:string-concat pack ":" name)
                                      #'tag-to-fundef "subr_"))))
    (unless (every #'upper-case-p pack)
      (warn "~S:~D: fixed package case: ~S" *input-file* *lineno* pack)
      (setq pack (string-upcase pack)))
    (unless (every (lambda (c) (or (not (alpha-char-p c)) (upper-case-p c)))
                   name)
      (warn "~S:~D: fixed function name case ~S" *input-file* *lineno* name)
      (setq name (string-upcase name)))
    (pushnew pack *module-all-packages* :test #'string=)
    (vector-push-extend fd *fundefs*)
    fd))

(defun find-fundef (funname &optional (condition (current-condition)))
  "find the FDEF pbject corresponding to the given FUNNAME or create a new one"
  (let ((pack *module-package*) (name funname) (pos (position #\: funname)))
    (when pos
      (setq pack (subseq funname 0 pos)) (incf pos)
      (when (char= #\: (aref funname pos)) (incf pos))
      (when (find #\: funname :start pos)
        (error "~S:~D: too many package markers in ~S"
               *input-file* *lineno* funname))
      (setq name (subseq funname pos)))
    (unless pack
      (error "~S:~D: function name ~S needs a package prefix"
             *input-file* *lineno* funname))
    (let ((fdef (or (find-if (lambda (fd)
                               (and (string= name (fundef-name fd))
                                    (string= pack (fundef-pack fd))))
                             *fundefs*)
                    (new-fundef name pack))))
      (stack-push-optimize (fundef-cond-stack fdef) condition)
      fdef)))

(defun funname-to-fundef (funname sig &optional
                          (condition (current-condition)))
  (let* ((fdef (find-fundef funname condition))
         (sig-old (find sig (fundef-signatures fdef) :test #'signature=)))
    (if sig-old
        (stack-push-optimize (signature-cond-stack sig-old) condition)
        (progn
          (stack-push-optimize (signature-cond-stack sig) condition)
          (push sig (fundef-signatures fdef))))
    fdef))

(defun fundef-lispfun (fundef sig)
  "Print a signature in a form suitable as argument list for LISPFUN."
  (format nil "(~a,~a,~d,~d,~:[no~;~]rest,~:[no~;~]key,~d,NIL)"
          (fundef-tag fundef) (aref *seclass* (signature-seclass sig))
          (signature-req sig) (signature-opt sig)
          (signature-rest-p sig) (signature-key-p sig)
          (length (signature-keywords sig))))

(defvar *brace-depth* 0)
(defun defun-p (line)
  "Parse a DEFUN(funname,lambdalist) line,
and turn it into  DEFUN(funname,lambdalist,signature)."
  (let* ((pos (next-non-blank line 0)) (sec "seclass_default") cc sig fname
         (len (length line)) (end (and pos (+ pos #.(length "DEFUN")))) comma)
    (when (and pos (< end len) (string= "DEFUN" line :start2 pos :end2 end)
               (case (setq cc (aref line end))
                 (#\F (setq sec "seclass_foldable") (incf end))
                 (#\N (setq sec "seclass_no_se") (incf end))
                 (#\R (setq sec "seclass_read") (incf end))
                 (#\W (setq sec "seclass_write") (incf end))
                 (#\D (setq sec "seclass_default") (incf end))
                 (#\( t)
                 (t (sys::whitespacep cc))))
      (unless (zerop *brace-depth*)
        (error "~S:~D: DEFUN must be at the top level (depth ~D): ~S"
               *input-file* *lineno* *brace-depth* line))
      (setq pos (next-non-blank line end))
      (unless (and pos (char= #\( (aref line pos)))
        (error "~S:~D: invalid DEFUN syntax in ~S"
               *input-file* *lineno* line))
      (unless (setq comma (position #\, line :start pos))
        (error "~S:~D: too few arguments to DEFUN in ~S"
               *input-file* *lineno* line))
      (setq end (position #\) line :start comma)
            fname (subseq line (next-non-blank line (1+ pos))
                          (prev-non-blank line comma)))
      (multiple-value-setq (sig cc)
        (parse-signature fname line :start (1+ comma) :end end))
      (let* ((rest (subseq line end))
             (all (ext:string-concat
                   (subseq line 0 end) ","
                   (fundef-lispfun (funname-to-fundef fname sig) sig)
                   (or cc "") rest)))
        (values all (- (length all) (length rest)))))))

(defstruct vardef
  tag (cond-stack (make-array 5 :adjustable t :fill-pointer 0)))
(defvar *vardefs* (make-array 10 :adjustable t :fill-pointer 0))
(defun tag-to-vardef (tag)
  (find tag *vardefs* :test #'string= :key #'vardef-tag))
(defun varname-to-vardef (varname &optional (condition (current-condition)))
  (let ((vdef (or (tag-to-vardef varname)
                  (let ((vd (make-vardef :tag varname)))
                    (vector-push-extend vd *vardefs*)
                    vd))))
    (stack-push-optimize (vardef-cond-stack vdef) condition)
    vdef))

;; Variable initializers. (We treat them separately from the variables
;; themselves, so that they are executed in order.)
(defstruct varinit tag init condition)
(defvar *varinits* (make-array 10 :adjustable t :fill-pointer 0))

(defun lexical-analysis (line &key (start 0) (end (length line)) in-comment)
  "Return the new line (with substitutions expanded) and the end position.
Also return status: NIL for parsing until the end and
  ';' when a comment was started but not ended;
  ')' when a closing parenthesis was encountered;
  ',' when a comma was encountered."
  (do ((paren-depth 0) in-string in-char in-subst subst-start in-func
       (ii start (1+ ii)) cc (done nil))
      ((or done (>= ii end))
       (when (= ii end)
         (when in-string
           (error "~S:~D: string not terminated: ~S"
                  *input-file* *lineno* line))
         (when in-char
           (error "~S:~D: character not terminated: ~S"
                  *input-file* *lineno* line))
         (when in-subst
           (error "~S:~D: backquote not terminated: ~S"
                  *input-file* *lineno* line)))
       (values line (1- ii) (cond (done done) (in-comment #\;))))
    (setq cc (aref line ii))
    (cond ((and (not in-comment) (not in-char) (not in-string) (not in-subst)
                (char= cc #\())
           (incf paren-depth))
          ((and (not in-comment) (not in-char) (not in-string) (not in-subst)
                (char= cc #\{))
           (when *must-close-next-defun* (setq *in-defun* t))
           (incf *brace-depth*))
          ((and (not in-comment) (not in-char) (not in-string) (not in-subst)
                (char= cc #\}))
           (decf *brace-depth*))
          ((and (not in-comment) (not in-char) (not in-string) (not in-subst)
                (zerop paren-depth) (char= cc #\,))
           (setq done #\,))
          ((and (not in-comment) (not in-char) (not in-string) (not in-subst)
                (char= cc #\)))
           (if (plusp paren-depth)
               (decf paren-depth)
               (setq done #\))))
          ((and (not in-char) (not in-string) (not in-subst)
                (< (1+ ii) end) (char= cc #\/)
                (char= (aref line (1+ ii)) #\*))
           (when in-comment
             (error "~S:~D: nested comments in ~S at ~D"
                    *input-file* *lineno* line ii))
           (setq in-comment t)
           (incf ii))
          ((and (not in-char) (not in-string) (not in-subst)
                (< (1+ ii) end) (char= cc #\*)
                (char= (aref line (1+ ii)) #\/))
           (unless in-comment
             (error "~S:~D: comment terminator outside comment in ~S at ~D"
                    *input-file* *lineno* line ii))
           (setq in-comment nil)
           (incf ii))
          ((and (not in-comment) (not in-char) (not in-subst)
                (char= cc #\"))
           (setq in-string (not in-string)))
          ((and (not in-comment) (not in-string) (not in-subst)
                (char= cc #\'))
           (setq in-char (not in-char)))
          ((and (not in-comment) (not in-string) (not in-char)
                (char= cc #\`))
           (if in-subst
               (if (= subst-start (1- ii))
                   (if in-func
                       (error "~S:~D: too many backquotes in ~S at ~D"
                              *input-file* *lineno* line ii)
                       (setq in-func t subst-start ii))
                   (if in-func
                       (let* ((id (subseq line (1+ subst-start) ii))
                              (def (find-fundef id))
                              (tag (fundef-tag def))
                              (tlen (1- (length tag))))
                         (if (and (< (1+ ii) end)
                                  (char= (aref line (1+ ii)) #\`))
                             (incf ii)
                             (error "~S:~D: bad SUBR reference in ~S at ~D"
                                    *input-file* *lineno* line ii))
                         (setq line (ext:string-concat
                                     (subseq line 0 (1- subst-start))
                                     "F(" tag ")" (subseq line (1+ ii)))
                               end (+ end tlen (- subst-start ii) 3 -1)
                               ii (+ subst-start tlen 3)
                               subst-start nil in-subst nil))
                       (let* ((id (subseq line (1+ subst-start) ii))
                              (def (init-to-objdef id))
                              (tag (objdef-tag def))
                              (tlen (1- (length tag))))
                         (setq line (ext:string-concat
                                     (subseq line 0 subst-start)
                                     "O(" tag ")" (subseq line (1+ ii)))
                               end (+ end tlen (- subst-start ii) 3)
                               ii (+ subst-start tlen 3)
                               subst-start nil in-subst nil))))
               (setq in-subst t subst-start ii))))))

(defun defvar-p (line)
  "Parse a DEFVAR(varname,initform) line, and turn it into DEFVAR(varname).
We remove the initform because it might contain backquoted stuff including
commas and parentheses."
  (let* ((pos (next-non-blank line 0)) cc vdef
         (len (length line)) (end (and pos (+ pos #.(length "DEFVAR")))) comma)
    (when (and pos (< end len) (string= "DEFVAR" line :start2 pos :end2 end)
               (or (char= (setq cc (aref line end)) #\()
                   (sys::whitespacep cc)))
      (setq pos (next-non-blank line end))
      (unless (and pos (char= #\( (aref line pos)))
        (error "~S:~D: invalid DEFVAR syntax in ~S"
               *input-file* *lineno* line))
      (unless (setq comma (position #\, line :start pos))
        (error "~S:~D: too few arguments to DEFVAR in ~S"
               *input-file* *lineno* line))
      (multiple-value-setq (line end cc)
        (lexical-analysis line :start (1+ comma) :end len))
      (cond ((eql cc #\;)
             (error "~S:~D: comment not terminated: ~S"
                    *input-file* *lineno* line))
            ((eql cc #\,)
             (error  "~S:~D: too many arguments to DEFVAR in ~S"
                     *input-file* *lineno* line))
            ((not (eql cc #\)))
             (error "~S:~D: DEFMACRO not terminated: ~S"
                    *input-file* *lineno* line)))
      (setq vdef (varname-to-vardef
                  (subseq line (next-non-blank line (1+ pos))
                          (prev-non-blank line comma))))
      (vector-push-extend
       (make-varinit :tag (vardef-tag vdef) :condition (current-condition)
                     :init (subseq line (next-non-blank line (1+ comma)) end))
       *varinits*)
      ;; return value from defvar-p: DEFVAR(varname)
      (ext:string-concat (subseq line 0 comma) (subseq line end)))))

(defun parse (&optional *lines*)
  "Parse the entire input"
  (loop :with in-comment :and condition :and status
    :for ln :in *lines* :and idx :upfrom 0 :and end = -1
    :for line = (line-contents ln) :do (setq *lineno* (line-number ln))
    (unless in-comment
      (when (defmodule-p line) (setq *module-line* idx))
      (when (setq condition (if-p line)) (sharp-if condition))
      (when (else-p line) (sharp-else))
      (when (setq condition (elif-p line)) (sharp-elif condition))
      (when (endif-p line) (sharp-endif))
      (setq line (or (defvar-p line) line))
      (multiple-value-bind (l p) (defun-p line)
        (when l (setq line l end (1- p)))))
    (loop (multiple-value-setq (line end status)
            (lexical-analysis line :start (1+ end) :in-comment in-comment))
      (setq in-comment (eql status #\;))
      (when (and *must-close-next-defun* *in-defun* (= *brace-depth* 0))
        (setq line (ext:string-concat line "}")
              *must-close-next-defun* nil *in-defun* nil))
      (when (and *init-2-name* (search *init-2-name* line))
        (setq *init-2-name* nil)) ; module defines its own init2
      (when (or (null status) (eql status #\;)) status (return)))
    (setf (line-contents ln) line)))

;; *** output ***

(defun print-condition-part (out condition)
  "Print a list (cond1 && cond2 && ...) to a stream"
  (case (length condition)
    (0 (write-char #\1 out))
    (1 (write (aref condition 0) :stream out :escape nil))
    (t (dotimes (i (length condition))
         (when (plusp i) (write-string " && " out))
         (format out "(~A)" (aref condition i))))))

(defun print-condition-whole (out cond-st)
  "Print a list of lists (cond1 || cond2 || ...) to a stream"
  (case (length cond-st)
    (0 (write-char #\0 out))
    (1 (print-condition-part out (aref cond-st 0)))
    (t (dotimes (i (length cond-st))
         (when (plusp i) (write-string " || " out))
         (write-char #\( out)
         (print-condition-part out (aref cond-st i))
         (write-char #\) out)))))

(declaim (inline condition-part-true condition-stack-false newline))
(defun condition-part-true (condition) (zerop (length condition)))
(defun condition-stack-false (cond-st) (zerop (length cond-st)))
(defun condition-stack-true (cond-st) (every #'condition-part-true cond-st))
(defun newline (out) (terpri out) (incf *lineno*))

(defmacro with-conditional ((out cond-stack) &body body)
  (let ((cs (gensym "WC-")) (always-true (gensym "WC-")))
    `(let ((,cs ,cond-stack))
       (unless (condition-stack-false ,cs)
         (let ((,always-true (condition-stack-true ,cs)))
           (unless ,always-true
             (write-string "#if " ,out)
             (print-condition-whole ,out ,cs)
             (newline ,out))
           ,@body
           (newline ,out)
           (unless ,always-true
             (write-string "#endif" ,out)
             (newline ,out)))))))

(defun print-tables-1 (out)
  "Output the tables just after the DEFMODULE line"
  (let ((object-tab-initdata
         (ext:string-concat "module__" *module-name* "__object_tab_initdata"))
        (object-tab
         (ext:string-concat "module__" *module-name* "__object_tab"))
        (subr-tab (ext:string-concat "module__" *module-name* "__subr_tab")))
    (newline out) (format out "#define O(varname) ~a._##varname" object-tab)
    (newline out)
    (format out "#define F(varname) subr_tab_ptr_as_object(&(~a._##varname))"
            subr-tab)
    (newline out) (newline out)
    (write-string "struct {" out) (newline out)
    (setq *objdefs* (sort *objdefs* #'string-lessp :key #'objdef-tag)
          *fundefs* (sort *fundefs* #'string-lessp :key #'fundef-tag))
    (loop :for od :across *objdefs*
      :do (with-conditional (out (objdef-cond-stack od))
            (format out "  object _~A;" (objdef-tag od))))
    (loop :for vd :across *vardefs*
      :do (with-conditional (out (vardef-cond-stack vd))
            (format out "  object _~A;" (vardef-tag vd))))
    (format out "} ~A;" object-tab) (newline out)
    (format
     out "uintC module__~A__object_tab_size = sizeof(~A)/sizeof(object);"
     *module-name* object-tab)
    (newline out) (newline out)
    (write-string "struct {" out) (newline out)
    (loop :for od :across *objdefs*
      :do (with-conditional (out (objdef-cond-stack od))
            (format out "  object_initdata_t _~A;" (objdef-tag od))))
    (loop :for vd :across *vardefs*
      :do (with-conditional (out (vardef-cond-stack vd))
            (format out "  object_initdata_t _~A;"  (vardef-tag vd))))
    (write-string "  int _dummy_to_avoid_trailing_comma_in_initializer;" out)
    (newline out) (format out "} ~A = {" object-tab-initdata) (newline out)
    (loop :for od :across *objdefs*
      :do (with-conditional (out (objdef-cond-stack od))
            (format out "  { ~S }," (objdef-init od))))
    (loop :for vd :across *vardefs*
      :do (with-conditional (out (vardef-cond-stack vd))
            (write-string "  { \"NIL\" }," out)))
    (write-string "  0" out) (newline out)
    (write-string "};" out) (newline out) (newline out)
    (format out "struct ~A_t {" subr-tab) (newline out)
    (write-string "  VAROBJECTS_ALIGNMENT_DUMMY_DECL" out) (newline out)
    (loop :for fd :across *fundefs*
      :do (with-conditional (out (fundef-cond-stack fd))
            (format out "  subr_t _~A;" (fundef-tag fd))))
    (write-string "  int _dummy_to_avoid_trailing_comma_in_initializer;" out)
    (newline out) (format out "} ~A;" subr-tab) (newline out) (newline out)))

(defun print-tables-2 (out)
  "Output the tables at the end of the file"
  (let ((subr-tab-initdata
         (ext:string-concat "module__" *module-name* "__subr_tab_initdata"))
        (subr-tab
         (ext:string-concat "module__" *module-name* "__subr_tab")))
    (newline out) (newline out)
    (format out "struct ~A_t ~A" subr-tab subr-tab) (newline out)
    (write-string "  #if defined(HEAPCODES) && (alignment_long < varobject_alignment) && defined(__GNUC__)" out) (newline out)
    (write-string "    __attribute__ ((aligned (varobject_alignment)))" out) (newline out)
    (write-string "  #endif" out) (newline out)
    (write-string "  = {" out) (newline out)
    (write-string "  #if varobjects_misaligned" out) (newline out)
    (write-string "  { 0 }," out) (newline out)
    (write-string "  #endif" out) (newline out)
    (loop :for fd :across *fundefs* :do
      (setf (fundef-signatures fd) (nreverse (fundef-signatures fd)))
      (loop :for sig :in (fundef-signatures fd) :do
        (with-conditional (out (signature-cond-stack sig))
          (format out "  LISPFUN_F~A" (fundef-lispfun fd sig)))))
    (write-string "  0" out) (newline out)
    (write-string "};" out) (newline out)
    (format out "uintC module__~A__subr_tab_size = (sizeof(struct ~A_t)-varobjects_misaligned-sizeof(int))/sizeof(subr_t);" *module-name* subr-tab)
    (newline out) (newline out)
    (write-string "struct {" out) (newline out)
    (loop :for fd :across *fundefs*
      :do (with-conditional (out (fundef-cond-stack fd))
            (format out "  subr_initdata_t _~A;" (fundef-tag fd))))
    (write-string "  int _dummy_to_avoid_trailing_comma_in_initializer;" out)
    (newline out) (format out "} ~A = {" subr-tab-initdata) (newline out)
    (loop :for fd :across *fundefs*
      :do (with-conditional (out (fundef-cond-stack fd))
            (format out "  { ~S, ~S }," (fundef-pack fd) (fundef-name fd))))
    (write-string "  0" out) (newline out)
    (write-string "};" out) (newline out) (newline out)
    (format out "void module__~A__init_function_1 (module_t* module)"
            *module-name*)
    (newline out) (write-string "{" out) (newline out)
    (loop :for fd :across *fundefs* :for tag = (fundef-tag fd) :do
      (loop :for sig :in (fundef-signatures fd) :do
        (when (signature-key-p sig)
          (with-conditional (out (signature-cond-stack sig))
            (dolist (kw (signature-keywords sig))
              (format out "  pushSTACK(O(~A));" (objdef-tag kw))
              (newline out))
            (format out "  ~A._~A.keywords = vectorof(~D);" subr-tab tag
                    (length (signature-keywords sig)))))))
    (loop :for vi :across *varinits*
      :do (with-conditional (out (vector (varinit-condition vi)))
            (format out "  O(~A) = (~A);" (varinit-tag vi) (varinit-init vi))))
    (write-string "}" out) (newline out)
    (when *init-2-name*         ; no init2 => define a dummy
      (newline out)
      (format out "void ~A (module_t* module)" *init-2-name*)
      (newline out) (write-string "{" out) (newline out)
      (write-string "}" out) (newline out))))

(defun output-all (out input-file &optional *lines* &aux (*lineno* 1))
  (format out "#line 1 ~S~%" input-file)
  (loop :for ln :in *lines* :and idx :upfrom 0 :do
    (when (/= *lineno* (line-number ln))
      (format out "#line ~D~%" (setq *lineno* (line-number ln))))
    (write-string (line-contents ln) out) (newline out)
    (when (position #\newline (line-contents ln)) (break "~s" ln))
    (when (= idx *module-line*)
      (print-tables-1 out)))
  (print-tables-2 out))

(defun mod-file (input)
  (make-pathname :name (ext:string-concat (pathname-name input) ".m")
                 :defaults input))

(defun modprep (*input-file* &optional (output (mod-file *input-file*)))
  (format t "~&;; ~S: ~S --> ~S~%" 'modprep *input-file* output)
  (with-open-file (in *input-file* :direction :input
                      #+UNICODE :external-format #+UNICODE charset:utf-8)
    (format t ";; ~S: reading ~S: ~:D byte~:P, "
            'modprep *input-file* (file-length in))
    (force-output)
    (setq *lines* (read-all-input in)))
  (format t "~:D line~:P~%" (length *lines*))
  (parse *lines*)
  (format t "~%;; ~S: ~:D object~:P, ~:D DEFUN~:P~[~:;~:* (~:d emulated)~]~
~[~*~:;~:*, ~:D DEFVAR~:P (~:D init~:P)~]~%;; packages: ~S~%"
          'modprep (length *objdefs*) (length *fundefs*)
          *emulation-count* (length *vardefs*) (length *varinits*)
          *module-all-packages*) ; should we write preload.lisp?
  (with-open-file (out output :direction :output :if-exists :supersede
                       #+UNICODE :external-format #+UNICODE charset:utf-8)
    (output-all out *input-file* *lines*)
    (format t "~&~S: wrote ~S (~:D byte~:P)~&"
            'modprep output (file-length out))))

(time (modprep (first *args*) (or (second *args*) (mod-file (first *args*)))))

;;; file modprep.lisp ends here
