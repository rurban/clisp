;;; Read /usr/share/pari/pari.desc and convert it to pari-call-out forms.
;;; Used by the CLISP PARI module at compile time (not at run time).
;;;
;;; Sam Steingold 2017
;;;
;;; This is Free Software, covered by the GNU GPL (v2+)
;;; See http://www.gnu.org/copyleft/gpl.html

#| debugging:
(in-package #:pari)
(def-c-type pari-gen c-pointer)
(defparameter descriptors (read-pari-desc-file "/usr/local/share/pari/pari.desc"))
(desc-to-ffi (first descriptors))
(desc-to-ffi (seventh descriptors))
(defun desc (n) (find n descriptors :key #'pari::desc-function :test #'string=))
(desc-to-ffi (desc "acos"))
(desc-to-ffi (desc "polcoeff"))
(desc-to-ffi (desc "Catalan"))
(desc-to-ffi (desc "ellformaldifferential"))
(desc-to-ffi (desc "eval"))
(desc-to-ffi (desc "for"))
(desc-to-ffi (desc "lfunconductor"))
(desc-to-ffi (desc "polsturm"))
(desc-to-ffi (desc "printf"))
(desc-to-ffi (desc "Pol"))
(desc-to-ffi (desc "sqrtn"))
(desc-to-ffi (desc "ellmodulareqn"))
(desc-to-ffi (desc "ffnbirred"))
(desc-to-ffi (desc "forqfvec"))
(desc-to-ffi (desc "stirling"))
(desc-to-ffi (desc "precision"))
(desc-to-ffi (desc "weber"))
(desc-to-ffi (desc "znstar"))
(desc-to-ffi (desc "stirling"))
(desc-to-ffi (desc "matadjoint"))
(desc-to-ffi (desc "algcentralproj"))
(mapcar #'desc-to-ffi (remove-if (lambda (d) (member (pari::desc-function d) '("ffnbirred" "polsturm"))) descriptors))
(mapcar #'desc-to-ffi descriptors)
|#

(defpackage "PARI"
  (:modern t)
  (:use #:cl #:ffi))
(in-package "PARI")

(defstruct desc
  lisp-name
  function
  class
  section
  c-name
  prototype
  help
  iterator
  doc
  wrapper
  description
  obsolete
  variant)

(defparameter *desc-slots*      ; all except lisp-name
  (mapcar #'clos:slot-definition-name
          (cdr (clos:class-slots (find-class 'desc)))))

;; append this to GP symbols which are also CL symbols
(defparameter *suffix* "_")

(defun intern-new (name &optional unique)
  ;; intern name in PARI ensuring that it is not a CL symbol
  (multiple-value-bind (symbol status) (intern name #1="PARI")
    (when (eq status :INHERITED)
      (setf (values symbol status)
            (intern (ext:string-concat name *suffix*) #1#)))
    (when (and unique status)
      (warn "Existing name: ~S (~S ~S)" name symbol status))
    symbol))

(defun lines-to-desc (lines)
  ;; convert list of line to table of fields
  (let ((ret (make-desc))
        field cur-data)
    (flet ((store ()
             (when (slot-value ret field)
               (error "~S: duplicate field ~S in ~S" 'lines-to-desc field lines))
             (setf (slot-value ret field)
                   (if (cdr cur-data)
                       (nreverse cur-data) ; many lines
                       (car cur-data)))))  ; one line
      (dolist (line lines)
        (let ((pos (search ": " line)))
          (cond ((and pos (char/= #\Space (char line 0))) ; new field
                 (when field
                   (store))
                 (setf field (or (find (subseq line 0 pos) *desc-slots*
                                       :test #'string-equal)
                                 (error "~S: unknown field: ~S" 'lines-to-desc line))
                       cur-data (list (subseq line (+ 2 pos)))))
                (t                ; continuation
                 (push line cur-data)))))
      (store)
      ret)))

(defun pari-from-string (s)
  ;; %read-from-string - unnecessary
  (or (parse-integer s :junk-allowed t)
      (let ((s (ext:trim-if #'sys::whitespacep s)))
        (if (string= s "[]")
            nil
            s))))

(defun parse-arglist-regexp (h &key (start 0) (end nil))
  (mapcar (lambda (s)
            (if (char= #\{ (char s 0))
                (let ((= (position #\= s)))
                  (if =
                      (list (intern-new (ext:trim-if #'sys::whitespacep
                                                     (subseq s 1 =)))
                            (pari-from-string
                             (subseq s (1+ =) (1- (length s)))))
                      (intern-new (subseq s 1 (1- (length s))))))
                (intern-new s)))
          (delete 0 (regexp:regexp-split
                     #,(regexp:regexp-compile " *, *")
                     h :start start :end end)
                  :key #'length)))

(defun pari-read-char (stream char)
  (case char
    (#\{ (read-delimited-list #\} stream))
    (#\[ (read-delimited-list #\] stream))
    (t char)))
(defun pari-arglist-readtable ()
  (let ((rt (copy-readtable nil)))
    (set-macro-character #\[ #'pari-read-char nil rt)
    (set-macro-character #\{ #'pari-read-char nil rt)
    (set-macro-character #\] (get-macro-character #\)) nil rt)
    (set-macro-character #\} (get-macro-character #\)) nil rt)
    ; (set-macro-character #\* #'pari-read-char nil rt)
    (set-macro-character #\, #'pari-read-char nil rt)
    ;(set-macro-character #\= #'pari-read-char nil rt)
    (set-syntax-from-char #\* #\Space rt) ; variadic: {foo}*
    ;(set-syntax-from-char #\, #\Space rt) ; argument separator
    (set-syntax-from-char #\= #\Space rt) ; default values
    rt))
(defconstant pari-arglist-readtable (pari-arglist-readtable))

(defun parse-arglist-read (h &key (start 0) (end nil))
  (do* ((*readtable* pari-arglist-readtable) arglist
        (args (read-from-string h t nil :start start :end end)))
       ((endp args)
        (nreverse arglist))
    (let (this)
      (loop (let ((o (pop args)))
              (when (or (null o) (eql o #\,))
                (return))
              (push o this)))
      (push (if (cdr this)
                (nreverse this)
                (car this))
            arglist))))

(defun desc-arglist (desc)
  ;; help often starts with f(x,a,b,{flag=4}) -- get the arglist!
  (let ((h (desc-help desc)) (f (desc-function desc)) end)
    (when (consp h)
      (setq h (car h)))
    (if (and h (ext:starts-with-p h f) (setq end (search "):" h)))
        ;;(parse-arglist-regexp h :start (1+ (position #\( h)) :end end)
        (parse-arglist-read h :start (position #\( h) :end (1+ end))
        ;; (or (warn "~S: no arglist in ~S" 'desc-arglist desc) ...)
        :unspecific)))

(defun maybe-cons (default value tail)
  (and (or (not (eq value default)) tail)
       (cons value tail)))

(defun parse-type-character (char)
  ;; 4 values: type, mode, default, vname
  (case char
    (#\G (values 'pari-gen :in nil))
    (#\& (values '(ffi:c-ptr pari-gen) :out))
    (#\L (values 'ffi:long :in 0))
    (#\U (values 'ffi:ulong :in 0))
    (#\V (values 'ffi:long :in)) ; loop variable
    (#\n (values 'ffi:long :in -1 'varno))
    (#\W (values '(ffi:c-ptr pari-gen) :in-out nil))
    (#\C (values 'pari-gen :in nil 'context))
    ((#\I #\E #\J) (values 'pari-gen :in))
    ((#\s #\r) (values 'ffi:c-string :in nil))   ; r is actually "const char*"
    (#\f (values '(ffi:c-ptr ffi:long) :in nil)) ; fake
    (#\p (values 'ffi:long :in 'pari-real-precision 'prec))
    (#\P (values 'ffi:long :in 'pari-series-precision 'precdl))
    (#\b (values 'ffi:long :in '(digits2bits pari-real-precision) 'precbit))
    (#\D (values #\D))
    (t nil)))

(defun parse-prototype (desc)
  ;; convert string prototype to (form ret-type params-specs)
  (when (equal "" (desc-prototype desc))
    (return-from parse-prototype (values 'pari-call-out 'pari-gen ())))
  (let* ((prototype (desc-prototype desc))
         (arg-start 1) params
         (end-prot (length prototype))
         (form (if (char= #\p (char prototype (1- end-prot)))
                   (progn
                     (decf end-prot)
                     'pari-call-out-prec)
                   'pari-call-out))
         (ret-type
          (case (char prototype 0)
            (#\i 'ffi:int)
            (#\l 'ffi:long)
            (#\u 'ffi:ulong)
            (#\v nil)           ; void
            (#\m 'pari-gen)     ; not gerepile-safe -- call gcopy?
            (t (setq arg-start 0)
               'pari-gen))))
    (do ((pos arg-start (1+ pos)) atype amode adefault aname optionalp)
        ((>= pos end-prot))
      (multiple-value-bind (type-sym-1 mode-1 default-1 name-1)
          (parse-type-character (char prototype pos))
        (when (and (< (1+ pos) end-prot) (char= #\* (char prototype (1+ pos))))
          #+(or)                ; not an interesting warning
          (unless (eq type-sym-1 'pari-gen)
            (warn "~S(~S:~S): Convert ~S/~S/~S to ~S/~S/~S because of *"
                  'parse-prototype (desc-function desc) prototype
                  type-sym-1 mode-1 default-1 'pari-gen :in nil))
          (setq type-sym-1 'pari-gen
                mode-1 :in
                default-1 nil
                pos (1+ pos)))
        (unless type-sym-1
          (error "~S(~S@~D): invalid type spec ~@C in ~S"
                 'parse-prototype prototype pos (char prototype pos) desc))
        (if (eq type-sym-1 #\D)
            (multiple-value-bind (type-sym-2 mode-2 default-2 name-2)
                (parse-type-character (char prototype (incf pos)))
              (if type-sym-2
                  (setq atype (and (ignore-errors
                                     (ffi:parse-c-type type-sym-2))
                                   type-sym-2)
                        amode mode-2
                        adefault default-2
                        aname name-2
                        ;; :out must NOT have default in pari-call-out
                        optionalp (eq mode-2 :in))
                  (let ((next-comma (position #\, prototype :start pos)))
                    (setq adefault (read-from-string prototype nil nil
                                                     :start pos :end next-comma)
                          pos (1+ next-comma))
                    (when (symbolp adefault)
                      (error "~S(~S): bad default ~S in ~S"
                             'parse-prototype prototype adefault desc))
                    (multiple-value-bind (type-sym-3 mode-3 default-3 name-3)
                        (parse-type-character (char prototype pos))
                      (declare (ignore default-3))
                      (setq atype type-sym-3
                            amode mode-3
                            aname name-3
                            optionalp (eq mode-3 :in)
                            pos (position #\, prototype :start pos))))))
            (setq atype type-sym-1
                  amode mode-1
                  adefault default-1
                  aname name-1
                  optionalp nil)))
      (push (list* aname atype
                   (maybe-cons :in amode
                               (maybe-cons :none (if (eq amode :in) :none :alloca)
                                           (and optionalp (list adefault)))))
            params))
    ;; set param names from arglist
    (let ((explicit-params (if (sys::memq (caar params) '(precbit context))
                               (cdr params)
                               params))
          (arglist (desc-arglist desc)))
      ;; precision & context params are not mentioned in the arglist
      (if (eq arglist :unspecific)
          (setq arglist nil)
          (when (/= (length arglist) (length explicit-params))
            (warn "~S(~S:~S): mismatch: params=~S arglist=~S in ~S"
                  'parse-prototype (desc-function desc) prototype
                  explicit-params arglist desc)
            (setq arglist nil)))
      ;; arglist is either nil or a list of the same length as explicit-params
      (do ((vars (reverse arglist) (cdr vars))
           (letter #\a)
           (pars explicit-params (cdr pars)))
          ((endp pars))
        (let ((a (car vars)))
          (cond (a              ; arglist from help has top priority
                 (when (and (consp a)
                            (eq (third (car pars)) :in)
                            (null (cddddr (car pars))))
                   ;; check for optionality, not sameness of default
                   (warn "~S(~S:~S): optional arg mismatch ~S vs ~S/~S in ~S"
                         'parse-prototype (desc-function desc) prototype
                         a (car pars) params (desc-help desc)))
                 (setf (caar pars)
                       (if (consp a)
                           (car a)
                           a)))
                ((caar pars)    ; parse-type-character returned a name
                 )              ; all set!
                (t              ; oops, have to generate a var name
                 (setf (caar pars) (intern-new (string letter))
                       letter (code-char (1+ (char-code letter)))))))
        ;; ensure we are not using "t" is the arg name
        (unless (eq (symbol-package (caar pars)) #.(find-package "PARI"))
          (setf (caar pars) (intern-new (symbol-name (caar pars)))))))
    (values form ret-type
            ;; (a pari-gen) --> a
            (mapcar (lambda (cell)
                      (if (and (eq (second cell) 'pari-gen)
                               (null (cddr cell)))
                          (car cell)
                          cell))
                    (nreverse params)))))

(defun desc-to-ffi (desc)
  ;; convert desc to the pari-call-out FFI form
  (multiple-value-bind (form ret-type params) (parse-prototype desc)
    (if form
        `(,form
          ,(if (eq ret-type 'pari-gen)
               (desc-lisp-name desc)
               (list (desc-lisp-name desc) ret-type))
          ,(desc-c-name desc) ,params
          ,@(and (string/= (desc-function desc) (desc-c-name desc))
                 `(,(desc-function desc))))
        (warn "~S: bad prototype in ~S" 'desc-to-ffi desc))))

(defun hash-counter-to-strings (ht)
  ;; pretty representation of a counter hash table
  (let ((alist ()) width-k width-v)
    (maphash (lambda (k v) (push (cons k v) alist)) ht)
    (setq alist (sort alist #'> :key #'cdr)
          width-k (reduce #'max alist :initial-value 0
                          :key (lambda (cell) (length (car cell))))
          ;; the biggest count is in the 1st cell
          width-v (length (format nil "~:D" (cdar alist))))
    (mapcar (lambda (cell)
              (format nil "~V@A: ~V:D" width-k (car cell) width-v (cdr cell)))
            alist)))

(defun read-lines-until (inf prefix)
  "Read lines from INF until the line starting with prefix."
  (do ((line (read-line inf nil nil) (read-line inf nil nil))
       (lines nil (cons line lines)))
      ((or (null line) (ext:starts-with-p line prefix))
       (values (nreverse lines) line))))

(defvar *pari-desc-filters*
  (macrolet ((no-section (name)
               `'(,name ,(compile nil `(lambda (d) (string= ,name (desc-section d)))))))
    `(("wrapped" ,#'desc-wrapper)
      ("no C-name" ,(lambda (d) (null (desc-c-name d))))
      ("obsolete" ,#'desc-obsolete)
      ("non-basic" ,(lambda (d) (string/= "basic" (desc-class d))))
      ,(no-section "programming/control")
      ,(no-section "programming/internals")
      ,(no-section "programming/parallel")
      ,(no-section "graphic")
      ,(no-section "sums"))))

(defun read-pari-desc-file (fname &key (filters *pari-desc-filters*))
  ;; https://pari.math.u-bordeaux.fr/dochtml/html-stable/usersch5.html#GP-prototypes-parser-codes
  (let ((functions-read 0) (descriptors ()) ; (names (make-hash-table))
        (filtered (make-hash-table :test 'equal))
        (classes (make-hash-table :test 'equal))
        (sections (make-hash-table :test 'equal)))
    (with-open-file (inf fname)
      (when *compile-verbose*
        (format t "Reading ~S (~:D bytes)~%" fname (file-length inf)))
      (do ((read-ahead (read-line inf nil nil)))
          ((null read-ahead)
           (setq descriptors (nreverse descriptors)))
        (multiple-value-bind (lines read-next)
            ;; every record starts with the Function field
            (read-lines-until inf "Function: ")
          ;; (break "read: ~S" lines)
          (let ((desc (lines-to-desc (cons read-ahead lines))) bad)
            (setq read-ahead read-next)
            (incf functions-read)
            (incf (gethash (desc-class desc) classes 0))
            (incf (gethash (desc-section desc) sections 0))
            (dolist (filter filters)
              (destructuring-bind (name fifunc) filter
                (when (funcall fifunc desc)
                  (incf (gethash name filtered 0))
                  ;; (warn "Ignore ~A ~S" name desc)
                  (setq bad t))))
            (unless bad
              (setf (desc-lisp-name desc) (intern-new (desc-function desc) t))
              (push desc descriptors))))))
    (when *compile-verbose*
      (format t "Read ~:D functions~%Ignored:~{~%  ~A~}~%~
                 Classes:~{~%  ~A~}~%Sections:~{~%  ~A~}~%~
                 Emitted ~:D functions~%"
              functions-read
              (hash-counter-to-strings filtered)
              (hash-counter-to-strings classes)
              (hash-counter-to-strings sections)
              (length descriptors)))
    descriptors))
