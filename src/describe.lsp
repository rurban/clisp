;;;; Apropos, Describe

(in-package "SYSTEM")

;;-----------------------------------------------------------------------------
;; APROPOS

(defun apropos-list (string &optional (package nil))
  (let* ((L nil)
         (fun #'(lambda (sym)
                  (when
                      #| (search string (symbol-name sym) :test #'char-equal) |#
                      (sys::search-string-equal string sym) ; 15 mal schneller!
                    (push sym L)
                ) )
        ))
    (if package
      (system::map-symbols fun package)
      (system::map-all-symbols fun)
    )
    (stable-sort (delete-duplicates L :test #'eq :from-end t)
                 #'string< :key #'symbol-name
    )
) )

(defvar *apropos-do-more* nil
  "Print values of the symbols in `apropos'.
It can be a list of :FUNCTION, :VARIABLE, :TYPE, :CLASS
to print the corresponding values, or T for all of them.")

(defun apropos-do-more (what)
  (or (eq t *apropos-do-more*) (member what *apropos-do-more* :test #'eq)))

(defun apropos (string &optional (package nil))
  (dolist (sym (apropos-list string package) (terpri))
    (format t "~&~s~40t" sym)
    (when (fboundp sym)
      (write-string "   ")
      (write-string (fbound-string sym))
      (when (apropos-do-more :function)
        (format t " [~s]" (fdefinition sym))))
    (when (boundp sym)
      (write-string "   ")
      (cond ((constantp sym) (write-string (ENGLISH "constant")))
            ((sys::symbol-macro-p (sys::%symbol-value sym))
             (write-string (ENGLISH "symbol-macro")))
            (t (write-string (ENGLISH "variable"))))
      (when (apropos-do-more :variable)
        (format t " [~s]" (sys::%symbol-value sym))))
    (let ((type (or (get sym 'system::type-symbol)
                    (get sym 'system::defstruct-description))))
      (when type
        (write-string "   ")
        (write-string (ENGLISH "type"))
        (when (apropos-do-more :type)
          (format t " [~s]" type))))
    (let ((class (get sym 'clos::closclass)))
      (when class
        (write-string "   ")
        (write-string (ENGLISH "class"))
        (when (apropos-do-more :class)
          (format t " [~s]" class)))))
  (values))

;;-----------------------------------------------------------------------------
;; DESCRIBE

; Number of recursive calls since the top-level call.
(defvar *describe-nesting*)

; A stream whose purpose is
; 1. to provide automatic indentation,
; 2. to distinguish top-level calls from recursive calls.
(clos:defclass describe-stream (fundamental-character-output-stream)
  ((target-stream :initarg :stream :type stream)
   (indent :initform 0 :type integer) ; current line's indentation
   (pending-indent :initform nil :type (or null integer))
) )
(clos:defmethod stream-write-char ((stream describe-stream) ch)
  (clos:with-slots (target-stream indent pending-indent) stream
    (if (eql ch #\Newline)
      (progn
        (write-char ch target-stream)
        (setq indent (* *describe-nesting* *print-indent-lists*))
        (setq pending-indent indent)
      )
      (progn
        (when pending-indent
          (dotimes (i pending-indent) (write-char #\Space target-stream))
          (setq pending-indent nil)
        )
        (write-char ch target-stream)
) ) ) )
(clos:defmethod stream-line-column ((stream describe-stream))
  (clos:with-slots (target-stream indent) stream
    (let ((pos (sys::line-position target-stream)))
      (if pos (max (- pos indent) 0) nil)
) ) )
(clos:defmethod stream-start-line-p ((stream describe-stream))
  (clos:with-slots (target-stream indent) stream
    (let ((pos (sys::line-position target-stream)))
      (if pos (<= pos indent) nil)
) ) )
(clos:defmethod stream-finish-output ((stream describe-stream))
  (clos:with-slots (target-stream pending-indent) stream
    (when pending-indent
      (dotimes (i pending-indent) (write-char #\Space target-stream))
      (setq pending-indent nil)
    )
    (finish-output target-stream)
) )
(clos:defmethod stream-force-output ((stream describe-stream))
  (clos:with-slots (target-stream pending-indent) stream
    (when pending-indent
      (dotimes (i pending-indent) (write-char #\Space target-stream))
      (setq pending-indent nil)
    )
    (force-output target-stream)
) )
(clos:defmethod stream-clear-output ((stream describe-stream))
  (clos:with-slots (target-stream pending-indent) stream
    (setq pending-indent nil)
    (clear-output target-stream)
) )

; List of objects which have been described during the current top-level call.
(defvar *describe-done*)

(defun describe-slotted-object (object stream)
  (let ((slotnames (mapcar #'clos::slotdef-name (clos::class-slots (clos:class-of object)))))
    (if slotnames
      (let* ((slotstrings (mapcar #'write-to-string slotnames)) more
             (tabpos (+ *print-indent-lists* 4 (reduce #'max (mapcar #'length slotstrings)))))
        (format stream (ENGLISH "~%Slots:"))
        (mapc #'(lambda (slotname slotstring)
                  (format stream "~%~V,0T  ~A~VT"
                          *print-indent-lists* slotstring tabpos)
                  (cond ((clos:slot-boundp object slotname)
                         (format stream "=  ~S" (clos:slot-value object slotname))
                         (pushnew (clos:slot-value object slotname) more))
                        ((format stream (ENGLISH "unbound")))))
              slotnames slotstrings)
        (dolist (vv (nreverse more)) (describe vv stream)))
      (format stream (ENGLISH "~%No slots.")))))

(clos:defgeneric describe-object (obj stream)
  (:method ((obj t) (stream stream))
    (ecase (type-of obj)
      #+(or AMIGA FFI)
      (FOREIGN-POINTER
       (format stream (ENGLISH "a foreign pointer")))
      #+FFI
      (FOREIGN-ADDRESS
       (format stream (ENGLISH "a foreign address")))
      #+FFI
      (FOREIGN-VARIABLE
       (format stream (ENGLISH "a foreign variable of foreign type ~S.")
               (deparse-c-type (sys::%record-ref obj 3))))
      #+FFI
      (FOREIGN-FUNCTION
       (format stream (ENGLISH "a foreign function taking foreign types ~:S and returning foreign type ~S.")
               (map 'list #'deparse-c-type (sys::%record-ref obj 3))
               (deparse-c-type (sys::%record-ref obj 2))))
      (BYTE
       (format stream (ENGLISH "a byte specifier, denoting the ~S bits starting at bit position ~S of an integer.")
               (byte-size obj) (byte-position obj)))
      (SPECIAL-OPERATOR
       (format stream (ENGLISH "a special form handler.")))
      (LOAD-TIME-EVAL
       (format stream (ENGLISH "a load-time evaluation promise.")))
      (SYMBOL-MACRO
       (format stream (ENGLISH "a symbol macro handler.")))
      (ENCODING
       (format stream (ENGLISH "an encoding.")))
      (WEAK-POINTER
       (multiple-value-bind (value validp) (weak-pointer-value obj)
         (if validp
           (progn
             (format stream (ENGLISH "a GC-invisible pointer to ~S.")
                     value)
             (describe value stream))
           (format stream (ENGLISH "a GC-invisible pointer to a now defunct object.")))))
      (READ-LABEL
       (format stream (ENGLISH "a label used for resolving #~D# references during READ.")
               (logand (sys::address-of obj)
                       (load-time-value (ash most-positive-fixnum -1)))))
      (FRAME-POINTER
       (format stream (ENGLISH "a pointer into the stack. It points to:"))
       (sys::describe-frame stream obj))
      (SYSTEM-INTERNAL
       (format stream (ENGLISH "a special-purpose object.")))
      (ADDRESS
       (format stream (ENGLISH "a machine address.")))))
  (:method ((obj clos:standard-object) (stream stream))
      (format stream (ENGLISH "an instance of the CLOS class ~S.")
              (clos:class-of obj))
   (describe-slotted-object obj stream))
  (:method ((obj structure-object) (stream stream)) ; CLISP specific
    (format stream (ENGLISH "a structure of type ~S.")
            (type-of obj))
    (let ((types (butlast (cdr (sys::%record-ref obj 0)))))
      (when types
        (format stream (ENGLISH "~%As such, it is also a structure of type ~{~S~^, ~}.")
                types)))
    (describe-slotted-object obj stream))
  (:method ((obj cons) (stream stream))
    (let ((len ; cf. function list-length in CLtL p. 265
           (do ((n 0 (+ n 2))
                (fast obj (cddr fast))
                (slow obj (cdr slow)))
               (nil)
             (when (atom fast) (return n))
             (when (atom (cdr fast)) (return (1+ n)))
             (when (eq (cdr fast) slow) (return nil)))))
      (if len
        (if (null (nthcdr len obj))
          (format stream (ENGLISH "a list of length ~S.")
                  len)
          (if (> len 1)
            (format stream (ENGLISH "a dotted list of length ~S.")
                    len)
            (progn (format stream (ENGLISH "a cons."))
                   (describe (car obj) stream)
                   (describe (cdr obj) stream))))
        (format stream (ENGLISH "a cyclic list.")))))
  (:method ((obj null) (stream stream))
    (format stream (ENGLISH "the empty list, "))
    (clos:call-next-method))
  (:method ((obj symbol) (stream stream))
    (format stream (ENGLISH "the symbol ~S, ")
            obj)
    (let ((home (symbol-package obj)) mored moree)
      (cond (home
             (format stream (ENGLISH "lies in ~S")
                     home)
             (pushnew home mored))
            (t (format stream (ENGLISH "is uninterned"))))
      (let ((accessible-packs nil))
        (let ((*print-escape* t) (*print-readably* nil))
          (let ((normal-printout
                 (if home
                   (let ((*package* home)) (prin1-to-string obj))
                   (let ((*print-gensym* nil)) (prin1-to-string obj)))))
            (dolist (pack (list-all-packages))
              (when ; obj in pack accessible?
                  (string=
                   (let ((*package* pack)) (prin1-to-string obj))
                   normal-printout)
                (push pack accessible-packs)))))
        (when accessible-packs
          (format stream (ENGLISH ", is accessible in the package~:[~;s~] ~{~A~^, ~}")
                  (cdr accessible-packs)
                  (sort (mapcar #'package-name accessible-packs)
                        #'string<))))
      (when (keywordp obj)
        (format stream (ENGLISH ", is a keyword")))
      (when (boundp obj)
        (if (constantp obj)
          (format stream (ENGLISH ", a constant"))
          (if (sys::special-variable-p obj)
            (format stream (ENGLISH ", a variable declared SPECIAL"))
            (format stream (ENGLISH ", a variable"))))
        (when (symbol-macro-expand obj)
          (format stream (ENGLISH " (macro: ~s)")
                  (macroexpand-1 obj))
          (push `(macroexpand-1 ',obj) moree))
        (format stream (ENGLISH ", value: ~s") (sys::%symbol-value obj))
        (pushnew (sys::%symbol-value obj) mored))
      (when (fboundp obj)
        (format stream (ENGLISH ", names "))
        (cond ((special-operator-p obj)
               (format stream (ENGLISH "a special operator"))
               (when (macro-function obj)
                 (format stream (ENGLISH " with macro definition"))))
              ((functionp (symbol-function obj))
               (format stream (ENGLISH "a~:[~; deprecated~] function")
                       (member obj *deprecated-functions-list* :test #'eq)))
              (t ; (macro-function obj)
               (format stream (ENGLISH "a macro"))))
        (pushnew (symbol-function obj) mored))
      (when (or (get obj 'system::type-symbol)
                (get obj 'system::defstruct-description)
                (get obj 'system::deftype-expander))
        (format stream (ENGLISH ", names a type"))
        (when (get obj 'system::deftype-expander)
          (push `(type-expand-1 ',obj) moree)))
      (when (get obj 'clos::closclass)
        (format stream (ENGLISH ", names a class")))
      (when (symbol-plist obj)
        (let ((properties
               (do ((l nil) (pl (symbol-plist obj) (cddr pl)))
                   ((null pl) (nreverse l))
                 (push (car pl) l))))
          (format stream (ENGLISH ", has the propert~@P ~{~S~^, ~}")
                  (length properties) properties))
        (push `(symbol-plist ',obj) moree))
      (format stream (ENGLISH "."))
      (dolist (ty '(compiler-macro setf structure type variable function))
        (let ((doc (documentation obj ty)))
          (when doc
            (format stream (ENGLISH "~%Documentation as a ~a:~%~a") ty doc))))
      (when moree
        (format stream (ENGLISH "~%For more information, evaluate ~{~S~^ or ~}.")
                moree))
      (dolist (zz (nreverse mored)) (describe zz stream))))
  (:method ((obj integer) (stream stream))
    (format stream (ENGLISH "an integer, uses ~S bit~:p, is represented as a ~(~A~).")
            (integer-length obj) (type-of obj)))
  (:method ((obj ratio) (stream stream))
    (format stream (ENGLISH "a rational, not integral number.")))
  (:method ((obj float) (stream stream))
    (format stream (ENGLISH "a float with ~S bits of mantissa (~(~A~)).")
            (float-digits obj) (type-of obj)))
  (:method ((obj complex) (stream stream))
    (format stream (ENGLISH "a complex number "))
    (let ((x (realpart obj))
          (y (imagpart obj)))
      (if (zerop y)
        (if (zerop x)
          (format stream (ENGLISH "at the origin"))
          (format stream (ENGLISH "on the ~:[posi~;nega~]tive real axis")
                  (minusp x)))
        (if (zerop x)
          (format stream (ENGLISH "on the ~:[posi~;nega~]tive imaginary axis")
                  (minusp y))
          (format stream (ENGLISH "in ~:[~:[first~;fourth~]~;~:[second~;third~]~] the quadrant")
                  (minusp x) (minusp y)))))
    (format stream (ENGLISH " of the Gaussian number plane.")))
  (:method ((obj character) (stream stream))
    (format stream (ENGLISH "a character"))
    (format stream (ENGLISH "."))
    #+UNICODE
    (let ((unicode-name (unicode-attributes obj)))
      (if unicode-name
        (format stream (ENGLISH "~%Unicode name: ~A") unicode-name)
        (format stream (ENGLISH "~%It is not defined by the Unicode standard."))))
    (format stream (ENGLISH "~%It is a ~:[non-~;~]printable character.")
            (graphic-char-p obj))
    (unless (standard-char-p obj)
      (format stream (ENGLISH "~%Its use is non-portable."))))
  (:method ((obj stream) (stream stream))
    (format stream (ENGLISH "a~:[~:[ closed ~;n output-~]~;~:[n input-~;n input/output-~]~]stream.")
            (and (input-stream-p obj) (open-stream-p obj))
            (and (output-stream-p obj) (open-stream-p obj))))
  (:method ((obj package) (stream stream))
    (if (package-name obj)
      (progn
        (format stream (ENGLISH "the package named ~A")
                (package-name obj))
        (let ((nicknames (package-nicknames obj)))
          (when nicknames
            (format stream (ENGLISH ". It has the nicknames ~{~A~^, ~}")
                    nicknames)))
        (format stream (ENGLISH "."))
        (let ((use-list (package-use-list obj))
              (used-by-list (package-used-by-list obj)))
          (format stream (ENGLISH "~%It "))
          (when use-list
            (format stream (ENGLISH "imports the external symbols of the package~:[~;s~] ~{~A~^, ~} and ")
                    (cdr use-list) (mapcar #'package-name use-list)))
          (let ((L nil)) ; maybe list all exported symbols
            (do-external-symbols (s obj) (push s L))
            (if (zerop *describe-nesting*)
              (format stream (ENGLISH "exports ~:[no symbols~;the symbols~:*~{~<~%~:; ~S~>~^~}~%~]")
                      (sort L #'string< :key #'symbol-name))
              (format stream (ENGLISH "exports ~[no symbols~:;~:*~:d symbols~]")
                      (length L))))
          (if used-by-list
            (format stream (ENGLISH " to the package~:[~;s~] ~{~A~^, ~}")
                    (cdr used-by-list)
                    (mapcar #'package-name used-by-list))
            (format stream (ENGLISH ", but no package uses these exports")))
          (format stream (ENGLISH "."))))
      (format stream (ENGLISH "a deleted package."))))
  (:method ((obj hash-table) (stream stream))
    (format stream (ENGLISH "a hash table with ~S entr~:@P.")
            (hash-table-count obj)))
  (:method ((obj readtable) (stream stream))
    (format stream (ENGLISH "~:[a~;the Common Lisp~] readtable.")
            (equalp obj (copy-readtable))))
  (:method ((obj pathname) (stream stream))
    (format stream (ENGLISH "a ~:[~;portable ~]pathname~:[.~;~:*, with the following components:~{~A~}~]")
            (sys::logical-pathname-p obj)
            (mapcan #'(lambda (kw component)
                        (when component
                          (list (format nil "~%~A = ~A"
                                        (symbol-name kw)
                                        (make-pathname kw component)))))
                    '(:host :device :directory :name :type :version)
                    (list (pathname-host obj)
                          (pathname-device obj)
                          (pathname-directory obj)
                          (pathname-name obj)
                          (pathname-type obj)
                          (pathname-version obj)))))
  (:method ((obj random-state) (stream stream))
    (format stream (ENGLISH "a random-state.")))
  (:method ((obj array) (stream stream))
    (let ((rank (array-rank obj))
          (eltype (array-element-type obj)))
      (format stream (ENGLISH "a~:[~; simple~] ~A dimensional array")
              (simple-array-p obj) rank)
      (when (eql rank 1)
        (format stream (ENGLISH " (vector)")))
      (unless (eq eltype 'T)
        (format stream (ENGLISH " of ~(~A~)s")
                eltype))
      (when (adjustable-array-p obj)
        (format stream (ENGLISH ", adjustable")))
      (when (plusp rank)
        (format stream (ENGLISH ", of size ~{~S~^ x ~}")
                (array-dimensions obj))
        (when (array-has-fill-pointer-p obj)
          (format stream (ENGLISH " and current length (fill-pointer) ~S")
                  (fill-pointer obj))))
      (format stream (ENGLISH "."))))
  (:method ((obj function) (stream stream))
    (ecase (type-of obj)
      #+FFI
      (FOREIGN-FUNCTION
       (format stream (ENGLISH "a foreign function."))
       (multiple-value-bind (req opt rest-p key-p keywords other-keys-p)
           (sys::function-signature obj)
         (sys::describe-signature stream req opt rest-p key-p keywords
                                  other-keys-p)))
      (COMPILED-FUNCTION ; SUBR
       (format stream (ENGLISH "a built-in system function."))
       (multiple-value-bind (name req opt rest-p keywords other-keys)
           (sys::subr-info obj)
         (when name
           (sys::describe-signature stream req opt rest-p
                                    keywords keywords other-keys))))
      (FUNCTION
       (format stream
               (ENGLISH "a~:[n interpret~; compil~]ed function.")
               (compiled-function-p obj))
       (if (compiled-function-p obj)
         (multiple-value-bind (req opt rest-p key-p keywords other-keys-p)
             (sys::signature obj)
           (sys::describe-signature stream req opt rest-p key-p keywords
                                    other-keys-p)
           (format stream (ENGLISH "~%For more information, evaluate ~{~S~^ or ~}.")
                   `((DISASSEMBLE #',(sys::closure-name obj)))))
         (let ((doc (sys::%record-ref obj 2)))
           (format stream (ENGLISH "~%argument list: ~:S")
                   (car (sys::%record-ref obj 1)))
           (when doc
             (format stream (ENGLISH "~%documentation: ~A") doc))))))))

(defun describe1 (obj stream)
  (let ((objstring (sys::write-to-short-string obj (or *print-right-margin* sys::*prin-linelength*))))
    (if (member obj *describe-done* :test #'eq)
      (format stream (ENGLISH "~&~%~A [see above]") objstring)
      (progn
        (push obj *describe-done*)
        (format stream (ENGLISH "~&~%~A is ") objstring)
        (describe-object obj stream)
) ) ) )

(defun describe (obj &optional stream)
  (if (typep stream 'describe-stream)
    ; Recursive call
    (let ((*describe-nesting* (1+ *describe-nesting*))
          (*print-right-margin*
             (max (- (or *print-right-margin* sys::*prin-linelength*)
                     *print-indent-lists*)
                  1)))
      (describe1 obj stream)
    )
    ; Top-level call
    (progn
      (cond ((eq stream 'nil) (setq stream *standard-output*))
            ((eq stream 't) (setq stream *terminal-io*)))
      (let ((*print-circle* t)
            (*describe-nesting* 0)
            (*describe-done* nil))
        (describe1 obj (clos:make-instance 'describe-stream :stream stream))
    ) )
  )
  (values)
)

;;-----------------------------------------------------------------------------
;; auxiliary functions for DESCRIBE of FUNCTION

; Liefert die Signatur eines funktionalen Objekts, als Werte:
; 1. req-anz
; 2. opt-anz
; 3. rest-p
; 4. key-p
; 5. keyword-list
; 6. allow-other-keys-p
(defun function-signature (obj)
  (if (sys::closurep obj)
    (if (compiled-function-p obj)
      ; compilierte Closure
      (multiple-value-bind (req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p)
          (sys::signature obj) ; siehe compiler.lsp
        (values req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p)
      )
      ; interpretierte Closure
      (let ((clos_keywords (sys::%record-ref obj 16)))
        (values (sys::%record-ref obj 12) ; req_anz
                (sys::%record-ref obj 13) ; opt_anz
                (sys::%record-ref obj 19) ; rest_flag
                (not (numberp clos_keywords))
                (if (not (numberp clos_keywords)) (copy-list clos_keywords))
                (sys::%record-ref obj 18) ; allow_flag
      ) )
    )
    (cond #+FFI
          ((eq (type-of obj) 'FOREIGN-FUNCTION)
           (values (sys::foreign-function-signature obj) 0 nil nil nil nil)
          )
          (t
           (multiple-value-bind (name req-anz opt-anz rest-p keywords allow-other-keys)
               (sys::subr-info obj)
             (if name
               (values req-anz opt-anz rest-p keywords keywords allow-other-keys)
               (error (ENGLISH "~S: ~S is not a function.")
                      'function-signature obj
               )
) ) )     )) )

(defun signature-to-list (req-anz opt-anz rest-p keyword-p keywords
                          allow-other-keys)
  (let ((args '()) (count -1))
      (dotimes (i req-anz)
      (push (intern (format nil "ARG~D" (incf count)) :sys) args))
      (when (plusp opt-anz)
        (push '&OPTIONAL args)
        (dotimes (i opt-anz)
        (push (intern (format nil "ARG~D" (incf count)) :sys) args)))
      (when rest-p
        (push '&REST args)
      (push 'other-args args))
      (when keyword-p
        (push '&KEY args)
      (dolist (kw keywords) (push kw args))
      (when allow-other-keys (push '&ALLOW-OTHER-KEYS args)))
    (nreverse args)))

(defun arglist (func)
  (multiple-value-call #'signature-to-list (function-signature func)))

(defun describe-signature (s req-anz opt-anz rest-p keyword-p keywords
                           allow-other-keys)
  (when s
    (format s (ENGLISH "~%Argument list: ")))
  (format s "(~{~A~^ ~})"
          (signature-to-list req-anz opt-anz rest-p keyword-p keywords
                             allow-other-keys)))

;;-----------------------------------------------------------------------------
;; auxiliary functions for DESCRIBE of CHARACTER

#+UNICODE (progn

(defun unicode-data-file ()
  ; $(lisplibdir)/data/UnicodeData.txt
  (merge-pathnames
    #-DOS "UnicodeData.txt" #+DOS "UNICODED.TXT"
    (let ((libdir (sys::lib-directory)))
      (make-pathname
        :host (pathname-host libdir)
        :device (pathname-device libdir)
        :directory #+UNIX (append (pathname-directory libdir) (list "data"))
                   #-UNIX (pathname-directory libdir)
) ) ) )

; Return the line associated with a Unicode code in the Unicode data file.
(defun unicode-attributes-line (code)
  (with-open-file (f (unicode-data-file) :direction :input
                                         :element-type 'character
                                         :external-format 'charset:ascii)
    ;; We know that the file's lines are sorted according to the code, and
    ;; we use this fact to perform a fast binary search.
    (flet ((code-at-pos ()
             ; Returns the code of the first line contained after the current
             ; position.
             (if (read-line f nil nil)
               (let ((c1 (read-char f nil nil))
                     (c2 (read-char f nil nil))
                     (c3 (read-char f nil nil))
                     (c4 (read-char f nil nil)))
                 (if (and c1 c2 c3 c4)
                   (parse-integer (coerce (list c1 c2 c3 c4) 'string)
                                  :radix 16
                   )
                   #x10000
               ) )
               #x10000
          )) )
      (let* ((blocksize 1024)
             (f-size (progn (file-position f :end) (file-position f)))
             (lblock 0) ; lower bound for block number
             (rblock (1+ (floor f-size blocksize))))
        ; The block number where the line starts is >= lblock, < rblock.
        (loop
          (when (= (- rblock lblock) 1) (return))
          (let ((mblock (ash (+ lblock rblock) -1)))
            (file-position f (* mblock blocksize))
            (if (> (code-at-pos) code)
              (setq rblock mblock)
              (setq lblock mblock)
        ) ) )
        ; Sequentially read the block, searching for the first line whose
        ; number is >= code.
        (file-position f (* lblock blocksize))
        (when (plusp code) ; hack to make code=0000 work
          (read-line f nil nil)
        )
        (loop
          (let ((line (read-line f nil nil)))
            (unless line (return nil))
            (let ((c (parse-integer line :end 4 :radix 16)))
              ; Treat the range start/end lines specially.
              (when (eql (char line 4) #\;)
                (when (eql (char line 5) #\<)
                  (let* ((piece1 (subseq line 5
                                              (or (position #\; line :start 5)
                                                  (length line))))
                         (n (length piece1)))
                    (cond ((and (= c code)
                                (>= n 8)
                                (string= piece1 ", First>" :start1 (- n 8))
                           )
                           (return
                             (concatenate 'string
                               (format nil "~4,'0X" code)
                               (subseq line 4 (+ 5 n -8))
                               (subseq line (+ 5 n -1))
                          )) )
                          ((and (>= c code)
                                (>= n 7)
                                (string= piece1 ", Last>" :start1 (- n 7))
                           )
                           (return
                             (concatenate 'string
                               (format nil "~4,'0X" code)
                               (subseq line 4 (+ 5 n -7))
                               (subseq line (+ 5 n -1))
                          )) )
              ) ) ) )
              (if (= c code) (return line))
              (if (> c code) (return nil))
        ) ) )
) ) ) )

;; Return the Unicode attributes of a character, as 14 values,
;; or NIL if the character is not defined in the Unicode standard.
;; The values are:
;;  1. Character name.
;;  2. General category.
;;  3. Canonical combining classes.
;;  4. Bidirectional category.
;;  5. Character decomposition.
;;  6. Decimal digit value.
;;  7. Digit value.
;;  8. Numeric value.
;;  9. mirrored-p.
;; 10. Old Unicode 1.0 name.
;; 11. Comment.
;; 12. Upper case equivalent mapping.
;; 13. Lower case equivalent mapping.
;; 14. Title case equivalent mapping.
(defun unicode-attributes (ch)
  (let ((line (unicode-attributes-line (char-code ch))))
    (if line
      (let ((pieces
              (loop for pos = 0 then (1+ semicolon-pos)
                    for semicolon-pos = (position #\; line :start pos)
                    do collect (subseq line pos (or semicolon-pos (length line)))
                    do (unless semicolon-pos (loop-finish))
           )) )
        (assert (= (parse-integer (nth 0 pieces) :radix 16) (char-code ch)))
        (values-list
          (append
            (mapcar #'(lambda (x) (if (equal x "") nil x))
                    (subseq pieces 1 12)
            )
            (mapcar #'(lambda (x)
                        (if (equal x "") nil (code-char (parse-integer x :radix 16)))
                      )
                    (subseq pieces 12 15)
        ) ) )
      )
      nil
) ) )

) ; #+UNICODE

;;-----------------------------------------------------------------------------

;; DOCUMENTATION mit abfragen und ausgeben??
;; function, variable, type, structure, setf

; Gibt object in einen String aus, der nach Möglichkeit höchstens max Zeichen
; lang sein soll.
(defun write-to-short-string (object max)
  ; Methode: probiere
  ; level = 0: length = 0,1,2
  ; level = 1: length = 1,2,3,4
  ; level = 2: length = 2,...,6
  ; usw. bis maximal level = 16.
  ; Dabei level möglichst groß, und bei festem level length möglichst groß.
  (if (or (numberp object) (symbolp object)) ; von length und level unbeeinflusst?
    (write-to-string object)
    (macrolet ((minlength (level) `,level)
               (maxlength (level) `(* 2 (+ ,level 1))))
      ; Um level möglist groß zu bekommen, dabei length = minlength wählen.
      (let* ((level ; Binärsuche nach dem richtigen level
               (let ((level1 0) (level2 16))
                 (loop
                   (when (= (- level2 level1) 1) (return))
                   (let ((levelm (floor (+ level1 level2) 2)))
                     (if (<= (length (write-to-string object :level levelm :length (minlength levelm))) max)
                       (setq level1 levelm) ; levelm passt, probiere größere
                       (setq level2 levelm) ; levelm passt nicht, probiere kleinere
                 ) ) )
                 level1
             ) )
             (length ; Binärsuche nach dem richtigen length
               (let ((length1 (minlength level)) (length2 (maxlength level)))
                 (loop
                   (when (= (- length2 length1) 1) (return))
                   (let ((lengthm (floor (+ length1 length2) 2)))
                     (if (<= (length (write-to-string object :level level :length lengthm)) max)
                       (setq length1 lengthm) ; lengthm passt, probiere größere
                       (setq length2 lengthm) ; lengthm passt nicht, probiere kleinere
                 ) ) )
                 length1
            )) )
        (write-to-string object :level level :length length)
) ) ) )
