#|
(in-package "FFI")
(defun ffi::lookup-foreign-function (name description)
  (let ((args (list name description)))
    (format *trace-output* "~&Calling Lookup with ~S.~%" args)
    #'(lambda (&rest rest)
        (format *trace-output* "~&Calling Looked-up(~S) with ~S.~%" args rest)
        nil)))
(defconstant ffi::ff-flag-alloca (ash 1 0))
(defconstant ffi::ff-language-c  (ash 1 9))
(defconstant ffi::ff-flag-out    (ash 1 4))
(def-call-out OpenLibrary
  (:name "OpenLibrary")
  (:language :c)                        ;every integral converted to (U)LONG
  ;;(:library "exec.library")
  (:arguments
   (libname c-string :in :alloca :a0)
   (version uint32   :in :none   :d0))
  (:return-type c-pointer :none  :d0))
(def-call-out ReadDOS
  (:name "Read")
  (:language :c)
  (:arguments
   (file c-pointer   :in :none   :d0)
   (buffer c-string  :out :none :d1)    ;inexpressible
   (length sint32    :in :none   :d2))
  (:return-type sint32 :none :d0))
|#

(in-package "AFFI")
#|
(macroexpand-1 '
(def-affi-call-out OpenLibrary
  (:name "OpenLibrary")
  (:language :c)                        ;every integral converted to (U)LONG
  (:library "exec.library") (:offset -552)
  (:arguments
   (libname :c-string :in :alloca :a1)
   (version :ulong     :in :none :d0))
  (:return-type :c-pointer :none :d0))
)
(defflibfun 'OPENLIBRARY 'SYSBASE -552 #x1A '* 'string 4)
;; Read        :io     c-pointer :none   :out
;; OpenLibrary string  c-string  :alloca :in
;; Write       :io     c-pointer :none   :in    ; :io to avoid copying
;; OpenWindow  *       c-pointer :none   :in    ; disable string arg?
|#

(defun error-bad-spec (whole &optional reason)
  (error "Bad FFI specification: ~S" whole))

(defun parse-ffi-type (argspec whole)
  (unless (and (consp argspec)
               (symbolp (first argspec)))
    (error-bad-spec whole))
  (or
   (case (second argspec)               ; c-type
     (:boolean  0)                      ; not implemented in AFFI.D as argument
     (:sint8   -1)                      ;add :char ?
     ((:uint8 :uchar)   1)
     ((:sint16 :short) -2)
     ((:uint16 :ushort) 2)
     ((:sint32 :long)  -4)
     ((:uint32 :ulong)  4)
     ((:c-pointer :c-string)            ;TODO revisit (dependency on allocation?)
      (if (eq :return-type (first argspec))
          (if (eq (second argspec) :c-string) 'string '*)
          (case (third argspec)         ; param-mode
            (:none          '*)
            (:in            (if (eq (second argspec) :c-string) 'string '*))
            ((:out :in-out) :io))))
     ((:io string *) (second argspec))) ; special pass-through
   (error-bad-spec whole)))

(defun parse-options (options keywords whole)
  (reverse options))

(defun calc-reg-mask (regs mask)
  (if (null regs) mask
      (calc-reg-mask
       (rest regs)
       (logior
        (ash mask 4)
        (1+ (position
             (first regs)
             '(:D0 :D1 :D2 :D3 :D4 :D5 :D6 :D7
               :A0 :A1 :A2 :A3 :A4 :A5 :A6)
             :test #'eq))))))

(defun calc-register-mask (regs)
  (labels
      ((calc (regs accu)
         (if (null regs) accu
             (calc (rest regs)
                   (logior
                    (ash accu 4)
                    (1+ (let ((reg (first regs))
                              (list '(:D0 :D1 :D2 :D3 :D4 :D5 :D6 :D7
                                      :A0 :A1 :A2 :A3 :A4 :A5 :A6)))
                          (if (keywordp reg)
                              (position reg list :test #'eq)
                              (position (symbol-name reg) list :test #'string-equal :key #'symbol-name)))))))))
    (calc (reverse regs) 0)))

(defmacro def-affi-call-out (&whole whole name &rest options)
  (let* ((alist (parse-options options '(:name :library :offset :language :arguments :return-type) whole))
         (library (first (check-library-name (second (assoc :library alist)))))
         (offset (second (assoc :offset alist)))
         (mask (calc-reg-mask (reverse (mapcar #'fifth (rest (assoc :arguments alist)))) 0))
         (rtype (parse-ffi-type (assoc :return-type alist) whole))
         (c-args (mapcar #'(lambda (spec) (list 'quote (parse-ffi-type spec whole)))
                         (rest (assoc :arguments alist))))
         )
    ;; declaim special?
    `(defflibfun ',name ',library ,offset ,mask ',rtype ,@c-args)))

