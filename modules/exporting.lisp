;; make some macros export the symbols they define
;; also substitute some commonly used CL & FFI symbols

(in-package "CL-USER")

(defmacro make-exporting (mod-pack &rest requested-symbols)
  `(progn
     (defconstant substitution
       (mapcar (lambda (symbol)
                 (cons (intern (string-downcase (symbol-name symbol))
                               ,mod-pack)
                       symbol))
               ',requested-symbols))
    (macrolet ((exporting (defining-macro-name)
                  (let ((original-macro-name
                         (intern (string-upcase defining-macro-name) "FFI"))
                        (new-macro-name
                         (intern defining-macro-name ,mod-pack)))
                    `(defmacro ,new-macro-name (name &rest more)
                       `(progn
                          (export ',name)
                          (,',original-macro-name
                           ,name ,@(sublis substitution more))))))
                (exporting-slots (defining-macro-name)
                  (let ((original-macro-name
                         (intern (string-upcase defining-macro-name) "FFI"))
                        (new-macro-name
                         (intern defining-macro-name ,mod-pack)))
                    `(defmacro ,new-macro-name (name &rest more)
                       (let ((sname (if (consp name) (car name) name)))
                         `(progn
                            (export '(,sname
                                      ,@(let ((cname (sys::string-concat
                                                      (symbol-name sname)
                                                      "-")))
                                             (list*
                                              (sys::concat-pnames
                                               "COPY-" cname)
                                              (sys::concat-pnames
                                               "MAKE-" cname)
                                              (sys::concat-pnames cname "-P")
                                              (mapcar (lambda (slot)
                                                        (sys::concat-pnames
                                                         cname (car slot)))
                                                      more)))))
                            (,',original-macro-name
                             ,name ,@(sublis substitution more)))))))
                (exporting-enums (defining-macro-name)
                  (let ((original-macro-name
                         (intern (string-upcase defining-macro-name) "FFI"))
                        (new-macro-name
                         (intern defining-macro-name ,mod-pack)))
                    `(defmacro ,new-macro-name (name &rest more)
                       `(progn
                          (export '(,name
                                    ,@(mapcar (lambda (slot)
                                                (if (consp slot)
                                                    (car slot) slot))
                                              more)))
                          (,',original-macro-name
                           ,name ,@(sublis substitution more))))))
                (normal (defining-macro-name)
                  (let ((original-macro-name
                         (intern (string-upcase defining-macro-name) "FFI"))
                        (new-macro-name
                         (intern defining-macro-name ,mod-pack )))
                    `(defmacro ,new-macro-name (&rest more)
                       `(,',original-macro-name
                         ,@(sublis substitution more))))))
       (exporting "defconstant")
       (exporting "defun")
       (exporting "defmacro")
       (exporting "define-modify-macro")
       (exporting "define-symbol-macro")
       (exporting "defsetf")
       (exporting "def-c-type")
       (exporting-enums "def-c-enum")
       (exporting-slots "def-c-struct")
       (exporting "def-c-var")
       (exporting "def-call-out")
       (normal "c-lines")
       (normal "eval-when"))))
