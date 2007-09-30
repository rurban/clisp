" Vim syntax file
" Language: Lisp (with extensions for CLISP)

" FFI
syn keyword lispDecl def-c-var def-c-const def-c-enum def-c-type
syn keyword lispDecl def-call-out def-call-in
syn keyword lispDecl c-function c-struct c-pointer c-ptr c-array c-array-ptr c-string
syn keyword lispFunc with-c-place with-c-var
syn keyword lispFunc with-foreign-object with-foreign-string
syn keyword lispFunc default-foreign-language default-foreign-library
syn keyword lispFunc element deref cast slot validp set-foreign-pointer
syn keyword lispFunc allocate-deep allocate-shallow foreign-free
syn keyword lispFunc int
syn keyword lispKey :arguments :return-type :library :full

" loop
syn keyword lispKey :while :until :for :do :if :then :else :when :unless :in
syn keyword lispKey :across :finally :collect :nconc :maximize :minimize :sum

" Macros et al
syn keyword lispFunc with-collect letf letf*
syn keyword lispFunc finalize

" general
syn match lispSpecial "\+[a-zA-Z_][a-zA-Z_0-9-]*\+"
