" Vim syntax file
" Language: Lisp (with extensions for CLISP)

" FFI
syn keyword lispDecl def-c-var def-c-const def-c-enum def-c-type def-c-struct
syn keyword lispDecl def-call-out def-call-in
syn keyword lispDecl c-function c-struct c-pointer c-ptr c-ptr-null
syn keyword lispDecl c-array c-array-ptr c-array-max c-string
syn keyword lispDecl char short int long uchar ushort uint ulong
syn keyword lispDecl size_t off_t time_t handle
syn match lispDecl "d\?word"
syn match lispDecl "\([us]_\?\)\?int\(8\|16\|32\|64\)\(_t\)\?"
syn keyword lispFunc with-c-place with-c-var
syn keyword lispFunc with-foreign-object with-foreign-string
syn keyword lispFunc default-foreign-language default-foreign-library
syn keyword lispFunc element deref cast slot validp set-foreign-pointer
syn keyword lispFunc allocate-deep allocate-shallow foreign-free
syn keyword lispFunc c-lines foreign-value
syn keyword lispKey :arguments :return-type :library :full
syn keyword lispKey :none :alloca :in :out :in-out

" loop
syn keyword lispKey :while :until :for :do :if :then :else :when :unless :in
syn keyword lispKey :across :finally :collect :nconc :maximize :minimize :sum

" Macros et al
syn keyword lispFunc with-collect letf letf*
syn keyword lispFunc finalize

" general
syn match lispSpecial "+[a-zA-Z_][a-zA-Z_0-9-]*+"
syn keyword lispKey :documentation
