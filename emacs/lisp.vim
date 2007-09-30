" Vim syntax file
" Language: Lisp (with extensions for CLISP)

" FFI
syn keyword lispFunc def-c-var def-c-const def-c-enum def-c-type
syn keyword lispFunc def-call-out def-call-in
syn keyword lispFunc c-function 

" Macros
syn keyword lispFunc with-collect letf letf*
