" Vim syntax file
" Language: C (CLISP *.d files)

" Read the C syntax to start with
runtime! syntax/c.vim
unlet b:current_syntax

syn keyword dType local global maygc var inline object
syn match dType "[us]int[LW]"
syn match dType "[a-zA-Z0-9_]*_t"
syn keyword dOperator popSTACK pushSTACK skipSTACK skipSTACKop STACKop loop
syn match dOperator "dotimesp?[CLW]"
syn keyword dObject true false NIL T nullobj
syn keyword dOperator nonreturning_function return_Values DEFUN O S
syn match dOperator "LISPFUN[NR]*"
syn match dOperator "VALUES[0-9]"
syn keyword dOperator SstringDispatch SstringCase
syn region dCommentL start="# " skip="\\$" end="$" keepend contains=@cCommentGroup,cComment2String,cCharacter,cNumbersCom,cSpaceError,@Spell

hi def link dType Type
hi def link dOperator Operator
hi def link dCommentL Comment
hi def link dObject Boolean

let b:current_syntax = "d"
