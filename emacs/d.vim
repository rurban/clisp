" Vim syntax file
" Language: C (CLISP *.d files)

" Read the C syntax to start with
runtime! syntax/c.vim
unlet b:current_syntax

syn keyword dType local global maygc var inline object
syn match dType "[us]int[LW]"
syn keyword dOperator popSTACK pushSTACK skipSTACK skipSTACKop STACKop loop
syn match dOperator "dotimesp?[CLW]"
syn keyword dObject true false NIL T nullobj
syn keyword dOperator nonreturning_function return_Values LISPFUN DEFUN
syn keyword dOperator SstringDispatch SstringCase
syntax region dCommentL start="# " skip="\\$" end="$" keepend contains=@cCommentGroup,cComment2String,cCharacter,cNumbersCom,cSpaceError,@Spell

hi def link dType Type
hi def link dOperator Operator
hi def link dCommentL Comment

let b:current_syntax = "d"
