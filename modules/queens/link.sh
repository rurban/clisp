make `if test -n "$with_wide"; then echo clisp-module-wide; else echo clisp-module; fi` INCLUDES='-I'"$absolute_linkkitdir"
NEW_FILES='callqueens.o queens.o'
NEW_LIBS='callqueens.o queens.o'
NEW_WLIBS='wcallqueens.o queens.o'
if test -n "$with_wide"; then
  NEW_FILES=$NEW_FILES' wcallqueens.o'
fi
NEW_MODULES='queens'
TO_LOAD=''
