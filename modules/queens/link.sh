${MAKE-make} clisp-module \
  CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
  INCLUDES="$absolute_linkkitdir"
NEW_FILES='callqueens.o queens.o'
NEW_LIBS='callqueens.o queens.o'
NEW_MODULES='queens'
TO_LOAD=''
