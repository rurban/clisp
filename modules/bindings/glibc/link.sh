${MAKE-make} clisp-module \
  CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
  INCLUDES="$absolute_linkkitdir"
NEW_FILES="linux.o"
NEW_LIBS="${NEW_FILES} -lm"
NEW_MODULES="linux"
TO_LOAD='linux wrap'
