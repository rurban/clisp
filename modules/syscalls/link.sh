files='calls.o'
make clisp-module CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="${files}"
NEW_LIBS="${files} -lm"
NEW_MODULES='syscalls'
TO_LOAD='posix'
TO_PRELOAD="preload.lisp"
