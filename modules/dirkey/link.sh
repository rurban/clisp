files='dirkey.o'
make clisp-module CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="${files}"
NEW_LIBS="${files} wldap32.lib"
NEW_MODULES='dirkey'
TO_LOAD='dirkey1'
TO_PRELOAD="preload.lisp"
