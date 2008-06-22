file_list=''
if test -f netica.c; then
  file_list="$file_list"' netica.o'
fi
${MAKE-make} clisp-module \
  CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
  INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list"
# netica 3.25 requires g++
NEW_LIBS="$file_list -lnetica -L${NETICA_C_API}/lib -lstdc++"
NEW_MODULES="netica"
TO_LOAD='netica wrap'
CLFLAGS="${CLFLAGS}"
