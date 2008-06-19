file_list=''
mod_list='netica'
if test -f netica.c; then
  file_list="$file_list"' netica.o'
fi
netica=${NETICA_C_API-/usr/local/netica}
${MAKE-make} clisp-module \
  NETICA=${netica}/src \
  CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
  INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list"
if [ -r ${netica}/lib/libnetica.a ]; then
  NEW_LIBS="$file_list ${netica}/lib/libnetica.a"
elif [ -r ${netica}/lib/netica.lib ]; then
  NEW_LIBS="$file_list ${netica}/lib/netica.lib"
else
  echo "$0: no netica library in ${netica}/lib" >&2
  exit 1
fi
NEW_MODULES="$mod_list"
TO_LOAD='netica wrap'
CLFLAGS="${CLFLAGS} -L${netica}/lib"
