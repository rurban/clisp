file_list=''
mod_list=''
if test -f postgresql.c; then
  file_list="$file_list"' postgresql.o'
  mod_list="$mod_list"' postgresql'
fi
make clisp-module CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"

# Test whether libcrypt exists.
have_libcrypt=''
echo 'int main() { return 0; }' > linkdummy.c
if "${CC}" "${CFLAGS}" linkdummy.c -lcrypt -o linkdummy > /dev/null 2>/dev/null; then
  have_libcrypt=yes
fi
rm -f linkdummy*

NEW_FILES="$file_list"
NEW_LIBS="$file_list -lpq"
if test -n "$have_libcrypt"; then
  NEW_LIBS="$NEW_LIBS -lcrypt"
fi
NEW_MODULES="$mod_list"
TO_LOAD='postgresql'
