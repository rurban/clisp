file_list=''
wfile_list=''
mod_list=''
if test -f linux.c; then
  file_list="$file_list"' linux.o'
  wfile_list="$wfile_list"' wlinux.o'
  mod_list="$mod_list"' linux'
fi
make `if test -n "$with_wide"; then echo clisp-module-wide; else echo clisp-module; fi` CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list"
NEW_LIBS="$file_list -lm"
NEW_WLIBS="$wfile_list -lm"
if test -n "$with_wide"; then
  NEW_FILES=$NEW_FILES" $wfile_list"
fi
NEW_MODULES="$mod_list"
TO_LOAD='linux'
