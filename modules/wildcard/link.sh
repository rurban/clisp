file_list=''
wfile_list=''
mod_list=''
if test -r wildcard.c; then
  file_list="$file_list"' wildcard.o'
  wfile_list="$wfile_list"' wwildcard.o'
  mod_list="$mod_list"' wildcard'
fi
make `if test -n "$with_wide"; then echo clisp-module-wide; else echo clisp-module; fi` CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list fnmatch.o wildcard.dvi"
NEW_LIBS="$file_list fnmatch.o"
NEW_WLIBS="$wfile_list fnmatch.o"
if test -n "$with_wide"; then
  NEW_FILES=$NEW_FILES" $wfile_list"
fi
NEW_MODULES="$mod_list"
TO_LOAD='wildcard'
