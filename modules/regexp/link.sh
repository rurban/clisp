file_list=''
wfile_list=''
mod_list=''
if test -r regexp.c; then
  file_list="$file_list"' regexp.o'
  wfile_list="$wfile_list"' wregexp.o'
  mod_list="$mod_list"' regexp'
fi
make `if test -n "$with_wide"; then echo clisp-module-wide; else echo clisp-module; fi` CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list regexi.o regex.o regexp.dvi"
NEW_LIBS="$file_list regexi.o regex.o"
NEW_WLIBS="$wfile_list regexi.o regex.o"
if test -n "$with_wide"; then
  NEW_FILES=$NEW_FILES" $wfile_list"
fi
NEW_MODULES="$mod_list"
TO_LOAD='regexp'
