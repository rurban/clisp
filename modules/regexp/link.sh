file_list=''
mod_list=''
if test -r regexp.c; then
  file_list="$file_list"' regexp.o'
  mod_list="$mod_list"' regexp'
fi
make clisp-module CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list regexi.o regex.o regexp.dvi"
NEW_LIBS="$file_list regexi.o regex.o"
NEW_MODULES="$mod_list"
TO_LOAD='regexp'
