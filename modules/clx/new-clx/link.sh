file_list=''
wfile_list=''
mod_list=''
file_list="$file_list"' clx.o'
wfile_list="$wfile_list"' wclx.o'
mod_list="$mod_list"' clx'
make `if test -n "$with_wide"; then echo clisp-module-wide; else echo clisp-module; fi` CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list"
if test -n "$with_wide"; then
  NEW_FILES=$NEW_FILES" $wfile_list"
fi
# Normally, the "-L... -lX11" library ought to be part of NEW_LIBS and NEW_WLIBS
# but we don't need it here because lisp.run itself is normally already linked
# with this library.
NEW_LIBS="$file_list -lXpm -lXext"
NEW_WLIBS="$wfile_list -lXpm -lXext"
NEW_MODULES="$mod_list"
NEW_FILES="$NEW_FILES clx-preload.lsp clx.lsp clx.fas image.lsp image.fas"
TO_PRELOAD='clx-preload.lsp'
TO_LOAD='clx image'
