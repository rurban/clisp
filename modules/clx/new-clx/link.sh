file_list=''
mod_list=''
file_list="$file_list"' clx.o'
mod_list="$mod_list"' clx'
make clisp-module CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list"
# Normally, the "-L... -lX11" library ought to be part of NEW_LIBS
# but we don't need it here because lisp.run itself is normally already linked
# with this library.
NEW_LIBS="$file_list -lXpm -lXext"
NEW_MODULES="$mod_list"
NEW_FILES="$NEW_FILES clx-preload.lsp clx.lsp clx.fas image.lsp image.fas"
TO_PRELOAD='clx-preload.lsp'
TO_LOAD='clx image'
