file_list=''
mod_list=''
file_list="$file_list"' clx.o'
mod_list="$mod_list"' clx'
make clisp-module CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list"
NEW_LIBS="$file_list -L/usr/X11R6/lib -lXpm -lXext -lX11"
NEW_MODULES="$mod_list"
NEW_FILES="$NEW_FILES clx-preload.lsp clx.lsp clx.fas image.lsp image.fas"
TO_PRELOAD='clx-preload.lsp'
TO_LOAD='clx image'
