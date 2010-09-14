${MAKE-make} clisp-module \
  CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}"
  CLISP_LINKKIT="$absolute_linkkitdir" CLISP="${CLISP}"
NEW_FILES=''
NEW_LIBS=''
NEW_MODULES='clx'
TO_LOAD="package depdefs clx dependent macros bufmac buffer display gcontext input requests fonts graphics text attributes translate keysyms manager image resource describe trace shape"
for f in $TO_LOAD; do
  NEW_FILES="$NEW_FILES $f.lisp $f.fas"
done
