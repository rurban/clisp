#
# This is the "link.sh" for the Oracle CLISP module
#
# $Id$

make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"

NEW_FILES="oracle.o oiface.o orafns.o"

# Get additional libs for Oracle client.  This may be
# system specific and require some tweaking.
NEW_LIBS="oracle.o oiface.o orafns.o -L ${ORACLE_HOME}/lib -lclntsh -ldl -lpthread -lm"

NEW_MODULES="oracle"
TO_LOAD="oracle"

