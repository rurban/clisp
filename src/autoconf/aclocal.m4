dnl local autoconf macros
dnl Bruno Haible 21.9.1997
dnl Marcus Daniels 10.4.1997
dnl
AC_PREREQ(2.12)dnl
dnl
dnl without AC_MSG_...:   with AC_MSG_... and caching:
dnl   AC_TRY_CPP          CL_CPP_CHECK
dnl   AC_TRY_COMPILE      CL_COMPILE_CHECK
dnl   AC_TRY_LINK         CL_LINK_CHECK
dnl   AC_TRY_RUN          CL_RUN_CHECK - would require cross-compiling support
dnl Usage:
dnl AC_TRY_CPP(INCLUDES,
dnl            ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND])
dnl CL_CPP_CHECK(ECHO-TEXT, CACHE-ID,
dnl              INCLUDES,
dnl              ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND])
dnl AC_TRY_xxx(INCLUDES, FUNCTION-BODY,
dnl            ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND])
dnl CL_xxx_CHECK(ECHO-TEXT, CACHE-ID,
dnl              INCLUDES, FUNCTION-BODY,
dnl              ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND])
dnl
define(CL_CPP_CHECK,
[AC_MSG_CHECKING(for $1)
AC_CACHE_VAL($2,[
AC_TRY_CPP([$3], $2=yes, $2=no)
])
AC_MSG_RESULT([$]$2)
if test [$]$2 = yes; then
  ifelse([$4], , :, [$4])
ifelse([$5], , , [else
  $5
])dnl
fi
])dnl
dnl
define(CL_COMPILE_CHECK,
[AC_MSG_CHECKING(for $1)
AC_CACHE_VAL($2,[
AC_TRY_COMPILE([$3],[$4], $2=yes, $2=no)
])
AC_MSG_RESULT([$]$2)
if test [$]$2 = yes; then
  ifelse([$5], , :, [$5])
ifelse([$6], , , [else
  $6
])dnl
fi
])dnl
dnl
define(CL_LINK_CHECK,
[AC_MSG_CHECKING(for $1)
AC_CACHE_VAL($2,[
AC_TRY_LINK([$3],[$4], $2=yes, $2=no)
])
AC_MSG_RESULT([$]$2)
if test [$]$2 = yes; then
  ifelse([$5], , :, [$5])
ifelse([$6], , , [else
  $6
])dnl
fi
])dnl
dnl
dnl CL_PROTO(IDENTIFIER, ACTION-IF-NOT-FOUND, FINAL-PROTOTYPE)
define(CL_PROTO,
[AC_MSG_CHECKING([for $1 declaration])
AC_CACHE_VAL(cl_cv_proto_[$1], [$2
cl_cv_proto_$1="$3"])
cl_cv_proto_$1=`echo "[$]cl_cv_proto_$1" | tr -s ' ' | sed -e 's/( /(/'`
AC_MSG_RESULTPROTO([$]cl_cv_proto_$1)
])dnl
dnl
dnl CL_PROTO_RET(INCLUDES, DECL, CACHE-ID, TYPE-IF-OK, TYPE-IF-FAILS)
define(CL_PROTO_RET,
[AC_TRY_COMPILE([$1]
AC_LANG_EXTERN[$2
], [], $3="$4", $3="$5")
])dnl
dnl
dnl CL_PROTO_TRY(INCLUDES, ANSI-DECL, TRAD-DECL, ACTION-IF-OK, ACTION-IF-FAILS)
define(CL_PROTO_TRY,
[AC_TRY_COMPILE([$1]
AC_LANG_EXTERN
[#if defined(__STDC__) || defined(__cplusplus)
$2
#else
$3
#endif
], [], [$4], [$5])
])dnl
dnl
dnl CL_PROTO_CONST(INCLUDES, ANSI-DECL, TRAD-DECL, CACHE-ID)
define(CL_PROTO_CONST,
[CL_PROTO_TRY([$1], [$2], [$3], $4="", $4="const")]
)dnl
dnl
dnl CL_SILENT(ACTION)
dnl performs ACTION, with AC_MSG_CHECKING and AC_MSG_RESULT being defined away.
define(CL_SILENT,
[pushdef([AC_MSG_CHECKING],[:])dnl
pushdef([AC_CHECKING],[:])dnl
pushdef([AC_MSG_RESULT],[:])dnl
pushdef([AC_MSG_RESULTPROTO],[:])dnl
$1[]dnl
popdef([AC_MSG_RESULTPROTO])dnl
popdef([AC_MSG_RESULT])dnl
popdef([AC_CHECKING])dnl
popdef([AC_MSG_CHECKING])dnl
])dnl
dnl
AC_DEFUN(CL_CC_GCC,
[AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK(whether using GNU C, cl_cv_prog_cc_gcc, [
AC_EGREP_CPP(yes,[#ifdef __GNUC__
  yes
#endif
], cl_cv_prog_cc_gcc=yes, cl_cv_prog_cc_gcc=no)
])
if test $cl_cv_prog_cc_gcc = yes; then
  CC_GCC=true
  GCC_X_NONE='-x none'
else
  CC_GCC=false
  GCC_X_NONE=''
fi
AC_SUBST(CC_GCC)dnl
AC_SUBST(GCC_X_NONE)dnl
])dnl
dnl
AC_DEFUN(CL_CC_CPLUSPLUS,
[AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK(whether using C++, cl_cv_prog_cc_cplusplus, [
AC_EGREP_CPP(yes,[#ifdef __cplusplus
  yes
#endif
], cl_cv_prog_cc_cplusplus=yes, cl_cv_prog_cc_cplusplus=no)
])
if test $cl_cv_prog_cc_cplusplus = yes; then
  CC_CPLUSPLUS=true
else
  CC_CPLUSPLUS=false
fi
AC_SUBST(CC_CPLUSPLUS)dnl
])dnl
dnl
AC_DEFUN(CL_CC_ANSI,
[AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK(whether using ANSI C, cl_cv_prog_cc_ansi, [
AC_EGREP_CPP(yes,[#if defined(__STDC__) || defined(__cplusplus)
  yes
#endif
], cl_cv_prog_cc_ansi=yes, cl_cv_prog_cc_ansi=no)
])
if test $cl_cv_prog_cc_ansi = yes; then
  CC_ANSI=true
else
  CC_ANSI=false
fi
AC_SUBST(CC_ANSI)dnl
])dnl
dnl
AC_DEFUN(CL_CC_NEED_CCPAUX,
[AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK(whether CPP likes indented directives, cl_cv_prog_cc_indented, [
AC_EGREP_CPP([#.*foo],[ #define foo],
cl_cv_prog_cc_indented=no, cl_cv_prog_cc_indented=yes)
])
if test $cl_cv_prog_cc_indented = yes; then
  CC_NEED_CCPAUX=false
else
  CC_NEED_CCPAUX=true
fi
AC_SUBST(CC_NEED_CCPAUX)dnl
])dnl
dnl
AC_DEFUN(CL_CC_NEED_DEELIF,
[AC_REQUIRE([AC_PROG_CPP])
dnl Bug in autoconf-2.1: If we put the # literally there, AC_FD_MSG doesn't get expanded.
sharp='#elif'
AC_CACHE_CHECK([whether CPP understands $sharp], cl_cv_prog_cc_elif, [
AC_TRY_CPP([#if 0
#elif 1
#else
#endif],
cl_cv_prog_cc_elif=yes, cl_cv_prog_cc_elif=no)
])
if test $cl_cv_prog_cc_elif = yes; then
  CC_NEED_DEELIF=false
else
  CC_NEED_DEELIF=true
fi
AC_SUBST(CC_NEED_DEELIF)dnl
])dnl
dnl
AC_DEFUN(CL_CC_NEED_DEERROR,
[AC_REQUIRE([AC_PROG_CPP])
dnl Bug in autoconf-2.1: If we put the # literally there, AC_FD_MSG doesn't get expanded.
sharp='#error'
AC_CACHE_CHECK([whether CPP understands $sharp], cl_cv_prog_cc_error, [
AC_TRY_CPP([#if 0
#error "bla"
#endif],
cl_cv_prog_cc_error=yes, cl_cv_prog_cc_error=no)
])
if test $cl_cv_prog_cc_error = yes; then
  CC_NEED_DEERROR=false
else
  CC_NEED_DEERROR=true
fi
AC_SUBST(CC_NEED_DEERROR)dnl
])dnl
dnl
AC_DEFUN(CL_CC_NEED_DEEMA,
[AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK(whether CPP likes empty macro arguments, cl_cv_prog_cc_ema, [
AC_TRY_CPP([#define divide(x,y,q_zuw,r_zuw) (r_zuw(x)-(q_zuw(x)/(y))*(y))
foo(x,y) int x,y; { int q; divide(x,y,q=,); return q; }],
cl_cv_prog_cc_ema=yes, cl_cv_prog_cc_ema=no)
])
if test $cl_cv_prog_cc_ema = yes; then
  CC_NEED_DEEMA=false
else
  CC_NEED_DEEMA=true
fi
AC_SUBST(CC_NEED_DEEMA)dnl
])dnl
dnl
AC_DEFUN(CL_CC_NEED_MERGESTRINGS,
[AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK(whether CC merges adjacent strings, cl_cv_prog_cc_mergestrings, [
AC_TRY_COMPILE([], [char* foo = "abc" "def";],
cl_cv_prog_cc_mergestrings=yes, cl_cv_prog_cc_mergestrings=no)
rm -f conftest*
])
if test $cl_cv_prog_cc_mergestrings = yes; then
  CC_NEED_MERGESTRINGS=false
else
  CC_NEED_MERGESTRINGS=true
fi
AC_SUBST(CC_NEED_MERGESTRINGS)dnl
])dnl
dnl
AC_DEFUN(CL_AS_UNDERSCORE,
[AC_BEFORE([$0], [CL_GLOBAL_CONSTRUCTORS])
AC_CACHE_CHECK(for underscore in external names, cl_cv_prog_as_underscore, [
cat > conftest.c <<EOF
int foo() { return 0; }
EOF
# look for the assembly language name in the .s file
AC_TRY_COMMAND(${CC-cc} -S conftest.c) >/dev/null 2>&1
if grep _foo conftest.s >/dev/null ; then
  cl_cv_prog_as_underscore=yes
else
  cl_cv_prog_as_underscore=no
fi
rm -f conftest*
])
if test $cl_cv_prog_as_underscore = yes; then
  AS_UNDERSCORE=true
  AC_DEFINE(ASM_UNDERSCORE)
else
  AS_UNDERSCORE=false
fi
AC_SUBST(AS_UNDERSCORE)dnl
])dnl
dnl
AC_DEFUN(CL_PROG_RANLIB, [AC_CHECK_PROG(RANLIB, ranlib, ranlib, true)])dnl
dnl
AC_DEFUN(CL_PROG_INSTALL,
[dnl This is mostly copied from AC_PROG_INSTALL.
# Find a good install program.  We prefer a C program (faster),
# so one script is as good as another.  But avoid the broken or
# incompatible versions:
# SysV /etc/install, /usr/sbin/install
# SunOS /usr/etc/install
# IRIX /sbin/install
# AIX /bin/install
# AFS /usr/afsws/bin/install, which mishandles nonexistent args
# SVR4 /usr/ucb/install, which tries to use the nonexistent group "staff"
# ./install, which can be erroneously created by make from ./install.sh.
AC_MSG_CHECKING(for a BSD compatible install)
if test -z "$INSTALL"; then
AC_CACHE_VAL(cl_cv_path_install,
[  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
  for ac_dir in $PATH; do
    # Account for people who put trailing slashes in PATH elements.
    case "$ac_dir/" in
    /|./|.//|/etc/*|/usr/sbin/*|/usr/etc/*|/sbin/*|/usr/afsws/bin/*|/usr/ucb/*) ;;
    *)
      # OSF1 and SCO ODT 3.0 have their own names for install.
      for ac_prog in ginstall installbsd scoinst install; do
        if test -f $ac_dir/$ac_prog; then
	  if test $ac_prog = install &&
            grep dspmsg $ac_dir/$ac_prog >/dev/null 2>&1; then
	    # AIX install.  It has an incompatible calling convention.
	    # OSF/1 installbsd also uses dspmsg, but is usable.
	    :
	  else
	    if test $ac_prog = installbsd &&
	      grep src/bos $ac_dir/$ac_prog >/dev/null 2>&1; then
	      # AIX installbsd doesn't work without option "-g".
	      :
	    else
	      ac_cv_path_install="$ac_dir/$ac_prog -c"
	      break 2
	    fi
	  fi
	fi
      done
      ;;
    esac
  done
  IFS="$ac_save_ifs"
  # As a last resort, use cp.
  test -z "$cl_cv_path_install" && cl_cv_path_install="cp"
])dnl
  INSTALL="$cl_cv_path_install"
fi
dnl We do special magic for INSTALL instead of AC_SUBST, to get
dnl relative paths right. 
AC_MSG_RESULT($INSTALL)
AC_SUBST(INSTALL)dnl
# Use test -z because SunOS4 sh mishandles braces in ${var-val}.
# It thinks the first close brace ends the variable substitution.
test -z "$INSTALL_PROGRAM" && INSTALL_PROGRAM='$(INSTALL)'
AC_SUBST(INSTALL_PROGRAM)dnl
if test -z "$INSTALL_DATA"; then
  case "$INSTALL" in
    cp | */cp ) INSTALL_DATA='$(INSTALL)' ;;
    * )         INSTALL_DATA='$(INSTALL) -m 644' ;;
  esac
fi
AC_SUBST(INSTALL_DATA)dnl
])dnl
dnl
AC_DEFUN(CL_CP,
[AC_CACHE_CHECK(how to copy files, cl_cv_prog_cp, [
echo "blabla" > conftest.x
err=`/bin/sh -c "cp -p conftest.x conftest.y 2>&1"`
if test -z "$err"; then
  cl_cv_prog_cp='cp -p'
else
  cl_cv_prog_cp='cp'
fi
rm -f conftest*
])
CP="$cl_cv_prog_cp"
AC_SUBST(CP)dnl
])dnl
dnl
AC_DEFUN(CL_PROG_LN,
[AC_REQUIRE([AC_PROG_LN_S])dnl
dnl SVR4 "ln" makes hard links to symbolic links, instead of resolving the
dnl symbolic link. To avoid this, use the "hln" program.
AC_CACHE_CHECK(how to make hard links, cl_cv_prog_ln, [
cl_cv_prog_ln="ln"
if test "$ac_cv_prog_LN_S" = "ln -s"; then
echo "blabla" > conftest.x
ln -s conftest.x conftest.y
ln conftest.y conftest.z
rm -f conftest.x
if cat conftest.z > /dev/null 2>&1 ; then
  # ln is usable.
  cl_cv_prog_ln="ln"
else
  # conftest.z is a symbolic link to the non-existent conftest.x
  cl_cv_prog_ln="hln"
fi
else
# If there are no symbolic links, the problem cannot occur.
cl_cv_prog_ln="ln"
fi
rm -f conftest*
])
LN="$cl_cv_prog_ln"
AC_SUBST(LN)dnl
])dnl
dnl
AC_DEFUN(CL_FIND_X,
[AC_CHECKING(for X11)
X_INCLUDES=''
X_LIBS=''
dnl First check for xmkmf.
AC_CHECK_PROG(HAVE_XMKMF, xmkmf, yes)dnl
if test "$HAVE_XMKMF" = yes; then
  have_x11=1
  AC_DEFINE(HAVE_X11)
  X_LIBS='-lX11'
  AC_PATH_X()dnl
  if test -n "$x_includes" -a "$x_includes" != NONE; then
    # Adding -I/usr/include may override gcc's private include hierarchy.
    if test "$x_includes" != "/usr/include"; then
      X_INCLUDES="-I$x_includes"
    fi
  fi
  if test -n "$x_libraries" -a "$x_libraries" != NONE; then
    X_LIBS="-L$x_libraries "$X_LIBS
  fi
else
  dnl From John Ousterhout <ouster@allspice.berkeley.edu>
  dnl There are some X11 installations around that don't have xmkmf.
  AC_MSG_CHECKING(for X11 header files)
  AC_TRY_CPP([#include <X11/Intrinsic.h>], have_x_includes=1)
  if test -z "$have_x_includes"; then
    for dir in /usr/X11/include /usr/openwin/include /usr/include/X11R5 /usr/X11R5/include /usr/include/X11R4 /usr/X386/include /usr/x386/include /usr/local/include /usr/unsupported/include; do
      if test -z "$have_x_includes"; then
        if test -r $dir/X11/Intrinsic.h; then
          x_includes="$dir"
          have_x_includes=1
        fi
      fi
    done
  fi
  if test -n "$x_includes" -a "$x_includes" != NONE; then
    X_INCLUDES="-I$x_includes"
  fi
  AC_MSG_RESULT($X_INCLUDES)
  AC_MSG_CHECKING(for X11 library)
  CL_SILENT([
  AC_CHECK_LIB(X11,main, have_x_libraries=1)dnl
  if test -z "$have_x_libraries"; then
    for dir in /usr/X11/lib /usr/openwin/lib /usr/lib/X11R5 /usr/X11R5/lib /usr/lib/X11R4 /usr/X386/lib /usr/x386/lib /usr/local/lib /usr/unsupported/lib; do
      if test -z "$have_x_libraries"; then
        if test -r $dir/libX11.a; then
          x_libraries="$dir"
          have_x_libraries=1
        fi
      fi
    done
  fi
  if test -n "$have_x_libraries"; then
    X_LIBS='-lX11'
    if test -n "$x_libraries" -a "$x_libraries" != NONE; then
      X_LIBS="-L$x_libraries "$X_LIBS
    fi
  else
    AC_CHECK_LIB(Xwindow,main, X_LIBS='-lXwindow' have_x_libraries=1)dnl
  fi
  ])
  AC_MSG_RESULT($X_LIBS)
  dnl Don't use X if either the header files or the libraries were not found.
  if test -n "$have_x_includes" -a -n "$have_x_libraries"; then
    have_x11=1
    AC_DEFINE(HAVE_X11)
  else
    X_INCLUDES=''
    X_LIBS=''
  fi
fi
AC_SUBST(X_INCLUDES)dnl
AC_SUBST(X_LIBS)dnl
])dnl
dnl
AC_DEFUN(CL_IRIX_SUN,
[AC_CHECK_FUNC(getpwnam, have_getpwnam=1)
if test -z "$have_getpwnam"; then
  AC_CHECK_LIB(sun,getpwnam)
fi
])dnl
dnl
AC_DEFUN(CL_DYNIX_SEQ,
[AC_CACHE_CHECK(for DYNIX/ptx libseq or libsocket, cl_cv_lib_sequent, [
AC_EGREP_CPP(yes,
[#if defined(_SEQUENT_)
  yes
#endif
], cl_cv_lib_sequent=yes, cl_cv_lib_sequent=no)
])
if test $cl_cv_lib_sequent = yes; then
AC_CHECK_LIB(seq,main,LIBS="$LIBS -lseq")
dnl libsocket is needed for select()
AC_CHECK_LIB(socket,main,LIBS="$LIBS -lsocket")
fi
])dnl
dnl
AC_DEFUN(CL_SOCKET,
[dnl Check whether -lsocket and maybe -lnsl is needed for the functions
dnl gethostbyname(), connect(), socket(), inet_addr(), setsockopt().
dnl On SVR4 and Solaris 2:
dnl   gethostbyname() and gethostent() in libnsl,
dnl   setsockopt() in libsocket,
dnl   libsocket requires libnsl.
dnl On SINIX-N 5.43:
dnl   gethostent() in libnsl,
dnl   gethostbyname() and setsockopt() in libsocket,
dnl   libsocket requires libnsl.
dnl Hence checking for gethostent() gives a better hint whether libnsl is
dnl needed than gethostbyname(). Once we found that, we check for setsockopt().
AC_CACHE_CHECK(whether gethostent requires -lnsl, cl_cv_lib_nsl, [
cl_cv_lib_nsl=no
AC_TRY_LINK(AC_LANG_EXTERN[char gethostent();], [gethostent();],
have_gethostent=1)
if test -z "$have_gethostent"; then
cl_save_LIBS="$LIBS"
LIBS="$LIBS -lnsl"
AC_TRY_LINK(AC_LANG_EXTERN[char gethostent();], [gethostent();],
cl_cv_lib_nsl=yes)
LIBS="$cl_save_LIBS"
fi
])
if test $cl_cv_lib_nsl = yes; then
  LIBS="$LIBS -lnsl"
fi
AC_CACHE_CHECK(whether setsockopt requires -lsocket, cl_cv_lib_socket, [
cl_cv_lib_socket=no
AC_TRY_LINK(AC_LANG_EXTERN[char setsockopt();], [setsockopt();],
have_setsockopt=1)
if test -z "$have_setsockopt"; then
cl_save_LIBS="$LIBS"
LIBS="$LIBS -lsocket"
AC_TRY_LINK(AC_LANG_EXTERN[char setsockopt();], [setsockopt();],
cl_cv_lib_socket=yes)
LIBS="$cl_save_LIBS"
fi
])
if test $cl_cv_lib_socket = yes; then
  LIBS="$LIBS -lsocket"
fi
])dnl
dnl
AC_DEFUN(CL_CC_WORKS,
[AC_CACHE_CHECK(whether CC works at all, cl_cv_prog_cc_works, [
AC_LANG_SAVE()
AC_LANG_C()
AC_TRY_RUN([int main() { exit(0); }],
cl_cv_prog_cc_works=yes, cl_cv_prog_cc_works=no,
AC_TRY_LINK([], [], cl_cv_prog_cc_works=yes, cl_cv_prog_cc_works=no))
AC_LANG_RESTORE()
])
case "$cl_cv_prog_cc_works" in
  *no) echo "Installation or configuration problem: C compiler cannot create executables."; exit 1;;
  *yes) ;;
esac
])dnl
dnl
AC_DEFUN(CL_CXX_WORKS,
[AC_CACHE_CHECK(whether CXX works at all, cl_cv_prog_cxx_works, [
AC_LANG_SAVE()
AC_LANG_CPLUSPLUS()
AC_TRY_RUN([int main() { exit(0); }],
cl_cv_prog_cxx_works=yes, cl_cv_prog_cxx_works=no,
AC_TRY_LINK([], [], cl_cv_prog_cxx_works=yes, cl_cv_prog_cxx_works=no))
AC_LANG_RESTORE()
])
case "$cl_cv_prog_cxx_works" in
  *no) echo "Installation or configuration problem: C++ compiler cannot create executables."; exit 1;;
  *yes) ;;
esac
])dnl
dnl
AC_DEFUN(CL_SHELLARGS,
[AC_REQUIRE([AC_PROG_CC])dnl
AC_CACHE_CHECK([for broken HP/UX, A/UX, OSF/1 and NeXTstep shell], cl_cv_prog_sh_broken, [
if test $cross_compiling = no; then
# A program that outputs its argument count:
cat > conftest.c <<EOF
#include "confdefs.h"
#include <stdio.h>
#ifdef __cplusplus
extern "C" void exit(int);
#endif
#if defined(__STDC__) || defined(__cplusplus)
int main (int argc, char** argv)
#else
int main (argc,argv) int argc; char** argv;
#endif
{ printf("%d\n",argc); exit(0); }
EOF
AC_TRY_EVAL(ac_link)
# How can a shell script forward its arguments to another program?
#                                    $ * "$ *" "$ @"
# conftest.sh                         1    2     1 (*)
# conftest.sh foo                     2    2     2
# conftest.sh foo bar                 3    2     3
# conftest.sh "foo bar"               3    2     2
# (*): 2 with HP-UX /bin/sh. We must use /bin/ksh instead.
#      2 as well with NeXTstep /bin/sh. No /bin/ksh. Examine $# first.
psubs='"$''@"'
cat > conftest.sh <<EOF
#!/bin/sh
exec ./conftest $psubs
EOF
chmod a+x conftest.sh
if test `./conftest.sh` = "1"; then
  cl_cv_prog_sh_broken=no
  cl_cv_prog_sh_good='/bin/sh'
else
  cl_cv_prog_sh_broken=yes
  if test -x /bin/ksh; then
    cl_cv_prog_sh_good='/bin/ksh'
  else
    cl_cv_prog_sh_good=''
  fi
fi
else
cl_cv_prog_sh_broken="guessing no"
cl_cv_prog_sh_good='/bin/sh'
fi
rm -f conftest*
])
GOOD_SH="$cl_cv_prog_sh_good"
if test "$GOOD_SH" = '/bin/ksh'; then
  AC_DEFINE(UNIX_USE_KSH)
fi
if test "$GOOD_SH" = ''; then
  AC_DEFINE(UNIX_BROKEN_SH)
fi
AC_SUBST(GOOD_SH)dnl
])dnl
dnl
AC_DEFUN(CL_CONFIG_SUBDIRS,
[dnl No AC_CONFIG_AUX_DIR_DEFAULT, so we don't need install.sh.
define([AC_LIST_SUBDIRS], [$1])dnl
subdirs="AC_LIST_SUBDIRS"
AC_SUBST(subdirs)dnl
])dnl
dnl
AC_DEFUN(CL_CANONICAL_HOST,
[AC_REQUIRE([AC_PROG_CC]) dnl Actually: AC_REQUIRE([CL_CC_WORKS])
dnl Set ac_aux_dir before the cache check, because AM_PROG_LIBTOOL needs it.
ac_aux_dir=${srcdir}/$1
AC_CACHE_CHECK(host system type, cl_cv_host, [
dnl A substitute for AC_CONFIG_AUX_DIR_DEFAULT, so we don't need install.sh.
ac_config_guess=$ac_aux_dir/config.guess
ac_config_sub=$ac_aux_dir/config.sub
dnl Mostly copied from AC_CANONICAL_HOST.
# Make sure we can run config.sub.
if $ac_config_sub sun4 >/dev/null 2>&1; then :
else AC_MSG_ERROR(can not run $ac_config_sub)
fi
host_alias=$host
case "$host_alias" in
NONE)
  case $nonopt in
  NONE) dnl config.guess needs to compile things
        host_alias=`export CC; $ac_config_guess` ;;
  *)    host_alias=$nonopt ;;
  esac ;;
esac
# Don't fail just because the system is not listed in GNU's database.
if test -n "$host_alias"; then
  host=`$ac_config_sub $host_alias`
else
  host=unknown-unknown-unknown
fi
cl_cv_host="$host"
])
host="$cl_cv_host"
host_cpu=`echo $host | sed 's/^\(.*\)-\(.*\)-\(.*\)$/\1/'`
host_vendor=`echo $host | sed 's/^\(.*\)-\(.*\)-\(.*\)$/\2/'`
host_os=`echo $host | sed 's/^\(.*\)-\(.*\)-\(.*\)$/\3/'`
AC_SUBST(host)dnl
AC_SUBST(host_cpu)dnl
AC_SUBST(host_vendor)dnl
AC_SUBST(host_os)dnl
])dnl
dnl
AC_DEFUN(CL_CANONICAL_HOST_CPU,
[AC_REQUIRE([CL_CANONICAL_HOST])AC_REQUIRE([AC_PROG_CC])
case "$host_cpu" in
changequote(,)dnl
  i[4567]86 )
    host_cpu=i386
    ;;
  alphaev[4-7] | alphaev56 | alphapca5[67] )
    host_cpu=alpha
    ;;
  hppa1.0 | hppa1.1 | hppa2.0 )
    host_cpu=hppa
    ;;
  powerpc )
    host_cpu=rs6000
    ;;
  c1 | c2 | c32 | c34 | c38 | c4 )
    host_cpu=convex
    ;;
changequote([,])dnl
  mips )
    AC_CACHE_CHECK([for 64-bit MIPS], cl_cv_host_mips64, [
AC_EGREP_CPP(yes,
[#if defined(_MIPS_SZLONG)
#if (_MIPS_SZLONG == 64)
/* We should also check for (_MIPS_SZPTR == 64), but gcc keeps this at 32. */
  yes
#endif
#endif
], cl_cv_host_mips64=yes, cl_cv_host_mips64=no)
])
if test $cl_cv_host_mips64 = yes; then
  host_cpu=mips64
fi
    ;;
dnl UltraSPARCs running Linux have `uname -m` = "sparc64", but the C compiler
dnl still generates 32-bit code.
  sparc | sparc64 )
    AC_CACHE_CHECK([for 64-bit SPARC], cl_cv_host_sparc64, [
AC_EGREP_CPP(yes,
[#if defined(__arch64__)
  yes
#endif
], cl_cv_host_sparc64=yes, cl_cv_host_sparc64=no)
])
if test $cl_cv_host_sparc64 = yes; then
  host_cpu=sparc64
else
  host_cpu=sparc
fi
    ;;
esac
dnl was AC_DEFINE_UNQUOTED(__${host_cpu}__) but KAI C++ 3.2d doesn't like this
cat >> confdefs.h <<EOF
#ifndef __${host_cpu}__
#define __${host_cpu}__ 1
#endif
EOF
])dnl
dnl
AC_DEFUN(CL_CANONICAL_HOST_CPU_FOR_FFCALL,
[AC_REQUIRE([CL_CANONICAL_HOST])AC_REQUIRE([AC_PROG_CC])
case "$host_cpu" in
changequote(,)dnl
  i[4567]86 )
    host_cpu=i386
    ;;
  alphaev[4-7] | alphaev56 | alphapca5[67] )
    host_cpu=alpha
    ;;
  hppa1.0 | hppa1.1 | hppa2.0 )
    host_cpu=hppa
    ;;
  powerpc )
    host_cpu=rs6000
    ;;
  c1 | c2 | c32 | c34 | c38 | c4 )
    host_cpu=convex
    ;;
changequote([,])dnl
  mips )
    AC_CACHE_CHECK([for 64-bit MIPS], cl_cv_host_mips64, [
AC_EGREP_CPP(yes,
[#if defined(_MIPS_SZLONG)
#if (_MIPS_SZLONG == 64)
/* We should also check for (_MIPS_SZPTR == 64), but gcc keeps this at 32. */
  yes
#endif
#endif
], cl_cv_host_mips64=yes, cl_cv_host_mips64=no)
])
if test $cl_cv_host_mips64 = yes; then
  host_cpu=mips64
else
  AC_CACHE_CHECK([for MIPS with n32 ABI], cl_cv_host_mipsn32, [
dnl Strictly speaking, the MIPS ABI (-32 or -n32) is independent from the CPU
dnl identification (-mips[12] or -mips[34]). But -n32 is commonly used together
dnl with -mips3, and it's easier to test the CPU identification.
AC_EGREP_CPP(yes,
[#if __mips >= 3
  yes
#endif
], cl_cv_host_mipsn32=yes, cl_cv_host_mipsn32=no)
])
if test $cl_cv_host_mipsn32 = yes; then
  host_cpu=mipsn32
fi
fi
    ;;
dnl UltraSPARCs running Linux have `uname -m` = "sparc64", but the C compiler
dnl still generates 32-bit code.
  sparc | sparc64 )
    AC_CACHE_CHECK([for 64-bit SPARC], cl_cv_host_sparc64, [
AC_EGREP_CPP(yes,
[#if defined(__arch64__)
  yes
#endif
], cl_cv_host_sparc64=yes, cl_cv_host_sparc64=no)
])
if test $cl_cv_host_sparc64 = yes; then
  host_cpu=sparc64
else
  host_cpu=sparc
fi
    ;;
esac
dnl was AC_DEFINE_UNQUOTED(__${host_cpu}__) but KAI C++ 3.2d doesn't like this
cat >> confdefs.h <<EOF
#ifndef __${host_cpu}__
#define __${host_cpu}__ 1
#endif
EOF
])dnl
dnl
AC_DEFUN(RL_VOID,
[CL_COMPILE_CHECK([working void], rl_cv_c_void, ,
[void f();
typedef void x; x g();
typedef void* y; y a;
], , AC_DEFINE(void,char))dnl
])dnl
dnl
AC_DEFUN(CL_VOID,
[CL_COMPILE_CHECK([working void], cl_cv_c_void, ,
[void f();
typedef void x; x g();
typedef void* y; y a;
], have_void=1, AC_DEFINE(void,char))dnl
if test -n "$have_void"; then
CL_COMPILE_CHECK([working \"return void\"], cl_cv_c_return_void,
[void f() {} typedef void x; x g() { return f(); }], [],
AC_DEFINE(return_void,[return]))dnl
fi
])dnl
dnl
AC_DEFUN(CL_PCC_STRUCT_RETURN,
[AC_CACHE_CHECK([for pcc non-reentrant struct return convention], cl_cv_c_struct_return_static, [
AC_TRY_RUN([typedef struct { int a; int b; int c; int d; int e; } foo;
foo foofun () { static foo foopi = {3141,5926,5358,9793,2385}; return foopi; }
foo* (*fun) () = (foo* (*) ()) foofun;
int main()
{ foo foo1;
  foo* fooptr1;
  foo foo2;
  foo* fooptr2;
  foo1 = foofun(); fooptr1 = (*fun)(&foo1);
  foo2 = foofun(); fooptr2 = (*fun)(&foo2);
  exit(!(fooptr1 == fooptr2 && fooptr1->c == 5358));
}], cl_cv_c_struct_return_static=yes, rm -f core
cl_cv_c_struct_return_static=no,
dnl When cross-compiling, don't assume anything.
dnl There are even weirder return value passing conventions than pcc.
cl_cv_c_struct_return_static="guessing no")
])
case "$cl_cv_c_struct_return_static" in
  *yes) AC_DEFINE(__PCC_STRUCT_RETURN__) ;;
  *no) ;;
esac
])dnl
dnl
AC_DEFUN(CL_SMALL_STRUCT_RETURN,
[AC_CACHE_CHECK([whether small structs are returned in registers], cl_cv_c_struct_return_small, [
AC_TRY_RUN([typedef struct { int x; } foo; int y;
foo foofun () { foo f; f.x = y; return f; }
int (*fun) () = (int (*) ()) foofun;
int main()
{ y = 37; if ((*fun)() != 37) exit(1);
  y = 55; if ((*fun)() != 55) exit(1);
  exit(0);
}], cl_cv_c_struct_return_small=yes, rm -f core
cl_cv_c_struct_return_small=no,
dnl When cross-compiling, don't assume anything.
dnl There are even weirder return value passing conventions than pcc.
cl_cv_c_struct_return_small="guessing no")
])
case "$cl_cv_c_struct_return_small" in
  *yes) AC_DEFINE(__SMALL_STRUCT_RETURN__) ;;
  *no) ;;
esac
])dnl
dnl
AC_DEFUN(CL_IREG_FLOAT_RETURN,
[AC_CACHE_CHECK([whether floats are returned in integer registers], cl_cv_c_float_return_ireg, [
AC_TRY_RUN([float x = (float)1.2;
float y = (float)1.3;
float fun () { return x*y; }
int main()
{ int val = (* (int (*) ()) fun) ();
  exit (!(val == 0x3FC7AE15 || val == 0x15AEC73F));
}], cl_cv_c_float_return_ireg=yes, rm -f core
cl_cv_c_float_return_ireg=no,
dnl When cross-compiling, assume no, because that's how it comes out on
dnl most platforms with floating-point unit, including m68k-linux.
cl_cv_c_float_return_ireg="guessing no")
])
case "$cl_cv_c_float_return_ireg" in
  *yes) AC_DEFINE(__IREG_FLOAT_RETURN__) ;;
  *no) ;;
esac
])dnl
dnl
AC_DEFUN(CL_LONGLONG,
[AC_CACHE_CHECK(for long long type, cl_cv_c_longlong, [
AC_TRY_RUN([int main()
{
/* long longs don't work right with gcc-2.7.2 on m68k */
/* long longs don't work right with gcc-2.7.2 on rs6000: avcall/tests.c gets
   miscompiled. */
#ifdef __GNUC__
#if defined(__m68k__) || (defined(_IBMR2) || defined(__powerpc))
#if (__GNUC__ == 2)
#if (__GNUC_MINOR__ <= 7)
  exit(1);
#endif
#endif
#endif
#endif
  { long x = 944938507; long y = 737962842; long z = 162359677;
    exit(!(((long long) x)*((long long) y)>>32 == z));
  }
}],
cl_cv_c_longlong=yes, cl_cv_c_longlong=no,
dnl When cross-compiling, don't assume anything.
cl_cv_c_longlong="guessing no")
])
case "$cl_cv_c_longlong" in
  *yes) AC_DEFINE(HAVE_LONGLONG) ;;
  *no) ;;
esac
])dnl
dnl
AC_DEFUN(CL_LONGDOUBLE,
[AC_CACHE_CHECK(for long double type, cl_cv_c_longdouble, [
AC_TRY_RUN([int main()
{ long double x = 2.7182818284590452354L; x = x*x; exit (x==0.0L); }],
cl_cv_c_longdouble=yes, cl_cv_c_longdouble=no,
dnl When cross-compiling, don't assume anything.
cl_cv_c_longdouble="guessing no")
])
case "$cl_cv_c_longdouble" in
  *yes) AC_DEFINE(HAVE_LONGDOUBLE) ;;
  *no) ;;
esac
])dnl
dnl
AC_DEFUN(CL_TEMPLATE_NULL,
[CL_COMPILE_CHECK([working template<>], cl_cv_c_templatenull,
[template <class T> class c {}; template <> class c<int> { int x; };], ,
AC_DEFINE(HAVE_TEMPLATE_NULL))
])dnl
dnl
AC_DEFUN(CL_STDC_HEADERS,
dnl This is AC_STDC_HEADERS from Autoconf 1.2. The AC_STDC_HEADERS from
dnl Autoconf 1.3 fails on 386BSD because it checks for correct ANSI ctype
dnl macros and 386BSD (as well as SGI's /bin/cc from Irix-4.0.5) doesn't
dnl have them. But we don't need them!
dnl The same holds for the mem* functions in <string.h> and SunOS.
[CL_CPP_CHECK([ANSI C header files], cl_cv_header_stdc,
[#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <float.h>
#include <limits.h>], AC_DEFINE(STDC_HEADERS))
])dnl
dnl
AC_DEFUN(CL_STDLIB_H,
[AC_BEFORE([$0], [CL_ABORT])
AC_CHECK_HEADERS(stdlib.h)]
)dnl
dnl
AC_DEFUN(CL_STDDEF_H,
[AC_BEFORE([$0], [CL_ABORT])
AC_CHECK_HEADERS(stddef.h)]
)dnl
dnl
AC_DEFUN(CL_OFFSETOF,
[CL_COMPILE_CHECK([offsetof in stddef.h], cl_cv_offsetof,
[#include <stddef.h>], [#ifndef offsetof
error no offsetof
#endif
], AC_DEFINE(HAVE_OFFSETOF))]
)dnl
dnl
AC_DEFUN(CL_LOCALE_H,
[AC_CHECK_HEADERS(locale.h)]
)dnl
dnl
AC_DEFUN(CL_UNISTD_H,
[AC_CHECK_HEADERS(unistd.h)]
)dnl
dnl
AC_DEFUN(CL_ACCESSFLAGS,
dnl Old BSD systems require #include <sys/file.h> for R_OK etc. being defined.
[AC_CHECK_HEADERS(sys/file.h)
if test $ac_cv_header_sys_file_h = yes; then
accessflags_decl='#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
'
accessflags_prog='int x = R_OK | W_OK | X_OK;'
CL_COMPILE_CHECK([R_OK in unistd.h], cl_cv_decl_R_OK_unistd_h,
$accessflags_decl, $accessflags_prog, accessflags_ok=1)dnl
if test -z "$accessflags_ok"; then
dnl CL_COMPILE_CHECK([R_OK in sys/file.h], cl_cv_decl_R_OK_sys_file_h,
dnl $accessflags_decl[#include <sys/file.h>], $accessflags_prog,
AC_DEFINE(ACCESS_NEEDS_SYS_FILE_H)
dnl accessflags_ok=1)dnl
fi
fi
])dnl
dnl
AC_DEFUN(CL_OPENFLAGS,
dnl BSD systems require #include <sys/file.h> for O_RDWR etc. being defined.
[AC_BEFORE([$0], [CL_MMAP])
AC_CHECK_HEADERS(sys/file.h)
if test $ac_cv_header_sys_file_h = yes; then
openflags_decl='#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#include <fcntl.h>
'
openflags_prog='int x = O_RDWR | O_RDONLY | O_WRONLY | O_CREAT | O_TRUNC;'
CL_COMPILE_CHECK([O_RDWR in fcntl.h], cl_cv_decl_O_RDWR_fcntl_h,
$openflags_decl, $openflags_prog, openflags_ok=1)dnl
if test -z "$openflags_ok"; then
dnl CL_COMPILE_CHECK([O_RDWR in sys/file.h], cl_cv_decl_O_RDWR_sys_file_h,
dnl $openflags_decl[#include <sys/file.h>], $openflags_prog,
AC_DEFINE(OPEN_NEEDS_SYS_FILE_H)
dnl openflags_ok=1)dnl
fi
fi
])dnl
dnl
AC_DEFUN(CL_DIR_HEADER,
[AC_BEFORE([$0], [CL_OPENDIR])dnl
AC_BEFORE([$0], [CL_CLOSEDIR])dnl
dnl This is mostly copied from AC_DIR_HEADER, AC_HEADER_DIRENT, AC_FUNC_CLOSEDIR_VOID
dnl but not from AC_CHECK_HEADERS_DIRENT.
dnl The closedir return check has been moved to CL_CLOSEDIR.
ac_header_dirent=no
for ac_hdr in dirent.h sys/ndir.h sys/dir.h ndir.h; do
  AC_CHECK_HEADER_DIRENT($ac_hdr, [ac_header_dirent=$ac_hdr; break])
done
case "$ac_header_dirent" in
dirent.h) AC_DEFINE(DIRENT) ;;
sys/ndir.h) AC_DEFINE(SYSNDIR) ;;
sys/dir.h) AC_DEFINE(SYSDIR) ;;
ndir.h) AC_DEFINE(NDIR) ;;
esac
# Two versions of opendir et al. are in -ldir and -lx on SCO Xenix.
if test $ac_header_dirent = dirent.h; then
AC_CHECK_LIB(dir, opendir, LIBS="$LIBS -ldir")
else
AC_CHECK_LIB(x, opendir, LIBS="$LIBS -lx")
fi
])dnl
dnl
AC_DEFUN(CL_UTSNAME,
[CL_COMPILE_CHECK([sys/utsname.h and struct utsname], cl_cv_struct_utsname,
[#include <sys/utsname.h>],
[struct utsname u;], AC_DEFINE(HAVE_SYS_UTSNAME_H))dnl
])dnl
dnl
AC_DEFUN(CL_NETDB,
[AC_BEFORE([$0], [CL_GETHOSTBYNAME])
AC_CHECK_HEADERS(netdb.h)]
)dnl
dnl
AC_DEFUN(CL_SHM_H,
[AC_BEFORE([$0], [CL_SHMGET])dnl
AC_BEFORE([$0], [CL_SHMAT])dnl
AC_BEFORE([$0], [CL_SHMDT])dnl
AC_BEFORE([$0], [CL_SHMCTL])dnl
AC_BEFORE([$0], [CL_SHM_RMID])dnl
AC_CHECK_HEADERS(sys/shm.h)
if test $ac_cv_header_sys_shm_h = yes; then
AC_CHECK_HEADERS(sys/ipc.h)
fi
])dnl
dnl
AC_DEFUN(RL_TERM,
[AC_CHECK_HEADERS(termios.h termio.h sys/termio.h sgtty.h)dnl
if test $ac_cv_header_termios_h = yes; then
  dnl HAVE_TERMIOS_H defined
  AC_CHECK_FUNCS(tcgetattr tcflow)dnl
fi
AC_CHECK_HEADERS(sys/stream.h sys/ptem.h)dnl
ioctl_decl='#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#if defined(HAVE_TERMIOS_H) && defined(HAVE_TCGETATTR) && defined(HAVE_TCFLOW)
#include <termios.h>
#else
#if defined(HAVE_TERMIO_H) || defined(HAVE_SYS_TERMIO_H)
#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#else
#include <termio.h>
#endif
#else
#include <sgtty.h>
#include <sys/ioctl.h>
#endif
#endif
#ifdef HAVE_SYS_STREAM_H
#include <sys/stream.h>
#endif
#ifdef HAVE_SYS_PTEM_H
#include <sys/ptem.h>
#endif
'
ioctl_prog='int x = FIONREAD;'
CL_COMPILE_CHECK([FIONREAD], cl_cv_decl_FIONREAD_termio_h,
$ioctl_decl, $ioctl_prog, ioctl_ok=1)dnl
if test -z "$ioctl_ok"; then
CL_COMPILE_CHECK([FIONREAD in sys/filio.h], cl_cv_decl_FIONREAD_sys_filio_h,
$ioctl_decl[#include <sys/filio.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_FILIO_H)
ioctl_ok=1)dnl
fi
if test -z "$ioctl_ok"; then
CL_COMPILE_CHECK([FIONREAD in sys/ioctl.h], cl_cv_decl_FIONREAD_sys_ioctl_h,
$ioctl_decl[#include <sys/ioctl.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_IOCTL_H)
ioctl_ok=1)dnl
fi
])dnl
dnl
AC_DEFUN(CL_TERM,
[AC_BEFORE([$0], [CL_IOCTL])
AC_CHECK_HEADERS(termios.h termio.h sys/termio.h sgtty.h)dnl
if test $ac_cv_header_termios_h = yes; then
dnl HAVE_TERMIOS_H defined
dnl A/UX has <termios.h> but is lacking tcgetattr etc.
CL_LINK_CHECK([tcgetattr], cl_cv_func_tcgetattr,
[#include <termios.h>], [struct termios t; tcgetattr(0,&t);],
AC_DEFINE(HAVE_TCGETATTR))dnl
if test $cl_cv_func_tcgetattr = yes; then
CL_PROTO([tcsetattr], [
AC_TRY_COMPILE([
#include <termios.h>
#ifndef tcsetattr
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int tcsetattr (int, int, struct termios *);
#else
int tcsetattr ();
#endif
#endif
], [], cl_cv_proto_tcsetattr_arg3="", cl_cv_proto_tcsetattr_arg3="const")
], [extern int tcsetattr (int, int, $cl_cv_proto_tcsetattr_arg3 struct termios *);])
AC_DEFINE_UNQUOTED(TCSETATTR_CONST,$cl_cv_proto_tcsetattr_arg3)
fi
CL_LINK_CHECK([TCSAFLUSH in termios.h], cl_cv_decl_TCSAFLUSH,
[#include <termios.h>], [int x = TCSAFLUSH;],
AC_DEFINE(HAVE_TCSAFLUSH))dnl
dnl Linux libc5 defines struct winsize in <termios.h>, <termio.h>, <sys/ioctl.h>.
dnl Linux libc6 defines struct winsize in <termio.h>, <sys/ioctl.h>.
dnl Since we don't want to include both <termios.h> and <termio.h> (they may
dnl conflict), prefer <sys/ioctl.h> to <termio.h>.
dnl SCO defines struct winsize in <sys/ptem.h>, which itself needs <sys/stream.h>.
CL_COMPILE_CHECK([struct winsize in termios.h], cl_cv_struct_winsize,
[#include <termios.h>], [struct winsize w;], )dnl
if test $cl_cv_struct_winsize = no; then
CL_COMPILE_CHECK([struct winsize in sys/ioctl.h], cl_cv_struct_winsize_ioctl,
[#include <sys/types.h>
#include <sys/ioctl.h>],
[struct winsize w;], AC_DEFINE(WINSIZE_NEED_SYS_IOCTL_H))dnl
if test $cl_cv_struct_winsize_ioctl = no; then
CL_COMPILE_CHECK([struct winsize in sys/ptem.h], cl_cv_struct_winsize_ptem,
[#include <sys/types.h>
#include <sys/stream.h>
#include <sys/ptem.h>],
[struct winsize w;], AC_DEFINE(WINSIZE_NEED_SYS_PTEM_H))dnl
fi
fi
fi
])dnl
dnl
AC_DEFUN(CL_CADDR_T,
[AC_CACHE_CHECK(for caddr_t in sys/types.h, cl_cv_type_caddr_t, [
AC_EGREP_HEADER(caddr_t, sys/types.h,
cl_cv_type_caddr_t=yes, cl_cv_type_caddr_t=no)
])
if test $cl_cv_type_caddr_t = yes; then
  AC_DEFINE(CADDR_T, caddr_t)
else
  AC_DEFINE(CADDR_T, void*)
fi
]
)dnl
dnl
AC_DEFUN(CL_CLOCK_T,
[AC_CACHE_CHECK(for clock_t in sys/types.h etc., cl_cv_type_clock_t, [
AC_EGREP_HEADER(clock_t, sys/types.h, have_clock=1)dnl
if test -z "$have_clock"; then
AC_EGREP_HEADER(clock_t, sys/times.h, have_clock=1)dnl
fi
if test -z "$have_clock"; then
AC_EGREP_HEADER(clock_t, time.h, have_clock=1)dnl
fi
if test -z "$have_clock"; then
  cl_cv_type_clock_t=no
else
  cl_cv_type_clock_t=yes
fi
])
if test $cl_cv_type_clock_t = yes; then
  AC_DEFINE(CLOCK_T, clock_t)
else
  AC_DEFINE(CLOCK_T, int)
fi
])dnl
dnl
AC_DEFUN(CL_DIRENT_WITHOUT_NAMLEN,
[CL_COMPILE_CHECK([d_namlen in struct dirent], cl_cv_struct_dirent_d_namlen,
[#include <dirent.h>], [struct dirent d; d.d_namlen;],
 , AC_DEFINE(DIRENT_WITHOUT_NAMLEN))]
)dnl
dnl
AC_DEFUN(CL_STRUCT_TM,
[AC_PROVIDE([$0])dnl
CL_COMPILE_CHECK([struct tm in sys/time.h], cl_cv_struct_tm_sys_time_h,
[#include <sys/types.h>
#include <sys/time.h>],
[struct tm *tp; tp->tm_sec;], AC_DEFINE(TM_IN_SYS_TIME))])dnl
dnl
AC_DEFUN(CL_STRLEN,
[
# This is a bit tricky since strlen is a gcc2 built-in function, and
# gcc's criterion when to reject a prototype for it is extremely mysterious.
#   extern int strlen (char * s); extern unsigned int strlen (char * s);
# produces an error, while
#   extern unsigned int strlen (char * s); extern int strlen (char * s);
# compiles well.
if test -z "$cl_cv_proto_strlen_macro"; then
AC_EGREP_CPP([is a macro], [#include <string.h>
#ifdef strlen
is a macro
#endif
], cl_cv_proto_strlen_macro=yes, cl_cv_proto_strlen_macro=no)
fi
if test $cl_cv_proto_strlen_macro = no; then
CL_PROTO([strlen], [
CL_PROTO_RET([#define strlen foo
#include <string.h>
], [int strlen();], cl_cv_proto_strlen_ret, int, size_t)
CL_PROTO_CONST([#define strlen foo
#include <string.h>
], [$cl_cv_proto_strlen_ret strlen (char* s);],
[$cl_cv_proto_strlen_ret strlen();], cl_cv_proto_strlen_arg1)],
[extern $cl_cv_proto_strlen_ret strlen ($cl_cv_proto_strlen_arg1 char*);])
AC_DEFINE_UNQUOTED(RETSTRLENTYPE,$cl_cv_proto_strlen_ret)
AC_DEFINE_UNQUOTED(STRLEN_CONST,$cl_cv_proto_strlen_arg1)
fi
])dnl
dnl
AC_DEFUN(CL_MEMSET,
[AC_CHECK_FUNCS(memset)
if test $ac_cv_func_memset = yes; then
CL_PROTO([memset], [
for y in 'int' 'size_t'; do
for x in 'char*' 'void*'; do
if test -z "$have_memset"; then
AC_TRY_COMPILE([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#undef memset
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
$x memset ($x s, int c, $y n);
#else
$x memset();
#endif
], [],
cl_cv_proto_memset_ret="$x"
cl_cv_proto_memset_arg1="$x"
cl_cv_proto_memset_arg3="$y"
have_memset=1)
fi
done
done
], [extern $cl_cv_proto_memset_ret memset ($cl_cv_proto_memset_arg1, int, $cl_cv_proto_memset_arg3);])
AC_DEFINE_UNQUOTED(RETMEMSETTYPE,$cl_cv_proto_memset_ret)
fi
])dnl
dnl
AC_DEFUN(CL_GMALLOC,
[dnl Invented by François Pinard <pinard@iro.umontreal.ca>
dnl (but he denies having invented it).
AC_CACHE_CHECK([for broken HP/UX malloc], cl_cv_func_malloc_broken, [
if test $cross_compiling = no; then
# Note that HP-UX has two different malloc() implementations.
# Both are broken. When used with CLISP, the one in the default libc.a
# leads to a SIGSEGV, the one in libmalloc.a leads to a SIGBUS.
case "$host_os" in
  hpux*) cl_cv_func_malloc_broken=yes ;;
  *) cl_cv_func_malloc_broken=no ;;
esac
else
cl_cv_func_malloc_broken="guessing no"
fi
])
case "$cl_cv_func_malloc_broken" in
  *yes) # Remedy: Use GNU malloc.
        GMALLOC=gmalloc ;;
  *no)  GMALLOC='' ;;
esac
AC_SUBST(GMALLOC)
])dnl
dnl
AC_DEFUN(CL_MALLOC,
[CL_PROTO([malloc], [
AC_EGREP_HEADER([void.*\*.*malloc], stdlib.h, malloc_void=1)dnl
if test -z "$malloc_void"; then
AC_TRY_COMPILE([
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
]AC_LANG_EXTERN[void* malloc();], [],
malloc_void=1)
fi
if test -n "$malloc_void"; then
cl_cv_proto_malloc_ret="void*"
else
cl_cv_proto_malloc_ret="char*"
fi
CL_PROTO_TRY([
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [$cl_cv_proto_malloc_ret malloc (unsigned int size);],
[$cl_cv_proto_malloc_ret malloc();],
cl_cv_proto_malloc_arg1="unsigned int", cl_cv_proto_malloc_arg1="size_t")
], [extern $cl_cv_proto_malloc_ret malloc ($cl_cv_proto_malloc_arg1);])
AC_DEFINE_UNQUOTED(RETMALLOCTYPE,$cl_cv_proto_malloc_ret)
AC_DEFINE_UNQUOTED(MALLOC_SIZE_T,$cl_cv_proto_malloc_arg1)
])dnl
dnl
AC_DEFUN(CL_FREE,
[CL_PROTO([free], [
CL_PROTO_RET([
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [int free();], cl_cv_proto_free_ret, int, void)],
[extern $cl_cv_proto_free_ret free ($cl_cv_proto_malloc_ret);])
AC_DEFINE_UNQUOTED(RETFREETYPE,$cl_cv_proto_free_ret)
])dnl
dnl
AC_DEFUN(CL_ALLOCA,
[# The Ultrix 4.2 mips builtin alloca declared by alloca.h only works
# for constant arguments.  Useless!
CL_LINK_CHECK(working alloca.h, cl_cv_header_alloca_h,
[#include <alloca.h>], [char *p = (char *) alloca(2 * sizeof(int));],
AC_DEFINE(HAVE_ALLOCA_H))dnl
decl="#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#ifdef _MSC_VER
#include <malloc.h>
#define alloca _alloca
#else
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#else
#ifdef _AIX
 #pragma alloca
#else
#ifndef alloca
char *alloca ();
#endif
#endif
#endif
#endif
#endif
"
CL_LINK_CHECK([alloca], cl_cv_func_alloca,
$decl, [char *p = (char *) alloca(1);],
 , [alloca_missing=1])dnl
if test -n "$alloca_missing"; then
  # The SVR3 libPW and SVR4 libucb both contain incompatible functions
  # that cause trouble.  Some versions do not even contain alloca or
  # contain a buggy version.  If you still want to use their alloca,
  # use ar to extract alloca.o from them instead of compiling alloca.c.
  ALLOCA=alloca.${ac_objext}
  AC_DEFINE(NO_ALLOCA)
fi
AC_SUBST(ALLOCA)dnl
])dnl
dnl
AC_DEFUN(CL_SETJMP,
[AC_CHECK_FUNC(_setjmp, , no__jmp=1)dnl
if test -z "$no__jmp"; then
AC_CHECK_FUNC(_longjmp, , no__jmp=1)dnl
fi
if test -z "$no__jmp"; then
AC_DEFINE(HAVE__JMP)
fi
AC_EGREP_HEADER([void.* longjmp], setjmp.h, , AC_DEFINE(LONGJMP_RETURNS))
])dnl
dnl
AC_DEFUN(RL_RETSIGTYPE,
[AC_MSG_CHECKING(return type of signal handlers)
CL_PROTO_RET(
[#include <sys/types.h>
#include <signal.h>
#ifdef signal
#undef signal
#endif
], [int (*signal ()) ();], cl_cv_proto_signal_ret, int, void)
AC_MSG_RESULT($cl_cv_proto_signal_ret)
AC_DEFINE_UNQUOTED(RETSIGTYPE,$cl_cv_proto_signal_ret)
if test $cl_cv_proto_signal_ret = void; then
  AC_DEFINE(RETSIGTYPE_VOID)
fi
])dnl
dnl
AC_DEFUN(CL_TYPE_SIGNAL,
[AC_CACHE_CHECK([return type of signal handlers], cl_cv_type_signal,
[AC_TRY_COMPILE([#include <sys/types.h>
#include <signal.h>
#ifdef signal
#undef signal
#endif
extern
#ifdef __cplusplus
"C" void (*signal (int, void (*)(int)))(int);
#else
void (*signal ()) ();
#endif
],
[], cl_cv_type_signal=void, [
AC_TRY_COMPILE([#include <sys/types.h>
#include <signal.h>
#ifdef signal
#undef signal
#endif
extern
#ifdef __cplusplus
"C" void (*signal (...))(...);
#else
void (*signal ()) ();
#endif
],
[], cl_cv_type_signal=void, cl_cv_type_signal=int)])])
AC_DEFINE_UNQUOTED(RETSIGTYPE, $cl_cv_type_signal)
AC_CACHE_CHECK([whether the signal handler function type needs dots], cl_cv_proto_signal_dots,
[AC_TRY_COMPILE([#include <sys/types.h>
#include <signal.h>
#ifdef signal
#undef signal
#endif
extern
#ifdef __cplusplus
"C" $cl_cv_type_signal (*signal (int, $cl_cv_type_signal (*)(int)))(int);
#else
$cl_cv_type_signal (*signal ()) ();
#endif
],
[], cl_cv_proto_signal_dots=no, cl_cv_proto_signal_dots=yes)])
if test $cl_cv_proto_signal_dots = yes; then
AC_DEFINE(SIGTYPE_DOTS)
fi
])dnl
dnl
AC_DEFUN(CL_SIGNALBLOCK,
[AC_BEFORE([$0], [CL_SIGNAL_UNBLOCK])dnl
AC_BEFORE([$0], [CL_SIGNAL_BLOCK_OTHERS])dnl
AC_BEFORE([$0], [CL_SIGPROCMASK])dnl
signalblocks=""
AC_CHECK_FUNC(sighold, AC_DEFINE(SIGNALBLOCK_SYSV)
signalblocks="$signalblocks SystemV", )dnl
AC_EGREP_HEADER(sigset_t, signal.h, , signals_not_posix=1)dnl
if test -z "$signals_not_posix"; then
AC_CHECK_FUNC(sigprocmask, AC_DEFINE(SIGNALBLOCK_POSIX)
signalblocks="$signalblocks POSIX", )dnl
fi
AC_CHECK_FUNC(sigblock, AC_DEFINE(SIGNALBLOCK_BSD)
signalblocks="$signalblocks BSD", )dnl
AC_CACHE_CHECK(for signal blocking interfaces, cl_cv_func_signalblocks, [
if test -z "$signalblocks"; then
  cl_cv_func_signalblocks="none"
else
  cl_cv_func_signalblocks=`echo $signalblocks`
fi
])
])dnl
dnl
AC_DEFUN(CL_SIGPROCMASK,
[AC_REQUIRE([CL_SIGNALBLOCK])dnl
case "$signalblocks" in
  *POSIX*)
CL_PROTO([sigprocmask], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
], [int sigprocmask (int how, sigset_t* set, sigset_t* oset);],
[int sigprocmask();],
cl_cv_proto_sigprocmask_arg2)],
[extern int sigprocmask (int, $cl_cv_proto_sigprocmask_arg2 sigset_t*, sigset_t*);])
AC_DEFINE_UNQUOTED(SIGPROCMASK_CONST,$cl_cv_proto_sigprocmask_arg2)
  ;;
  *) ;;
esac
])dnl
dnl
AC_DEFUN(CL_SIGNAL_REINSTALL,
[AC_BEFORE([$0], [CL_SIGNAL_UNBLOCK])dnl
AC_BEFORE([$0], [CL_SIGNAL_BLOCK_OTHERS])dnl
AC_CACHE_CHECK(whether signal handlers need to be reinstalled, cl_cv_func_signal_reinstall, [
AC_TRY_RUN([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __CYGWIN32__
/* On Cygwin32 version 18, this test would hang (SIGALRM not being signalled).
 * Let it fail instead. */
#error "better fail than hang"
#endif
/* volatile */ int gotsig=0;
RETSIGTYPE sigalrm_handler() { gotsig=1; }
int got_sig () { return gotsig; }
#ifdef __cplusplus
#ifdef SIGTYPE_DOTS
typedef RETSIGTYPE (*signal_handler) (...);
#else
typedef RETSIGTYPE (*signal_handler) (int);
#endif
#else
typedef RETSIGTYPE (*signal_handler) ();
#endif
int main() { /* returns 0 if they need not to be reinstalled */
  signal(SIGALRM,(signal_handler)sigalrm_handler); alarm(1); while (!got_sig());
  exit(!( (signal_handler)signal(SIGALRM,(signal_handler)sigalrm_handler)
          == (signal_handler)sigalrm_handler
      ) );
}], cl_cv_func_signal_reinstall=no, cl_cv_func_signal_reinstall=yes,
dnl When cross-compiling, don't assume anything.
cl_cv_func_signal_reinstall="guessing yes")
])
case "$cl_cv_func_signal_reinstall" in
  *yes) AC_DEFINE(SIGNAL_NEED_REINSTALL) ;;
  *no) ;;
esac
])dnl
dnl
AC_DEFUN(CL_SIGNAL_UNBLOCK,
[AC_REQUIRE([CL_SIGNAL_REINSTALL])AC_REQUIRE([CL_SIGNALBLOCK])dnl
case "$signalblocks" in
  *POSIX* | *BSD*)
AC_CACHE_CHECK(whether signals are blocked when signal handlers are entered, cl_cv_func_signal_blocked, [
AC_TRY_RUN([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __CYGWIN32__
/* On Cygwin32 version 18, this test would hang (SIGALRM not being signalled).
 * Let it fail instead. */
#error "better fail than hang"
#endif
int gotsig=0;
int wasblocked=0;
#ifdef __cplusplus
#ifdef SIGTYPE_DOTS
typedef RETSIGTYPE (*signal_handler) (...);
#else
typedef RETSIGTYPE (*signal_handler) (int);
#endif
#else
typedef RETSIGTYPE (*signal_handler) ();
#endif
RETSIGTYPE sigalrm_handler()
{ gotsig=1;
#ifdef SIGNAL_NEED_REINSTALL
  signal(SIGALRM,(signal_handler)sigalrm_handler);
#endif
#ifdef SIGNALBLOCK_POSIX
  { sigset_t blocked;
    sigprocmask(SIG_BLOCK, (sigset_t *) 0, &blocked);
    wasblocked = sigismember(&blocked,SIGALRM) ? 1 : 0;
  }
#else
  wasblocked = ((sigblock(0) & sigmask(SIGALRM)) != 0);
#endif
}
int got_sig () { return gotsig; }
int main() { /* returns 0 if they need not to be unblocked */
  signal(SIGALRM,(signal_handler)sigalrm_handler); alarm(1); while (!got_sig());
  exit(wasblocked);
}], cl_cv_func_signal_blocked=no, cl_cv_func_signal_blocked=yes,
dnl When cross-compiling, assume the worst case.
cl_cv_func_signal_blocked="guessing yes")
])
case "$cl_cv_func_signal_blocked" in
  *yes) AC_DEFINE(SIGNAL_NEED_UNBLOCK) ;;
  *no) ;;
esac
  ;;
  *) ;;
esac
])dnl
dnl
AC_DEFUN(CL_SIGNAL_BLOCK_OTHERS,
[AC_REQUIRE([CL_SIGNAL_REINSTALL])AC_REQUIRE([CL_SIGNALBLOCK])dnl
case "$signalblocks" in
  *POSIX* | *BSD*)
AC_CACHE_CHECK(whether other signals are blocked when signal handlers are entered, cl_cv_func_signal_blocked_others, [
AC_TRY_RUN([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __CYGWIN32__
/* On Cygwin32 version 18, this test would hang (SIGALRM not being signalled).
 * Let it fail instead. */
#error "better fail than hang"
#endif
int gotsig=0;
int somewereblocked=0;
#ifdef __cplusplus
#ifdef SIGTYPE_DOTS
typedef RETSIGTYPE (*signal_handler) (...);
#else
typedef RETSIGTYPE (*signal_handler) (int);
#endif
#else
typedef RETSIGTYPE (*signal_handler) ();
#endif
RETSIGTYPE sigalrm_handler()
{ gotsig=1;
#ifdef SIGNAL_NEED_REINSTALL
  signal(SIGALRM,(signal_handler)sigalrm_handler);
#endif
#ifdef SIGNALBLOCK_POSIX
  { sigset_t blocked;
    int i;
    sigprocmask(SIG_BLOCK, (sigset_t *) 0, &blocked);
    for (i=1; i<32; i++)
      if (i!=SIGALRM && sigismember(&blocked,i))
        somewereblocked = 1;
  }
#else
  somewereblocked = ((sigblock(0) & ~sigmask(SIGALRM)) != 0);
#endif
}
int got_sig () { return gotsig; }
int main() { /* returns 0 if they need not to be unblocked */
  signal(SIGALRM,(signal_handler)sigalrm_handler); alarm(1); while (!got_sig());
  exit(somewereblocked);
}], cl_cv_func_signal_blocked_others=no, cl_cv_func_signal_blocked_others=yes,
dnl When cross-compiling, assume the worst case.
cl_cv_func_signal_blocked_others="guessing yes")
])
case "$cl_cv_func_signal_blocked_others" in
  *yes) AC_DEFINE(SIGNAL_NEED_UNBLOCK_OTHERS) ;;
  *no) ;;
esac
  ;;
  *) ;;
esac
])dnl
dnl
AC_DEFUN(CL_SIGACTION,
[AC_BEFORE([$0], [CL_SIGACTION_REINSTALL])
AC_BEFORE([$0], [CL_SIGINTERRUPT])
AC_CHECK_FUNC(sigaction, AC_DEFINE(HAVE_SIGACTION)
have_sigaction=1)])dnl
dnl
AC_DEFUN(CL_SIGACTION_REINSTALL,
[AC_REQUIRE([CL_TYPE_SIGNAL])dnl
AC_REQUIRE([CL_SIGACTION])dnl
AC_REQUIRE([CL_MEMSET])dnl
AC_BEFORE([$0], [CL_SIGACTION_UNBLOCK])dnl
if test -n "$have_sigaction"; then
AC_CACHE_CHECK(whether sigaction handlers need to be reinstalled, cl_cv_func_sigaction_reinstall, [
AC_TRY_RUN([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __CYGWIN32__
/* On Cygwin32 version 18, this test would hang (SIGALRM not being signalled).
 * Let it fail instead. */
#error "better fail than hang"
#endif
#ifdef __cplusplus
#ifdef SIGTYPE_DOTS
typedef RETSIGTYPE (*signal_handler) (...);
#else
typedef RETSIGTYPE (*signal_handler) (int);
#endif
#else
typedef RETSIGTYPE (*signal_handler) ();
#endif
#if defined(__STDC__) || defined(__cplusplus)
signal_handler mysignal (int sig, signal_handler handler)
#else
signal_handler mysignal (sig, handler)
     int sig;
     signal_handler handler;
#endif
{ struct sigaction old_sa;
  struct sigaction new_sa;
#ifdef HAVE_MEMSET
  memset(&new_sa,0,sizeof(new_sa));
#else
  bzero(&new_sa,sizeof(new_sa));
#endif
  new_sa.sa_handler = handler;
  if (sigaction(sig,&new_sa,&old_sa)<0) { return (signal_handler)SIG_IGN; }
  return (signal_handler)old_sa.sa_handler;
}
/* volatile */ int gotsig=0;
RETSIGTYPE sigalrm_handler() { gotsig=1; }
int got_sig () { return gotsig; }
int main() { /* returns 0 if they need not to be reinstalled */
  mysignal(SIGALRM,(signal_handler)sigalrm_handler); alarm(1); while (!got_sig());
  exit(!( mysignal(SIGALRM,(signal_handler)sigalrm_handler)
          == (signal_handler)sigalrm_handler
      ) );
}], cl_cv_func_sigaction_reinstall=no, cl_cv_func_sigaction_reinstall=yes,
dnl When cross-compiling, don't assume anything.
cl_cv_func_sigaction_reinstall="guessing yes")
])
case "$cl_cv_func_sigaction_reinstall" in
  *yes) AC_DEFINE(SIGACTION_NEED_REINSTALL) ;;
  *no) ;;
esac
fi
])dnl
dnl
AC_DEFUN(CL_SIGACTION_UNBLOCK,
[AC_REQUIRE([CL_TYPE_SIGNAL])dnl
AC_REQUIRE([CL_SIGACTION])dnl
AC_REQUIRE([CL_MEMSET])dnl
AC_REQUIRE([CL_SIGACTION_REINSTALL])dnl
AC_REQUIRE([CL_SIGNALBLOCK])dnl
if test -n "$have_sigaction"; then
case "$signalblocks" in
  *POSIX* | *BSD*)
AC_CACHE_CHECK(whether signals are blocked when sigaction handlers are entered, cl_cv_func_sigaction_blocked, [
AC_TRY_RUN([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __CYGWIN32__
/* On Cygwin32 version 18, this test would hang (SIGALRM not being signalled).
 * Let it fail instead. */
#error "better fail than hang"
#endif
#ifdef __cplusplus
#ifdef SIGTYPE_DOTS
typedef RETSIGTYPE (*signal_handler) (...);
#else
typedef RETSIGTYPE (*signal_handler) (int);
#endif
#else
typedef RETSIGTYPE (*signal_handler) ();
#endif
#if defined(__STDC__) || defined(__cplusplus)
signal_handler mysignal (int sig, signal_handler handler)
#else
signal_handler mysignal (sig, handler)
     int sig;
     signal_handler handler;
#endif
{ struct sigaction old_sa;
  struct sigaction new_sa;
#ifdef HAVE_MEMSET
  memset(&new_sa,0,sizeof(new_sa));
#else
  bzero(&new_sa,sizeof(new_sa));
#endif
  new_sa.sa_handler = handler;
  if (sigaction(sig,&new_sa,&old_sa)<0) { return (signal_handler)SIG_IGN; }
  return (signal_handler)old_sa.sa_handler;
}
int gotsig=0;
int wasblocked=0;
RETSIGTYPE sigalrm_handler()
{ gotsig=1;
#ifdef SIGNAL_NEED_REINSTALL
  mysignal(SIGALRM,(signal_handler)sigalrm_handler);
#endif
#ifdef SIGNALBLOCK_POSIX
  { sigset_t blocked;
    sigprocmask(SIG_BLOCK, (sigset_t *) 0, &blocked);
    wasblocked = sigismember(&blocked,SIGALRM) ? 1 : 0;
  }
#else
  wasblocked = ((sigblock(0) & sigmask(SIGALRM)) != 0);
#endif
}
int got_sig () { return gotsig; }
int main() { /* returns 0 if they need not to be unblocked */
  mysignal(SIGALRM,(signal_handler)sigalrm_handler); alarm(1); while (!got_sig());
  exit(wasblocked);
}], cl_cv_func_sigaction_blocked=no, cl_cv_func_sigaction_blocked=yes,
dnl When cross-compiling, assume the worst case.
cl_cv_func_sigaction_blocked="guessing yes")
])
case "$cl_cv_func_sigaction_blocked" in
  *yes) AC_DEFINE(SIGACTION_NEED_UNBLOCK) ;;
  *no) ;;
esac
  ;;
  *) ;;
esac
fi
])dnl
dnl
AC_DEFUN(CL_SIGINTERRUPT,
[AC_REQUIRE([CL_SIGACTION])dnl
AC_CHECK_FUNC(siginterrupt, AC_DEFINE(HAVE_SIGINTERRUPT)
have_siginterrupt=1)dnl
if test -z "$have_siginterrupt" -a -z "$have_sigaction"; then
AC_CHECK_FUNCS(sigvec)dnl
fi
])dnl
dnl
AC_DEFUN(CL_SIGALTSTACK,
[AC_REQUIRE([CL_SIGACTION])dnl
CL_LINK_CHECK(sigaltstack, cl_cv_func_sigaltstack,
[#include <signal.h>],
[int x = SA_ONSTACK; stack_t ss; sigaltstack((stack_t*)0,&ss);],
AC_DEFINE(HAVE_SIGALTSTACK))dnl
])dnl
dnl
AC_DEFUN(CL_FPU_CONTROL,
[dnl Check for Linux with <fpu_control.h> and fpu_control_t or __setfpucw().
dnl glibc versions since October 1998 define fpu_control_t. Earlier versions
dnl define and declare __setfpucw(). Very early Linux libc versions have none,
dnl and __fpu_control is of type `unsigned short'.
CL_COMPILE_CHECK([fpu_control_t], cl_cv_type_fpu_control_t,
[#include <fpu_control.h>], [fpu_control_t x;],
AC_DEFINE(HAVE_FPU_CONTROL_T))
CL_COMPILE_CHECK([__setfpucw], cl_cv_func_setfpucw,
[#include <fpu_control.h>], [__setfpucw(_FPU_IEEE);],
AC_DEFINE(HAVE_SETFPUCW))
])dnl
dnl
AC_DEFUN(CL_RAISE,
[CL_LINK_CHECK([raise], cv_cv_func_raise,
[#include <sys/types.h>
#include <signal.h>], [raise(6);],
AC_DEFINE(HAVE_RAISE)dnl
)])dnl
dnl
AC_DEFUN(CL_ABORT,
[AC_REQUIRE([CL_STDLIB_H])dnl
CL_PROTO([abort], [
CL_PROTO_RET([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [int abort();], cl_cv_proto_abort_ret, int, void)
CL_PROTO_RET([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [$cl_cv_proto_abort_ret abort();], cl_cv_proto_abort_vol, [], [__volatile__])
], [extern $cl_cv_proto_abort_vol $cl_cv_proto_abort_ret abort (void);])
AC_DEFINE_UNQUOTED(RETABORTTYPE,$cl_cv_proto_abort_ret)
AC_DEFINE_UNQUOTED(ABORT_VOLATILE,$cl_cv_proto_abort_vol)
])dnl
dnl
AC_DEFUN(CL_PERROR,
[AC_MSG_CHECKING(for perror declaration)
AC_CACHE_VAL(cl_cv_proto_perror, [
AC_TRY_COMPILE([
/* Some systems declare perror() in <errno.h>, some in <stdio.h>, some don't
   declare it at all. */
#include <stdio.h>
#include <errno.h>
]AC_LANG_EXTERN[double perror ();], [],
cl_cv_proto_perror=no, cl_cv_proto_perror=yes)
])
AC_MSG_RESULT([$cl_cv_proto_perror])
if test $cl_cv_proto_perror = yes; then
AC_DEFINE(HAVE_PERROR_DECL)
fi
])dnl
dnl
AC_DEFUN(CL_SYS_ERRLIST,
[changequote(,)dnl
brackets='[]'
changequote([,])dnl
CL_PROTO([sys_errlist], [
AC_TRY_COMPILE([
/* Most systems declare sys_errlist in <errno.h>, NetBSD 1.0 in <stdio.h>. */
#include <stdio.h>
#include <errno.h>
extern char* sys_errlist $brackets ;
], [], cl_cv_proto_sys_errlist_const="", cl_cv_proto_sys_errlist_const="const")
], [extern $cl_cv_proto_sys_errlist_const char* $cl_cv_proto_sys_errlist_const sys_errlist$brackets;])
AC_DEFINE_UNQUOTED(SYS_ERRLIST_CONST,$cl_cv_proto_sys_errlist_const)
])dnl
dnl
AC_DEFUN(CL_GETENV,
[CL_PROTO([getenv], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [char* getenv (char* name);], [char* getenv();], cl_cv_proto_getenv_arg1)
], [extern char* getenv ($cl_cv_proto_getenv_arg1 char*);])
AC_DEFINE_UNQUOTED(GETENV_CONST,$cl_cv_proto_getenv_arg1)
])dnl
dnl
AC_DEFUN(CL_PUTENV,
[AC_CHECK_FUNCS(putenv)dnl
if test $ac_cv_func_putenv = yes; then
CL_PROTO([putenv], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [int putenv (char* name);], [int putenv();], cl_cv_proto_putenv_arg1)
], [extern int putenv ($cl_cv_proto_putenv_arg1 char*);])
AC_DEFINE_UNQUOTED(PUTENV_CONST,$cl_cv_proto_putenv_arg1)
else
AC_CHECK_FUNCS(setenv)dnl
fi
])dnl
dnl
AC_DEFUN(CL_SETLOCALE,
[CL_PROTO([setlocale], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
], [char* setlocale (int category, char* locale);], [char* setlocale();],
cl_cv_proto_setlocale_arg1)
], [extern char* setlocale (int, $cl_cv_proto_setlocale_arg1 char*);])
AC_DEFINE_UNQUOTED(SETLOCALE_CONST,$cl_cv_proto_setlocale_arg1)
])dnl
dnl
AC_DEFUN(CL_RLIMIT,
[AC_CHECK_FUNCS(setrlimit)dnl
if test $ac_cv_func_setrlimit = yes; then
CL_PROTO([getrlimit], [
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
],
[int getrlimit (enum __rlimit_resource resource, struct rlimit * rlim);],
[int getrlimit();],
[cl_cv_proto_getrlimit_arg1="enum __rlimit_resource"],
[cl_cv_proto_getrlimit_arg1="int"])
], [extern int getrlimit ($cl_cv_proto_getrlimit_arg1, struct rlimit *);])
AC_DEFINE_UNQUOTED(RLIMIT_RESOURCE_T,$cl_cv_proto_getrlimit_arg1)
CL_PROTO([setrlimit], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
], [int setrlimit (RLIMIT_RESOURCE_T resource, struct rlimit * rlim);],
[int setrlimit();],
cl_cv_proto_setrlimit_arg2)
], [extern int setrlimit ($cl_cv_proto_getrlimit_arg1, $cl_cv_proto_setrlimit_arg2 struct rlimit *);])
AC_DEFINE_UNQUOTED(SETRLIMIT_CONST,$cl_cv_proto_setrlimit_arg2)
fi
])dnl
dnl
AC_DEFUN(CL_VFORK,
[CL_PROTO([vfork], [
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif
], [pid_t vfork (void);], [pid_t vfork();],
cl_cv_proto_vfork_ret="pid_t", cl_cv_proto_vfork_ret="int")
], [extern $cl_cv_proto_vfork_ret vfork (void);])
AC_DEFINE_UNQUOTED(RETVFORKTYPE,$cl_cv_proto_vfork_ret)
])dnl
dnl
AC_DEFUN(CL_SETSID,
[AC_CHECK_FUNCS(setsid setpgid)])dnl
dnl
AC_DEFUN(CL_EXECV,
[changequote(,)dnl
brackets='[]'
changequote([,])dnl
CL_PROTO([execv], [
for z in '' 'const'; do
for y in '' 'const'; do
for x in '' 'const'; do
if test -z "$have_execv"; then
AC_TRY_COMPILE([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int execv ($x char* path, $y char* $z argv$brackets);
#else
int execv();
#endif
], [],
cl_cv_proto_execv_arg1="$x"
cl_cv_proto_execv_arg2a="$y"
cl_cv_proto_execv_arg2b="$z"
have_execv=1)
fi
done
done
done
], [extern int execv ($cl_cv_proto_execv_arg1 char*, $cl_cv_proto_execv_arg2a char* $cl_cv_proto_execv_arg2b$brackets);])
AC_DEFINE_UNQUOTED(EXECV_CONST,$cl_cv_proto_execv_arg1)
AC_DEFINE_UNQUOTED(EXECV1_CONST,$cl_cv_proto_execv_arg2a)
AC_DEFINE_UNQUOTED(EXECV2_CONST,$cl_cv_proto_execv_arg2b)
])dnl
dnl
AC_DEFUN(CL_EXECL,
[CL_PROTO([execl], [
for x in '' 'const'; do
if test -z "$have_execl"; then
AC_TRY_COMPILE([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int execl (EXECV_CONST char* path, $x char* arg, ...);
#else
int execl();
#endif
], [], [
cl_cv_proto_execl_arg2="$x"
cl_cv_proto_execl_dots=yes
cl_cv_proto_execl_args="$cl_cv_proto_execv_arg1 char*, $cl_cv_proto_execl_arg2 char*, ..."
have_execl=1])
fi
done
for x in '' 'const'; do
if test -z "$have_execl"; then
AC_TRY_COMPILE([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int execl (EXECV_CONST char* path, $x char* arg0, $x char* arg1, $x char* arg2, $x char* arg3);
#else
int execl();
#endif
], [], [
cl_cv_proto_execl_arg2="$x"
cl_cv_proto_execl_dots=no
cl_cv_proto_execl_args="$cl_cv_proto_execv_arg1 char*, $cl_cv_proto_execl_arg2 char*, $cl_cv_proto_execl_arg2 char*, $cl_cv_proto_execl_arg2 char*, $cl_cv_proto_execl_arg2 char*"
have_execl=1])
fi
done
], [extern int execl ($cl_cv_proto_execl_args);])
AC_DEFINE_UNQUOTED(EXECL_CONST,$cl_cv_proto_execl_arg2)
if test $cl_cv_proto_execl_dots = yes; then
  AC_DEFINE(EXECL_DOTS)
fi
])dnl
dnl
AC_DEFUN(CL_WAITPID,
[AC_CHECK_FUNCS(waitpid)
if test $ac_cv_func_waitpid = yes; then
CL_PROTO([waitpid], [
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
], [pid_t waitpid (pid_t pid, int* statusp, int options);],
[pid_t waitpid();],
cl_cv_proto_waitpid_arg1="pid_t", cl_cv_proto_waitpid_arg1="int")
], [extern pid_t waitpid ($cl_cv_proto_waitpid_arg1, int*, int);])
AC_DEFINE_UNQUOTED(PID_T,$cl_cv_proto_waitpid_arg1)
else
AC_CHECK_HEADERS(sys/wait.h)dnl
fi
])dnl
dnl
AC_DEFUN(CL_RUSAGE,
[AC_CHECK_HEADERS(sys/resource.h sys/times.h)dnl
if test $ac_cv_header_sys_resource_h = yes; then
  dnl HAVE_SYS_RESOURCE_H defined
  CL_LINK_CHECK([getrusage], cl_cv_func_getrusage,
[#include <sys/types.h> /* NetBSD 1.0 needs this */
#include <sys/time.h>
#include <sys/resource.h>],
    [struct rusage x; int y = RUSAGE_SELF; getrusage(y,&x); x.ru_utime.tv_sec;],
    AC_DEFINE(HAVE_GETRUSAGE))dnl
  if test $cl_cv_func_getrusage = yes; then
    CL_PROTO([getrusage], [
    CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h> /* NetBSD 1.0 needs this */
#include <sys/time.h>
#include <sys/resource.h>
],
[int getrusage (int who, struct rusage * rusage);],
[int getrusage();],
[cl_cv_proto_getrusage_arg1="int"],
[cl_cv_proto_getrusage_arg1="enum __rusage_who"])
], [extern int getrusage ($cl_cv_proto_getrusage_arg1, struct rusage *);])
    AC_DEFINE_UNQUOTED(RUSAGE_WHO_T,$cl_cv_proto_getrusage_arg1)
  fi
fi
])dnl
dnl
AC_DEFUN(CL_GETCWD,
[CL_LINK_CHECK([getcwd], cl_cv_func_getcwd, [
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
], [getcwd((char*)0,1024);], AC_DEFINE(HAVE_GETCWD),)
if test $cl_cv_func_getcwd = yes; then
CL_PROTO([getcwd], [
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [char* getcwd (char* buf, int bufsize);], [char* getcwd();],
cl_cv_proto_getcwd_arg2="int", cl_cv_proto_getcwd_arg2="size_t")
], [extern char* getcwd (char*, $cl_cv_proto_getcwd_arg2);])
AC_DEFINE_UNQUOTED(GETCWD_SIZE_T,$cl_cv_proto_getcwd_arg2)
fi
])dnl
dnl
AC_DEFUN(CL_CHDIR,
[AC_BEFORE([$0], [CL_FILECHARSET])dnl
CL_PROTO([chdir], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [int chdir (char* path);], [int chdir();], cl_cv_proto_chdir_arg1)
], [extern int chdir ($cl_cv_proto_chdir_arg1 char*);])
AC_DEFINE_UNQUOTED(CHDIR_CONST,$cl_cv_proto_chdir_arg1)
])dnl
dnl
AC_DEFUN(CL_MKDIR,
[AC_BEFORE([$0], [CL_OPEN])
CL_PROTO([mkdir], [
AC_EGREP_HEADER(mode_t, sys/types.h,
dnl mode_t defined. check if it is really used by mkdir() :
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
], [int mkdir (char* path, mode_t mode);], [int mkdir();], mode_t_unneeded=1, )
if test -z "$mode_t_unneeded"; then
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
], [int mkdir (const char* path, mode_t mode);], [int mkdir();], mode_t_unneeded=1, )
fi)dnl
if test -n "$mode_t_unneeded"; then
cl_cv_type_mode_t="mode_t"
else
cl_cv_type_mode_t="int"
fi
dnl Now MODE_T should be correct, check for const:
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
], [int mkdir (char* path, $cl_cv_type_mode_t mode);], [int mkdir();], cl_cv_proto_mkdir_arg1)
], [extern int mkdir ($cl_cv_proto_mkdir_arg1 char*, $cl_cv_type_mode_t);])
AC_DEFINE_UNQUOTED(MODE_T,$cl_cv_type_mode_t)
AC_DEFINE_UNQUOTED(MKDIR_CONST,$cl_cv_proto_mkdir_arg1)
])dnl
dnl
AC_DEFUN(CL_RMDIR,
[CL_PROTO([rmdir], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [int rmdir (char* path);], [int rmdir();], cl_cv_proto_rmdir_arg1)
], [extern int rmdir ($cl_cv_proto_rmdir_arg1 char*);])
AC_DEFINE_UNQUOTED(RMDIR_CONST,$cl_cv_proto_rmdir_arg1)
])dnl
dnl
AC_DEFUN(CL_FSTAT,
[AC_BEFORE([$0], [CL_STAT])dnl
AC_BEFORE([$0], [CL_LSTAT])dnl
dnl Must use AC_TRY_LINK instead of CL_PROTO because Linux defines fstat()
dnl as an inline function in <sys/stat.h> and libc-5.0.9 doesn't define `fstat',
dnl hence when compiling in C++ mode the declaration in <sys/stat.h> and that
dnl of CL_PROTO may not clash.
CL_PROTO([fstat], [
AC_TRY_LINK([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
]
AC_LANG_EXTERN
[#if defined(__STDC__) || defined(__cplusplus)
int fstat (int fd, struct stat * buf);
#else
int fstat();
#endif
], [return fstat(0,(struct stat *)0);],
cl_cv_proto_fstat_inline=no
have_fstat_decl=1)
if test -z "$have_fstat_decl"; then
  # This happens on Linux with g++ and libc-5.0.9.
  cl_cv_proto_fstat_inline=yes
fi
], [extern int fstat (int, struct stat *);])
if test $cl_cv_proto_fstat_inline = yes; then
  AC_DEFINE(FSTAT_INLINE)
fi
])dnl
dnl
AC_DEFUN(CL_STAT,
[AC_REQUIRE([CL_FSTAT])dnl
dnl Must use AC_TRY_LINK instead of CL_PROTO because Linux defines stat()
dnl as an inline function in <sys/stat.h> and libc-5.0.9 doesn't define `stat',
dnl hence when compiling in C++ mode the declaration in <sys/stat.h> and that
dnl of CL_PROTO may not clash.
CL_PROTO([stat], [
dnl Just assume that `stat' is defined as an inline function if and only if
dnl `fstat' is. We do this because if it is, in C++ mode, exactly one of the two
dnl trial declarations `extern "C" int stat ([const] char *, struct stat *)'
dnl will give an error, and the other will or will not link correctly (depends
dnl on libc: will with libc-5.2.18, will not with libc-5.0.9).
cl_cv_proto_stat_inline=$cl_cv_proto_fstat_inline
if test $cl_cv_proto_stat_inline = yes; then
  AC_DEFINE(STAT_INLINE)
fi
AC_TRY_LINK([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#if defined(__cplusplus) && defined(STAT_INLINE)
extern
#else
]
AC_LANG_EXTERN
[#endif
#if defined(__STDC__) || defined(__cplusplus)
int stat (char* path, struct stat * buf);
#else
int stat();
#endif
], [return stat((char*)"",(struct stat *)0);],
cl_cv_proto_stat_arg1=""
have_stat_decl=1)
if test -z "$have_stat_decl"; then
AC_TRY_LINK([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#if defined(__cplusplus) && defined(STAT_INLINE)
extern
#else
]
AC_LANG_EXTERN
[#endif
#if defined(__STDC__) || defined(__cplusplus)
int stat (const char* path, struct stat * buf);
#else
int stat();
#endif
], [return stat((const char*)"",(struct stat *)0);],
cl_cv_proto_stat_arg1="const"
have_stat_decl=1)
fi
dnl if test -z "$have_stat_decl"; then
dnl   # This happens on Linux with g++ and libc-5.0.9.
dnl   cl_cv_proto_stat_arg1="const"
dnl   cl_cv_proto_stat_inline=yes
dnl fi
], [extern int stat ($cl_cv_proto_stat_arg1 char*, struct stat *);])
AC_DEFINE_UNQUOTED(STAT_CONST,$cl_cv_proto_stat_arg1)
dnl if test $cl_cv_proto_stat_inline = yes; then
dnl   AC_DEFINE(STAT_INLINE)
dnl fi
])dnl
dnl
AC_DEFUN(CL_LSTAT,
[AC_REQUIRE([CL_FSTAT])dnl
dnl Cannot use AC_CHECK_FUNCS(lstat) because Linux defines lstat() as an
dnl inline function in <sys/stat.h>. Must use AC_TRY_LINK instead of CL_PROTO
dnl because Linux defines lstat() as an inline function in <sys/stat.h> and
dnl libc-5.0.9 doesn't define `lstat', hence when compiling in C++ mode
dnl the declaration in <sys/stat.h> and that of CL_PROTO may not clash.
CL_LINK_CHECK([lstat],cl_cv_func_lstat,[
#include <sys/types.h>
#include <sys/stat.h>
], [return lstat("",(struct stat *)0);],
AC_DEFINE(HAVE_LSTAT))dnl
if test $cl_cv_func_lstat = yes; then
CL_PROTO([lstat], [
dnl Just assume that `lstat' is defined as an inline function if and only if
dnl `fstat' is. We do this because if it is, in C++ mode, exactly one of the two
dnl trial declarations `extern "C" int lstat ([const] char *, struct stat *)'
dnl will give an error, and the other will or will not link correctly (depends
dnl on libc: will with libc-5.2.18, will not with libc-5.0.9).
cl_cv_proto_lstat_inline=$cl_cv_proto_fstat_inline
if test $cl_cv_proto_lstat_inline = yes; then
  AC_DEFINE(LSTAT_INLINE)
fi
AC_TRY_LINK([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#if defined(__cplusplus) && defined(LSTAT_INLINE)
extern
#else
]
AC_LANG_EXTERN
[#endif
#if defined(__STDC__) || defined(__cplusplus)
int lstat (char* path, struct stat * buf);
#else
int lstat();
#endif
], [return lstat((char*)"",(struct stat *)0);],
cl_cv_proto_lstat_arg1=""
have_lstat_decl=1)
if test -z "$have_lstat_decl"; then
AC_TRY_LINK([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#if defined(__cplusplus) && defined(STAT_INLINE)
extern
#else
]
AC_LANG_EXTERN
[#endif
#if defined(__STDC__) || defined(__cplusplus)
int lstat (const char* path, struct stat * buf);
#else
int lstat();
#endif
], [return lstat((const char*)"",(struct stat *)0);],
cl_cv_proto_lstat_arg1="const"
have_lstat_decl=1)
fi
dnl if test -z "$have_lstat_decl"; then
dnl   # This happens on Linux with g++ and libc-5.0.9.
dnl   cl_cv_proto_lstat_arg1="const"
dnl   cl_cv_proto_lstat_inline=yes
dnl fi
], [extern int lstat ($cl_cv_proto_lstat_arg1 char*, struct stat *);])
AC_DEFINE_UNQUOTED(LSTAT_CONST,$cl_cv_proto_lstat_arg1)
dnl if test $cl_cv_proto_lstat_inline = yes; then
dnl   AC_DEFINE(LSTAT_INLINE)
dnl fi
fi
])dnl
dnl
AC_DEFUN(CL_READLINK,
[AC_CHECK_FUNCS(readlink)dnl
if test $ac_cv_func_readlink = yes; then
CL_PROTO([readlink], [
for z in 'int' 'size_t'; do
for y in 'char*' 'void*'; do
for x in '' 'const'; do
if test -z "$have_readlink"; then
AC_TRY_COMPILE([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int readlink ($x char* path, $y buf, $z bufsiz);
#else
int readlink();
#endif
], [],
cl_cv_proto_readlink_arg1="$x"
cl_cv_proto_readlink_arg2="$y"
cl_cv_proto_readlink_arg3="$z"
have_readlink=1)
fi
done
done
done
], [extern int readlink ($cl_cv_proto_readlink_arg1 char*, $cl_cv_proto_readlink_arg2, $cl_cv_proto_readlink_arg3);])
AC_DEFINE_UNQUOTED(READLINK_CONST,$cl_cv_proto_readlink_arg1)
AC_DEFINE_UNQUOTED(READLINK_BUF_T,$cl_cv_proto_readlink_arg2)
AC_DEFINE_UNQUOTED(READLINK_SIZE_T,$cl_cv_proto_readlink_arg3)
fi
])dnl
dnl
AC_DEFUN(CL_ELOOP,
[AC_REQUIRE([AC_PROG_CC])dnl
AC_CACHE_CHECK(for ELOOP, cl_cv_decl_eloop, [
if test $cross_compiling = no; then
cat > conftest.c <<EOF
#include "confdefs.h"
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <stdio.h>
#ifdef ELOOP
int main () { printf("ELOOP\n"); exit(0); }
#else
extern int errno;
#define foo "conflink"
#define foobar "conflink/somefile"
int main()
{ /* If a system goes into an endless loop on this, it must be really broken. */
  if (symlink(foo,foo)<0) exit(1);
  if (unlink(foobar)>=0) { unlink(foo); exit(1); }
  printf("%d\n",errno); unlink(foo); exit(0);
}
#endif
EOF
AC_TRY_EVAL(ac_link)
cl_cv_decl_ELOOP=`./conftest`
if test "$cl_cv_decl_ELOOP" = "ELOOP"; then
  cl_cv_decl_eloop=yes
else
  cl_cv_decl_eloop="$cl_cv_decl_ELOOP"
fi
else
AC_EGREP_CPP(yes,[
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <stdio.h>
#ifdef ELOOP
yes
#endif
],
cl_cv_decl_eloop=yes,
cl_cv_decl_eloop=no)
cl_cv_decl_ELOOP="ELOOP"
fi
rm -f conftest*
])
AC_DEFINE_UNQUOTED(ELOOP_VALUE,$cl_cv_decl_ELOOP)
])dnl
dnl
AC_DEFUN(CL_OPENDIR,
[AC_REQUIRE([CL_DIR_HEADER])dnl
AC_BEFORE([$0], [CL_CLOSEDIR])dnl
AC_BEFORE([$0], [CL_FILECHARSET])dnl
CL_PROTO([opendir], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <$ac_header_dirent>
], [DIR* opendir (char* dirname);], [DIR* opendir();], cl_cv_proto_opendir_arg1)
], [extern DIR* opendir ($cl_cv_proto_opendir_arg1 char*);])
AC_DEFINE_UNQUOTED(OPENDIR_CONST,$cl_cv_proto_opendir_arg1)
])dnl
dnl
AC_DEFUN(CL_CLOSEDIR,
[AC_REQUIRE([CL_DIR_HEADER])dnl
AC_REQUIRE([CL_OPENDIR])dnl
AC_BEFORE([$0], [CL_FILECHARSET])dnl
CL_PROTO([closedir], [
CL_PROTO_RET([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#include <$ac_header_dirent>
], [
#if defined(__STDC__) || defined(__cplusplus)
int closedir (DIR* dir);
#else
int closedir();
#endif
], cl_cv_proto_closedir_ret, int, void)],
[extern $cl_cv_proto_closedir_ret closedir (DIR*);])
AC_DEFINE_UNQUOTED(RETCLOSEDIRTYPE,$cl_cv_proto_closedir_ret)
if test $cl_cv_proto_closedir_ret = void; then
  AC_DEFINE(VOID_CLOSEDIR)
else
  # The following test is necessary, because Cygwin32 declares closedir()
  # as returning int but the return value is unusable.
  AC_CACHE_CHECK(for usable closedir return value, cl_cv_func_closedir_retval,[
AC_TRY_RUN([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
/* Declare opendir(), closedir(). */
#include <$ac_header_dirent>
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
DIR* opendir (OPENDIR_CONST char* dirname);
#else
DIR* opendir();
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
RETCLOSEDIRTYPE closedir (DIR* dirp);
#else
RETCLOSEDIRTYPE closedir();
#endif
int main() { exit(closedir(opendir(".")) != 0); }],
cl_cv_func_closedir_retval=yes, cl_cv_func_closedir_retval=no,
# When cross-compiling, don't assume a return value.
cl_cv_func_closedir_retval="guessing no")])
case "$cl_cv_func_closedir_retval" in
  *no) AC_DEFINE(VOID_CLOSEDIR) ;;
esac
fi
])dnl
dnl
AC_DEFUN(CL_OPEN,
[AC_REQUIRE([CL_MKDIR])dnl defines MODE_T
AC_BEFORE([$0], [CL_FILECHARSET])dnl
CL_PROTO([open], [
for y in 'MODE_T mode' '...'; do
for x in '' 'const'; do
if test -z "$have_open"; then
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
], [int open ($x char* path, int flags, $y);], [int open();], [
cl_cv_proto_open_arg1="$x"
if test "$y" = "..."; then
cl_cv_proto_open_dots=yes
else
cl_cv_proto_open_dots=no
fi
have_open=1])
fi
done
done
if test -z "$cl_cv_proto_open_dots"; then
dnl This actually happens with AIX 3.2.5 cc: cc understands prototypes but
dnl does not define __STDC__. The include files contain a declaration
dnl "int open (const char*, int, ...);" which gives an error against
dnl "int open ();". The right solution would be a macro CL_C_PROTOTYPES.
  cl_cv_proto_open_arg1="const"
  cl_cv_proto_open_dots=yes
fi
if test $cl_cv_proto_open_dots = yes; then
cl_cv_proto_open_args="$cl_cv_proto_open_arg1 char*, int, ..."
else
cl_cv_proto_open_args="$cl_cv_proto_open_arg1 char*, int, $cl_cv_type_mode_t"
fi
], [extern int open ($cl_cv_proto_open_args);])
AC_DEFINE_UNQUOTED(OPEN_CONST,$cl_cv_proto_open_arg1)
if test $cl_cv_proto_open_dots = yes; then
AC_DEFINE(OPEN_DOTS)
fi
])dnl
dnl
AC_DEFUN(CL_READ_WRITE,
[CL_PROTO([read], [
for z in 'int' 'size_t' "unsigned $cl_cv_proto_retrwtype" 'off_t'; do
for y in 'char*' 'void*'; do
for x in 'int' 'long'; do
if test -z "$have_rw"; then
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [$x read (int fd, $y buf, $z count);],
[$x read();], [
cl_cv_proto_retrwtype="$x"
cl_cv_proto_rw_buf_t="$y"
cl_cv_proto_rw_size_t="$z"
have_rw=1])
fi
done
done
done
], [extern $cl_cv_proto_retrwtype read (int, $cl_cv_proto_rw_buf_t, $cl_cv_proto_rw_size_t);])
AC_DEFINE_UNQUOTED(RETRWTYPE,$cl_cv_proto_retrwtype)
AC_DEFINE_UNQUOTED(RW_BUF_T,$cl_cv_proto_rw_buf_t)
AC_DEFINE_UNQUOTED(RW_SIZE_T,$cl_cv_proto_rw_size_t)
])dnl
dnl
AC_DEFUN(CL_WRITE,
[CL_PROTO([write], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [$cl_cv_proto_retrwtype write (int fd, $cl_cv_proto_rw_buf_t buf, $cl_cv_proto_rw_size_t count);],
[$cl_cv_proto_retrwtype write();], cl_cv_proto_write_arg2)
], [extern $cl_cv_proto_retrwtype write (int, $cl_cv_proto_write_arg2 $cl_cv_proto_rw_buf_t, $cl_cv_proto_rw_size_t);])
AC_DEFINE_UNQUOTED(WRITE_CONST,$cl_cv_proto_write_arg2)
])dnl
dnl
dnl AC_DEFUN(CL_CHMOD,
dnl [CL_PROTO([chmod], [
dnl CL_PROTO_CONST([
dnl #if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
dnl #include <stdlib.h>
dnl #endif
dnl #ifdef HAVE_UNISTD_H
dnl #include <unistd.h>
dnl #endif
dnl #include <sys/types.h>
dnl #include <sys/stat.h>
dnl ], [int chmod (char* path, mode_t mode);], [int chmod();],
dnl cl_cv_proto_chmod_arg1)
dnl ], [extern int chmod ($cl_cv_proto_chmod_arg1 char*, mode_t);])
dnl AC_DEFINE_UNQUOTED(CHMOD_CONST,$cl_cv_proto_chmod_arg1)
dnl AC_CHECK_FUNCS(fchmod)
dnl ])dnl
dnl dnl
AC_DEFUN(CL_RENAME,
[AC_CHECK_FUNCS(rename)dnl
if test $ac_cv_func_rename = yes; then
CL_PROTO([rename], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
], [int rename (char* oldpath, char* newpath);], [int rename();],
cl_cv_proto_rename_arg1)
], [extern int rename ($cl_cv_proto_rename_arg1 char*, $cl_cv_proto_rename_arg1 char*);])
AC_DEFINE_UNQUOTED(RENAME_CONST,$cl_cv_proto_rename_arg1)
fi
])dnl
dnl
AC_DEFUN(CL_UNLINK,
[AC_BEFORE([$0], [CL_FILECHARSET])dnl
CL_PROTO([unlink], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [int unlink (char* path);], [int unlink();], cl_cv_proto_unlink_arg1)
], [extern int unlink ($cl_cv_proto_unlink_arg1 char*);])
AC_DEFINE_UNQUOTED(UNLINK_CONST,$cl_cv_proto_unlink_arg1)
])dnl
dnl
AC_DEFUN(CL_FSYNC,
[AC_CHECK_FUNCS(fsync)]
)dnl
dnl
AC_DEFUN(CL_IOCTL,
[AC_REQUIRE([CL_TERM])dnl
AC_REQUIRE([CL_OPENFLAGS])dnl
AC_REQUIRE([CL_CADDR_T])dnl
ioctl_decl1='#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#ifndef HAVE_TCSAFLUSH
#undef HAVE_TERMIOS_H
#endif
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#else
#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#else
#ifdef HAVE_TERMIO_H
#include <termio.h>
#else
#ifdef HAVE_SGTTY_H
#include <sgtty.h>
#include <sys/ioctl.h>
#endif
#endif
#endif
#endif
'
CL_PROTO([ioctl], [
dnl First find out whether this set of includes declares ioctl(), or whether
dnl we shall use <sys/ioctl.h> instead.
dnl Note: we must not include <sys/ioctl.h> deliberately since it is
dnl incompatible to <termios.h> (and doesn't even declare ioctl()) on SunOS 4.
AC_TRY_COMPILE($ioctl_decl1
AC_LANG_EXTERN[char* ioctl();], [], try_sys_ioctl=1, ioctl_decl="$ioctl_decl1")
if test -n "try_sys_ioctl"; then
ioctl_decl2='#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#include <sys/ioctl.h>
'
AC_TRY_COMPILE($ioctl_decl2
AC_LANG_EXTERN[char* ioctl();], [],
ioctl_decl="$ioctl_decl1", ioctl_decl="$ioctl_decl2")
fi
dnl Then find out about the correct ioctl declaration:
for y in 'caddr_t arg' 'void* arg' '...'; do
for x in 'int' 'unsigned long' 'long'; do
if test -z "$have_ioctl"; then
CL_PROTO_TRY($ioctl_decl[
#ifdef INCLUDE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
], [int ioctl (int fd, $x request, $y);], [int ioctl();], [
cl_cv_proto_ioctl_arg2="$x"
if test "$y" = "..."; then
cl_cv_proto_ioctl_dots=yes
cl_cv_proto_ioctl_args="int, $cl_cv_proto_ioctl_arg2, ..."
else
cl_cv_proto_ioctl_dots=no
cl_cv_proto_ioctl_arg3=`echo "$y" | sed -e 's, arg,,'`
cl_cv_proto_ioctl_args="int, $cl_cv_proto_ioctl_arg2, $cl_cv_proto_ioctl_arg3"
fi
have_ioctl=1])
fi
done
done
], [extern int ioctl ($cl_cv_proto_ioctl_args);])
AC_DEFINE_UNQUOTED(IOCTL_REQUEST_T,$cl_cv_proto_ioctl_arg2)
if test $cl_cv_proto_ioctl_dots = yes; then
AC_DEFINE(IOCTL_DOTS)
else
AC_DEFINE_UNQUOTED(IOCTL_ARGUMENT_T,$cl_cv_proto_ioctl_arg3)
fi
ioctl_decl="$ioctl_decl1"
ioctl_prog='int x = FIONREAD;'
CL_COMPILE_CHECK([FIONREAD], cl_cv_decl_FIONREAD_1,
$ioctl_decl, $ioctl_prog, ioctl_ok=1)dnl
if test -z "$ioctl_ok"; then
CL_COMPILE_CHECK([FIONREAD in sys/filio.h], cl_cv_decl_FIONREAD_1_sys_filio_h,
$ioctl_decl[#include <sys/filio.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_FILIO_H)
ioctl_ok=1)dnl
fi
if test -z "$ioctl_ok"; then
CL_COMPILE_CHECK([FIONREAD in sys/ioctl.h], cl_cv_decl_FIONREAD_1_sys_ioctl_h,
$ioctl_decl[#include <sys/ioctl.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_IOCTL_H)
ioctl_ok=1)dnl
fi
if test -n "$ioctl_ok"; then
AC_DEFINE(HAVE_FIONREAD)
# Now check whether FIONREAD reliably checks for the EOF of a regular file.
# The number of available characters returned by ioctl(fd,FIONREAD,...) should
# be > 0 for a non-empty regular file at least. On Solaris 2, it is 0.
AC_CACHE_CHECK(for reliable FIONREAD, cl_cv_decl_FIONREAD_reliable, [
AC_TRY_RUN([
/* Declare ioctl(). */
]$ioctl_decl[
#ifdef NEED_SYS_FILIO_H
#include <sys/filio.h>
#endif
#ifdef NEED_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int ioctl ($cl_cv_proto_ioctl_args);
#else
int ioctl();
#endif
/* Declare open(). */
#include <fcntl.h>
#ifdef OPEN_NEEDS_SYS_FILE_H
#include <sys/file.h>
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int open ($cl_cv_proto_open_args);
#else
int open();
#endif
int main ()
{ int fd = open("conftest.c",O_RDONLY,0644);
  long x;
  exit(!((fd >= 0) && (ioctl(fd,FIONREAD,&x) >= 0) && (x > 0)));
}],
cl_cv_decl_FIONREAD_reliable=yes, cl_cv_decl_FIONREAD_reliable=no,
dnl When cross-compiling, don't assume anything.
cl_cv_decl_FIONREAD_reliable="guessing no")
])
case "$cl_cv_decl_FIONREAD_reliable" in
  *yes) AC_DEFINE(HAVE_RELIABLE_FIONREAD) ;;
  *no) ;;
esac
fi
])dnl
dnl
AC_DEFUN(CL_FCNTL,
[CL_PROTO([fcntl], [
for x in 'int arg' '...'; do
if test -z "$have_fcntl"; then
CL_PROTO_TRY([
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
], [int fcntl (int fd, int cmd, $x);], [int fcntl();], [
if test "$x" = "..."; then
cl_cv_fcntl_arg3="..."
else
cl_cv_fcntl_arg3="int"
fi
have_fcntl=1])
fi
done
], [extern int fcntl (int, int, $cl_cv_fcntl_arg3);])
if test "$cl_cv_fcntl_arg3" = "..."; then
AC_DEFINE(FCNTL_DOTS)
fi
])dnl
dnl
AC_DEFUN(RL_SELECT,
[AC_CHECK_FUNCS(select)dnl
if test $ac_cv_func_select = yes; then
CL_COMPILE_CHECK([sys/select.h], cl_cv_header_sys_select_h,
[#include <sys/time.h>
#include <sys/select.h>], ,
AC_DEFINE(HAVE_SYS_SELECT_H))dnl
fi
])dnl
dnl
AC_DEFUN(CL_SELECT,
[AC_CHECK_FUNCS(select)dnl
if test $ac_cv_func_select = yes; then
CL_COMPILE_CHECK([sys/select.h], cl_cv_header_sys_select_h,
[#include <sys/time.h>
#include <sys/select.h>], ,
AC_DEFINE(HAVE_SYS_SELECT_H))dnl
CL_PROTO([select], [
for z in '' 'const'; do
for y in 'fd_set' 'int' 'void'; do
for x in 'int' 'size_t'; do
if test -z "$have_select"; then
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
], [int select ($x width, $y * readfds, $y * writefds, $y * exceptfds, $z struct timeval * timeout);],
[int select();], [
cl_cv_proto_select_arg1="$x"
cl_cv_proto_select_arg2="$y"
cl_cv_proto_select_arg5="$z"
have_select=1])
fi
done
done
done
], [extern int select ($cl_cv_proto_select_arg1, $cl_cv_proto_select_arg2 *, $cl_cv_proto_select_arg2 *, $cl_cv_proto_select_arg2 *, $cl_cv_proto_select_arg5 struct timeval *);])
AC_DEFINE_UNQUOTED(SELECT_WIDTH_T,$cl_cv_proto_select_arg1)
AC_DEFINE_UNQUOTED(SELECT_SET_T,$cl_cv_proto_select_arg2)
AC_DEFINE_UNQUOTED(SELECT_CONST,$cl_cv_proto_select_arg5)
else
AC_CHECK_FUNCS(poll)dnl
fi
])dnl
dnl
AC_DEFUN(CL_UALARM,
[AC_CHECK_FUNCS(ualarm)]
)dnl
dnl
AC_DEFUN(CL_SETITIMER,
[AC_CHECK_FUNCS(setitimer)dnl
if test $ac_cv_func_setitimer = yes; then
CL_PROTO([setitimer], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/time.h>
], [int setitimer (int which, struct itimerval * ivalue, struct itimerval * ovalue);],
[int setitimer();], cl_cv_proto_setitimer_arg2)
], [extern int setitimer (int, $cl_cv_proto_setitimer_arg2 struct itimerval *, struct itimerval *);])
AC_DEFINE_UNQUOTED(SETITIMER_CONST,$cl_cv_proto_setitimer_arg2)
fi
])dnl
dnl
AC_DEFUN(CL_USLEEP,
[AC_CHECK_FUNCS(usleep)]
)dnl
dnl
AC_DEFUN(CL_LOCALTIME,
[CL_PROTO([localtime], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef TM_IN_SYS_TIME
#include <sys/time.h>
#else
#include <time.h>
#endif
], [struct tm * localtime (time_t* clock);], [struct tm * localtime();],
cl_cv_proto_localtime_arg1)
], [extern struct tm * localtime ($cl_cv_proto_localtime_arg1 time_t*);])
AC_DEFINE_UNQUOTED(LOCALTIME_CONST,$cl_cv_proto_localtime_arg1)
])dnl
dnl
AC_DEFUN(CL_GETTIMEOFDAY,
[AC_BEFORE([$0], [CL_TIMES_CLOCK])
AC_CHECK_FUNCS(gettimeofday)dnl
if test $ac_cv_func_gettimeofday = yes; then
dnl HAVE_GETTIMEOFDAY is defined
CL_PROTO([gettimeofday], [
CL_PROTO_TRY([
#include <sys/types.h>
#include <sys/time.h>
], [int gettimeofday (struct timeval * tp, struct timezone * tzp);],
[int gettimeofday();],
cl_cv_proto_gettimeofday_dots=no
cl_cv_proto_gettimeofday_arg2="struct timezone *",
CL_PROTO_TRY([
#include <sys/types.h>
#include <sys/time.h>
], [int gettimeofday (struct timeval * tp, void * tzp);],
[int gettimeofday();],
cl_cv_proto_gettimeofday_dots=no
cl_cv_proto_gettimeofday_arg2="void *",
cl_cv_proto_gettimeofday_dots=yes
cl_cv_proto_gettimeofday_arg2="..."))
], [extern int gettimeofday (struct timeval *, $cl_cv_proto_gettimeofday_arg2);])
if test $cl_cv_proto_gettimeofday_dots = yes; then
AC_DEFINE(GETTIMEOFDAY_DOTS)
else
AC_DEFINE_UNQUOTED(GETTIMEOFDAY_TZP_T,$cl_cv_proto_gettimeofday_arg2)
fi
fi
])dnl
dnl
AC_DEFUN(CL_FTIME,
[AC_BEFORE([$0], [CL_TIMES_CLOCK])
AC_CHECK_FUNCS(ftime)])dnl
dnl
AC_DEFUN(CL_TIMES_CLOCK,
[AC_REQUIRE([CL_GETTIMEOFDAY])dnl
AC_REQUIRE([CL_FTIME])dnl
if test $ac_cv_func_gettimeofday = no -a $ac_cv_func_ftime = no; then
AC_CHECK_FUNC(times, , no_times=1)dnl
if test -z "$no_times"; then
AC_CACHE_CHECK(for times return value, cl_cv_func_times_return, [
AC_TRY_RUN([
#include <sys/types.h>
#include <time.h> /* needed for CLK_TCK */
#ifndef CLK_TCK
#include <sys/time.h> /* needed for CLK_TCK on SYSV PTX */
#endif
#include <sys/times.h>
int main ()
{ struct tms buffer;
  clock_t result1;
  clock_t result2;
  int ticks;
  result1 = times(&buffer);
  if ((result1 == (clock_t)0) || (result1 == (clock_t)(-1))) exit(1);
  sleep(1);
  result2 = times(&buffer);
  if ((result2 == (clock_t)0) || (result2 == (clock_t)(-1))) exit(1);
  ticks = result2 - result1;
  exit(!((ticks >= CLK_TCK/2) && (ticks <= 3*CLK_TCK/2)));
}], cl_cv_func_times_return=yes, cl_cv_func_times_return=no,
dnl When cross-compiling, don't assume anything.
cl_cv_func_times_return="guessing no")
])
case "$cl_cv_func_times_return" in
  *yes) AC_DEFINE(HAVE_TIMES_CLOCK) ;;
  *no)  ;;
esac
fi
fi
])dnl
dnl
AC_DEFUN(CL_GETPWNAM,
[CL_PROTO([getpwnam], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <pwd.h>
], [struct passwd * getpwnam (char* name);], [struct passwd * getpwnam();],
cl_cv_proto_getpwnam_arg1)
], [extern struct passwd * getpwnam ($cl_cv_proto_getpwnam_arg1 char*);])
AC_DEFINE_UNQUOTED(GETPWNAM_CONST,$cl_cv_proto_getpwnam_arg1)
])dnl
dnl
AC_DEFUN(CL_GETPWUID,
[AC_REQUIRE([AC_TYPE_UID_T])dnl
CL_PROTO([getpwuid], [
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <pwd.h>
#include <sys/types.h>
], [struct passwd * getpwuid (uid_t uid);], [struct passwd * getpwuid();],
cl_cv_proto_getpwuid_arg1="uid_t", cl_cv_proto_getpwuid_arg1="int")
], [extern struct passwd * getpwuid ($cl_cv_proto_getpwuid_arg1);])
AC_DEFINE_UNQUOTED(GETPWUID_UID_T,$cl_cv_proto_getpwuid_arg1)
])dnl
dnl
AC_DEFUN(CL_GETHOSTNAME,
[AC_CHECK_FUNCS(gethostname)dnl
if test $ac_cv_func_gethostname = yes; then
CL_PROTO([gethostname], [
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [int gethostname (char* name, int namelen);], [int gethostname();],
cl_cv_proto_gethostname_arg2="int", cl_cv_proto_gethostname_arg2="size_t")
], [extern int gethostname (char*, $cl_cv_proto_gethostname_arg2);])
AC_DEFINE_UNQUOTED(GETHOSTNAME_SIZE_T,$cl_cv_proto_gethostname_arg2)
fi
])dnl
dnl
AC_DEFUN(CL_GETHOSTBYNAME,
[AC_REQUIRE([CL_NETDB])dnl
if test $ac_cv_header_netdb_h = yes; then
  have_netdb=1
else
  AC_CHECK_HEADER(sun/netdb.h, have_netdb=1)
fi
if test -n "$have_netdb"; then
AC_DEFINE(HAVE_GETHOSTBYNAME)
CL_PROTO([gethostbyname], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef HAVE_NETDB_H
#include <sys/socket.h>
#include <netdb.h>
#else
#include <sun/netdb.h>
#endif
], [struct hostent * gethostbyname (char* name);],
[struct hostent * gethostbyname();],
cl_cv_proto_gethostbyname_arg1)
], [extern struct hostent * gethostbyname ($cl_cv_proto_gethostbyname_arg1 char*);])
AC_DEFINE_UNQUOTED(GETHOSTBYNAME_CONST,$cl_cv_proto_gethostbyname_arg1)
fi
])dnl
dnl
AC_DEFUN(CL_CONNECT,
[CL_PROTO([connect], [
for x in '' 'const'; do
for y in 'struct sockaddr *' 'void*'; do
for z in 'int' 'size_t'; do
if test -z "$have_connect_decl"; then
CL_PROTO_TRY([
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
], [int connect (int fd, $x $y name, $z namelen);], [int connect();], [
cl_cv_proto_connect_arg2a="$x"
cl_cv_proto_connect_arg2b="$y"
cl_cv_proto_connect_arg3="$z"
have_connect_decl=1])
fi
done
done
done
], [extern int connect (int, $cl_cv_proto_connect_arg2a $cl_cv_proto_connect_arg2b, $cl_cv_proto_connect_arg3);])
AC_DEFINE_UNQUOTED(CONNECT_CONST,$cl_cv_proto_connect_arg2a)
AC_DEFINE_UNQUOTED(CONNECT_NAME_T,$cl_cv_proto_connect_arg2b)
AC_DEFINE_UNQUOTED(CONNECT_ADDRLEN_T,$cl_cv_proto_connect_arg3)
])dnl
dnl
AC_DEFUN(CL_UNIXCONN,
[AC_CHECK_HEADERS(sys/un.h)dnl
if test $ac_cv_header_sys_un_h = yes; then
CL_COMPILE_CHECK([sun_len in struct sockaddr_un], cl_cv_struct_sockaddr_sun_len,
[#include <sys/types.h> /* NetBSD 1.0 needs this */
#include <sys/un.h>],
[struct sockaddr_un unaddr; unaddr.sun_len;], AC_DEFINE(HAVE_SOCKADDR_UN_LEN))dnl
fi
])dnl
dnl
AC_DEFUN(CL_TCPCONN,
[CL_COMPILE_CHECK([IPv4 sockets], cl_cv_socket_ipv4,
[#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>],
[int x = AF_INET; struct in_addr y; struct sockaddr_in z;],
AC_DEFINE(HAVE_IPV4))
CL_COMPILE_CHECK([IPv6 sockets], cl_cv_socket_ipv6,
[#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>],
[int x = AF_INET6; struct in6_addr y; struct sockaddr_in6 z;],
AC_DEFINE(HAVE_IPV6))
if test $cl_cv_socket_ipv6 = no; then
CL_COMPILE_CHECK([IPv6 sockets in linux/in6.h], cl_cv_socket_ipv6_linux,
[#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <linux/in6.h>],
[int x = AF_INET6; struct in6_addr y; struct sockaddr_in6 z;],
AC_DEFINE(IPV6_NEED_LINUX_IN6_H)
AC_DEFINE(HAVE_IPV6))
fi
AC_CHECK_FUNCS(inet_pton inet_ntop)
AC_CHECK_HEADERS(netinet/in.h arpa/inet.h)dnl
CL_PROTO([inet_addr], [
for x in '' 'const'; do
for y in 'struct in_addr' 'unsigned long' 'unsigned int'; do
if test -z "$have_inet_addr"; then
CL_PROTO_TRY([
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <netinet/in.h>
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
], [$y inet_addr ($x char *);], [$y inet_addr();], [
cl_cv_proto_inet_addr_ret="$y"
cl_cv_proto_inet_addr_arg1="$x"
have_inet_addr=1])
fi
done
done
], [extern $cl_cv_proto_inet_addr_ret inet_addr ($cl_cv_proto_inet_addr_arg1 char*);])
AC_DEFINE_UNQUOTED(RET_INET_ADDR_TYPE,$cl_cv_proto_inet_addr_ret)
AC_DEFINE_UNQUOTED(INET_ADDR_CONST,$cl_cv_proto_inet_addr_arg1)
if test "$cl_cv_proto_inet_addr_ret" = "struct in_addr"; then
AC_DEFINE(INET_ADDR_SUFFIX,[.s_addr])
else
AC_DEFINE(INET_ADDR_SUFFIX,[])
fi
AC_CHECK_HEADERS(netinet/tcp.h)dnl
CL_PROTO([setsockopt], [
for z in 'int' 'unsigned int' 'size_t'; do
for y in 'char*' 'void*'; do
for x in '' 'const'; do
if test -z "$have_setsockopt_decl"; then
CL_PROTO_TRY([
#include <sys/types.h>
#include <sys/socket.h>
], [int setsockopt (int, int, int, $x $y, $z);], [int setsockopt ();], [
cl_cv_proto_setsockopt_const="$x"
cl_cv_proto_setsockopt_arg_t="$y"
cl_cv_proto_setsockopt_optlen_t="$z"
have_setsockopt_decl=1])
fi
done
done
done
], [extern int setsockopt (int, int, int, $cl_cv_proto_setsockopt_const $cl_cv_proto_setsockopt_arg_t, $cl_cv_proto_setsockopt_optlen_t);])
AC_DEFINE_UNQUOTED(SETSOCKOPT_CONST,$cl_cv_proto_setsockopt_const)
AC_DEFINE_UNQUOTED(SETSOCKOPT_ARG_T,$cl_cv_proto_setsockopt_arg_t)
AC_DEFINE_UNQUOTED(SETSOCKOPT_OPTLEN_T,$cl_cv_proto_setsockopt_optlen_t)
])dnl
dnl
AC_DEFUN(CL_ADDRESS_RANGE,
[AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([CL_CC_ANSI])dnl
AC_REQUIRE([CL_MALLOC])dnl
address_range_prog='
#include <stdio.h>
#ifdef __cplusplus
extern "C" void exit(int);
#endif
#if defined(__STDC__) || defined(__cplusplus)
void printf_address (unsigned long addr)
#else
printf_address (addr)
  unsigned long addr;
#endif
{ if (sizeof(unsigned long) <= 4)
    printf ("0x%08X", (unsigned int)addr);
  else
    printf ("0x%08X%08X",(unsigned int)(addr>>32),(unsigned int)(addr&0xFFFFFFFF));
}
#define chop_address(addr) ((unsigned long)(char*)(addr) & ~0x00FFFFFFL)
'
if test $cl_cv_prog_cc_ansi = yes; then
  ul='UL'
else
  ul=''
fi
AC_CACHE_CHECK(for the code address range, cl_cv_address_code, [
if test $cross_compiling = no; then
cat > conftest.c <<EOF
#include "confdefs.h"
$address_range_prog
dnl printf_address(chop_address(&main)); doesn't work in C++.
int main() { printf_address(chop_address(&printf_address)); exit(0); }
EOF
AC_TRY_EVAL(ac_link)
cl_cv_address_code=`./conftest`
rm -f conftest*
else
cl_cv_address_code='guessing 0'
fi
])
x=`echo $cl_cv_address_code | sed -e 's,^guessing ,,'`"$ul"
AC_DEFINE_UNQUOTED(CODE_ADDRESS_RANGE,$x)
AC_CACHE_CHECK(for the malloc address range, cl_cv_address_malloc, [
if test $cross_compiling = no; then
cat > conftest.c <<EOF
#include "confdefs.h"
#include <sys/types.h>
/* declare malloc() */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifndef malloc
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
RETMALLOCTYPE malloc (MALLOC_SIZE_T size);
#else
RETMALLOCTYPE malloc();
#endif
#endif
$address_range_prog
int main() { printf_address(chop_address(malloc(10000))); exit(0); }
EOF
AC_TRY_EVAL(ac_link)
cl_cv_address_malloc=`./conftest`
rm -f conftest*
else
cl_cv_address_malloc='guessing 0'
fi
])
x=`echo $cl_cv_address_malloc | sed -e 's,^guessing ,,'`"$ul"
AC_DEFINE_UNQUOTED(MALLOC_ADDRESS_RANGE,$x)
AC_CACHE_CHECK(for the shared library address range, cl_cv_address_shlib, [
if test $cross_compiling = no; then
cat > conftest.c <<EOF
#include "confdefs.h"
$address_range_prog
/* Declare printf(). */
#if defined(sun) /* for SunOS 4, but not for IRIX 6 */
#ifdef __cplusplus
extern "C" int printf (const char *, ...);
#else
extern int printf ();
#endif
#endif
/* Declare tmpnam(). */
#ifdef __cplusplus
extern "C" char* tmpnam (char*);
#else
extern char* tmpnam ();
#endif
/* With normal simple DLLs, &printf is in the shared library. Fine.
   But with ELF, &printf is a trampoline function allocated near the
   program's code range. errno and other global variables - such as
   &stdout - are allocated near the program's code and bss as well.
   However, the return value of tmpnam(NULL) is a pointer to a static
   buffer in the shared library. (This buffer is unlikely to be named
   by a global symbol.) */
int main() {
  char* addr;
  addr = (char*) tmpnam((char*)0);
  if (!addr) addr = (char*) &printf;
  printf_address(chop_address(addr));
  exit(0);
}
EOF
AC_TRY_EVAL(ac_link)
cl_cv_address_shlib=`./conftest`
rm -f conftest*
else
cl_cv_address_shlib='guessing 0'
fi
])
x=`echo $cl_cv_address_shlib | sed -e 's,^guessing ,,'`"$ul"
AC_DEFINE_UNQUOTED(SHLIB_ADDRESS_RANGE,$x)
AC_CACHE_CHECK(for the stack address range, cl_cv_address_stack, [
if test $cross_compiling = no; then
cat > conftest.c <<EOF
#include "confdefs.h"
$address_range_prog
int main() { int dummy; printf_address(chop_address(&dummy)); exit(0); }
EOF
AC_TRY_EVAL(ac_link)
cl_cv_address_stack=`./conftest`
rm -f conftest*
else
cl_cv_address_stack='guessing ~0'
fi
])
x=`echo "$cl_cv_address_stack" | sed -e 's,^guessing ,,'`"$ul"
AC_DEFINE_UNQUOTED(STACK_ADDRESS_RANGE,$x)
])dnl
dnl
AC_DEFUN(CL_GETPAGESIZE,
[AC_BEFORE([$0], [CL_MPROTECT])
CL_LINK_CHECK([getpagesize], cl_cv_func_getpagesize, [
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
], [getpagesize();],
AC_DEFINE(HAVE_GETPAGESIZE)
have_getpagesize=1)dnl
if test -n "$have_getpagesize"; then
CL_PROTO([getpagesize], [
CL_PROTO_RET([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [int getpagesize();], cl_cv_proto_getpagesize_ret, int, size_t)
], [extern $cl_cv_proto_getpagesize_ret getpagesize (void);])
AC_DEFINE_UNQUOTED(RETGETPAGESIZETYPE,$cl_cv_proto_getpagesize_ret)
fi
])dnl
dnl
AC_DEFUN(CL_VADVISE,
[CL_LINK_CHECK([vadvise], cl_cv_func_vadvise,
[#include <sys/vadvise.h>], [vadvise(0);],
AC_DEFINE(HAVE_VADVISE)dnl
)])dnl
dnl
AC_DEFUN(CL_MACH_VM,
[CL_LINK_CHECK([vm_allocate], cl_cv_func_vm,
 , [vm_allocate(); task_self();],
AC_DEFINE(HAVE_MACH_VM)dnl
)])dnl
dnl
AC_DEFUN(CL_MMAP,
[AC_REQUIRE([CL_OPENFLAGS])dnl
AC_REQUIRE([AC_TYPE_SIZE_T])dnl On AIX, the mmap() prototype references size_t which is undefined.
AC_REQUIRE([AC_TYPE_OFF_T])dnl We use off_t below.
AC_BEFORE([$0], [CL_MUNMAP])AC_BEFORE([$0], [CL_MPROTECT])
AC_CHECK_HEADER(sys/mman.h, , no_mmap=1)dnl
if test -z "$no_mmap"; then
AC_CHECK_FUNC(mmap, , no_mmap=1)dnl
if test -z "$no_mmap"; then
AC_DEFINE(HAVE_MMAP)
CL_PROTO([mmap], [
for z in 'int' 'size_t'; do
for y in 'void*' 'caddr_t'; do
for x in 'void*' 'caddr_t'; do
if test -z "$have_mmap_decl"; then
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/mman.h>
], [$x mmap ($y addr, $z length, int prot, int flags, int fd, off_t off);],
[$x mmap();], [
cl_cv_proto_mmap_ret="$x"
cl_cv_proto_mmap_arg1="$y"
cl_cv_proto_mmap_arg2="$z"
have_mmap_decl=1])
fi
done
done
done
], [extern $cl_cv_proto_mmap_ret mmap ($cl_cv_proto_mmap_arg1, $cl_cv_proto_mmap_arg2, int, int, int, off_t);])
AC_DEFINE_UNQUOTED(RETMMAPTYPE,$cl_cv_proto_mmap_ret)
AC_DEFINE_UNQUOTED(MMAP_ADDR_T,$cl_cv_proto_mmap_arg1)
AC_DEFINE_UNQUOTED(MMAP_SIZE_T,$cl_cv_proto_mmap_arg2)
AC_CACHE_CHECK(for working mmap, cl_cv_func_mmap_works, [
case "$host" in
  i[3456]86-*-sysv4*)
    # UNIX_SYSV_UHC_1
    avoid=0x08000000 ;;
  mips-sgi-irix* | mips-dec-ultrix*)
    # UNIX_IRIX, UNIX_DEC_ULTRIX
    avoid=0x10000000 ;;
  rs6000-ibm-aix*)
    # UNIX_AIX
    avoid=0x20000000 ;;
  *)
    avoid=0 ;;
esac
mmap_prog_1='
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#ifdef OPEN_NEEDS_SYS_FILE_H
#include <sys/file.h>
#endif
#include <sys/types.h>
#include <sys/mman.h>
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
RETMMAPTYPE mmap (MMAP_ADDR_T addr, MMAP_SIZE_T length, int prot, int flags, int fd, off_t off);
#else
RETMMAPTYPE mmap();
#endif
int main () {
'
mmap_prog_2="#define bits_to_avoid $avoid"'
#define my_shift 24
#define my_low   1
#ifdef FOR_SUN4_29
#define my_high  31
#define my_size  32768 /* hope that 32768 is a multiple of the page size */
/* i*32 KB for i=1..31 gives a total of 15.5 MB, which is close to what we need */
#else
#define my_high  64
#define my_size  8192 /* hope that 8192 is a multiple of the page size */
/* i*8 KB for i=1..64 gives a total of 16.25 MB, which is close to what we need */
#endif
 {long i;
#define i_ok(i)  ((i) & (bits_to_avoid >> my_shift) == 0)
  for (i=my_low; i<=my_high; i++)
    if (i_ok(i))
      { caddr_t addr = (caddr_t)(i << my_shift);
/* Check for 8 MB, not 16 MB. This is more likely to work on Solaris 2. */
#if bits_to_avoid
        long size = i*my_size;
#else
        long size = ((i+1)/2)*my_size;
#endif
        if (mmap(addr,size,PROT_READ|PROT_WRITE,flags|MAP_FIXED,fd,0) == (RETMMAPTYPE)-1) exit(1);
    }
#define x(i)  *(unsigned char *) ((i<<my_shift) + (i*i))
#define y(i)  (unsigned char)((3*i-4)*(7*i+3))
  for (i=my_low; i<=my_high; i++) if (i_ok(i)) { x(i) = y(i); }
  for (i=my_high; i>=my_low; i--) if (i_ok(i)) { if (x(i) != y(i)) exit(1); }
  exit(0);
}}
'
AC_TRY_RUN([$mmap_prog_1
  int flags = MAP_ANON | MAP_PRIVATE;
  int fd = -1;
$mmap_prog_2
], have_mmap_anon=1
cl_cv_func_mmap_anon=yes, rm -f core,
: # When cross-compiling, don't assume anything.
)
AC_TRY_RUN([$mmap_prog_1
  int flags = MAP_ANONYMOUS | MAP_PRIVATE;
  int fd = -1;
$mmap_prog_2
], have_mmap_anon=1
cl_cv_func_mmap_anonymous=yes, rm -f core,
: # When cross-compiling, don't assume anything.
)
AC_TRY_RUN([$mmap_prog_1
#ifndef MAP_FILE
#define MAP_FILE 0
#endif
  int flags = MAP_FILE | MAP_PRIVATE;
  int fd = open("/dev/zero",O_RDONLY,0666);
  if (fd<0) exit(1);
$mmap_prog_2
], have_mmap_devzero=1
cl_cv_func_mmap_devzero=yes, rm -f core
retry_mmap=1,
: # When cross-compiling, don't assume anything.
)
if test -n "$retry_mmap"; then
AC_TRY_RUN([#define FOR_SUN4_29
$mmap_prog_1
#ifndef MAP_FILE
#define MAP_FILE 0
#endif
  int flags = MAP_FILE | MAP_PRIVATE;
  int fd = open("/dev/zero",O_RDONLY,0666);
  if (fd<0) exit(1);
$mmap_prog_2
], have_mmap_devzero=1
cl_cv_func_mmap_devzero_sun4_29=yes, rm -f core,
: # When cross-compiling, don't assume anything.
)
fi
if test -n "$have_mmap_anon" -o -n "$have_mmap_devzero"; then
cl_cv_func_mmap_works=yes
else
cl_cv_func_mmap_works=no
fi
])
if test "$cl_cv_func_mmap_anon" = yes; then
AC_DEFINE(HAVE_MMAP_ANON)
fi
if test "$cl_cv_func_mmap_anonymous" = yes; then
AC_DEFINE(HAVE_MMAP_ANONYMOUS)
fi
if test "$cl_cv_func_mmap_devzero" = yes; then
AC_DEFINE(HAVE_MMAP_DEVZERO)
fi
if test "$cl_cv_func_mmap_devzero_sun4_29" = yes; then
AC_DEFINE(HAVE_MMAP_DEVZERO_SUN4_29)
fi
fi
fi
])dnl
dnl
AC_DEFUN(CL_MUNMAP,
[AC_REQUIRE([CL_MMAP])dnl
if test -z "$no_mmap"; then
AC_CHECK_FUNCS(munmap)dnl
fi
])dnl
dnl
AC_DEFUN(CL_MSYNC,
[AC_REQUIRE([CL_MMAP])dnl
if test -z "$no_mmap"; then
AC_CHECK_FUNCS(msync)dnl
fi
])dnl
dnl
AC_DEFUN(CL_MPROTECT,
[AC_REQUIRE([CL_MALLOC])dnl
AC_REQUIRE([CL_GETPAGESIZE])dnl
AC_REQUIRE([CL_MMAP])dnl
AC_CHECK_FUNCS(mprotect)dnl
if test $ac_cv_func_mprotect = yes; then
CL_PROTO([mprotect], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/mman.h>
], [int mprotect (MMAP_ADDR_T addr, MMAP_SIZE_T len, int prot);],
[int mprotect();], cl_cv_proto_mprotect_arg1)
], [extern int mprotect ($cl_cv_proto_mprotect_arg1 $cl_cv_proto_mmap_arg1, $cl_cv_proto_mmap_arg2, int);])
AC_DEFINE_UNQUOTED(MPROTECT_CONST,$cl_cv_proto_mprotect_arg1)
AC_CACHE_CHECK(for working mprotect, cl_cv_func_mprotect_works, [
mprotect_prog='
#include <sys/types.h>
/* declare malloc() */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifndef malloc
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
RETMALLOCTYPE malloc (MALLOC_SIZE_T size);
#else
RETMALLOCTYPE malloc();
#endif
#endif
/* declare getpagesize() and mprotect() */
#include <sys/mman.h>
#ifndef HAVE_GETPAGESIZE
#include <sys/param.h>
#define getpagesize() PAGESIZE
#else
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
RETGETPAGESIZETYPE getpagesize (void);
#else
RETGETPAGESIZETYPE getpagesize();
#endif
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int mprotect (MPROTECT_CONST MMAP_ADDR_T addr, MMAP_SIZE_T len, int prot);
#else
int mprotect();
#endif
char foo;
int main () {
  unsigned long pagesize = getpagesize();
#define page_align(address)  (char*)((unsigned long)(address) & -pagesize)
'
AC_TRY_RUN([$mprotect_prog
  if ((pagesize-1) & pagesize) exit(1);
  exit(0); }], , no_mprotect=1,
# When cross-compiling, don't assume anything.
no_mprotect=1)
mprotect_prog="$mprotect_prog"'
  char* area = (char*) malloc(6*pagesize);
  char* fault_address = area + pagesize*7/2;
'
if test -z "$no_mprotect"; then
AC_TRY_RUN([$mprotect_prog
  if (mprotect(page_align(fault_address),pagesize,PROT_NONE) < 0) exit(0);
  foo = *fault_address; /* this should cause a core dump */
  exit(0); }],
  no_mprotect=1, rm -f core,
: # When cross-compiling, don't assume anything.
)
fi
if test -z "$no_mprotect"; then
AC_TRY_RUN([$mprotect_prog
  if (mprotect(page_align(fault_address),pagesize,PROT_NONE) < 0) exit(0);
  *fault_address = 'z'; /* this should cause a core dump */
  exit(0); }],
  no_mprotect=1, rm -f core,
: # When cross-compiling, don't assume anything.
)
fi
if test -z "$no_mprotect"; then
AC_TRY_RUN([$mprotect_prog
  if (mprotect(page_align(fault_address),pagesize,PROT_READ) < 0) exit(0);
  *fault_address = 'z'; /* this should cause a core dump */
  exit(0); }],
  no_mprotect=1, rm -f core,
: # When cross-compiling, don't assume anything.
)
fi
if test -z "$no_mprotect"; then
AC_TRY_RUN([$mprotect_prog
  if (mprotect(page_align(fault_address),pagesize,PROT_READ) < 0) exit(1);
  if (mprotect(page_align(fault_address),pagesize,PROT_READ|PROT_WRITE) < 0) exit(1);
  *fault_address = 'z'; /* this should not cause a core dump */
  exit(0); }], , no_mprotect=1
rm -f core,
: # When cross-compiling, don't assume anything.
)
fi
if test -z "$no_mprotect"; then
  cl_cv_func_mprotect_works=yes
else
  cl_cv_func_mprotect_works=no
fi
])
if test $cl_cv_func_mprotect_works = yes; then
  AC_DEFINE(HAVE_WORKING_MPROTECT)
fi
fi
])dnl
dnl
AC_DEFUN(CL_CODEEXEC,
[AC_REQUIRE([CL_MALLOC])dnl
AC_CACHE_CHECK([whether code in malloc'ed memory is executable], cl_cv_codeexec, [
dnl The test below does not work on host=hppa*-hp-hpux* because on this system
dnl function pointers are actually pointers into(!) a two-pointer struct.
dnl The test below does not work on host=rs6000-*-* because on this system
dnl function pointers are actually pointers to a three-pointer struct.
case "$host_os" in
  hpux*) cl_cv_codeexec="guessing yes" ;;
  *)
case "$host_cpu" in
  # On host=rs6000-*-aix3.2.5 malloc'ed memory is indeed not executable.
  rs6000) cl_cv_codeexec="guessing no" ;;
  *)
AC_TRY_RUN([
#include <sys/types.h>
/* declare malloc() */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifndef malloc
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
RETMALLOCTYPE malloc (MALLOC_SIZE_T size);
#else
RETMALLOCTYPE malloc();
#endif
#endif
int fun () { return 31415926; }
int main ()
{ long size = (char*)&main - (char*)&fun;
  char* funcopy = (char*) malloc(size);
  int i;
  for (i = 0; i < size; i++) { funcopy[i] = ((char*)&fun)[i]; }
  exit(!((*(int(*)())funcopy)() == 31415926));
}], cl_cv_codeexec=yes, rm -f core
cl_cv_codeexec=no, cl_cv_codeexec="guessing yes")
  ;;
esac
  ;;
esac
])
case "$cl_cv_codeexec" in
  *yes) AC_DEFINE(CODE_EXECUTABLE) ;;
  *no)  ;;
esac
])dnl
dnl
AC_DEFUN(CL_SHMGET,
[AC_REQUIRE([CL_SHM_H])dnl
AC_BEFORE([$0], [CL_SHM])dnl
if test "$ac_cv_header_sys_shm_h" = yes -a "$ac_cv_header_sys_ipc_h" = yes; then
CL_PROTO([shmget], [
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
], [int shmget (key_t key, int size, int shmflg);], [int shmget();],
cl_cv_proto_shmget_arg2="int", cl_cv_proto_shmget_arg2="size_t")
], [extern int shmget (key_t, $cl_cv_proto_shmget_arg2, int);])
AC_DEFINE_UNQUOTED(SHMGET_SIZE_T,$cl_cv_proto_shmget_arg2)
fi
])dnl
dnl
AC_DEFUN(CL_SHMAT,
[AC_REQUIRE([CL_SHM_H])dnl
AC_BEFORE([$0], [CL_SHM])dnl
if test "$ac_cv_header_sys_shm_h" = yes -a "$ac_cv_header_sys_ipc_h" = yes; then
CL_PROTO([shmat], [
CL_PROTO_RET([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
], [void* shmat();],
cl_cv_proto_shmat_ret, [void*], [char*])
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
], [$cl_cv_proto_shmat_ret shmat (int shmid, $cl_cv_proto_shmat_ret shmaddr, int shmflg);],
[$cl_cv_proto_shmat_ret shmat();], cl_cv_proto_shmat_arg2)
], [extern $cl_cv_proto_shmat_ret shmat (int, $cl_cv_proto_shmat_arg2 $cl_cv_proto_shmat_ret, int);])
AC_DEFINE_UNQUOTED(RETSHMATTYPE,$cl_cv_proto_shmat_ret)
AC_DEFINE_UNQUOTED(SHMAT_CONST,$cl_cv_proto_shmat_arg2)
fi
])dnl
dnl
AC_DEFUN(CL_SHMDT,
[AC_REQUIRE([CL_SHM_H])dnl
AC_BEFORE([$0], [CL_SHM])dnl
if test "$ac_cv_header_sys_shm_h" = yes -a "$ac_cv_header_sys_ipc_h" = yes; then
CL_PROTO([shmdt], [
for x in 'void*' 'char*' 'const void *' 'const char *'; do
if test -z "$have_shmdt_decl"; then
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
], [int shmdt($x addr);], [int shmdt();], [
cl_cv_proto_shmdt_arg1="$x"
have_shmdt_decl=1])
fi
done
], [extern int shmdt ($cl_cv_proto_shmdt_arg1);])
AC_DEFINE_UNQUOTED(SHMDT_ADDR_T,$cl_cv_proto_shmdt_arg1)
fi
])dnl
dnl
AC_DEFUN(CL_SHMCTL,
[AC_REQUIRE([CL_SHM_H])dnl
AC_BEFORE([$0], [CL_SHM])dnl
if test "$ac_cv_header_sys_shm_h" = yes -a "$ac_cv_header_sys_ipc_h" = yes; then
CL_PROTO([shmctl], [
CL_PROTO_TRY([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
], [int shmctl (int shmid, int cmd, struct shmid_ds * buf);], [int shmctl();], [
cl_cv_proto_shmctl_dots=no
cl_cv_proto_shmctl_args="int, int, struct shmid_ds *"], [
cl_cv_proto_shmctl_dots=yes
cl_cv_proto_shmctl_args="int, int, ..."])
], [extern int shmctl ($cl_cv_proto_shmctl_args);])
if test $cl_cv_proto_shmctl_dots = yes; then
  AC_DEFINE(SHMCTL_DOTS)
fi
fi
])dnl
dnl
AC_DEFUN(CL_SHM,
[AC_REQUIRE([CL_SHMGET])dnl
AC_REQUIRE([CL_SHMAT])dnl
AC_REQUIRE([CL_SHMDT])dnl
AC_REQUIRE([CL_SHMCTL])dnl
AC_BEFORE([$0], [CL_SHM_RMID])dnl
if test "$ac_cv_header_sys_shm_h" = yes -a "$ac_cv_header_sys_ipc_h" = yes; then
# This test is from Marcus Daniels
AC_CACHE_CHECK(for working shared memory, cl_cv_sys_shm_works, [
AC_TRY_RUN([#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int shmget (key_t key, $cl_cv_proto_shmget_arg2 size, int shmflg);
#else
int shmget();
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
$cl_cv_proto_shmat_ret shmat (int shmid, $cl_cv_proto_shmat_arg2 $cl_cv_proto_shmat_ret shmaddr, int shmflg);
#else
$cl_cv_proto_shmat_ret shmat();
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int shmdt ($cl_cv_proto_shmdt_arg1 shmaddr);
#else
int shmdt();
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int shmctl ($cl_cv_proto_shmctl_args);
#else
int shmctl();
#endif
/* try attaching a single segment to multiple addresses */
#define segsize 0x10000
#define attaches 128
#define base_addr 0x01000000
int main ()
{ int shmid, i; char* addr; char* result;
  if ((shmid = shmget(IPC_PRIVATE,segsize,0400)) < 0) exit(1);
  for (i=0, addr = (char*)0x01000000; i<attaches; i++, addr += segsize)
    { if ((result = shmat(shmid,addr,SHM_RDONLY)) == (char*)(-1)) break; }
  for (i=0, addr = (char*)0x01000000; i<attaches; i++, addr += segsize)
    shmdt(addr);
  shmctl(shmid,IPC_RMID,0);
  exit(result == (char*)(-1));
}], cl_cv_sys_shm_works=yes, cl_cv_sys_shm_works=no,
dnl When cross-compiling, don't assume anything.
cl_cv_sys_shm_works="guessing no")
])
fi
case "$cl_cv_sys_shm_works" in
  *yes) have_shm=1
        AC_DEFINE(HAVE_SHM)
        AC_CHECK_HEADERS(sys/sysmacros.h)
        ;;
  *) ;;
esac
])dnl
dnl
AC_DEFUN(CL_SHM_RMID,
[AC_REQUIRE([CL_SHM])dnl
if test -n "$have_shm"; then
AC_CACHE_CHECK(for attachability of removed shared memory, cl_cv_func_shmctl_attachable, [
AC_TRY_RUN([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#ifdef HAVE_SYS_SYSMACROS_H
#include <sys/sysmacros.h>
#endif
#if defined(__STDC__) || defined(__cplusplus)
]AC_LANG_EXTERN[int shmget (key_t key, SHMGET_SIZE_T size, int shmflg);
]AC_LANG_EXTERN[RETSHMATTYPE shmat (int shmid, SHMAT_CONST RETSHMATTYPE shmaddr, int shmflg);
]AC_LANG_EXTERN[
#ifdef SHMCTL_DOTS
int shmctl (int shmid, int cmd, ...);
#else
int shmctl (int shmid, int cmd, struct shmid_ds * buf);
#endif
#else
extern int shmget();
extern RETSHMATTYPE shmat();
extern int shmctl();
#endif
int main ()
{ unsigned int pagesize = 8192; /* should be a multiple of SHMLBA */
  unsigned long addr = (unsigned long) malloc(2*pagesize);
  addr += pagesize-1; addr = (addr/pagesize)*pagesize;
 {unsigned long addr1 = addr + 0x10000;
  unsigned long addr2 = addr + 0x20000;
  int id = shmget(IPC_PRIVATE,pagesize,IPC_CREAT|0600);
  if (id<0)
    { exit(1); }
  if (shmat(id,(RETSHMATTYPE)addr1,0) == (RETSHMATTYPE)(-1))
    { shmctl(id,IPC_RMID,NULL); exit(1); }
  if (shmctl(id,IPC_RMID,NULL) < 0)
    { exit(1); }
  if (shmat(id,(RETSHMATTYPE)addr2,0) == (RETSHMATTYPE)(-1))
    { shmctl(id,IPC_RMID,NULL); exit(1); }
  shmctl(id,IPC_RMID,NULL);
  exit(0);
}}
], cl_cv_func_shmctl_attachable=yes, cl_cv_func_shmctl_attachable=no,
dnl When cross-compiling, don't assume anything.
cl_cv_func_shmctl_attachable="guessing no")
])
case "$cl_cv_func_shmctl_attachable" in
  *yes) AC_DEFINE(SHM_RMID_VALID) ;;
  *no)  ;;
esac
fi
])dnl
dnl
AC_DEFUN(CL_DYNLOAD,
[dnl Some systems have dlopen in libc, some have it in libdl.
AC_CACHE_CHECK(for dlopen, cl_cv_func_dlopen, [
cl_cv_func_dlopen=no
cl_cv_lib_dl=no
AC_TRY_LINK(AC_LANG_EXTERN[char dlopen();], [dlopen();],
cl_cv_func_dlopen=yes)
if test "$cl_cv_func_dlopen" = no; then
cl_save_LIBS="$LIBS"
LIBS="$LIBS -ldl"
AC_TRY_LINK(AC_LANG_EXTERN[char dlopen();], [dlopen();],
cl_cv_lib_dl=yes
cl_cv_func_dlopen=yes)
LIBS="$cl_save_LIBS"
fi
])
if test "$cl_cv_func_dlopen" = yes; then
  AC_DEFINE(HAVE_DLOPEN)
  CL_PROTO([dlsym], [
  CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <dlfcn.h>
], [void* dlsym (void* handle, char* symbol);], [void* dlsym();],
cl_cv_proto_dlsym_arg2)],
[extern void* dlsym (void* handle, $cl_cv_proto_dlsym_arg2 char* symbol);])
  AC_DEFINE_UNQUOTED(DLSYM_CONST,$cl_cv_proto_dlsym_arg2)
  CL_PROTO([dlerror], [
  CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <dlfcn.h>
], [char * dlerror ();], [char * dlerror();], cl_cv_proto_dlerror_ret)
], [extern $cl_cv_proto_dlerror_ret char * dlerror ();])
  AC_DEFINE_UNQUOTED(DLERROR_CONST,$cl_cv_proto_dlerror_ret)
fi
LIBDL=
if test "$cl_cv_lib_dl" = yes; then
  LIBDL="-ldl"
fi
AC_SUBST(LIBDL)
])dnl
dnl
AC_DEFUN(CL_ICONV,
[AC_CACHE_CHECK(for iconv, cl_cv_func_iconv, [
AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
[iconv_t cd = iconv_open("",""); iconv(cd,NULL,NULL,NULL,NULL); iconv_close(cd);],
cl_cv_func_iconv=yes, cl_cv_func_iconv=no)
])
if test $cl_cv_func_iconv = yes; then
  AC_DEFINE(HAVE_ICONV)
CL_PROTO([iconv], [
CL_PROTO_CONST([
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <iconv.h>
], [size_t iconv (iconv_t cd, char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t* outbytesleft);],
[size_t iconv();],
cl_cv_proto_iconv_arg1)],
[extern size_t iconv (iconv_t cd, $cl_cv_proto_iconv_arg1 char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t* outbytesleft);])
AC_DEFINE_UNQUOTED(ICONV_CONST,$cl_cv_proto_iconv_arg1)
fi
])dnl
dnl
AC_DEFUN(CL_TERMCAP,
[dnl Some systems have tgetent(), tgetnum(), tgetstr(), tgetflag(), tputs(),
dnl tgoto() in libc, some have it in libtermcap, some have it in libncurses.
dnl Cygwin32 has tgetent() in libc and is lacking the others. The top-level
dnl configure will use the included GNU termcap in that case.
LIBTERMCAP=
AC_CHECK_FUNCS(tgetent)
if test $ac_cv_func_tgetent = yes; then
  :
else
  AC_CHECK_LIB(termcap,tgetent, LIBTERMCAP="-ltermcap")
  if test -z "$LIBTERMCAP"; then
    AC_CHECK_LIB(ncurses,tgetent, LIBTERMCAP="-lncurses")
  fi
fi
AC_SUBST(LIBTERMCAP)
])dnl
dnl
AC_DEFUN(CL_FILECHARSET,
[AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([CL_CHDIR])dnl
AC_REQUIRE([CL_OPENDIR])dnl
AC_REQUIRE([CL_CLOSEDIR])dnl
AC_REQUIRE([CL_OPEN])dnl
AC_REQUIRE([CL_UNLINK])dnl
AC_MSG_CHECKING(for the valid characters in filenames)
AC_CACHE_VAL(cl_cv_os_valid_filename_char,[
if test $cross_compiling = no; then
dnl Create the subdirectory the test program will use for its files.
mkdir conftestdir
cat > conftest.c <<EOF
#include "confdefs.h"
#include <sys/types.h>
#if defined(STDC_HEADERS) || defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <stdio.h>
/* Declare chdir(). */
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int chdir (CHDIR_CONST char* path);
#else
int chdir();
#endif
/* Declare open(). */
#include <fcntl.h>
#ifdef OPEN_NEEDS_SYS_FILE_H
#include <sys/file.h>
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
int open ($cl_cv_proto_open_args);
#else
int open();
#endif
/* Declare opendir(), readdir(), closedir(). */
#include <$ac_header_dirent>
#ifdef DIRENT
#define SDIRENT struct dirent
#else
#define SDIRENT struct direct
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
DIR* opendir (OPENDIR_CONST char* dirname);
#else
DIR* opendir();
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
SDIRENT* readdir (DIR* dirp);
#else
SDIRENT* readdir();
#endif
]AC_LANG_EXTERN[
#if defined(__STDC__) || defined(__cplusplus)
RETCLOSEDIRTYPE closedir (DIR* dirp);
#else
RETCLOSEDIRTYPE closedir();
#endif
changequote(,)dnl
/* A small program which checks for each character whether or not it is
 * valid in filenames. */
#define N 256
int main ()
{
#ifdef __CYGWIN32__
  /* The test below would cause a dialog box to pop up (ch == ':'),
     and create files which cause "rm -rf conftest*" to hang
     (ch == '"') || (ch == '<') || (ch == '>') || (ch == 197) || (ch == 206).
     Also, it would make appear that (ch >= 'A') && (ch <= 'Z') doesn't work,
     because it would create files in lower case. But we don't want to forbid
     upper case letters in file names. */
  printf("(ch >= 1) && (ch <= 127) && (ch != 34) && (ch != 42) && (ch != 47) && (ch != 58) && (ch != 60) && (ch != 62) && (ch != 63) && (ch != 92) || (ch == 131) || (ch >= 160) && (ch != 197) && (ch != 206)\n");
#else
  char legal[N];
  char filename[4];
  int i;
  if (chdir("conftestdir") < 0) exit(1);
  for (i = 0; i < N; i++) legal[i] = 0;
  strcpy(filename,"a_z");
  for (i = 0; i < N; i++)
    if (i != '\0')
      { filename[1] = i;
        /* Determine whether the filename is valid: create a file
         * and check that it is present afterwards, under the same name. */
        { int fd = open(filename, O_CREAT | O_RDWR, 0644);
          if (fd >=0)
            { DIR* dirp = opendir(".");
              if (dirp != (DIR*)0)
                { SDIRENT* d;
                  while ((d = readdir(dirp)))
                    { if (!strcmp(d->d_name,".")) continue;
                      if (!strcmp(d->d_name,"..")) continue;
                      if (!strcmp(d->d_name,filename)) legal[i] = 1;
                      /* Remove the file even if its name is something else. */
                      unlink(d->d_name);
                    }
                  closedir(dirp);
                }
              close(fd);
      } }   }
  /* Output a boolean expression equivalent to legal[ch] (0 <= ch < N). */
  { int need_or = 0;
    int z;
    for (z = 0; z < N; )
      { int x, y;
        if (! legal[z]) { z++; continue; }
        x = z;
        if (need_or) printf(" || ");
        z++;
        if ((z < N) && legal[z])
          { do { do { z++; } while ((z < N) && legal[z]);
                 y = z-1;
                 z++;
               } while ((z < N) && legal[z]);
            { int premises = 0;
              if (x > 0) premises++;
              if (y < N-1) premises++;
              for (i = x; i <= y; i++)
                if (! legal[i])
                  premises++;
              if (premises > 1) printf("(");
              { int need_and = 0;
                if (x > 0) { printf("(ch >= %d)",x); need_and = 1; }
                if (y < N-1)
                  { if (need_and) printf(" && ");
                    printf("(ch <= %d)",y);
                    need_and = 1;
                  }
                for (i = x; i <= y; i++)
                  if (! legal[i])
                    { if (need_and) printf(" && ");
                      printf("(ch != %d)",i);
                      need_and = 1;
                    }
                if (!need_and) printf("1");
              }
              if (premises > 1) printf(")");
            }
            z = y+1;
          }
          else
          { printf("(ch == %d)",x); z++; }
        need_or = 1;
      }
    printf("\n");
  }
#endif
  exit(0);
}
changequote([,])dnl
EOF
AC_TRY_EVAL(ac_link)
cl_cv_os_valid_filename_char=`./conftest`
fi
rm -rf conftest*
])
if test -z "$cl_cv_os_valid_filename_char"; then
  cl_cv_os_valid_filename_charset="guessing 7-bit"
else
  if test "$cl_cv_os_valid_filename_char" = '((ch >= 1) && (ch != 47))'; then
    cl_cv_os_valid_filename_charset="8-bit"
  else
    cl_cv_os_valid_filename_charset="7-bit"
  fi
fi
AC_MSG_RESULT($cl_cv_os_valid_filename_charset)
if test -n "$cl_cv_os_valid_filename_char"; then
  AC_DEFINE_UNQUOTED(VALID_FILENAME_CHAR,$cl_cv_os_valid_filename_char)
fi
])dnl
dnl
AC_DEFUN(CL_GLOBAL_CONSTRUCTORS,
[AC_REQUIRE([CL_AS_UNDERSCORE])dnl
if test -n "$GCC"; then
AC_CACHE_CHECK(for the global constructors function prefix,
cl_cv_cplusplus_ctorprefix, [
cat > conftest.cc << EOF
struct foo { foo (); };
foo foobar;
EOF
# look for the assembly language name in the .s file
AC_TRY_COMMAND(${CXX-g++} $CXXFLAGS -S conftest.cc) >/dev/null 2>&1
if grep '_GLOBAL_\$I\$foobar' conftest.s >/dev/null ; then
  cl_cv_cplusplus_ctorprefix='_GLOBAL_$I$'
else
  if grep '_GLOBAL_\.I\.foobar' conftest.s >/dev/null ; then
    cl_cv_cplusplus_ctorprefix='_GLOBAL_.I.'
  else
    if grep '_GLOBAL__I_foobar' conftest.s >/dev/null ; then
      cl_cv_cplusplus_ctorprefix='_GLOBAL__I_'
    else
      cl_cv_cplusplus_ctorprefix=unknown
    fi
  fi
fi
rm -f conftest*
])
if test "$cl_cv_cplusplus_ctorprefix" '!=' unknown; then
  ac_value='"'"$cl_cv_cplusplus_ctorprefix"'"'
  AC_DEFINE_UNQUOTED(CL_GLOBAL_CONSTRUCTOR_PREFIX,$ac_value)
  ac_value=`echo "$ac_value" | sed -e 's,I,D,'`
  AC_DEFINE_UNQUOTED(CL_GLOBAL_DESTRUCTOR_PREFIX,$ac_value)
dnl Check whether the global constructors/destructors functions are file-scope
dnl only by default. This is the case in egcs-1.1.2 or newer.
AC_CACHE_CHECK(whether the global constructors function need to be exported,
cl_cv_cplusplus_ctorexport, [
cat > conftest1.cc << EOF
struct foo { foo (); };
foo foobar;
EOF
cat > conftest2.cc << EOF
#include "confdefs.h"
#ifdef ASM_UNDERSCORE
#define ASM_UNDERSCORE_PREFIX "_"
#else
#define ASM_UNDERSCORE_PREFIX ""
#endif
struct foo { foo (); };
foo::foo () {}
extern "C" void ctor (void) __asm__ (ASM_UNDERSCORE_PREFIX CL_GLOBAL_CONSTRUCTOR_PREFIX "foobar");
int main() { ctor(); return 0; }
EOF
if AC_TRY_COMMAND(${CXX-g++} -o conftest${ac_exeext} $CXXFLAGS $CPPFLAGS $LDFLAGS conftest1.cc conftest2.cc $LIBS 1>&5) >/dev/null 2>&1 && test -s conftest${ac_exeext}; then
  cl_cv_cplusplus_ctorexport=no
else
  cl_cv_cplusplus_ctorexport=yes
fi
rm -f conftest*
])
if test "$cl_cv_cplusplus_ctorexport" = yes; then
  AC_DEFINE(CL_NEED_GLOBALIZE_CTORDTOR)
fi
fi
fi
])dnl
dnl
AC_DEFUN(CL_BUILTIN_STRLEN,
[AC_CACHE_CHECK(for inline __builtin_strlen, cl_cv_builtin_strlen, [
cat > conftest.$ac_ext <<EOF
#if defined(__STDC__) || defined(__cplusplus)
int foo (char* x)
#else
int foo (x) char* x;
#endif
{ return __builtin_strlen(x); }
EOF
if AC_TRY_COMMAND(${CC-cc} -S $CFLAGS $CPPFLAGS conftest.$ac_ext) >/dev/null 2>&1 ; then
  if grep strlen conftest.s >/dev/null ; then
    cl_cv_builtin_strlen=no
  else
    cl_cv_builtin_strlen=yes
  fi
else
  cl_cv_builtin_strlen=no
fi
rm -f conftest*
])
if test $cl_cv_builtin_strlen = yes; then
  AC_DEFINE(HAVE_BUILTIN_STRLEN)
fi
])dnl
dnl
AC_DEFUN(CL_BUILTIN_STRCMP,
[AC_CACHE_CHECK(for inline __builtin_strcmp, cl_cv_builtin_strcmp, [
cat > conftest.$ac_ext <<EOF
#if defined(__STDC__) || defined(__cplusplus)
int foo (char* x, char* y)
#else
int foo (x,y) char* x; char* y;
#endif
{ return __builtin_strcmp(x,y); }
EOF
if AC_TRY_COMMAND(${CC-cc} -S $CFLAGS $CPPFLAGS conftest.$ac_ext) >/dev/null 2>&1 ; then
  if grep strcmp conftest.s >/dev/null ; then
    cl_cv_builtin_strcmp=no
  else
    cl_cv_builtin_strcmp=yes
  fi
else
  cl_cv_builtin_strcmp=no
fi
rm -f conftest*
])
if test $cl_cv_builtin_strcmp = yes; then
  AC_DEFINE(HAVE_BUILTIN_STRCMP)
fi
])dnl
dnl
AC_DEFUN(CL_CHAR_UNSIGNED,
[dnl This is mostly copied from AC_C_CHAR_UNSIGNED.
AC_CACHE_CHECK(whether characters are unsigned, ac_cv_c_char_unsigned, [
if test $ac_cv_prog_gcc = yes; then
  # GCC predefines this symbol on systems where it applies.
AC_EGREP_CPP(yes,
[#ifdef __CHAR_UNSIGNED__
  yes
#endif
], ac_cv_c_char_unsigned=yes, ac_cv_c_char_unsigned=no)
else
AC_TRY_RUN(
[/* volatile prevents gcc2 from optimizing the test away on sparcs.  */
#if !defined(__STDC__) || __STDC__ != 1
#define volatile
#endif
int main() {
  volatile char c = 255; exit(c < 0);
}], ac_cv_c_char_unsigned=yes, ac_cv_c_char_unsigned=no,
ac_cv_c_char_unsigned="guessing no")
fi])
if test $ac_cv_prog_gcc = no; then
  # GCC defines __CHAR_UNSIGNED__ by itself, no need to fix up.
  case "$ac_cv_c_char_unsigned" in
    *yes) AC_DEFINE(__CHAR_UNSIGNED__) ;;
    *no) ;;
  esac
fi
])dnl
dnl
AC_DEFUN(CL_BOOL,
[AC_LANG_SAVE()
AC_LANG_CPLUSPLUS()
CL_COMPILE_CHECK([bool type], cl_cv_cplusplus_bool, , [bool x;],
AC_DEFINE(HAVE_BOOL), AC_DEFINE(bool,int))dnl
AC_LANG_RESTORE()
])dnl
dnl
AC_DEFUN(CL_MACHINE,
[AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([CL_CHAR_UNSIGNED])dnl
cl_machine_file_c=$2
cl_machine_file_h=$3
if test $cross_compiling = no; then
if test -z "$[$4]"; then
AC_CHECKING(for [$1])
cat > conftest.$ac_ext <<EOF
#include "confdefs.h"
EOF
cat "$cl_machine_file_c" >> conftest.$ac_ext
ORIGCC="$CC"
if test $ac_cv_prog_gcc = yes; then
# gcc -O (gcc version <= 2.3.2) crashes when compiling long long shifts for
# target 80386. Strip "-O".
CC=`echo "$CC " | sed -e 's/-O //g'`
fi
AC_TRY_EVAL(ac_link)
CC="$ORIGCC"
if test -s conftest; then
  echo "creating $cl_machine_file_h"
  ./conftest > conftest.h
  if cmp -s "$cl_machine_file_h" conftest.h 2>/dev/null; then
    # The file exists and we would not be changing it
    rm -f conftest.h
  else
    rm -f "$cl_machine_file_h"
    mv conftest.h "$cl_machine_file_h"
  fi
  [$4]=1
else
  echo "creation of $cl_machine_file_h failed"
fi
rm -f conftest*
fi
else
echo "cross-compiling - cannot create $cl_machine_file_h"
fi
])dnl
dnl
AC_DEFUN(CL_WORDS_LITTLEENDIAN,
[AC_MSG_CHECKING(byte ordering)
AC_CHECK_VAL(cl_cv_sys_endian, [
AC_TRY_RUN([int main () {
  /* Are we little or big endian?  From Harbison&Steele.  */
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  u.l = 1;
  exit (u.c[0] == 1);
}],
cl_cv_sys_endian=big,
cl_cv_sys_endian=little,
# must guess the endianness
)
if test -z "$cl_cv_sys_endian"; then
AC_EGREP_CPP(yes,[#if defined(m68k) || defined(mc68000) || defined(mc68020) || defined(sparc) || defined(__sparc__) || defined(MIPSEB) || defined(hppa) || defined(__hppa) || defined(m88000)
  yes
#endif
], cl_cv_sys_endian=big)
fi
if test -z "$cl_cv_sys_endian"; then
AC_EGREP_CPP(yes,[#if defined(i386) || defined(__i386) || defined(_I386) || defined(MIPSEL) || defined(__alpha)
  yes
#endif
], cl_cv_sys_endian=little)
fi
if test -z "$cl_cv_sys_endian"; then
cl_cv_sys_endian="guessing little"
fi
])
AC_MSG_RESULT([$cl_cv_sys_endian endian])
case "$cl_cv_sys_endian" in
  *little) AC_DEFINE(WORDS_LITTLEENDIAN) ;;
  *big)    ;;
esac
])dnl
dnl
