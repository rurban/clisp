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
## libtool.m4 - Configure libtool for the target system. -*-Shell-script-*-
## Copyright (C) 1996-1998 Free Software Foundation, Inc.
## Gordon Matzigkeit <gord@gnu.ai.mit.edu>, 1996
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
## As a special exception to the GNU General Public License, if you
## distribute this file as part of a program that contains a
## configuration script generated by Autoconf, you may include it under
## the same distribution terms that you use for the rest of that program.

# serial 24 AM_PROG_LIBTOOL
AC_DEFUN(AM_PROG_LIBTOOL,
[AC_REQUIRE([AM_ENABLE_SHARED])dnl
AC_REQUIRE([AM_ENABLE_STATIC])dnl
AC_REQUIRE([CL_CANONICAL_HOST])dnl
AC_REQUIRE([CL_PROG_RANLIB])dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AM_PROG_LD])dnl
AC_REQUIRE([AM_PROG_NM])dnl
AC_REQUIRE([AC_PROG_LN_S])dnl
dnl
# Always use our own libtool.
LIBTOOL='$(SHELL) $(top_builddir)/libtool'
AC_SUBST(LIBTOOL)dnl

# Check for any special flags to pass to ltconfig.
libtool_flags=
test "$enable_shared" = no && libtool_flags="$libtool_flags --disable-shared"
test "$enable_static" = no && libtool_flags="$libtool_flags --disable-static"
test "$silent" = yes && libtool_flags="$libtool_flags --silent"
test "$ac_cv_prog_gcc" = yes && libtool_flags="$libtool_flags --with-gcc"
test "$ac_cv_prog_gnu_ld" = yes && libtool_flags="$libtool_flags --with-gnu-ld"

# Some flags need to be propagated to the compiler or linker for good
# libtool support.
case "$host" in
*-*-irix6*)
  # Find out which ABI we are using.
  echo '[#]line __oline__ "configure"' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
    case "`/usr/bin/file conftest.o`" in
    *32-bit*)
      LD="${LD-ld} -32"
      ;;
    *N32*)
      LD="${LD-ld} -n32"
      ;;
    *64-bit*)
      LD="${LD-ld} -64"
      ;;
    esac
  fi
  rm -rf conftest*
  ;;

*-*-sco3.2v5*)
  # On SCO OpenServer 5, we need -belf to get full-featured binaries.
  CFLAGS="$CFLAGS -belf"
  ;;
esac

# Actually configure libtool.  ac_aux_dir is where install-sh is found.
CC="$CC" CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" \
LD="$LD" NM="$NM" RANLIB="$RANLIB" LN_S="$LN_S" \
${CONFIG_SHELL-/bin/sh} $ac_aux_dir/ltconfig \
$libtool_flags --no-verify $ac_aux_dir/ltmain.sh $host \
|| AC_MSG_ERROR([libtool configure failed])
])

# AM_ENABLE_SHARED - implement the --enable-shared flag
# Usage: AM_ENABLE_SHARED[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AM_ENABLE_SHARED,
[define([AM_ENABLE_SHARED_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(shared,
changequote(<<, >>)dnl
<<  --enable-shared         build shared libraries [default=>>AM_ENABLE_SHARED_DEFAULT]
changequote([, ])dnl
[  --enable-shared=PKGS    only build shared libraries if the current package
                          appears as an element in the PKGS list],
[p=${PACKAGE-default}
case "$enableval" in
yes) enable_shared=yes ;;
no) enable_shared=no ;;
*)
  enable_shared=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_shared=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_shared=AM_ENABLE_SHARED_DEFAULT)dnl
])

# AM_DISABLE_SHARED - set the default shared flag to --disable-shared
AC_DEFUN(AM_DISABLE_SHARED,
[AM_ENABLE_SHARED(no)])

# AM_DISABLE_STATIC - set the default static flag to --disable-static
AC_DEFUN(AM_DISABLE_STATIC,
[AM_ENABLE_STATIC(no)])

# AM_ENABLE_STATIC - implement the --enable-static flag
# Usage: AM_ENABLE_STATIC[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AM_ENABLE_STATIC,
[define([AM_ENABLE_STATIC_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(static,
changequote(<<, >>)dnl
<<  --enable-static         build static libraries [default=>>AM_ENABLE_STATIC_DEFAULT]
changequote([, ])dnl
[  --enable-static=PKGS    only build shared libraries if the current package
                          appears as an element in the PKGS list],
[p=${PACKAGE-default}
case "$enableval" in
yes) enable_static=yes ;;
no) enable_static=no ;;
*)
  enable_static=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_static=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_static=AM_ENABLE_STATIC_DEFAULT)dnl
])


# AM_PROG_LD - find the path to the GNU or non-GNU linker
AC_DEFUN(AM_PROG_LD,
[AC_ARG_WITH(gnu-ld,
[  --with-gnu-ld           assume the C compiler uses GNU ld [default=no]],
test "$withval" = no || with_gnu_ld=yes, with_gnu_ld=no)
AC_REQUIRE([AC_PROG_CC])
ac_prog=ld
if test "$ac_cv_prog_gcc" = yes; then
  # Check if gcc -print-prog-name=ld gives a path.
  AC_MSG_CHECKING([for ld used by GCC])
  ac_prog=`($CC -print-prog-name=ld) 2>&5`
  case "$ac_prog" in
  # Accept absolute paths.
  /* | [A-Za-z]:\\*)
    test -z "$LD" && LD="$ac_prog"
    ;;
  "")
    # If it fails, then pretend we aren't using GCC.
    ac_prog=ld
    ;;
  *)
    # If it is relative, then search for the first ld in PATH.
    with_gnu_ld=unknown
    ;;
  esac
elif test "$with_gnu_ld" = yes; then
  AC_MSG_CHECKING([for GNU ld])
else
  AC_MSG_CHECKING([for non-GNU ld])
fi
AC_CACHE_VAL(ac_cv_path_LD,
[if test -z "$LD"; then
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
  for ac_dir in $PATH; do
    test -z "$ac_dir" && ac_dir=.
    if test -f "$ac_dir/$ac_prog"; then
      ac_cv_path_LD="$ac_dir/$ac_prog"
      # Check to see if the program is GNU ld.  I'd rather use --version,
      # but apparently some GNU ld's only accept -v.
      # Break only if it was the GNU/non-GNU ld that we prefer.
      if "$ac_cv_path_LD" -v 2>&1 < /dev/null | egrep '(GNU|with BFD)' > /dev/null; then
	test "$with_gnu_ld" != no && break
      else
        test "$with_gnu_ld" != yes && break
      fi
    fi
  done
  IFS="$ac_save_ifs"
else
  ac_cv_path_LD="$LD" # Let the user override the test with a path.
fi])
LD="$ac_cv_path_LD"
if test -n "$LD"; then
  AC_MSG_RESULT($LD)
else
  AC_MSG_RESULT(no)
fi
test -z "$LD" && AC_MSG_ERROR([no acceptable ld found in \$PATH])
AC_SUBST(LD)
AM_PROG_LD_GNU
])

AC_DEFUN(AM_PROG_LD_GNU,
[AC_CACHE_CHECK([if the linker ($LD) is GNU ld], ac_cv_prog_gnu_ld,
[# I'd rather use --version here, but apparently some GNU ld's only accept -v.
if $LD -v 2>&1 </dev/null | egrep '(GNU|with BFD)' 1>&5; then
  ac_cv_prog_gnu_ld=yes
else
  ac_cv_prog_gnu_ld=no
fi])
])

# AM_PROG_NM - find the path to a BSD-compatible name lister
AC_DEFUN(AM_PROG_NM,
[AC_MSG_CHECKING([for BSD-compatible nm])
AC_CACHE_VAL(ac_cv_path_NM,
[case "$NM" in
/* | [A-Za-z]:\\*)
  ac_cv_path_NM="$NM" # Let the user override the test with a path.
  ;;
*)
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
  for ac_dir in /usr/ucb /usr/ccs/bin $PATH /bin; do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/nm; then
      # Check to see if the nm accepts a BSD-compat flag.
      # Adding the `sed 1q' prevents false positives on HP-UX, which says:
      #   nm: unknown option "B" ignored
      if ($ac_dir/nm -B /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
        ac_cv_path_NM="$ac_dir/nm -B"
      elif ($ac_dir/nm -p /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
        ac_cv_path_NM="$ac_dir/nm -p"
      else
        ac_cv_path_NM="$ac_dir/nm"
      fi
      break
    fi
  done
  IFS="$ac_save_ifs"
  test -z "$ac_cv_path_NM" && ac_cv_path_NM=nm
  ;;
esac])
NM="$ac_cv_path_NM"
AC_MSG_RESULT([$NM])
AC_SUBST(NM)
])
