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
AC_DEFUN(CL_UNISTD_H,
[AC_CHECK_HEADERS(unistd.h)]
)dnl
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
