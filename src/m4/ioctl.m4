dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_IOCTL],
[AC_REQUIRE([CL_TERM])dnl
AC_REQUIRE([CL_OPENFLAGS])dnl
AC_REQUIRE([CL_CADDR_T])dnl
ioctl_decl1='
#include <stdlib.h>
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
ioctl_decl2='
#include <stdlib.h>
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
AC_DEFINE_UNQUOTED(IOCTL_REQUEST_T,$cl_cv_proto_ioctl_arg2,[type of `request' in ioctl() declaration])
if test $cl_cv_proto_ioctl_dots = yes; then
AC_DEFINE(IOCTL_DOTS,,[declaration of ioctl() needs dots])
else
AC_DEFINE(IOCTL_ARGUMENT_T,$cl_cv_proto_ioctl_arg3,[type of `argument' in ioctl() declaration, if not superseded by dots])
fi
ioctl_decl="$ioctl_decl1"
ioctl_prog='int x = FIONREAD;'
CL_COMPILE_CHECK([FIONREAD], cl_cv_decl_FIONREAD_1,
$ioctl_decl, $ioctl_prog, ioctl_ok=1)dnl
if test -z "$ioctl_ok"; then
CL_COMPILE_CHECK([FIONREAD in sys/filio.h], cl_cv_decl_FIONREAD_1_sys_filio_h,
$ioctl_decl[#include <sys/filio.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_FILIO_H,,[need <sys/filio.h> for using ioctl() FIONREAD])
ioctl_ok=1)dnl
fi
if test -z "$ioctl_ok"; then
CL_COMPILE_CHECK([FIONREAD in sys/ioctl.h], cl_cv_decl_FIONREAD_1_sys_ioctl_h,
$ioctl_decl[#include <sys/ioctl.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_IOCTL_H,,[need <sys/ioctl.h> for using ioctl() FIONREAD])
ioctl_ok=1)dnl
fi
if test -n "$ioctl_ok"; then
AC_DEFINE(HAVE_FIONREAD,,[have the FIONREAD ioctl()])
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
  *yes) AC_DEFINE(HAVE_RELIABLE_FIONREAD,,[have the FIONREAD ioctl() and it works reliably on files]) ;;
  *no) ;;
esac
fi
])
