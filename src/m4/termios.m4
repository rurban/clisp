dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([RL_TERM],
[AC_CHECK_HEADERS(termios.h termio.h sys/termio.h sgtty.h)dnl
if test $ac_cv_header_termios_h = yes; then
  dnl HAVE_TERMIOS_H defined
  AC_CHECK_FUNCS(tcgetattr tcflow)dnl
fi
AC_CHECK_HEADERS(sys/stream.h sys/ptem.h)dnl
ioctl_decl='
#include <stdlib.h>
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
AC_DEFINE(NEED_SYS_FILIO_H,,[need <sys/filio.h> for using ioctl FIONREAD])
ioctl_ok=1)dnl
fi
if test -z "$ioctl_ok"; then
CL_COMPILE_CHECK([FIONREAD in sys/ioctl.h], cl_cv_decl_FIONREAD_sys_ioctl_h,
$ioctl_decl[#include <sys/ioctl.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_IOCTL_H,,[need <sys/ioctl.h> for using ioctl FIONREAD])
ioctl_ok=1)dnl
fi
])

AC_DEFUN([CL_TERM],
[AC_BEFORE([$0], [CL_IOCTL])
AC_CHECK_HEADERS(termios.h termio.h sys/termio.h sgtty.h)dnl
if test $ac_cv_header_termios_h = yes; then
dnl HAVE_TERMIOS_H defined
dnl A/UX has <termios.h> but is lacking tcgetattr etc.
CL_LINK_CHECK([tcgetattr], cl_cv_func_tcgetattr,
[#include <termios.h>], [struct termios t; tcgetattr(0,&t);],
AC_DEFINE(HAVE_TCGETATTR,,[have tcgetattr(), either as a function or as a macro defined by <termios.h>]))dnl
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
AC_DEFINE_UNQUOTED(TCSETATTR_CONST,$cl_cv_proto_tcsetattr_arg3,[declaration of tcsetattr() needs const])
fi
CL_LINK_CHECK([TCSAFLUSH in termios.h], cl_cv_decl_TCSAFLUSH,
[#include <termios.h>], [int x = TCSAFLUSH;],
AC_DEFINE(HAVE_TCSAFLUSH,,[<termios.h> defines TCSAFLUSH]))dnl
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
[struct winsize w;], AC_DEFINE(WINSIZE_NEED_SYS_IOCTL_H,,[have <termios.h> but need <sys/ioctl.h> for `struct winsize']))dnl
if test $cl_cv_struct_winsize_ioctl = no; then
CL_COMPILE_CHECK([struct winsize in sys/ptem.h], cl_cv_struct_winsize_ptem,
[#include <sys/types.h>
#include <sys/stream.h>
#include <sys/ptem.h>],
[struct winsize w;], AC_DEFINE(WINSIZE_NEED_SYS_PTEM_H,,[have <termios.h> but need <sys/ptem.h> for `struct winsize']))dnl
fi
fi
fi
])
