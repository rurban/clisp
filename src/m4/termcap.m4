dnl Copyright (C) 1993-2002 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels.

AC_PREREQ(2.13)

AC_DEFUN([CL_TERMCAP],[
dnl Some systems have tgetent(), tgetnum(), tgetstr(), tgetflag(), tputs(),
dnl tgoto() in libc, some have it in libtermcap, some have it in libncurses.
dnl When both libtermcap and libncurses exist, we prefer the latter, because
dnl libtermcap is being phased out.
LIBTERMCAP="broken"
AC_CHECK_FUNCS(tgetent)
if test $ac_cv_func_tgetent = yes; then
  LIBTERMCAP=""
else
  AC_CHECK_LIB(ncurses,tgetent, LIBTERMCAP="-lncurses")
  if test -z "$LIBTERMCAP"; then
    AC_CHECK_LIB(termcap,tgetent, LIBTERMCAP="-ltermcap")
  fi
fi
AC_SUBST(LIBTERMCAP)
])
