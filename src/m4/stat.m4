dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_LSTAT],
[dnl Cannot use AC_CHECK_FUNCS(lstat) because Linux defines lstat() as an
dnl inline function in <sys/stat.h>.
CL_LINK_CHECK([lstat],cl_cv_func_lstat,[
#include <sys/types.h>
#include <sys/stat.h>
], [return lstat("",(struct stat *)0);],
AC_DEFINE(HAVE_LSTAT,,[have lstat()?]))dnl
])
