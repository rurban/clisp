dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_PUTENV],
[dnl Not AC_CHECK_FUNCS(putenv) because it doesn't work when CC=g++.
AC_CACHE_CHECK([for putenv], ac_cv_func_putenv, [
AC_TRY_LINK(AC_LANG_EXTERN[
#ifdef __cplusplus
int putenv(char*);
#else
int putenv();
#endif
], [putenv("");],
ac_cv_func_putenv=yes, ac_cv_func_putenv=no)])
if test $ac_cv_func_putenv = yes; then
AC_DEFINE(HAVE_PUTENV, 1, [Define if you have the putenv() function.])
CL_PROTO([putenv], [
CL_PROTO_CONST([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [int putenv (char* name);], [int putenv();], cl_cv_proto_putenv_arg1)
], [extern int putenv ($cl_cv_proto_putenv_arg1 char*);])
AC_DEFINE_UNQUOTED(PUTENV_CONST,$cl_cv_proto_putenv_arg1,[declaration of putenv() needs const])
else
AC_CHECK_FUNCS(setenv)dnl
fi
])
