dnl Copyright (C) 1993-2002 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels.

AC_PREREQ(2.13)

AC_DEFUN([CL_OPENFLAGS],
dnl BSD systems require #include <sys/file.h> for O_RDWR etc. being defined.
[AC_BEFORE([$0], [CL_MMAP])
AC_CHECK_HEADERS(sys/file.h)
if test $ac_cv_header_sys_file_h = yes; then
openflags_decl='
#include <stdlib.h>
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
])
