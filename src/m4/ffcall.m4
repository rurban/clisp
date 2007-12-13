# -*- Autoconf -*-
# Copyright (C) 2007 Sam Steingold (GNU GPL2+)

AC_PREREQ(2.61)

AC_DEFUN([CL_FFCALL],[dnl
AC_ARG_WITH([ffcall],
[AC_HELP_STRING([--with-ffcall],[use FFCALL (default is YES, if present)])],
[cl_use_ffcall=$withval],[cl_use_ffcall=default])
AC_REQUIRE([AC_LIB_PREPARE_PREFIX])dnl prerequisite of AC_LIB_LINKFLAGS_BODY
AC_REQUIRE([AC_LIB_RPATH])dnl prerequisite of AC_LIB_LINKFLAGS_BODY
AC_LIB_LINKFLAGS_ADD([ffcall])dnl accept --with-libffcall-prefix
if test $cl_use_ffcall != no; then
 AC_CACHE_CHECK([whether ffcall is present],[cl_cv_have_ffcall],[dnl
  cl_cv_have_ffcall=no
  cl_save_CPPFLAGS="$CPPFLAGS"
  cl_save_LIBS="$LIBS"
  AC_LIB_LINKFLAGS_SEARCH(avcall)
  AC_LIB_APPENDTOVAR([CPPFLAGS], [$INCAVCALL])
  AC_LIB_APPENDTOVAR([LIBS], [$LIBAVCALL])
  AC_LIB_LINKFLAGS_SEARCH(callback)
  AC_LIB_APPENDTOVAR([CPPFLAGS], [$INCCALLBACK])
  AC_LIB_APPENDTOVAR([LIBS], [$LIBCALLBACK])
  AC_CHECK_HEADERS(avcall.h callback.h)
  AC_SEARCH_LIBS(__builtin_avcall,avcall)
  AC_SEARCH_LIBS(trampoline_r_data0,callback)
  if test $ac_cv_header_avcall_h = yes \
       -a $ac_cv_header_callback_h = yes \
       -a "$ac_cv_search___builtin_avcall" != no \
       -a "$ac_cv_search_trampoline_r_data0" != no
  then cl_cv_have_ffcall=yes
  else
    CPPFLAGS="$cl_save_CPPFLAGS"
    LIBS="$cl_save_LIBS"
    LIBAVCALL=
    LIBCALLBACK=
  fi
 ])
 AC_SUBST(LIBAVCALL)
 AC_SUBST(LIBCALLBACK)
 if test $cl_use_ffcall = yes -a $cl_cv_have_ffcall = no; then
   FFCALL=ffcall-1.8
   AC_MSG_ERROR([despite --with-ffcall, FFCALL was not found
 Either call configure without --with-ffcall or do
  mkdir tools; cd tools; prefix=`pwd`/${ac_cv_host}
  wget http://ftp.gnu.org/pub/gnu/ffcall/${FFCALL}.tar.gz
  tar xfz ${FFCALL}.tar.gz
  cd ${FFCALL}
  ./configure --prefix=\${prefix} && make && make check && make install
  cd ../..
  ./configure --with-libffcall-prefix=\${prefix} [$]*])
 fi
fi;])
