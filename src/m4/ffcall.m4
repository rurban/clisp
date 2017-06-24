# -*- Autoconf -*-
# Copyright (C) 2007-2010 Sam Steingold (GNU GPL2+)

AC_PREREQ(2.61)

dnl Download location of the newest libffcall release.
AC_DEFUN([CL_LIBFFCALL_DOWNLOAD_URL],
  [https://ftp.gnu.org/gnu/libffcall/libffcall-1.13.tar.gz])

AC_DEFUN([CL_FFCALL],[dnl
AC_ARG_WITH([ffcall],
[AC_HELP_STRING([--with-ffcall],[use FFCALL (default is YES, if present)])],
[cl_use_ffcall=$withval],[cl_use_ffcall=default])
if test $cl_use_ffcall != no; then
AC_LIB_FROMPACKAGE(avcall,libffcall)
AC_LIB_FROMPACKAGE(callback,libffcall)
AC_LIB_LINKFLAGS([avcall])
AC_LIB_LINKFLAGS([callback])
AC_CHECK_HEADERS(avcall.h callback.h)
if test "$ac_cv_header_avcall_h" = yes \
     -a "$ac_cv_header_callback_h" = yes
then
cl_save_LIBS="$LIBS"
AC_LIB_APPENDTOVAR([LIBS], [$LIBAVCALL])
AC_LIB_APPENDTOVAR([LIBS], [$LIBCALLBACK])
AC_SEARCH_LIBS(__builtin_avcall)
AC_SEARCH_LIBS(trampoline_r_data0)
if test "$ac_cv_search___builtin_avcall" = no \
     -o "$ac_cv_search_trampoline_r_data0" = no
then LIBS=$cl_save_LIBS
fi
fi
AC_CACHE_CHECK([whether libffcall is installed],[cl_cv_have_ffcall],
[if test "$ac_cv_header_avcall_h" = yes \
      -a "$ac_cv_header_callback_h" = yes \
      -a "$ac_cv_search___builtin_avcall" != no \
      -a "$ac_cv_search_trampoline_r_data0" != no
then cl_cv_have_ffcall=yes
else cl_cv_have_ffcall='no, consider installing GNU libffcall'
fi])
if test $cl_use_ffcall = yes -a "$cl_cv_have_ffcall" != yes; then
  if test "$ac_cv_build" = "$ac_cv_host"; then host_arg="";
  else host_arg=" --host=$ac_cv_host";
  fi
  libffcall_url='CL_LIBFFCALL_DOWNLOAD_URL'
  libffcall_targz=`echo "$libffcall_url" | sed -e 's|^.*/||'`
  libffcall_dirname=`echo "$libffcall_targz" | sed -e 's|\.tar\.gz$||'`
  AC_MSG_ERROR([despite --with-ffcall, FFCALL was not found
 Either call configure without --with-ffcall or do
  mkdir prerequisites; cd prerequisites; prefix=`pwd`/${ac_cv_host}
  wget ${libffcall_url}
  tar xfz ${libffcall_targz}
  cd ${libffcall_dirname}
  ./configure$host_arg --prefix=\${prefix} && make && make check && make install
  cd ../..
  ./configure --with-libffcall-prefix=\${prefix} ${ac_configure_args}])
fi
fi;])])
