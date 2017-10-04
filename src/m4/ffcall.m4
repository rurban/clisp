dnl -*- Autoconf -*-
dnl Copyright (C) 2007-2010, 2017 Sam Steingold
dnl Copyright (C) 2017 Bruno Haible
dnl This is free software, distributed under the GNU GPL v2+

AC_PREREQ([2.61])

dnl Download location of the newest libffcall release.
AC_DEFUN([CL_LIBFFCALL_DOWNLOAD_URL],
  [https://ftp.gnu.org/gnu/libffcall/libffcall-2.0.tar.gz])

AC_DEFUN([CL_FFCALL],[
  AC_ARG_WITH([ffcall],
    [AC_HELP_STRING([--with-ffcall],[use FFCALL (default is YES, if present)])],
    [cl_use_ffcall=$withval],
    [cl_use_ffcall=default])
  if test $cl_use_ffcall != no; then
    cl_save_CPPFLAGS="$CPPFLAGS"
    cl_save_LIBS="$LIBS"
    found_libffcall=
    dnl First, search for libffcall (installed by libffcall >= 2.0).
    AC_LIB_LINKFLAGS([ffcall])
    AC_LIB_APPENDTOVAR([LIBS], [$LIBFFCALL])
    unset ac_cv_header_avcall_h
    unset ac_cv_header_callback_h
    AC_CHECK_HEADERS([avcall.h callback.h])
    if test "$ac_cv_header_avcall_h" = yes -a "$ac_cv_header_callback_h" = yes; then
      AC_SEARCH_LIBS([ffcall_get_version])
      if test "$ac_cv_search_ffcall_get_version" != no; then
        found_libffcall=yes
      fi
    fi
    if test -z "$found_libffcall"; then
      CPPFLAGS="$cl_save_CPPFLAGS"
      LIBS="$cl_save_LIBS"
      dnl Second, search for libavcall and libcallback (installed by libffcall < 2.0).
      AC_LIB_FROMPACKAGE([avcall], [libffcall])
      AC_LIB_FROMPACKAGE([callback], [libffcall])
      AC_LIB_LINKFLAGS([avcall])
      AC_LIB_LINKFLAGS([callback])
      AC_LIB_APPENDTOVAR([LIBS], [$LIBAVCALL])
      AC_LIB_APPENDTOVAR([LIBS], [$LIBCALLBACK])
      unset ac_cv_header_avcall_h
      unset ac_cv_header_callback_h
      AC_CHECK_HEADERS([avcall.h callback.h])
      if test "$ac_cv_header_avcall_h" = yes -a "$ac_cv_header_callback_h" = yes; then
        AC_SEARCH_LIBS([__builtin_avcall])
        AC_SEARCH_LIBS([trampoline_r_data0])
        if test "$ac_cv_search___builtin_avcall" != no -a "$ac_cv_search_trampoline_r_data0" != no; then
          found_libffcall=yes
        fi
      fi
      if test -z "$found_libffcall"; then
        CPPFLAGS="$cl_save_CPPFLAGS"
        LIBS="$cl_save_LIBS"
      fi
    fi
    AC_CACHE_CHECK([whether libffcall is installed], [cl_cv_have_ffcall],
      [if test -n "$found_libffcall"; then
         cl_cv_have_ffcall=yes
       else
         cl_cv_have_ffcall='no, consider installing GNU libffcall'
       fi
      ])
    if test $cl_use_ffcall = yes -a "$cl_cv_have_ffcall" != yes; then
      if test "$ac_cv_build" = "$ac_cv_host"; then
        host_arg=""
      else
        host_arg=" --host=$ac_cv_host"
      fi
      libffcall_url='CL_LIBFFCALL_DOWNLOAD_URL'
      libffcall_targz=`echo "$libffcall_url" | sed -e 's|^.*/||'`
      libffcall_dirname=`echo "$libffcall_targz" | sed -e 's|\.tar\.gz$||'`
      AC_MSG_ERROR([Despite --with-ffcall, LIBFFCALL was not found
 Either call configure without --with-ffcall or do
  mkdir prerequisites; cd prerequisites; prefix=\$(pwd)/${ac_cv_host}
  wget ${libffcall_url}
  tar xfz ${libffcall_targz}
  cd ${libffcall_dirname}
  ./configure$host_arg --prefix=\${prefix} && make && make check && make install
  cd ../..
  ./configure --with-libffcall-prefix=\${prefix} ${ac_configure_args}])
    fi
  fi
])
