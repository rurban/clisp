# DO NOT EDIT! GENERATED AUTOMATICALLY!
# Copyright (C) 2002-2009 Free Software Foundation, Inc.
#
# This file is free software, distributed under the terms of the GNU
# General Public License.  As a special exception to the GNU General
# Public License, this file may be distributed as part of a program
# that contains a configuration script generated by Autoconf, under
# the same distribution terms as the rest of that program.
#
# Generated by gnulib-tool.
#
# This file represents the compiled summary of the specification in
# gnulib-cache.m4. It lists the computed macro invocations that need
# to be invoked from configure.ac.
# In projects using CVS, this file can be treated like other built files.


# This macro should be invoked from ./configure.ac, in the section
# "Checks for programs", right after AC_PROG_CC, and certainly before
# any checks for libraries, header files, types and library functions.
AC_DEFUN([rx_gl_EARLY],
[
  m4_pattern_forbid([^gl_[A-Z]])dnl the gnulib macro namespace
  m4_pattern_allow([^gl_ES$])dnl a valid locale name
  m4_pattern_allow([^gl_LIBOBJS$])dnl a variable
  m4_pattern_allow([^gl_LTLIBOBJS$])dnl a variable
  AC_REQUIRE([AC_PROG_RANLIB])
])

# This macro should be invoked from ./configure.ac, in the section
# "Check for header files, types and library functions".
AC_DEFUN([rx_gl_INIT],
[
  AM_CONDITIONAL([GL_COND_LIBTOOL], [false])
  gl_cond_libtool=false
  gl_libdeps=
  gl_ltlibdeps=
  m4_pushdef([AC_LIBOBJ], m4_defn([rx_gl_LIBOBJ]))
  m4_pushdef([AC_REPLACE_FUNCS], m4_defn([rx_gl_REPLACE_FUNCS]))
  m4_pushdef([AC_LIBSOURCES], m4_defn([rx_gl_LIBSOURCES]))
  m4_pushdef([rx_gl_LIBSOURCES_LIST], [])
  m4_pushdef([rx_gl_LIBSOURCES_DIR], [])
  gl_COMMON
  gl_source_base='modules/regexp/gllib'
  gl_FUNC_BTOWC
  gl_WCHAR_MODULE_INDICATOR([btowc])
  AC_SUBST([LIBINTL])
  AC_SUBST([LTLIBINTL])
  AC_FUNC_MALLOC
  AC_DEFINE([GNULIB_MALLOC_GNU], 1, [Define to indicate the 'malloc' module.])
  gl_FUNC_MALLOC_POSIX
  gl_STDLIB_MODULE_INDICATOR([malloc-posix])
  gl_REGEX
  gt_TYPE_SSIZE_T
  gl_STDDEF_H
  gl_STDLIB_H
  gl_UNISTD_H
  gl_FUNC_WCRTOMB
  gl_WCHAR_MODULE_INDICATOR([wcrtomb])
  m4_ifval(rx_gl_LIBSOURCES_LIST, [
    m4_syscmd([test ! -d ]m4_defn([rx_gl_LIBSOURCES_DIR])[ ||
      for gl_file in ]rx_gl_LIBSOURCES_LIST[ ; do
        if test ! -r ]m4_defn([rx_gl_LIBSOURCES_DIR])[/$gl_file ; then
          echo "missing file ]m4_defn([rx_gl_LIBSOURCES_DIR])[/$gl_file" >&2
          exit 1
        fi
      done])dnl
      m4_if(m4_sysval, [0], [],
        [AC_FATAL([expected source file, required through AC_LIBSOURCES, not found])])
  ])
  m4_popdef([rx_gl_LIBSOURCES_DIR])
  m4_popdef([rx_gl_LIBSOURCES_LIST])
  m4_popdef([AC_LIBSOURCES])
  m4_popdef([AC_REPLACE_FUNCS])
  m4_popdef([AC_LIBOBJ])
  AC_CONFIG_COMMANDS_PRE([
    rx_gl_libobjs=
    rx_gl_ltlibobjs=
    if test -n "$rx_gl_LIBOBJS"; then
      # Remove the extension.
      sed_drop_objext='s/\.o$//;s/\.obj$//'
      for i in `for i in $rx_gl_LIBOBJS; do echo "$i"; done | sed "$sed_drop_objext" | sort | uniq`; do
        rx_gl_libobjs="$rx_gl_libobjs $i.$ac_objext"
        rx_gl_ltlibobjs="$rx_gl_ltlibobjs $i.lo"
      done
    fi
    AC_SUBST([rx_gl_LIBOBJS], [$rx_gl_libobjs])
    AC_SUBST([rx_gl_LTLIBOBJS], [$rx_gl_ltlibobjs])
  ])
  gltests_libdeps=
  gltests_ltlibdeps=
  m4_pushdef([AC_LIBOBJ], m4_defn([rx_gltests_LIBOBJ]))
  m4_pushdef([AC_REPLACE_FUNCS], m4_defn([rx_gltests_REPLACE_FUNCS]))
  m4_pushdef([AC_LIBSOURCES], m4_defn([rx_gltests_LIBSOURCES]))
  m4_pushdef([rx_gltests_LIBSOURCES_LIST], [])
  m4_pushdef([rx_gltests_LIBSOURCES_DIR], [])
  gl_COMMON
  gl_source_base='tests'
  m4_ifval(rx_gltests_LIBSOURCES_LIST, [
    m4_syscmd([test ! -d ]m4_defn([rx_gltests_LIBSOURCES_DIR])[ ||
      for gl_file in ]rx_gltests_LIBSOURCES_LIST[ ; do
        if test ! -r ]m4_defn([rx_gltests_LIBSOURCES_DIR])[/$gl_file ; then
          echo "missing file ]m4_defn([rx_gltests_LIBSOURCES_DIR])[/$gl_file" >&2
          exit 1
        fi
      done])dnl
      m4_if(m4_sysval, [0], [],
        [AC_FATAL([expected source file, required through AC_LIBSOURCES, not found])])
  ])
  m4_popdef([rx_gltests_LIBSOURCES_DIR])
  m4_popdef([rx_gltests_LIBSOURCES_LIST])
  m4_popdef([AC_LIBSOURCES])
  m4_popdef([AC_REPLACE_FUNCS])
  m4_popdef([AC_LIBOBJ])
  AC_CONFIG_COMMANDS_PRE([
    rx_gltests_libobjs=
    rx_gltests_ltlibobjs=
    if test -n "$rx_gltests_LIBOBJS"; then
      # Remove the extension.
      sed_drop_objext='s/\.o$//;s/\.obj$//'
      for i in `for i in $rx_gltests_LIBOBJS; do echo "$i"; done | sed "$sed_drop_objext" | sort | uniq`; do
        rx_gltests_libobjs="$rx_gltests_libobjs $i.$ac_objext"
        rx_gltests_ltlibobjs="$rx_gltests_ltlibobjs $i.lo"
      done
    fi
    AC_SUBST([rx_gltests_LIBOBJS], [$rx_gltests_libobjs])
    AC_SUBST([rx_gltests_LTLIBOBJS], [$rx_gltests_ltlibobjs])
  ])
  LIBGNU_LIBDEPS="$gl_libdeps"
  AC_SUBST([LIBGNU_LIBDEPS])
  LIBGNU_LTLIBDEPS="$gl_ltlibdeps"
  AC_SUBST([LIBGNU_LTLIBDEPS])
])

# Like AC_LIBOBJ, except that the module name goes
# into rx_gl_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([rx_gl_LIBOBJ], [
  AS_LITERAL_IF([$1], [rx_gl_LIBSOURCES([$1.c])])dnl
  rx_gl_LIBOBJS="$rx_gl_LIBOBJS $1.$ac_objext"
])

# Like AC_REPLACE_FUNCS, except that the module name goes
# into rx_gl_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([rx_gl_REPLACE_FUNCS], [
  m4_foreach_w([gl_NAME], [$1], [AC_LIBSOURCES(gl_NAME[.c])])dnl
  AC_CHECK_FUNCS([$1], , [rx_gl_LIBOBJ($ac_func)])
])

# Like AC_LIBSOURCES, except the directory where the source file is
# expected is derived from the gnulib-tool parameterization,
# and alloca is special cased (for the alloca-opt module).
# We could also entirely rely on EXTRA_lib..._SOURCES.
AC_DEFUN([rx_gl_LIBSOURCES], [
  m4_foreach([_gl_NAME], [$1], [
    m4_if(_gl_NAME, [alloca.c], [], [
      m4_define([rx_gl_LIBSOURCES_DIR], [modules/regexp/gllib])
      m4_append([rx_gl_LIBSOURCES_LIST], _gl_NAME, [ ])
    ])
  ])
])

# Like AC_LIBOBJ, except that the module name goes
# into rx_gltests_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([rx_gltests_LIBOBJ], [
  AS_LITERAL_IF([$1], [rx_gltests_LIBSOURCES([$1.c])])dnl
  rx_gltests_LIBOBJS="$rx_gltests_LIBOBJS $1.$ac_objext"
])

# Like AC_REPLACE_FUNCS, except that the module name goes
# into rx_gltests_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([rx_gltests_REPLACE_FUNCS], [
  m4_foreach_w([gl_NAME], [$1], [AC_LIBSOURCES(gl_NAME[.c])])dnl
  AC_CHECK_FUNCS([$1], , [rx_gltests_LIBOBJ($ac_func)])
])

# Like AC_LIBSOURCES, except the directory where the source file is
# expected is derived from the gnulib-tool parameterization,
# and alloca is special cased (for the alloca-opt module).
# We could also entirely rely on EXTRA_lib..._SOURCES.
AC_DEFUN([rx_gltests_LIBSOURCES], [
  m4_foreach([_gl_NAME], [$1], [
    m4_if(_gl_NAME, [alloca.c], [], [
      m4_define([rx_gltests_LIBSOURCES_DIR], [tests])
      m4_append([rx_gltests_LIBSOURCES_LIST], _gl_NAME, [ ])
    ])
  ])
])

# This macro records the list of files which have been installed by
# gnulib-tool and may be removed by future gnulib-tool invocations.
AC_DEFUN([rx_gl_FILE_LIST], [
  build-aux/link-warning.h
  lib/btowc.c
  lib/dummy.c
  lib/gettext.h
  lib/malloc.c
  lib/regcomp.c
  lib/regex.c
  lib/regex.h
  lib/regex_internal.c
  lib/regex_internal.h
  lib/regexec.c
  lib/stddef.in.h
  lib/stdlib.in.h
  lib/unistd.in.h
  lib/wcrtomb.c
  m4/00gnulib.m4
  m4/btowc.m4
  m4/codeset.m4
  m4/gnulib-common.m4
  m4/locale-fr.m4
  m4/locale-ja.m4
  m4/locale-zh.m4
  m4/malloc.m4
  m4/mbrtowc.m4
  m4/mbstate_t.m4
  m4/regex.m4
  m4/ssize_t.m4
  m4/stddef_h.m4
  m4/stdlib_h.m4
  m4/unistd_h.m4
  m4/wchar_t.m4
  m4/wcrtomb.m4
])
