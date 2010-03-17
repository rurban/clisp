# DO NOT EDIT! GENERATED AUTOMATICALLY!
# Copyright (C) 2002-2010 Free Software Foundation, Inc.
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
AC_DEFUN([wc_gl_EARLY],
[
  m4_pattern_forbid([^gl_[A-Z]])dnl the gnulib macro namespace
  m4_pattern_allow([^gl_ES$])dnl a valid locale name
  m4_pattern_allow([^gl_LIBOBJS$])dnl a variable
  m4_pattern_allow([^gl_LTLIBOBJS$])dnl a variable
  AC_REQUIRE([AC_PROG_RANLIB])
  # Code from module arg-nonnull:
  # Code from module fnmatch:
  # Code from module fnmatch-gnu:
])

# This macro should be invoked from ./configure.ac, in the section
# "Check for header files, types and library functions".
AC_DEFUN([wc_gl_INIT],
[
  AM_CONDITIONAL([GL_COND_LIBTOOL], [false])
  gl_cond_libtool=false
  gl_libdeps=
  gl_ltlibdeps=
  m4_pushdef([AC_LIBOBJ], m4_defn([wc_gl_LIBOBJ]))
  m4_pushdef([AC_REPLACE_FUNCS], m4_defn([wc_gl_REPLACE_FUNCS]))
  m4_pushdef([AC_LIBSOURCES], m4_defn([wc_gl_LIBSOURCES]))
  m4_pushdef([wc_gl_LIBSOURCES_LIST], [])
  m4_pushdef([wc_gl_LIBSOURCES_DIR], [])
  gl_COMMON
  gl_source_base='modules/wildcard/gllib'
  # Code from module arg-nonnull:
  # Code from module fnmatch:
  gl_FUNC_FNMATCH_POSIX
  # Code from module fnmatch-gnu:
  gl_FUNC_FNMATCH_GNU
  # Code from module dummy:
  # End of code from modules
  m4_ifval(wc_gl_LIBSOURCES_LIST, [
    m4_syscmd([test ! -d ]m4_defn([wc_gl_LIBSOURCES_DIR])[ ||
      for gl_file in ]wc_gl_LIBSOURCES_LIST[ ; do
        if test ! -r ]m4_defn([wc_gl_LIBSOURCES_DIR])[/$gl_file ; then
          echo "missing file ]m4_defn([wc_gl_LIBSOURCES_DIR])[/$gl_file" >&2
          exit 1
        fi
      done])dnl
      m4_if(m4_sysval, [0], [],
        [AC_FATAL([expected source file, required through AC_LIBSOURCES, not found])])
  ])
  m4_popdef([wc_gl_LIBSOURCES_DIR])
  m4_popdef([wc_gl_LIBSOURCES_LIST])
  m4_popdef([AC_LIBSOURCES])
  m4_popdef([AC_REPLACE_FUNCS])
  m4_popdef([AC_LIBOBJ])
  AC_CONFIG_COMMANDS_PRE([
    wc_gl_libobjs=
    wc_gl_ltlibobjs=
    if test -n "$wc_gl_LIBOBJS"; then
      # Remove the extension.
      sed_drop_objext='s/\.o$//;s/\.obj$//'
      for i in `for i in $wc_gl_LIBOBJS; do echo "$i"; done | sed -e "$sed_drop_objext" | sort | uniq`; do
        wc_gl_libobjs="$wc_gl_libobjs $i.$ac_objext"
        wc_gl_ltlibobjs="$wc_gl_ltlibobjs $i.lo"
      done
    fi
    AC_SUBST([wc_gl_LIBOBJS], [$wc_gl_libobjs])
    AC_SUBST([wc_gl_LTLIBOBJS], [$wc_gl_ltlibobjs])
  ])
  gltests_libdeps=
  gltests_ltlibdeps=
  m4_pushdef([AC_LIBOBJ], m4_defn([wc_gltests_LIBOBJ]))
  m4_pushdef([AC_REPLACE_FUNCS], m4_defn([wc_gltests_REPLACE_FUNCS]))
  m4_pushdef([AC_LIBSOURCES], m4_defn([wc_gltests_LIBSOURCES]))
  m4_pushdef([wc_gltests_LIBSOURCES_LIST], [])
  m4_pushdef([wc_gltests_LIBSOURCES_DIR], [])
  gl_COMMON
  gl_source_base='tests'
  m4_ifval(wc_gltests_LIBSOURCES_LIST, [
    m4_syscmd([test ! -d ]m4_defn([wc_gltests_LIBSOURCES_DIR])[ ||
      for gl_file in ]wc_gltests_LIBSOURCES_LIST[ ; do
        if test ! -r ]m4_defn([wc_gltests_LIBSOURCES_DIR])[/$gl_file ; then
          echo "missing file ]m4_defn([wc_gltests_LIBSOURCES_DIR])[/$gl_file" >&2
          exit 1
        fi
      done])dnl
      m4_if(m4_sysval, [0], [],
        [AC_FATAL([expected source file, required through AC_LIBSOURCES, not found])])
  ])
  m4_popdef([wc_gltests_LIBSOURCES_DIR])
  m4_popdef([wc_gltests_LIBSOURCES_LIST])
  m4_popdef([AC_LIBSOURCES])
  m4_popdef([AC_REPLACE_FUNCS])
  m4_popdef([AC_LIBOBJ])
  AC_CONFIG_COMMANDS_PRE([
    wc_gltests_libobjs=
    wc_gltests_ltlibobjs=
    if test -n "$wc_gltests_LIBOBJS"; then
      # Remove the extension.
      sed_drop_objext='s/\.o$//;s/\.obj$//'
      for i in `for i in $wc_gltests_LIBOBJS; do echo "$i"; done | sed -e "$sed_drop_objext" | sort | uniq`; do
        wc_gltests_libobjs="$wc_gltests_libobjs $i.$ac_objext"
        wc_gltests_ltlibobjs="$wc_gltests_ltlibobjs $i.lo"
      done
    fi
    AC_SUBST([wc_gltests_LIBOBJS], [$wc_gltests_libobjs])
    AC_SUBST([wc_gltests_LTLIBOBJS], [$wc_gltests_ltlibobjs])
  ])
  LIBGNU_LIBDEPS="$gl_libdeps"
  AC_SUBST([LIBGNU_LIBDEPS])
  LIBGNU_LTLIBDEPS="$gl_ltlibdeps"
  AC_SUBST([LIBGNU_LTLIBDEPS])
])

# Like AC_LIBOBJ, except that the module name goes
# into wc_gl_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([wc_gl_LIBOBJ], [
  AS_LITERAL_IF([$1], [wc_gl_LIBSOURCES([$1.c])])dnl
  wc_gl_LIBOBJS="$wc_gl_LIBOBJS $1.$ac_objext"
])

# Like AC_REPLACE_FUNCS, except that the module name goes
# into wc_gl_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([wc_gl_REPLACE_FUNCS], [
  m4_foreach_w([gl_NAME], [$1], [AC_LIBSOURCES(gl_NAME[.c])])dnl
  AC_CHECK_FUNCS([$1], , [wc_gl_LIBOBJ($ac_func)])
])

# Like AC_LIBSOURCES, except the directory where the source file is
# expected is derived from the gnulib-tool parameterization,
# and alloca is special cased (for the alloca-opt module).
# We could also entirely rely on EXTRA_lib..._SOURCES.
AC_DEFUN([wc_gl_LIBSOURCES], [
  m4_foreach([_gl_NAME], [$1], [
    m4_if(_gl_NAME, [alloca.c], [], [
      m4_define([wc_gl_LIBSOURCES_DIR], [modules/wildcard/gllib])
      m4_append([wc_gl_LIBSOURCES_LIST], _gl_NAME, [ ])
    ])
  ])
])

# Like AC_LIBOBJ, except that the module name goes
# into wc_gltests_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([wc_gltests_LIBOBJ], [
  AS_LITERAL_IF([$1], [wc_gltests_LIBSOURCES([$1.c])])dnl
  wc_gltests_LIBOBJS="$wc_gltests_LIBOBJS $1.$ac_objext"
])

# Like AC_REPLACE_FUNCS, except that the module name goes
# into wc_gltests_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([wc_gltests_REPLACE_FUNCS], [
  m4_foreach_w([gl_NAME], [$1], [AC_LIBSOURCES(gl_NAME[.c])])dnl
  AC_CHECK_FUNCS([$1], , [wc_gltests_LIBOBJ($ac_func)])
])

# Like AC_LIBSOURCES, except the directory where the source file is
# expected is derived from the gnulib-tool parameterization,
# and alloca is special cased (for the alloca-opt module).
# We could also entirely rely on EXTRA_lib..._SOURCES.
AC_DEFUN([wc_gltests_LIBSOURCES], [
  m4_foreach([_gl_NAME], [$1], [
    m4_if(_gl_NAME, [alloca.c], [], [
      m4_define([wc_gltests_LIBSOURCES_DIR], [tests])
      m4_append([wc_gltests_LIBSOURCES_LIST], _gl_NAME, [ ])
    ])
  ])
])

# This macro records the list of files which have been installed by
# gnulib-tool and may be removed by future gnulib-tool invocations.
AC_DEFUN([wc_gl_FILE_LIST], [
  build-aux/arg-nonnull.h
  lib/dummy.c
  lib/fnmatch.c
  lib/fnmatch.in.h
  lib/fnmatch_loop.c
  m4/00gnulib.m4
  m4/fnmatch.m4
  m4/gnulib-common.m4
  m4/mbstate_t.m4
])
