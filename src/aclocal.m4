# generated automatically by aclocal 1.10.1 -*- Autoconf -*-

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
# 2005, 2006, 2007, 2008  Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

m4_ifndef([AC_AUTOCONF_VERSION],
  [m4_copy([m4_PACKAGE_VERSION], [AC_AUTOCONF_VERSION])])dnl
m4_if(AC_AUTOCONF_VERSION, [2.62],,
[m4_warning([this file was generated for autoconf 2.62.
You have another version of autoconf.  It may work, but is not guaranteed to.
If you have problems, you may need to regenerate the build system entirely.
To do so, use the procedure documented by the package, typically `autoreconf'.])])

# Copyright (C) 2002, 2003, 2005, 2006, 2007  Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_AUTOMAKE_VERSION(VERSION)
# ----------------------------
# Automake X.Y traces this macro to ensure aclocal.m4 has been
# generated from the m4 files accompanying Automake X.Y.
# (This private macro should not be called outside this file.)
AC_DEFUN([AM_AUTOMAKE_VERSION],
[am__api_version='1.10'
dnl Some users find AM_AUTOMAKE_VERSION and mistake it for a way to
dnl require some minimum version.  Point them to the right macro.
m4_if([$1], [1.10.1], [],
      [AC_FATAL([Do not call $0, use AM_INIT_AUTOMAKE([$1]).])])dnl
])

# _AM_AUTOCONF_VERSION(VERSION)
# -----------------------------
# aclocal traces this macro to find the Autoconf version.
# This is a private macro too.  Using m4_define simplifies
# the logic in aclocal, which can simply ignore this definition.
m4_define([_AM_AUTOCONF_VERSION], [])

# AM_SET_CURRENT_AUTOMAKE_VERSION
# -------------------------------
# Call AM_AUTOMAKE_VERSION and AM_AUTOMAKE_VERSION so they can be traced.
# This function is AC_REQUIREd by AC_INIT_AUTOMAKE.
AC_DEFUN([AM_SET_CURRENT_AUTOMAKE_VERSION],
[AM_AUTOMAKE_VERSION([1.10.1])dnl
m4_ifndef([AC_AUTOCONF_VERSION],
  [m4_copy([m4_PACKAGE_VERSION], [AC_AUTOCONF_VERSION])])dnl
_AM_AUTOCONF_VERSION(AC_AUTOCONF_VERSION)])

# AM_AUX_DIR_EXPAND                                         -*- Autoconf -*-

# Copyright (C) 2001, 2003, 2005  Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# For projects using AC_CONFIG_AUX_DIR([foo]), Autoconf sets
# $ac_aux_dir to `$srcdir/foo'.  In other projects, it is set to
# `$srcdir', `$srcdir/..', or `$srcdir/../..'.
#
# Of course, Automake must honor this variable whenever it calls a
# tool from the auxiliary directory.  The problem is that $srcdir (and
# therefore $ac_aux_dir as well) can be either absolute or relative,
# depending on how configure is run.  This is pretty annoying, since
# it makes $ac_aux_dir quite unusable in subdirectories: in the top
# source directory, any form will work fine, but in subdirectories a
# relative path needs to be adjusted first.
#
# $ac_aux_dir/missing
#    fails when called from a subdirectory if $ac_aux_dir is relative
# $top_srcdir/$ac_aux_dir/missing
#    fails if $ac_aux_dir is absolute,
#    fails when called from a subdirectory in a VPATH build with
#          a relative $ac_aux_dir
#
# The reason of the latter failure is that $top_srcdir and $ac_aux_dir
# are both prefixed by $srcdir.  In an in-source build this is usually
# harmless because $srcdir is `.', but things will broke when you
# start a VPATH build or use an absolute $srcdir.
#
# So we could use something similar to $top_srcdir/$ac_aux_dir/missing,
# iff we strip the leading $srcdir from $ac_aux_dir.  That would be:
#   am_aux_dir='\$(top_srcdir)/'`expr "$ac_aux_dir" : "$srcdir//*\(.*\)"`
# and then we would define $MISSING as
#   MISSING="\${SHELL} $am_aux_dir/missing"
# This will work as long as MISSING is not called from configure, because
# unfortunately $(top_srcdir) has no meaning in configure.
# However there are other variables, like CC, which are often used in
# configure, and could therefore not use this "fixed" $ac_aux_dir.
#
# Another solution, used here, is to always expand $ac_aux_dir to an
# absolute PATH.  The drawback is that using absolute paths prevent a
# configured tree to be moved without reconfiguration.

AC_DEFUN([AM_AUX_DIR_EXPAND],
[dnl Rely on autoconf to set up CDPATH properly.
AC_PREREQ([2.50])dnl
# expand $ac_aux_dir to an absolute path
am_aux_dir=`cd $ac_aux_dir && pwd`
])

# AM_CONDITIONAL                                            -*- Autoconf -*-

# Copyright (C) 1997, 2000, 2001, 2003, 2004, 2005, 2006
# Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# serial 8

# AM_CONDITIONAL(NAME, SHELL-CONDITION)
# -------------------------------------
# Define a conditional.
AC_DEFUN([AM_CONDITIONAL],
[AC_PREREQ(2.52)dnl
 ifelse([$1], [TRUE],  [AC_FATAL([$0: invalid condition: $1])],
	[$1], [FALSE], [AC_FATAL([$0: invalid condition: $1])])dnl
AC_SUBST([$1_TRUE])dnl
AC_SUBST([$1_FALSE])dnl
_AM_SUBST_NOTMAKE([$1_TRUE])dnl
_AM_SUBST_NOTMAKE([$1_FALSE])dnl
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi
AC_CONFIG_COMMANDS_PRE(
[if test -z "${$1_TRUE}" && test -z "${$1_FALSE}"; then
  AC_MSG_ERROR([[conditional "$1" was never defined.
Usually this means the macro was only invoked conditionally.]])
fi])])

# Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006
# Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# serial 9

# There are a few dirty hacks below to avoid letting `AC_PROG_CC' be
# written in clear, in which case automake, when reading aclocal.m4,
# will think it sees a *use*, and therefore will trigger all it's
# C support machinery.  Also note that it means that autoscan, seeing
# CC etc. in the Makefile, will ask for an AC_PROG_CC use...


# _AM_DEPENDENCIES(NAME)
# ----------------------
# See how the compiler implements dependency checking.
# NAME is "CC", "CXX", "GCJ", or "OBJC".
# We try a few techniques and use that to set a single cache variable.
#
# We don't AC_REQUIRE the corresponding AC_PROG_CC since the latter was
# modified to invoke _AM_DEPENDENCIES(CC); we would have a circular
# dependency, and given that the user is not expected to run this macro,
# just rely on AC_PROG_CC.
AC_DEFUN([_AM_DEPENDENCIES],
[AC_REQUIRE([AM_SET_DEPDIR])dnl
AC_REQUIRE([AM_OUTPUT_DEPENDENCY_COMMANDS])dnl
AC_REQUIRE([AM_MAKE_INCLUDE])dnl
AC_REQUIRE([AM_DEP_TRACK])dnl

ifelse([$1], CC,   [depcc="$CC"   am_compiler_list=],
       [$1], CXX,  [depcc="$CXX"  am_compiler_list=],
       [$1], OBJC, [depcc="$OBJC" am_compiler_list='gcc3 gcc'],
       [$1], UPC,  [depcc="$UPC"  am_compiler_list=],
       [$1], GCJ,  [depcc="$GCJ"  am_compiler_list='gcc3 gcc'],
                   [depcc="$$1"   am_compiler_list=])

AC_CACHE_CHECK([dependency style of $depcc],
               [am_cv_$1_dependencies_compiler_type],
[if test -z "$AMDEP_TRUE" && test -f "$am_depcomp"; then
  # We make a subdir and do the tests there.  Otherwise we can end up
  # making bogus files that we don't know about and never remove.  For
  # instance it was reported that on HP-UX the gcc test will end up
  # making a dummy file named `D' -- because `-MD' means `put the output
  # in D'.
  mkdir conftest.dir
  # Copy depcomp to subdir because otherwise we won't find it if we're
  # using a relative directory.
  cp "$am_depcomp" conftest.dir
  cd conftest.dir
  # We will build objects and dependencies in a subdirectory because
  # it helps to detect inapplicable dependency modes.  For instance
  # both Tru64's cc and ICC support -MD to output dependencies as a
  # side effect of compilation, but ICC will put the dependencies in
  # the current directory while Tru64 will put them in the object
  # directory.
  mkdir sub

  am_cv_$1_dependencies_compiler_type=none
  if test "$am_compiler_list" = ""; then
     am_compiler_list=`sed -n ['s/^#*\([a-zA-Z0-9]*\))$/\1/p'] < ./depcomp`
  fi
  for depmode in $am_compiler_list; do
    # Setup a source with many dependencies, because some compilers
    # like to wrap large dependency lists on column 80 (with \), and
    # we should not choose a depcomp mode which is confused by this.
    #
    # We need to recreate these files for each test, as the compiler may
    # overwrite some of them when testing with obscure command lines.
    # This happens at least with the AIX C compiler.
    : > sub/conftest.c
    for i in 1 2 3 4 5 6; do
      echo '#include "conftst'$i'.h"' >> sub/conftest.c
      # Using `: > sub/conftst$i.h' creates only sub/conftst1.h with
      # Solaris 8's {/usr,}/bin/sh.
      touch sub/conftst$i.h
    done
    echo "${am__include} ${am__quote}sub/conftest.Po${am__quote}" > confmf

    case $depmode in
    nosideeffect)
      # after this tag, mechanisms are not by side-effect, so they'll
      # only be used when explicitly requested
      if test "x$enable_dependency_tracking" = xyes; then
	continue
      else
	break
      fi
      ;;
    none) break ;;
    esac
    # We check with `-c' and `-o' for the sake of the "dashmstdout"
    # mode.  It turns out that the SunPro C++ compiler does not properly
    # handle `-M -o', and we need to detect this.
    if depmode=$depmode \
       source=sub/conftest.c object=sub/conftest.${OBJEXT-o} \
       depfile=sub/conftest.Po tmpdepfile=sub/conftest.TPo \
       $SHELL ./depcomp $depcc -c -o sub/conftest.${OBJEXT-o} sub/conftest.c \
         >/dev/null 2>conftest.err &&
       grep sub/conftst1.h sub/conftest.Po > /dev/null 2>&1 &&
       grep sub/conftst6.h sub/conftest.Po > /dev/null 2>&1 &&
       grep sub/conftest.${OBJEXT-o} sub/conftest.Po > /dev/null 2>&1 &&
       ${MAKE-make} -s -f confmf > /dev/null 2>&1; then
      # icc doesn't choke on unknown options, it will just issue warnings
      # or remarks (even with -Werror).  So we grep stderr for any message
      # that says an option was ignored or not supported.
      # When given -MP, icc 7.0 and 7.1 complain thusly:
      #   icc: Command line warning: ignoring option '-M'; no argument required
      # The diagnosis changed in icc 8.0:
      #   icc: Command line remark: option '-MP' not supported
      if (grep 'ignoring option' conftest.err ||
          grep 'not supported' conftest.err) >/dev/null 2>&1; then :; else
        am_cv_$1_dependencies_compiler_type=$depmode
        break
      fi
    fi
  done

  cd ..
  rm -rf conftest.dir
else
  am_cv_$1_dependencies_compiler_type=none
fi
])
AC_SUBST([$1DEPMODE], [depmode=$am_cv_$1_dependencies_compiler_type])
AM_CONDITIONAL([am__fastdep$1], [
  test "x$enable_dependency_tracking" != xno \
  && test "$am_cv_$1_dependencies_compiler_type" = gcc3])
])


# AM_SET_DEPDIR
# -------------
# Choose a directory name for dependency files.
# This macro is AC_REQUIREd in _AM_DEPENDENCIES
AC_DEFUN([AM_SET_DEPDIR],
[AC_REQUIRE([AM_SET_LEADING_DOT])dnl
AC_SUBST([DEPDIR], ["${am__leading_dot}deps"])dnl
])


# AM_DEP_TRACK
# ------------
AC_DEFUN([AM_DEP_TRACK],
[AC_ARG_ENABLE(dependency-tracking,
[  --disable-dependency-tracking  speeds up one-time build
  --enable-dependency-tracking   do not reject slow dependency extractors])
if test "x$enable_dependency_tracking" != xno; then
  am_depcomp="$ac_aux_dir/depcomp"
  AMDEPBACKSLASH='\'
fi
AM_CONDITIONAL([AMDEP], [test "x$enable_dependency_tracking" != xno])
AC_SUBST([AMDEPBACKSLASH])dnl
_AM_SUBST_NOTMAKE([AMDEPBACKSLASH])dnl
])

# Generate code to set up dependency tracking.              -*- Autoconf -*-

# Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005
# Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

#serial 3

# _AM_OUTPUT_DEPENDENCY_COMMANDS
# ------------------------------
AC_DEFUN([_AM_OUTPUT_DEPENDENCY_COMMANDS],
[for mf in $CONFIG_FILES; do
  # Strip MF so we end up with the name of the file.
  mf=`echo "$mf" | sed -e 's/:.*$//'`
  # Check whether this is an Automake generated Makefile or not.
  # We used to match only the files named `Makefile.in', but
  # some people rename them; so instead we look at the file content.
  # Grep'ing the first line is not enough: some people post-process
  # each Makefile.in and add a new line on top of each file to say so.
  # Grep'ing the whole file is not good either: AIX grep has a line
  # limit of 2048, but all sed's we know have understand at least 4000.
  if sed -n 's,^#.*generated by automake.*,X,p' "$mf" | grep X >/dev/null 2>&1; then
    dirpart=`AS_DIRNAME("$mf")`
  else
    continue
  fi
  # Extract the definition of DEPDIR, am__include, and am__quote
  # from the Makefile without running `make'.
  DEPDIR=`sed -n 's/^DEPDIR = //p' < "$mf"`
  test -z "$DEPDIR" && continue
  am__include=`sed -n 's/^am__include = //p' < "$mf"`
  test -z "am__include" && continue
  am__quote=`sed -n 's/^am__quote = //p' < "$mf"`
  # When using ansi2knr, U may be empty or an underscore; expand it
  U=`sed -n 's/^U = //p' < "$mf"`
  # Find all dependency output files, they are included files with
  # $(DEPDIR) in their names.  We invoke sed twice because it is the
  # simplest approach to changing $(DEPDIR) to its actual value in the
  # expansion.
  for file in `sed -n "
    s/^$am__include $am__quote\(.*(DEPDIR).*\)$am__quote"'$/\1/p' <"$mf" | \
       sed -e 's/\$(DEPDIR)/'"$DEPDIR"'/g' -e 's/\$U/'"$U"'/g'`; do
    # Make sure the directory exists.
    test -f "$dirpart/$file" && continue
    fdir=`AS_DIRNAME(["$file"])`
    AS_MKDIR_P([$dirpart/$fdir])
    # echo "creating $dirpart/$file"
    echo '# dummy' > "$dirpart/$file"
  done
done
])# _AM_OUTPUT_DEPENDENCY_COMMANDS


# AM_OUTPUT_DEPENDENCY_COMMANDS
# -----------------------------
# This macro should only be invoked once -- use via AC_REQUIRE.
#
# This code is only required when automatic dependency tracking
# is enabled.  FIXME.  This creates each `.P' file that we will
# need in order to bootstrap the dependency handling code.
AC_DEFUN([AM_OUTPUT_DEPENDENCY_COMMANDS],
[AC_CONFIG_COMMANDS([depfiles],
     [test x"$AMDEP_TRUE" != x"" || _AM_OUTPUT_DEPENDENCY_COMMANDS],
     [AMDEP_TRUE="$AMDEP_TRUE" ac_aux_dir="$ac_aux_dir"])
])

# Do all the work for Automake.                             -*- Autoconf -*-

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
# 2005, 2006, 2008 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# serial 13

# This macro actually does too much.  Some checks are only needed if
# your package does certain things.  But this isn't really a big deal.

# AM_INIT_AUTOMAKE(PACKAGE, VERSION, [NO-DEFINE])
# AM_INIT_AUTOMAKE([OPTIONS])
# -----------------------------------------------
# The call with PACKAGE and VERSION arguments is the old style
# call (pre autoconf-2.50), which is being phased out.  PACKAGE
# and VERSION should now be passed to AC_INIT and removed from
# the call to AM_INIT_AUTOMAKE.
# We support both call styles for the transition.  After
# the next Automake release, Autoconf can make the AC_INIT
# arguments mandatory, and then we can depend on a new Autoconf
# release and drop the old call support.
AC_DEFUN([AM_INIT_AUTOMAKE],
[AC_PREREQ([2.60])dnl
dnl Autoconf wants to disallow AM_ names.  We explicitly allow
dnl the ones we care about.
m4_pattern_allow([^AM_[A-Z]+FLAGS$])dnl
AC_REQUIRE([AM_SET_CURRENT_AUTOMAKE_VERSION])dnl
AC_REQUIRE([AC_PROG_INSTALL])dnl
if test "`cd $srcdir && pwd`" != "`pwd`"; then
  # Use -I$(srcdir) only when $(srcdir) != ., so that make's output
  # is not polluted with repeated "-I."
  AC_SUBST([am__isrc], [' -I$(srcdir)'])_AM_SUBST_NOTMAKE([am__isrc])dnl
  # test to see if srcdir already configured
  if test -f $srcdir/config.status; then
    AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
  fi
fi

# test whether we have cygpath
if test -z "$CYGPATH_W"; then
  if (cygpath --version) >/dev/null 2>/dev/null; then
    CYGPATH_W='cygpath -w'
  else
    CYGPATH_W=echo
  fi
fi
AC_SUBST([CYGPATH_W])

# Define the identity of the package.
dnl Distinguish between old-style and new-style calls.
m4_ifval([$2],
[m4_ifval([$3], [_AM_SET_OPTION([no-define])])dnl
 AC_SUBST([PACKAGE], [$1])dnl
 AC_SUBST([VERSION], [$2])],
[_AM_SET_OPTIONS([$1])dnl
dnl Diagnose old-style AC_INIT with new-style AM_AUTOMAKE_INIT.
m4_if(m4_ifdef([AC_PACKAGE_NAME], 1)m4_ifdef([AC_PACKAGE_VERSION], 1), 11,,
  [m4_fatal([AC_INIT should be called with package and version arguments])])dnl
 AC_SUBST([PACKAGE], ['AC_PACKAGE_TARNAME'])dnl
 AC_SUBST([VERSION], ['AC_PACKAGE_VERSION'])])dnl

_AM_IF_OPTION([no-define],,
[AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
 AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package])])dnl

# Some tools Automake needs.
AC_REQUIRE([AM_SANITY_CHECK])dnl
AC_REQUIRE([AC_ARG_PROGRAM])dnl
AM_MISSING_PROG(ACLOCAL, aclocal-${am__api_version})
AM_MISSING_PROG(AUTOCONF, autoconf)
AM_MISSING_PROG(AUTOMAKE, automake-${am__api_version})
AM_MISSING_PROG(AUTOHEADER, autoheader)
AM_MISSING_PROG(MAKEINFO, makeinfo)
AM_PROG_INSTALL_SH
AM_PROG_INSTALL_STRIP
AC_REQUIRE([AM_PROG_MKDIR_P])dnl
# We need awk for the "check" target.  The system "awk" is bad on
# some platforms.
AC_REQUIRE([AC_PROG_AWK])dnl
AC_REQUIRE([AC_PROG_MAKE_SET])dnl
AC_REQUIRE([AM_SET_LEADING_DOT])dnl
_AM_IF_OPTION([tar-ustar], [_AM_PROG_TAR([ustar])],
              [_AM_IF_OPTION([tar-pax], [_AM_PROG_TAR([pax])],
	      		     [_AM_PROG_TAR([v7])])])
_AM_IF_OPTION([no-dependencies],,
[AC_PROVIDE_IFELSE([AC_PROG_CC],
                  [_AM_DEPENDENCIES(CC)],
                  [define([AC_PROG_CC],
                          defn([AC_PROG_CC])[_AM_DEPENDENCIES(CC)])])dnl
AC_PROVIDE_IFELSE([AC_PROG_CXX],
                  [_AM_DEPENDENCIES(CXX)],
                  [define([AC_PROG_CXX],
                          defn([AC_PROG_CXX])[_AM_DEPENDENCIES(CXX)])])dnl
AC_PROVIDE_IFELSE([AC_PROG_OBJC],
                  [_AM_DEPENDENCIES(OBJC)],
                  [define([AC_PROG_OBJC],
                          defn([AC_PROG_OBJC])[_AM_DEPENDENCIES(OBJC)])])dnl
])
])


# When config.status generates a header, we must update the stamp-h file.
# This file resides in the same directory as the config header
# that is generated.  The stamp files are numbered to have different names.

# Autoconf calls _AC_AM_CONFIG_HEADER_HOOK (when defined) in the
# loop where config.status creates the headers, so we can generate
# our stamp files there.
AC_DEFUN([_AC_AM_CONFIG_HEADER_HOOK],
[# Compute $1's index in $config_headers.
_am_arg=$1
_am_stamp_count=1
for _am_header in $config_headers :; do
  case $_am_header in
    $_am_arg | $_am_arg:* )
      break ;;
    * )
      _am_stamp_count=`expr $_am_stamp_count + 1` ;;
  esac
done
echo "timestamp for $_am_arg" >`AS_DIRNAME(["$_am_arg"])`/stamp-h[]$_am_stamp_count])

# Copyright (C) 2001, 2003, 2005  Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_PROG_INSTALL_SH
# ------------------
# Define $install_sh.
AC_DEFUN([AM_PROG_INSTALL_SH],
[AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
install_sh=${install_sh-"\$(SHELL) $am_aux_dir/install-sh"}
AC_SUBST(install_sh)])

# Copyright (C) 2003, 2005  Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# serial 2

# Check whether the underlying file-system supports filenames
# with a leading dot.  For instance MS-DOS doesn't.
AC_DEFUN([AM_SET_LEADING_DOT],
[rm -rf .tst 2>/dev/null
mkdir .tst 2>/dev/null
if test -d .tst; then
  am__leading_dot=.
else
  am__leading_dot=_
fi
rmdir .tst 2>/dev/null
AC_SUBST([am__leading_dot])])

# Check to see how 'make' treats includes.	            -*- Autoconf -*-

# Copyright (C) 2001, 2002, 2003, 2005  Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# serial 3

# AM_MAKE_INCLUDE()
# -----------------
# Check to see how make treats includes.
AC_DEFUN([AM_MAKE_INCLUDE],
[am_make=${MAKE-make}
cat > confinc << 'END'
am__doit:
	@echo done
.PHONY: am__doit
END
# If we don't find an include directive, just comment out the code.
AC_MSG_CHECKING([for style of include used by $am_make])
am__include="#"
am__quote=
_am_result=none
# First try GNU make style include.
echo "include confinc" > confmf
# We grep out `Entering directory' and `Leaving directory'
# messages which can occur if `w' ends up in MAKEFLAGS.
# In particular we don't look at `^make:' because GNU make might
# be invoked under some other name (usually "gmake"), in which
# case it prints its new name instead of `make'.
if test "`$am_make -s -f confmf 2> /dev/null | grep -v 'ing directory'`" = "done"; then
   am__include=include
   am__quote=
   _am_result=GNU
fi
# Now try BSD make style include.
if test "$am__include" = "#"; then
   echo '.include "confinc"' > confmf
   if test "`$am_make -s -f confmf 2> /dev/null`" = "done"; then
      am__include=.include
      am__quote="\""
      _am_result=BSD
   fi
fi
AC_SUBST([am__include])
AC_SUBST([am__quote])
AC_MSG_RESULT([$_am_result])
rm -f confinc confmf
])

# Copyright (C) 1999, 2000, 2001, 2003, 2004, 2005
# Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# serial 5

# AM_PROG_CC_C_O
# --------------
# Like AC_PROG_CC_C_O, but changed for automake.
AC_DEFUN([AM_PROG_CC_C_O],
[AC_REQUIRE([AC_PROG_CC_C_O])dnl
AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
AC_REQUIRE_AUX_FILE([compile])dnl
# FIXME: we rely on the cache variable name because
# there is no other way.
set dummy $CC
ac_cc=`echo $[2] | sed ['s/[^a-zA-Z0-9_]/_/g;s/^[0-9]/_/']`
if eval "test \"`echo '$ac_cv_prog_cc_'${ac_cc}_c_o`\" != yes"; then
   # Losing compiler, so override with the script.
   # FIXME: It is wrong to rewrite CC.
   # But if we don't then we get into trouble of one sort or another.
   # A longer-term fix would be to have automake use am__CC in this case,
   # and then we could set am__CC="\$(top_srcdir)/compile \$(CC)"
   CC="$am_aux_dir/compile $CC"
fi
dnl Make sure AC_PROG_CC is never called again, or it will override our
dnl setting of CC.
m4_define([AC_PROG_CC],
          [m4_fatal([AC_PROG_CC cannot be called after AM_PROG_CC_C_O])])
])

# Fake the existence of programs that GNU maintainers use.  -*- Autoconf -*-

# Copyright (C) 1997, 1999, 2000, 2001, 2003, 2004, 2005
# Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# serial 5

# AM_MISSING_PROG(NAME, PROGRAM)
# ------------------------------
AC_DEFUN([AM_MISSING_PROG],
[AC_REQUIRE([AM_MISSING_HAS_RUN])
$1=${$1-"${am_missing_run}$2"}
AC_SUBST($1)])


# AM_MISSING_HAS_RUN
# ------------------
# Define MISSING if not defined so far and test if it supports --run.
# If it does, set am_missing_run to use it, otherwise, to nothing.
AC_DEFUN([AM_MISSING_HAS_RUN],
[AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
AC_REQUIRE_AUX_FILE([missing])dnl
test x"${MISSING+set}" = xset || MISSING="\${SHELL} $am_aux_dir/missing"
# Use eval to expand $SHELL
if eval "$MISSING --run true"; then
  am_missing_run="$MISSING --run "
else
  am_missing_run=
  AC_MSG_WARN([`missing' script is too old or missing])
fi
])

# Copyright (C) 2003, 2004, 2005, 2006  Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_PROG_MKDIR_P
# ---------------
# Check for `mkdir -p'.
AC_DEFUN([AM_PROG_MKDIR_P],
[AC_PREREQ([2.60])dnl
AC_REQUIRE([AC_PROG_MKDIR_P])dnl
dnl Automake 1.8 to 1.9.6 used to define mkdir_p.  We now use MKDIR_P,
dnl while keeping a definition of mkdir_p for backward compatibility.
dnl @MKDIR_P@ is magic: AC_OUTPUT adjusts its value for each Makefile.
dnl However we cannot define mkdir_p as $(MKDIR_P) for the sake of
dnl Makefile.ins that do not define MKDIR_P, so we do our own
dnl adjustment using top_builddir (which is defined more often than
dnl MKDIR_P).
AC_SUBST([mkdir_p], ["$MKDIR_P"])dnl
case $mkdir_p in
  [[\\/$]]* | ?:[[\\/]]*) ;;
  */*) mkdir_p="\$(top_builddir)/$mkdir_p" ;;
esac
])

# Helper functions for option handling.                     -*- Autoconf -*-

# Copyright (C) 2001, 2002, 2003, 2005  Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# serial 3

# _AM_MANGLE_OPTION(NAME)
# -----------------------
AC_DEFUN([_AM_MANGLE_OPTION],
[[_AM_OPTION_]m4_bpatsubst($1, [[^a-zA-Z0-9_]], [_])])

# _AM_SET_OPTION(NAME)
# ------------------------------
# Set option NAME.  Presently that only means defining a flag for this option.
AC_DEFUN([_AM_SET_OPTION],
[m4_define(_AM_MANGLE_OPTION([$1]), 1)])

# _AM_SET_OPTIONS(OPTIONS)
# ----------------------------------
# OPTIONS is a space-separated list of Automake options.
AC_DEFUN([_AM_SET_OPTIONS],
[AC_FOREACH([_AM_Option], [$1], [_AM_SET_OPTION(_AM_Option)])])

# _AM_IF_OPTION(OPTION, IF-SET, [IF-NOT-SET])
# -------------------------------------------
# Execute IF-SET if OPTION is set, IF-NOT-SET otherwise.
AC_DEFUN([_AM_IF_OPTION],
[m4_ifset(_AM_MANGLE_OPTION([$1]), [$2], [$3])])

# Check to make sure that the build environment is sane.    -*- Autoconf -*-

# Copyright (C) 1996, 1997, 2000, 2001, 2003, 2005
# Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# serial 4

# AM_SANITY_CHECK
# ---------------
AC_DEFUN([AM_SANITY_CHECK],
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftest.file
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftest.file 2> /dev/null`
   if test "$[*]" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftest.file`
   fi
   rm -f conftest.file
   if test "$[*]" != "X $srcdir/configure conftest.file" \
      && test "$[*]" != "X conftest.file $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "$[2]" = conftest.file
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
AC_MSG_RESULT(yes)])

# Copyright (C) 2001, 2003, 2005  Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_PROG_INSTALL_STRIP
# ---------------------
# One issue with vendor `install' (even GNU) is that you can't
# specify the program used to strip binaries.  This is especially
# annoying in cross-compiling environments, where the build's strip
# is unlikely to handle the host's binaries.
# Fortunately install-sh will honor a STRIPPROG variable, so we
# always use install-sh in `make install-strip', and initialize
# STRIPPROG with the value of the STRIP variable (set by the user).
AC_DEFUN([AM_PROG_INSTALL_STRIP],
[AC_REQUIRE([AM_PROG_INSTALL_SH])dnl
# Installed binaries are usually stripped using `strip' when the user
# run `make install-strip'.  However `strip' might not be the right
# tool to use in cross-compilation environments, therefore Automake
# will honor the `STRIP' environment variable to overrule this program.
dnl Don't test for $cross_compiling = yes, because it might be `maybe'.
if test "$cross_compiling" != no; then
  AC_CHECK_TOOL([STRIP], [strip], :)
fi
INSTALL_STRIP_PROGRAM="\$(install_sh) -c -s"
AC_SUBST([INSTALL_STRIP_PROGRAM])])

# Copyright (C) 2006  Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# _AM_SUBST_NOTMAKE(VARIABLE)
# ---------------------------
# Prevent Automake from outputting VARIABLE = @VARIABLE@ in Makefile.in.
# This macro is traced by Automake.
AC_DEFUN([_AM_SUBST_NOTMAKE])

# Check how to create a tarball.                            -*- Autoconf -*-

# Copyright (C) 2004, 2005  Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# serial 2

# _AM_PROG_TAR(FORMAT)
# --------------------
# Check how to create a tarball in format FORMAT.
# FORMAT should be one of `v7', `ustar', or `pax'.
#
# Substitute a variable $(am__tar) that is a command
# writing to stdout a FORMAT-tarball containing the directory
# $tardir.
#     tardir=directory && $(am__tar) > result.tar
#
# Substitute a variable $(am__untar) that extract such
# a tarball read from stdin.
#     $(am__untar) < result.tar
AC_DEFUN([_AM_PROG_TAR],
[# Always define AMTAR for backward compatibility.
AM_MISSING_PROG([AMTAR], [tar])
m4_if([$1], [v7],
     [am__tar='${AMTAR} chof - "$$tardir"'; am__untar='${AMTAR} xf -'],
     [m4_case([$1], [ustar],, [pax],,
              [m4_fatal([Unknown tar format])])
AC_MSG_CHECKING([how to create a $1 tar archive])
# Loop over all known methods to create a tar archive until one works.
_am_tools='gnutar m4_if([$1], [ustar], [plaintar]) pax cpio none'
_am_tools=${am_cv_prog_tar_$1-$_am_tools}
# Do not fold the above two line into one, because Tru64 sh and
# Solaris sh will not grok spaces in the rhs of `-'.
for _am_tool in $_am_tools
do
  case $_am_tool in
  gnutar)
    for _am_tar in tar gnutar gtar;
    do
      AM_RUN_LOG([$_am_tar --version]) && break
    done
    am__tar="$_am_tar --format=m4_if([$1], [pax], [posix], [$1]) -chf - "'"$$tardir"'
    am__tar_="$_am_tar --format=m4_if([$1], [pax], [posix], [$1]) -chf - "'"$tardir"'
    am__untar="$_am_tar -xf -"
    ;;
  plaintar)
    # Must skip GNU tar: if it does not support --format= it doesn't create
    # ustar tarball either.
    (tar --version) >/dev/null 2>&1 && continue
    am__tar='tar chf - "$$tardir"'
    am__tar_='tar chf - "$tardir"'
    am__untar='tar xf -'
    ;;
  pax)
    am__tar='pax -L -x $1 -w "$$tardir"'
    am__tar_='pax -L -x $1 -w "$tardir"'
    am__untar='pax -r'
    ;;
  cpio)
    am__tar='find "$$tardir" -print | cpio -o -H $1 -L'
    am__tar_='find "$tardir" -print | cpio -o -H $1 -L'
    am__untar='cpio -i -H $1 -d'
    ;;
  none)
    am__tar=false
    am__tar_=false
    am__untar=false
    ;;
  esac

  # If the value was cached, stop now.  We just wanted to have am__tar
  # and am__untar set.
  test -n "${am_cv_prog_tar_$1}" && break

  # tar/untar a dummy directory, and stop if the command works
  rm -rf conftest.dir
  mkdir conftest.dir
  echo GrepMe > conftest.dir/file
  AM_RUN_LOG([tardir=conftest.dir && eval $am__tar_ >conftest.tar])
  rm -rf conftest.dir
  if test -s conftest.tar; then
    AM_RUN_LOG([$am__untar <conftest.tar])
    grep GrepMe conftest.dir/file >/dev/null 2>&1 && break
  fi
done
rm -rf conftest.dir

AC_CACHE_VAL([am_cv_prog_tar_$1], [am_cv_prog_tar_$1=$_am_tool])
AC_MSG_RESULT([$am_cv_prog_tar_$1])])
AC_SUBST([am__tar])
AC_SUBST([am__untar])
]) # _AM_PROG_TAR

# alloca.m4 serial 9
dnl Copyright (C) 2002-2004, 2006, 2007, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_ALLOCA],
[
  dnl Work around a bug of AC_EGREP_CPP in autoconf-2.57.
  AC_REQUIRE([AC_PROG_CPP])
  AC_REQUIRE([AC_PROG_EGREP])

  AC_REQUIRE([AC_FUNC_ALLOCA])
  if test $ac_cv_func_alloca_works = no; then
    gl_PREREQ_ALLOCA
  fi

  # Define an additional variable used in the Makefile substitution.
  if test $ac_cv_working_alloca_h = yes; then
    AC_CACHE_CHECK([for alloca as a compiler built-in], [gl_cv_rpl_alloca], [
      AC_EGREP_CPP([Need own alloca], [
#if defined __GNUC__ || defined _AIX || defined _MSC_VER
        Need own alloca
#endif
        ], [gl_cv_rpl_alloca=yes], [gl_cv_rpl_alloca=no])
    ])
    if test $gl_cv_rpl_alloca = yes; then
      dnl OK, alloca can be implemented through a compiler built-in.
      AC_DEFINE([HAVE_ALLOCA], [1],
        [Define to 1 if you have 'alloca' after including <alloca.h>,
         a header that may be supplied by this distribution.])
      ALLOCA_H=alloca.h
    else
      dnl alloca exists as a library function, i.e. it is slow and probably
      dnl a memory leak. Don't define HAVE_ALLOCA in this case.
      ALLOCA_H=
    fi
  else
    ALLOCA_H=alloca.h
  fi
  AC_SUBST([ALLOCA_H])
])

# Prerequisites of lib/alloca.c.
# STACK_DIRECTION is already handled by AC_FUNC_ALLOCA.
AC_DEFUN([gl_PREREQ_ALLOCA], [:])

# btowc.m4 serial 3
dnl Copyright (C) 2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_BTOWC],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])

  AC_CHECK_FUNCS_ONCE([btowc])
  if test $ac_cv_func_btowc = no; then
    HAVE_BTOWC=0
  else

    dnl IRIX 6.5 btowc(EOF) is 0xFF, not WEOF.
    AC_REQUIRE([AC_PROG_CC])
    AC_REQUIRE([gt_LOCALE_FR])
    AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
    AC_CACHE_CHECK([whether btowc(EOF) is correct],
      [gl_cv_func_btowc_eof],
      [
        dnl Initial guess, used when cross-compiling or when no suitable locale
        dnl is present.
changequote(,)dnl
        case "$host_os" in
                 # Guess no on IRIX.
          irix*) gl_cv_func_btowc_eof="guessing no" ;;
                 # Guess yes otherwise.
          *)     gl_cv_func_btowc_eof="guessing yes" ;;
        esac
changequote([,])dnl
        if test $LOCALE_FR != none; then
          AC_TRY_RUN([
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>
int main ()
{
  if (setlocale (LC_ALL, "$LOCALE_FR") != NULL)
    {
      if (btowc (EOF) != WEOF)
        return 1;
    }
  return 0;
}],
            [gl_cv_func_btowc_eof=yes],
            [gl_cv_func_btowc_eof=no],
            [])
        fi
      ])
    case "$gl_cv_func_btowc_eof" in
      *yes) ;;
      *) REPLACE_BTOWC=1 ;;
    esac
  fi
  if test $HAVE_BTOWC = 0 || test $REPLACE_BTOWC = 1; then
    gl_REPLACE_WCHAR_H
    AC_LIBOBJ([btowc])
    gl_PREREQ_BTOWC
  fi
])

# Prerequisites of lib/btowc.c.
AC_DEFUN([gl_PREREQ_BTOWC], [
  :
])

# codeset.m4 serial 4 (gettext-0.18)
dnl Copyright (C) 2000-2002, 2006, 2008, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

AC_DEFUN([AM_LANGINFO_CODESET],
[
  AC_CACHE_CHECK([for nl_langinfo and CODESET], [am_cv_langinfo_codeset],
    [AC_TRY_LINK([#include <langinfo.h>],
      [char* cs = nl_langinfo(CODESET); return !cs;],
      [am_cv_langinfo_codeset=yes],
      [am_cv_langinfo_codeset=no])
    ])
  if test $am_cv_langinfo_codeset = yes; then
    AC_DEFINE([HAVE_LANGINFO_CODESET], [1],
      [Define if you have <langinfo.h> and nl_langinfo(CODESET).])
  fi
])

# serial 6  -*- Autoconf -*-
# Enable extensions on systems that normally disable them.

# Copyright (C) 2003, 2006-2008 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This definition of AC_USE_SYSTEM_EXTENSIONS is stolen from CVS
# Autoconf.  Perhaps we can remove this once we can assume Autoconf
# 2.62 or later everywhere, but since CVS Autoconf mutates rapidly
# enough in this area it's likely we'll need to redefine
# AC_USE_SYSTEM_EXTENSIONS for quite some time.

# AC_USE_SYSTEM_EXTENSIONS
# ------------------------
# Enable extensions on systems that normally disable them,
# typically due to standards-conformance issues.
# Remember that #undef in AH_VERBATIM gets replaced with #define by
# AC_DEFINE.  The goal here is to define all known feature-enabling
# macros, then, if reports of conflicts are made, disable macros that
# cause problems on some platforms (such as __EXTENSIONS__).
AC_DEFUN([AC_USE_SYSTEM_EXTENSIONS],
[AC_BEFORE([$0], [AC_COMPILE_IFELSE])dnl
AC_BEFORE([$0], [AC_RUN_IFELSE])dnl

  AC_REQUIRE([AC_CANONICAL_HOST])

  AC_CHECK_HEADER([minix/config.h], [MINIX=yes], [MINIX=])
  if test "$MINIX" = yes; then
    AC_DEFINE([_POSIX_SOURCE], [1],
      [Define to 1 if you need to in order for `stat' and other
       things to work.])
    AC_DEFINE([_POSIX_1_SOURCE], [2],
      [Define to 2 if the system does not provide POSIX.1 features
       except with this defined.])
    AC_DEFINE([_MINIX], [1],
      [Define to 1 if on MINIX.])
  fi

  dnl HP-UX 11.11 defines mbstate_t only if _XOPEN_SOURCE is defined to 500,
  dnl regardless of whether the flags -Ae or _D_HPUX_SOURCE=1 are already
  dnl provided.
  case "$host_os" in
    hpux*)
      AC_DEFINE([_XOPEN_SOURCE], [500],
        [Define to 500 only on HP-UX.])
      ;;
  esac

  AH_VERBATIM([__EXTENSIONS__],
[/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# undef _ALL_SOURCE
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# undef _GNU_SOURCE
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# undef _POSIX_PTHREAD_SEMANTICS
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# undef _TANDEM_SOURCE
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# undef __EXTENSIONS__
#endif
])
  AC_CACHE_CHECK([whether it is safe to define __EXTENSIONS__],
    [ac_cv_safe_to_define___extensions__],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM([[
#	  define __EXTENSIONS__ 1
	  ]AC_INCLUDES_DEFAULT])],
       [ac_cv_safe_to_define___extensions__=yes],
       [ac_cv_safe_to_define___extensions__=no])])
  test $ac_cv_safe_to_define___extensions__ = yes &&
    AC_DEFINE([__EXTENSIONS__])
  AC_DEFINE([_ALL_SOURCE])
  AC_DEFINE([_GNU_SOURCE])
  AC_DEFINE([_POSIX_PTHREAD_SEMANTICS])
  AC_DEFINE([_TANDEM_SOURCE])
])# AC_USE_SYSTEM_EXTENSIONS

# gl_USE_SYSTEM_EXTENSIONS
# ------------------------
# Enable extensions on systems that normally disable them,
# typically due to standards-conformance issues.
AC_DEFUN([gl_USE_SYSTEM_EXTENSIONS],
  [AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])])

# Check for fnmatch - serial 2.

# Copyright (C) 2000-2007, 2009 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# Autoconf defines AC_FUNC_FNMATCH, but that is obsolescent.
# New applications should use the macros below instead.

# _AC_FUNC_FNMATCH_IF([STANDARD = GNU | POSIX], [CACHE_VAR], [IF-TRUE], [IF-FALSE])
# -------------------------------------------------------------------------
# If a STANDARD compliant fnmatch is found, run IF-TRUE, otherwise
# IF-FALSE.  Use CACHE_VAR.
AC_DEFUN([_AC_FUNC_FNMATCH_IF],
[AC_CACHE_CHECK(
   [for working $1 fnmatch],
   [$2],
  [dnl Some versions of Solaris, SCO, and the GNU C Library
   dnl have a broken or incompatible fnmatch.
   dnl So we run a test program.  If we are cross-compiling, take no chance.
   dnl Thanks to John Oleynick, François Pinard, and Paul Eggert for this test.
   AC_RUN_IFELSE(
      [AC_LANG_PROGRAM(
	 [[#include <fnmatch.h>
	   static int
	   y (char const *pattern, char const *string, int flags)
	   {
	     return fnmatch (pattern, string, flags) == 0;
	   }
	   static int
	   n (char const *pattern, char const *string, int flags)
	   {
	     return fnmatch (pattern, string, flags) == FNM_NOMATCH;
	   }
	 ]],
	 [[char const *Apat = 'A' < '\\\\' ? "[A-\\\\\\\\]" : "[\\\\\\\\-A]";
	   char const *apat = 'a' < '\\\\' ? "[a-\\\\\\\\]" : "[\\\\\\\\-a]";
	   static char const A_1[] = { 'A' - 1, 0 };
	   static char const A01[] = { 'A' + 1, 0 };
	   static char const a_1[] = { 'a' - 1, 0 };
	   static char const a01[] = { 'a' + 1, 0 };
	   static char const bs_1[] = { '\\\\' - 1, 0 };
	   static char const bs01[] = { '\\\\' + 1, 0 };
	   return
	    !(n ("a*", "", 0)
	      && y ("a*", "abc", 0)
	      && n ("d*/*1", "d/s/1", FNM_PATHNAME)
	      && y ("a\\\\bc", "abc", 0)
	      && n ("a\\\\bc", "abc", FNM_NOESCAPE)
	      && y ("*x", ".x", 0)
	      && n ("*x", ".x", FNM_PERIOD)
	      && y (Apat, "\\\\", 0) && y (Apat, "A", 0)
	      && y (apat, "\\\\", 0) && y (apat, "a", 0)
	      && n (Apat, A_1, 0) == ('A' < '\\\\')
	      && n (apat, a_1, 0) == ('a' < '\\\\')
	      && y (Apat, A01, 0) == ('A' < '\\\\')
	      && y (apat, a01, 0) == ('a' < '\\\\')
	      && y (Apat, bs_1, 0) == ('A' < '\\\\')
	      && y (apat, bs_1, 0) == ('a' < '\\\\')
	      && n (Apat, bs01, 0) == ('A' < '\\\\')
	      && n (apat, bs01, 0) == ('a' < '\\\\')
	      && ]m4_if([$1], [GNU],
		   [y ("xxXX", "xXxX", FNM_CASEFOLD)
		    && y ("a++(x|yy)b", "a+xyyyyxb", FNM_EXTMATCH)
		    && n ("d*/*1", "d/s/1", FNM_FILE_NAME)
		    && y ("*", "x", FNM_FILE_NAME | FNM_LEADING_DIR)
		    && y ("x*", "x/y/z", FNM_FILE_NAME | FNM_LEADING_DIR)
		    && y ("*c*", "c/x", FNM_FILE_NAME | FNM_LEADING_DIR)],
		   1))[;]])],
      [$2=yes],
      [$2=no],
      [$2=cross])])
AS_IF([test $$2 = yes], [$3], [$4])
])# _AC_FUNC_FNMATCH_IF


# _AC_LIBOBJ_FNMATCH
# ------------------
# Prepare the replacement of fnmatch.
AC_DEFUN([_AC_LIBOBJ_FNMATCH],
[AC_REQUIRE([AC_FUNC_ALLOCA])dnl
AC_REQUIRE([AC_TYPE_MBSTATE_T])dnl
AC_CHECK_DECLS([isblank], [], [], [#include <ctype.h>])
AC_CHECK_FUNCS_ONCE([btowc isblank iswctype mbsrtowcs mempcpy wmemchr wmemcpy wmempcpy])
AC_CHECK_HEADERS_ONCE([wctype.h])
AC_LIBOBJ([fnmatch])
FNMATCH_H=fnmatch.h
])# _AC_LIBOBJ_FNMATCH


AC_DEFUN([gl_FUNC_FNMATCH_POSIX],
[
  FNMATCH_H=
  _AC_FUNC_FNMATCH_IF([POSIX], [ac_cv_func_fnmatch_posix],
		      [rm -f lib/fnmatch.h],
		      [_AC_LIBOBJ_FNMATCH])
  if test $ac_cv_func_fnmatch_posix != yes; then
    dnl We must choose a different name for our function, since on ELF systems
    dnl a broken fnmatch() in libc.so would override our fnmatch() if it is
    dnl compiled into a shared library.
    AC_DEFINE([fnmatch], [posix_fnmatch],
      [Define to a replacement function name for fnmatch().])
  fi
  AC_SUBST([FNMATCH_H])
])


AC_DEFUN([gl_FUNC_FNMATCH_GNU],
[
  dnl Persuade glibc <fnmatch.h> to declare FNM_CASEFOLD etc.
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  FNMATCH_H=
  _AC_FUNC_FNMATCH_IF([GNU], [ac_cv_func_fnmatch_gnu],
		      [rm -f lib/fnmatch.h],
		      [_AC_LIBOBJ_FNMATCH])
  if test $ac_cv_func_fnmatch_gnu != yes; then
    dnl We must choose a different name for our function, since on ELF systems
    dnl a broken fnmatch() in libc.so would override our fnmatch() if it is
    dnl compiled into a shared library.
    AC_DEFINE([fnmatch], [gnu_fnmatch],
      [Define to a replacement function name for fnmatch().])
  fi
  AC_SUBST([FNMATCH_H])
])

# gettext.m4 serial 62 (gettext-0.18)
dnl Copyright (C) 1995-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Authors:
dnl   Ulrich Drepper <drepper@cygnus.com>, 1995-2000.
dnl   Bruno Haible <haible@clisp.cons.org>, 2000-2006.

dnl Macro to add for using GNU gettext.

dnl Usage: AM_GNU_GETTEXT([INTLSYMBOL], [NEEDSYMBOL], [INTLDIR]).
dnl INTLSYMBOL can be one of 'external', 'no-libtool', 'use-libtool'. The
dnl    default (if it is not specified or empty) is 'no-libtool'.
dnl    INTLSYMBOL should be 'external' for packages with no intl directory,
dnl    and 'no-libtool' or 'use-libtool' for packages with an intl directory.
dnl    If INTLSYMBOL is 'use-libtool', then a libtool library
dnl    $(top_builddir)/intl/libintl.la will be created (shared and/or static,
dnl    depending on --{enable,disable}-{shared,static} and on the presence of
dnl    AM-DISABLE-SHARED). If INTLSYMBOL is 'no-libtool', a static library
dnl    $(top_builddir)/intl/libintl.a will be created.
dnl If NEEDSYMBOL is specified and is 'need-ngettext', then GNU gettext
dnl    implementations (in libc or libintl) without the ngettext() function
dnl    will be ignored.  If NEEDSYMBOL is specified and is
dnl    'need-formatstring-macros', then GNU gettext implementations that don't
dnl    support the ISO C 99 <inttypes.h> formatstring macros will be ignored.
dnl INTLDIR is used to find the intl libraries.  If empty,
dnl    the value `$(top_builddir)/intl/' is used.
dnl
dnl The result of the configuration is one of three cases:
dnl 1) GNU gettext, as included in the intl subdirectory, will be compiled
dnl    and used.
dnl    Catalog format: GNU --> install in $(datadir)
dnl    Catalog extension: .mo after installation, .gmo in source tree
dnl 2) GNU gettext has been found in the system's C library.
dnl    Catalog format: GNU --> install in $(datadir)
dnl    Catalog extension: .mo after installation, .gmo in source tree
dnl 3) No internationalization, always use English msgid.
dnl    Catalog format: none
dnl    Catalog extension: none
dnl If INTLSYMBOL is 'external', only cases 2 and 3 can occur.
dnl The use of .gmo is historical (it was needed to avoid overwriting the
dnl GNU format catalogs when building on a platform with an X/Open gettext),
dnl but we keep it in order not to force irrelevant filename changes on the
dnl maintainers.
dnl
AC_DEFUN([AM_GNU_GETTEXT],
[
  dnl Argument checking.
  ifelse([$1], [], , [ifelse([$1], [external], , [ifelse([$1], [no-libtool], , [ifelse([$1], [use-libtool], ,
    [errprint([ERROR: invalid first argument to AM_GNU_GETTEXT
])])])])])
  ifelse([$2], [], , [ifelse([$2], [need-ngettext], , [ifelse([$2], [need-formatstring-macros], ,
    [errprint([ERROR: invalid second argument to AM_GNU_GETTEXT
])])])])
  define([gt_included_intl],
    ifelse([$1], [external],
      ifdef([AM_GNU_GETTEXT_][INTL_SUBDIR], [yes], [no]),
      [yes]))
  define([gt_libtool_suffix_prefix], ifelse([$1], [use-libtool], [l], []))
  gt_NEEDS_INIT
  AM_GNU_GETTEXT_NEED([$2])

  AC_REQUIRE([AM_PO_SUBDIRS])dnl
  ifelse(gt_included_intl, yes, [
    AC_REQUIRE([AM_INTL_SUBDIR])dnl
  ])

  dnl Prerequisites of AC_LIB_LINKFLAGS_BODY.
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  AC_REQUIRE([AC_LIB_RPATH])

  dnl Sometimes libintl requires libiconv, so first search for libiconv.
  dnl Ideally we would do this search only after the
  dnl      if test "$USE_NLS" = "yes"; then
  dnl        if { eval "gt_val=\$$gt_func_gnugettext_libc"; test "$gt_val" != "yes"; }; then
  dnl tests. But if configure.in invokes AM_ICONV after AM_GNU_GETTEXT
  dnl the configure script would need to contain the same shell code
  dnl again, outside any 'if'. There are two solutions:
  dnl - Invoke AM_ICONV_LINKFLAGS_BODY here, outside any 'if'.
  dnl - Control the expansions in more detail using AC_PROVIDE_IFELSE.
  dnl Since AC_PROVIDE_IFELSE is only in autoconf >= 2.52 and not
  dnl documented, we avoid it.
  ifelse(gt_included_intl, yes, , [
    AC_REQUIRE([AM_ICONV_LINKFLAGS_BODY])
  ])

  dnl Sometimes, on MacOS X, libintl requires linking with CoreFoundation.
  gt_INTL_MACOSX

  dnl Set USE_NLS.
  AC_REQUIRE([AM_NLS])

  ifelse(gt_included_intl, yes, [
    BUILD_INCLUDED_LIBINTL=no
    USE_INCLUDED_LIBINTL=no
  ])
  LIBINTL=
  LTLIBINTL=
  POSUB=

  dnl Add a version number to the cache macros.
  case " $gt_needs " in
    *" need-formatstring-macros "*) gt_api_version=3 ;;
    *" need-ngettext "*) gt_api_version=2 ;;
    *) gt_api_version=1 ;;
  esac
  gt_func_gnugettext_libc="gt_cv_func_gnugettext${gt_api_version}_libc"
  gt_func_gnugettext_libintl="gt_cv_func_gnugettext${gt_api_version}_libintl"

  dnl If we use NLS figure out what method
  if test "$USE_NLS" = "yes"; then
    gt_use_preinstalled_gnugettext=no
    ifelse(gt_included_intl, yes, [
      AC_MSG_CHECKING([whether included gettext is requested])
      AC_ARG_WITH([included-gettext],
        [  --with-included-gettext use the GNU gettext library included here],
        nls_cv_force_use_gnu_gettext=$withval,
        nls_cv_force_use_gnu_gettext=no)
      AC_MSG_RESULT([$nls_cv_force_use_gnu_gettext])

      nls_cv_use_gnu_gettext="$nls_cv_force_use_gnu_gettext"
      if test "$nls_cv_force_use_gnu_gettext" != "yes"; then
    ])
        dnl User does not insist on using GNU NLS library.  Figure out what
        dnl to use.  If GNU gettext is available we use this.  Else we have
        dnl to fall back to GNU NLS library.

        if test $gt_api_version -ge 3; then
          gt_revision_test_code='
#ifndef __GNU_GETTEXT_SUPPORTED_REVISION
#define __GNU_GETTEXT_SUPPORTED_REVISION(major) ((major) == 0 ? 0 : -1)
#endif
changequote(,)dnl
typedef int array [2 * (__GNU_GETTEXT_SUPPORTED_REVISION(0) >= 1) - 1];
changequote([,])dnl
'
        else
          gt_revision_test_code=
        fi
        if test $gt_api_version -ge 2; then
          gt_expression_test_code=' + * ngettext ("", "", 0)'
        else
          gt_expression_test_code=
        fi

        AC_CACHE_CHECK([for GNU gettext in libc], [$gt_func_gnugettext_libc],
         [AC_TRY_LINK([#include <libintl.h>
$gt_revision_test_code
extern int _nl_msg_cat_cntr;
extern int *_nl_domain_bindings;],
            [bindtextdomain ("", "");
return * gettext ("")$gt_expression_test_code + _nl_msg_cat_cntr + *_nl_domain_bindings],
            [eval "$gt_func_gnugettext_libc=yes"],
            [eval "$gt_func_gnugettext_libc=no"])])

        if { eval "gt_val=\$$gt_func_gnugettext_libc"; test "$gt_val" != "yes"; }; then
          dnl Sometimes libintl requires libiconv, so first search for libiconv.
          ifelse(gt_included_intl, yes, , [
            AM_ICONV_LINK
          ])
          dnl Search for libintl and define LIBINTL, LTLIBINTL and INCINTL
          dnl accordingly. Don't use AC_LIB_LINKFLAGS_BODY([intl],[iconv])
          dnl because that would add "-liconv" to LIBINTL and LTLIBINTL
          dnl even if libiconv doesn't exist.
          AC_LIB_LINKFLAGS_BODY([intl])
          AC_CACHE_CHECK([for GNU gettext in libintl],
            [$gt_func_gnugettext_libintl],
           [gt_save_CPPFLAGS="$CPPFLAGS"
            CPPFLAGS="$CPPFLAGS $INCINTL"
            gt_save_LIBS="$LIBS"
            LIBS="$LIBS $LIBINTL"
            dnl Now see whether libintl exists and does not depend on libiconv.
            AC_TRY_LINK([#include <libintl.h>
$gt_revision_test_code
extern int _nl_msg_cat_cntr;
extern
#ifdef __cplusplus
"C"
#endif
const char *_nl_expand_alias (const char *);],
              [bindtextdomain ("", "");
return * gettext ("")$gt_expression_test_code + _nl_msg_cat_cntr + *_nl_expand_alias ("")],
              [eval "$gt_func_gnugettext_libintl=yes"],
              [eval "$gt_func_gnugettext_libintl=no"])
            dnl Now see whether libintl exists and depends on libiconv.
            if { eval "gt_val=\$$gt_func_gnugettext_libintl"; test "$gt_val" != yes; } && test -n "$LIBICONV"; then
              LIBS="$LIBS $LIBICONV"
              AC_TRY_LINK([#include <libintl.h>
$gt_revision_test_code
extern int _nl_msg_cat_cntr;
extern
#ifdef __cplusplus
"C"
#endif
const char *_nl_expand_alias (const char *);],
                [bindtextdomain ("", "");
return * gettext ("")$gt_expression_test_code + _nl_msg_cat_cntr + *_nl_expand_alias ("")],
               [LIBINTL="$LIBINTL $LIBICONV"
                LTLIBINTL="$LTLIBINTL $LTLIBICONV"
                eval "$gt_func_gnugettext_libintl=yes"
               ])
            fi
            CPPFLAGS="$gt_save_CPPFLAGS"
            LIBS="$gt_save_LIBS"])
        fi

        dnl If an already present or preinstalled GNU gettext() is found,
        dnl use it.  But if this macro is used in GNU gettext, and GNU
        dnl gettext is already preinstalled in libintl, we update this
        dnl libintl.  (Cf. the install rule in intl/Makefile.in.)
        if { eval "gt_val=\$$gt_func_gnugettext_libc"; test "$gt_val" = "yes"; } \
           || { { eval "gt_val=\$$gt_func_gnugettext_libintl"; test "$gt_val" = "yes"; } \
                && test "$PACKAGE" != gettext-runtime \
                && test "$PACKAGE" != gettext-tools; }; then
          gt_use_preinstalled_gnugettext=yes
        else
          dnl Reset the values set by searching for libintl.
          LIBINTL=
          LTLIBINTL=
          INCINTL=
        fi

    ifelse(gt_included_intl, yes, [
        if test "$gt_use_preinstalled_gnugettext" != "yes"; then
          dnl GNU gettext is not found in the C library.
          dnl Fall back on included GNU gettext library.
          nls_cv_use_gnu_gettext=yes
        fi
      fi

      if test "$nls_cv_use_gnu_gettext" = "yes"; then
        dnl Mark actions used to generate GNU NLS library.
        BUILD_INCLUDED_LIBINTL=yes
        USE_INCLUDED_LIBINTL=yes
        LIBINTL="ifelse([$3],[],\${top_builddir}/intl,[$3])/libintl.[]gt_libtool_suffix_prefix[]a $LIBICONV $LIBTHREAD"
        LTLIBINTL="ifelse([$3],[],\${top_builddir}/intl,[$3])/libintl.[]gt_libtool_suffix_prefix[]a $LTLIBICONV $LTLIBTHREAD"
        LIBS=`echo " $LIBS " | sed -e 's/ -lintl / /' -e 's/^ //' -e 's/ $//'`
      fi

      CATOBJEXT=
      if test "$gt_use_preinstalled_gnugettext" = "yes" \
         || test "$nls_cv_use_gnu_gettext" = "yes"; then
        dnl Mark actions to use GNU gettext tools.
        CATOBJEXT=.gmo
      fi
    ])

    if test -n "$INTL_MACOSX_LIBS"; then
      if test "$gt_use_preinstalled_gnugettext" = "yes" \
         || test "$nls_cv_use_gnu_gettext" = "yes"; then
        dnl Some extra flags are needed during linking.
        LIBINTL="$LIBINTL $INTL_MACOSX_LIBS"
        LTLIBINTL="$LTLIBINTL $INTL_MACOSX_LIBS"
      fi
    fi

    if test "$gt_use_preinstalled_gnugettext" = "yes" \
       || test "$nls_cv_use_gnu_gettext" = "yes"; then
      AC_DEFINE([ENABLE_NLS], [1],
        [Define to 1 if translation of program messages to the user's native language
   is requested.])
    else
      USE_NLS=no
    fi
  fi

  AC_MSG_CHECKING([whether to use NLS])
  AC_MSG_RESULT([$USE_NLS])
  if test "$USE_NLS" = "yes"; then
    AC_MSG_CHECKING([where the gettext function comes from])
    if test "$gt_use_preinstalled_gnugettext" = "yes"; then
      if { eval "gt_val=\$$gt_func_gnugettext_libintl"; test "$gt_val" = "yes"; }; then
        gt_source="external libintl"
      else
        gt_source="libc"
      fi
    else
      gt_source="included intl directory"
    fi
    AC_MSG_RESULT([$gt_source])
  fi

  if test "$USE_NLS" = "yes"; then

    if test "$gt_use_preinstalled_gnugettext" = "yes"; then
      if { eval "gt_val=\$$gt_func_gnugettext_libintl"; test "$gt_val" = "yes"; }; then
        AC_MSG_CHECKING([how to link with libintl])
        AC_MSG_RESULT([$LIBINTL])
        AC_LIB_APPENDTOVAR([CPPFLAGS], [$INCINTL])
      fi

      dnl For backward compatibility. Some packages may be using this.
      AC_DEFINE([HAVE_GETTEXT], [1],
       [Define if the GNU gettext() function is already present or preinstalled.])
      AC_DEFINE([HAVE_DCGETTEXT], [1],
       [Define if the GNU dcgettext() function is already present or preinstalled.])
    fi

    dnl We need to process the po/ directory.
    POSUB=po
  fi

  ifelse(gt_included_intl, yes, [
    dnl If this is used in GNU gettext we have to set BUILD_INCLUDED_LIBINTL
    dnl to 'yes' because some of the testsuite requires it.
    if test "$PACKAGE" = gettext-runtime || test "$PACKAGE" = gettext-tools; then
      BUILD_INCLUDED_LIBINTL=yes
    fi

    dnl Make all variables we use known to autoconf.
    AC_SUBST([BUILD_INCLUDED_LIBINTL])
    AC_SUBST([USE_INCLUDED_LIBINTL])
    AC_SUBST([CATOBJEXT])

    dnl For backward compatibility. Some configure.ins may be using this.
    nls_cv_header_intl=
    nls_cv_header_libgt=

    dnl For backward compatibility. Some Makefiles may be using this.
    DATADIRNAME=share
    AC_SUBST([DATADIRNAME])

    dnl For backward compatibility. Some Makefiles may be using this.
    INSTOBJEXT=.mo
    AC_SUBST([INSTOBJEXT])

    dnl For backward compatibility. Some Makefiles may be using this.
    GENCAT=gencat
    AC_SUBST([GENCAT])

    dnl For backward compatibility. Some Makefiles may be using this.
    INTLOBJS=
    if test "$USE_INCLUDED_LIBINTL" = yes; then
      INTLOBJS="\$(GETTOBJS)"
    fi
    AC_SUBST([INTLOBJS])

    dnl Enable libtool support if the surrounding package wishes it.
    INTL_LIBTOOL_SUFFIX_PREFIX=gt_libtool_suffix_prefix
    AC_SUBST([INTL_LIBTOOL_SUFFIX_PREFIX])
  ])

  dnl For backward compatibility. Some Makefiles may be using this.
  INTLLIBS="$LIBINTL"
  AC_SUBST([INTLLIBS])

  dnl Make all documented variables known to autoconf.
  AC_SUBST([LIBINTL])
  AC_SUBST([LTLIBINTL])
  AC_SUBST([POSUB])
])


dnl gt_NEEDS_INIT ensures that the gt_needs variable is initialized.
m4_define([gt_NEEDS_INIT],
[
  m4_divert_text([DEFAULTS], [gt_needs=])
  m4_define([gt_NEEDS_INIT], [])
])


dnl Usage: AM_GNU_GETTEXT_NEED([NEEDSYMBOL])
AC_DEFUN([AM_GNU_GETTEXT_NEED],
[
  m4_divert_text([INIT_PREPARE], [gt_needs="$gt_needs $1"])
])


dnl Usage: AM_GNU_GETTEXT_VERSION([gettext-version])
AC_DEFUN([AM_GNU_GETTEXT_VERSION], [])

# serial 12

# Copyright (C) 2001-2003, 2005, 2007, 2009 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

dnl From Jim Meyering.

AC_DEFUN([gl_FUNC_GETTIMEOFDAY],
[
  AC_REQUIRE([AC_C_RESTRICT])
  AC_REQUIRE([gl_HEADER_SYS_TIME_H])
  AC_CHECK_FUNCS_ONCE([gettimeofday])

  AC_CACHE_CHECK([for gettimeofday with POSIX signature],
    [gl_cv_func_gettimeofday_posix_signature],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
	  [[#include <sys/time.h>
	    struct timeval c;
	  ]],
	  [[
	    int (*f) (struct timeval *restrict, void *restrict) = gettimeofday;
	    int x = f (&c, 0);
	    return !(x | c.tv_sec | c.tv_usec);
	  ]])],
	[gl_cv_func_gettimeofday_posix_signature=yes],
	[gl_cv_func_gettimeofday_posix_signature=no])])

  gl_FUNC_GETTIMEOFDAY_CLOBBER

  if test $gl_cv_func_gettimeofday_posix_signature != yes; then
    REPLACE_GETTIMEOFDAY=1
    SYS_TIME_H=sys/time.h
    if test $gl_cv_func_gettimeofday_clobber != yes; then
      AC_LIBOBJ([gettimeofday])
      gl_PREREQ_GETTIMEOFDAY
    fi
  fi
])


dnl See if gettimeofday clobbers the static buffer that localtime uses
dnl for its return value.  The gettimeofday function from Mac OS X 10.0.4
dnl (i.e., Darwin 1.3.7) has this problem.
dnl
dnl If it does, then arrange to use gettimeofday and localtime only via
dnl the wrapper functions that work around the problem.

AC_DEFUN([gl_FUNC_GETTIMEOFDAY_CLOBBER],
[
 AC_REQUIRE([gl_HEADER_SYS_TIME_H])

 AC_CACHE_CHECK([whether gettimeofday clobbers localtime buffer],
  [gl_cv_func_gettimeofday_clobber],
  [AC_RUN_IFELSE(
     [AC_LANG_PROGRAM(
	[[#include <string.h>
	  #include <sys/time.h>
	  #include <time.h>
	  #include <stdlib.h>
	]],
	[[
	  time_t t = 0;
	  struct tm *lt;
	  struct tm saved_lt;
	  struct timeval tv;
	  lt = localtime (&t);
	  saved_lt = *lt;
	  gettimeofday (&tv, NULL);
	  return memcmp (lt, &saved_lt, sizeof (struct tm)) != 0;
	]])],
     [gl_cv_func_gettimeofday_clobber=no],
     [gl_cv_func_gettimeofday_clobber=yes],
     dnl When crosscompiling, assume it is broken.
     [gl_cv_func_gettimeofday_clobber=yes])])

 if test $gl_cv_func_gettimeofday_clobber = yes; then
   REPLACE_GETTIMEOFDAY=1
   SYS_TIME_H=sys/time.h
   gl_GETTIMEOFDAY_REPLACE_LOCALTIME
   AC_DEFINE([GETTIMEOFDAY_CLOBBERS_LOCALTIME], [1],
     [Define if gettimeofday clobbers the localtime buffer.])
 fi
])

AC_DEFUN([gl_GETTIMEOFDAY_REPLACE_LOCALTIME], [
  AC_LIBOBJ([gettimeofday])
  gl_PREREQ_GETTIMEOFDAY
  AC_DEFINE([gmtime], [rpl_gmtime],
    [Define to rpl_gmtime if the replacement function should be used.])
  AC_DEFINE([localtime], [rpl_localtime],
    [Define to rpl_localtime if the replacement function should be used.])
])

# Prerequisites of lib/gettimeofday.c.
AC_DEFUN([gl_PREREQ_GETTIMEOFDAY], [
  AC_CHECK_HEADERS([sys/timeb.h])
  AC_CHECK_FUNCS([_ftime])
])

# glibc21.m4 serial 4
dnl Copyright (C) 2000-2002, 2004, 2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Test for the GNU C Library, version 2.1 or newer.
# From Bruno Haible.

AC_DEFUN([gl_GLIBC21],
  [
    AC_CACHE_CHECK([whether we are using the GNU C Library 2.1 or newer],
      [ac_cv_gnu_library_2_1],
      [AC_EGREP_CPP([Lucky GNU user],
	[
#include <features.h>
#ifdef __GNU_LIBRARY__
 #if (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 1) || (__GLIBC__ > 2)
  Lucky GNU user
 #endif
#endif
	],
	[ac_cv_gnu_library_2_1=yes],
	[ac_cv_gnu_library_2_1=no])
      ]
    )
    AC_SUBST([GLIBC21])
    GLIBC21="$ac_cv_gnu_library_2_1"
  ]
)

# Determine whether recent-enough GNU Make is being used.

# Copyright (C) 2007 Free Software Foundation, Inc.

# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# Written by Paul Eggert.

# Set GNU_MAKE if we are using a recent-enough version of GNU make.

# Use --version AND trailing junk, because SGI Make doesn't fail on --version.

AC_DEFUN([gl_GNU_MAKE],
[
  AM_CONDITIONAL([GNU_MAKE],
    [${MAKE-make} --version /cannot/make/this >/dev/null 2>&1])
])

# gnulib-common.m4 serial 7
dnl Copyright (C) 2007-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# gl_COMMON
# is expanded unconditionally through gnulib-tool magic.
AC_DEFUN([gl_COMMON], [
  dnl Use AC_REQUIRE here, so that the code is expanded once only.
  AC_REQUIRE([gl_COMMON_BODY])
])
AC_DEFUN([gl_COMMON_BODY], [
  AH_VERBATIM([isoc99_inline],
[/* Work around a bug in Apple GCC 4.0.1 build 5465: In C99 mode, it supports
   the ISO C 99 semantics of 'extern inline' (unlike the GNU C semantics of
   earlier versions), but does not display it by setting __GNUC_STDC_INLINE__.
   __APPLE__ && __MACH__ test for MacOS X.
   __APPLE_CC__ tests for the Apple compiler and its version.
   __STDC_VERSION__ tests for the C99 mode.  */
#if defined __APPLE__ && defined __MACH__ && __APPLE_CC__ >= 5465 && !defined __cplusplus && __STDC_VERSION__ >= 199901L && !defined __GNUC_STDC_INLINE__
# define __GNUC_STDC_INLINE__ 1
#endif])
  AH_VERBATIM([unused_parameter],
[/* Define as a marker that can be attached to function parameter declarations
   for parameters that are not used.  This helps to reduce warnings, such as
   from GCC -Wunused-parameter.  */
#if __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7)
# define _UNUSED_PARAMETER_ __attribute__ ((__unused__))
#else
# define _UNUSED_PARAMETER_
#endif
])
])

# gl_MODULE_INDICATOR([modulename])
# defines a C macro indicating the presence of the given module.
AC_DEFUN([gl_MODULE_INDICATOR],
[
  AC_DEFINE([GNULIB_]translit([$1],[abcdefghijklmnopqrstuvwxyz./-],[ABCDEFGHIJKLMNOPQRSTUVWXYZ___]), [1],
    [Define to 1 when using the gnulib module ]$1[.])
])

# m4_foreach_w
# is a backport of autoconf-2.59c's m4_foreach_w.
# Remove this macro when we can assume autoconf >= 2.60.
m4_ifndef([m4_foreach_w],
  [m4_define([m4_foreach_w],
    [m4_foreach([$1], m4_split(m4_normalize([$2]), [ ]), [$3])])])

# AC_PROG_MKDIR_P
# is a backport of autoconf-2.60's AC_PROG_MKDIR_P.
# Remove this macro when we can assume autoconf >= 2.60.
m4_ifdef([AC_PROG_MKDIR_P], [], [
  AC_DEFUN([AC_PROG_MKDIR_P],
    [AC_REQUIRE([AM_PROG_MKDIR_P])dnl defined by automake
     MKDIR_P='$(mkdir_p)'
     AC_SUBST([MKDIR_P])])])

# AC_C_RESTRICT
# This definition overrides the AC_C_RESTRICT macro from autoconf 2.60..2.61,
# so that mixed use of GNU C and GNU C++ and mixed use of Sun C and Sun C++
# works.
# This definition can be removed once autoconf >= 2.62 can be assumed.
AC_DEFUN([AC_C_RESTRICT],
[AC_CACHE_CHECK([for C/C++ restrict keyword], [ac_cv_c_restrict],
  [ac_cv_c_restrict=no
   # The order here caters to the fact that C++ does not require restrict.
   for ac_kw in __restrict __restrict__ _Restrict restrict; do
     AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
      [[typedef int * int_ptr;
	int foo (int_ptr $ac_kw ip) {
	return ip[0];
       }]],
      [[int s[1];
	int * $ac_kw t = s;
	t[0] = 0;
	return foo(t)]])],
      [ac_cv_c_restrict=$ac_kw])
     test "$ac_cv_c_restrict" != no && break
   done
  ])
 AH_VERBATIM([restrict],
[/* Define to the equivalent of the C99 'restrict' keyword, or to
   nothing if this is not supported.  Do not define if restrict is
   supported directly.  */
#undef restrict
/* Work around a bug in Sun C++: it does not support _Restrict, even
   though the corresponding Sun C compiler does, which causes
   "#define restrict _Restrict" in the previous line.  Perhaps some future
   version of Sun C++ will work with _Restrict; if so, it'll probably
   define __RESTRICT, just as Sun C does.  */
#if defined __SUNPRO_CC && !defined __RESTRICT
# define _Restrict
#endif])
 case $ac_cv_c_restrict in
   restrict) ;;
   no) AC_DEFINE([restrict], []) ;;
   *)  AC_DEFINE_UNQUOTED([restrict], [$ac_cv_c_restrict]) ;;
 esac
])

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
AC_DEFUN([gl_EARLY],
[
  m4_pattern_forbid([^gl_[A-Z]])dnl the gnulib macro namespace
  m4_pattern_allow([^gl_ES$])dnl a valid locale name
  m4_pattern_allow([^gl_LIBOBJS$])dnl a variable
  m4_pattern_allow([^gl_LTLIBOBJS$])dnl a variable
  AC_REQUIRE([AC_PROG_RANLIB])
  AC_REQUIRE([AM_PROG_CC_C_O])
  AC_REQUIRE([AC_GNU_SOURCE])
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
])

# This macro should be invoked from ./configure.ac, in the section
# "Check for header files, types and library functions".
AC_DEFUN([gl_INIT],
[
  AM_CONDITIONAL([GL_COND_LIBTOOL], [true])
  gl_cond_libtool=true
  m4_pushdef([AC_LIBOBJ], m4_defn([gl_LIBOBJ]))
  m4_pushdef([AC_REPLACE_FUNCS], m4_defn([gl_REPLACE_FUNCS]))
  m4_pushdef([AC_LIBSOURCES], m4_defn([gl_LIBSOURCES]))
  m4_pushdef([gl_LIBSOURCES_LIST], [])
  m4_pushdef([gl_LIBSOURCES_DIR], [])
  gl_COMMON
  gl_source_base='src/gllib'
changequote(,)dnl
LTALLOCA=`echo "$ALLOCA" | sed 's/\.[^.]* /.lo /g;s/\.[^.]*$/.lo/'`
changequote([, ])dnl
AC_SUBST([LTALLOCA])
  gl_FUNC_ALLOCA
  gl_FUNC_BTOWC
  gl_WCHAR_MODULE_INDICATOR([btowc])
  # No macro. You should also use one of fnmatch-posix or fnmatch-gnu.
  gl_FUNC_FNMATCH_GNU
  dnl you must add AM_GNU_GETTEXT([external]) or similar to configure.ac.
  AM_GNU_GETTEXT_VERSION([0.17])
  AC_SUBST([LIBINTL])
  AC_SUBST([LTLIBINTL])
  gl_FUNC_GETTIMEOFDAY
  gl_GNU_MAKE
  gl_LIBSIGSEGV
  gl_AC_FUNC_LINK_FOLLOWS_SYMLINK
  gl_LOCALCHARSET
  LOCALCHARSET_TESTS_ENVIRONMENT="CHARSETALIASDIR=\"\$(top_builddir)/$gl_source_base\""
  AC_SUBST([LOCALCHARSET_TESTS_ENVIRONMENT])
  AC_FUNC_MALLOC
  AC_DEFINE([GNULIB_MALLOC_GNU], 1, [Define to indicate the 'malloc' module.])
  gl_FUNC_MALLOC_POSIX
  gl_STDLIB_MODULE_INDICATOR([malloc-posix])
  gl_FUNC_MBRTOWC
  gl_WCHAR_MODULE_INDICATOR([mbrtowc])
  gl_FUNC_MBSINIT
  gl_WCHAR_MODULE_INDICATOR([mbsinit])
  gl_MULTIARCH
  gl_REGEX
  gt_TYPE_SSIZE_T
  AM_STDBOOL_H
  gl_STDINT_H
  gl_STDLIB_H
  gl_HEADER_SYS_TIME_H
  AC_PROG_MKDIR_P
  gl_UNISTD_H
  gl_WCHAR_H
  gl_FUNC_WCRTOMB
  gl_WCHAR_MODULE_INDICATOR([wcrtomb])
  gl_WCTYPE_H
  m4_ifval(gl_LIBSOURCES_LIST, [
    m4_syscmd([test ! -d ]m4_defn([gl_LIBSOURCES_DIR])[ ||
      for gl_file in ]gl_LIBSOURCES_LIST[ ; do
        if test ! -r ]m4_defn([gl_LIBSOURCES_DIR])[/$gl_file ; then
          echo "missing file ]m4_defn([gl_LIBSOURCES_DIR])[/$gl_file" >&2
          exit 1
        fi
      done])dnl
      m4_if(m4_sysval, [0], [],
        [AC_FATAL([expected source file, required through AC_LIBSOURCES, not found])])
  ])
  m4_popdef([gl_LIBSOURCES_DIR])
  m4_popdef([gl_LIBSOURCES_LIST])
  m4_popdef([AC_LIBSOURCES])
  m4_popdef([AC_REPLACE_FUNCS])
  m4_popdef([AC_LIBOBJ])
  AC_CONFIG_COMMANDS_PRE([
    gl_libobjs=
    gl_ltlibobjs=
    if test -n "$gl_LIBOBJS"; then
      # Remove the extension.
      sed_drop_objext='s/\.o$//;s/\.obj$//'
      for i in `for i in $gl_LIBOBJS; do echo "$i"; done | sed "$sed_drop_objext" | sort | uniq`; do
        gl_libobjs="$gl_libobjs $i.$ac_objext"
        gl_ltlibobjs="$gl_ltlibobjs $i.lo"
      done
    fi
    AC_SUBST([gl_LIBOBJS], [$gl_libobjs])
    AC_SUBST([gl_LTLIBOBJS], [$gl_ltlibobjs])
  ])
  gltests_libdeps=
  gltests_ltlibdeps=
  m4_pushdef([AC_LIBOBJ], m4_defn([gltests_LIBOBJ]))
  m4_pushdef([AC_REPLACE_FUNCS], m4_defn([gltests_REPLACE_FUNCS]))
  m4_pushdef([AC_LIBSOURCES], m4_defn([gltests_LIBSOURCES]))
  m4_pushdef([gltests_LIBSOURCES_LIST], [])
  m4_pushdef([gltests_LIBSOURCES_DIR], [])
  gl_COMMON
  gl_source_base='tests'
  m4_ifval(gltests_LIBSOURCES_LIST, [
    m4_syscmd([test ! -d ]m4_defn([gltests_LIBSOURCES_DIR])[ ||
      for gl_file in ]gltests_LIBSOURCES_LIST[ ; do
        if test ! -r ]m4_defn([gltests_LIBSOURCES_DIR])[/$gl_file ; then
          echo "missing file ]m4_defn([gltests_LIBSOURCES_DIR])[/$gl_file" >&2
          exit 1
        fi
      done])dnl
      m4_if(m4_sysval, [0], [],
        [AC_FATAL([expected source file, required through AC_LIBSOURCES, not found])])
  ])
  m4_popdef([gltests_LIBSOURCES_DIR])
  m4_popdef([gltests_LIBSOURCES_LIST])
  m4_popdef([AC_LIBSOURCES])
  m4_popdef([AC_REPLACE_FUNCS])
  m4_popdef([AC_LIBOBJ])
  AC_CONFIG_COMMANDS_PRE([
    gltests_libobjs=
    gltests_ltlibobjs=
    if test -n "$gltests_LIBOBJS"; then
      # Remove the extension.
      sed_drop_objext='s/\.o$//;s/\.obj$//'
      for i in `for i in $gltests_LIBOBJS; do echo "$i"; done | sed "$sed_drop_objext" | sort | uniq`; do
        gltests_libobjs="$gltests_libobjs $i.$ac_objext"
        gltests_ltlibobjs="$gltests_ltlibobjs $i.lo"
      done
    fi
    AC_SUBST([gltests_LIBOBJS], [$gltests_libobjs])
    AC_SUBST([gltests_LTLIBOBJS], [$gltests_ltlibobjs])
  ])
])

# Like AC_LIBOBJ, except that the module name goes
# into gl_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([gl_LIBOBJ], [
  AS_LITERAL_IF([$1], [gl_LIBSOURCES([$1.c])])dnl
  gl_LIBOBJS="$gl_LIBOBJS $1.$ac_objext"
])

# Like AC_REPLACE_FUNCS, except that the module name goes
# into gl_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([gl_REPLACE_FUNCS], [
  m4_foreach_w([gl_NAME], [$1], [AC_LIBSOURCES(gl_NAME[.c])])dnl
  AC_CHECK_FUNCS([$1], , [gl_LIBOBJ($ac_func)])
])

# Like AC_LIBSOURCES, except the directory where the source file is
# expected is derived from the gnulib-tool parameterization,
# and alloca is special cased (for the alloca-opt module).
# We could also entirely rely on EXTRA_lib..._SOURCES.
AC_DEFUN([gl_LIBSOURCES], [
  m4_foreach([_gl_NAME], [$1], [
    m4_if(_gl_NAME, [alloca.c], [], [
      m4_define([gl_LIBSOURCES_DIR], [src/gllib])
      m4_append([gl_LIBSOURCES_LIST], _gl_NAME, [ ])
    ])
  ])
])

# Like AC_LIBOBJ, except that the module name goes
# into gltests_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([gltests_LIBOBJ], [
  AS_LITERAL_IF([$1], [gltests_LIBSOURCES([$1.c])])dnl
  gltests_LIBOBJS="$gltests_LIBOBJS $1.$ac_objext"
])

# Like AC_REPLACE_FUNCS, except that the module name goes
# into gltests_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([gltests_REPLACE_FUNCS], [
  m4_foreach_w([gl_NAME], [$1], [AC_LIBSOURCES(gl_NAME[.c])])dnl
  AC_CHECK_FUNCS([$1], , [gltests_LIBOBJ($ac_func)])
])

# Like AC_LIBSOURCES, except the directory where the source file is
# expected is derived from the gnulib-tool parameterization,
# and alloca is special cased (for the alloca-opt module).
# We could also entirely rely on EXTRA_lib..._SOURCES.
AC_DEFUN([gltests_LIBSOURCES], [
  m4_foreach([_gl_NAME], [$1], [
    m4_if(_gl_NAME, [alloca.c], [], [
      m4_define([gltests_LIBSOURCES_DIR], [tests])
      m4_append([gltests_LIBSOURCES_LIST], _gl_NAME, [ ])
    ])
  ])
])

# This macro records the list of files which have been installed by
# gnulib-tool and may be removed by future gnulib-tool invocations.
AC_DEFUN([gl_FILE_LIST], [
  build-aux/config.rpath
  build-aux/link-warning.h
  lib/alloca.c
  lib/alloca.in.h
  lib/btowc.c
  lib/config.charset
  lib/fnmatch.c
  lib/fnmatch.in.h
  lib/fnmatch_loop.c
  lib/gettext.h
  lib/gettimeofday.c
  lib/localcharset.c
  lib/localcharset.h
  lib/malloc.c
  lib/mbrtowc.c
  lib/mbsinit.c
  lib/ref-add.sin
  lib/ref-del.sin
  lib/regcomp.c
  lib/regex.c
  lib/regex.h
  lib/regex_internal.c
  lib/regex_internal.h
  lib/regexec.c
  lib/stdbool.in.h
  lib/stdint.in.h
  lib/stdlib.in.h
  lib/streq.h
  lib/sys_time.in.h
  lib/uniname.h
  lib/uniname/gen-uninames.lisp
  lib/uniname/uniname.c
  lib/uniname/uninames.h
  lib/unistd.in.h
  lib/unitypes.h
  lib/uniwidth.h
  lib/uniwidth/cjk.h
  lib/uniwidth/width.c
  lib/verify.h
  lib/wchar.in.h
  lib/wcrtomb.c
  lib/wctype.in.h
  m4/alloca.m4
  m4/btowc.m4
  m4/codeset.m4
  m4/extensions.m4
  m4/fnmatch.m4
  m4/gettext.m4
  m4/gettimeofday.m4
  m4/glibc2.m4
  m4/glibc21.m4
  m4/gnu-make.m4
  m4/gnulib-common.m4
  m4/iconv.m4
  m4/include_next.m4
  m4/intdiv0.m4
  m4/intl.m4
  m4/intldir.m4
  m4/intlmacosx.m4
  m4/intmax.m4
  m4/inttypes-pri.m4
  m4/inttypes_h.m4
  m4/lcmessage.m4
  m4/lib-ld.m4
  m4/lib-link.m4
  m4/lib-prefix.m4
  m4/libsigsegv.m4
  m4/link-follow.m4
  m4/localcharset.m4
  m4/locale-fr.m4
  m4/locale-ja.m4
  m4/locale-zh.m4
  m4/lock.m4
  m4/longlong.m4
  m4/malloc.m4
  m4/mbrtowc.m4
  m4/mbsinit.m4
  m4/mbstate_t.m4
  m4/multiarch.m4
  m4/nls.m4
  m4/nocrash.m4
  m4/po.m4
  m4/printf-posix.m4
  m4/progtest.m4
  m4/regex.m4
  m4/size_max.m4
  m4/ssize_t.m4
  m4/stdbool.m4
  m4/stdint.m4
  m4/stdint_h.m4
  m4/stdlib_h.m4
  m4/sys_time_h.m4
  m4/threadlib.m4
  m4/uintmax_t.m4
  m4/unistd_h.m4
  m4/visibility.m4
  m4/wchar.m4
  m4/wchar_t.m4
  m4/wcrtomb.m4
  m4/wctype.m4
  m4/wint_t.m4
  m4/xsize.m4
])

# iconv.m4 serial AM7 (gettext-0.18)
dnl Copyright (C) 2000-2002, 2007-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

AC_DEFUN([AM_ICONV_LINKFLAGS_BODY],
[
  dnl Prerequisites of AC_LIB_LINKFLAGS_BODY.
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  AC_REQUIRE([AC_LIB_RPATH])

  dnl Search for libiconv and define LIBICONV, LTLIBICONV and INCICONV
  dnl accordingly.
  AC_LIB_LINKFLAGS_BODY([iconv])
])

AC_DEFUN([AM_ICONV_LINK],
[
  dnl Some systems have iconv in libc, some have it in libiconv (OSF/1 and
  dnl those with the standalone portable GNU libiconv installed).
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles

  dnl Search for libiconv and define LIBICONV, LTLIBICONV and INCICONV
  dnl accordingly.
  AC_REQUIRE([AM_ICONV_LINKFLAGS_BODY])

  dnl Add $INCICONV to CPPFLAGS before performing the following checks,
  dnl because if the user has installed libiconv and not disabled its use
  dnl via --without-libiconv-prefix, he wants to use it. The first
  dnl AC_TRY_LINK will then fail, the second AC_TRY_LINK will succeed.
  am_save_CPPFLAGS="$CPPFLAGS"
  AC_LIB_APPENDTOVAR([CPPFLAGS], [$INCICONV])

  AC_CACHE_CHECK([for iconv], [am_cv_func_iconv], [
    am_cv_func_iconv="no, consider installing GNU libiconv"
    am_cv_lib_iconv=no
    AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
      [iconv_t cd = iconv_open("","");
       iconv(cd,NULL,NULL,NULL,NULL);
       iconv_close(cd);],
      [am_cv_func_iconv=yes])
    if test "$am_cv_func_iconv" != yes; then
      am_save_LIBS="$LIBS"
      LIBS="$LIBS $LIBICONV"
      AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
        [iconv_t cd = iconv_open("","");
         iconv(cd,NULL,NULL,NULL,NULL);
         iconv_close(cd);],
        [am_cv_lib_iconv=yes]
        [am_cv_func_iconv=yes])
      LIBS="$am_save_LIBS"
    fi
  ])
  if test "$am_cv_func_iconv" = yes; then
    AC_CACHE_CHECK([for working iconv], [am_cv_func_iconv_works], [
      dnl This tests against bugs in AIX 5.1 and HP-UX 11.11.
      am_save_LIBS="$LIBS"
      if test $am_cv_lib_iconv = yes; then
        LIBS="$LIBS $LIBICONV"
      fi
      AC_TRY_RUN([
#include <iconv.h>
#include <string.h>
int main ()
{
  /* Test against AIX 5.1 bug: Failures are not distinguishable from successful
     returns.  */
  {
    iconv_t cd_utf8_to_88591 = iconv_open ("ISO8859-1", "UTF-8");
    if (cd_utf8_to_88591 != (iconv_t)(-1))
      {
        static const char input[] = "\342\202\254"; /* EURO SIGN */
        char buf[10];
        const char *inptr = input;
        size_t inbytesleft = strlen (input);
        char *outptr = buf;
        size_t outbytesleft = sizeof (buf);
        size_t res = iconv (cd_utf8_to_88591,
                            (char **) &inptr, &inbytesleft,
                            &outptr, &outbytesleft);
        if (res == 0)
          return 1;
      }
  }
#if 0 /* This bug could be worked around by the caller.  */
  /* Test against HP-UX 11.11 bug: Positive return value instead of 0.  */
  {
    iconv_t cd_88591_to_utf8 = iconv_open ("utf8", "iso88591");
    if (cd_88591_to_utf8 != (iconv_t)(-1))
      {
        static const char input[] = "\304rger mit b\366sen B\374bchen ohne Augenma\337";
        char buf[50];
        const char *inptr = input;
        size_t inbytesleft = strlen (input);
        char *outptr = buf;
        size_t outbytesleft = sizeof (buf);
        size_t res = iconv (cd_88591_to_utf8,
                            (char **) &inptr, &inbytesleft,
                            &outptr, &outbytesleft);
        if ((int)res > 0)
          return 1;
      }
  }
#endif
  /* Test against HP-UX 11.11 bug: No converter from EUC-JP to UTF-8 is
     provided.  */
  if (/* Try standardized names.  */
      iconv_open ("UTF-8", "EUC-JP") == (iconv_t)(-1)
      /* Try IRIX, OSF/1 names.  */
      && iconv_open ("UTF-8", "eucJP") == (iconv_t)(-1)
      /* Try AIX names.  */
      && iconv_open ("UTF-8", "IBM-eucJP") == (iconv_t)(-1)
      /* Try HP-UX names.  */
      && iconv_open ("utf8", "eucJP") == (iconv_t)(-1))
    return 1;
  return 0;
}], [am_cv_func_iconv_works=yes], [am_cv_func_iconv_works=no],
        [case "$host_os" in
           aix* | hpux*) am_cv_func_iconv_works="guessing no" ;;
           *)            am_cv_func_iconv_works="guessing yes" ;;
         esac])
      LIBS="$am_save_LIBS"
    ])
    case "$am_cv_func_iconv_works" in
      *no) am_func_iconv=no am_cv_lib_iconv=no ;;
      *)   am_func_iconv=yes ;;
    esac
  else
    am_func_iconv=no am_cv_lib_iconv=no
  fi
  if test "$am_func_iconv" = yes; then
    AC_DEFINE([HAVE_ICONV], [1],
      [Define if you have the iconv() function and it works.])
  fi
  if test "$am_cv_lib_iconv" = yes; then
    AC_MSG_CHECKING([how to link with libiconv])
    AC_MSG_RESULT([$LIBICONV])
  else
    dnl If $LIBICONV didn't lead to a usable library, we don't need $INCICONV
    dnl either.
    CPPFLAGS="$am_save_CPPFLAGS"
    LIBICONV=
    LTLIBICONV=
  fi
  AC_SUBST([LIBICONV])
  AC_SUBST([LTLIBICONV])
])

AC_DEFUN([AM_ICONV],
[
  AM_ICONV_LINK
  if test "$am_cv_func_iconv" = yes; then
    AC_MSG_CHECKING([for iconv declaration])
    AC_CACHE_VAL([am_cv_proto_iconv], [
      AC_TRY_COMPILE([
#include <stdlib.h>
#include <iconv.h>
extern
#ifdef __cplusplus
"C"
#endif
#if defined(__STDC__) || defined(__cplusplus)
size_t iconv (iconv_t cd, char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);
#else
size_t iconv();
#endif
], [], [am_cv_proto_iconv_arg1=""], [am_cv_proto_iconv_arg1="const"])
      am_cv_proto_iconv="extern size_t iconv (iconv_t cd, $am_cv_proto_iconv_arg1 char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);"])
    am_cv_proto_iconv=`echo "[$]am_cv_proto_iconv" | tr -s ' ' | sed -e 's/( /(/'`
    AC_MSG_RESULT([${ac_t:-
         }$am_cv_proto_iconv])
    AC_DEFINE_UNQUOTED([ICONV_CONST], [$am_cv_proto_iconv_arg1],
      [Define as const if the declaration of iconv() needs const.])
  fi
])

# include_next.m4 serial 10
dnl Copyright (C) 2006-2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Paul Eggert and Derek Price.

dnl Sets INCLUDE_NEXT and PRAGMA_SYSTEM_HEADER.
dnl
dnl INCLUDE_NEXT expands to 'include_next' if the compiler supports it, or to
dnl 'include' otherwise.
dnl
dnl INCLUDE_NEXT_AS_FIRST_DIRECTIVE expands to 'include_next' if the compiler
dnl supports it in the special case that it is the first include directive in
dnl the given file, or to 'include' otherwise.
dnl
dnl PRAGMA_SYSTEM_HEADER can be used in files that contain #include_next,
dnl so as to avoid GCC warnings when the gcc option -pedantic is used.
dnl '#pragma GCC system_header' has the same effect as if the file was found
dnl through the include search path specified with '-isystem' options (as
dnl opposed to the search path specified with '-I' options). Namely, gcc
dnl does not warn about some things, and on some systems (Solaris and Interix)
dnl __STDC__ evaluates to 0 instead of to 1. The latter is an undesired side
dnl effect; we are therefore careful to use 'defined __STDC__' or '1' instead
dnl of plain '__STDC__'.

AC_DEFUN([gl_INCLUDE_NEXT],
[
  AC_LANG_PREPROC_REQUIRE()
  AC_CACHE_CHECK([whether the preprocessor supports include_next],
    [gl_cv_have_include_next],
    [rm -rf conftestd1a conftestd1b conftestd2
     mkdir conftestd1a conftestd1b conftestd2
     dnl The include of <stdio.h> is because IBM C 9.0 on AIX 6.1 supports
     dnl include_next when used as first preprocessor directive in a file,
     dnl but not when preceded by another include directive. Additionally,
     dnl with this same compiler, include_next is a no-op when used in a
     dnl header file that was included by specifying its absolute file name.
     dnl Despite these two bugs, include_next is used in the compiler's
     dnl <math.h>. By virtue of the second bug, we need to use include_next
     dnl as well in this case.
     cat <<EOF > conftestd1a/conftest.h
#define DEFINED_IN_CONFTESTD1
#include_next <conftest.h>
#ifdef DEFINED_IN_CONFTESTD2
int foo;
#else
#error "include_next doesn't work"
#endif
EOF
     cat <<EOF > conftestd1b/conftest.h
#define DEFINED_IN_CONFTESTD1
#include <stdio.h>
#include_next <conftest.h>
#ifdef DEFINED_IN_CONFTESTD2
int foo;
#else
#error "include_next doesn't work"
#endif
EOF
     cat <<EOF > conftestd2/conftest.h
#ifndef DEFINED_IN_CONFTESTD1
#error "include_next test doesn't work"
#endif
#define DEFINED_IN_CONFTESTD2
EOF
     gl_save_CPPFLAGS="$CPPFLAGS"
     CPPFLAGS="$gl_save_CPPFLAGS -Iconftestd1b -Iconftestd2"
     AC_COMPILE_IFELSE([#include <conftest.h>],
       [gl_cv_have_include_next=yes],
       [CPPFLAGS="$gl_save_CPPFLAGS -Iconftestd1a -Iconftestd2"
        AC_COMPILE_IFELSE([#include <conftest.h>],
          [gl_cv_have_include_next=buggy],
          [gl_cv_have_include_next=no])
       ])
     CPPFLAGS="$gl_save_CPPFLAGS"
     rm -rf conftestd1a conftestd1b conftestd2
    ])
  PRAGMA_SYSTEM_HEADER=
  if test $gl_cv_have_include_next = yes; then
    INCLUDE_NEXT=include_next
    INCLUDE_NEXT_AS_FIRST_DIRECTIVE=include_next
    if test -n "$GCC"; then
      PRAGMA_SYSTEM_HEADER='#pragma GCC system_header'
    fi
  else
    if test $gl_cv_have_include_next = buggy; then
      INCLUDE_NEXT=include
      INCLUDE_NEXT_AS_FIRST_DIRECTIVE=include_next
    else
      INCLUDE_NEXT=include
      INCLUDE_NEXT_AS_FIRST_DIRECTIVE=include
    fi
  fi
  AC_SUBST([INCLUDE_NEXT])
  AC_SUBST([INCLUDE_NEXT_AS_FIRST_DIRECTIVE])
  AC_SUBST([PRAGMA_SYSTEM_HEADER])
])

# gl_CHECK_NEXT_HEADERS(HEADER1 HEADER2 ...)
# ------------------------------------------
# For each arg foo.h, if #include_next works, define NEXT_FOO_H to be
# '<foo.h>'; otherwise define it to be
# '"///usr/include/foo.h"', or whatever other absolute file name is suitable.
# That way, a header file with the following line:
#	#@INCLUDE_NEXT@ @NEXT_FOO_H@
# behaves (after sed substitution) as if it contained
#	#include_next <foo.h>
# even if the compiler does not support include_next.
# The three "///" are to pacify Sun C 5.8, which otherwise would say
# "warning: #include of /usr/include/... may be non-portable".
# Use `""', not `<>', so that the /// cannot be confused with a C99 comment.
# Note: This macro assumes that the header file is not empty after
# preprocessing, i.e. it does not only define preprocessor macros but also
# provides some type/enum definitions or function/variable declarations.
AC_DEFUN([gl_CHECK_NEXT_HEADERS],
[
  AC_REQUIRE([gl_INCLUDE_NEXT])
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_CHECK_HEADERS_ONCE([$1])

  m4_foreach_w([gl_HEADER_NAME], [$1],
    [AS_VAR_PUSHDEF([gl_next_header],
		    [gl_cv_next_]m4_quote(m4_defn([gl_HEADER_NAME])))
     if test $gl_cv_have_include_next = yes; then
       AS_VAR_SET([gl_next_header], ['<'gl_HEADER_NAME'>'])
     else
       AC_CACHE_CHECK(
	 [absolute name of <]m4_quote(m4_defn([gl_HEADER_NAME]))[>],
	 m4_quote(m4_defn([gl_next_header])),
	 [AS_VAR_PUSHDEF([gl_header_exists],
			 [ac_cv_header_]m4_quote(m4_defn([gl_HEADER_NAME])))
	  if test AS_VAR_GET(gl_header_exists) = yes; then
	    AC_LANG_CONFTEST(
	      [AC_LANG_SOURCE(
		 [[#include <]]m4_dquote(m4_defn([gl_HEADER_NAME]))[[>]]
	       )])
	    dnl AIX "xlc -E" and "cc -E" omit #line directives for header files
	    dnl that contain only a #include of other header files and no
	    dnl non-comment tokens of their own. This leads to a failure to
	    dnl detect the absolute name of <dirent.h>, <signal.h>, <poll.h>
	    dnl and others. The workaround is to force preservation of comments
	    dnl through option -C. This ensures all necessary #line directives
	    dnl are present. GCC supports option -C as well.
	    case "$host_os" in
	      aix*) gl_absname_cpp="$ac_cpp -C" ;;
	      *)    gl_absname_cpp="$ac_cpp" ;;
	    esac
	    dnl eval is necessary to expand gl_absname_cpp.
	    dnl Ultrix and Pyramid sh refuse to redirect output of eval,
	    dnl so use subshell.
	    AS_VAR_SET([gl_next_header],
	      ['"'`(eval "$gl_absname_cpp conftest.$ac_ext") 2>&AS_MESSAGE_LOG_FD |
	       sed -n '\#/]m4_quote(m4_defn([gl_HEADER_NAME]))[#{
		 s#.*"\(.*/]m4_quote(m4_defn([gl_HEADER_NAME]))[\)".*#\1#
		 s#^/[^/]#//&#
		 p
		 q
	       }'`'"'])
	  else
	    AS_VAR_SET([gl_next_header], ['<'gl_HEADER_NAME'>'])
	  fi
	  AS_VAR_POPDEF([gl_header_exists])])
     fi
     AC_SUBST(
       AS_TR_CPP([NEXT_]m4_quote(m4_defn([gl_HEADER_NAME]))),
       [AS_VAR_GET([gl_next_header])])
     AS_VAR_POPDEF([gl_next_header])])
])

# intlmacosx.m4 serial 3 (gettext-0.18)
dnl Copyright (C) 2004-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Checks for special options needed on MacOS X.
dnl Defines INTL_MACOSX_LIBS.
AC_DEFUN([gt_INTL_MACOSX],
[
  dnl Check for API introduced in MacOS X 10.2.
  AC_CACHE_CHECK([for CFPreferencesCopyAppValue],
    [gt_cv_func_CFPreferencesCopyAppValue],
    [gt_save_LIBS="$LIBS"
     LIBS="$LIBS -Wl,-framework -Wl,CoreFoundation"
     AC_TRY_LINK([#include <CoreFoundation/CFPreferences.h>],
       [CFPreferencesCopyAppValue(NULL, NULL)],
       [gt_cv_func_CFPreferencesCopyAppValue=yes],
       [gt_cv_func_CFPreferencesCopyAppValue=no])
     LIBS="$gt_save_LIBS"])
  if test $gt_cv_func_CFPreferencesCopyAppValue = yes; then
    AC_DEFINE([HAVE_CFPREFERENCESCOPYAPPVALUE], [1],
      [Define to 1 if you have the MacOS X function CFPreferencesCopyAppValue in the CoreFoundation framework.])
  fi
  dnl Check for API introduced in MacOS X 10.3.
  AC_CACHE_CHECK([for CFLocaleCopyCurrent], [gt_cv_func_CFLocaleCopyCurrent],
    [gt_save_LIBS="$LIBS"
     LIBS="$LIBS -Wl,-framework -Wl,CoreFoundation"
     AC_TRY_LINK([#include <CoreFoundation/CFLocale.h>], [CFLocaleCopyCurrent();],
       [gt_cv_func_CFLocaleCopyCurrent=yes],
       [gt_cv_func_CFLocaleCopyCurrent=no])
     LIBS="$gt_save_LIBS"])
  if test $gt_cv_func_CFLocaleCopyCurrent = yes; then
    AC_DEFINE([HAVE_CFLOCALECOPYCURRENT], [1],
      [Define to 1 if you have the MacOS X function CFLocaleCopyCurrent in the CoreFoundation framework.])
  fi
  INTL_MACOSX_LIBS=
  if test $gt_cv_func_CFPreferencesCopyAppValue = yes || test $gt_cv_func_CFLocaleCopyCurrent = yes; then
    INTL_MACOSX_LIBS="-Wl,-framework -Wl,CoreFoundation"
  fi
  AC_SUBST([INTL_MACOSX_LIBS])
])

# lcmessage.m4 serial 6 (gettext-0.18)
dnl Copyright (C) 1995-2002, 2004-2005, 2008, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Authors:
dnl   Ulrich Drepper <drepper@cygnus.com>, 1995.

# Check whether LC_MESSAGES is available in <locale.h>.

AC_DEFUN([gt_LC_MESSAGES],
[
  AC_CACHE_CHECK([for LC_MESSAGES], [gt_cv_val_LC_MESSAGES],
    [AC_TRY_LINK([#include <locale.h>], [return LC_MESSAGES],
       [gt_cv_val_LC_MESSAGES=yes], [gt_cv_val_LC_MESSAGES=no])])
  if test $gt_cv_val_LC_MESSAGES = yes; then
    AC_DEFINE([HAVE_LC_MESSAGES], [1],
      [Define if your <locale.h> file defines LC_MESSAGES.])
  fi
])

# lib-ld.m4 serial 4 (gettext-0.18)
dnl Copyright (C) 1996-2003, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl Subroutines of libtool.m4,
dnl with replacements s/AC_/AC_LIB/ and s/lt_cv/acl_cv/ to avoid collision
dnl with libtool.m4.

dnl From libtool-1.4. Sets the variable with_gnu_ld to yes or no.
AC_DEFUN([AC_LIB_PROG_LD_GNU],
[AC_CACHE_CHECK([if the linker ($LD) is GNU ld], [acl_cv_prog_gnu_ld],
[# I'd rather use --version here, but apparently some GNU ld's only accept -v.
case `$LD -v 2>&1 </dev/null` in
*GNU* | *'with BFD'*)
  acl_cv_prog_gnu_ld=yes ;;
*)
  acl_cv_prog_gnu_ld=no ;;
esac])
with_gnu_ld=$acl_cv_prog_gnu_ld
])

dnl From libtool-1.4. Sets the variable LD.
AC_DEFUN([AC_LIB_PROG_LD],
[AC_ARG_WITH([gnu-ld],
[  --with-gnu-ld           assume the C compiler uses GNU ld [default=no]],
test "$withval" = no || with_gnu_ld=yes, with_gnu_ld=no)
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
# Prepare PATH_SEPARATOR.
# The user is always right.
if test "${PATH_SEPARATOR+set}" != set; then
  echo "#! /bin/sh" >conf$$.sh
  echo  "exit 0"   >>conf$$.sh
  chmod +x conf$$.sh
  if (PATH="/nonexistent;."; conf$$.sh) >/dev/null 2>&1; then
    PATH_SEPARATOR=';'
  else
    PATH_SEPARATOR=:
  fi
  rm -f conf$$.sh
fi
ac_prog=ld
if test "$GCC" = yes; then
  # Check if gcc -print-prog-name=ld gives a path.
  AC_MSG_CHECKING([for ld used by GCC])
  case $host in
  *-*-mingw*)
    # gcc leaves a trailing carriage return which upsets mingw
    ac_prog=`($CC -print-prog-name=ld) 2>&5 | tr -d '\015'` ;;
  *)
    ac_prog=`($CC -print-prog-name=ld) 2>&5` ;;
  esac
  case $ac_prog in
    # Accept absolute paths.
    [[\\/]* | [A-Za-z]:[\\/]*)]
      [re_direlt='/[^/][^/]*/\.\./']
      # Canonicalize the path of ld
      ac_prog=`echo $ac_prog| sed 's%\\\\%/%g'`
      while echo $ac_prog | grep "$re_direlt" > /dev/null 2>&1; do
	ac_prog=`echo $ac_prog| sed "s%$re_direlt%/%"`
      done
      test -z "$LD" && LD="$ac_prog"
      ;;
  "")
    # If it fails, then pretend we aren't using GCC.
    ac_prog=ld
    ;;
  *)
    # If it is relative, then search for the first ld in PATH.
    with_gnu_ld=unknown
    ;;
  esac
elif test "$with_gnu_ld" = yes; then
  AC_MSG_CHECKING([for GNU ld])
else
  AC_MSG_CHECKING([for non-GNU ld])
fi
AC_CACHE_VAL([acl_cv_path_LD],
[if test -z "$LD"; then
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}${PATH_SEPARATOR-:}"
  for ac_dir in $PATH; do
    test -z "$ac_dir" && ac_dir=.
    if test -f "$ac_dir/$ac_prog" || test -f "$ac_dir/$ac_prog$ac_exeext"; then
      acl_cv_path_LD="$ac_dir/$ac_prog"
      # Check to see if the program is GNU ld.  I'd rather use --version,
      # but apparently some GNU ld's only accept -v.
      # Break only if it was the GNU/non-GNU ld that we prefer.
      case `"$acl_cv_path_LD" -v 2>&1 < /dev/null` in
      *GNU* | *'with BFD'*)
	test "$with_gnu_ld" != no && break ;;
      *)
	test "$with_gnu_ld" != yes && break ;;
      esac
    fi
  done
  IFS="$ac_save_ifs"
else
  acl_cv_path_LD="$LD" # Let the user override the test with a path.
fi])
LD="$acl_cv_path_LD"
if test -n "$LD"; then
  AC_MSG_RESULT([$LD])
else
  AC_MSG_RESULT([no])
fi
test -z "$LD" && AC_MSG_ERROR([no acceptable ld found in \$PATH])
AC_LIB_PROG_LD_GNU
])

# lib-link.m4 serial 18 (gettext-0.18)
dnl Copyright (C) 2001-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

AC_PREREQ([2.54])

dnl AC_LIB_LINKFLAGS(name [, dependencies]) searches for libname and
dnl the libraries corresponding to explicit and implicit dependencies.
dnl Sets and AC_SUBSTs the LIB${NAME} and LTLIB${NAME} variables and
dnl augments the CPPFLAGS variable.
dnl Sets and AC_SUBSTs the LIB${NAME}_PREFIX variable to nonempty if libname
dnl was found in ${LIB${NAME}_PREFIX}/$acl_libdirstem.
AC_DEFUN([AC_LIB_LINKFLAGS],
[
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  AC_REQUIRE([AC_LIB_RPATH])
  pushdef([Name],[translit([$1],[./-], [___])])
  pushdef([NAME],[translit([$1],[abcdefghijklmnopqrstuvwxyz./-],
                                [ABCDEFGHIJKLMNOPQRSTUVWXYZ___])])
  AC_CACHE_CHECK([how to link with lib[]$1], [ac_cv_lib[]Name[]_libs], [
    AC_LIB_LINKFLAGS_BODY([$1], [$2])
    ac_cv_lib[]Name[]_libs="$LIB[]NAME"
    ac_cv_lib[]Name[]_ltlibs="$LTLIB[]NAME"
    ac_cv_lib[]Name[]_cppflags="$INC[]NAME"
    ac_cv_lib[]Name[]_prefix="$LIB[]NAME[]_PREFIX"
  ])
  LIB[]NAME="$ac_cv_lib[]Name[]_libs"
  LTLIB[]NAME="$ac_cv_lib[]Name[]_ltlibs"
  INC[]NAME="$ac_cv_lib[]Name[]_cppflags"
  LIB[]NAME[]_PREFIX="$ac_cv_lib[]Name[]_prefix"
  AC_LIB_APPENDTOVAR([CPPFLAGS], [$INC]NAME)
  AC_SUBST([LIB]NAME)
  AC_SUBST([LTLIB]NAME)
  AC_SUBST([LIB]NAME[_PREFIX])
  dnl Also set HAVE_LIB[]NAME so that AC_LIB_HAVE_LINKFLAGS can reuse the
  dnl results of this search when this library appears as a dependency.
  HAVE_LIB[]NAME=yes
  popdef([NAME])
  popdef([Name])
])

dnl AC_LIB_HAVE_LINKFLAGS(name, dependencies, includes, testcode)
dnl searches for libname and the libraries corresponding to explicit and
dnl implicit dependencies, together with the specified include files and
dnl the ability to compile and link the specified testcode. If found, it
dnl sets and AC_SUBSTs HAVE_LIB${NAME}=yes and the LIB${NAME} and
dnl LTLIB${NAME} variables and augments the CPPFLAGS variable, and
dnl #defines HAVE_LIB${NAME} to 1. Otherwise, it sets and AC_SUBSTs
dnl HAVE_LIB${NAME}=no and LIB${NAME} and LTLIB${NAME} to empty.
dnl Sets and AC_SUBSTs the LIB${NAME}_PREFIX variable to nonempty if libname
dnl was found in ${LIB${NAME}_PREFIX}/$acl_libdirstem.
AC_DEFUN([AC_LIB_HAVE_LINKFLAGS],
[
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  AC_REQUIRE([AC_LIB_RPATH])
  pushdef([Name],[translit([$1],[./-], [___])])
  pushdef([NAME],[translit([$1],[abcdefghijklmnopqrstuvwxyz./-],
                                [ABCDEFGHIJKLMNOPQRSTUVWXYZ___])])

  dnl Search for lib[]Name and define LIB[]NAME, LTLIB[]NAME and INC[]NAME
  dnl accordingly.
  AC_LIB_LINKFLAGS_BODY([$1], [$2])

  dnl Add $INC[]NAME to CPPFLAGS before performing the following checks,
  dnl because if the user has installed lib[]Name and not disabled its use
  dnl via --without-lib[]Name-prefix, he wants to use it.
  ac_save_CPPFLAGS="$CPPFLAGS"
  AC_LIB_APPENDTOVAR([CPPFLAGS], [$INC]NAME)

  AC_CACHE_CHECK([for lib[]$1], [ac_cv_lib[]Name], [
    ac_save_LIBS="$LIBS"
    LIBS="$LIBS $LIB[]NAME"
    AC_TRY_LINK([$3], [$4], [ac_cv_lib[]Name=yes], [ac_cv_lib[]Name=no])
    LIBS="$ac_save_LIBS"
  ])
  if test "$ac_cv_lib[]Name" = yes; then
    HAVE_LIB[]NAME=yes
    AC_DEFINE([HAVE_LIB]NAME, 1, [Define if you have the $1 library.])
    AC_MSG_CHECKING([how to link with lib[]$1])
    AC_MSG_RESULT([$LIB[]NAME])
  else
    HAVE_LIB[]NAME=no
    dnl If $LIB[]NAME didn't lead to a usable library, we don't need
    dnl $INC[]NAME either.
    CPPFLAGS="$ac_save_CPPFLAGS"
    LIB[]NAME=
    LTLIB[]NAME=
    LIB[]NAME[]_PREFIX=
  fi
  AC_SUBST([HAVE_LIB]NAME)
  AC_SUBST([LIB]NAME)
  AC_SUBST([LTLIB]NAME)
  AC_SUBST([LIB]NAME[_PREFIX])
  popdef([NAME])
  popdef([Name])
])

dnl Determine the platform dependent parameters needed to use rpath:
dnl   acl_libext,
dnl   acl_shlibext,
dnl   acl_hardcode_libdir_flag_spec,
dnl   acl_hardcode_libdir_separator,
dnl   acl_hardcode_direct,
dnl   acl_hardcode_minus_L.
AC_DEFUN([AC_LIB_RPATH],
[
  dnl Tell automake >= 1.10 to complain if config.rpath is missing.
  m4_ifdef([AC_REQUIRE_AUX_FILE], [AC_REQUIRE_AUX_FILE([config.rpath])])
  AC_REQUIRE([AC_PROG_CC])                dnl we use $CC, $GCC, $LDFLAGS
  AC_REQUIRE([AC_LIB_PROG_LD])            dnl we use $LD, $with_gnu_ld
  AC_REQUIRE([AC_CANONICAL_HOST])         dnl we use $host
  AC_REQUIRE([AC_CONFIG_AUX_DIR_DEFAULT]) dnl we use $ac_aux_dir
  AC_CACHE_CHECK([for shared library run path origin], [acl_cv_rpath], [
    CC="$CC" GCC="$GCC" LDFLAGS="$LDFLAGS" LD="$LD" with_gnu_ld="$with_gnu_ld" \
    ${CONFIG_SHELL-/bin/sh} "$ac_aux_dir/config.rpath" "$host" > conftest.sh
    . ./conftest.sh
    rm -f ./conftest.sh
    acl_cv_rpath=done
  ])
  wl="$acl_cv_wl"
  acl_libext="$acl_cv_libext"
  acl_shlibext="$acl_cv_shlibext"
  acl_libname_spec="$acl_cv_libname_spec"
  acl_library_names_spec="$acl_cv_library_names_spec"
  acl_hardcode_libdir_flag_spec="$acl_cv_hardcode_libdir_flag_spec"
  acl_hardcode_libdir_separator="$acl_cv_hardcode_libdir_separator"
  acl_hardcode_direct="$acl_cv_hardcode_direct"
  acl_hardcode_minus_L="$acl_cv_hardcode_minus_L"
  dnl Determine whether the user wants rpath handling at all.
  AC_ARG_ENABLE([rpath],
    [  --disable-rpath         do not hardcode runtime library paths],
    :, enable_rpath=yes)
])

dnl AC_LIB_FROMPACKAGE(name, package)
dnl declares that libname comes from the given package. The configure file
dnl will then not have a --with-libname-prefix option but a
dnl --with-package-prefix option. Several libraries can come from the same
dnl package. This declaration must occur before an AC_LIB_LINKFLAGS or similar
dnl macro call that searches for libname.
AC_DEFUN([AC_LIB_FROMPACKAGE],
[
  pushdef([NAME],[translit([$1],[abcdefghijklmnopqrstuvwxyz./-],
                                [ABCDEFGHIJKLMNOPQRSTUVWXYZ___])])
  define([acl_frompackage_]NAME, [$2])
  popdef([NAME])
  pushdef([PACK],[$2])
  pushdef([PACKUP],[translit(PACK,[abcdefghijklmnopqrstuvwxyz./-],
                                  [ABCDEFGHIJKLMNOPQRSTUVWXYZ___])])
  define([acl_libsinpackage_]PACKUP,
    m4_ifdef([acl_libsinpackage_]PACKUP, [acl_libsinpackage_]PACKUP[[, ]],)[lib$1])
  popdef([PACKUP])
  popdef([PACK])
])

dnl AC_LIB_LINKFLAGS_BODY(name [, dependencies]) searches for libname and
dnl the libraries corresponding to explicit and implicit dependencies.
dnl Sets the LIB${NAME}, LTLIB${NAME} and INC${NAME} variables.
dnl Also, sets the LIB${NAME}_PREFIX variable to nonempty if libname was found
dnl in ${LIB${NAME}_PREFIX}/$acl_libdirstem.
AC_DEFUN([AC_LIB_LINKFLAGS_BODY],
[
  AC_REQUIRE([AC_LIB_PREPARE_MULTILIB])
  pushdef([NAME],[translit([$1],[abcdefghijklmnopqrstuvwxyz./-],
                                [ABCDEFGHIJKLMNOPQRSTUVWXYZ___])])
  pushdef([PACK],[m4_ifdef([acl_frompackage_]NAME, [acl_frompackage_]NAME, lib[$1])])
  pushdef([PACKUP],[translit(PACK,[abcdefghijklmnopqrstuvwxyz./-],
                                  [ABCDEFGHIJKLMNOPQRSTUVWXYZ___])])
  pushdef([PACKLIBS],[m4_ifdef([acl_frompackage_]NAME, [acl_libsinpackage_]PACKUP, lib[$1])])
  dnl Autoconf >= 2.61 supports dots in --with options.
  pushdef([P_A_C_K],[m4_if(m4_version_compare(m4_defn([m4_PACKAGE_VERSION]),[2.61]),[-1],[translit(PACK,[.],[_])],PACK)])
  dnl By default, look in $includedir and $libdir.
  use_additional=yes
  AC_LIB_WITH_FINAL_PREFIX([
    eval additional_includedir=\"$includedir\"
    eval additional_libdir=\"$libdir\"
  ])
  AC_ARG_WITH(P_A_C_K[-prefix],
[[  --with-]]P_A_C_K[[-prefix[=DIR]  search for ]PACKLIBS[ in DIR/include and DIR/lib
  --without-]]P_A_C_K[[-prefix     don't search for ]PACKLIBS[ in includedir and libdir]],
[
    if test "X$withval" = "Xno"; then
      use_additional=no
    else
      if test "X$withval" = "X"; then
        AC_LIB_WITH_FINAL_PREFIX([
          eval additional_includedir=\"$includedir\"
          eval additional_libdir=\"$libdir\"
        ])
      else
        additional_includedir="$withval/include"
        additional_libdir="$withval/$acl_libdirstem"
        if test "$acl_libdirstem2" != "$acl_libdirstem" \
           && ! test -d "$withval/$acl_libdirstem"; then
          additional_libdir="$withval/$acl_libdirstem2"
        fi
      fi
    fi
])
  dnl Search the library and its dependencies in $additional_libdir and
  dnl $LDFLAGS. Using breadth-first-seach.
  LIB[]NAME=
  LTLIB[]NAME=
  INC[]NAME=
  LIB[]NAME[]_PREFIX=
  rpathdirs=
  ltrpathdirs=
  names_already_handled=
  names_next_round='$1 $2'
  while test -n "$names_next_round"; do
    names_this_round="$names_next_round"
    names_next_round=
    for name in $names_this_round; do
      already_handled=
      for n in $names_already_handled; do
        if test "$n" = "$name"; then
          already_handled=yes
          break
        fi
      done
      if test -z "$already_handled"; then
        names_already_handled="$names_already_handled $name"
        dnl See if it was already located by an earlier AC_LIB_LINKFLAGS
        dnl or AC_LIB_HAVE_LINKFLAGS call.
        uppername=`echo "$name" | sed -e 'y|abcdefghijklmnopqrstuvwxyz./-|ABCDEFGHIJKLMNOPQRSTUVWXYZ___|'`
        eval value=\"\$HAVE_LIB$uppername\"
        if test -n "$value"; then
          if test "$value" = yes; then
            eval value=\"\$LIB$uppername\"
            test -z "$value" || LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$value"
            eval value=\"\$LTLIB$uppername\"
            test -z "$value" || LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }$value"
          else
            dnl An earlier call to AC_LIB_HAVE_LINKFLAGS has determined
            dnl that this library doesn't exist. So just drop it.
            :
          fi
        else
          dnl Search the library lib$name in $additional_libdir and $LDFLAGS
          dnl and the already constructed $LIBNAME/$LTLIBNAME.
          found_dir=
          found_la=
          found_so=
          found_a=
          eval libname=\"$acl_libname_spec\"    # typically: libname=lib$name
          if test -n "$acl_shlibext"; then
            shrext=".$acl_shlibext"             # typically: shrext=.so
          else
            shrext=
          fi
          if test $use_additional = yes; then
            dir="$additional_libdir"
            dnl The same code as in the loop below:
            dnl First look for a shared library.
            if test -n "$acl_shlibext"; then
              if test -f "$dir/$libname$shrext"; then
                found_dir="$dir"
                found_so="$dir/$libname$shrext"
              else
                if test "$acl_library_names_spec" = '$libname$shrext$versuffix'; then
                  ver=`(cd "$dir" && \
                        for f in "$libname$shrext".*; do echo "$f"; done \
                        | sed -e "s,^$libname$shrext\\\\.,," \
                        | sort -t '.' -n -r -k1,1 -k2,2 -k3,3 -k4,4 -k5,5 \
                        | sed 1q ) 2>/dev/null`
                  if test -n "$ver" && test -f "$dir/$libname$shrext.$ver"; then
                    found_dir="$dir"
                    found_so="$dir/$libname$shrext.$ver"
                  fi
                else
                  eval library_names=\"$acl_library_names_spec\"
                  for f in $library_names; do
                    if test -f "$dir/$f"; then
                      found_dir="$dir"
                      found_so="$dir/$f"
                      break
                    fi
                  done
                fi
              fi
            fi
            dnl Then look for a static library.
            if test "X$found_dir" = "X"; then
              if test -f "$dir/$libname.$acl_libext"; then
                found_dir="$dir"
                found_a="$dir/$libname.$acl_libext"
              fi
            fi
            if test "X$found_dir" != "X"; then
              if test -f "$dir/$libname.la"; then
                found_la="$dir/$libname.la"
              fi
            fi
          fi
          if test "X$found_dir" = "X"; then
            for x in $LDFLAGS $LTLIB[]NAME; do
              AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
              case "$x" in
                -L*)
                  dir=`echo "X$x" | sed -e 's/^X-L//'`
                  dnl First look for a shared library.
                  if test -n "$acl_shlibext"; then
                    if test -f "$dir/$libname$shrext"; then
                      found_dir="$dir"
                      found_so="$dir/$libname$shrext"
                    else
                      if test "$acl_library_names_spec" = '$libname$shrext$versuffix'; then
                        ver=`(cd "$dir" && \
                              for f in "$libname$shrext".*; do echo "$f"; done \
                              | sed -e "s,^$libname$shrext\\\\.,," \
                              | sort -t '.' -n -r -k1,1 -k2,2 -k3,3 -k4,4 -k5,5 \
                              | sed 1q ) 2>/dev/null`
                        if test -n "$ver" && test -f "$dir/$libname$shrext.$ver"; then
                          found_dir="$dir"
                          found_so="$dir/$libname$shrext.$ver"
                        fi
                      else
                        eval library_names=\"$acl_library_names_spec\"
                        for f in $library_names; do
                          if test -f "$dir/$f"; then
                            found_dir="$dir"
                            found_so="$dir/$f"
                            break
                          fi
                        done
                      fi
                    fi
                  fi
                  dnl Then look for a static library.
                  if test "X$found_dir" = "X"; then
                    if test -f "$dir/$libname.$acl_libext"; then
                      found_dir="$dir"
                      found_a="$dir/$libname.$acl_libext"
                    fi
                  fi
                  if test "X$found_dir" != "X"; then
                    if test -f "$dir/$libname.la"; then
                      found_la="$dir/$libname.la"
                    fi
                  fi
                  ;;
              esac
              if test "X$found_dir" != "X"; then
                break
              fi
            done
          fi
          if test "X$found_dir" != "X"; then
            dnl Found the library.
            LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }-L$found_dir -l$name"
            if test "X$found_so" != "X"; then
              dnl Linking with a shared library. We attempt to hardcode its
              dnl directory into the executable's runpath, unless it's the
              dnl standard /usr/lib.
              if test "$enable_rpath" = no \
                 || test "X$found_dir" = "X/usr/$acl_libdirstem" \
                 || test "X$found_dir" = "X/usr/$acl_libdirstem2"; then
                dnl No hardcoding is needed.
                LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$found_so"
              else
                dnl Use an explicit option to hardcode DIR into the resulting
                dnl binary.
                dnl Potentially add DIR to ltrpathdirs.
                dnl The ltrpathdirs will be appended to $LTLIBNAME at the end.
                haveit=
                for x in $ltrpathdirs; do
                  if test "X$x" = "X$found_dir"; then
                    haveit=yes
                    break
                  fi
                done
                if test -z "$haveit"; then
                  ltrpathdirs="$ltrpathdirs $found_dir"
                fi
                dnl The hardcoding into $LIBNAME is system dependent.
                if test "$acl_hardcode_direct" = yes; then
                  dnl Using DIR/libNAME.so during linking hardcodes DIR into the
                  dnl resulting binary.
                  LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$found_so"
                else
                  if test -n "$acl_hardcode_libdir_flag_spec" && test "$acl_hardcode_minus_L" = no; then
                    dnl Use an explicit option to hardcode DIR into the resulting
                    dnl binary.
                    LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$found_so"
                    dnl Potentially add DIR to rpathdirs.
                    dnl The rpathdirs will be appended to $LIBNAME at the end.
                    haveit=
                    for x in $rpathdirs; do
                      if test "X$x" = "X$found_dir"; then
                        haveit=yes
                        break
                      fi
                    done
                    if test -z "$haveit"; then
                      rpathdirs="$rpathdirs $found_dir"
                    fi
                  else
                    dnl Rely on "-L$found_dir".
                    dnl But don't add it if it's already contained in the LDFLAGS
                    dnl or the already constructed $LIBNAME
                    haveit=
                    for x in $LDFLAGS $LIB[]NAME; do
                      AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
                      if test "X$x" = "X-L$found_dir"; then
                        haveit=yes
                        break
                      fi
                    done
                    if test -z "$haveit"; then
                      LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }-L$found_dir"
                    fi
                    if test "$acl_hardcode_minus_L" != no; then
                      dnl FIXME: Not sure whether we should use
                      dnl "-L$found_dir -l$name" or "-L$found_dir $found_so"
                      dnl here.
                      LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$found_so"
                    else
                      dnl We cannot use $acl_hardcode_runpath_var and LD_RUN_PATH
                      dnl here, because this doesn't fit in flags passed to the
                      dnl compiler. So give up. No hardcoding. This affects only
                      dnl very old systems.
                      dnl FIXME: Not sure whether we should use
                      dnl "-L$found_dir -l$name" or "-L$found_dir $found_so"
                      dnl here.
                      LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }-l$name"
                    fi
                  fi
                fi
              fi
            else
              if test "X$found_a" != "X"; then
                dnl Linking with a static library.
                LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$found_a"
              else
                dnl We shouldn't come here, but anyway it's good to have a
                dnl fallback.
                LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }-L$found_dir -l$name"
              fi
            fi
            dnl Assume the include files are nearby.
            additional_includedir=
            case "$found_dir" in
              */$acl_libdirstem | */$acl_libdirstem/)
                basedir=`echo "X$found_dir" | sed -e 's,^X,,' -e "s,/$acl_libdirstem/"'*$,,'`
                if test "$name" = '$1'; then
                  LIB[]NAME[]_PREFIX="$basedir"
                fi
                additional_includedir="$basedir/include"
                ;;
              */$acl_libdirstem2 | */$acl_libdirstem2/)
                basedir=`echo "X$found_dir" | sed -e 's,^X,,' -e "s,/$acl_libdirstem2/"'*$,,'`
                if test "$name" = '$1'; then
                  LIB[]NAME[]_PREFIX="$basedir"
                fi
                additional_includedir="$basedir/include"
                ;;
            esac
            if test "X$additional_includedir" != "X"; then
              dnl Potentially add $additional_includedir to $INCNAME.
              dnl But don't add it
              dnl   1. if it's the standard /usr/include,
              dnl   2. if it's /usr/local/include and we are using GCC on Linux,
              dnl   3. if it's already present in $CPPFLAGS or the already
              dnl      constructed $INCNAME,
              dnl   4. if it doesn't exist as a directory.
              if test "X$additional_includedir" != "X/usr/include"; then
                haveit=
                if test "X$additional_includedir" = "X/usr/local/include"; then
                  if test -n "$GCC"; then
                    case $host_os in
                      linux* | gnu* | k*bsd*-gnu) haveit=yes;;
                    esac
                  fi
                fi
                if test -z "$haveit"; then
                  for x in $CPPFLAGS $INC[]NAME; do
                    AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
                    if test "X$x" = "X-I$additional_includedir"; then
                      haveit=yes
                      break
                    fi
                  done
                  if test -z "$haveit"; then
                    if test -d "$additional_includedir"; then
                      dnl Really add $additional_includedir to $INCNAME.
                      INC[]NAME="${INC[]NAME}${INC[]NAME:+ }-I$additional_includedir"
                    fi
                  fi
                fi
              fi
            fi
            dnl Look for dependencies.
            if test -n "$found_la"; then
              dnl Read the .la file. It defines the variables
              dnl dlname, library_names, old_library, dependency_libs, current,
              dnl age, revision, installed, dlopen, dlpreopen, libdir.
              save_libdir="$libdir"
              case "$found_la" in
                */* | *\\*) . "$found_la" ;;
                *) . "./$found_la" ;;
              esac
              libdir="$save_libdir"
              dnl We use only dependency_libs.
              for dep in $dependency_libs; do
                case "$dep" in
                  -L*)
                    additional_libdir=`echo "X$dep" | sed -e 's/^X-L//'`
                    dnl Potentially add $additional_libdir to $LIBNAME and $LTLIBNAME.
                    dnl But don't add it
                    dnl   1. if it's the standard /usr/lib,
                    dnl   2. if it's /usr/local/lib and we are using GCC on Linux,
                    dnl   3. if it's already present in $LDFLAGS or the already
                    dnl      constructed $LIBNAME,
                    dnl   4. if it doesn't exist as a directory.
                    if test "X$additional_libdir" != "X/usr/$acl_libdirstem" \
                       && test "X$additional_libdir" != "X/usr/$acl_libdirstem2"; then
                      haveit=
                      if test "X$additional_libdir" = "X/usr/local/$acl_libdirstem" \
                         || test "X$additional_libdir" = "X/usr/local/$acl_libdirstem2"; then
                        if test -n "$GCC"; then
                          case $host_os in
                            linux* | gnu* | k*bsd*-gnu) haveit=yes;;
                          esac
                        fi
                      fi
                      if test -z "$haveit"; then
                        haveit=
                        for x in $LDFLAGS $LIB[]NAME; do
                          AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
                          if test "X$x" = "X-L$additional_libdir"; then
                            haveit=yes
                            break
                          fi
                        done
                        if test -z "$haveit"; then
                          if test -d "$additional_libdir"; then
                            dnl Really add $additional_libdir to $LIBNAME.
                            LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }-L$additional_libdir"
                          fi
                        fi
                        haveit=
                        for x in $LDFLAGS $LTLIB[]NAME; do
                          AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
                          if test "X$x" = "X-L$additional_libdir"; then
                            haveit=yes
                            break
                          fi
                        done
                        if test -z "$haveit"; then
                          if test -d "$additional_libdir"; then
                            dnl Really add $additional_libdir to $LTLIBNAME.
                            LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }-L$additional_libdir"
                          fi
                        fi
                      fi
                    fi
                    ;;
                  -R*)
                    dir=`echo "X$dep" | sed -e 's/^X-R//'`
                    if test "$enable_rpath" != no; then
                      dnl Potentially add DIR to rpathdirs.
                      dnl The rpathdirs will be appended to $LIBNAME at the end.
                      haveit=
                      for x in $rpathdirs; do
                        if test "X$x" = "X$dir"; then
                          haveit=yes
                          break
                        fi
                      done
                      if test -z "$haveit"; then
                        rpathdirs="$rpathdirs $dir"
                      fi
                      dnl Potentially add DIR to ltrpathdirs.
                      dnl The ltrpathdirs will be appended to $LTLIBNAME at the end.
                      haveit=
                      for x in $ltrpathdirs; do
                        if test "X$x" = "X$dir"; then
                          haveit=yes
                          break
                        fi
                      done
                      if test -z "$haveit"; then
                        ltrpathdirs="$ltrpathdirs $dir"
                      fi
                    fi
                    ;;
                  -l*)
                    dnl Handle this in the next round.
                    names_next_round="$names_next_round "`echo "X$dep" | sed -e 's/^X-l//'`
                    ;;
                  *.la)
                    dnl Handle this in the next round. Throw away the .la's
                    dnl directory; it is already contained in a preceding -L
                    dnl option.
                    names_next_round="$names_next_round "`echo "X$dep" | sed -e 's,^X.*/,,' -e 's,^lib,,' -e 's,\.la$,,'`
                    ;;
                  *)
                    dnl Most likely an immediate library name.
                    LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$dep"
                    LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }$dep"
                    ;;
                esac
              done
            fi
          else
            dnl Didn't find the library; assume it is in the system directories
            dnl known to the linker and runtime loader. (All the system
            dnl directories known to the linker should also be known to the
            dnl runtime loader, otherwise the system is severely misconfigured.)
            LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }-l$name"
            LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }-l$name"
          fi
        fi
      fi
    done
  done
  if test "X$rpathdirs" != "X"; then
    if test -n "$acl_hardcode_libdir_separator"; then
      dnl Weird platform: only the last -rpath option counts, the user must
      dnl pass all path elements in one option. We can arrange that for a
      dnl single library, but not when more than one $LIBNAMEs are used.
      alldirs=
      for found_dir in $rpathdirs; do
        alldirs="${alldirs}${alldirs:+$acl_hardcode_libdir_separator}$found_dir"
      done
      dnl Note: acl_hardcode_libdir_flag_spec uses $libdir and $wl.
      acl_save_libdir="$libdir"
      libdir="$alldirs"
      eval flag=\"$acl_hardcode_libdir_flag_spec\"
      libdir="$acl_save_libdir"
      LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$flag"
    else
      dnl The -rpath options are cumulative.
      for found_dir in $rpathdirs; do
        acl_save_libdir="$libdir"
        libdir="$found_dir"
        eval flag=\"$acl_hardcode_libdir_flag_spec\"
        libdir="$acl_save_libdir"
        LIB[]NAME="${LIB[]NAME}${LIB[]NAME:+ }$flag"
      done
    fi
  fi
  if test "X$ltrpathdirs" != "X"; then
    dnl When using libtool, the option that works for both libraries and
    dnl executables is -R. The -R options are cumulative.
    for found_dir in $ltrpathdirs; do
      LTLIB[]NAME="${LTLIB[]NAME}${LTLIB[]NAME:+ }-R$found_dir"
    done
  fi
  popdef([P_A_C_K])
  popdef([PACKLIBS])
  popdef([PACKUP])
  popdef([PACK])
  popdef([NAME])
])

dnl AC_LIB_APPENDTOVAR(VAR, CONTENTS) appends the elements of CONTENTS to VAR,
dnl unless already present in VAR.
dnl Works only for CPPFLAGS, not for LIB* variables because that sometimes
dnl contains two or three consecutive elements that belong together.
AC_DEFUN([AC_LIB_APPENDTOVAR],
[
  for element in [$2]; do
    haveit=
    for x in $[$1]; do
      AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
      if test "X$x" = "X$element"; then
        haveit=yes
        break
      fi
    done
    if test -z "$haveit"; then
      [$1]="${[$1]}${[$1]:+ }$element"
    fi
  done
])

dnl For those cases where a variable contains several -L and -l options
dnl referring to unknown libraries and directories, this macro determines the
dnl necessary additional linker options for the runtime path.
dnl AC_LIB_LINKFLAGS_FROM_LIBS([LDADDVAR], [LIBSVALUE], [USE-LIBTOOL])
dnl sets LDADDVAR to linker options needed together with LIBSVALUE.
dnl If USE-LIBTOOL evaluates to non-empty, linking with libtool is assumed,
dnl otherwise linking without libtool is assumed.
AC_DEFUN([AC_LIB_LINKFLAGS_FROM_LIBS],
[
  AC_REQUIRE([AC_LIB_RPATH])
  AC_REQUIRE([AC_LIB_PREPARE_MULTILIB])
  $1=
  if test "$enable_rpath" != no; then
    if test -n "$acl_hardcode_libdir_flag_spec" && test "$acl_hardcode_minus_L" = no; then
      dnl Use an explicit option to hardcode directories into the resulting
      dnl binary.
      rpathdirs=
      next=
      for opt in $2; do
        if test -n "$next"; then
          dir="$next"
          dnl No need to hardcode the standard /usr/lib.
          if test "X$dir" != "X/usr/$acl_libdirstem" \
             && test "X$dir" != "X/usr/$acl_libdirstem2"; then
            rpathdirs="$rpathdirs $dir"
          fi
          next=
        else
          case $opt in
            -L) next=yes ;;
            -L*) dir=`echo "X$opt" | sed -e 's,^X-L,,'`
                 dnl No need to hardcode the standard /usr/lib.
                 if test "X$dir" != "X/usr/$acl_libdirstem" \
                    && test "X$dir" != "X/usr/$acl_libdirstem2"; then
                   rpathdirs="$rpathdirs $dir"
                 fi
                 next= ;;
            *) next= ;;
          esac
        fi
      done
      if test "X$rpathdirs" != "X"; then
        if test -n ""$3""; then
          dnl libtool is used for linking. Use -R options.
          for dir in $rpathdirs; do
            $1="${$1}${$1:+ }-R$dir"
          done
        else
          dnl The linker is used for linking directly.
          if test -n "$acl_hardcode_libdir_separator"; then
            dnl Weird platform: only the last -rpath option counts, the user
            dnl must pass all path elements in one option.
            alldirs=
            for dir in $rpathdirs; do
              alldirs="${alldirs}${alldirs:+$acl_hardcode_libdir_separator}$dir"
            done
            acl_save_libdir="$libdir"
            libdir="$alldirs"
            eval flag=\"$acl_hardcode_libdir_flag_spec\"
            libdir="$acl_save_libdir"
            $1="$flag"
          else
            dnl The -rpath options are cumulative.
            for dir in $rpathdirs; do
              acl_save_libdir="$libdir"
              libdir="$dir"
              eval flag=\"$acl_hardcode_libdir_flag_spec\"
              libdir="$acl_save_libdir"
              $1="${$1}${$1:+ }$flag"
            done
          fi
        fi
      fi
    fi
  fi
  AC_SUBST([$1])
])

# lib-prefix.m4 serial 6 (gettext-0.18)
dnl Copyright (C) 2001-2005, 2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

dnl AC_LIB_ARG_WITH is synonymous to AC_ARG_WITH in autoconf-2.13, and
dnl similar to AC_ARG_WITH in autoconf 2.52...2.57 except that is doesn't
dnl require excessive bracketing.
ifdef([AC_HELP_STRING],
[AC_DEFUN([AC_LIB_ARG_WITH], [AC_ARG_WITH([$1],[[$2]],[$3],[$4])])],
[AC_DEFUN([AC_][LIB_ARG_WITH], [AC_ARG_WITH([$1],[$2],[$3],[$4])])])

dnl AC_LIB_PREFIX adds to the CPPFLAGS and LDFLAGS the flags that are needed
dnl to access previously installed libraries. The basic assumption is that
dnl a user will want packages to use other packages he previously installed
dnl with the same --prefix option.
dnl This macro is not needed if only AC_LIB_LINKFLAGS is used to locate
dnl libraries, but is otherwise very convenient.
AC_DEFUN([AC_LIB_PREFIX],
[
  AC_BEFORE([$0], [AC_LIB_LINKFLAGS])
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([AC_LIB_PREPARE_MULTILIB])
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  dnl By default, look in $includedir and $libdir.
  use_additional=yes
  AC_LIB_WITH_FINAL_PREFIX([
    eval additional_includedir=\"$includedir\"
    eval additional_libdir=\"$libdir\"
  ])
  AC_LIB_ARG_WITH([lib-prefix],
[  --with-lib-prefix[=DIR] search for libraries in DIR/include and DIR/lib
  --without-lib-prefix    don't search for libraries in includedir and libdir],
[
    if test "X$withval" = "Xno"; then
      use_additional=no
    else
      if test "X$withval" = "X"; then
        AC_LIB_WITH_FINAL_PREFIX([
          eval additional_includedir=\"$includedir\"
          eval additional_libdir=\"$libdir\"
        ])
      else
        additional_includedir="$withval/include"
        additional_libdir="$withval/$acl_libdirstem"
      fi
    fi
])
  if test $use_additional = yes; then
    dnl Potentially add $additional_includedir to $CPPFLAGS.
    dnl But don't add it
    dnl   1. if it's the standard /usr/include,
    dnl   2. if it's already present in $CPPFLAGS,
    dnl   3. if it's /usr/local/include and we are using GCC on Linux,
    dnl   4. if it doesn't exist as a directory.
    if test "X$additional_includedir" != "X/usr/include"; then
      haveit=
      for x in $CPPFLAGS; do
        AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
        if test "X$x" = "X-I$additional_includedir"; then
          haveit=yes
          break
        fi
      done
      if test -z "$haveit"; then
        if test "X$additional_includedir" = "X/usr/local/include"; then
          if test -n "$GCC"; then
            case $host_os in
              linux* | gnu* | k*bsd*-gnu) haveit=yes;;
            esac
          fi
        fi
        if test -z "$haveit"; then
          if test -d "$additional_includedir"; then
            dnl Really add $additional_includedir to $CPPFLAGS.
            CPPFLAGS="${CPPFLAGS}${CPPFLAGS:+ }-I$additional_includedir"
          fi
        fi
      fi
    fi
    dnl Potentially add $additional_libdir to $LDFLAGS.
    dnl But don't add it
    dnl   1. if it's the standard /usr/lib,
    dnl   2. if it's already present in $LDFLAGS,
    dnl   3. if it's /usr/local/lib and we are using GCC on Linux,
    dnl   4. if it doesn't exist as a directory.
    if test "X$additional_libdir" != "X/usr/$acl_libdirstem"; then
      haveit=
      for x in $LDFLAGS; do
        AC_LIB_WITH_FINAL_PREFIX([eval x=\"$x\"])
        if test "X$x" = "X-L$additional_libdir"; then
          haveit=yes
          break
        fi
      done
      if test -z "$haveit"; then
        if test "X$additional_libdir" = "X/usr/local/$acl_libdirstem"; then
          if test -n "$GCC"; then
            case $host_os in
              linux*) haveit=yes;;
            esac
          fi
        fi
        if test -z "$haveit"; then
          if test -d "$additional_libdir"; then
            dnl Really add $additional_libdir to $LDFLAGS.
            LDFLAGS="${LDFLAGS}${LDFLAGS:+ }-L$additional_libdir"
          fi
        fi
      fi
    fi
  fi
])

dnl AC_LIB_PREPARE_PREFIX creates variables acl_final_prefix,
dnl acl_final_exec_prefix, containing the values to which $prefix and
dnl $exec_prefix will expand at the end of the configure script.
AC_DEFUN([AC_LIB_PREPARE_PREFIX],
[
  dnl Unfortunately, prefix and exec_prefix get only finally determined
  dnl at the end of configure.
  if test "X$prefix" = "XNONE"; then
    acl_final_prefix="$ac_default_prefix"
  else
    acl_final_prefix="$prefix"
  fi
  if test "X$exec_prefix" = "XNONE"; then
    acl_final_exec_prefix='${prefix}'
  else
    acl_final_exec_prefix="$exec_prefix"
  fi
  acl_save_prefix="$prefix"
  prefix="$acl_final_prefix"
  eval acl_final_exec_prefix=\"$acl_final_exec_prefix\"
  prefix="$acl_save_prefix"
])

dnl AC_LIB_WITH_FINAL_PREFIX([statement]) evaluates statement, with the
dnl variables prefix and exec_prefix bound to the values they will have
dnl at the end of the configure script.
AC_DEFUN([AC_LIB_WITH_FINAL_PREFIX],
[
  acl_save_prefix="$prefix"
  prefix="$acl_final_prefix"
  acl_save_exec_prefix="$exec_prefix"
  exec_prefix="$acl_final_exec_prefix"
  $1
  exec_prefix="$acl_save_exec_prefix"
  prefix="$acl_save_prefix"
])

dnl AC_LIB_PREPARE_MULTILIB creates
dnl - a variable acl_libdirstem, containing the basename of the libdir, either
dnl   "lib" or "lib64" or "lib/64",
dnl - a variable acl_libdirstem2, as a secondary possible value for
dnl   acl_libdirstem, either the same as acl_libdirstem or "lib/sparcv9" or
dnl   "lib/amd64".
AC_DEFUN([AC_LIB_PREPARE_MULTILIB],
[
  dnl There is no formal standard regarding lib and lib64.
  dnl On glibc systems, the current practice is that on a system supporting
  dnl 32-bit and 64-bit instruction sets or ABIs, 64-bit libraries go under
  dnl $prefix/lib64 and 32-bit libraries go under $prefix/lib. We determine
  dnl the compiler's default mode by looking at the compiler's library search
  dnl path. If at least one of its elements ends in /lib64 or points to a
  dnl directory whose absolute pathname ends in /lib64, we assume a 64-bit ABI.
  dnl Otherwise we use the default, namely "lib".
  dnl On Solaris systems, the current practice is that on a system supporting
  dnl 32-bit and 64-bit instruction sets or ABIs, 64-bit libraries go under
  dnl $prefix/lib/64 (which is a symlink to either $prefix/lib/sparcv9 or
  dnl $prefix/lib/amd64) and 32-bit libraries go under $prefix/lib.
  AC_REQUIRE([AC_CANONICAL_HOST])
  acl_libdirstem=lib
  acl_libdirstem2=
  case "$host_os" in
    solaris*)
      dnl See Solaris 10 Software Developer Collection > Solaris 64-bit Developer's Guide > The Development Environment
      dnl <http://docs.sun.com/app/docs/doc/816-5138/dev-env?l=en&a=view>.
      dnl "Portable Makefiles should refer to any library directories using the 64 symbolic link."
      dnl But we want to recognize the sparcv9 or amd64 subdirectory also if the
      dnl symlink is missing, so we set acl_libdirstem2 too.
      AC_CACHE_CHECK([for 64-bit host], [gl_cv_solaris_64bit],
        [AC_EGREP_CPP([sixtyfour bits], [
#ifdef _LP64
sixtyfour bits
#endif
           ], [gl_cv_solaris_64bit=yes], [gl_cv_solaris_64bit=no])
        ])
      if test $gl_cv_solaris_64bit = yes; then
        acl_libdirstem=lib/64
        case "$host_cpu" in
          sparc*)        acl_libdirstem2=lib/sparcv9 ;;
          i*86 | x86_64) acl_libdirstem2=lib/amd64 ;;
        esac
      fi
      ;;
    *)
      searchpath=`(LC_ALL=C $CC -print-search-dirs) 2>/dev/null | sed -n -e 's,^libraries: ,,p' | sed -e 's,^=,,'`
      if test -n "$searchpath"; then
        acl_save_IFS="${IFS= 	}"; IFS=":"
        for searchdir in $searchpath; do
          if test -d "$searchdir"; then
            case "$searchdir" in
              */lib64/ | */lib64 ) acl_libdirstem=lib64 ;;
              *) searchdir=`cd "$searchdir" && pwd`
                 case "$searchdir" in
                   */lib64 ) acl_libdirstem=lib64 ;;
                 esac ;;
            esac
          fi
        done
        IFS="$acl_save_IFS"
      fi
      ;;
  esac
  test -n "$acl_libdirstem2" || acl_libdirstem2="$acl_libdirstem"
])

# libsigsegv.m4 serial 3
dnl Copyright (C) 2002-2003, 2008, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible, Sam Steingold.

AC_DEFUN([gl_LIBSIGSEGV],
[
  dnl Prerequisites of AC_LIB_LINKFLAGS_BODY.
  AC_REQUIRE([AC_LIB_PREPARE_PREFIX])
  AC_REQUIRE([AC_LIB_RPATH])

  dnl Search for libsigsegv and define LIBSIGSEGV, LTLIBSIGSEGV and INCSIGSEGV
  dnl accordingly.
  AC_LIB_LINKFLAGS_BODY([sigsegv])

  dnl Add $INCSIGSEGV to CPPFLAGS before performing the following checks,
  dnl because if the user has installed libsigsegv and not disabled its use
  dnl via --without-libsigsegv-prefix, he wants to use it.
  gl_save_CPPFLAGS="$CPPFLAGS"
  AC_LIB_APPENDTOVAR([CPPFLAGS], [$INCSIGSEGV])

  AC_CACHE_CHECK([for libsigsegv], [gl_cv_lib_sigsegv], [
    gl_cv_lib_sigsegv="no, consider installing GNU libsigsegv"
    gl_save_LIBS="$LIBS"
    LIBS="$LIBS $LIBSIGSEGV"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <sigsegv.h>]],
      [sigsegv_deinstall_handler();])],
      [gl_cv_lib_sigsegv=yes])
    LIBS="$gl_save_LIBS"
  ])
  if test "$gl_cv_lib_sigsegv" = yes; then
    AC_DEFINE([HAVE_LIBSIGSEGV], [1],
      [Define if you have the libsigsegv library.])
    AC_MSG_CHECKING([how to link with libsigsegv])
    AC_MSG_RESULT([$LIBSIGSEGV])
  else
    dnl If $LIBSIGSEGV didn't lead to a usable library, we don't need
    dnl $INCSIGSEGV either.
    CPPFLAGS="$gl_save_CPPFLAGS"
    LIBSIGSEGV=
    LTLIBSIGSEGV=
  fi
  AC_SUBST([LIBSIGSEGV])
  AC_SUBST([LTLIBSIGSEGV])
])

# serial 10
dnl Run a program to determine whether link(2) follows symlinks.
dnl Set LINK_FOLLOWS_SYMLINKS accordingly.

# Copyright (C) 1999-2001, 2004-2006, 2009 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_AC_FUNC_LINK_FOLLOWS_SYMLINK],
[dnl
  AC_CACHE_CHECK(
    [whether link(2) dereferences a symlink specified with a trailing slash],
		 gl_ac_cv_func_link_follows_symlink,
  [
    # Create a regular file.
    echo > conftest.file
    AC_TRY_RUN(
      [
#       include <sys/types.h>
#       include <sys/stat.h>
#       include <unistd.h>
#       include <stdlib.h>

#       define SAME_INODE(Stat_buf_1, Stat_buf_2) \
	  ((Stat_buf_1).st_ino == (Stat_buf_2).st_ino \
	   && (Stat_buf_1).st_dev == (Stat_buf_2).st_dev)

	int
	main ()
	{
	  const char *file = "conftest.file";
	  const char *sym = "conftest.sym";
	  const char *hard = "conftest.hard";
	  struct stat sb_file, sb_hard;

	  /* Create a symlink to the regular file. */
	  if (symlink (file, sym))
	    abort ();

	  /* Create a hard link to that symlink.  */
	  if (link (sym, hard))
	    abort ();

	  if (lstat (hard, &sb_hard))
	    abort ();
	  if (lstat (file, &sb_file))
	    abort ();

	  /* If the dev/inode of hard and file are the same, then
	     the link call followed the symlink.  */
	  return SAME_INODE (sb_hard, sb_file) ? 0 : 1;
	}
      ],
      gl_ac_cv_func_link_follows_symlink=yes,
      gl_ac_cv_func_link_follows_symlink=no,
      gl_ac_cv_func_link_follows_symlink=yes dnl We're cross compiling.
    )
  ])
  if test $gl_ac_cv_func_link_follows_symlink = yes; then
    AC_DEFINE([LINK_FOLLOWS_SYMLINKS], [1],
      [Define if `link(2)' dereferences symbolic links.])
  fi
])

# localcharset.m4 serial 6
dnl Copyright (C) 2002, 2004, 2006, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_LOCALCHARSET],
[
  dnl Prerequisites of lib/localcharset.c.
  AC_REQUIRE([AM_LANGINFO_CODESET])
  AC_CHECK_DECLS_ONCE([getc_unlocked])

  dnl Prerequisites of the lib/Makefile.am snippet.
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_GLIBC21])
])

# locale-fr.m4 serial 10
dnl Copyright (C) 2003, 2005-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

dnl Determine the name of a french locale with traditional encoding.
AC_DEFUN([gt_LOCALE_FR],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([AM_LANGINFO_CODESET])
  AC_CACHE_CHECK([for a traditional french locale], [gt_cv_locale_fr], [
    macosx=
changequote(,)dnl
    case "$host_os" in
      darwin[56]*) ;;
      darwin*) macosx=yes;;
    esac
changequote([,])dnl
    if test -n "$macosx"; then
      # On Darwin 7 (MacOS X), the libc supports some locales in non-UTF-8
      # encodings, but the kernel does not support them. The documentation
      # says:
      #   "... all code that calls BSD system routines should ensure
      #    that the const *char parameters of these routines are in UTF-8
      #    encoding. All BSD system functions expect their string
      #    parameters to be in UTF-8 encoding and nothing else."
      # See the comments in config.charset. Therefore we bypass the test.
      gt_cv_locale_fr=none
    else
      AC_LANG_CONFTEST([AC_LANG_SOURCE([
changequote(,)dnl
#include <locale.h>
#include <time.h>
#if HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif
#include <stdlib.h>
#include <string.h>
struct tm t;
char buf[16];
int main () {
  /* Check whether the given locale name is recognized by the system.  */
  if (setlocale (LC_ALL, "") == NULL) return 1;
  /* Check whether nl_langinfo(CODESET) is nonempty and not "ASCII" or "646".
     On MacOS X 10.3.5 (Darwin 7.5) in the fr_FR locale, nl_langinfo(CODESET)
     is empty, and the behaviour of Tcl 8.4 in this locale is not useful.
     On OpenBSD 4.0, when an unsupported locale is specified, setlocale()
     succeeds but then nl_langinfo(CODESET) is "646". In this situation,
     some unit tests fail.  */
#if HAVE_LANGINFO_CODESET
  {
    const char *cs = nl_langinfo (CODESET);
    if (cs[0] == '\0' || strcmp (cs, "ASCII") == 0 || strcmp (cs, "646") == 0)
      return 1;
  }
#endif
#ifdef __CYGWIN__
  /* On Cygwin, avoid locale names without encoding suffix, because the
     locale_charset() function relies on the encoding suffix.  Note that
     LC_ALL is set on the command line.  */
  if (strchr (getenv ("LC_ALL"), '.') == NULL) return 1;
#endif
  /* Check whether in the abbreviation of the second month, the second
     character (should be U+00E9: LATIN SMALL LETTER E WITH ACUTE) is only
     one byte long. This excludes the UTF-8 encoding.  */
  t.tm_year = 1975 - 1900; t.tm_mon = 2 - 1; t.tm_mday = 4;
  if (strftime (buf, sizeof (buf), "%b", &t) < 3 || buf[2] != 'v') return 1;
  /* Check whether the decimal separator is a comma.
     On NetBSD 3.0 in the fr_FR.ISO8859-1 locale, localeconv()->decimal_point
     are nl_langinfo(RADIXCHAR) are both ".".  */
  if (localeconv () ->decimal_point[0] != ',') return 1;
  return 0;
}
changequote([,])dnl
        ])])
      if AC_TRY_EVAL([ac_link]) && test -s conftest$ac_exeext; then
        # Setting LC_ALL is not enough. Need to set LC_TIME to empty, because
        # otherwise on MacOS X 10.3.5 the LC_TIME=C from the beginning of the
        # configure script would override the LC_ALL setting. Likewise for
        # LC_CTYPE, which is also set at the beginning of the configure script.
        # Test for the usual locale name.
        if (LC_ALL=fr_FR LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
          gt_cv_locale_fr=fr_FR
        else
          # Test for the locale name with explicit encoding suffix.
          if (LC_ALL=fr_FR.ISO-8859-1 LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
            gt_cv_locale_fr=fr_FR.ISO-8859-1
          else
            # Test for the AIX, OSF/1, FreeBSD, NetBSD, OpenBSD locale name.
            if (LC_ALL=fr_FR.ISO8859-1 LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
              gt_cv_locale_fr=fr_FR.ISO8859-1
            else
              # Test for the HP-UX locale name.
              if (LC_ALL=fr_FR.iso88591 LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
                gt_cv_locale_fr=fr_FR.iso88591
              else
                # Test for the Solaris 7 locale name.
                if (LC_ALL=fr LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
                  gt_cv_locale_fr=fr
                else
                  # None found.
                  gt_cv_locale_fr=none
                fi
              fi
            fi
          fi
        fi
      fi
      rm -fr conftest*
    fi
  ])
  LOCALE_FR=$gt_cv_locale_fr
  AC_SUBST([LOCALE_FR])
])

dnl Determine the name of a french locale with UTF-8 encoding.
AC_DEFUN([gt_LOCALE_FR_UTF8],
[
  AC_REQUIRE([AM_LANGINFO_CODESET])
  AC_CACHE_CHECK([for a french Unicode locale], [gt_cv_locale_fr_utf8], [
    AC_LANG_CONFTEST([AC_LANG_SOURCE([
changequote(,)dnl
#include <locale.h>
#include <time.h>
#if HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif
#include <stdlib.h>
#include <string.h>
struct tm t;
char buf[16];
int main () {
  /* On BeOS and Haiku, locales are not implemented in libc.  Rather, libintl
     imitates locale dependent behaviour by looking at the environment
     variables, and all locales use the UTF-8 encoding.  */
#if !(defined __BEOS__ || defined __HAIKU__)
  /* Check whether the given locale name is recognized by the system.  */
  if (setlocale (LC_ALL, "") == NULL) return 1;
  /* Check whether nl_langinfo(CODESET) is nonempty and not "ASCII" or "646".
     On MacOS X 10.3.5 (Darwin 7.5) in the fr_FR locale, nl_langinfo(CODESET)
     is empty, and the behaviour of Tcl 8.4 in this locale is not useful.
     On OpenBSD 4.0, when an unsupported locale is specified, setlocale()
     succeeds but then nl_langinfo(CODESET) is "646". In this situation,
     some unit tests fail.  */
# if HAVE_LANGINFO_CODESET
  {
    const char *cs = nl_langinfo (CODESET);
    if (cs[0] == '\0' || strcmp (cs, "ASCII") == 0 || strcmp (cs, "646") == 0)
      return 1;
  }
# endif
# ifdef __CYGWIN__
  /* On Cygwin, avoid locale names without encoding suffix, because the
     locale_charset() function relies on the encoding suffix.  Note that
     LC_ALL is set on the command line.  */
  if (strchr (getenv ("LC_ALL"), '.') == NULL) return 1;
# endif
  /* Check whether in the abbreviation of the second month, the second
     character (should be U+00E9: LATIN SMALL LETTER E WITH ACUTE) is
     two bytes long, with UTF-8 encoding.  */
  t.tm_year = 1975 - 1900; t.tm_mon = 2 - 1; t.tm_mday = 4;
  if (strftime (buf, sizeof (buf), "%b", &t) < 4
      || buf[1] != (char) 0xc3 || buf[2] != (char) 0xa9 || buf[3] != 'v')
    return 1;
#endif
  /* Check whether the decimal separator is a comma.
     On NetBSD 3.0 in the fr_FR.ISO8859-1 locale, localeconv()->decimal_point
     are nl_langinfo(RADIXCHAR) are both ".".  */
  if (localeconv () ->decimal_point[0] != ',') return 1;
  return 0;
}
changequote([,])dnl
      ])])
    if AC_TRY_EVAL([ac_link]) && test -s conftest$ac_exeext; then
      # Setting LC_ALL is not enough. Need to set LC_TIME to empty, because
      # otherwise on MacOS X 10.3.5 the LC_TIME=C from the beginning of the
      # configure script would override the LC_ALL setting. Likewise for
      # LC_CTYPE, which is also set at the beginning of the configure script.
      # Test for the usual locale name.
      if (LC_ALL=fr_FR LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
        gt_cv_locale_fr_utf8=fr_FR
      else
        # Test for the locale name with explicit encoding suffix.
        if (LC_ALL=fr_FR.UTF-8 LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
          gt_cv_locale_fr_utf8=fr_FR.UTF-8
        else
          # Test for the Solaris 7 locale name.
          if (LC_ALL=fr.UTF-8 LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
            gt_cv_locale_fr_utf8=fr.UTF-8
          else
            # None found.
            gt_cv_locale_fr_utf8=none
          fi
        fi
      fi
    fi
    rm -fr conftest*
  ])
  LOCALE_FR_UTF8=$gt_cv_locale_fr_utf8
  AC_SUBST([LOCALE_FR_UTF8])
])

# locale-ja.m4 serial 6
dnl Copyright (C) 2003, 2005-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

dnl Determine the name of a japanese locale with EUC-JP encoding.
AC_DEFUN([gt_LOCALE_JA],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([AM_LANGINFO_CODESET])
  AC_CACHE_CHECK([for a traditional japanese locale], [gt_cv_locale_ja], [
    macosx=
changequote(,)dnl
    case "$host_os" in
      darwin[56]*) ;;
      darwin*) macosx=yes;;
    esac
changequote([,])dnl
    if test -n "$macosx"; then
      # On Darwin 7 (MacOS X), the libc supports some locales in non-UTF-8
      # encodings, but the kernel does not support them. The documentation
      # says:
      #   "... all code that calls BSD system routines should ensure
      #    that the const *char parameters of these routines are in UTF-8
      #    encoding. All BSD system functions expect their string
      #    parameters to be in UTF-8 encoding and nothing else."
      # See the comments in config.charset. Therefore we bypass the test.
      gt_cv_locale_ja=none
    else
      AC_LANG_CONFTEST([AC_LANG_SOURCE([
changequote(,)dnl
#include <locale.h>
#include <time.h>
#if HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif
#include <stdlib.h>
#include <string.h>
struct tm t;
char buf[16];
int main ()
{
  const char *p;
  /* Check whether the given locale name is recognized by the system.  */
  if (setlocale (LC_ALL, "") == NULL) return 1;
  /* Check whether nl_langinfo(CODESET) is nonempty and not "ASCII" or "646".
     On MacOS X 10.3.5 (Darwin 7.5) in the fr_FR locale, nl_langinfo(CODESET)
     is empty, and the behaviour of Tcl 8.4 in this locale is not useful.
     On OpenBSD 4.0, when an unsupported locale is specified, setlocale()
     succeeds but then nl_langinfo(CODESET) is "646". In this situation,
     some unit tests fail.  */
#if HAVE_LANGINFO_CODESET
  {
    const char *cs = nl_langinfo (CODESET);
    if (cs[0] == '\0' || strcmp (cs, "ASCII") == 0 || strcmp (cs, "646") == 0)
      return 1;
  }
#endif
#ifdef __CYGWIN__
  /* On Cygwin, avoid locale names without encoding suffix, because the
     locale_charset() function relies on the encoding suffix.  Note that
     LC_ALL is set on the command line.  */
  if (strchr (getenv ("LC_ALL"), '.') == NULL) return 1;
#endif
  /* Check whether MB_CUR_MAX is > 1.  This excludes the dysfunctional locales
     on Cygwin 1.5.x.  */
  if (MB_CUR_MAX == 1)
    return 1;
  /* Check whether in a month name, no byte in the range 0x80..0x9F occurs.
     This excludes the UTF-8 encoding.  */
  t.tm_year = 1975 - 1900; t.tm_mon = 2 - 1; t.tm_mday = 4;
  if (strftime (buf, sizeof (buf), "%B", &t) < 2) return 1;
  for (p = buf; *p != '\0'; p++)
    if ((unsigned char) *p >= 0x80 && (unsigned char) *p < 0xa0)
      return 1;
  return 0;
}
changequote([,])dnl
        ])])
      if AC_TRY_EVAL([ac_link]) && test -s conftest$ac_exeext; then
        # Setting LC_ALL is not enough. Need to set LC_TIME to empty, because
        # otherwise on MacOS X 10.3.5 the LC_TIME=C from the beginning of the
        # configure script would override the LC_ALL setting. Likewise for
        # LC_CTYPE, which is also set at the beginning of the configure script.
        # Test for the AIX locale name.
        if (LC_ALL=ja_JP LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
          gt_cv_locale_ja=ja_JP
        else
          # Test for the locale name with explicit encoding suffix.
          if (LC_ALL=ja_JP.EUC-JP LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
            gt_cv_locale_ja=ja_JP.EUC-JP
          else
            # Test for the HP-UX, OSF/1, NetBSD locale name.
            if (LC_ALL=ja_JP.eucJP LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
              gt_cv_locale_ja=ja_JP.eucJP
            else
              # Test for the IRIX, FreeBSD locale name.
              if (LC_ALL=ja_JP.EUC LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
                gt_cv_locale_ja=ja_JP.EUC
              else
                # Test for the Solaris 7 locale name.
                if (LC_ALL=ja LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
                  gt_cv_locale_ja=ja
                else
                  # Special test for NetBSD 1.6.
                  if test -f /usr/share/locale/ja_JP.eucJP/LC_CTYPE; then
                    gt_cv_locale_ja=ja_JP.eucJP
                  else
                    # None found.
                    gt_cv_locale_ja=none
                  fi
                fi
              fi
            fi
          fi
        fi
      fi
      rm -fr conftest*
    fi
  ])
  LOCALE_JA=$gt_cv_locale_ja
  AC_SUBST([LOCALE_JA])
])

# locale-zh.m4 serial 5
dnl Copyright (C) 2003, 2005-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

dnl Determine the name of a chinese locale with GB18030 encoding.
AC_DEFUN([gt_LOCALE_ZH_CN],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([AM_LANGINFO_CODESET])
  AC_CACHE_CHECK([for a transitional chinese locale], [gt_cv_locale_zh_CN], [
    macosx=
changequote(,)dnl
    case "$host_os" in
      darwin[56]*) ;;
      darwin*) macosx=yes;;
    esac
changequote([,])dnl
    if test -n "$macosx"; then
      # On Darwin 7 (MacOS X), the libc supports some locales in non-UTF-8
      # encodings, but the kernel does not support them. The documentation
      # says:
      #   "... all code that calls BSD system routines should ensure
      #    that the const *char parameters of these routines are in UTF-8
      #    encoding. All BSD system functions expect their string
      #    parameters to be in UTF-8 encoding and nothing else."
      # See the comments in config.charset. Therefore we bypass the test.
      gt_cv_locale_zh_CN=none
    else
      AC_LANG_CONFTEST([AC_LANG_SOURCE([
changequote(,)dnl
#include <locale.h>
#include <stdlib.h>
#include <time.h>
#if HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif
#include <stdlib.h>
#include <string.h>
struct tm t;
char buf[16];
int main ()
{
  const char *p;
  /* Check whether the given locale name is recognized by the system.  */
  if (setlocale (LC_ALL, "") == NULL) return 1;
  /* Check whether nl_langinfo(CODESET) is nonempty and not "ASCII" or "646".
     On MacOS X 10.3.5 (Darwin 7.5) in the fr_FR locale, nl_langinfo(CODESET)
     is empty, and the behaviour of Tcl 8.4 in this locale is not useful.
     On OpenBSD 4.0, when an unsupported locale is specified, setlocale()
     succeeds but then nl_langinfo(CODESET) is "646". In this situation,
     some unit tests fail.  */
#if HAVE_LANGINFO_CODESET
  {
    const char *cs = nl_langinfo (CODESET);
    if (cs[0] == '\0' || strcmp (cs, "ASCII") == 0 || strcmp (cs, "646") == 0)
      return 1;
  }
#endif
#ifdef __CYGWIN__
  /* On Cygwin, avoid locale names without encoding suffix, because the
     locale_charset() function relies on the encoding suffix.  Note that
     LC_ALL is set on the command line.  */
  if (strchr (getenv ("LC_ALL"), '.') == NULL) return 1;
#endif
  /* Check whether in a month name, no byte in the range 0x80..0x9F occurs.
     This excludes the UTF-8 encoding.  */
  t.tm_year = 1975 - 1900; t.tm_mon = 2 - 1; t.tm_mday = 4;
  if (strftime (buf, sizeof (buf), "%B", &t) < 2) return 1;
  for (p = buf; *p != '\0'; p++)
    if ((unsigned char) *p >= 0x80 && (unsigned char) *p < 0xa0)
      return 1;
  /* Check whether a typical GB18030 multibyte sequence is recognized as a
     single wide character.  This excludes the GB2312 and GBK encodings.  */
  if (mblen ("\203\062\332\066", 5) != 4)
    return 1;
  return 0;
}
changequote([,])dnl
        ])])
      if AC_TRY_EVAL([ac_link]) && test -s conftest$ac_exeext; then
        # Setting LC_ALL is not enough. Need to set LC_TIME to empty, because
        # otherwise on MacOS X 10.3.5 the LC_TIME=C from the beginning of the
        # configure script would override the LC_ALL setting. Likewise for
        # LC_CTYPE, which is also set at the beginning of the configure script.
        # Test for the locale name without encoding suffix.
        if (LC_ALL=zh_CN LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
          gt_cv_locale_zh_CN=zh_CN
        else
          # Test for the locale name with explicit encoding suffix.
          if (LC_ALL=zh_CN.GB18030 LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
            gt_cv_locale_zh_CN=zh_CN.GB18030
          else
            # None found.
            gt_cv_locale_zh_CN=none
          fi
        fi
      else
        # If there was a link error, due to mblen(), the system is so old that
        # it certainly doesn't have a chinese locale.
        gt_cv_locale_zh_CN=none
      fi
      rm -fr conftest*
    fi
  ])
  LOCALE_ZH_CN=$gt_cv_locale_zh_CN
  AC_SUBST([LOCALE_ZH_CN])
])

# longlong.m4 serial 14
dnl Copyright (C) 1999-2007, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Paul Eggert.

# Define HAVE_LONG_LONG_INT if 'long long int' works.
# This fixes a bug in Autoconf 2.61, but can be removed once we
# assume 2.62 everywhere.

# Note: If the type 'long long int' exists but is only 32 bits large
# (as on some very old compilers), HAVE_LONG_LONG_INT will not be
# defined. In this case you can treat 'long long int' like 'long int'.

AC_DEFUN([AC_TYPE_LONG_LONG_INT],
[
  AC_CACHE_CHECK([for long long int], [ac_cv_type_long_long_int],
    [AC_LINK_IFELSE(
       [_AC_TYPE_LONG_LONG_SNIPPET],
       [dnl This catches a bug in Tandem NonStop Kernel (OSS) cc -O circa 2004.
	dnl If cross compiling, assume the bug isn't important, since
	dnl nobody cross compiles for this platform as far as we know.
	AC_RUN_IFELSE(
	  [AC_LANG_PROGRAM(
	     [[@%:@include <limits.h>
	       @%:@ifndef LLONG_MAX
	       @%:@ define HALF \
			(1LL << (sizeof (long long int) * CHAR_BIT - 2))
	       @%:@ define LLONG_MAX (HALF - 1 + HALF)
	       @%:@endif]],
	     [[long long int n = 1;
	       int i;
	       for (i = 0; ; i++)
		 {
		   long long int m = n << i;
		   if (m >> i != n)
		     return 1;
		   if (LLONG_MAX / 2 < m)
		     break;
		 }
	       return 0;]])],
	  [ac_cv_type_long_long_int=yes],
	  [ac_cv_type_long_long_int=no],
	  [ac_cv_type_long_long_int=yes])],
       [ac_cv_type_long_long_int=no])])
  if test $ac_cv_type_long_long_int = yes; then
    AC_DEFINE([HAVE_LONG_LONG_INT], [1],
      [Define to 1 if the system has the type `long long int'.])
  fi
])

# Define HAVE_UNSIGNED_LONG_LONG_INT if 'unsigned long long int' works.
# This fixes a bug in Autoconf 2.61, but can be removed once we
# assume 2.62 everywhere.

# Note: If the type 'unsigned long long int' exists but is only 32 bits
# large (as on some very old compilers), AC_TYPE_UNSIGNED_LONG_LONG_INT
# will not be defined. In this case you can treat 'unsigned long long int'
# like 'unsigned long int'.

AC_DEFUN([AC_TYPE_UNSIGNED_LONG_LONG_INT],
[
  AC_CACHE_CHECK([for unsigned long long int],
    [ac_cv_type_unsigned_long_long_int],
    [AC_LINK_IFELSE(
       [_AC_TYPE_LONG_LONG_SNIPPET],
       [ac_cv_type_unsigned_long_long_int=yes],
       [ac_cv_type_unsigned_long_long_int=no])])
  if test $ac_cv_type_unsigned_long_long_int = yes; then
    AC_DEFINE([HAVE_UNSIGNED_LONG_LONG_INT], [1],
      [Define to 1 if the system has the type `unsigned long long int'.])
  fi
])

# Expands to a C program that can be used to test for simultaneous support
# of 'long long' and 'unsigned long long'. We don't want to say that
# 'long long' is available if 'unsigned long long' is not, or vice versa,
# because too many programs rely on the symmetry between signed and unsigned
# integer types (excluding 'bool').
AC_DEFUN([_AC_TYPE_LONG_LONG_SNIPPET],
[
  AC_LANG_PROGRAM(
    [[/* For now, do not test the preprocessor; as of 2007 there are too many
	 implementations with broken preprocessors.  Perhaps this can
	 be revisited in 2012.  In the meantime, code should not expect
	 #if to work with literals wider than 32 bits.  */
      /* Test literals.  */
      long long int ll = 9223372036854775807ll;
      long long int nll = -9223372036854775807LL;
      unsigned long long int ull = 18446744073709551615ULL;
      /* Test constant expressions.   */
      typedef int a[((-9223372036854775807LL < 0 && 0 < 9223372036854775807ll)
		     ? 1 : -1)];
      typedef int b[(18446744073709551615ULL <= (unsigned long long int) -1
		     ? 1 : -1)];
      int i = 63;]],
    [[/* Test availability of runtime routines for shift and division.  */
      long long int llmax = 9223372036854775807ll;
      unsigned long long int ullmax = 18446744073709551615ull;
      return ((ll << 63) | (ll >> 63) | (ll < i) | (ll > i)
	      | (llmax / ll) | (llmax % ll)
	      | (ull << 63) | (ull >> 63) | (ull << i) | (ull >> i)
	      | (ullmax / ull) | (ullmax % ull));]])
])

# malloc.m4 serial 9
dnl Copyright (C) 2007, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# gl_FUNC_MALLOC_POSIX
# --------------------
# Test whether 'malloc' is POSIX compliant (sets errno to ENOMEM when it
# fails), and replace malloc if it is not.
AC_DEFUN([gl_FUNC_MALLOC_POSIX],
[
  AC_REQUIRE([gl_CHECK_MALLOC_POSIX])
  if test $gl_cv_func_malloc_posix = yes; then
    HAVE_MALLOC_POSIX=1
    AC_DEFINE([HAVE_MALLOC_POSIX], [1],
      [Define if the 'malloc' function is POSIX compliant.])
  else
    AC_LIBOBJ([malloc])
    HAVE_MALLOC_POSIX=0
  fi
  AC_SUBST([HAVE_MALLOC_POSIX])
])

# Test whether malloc, realloc, calloc are POSIX compliant,
# Set gl_cv_func_malloc_posix to yes or no accordingly.
AC_DEFUN([gl_CHECK_MALLOC_POSIX],
[
  AC_CACHE_CHECK([whether malloc, realloc, calloc are POSIX compliant],
    [gl_cv_func_malloc_posix],
    [
      dnl It is too dangerous to try to allocate a large amount of memory:
      dnl some systems go to their knees when you do that. So assume that
      dnl all Unix implementations of the function are POSIX compliant.
      AC_TRY_COMPILE([],
        [#if (defined _WIN32 || defined __WIN32__) && ! defined __CYGWIN__
         choke me
         #endif
        ], [gl_cv_func_malloc_posix=yes], [gl_cv_func_malloc_posix=no])
    ])
])

# mbrtowc.m4 serial 13
dnl Copyright (C) 2001-2002, 2004-2005, 2008, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_MBRTOWC],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])

  AC_REQUIRE([AC_TYPE_MBSTATE_T])
  gl_MBSTATE_T_BROKEN
  if test $REPLACE_MBSTATE_T = 1; then
    REPLACE_MBRTOWC=1
  fi
  AC_CHECK_FUNCS_ONCE([mbrtowc])
  if test $ac_cv_func_mbrtowc = no; then
    HAVE_MBRTOWC=0
  fi
  if test $HAVE_MBRTOWC != 0 && test $REPLACE_MBRTOWC != 1; then
    gl_MBRTOWC_NULL_ARG
    gl_MBRTOWC_RETVAL
    gl_MBRTOWC_NUL_RETVAL
    case "$gl_cv_func_mbrtowc_null_arg" in
      *yes) ;;
      *) AC_DEFINE([MBRTOWC_NULL_ARG_BUG], [1],
           [Define if the mbrtowc function has the NULL string argument bug.])
         REPLACE_MBRTOWC=1
         ;;
    esac
    case "$gl_cv_func_mbrtowc_retval" in
      *yes) ;;
      *) AC_DEFINE([MBRTOWC_RETVAL_BUG], [1],
           [Define if the mbrtowc function returns a wrong return value.])
         REPLACE_MBRTOWC=1
         ;;
    esac
    case "$gl_cv_func_mbrtowc_nul_retval" in
      *yes) ;;
      *) AC_DEFINE([MBRTOWC_NUL_RETVAL_BUG], [1],
           [Define if the mbrtowc function does not return 0 for a NUL character.])
         REPLACE_MBRTOWC=1
         ;;
    esac
  fi
  if test $HAVE_MBRTOWC = 0 || test $REPLACE_MBRTOWC = 1; then
    gl_REPLACE_WCHAR_H
    AC_LIBOBJ([mbrtowc])
    gl_PREREQ_MBRTOWC
  fi
])

dnl Test whether mbsinit() and mbrtowc() need to be overridden in a way that
dnl redefines the semantics of the given mbstate_t type.
dnl Result is REPLACE_MBSTATE_T.
dnl When this is set to 1, we replace both mbsinit() and mbrtowc(), in order to
dnl avoid inconsistencies.

AC_DEFUN([gl_MBSTATE_T_BROKEN],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])

  AC_REQUIRE([AC_TYPE_MBSTATE_T])
  AC_CHECK_FUNCS_ONCE([mbsinit])
  AC_CHECK_FUNCS_ONCE([mbrtowc])
  if test $ac_cv_func_mbsinit = yes && test $ac_cv_func_mbrtowc = yes; then
    gl_MBRTOWC_INCOMPLETE_STATE
    case "$gl_cv_func_mbrtowc_incomplete_state" in
      *yes) REPLACE_MBSTATE_T=0 ;;
      *)    REPLACE_MBSTATE_T=1 ;;
    esac
  else
    REPLACE_MBSTATE_T=1
  fi
  if test $REPLACE_MBSTATE_T = 1; then
    gl_REPLACE_WCHAR_H
  fi
])

dnl Test whether mbrtowc puts the state into non-initial state when parsing an
dnl incomplete multibyte character.
dnl Result is gl_cv_func_mbrtowc_incomplete_state.

AC_DEFUN([gl_MBRTOWC_INCOMPLETE_STATE],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([gt_LOCALE_JA])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether mbrtowc handles incomplete characters],
    [gl_cv_func_mbrtowc_incomplete_state],
    [
      dnl Initial guess, used when cross-compiling or when no suitable locale
      dnl is present.
changequote(,)dnl
      case "$host_os" in
              # Guess no on AIX and OSF/1.
        osf*) gl_cv_func_mbrtowc_incomplete_state="guessing no" ;;
              # Guess yes otherwise.
        *)    gl_cv_func_mbrtowc_incomplete_state="guessing yes" ;;
      esac
changequote([,])dnl
      if test $LOCALE_JA != none; then
        AC_TRY_RUN([
#include <locale.h>
#include <string.h>
#include <wchar.h>
int main ()
{
  if (setlocale (LC_ALL, "$LOCALE_JA") != NULL)
    {
      const char input[] = "B\217\253\344\217\251\316er"; /* "Büßer" */
      mbstate_t state;
      wchar_t wc;

      memset (&state, '\0', sizeof (mbstate_t));
      if (mbrtowc (&wc, input + 1, 1, &state) == (size_t)(-2))
        if (mbsinit (&state))
          return 1;
    }
  return 0;
}],
          [gl_cv_func_mbrtowc_incomplete_state=yes],
          [gl_cv_func_mbrtowc_incomplete_state=no],
          [])
      fi
    ])
])

dnl Test whether mbrtowc supports a NULL string argument correctly.
dnl Result is gl_cv_func_mbrtowc_null_arg.

AC_DEFUN([gl_MBRTOWC_NULL_ARG],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([gt_LOCALE_FR_UTF8])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether mbrtowc handles a NULL string argument],
    [gl_cv_func_mbrtowc_null_arg],
    [
      dnl Initial guess, used when cross-compiling or when no suitable locale
      dnl is present.
changequote(,)dnl
      case "$host_os" in
              # Guess no on OSF/1.
        osf*) gl_cv_func_mbrtowc_null_arg="guessing no" ;;
              # Guess yes otherwise.
        *)    gl_cv_func_mbrtowc_null_arg="guessing yes" ;;
      esac
changequote([,])dnl
      if test $LOCALE_FR_UTF8 != none; then
        AC_TRY_RUN([
#include <locale.h>
#include <string.h>
#include <wchar.h>
int main ()
{
  if (setlocale (LC_ALL, "$LOCALE_FR_UTF8") != NULL)
    {
      mbstate_t state;
      wchar_t wc;
      int ret;

      memset (&state, '\0', sizeof (mbstate_t));
      wc = (wchar_t) 0xBADFACE;
      mbrtowc (&wc, NULL, 5, &state);
      /* Check that wc was not modified.  */
      if (wc != (wchar_t) 0xBADFACE)
        return 1;
    }
  return 0;
}], [gl_cv_func_mbrtowc_null_arg=yes], [gl_cv_func_mbrtowc_null_arg=no], [])
      fi
    ])
])

dnl Test whether mbrtowc, when parsing the end of a multibyte character,
dnl correctly returns the number of bytes that were needed to complete the
dnl character (not the total number of bytes of the multibyte character).
dnl Result is gl_cv_func_mbrtowc_retval.

AC_DEFUN([gl_MBRTOWC_RETVAL],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([gt_LOCALE_FR_UTF8])
  AC_REQUIRE([gt_LOCALE_JA])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether mbrtowc has a correct return value],
    [gl_cv_func_mbrtowc_retval],
    [
      dnl Initial guess, used when cross-compiling or when no suitable locale
      dnl is present.
changequote(,)dnl
      case "$host_os" in
                          # Guess no on HP-UX and Solaris.
        hpux* | solaris*) gl_cv_func_mbrtowc_retval="guessing no" ;;
                          # Guess yes otherwise.
        *)                gl_cv_func_mbrtowc_retval="guessing yes" ;;
      esac
changequote([,])dnl
      if test $LOCALE_FR_UTF8 != none || test $LOCALE_JA != none; then
        AC_TRY_RUN([
#include <locale.h>
#include <string.h>
#include <wchar.h>
int main ()
{
  /* This fails on Solaris.  */
  if (setlocale (LC_ALL, "$LOCALE_FR_UTF8") != NULL)
    {
      char input[] = "B\303\274\303\237er"; /* "Büßer" */
      mbstate_t state;
      wchar_t wc;

      memset (&state, '\0', sizeof (mbstate_t));
      if (mbrtowc (&wc, input + 1, 1, &state) == (size_t)(-2))
        {
          input[1] = '\0';
          if (mbrtowc (&wc, input + 2, 5, &state) != 1)
            return 1;
        }
    }
  /* This fails on HP-UX 11.11.  */
  if (setlocale (LC_ALL, "$LOCALE_JA") != NULL)
    {
      char input[] = "B\217\253\344\217\251\316er"; /* "Büßer" */
      mbstate_t state;
      wchar_t wc;

      memset (&state, '\0', sizeof (mbstate_t));
      if (mbrtowc (&wc, input + 1, 1, &state) == (size_t)(-2))
        {
          input[1] = '\0';
          if (mbrtowc (&wc, input + 2, 5, &state) != 2)
            return 1;
        }
    }
  return 0;
}],
          [gl_cv_func_mbrtowc_retval=yes],
          [gl_cv_func_mbrtowc_retval=no],
          [])
      fi
    ])
])

dnl Test whether mbrtowc, when parsing a NUL character, correctly returns 0.
dnl Result is gl_cv_func_mbrtowc_nul_retval.

AC_DEFUN([gl_MBRTOWC_NUL_RETVAL],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([gt_LOCALE_ZH_CN])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether mbrtowc returns 0 when parsing a NUL character],
    [gl_cv_func_mbrtowc_nul_retval],
    [
      dnl Initial guess, used when cross-compiling or when no suitable locale
      dnl is present.
changequote(,)dnl
      case "$host_os" in
                    # Guess no on Solaris 9.
        solaris2.9) gl_cv_func_mbrtowc_nul_retval="guessing no" ;;
                    # Guess yes otherwise.
        *)          gl_cv_func_mbrtowc_nul_retval="guessing yes" ;;
      esac
changequote([,])dnl
      if test $LOCALE_ZH_CN != none; then
        AC_TRY_RUN([
#include <locale.h>
#include <string.h>
#include <wchar.h>
int main ()
{
  /* This fails on Solaris 9.  */
  if (setlocale (LC_ALL, "$LOCALE_ZH_CN") != NULL)
    {
      mbstate_t state;
      wchar_t wc;

      memset (&state, '\0', sizeof (mbstate_t));
      if (mbrtowc (&wc, "", 1, &state) != 0)
        return 1;
    }
  return 0;
}],
          [gl_cv_func_mbrtowc_nul_retval=yes],
          [gl_cv_func_mbrtowc_nul_retval=no],
          [])
      fi
    ])
])

# Prerequisites of lib/mbrtowc.c.
AC_DEFUN([gl_PREREQ_MBRTOWC], [
  :
])


dnl From Paul Eggert

dnl This override of an autoconf macro can be removed when autoconf 2.60 or
dnl newer can be assumed everywhere.

m4_if(m4_version_compare(m4_defn([m4_PACKAGE_VERSION]),[2.60]),[-1],[
AC_DEFUN([AC_FUNC_MBRTOWC],
[
  dnl Same as AC_FUNC_MBRTOWC in autoconf-2.60.
  AC_CACHE_CHECK([whether mbrtowc and mbstate_t are properly declared],
    gl_cv_func_mbrtowc,
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
            [[#include <wchar.h>]],
            [[wchar_t wc;
              char const s[] = "";
              size_t n = 1;
              mbstate_t state;
              return ! (sizeof state && (mbrtowc) (&wc, s, n, &state));]])],
       gl_cv_func_mbrtowc=yes,
       gl_cv_func_mbrtowc=no)])
  if test $gl_cv_func_mbrtowc = yes; then
    AC_DEFINE([HAVE_MBRTOWC], [1],
      [Define to 1 if mbrtowc and mbstate_t are properly declared.])
  fi
])
])

# mbsinit.m4 serial 3
dnl Copyright (C) 2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_MBSINIT],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])

  AC_REQUIRE([AC_TYPE_MBSTATE_T])
  gl_MBSTATE_T_BROKEN
  if test $REPLACE_MBSTATE_T = 1; then
    REPLACE_MBSINIT=1
  fi
  AC_CHECK_FUNCS_ONCE([mbsinit])
  if test $ac_cv_func_mbsinit = no; then
    HAVE_MBSINIT=0
  fi
  if test $HAVE_MBSINIT = 0 || test $REPLACE_MBSINIT = 1; then
    gl_REPLACE_WCHAR_H
    AC_LIBOBJ([mbsinit])
    gl_PREREQ_MBSINIT
  fi
])

# Prerequisites of lib/mbsinit.c.
AC_DEFUN([gl_PREREQ_MBSINIT], [
  :
])

# mbstate_t.m4 serial 12
dnl Copyright (C) 2000-2002, 2008, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# From Paul Eggert.

# BeOS 5 has <wchar.h> but does not define mbstate_t,
# so you can't declare an object of that type.
# Check for this incompatibility with Standard C.

# AC_TYPE_MBSTATE_T
# -----------------
AC_DEFUN([AC_TYPE_MBSTATE_T],
[
   AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS]) dnl for HP-UX 11.11

   AC_CACHE_CHECK([for mbstate_t], [ac_cv_type_mbstate_t],
     [AC_COMPILE_IFELSE(
	[AC_LANG_PROGRAM(
	   [AC_INCLUDES_DEFAULT[
#	    include <wchar.h>]],
	   [[mbstate_t x; return sizeof x;]])],
	[ac_cv_type_mbstate_t=yes],
	[ac_cv_type_mbstate_t=no])])
   if test $ac_cv_type_mbstate_t = yes; then
     AC_DEFINE([HAVE_MBSTATE_T], [1],
	       [Define to 1 if <wchar.h> declares mbstate_t.])
   else
     AC_DEFINE([mbstate_t], [int],
	       [Define to a type if <wchar.h> does not define.])
   fi
])

# multiarch.m4 serial 3
dnl Copyright (C) 2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Determine whether the compiler is or may be producing universal binaries.
#
# On MacOS X 10.5 and later systems, the user can create libraries and
# executables that work on multiple system types--known as "fat" or
# "universal" binaries--by specifying multiple '-arch' options to the
# compiler but only a single '-arch' option to the preprocessor.  Like
# this:
#
#     ./configure CC="gcc -arch i386 -arch x86_64 -arch ppc -arch ppc64" \
#                 CXX="g++ -arch i386 -arch x86_64 -arch ppc -arch ppc64" \
#                 CPP="gcc -E" CXXCPP="g++ -E"
#
# Detect this situation and set the macro AA_APPLE_UNIVERSAL_BUILD at the
# beginning of config.h and set APPLE_UNIVERSAL_BUILD accordingly.

AC_DEFUN([gl_MULTIARCH],
[
  dnl This AC_REQUIRE is not necessary in theory. It works around a bug in
  dnl autoconf <= 2.63: AC_REQUIRE invocations inside AC_REQUIREd macros are
  dnl being handled better than AC_REQUIRE invocations inside normally invoked
  dnl macros.
  AC_REQUIRE([gl_MULTIARCH_BODY])
])

AC_DEFUN([gl_MULTIARCH_BODY],
[
  dnl Code similar to autoconf-2.63 AC_C_BIGENDIAN.
  gl_cv_c_multiarch=no
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE(
      [[#ifndef __APPLE_CC__
         not a universal capable compiler
        #endif
        typedef int dummy;
      ]])],
    [
     dnl Check for potential -arch flags.  It is not universal unless
     dnl there are at least two -arch flags with different values.
     arch=
     prev=
     for word in ${CC} ${CFLAGS} ${CPPFLAGS} ${LDFLAGS}; do
       if test -n "$prev"; then
         case $word in
           i?86 | x86_64 | ppc | ppc64)
             if test -z "$arch" || test "$arch" = "$word"; then
               arch="$word"
             else
               gl_cv_c_multiarch=yes
             fi
             ;;
         esac
         prev=
       else
         if test "x$word" = "x-arch"; then
           prev=arch
         fi
       fi
     done
    ])
  if test $gl_cv_c_multiarch = yes; then
    AC_DEFINE([AA_APPLE_UNIVERSAL_BUILD], [1],
      [Define if the compiler is building for multiple architectures of Apple platforms at once.])
    APPLE_UNIVERSAL_BUILD=1
  else
    APPLE_UNIVERSAL_BUILD=0
  fi
  AC_SUBST([APPLE_UNIVERSAL_BUILD])
])

# nls.m4 serial 5 (gettext-0.18)
dnl Copyright (C) 1995-2003, 2005-2006, 2008, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Authors:
dnl   Ulrich Drepper <drepper@cygnus.com>, 1995-2000.
dnl   Bruno Haible <haible@clisp.cons.org>, 2000-2003.

AC_PREREQ([2.50])

AC_DEFUN([AM_NLS],
[
  AC_MSG_CHECKING([whether NLS is requested])
  dnl Default is enabled NLS
  AC_ARG_ENABLE([nls],
    [  --disable-nls           do not use Native Language Support],
    USE_NLS=$enableval, USE_NLS=yes)
  AC_MSG_RESULT([$USE_NLS])
  AC_SUBST([USE_NLS])
])

# nocrash.m4 serial 2
dnl Copyright (C) 2005, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl Based on libsigsegv, from Bruno Haible and Paolo Bonzini.

AC_PREREQ([2.13])

dnl Expands to some code for use in .c programs that will cause the configure
dnl test to exit instead of crashing. This is useful to avoid triggering
dnl action from a background debugger and to avoid core dumps.
dnl Usage:   ...
dnl          ]GL_NOCRASH[
dnl          ...
dnl          int main() { nocrash_init(); ... }
AC_DEFUN([GL_NOCRASH],[[
#include <stdlib.h>
#if defined __MACH__ && defined __APPLE__
/* Avoid a crash on MacOS X.  */
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/thread_status.h>
#include <mach/exception.h>
#include <mach/task.h>
#include <pthread.h>
/* The exception port on which our thread listens.  */
static mach_port_t our_exception_port;
/* The main function of the thread listening for exceptions of type
   EXC_BAD_ACCESS.  */
static void *
mach_exception_thread (void *arg)
{
  /* Buffer for a message to be received.  */
  struct {
    mach_msg_header_t head;
    mach_msg_body_t msgh_body;
    char data[1024];
  } msg;
  mach_msg_return_t retval;
  /* Wait for a message on the exception port.  */
  retval = mach_msg (&msg.head, MACH_RCV_MSG | MACH_RCV_LARGE, 0, sizeof (msg),
                     our_exception_port, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
  if (retval != MACH_MSG_SUCCESS)
    abort ();
  exit (1);
}
static void
nocrash_init (void)
{
  mach_port_t self = mach_task_self ();
  /* Allocate a port on which the thread shall listen for exceptions.  */
  if (mach_port_allocate (self, MACH_PORT_RIGHT_RECEIVE, &our_exception_port)
      == KERN_SUCCESS) {
    /* See http://web.mit.edu/darwin/src/modules/xnu/osfmk/man/mach_port_insert_right.html.  */
    if (mach_port_insert_right (self, our_exception_port, our_exception_port,
                                MACH_MSG_TYPE_MAKE_SEND)
        == KERN_SUCCESS) {
      /* The exceptions we want to catch.  Only EXC_BAD_ACCESS is interesting
         for us.  */
      exception_mask_t mask = EXC_MASK_BAD_ACCESS;
      /* Create the thread listening on the exception port.  */
      pthread_attr_t attr;
      pthread_t thread;
      if (pthread_attr_init (&attr) == 0
          && pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED) == 0
          && pthread_create (&thread, &attr, mach_exception_thread, NULL) == 0) {
        pthread_attr_destroy (&attr);
        /* Replace the exception port info for these exceptions with our own.
           Note that we replace the exception port for the entire task, not only
           for a particular thread.  This has the effect that when our exception
           port gets the message, the thread specific exception port has already
           been asked, and we don't need to bother about it.
           See http://web.mit.edu/darwin/src/modules/xnu/osfmk/man/task_set_exception_ports.html.  */
        task_set_exception_ports (self, mask, our_exception_port,
                                  EXCEPTION_DEFAULT, MACHINE_THREAD_STATE);
      }
    }
  }
}
#else
/* Avoid a crash on POSIX systems.  */
#include <signal.h>
/* A POSIX signal handler.  */
static void
exception_handler (int sig)
{
  exit (1);
}
static void
nocrash_init (void)
{
#ifdef SIGSEGV
  signal (SIGSEGV, exception_handler);
#endif
#ifdef SIGBUS
  signal (SIGBUS, exception_handler);
#endif
}
#endif
]])

# po.m4 serial 17 (gettext-0.18)
dnl Copyright (C) 1995-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Authors:
dnl   Ulrich Drepper <drepper@cygnus.com>, 1995-2000.
dnl   Bruno Haible <haible@clisp.cons.org>, 2000-2003.

AC_PREREQ([2.50])

dnl Checks for all prerequisites of the po subdirectory.
AC_DEFUN([AM_PO_SUBDIRS],
[
  AC_REQUIRE([AC_PROG_MAKE_SET])dnl
  AC_REQUIRE([AC_PROG_INSTALL])dnl
  AC_REQUIRE([AM_PROG_MKDIR_P])dnl defined by automake
  AC_REQUIRE([AM_NLS])dnl

  dnl Release version of the gettext macros. This is used to ensure that
  dnl the gettext macros and po/Makefile.in.in are in sync.
  AC_SUBST([GETTEXT_MACRO_VERSION], [0.17])

  dnl Perform the following tests also if --disable-nls has been given,
  dnl because they are needed for "make dist" to work.

  dnl Search for GNU msgfmt in the PATH.
  dnl The first test excludes Solaris msgfmt and early GNU msgfmt versions.
  dnl The second test excludes FreeBSD msgfmt.
  AM_PATH_PROG_WITH_TEST(MSGFMT, msgfmt,
    [$ac_dir/$ac_word --statistics /dev/null >&]AS_MESSAGE_LOG_FD[ 2>&1 &&
     (if $ac_dir/$ac_word --statistics /dev/null 2>&1 >/dev/null | grep usage >/dev/null; then exit 1; else exit 0; fi)],
    :)
  AC_PATH_PROG([GMSGFMT], [gmsgfmt], [$MSGFMT])

  dnl Test whether it is GNU msgfmt >= 0.15.
changequote(,)dnl
  case `$MSGFMT --version | sed 1q | sed -e 's,^[^0-9]*,,'` in
    '' | 0.[0-9] | 0.[0-9].* | 0.1[0-4] | 0.1[0-4].*) MSGFMT_015=: ;;
    *) MSGFMT_015=$MSGFMT ;;
  esac
changequote([,])dnl
  AC_SUBST([MSGFMT_015])
changequote(,)dnl
  case `$GMSGFMT --version | sed 1q | sed -e 's,^[^0-9]*,,'` in
    '' | 0.[0-9] | 0.[0-9].* | 0.1[0-4] | 0.1[0-4].*) GMSGFMT_015=: ;;
    *) GMSGFMT_015=$GMSGFMT ;;
  esac
changequote([,])dnl
  AC_SUBST([GMSGFMT_015])

  dnl Search for GNU xgettext 0.12 or newer in the PATH.
  dnl The first test excludes Solaris xgettext and early GNU xgettext versions.
  dnl The second test excludes FreeBSD xgettext.
  AM_PATH_PROG_WITH_TEST(XGETTEXT, xgettext,
    [$ac_dir/$ac_word --omit-header --copyright-holder= --msgid-bugs-address= /dev/null >&]AS_MESSAGE_LOG_FD[ 2>&1 &&
     (if $ac_dir/$ac_word --omit-header --copyright-holder= --msgid-bugs-address= /dev/null 2>&1 >/dev/null | grep usage >/dev/null; then exit 1; else exit 0; fi)],
    :)
  dnl Remove leftover from FreeBSD xgettext call.
  rm -f messages.po

  dnl Test whether it is GNU xgettext >= 0.15.
changequote(,)dnl
  case `$XGETTEXT --version | sed 1q | sed -e 's,^[^0-9]*,,'` in
    '' | 0.[0-9] | 0.[0-9].* | 0.1[0-4] | 0.1[0-4].*) XGETTEXT_015=: ;;
    *) XGETTEXT_015=$XGETTEXT ;;
  esac
changequote([,])dnl
  AC_SUBST([XGETTEXT_015])

  dnl Search for GNU msgmerge 0.11 or newer in the PATH.
  AM_PATH_PROG_WITH_TEST(MSGMERGE, msgmerge,
    [$ac_dir/$ac_word --update -q /dev/null /dev/null >&]AS_MESSAGE_LOG_FD[ 2>&1], :)

  dnl Installation directories.
  dnl Autoconf >= 2.60 defines localedir. For older versions of autoconf, we
  dnl have to define it here, so that it can be used in po/Makefile.
  test -n "$localedir" || localedir='${datadir}/locale'
  AC_SUBST([localedir])

  dnl Support for AM_XGETTEXT_OPTION.
  test -n "${XGETTEXT_EXTRA_OPTIONS+set}" || XGETTEXT_EXTRA_OPTIONS=
  AC_SUBST([XGETTEXT_EXTRA_OPTIONS])

  AC_CONFIG_COMMANDS([po-directories], [[
    for ac_file in $CONFIG_FILES; do
      # Support "outfile[:infile[:infile...]]"
      case "$ac_file" in
        *:*) ac_file=`echo "$ac_file"|sed 's%:.*%%'` ;;
      esac
      # PO directories have a Makefile.in generated from Makefile.in.in.
      case "$ac_file" in */Makefile.in)
        # Adjust a relative srcdir.
        ac_dir=`echo "$ac_file"|sed 's%/[^/][^/]*$%%'`
        ac_dir_suffix="/`echo "$ac_dir"|sed 's%^\./%%'`"
        ac_dots=`echo "$ac_dir_suffix"|sed 's%/[^/]*%../%g'`
        # In autoconf-2.13 it is called $ac_given_srcdir.
        # In autoconf-2.50 it is called $srcdir.
        test -n "$ac_given_srcdir" || ac_given_srcdir="$srcdir"
        case "$ac_given_srcdir" in
          .)  top_srcdir=`echo $ac_dots|sed 's%/$%%'` ;;
          /*) top_srcdir="$ac_given_srcdir" ;;
          *)  top_srcdir="$ac_dots$ac_given_srcdir" ;;
        esac
        # Treat a directory as a PO directory if and only if it has a
        # POTFILES.in file. This allows packages to have multiple PO
        # directories under different names or in different locations.
        if test -f "$ac_given_srcdir/$ac_dir/POTFILES.in"; then
          rm -f "$ac_dir/POTFILES"
          test -n "$as_me" && echo "$as_me: creating $ac_dir/POTFILES" || echo "creating $ac_dir/POTFILES"
          cat "$ac_given_srcdir/$ac_dir/POTFILES.in" | sed -e "/^#/d" -e "/^[ 	]*\$/d" -e "s,.*,     $top_srcdir/& \\\\," | sed -e "\$s/\(.*\) \\\\/\1/" > "$ac_dir/POTFILES"
          POMAKEFILEDEPS="POTFILES.in"
          # ALL_LINGUAS, POFILES, UPDATEPOFILES, DUMMYPOFILES, GMOFILES depend
          # on $ac_dir but don't depend on user-specified configuration
          # parameters.
          if test -f "$ac_given_srcdir/$ac_dir/LINGUAS"; then
            # The LINGUAS file contains the set of available languages.
            if test -n "$OBSOLETE_ALL_LINGUAS"; then
              test -n "$as_me" && echo "$as_me: setting ALL_LINGUAS in configure.in is obsolete" || echo "setting ALL_LINGUAS in configure.in is obsolete"
            fi
            ALL_LINGUAS_=`sed -e "/^#/d" -e "s/#.*//" "$ac_given_srcdir/$ac_dir/LINGUAS"`
            # Hide the ALL_LINGUAS assigment from automake < 1.5.
            eval 'ALL_LINGUAS''=$ALL_LINGUAS_'
            POMAKEFILEDEPS="$POMAKEFILEDEPS LINGUAS"
          else
            # The set of available languages was given in configure.in.
            # Hide the ALL_LINGUAS assigment from automake < 1.5.
            eval 'ALL_LINGUAS''=$OBSOLETE_ALL_LINGUAS'
          fi
          # Compute POFILES
          # as      $(foreach lang, $(ALL_LINGUAS), $(srcdir)/$(lang).po)
          # Compute UPDATEPOFILES
          # as      $(foreach lang, $(ALL_LINGUAS), $(lang).po-update)
          # Compute DUMMYPOFILES
          # as      $(foreach lang, $(ALL_LINGUAS), $(lang).nop)
          # Compute GMOFILES
          # as      $(foreach lang, $(ALL_LINGUAS), $(srcdir)/$(lang).gmo)
          case "$ac_given_srcdir" in
            .) srcdirpre= ;;
            *) srcdirpre='$(srcdir)/' ;;
          esac
          POFILES=
          UPDATEPOFILES=
          DUMMYPOFILES=
          GMOFILES=
          for lang in $ALL_LINGUAS; do
            POFILES="$POFILES $srcdirpre$lang.po"
            UPDATEPOFILES="$UPDATEPOFILES $lang.po-update"
            DUMMYPOFILES="$DUMMYPOFILES $lang.nop"
            GMOFILES="$GMOFILES $srcdirpre$lang.gmo"
          done
          # CATALOGS depends on both $ac_dir and the user's LINGUAS
          # environment variable.
          INST_LINGUAS=
          if test -n "$ALL_LINGUAS"; then
            for presentlang in $ALL_LINGUAS; do
              useit=no
              if test "%UNSET%" != "$LINGUAS"; then
                desiredlanguages="$LINGUAS"
              else
                desiredlanguages="$ALL_LINGUAS"
              fi
              for desiredlang in $desiredlanguages; do
                # Use the presentlang catalog if desiredlang is
                #   a. equal to presentlang, or
                #   b. a variant of presentlang (because in this case,
                #      presentlang can be used as a fallback for messages
                #      which are not translated in the desiredlang catalog).
                case "$desiredlang" in
                  "$presentlang"*) useit=yes;;
                esac
              done
              if test $useit = yes; then
                INST_LINGUAS="$INST_LINGUAS $presentlang"
              fi
            done
          fi
          CATALOGS=
          if test -n "$INST_LINGUAS"; then
            for lang in $INST_LINGUAS; do
              CATALOGS="$CATALOGS $lang.gmo"
            done
          fi
          test -n "$as_me" && echo "$as_me: creating $ac_dir/Makefile" || echo "creating $ac_dir/Makefile"
          sed -e "/^POTFILES =/r $ac_dir/POTFILES" -e "/^# Makevars/r $ac_given_srcdir/$ac_dir/Makevars" -e "s|@POFILES@|$POFILES|g" -e "s|@UPDATEPOFILES@|$UPDATEPOFILES|g" -e "s|@DUMMYPOFILES@|$DUMMYPOFILES|g" -e "s|@GMOFILES@|$GMOFILES|g" -e "s|@CATALOGS@|$CATALOGS|g" -e "s|@POMAKEFILEDEPS@|$POMAKEFILEDEPS|g" "$ac_dir/Makefile.in" > "$ac_dir/Makefile"
          for f in "$ac_given_srcdir/$ac_dir"/Rules-*; do
            if test -f "$f"; then
              case "$f" in
                *.orig | *.bak | *~) ;;
                *) cat "$f" >> "$ac_dir/Makefile" ;;
              esac
            fi
          done
        fi
        ;;
      esac
    done]],
   [# Capture the value of obsolete ALL_LINGUAS because we need it to compute
    # POFILES, UPDATEPOFILES, DUMMYPOFILES, GMOFILES, CATALOGS. But hide it
    # from automake < 1.5.
    eval 'OBSOLETE_ALL_LINGUAS''="$ALL_LINGUAS"'
    # Capture the value of LINGUAS because we need it to compute CATALOGS.
    LINGUAS="${LINGUAS-%UNSET%}"
   ])
])

dnl Postprocesses a Makefile in a directory containing PO files.
AC_DEFUN([AM_POSTPROCESS_PO_MAKEFILE],
[
  # When this code is run, in config.status, two variables have already been
  # set:
  # - OBSOLETE_ALL_LINGUAS is the value of LINGUAS set in configure.in,
  # - LINGUAS is the value of the environment variable LINGUAS at configure
  #   time.

changequote(,)dnl
  # Adjust a relative srcdir.
  ac_dir=`echo "$ac_file"|sed 's%/[^/][^/]*$%%'`
  ac_dir_suffix="/`echo "$ac_dir"|sed 's%^\./%%'`"
  ac_dots=`echo "$ac_dir_suffix"|sed 's%/[^/]*%../%g'`
  # In autoconf-2.13 it is called $ac_given_srcdir.
  # In autoconf-2.50 it is called $srcdir.
  test -n "$ac_given_srcdir" || ac_given_srcdir="$srcdir"
  case "$ac_given_srcdir" in
    .)  top_srcdir=`echo $ac_dots|sed 's%/$%%'` ;;
    /*) top_srcdir="$ac_given_srcdir" ;;
    *)  top_srcdir="$ac_dots$ac_given_srcdir" ;;
  esac

  # Find a way to echo strings without interpreting backslash.
  if test "X`(echo '\t') 2>/dev/null`" = 'X\t'; then
    gt_echo='echo'
  else
    if test "X`(printf '%s\n' '\t') 2>/dev/null`" = 'X\t'; then
      gt_echo='printf %s\n'
    else
      echo_func () {
        cat <<EOT
$*
EOT
      }
      gt_echo='echo_func'
    fi
  fi

  # A sed script that extracts the value of VARIABLE from a Makefile.
  sed_x_variable='
# Test if the hold space is empty.
x
s/P/P/
x
ta
# Yes it was empty. Look if we have the expected variable definition.
/^[	 ]*VARIABLE[	 ]*=/{
  # Seen the first line of the variable definition.
  s/^[	 ]*VARIABLE[	 ]*=//
  ba
}
bd
:a
# Here we are processing a line from the variable definition.
# Remove comment, more precisely replace it with a space.
s/#.*$/ /
# See if the line ends in a backslash.
tb
:b
s/\\$//
# Print the line, without the trailing backslash.
p
tc
# There was no trailing backslash. The end of the variable definition is
# reached. Clear the hold space.
s/^.*$//
x
bd
:c
# A trailing backslash means that the variable definition continues in the
# next line. Put a nonempty string into the hold space to indicate this.
s/^.*$/P/
x
:d
'
changequote([,])dnl

  # Set POTFILES to the value of the Makefile variable POTFILES.
  sed_x_POTFILES=`$gt_echo "$sed_x_variable" | sed -e '/^ *#/d' -e 's/VARIABLE/POTFILES/g'`
  POTFILES=`sed -n -e "$sed_x_POTFILES" < "$ac_file"`
  # Compute POTFILES_DEPS as
  #   $(foreach file, $(POTFILES), $(top_srcdir)/$(file))
  POTFILES_DEPS=
  for file in $POTFILES; do
    POTFILES_DEPS="$POTFILES_DEPS "'$(top_srcdir)/'"$file"
  done
  POMAKEFILEDEPS=""

  if test -n "$OBSOLETE_ALL_LINGUAS"; then
    test -n "$as_me" && echo "$as_me: setting ALL_LINGUAS in configure.in is obsolete" || echo "setting ALL_LINGUAS in configure.in is obsolete"
  fi
  if test -f "$ac_given_srcdir/$ac_dir/LINGUAS"; then
    # The LINGUAS file contains the set of available languages.
    ALL_LINGUAS_=`sed -e "/^#/d" -e "s/#.*//" "$ac_given_srcdir/$ac_dir/LINGUAS"`
    POMAKEFILEDEPS="$POMAKEFILEDEPS LINGUAS"
  else
    # Set ALL_LINGUAS to the value of the Makefile variable LINGUAS.
    sed_x_LINGUAS=`$gt_echo "$sed_x_variable" | sed -e '/^ *#/d' -e 's/VARIABLE/LINGUAS/g'`
    ALL_LINGUAS_=`sed -n -e "$sed_x_LINGUAS" < "$ac_file"`
  fi
  # Hide the ALL_LINGUAS assigment from automake < 1.5.
  eval 'ALL_LINGUAS''=$ALL_LINGUAS_'
  # Compute POFILES
  # as      $(foreach lang, $(ALL_LINGUAS), $(srcdir)/$(lang).po)
  # Compute UPDATEPOFILES
  # as      $(foreach lang, $(ALL_LINGUAS), $(lang).po-update)
  # Compute DUMMYPOFILES
  # as      $(foreach lang, $(ALL_LINGUAS), $(lang).nop)
  # Compute GMOFILES
  # as      $(foreach lang, $(ALL_LINGUAS), $(srcdir)/$(lang).gmo)
  # Compute PROPERTIESFILES
  # as      $(foreach lang, $(ALL_LINGUAS), $(top_srcdir)/$(DOMAIN)_$(lang).properties)
  # Compute CLASSFILES
  # as      $(foreach lang, $(ALL_LINGUAS), $(top_srcdir)/$(DOMAIN)_$(lang).class)
  # Compute QMFILES
  # as      $(foreach lang, $(ALL_LINGUAS), $(srcdir)/$(lang).qm)
  # Compute MSGFILES
  # as      $(foreach lang, $(ALL_LINGUAS), $(srcdir)/$(frob $(lang)).msg)
  # Compute RESOURCESDLLFILES
  # as      $(foreach lang, $(ALL_LINGUAS), $(srcdir)/$(frob $(lang))/$(DOMAIN).resources.dll)
  case "$ac_given_srcdir" in
    .) srcdirpre= ;;
    *) srcdirpre='$(srcdir)/' ;;
  esac
  POFILES=
  UPDATEPOFILES=
  DUMMYPOFILES=
  GMOFILES=
  PROPERTIESFILES=
  CLASSFILES=
  QMFILES=
  MSGFILES=
  RESOURCESDLLFILES=
  for lang in $ALL_LINGUAS; do
    POFILES="$POFILES $srcdirpre$lang.po"
    UPDATEPOFILES="$UPDATEPOFILES $lang.po-update"
    DUMMYPOFILES="$DUMMYPOFILES $lang.nop"
    GMOFILES="$GMOFILES $srcdirpre$lang.gmo"
    PROPERTIESFILES="$PROPERTIESFILES \$(top_srcdir)/\$(DOMAIN)_$lang.properties"
    CLASSFILES="$CLASSFILES \$(top_srcdir)/\$(DOMAIN)_$lang.class"
    QMFILES="$QMFILES $srcdirpre$lang.qm"
    frobbedlang=`echo $lang | sed -e 's/\..*$//' -e 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/'`
    MSGFILES="$MSGFILES $srcdirpre$frobbedlang.msg"
    frobbedlang=`echo $lang | sed -e 's/_/-/g' -e 's/^sr-CS/sr-SP/' -e 's/@latin$/-Latn/' -e 's/@cyrillic$/-Cyrl/' -e 's/^sr-SP$/sr-SP-Latn/' -e 's/^uz-UZ$/uz-UZ-Latn/'`
    RESOURCESDLLFILES="$RESOURCESDLLFILES $srcdirpre$frobbedlang/\$(DOMAIN).resources.dll"
  done
  # CATALOGS depends on both $ac_dir and the user's LINGUAS
  # environment variable.
  INST_LINGUAS=
  if test -n "$ALL_LINGUAS"; then
    for presentlang in $ALL_LINGUAS; do
      useit=no
      if test "%UNSET%" != "$LINGUAS"; then
        desiredlanguages="$LINGUAS"
      else
        desiredlanguages="$ALL_LINGUAS"
      fi
      for desiredlang in $desiredlanguages; do
        # Use the presentlang catalog if desiredlang is
        #   a. equal to presentlang, or
        #   b. a variant of presentlang (because in this case,
        #      presentlang can be used as a fallback for messages
        #      which are not translated in the desiredlang catalog).
        case "$desiredlang" in
          "$presentlang"*) useit=yes;;
        esac
      done
      if test $useit = yes; then
        INST_LINGUAS="$INST_LINGUAS $presentlang"
      fi
    done
  fi
  CATALOGS=
  JAVACATALOGS=
  QTCATALOGS=
  TCLCATALOGS=
  CSHARPCATALOGS=
  if test -n "$INST_LINGUAS"; then
    for lang in $INST_LINGUAS; do
      CATALOGS="$CATALOGS $lang.gmo"
      JAVACATALOGS="$JAVACATALOGS \$(DOMAIN)_$lang.properties"
      QTCATALOGS="$QTCATALOGS $lang.qm"
      frobbedlang=`echo $lang | sed -e 's/\..*$//' -e 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/'`
      TCLCATALOGS="$TCLCATALOGS $frobbedlang.msg"
      frobbedlang=`echo $lang | sed -e 's/_/-/g' -e 's/^sr-CS/sr-SP/' -e 's/@latin$/-Latn/' -e 's/@cyrillic$/-Cyrl/' -e 's/^sr-SP$/sr-SP-Latn/' -e 's/^uz-UZ$/uz-UZ-Latn/'`
      CSHARPCATALOGS="$CSHARPCATALOGS $frobbedlang/\$(DOMAIN).resources.dll"
    done
  fi

  sed -e "s|@POTFILES_DEPS@|$POTFILES_DEPS|g" -e "s|@POFILES@|$POFILES|g" -e "s|@UPDATEPOFILES@|$UPDATEPOFILES|g" -e "s|@DUMMYPOFILES@|$DUMMYPOFILES|g" -e "s|@GMOFILES@|$GMOFILES|g" -e "s|@PROPERTIESFILES@|$PROPERTIESFILES|g" -e "s|@CLASSFILES@|$CLASSFILES|g" -e "s|@QMFILES@|$QMFILES|g" -e "s|@MSGFILES@|$MSGFILES|g" -e "s|@RESOURCESDLLFILES@|$RESOURCESDLLFILES|g" -e "s|@CATALOGS@|$CATALOGS|g" -e "s|@JAVACATALOGS@|$JAVACATALOGS|g" -e "s|@QTCATALOGS@|$QTCATALOGS|g" -e "s|@TCLCATALOGS@|$TCLCATALOGS|g" -e "s|@CSHARPCATALOGS@|$CSHARPCATALOGS|g" -e 's,^#distdir:,distdir:,' < "$ac_file" > "$ac_file.tmp"
  if grep -l '@TCLCATALOGS@' "$ac_file" > /dev/null; then
    # Add dependencies that cannot be formulated as a simple suffix rule.
    for lang in $ALL_LINGUAS; do
      frobbedlang=`echo $lang | sed -e 's/\..*$//' -e 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/'`
      cat >> "$ac_file.tmp" <<EOF
$frobbedlang.msg: $lang.po
	@echo "\$(MSGFMT) -c --tcl -d \$(srcdir) -l $lang $srcdirpre$lang.po"; \
	\$(MSGFMT) -c --tcl -d "\$(srcdir)" -l $lang $srcdirpre$lang.po || { rm -f "\$(srcdir)/$frobbedlang.msg"; exit 1; }
EOF
    done
  fi
  if grep -l '@CSHARPCATALOGS@' "$ac_file" > /dev/null; then
    # Add dependencies that cannot be formulated as a simple suffix rule.
    for lang in $ALL_LINGUAS; do
      frobbedlang=`echo $lang | sed -e 's/_/-/g' -e 's/^sr-CS/sr-SP/' -e 's/@latin$/-Latn/' -e 's/@cyrillic$/-Cyrl/' -e 's/^sr-SP$/sr-SP-Latn/' -e 's/^uz-UZ$/uz-UZ-Latn/'`
      cat >> "$ac_file.tmp" <<EOF
$frobbedlang/\$(DOMAIN).resources.dll: $lang.po
	@echo "\$(MSGFMT) -c --csharp -d \$(srcdir) -l $lang $srcdirpre$lang.po -r \$(DOMAIN)"; \
	\$(MSGFMT) -c --csharp -d "\$(srcdir)" -l $lang $srcdirpre$lang.po -r "\$(DOMAIN)" || { rm -f "\$(srcdir)/$frobbedlang.msg"; exit 1; }
EOF
    done
  fi
  if test -n "$POMAKEFILEDEPS"; then
    cat >> "$ac_file.tmp" <<EOF
Makefile: $POMAKEFILEDEPS
EOF
  fi
  mv "$ac_file.tmp" "$ac_file"
])

dnl Initializes the accumulator used by AM_XGETTEXT_OPTION.
AC_DEFUN([AM_XGETTEXT_OPTION_INIT],
[
  XGETTEXT_EXTRA_OPTIONS=
])

dnl Registers an option to be passed to xgettext in the po subdirectory.
AC_DEFUN([AM_XGETTEXT_OPTION],
[
  AC_REQUIRE([AM_XGETTEXT_OPTION_INIT])
  XGETTEXT_EXTRA_OPTIONS="$XGETTEXT_EXTRA_OPTIONS $1"
])

# progtest.m4 serial 6 (gettext-0.18)
dnl Copyright (C) 1996-2003, 2005, 2008, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl
dnl This file can can be used in projects which are not available under
dnl the GNU General Public License or the GNU Library General Public
dnl License but which still want to provide support for the GNU gettext
dnl functionality.
dnl Please note that the actual code of the GNU gettext library is covered
dnl by the GNU Library General Public License, and the rest of the GNU
dnl gettext package package is covered by the GNU General Public License.
dnl They are *not* in the public domain.

dnl Authors:
dnl   Ulrich Drepper <drepper@cygnus.com>, 1996.

AC_PREREQ([2.50])

# Search path for a program which passes the given test.

dnl AM_PATH_PROG_WITH_TEST(VARIABLE, PROG-TO-CHECK-FOR,
dnl   TEST-PERFORMED-ON-FOUND_PROGRAM [, VALUE-IF-NOT-FOUND [, PATH]])
AC_DEFUN([AM_PATH_PROG_WITH_TEST],
[
# Prepare PATH_SEPARATOR.
# The user is always right.
if test "${PATH_SEPARATOR+set}" != set; then
  echo "#! /bin/sh" >conf$$.sh
  echo  "exit 0"   >>conf$$.sh
  chmod +x conf$$.sh
  if (PATH="/nonexistent;."; conf$$.sh) >/dev/null 2>&1; then
    PATH_SEPARATOR=';'
  else
    PATH_SEPARATOR=:
  fi
  rm -f conf$$.sh
fi

# Find out how to test for executable files. Don't use a zero-byte file,
# as systems may use methods other than mode bits to determine executability.
cat >conf$$.file <<_ASEOF
#! /bin/sh
exit 0
_ASEOF
chmod +x conf$$.file
if test -x conf$$.file >/dev/null 2>&1; then
  ac_executable_p="test -x"
else
  ac_executable_p="test -f"
fi
rm -f conf$$.file

# Extract the first word of "$2", so it can be a program name with args.
set dummy $2; ac_word=[$]2
AC_MSG_CHECKING([for $ac_word])
AC_CACHE_VAL([ac_cv_path_$1],
[case "[$]$1" in
  [[\\/]]* | ?:[[\\/]]*)
    ac_cv_path_$1="[$]$1" # Let the user override the test with a path.
    ;;
  *)
    ac_save_IFS="$IFS"; IFS=$PATH_SEPARATOR
    for ac_dir in ifelse([$5], , $PATH, [$5]); do
      IFS="$ac_save_IFS"
      test -z "$ac_dir" && ac_dir=.
      for ac_exec_ext in '' $ac_executable_extensions; do
        if $ac_executable_p "$ac_dir/$ac_word$ac_exec_ext"; then
          echo "$as_me: trying $ac_dir/$ac_word..." >&AS_MESSAGE_LOG_FD
          if [$3]; then
            ac_cv_path_$1="$ac_dir/$ac_word$ac_exec_ext"
            break 2
          fi
        fi
      done
    done
    IFS="$ac_save_IFS"
dnl If no 4th arg is given, leave the cache variable unset,
dnl so AC_PATH_PROGS will keep looking.
ifelse([$4], , , [  test -z "[$]ac_cv_path_$1" && ac_cv_path_$1="$4"
])dnl
    ;;
esac])dnl
$1="$ac_cv_path_$1"
if test ifelse([$4], , [-n "[$]$1"], ["[$]$1" != "$4"]); then
  AC_MSG_RESULT([$][$1])
else
  AC_MSG_RESULT([no])
fi
AC_SUBST([$1])dnl
])

# serial 53

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2004, 2005,
# 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

dnl Initially derived from code in GNU grep.
dnl Mostly written by Jim Meyering.

AC_PREREQ([2.50])

AC_DEFUN([gl_REGEX],
[
  AC_CHECK_HEADERS_ONCE([locale.h])

  AC_ARG_WITH([included-regex],
    [AS_HELP_STRING([--without-included-regex],
		    [don't compile regex; this is the default on 32-bit
		     systems with recent-enough versions of the GNU C
		     Library (use with caution on other systems).
		     On systems with 64-bit ptrdiff_t and 32-bit int,
		     --with-included-regex is the default, in case
		     regex functions operate on very long strings (>2GB)])])

  case $with_included_regex in #(
  yes|no) ac_use_included_regex=$with_included_regex
	;;
  '')
    # If the system regex support is good enough that it passes the
    # following run test, then default to *not* using the included regex.c.
    # If cross compiling, assume the test would fail and use the included
    # regex.c.
    AC_CACHE_CHECK([for working re_compile_pattern],
		   [gl_cv_func_re_compile_pattern_working],
      [AC_RUN_IFELSE(
	[AC_LANG_PROGRAM(
	  [AC_INCLUDES_DEFAULT[
	   #if HAVE_LOCALE_H
	    #include <locale.h>
	   #endif
	   #include <limits.h>
	   #include <regex.h>
	   ]],
	  [[static struct re_pattern_buffer regex;
	    unsigned char folded_chars[UCHAR_MAX + 1];
	    int i;
	    const char *s;
	    struct re_registers regs;

	    #if HAVE_LOCALE_H
	      /* http://sourceware.org/ml/libc-hacker/2006-09/msg00008.html
		 This test needs valgrind to catch the bug on Debian
		 GNU/Linux 3.1 x86, but it might catch the bug better
		 on other platforms and it shouldn't hurt to try the
		 test here.  */
	      if (setlocale (LC_ALL, "en_US.UTF-8"))
		{
		  static char const pat[] = "insert into";
		  static char const data[] =
		    "\xFF\0\x12\xA2\xAA\xC4\xB1,K\x12\xC4\xB1*\xACK";
		  re_set_syntax (RE_SYNTAX_GREP | RE_HAT_LISTS_NOT_NEWLINE
				 | RE_ICASE);
		  memset (&regex, 0, sizeof regex);
		  s = re_compile_pattern (pat, sizeof pat - 1, &regex);
		  if (s)
		    return 1;
		  if (re_search (&regex, data, sizeof data - 1,
				 0, sizeof data - 1, &regs)
		      != -1)
		    return 1;
		  if (! setlocale (LC_ALL, "C"))
		    return 1;
		}
	    #endif

	    /* This test is from glibc bug 3957, reported by Andrew Mackey.  */
	    re_set_syntax (RE_SYNTAX_EGREP | RE_HAT_LISTS_NOT_NEWLINE);
	    memset (&regex, 0, sizeof regex);
	    s = re_compile_pattern ("a[^x]b", 6, &regex);
	    if (s)
	      return 1;

	    /* This should fail, but succeeds for glibc-2.5.  */
	    if (re_search (&regex, "a\nb", 3, 0, 3, &regs) != -1)
	      return 1;

	    /* This regular expression is from Spencer ere test number 75
	       in grep-2.3.  */
	    re_set_syntax (RE_SYNTAX_POSIX_EGREP);
	    memset (&regex, 0, sizeof regex);
	    for (i = 0; i <= UCHAR_MAX; i++)
	      folded_chars[i] = i;
	    regex.translate = folded_chars;
	    s = re_compile_pattern ("a[[:@:>@:]]b\n", 11, &regex);
	    /* This should fail with _Invalid character class name_ error.  */
	    if (!s)
	      return 1;

	    /* This should succeed, but does not for glibc-2.1.3.  */
	    memset (&regex, 0, sizeof regex);
	    s = re_compile_pattern ("{1", 2, &regex);

	    if (s)
	      return 1;

	    /* The following example is derived from a problem report
	       against gawk from Jorge Stolfi <stolfi@ic.unicamp.br>.  */
	    memset (&regex, 0, sizeof regex);
	    s = re_compile_pattern ("[an\371]*n", 7, &regex);
	    if (s)
	      return 1;

	    /* This should match, but does not for glibc-2.2.1.  */
	    if (re_match (&regex, "an", 2, 0, &regs) != 2)
	      return 1;

	    memset (&regex, 0, sizeof regex);
	    s = re_compile_pattern ("x", 1, &regex);
	    if (s)
	      return 1;

	    /* glibc-2.2.93 does not work with a negative RANGE argument.  */
	    if (re_search (&regex, "wxy", 3, 2, -2, &regs) != 1)
	      return 1;

	    /* The version of regex.c in older versions of gnulib
	       ignored RE_ICASE.  Detect that problem too.  */
	    re_set_syntax (RE_SYNTAX_EMACS | RE_ICASE);
	    memset (&regex, 0, sizeof regex);
	    s = re_compile_pattern ("x", 1, &regex);
	    if (s)
	      return 1;

	    if (re_search (&regex, "WXY", 3, 0, 3, &regs) < 0)
	      return 1;

	    /* Catch a bug reported by Vin Shelton in
	       http://lists.gnu.org/archive/html/bug-coreutils/2007-06/msg00089.html
	       */
	    re_set_syntax (RE_SYNTAX_POSIX_BASIC
			   & ~RE_CONTEXT_INVALID_DUP
			   & ~RE_NO_EMPTY_RANGES);
	    memset (&regex, 0, sizeof regex);
	    s = re_compile_pattern ("[[:alnum:]_-]\\\\+$", 16, &regex);
	    if (s)
	      return 1;

	    /* REG_STARTEND was added to glibc on 2004-01-15.
	       Reject older versions.  */
	    if (! REG_STARTEND)
	      return 1;

	    /* Reject hosts whose regoff_t values are too narrow.
	       These include glibc 2.3.5 on hosts with 64-bit ptrdiff_t
	       and 32-bit int.  */
	    if (sizeof (regoff_t) < sizeof (ptrdiff_t)
		|| sizeof (regoff_t) < sizeof (ssize_t))
	      return 1;

	    return 0;]])],
       [gl_cv_func_re_compile_pattern_working=yes],
       [gl_cv_func_re_compile_pattern_working=no],
       dnl When crosscompiling, assume it is not working.
       [gl_cv_func_re_compile_pattern_working=no])])
    case $gl_cv_func_re_compile_pattern_working in #(
    yes) ac_use_included_regex=no;; #(
    no) ac_use_included_regex=yes;;
    esac
    ;;
  *) AC_MSG_ERROR([Invalid value for --with-included-regex: $with_included_regex])
    ;;
  esac

  if test $ac_use_included_regex = yes; then
    AC_DEFINE([_REGEX_LARGE_OFFSETS], [1],
      [Define if you want regoff_t to be at least as wide POSIX requires.])
    AC_DEFINE([re_syntax_options], [rpl_re_syntax_options],
      [Define to rpl_re_syntax_options if the replacement should be used.])
    AC_DEFINE([re_set_syntax], [rpl_re_set_syntax],
      [Define to rpl_re_set_syntax if the replacement should be used.])
    AC_DEFINE([re_compile_pattern], [rpl_re_compile_pattern],
      [Define to rpl_re_compile_pattern if the replacement should be used.])
    AC_DEFINE([re_compile_fastmap], [rpl_re_compile_fastmap],
      [Define to rpl_re_compile_fastmap if the replacement should be used.])
    AC_DEFINE([re_search], [rpl_re_search],
      [Define to rpl_re_search if the replacement should be used.])
    AC_DEFINE([re_search_2], [rpl_re_search_2],
      [Define to rpl_re_search_2 if the replacement should be used.])
    AC_DEFINE([re_match], [rpl_re_match],
      [Define to rpl_re_match if the replacement should be used.])
    AC_DEFINE([re_match_2], [rpl_re_match_2],
      [Define to rpl_re_match_2 if the replacement should be used.])
    AC_DEFINE([re_set_registers], [rpl_re_set_registers],
      [Define to rpl_re_set_registers if the replacement should be used.])
    AC_DEFINE([re_comp], [rpl_re_comp],
      [Define to rpl_re_comp if the replacement should be used.])
    AC_DEFINE([re_exec], [rpl_re_exec],
      [Define to rpl_re_exec if the replacement should be used.])
    AC_DEFINE([regcomp], [rpl_regcomp],
      [Define to rpl_regcomp if the replacement should be used.])
    AC_DEFINE([regexec], [rpl_regexec],
      [Define to rpl_regexec if the replacement should be used.])
    AC_DEFINE([regerror], [rpl_regerror],
      [Define to rpl_regerror if the replacement should be used.])
    AC_DEFINE([regfree], [rpl_regfree],
      [Define to rpl_regfree if the replacement should be used.])
    AC_LIBOBJ([regex])
    gl_PREREQ_REGEX
  fi
])

# Prerequisites of lib/regex.c and lib/regex_internal.c.
AC_DEFUN([gl_PREREQ_REGEX],
[
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])
  AC_REQUIRE([AC_C_RESTRICT])
  AC_REQUIRE([AC_TYPE_MBSTATE_T])
  AC_CHECK_HEADERS([libintl.h])
  AC_CHECK_FUNCS_ONCE([isblank iswctype wcscoll])
  AC_CHECK_DECLS([isblank], [], [], [#include <ctype.h>])
])

# ssize_t.m4 serial 4 (gettext-0.15)
dnl Copyright (C) 2001-2003, 2006 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.
dnl Test whether ssize_t is defined.

AC_DEFUN([gt_TYPE_SSIZE_T],
[
  AC_CACHE_CHECK([for ssize_t], [gt_cv_ssize_t],
    [AC_TRY_COMPILE([#include <sys/types.h>],
       [int x = sizeof (ssize_t *) + sizeof (ssize_t);
        return !x;],
       [gt_cv_ssize_t=yes], [gt_cv_ssize_t=no])])
  if test $gt_cv_ssize_t = no; then
    AC_DEFINE([ssize_t], [int],
              [Define as a signed type of the same size as size_t.])
  fi
])

# Check for stdbool.h that conforms to C99.

dnl Copyright (C) 2002-2006, 2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Prepare for substituting <stdbool.h> if it is not supported.

AC_DEFUN([AM_STDBOOL_H],
[
  AC_REQUIRE([AC_HEADER_STDBOOL])

  # Define two additional variables used in the Makefile substitution.

  if test "$ac_cv_header_stdbool_h" = yes; then
    STDBOOL_H=''
  else
    STDBOOL_H='stdbool.h'
  fi
  AC_SUBST([STDBOOL_H])

  if test "$ac_cv_type__Bool" = yes; then
    HAVE__BOOL=1
  else
    HAVE__BOOL=0
  fi
  AC_SUBST([HAVE__BOOL])
])

# AM_STDBOOL_H will be renamed to gl_STDBOOL_H in the future.
AC_DEFUN([gl_STDBOOL_H], [AM_STDBOOL_H])

# This macro is only needed in autoconf <= 2.59.  Newer versions of autoconf
# have this macro built-in.

AC_DEFUN([AC_HEADER_STDBOOL],
  [AC_CACHE_CHECK([for stdbool.h that conforms to C99],
     [ac_cv_header_stdbool_h],
     [AC_TRY_COMPILE(
	[
	  #include <stdbool.h>
	  #ifndef bool
	   "error: bool is not defined"
	  #endif
	  #ifndef false
	   "error: false is not defined"
	  #endif
	  #if false
	   "error: false is not 0"
	  #endif
	  #ifndef true
	   "error: true is not defined"
	  #endif
	  #if true != 1
	   "error: true is not 1"
	  #endif
	  #ifndef __bool_true_false_are_defined
	   "error: __bool_true_false_are_defined is not defined"
	  #endif

	  struct s { _Bool s: 1; _Bool t; } s;

	  char a[true == 1 ? 1 : -1];
	  char b[false == 0 ? 1 : -1];
	  char c[__bool_true_false_are_defined == 1 ? 1 : -1];
	  char d[(bool) 0.5 == true ? 1 : -1];
	  bool e = &s;
	  char f[(_Bool) 0.0 == false ? 1 : -1];
	  char g[true];
	  char h[sizeof (_Bool)];
	  char i[sizeof s.t];
	  enum { j = false, k = true, l = false * true, m = true * 256 };
	  _Bool n[m];
	  char o[sizeof n == m * sizeof n[0] ? 1 : -1];
	  char p[-1 - (_Bool) 0 < 0 && -1 - (bool) 0 < 0 ? 1 : -1];
	  #if defined __xlc__ || defined __GNUC__
	   /* Catch a bug in IBM AIX xlc compiler version 6.0.0.0
	      reported by James Lemley on 2005-10-05; see
	      http://lists.gnu.org/archive/html/bug-coreutils/2005-10/msg00086.html
	      This test is not quite right, since xlc is allowed to
	      reject this program, as the initializer for xlcbug is
	      not one of the forms that C requires support for.
	      However, doing the test right would require a run-time
	      test, and that would make cross-compilation harder.
	      Let us hope that IBM fixes the xlc bug, and also adds
	      support for this kind of constant expression.  In the
	      meantime, this test will reject xlc, which is OK, since
	      our stdbool.h substitute should suffice.  We also test
	      this with GCC, where it should work, to detect more
	      quickly whether someone messes up the test in the
	      future.  */
	   char digs[] = "0123456789";
	   int xlcbug = 1 / (&(digs + 5)[-2 + (bool) 1] == &digs[4] ? 1 : -1);
	  #endif
	  /* Catch a bug in an HP-UX C compiler.  See
	     http://gcc.gnu.org/ml/gcc-patches/2003-12/msg02303.html
	     http://lists.gnu.org/archive/html/bug-coreutils/2005-11/msg00161.html
	   */
	  _Bool q = true;
	  _Bool *pq = &q;
	],
	[
	  *pq |= q;
	  *pq |= ! q;
	  /* Refer to every declared value, to avoid compiler optimizations.  */
	  return (!a + !b + !c + !d + !e + !f + !g + !h + !i + !!j + !k + !!l
		  + !m + !n + !o + !p + !q + !pq);
	],
	[ac_cv_header_stdbool_h=yes],
	[ac_cv_header_stdbool_h=no])])
   AC_CHECK_TYPES([_Bool])
   if test $ac_cv_header_stdbool_h = yes; then
     AC_DEFINE([HAVE_STDBOOL_H], [1], [Define to 1 if stdbool.h conforms to C99.])
   fi])

# stdint.m4 serial 33
dnl Copyright (C) 2001-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Paul Eggert and Bruno Haible.
dnl Test whether <stdint.h> is supported or must be substituted.

AC_DEFUN([gl_STDINT_H],
[
  AC_PREREQ([2.59])dnl

  dnl Check for long long int and unsigned long long int.
  AC_REQUIRE([AC_TYPE_LONG_LONG_INT])
  if test $ac_cv_type_long_long_int = yes; then
    HAVE_LONG_LONG_INT=1
  else
    HAVE_LONG_LONG_INT=0
  fi
  AC_SUBST([HAVE_LONG_LONG_INT])
  AC_REQUIRE([AC_TYPE_UNSIGNED_LONG_LONG_INT])
  if test $ac_cv_type_unsigned_long_long_int = yes; then
    HAVE_UNSIGNED_LONG_LONG_INT=1
  else
    HAVE_UNSIGNED_LONG_LONG_INT=0
  fi
  AC_SUBST([HAVE_UNSIGNED_LONG_LONG_INT])

  dnl Check for <inttypes.h>.
  dnl AC_INCLUDES_DEFAULT defines $ac_cv_header_inttypes_h.
  if test $ac_cv_header_inttypes_h = yes; then
    HAVE_INTTYPES_H=1
  else
    HAVE_INTTYPES_H=0
  fi
  AC_SUBST([HAVE_INTTYPES_H])

  dnl Check for <sys/types.h>.
  dnl AC_INCLUDES_DEFAULT defines $ac_cv_header_sys_types_h.
  if test $ac_cv_header_sys_types_h = yes; then
    HAVE_SYS_TYPES_H=1
  else
    HAVE_SYS_TYPES_H=0
  fi
  AC_SUBST([HAVE_SYS_TYPES_H])

  gl_CHECK_NEXT_HEADERS([stdint.h])
  if test $ac_cv_header_stdint_h = yes; then
    HAVE_STDINT_H=1
  else
    HAVE_STDINT_H=0
  fi
  AC_SUBST([HAVE_STDINT_H])

  dnl Now see whether we need a substitute <stdint.h>.
  if test $ac_cv_header_stdint_h = yes; then
    AC_CACHE_CHECK([whether stdint.h conforms to C99],
      [gl_cv_header_working_stdint_h],
      [gl_cv_header_working_stdint_h=no
       AC_COMPILE_IFELSE([
         AC_LANG_PROGRAM([[
#define __STDC_LIMIT_MACROS 1 /* to make it work also in C++ mode */
#define __STDC_CONSTANT_MACROS 1 /* to make it work also in C++ mode */
#define _GL_JUST_INCLUDE_SYSTEM_STDINT_H 1 /* work if build isn't clean */
#include <stdint.h>
/* Dragonfly defines WCHAR_MIN, WCHAR_MAX only in <wchar.h>.  */
#if !(defined WCHAR_MIN && defined WCHAR_MAX)
#error "WCHAR_MIN, WCHAR_MAX not defined in <stdint.h>"
#endif
]
gl_STDINT_INCLUDES
[
#ifdef INT8_MAX
int8_t a1 = INT8_MAX;
int8_t a1min = INT8_MIN;
#endif
#ifdef INT16_MAX
int16_t a2 = INT16_MAX;
int16_t a2min = INT16_MIN;
#endif
#ifdef INT32_MAX
int32_t a3 = INT32_MAX;
int32_t a3min = INT32_MIN;
#endif
#ifdef INT64_MAX
int64_t a4 = INT64_MAX;
int64_t a4min = INT64_MIN;
#endif
#ifdef UINT8_MAX
uint8_t b1 = UINT8_MAX;
#else
typedef int b1[(unsigned char) -1 != 255 ? 1 : -1];
#endif
#ifdef UINT16_MAX
uint16_t b2 = UINT16_MAX;
#endif
#ifdef UINT32_MAX
uint32_t b3 = UINT32_MAX;
#endif
#ifdef UINT64_MAX
uint64_t b4 = UINT64_MAX;
#endif
int_least8_t c1 = INT8_C (0x7f);
int_least8_t c1max = INT_LEAST8_MAX;
int_least8_t c1min = INT_LEAST8_MIN;
int_least16_t c2 = INT16_C (0x7fff);
int_least16_t c2max = INT_LEAST16_MAX;
int_least16_t c2min = INT_LEAST16_MIN;
int_least32_t c3 = INT32_C (0x7fffffff);
int_least32_t c3max = INT_LEAST32_MAX;
int_least32_t c3min = INT_LEAST32_MIN;
int_least64_t c4 = INT64_C (0x7fffffffffffffff);
int_least64_t c4max = INT_LEAST64_MAX;
int_least64_t c4min = INT_LEAST64_MIN;
uint_least8_t d1 = UINT8_C (0xff);
uint_least8_t d1max = UINT_LEAST8_MAX;
uint_least16_t d2 = UINT16_C (0xffff);
uint_least16_t d2max = UINT_LEAST16_MAX;
uint_least32_t d3 = UINT32_C (0xffffffff);
uint_least32_t d3max = UINT_LEAST32_MAX;
uint_least64_t d4 = UINT64_C (0xffffffffffffffff);
uint_least64_t d4max = UINT_LEAST64_MAX;
int_fast8_t e1 = INT_FAST8_MAX;
int_fast8_t e1min = INT_FAST8_MIN;
int_fast16_t e2 = INT_FAST16_MAX;
int_fast16_t e2min = INT_FAST16_MIN;
int_fast32_t e3 = INT_FAST32_MAX;
int_fast32_t e3min = INT_FAST32_MIN;
int_fast64_t e4 = INT_FAST64_MAX;
int_fast64_t e4min = INT_FAST64_MIN;
uint_fast8_t f1 = UINT_FAST8_MAX;
uint_fast16_t f2 = UINT_FAST16_MAX;
uint_fast32_t f3 = UINT_FAST32_MAX;
uint_fast64_t f4 = UINT_FAST64_MAX;
#ifdef INTPTR_MAX
intptr_t g = INTPTR_MAX;
intptr_t gmin = INTPTR_MIN;
#endif
#ifdef UINTPTR_MAX
uintptr_t h = UINTPTR_MAX;
#endif
intmax_t i = INTMAX_MAX;
uintmax_t j = UINTMAX_MAX;

#include <limits.h> /* for CHAR_BIT */
#define TYPE_MINIMUM(t) \
  ((t) ((t) 0 < (t) -1 ? (t) 0 : ~ (t) 0 << (sizeof (t) * CHAR_BIT - 1)))
#define TYPE_MAXIMUM(t) \
  ((t) ((t) 0 < (t) -1 ? (t) -1 : ~ (~ (t) 0 << (sizeof (t) * CHAR_BIT - 1))))
struct s {
  int check_PTRDIFF:
      PTRDIFF_MIN == TYPE_MINIMUM (ptrdiff_t)
      && PTRDIFF_MAX == TYPE_MAXIMUM (ptrdiff_t)
      ? 1 : -1;
  /* Detect bug in FreeBSD 6.0 / ia64.  */
  int check_SIG_ATOMIC:
      SIG_ATOMIC_MIN == TYPE_MINIMUM (sig_atomic_t)
      && SIG_ATOMIC_MAX == TYPE_MAXIMUM (sig_atomic_t)
      ? 1 : -1;
  int check_SIZE: SIZE_MAX == TYPE_MAXIMUM (size_t) ? 1 : -1;
  int check_WCHAR:
      WCHAR_MIN == TYPE_MINIMUM (wchar_t)
      && WCHAR_MAX == TYPE_MAXIMUM (wchar_t)
      ? 1 : -1;
  /* Detect bug in mingw.  */
  int check_WINT:
      WINT_MIN == TYPE_MINIMUM (wint_t)
      && WINT_MAX == TYPE_MAXIMUM (wint_t)
      ? 1 : -1;

  /* Detect bugs in glibc 2.4 and Solaris 10 stdint.h, among others.  */
  int check_UINT8_C:
        (-1 < UINT8_C (0)) == (-1 < (uint_least8_t) 0) ? 1 : -1;
  int check_UINT16_C:
        (-1 < UINT16_C (0)) == (-1 < (uint_least16_t) 0) ? 1 : -1;

  /* Detect bugs in OpenBSD 3.9 stdint.h.  */
#ifdef UINT8_MAX
  int check_uint8: (uint8_t) -1 == UINT8_MAX ? 1 : -1;
#endif
#ifdef UINT16_MAX
  int check_uint16: (uint16_t) -1 == UINT16_MAX ? 1 : -1;
#endif
#ifdef UINT32_MAX
  int check_uint32: (uint32_t) -1 == UINT32_MAX ? 1 : -1;
#endif
#ifdef UINT64_MAX
  int check_uint64: (uint64_t) -1 == UINT64_MAX ? 1 : -1;
#endif
  int check_uint_least8: (uint_least8_t) -1 == UINT_LEAST8_MAX ? 1 : -1;
  int check_uint_least16: (uint_least16_t) -1 == UINT_LEAST16_MAX ? 1 : -1;
  int check_uint_least32: (uint_least32_t) -1 == UINT_LEAST32_MAX ? 1 : -1;
  int check_uint_least64: (uint_least64_t) -1 == UINT_LEAST64_MAX ? 1 : -1;
  int check_uint_fast8: (uint_fast8_t) -1 == UINT_FAST8_MAX ? 1 : -1;
  int check_uint_fast16: (uint_fast16_t) -1 == UINT_FAST16_MAX ? 1 : -1;
  int check_uint_fast32: (uint_fast32_t) -1 == UINT_FAST32_MAX ? 1 : -1;
  int check_uint_fast64: (uint_fast64_t) -1 == UINT_FAST64_MAX ? 1 : -1;
  int check_uintptr: (uintptr_t) -1 == UINTPTR_MAX ? 1 : -1;
  int check_uintmax: (uintmax_t) -1 == UINTMAX_MAX ? 1 : -1;
  int check_size: (size_t) -1 == SIZE_MAX ? 1 : -1;
};
         ]])],
         [gl_cv_header_working_stdint_h=yes])])
  fi
  if test "$gl_cv_header_working_stdint_h" = yes; then
    STDINT_H=
  else
    dnl Check for <sys/inttypes.h>, and for
    dnl <sys/bitypes.h> (used in Linux libc4 >= 4.6.7 and libc5).
    AC_CHECK_HEADERS([sys/inttypes.h sys/bitypes.h])
    if test $ac_cv_header_sys_inttypes_h = yes; then
      HAVE_SYS_INTTYPES_H=1
    else
      HAVE_SYS_INTTYPES_H=0
    fi
    AC_SUBST([HAVE_SYS_INTTYPES_H])
    if test $ac_cv_header_sys_bitypes_h = yes; then
      HAVE_SYS_BITYPES_H=1
    else
      HAVE_SYS_BITYPES_H=0
    fi
    AC_SUBST([HAVE_SYS_BITYPES_H])

    dnl Check for <wchar.h> (missing in Linux uClibc when built without wide
    dnl character support).
    AC_CHECK_HEADERS_ONCE([wchar.h])

    gl_STDINT_TYPE_PROPERTIES
    STDINT_H=stdint.h
  fi
  AC_SUBST([STDINT_H])
])

dnl gl_STDINT_BITSIZEOF(TYPES, INCLUDES)
dnl Determine the size of each of the given types in bits.
AC_DEFUN([gl_STDINT_BITSIZEOF],
[
  dnl Use a shell loop, to avoid bloating configure, and
  dnl - extra AH_TEMPLATE calls, so that autoheader knows what to put into
  dnl   config.h.in,
  dnl - extra AC_SUBST calls, so that the right substitutions are made.
  m4_foreach_w([gltype], [$1],
    [AH_TEMPLATE([BITSIZEOF_]translit(gltype,[abcdefghijklmnopqrstuvwxyz ],[ABCDEFGHIJKLMNOPQRSTUVWXYZ_]),
       [Define to the number of bits in type ']gltype['.])])
  for gltype in $1 ; do
    AC_CACHE_CHECK([for bit size of $gltype], [gl_cv_bitsizeof_${gltype}],
      [AC_COMPUTE_INT([result], [sizeof ($gltype) * CHAR_BIT],
         [$2
#include <limits.h>], [result=unknown])
       eval gl_cv_bitsizeof_${gltype}=\$result
      ])
    eval result=\$gl_cv_bitsizeof_${gltype}
    if test $result = unknown; then
      dnl Use a nonempty default, because some compilers, such as IRIX 5 cc,
      dnl do a syntax check even on unused #if conditions and give an error
      dnl on valid C code like this:
      dnl   #if 0
      dnl   # if  > 32
      dnl   # endif
      dnl   #endif
      result=0
    fi
    GLTYPE=`echo "$gltype" | tr 'abcdefghijklmnopqrstuvwxyz ' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_'`
    AC_DEFINE_UNQUOTED([BITSIZEOF_${GLTYPE}], [$result])
    eval BITSIZEOF_${GLTYPE}=\$result
  done
  m4_foreach_w([gltype], [$1],
    [AC_SUBST([BITSIZEOF_]translit(gltype,[abcdefghijklmnopqrstuvwxyz ],[ABCDEFGHIJKLMNOPQRSTUVWXYZ_]))])
])

dnl gl_CHECK_TYPES_SIGNED(TYPES, INCLUDES)
dnl Determine the signedness of each of the given types.
dnl Define HAVE_SIGNED_TYPE if type is signed.
AC_DEFUN([gl_CHECK_TYPES_SIGNED],
[
  dnl Use a shell loop, to avoid bloating configure, and
  dnl - extra AH_TEMPLATE calls, so that autoheader knows what to put into
  dnl   config.h.in,
  dnl - extra AC_SUBST calls, so that the right substitutions are made.
  m4_foreach_w([gltype], [$1],
    [AH_TEMPLATE([HAVE_SIGNED_]translit(gltype,[abcdefghijklmnopqrstuvwxyz ],[ABCDEFGHIJKLMNOPQRSTUVWXYZ_]),
       [Define to 1 if ']gltype[' is a signed integer type.])])
  for gltype in $1 ; do
    AC_CACHE_CHECK([whether $gltype is signed], [gl_cv_type_${gltype}_signed],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([$2[
            int verify[2 * (($gltype) -1 < ($gltype) 0) - 1];]])],
         result=yes, result=no)
       eval gl_cv_type_${gltype}_signed=\$result
      ])
    eval result=\$gl_cv_type_${gltype}_signed
    GLTYPE=`echo $gltype | tr 'abcdefghijklmnopqrstuvwxyz ' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_'`
    if test "$result" = yes; then
      AC_DEFINE_UNQUOTED([HAVE_SIGNED_${GLTYPE}], [1])
      eval HAVE_SIGNED_${GLTYPE}=1
    else
      eval HAVE_SIGNED_${GLTYPE}=0
    fi
  done
  m4_foreach_w([gltype], [$1],
    [AC_SUBST([HAVE_SIGNED_]translit(gltype,[abcdefghijklmnopqrstuvwxyz ],[ABCDEFGHIJKLMNOPQRSTUVWXYZ_]))])
])

dnl gl_INTEGER_TYPE_SUFFIX(TYPES, INCLUDES)
dnl Determine the suffix to use for integer constants of the given types.
dnl Define t_SUFFIX for each such type.
AC_DEFUN([gl_INTEGER_TYPE_SUFFIX],
[
  dnl Use a shell loop, to avoid bloating configure, and
  dnl - extra AH_TEMPLATE calls, so that autoheader knows what to put into
  dnl   config.h.in,
  dnl - extra AC_SUBST calls, so that the right substitutions are made.
  m4_foreach_w([gltype], [$1],
    [AH_TEMPLATE(translit(gltype,[abcdefghijklmnopqrstuvwxyz ],[ABCDEFGHIJKLMNOPQRSTUVWXYZ_])[_SUFFIX],
       [Define to l, ll, u, ul, ull, etc., as suitable for
        constants of type ']gltype['.])])
  for gltype in $1 ; do
    AC_CACHE_CHECK([for $gltype integer literal suffix],
      [gl_cv_type_${gltype}_suffix],
      [eval gl_cv_type_${gltype}_suffix=no
       eval result=\$gl_cv_type_${gltype}_signed
       if test "$result" = yes; then
         glsufu=
       else
         glsufu=u
       fi
       for glsuf in "$glsufu" ${glsufu}l ${glsufu}ll ${glsufu}i64; do
         case $glsuf in
           '')  gltype1='int';;
           l)	gltype1='long int';;
           ll)	gltype1='long long int';;
           i64)	gltype1='__int64';;
           u)	gltype1='unsigned int';;
           ul)	gltype1='unsigned long int';;
           ull)	gltype1='unsigned long long int';;
           ui64)gltype1='unsigned __int64';;
         esac
         AC_COMPILE_IFELSE(
           [AC_LANG_PROGRAM([$2[
              extern $gltype foo;
              extern $gltype1 foo;]])],
           [eval gl_cv_type_${gltype}_suffix=\$glsuf])
         eval result=\$gl_cv_type_${gltype}_suffix
         test "$result" != no && break
       done])
    GLTYPE=`echo $gltype | tr 'abcdefghijklmnopqrstuvwxyz ' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_'`
    eval result=\$gl_cv_type_${gltype}_suffix
    test "$result" = no && result=
    eval ${GLTYPE}_SUFFIX=\$result
    AC_DEFINE_UNQUOTED([${GLTYPE}_SUFFIX], [$result])
  done
  m4_foreach_w([gltype], [$1],
    [AC_SUBST(translit(gltype,[abcdefghijklmnopqrstuvwxyz ],[ABCDEFGHIJKLMNOPQRSTUVWXYZ_])[_SUFFIX])])
])

dnl gl_STDINT_INCLUDES
AC_DEFUN([gl_STDINT_INCLUDES],
[[
  /* BSD/OS 4.0.1 has a bug: <stddef.h>, <stdio.h> and <time.h> must be
     included before <wchar.h>.  */
  #include <stddef.h>
  #include <signal.h>
  #if HAVE_WCHAR_H
  # include <stdio.h>
  # include <time.h>
  # include <wchar.h>
  #endif
]])

dnl gl_STDINT_TYPE_PROPERTIES
dnl Compute HAVE_SIGNED_t, BITSIZEOF_t and t_SUFFIX, for all the types t
dnl of interest to stdint.in.h.
AC_DEFUN([gl_STDINT_TYPE_PROPERTIES],
[
  AC_REQUIRE([gl_MULTIARCH])
  if test $APPLE_UNIVERSAL_BUILD = 0; then
    gl_STDINT_BITSIZEOF([ptrdiff_t size_t],
      [gl_STDINT_INCLUDES])
  fi
  gl_STDINT_BITSIZEOF([sig_atomic_t wchar_t wint_t],
    [gl_STDINT_INCLUDES])
  gl_CHECK_TYPES_SIGNED([sig_atomic_t wchar_t wint_t],
    [gl_STDINT_INCLUDES])
  gl_cv_type_ptrdiff_t_signed=yes
  gl_cv_type_size_t_signed=no
  if test $APPLE_UNIVERSAL_BUILD = 0; then
    gl_INTEGER_TYPE_SUFFIX([ptrdiff_t size_t],
      [gl_STDINT_INCLUDES])
  fi
  gl_INTEGER_TYPE_SUFFIX([sig_atomic_t wchar_t wint_t],
    [gl_STDINT_INCLUDES])
])

dnl Autoconf >= 2.61 has AC_COMPUTE_INT built-in.
dnl Remove this when we can assume autoconf >= 2.61.
m4_ifdef([AC_COMPUTE_INT], [], [
  AC_DEFUN([AC_COMPUTE_INT], [_AC_COMPUTE_INT([$2],[$1],[$3],[$4])])
])

# Hey Emacs!
# Local Variables:
# indent-tabs-mode: nil
# End:

# stdlib_h.m4 serial 13
dnl Copyright (C) 2007, 2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_STDLIB_H],
[
  AC_REQUIRE([gl_STDLIB_H_DEFAULTS])
  gl_CHECK_NEXT_HEADERS([stdlib.h])
  AC_CHECK_TYPES([struct random_data],
    [], [HAVE_STRUCT_RANDOM_DATA=0],
    [[#include <stdlib.h>]])
])

AC_DEFUN([gl_STDLIB_MODULE_INDICATOR],
[
  dnl Use AC_REQUIRE here, so that the default settings are expanded once only.
  AC_REQUIRE([gl_STDLIB_H_DEFAULTS])
  GNULIB_[]m4_translit([$1],[abcdefghijklmnopqrstuvwxyz./-],[ABCDEFGHIJKLMNOPQRSTUVWXYZ___])=1
])

AC_DEFUN([gl_STDLIB_H_DEFAULTS],
[
  GNULIB_MALLOC_POSIX=0;  AC_SUBST([GNULIB_MALLOC_POSIX])
  GNULIB_REALLOC_POSIX=0; AC_SUBST([GNULIB_REALLOC_POSIX])
  GNULIB_CALLOC_POSIX=0;  AC_SUBST([GNULIB_CALLOC_POSIX])
  GNULIB_ATOLL=0;         AC_SUBST([GNULIB_ATOLL])
  GNULIB_GETLOADAVG=0;    AC_SUBST([GNULIB_GETLOADAVG])
  GNULIB_GETSUBOPT=0;     AC_SUBST([GNULIB_GETSUBOPT])
  GNULIB_MKDTEMP=0;       AC_SUBST([GNULIB_MKDTEMP])
  GNULIB_MKSTEMP=0;       AC_SUBST([GNULIB_MKSTEMP])
  GNULIB_PUTENV=0;        AC_SUBST([GNULIB_PUTENV])
  GNULIB_RANDOM_R=0;      AC_SUBST([GNULIB_RANDOM_R])
  GNULIB_RPMATCH=0;       AC_SUBST([GNULIB_RPMATCH])
  GNULIB_SETENV=0;        AC_SUBST([GNULIB_SETENV])
  GNULIB_STRTOD=0;        AC_SUBST([GNULIB_STRTOD])
  GNULIB_STRTOLL=0;       AC_SUBST([GNULIB_STRTOLL])
  GNULIB_STRTOULL=0;      AC_SUBST([GNULIB_STRTOULL])
  GNULIB_UNSETENV=0;      AC_SUBST([GNULIB_UNSETENV])
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_ATOLL=1;              AC_SUBST([HAVE_ATOLL])
  HAVE_CALLOC_POSIX=1;       AC_SUBST([HAVE_CALLOC_POSIX])
  HAVE_GETSUBOPT=1;          AC_SUBST([HAVE_GETSUBOPT])
  HAVE_MALLOC_POSIX=1;       AC_SUBST([HAVE_MALLOC_POSIX])
  HAVE_MKDTEMP=1;            AC_SUBST([HAVE_MKDTEMP])
  HAVE_REALLOC_POSIX=1;      AC_SUBST([HAVE_REALLOC_POSIX])
  HAVE_RANDOM_R=1;           AC_SUBST([HAVE_RANDOM_R])
  HAVE_RPMATCH=1;            AC_SUBST([HAVE_RPMATCH])
  HAVE_SETENV=1;             AC_SUBST([HAVE_SETENV])
  HAVE_STRTOD=1;             AC_SUBST([HAVE_STRTOD])
  HAVE_STRTOLL=1;            AC_SUBST([HAVE_STRTOLL])
  HAVE_STRTOULL=1;           AC_SUBST([HAVE_STRTOULL])
  HAVE_STRUCT_RANDOM_DATA=1; AC_SUBST([HAVE_STRUCT_RANDOM_DATA])
  HAVE_SYS_LOADAVG_H=0;      AC_SUBST([HAVE_SYS_LOADAVG_H])
  HAVE_UNSETENV=1;           AC_SUBST([HAVE_UNSETENV])
  HAVE_DECL_GETLOADAVG=1;    AC_SUBST([HAVE_DECL_GETLOADAVG])
  REPLACE_MKSTEMP=0;         AC_SUBST([REPLACE_MKSTEMP])
  REPLACE_PUTENV=0;          AC_SUBST([REPLACE_PUTENV])
  REPLACE_STRTOD=0;          AC_SUBST([REPLACE_STRTOD])
  VOID_UNSETENV=0;           AC_SUBST([VOID_UNSETENV])
])

# Configure a replacement for <sys/time.h>.

# Copyright (C) 2007 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# Written by Paul Eggert and Martin Lambers.

AC_DEFUN([gl_HEADER_SYS_TIME_H],
[
  dnl Use AC_REQUIRE here, so that the REPLACE_GETTIMEOFDAY=0 statement
  dnl below is expanded once only, before all REPLACE_GETTIMEOFDAY=1
  dnl statements that occur in other macros.
  AC_REQUIRE([gl_HEADER_SYS_TIME_H_BODY])
])

AC_DEFUN([gl_HEADER_SYS_TIME_H_BODY],
[
  AC_REQUIRE([AC_C_RESTRICT])
  gl_CHECK_NEXT_HEADERS([sys/time.h])

  if test $ac_cv_header_sys_time_h = yes; then
    HAVE_SYS_TIME_H=1
  else
    HAVE_SYS_TIME_H=0
  fi
  AC_SUBST([HAVE_SYS_TIME_H])

  AC_CACHE_CHECK([for struct timeval], [gl_cv_sys_struct_timeval],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
	  [[#if HAVE_SYS_TIME_H
	     #include <sys/time.h>
	    #endif
	    #include <time.h>
	  ]],
	  [[static struct timeval x; x.tv_sec = x.tv_usec;]])],
       [gl_cv_sys_struct_timeval=yes],
       [gl_cv_sys_struct_timeval=no])])
  if test $gl_cv_sys_struct_timeval = yes; then
    HAVE_STRUCT_TIMEVAL=1
  else
    HAVE_STRUCT_TIMEVAL=0
  fi
  AC_SUBST([HAVE_STRUCT_TIMEVAL])

  dnl Assume POSIX behavior unless another module says otherwise.
  REPLACE_GETTIMEOFDAY=0
  AC_SUBST([REPLACE_GETTIMEOFDAY])
  if test $HAVE_SYS_TIME_H = 0 || test $HAVE_STRUCT_TIMEVAL = 0; then
    SYS_TIME_H=sys/time.h
  else
    SYS_TIME_H=
  fi
  AC_SUBST([SYS_TIME_H])
])

# unistd_h.m4 serial 16
dnl Copyright (C) 2006-2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl Written by Simon Josefsson, Bruno Haible.

AC_DEFUN([gl_UNISTD_H],
[
  dnl Use AC_REQUIRE here, so that the default behavior below is expanded
  dnl once only, before all statements that occur in other macros.
  AC_REQUIRE([gl_UNISTD_H_DEFAULTS])

  gl_CHECK_NEXT_HEADERS([unistd.h])

  AC_CHECK_HEADERS_ONCE([unistd.h])
  if test $ac_cv_header_unistd_h = yes; then
    HAVE_UNISTD_H=1
  else
    HAVE_UNISTD_H=0
  fi
  AC_SUBST([HAVE_UNISTD_H])
])

AC_DEFUN([gl_UNISTD_MODULE_INDICATOR],
[
  dnl Use AC_REQUIRE here, so that the default settings are expanded once only.
  AC_REQUIRE([gl_UNISTD_H_DEFAULTS])
  GNULIB_[]m4_translit([$1],[abcdefghijklmnopqrstuvwxyz./-],[ABCDEFGHIJKLMNOPQRSTUVWXYZ___])=1
])

AC_DEFUN([gl_UNISTD_H_DEFAULTS],
[
  GNULIB_CHOWN=0;            AC_SUBST([GNULIB_CHOWN])
  GNULIB_CLOSE=0;            AC_SUBST([GNULIB_CLOSE])
  GNULIB_DUP2=0;             AC_SUBST([GNULIB_DUP2])
  GNULIB_ENVIRON=0;          AC_SUBST([GNULIB_ENVIRON])
  GNULIB_EUIDACCESS=0;       AC_SUBST([GNULIB_EUIDACCESS])
  GNULIB_FCHDIR=0;           AC_SUBST([GNULIB_FCHDIR])
  GNULIB_FSYNC=0;            AC_SUBST([GNULIB_FSYNC])
  GNULIB_FTRUNCATE=0;        AC_SUBST([GNULIB_FTRUNCATE])
  GNULIB_GETCWD=0;           AC_SUBST([GNULIB_GETCWD])
  GNULIB_GETDOMAINNAME=0;    AC_SUBST([GNULIB_GETDOMAINNAME])
  GNULIB_GETDTABLESIZE=0;    AC_SUBST([GNULIB_GETDTABLESIZE])
  GNULIB_GETHOSTNAME=0;      AC_SUBST([GNULIB_GETHOSTNAME])
  GNULIB_GETLOGIN_R=0;       AC_SUBST([GNULIB_GETLOGIN_R])
  GNULIB_GETPAGESIZE=0;      AC_SUBST([GNULIB_GETPAGESIZE])
  GNULIB_GETUSERSHELL=0;     AC_SUBST([GNULIB_GETUSERSHELL])
  GNULIB_LCHOWN=0;           AC_SUBST([GNULIB_LCHOWN])
  GNULIB_LSEEK=0;            AC_SUBST([GNULIB_LSEEK])
  GNULIB_READLINK=0;         AC_SUBST([GNULIB_READLINK])
  GNULIB_SLEEP=0;            AC_SUBST([GNULIB_SLEEP])
  GNULIB_UNISTD_H_SIGPIPE=0; AC_SUBST([GNULIB_UNISTD_H_SIGPIPE])
  GNULIB_WRITE=0;            AC_SUBST([GNULIB_WRITE])
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_DUP2=1;            AC_SUBST([HAVE_DUP2])
  HAVE_EUIDACCESS=1;      AC_SUBST([HAVE_EUIDACCESS])
  HAVE_FSYNC=1;           AC_SUBST([HAVE_FSYNC])
  HAVE_FTRUNCATE=1;       AC_SUBST([HAVE_FTRUNCATE])
  HAVE_GETDOMAINNAME=1;   AC_SUBST([HAVE_GETDOMAINNAME])
  HAVE_GETDTABLESIZE=1;   AC_SUBST([HAVE_GETDTABLESIZE])
  HAVE_GETHOSTNAME=1;     AC_SUBST([HAVE_GETHOSTNAME])
  HAVE_GETPAGESIZE=1;     AC_SUBST([HAVE_GETPAGESIZE])
  HAVE_GETUSERSHELL=1;    AC_SUBST([HAVE_GETUSERSHELL])
  HAVE_READLINK=1;        AC_SUBST([HAVE_READLINK])
  HAVE_SLEEP=1;           AC_SUBST([HAVE_SLEEP])
  HAVE_DECL_ENVIRON=1;    AC_SUBST([HAVE_DECL_ENVIRON])
  HAVE_DECL_GETLOGIN_R=1; AC_SUBST([HAVE_DECL_GETLOGIN_R])
  HAVE_OS_H=0;            AC_SUBST([HAVE_OS_H])
  HAVE_SYS_PARAM_H=0;     AC_SUBST([HAVE_SYS_PARAM_H])
  REPLACE_CHOWN=0;        AC_SUBST([REPLACE_CHOWN])
  REPLACE_CLOSE=0;        AC_SUBST([REPLACE_CLOSE])
  REPLACE_FCHDIR=0;       AC_SUBST([REPLACE_FCHDIR])
  REPLACE_GETCWD=0;       AC_SUBST([REPLACE_GETCWD])
  REPLACE_GETPAGESIZE=0;  AC_SUBST([REPLACE_GETPAGESIZE])
  REPLACE_LCHOWN=0;       AC_SUBST([REPLACE_LCHOWN])
  REPLACE_LSEEK=0;        AC_SUBST([REPLACE_LSEEK])
  REPLACE_WRITE=0;        AC_SUBST([REPLACE_WRITE])
  UNISTD_H_HAVE_WINSOCK2_H=0; AC_SUBST([UNISTD_H_HAVE_WINSOCK2_H])
])

dnl A placeholder for ISO C99 <wchar.h>, for platforms that have issues.

dnl Copyright (C) 2007-2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl Written by Eric Blake.

# wchar.m4 serial 22

AC_DEFUN([gl_WCHAR_H],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])
  AC_CACHE_CHECK([whether <wchar.h> is standalone],
    [gl_cv_header_wchar_h_standalone],
    [AC_COMPILE_IFELSE([[#include <wchar.h>
wchar_t w;]],
      [gl_cv_header_wchar_h_standalone=yes],
      [gl_cv_header_wchar_h_standalone=no])])

  AC_REQUIRE([gt_TYPE_WINT_T])
  if test $gt_cv_c_wint_t = yes; then
    HAVE_WINT_T=1
  else
    HAVE_WINT_T=0
  fi
  AC_SUBST([HAVE_WINT_T])

  if test $gl_cv_header_wchar_h_standalone != yes || test $gt_cv_c_wint_t != yes; then
    WCHAR_H=wchar.h
  fi

  dnl Prepare for creating substitute <wchar.h>.
  dnl Do it always: WCHAR_H may be empty here but can be set later.
  dnl Check for <wchar.h> (missing in Linux uClibc when built without wide
  dnl character support).
  AC_CHECK_HEADERS_ONCE([wchar.h])
  if test $ac_cv_header_wchar_h = yes; then
    HAVE_WCHAR_H=1
  else
    HAVE_WCHAR_H=0
  fi
  AC_SUBST([HAVE_WCHAR_H])
  gl_CHECK_NEXT_HEADERS([wchar.h])
])

dnl Unconditionally enables the replacement of <wchar.h>.
AC_DEFUN([gl_REPLACE_WCHAR_H],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])
  WCHAR_H=wchar.h
])

AC_DEFUN([gl_WCHAR_MODULE_INDICATOR],
[
  dnl Use AC_REQUIRE here, so that the default settings are expanded once only.
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])
  GNULIB_[]m4_translit([$1],[abcdefghijklmnopqrstuvwxyz./-],[ABCDEFGHIJKLMNOPQRSTUVWXYZ___])=1
])

AC_DEFUN([gl_WCHAR_H_DEFAULTS],
[
  GNULIB_BTOWC=0;      AC_SUBST([GNULIB_BTOWC])
  GNULIB_WCTOB=0;      AC_SUBST([GNULIB_WCTOB])
  GNULIB_MBSINIT=0;    AC_SUBST([GNULIB_MBSINIT])
  GNULIB_MBRTOWC=0;    AC_SUBST([GNULIB_MBRTOWC])
  GNULIB_MBRLEN=0;     AC_SUBST([GNULIB_MBRLEN])
  GNULIB_MBSRTOWCS=0;  AC_SUBST([GNULIB_MBSRTOWCS])
  GNULIB_MBSNRTOWCS=0; AC_SUBST([GNULIB_MBSNRTOWCS])
  GNULIB_WCRTOMB=0;    AC_SUBST([GNULIB_WCRTOMB])
  GNULIB_WCSRTOMBS=0;  AC_SUBST([GNULIB_WCSRTOMBS])
  GNULIB_WCSNRTOMBS=0; AC_SUBST([GNULIB_WCSNRTOMBS])
  GNULIB_WCWIDTH=0;    AC_SUBST([GNULIB_WCWIDTH])
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_BTOWC=1;        AC_SUBST([HAVE_BTOWC])
  HAVE_MBSINIT=1;      AC_SUBST([HAVE_MBSINIT])
  HAVE_MBRTOWC=1;      AC_SUBST([HAVE_MBRTOWC])
  HAVE_MBRLEN=1;       AC_SUBST([HAVE_MBRLEN])
  HAVE_MBSRTOWCS=1;    AC_SUBST([HAVE_MBSRTOWCS])
  HAVE_MBSNRTOWCS=1;   AC_SUBST([HAVE_MBSNRTOWCS])
  HAVE_WCRTOMB=1;      AC_SUBST([HAVE_WCRTOMB])
  HAVE_WCSRTOMBS=1;    AC_SUBST([HAVE_WCSRTOMBS])
  HAVE_WCSNRTOMBS=1;   AC_SUBST([HAVE_WCSNRTOMBS])
  HAVE_DECL_WCTOB=1;   AC_SUBST([HAVE_DECL_WCTOB])
  HAVE_DECL_WCWIDTH=1; AC_SUBST([HAVE_DECL_WCWIDTH])
  REPLACE_MBSTATE_T=0; AC_SUBST([REPLACE_MBSTATE_T])
  REPLACE_BTOWC=0;     AC_SUBST([REPLACE_BTOWC])
  REPLACE_WCTOB=0;     AC_SUBST([REPLACE_WCTOB])
  REPLACE_MBSINIT=0;   AC_SUBST([REPLACE_MBSINIT])
  REPLACE_MBRTOWC=0;   AC_SUBST([REPLACE_MBRTOWC])
  REPLACE_MBRLEN=0;    AC_SUBST([REPLACE_MBRLEN])
  REPLACE_MBSRTOWCS=0; AC_SUBST([REPLACE_MBSRTOWCS])
  REPLACE_MBSNRTOWCS=0;AC_SUBST([REPLACE_MBSNRTOWCS])
  REPLACE_WCRTOMB=0;   AC_SUBST([REPLACE_WCRTOMB])
  REPLACE_WCSRTOMBS=0; AC_SUBST([REPLACE_WCSRTOMBS])
  REPLACE_WCWIDTH=0;   AC_SUBST([REPLACE_WCWIDTH])
  WCHAR_H='';          AC_SUBST([WCHAR_H])
])

# wcrtomb.m4 serial 2
dnl Copyright (C) 2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_WCRTOMB],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])

  AC_REQUIRE([AC_TYPE_MBSTATE_T])
  AC_CHECK_FUNCS_ONCE([wcrtomb])
  if test $ac_cv_func_wcrtomb = no; then
    HAVE_WCRTOMB=0
  else

    dnl On OSF/1 5.1 and Solaris 10, wcrtomb (NULL, 0, NULL) sometimes
    dnl returns 0 instead of 1.
    AC_REQUIRE([AC_PROG_CC])
    AC_REQUIRE([gt_LOCALE_FR])
    AC_REQUIRE([gt_LOCALE_FR_UTF8])
    AC_REQUIRE([gt_LOCALE_JA])
    AC_REQUIRE([gt_LOCALE_ZH_CN])
    AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
    AC_CACHE_CHECK([whether wcrtomb return value is correct],
      [gl_cv_func_wcrtomb_retval],
      [
        dnl Initial guess, used when cross-compiling or when no suitable locale
        dnl is present.
changequote(,)dnl
        case "$host_os" in
                           # Guess no on OSF/1 and Solaris.
          osf* | solaris*) gl_cv_func_wcrtomb_retval="guessing no" ;;
                           # Guess yes otherwise.
          *)               gl_cv_func_wcrtomb_retval="guessing yes" ;;
        esac
changequote([,])dnl
        if test $LOCALE_FR != none || test $LOCALE_FR_UTF8 != none || test $LOCALE_JA != none || test $LOCALE_ZH_CN != none; then
          AC_TRY_RUN([
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>
int main ()
{
  if (setlocale (LC_ALL, "$LOCALE_FR") != NULL)
    {
      if (wcrtomb (NULL, 0, NULL) != 1)
        return 1;
    }
  if (setlocale (LC_ALL, "$LOCALE_FR_UTF8") != NULL)
    {
      if (wcrtomb (NULL, 0, NULL) != 1)
        return 1;
    }
  if (setlocale (LC_ALL, "$LOCALE_JA") != NULL)
    {
      if (wcrtomb (NULL, 0, NULL) != 1)
        return 1;
    }
  if (setlocale (LC_ALL, "$LOCALE_ZH_CN") != NULL)
    {
      if (wcrtomb (NULL, 0, NULL) != 1)
        return 1;
    }
  return 0;
}],
            [gl_cv_func_wcrtomb_retval=yes],
            [gl_cv_func_wcrtomb_retval=no],
            [])
        fi
      ])
    case "$gl_cv_func_wcrtomb_retval" in
      *yes) ;;
      *) REPLACE_WCRTOMB=1 ;;
    esac
  fi
  if test $HAVE_WCRTOMB = 0 || test $REPLACE_WCRTOMB = 1; then
    gl_REPLACE_WCHAR_H
    AC_LIBOBJ([wcrtomb])
    gl_PREREQ_WCRTOMB
  fi
])

# Prerequisites of lib/wcrtomb.c.
AC_DEFUN([gl_PREREQ_WCRTOMB], [
  :
])

# wctype.m4 serial 2

dnl A placeholder for ISO C99 <wctype.h>, for platforms that lack it.

dnl Copyright (C) 2006-2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl Written by Paul Eggert.

AC_DEFUN([gl_WCTYPE_H],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_CHECK_FUNCS_ONCE([iswcntrl])
  if test $ac_cv_func_iswcntrl = yes; then
    HAVE_ISWCNTRL=1
  else
    HAVE_ISWCNTRL=0
  fi
  AC_SUBST([HAVE_ISWCNTRL])
  AC_CHECK_HEADERS_ONCE([wctype.h])
  AC_REQUIRE([AC_C_INLINE])

  AC_REQUIRE([gt_TYPE_WINT_T])
  if test $gt_cv_c_wint_t = yes; then
    HAVE_WINT_T=1
  else
    HAVE_WINT_T=0
  fi
  AC_SUBST([HAVE_WINT_T])

  WCTYPE_H=wctype.h
  if test $ac_cv_header_wctype_h = yes; then
    if test $ac_cv_func_iswcntrl = yes; then
      dnl Linux libc5 has an iswprint function that returns 0 for all arguments.
      dnl The other functions are likely broken in the same way.
      AC_CACHE_CHECK([whether iswcntrl works], [gl_cv_func_iswcntrl_works],
        [
          AC_TRY_RUN([#include <stddef.h>
                      #include <stdio.h>
                      #include <time.h>
                      #include <wchar.h>
                      #include <wctype.h>
                      int main () { return iswprint ('x') == 0; }],
            [gl_cv_func_iswcntrl_works=yes], [gl_cv_func_iswcntrl_works=no],
            [AC_TRY_COMPILE([#include <stdlib.h>
                          #if __GNU_LIBRARY__ == 1
                          Linux libc5 i18n is broken.
                          #endif], [],
              [gl_cv_func_iswcntrl_works=yes], [gl_cv_func_iswcntrl_works=no])
            ])
        ])
      if test $gl_cv_func_iswcntrl_works = yes; then
        WCTYPE_H=
      fi
    fi
    dnl Compute NEXT_WCTYPE_H even if WCTYPE_H is empty,
    dnl for the benefit of builds from non-distclean directories.
    gl_CHECK_NEXT_HEADERS([wctype.h])
    HAVE_WCTYPE_H=1
  else
    HAVE_WCTYPE_H=0
  fi
  AC_SUBST([HAVE_WCTYPE_H])
  AC_SUBST([WCTYPE_H])

  if test "$gl_cv_func_iswcntrl_works" = no; then
    REPLACE_ISWCNTRL=1
  else
    REPLACE_ISWCNTRL=0
  fi
  AC_SUBST([REPLACE_ISWCNTRL])
])

# wint_t.m4 serial 4 (gettext-0.18)
dnl Copyright (C) 2003, 2007-2009 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.
dnl Test whether <wchar.h> has the 'wint_t' type.
dnl Prerequisite: AC_PROG_CC

AC_DEFUN([gt_TYPE_WINT_T],
[
  AC_CACHE_CHECK([for wint_t], [gt_cv_c_wint_t],
    [AC_TRY_COMPILE([
/* Tru64 with Desktop Toolkit C has a bug: <stdio.h> must be included before
   <wchar.h>.
   BSD/OS 4.0.1 has a bug: <stddef.h>, <stdio.h> and <time.h> must be included
   before <wchar.h>.  */
#include <stddef.h>
#include <stdio.h>
#include <time.h>
#include <wchar.h>
       wint_t foo = (wchar_t)'\0';], ,
       [gt_cv_c_wint_t=yes], [gt_cv_c_wint_t=no])])
  if test $gt_cv_c_wint_t = yes; then
    AC_DEFINE([HAVE_WINT_T], [1], [Define if you have the 'wint_t' type.])
  fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_ADDRESS_RANGE],
[AC_REQUIRE([AC_PROG_CC])dnl
address_range_prog='
#include <stdio.h>
int printf_address (unsigned long addr) {
  FILE* out = fopen("conftest.h","w");
  if (sizeof(unsigned long) <= 4)
    fprintf(out,"0x%08X\n", (unsigned int)addr);
  else
    fprintf(out,"0x%08X%08X\n",(unsigned int)(addr>>32),(unsigned int)(addr&0xFFFFFFFF));
  return ferror(out) || fclose(out);
}
#define chop_address(addr) ((unsigned long)(char*)(addr) & ~0x00FFFFFFL)
'
AC_CACHE_CHECK(for the code address range, cl_cv_address_code, [dnl
AC_RUN_IFELSE([#include "confdefs.h"
$address_range_prog
dnl printf_address(chop_address(&main)); doesn't work in C++.
int main() { return printf_address(chop_address(&printf_address)); }],
[cl_cv_address_code=`cat conftest.h`],[cl_cv_address_code='guessing 0'],
[cl_cv_address_code='guessing 0'])
rm -f conftest.h
])
x=`echo $cl_cv_address_code | sed -e 's,^guessing ,,'`"UL"
AC_DEFINE_UNQUOTED(CODE_ADDRESS_RANGE,$x,[address range of program code (text+data+bss)])
dnl
AC_CACHE_CHECK(for the malloc address range, cl_cv_address_malloc, [dnl
AC_RUN_IFELSE([#include "confdefs.h"
#include <sys/types.h>
/* declare malloc() */
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
$address_range_prog
int main() { return printf_address(chop_address(malloc(10000))); }],
[cl_cv_address_malloc=`cat conftest.h`],[cl_cv_address_malloc='guessing 0'],
[cl_cv_address_malloc='guessing 0'])
rm -f conftest.h
])
x=`echo $cl_cv_address_malloc | sed -e 's,^guessing ,,'`"UL"
AC_DEFINE_UNQUOTED(MALLOC_ADDRESS_RANGE,$x,[address range of malloc() memory])
dnl
AC_CACHE_CHECK(for the shared library address range, cl_cv_address_shlib, [dnl
AC_RUN_IFELSE([#include "confdefs.h"
$address_range_prog
/* Declare printf(). */
#if defined(sun) /* for SunOS 4, but not for IRIX 6 */
#ifdef __cplusplus
extern "C" int printf (const char *, ...);
#else
extern int printf ();
#endif
#endif
/* Declare tmpnam(). */
#ifdef __cplusplus
extern "C" char* tmpnam (char*);
#else
extern char* tmpnam ();
#endif
/* With normal simple DLLs, &printf is in the shared library. Fine.
   But with ELF, &printf is a trampoline function allocated near the
   program's code range. errno and other global variables - such as
   &stdout - are allocated near the program's code and bss as well.
   However, the return value of tmpnam(NULL) is a pointer to a static
   buffer in the shared library. (This buffer is unlikely to be named
   by a global symbol.) */
int main() {
  char* addr;
  addr = (char*) tmpnam((char*)0);
  if (!addr) addr = (char*) &printf;
  return printf_address(chop_address(addr));
}],[cl_cv_address_shlib=`cat conftest.h`],[cl_cv_address_shlib='guessing 0'],
[cl_cv_address_shlib='guessing 0'])
rm -f conftest.h
])
x=`echo $cl_cv_address_shlib | sed -e 's,^guessing ,,'`"UL"
AC_DEFINE_UNQUOTED(SHLIB_ADDRESS_RANGE,$x,[address range of shared library code])

AC_CACHE_CHECK(for the stack address range, cl_cv_address_stack, [dnl
AC_RUN_IFELSE([#include "confdefs.h"
#include "confdefs.h"
$address_range_prog
int main() { int dummy; return printf_address(chop_address(&dummy)); }],
[cl_cv_address_stack=`cat conftest.h`],[cl_cv_address_stack='guessing ~0'],
[cl_cv_address_stack='guessing ~0'])
rm -f conftest.h
])
x=`echo "$cl_cv_address_stack" | sed -e 's,^guessing ,,'`"UL"
AC_DEFINE_UNQUOTED(STACK_ADDRESS_RANGE,$x,[address range of the C stack])
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_AS_UNDERSCORE],
[AC_BEFORE([$0], [CL_GLOBAL_CONSTRUCTORS])
m4_pattern_allow([^AS_UNDERSCORE$])
AC_CACHE_CHECK(for underscore in external names, cl_cv_prog_as_underscore, [
cat > conftest.c <<EOF
#ifdef __cplusplus
extern "C"
#endif
int foo() { return 0; }
EOF
# look for the assembly language name in the .s file
AC_TRY_COMMAND(${CC-cc} -S conftest.c) >/dev/null 2>&1
if grep _foo conftest.s >/dev/null ; then
  cl_cv_prog_as_underscore=yes
else
  cl_cv_prog_as_underscore=no
fi
rm -f conftest*
])
if test $cl_cv_prog_as_underscore = yes; then
  AS_UNDERSCORE=true
  AC_DEFINE(ASM_UNDERSCORE,,[symbols are prefixed by an underscore in assembly language])
else
  AS_UNDERSCORE=false
fi
AC_SUBST(AS_UNDERSCORE)dnl
])

dnl -*- Autoconf -*-
# bold.m4 serial 2 (clisp)
dnl Copyright (C) 1999-2002 Ralf S. Engelschall <rse@engelschall.com>
dnl Copyright (C) 2002-2008 Bruno Haible <bruno@clisp.org>
dnl Copyright (C) 2006 Sam Steingold <sds@gnu.org>
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

# Determine the escape sequences for switching bold output on and off.
AC_DEFUN([RSE_BOLD],
[
  dnl Not pretty.
  dnl AC_REQUIRE([AC_PROG_AWK])

  case $TERM in
    # for the most important terminal types we directly know the sequences
    xterm*|vt220*)
      term_bold=`${AWK:-awk} 'BEGIN { printf("%c%c%c%c", 27, 91, 49, 109); }' </dev/null 2>/dev/null`
      term_norm=`${AWK:-awk} 'BEGIN { printf("%c%c%c", 27, 91, 109); }' </dev/null 2>/dev/null`
      ;;
    vt100*)
      term_bold=`${AWK:-awk} 'BEGIN { printf("%c%c%c%c%c%c", 27, 91, 49, 109, 0, 0); }' </dev/null 2>/dev/null`
      term_norm=`${AWK:-awk} 'BEGIN { printf("%c%c%c%c%c", 27, 91, 109, 0, 0); }' </dev/null 2>/dev/null`
      ;;
    # for all others, we try to use a possibly existing `tput' or `tcout' utility
    *)
      paths=`echo "$PATH" | sed -e 's/:/ /g'`
      for tool in tput tcout; do
        for dir in $paths; do
          if test -r "$dir/$tool"; then
            for seq in bold md smso; do # 'smso' is last
              bold="`$dir/$tool $seq 2>/dev/null`"
              if test -n "$bold"; then
                term_bold="$bold"
                break
              fi
            done
            if test -n "$term_bold"; then
              for seq in sgr0 me rmso reset; do # 'reset' is last
                norm="`$dir/$tool $seq 2>/dev/null`"
                if test -n "$norm"; then
                  term_norm="$norm"
                  break
                fi
              done
            fi
            break
          fi
        done
        if test -n "$term_bold" && test -n "$term_norm"; then
          break
        fi
      done
      ;;
  esac

])
AC_DEFUN([BOLD_MSG],[AC_MSG_NOTICE([${term_bold}** $1${term_norm}])])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_BUILTIN_STRLEN],
[AC_CACHE_CHECK(for inline __builtin_strlen, cl_cv_builtin_strlen, [
cat > conftest.$ac_ext <<EOF
int foo (char* x)
{ return __builtin_strlen(x); }
EOF
if AC_TRY_COMMAND(${CC-cc} -S $CFLAGS $CPPFLAGS conftest.$ac_ext) >/dev/null 2>&1 ; then
  if grep strlen conftest.s >/dev/null ; then
    cl_cv_builtin_strlen=no
  else
    cl_cv_builtin_strlen=yes
  fi
else
  cl_cv_builtin_strlen=no
fi
rm -f conftest*
])
if test $cl_cv_builtin_strlen = yes; then
  AC_DEFINE(HAVE_BUILTIN_STRLEN,,[__builtin_strlen() is compiled inline (not a call to strlen())])
fi
])

dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.61)

AC_DEFUN([CL_CC_CPLUSPLUS],
[AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK(whether using C++, cl_cv_prog_cc_cplusplus, [
AC_EGREP_CPP(yes,[#ifdef __cplusplus
  yes
#endif
], cl_cv_prog_cc_cplusplus=yes, cl_cv_prog_cc_cplusplus=no)
])
if test $cl_cv_prog_cc_cplusplus = yes; then
  CC_CPLUSPLUS=true
else
  CC_CPLUSPLUS=false
fi
AC_SUBST(CC_CPLUSPLUS)dnl
])

AC_DEFUN([CL_CXX_WORKS],
[AC_CACHE_CHECK(whether CXX works at all, cl_cv_prog_cxx_works, [
AC_LANG_PUSH(C++)
AC_RUN_IFELSE([AC_LANG_SOURCE([int main() { exit(0); }])],
[cl_cv_prog_cxx_works=yes], [cl_cv_prog_cxx_works=no],
[AC_LINK_IFELSE([AC_LANG_SOURCE([])], [cl_cv_prog_cxx_works=yes],
[cl_cv_prog_cxx_works=no])])
AC_LANG_POP(C++)
])
if test "$cl_cv_prog_cxx_works" = "no"; then
AC_MSG_FAILURE([Installation or configuration problem: C++ compiler cannot create executables.])
fi
])

AC_DEFUN([CL_TEMPLATE_NULL],
[CL_COMPILE_CHECK([working template<>], cl_cv_c_templatenull,
[template <class T> class c {}; template <> class c<int> { int x; };], ,
AC_DEFINE(HAVE_TEMPLATE_NULL))
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_CADDR_T],
[AC_CACHE_CHECK(for caddr_t in sys/types.h, cl_cv_type_caddr_t, [
AC_EGREP_HEADER(caddr_t, sys/types.h,
cl_cv_type_caddr_t=yes, cl_cv_type_caddr_t=no)
])
if test $cl_cv_type_caddr_t = yes; then
  AC_DEFINE(CADDR_T, caddr_t, [what is your caddr_t type?])
else
  AC_DEFINE(CADDR_T, void*)
fi
])

dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.61)

AC_DEFUN([CL_CC_NEED_DEEMA],
[AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK(whether CPP likes empty macro arguments, cl_cv_prog_cc_ema, [
AC_PREPROC_IFELSE([AC_LANG_SOURCE([#define divide(x,y,q_zuw,r_zuw) (r_zuw(x)-(q_zuw(x)/(y))*(y))
foo(x,y) int x,y; { int q; divide(x,y,q=,); return q; }])],
cl_cv_prog_cc_ema=yes, cl_cv_prog_cc_ema=no)])
if test $cl_cv_prog_cc_ema = yes; then
  CC_NEED_DEEMA=false
else
  CC_NEED_DEEMA=true
fi
AC_SUBST(CC_NEED_DEEMA)dnl
])

dnl Copyright (C) 1993-2002 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels.

AC_PREREQ(2.13)

AC_DEFUN([CL_CC_GCC],
[AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK(whether using GNU C, cl_cv_prog_cc_gcc, [
AC_EGREP_CPP(yes,[#ifdef __GNUC__
  yes
#endif
], cl_cv_prog_cc_gcc=yes, cl_cv_prog_cc_gcc=no)
])
if test $cl_cv_prog_cc_gcc = yes; then
  CC_GCC=true
  GCC_X_NONE='-x none'
else
  CC_GCC=false
  GCC_X_NONE=''
fi
AC_SUBST(CC_GCC)dnl
AC_SUBST(GCC_X_NONE)dnl
])

dnl Copyright (C) 2008 Sam Steingold (GNU GPLv2+)
dnl -*- Autoconf -*-

AC_PREREQ(2.13)

AC_DEFUN([CL_CC_SUNPRO],
[AC_REQUIRE([AC_PROG_CPP])
AC_CACHE_CHECK(whether using SUNPRO C, cl_cv_prog_cc_sunpro, [
AC_EGREP_CPP(yes,[#ifdef __SUNPRO_C
  yes
#endif
], cl_cv_prog_cc_sunpro=yes, cl_cv_prog_cc_sunpro=no)])
if test $cl_cv_prog_cc_sunpro = yes; then
  CC_SUNPRO=true
else
  CC_SUNPRO=false
fi
AC_SUBST(CC_SUNPRO)dnl
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.13)

AC_DEFUN([CL_CLOSEDIR],
[AC_BEFORE([$0], [CL_FILECHARSET])dnl
# The following test is necessary, because Cygwin32 declares closedir()
# as returning int but the return value is unusable.
AC_CACHE_CHECK(for usable closedir return value, cl_cv_func_closedir_retval,[
AC_TRY_RUN([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
/* Declare opendir(), closedir(). */
#include <dirent.h>
int main() { exit(closedir(opendir(".")) != 0); }],
cl_cv_func_closedir_retval=yes, cl_cv_func_closedir_retval=no,
# When cross-compiling, don't assume a return value.
cl_cv_func_closedir_retval="guessing no")])
case "$cl_cv_func_closedir_retval" in
  *no) AC_DEFINE(VOID_CLOSEDIR,,[closedir() return value is void or unusable]) ;;
esac
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2004, 2007-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.13)

AC_DEFUN([CL_CONNECT],
[AC_CHECK_FUNCS(connect)
if test $ac_cv_func_connect = yes; then
CL_PROTO([connect], [
for x in CONST_VARIANTS; do
for y in 'struct sockaddr *' 'void*'; do
for z in SIZE_VARIANTS; do
if test -z "$have_connect_decl"; then
CL_PROTO_TRY([
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
], [int connect (int fd, $x $y name, $z namelen);], [
cl_cv_proto_connect_arg2a="$x"
cl_cv_proto_connect_arg2b="$y"
cl_cv_proto_connect_arg3="$z"
have_connect_decl=1])
fi
done
done
done
if test -z "$have_connect_decl"; then
CL_PROTO_MISSING(connect)
fi
], [extern int connect (int, $cl_cv_proto_connect_arg2a $cl_cv_proto_connect_arg2b, $cl_cv_proto_connect_arg3);])
AC_DEFINE_UNQUOTED(CONNECT_CONST,$cl_cv_proto_connect_arg2a,[does declaration of connect() need const?])
AC_DEFINE_UNQUOTED(CONNECT_NAME_T,$cl_cv_proto_connect_arg2b,[type of `name' in connect() declaration])
AC_DEFINE_UNQUOTED(CONNECT_ADDRLEN_T,$cl_cv_proto_connect_arg3,[type of `addrlen' in connect() declaration])
fi])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_DYNLOAD],
[dnl Some systems have dlopen in libc, some have it in libdl.
AC_CHECK_HEADERS(dlfcn.h)
if test "$ac_cv_header_dlfcn_h" = yes; then
  AC_SEARCH_LIBS(dlopen, dl)
  AC_CHECK_FUNCS(dlopen dlsym dlvsym dlerror dlclose dladdr)
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_ELOOP],
[AC_REQUIRE([AC_PROG_CC])dnl
AC_CACHE_CHECK(for ELOOP, cl_cv_decl_eloop, [dnl
AC_RUN_IFELSE([#include "confdefs.h"
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <stdio.h>
#ifdef ELOOP
int main () {
  if (freopen("conftest.out", "w", stdout) == NULL) return 1;
  printf("ELOOP\n");
  return ferror(stdout) || fclose(stdout);
}
#else
extern int errno;
#define foo "conflink"
#define foobar "conflink/somefile"
int main() {
  /* If a system goes into an endless loop on this, it must be really broken. */
  if (symlink(foo,foo)<0) return 1;
  if (unlink(foobar)>=0) { unlink(foo); return 1; }
  if (freopen("conftest.out", "w", stdout) == NULL) return 1;
  printf("%d\n",errno); unlink(foo);
  return ferror(stdout) || fclose(stdout);
}
#endif],[cl_cv_decl_ELOOP=`cat conftest.out`
if test "$cl_cv_decl_ELOOP" = "ELOOP"; then
  cl_cv_decl_eloop=yes
else
  cl_cv_decl_eloop="$cl_cv_decl_ELOOP"
fi],[cl_cv_decl_eloop=no
cl_cv_decl_ELOOP="ELOOP"],[AC_EGREP_CPP(yes,[
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <stdio.h>
#ifdef ELOOP
yes
#endif
],[cl_cv_decl_eloop=yes],[cl_cv_decl_eloop=no])
cl_cv_decl_ELOOP="ELOOP"])
rm -f conftest.out
])
AC_DEFINE_UNQUOTED(ELOOP_VALUE,$cl_cv_decl_ELOOP,[the real value of ELOOP even if it is hidden in <errno.h>])
])

dnl -*- Autoconf -*-
dnl Copyright (C) 2002-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Sam Steingold.

AC_PREREQ(2.61)

dnl Each of the macros determines a facet of the default floating-point
dnl environment.
dnl   CL_FLOAT_DIV0        CL_DOUBLE_DIV0        CL_LONGDOUBLE_DIV0
dnl   CL_FLOAT_OVERFLOW    CL_DOUBLE_OVERFLOW    CL_LONGDOUBLE_OVERFLOW
dnl   CL_FLOAT_UNDERFLOW   CL_DOUBLE_UNDERFLOW   CL_LONGDOUBLE_UNDERFLOW
dnl   CL_FLOAT_INEXACT     CL_DOUBLE_INEXACT     CL_LONGDOUBLE_INEXACT

m4_define([CL_FENV_SIGNAL_INSTALL],[signal (SIGFPE, sigfpe_handler);
#if (defined (__sgi) || defined (_AIX)) && defined (SIGTRAP)
  signal (SIGTRAP, sigfpe_handler);
#endif])

AC_DEFUN([CL_FENV_CHECK],[dnl
AC_CACHE_CHECK([whether $1],[$2],
[AC_RUN_IFELSE([AC_LANG_SOURCE([#include <stdlib.h>
#include <signal.h>
static void sigfpe_handler (int sig) { exit (1); }
$3])],[$2=no], [$2=yes], [$2="guessing no"])])])

AC_DEFUN([CL_FLOAT_DIV0],
[CL_FENV_CHECK([single-float divbyzero raises an exception],
[cl_cv_cc_float_divbyzero],[dnl
float x = 1;
float y = 0;
float z;
float nan;
int main () {
  CL_FENV_SIGNAL_INSTALL
  z = x / y; nan = y / y;
  return 0;
}])
if test "$cl_cv_cc_float_divbyzero" = yes; then
  AC_DEFINE(FLOAT_DIV0_EXCEPTION,1,
    [Define to 1 if 'float' division by zero raises an exception.])
fi
])

AC_DEFUN([CL_DOUBLE_DIV0],
[CL_FENV_CHECK([double-float divbyzero raises an exception],
[cl_cv_cc_double_divbyzero],[dnl
double x = 1;
double y = 0;
double z;
double nan;
int main () {
  CL_FENV_SIGNAL_INSTALL
  z = x / y; nan = y / y;
  return 0;
}])
if test "$cl_cv_cc_double_divbyzero" = yes; then
  AC_DEFINE(DOUBLE_DIV0_EXCEPTION,1,
    [Define to 1 if 'double' division by zero raises an exception.])
fi
])

AC_DEFUN([CL_LONGDOUBLE_DIV0],
[CL_FENV_CHECK([long-float divbyzero raises an exception],
[cl_cv_cc_longdouble_divbyzero],[dnl
long double x = 1;
long double y = 0;
long double z;
long double nan;
int main () {
  CL_FENV_SIGNAL_INSTALL
  z = x / y; nan = y / y;
  return 0;
}])
if test "$cl_cv_cc_longdouble_divbyzero" = yes; then
  AC_DEFINE(LONGDOUBLE_DIV0_EXCEPTION,1,
    [Define to 1 if 'long double' division by zero raises an exception.])
fi
])


AC_DEFUN([CL_FLOAT_OVERFLOW],
[CL_FENV_CHECK([single-float overflow raises an exception],
[cl_cv_cc_float_overflow],[dnl
#include <float.h>
float x = FLT_MAX;
float y = FLT_MAX;
float z;
int main () {
  CL_FENV_SIGNAL_INSTALL
  z = x * y;
  return 0;
}])
if test "$cl_cv_cc_float_overflow" = yes; then
  AC_DEFINE(FLOAT_OVERFLOW_EXCEPTION,1,
    [Define to 1 if 'float' overflow raises an exception.])
fi
])

AC_DEFUN([CL_DOUBLE_OVERFLOW],
[CL_FENV_CHECK([double-float overflow raises an exception],
[cl_cv_cc_double_overflow],[dnl
#include <float.h>
double x = DBL_MAX;
double y = DBL_MAX;
double z;
int main () {
  CL_FENV_SIGNAL_INSTALL
  z = x * y;
  return 0;
}])
if test "$cl_cv_cc_double_overflow" = yes; then
  AC_DEFINE(DOUBLE_OVERFLOW_EXCEPTION,1,
    [Define to 1 if 'double' overflow raises an exception.])
fi
])

AC_DEFUN([CL_LONGDOUBLE_OVERFLOW],
[CL_FENV_CHECK([long-float overflow raises an exception],
[cl_cv_cc_longdouble_overflow],[dnl
#include <float.h>
long double x;
long double y;
long double z;
int main () {
  x = LDBL_MAX;
  y = LDBL_MAX;
  CL_FENV_SIGNAL_INSTALL
  z = x * y;
  return 0;
}])
if test "$cl_cv_cc_longdouble_overflow" = yes; then
  AC_DEFINE(LONGDOUBLE_OVERFLOW_EXCEPTION,1,
    [Define to 1 if 'long double' overflow raises an exception.])
fi
])


AC_DEFUN([CL_FLOAT_UNDERFLOW],
[CL_FENV_CHECK([single-float underflow raises an exception],
[cl_cv_cc_float_underflow],[dnl
#include <float.h>
float x = FLT_MIN;
float y = FLT_MIN;
float z;
int main () {
  CL_FENV_SIGNAL_INSTALL
  z = x * y;
  return 0;
}])
if test "$cl_cv_cc_float_underflow" = yes; then
  AC_DEFINE(FLOAT_UNDERFLOW_EXCEPTION,1,
    [Define to 1 if 'float' underflow raises an exception.])
fi
])

AC_DEFUN([CL_DOUBLE_UNDERFLOW],
[CL_FENV_CHECK([double-float underflow raises an exception],
[cl_cv_cc_double_underflow],[dnl
#include <float.h>
double x = DBL_MIN;
double y = DBL_MIN;
double z;
int main () {
  CL_FENV_SIGNAL_INSTALL
  z = x * y;
  return 0;
}])
if test "$cl_cv_cc_double_underflow" = yes; then
  AC_DEFINE(DOUBLE_UNDERFLOW_EXCEPTION,1,
    [Define to 1 if 'double' underflow raises an exception.])
fi
])

AC_DEFUN([CL_LONGDOUBLE_UNDERFLOW],
[CL_FENV_CHECK([long-float underflow raises an exception],
[cl_cv_cc_longdouble_underflow],[dnl
#include <float.h>
long double x;
long double y;
long double z;
int main () {
  x = LDBL_MIN;
  y = LDBL_MIN;
  CL_FENV_SIGNAL_INSTALL
  z = x * y;
  return 0;
}])
if test "$cl_cv_cc_longdouble_underflow" = yes; then
  AC_DEFINE(LONGDOUBLE_UNDERFLOW_EXCEPTION,1,
    [Define to 1 if 'long double' underflow raises an exception.])
fi
])


AC_DEFUN([CL_FLOAT_INEXACT],
[CL_FENV_CHECK([single-float inexact raises an exception],
[cl_cv_cc_float_inexact],[dnl
float x = 1;
float y = 3;
float z;
int main () {
  CL_FENV_SIGNAL_INSTALL
  z = x / y;
  return 0;
}])
if test "$cl_cv_cc_float_inexact" = yes; then
  AC_DEFINE(FLOAT_INEXACT_EXCEPTION,1,
    [Define to 1 if a 'float' inexact operation raises an exception.])
fi
])

AC_DEFUN([CL_DOUBLE_INEXACT],
[CL_FENV_CHECK([double-float inexact raises an exception],
[cl_cv_cc_double_inexact],[dnl
double x = 1;
double y = 3;
double z;
int main () {
  CL_FENV_SIGNAL_INSTALL
  z = x / y;
  return 0;
}])
if test "$cl_cv_cc_double_inexact" = yes; then
  AC_DEFINE(DOUBLE_INEXACT_EXCEPTION,1,
    [Define to 1 if a 'double' inexact operation raises an exception.])
fi
])

AC_DEFUN([CL_LONGDOUBLE_INEXACT],
[CL_FENV_CHECK([long-float inexact raises an exception],
[cl_cv_cc_longdouble_inexact],[dnl
long double x = 1;
long double y = 3;
long double z;
int main () {
  CL_FENV_SIGNAL_INSTALL
  z = x / y;
  return 0;
}])
if test "$cl_cv_cc_longdouble_inexact" = yes; then
  AC_DEFINE(LONGDOUBLE_INEXACT_EXCEPTION,1,
    [Define to 1 if a 'long double' inexact operation raises an exception.])
fi
])

# -*- Autoconf -*-
# Copyright (C) 2007-2008 Sam Steingold (GNU GPL2+)

AC_PREREQ(2.61)

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
else cl_cv_have_ffcall=no
fi])
if test $cl_use_ffcall = yes -a $cl_cv_have_ffcall = no; then
  if [ "$ac_cv_build" = "$ac_cv_host" ]; then host_arg="";
  else host_arg=" --host=$ac_cv_host";
  fi
  FFCALL=ffcall-1.8
  AC_MSG_ERROR([despite --with-ffcall, FFCALL was not found
 Either call configure without --with-ffcall or do
  mkdir tools; cd tools; prefix=`pwd`/${ac_cv_host}
  wget http://ftp.gnu.org/pub/gnu/ffcall/${FFCALL}.tar.gz
  tar xfz ${FFCALL}.tar.gz
  cd ${FFCALL}
  ./configure$host_arg --prefix=\${prefix} && make && make check && make install
  cd ../..
  ./configure --with-libffcall-prefix=\${prefix} [$]*])
fi
fi;])])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.13)

AC_DEFUN([CL_FILECHARSET],
[AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([CL_CLOSEDIR])dnl
AC_CACHE_CHECK(for the valid characters in filenames,
cl_cv_os_valid_filename_char,[dnl
dnl Create the subdirectory the test program will use for its files.
mkdir conftestdir
AC_RUN_IFELSE([[#include "confdefs.h"
#include <sys/types.h>
#include <stdlib.h>
/* Declare chdir(). */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <stdio.h>
/* Declare open(). */
#include <fcntl.h>
/* Declare opendir(), readdir(), closedir(). */
#include <dirent.h>
/* A small program which checks for each character whether or not it is
 * valid in filenames. */
#define N 256
int main ()
{
  if (freopen("conftest.out", "w", stdout) == NULL) return 1;
#if defined(__CYGWIN32__) || defined(__MINGW32__)
  /* The test below would cause a dialog box to pop up (ch == ':'),
     and create files which cause "rm -rf conftest*" to hang
     (ch == '"') || (ch == '<') || (ch == '>') || (ch == 197) || (ch == 206).
     Also, it would make appear that (ch >= 'A') && (ch <= 'Z') doesn't work,
     because it would create files in lower case. But we don't want to forbid
     upper case letters in file names. */
  printf("(ch >= 1) && (ch <= 127) && (ch != 34) && (ch != 42) && (ch != 47) && (ch != 58) && (ch != 60) && (ch != 62) && (ch != 63) && (ch != 92) || (ch == 131) || (ch >= 160) && (ch != 197) && (ch != 206)\n");
#else
  char legal[N];
  char filename[4];
  int i;
  if (chdir("conftestdir") < 0) return 1;
  for (i = 0; i < N; i++) legal[i] = 0;
  strcpy(filename,"a_z");
  for (i = 0; i < N; i++)
    if (i != '\0')
      { filename[1] = i;
        /* Determine whether the filename is valid: create a file
         * and check that it is present afterwards, under the same name. */
        { int fd = open(filename, O_CREAT | O_RDWR, 0644);
          if (fd >=0)
            { DIR* dirp = opendir(".");
              if (dirp != (DIR*)0)
                { struct dirent * d;
                  while ((d = readdir(dirp)))
                    { if (!strcmp(d->d_name,".")) continue;
                      if (!strcmp(d->d_name,"..")) continue;
                      if (!strncmp(d->d_name,".nfs",4)) continue;
                      if (!strcmp(d->d_name,filename)) legal[i] = 1;
                      /* Remove the file even if its name is something else. */
                      unlink(d->d_name);
                    }
                  closedir(dirp);
                }
              close(fd);
      } }   }
  /* Output a boolean expression equivalent to legal[ch] (0 <= ch < N). */
  { int need_or = 0;
    int z;
    for (z = 0; z < N; )
      { int x, y;
        if (! legal[z]) { z++; continue; }
        x = z;
        if (need_or) printf(" || ");
        z++;
        if ((z < N) && legal[z])
          { do { do { z++; } while ((z < N) && legal[z]);
                 y = z-1;
                 z++;
               } while ((z < N) && legal[z]);
            { int premises = 0;
              if (x > 0) premises++;
              if (y < N-1) premises++;
              for (i = x; i <= y; i++)
                if (! legal[i])
                  premises++;
              if (premises > 1) printf("(");
              { int need_and = 0;
                if (x > 0) { printf("(ch >= %d)",x); need_and = 1; }
                if (y < N-1)
                  { if (need_and) printf(" && ");
                    printf("(ch <= %d)",y);
                    need_and = 1;
                  }
                for (i = x; i <= y; i++)
                  if (! legal[i])
                    { if (need_and) printf(" && ");
                      printf("(ch != %d)",i);
                      need_and = 1;
                    }
                if (!need_and) printf("1");
              }
              if (premises > 1) printf(")");
            }
            z = y+1;
          }
          else
          { printf("(ch == %d)",x); z++; }
        need_or = 1;
      }
    printf("\n");
  }
#endif
  return ferror(stdout) || fclose(stdout);
}]],[cl_cv_os_valid_filename_char=`cat conftest.out`],
[cl_cv_os_valid_filename_char=''],[cl_cv_os_valid_filename_char=''])
dnl clean up
# Workaround a problem with NFS on Solaris 7, where unlink()ed files reappear
# immediately under a different name and disappear only after 1. the process
# doing readdir() has exited and 2. waiting a second or two.
# Even "rm -rf conftestdir" goes into an endless loop, eating CPU time, under
# these conditions.
period=1
while test -n "`ls conftestdir/.nfs* 2>/dev/null`"; do
  echo "waiting for NFS..."
  rm -f conftestdir/.nfs* 2>/dev/null
  sleep $period
  period=`expr 2 '*' $period`
done
# Now it's safe to do "rm -rf conftestdir".
rm -rf conftestdir
rm -f conftest.out
if test -z "$cl_cv_os_valid_filename_char"; then
  cl_cv_os_valid_filename_charset="guessing 7-bit"
else
  if test "$cl_cv_os_valid_filename_char" = '((ch >= 1) && (ch != 47))'; then
    cl_cv_os_valid_filename_charset="8-bit"
  else
    cl_cv_os_valid_filename_charset="7-bit"
  fi
fi
])
if test -n "$cl_cv_os_valid_filename_char"; then
  AC_DEFINE_UNQUOTED(VALID_FILENAME_CHAR,$cl_cv_os_valid_filename_char,[expression in ch which is true if ch is a valid character in filenames])
fi
])

# floatparam.m4 serial 2  -*- Autoconf -*-
dnl Copyright (C) 2005-2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible, Sam Steingold.

AC_DEFUN([CL_FLOATPARAM_CROSS],[
  cl_machine_file_h=$1
  {
    echo "/* Rounding modes, for use below */"
    echo "#define rounds_to_nearest        0  /* 0.5 ulp */"
    echo "#define rounds_to_zero           1  /* 1 ulp */"
    echo "#define rounds_to_infinity       2  /* 1 ulp */"
    echo "#define rounds_to_minus_infinity 3  /* 1 ulp */"
    echo
    for type in float double "long double"; do
      if test -n "$type"; then
        epsilon_bits=-1; y="($type)1.0"
        while true; do
          AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
            [[typedef int verify[2*(
               (($type)(($type)1.0 + ($type)($y)) == ($type)1.0)
               || ($type)(($type)(($type)1.0 + ($type)($y)) - ($type)1.0) != ($type)($y)
             ) - 1];]])],
            [break;])
          epsilon_bits=`expr $epsilon_bits + 1`; y="$y * ($type)0.5"
        done
        negepsilon_bits=-1; y="($type)-1.0"
        while true; do
          AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
            [[typedef int verify[2*(
               (($type)(($type)1.0 + ($type)($y)) == ($type)1.0)
               || ($type)(($type)(($type)1.0 + ($type)($y)) - ($type)1.0) != ($type)($y)
             ) - 1];]])],
            [break;])
          negepsilon_bits=`expr $negepsilon_bits + 1`; y="$y * ($type)0.5"
        done
        echo "/* Properties of type \`$type': */"
        echo "/* Largest n for which 1+2^(-n) is exactly represented is $epsilon_bits. */"
        echo "/* Largest n for which 1-2^(-n) is exactly represented is $negepsilon_bits. */"
        if test `expr $negepsilon_bits '<=' $epsilon_bits` = 1; then
          echo "#error \"No exponent jump at 1.0 for type $type!\""
        else
          if test `expr $negepsilon_bits '>' $epsilon_bits + 1` = 1; then
            echo "/* Base for type '$type' is 2^"`expr $negepsilon_bits - $epsilon_bits`
          fi
          echo "#define "`echo $type | sed -e 's, ,_,g'`"_mant_bits "`expr $epsilon_bits + 1`
        fi
        x="($type)1.0"
        i=$epsilon_bits
        while test $i != 0; do
          x="$x * ($type)0.5"
          i=`expr $i - 1`
        done
        x="($type)($x)"
        y1="($type)(($type)1.0 + ($type)5.0*$x)"
        y2="($type)(($type)1.0 + ($type)6.0*$x)"
        ys1="($type)(($type)1.0 + ($type)5.4*$x)"
        ys2="($type)(($type)1.0 + ($type)5.6*$x)"
        z1="($type)(($type)-1.0 + ($type)(-5.0)*$x)"
        z2="($type)(($type)-1.0 + ($type)(-6.0)*$x)"
        zs1="($type)(($type)-1.0 + ($type)(-5.4)*$x)"
        zs2="($type)(($type)-1.0 + ($type)(-5.6)*$x)"
        rounds=
        if test -z "$rounds"; then
          AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
            [[typedef int verify[2*(
               $ys1 == $y1 && $ys2 == $y2 && $zs1 == $z1 && $zs2 == $z2
             ) - 1];]])],
            [rounds=rounds_to_nearest])
        fi
        if test -z "$rounds"; then
          AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
            [[typedef int verify[2*(
               $ys1 == $y1 && $ys2 == $y1 && $zs1 == $z1 && $zs2 == $z1
             ) - 1];]])],
            [rounds=rounds_to_zero])
        fi
        if test -z "$rounds"; then
          AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
            [[typedef int verify[2*(
               $ys1 == $y2 && $ys2 == $y2 && $zs1 == $z1 && $zs2 == $z1
             ) - 1];]])],
            [rounds=rounds_to_infinity])
        fi
        if test -z "$rounds"; then
          AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
            [[typedef int verify[2*(
               $ys1 == $y1 && $ys2 == $y1 && $zs1 == $z2 && $zs2 == $z2
             ) - 1];]])],
            [rounds=rounds_to_minus_infinity])
        fi
        if test -n "$rounds"; then
          echo "#define "`echo $type | sed -e 's, ,_,g'`"_rounds $rounds"
        else
          echo "#error \"Unknown rounding mode for type $type!\""
        fi
        echo
      fi
    done
    dnl Words-in-a-double endianness test. Note that, assuming IEEE 754 format,
    dnl 2.5479915693083957     = { 0x40 0x04 0x62 0x49 0x67 0x65 0x4E 0x64 } ..bIgeNd
    dnl 1.4396527506122064e164 = { 0x62 0x04 0x00 0x00 0x4E 0x65 0x67 0x49 } b...NegI
    dnl 2.5495230282078065     = { 0x40 0x04 0x65 0x6C 0x54 0x54 0x69 0x4C } ..elTTiL
    dnl 1.4139248369879473e214 = { 0x6C 0x65 0x00 0x00 0x4C 0x69 0x54 0x54 } le..LiTT
    double_wordorder_bigendian_p=
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[double a[9] = {
       0, 2.5479915693083957, 0, 1.4396527506122064e164,
       0, 2.5495230282078065, 0, 1.4139248369879473e214,
       0 };]], [])], [dnl
      if grep LiTTle conftest.$ac_objext >/dev/null ; then
        double_wordorder_bigendian_p=0
      else
        if grep bIgeN conftest.$ac_objext >/dev/null ; then
          double_wordorder_bigendian_p=1
        fi
      fi])
    if test -n "$double_wordorder_bigendian_p"; then
      echo "#define double_wordorder_bigendian_p $double_wordorder_bigendian_p"
    else
      echo "/* Dazed and confused!  Better not define anything. */"
    fi
    echo
  } > "$cl_machine_file_h"
])

dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.61)

dnl without AC_MSG_...:   with AC_MSG_... and caching:
dnl   AC_COMPILE_IFELSE      CL_COMPILE_CHECK
dnl   AC_LINK_IFELSE         CL_LINK_CHECK
dnl Usage:
dnl AC_xxx_IFELSE(PROGRAM, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND])
dnl CL_xxx_CHECK(ECHO-TEXT, CACHE-ID, PROGRAM,
dnl              ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND])

dnl the next macro avoids aclocal warnings about wrong macro order
dnl when making aclocal.m4, see Makefile.devel.
dnl note that this macro cannot call AC_CONFIG_AUX_DIR directly because
dnl the required macros are evaluated BEFORE the macro itself
dnl and some of them require AC_CONFIG_AUX_DIR.
dnl <http://article.gmane.org/gmane.comp.lib.gnulib.bugs/16312>
AC_DEFUN([CL_MODULE_COMMON_CHECKS],[dnl
AC_REQUIRE([AC_CONFIG_AUX_DIR], [AC_CONFIG_AUX_DIR([$1])])dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_PROG_CPP])dnl
AC_REQUIRE([AC_GNU_SOURCE])dnl
AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])dnl
AC_CHECK_HEADERS(time.h sys/time.h)dnl
])

AC_DEFUN([CL_CHECK],[dnl
  AC_CACHE_CHECK([for $2],[$3],
    [$1([AC_LANG_PROGRAM([$4],[$5])],[$3=yes],[$3=no])])
  AS_IF([test $$3 = yes], [$6], [$7])
])

AC_DEFUN([CL_COMPILE_CHECK], [CL_CHECK([AC_COMPILE_IFELSE],$@)])
AC_DEFUN([CL_LINK_CHECK], [CL_CHECK([AC_LINK_IFELSE],$@)])

dnl Expands to the "extern ..." prefix used for system declarations.
dnl AC_LANG_EXTERN()
AC_DEFUN([AC_LANG_EXTERN],
[extern
#ifdef __cplusplus
"C"
#endif
])

AC_DEFUN([CL_CC_WORKS],
[AC_CACHE_CHECK(whether CC works at all, cl_cv_prog_cc_works, [
AC_LANG_PUSH(C)
AC_RUN_IFELSE(AC_LANG_PROGRAM([],[]),
[cl_cv_prog_cc_works=yes], [cl_cv_prog_cc_works=no],
AC_LINK_IFELSE(AC_LANG_PROGRAM([],[]),
[cl_cv_prog_cc_works=yes], [cl_cv_prog_cc_works=no]))
AC_LANG_POP(C)
])
if test "$cl_cv_prog_cc_works" = no; then
AC_MSG_FAILURE([Installation or configuration problem: C compiler cannot create executables.])
fi
])

AC_DEFUN([CL_CANONICAL_HOST_CPU],
[AC_REQUIRE([AC_CANONICAL_HOST])AC_REQUIRE([AC_PROG_CC])
case "$host_cpu" in
changequote(,)dnl
  i[4567]86 )
    host_cpu_instructionset=i386
    ;;
  alphaev[4-8] | alphaev56 | alphapca5[67] | alphaev6[78] )
    host_cpu_instructionset=alpha
    ;;
  hppa1.0 | hppa1.1 | hppa2.0* | hppa64 )
    host_cpu_instructionset=hppa
    ;;
  rs6000 )
    host_cpu_instructionset=powerpc
    ;;
  c1 | c2 | c32 | c34 | c38 | c4 )
    host_cpu_instructionset=convex
    ;;
  arm* )
    host_cpu_instructionset=arm
    ;;
changequote([,])dnl
  mips* )
    AC_CACHE_CHECK([for 64-bit MIPS], cl_cv_host_mips64, [
AC_EGREP_CPP(yes,
[#if defined(_MIPS_SZLONG)
#if (_MIPS_SZLONG == 64)
/* We should also check for (_MIPS_SZPTR == 64), but gcc keeps this at 32. */
  yes
#endif
#endif
], cl_cv_host_mips64=yes, cl_cv_host_mips64=no)
])
if test $cl_cv_host_mips64 = yes; then
  host_cpu_instructionset=mips64
else
  host_cpu_instructionset=mips
fi
    ;;
dnl On powerpc64 systems, the C compiler may still be generating 32-bit code.
  powerpc64 )
    AC_CACHE_CHECK([for 64-bit PowerPC], cl_cv_host_powerpc64, [
AC_EGREP_CPP(yes,
[#if defined(__powerpc64__) || defined(_ARCH_PPC64)
  yes
#endif
], cl_cv_host_powerpc64=yes, cl_cv_host_powerpc64=no)
])
if test $cl_cv_host_powerpc64 = yes; then
  host_cpu_instructionset=powerpc64
else
  host_cpu_instructionset=powerpc
fi
    ;;
dnl UltraSPARCs running Linux have `uname -m` = "sparc64", but the C compiler
dnl still generates 32-bit code.
  sparc | sparc64 )
    AC_CACHE_CHECK([for 64-bit SPARC], cl_cv_host_sparc64, [
AC_EGREP_CPP(yes,
[#if defined(__sparcv9) || defined(__arch64__)
  yes
#endif
], cl_cv_host_sparc64=yes, cl_cv_host_sparc64=no)
])
if test $cl_cv_host_sparc64 = yes; then
  host_cpu_instructionset=sparc64
else
  host_cpu_instructionset=sparc
fi
    ;;
dnl On x86_64 systems, the C compiler may still be generating 32-bit code.
  x86_64 )
    AC_CACHE_CHECK([for 64-bit x86_64], cl_cv_host_x86_64, [
AC_EGREP_CPP(yes,
[#if defined(__LP64__) || defined(__x86_64__) || defined(__amd64__)
  yes
#endif
], cl_cv_host_x86_64=yes, cl_cv_host_x86_64=no)
])
if test $cl_cv_host_x86_64 = yes; then
  host_cpu_instructionset=x86_64
else
  host_cpu_instructionset=i386
fi
    ;;
  *)
    host_cpu_instructionset=$host_cpu
    ;;
esac
dnl was AC_DEFINE_UNQUOTED(__${host_cpu}__) but KAI C++ 3.2d doesn't like this
cat >> confdefs.h <<EOF
#ifndef __${host_cpu_instructionset}__
#define __${host_cpu_instructionset}__ 1
#endif
EOF
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_GETHOSTBYNAME],
[AC_CHECK_HEADERS(netdb.h,
[AC_DEFINE(HAVE_GETHOSTBYNAME,,[have gethostbyname()])dnl
break])])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_GETPAGESIZE],
[AC_BEFORE([$0], [CL_MPROTECT])
CL_LINK_CHECK([getpagesize], cl_cv_func_getpagesize, [
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
], [getpagesize();],
AC_DEFINE(HAVE_GETPAGESIZE,,[have getpagesize()])
have_getpagesize=1)dnl
if test -n "$have_getpagesize"; then
CL_PROTO([getpagesize], [
CL_PROTO_RET([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
], [int getpagesize();],
cl_cv_proto_getpagesize_ret, int, size_t)
], [extern $cl_cv_proto_getpagesize_ret getpagesize (void);])
AC_DEFINE_UNQUOTED(RETGETPAGESIZETYPE,$cl_cv_proto_getpagesize_ret,[return type of getpagesize()])
else
dnl Otherwise we use PAGESIZE defined in <sys/param.h>.
dnl But mingw32 doesn't have <sys/param.h>.
AC_CHECK_HEADERS(sys/param.h)
fi
])

# Configure paths for GTK+
# Owen Taylor     1997-2001

dnl AM_PATH_GTK_2_0([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, MODULES]]]])
dnl Test for GTK+, and define GTK_CFLAGS and GTK_LIBS, if gthread is specified in MODULES, 
dnl pass to pkg-config
dnl
AC_DEFUN([AM_PATH_GTK_2_0],
[dnl 
dnl Get the cflags and libraries from pkg-config
dnl
AC_ARG_ENABLE(gtktest, [  --disable-gtktest       do not try to compile and run a test GTK+ program],
		    , enable_gtktest=yes)

  pkg_config_args=gtk+-2.0
  for module in . $4
  do
      case "$module" in
         gthread) 
             pkg_config_args="$pkg_config_args gthread-2.0"
         ;;
      esac
  done

  no_gtk=""

  AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

  if test x$PKG_CONFIG != xno ; then
    if pkg-config --atleast-pkgconfig-version 0.7 ; then
      :
    else
      echo "*** pkg-config too old; version 0.7 or better required."
      no_gtk=yes
      PKG_CONFIG=no
    fi
  else
    no_gtk=yes
  fi

  min_gtk_version=ifelse([$1], ,2.0.0,$1)
  AC_MSG_CHECKING(for GTK+ - version >= $min_gtk_version)

  if test x$PKG_CONFIG != xno ; then
    ## don't try to run the test against uninstalled libtool libs
    if $PKG_CONFIG --uninstalled $pkg_config_args; then
	  echo "Will use uninstalled version of GTK+ found in PKG_CONFIG_PATH"
	  enable_gtktest=no
    fi

    if $PKG_CONFIG --atleast-version $min_gtk_version $pkg_config_args; then
	  :
    else
	  no_gtk=yes
    fi
  fi

  if test x"$no_gtk" = x ; then
    GTK_CFLAGS=`$PKG_CONFIG $pkg_config_args --cflags`
    GTK_LIBS=`$PKG_CONFIG $pkg_config_args --libs`
    gtk_config_major_version=`$PKG_CONFIG --modversion gtk+-2.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtk_config_minor_version=`$PKG_CONFIG --modversion gtk+-2.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtk_config_micro_version=`$PKG_CONFIG --modversion gtk+-2.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_gtktest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $GTK_CFLAGS"
      LIBS="$GTK_LIBS $LIBS"
dnl
dnl Now check if the installed GTK+ is sufficiently new. (Also sanity
dnl checks the results of pkg-config to some extent)
dnl
      rm -f conf.gtktest
      AC_TRY_RUN([
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>

int 
main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.gtktest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = g_strdup("$min_gtk_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gtk_version");
     exit(1);
   }

  if ((gtk_major_version != $gtk_config_major_version) ||
      (gtk_minor_version != $gtk_config_minor_version) ||
      (gtk_micro_version != $gtk_config_micro_version))
    {
      printf("\n*** 'pkg-config --modversion gtk+-2.0' returned %d.%d.%d, but GTK+ (%d.%d.%d)\n", 
             $gtk_config_major_version, $gtk_config_minor_version, $gtk_config_micro_version,
             gtk_major_version, gtk_minor_version, gtk_micro_version);
      printf ("*** was found! If pkg-config was correct, then it is best\n");
      printf ("*** to remove the old version of GTK+. You may also be able to fix the error\n");
      printf("*** by modifying your LD_LIBRARY_PATH enviroment variable, or by editing\n");
      printf("*** /etc/ld.so.conf. Make sure you have run ldconfig if that is\n");
      printf("*** required on your system.\n");
      printf("*** If pkg-config was wrong, set the environment variable PKG_CONFIG_PATH\n");
      printf("*** to point to the correct configuration files\n");
    } 
  else if ((gtk_major_version != GTK_MAJOR_VERSION) ||
	   (gtk_minor_version != GTK_MINOR_VERSION) ||
           (gtk_micro_version != GTK_MICRO_VERSION))
    {
      printf("*** GTK+ header files (version %d.%d.%d) do not match\n",
	     GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
      printf("*** library (version %d.%d.%d)\n",
	     gtk_major_version, gtk_minor_version, gtk_micro_version);
    }
  else
    {
      if ((gtk_major_version > major) ||
        ((gtk_major_version == major) && (gtk_minor_version > minor)) ||
        ((gtk_major_version == major) && (gtk_minor_version == minor) && (gtk_micro_version >= micro)))
      {
        return 0;
       }
     else
      {
        printf("\n*** An old version of GTK+ (%d.%d.%d) was found.\n",
               gtk_major_version, gtk_minor_version, gtk_micro_version);
        printf("*** You need a version of GTK+ newer than %d.%d.%d. The latest version of\n",
	       major, minor, micro);
        printf("*** GTK+ is always available from ftp://ftp.gtk.org.\n");
        printf("***\n");
        printf("*** If you have already installed a sufficiently new version, this error\n");
        printf("*** probably means that the wrong copy of the pkg-config shell script is\n");
        printf("*** being found. The easiest way to fix this is to remove the old version\n");
        printf("*** of GTK+, but you can also set the PKG_CONFIG environment to point to the\n");
        printf("*** correct copy of pkg-config. (In this case, you will have to\n");
        printf("*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n");
        printf("*** so that the correct libraries are found at run-time))\n");
      }
    }
  return 1;
}
],, no_gtk=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_gtk" = x ; then
     AC_MSG_RESULT(yes (version $gtk_config_major_version.$gtk_config_minor_version.$gtk_config_micro_version))
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$PKG_CONFIG" = "no" ; then
       echo "*** A new enough version of pkg-config was not found."
       echo "*** See http://pkgconfig.sourceforge.net"
     else
       if test -f conf.gtktest ; then
        :
       else
          echo "*** Could not run GTK+ test program, checking why..."
	  ac_save_CFLAGS="$CFLAGS"
	  ac_save_LIBS="$LIBS"
          CFLAGS="$CFLAGS $GTK_CFLAGS"
          LIBS="$LIBS $GTK_LIBS"
          AC_TRY_LINK([
#include <gtk/gtk.h>
#include <stdio.h>
],      [ return ((gtk_major_version) || (gtk_minor_version) || (gtk_micro_version)); ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding GTK+ or finding the wrong"
          echo "*** version of GTK+. If it is not finding GTK+, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH" ],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means GTK+ is incorrectly installed."])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GTK_CFLAGS=""
     GTK_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  rm -f conf.gtktest
])

# intparam.m4 serial 3  -*- Autoconf -*-
dnl Copyright (C) 2005-2008 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible, Sam Steingold

AC_DEFUN([CL_INTPARAM_CROSS],
[
  AC_REQUIRE([AC_TYPE_LONG_LONG_INT])
  AC_REQUIRE([AC_C_BIGENDIAN])
  cl_machine_file_h=$1
  {
    CL_INTPARAM_BITSIZE([signed char], [char_bitsize])
    CL_INTPARAM_BITSIZE([short], [short_bitsize])
    CL_INTPARAM_BITSIZE([int], [int_bitsize])
    CL_INTPARAM_BITSIZE([long], [long_bitsize])
    if test $ac_cv_type_long_long_int = yes; then
      CL_INTPARAM_BITSIZE([long long], [longlong_bitsize])
    fi
    CL_INTPARAM_BITSIZE([unsigned char], [uchar_bitsize])
    CL_INTPARAM_BITSIZE([unsigned short], [ushort_bitsize])
    CL_INTPARAM_BITSIZE([unsigned int], [uint_bitsize])
    CL_INTPARAM_BITSIZE([unsigned long], [ulong_bitsize])
    if test $ac_cv_type_long_long_int = yes; then
      CL_INTPARAM_BITSIZE([unsigned long long], [ulonglong_bitsize])
    fi
    if test -n "$char_bitsize"; then
      echo "/* Integers of type char have $char_bitsize bits. */"
      echo "#define char_bitsize $char_bitsize"
      echo
    else
      echo "#error \"Integers of type char have no binary representation!!\""
    fi
    if test -n "$short_bitsize"; then
      echo "/* Integers of type short have $short_bitsize bits. */"
      echo "#define short_bitsize $short_bitsize"
      echo
    else
      echo "#error \"Integers of type short have no binary representation!!\""
    fi
    if test -n "$int_bitsize"; then
      echo "/* Integers of type int have $int_bitsize bits. */"
      echo "#define int_bitsize $int_bitsize"
      echo
    else
      echo "#error \"Integers of type int have no binary representation!!\""
    fi
    if test -n "$long_bitsize"; then
      echo "/* Integers of type long have $long_bitsize bits. */"
      echo "#define long_bitsize $long_bitsize"
      echo
    else
      echo "#error \"Integers of type long have no binary representation!!\""
    fi
    if test $ac_cv_type_long_long_int = yes; then
      if test -n "$longlong_bitsize"; then
        echo "/* Integers of type long long have $longlong_bitsize bits. */"
        echo "#define long_long_bitsize $longlong_bitsize"
        echo
      else
        echo "#error \"Integers of type long long have no binary representation!!\""
      fi
    fi
    if test -n "$uchar_bitsize"; then
      echo "/* Integers of type unsigned char have $uchar_bitsize bits. */"
      echo
    else
      echo "#error \"Integers of type unsigned char have no binary representation!!\""
    fi
    if test -n "$ushort_bitsize"; then
      echo "/* Integers of type unsigned short have $ushort_bitsize bits. */"
      echo
    else
      echo "#error \"Integers of type unsigned short have no binary representation!!\""
    fi
    if test -n "$uint_bitsize"; then
      echo "/* Integers of type unsigned int have $uint_bitsize bits. */"
      echo
    else
      echo "#error \"Integers of type unsigned int have no binary representation!!\""
    fi
    if test -n "$ulong_bitsize"; then
      echo "/* Integers of type unsigned long have $ulong_bitsize bits. */"
      echo
    else
      echo "#error \"Integers of type unsigned long have no binary representation!!\""
    fi
    if test $ac_cv_type_long_long_int = yes; then
      if test -n "$ulonglong_bitsize"; then
        echo "/* Integers of type unsigned long long have $ulonglong_bitsize bits. */"
        echo
      else
        echo "#error \"Integers of type unsigned long long have no binary representation!!\""
      fi
    fi
    if test "$char_bitsize" != "$uchar_bitsize"; then
      echo "#error \"Integer types char and unsigned char have different sizes!!\""
    fi
    if test "$short_bitsize" != "$ushort_bitsize"; then
      echo "#error \"Integer types short and unsigned short have different sizes!!\""
    fi
    if test "$int_bitsize" != "$uint_bitsize"; then
      echo "#error \"Integer types int and unsigned int have different sizes!!\""
    fi
    if test "$long_bitsize" != "$ulong_bitsize"; then
      echo "#error \"Integer types long and unsigned long have different sizes!!\""
    fi
    if test $ac_cv_type_long_long_int = yes; then
      if test "$longlong_bitsize" != "$ulonglong_bitsize"; then
        echo "#error \"Integer types long long and unsigned long long have different sizes!!\""
      fi
    fi
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
      [[typedef int verify[2*(sizeof(char*)<=sizeof (long))-1];]])],
      [], [echo "#error \"Type char * does not fit into a long!!\""])
    _AC_COMPUTE_INT([sizeof (char *)], [pointer_size])
    pointer_bitsize=`expr $pointer_size '*' $char_bitsize`
    echo "/* Pointers of type char * have $pointer_bitsize bits. */"
    echo "#define pointer_bitsize $pointer_bitsize"
    echo
    CL_INTPARAM_SIZEOF([char], [sizeof_char])
    CL_INTPARAM_ALIGNOF([char], [alignment_char])
    echo "/* Type char has sizeof = $sizeof_char and alignment = $alignment_char. */"
    echo "#define sizeof_char $sizeof_char"
    echo "#define alignment_char $alignment_char"
    echo
    CL_INTPARAM_SIZEOF([unsigned char], [sizeof_uchar])
    CL_INTPARAM_ALIGNOF([unsigned char], [alignment_uchar])
    echo "/* Type unsigned char has sizeof = $sizeof_uchar and alignment = $alignment_uchar. */"
    echo
    CL_INTPARAM_SIZEOF([short], [sizeof_short])
    CL_INTPARAM_ALIGNOF([short], [alignment_short])
    echo "/* Type short has sizeof = $sizeof_short and alignment = $alignment_short. */"
    echo "#define sizeof_short $sizeof_short"
    echo "#define alignment_short $alignment_short"
    echo
    CL_INTPARAM_SIZEOF([unsigned short], [sizeof_ushort])
    CL_INTPARAM_ALIGNOF([unsigned short], [alignment_ushort])
    echo "/* Type unsigned short has sizeof = $sizeof_ushort and alignment = $alignment_ushort. */"
    echo
    CL_INTPARAM_SIZEOF([int], [sizeof_int])
    CL_INTPARAM_ALIGNOF([int], [alignment_int])
    echo "/* Type int has sizeof = $sizeof_int and alignment = $alignment_int. */"
    echo "#define sizeof_int $sizeof_int"
    echo "#define alignment_int $alignment_int"
    echo
    CL_INTPARAM_SIZEOF([unsigned int], [sizeof_uint])
    CL_INTPARAM_ALIGNOF([unsigned int], [alignment_uint])
    echo "/* Type unsigned int has sizeof = $sizeof_uint and alignment = $alignment_uint. */"
    echo
    CL_INTPARAM_SIZEOF([long], [sizeof_long])
    CL_INTPARAM_ALIGNOF([long], [alignment_long])
    echo "/* Type long has sizeof = $sizeof_long and alignment = $alignment_long. */"
    echo "#define sizeof_long $sizeof_long"
    echo "#define alignment_long $alignment_long"
    echo
    CL_INTPARAM_SIZEOF([unsigned long], [sizeof_ulong])
    CL_INTPARAM_ALIGNOF([unsigned long], [alignment_ulong])
    echo "/* Type unsigned long has sizeof = $sizeof_ulong and alignment = $alignment_ulong. */"
    echo
    if test $ac_cv_type_long_long_int = yes; then
      CL_INTPARAM_SIZEOF([long long], [sizeof_longlong])
      CL_INTPARAM_ALIGNOF([long long], [alignment_longlong])
      echo "/* Type long long has sizeof = $sizeof_longlong and alignment = $alignment_longlong. */"
      echo "#define sizeof_long_long $sizeof_longlong"
      echo "#define alignment_long_long $alignment_longlong"
      echo
      CL_INTPARAM_SIZEOF([unsigned long long], [sizeof_ulonglong])
      CL_INTPARAM_ALIGNOF([unsigned long long], [alignment_ulonglong])
      echo "/* Type unsigned long long has sizeof = $sizeof_ulonglong and alignment = $alignment_ulonglong. */"
      echo
    fi
    CL_INTPARAM_SIZEOF([float], [sizeof_float])
    CL_INTPARAM_ALIGNOF([float], [alignment_float])
    echo "/* Type float has sizeof = $sizeof_float and alignment = $alignment_float. */"
    echo "#define sizeof_float $sizeof_float"
    echo "#define alignment_float $alignment_float"
    echo
    CL_INTPARAM_SIZEOF([double], [sizeof_double])
    CL_INTPARAM_ALIGNOF([double], [alignment_double])
    echo "/* Type double has sizeof = $sizeof_double and alignment = $alignment_double. */"
    echo "#define sizeof_double $sizeof_double"
    echo "#define alignment_double $alignment_double"
    echo
    CL_INTPARAM_SIZEOF([long double], [sizeof_longdouble])
    CL_INTPARAM_ALIGNOF([long double], [alignment_longdouble])
    echo "/* Type long double has sizeof = $sizeof_longdouble and alignment = $alignment_longdouble. */"
    echo "#define sizeof_long_double $sizeof_longdouble"
    echo "#define alignment_long_double $alignment_longdouble"
    echo
    CL_INTPARAM_SIZEOF([char *], [sizeof_char_ptr])
    CL_INTPARAM_ALIGNOF([char *], [alignment_char_ptr])
    echo "/* Type char * has sizeof = $sizeof_char_ptr and alignment = $alignment_char_ptr. */"
    echo
    CL_INTPARAM_SIZEOF([long *], [sizeof_long_ptr])
    CL_INTPARAM_ALIGNOF([long *], [alignment_long_ptr])
    echo "/* Type long * has sizeof = $sizeof_long_ptr and alignment = $alignment_long_ptr. */"
    echo
    dnl disabled because:
    dnl - the results of these are not used anywhere
    dnl - the results of non-cross are not generated (see src/intparam.h)
    dnl - the C code fails with "expected identifier or '(' before ')' token"
    dnl   on the line "typedef int verify[2*(alignof(void (*)(void)) == 256) - 1];"
    dnl CL_INTPARAM_SIZEOF([void (*)(void)], [sizeof_function_ptr])
    dnl CL_INTPARAM_ALIGNOF([void (*)(void)], [alignment_function_ptr])
    echo "/* Type function * has sizeof = $sizeof_function_ptr and alignment = $alignment_function_ptr. */"
    echo
    case $ac_cv_c_bigendian in
      yes)
        echo "/* Type unsigned short is stored BIG-ENDIAN in memory (i.e. like mc68000 or sparc). */"
        echo "#define short_big_endian"
        echo "/* Type unsigned int is stored BIG-ENDIAN in memory (i.e. like mc68000 or sparc). */"
        echo "#define int_big_endian"
        echo "/* Type unsigned long is stored BIG-ENDIAN in memory (i.e. like mc68000 or sparc). */"
        echo "#define long_big_endian"
        if test $ac_cv_type_long_long_int = yes; then
          echo "/* Type unsigned long long is stored BIG-ENDIAN in memory (i.e. like mc68000 or sparc). */"
          echo "#define long_long_big_endian"
        fi
        ;;
      no)
        echo "/* Type unsigned short is stored LITTLE-ENDIAN in memory (i.e. like Z80 or VAX). */"
        echo "#define short_little_endian"
        echo "/* Type unsigned int is stored LITTLE-ENDIAN in memory (i.e. like Z80 or VAX). */"
        echo "#define int_little_endian"
        echo "/* Type unsigned long is stored LITTLE-ENDIAN in memory (i.e. like Z80 or VAX). */"
        echo "#define long_little_endian"
        if test $ac_cv_type_long_long_int = yes; then
          echo "/* Type unsigned long long is stored LITTLE-ENDIAN in memory (i.e. like Z80 or VAX). */"
          echo "#define long_long_little_endian"
        fi
        ;;
      *)
        echo "#error \"Type short is stored in memory in an obscure manner!!\""
        echo "#error \"Type int is stored in memory in an obscure manner!!\""
        echo "#error \"Type long is stored in memory in an obscure manner!!\""
        if test $ac_cv_type_long_long_int = yes; then
          echo "#error \"Type long long is stored in memory in an obscure manner!!\""
        fi
        ;;
    esac
    echo
    case $host_cpu in
      hppa)
        echo "/* Stack grows up. */"
        echo "#define stack_grows_up"
        ;;
      *)
        echo "/* Stack grows down. */"
        echo "#define stack_grows_down"
        ;;
    esac
  } > "$cl_machine_file_h"
])

dnl CL_INTPARAM_BITSIZE(type, variable)
dnl puts into variable the determined bitsize of the type.
AC_DEFUN([CL_INTPARAM_BITSIZE],
[
  n=1; x="($1)2"
  while true; do
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
      [[typedef int verify[2*(($1)($x) == 0) - 1];]])],
      [$2=$n; break;],
      [if test $n = 1000; then $2=; break; fi;])
    n=`expr $n + 1`; x="$x * ($1)2"
  done
])

dnl CL_INTPARAM_SIZEOF(type, variable)
dnl puts into variable the determined size of the type.
AC_DEFUN([CL_INTPARAM_SIZEOF],
[
  _AC_COMPUTE_INT([sizeof($1)], [$2])
])

dnl CL_INTPARAM_ALIGNOF(type, variable)
dnl puts into variable the determined alignment of the type.
AC_DEFUN([CL_INTPARAM_ALIGNOF],[
  dnl Simplify the guessing by assuming that the alignment is a power of 2.
  n=1
  while true; do
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <stddef.h>
#ifdef __cplusplus
# ifdef __GNUC__
#  define alignof(type)  __alignof__ (type)
# else
   template <class type> struct alignof_helper { char slot1; type slot2; };
#  define alignof(type)  offsetof (alignof_helper<type>, slot2)
# endif
#else
# define alignof(type)  offsetof (struct { char slot1; type slot2; }, slot2)
#endif
]], [[typedef int verify[2*(alignof($1) == $n) - 1];]])],
      [$2=$n; break;]
      [if test $n = 0; then $2=; break; fi])
    n=`expr $n '*' 2`
  done
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2004, 2007-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_IOCTL],
[AC_REQUIRE([CL_TERM])dnl
AC_REQUIRE([CL_CADDR_T])dnl
AC_CHECK_FUNCS(ioctl)
if test $ac_cv_func_ioctl = yes; then
ioctl_decl1='
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#ifndef HAVE_TCSAFLUSH
#undef HAVE_TERMIOS_H
#endif
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#else
#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#else
#ifdef HAVE_TERMIO_H
#include <termio.h>
#else
#ifdef HAVE_SGTTY_H
#include <sgtty.h>
#include <sys/ioctl.h>
#endif
#endif
#endif
#endif
'
CL_PROTO([ioctl], [
dnl First find out whether this set of includes declares ioctl(), or whether
dnl we shall use <sys/ioctl.h> instead.
dnl Note: we must not include <sys/ioctl.h> deliberately since it is
dnl incompatible to <termios.h> (and doesn't even declare ioctl()) on SunOS 4.
AC_TRY_COMPILE($ioctl_decl1
AC_LANG_EXTERN[char* ioctl();], [], try_sys_ioctl=1, ioctl_decl="$ioctl_decl1")
if test -n "try_sys_ioctl"; then
ioctl_decl2='
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#include <sys/ioctl.h>
'
AC_TRY_COMPILE($ioctl_decl2
AC_LANG_EXTERN[char* ioctl();], [],
ioctl_decl="$ioctl_decl1", ioctl_decl="$ioctl_decl2")
fi
dnl Then find out about the correct ioctl declaration:
for y in 'caddr_t arg' 'void* arg' '...'; do
for x in SIZE_VARIANTS; do
if test -z "$have_ioctl"; then
CL_PROTO_TRY($ioctl_decl[
#ifdef INCLUDE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
], [int ioctl (int fd, $x request, $y);], [
cl_cv_proto_ioctl_arg2="$x"
if test "$y" = "..."; then
cl_cv_proto_ioctl_dots=yes
cl_cv_proto_ioctl_args="int, $cl_cv_proto_ioctl_arg2, ..."
else
cl_cv_proto_ioctl_dots=no
cl_cv_proto_ioctl_arg3=`echo "$y" | sed -e 's, arg,,'`
cl_cv_proto_ioctl_args="int, $cl_cv_proto_ioctl_arg2, $cl_cv_proto_ioctl_arg3"
fi
have_ioctl=1])
fi
done
done
if test -z "$have_ioctl"; then
CL_PROTO_MISSING(ioctl)
fi
], [extern int ioctl ($cl_cv_proto_ioctl_args);])
AC_DEFINE_UNQUOTED(IOCTL_REQUEST_T,$cl_cv_proto_ioctl_arg2,[type of `request' in ioctl() declaration])
if test $cl_cv_proto_ioctl_dots = yes; then
AC_DEFINE(IOCTL_DOTS,,[declaration of ioctl() needs dots])
else
AC_DEFINE_UNQUOTED(IOCTL_ARGUMENT_T,$cl_cv_proto_ioctl_arg3,[type of `argument' in ioctl() declaration, if not superseded by dots])
fi
ioctl_decl="$ioctl_decl1"
ioctl_prog='int x = FIONREAD;'
CL_COMPILE_CHECK([FIONREAD], cl_cv_decl_FIONREAD_1,
$ioctl_decl, $ioctl_prog, ioctl_ok=1)dnl
if test -z "$ioctl_ok"; then
CL_COMPILE_CHECK([FIONREAD in sys/filio.h], cl_cv_decl_FIONREAD_1_sys_filio_h,
$ioctl_decl[#include <sys/filio.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_FILIO_H,,[need <sys/filio.h> for using ioctl() FIONREAD])
ioctl_ok=1)dnl
fi
if test -z "$ioctl_ok"; then
CL_COMPILE_CHECK([FIONREAD in sys/ioctl.h], cl_cv_decl_FIONREAD_1_sys_ioctl_h,
$ioctl_decl[#include <sys/ioctl.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_IOCTL_H,,[need <sys/ioctl.h> for using ioctl() FIONREAD])
ioctl_ok=1)dnl
fi
if test -n "$ioctl_ok"; then
AC_DEFINE(HAVE_FIONREAD,,[have the FIONREAD ioctl()])
# Now check whether FIONREAD reliably checks for the EOF of a regular file.
# The number of available characters returned by ioctl(fd,FIONREAD,...) should
# be > 0 for a non-empty regular file at least. On Solaris 2, it is 0.
AC_CACHE_CHECK(for reliable FIONREAD, cl_cv_decl_FIONREAD_reliable, [
AC_TRY_RUN([
/* Declare ioctl(). */
]$ioctl_decl[
#ifdef NEED_SYS_FILIO_H
#include <sys/filio.h>
#endif
#ifdef NEED_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
/* Declare open(). */
#include <fcntl.h>
int main ()
{ int fd = open("conftest.c",O_RDONLY,0644);
  unsigned long bytes_ready;
  /* Clear bytes_ready before use. Some kernels (such as Linux-2.4.18 on ia64)
     apparently expect an 'int *', not a 'long *', as argument of this ioctl,
     and thus fill only part of the bytes_ready variable. Fortunately,
     endianness is not a problem here, because we only check whether
     bytes_ready is == 0 or != 0. */
  bytes_ready = 0;
  exit(!((fd >= 0) && (ioctl(fd,FIONREAD,&bytes_ready) >= 0) && (bytes_ready > 0)));
}],
cl_cv_decl_FIONREAD_reliable=yes, cl_cv_decl_FIONREAD_reliable=no,
dnl When cross-compiling, don't assume anything.
cl_cv_decl_FIONREAD_reliable="guessing no")
])
case "$cl_cv_decl_FIONREAD_reliable" in
  *yes) AC_DEFINE(HAVE_RELIABLE_FIONREAD,,[have the FIONREAD ioctl() and it works reliably on files]) ;;
  *no) ;;
esac
fi
fi
])

# libtool.m4 - Configure libtool for the host system. -*-Autoconf-*-

# serial 52 AC_PROG_LIBTOOL


# AC_PROVIDE_IFELSE(MACRO-NAME, IF-PROVIDED, IF-NOT-PROVIDED)
# -----------------------------------------------------------
# If this macro is not defined by Autoconf, define it here.
m4_ifdef([AC_PROVIDE_IFELSE],
         [],
         [m4_define([AC_PROVIDE_IFELSE],
	         [m4_ifdef([AC_PROVIDE_$1],
		           [$2], [$3])])])


# AC_PROG_LIBTOOL
# ---------------
AC_DEFUN([AC_PROG_LIBTOOL],
[AC_REQUIRE([_AC_PROG_LIBTOOL])dnl
dnl If AC_PROG_CXX has already been expanded, run AC_LIBTOOL_CXX
dnl immediately, otherwise, hook it in at the end of AC_PROG_CXX.
  AC_PROVIDE_IFELSE([AC_PROG_CXX],
    [AC_LIBTOOL_CXX],
    [define([AC_PROG_CXX], defn([AC_PROG_CXX])[AC_LIBTOOL_CXX
  ])])
dnl And a similar setup for Fortran 77 support
  AC_PROVIDE_IFELSE([AC_PROG_F77],
    [AC_LIBTOOL_F77],
    [define([AC_PROG_F77], defn([AC_PROG_F77])[AC_LIBTOOL_F77
])])

dnl Quote A][M_PROG_GCJ so that aclocal doesn't bring it in needlessly.
dnl If either AC_PROG_GCJ or A][M_PROG_GCJ have already been expanded, run
dnl AC_LIBTOOL_GCJ immediately, otherwise, hook it in at the end of both.
  AC_PROVIDE_IFELSE([AC_PROG_GCJ],
    [AC_LIBTOOL_GCJ],
    [AC_PROVIDE_IFELSE([A][M_PROG_GCJ],
      [AC_LIBTOOL_GCJ],
      [AC_PROVIDE_IFELSE([LT_AC_PROG_GCJ],
	[AC_LIBTOOL_GCJ],
      [ifdef([AC_PROG_GCJ],
	     [define([AC_PROG_GCJ], defn([AC_PROG_GCJ])[AC_LIBTOOL_GCJ])])
       ifdef([A][M_PROG_GCJ],
	     [define([A][M_PROG_GCJ], defn([A][M_PROG_GCJ])[AC_LIBTOOL_GCJ])])
       ifdef([LT_AC_PROG_GCJ],
	     [define([LT_AC_PROG_GCJ],
		defn([LT_AC_PROG_GCJ])[AC_LIBTOOL_GCJ])])])])
])])# AC_PROG_LIBTOOL


# _AC_PROG_LIBTOOL
# ----------------
AC_DEFUN([_AC_PROG_LIBTOOL],
[AC_REQUIRE([AC_LIBTOOL_SETUP])dnl
AC_BEFORE([$0],[AC_LIBTOOL_CXX])dnl
AC_BEFORE([$0],[AC_LIBTOOL_F77])dnl
AC_BEFORE([$0],[AC_LIBTOOL_GCJ])dnl

# This can be used to rebuild libtool when needed
LIBTOOL_DEPS="$ac_aux_dir/ltmain.sh"

# Always use our own libtool.
LIBTOOL='$(SHELL) $(top_builddir)/libtool'
AC_SUBST(LIBTOOL)dnl

# Prevent multiple expansion
define([AC_PROG_LIBTOOL], [])
])# _AC_PROG_LIBTOOL


# AC_LIBTOOL_SETUP
# ----------------
AC_DEFUN([AC_LIBTOOL_SETUP],
[AC_PREREQ(2.50)dnl
AC_REQUIRE([AC_ENABLE_SHARED])dnl
AC_REQUIRE([AC_ENABLE_STATIC])dnl
AC_REQUIRE([AC_ENABLE_FAST_INSTALL])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_PROG_LD])dnl
AC_REQUIRE([AC_PROG_LD_RELOAD_FLAG])dnl
AC_REQUIRE([AC_PROG_NM])dnl

AC_REQUIRE([AC_PROG_LN_S])dnl
AC_REQUIRE([AC_DEPLIBS_CHECK_METHOD])dnl
# Autoconf 2.13's AC_OBJEXT and AC_EXEEXT macros only works for C compilers!
AC_REQUIRE([AC_OBJEXT])dnl
AC_REQUIRE([AC_EXEEXT])dnl
dnl
AC_LIBTOOL_SYS_MAX_CMD_LEN
AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE
AC_LIBTOOL_OBJDIR

AC_REQUIRE([_LT_AC_SYS_COMPILER])dnl
_LT_AC_PROG_ECHO_BACKSLASH

case $host_os in
aix3*)
  # AIX sometimes has problems with the GCC collect2 program.  For some
  # reason, if we set the COLLECT_NAMES environment variable, the problems
  # vanish in a puff of smoke.
  if test "X${COLLECT_NAMES+set}" != Xset; then
    COLLECT_NAMES=
    export COLLECT_NAMES
  fi
  ;;
esac

# Sed substitution that helps us do robust quoting.  It backslashifies
# metacharacters that are still active within double-quoted strings.
Xsed='sed -e 1s/^X//'
[sed_quote_subst='s/\([\\"\\`$\\\\]\)/\\\1/g']

# Same as above, but do not quote variable references.
[double_quote_subst='s/\([\\"\\`\\\\]\)/\\\1/g']

# Sed substitution to delay expansion of an escaped shell variable in a
# double_quote_subst'ed string.
delay_variable_subst='s/\\\\\\\\\\\$/\\\\\\$/g'

# Sed substitution to avoid accidental globbing in evaled expressions
no_glob_subst='s/\*/\\\*/g'

# Constants:
rm="rm -f"

# Global variables:
default_ofile=libtool
can_build_shared=yes

# All known linkers require a `.a' archive for static linking (except MSVC,
# which needs '.lib').
libext=a
ltmain="$ac_aux_dir/ltmain.sh"
ofile="$default_ofile"
with_gnu_ld="$lt_cv_prog_gnu_ld"

AC_CHECK_TOOL(AR, ar, false)
AC_CHECK_TOOL(RANLIB, ranlib, :)
AC_CHECK_TOOL(STRIP, strip, :)

old_CC="$CC"
old_CFLAGS="$CFLAGS"

# Set sane defaults for various variables
test -z "$AR" && AR=ar
test -z "$AR_FLAGS" && AR_FLAGS=cru
test -z "$AS" && AS=as
test -z "$CC" && CC=cc
test -z "$LTCC" && LTCC=$CC
test -z "$LTCFLAGS" && LTCFLAGS=$CFLAGS
test -z "$DLLTOOL" && DLLTOOL=dlltool
test -z "$LD" && LD=ld
test -z "$LN_S" && LN_S="ln -s"
test -z "$MAGIC_CMD" && MAGIC_CMD=file
test -z "$NM" && NM=nm
test -z "$SED" && SED=sed
test -z "$OBJDUMP" && OBJDUMP=objdump
test -z "$RANLIB" && RANLIB=:
test -z "$STRIP" && STRIP=:
test -z "$ac_objext" && ac_objext=o

# Determine commands to create old-style static archives.
old_archive_cmds='$AR $AR_FLAGS $oldlib$oldobjs'
old_postinstall_cmds='chmod 644 $oldlib'
old_postuninstall_cmds=

if test -n "$RANLIB"; then
  case $host_os in
  openbsd*)
    old_postinstall_cmds="$old_postinstall_cmds~\$RANLIB -t \$oldlib"
    ;;
  *)
    old_postinstall_cmds="$old_postinstall_cmds~\$RANLIB \$oldlib"
    ;;
  esac
  old_archive_cmds="$old_archive_cmds~\$RANLIB \$oldlib"
fi

_LT_CC_BASENAME([$compiler])

# Only perform the check for file, if the check method requires it
case $deplibs_check_method in
file_magic*)
  if test "$file_magic_cmd" = '$MAGIC_CMD'; then
    AC_PATH_MAGIC
  fi
  ;;
esac

_LT_REQUIRED_DARWIN_CHECKS

AC_PROVIDE_IFELSE([AC_LIBTOOL_DLOPEN], enable_dlopen=yes, enable_dlopen=no)
AC_PROVIDE_IFELSE([AC_LIBTOOL_WIN32_DLL],
enable_win32_dll=yes, enable_win32_dll=no)

AC_ARG_ENABLE([libtool-lock],
    [AC_HELP_STRING([--disable-libtool-lock],
	[avoid locking (might break parallel builds)])])
test "x$enable_libtool_lock" != xno && enable_libtool_lock=yes

AC_ARG_WITH([pic],
    [AC_HELP_STRING([--with-pic],
	[try to use only PIC/non-PIC objects @<:@default=use both@:>@])],
    [pic_mode="$withval"],
    [pic_mode=default])
test -z "$pic_mode" && pic_mode=default

# Use C for the default configuration in the libtool script
tagname=
AC_LIBTOOL_LANG_C_CONFIG
_LT_AC_TAGCONFIG
])# AC_LIBTOOL_SETUP


# _LT_AC_SYS_COMPILER
# -------------------
AC_DEFUN([_LT_AC_SYS_COMPILER],
[AC_REQUIRE([AC_PROG_CC])dnl

# If no C compiler was specified, use CC.
LTCC=${LTCC-"$CC"}

# If no C compiler flags were specified, use CFLAGS.
LTCFLAGS=${LTCFLAGS-"$CFLAGS"}

# Allow CC to be a program name with arguments.
compiler=$CC
])# _LT_AC_SYS_COMPILER


# _LT_CC_BASENAME(CC)
# -------------------
# Calculate cc_basename.  Skip known compiler wrappers and cross-prefix.
AC_DEFUN([_LT_CC_BASENAME],
[for cc_temp in $1""; do
  case $cc_temp in
    compile | *[[\\/]]compile | ccache | *[[\\/]]ccache ) ;;
    distcc | *[[\\/]]distcc | purify | *[[\\/]]purify ) ;;
    \-*) ;;
    *) break;;
  esac
done
cc_basename=`$echo "X$cc_temp" | $Xsed -e 's%.*/%%' -e "s%^$host_alias-%%"`
])


# _LT_COMPILER_BOILERPLATE
# ------------------------
# Check for compiler boilerplate output or warnings with
# the simple compiler test code.
AC_DEFUN([_LT_COMPILER_BOILERPLATE],
[AC_REQUIRE([LT_AC_PROG_SED])dnl
ac_outfile=conftest.$ac_objext
echo "$lt_simple_compile_test_code" >conftest.$ac_ext
eval "$ac_compile" 2>&1 >/dev/null | $SED '/^$/d; /^ *+/d' >conftest.err
_lt_compiler_boilerplate=`cat conftest.err`
$rm conftest*
])# _LT_COMPILER_BOILERPLATE


# _LT_LINKER_BOILERPLATE
# ----------------------
# Check for linker boilerplate output or warnings with
# the simple link test code.
AC_DEFUN([_LT_LINKER_BOILERPLATE],
[AC_REQUIRE([LT_AC_PROG_SED])dnl
ac_outfile=conftest.$ac_objext
echo "$lt_simple_link_test_code" >conftest.$ac_ext
eval "$ac_link" 2>&1 >/dev/null | $SED '/^$/d; /^ *+/d' >conftest.err
_lt_linker_boilerplate=`cat conftest.err`
$rm -r conftest*
])# _LT_LINKER_BOILERPLATE

# _LT_REQUIRED_DARWIN_CHECKS
# --------------------------
# Check for some things on darwin
AC_DEFUN([_LT_REQUIRED_DARWIN_CHECKS],[
  case $host_os in
    rhapsody* | darwin*)
    AC_CHECK_TOOL([DSYMUTIL], [dsymutil], [:])
    AC_CHECK_TOOL([NMEDIT], [nmedit], [:])

    AC_CACHE_CHECK([for -single_module linker flag],[lt_cv_apple_cc_single_mod],
      [lt_cv_apple_cc_single_mod=no
      if test -z "${LT_MULTI_MODULE}"; then
   # By default we will add the -single_module flag. You can override
   # by either setting the environment variable LT_MULTI_MODULE
   # non-empty at configure time, or by adding -multi_module to the
   # link flags.
   echo "int foo(void){return 1;}" > conftest.c
   $LTCC $LTCFLAGS $LDFLAGS -o libconftest.dylib \
     -dynamiclib ${wl}-single_module conftest.c
   if test -f libconftest.dylib; then
     lt_cv_apple_cc_single_mod=yes
     rm -rf libconftest.dylib*
   fi
   rm conftest.c
      fi])
    AC_CACHE_CHECK([for -exported_symbols_list linker flag],
      [lt_cv_ld_exported_symbols_list],
      [lt_cv_ld_exported_symbols_list=no
      save_LDFLAGS=$LDFLAGS
      echo "_main" > conftest.sym
      LDFLAGS="$LDFLAGS -Wl,-exported_symbols_list,conftest.sym"
      AC_LINK_IFELSE([AC_LANG_PROGRAM([],[])],
   [lt_cv_ld_exported_symbols_list=yes],
   [lt_cv_ld_exported_symbols_list=no])
   LDFLAGS="$save_LDFLAGS"
    ])
    case $host_os in
    rhapsody* | darwin1.[[0123]])
      _lt_dar_allow_undefined='${wl}-undefined ${wl}suppress' ;;
    darwin1.*)
     _lt_dar_allow_undefined='${wl}-flat_namespace ${wl}-undefined ${wl}suppress' ;;
    darwin*)
      # if running on 10.5 or later, the deployment target defaults
      # to the OS version, if on x86, and 10.4, the deployment
      # target defaults to 10.4. Don't you love it?
      case ${MACOSX_DEPLOYMENT_TARGET-10.0},$host in
   10.0,*86*-darwin8*|10.0,*-darwin[[91]]*)
     _lt_dar_allow_undefined='${wl}-undefined ${wl}dynamic_lookup' ;;
   10.[[012]]*)
     _lt_dar_allow_undefined='${wl}-flat_namespace ${wl}-undefined ${wl}suppress' ;;
   10.*)
     _lt_dar_allow_undefined='${wl}-undefined ${wl}dynamic_lookup' ;;
      esac
    ;;
  esac
    if test "$lt_cv_apple_cc_single_mod" = "yes"; then
      _lt_dar_single_mod='$single_module'
    fi
    if test "$lt_cv_ld_exported_symbols_list" = "yes"; then
      _lt_dar_export_syms=' ${wl}-exported_symbols_list,$output_objdir/${libname}-symbols.expsym'
    else
      _lt_dar_export_syms="~$NMEDIT -s \$output_objdir/\${libname}-symbols.expsym \${lib}"
    fi
    if test "$DSYMUTIL" != ":"; then
      _lt_dsymutil="~$DSYMUTIL \$lib || :"
    else
      _lt_dsymutil=
    fi
    ;;
  esac
])

# _LT_AC_SYS_LIBPATH_AIX
# ----------------------
# Links a minimal program and checks the executable
# for the system default hardcoded library path. In most cases,
# this is /usr/lib:/lib, but when the MPI compilers are used
# the location of the communication and MPI libs are included too.
# If we don't find anything, use the default library path according
# to the aix ld manual.
AC_DEFUN([_LT_AC_SYS_LIBPATH_AIX],
[AC_REQUIRE([LT_AC_PROG_SED])dnl
AC_LINK_IFELSE(AC_LANG_PROGRAM,[
lt_aix_libpath_sed='
    /Import File Strings/,/^$/ {
	/^0/ {
	    s/^0  *\(.*\)$/\1/
	    p
	}
    }'
aix_libpath=`dump -H conftest$ac_exeext 2>/dev/null | $SED -n -e "$lt_aix_libpath_sed"`
# Check for a 64-bit object if we didn't find anything.
if test -z "$aix_libpath"; then
  aix_libpath=`dump -HX64 conftest$ac_exeext 2>/dev/null | $SED -n -e "$lt_aix_libpath_sed"`
fi],[])
if test -z "$aix_libpath"; then aix_libpath="/usr/lib:/lib"; fi
])# _LT_AC_SYS_LIBPATH_AIX


# _LT_AC_SHELL_INIT(ARG)
# ----------------------
AC_DEFUN([_LT_AC_SHELL_INIT],
[ifdef([AC_DIVERSION_NOTICE],
	     [AC_DIVERT_PUSH(AC_DIVERSION_NOTICE)],
	 [AC_DIVERT_PUSH(NOTICE)])
$1
AC_DIVERT_POP
])# _LT_AC_SHELL_INIT


# _LT_AC_PROG_ECHO_BACKSLASH
# --------------------------
# Add some code to the start of the generated configure script which
# will find an echo command which doesn't interpret backslashes.
AC_DEFUN([_LT_AC_PROG_ECHO_BACKSLASH],
[_LT_AC_SHELL_INIT([
# Check that we are running under the correct shell.
SHELL=${CONFIG_SHELL-/bin/sh}

case X$ECHO in
X*--fallback-echo)
  # Remove one level of quotation (which was required for Make).
  ECHO=`echo "$ECHO" | sed 's,\\\\\[$]\\[$]0,'[$]0','`
  ;;
esac

echo=${ECHO-echo}
if test "X[$]1" = X--no-reexec; then
  # Discard the --no-reexec flag, and continue.
  shift
elif test "X[$]1" = X--fallback-echo; then
  # Avoid inline document here, it may be left over
  :
elif test "X`($echo '\t') 2>/dev/null`" = 'X\t' ; then
  # Yippee, $echo works!
  :
else
  # Restart under the correct shell.
  exec $SHELL "[$]0" --no-reexec ${1+"[$]@"}
fi

if test "X[$]1" = X--fallback-echo; then
  # used as fallback echo
  shift
  cat <<EOF
[$]*
EOF
  exit 0
fi

# The HP-UX ksh and POSIX shell print the target directory to stdout
# if CDPATH is set.
(unset CDPATH) >/dev/null 2>&1 && unset CDPATH

if test -z "$ECHO"; then
if test "X${echo_test_string+set}" != Xset; then
# find a string as large as possible, as long as the shell can cope with it
  for cmd in 'sed 50q "[$]0"' 'sed 20q "[$]0"' 'sed 10q "[$]0"' 'sed 2q "[$]0"' 'echo test'; do
    # expected sizes: less than 2Kb, 1Kb, 512 bytes, 16 bytes, ...
    if (echo_test_string=`eval $cmd`) 2>/dev/null &&
       echo_test_string=`eval $cmd` &&
       (test "X$echo_test_string" = "X$echo_test_string") 2>/dev/null
    then
      break
    fi
  done
fi

if test "X`($echo '\t') 2>/dev/null`" = 'X\t' &&
   echo_testing_string=`($echo "$echo_test_string") 2>/dev/null` &&
   test "X$echo_testing_string" = "X$echo_test_string"; then
  :
else
  # The Solaris, AIX, and Digital Unix default echo programs unquote
  # backslashes.  This makes it impossible to quote backslashes using
  #   echo "$something" | sed 's/\\/\\\\/g'
  #
  # So, first we look for a working echo in the user's PATH.

  lt_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
  for dir in $PATH /usr/ucb; do
    IFS="$lt_save_ifs"
    if (test -f $dir/echo || test -f $dir/echo$ac_exeext) &&
       test "X`($dir/echo '\t') 2>/dev/null`" = 'X\t' &&
       echo_testing_string=`($dir/echo "$echo_test_string") 2>/dev/null` &&
       test "X$echo_testing_string" = "X$echo_test_string"; then
      echo="$dir/echo"
      break
    fi
  done
  IFS="$lt_save_ifs"

  if test "X$echo" = Xecho; then
    # We didn't find a better echo, so look for alternatives.
    if test "X`(print -r '\t') 2>/dev/null`" = 'X\t' &&
       echo_testing_string=`(print -r "$echo_test_string") 2>/dev/null` &&
       test "X$echo_testing_string" = "X$echo_test_string"; then
      # This shell has a builtin print -r that does the trick.
      echo='print -r'
    elif (test -f /bin/ksh || test -f /bin/ksh$ac_exeext) &&
	 test "X$CONFIG_SHELL" != X/bin/ksh; then
      # If we have ksh, try running configure again with it.
      ORIGINAL_CONFIG_SHELL=${CONFIG_SHELL-/bin/sh}
      export ORIGINAL_CONFIG_SHELL
      CONFIG_SHELL=/bin/ksh
      export CONFIG_SHELL
      exec $CONFIG_SHELL "[$]0" --no-reexec ${1+"[$]@"}
    else
      # Try using printf.
      echo='printf %s\n'
      if test "X`($echo '\t') 2>/dev/null`" = 'X\t' &&
	 echo_testing_string=`($echo "$echo_test_string") 2>/dev/null` &&
	 test "X$echo_testing_string" = "X$echo_test_string"; then
	# Cool, printf works
	:
      elif echo_testing_string=`($ORIGINAL_CONFIG_SHELL "[$]0" --fallback-echo '\t') 2>/dev/null` &&
	   test "X$echo_testing_string" = 'X\t' &&
	   echo_testing_string=`($ORIGINAL_CONFIG_SHELL "[$]0" --fallback-echo "$echo_test_string") 2>/dev/null` &&
	   test "X$echo_testing_string" = "X$echo_test_string"; then
	CONFIG_SHELL=$ORIGINAL_CONFIG_SHELL
	export CONFIG_SHELL
	SHELL="$CONFIG_SHELL"
	export SHELL
	echo="$CONFIG_SHELL [$]0 --fallback-echo"
      elif echo_testing_string=`($CONFIG_SHELL "[$]0" --fallback-echo '\t') 2>/dev/null` &&
	   test "X$echo_testing_string" = 'X\t' &&
	   echo_testing_string=`($CONFIG_SHELL "[$]0" --fallback-echo "$echo_test_string") 2>/dev/null` &&
	   test "X$echo_testing_string" = "X$echo_test_string"; then
	echo="$CONFIG_SHELL [$]0 --fallback-echo"
      else
	# maybe with a smaller string...
	prev=:

	for cmd in 'echo test' 'sed 2q "[$]0"' 'sed 10q "[$]0"' 'sed 20q "[$]0"' 'sed 50q "[$]0"'; do
	  if (test "X$echo_test_string" = "X`eval $cmd`") 2>/dev/null
	  then
	    break
	  fi
	  prev="$cmd"
	done

	if test "$prev" != 'sed 50q "[$]0"'; then
	  echo_test_string=`eval $prev`
	  export echo_test_string
	  exec ${ORIGINAL_CONFIG_SHELL-${CONFIG_SHELL-/bin/sh}} "[$]0" ${1+"[$]@"}
	else
	  # Oops.  We lost completely, so just stick with echo.
	  echo=echo
	fi
      fi
    fi
  fi
fi
fi

# Copy echo and quote the copy suitably for passing to libtool from
# the Makefile, instead of quoting the original, which is used later.
ECHO=$echo
if test "X$ECHO" = "X$CONFIG_SHELL [$]0 --fallback-echo"; then
   ECHO="$CONFIG_SHELL \\\$\[$]0 --fallback-echo"
fi

AC_SUBST(ECHO)
])])# _LT_AC_PROG_ECHO_BACKSLASH


# _LT_AC_LOCK
# -----------
AC_DEFUN([_LT_AC_LOCK],
[AC_ARG_ENABLE([libtool-lock],
    [AC_HELP_STRING([--disable-libtool-lock],
	[avoid locking (might break parallel builds)])])
test "x$enable_libtool_lock" != xno && enable_libtool_lock=yes

# Some flags need to be propagated to the compiler or linker for good
# libtool support.
case $host in
ia64-*-hpux*)
  # Find out which ABI we are using.
  echo 'int i;' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
    case `/usr/bin/file conftest.$ac_objext` in
    *ELF-32*)
      HPUX_IA64_MODE="32"
      ;;
    *ELF-64*)
      HPUX_IA64_MODE="64"
      ;;
    esac
  fi
  rm -rf conftest*
  ;;
*-*-irix6*)
  # Find out which ABI we are using.
  echo '[#]line __oline__ "configure"' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
   if test "$lt_cv_prog_gnu_ld" = yes; then
    case `/usr/bin/file conftest.$ac_objext` in
    *32-bit*)
      LD="${LD-ld} -melf32bsmip"
      ;;
    *N32*)
      LD="${LD-ld} -melf32bmipn32"
      ;;
    *64-bit*)
      LD="${LD-ld} -melf64bmip"
      ;;
    esac
   else
    case `/usr/bin/file conftest.$ac_objext` in
    *32-bit*)
      LD="${LD-ld} -32"
      ;;
    *N32*)
      LD="${LD-ld} -n32"
      ;;
    *64-bit*)
      LD="${LD-ld} -64"
      ;;
    esac
   fi
  fi
  rm -rf conftest*
  ;;

x86_64-*kfreebsd*-gnu|x86_64-*linux*|ppc*-*linux*|powerpc*-*linux*| \
s390*-*linux*|sparc*-*linux*)
  # Find out which ABI we are using.
  echo 'int i;' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
    case `/usr/bin/file conftest.o` in
    *32-bit*)
      case $host in
        x86_64-*kfreebsd*-gnu)
          LD="${LD-ld} -m elf_i386_fbsd"
          ;;
        x86_64-*linux*)
          LD="${LD-ld} -m elf_i386"
          ;;
        ppc64-*linux*|powerpc64-*linux*)
          LD="${LD-ld} -m elf32ppclinux"
          ;;
        s390x-*linux*)
          LD="${LD-ld} -m elf_s390"
          ;;
        sparc64-*linux*)
          LD="${LD-ld} -m elf32_sparc"
          ;;
      esac
      ;;
    *64-bit*)
      case $host in
        x86_64-*kfreebsd*-gnu)
          LD="${LD-ld} -m elf_x86_64_fbsd"
          ;;
        x86_64-*linux*)
          LD="${LD-ld} -m elf_x86_64"
          ;;
        ppc*-*linux*|powerpc*-*linux*)
          LD="${LD-ld} -m elf64ppc"
          ;;
        s390*-*linux*)
          LD="${LD-ld} -m elf64_s390"
          ;;
        sparc*-*linux*)
          LD="${LD-ld} -m elf64_sparc"
          ;;
      esac
      ;;
    esac
  fi
  rm -rf conftest*
  ;;

*-*-sco3.2v5*)
  # On SCO OpenServer 5, we need -belf to get full-featured binaries.
  SAVE_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -belf"
  AC_CACHE_CHECK([whether the C compiler needs -belf], lt_cv_cc_needs_belf,
    [AC_LANG_PUSH(C)
     AC_TRY_LINK([],[],[lt_cv_cc_needs_belf=yes],[lt_cv_cc_needs_belf=no])
     AC_LANG_POP])
  if test x"$lt_cv_cc_needs_belf" != x"yes"; then
    # this is probably gcc 2.8.0, egcs 1.0 or newer; no need for -belf
    CFLAGS="$SAVE_CFLAGS"
  fi
  ;;
sparc*-*solaris*)
  # Find out which ABI we are using.
  echo 'int i;' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
    case `/usr/bin/file conftest.o` in
    *64-bit*)
      case $lt_cv_prog_gnu_ld in
      yes*) LD="${LD-ld} -m elf64_sparc" ;;
      *)
        if ${LD-ld} -64 -r -o conftest2.o conftest.o >/dev/null 2>&1; then
	  LD="${LD-ld} -64"
	fi
	;;
      esac
      ;;
    esac
  fi
  rm -rf conftest*
  ;;

AC_PROVIDE_IFELSE([AC_LIBTOOL_WIN32_DLL],
[*-*-cygwin* | *-*-mingw* | *-*-pw32*)
  AC_CHECK_TOOL(DLLTOOL, dlltool, false)
  AC_CHECK_TOOL(AS, as, false)
  AC_CHECK_TOOL(OBJDUMP, objdump, false)
  ;;
  ])
esac

need_locks="$enable_libtool_lock"

])# _LT_AC_LOCK


# AC_LIBTOOL_COMPILER_OPTION(MESSAGE, VARIABLE-NAME, FLAGS,
#		[OUTPUT-FILE], [ACTION-SUCCESS], [ACTION-FAILURE])
# ----------------------------------------------------------------
# Check whether the given compiler option works
AC_DEFUN([AC_LIBTOOL_COMPILER_OPTION],
[AC_REQUIRE([LT_AC_PROG_SED])
AC_CACHE_CHECK([$1], [$2],
  [$2=no
  ifelse([$4], , [ac_outfile=conftest.$ac_objext], [ac_outfile=$4])
   echo "$lt_simple_compile_test_code" > conftest.$ac_ext
   lt_compiler_flag="$3"
   # Insert the option either (1) after the last *FLAGS variable, or
   # (2) before a word containing "conftest.", or (3) at the end.
   # Note that $ac_compile itself does not contain backslashes and begins
   # with a dollar sign (not a hyphen), so the echo should work correctly.
   # The option is referenced via a variable to avoid confusing sed.
   lt_compile=`echo "$ac_compile" | $SED \
   -e 's:.*FLAGS}\{0,1\} :&$lt_compiler_flag :; t' \
   -e 's: [[^ ]]*conftest\.: $lt_compiler_flag&:; t' \
   -e 's:$: $lt_compiler_flag:'`
   (eval echo "\"\$as_me:__oline__: $lt_compile\"" >&AS_MESSAGE_LOG_FD)
   (eval "$lt_compile" 2>conftest.err)
   ac_status=$?
   cat conftest.err >&AS_MESSAGE_LOG_FD
   echo "$as_me:__oline__: \$? = $ac_status" >&AS_MESSAGE_LOG_FD
   if (exit $ac_status) && test -s "$ac_outfile"; then
     # The compiler can only warn and ignore the option if not recognized
     # So say no if there are warnings other than the usual output.
     $echo "X$_lt_compiler_boilerplate" | $Xsed -e '/^$/d' >conftest.exp
     $SED '/^$/d; /^ *+/d' conftest.err >conftest.er2
     if test ! -s conftest.er2 || diff conftest.exp conftest.er2 >/dev/null; then
       $2=yes
     fi
   fi
   $rm conftest*
])

if test x"[$]$2" = xyes; then
    ifelse([$5], , :, [$5])
else
    ifelse([$6], , :, [$6])
fi
])# AC_LIBTOOL_COMPILER_OPTION


# AC_LIBTOOL_LINKER_OPTION(MESSAGE, VARIABLE-NAME, FLAGS,
#                          [ACTION-SUCCESS], [ACTION-FAILURE])
# ------------------------------------------------------------
# Check whether the given compiler option works
AC_DEFUN([AC_LIBTOOL_LINKER_OPTION],
[AC_REQUIRE([LT_AC_PROG_SED])dnl
AC_CACHE_CHECK([$1], [$2],
  [$2=no
   save_LDFLAGS="$LDFLAGS"
   LDFLAGS="$LDFLAGS $3"
   echo "$lt_simple_link_test_code" > conftest.$ac_ext
   if (eval $ac_link 2>conftest.err) && test -s conftest$ac_exeext; then
     # The linker can only warn and ignore the option if not recognized
     # So say no if there are warnings
     if test -s conftest.err; then
       # Append any errors to the config.log.
       cat conftest.err 1>&AS_MESSAGE_LOG_FD
       $echo "X$_lt_linker_boilerplate" | $Xsed -e '/^$/d' > conftest.exp
       $SED '/^$/d; /^ *+/d' conftest.err >conftest.er2
       if diff conftest.exp conftest.er2 >/dev/null; then
         $2=yes
       fi
     else
       $2=yes
     fi
   fi
   $rm -r conftest*
   LDFLAGS="$save_LDFLAGS"
])

if test x"[$]$2" = xyes; then
    ifelse([$4], , :, [$4])
else
    ifelse([$5], , :, [$5])
fi
])# AC_LIBTOOL_LINKER_OPTION


# AC_LIBTOOL_SYS_MAX_CMD_LEN
# --------------------------
AC_DEFUN([AC_LIBTOOL_SYS_MAX_CMD_LEN],
[# find the maximum length of command line arguments
AC_MSG_CHECKING([the maximum length of command line arguments])
AC_CACHE_VAL([lt_cv_sys_max_cmd_len], [dnl
  i=0
  teststring="ABCD"

  case $build_os in
  msdosdjgpp*)
    # On DJGPP, this test can blow up pretty badly due to problems in libc
    # (any single argument exceeding 2000 bytes causes a buffer overrun
    # during glob expansion).  Even if it were fixed, the result of this
    # check would be larger than it should be.
    lt_cv_sys_max_cmd_len=12288;    # 12K is about right
    ;;

  gnu*)
    # Under GNU Hurd, this test is not required because there is
    # no limit to the length of command line arguments.
    # Libtool will interpret -1 as no limit whatsoever
    lt_cv_sys_max_cmd_len=-1;
    ;;

  cygwin* | mingw*)
    # On Win9x/ME, this test blows up -- it succeeds, but takes
    # about 5 minutes as the teststring grows exponentially.
    # Worse, since 9x/ME are not pre-emptively multitasking,
    # you end up with a "frozen" computer, even though with patience
    # the test eventually succeeds (with a max line length of 256k).
    # Instead, let's just punt: use the minimum linelength reported by
    # all of the supported platforms: 8192 (on NT/2K/XP).
    lt_cv_sys_max_cmd_len=8192;
    ;;

  amigaos*)
    # On AmigaOS with pdksh, this test takes hours, literally.
    # So we just punt and use a minimum line length of 8192.
    lt_cv_sys_max_cmd_len=8192;
    ;;

  netbsd* | freebsd* | openbsd* | darwin* | dragonfly*)
    # This has been around since 386BSD, at least.  Likely further.
    if test -x /sbin/sysctl; then
      lt_cv_sys_max_cmd_len=`/sbin/sysctl -n kern.argmax`
    elif test -x /usr/sbin/sysctl; then
      lt_cv_sys_max_cmd_len=`/usr/sbin/sysctl -n kern.argmax`
    else
      lt_cv_sys_max_cmd_len=65536	# usable default for all BSDs
    fi
    # And add a safety zone
    lt_cv_sys_max_cmd_len=`expr $lt_cv_sys_max_cmd_len \/ 4`
    lt_cv_sys_max_cmd_len=`expr $lt_cv_sys_max_cmd_len \* 3`
    ;;

  interix*)
    # We know the value 262144 and hardcode it with a safety zone (like BSD)
    lt_cv_sys_max_cmd_len=196608
    ;;

  osf*)
    # Dr. Hans Ekkehard Plesser reports seeing a kernel panic running configure
    # due to this test when exec_disable_arg_limit is 1 on Tru64. It is not
    # nice to cause kernel panics so lets avoid the loop below.
    # First set a reasonable default.
    lt_cv_sys_max_cmd_len=16384
    #
    if test -x /sbin/sysconfig; then
      case `/sbin/sysconfig -q proc exec_disable_arg_limit` in
        *1*) lt_cv_sys_max_cmd_len=-1 ;;
      esac
    fi
    ;;
  sco3.2v5*)
    lt_cv_sys_max_cmd_len=102400
    ;;
  sysv5* | sco5v6* | sysv4.2uw2*)
    kargmax=`grep ARG_MAX /etc/conf/cf.d/stune 2>/dev/null`
    if test -n "$kargmax"; then
      lt_cv_sys_max_cmd_len=`echo $kargmax | sed 's/.*[[ 	]]//'`
    else
      lt_cv_sys_max_cmd_len=32768
    fi
    ;;
  *)
    lt_cv_sys_max_cmd_len=`(getconf ARG_MAX) 2> /dev/null`
    if test -n "$lt_cv_sys_max_cmd_len"; then
      lt_cv_sys_max_cmd_len=`expr $lt_cv_sys_max_cmd_len \/ 4`
      lt_cv_sys_max_cmd_len=`expr $lt_cv_sys_max_cmd_len \* 3`
    else
      SHELL=${SHELL-${CONFIG_SHELL-/bin/sh}}
      while (test "X"`$SHELL [$]0 --fallback-echo "X$teststring" 2>/dev/null` \
	       = "XX$teststring") >/dev/null 2>&1 &&
	      new_result=`expr "X$teststring" : ".*" 2>&1` &&
	      lt_cv_sys_max_cmd_len=$new_result &&
	      test $i != 17 # 1/2 MB should be enough
      do
        i=`expr $i + 1`
        teststring=$teststring$teststring
      done
      teststring=
      # Add a significant safety factor because C++ compilers can tack on massive
      # amounts of additional arguments before passing them to the linker.
      # It appears as though 1/2 is a usable value.
      lt_cv_sys_max_cmd_len=`expr $lt_cv_sys_max_cmd_len \/ 2`
    fi
    ;;
  esac
])
if test -n $lt_cv_sys_max_cmd_len ; then
  AC_MSG_RESULT($lt_cv_sys_max_cmd_len)
else
  AC_MSG_RESULT(none)
fi
])# AC_LIBTOOL_SYS_MAX_CMD_LEN


# _LT_AC_CHECK_DLFCN
# ------------------
AC_DEFUN([_LT_AC_CHECK_DLFCN],
[AC_CHECK_HEADERS(dlfcn.h)dnl
])# _LT_AC_CHECK_DLFCN


# _LT_AC_TRY_DLOPEN_SELF (ACTION-IF-TRUE, ACTION-IF-TRUE-W-USCORE,
#                           ACTION-IF-FALSE, ACTION-IF-CROSS-COMPILING)
# ---------------------------------------------------------------------
AC_DEFUN([_LT_AC_TRY_DLOPEN_SELF],
[AC_REQUIRE([_LT_AC_CHECK_DLFCN])dnl
if test "$cross_compiling" = yes; then :
  [$4]
else
  lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
  lt_status=$lt_dlunknown
  cat > conftest.$ac_ext <<EOF
[#line __oline__ "configure"
#include "confdefs.h"

#if HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#include <stdio.h>

#ifdef RTLD_GLOBAL
#  define LT_DLGLOBAL		RTLD_GLOBAL
#else
#  ifdef DL_GLOBAL
#    define LT_DLGLOBAL		DL_GLOBAL
#  else
#    define LT_DLGLOBAL		0
#  endif
#endif

/* We may have to define LT_DLLAZY_OR_NOW in the command line if we
   find out it does not work in some platform. */
#ifndef LT_DLLAZY_OR_NOW
#  ifdef RTLD_LAZY
#    define LT_DLLAZY_OR_NOW		RTLD_LAZY
#  else
#    ifdef DL_LAZY
#      define LT_DLLAZY_OR_NOW		DL_LAZY
#    else
#      ifdef RTLD_NOW
#        define LT_DLLAZY_OR_NOW	RTLD_NOW
#      else
#        ifdef DL_NOW
#          define LT_DLLAZY_OR_NOW	DL_NOW
#        else
#          define LT_DLLAZY_OR_NOW	0
#        endif
#      endif
#    endif
#  endif
#endif

#ifdef __cplusplus
extern "C" void exit (int);
#endif

void fnord() { int i=42;}
int main ()
{
  void *self = dlopen (0, LT_DLGLOBAL|LT_DLLAZY_OR_NOW);
  int status = $lt_dlunknown;

  if (self)
    {
      if (dlsym (self,"fnord"))       status = $lt_dlno_uscore;
      else if (dlsym( self,"_fnord")) status = $lt_dlneed_uscore;
      /* dlclose (self); */
    }
  else
    puts (dlerror ());

    exit (status);
}]
EOF
  if AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext} 2>/dev/null; then
    (./conftest; exit; ) >&AS_MESSAGE_LOG_FD 2>/dev/null
    lt_status=$?
    case x$lt_status in
      x$lt_dlno_uscore) $1 ;;
      x$lt_dlneed_uscore) $2 ;;
      x$lt_dlunknown|x*) $3 ;;
    esac
  else :
    # compilation failed
    $3
  fi
fi
rm -fr conftest*
])# _LT_AC_TRY_DLOPEN_SELF


# AC_LIBTOOL_DLOPEN_SELF
# ----------------------
AC_DEFUN([AC_LIBTOOL_DLOPEN_SELF],
[AC_REQUIRE([_LT_AC_CHECK_DLFCN])dnl
if test "x$enable_dlopen" != xyes; then
  enable_dlopen=unknown
  enable_dlopen_self=unknown
  enable_dlopen_self_static=unknown
else
  lt_cv_dlopen=no
  lt_cv_dlopen_libs=

  case $host_os in
  beos*)
    lt_cv_dlopen="load_add_on"
    lt_cv_dlopen_libs=
    lt_cv_dlopen_self=yes
    ;;

  mingw* | pw32*)
    lt_cv_dlopen="LoadLibrary"
    lt_cv_dlopen_libs=
   ;;

  cygwin*)
    lt_cv_dlopen="dlopen"
    lt_cv_dlopen_libs=
   ;;

  darwin*)
  # if libdl is installed we need to link against it
    AC_CHECK_LIB([dl], [dlopen],
		[lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-ldl"],[
    lt_cv_dlopen="dyld"
    lt_cv_dlopen_libs=
    lt_cv_dlopen_self=yes
    ])
   ;;

  *)
    AC_CHECK_FUNC([shl_load],
	  [lt_cv_dlopen="shl_load"],
      [AC_CHECK_LIB([dld], [shl_load],
	    [lt_cv_dlopen="shl_load" lt_cv_dlopen_libs="-ldld"],
	[AC_CHECK_FUNC([dlopen],
	      [lt_cv_dlopen="dlopen"],
	  [AC_CHECK_LIB([dl], [dlopen],
		[lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-ldl"],
	    [AC_CHECK_LIB([svld], [dlopen],
		  [lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-lsvld"],
	      [AC_CHECK_LIB([dld], [dld_link],
		    [lt_cv_dlopen="dld_link" lt_cv_dlopen_libs="-ldld"])
	      ])
	    ])
	  ])
	])
      ])
    ;;
  esac

  if test "x$lt_cv_dlopen" != xno; then
    enable_dlopen=yes
  else
    enable_dlopen=no
  fi

  case $lt_cv_dlopen in
  dlopen)
    save_CPPFLAGS="$CPPFLAGS"
    test "x$ac_cv_header_dlfcn_h" = xyes && CPPFLAGS="$CPPFLAGS -DHAVE_DLFCN_H"

    save_LDFLAGS="$LDFLAGS"
    wl=$lt_prog_compiler_wl eval LDFLAGS=\"\$LDFLAGS $export_dynamic_flag_spec\"

    save_LIBS="$LIBS"
    LIBS="$lt_cv_dlopen_libs $LIBS"

    AC_CACHE_CHECK([whether a program can dlopen itself],
	  lt_cv_dlopen_self, [dnl
	  _LT_AC_TRY_DLOPEN_SELF(
	    lt_cv_dlopen_self=yes, lt_cv_dlopen_self=yes,
	    lt_cv_dlopen_self=no, lt_cv_dlopen_self=cross)
    ])

    if test "x$lt_cv_dlopen_self" = xyes; then
      wl=$lt_prog_compiler_wl eval LDFLAGS=\"\$LDFLAGS $lt_prog_compiler_static\"
      AC_CACHE_CHECK([whether a statically linked program can dlopen itself],
    	  lt_cv_dlopen_self_static, [dnl
	  _LT_AC_TRY_DLOPEN_SELF(
	    lt_cv_dlopen_self_static=yes, lt_cv_dlopen_self_static=yes,
	    lt_cv_dlopen_self_static=no,  lt_cv_dlopen_self_static=cross)
      ])
    fi

    CPPFLAGS="$save_CPPFLAGS"
    LDFLAGS="$save_LDFLAGS"
    LIBS="$save_LIBS"
    ;;
  esac

  case $lt_cv_dlopen_self in
  yes|no) enable_dlopen_self=$lt_cv_dlopen_self ;;
  *) enable_dlopen_self=unknown ;;
  esac

  case $lt_cv_dlopen_self_static in
  yes|no) enable_dlopen_self_static=$lt_cv_dlopen_self_static ;;
  *) enable_dlopen_self_static=unknown ;;
  esac
fi
])# AC_LIBTOOL_DLOPEN_SELF


# AC_LIBTOOL_PROG_CC_C_O([TAGNAME])
# ---------------------------------
# Check to see if options -c and -o are simultaneously supported by compiler
AC_DEFUN([AC_LIBTOOL_PROG_CC_C_O],
[AC_REQUIRE([LT_AC_PROG_SED])dnl
AC_REQUIRE([_LT_AC_SYS_COMPILER])dnl
AC_CACHE_CHECK([if $compiler supports -c -o file.$ac_objext],
  [_LT_AC_TAGVAR(lt_cv_prog_compiler_c_o, $1)],
  [_LT_AC_TAGVAR(lt_cv_prog_compiler_c_o, $1)=no
   $rm -r conftest 2>/dev/null
   mkdir conftest
   cd conftest
   mkdir out
   echo "$lt_simple_compile_test_code" > conftest.$ac_ext

   lt_compiler_flag="-o out/conftest2.$ac_objext"
   # Insert the option either (1) after the last *FLAGS variable, or
   # (2) before a word containing "conftest.", or (3) at the end.
   # Note that $ac_compile itself does not contain backslashes and begins
   # with a dollar sign (not a hyphen), so the echo should work correctly.
   lt_compile=`echo "$ac_compile" | $SED \
   -e 's:.*FLAGS}\{0,1\} :&$lt_compiler_flag :; t' \
   -e 's: [[^ ]]*conftest\.: $lt_compiler_flag&:; t' \
   -e 's:$: $lt_compiler_flag:'`
   (eval echo "\"\$as_me:__oline__: $lt_compile\"" >&AS_MESSAGE_LOG_FD)
   (eval "$lt_compile" 2>out/conftest.err)
   ac_status=$?
   cat out/conftest.err >&AS_MESSAGE_LOG_FD
   echo "$as_me:__oline__: \$? = $ac_status" >&AS_MESSAGE_LOG_FD
   if (exit $ac_status) && test -s out/conftest2.$ac_objext
   then
     # The compiler can only warn and ignore the option if not recognized
     # So say no if there are warnings
     $echo "X$_lt_compiler_boilerplate" | $Xsed -e '/^$/d' > out/conftest.exp
     $SED '/^$/d; /^ *+/d' out/conftest.err >out/conftest.er2
     if test ! -s out/conftest.er2 || diff out/conftest.exp out/conftest.er2 >/dev/null; then
       _LT_AC_TAGVAR(lt_cv_prog_compiler_c_o, $1)=yes
     fi
   fi
   chmod u+w . 2>&AS_MESSAGE_LOG_FD
   $rm conftest*
   # SGI C++ compiler will create directory out/ii_files/ for
   # template instantiation
   test -d out/ii_files && $rm out/ii_files/* && rmdir out/ii_files
   $rm out/* && rmdir out
   cd ..
   rmdir conftest
   $rm conftest*
])
])# AC_LIBTOOL_PROG_CC_C_O


# AC_LIBTOOL_SYS_HARD_LINK_LOCKS([TAGNAME])
# -----------------------------------------
# Check to see if we can do hard links to lock some files if needed
AC_DEFUN([AC_LIBTOOL_SYS_HARD_LINK_LOCKS],
[AC_REQUIRE([_LT_AC_LOCK])dnl

hard_links="nottested"
if test "$_LT_AC_TAGVAR(lt_cv_prog_compiler_c_o, $1)" = no && test "$need_locks" != no; then
  # do not overwrite the value of need_locks provided by the user
  AC_MSG_CHECKING([if we can lock with hard links])
  hard_links=yes
  $rm conftest*
  ln conftest.a conftest.b 2>/dev/null && hard_links=no
  touch conftest.a
  ln conftest.a conftest.b 2>&5 || hard_links=no
  ln conftest.a conftest.b 2>/dev/null && hard_links=no
  AC_MSG_RESULT([$hard_links])
  if test "$hard_links" = no; then
    AC_MSG_WARN([`$CC' does not support `-c -o', so `make -j' may be unsafe])
    need_locks=warn
  fi
else
  need_locks=no
fi
])# AC_LIBTOOL_SYS_HARD_LINK_LOCKS


# AC_LIBTOOL_OBJDIR
# -----------------
AC_DEFUN([AC_LIBTOOL_OBJDIR],
[AC_CACHE_CHECK([for objdir], [lt_cv_objdir],
[rm -f .libs 2>/dev/null
mkdir .libs 2>/dev/null
if test -d .libs; then
  lt_cv_objdir=.libs
else
  # MS-DOS does not allow filenames that begin with a dot.
  lt_cv_objdir=_libs
fi
rmdir .libs 2>/dev/null])
objdir=$lt_cv_objdir
])# AC_LIBTOOL_OBJDIR


# AC_LIBTOOL_PROG_LD_HARDCODE_LIBPATH([TAGNAME])
# ----------------------------------------------
# Check hardcoding attributes.
AC_DEFUN([AC_LIBTOOL_PROG_LD_HARDCODE_LIBPATH],
[AC_MSG_CHECKING([how to hardcode library paths into programs])
_LT_AC_TAGVAR(hardcode_action, $1)=
if test -n "$_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)" || \
   test -n "$_LT_AC_TAGVAR(runpath_var, $1)" || \
   test "X$_LT_AC_TAGVAR(hardcode_automatic, $1)" = "Xyes" ; then

  # We can hardcode non-existant directories.
  if test "$_LT_AC_TAGVAR(hardcode_direct, $1)" != no &&
     # If the only mechanism to avoid hardcoding is shlibpath_var, we
     # have to relink, otherwise we might link with an installed library
     # when we should be linking with a yet-to-be-installed one
     ## test "$_LT_AC_TAGVAR(hardcode_shlibpath_var, $1)" != no &&
     test "$_LT_AC_TAGVAR(hardcode_minus_L, $1)" != no; then
    # Linking always hardcodes the temporary library directory.
    _LT_AC_TAGVAR(hardcode_action, $1)=relink
  else
    # We can link without hardcoding, and we can hardcode nonexisting dirs.
    _LT_AC_TAGVAR(hardcode_action, $1)=immediate
  fi
else
  # We cannot hardcode anything, or else we can only hardcode existing
  # directories.
  _LT_AC_TAGVAR(hardcode_action, $1)=unsupported
fi
AC_MSG_RESULT([$_LT_AC_TAGVAR(hardcode_action, $1)])

if test "$_LT_AC_TAGVAR(hardcode_action, $1)" = relink; then
  # Fast installation is not supported
  enable_fast_install=no
elif test "$shlibpath_overrides_runpath" = yes ||
     test "$enable_shared" = no; then
  # Fast installation is not necessary
  enable_fast_install=needless
fi
])# AC_LIBTOOL_PROG_LD_HARDCODE_LIBPATH


# AC_LIBTOOL_SYS_LIB_STRIP
# ------------------------
AC_DEFUN([AC_LIBTOOL_SYS_LIB_STRIP],
[striplib=
old_striplib=
AC_MSG_CHECKING([whether stripping libraries is possible])
if test -n "$STRIP" && $STRIP -V 2>&1 | grep "GNU strip" >/dev/null; then
  test -z "$old_striplib" && old_striplib="$STRIP --strip-debug"
  test -z "$striplib" && striplib="$STRIP --strip-unneeded"
  AC_MSG_RESULT([yes])
else
# FIXME - insert some real tests, host_os isn't really good enough
  case $host_os in
   darwin*)
       if test -n "$STRIP" ; then
         striplib="$STRIP -x"
         old_striplib="$STRIP -S"
         AC_MSG_RESULT([yes])
       else
  AC_MSG_RESULT([no])
fi
       ;;
   *)
  AC_MSG_RESULT([no])
    ;;
  esac
fi
])# AC_LIBTOOL_SYS_LIB_STRIP


# AC_LIBTOOL_SYS_DYNAMIC_LINKER
# -----------------------------
# PORTME Fill in your ld.so characteristics
AC_DEFUN([AC_LIBTOOL_SYS_DYNAMIC_LINKER],
[AC_REQUIRE([LT_AC_PROG_SED])dnl
AC_MSG_CHECKING([dynamic linker characteristics])
library_names_spec=
libname_spec='lib$name'
soname_spec=
shrext_cmds=".so"
postinstall_cmds=
postuninstall_cmds=
finish_cmds=
finish_eval=
shlibpath_var=
shlibpath_overrides_runpath=unknown
version_type=none
dynamic_linker="$host_os ld.so"
sys_lib_dlsearch_path_spec="/lib /usr/lib"
m4_if($1,[],[
if test "$GCC" = yes; then
  case $host_os in
    darwin*) lt_awk_arg="/^libraries:/,/LR/" ;;
    *) lt_awk_arg="/^libraries:/" ;;
  esac
  lt_search_path_spec=`$CC -print-search-dirs | awk $lt_awk_arg | $SED -e "s/^libraries://" -e "s,=/,/,g"`
  if echo "$lt_search_path_spec" | grep ';' >/dev/null ; then
    # if the path contains ";" then we assume it to be the separator
    # otherwise default to the standard path separator (i.e. ":") - it is
    # assumed that no part of a normal pathname contains ";" but that should
    # okay in the real world where ";" in dirpaths is itself problematic.
    lt_search_path_spec=`echo "$lt_search_path_spec" | $SED -e 's/;/ /g'`
  else
    lt_search_path_spec=`echo "$lt_search_path_spec" | $SED  -e "s/$PATH_SEPARATOR/ /g"`
  fi
  # Ok, now we have the path, separated by spaces, we can step through it
  # and add multilib dir if necessary.
  lt_tmp_lt_search_path_spec=
  lt_multi_os_dir=`$CC $CPPFLAGS $CFLAGS $LDFLAGS -print-multi-os-directory 2>/dev/null`
  for lt_sys_path in $lt_search_path_spec; do
    if test -d "$lt_sys_path/$lt_multi_os_dir"; then
      lt_tmp_lt_search_path_spec="$lt_tmp_lt_search_path_spec $lt_sys_path/$lt_multi_os_dir"
    else
      test -d "$lt_sys_path" && \
	lt_tmp_lt_search_path_spec="$lt_tmp_lt_search_path_spec $lt_sys_path"
    fi
  done
  lt_search_path_spec=`echo $lt_tmp_lt_search_path_spec | awk '
BEGIN {RS=" "; FS="/|\n";} {
  lt_foo="";
  lt_count=0;
  for (lt_i = NF; lt_i > 0; lt_i--) {
    if ($lt_i != "" && $lt_i != ".") {
      if ($lt_i == "..") {
        lt_count++;
      } else {
        if (lt_count == 0) {
          lt_foo="/" $lt_i lt_foo;
        } else {
          lt_count--;
        }
      }
    }
  }
  if (lt_foo != "") { lt_freq[[lt_foo]]++; }
  if (lt_freq[[lt_foo]] == 1) { print lt_foo; }
}'`
  sys_lib_search_path_spec=`echo $lt_search_path_spec`
else
  sys_lib_search_path_spec="/lib /usr/lib /usr/local/lib"
fi])
need_lib_prefix=unknown
hardcode_into_libs=no

# when you set need_version to no, make sure it does not cause -set_version
# flags to be left without arguments
need_version=unknown

case $host_os in
aix3*)
  version_type=linux
  library_names_spec='${libname}${release}${shared_ext}$versuffix $libname.a'
  shlibpath_var=LIBPATH

  # AIX 3 has no versioning support, so we append a major version to the name.
  soname_spec='${libname}${release}${shared_ext}$major'
  ;;

aix[[4-9]]*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  hardcode_into_libs=yes
  if test "$host_cpu" = ia64; then
    # AIX 5 supports IA64
    library_names_spec='${libname}${release}${shared_ext}$major ${libname}${release}${shared_ext}$versuffix $libname${shared_ext}'
    shlibpath_var=LD_LIBRARY_PATH
  else
    # With GCC up to 2.95.x, collect2 would create an import file
    # for dependence libraries.  The import file would start with
    # the line `#! .'.  This would cause the generated library to
    # depend on `.', always an invalid library.  This was fixed in
    # development snapshots of GCC prior to 3.0.
    case $host_os in
      aix4 | aix4.[[01]] | aix4.[[01]].*)
      if { echo '#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 97)'
	   echo ' yes '
	   echo '#endif'; } | ${CC} -E - | grep yes > /dev/null; then
	:
      else
	can_build_shared=no
      fi
      ;;
    esac
    # AIX (on Power*) has no versioning support, so currently we can not hardcode correct
    # soname into executable. Probably we can add versioning support to
    # collect2, so additional links can be useful in future.
    if test "$aix_use_runtimelinking" = yes; then
      # If using run time linking (on AIX 4.2 or later) use lib<name>.so
      # instead of lib<name>.a to let people know that these are not
      # typical AIX shared libraries.
      library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
    else
      # We preserve .a as extension for shared libraries through AIX4.2
      # and later when we are not doing run time linking.
      library_names_spec='${libname}${release}.a $libname.a'
      soname_spec='${libname}${release}${shared_ext}$major'
    fi
    shlibpath_var=LIBPATH
  fi
  ;;

amigaos*)
  library_names_spec='$libname.ixlibrary $libname.a'
  # Create ${libname}_ixlibrary.a entries in /sys/libs.
  finish_eval='for lib in `ls $libdir/*.ixlibrary 2>/dev/null`; do libname=`$echo "X$lib" | $Xsed -e '\''s%^.*/\([[^/]]*\)\.ixlibrary$%\1%'\''`; test $rm /sys/libs/${libname}_ixlibrary.a; $show "cd /sys/libs && $LN_S $lib ${libname}_ixlibrary.a"; cd /sys/libs && $LN_S $lib ${libname}_ixlibrary.a || exit 1; done'
  ;;

beos*)
  library_names_spec='${libname}${shared_ext}'
  dynamic_linker="$host_os ld.so"
  shlibpath_var=LIBRARY_PATH
  ;;

bsdi[[45]]*)
  version_type=linux
  need_version=no
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
  soname_spec='${libname}${release}${shared_ext}$major'
  finish_cmds='PATH="\$PATH:/sbin" ldconfig $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  sys_lib_search_path_spec="/shlib /usr/lib /usr/X11/lib /usr/contrib/lib /lib /usr/local/lib"
  sys_lib_dlsearch_path_spec="/shlib /usr/lib /usr/local/lib"
  # the default ld.so.conf also contains /usr/contrib/lib and
  # /usr/X11R6/lib (/usr/X11 is a link to /usr/X11R6), but let us allow
  # libtool to hard-code these into programs
  ;;

cygwin* | mingw* | pw32*)
  version_type=windows
  shrext_cmds=".dll"
  need_version=no
  need_lib_prefix=no

  case $GCC,$host_os in
  yes,cygwin* | yes,mingw* | yes,pw32*)
    library_names_spec='$libname.dll.a'
    # DLL is installed to $(libdir)/../bin by postinstall_cmds
    postinstall_cmds='base_file=`basename \${file}`~
      dlpath=`$SHELL 2>&1 -c '\''. $dir/'\''\${base_file}'\''i;echo \$dlname'\''`~
      dldir=$destdir/`dirname \$dlpath`~
      test -d \$dldir || mkdir -p \$dldir~
      $install_prog $dir/$dlname \$dldir/$dlname~
      chmod a+x \$dldir/$dlname'
    postuninstall_cmds='dldll=`$SHELL 2>&1 -c '\''. $file; echo \$dlname'\''`~
      dlpath=$dir/\$dldll~
       $rm \$dlpath'
    shlibpath_overrides_runpath=yes

    case $host_os in
    cygwin*)
      # Cygwin DLLs use 'cyg' prefix rather than 'lib'
      soname_spec='`echo ${libname} | sed -e 's/^lib/cyg/'``echo ${release} | $SED -e 's/[[.]]/-/g'`${versuffix}${shared_ext}'
      sys_lib_search_path_spec="/usr/lib /lib/w32api /lib /usr/local/lib"
      ;;
    mingw*)
      # MinGW DLLs use traditional 'lib' prefix
      soname_spec='${libname}`echo ${release} | $SED -e 's/[[.]]/-/g'`${versuffix}${shared_ext}'
      sys_lib_search_path_spec=`$CC -print-search-dirs | grep "^libraries:" | $SED -e "s/^libraries://" -e "s,=/,/,g"`
      if echo "$sys_lib_search_path_spec" | [grep ';[c-zC-Z]:/' >/dev/null]; then
        # It is most probably a Windows format PATH printed by
        # mingw gcc, but we are running on Cygwin. Gcc prints its search
        # path with ; separators, and with drive letters. We can handle the
        # drive letters (cygwin fileutils understands them), so leave them,
        # especially as we might pass files found there to a mingw objdump,
        # which wouldn't understand a cygwinified path. Ahh.
        sys_lib_search_path_spec=`echo "$sys_lib_search_path_spec" | $SED -e 's/;/ /g'`
      else
        sys_lib_search_path_spec=`echo "$sys_lib_search_path_spec" | $SED  -e "s/$PATH_SEPARATOR/ /g"`
      fi
      ;;
    pw32*)
      # pw32 DLLs use 'pw' prefix rather than 'lib'
      library_names_spec='`echo ${libname} | sed -e 's/^lib/pw/'``echo ${release} | $SED -e 's/[[.]]/-/g'`${versuffix}${shared_ext}'
      ;;
    esac
    ;;

  *)
    library_names_spec='${libname}`echo ${release} | $SED -e 's/[[.]]/-/g'`${versuffix}${shared_ext} $libname.lib'
    ;;
  esac
  dynamic_linker='Win32 ld.exe'
  # FIXME: first we should search . and the directory the executable is in
  shlibpath_var=PATH
  ;;

darwin* | rhapsody*)
  dynamic_linker="$host_os dyld"
  version_type=darwin
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}${versuffix}$shared_ext ${libname}${release}${major}$shared_ext ${libname}$shared_ext'
  soname_spec='${libname}${release}${major}$shared_ext'
  shlibpath_overrides_runpath=yes
  shlibpath_var=DYLD_LIBRARY_PATH
  shrext_cmds='`test .$module = .yes && echo .so || echo .dylib`'
  m4_if([$1], [],[
  sys_lib_search_path_spec="$sys_lib_search_path_spec /usr/local/lib"]) 
  sys_lib_dlsearch_path_spec='/usr/local/lib /lib /usr/lib'
  ;;

dgux*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname$shared_ext'
  soname_spec='${libname}${release}${shared_ext}$major'
  shlibpath_var=LD_LIBRARY_PATH
  ;;

freebsd1*)
  dynamic_linker=no
  ;;

freebsd* | dragonfly*)
  # DragonFly does not have aout.  When/if they implement a new
  # versioning mechanism, adjust this.
  if test -x /usr/bin/objformat; then
    objformat=`/usr/bin/objformat`
  else
    case $host_os in
    freebsd[[123]]*) objformat=aout ;;
    *) objformat=elf ;;
    esac
  fi
  version_type=freebsd-$objformat
  case $version_type in
    freebsd-elf*)
      library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext} $libname${shared_ext}'
      need_version=no
      need_lib_prefix=no
      ;;
    freebsd-*)
      library_names_spec='${libname}${release}${shared_ext}$versuffix $libname${shared_ext}$versuffix'
      need_version=yes
      ;;
  esac
  shlibpath_var=LD_LIBRARY_PATH
  case $host_os in
  freebsd2*)
    shlibpath_overrides_runpath=yes
    ;;
  freebsd3.[[01]]* | freebsdelf3.[[01]]*)
    shlibpath_overrides_runpath=yes
    hardcode_into_libs=yes
    ;;
  freebsd3.[[2-9]]* | freebsdelf3.[[2-9]]* | \
  freebsd4.[[0-5]] | freebsdelf4.[[0-5]] | freebsd4.1.1 | freebsdelf4.1.1)
    shlibpath_overrides_runpath=no
    hardcode_into_libs=yes
    ;;
  *) # from 4.6 on, and DragonFly
    shlibpath_overrides_runpath=yes
    hardcode_into_libs=yes
    ;;
  esac
  ;;

gnu*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}${major} ${libname}${shared_ext}'
  soname_spec='${libname}${release}${shared_ext}$major'
  shlibpath_var=LD_LIBRARY_PATH
  hardcode_into_libs=yes
  ;;

hpux9* | hpux10* | hpux11*)
  # Give a soname corresponding to the major version so that dld.sl refuses to
  # link against other versions.
  version_type=sunos
  need_lib_prefix=no
  need_version=no
  case $host_cpu in
  ia64*)
    shrext_cmds='.so'
    hardcode_into_libs=yes
    dynamic_linker="$host_os dld.so"
    shlibpath_var=LD_LIBRARY_PATH
    shlibpath_overrides_runpath=yes # Unless +noenvvar is specified.
    library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
    soname_spec='${libname}${release}${shared_ext}$major'
    if test "X$HPUX_IA64_MODE" = X32; then
      sys_lib_search_path_spec="/usr/lib/hpux32 /usr/local/lib/hpux32 /usr/local/lib"
    else
      sys_lib_search_path_spec="/usr/lib/hpux64 /usr/local/lib/hpux64"
    fi
    sys_lib_dlsearch_path_spec=$sys_lib_search_path_spec
    ;;
   hppa*64*)
     shrext_cmds='.sl'
     hardcode_into_libs=yes
     dynamic_linker="$host_os dld.sl"
     shlibpath_var=LD_LIBRARY_PATH # How should we handle SHLIB_PATH
     shlibpath_overrides_runpath=yes # Unless +noenvvar is specified.
     library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
     soname_spec='${libname}${release}${shared_ext}$major'
     sys_lib_search_path_spec="/usr/lib/pa20_64 /usr/ccs/lib/pa20_64"
     sys_lib_dlsearch_path_spec=$sys_lib_search_path_spec
     ;;
   *)
    shrext_cmds='.sl'
    dynamic_linker="$host_os dld.sl"
    shlibpath_var=SHLIB_PATH
    shlibpath_overrides_runpath=no # +s is required to enable SHLIB_PATH
    library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
    soname_spec='${libname}${release}${shared_ext}$major'
    ;;
  esac
  # HP-UX runs *really* slowly unless shared libraries are mode 555.
  postinstall_cmds='chmod 555 $lib'
  ;;

interix[[3-9]]*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major ${libname}${shared_ext}'
  soname_spec='${libname}${release}${shared_ext}$major'
  dynamic_linker='Interix 3.x ld.so.1 (PE, like ELF)'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=no
  hardcode_into_libs=yes
  ;;

irix5* | irix6* | nonstopux*)
  case $host_os in
    nonstopux*) version_type=nonstopux ;;
    *)
	if test "$lt_cv_prog_gnu_ld" = yes; then
		version_type=linux
	else
		version_type=irix
	fi ;;
  esac
  need_lib_prefix=no
  need_version=no
  soname_spec='${libname}${release}${shared_ext}$major'
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major ${libname}${release}${shared_ext} $libname${shared_ext}'
  case $host_os in
  irix5* | nonstopux*)
    libsuff= shlibsuff=
    ;;
  *)
    case $LD in # libtool.m4 will add one of these switches to LD
    *-32|*"-32 "|*-melf32bsmip|*"-melf32bsmip ")
      libsuff= shlibsuff= libmagic=32-bit;;
    *-n32|*"-n32 "|*-melf32bmipn32|*"-melf32bmipn32 ")
      libsuff=32 shlibsuff=N32 libmagic=N32;;
    *-64|*"-64 "|*-melf64bmip|*"-melf64bmip ")
      libsuff=64 shlibsuff=64 libmagic=64-bit;;
    *) libsuff= shlibsuff= libmagic=never-match;;
    esac
    ;;
  esac
  shlibpath_var=LD_LIBRARY${shlibsuff}_PATH
  shlibpath_overrides_runpath=no
  sys_lib_search_path_spec="/usr/lib${libsuff} /lib${libsuff} /usr/local/lib${libsuff}"
  sys_lib_dlsearch_path_spec="/usr/lib${libsuff} /lib${libsuff}"
  hardcode_into_libs=yes
  ;;

# No shared lib support for Linux oldld, aout, or coff.
linux*oldld* | linux*aout* | linux*coff*)
  dynamic_linker=no
  ;;

# This must be Linux ELF.
linux* | k*bsd*-gnu)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
  soname_spec='${libname}${release}${shared_ext}$major'
  finish_cmds='PATH="\$PATH:/sbin" ldconfig -n $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=no
  # This implies no fast_install, which is unacceptable.
  # Some rework will be needed to allow for fast_install
  # before this can be enabled.
  hardcode_into_libs=yes

  # Append ld.so.conf contents to the search path
  if test -f /etc/ld.so.conf; then
    lt_ld_extra=`awk '/^include / { system(sprintf("cd /etc; cat %s 2>/dev/null", \[$]2)); skip = 1; } { if (!skip) print \[$]0; skip = 0; }' < /etc/ld.so.conf | $SED -e 's/#.*//;/^[ 	]*hwcap[ 	]/d;s/[:,	]/ /g;s/=[^=]*$//;s/=[^= ]* / /g;/^$/d' | tr '\n' ' '`
    sys_lib_dlsearch_path_spec="/lib /usr/lib $lt_ld_extra"
  fi

  # We used to test for /lib/ld.so.1 and disable shared libraries on
  # powerpc, because MkLinux only supported shared libraries with the
  # GNU dynamic linker.  Since this was broken with cross compilers,
  # most powerpc-linux boxes support dynamic linking these days and
  # people can always --disable-shared, the test was removed, and we
  # assume the GNU/Linux dynamic linker is in use.
  dynamic_linker='GNU/Linux ld.so'
  ;;

netbsd*)
  version_type=sunos
  need_lib_prefix=no
  need_version=no
  if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
    library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${shared_ext}$versuffix'
    finish_cmds='PATH="\$PATH:/sbin" ldconfig -m $libdir'
    dynamic_linker='NetBSD (a.out) ld.so'
  else
    library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major ${libname}${shared_ext}'
    soname_spec='${libname}${release}${shared_ext}$major'
    dynamic_linker='NetBSD ld.elf_so'
  fi
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  hardcode_into_libs=yes
  ;;

newsos6)
  version_type=linux
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  ;;

nto-qnx*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
  soname_spec='${libname}${release}${shared_ext}$major'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  ;;

openbsd*)
  version_type=sunos
  sys_lib_dlsearch_path_spec="/usr/lib"
  need_lib_prefix=no
  # Some older versions of OpenBSD (3.3 at least) *do* need versioned libs.
  case $host_os in
    openbsd3.3 | openbsd3.3.*) need_version=yes ;;
    *)                         need_version=no  ;;
  esac
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${shared_ext}$versuffix'
  finish_cmds='PATH="\$PATH:/sbin" ldconfig -m $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
    case $host_os in
      openbsd2.[[89]] | openbsd2.[[89]].*)
	shlibpath_overrides_runpath=no
	;;
      *)
	shlibpath_overrides_runpath=yes
	;;
      esac
  else
    shlibpath_overrides_runpath=yes
  fi
  ;;

os2*)
  libname_spec='$name'
  shrext_cmds=".dll"
  need_lib_prefix=no
  library_names_spec='$libname${shared_ext} $libname.a'
  dynamic_linker='OS/2 ld.exe'
  shlibpath_var=LIBPATH
  ;;

osf3* | osf4* | osf5*)
  version_type=osf
  need_lib_prefix=no
  need_version=no
  soname_spec='${libname}${release}${shared_ext}$major'
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
  shlibpath_var=LD_LIBRARY_PATH
  sys_lib_search_path_spec="/usr/shlib /usr/ccs/lib /usr/lib/cmplrs/cc /usr/lib /usr/local/lib /var/shlib"
  sys_lib_dlsearch_path_spec="$sys_lib_search_path_spec"
  ;;

rdos*)
  dynamic_linker=no
  ;;

solaris*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
  soname_spec='${libname}${release}${shared_ext}$major'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  hardcode_into_libs=yes
  # ldd complains unless libraries are executable
  postinstall_cmds='chmod +x $lib'
  ;;

sunos4*)
  version_type=sunos
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${shared_ext}$versuffix'
  finish_cmds='PATH="\$PATH:/usr/etc" ldconfig $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  if test "$with_gnu_ld" = yes; then
    need_lib_prefix=no
  fi
  need_version=yes
  ;;

sysv4 | sysv4.3*)
  version_type=linux
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
  soname_spec='${libname}${release}${shared_ext}$major'
  shlibpath_var=LD_LIBRARY_PATH
  case $host_vendor in
    sni)
      shlibpath_overrides_runpath=no
      need_lib_prefix=no
      export_dynamic_flag_spec='${wl}-Blargedynsym'
      runpath_var=LD_RUN_PATH
      ;;
    siemens)
      need_lib_prefix=no
      ;;
    motorola)
      need_lib_prefix=no
      need_version=no
      shlibpath_overrides_runpath=no
      sys_lib_search_path_spec='/lib /usr/lib /usr/ccs/lib'
      ;;
  esac
  ;;

sysv4*MP*)
  if test -d /usr/nec ;then
    version_type=linux
    library_names_spec='$libname${shared_ext}.$versuffix $libname${shared_ext}.$major $libname${shared_ext}'
    soname_spec='$libname${shared_ext}.$major'
    shlibpath_var=LD_LIBRARY_PATH
  fi
  ;;

sysv5* | sco3.2v5* | sco5v6* | unixware* | OpenUNIX* | sysv4*uw2*)
  version_type=freebsd-elf
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext} $libname${shared_ext}'
  soname_spec='${libname}${release}${shared_ext}$major'
  shlibpath_var=LD_LIBRARY_PATH
  hardcode_into_libs=yes
  if test "$with_gnu_ld" = yes; then
    sys_lib_search_path_spec='/usr/local/lib /usr/gnu/lib /usr/ccs/lib /usr/lib /lib'
    shlibpath_overrides_runpath=no
  else
    sys_lib_search_path_spec='/usr/ccs/lib /usr/lib'
    shlibpath_overrides_runpath=yes
    case $host_os in
      sco3.2v5*)
        sys_lib_search_path_spec="$sys_lib_search_path_spec /lib"
	;;
    esac
  fi
  sys_lib_dlsearch_path_spec='/usr/lib'
  ;;

uts4*)
  version_type=linux
  library_names_spec='${libname}${release}${shared_ext}$versuffix ${libname}${release}${shared_ext}$major $libname${shared_ext}'
  soname_spec='${libname}${release}${shared_ext}$major'
  shlibpath_var=LD_LIBRARY_PATH
  ;;

*)
  dynamic_linker=no
  ;;
esac
AC_MSG_RESULT([$dynamic_linker])
test "$dynamic_linker" = no && can_build_shared=no

AC_CACHE_VAL([lt_cv_sys_lib_search_path_spec],
[lt_cv_sys_lib_search_path_spec="$sys_lib_search_path_spec"])
sys_lib_search_path_spec="$lt_cv_sys_lib_search_path_spec"
AC_CACHE_VAL([lt_cv_sys_lib_dlsearch_path_spec],
[lt_cv_sys_lib_dlsearch_path_spec="$sys_lib_dlsearch_path_spec"])
sys_lib_dlsearch_path_spec="$lt_cv_sys_lib_dlsearch_path_spec"

variables_saved_for_relink="PATH $shlibpath_var $runpath_var"
if test "$GCC" = yes; then
  variables_saved_for_relink="$variables_saved_for_relink GCC_EXEC_PREFIX COMPILER_PATH LIBRARY_PATH"
fi
])# AC_LIBTOOL_SYS_DYNAMIC_LINKER


# _LT_AC_TAGCONFIG
# ----------------
AC_DEFUN([_LT_AC_TAGCONFIG],
[AC_REQUIRE([LT_AC_PROG_SED])dnl
AC_ARG_WITH([tags],
    [AC_HELP_STRING([--with-tags@<:@=TAGS@:>@],
        [include additional configurations @<:@automatic@:>@])],
    [tagnames="$withval"])

if test -f "$ltmain" && test -n "$tagnames"; then
  if test ! -f "${ofile}"; then
    AC_MSG_WARN([output file `$ofile' does not exist])
  fi

  if test -z "$LTCC"; then
    eval "`$SHELL ${ofile} --config | grep '^LTCC='`"
    if test -z "$LTCC"; then
      AC_MSG_WARN([output file `$ofile' does not look like a libtool script])
    else
      AC_MSG_WARN([using `LTCC=$LTCC', extracted from `$ofile'])
    fi
  fi
  if test -z "$LTCFLAGS"; then
    eval "`$SHELL ${ofile} --config | grep '^LTCFLAGS='`"
  fi

  # Extract list of available tagged configurations in $ofile.
  # Note that this assumes the entire list is on one line.
  available_tags=`grep "^available_tags=" "${ofile}" | $SED -e 's/available_tags=\(.*$\)/\1/' -e 's/\"//g'`

  lt_save_ifs="$IFS"; IFS="${IFS}$PATH_SEPARATOR,"
  for tagname in $tagnames; do
    IFS="$lt_save_ifs"
    # Check whether tagname contains only valid characters
    case `$echo "X$tagname" | $Xsed -e 's:[[-_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890,/]]::g'` in
    "") ;;
    *)  AC_MSG_ERROR([invalid tag name: $tagname])
	;;
    esac

    if grep "^# ### BEGIN LIBTOOL TAG CONFIG: $tagname$" < "${ofile}" > /dev/null
    then
      AC_MSG_ERROR([tag name \"$tagname\" already exists])
    fi

    # Update the list of available tags.
    if test -n "$tagname"; then
      echo appending configuration tag \"$tagname\" to $ofile

      case $tagname in
      CXX)
	if test -n "$CXX" && ( test "X$CXX" != "Xno" &&
	    ( (test "X$CXX" = "Xg++" && `g++ -v >/dev/null 2>&1` ) ||
	    (test "X$CXX" != "Xg++"))) ; then
	  AC_LIBTOOL_LANG_CXX_CONFIG
	else
	  tagname=""
	fi
	;;

      F77)
	if test -n "$F77" && test "X$F77" != "Xno"; then
	  AC_LIBTOOL_LANG_F77_CONFIG
	else
	  tagname=""
	fi
	;;

      GCJ)
	if test -n "$GCJ" && test "X$GCJ" != "Xno"; then
	  AC_LIBTOOL_LANG_GCJ_CONFIG
	else
	  tagname=""
	fi
	;;

      RC)
	AC_LIBTOOL_LANG_RC_CONFIG
	;;

      *)
	AC_MSG_ERROR([Unsupported tag name: $tagname])
	;;
      esac

      # Append the new tag name to the list of available tags.
      if test -n "$tagname" ; then
      available_tags="$available_tags $tagname"
    fi
    fi
  done
  IFS="$lt_save_ifs"

  # Now substitute the updated list of available tags.
  if eval "sed -e 's/^available_tags=.*\$/available_tags=\"$available_tags\"/' \"$ofile\" > \"${ofile}T\""; then
    mv "${ofile}T" "$ofile"
    chmod +x "$ofile"
  else
    rm -f "${ofile}T"
    AC_MSG_ERROR([unable to update list of available tagged configurations.])
  fi
fi
])# _LT_AC_TAGCONFIG


# AC_LIBTOOL_DLOPEN
# -----------------
# enable checks for dlopen support
AC_DEFUN([AC_LIBTOOL_DLOPEN],
 [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])
])# AC_LIBTOOL_DLOPEN


# AC_LIBTOOL_WIN32_DLL
# --------------------
# declare package support for building win32 DLLs
AC_DEFUN([AC_LIBTOOL_WIN32_DLL],
[AC_BEFORE([$0], [AC_LIBTOOL_SETUP])
])# AC_LIBTOOL_WIN32_DLL


# AC_ENABLE_SHARED([DEFAULT])
# ---------------------------
# implement the --enable-shared flag
# DEFAULT is either `yes' or `no'.  If omitted, it defaults to `yes'.
AC_DEFUN([AC_ENABLE_SHARED],
[define([AC_ENABLE_SHARED_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE([shared],
    [AC_HELP_STRING([--enable-shared@<:@=PKGS@:>@],
	[build shared libraries @<:@default=]AC_ENABLE_SHARED_DEFAULT[@:>@])],
    [p=${PACKAGE-default}
    case $enableval in
    yes) enable_shared=yes ;;
    no) enable_shared=no ;;
    *)
      enable_shared=no
      # Look at the argument we got.  We use all the common list separators.
      lt_save_ifs="$IFS"; IFS="${IFS}$PATH_SEPARATOR,"
      for pkg in $enableval; do
	IFS="$lt_save_ifs"
	if test "X$pkg" = "X$p"; then
	  enable_shared=yes
	fi
      done
      IFS="$lt_save_ifs"
      ;;
    esac],
    [enable_shared=]AC_ENABLE_SHARED_DEFAULT)
])# AC_ENABLE_SHARED


# AC_DISABLE_SHARED
# -----------------
# set the default shared flag to --disable-shared
AC_DEFUN([AC_DISABLE_SHARED],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_SHARED(no)
])# AC_DISABLE_SHARED


# AC_ENABLE_STATIC([DEFAULT])
# ---------------------------
# implement the --enable-static flag
# DEFAULT is either `yes' or `no'.  If omitted, it defaults to `yes'.
AC_DEFUN([AC_ENABLE_STATIC],
[define([AC_ENABLE_STATIC_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE([static],
    [AC_HELP_STRING([--enable-static@<:@=PKGS@:>@],
	[build static libraries @<:@default=]AC_ENABLE_STATIC_DEFAULT[@:>@])],
    [p=${PACKAGE-default}
    case $enableval in
    yes) enable_static=yes ;;
    no) enable_static=no ;;
    *)
     enable_static=no
      # Look at the argument we got.  We use all the common list separators.
      lt_save_ifs="$IFS"; IFS="${IFS}$PATH_SEPARATOR,"
      for pkg in $enableval; do
	IFS="$lt_save_ifs"
	if test "X$pkg" = "X$p"; then
	  enable_static=yes
	fi
      done
      IFS="$lt_save_ifs"
      ;;
    esac],
    [enable_static=]AC_ENABLE_STATIC_DEFAULT)
])# AC_ENABLE_STATIC


# AC_DISABLE_STATIC
# -----------------
# set the default static flag to --disable-static
AC_DEFUN([AC_DISABLE_STATIC],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_STATIC(no)
])# AC_DISABLE_STATIC


# AC_ENABLE_FAST_INSTALL([DEFAULT])
# ---------------------------------
# implement the --enable-fast-install flag
# DEFAULT is either `yes' or `no'.  If omitted, it defaults to `yes'.
AC_DEFUN([AC_ENABLE_FAST_INSTALL],
[define([AC_ENABLE_FAST_INSTALL_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE([fast-install],
    [AC_HELP_STRING([--enable-fast-install@<:@=PKGS@:>@],
    [optimize for fast installation @<:@default=]AC_ENABLE_FAST_INSTALL_DEFAULT[@:>@])],
    [p=${PACKAGE-default}
    case $enableval in
    yes) enable_fast_install=yes ;;
    no) enable_fast_install=no ;;
    *)
      enable_fast_install=no
      # Look at the argument we got.  We use all the common list separators.
      lt_save_ifs="$IFS"; IFS="${IFS}$PATH_SEPARATOR,"
      for pkg in $enableval; do
	IFS="$lt_save_ifs"
	if test "X$pkg" = "X$p"; then
	  enable_fast_install=yes
	fi
      done
      IFS="$lt_save_ifs"
      ;;
    esac],
    [enable_fast_install=]AC_ENABLE_FAST_INSTALL_DEFAULT)
])# AC_ENABLE_FAST_INSTALL


# AC_DISABLE_FAST_INSTALL
# -----------------------
# set the default to --disable-fast-install
AC_DEFUN([AC_DISABLE_FAST_INSTALL],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_FAST_INSTALL(no)
])# AC_DISABLE_FAST_INSTALL


# AC_LIBTOOL_PICMODE([MODE])
# --------------------------
# implement the --with-pic flag
# MODE is either `yes' or `no'.  If omitted, it defaults to `both'.
AC_DEFUN([AC_LIBTOOL_PICMODE],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
pic_mode=ifelse($#,1,$1,default)
])# AC_LIBTOOL_PICMODE


# AC_PROG_EGREP
# -------------
# This is predefined starting with Autoconf 2.54, so this conditional
# definition can be removed once we require Autoconf 2.54 or later.
m4_ifndef([AC_PROG_EGREP], [AC_DEFUN([AC_PROG_EGREP],
[AC_CACHE_CHECK([for egrep], [ac_cv_prog_egrep],
   [if echo a | (grep -E '(a|b)') >/dev/null 2>&1
    then ac_cv_prog_egrep='grep -E'
    else ac_cv_prog_egrep='egrep'
    fi])
 EGREP=$ac_cv_prog_egrep
 AC_SUBST([EGREP])
])])


# AC_PATH_TOOL_PREFIX
# -------------------
# find a file program which can recognize shared library
AC_DEFUN([AC_PATH_TOOL_PREFIX],
[AC_REQUIRE([AC_PROG_EGREP])dnl
AC_MSG_CHECKING([for $1])
AC_CACHE_VAL(lt_cv_path_MAGIC_CMD,
[case $MAGIC_CMD in
[[\\/*] |  ?:[\\/]*])
  lt_cv_path_MAGIC_CMD="$MAGIC_CMD" # Let the user override the test with a path.
  ;;
*)
  lt_save_MAGIC_CMD="$MAGIC_CMD"
  lt_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
dnl $ac_dummy forces splitting on constant user-supplied paths.
dnl POSIX.2 word splitting is done only on the output of word expansions,
dnl not every word.  This closes a longstanding sh security hole.
  ac_dummy="ifelse([$2], , $PATH, [$2])"
  for ac_dir in $ac_dummy; do
    IFS="$lt_save_ifs"
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/$1; then
      lt_cv_path_MAGIC_CMD="$ac_dir/$1"
      if test -n "$file_magic_test_file"; then
	case $deplibs_check_method in
	"file_magic "*)
	  file_magic_regex=`expr "$deplibs_check_method" : "file_magic \(.*\)"`
	  MAGIC_CMD="$lt_cv_path_MAGIC_CMD"
	  if eval $file_magic_cmd \$file_magic_test_file 2> /dev/null |
	    $EGREP "$file_magic_regex" > /dev/null; then
	    :
	  else
	    cat <<EOF 1>&2

*** Warning: the command libtool uses to detect shared libraries,
*** $file_magic_cmd, produces output that libtool cannot recognize.
*** The result is that libtool may fail to recognize shared libraries
*** as such.  This will affect the creation of libtool libraries that
*** depend on shared libraries, but programs linked with such libtool
*** libraries will work regardless of this problem.  Nevertheless, you
*** may want to report the problem to your system manager and/or to
*** bug-libtool@gnu.org

EOF
	  fi ;;
	esac
      fi
      break
    fi
  done
  IFS="$lt_save_ifs"
  MAGIC_CMD="$lt_save_MAGIC_CMD"
  ;;
esac])
MAGIC_CMD="$lt_cv_path_MAGIC_CMD"
if test -n "$MAGIC_CMD"; then
  AC_MSG_RESULT($MAGIC_CMD)
else
  AC_MSG_RESULT(no)
fi
])# AC_PATH_TOOL_PREFIX


# AC_PATH_MAGIC
# -------------
# find a file program which can recognize a shared library
AC_DEFUN([AC_PATH_MAGIC],
[AC_PATH_TOOL_PREFIX(${ac_tool_prefix}file, /usr/bin$PATH_SEPARATOR$PATH)
if test -z "$lt_cv_path_MAGIC_CMD"; then
  if test -n "$ac_tool_prefix"; then
    AC_PATH_TOOL_PREFIX(file, /usr/bin$PATH_SEPARATOR$PATH)
  else
    MAGIC_CMD=:
  fi
fi
])# AC_PATH_MAGIC


# AC_PROG_LD
# ----------
# find the pathname to the GNU or non-GNU linker
AC_DEFUN([AC_PROG_LD],
[AC_ARG_WITH([gnu-ld],
    [AC_HELP_STRING([--with-gnu-ld],
	[assume the C compiler uses GNU ld @<:@default=no@:>@])],
    [test "$withval" = no || with_gnu_ld=yes],
    [with_gnu_ld=no])
AC_REQUIRE([LT_AC_PROG_SED])dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
ac_prog=ld
if test "$GCC" = yes; then
  # Check if gcc -print-prog-name=ld gives a path.
  AC_MSG_CHECKING([for ld used by $CC])
  case $host in
  *-*-mingw*)
    # gcc leaves a trailing carriage return which upsets mingw
    ac_prog=`($CC -print-prog-name=ld) 2>&5 | tr -d '\015'` ;;
  *)
    ac_prog=`($CC -print-prog-name=ld) 2>&5` ;;
  esac
  case $ac_prog in
    # Accept absolute paths.
    [[\\/]]* | ?:[[\\/]]*)
      re_direlt='/[[^/]][[^/]]*/\.\./'
      # Canonicalize the pathname of ld
      ac_prog=`echo $ac_prog| $SED 's%\\\\%/%g'`
      while echo $ac_prog | grep "$re_direlt" > /dev/null 2>&1; do
	ac_prog=`echo $ac_prog| $SED "s%$re_direlt%/%"`
      done
      test -z "$LD" && LD="$ac_prog"
      ;;
  "")
    # If it fails, then pretend we aren't using GCC.
    ac_prog=ld
    ;;
  *)
    # If it is relative, then search for the first ld in PATH.
    with_gnu_ld=unknown
    ;;
  esac
elif test "$with_gnu_ld" = yes; then
  AC_MSG_CHECKING([for GNU ld])
else
  AC_MSG_CHECKING([for non-GNU ld])
fi
AC_CACHE_VAL(lt_cv_path_LD,
[if test -z "$LD"; then
  lt_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
  for ac_dir in $PATH; do
    IFS="$lt_save_ifs"
    test -z "$ac_dir" && ac_dir=.
    if test -f "$ac_dir/$ac_prog" || test -f "$ac_dir/$ac_prog$ac_exeext"; then
      lt_cv_path_LD="$ac_dir/$ac_prog"
      # Check to see if the program is GNU ld.  I'd rather use --version,
      # but apparently some variants of GNU ld only accept -v.
      # Break only if it was the GNU/non-GNU ld that we prefer.
      case `"$lt_cv_path_LD" -v 2>&1 </dev/null` in
      *GNU* | *'with BFD'*)
	test "$with_gnu_ld" != no && break
	;;
      *)
	test "$with_gnu_ld" != yes && break
	;;
      esac
    fi
  done
  IFS="$lt_save_ifs"
else
  lt_cv_path_LD="$LD" # Let the user override the test with a path.
fi])
LD="$lt_cv_path_LD"
if test -n "$LD"; then
  AC_MSG_RESULT($LD)
else
  AC_MSG_RESULT(no)
fi
test -z "$LD" && AC_MSG_ERROR([no acceptable ld found in \$PATH])
AC_PROG_LD_GNU
])# AC_PROG_LD


# AC_PROG_LD_GNU
# --------------
AC_DEFUN([AC_PROG_LD_GNU],
[AC_REQUIRE([AC_PROG_EGREP])dnl
AC_CACHE_CHECK([if the linker ($LD) is GNU ld], lt_cv_prog_gnu_ld,
[# I'd rather use --version here, but apparently some GNU lds only accept -v.
case `$LD -v 2>&1 </dev/null` in
*GNU* | *'with BFD'*)
  lt_cv_prog_gnu_ld=yes
  ;;
*)
  lt_cv_prog_gnu_ld=no
  ;;
esac])
with_gnu_ld=$lt_cv_prog_gnu_ld
])# AC_PROG_LD_GNU


# AC_PROG_LD_RELOAD_FLAG
# ----------------------
# find reload flag for linker
#   -- PORTME Some linkers may need a different reload flag.
AC_DEFUN([AC_PROG_LD_RELOAD_FLAG],
[AC_CACHE_CHECK([for $LD option to reload object files],
  lt_cv_ld_reload_flag,
  [lt_cv_ld_reload_flag='-r'])
reload_flag=$lt_cv_ld_reload_flag
case $reload_flag in
"" | " "*) ;;
*) reload_flag=" $reload_flag" ;;
esac
reload_cmds='$LD$reload_flag -o $output$reload_objs'
case $host_os in
  darwin*)
    if test "$GCC" = yes; then
      reload_cmds='$LTCC $LTCFLAGS -nostdlib ${wl}-r -o $output$reload_objs'
    else
      reload_cmds='$LD$reload_flag -o $output$reload_objs'
    fi
    ;;
esac
])# AC_PROG_LD_RELOAD_FLAG


# AC_DEPLIBS_CHECK_METHOD
# -----------------------
# how to check for library dependencies
#  -- PORTME fill in with the dynamic library characteristics
AC_DEFUN([AC_DEPLIBS_CHECK_METHOD],
[AC_CACHE_CHECK([how to recognize dependent libraries],
lt_cv_deplibs_check_method,
[lt_cv_file_magic_cmd='$MAGIC_CMD'
lt_cv_file_magic_test_file=
lt_cv_deplibs_check_method='unknown'
# Need to set the preceding variable on all platforms that support
# interlibrary dependencies.
# 'none' -- dependencies not supported.
# `unknown' -- same as none, but documents that we really don't know.
# 'pass_all' -- all dependencies passed with no checks.
# 'test_compile' -- check by making test program.
# 'file_magic [[regex]]' -- check by looking for files in library path
# which responds to the $file_magic_cmd with a given extended regex.
# If you have `file' or equivalent on your system and you're not sure
# whether `pass_all' will *always* work, you probably want this one.

case $host_os in
aix[[4-9]]*)
  lt_cv_deplibs_check_method=pass_all
  ;;

beos*)
  lt_cv_deplibs_check_method=pass_all
  ;;

bsdi[[45]]*)
  lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (shared object|dynamic lib)'
  lt_cv_file_magic_cmd='/usr/bin/file -L'
  lt_cv_file_magic_test_file=/shlib/libc.so
  ;;

cygwin*)
  # func_win32_libid is a shell function defined in ltmain.sh
  lt_cv_deplibs_check_method='file_magic ^x86 archive import|^x86 DLL'
  lt_cv_file_magic_cmd='func_win32_libid'
  ;;

mingw* | pw32*)
  # Base MSYS/MinGW do not provide the 'file' command needed by
  # func_win32_libid shell function, so use a weaker test based on 'objdump',
  # unless we find 'file', for example because we are cross-compiling.
  if ( file / ) >/dev/null 2>&1; then
    lt_cv_deplibs_check_method='file_magic ^x86 archive import|^x86 DLL'
    lt_cv_file_magic_cmd='func_win32_libid'
  else
    lt_cv_deplibs_check_method='file_magic file format pei*-i386(.*architecture: i386)?'
    lt_cv_file_magic_cmd='$OBJDUMP -f'
  fi
  ;;

darwin* | rhapsody*)
  lt_cv_deplibs_check_method=pass_all
  ;;

freebsd* | dragonfly*)
  if echo __ELF__ | $CC -E - | grep __ELF__ > /dev/null; then
    case $host_cpu in
    i*86 )
      # Not sure whether the presence of OpenBSD here was a mistake.
      # Let's accept both of them until this is cleared up.
      lt_cv_deplibs_check_method='file_magic (FreeBSD|OpenBSD|DragonFly)/i[[3-9]]86 (compact )?demand paged shared library'
      lt_cv_file_magic_cmd=/usr/bin/file
      lt_cv_file_magic_test_file=`echo /usr/lib/libc.so.*`
      ;;
    esac
  else
    lt_cv_deplibs_check_method=pass_all
  fi
  ;;

gnu*)
  lt_cv_deplibs_check_method=pass_all
  ;;

hpux10.20* | hpux11*)
  lt_cv_file_magic_cmd=/usr/bin/file
  case $host_cpu in
  ia64*)
    lt_cv_deplibs_check_method='file_magic (s[[0-9]][[0-9]][[0-9]]|ELF-[[0-9]][[0-9]]) shared object file - IA64'
    lt_cv_file_magic_test_file=/usr/lib/hpux32/libc.so
    ;;
  hppa*64*)
    [lt_cv_deplibs_check_method='file_magic (s[0-9][0-9][0-9]|ELF-[0-9][0-9]) shared object file - PA-RISC [0-9].[0-9]']
    lt_cv_file_magic_test_file=/usr/lib/pa20_64/libc.sl
    ;;
  *)
    lt_cv_deplibs_check_method='file_magic (s[[0-9]][[0-9]][[0-9]]|PA-RISC[[0-9]].[[0-9]]) shared library'
    lt_cv_file_magic_test_file=/usr/lib/libc.sl
    ;;
  esac
  ;;

interix[[3-9]]*)
  # PIC code is broken on Interix 3.x, that's why |\.a not |_pic\.a here
  lt_cv_deplibs_check_method='match_pattern /lib[[^/]]+(\.so|\.a)$'
  ;;

irix5* | irix6* | nonstopux*)
  case $LD in
  *-32|*"-32 ") libmagic=32-bit;;
  *-n32|*"-n32 ") libmagic=N32;;
  *-64|*"-64 ") libmagic=64-bit;;
  *) libmagic=never-match;;
  esac
  lt_cv_deplibs_check_method=pass_all
  ;;

# This must be Linux ELF.
linux* | k*bsd*-gnu)
  lt_cv_deplibs_check_method=pass_all
  ;;

netbsd*)
  if echo __ELF__ | $CC -E - | grep __ELF__ > /dev/null; then
    lt_cv_deplibs_check_method='match_pattern /lib[[^/]]+(\.so\.[[0-9]]+\.[[0-9]]+|_pic\.a)$'
  else
    lt_cv_deplibs_check_method='match_pattern /lib[[^/]]+(\.so|_pic\.a)$'
  fi
  ;;

newos6*)
  lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (executable|dynamic lib)'
  lt_cv_file_magic_cmd=/usr/bin/file
  lt_cv_file_magic_test_file=/usr/lib/libnls.so
  ;;

nto-qnx*)
  lt_cv_deplibs_check_method=unknown
  ;;

openbsd*)
  if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
    lt_cv_deplibs_check_method='match_pattern /lib[[^/]]+(\.so\.[[0-9]]+\.[[0-9]]+|\.so|_pic\.a)$'
  else
    lt_cv_deplibs_check_method='match_pattern /lib[[^/]]+(\.so\.[[0-9]]+\.[[0-9]]+|_pic\.a)$'
  fi
  ;;

osf3* | osf4* | osf5*)
  lt_cv_deplibs_check_method=pass_all
  ;;

rdos*)
  lt_cv_deplibs_check_method=pass_all
  ;;

solaris*)
  lt_cv_deplibs_check_method=pass_all
  ;;

sysv4 | sysv4.3*)
  case $host_vendor in
  motorola)
    lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (shared object|dynamic lib) M[[0-9]][[0-9]]* Version [[0-9]]'
    lt_cv_file_magic_test_file=`echo /usr/lib/libc.so*`
    ;;
  ncr)
    lt_cv_deplibs_check_method=pass_all
    ;;
  sequent)
    lt_cv_file_magic_cmd='/bin/file'
    lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB (shared object|dynamic lib )'
    ;;
  sni)
    lt_cv_file_magic_cmd='/bin/file'
    lt_cv_deplibs_check_method="file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB dynamic lib"
    lt_cv_file_magic_test_file=/lib/libc.so
    ;;
  siemens)
    lt_cv_deplibs_check_method=pass_all
    ;;
  pc)
    lt_cv_deplibs_check_method=pass_all
    ;;
  esac
  ;;

sysv5* | sco3.2v5* | sco5v6* | unixware* | OpenUNIX* | sysv4*uw2*)
  lt_cv_deplibs_check_method=pass_all
  ;;
esac
])
file_magic_cmd=$lt_cv_file_magic_cmd
deplibs_check_method=$lt_cv_deplibs_check_method
test -z "$deplibs_check_method" && deplibs_check_method=unknown
])# AC_DEPLIBS_CHECK_METHOD


# AC_PROG_NM
# ----------
# find the pathname to a BSD-compatible name lister
AC_DEFUN([AC_PROG_NM],
[AC_CACHE_CHECK([for BSD-compatible nm], lt_cv_path_NM,
[if test -n "$NM"; then
  # Let the user override the test.
  lt_cv_path_NM="$NM"
else
  lt_nm_to_check="${ac_tool_prefix}nm"
  if test -n "$ac_tool_prefix" && test "$build" = "$host"; then
    lt_nm_to_check="$lt_nm_to_check nm"
  fi
  for lt_tmp_nm in $lt_nm_to_check; do
    lt_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
    for ac_dir in $PATH /usr/ccs/bin/elf /usr/ccs/bin /usr/ucb /bin; do
      IFS="$lt_save_ifs"
      test -z "$ac_dir" && ac_dir=.
      tmp_nm="$ac_dir/$lt_tmp_nm"
      if test -f "$tmp_nm" || test -f "$tmp_nm$ac_exeext" ; then
	# Check to see if the nm accepts a BSD-compat flag.
	# Adding the `sed 1q' prevents false positives on HP-UX, which says:
	#   nm: unknown option "B" ignored
	# Tru64's nm complains that /dev/null is an invalid object file
	case `"$tmp_nm" -B /dev/null 2>&1 | sed '1q'` in
	*/dev/null* | *'Invalid file or object type'*)
	  lt_cv_path_NM="$tmp_nm -B"
	  break
	  ;;
	*)
	  case `"$tmp_nm" -p /dev/null 2>&1 | sed '1q'` in
	  */dev/null*)
	    lt_cv_path_NM="$tmp_nm -p"
	    break
	    ;;
	  *)
	    lt_cv_path_NM=${lt_cv_path_NM="$tmp_nm"} # keep the first match, but
	    continue # so that we can try to find one that supports BSD flags
	    ;;
	  esac
	  ;;
	esac
      fi
    done
    IFS="$lt_save_ifs"
  done
  test -z "$lt_cv_path_NM" && lt_cv_path_NM=nm
fi])
NM="$lt_cv_path_NM"
])# AC_PROG_NM


# AC_CHECK_LIBM
# -------------
# check for math library
AC_DEFUN([AC_CHECK_LIBM],
[AC_REQUIRE([AC_CANONICAL_HOST])dnl
LIBM=
case $host in
*-*-beos* | *-*-cygwin* | *-*-pw32* | *-*-darwin*)
  # These system don't have libm, or don't need it
  ;;
*-ncr-sysv4.3*)
  AC_CHECK_LIB(mw, _mwvalidcheckl, LIBM="-lmw")
  AC_CHECK_LIB(m, cos, LIBM="$LIBM -lm")
  ;;
*)
  AC_CHECK_LIB(m, cos, LIBM="-lm")
  ;;
esac
])# AC_CHECK_LIBM


# AC_LIBLTDL_CONVENIENCE([DIRECTORY])
# -----------------------------------
# sets LIBLTDL to the link flags for the libltdl convenience library and
# LTDLINCL to the include flags for the libltdl header and adds
# --enable-ltdl-convenience to the configure arguments.  Note that
# AC_CONFIG_SUBDIRS is not called here.  If DIRECTORY is not provided,
# it is assumed to be `libltdl'.  LIBLTDL will be prefixed with
# '${top_builddir}/' and LTDLINCL will be prefixed with '${top_srcdir}/'
# (note the single quotes!).  If your package is not flat and you're not
# using automake, define top_builddir and top_srcdir appropriately in
# the Makefiles.
AC_DEFUN([AC_LIBLTDL_CONVENIENCE],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  case $enable_ltdl_convenience in
  no) AC_MSG_ERROR([this package needs a convenience libltdl]) ;;
  "") enable_ltdl_convenience=yes
      ac_configure_args="$ac_configure_args --enable-ltdl-convenience" ;;
  esac
  LIBLTDL='${top_builddir}/'ifelse($#,1,[$1],['libltdl'])/libltdlc.la
  LTDLINCL='-I${top_srcdir}/'ifelse($#,1,[$1],['libltdl'])
  # For backwards non-gettext consistent compatibility...
  INCLTDL="$LTDLINCL"
])# AC_LIBLTDL_CONVENIENCE


# AC_LIBLTDL_INSTALLABLE([DIRECTORY])
# -----------------------------------
# sets LIBLTDL to the link flags for the libltdl installable library and
# LTDLINCL to the include flags for the libltdl header and adds
# --enable-ltdl-install to the configure arguments.  Note that
# AC_CONFIG_SUBDIRS is not called here.  If DIRECTORY is not provided,
# and an installed libltdl is not found, it is assumed to be `libltdl'.
# LIBLTDL will be prefixed with '${top_builddir}/'# and LTDLINCL with
# '${top_srcdir}/' (note the single quotes!).  If your package is not
# flat and you're not using automake, define top_builddir and top_srcdir
# appropriately in the Makefiles.
# In the future, this macro may have to be called after AC_PROG_LIBTOOL.
AC_DEFUN([AC_LIBLTDL_INSTALLABLE],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  AC_CHECK_LIB(ltdl, lt_dlinit,
  [test x"$enable_ltdl_install" != xyes && enable_ltdl_install=no],
  [if test x"$enable_ltdl_install" = xno; then
     AC_MSG_WARN([libltdl not installed, but installation disabled])
   else
     enable_ltdl_install=yes
   fi
  ])
  if test x"$enable_ltdl_install" = x"yes"; then
    ac_configure_args="$ac_configure_args --enable-ltdl-install"
    LIBLTDL='${top_builddir}/'ifelse($#,1,[$1],['libltdl'])/libltdl.la
    LTDLINCL='-I${top_srcdir}/'ifelse($#,1,[$1],['libltdl'])
  else
    ac_configure_args="$ac_configure_args --enable-ltdl-install=no"
    LIBLTDL="-lltdl"
    LTDLINCL=
  fi
  # For backwards non-gettext consistent compatibility...
  INCLTDL="$LTDLINCL"
])# AC_LIBLTDL_INSTALLABLE


# AC_LIBTOOL_CXX
# --------------
# enable support for C++ libraries
AC_DEFUN([AC_LIBTOOL_CXX],
[AC_REQUIRE([_LT_AC_LANG_CXX])
])# AC_LIBTOOL_CXX


# _LT_AC_LANG_CXX
# ---------------
AC_DEFUN([_LT_AC_LANG_CXX],
[AC_REQUIRE([AC_PROG_CXX])
AC_REQUIRE([_LT_AC_PROG_CXXCPP])
_LT_AC_SHELL_INIT([tagnames=${tagnames+${tagnames},}CXX])
])# _LT_AC_LANG_CXX

# _LT_AC_PROG_CXXCPP
# ------------------
AC_DEFUN([_LT_AC_PROG_CXXCPP],
[
AC_REQUIRE([AC_PROG_CXX])
if test -n "$CXX" && ( test "X$CXX" != "Xno" &&
    ( (test "X$CXX" = "Xg++" && `g++ -v >/dev/null 2>&1` ) ||
    (test "X$CXX" != "Xg++"))) ; then
  AC_PROG_CXXCPP
fi
])# _LT_AC_PROG_CXXCPP

# AC_LIBTOOL_F77
# --------------
# enable support for Fortran 77 libraries
AC_DEFUN([AC_LIBTOOL_F77],
[AC_REQUIRE([_LT_AC_LANG_F77])
])# AC_LIBTOOL_F77


# _LT_AC_LANG_F77
# ---------------
AC_DEFUN([_LT_AC_LANG_F77],
[AC_REQUIRE([AC_PROG_F77])
_LT_AC_SHELL_INIT([tagnames=${tagnames+${tagnames},}F77])
])# _LT_AC_LANG_F77


# AC_LIBTOOL_GCJ
# --------------
# enable support for GCJ libraries
AC_DEFUN([AC_LIBTOOL_GCJ],
[AC_REQUIRE([_LT_AC_LANG_GCJ])
])# AC_LIBTOOL_GCJ


# _LT_AC_LANG_GCJ
# ---------------
AC_DEFUN([_LT_AC_LANG_GCJ],
[AC_PROVIDE_IFELSE([AC_PROG_GCJ],[],
  [AC_PROVIDE_IFELSE([A][M_PROG_GCJ],[],
    [AC_PROVIDE_IFELSE([LT_AC_PROG_GCJ],[],
      [ifdef([AC_PROG_GCJ],[AC_REQUIRE([AC_PROG_GCJ])],
	 [ifdef([A][M_PROG_GCJ],[AC_REQUIRE([A][M_PROG_GCJ])],
	   [AC_REQUIRE([A][C_PROG_GCJ_OR_A][M_PROG_GCJ])])])])])])
_LT_AC_SHELL_INIT([tagnames=${tagnames+${tagnames},}GCJ])
])# _LT_AC_LANG_GCJ


# AC_LIBTOOL_RC
# -------------
# enable support for Windows resource files
AC_DEFUN([AC_LIBTOOL_RC],
[AC_REQUIRE([LT_AC_PROG_RC])
_LT_AC_SHELL_INIT([tagnames=${tagnames+${tagnames},}RC])
])# AC_LIBTOOL_RC


# AC_LIBTOOL_LANG_C_CONFIG
# ------------------------
# Ensure that the configuration vars for the C compiler are
# suitably defined.  Those variables are subsequently used by
# AC_LIBTOOL_CONFIG to write the compiler configuration to `libtool'.
AC_DEFUN([AC_LIBTOOL_LANG_C_CONFIG], [_LT_AC_LANG_C_CONFIG])
AC_DEFUN([_LT_AC_LANG_C_CONFIG],
[lt_save_CC="$CC"
AC_LANG_PUSH(C)

# Source file extension for C test sources.
ac_ext=c

# Object file extension for compiled C test sources.
objext=o
_LT_AC_TAGVAR(objext, $1)=$objext

# Code to be used in simple compile tests
lt_simple_compile_test_code="int some_variable = 0;"

# Code to be used in simple link tests
lt_simple_link_test_code='int main(){return(0);}'

_LT_AC_SYS_COMPILER

# save warnings/boilerplate of simple test code
_LT_COMPILER_BOILERPLATE
_LT_LINKER_BOILERPLATE

AC_LIBTOOL_PROG_COMPILER_NO_RTTI($1)
AC_LIBTOOL_PROG_COMPILER_PIC($1)
AC_LIBTOOL_PROG_CC_C_O($1)
AC_LIBTOOL_SYS_HARD_LINK_LOCKS($1)
AC_LIBTOOL_PROG_LD_SHLIBS($1)
AC_LIBTOOL_SYS_DYNAMIC_LINKER($1)
AC_LIBTOOL_PROG_LD_HARDCODE_LIBPATH($1)
AC_LIBTOOL_SYS_LIB_STRIP
AC_LIBTOOL_DLOPEN_SELF

# Report which library types will actually be built
AC_MSG_CHECKING([if libtool supports shared libraries])
AC_MSG_RESULT([$can_build_shared])

AC_MSG_CHECKING([whether to build shared libraries])
test "$can_build_shared" = "no" && enable_shared=no

# On AIX, shared libraries and static libraries use the same namespace, and
# are all built from PIC.
case $host_os in
aix3*)
  test "$enable_shared" = yes && enable_static=no
  if test -n "$RANLIB"; then
    archive_cmds="$archive_cmds~\$RANLIB \$lib"
    postinstall_cmds='$RANLIB $lib'
  fi
  ;;

aix[[4-9]]*)
  if test "$host_cpu" != ia64 && test "$aix_use_runtimelinking" = no ; then
    test "$enable_shared" = yes && enable_static=no
  fi
    ;;
esac
AC_MSG_RESULT([$enable_shared])

AC_MSG_CHECKING([whether to build static libraries])
# Make sure either enable_shared or enable_static is yes.
test "$enable_shared" = yes || enable_static=yes
AC_MSG_RESULT([$enable_static])

AC_LIBTOOL_CONFIG($1)

AC_LANG_POP
CC="$lt_save_CC"
])# AC_LIBTOOL_LANG_C_CONFIG


# AC_LIBTOOL_LANG_CXX_CONFIG
# --------------------------
# Ensure that the configuration vars for the C compiler are
# suitably defined.  Those variables are subsequently used by
# AC_LIBTOOL_CONFIG to write the compiler configuration to `libtool'.
AC_DEFUN([AC_LIBTOOL_LANG_CXX_CONFIG], [_LT_AC_LANG_CXX_CONFIG(CXX)])
AC_DEFUN([_LT_AC_LANG_CXX_CONFIG],
[AC_LANG_PUSH(C++)
AC_REQUIRE([AC_PROG_CXX])
AC_REQUIRE([_LT_AC_PROG_CXXCPP])

_LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
_LT_AC_TAGVAR(allow_undefined_flag, $1)=
_LT_AC_TAGVAR(always_export_symbols, $1)=no
_LT_AC_TAGVAR(archive_expsym_cmds, $1)=
_LT_AC_TAGVAR(export_dynamic_flag_spec, $1)=
_LT_AC_TAGVAR(hardcode_direct, $1)=no
_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)=
_LT_AC_TAGVAR(hardcode_libdir_flag_spec_ld, $1)=
_LT_AC_TAGVAR(hardcode_libdir_separator, $1)=
_LT_AC_TAGVAR(hardcode_minus_L, $1)=no
_LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=unsupported
_LT_AC_TAGVAR(hardcode_automatic, $1)=no
_LT_AC_TAGVAR(module_cmds, $1)=
_LT_AC_TAGVAR(module_expsym_cmds, $1)=
_LT_AC_TAGVAR(link_all_deplibs, $1)=unknown
_LT_AC_TAGVAR(old_archive_cmds, $1)=$old_archive_cmds
_LT_AC_TAGVAR(no_undefined_flag, $1)=
_LT_AC_TAGVAR(whole_archive_flag_spec, $1)=
_LT_AC_TAGVAR(enable_shared_with_static_runtimes, $1)=no

# Dependencies to place before and after the object being linked:
_LT_AC_TAGVAR(predep_objects, $1)=
_LT_AC_TAGVAR(postdep_objects, $1)=
_LT_AC_TAGVAR(predeps, $1)=
_LT_AC_TAGVAR(postdeps, $1)=
_LT_AC_TAGVAR(compiler_lib_search_path, $1)=
_LT_AC_TAGVAR(compiler_lib_search_dirs, $1)=

# Source file extension for C++ test sources.
ac_ext=cpp

# Object file extension for compiled C++ test sources.
objext=o
_LT_AC_TAGVAR(objext, $1)=$objext

# Code to be used in simple compile tests
lt_simple_compile_test_code="int some_variable = 0;"

# Code to be used in simple link tests
lt_simple_link_test_code='int main(int, char *[[]]) { return(0); }'

# ltmain only uses $CC for tagged configurations so make sure $CC is set.
_LT_AC_SYS_COMPILER

# save warnings/boilerplate of simple test code
_LT_COMPILER_BOILERPLATE
_LT_LINKER_BOILERPLATE

# Allow CC to be a program name with arguments.
lt_save_CC=$CC
lt_save_LD=$LD
lt_save_GCC=$GCC
GCC=$GXX
lt_save_with_gnu_ld=$with_gnu_ld
lt_save_path_LD=$lt_cv_path_LD
if test -n "${lt_cv_prog_gnu_ldcxx+set}"; then
  lt_cv_prog_gnu_ld=$lt_cv_prog_gnu_ldcxx
else
  $as_unset lt_cv_prog_gnu_ld
fi
if test -n "${lt_cv_path_LDCXX+set}"; then
  lt_cv_path_LD=$lt_cv_path_LDCXX
else
  $as_unset lt_cv_path_LD
fi
test -z "${LDCXX+set}" || LD=$LDCXX
CC=${CXX-"c++"}
compiler=$CC
_LT_AC_TAGVAR(compiler, $1)=$CC
_LT_CC_BASENAME([$compiler])

# We don't want -fno-exception wen compiling C++ code, so set the
# no_builtin_flag separately
if test "$GXX" = yes; then
  _LT_AC_TAGVAR(lt_prog_compiler_no_builtin_flag, $1)=' -fno-builtin'
else
  _LT_AC_TAGVAR(lt_prog_compiler_no_builtin_flag, $1)=
fi

if test "$GXX" = yes; then
  # Set up default GNU C++ configuration

  AC_PROG_LD

  # Check if GNU C++ uses GNU ld as the underlying linker, since the
  # archiving commands below assume that GNU ld is being used.
  if test "$with_gnu_ld" = yes; then
    _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname $wl$soname -o $lib'
    _LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared -nostdlib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'

    _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}--rpath ${wl}$libdir'
    _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}--export-dynamic'

    # If archive_cmds runs LD, not CC, wlarc should be empty
    # XXX I think wlarc can be eliminated in ltcf-cxx, but I need to
    #     investigate it a little bit more. (MM)
    wlarc='${wl}'

    # ancient GNU ld didn't support --whole-archive et. al.
    if eval "`$CC -print-prog-name=ld` --help 2>&1" | \
	grep 'no-whole-archive' > /dev/null; then
      _LT_AC_TAGVAR(whole_archive_flag_spec, $1)="$wlarc"'--whole-archive$convenience '"$wlarc"'--no-whole-archive'
    else
      _LT_AC_TAGVAR(whole_archive_flag_spec, $1)=
    fi
  else
    with_gnu_ld=no
    wlarc=

    # A generic and very simple default shared library creation
    # command for GNU C++ for the case where it uses the native
    # linker, instead of GNU ld.  If possible, this setting should
    # overridden to take advantage of the native linker features on
    # the platform it is being used on.
    _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags -o $lib'
  fi

  # Commands to make compiler produce verbose output that lists
  # what "hidden" libraries, object files and flags are used when
  # linking a shared library.
  output_verbose_link_cmd='$CC -shared $CFLAGS -v conftest.$objext 2>&1 | grep "\-L"'

else
  GXX=no
  with_gnu_ld=no
  wlarc=
fi

# PORTME: fill in a description of your system's C++ link characteristics
AC_MSG_CHECKING([whether the $compiler linker ($LD) supports shared libraries])
_LT_AC_TAGVAR(ld_shlibs, $1)=yes
case $host_os in
  aix3*)
    # FIXME: insert proper C++ library support
    _LT_AC_TAGVAR(ld_shlibs, $1)=no
    ;;
  aix[[4-9]]*)
    if test "$host_cpu" = ia64; then
      # On IA64, the linker does run time linking by default, so we don't
      # have to do anything special.
      aix_use_runtimelinking=no
      exp_sym_flag='-Bexport'
      no_entry_flag=""
    else
      aix_use_runtimelinking=no

      # Test if we are trying to use run time linking or normal
      # AIX style linking. If -brtl is somewhere in LDFLAGS, we
      # need to do runtime linking.
      case $host_os in aix4.[[23]]|aix4.[[23]].*|aix[[5-9]]*)
	for ld_flag in $LDFLAGS; do
	  case $ld_flag in
	  *-brtl*)
	    aix_use_runtimelinking=yes
	    break
	    ;;
	  esac
	done
	;;
      esac

      exp_sym_flag='-bexport'
      no_entry_flag='-bnoentry'
    fi

    # When large executables or shared objects are built, AIX ld can
    # have problems creating the table of contents.  If linking a library
    # or program results in "error TOC overflow" add -mminimal-toc to
    # CXXFLAGS/CFLAGS for g++/gcc.  In the cases where that is not
    # enough to fix the problem, add -Wl,-bbigtoc to LDFLAGS.

    _LT_AC_TAGVAR(archive_cmds, $1)=''
    _LT_AC_TAGVAR(hardcode_direct, $1)=yes
    _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=':'
    _LT_AC_TAGVAR(link_all_deplibs, $1)=yes

    if test "$GXX" = yes; then
      case $host_os in aix4.[[012]]|aix4.[[012]].*)
      # We only want to do this on AIX 4.2 and lower, the check
      # below for broken collect2 doesn't work under 4.3+
	collect2name=`${CC} -print-prog-name=collect2`
	if test -f "$collect2name" && \
	   strings "$collect2name" | grep resolve_lib_name >/dev/null
	then
	  # We have reworked collect2
	  :
	else
	  # We have old collect2
	  _LT_AC_TAGVAR(hardcode_direct, $1)=unsupported
	  # It fails to find uninstalled libraries when the uninstalled
	  # path is not listed in the libpath.  Setting hardcode_minus_L
	  # to unsupported forces relinking
	  _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes
	  _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-L$libdir'
	  _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=
	fi
	;;
      esac
      shared_flag='-shared'
      if test "$aix_use_runtimelinking" = yes; then
	shared_flag="$shared_flag "'${wl}-G'
      fi
    else
      # not using gcc
      if test "$host_cpu" = ia64; then
	# VisualAge C++, Version 5.5 for AIX 5L for IA-64, Beta 3 Release
	# chokes on -Wl,-G. The following line is correct:
	shared_flag='-G'
      else
	if test "$aix_use_runtimelinking" = yes; then
	  shared_flag='${wl}-G'
	else
	  shared_flag='${wl}-bM:SRE'
	fi
      fi
    fi

    # It seems that -bexpall does not export symbols beginning with
    # underscore (_), so it is better to generate a list of symbols to export.
    _LT_AC_TAGVAR(always_export_symbols, $1)=yes
    if test "$aix_use_runtimelinking" = yes; then
      # Warning - without using the other runtime loading flags (-brtl),
      # -berok will link without error, but may produce a broken library.
      _LT_AC_TAGVAR(allow_undefined_flag, $1)='-berok'
      # Determine the default libpath from the value encoded in an empty executable.
      _LT_AC_SYS_LIBPATH_AIX
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-blibpath:$libdir:'"$aix_libpath"

      _LT_AC_TAGVAR(archive_expsym_cmds, $1)="\$CC"' -o $output_objdir/$soname $libobjs $deplibs '"\${wl}$no_entry_flag"' $compiler_flags `if test "x${allow_undefined_flag}" != "x"; then echo "${wl}${allow_undefined_flag}"; else :; fi` '"\${wl}$exp_sym_flag:\$export_symbols $shared_flag"
     else
      if test "$host_cpu" = ia64; then
	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-R $libdir:/usr/lib:/lib'
	_LT_AC_TAGVAR(allow_undefined_flag, $1)="-z nodefs"
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)="\$CC $shared_flag"' -o $output_objdir/$soname $libobjs $deplibs '"\${wl}$no_entry_flag"' $compiler_flags ${wl}${allow_undefined_flag} '"\${wl}$exp_sym_flag:\$export_symbols"
      else
	# Determine the default libpath from the value encoded in an empty executable.
	_LT_AC_SYS_LIBPATH_AIX
	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-blibpath:$libdir:'"$aix_libpath"
	# Warning - without using the other run time loading flags,
	# -berok will link without error, but may produce a broken library.
	_LT_AC_TAGVAR(no_undefined_flag, $1)=' ${wl}-bernotok'
	_LT_AC_TAGVAR(allow_undefined_flag, $1)=' ${wl}-berok'
	# Exported symbols can be pulled into shared objects from archives
	_LT_AC_TAGVAR(whole_archive_flag_spec, $1)='$convenience'
	_LT_AC_TAGVAR(archive_cmds_need_lc, $1)=yes
	# This is similar to how AIX traditionally builds its shared libraries.
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)="\$CC $shared_flag"' -o $output_objdir/$soname $libobjs $deplibs ${wl}-bnoentry $compiler_flags ${wl}-bE:$export_symbols${allow_undefined_flag}~$AR $AR_FLAGS $output_objdir/$libname$release.a $output_objdir/$soname'
      fi
    fi
    ;;

  beos*)
    if $LD --help 2>&1 | grep ': supported targets:.* elf' > /dev/null; then
      _LT_AC_TAGVAR(allow_undefined_flag, $1)=unsupported
      # Joseph Beckenbach <jrb3@best.com> says some releases of gcc
      # support --undefined.  This deserves some investigation.  FIXME
      _LT_AC_TAGVAR(archive_cmds, $1)='$CC -nostart $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
    else
      _LT_AC_TAGVAR(ld_shlibs, $1)=no
    fi
    ;;

  chorus*)
    case $cc_basename in
      *)
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
    esac
    ;;

  cygwin* | mingw* | pw32*)
    # _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1) is actually meaningless,
    # as there is no search path for DLLs.
    _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-L$libdir'
    _LT_AC_TAGVAR(allow_undefined_flag, $1)=unsupported
    _LT_AC_TAGVAR(always_export_symbols, $1)=no
    _LT_AC_TAGVAR(enable_shared_with_static_runtimes, $1)=yes

    if $LD --help 2>&1 | grep 'auto-import' > /dev/null; then
      _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags -o $output_objdir/$soname ${wl}--enable-auto-image-base -Xlinker --out-implib -Xlinker $lib'
      # If the export-symbols file already is a .def file (1st line
      # is EXPORTS), use it as is; otherwise, prepend...
      _LT_AC_TAGVAR(archive_expsym_cmds, $1)='if test "x`$SED 1q $export_symbols`" = xEXPORTS; then
	cp $export_symbols $output_objdir/$soname.def;
      else
	echo EXPORTS > $output_objdir/$soname.def;
	cat $export_symbols >> $output_objdir/$soname.def;
      fi~
      $CC -shared -nostdlib $output_objdir/$soname.def $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags -o $output_objdir/$soname ${wl}--enable-auto-image-base -Xlinker --out-implib -Xlinker $lib'
    else
      _LT_AC_TAGVAR(ld_shlibs, $1)=no
    fi
  ;;
      darwin* | rhapsody*)
      _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
      _LT_AC_TAGVAR(hardcode_direct, $1)=no
      _LT_AC_TAGVAR(hardcode_automatic, $1)=yes
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=unsupported
      _LT_AC_TAGVAR(whole_archive_flag_spec, $1)=''
      _LT_AC_TAGVAR(link_all_deplibs, $1)=yes
      _LT_AC_TAGVAR(allow_undefined_flag, $1)="$_lt_dar_allow_undefined"
      if test "$GXX" = yes ; then
      output_verbose_link_cmd='echo'
      _LT_AC_TAGVAR(archive_cmds, $1)="\$CC -dynamiclib \$allow_undefined_flag -o \$lib \$libobjs \$deplibs \$compiler_flags -install_name \$rpath/\$soname \$verstring $_lt_dar_single_mod${_lt_dsymutil}"
      _LT_AC_TAGVAR(module_cmds, $1)="\$CC \$allow_undefined_flag -o \$lib -bundle \$libobjs \$deplibs \$compiler_flags${_lt_dsymutil}"
      _LT_AC_TAGVAR(archive_expsym_cmds, $1)="sed 's,^,_,' < \$export_symbols > \$output_objdir/\${libname}-symbols.expsym~\$CC -dynamiclib \$allow_undefined_flag -o \$lib \$libobjs \$deplibs \$compiler_flags -install_name \$rpath/\$soname \$verstring ${_lt_dar_single_mod}${_lt_dar_export_syms}${_lt_dsymutil}"
      _LT_AC_TAGVAR(module_expsym_cmds, $1)="sed -e 's,^,_,' < \$export_symbols > \$output_objdir/\${libname}-symbols.expsym~\$CC \$allow_undefined_flag -o \$lib -bundle \$libobjs \$deplibs \$compiler_flags${_lt_dar_export_syms}${_lt_dsymutil}"
      if test "$lt_cv_apple_cc_single_mod" != "yes"; then
        _LT_AC_TAGVAR(archive_cmds, $1)="\$CC -r -keep_private_externs -nostdlib -o \${lib}-master.o \$libobjs~\$CC -dynamiclib \$allow_undefined_flag -o \$lib \${lib}-master.o \$deplibs \$compiler_flags -install_name \$rpath/\$soname \$verstring${_lt_dsymutil}"
        _LT_AC_TAGVAR(archive_expsym_cmds, $1)="sed 's,^,_,' < \$export_symbols > \$output_objdir/\${libname}-symbols.expsym~\$CC -r -keep_private_externs -nostdlib -o \${lib}-master.o \$libobjs~\$CC -dynamiclib \$allow_undefined_flag -o \$lib \${lib}-master.o \$deplibs \$compiler_flags -install_name \$rpath/\$soname \$verstring${_lt_dar_export_syms}${_lt_dsymutil}"
      fi
      else
      case $cc_basename in
        xlc*)
         output_verbose_link_cmd='echo'
          _LT_AC_TAGVAR(archive_cmds, $1)='$CC -qmkshrobj ${wl}-single_module $allow_undefined_flag -o $lib $libobjs $deplibs $compiler_flags ${wl}-install_name ${wl}`echo $rpath/$soname` $xlcverstring'
          _LT_AC_TAGVAR(module_cmds, $1)='$CC $allow_undefined_flag -o $lib -bundle $libobjs $deplibs$compiler_flags'
          # Don't fix this by using the ld -exported_symbols_list flag, it doesn't exist in older darwin lds
          _LT_AC_TAGVAR(archive_expsym_cmds, $1)='sed -e "s,#.*,," -e "s,^[    ]*,," -e "s,^\(..*\),_&," < $export_symbols > $output_objdir/${libname}-symbols.expsym~$CC -qmkshrobj ${wl}-single_module $allow_undefined_flag -o $lib $libobjs $deplibs $compiler_flags ${wl}-install_name ${wl}$rpath/$soname $xlcverstring~nmedit -s $output_objdir/${libname}-symbols.expsym ${lib}'
          _LT_AC_TAGVAR(module_expsym_cmds, $1)='sed -e "s,#.*,," -e "s,^[    ]*,," -e "s,^\(..*\),_&," < $export_symbols > $output_objdir/${libname}-symbols.expsym~$CC $allow_undefined_flag  -o $lib -bundle $libobjs $deplibs$compiler_flags~nmedit -s $output_objdir/${libname}-symbols.expsym ${lib}'
          ;;
       *)
         _LT_AC_TAGVAR(ld_shlibs, $1)=no
          ;;
      esac
      fi
        ;;

  dgux*)
    case $cc_basename in
      ec++*)
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
      ghcx*)
	# Green Hills C++ Compiler
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
      *)
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
    esac
    ;;
  freebsd[[12]]*)
    # C++ shared libraries reported to be fairly broken before switch to ELF
    _LT_AC_TAGVAR(ld_shlibs, $1)=no
    ;;
  freebsd-elf*)
    _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
    ;;
  freebsd* | dragonfly*)
    # FreeBSD 3 and later use GNU C++ and GNU ld with standard ELF
    # conventions
    _LT_AC_TAGVAR(ld_shlibs, $1)=yes
    ;;
  gnu*)
    ;;
  hpux9*)
    _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}+b ${wl}$libdir'
    _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:
    _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-E'
    _LT_AC_TAGVAR(hardcode_direct, $1)=yes
    _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes # Not in the search PATH,
				# but as the default
				# location of the library.

    case $cc_basename in
    CC*)
      # FIXME: insert proper C++ library support
      _LT_AC_TAGVAR(ld_shlibs, $1)=no
      ;;
    aCC*)
      _LT_AC_TAGVAR(archive_cmds, $1)='$rm $output_objdir/$soname~$CC -b ${wl}+b ${wl}$install_libdir -o $output_objdir/$soname $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags~test $output_objdir/$soname = $lib || mv $output_objdir/$soname $lib'
      # Commands to make compiler produce verbose output that lists
      # what "hidden" libraries, object files and flags are used when
      # linking a shared library.
      #
      # There doesn't appear to be a way to prevent this compiler from
      # explicitly linking system object files so we need to strip them
      # from the output so that they don't get included in the library
      # dependencies.
      output_verbose_link_cmd='templist=`($CC -b $CFLAGS -v conftest.$objext 2>&1) | grep "[[-]]L"`; list=""; for z in $templist; do case $z in conftest.$objext) list="$list $z";; *.$objext);; *) list="$list $z";;esac; done; echo $list'
      ;;
    *)
      if test "$GXX" = yes; then
        _LT_AC_TAGVAR(archive_cmds, $1)='$rm $output_objdir/$soname~$CC -shared -nostdlib -fPIC ${wl}+b ${wl}$install_libdir -o $output_objdir/$soname $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags~test $output_objdir/$soname = $lib || mv $output_objdir/$soname $lib'
      else
        # FIXME: insert proper C++ library support
        _LT_AC_TAGVAR(ld_shlibs, $1)=no
      fi
      ;;
    esac
    ;;
  hpux10*|hpux11*)
    if test $with_gnu_ld = no; then
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}+b ${wl}$libdir'
      _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:

      case $host_cpu in
      hppa*64*|ia64*) ;;
      *)
	_LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-E'
        ;;
      esac
    fi
    case $host_cpu in
    hppa*64*|ia64*)
      _LT_AC_TAGVAR(hardcode_direct, $1)=no
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;
    *)
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes # Not in the search PATH,
					      # but as the default
					      # location of the library.
      ;;
    esac

    case $cc_basename in
      CC*)
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
      aCC*)
	case $host_cpu in
	hppa*64*)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -b ${wl}+h ${wl}$soname -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags'
	  ;;
	ia64*)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -b ${wl}+h ${wl}$soname ${wl}+nodefaultrpath -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags'
	  ;;
	*)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -b ${wl}+h ${wl}$soname ${wl}+b ${wl}$install_libdir -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags'
	  ;;
	esac
	# Commands to make compiler produce verbose output that lists
	# what "hidden" libraries, object files and flags are used when
	# linking a shared library.
	#
	# There doesn't appear to be a way to prevent this compiler from
	# explicitly linking system object files so we need to strip them
	# from the output so that they don't get included in the library
	# dependencies.
	output_verbose_link_cmd='templist=`($CC -b $CFLAGS -v conftest.$objext 2>&1) | grep "\-L"`; list=""; for z in $templist; do case $z in conftest.$objext) list="$list $z";; *.$objext);; *) list="$list $z";;esac; done; echo $list'
	;;
      *)
	if test "$GXX" = yes; then
	  if test $with_gnu_ld = no; then
	    case $host_cpu in
	    hppa*64*)
	      _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib -fPIC ${wl}+h ${wl}$soname -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags'
	      ;;
	    ia64*)
	      _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib -fPIC ${wl}+h ${wl}$soname ${wl}+nodefaultrpath -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags'
	      ;;
	    *)
	      _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib -fPIC ${wl}+h ${wl}$soname ${wl}+b ${wl}$install_libdir -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags'
	      ;;
	    esac
	  fi
	else
	  # FIXME: insert proper C++ library support
	  _LT_AC_TAGVAR(ld_shlibs, $1)=no
	fi
	;;
    esac
    ;;
  interix[[3-9]]*)
    _LT_AC_TAGVAR(hardcode_direct, $1)=no
    _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
    _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath,$libdir'
    _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-E'
    # Hack: On Interix 3.x, we cannot compile PIC because of a broken gcc.
    # Instead, shared libraries are loaded at an image base (0x10000000 by
    # default) and relocated if they conflict, which is a slow very memory
    # consuming and fragmenting process.  To avoid this, we pick a random,
    # 256 KiB-aligned image base between 0x50000000 and 0x6FFC0000 at link
    # time.  Moving up from 0x10000000 also allows more sbrk(2) space.
    _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $pic_flag $libobjs $deplibs $compiler_flags ${wl}-h,$soname ${wl}--image-base,`expr ${RANDOM-$$} % 4096 / 2 \* 262144 + 1342177280` -o $lib'
    _LT_AC_TAGVAR(archive_expsym_cmds, $1)='sed "s,^,_," $export_symbols >$output_objdir/$soname.expsym~$CC -shared $pic_flag $libobjs $deplibs $compiler_flags ${wl}-h,$soname ${wl}--retain-symbols-file,$output_objdir/$soname.expsym ${wl}--image-base,`expr ${RANDOM-$$} % 4096 / 2 \* 262144 + 1342177280` -o $lib'
    ;;
  irix5* | irix6*)
    case $cc_basename in
      CC*)
	# SGI C++
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -all -multigot $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'

	# Archives containing C++ object files must be created using
	# "CC -ar", where "CC" is the IRIX C++ compiler.  This is
	# necessary to make sure instantiated templates are included
	# in the archive.
	_LT_AC_TAGVAR(old_archive_cmds, $1)='$CC -ar -WR,-u -o $oldlib $oldobjs'
	;;
      *)
	if test "$GXX" = yes; then
	  if test "$with_gnu_ld" = no; then
	    _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
	  else
	    _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` -o $lib'
	  fi
	fi
	_LT_AC_TAGVAR(link_all_deplibs, $1)=yes
	;;
    esac
    _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath ${wl}$libdir'
    _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:
    ;;
  linux* | k*bsd*-gnu)
    case $cc_basename in
      KCC*)
	# Kuck and Associates, Inc. (KAI) C++ Compiler

	# KCC will only create a shared library if the output file
	# ends with ".so" (or ".sl" for HP-UX), so rename the library
	# to its proper name (with version) after linking.
	_LT_AC_TAGVAR(archive_cmds, $1)='tempext=`echo $shared_ext | $SED -e '\''s/\([[^()0-9A-Za-z{}]]\)/\\\\\1/g'\''`; templib=`echo $lib | $SED -e "s/\${tempext}\..*/.so/"`; $CC $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags --soname $soname -o \$templib; mv \$templib $lib'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='tempext=`echo $shared_ext | $SED -e '\''s/\([[^()0-9A-Za-z{}]]\)/\\\\\1/g'\''`; templib=`echo $lib | $SED -e "s/\${tempext}\..*/.so/"`; $CC $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags --soname $soname -o \$templib ${wl}-retain-symbols-file,$export_symbols; mv \$templib $lib'
	# Commands to make compiler produce verbose output that lists
	# what "hidden" libraries, object files and flags are used when
	# linking a shared library.
	#
	# There doesn't appear to be a way to prevent this compiler from
	# explicitly linking system object files so we need to strip them
	# from the output so that they don't get included in the library
	# dependencies.
	output_verbose_link_cmd='templist=`$CC $CFLAGS -v conftest.$objext -o libconftest$shared_ext 2>&1 | grep "ld"`; rm -f libconftest$shared_ext; list=""; for z in $templist; do case $z in conftest.$objext) list="$list $z";; *.$objext);; *) list="$list $z";;esac; done; echo $list'

	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}--rpath,$libdir'
	_LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}--export-dynamic'

	# Archives containing C++ object files must be created using
	# "CC -Bstatic", where "CC" is the KAI C++ compiler.
	_LT_AC_TAGVAR(old_archive_cmds, $1)='$CC -Bstatic -o $oldlib $oldobjs'
	;;
      icpc*)
	# Intel C++
	with_gnu_ld=yes
	# version 8.0 and above of icpc choke on multiply defined symbols
	# if we add $predep_objects and $postdep_objects, however 7.1 and
	# earlier do not add the objects themselves.
	case `$CC -V 2>&1` in
	*"Version 7."*)
  	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname $wl$soname -o $lib'
  	  _LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
	  ;;
	*)  # Version 8.0 or newer
	  tmp_idyn=
	  case $host_cpu in
	    ia64*) tmp_idyn=' -i_dynamic';;
	  esac
  	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared'"$tmp_idyn"' $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
	  _LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared'"$tmp_idyn"' $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
	  ;;
	esac
	_LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath,$libdir'
	_LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}--export-dynamic'
	_LT_AC_TAGVAR(whole_archive_flag_spec, $1)='${wl}--whole-archive$convenience ${wl}--no-whole-archive'
	;;
      pgCC* | pgcpp*)
        # Portland Group C++ compiler
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $pic_flag $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname ${wl}$soname -o $lib'
  	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared $pic_flag $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname ${wl}$soname ${wl}-retain-symbols-file ${wl}$export_symbols -o $lib'

	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}--rpath ${wl}$libdir'
	_LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}--export-dynamic'
	_LT_AC_TAGVAR(whole_archive_flag_spec, $1)='${wl}--whole-archive`for conv in $convenience\"\"; do test  -n \"$conv\" && new_convenience=\"$new_convenience,$conv\"; done; $echo \"$new_convenience\"` ${wl}--no-whole-archive'
        ;;
      cxx*)
	# Compaq C++
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname $wl$soname -o $lib'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname $wl$soname  -o $lib ${wl}-retain-symbols-file $wl$export_symbols'

	runpath_var=LD_RUN_PATH
	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-rpath $libdir'
	_LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:

	# Commands to make compiler produce verbose output that lists
	# what "hidden" libraries, object files and flags are used when
	# linking a shared library.
	#
	# There doesn't appear to be a way to prevent this compiler from
	# explicitly linking system object files so we need to strip them
	# from the output so that they don't get included in the library
	# dependencies.
	output_verbose_link_cmd='templist=`$CC -shared $CFLAGS -v conftest.$objext 2>&1 | grep "ld"`; templist=`echo $templist | $SED "s/\(^.*ld.*\)\( .*ld .*$\)/\1/"`; list=""; for z in $templist; do case $z in conftest.$objext) list="$list $z";; *.$objext);; *) list="$list $z";;esac; done; echo $list'
	;;
      *)
	case `$CC -V 2>&1 | sed 5q` in
	*Sun\ C*)
	  # Sun C++ 5.9
	  _LT_AC_TAGVAR(no_undefined_flag, $1)=' -zdefs'
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -G${allow_undefined_flag} -h$soname -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags'
	  _LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -G${allow_undefined_flag} -h$soname -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-retain-symbols-file ${wl}$export_symbols'
	  _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-R$libdir'
	  _LT_AC_TAGVAR(whole_archive_flag_spec, $1)='${wl}--whole-archive`new_convenience=; for conv in $convenience\"\"; do test -z \"$conv\" || new_convenience=\"$new_convenience,$conv\"; done; $echo \"$new_convenience\"` ${wl}--no-whole-archive'

	  # Not sure whether something based on
	  # $CC $CFLAGS -v conftest.$objext -o libconftest$shared_ext 2>&1
	  # would be better.
	  output_verbose_link_cmd='echo'

	  # Archives containing C++ object files must be created using
	  # "CC -xar", where "CC" is the Sun C++ compiler.  This is
	  # necessary to make sure instantiated templates are included
	  # in the archive.
	  _LT_AC_TAGVAR(old_archive_cmds, $1)='$CC -xar -o $oldlib $oldobjs'
	  ;;
	esac
	;;
    esac
    ;;
  lynxos*)
    # FIXME: insert proper C++ library support
    _LT_AC_TAGVAR(ld_shlibs, $1)=no
    ;;
  m88k*)
    # FIXME: insert proper C++ library support
    _LT_AC_TAGVAR(ld_shlibs, $1)=no
    ;;
  mvs*)
    case $cc_basename in
      cxx*)
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
      *)
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
    esac
    ;;
  netbsd*)
    if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
      _LT_AC_TAGVAR(archive_cmds, $1)='$LD -Bshareable  -o $lib $predep_objects $libobjs $deplibs $postdep_objects $linker_flags'
      wlarc=
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-R$libdir'
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
    fi
    # Workaround some broken pre-1.5 toolchains
    output_verbose_link_cmd='$CC -shared $CFLAGS -v conftest.$objext 2>&1 | grep conftest.$objext | $SED -e "s:-lgcc -lc -lgcc::"'
    ;;
  openbsd2*)
    # C++ shared libraries are fairly broken
    _LT_AC_TAGVAR(ld_shlibs, $1)=no
    ;;
  openbsd*)
    if test -f /usr/libexec/ld.so; then
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $pic_flag $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags -o $lib'
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath,$libdir'
      if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared $pic_flag $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-retain-symbols-file,$export_symbols -o $lib'
	_LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-E'
	_LT_AC_TAGVAR(whole_archive_flag_spec, $1)="$wlarc"'--whole-archive$convenience '"$wlarc"'--no-whole-archive'
      fi
      output_verbose_link_cmd='echo'
    else
      _LT_AC_TAGVAR(ld_shlibs, $1)=no
    fi
    ;;
  osf3*)
    case $cc_basename in
      KCC*)
	# Kuck and Associates, Inc. (KAI) C++ Compiler

	# KCC will only create a shared library if the output file
	# ends with ".so" (or ".sl" for HP-UX), so rename the library
	# to its proper name (with version) after linking.
	_LT_AC_TAGVAR(archive_cmds, $1)='tempext=`echo $shared_ext | $SED -e '\''s/\([[^()0-9A-Za-z{}]]\)/\\\\\1/g'\''`; templib=`echo $lib | $SED -e "s/\${tempext}\..*/.so/"`; $CC $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags --soname $soname -o \$templib; mv \$templib $lib'

	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath,$libdir'
	_LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:

	# Archives containing C++ object files must be created using
	# "CC -Bstatic", where "CC" is the KAI C++ compiler.
	_LT_AC_TAGVAR(old_archive_cmds, $1)='$CC -Bstatic -o $oldlib $oldobjs'

	;;
      RCC*)
	# Rational C++ 2.4.1
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
      cxx*)
	_LT_AC_TAGVAR(allow_undefined_flag, $1)=' ${wl}-expect_unresolved ${wl}\*'
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared${allow_undefined_flag} $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname $soname `test -n "$verstring" && echo ${wl}-set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'

	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath ${wl}$libdir'
	_LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:

	# Commands to make compiler produce verbose output that lists
	# what "hidden" libraries, object files and flags are used when
	# linking a shared library.
	#
	# There doesn't appear to be a way to prevent this compiler from
	# explicitly linking system object files so we need to strip them
	# from the output so that they don't get included in the library
	# dependencies.
	output_verbose_link_cmd='templist=`$CC -shared $CFLAGS -v conftest.$objext 2>&1 | grep "ld" | grep -v "ld:"`; templist=`echo $templist | $SED "s/\(^.*ld.*\)\( .*ld.*$\)/\1/"`; list=""; for z in $templist; do case $z in conftest.$objext) list="$list $z";; *.$objext);; *) list="$list $z";;esac; done; echo $list'
	;;
      *)
	if test "$GXX" = yes && test "$with_gnu_ld" = no; then
	  _LT_AC_TAGVAR(allow_undefined_flag, $1)=' ${wl}-expect_unresolved ${wl}\*'
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib ${allow_undefined_flag} $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'

	  _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath ${wl}$libdir'
	  _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:

	  # Commands to make compiler produce verbose output that lists
	  # what "hidden" libraries, object files and flags are used when
	  # linking a shared library.
	  output_verbose_link_cmd='$CC -shared $CFLAGS -v conftest.$objext 2>&1 | grep "\-L"'

	else
	  # FIXME: insert proper C++ library support
	  _LT_AC_TAGVAR(ld_shlibs, $1)=no
	fi
	;;
    esac
    ;;
  osf4* | osf5*)
    case $cc_basename in
      KCC*)
	# Kuck and Associates, Inc. (KAI) C++ Compiler

	# KCC will only create a shared library if the output file
	# ends with ".so" (or ".sl" for HP-UX), so rename the library
	# to its proper name (with version) after linking.
	_LT_AC_TAGVAR(archive_cmds, $1)='tempext=`echo $shared_ext | $SED -e '\''s/\([[^()0-9A-Za-z{}]]\)/\\\\\1/g'\''`; templib=`echo $lib | $SED -e "s/\${tempext}\..*/.so/"`; $CC $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags --soname $soname -o \$templib; mv \$templib $lib'

	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath,$libdir'
	_LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:

	# Archives containing C++ object files must be created using
	# the KAI C++ compiler.
	_LT_AC_TAGVAR(old_archive_cmds, $1)='$CC -o $oldlib $oldobjs'
	;;
      RCC*)
	# Rational C++ 2.4.1
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
      cxx*)
	_LT_AC_TAGVAR(allow_undefined_flag, $1)=' -expect_unresolved \*'
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared${allow_undefined_flag} $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags -msym -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='for i in `cat $export_symbols`; do printf "%s %s\\n" -exported_symbol "\$i" >> $lib.exp; done~
	  echo "-hidden">> $lib.exp~
	  $CC -shared$allow_undefined_flag $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags -msym -soname $soname -Wl,-input -Wl,$lib.exp  `test -n "$verstring" && echo -set_version	$verstring` -update_registry ${output_objdir}/so_locations -o $lib~
	  $rm $lib.exp'

	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-rpath $libdir'
	_LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:

	# Commands to make compiler produce verbose output that lists
	# what "hidden" libraries, object files and flags are used when
	# linking a shared library.
	#
	# There doesn't appear to be a way to prevent this compiler from
	# explicitly linking system object files so we need to strip them
	# from the output so that they don't get included in the library
	# dependencies.
	output_verbose_link_cmd='templist=`$CC -shared $CFLAGS -v conftest.$objext 2>&1 | grep "ld" | grep -v "ld:"`; templist=`echo $templist | $SED "s/\(^.*ld.*\)\( .*ld.*$\)/\1/"`; list=""; for z in $templist; do case $z in conftest.$objext) list="$list $z";; *.$objext);; *) list="$list $z";;esac; done; echo $list'
	;;
      *)
	if test "$GXX" = yes && test "$with_gnu_ld" = no; then
	  _LT_AC_TAGVAR(allow_undefined_flag, $1)=' ${wl}-expect_unresolved ${wl}\*'
	 _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib ${allow_undefined_flag} $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-msym ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'

	  _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath ${wl}$libdir'
	  _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:

	  # Commands to make compiler produce verbose output that lists
	  # what "hidden" libraries, object files and flags are used when
	  # linking a shared library.
	  output_verbose_link_cmd='$CC -shared $CFLAGS -v conftest.$objext 2>&1 | grep "\-L"'

	else
	  # FIXME: insert proper C++ library support
	  _LT_AC_TAGVAR(ld_shlibs, $1)=no
	fi
	;;
    esac
    ;;
  psos*)
    # FIXME: insert proper C++ library support
    _LT_AC_TAGVAR(ld_shlibs, $1)=no
    ;;
  sunos4*)
    case $cc_basename in
      CC*)
	# Sun C++ 4.x
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
      lcc*)
	# Lucid
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
      *)
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
    esac
    ;;
  solaris*)
    case $cc_basename in
      CC*)
	# Sun C++ 4.2, 5.x and Centerline C++
        _LT_AC_TAGVAR(archive_cmds_need_lc,$1)=yes
	_LT_AC_TAGVAR(no_undefined_flag, $1)=' -zdefs'
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -G${allow_undefined_flag}  -h$soname -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$echo "{ global:" > $lib.exp~cat $export_symbols | $SED -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
	$CC -G${allow_undefined_flag}  ${wl}-M ${wl}$lib.exp -h$soname -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags~$rm $lib.exp'

	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-R$libdir'
	_LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
	case $host_os in
	  solaris2.[[0-5]] | solaris2.[[0-5]].*) ;;
	  *)
	    # The compiler driver will combine and reorder linker options,
	    # but understands `-z linker_flag'.
	    # Supported since Solaris 2.6 (maybe 2.5.1?)
	    _LT_AC_TAGVAR(whole_archive_flag_spec, $1)='-z allextract$convenience -z defaultextract'
	    ;;
	esac
	_LT_AC_TAGVAR(link_all_deplibs, $1)=yes

	output_verbose_link_cmd='echo'

	# Archives containing C++ object files must be created using
	# "CC -xar", where "CC" is the Sun C++ compiler.  This is
	# necessary to make sure instantiated templates are included
	# in the archive.
	_LT_AC_TAGVAR(old_archive_cmds, $1)='$CC -xar -o $oldlib $oldobjs'
	;;
      gcx*)
	# Green Hills C++ Compiler
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-h $wl$soname -o $lib'

	# The C++ compiler must be used to create the archive.
	_LT_AC_TAGVAR(old_archive_cmds, $1)='$CC $LDFLAGS -archive -o $oldlib $oldobjs'
	;;
      *)
	# GNU C++ compiler with Solaris linker
	if test "$GXX" = yes && test "$with_gnu_ld" = no; then
	  _LT_AC_TAGVAR(no_undefined_flag, $1)=' ${wl}-z ${wl}defs'
	  if $CC --version | grep -v '^2\.7' > /dev/null; then
	    _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -nostdlib $LDFLAGS $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-h $wl$soname -o $lib'
	    _LT_AC_TAGVAR(archive_expsym_cmds, $1)='$echo "{ global:" > $lib.exp~cat $export_symbols | $SED -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
		$CC -shared -nostdlib ${wl}-M $wl$lib.exp -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags~$rm $lib.exp'

	    # Commands to make compiler produce verbose output that lists
	    # what "hidden" libraries, object files and flags are used when
	    # linking a shared library.
	    output_verbose_link_cmd="$CC -shared $CFLAGS -v conftest.$objext 2>&1 | grep \"\-L\""
	  else
	    # g++ 2.7 appears to require `-G' NOT `-shared' on this
	    # platform.
	    _LT_AC_TAGVAR(archive_cmds, $1)='$CC -G -nostdlib $LDFLAGS $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags ${wl}-h $wl$soname -o $lib'
	    _LT_AC_TAGVAR(archive_expsym_cmds, $1)='$echo "{ global:" > $lib.exp~cat $export_symbols | $SED -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
		$CC -G -nostdlib ${wl}-M $wl$lib.exp -o $lib $predep_objects $libobjs $deplibs $postdep_objects $compiler_flags~$rm $lib.exp'

	    # Commands to make compiler produce verbose output that lists
	    # what "hidden" libraries, object files and flags are used when
	    # linking a shared library.
	    output_verbose_link_cmd="$CC -G $CFLAGS -v conftest.$objext 2>&1 | grep \"\-L\""
	  fi

	  _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-R $wl$libdir'
	  case $host_os in
	  solaris2.[[0-5]] | solaris2.[[0-5]].*) ;;
	  *)
	    _LT_AC_TAGVAR(whole_archive_flag_spec, $1)='${wl}-z ${wl}allextract$convenience ${wl}-z ${wl}defaultextract'
	    ;;
	  esac
	fi
	;;
    esac
    ;;
  sysv4*uw2* | sysv5OpenUNIX* | sysv5UnixWare7.[[01]].[[10]]* | unixware7* | sco3.2v5.0.[[024]]*)
    _LT_AC_TAGVAR(no_undefined_flag, $1)='${wl}-z,text'
    _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
    _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
    runpath_var='LD_RUN_PATH'

    case $cc_basename in
      CC*)
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -G ${wl}-h,$soname -o $lib $libobjs $deplibs $compiler_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -G ${wl}-Bexport:$export_symbols ${wl}-h,$soname -o $lib $libobjs $deplibs $compiler_flags'
	;;
      *)
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared ${wl}-h,$soname -o $lib $libobjs $deplibs $compiler_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared ${wl}-Bexport:$export_symbols ${wl}-h,$soname -o $lib $libobjs $deplibs $compiler_flags'
	;;
    esac
    ;;
  sysv5* | sco3.2v5* | sco5v6*)
    # Note: We can NOT use -z defs as we might desire, because we do not
    # link with -lc, and that would cause any symbols used from libc to
    # always be unresolved, which means just about no library would
    # ever link correctly.  If we're not using GNU ld we use -z text
    # though, which does catch some bad symbols but isn't as heavy-handed
    # as -z defs.
    # For security reasons, it is highly recommended that you always
    # use absolute paths for naming shared libraries, and exclude the
    # DT_RUNPATH tag from executables and libraries.  But doing so
    # requires that you compile everything twice, which is a pain.
    # So that behaviour is only enabled if SCOABSPATH is set to a
    # non-empty value in the environment.  Most likely only useful for
    # creating official distributions of packages.
    # This is a hack until libtool officially supports absolute path
    # names for shared libraries.
    _LT_AC_TAGVAR(no_undefined_flag, $1)='${wl}-z,text'
    _LT_AC_TAGVAR(allow_undefined_flag, $1)='${wl}-z,nodefs'
    _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
    _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
    _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='`test -z "$SCOABSPATH" && echo ${wl}-R,$libdir`'
    _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=':'
    _LT_AC_TAGVAR(link_all_deplibs, $1)=yes
    _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-Bexport'
    runpath_var='LD_RUN_PATH'

    case $cc_basename in
      CC*)
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -G ${wl}-h,\${SCOABSPATH:+${install_libdir}/}$soname -o $lib $libobjs $deplibs $compiler_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -G ${wl}-Bexport:$export_symbols ${wl}-h,\${SCOABSPATH:+${install_libdir}/}$soname -o $lib $libobjs $deplibs $compiler_flags'
	;;
      *)
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared ${wl}-h,\${SCOABSPATH:+${install_libdir}/}$soname -o $lib $libobjs $deplibs $compiler_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared ${wl}-Bexport:$export_symbols ${wl}-h,\${SCOABSPATH:+${install_libdir}/}$soname -o $lib $libobjs $deplibs $compiler_flags'
	;;
    esac
    ;;
  tandem*)
    case $cc_basename in
      NCC*)
	# NonStop-UX NCC 3.20
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
      *)
	# FIXME: insert proper C++ library support
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	;;
    esac
    ;;
  vxworks*)
    # FIXME: insert proper C++ library support
    _LT_AC_TAGVAR(ld_shlibs, $1)=no
    ;;
  *)
    # FIXME: insert proper C++ library support
    _LT_AC_TAGVAR(ld_shlibs, $1)=no
    ;;
esac
AC_MSG_RESULT([$_LT_AC_TAGVAR(ld_shlibs, $1)])
test "$_LT_AC_TAGVAR(ld_shlibs, $1)" = no && can_build_shared=no

_LT_AC_TAGVAR(GCC, $1)="$GXX"
_LT_AC_TAGVAR(LD, $1)="$LD"

AC_LIBTOOL_POSTDEP_PREDEP($1)
AC_LIBTOOL_PROG_COMPILER_PIC($1)
AC_LIBTOOL_PROG_CC_C_O($1)
AC_LIBTOOL_SYS_HARD_LINK_LOCKS($1)
AC_LIBTOOL_PROG_LD_SHLIBS($1)
AC_LIBTOOL_SYS_DYNAMIC_LINKER($1)
AC_LIBTOOL_PROG_LD_HARDCODE_LIBPATH($1)

AC_LIBTOOL_CONFIG($1)

AC_LANG_POP
CC=$lt_save_CC
LDCXX=$LD
LD=$lt_save_LD
GCC=$lt_save_GCC
with_gnu_ldcxx=$with_gnu_ld
with_gnu_ld=$lt_save_with_gnu_ld
lt_cv_path_LDCXX=$lt_cv_path_LD
lt_cv_path_LD=$lt_save_path_LD
lt_cv_prog_gnu_ldcxx=$lt_cv_prog_gnu_ld
lt_cv_prog_gnu_ld=$lt_save_with_gnu_ld
])# AC_LIBTOOL_LANG_CXX_CONFIG

# AC_LIBTOOL_POSTDEP_PREDEP([TAGNAME])
# ------------------------------------
# Figure out "hidden" library dependencies from verbose
# compiler output when linking a shared library.
# Parse the compiler output and extract the necessary
# objects, libraries and library flags.
AC_DEFUN([AC_LIBTOOL_POSTDEP_PREDEP],
[AC_REQUIRE([LT_AC_PROG_SED])dnl
dnl we can't use the lt_simple_compile_test_code here,
dnl because it contains code intended for an executable,
dnl not a library.  It's possible we should let each
dnl tag define a new lt_????_link_test_code variable,
dnl but it's only used here...
ifelse([$1],[],[cat > conftest.$ac_ext <<EOF
int a;
void foo (void) { a = 0; }
EOF
],[$1],[CXX],[cat > conftest.$ac_ext <<EOF
class Foo
{
public:
  Foo (void) { a = 0; }
private:
  int a;
};
EOF
],[$1],[F77],[cat > conftest.$ac_ext <<EOF
      subroutine foo
      implicit none
      integer*4 a
      a=0
      return
      end
EOF
],[$1],[GCJ],[cat > conftest.$ac_ext <<EOF
public class foo {
  private int a;
  public void bar (void) {
    a = 0;
  }
};
EOF
])
dnl Parse the compiler output and extract the necessary
dnl objects, libraries and library flags.
if AC_TRY_EVAL(ac_compile); then
  # Parse the compiler output and extract the necessary
  # objects, libraries and library flags.

  # Sentinel used to keep track of whether or not we are before
  # the conftest object file.
  pre_test_object_deps_done=no

  # The `*' in the case matches for architectures that use `case' in
  # $output_verbose_cmd can trigger glob expansion during the loop
  # eval without this substitution.
  output_verbose_link_cmd=`$echo "X$output_verbose_link_cmd" | $Xsed -e "$no_glob_subst"`

  for p in `eval $output_verbose_link_cmd`; do
    case $p in

    -L* | -R* | -l*)
       # Some compilers place space between "-{L,R}" and the path.
       # Remove the space.
       if test $p = "-L" \
	  || test $p = "-R"; then
	 prev=$p
	 continue
       else
	 prev=
       fi

       if test "$pre_test_object_deps_done" = no; then
	 case $p in
	 -L* | -R*)
	   # Internal compiler library paths should come after those
	   # provided the user.  The postdeps already come after the
	   # user supplied libs so there is no need to process them.
	   if test -z "$_LT_AC_TAGVAR(compiler_lib_search_path, $1)"; then
	     _LT_AC_TAGVAR(compiler_lib_search_path, $1)="${prev}${p}"
	   else
	     _LT_AC_TAGVAR(compiler_lib_search_path, $1)="${_LT_AC_TAGVAR(compiler_lib_search_path, $1)} ${prev}${p}"
	   fi
	   ;;
	 # The "-l" case would never come before the object being
	 # linked, so don't bother handling this case.
	 esac
       else
	 if test -z "$_LT_AC_TAGVAR(postdeps, $1)"; then
	   _LT_AC_TAGVAR(postdeps, $1)="${prev}${p}"
	 else
	   _LT_AC_TAGVAR(postdeps, $1)="${_LT_AC_TAGVAR(postdeps, $1)} ${prev}${p}"
	 fi
       fi
       ;;

    *.$objext)
       # This assumes that the test object file only shows up
       # once in the compiler output.
       if test "$p" = "conftest.$objext"; then
	 pre_test_object_deps_done=yes
	 continue
       fi

       if test "$pre_test_object_deps_done" = no; then
	 if test -z "$_LT_AC_TAGVAR(predep_objects, $1)"; then
	   _LT_AC_TAGVAR(predep_objects, $1)="$p"
	 else
	   _LT_AC_TAGVAR(predep_objects, $1)="$_LT_AC_TAGVAR(predep_objects, $1) $p"
	 fi
       else
	 if test -z "$_LT_AC_TAGVAR(postdep_objects, $1)"; then
	   _LT_AC_TAGVAR(postdep_objects, $1)="$p"
	 else
	   _LT_AC_TAGVAR(postdep_objects, $1)="$_LT_AC_TAGVAR(postdep_objects, $1) $p"
	 fi
       fi
       ;;

    *) ;; # Ignore the rest.

    esac
  done

  # Clean up.
  rm -f a.out a.exe
else
  echo "libtool.m4: error: problem compiling $1 test program"
fi

$rm -f confest.$objext

_LT_AC_TAGVAR(compiler_lib_search_dirs, $1)=
if test -n "$_LT_AC_TAGVAR(compiler_lib_search_path, $1)"; then
  _LT_AC_TAGVAR(compiler_lib_search_dirs, $1)=`echo " ${_LT_AC_TAGVAR(compiler_lib_search_path, $1)}" | ${SED} -e 's! -L! !g' -e 's!^ !!'`
fi

# PORTME: override above test on systems where it is broken
ifelse([$1],[CXX],
[case $host_os in
interix[[3-9]]*)
  # Interix 3.5 installs completely hosed .la files for C++, so rather than
  # hack all around it, let's just trust "g++" to DTRT.
  _LT_AC_TAGVAR(predep_objects,$1)=
  _LT_AC_TAGVAR(postdep_objects,$1)=
  _LT_AC_TAGVAR(postdeps,$1)=
  ;;

linux*)
  case `$CC -V 2>&1 | sed 5q` in
  *Sun\ C*)
    # Sun C++ 5.9
    #
    # The more standards-conforming stlport4 library is
    # incompatible with the Cstd library. Avoid specifying
    # it if it's in CXXFLAGS. Ignore libCrun as
    # -library=stlport4 depends on it.
    case " $CXX $CXXFLAGS " in
    *" -library=stlport4 "*)
      solaris_use_stlport4=yes
      ;;
    esac
    if test "$solaris_use_stlport4" != yes; then
      _LT_AC_TAGVAR(postdeps,$1)='-library=Cstd -library=Crun'
    fi
    ;;
  esac
  ;;

solaris*)
  case $cc_basename in
  CC*)
    # The more standards-conforming stlport4 library is
    # incompatible with the Cstd library. Avoid specifying
    # it if it's in CXXFLAGS. Ignore libCrun as
    # -library=stlport4 depends on it.
    case " $CXX $CXXFLAGS " in
    *" -library=stlport4 "*)
      solaris_use_stlport4=yes
      ;;
    esac

    # Adding this requires a known-good setup of shared libraries for
    # Sun compiler versions before 5.6, else PIC objects from an old
    # archive will be linked into the output, leading to subtle bugs.
    if test "$solaris_use_stlport4" != yes; then
      _LT_AC_TAGVAR(postdeps,$1)='-library=Cstd -library=Crun'
    fi
    ;;
  esac
  ;;
esac
])
case " $_LT_AC_TAGVAR(postdeps, $1) " in
*" -lc "*) _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no ;;
esac
])# AC_LIBTOOL_POSTDEP_PREDEP

# AC_LIBTOOL_LANG_F77_CONFIG
# --------------------------
# Ensure that the configuration vars for the C compiler are
# suitably defined.  Those variables are subsequently used by
# AC_LIBTOOL_CONFIG to write the compiler configuration to `libtool'.
AC_DEFUN([AC_LIBTOOL_LANG_F77_CONFIG], [_LT_AC_LANG_F77_CONFIG(F77)])
AC_DEFUN([_LT_AC_LANG_F77_CONFIG],
[AC_REQUIRE([AC_PROG_F77])
AC_LANG_PUSH(Fortran 77)

_LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
_LT_AC_TAGVAR(allow_undefined_flag, $1)=
_LT_AC_TAGVAR(always_export_symbols, $1)=no
_LT_AC_TAGVAR(archive_expsym_cmds, $1)=
_LT_AC_TAGVAR(export_dynamic_flag_spec, $1)=
_LT_AC_TAGVAR(hardcode_direct, $1)=no
_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)=
_LT_AC_TAGVAR(hardcode_libdir_flag_spec_ld, $1)=
_LT_AC_TAGVAR(hardcode_libdir_separator, $1)=
_LT_AC_TAGVAR(hardcode_minus_L, $1)=no
_LT_AC_TAGVAR(hardcode_automatic, $1)=no
_LT_AC_TAGVAR(module_cmds, $1)=
_LT_AC_TAGVAR(module_expsym_cmds, $1)=
_LT_AC_TAGVAR(link_all_deplibs, $1)=unknown
_LT_AC_TAGVAR(old_archive_cmds, $1)=$old_archive_cmds
_LT_AC_TAGVAR(no_undefined_flag, $1)=
_LT_AC_TAGVAR(whole_archive_flag_spec, $1)=
_LT_AC_TAGVAR(enable_shared_with_static_runtimes, $1)=no

# Source file extension for f77 test sources.
ac_ext=f

# Object file extension for compiled f77 test sources.
objext=o
_LT_AC_TAGVAR(objext, $1)=$objext

# Code to be used in simple compile tests
lt_simple_compile_test_code="\
      subroutine t
      return
      end
"

# Code to be used in simple link tests
lt_simple_link_test_code="\
      program t
      end
"

# ltmain only uses $CC for tagged configurations so make sure $CC is set.
_LT_AC_SYS_COMPILER

# save warnings/boilerplate of simple test code
_LT_COMPILER_BOILERPLATE
_LT_LINKER_BOILERPLATE

# Allow CC to be a program name with arguments.
lt_save_CC="$CC"
CC=${F77-"f77"}
compiler=$CC
_LT_AC_TAGVAR(compiler, $1)=$CC
_LT_CC_BASENAME([$compiler])

AC_MSG_CHECKING([if libtool supports shared libraries])
AC_MSG_RESULT([$can_build_shared])

AC_MSG_CHECKING([whether to build shared libraries])
test "$can_build_shared" = "no" && enable_shared=no

# On AIX, shared libraries and static libraries use the same namespace, and
# are all built from PIC.
case $host_os in
aix3*)
  test "$enable_shared" = yes && enable_static=no
  if test -n "$RANLIB"; then
    archive_cmds="$archive_cmds~\$RANLIB \$lib"
    postinstall_cmds='$RANLIB $lib'
  fi
  ;;
aix[[4-9]]*)
  if test "$host_cpu" != ia64 && test "$aix_use_runtimelinking" = no ; then
    test "$enable_shared" = yes && enable_static=no
  fi
  ;;
esac
AC_MSG_RESULT([$enable_shared])

AC_MSG_CHECKING([whether to build static libraries])
# Make sure either enable_shared or enable_static is yes.
test "$enable_shared" = yes || enable_static=yes
AC_MSG_RESULT([$enable_static])

_LT_AC_TAGVAR(GCC, $1)="$G77"
_LT_AC_TAGVAR(LD, $1)="$LD"

AC_LIBTOOL_PROG_COMPILER_PIC($1)
AC_LIBTOOL_PROG_CC_C_O($1)
AC_LIBTOOL_SYS_HARD_LINK_LOCKS($1)
AC_LIBTOOL_PROG_LD_SHLIBS($1)
AC_LIBTOOL_SYS_DYNAMIC_LINKER($1)
AC_LIBTOOL_PROG_LD_HARDCODE_LIBPATH($1)

AC_LIBTOOL_CONFIG($1)

AC_LANG_POP
CC="$lt_save_CC"
])# AC_LIBTOOL_LANG_F77_CONFIG


# AC_LIBTOOL_LANG_GCJ_CONFIG
# --------------------------
# Ensure that the configuration vars for the C compiler are
# suitably defined.  Those variables are subsequently used by
# AC_LIBTOOL_CONFIG to write the compiler configuration to `libtool'.
AC_DEFUN([AC_LIBTOOL_LANG_GCJ_CONFIG], [_LT_AC_LANG_GCJ_CONFIG(GCJ)])
AC_DEFUN([_LT_AC_LANG_GCJ_CONFIG],
[AC_LANG_SAVE

# Source file extension for Java test sources.
ac_ext=java

# Object file extension for compiled Java test sources.
objext=o
_LT_AC_TAGVAR(objext, $1)=$objext

# Code to be used in simple compile tests
lt_simple_compile_test_code="class foo {}"

# Code to be used in simple link tests
lt_simple_link_test_code='public class conftest { public static void main(String[[]] argv) {}; }'

# ltmain only uses $CC for tagged configurations so make sure $CC is set.
_LT_AC_SYS_COMPILER

# save warnings/boilerplate of simple test code
_LT_COMPILER_BOILERPLATE
_LT_LINKER_BOILERPLATE

# Allow CC to be a program name with arguments.
lt_save_CC="$CC"
CC=${GCJ-"gcj"}
compiler=$CC
_LT_AC_TAGVAR(compiler, $1)=$CC
_LT_CC_BASENAME([$compiler])

# GCJ did not exist at the time GCC didn't implicitly link libc in.
_LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no

_LT_AC_TAGVAR(old_archive_cmds, $1)=$old_archive_cmds

AC_LIBTOOL_PROG_COMPILER_NO_RTTI($1)
AC_LIBTOOL_PROG_COMPILER_PIC($1)
AC_LIBTOOL_PROG_CC_C_O($1)
AC_LIBTOOL_SYS_HARD_LINK_LOCKS($1)
AC_LIBTOOL_PROG_LD_SHLIBS($1)
AC_LIBTOOL_SYS_DYNAMIC_LINKER($1)
AC_LIBTOOL_PROG_LD_HARDCODE_LIBPATH($1)

AC_LIBTOOL_CONFIG($1)

AC_LANG_RESTORE
CC="$lt_save_CC"
])# AC_LIBTOOL_LANG_GCJ_CONFIG


# AC_LIBTOOL_LANG_RC_CONFIG
# -------------------------
# Ensure that the configuration vars for the Windows resource compiler are
# suitably defined.  Those variables are subsequently used by
# AC_LIBTOOL_CONFIG to write the compiler configuration to `libtool'.
AC_DEFUN([AC_LIBTOOL_LANG_RC_CONFIG], [_LT_AC_LANG_RC_CONFIG(RC)])
AC_DEFUN([_LT_AC_LANG_RC_CONFIG],
[AC_LANG_SAVE

# Source file extension for RC test sources.
ac_ext=rc

# Object file extension for compiled RC test sources.
objext=o
_LT_AC_TAGVAR(objext, $1)=$objext

# Code to be used in simple compile tests
lt_simple_compile_test_code='sample MENU { MENUITEM "&Soup", 100, CHECKED }'

# Code to be used in simple link tests
lt_simple_link_test_code="$lt_simple_compile_test_code"

# ltmain only uses $CC for tagged configurations so make sure $CC is set.
_LT_AC_SYS_COMPILER

# save warnings/boilerplate of simple test code
_LT_COMPILER_BOILERPLATE
_LT_LINKER_BOILERPLATE

# Allow CC to be a program name with arguments.
lt_save_CC="$CC"
CC=${RC-"windres"}
compiler=$CC
_LT_AC_TAGVAR(compiler, $1)=$CC
_LT_CC_BASENAME([$compiler])
_LT_AC_TAGVAR(lt_cv_prog_compiler_c_o, $1)=yes

AC_LIBTOOL_CONFIG($1)

AC_LANG_RESTORE
CC="$lt_save_CC"
])# AC_LIBTOOL_LANG_RC_CONFIG


# AC_LIBTOOL_CONFIG([TAGNAME])
# ----------------------------
# If TAGNAME is not passed, then create an initial libtool script
# with a default configuration from the untagged config vars.  Otherwise
# add code to config.status for appending the configuration named by
# TAGNAME from the matching tagged config vars.
AC_DEFUN([AC_LIBTOOL_CONFIG],
[# The else clause should only fire when bootstrapping the
# libtool distribution, otherwise you forgot to ship ltmain.sh
# with your package, and you will get complaints that there are
# no rules to generate ltmain.sh.
if test -f "$ltmain"; then
  # See if we are running on zsh, and set the options which allow our commands through
  # without removal of \ escapes.
  if test -n "${ZSH_VERSION+set}" ; then
    setopt NO_GLOB_SUBST
  fi
  # Now quote all the things that may contain metacharacters while being
  # careful not to overquote the AC_SUBSTed values.  We take copies of the
  # variables and quote the copies for generation of the libtool script.
  for var in echo old_CC old_CFLAGS AR AR_FLAGS EGREP RANLIB LN_S LTCC LTCFLAGS NM \
    SED SHELL STRIP \
    libname_spec library_names_spec soname_spec extract_expsyms_cmds \
    old_striplib striplib file_magic_cmd finish_cmds finish_eval \
    deplibs_check_method reload_flag reload_cmds need_locks \
    lt_cv_sys_global_symbol_pipe lt_cv_sys_global_symbol_to_cdecl \
    lt_cv_sys_global_symbol_to_c_name_address \
    sys_lib_search_path_spec sys_lib_dlsearch_path_spec \
    old_postinstall_cmds old_postuninstall_cmds \
    _LT_AC_TAGVAR(compiler, $1) \
    _LT_AC_TAGVAR(CC, $1) \
    _LT_AC_TAGVAR(LD, $1) \
    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1) \
    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1) \
    _LT_AC_TAGVAR(lt_prog_compiler_static, $1) \
    _LT_AC_TAGVAR(lt_prog_compiler_no_builtin_flag, $1) \
    _LT_AC_TAGVAR(export_dynamic_flag_spec, $1) \
    _LT_AC_TAGVAR(thread_safe_flag_spec, $1) \
    _LT_AC_TAGVAR(whole_archive_flag_spec, $1) \
    _LT_AC_TAGVAR(enable_shared_with_static_runtimes, $1) \
    _LT_AC_TAGVAR(old_archive_cmds, $1) \
    _LT_AC_TAGVAR(old_archive_from_new_cmds, $1) \
    _LT_AC_TAGVAR(predep_objects, $1) \
    _LT_AC_TAGVAR(postdep_objects, $1) \
    _LT_AC_TAGVAR(predeps, $1) \
    _LT_AC_TAGVAR(postdeps, $1) \
    _LT_AC_TAGVAR(compiler_lib_search_path, $1) \
    _LT_AC_TAGVAR(compiler_lib_search_dirs, $1) \
    _LT_AC_TAGVAR(archive_cmds, $1) \
    _LT_AC_TAGVAR(archive_expsym_cmds, $1) \
    _LT_AC_TAGVAR(postinstall_cmds, $1) \
    _LT_AC_TAGVAR(postuninstall_cmds, $1) \
    _LT_AC_TAGVAR(old_archive_from_expsyms_cmds, $1) \
    _LT_AC_TAGVAR(allow_undefined_flag, $1) \
    _LT_AC_TAGVAR(no_undefined_flag, $1) \
    _LT_AC_TAGVAR(export_symbols_cmds, $1) \
    _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1) \
    _LT_AC_TAGVAR(hardcode_libdir_flag_spec_ld, $1) \
    _LT_AC_TAGVAR(hardcode_libdir_separator, $1) \
    _LT_AC_TAGVAR(hardcode_automatic, $1) \
    _LT_AC_TAGVAR(module_cmds, $1) \
    _LT_AC_TAGVAR(module_expsym_cmds, $1) \
    _LT_AC_TAGVAR(lt_cv_prog_compiler_c_o, $1) \
    _LT_AC_TAGVAR(fix_srcfile_path, $1) \
    _LT_AC_TAGVAR(exclude_expsyms, $1) \
    _LT_AC_TAGVAR(include_expsyms, $1); do

    case $var in
    _LT_AC_TAGVAR(old_archive_cmds, $1) | \
    _LT_AC_TAGVAR(old_archive_from_new_cmds, $1) | \
    _LT_AC_TAGVAR(archive_cmds, $1) | \
    _LT_AC_TAGVAR(archive_expsym_cmds, $1) | \
    _LT_AC_TAGVAR(module_cmds, $1) | \
    _LT_AC_TAGVAR(module_expsym_cmds, $1) | \
    _LT_AC_TAGVAR(old_archive_from_expsyms_cmds, $1) | \
    _LT_AC_TAGVAR(export_symbols_cmds, $1) | \
    extract_expsyms_cmds | reload_cmds | finish_cmds | \
    postinstall_cmds | postuninstall_cmds | \
    old_postinstall_cmds | old_postuninstall_cmds | \
    sys_lib_search_path_spec | sys_lib_dlsearch_path_spec)
      # Double-quote double-evaled strings.
      eval "lt_$var=\\\"\`\$echo \"X\$$var\" | \$Xsed -e \"\$double_quote_subst\" -e \"\$sed_quote_subst\" -e \"\$delay_variable_subst\"\`\\\""
      ;;
    *)
      eval "lt_$var=\\\"\`\$echo \"X\$$var\" | \$Xsed -e \"\$sed_quote_subst\"\`\\\""
      ;;
    esac
  done

  case $lt_echo in
  *'\[$]0 --fallback-echo"')
    lt_echo=`$echo "X$lt_echo" | $Xsed -e 's/\\\\\\\[$]0 --fallback-echo"[$]/[$]0 --fallback-echo"/'`
    ;;
  esac

ifelse([$1], [],
  [cfgfile="${ofile}T"
  trap "$rm \"$cfgfile\"; exit 1" 1 2 15
  $rm -f "$cfgfile"
  AC_MSG_NOTICE([creating $ofile])],
  [cfgfile="$ofile"])

  cat <<__EOF__ >> "$cfgfile"
ifelse([$1], [],
[#! $SHELL

# `$echo "$cfgfile" | sed 's%^.*/%%'` - Provide generalized library-building support services.
# Generated automatically by $PROGRAM (GNU $PACKAGE $VERSION$TIMESTAMP)
# NOTE: Changes made to this file will be lost: look at ltmain.sh.
#
# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
# Free Software Foundation, Inc.
#
# This file is part of GNU Libtool:
# Originally by Gordon Matzigkeit <gord@gnu.ai.mit.edu>, 1996
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# As a special exception to the GNU General Public License, if you
# distribute this file as part of a program that contains a
# configuration script generated by Autoconf, you may include it under
# the same distribution terms that you use for the rest of that program.

# A sed program that does not truncate output.
SED=$lt_SED

# Sed that helps us avoid accidentally triggering echo(1) options like -n.
Xsed="$SED -e 1s/^X//"

# The HP-UX ksh and POSIX shell print the target directory to stdout
# if CDPATH is set.
(unset CDPATH) >/dev/null 2>&1 && unset CDPATH

# The names of the tagged configurations supported by this script.
available_tags=

# ### BEGIN LIBTOOL CONFIG],
[# ### BEGIN LIBTOOL TAG CONFIG: $tagname])

# Libtool was configured on host `(hostname || uname -n) 2>/dev/null | sed 1q`:

# Shell to use when invoking shell scripts.
SHELL=$lt_SHELL

# Whether or not to build shared libraries.
build_libtool_libs=$enable_shared

# Whether or not to build static libraries.
build_old_libs=$enable_static

# Whether or not to add -lc for building shared libraries.
build_libtool_need_lc=$_LT_AC_TAGVAR(archive_cmds_need_lc, $1)

# Whether or not to disallow shared libs when runtime libs are static
allow_libtool_libs_with_static_runtimes=$_LT_AC_TAGVAR(enable_shared_with_static_runtimes, $1)

# Whether or not to optimize for fast installation.
fast_install=$enable_fast_install

# The host system.
host_alias=$host_alias
host=$host
host_os=$host_os

# The build system.
build_alias=$build_alias
build=$build
build_os=$build_os

# An echo program that does not interpret backslashes.
echo=$lt_echo

# The archiver.
AR=$lt_AR
AR_FLAGS=$lt_AR_FLAGS

# A C compiler.
LTCC=$lt_LTCC

# LTCC compiler flags.
LTCFLAGS=$lt_LTCFLAGS

# A language-specific compiler.
CC=$lt_[]_LT_AC_TAGVAR(compiler, $1)

# Is the compiler the GNU C compiler?
with_gcc=$_LT_AC_TAGVAR(GCC, $1)

# An ERE matcher.
EGREP=$lt_EGREP

# The linker used to build libraries.
LD=$lt_[]_LT_AC_TAGVAR(LD, $1)

# Whether we need hard or soft links.
LN_S=$lt_LN_S

# A BSD-compatible nm program.
NM=$lt_NM

# A symbol stripping program
STRIP=$lt_STRIP

# Used to examine libraries when file_magic_cmd begins "file"
MAGIC_CMD=$MAGIC_CMD

# Used on cygwin: DLL creation program.
DLLTOOL="$DLLTOOL"

# Used on cygwin: object dumper.
OBJDUMP="$OBJDUMP"

# Used on cygwin: assembler.
AS="$AS"

# The name of the directory that contains temporary libtool files.
objdir=$objdir

# How to create reloadable object files.
reload_flag=$lt_reload_flag
reload_cmds=$lt_reload_cmds

# How to pass a linker flag through the compiler.
wl=$lt_[]_LT_AC_TAGVAR(lt_prog_compiler_wl, $1)

# Object file suffix (normally "o").
objext="$ac_objext"

# Old archive suffix (normally "a").
libext="$libext"

# Shared library suffix (normally ".so").
shrext_cmds='$shrext_cmds'

# Executable file suffix (normally "").
exeext="$exeext"

# Additional compiler flags for building library objects.
pic_flag=$lt_[]_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)
pic_mode=$pic_mode

# What is the maximum length of a command?
max_cmd_len=$lt_cv_sys_max_cmd_len

# Does compiler simultaneously support -c and -o options?
compiler_c_o=$lt_[]_LT_AC_TAGVAR(lt_cv_prog_compiler_c_o, $1)

# Must we lock files when doing compilation?
need_locks=$lt_need_locks

# Do we need the lib prefix for modules?
need_lib_prefix=$need_lib_prefix

# Do we need a version for libraries?
need_version=$need_version

# Whether dlopen is supported.
dlopen_support=$enable_dlopen

# Whether dlopen of programs is supported.
dlopen_self=$enable_dlopen_self

# Whether dlopen of statically linked programs is supported.
dlopen_self_static=$enable_dlopen_self_static

# Compiler flag to prevent dynamic linking.
link_static_flag=$lt_[]_LT_AC_TAGVAR(lt_prog_compiler_static, $1)

# Compiler flag to turn off builtin functions.
no_builtin_flag=$lt_[]_LT_AC_TAGVAR(lt_prog_compiler_no_builtin_flag, $1)

# Compiler flag to allow reflexive dlopens.
export_dynamic_flag_spec=$lt_[]_LT_AC_TAGVAR(export_dynamic_flag_spec, $1)

# Compiler flag to generate shared objects directly from archives.
whole_archive_flag_spec=$lt_[]_LT_AC_TAGVAR(whole_archive_flag_spec, $1)

# Compiler flag to generate thread-safe objects.
thread_safe_flag_spec=$lt_[]_LT_AC_TAGVAR(thread_safe_flag_spec, $1)

# Library versioning type.
version_type=$version_type

# Format of library name prefix.
libname_spec=$lt_libname_spec

# List of archive names.  First name is the real one, the rest are links.
# The last name is the one that the linker finds with -lNAME.
library_names_spec=$lt_library_names_spec

# The coded name of the library, if different from the real name.
soname_spec=$lt_soname_spec

# Commands used to build and install an old-style archive.
RANLIB=$lt_RANLIB
old_archive_cmds=$lt_[]_LT_AC_TAGVAR(old_archive_cmds, $1)
old_postinstall_cmds=$lt_old_postinstall_cmds
old_postuninstall_cmds=$lt_old_postuninstall_cmds

# Create an old-style archive from a shared archive.
old_archive_from_new_cmds=$lt_[]_LT_AC_TAGVAR(old_archive_from_new_cmds, $1)

# Create a temporary old-style archive to link instead of a shared archive.
old_archive_from_expsyms_cmds=$lt_[]_LT_AC_TAGVAR(old_archive_from_expsyms_cmds, $1)

# Commands used to build and install a shared archive.
archive_cmds=$lt_[]_LT_AC_TAGVAR(archive_cmds, $1)
archive_expsym_cmds=$lt_[]_LT_AC_TAGVAR(archive_expsym_cmds, $1)
postinstall_cmds=$lt_postinstall_cmds
postuninstall_cmds=$lt_postuninstall_cmds

# Commands used to build a loadable module (assumed same as above if empty)
module_cmds=$lt_[]_LT_AC_TAGVAR(module_cmds, $1)
module_expsym_cmds=$lt_[]_LT_AC_TAGVAR(module_expsym_cmds, $1)

# Commands to strip libraries.
old_striplib=$lt_old_striplib
striplib=$lt_striplib

# Dependencies to place before the objects being linked to create a
# shared library.
predep_objects=$lt_[]_LT_AC_TAGVAR(predep_objects, $1)

# Dependencies to place after the objects being linked to create a
# shared library.
postdep_objects=$lt_[]_LT_AC_TAGVAR(postdep_objects, $1)

# Dependencies to place before the objects being linked to create a
# shared library.
predeps=$lt_[]_LT_AC_TAGVAR(predeps, $1)

# Dependencies to place after the objects being linked to create a
# shared library.
postdeps=$lt_[]_LT_AC_TAGVAR(postdeps, $1)

# The directories searched by this compiler when creating a shared
# library
compiler_lib_search_dirs=$lt_[]_LT_AC_TAGVAR(compiler_lib_search_dirs, $1)

# The library search path used internally by the compiler when linking
# a shared library.
compiler_lib_search_path=$lt_[]_LT_AC_TAGVAR(compiler_lib_search_path, $1)

# Method to check whether dependent libraries are shared objects.
deplibs_check_method=$lt_deplibs_check_method

# Command to use when deplibs_check_method == file_magic.
file_magic_cmd=$lt_file_magic_cmd

# Flag that allows shared libraries with undefined symbols to be built.
allow_undefined_flag=$lt_[]_LT_AC_TAGVAR(allow_undefined_flag, $1)

# Flag that forces no undefined symbols.
no_undefined_flag=$lt_[]_LT_AC_TAGVAR(no_undefined_flag, $1)

# Commands used to finish a libtool library installation in a directory.
finish_cmds=$lt_finish_cmds

# Same as above, but a single script fragment to be evaled but not shown.
finish_eval=$lt_finish_eval

# Take the output of nm and produce a listing of raw symbols and C names.
global_symbol_pipe=$lt_lt_cv_sys_global_symbol_pipe

# Transform the output of nm in a proper C declaration
global_symbol_to_cdecl=$lt_lt_cv_sys_global_symbol_to_cdecl

# Transform the output of nm in a C name address pair
global_symbol_to_c_name_address=$lt_lt_cv_sys_global_symbol_to_c_name_address

# This is the shared library runtime path variable.
runpath_var=$runpath_var

# This is the shared library path variable.
shlibpath_var=$shlibpath_var

# Is shlibpath searched before the hard-coded library search path?
shlibpath_overrides_runpath=$shlibpath_overrides_runpath

# How to hardcode a shared library path into an executable.
hardcode_action=$_LT_AC_TAGVAR(hardcode_action, $1)

# Whether we should hardcode library paths into libraries.
hardcode_into_libs=$hardcode_into_libs

# Flag to hardcode \$libdir into a binary during linking.
# This must work even if \$libdir does not exist.
hardcode_libdir_flag_spec=$lt_[]_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)

# If ld is used when linking, flag to hardcode \$libdir into
# a binary during linking. This must work even if \$libdir does
# not exist.
hardcode_libdir_flag_spec_ld=$lt_[]_LT_AC_TAGVAR(hardcode_libdir_flag_spec_ld, $1)

# Whether we need a single -rpath flag with a separated argument.
hardcode_libdir_separator=$lt_[]_LT_AC_TAGVAR(hardcode_libdir_separator, $1)

# Set to yes if using DIR/libNAME${shared_ext} during linking hardcodes DIR into the
# resulting binary.
hardcode_direct=$_LT_AC_TAGVAR(hardcode_direct, $1)

# Set to yes if using the -LDIR flag during linking hardcodes DIR into the
# resulting binary.
hardcode_minus_L=$_LT_AC_TAGVAR(hardcode_minus_L, $1)

# Set to yes if using SHLIBPATH_VAR=DIR during linking hardcodes DIR into
# the resulting binary.
hardcode_shlibpath_var=$_LT_AC_TAGVAR(hardcode_shlibpath_var, $1)

# Set to yes if building a shared library automatically hardcodes DIR into the library
# and all subsequent libraries and executables linked against it.
hardcode_automatic=$_LT_AC_TAGVAR(hardcode_automatic, $1)

# Variables whose values should be saved in libtool wrapper scripts and
# restored at relink time.
variables_saved_for_relink="$variables_saved_for_relink"

# Whether libtool must link a program against all its dependency libraries.
link_all_deplibs=$_LT_AC_TAGVAR(link_all_deplibs, $1)

# Compile-time system search path for libraries
sys_lib_search_path_spec=$lt_sys_lib_search_path_spec

# Run-time system search path for libraries
sys_lib_dlsearch_path_spec=$lt_sys_lib_dlsearch_path_spec

# Fix the shell variable \$srcfile for the compiler.
fix_srcfile_path=$lt_fix_srcfile_path

# Set to yes if exported symbols are required.
always_export_symbols=$_LT_AC_TAGVAR(always_export_symbols, $1)

# The commands to list exported symbols.
export_symbols_cmds=$lt_[]_LT_AC_TAGVAR(export_symbols_cmds, $1)

# The commands to extract the exported symbol list from a shared archive.
extract_expsyms_cmds=$lt_extract_expsyms_cmds

# Symbols that should not be listed in the preloaded symbols.
exclude_expsyms=$lt_[]_LT_AC_TAGVAR(exclude_expsyms, $1)

# Symbols that must always be exported.
include_expsyms=$lt_[]_LT_AC_TAGVAR(include_expsyms, $1)

ifelse([$1],[],
[# ### END LIBTOOL CONFIG],
[# ### END LIBTOOL TAG CONFIG: $tagname])

__EOF__

ifelse([$1],[], [
  case $host_os in
  aix3*)
    cat <<\EOF >> "$cfgfile"

# AIX sometimes has problems with the GCC collect2 program.  For some
# reason, if we set the COLLECT_NAMES environment variable, the problems
# vanish in a puff of smoke.
if test "X${COLLECT_NAMES+set}" != Xset; then
  COLLECT_NAMES=
  export COLLECT_NAMES
fi
EOF
    ;;
  esac

  # We use sed instead of cat because bash on DJGPP gets confused if
  # if finds mixed CR/LF and LF-only lines.  Since sed operates in
  # text mode, it properly converts lines to CR/LF.  This bash problem
  # is reportedly fixed, but why not run on old versions too?
  sed '$q' "$ltmain" >> "$cfgfile" || (rm -f "$cfgfile"; exit 1)

  mv -f "$cfgfile" "$ofile" || \
    (rm -f "$ofile" && cp "$cfgfile" "$ofile" && rm -f "$cfgfile")
  chmod +x "$ofile"
])
else
  # If there is no Makefile yet, we rely on a make rule to execute
  # `config.status --recheck' to rerun these tests and create the
  # libtool script then.
  ltmain_in=`echo $ltmain | sed -e 's/\.sh$/.in/'`
  if test -f "$ltmain_in"; then
    test -f Makefile && make "$ltmain"
  fi
fi
])# AC_LIBTOOL_CONFIG


# AC_LIBTOOL_PROG_COMPILER_NO_RTTI([TAGNAME])
# -------------------------------------------
AC_DEFUN([AC_LIBTOOL_PROG_COMPILER_NO_RTTI],
[AC_REQUIRE([_LT_AC_SYS_COMPILER])dnl

_LT_AC_TAGVAR(lt_prog_compiler_no_builtin_flag, $1)=

if test "$GCC" = yes; then
  _LT_AC_TAGVAR(lt_prog_compiler_no_builtin_flag, $1)=' -fno-builtin'

  AC_LIBTOOL_COMPILER_OPTION([if $compiler supports -fno-rtti -fno-exceptions],
    lt_cv_prog_compiler_rtti_exceptions,
    [-fno-rtti -fno-exceptions], [],
    [_LT_AC_TAGVAR(lt_prog_compiler_no_builtin_flag, $1)="$_LT_AC_TAGVAR(lt_prog_compiler_no_builtin_flag, $1) -fno-rtti -fno-exceptions"])
fi
])# AC_LIBTOOL_PROG_COMPILER_NO_RTTI


# AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE
# ---------------------------------
AC_DEFUN([AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE],
[AC_REQUIRE([AC_CANONICAL_HOST])
AC_REQUIRE([LT_AC_PROG_SED])
AC_REQUIRE([AC_PROG_NM])
AC_REQUIRE([AC_OBJEXT])
# Check for command to grab the raw symbol name followed by C symbol from nm.
AC_MSG_CHECKING([command to parse $NM output from $compiler object])
AC_CACHE_VAL([lt_cv_sys_global_symbol_pipe],
[
# These are sane defaults that work on at least a few old systems.
# [They come from Ultrix.  What could be older than Ultrix?!! ;)]

# Character class describing NM global symbol codes.
symcode='[[BCDEGRST]]'

# Regexp to match symbols that can be accessed directly from C.
sympat='\([[_A-Za-z]][[_A-Za-z0-9]]*\)'

# Transform an extracted symbol line into a proper C declaration
lt_cv_sys_global_symbol_to_cdecl="sed -n -e 's/^. .* \(.*\)$/extern int \1;/p'"

# Transform an extracted symbol line into symbol name and symbol address
lt_cv_sys_global_symbol_to_c_name_address="sed -n -e 's/^: \([[^ ]]*\) $/  {\\\"\1\\\", (lt_ptr) 0},/p' -e 's/^$symcode \([[^ ]]*\) \([[^ ]]*\)$/  {\"\2\", (lt_ptr) \&\2},/p'"

# Define system-specific variables.
case $host_os in
aix*)
  symcode='[[BCDT]]'
  ;;
cygwin* | mingw* | pw32*)
  symcode='[[ABCDGISTW]]'
  ;;
hpux*) # Its linker distinguishes data from code symbols
  if test "$host_cpu" = ia64; then
    symcode='[[ABCDEGRST]]'
  fi
  lt_cv_sys_global_symbol_to_cdecl="sed -n -e 's/^T .* \(.*\)$/extern int \1();/p' -e 's/^$symcode* .* \(.*\)$/extern char \1;/p'"
  lt_cv_sys_global_symbol_to_c_name_address="sed -n -e 's/^: \([[^ ]]*\) $/  {\\\"\1\\\", (lt_ptr) 0},/p' -e 's/^$symcode* \([[^ ]]*\) \([[^ ]]*\)$/  {\"\2\", (lt_ptr) \&\2},/p'"
  ;;
linux* | k*bsd*-gnu)
  if test "$host_cpu" = ia64; then
    symcode='[[ABCDGIRSTW]]'
    lt_cv_sys_global_symbol_to_cdecl="sed -n -e 's/^T .* \(.*\)$/extern int \1();/p' -e 's/^$symcode* .* \(.*\)$/extern char \1;/p'"
    lt_cv_sys_global_symbol_to_c_name_address="sed -n -e 's/^: \([[^ ]]*\) $/  {\\\"\1\\\", (lt_ptr) 0},/p' -e 's/^$symcode* \([[^ ]]*\) \([[^ ]]*\)$/  {\"\2\", (lt_ptr) \&\2},/p'"
  fi
  ;;
irix* | nonstopux*)
  symcode='[[BCDEGRST]]'
  ;;
osf*)
  symcode='[[BCDEGQRST]]'
  ;;
solaris*)
  symcode='[[BDRT]]'
  ;;
sco3.2v5*)
  symcode='[[DT]]'
  ;;
sysv4.2uw2*)
  symcode='[[DT]]'
  ;;
sysv5* | sco5v6* | unixware* | OpenUNIX*)
  symcode='[[ABDT]]'
  ;;
sysv4)
  symcode='[[DFNSTU]]'
  ;;
esac

# Handle CRLF in mingw tool chain
opt_cr=
case $build_os in
mingw*)
  opt_cr=`echo 'x\{0,1\}' | tr x '\015'` # option cr in regexp
  ;;
esac

# If we're using GNU nm, then use its standard symbol codes.
case `$NM -V 2>&1` in
*GNU* | *'with BFD'*)
  symcode='[[ABCDGIRSTW]]' ;;
esac

# Try without a prefix undercore, then with it.
for ac_symprfx in "" "_"; do

  # Transform symcode, sympat, and symprfx into a raw symbol and a C symbol.
  symxfrm="\\1 $ac_symprfx\\2 \\2"

  # Write the raw and C identifiers.
  lt_cv_sys_global_symbol_pipe="sed -n -e 's/^.*[[ 	]]\($symcode$symcode*\)[[ 	]][[ 	]]*$ac_symprfx$sympat$opt_cr$/$symxfrm/p'"

  # Check to see that the pipe works correctly.
  pipe_works=no

  rm -f conftest*
  cat > conftest.$ac_ext <<EOF
#ifdef __cplusplus
extern "C" {
#endif
char nm_test_var;
void nm_test_func(){}
#ifdef __cplusplus
}
#endif
int main(){nm_test_var='a';nm_test_func();return(0);}
EOF

  if AC_TRY_EVAL(ac_compile); then
    # Now try to grab the symbols.
    nlist=conftest.nm
    if AC_TRY_EVAL(NM conftest.$ac_objext \| $lt_cv_sys_global_symbol_pipe \> $nlist) && test -s "$nlist"; then
      # Try sorting and uniquifying the output.
      if sort "$nlist" | uniq > "$nlist"T; then
	mv -f "$nlist"T "$nlist"
      else
	rm -f "$nlist"T
      fi

      # Make sure that we snagged all the symbols we need.
      if grep ' nm_test_var$' "$nlist" >/dev/null; then
	if grep ' nm_test_func$' "$nlist" >/dev/null; then
	  cat <<EOF > conftest.$ac_ext
#ifdef __cplusplus
extern "C" {
#endif

EOF
	  # Now generate the symbol file.
	  eval "$lt_cv_sys_global_symbol_to_cdecl"' < "$nlist" | grep -v main >> conftest.$ac_ext'

	  cat <<EOF >> conftest.$ac_ext
#if defined (__STDC__) && __STDC__
# define lt_ptr_t void *
#else
# define lt_ptr_t char *
# define const
#endif

/* The mapping between symbol names and symbols. */
const struct {
  const char *name;
  lt_ptr_t address;
}
lt_preloaded_symbols[[]] =
{
EOF
	  $SED "s/^$symcode$symcode* \(.*\) \(.*\)$/  {\"\2\", (lt_ptr_t) \&\2},/" < "$nlist" | grep -v main >> conftest.$ac_ext
	  cat <<\EOF >> conftest.$ac_ext
  {0, (lt_ptr_t) 0}
};

#ifdef __cplusplus
}
#endif
EOF
	  # Now try linking the two files.
	  mv conftest.$ac_objext conftstm.$ac_objext
	  lt_save_LIBS="$LIBS"
	  lt_save_CFLAGS="$CFLAGS"
	  LIBS="conftstm.$ac_objext"
	  CFLAGS="$CFLAGS$_LT_AC_TAGVAR(lt_prog_compiler_no_builtin_flag, $1)"
	  if AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext}; then
	    pipe_works=yes
	  fi
	  LIBS="$lt_save_LIBS"
	  CFLAGS="$lt_save_CFLAGS"
	else
	  echo "cannot find nm_test_func in $nlist" >&AS_MESSAGE_LOG_FD
	fi
      else
	echo "cannot find nm_test_var in $nlist" >&AS_MESSAGE_LOG_FD
      fi
    else
      echo "cannot run $lt_cv_sys_global_symbol_pipe" >&AS_MESSAGE_LOG_FD
    fi
  else
    echo "$progname: failed program was:" >&AS_MESSAGE_LOG_FD
    cat conftest.$ac_ext >&5
  fi
  rm -rf conftest* conftst*

  # Do not use the global_symbol_pipe unless it works.
  if test "$pipe_works" = yes; then
    break
  else
    lt_cv_sys_global_symbol_pipe=
  fi
done
])
if test -z "$lt_cv_sys_global_symbol_pipe"; then
  lt_cv_sys_global_symbol_to_cdecl=
fi
if test -z "$lt_cv_sys_global_symbol_pipe$lt_cv_sys_global_symbol_to_cdecl"; then
  AC_MSG_RESULT(failed)
else
  AC_MSG_RESULT(ok)
fi
]) # AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE


# AC_LIBTOOL_PROG_COMPILER_PIC([TAGNAME])
# ---------------------------------------
AC_DEFUN([AC_LIBTOOL_PROG_COMPILER_PIC],
[_LT_AC_TAGVAR(lt_prog_compiler_wl, $1)=
_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)=
_LT_AC_TAGVAR(lt_prog_compiler_static, $1)=

AC_MSG_CHECKING([for $compiler option to produce PIC])
 ifelse([$1],[CXX],[
  # C++ specific cases for pic, static, wl, etc.
  if test "$GXX" = yes; then
    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-static'

    case $host_os in
    aix*)
      # All AIX code is PIC.
      if test "$host_cpu" = ia64; then
	# AIX 5 now supports IA64 processor
	_LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
      fi
      ;;
    amigaos*)
      # FIXME: we need at least 68020 code to build shared libraries, but
      # adding the `-m68020' flag to GCC prevents building anything better,
      # like `-m68040'.
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-m68020 -resident32 -malways-restore-a4'
      ;;
    beos* | irix5* | irix6* | nonstopux* | osf3* | osf4* | osf5*)
      # PIC is the default for these OSes.
      ;;
    mingw* | cygwin* | os2* | pw32*)
      # This hack is so that the source file can tell whether it is being
      # built for inclusion in a dll (and should export symbols for example).
      # Although the cygwin gcc ignores -fPIC, still need this for old-style
      # (--disable-auto-import) libraries
      m4_if([$1], [GCJ], [],
	[_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-DDLL_EXPORT'])
      ;;
    darwin* | rhapsody*)
      # PIC is the default on this platform
      # Common symbols not allowed in MH_DYLIB files
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-fno-common'
      ;;
    *djgpp*)
      # DJGPP does not support shared libraries at all
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)=
      ;;
    interix[[3-9]]*)
      # Interix 3.x gcc -fpic/-fPIC options generate broken code.
      # Instead, we relocate shared libraries at runtime.
      ;;
    sysv4*MP*)
      if test -d /usr/nec; then
	_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)=-Kconform_pic
      fi
      ;;
    hpux*)
      # PIC is the default for IA64 HP-UX and 64-bit HP-UX, but
      # not for PA HP-UX.
      case $host_cpu in
      hppa*64*|ia64*)
	;;
      *)
	_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-fPIC'
	;;
      esac
      ;;
    *)
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-fPIC'
      ;;
    esac
  else
    case $host_os in
      aix[[4-9]]*)
	# All AIX code is PIC.
	if test "$host_cpu" = ia64; then
	  # AIX 5 now supports IA64 processor
	  _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
	else
	  _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-bnso -bI:/lib/syscalls.exp'
	fi
	;;
      chorus*)
	case $cc_basename in
	cxch68*)
	  # Green Hills C++ Compiler
	  # _LT_AC_TAGVAR(lt_prog_compiler_static, $1)="--no_auto_instantiation -u __main -u __premain -u _abort -r $COOL_DIR/lib/libOrb.a $MVME_DIR/lib/CC/libC.a $MVME_DIR/lib/classix/libcx.s.a"
	  ;;
	esac
	;;
       darwin*)
         # PIC is the default on this platform
         # Common symbols not allowed in MH_DYLIB files
         case $cc_basename in
           xlc*)
           _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-qnocommon'
           _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
           ;;
         esac
       ;;
      dgux*)
	case $cc_basename in
	  ec++*)
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
	    ;;
	  ghcx*)
	    # Green Hills C++ Compiler
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-pic'
	    ;;
	  *)
	    ;;
	esac
	;;
      freebsd* | dragonfly*)
	# FreeBSD uses GNU C++
	;;
      hpux9* | hpux10* | hpux11*)
	case $cc_basename in
	  CC*)
	    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
	    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='${wl}-a ${wl}archive'
	    if test "$host_cpu" != ia64; then
	      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='+Z'
	    fi
	    ;;
	  aCC*)
	    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
	    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='${wl}-a ${wl}archive'
	    case $host_cpu in
	    hppa*64*|ia64*)
	      # +Z the default
	      ;;
	    *)
	      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='+Z'
	      ;;
	    esac
	    ;;
	  *)
	    ;;
	esac
	;;
      interix*)
	# This is c89, which is MS Visual C++ (no shared libs)
	# Anyone wants to do a port?
	;;
      irix5* | irix6* | nonstopux*)
	case $cc_basename in
	  CC*)
	    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
	    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-non_shared'
	    # CC pic flag -KPIC is the default.
	    ;;
	  *)
	    ;;
	esac
	;;
      linux* | k*bsd*-gnu)
	case $cc_basename in
	  KCC*)
	    # KAI C++ Compiler
	    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='--backend -Wl,'
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-fPIC'
	    ;;
	  icpc* | ecpc*)
	    # Intel C++
	    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
	    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-static'
	    ;;
	  pgCC* | pgcpp*)
	    # Portland Group C++ compiler.
	    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-fpic'
	    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
	    ;;
	  cxx*)
	    # Compaq C++
	    # Make sure the PIC flag is empty.  It appears that all Alpha
	    # Linux and Compaq Tru64 Unix objects are PIC.
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)=
	    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-non_shared'
	    ;;
	  *)
	    case `$CC -V 2>&1 | sed 5q` in
	    *Sun\ C*)
	      # Sun C++ 5.9
	      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
	      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
	      _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Qoption ld '
	      ;;
	    esac
	    ;;
	esac
	;;
      lynxos*)
	;;
      m88k*)
	;;
      mvs*)
	case $cc_basename in
	  cxx*)
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-W c,exportall'
	    ;;
	  *)
	    ;;
	esac
	;;
      netbsd*)
	;;
      osf3* | osf4* | osf5*)
	case $cc_basename in
	  KCC*)
	    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='--backend -Wl,'
	    ;;
	  RCC*)
	    # Rational C++ 2.4.1
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-pic'
	    ;;
	  cxx*)
	    # Digital/Compaq C++
	    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
	    # Make sure the PIC flag is empty.  It appears that all Alpha
	    # Linux and Compaq Tru64 Unix objects are PIC.
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)=
	    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-non_shared'
	    ;;
	  *)
	    ;;
	esac
	;;
      psos*)
	;;
      solaris*)
	case $cc_basename in
	  CC*)
	    # Sun C++ 4.2, 5.x and Centerline C++
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
	    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
	    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Qoption ld '
	    ;;
	  gcx*)
	    # Green Hills C++ Compiler
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-PIC'
	    ;;
	  *)
	    ;;
	esac
	;;
      sunos4*)
	case $cc_basename in
	  CC*)
	    # Sun C++ 4.x
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-pic'
	    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
	    ;;
	  lcc*)
	    # Lucid
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-pic'
	    ;;
	  *)
	    ;;
	esac
	;;
      tandem*)
	case $cc_basename in
	  NCC*)
	    # NonStop-UX NCC 3.20
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
	    ;;
	  *)
	    ;;
	esac
	;;
      sysv5* | unixware* | sco3.2v5* | sco5v6* | OpenUNIX*)
	case $cc_basename in
	  CC*)
	    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
	    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
	    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
	    ;;
	esac
	;;
      vxworks*)
	;;
      *)
	_LT_AC_TAGVAR(lt_prog_compiler_can_build_shared, $1)=no
	;;
    esac
  fi
],
[
  if test "$GCC" = yes; then
    _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
    _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-static'

    case $host_os in
      aix*)
      # All AIX code is PIC.
      if test "$host_cpu" = ia64; then
	# AIX 5 now supports IA64 processor
	_LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
      fi
      ;;

    amigaos*)
      # FIXME: we need at least 68020 code to build shared libraries, but
      # adding the `-m68020' flag to GCC prevents building anything better,
      # like `-m68040'.
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-m68020 -resident32 -malways-restore-a4'
      ;;

    beos* | irix5* | irix6* | nonstopux* | osf3* | osf4* | osf5*)
      # PIC is the default for these OSes.
      ;;

    mingw* | cygwin* | pw32* | os2*)
      # This hack is so that the source file can tell whether it is being
      # built for inclusion in a dll (and should export symbols for example).
      # Although the cygwin gcc ignores -fPIC, still need this for old-style
      # (--disable-auto-import) libraries
      m4_if([$1], [GCJ], [],
	[_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-DDLL_EXPORT'])
      ;;

    darwin* | rhapsody*)
      # PIC is the default on this platform
      # Common symbols not allowed in MH_DYLIB files
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-fno-common'
      ;;

    interix[[3-9]]*)
      # Interix 3.x gcc -fpic/-fPIC options generate broken code.
      # Instead, we relocate shared libraries at runtime.
      ;;

    msdosdjgpp*)
      # Just because we use GCC doesn't mean we suddenly get shared libraries
      # on systems that don't support them.
      _LT_AC_TAGVAR(lt_prog_compiler_can_build_shared, $1)=no
      enable_shared=no
      ;;

    sysv4*MP*)
      if test -d /usr/nec; then
	_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)=-Kconform_pic
      fi
      ;;

    hpux*)
      # PIC is the default for IA64 HP-UX and 64-bit HP-UX, but
      # not for PA HP-UX.
      case $host_cpu in
      hppa*64*|ia64*)
	# +Z the default
	;;
      *)
	_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-fPIC'
	;;
      esac
      ;;

    *)
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-fPIC'
      ;;
    esac
  else
    # PORTME Check for flag to pass linker flags through the system compiler.
    case $host_os in
    aix*)
      _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
      if test "$host_cpu" = ia64; then
	# AIX 5 now supports IA64 processor
	_LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
      else
	_LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-bnso -bI:/lib/syscalls.exp'
      fi
      ;;
      darwin*)
        # PIC is the default on this platform
        # Common symbols not allowed in MH_DYLIB files
       case $cc_basename in
         xlc*)
         _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-qnocommon'
         _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
         ;;
       esac
       ;;

    mingw* | cygwin* | pw32* | os2*)
      # This hack is so that the source file can tell whether it is being
      # built for inclusion in a dll (and should export symbols for example).
      m4_if([$1], [GCJ], [],
	[_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-DDLL_EXPORT'])
      ;;

    hpux9* | hpux10* | hpux11*)
      _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
      # PIC is the default for IA64 HP-UX and 64-bit HP-UX, but
      # not for PA HP-UX.
      case $host_cpu in
      hppa*64*|ia64*)
	# +Z the default
	;;
      *)
	_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='+Z'
	;;
      esac
      # Is there a better lt_prog_compiler_static that works with the bundled CC?
      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='${wl}-a ${wl}archive'
      ;;

    irix5* | irix6* | nonstopux*)
      _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
      # PIC (with -KPIC) is the default.
      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-non_shared'
      ;;

    newsos6)
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
      ;;

    linux* | k*bsd*-gnu)
      case $cc_basename in
      icc* | ecc*)
	_LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
	_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
	_LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-static'
        ;;
      pgcc* | pgf77* | pgf90* | pgf95*)
        # Portland Group compilers (*not* the Pentium gcc compiler,
	# which looks to be a dead project)
	_LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
	_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-fpic'
	_LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
        ;;
      ccc*)
        _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
        # All Alpha code is PIC.
        _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-non_shared'
        ;;
      *)
        case `$CC -V 2>&1 | sed 5q` in
	*Sun\ C*)
	  # Sun C 5.9
	  _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
	  _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
	  _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
	  ;;
	*Sun\ F*)
	  # Sun Fortran 8.3 passes all unrecognized flags to the linker
	  _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
	  _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
	  _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)=''
	  ;;
	esac
	;;
      esac
      ;;

    osf3* | osf4* | osf5*)
      _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
      # All OSF/1 code is PIC.
      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-non_shared'
      ;;

    rdos*)
      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-non_shared'
      ;;

    solaris*)
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
      case $cc_basename in
      f77* | f90* | f95*)
	_LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Qoption ld ';;
      *)
	_LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,';;
      esac
      ;;

    sunos4*)
      _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Qoption ld '
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-PIC'
      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
      ;;

    sysv4 | sysv4.2uw2* | sysv4.3*)
      _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
      ;;

    sysv4*MP*)
      if test -d /usr/nec ;then
	_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-Kconform_pic'
	_LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
      fi
      ;;

    sysv5* | unixware* | sco3.2v5* | sco5v6* | OpenUNIX*)
      _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-KPIC'
      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
      ;;

    unicos*)
      _LT_AC_TAGVAR(lt_prog_compiler_wl, $1)='-Wl,'
      _LT_AC_TAGVAR(lt_prog_compiler_can_build_shared, $1)=no
      ;;

    uts4*)
      _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)='-pic'
      _LT_AC_TAGVAR(lt_prog_compiler_static, $1)='-Bstatic'
      ;;

    *)
      _LT_AC_TAGVAR(lt_prog_compiler_can_build_shared, $1)=no
      ;;
    esac
  fi
])
AC_MSG_RESULT([$_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)])

#
# Check to make sure the PIC flag actually works.
#
if test -n "$_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)"; then
  AC_LIBTOOL_COMPILER_OPTION([if $compiler PIC flag $_LT_AC_TAGVAR(lt_prog_compiler_pic, $1) works],
    _LT_AC_TAGVAR(lt_cv_prog_compiler_pic_works, $1),
    [$_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)ifelse([$1],[],[ -DPIC],[ifelse([$1],[CXX],[ -DPIC],[])])], [],
    [case $_LT_AC_TAGVAR(lt_prog_compiler_pic, $1) in
     "" | " "*) ;;
     *) _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)=" $_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)" ;;
     esac],
    [_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)=
     _LT_AC_TAGVAR(lt_prog_compiler_can_build_shared, $1)=no])
fi
case $host_os in
  # For platforms which do not support PIC, -DPIC is meaningless:
  *djgpp*)
    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)=
    ;;
  *)
    _LT_AC_TAGVAR(lt_prog_compiler_pic, $1)="$_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)ifelse([$1],[],[ -DPIC],[ifelse([$1],[CXX],[ -DPIC],[])])"
    ;;
esac

#
# Check to make sure the static flag actually works.
#
wl=$_LT_AC_TAGVAR(lt_prog_compiler_wl, $1) eval lt_tmp_static_flag=\"$_LT_AC_TAGVAR(lt_prog_compiler_static, $1)\"
AC_LIBTOOL_LINKER_OPTION([if $compiler static flag $lt_tmp_static_flag works],
  _LT_AC_TAGVAR(lt_cv_prog_compiler_static_works, $1),
  $lt_tmp_static_flag,
  [],
  [_LT_AC_TAGVAR(lt_prog_compiler_static, $1)=])
])


# AC_LIBTOOL_PROG_LD_SHLIBS([TAGNAME])
# ------------------------------------
# See if the linker supports building shared libraries.
AC_DEFUN([AC_LIBTOOL_PROG_LD_SHLIBS],
[AC_REQUIRE([LT_AC_PROG_SED])dnl
AC_MSG_CHECKING([whether the $compiler linker ($LD) supports shared libraries])
ifelse([$1],[CXX],[
  _LT_AC_TAGVAR(export_symbols_cmds, $1)='$NM $libobjs $convenience | $global_symbol_pipe | $SED '\''s/.* //'\'' | sort | uniq > $export_symbols'
  case $host_os in
  aix[[4-9]]*)
    # If we're using GNU nm, then we don't want the "-C" option.
    # -C means demangle to AIX nm, but means don't demangle with GNU nm
    if $NM -V 2>&1 | grep 'GNU' > /dev/null; then
      _LT_AC_TAGVAR(export_symbols_cmds, $1)='$NM -Bpg $libobjs $convenience | awk '\''{ if (((\[$]2 == "T") || (\[$]2 == "D") || (\[$]2 == "B")) && ([substr](\[$]3,1,1) != ".")) { print \[$]3 } }'\'' | sort -u > $export_symbols'
    else
      _LT_AC_TAGVAR(export_symbols_cmds, $1)='$NM -BCpg $libobjs $convenience | awk '\''{ if (((\[$]2 == "T") || (\[$]2 == "D") || (\[$]2 == "B")) && ([substr](\[$]3,1,1) != ".")) { print \[$]3 } }'\'' | sort -u > $export_symbols'
    fi
    ;;
  pw32*)
    _LT_AC_TAGVAR(export_symbols_cmds, $1)="$ltdll_cmds"
  ;;
  cygwin* | mingw*)
    _LT_AC_TAGVAR(export_symbols_cmds, $1)='$NM $libobjs $convenience | $global_symbol_pipe | $SED -e '\''/^[[BCDGRS]][[ ]]/s/.*[[ ]]\([[^ ]]*\)/\1 DATA/;/^.*[[ ]]__nm__/s/^.*[[ ]]__nm__\([[^ ]]*\)[[ ]][[^ ]]*/\1 DATA/;/^I[[ ]]/d;/^[[AITW]][[ ]]/s/.*[[ ]]//'\'' | sort | uniq > $export_symbols'
  ;;
  *)
    _LT_AC_TAGVAR(export_symbols_cmds, $1)='$NM $libobjs $convenience | $global_symbol_pipe | $SED '\''s/.* //'\'' | sort | uniq > $export_symbols'
  ;;
  esac
  _LT_AC_TAGVAR(exclude_expsyms, $1)=['_GLOBAL_OFFSET_TABLE_|_GLOBAL__F[ID]_.*']
],[
  runpath_var=
  _LT_AC_TAGVAR(allow_undefined_flag, $1)=
  _LT_AC_TAGVAR(enable_shared_with_static_runtimes, $1)=no
  _LT_AC_TAGVAR(archive_cmds, $1)=
  _LT_AC_TAGVAR(archive_expsym_cmds, $1)=
  _LT_AC_TAGVAR(old_archive_From_new_cmds, $1)=
  _LT_AC_TAGVAR(old_archive_from_expsyms_cmds, $1)=
  _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)=
  _LT_AC_TAGVAR(whole_archive_flag_spec, $1)=
  _LT_AC_TAGVAR(thread_safe_flag_spec, $1)=
  _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)=
  _LT_AC_TAGVAR(hardcode_libdir_flag_spec_ld, $1)=
  _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=
  _LT_AC_TAGVAR(hardcode_direct, $1)=no
  _LT_AC_TAGVAR(hardcode_minus_L, $1)=no
  _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=unsupported
  _LT_AC_TAGVAR(link_all_deplibs, $1)=unknown
  _LT_AC_TAGVAR(hardcode_automatic, $1)=no
  _LT_AC_TAGVAR(module_cmds, $1)=
  _LT_AC_TAGVAR(module_expsym_cmds, $1)=
  _LT_AC_TAGVAR(always_export_symbols, $1)=no
  _LT_AC_TAGVAR(export_symbols_cmds, $1)='$NM $libobjs $convenience | $global_symbol_pipe | $SED '\''s/.* //'\'' | sort | uniq > $export_symbols'
  # include_expsyms should be a list of space-separated symbols to be *always*
  # included in the symbol list
  _LT_AC_TAGVAR(include_expsyms, $1)=
  # exclude_expsyms can be an extended regexp of symbols to exclude
  # it will be wrapped by ` (' and `)$', so one must not match beginning or
  # end of line.  Example: `a|bc|.*d.*' will exclude the symbols `a' and `bc',
  # as well as any symbol that contains `d'.
  _LT_AC_TAGVAR(exclude_expsyms, $1)=['_GLOBAL_OFFSET_TABLE_|_GLOBAL__F[ID]_.*']
  # Although _GLOBAL_OFFSET_TABLE_ is a valid symbol C name, most a.out
  # platforms (ab)use it in PIC code, but their linkers get confused if
  # the symbol is explicitly referenced.  Since portable code cannot
  # rely on this symbol name, it's probably fine to never include it in
  # preloaded symbol tables.
  # Exclude shared library initialization/finalization symbols.
dnl Note also adjust exclude_expsyms for C++ above.
  extract_expsyms_cmds=
  # Just being paranoid about ensuring that cc_basename is set.
  _LT_CC_BASENAME([$compiler])
  case $host_os in
  cygwin* | mingw* | pw32*)
    # FIXME: the MSVC++ port hasn't been tested in a loooong time
    # When not using gcc, we currently assume that we are using
    # Microsoft Visual C++.
    if test "$GCC" != yes; then
      with_gnu_ld=no
    fi
    ;;
  interix*)
    # we just hope/assume this is gcc and not c89 (= MSVC++)
    with_gnu_ld=yes
    ;;
  openbsd*)
    with_gnu_ld=no
    ;;
  esac

  _LT_AC_TAGVAR(ld_shlibs, $1)=yes
  if test "$with_gnu_ld" = yes; then
    # If archive_cmds runs LD, not CC, wlarc should be empty
    wlarc='${wl}'

    # Set some defaults for GNU ld with shared library support. These
    # are reset later if shared libraries are not supported. Putting them
    # here allows them to be overridden if necessary.
    runpath_var=LD_RUN_PATH
    _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}--rpath ${wl}$libdir'
    _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}--export-dynamic'
    # ancient GNU ld didn't support --whole-archive et. al.
    if $LD --help 2>&1 | grep 'no-whole-archive' > /dev/null; then
	_LT_AC_TAGVAR(whole_archive_flag_spec, $1)="$wlarc"'--whole-archive$convenience '"$wlarc"'--no-whole-archive'
      else
  	_LT_AC_TAGVAR(whole_archive_flag_spec, $1)=
    fi
    supports_anon_versioning=no
    case `$LD -v 2>/dev/null` in
      *\ [[01]].* | *\ 2.[[0-9]].* | *\ 2.10.*) ;; # catch versions < 2.11
      *\ 2.11.93.0.2\ *) supports_anon_versioning=yes ;; # RH7.3 ...
      *\ 2.11.92.0.12\ *) supports_anon_versioning=yes ;; # Mandrake 8.2 ...
      *\ 2.11.*) ;; # other 2.11 versions
      *) supports_anon_versioning=yes ;;
    esac

    # See if GNU ld supports shared libraries.
    case $host_os in
    aix[[3-9]]*)
      # On AIX/PPC, the GNU linker is very broken
      if test "$host_cpu" != ia64; then
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	cat <<EOF 1>&2

*** Warning: the GNU linker, at least up to release 2.9.1, is reported
*** to be unable to reliably create shared libraries on AIX.
*** Therefore, libtool is disabling shared libraries support.  If you
*** really care for shared libraries, you may want to modify your PATH
*** so that a non-GNU linker is found, and then restart.

EOF
      fi
      ;;

    amigaos*)
      _LT_AC_TAGVAR(archive_cmds, $1)='$rm $output_objdir/a2ixlibrary.data~$echo "#define NAME $libname" > $output_objdir/a2ixlibrary.data~$echo "#define LIBRARY_ID 1" >> $output_objdir/a2ixlibrary.data~$echo "#define VERSION $major" >> $output_objdir/a2ixlibrary.data~$echo "#define REVISION $revision" >> $output_objdir/a2ixlibrary.data~$AR $AR_FLAGS $lib $libobjs~$RANLIB $lib~(cd $output_objdir && a2ixlibrary -32)'
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-L$libdir'
      _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes

      # Samuel A. Falvo II <kc5tja@dolphin.openprojects.net> reports
      # that the semantics of dynamic libraries on AmigaOS, at least up
      # to version 4, is to share data among multiple programs linked
      # with the same dynamic library.  Since this doesn't match the
      # behavior of shared libraries on other platforms, we can't use
      # them.
      _LT_AC_TAGVAR(ld_shlibs, $1)=no
      ;;

    beos*)
      if $LD --help 2>&1 | grep ': supported targets:.* elf' > /dev/null; then
	_LT_AC_TAGVAR(allow_undefined_flag, $1)=unsupported
	# Joseph Beckenbach <jrb3@best.com> says some releases of gcc
	# support --undefined.  This deserves some investigation.  FIXME
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -nostart $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
      else
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
      fi
      ;;

    cygwin* | mingw* | pw32*)
      # _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1) is actually meaningless,
      # as there is no search path for DLLs.
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-L$libdir'
      _LT_AC_TAGVAR(allow_undefined_flag, $1)=unsupported
      _LT_AC_TAGVAR(always_export_symbols, $1)=no
      _LT_AC_TAGVAR(enable_shared_with_static_runtimes, $1)=yes
      _LT_AC_TAGVAR(export_symbols_cmds, $1)='$NM $libobjs $convenience | $global_symbol_pipe | $SED -e '\''/^[[BCDGRS]][[ ]]/s/.*[[ ]]\([[^ ]]*\)/\1 DATA/'\'' -e '\''/^[[AITW]][[ ]]/s/.*[[ ]]//'\'' | sort | uniq > $export_symbols'

      if $LD --help 2>&1 | grep 'auto-import' > /dev/null; then
        _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $libobjs $deplibs $compiler_flags -o $output_objdir/$soname ${wl}--enable-auto-image-base -Xlinker --out-implib -Xlinker $lib'
	# If the export-symbols file already is a .def file (1st line
	# is EXPORTS), use it as is; otherwise, prepend...
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='if test "x`$SED 1q $export_symbols`" = xEXPORTS; then
	  cp $export_symbols $output_objdir/$soname.def;
	else
	  echo EXPORTS > $output_objdir/$soname.def;
	  cat $export_symbols >> $output_objdir/$soname.def;
	fi~
	$CC -shared $output_objdir/$soname.def $libobjs $deplibs $compiler_flags -o $output_objdir/$soname ${wl}--enable-auto-image-base -Xlinker --out-implib -Xlinker $lib'
      else
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
      fi
      ;;

    interix[[3-9]]*)
      _LT_AC_TAGVAR(hardcode_direct, $1)=no
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath,$libdir'
      _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-E'
      # Hack: On Interix 3.x, we cannot compile PIC because of a broken gcc.
      # Instead, shared libraries are loaded at an image base (0x10000000 by
      # default) and relocated if they conflict, which is a slow very memory
      # consuming and fragmenting process.  To avoid this, we pick a random,
      # 256 KiB-aligned image base between 0x50000000 and 0x6FFC0000 at link
      # time.  Moving up from 0x10000000 also allows more sbrk(2) space.
      _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $pic_flag $libobjs $deplibs $compiler_flags ${wl}-h,$soname ${wl}--image-base,`expr ${RANDOM-$$} % 4096 / 2 \* 262144 + 1342177280` -o $lib'
      _LT_AC_TAGVAR(archive_expsym_cmds, $1)='sed "s,^,_," $export_symbols >$output_objdir/$soname.expsym~$CC -shared $pic_flag $libobjs $deplibs $compiler_flags ${wl}-h,$soname ${wl}--retain-symbols-file,$output_objdir/$soname.expsym ${wl}--image-base,`expr ${RANDOM-$$} % 4096 / 2 \* 262144 + 1342177280` -o $lib'
      ;;

    gnu* | linux* | k*bsd*-gnu)
      if $LD --help 2>&1 | grep ': supported targets:.* elf' > /dev/null; then
	tmp_addflag=
	case $cc_basename,$host_cpu in
	pgcc*)				# Portland Group C compiler
	  _LT_AC_TAGVAR(whole_archive_flag_spec, $1)='${wl}--whole-archive`for conv in $convenience\"\"; do test  -n \"$conv\" && new_convenience=\"$new_convenience,$conv\"; done; $echo \"$new_convenience\"` ${wl}--no-whole-archive'
	  tmp_addflag=' $pic_flag'
	  ;;
	pgf77* | pgf90* | pgf95*)	# Portland Group f77 and f90 compilers
	  _LT_AC_TAGVAR(whole_archive_flag_spec, $1)='${wl}--whole-archive`for conv in $convenience\"\"; do test  -n \"$conv\" && new_convenience=\"$new_convenience,$conv\"; done; $echo \"$new_convenience\"` ${wl}--no-whole-archive'
	  tmp_addflag=' $pic_flag -Mnomain' ;;
	ecc*,ia64* | icc*,ia64*)		# Intel C compiler on ia64
	  tmp_addflag=' -i_dynamic' ;;
	efc*,ia64* | ifort*,ia64*)	# Intel Fortran compiler on ia64
	  tmp_addflag=' -i_dynamic -nofor_main' ;;
	ifc* | ifort*)			# Intel Fortran compiler
	  tmp_addflag=' -nofor_main' ;;
	esac
	case `$CC -V 2>&1 | sed 5q` in
	*Sun\ C*)			# Sun C 5.9
	  _LT_AC_TAGVAR(whole_archive_flag_spec, $1)='${wl}--whole-archive`new_convenience=; for conv in $convenience\"\"; do test -z \"$conv\" || new_convenience=\"$new_convenience,$conv\"; done; $echo \"$new_convenience\"` ${wl}--no-whole-archive'
	  tmp_sharedflag='-G' ;;
	*Sun\ F*)			# Sun Fortran 8.3
	  tmp_sharedflag='-G' ;;
	*)
	  tmp_sharedflag='-shared' ;;
	esac
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC '"$tmp_sharedflag""$tmp_addflag"' $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'

	if test $supports_anon_versioning = yes; then
	  _LT_AC_TAGVAR(archive_expsym_cmds, $1)='$echo "{ global:" > $output_objdir/$libname.ver~
  cat $export_symbols | sed -e "s/\(.*\)/\1;/" >> $output_objdir/$libname.ver~
  $echo "local: *; };" >> $output_objdir/$libname.ver~
	  $CC '"$tmp_sharedflag""$tmp_addflag"' $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-version-script ${wl}$output_objdir/$libname.ver -o $lib'
	fi
      else
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
      fi
      ;;

    netbsd*)
      if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
	_LT_AC_TAGVAR(archive_cmds, $1)='$LD -Bshareable $libobjs $deplibs $linker_flags -o $lib'
	wlarc=
      else
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
      fi
      ;;

    solaris*)
      if $LD -v 2>&1 | grep 'BFD 2\.8' > /dev/null; then
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	cat <<EOF 1>&2

*** Warning: The releases 2.8.* of the GNU linker cannot reliably
*** create shared libraries on Solaris systems.  Therefore, libtool
*** is disabling shared libraries support.  We urge you to upgrade GNU
*** binutils to release 2.9.1 or newer.  Another option is to modify
*** your PATH or compiler configuration so that the native linker is
*** used, and then restart.

EOF
      elif $LD --help 2>&1 | grep ': supported targets:.* elf' > /dev/null; then
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
      else
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
      fi
      ;;

    sysv5* | sco3.2v5* | sco5v6* | unixware* | OpenUNIX*)
      case `$LD -v 2>&1` in
        *\ [[01]].* | *\ 2.[[0-9]].* | *\ 2.1[[0-5]].*)
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
	cat <<_LT_EOF 1>&2

*** Warning: Releases of the GNU linker prior to 2.16.91.0.3 can not
*** reliably create shared libraries on SCO systems.  Therefore, libtool
*** is disabling shared libraries support.  We urge you to upgrade GNU
*** binutils to release 2.16.91.0.3 or newer.  Another option is to modify
*** your PATH or compiler configuration so that the native linker is
*** used, and then restart.

_LT_EOF
	;;
	*)
	  if $LD --help 2>&1 | grep ': supported targets:.* elf' > /dev/null; then
	    _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='`test -z "$SCOABSPATH" && echo ${wl}-rpath,$libdir`'
	    _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname,\${SCOABSPATH:+${install_libdir}/}$soname -o $lib'
	    _LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname,\${SCOABSPATH:+${install_libdir}/}$soname,-retain-symbols-file,$export_symbols -o $lib'
	  else
	    _LT_AC_TAGVAR(ld_shlibs, $1)=no
	  fi
	;;
      esac
      ;;

    sunos4*)
      _LT_AC_TAGVAR(archive_cmds, $1)='$LD -assert pure-text -Bshareable -o $lib $libobjs $deplibs $linker_flags'
      wlarc=
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;

    *)
      if $LD --help 2>&1 | grep ': supported targets:.* elf' > /dev/null; then
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
      else
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
      fi
      ;;
    esac

    if test "$_LT_AC_TAGVAR(ld_shlibs, $1)" = no; then
      runpath_var=
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)=
      _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)=
      _LT_AC_TAGVAR(whole_archive_flag_spec, $1)=
    fi
  else
    # PORTME fill in a description of your system's linker (not GNU ld)
    case $host_os in
    aix3*)
      _LT_AC_TAGVAR(allow_undefined_flag, $1)=unsupported
      _LT_AC_TAGVAR(always_export_symbols, $1)=yes
      _LT_AC_TAGVAR(archive_expsym_cmds, $1)='$LD -o $output_objdir/$soname $libobjs $deplibs $linker_flags -bE:$export_symbols -T512 -H512 -bM:SRE~$AR $AR_FLAGS $lib $output_objdir/$soname'
      # Note: this linker hardcodes the directories in LIBPATH if there
      # are no directories specified by -L.
      _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes
      if test "$GCC" = yes && test -z "$lt_prog_compiler_static"; then
	# Neither direct hardcoding nor static linking is supported with a
	# broken collect2.
	_LT_AC_TAGVAR(hardcode_direct, $1)=unsupported
      fi
      ;;

    aix[[4-9]]*)
      if test "$host_cpu" = ia64; then
	# On IA64, the linker does run time linking by default, so we don't
	# have to do anything special.
	aix_use_runtimelinking=no
	exp_sym_flag='-Bexport'
	no_entry_flag=""
      else
	# If we're using GNU nm, then we don't want the "-C" option.
	# -C means demangle to AIX nm, but means don't demangle with GNU nm
	if $NM -V 2>&1 | grep 'GNU' > /dev/null; then
	  _LT_AC_TAGVAR(export_symbols_cmds, $1)='$NM -Bpg $libobjs $convenience | awk '\''{ if (((\[$]2 == "T") || (\[$]2 == "D") || (\[$]2 == "B")) && ([substr](\[$]3,1,1) != ".")) { print \[$]3 } }'\'' | sort -u > $export_symbols'
	else
	  _LT_AC_TAGVAR(export_symbols_cmds, $1)='$NM -BCpg $libobjs $convenience | awk '\''{ if (((\[$]2 == "T") || (\[$]2 == "D") || (\[$]2 == "B")) && ([substr](\[$]3,1,1) != ".")) { print \[$]3 } }'\'' | sort -u > $export_symbols'
	fi
	aix_use_runtimelinking=no

	# Test if we are trying to use run time linking or normal
	# AIX style linking. If -brtl is somewhere in LDFLAGS, we
	# need to do runtime linking.
	case $host_os in aix4.[[23]]|aix4.[[23]].*|aix[[5-9]]*)
	  for ld_flag in $LDFLAGS; do
  	  if (test $ld_flag = "-brtl" || test $ld_flag = "-Wl,-brtl"); then
  	    aix_use_runtimelinking=yes
  	    break
  	  fi
	  done
	  ;;
	esac

	exp_sym_flag='-bexport'
	no_entry_flag='-bnoentry'
      fi

      # When large executables or shared objects are built, AIX ld can
      # have problems creating the table of contents.  If linking a library
      # or program results in "error TOC overflow" add -mminimal-toc to
      # CXXFLAGS/CFLAGS for g++/gcc.  In the cases where that is not
      # enough to fix the problem, add -Wl,-bbigtoc to LDFLAGS.

      _LT_AC_TAGVAR(archive_cmds, $1)=''
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=':'
      _LT_AC_TAGVAR(link_all_deplibs, $1)=yes

      if test "$GCC" = yes; then
	case $host_os in aix4.[[012]]|aix4.[[012]].*)
	# We only want to do this on AIX 4.2 and lower, the check
	# below for broken collect2 doesn't work under 4.3+
	  collect2name=`${CC} -print-prog-name=collect2`
	  if test -f "$collect2name" && \
  	   strings "$collect2name" | grep resolve_lib_name >/dev/null
	  then
  	  # We have reworked collect2
  	  :
	  else
  	  # We have old collect2
  	  _LT_AC_TAGVAR(hardcode_direct, $1)=unsupported
  	  # It fails to find uninstalled libraries when the uninstalled
  	  # path is not listed in the libpath.  Setting hardcode_minus_L
  	  # to unsupported forces relinking
  	  _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes
  	  _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-L$libdir'
  	  _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=
	  fi
	  ;;
	esac
	shared_flag='-shared'
	if test "$aix_use_runtimelinking" = yes; then
	  shared_flag="$shared_flag "'${wl}-G'
	fi
      else
	# not using gcc
	if test "$host_cpu" = ia64; then
  	# VisualAge C++, Version 5.5 for AIX 5L for IA-64, Beta 3 Release
  	# chokes on -Wl,-G. The following line is correct:
	  shared_flag='-G'
	else
	  if test "$aix_use_runtimelinking" = yes; then
	    shared_flag='${wl}-G'
	  else
	    shared_flag='${wl}-bM:SRE'
	  fi
	fi
      fi

      # It seems that -bexpall does not export symbols beginning with
      # underscore (_), so it is better to generate a list of symbols to export.
      _LT_AC_TAGVAR(always_export_symbols, $1)=yes
      if test "$aix_use_runtimelinking" = yes; then
	# Warning - without using the other runtime loading flags (-brtl),
	# -berok will link without error, but may produce a broken library.
	_LT_AC_TAGVAR(allow_undefined_flag, $1)='-berok'
       # Determine the default libpath from the value encoded in an empty executable.
       _LT_AC_SYS_LIBPATH_AIX
       _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-blibpath:$libdir:'"$aix_libpath"
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)="\$CC"' -o $output_objdir/$soname $libobjs $deplibs '"\${wl}$no_entry_flag"' $compiler_flags `if test "x${allow_undefined_flag}" != "x"; then echo "${wl}${allow_undefined_flag}"; else :; fi` '"\${wl}$exp_sym_flag:\$export_symbols $shared_flag"
       else
	if test "$host_cpu" = ia64; then
	  _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-R $libdir:/usr/lib:/lib'
	  _LT_AC_TAGVAR(allow_undefined_flag, $1)="-z nodefs"
	  _LT_AC_TAGVAR(archive_expsym_cmds, $1)="\$CC $shared_flag"' -o $output_objdir/$soname $libobjs $deplibs '"\${wl}$no_entry_flag"' $compiler_flags ${wl}${allow_undefined_flag} '"\${wl}$exp_sym_flag:\$export_symbols"
	else
	 # Determine the default libpath from the value encoded in an empty executable.
	 _LT_AC_SYS_LIBPATH_AIX
	 _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-blibpath:$libdir:'"$aix_libpath"
	  # Warning - without using the other run time loading flags,
	  # -berok will link without error, but may produce a broken library.
	  _LT_AC_TAGVAR(no_undefined_flag, $1)=' ${wl}-bernotok'
	  _LT_AC_TAGVAR(allow_undefined_flag, $1)=' ${wl}-berok'
	  # Exported symbols can be pulled into shared objects from archives
	  _LT_AC_TAGVAR(whole_archive_flag_spec, $1)='$convenience'
	  _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=yes
	  # This is similar to how AIX traditionally builds its shared libraries.
	  _LT_AC_TAGVAR(archive_expsym_cmds, $1)="\$CC $shared_flag"' -o $output_objdir/$soname $libobjs $deplibs ${wl}-bnoentry $compiler_flags ${wl}-bE:$export_symbols${allow_undefined_flag}~$AR $AR_FLAGS $output_objdir/$libname$release.a $output_objdir/$soname'
	fi
      fi
      ;;

    amigaos*)
      _LT_AC_TAGVAR(archive_cmds, $1)='$rm $output_objdir/a2ixlibrary.data~$echo "#define NAME $libname" > $output_objdir/a2ixlibrary.data~$echo "#define LIBRARY_ID 1" >> $output_objdir/a2ixlibrary.data~$echo "#define VERSION $major" >> $output_objdir/a2ixlibrary.data~$echo "#define REVISION $revision" >> $output_objdir/a2ixlibrary.data~$AR $AR_FLAGS $lib $libobjs~$RANLIB $lib~(cd $output_objdir && a2ixlibrary -32)'
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-L$libdir'
      _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes
      # see comment about different semantics on the GNU ld section
      _LT_AC_TAGVAR(ld_shlibs, $1)=no
      ;;

    bsdi[[45]]*)
      _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)=-rdynamic
      ;;

    cygwin* | mingw* | pw32*)
      # When not using gcc, we currently assume that we are using
      # Microsoft Visual C++.
      # hardcode_libdir_flag_spec is actually meaningless, as there is
      # no search path for DLLs.
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)=' '
      _LT_AC_TAGVAR(allow_undefined_flag, $1)=unsupported
      # Tell ltmain to make .lib files, not .a files.
      libext=lib
      # Tell ltmain to make .dll files, not .so files.
      shrext_cmds=".dll"
      # FIXME: Setting linknames here is a bad hack.
      _LT_AC_TAGVAR(archive_cmds, $1)='$CC -o $lib $libobjs $compiler_flags `echo "$deplibs" | $SED -e '\''s/ -lc$//'\''` -link -dll~linknames='
      # The linker will automatically build a .lib file if we build a DLL.
      _LT_AC_TAGVAR(old_archive_From_new_cmds, $1)='true'
      # FIXME: Should let the user specify the lib program.
      _LT_AC_TAGVAR(old_archive_cmds, $1)='lib -OUT:$oldlib$oldobjs$old_deplibs'
      _LT_AC_TAGVAR(fix_srcfile_path, $1)='`cygpath -w "$srcfile"`'
      _LT_AC_TAGVAR(enable_shared_with_static_runtimes, $1)=yes
      ;;

    darwin* | rhapsody*)
      case $host_os in
        rhapsody* | darwin1.[[012]])
         _LT_AC_TAGVAR(allow_undefined_flag, $1)='${wl}-undefined ${wl}suppress'
         ;;
       *) # Darwin 1.3 on
         if test -z ${MACOSX_DEPLOYMENT_TARGET} ; then
           _LT_AC_TAGVAR(allow_undefined_flag, $1)='${wl}-flat_namespace ${wl}-undefined ${wl}suppress'
         else
           case ${MACOSX_DEPLOYMENT_TARGET} in
             10.[[012]])
               _LT_AC_TAGVAR(allow_undefined_flag, $1)='${wl}-flat_namespace ${wl}-undefined ${wl}suppress'
               ;;
             10.*)
               _LT_AC_TAGVAR(allow_undefined_flag, $1)='${wl}-undefined ${wl}dynamic_lookup'
               ;;
           esac
         fi
         ;;
      esac
      _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
      _LT_AC_TAGVAR(hardcode_direct, $1)=no
      _LT_AC_TAGVAR(hardcode_automatic, $1)=yes
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=unsupported
      _LT_AC_TAGVAR(whole_archive_flag_spec, $1)=''
      _LT_AC_TAGVAR(link_all_deplibs, $1)=yes
    if test "$GCC" = yes ; then
    	output_verbose_link_cmd='echo'
        _LT_AC_TAGVAR(archive_cmds, $1)="\$CC -dynamiclib \$allow_undefined_flag -o \$lib \$libobjs \$deplibs \$compiler_flags -install_name \$rpath/\$soname \$verstring $_lt_dar_single_mod${_lt_dsymutil}"
        _LT_AC_TAGVAR(module_cmds, $1)="\$CC \$allow_undefined_flag -o \$lib -bundle \$libobjs \$deplibs \$compiler_flags${_lt_dsymutil}"
        _LT_AC_TAGVAR(archive_expsym_cmds, $1)="sed 's,^,_,' < \$export_symbols > \$output_objdir/\${libname}-symbols.expsym~\$CC -dynamiclib \$allow_undefined_flag -o \$lib \$libobjs \$deplibs \$compiler_flags -install_name \$rpath/\$soname \$verstring ${_lt_dar_single_mod}${_lt_dar_export_syms}${_lt_dsymutil}"
        _LT_AC_TAGVAR(module_expsym_cmds, $1)="sed -e 's,^,_,' < \$export_symbols > \$output_objdir/\${libname}-symbols.expsym~\$CC \$allow_undefined_flag -o \$lib -bundle \$libobjs \$deplibs \$compiler_flags${_lt_dar_export_syms}${_lt_dsymutil}"
    else
      case $cc_basename in
        xlc*)
         output_verbose_link_cmd='echo'
         _LT_AC_TAGVAR(archive_cmds, $1)='$CC -qmkshrobj $allow_undefined_flag -o $lib $libobjs $deplibs $compiler_flags ${wl}-install_name ${wl}`echo $rpath/$soname` $xlcverstring'
         _LT_AC_TAGVAR(module_cmds, $1)='$CC $allow_undefined_flag -o $lib -bundle $libobjs $deplibs$compiler_flags'
          # Don't fix this by using the ld -exported_symbols_list flag, it doesn't exist in older darwin lds
         _LT_AC_TAGVAR(archive_expsym_cmds, $1)='sed -e "s,#.*,," -e "s,^[    ]*,," -e "s,^\(..*\),_&," < $export_symbols > $output_objdir/${libname}-symbols.expsym~$CC -qmkshrobj $allow_undefined_flag -o $lib $libobjs $deplibs $compiler_flags ${wl}-install_name ${wl}$rpath/$soname $xlcverstring~nmedit -s $output_objdir/${libname}-symbols.expsym ${lib}'
          _LT_AC_TAGVAR(module_expsym_cmds, $1)='sed -e "s,#.*,," -e "s,^[    ]*,," -e "s,^\(..*\),_&," < $export_symbols > $output_objdir/${libname}-symbols.expsym~$CC $allow_undefined_flag  -o $lib -bundle $libobjs $deplibs$compiler_flags~nmedit -s $output_objdir/${libname}-symbols.expsym ${lib}'
          ;;
       *)
         _LT_AC_TAGVAR(ld_shlibs, $1)=no
          ;;
      esac
    fi
      ;;

    dgux*)
      _LT_AC_TAGVAR(archive_cmds, $1)='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-L$libdir'
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;

    freebsd1*)
      _LT_AC_TAGVAR(ld_shlibs, $1)=no
      ;;

    # FreeBSD 2.2.[012] allows us to include c++rt0.o to get C++ constructor
    # support.  Future versions do this automatically, but an explicit c++rt0.o
    # does not break anything, and helps significantly (at the cost of a little
    # extra space).
    freebsd2.2*)
      _LT_AC_TAGVAR(archive_cmds, $1)='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags /usr/lib/c++rt0.o'
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-R$libdir'
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;

    # Unfortunately, older versions of FreeBSD 2 do not have this feature.
    freebsd2*)
      _LT_AC_TAGVAR(archive_cmds, $1)='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;

    # FreeBSD 3 and greater uses gcc -shared to do shared libraries.
    freebsd* | dragonfly*)
      _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -o $lib $libobjs $deplibs $compiler_flags'
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-R$libdir'
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;

    hpux9*)
      if test "$GCC" = yes; then
	_LT_AC_TAGVAR(archive_cmds, $1)='$rm $output_objdir/$soname~$CC -shared -fPIC ${wl}+b ${wl}$install_libdir -o $output_objdir/$soname $libobjs $deplibs $compiler_flags~test $output_objdir/$soname = $lib || mv $output_objdir/$soname $lib'
      else
	_LT_AC_TAGVAR(archive_cmds, $1)='$rm $output_objdir/$soname~$LD -b +b $install_libdir -o $output_objdir/$soname $libobjs $deplibs $linker_flags~test $output_objdir/$soname = $lib || mv $output_objdir/$soname $lib'
      fi
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}+b ${wl}$libdir'
      _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes

      # hardcode_minus_L: Not really in the search PATH,
      # but as the default location of the library.
      _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes
      _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-E'
      ;;

    hpux10*)
      if test "$GCC" = yes -a "$with_gnu_ld" = no; then
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -fPIC ${wl}+h ${wl}$soname ${wl}+b ${wl}$install_libdir -o $lib $libobjs $deplibs $compiler_flags'
      else
	_LT_AC_TAGVAR(archive_cmds, $1)='$LD -b +h $soname +b $install_libdir -o $lib $libobjs $deplibs $linker_flags'
      fi
      if test "$with_gnu_ld" = no; then
	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}+b ${wl}$libdir'
	_LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:

	_LT_AC_TAGVAR(hardcode_direct, $1)=yes
	_LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-E'

	# hardcode_minus_L: Not really in the search PATH,
	# but as the default location of the library.
	_LT_AC_TAGVAR(hardcode_minus_L, $1)=yes
      fi
      ;;

    hpux11*)
      if test "$GCC" = yes -a "$with_gnu_ld" = no; then
	case $host_cpu in
	hppa*64*)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared ${wl}+h ${wl}$soname -o $lib $libobjs $deplibs $compiler_flags'
	  ;;
	ia64*)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared ${wl}+h ${wl}$soname ${wl}+nodefaultrpath -o $lib $libobjs $deplibs $compiler_flags'
	  ;;
	*)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared -fPIC ${wl}+h ${wl}$soname ${wl}+b ${wl}$install_libdir -o $lib $libobjs $deplibs $compiler_flags'
	  ;;
	esac
      else
	case $host_cpu in
	hppa*64*)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -b ${wl}+h ${wl}$soname -o $lib $libobjs $deplibs $compiler_flags'
	  ;;
	ia64*)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -b ${wl}+h ${wl}$soname ${wl}+nodefaultrpath -o $lib $libobjs $deplibs $compiler_flags'
	  ;;
	*)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -b ${wl}+h ${wl}$soname ${wl}+b ${wl}$install_libdir -o $lib $libobjs $deplibs $compiler_flags'
	  ;;
	esac
      fi
      if test "$with_gnu_ld" = no; then
	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}+b ${wl}$libdir'
	_LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:

	case $host_cpu in
	hppa*64*|ia64*)
	  _LT_AC_TAGVAR(hardcode_libdir_flag_spec_ld, $1)='+b $libdir'
	  _LT_AC_TAGVAR(hardcode_direct, $1)=no
	  _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
	  ;;
	*)
	  _LT_AC_TAGVAR(hardcode_direct, $1)=yes
	  _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-E'

	  # hardcode_minus_L: Not really in the search PATH,
	  # but as the default location of the library.
	  _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes
	  ;;
	esac
      fi
      ;;

    irix5* | irix6* | nonstopux*)
      if test "$GCC" = yes; then
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
      else
	_LT_AC_TAGVAR(archive_cmds, $1)='$LD -shared $libobjs $deplibs $linker_flags -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
	_LT_AC_TAGVAR(hardcode_libdir_flag_spec_ld, $1)='-rpath $libdir'
      fi
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath ${wl}$libdir'
      _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:
      _LT_AC_TAGVAR(link_all_deplibs, $1)=yes
      ;;

    netbsd*)
      if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
	_LT_AC_TAGVAR(archive_cmds, $1)='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'  # a.out
      else
	_LT_AC_TAGVAR(archive_cmds, $1)='$LD -shared -o $lib $libobjs $deplibs $linker_flags'      # ELF
      fi
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-R$libdir'
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;

    newsos6)
      _LT_AC_TAGVAR(archive_cmds, $1)='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath ${wl}$libdir'
      _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;

    openbsd*)
      if test -f /usr/libexec/ld.so; then
	_LT_AC_TAGVAR(hardcode_direct, $1)=yes
	_LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
	if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
	  _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $pic_flag -o $lib $libobjs $deplibs $compiler_flags'
	  _LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared $pic_flag -o $lib $libobjs $deplibs $compiler_flags ${wl}-retain-symbols-file,$export_symbols'
	  _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath,$libdir'
	  _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-E'
	else
	  case $host_os in
	   openbsd[[01]].* | openbsd2.[[0-7]] | openbsd2.[[0-7]].*)
	     _LT_AC_TAGVAR(archive_cmds, $1)='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'
	     _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-R$libdir'
	     ;;
	   *)
	     _LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared $pic_flag -o $lib $libobjs $deplibs $compiler_flags'
	     _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath,$libdir'
	     ;;
	  esac
        fi
      else
	_LT_AC_TAGVAR(ld_shlibs, $1)=no
      fi
      ;;

    os2*)
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-L$libdir'
      _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes
      _LT_AC_TAGVAR(allow_undefined_flag, $1)=unsupported
      _LT_AC_TAGVAR(archive_cmds, $1)='$echo "LIBRARY $libname INITINSTANCE" > $output_objdir/$libname.def~$echo "DESCRIPTION \"$libname\"" >> $output_objdir/$libname.def~$echo DATA >> $output_objdir/$libname.def~$echo " SINGLE NONSHARED" >> $output_objdir/$libname.def~$echo EXPORTS >> $output_objdir/$libname.def~emxexp $libobjs >> $output_objdir/$libname.def~$CC -Zdll -Zcrtdll -o $lib $libobjs $deplibs $compiler_flags $output_objdir/$libname.def'
      _LT_AC_TAGVAR(old_archive_From_new_cmds, $1)='emximp -o $output_objdir/$libname.a $output_objdir/$libname.def'
      ;;

    osf3*)
      if test "$GCC" = yes; then
	_LT_AC_TAGVAR(allow_undefined_flag, $1)=' ${wl}-expect_unresolved ${wl}\*'
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared${allow_undefined_flag} $libobjs $deplibs $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
      else
	_LT_AC_TAGVAR(allow_undefined_flag, $1)=' -expect_unresolved \*'
	_LT_AC_TAGVAR(archive_cmds, $1)='$LD -shared${allow_undefined_flag} $libobjs $deplibs $linker_flags -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
      fi
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath ${wl}$libdir'
      _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:
      ;;

    osf4* | osf5*)	# as osf3* with the addition of -msym flag
      if test "$GCC" = yes; then
	_LT_AC_TAGVAR(allow_undefined_flag, $1)=' ${wl}-expect_unresolved ${wl}\*'
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared${allow_undefined_flag} $libobjs $deplibs $compiler_flags ${wl}-msym ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='${wl}-rpath ${wl}$libdir'
      else
	_LT_AC_TAGVAR(allow_undefined_flag, $1)=' -expect_unresolved \*'
	_LT_AC_TAGVAR(archive_cmds, $1)='$LD -shared${allow_undefined_flag} $libobjs $deplibs $linker_flags -msym -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='for i in `cat $export_symbols`; do printf "%s %s\\n" -exported_symbol "\$i" >> $lib.exp; done; echo "-hidden">> $lib.exp~
	$LD -shared${allow_undefined_flag} -input $lib.exp $linker_flags $libobjs $deplibs -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib~$rm $lib.exp'

	# Both c and cxx compiler support -rpath directly
	_LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-rpath $libdir'
      fi
      _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=:
      ;;

    solaris*)
      _LT_AC_TAGVAR(no_undefined_flag, $1)=' -z text'
      if test "$GCC" = yes; then
	wlarc='${wl}'
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared ${wl}-h ${wl}$soname -o $lib $libobjs $deplibs $compiler_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$echo "{ global:" > $lib.exp~cat $export_symbols | $SED -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
	  $CC -shared ${wl}-M ${wl}$lib.exp ${wl}-h ${wl}$soname -o $lib $libobjs $deplibs $compiler_flags~$rm $lib.exp'
      else
	wlarc=''
	_LT_AC_TAGVAR(archive_cmds, $1)='$LD -G${allow_undefined_flag} -h $soname -o $lib $libobjs $deplibs $linker_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$echo "{ global:" > $lib.exp~cat $export_symbols | $SED -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
  	$LD -G${allow_undefined_flag} -M $lib.exp -h $soname -o $lib $libobjs $deplibs $linker_flags~$rm $lib.exp'
      fi
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-R$libdir'
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      case $host_os in
      solaris2.[[0-5]] | solaris2.[[0-5]].*) ;;
      *)
	# The compiler driver will combine and reorder linker options,
	# but understands `-z linker_flag'.  GCC discards it without `$wl',
	# but is careful enough not to reorder.
 	# Supported since Solaris 2.6 (maybe 2.5.1?)
	if test "$GCC" = yes; then
	  _LT_AC_TAGVAR(whole_archive_flag_spec, $1)='${wl}-z ${wl}allextract$convenience ${wl}-z ${wl}defaultextract'
	else
	  _LT_AC_TAGVAR(whole_archive_flag_spec, $1)='-z allextract$convenience -z defaultextract'
	fi
	;;
      esac
      _LT_AC_TAGVAR(link_all_deplibs, $1)=yes
      ;;

    sunos4*)
      if test "x$host_vendor" = xsequent; then
	# Use $CC to link under sequent, because it throws in some extra .o
	# files that make .init and .fini sections work.
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -G ${wl}-h $soname -o $lib $libobjs $deplibs $compiler_flags'
      else
	_LT_AC_TAGVAR(archive_cmds, $1)='$LD -assert pure-text -Bstatic -o $lib $libobjs $deplibs $linker_flags'
      fi
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-L$libdir'
      _LT_AC_TAGVAR(hardcode_direct, $1)=yes
      _LT_AC_TAGVAR(hardcode_minus_L, $1)=yes
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;

    sysv4)
      case $host_vendor in
	sni)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
	  _LT_AC_TAGVAR(hardcode_direct, $1)=yes # is this really true???
	;;
	siemens)
	  ## LD is ld it makes a PLAMLIB
	  ## CC just makes a GrossModule.
	  _LT_AC_TAGVAR(archive_cmds, $1)='$LD -G -o $lib $libobjs $deplibs $linker_flags'
	  _LT_AC_TAGVAR(reload_cmds, $1)='$CC -r -o $output$reload_objs'
	  _LT_AC_TAGVAR(hardcode_direct, $1)=no
        ;;
	motorola)
	  _LT_AC_TAGVAR(archive_cmds, $1)='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
	  _LT_AC_TAGVAR(hardcode_direct, $1)=no #Motorola manual says yes, but my tests say they lie
	;;
      esac
      runpath_var='LD_RUN_PATH'
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;

    sysv4.3*)
      _LT_AC_TAGVAR(archive_cmds, $1)='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='-Bexport'
      ;;

    sysv4*MP*)
      if test -d /usr/nec; then
	_LT_AC_TAGVAR(archive_cmds, $1)='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
	_LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
	runpath_var=LD_RUN_PATH
	hardcode_runpath_var=yes
	_LT_AC_TAGVAR(ld_shlibs, $1)=yes
      fi
      ;;

    sysv4*uw2* | sysv5OpenUNIX* | sysv5UnixWare7.[[01]].[[10]]* | unixware7* | sco3.2v5.0.[[024]]*)
      _LT_AC_TAGVAR(no_undefined_flag, $1)='${wl}-z,text'
      _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      runpath_var='LD_RUN_PATH'

      if test "$GCC" = yes; then
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared ${wl}-h,$soname -o $lib $libobjs $deplibs $compiler_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared ${wl}-Bexport:$export_symbols ${wl}-h,$soname -o $lib $libobjs $deplibs $compiler_flags'
      else
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -G ${wl}-h,$soname -o $lib $libobjs $deplibs $compiler_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -G ${wl}-Bexport:$export_symbols ${wl}-h,$soname -o $lib $libobjs $deplibs $compiler_flags'
      fi
      ;;

    sysv5* | sco3.2v5* | sco5v6*)
      # Note: We can NOT use -z defs as we might desire, because we do not
      # link with -lc, and that would cause any symbols used from libc to
      # always be unresolved, which means just about no library would
      # ever link correctly.  If we're not using GNU ld we use -z text
      # though, which does catch some bad symbols but isn't as heavy-handed
      # as -z defs.
      _LT_AC_TAGVAR(no_undefined_flag, $1)='${wl}-z,text'
      _LT_AC_TAGVAR(allow_undefined_flag, $1)='${wl}-z,nodefs'
      _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='`test -z "$SCOABSPATH" && echo ${wl}-R,$libdir`'
      _LT_AC_TAGVAR(hardcode_libdir_separator, $1)=':'
      _LT_AC_TAGVAR(link_all_deplibs, $1)=yes
      _LT_AC_TAGVAR(export_dynamic_flag_spec, $1)='${wl}-Bexport'
      runpath_var='LD_RUN_PATH'

      if test "$GCC" = yes; then
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -shared ${wl}-h,\${SCOABSPATH:+${install_libdir}/}$soname -o $lib $libobjs $deplibs $compiler_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -shared ${wl}-Bexport:$export_symbols ${wl}-h,\${SCOABSPATH:+${install_libdir}/}$soname -o $lib $libobjs $deplibs $compiler_flags'
      else
	_LT_AC_TAGVAR(archive_cmds, $1)='$CC -G ${wl}-h,\${SCOABSPATH:+${install_libdir}/}$soname -o $lib $libobjs $deplibs $compiler_flags'
	_LT_AC_TAGVAR(archive_expsym_cmds, $1)='$CC -G ${wl}-Bexport:$export_symbols ${wl}-h,\${SCOABSPATH:+${install_libdir}/}$soname -o $lib $libobjs $deplibs $compiler_flags'
      fi
      ;;

    uts4*)
      _LT_AC_TAGVAR(archive_cmds, $1)='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
      _LT_AC_TAGVAR(hardcode_libdir_flag_spec, $1)='-L$libdir'
      _LT_AC_TAGVAR(hardcode_shlibpath_var, $1)=no
      ;;

    *)
      _LT_AC_TAGVAR(ld_shlibs, $1)=no
      ;;
    esac
  fi
])
AC_MSG_RESULT([$_LT_AC_TAGVAR(ld_shlibs, $1)])
test "$_LT_AC_TAGVAR(ld_shlibs, $1)" = no && can_build_shared=no

#
# Do we need to explicitly link libc?
#
case "x$_LT_AC_TAGVAR(archive_cmds_need_lc, $1)" in
x|xyes)
  # Assume -lc should be added
  _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=yes

  if test "$enable_shared" = yes && test "$GCC" = yes; then
    case $_LT_AC_TAGVAR(archive_cmds, $1) in
    *'~'*)
      # FIXME: we may have to deal with multi-command sequences.
      ;;
    '$CC '*)
      # Test whether the compiler implicitly links with -lc since on some
      # systems, -lgcc has to come before -lc. If gcc already passes -lc
      # to ld, don't add -lc before -lgcc.
      AC_MSG_CHECKING([whether -lc should be explicitly linked in])
      $rm conftest*
      echo "$lt_simple_compile_test_code" > conftest.$ac_ext

      if AC_TRY_EVAL(ac_compile) 2>conftest.err; then
        soname=conftest
        lib=conftest
        libobjs=conftest.$ac_objext
        deplibs=
        wl=$_LT_AC_TAGVAR(lt_prog_compiler_wl, $1)
	pic_flag=$_LT_AC_TAGVAR(lt_prog_compiler_pic, $1)
        compiler_flags=-v
        linker_flags=-v
        verstring=
        output_objdir=.
        libname=conftest
        lt_save_allow_undefined_flag=$_LT_AC_TAGVAR(allow_undefined_flag, $1)
        _LT_AC_TAGVAR(allow_undefined_flag, $1)=
        if AC_TRY_EVAL(_LT_AC_TAGVAR(archive_cmds, $1) 2\>\&1 \| grep \" -lc \" \>/dev/null 2\>\&1)
        then
	  _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=no
        else
	  _LT_AC_TAGVAR(archive_cmds_need_lc, $1)=yes
        fi
        _LT_AC_TAGVAR(allow_undefined_flag, $1)=$lt_save_allow_undefined_flag
      else
        cat conftest.err 1>&5
      fi
      $rm conftest*
      AC_MSG_RESULT([$_LT_AC_TAGVAR(archive_cmds_need_lc, $1)])
      ;;
    esac
  fi
  ;;
esac
])# AC_LIBTOOL_PROG_LD_SHLIBS


# _LT_AC_FILE_LTDLL_C
# -------------------
# Be careful that the start marker always follows a newline.
AC_DEFUN([_LT_AC_FILE_LTDLL_C], [
# /* ltdll.c starts here */
# #define WIN32_LEAN_AND_MEAN
# #include <windows.h>
# #undef WIN32_LEAN_AND_MEAN
# #include <stdio.h>
#
# #ifndef __CYGWIN__
# #  ifdef __CYGWIN32__
# #    define __CYGWIN__ __CYGWIN32__
# #  endif
# #endif
#
# #ifdef __cplusplus
# extern "C" {
# #endif
# BOOL APIENTRY DllMain (HINSTANCE hInst, DWORD reason, LPVOID reserved);
# #ifdef __cplusplus
# }
# #endif
#
# #ifdef __CYGWIN__
# #include <cygwin/cygwin_dll.h>
# DECLARE_CYGWIN_DLL( DllMain );
# #endif
# HINSTANCE __hDllInstance_base;
#
# BOOL APIENTRY
# DllMain (HINSTANCE hInst, DWORD reason, LPVOID reserved)
# {
#   __hDllInstance_base = hInst;
#   return TRUE;
# }
# /* ltdll.c ends here */
])# _LT_AC_FILE_LTDLL_C


# _LT_AC_TAGVAR(VARNAME, [TAGNAME])
# ---------------------------------
AC_DEFUN([_LT_AC_TAGVAR], [ifelse([$2], [], [$1], [$1_$2])])


# old names
AC_DEFUN([AM_PROG_LIBTOOL],   [AC_PROG_LIBTOOL])
AC_DEFUN([AM_ENABLE_SHARED],  [AC_ENABLE_SHARED($@)])
AC_DEFUN([AM_ENABLE_STATIC],  [AC_ENABLE_STATIC($@)])
AC_DEFUN([AM_DISABLE_SHARED], [AC_DISABLE_SHARED($@)])
AC_DEFUN([AM_DISABLE_STATIC], [AC_DISABLE_STATIC($@)])
AC_DEFUN([AM_PROG_LD],        [AC_PROG_LD])
AC_DEFUN([AM_PROG_NM],        [AC_PROG_NM])

# This is just to silence aclocal about the macro not being used
ifelse([AC_DISABLE_FAST_INSTALL])

AC_DEFUN([LT_AC_PROG_GCJ],
[AC_CHECK_TOOL(GCJ, gcj, no)
  test "x${GCJFLAGS+set}" = xset || GCJFLAGS="-g -O2"
  AC_SUBST(GCJFLAGS)
])

AC_DEFUN([LT_AC_PROG_RC],
[AC_CHECK_TOOL(RC, windres, no)
])


# Cheap backport of AS_EXECUTABLE_P and required macros
# from Autoconf 2.59; we should not use $as_executable_p directly.

# _AS_TEST_PREPARE
# ----------------
m4_ifndef([_AS_TEST_PREPARE],
[m4_defun([_AS_TEST_PREPARE],
[if test -x / >/dev/null 2>&1; then
  as_executable_p='test -x'
else
  as_executable_p='test -f'
fi
])])# _AS_TEST_PREPARE

# AS_EXECUTABLE_P
# ---------------
# Check whether a file is executable.
m4_ifndef([AS_EXECUTABLE_P],
[m4_defun([AS_EXECUTABLE_P],
[AS_REQUIRE([_AS_TEST_PREPARE])dnl
$as_executable_p $1[]dnl
])])# AS_EXECUTABLE_P

# NOTE: This macro has been submitted for inclusion into   #
#  GNU Autoconf as AC_PROG_SED.  When it is available in   #
#  a released version of Autoconf we should remove this    #
#  macro and use it instead.                               #
# LT_AC_PROG_SED
# --------------
# Check for a fully-functional sed program, that truncates
# as few characters as possible.  Prefer GNU sed if found.
AC_DEFUN([LT_AC_PROG_SED],
[AC_MSG_CHECKING([for a sed that does not truncate output])
AC_CACHE_VAL(lt_cv_path_SED,
[# Loop through the user's path and test for sed and gsed.
# Then use that list of sed's as ones to test for truncation.
as_save_IFS=$IFS; IFS=$PATH_SEPARATOR
for as_dir in $PATH
do
  IFS=$as_save_IFS
  test -z "$as_dir" && as_dir=.
  for lt_ac_prog in sed gsed; do
    for ac_exec_ext in '' $ac_executable_extensions; do
      if AS_EXECUTABLE_P(["$as_dir/$lt_ac_prog$ac_exec_ext"]); then
        lt_ac_sed_list="$lt_ac_sed_list $as_dir/$lt_ac_prog$ac_exec_ext"
      fi
    done
  done
done
IFS=$as_save_IFS
lt_ac_max=0
lt_ac_count=0
# Add /usr/xpg4/bin/sed as it is typically found on Solaris
# along with /bin/sed that truncates output.
for lt_ac_sed in $lt_ac_sed_list /usr/xpg4/bin/sed; do
  test ! -f $lt_ac_sed && continue
  cat /dev/null > conftest.in
  lt_ac_count=0
  echo $ECHO_N "0123456789$ECHO_C" >conftest.in
  # Check for GNU sed and select it if it is found.
  if "$lt_ac_sed" --version 2>&1 < /dev/null | grep 'GNU' > /dev/null; then
    lt_cv_path_SED=$lt_ac_sed
    break
  fi
  while true; do
    cat conftest.in conftest.in >conftest.tmp
    mv conftest.tmp conftest.in
    cp conftest.in conftest.nl
    echo >>conftest.nl
    $lt_ac_sed -e 's/a$//' < conftest.nl >conftest.out || break
    cmp -s conftest.out conftest.nl || break
    # 10000 chars as input seems more than enough
    test $lt_ac_count -gt 10 && break
    lt_ac_count=`expr $lt_ac_count + 1`
    if test $lt_ac_count -gt $lt_ac_max; then
      lt_ac_max=$lt_ac_count
      lt_cv_path_SED=$lt_ac_sed
    fi
  done
done
])
SED=$lt_cv_path_SED
AC_SUBST([SED])
AC_MSG_RESULT([$SED])
])

dnl -*- Autoconf -*-
dnl Copyright (C) 2008 Sam Steingold (repleased under GPLv2+)

AC_DEFUN([CL_LIGHTNING], [
AC_ARG_WITH([lightning-prefix],
[AS_HELP_STRING([--with-lightning-prefix],
[additional place to look for the lightning headers])],
[if test -f $withval/include/lightning.h; then
  AC_LIB_APPENDTOVAR([CPPFLAGS],-I$withval/include)
fi],[])
AC_CHECK_HEADERS(lightning.h)])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.13)

AC_DEFUN([CL_PROG_LN],
[AC_CACHE_CHECK(how to make hard links, cl_cv_prog_LN, [
rm -f conftestdata conftestfile
echo data > conftestfile
if ln conftestfile conftestdata 2>/dev/null; then
  cl_cv_prog_LN=ln
else
  cl_cv_prog_LN="cp -p"
fi
rm -f conftestdata conftestfile
])
LN="$cl_cv_prog_LN"
AC_SUBST(LN)dnl
])

AC_DEFUN([CL_PROG_LN_S],
[AC_REQUIRE([CL_PROG_LN])dnl
dnl Make a symlink if possible; otherwise try a hard link. On filesystems
dnl which support neither symlink nor hard link, use a plain copy.
AC_CACHE_CHECK([whether ln -s works], [cl_cv_prog_LN_S_works], [dnl
rm -f conftestdata
if ln -s X conftestdata 2>/dev/null; then
  cl_cv_prog_LN_S_works=yes
else
  cl_cv_prog_LN_S_works=no
fi
rm -f conftestdata
])
if test $cl_cv_prog_LN_S_works = yes; then
  LN_S="ln -s"
else
  LN_S="$cl_cv_prog_LN"
fi
AC_SUBST(LN_S)])

AC_DEFUN([CL_PROG_HLN],
[AC_REQUIRE([CL_PROG_LN_S])dnl
dnl according to the Linux ln(1):
dnl   "making a hard link to a symbolic link is not portable":
dnl SVR4 (Solaris, Linux) create symbolic links
dnl   (breaks when the target is relative)
dnl Cygwin (1.3.12) is even worse: it makes hard links to the symbolic link,
dnl   instead of resolving the symbolic link.
dnl Good behavior means creating a hard link to the symbolic link's target.
dnl To avoid this, use the "hln" program.
dnl cf gl_AC_FUNC_LINK_FOLLOWS_SYMLINK in gnulib/m4/link-follow.m4
AC_CACHE_CHECK(how to make hard links to symlinks, cl_cv_prog_hln, [
cl_cv_prog_hln="ln"
if test "$cl_cv_prog_LN_S_works" = "yes"; then
echo "blabla" > conftest.x
ln -s conftest.x conftest.y
mkdir conftest.d
cd conftest.d
ln ../conftest.y conftest.z 2>&AC_FD_CC
data=`cat conftest.z 2>/dev/null`
if test "$data" = "blabla" ; then
  # conftest.z contains the correct data -- good!
  cl_cv_prog_hln="ln"
else
  # ln cannot link to symbolic links
  cl_cv_prog_hln="hln"
fi
cd ..
rm -fr conftest.*
else
# If there are no symbolic links, the problem cannot occur.
cl_cv_prog_hln="ln"
fi
])
HLN="$cl_cv_prog_hln"
AC_SUBST(HLN)dnl
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_MACH_VM],
[CL_LINK_CHECK([vm_allocate], cl_cv_func_vm,
 , [vm_allocate(); task_self();],
AC_DEFINE(HAVE_MACH_VM,,[have vm_allocate() and task_self() functions])dnl
)])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_MMAP],
[AC_BEFORE([$0], [CL_MUNMAP])AC_BEFORE([$0], [CL_MPROTECT])
AC_CHECK_HEADER(sys/mman.h, , no_mmap=1)dnl
if test -z "$no_mmap"; then
AC_CHECK_FUNC(mmap, , no_mmap=1)dnl
if test -z "$no_mmap"; then
AC_DEFINE(HAVE_MMAP,,[have <sys/mmap.h> and the mmap() function])
AC_CACHE_CHECK(for working mmap, cl_cv_func_mmap_works, [
case "$host" in
  i[34567]86-*-sysv4*)
    # UNIX_SYSV_UHC_1
    avoid=0x08000000 ;;
  mips-sgi-irix* | mips-dec-ultrix*)
    # UNIX_IRIX, UNIX_DEC_ULTRIX
    avoid=0x10000000 ;;
  rs6000-ibm-aix*)
    # UNIX_AIX
    avoid=0x20000000 ;;
  *)
    avoid=0 ;;
esac
mmap_prog_1='
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
int main () {
'
mmap_prog_2="#define bits_to_avoid $avoid"'
#define my_shift 24
#define my_low   1
#ifdef FOR_SUN4_29
#define my_high  31
#define my_size  32768 /* hope that 32768 is a multiple of the page size */
/* i*32 KB for i=1..31 gives a total of 15.5 MB, which is close to what we need */
#else
#define my_high  64
#define my_size  8192 /* hope that 8192 is a multiple of the page size */
/* i*8 KB for i=1..64 gives a total of 16.25 MB, which is close to what we need */
#endif
 {long i;
#define i_ok(i)  ((i) & (bits_to_avoid >> my_shift) == 0)
  for (i=my_low; i<=my_high; i++)
    if (i_ok(i))
      { caddr_t addr = (caddr_t)(i << my_shift);
/* Check for 8 MB, not 16 MB. This is more likely to work on Solaris 2. */
#if bits_to_avoid
        long size = i*my_size;
#else
        long size = ((i+1)/2)*my_size;
#endif
        if (mmap(addr,size,PROT_READ|PROT_WRITE,flags|MAP_FIXED,fd,0) == (void*)-1) exit(1);
    }
#define x(i)  *(unsigned char *) ((i<<my_shift) + (i*i))
#define y(i)  (unsigned char)((3*i-4)*(7*i+3))
  for (i=my_low; i<=my_high; i++) if (i_ok(i)) { x(i) = y(i); }
  for (i=my_high; i>=my_low; i--) if (i_ok(i)) { if (x(i) != y(i)) exit(1); }
  exit(0);
}}
'
AC_TRY_RUN(GL_NOCRASH[$mmap_prog_1
  int flags = MAP_ANON | MAP_PRIVATE;
  int fd = -1;
  nocrash_init();
$mmap_prog_2
], have_mmap_anon=1
cl_cv_func_mmap_anon=yes, ,
: # When cross-compiling, don't assume anything.
)
AC_TRY_RUN(GL_NOCRASH[$mmap_prog_1
  int flags = MAP_ANONYMOUS | MAP_PRIVATE;
  int fd = -1;
  nocrash_init();
$mmap_prog_2
], have_mmap_anon=1
cl_cv_func_mmap_anonymous=yes, ,
: # When cross-compiling, don't assume anything.
)
AC_TRY_RUN(GL_NOCRASH[$mmap_prog_1
#ifndef MAP_FILE
#define MAP_FILE 0
#endif
  int flags = MAP_FILE | MAP_PRIVATE;
  int fd = open("/dev/zero",O_RDONLY,0666);
  if (fd<0) exit(1);
  nocrash_init();
$mmap_prog_2
], have_mmap_devzero=1
cl_cv_func_mmap_devzero=yes, 
retry_mmap=1,
: # When cross-compiling, don't assume anything.
)
if test -n "$retry_mmap"; then
AC_TRY_RUN(GL_NOCRASH[#define FOR_SUN4_29
$mmap_prog_1
#ifndef MAP_FILE
#define MAP_FILE 0
#endif
  int flags = MAP_FILE | MAP_PRIVATE;
  int fd = open("/dev/zero",O_RDONLY,0666);
  if (fd<0) exit(1);
  nocrash_init();
$mmap_prog_2
], have_mmap_devzero=1
cl_cv_func_mmap_devzero_sun4_29=yes, ,
: # When cross-compiling, don't assume anything.
)
fi
if test -n "$have_mmap_anon" -o -n "$have_mmap_devzero"; then
cl_cv_func_mmap_works=yes
else
cl_cv_func_mmap_works=no
fi
])
if test "$cl_cv_func_mmap_anon" = yes; then
AC_DEFINE(HAVE_MMAP_ANON,,[<sys/mman.h> defines MAP_ANON and mmaping with MAP_ANON works])
fi
if test "$cl_cv_func_mmap_anonymous" = yes; then
AC_DEFINE(HAVE_MMAP_ANONYMOUS,,[<sys/mman.h> defines MAP_ANONYMOUS and mmaping with MAP_ANONYMOUS works])
fi
if test "$cl_cv_func_mmap_devzero" = yes; then
AC_DEFINE(HAVE_MMAP_DEVZERO,,[mmaping of the special device /dev/zero works])
fi
if test "$cl_cv_func_mmap_devzero_sun4_29" = yes; then
AC_DEFINE(HAVE_MMAP_DEVZERO_SUN4_29,,[mmaping of the special device /dev/zero works, but only on addresses < 2^29])
fi
fi
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_MPROTECT],
[AC_REQUIRE([CL_GETPAGESIZE])dnl
AC_REQUIRE([CL_MMAP])dnl
AC_CHECK_FUNCS(mprotect)dnl
if test $ac_cv_func_mprotect = yes; then
AC_CACHE_CHECK(for working mprotect, cl_cv_func_mprotect_works, [
mprotect_prog='
#include <sys/types.h>
/* declare malloc() */
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
/* declare getpagesize() and mprotect() */
#include <sys/mman.h>
#ifndef HAVE_GETPAGESIZE
#include <sys/param.h>
#define getpagesize() PAGESIZE
#else
]AC_LANG_EXTERN[
RETGETPAGESIZETYPE getpagesize (void);
#endif
char foo;
int main () {
  unsigned long pagesize = getpagesize();
#define page_align(address)  (char*)((unsigned long)(address) & -pagesize)
'
AC_TRY_RUN([$mprotect_prog
  if ((pagesize-1) & pagesize) exit(1);
  exit(0); }], , no_mprotect=1,
# When cross-compiling, don't assume anything.
no_mprotect=1)
mprotect_prog="$mprotect_prog"'
  char* area = (char*) malloc(6*pagesize);
  char* fault_address = area + pagesize*7/2;
'
if test -z "$no_mprotect"; then
AC_TRY_RUN(GL_NOCRASH[$mprotect_prog
  nocrash_init();
  if (mprotect(page_align(fault_address),pagesize,PROT_NONE) < 0) exit(0);
  foo = *fault_address; /* this should cause an exception or signal */
  exit(0); }], no_mprotect=1, ,
: # When cross-compiling, don't assume anything.
)
fi
if test -z "$no_mprotect"; then
AC_TRY_RUN(GL_NOCRASH[$mprotect_prog
  nocrash_init();
  if (mprotect(page_align(fault_address),pagesize,PROT_NONE) < 0) exit(0);
  *fault_address = 'z'; /* this should cause an exception or signal */
  exit(0); }], no_mprotect=1, ,
: # When cross-compiling, don't assume anything.
)
fi
if test -z "$no_mprotect"; then
AC_TRY_RUN(GL_NOCRASH[$mprotect_prog
  nocrash_init();
  if (mprotect(page_align(fault_address),pagesize,PROT_READ) < 0) exit(0);
  *fault_address = 'z'; /* this should cause an exception or signal */
  exit(0); }], no_mprotect=1, ,
: # When cross-compiling, don't assume anything.
)
fi
if test -z "$no_mprotect"; then
AC_TRY_RUN(GL_NOCRASH[$mprotect_prog
  nocrash_init();
  if (mprotect(page_align(fault_address),pagesize,PROT_READ) < 0) exit(1);
  if (mprotect(page_align(fault_address),pagesize,PROT_READ|PROT_WRITE) < 0) exit(1);
  *fault_address = 'z'; /* this should not cause an exception or signal */
  exit(0); }], , no_mprotect=1,
: # When cross-compiling, don't assume anything.
)
fi
if test -z "$no_mprotect"; then
  cl_cv_func_mprotect_works=yes
else
  cl_cv_func_mprotect_works=no
fi
])
if test $cl_cv_func_mprotect_works = yes; then
  AC_DEFINE(HAVE_WORKING_MPROTECT,,[have a working mprotect() function])
fi
fi
])

dnl Copyright (C) 1993-2002 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels.

AC_PREREQ(2.13)

AC_DEFUN([CL_MSYNC],
[AC_REQUIRE([CL_MMAP])dnl
if test -z "$no_mmap"; then
AC_CHECK_FUNCS(msync)dnl
fi
])

dnl Copyright (C) 1993-2002 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels.

AC_PREREQ(2.13)

AC_DEFUN([CL_MUNMAP],
[AC_REQUIRE([CL_MMAP])dnl
if test -z "$no_mmap"; then
AC_CHECK_FUNCS(munmap)dnl
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.61)

dnl CL_MACHINE([MESSAGE], [PROGRAM_TO_RUN], [CROSS_MACRO], [DESTINATION], [CACHE_VAR])
AC_DEFUN([CL_MACHINE],
[AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_C_CHAR_UNSIGNED])dnl
cl_machine_file_c=$2
if test -z "$[$5]"; then
AC_MSG_CHECKING(for [$1])
cl_machine_file_h=$4
ORIGCC="$CC"
if test $ac_cv_prog_gcc = yes; then
# gcc -O (gcc version <= 2.3.2) crashes when compiling long long shifts for
# target 80386. Strip "-O".
CC=`echo "$CC " | sed -e 's/-O //g'`
fi
cl_machine_file_program=`cat "$cl_machine_file_c"`
AC_RUN_IFELSE([#include "confdefs.h"
$cl_machine_file_program],[AC_MSG_RESULT(created $cl_machine_file_h)
if cmp -s "$cl_machine_file_h" conftest.h 2>/dev/null; then
  # The file exists and we would not be changing it
  rm -f conftest.h
else
  rm -f "$cl_machine_file_h"
  mv conftest.h "$cl_machine_file_h"
fi
[$5]=1],[AC_MSG_RESULT(creation of $cl_machine_file_h failed)],
[AC_MSG_RESULT(creating $cl_machine_file_h)
$3([$4])])
rm -f conftest.h
CC="$ORIGCC"
fi
])

# pkg.m4 - Macros to locate and utilise pkg-config.            -*- Autoconf -*-
# 
# Copyright © 2004 Scott James Remnant <scott@netsplit.com>.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
# As a special exception to the GNU General Public License, if you
# distribute this file as part of a program that contains a
# configuration script generated by Autoconf, you may include it under
# the same distribution terms that you use for the rest of that program.

# PKG_PROG_PKG_CONFIG([MIN-VERSION])
# ----------------------------------
AC_DEFUN([PKG_PROG_PKG_CONFIG],
[m4_pattern_forbid([^_?PKG_[A-Z_]+$])
m4_pattern_allow([^PKG_CONFIG(_PATH)?$])
AC_ARG_VAR([PKG_CONFIG], [path to pkg-config utility])dnl
if test "x$ac_cv_env_PKG_CONFIG_set" != "xset"; then
	AC_PATH_TOOL([PKG_CONFIG], [pkg-config])
fi
if test -n "$PKG_CONFIG"; then
	_pkg_min_version=m4_default([$1], [0.9.0])
	AC_MSG_CHECKING([pkg-config is at least version $_pkg_min_version])
	if $PKG_CONFIG --atleast-pkgconfig-version $_pkg_min_version; then
		AC_MSG_RESULT([yes])
	else
		AC_MSG_RESULT([no])
		PKG_CONFIG=""
	fi
		
fi[]dnl
])# PKG_PROG_PKG_CONFIG

# PKG_CHECK_EXISTS(MODULES, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
#
# Check to see whether a particular set of modules exists.  Similar
# to PKG_CHECK_MODULES(), but does not set variables or print errors.
#
#
# Similar to PKG_CHECK_MODULES, make sure that the first instance of
# this or PKG_CHECK_MODULES is called, or make sure to call
# PKG_CHECK_EXISTS manually
# --------------------------------------------------------------
AC_DEFUN([PKG_CHECK_EXISTS],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
if test -n "$PKG_CONFIG" && \
    AC_RUN_LOG([$PKG_CONFIG --exists --print-errors "$1"]); then
  m4_ifval([$2], [$2], [:])
m4_ifvaln([$3], [else
  $3])dnl
fi])


# _PKG_CONFIG([VARIABLE], [COMMAND], [MODULES])
# ---------------------------------------------
m4_define([_PKG_CONFIG],
[if test -n "$$1"; then
    pkg_cv_[]$1="$$1"
 elif test -n "$PKG_CONFIG"; then
    PKG_CHECK_EXISTS([$3],
                     [pkg_cv_[]$1=`$PKG_CONFIG --[]$2 "$3" 2>/dev/null`],
		     [pkg_failed=yes])
 else
    pkg_failed=untried
fi[]dnl
])# _PKG_CONFIG

# _PKG_SHORT_ERRORS_SUPPORTED
# -----------------------------
AC_DEFUN([_PKG_SHORT_ERRORS_SUPPORTED],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])
if $PKG_CONFIG --atleast-pkgconfig-version 0.20; then
        _pkg_short_errors_supported=yes
else
        _pkg_short_errors_supported=no
fi[]dnl
])# _PKG_SHORT_ERRORS_SUPPORTED


# PKG_CHECK_MODULES(VARIABLE-PREFIX, MODULES, [ACTION-IF-FOUND],
# [ACTION-IF-NOT-FOUND])
#
#
# Note that if there is a possibility the first call to
# PKG_CHECK_MODULES might not happen, you should be sure to include an
# explicit call to PKG_PROG_PKG_CONFIG in your configure.ac
#
#
# --------------------------------------------------------------
AC_DEFUN([PKG_CHECK_MODULES],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
AC_ARG_VAR([$1][_CFLAGS], [C compiler flags for $1, overriding pkg-config])dnl
AC_ARG_VAR([$1][_LIBS], [linker flags for $1, overriding pkg-config])dnl

pkg_failed=no
AC_MSG_CHECKING([for $1])

_PKG_CONFIG([$1][_CFLAGS], [cflags], [$2])
_PKG_CONFIG([$1][_LIBS], [libs], [$2])

m4_define([_PKG_TEXT], [Alternatively, you may set the environment variables $1[]_CFLAGS
and $1[]_LIBS to avoid the need to call pkg-config.
See the pkg-config man page for more details.])

if test $pkg_failed = yes; then
        _PKG_SHORT_ERRORS_SUPPORTED
        if test $_pkg_short_errors_supported = yes; then
	        $1[]_PKG_ERRORS=`$PKG_CONFIG --short-errors --print-errors "$2" 2>&1`
        else 
	        $1[]_PKG_ERRORS=`$PKG_CONFIG --print-errors "$2" 2>&1`
        fi
	# Put the nasty error message in config.log where it belongs
	echo "$$1[]_PKG_ERRORS" >&AS_MESSAGE_LOG_FD

	ifelse([$4], , [AC_MSG_ERROR(dnl
[Package requirements ($2) were not met:

$$1_PKG_ERRORS

Consider adjusting the PKG_CONFIG_PATH environment variable if you
installed software in a non-standard prefix.

_PKG_TEXT
])],
		[AC_MSG_RESULT([no])
                $4])
elif test $pkg_failed = untried; then
	ifelse([$4], , [AC_MSG_FAILURE(dnl
[The pkg-config script could not be found or is too old.  Make sure it
is in your PATH or set the PKG_CONFIG environment variable to the full
path to pkg-config.

_PKG_TEXT

To get pkg-config, see <http://pkg-config.freedesktop.org/>.])],
		[$4])
else
	$1[]_CFLAGS=$pkg_cv_[]$1[]_CFLAGS
	$1[]_LIBS=$pkg_cv_[]$1[]_LIBS
        AC_MSG_RESULT([yes])
	ifelse([$3], , :, [$3])
fi[]dnl
])# PKG_CHECK_MODULES

dnl -*- Autoconf -*-
dnl Copyright (C) 2004-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible.

AC_PREREQ(2.57)

AC_DEFUN([CL_POLL],
[AC_CHECK_FUNC(poll,
  [# Check whether poll() works on special files (like /dev/null)
   # and ttys (like /dev/tty). On MacOS X 10.4.0, it doesn't.
   AC_TRY_RUN([
#include <fcntl.h>
#include <poll.h>
     int main()
     {
       struct pollfd ufd;
       /* Try /dev/null for reading. */
       ufd.fd = open ("/dev/null", O_RDONLY);
       if (ufd.fd < 0) /* If /dev/null does not exist, it's not MacOS X. */
         return 0;
       ufd.events = POLLIN;
       ufd.revents = 0;
       if (!(poll (&ufd, 1, 0) == 1 && ufd.revents == POLLIN))
         return 1;
       /* Try /dev/null for writing. */
       ufd.fd = open ("/dev/null", O_WRONLY);
       if (ufd.fd < 0) /* If /dev/null does not exist, it's not MacOS X. */
         return 0;
       ufd.events = POLLOUT;
       ufd.revents = 0;
       if (!(poll (&ufd, 1, 0) == 1 && ufd.revents == POLLOUT))
         return 1;
       /* Trying /dev/tty may be too environment dependent. */
       return 0;
     }],
     [cl_cv_func_poll=yes],
     [cl_cv_func_poll=no],
     [# When cross-compiling, assume that poll() works everywhere except on
      # MacOS X, regardless of its version.
      AC_EGREP_CPP([MacOSX], [
#if defined(__APPLE__) && defined(__MACH__)
This is MacOSX
#endif
], [cl_cv_func_poll=no], [cl_cv_func_poll=yes])])])
dnl if AC_CHECK_FUNC does not find poll, cl_cv_func_poll will not be set
if test x$cl_cv_func_poll = xyes; then
  AC_DEFINE([HAVE_POLL], 1,
    [Define to 1 if you have the 'poll' function and it works.])
# Now check whether poll() works reliably on regular files, i.e. signals
# immediate readability and writability, both before EOF and at EOF.
# On FreeBSD 4.0, it doesn't.
AC_CACHE_CHECK([for reliable poll()], cl_cv_func_poll_reliable, [
AC_TRY_RUN([
/* Declare poll(). */
#include <poll.h>
/* Declare open(). */
#include <fcntl.h>
/* Declare lseek(). */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
int main ()
{ int fd = open("conftest.c",O_RDWR,0644);
  int correct_readability_nonempty, correct_readability_empty;
  int correct_writability_nonempty, correct_writability_empty;
  struct pollfd pollfd_bag[1];
  {
    pollfd_bag[0].fd = fd;
    pollfd_bag[0].events = POLLIN;
    pollfd_bag[0].revents = 0;
    correct_readability_nonempty =
      (poll(&pollfd_bag[0],1,0) >= 0 && pollfd_bag[0].revents != 0);
  }
  {
    pollfd_bag[0].fd = fd;
    pollfd_bag[0].events = POLLOUT;
    pollfd_bag[0].revents = 0;
    correct_writability_nonempty =
      (poll(&pollfd_bag[0],1,0) >= 0 && pollfd_bag[0].revents != 0);
  }
  lseek(fd,0,SEEK_END);
  {
    pollfd_bag[0].fd = fd;
    pollfd_bag[0].events = POLLIN;
    pollfd_bag[0].revents = 0;
    correct_readability_empty =
      (poll(&pollfd_bag[0],1,0) >= 0 && pollfd_bag[0].revents != 0);
  }
  {
    pollfd_bag[0].fd = fd;
    pollfd_bag[0].events = POLLOUT;
    pollfd_bag[0].revents = 0;
    correct_writability_empty =
      (poll(&pollfd_bag[0],1,0) >= 0 && pollfd_bag[0].revents != 0);
  }
  exit(!(correct_readability_nonempty && correct_readability_empty
         && correct_writability_nonempty && correct_writability_empty));
}],
cl_cv_func_poll_reliable=yes, cl_cv_func_poll_reliable=no,
dnl When cross-compiling, don't assume anything.
cl_cv_func_poll_reliable="guessing no")
])
case "$cl_cv_func_poll_reliable" in
  *yes) AC_DEFINE(HAVE_RELIABLE_POLL,,[have poll() and it works reliably on files]) ;;
  *no) ;;
esac
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2002, 2007-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.13)

dnl CL_PROTO(IDENTIFIER, ACTION-IF-NOT-FOUND, FINAL-PROTOTYPE)
AC_DEFUN([CL_PROTO],
[AC_MSG_CHECKING([for $1 declaration])
AC_CACHE_VAL(cl_cv_proto_[$1], [$2
cl_cv_proto_$1="$3"])
cl_cv_proto_$1=`echo "[$]cl_cv_proto_$1" | tr -s ' ' | sed -e 's/( /(/'`
AC_MSG_RESULT([$]{ac_t:-
         }[$]cl_cv_proto_$1)
])

dnl CL_PROTO_RET(INCLUDES, DECL, CACHE-ID, TYPE-IF-OK, TYPE-IF-FAILS)
AC_DEFUN([CL_PROTO_RET],
[AC_TRY_COMPILE([$1]
AC_LANG_EXTERN
[$2
], [], $3="$4", $3="$5")
])

dnl CL_PROTO_TRY(INCLUDES, DECL, ACTION-IF-OK, ACTION-IF-FAILS)
AC_DEFUN([CL_PROTO_TRY],
[AC_TRY_COMPILE([$1]
AC_LANG_EXTERN
[$2
], [], [$3], [$4])
])

dnl CL_PROTO_CONST(INCLUDES, DECL, CACHE-ID)
AC_DEFUN([CL_PROTO_CONST],
[CL_PROTO_TRY([$1], [$2], $3="", $3="const")]
)

dnl CL_PROTO_MISSING(function_name)
AC_DEFUN([CL_PROTO_MISSING],
[AC_MSG_FAILURE([please report the $1() declaration on your platform to $PACKAGE_NAME developers at $PACKAGE_BUGREPORT])])

m4_define([CONST_VARIANTS],['' 'const' '__const'])
m4_define([SIZE_VARIANTS],['unsigned int' 'int' 'unsigned long' 'long' 'size_t' 'socklen_t'])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_PUTENV],
[AC_REQUIRE([AC_GNU_SOURCE])
dnl Not AC_CHECK_FUNCS(putenv) because it doesn't work when CC=g++.
AC_CACHE_CHECK([for putenv], ac_cv_func_putenv, [
AC_LINK_IFELSE(AC_LANG_PROGRAM([#include <stdlib.h>], [putenv("")]),
ac_cv_func_putenv=yes, ac_cv_func_putenv=no)])
if test $ac_cv_func_putenv = yes; then
  AC_DEFINE(HAVE_PUTENV, 1, [Define if you have the putenv() function.])
fi
AC_CHECK_FUNCS(setenv unsetenv)dnl
AC_CHECK_DECLS(environ,,,[#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif])
if test "$ac_cv_func_unsetenv" = yes; then
  AC_MSG_CHECKING(return value of unsetenv)
  CL_PROTO_RET([#include <stdlib.h>],[int unsetenv(char*);],
cl_cv_proto_unsetenv_ret,int,void)
  AC_MSG_RESULT($cl_cv_proto_unsetenv_ret)
  if test "$cl_cv_proto_unsetenv_ret" = int;
  then cl_cv_proto_unsetenv_posix=1
  else cl_cv_proto_unsetenv_posix=0
  fi
  AC_DEFINE_UNQUOTED(UNSETENV_POSIX,$cl_cv_proto_unsetenv_posix,
  [define to 1 if the return type of unsetenv is int])
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 2002-2008 Sam Steingold, Bruno Haible
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

AC_PREREQ(2.57)

AC_DEFUN([CL_READLINE],[dnl
AC_ARG_WITH([readline],
AC_HELP_STRING([--with-readline],[use readline (default is YES, if present)]),
[ac_cv_use_readline=$withval], [ac_cv_use_readline=default])
ac_cv_have_readline=no
if test "$ac_cv_use_readline" = "no" ; then
AC_MSG_NOTICE([not checking for readline])
else
AC_REQUIRE([CL_TERMCAP])dnl
if test $ac_cv_search_tgetent != no ; then
 AC_LIB_LINKFLAGS_BODY(readline)
 ac_save_CPPFLAGS="$CPPFLAGS"
 CPPFLAGS="$CPPFLAGS $INCREADLINE"
 AC_CHECK_HEADERS(readline/readline.h)
 if test "$ac_cv_header_readline_readline_h" != yes; then
  CPPFLAGS="$ac_save_CPPFLAGS"
 else # have <readline/readline.h> => check library
  ac_save_LIBS="$LIBS"
  LIBS="$LIBREADLINE $LIBS"
  AC_CHECK_FUNC(readline)dnl do not define HAVE_READLINE
  AC_CHECK_FUNCS(rl_filename_completion_function)
  if test "$ac_cv_func_readline" != yes ; then
    LIBS="$ac_save_LIBS"
    CPPFLAGS="$ac_save_CPPFLAGS"
  else # have readline => check modern features
    if test $ac_cv_func_rl_filename_completion_function = no ;
    then RL_FCF=filename_completion_function
    else RL_FCF=rl_filename_completion_function
    fi
    dnl READLINE_CONST is necessary for C++ compilation of stream.d
    CL_PROTO([filename_completion_function], [
      CL_PROTO_CONST([
#include <stdio.h>
#include <readline/readline.h>
      ],[char* ${RL_FCF} (char *, int);],
      cl_cv_proto_readline_const) ],
      [extern char* ${RL_FCF}($cl_cv_proto_readline_const char*, int);])
    AC_CHECK_DECLS([rl_already_prompted, rl_readline_name, rl_gnu_readline_p],,,
[#include <stdio.h>
#include <readline/readline.h>])
    AC_MSG_CHECKING(for a modern readline)
    if test "$ac_cv_have_decl_rl_already_prompted" = yes \
         -a "$ac_cv_have_decl_rl_gnu_readline_p" = yes; then
      dnl LIBREADLINE has been added to LIBS.
      AC_MSG_RESULT([found a modern GNU readline])
      ac_cv_have_readline=yes
    else
      AC_MSG_RESULT([readline is too old and will not be used])
      LIBS="$ac_save_LIBS"
      CPPFLAGS="$ac_save_CPPFLAGS"
    fi
  fi
 fi
fi
if test "$ac_cv_have_readline" = yes; then
  AC_DEFINE_UNQUOTED(READLINE_FILE_COMPLETE,${RL_FCF},[The readline built-in filename completion function, either rl_filename_completion_function() or filename_completion_function()])
  AC_DEFINE_UNQUOTED(READLINE_CONST,$cl_cv_proto_readline_const,[declaration of filename_completion_function() needs const in the first argument])
  AC_DEFINE(HAVE_READLINE,,[have a working modern GNU readline])
elif test "$ac_cv_use_readline" = yes; then
  AC_MSG_FAILURE([despite --with-readline, GNU readline was not found (try --with-libreadline-prefix)])
fi
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_RLIMIT],
[AC_CHECK_FUNCS(getrlimit setrlimit)dnl
if test $ac_cv_func_setrlimit = yes; then
AC_CHECK_SIZEOF(rlim_t,,[#include <stdio.h>
#include <sys/resource.h>])
CL_PROTO([getrlimit], [
CL_PROTO_TRY([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
],
[int getrlimit (int resource, struct rlimit * rlim);],
[cl_cv_proto_getrlimit_arg1="int"],
[cl_cv_proto_getrlimit_arg1="enum __rlimit_resource"])
], [extern int getrlimit ($cl_cv_proto_getrlimit_arg1, struct rlimit *);])
AC_DEFINE_UNQUOTED(RLIMIT_RESOURCE_T,$cl_cv_proto_getrlimit_arg1,[type of `resource' in setrlimit() declaration])
CL_PROTO([setrlimit], [
CL_PROTO_CONST([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
], [int setrlimit (RLIMIT_RESOURCE_T resource, struct rlimit * rlim);],
cl_cv_proto_setrlimit_arg2)
], [extern int setrlimit ($cl_cv_proto_getrlimit_arg1, $cl_cv_proto_setrlimit_arg2 struct rlimit *);])
AC_DEFINE_UNQUOTED(SETRLIMIT_CONST,$cl_cv_proto_setrlimit_arg2,[declaration of setrlimit() needs const])
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold, Peter Burwood.

AC_PREREQ(2.57)

AC_DEFUN([CL_RUSAGE],
[AC_CHECK_HEADERS(sys/resource.h sys/times.h)dnl
if test $ac_cv_header_sys_resource_h = yes; then
  dnl HAVE_SYS_RESOURCE_H defined
  CL_LINK_CHECK([getrusage], cl_cv_func_getrusage,
[#include <sys/types.h> /* NetBSD 1.0 needs this */
#include <sys/time.h>
#include <sys/resource.h>],
    [struct rusage x; int y = RUSAGE_SELF; getrusage(y,&x); x.ru_utime.tv_sec;])dnl
  if test $cl_cv_func_getrusage = yes; then
    CL_PROTO([getrusage], [CL_PROTO_TRY([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h> /* NetBSD 1.0 needs this */
#include <sys/time.h>
#include <sys/resource.h>
],
[int getrusage (int who, struct rusage * rusage);],
[cl_cv_proto_getrusage_arg1="int"],
[cl_cv_proto_getrusage_arg1="enum __rusage_who"])
], [extern int getrusage ($cl_cv_proto_getrusage_arg1, struct rusage *);])dnl
    AC_CACHE_CHECK(whether getrusage works, cl_cv_func_getrusage_works, [
    AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <stdio.h>
#include <sys/types.h> /* NetBSD 1.0 needs this */
#include <sys/time.h>
#include <time.h> /* for time(2) */
#include <sys/resource.h>
int main ()
{
  struct rusage used, prev;
  time_t end = time(NULL)+2;
  int count = 0;

  if ((count = getrusage(RUSAGE_SELF, &prev))) {
    /* getrusage is defined but does not do anything. */
    /*fprintf(stderr,"getrusage failed: return=%d\n",count);*/
    return 1;
  }
  while (time(NULL) < end) {
    count++;
    getrusage(RUSAGE_SELF, &used);
    if ((used.ru_utime.tv_usec != prev.ru_utime.tv_usec)
        || (used.ru_utime.tv_sec != prev.ru_utime.tv_sec)
        || (used.ru_stime.tv_usec != prev.ru_stime.tv_usec)
        || (used.ru_stime.tv_sec != prev.ru_stime.tv_sec)) {
      /*fprintf(stderr,"success after %d runs\n",count);*/
      return 0;
    }
  }
  /* getrusage is defined but does not work. */
  /*fprintf(stderr,"failure after %d runs\n",count);*/
  return 1;
}]])],[cl_cv_func_getrusage_works=yes],[cl_cv_func_getrusage_works=no],
dnl When cross-compiling, don't assume anything.
[cl_cv_func_getrusage_works="guessing no"])])
  fi
  if test "$cl_cv_func_getrusage_works" = yes; then
    AC_DEFINE(HAVE_GETRUSAGE,,[have <sys/time.h>, the getrusage() function, the struct rusage type, and <sys/resource.h> defines RUSAGE_SELF])
    AC_DEFINE_UNQUOTED(RUSAGE_WHO_T,$cl_cv_proto_getrusage_arg1,[type of `who' in getrusage() declaration])
  fi
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2004, 2007-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_SELECT],
[dnl Not AC_CHECK_FUNCS(select) because it doesn't work when CC=g++.
AC_CACHE_CHECK([for select], ac_cv_func_select, [
AC_TRY_LINK([
#ifdef __BEOS__
#include <sys/socket.h>
#endif
#include <sys/time.h>
]AC_LANG_EXTERN[
#ifdef __cplusplus
int select(int, fd_set*, fd_set*, fd_set*, struct timeval *);
#else
int select();
#endif
], [select(0,(fd_set*)0,(fd_set*)0,(fd_set*)0,(struct timeval *)0);],
ac_cv_func_select=yes, ac_cv_func_select=no)])
if test $ac_cv_func_select = yes; then
AC_DEFINE(HAVE_SELECT, 1, [Define if you have the select() function.])
CL_COMPILE_CHECK([sys/select.h], cl_cv_header_sys_select_h,
[#ifdef __BEOS__
#include <sys/socket.h>
#endif
#include <sys/time.h>
#include <sys/select.h>], ,
AC_DEFINE(HAVE_SYS_SELECT_H,,[have <sys/select.h>?]))dnl
CL_PROTO([select], [
for z in CONST_VARIANTS; do
for y in 'fd_set' 'int' 'void' 'struct fd_set'; do
for x in SIZE_VARIANTS; do
if test -z "$have_select"; then
CL_PROTO_TRY([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef __BEOS__
#include <sys/socket.h>
#endif
#include <sys/time.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
], [int select ($x width, $y * readfds, $y * writefds, $y * exceptfds, $z struct timeval * timeout);],
[cl_cv_proto_select_arg1="$x"
cl_cv_proto_select_arg2="$y"
cl_cv_proto_select_arg5="$z"
have_select=1])
fi
done
done
done
if test -z "$have_select"; then
CL_PROTO_MISSING(select)
fi
], [extern int select ($cl_cv_proto_select_arg1, $cl_cv_proto_select_arg2 *, $cl_cv_proto_select_arg2 *, $cl_cv_proto_select_arg2 *, $cl_cv_proto_select_arg5 struct timeval *);])
AC_DEFINE_UNQUOTED(SELECT_WIDTH_T,$cl_cv_proto_select_arg1,[type of `width' in select() declaration])
AC_DEFINE_UNQUOTED(SELECT_SET_T,$cl_cv_proto_select_arg2,[type of `* readfds', `* writefds', `* exceptfds' in select() declaration])
AC_DEFINE_UNQUOTED(SELECT_CONST,$cl_cv_proto_select_arg5,[declaration of select() needs const in the fifth argument])
# Now check whether select() works reliably on regular files, i.e. signals
# immediate readability and writability, both before EOF and at EOF.
AC_CACHE_CHECK([for reliable select()], cl_cv_func_select_reliable, [
AC_TRY_RUN([
/* Declare select(). */
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef __BEOS__
#include <sys/socket.h>
#endif
#include <sys/time.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
]AC_LANG_EXTERN[
int select (SELECT_WIDTH_T, SELECT_SET_T*, SELECT_SET_T*, SELECT_SET_T*, SELECT_CONST struct timeval *);
/* Declare open(). */
#include <fcntl.h>
int main ()
{ int fd = open("conftest.c",O_RDWR,0644);
  int correct_readability_nonempty, correct_readability_empty;
  int correct_writability_nonempty, correct_writability_empty;
  fd_set handle_set;
  struct timeval zero_time;
  {
    FD_ZERO(&handle_set); FD_SET(fd,&handle_set);
    zero_time.tv_sec = 0; zero_time.tv_usec = 0;
    correct_readability_nonempty =
      (select(FD_SETSIZE,&handle_set,NULL,NULL,&zero_time) == 1);
  }
  {
    FD_ZERO(&handle_set); FD_SET(fd,&handle_set);
    zero_time.tv_sec = 0; zero_time.tv_usec = 0;
    correct_writability_nonempty =
      (select(FD_SETSIZE,NULL,&handle_set,NULL,&zero_time) == 1);
  }
  lseek(fd,0,SEEK_END);
  {
    FD_ZERO(&handle_set); FD_SET(fd,&handle_set);
    zero_time.tv_sec = 0; zero_time.tv_usec = 0;
    correct_readability_empty =
      (select(FD_SETSIZE,&handle_set,NULL,NULL,&zero_time) == 1);
  }
  {
    FD_ZERO(&handle_set); FD_SET(fd,&handle_set);
    zero_time.tv_sec = 0; zero_time.tv_usec = 0;
    correct_writability_empty =
      (select(FD_SETSIZE,NULL,&handle_set,NULL,&zero_time) == 1);
  }
  exit(!(correct_readability_nonempty && correct_readability_empty
         && correct_writability_nonempty && correct_writability_empty));
}],
cl_cv_func_select_reliable=yes, cl_cv_func_select_reliable=no,
dnl When cross-compiling, don't assume anything.
cl_cv_func_select_reliable="guessing no")
])
case "$cl_cv_func_select_reliable" in
  *yes) AC_DEFINE(HAVE_RELIABLE_SELECT,,[have select() and it works reliably on files]) ;;
  *no) ;;
esac
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_SETJMP],
[AC_CHECK_FUNC(_setjmp, , no__jmp=1)dnl
if test -z "$no__jmp"; then
AC_CHECK_FUNC(_longjmp, , no__jmp=1)dnl
fi
if test -z "$no__jmp"; then
AC_DEFINE(HAVE__JMP,,[have _setjmp() and _longjmp()])
fi
AC_EGREP_HEADER([void.* longjmp], setjmp.h, , AC_DEFINE(LONGJMP_RETURNS,,[longjmp() may return]))
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_SHM_RMID],
[AC_REQUIRE([CL_SHM])dnl
if test "$cl_cv_sys_shm_works" = "yes"; then
AC_CACHE_CHECK(for attachability of removed shared memory, cl_cv_func_shmctl_attachable, [
AC_TRY_RUN([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#ifdef HAVE_SYS_SYSMACROS_H
#include <sys/sysmacros.h>
#endif
int main ()
{ unsigned int pagesize = 8192; /* should be a multiple of SHMLBA */
  unsigned long addr = (unsigned long) malloc(2*pagesize);
  addr += pagesize-1; addr = (addr/pagesize)*pagesize;
 {unsigned long addr1 = addr + 0x100000;
  unsigned long addr2 = addr + 0x200000;
  int id = shmget(IPC_PRIVATE,pagesize,IPC_CREAT|0600);
  if (id<0)
    { exit(1); }
  if (shmat(id,(void*)addr1,0) == (void*)(-1))
    { shmctl(id,IPC_RMID,NULL); exit(1); }
  if (shmctl(id,IPC_RMID,NULL) < 0)
    { exit(1); }
  if (shmat(id,(void*)addr2,0) == (void*)(-1))
    { shmctl(id,IPC_RMID,NULL); exit(1); }
  shmctl(id,IPC_RMID,NULL);
  exit(0);
}}
], cl_cv_func_shmctl_attachable=yes, cl_cv_func_shmctl_attachable=no,
dnl When cross-compiling, don't assume anything.
cl_cv_func_shmctl_attachable="guessing no")
])
case "$cl_cv_func_shmctl_attachable" in
  *yes) AC_DEFINE(SHM_RMID_VALID,,[attaching removed (but alive!) shared memory segments works]) ;;
  *no)  ;;
esac
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_SHM_H],
[AC_BEFORE([$0], [CL_SHM])dnl
AC_CHECK_HEADERS(sys/shm.h)
if test $ac_cv_header_sys_shm_h = yes; then
AC_CHECK_HEADERS(sys/ipc.h)
fi
])

AC_DEFUN([CL_SHM],
[AC_BEFORE([$0], [CL_SHM_RMID])dnl
AC_REQUIRE([CL_SHM_H])dnl
if test "$ac_cv_header_sys_shm_h" = yes -a "$ac_cv_header_sys_ipc_h" = yes; then
# This test is from Marcus Daniels
AC_CACHE_CHECK(for working shared memory, cl_cv_sys_shm_works, [
AC_TRY_RUN([#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
/* try attaching a single segment to multiple addresses */
#define segsize 0x10000
#define attaches 128
#define base_addr 0x01000000
int main ()
{ int shmid, i; char* addr; char* result;
  if ((shmid = shmget(IPC_PRIVATE,segsize,0400)) < 0) exit(1);
  for (i=0, addr = (char*)0x01000000; i<attaches; i++, addr += segsize)
    if ((result = (char*)shmat(shmid,addr,SHM_RDONLY)) == (char*)(-1)) break;
  for (i=0, addr = (char*)0x01000000; i<attaches; i++, addr += segsize)
    shmdt(addr);
  shmctl(shmid,IPC_RMID,0);
  exit(result == (char*)(-1));
}], cl_cv_sys_shm_works=yes, cl_cv_sys_shm_works=no,
dnl When cross-compiling, don't assume anything.
cl_cv_sys_shm_works="guessing no")
])
fi
case "$cl_cv_sys_shm_works" in
  *yes) AC_DEFINE(HAVE_SHM,,[have <sys/shm.h> and <sys/ipc.h> and shared memory works])
        AC_CHECK_HEADERS(sys/sysmacros.h)
        ;;
  *) ;;
esac
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_SIGINTERRUPT],
[AC_REQUIRE([CL_SIGACTION])dnl
AC_CHECK_FUNCS(siginterrupt)dnl
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_SIGNAL_REINSTALL],
[AC_BEFORE([$0], [CL_SIGNAL_UNBLOCK])dnl
AC_BEFORE([$0], [CL_SIGNAL_BLOCK_OTHERS])dnl
AC_CACHE_CHECK(whether signal handlers need to be reinstalled, cl_cv_func_signal_reinstall, [
AC_TRY_RUN([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __CYGWIN32__
/* On Cygwin32 version 18, this test would hang (SIGALRM not being signalled).
 * Let it fail instead. */
#error "better fail than hang"
#endif
volatile int gotsig=0;
void sigalrm_handler() { gotsig=1; }
int got_sig () { return gotsig; }
typedef void (*signal_handler_t) (int);
int main() { /* returns 0 if they need not to be reinstalled */
  signal(SIGALRM,(signal_handler_t)sigalrm_handler); alarm(1); while (!got_sig());
  exit(!( (signal_handler_t)signal(SIGALRM,(signal_handler_t)sigalrm_handler)
          == (signal_handler_t)sigalrm_handler
      ) );
}], cl_cv_func_signal_reinstall=no, cl_cv_func_signal_reinstall=yes,
dnl When cross-compiling, don't assume anything.
cl_cv_func_signal_reinstall="guessing yes")
])
case "$cl_cv_func_signal_reinstall" in
  *yes) AC_DEFINE(SIGNAL_NEED_REINSTALL,,[signal handlers need to be reinstalled when they are activated]) ;;
  *no) ;;
esac
])

AC_DEFUN([CL_SIGNAL_UNBLOCK],
[AC_REQUIRE([CL_SIGNAL_REINSTALL])dnl
case "$signalblocks" in
  *POSIX* | *BSD*)
AC_CACHE_CHECK(whether signals are blocked when signal handlers are entered, cl_cv_func_signal_blocked, [
AC_TRY_RUN([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __CYGWIN32__
/* On Cygwin32 version 18, this test would hang (SIGALRM not being signalled).
 * Let it fail instead. */
#error "better fail than hang"
#endif
volatile int gotsig=0;
volatile int wasblocked=0;
typedef void (*signal_handler_t) (int);
void sigalrm_handler()
{ gotsig=1;
#ifdef SIGNAL_NEED_REINSTALL
  signal(SIGALRM,(signal_handler_t)sigalrm_handler);
#endif
  { sigset_t blocked;
    sigprocmask(SIG_BLOCK, (sigset_t *) 0, &blocked);
    wasblocked = sigismember(&blocked,SIGALRM) ? 1 : 0;
  }
}
int got_sig () { return gotsig; }
int main() { /* returns 0 if they need not to be unblocked */
  signal(SIGALRM,(signal_handler_t)sigalrm_handler); alarm(1); while (!got_sig());
  exit(wasblocked);
}], cl_cv_func_signal_blocked=no, cl_cv_func_signal_blocked=yes,
dnl When cross-compiling, assume the worst case.
cl_cv_func_signal_blocked="guessing yes")
])
case "$cl_cv_func_signal_blocked" in
  *yes) AC_DEFINE(SIGNAL_NEED_UNBLOCK,,[SIGNALBLOCK_BSD is defined above and signals need to be unblocked when signal handlers are left]) ;;
  *no) ;;
esac
  ;;
  *) ;;
esac
])

AC_DEFUN([CL_SIGNAL_BLOCK_OTHERS],
[AC_REQUIRE([CL_SIGNAL_REINSTALL])dnl
case "$signalblocks" in
  *POSIX* | *BSD*)
AC_CACHE_CHECK(whether other signals are blocked when signal handlers are entered, cl_cv_func_signal_blocked_others, [
AC_TRY_RUN([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __CYGWIN32__
/* On Cygwin32 version 18, this test would hang (SIGALRM not being signalled).
 * Let it fail instead. */
#error "better fail than hang"
#endif
volatile int gotsig=0;
volatile int somewereblocked=0;
typedef void (*signal_handler_t) (int);
void sigalrm_handler()
{ gotsig=1;
#ifdef SIGNAL_NEED_REINSTALL
  signal(SIGALRM,(signal_handler_t)sigalrm_handler);
#endif
  { sigset_t blocked;
    int i;
    sigprocmask(SIG_BLOCK, (sigset_t *) 0, &blocked);
    for (i=1; i<32; i++)
      if (i!=SIGALRM && sigismember(&blocked,i))
        somewereblocked = 1;
  }
}
int got_sig () { return gotsig; }
int main() { /* returns 0 if they need not to be unblocked */
  signal(SIGALRM,(signal_handler_t)sigalrm_handler); alarm(1); while (!got_sig());
  exit(somewereblocked);
}], cl_cv_func_signal_blocked_others=no, cl_cv_func_signal_blocked_others=yes,
dnl When cross-compiling, assume the worst case.
cl_cv_func_signal_blocked_others="guessing yes")
])
case "$cl_cv_func_signal_blocked_others" in
  *yes) AC_DEFINE(SIGNAL_NEED_UNBLOCK_OTHERS,,[SIGNALBLOCK_BSD is defined above and other signals need to be unblocked when signal handlers are left]) ;;
  *no) ;;
esac
  ;;
  *) ;;
esac
])

AC_DEFUN([CL_SIGACTION],
[AC_BEFORE([$0], [CL_SIGACTION_REINSTALL])
AC_BEFORE([$0], [CL_SIGINTERRUPT])
AC_CHECK_FUNCS(sigaction)])

AC_DEFUN([CL_SIGACTION_REINSTALL],
[AC_REQUIRE([CL_SIGACTION])dnl
AC_BEFORE([$0], [CL_SIGACTION_UNBLOCK])dnl
if test -n "$have_sigaction"; then
AC_CACHE_CHECK(whether sigaction handlers need to be reinstalled, cl_cv_func_sigaction_reinstall, [
AC_TRY_RUN([
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __CYGWIN32__
/* On Cygwin32 version 18, this test would hang (SIGALRM not being signalled).
 * Let it fail instead. */
#error "better fail than hang"
#endif
typedef void (*signal_handler_t) (int);
signal_handler_t mysignal (int sig, signal_handler_t handler)
{ struct sigaction old_sa;
  struct sigaction new_sa;
  memset(&new_sa,0,sizeof(new_sa));
  new_sa.sa_handler = handler;
  if (sigaction(sig,&new_sa,&old_sa)<0) { return (signal_handler_t)SIG_IGN; }
  return (signal_handler_t)old_sa.sa_handler;
}
volatile int gotsig=0;
void sigalrm_handler() { gotsig=1; }
int got_sig () { return gotsig; }
int main() { /* returns 0 if they need not to be reinstalled */
  mysignal(SIGALRM,(signal_handler_t)sigalrm_handler); alarm(1); while (!got_sig());
  exit(!( mysignal(SIGALRM,(signal_handler_t)sigalrm_handler)
          == (signal_handler_t)sigalrm_handler
      ) );
}], cl_cv_func_sigaction_reinstall=no, cl_cv_func_sigaction_reinstall=yes,
dnl When cross-compiling, don't assume anything.
cl_cv_func_sigaction_reinstall="guessing yes")
])
case "$cl_cv_func_sigaction_reinstall" in
  *yes) AC_DEFINE(SIGACTION_NEED_REINSTALL,,[signal handlers installed via sigaction() need to be reinstalled when they are activated]) ;;
  *no) ;;
esac
fi
])

AC_DEFUN([CL_SIGACTION_UNBLOCK],
[AC_REQUIRE([CL_SIGACTION])dnl
AC_REQUIRE([CL_SIGACTION_REINSTALL])dnl
if test -n "$have_sigaction"; then
case "$signalblocks" in
  *POSIX* | *BSD*)
AC_CACHE_CHECK(whether signals are blocked when sigaction handlers are entered, cl_cv_func_sigaction_blocked, [
AC_TRY_RUN([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __CYGWIN32__
/* On Cygwin32 version 18, this test would hang (SIGALRM not being signalled).
 * Let it fail instead. */
#error "better fail than hang"
#endif
typedef void (*signal_handler_t) (int);
signal_handler_t mysignal (int sig, signal_handler_t handler)
{ struct sigaction old_sa;
  struct sigaction new_sa;
  memset(&new_sa,0,sizeof(new_sa));
  new_sa.sa_handler = handler;
  if (sigaction(sig,&new_sa,&old_sa)<0) { return (signal_handler_t)SIG_IGN; }
  return (signal_handler_t)old_sa.sa_handler;
}
volatile int gotsig=0;
volatile int wasblocked=0;
void sigalrm_handler()
{ gotsig=1;
#ifdef SIGNAL_NEED_REINSTALL
  mysignal(SIGALRM,(signal_handler_t)sigalrm_handler);
#endif
  { sigset_t blocked;
    sigprocmask(SIG_BLOCK, (sigset_t *) 0, &blocked);
    wasblocked = sigismember(&blocked,SIGALRM) ? 1 : 0;
  }
}
int got_sig () { return gotsig; }
int main() { /* returns 0 if they need not to be unblocked */
  mysignal(SIGALRM,(signal_handler_t)sigalrm_handler); alarm(1); while (!got_sig());
  exit(wasblocked);
}], cl_cv_func_sigaction_blocked=no, cl_cv_func_sigaction_blocked=yes,
dnl When cross-compiling, assume the worst case.
cl_cv_func_sigaction_blocked="guessing yes")
])
case "$cl_cv_func_sigaction_blocked" in
  *yes) AC_DEFINE(SIGACTION_NEED_UNBLOCK,,[signals need to be unblocked when signal handlers installed via sigaction() are left]) ;;
  *no) ;;
esac
  ;;
  *) ;;
esac
fi
])

dnl Copyright (C) 1993-2002 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels.

AC_PREREQ(2.13)

AC_DEFUN([CL_SOCKET],
[dnl Check whether -lsocket and maybe -lnsl is needed for the functions
dnl gethostbyname(), connect(), socket(), inet_addr(), setsockopt().
dnl On SVR4 and Solaris 2:
dnl   gethostbyname() and gethostent() in libnsl,
dnl   setsockopt() in libsocket,
dnl   libsocket requires libnsl.
dnl On SINIX-N 5.43:
dnl   gethostent() in libnsl,
dnl   gethostbyname() and setsockopt() in libsocket,
dnl   libsocket requires libnsl.
dnl Hence checking for gethostent() gives a better hint whether libnsl is
dnl needed than gethostbyname(). Once we found that, we check for setsockopt().
AC_CACHE_CHECK(whether gethostent requires -lnsl, cl_cv_lib_nsl, [
cl_cv_lib_nsl=no
AC_TRY_LINK(AC_LANG_EXTERN[char gethostent();], [gethostent();],
have_gethostent=1)
if test -z "$have_gethostent"; then
cl_save_LIBS="$LIBS"
LIBS="$LIBS -lnsl"
AC_TRY_LINK(AC_LANG_EXTERN[char gethostent();], [gethostent();],
cl_cv_lib_nsl=yes)
LIBS="$cl_save_LIBS"
fi
])
if test $cl_cv_lib_nsl = yes; then
  LIBS="$LIBS -lnsl"
fi
AC_CACHE_CHECK(whether setsockopt requires -lsocket, cl_cv_lib_socket, [
cl_cv_lib_socket=no
AC_TRY_LINK(AC_LANG_EXTERN[char setsockopt();], [setsockopt();],
have_setsockopt=1)
if test -z "$have_setsockopt"; then
cl_save_LIBS="$LIBS"
LIBS="$LIBS -lsocket"
AC_TRY_LINK(AC_LANG_EXTERN[char setsockopt();], [setsockopt();],
cl_cv_lib_socket=yes)
LIBS="$cl_save_LIBS"
fi
])
if test $cl_cv_lib_socket = yes; then
  LIBS="$LIBS -lsocket"
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_SOCKLEN_T],
[AC_CACHE_CHECK(for socklen_t in sys/socket.h, cl_cv_type_socklen_t, [
AC_EGREP_HEADER(socklen_t, sys/socket.h,
cl_cv_type_socklen_t=yes, cl_cv_type_socklen_t=no)
])
if test $cl_cv_type_socklen_t = yes; then
  AC_DEFINE(CLISP_SOCKLEN_T, socklen_t,
[socklen_t (if defined in <sys/socket.h>) or int otherwise])
else
  AC_DEFINE(CLISP_SOCKLEN_T, int)
fi
]
)

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_STAT],
[dnl Cannot use AC_CHECK_FUNCS(lstat) because Linux defines lstat() as an
dnl inline function in <sys/stat.h>.
CL_LINK_CHECK([lstat],cl_cv_func_lstat,[
#include <sys/types.h>
#include <sys/stat.h>
], [return lstat("",(struct stat *)0);],
AC_DEFINE(HAVE_LSTAT,,[have lstat()?]))dnl
AC_CHECK_HEADERS(sys/stat.h)
if test "$ac_cv_header_sys_stat_h" = "yes"; then
 AC_CHECK_MEMBERS([struct stat.st_rdev, struct stat.st_blksize, struct stat.st_blocks],,,[#include <sys/stat.h>])
fi
AC_CHECK_SIZEOF(ino_t)
AC_CHECK_SIZEOF(dev_t)
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2004, 2007-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_TCPCONN],
[CL_COMPILE_CHECK([IPv4 sockets], cl_cv_socket_ipv4,
[#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>],
[int x = AF_INET; struct in_addr y; struct sockaddr_in z;],
AC_DEFINE(HAVE_IPV4,,[<sys/socket.h> defines AF_INET]))
CL_COMPILE_CHECK([IPv6 sockets], cl_cv_socket_ipv6,
[#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>],
[int x = AF_INET6; struct in6_addr y; struct sockaddr_in6 z;],
AC_DEFINE(HAVE_IPV6,,[<sys/socket.h> defines AF_INET6]))
if test $cl_cv_socket_ipv6 = no; then
CL_COMPILE_CHECK([IPv6 sockets in linux/in6.h], cl_cv_socket_ipv6_linux,
[#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <linux/in6.h>],
[int x = AF_INET6; struct in6_addr y; struct sockaddr_in6 z;],
AC_DEFINE(IPV6_NEED_LINUX_IN6_H,,[need <linux/in6.h> for the in6_addr and sockaddr_in6 types])
AC_DEFINE(HAVE_IPV6))
fi
AC_CHECK_FUNCS(inet_pton inet_ntop inet_addr setsockopt getsockopt)
AC_CHECK_HEADERS(netinet/in.h arpa/inet.h)dnl
if test $ac_cv_func_inet_addr = yes; then
CL_PROTO([inet_addr], [
for x in CONST_VARIANTS; do
for y in SIZE_VARIANTS; do
if test -z "$have_inet_addr"; then
CL_PROTO_TRY([
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <netinet/in.h>
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef __BEOS__
#include <sys/socket.h>
#include <netdb.h>
#endif
], [$y inet_addr ($x char *);], [
cl_cv_proto_inet_addr_ret="$y"
cl_cv_proto_inet_addr_arg1="$x"
have_inet_addr=1])
fi
done
done
if test -z "$have_inet_addr"; then
CL_PROTO_MISSING(inet_addr)
fi
], [extern $cl_cv_proto_inet_addr_ret inet_addr ($cl_cv_proto_inet_addr_arg1 char*);])
AC_DEFINE_UNQUOTED(RET_INET_ADDR_TYPE,$cl_cv_proto_inet_addr_ret,[return type of inet_addr()])
AC_DEFINE_UNQUOTED(INET_ADDR_CONST,$cl_cv_proto_inet_addr_arg1,[declaration of inet_addr() needs const])
if test "$cl_cv_proto_inet_addr_ret" = "struct in_addr"; then
AC_DEFINE(INET_ADDR_SUFFIX,[.s_addr],[Define as .s_addr if the return type of inet_addr() is a struct type, as empty if it is a scalar type])
else
AC_DEFINE(INET_ADDR_SUFFIX,[])
fi
fi
AC_CHECK_HEADERS(netinet/tcp.h,,,
dnl AIX 4 requires <netinet/in.h> to be included before <netinet/tcp.h>.
[#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
])
if test $ac_cv_func_setsockopt = yes; then
CL_PROTO([setsockopt], [
for z in SIZE_VARIANTS; do
for y in 'char*' 'void*'; do
for x in CONST_VARIANTS; do
if test -z "$have_setsockopt_decl"; then
CL_PROTO_TRY([
#include <sys/types.h>
#include <sys/socket.h>
], [int setsockopt (int, int, int, $x $y, $z);], [
cl_cv_proto_setsockopt_const="$x"
cl_cv_proto_setsockopt_arg_t="$y"
cl_cv_proto_setsockopt_optlen_t="$z"
have_setsockopt_decl=1])
fi
done
done
done
if test -z "$have_setsockopt_decl"; then
CL_PROTO_MISSING(setsockopt)
fi
], [extern int setsockopt (int, int, int, $cl_cv_proto_setsockopt_const $cl_cv_proto_setsockopt_arg_t, $cl_cv_proto_setsockopt_optlen_t);])
AC_DEFINE_UNQUOTED(SETSOCKOPT_CONST,$cl_cv_proto_setsockopt_const,[declaration of setsockopt() needs const])
AC_DEFINE_UNQUOTED(SETSOCKOPT_ARG_T,$cl_cv_proto_setsockopt_arg_t,[type of `optval' in setsockopt() declaration])
AC_DEFINE_UNQUOTED(SETSOCKOPT_OPTLEN_T,$cl_cv_proto_setsockopt_optlen_t,[type of `optlen' in setsockopt() declaration])
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2002, 2005 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold

AC_PREREQ(2.13)

AC_DEFUN([CL_TERMCAP],[
dnl Some systems have tgetent(), tgetnum(), tgetstr(), tgetflag(), tputs(),
dnl tgoto() in libc, some have it in libtermcap, some have it in libncurses.
dnl When both libtermcap and libncurses exist, we prefer the latter,
dnl because libtermcap is being phased out.
dnl libcurses is useless: all platforms which have libcurses also have
dnl libtermcap, also they were all different on the various Unix systems,
dnl and often buggy
termcap_prefix=""
AC_ARG_WITH([libtermcap-prefix],
[  --with-libtermcap-prefix[=DIR]  search for ncurses and termcap in DIR],
[case "$withval" in
  /*) termcap_prefix=$withval; ;;
esac])
if test x$termcap_prefix != x; then
  LDFLAGS_save=$LDFLAGS
  LDFLAGS=$LDFLAGS" -L$termcap_prefix/lib"
fi
LIBTERMCAP="broken"
INCTERMCAP=""
AC_SEARCH_LIBS(tgetent, ncurses termcap, LIBTERMCAP="")
if test x$termcap_prefix != x; then
  LDFLAGS=$LDFLAGS_save
  if test $LIBTERMCAP != broken; then
    INCTERMCAP=-I$termcap_prefix/include
    LIBTERMCAP=-L$termcap_prefix/lib
  fi
fi
AC_SUBST(LIBTERMCAP)
AC_SUBST(INCTERMCAP)
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_TERM],
[AC_BEFORE([$0], [CL_IOCTL])
AC_CHECK_HEADERS(termios.h termio.h sys/termio.h sgtty.h)dnl
if test $ac_cv_header_termios_h = yes; then
dnl HAVE_TERMIOS_H defined
dnl A/UX has <termios.h> but is lacking tcgetattr etc.
CL_LINK_CHECK([tcgetattr], cl_cv_func_tcgetattr,
[#include <termios.h>], [struct termios t; tcgetattr(0,&t);],
AC_DEFINE(HAVE_TCGETATTR,,[have tcgetattr(), either as a function or as a macro defined by <termios.h>]))dnl
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

dnl -*- Autoconf -*-
dnl Copyright (C) 2005-2006 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Sam Steingold.

AC_PREREQ(2.13)

AC_DEFUN([CL_TEST_NT],[dnl check that test(1) can serve as make(1)
AC_CACHE_CHECK(whether test -nt works, cl_cv_test_nt, [
rm -f conftestfile1 conftestfile2
touch conftestfile1
# see makemake.in, rule "anymodule":
if (test -f conftestfile1 -a conftestfile1 -nt conftestfile2) 1>&5 2>&5
then cl_cv_test_nt=yes
else cl_cv_test_nt=no
fi
rm -f conftestfile1
])
TEST_NT=${cl_cv_test_nt-no}
AC_SUBST(TEST_NT)dnl
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_UNIXCONN],
[AC_CHECK_HEADERS(sys/un.h)dnl
if test $ac_cv_header_sys_un_h = yes; then
CL_COMPILE_CHECK([sun_len in struct sockaddr_un], cl_cv_struct_sockaddr_sun_len,
[#include <sys/types.h> /* NetBSD 1.0 needs this */
#include <sys/un.h>],
[struct sockaddr_un unaddr; unaddr.sun_len;], AC_DEFINE(HAVE_SOCKADDR_UN_LEN,,[`struct sockaddr_un' from <sys/un.h> has a `sun_len' field]))dnl
fi
])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2003 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_VADVISE],
[CL_LINK_CHECK([vadvise], cl_cv_func_vadvise,
[#include <sys/vadvise.h>], [vadvise(0);],
AC_DEFINE(HAVE_VADVISE,,[have the vadvise() system call])dnl
)])

dnl -*- Autoconf -*-
dnl Copyright (C) 1993-2008 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

dnl From Bruno Haible, Marcus Daniels, Sam Steingold.

AC_PREREQ(2.57)

AC_DEFUN([CL_WAITPID],
[CL_PROTO([waitpid], [
CL_PROTO_TRY([
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
], [pid_t waitpid (pid_t pid, int* statusp, int options);],
cl_cv_proto_waitpid_arg1="pid_t", cl_cv_proto_waitpid_arg1="int")
], [extern pid_t waitpid ($cl_cv_proto_waitpid_arg1, int*, int);])
AC_DEFINE_UNQUOTED(PID_T,$cl_cv_proto_waitpid_arg1,[type of `pid' in waitpid() declaration])
])

