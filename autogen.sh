#!/bin/sh
# Convenience script for regenerating all autogeneratable files that are
# omitted from the version control repository. In particular, this script
# also regenerates all aclocal.m4, config.h.in, Makefile.in, configure files
# with new versions of autoconf or automake.
#
# This script requires
#   - autoconf >= 2.60 and automake >= 1.10 in the PATH,
#   - GNU make in the PATH,
#   - cvs in the PATH,
#   - wget in the PATH,
#   - an internet connection.

# Usage: ./autogen.sh [--skip-gnulib]
#
# Usage after a first-time CVS checkout:     ./autogen.sh
# Usage after a CVS update:                  ./autogen.sh [--skip-gnulib]

skip_gnulib=false
while :; do
  case "$1" in
    --skip-gnulib) skip_gnulib=true; shift;;
    *) break ;;
  esac
done

if ! $skip_gnulib; then
  make -f Makefile.devel gnulib-imported
fi
make -f Makefile.devel build-prerequisites
