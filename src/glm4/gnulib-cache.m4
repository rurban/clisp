# Copyright (C) 2002-2018 Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.
#
# As a special exception to the GNU General Public License,
# this file may be distributed as part of a program that
# contains a configuration script generated by Autoconf, under
# the same distribution terms as the rest of that program.
#
# Generated by gnulib-tool.
#
# This file represents the specification of how gnulib-tool is used.
# It acts as a cache: It is written and read by gnulib-tool.
# In projects that use version control, this file is meant to be put under
# version control, like the configure.ac and various Makefile.am files.


# Specification in the form of a command-line invocation:
#   gnulib-tool --import --lib=libgnu --source-base=src/gllib --m4-base=src/glm4 --doc-base=doc --tests-base=tests --aux-dir=src/build-aux --avoid=xalloc-die --no-conditional-dependencies --no-libtool --macro-prefix=gl --no-vc-files accept alloca-opt arpa_inet bind c-strtod close connect environ errno fd-hook fnmatch-gnu getloadavg getpagesize getpeername getsockname getsockopt gettext gettimeofday gnu-make havelib host-cpu-c-abi inet_ntop inet_pton ioctl libsigsegv libunistring-optional link-follow listen localcharset lstat mkdtemp mkfifo mknod mkstemp mktime netinet_in no-c++ nocrash noreturn readlink recv recvfrom regex select send sendto setenv setsockopt shutdown socket socketlib sockets socklen stat stdbool stdint stdlib streq strerror_r-posix strftime strptime strverscmp sys_select sys_time sys_uio sys_wait uname uniname/uniname unistd unitypes uniwidth/width unsetenv vma-iter

# Specification in the form of a few gnulib-tool.m4 macro invocations:
gl_LOCAL_DIR([])
gl_MODULES([
  accept
  alloca-opt
  arpa_inet
  bind
  c-strtod
  close
  connect
  environ
  errno
  fd-hook
  fnmatch-gnu
  getloadavg
  getpagesize
  getpeername
  getsockname
  getsockopt
  gettext
  gettimeofday
  gnu-make
  havelib
  host-cpu-c-abi
  inet_ntop
  inet_pton
  ioctl
  libsigsegv
  libunistring-optional
  link-follow
  listen
  localcharset
  lstat
  mkdtemp
  mkfifo
  mknod
  mkstemp
  mktime
  netinet_in
  no-c++
  nocrash
  noreturn
  readlink
  recv
  recvfrom
  regex
  select
  send
  sendto
  setenv
  setsockopt
  shutdown
  socket
  socketlib
  sockets
  socklen
  stat
  stdbool
  stdint
  stdlib
  streq
  strerror_r-posix
  strftime
  strptime
  strverscmp
  sys_select
  sys_time
  sys_uio
  sys_wait
  uname
  uniname/uniname
  unistd
  unitypes
  uniwidth/width
  unsetenv
  vma-iter
])
gl_AVOID([ xalloc-die])
gl_SOURCE_BASE([src/gllib])
gl_M4_BASE([src/glm4])
gl_PO_BASE([])
gl_DOC_BASE([doc])
gl_TESTS_BASE([tests])
gl_LIB([libgnu])
gl_MAKEFILE_NAME([])
gl_MACRO_PREFIX([gl])
gl_PO_DOMAIN([])
gl_WITNESS_C_MACRO([])
gl_VC_FILES([false])
