# Copyright (C) 1998, 1999 by Sam Steingold
# GNU General Public License v.2 (GPL2) is applicable:
# No warranty; you may copy/modify/redistribute under the same
# conditions with the source code. See <URL:http://www.gnu.org>
# for the details and the precise copyright document.

# The purpose of this file is creation of source/binary RPMs, **NOT**
# building/installing CLISP.  If you read the comments below, you will
# learn why.

# to create the source/binary RPMs, do
#  rpm -ba --sign clisp.spec

%define name clisp
%define version 1999.05.15
%define clisp_build build

# don't you just love that you have to fit the macro into one line?
# this automatically upgrades `release' with each build.
# don't forget to remove the file `.release' when changing `version'.
%define release %(test -f .release || echo 0 >> .release; echo "1 + " `cat .release` | bc > ,.release; mv ,.release .release; cat .release)
#%define release %(cat .release)

Summary:      Common Lisp (ANSI CL) implementation
Name:         %{name}
Version:      %{version}
Release:      %{release}
Icon:         clisp.gif
Copyright:    GPL
Group:        development/languages
Source:       ftp://clisp.cons.org/pub/lisp/clisp/source/clispsrc.tar.gz
URL:          http://clisp.cons.org/
Packager:     Red Hat Contrib|Net <rhcn-bugs@redhat.com>
Provides:     clisp, ansi-cl
Distribution: Red Hat Contrib|Net
%description
Common Lisp is a high-level, all-purpose programming language.
CLISP is a Common Lisp implementation by Bruno Haible of Karlsruhe
University and Michael Stoll of Munich University, both in Germany.
It mostly supports Common Lisp as described in the ANSI CL standard.
It runs on microcomputers (DOS, OS/2, Windows NT, Windows 95, Amiga
500-4000, Acorn RISC PC) as well as on Unix workstations (Linux, SVR4,
Sun4, DEC Alpha OSF, HP-UX, NeXTstep, SGI, AIX, Sun3 and others) and
needs only 2 MB of RAM.
It is free software and may be distributed under the terms of GNU GPL,
while it is possible to distribute commercial applications compiled
with CLISP.
The user interface comes in German, English, French and Spanish.
CLISP includes an interpreter, a compiler, a large subset of CLOS, a
foreign language interface and a socket interface.
An X11 interface is available through CLX and Garnet.

More information on at <http://clisp.cons.org/>.
Sources and selected binaries are available by anonymous ftp from
<ftp://clisp.cons.org/pub/lisp/clisp/>.
The latest and greatest i386 binary RPM is on
<ftp://cellar.goems.com/pub/clisp>.

The is built with glibc2.1; if you want to use it with glibc2.0, you will
need <ftp://ftp.suse.com/pub/suse_update/suse61/a1/regframe.rpm>.

The package was created by Sam Steingold <sds@goems.com>.
(RHCN requires that I put their e-mail into the "Packager:" header).

# RPM doesn't provide for comfortable operation: when I want to create a
# package, I have to untar, build and install (--short-circuit works for
# compilation and installation only, so if I want to build a binary RPM,
# I am doomed to untar, compile and install!)  This is unacceptable, so
# I disabled untar completely - I don't need it anyway, I work from a
# CVS repository, and I comment out the build clause and `make install`.
# If *YOU* want to build using RPM, you are welcome to it: just
# uncomment the commands in the appropriate sections (do not uncomment
# the doubly commented lines - they are maintainer-only commands).
# Additionally, RPM barfs on rpmrc created with `rpm --showrc > /etc/rpmrc`
# which is an unspeakable abomination.
# I reported all these as bugs and was told "it's a feature, not a bug".

%prep
cat <<EOF
This will build RPMs for CLISP: %{name}-%{version}-%{release}.
We assume that you are in the top level source directory already.
No unpacking or patching is done - we go straight to build and
creating the RPMs.  See 'clisp.spec' for more information.
EOF
%setup -T -D -n /usr/src/%{name}
%build
##rm -rf src/VERSION
##date +%Y-%02m-%02d > src/VERSION
##make -f Makefile.devel src/version.h
## make -f Makefile.devel
## make -f Makefile.devel check-configures
echo "Uncomment 'configure' in 'clisp.spec' if you want to build";
#./configure --prefix=/usr --fsstnd=redhat --with-module=wildcard \
#    --with-module=regexp --with-module=bindings/linuxlibc6 \
#    --with-module=clx/new-clx --with-module=postgresql642 \
#    --with-export-syscalls --build %{clisp_build}
%install
echo "Uncomment 'make install' in 'clisp.spec' if you want to install";
# cd %{clisp_build}
# make install

# create the source tar, necessary for source RPMs
cd /usr/src/%{name}
# remove the junk created by CVS
find . -name ".#*" | xargs rm -f
cd ..
rm -fv clisp-%{version} # redhat/SOURCES/clispsrc.tar.gz
ln -sv clisp clisp-%{version}
tar -c -h -f redhat/SOURCES/clispsrc.tar clisp-%{version}/ \
    --exclude build --exclude CLHSROOT --exclude CVS --exclude .cvsignore
gzip -9vf redhat/SOURCES/clispsrc.tar
rm -fv clisp-%{version}
cd clisp

%files
%dir /usr/lib/clisp/
/usr/lib/clisp/*
/usr/bin/clisp
%dir /usr/doc/%{name}-%{version}/
/usr/doc/%{name}-%{version}/*
/usr/man/man3/clreadline.3
/usr/man/man1/clisp.1
/usr/share/locale/de/LC_MESSAGES/clisp.mo
/usr/share/locale/en/LC_MESSAGES/clisp.mo
/usr/share/locale/es/LC_MESSAGES/clisp.mo
/usr/share/locale/fr/LC_MESSAGES/clisp.mo
