# $Id$
# Copyright (C) 1998, 1999 by Sam Steingold
# GNU General Public License v.2 (GPL2) is applicable:
# No warranty; you may copy/modify/redistribute under the same
# conditions with the source code. See <URL:http://www.gnu.org>
# for the details and the precise copyright document.

# The purpose of this file is creation of source/binary RPMs, **NOT**
# building/installing CLISP.  If you read the comments below, you will
# learn why.

# to create the source/binary RPMs, do (as root!)
# rpm -ba --sign clisp.spec

%define name clisp
%define version 1999.01.08
%define clisp_build build

# don't you just love that you have to fit the macro into one line?
# this automatically upgrades `release' with each build.
# don't forget to remove the file `.release' when changing `version'.
%define release %(test -f .release || echo 0 >> .release; echo "1 + " `cat .release` | bc > ,.release; mv ,.release .release; cat .release)

Summary:      Common Lisp (ANSI CL) implementation
Name:         %{name}
Version:      %{version}
Release:      %{release}
Icon:         clisp.gif
Copyright:    GPL
Group:        development/languages
Source:       ftp://seagull.cdrom.com/pub/lisp/clisp/source/clispsrc.tar.gz
URL:          http://clisp.cons.org
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
<ftp://ftp2.cons.org/pub/lisp/clisp>.
The latest and greatest i386 binary RPM is on
<ftp://cellar.goems.com/pub/clisp>.

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
./configure --prefix=/usr --fsstnd=redhat --with-module=wildcard \
    --with-module=regexp --with-module=bindings/linuxlibc6 \
    --with-module=clx/new-clx --with-module=postgresql642 \
    --with-export-syscalls --build %{clisp_build}
%install
cd %{clisp_build}
# make install
test -d doc || mkdir doc
cp CLOS-guide.txt clisp.html cltl2.txt readline.dvi \
    LISP-tutorial.txt clreadline.3 editors.txt clisp.1 clreadline.dvi \
    impnotes.html clisp.gif clreadline.html doc
cd ..

# Can you believe it?!!  RPM runs chown -R root.root / chmod -R!!!
# Who was the wise guy who invented this?!  Now not only I have to run
# rpm as root (as I should not have to - chown/chmod can be done in the
# package file itself, not on disk!) but I also cannot work with the
# sources afterwards!
# Allright, I can set `fixperms' in /etc/rpmrc, but how do I avoid chown?!
# Unfortunately, the following screws up the docs: they are installed
# root.src, not root.root, and RPM on the target machine will complain.
#cd /usr/src/%{name}
#chgrp -R src .
#chmod -R g+wX .

# create the source tar, necessary for source RPMs
cd /usr/src/%{name}
# remove the junk created by CVS
find . -name ".#*" | xargs rm -f
cd ..
mv clisp clisp-%{version}
tar cf redhat/SOURCES/clispsrc.tar clisp-%{version} \
    --exclude build --exclude CVS --exclude .cvsignore
gzip -9v redhat/SOURCES/clispsrc.tar
mv clisp-%{version} clisp
cd clisp

# BuildRoot: /tmp/build-%{name}-%{version}

%files
%defattr(644,root,root)
%dir /usr/lib/clisp/
%dir /usr/lib/clisp/data/
%dir /usr/lib/clisp/base/
%dir /usr/lib/clisp/full/
%docdir /usr/doc/%{name}-%{version}
%doc build/ANNOUNCE
%doc build/GNU-GPL
%doc build/MAGIC.add
%doc build/README
%doc build/SUMMARY
%doc build/COPYRIGHT
%doc build/NEWS
%doc build/README.en
%doc build/README.de
%doc build/README.es
%doc build/doc
/usr/man/man3/clreadline.3
/usr/man/man1/clisp.1
%attr(755,root,root) /usr/bin/clisp
%attr(755,root,root) /usr/lib/clisp/base/lisp.run
%attr(755,root,root) /usr/lib/clisp/full/lisp.run
/usr/lib/clisp/base/lispinit.mem
/usr/lib/clisp/full/lispinit.mem
/usr/lib/clisp/data/UnicodeData.txt
/usr/share/locale/de/LC_MESSAGES/clisp.mo
/usr/share/locale/en/LC_MESSAGES/clisp.mo
/usr/share/locale/es/LC_MESSAGES/clisp.mo
/usr/share/locale/fr/LC_MESSAGES/clisp.mo
