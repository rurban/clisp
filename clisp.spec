# Copyright (C) 1998, 1999, 2002 by Sam Steingold
# Distributed under the terms of the GNU General Public License.
# See <http://www.gnu.org/copyleft/gpl.html>.

# The purpose of this file is creation of source/binary RPMs,
# **NOT** building/installing CLISP.
# If you read the comments below, you will learn why.

# to create the source/binary RPMs, do
#  rpmbuild -ba --sign clisp.spec

%define src /usr/local/src
%define prefix /usr
%define name clisp
# the release version is the same as the current development version,
# at least for some time :-)
%define version %(cat %{src}/%{name}/current/src/VERSION)
%define builddir build-rpm
%define mysrc %{src}/%{name}/current

# don't you just love that you have to fit the macro into one line?
# this automatically upgrades `release' with each build.
# don't forget to remove the file `.release' when changing `version'.
#%define release %(test -f .release || echo 0 >> .release; echo "1 + " `cat .release` | bc > .,release; mv -fv .,release .release; cat .release)
#%define release %(cat .release)
%define release 1

Summary:      Common Lisp (ANSI CL) implementation
Name:         %{name}
Version:      %{version}
Release:      %{release}
# this crap does not accept PNG
#Icon:         clisp.png
Copyright:    GPL
Group:        development/languages
Source:       ftp://cvs2.cons.org/pub/lisp/clisp/source/latest/
URL:          http://clisp.cons.org/
Packager:     Sam Steingold <sds@gnu.org>
Provides:     clisp, ansi-cl
Distribution: Red Hat GNU/Linux
%description
%(cat SUMMARY)

# RPM doesn't provide for comfortable operation: when I want to create a
# package, I have to untar, build and install (--short-circuit works for
# compilation and installation only, so if I want to build a binary RPM,
# I am doomed to untar, compile and install!)  This is unacceptable, so
# I disabled untar completely - I don't need it anyway, I work from a
# CVS repository, and I comment out the build clause and `make install`.
# If *YOU* want to build using RPM, you are welcome to it: just
# uncomment the commands in the appropriate sections.
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
%setup -T -D -n %{mysrc}
%build
echo "Uncomment 'configure' in 'clisp.spec' if you want to build";
#rm -rf %{builddir}
#./configure --prefix=%{prefix} --fsstnd=redhat --with-module=regexp \
#    --with-module=bindings/glibc --with-module=clx/new-clx \
#    --with-module=syscalls --build %{builddir}
%install
echo "Uncomment 'make install' in 'clisp.spec' if you want to install";
# cd %{builddir}
# make install

# create the source tar, necessary for source RPMs
# this has to be done just once - uncomment if you want it
#cd %{mysrc}
#make -f Makefile.devel src-distrib
#ln /tmp/%{name}-%{version}.tar.bz2 /usr/src/redhat/SOURCES/
%clean
echo "Uncomment removing builddir in 'clisp.spec' if you want a cleanup"
#rm -rf %{builddir}
%files
%{prefix}/bin/clisp
%{prefix}/lib/clisp/
%{prefix}/share/doc/%{name}-%{version}/
%{prefix}/share/man/man1/clisp.1
%{prefix}/share/locale/de/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/de/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/en/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/en/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/es/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/es/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/fr/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/fr/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/nl/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/nl/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/ru/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/ru/LC_MESSAGES/clisplow.mo
