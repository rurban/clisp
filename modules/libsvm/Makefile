# Makefile for CLISP module set libsvm

srcdir = .
AR = ar
RANLIB = ranlib
CC = gcc
CXX = g++
CPPFLAGS =
CXXFLAGS = -O3 -ffast-math -Wall
CLISP = clisp -q -norc
INCLUDES = $$($(CLISP) -b)/linkkit

LN = ln
LN_S = ln -s

MAKE = make

SHELL = /bin/sh

GENERATED = libsvm.fas svm$(SHREXT) libsvm.o link.sh preload.lisp
DISTRIBFILES = Makefile $(GENERATED) $(srcdir)/libsvm.lisp $(srcdir)/README
distribdir =

# default target: make the module
clisp-module : $(GENERATED)

svm$(SHREXT) : $(srcdir)/svm.cpp $(srcdir)/svm.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(INCLUDES) \
		-fPIC -shared -o svm$(SHREXT) $< -lm

svm.o : $(srcdir)/svm.cpp $(srcdir)/svm.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(srcdir) -c $<

svm.a : svm.o
	$(AR) rcv svm.a svm.o
	$(RANLIB) svm.a

libsvm.c libsvm.fas : $(srcdir)/libsvm.lisp
	$(CLISP) -c $< -o ./

libsvm.o : libsvm.c $(srcdir)/svm.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -I$(srcdir) -c libsvm.c

preload.lisp : $(srcdir)/preload.lisp
	$(LN_S) $< .

link.sh : $(srcdir)/link.sh
	$(LN_S) $< .

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f core *.o *.a svm$(SHREXT)

distclean : clean

force :

