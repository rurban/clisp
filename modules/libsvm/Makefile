# Makefile for CLISP module set libsvm

AR = ar
RANLIB = ranlib
CC = gcc
CXX = g++
CPPFLAGS =
CXXFLAGS = -O3 -ffast-math -Wall
CLISP = clisp -q -norc
INCLUDES = $$($(CLISP) -b)/linkkit

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = svm.so link.sh Makefile libsvm.lisp libsvm.o
distribdir =

all : libsvm.fas svm.so libsvm.o

svm.so : svm.cpp svm.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(INCLUDES) \
		-fPIC -shared -o svm.so svm.cpp -lm

svm.o : svm.cpp svm.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(INCLUDES) -c svm.cpp

svm.a : svm.o
	$(AR) rcv svm.a svm.o
	$(RANLIB) svm.a

libsvm.c libsvm.fas : libsvm.lisp
	$(CLISP) -c libsvm.lisp

libsvm.o : libsvm.c svm.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c libsvm.c

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f core *.o *.a

distclean : clean

force :

