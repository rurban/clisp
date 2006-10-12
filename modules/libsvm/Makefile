# Makefile for CLISP module set libsvm

CC = gcc
CXX = g++
CPPFLAGS =
CXXFLAGS = -O3 -ffast-math -Wall
INCLUDES= ..

CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = svm.so link.sh Makefile libsvm.lisp
distribdir =

all : libsvm.fas svm.so

svm.so : svm.cpp svm.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(INCLUDES) \
		-shared -o svm.so svm.cpp -lm

libsvm.fas : libsvm.lisp
	$(CLISP) -c libsvm.lisp

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f core *.o *.a

distclean : clean

force :

