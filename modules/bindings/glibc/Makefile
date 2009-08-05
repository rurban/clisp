# Makefile for CLISP module set bindings/glibc

srcdir = .
CC = gcc
CPPFLAGS =
CFLAGS = -Wall -O2
INCLUDES = ../..

CLISP = clisp -q -norc

LN = ln
LN_S = ln -s

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = linux.o link.sh Makefile $(srcdir)/linux.lisp $(srcdir)/wrap.lisp
distribdir =

all : linux.o linux.fas wrap.fas link.sh

linux.c linux.fas : $(srcdir)/linux.lisp
	$(CLISP) -c $< -o ./

linux.o : linux.c $(INCLUDES)/clisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c linux.c

%.fas: $(srcdir)/%.lisp
	$(CLISP) -c $< -o ./

link.sh : $(srcdir)/link.sh
	$(LN_S) $< .

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

