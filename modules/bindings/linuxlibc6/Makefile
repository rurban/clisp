# Makefile for CLISP module set linuxlibc6

CC =
CFLAGS =
INCLUDES=

CLISP =

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = linux.o link.sh Makefile linux.lisp wrap.lisp
distribdir =

all : linux.o wrap.fas

linux.c : linux.lisp
	$(CLISP) -c linux.lisp

linux.o : linux.c
	$(CC) $(CFLAGS) -I$(INCLUDES) -c linux.c

wrap.fas: wrap.lisp
	$(CLISP) -c wrap.lisp

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

