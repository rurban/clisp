# Makefile for CLISP module set postgresql

CC =
CFLAGS =
INCLUDES=

CLISP =

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = postgresql.o link.sh Makefile postgresql.lisp
distribdir =

all : postgresql.o

postgresql.c : postgresql.lisp
	$(CLISP) -c postgresql.lisp

postgresql.o : postgresql.c
	$(CC) $(CFLAGS) -I$(INCLUDES) -c postgresql.c

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

