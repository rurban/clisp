# Makefile for CLISP module set postgresql

CC =
CFLAGS =
INCLUDES=

CLISP =

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = postgresql.o link.sh Makefile postgresql.lsp
distribdir =

all : postgresql.o

postgresql.c : postgresql.lsp
	$(CLISP) -c postgresql.lsp

postgresql.o : postgresql.c
	$(CC) $(CFLAGS) -I$(INCLUDES) -c postgresql.c

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

