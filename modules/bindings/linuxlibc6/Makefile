# Makefile for CLISP module set linuxlibc6

CC =
CFLAGS =
INCLUDES=

CLISP =

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = linux.o link.sh Makefile linux.lsp
WDISTRIBFILES = $(DISTRIBFILES) wlinux.o
distribdir =

all : linux.o

linux.c : linux.lsp
	$(CLISP) -c linux.lsp

linux.o : linux.c
	$(CC) $(CFLAGS) -I$(INCLUDES) -c linux.c

wlinux.o : linux.c
	$(CC) $(CFLAGS) -DWIDE -I$(INCLUDES) -c linux.c -o wlinux.o

# Make a module
clisp-module : all

clisp-module-wide : all wlinux.o

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clisp-module-distrib-wide : clisp-module-wide force
	$(LN) $(WDISTRIBFILES) $(distribdir)

force :

