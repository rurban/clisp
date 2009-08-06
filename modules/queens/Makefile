# Makefile for the sample CLISP module QUEENS

srcdir = .

# Programs used by "make":
CC = gcc -O
CPPFLAGS =
CFLAGS = -Wall -fomit-frame-pointer -O2
CPP = gcc -O -E -w
CLFLAGS =
CLISP = clisp -q -norc
INCLUDES = $$($(CLISP) -b)/linkkit

LN = ln
LN_S = ln -s

MAKE = make

SHELL = /bin/sh

GENERATED = queens.o callqueens.o link.sh
DISTRIBFILES = Makefile $(GENERATED)
distribdir =

# default target: make the module
clisp-module : $(GENERATED)

queens.o : $(srcdir)/queens.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

callqueens.m.c : $(srcdir)/callqueens.c
	$(CLISP) -C $(INCLUDES)/modprep $< -o ./

callqueens.o : callqueens.m.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c callqueens.m.c -o callqueens.o

link.sh : $(srcdir)/link.sh
	$(LN_S) $< .

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

