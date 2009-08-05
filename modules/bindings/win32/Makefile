# Makefile for CLISP module set win32

srcdir = .
CC = gcc
CPPFLAGS =
CFLAGS = -Wall -O2
INCLUDES= ../..

CLISP = clisp -q -norc

LN = ln
LN_S = ln -s

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = link.sh Makefile $(srcdir)/win32.lisp # win32.o
distribdir =

# add win32.o if a C file is generated
all : link.sh win32.fas # win32.o

win32.c win32.fas : $(srcdir)/win32.lisp
	$(CLISP) -c $< -o ./

win32.o : win32.c $(INCLUDES)/clisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c win32.c

link.sh : $(srcdir)/link.sh
	$(LN_S) $< .

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

