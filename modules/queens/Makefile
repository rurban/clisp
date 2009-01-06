# Makefile for the sample CLISP module QUEENS

# Programs used by "make":
CC = gcc -O
CPPFLAGS =
CFLAGS = -Wall -fomit-frame-pointer -O2
CPP = gcc -O -E -w
CLFLAGS =
CLISP = clisp -q -norc
INCLUDES = $$($(CLISP) -b)/linkkit

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = *
distribdir =

all : queens.o callqueens.o

queens.o : queens.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c queens.c

callqueens.m.c : callqueens.c
	$(CLISP) -C $(INCLUDES)/modprep callqueens.c

callqueens.o : callqueens.m.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c callqueens.m.c -o callqueens.o

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

