# Makefile for the sample CLISP module QUEENS

# Programs used by "make":
CC = gcc -O
CFLAGS = -Wall -fomit-frame-pointer -O2
INCLUDES=.
CPP = gcc -O -E -w
CLFLAGS =

MODPREP = ../clisp -q -norc ../modprep

CLISP =

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = *
distribdir =

all : queens.o callqueens.o

queens.o : queens.c
	$(CC) $(CFLAGS) -c queens.c

callqueens.m.c : callqueens.c
	$(MODPREP) callqueens.c

callqueens.o : callqueens.m.c
	$(CC) $(CFLAGS) -I$(INCLUDES) -c callqueens.m.c -o callqueens.o

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

force :

