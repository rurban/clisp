# Makefile for CLISP module set matlab
# <http://www.mathworks.com/access/helpdesk/help/techdoc/apiref/apiref.shtml>

srcdir = .
CC = gcc
CPPFLAGS =
CFLAGS = -Wall -O2
# MATLAB = /usr/local/matlab/extern/include
MATLAB = d:/MATLAB7/extern/include
CLISP = clisp -q -norc
INCLUDES = $$($(CLISP) -b)/linkkit

LN = ln
LN_S = ln -s

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = matlab.o link.sh Makefile \
	$(srcdir)/matlab.lisp $(srcdir)/wrap.lisp
distribdir =

all : matlab.fas matlab.o wrap.fas link.sh

matlab.c matlab.fas : $(srcdir)/matlab.lisp
	$(CLISP) -c $< -o ./

matlab.o : matlab.c
	$(CC) $(CFLAGS) -I$(INCLUDES) -I$(MATLAB) -c matlab.c

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

