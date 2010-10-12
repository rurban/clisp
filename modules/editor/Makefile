# Makefile for CLISP module set EDITOR

srcdir = .
CLISP = clisp -norc -q
CLISP_LINKKIT = $$($(CLISP) -b)/linkkit

LN = ln
LN_S = ln -s

MAKE = make

SHELL = /bin/sh

GENERATED = editor.fas link.sh
DISTRIBFILES = Makefile $(GENERATED) $(srcdir)/editor.lisp
distribdir =

# default target: make the module
clisp-module : $(GENERATED)

editor.fas: $(srcdir)/editor.lisp
	$(CLISP) -c $(srcdir)/editor.lisp -o ./

link.sh : $(srcdir)/link.sh
	$(LN_S) $(srcdir)/link.sh .

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f *.fas *.lib

distclean : clean

force :

