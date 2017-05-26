# Makefile for CLISP module set ASDF

srcdir = .
CLISP = clisp -norc -q
CLISP_LINKKIT = $$($(CLISP) -b)/linkkit

LN = ln
LN_S = ln -s

MAKE = make

SHELL = /bin/sh

GENERATED = asdf.fas link.sh
DISTRIBFILES = Makefile $(GENERATED) $(srcdir)/asdf.lisp
distribdir =

# default target: make the module
clisp-module : $(GENERATED)

asdf.fas: $(srcdir)/asdf.lisp
	$(CLISP) -m 2MW -c $(srcdir)/asdf.lisp -o ./

link.sh : $(srcdir)/link.sh
	$(LN_S) $(srcdir)/link.sh .

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f *.fas *.lib

distclean : clean

force :

