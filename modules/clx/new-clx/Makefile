# Makefile for CLISP module set clx

CC = cc
CFLAGS = -O
INCLUDES= ../../
CLISP = clisp
LN = ln
SHELL = /bin/sh

DISTRIBFILES = link.sh Makefile clx-preload.lsp clx.lsp clx.fas image.lsp image.fas clx.o
distribdir =

### Custom defs.
E2D = ./e2d
CCMP2C = ../../ccmp2c
COMMENT5 = ../../comment5
ANSIDECL = ../../ansidecl
RM = rm -f
WANTS = -DWANT_XSHAPE=1 -DWANT_XPM=1

all : clx.fas image.fas clx.o

clx.fas clx.lib: clx.lsp clx-preload.lsp
	$(CLISP) -q -i clx-preload.lsp -c clx.lsp

image.fas: image.lsp clx.lib
	$(CLISP) -q -i clx-preload.lsp -c image.lsp

$(E2D): e2d.c
	$(CC) $(CFLAGS) -I$(INCLUDES) -o $(E2D) e2d.c

clx.e: clx.f
	$(CCMP2C) clx.f > genclx.c
	$(CC) $(CFLAGS) $(WANTS) genclx.c -o genclx
	./genclx > clx.e
	$(RM) genclx.c
	$(RM) genclx

clx.d: clx.e $(E2D)
	echo "#line 1 \"clx.e\"" > clx.temp
	sed -e 's,//.*$$,,' -e 's,/\*[^/]*\*/, ,g' -e 's,/\*[^*]*\*/, ,g' -e 's,\(/\*.*\)`,\1,g' < clx.e >> clx.temp
	$(E2D) clx clx.temp clx.d
	$(RM) clx.temp

clx.c: clx.d
	$(COMMENT5) clx.d | $(ANSIDECL) > clx.c

clx.o: clx.c
	$(CC) $(CFLAGS) -I$(INCLUDES) -c clx.c

# Make a module
clisp-module : clx.fas image.fas clx.o

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)


clean : force
	$(RM) genclx.c genclx clx.temp
	$(RM) *.lib *.fas
	$(RM) clx.e clx.d clx.c clx.o clx.tabs.c
	$(RM) e2d

force:

