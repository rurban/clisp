# GNU-Makefile for tiny GCC library
# Jörg Höhle, 6-Aug-96

# 24.7.96: removed gnulib files from LIBFILES, use -lgcc instead
#        lib/gnuarith.o lib/_ashldi3.o lib/_ucmpdi2.o lib/_negdi2.o\

FLAGS=-Wall -fomit-frame-pointer
# -IGCC:include is used by the GCC2 inlines
INCPATH=-nostdinc -Iinclude -IADE:os-include -IADE:include

#CP    =copy clone
CP     =cp -p

CC     =gcc -v
CFLAGS =-O2 -mc68000 $(INCPATH) $(FLAGS)
AS     =as
ASFLAGS=-mc68010
LD     =ld
LDFLAGS=-x -nostdlib -Llib -l$(LIBSHORTNAME)
#AR from gcc-2.5.6/8 doesn't strip dirnames which I believe is wrong
AR     =ar233

#LIBSHORTNAME=tiny
LIBSHORTNAME=cclisp
LIBFILENAME=lib/lib$(LIBSHORTNAME).a
CRT0FILENAME=lib/crt0.o

STARTUPFILES=$(addprefix startup/,\
 main.c def_handle.c wbmain.c exit.c parseargs.c gcc2__main.s)

MISCFILES=misc/setjmp.s misc/strlen.c misc/CreatePort.c misc/setmode.c misc/getenv.c
# MISCOBJS=$(subst .c,.o,$(MISCFILES))

# Order is important for startup
LIBFILES=\
 startup/main.o startup/wbmain.o startup/exit.o startup/parseargs.o\
 misc/setjmp.o misc/strlen.o startup/gcc2__main.o\
 misc/CreatePort.o misc/BPTRfprintf.o misc/setmode.o misc/getenv.o\
 startup/def_handle.o startup/def_window.o startup/def_glue.o

# clisp.c gives nicer defaults
CLISPLIBFILES=$(filter-out startup/def_glue.o,\
 $(subst startup/def_window.o,startup/clisp.o,$(LIBFILES)))

all: $(LIBFILENAME) $(CRT0FILENAME)

lib/libcclisp.a: $(CLISPLIBFILES)
	$(RM) $@
	$(AR) -qv $@ $^
	ranlib $@

lib/libtiny.a: $(LIBFILES)
	$(RM) $@
	$(AR) -qv $@ $^
	ranlib $@

lib/crt0.o: startup/crt0.o
	$(CP) $< $@

lib/gnuarith.o:
	echo "gnuarith.o is taken from GCCBuRP"

lib/_ashldi3.o lib/_ucmpdi2.o lib/_negdi2.o lib/_udivdi3.o:
	echo "$@ comes from GCC-2.5.8"

# Luckily, -lgcc is not required here
test: test.o $(LIBFILENAME) $(CRT0FILENAME)
	$(LD) $(CRT0FILENAME) $< $(LDFLAGS) -o $@

test-s: test.o $(LIBFILENAME) $(CRT0FILENAME)
	$(LD) -s $(CRT0FILENAME) $< $(LDFLAGS) -o $@

setmode-main: misc/setmode.c $(LIBFILENAME) $(CRT0FILENAME)
	$(CC) $(CFLAGS) -c -DMAIN -DJCHLIB misc/setmode.c -o setmode-main.o
	$(LD) -s $(CRT0FILENAME) setmode-main.o $(LDFLAGS) -o $@

clean:
	$(RM) startup/*.o misc/*.o test.o $(LIBFILENAME) $(CRT0FILENAME)

# Amiga object module format
%.ao: %.o
	sobja -b -s $< $@

# Dillon's object module disassembler
%.dobj: %.ao
	dobj $< -o $@

# Dependencies
startup/main.o: startup/defs.h

startup/exit.o: startup/defs.h

startup/wbmain.o: startup/defs.h

