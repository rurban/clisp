# Makefile for the GNU C preprocessor

CC = cc
CFLAGS = -I. -O -DOUTPUT_EXTENDED_LINE_COMMANDS

# Choose which alloca() function to use.
# The builtin alloca():
ALLOCA =
# The portable alloca() from alloca.c:
#ALLOCA = alloca.o

cccp : cccp.o cexp.o version.o obstack.o $(ALLOCA)
	$(CC) $(CFLAGS) cccp.o cexp.o version.o obstack.o $(ALLOCA) -o cccp

cccp.o : cccp.c config.h
	$(CC) $(CFLAGS) -c cccp.c

cexp.o : cexp.c config.h
	$(CC) $(CFLAGS) -c cexp.c

version.o : version.c
	$(CC) $(CFLAGS) -c version.c

obstack.o : obstack.c obstack.h
	$(CC) $(CFLAGS) -c obstack.c

alloca.o : alloca.c
	$(CC) $(CFLAGS) -c alloca.c

