# Makefile for the character set conversion utilities
# Bruno Haible 8.7.1994


# Directories:
prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin


# Programs:
CC = cc
CFLAGS = -O
#CC = gcc
#CFLAGS = -O2 -fomit-frame-pointer -N -s

# Targets:

unix : $(bindir)/cv-to-ascii $(bindir)/touchr $(bindir)/to-ascii $(bindir)/all-to-ascii

dos : $(bindir)/crlf-to-nl $(bindir)/nl-to-crlf $(bindir)/cv-fr-ibmpc $(bindir)/cv-to-ibmpc $(bindir)/touchr $(bindir)/from-ibmpc $(bindir)/to-ibmpc $(bindir)/all-from-ibmpc $(bindir)/all-to-ibmpc

atari : $(bindir)/crlf-to-nl $(bindir)/nl-to-crlf $(bindir)/cv-fr-atari $(bindir)/cv-to-atari $(bindir)/touchr $(bindir)/from-atari $(bindir)/to-atari $(bindir)/all-from-atari $(bindir)/all-to-atari

hp : $(bindir)/cv-fr-hp $(bindir)/cv-to-hp $(bindir)/touchr $(bindir)/from-hp $(bindir)/to-hp $(bindir)/all-from-hp $(bindir)/all-to-hp

next : $(bindir)/cv-fr-next $(bindir)/cv-to-next $(bindir)/touchr $(bindir)/from-next $(bindir)/to-next $(bindir)/all-from-next $(bindir)/all-to-next


# CR/LF <--> NL conversion:

$(bindir)/crlf-to-nl : crlf-to-nl.c
	$(CC) $(CFLAGS) crlf-to-nl.c -o $(bindir)/crlf-to-nl

$(bindir)/nl-to-crlf : nl-to-crlf.c
	$(CC) $(CFLAGS) nl-to-crlf.c -o $(bindir)/nl-to-crlf

# functionally the same as crlf-to-nl, except that it converts "in place".
$(bindir)/fromdos : fromdos.c
	$(CC) $(CFLAGS) fromdos.c -o $(bindir)/fromdos


# The central character set is Latin-1 = ISO 8859-1.

# from/to Atari ST/TT character set

$(bindir)/cv-fr-atari : cv-fr-atari.c
	$(CC) $(CFLAGS) cv-fr-atari.c -o $(bindir)/cv-fr-atari

$(bindir)/cv-to-atari : cv-to-atari.c
	$(CC) $(CFLAGS) cv-to-atari.c -o $(bindir)/cv-to-atari

# from/to HP Roman8 character set

$(bindir)/cv-fr-hp : cv-fr-hp.c
	$(CC) $(CFLAGS) cv-fr-hp.c -o $(bindir)/cv-fr-hp

$(bindir)/cv-to-hp : cv-to-hp.c
	$(CC) $(CFLAGS) cv-to-hp.c -o $(bindir)/cv-to-hp

# from/to IBM PC character set

$(bindir)/cv-fr-ibmpc : cv-fr-ibmpc.c
	$(CC) $(CFLAGS) cv-fr-ibmpc.c -o $(bindir)/cv-fr-ibmpc

$(bindir)/cv-to-ibmpc : cv-to-ibmpc.c
	$(CC) $(CFLAGS) cv-to-ibmpc.c -o $(bindir)/cv-to-ibmpc

# from/to NeXTstep character set

$(bindir)/cv-fr-next : cv-fr-next.c
	$(CC) $(CFLAGS) cv-fr-next.c -o $(bindir)/cv-fr-next

$(bindir)/cv-to-next : cv-to-next.c
	$(CC) $(CFLAGS) cv-to-next.c -o $(bindir)/cv-to-next

# to plain Ascii character set

$(bindir)/cv-to-ascii : cv-to-ascii.c
	$(CC) $(CFLAGS) cv-to-ascii.c -o $(bindir)/cv-to-ascii


# Historically, the secondary character set is the Atari ST/TT character set.

# from/to HP Roman8 character set

$(bindir)/cvhpatari : cvhpatari.c
	$(CC) $(CFLAGS) cvhpatari.c -o $(bindir)/cvhpatari

$(bindir)/cvatarihp : cvatarihp.c
	$(CC) $(CFLAGS) cvatarihp.c -o $(bindir)/cvatarihp

# from/to IBM PC character set

$(bindir)/cvibmpcatari : cvibmpcatari.c
	$(CC) $(CFLAGS) cvibmpcatari.c -o $(bindir)/cvibmpcatari

$(bindir)/cvatariibmpc : cvatariibmpc.c
	$(CC) $(CFLAGS) cvatariibmpc.c -o $(bindir)/cvatariibmpc

# to plain Ascii character set

$(bindir)/cvatariascii : cvatariascii.c
	$(CC) $(CFLAGS) cvatariascii.c -o $(bindir)/cvatariascii


# Preserving the date:

$(bindir)/touchr : touchr.c
	$(CC) $(CFLAGS) touchr.c -o $(bindir)/touchr || (echo '#!/bin/sh' > $(bindir)/touchr ; chmod a+x $(bindir)/touchr)


# Wrappers that get two filename arguments and preserve the date:

$(bindir)/from-atari : from-atari
	sed -e 's,$${CHARSETBINDIR},$(bindir)/,g' < from-atari > $(bindir)/from-atari
	chmod a+x $(bindir)/from-atari

$(bindir)/to-atari : to-atari
	sed -e 's,$${CHARSETBINDIR},$(bindir)/,g' < to-atari > $(bindir)/to-atari
	chmod a+x $(bindir)/to-atari

$(bindir)/from-hp : from-hp
	sed -e 's,$${CHARSETBINDIR},$(bindir)/,g' < from-hp > $(bindir)/from-hp
	chmod a+x $(bindir)/from-hp

$(bindir)/to-hp : to-hp
	sed -e 's,$${CHARSETBINDIR},$(bindir)/,g' < to-hp > $(bindir)/to-hp
	chmod a+x $(bindir)/to-hp

$(bindir)/from-ibmpc : from-ibmpc
	sed -e 's,$${CHARSETBINDIR},$(bindir)/,g' < from-ibmpc > $(bindir)/from-ibmpc
	chmod a+x $(bindir)/from-ibmpc

$(bindir)/to-ibmpc : to-ibmpc
	sed -e 's,$${CHARSETBINDIR},$(bindir)/,g' < to-ibmpc > $(bindir)/to-ibmpc
	chmod a+x $(bindir)/to-ibmpc

$(bindir)/from-next : from-next
	sed -e 's,$${CHARSETBINDIR},$(bindir)/,g' < from-next > $(bindir)/from-next
	chmod a+x $(bindir)/from-next

$(bindir)/to-next : to-next
	sed -e 's,$${CHARSETBINDIR},$(bindir)/,g' < to-next > $(bindir)/to-next
	chmod a+x $(bindir)/to-next

$(bindir)/to-ascii : to-ascii
	sed -e 's,$${CHARSETBINDIR},$(bindir)/,g' < to-ascii > $(bindir)/to-ascii
	chmod a+x $(bindir)/to-ascii


# Wrappers that get any number of filename arguments:

$(bindir)/all-from-atari : all
	sed -e 's,CONVERTER,from-atari,g' -e 's,$${CHARSETBINDIR},$(bindir)/,g' < all > $(bindir)/all-from-atari
	chmod a+x $(bindir)/all-from-atari

$(bindir)/all-to-atari : all
	sed -e 's,CONVERTER,to-atari,g' -e 's,$${CHARSETBINDIR},$(bindir)/,g' < all > $(bindir)/all-to-atari
	chmod a+x $(bindir)/all-to-atari

$(bindir)/all-from-hp : all
	sed -e 's,CONVERTER,from-hp,g' -e 's,$${CHARSETBINDIR},$(bindir)/,g' < all > $(bindir)/all-from-hp
	chmod a+x $(bindir)/all-from-hp

$(bindir)/all-to-hp : all
	sed -e 's,CONVERTER,to-hp,g' -e 's,$${CHARSETBINDIR},$(bindir)/,g' < all > $(bindir)/all-to-hp
	chmod a+x $(bindir)/all-to-hp

$(bindir)/all-from-ibmpc : all
	sed -e 's,CONVERTER,from-ibmpc,g' -e 's,$${CHARSETBINDIR},$(bindir)/,g' < all > $(bindir)/all-from-ibmpc
	chmod a+x $(bindir)/all-from-ibmpc

$(bindir)/all-to-ibmpc : all
	sed -e 's,CONVERTER,to-ibmpc,g' -e 's,$${CHARSETBINDIR},$(bindir)/,g' < all > $(bindir)/all-to-ibmpc
	chmod a+x $(bindir)/all-to-ibmpc

$(bindir)/all-from-next : all
	sed -e 's,CONVERTER,from-next,g' -e 's,$${CHARSETBINDIR},$(bindir)/,g' < all > $(bindir)/all-from-next
	chmod a+x $(bindir)/all-from-next

$(bindir)/all-to-next : all
	sed -e 's,CONVERTER,to-next,g' -e 's,$${CHARSETBINDIR},$(bindir)/,g' < all > $(bindir)/all-to-next
	chmod a+x $(bindir)/all-to-next

$(bindir)/all-to-ascii : all
	sed -e 's,CONVERTER,to-ascii,g' -e 's,$${CHARSETBINDIR},$(bindir)/,g' < all > $(bindir)/all-to-ascii
	chmod a+x $(bindir)/all-to-ascii


clean : force
	rm -f *.s *.o core
	rm -f crlf-to-nl nl-to-crlf fromdos
	rm -f cv-fr-atari cv-to-atari cv-fr-hp cv-to-hp cv-fr-ibmpc cv-to-ibmpc cv-fr-next cv-to-next cv-to-ascii
	rm -f cvhpatari cvatarihp cvibmpcatari cvatariibmpc cvatariascii
	rm -f touchr

force :

