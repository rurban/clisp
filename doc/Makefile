# CLISP Implementation Notes generation
# (c) 2000-2003 Sam Steingold

DTDVER=4.2
IMPNOTES=impnotes.dsl cl-ent.xml clhs-ent.xml impbody.xml impent.xml	\
	impext.xml impissue.xml impbyte.xml unix-ent.xml		\
	../modules/dirkey/dirkey.xml ../modules/syscalls/syscalls.xml	\
	../modules/regexp/regexp.xml ../modules/netica/netica.xml \
	../modules/berkeley-db/berkeley-db.xml ../modules/pcre/pcre.xml \
	../modules/oracle/oracle.xml ../modules/fastcgi/fastcgi.xml
RM=/bin/rm -f
# DOC=$(shell pwd)

ifneq (,$(filter Windows%,$(OS)))
XMLHOME=c:/doc/xml
NSGMLS=d:/gnu/sp/bin/nsgmls.exe
XMLSOC=d:/gnu/sp/pubtext/xml.soc
JAVAXML=c:/java
JAVA=/cygdrive/c/java/jdk1.3/bin/java.exe
SEP=;
DTD=$(XMLHOME)/db4xml/docbookx.dtd
else
XMLHOME=/usr/local/src/xml
NSGMLS=nsgmls
# XMLSOC=/usr/local/src/sp/pubtext/xml.soc
XMLSOC=/usr/share/sgml/xml.soc
JAVAXML=/usr/local/src/xml
JAVA=java
SEP=:
DTD=/usr/share/sgml/docbook/xml-dtd-$(DTDVER)/docbookx.dtd
endif

XALAN=$(JAVAXML)/xalan-j_2_0_0
SAXON=$(JAVAXML)/saxon
CLASSPATH=$(XALAN)/bin/xerces.jar$(SEP)$(XALAN)/bin/xalan.jar$(SEP)$(SAXON)/saxon.jar

TEXI2HTML=texi2html

DIST=cvs2.cons.org:/home/ftp/pub/lisp/clisp/snapshots

# this is the original DTD
# when processing with a tool which can get the DTD from the net,
# e.g., (open)jade, you can use this definition,
# otherwise you have to stick with the local DTDs
DTD=http://www.oasis-open.org/docbook/xml/$(DTDVER)/docbookx.dtd

# to generate HTML, you will need:
# http://xml.apache.org/xalan-j
# http://nwalsh.com/docbook/xsl/dbx129.zip
#  or
# http://users.iclway.co.uk/mhkay/saxon/
#  or
# http://openjade.sourceforge.net/
# http://www.jclark.com/jade/

all: check impnotes.html regexp.html wildcard.html

impnotes.xml: impnotes.xml.in $(IMPNOTES) ../src/VERSION Makefile
	$(RM) $@
	sed -e 's,@DTD@,$(DTD),' \
	    -e 's,@DTDVER@,$(DTDVER),' \
	    -e 's,@VERSION@,'`cat ../src/VERSION`',' \
	    -e 's,@TODAY@,'`date +"%Y-%m-%d"`',' $< > $@

check: impnotes.xml $(IMPNOTES)
	$(NSGMLS) -s -wxml -c$(XMLSOC) $<

impnotes.texi: impnotes.xml $(IMPNOTES)
	time docbook2texi $<

impnotes.html: impnotes.xml $(IMPNOTES) # tidy.conf
	time docbook2html --dsl impnotes.dsl -u $<
#	$(JAVA) -classpath "$(CLASSPATH)" com.icl.saxon.StyleSheet \
#		-t -dt -o $@ $< $(XMLHOME)/docbook.xsl/xhtml/docbook.xsl \
#		process.source.toc=1 html.stylesheet=impnotes.css
#	$(JAVA) -classpath "$(CLASSPATH)" \
#		org.apache.xalan.xslt.Process \
#		-diag -edump -in $< -out $@ \
#		-xsl "file://$(XMLHOME)/docbook.xsl/xhtml/docbook.xsl"
#	cp impnotes.html impnotes-saved.html
#	tidy -config tidy.conf -f tidy.err impnotes.html || true
	egrep -Hn 'NAME="[A-Z0-9]*"' $@ || true
	(test -n "$(DIST)" && scp -p impnotes.html $(DIST) && \
	 test -d "../build/" && cd ../build && make clisp.html && \
	 make clisp.1 && scp -p clisp.1 clisp.html $(DIST)) || true

impnotes.pdf: impnotes.xml $(IMPNOTES)
	time docbook2pdf $<
	$(RM) impnotes.out

html: impnotes.xml $(IMPNOTES)
	if [ ! -h $@ ]; then $(RM) -r $@; ln -s ../../gnu/clisp/impnotes $@; fi
	cd html && sed 's/^/ln -vfs /' .symlinks | sh -
	time docbook2html --dsl impnotes.dsl -o $@ $<
	egrep -Hnr 'NAME="[A-Z0-9]*"' $@ || true
	(test -n "$(DIST)" && scp -p html/*.html $(DIST)/impnotes/;) || true

up: impnotes.html html
	scp impnotes.html sf:/home/groups/c/cl/clisp/www/impnotes-new.html
	scp -r html/* sf:/home/groups/c/cl/clisp/www/impnotes-new/

regexp.html: ../modules/regexp/regexp.texinfo
	$(TEXI2HTML) -monolithic -verbose $<

wildcard.html: ../modules/wildcard/wildcard.texinfo
	$(TEXI2HTML) -monolithic -verbose $<

clean: force
	$(RM) impnotes.xml impnotes.texi impnotes.html impnotes.pdf html

count: $(IMPNOTES)
	wc $^

force:
