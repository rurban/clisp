DTDVER=4.1.2
IMPNOTES=impnotes.xml cl-ent.xml clhs-ent.xml impbody.xml impent.xml \
	impext.xml impissue.xml
RM=/bin/rm -f
# DOC=$(shell pwd)

ifneq (,$(filter Windows%,$(OS)))
XMLHOME=c:/doc/xml
NSGMLS=/cygdrive/c/gnu/sp/bin/nsgmls.exe
XMLSOC=c:/gnu/sp/pubtext/xml.soc
XALAN=c:/java/xalan-j_2_0_0
CLASSPATH=$(XALAN)/bin/xerces.jar;$(XALAN)/bin/xalan.jar
JAVA=/cygdrive/c/java/jdk1.3/bin/java.exe
else
XMLHOME=/usr/local/src/xml
NSGMLS=nsgmls
XMLSOC=/usr/local/src/sp/pubtext/xml.soc
XALAN=/usr/local/src/xml/xalan-j_2_0_0
CLASSPATH=$(XALAN)/bin/xerces.jar:$(XALAN)/bin/xalan.jar \
JAVA=java
endif

DBHOME=$(XMLHOME)/db4xml

# this is the original DTD
# DTD="http://www.oasis-open.org/docbook/xml/$(DTDVER)"

# to generate HTML, you will need:
# http://xml.apache.org/xalan-j
# http://nwalsh.com/docbook/xsl/dbx129.zip

impnotes.xml: impnotes.xml.in Makefile
	$(RM) $@
	echo '<?xml version="1.0" encoding="UTF-8"?>' >> $@
	echo '<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V'$(DTDVER)'//EN"' >> $@
	echo '    "$(DBHOME)/docbookx.dtd" ['>> $@
	cat $< >> $@

check: impnotes.xml $(IMPNOTES)
	$(NSGMLS) -s -wxml -c$(XMLSOC) $<

impnotes.texi: $(IMPNOTES)
	docbook2texi impnotes.xml > $@

impnotes.html: impnotes.xml $(IMPNOTES)
	$(JAVA) -classpath "$(CLASSPATH)" \
		org.apache.xalan.xslt.Process \
		-diag -edump -in $< -out $@ \
		-xsl "file://$(XMLHOME)/docbook-xsl/html/docbook.xsl"

clean: force
	$(RM) impnotes.xml impnotes.texi impnotes.html

force:
