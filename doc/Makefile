DTDVER=4.1.2
#DTD="/kdd2/web/misc/db4xml/docbookx.dtd"
#DTD="c:/doc/db4xml/docbookx.dtd"
DTD="http://www.oasis-open.org/docbook/xml/$(DTDVER)/docbookx.dtd"
IMPNOTES=impnotes.xml cl-ent.xml clhs-ent.xml impbody.xml impent.xml \
	impext.xml impissue.xml
RM=/bin/rm -f

ifneq (,$(filter Windows%,$(OS)))
NSGMLS=c:/gnu/sp/bin/nsgmls.exe
XMLSOC=c:/gnu/sp/pubtext/xml.soc
else
NSGMLS=nsgmls
XMLSOC=/usr/local/src/sp/pubtext/xml.soc
endif


impnotes.xml: impnotes.xml.in Makefile
	$(RM) $@
	echo '<?xml version="1.0"?>' >> $@
	echo '<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V'$(DTDVER)'//EN"' >> $@
	echo '    $(DTD) ['>> $@
	cat $< >> $@

check: $(IMPNOTES)
	$(NSGMLS) -s -wxml -c$(XMLSOC) impnotes.xml

impnotes.texi: $(IMPNOTES)
	docbook2texi impnotes.xml > $@

clean: force
	$(RM) impnotes.xml impnotes.texi

force:
