DTDVERS=3.1.7
DTD="/kdd2/web/misc/db3xml/docbookx.dtd"
#DTD="http://nwalsh.com/docbook/xml/$(DTDVERS)/docbookx.dtd"
IMPNOTES=impnotes.xml cl-ent.xml clhs-ent.xml impbody.xml impent.xml \
	impext.xml impissue.xml
RM=/bin/rm -f

impnotes.xml: impnotes.xml.in
	$(RM) $@
	echo '<?xml version="1.0"?>' >> $@
	echo '<!DOCTYPE book PUBLIC "-//Norman Walsh//DTD DocBk XML V'$(DTDVERS)'//EN"' >> $@
	echo '    $(DTD) ['>> $@
	cat $^ >> $@

check: $(IMPNOTES)
	nsgmls -s -wxml -c/usr/local/src/sp/pubtext/xml.soc impnotes.xml

impnotes.texi: $(IMPNOTES)
	docbook2texi impnotes.xml > $@

clean: force
	$(RM) impnotes.xml impnotes.texi

force:
