DTD="/kdd2/web/misc/db3xml/docbookx.dtd"
# "http://nwalsh.com/docbook/xml/3.1.4/docbookx.dtd"

impnotes.xml: impnotes.xml.in
	echo '<?xml version="1.0"?>' >> impnotes.xml
	echo '<!DOCTYPE book PUBLIC "-//Norman Walsh//DTD DocBk XML V3.1.4//EN"' >> impnotes.xml
	echo '${DTD} [ '>> impnotes.xml
	cat impnotes.xml.in >> impnotes.xml

check: impnotes.xml
	nsgmls -s -wxml -c/usr/local/src/sp/pubtext/xml.soc impnotes.xml

clean: force
	/bin/rm -f impnotes.xml

force:
