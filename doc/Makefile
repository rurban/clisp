# CLISP Implementation Notes generation
# (c) 2000-2007 Sam Steingold

# To generate HTML, you will need xsltproc & xmllint from the libxml2
# (http://xmlsoft.org/) package in addition to the DocBook DTDs and XSL
# stylesheets (http://www.docbook.org/)
# When writing docbook documentation, you need the "DocBook Definitive Guide"
# http://docbook.org/tdg/en/html/docbook-x.html

# which DTD to validate against
DTDVER=4.5

# all included XML files
IMPNOTES=cl-ent.xml clhs-ent.xml impbody.xml impent.xml mop.xml gray.xml     \
	impext.xml impissue.xml impbyte.xml unix-ent.xml mop-ent.xml faq.xml \
	history.xml \
	../modules/berkeley-db/berkeley-db.xml	\
	../modules/berkeley-db/ent-bdb.xml	\
	../modules/dirkey/dirkey.xml		\
	../modules/fastcgi/fastcgi.xml		\
	../modules/gdbm/gdbm.xml		\
	../modules/gtk2/gtk.xml			\
	../modules/i18n/i18n.xml		\
	../modules/libsvm/svm.xml		\
	../modules/matlab/matlab.xml		\
	../modules/netica/netica.xml		\
	../modules/oracle/oracle.xml		\
	../modules/pari/pari.xml		\
	../modules/pcre/pcre.xml		\
	../modules/postgresql/postgresql.xml	\
	../modules/rawsock/rawsock.xml		\
	../modules/readline/readline.xml	\
	../modules/regexp/regexp.xml		\
	../modules/syscalls/syscalls.xml	\
	../modules/wildcard/wildcard.xml	\
	../modules/zlib/zlib.xml
# targets for <olink> resolution
OLINK=tdb-man.xml tdb-pile.xml tdb-chunk.xml olink-pile.xml olink-chunk.xml

RM=/bin/rm -f

# what to do when there is no local system-wide DTD & XSL?
DOWNLOAD=yes

ifneq (,$(filter Windows%,$(OS)))
DTD_PATH=/usr/share/docbook-xml$(DTDVER)/docbookx.dtd
# Make sure to use a trailing slash.
XSL_PATH=$(firstword $(wildcard /usr/share/docbook-xsl*/))
FOP=/cygdrive/c/java/fop-0.20.5/fop.bat
else
DTD_PATH=$(firstword $(wildcard \
	/usr/share/sgml/docbook/xml-dtd-$(DTDVER)*/docbookx.dtd))
XSL_PATH=$(firstword $(wildcard /usr/share/sgml/docbook/xsl-stylesheets1*/))
FOP=../../../fop/fop
endif

# common dependencies for all checkers
CHECK_DEPS = impnotes.xml clisp.xml
ifeq (,$(wildcard $(DTD_PATH))) # system DTD does not exist
ifeq (yes,$(DOWNLOAD))		# download once
CHECK_DEPS += docbook-dtd
DTD_PATH=./docbook-dtd/docbookx.dtd
else # download on each check (you need network-aware tools to use it)
# this is the original DTD
DTD_PATH=http://www.oasis-open.org/docbook/xml/$(DTDVER)/docbookx.dtd
#DTD_PATH=http://www.docbook.org/xml/$(DTDVER)/docbookx.dtd
endif
endif

# common dependencies for all generators
GEN_DEPS = common.xsl
ifeq (,$(wildcard $(XSL_PATH))) # system stylesheets do not exist
ifeq (yes,$(DOWNLOAD))	       # download once
GEN_DEPS += docbook-xsl
XSL_PATH=./docbook-xsl/
else # download on each build (you need network-aware tools to use it)
XSL_PATH=http://docbook.xml-doc.org/snapshots/xsl/
# XSL_PATH=http://docbook.sourceforge.net/release/xsl/snapshot/
# XSL_PATH=http://docbook.sourceforge.net/release/xsl/current/
endif
endif

# The xmlcatalog utility (from libxml2)
XMLCATALOG=xmlcatalog

# chose one:
#TEXI2HTML=texi2html -monolithic -verbose $< -out_file $@
TEXI2HTML=makeinfo --verbose --no-split --no-headers --html $< -o $@

DIST=podval:~/public_html/clisp

##### nothing user-serviceable below #####

# name of XML catalog file to create
# (if not specified, no local catalog created or used)
CATALOG_FILE=catalog.xml

# Stylesheets URI used internally in the CLISP project *.xsl files.
XSL_URI=http://docbook.sourceforge.net/release/xsl/current/

FILLIN=. ../version.sh && sed -e 's,@DTD@,$(DTD_PATH),' \
	    -e 's,@DTDVER@,$(DTDVER),' \
	    -e 's,@VERSION@,'$${VERSION_NUMBER}',' \
	    -e 's,@TODAY@,'`date +"%Y-%m-%d"`','

# cannot use profiling <http://www.sagehill.net/docbookxsl/Profiling.html>
# because that would require XSL tools to be installed on user machine
SGML_UNCOMMENT=-e 's/^<!--\#\(.*\)-->$$/\#\1/'

E=\ *<!--\(\#[^-]*\)--> *
ROFF_UNCOMMENT=-e 's/^$E$$/\1/' -e 's/$E$$/\n\1/' -e 's/^$E/\1\n/' -e 's/$E/\n\1\n/g'

all: check impnotes.html regexp.html man

ifneq (,$(CATALOG_FILE))
# depends on Makefile because of $(XSL_PATH)
$(CATALOG_FILE): Makefile
	$(RM) $(CATALOG_FILE)
	$(XMLCATALOG) --create > $(CATALOG_FILE)
	$(XMLCATALOG) --noout --add "rewriteURI" \
		$(XSL_URI) $(XSL_PATH) $(CATALOG_FILE)
	$(XMLCATALOG) --noout --add "rewriteSystem" \
		$(XSL_URI) $(XSL_PATH) $(CATALOG_FILE)
XSLTPROC=XML_CATALOG_FILES="$(CATALOG_FILE) ${XML_CATALOG_FILES}" xsltproc
else
XSLTPROC=xsltproc
endif
GOOGLEADS=0

MAKE_TDB=$(XSLTPROC) --timing --stringparam collect.xref.targets "only" \
	--stringparam targets.filename

tdb-man.xml: clisp.xml pile.xsl $(GEN_DEPS) $(CATALOG_FILE)
	$(MAKE_TDB) "$@" pile.xsl $<

tdb-pile.xml: impnotes.xml $(IMPNOTES) pile.xsl $(GEN_DEPS) $(CATALOG_FILE)
	$(MAKE_TDB) "$@" pile.xsl $<

tdb-chunk.xml: impnotes.xml $(IMPNOTES) chunk.xsl $(GEN_DEPS) $(CATALOG_FILE)
	$(MAKE_TDB) "$@" chunk.xsl $<

clisp.xml: clisp.xml.in ../version.sh Makefile
	$(RM) $@
	$(FILLIN) $< > $@

XMLOUT=$(XSLTPROC) --timing --param google.ads $(GOOGLEADS) \
	--stringparam target.database.document
DOC_MAN=--stringparam current.docid "man"
DOC_IMP=--stringparam current.docid "impnotes"

clisp.html: clisp.xml $(GEN_DEPS) pile.xsl $(OLINK)
	$(XMLOUT) "olink-pile.xml" $(DOC_MAN) -o $@ pile.xsl $<

_clisp.html: clisp.html
	sed $(SGML_UNCOMMENT) $< > $@

clisp.1: clisp.xml $(GEN_DEPS) man.xsl $(OLINK)
	$(XMLOUT) "olink-pile.xml" $(DOC_MAN) -o $@ man.xsl $<

_clisp.1: clisp.1
	sed $(ROFF_UNCOMMENT) $< > $@

impnotes.xml: impnotes.xml.in $(IMPNOTES) ../version.sh Makefile
	$(RM) $@
	$(FILLIN) $< > $@

check: $(IMPNOTES) $(CHECK_DEPS)
	xmllint --noout --valid --postvalid --timing --noent clisp.xml
	xmllint --noout --valid --postvalid --timing --noent impnotes.xml

impnotes.texi: impnotes.xml $(IMPNOTES)
	time docbook2texi $<

impnotes.html: impnotes.xml $(IMPNOTES) $(GEN_DEPS) pile.xsl $(OLINK)
	$(XMLOUT) "olink-pile.xml" $(DOC_IMP) -o $@ pile.xsl $<
#	cp impnotes.html impnotes-saved.html
#	tidy -config tidy.conf -f tidy.err impnotes.html || true
	egrep -Hn 'id="id[0-9]*"' $@ || true
	(test -n "$(DIST)" && scp -p impnotes.html impnotes.css $(DIST)/) \
		|| true

man: _clisp.html _clisp.1
	(cd ../build && make clisp.html clisp.1 && \
	 test -n "$(DIST)" && scp -p clisp.1 clisp.html $(DIST)/) || true

impnotes.fo: impnotes.xml $(IMPNOTES) fo.xsl $(XSL_TARGET)
	$(XMLOUT) "olink-pile.xml" $(DOC_IMP) -o $@ fo.xsl $<

# http://xmlgraphics.apache.org/fop/
impnotes.pdf: impnotes.fo
	${FOP} $< $@

html: impnotes.xml $(IMPNOTES) $(GEN_DEPS) chunk.xsl pile.xsl clisp.xml $(OLINK)
	if [ ! -h $@ ]; then $(RM) -r $@; ln -s ../../sf/www/impnotes $@; fi
	cd html && grep -v -e "^#" .symlinks | sed 's/^/ln -vf /' | sh -
	$(XMLOUT) "olink-chunk.xml" $(DOC_IMP) -o $@/ chunk.xsl $<
	$(XSLTPROC) --timing -o $@/id-href.map id-href-map.xsl tdb-chunk.xml
	$(XMLOUT) "olink-chunk.xml" $(DOC_MAN) -o $@/clisp.html pile.xsl clisp.xml
	sed $(SGML_UNCOMMENT) $@/clisp.html > _clisp-1.html
	cd ../build; make clisp-1.html;
	mv -f ../build/clisp-1.html $@/clisp.html
	rm -f _clisp-1.html
	egrep -Hnr 'id="id[0-9]*"' $@ || true
	(test -n "$(DIST)" && scp -p html/*.html html/id-href.map impnotes.css \
	 $(DIST)/impnotes/;) || true

up: impnotes.html html
	scp impnotes.html sf:/home/groups/c/cl/clisp/www/impnotes-new.html
	scp -r html/* sf:/home/groups/c/cl/clisp/www/impnotes-new/

regexp.html: ../modules/regexp/regexp.texinfo
	$(TEXI2HTML)

clean: force
	$(RM) impnotes.xml impnotes.texi impnotes.html impnotes.pdf html \
	  clisp.xml clisp.html clisp.1 tdb-*.xml $(CATALOG_FILE) impnotes.fo

count: $(IMPNOTES)
	wc $^

DTD_DOWNLOAD=http://www.docbook.org/xml/$(DTDVER)/docbook-xml-$(DTDVER).zip
docbook-dtd :
	mkdir docbook-dtd && cd docbook-dtd && \
	  wget -O docbook-xml.zip $(DTD_DOWNLOAD) && \
	  unzip docbook-xml.zip && $(RM) docbook-xml.zip

docbook-xsl :
	rsync -av --port=5873 docbook.xml-doc.org::xsl docbook-xsl

SEARCH=
search: force
	@if test -z "$(SEARCH)"; then echo "usage: make search SEARCH=string"; \
	else grep -n '$(SEARCH)' $(IMPNOTES); fi

force:
