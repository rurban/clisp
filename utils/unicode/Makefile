
# Destination directory. Define to empty for the current directory.
DESTDIR = ../../src/

ALL = $(DESTDIR)uni_upcase.c $(DESTDIR)uni_downcase.c $(DESTDIR)uni_attribute.c \
 $(DESTDIR)nls_ascii.c \
 $(DESTDIR)nls_iso8859_1.c \
 $(DESTDIR)nls_iso8859_2.c \
 $(DESTDIR)nls_iso8859_3.c \
 $(DESTDIR)nls_iso8859_4.c \
 $(DESTDIR)nls_iso8859_5.c \
 $(DESTDIR)nls_iso8859_6.c \
 $(DESTDIR)nls_iso8859_7.c \
 $(DESTDIR)nls_iso8859_8.c \
 $(DESTDIR)nls_iso8859_9.c \
 $(DESTDIR)nls_iso8859_10.c \
 $(DESTDIR)nls_iso8859_13.c \
 $(DESTDIR)nls_iso8859_14.c \
 $(DESTDIR)nls_iso8859_15.c \
 $(DESTDIR)nls_koi8_r.c \
 $(DESTDIR)nls_mac_arabic.c \
 $(DESTDIR)nls_mac_centraleurope.c \
 $(DESTDIR)nls_mac_croatian.c \
 $(DESTDIR)nls_mac_cyrillic.c \
 $(DESTDIR)nls_mac_dingbat.c \
 $(DESTDIR)nls_mac_greek.c \
 $(DESTDIR)nls_mac_hebrew.c \
 $(DESTDIR)nls_mac_iceland.c \
 $(DESTDIR)nls_mac_roman.c \
 $(DESTDIR)nls_mac_romania.c \
 $(DESTDIR)nls_mac_symbol.c \
 $(DESTDIR)nls_mac_thai.c \
 $(DESTDIR)nls_mac_turkish.c \
 $(DESTDIR)nls_mac_ukraine.c \
 $(DESTDIR)nls_cp437_ms.c \
 $(DESTDIR)nls_cp437_ibm.c \
 $(DESTDIR)nls_cp737.c \
 $(DESTDIR)nls_cp775.c \
 $(DESTDIR)nls_cp850.c \
 $(DESTDIR)nls_cp852_ms.c \
 $(DESTDIR)nls_cp852_ibm.c \
 $(DESTDIR)nls_cp855.c \
 $(DESTDIR)nls_cp857.c \
 $(DESTDIR)nls_cp860_ms.c \
 $(DESTDIR)nls_cp860_ibm.c \
 $(DESTDIR)nls_cp861_ms.c \
 $(DESTDIR)nls_cp861_ibm.c \
 $(DESTDIR)nls_cp862_ms.c \
 $(DESTDIR)nls_cp862_ibm.c \
 $(DESTDIR)nls_cp863_ms.c \
 $(DESTDIR)nls_cp863_ibm.c \
 $(DESTDIR)nls_cp864_ms.c \
 $(DESTDIR)nls_cp864_ibm.c \
 $(DESTDIR)nls_cp865_ms.c \
 $(DESTDIR)nls_cp865_ibm.c \
 $(DESTDIR)nls_cp866.c \
 $(DESTDIR)nls_cp869_ms.c \
 $(DESTDIR)nls_cp869_ibm.c \
 $(DESTDIR)nls_cp874_ms.c \
 $(DESTDIR)nls_cp874_ibm.c \
 $(DESTDIR)nls_cp1250.c \
 $(DESTDIR)nls_cp1251.c \
 $(DESTDIR)nls_cp1252.c \
 $(DESTDIR)nls_cp1253.c \
 $(DESTDIR)nls_cp1254.c \
 $(DESTDIR)nls_cp1255.c \
 $(DESTDIR)nls_cp1256.c \
 $(DESTDIR)nls_cp1257.c \
 $(DESTDIR)nls_cp1258.c \
 $(DESTDIR)nls_hp_roman8.c \
 $(DESTDIR)nls_nextstep.c

JAVA = java
JAVAC = javac -O
ICONV = iconv_prog --loose

all : $(ALL)

$(DESTDIR)uni_upcase.c : upcase_table.class
	$(JAVA) upcase_table uni_upcase.c $(DESTDIR)

upcase_table.class : upcase_table.java
	$(JAVAC) upcase_table.java

$(DESTDIR)uni_downcase.c : downcase_table.class
	$(JAVA) downcase_table uni_downcase.c $(DESTDIR)

downcase_table.class : downcase_table.java
	$(JAVAC) downcase_table.java

$(DESTDIR)uni_attribute.c : attribute_table.class
	$(JAVA) attribute_table uni_attribute.c $(DESTDIR)

attribute_table.class : attribute_table.java
	$(JAVAC) attribute_table.java

# All the .in files are in the following format:
# One line per input character (except 0x0A and 0x0D), value being the
# corresponding character (if ASCII) or a \unnnn escape sequence.

$(DESTDIR)nls_ascii.c : nls_ascii.in nls_table.class
	$(JAVA) nls_table nls_ascii.c $(DESTDIR) < nls_ascii.in

$(DESTDIR)nls_iso8859_1.c : nls_iso8859_1.in nls_table.class
	$(JAVA) nls_table nls_iso8859_1.c $(DESTDIR) < nls_iso8859_1.in

$(DESTDIR)nls_iso8859_2.c : nls_iso8859_2.in nls_table.class
	$(JAVA) nls_table nls_iso8859_2.c $(DESTDIR) < nls_iso8859_2.in

$(DESTDIR)nls_iso8859_3.c : nls_iso8859_3.in nls_table.class
	$(JAVA) nls_table nls_iso8859_3.c $(DESTDIR) < nls_iso8859_3.in

$(DESTDIR)nls_iso8859_4.c : nls_iso8859_4.in nls_table.class
	$(JAVA) nls_table nls_iso8859_4.c $(DESTDIR) < nls_iso8859_4.in

$(DESTDIR)nls_iso8859_5.c : nls_iso8859_5.in nls_table.class
	$(JAVA) nls_table nls_iso8859_5.c $(DESTDIR) < nls_iso8859_5.in

$(DESTDIR)nls_iso8859_6.c : nls_iso8859_6.in nls_table.class
	$(JAVA) nls_table nls_iso8859_6.c $(DESTDIR) < nls_iso8859_6.in

$(DESTDIR)nls_iso8859_7.c : nls_iso8859_7.in nls_table.class
	$(JAVA) nls_table nls_iso8859_7.c $(DESTDIR) < nls_iso8859_7.in

$(DESTDIR)nls_iso8859_8.c : nls_iso8859_8.in nls_table.class
	$(JAVA) nls_table nls_iso8859_8.c $(DESTDIR) < nls_iso8859_8.in

$(DESTDIR)nls_iso8859_9.c : nls_iso8859_9.in nls_table.class
	$(JAVA) nls_table nls_iso8859_9.c $(DESTDIR) < nls_iso8859_9.in

$(DESTDIR)nls_iso8859_10.c : nls_iso8859_10.in nls_table.class
	$(JAVA) nls_table nls_iso8859_10.c $(DESTDIR) < nls_iso8859_10.in

$(DESTDIR)nls_iso8859_13.c : nls_iso8859_13.in nls_table.class
	$(JAVA) nls_table nls_iso8859_13.c $(DESTDIR) < nls_iso8859_13.in

$(DESTDIR)nls_iso8859_14.c : nls_iso8859_14.in nls_table.class
	$(JAVA) nls_table nls_iso8859_14.c $(DESTDIR) < nls_iso8859_14.in

$(DESTDIR)nls_iso8859_15.c : nls_iso8859_15.in nls_table.class
	$(JAVA) nls_table nls_iso8859_15.c $(DESTDIR) < nls_iso8859_15.in

$(DESTDIR)nls_koi8_r.c : nls_koi8_r.in nls_table.class
	$(JAVA) nls_table nls_koi8_r.c $(DESTDIR) < nls_koi8_r.in

$(DESTDIR)nls_mac_arabic.c : nls_mac_arabic.in nls_table.class
	$(JAVA) nls_table nls_mac_arabic.c $(DESTDIR) < nls_mac_arabic.in

$(DESTDIR)nls_mac_centraleurope.c : nls_mac_centraleurope.in nls_table.class
	$(JAVA) nls_table nls_mac_centraleurope.c $(DESTDIR) < nls_mac_centraleurope.in

$(DESTDIR)nls_mac_croatian.c : nls_mac_croatian.in nls_table.class
	$(JAVA) nls_table nls_mac_croatian.c $(DESTDIR) < nls_mac_croatian.in

$(DESTDIR)nls_mac_cyrillic.c : nls_mac_cyrillic.in nls_table.class
	$(JAVA) nls_table nls_mac_cyrillic.c $(DESTDIR) < nls_mac_cyrillic.in

$(DESTDIR)nls_mac_dingbat.c : nls_mac_dingbat.in nls_table.class
	$(JAVA) nls_table nls_mac_dingbat.c $(DESTDIR) < nls_mac_dingbat.in

$(DESTDIR)nls_mac_greek.c : nls_mac_greek.in nls_table.class
	$(JAVA) nls_table nls_mac_greek.c $(DESTDIR) < nls_mac_greek.in

$(DESTDIR)nls_mac_hebrew.c : nls_mac_hebrew.in nls_table.class
	$(JAVA) nls_table nls_mac_hebrew.c $(DESTDIR) < nls_mac_hebrew.in

$(DESTDIR)nls_mac_iceland.c : nls_mac_iceland.in nls_table.class
	$(JAVA) nls_table nls_mac_iceland.c $(DESTDIR) < nls_mac_iceland.in

$(DESTDIR)nls_mac_roman.c : nls_mac_roman.in nls_table.class
	$(JAVA) nls_table nls_mac_roman.c $(DESTDIR) < nls_mac_roman.in

$(DESTDIR)nls_mac_romania.c : nls_mac_romania.in nls_table.class
	$(JAVA) nls_table nls_mac_romania.c $(DESTDIR) < nls_mac_romania.in

$(DESTDIR)nls_mac_symbol.c : nls_mac_symbol.in nls_table.class
	$(JAVA) nls_table nls_mac_symbol.c $(DESTDIR) < nls_mac_symbol.in

$(DESTDIR)nls_mac_thai.c : nls_mac_thai.in nls_table.class
	$(JAVA) nls_table nls_mac_thai.c $(DESTDIR) < nls_mac_thai.in

$(DESTDIR)nls_mac_turkish.c : nls_mac_turkish.in nls_table.class
	$(JAVA) nls_table nls_mac_turkish.c $(DESTDIR) < nls_mac_turkish.in

$(DESTDIR)nls_mac_ukraine.c : nls_mac_ukraine.in nls_table.class
	$(JAVA) nls_table nls_mac_ukraine.c $(DESTDIR) < nls_mac_ukraine.in

$(DESTDIR)nls_cp437_ms.c : nls_cp437_ms.in nls_table.class
	$(JAVA) nls_table nls_cp437_ms.c $(DESTDIR) < nls_cp437_ms.in

$(DESTDIR)nls_cp437_ibm.c : nls_cp437_ibm.in nls_table.class
	$(JAVA) nls_table nls_cp437_ibm.c $(DESTDIR) < nls_cp437_ibm.in

$(DESTDIR)nls_cp737.c : nls_cp737.in nls_table.class
	$(JAVA) nls_table nls_cp737.c $(DESTDIR) < nls_cp737.in

$(DESTDIR)nls_cp775.c : nls_cp775.in nls_table.class
	$(JAVA) nls_table nls_cp775.c $(DESTDIR) < nls_cp775.in

$(DESTDIR)nls_cp850.c : nls_cp850.in nls_table.class
	$(JAVA) nls_table nls_cp850.c $(DESTDIR) < nls_cp850.in

$(DESTDIR)nls_cp852_ms.c : nls_cp852_ms.in nls_table.class
	$(JAVA) nls_table nls_cp852_ms.c $(DESTDIR) < nls_cp852_ms.in

$(DESTDIR)nls_cp852_ibm.c : nls_cp852_ibm.in nls_table.class
	$(JAVA) nls_table nls_cp852_ibm.c $(DESTDIR) < nls_cp852_ibm.in

$(DESTDIR)nls_cp855.c : nls_cp855.in nls_table.class
	$(JAVA) nls_table nls_cp855.c $(DESTDIR) < nls_cp855.in

$(DESTDIR)nls_cp857.c : nls_cp857.in nls_table.class
	$(JAVA) nls_table nls_cp857.c $(DESTDIR) < nls_cp857.in

$(DESTDIR)nls_cp860_ms.c : nls_cp860_ms.in nls_table.class
	$(JAVA) nls_table nls_cp860_ms.c $(DESTDIR) < nls_cp860_ms.in

$(DESTDIR)nls_cp860_ibm.c : nls_cp860_ibm.in nls_table.class
	$(JAVA) nls_table nls_cp860_ibm.c $(DESTDIR) < nls_cp860_ibm.in

$(DESTDIR)nls_cp861_ms.c : nls_cp861_ms.in nls_table.class
	$(JAVA) nls_table nls_cp861_ms.c $(DESTDIR) < nls_cp861_ms.in

$(DESTDIR)nls_cp861_ibm.c : nls_cp861_ibm.in nls_table.class
	$(JAVA) nls_table nls_cp861_ibm.c $(DESTDIR) < nls_cp861_ibm.in

$(DESTDIR)nls_cp862_ms.c : nls_cp862_ms.in nls_table.class
	$(JAVA) nls_table nls_cp862_ms.c $(DESTDIR) < nls_cp862_ms.in

$(DESTDIR)nls_cp862_ibm.c : nls_cp862_ibm.in nls_table.class
	$(JAVA) nls_table nls_cp862_ibm.c $(DESTDIR) < nls_cp862_ibm.in

$(DESTDIR)nls_cp863_ms.c : nls_cp863_ms.in nls_table.class
	$(JAVA) nls_table nls_cp863_ms.c $(DESTDIR) < nls_cp863_ms.in

$(DESTDIR)nls_cp863_ibm.c : nls_cp863_ibm.in nls_table.class
	$(JAVA) nls_table nls_cp863_ibm.c $(DESTDIR) < nls_cp863_ibm.in

$(DESTDIR)nls_cp864_ms.c : nls_cp864_ms.in nls_table.class
	$(JAVA) nls_table nls_cp864_ms.c $(DESTDIR) < nls_cp864_ms.in

$(DESTDIR)nls_cp864_ibm.c : nls_cp864_ibm.in nls_table.class
	$(JAVA) nls_table nls_cp864_ibm.c $(DESTDIR) < nls_cp864_ibm.in

$(DESTDIR)nls_cp865_ms.c : nls_cp865_ms.in nls_table.class
	$(JAVA) nls_table nls_cp865_ms.c $(DESTDIR) < nls_cp865_ms.in

$(DESTDIR)nls_cp865_ibm.c : nls_cp865_ibm.in nls_table.class
	$(JAVA) nls_table nls_cp865_ibm.c $(DESTDIR) < nls_cp865_ibm.in

$(DESTDIR)nls_cp866.c : nls_cp866.in nls_table.class
	$(JAVA) nls_table nls_cp866.c $(DESTDIR) < nls_cp866.in

$(DESTDIR)nls_cp869_ms.c : nls_cp869_ms.in nls_table.class
	$(JAVA) nls_table nls_cp869_ms.c $(DESTDIR) < nls_cp869_ms.in

$(DESTDIR)nls_cp869_ibm.c : nls_cp869_ibm.in nls_table.class
	$(JAVA) nls_table nls_cp869_ibm.c $(DESTDIR) < nls_cp869_ibm.in

$(DESTDIR)nls_cp874_ms.c : nls_cp874_ms.in nls_table.class
	$(JAVA) nls_table nls_cp874_ms.c $(DESTDIR) < nls_cp874_ms.in

$(DESTDIR)nls_cp874_ibm.c : nls_cp874_ibm.in nls_table.class
	$(JAVA) nls_table nls_cp874_ibm.c $(DESTDIR) < nls_cp874_ibm.in

$(DESTDIR)nls_cp1250.c : nls_cp1250.in nls_table.class
	$(JAVA) nls_table nls_cp1250.c $(DESTDIR) < nls_cp1250.in

$(DESTDIR)nls_cp1251.c : nls_cp1251.in nls_table.class
	$(JAVA) nls_table nls_cp1251.c $(DESTDIR) < nls_cp1251.in

$(DESTDIR)nls_cp1252.c : nls_cp1252.in nls_table.class
	$(JAVA) nls_table nls_cp1252.c $(DESTDIR) < nls_cp1252.in

$(DESTDIR)nls_cp1253.c : nls_cp1253.in nls_table.class
	$(JAVA) nls_table nls_cp1253.c $(DESTDIR) < nls_cp1253.in

$(DESTDIR)nls_cp1254.c : nls_cp1254.in nls_table.class
	$(JAVA) nls_table nls_cp1254.c $(DESTDIR) < nls_cp1254.in

$(DESTDIR)nls_cp1255.c : nls_cp1255.in nls_table.class
	$(JAVA) nls_table nls_cp1255.c $(DESTDIR) < nls_cp1255.in

$(DESTDIR)nls_cp1256.c : nls_cp1256.in nls_table.class
	$(JAVA) nls_table nls_cp1256.c $(DESTDIR) < nls_cp1256.in

$(DESTDIR)nls_cp1257.c : nls_cp1257.in nls_table.class
	$(JAVA) nls_table nls_cp1257.c $(DESTDIR) < nls_cp1257.in

$(DESTDIR)nls_cp1258.c : nls_cp1258.in nls_table.class
	$(JAVA) nls_table nls_cp1258.c $(DESTDIR) < nls_cp1258.in

$(DESTDIR)nls_hp_roman8.c : nls_hp_roman8.in nls_table.class
	$(JAVA) nls_table nls_hp_roman8.c $(DESTDIR) < nls_hp_roman8.in

$(DESTDIR)nls_nextstep.c : nls_nextstep.in nls_table.class
	$(JAVA) nls_table nls_nextstep.c $(DESTDIR) < nls_nextstep.in

nls_table.class : nls_table.java
	$(JAVAC) nls_table.java

# The .in files come from different sources.
# ftp.unicode.org tables, linux kernel, or by running the Java "native2ascii"
# program.

nls_ascii.in : nls_txt_to_in.class
#	$(ICONV) --from-code=ASCII --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_ascii.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/ASCII.TXT > nls_ascii.in

nls_iso8859_1.in : nls_txt_to_in.class
#	$(ICONV) --from-code=ISO-8859-1 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_1.in
#	native2ascii -encoding 8859_1           < nls_table_in | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_1.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-1.TXT > nls_iso8859_1.in

nls_iso8859_2.in : nls_txt_to_in.class
#	$(ICONV) --from-code=ISO-8859-2 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_2.in
#	native2ascii -encoding 8859_2           < nls_table_in | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_2.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-2.TXT > nls_iso8859_2.in

nls_iso8859_3.in : nls_txt_to_in.class
#	native2ascii -encoding 8859_3           < nls_table_in | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_3.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-3.TXT > nls_iso8859_3.in

nls_iso8859_4.in : nls_txt_to_in.class
#	$(ICONV) --from-code=ISO-8859-4 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_4.in
#	native2ascii -encoding 8859_4           < nls_table_in | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_4.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-4.TXT > nls_iso8859_4.in

nls_iso8859_5.in : nls_txt_to_in.class
#	$(ICONV) --from-code=ISO-8859-5 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_5.in
#	native2ascii -encoding 8859_5           < nls_table_in | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_5.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-5.TXT > nls_iso8859_5.in

nls_iso8859_6.in : nls_txt_to_in.class
#	native2ascii -encoding 8859_6           < nls_table_in | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_6.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-6.TXT > nls_iso8859_6.in

nls_iso8859_7.in : nls_txt_to_in.class
#	native2ascii -encoding 8859_7           < nls_table_in | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_7.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-7.TXT > nls_iso8859_7.in

nls_iso8859_8.in : nls_txt_to_in.class
#	native2ascii -encoding 8859_8           < nls_table_in | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_8.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-8.TXT > nls_iso8859_8.in

nls_iso8859_9.in : nls_txt_to_in.class
#	$(ICONV) --from-code=ISO-8859-9 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_9.in
#	native2ascii -encoding 8859_9           < nls_table_in | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_9.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-9.TXT > nls_iso8859_9.in

nls_iso8859_10.in :
	$(ICONV) --from-code=ISO-8859-10 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_10.in

nls_iso8859_13.in :
	$(ICONV) --from-code=ISO-8859-13 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_13.in

nls_iso8859_14.in : nls_txt_to_in.class
#	$(ICONV) --from-code=ISO-8859-14 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_14.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-14.TXT > nls_iso8859_14.in

nls_iso8859_15.in : nls_txt_to_in.class
#	$(ICONV) --from-code=ISO-8859-15 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 | sed -e 's/u00[89][0-9a-f]/ufffd/' > nls_iso8859_15.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/8859-15.TXT > nls_iso8859_15.in

#nls_koi8_r.in :
#	> nls_koi8_r.in

nls_mac_arabic.in :
	native2ascii -encoding MacArabic        < nls_table_in > nls_mac_arabic.in

nls_mac_centraleurope.in :
	native2ascii -encoding MacCentralEurope < nls_table_in > nls_mac_centraleurope.in
#	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-MAC-LATIN2.TXT > nls_mac_centraleurope.in

nls_mac_croatian.in :
	native2ascii -encoding MacCroatian      < nls_table_in | sed -e 's/uf8ff/ufffd/' > nls_mac_croatian.in

nls_mac_cyrillic.in :
	native2ascii -encoding MacCyrillic      < nls_table_in > nls_mac_cyrillic.in
#	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-MAC-CYRILLIC.TXT > nls_mac_cyrillic.in

nls_mac_dingbat.in :
	native2ascii -encoding MacDingbat       < nls_table_in > nls_mac_dingbat.in

nls_mac_greek.in :
	native2ascii -encoding MacGreek         < nls_table_in > nls_mac_greek.in
#	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-MAC-GREEK.TXT > nls_mac_greek.in

nls_mac_hebrew.in :
	native2ascii -encoding MacHebrew        < nls_table_in > nls_mac_hebrew.in

nls_mac_iceland.in :
	native2ascii -encoding MacIceland       < nls_table_in | sed -e 's/uf8ff/ufffd/' > nls_mac_iceland.in
#	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-MAC-ICELAND.TXT > nls_mac_iceland.in

nls_mac_roman.in :
	native2ascii -encoding MacRoman         < nls_table_in | sed -e 's/uf8ff/ufffd/' > nls_mac_roman.in
#	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-MAC-ROMAN.TXT > nls_mac_roman.in

nls_mac_romania.in :
	native2ascii -encoding MacRomania       < nls_table_in | sed -e 's/uf8ff/ufffd/' > nls_mac_romania.in

nls_mac_symbol.in :
	native2ascii -encoding MacSymbol        < nls_table_in > nls_mac_symbol.in

nls_mac_thai.in :
	native2ascii -encoding MacThai          < nls_table_in > nls_mac_thai.in

nls_mac_turkish.in :
	native2ascii -encoding MacTurkish       < nls_table_in | sed -e 's/uf8ff/ufffd/' > nls_mac_turkish.in
#	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-MAC-TURKISH.TXT > nls_mac_turkish.in

nls_mac_ukraine.in :
	native2ascii -encoding MacUkraine       < nls_table_in > nls_mac_ukraine.in

nls_cp437_ms.in : nls_txt_to_in.class
#	native2ascii -encoding Cp437            < nls_table_in > nls_cp437_ms.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP437.TXT > nls_cp437_ms.in

nls_cp437_ibm.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/IBM-PC-CP437.TXT > nls_cp437_ibm.in

nls_cp737.in : nls_txt_to_in.class
#	native2ascii -encoding Cp737            < nls_table_in > nls_cp737.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP737.TXT > nls_cp737.in

nls_cp775.in : nls_txt_to_in.class
#	native2ascii -encoding Cp775            < nls_table_in > nls_cp775.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP775.TXT > nls_cp775.in

nls_cp850.in : nls_txt_to_in.class
#	native2ascii -encoding Cp850            < nls_table_in > nls_cp850.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP850.TXT > nls_cp850.in

nls_cp852_ms.in : nls_txt_to_in.class
#	native2ascii -encoding Cp852            < nls_table_in > nls_cp852_ms.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP852.TXT > nls_cp852_ms.in

nls_cp852_ibm.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/IBM-PC-CP852.TXT > nls_cp852_ibm.in

nls_cp855.in : nls_txt_to_in.class
#	native2ascii -encoding Cp855            < nls_table_in > nls_cp855.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP855.TXT > nls_cp855.in

nls_cp857.in : nls_txt_to_in.class
#	native2ascii -encoding Cp857            < nls_table_in > nls_cp857.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP857.TXT > nls_cp857.in

nls_cp860_ms.in : nls_txt_to_in.class
#	native2ascii -encoding Cp860            < nls_table_in > nls_cp860_ms.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP860.TXT > nls_cp860_ms.in

nls_cp860_ibm.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/IBM-PC-CP860.TXT > nls_cp860_ibm.in

nls_cp861_ms.in : nls_txt_to_in.class
#	native2ascii -encoding Cp861            < nls_table_in > nls_cp861_ms.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP861.TXT > nls_cp861_ms.in

nls_cp861_ibm.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/IBM-PC-CP861.TXT > nls_cp861_ibm.in

nls_cp862_ms.in : nls_txt_to_in.class
#	native2ascii -encoding Cp862            < nls_table_in > nls_cp862_ms.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP862.TXT > nls_cp862_ms.in

nls_cp862_ibm.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/IBM-PC-CP862.TXT > nls_cp862_ibm.in

nls_cp863_ms.in : nls_txt_to_in.class
#	native2ascii -encoding Cp863            < nls_table_in > nls_cp863_ms.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP863.TXT > nls_cp863_ms.in

nls_cp863_ibm.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/IBM-PC-CP863.TXT > nls_cp863_ibm.in

nls_cp864_ms.in : nls_txt_to_in.class
#	native2ascii -encoding Cp864            < nls_table_in > nls_cp864_ms.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP864.TXT > nls_cp864_ms.in

nls_cp864_ibm.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/IBM-PC-CP864.TXT > nls_cp864_ibm.in

nls_cp865_ms.in : nls_txt_to_in.class
#	native2ascii -encoding Cp865            < nls_table_in > nls_cp865_ms.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP865.TXT > nls_cp865_ms.in

nls_cp865_ibm.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/IBM-PC-CP865.TXT > nls_cp865_ibm.in

nls_cp866.in : nls_txt_to_in.class
#	native2ascii -encoding Cp866            < nls_table_in > nls_cp866.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP866.TXT > nls_cp866.in

nls_cp869_ms.in : nls_txt_to_in.class
#	native2ascii -encoding Cp869            < nls_table_in > nls_cp869_ms.in
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP869.TXT > nls_cp869_ms.in

nls_cp869_ibm.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/IBM-PC-CP869.TXT > nls_cp869_ibm.in

nls_cp874_ms.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-PC-CP874.TXT > nls_cp874_ms.in
#	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-WIN-CP874.TXT > nls_cp874_ms.in

nls_cp874_ibm.in : nls_txt_to_in.class
#	native2ascii -encoding Cp874            < nls_table_in > nls_cp874_ibm.in # old, lacks Euro sign
	$(JAVA) nls_txt_to_in < ftp.unicode.org/IBM-PC-CP874.TXT > nls_cp874_ibm.in

nls_cp1250.in : nls_txt_to_in.class
#	native2ascii -encoding Cp1250           < nls_table_in > nls_cp1250.in # old, lacks Euro sign
#	$(ICONV) --from-code=CP1250 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_cp1250.in # old, lacks Euro sign
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-WIN-CP1250.TXT > nls_cp1250.in

nls_cp1251.in : nls_txt_to_in.class
#	native2ascii -encoding Cp1251           < nls_table_in > nls_cp1251.in # old, lacks Euro sign
#	$(ICONV) --from-code=CP1251 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_cp1251.in # old, lacks Euro sign
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-WIN-CP1251.TXT > nls_cp1251.in

nls_cp1252.in : nls_txt_to_in.class
#	native2ascii -encoding Cp1252           < nls_table_in > nls_cp1252.in # old, lacks Euro sign
#	$(ICONV) --from-code=CP1252 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_cp1252.in # old, lacks Euro sign
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-WIN-CP1252.TXT > nls_cp1252.in

nls_cp1253.in : nls_txt_to_in.class
#	native2ascii -encoding Cp1253           < nls_table_in > nls_cp1253.in # old, lacks Euro sign
#	$(ICONV) --from-code=CP1253 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_cp1253.in # old, lacks Euro sign
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-WIN-CP1253.TXT > nls_cp1253.in

nls_cp1254.in : nls_txt_to_in.class
#	native2ascii -encoding Cp1254           < nls_table_in > nls_cp1254.in # old, lacks Euro sign
#	$(ICONV) --from-code=CP1254 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_cp1254.in # old, lacks Euro sign
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-WIN-CP1254.TXT > nls_cp1254.in

nls_cp1255.in : nls_txt_to_in.class
#	native2ascii -encoding Cp1255           < nls_table_in > nls_cp1255.in # old, lacks Euro sign and other things
#	$(ICONV) --from-code=CP1255 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_cp1255.in # old, lacks Euro sign
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-WIN-CP1255.TXT > nls_cp1255.in

nls_cp1256.in : nls_txt_to_in.class
#	native2ascii -encoding Cp1256           < nls_table_in > nls_cp1256.in # old, lacks Euro sign and other things
#	$(ICONV) --from-code=CP1256 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_cp1256.in # old, lacks Euro sign
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-WIN-CP1256.TXT > nls_cp1256.in

nls_cp1257.in : nls_txt_to_in.class
#	native2ascii -encoding Cp1257           < nls_table_in > nls_cp1257.in # old, lacks Euro sign
#	$(ICONV) --from-code=CP1257 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_cp1257.in # old, lacks Euro sign
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-WIN-CP1257.TXT > nls_cp1257.in

nls_cp1258.in : nls_txt_to_in.class
#	native2ascii -encoding Cp1258           < nls_table_in > nls_cp1258.in # old, lacks Euro sign
#	$(ICONV) --from-code=CP1258 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_cp1258.in # old, lacks Euro sign
	$(JAVA) nls_txt_to_in < ftp.unicode.org/MS-WIN-CP1258.TXT > nls_cp1258.in

nls_hp_roman8.in : nls_txt_to_in.class
	$(ICONV) --from-code=HP-ROMAN8 --to-code=UTF-8 < nls_table_in | native2ascii -encoding UTF8 > nls_hp_roman8.in

nls_nextstep.in : nls_txt_to_in.class
	$(JAVA) nls_txt_to_in < ftp.unicode.org/NEXTSTEP.TXT > nls_nextstep.in

nls_txt_to_in.class : nls_txt_to_in.java
	$(JAVAC) nls_txt_to_in.java


clean : force
	rm -f *.class *.c

force :

