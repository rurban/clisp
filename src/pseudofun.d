# Liste aller Pseudofunktionen
# Bruno Haible 14.2.1999

# Der Macro PSEUDOFUN deklariert eine Pseudofunktion.
# PSEUDOFUN(fun)
# > fun: C-Funktion

# Expander für die Deklaration der Tabelle:
  #define PSEUDOFUN_A(fun)  Pseudofun pseudo_##fun;

# Expander für die Deklaration der Tabellenelemente:
  #define PSEUDOFUN_B(fun)

# Expander für die Initialisierung der Tabelle:
  #define PSEUDOFUN_C(fun)  (Pseudofun)(&fun),

# Der Macro XPSEUDO deklariert eine Pseudofunktion, die nicht in stream.d
# definiert ist.
# XPSEUDO(rettype,name,arglist)
# > name: C-Funktion oder C-Variable

# Expander für die Deklaration der Tabelle:
  #define XPSEUDO_A(rettype,name,arglist)  Pseudofun pseudo_##name;

# Expander für die Deklaration der Tabellenelemente:
  #define XPSEUDO_B(rettype,name,arglist)  extern rettype name arglist;

# Expander für die Initialisierung der Tabelle:
  #define XPSEUDO_C(rettype,name,arglist)  (Pseudofun)(&name),

# Welche Expander benutzt werden, muss vom Hauptfile aus eingestellt werden.

PSEUDOFUN(rd_by_error) PSEUDOFUN(rd_by_array_error) PSEUDOFUN(rd_by_array_dummy)
PSEUDOFUN(wr_by_error) PSEUDOFUN(wr_by_array_error) PSEUDOFUN(wr_by_array_dummy)
PSEUDOFUN(rd_ch_error) PSEUDOFUN(pk_ch_dummy) PSEUDOFUN(rd_ch_array_error) PSEUDOFUN(rd_ch_array_dummy)
PSEUDOFUN(wr_ch_error) PSEUDOFUN(wr_ch_array_error) PSEUDOFUN(wr_ch_array_dummy)
PSEUDOFUN(wr_ss_dummy) PSEUDOFUN(wr_ss_dummy_nogc)

PSEUDOFUN(rd_by_synonym) PSEUDOFUN(rd_by_array_synonym) PSEUDOFUN(wr_by_synonym) PSEUDOFUN(wr_by_array_synonym) PSEUDOFUN(rd_ch_synonym) PSEUDOFUN(pk_ch_synonym) PSEUDOFUN(rd_ch_array_synonym) PSEUDOFUN(wr_ch_synonym) PSEUDOFUN(wr_ch_array_synonym) PSEUDOFUN(wr_ss_synonym)
PSEUDOFUN(wr_by_broad) PSEUDOFUN(wr_by_array_broad0) PSEUDOFUN(wr_by_array_broad1) PSEUDOFUN(wr_ch_broad) PSEUDOFUN(wr_ch_array_broad0) PSEUDOFUN(wr_ch_array_broad1) PSEUDOFUN(wr_ss_broad)
PSEUDOFUN(rd_by_concat) PSEUDOFUN(rd_by_array_concat) PSEUDOFUN(rd_ch_concat) PSEUDOFUN(pk_ch_concat) PSEUDOFUN(rd_ch_array_concat)
PSEUDOFUN(wr_by_twoway) PSEUDOFUN(wr_by_array_twoway) PSEUDOFUN(wr_ch_twoway) PSEUDOFUN(wr_ch_array_twoway) PSEUDOFUN(wr_ss_twoway)
PSEUDOFUN(rd_by_twoway) PSEUDOFUN(rd_by_array_twoway) PSEUDOFUN(rd_ch_twoway) PSEUDOFUN(pk_ch_twoway) PSEUDOFUN(rd_ch_array_twoway)
PSEUDOFUN(rd_by_echo) PSEUDOFUN(rd_ch_echo)
PSEUDOFUN(rd_ch_str_in) PSEUDOFUN(rd_ch_array_str_in)
PSEUDOFUN(wr_ch_str_out) PSEUDOFUN(wr_ss_str_out)
PSEUDOFUN(wr_ch_str_push)
PSEUDOFUN(wr_ch_pphelp) PSEUDOFUN(wr_ss_pphelp)
PSEUDOFUN(rd_ch_buff_in)
PSEUDOFUN(wr_ch_buff_out)
#ifdef GENERIC_STREAMS
PSEUDOFUN(rd_ch_generic) PSEUDOFUN(pk_ch_generic) PSEUDOFUN(wr_ch_generic) PSEUDOFUN(wr_ss_generic) PSEUDOFUN(rd_by_generic) PSEUDOFUN(wr_by_generic)
#endif

PSEUDOFUN(rd_by_iau_unbuffered) PSEUDOFUN(rd_by_ias_unbuffered) PSEUDOFUN(rd_by_iau8_unbuffered) PSEUDOFUN(rd_by_array_iau8_unbuffered)
PSEUDOFUN(wr_by_iau_unbuffered) PSEUDOFUN(wr_by_ias_unbuffered) PSEUDOFUN(wr_by_iau8_unbuffered) PSEUDOFUN(wr_by_array_iau8_unbuffered)
PSEUDOFUN(rd_ch_unbuffered) PSEUDOFUN(rd_ch_array_unbuffered)
PSEUDOFUN(wr_ch_unbuffered_unix) PSEUDOFUN(wr_ch_array_unbuffered_unix) PSEUDOFUN(wr_ss_unbuffered_unix)
PSEUDOFUN(wr_ch_unbuffered_mac) PSEUDOFUN(wr_ch_array_unbuffered_mac) PSEUDOFUN(wr_ss_unbuffered_mac)
PSEUDOFUN(wr_ch_unbuffered_dos) PSEUDOFUN(wr_ch_array_unbuffered_dos) PSEUDOFUN(wr_ss_unbuffered_dos)
PSEUDOFUN(rd_ch_buffered) PSEUDOFUN(rd_ch_array_buffered)
PSEUDOFUN(wr_ch_buffered_unix) PSEUDOFUN(wr_ch_array_buffered_unix) PSEUDOFUN(wr_ss_buffered_unix)
PSEUDOFUN(wr_ch_buffered_mac) PSEUDOFUN(wr_ch_array_buffered_mac) PSEUDOFUN(wr_ss_buffered_mac)
PSEUDOFUN(wr_ch_buffered_dos) PSEUDOFUN(wr_ch_array_buffered_dos) PSEUDOFUN(wr_ss_buffered_dos)
PSEUDOFUN(rd_by_iau_buffered) PSEUDOFUN(rd_by_ias_buffered) PSEUDOFUN(rd_by_ibu_buffered) PSEUDOFUN(rd_by_ibs_buffered) PSEUDOFUN(rd_by_icu_buffered) PSEUDOFUN(rd_by_ics_buffered) PSEUDOFUN(rd_by_iau8_buffered)
PSEUDOFUN(rd_by_array_iau8_buffered)
PSEUDOFUN(wr_by_iau_buffered) PSEUDOFUN(wr_by_ias_buffered) PSEUDOFUN(wr_by_ibu_buffered) PSEUDOFUN(wr_by_ibs_buffered) PSEUDOFUN(wr_by_icu_buffered) PSEUDOFUN(wr_by_ics_buffered) PSEUDOFUN(wr_by_iau8_buffered)
PSEUDOFUN(wr_by_array_iau8_buffered)
#if defined(KEYBOARD) || defined(MAYBE_NEXTAPP)
PSEUDOFUN(rd_ch_keyboard)
#endif
#if defined(MAYBE_NEXTAPP)
PSEUDOFUN(wr_ch_terminal) PSEUDOFUN(rd_ch_terminal)
#endif
#if defined(UNIX) || defined(MSDOS) || defined(AMIGAOS) || defined(RISCOS)
PSEUDOFUN(wr_ch_terminal1) PSEUDOFUN(rd_ch_terminal1) PSEUDOFUN(wr_ss_terminal1)
#ifdef MSDOS
PSEUDOFUN(wr_ch_terminal2) PSEUDOFUN(rd_ch_terminal2) PSEUDOFUN(wr_ss_terminal2)
#endif
#if defined(GNU_READLINE) || defined(MAYBE_NEXTAPP)
PSEUDOFUN(wr_ch_terminal3) PSEUDOFUN(rd_ch_terminal3) PSEUDOFUN(wr_ss_terminal3)
#endif
#endif
#ifdef SCREEN
PSEUDOFUN(wr_ch_window)
#endif
#ifdef PRINTER
PSEUDOFUN(wr_ch_printer)
#endif

# External definitions from ENCODING.D:
#ifdef UNICODE
XPSEUDO(uintL, uni16_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDO(void, uni16be_mbstowcs, (object encoding, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDO(void, uni16le_mbstowcs, (object encoding, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDO(uintL, uni16_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDO(void, uni16be_wcstombs, (object encoding, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDO(void, uni16le_wcstombs, (object encoding, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDO(uintL, utf8_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDO(void, utf8_mbstowcs, (object encoding, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDO(uintL, utf8_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDO(void, utf8_wcstombs, (object encoding, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDO(uintL, java_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDO(void, java_mbstowcs, (object encoding, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDO(uintL, java_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDO(void, java_wcstombs, (object encoding, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDO(uintL, nls_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDO(void, nls_mbstowcs, (object encoding, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDO(void, nls_asciiext_mbstowcs, (object encoding, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDO(uintL, nls_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDO(void, nls_wcstombs, (object encoding, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDO(void, nls_asciiext_wcstombs, (object encoding, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDO(struct nls_table, nls_ascii_table,)
XPSEUDO(struct nls_table, nls_iso8859_1_table,)
XPSEUDO(struct nls_table, nls_iso8859_2_table,)
XPSEUDO(struct nls_table, nls_iso8859_3_table,)
XPSEUDO(struct nls_table, nls_iso8859_4_table,)
XPSEUDO(struct nls_table, nls_iso8859_5_table,)
XPSEUDO(struct nls_table, nls_iso8859_6_table,)
XPSEUDO(struct nls_table, nls_iso8859_7_table,)
XPSEUDO(struct nls_table, nls_iso8859_8_table,)
XPSEUDO(struct nls_table, nls_iso8859_9_table,)
XPSEUDO(struct nls_table, nls_iso8859_14_table,)
XPSEUDO(struct nls_table, nls_iso8859_15_table,)
XPSEUDO(struct nls_table, nls_koi8_r_table,)
XPSEUDO(struct nls_table, nls_mac_arabic_table,)
XPSEUDO(struct nls_table, nls_mac_centraleurope_table,)
XPSEUDO(struct nls_table, nls_mac_croatian_table,)
XPSEUDO(struct nls_table, nls_mac_cyrillic_table,)
XPSEUDO(struct nls_table, nls_mac_dingbat_table,)
XPSEUDO(struct nls_table, nls_mac_greek_table,)
XPSEUDO(struct nls_table, nls_mac_hebrew_table,)
XPSEUDO(struct nls_table, nls_mac_iceland_table,)
XPSEUDO(struct nls_table, nls_mac_roman_table,)
XPSEUDO(struct nls_table, nls_mac_romania_table,)
XPSEUDO(struct nls_table, nls_mac_symbol_table,)
XPSEUDO(struct nls_table, nls_mac_thai_table,)
XPSEUDO(struct nls_table, nls_mac_turkish_table,)
XPSEUDO(struct nls_table, nls_mac_ukraine_table,)
XPSEUDO(struct nls_table, nls_cp437_ms_table,)
XPSEUDO(struct nls_table, nls_cp437_ibm_table,)
XPSEUDO(struct nls_table, nls_cp737_table,)
XPSEUDO(struct nls_table, nls_cp775_table,)
XPSEUDO(struct nls_table, nls_cp850_table,)
XPSEUDO(struct nls_table, nls_cp852_ms_table,)
XPSEUDO(struct nls_table, nls_cp852_ibm_table,)
XPSEUDO(struct nls_table, nls_cp855_table,)
XPSEUDO(struct nls_table, nls_cp857_table,)
XPSEUDO(struct nls_table, nls_cp860_ms_table,)
XPSEUDO(struct nls_table, nls_cp860_ibm_table,)
XPSEUDO(struct nls_table, nls_cp861_ms_table,)
XPSEUDO(struct nls_table, nls_cp861_ibm_table,)
XPSEUDO(struct nls_table, nls_cp862_ms_table,)
XPSEUDO(struct nls_table, nls_cp862_ibm_table,)
XPSEUDO(struct nls_table, nls_cp863_ms_table,)
XPSEUDO(struct nls_table, nls_cp863_ibm_table,)
XPSEUDO(struct nls_table, nls_cp864_ms_table,)
XPSEUDO(struct nls_table, nls_cp864_ibm_table,)
XPSEUDO(struct nls_table, nls_cp865_ms_table,)
XPSEUDO(struct nls_table, nls_cp865_ibm_table,)
XPSEUDO(struct nls_table, nls_cp866_table,)
XPSEUDO(struct nls_table, nls_cp869_ms_table,)
XPSEUDO(struct nls_table, nls_cp869_ibm_table,)
XPSEUDO(struct nls_table, nls_cp874_ms_table,)
XPSEUDO(struct nls_table, nls_cp874_ibm_table,)
XPSEUDO(struct nls_table, nls_cp1250_table,)
XPSEUDO(struct nls_table, nls_cp1251_table,)
XPSEUDO(struct nls_table, nls_cp1252_table,)
XPSEUDO(struct nls_table, nls_cp1253_table,)
XPSEUDO(struct nls_table, nls_cp1254_table,)
XPSEUDO(struct nls_table, nls_cp1255_table,)
XPSEUDO(struct nls_table, nls_cp1256_table,)
XPSEUDO(struct nls_table, nls_cp1257_table,)
XPSEUDO(struct nls_table, nls_cp1258_table,)
XPSEUDO(struct nls_table, nls_nextstep_table,)
#ifdef HAVE_ICONV
XPSEUDO(uintL, iconv_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDO(void, iconv_mbstowcs, (object encoding, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDO(uintL, iconv_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDO(void, iconv_wcstombs, (object encoding, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
#endif
#endif

