# Liste aller Pseudofunktionen
# Bruno Haible 30.4.1995

# Der Macro PSEUDOFUN deklariert eine Pseudofunktion.
# PSEUDOFUN(fun)
# > fun: C-Funktion

# Expander für die Deklaration der Tabelle:
  #define PSEUDOFUN_A(fun)  Pseudofun pseudo_##fun;

# Expander für die Initialisierung der Tabelle:
  #define PSEUDOFUN_B(fun)  (Pseudofun)(&fun),

# Welcher Expander benutzt wird, muss vom Hauptfile aus eingestellt werden.

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

#ifdef HANDLES
PSEUDOFUN(rd_by_handle) PSEUDOFUN(rd_by_array_handle) PSEUDOFUN(wr_by_handle) PSEUDOFUN(wr_by_array_handle) PSEUDOFUN(rd_ch_handle) PSEUDOFUN(rd_ch_array_handle) PSEUDOFUN(wr_ch_handle_x) PSEUDOFUN(wr_ch_array_handle_x) PSEUDOFUN(wr_ss_handle_x)
#endif
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
PSEUDOFUN(rd_ch_ch_file) PSEUDOFUN(rd_ch_array_ch_file) PSEUDOFUN(wr_ch_ch_file) PSEUDOFUN(wr_ch_array_ch_file) PSEUDOFUN(wr_ss_ch_file)
PSEUDOFUN(rd_by_iau_file) PSEUDOFUN(wr_by_iau_file)
PSEUDOFUN(rd_by_ias_file) PSEUDOFUN(wr_by_ias_file)
PSEUDOFUN(rd_by_ibu_file) PSEUDOFUN(wr_by_ibu_file)
PSEUDOFUN(rd_by_ibs_file) PSEUDOFUN(wr_by_ibs_file)
PSEUDOFUN(rd_by_icu_file) PSEUDOFUN(wr_by_icu_file)
PSEUDOFUN(rd_by_ics_file) PSEUDOFUN(wr_by_ics_file)
PSEUDOFUN(read_byte_array_iau8_file) PSEUDOFUN(write_byte_array_iau8_file)
#ifdef PRINTER
PSEUDOFUN(wr_ch_printer)
#endif
#ifdef PIPES
PSEUDOFUN(rd_ch_pipe_in) PSEUDOFUN(rd_ch_array_pipe_in)
PSEUDOFUN(wr_ch_pipe_out) PSEUDOFUN(wr_ch_array_pipe_out) PSEUDOFUN(wr_ss_pipe_out)
#endif
#ifdef X11SOCKETS
PSEUDOFUN(rd_ch_x11socket) PSEUDOFUN(rd_ch_array_x11socket) PSEUDOFUN(wr_ch_x11socket) PSEUDOFUN(wr_ch_array_x11socket) PSEUDOFUN(wr_ss_x11socket) PSEUDOFUN(rd_by_x11socket) PSEUDOFUN(rd_by_array_x11socket) PSEUDOFUN(wr_by_x11socket) PSEUDOFUN(wr_by_array_x11socket)
#endif
#ifdef SOCKET_STREAMS
PSEUDOFUN(rd_ch_socket) PSEUDOFUN(rd_ch_array_socket) PSEUDOFUN(wr_ch_socket) PSEUDOFUN(wr_ch_array_socket) PSEUDOFUN(wr_ss_socket) PSEUDOFUN(rd_by_socket) PSEUDOFUN(rd_by_array_socket) PSEUDOFUN(wr_by_socket) PSEUDOFUN(wr_by_array_socket)
#endif

