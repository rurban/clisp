# Handling of signal SIGWINCH.

# ------------------------------ Specification ---------------------------------

#if defined(HAVE_SIGNALS) && defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
# Install a handler for SIGWINCH.
  local void install_sigwinch_handler (void);
#endif

#ifdef HAVE_SIGNALS
# Get the window width and adapt SYS::*PRIN-LINELENGTH* to it.
  local void update_linelength (void);
#endif

# Block signal SIGWINCH during GC.
# gc_signalblock_on(); ... gc_signalblock_off();

# ------------------------------ Implementation --------------------------------

#ifdef HAVE_SIGNALS

# Passt den Wert von SYS::*PRIN-LINELENGTH* an die aktuelle Breite des
# Terminal-Fensters an.
# update_linelength();
  local void update_linelength (void);
  local void update_linelength()
    { # SYS::*PRIN-LINELENGTH* := Breite des Terminal-Fensters - 1
      #if !defined(NEXTAPP)
      # [vgl. 'term.c' in 'calc' von Hans-J. Böhm, Vernon Lee, Alan J. Demers]
      if (isatty(stdout_handle)) # Standard-Output ein Terminal?
        { /* var int lines = 0; */
          var int columns = 0;
          #ifdef TIOCGWINSZ
          # Probiere erst ioctl:
          { var struct winsize stdout_window_size;
            if (!( ioctl(stdout_handle,TIOCGWINSZ,&stdout_window_size) <0))
              { /* lines = stdout_window_size.ws_row; */
                columns = stdout_window_size.ws_col;
          }   }
          # Das kann - entgegen der Dokumentation - scheitern!
          if (/* (lines > 0) && */ (columns > 0)) goto OK;
          #endif
          #if !(defined(NO_TERMCAP_NCURSES) || defined(WATCOM))
          # Nun probieren wir's über termcap:
          { var const char* term_name = getenv("TERM");
            if (term_name==NULL) { term_name = "unknown"; }
           {var char termcap_entry_buf[10000];
            if ( tgetent(&!termcap_entry_buf,term_name) ==1)
              { /* lines = tgetnum("li"); if (lines<0) { lines = 0; } */
                columns = tgetnum("co"); if (columns<0) { columns = 0; }
              }
          }}
          #endif
          # Hoffentlich enthält columns jetzt einen vernünftigen Wert.
          if (/* (lines > 0) && */ (columns > 0)) goto OK;
          if (FALSE)
            { OK:
              # Wert von SYS::*PRIN-LINELENGTH* verändern:
              Symbol_value(S(prin_linelength)) = fixnum(columns-1);
            }
        }
      #else # defined(NEXTAPP)
      if (nxterminal_line_length > 0)
        # Wert von SYS::*PRIN-LINELENGTH* verändern:
        { Symbol_value(S(prin_linelength)) = fixnum(nxterminal_line_length-1); }
      #endif
    }

#endif

#if defined(HAVE_SIGNALS) && defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)

# Signal-Handler für Signal SIGWINCH:
  local void sigwinch_handler (int sig);
  local void sigwinch_handler(sig)
    var int sig; # sig = SIGWINCH
    { inc_break_sem_5();
      signal_acknowledge(SIGWINCH,&sigwinch_handler);
      update_linelength();
      dec_break_sem_5();
    }

#define install_sigwinch_handler()  \
  SIGNAL(SIGWINCH,&sigwinch_handler);

#endif

#if defined(HAVE_SIGNALS) && defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
  # Signal SIGWINCH blockieren, denn eine Veränderung des Wertes von
  # SYS::*PRIN-LINELENGTH* können wir während der GC nicht brauchen.
  # Dann Signal SIGWINCH wieder freigeben.
  #define gc_signalblock_on()  signalblock_on(SIGWINCH)
  #define gc_signalblock_off()  signalblock_off(SIGWINCH)
#else
  #define gc_signalblock_on()
  #define gc_signalblock_off()
#endif
