# Handling of signals SIGINT and SIGALRM.

# ------------------------------ Specification ---------------------------------

#ifdef PENDING_INTERRUPTS
# Flag telling whether a Ctrl-C has been seen and is waiting to be handled.
  extern uintB interrupt_pending;
#endif

#ifdef HAVE_SIGNALS
# Installs the Ctrl-C handler.
  local void install_sigint_handler (void);
#endif
#ifdef WIN32_NATIVE
# Installs the Ctrl-C handler.
  extern void install_sigint_handler (void);
#endif

# ------------------------------ Implementation --------------------------------

#ifdef PENDING_INTERRUPTS
  # Flag, ob eine Unterbrechung anliegt.
  global uintB interrupt_pending = FALSE;
#endif

#ifdef HAVE_SIGNALS

# Eine Tastatur-Unterbrechung (Signal SIGINT, erzeugt durch Ctrl-C)
# wird eine Sekunde lang aufgehoben. In dieser Zeit kann sie mittels
# 'interruptp' auf fortsetzbare Art behandelt werden. Nach Ablauf dieser
# Zeit wird das Programm nichtfortsetzbar unterbrochen.
# Signal-Handler für Signal SIGINT:
  local void interrupt_handler (int sig);
  local void interrupt_handler(sig)
    var int sig; # sig = SIGINT
    { inc_break_sem_5();
      signal_acknowledge(SIGINT,&interrupt_handler);
  #ifdef PENDING_INTERRUPTS
      if (!interrupt_pending) # Liegt schon ein Interrupt an -> nichts zu tun
        { interrupt_pending = TRUE; # Flag für 'interruptp' setzen
          #ifdef HAVE_UALARM
          # eine halbe Sekunde warten, dann jede 1/20 sec probieren
          ualarm(ticks_per_second/2,ticks_per_second/20);
          #else
          alarm(1); # eine Sekunde warten, weiter geht's dann bei alarm_handler
          #endif
        }
      dec_break_sem_5();
    }
  local void alarm_handler (int sig);
  local void alarm_handler(sig)
    var int sig; # sig = SIGALRM
    { # Die Zeit ist nun abgelaufen.
      inc_break_sem_5();
      #ifdef EMUNIX # Verhindere Programm-Beendigung durch SIGALRM
      #ifndef HAVE_UALARM
      alarm(0); # SIGALRM-Timer abbrechen
      #endif
      #endif
      signal_acknowledge(SIGALRM,&alarm_handler);
  #endif # PENDING_INTERRUPTS (!)
      dec_break_sem_5();
    #ifndef NO_ASYNC_INTERRUPTS
      # Warten, bis Unterbrechung erlaubt:
      if (!break_sems_cleared())
    #endif
        {
          #ifndef WATCOM
          #ifndef HAVE_UALARM
          alarm(1); # Probieren wir's in einer Sekunde nochmal
          #endif
          #endif
          return; # Nach kurzer Zeit wird wieder ein SIGALRM ausgelöst.
        }
    #ifndef NO_ASYNC_INTERRUPTS
      # Wir springen jetzt aus dem signal-Handler heraus, weder mit 'return'
      # noch mit 'longjmp'.
      #
      # Hans-J. Boehm <boehm@parc.xerox.com> weist darauf hin, dass dies
      # Probleme bringen kann, wenn das Signal ein laufendes malloc() oder
      # free() unterbrochen hat und die malloc()-Library nicht reentrant ist.
      # Abhilfe: statt malloc() stets xmalloc() verwenden, das eine Break-
      # Semaphore setzt? Aber was ist mit malloc()-Aufrufen, die von Routinen
      # wie opendir(), getpwnam(), tgetent(), ... abgesetzt werden? Soll man
      # malloc() selber definieren und darauf hoffen, dass es von allen Library-
      # funktionen aufgerufen wird (statisch gelinkt oder per DLL)??
      #
      #ifdef RISCOS
      prepare_signal_handler_exit(sig);
      #endif
      #if (defined(USE_SIGACTION) ? defined(SIGACTION_NEED_UNBLOCK) : defined(SIGNAL_NEED_UNBLOCK)) || (defined(GNU_READLINE) && (defined(SIGNALBLOCK_BSD) || defined(SIGNALBLOCK_POSIX)))
      # Falls entweder [SIGNAL_NEED_UNBLOCK] mit signal() installierte Handler
      # sowieso mit blockiertem Signal aufgerufen werden - das sind üblicherweise
      # BSD-Systeme -, oder falls andere unsichere Komponenten [GNU_READLINE]
      # per sigaction() o.ä. das Blockieren des Signals beim Aufruf veranlassen
      # können, müssen wir das gerade blockierte Signal entblockieren:
        #if defined(SIGNALBLOCK_POSIX)
          { var sigset_t sigblock_mask;
            sigemptyset(&sigblock_mask); sigaddset(&sigblock_mask,sig);
            sigprocmask(SIG_UNBLOCK,&sigblock_mask,NULL);
          }
        #elif defined(SIGNALBLOCK_BSD)
          sigsetmask(sigblock(0) & ~sigmask(sig));
        #endif
      #endif
      #ifdef HAVE_SAVED_STACK
      # STACK auf einen sinnvollen Wert setzen:
      if (!(saved_STACK==NULL)) { setSTACK(STACK = saved_STACK); }
      #endif
      # Über 'fehler' in eine Break-Schleife springen:
      fehler(serious_condition,
             GETTEXT("Ctrl-C: User break")
            );
    #endif
    }

  #ifdef PENDING_INTERRUPTS
    #define install_sigint_handler()  \
      SIGNAL(SIGINT,&interrupt_handler); \
      SIGNAL(SIGALRM,&alarm_handler);
  #else
    #define install_sigint_handler()  \
      SIGNAL(SIGINT,&interrupt_handler);
  #endif

#endif

#ifdef WIN32_NATIVE

  # This is the Ctrl-C handler. It is executed in the main thread and must
  # not return!
  global void interrupt_handler (void);
  global void interrupt_handler()
    { # asciz_out("Entering interrupt handler.\n");
      #ifdef HAVE_SAVED_STACK
      # STACK auf einen sinnvollen Wert setzen:
      if (!(saved_STACK==NULL)) { setSTACK(STACK = saved_STACK); }
      #endif
      # Über 'fehler' in eine Break-Schleife springen:
      fehler(serious_condition,
             GETTEXT("Ctrl-C: User break")
            );
    }

  # install_sigint_handler(); is defined in win32aux.d.

#endif
