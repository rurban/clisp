# Zeitmessungsfunktionen für CLISP
# Bruno Haible 1990-1999

#include "lispbibl.c"
#include "arilev0.c"  # für high16, low16 in %%TIME,
                      # für divu in GET-UNIVERSAL-TIME,
                      # für mulu32 in GET-INTERNAL-RUN-TIME, GET-INTERNAL-REAL-TIME

# -----------------------------------------------------------------------------
#                          Zeitmessung

# Variablen für Zeitmessung:
#ifdef TIME_AMIGAOS
  # (Grundeinheit ist 1/50 sec, ein 32-Bit-Zähler reicht also
  # für 994d 4h 55m 45.92s, und keine LISP-Session dauert 2.7 Jahre.)
#endif
#ifdef TIME_MSDOS
  # (Grundeinheit ist 1/100 sec, ein 32-Bit-Zähler reicht also
  # für 497d 2h 27m 52.96s, und keine LISP-Session dauert 1.3 Jahre.)
#endif
#if defined(TIME_UNIX_TIMES) || defined(TIME_RISCOS)
  # (Grundeinheit ist etwa 1/60 sec oder 1/100 sec, ein 32-Bit-Zähler reicht
  # also eine ganze Weile.)
#endif
#ifdef TIME_UNIX
  # Grundeinheit ist 1 µsec.
  # (Egal, ob der Systemtakt nun - abhängig vom lokalen Stromnetz - 60 Hz
  # oder 50 Hz beträgt oder eine genauere Uhr eingebaut ist.)
#endif
#ifdef TIME_WIN32
  # Grundeinheit ist 0.1 µsec.
#endif
  # Zeit, die abläuft:
    local internal_time realstart_time;  # Real-Time beim LISP-Start
#ifndef HAVE_RUN_TIME
  # Zeit, die das LISP insgesamt verbraucht:
    local uintL run_time = 0;       # Runtime bisher insgesamt
    local uintL runstop_time;       # bei laufender Run-Time-Stoppuhr:
                                    # Zeitpunkt des letzten Run/Stop-Wechsels
    local boolean run_flag = FALSE; # /= 0 wenn die Run-Time-Stoppuhr läuft
#endif

#ifdef TIME_RELATIVE

# UP: greift die aktuelle Zeit ab
# get_time()
 #ifdef TIME_AMIGAOS
# < uintL ergebnis : aktueller Stand des 50Hz-Zählers
  global uintL get_time(void);
  global uintL get_time()
    { var struct DateStamp datestamp;
      begin_system_call();
      DateStamp(&datestamp); # aktuelle Uhrzeit holen
      end_system_call();
      # und in Ticks ab 1.1.1978 00:00:00 umrechnen:
      return ((uintL)(datestamp.ds_Days)*24*60 + (uintL)(datestamp.ds_Minute))
             *60*ticks_per_second + (uintL)(datestamp.ds_Tick);
    }
 #endif
 #ifdef TIME_MSDOS
# < uintL ergebnis : aktueller Stand des 100Hz-Zählers
  global uintL get_time(void);
  #if defined(DJUNIX) && 0 # Vorsicht: das geht eine Stunde nach!!
    global uintL get_time()
      { var struct timeval real_time;
        begin_system_call();
        gettimeofday(&real_time,NULL);
        end_system_call();
        return (uintL)(real_time.tv_sec) * 100
               + (uintL)((uintW)((uintL)(real_time.tv_usec) / 16) / 625); # tv_usec/10000
      }
  #endif
  #if defined(DJUNIX) || defined(WATCOM)
    typedef struct { uintW year;  # Jahr (1980..2099)
                     uintB month; # Monat (1..12)
                     uintB day;   # Tag (1..31)
                     uintB hour;  # Stunde (0..23)
                     uintB min;   # Minute (0..59)
                     uintB sec;   # Sekunde (0..59)
                     uintB hsec;  # Hundertstel Sekunde (0..59)
                   }
            internal_decoded_time;
    local void get_decoded_time (internal_decoded_time* timepoint);
    local void get_decoded_time(timepoint)
      var internal_decoded_time* timepoint;
      { var union REGS in;
        var union REGS out;
        begin_system_call();
        loop
          { # Datum-Teil holen:
            in.regB.ah = 0x2A; # DOS Get Date
            intdos(&in,&out);
            timepoint->year = out.regW.cx;
            timepoint->month = out.regB.dh;
            timepoint->day = out.regB.dl;
            # Uhrzeit-Teil holen:
            in.regB.ah = 0x2C; # DOS Get Time
            intdos(&in,&out);
            timepoint->hour = out.regB.ch;
            timepoint->min = out.regB.cl;
            timepoint->sec = out.regB.dh;
            timepoint->hsec = out.regB.dl;
            # und auf Tageswechsel überprüfen:
            if (!(timepoint->sec == 0)) break;
            if (!(timepoint->min == 0)) break;
            if (!(timepoint->hour == 0)) break;
            in.regB.ah = 0x2A; # DOS Get Date
            intdos(&in,&out);
            if (timepoint->day == out.regB.dl) break;
            # Datum hat sich zwischenzeitlich verändert -> wiederholen
          }
        end_system_call();
      }
    global uintL get_time()
      { var internal_decoded_time timepoint;
        get_decoded_time(&timepoint);
       {local var uintW monthoffsets[12] = { # Jahrtag ab dem letzten 1. März
          # Monat  1   2   3  4  5  6  7   8   9   10  11  12
                  306,337, 0,31,61,92,122,153,184,214,245,275,
          };
        var uintL UTTag;
        timepoint.year -= 1980;
        if (timepoint.month >= 3) { timepoint.year += 1; }
        UTTag = (uintL)timepoint.year * 365 + (uintL)ceiling(timepoint.year,4)
                + (uintL)monthoffsets[timepoint.month-1] + (uintL)timepoint.day + 3345;
        # Zeitzone mitberücksichtigen??
        return (((UTTag * 24 + (uintL)timepoint.hour)
                        * 60 + (uintL)timepoint.min)
                        * 60 + (uintL)timepoint.sec)
                        * 100 + (uintL)timepoint.hsec;
      }}
  #endif
  #if defined(EMUNIX)
    global uintL get_time()
      { var struct timeb real_time;
        begin_system_call();
        __ftime(&real_time);
        end_system_call();
        return (uintL)(real_time.time) * ticks_per_second
               + (uintL)((uintW)(real_time.millitm) / (1000/ticks_per_second));
      }
  #endif
 #endif
 #ifdef TIME_UNIX_TIMES
# < uintL ergebnis : aktueller Stand des CLK_TCK Hz - Zählers
  local uintL get_time(void);
  local uintL get_time()
    { var struct tms buffer;
      return (uintL)times(&buffer);
    }
 #endif
 #ifdef TIME_RISCOS
# < uintL ergebnis : aktueller Stand des CLK_TCK Hz - Zählers
  global uintL get_time(void);
  #include <sys/os.h>
  global uintL get_time()
    { var int regs[10];
      var os_error * err;
      begin_system_call();
      err = os_swi(0x42,regs);
      if (err) { __seterr(err); OS_error(); }
      end_system_call();
      return (uintL)(regs[0]);
    }
 #endif

#ifndef HAVE_RUN_TIME

# UP: Hält die Run-Time-Stoppuhr an
# run_time_stop();
  global void run_time_stop (void);
  global void run_time_stop()
    { if (!run_flag) return; # Run-Time-Stoppuhr ist schon angehalten -> OK
      # zuletzt verbrauchte Run-Time zur bisherigen Run-Time addieren:
      run_time += get_time()-runstop_time;
      run_flag = FALSE; # Run-Time-Stoppuhr steht
    }

# UP: Lässt die Run-Time-Stoppuhr weiterlaufen
# run_time_restart();
  global void run_time_restart (void);
  global void run_time_restart()
    { if (run_flag) return; # Run-Time-Stoppuhr läuft schon -> OK
      runstop_time = get_time(); # aktuelle Zeit abspeichern
      run_flag = TRUE; # Run-Time-Stoppuhr läuft
    }

#endif

# UP: Liefert die Real-Time
# get_real_time()
# < uintL ergebnis: Zeit seit LISP-System-Start (in 1/200 sec bzw. in 1/50 sec bzw. in 1/100 sec bzw. in 1/CLK_TCK sec)
  global uintL get_real_time (void);
  global uintL get_real_time()
    { return get_time()-realstart_time; }

#endif

#ifdef TIME_UNIX_TIMES

# UP: Liefert die Run-Time
# get_run_time(&runtime);
# < internal_time runtime: Run-Time seit LISP-System-Start (in Ticks)
# < uintL ergebnis: wie get_time()
  global uintL get_run_time (internal_time* runtime);
  global uintL get_run_time(runtime)
    var internal_time* runtime;
    { var struct tms tms;
      var uintL now_time;
      begin_system_call();
      now_time = times(&tms);
      end_system_call();
      *runtime = tms.tms_utime + tms.tms_stime; # User time + System time
      return now_time; # vgl. get_time()
    }

#endif

#ifdef TIME_UNIX

# UP: Liefert die Real-Time
# get_real_time()
# < internal_time* ergebnis: absolute Zeit
  global void get_real_time (internal_time*);
  global void get_real_time(internal_time* it)
    {
     #ifdef HAVE_GETTIMEOFDAY
      begin_system_call();
      if (gettimeofday((struct timeval *)it,NULL) != 0) { OS_error(); }
      end_system_call();
     #elif defined(HAVE_FTIME)
      var struct timeb timebuf;
      begin_system_call();
      ftime(&timebuf);
      end_system_call();
      it->tv_sec = timebuf.time;
      it->tv_usec = (uintL)(timebuf.millitm) * (ticks_per_second/1000);
     #endif
    }

# UP: Liefert die Run-Time
# get_run_time(&runtime);
# < internal_time runtime: Run-Time seit LISP-System-Start (in Ticks)
  global void get_run_time (internal_time* runtime);
  global void get_run_time(runtime)
    var internal_time* runtime;
    {
      #if defined(HAVE_GETRUSAGE)
      var struct rusage rusage;
      begin_system_call();
      if (!( getrusage(RUSAGE_SELF,&rusage) ==0)) { OS_error(); }
      end_system_call();
      # runtime = rusage.ru_utime + rusage.ru_stime; # User time + System time
      add_internal_time(rusage.ru_utime,rusage.ru_stime, *runtime);
      #elif defined(HAVE_SYS_TIMES_H)
      var uintL used_time; # verbrauchte Zeit, gemessen in 1/HZ Sekunden
      var struct tms tms;
      begin_system_call();
      if (times(&tms) == (CLOCK_T)(-1))
        { used_time = 0; } # times scheitert -> used_time unbekannt
        else
        { used_time = tms.tms_utime + tms.tms_stime; } # User time + System time
      end_system_call();
      # in Sekunden und Mikrosekunden umwandeln: # verwende HZ oder CLK_TCK ??
      runtime->tv_sec = floor(used_time,HZ);
      runtime->tv_usec = (used_time % HZ) * floor(2*1000000+HZ,2*HZ);
      #endif
    }

#endif

#ifdef TIME_WIN32

# UP: Liefert die Real-Time
# get_real_time()
# < internal_time* ergebnis: absolute Zeit
  global void get_real_time (internal_time*);
  global void get_real_time(internal_time* it)
    { var struct timeb timebuf;
      begin_system_call();
      ftime(&timebuf);
      end_system_call();
     {
      #ifdef HAVE_LONGLONG
      var ULONGLONG ticks =
        + (ULONGLONG)134774 * (ULONGLONG)86400 * (ULONGLONG)ticks_per_second
        + (ULONGLONG)timebuf.time * (ULONGLONG)ticks_per_second
        + (ULONGLONG)timebuf.millitm * (ULONGLONG)(ticks_per_second/1000);
      it->dwLowDateTime = (uint32)ticks;
      it->dwHighDateTime = (uint32)(ticks>>32);
      #else
      var internal_time t1 = { 0xD53E8000, 0x19DB1DE };
      var internal_time t2;
      var internal_time t3;
      mulu32(timebuf.time,ticks_per_second,
             t1.dwHighDateTime=,t1.dwLowDateTime=);
      mulu32(timebuf.millitm,ticks_per_second/1000,
             t2.dwHighDateTime=,t2.dwLowDateTime=);
      add_internal_time(t1,t2, it);
      add_internal_time(it,t3, it);
      #endif
    }}

# UP: Liefert die Run-Time
# get_run_time(&runtime);
# < internal_time runtime: Run-Time seit LISP-System-Start (in Ticks)
  global void get_run_time (internal_time* runtime);
  global void get_run_time(runtime)
    var internal_time* runtime;
    { var FILETIME creation_time;
      var FILETIME exit_time;
      var FILETIME kernel_time;
      var FILETIME user_time;
      begin_system_call();
      if (GetProcessTimes(GetCurrentProcess(),&creation_time,&exit_time,&kernel_time,&user_time))
        { end_system_call();
          add_internal_time(user_time,kernel_time, *runtime); # User time + Kernel time
        }
        else
        { if (!(GetLastError()==ERROR_CALL_NOT_IMPLEMENTED)) { OS_error(); }
          # GetProcessTimes() is not implemented on Win95. Use get_real_time()
          # instead. This is only a crude approximation, I know.
          # (We keep HAVE_RUN_TIME defined, so that Win95 users will notice
          # that "Run time" and "Real time" are always the same and draw their
          # conclusions from it.)
          end_system_call();
         {var internal_time real_time;
          get_real_time(&real_time);
          sub_internal_time(real_time,realstart_time, *runtime);
        }}
    }

#endif

# UP: Liefert die Run-Time
# get_running_times(&timescore);
# < timescore.runtime:  Run-Time seit LISP-System-Start (in Ticks)
# < timescore.realtime: Real-Time seit LISP-System-Start (in Ticks)
# < timescore.gctime:   GC-Time seit LISP-System-Start (in Ticks)
# < timescore.gccount:  Anzahl der GC's seit LISP-System-Start
# < timescore.gcfreed:  Größe des von den GC's bisher wiederbeschafften Platzes
  global void get_running_times (timescore*);
  global void get_running_times (tm)
    var timescore* tm;
    {
     #ifndef HAVE_RUN_TIME
      var uintL time = get_time();
      tm->realtime = time - realstart_time;
      tm->runtime = (run_flag ?
                      time - runstop_time + run_time : # Run-Time-Stoppuhr läuft noch
                      run_time # Run-Time-Stoppuhr steht
                    );
     #endif
     #ifdef TIME_UNIX
      # Real-Time holen:
      var internal_time real_time;
      get_real_time(&real_time);
      tm->realtime.tv_sec = real_time.tv_sec - realstart_time.tv_sec;
      tm->realtime.tv_usec = real_time.tv_usec;
      # Run-Time holen:
      get_run_time(&tm->runtime);
     #endif
     #ifdef TIME_UNIX_TIMES
      # Run-Time und Real-Time auf einmal holen:
      tm->realtime = get_run_time(&tm->runtime) - realstart_time; # vgl. get_real_time()
     #endif
     #ifdef TIME_WIN32
      # Real-Time holen:
      var internal_time real_time;
      get_real_time(&real_time);
      sub_internal_time(real_time,realstart_time, tm->realtime);
      # Run-Time holen:
      get_run_time(&tm->runtime);
     #endif
      tm->gctime = gc_time;
      tm->gccount = gc_count;
      tm->gcfreed = gc_space;
    }

#if defined(MSDOS)
# UP: Wandelt das DOS-Zeitformat in Decoded-Time um.
# convert_timedate(time,date,&timepoint)
# > uintW time: Uhrzeit
#         Als Word: Bits 15..11: Stunde in {0,...,23},
#                   Bits 10..5:  Minute in {0,...,59},
#                   Bits 4..0:   Sekunde/2 in {0,...,29}.
# > uintW date: Datum
#         Als Word: Bits 15..9: Jahr-1980 in {0,...,119},
#                   Bits 8..5:  Monat in {1,...,12},
#                   Bits 4..0:  Tag in {1,...,31}.
# < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
#   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums
  global void convert_timedate (uintW time, uintW date, decoded_time* timepoint);
  global void convert_timedate(time,date, timepoint)
    var uintW time;
    var uintW date;
    var decoded_time* timepoint;
    { timepoint->Sekunden = fixnum( (time & (bit(5) - 1)) << 1 );
      time = time>>5;
      timepoint->Minuten = fixnum( time & (bit(6) - 1));
      time = time>>6;
      timepoint->Stunden = fixnum( time);
      timepoint->Tag = fixnum( date & (bit(5) - 1));
      date = date>>5;
      timepoint->Monat = fixnum( date & (bit(4) - 1));
      date = date>>4;
      timepoint->Jahr = fixnum( date+1980);
    }
#endif
#ifdef AMIGAOS
# UP: Wandelt das Amiga-Zeitformat in Decoded-Time um.
# convert_time(&datestamp,&timepoint);
# > struct DateStamp datestamp: Uhrzeit
#          datestamp.ds_Days   : Anzahl Tage seit 1.1.1978
#          datestamp.ds_Minute : Anzahl Minuten seit 00:00 des Tages
#          datestamp.ds_Tick   : Anzahl Ticks seit Beginn der Minute
# < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
#   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums
  # include "arilev0.c"  # für Division
  global void convert_time (const struct DateStamp * datestamp, decoded_time* timepoint);
  global void convert_time(datestamp,timepoint)
    var const struct DateStamp * datestamp;
    var decoded_time* timepoint;
    { # Methode:
      # ds_Tick durch ticks_per_second dividieren, liefert Sekunden.
      # ds_Minute durch 60 dividierem liefert Stunden und (als Rest) Minuten.
      # ds_Days in Tag, Monat, Jahr umrechnen:
      #   d := ds_Days - 790; # Tage seit 1.3.1980 (Schaltjahr)
      #   y := floor((4*d+3)/1461); # März-Jahre ab 1.3.1980
      #   d := d - floor(y*1461/4); # Tage ab letztem März-Jahres-Anfang
      #   (Diese Rechnung geht gut, solange jedes vierte Jahr ein Schaltjahr
      #    ist, d.h. bis zum Jahr 2099.)
      #   m := floor((5*d+2)/153); # Monat ab letztem März
      #   d := d - floor((153*m+2)/5); # Tag ab letztem Monatsanfang
      #   m := m+2; if (m>=12) then { m:=m-12; y:=y+1; } # auf Jahre umrechnen
      #   Tag d+1, Monat m+1, Jahr 1980+y.
      {var uintL sec;
       divu_3216_1616(datestamp->ds_Tick,ticks_per_second,sec=,);
       timepoint->Sekunden = fixnum(sec);
      }
      {var uintL std;
       var uintL min;
       divu_3216_1616(datestamp->ds_Minute,60,std=,min=);
       timepoint->Minuten = fixnum(min);
       timepoint->Stunden = fixnum(std);
      }
      {var uintL y;
       var uintW m;
       var uintW d;
       divu_3216_1616(4*(datestamp->ds_Days - 424),1461,y=,d=); # y = März-Jahre ab 1.1.1979
       d = floor(d,4); # Tage ab dem letzten März-Jahres-Anfang
       divu_1616_1616(5*d+2,153,m=,d=); # m = Monat ab letztem März
       d = floor(d,5); # Tag ab letztem Monatsanfang
       # m=0..9 -> Monat März..Dezember des Jahres 1979+y,
       # m=10..11 -> Monat Januar..Februar des Jahres 1980+y.
       if (m<10) { m += 12; y -= 1; } # auf Jahre umrechnen
       timepoint->Tag = fixnum(1+(uintL)d);
       timepoint->Monat = fixnum(-9+(uintL)m);
       timepoint->Jahr = fixnum(1980+y);
    } }
#endif
#if defined(UNIX) || defined(MSDOS) || defined(RISCOS)
# UP: Wandelt das System-Zeitformat in Decoded-Time um.
# convert_time(&time,&timepoint);
# > time_t time: Zeit im System-Zeitformat
# < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
#   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums
  global void convert_time (const time_t* time, decoded_time* timepoint);
  global void convert_time(time,timepoint)
    var const time_t* time;
    var decoded_time* timepoint;
    { begin_system_call();
     {var struct tm * tm = localtime(time); # decodieren
      # (Das Zeitformat des Systems muss auch das System auseinandernehmen.)
      end_system_call();
      if (!(tm==NULL))
        # localtime war erfolgreich
        { timepoint->Sekunden = fixnum(tm->tm_sec);
          timepoint->Minuten  = fixnum(tm->tm_min);
          timepoint->Stunden  = fixnum(tm->tm_hour);
          timepoint->Tag      = fixnum(tm->tm_mday);
          timepoint->Monat    = fixnum(1+tm->tm_mon);
          timepoint->Jahr     = fixnum(1900+tm->tm_year);
        }
        else
        # gescheitert -> verwende 1.1.1900, 00:00:00 als Default
        { timepoint->Sekunden = Fixnum_0;
          timepoint->Minuten  = Fixnum_0;
          timepoint->Stunden  = Fixnum_0;
          timepoint->Tag      = Fixnum_1;
          timepoint->Monat    = Fixnum_1;
          timepoint->Jahr     = fixnum(1900);
        }
    }}
#endif
#ifdef WIN32_NATIVE
# UP: Wandelt das System-Zeitformat in Decoded-Time um.
# convert_time(&time,&timepoint);
# > FILETIME time: Zeit im System-Zeitformat
# < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
#   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums
  global void convert_time (const FILETIME* time, decoded_time* timepoint);
  global void convert_time(time,timepoint)
    var const FILETIME* time;
    var decoded_time* timepoint;
    { var FILETIME ltime;
      var SYSTEMTIME ltm;
      if (!FileTimeToLocalFileTime(time,&ltime)) { OS_error(); }
      if (!FileTimeToSystemTime(&ltime,&ltm)) { OS_error(); }
      timepoint->Sekunden = fixnum(ltm.wSecond);
      timepoint->Minuten  = fixnum(ltm.wMinute);
      timepoint->Stunden  = fixnum(ltm.wHour);
      timepoint->Tag      = fixnum(ltm.wDay);
      timepoint->Monat    = fixnum(ltm.wMonth);
      timepoint->Jahr     = fixnum(ltm.wYear);
    }
#endif

# UP: Initialisiert die Zeitvariablen beim LISP-System-Start.
# init_time();
  global void init_time (void);
  global void init_time()
    {
      # Es ist noch keine GC dagewesen -> hat auch noch keine Zeit verbraucht.
      # gc_count=0;
      # gc_time=0;
      # gc_space=0;
      #ifdef TIME_RELATIVE
      realstart_time = get_time(); # Zeitzähler jetzt, beim Systemstart
      #endif
      #ifndef HAVE_RUN_TIME
      # run_time = 0; # Noch keine Run-Time verbraucht,
      # run_flag = FALSE; # denn System läuft noch nicht.
      run_time_restart(); # Run-Time-Stoppuhr loslaufen lassen
      #endif
      #if defined(TIME_UNIX) || defined(TIME_WIN32)
      get_real_time(&realstart_time); # Zeitzähler jetzt, beim Systemstart
      #endif
      #ifdef TIME_RELATIVE
      # Start-Zeit holen und merken:
      { var decoded_time timepoint;
        #ifdef AMIGAOS
        { var struct DateStamp datestamp; # aktuelle Uhrzeit
          DateStamp(&datestamp);
          convert_time(&datestamp,&timepoint); # in Decoded-Time umwandeln
        }
        #endif
        #if defined(DJUNIX) && 0 # das geht eine Stunde nach!!
        { var struct timeval real_time;
          begin_system_call();
          gettimeofday(&real_time,NULL); # aktuelle Uhrzeit
          end_system_call();
          convert_time(&real_time.tv_sec,&timepoint); # in Decoded-Time umwandeln
        }
        #endif
        #if defined(DJUNIX) || defined(WATCOM)
        { var internal_decoded_time idt;
          get_decoded_time(&idt);
          timepoint.Sekunden = fixnum(idt.sec);
          timepoint.Minuten  = fixnum(idt.min);
          timepoint.Stunden  = fixnum(idt.hour);
          timepoint.Tag      = fixnum(idt.day);
          timepoint.Monat    = fixnum(idt.month);
          timepoint.Jahr     = fixnum(idt.year);
        }
        #endif
        #if defined(EMUNIX)
        { var struct timeb real_time;
          begin_system_call();
          __ftime(&real_time); # aktuelle Uhrzeit
          end_system_call();
          convert_time(&real_time.time,&timepoint); # in Decoded-Time umwandeln
        }
        #endif
        #if defined(UNIX) || defined(RISCOS) # TIME_UNIX_TIMES || TIME_RISCOS
        { var time_t real_time;
          begin_system_call();
          time(&real_time); # aktuelle Uhrzeit
          end_system_call();
          convert_time(&real_time,&timepoint); # in Decoded-Time umwandeln
        }
        #endif
        set_start_time(&timepoint); # Start-Zeit merken
      }
      #endif
    }

# -----------------------------------------------------------------------------
#                            Zeitfunktionen

#ifdef TIME_AMIGAOS
  # Ein kleineres Bug:
  # - Wrap-Around der Uhrzeit nach 2.7 Jahren.
  # Decoded Time =
  #   Sekunde, Minute, Stunde, Tag, Monat, Jahr, Wochentag, Sommerzeit, Zeitzone
  # Universal Time =
  #   Sekunden seit 1.1.1900
  # Internal Time =
  #   50stel Sekunden seit LISP-System-Start
#endif
#ifdef TIME_MSDOS
  # Ein kleineres Bug:
  # - Wrap-Around der Uhrzeit nach 1.36 Jahren.
  # Decoded Time =
  #   Sekunde, Minute, Stunde, Tag, Monat, Jahr, Wochentag, Sommerzeit, Zeitzone
  # Universal Time =
  #   Sekunden seit 1.1.1900
  # Internal Time =
  #   100stel Sekunden seit LISP-System-Start
#endif
#if defined(TIME_UNIX_TIMES) || defined(TIME_RISCOS)
  # Zwei kleinere Bugs:
  # - Wrap-Around der Uhrzeit nach vielen Tagen,
  # - LISP-Uhr geht um max. 1 Sekunde nach gegenüber der wahren Uhr.
  # Decoded Time =
  #   Sekunde, Minute, Stunde, Tag, Monat, Jahr, Wochentag, Sommerzeit, Zeitzone
  # Universal Time =
  #   Sekunden seit 1.1.1900
  # Internal Time =
  #   CLK_TCK-stel Sekunden seit LISP-System-Start
#endif
#ifdef TIME_UNIX
  # Ein kleineres Bug:
  # - %%TIME funktioniert nur für Zeitdifferenzen <= 194 Tagen.
  # Decoded Time =
  #   Sekunde, Minute, Stunde, Tag, Monat, Jahr, Wochentag, Sommerzeit, Zeitzone
  # Universal Time =
  #   Sekunden seit 1.1.1900
  # Internal Time =
  #   Mikrosekunden seit LISP-System-Start
#endif
#ifdef TIME_WIN32
  # Ein kleineres Bug:
  # - %%TIME funktioniert nur für Zeitdifferenzen <= 19 Tagen.
  # Decoded Time =
  #   Sekunde, Minute, Stunde, Tag, Monat, Jahr, Wochentag, Sommerzeit, Zeitzone
  # Universal Time =
  #   Sekunden seit 1.1.1900
  # Internal Time =
  #   Zehntel Mikrosekunden seit LISP-System-Start
#endif

#ifdef TIME_RELATIVE

# Uhrzeit und Datum beim LISP-Start:
  local decoded_time realstart_datetime;

# UP: Berechnet die Uhrzeit beim LISP-System-Start als Universal Time.
# calc_start_UT(&timepoint)
# > decoded_time timepoint: Zeit beim LISP-System-Start
# < ergebnis: Universal Time
# can trigger GC
  local object calc_start_UT (const decoded_time* timepoint);
  local object calc_start_UT(timepoint)
    var const decoded_time* timepoint;
    { # (ENCODE-UNIVERSAL-TIME Sekunden Minuten Stunden Tag Monat Jahr) ausführen:
      pushSTACK(timepoint->Sekunden);
      pushSTACK(timepoint->Minuten);
      pushSTACK(timepoint->Stunden);
      pushSTACK(timepoint->Tag);
      pushSTACK(timepoint->Monat);
      pushSTACK(timepoint->Jahr);
      funcall(S(encode_universal_time),6);
      # als Start-Universal-Time abspeichern:
      return O(start_UT) = value1;
    }

# UP: Merkt sich die Uhrzeit beim LISP-System-Start.
# set_start_time(&timepoint);
# > timepoint: Zeit beim LISP-System-Start
# >   timepoint.Sekunden in {0,...,59},
# >   timepoint.Minuten in {0,...,59},
# >   timepoint.Stunden in {0,...,23},
# >   timepoint.Tag in {1,...,31},
# >   timepoint.Monat in {1,...,12},
# >   timepoint.Jahr in {1980,...,2999},
# >   jeweils als Fixnums.
# can trigger GC
  global void set_start_time (const decoded_time* timepoint);
  global void set_start_time(timepoint)
    var const decoded_time* timepoint;
    { # Start-Zeit merken:
      realstart_datetime = *timepoint;
      # und, wenn möglich, gleich in Universal Time umwandeln:
      if (!eq(Symbol_function(S(encode_universal_time)),unbound))
        # Ist ENCODE-UNIVERSAL-TIME definiert -> sofort in UT umwandeln:
        { calc_start_UT(timepoint); }
    }

#endif

# Liefert die Uhrzeit in Sekunden (seit Systemstart bzw. 1.1.1900) als uintL.
  local uintL real_time_sec (void);
  local uintL real_time_sec()
    {
     #ifdef TIME_1
      var uintL real_time = get_real_time();
      # real_time := floor(real_time,ticks_per_second) :
      #if (ticks_per_second == 1000000UL)
        divu_3216_3216(real_time>>6,ticks_per_second>>6,real_time=,);
      #elif (ticks_per_second < bit(16))
        divu_3216_3216(real_time,ticks_per_second,real_time=,);
      #else
        divu_3232_3232(real_time,ticks_per_second,real_time=,);
      #endif
     #endif
     #ifdef TIME_2
      #ifdef TIME_UNIX
       var uintL real_time; # Sekunden
       var internal_time it;
       get_real_time(&it);
       # it.tv_sec sind Sekunden seit 1.1.1970
       # 25567*24*60*60 Sekunden zwischen 1.1.1900 und 1.1.1970
       real_time = 2208988800UL + it.tv_sec;
      #endif
      #ifdef TIME_WIN32
       var internal_time offset = # difference between 1.1.1601 and 1.1.1900
       #ifdef HAVE_LONGLONG
         { (ULONG)((ULONGLONG)109207 * (ULONGLONG)86400 * (ULONGLONG)ticks_per_second),
           (ULONG)(((ULONGLONG)109207 * (ULONGLONG)86400 * (ULONGLONG)ticks_per_second) >> 32)
         };
       #else
         { 0xFDE04000, 0x14F373B };
       #endif
       var internal_time internal_real_time;
       var uintL real_time;
       get_real_time(&internal_real_time);
       sub_internal_time(internal_real_time,offset,internal_real_time);
       divu_6432_3232(internal_real_time.dwHighDateTime,
                      internal_real_time.dwLowDateTime,
                      ticks_per_second,
                      real_time=,);
      #endif
     #endif
     return real_time;
    }

LISPFUNN(get_universal_time,0)
# (get-universal-time), CLTL S. 445
#ifdef TIME_RELATIVE
  # (defun get-universal-time ()
  #   (+ (sys::get-start-time)
  #      (floor (get-internal-real-time) internal-time-units-per-second)
  # ) )
  { var object start_time = O(start_UT);
    if (nullp(start_time)) # Start-Universal-Time noch NIL ?
      # nein -> schon berechnet.
      # ja -> jetzt erst berechnen:
      { start_time = calc_start_UT(&realstart_datetime); }
    # start_time = die Uhrzeit des LISP-System-Starts in Universal Time.
    pushSTACK(start_time);
    pushSTACK(UL_to_I(real_time_sec())); # Sekunden seit Systemstart
    funcall(L(plus),2); # addieren
  }
#endif
#ifdef TIME_ABSOLUTE
  { value1 = UL_to_I(real_time_sec()); mv_count=1; }
#endif

#if defined(UNIX) || defined(WIN32)
LISPFUN(default_time_zone,0,1,norest,nokey,0,NIL)
# (sys::default-time-zone) liefert die aktuelle Zeitzone.
# (sys::default-time-zone UTstunde) liefert die aktuelle Zeitzone zu einem
# bestimmten Zeitpunkt.
# 1. Wert: Zeitzone mit Sommerzeit-Berücksichtigung.
# 2. Wert: Sommerzeit-p.
  { # Da die Zeitzone oft per TZ-Environment-Variable einstellbar ist, wird
    # sie häufig außerhalb des Kernels verwaltet. Man hat nur per localtime()
    # und gmtime() Zugriff auf sie.
    # Methode:
    #   Zeitzone = (gmtime(t) - localtime(t))/3600.
    #   Sommerzeit-p wird dem Ergebnis von localtime(t) entnommen.
    var object arg = popSTACK();
    var time_t now;
    if (integerp(arg))
      { # bestimmter Zeitpunkt
        # Annahme: time_t ist die Anzahl der Sekunden seit 1.1.1970. ??
        if (posfixnump(arg)
            && (posfixnum_to_L(arg) >= 613608) # arg >= 1.1.1970
            && (posfixnum_to_L(arg) < 1314888) # arg < 1.1.2050
           )
          { now = (posfixnum_to_L(arg) - 613608) * 3600; }
        elif (R_minusp(arg) || (posfixnump(arg) && (posfixnum_to_L(arg) < 613608)))
          { now = 0; } # < 1.1.1970 -> treat like 1.1.1970
        else
          { now = (uintL)(1314888 - 613608) * 3600; } # >= 1.1.2050 -> treat like 1.1.2050
      }
      else
      # jetzt
      { begin_system_call(); time(&now); end_system_call(); }
    { var struct tm now_local;
      var struct tm now_gm;
      begin_system_call();
      now_local = *(localtime(&now));
      now_gm = *(gmtime(&now));
      end_system_call();
      # secondswest = mktime(now_gm) - mktime(now_local); wäre schön.
      # mktime() ist allerdings nicht weit verbreitet. Unter SunOS4 müsste man
      # timegm() nehmen. Daher tun wir's selber:
     {var sintL dayswest = # Tage-Differenz, kann als 0,1,-1 angenommen werden
        (now_gm.tm_year < now_local.tm_year ? -1 :
         now_gm.tm_year > now_local.tm_year ? 1 :
         (now_gm.tm_mon < now_local.tm_mon ? -1 :
          now_gm.tm_mon > now_local.tm_mon ? 1 :
          (now_gm.tm_mday < now_local.tm_mday ? -1 :
           now_gm.tm_mday > now_local.tm_mday ? 1 :
           0
        )));
      var sintL hourswest = 24*dayswest + (sintL)(now_gm.tm_hour - now_local.tm_hour);
      var sintL minuteswest = 60*hourswest + (sintL)(now_gm.tm_min - now_local.tm_min);
      var sintL secondswest = 60*minuteswest + (sintL)(now_gm.tm_sec - now_local.tm_sec);
      # Zeitzone in Stunden = (Zeitzone in Sekunden / 3600) :
      pushSTACK(L_to_I(secondswest));
      pushSTACK(fixnum(3600));
      funcall(L(durch),2);
      # Sommerzeit-p entnehmen:
      # tm_isdst < 0 bedeutet "unbekannt"; wir nehmen an, keine Sommerzeit.
      value2 = (now_local.tm_isdst > 0 ? T : NIL);
      mv_count=2;
  } }}
#endif # UNIX || WIN32

LISPFUNN(get_internal_run_time,0)
# (GET-INTERNAL-RUN-TIME), CLTL S. 446
  { var timescore tm;
    get_running_times(&tm); # Run-Time seit LISP-System-Start abfragen
   #ifdef TIME_1
    value1 = UL_to_I(tm.runtime); mv_count=1; # in Integer umwandeln
   #endif
   #ifdef TIME_2
    { var internal_time* tp = &tm.runtime; # Run-Time
      #ifdef TIME_UNIX
       # in Mikrosekunden umwandeln: tp->tv_sec * ticks_per_second + tp->tv_usec
       #ifdef intQsize
       value1 = UQ_to_I((uintQ)(tp->tv_sec) * ticks_per_second + (uintQ)(tp->tv_usec));
       #else
       {var uintL run_time_hi;
        var uintL run_time_lo;
        mulu32(tp->tv_sec,ticks_per_second, run_time_hi=,run_time_lo=);
        if ((run_time_lo += tp->tv_usec) < tp->tv_usec) { run_time_hi += 1; }
        value1 = L2_to_I(run_time_hi,run_time_lo);
       }
       #endif
      #endif
      #ifdef TIME_WIN32
       value1 = L2_to_I(tp->dwHighDateTime,tp->dwLowDateTime);
      #endif
      mv_count=1;
    }
   #endif
  }

LISPFUNN(get_internal_real_time,0)
# (GET-INTERNAL-REAL-TIME), CLTL S. 446
#ifdef TIME_1
  { value1 = UL_to_I(get_real_time()); # Real-Time seit LISP-System-Start, als Integer
    mv_count=1;
  }
#endif
#ifdef TIME_2
  { var internal_time tp; # Real-Time absolut
    get_real_time(&tp);
    #ifdef TIME_UNIX
     # in Mikrosekunden umwandeln: tp.tv_sec * ticks_per_second + tp.tv_usec
     #ifdef intQsize
     value1 = UQ_to_I((uintQ)(tp.tv_sec) * ticks_per_second +
                      (uintQ)(tp.tv_usec));
     #else
     {var uintL real_time_hi;
      var uintL real_time_lo;
      mulu32(tp.tv_sec,ticks_per_second, real_time_hi=,real_time_lo=);
      if ((real_time_lo += tp.tv_usec) < tp.tv_usec) { real_time_hi += 1; }
      value1 = L2_to_I(real_time_hi,real_time_lo);
     }
     #endif
    #endif
    #ifdef TIME_WIN32
     value1 = L2_to_I(tp.dwHighDateTime,tp.dwLowDateTime);
    #endif
    mv_count=1;
  }
#endif

#ifdef SLEEP_1
LISPFUNN(sleep,1)
#if defined(TIME_MSDOS) || defined(RISCOS)
# (SYSTEM::%SLEEP delay) wartet delay/200 bzw. delay/100 Sekunden.
# Argument delay muss ein Integer >=0, <2^32 (TIME_MSDOS: sogar <2^31) sein.
  { var uintL delay = I_to_UL(popSTACK()); # Pausenlänge
    #ifdef EMUNIX_PORTABEL
    if (TRUE)
      # Unter OS/2 (Multitasking!) nicht CPU-Zeit verbraten!
      # select erlaubt eine wunderschöne Implementation von usleep():
      { loop
          { var uintL start_time = get_real_time();
            { var struct timeval timeout; # Zeitintervall
              divu_3216_3216(delay,ticks_per_second, timeout.tv_sec =, timeout.tv_usec = 1000000/ticks_per_second * (uintL) );
              begin_system_call();
             {var int ergebnis = select(FD_SETSIZE,NULL,NULL,NULL,&timeout);
              if ((ergebnis<0) && !(errno==EINTR)) { OS_error(); }
              end_system_call();
            }}
            interruptp( { pushSTACK(S(sleep)); tast_break(); } ); # evtl. Break-Schleife aufrufen
           {var uintL end_time = get_real_time();
            var uintL slept = end_time - start_time; # so lang haben wir geschlafen
            # Haben wir genug geschlafen?
            if (slept >= delay) break;
            # Wie lange müssen wir noch schlafen?
            delay -= slept;
          }}
      }
      else
    #endif
    { var uintL endtime = get_real_time() + delay; # zur momentanen Real-Time addieren,
      # ergibt Zeit, bis zu der zu warten ist.
      # warten, bis die Real-Time bei endtime angelangt ist:
      # (Attention: the MSDOS clock always advances 5 or 6 ticks at a time!)
      do {} until ((sintL)(get_real_time()-endtime) >= 0);
    }
    value1 = NIL; mv_count=1; # 1 Wert NIL
  }
#endif
#ifdef TIME_AMIGAOS
# (SYSTEM::%SLEEP delay) wartet delay/50 Sekunden.
# Argument delay muss ein Integer >=0, <2^32 sein.
  { var uintL delay = I_to_UL(popSTACK()); # Pausenlänge
    if (delay>0) { begin_system_call(); Delay(delay); end_system_call(); }
    value1 = NIL; mv_count=1; # 1 Wert NIL
  }
#endif
#endif
#ifdef SLEEP_2
#ifdef TIME_UNIX_TIMES
# Ein sehr unvollkommener Ersatz für die gettimeofday-Funktion.
# Taugt nur für die Messung von Zeitdifferenzen!
  local int gettimeofday (struct timeval * tp, void* tzp);
  local int gettimeofday(tp,tzp)
    var struct timeval * tp;
    var void* tzp;
    { if (!(tp==NULL))
        { var uintL realtime = get_real_time();
          # in Sekunden und Mikrosekunden umwandeln:
          tp->tv_sec = floor(realtime,ticks_per_second);
          tp->tv_usec = (realtime % ticks_per_second) * floor(2*1000000+ticks_per_second,2*ticks_per_second);
        }
      return 0;
    }
#endif
LISPFUNN(sleep,2)
#if defined(TIME_UNIX) || defined(TIME_UNIX_TIMES)
# (SYSTEM::%SLEEP delay-seconds delay-useconds) wartet
# delay-seconds Sekunden und delay-useconds Mikrosekunden.
# Argument delay-seconds muss ein Fixnum >=0, <=16700000 sein,
# Argument delay-useconds muss ein Fixnum >=0, <=1000000 sein.
  { var uintL useconds = posfixnum_to_L(popSTACK());
    var uintL seconds = posfixnum_to_L(popSTACK());
    begin_system_call();
    loop
      { var struct timeval start_time;
        var struct timeval end_time;
        if (!( gettimeofday(&start_time,NULL) ==0)) { OS_error(); }
        #ifdef HAVE_SELECT
          # select erlaubt eine wunderschöne Implementation von usleep():
          { var struct timeval timeout; # Zeitintervall
            timeout.tv_sec = seconds; timeout.tv_usec = useconds;
           {var int ergebnis;
            ergebnis = select(FD_SETSIZE,NULL,NULL,NULL,&timeout);
            if ((ergebnis<0) && !(errno==EINTR)) { OS_error(); }
          }}
        #else
          if (seconds>0) { sleep(seconds); }
          #ifdef HAVE_USLEEP
          if (useconds>0) { usleep(useconds); }
          #endif
        #endif
        interruptp(
          { end_system_call();
            pushSTACK(S(sleep)); tast_break(); # evtl. Break-Schleife aufrufen
            begin_system_call();
          });
        if (!( gettimeofday(&end_time,NULL) ==0)) { OS_error(); }
       {# Überprüfen, ob wir genügend lang geschlafen haben, oder ob
        # wir wegen eines Signals zu früh aufgeweckt wurden:
        var struct timeval slept; # so lang haben wir geschlafen
        # sozusagen sub_internal_time(end_time,start_time, slept);
        slept.tv_sec = end_time.tv_sec - start_time.tv_sec;
        if (end_time.tv_usec < start_time.tv_usec)
          { end_time.tv_usec += 1000000; slept.tv_sec -= 1; }
        slept.tv_usec = end_time.tv_usec - start_time.tv_usec;
        # Haben wir genug geschlafen?
        if ((slept.tv_sec > seconds)
            || ((slept.tv_sec == seconds) && (slept.tv_usec >= useconds))
           )
          break;
        # Wie lange müssen wir noch schlafen?
        seconds -= slept.tv_sec;
        if (useconds < slept.tv_usec) { seconds -= 1; useconds += 1000000; }
        useconds -= slept.tv_usec;
        #if !defined(HAVE_SELECT) && !defined(HAVE_USLEEP)
        if (seconds==0) break; # CPU-Zeit fressende Warteschleife vermeiden
        #endif
      }}
    end_system_call();
    value1 = NIL; mv_count=1; # 1 Wert NIL
  }
#endif
#ifdef TIME_WIN32
# (SYSTEM::%SLEEP delay-seconds delay-mseconds) wartet
# delay-seconds Sekunden und delay-mseconds Millisekunden.
# Argument delay-seconds muss ein Fixnum >=0, <=4290000 sein,
# Argument delay-useconds muss ein Fixnum >=0, <=1000 sein.
  { var uintL mseconds = posfixnum_to_L(popSTACK());
    var uintL seconds = posfixnum_to_L(popSTACK());
    begin_system_call();
    if (!msleep(1000*seconds+mseconds))
      { end_system_call();
        pushSTACK(S(sleep)); tast_break(); # evtl. Break-Schleife aufrufen
      }
    else
      { end_system_call(); }
    value1 = NIL; mv_count=1; # 1 Wert NIL
  }
#endif
#endif

LISPFUNN(time,0)
# (SYSTEM::%%TIME) liefert den bisherigen Time/Space-Verbrauch, ohne selbst
# Platz anzufordern (und damit eventuell selbst eine GC zu verursachen).
# 9 Werte:
#   Real-Time (Zeit seit Systemstart) in 2 Werten,
#   Run-Time (verbrauchte Zeit seit Systemstart) in 2 Werten,
#   GC-Time (durch GC verbrauchte Zeit seit Systemstart) in 2 Werten,
#   #ifdef TIME_AMIGAOS
#     jeweils in 50stel Sekunden,
#     jeweils (ldb (byte 16 16) time) und (ldb (byte 16 0) time).
#   #endif
#   #ifdef TIME_MSDOS
#     jeweils in 100stel Sekunden,
#     jeweils (ldb (byte 16 16) time) und (ldb (byte 16 0) time).
#   #endif
#   #if defined(TIME_UNIX_TIMES) || defined(TIME_RISCOS)
#     jeweils in CLK_TCK-stel Sekunden,
#     jeweils (ldb (byte 16 16) time) und (ldb (byte 16 0) time).
#   #endif
#   #ifdef TIME_UNIX
#     jeweils in Mikrosekunden,
#     jeweils ganze Sekunden und Mikrosekunden.
#   #endif
#   #ifdef TIME_WIN32
#     jeweils in Zehntel Mikrosekunden,
#     jeweils ganze Sekunden und Zehntel Mikrosekunden.
#   #endif
#   Space (seit Systemstart verbrauchter Platz, in Bytes)
#     in 2 Werten: (ldb (byte 24 24) Space), (ldb (byte 24 0) Space).
#   GC-Count (Anzahl der durchgeführten Garbage Collections).
  { var timescore tm;
    get_running_times(&tm); # Run-Time abfragen
    #ifdef TIME_1
      #define as_2_values(time)  \
        pushSTACK(fixnum(high16(time))); \
        pushSTACK(fixnum(low16(time)));
    #endif
    #ifdef TIME_2
      #ifdef TIME_UNIX
        #define as_2_values(time)  \
          pushSTACK(fixnum(time.tv_sec)); \
          pushSTACK(fixnum(time.tv_usec));
      #endif
      #ifdef TIME_WIN32
        #define as_2_values(time)  \
          { var uintL tv_sec;           \
            var uintL tv_usec;          \
            divu_6432_3232(time.dwHighDateTime,time.dwLowDateTime,ticks_per_second, tv_sec=, tv_usec=); \
            pushSTACK(fixnum(tv_sec));  \
            pushSTACK(fixnum(tv_usec)); \
          }
      #endif
    #endif
    as_2_values(tm.realtime); # erste zwei Werte: Real-Time
    as_2_values(tm.runtime); # nächste zwei Werte: Run-Time
    as_2_values(tm.gctime); # nächste zwei Werte: GC-Time
    # nächste zwei Werte: Space
    # tm.gcfreed = von der GC bisher wieder verfügbar gemachter Platz
    {var uintL used = used_space(); # momentan belegter Platz
     # beides addieren:
     #ifdef intQsize
     tm.gcfreed += used;
     #else
     if ((tm.gcfreed.lo += used) < used) { tm.gcfreed.hi += 1; }
     #endif
    }
    # Jetzt ist tm.gcfreed = bisher insgesamt verbrauchter Platz
    #if (oint_data_len<24)
      #error "Funktion SYS::%%TIME anpassen!"
    #endif
    # In 24-Bit-Stücke zerhacken:
    #ifdef intQsize
    pushSTACK(fixnum( (tm.gcfreed>>24) & (bit(24)-1) ));
    pushSTACK(fixnum( tm.gcfreed & (bit(24)-1) ));
    #else
    pushSTACK(fixnum( ((tm.gcfreed.hi << 8) + (tm.gcfreed.lo >> 24)) & (bit(24)-1) ));
    pushSTACK(fixnum( tm.gcfreed.lo & (bit(24)-1) ));
    #endif
    # letzter Wert: GC-Count
    pushSTACK(fixnum(tm.gccount));
    funcall(L(values),9); # 9 Werte produzieren
  }


