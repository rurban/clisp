/*
 * Time measuring functions for CLISP
 * Bruno Haible 1990-2002
 * Sam Steingold 1998-2003
 */

#include "lispbibl.c"
/* for high16, low16 in %%TIME,
   for divu in GET-UNIVERSAL-TIME,
   for mulu32 in GET-INTERNAL-RUN-TIME, GET-INTERNAL-REAL-TIME */
#include "arilev0.c"

/* Main types:
 Decoded time =
   seconds, minutes, hour, day, month, year,
   day-of-week, daylight-saving-time, time zone.
 Universal time =
   seconds since 1900-01-01 */
#ifdef TIME_AMIGAOS
  /* A small bug:
   - Wrap-around of internal_time_t after 2.7 years.
   Internal time =
     1/50 sec since session start */
#endif
#ifdef TIME_MSDOS
  /* A small bug:
   - Wrap-around of internal_time_t after 1.36 years.
   Internal time =
     1/100 sec since session start */
#endif
#if defined(TIME_UNIX_TIMES) || defined(TIME_RISCOS)
  /* Two small bugs:
   - Wrap-around of internal_time_t after many days.
   - LISP clock may be up to 1 sec behind the true clock.
   Internal time =
     1/CLK_TCK sec since session start */
#endif
#ifdef TIME_UNIX
  /* A small bug:
   - %%TIME works only for time differences <= 194 days.
   Internal time =
     microseconds since session start */
#endif
#ifdef TIME_WIN32
  /* A small bug:
   - %%TIME works only for time differences <= 19 days.
   Internal time =
     tenth of microseconds since session start */
#endif

/* ------------------------------------------------------------------------
 *                    Measuring time consumption */

/* Time consumption is measured with sub-second resolution,
 using internal_time_t. */

/* Variables: */
#ifdef TIME_AMIGAOS
  /* (The unit is 1/50 sec, a 32 bit counter therefore suffices for
   994d 4h 55m 45.92s, and no LISP session runs for 2.7 years.) */
#endif
#ifdef TIME_MSDOS
  /* (The unit is 1/100 sec, a 32 bit counter therefore suffices for
   497d 2h 27m 52.96s, and no LISP session runs for 1.3 years.) */
#endif
#if defined(TIME_UNIX_TIMES) || defined(TIME_RISCOS)
  /* (The unit is ca. 1/60 sec or 1/100 sec, a 32 bit counter therefore
   suffices for a long while.) */
#endif
#ifdef TIME_UNIX
  /* The unit is 1 µsec.
   (Independently whether the system clock is 60 Hz or 50 Hz or better.) */
#endif
#ifdef TIME_WIN32
  /* The unit is 0.1 µsec. */
#endif
  /* Running time: */
local internal_time_t realstart_time; /* real time at start of LISP session */
#ifndef HAVE_RUN_TIME
  /* Time that the LISP session consumes: */
    local uintL run_time = 0;       /* total runtime up to now */
    local uintL runstop_time;       /* if the stop watch is running: */
                                    /* the time of the last run-stop change */
    local bool run_flag = false; /* true if the stop watch is running */
#endif


#ifdef TIME_RELATIVE

/* Returns the current hi-res time.
 get_time() */
 #ifdef TIME_AMIGAOS
/* < uintL result: current value of the 50Hz counter */
global uintL get_time(void)
{
  var struct DateStamp datestamp;
  begin_system_call();
  DateStamp(&datestamp);        /* get current time and date */
  end_system_call();
  /* convert to ticks starting at 1978-01-01 00:00:00: */
  return ((uintL)(datestamp.ds_Days)*24*60 + (uintL)(datestamp.ds_Minute))
    *60*ticks_per_second + (uintL)(datestamp.ds_Tick);
}
 #endif
 #ifdef TIME_MSDOS
/* < uintL result: current value of the 100Hz counter */
global uintL get_time(void);
  #ifdef EMUNIX
global uintL get_time()
{
  var struct timeb real_time;
  begin_system_call();
  __ftime(&real_time);
  end_system_call();
  return (uintL)(real_time.time) * ticks_per_second
    + (uintL)((uintW)(real_time.millitm)/(1000/ticks_per_second));
}
  #endif
 #endif
 #ifdef TIME_UNIX_TIMES
/* < uintL result: current value of the CLK_TCK Hz counter */
local uintL get_time(void)
{
  var struct tms buffer;
  return (uintL)times(&buffer);
}
 #endif
 #ifdef TIME_RISCOS
/* < uintL result: current value of the CLK_TCK Hz counter */
global uintL get_time(void);
  #include <sys/os.h>
global uintL get_time()
{
  var int regs[10];
  var os_error * err;
  begin_system_call();
  err = os_swi(0x42,regs);
  if (err) { __seterr(err); OS_error(); }
  end_system_call();
  return (uintL)(regs[0]);
}
 #endif

#ifndef HAVE_RUN_TIME

/* Stops the swop-watch.
 run_time_stop(); */
global void run_time_stop (void)
{
  if (!run_flag) return;        /* stop-watch already stopped -> OK */
  /* Add the now consumed runtime to the total runtime: */
  run_time += get_time()-runstop_time;
  run_flag = false;             /* stop-watch now stopped */
}

/* Lets the stop-watch run again.
 run_time_restart(); */
global void run_time_restart (void)
{
  if (run_flag) return; # stop-watch already running -> OK
  runstop_time = get_time(); # save current time
  run_flag = true; # stop-watch now running
}

#endif

/* Returns the real time counter.
 get_real_time()
 < uintL result: Time since start of LISP session (in 1/50 sec or in 1/100 sec
                 or in 1/CLK_TCK sec) */
global uintL get_real_time (void)
{ return get_time()-realstart_time; }

#endif

#ifdef TIME_UNIX_TIMES

/* Returns the run time counter.
 get_run_time(&runtime);
 < internal_time_t runtime: consumed run time since session start (in ticks)
 < uintL result: same as for get_time() */
global uintL get_run_time (internal_time_t* runtime)
{
  var struct tms tms;
  var uintL now_time;
  begin_system_call();
  now_time = times(&tms);
  end_system_call();
  *runtime = tms.tms_utime + tms.tms_stime; /* user time + system time */
  return now_time;                          /* cf. get_time() */
}

#endif

#ifdef TIME_UNIX

/* Returns the real time counter.
 get_real_time()
 < internal_time_t* result: absolute time */
global void get_real_time (internal_time_t* it)
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

/* Returns the run time counter.
 get_run_time(&runtime);
 < internal_time_t runtime: consumed run time since session start (in ticks) */
global void get_run_time (internal_time_t* runtime)
{
 #if defined(HAVE_GETRUSAGE)
  var struct rusage rusage;
  begin_system_call();
  if (!( getrusage(RUSAGE_SELF,&rusage) ==0)) { OS_error(); }
  end_system_call();
  /* runtime = user time + system time */
  add_internal_time(rusage.ru_utime,rusage.ru_stime, *runtime);
 #elif defined(HAVE_SYS_TIMES_H)
  var uintL used_time;     /* consumed time, measured in 1/HZ seconds */
  var struct tms tms;
  begin_system_call();
  if (times(&tms) == (clock_t)(-1))
    used_time = 0;             /* times() failed -> used_time unknown */
  else
    used_time = tms.tms_utime + tms.tms_stime; /* user time + system time */
  end_system_call();
  /* Convert to seconds and microseconds: (use HZ or CLK_TCK ??) */
  runtime->tv_sec = floor(used_time,HZ);
  runtime->tv_usec = (used_time % HZ) * floor(2*1000000+HZ,2*HZ);
 #endif
}

#endif

#ifdef TIME_WIN32

/* Returns the real time counter.
 get_real_time()
 < internal_time_t* ergebnis: absolute time */
global void get_real_time (internal_time_t* it)
{ GetSystemTimeAsFileTime(it); }

/* Returns the run time counter.
 get_run_time(&runtime);
 < internal_time_t runtime: consumed run time since session start (in ticks) */
global void get_run_time (internal_time_t* runtime)
{
  var FILETIME creation_time;
  var FILETIME exit_time;
  var FILETIME kernel_time;
  var FILETIME user_time;
  begin_system_call();
  if (GetProcessTimes(GetCurrentProcess(),&creation_time,&exit_time,
                      &kernel_time,&user_time)) {
    end_system_call();
    /* runtime = User time + Kernel time */
    add_internal_time(user_time,kernel_time, *runtime);
  } else {
    if (!(GetLastError()==ERROR_CALL_NOT_IMPLEMENTED)) { OS_error(); }
    /* GetProcessTimes() is not implemented on Win95. Use get_real_time()
     instead. This is only a crude approximation, I know.
     (We keep HAVE_RUN_TIME defined, so that Win95 users will notice
     that "Run time" and "Real time" are always the same and draw their
     conclusions from it.) */
    end_system_call();
    var internal_time_t real_time;
    get_real_time(&real_time);
    sub_internal_time(real_time,realstart_time, *runtime);
  }
}

#endif

/* Returns the whole set of run time counters.
 get_running_times(&timescore);
 < timescore.runtime:  consumed run time since start of session (in ticks)
 < timescore.realtime: real time since start of session (in ticks)
 < timescore.gctime:   GC time since start of session (in ticks)
 < timescore.gccount:  number of GCs since start of session
 < timescore.gcfreed:  number of reclaimed bytes since start of session */
global void get_running_times (timescore_t* tm)
{
 #ifndef HAVE_RUN_TIME
  var uintL time = get_time();
  tm->realtime = time - realstart_time;
  tm->runtime = (run_flag ?
                 time - runstop_time + run_time : /* stop-watch still running*/
                 run_time);  /* stop-watched stopped */
 #endif
 #ifdef TIME_UNIX
   /* Get real time: */
   var internal_time_t real_time;
   get_real_time(&real_time);
   tm->realtime.tv_sec = real_time.tv_sec - realstart_time.tv_sec;
   tm->realtime.tv_usec = real_time.tv_usec;
   /* Get run time: */
   get_run_time(&tm->runtime);
 #endif
 #ifdef TIME_UNIX_TIMES
   /* Get run time and real time both together: (vgl. get_real_time()) */
   tm->realtime = get_run_time(&tm->runtime) - realstart_time;
 #endif
 #ifdef TIME_WIN32
   /* Get real time: */
   var internal_time_t real_time;
   get_real_time(&real_time);
   sub_internal_time(real_time,realstart_time, tm->realtime);
   /* Get run time: */
   get_run_time(&tm->runtime);
 #endif
   tm->gctime = gc_time;
   tm->gccount = gc_count;
   tm->gcfreed = gc_space;
}

#ifdef TIME_2
/* Converts an internal_time_t to a Lisp integer.
 internal_time_to_I(&it) */
local object internal_time_to_I (const internal_time_t* tp)
{
 #ifdef TIME_UNIX
  /* Convert to microseconds: tp->tv_sec * ticks_per_second + tp->tv_usec */
  #ifdef intQsize
  return UQ_to_I((uintQ)(tp->tv_sec)*ticks_per_second + (uintQ)(tp->tv_usec));
  #else
  var uintL hi;
  var uintL lo;
  mulu32(tp->tv_sec,ticks_per_second, hi=,lo=);
  if ((lo += tp->tv_usec) < tp->tv_usec) { hi += 1; }
  return L2_to_I(hi,lo);
  #endif
 #endif
 #ifdef TIME_WIN32
  return L2_to_I(tp->dwHighDateTime,tp->dwLowDateTime);
 #endif
}
#endif

LISPFUNNR(get_internal_real_time,0)
{ /* (GET-INTERNAL-REAL-TIME), CLTL p. 446 */
 #ifdef TIME_1
  VALUES1(UL_to_I(get_real_time())); /* get real time since start of session */
 #endif
 #ifdef TIME_2
  var internal_time_t tp;       /* absolute real time */
  get_real_time(&tp);
  VALUES1(internal_time_to_I(&tp)); /* convert to integer */
 #endif
}

LISPFUNNR(get_internal_run_time,0)
{ /* (GET-INTERNAL-RUN-TIME), CLTL p. 446 */
  var timescore_t tm;
  get_running_times(&tm); /* get run time since start of session */
 #ifdef TIME_1
  VALUES1(UL_to_I(tm.runtime)); /* convert to integer */
 #endif
 #ifdef TIME_2
  VALUES1(internal_time_to_I(&tm.runtime)); /* convert to integer */
 #endif
}

/* ------------------------------------------------------------------------
 *                    Converting the system time format */

#if defined(MSDOS)
/* UP: Wandelt das DOS-Zeitformat in Decoded-Time um.
 convert_timedate(time,date,&timepoint)
 > uintW time: Uhrzeit
         Als Word: Bits 15..11: Stunde in {0,...,23},
                   Bits 10..5:  Minute in {0,...,59},
                   Bits 4..0:   Sekunde/2 in {0,...,29}.
 > uintW date: Datum
         Als Word: Bits 15..9: Jahr-1980 in {0,...,119},
                   Bits 8..5:  Monat in {1,...,12},
                   Bits 4..0:  Tag in {1,...,31}.
 < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums */
global void convert_timedate (uintW time, uintW date,
                              decoded_time_t* timepoint)
{
  timepoint->Sekunden = fixnum( (time & (bit(5) - 1)) << 1 );
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
/* UP: Wandelt das Amiga-Zeitformat in Decoded-Time um.
 convert_time(&datestamp,&timepoint);
 > struct DateStamp datestamp: Uhrzeit
          datestamp.ds_Days   : Anzahl Tage seit 1.1.1978
          datestamp.ds_Minute : Anzahl Minuten seit 00:00 des Tages
          datestamp.ds_Tick   : Anzahl Ticks seit Beginn der Minute
 < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums  */
global void convert_time (const struct DateStamp * datestamp,
                          decoded_time_t* timepoint)
{
  /* Methode:
    ds_Tick durch ticks_per_second dividieren, liefert Sekunden.
    ds_Minute durch 60 dividierem liefert Stunden und (als Rest) Minuten.
    ds_Days in Tag, Monat, Jahr umrechnen:
      d := ds_Days - 790; # Tage seit 1.3.1980 (Schaltjahr)
      y := floor((4*d+3)/1461); # März-Jahre ab 1.3.1980
      d := d - floor(y*1461/4); # Tage ab letztem März-Jahres-Anfang
      (Diese Rechnung geht gut, solange jedes vierte Jahr ein Schaltjahr
       ist, d.h. bis zum Jahr 2099.)
      m := floor((5*d+2)/153); # Monat ab letztem März
      d := d - floor((153*m+2)/5); # Tag ab letztem Monatsanfang
      m := m+2; if (m>=12) then { m:=m-12; y:=y+1; } # auf Jahre umrechnen
      Tag d+1, Monat m+1, Jahr 1980+y. */
  {
    var uintL sec;
    divu_3216_1616(datestamp->ds_Tick,ticks_per_second,sec=,);
    timepoint->Sekunden = fixnum(sec);
  }
  {
    var uintL std;
    var uintL min;
    divu_3216_1616(datestamp->ds_Minute,60,std=,min=);
    timepoint->Minuten = fixnum(min);
    timepoint->Stunden = fixnum(std);
  }
  {
    var uintL y;
    var uintW m;
    var uintW d;
    divu_3216_1616(4*(datestamp->ds_Days - 424),1461,y=,d=); /* y = März-Jahre ab 1.1.1979 */
    d = floor(d,4);        /* Tage ab dem letzten März-Jahres-Anfang */
    divu_1616_1616(5*d+2,153,m=,d=); /* m = Monat ab letztem März */
    d = floor(d,5);                  /* Tag ab letztem Monatsanfang */
    /* m=0..9 -> Monat März..Dezember des Jahres 1979+y,
       m=10..11 -> Monat Januar..Februar des Jahres 1980+y. */
    if (m<10) { m += 12; y -= 1; } /* auf Jahre umrechnen */
    timepoint->Tag = fixnum(1+(uintL)d);
    timepoint->Monat = fixnum(-9+(uintL)m);
    timepoint->Jahr = fixnum(1980+y);
  }
}
#endif
#if defined(UNIX) || defined(MSDOS) || defined(RISCOS)
/* UP: Wandelt das System-Zeitformat in Decoded-Time um.
 convert_time(&time,&timepoint);
 > time_t time: in system time format
 < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums */
global void convert_time (const time_t* time, decoded_time_t* timepoint)
{
  begin_system_call();
  var struct tm * tm = localtime(time); /* decodieren */
  /* (Das Zeitformat des Systems muss auch das System auseinandernehmen.) */
  end_system_call();
  if (tm) {
    /* localtime war erfolgreich */
    timepoint->Sekunden = fixnum(tm->tm_sec);
    timepoint->Minuten  = fixnum(tm->tm_min);
    timepoint->Stunden  = fixnum(tm->tm_hour);
    timepoint->Tag      = fixnum(tm->tm_mday);
    timepoint->Monat    = fixnum(1+tm->tm_mon);
    timepoint->Jahr     = fixnum(1900+tm->tm_year);
  } else {
    /* gescheitert -> verwende 1.1.1900, 00:00:00 als Default */
    timepoint->Sekunden = Fixnum_0;
    timepoint->Minuten  = Fixnum_0;
    timepoint->Stunden  = Fixnum_0;
    timepoint->Tag      = Fixnum_1;
    timepoint->Monat    = Fixnum_1;
    timepoint->Jahr     = fixnum(1900);
  }
}
#endif
#ifdef WIN32_NATIVE
/* UP: Wandelt das System-Zeitformat in Decoded-Time um.
 convert_time(&time,&timepoint);
 > FILETIME time: in system time format
 < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums */
global void convert_time (const FILETIME* time, decoded_time_t* timepoint)
{
  var FILETIME ltime;
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

/* Converts a decoded time to universal time.
 encode_universal_time(&timepoint)
 > decoded_time_t timepoint: decoded time
 < result: universal time
 can trigger GC */
local object encode_universal_time (const decoded_time_t* timepoint)
{ /* (ENCODE-UNIVERSAL-TIME Sekunden Minuten Stunden Tag Monat Jahr): */
  pushSTACK(timepoint->Sekunden);
  pushSTACK(timepoint->Minuten);
  pushSTACK(timepoint->Stunden);
  pushSTACK(timepoint->Tag);
  pushSTACK(timepoint->Monat);
  pushSTACK(timepoint->Jahr);
  funcall(S(encode_universal_time),6);
  return value1;
}

#ifdef AMIGAOS
/* UP: convert the system (Amiga) time format into lisp universal time.
 convert_time_to_universal(&datestamp)
 > struct DateStamp datestamp: Uhrzeit
          datestamp.ds_Days   : Anzahl Tage seit 1.1.1978
          datestamp.ds_Minute : Anzahl Minuten seit 00:00 des Tages
          datestamp.ds_Tick   : Anzahl Ticks seit Beginn der Minute
 < result: integer denoting the seconds since 1900-01-01 00:00 GMT
 can trigger GC */
global object convert_time_to_universal (const struct DateStamp * datestamp)
{
  var decoded_time_t timepoint;
  convert_time(datestamp,&timepoint);
  /* Have to go through ENCODE-UNIVERSAL-TIME because a DateStamp is an
   encoded time format, and because we must take the timezone into account.*/
  return encode_universal_time(&timepoint);
}
#endif
#if defined(UNIX) || defined(MSDOS) || defined(RISCOS)
/* UP: convert the system time format into lisp universal time.
 convert_time_to_universal(&time)
 > time_t time: in system time format
 < result: integer denoting the seconds since 1900-01-01 00:00 GMT
 can trigger GC */
global object convert_time_to_universal (const time_t* time)
{
 #if defined(MSDOS) || defined(RISCOS)
  var decoded_time_t timepoint;
  convert_time(time,&timepoint);
  /* Have to go through ENCODE-UNIVERSAL-TIME because we must take the
     timezone into account. */
  return encode_universal_time(&timepoint);
 #endif
 #if defined(UNIX)
  /* Since we get the timezone from the OS (sys::defaul-time-zone),
     we can assume that the OS's timezone and CLISP's timezone agree. */
  return UL_to_I(UNIX_LISP_TIME_DIFF + (uintL)(*time));
 #endif
}
#endif

#if defined(WIN32_NATIVE)
/* UP: convert the system (win32) time format into lisp universal time.
 convert_time_to_universal(&time)
 > FILETIME time: in system time format
 < result: integer denoting the seconds since 1900-01-01 00:00 GMT
 can trigger GC */
global object convert_time_to_universal (const FILETIME* time) {
  /* Since we get the timezone from the OS (sys::defaul-time-zone),
     we can assume that the OS's timezone and CLISP's timezone agree. */
  var internal_time_t offset = /* difference between 1.1.1601 and 1.1.1900 */
   #ifdef HAVE_LONGLONG
    { (ULONG)((ULONGLONG)109207 * (ULONGLONG)86400
              * (ULONGLONG)ticks_per_second),
      (ULONG)(((ULONGLONG)109207 * (ULONGLONG)86400
               * (ULONGLONG)ticks_per_second) >> 32) };
   #else
    { 0xFDE04000, 0x14F373B };
   #endif
  var internal_time_t internal_real_time;
  var uintL real_time;
  sub_internal_time(*time,offset,internal_real_time);
  divu_6432_3232(internal_real_time.dwHighDateTime,
                 internal_real_time.dwLowDateTime,
                 ticks_per_second,
                 real_time=,);
  return UL_to_I(real_time);
}
#endif

/* ------------------------------------------------------------------------
 *                        Measuring wall clock time */

/* Wall clock time is measured with second resolution only. */

/*Returns the wall clock time in seconds (since session start or 1900-01-01).*/
local uintL real_time_sec (void)
{
 #ifdef TIME_1
  var uintL real_time = get_real_time();
  /* real_time := floor(real_time,ticks_per_second) : */
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
  var uintL real_time;     /* seconds */
  var internal_time_t it;
  get_real_time(&it);
  real_time = UNIX_LISP_TIME_DIFF + it.tv_sec;
  #endif
  #ifdef TIME_WIN32
  var internal_time_t offset = /* difference between 1.1.1601 and 1.1.1900 */
   #ifdef HAVE_LONGLONG
    { (ULONG)((ULONGLONG)109207 *
              (ULONGLONG)86400 * (ULONGLONG)ticks_per_second),
      (ULONG)(((ULONGLONG)109207 *
               (ULONGLONG)86400 * (ULONGLONG)ticks_per_second) >> 32)};
   #else
    { 0xFDE04000, 0x14F373B };
   #endif
  var internal_time_t internal_real_time;
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

#ifdef TIME_RELATIVE

/* Uhrzeit und Datum beim LISP-Start: */
local decoded_time_t realstart_datetime;

/* Sets the time of the start of the session.
 set_start_time(&timepoint);
 > timepoint: Zeit beim LISP-System-Start
 >   timepoint.Sekunden in {0,...,59},
 >   timepoint.Minuten in {0,...,59},
 >   timepoint.Stunden in {0,...,23},
 >   timepoint.Tag in {1,...,31},
 >   timepoint.Monat in {1,...,12},
 >   timepoint.Jahr in {1980,...,2999},
 >   jeweils als Fixnums.
 can trigger GC */
local void set_start_time (const decoded_time_t* timepoint)
{ /* Start-Zeit merken: */
  realstart_datetime = *timepoint;
  /* und, wenn möglich, gleich in Universal Time umwandeln: */
  if (boundp(Symbol_function(S(encode_universal_time)))) {
    /* Ist ENCODE-UNIVERSAL-TIME definiert -> sofort in UT umwandeln: */
    O(start_UT) = encode_universal_time(timepoint);
  }
}

/* Returns the time of the start of the session.
 get_start_time()
 can trigger GC */
local uintL get_start_time (void)
{
  var object start_time = O(start_UT);
  if (nullp(start_time)) {  /* Start-Universal-Time noch NIL ? */
    /* nein -> schon berechnet.
       ja -> jetzt erst berechnen: */
    start_time = O(start_UT) = encode_universal_time(&realstart_datetime);
  }
  return I_to_UL(start_time);
}

#endif

/* Returns the wall clock time in seconds (since 1900-01-01).
 can trigger GC */
local uintL universal_time_sec (void);
#ifdef TIME_RELATIVE
local uintL universal_time_sec()
{ return get_start_time() + real_time_sec(); }
#endif
#ifdef TIME_ABSOLUTE
#define universal_time_sec() real_time_sec()
#endif

LISPFUNNR(get_universal_time,0)
{ /* (get-universal-time), CLTL p. 445 */
  VALUES1(UL_to_I(universal_time_sec()));
}

/* UP: Initialisiert die Zeitvariablen beim LISP-System-Start.
 init_time(); */
global void init_time (void)
{ /* Es ist noch keine GC dagewesen -> hat auch noch keine Zeit verbraucht.
     gc_count=0;
     gc_time=0;
     gc_space=0; */
 #ifdef TIME_RELATIVE
  realstart_time = get_time(); /* Zeitzähler jetzt, beim Systemstart */
 #endif
 #ifndef HAVE_RUN_TIME
  /* run_time = 0; -- Noch keine Run-Time verbraucht, */
  run_flag = false;         /* denn System läuft noch nicht. */
  run_time_restart();       /* Run-Time-Stoppuhr loslaufen lassen */
 #endif
 #if defined(TIME_UNIX) || defined(TIME_WIN32)
  get_real_time(&realstart_time); /* Zeitzähler jetzt, beim Systemstart */
 #endif
 #ifdef TIME_RELATIVE
  { /* Start-Zeit holen und merken: */
    var decoded_time_t timepoint;
   #ifdef AMIGAOS
    {
      var struct DateStamp datestamp; /* aktuelle Uhrzeit */
      DateStamp(&datestamp);
      convert_time(&datestamp,&timepoint); /* in Decoded-Time umwandeln */
    }
   #endif
   #ifdef EMUNIX
    {
      var struct timeb real_time;
      begin_system_call();
      __ftime(&real_time);  /* aktuelle Uhrzeit */
      end_system_call();
      convert_time(&real_time.time,&timepoint); /* in Decoded-Time umwandeln */
    }
   #endif
   #if defined(UNIX) || defined(RISCOS) /* TIME_UNIX_TIMES || TIME_RISCOS */
    {
      var time_t real_time;
      begin_system_call();
      time(&real_time);     /* aktuelle Uhrzeit */
      end_system_call();
      convert_time(&real_time,&timepoint); /* in Decoded-Time umwandeln */
    }
   #endif
    set_start_time(&timepoint); /* Start-Zeit merken */
  }
 #endif
}

/* ------------------------------------------------------------------------
 *                        Other time related functions */

#if defined(UNIX) || defined(WIN32)
LISPFUN(default_time_zone,seclass_default,0,1,norest,nokey,0,NIL)
{ /* (sys::default-time-zone) liefert die aktuelle Zeitzone.
 (sys::default-time-zone UTstunde) liefert die aktuelle Zeitzone zu einem
 bestimmten Zeitpunkt.
 1. Wert: Zeitzone mit Sommerzeit-Berücksichtigung.
 2. Wert: Sommerzeit-p.
 Da die Zeitzone oft per TZ-Environment-Variable einstellbar ist, wird
 sie häufig außerhalb des Kernels verwaltet. Man hat nur per localtime()
 und gmtime() Zugriff auf sie.
 Methode:
   Zeitzone = (gmtime(t) - localtime(t))/3600.
   Sommerzeit-p wird dem Ergebnis von localtime(t) entnommen. */
  var object arg = popSTACK();
  var time_t now;
  if (integerp(arg)) {
    /* bestimmter Zeitpunkt
       Annahme: time_t ist die Anzahl der Sekunden seit 1.1.1970. ?? */
   #ifdef WIN32
    const uintL time_max = 1210131; /* Win32 crashes for values greater than that */
   #else
    const uintL time_max = 1314888; /* 1.1.2050, quite arbitrary */
   #endif
    if (posfixnump(arg)
        && (posfixnum_to_L(arg) >= 613608)   /* arg >= 1.1.1970 */
        && (posfixnum_to_L(arg) <= time_max)) { /* arg < time_max */
      now = (posfixnum_to_L(arg) - 613608) * 3600;
    } else if (R_minusp(arg) ||
               (posfixnump(arg) && (posfixnum_to_L(arg) < 613608))) {
      now = 0;                /* < 1.1.1970 -> treat like 1.1.1970 */
    } else {
      now = (uintL)(time_max - 613608) * 3600; /* > max -> treat like max */
    }
  } else { /* jetzt */
    begin_system_call(); time(&now); end_system_call();
  }
  var struct tm now_local;
  var struct tm now_gm;
  begin_system_call();
  now_local = *(localtime(&now));
  now_gm = *(gmtime(&now));
  end_system_call();
  /* secondswest = mktime(now_gm) - mktime(now_local); wäre schön.
     mktime() ist allerdings nicht weit verbreitet. Unter SunOS4 müsste man
     timegm() nehmen. Daher tun wir's selber: */
  var sintL dayswest = /* Tage-Differenz, kann als 0,1,-1 angenommen werden */
    (now_gm.tm_year < now_local.tm_year ? -1 :
     now_gm.tm_year > now_local.tm_year ? 1 :
     (now_gm.tm_mon < now_local.tm_mon ? -1 :
      now_gm.tm_mon > now_local.tm_mon ? 1 :
      (now_gm.tm_mday < now_local.tm_mday ? -1 :
       now_gm.tm_mday > now_local.tm_mday ? 1 :
       0)));
  var sintL hourswest = 24*dayswest
    + (sintL)(now_gm.tm_hour - now_local.tm_hour);
  var sintL minuteswest = 60*hourswest
    + (sintL)(now_gm.tm_min - now_local.tm_min);
  var sintL secondswest = 60*minuteswest
    + (sintL)(now_gm.tm_sec - now_local.tm_sec);
  /* Zeitzone in Stunden = (Zeitzone in Sekunden / 3600) : */
  pushSTACK(L_to_I(secondswest));
  pushSTACK(fixnum(3600));
  funcall(L(durch),2);
  /* Sommerzeit-p entnehmen:
     tm_isdst < 0 bedeutet "unbekannt"; wir nehmen an, keine Sommerzeit. */
  VALUES2(value1, now_local.tm_isdst > 0 ? T : NIL);
}
#endif  /* UNIX || WIN32 */

#ifdef SLEEP_1
LISPFUNN(sleep,1)
#if defined(TIME_MSDOS) || defined(RISCOS)
{ /* (SYSTEM::%SLEEP delay) wartet delay/200 bzw. delay/100 Sekunden.
 Argument delay muss ein Integer >=0, <2^32 (TIME_MSDOS: sogar <2^31) sein. */
  var uintL delay = I_to_UL(popSTACK()); /* Pausenlänge */
 #ifdef EMUNIX
  if (true) {
    /* Unter OS/2 (Multitasking!) nicht CPU-Zeit verbraten!
       select erlaubt eine wunderschöne Implementation von usleep(): */
    loop {
      var uintL start_time = get_real_time();
      {
        var struct timeval timeout; /* Zeitintervall */
        divu_3216_3216(delay,ticks_per_second, timeout.tv_sec =, timeout.tv_usec = 1000000/ticks_per_second * (uintL) );
        begin_system_call();
        var int ergebnis = select(FD_SETSIZE,NULL,NULL,NULL,&timeout);
        if ((ergebnis<0) && !(errno==EINTR)) { OS_error(); }
        end_system_call();
      }
      interruptp( { pushSTACK(S(sleep)); tast_break(); } ); /* evtl. Break-Schleife aufrufen */
      var uintL end_time = get_real_time();
      var uintL slept = end_time - start_time; /* so lang haben wir geschlafen */
      /* Haben wir genug geschlafen? */
      if (slept >= delay)
        break;
      /* Wie lange müssen wir noch schlafen? */
      delay -= slept;
    }
  } else
 #endif
  {
    var uintL endtime = get_real_time() + delay; /* zur momentanen Real-Time addieren, */
    /* ergibt Zeit, bis zu der zu warten ist.
       warten, bis die Real-Time bei endtime angelangt ist:
       (Attention: the MSDOS clock always advances 5 or 6 ticks at a time!) */
    do {} while ((sintL)(get_real_time()-endtime) < 0);
  }
  VALUES1(NIL);               /* 1 Wert NIL */
}
#endif
#ifdef TIME_AMIGAOS
{ /* (SYSTEM::%SLEEP delay) wartet delay/50 Sekunden.
 Argument delay muss ein Integer >=0, <2^32 sein. */
  var uintL delay = I_to_UL(popSTACK()); /* Pausenlänge */
  if (delay>0) { begin_system_call(); Delay(delay); end_system_call(); }
  VALUES1(NIL);               /* 1 Wert NIL */
}
#endif
#endif
#ifdef SLEEP_2
#ifdef TIME_UNIX_TIMES
/* Ein sehr unvollkommener Ersatz für die gettimeofday-Funktion.
 Taugt nur für die Messung von Zeitdifferenzen! */
local int gettimeofday (struct timeval * tp, void* tzp)
{
  if (!(tp==NULL)) {
    var uintL realtime = get_real_time();
    /* in Sekunden und Mikrosekunden umwandeln: */
    tp->tv_sec = floor(realtime,ticks_per_second);
    tp->tv_usec = (realtime % ticks_per_second)
      * floor(2*1000000+ticks_per_second,2*ticks_per_second);
  }
  return 0;
}
#endif
LISPFUNN(sleep,2)
#if defined(TIME_UNIX) || defined(TIME_UNIX_TIMES)
{ /* (SYSTEM::%SLEEP delay-seconds delay-useconds) wartet
 delay-seconds Sekunden und delay-useconds Mikrosekunden.
 Argument delay-seconds muss ein Fixnum >=0, <=16700000 sein,
 Argument delay-useconds muss ein Fixnum >=0, <=1000000 sein. */
  var uintL useconds = posfixnum_to_L(popSTACK());
  var uintL seconds = posfixnum_to_L(popSTACK());
  begin_system_call();
  loop {
    var struct timeval start_time;
    var struct timeval end_time;
    if (!( gettimeofday(&start_time,NULL) ==0)) { OS_error(); }
   #ifdef HAVE_SELECT
    { /* select erlaubt eine wunderschöne Implementation von usleep(): */
      var struct timeval timeout; /* Zeitintervall */
      timeout.tv_sec = seconds; timeout.tv_usec = useconds;
      var int ergebnis;
      ergebnis = select(FD_SETSIZE,NULL,NULL,NULL,&timeout);
      if ((ergebnis<0) && !(errno==EINTR)) { OS_error(); }
    }
   #else
    if (seconds>0) { sleep(seconds); }
    #ifdef HAVE_USLEEP
    if (useconds>0) { usleep(useconds); }
    #endif
   #endif
    interruptp({
      end_system_call();
      pushSTACK(S(sleep)); tast_break(); /* evtl. Break-Schleife aufrufen */
      begin_system_call();
    });
    if (!( gettimeofday(&end_time,NULL) ==0)) { OS_error(); }
    /* Überprüfen, ob wir genügend lang geschlafen haben, oder ob
       wir wegen eines Signals zu früh aufgeweckt wurden: */
    var struct timeval slept; /* so lang haben wir geschlafen */
    /* sozusagen sub_internal_time(end_time,start_time, slept); */
    slept.tv_sec = end_time.tv_sec - start_time.tv_sec;
    if (end_time.tv_usec < start_time.tv_usec) {
      end_time.tv_usec += 1000000; slept.tv_sec -= 1;
    }
    slept.tv_usec = end_time.tv_usec - start_time.tv_usec;
    /* Haben wir genug geschlafen? */
    if ((slept.tv_sec > seconds)
        || ((slept.tv_sec == seconds) && (slept.tv_usec >= useconds)))
      break;
    /* Wie lange müssen wir noch schlafen? */
    seconds -= slept.tv_sec;
    if (useconds < slept.tv_usec) { seconds -= 1; useconds += 1000000; }
    useconds -= slept.tv_usec;
   #if !defined(HAVE_SELECT) && !defined(HAVE_USLEEP)
    if (seconds==0) break; /* CPU-Zeit fressende Warteschleife vermeiden */
   #endif
  }
  end_system_call();
  VALUES1(NIL);
}
#endif
#ifdef TIME_WIN32
{ /* (SYSTEM::%SLEEP delay-seconds delay-mseconds) wartet
 delay-seconds Sekunden und delay-mseconds Millisekunden.
 Argument delay-seconds muss ein Fixnum >=0, <=4290000 sein,
 Argument delay-useconds muss ein Fixnum >=0, <=1000 sein. */
  var uintL mseconds = posfixnum_to_L(popSTACK());
  var uintL seconds = posfixnum_to_L(popSTACK());
  begin_system_call();
  if (!msleep(1000*seconds+mseconds)) {
    end_system_call();
    pushSTACK(S(sleep)); tast_break(); /* evtl. Break-Schleife aufrufen */
  } else {
    end_system_call();
  }
  VALUES1(NIL);
}
#endif
#endif

LISPFUNNR(time,0)
{ /* (SYSTEM::%%TIME) liefert den bisherigen Time/Space-Verbrauch, ohne selbst
 Platz anzufordern (und damit eventuell selbst eine GC zu verursachen).
 9 Werte:
   Real-Time (Zeit seit Systemstart) in 2 Werten,
   Run-Time (verbrauchte Zeit seit Systemstart) in 2 Werten,
   GC-Time (durch GC verbrauchte Zeit seit Systemstart) in 2 Werten,
   #ifdef TIME_AMIGAOS
     jeweils in 50stel Sekunden,
     jeweils (ldb (byte 16 16) time) und (ldb (byte 16 0) time).
   #endif
   #ifdef TIME_MSDOS
     jeweils in 100stel Sekunden,
     jeweils (ldb (byte 16 16) time) und (ldb (byte 16 0) time).
   #endif
   #if defined(TIME_UNIX_TIMES) || defined(TIME_RISCOS)
     jeweils in CLK_TCK-stel Sekunden,
     jeweils (ldb (byte 16 16) time) und (ldb (byte 16 0) time).
   #endif
   #ifdef TIME_UNIX
     jeweils in Mikrosekunden,
     jeweils ganze Sekunden und Mikrosekunden.
   #endif
   #ifdef TIME_WIN32
     jeweils in Zehntel Mikrosekunden,
     jeweils ganze Sekunden und Zehntel Mikrosekunden.
   #endif
   Space (seit Systemstart verbrauchter Platz, in Bytes)
     in 2 Werten: (ldb (byte 24 24) Space), (ldb (byte 24 0) Space).
   GC-Count (Anzahl der durchgeführten Garbage Collections). */
  var timescore_t tm;
  get_running_times(&tm);     /* Run-Time abfragen */
 #ifdef TIME_1
  #define as_2_values(time)                \
    pushSTACK(fixnum(high16(time)));       \
    pushSTACK(fixnum(low16(time)));
  #endif
 #ifdef TIME_2
  #ifdef TIME_UNIX
   #define as_2_values(time)                 \
     pushSTACK(fixnum(time.tv_sec));         \
     pushSTACK(fixnum(time.tv_usec));
  #endif
  #ifdef TIME_WIN32
    #define as_2_values(time)               \
      { var uintL tv_sec;                       \
        var uintL tv_usec;                                              \
        divu_6432_3232(time.dwHighDateTime,time.dwLowDateTime,ticks_per_second, tv_sec=, tv_usec=); \
        pushSTACK(fixnum(tv_sec));                                      \
        pushSTACK(fixnum(tv_usec));                                     \
      }
  #endif
 #endif
  as_2_values(tm.realtime);   /* erste zwei Werte: Real-Time */
  as_2_values(tm.runtime);    /* nächste zwei Werte: Run-Time */
  as_2_values(tm.gctime);     /* nächste zwei Werte: GC-Time */
  /* nächste zwei Werte: Space
     tm.gcfreed = von der GC bisher wieder verfügbar gemachter Platz */
  {
    var uintL used = used_space(); /* momentan belegter Platz */
    /* beides addieren: */
   #ifdef intQsize
    tm.gcfreed += used;
   #else
    if ((tm.gcfreed.lo += used) < used) { tm.gcfreed.hi += 1; }
   #endif
  }
  /* Jetzt ist tm.gcfreed = bisher insgesamt verbrauchter Platz */
  #if (oint_data_len<24)
  #error "Funktion SYS::%%TIME anpassen!"
  #endif
  /* In 24-Bit-Stücke zerhacken: */
 #ifdef intQsize
  pushSTACK(fixnum( (tm.gcfreed>>24) & (bit(24)-1) ));
  pushSTACK(fixnum( tm.gcfreed & (bit(24)-1) ));
 #else
  pushSTACK(fixnum( ((tm.gcfreed.hi << 8) + (tm.gcfreed.lo >> 24)) & (bit(24)-1) ));
  pushSTACK(fixnum( tm.gcfreed.lo & (bit(24)-1) ));
 #endif
  /* last value: GC count */
  pushSTACK(fixnum(tm.gccount));
  funcall(L(values),9);       /* return 9 values */
}

/* (SYS::DELTA4 n1 n2 o1 o2 shift)
 compute the difference between [n1 n2] and [o1 o2]
 as positional numbers with SHIFT digits, i.e.,
 (- (+ (ash n1 shift) n2) (+ (ash o1 shift) o2))
 the difference must be positive
 all numbers must be fixnums; the result is (UNSIGNED-BYTE 64) */
LISPFUNNF(delta4,5) {
  if (!posfixnump(STACK_0)) fehler_posfixnum(STACK_0);
  var uintL shift = posfixnum_to_L(STACK_0);
  if (!posfixnump(STACK_1)) fehler_posfixnum(STACK_1);
  var uintL o2 = posfixnum_to_L(STACK_1);
  if (!posfixnump(STACK_2)) fehler_posfixnum(STACK_2);
  var uintL o1 = posfixnum_to_L(STACK_2);
  if (!posfixnump(STACK_3)) fehler_posfixnum(STACK_3);
  var uintL n2 = posfixnum_to_L(STACK_3);
  if (!posfixnump(STACK_4)) fehler_posfixnum(STACK_4);
  var uintL n1 = posfixnum_to_L(STACK_4);
  if ((o1 > n1) /* use the arguments on the stack for error reporting */
      || ((o1 == n1) && (o2 > n2))) {
    skipSTACK(1); pushSTACK(S(delta4));
    fehler(arithmetic_error,"~: negative difference: [~ ~] > [~ ~]");
  }
  var uintL del = n1 - o1;
  if (shift + I_integer_length(fixnum(del)) > 64) {
    pushSTACK(STACK_0); pushSTACK(S(ash));
    fehler(arithmetic_error,GETTEXT("~: too large shift amount ~"));
  }
 #ifdef intQsize
  VALUES1(UQ_to_I((del << shift) + n2 - o2));
 #else
  VALUES1(UL2_to_I(del >> 32-shift,(del << shift) + n2 - o2));
 #endif
  skipSTACK(5);
}
