/*
 * system calls
 * Copyright (C) 2003-2005 Sam Steingold
 * Copyright (C) 2005 Bruno Haible
 * Copyright (C) 2005 Arseny Slobodyuk
 * GPL2
 */

#if defined(_WIN32)
/* need this for CreateHardLink to work */
# define WINVER 0x0500
/* get ASCII functions */
# undef UNICODE
#endif
#if defined(__CYGWIN__)
# define UNIX_CYGWIN32
# undef UNICODE
#endif

#include "clisp.h"
#include "config.h"

#if defined(TIME_WITH_SYS_TIME)
# include <sys/time.h>
# include <time.h>
#else
# if defined(HAVE_SYS_TIME_H)
#  include <sys/time.h>
# elif defined(HAVE_TIME_H)
#  include <time.h>
# endif
#endif
#if defined(HAVE_UNISTD_H)
# if defined(__sun)
#  define __EXTENSIONS__        /* enable confstr() prototype */
# endif
# include <unistd.h>
#endif
#if defined(HAVE_SYS_UNISTD_H)
# include <sys/unistd.h>
#endif
#if defined(HAVE_ERRNO_H)
# include <errno.h>
#endif
#if defined(HAVE_SYS_TYPES_H)
# include <sys/types.h>
#endif
#if defined(HAVE_SYS_STAT_H)
# include <sys/stat.h>
#endif
#if defined(HAVE_SYS_RESOURCE_H)
# include <sys/resource.h>
#endif
#if defined(HAVE_SYS_STATVFS_H)
# include <sys/statvfs.h>
#endif
#if defined(HAVE_CRYPT_H)
# include <crypt.h>
#endif
#if defined(HAVE_UTIME_H)
# include <utime.h>
#endif
#if defined(HAVE_WCHAR_H)
# include <wchar.h>
#endif
#include <limits.h>
#if !defined(NZERO)             /* should be defined in <limits.h> */
#  define NZERO 20
#endif
#if defined(HAVE_SYSLOG_H)
# include <syslog.h>
#endif

#include <stdio.h>             /* for BUFSIZ */

/* #define DEBUG */
#if defined(DEBUG)
extern object nobject_out (FILE* stream, object obj);
# define XOUT(obj,label)                                                \
  (printf("[%s:%d] %s: %s:\n",__FILE__,__LINE__,STRING(obj),label),     \
   obj=nobject_out(stdout,obj), printf("\n"))
#else
# undef OBJECT_OUT
# define OBJECT_OUT(o,l)
# define XOUT(o,l)
#endif

DEFMODULE(syscalls,"POSIX")

#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
/* we use posix fcntl() on unix and win32 LockFileEx() on win32.
   since cygwin supports fcntl(), we use it there, but another option
   would be to use cygwin get_osfhandle() + win32 LockFileEx(),
   see <http://article.gmane.org/gmane.os.cygwin/35175> */
#if defined(HAVE_FCNTL_H)
# include <fcntl.h>
#endif

DEFUN(POSIX::STREAM-LOCK, stream lockp &key BLOCK SHARED START LENGTH)
{ /* the interface to fcntl(2) */
  Handle fd = (Handle)-1;
  bool lock_p = !nullp(STACK_4), failed_p;
  object stream;
  uintL start = missingp(STACK_1) ? 0 : I_to_L(STACK_1);
  uintL length;
#if defined(WIN32_NATIVE)
  DWORD flags = !lock_p ? 0 :
    (missingp(STACK_2) ? LOCKFILE_EXCLUSIVE_LOCK : 0) |
    (nullp(STACK_3) ? 0 : LOCKFILE_FAIL_IMMEDIATELY);
  OVERLAPPED ol = {0,0,start,0,NULL};
#else
  int cmd = nullp(STACK_3) ? F_SETLK : F_SETLKW;
  struct flock fl;
  fl.l_type = missingp(STACK_2) ? F_RDLCK : F_WRLCK;
  fl.l_whence = SEEK_SET;
  fl.l_start = start;
#endif
  if (posfixnump(STACK_5)) {
    fd = (Handle)posfixnum_to_L(STACK_5);
    stream = nullobj;
  } else {
    stream = open_file_stream_handle(STACK_5,&fd);
  }
  if (missingp(STACK_0)) { /* no :LENGTH => use file size */
    if (posfixnump(STACK_5)) { /* no stream given, use OS to get file size */
#    if defined(WIN32_NATIVE)
      LARGE_INTEGER size;
      begin_system_call();
      size.LowPart = GetFileSize(fd,&size.HighPart);
      /* Value returned can be (LONG) -1 even on success,
         check the last error code */
      if ((LONG)size.LowPart == -1) failed_p = GetLastError();
        else failed_p = 0;
      end_system_call();
      if (failed_p) goto error;
      length = size.LowPart;
#    elif defined(HAVE_FSTAT)
      struct stat st;
      begin_system_call();
      failed_p = (-1 == fstat(fd,&st));
      end_system_call();
      if (failed_p) goto error;
      length = st.st_size;
#    else
      length = 0;
#    endif
    } else { /* a valid stream has been supplied */
      pushSTACK(stream); funcall(L(file_length),1);
      length = I_to_UL(value1);
    }
  } else length = I_to_UL(STACK_0);
  begin_system_call();
#if defined(WIN32_NATIVE)
  if (lock_p) {
    failed_p = !LockFileEx(fd,flags,0,length,0,&ol);
    if (failed_p && nullp(STACK_3) && GetLastError() == ERROR_LOCK_VIOLATION)
      failed_p = lock_p = false; /* failed to lock, :BLOCK NIL */
  } else
    failed_p = !UnlockFileEx(fd,0,length,0,&ol);
#else
  fl.l_len = length;
  if ((failed_p = (-1 == fcntl(fd,cmd,&fl)))
      && lock_p && (cmd == F_SETLK) && (errno == EACCES || errno == EAGAIN))
    failed_p = lock_p = false; /* failed to lock, :BLOCK NIL */
#endif
  end_system_call();
  if (failed_p) { error:
    if (eq(stream,nullobj)) OS_error();
    else OS_filestream_error(stream);
  }
  skipSTACK(6);
  VALUES_IF(lock_p);
}

#endif  /* fcntl | WIN32_NATIVE */

/* ============================== syslog ============================== */
#if defined(HAVE_SYSLOG)
DEFCHECKER(check_syslog_severity,prefix=LOG,reverse=sint_to_I,  \
           EMERG ALERT CRIT ERR WARNING NOTICE INFO DEBUG)
DEFCHECKER(check_syslog_facility,default=LOG_USER,prefix=LOG,\
           KERN USER MAIL NEWS UUCP DAEMON AUTH CRON LPR SYSLOG AUTHPRIV FTP \
           LOCAL0 LOCAL1 LOCAL2 LOCAL3 LOCAL4 LOCAL5 LOCAL6 LOCAL7)
DEFFLAGSET(syslog_opt_flags,LOG_PID LOG_CONS LOG_NDELAY LOG_ODELAY LOG_NOWAIT)
#if defined(HAVE_OPENLOG)
DEFUN(POSIX:OPENLOG,ident &key :PID :CONS :NDELAY :ODELAY :NOWAIT :FACILITY) {
  int facility = check_syslog_facility(popSTACK());
  int logopt = syslog_opt_flags();
  with_string_0(check_string(popSTACK()),GLO(misc_encoding),ident, {
      begin_system_call();
      openlog(ident,logopt,facility);
      end_system_call();
    });
  VALUES0;
}
#endif
#if defined(HAVE_SETLOGMASK)
DEFUN(POSIX:SETLOGMASK, maskpri) {
  int priority = (missingp(STACK_0) ? (skipSTACK(1),0) /*query*/ :
                  check_syslog_severity(popSTACK()));
  int logmask;
  begin_system_call();
  logmask = setlogmask(LOG_MASK(priority));
  end_system_call();
  VALUES1(check_syslog_severity_reverse(logmask));
}
#endif
DEFUN(POSIX::%SYSLOG, severity facility message) {
  int priority =
    check_syslog_severity(STACK_2) | check_syslog_facility(STACK_1);
  with_string_0(STACK_0 = check_string(STACK_0),GLO(misc_encoding),mesg, {
      begin_system_call();
      syslog(priority,mesg);
      end_system_call();
    });
  VALUES0; skipSTACK(3);
}
#if defined(HAVE_CLOSELOG)
DEFUN(POSIX:CLOSELOG,) {
  begin_system_call(); closelog(); end_system_call();
  VALUES0;
}
#endif
#endif  /* HAVE_SYSLOG */

/* ========================= processes & signals ========================= */
#if defined(HAVE_GETSID)
DEFUN(POSIX:GETSID, pid) {
  pid_t pid = posfixnum_to_L(check_posfixnum(popSTACK()));
  pid_t ret;
  begin_system_call(); ret=getsid(pid); end_system_call();
  if (ret==(pid_t)-1) OS_error();
  VALUES1(fixnum(ret));
}
#endif
#if defined(HAVE_SETSID)
DEFUN(POSIX:SETSID,) {
  pid_t ret;
  begin_system_call(); ret=setsid(); end_system_call();
  if (ret==(pid_t)-1) OS_error();
  VALUES1(fixnum(ret));
}
#endif
#if defined(HAVE_GETPGRP)
DEFUN(POSIX:GETPGRP, pid) {
  pid_t pid = posfixnum_to_L(check_posfixnum(popSTACK()));
  pid_t ret;
  begin_system_call(); ret=getpgrp(pid); end_system_call();
  if (ret==(pid_t)-1) OS_error();
  VALUES1(fixnum(ret));
}
#endif
#if defined(HAVE_SETPGRP)
DEFUN(POSIX:SETPGRP,) {
  pid_t ret;
  begin_system_call(); ret=setpgrp(); end_system_call();
  if (ret==(pid_t)-1) OS_error();
  VALUES1(fixnum(ret));
}
#endif
#if defined(HAVE_SIGNAL_H)
/* http://www.opengroup.org/onlinepubs/009695399/basedefs/signal.h.html */
# include <signal.h>
#endif
DEFCHECKER(check_signal,SIGABRT SIGALRM SIGBUS SIGCHLD SIGCONT SIGFPE SIGHUP \
           SIGILL SIGINT SIGKILL SIGPIPE SIGQUIT SIGSEGV SIGSTOP SIGTERM \
           SIGTSTP SIGTTIN SIGTTOU SIGUSR1 SIGUSR2 SIGPOLL SIGPROF SIGSYS \
           SIGTRAP SIGURG SIGVTALRM SIGXCPU SIGXFSZ)
#if defined(HAVE_KILL)
DEFUN(POSIX:KILL, pid sig) {
  int sig = check_signal(popSTACK());
  pid_t pid = posfixnum_to_L(check_posfixnum(popSTACK()));
  int ret;
  begin_system_call(); ret=kill(pid,sig); end_system_call();
  if (ret==-1) OS_error();
  VALUES0;
}
#endif

/* ========================== process priority ========================== */
#if defined(WIN32_NATIVE)
DEFCHECKER(check_priority_value,suffix=PRIORITY_CLASS,default=0,        \
           REALTIME HIGH ABOVE-NORMAL NORMAL BELOW-NORMAL LOW IDLE)
#else
DEFCHECKER(check_priority_value,default=0,reverse=sint_to_I,                \
           REALTIME=-NZERO HIGH=(-NZERO/2) ABOVE-NORMAL=(-NZERO/4) NORMAL=0 \
           BELOW-NORMAL=(NZERO/4) LOW=(NZERO/2) IDLE=NZERO)
#endif
DEFCHECKER(check_priority_which,prefix=PRIO,default=0, PROCESS PGRP USER)
DEFUN(OS:PRIORITY, pid &optional which) {
  int which = check_priority_which(popSTACK());
  int pid = posfixnum_to_L(check_posfixnum(popSTACK()));
  int res;
#if defined(HAVE_GETPRIORITY)
  errno = 0;
  begin_system_call();
  res = getpriority(which,pid);
  end_system_call();
  if (errno) OS_error();
#elif defined(WIN32_NATIVE)
  HANDLE handle;
  begin_system_call();
  handle = OpenProcess(PROCESS_QUERY_INFORMATION,FALSE,pid);
  if (handle == NULL) OS_error();
  res = (int)GetPriorityClass(handle);
  CloseHandle(handle);
  end_system_call();
#else
  NOTREACHED;
#endif
  VALUES1(check_priority_value_reverse(res));
}
DEFUN(OS:SET-PRIORITY, pid which value) {
  int value = check_priority_value(STACK_0);
  int which = check_priority_which(STACK_1);
  int pid = posfixnum_to_L(check_posfixnum(STACK_2));
  begin_system_call();
#if defined(HAVE_SETPRIORITY)
  if (setpriority(which,pid,value)) OS_error();
#elif defined(WIN32_NATIVE)
  {
    HANDLE handle = OpenProcess(PROCESS_QUERY_INFORMATION,FALSE,pid);
    if (handle == NULL) OS_error();
    if (!SetPriorityClass(handle,value)) OS_error();
    CloseHandle(handle);
  }
#else
  NOTREACHED;
#endif
  end_system_call();
  VALUES1(STACK_0); skipSTACK(3);
}

/* posix math functions in <math.h> */
/* Must include <math.h> */
#define decimal_string  solaris_decimal_string  /* needed on Solaris */
#undef floor  /* needed on Linux */
#include <math.h>
#define floor(a,b)  ((a) / (b))
#undef decimal_string

#define D_S           to_double(popSTACK())
#define I_S           to_int(popSTACK())
#define N_D(n,v)  \
  { double x=n; v=c_double_to_DF((dfloatjanus*)&x); }
#define VAL_D(func)   double res=func(D_S); N_D(res,value1)
#define VAL_ID(func)  \
 double xx=D_S; int nn=I_S; double res=func(nn,xx); N_D(res,value1)

#if defined(HAVE_ERFC)
DEFUNF(POSIX::ERF,x) { VAL_D(erf); mv_count=1; }
#endif
#if defined(HAVE_ERFC)
DEFUNF(POSIX::ERFC,x) { VAL_D(erfc); mv_count=1; }
#endif

DEFUNF(POSIX::J0,x) { VAL_D(j0); mv_count=1; }
DEFUNF(POSIX::J1,x) { VAL_D(j1); mv_count=1; }
DEFUNF(POSIX::JN,i x) { VAL_ID(jn); mv_count=1; }
DEFUNF(POSIX::Y0,x) { VAL_D(y0); mv_count=1; }
DEFUNF(POSIX::Y1,x) { VAL_D(y1); mv_count=1; }
DEFUNF(POSIX::YN,i y) { VAL_ID(yn); mv_count=1; }

#if defined(HAVE_LGAMMA) || HAVE_DECL_LGAMMA_R
DEFUNF(POSIX::LGAMMA,x) {
# if HAVE_DECL_LGAMMA_R
  int sign;
  double res = lgamma_r(D_S,&sign);
  value2 = (sign > 0 ? Fixnum_1 : Fixnum_minus1);
# else
  double res = lgamma(D_S);
# if HAVE_DECL_SIGNGAM
  value2 = (signgam > 0 ? Fixnum_1 : Fixnum_minus1);
# else
  value2 = NIL;
# endif
# endif
  N_D(res,value1); mv_count=2;
}
#endif

#if defined(HAVE_CLOCK)
DEFUN(POSIX:BOGOMIPS,)
{
  if (clock() != (clock_t)-1) {
    unsigned long loops = 1;
    while ((loops <<= 1)) {
      unsigned long ticks, ii;
      ticks = clock();
      for (ii = loops; ii > 0; ii--);
      ticks = clock() - ticks;
      if (ticks >= CLOCKS_PER_SEC) {
        double bogo = (1.0 * loops / ticks) * (CLOCKS_PER_SEC / 500000.0);
        N_D(bogo,value1); mv_count=1;
        return;
      }
    }
  }
  N_D(-1.0,value1); mv_count=1;
}
#endif /* HAVE_CLOCK */

#undef D_S
#undef I_S
#undef N_D
#undef VAL_D
#undef VAL_ID

/* "gcc --mno-cygwin -l crypt" links with cygwin lib-crypt,
   so we have to disable this explicitly */
#if defined(HAVE_CRYPT) && !defined(WIN32_NATIVE)
DEFUN(POSIX::CRYPT, key salt) {
  char *result;
  STACK_0 = check_string(STACK_0);
  STACK_1 = check_string(STACK_1);
  with_string_0(STACK_0,GLO(misc_encoding),salt, {
      with_string_0(STACK_1,GLO(misc_encoding),key, {
          begin_system_call();
          result = crypt(key,salt);
          end_system_call();
        });
    });
  if (result == NULL) OS_error();
  VALUES1(asciz_to_string(result,GLO(misc_encoding)));
  skipSTACK(2);
}
#endif
#if defined(HAVE_ENCRYPT) || defined(HAVE_SETKEY)
/* (encrypt (encrypt X t) nil) == X:
   (let ((v (make-array 8 :element-type (quote (unsigned-byte 8)))) u)
     (dotimes (i 8) (setf (aref v i) (random 256)))
     (setq u (copy-seq v))
     (print v) (os:encrypt v nil)
     (print v) (os:encrypt v t)
     (print v) (equalp v u))
 the above should return T */
/* move information from a bit vector to the char block
 can trigger GC */
static void get_block (char block[64], object vector) {
  while (!bit_vector_p(Atype_8Bit,vector)
         || vector_length(vector) != 8) {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(vector);          /* TYPE-ERROR slot DATUM */
    pushSTACK(`(VECTOR (UNSIGNED-BYTE 8) 8)`); /* EXPECTED-TYPE */
    pushSTACK(STACK_0); pushSTACK(vector);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not of type ~S"));
    vector = value1;
  }
  {
    uintL index=0, ii, jj, kk=0;
    object dv = array_displace_check(vector,8,&index);
    uint8* ptr1 = TheSbvector(dv)->data + index;
    for (ii = 0; ii<8; ii++) {
      uint8 bb = *ptr1++;
      for (jj = 0; jj<8; jj++)
        block[kk++] = ((bb & bit(jj)) != 0);
    }
  }
}
#endif
#if defined(HAVE_ENCRYPT) && !defined(WIN32_NATIVE)
/* the inverse of get_block(): move data from block to vector,
 which is known to be a (VECTOR BIT) */
static void set_block (char block[64], object vector) {
  uintL index=0, ii, jj, kk=0;
  object dv = array_displace_check(vector,8,&index);
  uint8* ptr1 = TheSbvector(dv)->data + index;
  for (ii = 0; ii<8; ii++) {
    uint8 bb = 0;
    for (jj = 0; jj<8; jj++)
      bb |= (block[kk++]!=0) << jj;
    *ptr1++ = bb;
  }
}
DEFUN(POSIX::ENCRYPT, block flag) {
  int flag = nullp(popSTACK());
  char block[64];
  get_block(block,STACK_0);
  begin_system_call();
  errno = 0; encrypt(block,flag);
  if (errno) OS_error();
  end_system_call();
  set_block(block,STACK_0);
  VALUES1(popSTACK());
}
#endif
#if defined(HAVE_SETKEY) && !defined(WIN32_NATIVE)
DEFUN(POSIX::SETKEY, key) {
  char block[64];
  get_block(block,popSTACK());
  begin_system_call();
  errno = 0; setkey(block);
  if (errno) OS_error();
  end_system_call();
  VALUES0;
}
#endif

/* ========= SYSTEM INFORMATION ========== */

#if defined(HAVE_SYS_UTSNAME_H)
# include <sys/utsname.h>
#endif

#if defined(HAVE_UNAME)
DEFUN(POSIX::UNAME,)
{ /* Lisp interface to uname(2) */
  struct utsname utsname;
  begin_system_call(); uname(&utsname); end_system_call();
#define UN(str) pushSTACK(asciz_to_string(str,GLO(misc_encoding)));
  UN(utsname.sysname);
  UN(utsname.nodename);
  UN(utsname.release);
  UN(utsname.version);
  UN(utsname.machine);
#undef UN
  funcall(`POSIX::MAKE-UNAME`,5);
}
#endif /* HAVE_UNAME */

#if defined(HAVE_SYSCONF)
DEFCHECKER(sysconf_arg,prefix=_SC,default=,                             \
           AIO-LISTIO-MAX AIO-MAX AIO-PRIO-DELTA-MAX                    \
           ARG-MAX ATEXIT-MAX BC-BASE-MAX BC-DIM-MAX BC-SCALE-MAX       \
           BC-STRING-MAX CHILD-MAX CLK-TCK COLL-WEIGHTS-MAX DELAYTIMER-MAX \
           EXPR-NEST-MAX HOST-NAME-MAX IOV-MAX LINE-MAX LOGIN-NAME-MAX  \
           NGROUPS-MAX GETGR-R-SIZE-MAX GETPW-R-SIZE-MAX MQ-OPEN-MAX    \
           MQ-PRIO-MAX OPEN-MAX ADVISORY-INFO BARRIERS ASYNCHRONOUS-IO  \
           CLOCK-SELECTION CPUTIME FSYNC IPV6 JOB-CONTROL MAPPED-FILES  \
           MEMLOCK MEMLOCK-RANGE MEMORY-PROTECTION MESSAGE-PASSING      \
           MONOTONIC-CLOCK PRIORITIZED-IO PRIORITY-SCHEDULING RAW-SOCKETS \
           READER-WRITER-LOCKS REALTIME-SIGNALS REGEXP SAVED-IDS SEMAPHORES \
           SHARED-MEMORY-OBJECTS SHELL SPAWN SPIN-LOCKS SPORADIC-SERVER \
           SS-REPL-MAX SYNCHRONIZED-IO THREAD-ATTR-STACKADDR            \
           THREAD-ATTR-STACKSIZE THREAD-CPUTIME THREAD-PRIO-INHERIT     \
           THREAD-PRIO-PROTECT THREAD-PRIORITY-SCHEDULING               \
           THREAD-PROCESS-SHARED THREAD-SAFE-FUNCTIONS THREAD-SPORADIC-SERVER \
           THREADS TIMEOUTS TIMERS TRACE TRACE-EVENT-FILTER             \
           TRACE-EVENT-NAME-MAX TRACE-INHERIT TRACE-LOG TRACE-NAME-MAX  \
           TRACE-SYS-MAX TRACE-USER-EVENT-MAX TYPED-MEMORY-OBJECTS VERSION \
           V6-ILP32-OFF32 V6-ILP32-OFFBIG V6-LP64-OFF64 V6-LPBIG-OFFBIG \
           2-C-BIND 2-C-DEV 2-CHAR-TERM 2-FORT-DEV 2-FORT-RUN 2-LOCALEDEF \
           2-PBS 2-PBS-ACCOUNTING 2-PBS-CHECKPOINT 2-PBS-LOCATE 2-PBS-MESSAGE \
           2-PBS-TRACK 2-SW-DEV 2-UPE 2-VERSION PAGESIZE PHYS-PAGES     \
           AVPHYS-PAGES THREAD-DESTRUCTOR-ITERATIONS THREAD-KEYS-MAX    \
           THREAD-STACK-MIN THREAD-THREADS-MAX RE-DUP-MAX RTSIG-MAX     \
           SEM-NSEMS-MAX SEM-VALUE-MAX SIGQUEUE-MAX STREAM-MAX SYMLOOP-MAX \
           TIMER-MAX TTY-NAME-MAX TZNAME-MAX XBS5-ILP32-OFF32           \
           XBS5-ILP32-OFFBIG XBS5-LP64-OFF64 XBS5-LPBIG-OFFBIG XOPEN-CRYPT \
           XOPEN-ENH-I18N XOPEN-LEGACY XOPEN-REALTIME XOPEN-REALTIME-THREADS \
           XOPEN-SHM XOPEN-STREAMS XOPEN-UNIX XOPEN-VERSION NPROCESSORS-CONF \
           NPROCESSORS-ONLN)
DEFUN(POSIX::SYSCONF, &optional what)
{ /* Lisp interface to sysconf(3c) */
  object what = popSTACK();
  if (!missingp(what)) {
    int cmd = sysconf_arg(what), res;
    begin_system_call(); res = sysconf(cmd); end_system_call();
    VALUES1(L_to_I(res));
  } else { /* all possible values */
    int pos = 0;
    for (; pos < sysconf_arg_table_size; pos++) {
      int res;
      begin_system_call();
      res = sysconf(sysconf_arg_table[pos].c_const);
      end_system_call();
      pushSTACK(*sysconf_arg_table[pos].l_const);
      pushSTACK(L_to_I(res));
    }
    VALUES1(listof(2*sysconf_arg_table_size));
  }
}
#endif /* HAVE_SYSCONF */

#if defined(HAVE_CONFSTR)
DEFCHECKER(confstr_arg,prefix=_CS,PATH POSIX-V6-ILP32-OFF32-CFLAGS      \
           POSIX-V6-ILP32-OFF32-LDFLAGS POSIX-V6-ILP32-OFF32-LIBS       \
           POSIX-V6-ILP32-OFFBIG-CFLAGS POSIX-V6-ILP32-OFFBIG-LDFLAGS   \
           POSIX-V6-ILP32-OFFBIG-LIBS POSIX-V6-LP64-OFF64-CFLAGS        \
           POSIX-V6-LP64-OFF64-LDFLAGS POSIX-V6-LP64-OFF64-LIBS         \
           POSIX-V6-LPBIG-OFFBIG-CFLAGS POSIX-V6-LPBIG-OFFBIG-LDFLAGS   \
           POSIX-V6-LPBIG-OFFBIG-LIBS POSIX-V6-WIDTH-RESTRICTED-ENVS    \
           XBS5-ILP32-OFF32-CFLAGS XBS5-ILP32-OFF32-LDFLAGS             \
           XBS5-ILP32-OFF32-LIBS XBS5-ILP32-OFF32-LINTFLAGS             \
           XBS5-ILP32-OFFBIG-CFLAGS XBS5-ILP32-OFFBIG-LDFLAGS           \
           XBS5-ILP32-OFFBIG-LIBS XBS5-ILP32-OFFBIG-LINTFLAGS           \
           XBS5-LP64-OFF64-CFLAGS XBS5-LP64-OFF64-LDFLAGS               \
           XBS5-LP64-OFF64-LIBS XBS5-LP64-OFF64-LINTFLAGS               \
           XBS5-LPBIG-OFFBIG-CFLAGS XBS5-LPBIG-OFFBIG-LDFLAGS           \
           XBS5-LPBIG-OFFBIG-LIBS XBS5-LPBIG-OFFBIG-LINTFLAGS)
DEFUN(POSIX::CONFSTR, &optional what)
{ /* Lisp interface to confstr(3c) */
#define CS_S(cmd) \
  begin_system_call(); res = confstr(cmd,buf,BUFSIZ); end_system_call(); \
  if (res == 0) pushSTACK(T);                                           \
  else if (res <= BUFSIZ) value1 = asciz_to_string(buf,GLO(misc_encoding)); \
  else {                                                                \
    /* Here we cannot use alloca(), because alloca() is generally unsafe \
       for sizes > BUFSIZ. */                                           \
    char *tmp;                                                          \
    begin_system_call();                                                \
    tmp = (char*)my_malloc(res);                                        \
    confstr(cmd,tmp,res);                                               \
    end_system_call();                                                  \
    value1 = asciz_to_string(tmp,GLO(misc_encoding));                   \
    begin_system_call();                                                \
    free(tmp);                                                          \
    end_system_call();                                                  \
  }

  size_t res;
  char buf[BUFSIZ];
  object what = popSTACK();
  if (!missingp(what)) {
    int cmd = confstr_arg(what);
    CS_S(cmd); mv_count = 1;
  } else { /* all possible values */
    unsigned int pos = 0;
    for (; pos < confstr_arg_table_size; pos++) {
      CS_S(confstr_arg_table[pos].c_const);
      pushSTACK(*confstr_arg_table[pos].l_const);
      pushSTACK(value1);
    }
    VALUES1(listof(2*confstr_arg_table_size));
  }
}
#endif /* HAVE_CONFSTR */

#if defined(HAVE_GETRUSAGE)
DEFUN(POSIX::USAGE,)
{ /* getrusage(3) */

#define GETRU(who)                                              \
  begin_system_call(); getrusage(who,&ru); end_system_call();   \
  tmp = ru.ru_utime.tv_sec + 0.000001 * ru.ru_utime.tv_usec;    \
  pushSTACK(c_double_to_DF((dfloatjanus*)&tmp));                \
  tmp = ru.ru_stime.tv_sec + 0.000001 * ru.ru_stime.tv_usec;    \
  pushSTACK(c_double_to_DF((dfloatjanus*)&tmp));                \
  pushSTACK(L_to_I(ru.ru_maxrss));                              \
  pushSTACK(L_to_I(ru.ru_ixrss));                               \
  pushSTACK(L_to_I(ru.ru_idrss));                               \
  pushSTACK(L_to_I(ru.ru_isrss));                               \
  pushSTACK(L_to_I(ru.ru_minflt));                              \
  pushSTACK(L_to_I(ru.ru_majflt));                              \
  pushSTACK(L_to_I(ru.ru_nswap));                               \
  pushSTACK(L_to_I(ru.ru_inblock));                             \
  pushSTACK(L_to_I(ru.ru_oublock));                             \
  pushSTACK(L_to_I(ru.ru_msgsnd));                              \
  pushSTACK(L_to_I(ru.ru_msgrcv));                              \
  pushSTACK(L_to_I(ru.ru_nsignals));                            \
  pushSTACK(L_to_I(ru.ru_nvcsw));                               \
  pushSTACK(L_to_I(ru.ru_nivcsw))

  struct rusage ru;
  double tmp;
  pushSTACK(NIL);               /* to save the children data */
  GETRU(RUSAGE_SELF);
  GETRU(RUSAGE_CHILDREN);
  funcall(`POSIX::MAKE-USAGE`,16); /* children */
  STACK_(14) = value1;
  funcall(`POSIX::MAKE-USAGE`,16); /* self */
  value2 = popSTACK();
  mv_count = 2;
#undef GETRU
}
#endif /* HAVE_GETRUSAGE */

#if defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)
DEFCHECKER(getrlimit_arg,prefix=RLIMIT, CPU FSIZE DATA STACK CORE RSS NOFILE \
           AS NPROC MEMLOCK LOCKS)
#if SIZEOF_RLIMT_T == 8
# define rlim_to_I_0(lim) uint64_to_I(lim)
# define I_to_rlim_0(lim) I_to_uint64(check_uint64(lim))
#else
# define rlim_to_I_0(lim) uint32_to_I(lim)
# define I_to_rlim_0(lim) I_to_uint32(check_uint32(lim))
#endif
static /* maygc */ inline object rlim_to_I (rlim_t lim)
{ return lim == RLIM_INFINITY ? NIL : rlim_to_I_0(lim); }
static /* maygc */ inline rlim_t I_to_rlim (object lim)
{ return missingp(lim) ? RLIM_INFINITY : I_to_rlim_0(lim); }
#endif /* HAVE_GETRLIMIT || HAVE_SETRLIMIT */
#if defined(HAVE_GETRLIMIT)
DEFUN(POSIX::RLIMIT, &optional what)
{ /* getrlimit(3) */
#define RLIM(what)                                                      \
  begin_system_call();                                                  \
  if (getrlimit(what,&rl)) OS_error();                                  \
  end_system_call();                                                    \
  pushSTACK(rlim_to_I(rl.rlim_cur)); pushSTACK(rlim_to_I(rl.rlim_max))

  struct rlimit rl;
  object what = popSTACK();
  if (!missingp(what)) {
    int cmd = getrlimit_arg(what);
    RLIM(cmd);
    funcall(L(values),2);
  } else {
    unsigned int pos;
    for (pos = 0; pos < getrlimit_arg_table_size; pos++) {
      pushSTACK(*getrlimit_arg_table[pos].l_const);
      RLIM(getrlimit_arg_table[pos].c_const);
      funcall(`POSIX::MAKE-RLIMIT`,2); pushSTACK(value1);
    }
    VALUES1(listof(2*getrlimit_arg_table_size));
  }
#undef RLIM
}
#endif /* HAVE_GETRLIMIT */
#if defined(HAVE_SETRLIMIT)
/* parse the RLIMIT structure
 can trigger GC */
static void check_rlimit (object arg, struct rlimit *rl) {
  pushSTACK(check_classname(arg,`POSIX::RLIMIT`));
  rl->rlim_cur = I_to_rlim(TheStructure(STACK_0)->recdata[1]);
  rl->rlim_max = I_to_rlim(TheStructure(STACK_0)->recdata[2]);
  skipSTACK(1);
}
DEFUN(POSIX::SET-RLIMIT, what cur max)
{ /* setrlimit(3): 3 ways to call:
   (setf (rlimit what) (values cur max))
   (setf (rlimit what) #S(rlimit :cur cur :max max))
   (setf (rlimit) rlimit-alist-as-returned-by-rlimit-without-arguments) */
  if (nullp(STACK_2)) {         /* 3rd way */
    if (!nullp(STACK_0)) goto rlimit_bad;
    STACK_0 = STACK_1;
    while (!endp(STACK_0)) {
      int what = getrlimit_arg(Car(STACK_0));
      struct rlimit rl;
      STACK_0 = Cdr(STACK_0);
      if (!consp(STACK_0)) { STACK_0 = NIL; goto rlimit_bad; }
      check_rlimit(Car(STACK_0),&rl);
      STACK_0 = Cdr(STACK_0);
      begin_system_call();
      if (setrlimit(what,&rl)) OS_error();
      end_system_call();
    }
  } else {
    int what = getrlimit_arg(STACK_2);
    struct rlimit rl;
    if (nullp(STACK_1) || posfixnump(STACK_1)) { /* 1st way */
      rl.rlim_cur = I_to_rlim(STACK_1);
      rl.rlim_max = I_to_rlim(STACK_0);
    } else {                    /* 2nd way */
      if (!nullp(STACK_0)) goto rlimit_bad;
      check_rlimit(STACK_1,&rl);
    }
    begin_system_call();
    if (setrlimit(what,&rl)) OS_error();
    end_system_call();
  }
  VALUES2(STACK_1,STACK_0); skipSTACK(3); return;
 rlimit_bad:
  pushSTACK(TheSubr(subr_self)->name);
  fehler(error,GETTEXT("~S: bad arguments: ~S ~S ~S"));
}
#endif /* HAVE_SETRLIMIT */

/* ==== SOCKETS ===== */
#if defined(HAVE_NETDB_H)
# include <netdb.h>
#endif

#define H_ERRMSG                                                           \
        (h_errno == HOST_NOT_FOUND ? "host not found" :                    \
         (h_errno == TRY_AGAIN ? "try again later" :                       \
          (h_errno == NO_RECOVERY ? "a non-recoverable error occurred" :   \
           (h_errno == NO_DATA ? "valid name, but no data for this host" : \
            (h_errno == NO_ADDRESS ? "no IP address for this host" :       \
             "unknown error")))))

#if 0
void print_he (struct hostent *he) {
 int ii;
 char **pp;
 struct in_addr in;
 printf("h_name: %s; h_length: %d; h_addrtype: %d\n [size in.s_addr: %d]\n",
        he->h_name,he->h_length,he->h_addrtype,sizeof(in.s_addr));
 for (pp = he->h_aliases; *pp != 0; pp++) printf("\t%s", *pp);
 printf("\n IP:");
 for (pp = he->h_addr_list; *pp != 0; pp++) {
   (void) memcpy(&in.s_addr, *pp, sizeof (in.s_addr));
   (void) printf("\t%s", inet_ntoa(in));
 }
 printf("\n");
}
#endif

/* while TEST collect EXPR into VAL -- see src/socket.d
 can trigger GC */
#define ARR_TO_LIST(val,test,expr)                      \
  { int ii; for (ii = 0; test; ii ++) { pushSTACK(expr); } val = listof(ii); }

/* push the contents of HE onto the stack
 4 values are pushed:
   h_name
   list of h_aliases
   list of h_addr_list
   addrtype
 can trigger GC */
static void hostent_to_stack (struct hostent *he) {
  object tmp;
  pushSTACK(ascii_to_string(he->h_name));
  ARR_TO_LIST(tmp,(he->h_aliases[ii] != NULL),
              asciz_to_string(he->h_aliases[ii],GLO(misc_encoding)));
  pushSTACK(tmp);
  ARR_TO_LIST(tmp,(he->h_addr_list[ii] != NULL),
              addr_to_string(he->h_addrtype,he->h_addr_list[ii]));
  pushSTACK(tmp);
  pushSTACK(fixnum(he->h_addrtype));
}

DEFUN(POSIX::RESOLVE-HOST-IPADDR,host)
{ /* Lisp interface to gethostbyname(3) and gethostbyaddr(3) */
  object arg = popSTACK();
  struct hostent *he = NULL;

  if (nullp(arg)) {
#  if !defined(HAVE_GETHOSTENT)
    VALUES1(NIL);
#  else
    int count = 0;
    begin_system_call();
    for (; (he = gethostent()); count++) {
      hostent_to_stack(he);
      funcall(`POSIX::MAKE-HOSTENT`,4);
      pushSTACK(value1);
    }
    endhostent();
    end_system_call();
    VALUES1(listof(count));
#  endif
    return;
  }

  he = resolve_host(arg);

  if (he == NULL) {
    pushSTACK(arg); pushSTACK(arg);
    STACK_1 = ascii_to_string(H_ERRMSG);
    pushSTACK(`POSIX::RESOLVE-HOST-IPADDR`);
    fehler(os_error,"~S (~S): ~S");
  }

  hostent_to_stack(he);
  funcall(`POSIX::MAKE-HOSTENT`,4);
}

#if defined(HAVE_GETLOGIN) && defined(HAVE_GETPWNAM) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)

#if defined(HAVE_PWD_H)
# include <pwd.h>
#endif

#define PASSWD_TO_STACK(pwd)                                            \
  pushSTACK(asciz_to_string(pwd->pw_name,GLO(misc_encoding)));          \
  pushSTACK(asciz_to_string(pwd->pw_passwd,GLO(misc_encoding)));        \
  pushSTACK(UL_to_I(pwd->pw_uid));                                      \
  pushSTACK(UL_to_I(pwd->pw_gid));                                      \
  pushSTACK(asciz_to_string(pwd->pw_gecos,GLO(misc_encoding)));         \
  pushSTACK(asciz_to_string(pwd->pw_dir,GLO(misc_encoding)));           \
  pushSTACK(asciz_to_string(pwd->pw_shell,GLO(misc_encoding)))

DEFUN(POSIX::USER-DATA, user)
{ /* return the USER-DATA for the user or a list thereof if user is NIL. */
  object user = popSTACK();
  struct passwd *pwd = NULL;

# if defined(HAVE_GETPWENT)
  if (nullp(user)) { /* all users as a list */
    int count = 0;
    begin_system_call();
    for (; (pwd = getpwent()); count++) {
      PASSWD_TO_STACK(pwd);
      funcall(`POSIX::MAKE-USER-DATA`,7);
      pushSTACK(value1);
    }
    endpwent();
    end_system_call();
    VALUES1(listof(count));
    return;
  }
#endif

  begin_system_call();
  if (posfixnump(user))
    pwd = getpwuid(posfixnum_to_L(user));
  else if (eq(user,`:DEFAULT`)) {
    char *username = getlogin();
    if (username != NULL)
      pwd = getpwnam(username);
    else
      pwd = getpwuid(getuid());
  } else if (symbolp(user)) {
    with_string_0(Symbol_name(user),GLO(misc_encoding),userz,
                  { pwd = getpwnam(userz); });
  } else if (stringp(user)) {
    with_string_0(user,GLO(misc_encoding),userz,
                  { pwd = getpwnam(userz); });
  } else {
    end_system_call(); fehler_string_integer(user);
  }
  end_system_call();

  if (NULL == pwd) { OS_error(); }
  PASSWD_TO_STACK(pwd);
  funcall(`POSIX::MAKE-USER-DATA`,7);
}
#endif  /* getlogin getpwent getpwnam getpwuid getuid */

#if defined(HAVE_FSTAT) && defined(HAVE_STAT)
# if !defined(HAVE_LSTAT)
#  define lstat stat
# endif
DEFUN(POSIX::FILE-STAT, file &optional linkp)
{ /* Lisp interface to stat(2), lstat(2) and fstat(2)
 the first arg can be a pathname designator or a file descriptor designator
 the return value is the FILE-STAT structure */
  bool link_p = missingp(STACK_0);
  object file = (skipSTACK(1), popSTACK());
  struct stat buf;

  if (builtin_stream_p(file)) {
    pushSTACK(file);
    funcall(L(built_in_stream_open_p),1);
    if (!nullp(value1)) { /* open stream ==> use FD */
      begin_system_call();
      /* win32 declares fstat() as accepting int, not HANDLE */
      if (fstat((int)stream_lend_handle(file,true,NULL),&buf) < 0) OS_error();
      end_system_call();
    } else goto stat_pathname;
  } else if (integerp(file)) {
    begin_system_call();
    if (fstat(I_to_L(file),&buf) < 0) OS_error();
    end_system_call();
  } else { stat_pathname:
    file = physical_namestring(file);
    with_string_0(file,GLO(pathname_encoding),namez, {
      begin_system_call();
      if ((link_p ? stat(namez,&buf) : lstat(namez,&buf)) < 0)
        OS_error();
      end_system_call();
    });
  }

  pushSTACK(file);                    /* the object stat'ed */
  pushSTACK(L_to_I(buf.st_dev));      /* device */
  pushSTACK(UL_to_I(buf.st_ino));     /* inode */
  pushSTACK(UL_to_I(buf.st_mode));    /* protection */
  pushSTACK(UL_to_I(buf.st_nlink));   /* number of hard links */
  pushSTACK(UL_to_I(buf.st_uid));     /* user ID of owner */
  pushSTACK(UL_to_I(buf.st_gid));     /* group ID of owner */
#if defined(HAVE_STAT_ST_RDEV)
  pushSTACK(L_to_I(buf.st_rdev));     /* device type (if inode device) */
#else
  pushSTACK(NIL);
#endif
  pushSTACK(L_to_I(buf.st_size));     /* total size, in bytes */
#if defined(HAVE_STAT_ST_BLKSIZE)
  pushSTACK(UL_to_I(buf.st_blksize)); /* blocksize for filesystem I/O */
#else
  pushSTACK(NIL);
#endif
#if defined(HAVE_STAT_ST_BLOCKS)
  pushSTACK(UL_to_I(buf.st_blocks));  /* number of blocks allocated */
#else
  pushSTACK(NIL);
#endif
  pushSTACK(UL_to_I(buf.st_atime+UNIX_LISP_TIME_DIFF));/*time of last access*/
  pushSTACK(UL_to_I(buf.st_mtime+UNIX_LISP_TIME_DIFF));/*last modification*/
  pushSTACK(UL_to_I(buf.st_ctime+UNIX_LISP_TIME_DIFF));/*time of last change*/
  funcall(`POSIX::MAKE-FILE-STAT`,14);
}
#endif  /* fstat lstat fstat */

#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
/* error-signalling replacement for chmod()
   STACK_O is the path - for error reporting
 can trigger GC */
static void my_chmod (char *path, mode_t mode) {
#if defined(HAVE_CHMOD)
  if (chmod(path,mode)) OS_file_error(STACK_0);
#else
  end_system_call();
  pushSTACK(CLSTEXT("~S(~S ~S ~S): this platform lacks ~S"));
  pushSTACK(TheSubr(subr_self)->name); pushSTACK(STACK_2);
  pushSTACK(`:MODE`); pushSTACK(fixnum(mode));
  pushSTACK(`"chmod()"`);
  funcall(S(warn),5);
  begin_system_call();
#endif
}
/* error-signalling replacement for chown()
   STACK_O is the path - for error reporting
 can trigger GC */
static void my_chown (char *path, uid_t uid, gid_t gid) {
#if defined(HAVE_CHOWN)
  if (chown(path,uid,gid)) OS_file_error(STACK_0);
#else
  end_system_call();
  pushSTACK(CLSTEXT("~S(~S ~S ~S ~S ~S): this platform lacks ~S"));
  pushSTACK(TheSubr(subr_self)->name); pushSTACK(STACK_2);
  pushSTACK(`:UID`); pushSTACK((uid != (uid_t)-1) ? fixnum(uid) : NIL);
  pushSTACK(`:GID`); pushSTACK((gid != (gid_t)-1) ? fixnum(gid) : NIL);
  pushSTACK(`"chown()"`);
  funcall(S(warn),7);
  begin_system_call();
#endif
}
/* error-signalling replacement for utime()
   STACK_O is the path - for error reporting
 can trigger GC */
static void my_utime (char *path, bool utb_a, bool utb_m, struct utimbuf *utb) {
  if (utb_a && !utb_m) {
    struct stat st;
    if (stat(path,&st) < 0) OS_file_error(STACK_0);
    utb->modtime = st.st_mtime;
  }
  if (utb_m && !utb_a) {
    struct stat st;
    if (stat(path,&st) < 0) OS_file_error(STACK_0);
    utb->actime = st.st_atime;
  }
#if defined(HAVE_UTIME)
  if (utime(path,utb)) OS_file_error(STACK_0);
#else
  end_system_call();
  pushSTACK(CLSTEXT("~S(~S ~S ~S ~S ~S): this platform lacks ~S"));
  pushSTACK(TheSubr(subr_self)->name); pushSTACK(STACK_2);
  pushSTACK(`:ATIME`);
  pushSTACK(utb_a ? UL_to_I(utb->actime + UNIX_LISP_TIME_DIFF) : NIL);
  pushSTACK(`:MTIME`);
  pushSTACK(utb_m ? UL_to_I(utb->modtime + UNIX_LISP_TIME_DIFF) : NIL);
  pushSTACK(`"utime()"`);
  funcall(S(warn),7);
  begin_system_call();
#endif
}
DEFUN(POSIX::SET-FILE-STAT, file &key :ATIME :MTIME :MODE :UID :GID)
{ /* interface to chmod(2), chown(2), utime(2)
     http://www.opengroup.org/onlinepubs/009695399/functions/utime.html
     http://www.opengroup.org/onlinepubs/009695399/functions/chown.html
     http://www.opengroup.org/onlinepubs/009695399/functions/chmod.html */
  gid_t gid = (missingp(STACK_0) ? skipSTACK(1), (gid_t)-1
               : posfixnum_to_L(check_posfixnum(popSTACK())));
  uid_t uid = (missingp(STACK_0) ? skipSTACK(1), (uid_t)-1
               : posfixnum_to_L(check_posfixnum(popSTACK())));
  mode_t mode = (missingp(STACK_0) ? skipSTACK(1), (mode_t)-1
                 : posfixnum_to_L(check_posfixnum(popSTACK())));
  struct utimbuf utb;
  bool utb_a = false, utb_m = false;
  if (!missingp(STACK_0)) {     /* mtime */
    if (integerp(STACK_0))
      utb.modtime = I_to_L(STACK_0) - UNIX_LISP_TIME_DIFF;
    else if (eq(STACK_0,T)) utb.modtime = time(&utb.modtime);
    else {
      object path = physical_namestring(STACK_0);
      struct stat st;
      with_string_0(path,GLO(pathname_encoding),pathz,
                    { if (stat(pathz,&st) < 0) OS_file_error(path); });
      utb.modtime = st.st_mtime;
    }
    utb_m = true;
  }
  if (!missingp(STACK_1)) {     /* atime */
    if (eq(STACK_0,STACK_1)) utb.actime = utb.modtime;
    else if (integerp(STACK_1))
      utb.actime = I_to_L(STACK_0) - UNIX_LISP_TIME_DIFF;
    else if (eq(STACK_1,T)) utb.actime = time(&utb.actime);
    else {
      object path = physical_namestring(STACK_1);
      struct stat st;
      with_string_0(path,GLO(pathname_encoding),pathz,
                    { if (stat(pathz,&st) < 0) OS_file_error(path); });
      utb.actime = st.st_atime;
    }
    utb_a = true;
  }
  skipSTACK(2);                 /* drop atime & mtime */
  STACK_0 = physical_namestring(STACK_0);
  with_string_0(STACK_0,GLO(pathname_encoding),path, {
      begin_system_call();
      if (mode != (mode_t)-1) my_chmod(path,mode);
      if ((uid != (uid_t)-1) || (gid != (gid_t)-1)) my_chown(path,uid,gid);
      if (utb_a || utb_m) my_utime(path,utb_a,utb_m,&utb);
      end_system_call();
    });
  VALUES0; skipSTACK(1);
}
#endif  /* chmod chown utime */

/* <http://www.opengroup.org/onlinepubs/009695399/basedefs/sys/stat.h.html> */
DEFCHECKER(check_chmod_mode,prefix=S,default=, bitmasks=both, SUID SGID SVTX \
           RWXU RUSR WUSR XUSR RWXG RGRP WGRP XGRP RWXO ROTH WOTH XOTH)

DEFUN(POSIX::CONVERT-MODE, mode)
{ /* convert between symbolic and numeric permissions */
  if (posfixnump(STACK_0))
    VALUES1(check_chmod_mode_to_list(posfixnum_to_L(check_posfixnum(popSTACK()))));
  else if (listp(STACK_0))
    VALUES1(fixnum(check_chmod_mode_from_list(popSTACK())));
  else VALUES1(fixnum(check_chmod_mode(popSTACK())));
}

#if defined(HAVE_UMASK)
DEFUN(POSIX::UMASK, cmask)
{ /* lisp interface to umask(2)
     http://www.opengroup.org/onlinepubs/009695399/functions/umask.html */
  mode_t cmask = posfixnum_to_L(check_posfixnum(popSTACK()));
  begin_system_call();
  cmask = umask(cmask);
  end_system_call();
  VALUES1(fixnum(cmask));
}
#endif  /* umask */

#if defined(HAVE_MKNOD)
DEFCHECKER(mknod_type_check,prefix=S,default=,          \
           IFIFO IFSOCK IFCHR IFDIR IFBLK IFREG)
DEFUN(POSIX::MKNOD, path type mode)
{ /* lisp interface to mknod(2)
     http://www.opengroup.org/onlinepubs/009695399/functions/mknod.html */
  mode_t mode = posfixnum_to_L(check_posfixnum(popSTACK()));
  mode |= mknod_type_check(popSTACK());
  funcall(L(namestring),1);     /* drop path from STACK */
  with_string_0(value1,GLO(pathname_encoding),path, {
      begin_system_call();
      if (mknod(path,mode,0)) OS_file_error(value1);
      end_system_call();
    });
  VALUES0;
}
#endif  /* mknod */

#if defined(HAVE_FSTATVFS) && defined(HAVE_STATVFS)
/* there is also a legacy interface (f)statfs()
   which is not POSIX and is not supported */

DEFUN(POSIX::STAT-VFS, file)
{ /* Lisp interface to statvfs(2), fstatvfs(2)
 the first arg can be a pathname designator or a file descriptor designator
 the return value is the STAT-VFS structure */
  object file = popSTACK();
  struct statvfs buf;

  if (builtin_stream_p(file)) {
    pushSTACK(file);
    funcall(L(built_in_stream_open_p),1);
    if (!nullp(value1)) { /* open stream ==> use FD */
      begin_system_call();
      if (fstatvfs(stream_lend_handle(file,true,NULL),&buf) < 0) OS_error();
      end_system_call();
    } else goto stat_pathname;
  } else if (integerp(file)) {
    begin_system_call();
    if (fstatvfs(I_to_L(file),&buf) < 0) OS_error();
    end_system_call();
  } else { stat_pathname:
    file = physical_namestring(file);
    with_string_0(file,GLO(pathname_encoding),namez, {
      begin_system_call();
      if (statvfs(namez,&buf) < 0) OS_error();
      end_system_call();
    });
  }

  pushSTACK(file);                  /* the object statvfs'ed */
  /* FIXME: Should use ulong_to_I for most of these, and map -1 to NIL. */
  pushSTACK(UL_to_I(buf.f_bsize));  /* file system block size */
  pushSTACK(UL_to_I(buf.f_frsize)); /* fundamental file system block size */
  pushSTACK(UL_to_I(buf.f_blocks)); /* total # of blocks on file system */
  pushSTACK(UL_to_I(buf.f_bfree));  /* total number of free blocks */
  pushSTACK(UL_to_I(buf.f_bavail)); /* # of free blocks available to
                                       non-privileged processes */
  pushSTACK(UL_to_I(buf.f_files));  /* total # of file serial numbers */
  pushSTACK(UL_to_I(buf.f_ffree));  /* total # of free file serial numbers */
  pushSTACK(UL_to_I(buf.f_favail)); /* # of file serial numbers available to
                                       non-privileged processes */
#if HAVE_SCALAR_FSID
  pushSTACK(UL_to_I(buf.f_fsid));   /* file system ID */
#else
  /* On Linux, f_fsid of 'struct statfs' is a struct consisting of two ints.
     With glibc <= 2.1, f_fsid of 'struct statvfs' is the same. We are
     prepared to return one number only, so we just return the first int.
     This matches the behaviour of glibc >= 2.2 on 32-bit platforms. */
  pushSTACK(UL_to_I(*(uintL*)&buf.f_fsid));   /* file system ID */
#endif
  { /* bit mask of f_flag values */
    unsigned long count = 0;
#  ifdef ST_RDONLY
    if (buf.f_flag & ST_RDONLY) { pushSTACK(S(Kread_only)); count++; }
#  endif
#  ifdef ST_NOSUID
    if (buf.f_flag & ST_NOSUID) { pushSTACK(`:NO-SUID`); count++; }
#  endif
#  ifdef ST_NOTRUNC
    if (buf.f_flag & ST_NOTRUNC) { pushSTACK(`:NO-TRUNCATE`); count++; }
#  endif
    if (count) {
      object res = listof(count);
      pushSTACK(res);
    } else pushSTACK(NIL);
  }
  pushSTACK(UL_to_I(buf.f_namemax));/* maximum filename length */
  funcall(`POSIX::MAKE-STAT-VFS`,12);
}

#endif  /* fstatvfs statvfs */


/* FILE-OWNER */

#if defined(UNIX)

static const char *
get_owner (const char *filename)
{
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) {
    struct passwd *pwd = getpwuid(statbuf.st_uid);
    if (pwd)
      return pwd->pw_name;
  }
  return "";
}

#endif

#if defined(WIN32_NATIVE)

#include <windows.h>
#include <aclapi.h>

/* Some functions missing in Windows95/98/ME. */

/* Added in Windows NT 4.0 */
static DWORD WINAPI (*GetSecurityInfoFunc) (HANDLE handle, SE_OBJECT_TYPE ObjectType, SECURITY_INFORMATION SecurityInfo, PSID* ppsidOwner, PSID* ppsidGroup, PACL* ppDacl, PACL* ppSacl, PSECURITY_DESCRIPTOR* ppSecurityDescriptor);
#undef GetSecurityInfo
#define GetSecurityInfo (*GetSecurityInfoFunc)

/* Added in Windows NT Workstation */
static BOOL WINAPI (*LookupAccountSidFunc) (LPCTSTR lpSystemName, PSID lpSid, LPTSTR lpName, LPDWORD cchName, LPTSTR lpReferencedDomainName, LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse);
#undef LookupAccountSid
#define LookupAccountSid (*LookupAccountSidFunc)

/* Added in Windows NT Workstation */
static DWORD WINAPI (*GetLengthSidFunc) (PSID pSid);
#undef GetLengthSid
#define GetLengthSid (*GetLengthSidFunc)

/* Added in Windows NT Workstation */
static BOOL WINAPI (*CopySidFunc) (DWORD nDestinationSidLength, PSID pDestinationSid, PSID pSourceSid);
#undef CopySid
#define CopySid (*CopySidFunc)

/* Added in Windows NT Workstation */
static BOOL WINAPI (*EqualSidFunc) (PSID pSid1, PSID pSid2);
#undef EqualSid
#define EqualSid (*EqualSidFunc)

/* Added in Windows 2000 Professional */
static BOOL WINAPI (*ConvertSidToStringSidFunc) (IN PSID Sid, OUT LPTSTR *StringSid);
#undef ConvertSidToStringSid
#define ConvertSidToStringSid (*ConvertSidToStringSidFunc)

static BOOL initialized_sid_apis = FALSE;

static void
initialize_sid_apis ()
{
  HMODULE advapi32 = LoadLibrary("advapi32.dll");
  if (advapi32 != NULL) {
    GetSecurityInfoFunc =
      (DWORD WINAPI (*) (HANDLE, SE_OBJECT_TYPE, SECURITY_INFORMATION, PSID*, PSID*, PACL*, PACL*, PSECURITY_DESCRIPTOR*))
      GetProcAddress(advapi32, "GetSecurityInfo");
    LookupAccountSidFunc =
      (BOOL WINAPI (*) (LPCTSTR, PSID, LPTSTR, LPDWORD, LPTSTR, LPDWORD, PSID_NAME_USE))
      GetProcAddress(advapi32, "LookupAccountSidA");
    GetLengthSidFunc =
      (DWORD WINAPI (*) (PSID)) GetProcAddress(advapi32, "GetLengthSid");
    CopySidFunc =
      (BOOL WINAPI (*) (DWORD, PSID, PSID)) GetProcAddress(advapi32, "CopySid");
    EqualSidFunc =
      (BOOL WINAPI (*) (PSID, PSID)) GetProcAddress(advapi32, "EqualSid");
    ConvertSidToStringSidFunc =
      (BOOL WINAPI (*) (PSID, LPTSTR*))
      GetProcAddress(advapi32, "ConvertSidToStringSidA");
  }
  initialized_sid_apis = TRUE;
}

/* A cache mapping SID -> owner. */
struct sid_cache_entry {
  PSID psid;
  char *name;
};
static struct sid_cache_entry *sid_cache = NULL;
static size_t sid_cache_count = 0;
static size_t sid_cache_allocated = 0;

static const char *
sid_cache_get (PSID psid)
{
  size_t i;
  for (i = 0; i < sid_cache_count; i++)
    if (EqualSid(psid, sid_cache[i].psid))
      return sid_cache[i].name;
  return NULL;
}

static void
sid_cache_put (PSID psid, const char *name)
{
  if (sid_cache_count == sid_cache_allocated) {
    size_t new_allocated = 2 * sid_cache_allocated + 5;
    sid_cache =
      (sid_cache != NULL
       ? realloc(sid_cache, new_allocated * sizeof(struct sid_cache_entry))
       : malloc(new_allocated * sizeof(struct sid_cache_entry)));
    sid_cache_allocated = (sid_cache == NULL)?0:new_allocated;
  }
  if (sid_cache != NULL) {
    DWORD psid_len = GetLengthSid(psid);
    size_t name_len = strlen(name) + 1;
    char *memory = malloc(psid_len+name_len);
    if (memory == NULL)
      return;
    if (!CopySid(psid_len, memory, psid)) return;
    memcpy(memory+psid_len, name, name_len);
    sid_cache[sid_cache_count].psid = memory;
    sid_cache[sid_cache_count].name = memory + psid_len;
    sid_cache_count++;
  }
}

static const char *
get_owner (const char *filename)
{
  const char *owner;

  if (!initialized_sid_apis)
    initialize_sid_apis();
  owner = "";
  if (GetSecurityInfoFunc != NULL
      && LookupAccountSidFunc != NULL
      && GetLengthSidFunc != NULL
      && CopySidFunc != NULL
      && EqualSidFunc != NULL) {
    /* On Windows, directories don't have an owner. */
    WIN32_FIND_DATA entry;
    HANDLE searchhandle = FindFirstFile(filename, &entry);
    if (searchhandle != INVALID_HANDLE_VALUE) {
      if (!(entry.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
        /* It's a file. */
        HANDLE filehandle =
         CreateFile(filename, GENERIC_READ,
                    FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
        if (filehandle != INVALID_HANDLE_VALUE) {
          /* Get the owner. */
          PSID psid;
          PSECURITY_DESCRIPTOR psd;
          DWORD err =
            GetSecurityInfo(filehandle, SE_FILE_OBJECT,
                            OWNER_SECURITY_INFORMATION,
                            &psid, NULL, NULL, NULL, &psd);
          if (err == 0) {
            owner = sid_cache_get(psid);
            if (owner == NULL) {
              static char buf1[4000];
              DWORD buf1size = sizeof(buf1);
              static char buf2[4000];
              DWORD buf2size = sizeof(buf2);
              SID_NAME_USE role;
              if (!LookupAccountSid(NULL, psid, buf1, &buf1size, buf2, &buf2size, &role)) {
                if (ConvertSidToStringSidFunc != NULL) {
                  /* Fallback: Use S-R-I-S-S... notation.  */
                  char *s;
                  if (!ConvertSidToStringSid(psid, &s)) owner = "";
                  else {
                    strcpy(buf1, s);
                    LocalFree(s);
                    owner = buf1;
                  }
                } else {
                  strcpy(buf1, "");
                  owner = buf1;
                }
              } else { /* DOMAIN\Account */
                int len = strlen(buf2);
                buf2[len] = '\\';
                strcpy(buf2+len+1,buf1);
                owner = buf2;
              }
              sid_cache_put(psid, owner);
            }
            LocalFree(psd);
          }
          CloseHandle(filehandle);
        }
      }
      FindClose(searchhandle);
    }
  }
  return owner;
}

#endif /* WIN32_NATIVE */

DEFUN(OS::FILE-OWNER, file)
{
  object file;
  const char *result;
  file = physical_namestring(STACK_0);
  with_string_0(file,GLO(misc_encoding),filename, {
    begin_system_call();
    result = get_owner(filename);
    end_system_call();
  });
  VALUES1(asciz_to_string(result,GLO(misc_encoding)));
  skipSTACK(1);
}

/* end of FILE-OWNER */

#if defined(WIN32_NATIVE)

/* Pointers to functions unavailable on windows 95, 98, ME */

typedef BOOL (WINAPI * CreateHardLinkFuncType)
  (LPCTSTR lpFileName, LPCTSTR lpExistingFileName,
   LPSECURITY_ATTRIBUTES lpSecurityAttributes);
static CreateHardLinkFuncType CreateHardLinkFunc = NULL;

typedef BOOL (WINAPI * BackupWriteFuncType)
  (HANDLE hFile, LPBYTE lpBuffer, DWORD nNumberOfBytesToWrite,
   LPDWORD lpNumberOfBytesWritten, BOOL bAbort, BOOL bProcessSecurity,
   LPVOID *lpContext);
static BackupWriteFuncType BackupWriteFunc = NULL;
static HMODULE kernel32 = NULL;
#endif

#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
typedef HRESULT (WINAPI * StgOpenStorageExFuncType) (const WCHAR* pwcsName,
            DWORD grfMode, DWORD stgfmt, DWORD grfAttrs, void * reserved1,
            void * reserved2, REFIID riid, void ** ppObjectOpen);
static StgOpenStorageExFuncType StgOpenStorageExFunc = NULL;
static HMODULE ole32 = NULL;
#endif

void module__syscalls__init_function_2 (module_t* module) {
#if defined(WIN32_NATIVE)
  kernel32 = LoadLibrary ("kernel32.dll");
  if (kernel32 != NULL) {
    CreateHardLinkFunc = (CreateHardLinkFuncType)
      GetProcAddress (kernel32, "CreateHardLinkA");
    BackupWriteFunc = (BackupWriteFuncType)
      GetProcAddress (kernel32, "BackupWrite");
  }
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  ole32 =  LoadLibrary ("ole32.dll");
  if (ole32 != NULL)
    StgOpenStorageExFunc = (StgOpenStorageExFuncType)
      GetProcAddress (ole32, "StgOpenStorageEx");
#endif
}

/* COPY-FILE related functions. */

#if defined(WIN32_NATIVE)
/* Checks if it's safe to call OldHardLink */
static BOOL OldHardLinkGuard () {
  OSVERSIONINFO vi;
  if (BackupWriteFunc == NULL) return FALSE;
  vi.dwOSVersionInfoSize = sizeof(vi);
  if (!GetVersionEx(&vi)) return FALSE;
  return vi.dwPlatformId == VER_PLATFORM_WIN32_NT;
}

/* From knowledge base article Q234727
   This approach works on NT >= 3.51. */
static BOOL OldHardLink( LPCTSTR source, LPCTSTR dest ) {

   WCHAR  wsource[ MAX_PATH + 1 ];
   WCHAR  wdest[ MAX_PATH + 1 ];
   WCHAR  wdestfull[ MAX_PATH + 1 ];
   LPWSTR wdestfullfile;

   HANDLE hFileSource;

   WIN32_STREAM_ID StreamId;
   DWORD dwBytesWritten;
   LPVOID lpContext;
   DWORD cbPathLen;
   DWORD StreamHeaderSize;

   BOOL bSuccess;

   /* convert from ANSI to UNICODE */
   if (MultiByteToWideChar(CP_ACP, MB_ERR_INVALID_CHARS /*error on invalid chars*/,
     dest, -1/*null terminated*/, wdest, MAX_PATH + 1) == 0) return FALSE;

   /* open existing file that we link to */
   hFileSource = CreateFile(source, FILE_WRITE_ATTRIBUTES,
     FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
     NULL, /* sa */ OPEN_EXISTING, 0, NULL );

   if (hFileSource == INVALID_HANDLE_VALUE) return FALSE;

   /* validate and sanitize supplied link path and use the result
      the full path MUST be Unicode for BackupWrite */
   cbPathLen = GetFullPathNameW( wdest , MAX_PATH, wdestfull, &wdestfullfile);

   if (cbPathLen == 0) return FALSE;

   cbPathLen = (cbPathLen + 1) * sizeof(WCHAR); // adjust for byte count

   /* prepare and write the WIN32_STREAM_ID out */
   lpContext = NULL;

   StreamId.dwStreamId = BACKUP_LINK;
   StreamId.dwStreamAttributes = 0;
   StreamId.dwStreamNameSize = 0;
   StreamId.Size.HighPart = 0;
   StreamId.Size.LowPart = cbPathLen;

   /* compute length of variable size WIN32_STREAM_ID */
   StreamHeaderSize = (LPBYTE)&StreamId.cStreamName - (LPBYTE)&StreamId
                      + StreamId.dwStreamNameSize ;

   bSuccess = BackupWriteFunc(hFileSource,
                         (LPBYTE)&StreamId,  /* buffer to write */
                         StreamHeaderSize,   /* number of bytes to write */
                         &dwBytesWritten,
                         FALSE,              /* don't abort yet */
                         FALSE,              /* don't process security */
                         &lpContext);
   bSuccess &= BackupWriteFunc(hFileSource,(LPBYTE)wdestfull, cbPathLen,
        &dwBytesWritten, FALSE, FALSE, &lpContext);
   /* free context */
   bSuccess &= BackupWriteFunc(hFileSource,NULL,0,&dwBytesWritten,TRUE, FALSE,
        &lpContext);
   CloseHandle( hFileSource );
   return bSuccess;
}

static inline int MkHardLink (char* old_pathstring, char* new_pathstring) {
  if (CreateHardLinkFunc != NULL)
    return CreateHardLinkFunc(new_pathstring,old_pathstring,NULL);
  if (OldHardLinkGuard())
    return OldHardLink(old_pathstring,new_pathstring);
  SetLastError(ERROR_INVALID_FUNCTION); /* or what ? */
  return 0;
}
#endif

/* Hard/Soft Link a file
 > old_pathstring: old file name, ASCIZ-String
 > new_pathstring: new file name, ASCIZ-String
 > STACK_3: old pathname
 > STACK_1: new pathname */
#if defined(WIN32_NATIVE)
# define HAVE_LINK
#endif
#if defined(HAVE_LINK)
static inline void hardlink_file (char* old_pathstring, char* new_pathstring) {
  begin_system_call();
# if defined(WIN32_NATIVE)
  if (MkHardLink(old_pathstring,new_pathstring) == FALSE)
    if (GetLastError() == ERROR_FILE_NOT_FOUND)
# else
  if (link(old_pathstring,new_pathstring) < 0)
    if (errno==ENOENT)
# endif
      OS_file_error(STACK_3);
    else OS_file_error(STACK_1);
  end_system_call();
}
#endif
#if defined(HAVE_SYMLINK)
static inline void symlink_file (char* old_pathstring, char* new_pathstring) {
  begin_system_call();
  if (symlink(old_pathstring,new_pathstring) < 0) { /* symlink file */
    if (errno==ENOENT) OS_file_error(STACK_3);
    else OS_file_error(STACK_1);
  }
  end_system_call();
}
#endif

/* Copy attributes from stream STACK_1 to stream STACK_0 and close them
   can trigger GC */
static void copy_attributes_and_close () {
# if defined(UNIX)
#  if defined(HAVE_FSTAT)       /* no go without fstat() */
  Handle source_fd = stream_lend_handle(STACK_1,true,NULL);
  Handle dest_fd = stream_lend_handle(STACK_0,true,NULL);
  struct stat source_sb;
  struct stat dest_sb;

  if (fstat(source_fd, &source_sb) == -1) {
    pushSTACK(file_stream_truename(STACK_1));
    goto close_and_err;
  }
  if (fstat(dest_fd, &dest_sb) == -1) {
    pushSTACK(file_stream_truename(STACK_0));
    goto close_and_err;
  }
# if defined(HAVE_CHMOD) /*** file mode ***/
  if (((source_sb.st_mode & 0777) != (dest_sb.st_mode & 0777))
      && (fchmod(dest_fd, source_sb.st_mode & 0777) == -1)) {
    pushSTACK(file_stream_truename(STACK_0));
    goto close_and_err;
  }
# endif

# if defined(HAVE_FCHOWN) /*** owner/group ***/
  if (fchown(dest_fd, source_sb.st_uid, source_sb.st_gid) == -1) {
    pushSTACK(file_stream_truename(STACK_0));
    goto close_and_err;
  }
# endif
# if defined(HAVE_UTIME)
  { /*** access/mod times ***/
    struct utimbuf utb;
    int utime_ret;
    /* first element of the array is access time, second is mod time. set
       both tv_usec to zero since the file system can't gurantee that
       kind of precision anyway. */
    utb.actime  = source_sb.st_atime;
    utb.modtime = source_sb.st_mtime;
    with_string_0(physical_namestring(file_stream_truename(STACK_0)),
                  GLO(pathname_encoding), dest_asciz,
                  { utime_ret = utime(dest_asciz, &utb); });
    if (utime_ret == -1) {
      pushSTACK(file_stream_truename(STACK_0));
      goto close_and_err;
    }
  }
# endif
  goto close_success;
#  endif  /* fstat() */
# else
  /*** FIXME: windows? ***/
# endif
 close_success:
  builtin_stream_close(&STACK_0);
  builtin_stream_close(&STACK_1);
  return;
 close_and_err:
  builtin_stream_close(&STACK_1);
  builtin_stream_close(&STACK_2);
  OS_file_error(STACK_0);
}

/* on success, push (source dest byte-count) on retval (an address in STACK)
 can trigger GC */
static void copy_file_low (object source, object dest,
                           bool preserve_p, if_exists_t if_exists,
                           if_does_not_exist_t if_not_exists,
                           gcv_object_t* retval) {
/* (let ((buffer (make-array buffer-size :element-type 'unsigned-byte)))
    (with-open-file (source-stream source :direction :input
                                   :element-type 'unsigned-byte)
     (with-open-file (dest-stream dest :direction (if append-p :append :output)
                                  :element-type 'unsigned-byte)
       (loop for bytes-read = (read-byte-sequence buffer source-stream)
             until (= 0 bytes-read)
             do (write-byte-sequence buffer dest-stream :end bytes-read)))))
*/
  uintL total_count = 0; /* return value: total byte count */
  /* create the two streams */
  pushSTACK(dest);
  /* input: */
  pushSTACK(source);            /* filename */
  pushSTACK(`:DIRECTION`); pushSTACK(`:INPUT`);
  pushSTACK(`:ELEMENT-TYPE`); pushSTACK(S(unsigned_byte));
  pushSTACK(`:IF-DOES-NOT-EXIST`);
  pushSTACK(if_does_not_exist_symbol(if_not_exists));
  funcall(L(open),7); source = value1;
  if (nullp(source)) {
    skipSTACK(1); /* drop dest */
    return;
  }
  pushSTACK(STACK_0); STACK_1 = source;
  /* stack layout: 1: source stream; 0: dest path */
  /* output: */
  pushSTACK(`:DIRECTION`); pushSTACK(`:OUTPUT`);
  pushSTACK(`:ELEMENT-TYPE`); pushSTACK(S(unsigned_byte));
  pushSTACK(`:IF-EXISTS`); pushSTACK(if_exists_symbol(if_exists));
  funcall(L(open),7); dest = value1;
  if (nullp(dest)) {
    builtin_stream_close(&STACK_0);
    skipSTACK(1); /* drop source */
    return;
  }
  pushSTACK(dest);
  /* stack layout: 0=output stream; 1=input stream */
  { /* make the bit buffer and copy data */
    uintL bytes_read;
    char buffer[strm_buffered_bufflen];
    /* stack layout: 0 - dest-stream; 1 - source-stream */
    Handle fd_in = stream_lend_handle(STACK_1,true,NULL);
    Handle fd_ou = stream_lend_handle(STACK_0,false,NULL);
    while ((bytes_read = fd_read(fd_in,buffer,strm_buffered_bufflen,
                                 persev_full))) {
      total_count += bytes_read;
      fd_write(fd_ou,buffer,bytes_read,persev_full);
    }
  }
  if (!preserve_p) {
    builtin_stream_close(&STACK_0);
    builtin_stream_close(&STACK_1);
  } else
    copy_attributes_and_close();
  /* clean up the stack */
  pushSTACK(allocate_cons());
  Cdr(STACK_0) = *retval;
  *retval = STACK_0;
  STACK_2 = file_stream_truename(STACK_2); /* source */
  STACK_1 = file_stream_truename(STACK_1); /* dest */
  STACK_0 = UL_to_I(total_count);
  Car(*retval) = listof(3);
}

typedef enum {
  COPY_METHOD_COPY,
  COPY_METHOD_SYMLINK,
  COPY_METHOD_HARDLINK,
  COPY_METHOD_RENAME
} copy_method_t;
static inline copy_method_t check_copy_method (object method) {
  if (missingp(method) || eq(method,`:COPY`))
    return COPY_METHOD_COPY;
  else if (eq(method,`:SYMLINK`))
    return COPY_METHOD_SYMLINK;
  else if (eq(method,`:HARDLINK`))
    return COPY_METHOD_HARDLINK;
  else if (eq(method,`:RENAME`))
    return COPY_METHOD_RENAME;
  else {
    pushSTACK(method);           /* TYPE-ERROR slot DATUM */
    pushSTACK(`(MEMBER :HARDLINK :SYMLINK :RENAME :COPY)`); /* EXPECTED-TYPE */
    pushSTACK(method);
    pushSTACK(`:METHOD`);
    pushSTACK(`POSIX::COPY-FILE`);
    fehler(type_error,GETTEXT("~S: ~S illegal ~S argument ~S"));
  }
}
static inline object copy_method_object (copy_method_t method) {
  switch (method) {
    case COPY_METHOD_COPY:     return `:COPY`;
    case COPY_METHOD_SYMLINK:  return `:SYMLINK`;
    case COPY_METHOD_HARDLINK: return `:HARDLINK`;
    case COPY_METHOD_RENAME:   return `:RENAME`;
    default: NOTREACHED;
  }
}

/* copy just one file: source --> dest (both STRINGs, NIL or PATHNAME)
   can trigger GC */
static void copy_one_file (object source, object src_path,
                           object dest, object dest_path,
                           copy_method_t method, bool preserve_p,
                           if_exists_t if_exists,
                           if_does_not_exist_t if_not_exists,
                           gcv_object_t* retval) {
  pushSTACK(source); pushSTACK(src_path);
  pushSTACK(dest); pushSTACK(dest_path);
  XOUT(source,"copy_one_file");
  XOUT(src_path,"copy_one_file");
  XOUT(dest,"copy_one_file");
  XOUT(dest_path,"copy_one_file");
  /* merge source into dest: "cp foo bar/" --> "cp foo bar/foo" */
  pushSTACK(STACK_2); /* src_path */
  funcall(L(merge_pathnames),2); pushSTACK(value1); /* dest_path */

  if (method == COPY_METHOD_COPY) {
    copy_file_low(STACK_2,STACK_0,preserve_p,if_exists,if_not_exists,retval);
    skipSTACK(4);
    return;
  }

  pushSTACK(STACK_0); funcall(L(probe_file),1);
  if (!nullp(value1)) { /* destination exists; value1 == truename */
    pushSTACK(value1); STACK_2 = dest = value1;
    /* STACK: 0=dest_true; 1=dest_path; 2=dest; 3=src_path; 4=src */
    switch (if_exists) {
      case IF_EXISTS_NIL: skipSTACK(5); return;
      case IF_EXISTS_APPEND:
        /* we know that method != COPY_METHOD_COPY - handled above! */
        pushSTACK(`:APPEND`);
        pushSTACK(copy_method_object(method));
        pushSTACK(`POSIX::COPY-FILE`);
        fehler(error,GETTEXT("~S: ~S forbids ~S"));
      case IF_EXISTS_OVERWRITE:
      case IF_EXISTS_SUPERSEDE:
      case IF_EXISTS_RENAME_AND_DELETE:
        /* these are the same since (sym)link/rename are atomic */
        break;
      case IF_EXISTS_UNBOUND: case IF_EXISTS_ERROR:
      case IF_EXISTS_RENAME:    /* delegate to OPEN */
        pushSTACK(value1);      /* destination */
        pushSTACK(`:IF-EXISTS`); pushSTACK(if_exists_symbol(if_exists));
        pushSTACK(`:DIRECTION`); pushSTACK(`:OUTPUT`);
        funcall(L(open),5);
        pushSTACK(value1); builtin_stream_close(&STACK_0);
        funcall(L(delete_file),1);
        break;
      default: NOTREACHED;
    }
  } else pushSTACK(STACK_0); /* destination does not exist, use dest_path */

  pushSTACK(STACK_3); funcall(L(probe_file),1);
  if (nullp(value1)) { /* source does not exist */
    if (method == COPY_METHOD_RENAME || method == COPY_METHOD_HARDLINK) {
      if (if_not_exists == IF_DOES_NOT_EXIST_NIL) {
        skipSTACK(6); return;
      } else { /* delegate error to OPEN */
        pushSTACK(STACK_3);     /* source */
        pushSTACK(`:IF-DOES-NOT-EXIST`);
        pushSTACK(if_does_not_exist_symbol(if_not_exists));
        pushSTACK(`:DIRECTION`); pushSTACK(`:INPUT`);
        funcall(L(open),5);
        NOTREACHED;
      }
    }
  } else {
    pushSTACK(value1); funcall(L(truename),1); pushSTACK(value1);
  }

  /* stack layout: 0=src_true; 1=dest_true ... */
  switch (method) {
    case COPY_METHOD_RENAME:
      pushSTACK(STACK_0); pushSTACK(STACK_2); funcall(L(rename_file),2);
      source = STACK_4; dest = STACK_1;
      break;
    case COPY_METHOD_SYMLINK:
#    if defined(HAVE_SYMLINK)
      dest = physical_namestring(STACK_1);
      /* use the original argument, not the truename here,
         so that the user can create relative symlinks */
      source = (stringp(STACK_5) ? (object)STACK_5
                : physical_namestring(STACK_4));
      with_string_0(source, GLO(pathname_encoding), source_asciz, {
        with_string_0(dest, GLO(pathname_encoding), dest_asciz,
                      { symlink_file(source_asciz,dest_asciz); });
      });
      break;
#    endif
      /* FALLTHROUGH if no symlinks */
    case COPY_METHOD_HARDLINK:
#    if defined(HAVE_LINK)
      dest = physical_namestring(STACK_1);
      source = physical_namestring(STACK_0);
      with_string_0(source, GLO(pathname_encoding), source_asciz, {
        with_string_0(dest, GLO(pathname_encoding), dest_asciz,
                      { hardlink_file(source_asciz,dest_asciz); });
      });
      break;
#    endif
      /* FALLTHROUGH if no hardlinks */
    default:
      copy_file_low(STACK_0,STACK_1,preserve_p,if_exists,if_not_exists,retval);
      skipSTACK(6);
      return;
  }
  /* update retval */
  STACK_0 = dest;
  STACK_1 = source;
  STACK_2 = allocate_cons();
  Cdr(STACK_2) = *retval;
  *retval = STACK_2;
  Car(*retval) = listof(2);
  skipSTACK(4);
}

/* (COPY-FILE source target &key method preserve (if-exists :supersede)
              (if-does-not-exist :error))
 source and target are pathname designators (whether or not they
 can be streams is up for debate). if target is missing a name or
 type designator it is taken from source.
 keywords:
 method := :hardlink      ; make a hard link
         | :symlink       ; make a symbolic link
         | :rename        ; move
         | :copy (or nil) ; make a copy
  if the underlying file system does not support a given operation
  a copy is made

 preserve := t ;; preserve as much of source-file's attributes as
               ;; possible
           | nil ;; don't try to preserve source-file's attributes
                 ;; when creating target-file
 for target:
 if-exists := :supersede ;; the existing file is superseded. that is
                         ;; a new file with the same name (and
                         ;; attributes if possible) is created.
            | :error ;; an error of type file-error is signaled.
            | :new-version ;; a new file is created with a larger
                           ;; version number
            | :rename ;; the existing file is renamed to "orig.bak"
            | :append ;; the contents of source-file are appended to
                      ;; the end of target-file
 for source:
 if-does-not-exist := nil ;; do nothing and return nil
                    | :error ;; (default) signal an error
 */
DEFUN(POSIX::COPY-FILE, source target &key METHOD PRESERVE \
      IF-EXISTS IF-DOES-NOT-EXIST)
{
  if_does_not_exist_t if_not_exists = check_if_does_not_exist(STACK_0);
  if_exists_t if_exists = check_if_exists(STACK_1);
  bool preserve_p = (!nullp(STACK_2) && boundp(STACK_2));
  bool wild_source_p, wild_dest_p;
  copy_method_t method = check_copy_method(STACK_3);
  STACK_1 = NIL; /* return value */
  /* stack: 5 - source; 4 - dest */
  pushSTACK(STACK_5); funcall(L(pathname),1); STACK_3 = value1;
  pushSTACK(STACK_3); funcall(L(wild_pathname_p),1);
  wild_source_p = !nullp(value1);
  pushSTACK(STACK_4); funcall(L(pathname),1); STACK_2 = value1;
  pushSTACK(STACK_2); funcall(L(wild_pathname_p),1);
  wild_dest_p = !nullp(value1);
  XOUT(STACK_3,"POSIX::COPY-FILE -- source");
  XOUT(STACK_2,"POSIX::COPY-FILE -- dest");
  if (wild_source_p) {
    pushSTACK(STACK_3);         /* source pathname */
    pushSTACK(`:IF-DOES-NOT-EXIST`); pushSTACK(`:DISCARD`);
    funcall(L(directory),3);
    STACK_0 = value1;
    XOUT(STACK_0,"POSIX::COPY-FILE: source list");
    if (wild_dest_p) {
      while (!nullp(STACK_0)) {
        pushSTACK(Car(STACK_0)); /* truename */
        pushSTACK(STACK_(3+1)); /* source */
        pushSTACK(STACK_(2+2)); /* dest */
        funcall(L(translate_pathname),3);
        copy_one_file(NIL,Car(STACK_0),NIL,value1,method,
                      preserve_p,if_exists,if_not_exists,&STACK_1);
        STACK_0 = Cdr(STACK_0);
      }
    } else { /* non-wild dest, must be a directory */
      pushSTACK(STACK_2); funcall(L(probe_directory),1);
      if (nullp(value1)) {      /* dest is a non-exitent dir */
        pushSTACK(STACK_2); funcall(L(make_dir),1);
      }
      while (!nullp(STACK_0)) {
        copy_one_file(NIL,Car(STACK_0),STACK_4,STACK_2,method,
                      preserve_p,if_exists,if_not_exists,&STACK_1);
        STACK_0 = Cdr(STACK_0);
      }
    }
  } else /* non-wild source */
    copy_one_file(STACK_5,STACK_3,STACK_4,STACK_2,method,preserve_p,
                  if_exists,if_not_exists,&STACK_1);
  VALUES1(STACK_1);
  skipSTACK(6);
}

DEFUN(POSIX::DUPLICATE-HANDLE, old &optional new)
{ /* Lisp interface to dup(2)/dup2(2). */
  Handle new_handle = (Handle)posfixnum_default2(popSTACK(),(uintL)-1);
  Handle old_handle = (Handle)posfixnum_to_L(check_posfixnum(popSTACK()));
  begin_system_call();
  new_handle = handle_dup(old_handle,new_handle);
  end_system_call();
  VALUES1(fixnum(new_handle));
}

#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
#include <shlobj.h>
/* push the 8 members of WIN32_FIND_DATA onto the STACK */
static void wfd_to_stack (WIN32_FIND_DATA *wfd) {
  pushSTACK(UL_to_I(wfd->dwFileAttributes));
  pushSTACK(convert_time_to_universal_w32(&(wfd->ftCreationTime)));
  pushSTACK(convert_time_to_universal_w32(&(wfd->ftLastAccessTime)));
  pushSTACK(convert_time_to_universal_w32(&(wfd->ftLastWriteTime)));
  pushSTACK(UL_to_I(wfd->nFileSizeHigh));
  pushSTACK(UL_to_I(wfd->nFileSizeLow));
  pushSTACK(asciz_to_string(wfd->cFileName,GLO(pathname_encoding)));
  pushSTACK(asciz_to_string(wfd->cAlternateFileName,GLO(pathname_encoding)));
}

DEFUN(POSIX::FILE-INFO, file)
{
  WIN32_FIND_DATA wfd;
  HANDLE hf;
  object file = physical_namestring(STACK_0);
  with_string_0(file, GLO(pathname_encoding), pathz, {
    begin_system_call();
    hf = FindFirstFile(pathz, &wfd);
    end_system_call();
    if (hf == INVALID_HANDLE_VALUE) { OS_file_error(STACK_0); }
  });
  wfd_to_stack(&wfd); funcall(`POSIX::MAKE-FILE-INFO`,8); skipSTACK(1);
  begin_system_call(); FindClose(hf); end_system_call();
}

DEFUN(POSIX::MAKE-SHORTCUT, file &key WORKING-DIRECTORY ARGUMENTS \
      SHOW-COMMAND ICON DESCRIPTION HOT-KEY PATH)
{
  HRESULT hres;
  IShellLink* psl;
  IPersistFile* ppf;
  gcv_object_t *file = &STACK_7;

  /* Get a pointer to the IShellLink interface. */
  begin_system_call();
  hres = CoCreateInstance(&CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
                          &IID_IShellLink, (LPVOID*)&psl);
  if (!SUCCEEDED(hres)) goto fail_none;
  end_system_call();
  if (!missingp(STACK_0)) {     /* PATH */
    object path = check_string(STACK_0);
    with_string_0(path,GLO(pathname_encoding),pathz, {
      begin_system_call();
      hres = psl->lpVtbl->SetPath(psl,pathz);
      if (!SUCCEEDED(hres)) goto fail_psl;
      end_system_call();
    });
  }
  skipSTACK(1);                 /* drop PATH */
  if (!missingp(STACK_0)) {     /* HOT-KEY */
    WORD hot_key = 0;
    object hk = STACK_0;
    BYTE *pb = (BYTE*)&hot_key;
   restart_hot_key:
    if (charp(hk)) hot_key = char_int(hk);
    else while (consp(hk)) {
      if (eq(Car(hk),`:CONTROL`)) pb[1] |= HOTKEYF_CONTROL;
      else if (eq(Car(hk),`:ALT`)) pb[1] |= HOTKEYF_ALT;
      else if (eq(Car(hk),`:EXT`)) pb[1] |= HOTKEYF_EXT;
      else if (eq(Car(hk),`:SHIFT`)) pb[1] |= HOTKEYF_SHIFT;
      else if (charp(Car(hk))) {
        pb[0] = char_int(hk);
        break;
      } else {
        pushSTACK(NIL);         /* no PLACE */
        pushSTACK(hk);          /* TYPE-ERROR slot DATUM */
        pushSTACK(`(MEMBER :ALT :CONTROL :EXT :SHIFT)`); /* EXPECTED-TYPE */
        pushSTACK(STACK_0); pushSTACK(hk); pushSTACK(TheSubr(subr_self)->name);
        check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
        hk = value1;
        goto restart_hot_key;
      }
      hk = Cdr(hk);
    }
    if (pb[0] == 0) {           /* STACK_0 is the HOT-KEY arg */
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,GETTEXT("~S: invalid hotkey spec ~S"));
    }
    begin_system_call();
    hres = psl->lpVtbl->SetHotkey(psl,hot_key);
    if (!SUCCEEDED(hres)) goto fail_psl;
    end_system_call();
  }
  skipSTACK(1);                 /* drop HOT-KEY */
  if (!missingp(STACK_0)) {     /* DESCRIPTION */
    object desc = check_string(STACK_0);
    with_string_0(desc,GLO(pathname_encoding),descz, {
      begin_system_call();
      hres = psl->lpVtbl->SetDescription(psl,descz);
      if (!SUCCEEDED(hres)) goto fail_psl;
      end_system_call();
    });
  }
  skipSTACK(1);                 /* drop DESCRIPTION */
  if (!missingp(STACK_0)) {     /* ICON */
    object icon_name;
    int icon_idx = 0;
    if (consp(STACK_0)) {       /* (file . index) or (file index) */
      icon_name = check_string(Car(STACK_0));
      icon_idx = posfixnum_to_L(check_posfixnum(consp(Cdr(STACK_0))
                                                ? Car(Cdr(STACK_0))
                                                : Cdr(STACK_0)));
    } else icon_name = check_string(STACK_0);
    with_string_0(icon_name,GLO(pathname_encoding),iconz, {
      begin_system_call();
      hres = psl->lpVtbl->SetIconLocation(psl,iconz,icon_idx);
      if (!SUCCEEDED(hres)) goto fail_psl;
      end_system_call();
    });
  }
  skipSTACK(1);                 /* drop ICON */
  if (!missingp(STACK_0)) {     /* SHOW-COMMAND */
    object sc = STACK_0;
    int sci;
   restart_show_command:
    if (eq(sc,`:NORMAL`)) sci = SW_SHOWNORMAL;
    else if (eq(sc,`:MAX`)) sci = SW_SHOWMAXIMIZED;
    else if (eq(sc,`:MIN`)) sci = SW_SHOWMINIMIZED;
    else {
      pushSTACK(NIL);           /* no PLACE */
      pushSTACK(sc);            /* TYPE-ERROR slot DATUM */
      pushSTACK(`(MEMBER :NORMAL :MAX :MIN)`); /* EXPECTED-TYPE */
      pushSTACK(STACK_0); pushSTACK(sc); pushSTACK(TheSubr(subr_self)->name);
      check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
      sc = value1;
      goto restart_show_command;
    }
    begin_system_call();
    hres = psl->lpVtbl->SetShowCmd(psl,sci);
    if (!SUCCEEDED(hres)) goto fail_psl;
    end_system_call();
  }
  skipSTACK(1);                 /* drop SHOW-COMMAND */
  if (!missingp(STACK_0)) {     /* ARGUMENTS */
    object args = check_string(STACK_0);
    with_string_0(args,GLO(pathname_encoding),argz, {
      begin_system_call();
      hres = psl->lpVtbl->SetArguments(psl,argz);
      if (!SUCCEEDED(hres)) goto fail_psl;
      end_system_call();
    });
  }
  skipSTACK(1);                 /* drop ARGUMENTS */
  if (!missingp(STACK_0)) {     /* WORKING-DIRECTORY */
    object wd = check_string(STACK_0);
    with_string_0(wd,GLO(pathname_encoding),wdz, {
      begin_system_call();
      hres = psl->lpVtbl->SetWorkingDirectory(psl,wdz);
      if (!SUCCEEDED(hres)) goto fail_psl;
      end_system_call();
    });
  }
  skipSTACK(1);                 /* drop WORKING-DIRECTORY */
  STACK_0 = physical_namestring(STACK_0); /* pathname */

  begin_system_call();
  hres = psl->lpVtbl->QueryInterface(psl,&IID_IPersistFile,(LPVOID*)&ppf);
  if (!SUCCEEDED(hres)) goto fail_psl;
  { /* Ensure that the string is Unicode & Save the shortcut. */
    WCHAR wsz[MAX_PATH];
    with_string_0(*file, GLO(pathname_encoding), pathz, {
      MultiByteToWideChar(CP_ACP, 0, pathz, -1, wsz, MAX_PATH);
      hres = ppf->lpVtbl->Save(ppf, wsz, TRUE);
      if (!SUCCEEDED(hres)) goto fail_ppf;
    });
  }
  ppf->lpVtbl->Release(ppf);
  psl->lpVtbl->Release(psl);
  end_system_call();
  VALUES1(popSTACK()); return;
 fail_ppf: ppf->lpVtbl->Release(ppf);
 fail_psl: psl->lpVtbl->Release(psl);
 fail_none: end_system_call(); OS_file_error(*file);
}

DEFUN(POSIX::SHORTCUT-INFO, file)
{
  HRESULT hres;
  IShellLink* psl;
  char path[MAX_PATH], wd[MAX_PATH], args[MAX_PATH],
    icon[MAX_PATH], desc[MAX_PATH];
  WIN32_FIND_DATA wfd;
  IPersistFile* ppf;
  gcv_object_t *file = &STACK_0;
  int icon_idx, show_cmd;
  WORD hot_key;

  STACK_0 = physical_namestring(STACK_0);

  /* Get a pointer to the IShellLink interface. */
  begin_system_call();
  hres = CoCreateInstance(&CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
                          &IID_IShellLink, (LPVOID*)&psl);
  if (!SUCCEEDED(hres)) goto fail_none;
  /* Get a pointer to the IPersistFile interface. */
  hres = psl->lpVtbl->QueryInterface(psl,&IID_IPersistFile,(LPVOID*)&ppf);
  if (!SUCCEEDED(hres)) goto fail_psl;
  { /* Ensure that the string is Unicode & Load the shortcut. */
    WCHAR wsz[MAX_PATH];
    with_string_0(STACK_0, GLO(pathname_encoding), pathz, {
      MultiByteToWideChar(CP_ACP, 0, pathz, -1, wsz, MAX_PATH);
      hres = ppf->lpVtbl->Load(ppf, wsz, STGM_READ);
      if (!SUCCEEDED(hres)) goto fail_ppf;
    });
  }
  /* Resolve the link. */
  hres = psl->lpVtbl->Resolve(psl,NULL,0);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 1 path, 2 file info */
  hres = psl->lpVtbl->GetPath(psl,path, MAX_PATH, &wfd, 4/*SLGP_RAWPATH*/);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 3 working directory */
  hres = psl->lpVtbl->GetWorkingDirectory(psl,wd, MAX_PATH);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 4 arguments */
  hres = psl->lpVtbl->GetArguments(psl,args, MAX_PATH);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 5 show command */
  hres = psl->lpVtbl->GetShowCmd(psl,&show_cmd);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 6 icon */
  hres = psl->lpVtbl->GetIconLocation(psl,icon, MAX_PATH, &icon_idx);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 7 description */
  hres = psl->lpVtbl->GetDescription(psl,desc, MAX_PATH);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 8 hot key */
  hres = psl->lpVtbl->GetHotkey(psl,&hot_key);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  ppf->lpVtbl->Release(ppf);
  psl->lpVtbl->Release(psl);
  end_system_call();
  pushSTACK(asciz_to_string(path,GLO(pathname_encoding))); /* 1 */
  wfd_to_stack(&wfd); funcall(`POSIX::MAKE-FILE-INFO`,8);
  pushSTACK(value1);                                    /* 2 */
  pushSTACK(asciz_to_string(wd,GLO(pathname_encoding)));   /* 3 */
  pushSTACK(asciz_to_string(args,GLO(pathname_encoding))); /* 4 */
  switch (show_cmd) {                                   /* 5 */
    case SW_SHOWNORMAL: pushSTACK(`:NORMAL`); break;
    case SW_SHOWMAXIMIZED: pushSTACK(`:MAX`); break;
    case SW_SHOWMINIMIZED: pushSTACK(`:MIN`); break;
    default: NOTREACHED;
  }
  pushSTACK(asciz_to_string(icon,GLO(pathname_encoding)));
  pushSTACK(fixnum(icon_idx));
  { object tmp = listof(2); pushSTACK(tmp); }           /* 6 */
  pushSTACK(asciz_to_string(desc,GLO(pathname_encoding))); /* 7 */
  { int count=0;                                        /* 8 */
    BYTE *pb = (BYTE*)&hot_key;
    if (pb[1] & HOTKEYF_ALT) { pushSTACK(`:ALT`); count++; }
    if (pb[1] & HOTKEYF_CONTROL) { pushSTACK(`:CONTROL`); count++; }
    if (pb[1] & HOTKEYF_EXT) { pushSTACK(`:EXT`); count++; }
    if (pb[1] & HOTKEYF_SHIFT) { pushSTACK(`:SHIFT`); count++; }
    pushSTACK(int_char(pb[0]));
    if (count) { object tmp = listof(count+1); pushSTACK(tmp); }
  }
  funcall(`POSIX::MAKE-SHORTCUT-INFO`,9);
  return;
 fail_ppf: ppf->lpVtbl->Release(ppf);
 fail_psl: psl->lpVtbl->Release(psl);
 fail_none: end_system_call(); OS_file_error(*file);
}

DEFUN(POSIX::SYSTEM-INFO,)
{ /* interface to GetSystemInfo() */
  SYSTEM_INFO si;
  begin_system_call();
  GetSystemInfo(&si);
  end_system_call();
  switch (si.wProcessorArchitecture) {
    case PROCESSOR_ARCHITECTURE_UNKNOWN: pushSTACK(`:UNKNOWN`); break;
    case PROCESSOR_ARCHITECTURE_INTEL:   pushSTACK(`:INTEL`); break;
    case PROCESSOR_ARCHITECTURE_MIPS:    pushSTACK(`:MIPS`); break;
    case PROCESSOR_ARCHITECTURE_ALPHA:   pushSTACK(`:ALPHA`); break;
    case PROCESSOR_ARCHITECTURE_PPC:     pushSTACK(`:PPC`); break;
    case PROCESSOR_ARCHITECTURE_IA64 :   pushSTACK(`:IA64`); break;
    default: pushSTACK(UL_to_I(si.wProcessorArchitecture));
  }
  pushSTACK(UL_to_I(si.dwPageSize));
  pushSTACK(UL_to_I((DWORD)si.lpMinimumApplicationAddress));
  pushSTACK(UL_to_I((DWORD)si.lpMaximumApplicationAddress));
  pushSTACK(UL_to_I(si.dwActiveProcessorMask));
  pushSTACK(UL_to_I(si.dwNumberOfProcessors));
  pushSTACK(UL_to_I(si.dwAllocationGranularity));
  pushSTACK(fixnum(si.wProcessorLevel));
  pushSTACK(fixnum(si.wProcessorRevision));
  funcall(`POSIX::MAKE-SYSTEM-INFO`,9);
}

DEFUN(POSIX::VERSION,)
{ /* interface to GetVersionEx() */
  OSVERSIONINFOEX vi;
  vi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
  begin_system_call();
  if (!GetVersionEx((OSVERSIONINFO*)&vi)) OS_error();
  end_system_call();

  pushSTACK(UL_to_I(vi.dwMajorVersion));
  pushSTACK(UL_to_I(vi.dwMinorVersion));
  pushSTACK(UL_to_I(vi.dwBuildNumber));
  switch (vi.dwPlatformId) {
    case VER_PLATFORM_WIN32s:        pushSTACK(`:S`); break;
    case VER_PLATFORM_WIN32_WINDOWS: pushSTACK(`:WINDOWS`); break;
    case VER_PLATFORM_WIN32_NT:      pushSTACK(`:NT`); break;
    default: pushSTACK(UL_to_I(vi.dwPlatformId));
  }
  pushSTACK(asciz_to_string(vi.szCSDVersion,GLO(misc_encoding)));
  pushSTACK(UL_to_I(vi.wServicePackMajor));
  pushSTACK(UL_to_I(vi.wServicePackMinor));
  { /* wSuiteMask */
    object suites = NIL;
    unsigned int count = 0;
    if (vi.wSuiteMask & VER_SUITE_BACKOFFICE)
      { pushSTACK(`:BACKOFFICE`); count++; }
    if (vi.wSuiteMask & VER_SUITE_DATACENTER)
      { pushSTACK(`:DATACENTER`); count++; }
    if (vi.wSuiteMask & VER_SUITE_ENTERPRISE)
      { pushSTACK(`:ENTERPRISE`); count++; }
    if (vi.wSuiteMask & VER_SUITE_SMALLBUSINESS)
      { pushSTACK(`:SMALLBUSINESS`); count++; }
    if (vi.wSuiteMask & VER_SUITE_SMALLBUSINESS_RESTRICTED)
      { pushSTACK(`:SMALLBUSINESS-RESTRICTED`); count++; }
    if (vi.wSuiteMask & VER_SUITE_TERMINAL)
      { pushSTACK(`:TERMINAL`); count++; }
    if (vi.wSuiteMask & VER_SUITE_PERSONAL)
      { pushSTACK(`:PERSONAL`); count++; }
    if (count) suites = listof(count);
    pushSTACK(suites);
  }
  switch (vi.wProductType) {
    case VER_NT_WORKSTATION:       pushSTACK(`:WORKSTATION`); break;
    case VER_NT_DOMAIN_CONTROLLER: pushSTACK(`:DOMAIN-CONTROLLER`); break;
    case VER_NT_SERVER:            pushSTACK(`:SERVER`); break;
    default: pushSTACK(UL_to_I(vi.wProductType));
  }
  funcall(`POSIX::MAKE-VERSION`,9);
}

DEFUN(POSIX::MEMORY-STATUS,)
{ /* interface to GlobalMemoryStatus() */
#ifdef HAVE_GLOBALMEMORYSTATUSEX
  MEMORYSTATUSEX ms;
  ms.dwLength = sizeof(MEMORYSTATUSEX);
  begin_system_call();
  if (!GlobalMemoryStatusEx(&ms)) OS_error();
  end_system_call();
  pushSTACK(UQ_to_I(ms.ullTotalPhys));
  pushSTACK(UQ_to_I(ms.ullAvailPhys));
  pushSTACK(UQ_to_I(ms.ullTotalPageFile));
  pushSTACK(UQ_to_I(ms.ullAvailPageFile));
  pushSTACK(UQ_to_I(ms.ullTotalVirtual));
  pushSTACK(UQ_to_I(ms.ullAvailVirtual));
#else
  MEMORYSTATUS ms;
  ms.dwLength = sizeof(MEMORYSTATUS);
  begin_system_call(); GlobalMemoryStatus(&ms); end_system_call();
  pushSTACK(UL_to_I(ms.dwTotalPhys));
  pushSTACK(UL_to_I(ms.dwAvailPhys));
  pushSTACK(UL_to_I(ms.dwTotalPageFile));
  pushSTACK(UL_to_I(ms.dwAvailPageFile));
  pushSTACK(UL_to_I(ms.dwTotalVirtual));
  pushSTACK(UL_to_I(ms.dwAvailVirtual));
#endif
  funcall(`POSIX::MKMEMSTAT`,6);
}

/* FILE-PROPERTIES */

#ifndef PIDSI_TITLE
#define PIDSI_TITLE               0x00000002L
#define PIDSI_SUBJECT             0x00000003L
#define PIDSI_AUTHOR              0x00000004L
#define PIDSI_KEYWORDS            0x00000005L
#define PIDSI_COMMENTS            0x00000006L
#define PIDSI_TEMPLATE            0x00000007L
#define PIDSI_LASTAUTHOR          0x00000008L
#define PIDSI_REVNUMBER           0x00000009L
#define PIDSI_EDITTIME            0x0000000aL
#define PIDSI_LASTPRINTED         0x0000000bL
#define PIDSI_CREATE_DTM          0x0000000cL
#define PIDSI_LASTSAVE_DTM        0x0000000dL
#define PIDSI_PAGECOUNT           0x0000000eL
#define PIDSI_WORDCOUNT           0x0000000fL
#define PIDSI_CHARCOUNT           0x00000010L
#define PIDSI_THUMBNAIL           0x00000011L
#define PIDSI_APPNAME             0x00000012L
#define PIDSI_DOC_SECURITY        0x00000013L
#define PID_CODEPAGE	0x1
#define PID_LOCALE	0x80000000
#define PRSPEC_LPWSTR	( 0 )
#define PRSPEC_PROPID	( 1 )
#define STG_E_PROPSETMISMATCHED   0x800300F0L
#endif

/* Pushes corresponding value to STACK */
static int PropVariantToLisp (PROPVARIANT *pvar) {
  if(pvar->vt & VT_ARRAY) {
    pushSTACK(`:ARRAY`);
    return 1;
  }
  if(pvar->vt & VT_BYREF) {
    pushSTACK(`:BYREF`);
    return 1;
  }
  switch(pvar->vt) {
    case VT_EMPTY: pushSTACK(`:EMPTY`); break;
    case VT_NULL:  pushSTACK(`:NULL`);  break;
    case VT_BLOB:  pushSTACK(`:BLOB`);  break;
    case VT_BOOL:  pushSTACK(pvar->boolVal ? T : NIL); break;
    case VT_I1:    pushSTACK(sfixnum(pvar->cVal)); break;
    case VT_UI1:   pushSTACK(fixnum(pvar->bVal)); break;
    case VT_I2:    pushSTACK(sfixnum(pvar->iVal)); break;
    case VT_UI2:   pushSTACK(fixnum(pvar->uiVal)); break;
    case VT_I4:
    case VT_INT:   pushSTACK(L_to_I(pvar->lVal)); break;
    case VT_UI4:
    case VT_UINT:  pushSTACK(UL_to_I(pvar->ulVal)); break;
    case VT_ERROR: pushSTACK(UL_to_I(pvar->scode)); break;
    case VT_I8:    pushSTACK(sint64_to_I(*((sint64 *)&pvar->hVal))); break;
    case VT_CY: {
      double dbl = (*((sint64 *)&pvar->cyVal))/10000.0;
      pushSTACK(c_double_to_DF((dfloatjanus *)&dbl));
    } break;
    case VT_UI8: pushSTACK(uint64_to_I(*((uint64 *)&pvar->uhVal)));  break;
    case VT_R4:  pushSTACK(c_float_to_FF((ffloatjanus *)&pvar->fltVal)); break;
    case VT_R8:  pushSTACK(c_double_to_DF((dfloatjanus *)&pvar->dblVal));break;
    case VT_DATE:pushSTACK(c_double_to_DF((dfloatjanus *)&pvar->date)); break;
    case VT_BSTR:
      pushSTACK(n_char_to_string((const char *)pvar->bstrVal,
                                 *((DWORD *)(((const char *)pvar->bstrVal)-4)),
                                 Symbol_value(S(unicode_16_little_endian))));
      break;
    case VT_LPSTR:
      pushSTACK(asciz_to_string(pvar->pszVal,GLO(misc_encoding)));
      break;
    case VT_LPWSTR:
      pushSTACK(n_char_to_string((const char *)pvar->pwszVal,
                                 wcslen(pvar->pwszVal)*2,
                                 Symbol_value(S(unicode_16_little_endian))));
      break;
    case VT_FILETIME:
      pushSTACK(convert_time_to_universal_w32(&(pvar->filetime))); break;
    case VT_CF: pushSTACK(`:CLIPBOARD-FORMAT`); break;
    default:    pushSTACK(`:NOTIMPLEMENTED`); break;
  }
  return 1;
}
/* popSTACK -> pvar  */
static int LispToPropVariant (PROPVARIANT * pvar) {
  int rv = 0;int sfp = 0;
  VARTYPE typehint = VT_EMPTY;
  if (consp(STACK_0)) {
    /* (KW VALUE) OR (KW NIL) ? */
    if (!nullp(Cdr(STACK_0)) && !nullp(Car(STACK_0))
        && consp(Cdr(STACK_0)) && nullp(Cdr(Cdr(STACK_0)))
        && symbolp(Car(STACK_0))) {
           if (eq(Car(STACK_0),`:I1`)) typehint = VT_I1;
      else if (eq(Car(STACK_0),`:UI1`)) typehint = VT_UI1;
      else if (eq(Car(STACK_0),`:I2`)) typehint = VT_I2;
      else if (eq(Car(STACK_0),`:UI2`)) typehint = VT_UI2;
      else if (eq(Car(STACK_0),`:I4`)) typehint = VT_I4;
      else if (eq(Car(STACK_0),`:INT`)) typehint = VT_INT;
      else if (eq(Car(STACK_0),`:UI4`)) typehint = VT_UI4;
      else if (eq(Car(STACK_0),`:UINT`)) typehint = VT_UINT;
      else if (eq(Car(STACK_0),`:I8`)) typehint = VT_I8;
      else if (eq(Car(STACK_0),`:UI8`)) typehint = VT_UI8;
      else if (eq(Car(STACK_0),`:R4`)) typehint = VT_R4;
      else if (eq(Car(STACK_0),`:R8`)) typehint = VT_R8;
      else if (eq(Car(STACK_0),`:CY`)) typehint = VT_CY;
      else if (eq(Car(STACK_0),`:DATE`)) typehint = VT_DATE;
      else if (eq(Car(STACK_0),`:BSTR`)) typehint = VT_BSTR;
      else if (eq(Car(STACK_0),`:BOOL`)) typehint = VT_BOOL;
      else if (eq(Car(STACK_0),`:ERROR`)) typehint = VT_ERROR;
      else if (eq(Car(STACK_0),`:FILETIME`)) typehint = VT_FILETIME;
      else if (eq(Car(STACK_0),`:LPSTR`)) typehint = VT_LPSTR;
      else if (eq(Car(STACK_0),`:LPWSTR`)) typehint = VT_LPWSTR;
      else { skipSTACK(1); return 0; }
      STACK_0 = Car(Cdr(STACK_0)); /* VALUE */
    } else { skipSTACK(1); return 0; }
  }
  if (stringp(STACK_0)
      && (typehint == VT_EMPTY || typehint == VT_BSTR
          || typehint == VT_LPSTR || typehint == VT_LPWSTR)) {
    if (typehint == VT_EMPTY) {
#    define STG_STRINGS_NONUNICODE
#    ifdef STG_STRINGS_UNICODE
      typehint = VT_LPWSTR;
#    else
      typehint = VT_LPSTR;
#    endif
    }
    do {
      uintL str_len;
      uintL str_offset;
      object str_string = unpack_string_ro(STACK_0,&str_len,&str_offset);
      const chart* ptr1;
      unpack_sstring_alloca(str_string,str_len,str_offset, ptr1=);
      if (typehint == VT_LPWSTR || typehint == VT_BSTR) {
        uintL str_bytelen =
          cslen_f(Symbol_value(S(unicode_16_little_endian)),ptr1,str_len);
        LPWSTR str = SysAllocStringByteLen(NULL,str_bytelen+4);
        if (typehint == VT_BSTR) {
          /* it's ok, SysAllocStringByteLen returns pointer after DWORD */
          *(((DWORD *)str)-1) = (DWORD)str_bytelen;
        }
        cstombs_f(Symbol_value(S(unicode_16_little_endian)),ptr1,
                  str_len,(uintB *)str,str_bytelen);
        ((uintB *)str)[str_bytelen] = '\0';
        ((uintB *)str)[str_bytelen+1] = '\0';
        pvar->pwszVal = str;
        pvar->vt = typehint;
      } else { /* Win XP explorer seems to create ANSI strings. So do we. */
        uintL str_bytelen = cslen_f(GLO(misc_encoding),ptr1,str_len);
        char * str = (char *) SysAllocStringByteLen(NULL, str_bytelen+2);
        cstombs_f(GLO(misc_encoding),ptr1,str_len,(uintB *)str,str_bytelen);
        str[str_bytelen] = '\0';
        pvar->pszVal = str;
        pvar->vt = VT_LPSTR;
      }
      rv = 1;
    } while(0);
  } else if (integerp(STACK_0)) {
    if (typehint == VT_EMPTY) typehint = VT_FILETIME; /* assume FILETIME */
    if (typehint == VT_FILETIME) {
      pvar->vt = VT_FILETIME; rv = 1;
      convert_time_from_universal_w32(STACK_0,&(pvar->filetime));
    } else if (typehint == VT_I1) {
      pvar->vt = typehint; pvar->cVal = I_to_sint8(STACK_0); rv = 1;
    } else if (typehint == VT_UI1) {
      pvar->vt = typehint; pvar->bVal = I_to_uint8(STACK_0); rv = 1;
    } else if (typehint == VT_I2) {
      pvar->vt = typehint; pvar->iVal = I_to_sint16(STACK_0); rv = 1;
    } else if (typehint == VT_UI2) {
      pvar->vt = typehint; pvar->uiVal = I_to_uint16(STACK_0); rv = 1;
    } else if (typehint == VT_I4 || typehint == VT_INT) { /* VT_I4 != VT_INT */
      pvar->vt = typehint; pvar->lVal = I_to_sint32(STACK_0); rv = 1;
    } else if (typehint == VT_UI4 || typehint == VT_UINT) {
      pvar->vt = typehint; pvar->ulVal = I_to_uint32(STACK_0); rv = 1;
    } else if (typehint == VT_ERROR) {
      pvar->vt = typehint; pvar->scode = I_to_uint32(STACK_0); rv = 1;
    } else if (typehint == VT_I8) {
      pvar->vt = typehint;
      *((sint64 *)&pvar->hVal) = I_to_sint64(STACK_0);rv = 1;
    } else if (typehint == VT_UI8) {
      pvar->vt = typehint;
      *((uint64 *)&pvar->uhVal) = I_to_uint64(STACK_0);rv = 1;
    } else if (typehint == VT_CY) {
      sint64 i64 = I_to_uint64(STACK_0);
      pvar->vt = typehint;
      *((uint64 *)&pvar->cyVal) = i64*10000;rv = 1;
    }
  } else if ((sfp = single_float_p(STACK_0)) || double_float_p(STACK_0)) {
    if (typehint == VT_EMPTY) typehint = (sfp?VT_R4:VT_R8);
    if (typehint == VT_R4) {
      if (sfp) {
        pvar->vt = VT_R4;
        pvar->fltVal = 0;
        FF_to_c_float(STACK_0,(ffloatjanus *)&pvar->fltVal);
        rv = 1;
      }
    } else if (typehint == VT_R8) {
      pvar->vt = VT_R8;
      if (sfp) {
        float v = 0;
        FF_to_c_float(STACK_0,(ffloatjanus *)&v);
        pvar->dblVal = v;
      } else {
        pvar->dblVal = 0; /* DF_to_c_double takes only clean doubles */
        DF_to_c_double(STACK_0,(dfloatjanus *)&pvar->dblVal);
      }
      rv = 1;
    } else if (typehint == VT_DATE && double_float_p(STACK_0)) {
      /* A 64-bit floating point number representing the number of days
         (not seconds) since December 31, 1899. For example, January 1, 1900,
         is 2.0, January 2, 1900, is 3.0, and so on). This is stored in the
         same representation as VT_R8. */
      pvar->vt = VT_DATE;
      pvar->date = 0;
      DF_to_c_double(STACK_0,(dfloatjanus *)&pvar->date);
      rv = 1;
    } else if (typehint == VT_CY) {
      double dbl = 0; float v = 0;
      pvar->vt = typehint;
      if (sfp) {
        FF_to_c_float(STACK_0,(ffloatjanus *)&v);
        dbl = v;
      } else {
        DF_to_c_double(STACK_0,(dfloatjanus *)&dbl);
      }
      *((uint64 *)&pvar->cyVal) = (uint64) (dbl*10000 + 0.5);rv = 1;
    }
  } else if (symbolp(STACK_0)) {
    if (typehint == VT_EMPTY && eq(STACK_0,`:EMPTY`)) {
      pvar->vt = VT_EMPTY; rv = 1; } else
    if (typehint == VT_EMPTY && eq(STACK_0,`:NULL`)) {
      pvar->vt = VT_NULL;  rv = 1; } else
    if (typehint == VT_BOOL && eq(STACK_0,NIL)) {
      pvar->vt = VT_BOOL; pvar->boolVal = FALSE;  rv = 1; } else
    if (typehint == VT_BOOL && eq(STACK_0,T)) {
      pvar->vt = VT_BOOL; pvar->boolVal = TRUE;  rv = 1; }
  }
  skipSTACK(1);
  return rv;
}

WINOLEAPI PropVariantClear(PROPVARIANT* pvar);

static PROPID kwtopropid (object kw) {
  if (eq(kw,`:CODEPAGE`)) return PID_CODEPAGE;
  if (eq(kw,`:LOCALE`)) return PID_LOCALE;
  if (eq(kw,`:TITLE`)) return PIDSI_TITLE;
  if (eq(kw,`:SUBJECT`)) return PIDSI_SUBJECT;
  if (eq(kw,`:AUTHOR`)) return PIDSI_AUTHOR;
  if (eq(kw,`:KEYWORDS`)) return PIDSI_KEYWORDS;
  if (eq(kw,`:COMMENTS`)) return PIDSI_COMMENTS;
  if (eq(kw,`:TEMPLATE`)) return PIDSI_TEMPLATE;
  if (eq(kw,`:LASTAUTHOR`)) return PIDSI_LASTAUTHOR;
  if (eq(kw,`:REVNUMBER`)) return PIDSI_REVNUMBER;
  if (eq(kw,`:EDITTIME`)) return PIDSI_EDITTIME;
  if (eq(kw,`:LASTPRINTED`)) return PIDSI_LASTPRINTED;
  if (eq(kw,`:CREATE-DTM`)) return PIDSI_CREATE_DTM;
  if (eq(kw,`:LASTSAVE-DTM`)) return PIDSI_LASTSAVE_DTM;
  if (eq(kw,`:PAGECOUNT`)) return PIDSI_PAGECOUNT;
  if (eq(kw,`:WORDCOUNT`)) return PIDSI_WORDCOUNT;
  if (eq(kw,`:CHARCOUNT`)) return PIDSI_CHARCOUNT;
  if (eq(kw,`:THUMBNAIL`)) return PIDSI_THUMBNAIL;
  if (eq(kw,`:APPNAME`)) return PIDSI_APPNAME;
  if (eq(kw,`:DOC-SECURITY`)) return PIDSI_DOC_SECURITY;
  return (PROPID)-1;
}

/* string -> PROPSPEC */
static void PropSpecSetStr (object str, PROPSPEC * pspec) {
  pspec->ulKind = PRSPEC_LPWSTR;
  { uintL str_len;
    uintL str_offset;
    object str_string = unpack_string_ro(str,&str_len,&str_offset);
    const chart* ptr1;
    unpack_sstring_alloca(str_string,str_len,str_offset, ptr1=);
    { uintL str_bytelen =
        cslen_f(Symbol_value(S(unicode_16_little_endian)),ptr1,str_len);
      pspec->lpwstr = (LPOLESTR) malloc(str_bytelen+2);
      cstombs_f(Symbol_value(S(unicode_16_little_endian)),ptr1,
                str_len,(uintB *)pspec->lpwstr,str_bytelen);
      ((uintB *)pspec->lpwstr)[str_bytelen] = '\0';
      ((uintB *)pspec->lpwstr)[str_bytelen+1] = '\0';
    }
  }
}

/* list (ID STRING) -> PROPSPEC(ID), PROPSPEC(STR)
   STACK may don't match the pattern (then function returns false)
   any of pspec1, pspec2 can be NULL */
static int propspeclistp (object arg, PROPSPEC * pspec1,PROPSPEC * pspec2) {
  /* check if it is (INT STRING) */
  if (consp(arg) && !nullp(Cdr(arg)) && !nullp(Car(arg))
      && consp(Cdr(arg)) && nullp(Cdr(Cdr(arg)))
      && !nullp(Car(Cdr(arg)))
      && (integerp(Car(arg)) || symbolp(Car(arg)))
      && stringp(Car(Cdr(arg)))) {
    /* set pspec1 to ID and pspec2 to STRING */
    if (pspec1) {
      pspec1->ulKind = PRSPEC_PROPID;
      if (integerp(Car(arg)))
        pspec1->propid = I_to_UL(Car(arg));
      else {
        pspec1->propid = kwtopropid(Car(arg));
        if (pspec1->propid == (PROPID) -1)
          return 0;
      }
    }
    if (pspec2)
      PropSpecSetStr(Car(Cdr(arg)),pspec2);
    return 1;
  }
  return 0;
}

/* (keyword, int, list (ID STRING) or string) -> PROPSPEC
   uses malloc to allocate memory for string specifiers
     (when ulKind == PRSPEC_LPWSTR)
   pspec2 can be NULL */
static int PropSpecSet (object arg, PROPSPEC * pspec1, PROPSPEC * pspec2) {
  ZeroMemory(pspec1, sizeof(PROPSPEC));
  if (pspec2) ZeroMemory(pspec2, sizeof(PROPSPEC));
  if (symbolp(arg)) {
    pspec1->ulKind = PRSPEC_PROPID;
    pspec1->propid = kwtopropid(arg);
    if (pspec1->propid == (PROPID) -1) return 0;
    return 1;
  } else if (stringp(arg)) {
    PropSpecSetStr(arg,pspec1);
    return 1;
  } else if (integerp(arg)) {
    pspec1->ulKind = PRSPEC_PROPID;
    pspec1->propid = I_to_UL(arg);
    return 1;
  } else if (propspeclistp(arg,pspec1,pspec2)) return 2;
  return 0;
}

static const char * DecodeHRESULT (HRESULT hres) {
  static char buf[128];
#define msgcase(x) case x: return #x; break;
  switch (hres) {
  msgcase(E_UNEXPECTED)
  msgcase(STG_E_FILENOTFOUND)
  msgcase(STG_E_ACCESSDENIED)
  msgcase(STG_E_INSUFFICIENTMEMORY)
  msgcase(STG_E_INVALIDFUNCTION)
  msgcase(STG_E_REVERTED)
  msgcase(STG_E_INVALIDPARAMETER)
  msgcase(STG_E_INVALIDNAME)
  msgcase(S_FALSE)
  msgcase(STG_E_INVALIDPOINTER)
  msgcase(HRESULT_FROM_WIN32(ERROR_NO_UNICODE_TRANSLATION))
  msgcase(HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED))
  msgcase(STG_E_WRITEFAULT)
  msgcase(STG_E_MEDIUMFULL)
  msgcase(STG_E_PROPSETMISMATCHED)
  }
#undef msgcase
  sprintf(buf,"0x%x",hres);
  return buf;
}

#define with_string_0w(string,wcvar,statement) \
  do { var uintL wcvar##_len;                  \
    var uintL wcvar##_offset;                  \
    var object wcvar##_string = unpack_string_ro(string,&wcvar##_len,&wcvar##_offset); \
    var const chart* ptr1;                     \
    unpack_sstring_alloca(wcvar##_string,wcvar##_len,wcvar##_offset, ptr1=); \
   {var uintL wcvar##_bytelen =                \
     cslen_f(Symbol_value(S(unicode_16_little_endian)),ptr1,wcvar##_len); \
    var DYNAMIC_ARRAY(wcvar##_data,uintB,wcvar##_bytelen+2); \
    cstombs_f(Symbol_value(S(unicode_16_little_endian)),ptr1,\
      wcvar##_len,&wcvar##_data[0],wcvar##_bytelen); \
    wcvar##_data[wcvar##_bytelen] = '\0';            \
    wcvar##_data[wcvar##_bytelen+1] = '\0';          \
    {var WCHAR* wcvar = (WCHAR*) &wcvar##_data[0];   \
     statement                                       \
    }                                                \
    FREE_DYNAMIC_ARRAY(wcvar##_data);                \
  }} while(0)

/* there's no PropVariantInit in my cygwin headers */
#define MyPropVariantInit(ppv) ZeroMemory(ppv,sizeof(PROPVARIANT))

/* (OS::FILE-PROPERTIES filename set [specifier value|:INITID init-id]*)
     Wrapper for Win32 IPropertyStorage functionality
     filename - a compound file name or (on NTFS) name of any file
     set      - :BUILT-IN or :USER-DEFINED property set
     specifier - property specifier: integer, keyword, string or
       list of integer or keyword and string.
       Integer specifier - a property identifier
       Keyword:  :CODEPAGE, :LOCALE,   :TITLE, :SUBJECT, :AUTHOR,
                 :KEYWORDS, :COMMENTS, :TEMPLATE, :LASTAUTHOR,
                 :REVNUMBER, :EDITTIME, :LASTPRINTED,:CREATE-DTM,
                 :LASTSAVE-DTM, :PAGECOUNT, :WORDCOUNT, :CHARCOUNT,
                 :THUMBNAIL, :APPNAME, :DOC-SECURITY - predefined IDs.
       String: string property specifier. If no match is found, first
         ID >= init-id (which defaults to 2) is associated with the
         string and it's value is replaced with new value.
       (int|keyword string) - first element is used as specifier,
         string is associated with this ID.
     value - new value of the property, suitable lisp object, nil or list of
       keyword and value itself. If value is NIL, no assignment is done.
       :EMPTY and :NULL correspond VT_EMPTY and VT_NULL datatypes.
       Keyword in the list specifies the desired type of property being set.
       Supported types are :I1, :UI1, :I2, :UI2, :I4, :UI4, :UINT, :I8,
         :UI8, :R4, :R8, :DATE, :BSTR, :BOOL, :ERROR, :FILETIME,
         :LPSTR, :LPWSTR. FILETIMEs are converted to/from universal time format,
         while DATEs are not.

     returns multiple values - property contents before assignment. */
DEFUN(POSIX::FILE-PROPERTIES, file set &rest pairs)
{
  /* TODO: close interfaces even on errors;
           support more datatypes
           use IPropertySetStorage::Create when it doesn't exist */
  IPropertyStorage * ppropstg = NULL;
  IPropertySetStorage * ppropsetstg = NULL;
  HRESULT hres;
  REFFMTID  fmtid = NULL;
  PROPSPEC * pspecrd = NULL;
  PROPSPEC * pspecwr = NULL;
  PROPVARIANT * pvarrd = NULL;
  PROPVARIANT * pvarwr = NULL;
  PROPID * propidwpnvec = NULL; /* args for WritePropertyNames */
  LPWSTR * lpwstrwpnvec = NULL;
  int ifile = argcount + 1;
  int iset = argcount;
  int i;
  unsigned int initid = 2;
  int use_wpn = 0; /* should WritePropertyNames be used ? */
  int nproprd = 0, npropwr = 0; /* npropd >= npropwr */
  int cproprd = 0, cpropwr = 0;
  int cwpnpar = 0;
  /* argcount is (length pairs), not the total arg count */
  /* no &rest ? no sense. */
  if (argcount == 0) {
    skipSTACK(2);
    VALUES0;
    return;
  }
  /* count the number of r/rw props, checking arglist sanity */
  if (argcount % 2)
    fehler_key_odd(argcount,TheSubr(subr_self)->name);
  for(i=argcount-1;i>=0;i--) {
    if (i % 2) { /* specifier */
      if (!symbolp(STACK_(i)) && !stringp(STACK_(i))
          && !posfixnump(STACK_(i))) {
        if (!propspeclistp(STACK_(i),NULL,NULL)) {
          pushSTACK(TheSubr(subr_self)->name);
          fehler(program_error,
            GETTEXT("~S: bad property specifier - it must be string, "
                    "positive number, list or keyword"));
        } else { use_wpn++; nproprd++; }
      } else if (symbolp(STACK_(i)) && eq(STACK_(i),`:INITID`)) initid = 0;
      else nproprd++;
    } else { /* value */
      if (!initid) {
        if (integerp(STACK_(i))) initid = I_to_UL(STACK_(i));
        else {
          pushSTACK(STACK_(i));
          pushSTACK(TheSubr(subr_self)->name);
          fehler(program_error,GETTEXT("~S: bad INITID specifier: ~S"));
        }
      } else if (!nullp(STACK_(i))) npropwr++;
    }
  }
  if (!StgOpenStorageExFunc) {
    SetLastError(ERROR_INVALID_FUNCTION);
    OS_error();
  }
  STACK_(ifile) = physical_namestring(STACK_(ifile));
  with_string_0w(STACK_(ifile), filename, {
    hres = StgOpenStorageExFunc(filename,
      ((npropwr||use_wpn)?STGM_READWRITE:STGM_READ) | STGM_SHARE_EXCLUSIVE,
      4 /* STGFMT_ANY */, 0, NULL /*&stgOp*/, 0, &IID_IPropertySetStorage,
      (void **)&ppropsetstg);
  });
  if (FAILED(hres)) {
    pushSTACK(STACK_(ifile));
    pushSTACK(TheSubr(subr_self)->name);
    switch(hres) {
      case STG_E_FILENOTFOUND:
        fehler(file_error,GETTEXT("~S: file ~S does not exist"));
      case STG_E_FILEALREADYEXISTS:
        fehler(file_error,GETTEXT("~S: file ~S is not a compound file nor it is on the NTFS file system"));
      default:
        fehler(file_error,GETTEXT("~S: StgOpenStorageEx() failed on file ~S"));
    }
  }
  if (eq(STACK_(iset),`:USER-DEFINED`))
    fmtid = (REFFMTID)&FMTID_UserDefinedProperties;
  else if (eq(STACK_(iset),`:BUILT-IN`))
    fmtid = (REFFMTID)&FMTID_SummaryInformation;
  else {
    pushSTACK(STACK_(iset));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(file_error,GETTEXT("~S: invalid property set specifier ~S"));
  }
  hres = ppropsetstg->lpVtbl->Open(ppropsetstg, fmtid,
                                   ((npropwr||use_wpn)?STGM_READWRITE:STGM_READ)
                                   | STGM_SHARE_EXCLUSIVE, &ppropstg);
  if (FAILED(hres)) {
    pushSTACK(asciz_to_string(DecodeHRESULT(hres),GLO(misc_encoding)));
    pushSTACK(STACK_(ifile+1));
    pushSTACK(STACK_(iset+2));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(file_error,GETTEXT("~S: unable to open ~S IPropertySetStorage of ~S: error ~S"));
  }
  /* fill the specifiers, init the variables */
  pspecrd = (PROPSPEC *)malloc(sizeof(PROPSPEC) * nproprd);
  pvarrd = (PROPVARIANT *)malloc(sizeof(PROPVARIANT) * nproprd);
  pspecwr = (PROPSPEC *)malloc(sizeof(PROPSPEC) * npropwr);
  pvarwr = (PROPVARIANT *)malloc(sizeof(PROPVARIANT) * npropwr);
  if (use_wpn) {
    propidwpnvec = (PROPID *)malloc(sizeof(PROPID)*use_wpn);
    lpwstrwpnvec = (LPWSTR *)malloc(sizeof(LPWSTR)*use_wpn);
  }
  if (pspecrd == NULL || pvarrd == NULL
      || pspecwr == NULL || pvarwr == NULL
      || use_wpn && (propidwpnvec == NULL || lpwstrwpnvec == NULL)) {
    free(pspecrd); free(pvarrd);
    free(pspecwr); free(pvarwr);
    free(propidwpnvec);free(lpwstrwpnvec);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(storage_condition,GETTEXT("~S: no memory"));
  }
  for(i=0;i<argcount;i+=2) {
    /* i+1 - specifier, i - value */
    PROPSPEC second;
    int pssresult;
    if (symbolp(STACK_(i+1)) && eq(STACK_(i+1),`:INITID`)) continue;
    pssresult = PropSpecSet(STACK_(i+1),pspecrd+nproprd-cproprd-1,&second);
    MyPropVariantInit(pvarrd+nproprd-cproprd-1);
    if (!nullp(STACK_(i))) {
      PropSpecSet(STACK_(i+1),pspecwr+npropwr-cpropwr-1,NULL);
      MyPropVariantInit(pvarwr+npropwr-cpropwr-1);
      pushSTACK(STACK_(i));
      if (!LispToPropVariant(pvarwr+npropwr-cpropwr-1)) {
        pushSTACK(STACK_(i));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,GETTEXT("~S: cannot convert ~S to PROPVARIANT"));
      }
      cpropwr++;
    }
    if (use_wpn && pssresult == 2) {
      propidwpnvec[cwpnpar] = pspecrd[nproprd-cproprd-1].propid;
      lpwstrwpnvec[cwpnpar] = second.lpwstr;
      cwpnpar++;
    }
    cproprd++;
  }
  hres = ppropstg->lpVtbl->ReadMultiple(ppropstg,nproprd, pspecrd, pvarrd);
  if(FAILED(hres)) {
    pushSTACK(asciz_to_string(DecodeHRESULT(hres),GLO(misc_encoding)));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: ReadMultiple error: ~S"));
  }
  if (npropwr > 0) {
    hres = ppropstg->lpVtbl->WriteMultiple(ppropstg,npropwr,pspecwr,pvarwr,initid);
    if(FAILED(hres)) {
      pushSTACK(asciz_to_string(DecodeHRESULT(hres),GLO(misc_encoding)));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,GETTEXT("~S: WriteMultiple error: ~S"));
    }
  }
  for (i=0;i<nproprd;i++)
    if (!PropVariantToLisp(pvarrd+i)) {
      pushSTACK(fixnum(i));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,GETTEXT("~S: cannot convert value ~S to Lisp datatype"));
    }
  if (use_wpn) {
    hres = ppropstg->lpVtbl->WritePropertyNames(ppropstg,use_wpn,propidwpnvec,lpwstrwpnvec);
    if (FAILED(hres)) {
      pushSTACK(asciz_to_string(DecodeHRESULT(hres),GLO(misc_encoding)));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,GETTEXT("~S: WritePropertyNames: ~S"));
    }
  }
  if (sizeof(mv_space)/sizeof(mv_space[0]) < nproprd) {
    pushSTACK(TheSubr(subr_self)->name);
    fehler(program_error,GETTEXT("~S: multiple value count limit reached"));
  }
  mv_count = nproprd;
  for (i=0;i<nproprd;i++) mv_space[nproprd-i-1] = popSTACK();
  skipSTACK(argcount+2); /* two first args */
  for (i=0;i<nproprd;i++) {
    PropVariantClear(pvarrd+i);
    if (pspecrd[i].ulKind == PRSPEC_LPWSTR) free(pspecrd[i].lpwstr);
  }
  for (i=0;i<npropwr;i++) {
    if (pvarwr[i].vt == VT_LPWSTR || pvarwr[i].vt == VT_BSTR)
      SysFreeString(pvarwr[i].pwszVal);
    if (pvarwr[i].vt == VT_LPSTR)
      SysFreeString((BSTR)pvarwr[i].pszVal);
    if (pspecwr[i].ulKind == PRSPEC_LPWSTR) free(pspecwr[i].lpwstr);
  }
  for (i=0;i<use_wpn;i++) free(lpwstrwpnvec[i]);
  free(pspecrd); free(pvarrd); free(pspecwr); free(pvarwr);
  free(propidwpnvec); free(lpwstrwpnvec);
  ppropstg->lpVtbl->Release(ppropstg);
  ppropsetstg->lpVtbl->Release(ppropsetstg);
}
#endif  /* WIN32_NATIVE || UNIX_CYGWIN32 */
