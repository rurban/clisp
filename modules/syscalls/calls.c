/*
 * system calls
 * Copyright (C) 2003-2004 Sam Steingold
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
  object stream = nullobj;
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
  if (posfixnump(STACK_5)) fd = (Handle)posfixnum_to_L(STACK_5);
  else stream = open_file_stream_handle(STACK_5,&fd);
  if (missingp(STACK_0)) { /* no :LENGTH => use file size */
    if (posfixnump(STACK_0)) { /* no stream given, use OS to get file size */
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
    } else {
      pushSTACK(stream); funcall(L(file_length),1);
      length = I_to_UL(value1);
    }
  } else length = I_to_L(STACK_0);
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
DEFUNF(POSIX:YN,i y){ VAL_ID(yn); mv_count=1; }

#if defined(HAVE_LGAMMA) || defined(HAVE_LGAMMA_R)
DEFUNF(POSIX::LGAMMA,x) {
# if defined(HAVE_LGAMMA_R)
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

#if defined(HAVE_CRYPT)
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
#if defined(HAVE_ENCRYPT)
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
#if defined(HAVE_SETKEY)
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
DEFCHECKER(sysconf_arg, _SC_AIO_LISTIO_MAX _SC_AIO_MAX _SC_AIO_PRIO_DELTA_MAX \
           _SC_ARG_MAX _SC_ATEXIT_MAX _SC_BC_BASE_MAX _SC_BC_DIM_MAX    \
           _SC_BC_SCALE_MAX _SC_BC_STRING_MAX _SC_CHILD_MAX _SC_CLK_TCK \
           _SC_COLL_WEIGHTS_MAX _SC_DELAYTIMER_MAX _SC_EXPR_NEST_MAX    \
           _SC_HOST_NAME_MAX _SC_IOV_MAX _SC_LINE_MAX _SC_LOGIN_NAME_MAX \
           _SC_NGROUPS_MAX _SC_GETGR_R_SIZE_MAX _SC_GETPW_R_SIZE_MAX    \
           _SC_MQ_OPEN_MAX _SC_MQ_PRIO_MAX _SC_OPEN_MAX _SC_ADVISORY_INFO \
           _SC_BARRIERS _SC_ASYNCHRONOUS_IO _SC_CLOCK_SELECTION _SC_CPUTIME \
           _SC_FSYNC _SC_IPV6 _SC_JOB_CONTROL _SC_MAPPED_FILES _SC_MEMLOCK \
           _SC_MEMLOCK_RANGE _SC_MEMORY_PROTECTION _SC_MESSAGE_PASSING  \
           _SC_MONOTONIC_CLOCK _SC_PRIORITIZED_IO _SC_PRIORITY_SCHEDULING \
           _SC_RAW_SOCKETS _SC_READER_WRITER_LOCKS _SC_REALTIME_SIGNALS \
           _SC_REGEXP _SC_SAVED_IDS _SC_SEMAPHORES _SC_SHARED_MEMORY_OBJECTS \
           _SC_SHELL _SC_SPAWN _SC_SPIN_LOCKS _SC_SPORADIC_SERVER       \
           _SC_SS_REPL_MAX _SC_SYNCHRONIZED_IO _SC_THREAD_ATTR_STACKADDR \
           _SC_THREAD_ATTR_STACKSIZE _SC_THREAD_CPUTIME                 \
           _SC_THREAD_PRIO_INHERIT _SC_THREAD_PRIO_PROTECT              \
           _SC_THREAD_PRIORITY_SCHEDULING _SC_THREAD_PROCESS_SHARED     \
           _SC_THREAD_SAFE_FUNCTIONS _SC_THREAD_SPORADIC_SERVER         \
           _SC_THREADS _SC_TIMEOUTS _SC_TIMERS _SC_TRACE                \
           _SC_TRACE_EVENT_FILTER _SC_TRACE_EVENT_NAME_MAX _SC_TRACE_INHERIT \
           _SC_TRACE_LOG _SC_TRACE_NAME_MAX _SC_TRACE_SYS_MAX           \
           _SC_TRACE_USER_EVENT_MAX _SC_TYPED_MEMORY_OBJECTS _SC_VERSION \
           _SC_V6_ILP32_OFF32 _SC_V6_ILP32_OFFBIG _SC_V6_LP64_OFF64     \
           _SC_V6_LPBIG_OFFBIG _SC_2_C_BIND _SC_2_C_DEV _SC_2_CHAR_TERM \
           _SC_2_FORT_DEV _SC_2_FORT_RUN _SC_2_LOCALEDEF _SC_2_PBS      \
           _SC_2_PBS_ACCOUNTING _SC_2_PBS_CHECKPOINT _SC_2_PBS_LOCATE   \
           _SC_2_PBS_MESSAGE _SC_2_PBS_TRACK _SC_2_SW_DEV _SC_2_UPE     \
           _SC_2_VERSION _SC_PAGESIZE _SC_PHYS_PAGES _SC_AVPHYS_PAGES   \
           _SC_THREAD_DESTRUCTOR_ITERATIONS _SC_THREAD_KEYS_MAX         \
           _SC_THREAD_STACK_MIN _SC_THREAD_THREADS_MAX _SC_RE_DUP_MAX   \
           _SC_RTSIG_MAX _SC_SEM_NSEMS_MAX _SC_SEM_VALUE_MAX _SC_SIGQUEUE_MAX \
           _SC_STREAM_MAX _SC_SYMLOOP_MAX _SC_TIMER_MAX _SC_TTY_NAME_MAX \
           _SC_TZNAME_MAX _SC_XBS5_ILP32_OFF32 _SC_XBS5_ILP32_OFFBIG    \
           _SC_XBS5_LP64_OFF64 _SC_XBS5_LPBIG_OFFBIG _SC_XOPEN_CRYPT    \
           _SC_XOPEN_ENH_I18N _SC_XOPEN_LEGACY _SC_XOPEN_REALTIME       \
           _SC_XOPEN_REALTIME_THREADS _SC_XOPEN_SHM _SC_XOPEN_STREAMS   \
           _SC_XOPEN_UNIX _SC_XOPEN_VERSION                             \
           _SC_NPROCESSORS_CONF _SC_NPROCESSORS_ONLN)
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
DEFCHECKER(confstr_arg,_CS_PATH _CS_POSIX_V6_ILP32_OFF32_CFLAGS         \
           _CS_POSIX_V6_ILP32_OFF32_LDFLAGS _CS_POSIX_V6_ILP32_OFF32_LIBS \
           _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS \
           _CS_POSIX_V6_ILP32_OFFBIG_LIBS _CS_POSIX_V6_LP64_OFF64_CFLAGS \
           _CS_POSIX_V6_LP64_OFF64_LDFLAGS _CS_POSIX_V6_LP64_OFF64_LIBS \
           _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS \
           _CS_POSIX_V6_LPBIG_OFFBIG_LIBS _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS \
           _CS_XBS5_ILP32_OFF32_CFLAGS _CS_XBS5_ILP32_OFF32_LDFLAGS     \
           _CS_XBS5_ILP32_OFF32_LIBS _CS_XBS5_ILP32_OFF32_LINTFLAGS     \
           _CS_XBS5_ILP32_OFFBIG_CFLAGS _CS_XBS5_ILP32_OFFBIG_LDFLAGS   \
           _CS_XBS5_ILP32_OFFBIG_LIBS _CS_XBS5_ILP32_OFFBIG_LINTFLAGS   \
           _CS_XBS5_LP64_OFF64_CFLAGS _CS_XBS5_LP64_OFF64_LDFLAGS       \
           _CS_XBS5_LP64_OFF64_LIBS _CS_XBS5_LP64_OFF64_LINTFLAGS       \
           _CS_XBS5_LPBIG_OFFBIG_CFLAGS _CS_XBS5_LPBIG_OFFBIG_LDFLAGS   \
           _CS_XBS5_LPBIG_OFFBIG_LIBS _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS)
DEFUN(POSIX::CONFSTR, &optional what)
{ /* Lisp interface to confstr(3c) */
#define CS_S(cmd) \
  begin_system_call(); res = confstr(cmd,buf,BUFSIZ); end_system_call(); \
  if (res == 0) pushSTACK(T);                                           \
  else if (res <= BUFSIZ) value1 = asciz_to_string(buf,GLO(misc_encoding)); \
  else {                                                                \
    char *tmp = alloca(res);                                            \
    begin_system_call();                                                \
    confstr(cmd,tmp,res);                                               \
    end_system_call();                                                  \
    value1 = asciz_to_string(tmp,GLO(misc_encoding));                   \
  }

  size_t res;
  char buf[BUFSIZ];
  object what = popSTACK();
  if (!missingp(what)) {
    int cmd = confstr_arg(what);
    CS_S(cmd); mv_count = 1;
  } else { /* all possible values */
    int pos = 0;
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

DEFCHECKER(getrlimit_arg, RLIMIT_CPU RLIMIT_FSIZE RLIMIT_DATA RLIMIT_STACK \
           RLIMIT_CORE RLIMIT_RSS RLIMIT_NOFILE RLIMIT_AS RLIMIT_NPROC  \
           RLIMIT_MEMLOCK RLIMIT_LOCKS)
#if defined(HAVE_GETRLIMIT)
DEFUN(POSIX::LIMITS, &optional what)
{ /* getrlimit(3) */
#define RLIM(what)                                                      \
  begin_system_call();                                                  \
  if (getrlimit(what,&rl)) OS_error();                                  \
  end_system_call();                                                    \
  pushSTACK(rl.rlim_cur == RLIM_INFINITY ? NIL : L_to_I(rl.rlim_cur));  \
  pushSTACK(rl.rlim_max == RLIM_INFINITY ? NIL : L_to_I(rl.rlim_max))

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
}
#endif /* HAVE_GETRLIMIT */
#if defined(HAVE_SETRLIMIT)
DEFUN(POSIX::SET-LIMITS, what cur max)
{ /* setrlimit(3) */
  NOTREACHED;
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

#if defined(UNIX)

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

#if defined(HAVE_FSTAT) && defined(HAVE_LSTAT) && defined(HAVE_STAT)
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
      if (fstat(stream_lend_handle(file,true,NULL),&buf) < 0) OS_error();
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
  pushSTACK(L_to_I(buf.st_rdev));     /* device type (if inode device) */
  pushSTACK(L_to_I(buf.st_size));     /* total size, in bytes */
  pushSTACK(UL_to_I(buf.st_blksize)); /* blocksize for filesystem I/O */
  pushSTACK(UL_to_I(buf.st_blocks));  /* number of blocks allocated */
  pushSTACK(UL_to_I(buf.st_atime+UNIX_LISP_TIME_DIFF));/*time of last access*/
  pushSTACK(UL_to_I(buf.st_mtime+UNIX_LISP_TIME_DIFF));/*last modification*/
  pushSTACK(UL_to_I(buf.st_ctime+UNIX_LISP_TIME_DIFF));/*time of last change*/
  funcall(`POSIX::MAKE-FILE-STAT`,14);
}
#endif  /* fstat lstat fstat */
#if defined(HAVE_CHMOD) && defined(HAVE_CHOWN)
DEFUN(POSIX::SET-FILE-STAT, file &key :MODE :UID :GID)
{ /* interface to chmod(2) and chown(2)
     http://www.opengroup.org/onlinepubs/009695399/functions/chown.html
     http://www.opengroup.org/onlinepubs/009695399/functions/chmod.html */
  gid_t gid = (missingp(STACK_0) ? skipSTACK(1), (gid_t)-1
               : posfixnum_to_L(check_posfixnum(popSTACK())));
  uid_t uid = (missingp(STACK_0) ? skipSTACK(1), (uid_t)-1
               : posfixnum_to_L(check_posfixnum(popSTACK())));
  mode_t mode = (missingp(STACK_0) ? skipSTACK(1), -1
                 : posfixnum_to_L(check_posfixnum(popSTACK())));
  STACK_0 = check_string(STACK_0);
  with_string_0(STACK_0,GLO(pathname_encoding),path, {
      begin_system_call();
      if ((mode != -1) && chmod(path,mode)) OS_file_error(STACK_0);
      if (((uid != (uid_t)-1) || (gid != (gid_t)-1)) && chown(path,uid,gid))
        OS_file_error(STACK_0);
      end_system_call();
    });
  VALUES0;
}
#endif  /* chmod chown */

DEFUN(POSIX::CONVERT-MODE, mode)
{ /* convert between symbolic and numeric permissions */
 convert_mode_restart:
  if (posfixnump(STACK_0)) {
    mode_t mode = posfixnum_to_L(check_posfixnum(popSTACK()));
    int count = 0;
    if (mode & S_ISUID)         /* Set user ID on execution. */
      { pushSTACK(`:SUID`); count++; }
    if (mode & S_ISGID)         /* Set group ID on execution. */
      { pushSTACK(`:SGID`); count++; }
    if (mode & S_ISVTX)  /* On directories, restricted deletion flag, on files,
                            sticky bit: save swapped text even after use */
      { pushSTACK(`:SVTX`); count++; }
    if (mode & S_IRUSR)         /* Read by owner. */
      { pushSTACK(`:RUSR`); count++; }
    if (mode & S_IWUSR)         /* Write by owner. */
      { pushSTACK(`:WUSR`); count++; }
    if (mode & S_IXUSR)         /* Execute (search) by owner. */
      { pushSTACK(`:XUSR`); count++; }
    if (mode & S_IRGRP)         /* Read by group. */
      { pushSTACK(`:RGRP`); count++; }
    if (mode & S_IWGRP)         /* Write by group. */
      { pushSTACK(`:WGRP`); count++; }
    if (mode & S_IXGRP)         /* Execute (search) by group. */
      { pushSTACK(`:XGRP`); count++; }
    if (mode & S_IROTH)         /* Read by others. */
      { pushSTACK(`:ROTH`); count++; }
    if (mode & S_IWOTH)         /* Write by others. */
      { pushSTACK(`:WOTH`); count++; }
    if (mode & S_IXOTH)         /* Execute (search) by others. */
      { pushSTACK(`:XOTH`); count++; }
    VALUES1(listof(count));
  } else if (listp(STACK_0)) {
    mode_t mode = 0;
    pushSTACK(STACK_0);
    while (consp(STACK_0)) {
      object type = Car(STACK_0); STACK_0 = Cdr(STACK_0);
           if (eq(type,`:SUID`)) mode |= S_ISUID;
      else if (eq(type,`:SGID`)) mode |= S_ISGID;
      else if (eq(type,`:SVTX`)) mode |= S_ISVTX;
      else if (eq(type,`:RWXU`)) mode |= S_IRWXU; /* owner: Read Write eXec */
      else if (eq(type,`:RUSR`)) mode |= S_IRUSR;
      else if (eq(type,`:WUSR`)) mode |= S_IWUSR;
      else if (eq(type,`:XUSR`)) mode |= S_IXUSR;
      else if (eq(type,`:RWXG`)) mode |= S_IRWXG; /* group: Read Write eXec */
      else if (eq(type,`:RGRP`)) mode |= S_IRGRP;
      else if (eq(type,`:WGRP`)) mode |= S_IWGRP;
      else if (eq(type,`:XGRP`)) mode |= S_IXGRP;
      else if (eq(type,`:RWXO`)) mode |= S_IRWXO; /* others: Read Write eXec */
      else if (eq(type,`:ROTH`)) mode |= S_IROTH;
      else if (eq(type,`:WOTH`)) mode |= S_IWOTH;
      else if (eq(type,`:XOTH`)) mode |= S_IXOTH;
      else { convert_mode_error:
        STACK_0 = NIL;          /* no PLACE */
        pushSTACK(STACK_1);     /* TYPE-ERROR slot DATUM */
        pushSTACK(`(MEMBER :SUID :SGID :RWXU :RUSR :WUSR :XUSR :RWXG :RGRP :WGRP :XGRP :RWXO :ROTH :WOTH :XOTH :SVTX)`); /* EXPECTED-TYPE */
        pushSTACK(STACK_0); pushSTACK(STACK_2);
        pushSTACK(TheSubr(subr_self)->name);
        check_value(type_error,GETTEXT("~S: ~S is not of type ~S"));
        STACK_0 = value1;
        goto convert_mode_restart;
      }
    }
    skipSTACK(2);               /* drop the tail and the argument */
    VALUES1(fixnum(mode));
  } else {
    pushSTACK(NIL);             /* no PLACE */
    goto convert_mode_error;
  }
}

#if defined(HAVE_UMASK)
DEFUN(POSIX::UMASK, cmask)
{ /* lisp interface to mknod(2)
     http://www.opengroup.org/onlinepubs/009695399/functions/umask.html */
  mode_t cmask = posfixnum_to_L(check_posfixnum(popSTACK()));
  begin_system_call();
  cmask = umask(cmask);
  end_system_call();
  VALUES1(fixnum(cmask));
}
#endif  /* umask */

#if defined(HAVE_MKNOD)
DEFCHECKER(mknod_type_check, S_IFIFO S_IFSOCK S_IFCHR S_IFDIR S_IFBLK S_IFREG)
DEFUN(POSIX::MKNOD, path type mode)
{ /* lisp interface to mknod(2)
     http://www.opengroup.org/onlinepubs/009695399/functions/mknod.html */
  mode_t mode = (posfixnum_to_L(check_posfixnum(popSTACK()))
                 | mknod_type_check(popSTACK()));
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

#endif /* UNIX */

/* COPY-FILE related functions. */

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
}

#if defined(WIN32_NATIVE)
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
# if defined(HAVE_UTIMES)
  { /*** access/mod times ***/
    struct timeval utb[2];
    int utimes_ret;
    /* first element of the array is access time, second is mod time. set
       both tv_usec to zero since the file system can't gurantee that
       kind of precision anyway. */
    utb[0].tv_sec = source_sb.st_atime;
    utb[0].tv_usec = 0;
    utb[1].tv_sec = source_sb.st_mtime;
    utb[1].tv_usec = 0;
    with_string_0(physical_namestring(file_stream_truename(STACK_0)),
                  GLO(pathname_encoding), dest_asciz,
                  { utimes_ret = utimes(dest_asciz, utb); });
    if (utimes_ret == -1) {
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
    while ((bytes_read = read_helper(fd_in,buffer,strm_buffered_bufflen,
                                     false))) {
      total_count += bytes_read;
      write_helper(fd_ou,buffer,bytes_read,false);
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
      source = stringp(STACK_5) ? (object)STACK_5:physical_namestring(STACK_4);
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
  STACK_1 = check_posfixnum(STACK_1);
  if (boundp(STACK_0)) STACK_0 = check_posfixnum(STACK_0);
 {Handle old_handle = (Handle)posfixnum_to_L(STACK_1);
  Handle new_handle = (posfixnump(STACK_0) ? (Handle)posfixnum_to_L(STACK_0)
                       : (Handle)-1);
  begin_system_call();
  new_handle = handle_dup(old_handle,new_handle);
  end_system_call();
  VALUES1(fixnum(new_handle));
}}

#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
#include <shlobj.h>
/* push the 8 members of WIN32_FIND_DATA onto the STACK */
static void wfd_to_stack (WIN32_FIND_DATA *wfd) {
  pushSTACK(UL_to_I(wfd->dwFileAttributes));
#if defined(WIN32_NATIVE)
  pushSTACK(convert_time_to_universal(&(wfd->ftCreationTime)));
  pushSTACK(convert_time_to_universal(&(wfd->ftLastAccessTime)));
  pushSTACK(convert_time_to_universal(&(wfd->ftLastWriteTime)));
#else  /* cygwin */
  { time_t unix_time = to_time_t_(&(wfd->ftCreationTime));
    pushSTACK(convert_time_to_universal(&unix_time));
    unix_time = to_time_t_(&(wfd->ftLastAccessTime));
    pushSTACK(convert_time_to_universal(&unix_time));
    unix_time = to_time_t_(&(wfd->ftLastWriteTime));
    pushSTACK(convert_time_to_universal(&unix_time));
  }
#endif
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
  wfd_to_stack(&wfd); FindClose(hf);
  funcall(`POSIX::MAKE-FILE-INFO`,8); skipSTACK(1);
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

#endif
