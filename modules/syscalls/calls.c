/*
 * system calls
 * Copyright (C) 2003 Sam Steingold
 * GPL2
 */

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
#if defined(_WIN32)
/* need this for CreateHardLink to work */
# define WINVER 0x0500
/* get ASCII functions */
# undef UNICODE
#endif

#include "clisp.h"

/* #define DEBUG */
#if defined(DEBUG)
# include <stdio.h>
extern object nobject_out (FILE* stream, object obj);
# define XOUT(obj,label)                                                \
  (printf("[%s:%d] %s: %s:\n",__FILE__,__LINE__,STRING(obj),label),     \
   obj=nobject_out(stdout,obj), printf("\n"))
#else
# undef OBJECT_OUT
# define OBJECT_OUT(o,l)
# define XOUT(o,l)
#endif

DEFMODULE(syscalls,"POSIX");

#if CLISP_UNICODE
DEFVAR(misc_encoding, (funcall(L(misc_encoding),0),value1));
DEFVAR(pathname_encoding, (funcall(L(pathname_encoding),0),value1));
#endif

#if defined(HAVE_FLOCK)

#if defined(HAVE_SYS_FILE_H)
# include <sys/file.h>
#endif

DEFUN(POSIX::STREAM-LOCK, stream lockp &key BLOCK SHARED)
{ /* the interface to flock(2) */
  Handle fd = (Handle)-1;
  bool lock_p = !nullp(STACK_2), failed_p;
  int operation = !lock_p ? LOCK_UN
    : (nullp(STACK_1) || eq(unbound,STACK_1) ? LOCK_EX : LOCK_SH);
  object stream = nullobj;
  if (nullp(STACK_0)) operation |= LOCK_NB;
  if (posfixnump(STACK_3)) fd = posfixnum_to_L(STACK_3) ;
  else stream = open_file_stream_handle(STACK_3,&fd);
  begin_system_call();
  failed_p = flock(fd,operation);
  end_system_call();
  if (failed_p) {
    if (eq(stream,nullobj)) OS_error();
    else OS_filestream_error(stream);
  }
  skipSTACK(4);
  VALUES_IF(lock_p);
}

#endif

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
  { double x=n; dfloatjanus t=*(dfloatjanus*)&x; v=c_double_to_DF(&t); }
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


/* ========= SYSTEM INFORMATION ========== */

#if defined(HAVE_SYSCONF) || defined(HAVE_UNAME)
#if defined(HAVE_SYS_UTSNAME_H)
# include <sys/utsname.h>
#endif

/* Lisp interface to uname(2) & sysconf(3c) */
DEFUN(POSIX::SYSINFO-INTERNAL,)
{ /* if you modify this function wrt it's return values,
 you should modify POSIX:SYSINFO in posix.lisp accordingly */
  long res, count = 0;

#if defined(HAVE_UNAME)
  struct utsname utsname;
  begin_system_call(); uname(&utsname); end_system_call();
#define UN(str) pushSTACK(asciz_to_string(str,O(misc_encoding))); count++;
  UN(utsname.sysname);
  UN(utsname.nodename);
  UN(utsname.release);
  UN(utsname.version);
  UN(utsname.machine);
#undef UN
#endif /* HAVE_UNAME */

#if defined(HAVE_SYSCONF)
#define SC_S(cmd) \
  begin_system_call(); res = sysconf(cmd); end_system_call(); \
  pushSTACK(res == -1 ? T : L_to_I(res)); count++;

#if defined(_SC_PAGESIZE)
  SC_S(_SC_PAGESIZE);
#else
  pushSTACK(NIL); count++;
#endif
#if defined(_SC_PHYS_PAGES)
  SC_S(_SC_PHYS_PAGES);
#else
  pushSTACK(NIL); count++;
#endif
#if defined(_SC_AVPHYS_PAGES)
  SC_S(_SC_AVPHYS_PAGES);
#else
  pushSTACK(NIL); count++;
#endif
#if defined(_SC_NPROCESSORS_CONF)
  SC_S(_SC_NPROCESSORS_CONF);
#else
  pushSTACK(NIL); count++;
#endif
#if defined(_SC_NPROCESSORS_ONLN)
  SC_S(_SC_NPROCESSORS_ONLN);
#else
  pushSTACK(NIL); count++;
#endif
#if defined(_SC_THREAD_THREADS_MAX)
  SC_S(_SC_THREAD_THREADS_MAX);
#else
  pushSTACK(NIL); count++;
#endif
#undef SC_S
#endif /* HAVE_SYSCONF */
  funcall(L(values),count);
}
#endif  /* HAVE_SYSCONF || HAVE_UNAME */

#if defined(HAVE_GETRUSAGE) || defined(HAVE_GETRLIMIT)
#if defined(HAVE_SYS_RESOURCE_H)
# include <sys/resource.h>
#endif

#if defined(HAVE_GETRUSAGE)
#define GETRU(who)                                              \
  begin_system_call(); getrusage(who,&ru); end_system_call();   \
  pushSTACK(L_to_I(ru.ru_utime.tv_sec));  count++;              \
  pushSTACK(L_to_I(ru.ru_utime.tv_usec)); count++;              \
  pushSTACK(L_to_I(ru.ru_stime.tv_sec));  count++;              \
  pushSTACK(L_to_I(ru.ru_stime.tv_usec)); count++;              \
  pushSTACK(L_to_I(ru.ru_maxrss));        count++;              \
  pushSTACK(L_to_I(ru.ru_idrss));         count++;              \
  pushSTACK(L_to_I(ru.ru_minflt));        count++;              \
  pushSTACK(L_to_I(ru.ru_majflt));        count++;              \
  pushSTACK(L_to_I(ru.ru_nswap));         count++;              \
  pushSTACK(L_to_I(ru.ru_inblock));       count++;              \
  pushSTACK(L_to_I(ru.ru_oublock));       count++;              \
  pushSTACK(L_to_I(ru.ru_msgsnd));        count++;              \
  pushSTACK(L_to_I(ru.ru_msgrcv));        count++;              \
  pushSTACK(L_to_I(ru.ru_nsignals));      count++;              \
  pushSTACK(L_to_I(ru.ru_nvcsw));         count++;              \
  pushSTACK(L_to_I(ru.ru_nivcsw));        count++
#else
#define GETRU(who) count+=16;                                    \
  pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); \
  pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); \
  pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); \
  pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); \
  pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); \
  pushSTACK(Fixnum_0)
#endif

#if defined(HAVE_GETRLIMIT)
#define RLIM(what)                                                      \
  begin_system_call(); getrlimit(what,&rl); end_system_call(); count += 2; \
  pushSTACK(rl.rlim_cur == RLIM_INFINITY ? T : L_to_I(rl.rlim_cur));    \
  pushSTACK(rl.rlim_max == RLIM_INFINITY ? T : L_to_I(rl.rlim_max))
#endif

DEFUN(POSIX::RESOURCE-USAGE-LIMITS-INTERNAL,)
{ /* if you modify this function wrt its return values,
 you should modify POSIX:RESOURCE-USAGE-LIMITS in posix.lisp accordingly */
  long count = 0;
  struct rlimit rl;
  struct rusage ru;

  GETRU(RUSAGE_SELF);
  GETRU(RUSAGE_CHILDREN);

# undef GETRU

# if defined(HAVE_GETRLIMIT) && defined(RLIMIT_CORE)
  RLIM(RLIMIT_CORE);
# else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
# endif
# if defined(HAVE_GETRLIMIT) && defined(RLIMIT_CPU)
  RLIM(RLIMIT_CPU);
# else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
# endif
# if defined(HAVE_GETRLIMIT) && defined(RLIMIT_DATA)
  RLIM(RLIMIT_DATA);
# else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
# endif
# if defined(HAVE_GETRLIMIT) && defined(RLIMIT_FSIZE)
  RLIM(RLIMIT_FSIZE);
# else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
# endif
# if defined(HAVE_GETRLIMIT) && defined(RLIMIT_NOFILE)
  RLIM(RLIMIT_NOFILE);
# else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
# endif
# if defined(HAVE_GETRLIMIT) && defined(RLIMIT_STACK)
  RLIM(RLIMIT_STACK);
# else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
# endif
# if defined(HAVE_GETRLIMIT) && defined(RLIMIT_VMEM)
  RLIM(RLIMIT_VMEM);
# else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
# endif
# if defined(HAVE_GETRLIMIT) && defined(RLIMIT_RSS)
  RLIM(RLIMIT_RSS);
# else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
# endif
# if defined(HAVE_GETRLIMIT) && defined(RLIMIT_MEMLOCK)
  RLIM(RLIMIT_MEMLOCK);
# else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
# endif

# if defined(RLIM)
#  undef RLIM
# endif

  funcall(L(values),count);
}
#endif /* HAVE_GETRLIMIT || HAVE_GETRUSAGE */

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
 BUF is temporary storage for ipv4_ntop()
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
              asciz_to_string(he->h_aliases[ii],O(misc_encoding)));
  pushSTACK(tmp);
  ARR_TO_LIST(tmp,(he->h_addr_list[ii] != NULL),
              addr_to_string(he->h_addrtype,he->h_addr_list[ii]));
  pushSTACK(tmp);
  pushSTACK(fixnum(he->h_addrtype));
}

/* Lisp interface to gethostbyname(3) and gethostbyaddr(3) */
DEFUN(POSIX::RESOLVE-HOST-IPADDR-INTERNAL,host)
{ /* if you modify this function wrt its return values,
 you should modify POSIX:RESOLVE-HOST-IPADDR in posix.lisp accordingly */
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
      funcall(L(vector),4);
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
    fehler(os_error,"~ (~): ~");
  }

  hostent_to_stack(he);
  funcall(L(values),4);
}

/* ===== PATH ===== */
#if defined(UNIX)

#if defined(HAVE_GETLOGIN) && defined(HAVE_GETPWNAM) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)

#if defined(HAVE_PWD_H)
# include <pwd.h>
#endif

#define PASSWD_TO_STACK(pwd)                                   \
  pushSTACK(asciz_to_string(pwd->pw_name,O(misc_encoding)));   \
  pushSTACK(asciz_to_string(pwd->pw_passwd,O(misc_encoding))); \
  pushSTACK(UL_to_I(pwd->pw_uid));                             \
  pushSTACK(UL_to_I(pwd->pw_gid));                             \
  pushSTACK(asciz_to_string(pwd->pw_gecos,O(misc_encoding)));  \
  pushSTACK(asciz_to_string(pwd->pw_dir,O(misc_encoding)));    \
  pushSTACK(asciz_to_string(pwd->pw_shell,O(misc_encoding)))

/* return the data for the user as 7 values (slots of struct passwd) or
  a list of simple vectors of length 7 if user is NIL.
  if you modify this function wrt its return values, you should modify
  POSIX:USER-DATA in posix.lisp accordingly */
DEFUN(POSIX::USER-DATA-INTERNAL, user) {
  object user = popSTACK();
  struct passwd *pwd = NULL;

# if defined(HAVE_GETPWENT)
  if (nullp(user)) { /* all users as a list */
    int count = 0;
    begin_system_call();
    for (; (pwd = getpwent()); count++) {
      PASSWD_TO_STACK(pwd);
      funcall(L(vector),7);
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
    with_string_0(Symbol_name(user),O(misc_encoding),userz,
                  { pwd = getpwnam(userz); });
  } else if (stringp(user)) {
    with_string_0(user,O(misc_encoding),userz,
                  { pwd = getpwnam(userz); });
  } else {
    end_system_call(); fehler_string_integer(user);
  }
  end_system_call();

  if (NULL == pwd) { OS_error(); }
  PASSWD_TO_STACK(pwd);
  funcall(L(values),7);
}
#endif  /* getlogin getpwent getpwnam getpwuid getuid */

#if defined(HAVE_FSTAT) && defined(HAVE_LSTAT) && defined(HAVE_STAT)

#if defined(HAVE_SYS_STAT_H)
# include <sys/stat.h>
#endif

static object whole_namestring (object path) {
  pushSTACK(path); funcall(L(namestring),1); return value1;
}

/* Lisp interface to stat(2), lstat(2) and fstat(2)
 the first arg can be: file stream, pathname, string, symbol, number.
 the return values are: the file descriptor (int) or the file name
 (string) on which the appropriate stat function was called,
 as well as the 13 slots of the struct stat.
 (POSIX::FILE-STAT-INTERNAL file &optional link-p)
 if you modify this function wrt its return values,
 you should modify POSIX:FILE-STAT in posix.lisp accordingly */
DEFUN(POSIX::FILE-STAT-INTERNAL, file &optional linkp) {
  bool link_p = missingp(STACK_0); skipSTACK(1);
  object file = popSTACK();
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
    file = whole_namestring(file);
    with_string_0(file,O(pathname_encoding),namez, {
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
  funcall(L(values),14);
}
#endif  /* fstat lstat fstat */
#endif /* UNIX */

/* COPY-FILE related functions. */

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
  if (CreateHardLink(new_pathstring,old_pathstring,NULL) == 0) {
    if (GetLastError() == ERROR_FILE_NOT_FOUND)
# else
  if (link(old_pathstring,new_pathstring) < 0) { /* hardlink file */
    if (errno==ENOENT)
# endif
      OS_file_error(STACK_3);
    else OS_file_error(STACK_1);
  }
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
    with_string_0(whole_namestring(file_stream_truename(STACK_0)),
                  O(pathname_encoding), dest_asciz,
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
  /*** FIXME: windows? amiga? riscos? ***/
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
                                     true))) {
      total_count += bytes_read;
      full_write(fd_ou,buffer,bytes_read);
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
    pushSTACK(`(MEMBER :HARDLINK :SYMLINK :RENAME :COPY)`); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(method);
    pushSTACK(`:METHOD`);
    pushSTACK(`POSIX::COPY-FILE`);
    fehler(type_error,GETTEXT("~: ~ illegal ~ argument ~"));
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

/* copy just one file: source --> dest (both strings)
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
  if (!nullp(value1)) { /* destination exists */
    pushSTACK(value1); funcall(L(truename),1);
    pushSTACK(value1); dest = value1; STACK_2 = dest;
    /* STACK: 0=dest_true; 1=dest_path; 2=dest; 3=src_path; 4=src */
    switch (if_exists) {
      case IF_EXISTS_NIL: skipSTACK(5); return;
      case IF_EXISTS_APPEND:
        if (method != COPY_METHOD_COPY) {
          pushSTACK(`:APPEND`);
          pushSTACK(copy_method_object(method));
          pushSTACK(`POSIX::COPY-FILE`);
          fehler(error,GETTEXT("~: ~ forbids ~"));
        }
        break;
      case IF_EXISTS_OVERWRITE:
      case IF_EXISTS_SUPERSEDE:
      case IF_EXISTS_RENAME_AND_DELETE:
        /* these are the same since (sym)link/rename are atomic */
        break;
      case IF_EXISTS_UNBOUND: case IF_EXISTS_ERROR:
      case IF_EXISTS_RENAME:    /* delegate to OPEN */
        pushSTACK(value1);      /* destination */
        pushSTACK(`:IF-EXIST`); pushSTACK(if_exists_symbol(if_exists));
        pushSTACK(`:DIRECTION`); pushSTACK(`:OUTPUT`);
        funcall(L(open),5);
        pushSTACK(value1); builtin_stream_close(&STACK_0);
        funcall(L(delete_file),1);
        break;
      default: NOTREACHED;
    }
  } else pushSTACK(NIL); /* destination does not exist */

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
  dest = STACK_3; /* restore true namestring */
  switch (method) {
    case COPY_METHOD_RENAME:
      pushSTACK(STACK_0); pushSTACK(STACK_2); funcall(L(rename_file),2);
      source = STACK_4; dest = STACK_1;
      break;
    case COPY_METHOD_SYMLINK:
#    if defined(UNIX)
      /* use the original argument, not the truename here,
         so that the user can create relative symlinks */
      source = stringp(STACK_5) ? (object)STACK_5 : whole_namestring(STACK_4);
      with_string_0(source, O(pathname_encoding), source_asciz, {
        with_string_0(dest, O(pathname_encoding), dest_asciz,
                      { symlink_file(source_asciz,dest_asciz); });
      });
      break;
#    endif
      /* FALLTHROUGH if no symlinks */
    case COPY_METHOD_HARDLINK:
#    if defined(UNIX)
      with_string_0(source, O(pathname_encoding), source_asciz, {
        with_string_0(dest, O(pathname_encoding), dest_asciz,
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
DEFUN(POSIX::COPY-FILE, source target &key METHOD PRESERVE IF-EXISTS IF-DOES-NOT-EXIST)
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
