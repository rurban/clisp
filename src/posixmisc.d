# various exported syscalls
# Sam Steingold 1999-2000

#if defined(EXPORT_SYSCALLS)

#include "lispbibl.c"

#if !defined(WIN32_NATIVE)

# =============================================================================

#include <sys/utsname.h>

# Lisp interface to uname(2) & sysconf(3c)
LISPFUNN(sysinfo_,0)
# (POSIX::SYSINFO-INTERNAL)
# if you modify this function wrt it's return values,
# you should modify POSIX:SYSINFO in posix.lisp accordingly
{
  var long res;
  var long count = 0;
  var struct utsname utsname;

  begin_system_call(); uname(&utsname); end_system_call();

#define UN(str) pushSTACK(asciz_to_string(str,O(misc_encoding))); count++;

  UN(utsname.sysname);
  UN(utsname.nodename);
  UN(utsname.release);
  UN(utsname.version);
  UN(utsname.machine);

#undef UN

#define SC_S(cmd) \
  begin_system_call(); res = sysconf(cmd); end_system_call(); \
  pushSTACK(res == -1 ? T : L_to_I(res)); count++;

#ifdef _SC_PAGESIZE
  SC_S(_SC_PAGESIZE);
#else
  pushSTACK(NIL); count++;
#endif
#ifdef _SC_PHYS_PAGES
  SC_S(_SC_PHYS_PAGES);
#else
  pushSTACK(NIL); count++;
#endif
#ifdef _SC_AVPHYS_PAGES
  SC_S(_SC_AVPHYS_PAGES);
#else
  pushSTACK(NIL); count++;
#endif
#ifdef _SC_NPROCESSORS_CONF
  SC_S(_SC_NPROCESSORS_CONF);
#else
  pushSTACK(NIL); count++;
#endif
#ifdef _SC_NPROCESSORS_ONLN
  SC_S(_SC_NPROCESSORS_ONLN);
#else
  pushSTACK(NIL); count++;
#endif
#ifdef _SC_THREAD_THREADS_MAX
  SC_S(_SC_THREAD_THREADS_MAX);
#else
  pushSTACK(NIL); count++;
#endif

#undef SC_S

  funcall(L(values),count);
}

# =============================================================================

#include <sys/resource.h>

#define RU_S \
  pushSTACK(L_to_I(ru.ru_utime.tv_sec));  count++; \
  pushSTACK(L_to_I(ru.ru_utime.tv_usec)); count++; \
  pushSTACK(L_to_I(ru.ru_stime.tv_sec));  count++; \
  pushSTACK(L_to_I(ru.ru_stime.tv_usec)); count++; \
  pushSTACK(L_to_I(ru.ru_maxrss));        count++; \
  pushSTACK(L_to_I(ru.ru_idrss));         count++; \
  pushSTACK(L_to_I(ru.ru_minflt));        count++; \
  pushSTACK(L_to_I(ru.ru_majflt));        count++; \
  pushSTACK(L_to_I(ru.ru_nswap));         count++; \
  pushSTACK(L_to_I(ru.ru_inblock));       count++; \
  pushSTACK(L_to_I(ru.ru_oublock));       count++; \
  pushSTACK(L_to_I(ru.ru_msgsnd));        count++; \
  pushSTACK(L_to_I(ru.ru_msgrcv));        count++; \
  pushSTACK(L_to_I(ru.ru_nsignals));      count++; \
  pushSTACK(L_to_I(ru.ru_nvcsw));         count++; \
  pushSTACK(L_to_I(ru.ru_nivcsw));        count++;

#define GETRU(who) \
 begin_system_call(); getrusage(who,&ru); end_system_call(); RU_S

#define RLIM(what) \
 begin_system_call(); getrlimit(what,&rl); end_system_call(); count += 2; \
 pushSTACK(rl.rlim_cur == RLIM_INFINITY ? T : L_to_I(rl.rlim_cur)); \
 pushSTACK(rl.rlim_max == RLIM_INFINITY ? T : L_to_I(rl.rlim_max));

LISPFUNN(resource_usage_limits_,0)
# (POSIX::RESOURCE-USAGE-LIMITS-INTERNAL)
# if you modify this function wrt its return values,
# you should modify POSIX:RESOURCE-USAGE-LIMITS in posix.lisp accordingly
{
  var long count = 0;
  var struct rlimit rl;
  var struct rusage ru;

  GETRU(RUSAGE_SELF);
  GETRU(RUSAGE_CHILDREN);

#undef GETRU
#undef RU_S

#ifdef RLIMIT_CORE
  RLIM(RLIMIT_CORE);
#else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
#endif
#ifdef RLIMIT_CPU
  RLIM(RLIMIT_CPU);
#else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
#endif
#ifdef RLIMIT_DATA
  RLIM(RLIMIT_DATA);
#else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
#endif
#ifdef RLIMIT_FSIZE
  RLIM(RLIMIT_FSIZE);
#else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
#endif
#ifdef RLIMIT_NOFILE
  RLIM(RLIMIT_NOFILE);
#else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
#endif
#ifdef RLIMIT_STACK
  RLIM(RLIMIT_STACK);
#else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
#endif
#ifdef RLIMIT_VMEM
  RLIM(RLIMIT_VMEM);
#else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
#endif
#ifdef RLIMIT_RSS
  RLIM(RLIMIT_RSS);
#else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
#endif
#ifdef RLIMIT_MEMLOCK
  RLIM(RLIMIT_MEMLOCK);
#else
  pushSTACK(NIL); pushSTACK(NIL); count += 2;
#endif

#undef RLIM

  funcall(L(values),count);
}

#endif
#endif
