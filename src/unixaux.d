# Hilfsfunktionen für CLISP auf UNIX
# Bruno Haible 1990-1999

#include "lispbibl.c"

# ==============================================================================

#ifdef NEED_OWN_UALARM
# Ein Ersatz für die ualarm-Funktion.
  global unsigned int ualarm (unsigned int value, unsigned int interval);
  global unsigned int ualarm(value,interval)
    var unsigned int value;
    var unsigned int interval;
    { var struct itimerval itimer;
      itimer.it_value.tv_sec = floor(value,1000000);
      itimer.it_value.tv_usec = value % 1000000;
      itimer.it_interval.tv_sec = floor(interval,1000000);
      itimer.it_interval.tv_usec = interval % 1000000;
      setitimer(ITIMER_REAL,&itimer,NULL);
      return 0; # den Rückgabewert ignorieren wir immer.
    }
#endif

# ==============================================================================

#ifdef NEED_OWN_SELECT
# Ein Ersatz für die select-Funktion.
  global int select (int width, fd_set* readfds, fd_set* writefds, fd_set* exceptfds, struct timeval * timeout);
  global int select(width,readfds,writefds,exceptfds,timeout)
    var int width;
    var fd_set* readfds;
    var fd_set* writefds;
    var fd_set* exceptfds;
    var struct timeval * timeout;
    { var struct pollfd pollfd_bag[FD_SETSIZE];
      var struct pollfd * pollfd_ptr = &pollfd_bag[0];
      var int pollfd_count = 0;
      if (width<0) { errno = EINVAL; return -1; }
      if (width>FD_SETSIZE) { width = FD_SETSIZE; }
      { var int fd;
        for (fd=0; fd<width; fd++)
          { var short events = 0;
            if (!(readfds==NULL) && FD_ISSET(fd,readfds)) { events |= POLLIN; }
            if (!(writefds==NULL) && FD_ISSET(fd,writefds)) { events |= POLLOUT; }
            if (!(exceptfds==NULL) && FD_ISSET(fd,exceptfds)) { events |= POLLPRI; }
            if (events)
              { pollfd_ptr->fd = fd;
                pollfd_ptr->events = events;
                pollfd_ptr->revents = 0;
                pollfd_ptr++; pollfd_count++;
      }   }   }
     {var int poll_timeout = timeout->tv_sec * 1000 + timeout->tv_usec / (1000000/1000);
      var int result = poll(pollfd_count,&pollfd_bag[0],poll_timeout);
      if (result>=0)
        { pollfd_ptr = &pollfd_bag[0];
          until (pollfd_count == 0)
            { var int fd = pollfd_ptr->fd;
              var short revents = pollfd_ptr->revents;
              if (!(readfds==NULL) && (revents & POLLIN)) { FD_SET(fd,readfds); }
              if (!(writefds==NULL) && (revents & POLLOUT)) { FD_SET(fd,writefds); }
              if (!(exceptfds==NULL) && (revents & (POLLPRI|POLLERR|POLLHUP))) { FD_SET(fd,exceptfds); }
              pollfd_ptr++; pollfd_count--;
        }   }
      return result;
    }}
#endif

# ==============================================================================

#ifdef NEED_OWN_GETTIMEOFDAY
# Ein Ersatz für die gettimeofday-Funktion.
  global int gettimeofday (struct timeval * tp, struct timezone * tzp);
  global int gettimeofday(tp,tzp)
    var struct timeval * tp;
    var struct timezone * tzp;
    { var struct timeb timebuf;
      if (!((tp==NULL) && (tzp==NULL)))
        { ftime(&timebuf);
          if (!(tp==NULL))
            { tp->tv_sec = timebuf.time;
              tp->tv_usec = (long)(timebuf.millitm) * (1000000/1000);
            }
          if (!(tzp==NULL))
            { tzp->tz_minuteswest = timebuf.timezone;
              tzp->tz_dsttime = 0; # ??
            }
        }
      return 0;
    }
#endif

# ==============================================================================

#ifdef NEED_OWN_RENAME
# Ein Ersatz für die rename-Funktion.
  global int rename (const char* oldpath, const char* newpath);
  global int rename(oldpath,newpath)
    var const char* oldpath;
    var const char* newpath;
    { var int result;
      if ((result = access(oldpath,0)) < 0) # oldpath überhaupt da?
        { return result; }
      if ((result = access(newpath,0)) < 0) # newpath auch da?
        { if (!(errno==ENOENT)) return result; }
        else
        { # Überprüfe, ob oldpath und newpath dasselbe sind.
          # Dann darf nämlich nichts gelöscht werden!
          var struct stat oldstatbuf;
          var struct stat newstatbuf;
          if ((result = stat(oldpath,&oldstatbuf)) < 0) { return result; }
          if ((result = stat(newpath,&newstatbuf)) < 0) { return result; }
          if ((oldstatbuf.st_dev == newstatbuf.st_dev)
              && (oldstatbuf.st_ino == newstatbuf.st_ino)
             )
            { return 0; }
          if ((result = unlink(newpath)) < 0) # newpath löschen
            { return result; }
        }
      if ((result = link(oldpath,newpath)) < 0) # newpath neu anlegen
        { return result; }
      if ((result = unlink(oldpath)) < 0) # oldpath kann nun gelöscht werden
        { return result; }
      return 0;
    }
#endif

# ==============================================================================

#ifdef EINTR

#ifdef UNIX # EMUNIX und RISCOS brauchen das nicht

# Ein Wrapper um die open-Funktion.
  global int nonintr_open (OPEN_CONST char* path, int flags, MODE_T mode);
  global int nonintr_open(path,flags,mode)
    var OPEN_CONST char* path;
    var int flags;
    var MODE_T mode;
    { var int retval;
      do { retval = open(path,flags,mode); } while ((retval < 0) && (errno == EINTR));
      return retval;
    }

# Ein Wrapper um die close-Funktion.
  global int nonintr_close (int fd);
  global int nonintr_close(fd)
    var int fd;
    { var int retval;
      do { retval = close(fd); } while ((retval < 0) && (errno == EINTR));
      return retval;
    }

# Ein Wrapper um die ioctl-Funktion.
  #undef ioctl
  global int nonintr_ioctl (int fd, IOCTL_REQUEST_T request, IOCTL_ARGUMENT_T arg);
  global int nonintr_ioctl(fd,request,arg)
    var int fd;
    var IOCTL_REQUEST_T request;
    var IOCTL_ARGUMENT_T arg;
    { var int retval;
      do { retval = ioctl(fd,request,arg); } while ((retval != 0) && (errno == EINTR));
      return retval;
    }

#endif

#ifdef UNIX_TERM_TERMIOS

# Ein Wrapper um die tcsetattr-Funktion.
  global int nonintr_tcsetattr (int fd, int optional_actions, struct termios * tp);
  global int nonintr_tcsetattr(fd,optional_actions,tp)
    var int fd;
    var int optional_actions;
    var struct termios * tp;
    { var int retval;
      do { retval = tcsetattr(fd,optional_actions,tp); }
         while ((retval != 0) && (errno == EINTR));
      return retval;
    }

# Ein Wrapper um die tcdrain-Funktion.
  global int nonintr_tcdrain (int fd);
  global int nonintr_tcdrain(fd)
    var int fd;
    { var int retval;
      do { retval = tcdrain(fd); } while ((retval != 0) && (errno == EINTR));
      return retval;
    }

# Ein Wrapper um die tcflush-Funktion.
  global int nonintr_tcflush (int fd, int flag);
  global int nonintr_tcflush(fd,flag)
    var int fd;
    var int flag;
    { var int retval;
      do { retval = tcflush(fd,flag); } while ((retval != 0) && (errno == EINTR));
      return retval;
    }

#endif

#ifdef NEED_OWN_SIGINTERRUPT

# Ein Ersatz für die siginterrupt-Funktion.
  global int siginterrupt (int sig, int flag);
  #if defined(HAVE_SIGACTION)
    extern_C int sigaction (/* int sig, [const] struct sigaction * new, struct sigaction * old */);
  #elif defined(HAVE_SIGVEC) && defined(SV_INTERRUPT)
    extern_C int sigvec (/* int sig, [const] struct sigvec * new, struct sigvec * old */);
  #endif
  global int siginterrupt (sig,flag)
    var int sig;
    var int flag;
    {
     #if defined(HAVE_SIGACTION)
      var struct sigaction sa;
      sigaction(sig,(struct sigaction *)NULL,&sa);
      #ifdef SA_INTERRUPT
      if (flag)
        { if (sa.sa_flags & SA_INTERRUPT) return 0;
          sa.sa_flags |= SA_INTERRUPT; # system calls will be interrupted
        }
        else
        { if (!(sa.sa_flags & SA_INTERRUPT)) return 0;
          sa.sa_flags &= ~ SA_INTERRUPT; # system calls will be restarted
        }
      #endif
      #ifdef SA_RESTART
      if (flag)
        { if (!(sa.sa_flags & SA_RESTART)) return 0;
          sa.sa_flags &= ~ SA_RESTART; # system calls will be interrupted
        }
        else
        { if (sa.sa_flags & SA_RESTART) return 0;
          sa.sa_flags |= SA_RESTART; # system calls will be restarted
        }
      #endif
      sigaction(sig,&sa,(struct sigaction *)NULL);
     #elif defined(HAVE_SIGVEC) && defined(SV_INTERRUPT)
      var struct sigvec sv;
      sigvec(sig,(struct sigvec *)NULL,&sv);
      if (flag)
        { if (sv.sv_flags & SV_INTERRUPT) return 0;
          sv.sv_flags |= SV_INTERRUPT; # system calls will be interrupted
        }
        else
        { if (!(sv.sv_flags & SV_INTERRUPT)) return 0;
          sv.sv_flags &= ~ SV_INTERRUPT; # system calls will be restarted
        }
      sigvec(sig,&sv,(struct sigvec *)NULL);
     #endif
      return 0; # den Rückgabewert ignorieren wir immer.
    }

#endif

#endif

# Ein Wrapper um die read-Funktion.
  global RETRWTYPE full_read (int fd, RW_BUF_T bufarea, RW_SIZE_T nbyte);
  global RETRWTYPE full_read (fd,bufarea,nbyte)
    var int fd;
    var RW_BUF_T bufarea;
    var RW_SIZE_T nbyte;
    { var char* buf = (char*) bufarea;
      var RETRWTYPE retval;
      var RW_SIZE_T done = 0;
      #if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED)) || defined(SELFMADE_MMAP)
      # Must adjust the memory permissions before calling read().
      # - On SunOS4 a missing write permission causes the read() call to hang
      #   in an endless loop.
      # - With Linux 2.2 the read call returns with errno=EFAULT, but with
      #   unpredictable side side effects: If fd refers to a socket, some of
      #   the socket data gets lost.
      # The SunOS4 behaviour is clearly a bug, but the Linux 2.2 behaviour is
      # not. The POSIX spec says that read() returns with errno=EFAULT, but
      # does not specify anything about possible side effects.
      handle_fault_range(PROT_READ_WRITE,(aint)buf,(aint)buf+nbyte);
      #endif
      until (nbyte==0)
        { retval = read(fd,buf,nbyte);
          if (retval == 0) break;
          elif (retval < 0)
            {
              #ifdef EINTR
              if (!(errno == EINTR))
              #endif
                return retval;
            }
          else { buf += retval; done += (RW_SIZE_T)retval; nbyte -= (RW_SIZE_T)retval; }
        }
      return done;
    }

# Ein Wrapper um die write-Funktion.
  global RETRWTYPE full_write (int fd, WRITE_CONST RW_BUF_T bufarea, RW_SIZE_T nbyte);
  global RETRWTYPE full_write (fd,bufarea,nbyte)
    var int fd;
    var WRITE_CONST RW_BUF_T bufarea;
    var RW_SIZE_T nbyte;
    { var WRITE_CONST char* buf = (WRITE_CONST char*) bufarea;
      var RETRWTYPE retval;
      var RW_SIZE_T done = 0;
      #if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED)) || defined(SELFMADE_MMAP)
      # Must adjust the memory permissions before calling write().
      handle_fault_range(PROT_READ,(aint)buf,(aint)buf+nbyte);
      #endif
      until (nbyte==0)
        { retval = write(fd,buf,nbyte);
          if (retval == 0) break;
          elif (retval < 0)
            {
              #ifdef EINTR
              if (!(errno == EINTR))
              #endif
                return retval;
            }
          else { buf += retval; done += (RW_SIZE_T)retval; nbyte -= (RW_SIZE_T)retval; }
        }
      return done;
    }

#ifdef PID_T

# Auf die Beendingung eines Child-Prozesses warten:
  global int wait2 (PID_T child);
  global int wait2(child)
    var PID_T child;
    { var int status = 0;
      # vgl. WAIT(2V) und #include <sys/wait.h> :
      #   WIFSTOPPED(status)  ==  ((status & 0xFF) == 0177)
      #   WEXITSTATUS(status)  == ((status & 0xFF00) >> 8)
      #ifdef HAVE_WAITPID
      loop
        { var int ergebnis = waitpid(child,&status,0);
          if (!(ergebnis == child))
            { if (ergebnis<0)
                { if (errno==EINTR) continue;
                  #ifdef ECHILD
                  if (errno==ECHILD) # Wenn der Child-Prozess nicht mehr da ist,
                    { status = 0; break; } # ist er wohl korrekt beendet worden.
                  #endif
                }
              OS_error();
            }
          if (!((status & 0xFF) == 0177)) break; # Child-Prozess beendet?
        }
      #else
      loop
        { var int ergebnis = wait(&status);
          if (ergebnis < 0)
            { if (errno==EINTR) continue;
              #ifdef ECHILD
              if (errno==ECHILD) # Wenn der Child-Prozess nicht mehr da ist,
                { status = 0; break; } # ist er wohl korrekt beendet worden.
              #endif
              OS_error();
            }
          if ((ergebnis == child) && !((status & 0xFF) == 0177)) break; # Child-Prozess beendet?
        }
      #endif
      return status;
    }

#endif

# ==============================================================================

#if defined(UNIX)

# This is like the signal() function, except that
# - It uses sigaction() if needed in order to not block other signals,
# - It calls siginterrupt(sig,0) so that these signals avoid to interrupt
#   system calls.
  global signal_handler install_signal_handler (int sig, signal_handler handler);
  global signal_handler install_signal_handler(sig,handler)
    var int sig;
    var signal_handler handler;
    { var signal_handler old_handler;
      #if defined(USE_SIGACTION)
        var struct sigaction old_sa;
        var struct sigaction new_sa;
        #ifdef HAVE_MEMSET
        memset(&new_sa,0,sizeof(new_sa));
        #else
        bzero(&new_sa,sizeof(new_sa));
        #endif
        new_sa.sa_handler = handler;
        # Do not block other signals, except possibly SIGINT and SIGALRM
        # (because our SIGINT/SIGALRM handlers expects the STACK_register
        # to be valid).
        sigemptyset(&new_sa.sa_mask);
        #ifdef HAVE_SAVED_STACK
        if (!(sig == SIGINT || sig == SIGALRM))
          { sigaddset(&new_sa.sa_mask,SIGINT);
            sigaddset(&new_sa.sa_mask,SIGALRM);
          }
        #endif
        # new_sa.sa_mask = 0; # Do not block other signals.
        #ifdef EINTR
        #ifdef SA_RESTART
        new_sa.sa_flags |= SA_RESTART; # system calls will be restarted
        #endif
        #endif
        if (sigaction(sig,&new_sa,&old_sa)<0)
          { old_handler = (signal_handler)SIG_IGN; }
        else
          { old_handler = (signal_handler)old_sa.sa_handler; }
      #else
        old_handler = signal(sig,handler);
        #ifdef EINTR
        siginterrupt(sig,0);
        #endif
      #endif
      return old_handler;
    }
#endif

# ==============================================================================

#if defined(UNIX_LINUX) && (defined(FAST_FLOAT) || defined(FAST_DOUBLE)) && (defined(HAVE_FPU_CONTROL_T) || !defined(HAVE_SETFPUCW))

# Damit Division durch 0.0 ein NaN und kein SIGFPE liefert:
# Entweder mit -lieee linken,
# oder libc-linux/sysdeps/linux/{i386,m68k}/ieee.c kopieren:

#include <fpu_control.h>

#if defined(HAVE_FPU_CONTROL_T)
global fpu_control_t __fpu_control = _FPU_IEEE;
#else # !defined(HAVE_FPU_CONTROL_T) && !defined(HAVE_SETFPUCW)
global unsigned short __fpu_control = _FPU_IEEE;
#endif

#endif

# ==============================================================================

#if defined(UNIX_CYGWIN32)

# Prepare for <windows.h>.
#define ULONG     OS_ULONG
#undef unused

# ------------------------------------------------------------------------------

# The library's abort() function just makes the program exit.
# But I want to see a backtrace!

int abort_dummy;
global void abort()
  { abort_dummy = 1/0; }

# ------------------------------------------------------------------------------

# The library's alarm() function is just a dummy.

#include <windows.h>
#include <signal.h>
#include <sys/time.h>

extern int _raise (int sig);

static HANDLE alarm_thread = NULL;
static struct timeval alarm_date;
static unsigned int alarm_interval;

static DWORD WINAPI do_alarm (LPVOID arg);
static DWORD WINAPI do_alarm(arg)
  LPVOID arg;
  { struct timeval now;
   start:
    gettimeofday(&now,NULL);
    if (now.tv_sec < alarm_date.tv_sec
        || now.tv_sec == alarm_date.tv_sec
           && now.tv_usec < alarm_date.tv_usec)
      { struct timeval diff;
        diff.tv_sec = alarm_date.tv_sec - now.tv_sec;
        if (alarm_date.tv_usec >= now.tv_usec)
          { diff.tv_usec = alarm_date.tv_usec - now.tv_usec; }
        else
          { diff.tv_usec = 1000000 + alarm_date.tv_usec - now.tv_usec;
            diff.tv_sec -= 1;
          }
        Sleep((DWORD)(diff.tv_sec * 1000 + (diff.tv_usec + 500) / 1000));
      }
    if (alarm_interval > 0)
      { alarm_date.tv_usec += alarm_interval;
        alarm_date.tv_sec += alarm_date.tv_usec / 1000000;
        alarm_date.tv_usec = alarm_date.tv_usec % 1000000;
        _raise(SIGALRM);
        goto start;
      }
    else
      { _raise(SIGALRM); }
    alarm_thread = NULL; return 0;
  }

global unsigned int alarm (seconds)
  unsigned int seconds;
  { struct timeval now;
    unsigned int remaining;
    DWORD alarm_thread_id;
    if (alarm_thread == NULL && seconds == 0) return 0;
    gettimeofday(&now,NULL);
    if (alarm_thread != NULL)
      { if (now.tv_sec < alarm_date.tv_sec
            || now.tv_sec == alarm_date.tv_sec
               && now.tv_usec < alarm_date.tv_usec)
          remaining = (alarm_date.tv_sec - now.tv_sec)
                      - (alarm_date.tv_usec < now.tv_usec);
        else
          remaining = 0;
        TerminateThread(alarm_thread,0); alarm_thread = NULL;
      }
    else
      remaining = 0;
    if (seconds > 0)
      { now.tv_sec += seconds;
        alarm_date = now; alarm_interval = 0;
        alarm_thread = CreateThread(NULL,10000,do_alarm,0,0,&alarm_thread_id);
      }
    return remaining;
  }

global unsigned int ualarm (value, interval)
  unsigned int value;
  unsigned int interval;
  { struct timeval now;
    unsigned int remaining;
    DWORD alarm_thread_id;
    if (alarm_thread == NULL && value == 0 && interval == 0) return 0;
    gettimeofday(&now,NULL);
    if (alarm_thread != NULL)
      { if (now.tv_sec < alarm_date.tv_sec
            || now.tv_sec == alarm_date.tv_sec
               && now.tv_usec < alarm_date.tv_usec)
          remaining = (alarm_date.tv_sec - now.tv_sec)*1000000
                      + alarm_date.tv_usec - now.tv_usec;
        else
          remaining = 0;
        TerminateThread(alarm_thread,0); alarm_thread = NULL;
      }
    else
      remaining = 0;
    if (value > 0 || interval > 0)
      { now.tv_usec += value;
        now.tv_sec += now.tv_usec / 1000000;
        now.tv_usec = now.tv_usec % 1000000;
        alarm_date = now; alarm_interval = interval;
        alarm_thread = CreateThread(NULL,10000,do_alarm,0,0,&alarm_thread_id);
      }
    return remaining;
  }

# ------------------------------------------------------------------------------

# The library's sleep() function is not interruptible by Ctrl-C.

#include <windows.h>
#include <signal.h>
#include <sys/time.h>

static BOOL DoInterruptible (LPTHREAD_START_ROUTINE fn, LPVOID arg);
static HANDLE interruptible_thread;
static BOOL interrupt_handler (DWORD CtrlType);
static BOOL interrupt_handler(CtrlType)
  DWORD CtrlType;
  { if (CtrlType == CTRL_C_EVENT /* || CtrlType == CTRL_BREAK_EVENT */ )
      { # Terminate the interruptible operation, set the exitcode to 1.
        TerminateThread(interruptible_thread,1);
        # Invoke signal handler.
        _raise(SIGINT);
        # Don't invoke the other handlers (in particular, the default handler)
        return TRUE;
      }
    else
      # Do invoke the other handlers.
      return FALSE;
  }
static BOOL DoInterruptible(fn,arg)
  LPTHREAD_START_ROUTINE fn;
  LPVOID arg;
  { HANDLE thread;
    DWORD thread_id;
    DWORD thread_exitcode;
    thread = CreateThread(NULL,10000,fn,arg,0,&thread_id);
    if (thread == NULL) return FALSE;
    interruptible_thread = thread;
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)interrupt_handler,TRUE);
    WaitForSingleObject(interruptible_thread,INFINITE);
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)interrupt_handler,FALSE);
    GetExitCodeThread(interruptible_thread,&thread_exitcode);
    if (thread_exitcode==0)
      return TRUE; # successful termination
    else
      return FALSE;
  }
static DWORD WINAPI do_sleep(arg)
  LPVOID arg;
  { Sleep((DWORD)arg); return 0; }

global unsigned int sleep (unsigned int seconds)
  { struct timeval target_time;
    struct timeval end_time;
    gettimeofday(&target_time,NULL); target_time.tv_sec += seconds;
    DoInterruptible(&do_sleep,(void*)(seconds * 1000));
    gettimeofday(&end_time,NULL);
    if (end_time.tv_sec < target_time.tv_sec
        || end_time.tv_sec == target_time.tv_sec
           && end_time.tv_usec < target_time.tv_usec)
      return (target_time.tv_sec - end_time.tv_sec)
             - (target_time.tv_usec < end_time.tv_usec);
    else
      return 0;
  }

global unsigned int usleep (unsigned int useconds)
  { struct timeval target_time;
    struct timeval end_time;
    gettimeofday(&target_time,NULL);
    target_time.tv_usec += useconds;
    target_time.tv_sec += target_time.tv_usec / 1000000;
    target_time.tv_usec = target_time.tv_usec % 1000000;
    DoInterruptible(&do_sleep,(void*)((useconds + 500) / 1000));
    gettimeofday(&end_time,NULL);
    if (end_time.tv_sec < target_time.tv_sec
        || end_time.tv_sec == target_time.tv_sec
           && end_time.tv_usec < target_time.tv_usec)
      return (target_time.tv_sec - end_time.tv_sec)*1000000
             + target_time.tv_usec - end_time.tv_usec;
    else
      return 0;
  }

# ------------------------------------------------------------------------------

#endif

# ==============================================================================

#if defined(HAVE_MMAP) && defined(UNIX_CONVEX)

# Ein Wrapper um die mmap-Funktion.
  #undef mmap
  global RETMMAPTYPE fixed_mmap (MMAP_ADDR_T addr, MMAP_SIZE_T len, int prot, int flags, int fd, off_t off);
  global RETMMAPTYPE fixed_mmap(addr,len,prot,flags,fd,off)
    var MMAP_ADDR_T addr;
    var MMAP_SIZE_T len;
    var int prot;
    var int flags;
    var int fd;
    var off_t off;
    { if (fd < 0)
        # Brauche ein Handle auf ein reguläres File.
        { local var int regular_fd = -2;
          #define regular_file  "/tmp/lispdummy.mmap"
          if (regular_fd < -1)
            { regular_fd = open(regular_file,O_CREAT|O_TRUNC|O_RDWR,my_open_mask);
              if (regular_fd >= 0) { unlink(regular_file); }
            }
          if (regular_fd >= 0)
            { return mmap(addr,&len,prot,flags,regular_fd,off); }
        }
      return mmap(addr,&len,prot,flags|MAP_FILE,fd,off);
    }

# Ein Ersatz für die mprotect-Funktion.
  global int mprotect(addr,len,prot)
    var MMAP_ADDR_T addr;
    var MMAP_SIZE_T len;
    var int prot;
    { return mremap(addr,&len,prot,MAP_PRIVATE); }

#endif

# ==============================================================================

#ifdef UNIX_CONVEX

# The purpose of this hack is to minimize crashes when memory is tight.
global int __ap$sigblock (int sigmask) { return 0; }
global int __ap$sigstack (struct sigstack *ss, struct sigstack *oss) { return 0; }

#endif

# ==============================================================================

