# Hilfsfunktionen für CLISP auf UNIX
# Bruno Haible 1990-2002
# Sam Steingold 1998-2002

#include "lispbibl.c"

# =============================================================================

#ifdef NEED_OWN_UALARM
# an emulation of ualarm(3).
global unsigned int ualarm (unsigned int value, unsigned int interval) {
  var struct itimerval itimer;
  itimer.it_value.tv_sec = floor(value,1000000);
  itimer.it_value.tv_usec = value % 1000000;
  itimer.it_interval.tv_sec = floor(interval,1000000);
  itimer.it_interval.tv_usec = interval % 1000000;
  setitimer(ITIMER_REAL,&itimer,NULL);
  return 0; # ignore the return value
}
#endif

# =============================================================================

#ifdef NEED_OWN_SELECT
# Ein Ersatz für die select-Funktion.
  global int select (int width, fd_set* readfds, fd_set* writefds, fd_set* exceptfds, struct timeval * timeout);
  global int select(width,readfds,writefds,exceptfds,timeout)
    var int width;
    var fd_set* readfds;
    var fd_set* writefds;
    var fd_set* exceptfds;
    var struct timeval * timeout;
    {
      var struct pollfd pollfd_bag[FD_SETSIZE];
      var struct pollfd * pollfd_ptr = &pollfd_bag[0];
      var int pollfd_count = 0;
      if (width<0) {
        errno = EINVAL; return -1;
      }
      if (width>FD_SETSIZE)
        width = FD_SETSIZE;
      {
        var int fd;
        for (fd=0; fd<width; fd++) {
          var short events = 0;
          if (!(readfds==NULL) && FD_ISSET(fd,readfds))
            events |= POLLIN;
          if (!(writefds==NULL) && FD_ISSET(fd,writefds))
            events |= POLLOUT;
          if (!(exceptfds==NULL) && FD_ISSET(fd,exceptfds))
            events |= POLLPRI;
          if (events) {
            pollfd_ptr->fd = fd;
            pollfd_ptr->events = events;
            pollfd_ptr->revents = 0;
            pollfd_ptr++; pollfd_count++;
          }
        }
      }
      var int poll_timeout = timeout->tv_sec * 1000 + timeout->tv_usec / (1000000/1000);
      var int result = poll(pollfd_count,&pollfd_bag[0],poll_timeout);
      if (result>=0) {
        pollfd_ptr = &pollfd_bag[0];
        until (pollfd_count == 0) {
          var int fd = pollfd_ptr->fd;
          var short revents = pollfd_ptr->revents;
          if (!(readfds==NULL) && (revents & POLLIN))
            FD_SET(fd,readfds);
          if (!(writefds==NULL) && (revents & POLLOUT))
            FD_SET(fd,writefds);
          if (!(exceptfds==NULL) && (revents & (POLLPRI|POLLERR|POLLHUP)))
            FD_SET(fd,exceptfds);
          pollfd_ptr++; pollfd_count--;
        }
      }
      return result;
    }
#endif

# =============================================================================

#ifdef NEED_OWN_GETTIMEOFDAY
# Ein Ersatz für die gettimeofday-Funktion.
  global int gettimeofday (struct timeval * tp, struct timezone * tzp);
  global int gettimeofday(tp,tzp)
    var struct timeval * tp;
    var struct timezone * tzp;
    {
      var struct timeb timebuf;
      if (!((tp==NULL) && (tzp==NULL))) {
        ftime(&timebuf);
        if (!(tp==NULL)) {
          tp->tv_sec = timebuf.time;
          tp->tv_usec = (long)(timebuf.millitm) * (1000000/1000);
        }
        if (!(tzp==NULL)) {
          tzp->tz_minuteswest = timebuf.timezone;
          tzp->tz_dsttime = 0; # ??
        }
      }
      return 0;
    }
#endif

# =============================================================================

#ifdef EINTR

#ifdef UNIX # EMUNIX und RISCOS brauchen das nicht

# Ein Wrapper um die open-Funktion.
global int nonintr_open (const char* path, int flags, mode_t mode)
{
  var int retval;
  do { retval = open(path,flags,mode);
  } while ((retval < 0) && (errno == EINTR));
  return retval;
}

# Ein Wrapper um die close-Funktion.
  global int nonintr_close (int fd);
  global int nonintr_close(fd)
    var int fd;
    {
      var int retval;
      do {
        retval = close(fd);
      } while ((retval < 0) && (errno == EINTR));
      return retval;
    }

# Ein Wrapper um die ioctl-Funktion.
  #undef ioctl
  global int nonintr_ioctl (int fd, IOCTL_REQUEST_T request, IOCTL_ARGUMENT_T arg);
  global int nonintr_ioctl(fd,request,arg)
    var int fd;
    var IOCTL_REQUEST_T request;
    var IOCTL_ARGUMENT_T arg;
    {
      var int retval;
      do {
        retval = ioctl(fd,request,arg);
      } while ((retval != 0) && (errno == EINTR));
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
    {
      var int retval;
      do {
        retval = tcsetattr(fd,optional_actions,tp);
      } while ((retval != 0) && (errno == EINTR));
      return retval;
    }

# Ein Wrapper um die tcdrain-Funktion.
  global int nonintr_tcdrain (int fd);
  global int nonintr_tcdrain(fd)
    var int fd;
    {
      var int retval;
      do {
        retval = tcdrain(fd);
      } while ((retval != 0) && (errno == EINTR));
      return retval;
    }

# Ein Wrapper um die tcflush-Funktion.
  global int nonintr_tcflush (int fd, int flag);
  global int nonintr_tcflush(fd,flag)
    var int fd;
    var int flag;
    {
      var int retval;
      do {
        retval = tcflush(fd,flag);
      } while ((retval != 0) && (errno == EINTR));
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
      if (flag) {
        if (sa.sa_flags & SA_INTERRUPT)
          return 0;
        sa.sa_flags |= SA_INTERRUPT; # system calls will be interrupted
      } else {
        if (!(sa.sa_flags & SA_INTERRUPT))
          return 0;
        sa.sa_flags &= ~ SA_INTERRUPT; # system calls will be restarted
      }
      #endif
      #ifdef SA_RESTART
      if (flag) {
        if (!(sa.sa_flags & SA_RESTART))
          return 0;
        sa.sa_flags &= ~ SA_RESTART; # system calls will be interrupted
      } else {
        if (sa.sa_flags & SA_RESTART)
          return 0;
        sa.sa_flags |= SA_RESTART; # system calls will be restarted
      }
      #endif
      sigaction(sig,&sa,(struct sigaction *)NULL);
     #elif defined(HAVE_SIGVEC) && defined(SV_INTERRUPT)
      var struct sigvec sv;
      sigvec(sig,(struct sigvec *)NULL,&sv);
      if (flag) {
        if (sv.sv_flags & SV_INTERRUPT)
          return 0;
        sv.sv_flags |= SV_INTERRUPT; # system calls will be interrupted
      } else {
        if (!(sv.sv_flags & SV_INTERRUPT))
          return 0;
        sv.sv_flags &= ~ SV_INTERRUPT; # system calls will be restarted
      }
      sigvec(sig,&sv,(struct sigvec *)NULL);
     #endif
      return 0; # den Rückgabewert ignorieren wir immer.
    }

#endif

#endif

# a wrapper for read()
global ssize_t read_helper (int fd, void* bufarea, size_t nbyte,
                            bool partial_p) {
  var char* buf = (char*) bufarea;
  var ssize_t retval;
  var size_t done = 0;
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
  while (nbyte!=0) {
    retval = read(fd,buf,nbyte);
    if (retval == 0)
      break;
    else if (retval < 0) {
     #ifdef EINTR
      if (errno != EINTR)
     #endif
        return retval;
    } else {
      buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
      if (partial_p)
        break;
    }
  }
  return done;
}

# Ein Wrapper um die write-Funktion.
  global ssize_t full_write (int fd, const void* bufarea, size_t nbyte);
  global ssize_t full_write (fd,bufarea,nbyte)
    var int fd;
    var const void* bufarea;
    var size_t nbyte;
    {
      var const char* buf = (const char*) bufarea;
      var ssize_t retval;
      var size_t done = 0;
      #if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED)) || defined(SELFMADE_MMAP)
      # Must adjust the memory permissions before calling write().
      handle_fault_range(PROT_READ,(aint)buf,(aint)buf+nbyte);
      #endif
      until (nbyte==0) {
        retval = write(fd,buf,nbyte);
        if (retval == 0)
          break;
        elif (retval < 0) {
          #ifdef EINTR
          if (!(errno == EINTR))
          #endif
            return retval;
        } else {
          buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
        }
      }
      return done;
    }

#ifdef UNIX_BEOS

# BeOS 5 sockets cannot be used like file descriptors.

# A wrapper around the recv() function.
  global ssize_t sock_read (int fd, void* bufarea, size_t nbyte);
  global ssize_t sock_read (fd,bufarea,nbyte)
    var int fd;
    var void* bufarea;
    var size_t nbyte;
    {
      var char* buf = (char*) bufarea;
      var ssize_t retval;
      var size_t done = 0;
      #if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED)) || defined(SELFMADE_MMAP)
      # Must adjust the memory permissions before calling recv().
      handle_fault_range(PROT_READ_WRITE,(aint)buf,(aint)buf+nbyte);
      #endif
      until (nbyte==0) {
        retval = recv(fd,buf,nbyte,0);
        if (retval == 0)
          break;
        elif (retval < 0) {
          #ifdef EINTR
          if (!(errno == EINTR))
          #endif
            return retval;
        } else {
          buf += retval; done += retval; nbyte -= retval;
          break; # return partial read
        }
      }
      return done;
    }

# A wrapper around the send() function.
  global ssize_t sock_write (int fd, const void* bufarea, size_t nbyte);
  global ssize_t sock_write (fd,bufarea,nbyte)
    var int fd;
    var const void* bufarea;
    var size_t nbyte;
    {
      var const char* buf = (const char*) bufarea;
      var ssize_t retval;
      var size_t done = 0;
      #if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED)) || defined(SELFMADE_MMAP)
      # Must adjust the memory permissions before calling send().
      handle_fault_range(PROT_READ,(aint)buf,(aint)buf+nbyte);
      #endif
      until (nbyte==0) {
        retval = send(fd,buf,nbyte,0);
        if (retval == 0)
          break;
        elif (retval < 0) {
          #ifdef EINTR
          if (!(errno == EINTR))
          #endif
            return retval;
        } else {
          buf += retval; done += retval; nbyte -= retval;
        }
      }
      return done;
    }

#endif

#ifdef PID_T

# Auf die Beendingung eines Child-Prozesses warten:
  global int wait2 (PID_T child);
  global int wait2(child)
    var PID_T child;
    {
      var int status = 0;
      # vgl. WAIT(2V) und #include <sys/wait.h> :
      #   WIFSTOPPED(status)  ==  ((status & 0xFF) == 0177)
      #   WEXITSTATUS(status)  == ((status & 0xFF00) >> 8)
      loop {
        var int ergebnis = waitpid(child,&status,0);
        if (!(ergebnis == child)) {
          if (ergebnis<0) {
            if (errno==EINTR)
              continue;
            #ifdef ECHILD
            if (errno==ECHILD) { # Wenn der Child-Prozess nicht mehr da ist,
              status = 0; break; # ist er wohl korrekt beendet worden.
            }
            #endif
          }
          OS_error();
        }
        if (!((status & 0xFF) == 0177)) # Child-Prozess beendet?
          break;
      }
      return status;
    }

#endif

# =============================================================================

#if defined(UNIX)

/* This is like the signal() function, except that
 - It uses sigaction() if needed in order to not block other signals,
 - It calls siginterrupt(sig,0) so that these signals avoid to interrupt
   system calls. */
global signal_handler_t install_signal_handler (int sig,
                                                signal_handler_t handler) {
  var signal_handler_t old_handler;
 #if defined(USE_SIGACTION)
  var struct sigaction old_sa;
  var struct sigaction new_sa;
  memset(&new_sa,0,sizeof(new_sa));
  new_sa.sa_handler = handler;
  /* Do not block other signals, except possibly SIGINT and SIGALRM
     (because our SIGINT/SIGALRM handlers expects the STACK_register
     to be valid). */
  sigemptyset(&new_sa.sa_mask);
 #ifdef HAVE_SAVED_STACK
  if (!(sig == SIGINT || sig == SIGALRM)) {
    sigaddset(&new_sa.sa_mask,SIGINT);
    sigaddset(&new_sa.sa_mask,SIGALRM);
  }
 #endif
  /* new_sa.sa_mask = 0; / * Do not block other signals. */
 #ifdef EINTR
  #ifdef SA_RESTART
  new_sa.sa_flags |= SA_RESTART; /* system calls will be restarted */
  #endif
 #endif
  if (sigaction(sig,&new_sa,&old_sa)<0)
    old_handler = (signal_handler_t)SIG_IGN;
  else
    old_handler = (signal_handler_t)old_sa.sa_handler;
 #else
  old_handler = signal(sig,handler);
  #ifdef EINTR
  siginterrupt(sig,0);
  #endif
 #endif
  return old_handler;
}
#endif

/* ======================================================================= */

#if defined(UNIX_CYGWIN32)

# Prepare for <windows.h>.
#define ULONG     OS_ULONG
#undef unused

# -----------------------------------------------------------------------------

# The library's abort() function just makes the program exit.
# But I want to see a backtrace!

int abort_dummy;
global void abort()
  {
    abort_dummy = 1/0;
  }

# -----------------------------------------------------------------------------

/* Cygwin internal in <src/winsup/cygwin/times.cc>
 Convert a Win32 time to "UNIX" format.
 used by syscalls and dirkey modules */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define FACTOR (0x19db1ded53ea710LL)
#define NSPERSEC 10000000LL
global long to_time_t_ (FILETIME * ptr) {
  /* A file time is the number of 100ns since jan 1 1601
     stuffed into two long words.
     A time_t is the number of seconds since jan 1 1970.  */
  long rem;
  long long x =
    ((long long) ptr->dwHighDateTime << 32) + ((unsigned) ptr->dwLowDateTime);
  /* pass "no time" as epoch */
  if (x == 0) return 0;
  x -= FACTOR;               /* number of 100ns between 1601 and 1970 */
  rem = x % ((long long) NSPERSEC);
  rem += (NSPERSEC/2);
  x /= (long long) NSPERSEC;	/* number of 100ns in a second */
  x += (long long) (rem/NSPERSEC);
  return x;
}
#undef FACTOR
#undef NSPERSEC

#endif

# =============================================================================

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
    {
      if (fd < 0) {
        # Brauche ein Handle auf ein reguläres File.
        var local int regular_fd = -2;
        #define regular_file  "/tmp/lispdummy.mmap"
        if (regular_fd < -1) {
          regular_fd = open(regular_file,O_CREAT|O_TRUNC|O_RDWR,my_open_mask);
          if (regular_fd >= 0)
            unlink(regular_file);
        }
        if (regular_fd >= 0) {
          return mmap(addr,&len,prot,flags,regular_fd,off);
        }
      }
      return mmap(addr,&len,prot,flags|MAP_FILE,fd,off);
    }

# Ein Ersatz für die mprotect-Funktion.
  global int mprotect(addr,len,prot)
    var MMAP_ADDR_T addr;
    var MMAP_SIZE_T len;
    var int prot;
    {
      return mremap(addr,&len,prot,MAP_PRIVATE);
    }

#endif

# =============================================================================

#ifdef UNIX_CONVEX

# The purpose of this hack is to minimize crashes when memory is tight.
global int __ap$sigblock (int sigmask) { return 0; }
global int __ap$sigstack (struct sigstack *ss, struct sigstack *oss) { return 0; }

#endif

# =============================================================================
