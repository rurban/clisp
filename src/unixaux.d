# Hilfsfunktionen für CLISP auf UNIX
# Bruno Haible 1990-2003
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
  global int select (int width, fd_set* readfds, fd_set* writefds, fd_set* exceptfds, struct timeval * timeout) {
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
      while (pollfd_count != 0) {
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
  global int gettimeofday (struct timeval * tp, struct timezone * tzp) {
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

#ifdef UNIX # EMUNIX braucht das nicht

# Ein Wrapper um die open-Funktion.
global int nonintr_open (const char* path, int flags, mode_t mode)
{
  var int retval;
  do {
    retval = open(path,flags,mode);
  } while ((retval < 0) && (errno == EINTR));
  return retval;
}

# Ein Wrapper um die close-Funktion.
  global int nonintr_close (int fd) {
    var int retval;
    do {
      retval = close(fd);
    } while ((retval < 0) && (errno == EINTR));
    return retval;
  }

# Ein Wrapper um die ioctl-Funktion.
  #undef ioctl
  global int nonintr_ioctl (int fd, IOCTL_REQUEST_T request, IOCTL_ARGUMENT_T arg) {
    var int retval;
    do {
      retval = ioctl(fd,request,arg);
    } while ((retval != 0) && (errno == EINTR));
    return retval;
  }

#endif

#ifdef UNIX_TERM_TERMIOS

# Ein Wrapper um die tcsetattr-Funktion.
  global int nonintr_tcsetattr (int fd, int optional_actions, struct termios * tp) {
    var int retval;
    do {
      retval = tcsetattr(fd,optional_actions,tp);
    } while ((retval != 0) && (errno == EINTR));
    return retval;
  }

# Ein Wrapper um die tcdrain-Funktion.
  global int nonintr_tcdrain (int fd) {
    var int retval;
    do {
      retval = tcdrain(fd);
    } while ((retval != 0) && (errno == EINTR));
    return retval;
  }

# Ein Wrapper um die tcflush-Funktion.
  global int nonintr_tcflush (int fd, int flag) {
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
  global int siginterrupt (int sig, int flag) {
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
global ssize_t read_helper (int fd, void* bufarea, size_t nbyte, bool no_hang)
{
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
 {NO_BLOCK_DECL(fd);
  if (no_hang) START_NO_BLOCK(fd);
  while (nbyte!=0) {
    retval = read(fd,buf,nbyte);
    if (retval == 0)
      break;
    else if (retval < 0) {
      if (no_hang && (errno == EAGAIN)) {
        /* FIXME: signal blocking state reached -- just use errno?
           never executes - printf("read_helper - read blocked\n"); */
        goto end;
      }
     #ifdef EINTR
      if (errno != EINTR)
     #endif
        return retval;
    } else {
      buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
      if (no_hang)
        break;
    }
  }
  end:
    if (no_hang) END_NO_BLOCK(fd);
  }
  // never executes
  // if (errno == EAGAIN) printf("returning with block from read_helper\n");
  return done;
}

# Ein Wrapper um die write-Funktion.
global ssize_t write_helper (int fd, const void* bufarea, size_t nbyte,
                             bool no_hang)
{
  var const char* buf = (const char*) bufarea;
  var ssize_t retval;
  var size_t done = 0;
#if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED)) || defined(SELFMADE_MMAP)
  /* Must adjust the memory permissions before calling write(). */
  handle_fault_range(PROT_READ,(aint)buf,(aint)buf+nbyte);
#endif
  {NO_BLOCK_DECL(fd);
   if (no_hang) START_NO_BLOCK(fd);
   while (nbyte!=0) {
     retval = write(fd,buf,nbyte);
     if (retval < 0) {
       if (no_hang && (errno == EAGAIN)) goto end;
      #ifdef EINTR
       /* FIXME: no way to interrupt a large write? *** */
       if (errno != EINTR)
      #endif
         {
           done = retval; /* -1 */
           goto end;
         }
     } else {
       buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
     }
   }
   end:
   if (no_hang) END_NO_BLOCK(fd);
  }
  return done;
}

#ifdef UNIX_BEOS

# BeOS 5 sockets cannot be used like file descriptors.

# A wrapper around the recv() function.
  global ssize_t sock_read (int fd, void* bufarea, size_t nbyte) {
    var char* buf = (char*) bufarea;
    var ssize_t retval;
    var size_t done = 0;
    #if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED)) || defined(SELFMADE_MMAP)
      # Must adjust the memory permissions before calling recv().
      handle_fault_range(PROT_READ_WRITE,(aint)buf,(aint)buf+nbyte);
    #endif
    while (nbyte!=0) {
      retval = recv(fd,buf,nbyte,0);
      if (retval == 0)
        break;
      else if (retval < 0) {
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

/* A wrapper around the send() function.
 FIXME: no_hang case totally untested ! */
global ssize_t sock_write (int fd, const void* bufarea, size_t nbyte,
                           bool no_hang)
{
  var const char* buf = (const char*) bufarea;
  var ssize_t retval;
  var size_t done = 0;
  #if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED)) || defined(SELFMADE_MMAP)
    /* Must adjust the memory permissions before calling send(). */
    handle_fault_range(PROT_READ,(aint)buf,(aint)buf+nbyte);
  #endif
  {NO_BLOCK_DECL(fd);
   if (no_hang) START_NO_BLOCK(fd);
   while (nbyte!=0) {
     retval = send(fd,buf,nbyte,0);
     if (retval < 0) {
       if (errno == EWOULDBLOCK) goto end;
      #ifdef EINTR
       if (errno != EINTR)
      #endif
         {
           done = retval;
           goto end;
         }
     } else {
       buf += retval; done += retval; nbyte -= retval;
     }
   }
   end:
   if (no_hang) END_NO_BLOCK(fd);
  }
  return done;
}

#endif

#ifdef PID_T

# Auf die Beendingung eines Child-Prozesses warten:
  global int wait2 (PID_T child) {
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
global void abort() {
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
  x /= (long long) NSPERSEC;    /* number of 100ns in a second */
  x += (long long) (rem/NSPERSEC);
  return x;
}
#undef FACTOR
#undef NSPERSEC

#endif

# =============================================================================
