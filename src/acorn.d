# Include-File für Acorn RISC OS - Version von CLISP
# Bruno Haible, Peter Burwood 20.12.1994, 11.4.1997

# Konstanten für Steuerzeichen:

#define BEL  7              # Ton ausgeben
# define NL  10             # New line, siehe LISPBIBL.D
#define RUBOUT 127          # Rubout = Delete
#define CRLFstring  "\n"    # C-String, der BS-Newline enthält

#define stdin_handle   0  # File-Handle von Standard-Input
#define stdout_handle  1  # File-Handle von Standard-Output
#define stderr_handle  2  /* the file handle for the standard error */

# This file supports UnixLib 3.7b.

# Deklaration von Typen von Ein-/Ausgabe-Parametern von Betriebssystemfunktionen
  #include <stdlib.h>
  #include <sys/types.h>
  #include <unistd.h>

# Tabelle der System-Fehlermeldungen
  #include <errno.h>
  # extern volatile int errno; # letzter Fehlercode
  #define OS_errno errno
  #define OS_set_errno(e) (errno=(e))
  extern int sys_nerr; # Anzahl der Betriebssystem-Fehlermeldungen
  #define SYS_ERRLIST_CONST
  extern SYS_ERRLIST_CONST char* SYS_ERRLIST_CONST sys_errlist[]; # Betriebssystem-Fehlermeldungen
  extern char* sys_errlist[]; # Betriebssystem-Fehlermeldungen
  # siehe <errno.h>, PERROR(3)
# wird verwendet von ERROR, SPVW, STREAM, PATHNAME

# Bereitstellen des Arbeitsspeichers
  extern_C void* malloc (size_t size);
  extern_C void free (void* ptr);
  extern_C void* realloc (void* ptr, size_t size);
  # siehe <stdlib.h>
# wird verwendet von SPVW, STREAM

# Normales Programmende
  nonreturning_function(extern, _exit, (int status)); # siehe EXIT(2V)
  nonreturning_function(extern, exit, (int status)); # siehe EXIT(2V)
# wird verwendet von SPVW, PATHNAME, STREAM

# Sofortiger Programmabbruch, Sprung in den Debugger
  extern_C void abort (void); # siehe ABORT(3)
  # siehe <stdlib.h>
# wird verwendet von SPVW, DEBUG, EVAL, IO

# Signalbehandlung
  #include <signal.h>
  # Ein Signal-Handler ist eine Funktion ohne Ergebnis.
  typedef void (*signal_handler_t) ();
  extern_C signal_handler_t signal (int sig, signal_handler_t handler); # siehe SIGNAL(3V)
  extern void prepare_signal_handler_exit (int sig); # siehe ACORNSIG.D
  #if 1
    #define SIGNAL_NEED_REINSTALL
    #undef SIGWINCH # Removed until Posix signal handling complete.
  #else
    # New signal handling supports Posix and BSD signal handling, but it
    # is not fully debugged yet. For now, use the old signal handling system.
    # When it works, we will use the Posix system and will need the following defines.
    #define SIGNALBLOCK_POSIX 1
    #define SIGNAL_NEED_REINSTALL 1
    #define SIGNAL_NEED_UNBLOCK 1
    #define HAVE_SIGACTION 1
    #define SIGACTION_NEED_REINSTALL 1
    #define SIGACTION_NEED_UNBLOCK 1
  #endif
  #if defined(SIGNAL_NEED_UNBLOCK_OTHERS) && defined(HAVE_SIGACTION)
    # Auf manchen BSD-Systemen (z.B. SunOS 4.1.3_U1) werden bei Aufruf eines
    # Signal-Handlers auch noch andere als das aktuelle Signal blockiert.
    # Das können wir nicht brauchen und verwenden daher sigaction() statt
    # signal().
    #define USE_SIGACTION
  #endif
  #define signal_acknowledge(sig,handler)  signal(sig,handler) # Handler bleibt weiter aktiv
  #define siginterrupt(sig,flag)
  #define SIGNAL(sig,handler)  signal(sig,handler)
  # Ein Signal blockieren und wieder freigeben:
  #if defined(SIGNALBLOCK_POSIX)
    extern_C int sigprocmask (int how, const sigset_t* set, sigset_t* oset); # siehe SIGPROCMASK(2V)
    #ifndef sigemptyset # RISC OS definiert dies manchmal als Macro
      extern_C int sigemptyset (sigset_t* set); # siehe SIGSETOPS(3V)
    #endif
    #ifndef sigaddset
      extern_C int sigaddset (sigset_t* set, int signo); # siehe SIGSETOPS(3V)
    #endif
    #define signalblock_on(sig)  \
      { var sigset_t sigblock_mask;                                 \
        sigemptyset(&sigblock_mask); sigaddset(&sigblock_mask,sig); \
        sigprocmask(SIG_BLOCK,&sigblock_mask,NULL);
    #define signalblock_off(sig)  \
        sigprocmask(SIG_UNBLOCK,&sigblock_mask,NULL); \
      }
  #elif defined(SIGNALBLOCK_BSD)
    extern_C int sigblock (int mask); # siehe SIGBLOCK(2)
    extern_C int sigsetmask (int mask); # siehe SIGSETMASK(2)
    #define signalblock_on(sig)  \
      { var int old_sigblock_mask = sigblock(sigmask(sig));
    #define signalblock_off(sig)  \
        sigsetmask(old_sigblock_mask); \
      }
  #elif defined(SIGWINCH) # Ensure we know how to block signals if SIGWINCH defined
    #error "Wie blockiert man Signale?"
  #endif
  # Das Signal, das man bekommt, wenn ein Tochterprozess beendet wird: SIGCLD
  #if defined(SIGCHLD) && !defined(SIGCLD)
    #define SIGCLD  SIGCHLD
  #endif
  # Ein Signal veranlassen.
  extern_C int raise (int sig);
# wird verwendet von SPVW

# Environment-Variablen abfragen:
  extern_C char* getenv (const char* name); # siehe GETENV(3V)
  # siehe <stdlib.h>
# wird verwendet von PATHNAME, SPVW, MISC

# Working Directory setzen:
  extern_C int chdir (const char* path); # siehe CHDIR(2V)
  # siehe <unistd.h>
# wird verwendet von PATHNAME

# Working Directory abfragen:
  #if 0 # unbrauchbar, da es einen Pfad im Unix-Format liefert
    #include <sys/param.h> # definiert MAXPATHLEN
    extern_C char* getcwd (char* buf, int bufsize);
    #define getwd(buf)  getcwd(buf,MAXPATHLEN)
    # siehe <unistd.h>
  #endif
  # Benutze stattdessen die Funktionen os_fopen(), os_fclose(), os_swi().
# wird verwendet von PATHNAME

# Information zu einem File erfragen:
  #include <sys/types.h>
  #include <sys/stat.h>
  extern_C int stat (const char* path, struct stat * buf); # siehe STAT(2V)
  #define lstat stat
  extern_C int fstat (int fd, struct stat * buf); # siehe STAT(2V)
  #ifndef S_ISDIR
    #define S_ISDIR(m)  (((m)&S_IFMT) == S_IFDIR)
  #endif
  #ifndef S_ISLNK
    #define S_ISLNK(m)  (((m)&S_IFMT) == S_IFLNK)
  #endif
  #ifndef S_ISREG
    #define S_ISREG(m)  (((m)&S_IFMT) == S_IFREG)
  #endif
  # siehe <unistd.h>
  #define ELOOP_VALUE  ELOOP  # can't actually occur
# wird verwendet von PATHNAME, STREAM, SPVW

# File löschen:
  extern_C int unlink (char* path); # siehe UNLINK(2V)
  # siehe <unistd.h>
# wird verwendet von PATHNAME

# File umbenennen:
  extern_C int rename (const char* oldpath, const char* newpath); # siehe RENAME(2V)
# wird verwendet von PATHNAME

# Directory-Suche:
  #include <dirent.h>
  #define SDIRENT  struct dirent
  #if 0 # Die allgemeinen Routinen
    #undef HAVE_STRUCT_DIRENT_D_NAMLEN /* d_namlen includes the trailing null byte */
    #define d_reclen  d_namlen
    extern_C DIR* opendir (char* dirname); # siehe DIRECTORY(3V)
    extern_C SDIRENT* readdir (DIR* dirp); # siehe DIRECTORY(3V)
    extern_C int closedir (DIR* dirp); # siehe DIRECTORY(3V)
  #else # Die Routinen, die wir benutzen, sind die in acornaux.d.
    #define opendir  my_opendir  # <dirent.h> deklariert opendir() bereits...
    #define readdir  my_readdir
    #define closedir  my_closedir
    extern_C DIR* opendir (const char* dirname, const char* wildname);
    extern_C SDIRENT* readdir (DIR* dirp);
    extern_C int closedir (DIR* dirp);
  #endif
  #define CLOSEDIR  closedir
# wird verwendet von PATHNAME

# Directory anlegen:
  extern_C int mkdir (const char* path, mode_t mode);
# wird verwendet von PATHNAME

# Directory löschen:
  #if 0 # löscht auch nichtleere Directories, viel zu gefährlich!
    extern_C int rmdir (char* path); # siehe RMDIR(2V)
    # siehe <unistd.h>
  #endif
# wird verwendet von PATHNAME

# Arbeiten mit offenen Files:
  #include <sys/types.h>
  # include <unistd.h> # siehe oben
  #include <fcntl.h>
  extern_C int open (const char* path, int flags, ...); # siehe OPEN(2V)
  #define O_BINARY  0
  #define my_open_mask  0644
  #define Handle  uintW  # Typ eines File-Deskriptors
  extern_C long lseek (int fd, long offset, int whence); # siehe LSEEK(2V)
  #define RETRWTYPE ssize_t
  #define RW_BUF_T  void*
  #define RW_SIZE_T size_t
  #define WRITE_CONST
  extern_C RETRWTYPE read (int fd, RW_BUF_T buf, RW_SIZE_T nbyte); # siehe READ(2V)
  extern_C RETRWTYPE write (int fd, WRITE_CONST RW_BUF_T buf, RW_SIZE_T nbyte); # siehe WRITE(2V)
  extern_C int close (int fd); # siehe CLOSE(2V)
  # siehe <unistd.h>
  #define OPEN open
  #define CLOSE close
  # Wrapper um die System-Aufrufe, die Teilergebnisse und evtl. EINTR behandeln:
  extern_C RETRWTYPE read_helper (int fd, RW_BUF_T buf, RW_SIZE_T nbyte, bool partial_p);
  #define safe_read(f,b,n)  read_helper(f,b,n,true)
  #define full_read(f,b,n)  read_helper(f,b,n,false)
  extern_C RETRWTYPE full_write (int fd, WRITE_CONST RW_BUF_T buf, RW_SIZE_T nbyte);
# wird verwendet von STREAM, PATHNAME, SPVW, MISC

# Terminal-Abfragen, Abfragen der Fenster-Größe:
  extern_C int isatty (int fd); # siehe TTYNAME(3V)
  extern_C int ioctl (int fd, int request, void* arg); # siehe IOCTL(2)
  # siehe <unistd.h>
  #define UNIX_TERM_TERMIO
  #include <termio.h>
  extern_C int fcntl (int fd, int cmd, ...); # siehe FCNTL(2V)
  # siehe <unistd.h>
  extern_C int tgetent (char* bp, char* name); # siehe TERMCAP(3X)
  extern_C int tgetnum (char* id); # siehe TERMCAP(3X)
  extern_C int tgetflag (char* id); # siehe TERMCAP(3X)
  extern_C char* tgetstr (char* id, char** area); # siehe TERMCAP(3X)
  # siehe <termcap.h>
  #ifndef NCCS
    #define NCCS  sizeof(((struct termio *)0)->c_cc)
  #endif
# wird verwendet von SPVW, STREAM

# Datum/Uhrzeit verarbeiten:
  #include <time.h>
  extern_C time_t time (time_t* clock); # siehe TIME(3V)
  extern_C struct tm * localtime (const time_t* clock); # siehe CTIME(3V)
  extern_C struct tm * gmtime (const time_t* clock); # siehe CTIME(3V)
# wird verwendet von SPVW, MISC

# Datum/Uhrzeit abfragen:
  #include <time.h> # für CLK_TCK nötig
  #include <sys/times.h>
  extern_C clock_t times (struct tms * buffer); # siehe TIMES(3V)
  extern_C time_t time (time_t* tloc); # siehe TIME(3V)
# wird verwendet von SPVW, MISC

# Eine bestimmte Zeit Pause machen:
  extern_C unsigned int sleep (unsigned int seconds);
  # siehe <unistd.h>
# wird verwendet von MISC

# Programme aufrufen:
  extern_C int execv (const char* path, char** argv); # siehe EXECL(3V)
  extern_C int execl (const char* path, const char *arg, ...); # siehe EXECL(3V)
  extern_C int execlp (const char* path, const char *arg, ...); # siehe EXECL(3V)
  extern_C pid_t wait (int* statusp);
  # siehe <unistd.h>
  #define PID_T  pid_t  # für unixaux.d
# extern_C int wait2 (PID_T pid); # siehe unixaux.d
# wird verwendet von STREAM, PATHNAME, SPVW

