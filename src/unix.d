# Include-File für UNIX-Version von CLISP
# Bruno Haible 1990-2000


# Konstanten für Steuerzeichen:

#define BEL  7              # Ton ausgeben
# define NL  10             # New line, siehe LISPBIBL.D
#define RUBOUT 127          # Rubout = Delete
#define CRLFstring  "\n"    # C-String, der BS-Newline enthält

#define stdin_handle   0  # File-Handle von Standard-Input
#define stdout_handle  1  # File-Handle von Standard-Output

# Deklaration von Typen von Ein-/Ausgabe-Parametern von Betriebssystemfunktionen
  #ifdef STDC_HEADERS
    #include <stdlib.h>
  #endif
  #include <sys/types.h>  /* declares pid_t, uid_t */
  #ifdef HAVE_UNISTD_H
    #include <unistd.h>
  #endif

# Tabelle der System-Fehlermeldungen
  #include <errno.h>
  extern int errno; # letzter Fehlercode
  # NB: errno kann ein Macro sein, der eine Funktion aufruft. Daher müssen
  # Zugriff und Zuweisung auf errno durch begin_system_call()/end_system_call()
  # geschützt sein.
  #define OS_errno errno
  extern int sys_nerr; # Anzahl der Betriebssystem-Fehlermeldungen
  extern SYS_ERRLIST_CONST char* SYS_ERRLIST_CONST sys_errlist[]; # Betriebssystem-Fehlermeldungen
  # siehe PERROR(3)
# wird verwendet von ERROR, SPVW, STREAM, PATHNAME

# Bereitstellen des Arbeitsspeichers
  #ifdef HAVE_GETPAGESIZE
    extern_C RETGETPAGESIZETYPE getpagesize (void); # siehe GETPAGESIZE(2)
  #endif
  #ifndef malloc
    extern_C RETMALLOCTYPE malloc (MALLOC_SIZE_T size); # siehe MALLOC(3V)
  #endif
  #ifndef free
    extern_C RETFREETYPE free (RETMALLOCTYPE ptr); # siehe MALLOC(3V)
  #endif
  #ifndef realloc
    extern_C RETMALLOCTYPE realloc (RETMALLOCTYPE ptr, MALLOC_SIZE_T size); # siehe REALLOC(3)
  #endif
  #ifdef UNIX_NEXTSTEP
    # Ignoriere den Inhalt von libposix.a, da er nicht dokumentiert ist:
    #undef HAVE_MMAP
    #undef HAVE_MUNMAP
    #undef MMAP_ADDR_T
    #undef MMAP_SIZE_T
    #undef RETMMAPTYPE
  #endif
  #ifdef UNIX_RHAPSODY
    # Ignore mmap and friends, because the configure test says no working mmap.
    #undef HAVE_MMAP
    #undef HAVE_MUNMAP
    #undef MMAP_ADDR_T
    #undef MMAP_SIZE_T
    #undef RETMMAPTYPE
    #undef HAVE_WORKING_MPROTECT
  #endif
  #if defined(HAVE_MMAP) || defined(HAVE_MMAP_ANON) || defined(HAVE_MMAP_ANONYMOUS) || defined(HAVE_MMAP_DEVZERO) || defined(HAVE_MMAP_DEVZERO_SUN4_29)
    #include <sys/types.h>
    #include <sys/mman.h>
    #ifdef UNIX_CONVEX
      #define mmap fixed_mmap  # Unter UNIX_CONVEX ist das Interface von mmap() kaputt.
      #define HAVE_WORKING_MPROTECT  # Das eigene mprotect() in unixaux.d reicht aus.
    #endif
    #if defined(HAVE_MMAP_ANONYMOUS) && !defined(HAVE_MMAP_ANON)
      # HP-UX verwendet MAP_ANONYMOUS statt MAP_ANON.
      #define MAP_ANON MAP_ANONYMOUS
      #define HAVE_MMAP_ANON
    #endif
    #if defined(UNIX_SUNOS4) || defined(UNIX_SUNOS5)
      # Für SINGLEMAP_MEMORY:
        #if defined(HAVE_MMAP_DEVZERO_SUN4_29) && defined(SUN4_29) && !defined(HAVE_MMAP_DEVZERO)
          # Unter Annahme der SUN4_29-Typcodeverteilung ist
          # HAVE_MMAP_DEVZERO_SUN4_29 ein hinreichender Ersatz für HAVE_MMAP_DEVZERO.
          #define HAVE_MMAP_DEVZERO
        #endif
    #endif
    #if defined(UNIX_SUNOS4) || defined(UNIX_LINUX)
      # NB: Unter UNIX_SUNOS4 ist nicht HAVE_MMAP_ANON, nur HAVE_MMAP_DEVZERO
      # definiert. Ersteres, weil es MAP_ANON nicht gibt; letzteres, weil
      # das Testprogramm Adressen bis 0x4000000 belegt (SUN4_29 aber nur
      # Adressen bis 0x2000000 verkraftet - daher der spezielle Test
      # HAVE_MMAP_DEVZERO_SUN4_29). Außerdem besteht ein Limit von 20 MB
      # mmap()-Speicher (danach kommt ENOMEM).
      # Wir können wahlweise MULTIMAP_MEMORY oder SINGLEMAP_MEMORY verwenden.
      # Für MULTIMAP_MEMORY:
        #include <sys/vfs.h>
        extern_C int fstatfs (int fd, struct statfs * buf); # siehe STATFS(2)
    #endif
    #ifdef UNIX_SUNOS5
      # NB: Unter UNIX_SUNOS5 sollte HAVE_MMAP_DEVZERO definiert sein.
      # Dabei gibt es allerdings ein Limit von 25 MB mmap()-Speicher.
      # Da die Shared-Memory-Facility von UNIX_SUNOS5 sich weigert,
      # Speicher an Adressen >= 0x06000000 oder mehr als 6 Mal zu attachen,
      # müssen wir SINGLEMAP_MEMORY verwenden.
    #endif
    #ifdef HAVE_MSYNC
      #ifdef MS_INVALIDATE
        # Getestet nur auf UNIX_LINUX, nicht UNIX_SUNOS4, nicht UNIX_SUNOS5. ??
        # Für MULTIMAP_MEMORY_FILE:
          extern_C int msync (MMAP_ADDR_T addr, MMAP_SIZE_T len, int flags);
      #else
        # NetBSD has a 2-argument msync(), unusable for our purposes.
        #undef HAVE_MSYNC
      #endif
    #endif
  #endif
  #ifdef HAVE_MACH_VM # Funktionen vm_allocate(), task_self(), ... vorhanden
    # Die Header-Files von UNIX_NEXTSTEP müssen ja unbeschreiblich aussehen...
    #include <sys/time.h> /* needed for <sys/resource.h> on UNIX_RHAPSODY */
    #include <sys/resource.h>
    #undef local
    #include <mach/mach_interface.h>
    #if defined(UNIX_NEXTSTEP) || defined(UNIX_RHAPSODY)
      #include <mach/mach_init.h>
    #endif
    #ifdef UNIX_OSF
      #include <mach_init.h>
    #endif
    # include <mach/mach.h>
    #include <mach/mach_traps.h> # für map_fd()
    #include <mach/machine/vm_param.h>
    #define local static
    # Damit kann man mmap(), munmap() und mprotect() selber schreiben. Siehe spvw.d.
    #define HAVE_MMAP
    #define HAVE_MUNMAP
    #define HAVE_WORKING_MPROTECT
    #define MMAP_ADDR_T  vm_address_t
    #define MMAP_SIZE_T  vm_size_t
    #define RETMMAPTYPE  MMAP_ADDR_T
    #define MPROTECT_CONST
    #define PROT_NONE  0
    #define PROT_READ  VM_PROT_READ
    #define PROT_WRITE VM_PROT_WRITE
    #define PROT_EXEC  VM_PROT_EXECUTE
  #endif
  #ifdef HAVE_MMAP
    extern_C RETMMAPTYPE mmap (MMAP_ADDR_T addr, MMAP_SIZE_T len, int prot, int flags, int fd, off_t off); # siehe MMAP(2)
  #endif
  #ifdef HAVE_MUNMAP
    extern_C int munmap (MMAP_ADDR_T addr, MMAP_SIZE_T len); # siehe MUNMAP(2)
  #endif
  #ifdef HAVE_WORKING_MPROTECT
    extern_C int mprotect (MPROTECT_CONST MMAP_ADDR_T addr, MMAP_SIZE_T len, int prot); # siehe MPROTECT(2)
  #endif
  # Mögliche Werte von prot: PROT_NONE, PROT_READ, PROT_READ_WRITE.
  #ifndef PROT_NONE
    #define PROT_NONE  0
  #endif
  #define PROT_READ_WRITE  (PROT_READ | PROT_WRITE)
  #ifdef HAVE_SHM
    #include <sys/types.h>
    #include <sys/ipc.h>
    #include <sys/shm.h>
    #ifdef HAVE_SYS_SYSMACROS_H
      #include <sys/sysmacros.h>
    #endif
    #ifdef UNIX_HPUX
      #include <sys/vmmac.h> # für SHMLBA
    #endif
    #ifdef UNIX_AUX
      #include <sys/mmu.h> # für SHMLBA
    #endif
    #ifdef UNIX_LINUX
      #include <asm/page.h> # für SHMLBA in Linux 2.0
    #endif
    #if defined(UNIX_SUNOS4) || defined(UNIX_SUNOS5)
      #define SHMMAX  0x100000 # maximale Shared-Memory-Segment-Größe = 1 MB
    #endif
    #ifndef SHMMAX
      #define SHMMAX  0xFFFFFFFFUL # maximale Shared-Memory-Segment-Größe wird als unendlich angenommen
    #endif
    extern_C int shmget (key_t key, SHMGET_SIZE_T size, int shmflg); # siehe SHMGET(2)
    extern_C RETSHMATTYPE shmat (int shmid, SHMAT_CONST RETSHMATTYPE shmaddr, int shmflg); # siehe SHMOP(2)
    extern_C int shmdt (SHMDT_ADDR_T shmaddr); # siehe SHMOP(2)
    #ifdef SHMCTL_DOTS
      extern_C int shmctl (int shmid, int cmd, ...); # siehe SHMCTL(2)
    #else
      extern_C int shmctl (int shmid, int cmd, struct shmid_ds * buf); # siehe SHMCTL(2)
    #endif
  #endif
# wird verwendet von SPVW, STREAM

# Steuerung der Pagingverhaltens
  #ifdef HAVE_VADVISE
    #include <sys/vadvise.h> # Steuercodes
    extern_C void vadvise (int param); # Paging-System steuern # siehe VADVISE(2)
  #endif
  # madvise() verwenden??
# wird verwendet von SPVW

# Stack hinreichend groß machen
  #ifdef UNIX_NEXTSTEP
    #include <sys/types.h>
    #include <sys/time.h>
    #include <sys/resource.h>
    extern_C int getrlimit (RLIMIT_RESOURCE_T resource, struct rlimit * rlim); # see GETRLIMIT(2)
    extern_C int setrlimit (RLIMIT_RESOURCE_T resource, SETRLIMIT_CONST struct rlimit * rlim); # see SETRLIMIT(2)
  #endif
# wird verwendet von SPVW

# Normales Programmende
  nonreturning_function(extern_C, _exit, (int status)); # siehe EXIT(2V)
  nonreturning_function(extern_C, exit, (int status)); # siehe EXIT(2V)
# wird verwendet von SPVW, PATHNAME, STREAM

# Sofortiger Programmabbruch, Sprung in den Debugger
  extern_C ABORT_VOLATILE RETABORTTYPE abort (void); # siehe ABORT(3)
# wird verwendet von SPVW, DEBUG, EVAL, IO

# Signalbehandlung
  #include <signal.h>
  # Ein Signal-Handler ist eine Funktion ohne Ergebnis.
  #ifdef __cplusplus
    #ifdef SIGTYPE_DOTS
      typedef RETSIGTYPE (*signal_handler) (...);
    #else
      typedef RETSIGTYPE (*signal_handler) (int);
    #endif
  #else
    typedef RETSIGTYPE (*signal_handler) ();
  #endif
  # Ein Signal möglichst sauber installieren:
  extern_C signal_handler signal (int sig, signal_handler handler); # siehe SIGNAL(3V)
  #if defined(SIGNAL_NEED_UNBLOCK_OTHERS) && defined(HAVE_SIGACTION)
    # Auf manchen BSD-Systemen (z.B. SunOS 4.1.3_U1) werden bei Aufruf eines
    # Signal-Handlers auch noch andere als das aktuelle Signal blockiert.
    # Das können wir nicht brauchen und verwenden daher sigaction() statt
    # signal().
    #define USE_SIGACTION
  #endif
  extern signal_handler install_signal_handler (int sig, signal_handler handler);
  #define SIGNAL(sig,handler)  install_signal_handler(sig,handler)
  # Ein Signal blockieren und wieder freigeben:
  #if defined(SIGNALBLOCK_POSIX)
    extern_C int sigprocmask (int how, SIGPROCMASK_CONST sigset_t* set, sigset_t* oset); # siehe SIGPROCMASK(2V)
    #ifndef sigemptyset # UNIX_LINUX definiert dies manchmal als Macro
      extern_C int sigemptyset (sigset_t* set); # siehe SIGSETOPS(3V)
    #endif
    #ifndef sigaddset # UNIX_LINUX definiert dies manchmal als Macro
      extern_C int sigaddset (sigset_t* set, int signo); # siehe SIGSETOPS(3V)
    #endif
    #define signalblock_on(sig)  \
      { var sigset_t sigblock_mask;                                 \
        sigemptyset(&sigblock_mask); sigaddset(&sigblock_mask,sig); \
        sigprocmask(SIG_BLOCK,&sigblock_mask,NULL);
    #define signalblock_off(sig)  \
        sigprocmask(SIG_UNBLOCK,&sigblock_mask,NULL); \
      }
  #elif defined(SIGNALBLOCK_SYSV)
    extern_C int sighold (int sig);
    extern_C int sigrelse (int sig);
    #define signalblock_on(sig)  sighold(sig);
    #define signalblock_off(sig)  sigrelse(sig);
  #elif defined(SIGNALBLOCK_BSD)
    extern_C int sigblock (int mask); # siehe SIGBLOCK(2)
    extern_C int sigsetmask (int mask); # siehe SIGSETMASK(2)
    #define signalblock_on(sig)  \
      { var int old_sigblock_mask = sigblock(sigmask(sig));
    #define signalblock_off(sig)  \
        sigsetmask(old_sigblock_mask); \
      }
  #else
    #error "Wie blockiert man Signale?"
  #endif
  # Ein Signal erst eine bestimmte Zeit später ausliefern:
  # extern_C {unsigned|} int alarm ({unsigned|} int seconds); # siehe ALARM(3V)
  #ifdef UNIX_CYGWIN32
    #define HAVE_UALARM  # alarm() und ualarm() siehe unixaux.d
  #endif
  #if !defined(HAVE_UALARM) && defined(HAVE_SETITIMER)
    #define NEED_OWN_UALARM # mit setitimer() kann man ualarm() selber schreiben
    #include <sys/time.h>
    extern_C int setitimer (int which, SETITIMER_CONST struct itimerval * ivalue, struct itimerval * ovalue); # siehe SETITIMER(2)
    #define HAVE_UALARM
  #endif
  #ifdef HAVE_UALARM
    extern_C unsigned int ualarm (unsigned int value, unsigned int interval); # siehe UALARM(3)
  #endif
  # Die Ankunft eines Signals quittieren (aus dem Signal-Handler heraus):
  #ifdef USE_SIGACTION
    #ifdef SIGACTION_NEED_REINSTALL
      #define signal_acknowledge(sig,handler)  install_signal_handler(sig,handler) # Handler bleibt weiter aktiv
    #else # Signalverwaltung nach BSD hat das nicht nötig
      #define signal_acknowledge(sig,handler)
    #endif
  #else
    #ifdef SIGNAL_NEED_REINSTALL # UNIX_SYSV || UNIX_LINUX || ...
      #define signal_acknowledge(sig,handler)  install_signal_handler(sig,handler) # Handler bleibt weiter aktiv
    #else # Signalverwaltung nach BSD hat das nicht nötig
      #define signal_acknowledge(sig,handler)
    #endif
  #endif
  # Das Signal, das man bekommt, wenn ein Tochterprozess beendet wird: SIGCLD
  #if defined(SIGCHLD) && !defined(SIGCLD)
    #define SIGCLD  SIGCHLD
  #endif
  # Das Verhalten von Signalen bei System-Calls beeinflussen:
  # flag=0: Nach Signal sig laufen System-Calls weiter.
  # flag=1: Durch Signal sig werden System-Calls abgebrochen, mit errno=EINTR.
  #ifdef EINTR
    extern_C int siginterrupt (int sig, int flag); # siehe SIGINTERRUPT(3V)
    #ifndef HAVE_SIGINTERRUPT
      # mit sigaction() oder sigvec() kann man siginterrupt() selber schreiben
      #define NEED_OWN_SIGINTERRUPT
    #endif
  #else
    #define siginterrupt(sig,flag)
  #endif
  # Zur Behebung von SIGSEGV-Signalen nach Schreibzugriff auf
  # schreibgeschützte Bereiche. Siehe unix/sigsegv.c.
  # Obacht: Hans-J. Boehm <boehm@parc.xerox.com> sagt, dass Schreibzugriffe
  # aus Betriebssystem-Aufrufen heraus (z.B. read()) auf vielen Systemen
  # wider Erwarten kein Signal auslösen. (Unter Linux funktioniert's.)
  #ifndef SPVW_MIXED_BLOCKS
  # Wir haben das Glück, mit read() nur in den C-Stack und in Strings zu
  # schreiben, nicht jedoch in eventuell mprotect-geschützte Bereiche.
  #endif
  # Ein Signal veranlassen.
  #ifdef HAVE_RAISE
    extern_C int raise (int sig);
  #endif
# wird verwendet von SPVW

# Environment-Variablen abfragen:
  extern_C char* getenv (GETENV_CONST char* name); # siehe GETENV(3V)
# wird verwendet von PATHNAME, SPVW, MISC

# Environment-Variablen setzen:
  #if defined(HAVE_PUTENV)
    extern_C int putenv (PUTENV_CONST char* name); # siehe PUTENV(3)
  #elif defined(HAVE_SETENV)
    extern_C int setenv (GETENV_CONST char* name, GETENV_CONST char* value, int overwrite); # siehe SETENV(3)
  #endif
# wird verwendet von SPVW

# Anpassung an lokale Präferenzen:
  #ifdef HAVE_LOCALE_H
    #include <locale.h>
    extern_C char* setlocale (int category, SETLOCALE_CONST char* locale);
  #endif
# wird verwendet von SPVW

# Home-Directory eines Benutzers holen:
  #include <pwd.h>
  extern_C struct passwd * getpwnam (GETPWNAM_CONST char* name); # siehe GETPWENT(3V)
  extern_C struct passwd * getpwuid (GETPWUID_UID_T uid); # siehe GETPWENT(3V)
  extern_C uid_t getuid (void); # siehe GETUID(2V)
  extern uid_t user_uid; # Real User ID des laufenden Prozesses
  extern_C char* getlogin (void); # siehe GETLOGIN(3V)
# wird verwendet von PATHNAME, SPVW

# Working Directory setzen:
  extern_C int chdir (CHDIR_CONST char* path); # siehe CHDIR(2V)
# wird verwendet von PATHNAME

# Working Directory abfragen:
  #include <sys/param.h>
  # Maximale Pfadlänge (incl. Nullbyte am Schluss), die von getwd geliefert wird:
  #ifndef MAXPATHLEN
    #define MAXPATHLEN  1024  # siehe <sys/param.h>
  #endif
  #ifdef HAVE_GETCWD
    extern_C char* getcwd (char* buf, GETCWD_SIZE_T bufsize);
    #define getwd(buf)  getcwd(buf,MAXPATHLEN)
  #else
    extern_C char* getwd (char* pathname); # siehe GETWD(3)
  #endif
# wird verwendet von PATHNAME

# Maximalzahl symbolischer Links, die nacheinander aufgelöst werden:
  #ifndef MAXSYMLINKS
    #define MAXSYMLINKS  8  # siehe <sys/param.h>
  #endif
# wird verwendet von PATHNAME

# Auflösen symbolischer Links in Pfadnamen:
  #ifdef HAVE_READLINK
    extern_C int readlink (READLINK_CONST char* path, READLINK_BUF_T buf, READLINK_SIZE_T bufsiz); # siehe READLINK(2)
  #endif
# wird verwendet von PATHNAME

# Information zu einem File erfragen:
  #include <sys/types.h>
  #include <sys/stat.h>
  #ifdef STAT_MACROS_BROKEN
    #undef S_ISDIR
    #undef S_ISLNK
    #undef S_ISREG
  #endif
  #ifdef STAT_INLINE
    extern int stat (STAT_CONST char* path, struct stat * buf); # siehe STAT(2V)
  #else
    extern_C int stat (STAT_CONST char* path, struct stat * buf); # siehe STAT(2V)
  #endif
  #ifdef HAVE_LSTAT
    #ifdef LSTAT_INLINE
      extern int lstat (LSTAT_CONST char* path, struct stat * buf); # siehe STAT(2V)
    #else
      extern_C int lstat (LSTAT_CONST char* path, struct stat * buf); # siehe STAT(2V)
    #endif
  #else
    #define lstat stat
    #define S_ISLNK(m)  FALSE
  #endif
  #ifdef FSTAT_INLINE
    extern int fstat (int fd, struct stat * buf); # siehe STAT(2V)
  #else
    extern_C int fstat (int fd, struct stat * buf); # siehe STAT(2V)
  #endif
  #ifndef S_ISDIR
    #define S_ISDIR(m)  (((m)&S_IFMT) == S_IFDIR)
  #endif
  #ifndef S_ISLNK
    #define S_ISLNK(m)  (((m)&S_IFMT) == S_IFLNK)
  #endif
  #ifndef S_ISREG
    #define S_ISREG(m)  (((m)&S_IFMT) == S_IFREG)
  #endif
# wird verwendet von PATHNAME, STREAM, SPVW

# File löschen:
  extern_C int unlink (UNLINK_CONST char* path); # siehe UNLINK(2V)
# wird verwendet von PATHNAME, UNIXAUX

# File umbenennen:
  extern_C int rename (RENAME_CONST char* oldpath, RENAME_CONST char* newpath); # siehe RENAME(2V)
# wird verwendet von PATHNAME, UNIXAUX

# Directory-Suche:
  #if defined(DIRENT) || defined(_POSIX_VERSION)
    #include <dirent.h>
    #define SDIRENT  struct dirent
  #else
    #ifdef SYSNDIR
      #include <sys/ndir.h>
    #else
      #ifdef SYSDIR
        #include <sys/dir.h>
      #else
        #ifdef NDIR
          #include <ndir.h>
        #else
          #include <dir.h>
        #endif
      #endif
    #endif
    #define SDIRENT  struct direct
  #endif
  extern_C DIR* opendir (OPENDIR_CONST char* dirname); # siehe DIRECTORY(3V)
  extern_C SDIRENT* readdir (DIR* dirp); # siehe DIRECTORY(3V)
  extern_C RETCLOSEDIRTYPE closedir (DIR* dirp); # siehe DIRECTORY(3V)
  #ifdef VOID_CLOSEDIR
    #define CLOSEDIR(dirp)  (closedir(dirp),0)
  #else
    #define CLOSEDIR  closedir
  #endif
# wird verwendet von PATHNAME

# Directory anlegen:
  extern_C int mkdir (MKDIR_CONST char* path, mode_t mode); # siehe MKDIR(2V)
# wird verwendet von PATHNAME

# Directory löschen:
  extern_C int rmdir (RMDIR_CONST char* path); # siehe RMDIR(2V)
# wird verwendet von PATHNAME

# Arbeiten mit offenen Files:
  #include <sys/types.h>
  # include <unistd.h> # siehe oben
  #include <fcntl.h>
  #if defined(ACCESS_NEEDS_SYS_FILE_H) || defined(OPEN_NEEDS_SYS_FILE_H)
    #include <sys/file.h>
  #endif
  #ifdef OPEN_DOTS
    extern_C int open (OPEN_CONST char* path, int flags, ...); # siehe OPEN(2V)
  #else
    extern_C int open (OPEN_CONST char* path, int flags, mode_t mode); # siehe OPEN(2V)
  #endif
  # Only a few Unices (like UNIX_CYGWIN32) have O_TEXT and O_BINARY.
  #ifndef O_BINARY
    #define O_BINARY  0
  #endif
  #define my_open_mask  0644
  #define Handle  uintW  # Typ eines File-Deskriptors
  extern_C off_t lseek (int fd, off_t offset, int whence); # siehe LSEEK(2V)
  #ifndef SEEK_SET # wg. UNIX_NEXTSTEP
    # Positionierungsmodi, vgl. <unistd.h> :
    #define SEEK_SET  0
    #define SEEK_CUR  1
    #define SEEK_END  2
  #endif
  extern_C RETRWTYPE read (int fd, RW_BUF_T buf, RW_SIZE_T nbyte); # siehe READ(2V)
  extern_C RETRWTYPE write (int fd, WRITE_CONST RW_BUF_T buf, RW_SIZE_T nbyte); # siehe WRITE(2V)
  extern_C int close (int fd); # siehe CLOSE(2V)
  #ifdef HAVE_FSYNC
    extern_C int fsync (int fd); # siehe FSYNC(2)
  #endif
  #if !defined(HAVE_SELECT) && defined(HAVE_POLL)
    #define NEED_OWN_SELECT # mit poll() kann man select() selber schreiben
    #include <poll.h>
    extern_C int poll (struct pollfd * fds, unsigned long nfds, int timeout);
    #ifndef _EMUL_SYS_TIME_H
      #define _EMUL_SYS_TIME_H
      struct timeval { long tv_sec; long tv_usec; };
      struct timezone { int tz_minuteswest; int tz_dsttime; };
    #endif
    #define SELECT_WIDTH_T int
    #define SELECT_SET_T fd_set
    #define SELECT_CONST
    #define HAVE_SELECT # siehe unixaux.d
  #endif
  #ifdef HAVE_SELECT
    #ifndef _EMUL_SYS_TIME_H
      #include <sys/time.h>
    #endif
    #ifdef HAVE_SYS_SELECT_H
      #include <sys/select.h>
    #endif
    #ifndef FD_SETSIZE
      # Definition des Typs fd_set, vgl. <sys/types.h> :
      #ifdef UNIX_HPUX # dort ist fd_set bereits definiert, aber FD_SETSIZE nicht
        #define fd_set  my_fd_set
      #endif
      #define FD_SETSIZE  256  # Maximalzahl von File-Deskriptoren
      typedef int  fd_mask;  # eine Bitgruppe
      #define NFDBITS  (sizeof(fd_mask) * 8)  # Anzahl Bits in einer Bitgruppe
      typedef struct fd_set { fd_mask fds_bits[ceiling(FD_SETSIZE,NFDBITS)]; }
              fd_set;
      #define FD_SET(n,p)  ((p)->fds_bits[(n)/NFDBITS] |= bit((n)%NFDBITS))
      #define FD_CLR(n,p)  ((p)->fds_bits[(n)/NFDBITS] &= ~bit((n)%NFDBITS))
      #define FD_ISSET(n,p)  ((p)->fds_bits[(n)/NFDBITS] & bit((n)%NFDBITS))
      #define FD_ZERO(p)  bzero((char*)(p),sizeof(*(p)))
      #include <string.h>
      #ifndef memset
        extern_C RETMEMSETTYPE memset (void* ptr, int c, size_t len); # siehe MEMORY(3)
      #endif
      #define bzero(ptr,len)  memset(ptr,0,len)
    #endif
    extern_C int select (SELECT_WIDTH_T width, SELECT_SET_T* readfds,
                       SELECT_SET_T* writefds, SELECT_SET_T* exceptfds,
                       SELECT_CONST struct timeval * timeout); # siehe SELECT(2)
  #endif
  #ifdef EINTR
    # Wrapper um die System-Aufrufe, die EINTR abfangen und behandeln:
    extern int nonintr_open (OPEN_CONST char* path, int flags, mode_t mode);
    extern int nonintr_close (int fd);
    #define OPEN nonintr_open
    #define CLOSE nonintr_close
  #else
    #define OPEN open
    #define CLOSE close
  #endif
  # Wrapper um die System-Aufrufe, die Teilergebnisse und evtl. EINTR behandeln:
  extern RETRWTYPE full_read (int fd, RW_BUF_T buf, RW_SIZE_T nbyte);
  extern RETRWTYPE full_write (int fd, WRITE_CONST RW_BUF_T buf, RW_SIZE_T nbyte);
# wird verwendet von STREAM, PATHNAME, SPVW, MISC, UNIXAUX

# Terminal-Abfragen, Abfragen der Fenster-Größe:
  extern_C int isatty (int fd); # siehe TTYNAME(3V)
  #if 0
    extern_C char* ttyname (int fd); # siehe TTYNAME(3V)
  #endif
  #ifdef IOCTL_DOTS
    extern_C int ioctl (int fd, IOCTL_REQUEST_T request, ...); # siehe IOCTL(2)
    #define IOCTL_ARGUMENT_T  CADDR_T
  #else
    extern_C int ioctl (int fd, IOCTL_REQUEST_T request, IOCTL_ARGUMENT_T arg); # siehe IOCTL(2)
    # 3. Argument stets zum Typ IOCTL_ARGUMENT_T (meist CADDR_T) casten:
    #define ioctl(fd,request,arg)  (ioctl)(fd,request,(IOCTL_ARGUMENT_T)(arg))
  #endif
  #if defined(HAVE_TERMIOS_H) && defined(HAVE_TCGETATTR) && defined(HAVE_TCSAFLUSH)
    #define UNIX_TERM_TERMIOS
    #include <termios.h> # siehe TERMIOS(3V)
    #ifndef tcgetattr
      extern_C int tcgetattr (int fd, struct termios * tp);
    #endif
    #ifndef tcsetattr
      extern_C int tcsetattr (int fd, int optional_actions, TCSETATTR_CONST struct termios * tp);
    #endif
    #ifndef tcdrain
      extern_C int tcdrain (int fd); # siehe TERMIOS(3V)
    #endif
    #ifndef tcflush
      extern_C int tcflush (int fd, int flag); # siehe TERMIOS(3V)
    #endif
    #undef TCSETATTR  # wg. HP-UX 10
    #define TCSETATTR tcsetattr
    #define TCDRAIN tcdrain
    #define TCFLUSH tcflush
    #ifndef NCCS
      #define NCCS  sizeof(((struct termios *)0)->c_cc)
    #endif
    #if defined(WINSIZE_NEED_SYS_IOCTL_H) # glibc2 needs this for "struct winsize"
      #include <sys/ioctl.h>
    #elif defined(WINSIZE_NEED_SYS_PTEM_H) # SCO needs this for "struct winsize"
      #include <sys/stream.h>
      #include <sys/ptem.h>
    #endif
  #elif defined(HAVE_SYS_TERMIO_H) || defined(HAVE_TERMIO_H)
    #define UNIX_TERM_TERMIO
    #if defined(HAVE_SYS_TERMIO_H)
      #include <sys/termio.h> # siehe TERMIO(4)
    #elif defined(HAVE_TERMIO_H)
      #include <termio.h>
    #endif
    #ifndef NCCS
      #define NCCS  sizeof(((struct termio *)0)->c_cc)
    #endif
  #elif defined(HAVE_SGTTY_H)
    # kompatibel zu V7 oder 4BSD, ioctls der Form TIOC....
    #define UNIX_TERM_SGTTY
    #include <sgtty.h>
    #include <sys/ioctl.h> # siehe TTY(4)
  #endif
  #if defined(NEED_SYS_FILIO_H)
    #include <sys/filio.h>
  #elif defined(NEED_SYS_IOCTL_H)
    #include <sys/ioctl.h>
  #endif
  #ifndef HAVE_SELECT
    # include <fcntl.h> # siehe oben
    #ifdef FCNTL_DOTS
      extern_C int fcntl (int fd, int cmd, ...); # siehe FCNTL(2V)
    #else
      extern_C int fcntl (int fd, int cmd, int arg); # siehe FCNTL(2V)
    #endif
  #endif
  #if (defined(UNIX_TERM_TERMIOS) || defined(UNIX_TERM_TERMIO)) && !(defined(TCIFLUSH) && defined(TCOFLUSH))
    #define TCIFLUSH 0
    #define TCOFLUSH 1
  #endif
  extern_C int tgetent (const char* bp, const char* name); # siehe TERMCAP(3X)
  extern_C int tgetnum (const char* id); # siehe TERMCAP(3X)
  extern_C int tgetflag (const char* id); # siehe TERMCAP(3X)
  extern_C const char* tgetstr (const char* id, char** area); # siehe TERMCAP(3X)
  #ifdef EINTR
    # Wrapper um die System-Aufrufe, die EINTR abfangen und behandeln:
    extern int nonintr_ioctl (int fd, IOCTL_REQUEST_T request, IOCTL_ARGUMENT_T arg);
    #undef ioctl
    #define ioctl(fd,request,arg)  nonintr_ioctl(fd,request,(IOCTL_ARGUMENT_T)(arg))
    #ifdef UNIX_TERM_TERMIOS
      extern int nonintr_tcsetattr (int fd, int optional_actions, struct termios * tp);
      extern int nonintr_tcdrain (int fd); # siehe TERMIOS(3V)
      extern int nonintr_tcflush (int fd, int flag); # siehe TERMIOS(3V)
      #undef TCSETATTR
      #define TCSETATTR nonintr_tcsetattr
      #undef TCDRAIN
      #define TCDRAIN nonintr_tcdrain
      #undef TCFLUSH
      #define TCFLUSH nonintr_tcflush
    #endif
  #endif
# wird verwendet von SPVW, STREAM

# Datum/Uhrzeit verarbeiten:
  #ifdef TM_IN_SYS_TIME
    #include <sys/time.h>
  #else
    #include <time.h>
  #endif
  extern_C time_t time (time_t* clock); # siehe TIME(3V)
  extern_C struct tm * localtime (LOCALTIME_CONST time_t* clock); # siehe CTIME(3V)
  extern_C struct tm * gmtime (LOCALTIME_CONST time_t* clock); # siehe CTIME(3V)
# wird verwendet von SPVW, MISC

# Datum/Uhrzeit abfragen:
  #if defined(HAVE_GETTIMEOFDAY)
    #include <sys/time.h>
    #ifdef GETTIMEOFDAY_DOTS
      extern_C int gettimeofday (struct timeval * tp, ...); # siehe GETTIMEOFDAY(2)
    #else
      extern_C int gettimeofday (struct timeval * tp, GETTIMEOFDAY_TZP_T tzp); # siehe GETTIMEOFDAY(2)
    #endif
    #ifdef UNIX_CYGWIN32
      # gettimeofday() always returns 1. Let it return 0.
      #define gettimeofday(tv,tz)  ((gettimeofday)(tv,tz), 0)
    #endif
  #elif defined(HAVE_FTIME)
    #include <sys/timeb.h>
    extern_C int ftime (struct timeb * tp); # siehe TIME(3V)
    # Emuliere gettimeofday() in unixaux.d:
    #define NEED_OWN_GETTIMEOFDAY
    #ifndef _EMUL_SYS_TIME_H
      #define _EMUL_SYS_TIME_H
      struct timeval { long tv_sec; long tv_usec; };
      struct timezone { int tz_minuteswest; int tz_dsttime; };
    #endif
    extern int gettimeofday (struct timeval * tp, struct timezone * tzp); # siehe unixaux.d
  #elif defined(HAVE_TIMES_CLOCK)
    #include <time.h> # für CLK_TCK nötig
    #ifndef CLK_TCK
      #include <sys/time.h> # für CLK_TCK nötig, unter UNIX_SYSV_PTX
    #endif
    #include <sys/times.h>
    extern_C clock_t times (struct tms * buffer); # siehe TIMES(3V)
    extern_C time_t time (time_t* tloc); # siehe TIME(3V)
  #else
    #error "Cannot access real time with finer resolution than 1 second."
  #endif
# wird verwendet von SPVW, MISC

# vom Prozess verbrauchte Zeit erfragen:
  #if defined(HAVE_GETRUSAGE)
    #include <sys/types.h>
    #include <sys/time.h>
    #include <sys/resource.h>
    extern_C int getrusage (RUSAGE_WHO_T who, struct rusage * rusage); # siehe GETRUSAGE(2)
    # Prototyp wertlos, da 'struct rusage' /= 'struct rusage' - verkorxtes ANSI!
  #elif defined(HAVE_SYS_TIMES_H)
    #include <sys/types.h>
    #include <sys/param.h> # definiert HZ, Maßeinheit ist 1/HZ Sekunden
    #include <sys/times.h>
    extern_C clock_t times (struct tms * buffer); # siehe TIMES(3V)
  #endif
  # Alternative:
  # #include <??>
  # extern_C ?? vtimes (struct vtimes * par_vm, struct vtimes * ch_vm); # siehe VTIMES(3C)
# wird verwendet von SPVW

# Eine bestimmte Zeit Pause machen:
  extern_C unsigned int sleep (unsigned int seconds); # siehe SLEEP(3V)
  #ifdef HAVE_USLEEP
    # extern_C {int|void} usleep (unsigned int useconds); # siehe USLEEP(3)
  #endif
# wird verwendet von MISC

# Programme aufrufen:
  #define SHELL  "/bin/sh"  # Name der für Kommandos benutzten Shell
  extern_C int pipe (int fd[2]); # siehe PIPE(2V)
  #ifdef HAVE_VFORK_H
    #include <vfork.h>
  #endif
  extern_C RETVFORKTYPE vfork (void); # siehe VFORK(2)
  extern_C int dup2 (int fd1, int fd2); # siehe DUP(2V)
  #if defined(HAVE_SETPGID)
    extern_C pid_t getpid (void); # siehe GETPID(2V)
    extern_C int setpgid (pid_t pid, pid_t pgid); # siehe SETPGID(2V), SETSID(2V), TERMIO(4)
    #define SETSID()  { register pid_t pid = getpid(); setpgid(pid,pid); }
  #elif defined(HAVE_SETSID)
    extern_C pid_t setsid (void); # siehe SETSID(2V), TERMIO(4)
    #define SETSID()  setsid()
  #else
    #define SETSID()
  #endif
  extern_C int execv (EXECV_CONST char* path, EXECV1_CONST char* EXECV2_CONST argv[]); # siehe EXECL(3V)
  #ifdef EXECL_DOTS
    extern_C int execl (EXECV_CONST char* path, EXECL_CONST char* arg, ...); # siehe EXECL(3V)
  #else
    extern_C int execl (EXECV_CONST char* path, EXECL_CONST char* arg0, EXECL_CONST char* arg1, EXECL_CONST char* arg2, EXECL_CONST char* arg3); # siehe EXECL(3V)
  #endif
  #ifdef EXECL_DOTS
    extern_C int execlp (EXECV_CONST char* path, EXECL_CONST char* arg, ...); # siehe EXECL(3V)
  #else
    extern_C int execlp (EXECV_CONST char* path, EXECL_CONST char* arg0, EXECL_CONST char* arg1, EXECL_CONST char* arg2, EXECL_CONST char* arg3); # siehe EXECL(3V)
  #endif
  # NB: Im Zeitraum zwischen vfork() und execv()/execl()/execlp() darf der
  # Child-Prozess nur auf Daten im Stack und konstante Daten zugreifen.
  # Denn der Parent-Prozess läuft in dieser Zeit schon weiter und kann dabei
  # Daten in STACK, malloc()-Bereich, Lisp-Daten-Bereich usw. modifizieren.
  #include <sys/wait.h>
  extern_C pid_t waitpid (PID_T pid, int* statusp, int options); # siehe WAIT(2V)
  extern int wait2 (PID_T pid); # siehe unixaux.d
# wird verwendet von STREAM, PATHNAME, SPVW, UNIXAUX

# Zufallszahlen besorgen:
  #ifndef rand # Manche definieren rand() als Macro...
    extern_C int rand (void); # siehe RAND(3V)
  #endif
  extern_C pid_t getpid (void); # siehe GETPID(2V)
# wird verwendet von LISPARIT

# MACHINE-TYPE und MACHINE-VERSION und evtl. MACHINE-INSTANCE bestimmen:
  #ifdef HAVE_SYS_UTSNAME_H
    #include <sys/utsname.h>
    extern_C int uname (struct utsname * buf); # siehe UNAME(2V)
  #endif
# wird verwendet von MISC

# MACHINE-INSTANCE bestimmen:
  #ifdef HAVE_GETHOSTNAME
    extern_C int gethostname (char* name, GETHOSTNAME_SIZE_T namelen); # siehe GETHOSTNAME(2)
  #endif
  #ifdef HAVE_GETHOSTBYNAME
    #include <sys/types.h>
    #ifdef HAVE_NETDB_H
      #include <sys/socket.h>
      #include <netdb.h>
    #else
      #include <sun/netdb.h>
    #endif
    extern_C struct hostent * gethostbyname (GETHOSTBYNAME_CONST char* name); # siehe GETHOSTENT(3)
  #endif
  #ifndef MAXHOSTNAMELEN
    #define MAXHOSTNAMELEN 64 # siehe <sys/param.h>
  #endif
# wird verwendet von MISC

# Arbeiten mit Sockets:
  #ifdef HAVE_GETHOSTBYNAME
    # Type of a socket
    #define SOCKET  int
    # Error value for functions returning a socket
    #define INVALID_SOCKET  (SOCKET)(-1)
    # Error value for functions returning an `int' status
    #define SOCKET_ERROR  (-1)
    # Accessing the error code
    #define sock_errno  errno
    #define sock_errno_is(val)  (errno == val)
    #define sock_set_errno(val)  (void)(errno = val)
    # Signalling a socket related error
    #define SOCK_error()  OS_error()
    # Reading and writing from a socket
    #define sock_read  full_read
    #define sock_write  full_write
    # Closing a socket
    #define closesocket  close
    # Wrapping and unwrapping of a socket in a Lisp object
    #define allocate_socket(fd)  allocate_handle(fd)
    #define TheSocket(obj)  TheHandle(obj)
  #endif
# wird verwendet von SOCKET, STREAM

# Dynamisches Laden von Modulen:
  #ifdef HAVE_DLOPEN
    #include <dlfcn.h>
    extern_C void* dlopen (const char * library, int flag);
    extern_C void* dlsym (void* handle, DLSYM_CONST char * symbol);
    extern_C int dlclose (void* handle);
    extern_C DLERROR_CONST char * dlerror (void);
    #define HAVE_DYNLOAD
  #endif

# Character set conversion:
  #ifdef HAVE_ICONV
    #include <iconv.h>
    extern_C iconv_t iconv_open (const char * to_code, const char * from_code);
    extern_C size_t iconv (iconv_t cd, ICONV_CONST char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t* outbytesleft);
    extern_C int iconv_close (iconv_t cd);
  #endif


# CLISP als NeXTstep-GUI-Applikation:
  #ifdef NEXTAPP
    # Terminal-Stream, wie ihn nxterminal.m über die Klasse LispServer
    # implementiert.
      extern void nxterminal_send_output (void);
      extern void nxterminal_write_char (unsigned char ch);
      extern void nxterminal_write_string (unsigned char * string);
      extern unsigned char nxterminal_read_char (int* linepos);
      extern int nxterminal_unread_char (void);
      extern int nxterminal_listen (void);
      extern int nxterminal_init (void);
      extern int nxterminal_exit (void);
      extern int nxterminal_line_length;
  #endif

