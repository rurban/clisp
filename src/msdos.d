# Include-File für OS/2-EMUNIX-Version von CLISP
# Bruno Haible 1992-2000


# Konstanten für Steuerzeichen:

#define BEL  7              # Ton ausgeben
# define NL  10             # New line, siehe LISPBIBL.D
#define RUBOUT 127          # Rubout = Delete
#define CRLFstring  "\r\n"  # C-String, der BS-Newline enthält

#define stdin_handle  0  # File-Handle von Standard-Input
#define stdout_handle  1  # File-Handle von Standard-Output

# Deklaration von Typen von Ein-/Ausgabe-Parametern von Betriebssystemfunktionen
  #include <stdlib.h>

# Tabelle der System-Fehlermeldungen
  #include <errno.h>
  #define ENOMORE  18  # Fehlercode bei findfirst/findnext, fehlt in errno.h
  #ifndef errno
    extern int errno; # letzter Fehlercode
  #endif
  #define OS_errno errno
  #define OS_set_errno(e) (errno=(e))
  extern const int sys_nerr; # Anzahl der Betriebssystem-Fehlermeldungen
  extern const char* const sys_errlist[]; # Betriebssystem-Fehlermeldungen
  # siehe PERROR(3)
# wird verwendet von ERROR, STREAM, PATHNAME

# Bereitstellen des Arbeitsspeichers
  extern void* malloc (size_t size); # siehe MALLOC(3V)
  extern void free (void* ptr); # siehe MALLOC(3V)
# wird verwendet von SPVW

# Normales Programmende
  nonreturning_function(extern, _exit, (int status)); # siehe EXIT(2V)
# wird verwendet von SPVW

# Sofortiger Programmabbruch, Sprung in den Debugger
  extern void abort (void); # siehe ABORT(3)
# wird verwendet von SPVW, DEBUG, EVAL, IO

# Signalbehandlung
  #include <signal.h>
  # Ein Signal-Handler ist eine Funktion ohne Ergebnis.
  typedef SIGTY (*signal_handler_t) ();
  extern signal_handler_t signal (int sig, signal_handler_t handler); # siehe SIGNAL(3V)
  # Ein Signal erst eine bestimmte Zeit später ausliefern:
  extern unsigned int alarm (unsigned int seconds); # siehe ALARM(3V)
  # Die Ankunft eines Signals quittieren (aus dem Signal-Handler heraus):
  #define signal_acknowledge(sig,handler)  signal(sig,SIG_ACK)
  # Das Verhalten von Signalen bei System-Calls ist OK:
  #define SIGNAL  signal
  # Ein Signal veranlassen.
  extern int raise (int sig);
# wird verwendet von SPVW

# Environment-Variablen abfragen:
  extern char* getenv (const char* name); # siehe GETENV(3V)
# wird verwendet von PATHNAME, MISC

# Environment-Variablen setzen:
  #define HAVE_PUTENV
  extern int putenv (const char* name); # siehe PUTENV(3)
# wird verwendet von SPVW

# Working Directory setzen:
  extern int chdir (const char* path); # siehe CHDIR(2V)
# wird verwendet von PATHNAME

# Working Directory abfragen:
  # Maximale Pfadlänge (incl. Nullbyte am Schluss), die von getwd geliefert wird:
    #define MAXPATHLEN  1024  # siehe <sys/param.h>
  extern char _getdrive (void);
  extern int _chdrive (char drive);
  extern char* getcwd (char* buf, size_t bufsize);
  extern int _getcwd1 (char* buf, char drive);
# wird verwendet von PATHNAME

# Information zu einem File erfragen:
  extern int access (const char* path, int amode);
  #include <io.h>
  #include <sys/types.h>
  #include <sys/stat.h>
  extern int stat (const char* path, struct stat * buf); # siehe STAT(2V)
  extern int fstat (int fd, struct stat * buf); # siehe STAT(2V)
  # Test auf Directory:
    #undef S_ISDIR
    #define S_ISDIR(m)  (((m)&S_IFMT) == S_IFDIR)
  # Test auf reguläres File:
    #undef S_ISREG # unser Macro hierfür ist effizienter
    #define S_ISREG(m)  (((m)&(S_IFMT&~S_IFREG)) == 0)
  # Links gibt es keine:
    #define S_ISLNK(m)  false
  # siehe auch PATHNAME:get_file_write_datetime()
# wird verwendet von PATHNAME, STREAM

# File löschen:
  extern int unlink (const char* path); # siehe UNLINK(2V)
# wird verwendet von PATHNAME

# File umbenennen:
  extern int rename (const char* oldpath, const char* newpath); # siehe RENAME(2V)
# wird verwendet von PATHNAME

# Directory-Suche:
  #include <dirent.h>
  #if 0
    struct ffblk {
      # gleich aufgebaut wie struct _find in <sys/emx.h>
      char ff_reserved[21];
      unsigned char ff_attrib;
      unsigned short ff_ftime;
      unsigned short ff_fdate;
      unsigned short ff_fsize[2]; # Vorsicht Alignment!
      char ff_name[16];
    };
  #else # so ist's eleganter:
    #include <emx/syscalls.h>
    #define ffblk  _find
    #define ff_reserved  reserved
    #define ff_attrib    attr
    #define ff_ftime     time
    #define ff_fdate     date
    #define ff_fsize     size_lo
    #define ff_name      name
  #endif
  extern int __findfirst (const char* pathname, int attrib, struct ffblk * ffblk);
  extern int __findnext (struct ffblk * ffblk);
  #define findfirst(path,buf,attr)  __findfirst(path,attr,buf)
  #define findnext(buf)  __findnext(buf)
  #define FA_DIREC   A_DIR
  #define FA_ARCH    A_ARCHIVE
  #define FA_RDONLY  A_RONLY
  #if 0
    # Kompatibel zu UNIX:
    #define SDIRENT  struct dirent
    extern DIR* opendir (const char* dirname); # siehe DIRECTORY(3V)
    extern SDIRENT* readdir (DIR* dirp); # siehe DIRECTORY(3V)
    extern void closedir (DIR* dirp); # siehe DIRECTORY(3V)
    #define CLOSEDIR(dirp)  (closedir(dirp),0)
  #endif
# wird verwendet von PATHNAME

# Directory anlegen:
  extern int mkdir (const char* path, long attrib);
# wird verwendet von PATHNAME

# Directory löschen:
  extern int rmdir (const char* path); # siehe RMDIR(2V)
# wird verwendet von PATHNAME

# Arbeiten mit offenen Files:
  #include <sys/types.h>
  #include <sys/file.h>
  #include <io.h>
  extern int open (const char* path, int flags, ...); # siehe OPEN(2V)
  # EMX definiert creat(path,mode) == open(path,O_WRONLY|O_TRUNC|O_CREAT,mode),
  # was wir wegen unserer Bufferung in STREAM nicht brauchen können.
  #define creat(path,mode)  open(path,O_RDWR|O_TRUNC|O_CREAT,mode)
  #define my_open_mask  0644
  extern int setmode (int fd, int mode); # mode = O_TEXT oder O_BINARY
  # File-Modus: i.a. unbestimmt. (Nur bei EMUNIX kann man bei open() O_TEXT
  #   bzw. O_BINARY im 2. Argument angeben.) Daher ein setmode() nötig.
  #define Handle  uintW  # Typ eines File-Deskriptors
  extern off_t lseek (int fd, off_t offset, int whence); # siehe LSEEK(2V)
  extern int read (int fd, void* buf, size_t nbyte); # siehe READ(2V)
  extern int write (int fd, const void* buf, size_t nbyte); # siehe WRITE(2V)
  extern int close (int fd); # siehe CLOSE(2V)
  extern int dup (int fd); # siehe DUP(2V)
  extern int fsync (int fd); # siehe FSYNC(2)
  #include <sys/time.h>
  extern int select (int width, fd_set* readfds, fd_set* writefds, fd_set* exceptfds,
                     struct timeval * timeout); # siehe SELECT(2)
  # Wrapper um die System-Aufrufe, die Teilergebnisse und evtl. EINTR behandeln:
  extern int read_helper (int fd, void* buf, size_t nbyte, bool partial_p);
  #define safe_read(f,b,n)  read_helper(f,b,n,true)
  #define full_read(f,b,n)  read_helper(f,b,n,false)
  extern int full_write (int fd, const void* buf, size_t nbyte);
  #define OPEN open
  #define CLOSE close
# wird verwendet von STREAM, PATHNAME, SPVW, MISC

# Terminal-Abfragen:
  extern int isatty (int fd); # siehe TTYNAME(3V)
  extern int ioctl (int fd, int request, ...); # siehe IOCTL(2)
  extern int __ioctl1 (int fd, int code); # führt einen INT 21,44,code aus
  #define CADDR_T  unsigned long
  #include <sys/ioctl.h>
  extern int eof (int fd); # meldet, ob EOF erreicht
  extern void _scrsize (int* dst); # dst[0]:=columns, dst[1]:=rows
  # vgl. UNIX_TERM_TERMIO
  #include <sys/termio.h> # siehe TERMIO(4)
  extern int tgetent (char* bp, char* name); # siehe TERMCAP(3X)
  extern int tgetnum (char* id); # siehe TERMCAP(3X)
  # "There's a bug in ioctl (..., TCSETAF, ...), which causes the new mode
  # flags to be ignored.  Use TCSETA instead.  If you have to flush the
  # buffer, use TCSETAF, then TCSETA." Eberhard Mattes 9.3.1993
  #undef TCSETAF
  #define TCSETAF  TCSETA
# wird verwendet von SPVW, STREAM

# Tastatur abfragen, direkte Bildschirm-Ausgabe
  #include <sys/video.h>
  extern int v_init (void);
  extern int v_hardware (void);
  extern void v_dimen (int* width, int* height);
  extern void v_getctype (int* start, int* end);
  extern void v_ctype (int start, int end);
  extern void v_attrib (int attrib);
  extern void v_putc (char c);
  extern void v_putn (char c, int count);
  extern void v_getxy (int* x, int* y);
  extern void v_gotoxy (int x, int y);
  extern void v_clear (void);
  extern void v_clreol (void);
  extern void v_delline (int count);
  extern void v_insline (int count);
  extern void v_scroll (int x_left, int y_top, int x_right, int y_bottom, int count, int direction);
# wird verwendet von STREAM

# Datum/Uhrzeit verarbeiten:
  #include <time.h>
  extern struct tm * localtime (const time_t* clock); # siehe CTIME(3V)
# wird verwendet von SPVW

# Datum/Uhrzeit abfragen:
  #include <sys/timeb.h>
  extern void __ftime (struct timeb * time); # ftime() ohne Zeitzone
  # siehe auch SPVW.D:get_time() und SPVW.D:get_decoded_time()
# wird verwendet von SPVW

# Programme aufrufen:
  #include <process.h>
  extern int spawnv (int pmode, const char* path, char* const argv[]);
  extern int system (const char* command);
  # system(NULL) stellt fest, ob ein Kommandoprozessor zur Verfügung steht.
  # system(command) übergibt dem Kommandoprozessor einen Befehl.
# wird verwendet von PATHNAME

# Programme aufrufen:
  extern FILE* popen (const char* command, const char* mode);
  extern int pclose (FILE* f);
  extern int fileno (FILE* f);
# wird verwendet von STREAM

# "Zufalls"zahlen besorgen:
  #include <time.h>
  extern time_t time (time_t* tloc); # siehe TIME(3V)
# wird verwendet von LISPARIT


# Versionsabfragen:
  #include <stdlib.h>
  extern const unsigned char _osmode;
  extern const unsigned char _osmajor;
  extern const unsigned char _osminor;
# wird verwendet von SPVW, PATHNAME, STREAM, MISC


# Umgehen eines lästigen ENAMETOOLONG Errors bei Benutzung von langen
# Filenamen auf FAT-Drives unter OS/2:
  #define chdir  my_chdir
  #define access  my_access
  #define stat(x,y)  my_stat(x,y)
  #define unlink  my_unlink
  #define rename  my_rename
  #define __findfirst  my___findfirst
  #undef mkdir
  #define mkdir  my_mkdir
  #define open(x,y)  my_open(x,y)
  #undef creat
  #define creat  my_creat
  #define spawnv  my_spawnv
  extern int chdir (const char* path); # siehe CHDIR(2V)
  extern int access (const char* path, int amode);
  extern int stat (const char* path, struct stat * buf); # siehe STAT(2V)
  extern int unlink (const char* path); # siehe UNLINK(2V)
  extern int rename (const char* oldpath, const char* newpath); # siehe RENAME(2V)
  extern int __findfirst (const char* pathname, int attrib, struct ffblk * ffblk);
  extern int mkdir (const char* path, long attrib);
  extern int open (const char* path, int flags); # siehe OPEN(2V)
  extern int creat (const char* path, int pmode);
  extern int spawnv (int pmode, const char* path, const char* const argv[]);


# In der OS/2 - Version kann man folgendes verwenden:
# - _sleep2()
# - Kommando-Interpreter: cmd.exe, nicht command.com .
# File-Funktionen austesten!!
