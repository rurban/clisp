  # Behandlung von UNIX-Fehlern
  # OS_error();
  # > int errno: Fehlercode
    nonreturning_function(global, OS_error, (void));
    nonreturning_function(global, OS_file_error, (object pathname));

  # Problem: viele verschiedene UNIX-Versionen, jede wieder mit anderen
  # Fehlermeldungen.
  # Abhilfe: Die Fehlernamen sind einigermaßen portabel. Die englische
  # Fehlermeldung übernehmen wir, die Übersetzungen machen wir selbst.
  # Französische Fehlermeldungen von Tristan <marc@david.saclay.cea.fr>.

  #if !(defined(UNIX) || defined(EMUNIX) || defined(WATCOM)) # Konflikt mit unix.d bzw. msdos.d bzw. <stdlib.h> vermeiden
    extern int sys_nerr; # Anzahl der Betriebssystem-Fehlermeldungen
    extern char* sys_errlist[]; # Betriebssystem-Fehlermeldungen
  #endif

  # Tabelle der Fehlermeldungen und ihrer Namen:
    typedef struct { const char* name; const char* msg; } os_error_t;
    local os_error_t* errormsg_table;

  #ifdef GNU_GETTEXT
    # Translate the messages when we access the table, not when filling it.
    #define clgettext
  #endif

  # Initialisierung der Tabelle:
    global int init_errormsg_table (void);
    global int init_errormsg_table()
      { var uintC i;
        begin_system_call();
        errormsg_table = (os_error_t*) malloc(sys_nerr * sizeof(os_error_t));
        end_system_call();
        if (errormsg_table == NULL) # Speicher reicht nicht?
          { return -1; }
        # Tabelle vor-initialisieren:
        for (i=0; i<sys_nerr; i++)
          { errormsg_table[i].name = "";
            errormsg_table[i].msg = sys_errlist[i];
          }
        # Tabelle initialisieren:
        # Obacht: Auf sys_nerr ist kein Verlass. (Bei IRIX 5.2 ist EDQUOT >= sys_nerr !)
        /* allgemein verbreitete UNIX-Errors: */
        #ifdef EPERM
        if (EPERM < sys_nerr) {
        errormsg_table[EPERM].name = "EPERM";
        errormsg_table[EPERM].msg = GETTEXT("Operation not permitted");
        }
        #endif
        #ifdef ENOENT
        if (ENOENT < sys_nerr) {
        errormsg_table[ENOENT].name = "ENOENT";
        errormsg_table[ENOENT].msg = GETTEXT("No such file or directory");
        }
        #endif
        #ifdef ESRCH
        if (ESRCH < sys_nerr) {
        errormsg_table[ESRCH].name = "ESRCH";
        errormsg_table[ESRCH].msg = GETTEXT("No such process");
        }
        #endif
        #ifdef EINTR
        if (EINTR < sys_nerr) {
        errormsg_table[EINTR].name = "EINTR";
        errormsg_table[EINTR].msg = GETTEXT("Interrupted system call");
        }
        #endif
        #ifdef EIO
        if (EIO < sys_nerr) {
        errormsg_table[EIO].name = "EIO";
        errormsg_table[EIO].msg = GETTEXT("I/O error");
        }
        #endif
        #ifdef ENXIO
        if (ENXIO < sys_nerr) {
        errormsg_table[ENXIO].name = "ENXIO";
        errormsg_table[ENXIO].msg = GETTEXT("No such device or address");
        }
        #endif
        #ifdef E2BIG
        if (E2BIG < sys_nerr) {
        errormsg_table[E2BIG].name = "E2BIG";
        errormsg_table[E2BIG].msg = GETTEXT("Arg list too long");
        }
        #endif
        #ifdef ENOEXEC
        if (ENOEXEC < sys_nerr) {
        errormsg_table[ENOEXEC].name = "ENOEXEC";
        errormsg_table[ENOEXEC].msg = GETTEXT("Exec format error");
        }
        #endif
        #ifdef EBADF
        if (EBADF < sys_nerr) {
        errormsg_table[EBADF].name = "EBADF";
        errormsg_table[EBADF].msg = GETTEXT("Bad file number");
        }
        #endif
        #ifdef ECHILD
        if (ECHILD < sys_nerr) {
        errormsg_table[ECHILD].name = "ECHILD";
        errormsg_table[ECHILD].msg = GETTEXT("No child processes");
        }
        #endif
        #ifdef EAGAIN
        if (EAGAIN < sys_nerr) {
        errormsg_table[EAGAIN].name = "EAGAIN";
        errormsg_table[EAGAIN].msg = GETTEXT("No more processes");
        }
        #endif
        #ifdef ENOMEM
        if (ENOMEM < sys_nerr) {
        errormsg_table[ENOMEM].name = "ENOMEM";
        errormsg_table[ENOMEM].msg = GETTEXT("Not enough memory");
        }
        #endif
        #ifdef EACCES
        if (EACCES < sys_nerr) {
        errormsg_table[EACCES].name = "EACCES";
        errormsg_table[EACCES].msg = GETTEXT("Permission denied");
        }
        #endif
        #ifdef EFAULT
        if (EFAULT < sys_nerr) {
        errormsg_table[EFAULT].name = "EFAULT";
        errormsg_table[EFAULT].msg = GETTEXT("Bad address");
        }
        #endif
        #ifdef ENOTBLK
        if (ENOTBLK < sys_nerr) {
        errormsg_table[ENOTBLK].name = "ENOTBLK";
        errormsg_table[ENOTBLK].msg = GETTEXT("Block device required");
        }
        #endif
        #ifdef EBUSY
        if (EBUSY < sys_nerr) {
        errormsg_table[EBUSY].name = "EBUSY";
        errormsg_table[EBUSY].msg = GETTEXT("Device busy");
        }
        #endif
        #ifdef EEXIST
        if (EEXIST < sys_nerr) {
        errormsg_table[EEXIST].name = "EEXIST";
        errormsg_table[EEXIST].msg = GETTEXT("File exists");
        }
        #endif
        #ifdef EXDEV
        if (EXDEV < sys_nerr) {
        errormsg_table[EXDEV].name = "EXDEV";
        errormsg_table[EXDEV].msg = GETTEXT("Cross-device link");
        }
        #endif
        #ifdef ENODEV
        if (ENODEV < sys_nerr) {
        errormsg_table[ENODEV].name = "ENODEV";
        errormsg_table[ENODEV].msg = GETTEXT("No such device");
        }
        #endif
        #ifdef ENOTDIR
        if (ENOTDIR < sys_nerr) {
        errormsg_table[ENOTDIR].name = "ENOTDIR";
        errormsg_table[ENOTDIR].msg = GETTEXT("Not a directory");
        }
        #endif
        #ifdef EISDIR
        if (EISDIR < sys_nerr) {
        errormsg_table[EISDIR].name = "EISDIR";
        errormsg_table[EISDIR].msg = GETTEXT("Is a directory");
        }
        #endif
        #ifdef EINVAL
        if (EINVAL < sys_nerr) {
        errormsg_table[EINVAL].name = "EINVAL";
        errormsg_table[EINVAL].msg = GETTEXT("Invalid argument");
        }
        #endif
        #ifdef ENFILE
        if (ENFILE < sys_nerr) {
        errormsg_table[ENFILE].name = "ENFILE";
        errormsg_table[ENFILE].msg = GETTEXT("File table overflow");
        }
        #endif
        #ifdef EMFILE
        if (EMFILE < sys_nerr) {
        errormsg_table[EMFILE].name = "EMFILE";
        errormsg_table[EMFILE].msg = GETTEXT("Too many open files");
        }
        #endif
        #ifdef ENOTTY
        if (ENOTTY < sys_nerr) {
        errormsg_table[ENOTTY].name = "ENOTTY";
        errormsg_table[ENOTTY].msg = GETTEXT("Inappropriate ioctl for device");
        }
        #endif
        #ifdef ETXTBSY
        if (ETXTBSY < sys_nerr) {
        errormsg_table[ETXTBSY].name = "ETXTBSY";
        errormsg_table[ETXTBSY].msg = GETTEXT("Text file busy");
        }
        #endif
        #ifdef EFBIG
        if (EFBIG < sys_nerr) {
        errormsg_table[EFBIG].name = "EFBIG";
        errormsg_table[EFBIG].msg = GETTEXT("File too large");
        }
        #endif
        #ifdef ENOSPC
        if (ENOSPC < sys_nerr) {
        errormsg_table[ENOSPC].name = "ENOSPC";
        errormsg_table[ENOSPC].msg = GETTEXT("No space left on device");
        }
        #endif
        #ifdef ESPIPE
        if (ESPIPE < sys_nerr) {
        errormsg_table[ESPIPE].name = "ESPIPE";
        errormsg_table[ESPIPE].msg = GETTEXT("Illegal seek");
        }
        #endif
        #ifdef EROFS
        if (EROFS < sys_nerr) {
        errormsg_table[EROFS].name = "EROFS";
        errormsg_table[EROFS].msg = GETTEXT("Read-only file system");
        }
        #endif
        #ifdef EMLINK
        if (EMLINK < sys_nerr) {
        errormsg_table[EMLINK].name = "EMLINK";
        errormsg_table[EMLINK].msg = GETTEXT("Too many links");
        }
        #endif
        #ifdef EPIPE
        if (EPIPE < sys_nerr) {
        errormsg_table[EPIPE].name = "EPIPE";
        errormsg_table[EPIPE].msg = GETTEXT("Broken pipe, child process terminated");
          # Note that these "translations" exploit that CLISP only catches
          # SIGPIPEs from subprocesses. Other pipes lead to a deadly signal
          # and never to this error message.
        }
        #endif
        /* Errors bei mathematischen Funktionen: */
        #ifdef EDOM
        if (EDOM < sys_nerr) {
        errormsg_table[EDOM].name = "EDOM";
        errormsg_table[EDOM].msg = GETTEXT("Argument out of domain");
        }
        #endif
        #ifdef ERANGE
        if (ERANGE < sys_nerr) {
        errormsg_table[ERANGE].name = "ERANGE";
        errormsg_table[ERANGE].msg = GETTEXT("Result too large");
        }
        #endif
        /* Errors bei Non-Blocking I/O und Interrupt I/O: */
        #ifdef EWOULDBLOCK
        if (EWOULDBLOCK < sys_nerr) {
        errormsg_table[EWOULDBLOCK].name = "EWOULDBLOCK";
        errormsg_table[EWOULDBLOCK].msg = GETTEXT("Operation would block");
        }
        #endif
        #ifdef EINPROGRESS
        if (EINPROGRESS < sys_nerr) {
        errormsg_table[EINPROGRESS].name = "EINPROGRESS";
        errormsg_table[EINPROGRESS].msg = GETTEXT("Operation now in progress");
        }
        #endif
        #ifdef EALREADY
        if (EALREADY < sys_nerr) {
        errormsg_table[EALREADY].name = "EALREADY";
        errormsg_table[EALREADY].msg = GETTEXT("Operation already in progress");
        }
        #endif
        /* weitere allgemein übliche Errors: */
        #ifdef ELOOP
        if (ELOOP < sys_nerr) {
        errormsg_table[ELOOP].name = "ELOOP";
        errormsg_table[ELOOP].msg = GETTEXT("Too many levels of symbolic links");
        }
        #endif
        #ifdef ENAMETOOLONG
        if (ENAMETOOLONG < sys_nerr) {
        errormsg_table[ENAMETOOLONG].name = "ENAMETOOLONG";
        errormsg_table[ENAMETOOLONG].msg = GETTEXT("File name too long");
        }
        #endif
        #ifdef ENOTEMPTY
        if (ENOTEMPTY < sys_nerr) {
        errormsg_table[ENOTEMPTY].name = "ENOTEMPTY";
        errormsg_table[ENOTEMPTY].msg = GETTEXT("Directory not empty");
        }
        #endif
        /* Errors im Zusammenhang mit Network File System (NFS): */
        #ifdef ESTALE
        if (ESTALE < sys_nerr) {
        errormsg_table[ESTALE].name = "ESTALE";
        errormsg_table[ESTALE].msg = GETTEXT("Stale NFS file handle");
        }
        #endif
        #ifdef EREMOTE
        if (EREMOTE < sys_nerr) {
        errormsg_table[EREMOTE].name = "EREMOTE";
        errormsg_table[EREMOTE].msg = GETTEXT("Too many levels of remote in path");
        }
        #endif
        /* Errors im Zusammenhang mit Sockets, IPC und Netzwerk: */
        #ifdef ENOTSOCK
        if (ENOTSOCK < sys_nerr) {
        errormsg_table[ENOTSOCK].name = "ENOTSOCK";
        errormsg_table[ENOTSOCK].msg = GETTEXT("Socket operation on non-socket");
        }
        #endif
        #ifdef EDESTADDRREQ
        if (EDESTADDRREQ < sys_nerr) {
        errormsg_table[EDESTADDRREQ].name = "EDESTADDRREQ";
        errormsg_table[EDESTADDRREQ].msg = GETTEXT("Destination address required");
        }
        #endif
        #ifdef EMSGSIZE
        if (EMSGSIZE < sys_nerr) {
        errormsg_table[EMSGSIZE].name = "EMSGSIZE";
        errormsg_table[EMSGSIZE].msg = GETTEXT("Message too long");
        }
        #endif
        #ifdef EPROTOTYPE
        if (EPROTOTYPE < sys_nerr) {
        errormsg_table[EPROTOTYPE].name = "EPROTOTYPE";
        errormsg_table[EPROTOTYPE].msg = GETTEXT("Protocol wrong type for socket");
        }
        #endif
        #ifdef ENOPROTOOPT
        if (ENOPROTOOPT < sys_nerr) {
        errormsg_table[ENOPROTOOPT].name = "ENOPROTOOPT";
        errormsg_table[ENOPROTOOPT].msg = GETTEXT("Option not supported by protocol");
        }
        #endif
        #ifdef EPROTONOSUPPORT
        if (EPROTONOSUPPORT < sys_nerr) {
        errormsg_table[EPROTONOSUPPORT].name = "EPROTONOSUPPORT";
        errormsg_table[EPROTONOSUPPORT].msg = GETTEXT("Protocol not supported");
        }
        #endif
        #ifdef ESOCKTNOSUPPORT
        if (ESOCKTNOSUPPORT < sys_nerr) {
        errormsg_table[ESOCKTNOSUPPORT].name = "ESOCKTNOSUPPORT";
        errormsg_table[ESOCKTNOSUPPORT].msg = GETTEXT("Socket type not supported");
        }
        #endif
        #ifdef EOPNOTSUPP
        if (EOPNOTSUPP < sys_nerr) {
        errormsg_table[EOPNOTSUPP].name = "EOPNOTSUPP";
        errormsg_table[EOPNOTSUPP].msg = GETTEXT("Operation not supported on socket");
        }
        #endif
        #ifdef EPFNOSUPPORT
        if (EPFNOSUPPORT < sys_nerr) {
        errormsg_table[EPFNOSUPPORT].name = "EPFNOSUPPORT";
        errormsg_table[EPFNOSUPPORT].msg = GETTEXT("Protocol family not supported");
        }
        #endif
        #ifdef EAFNOSUPPORT
        if (EAFNOSUPPORT < sys_nerr) {
        errormsg_table[EAFNOSUPPORT].name = "EAFNOSUPPORT";
        errormsg_table[EAFNOSUPPORT].msg = GETTEXT("Address family not supported by protocol family");
        }
        #endif
        #ifdef EADDRINUSE
        if (EADDRINUSE < sys_nerr) {
        errormsg_table[EADDRINUSE].name = "EADDRINUSE";
        errormsg_table[EADDRINUSE].msg = GETTEXT("Address already in use");
        }
        #endif
        #ifdef EADDRNOTAVAIL
        if (EADDRNOTAVAIL < sys_nerr) {
        errormsg_table[EADDRNOTAVAIL].name = "EADDRNOTAVAIL";
        errormsg_table[EADDRNOTAVAIL].msg = GETTEXT("Can't assign requested address");
        }
        #endif
        #ifdef ENETDOWN
        if (ENETDOWN < sys_nerr) {
        errormsg_table[ENETDOWN].name = "ENETDOWN";
        errormsg_table[ENETDOWN].msg = GETTEXT("Network is down");
        }
        #endif
        #ifdef ENETUNREACH
        if (ENETUNREACH < sys_nerr) {
        errormsg_table[ENETUNREACH].name = "ENETUNREACH";
        errormsg_table[ENETUNREACH].msg = GETTEXT("Network is unreachable");
        }
        #endif
        #ifdef ENETRESET
        if (ENETRESET < sys_nerr) {
        errormsg_table[ENETRESET].name = "ENETRESET";
        errormsg_table[ENETRESET].msg = GETTEXT("Network dropped connection on reset");
        }
        #endif
        #ifdef ECONNABORTED
        if (ECONNABORTED < sys_nerr) {
        errormsg_table[ECONNABORTED].name = "ECONNABORTED";
        errormsg_table[ECONNABORTED].msg = GETTEXT("Software caused connection abort");
        }
        #endif
        #ifdef ECONNRESET
        if (ECONNRESET < sys_nerr) {
        errormsg_table[ECONNRESET].name = "ECONNRESET";
        errormsg_table[ECONNRESET].msg = GETTEXT("Connection reset by peer");
        }
        #endif
        #ifdef ENOBUFS
        if (ENOBUFS < sys_nerr) {
        errormsg_table[ENOBUFS].name = "ENOBUFS";
        errormsg_table[ENOBUFS].msg = GETTEXT("No buffer space available");
        }
        #endif
        #ifdef EISCONN
        if (EISCONN < sys_nerr) {
        errormsg_table[EISCONN].name = "EISCONN";
        errormsg_table[EISCONN].msg = GETTEXT("Socket is already connected");
        }
        #endif
        #ifdef ENOTCONN
        if (ENOTCONN < sys_nerr) {
        errormsg_table[ENOTCONN].name = "ENOTCONN";
        errormsg_table[ENOTCONN].msg = GETTEXT("Socket is not connected");
        }
        #endif
        #ifdef ESHUTDOWN
        if (ESHUTDOWN < sys_nerr) {
        errormsg_table[ESHUTDOWN].name = "ESHUTDOWN";
        errormsg_table[ESHUTDOWN].msg = GETTEXT("Can't send after socket shutdown");
        }
        #endif
        #ifdef ETOOMANYREFS
        if (ETOOMANYREFS < sys_nerr) {
        errormsg_table[ETOOMANYREFS].name = "ETOOMANYREFS";
        errormsg_table[ETOOMANYREFS].msg = GETTEXT("Too many references: can't splice");
        }
        #endif
        #ifdef ETIMEDOUT
        if (ETIMEDOUT < sys_nerr) {
        errormsg_table[ETIMEDOUT].name = "ETIMEDOUT";
        errormsg_table[ETIMEDOUT].msg = GETTEXT("Connection timed out");
        }
        #endif
        #ifdef ECONNREFUSED
        if (ECONNREFUSED < sys_nerr) {
        errormsg_table[ECONNREFUSED].name = "ECONNREFUSED";
        errormsg_table[ECONNREFUSED].msg = GETTEXT("Connection refused");
        }
        #endif
        #if 0
        errormsg_table[].name = "";
        errormsg_table[].msg = GETTEXT("Remote peer released connection");
        #endif
        #ifdef EHOSTDOWN
        if (EHOSTDOWN < sys_nerr) {
        errormsg_table[EHOSTDOWN].name = "EHOSTDOWN";
        errormsg_table[EHOSTDOWN].msg = GETTEXT("Host is down");
        }
        #endif
        #ifdef EHOSTUNREACH
        if (EHOSTUNREACH < sys_nerr) {
        errormsg_table[EHOSTUNREACH].name = "EHOSTUNREACH";
        errormsg_table[EHOSTUNREACH].msg = GETTEXT("Host is unreachable");
        }
        #endif
        #if 0
        errormsg_table[].name = "";
        errormsg_table[].msg = GETTEXT("Networking error");
        #endif
        /* Quotas: */
        #ifdef EPROCLIM
        if (EPROCLIM < sys_nerr) {
        errormsg_table[EPROCLIM].name = "EPROCLIM";
        errormsg_table[EPROCLIM].msg = GETTEXT("Too many processes");
        }
        #endif
        #ifdef EUSERS
        if (EUSERS < sys_nerr) {
        errormsg_table[EUSERS].name = "EUSERS";
        errormsg_table[EUSERS].msg = GETTEXT("Too many users");
        }
        #endif
        #ifdef EDQUOT
        if (EDQUOT < sys_nerr) {
        errormsg_table[EDQUOT].name = "EDQUOT";
        errormsg_table[EDQUOT].msg = GETTEXT("Disk quota exceeded");
        }
        #endif
        /* Errors im Zusammenhang mit STREAMS: */
        #ifdef ENOSTR
        if (ENOSTR < sys_nerr) {
        errormsg_table[ENOSTR].name = "ENOSTR";
        errormsg_table[ENOSTR].msg = GETTEXT("Not a stream device");
        }
        #endif
        #ifdef ETIME
        if (ETIME < sys_nerr) {
        errormsg_table[ETIME].name = "ETIME";
        errormsg_table[ETIME].msg = GETTEXT("Timer expired");
        }
        #endif
        #ifdef ENOSR
        if (ENOSR < sys_nerr) {
        errormsg_table[ENOSR].name = "ENOSR";
        errormsg_table[ENOSR].msg = GETTEXT("Out of stream resources");
        }
        #endif
        #ifdef ENOMSG
        if (ENOMSG < sys_nerr) {
        errormsg_table[ENOMSG].name = "ENOMSG";
        errormsg_table[ENOMSG].msg = GETTEXT("No message of desired type");
        }
        #endif
        #ifdef EBADMSG
        if (EBADMSG < sys_nerr) {
        errormsg_table[EBADMSG].name = "EBADMSG";
        errormsg_table[EBADMSG].msg = GETTEXT("Not a data message");
        }
        #endif
        /* Errors bei SystemV IPC: */
        #ifdef EIDRM
        if (EIDRM < sys_nerr) {
        errormsg_table[EIDRM].name = "EIDRM";
        errormsg_table[EIDRM].msg = GETTEXT("Identifier removed");
        }
        #endif
        /* Errors bei SystemV Record-Locking: */
        #ifdef EDEADLK
        if (EDEADLK < sys_nerr) {
        errormsg_table[EDEADLK].name = "EDEADLK";
        errormsg_table[EDEADLK].msg = GETTEXT("Resource deadlock would occur");
        }
        #endif
        #ifdef ENOLCK
        if (ENOLCK < sys_nerr) {
        errormsg_table[ENOLCK].name = "ENOLCK";
        errormsg_table[ENOLCK].msg = GETTEXT("No record locks available");
        }
        #endif
        /* Errors bei Remote File System (RFS): */
        #ifdef ENONET
        if (ENONET < sys_nerr) {
        errormsg_table[ENONET].name = "ENONET";
        errormsg_table[ENONET].msg = GETTEXT("Machine is not on the network");
        }
        #endif
        #ifdef EREMOTE
        if (EREMOTE < sys_nerr) {
        errormsg_table[EREMOTE].name = "EREMOTE";
        errormsg_table[EREMOTE].msg = GETTEXT("Object is remote");
        }
        #endif
        #ifdef ERREMOTE
        if (ERREMOTE < sys_nerr) {
        errormsg_table[ERREMOTE].name = "ERREMOTE";
        errormsg_table[ERREMOTE].msg = GETTEXT("Object is remote");
        }
        #endif
        #ifdef ENOLINK
        if (ENOLINK < sys_nerr) {
        errormsg_table[ENOLINK].name = "ENOLINK";
        errormsg_table[ENOLINK].msg = GETTEXT("Link has been severed");
        }
        #endif
        #ifdef EADV
        if (EADV < sys_nerr) {
        errormsg_table[EADV].name = "EADV";
        errormsg_table[EADV].msg = GETTEXT("Advertise error");
        }
        #endif
        #ifdef ESRMNT
        if (ESRMNT < sys_nerr) {
        errormsg_table[ESRMNT].name = "ESRMNT";
        errormsg_table[ESRMNT].msg = GETTEXT("Srmount error");
        }
        #endif
        #ifdef ECOMM
        if (ECOMM < sys_nerr) {
        errormsg_table[ECOMM].name = "ECOMM";
        errormsg_table[ECOMM].msg = GETTEXT("Communication error on send");
        }
        #endif
        #ifdef EPROTO
        if (EPROTO < sys_nerr) {
        errormsg_table[EPROTO].name = "EPROTO";
        errormsg_table[EPROTO].msg = GETTEXT("Protocol error");
        }
        #endif
        #ifdef EMULTIHOP
        if (EMULTIHOP < sys_nerr) {
        errormsg_table[EMULTIHOP].name = "EMULTIHOP";
        errormsg_table[EMULTIHOP].msg = GETTEXT("Multihop attempted");
        }
        #endif
        #ifdef EDOTDOT
        if (EDOTDOT < sys_nerr) {
        errormsg_table[EDOTDOT].name = "EDOTDOT";
        errormsg_table[EDOTDOT].msg =
          "";
        }
        #endif
        #ifdef EREMCHG
        if (EREMCHG < sys_nerr) {
        errormsg_table[EREMCHG].name = "EREMCHG";
        errormsg_table[EREMCHG].msg = GETTEXT("Remote address changed");
        }
        #endif
        /* Errors von POSIX: */
        #ifdef ENOSYS
        if (ENOSYS < sys_nerr) {
        errormsg_table[ENOSYS].name = "ENOSYS";
        errormsg_table[ENOSYS].msg = GETTEXT("Function not implemented");
        }
        #endif
        /* Sonstige: */
        #ifdef EMSDOS /* emx 0.8e - 0.8h */
        if (EMSDOS < sys_nerr) {
        errormsg_table[EMSDOS].name = "EMSDOS";
        errormsg_table[EMSDOS].msg = GETTEXT("Not supported under MS-DOS");
        }
        #endif
        return 0;
      }

  #ifdef GNU_GETTEXT
    # Now translate when accessing the table.
    #undef clgettext
    #define translate(string)  clgettext(string)
  #else
    #define translate(string)  string
  #endif

    local void OS_error_internal (uintC errcode);
    local void OS_error_internal(errcode)
      var uintC errcode;
      { # Meldungbeginn ausgeben:
        #ifdef UNIX
        write_errorstring(GETTEXT("UNIX error "));
        #else
        write_errorstring(GETTEXT("UNIX library error "));
        #endif
        # Fehlernummer ausgeben:
        write_errorobject(fixnum(errcode));
        #if 0
        { # Fehlermeldung des Betriebssystems ausgeben:
          if (errcode < sys_nerr)
            { var const char* errormsg = translate(sys_errlist[errcode]);
              write_errorasciz(": ");
              write_errorasciz(errormsg);
        }   }
        #else # nach Möglichkeit noch ausführlicher:
        { # eigene Fehlermeldung ausgeben:
          if (errcode < sys_nerr)
            # Zu dieser Fehlernummer ist ein Text da.
            { var const char* errorname = errormsg_table[errcode].name;
              var const char* errormsg = translate(errormsg_table[errcode].msg);
              if (!(errorname[0] == 0)) # bekannter Name?
                { write_errorasciz(" (");
                  write_errorasciz(errorname);
                  write_errorasciz(")");
                }
              if (!(errormsg[0] == 0)) # nichtleere Meldung?
                { write_errorasciz(": ");
                  write_errorasciz(errormsg);
                }
        }   }
        #endif
      }
    global void OS_error()
      { var uintC errcode; # positive Fehlernummer
        end_system_call(); # just in case
        begin_system_call();
        errcode = errno;
        errno = 0; # Fehlercode löschen (fürs nächste Mal)
        end_system_call();
        clr_break_sem_4(); # keine UNIX-Operation mehr aktiv
        begin_error(); # Fehlermeldung anfangen
        if (!nullp(STACK_3)) # *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ?
          { STACK_3 = S(simple_os_error); }
        OS_error_internal(errcode);
        end_error(args_end_pointer STACKop 7); # Fehlermeldung beenden
      }
    global void OS_file_error(pathname)
      var object pathname;
      { var uintC errcode; # positive Fehlernummer
        begin_system_call();
        errcode = errno;
        errno = 0; # Fehlercode löschen (fürs nächste Mal)
        end_system_call();
        clr_break_sem_4(); # keine UNIX-Operation mehr aktiv
        pushSTACK(pathname); # Wert von PATHNAME für FILE-ERROR
        begin_error(); # Fehlermeldung anfangen
        if (!nullp(STACK_3)) # *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ?
          { STACK_3 = S(simple_file_error); }
        OS_error_internal(errcode);
        end_error(args_end_pointer STACKop 7); # Fehlermeldung beenden
      }

  # Ausgabe eines Fehlers, direkt übers Betriebssystem
  # errno_out(errorcode);
  # > int errorcode: Fehlercode
    global void errno_out (int errorcode);
    global void errno_out(errorcode)
      var int errorcode;
      { asciz_out(" errno = ");
        if ((uintL)errorcode < sys_nerr)
          { var const char* errorname = errormsg_table[errorcode].name;
            var const char* errormsg = translate(errormsg_table[errorcode].msg);
            if (!(errorname[0] == 0)) # bekannter Name?
              { asciz_out(errorname); }
              else
              { dez_out(errorcode); }
            if (!(errormsg[0] == 0)) # nichtleere Meldung?
              { asciz_out(": "); asciz_out(errormsg); }
          }
          else
          { dez_out(errorcode); }
        asciz_out("." NLstring);
      }

