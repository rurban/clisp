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
    typedef struct { const char* name; const char* msg; } os_error;
    local os_error* errormsg_table;

  #ifdef GNU_GETTEXT
    # Translate the messages when we access the table, not when filling it.
    #define clgettext
  #endif

  # Initialisierung der Tabelle:
    global int init_errormsg_table (void);
    global int init_errormsg_table()
      { var uintC i;
        begin_system_call();
        errormsg_table = (os_error*) malloc(sys_nerr * sizeof(os_error));
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
        errormsg_table[EPERM].msg =
          ENGLISH ? "Operation not permitted" :
          DEUTSCH ? "Keine Berechtigung dazu" :
          FRANCAIS ? "Opération non autorisée" :
          "";
        }
        #endif
        #ifdef ENOENT
        if (ENOENT < sys_nerr) {
        errormsg_table[ENOENT].name = "ENOENT";
        errormsg_table[ENOENT].msg =
          ENGLISH ? "No such file or directory" :
          DEUTSCH ? "File oder Directory existiert nicht" :
          FRANCAIS ? "Fichier ou répertoire non existant" :
          "";
        }
        #endif
        #ifdef ESRCH
        if (ESRCH < sys_nerr) {
        errormsg_table[ESRCH].name = "ESRCH";
        errormsg_table[ESRCH].msg =
          ENGLISH ? "No such process" :
          DEUTSCH ? "Dieser Prozess existiert nicht (mehr)" :
          FRANCAIS ? "Processus inexistant" :
          "";
        }
        #endif
        #ifdef EINTR
        if (EINTR < sys_nerr) {
        errormsg_table[EINTR].name = "EINTR";
        errormsg_table[EINTR].msg =
          ENGLISH ? "Interrupted system call" :
          DEUTSCH ? "Unterbrechung während Betriebssystem-Aufruf" :
          FRANCAIS ? "Appel système interrompu" :
          "";
        }
        #endif
        #ifdef EIO
        if (EIO < sys_nerr) {
        errormsg_table[EIO].name = "EIO";
        errormsg_table[EIO].msg =
          ENGLISH ? "I/O error" :
          DEUTSCH ? "Fehler bei Schreib-/Lesezugriff" :
          FRANCAIS ? "Erreur E/S" :
          "";
        }
        #endif
        #ifdef ENXIO
        if (ENXIO < sys_nerr) {
        errormsg_table[ENXIO].name = "ENXIO";
        errormsg_table[ENXIO].msg =
          ENGLISH ? "No such device or address" :
          DEUTSCH ? "Gerät existiert nicht oder Laufwerk leer" :
          FRANCAIS ? "Périphérique ou adresse inexistant" :
          "";
        }
        #endif
        #ifdef E2BIG
        if (E2BIG < sys_nerr) {
        errormsg_table[E2BIG].name = "E2BIG";
        errormsg_table[E2BIG].msg =
          ENGLISH ? "Arg list too long" :
          DEUTSCH ? "Zu lange Argumentliste" :
          FRANCAIS ? "Liste d'arguments trop longue" :
          "";
        }
        #endif
        #ifdef ENOEXEC
        if (ENOEXEC < sys_nerr) {
        errormsg_table[ENOEXEC].name = "ENOEXEC";
        errormsg_table[ENOEXEC].msg =
          ENGLISH ? "Exec format error" :
          DEUTSCH ? "Kein ausführbares Programm" :
          FRANCAIS ? "Programme non exécutable" :
          "";
        }
        #endif
        #ifdef EBADF
        if (EBADF < sys_nerr) {
        errormsg_table[EBADF].name = "EBADF";
        errormsg_table[EBADF].msg =
          ENGLISH ? "Bad file number" :
          DEUTSCH ? "File-Descriptor wurde nicht für diese Operation geöffnet" :
          FRANCAIS ? "Descripteur de fichier non alloué" :
          "";
        }
        #endif
        #ifdef ECHILD
        if (ECHILD < sys_nerr) {
        errormsg_table[ECHILD].name = "ECHILD";
        errormsg_table[ECHILD].msg =
          ENGLISH ? "No child processes" :
          DEUTSCH ? "Worauf warten?" :
          FRANCAIS ? "Pas de processus fils" :
          "";
        }
        #endif
        #ifdef EAGAIN
        if (EAGAIN < sys_nerr) {
        errormsg_table[EAGAIN].name = "EAGAIN";
        errormsg_table[EAGAIN].msg =
          ENGLISH ? "No more processes" :
          DEUTSCH ? "Kann keinen weiteren Prozess erzeugen" :
          FRANCAIS ? "Essayez encore" :
          "";
        }
        #endif
        #ifdef ENOMEM
        if (ENOMEM < sys_nerr) {
        errormsg_table[ENOMEM].name = "ENOMEM";
        errormsg_table[ENOMEM].msg =
          ENGLISH ? "Not enough memory" :
          DEUTSCH ? "Hauptspeicher oder Swapspace reicht nicht" :
          FRANCAIS ? "Pas assez de mémoire" :
          "";
        }
        #endif
        #ifdef EACCES
        if (EACCES < sys_nerr) {
        errormsg_table[EACCES].name = "EACCES";
        errormsg_table[EACCES].msg =
          ENGLISH ? "Permission denied" :
          DEUTSCH ? "Keine Berechtigung" :
          FRANCAIS ? "Permission refusée" :
          "";
        }
        #endif
        #ifdef EFAULT
        if (EFAULT < sys_nerr) {
        errormsg_table[EFAULT].name = "EFAULT";
        errormsg_table[EFAULT].msg =
          ENGLISH ? "Bad address" :
          DEUTSCH ? "Ungültige Adresse" :
          FRANCAIS ? "Mauvaise adresse" :
          "";
        }
        #endif
        #ifdef ENOTBLK
        if (ENOTBLK < sys_nerr) {
        errormsg_table[ENOTBLK].name = "ENOTBLK";
        errormsg_table[ENOTBLK].msg =
          ENGLISH ? "Block device required" :
          DEUTSCH ? "Nur block-strukturierte Geräte erlaubt" :
          FRANCAIS ? "Périphérique bloc requis" :
          "";
        }
        #endif
        #ifdef EBUSY
        if (EBUSY < sys_nerr) {
        errormsg_table[EBUSY].name = "EBUSY";
        errormsg_table[EBUSY].msg =
          ENGLISH ? "Device busy" :
          DEUTSCH ? "Filesystem darf nicht gekappt werden" :
          FRANCAIS ? "Périphérique occupé" :
          "";
        }
        #endif
        #ifdef EEXIST
        if (EEXIST < sys_nerr) {
        errormsg_table[EEXIST].name = "EEXIST";
        errormsg_table[EEXIST].msg =
          ENGLISH ? "File exists" :
          DEUTSCH ? "File existiert schon" :
          FRANCAIS ? "Le fichier existe déjà" :
          "";
        }
        #endif
        #ifdef EXDEV
        if (EXDEV < sys_nerr) {
        errormsg_table[EXDEV].name = "EXDEV";
        errormsg_table[EXDEV].msg =
          ENGLISH ? "Cross-device link" :
          DEUTSCH ? "Links können nur aufs selbe Gerät gehen" :
          FRANCAIS ? "Lien entre périphériques différents" :
          "";
        }
        #endif
        #ifdef ENODEV
        if (ENODEV < sys_nerr) {
        errormsg_table[ENODEV].name = "ENODEV";
        errormsg_table[ENODEV].msg =
          ENGLISH ? "No such device" :
          DEUTSCH ? "Gerät nicht da oder unpassend" :
          FRANCAIS ? "Périphérique inexistant" :
          "";
        }
        #endif
        #ifdef ENOTDIR
        if (ENOTDIR < sys_nerr) {
        errormsg_table[ENOTDIR].name = "ENOTDIR";
        errormsg_table[ENOTDIR].msg =
          ENGLISH ? "Not a directory" :
          DEUTSCH ? "Das ist kein Directory" :
          FRANCAIS ? "N'est pas un répertoire" :
          "";
        }
        #endif
        #ifdef EISDIR
        if (EISDIR < sys_nerr) {
        errormsg_table[EISDIR].name = "EISDIR";
        errormsg_table[EISDIR].msg =
          ENGLISH ? "Is a directory" :
          DEUTSCH ? "Das ist ein Directory" :
          FRANCAIS ? "Est un répertoire" :
          "";
        }
        #endif
        #ifdef EINVAL
        if (EINVAL < sys_nerr) {
        errormsg_table[EINVAL].name = "EINVAL";
        errormsg_table[EINVAL].msg =
          ENGLISH ? "Invalid argument" :
          DEUTSCH ? "Ungültiger Parameter" :
          FRANCAIS ? "Paramètre illicite" :
          "";
        }
        #endif
        #ifdef ENFILE
        if (ENFILE < sys_nerr) {
        errormsg_table[ENFILE].name = "ENFILE";
        errormsg_table[ENFILE].msg =
          ENGLISH ? "File table overflow" :
          DEUTSCH ? "Tabelle der offenen Files ist voll" :
          FRANCAIS ? "Dépassement de la table des fichiers" :
          "";
        }
        #endif
        #ifdef EMFILE
        if (EMFILE < sys_nerr) {
        errormsg_table[EMFILE].name = "EMFILE";
        errormsg_table[EMFILE].msg =
          ENGLISH ? "Too many open files" :
          DEUTSCH ? "Zu viele offene Files" :
          FRANCAIS ? "Trop de fichiers ouverts" :
          "";
        }
        #endif
        #ifdef ENOTTY
        if (ENOTTY < sys_nerr) {
        errormsg_table[ENOTTY].name = "ENOTTY";
        errormsg_table[ENOTTY].msg =
          ENGLISH ? "Inappropriate ioctl for device" :
          DEUTSCH ? "Falscher Gerätetyp" :
          FRANCAIS ? "Périphérique ne comprend pas ce ioctl" :
          "";
        }
        #endif
        #ifdef ETXTBSY
        if (ETXTBSY < sys_nerr) {
        errormsg_table[ETXTBSY].name = "ETXTBSY";
        errormsg_table[ETXTBSY].msg =
          ENGLISH ? "Text file busy" :
          DEUTSCH ? "Programm wird gerade geändert oder ausgeführt" :
          FRANCAIS ? "Fichier code occupé" :
          "";
        }
        #endif
        #ifdef EFBIG
        if (EFBIG < sys_nerr) {
        errormsg_table[EFBIG].name = "EFBIG";
        errormsg_table[EFBIG].msg =
          ENGLISH ? "File too large" :
          DEUTSCH ? "Zu großes File" :
          FRANCAIS ? "Fichier trop grand" :
          "";
        }
        #endif
        #ifdef ENOSPC
        if (ENOSPC < sys_nerr) {
        errormsg_table[ENOSPC].name = "ENOSPC";
        errormsg_table[ENOSPC].msg =
          ENGLISH ? "No space left on device" :
          DEUTSCH ? "Platte oder Diskette voll" :
          FRANCAIS ? "Plus d'espace libre sur le périphérique" :
          "";
        }
        #endif
        #ifdef ESPIPE
        if (ESPIPE < sys_nerr) {
        errormsg_table[ESPIPE].name = "ESPIPE";
        errormsg_table[ESPIPE].msg =
          ENGLISH ? "Illegal seek" :
          DEUTSCH ? "Nicht positionierbares File" :
          FRANCAIS ? "seek illégal" :
          "";
        }
        #endif
        #ifdef EROFS
        if (EROFS < sys_nerr) {
        errormsg_table[EROFS].name = "EROFS";
        errormsg_table[EROFS].msg =
          ENGLISH ? "Read-only file system" :
          DEUTSCH ? "Dieses Filesystem erlaubt keinen Schreibzugriff" :
          FRANCAIS ? "Système de fichiers en lecture seulement" :
          "";
        }
        #endif
        #ifdef EMLINK
        if (EMLINK < sys_nerr) {
        errormsg_table[EMLINK].name = "EMLINK";
        errormsg_table[EMLINK].msg =
          ENGLISH ? "Too many links" :
          DEUTSCH ? "Zu viele Links auf ein File" :
          FRANCAIS ? "Trop de liens" :
          "";
        }
        #endif
        #ifdef EPIPE
        if (EPIPE < sys_nerr) {
        errormsg_table[EPIPE].name = "EPIPE";
        errormsg_table[EPIPE].msg =
          # Note that these "translations" exploit that CLISP only catches
          # SIGPIPEs from subprocesses. Other pipes lead to a deadly signal
          # and never to this error message.
          ENGLISH ? "Broken pipe, child process terminated" :
          DEUTSCH ? "Rohrbruch, Tochterprozess beendet" :
          FRANCAIS ? "Rupture du tuyau, processus fils terminé" :
          "";
        }
        #endif
        /* Errors bei mathematischen Funktionen: */
        #ifdef EDOM
        if (EDOM < sys_nerr) {
        errormsg_table[EDOM].name = "EDOM";
        errormsg_table[EDOM].msg =
          ENGLISH ? "Argument out of domain" :
          DEUTSCH ? "Argument zu mathematischer Funktion außerhalb des Definitionsbereichs" :
          FRANCAIS ? "Argument mathématique en dehors du domaine de définition de la fonction" :
          "";
        }
        #endif
        #ifdef ERANGE
        if (ERANGE < sys_nerr) {
        errormsg_table[ERANGE].name = "ERANGE";
        errormsg_table[ERANGE].msg =
          ENGLISH ? "Result too large" :
          DEUTSCH ? "Ergebnis mathematischer Funktion zu groß" :
          FRANCAIS ? "Résultat mathématique non représentable" :
          "";
        }
        #endif
        /* Errors bei Non-Blocking I/O und Interrupt I/O: */
        #ifdef EWOULDBLOCK
        if (EWOULDBLOCK < sys_nerr) {
        errormsg_table[EWOULDBLOCK].name = "EWOULDBLOCK";
        errormsg_table[EWOULDBLOCK].msg =
          ENGLISH ? "Operation would block" :
          DEUTSCH ? "Darauf müsste gewartet werden" :
          FRANCAIS ? "L'opération devrait bloquer" :
          "";
        }
        #endif
        #ifdef EINPROGRESS
        if (EINPROGRESS < sys_nerr) {
        errormsg_table[EINPROGRESS].name = "EINPROGRESS";
        errormsg_table[EINPROGRESS].msg =
          ENGLISH ? "Operation now in progress" :
          DEUTSCH ? "Das kann lange dauern" :
          FRANCAIS ? "Opération maintenant en cours" :
          "";
        }
        #endif
        #ifdef EALREADY
        if (EALREADY < sys_nerr) {
        errormsg_table[EALREADY].name = "EALREADY";
        errormsg_table[EALREADY].msg =
          ENGLISH ? "Operation already in progress" :
          DEUTSCH ? "Es läuft schon eine Operation" :
          FRANCAIS ? "Opération déjà en cours" :
          "";
        }
        #endif
        /* weitere allgemein übliche Errors: */
        #ifdef ELOOP
        if (ELOOP < sys_nerr) {
        errormsg_table[ELOOP].name = "ELOOP";
        errormsg_table[ELOOP].msg =
          ENGLISH ? "Too many levels of symbolic links" :
          DEUTSCH ? "Zu viele symbolische Links in einem Pathname" :
          FRANCAIS ? "Trop de liens symboliques rencontrés" :
          "";
        }
        #endif
        #ifdef ENAMETOOLONG
        if (ENAMETOOLONG < sys_nerr) {
        errormsg_table[ENAMETOOLONG].name = "ENAMETOOLONG";
        errormsg_table[ENAMETOOLONG].msg =
          ENGLISH ? "File name too long" :
          DEUTSCH ? "Zu langer Filename" :
          FRANCAIS ? "Nom du fichier trop long" :
          "";
        }
        #endif
        #ifdef ENOTEMPTY
        if (ENOTEMPTY < sys_nerr) {
        errormsg_table[ENOTEMPTY].name = "ENOTEMPTY";
        errormsg_table[ENOTEMPTY].msg =
          ENGLISH ? "Directory not empty" :
          DEUTSCH ? "Directory ist nicht leer" :
          FRANCAIS ? "Répertoire non vide" :
          "";
        }
        #endif
        /* Errors im Zusammenhang mit Network File System (NFS): */
        #ifdef ESTALE
        if (ESTALE < sys_nerr) {
        errormsg_table[ESTALE].name = "ESTALE";
        errormsg_table[ESTALE].msg =
          ENGLISH ? "Stale NFS file handle" :
          DEUTSCH ? "Offenes File auf entferntem Filesystem wurde gelöscht" :
          FRANCAIS ? "Fichier NFS perdu" :
          "";
        }
        #endif
        #ifdef EREMOTE
        if (EREMOTE < sys_nerr) {
        errormsg_table[EREMOTE].name = "EREMOTE";
        errormsg_table[EREMOTE].msg =
          ENGLISH ? "Too many levels of remote in path" :
          DEUTSCH ? "Mount läuft nicht auf entfernten Filesystemen" :
          FRANCAIS ? "Mount éloigné ne marche pas" :
          "";
        }
        #endif
        /* Errors im Zusammenhang mit Sockets, IPC und Netzwerk: */
        #ifdef ENOTSOCK
        if (ENOTSOCK < sys_nerr) {
        errormsg_table[ENOTSOCK].name = "ENOTSOCK";
        errormsg_table[ENOTSOCK].msg =
          ENGLISH ? "Socket operation on non-socket" :
          DEUTSCH ? "Socket-Operation und kein Socket" :
          FRANCAIS ? "Opération de type socket sur un fichier non-socket" :
          "";
        }
        #endif
        #ifdef EDESTADDRREQ
        if (EDESTADDRREQ < sys_nerr) {
        errormsg_table[EDESTADDRREQ].name = "EDESTADDRREQ";
        errormsg_table[EDESTADDRREQ].msg =
          ENGLISH ? "Destination address required" :
          DEUTSCH ? "Operation braucht Zieladresse" :
          FRANCAIS ? "Adresse de destination obligatoire" :
          "";
        }
        #endif
        #ifdef EMSGSIZE
        if (EMSGSIZE < sys_nerr) {
        errormsg_table[EMSGSIZE].name = "EMSGSIZE";
        errormsg_table[EMSGSIZE].msg =
          ENGLISH ? "Message too long" :
          DEUTSCH ? "Zu lange Nachricht" :
          FRANCAIS ? "Message trop long" :
          "";
        }
        #endif
        #ifdef EPROTOTYPE
        if (EPROTOTYPE < sys_nerr) {
        errormsg_table[EPROTOTYPE].name = "EPROTOTYPE";
        errormsg_table[EPROTOTYPE].msg =
          ENGLISH ? "Protocol wrong type for socket" :
          DEUTSCH ? "Dieses Protokoll passt nicht zu diesem Socket" :
          FRANCAIS ? "Mauvais type de protocole pour un socket" :
          "";
        }
        #endif
        #ifdef ENOPROTOOPT
        if (ENOPROTOOPT < sys_nerr) {
        errormsg_table[ENOPROTOOPT].name = "ENOPROTOOPT";
        errormsg_table[ENOPROTOOPT].msg =
          ENGLISH ? "Option not supported by protocol" :
          DEUTSCH ? "Fehlerhafte Option zu Protokoll auf Socket" :
          FRANCAIS ? "Protocole non disponible" :
          "";
        }
        #endif
        #ifdef EPROTONOSUPPORT
        if (EPROTONOSUPPORT < sys_nerr) {
        errormsg_table[EPROTONOSUPPORT].name = "EPROTONOSUPPORT";
        errormsg_table[EPROTONOSUPPORT].msg =
          ENGLISH ? "Protocol not supported" :
          DEUTSCH ? "Protokoll nicht implementiert" :
          FRANCAIS ? "Protocole non supporté" :
          "";
        }
        #endif
        #ifdef ESOCKTNOSUPPORT
        if (ESOCKTNOSUPPORT < sys_nerr) {
        errormsg_table[ESOCKTNOSUPPORT].name = "ESOCKTNOSUPPORT";
        errormsg_table[ESOCKTNOSUPPORT].msg =
          ENGLISH ? "Socket type not supported" :
          DEUTSCH ? "Socket-Typ nicht implementiert" :
          FRANCAIS ? "Type de socket non supporté" :
          "";
        }
        #endif
        #ifdef EOPNOTSUPP
        if (EOPNOTSUPP < sys_nerr) {
        errormsg_table[EOPNOTSUPP].name = "EOPNOTSUPP";
        errormsg_table[EOPNOTSUPP].msg =
          ENGLISH ? "Operation not supported on socket" :
          DEUTSCH ? "Operation auf diesem Socket nicht implementiert" :
          FRANCAIS ? "Opération non supportée sur socket" :
          "";
        }
        #endif
        #ifdef EPFNOSUPPORT
        if (EPFNOSUPPORT < sys_nerr) {
        errormsg_table[EPFNOSUPPORT].name = "EPFNOSUPPORT";
        errormsg_table[EPFNOSUPPORT].msg =
          ENGLISH ? "Protocol family not supported" :
          DEUTSCH ? "Protokoll-Familie nicht implementiert" :
          FRANCAIS ? "Famille de protocoles non supportée" :
          "";
        }
        #endif
        #ifdef EAFNOSUPPORT
        if (EAFNOSUPPORT < sys_nerr) {
        errormsg_table[EAFNOSUPPORT].name = "EAFNOSUPPORT";
        errormsg_table[EAFNOSUPPORT].msg =
          ENGLISH ? "Address family not supported by protocol family" :
          DEUTSCH ? "Adressen-Familie passt nicht zu diesem Protokoll" :
          FRANCAIS ? "Famille d'adresses non supportée par le protocole" :
          "";
        }
        #endif
        #ifdef EADDRINUSE
        if (EADDRINUSE < sys_nerr) {
        errormsg_table[EADDRINUSE].name = "EADDRINUSE";
        errormsg_table[EADDRINUSE].msg =
          ENGLISH ? "Address already in use" :
          DEUTSCH ? "Adresse schon belegt" :
          FRANCAIS ? "Adresse déjà utilisée" :
          "";
        }
        #endif
        #ifdef EADDRNOTAVAIL
        if (EADDRNOTAVAIL < sys_nerr) {
        errormsg_table[EADDRNOTAVAIL].name = "EADDRNOTAVAIL";
        errormsg_table[EADDRNOTAVAIL].msg =
          ENGLISH ? "Can't assign requested address" :
          DEUTSCH ? "Adresse nicht (auf diesem Rechner) verfügbar" :
          FRANCAIS ? "Ne peut pas assigner l'adresse demandée" :
          "";
        }
        #endif
        #ifdef ENETDOWN
        if (ENETDOWN < sys_nerr) {
        errormsg_table[ENETDOWN].name = "ENETDOWN";
        errormsg_table[ENETDOWN].msg =
          ENGLISH ? "Network is down" :
          DEUTSCH ? "Netz streikt" :
          FRANCAIS ? "Le réseau est éteint" :
          "";
        }
        #endif
        #ifdef ENETUNREACH
        if (ENETUNREACH < sys_nerr) {
        errormsg_table[ENETUNREACH].name = "ENETUNREACH";
        errormsg_table[ENETUNREACH].msg =
          ENGLISH ? "Network is unreachable" :
          DEUTSCH ? "Netz unbekannt und außer Sichtweite" :
          FRANCAIS ? "Le réseau ne peut être atteint" :
          "";
        }
        #endif
        #ifdef ENETRESET
        if (ENETRESET < sys_nerr) {
        errormsg_table[ENETRESET].name = "ENETRESET";
        errormsg_table[ENETRESET].msg =
          ENGLISH ? "Network dropped connection on reset" :
          DEUTSCH ? "Rechner bootete, Verbindung gekappt" :
          FRANCAIS ? "Le réseau a rompu la connection à cause d'une remise à zéro" :
          "";
        }
        #endif
        #ifdef ECONNABORTED
        if (ECONNABORTED < sys_nerr) {
        errormsg_table[ECONNABORTED].name = "ECONNABORTED";
        errormsg_table[ECONNABORTED].msg =
          ENGLISH ? "Software caused connection abort" :
          DEUTSCH ? "Musste diese Verbindung kappen" :
          FRANCAIS ? "Echec de connection à cause du logiciel" :
          "";
        }
        #endif
        #ifdef ECONNRESET
        if (ECONNRESET < sys_nerr) {
        errormsg_table[ECONNRESET].name = "ECONNRESET";
        errormsg_table[ECONNRESET].msg =
          ENGLISH ? "Connection reset by peer" :
          DEUTSCH ? "Gegenseite kappte die Verbindung" :
          FRANCAIS ? "Connection remise à zéro par le correspondant" :
          "";
        }
        #endif
        #ifdef ENOBUFS
        if (ENOBUFS < sys_nerr) {
        errormsg_table[ENOBUFS].name = "ENOBUFS";
        errormsg_table[ENOBUFS].msg =
          ENGLISH ? "No buffer space available" :
          DEUTSCH ? "Nicht genügend Platz für einen Buffer" :
          FRANCAIS ? "Pas d'espace disponible pour un buffer" :
          "";
        }
        #endif
        #ifdef EISCONN
        if (EISCONN < sys_nerr) {
        errormsg_table[EISCONN].name = "EISCONN";
        errormsg_table[EISCONN].msg =
          ENGLISH ? "Socket is already connected" :
          DEUTSCH ? "Socket ist bereits verbunden" :
          FRANCAIS ? "Le socket est déjà connecté" :
          "";
        }
        #endif
        #ifdef ENOTCONN
        if (ENOTCONN < sys_nerr) {
        errormsg_table[ENOTCONN].name = "ENOTCONN";
        errormsg_table[ENOTCONN].msg =
          ENGLISH ? "Socket is not connected" :
          DEUTSCH ? "Socket hat keine Verbindung" :
          FRANCAIS ? "Le socket n'est pas connecté" :
          "";
        }
        #endif
        #ifdef ESHUTDOWN
        if (ESHUTDOWN < sys_nerr) {
        errormsg_table[ESHUTDOWN].name = "ESHUTDOWN";
        errormsg_table[ESHUTDOWN].msg =
          ENGLISH ? "Can't send after socket shutdown" :
          DEUTSCH ? "Shutdown hat den Socket schon deaktiviert" :
          FRANCAIS ? "Impossibilité d'envoyer après un arrêt de socket" :
          "";
        }
        #endif
        #ifdef ETOOMANYREFS
        if (ETOOMANYREFS < sys_nerr) {
        errormsg_table[ETOOMANYREFS].name = "ETOOMANYREFS";
        errormsg_table[ETOOMANYREFS].msg =
          /*ENGLISH*/ TRUE ? "Too many references: can't splice" :
          "";
        }
        #endif
        #ifdef ETIMEDOUT
        if (ETIMEDOUT < sys_nerr) {
        errormsg_table[ETIMEDOUT].name = "ETIMEDOUT";
        errormsg_table[ETIMEDOUT].msg =
          ENGLISH ? "Connection timed out" :
          DEUTSCH ? "Verbindung nach Timeout gekappt" :
          FRANCAIS ? "Durée écoulée pour la connection" :
          "";
        }
        #endif
        #ifdef ECONNREFUSED
        if (ECONNREFUSED < sys_nerr) {
        errormsg_table[ECONNREFUSED].name = "ECONNREFUSED";
        errormsg_table[ECONNREFUSED].msg =
          ENGLISH ? "Connection refused" :
          DEUTSCH ? "Gegenseite verweigert die Verbindung" :
          FRANCAIS ? "Connection refusée" :
          "";
        }
        #endif
        #if 0
        errormsg_table[].name = "";
        errormsg_table[].msg =
          /*ENGLISH*/ TRUE ? "Remote peer released connection" :
          "";
        #endif
        #ifdef EHOSTDOWN
        if (EHOSTDOWN < sys_nerr) {
        errormsg_table[EHOSTDOWN].name = "EHOSTDOWN";
        errormsg_table[EHOSTDOWN].msg =
          ENGLISH ? "Host is down" :
          DEUTSCH ? "Gegenseite ist wohl abgeschaltet" :
          FRANCAIS ? "L'hôte est éteint" :
          "";
        }
        #endif
        #ifdef EHOSTUNREACH
        if (EHOSTUNREACH < sys_nerr) {
        errormsg_table[EHOSTUNREACH].name = "EHOSTUNREACH";
        errormsg_table[EHOSTUNREACH].msg =
          ENGLISH ? "Host is unreachable" :
          DEUTSCH ? "Gegenseite nicht in Sichtweite, nicht erreichbar" :
          FRANCAIS ? "Aucune route pour cet hôte" :
          "";
        }
        #endif
        #if 0
        errormsg_table[].name = "";
        errormsg_table[].msg =
          /*ENGLISH*/ TRUE ? "Networking error" :
          "";
        #endif
        /* Quotas: */
        #ifdef EPROCLIM
        if (EPROCLIM < sys_nerr) {
        errormsg_table[EPROCLIM].name = "EPROCLIM";
        errormsg_table[EPROCLIM].msg =
          ENGLISH ? "Too many processes" :
          DEUTSCH ? "Zu viele Prozesse am Laufen" :
          FRANCAIS ? "Trop de processus" :
          "";
        }
        #endif
        #ifdef EUSERS
        if (EUSERS < sys_nerr) {
        errormsg_table[EUSERS].name = "EUSERS";
        errormsg_table[EUSERS].msg =
          ENGLISH ? "Too many users" :
          DEUTSCH ? "Zu viele Benutzer aktiv" :
          FRANCAIS ? "Trop d'utilisateurs" :
          "";
        }
        #endif
        #ifdef EDQUOT
        if (EDQUOT < sys_nerr) {
        errormsg_table[EDQUOT].name = "EDQUOT";
        errormsg_table[EDQUOT].msg =
          ENGLISH ? "Disk quota exceeded" :
          DEUTSCH ? "Plattenplatz rationiert, Ihr Anteil ist erschöpft" :
          FRANCAIS ? "Ration d'espace est épuisée" :
          "";
        }
        #endif
        /* Errors im Zusammenhang mit STREAMS: */
        #ifdef ENOSTR
        if (ENOSTR < sys_nerr) {
        errormsg_table[ENOSTR].name = "ENOSTR";
        errormsg_table[ENOSTR].msg =
          ENGLISH ? "Not a stream device" :
          DEUTSCH ? "Das ist kein STREAM" :
          FRANCAIS ? "Pas un STREAM" :
          "";
        }
        #endif
        #ifdef ETIME
        if (ETIME < sys_nerr) {
        errormsg_table[ETIME].name = "ETIME";
        errormsg_table[ETIME].msg =
          ENGLISH ? "Timer expired" :
          DEUTSCH ? "STREAM braucht länger als erwartet" :
          FRANCAIS ? "Timer de STREAM écoulé" :
          "";
        }
        #endif
        #ifdef ENOSR
        if (ENOSR < sys_nerr) {
        errormsg_table[ENOSR].name = "ENOSR";
        errormsg_table[ENOSR].msg =
          ENGLISH ? "Out of stream resources" :
          DEUTSCH ? "Kein Platz für weiteren STREAM" :
          FRANCAIS ? "Plus de place pour un STREAM" :
          "";
        }
        #endif
        #ifdef ENOMSG
        if (ENOMSG < sys_nerr) {
        errormsg_table[ENOMSG].name = "ENOMSG";
        errormsg_table[ENOMSG].msg =
          ENGLISH ? "No message of desired type" :
          DEUTSCH ? "Nachrichten dieses Typs gibt es hier nicht" :
          FRANCAIS ? "Pas de messages du type désigné" :
          "";
        }
        #endif
        #ifdef EBADMSG
        if (EBADMSG < sys_nerr) {
        errormsg_table[EBADMSG].name = "EBADMSG";
        errormsg_table[EBADMSG].msg =
          ENGLISH ? "Not a data message" :
          DEUTSCH ? "Nachricht von unbekanntem Typ angekommen" :
          FRANCAIS ? "Reçu message de type inconnu" :
          "";
        }
        #endif
        /* Errors bei SystemV IPC: */
        #ifdef EIDRM
        if (EIDRM < sys_nerr) {
        errormsg_table[EIDRM].name = "EIDRM";
        errormsg_table[EIDRM].msg =
          ENGLISH ? "Identifier removed" :
          DEUTSCH ? "Name (einer Semaphore) wurde gelöscht" :
          FRANCAIS ? "Identificateur supprimé" :
          "";
        }
        #endif
        /* Errors bei SystemV Record-Locking: */
        #ifdef EDEADLK
        if (EDEADLK < sys_nerr) {
        errormsg_table[EDEADLK].name = "EDEADLK";
        errormsg_table[EDEADLK].msg =
          ENGLISH ? "Resource deadlock would occur" :
          DEUTSCH ? "Das würde zu einem Deadlock führen" :
          FRANCAIS ? "Blocage mutuel de la ressource " :
          "";
        }
        #endif
        #ifdef ENOLCK
        if (ENOLCK < sys_nerr) {
        errormsg_table[ENOLCK].name = "ENOLCK";
        errormsg_table[ENOLCK].msg =
          ENGLISH ? "No record locks available" :
          DEUTSCH ? "Zu viele Zugriffsvorbehalte auf einmal" :
          FRANCAIS ? "Pas de verrou disponible" :
          "";
        }
        #endif
        /* Errors bei Remote File System (RFS): */
        #ifdef ENONET
        if (ENONET < sys_nerr) {
        errormsg_table[ENONET].name = "ENONET";
        errormsg_table[ENONET].msg =
          ENGLISH ? "Machine is not on the network" :
          DEUTSCH ? "Rechner nicht übers Netz erreichbar" :
          FRANCAIS ? "La machine n'est pas sur le réseau" :
          "";
        }
        #endif
        #ifdef EREMOTE
        if (EREMOTE < sys_nerr) {
        errormsg_table[EREMOTE].name = "EREMOTE";
        errormsg_table[EREMOTE].msg =
          ENGLISH ? "Object is remote" :
          DEUTSCH ? "Das kann nur der dortige Rechner" :
          FRANCAIS ? "Objet à distance" :
          "";
        }
        #endif
        #ifdef ERREMOTE
        if (ERREMOTE < sys_nerr) {
        errormsg_table[ERREMOTE].name = "ERREMOTE";
        errormsg_table[ERREMOTE].msg =
          ENGLISH ? "Object is remote" :
          DEUTSCH ? "Das kann nur der dortige Rechner" :
          FRANCAIS ? "Objet à distance" :
          "";
        }
        #endif
        #ifdef ENOLINK
        if (ENOLINK < sys_nerr) {
        errormsg_table[ENOLINK].name = "ENOLINK";
        errormsg_table[ENOLINK].msg =
          ENGLISH ? "Link has been severed" :
          DEUTSCH ? "Verbindung ist zusammengebrochen" :
          FRANCAIS ? "Le lien a été coupé" :
          "";
        }
        #endif
        #ifdef EADV
        if (EADV < sys_nerr) {
        errormsg_table[EADV].name = "EADV";
        errormsg_table[EADV].msg =
          ENGLISH ? "Advertise error" :
          DEUTSCH ? "Andere Rechner benutzen noch unsere Ressourcen" :
          FRANCAIS ? "Erreur d'annonce" :
          "";
        }
        #endif
        #ifdef ESRMNT
        if (ESRMNT < sys_nerr) {
        errormsg_table[ESRMNT].name = "ESRMNT";
        errormsg_table[ESRMNT].msg =
          ENGLISH ? "Srmount error" :
          DEUTSCH ? "Andere Rechner benutzen noch unsere Ressourcen" :
          FRANCAIS ? "Erreur srmount" :
          "";
        }
        #endif
        #ifdef ECOMM
        if (ECOMM < sys_nerr) {
        errormsg_table[ECOMM].name = "ECOMM";
        errormsg_table[ECOMM].msg =
          ENGLISH ? "Communication error on send" :
          DEUTSCH ? "Beim Senden: Rechner nicht erreichbar" :
          FRANCAIS ? "Erreur de communication lors d'un envoi" :
          "";
        }
        #endif
        #ifdef EPROTO
        if (EPROTO < sys_nerr) {
        errormsg_table[EPROTO].name = "EPROTO";
        errormsg_table[EPROTO].msg =
          ENGLISH ? "Protocol error" :
          DEUTSCH ? "Protokoll klappt nicht" :
          FRANCAIS ? "Erreur de protocole" :
          "";
        }
        #endif
        #ifdef EMULTIHOP
        if (EMULTIHOP < sys_nerr) {
        errormsg_table[EMULTIHOP].name = "EMULTIHOP";
        errormsg_table[EMULTIHOP].msg =
          ENGLISH ? "Multihop attempted" :
          DEUTSCH ? "Ressourcen nicht direkt erreichbar" :
          FRANCAIS ? "Tentative de sauts multiples" :
          "";
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
        errormsg_table[EREMCHG].msg =
          ENGLISH ? "Remote address changed" :
          DEUTSCH ? "Rechner hat jetzt eine andere Adresse" :
          FRANCAIS ? "Adresse à distance changée" :
          "";
        }
        #endif
        /* Errors von POSIX: */
        #ifdef ENOSYS
        if (ENOSYS < sys_nerr) {
        errormsg_table[ENOSYS].name = "ENOSYS";
        errormsg_table[ENOSYS].msg =
          ENGLISH ? "Function not implemented" :
          DEUTSCH ? "Funktion ist nicht implementiert" :
          FRANCAIS ? "Fonction non implémentée" :
          "";
        }
        #endif
        /* Sonstige: */
        #ifdef EMSDOS /* emx 0.8e - 0.8h */
        if (EMSDOS < sys_nerr) {
        errormsg_table[EMSDOS].name = "EMSDOS";
        errormsg_table[EMSDOS].msg =
          ENGLISH ? "Not supported under MS-DOS" :
          DEUTSCH ? "Das geht unter MS-DOS nicht" :
          FRANCAIS ? "Pas supporté sous MS-DOS" :
          "";
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
        write_errorstring(DEUTSCH ? "UNIX-Fehler " :
                          ENGLISH ? "UNIX error " :
                          FRANCAIS ? "Erreur UNIX " :
                          ""
                         );
        #else
        write_errorstring(DEUTSCH ? "UNIX-Bibliotheks-Fehler " :
                          ENGLISH ? "UNIX library error " :
                          FRANCAIS ? "Erreur dans la librairie UNIX " :
                          ""
                         );
        #endif
        # Fehlernummer ausgeben:
        write_errorobject(fixnum(errcode));
        #if 0
        { # Fehlermeldung des Betriebssystems ausgeben:
          if (errcode < sys_nerr)
            { var const char* errormsg = translate(sys_errlist[errcode]);
              write_errorstring(": ");
              write_errorstring(errormsg);
        }   }
        #else # nach Möglichkeit noch ausführlicher:
        { # eigene Fehlermeldung ausgeben:
          if (errcode < sys_nerr)
            # Zu dieser Fehlernummer ist ein Text da.
            { var const char* errorname = errormsg_table[errcode].name;
              var const char* errormsg = translate(errormsg_table[errcode].msg);
              if (!(errorname[0] == 0)) # bekannter Name?
                { write_errorstring(" (");
                  write_errorstring(errorname);
                  write_errorstring(")");
                }
              if (!(errormsg[0] == 0)) # nichtleere Meldung?
                { write_errorstring(": ");
                  write_errorstring(errormsg);
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

