  # Behandlung von DJUNIX-(DOS-)Fehlern
  # OS_error();
  # > int errno: Fehlercode
    nonreturning_function(global, OS_error, (void));
    nonreturning_function(global, OS_file_error, (object pathname));
    local void OS_error_internal (uintC errcode);
    local void OS_error_internal(errcode)
      var uintC errcode;
      { # Meldungbeginn ausgeben:
        write_errorstring(DEUTSCH ? "DJDOS-Fehler " :
                          ENGLISH ? "DJDOS error " :
                          FRANCAIS ? "Erreur DJDOS " :
                          ""
                         );
        # Fehlernummer ausgeben:
        write_errorobject(fixnum(errcode));
        # nach Möglichkeit noch ausführlicher:
        if (errcode < 36)
          {# Zu Fehlernummern <36 ist ein Text da.
           #ifdef LANGUAGE_STATIC
             #define lang3(english,deutsch,francais)  ENGLISH ? english : DEUTSCH ? deutsch : FRANCAIS ? francais : ""
             #define lang1(string)  string
             #define langcount  1
             #define language  0
             #define translate(string)  string
           #else
             #ifndef GNU_GETTEXT
               #define lang3(english,deutsch,francais)  english, deutsch, francais
               #define lang1(string)  string, string, string
               #define langcount  3
               #define translate(string)  string
             #else # GNU_GETTEXT
               #define lang3(english,deutsch,francais)  english
               #define lang1(string)  string
               #define langcount  1
               #define language  0
               #define translate(string)  clgettext(string)
             #endif
           #endif
           local const char* errormsg_table[36*(1+langcount)] = {
             /*  0 */ "", lang1(""),
             /*  1 */ "ENOSYS",
                      lang3( /* ENGLISH */ "Function not implemented" ,
                             /* DEUTSCH */ "Funktion ist nicht implementiert" ,
                             /* FRANCAIS */ "Fonction non implémentée"),
             /*  2 */ "ENOENT",
                      lang3( /* ENGLISH */ "No such file or directory" ,
                             /* DEUTSCH */ "File oder Directory existiert nicht" ,
                             /* FRANCAIS */ "Fichier ou répertoire non existant"),
             /*  3 */ "ENOTDIR",
                      lang3( /* ENGLISH */ "Not a directory" ,
                             /* DEUTSCH */ "Das ist kein Directory" ,
                             /* FRANCAIS */ "N'est pas un répertoire"),
             /*  4 */ "EMFILE",
                      lang3( /* ENGLISH */ "Too many open files" ,
                             /* DEUTSCH */ "Zu viele offene Files" ,
                             /* FRANCAIS */ "Trop de fichiers ouverts"),
             /*  5 */ "EACCES",
                      lang3( /* ENGLISH */ "Permission denied" ,
                             /* DEUTSCH */ "Keine Berechtigung" ,
                             /* FRANCAIS */ "Permission refusée"),
             /*  6 */ "EBADF",
                      lang3( /* ENGLISH */ "Bad file number" ,
                             /* DEUTSCH */ "File-Descriptor wurde nicht für diese Operation geöffnet" ,
                             /* FRANCAIS */ "Descripteur de fichier non alloué"),
             /*  7 */ "EARENA",
                      lang3( /* ENGLISH */ "Memory control blocks destroyed" ,
                             /* DEUTSCH */ "Speicherverwaltung ist durcheinander" ,
                             /* FRANCAIS */ "gestionnaire de mémoire perdu"),
             /*  8 */ "ENOMEM",
                      lang3( /* ENGLISH */ "Not enough memory" ,
                             /* DEUTSCH */ "Hauptspeicher oder Swapspace reicht nicht" ,
                             /* FRANCAIS */ "Pas assez de mémoire"),
             /*  9 */ "ESEGV",
                      lang3( /* ENGLISH */ "Invalid memory address" ,
                             /* DEUTSCH */ "Ungültige Speicher-Adresse" ,
                             /* FRANCAIS */ "adresse mémoire illicite"),
             /* 10 */ "EBADENV",
                      lang3( /* ENGLISH */ "Invalid environment" ,
                             /* DEUTSCH */ "Ungültiges Environment" ,
                             /* FRANCAIS */ "environnement incorrect"),
             /* 11 */ "", lang1(""),
             /* 12 */ "EACCODE",
                      lang3( /* ENGLISH */ "Invalid access code" ,
                             /* DEUTSCH */ "Ungültiger Zugriffsmodus" ,
                             /* FRANCAIS */ "mode d'accès illégal"),
             /* 13...14 */ "", lang1(""), "", lang1(""),
             /* 15 */ "ENODEV",
                      lang3( /* ENGLISH */ "No such device" ,
                             /* DEUTSCH */ "Gerät nicht da oder unpassend" ,
                             /* FRANCAIS */ "Périphérique inexistant"),
             /* 16 */ "ECURDIR",
                      lang3( /* ENGLISH */ "Attempt to remove the current directory" ,
                             /* DEUTSCH */ "Das aktuelle Verzeichnis kann nicht entfernt werden" ,
                             /* FRANCAIS */ "Le répertoire courant ne peut pas être effacé"),
             /* 17 */ "ENOTSAME",
                      lang3( /* ENGLISH */ "Can't move to other than the same device" ,
                             /* DEUTSCH */ "Verschieben geht nicht über Laufwerksgrenzen hinweg" ,
                             /* FRANCAIS */ "ne peux pas déplacer au-delà de l'unité"),
             /* 18 */ "ENOMORE",
                      lang3( /* ENGLISH */ "No more files" ,
                             /* DEUTSCH */ "Keine weiteren Dateien" ,
                             /* FRANCAIS */ "Pas plus de fichiers"),
             /* 19 */ "EINVAL",
                      lang3( /* ENGLISH */ "Invalid argument" ,
                             /* DEUTSCH */ "Ungültiger Parameter" ,
                             /* FRANCAIS */ "Paramètre illicite"),
             /* 20 */ "E2BIG",
                      lang3( /* ENGLISH */ "Arg list too long" ,
                             /* DEUTSCH */ "Zu lange Argumentliste" ,
                             /* FRANCAIS */ "Liste d'arguments trop longue"),
             /* 21 */ "ENOEXEC",
                      lang3( /* ENGLISH */ "Exec format error" ,
                             /* DEUTSCH */ "Kein ausführbares Programm" ,
                             /* FRANCAIS */ "Programme non exécutable"),
             /* 22 */ "EXDEV",
                      lang3( /* ENGLISH */ "Cross-device link" ,
                             /* DEUTSCH */ "Links können nur aufs selbe Gerät gehen" ,
                             /* FRANCAIS */ "Lien entre périphériques différents"),
             /* 23...27 */ "", lang1(""), "", lang1(""), "", lang1(""), "", lang1(""), "", lang1(""),
             /* 28...32 */ "", lang1(""), "", lang1(""), "", lang1(""), "", lang1(""), "", lang1(""),
             /* 33 */ "EDOM",
                      lang3( /* ENGLISH */ "Argument out of domain" ,
                             /* DEUTSCH */ "Argument zu mathematischer Funktion außerhalb des Definitionsbereichs" ,
                             /* FRANCAIS */ "Argument mathématique en dehors du domaine de définition de la fonction"),
             /* 34 */ "ERANGE",
                      lang3( /* ENGLISH */ "Result too large" ,
                             /* DEUTSCH */ "Ergebnis mathematischer Funktion zu groß" ,
                             /* FRANCAIS */ "Résultat mathématique non représentable"),
             /* 35 */ "EEXIST",
                      lang3( /* ENGLISH */ "File exists" ,
                             /* DEUTSCH */ "File existiert schon" ,
                             /* FRANCAIS */ "Le fichier existe déjà"),
             };
           var const char* errorname = errormsg_table[errcode*(1+langcount)];
           var const char* errormsg = translate(errormsg_table[errcode*(1+langcount)+1+language]);
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
    global void OS_error()
      { var uintC errcode; # positive Fehlernummer
        end_system_call(); # just in case
        begin_system_call();
        errcode = errno;
        end_system_call();
        clr_break_sem_4(); # keine DOS-Operation mehr aktiv
        begin_error(); # Fehlermeldung anfangen
        OS_error_internal(errcode);
        end_error(args_end_pointer STACKop 7); # Fehlermeldung beenden
      }
    global void OS_file_error(pathname)
      var object pathname;
      { var uintC errcode; # positive Fehlernummer
        begin_system_call();
        errcode = errno;
        end_system_call();
        clr_break_sem_4(); # keine DOS-Operation mehr aktiv
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
      { asciz_out(" errno = "); dez_out(errorcode); asciz_out("." NLstring); }

