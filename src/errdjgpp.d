  # Behandlung von DJUNIX-(DOS-)Fehlern
  # OS_error();
  # > int errno: Fehlercode
    nonreturning_function(global, OS_error, (void));
    nonreturning_function(global, OS_file_error, (object pathname));
    local void OS_error_internal (uintC errcode);
    local void OS_error_internal(errcode)
      var uintC errcode;
      { # Meldungbeginn ausgeben:
        write_errorstring(GETTEXT("DJDOS error "));
        # Fehlernummer ausgeben:
        write_errorobject(fixnum(errcode));
        # nach Möglichkeit noch ausführlicher:
        if (errcode < 36)
          {# Zu Fehlernummern <36 ist ein Text da.
           #ifdef LANGUAGE_STATIC
             #define lang3(english)  english
             #define lang1(string)  string
             #define langcount  1
             #define language  0
             #define translate(string)  string
           #else
             #ifndef GNU_GETTEXT
               #define lang3(english)  english
               #define lang1(string)  string
               #define langcount  1
               #define translate(string)  string
             #else # GNU_GETTEXT
               #define lang3(english)  english
               #define lang1(string)  string
               #define langcount  1
               #define language  0
               #define translate(string)  clgettext(string)
             #endif
           #endif
           local const char* errormsg_table[36*(1+langcount)] = {
             /*  0 */ "", lang1(""),
             /*  1 */ "ENOSYS",
                      lang3( /* ENGLISH */ "Function not implemented"),
             /*  2 */ "ENOENT",
                      lang3( /* ENGLISH */ "No such file or directory"),
             /*  3 */ "ENOTDIR",
                      lang3( /* ENGLISH */ "Not a directory"),
             /*  4 */ "EMFILE",
                      lang3( /* ENGLISH */ "Too many open files"),
             /*  5 */ "EACCES",
                      lang3( /* ENGLISH */ "Permission denied"),
             /*  6 */ "EBADF",
                      lang3( /* ENGLISH */ "Bad file number"),
             /*  7 */ "EARENA",
                      lang3( /* ENGLISH */ "Memory control blocks destroyed"),
             /*  8 */ "ENOMEM",
                      lang3( /* ENGLISH */ "Not enough memory"),
             /*  9 */ "ESEGV",
                      lang3( /* ENGLISH */ "Invalid memory address"),
             /* 10 */ "EBADENV",
                      lang3( /* ENGLISH */ "Invalid environment"),
             /* 11 */ "", lang1(""),
             /* 12 */ "EACCODE",
                      lang3( /* ENGLISH */ "Invalid access code"),
             /* 13...14 */ "", lang1(""), "", lang1(""),
             /* 15 */ "ENODEV",
                      lang3( /* ENGLISH */ "No such device"),
             /* 16 */ "ECURDIR",
                      lang3( /* ENGLISH */ "Attempt to remove the current directory"),
             /* 17 */ "ENOTSAME",
                      lang3( /* ENGLISH */ "Can't move to other than the same device"),
             /* 18 */ "ENOMORE",
                      lang3( /* ENGLISH */ "No more files"),
             /* 19 */ "EINVAL",
                      lang3( /* ENGLISH */ "Invalid argument"),
             /* 20 */ "E2BIG",
                      lang3( /* ENGLISH */ "Arg list too long"),
             /* 21 */ "ENOEXEC",
                      lang3( /* ENGLISH */ "Exec format error"),
             /* 22 */ "EXDEV",
                      lang3( /* ENGLISH */ "Cross-device link"),
             /* 23...27 */ "", lang1(""), "", lang1(""), "", lang1(""), "", lang1(""), "", lang1(""),
             /* 28...32 */ "", lang1(""), "", lang1(""), "", lang1(""), "", lang1(""), "", lang1(""),
             /* 33 */ "EDOM",
                      lang3( /* ENGLISH */ "Argument out of domain"),
             /* 34 */ "ERANGE",
                      lang3( /* ENGLISH */ "Result too large"),
             /* 35 */ "EEXIST",
                      lang3( /* ENGLISH */ "File exists"),
             };
           var const char* errorname = errormsg_table[errcode*(1+langcount)];
           var const char* errormsg = translate(errormsg_table[errcode*(1+langcount)+1+language]);
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
    global void OS_error()
      { var uintC errcode; # positive Fehlernummer
        end_system_call(); # just in case
        begin_system_call();
        errcode = errno;
        end_system_call();
        clr_break_sem_4(); # keine DOS-Operation mehr aktiv
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

