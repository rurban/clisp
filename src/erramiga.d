  # Behandlung von AMIGAOS-Fehlern
  # OS_error();
  # > IoErr(): Fehlercode
    nonreturning_function(global, OS_error, (void));
    nonreturning_function(global, OS_file_error, (object pathname));
    local void OS_error_internal (uintC errcode);
    local void OS_error_internal(errcode)
      var uintC errcode;
      { # Meldungbeginn ausgeben:
        write_errorstring(GETTEXT("Amiga OS error "));
        # Fehlernummer ausgeben:
        write_errorobject(fixnum(errcode));
        {
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
          local const char* error100_msg_table[23*(1+langcount)] = {
            /* 100 */ "", lang1(""),
            /* 101 */ "", lang1(""),
            /* 102 */ "", lang1(""),
            /* 103 */ "ERROR_NO_FREE_STORE",
                      lang3( /* ENGLISH */ "not enough memory available"),
            /* 104 */ "", lang1(""),
            /* 105 */ "ERROR_TASK_TABLE_FULL",
                      lang3( /* ENGLISH */ "process table full"),
            /* 106 */ "", lang1(""),
            /* 107 */ "", lang1(""),
            /* 108 */ "", lang1(""),
            /* 109 */ "", lang1(""),
            /* 110 */ "", lang1(""),
            /* 111 */ "", lang1(""),
            /* 112 */ "", lang1(""),
            /* 113 */ "", lang1(""),
            /* 114 */ "ERROR_BAD_TEMPLATE",
                      lang3( /* ENGLISH */ "bad template"),
            /* 115 */ "ERROR_BAD_NUMBER",
                      lang3( /* ENGLISH */ "bad number"),
            /* 116 */ "ERROR_REQUIRED_ARG_MISSING",
                      lang3( /* ENGLISH */ "required argument missing"),
            /* 117 */ "ERROR_KEY_NEEDS_ARG",
                      lang3( /* ENGLISH */ "value after keyword missing"),
            /* 118 */ "ERROR_TOO_MANY_ARGS",
                      lang3( /* ENGLISH */ "wrong number of arguments"),
            /* 119 */ "ERROR_UNMATCHED_QUOTES",
                      lang3( /* ENGLISH */ "unmatched quotes"),
            /* 120 */ "ERROR_LINE_TOO_LONG",
                      lang3( /* ENGLISH */ "argument line invalid or too long"),
            /* 121 */ "ERROR_FILE_NOT_OBJECT",
                      lang3( /* ENGLISH */ "file is not executable"),
            /* 122 */ "ERROR_INVALID_RESIDENT_LIBRARY",
                      lang3( /* ENGLISH */ "invalid resident library"),
            };
          local const char* error200_msg_table[44*(1+langcount)] = {
            /* 200 */ "", lang1(""),
            /* 201 */ "ERROR_NO_DEFAULT_DIR",
                      lang3( "" ,
                             "" ,
                             ""),
            /* 202 */ "ERROR_OBJECT_IN_USE",
                      lang3( /* ENGLISH */ "object is in use"),
            /* 203 */ "ERROR_OBJECT_EXISTS",
                      lang3( /* ENGLISH */ "object already exists"),
            /* 204 */ "ERROR_DIR_NOT_FOUND",
                      lang3( /* ENGLISH */ "directory not found"),
            /* 205 */ "ERROR_OBJECT_NOT_FOUND",
                      lang3( /* ENGLISH */ "object not found"),
            /* 206 */ "ERROR_BAD_STREAM_NAME",
                      lang3( /* ENGLISH */ "invalid window description"),
            /* 207 */ "ERROR_OBJECT_TOO_LARGE",
                      lang3( /* ENGLISH */ "object too large"),
            /* 208 */ "", lang1(""),
            /* 209 */ "ERROR_ACTION_NOT_KNOWN",
                      lang3( /* ENGLISH */ "packet request type unknown"),
            /* 210 */ "ERROR_INVALID_COMPONENT_NAME",
                      lang3( /* ENGLISH */ "object name invalid"),
            /* 211 */ "ERROR_INVALID_LOCK",
                      lang3( /* ENGLISH */ "invalid object lock"),
            /* 212 */ "ERROR_OBJECT_WRONG_TYPE",
                      lang3( /* ENGLISH */ "object is not of required type"),
            /* 213 */ "ERROR_DISK_NOT_VALIDATED",
                      lang3( /* ENGLISH */ "disk not validated"),
            /* 214 */ "ERROR_DISK_WRITE_PROTECTED",
                      lang3( /* ENGLISH */ "disk is write-protected"),
            /* 215 */ "ERROR_RENAME_ACROSS_DEVICES",
                      lang3( /* ENGLISH */ "rename across devices attempted"),
            /* 216 */ "ERROR_DIRECTORY_NOT_EMPTY",
                      lang3( /* ENGLISH */ "directory not empty"),
            /* 217 */ "ERROR_TOO_MANY_LEVELS",
                      lang3( /* ENGLISH */ "too many levels"),
            /* 218 */ "ERROR_DEVICE_NOT_MOUNTED",
                      lang3( /* ENGLISH */ "device (or volume) is not mounted"),
            /* 219 */ "ERROR_SEEK_ERROR",
                      lang3( /* ENGLISH */ "seek failure"),
            /* 220 */ "ERROR_COMMENT_TOO_BIG",
                      lang3( /* ENGLISH */ "comment is too long"),
            /* 221 */ "ERROR_DISK_FULL",
                      lang3( /* ENGLISH */ "disk is full"),
            /* 222 */ "ERROR_DELETE_PROTECTED",
                      lang3( /* ENGLISH */ "object is protected from deletion"),
            /* 223 */ "ERROR_WRITE_PROTECTED",
                      lang3( /* ENGLISH */ "file is write protected"),
            /* 224 */ "ERROR_READ_PROTECTED",
                      lang3( /* ENGLISH */ "file is read protected"),
            /* 225 */ "ERROR_NOT_A_DOS_DISK",
                      lang3( /* ENGLISH */ "not a valid DOS disk"),
            /* 226 */ "ERROR_NO_DISK",
                      lang3( /* ENGLISH */ "no disk in drive"),
            /* 227 */ "", lang1(""),
            /* 228 */ "", lang1(""),
            /* 229 */ "", lang1(""),
            /* 230 */ "", lang1(""),
            /* 231 */ "", lang1(""),
            /* 232 */ "ERROR_NO_MORE_ENTRIES",
                      lang3( /* ENGLISH */ "no more entries in directory"),
            /* 233 */ "ERROR_IS_SOFT_LINK",
                      lang3( /* ENGLISH */ "object is soft link"),
            /* 234 */ "ERROR_OBJECT_LINKED",
                      lang3( /* ENGLISH */ "object is linked"),
            /* 235 */ "ERROR_BAD_HUNK",
                      lang3( /* ENGLISH */ "bad loadfile hunk"),
            /* 236 */ "ERROR_NOT_IMPLEMENTED",
                      lang3( /* ENGLISH */ "function not implemented"),
            /* 237 */ "", lang1(""),
            /* 238 */ "", lang1(""),
            /* 239 */ "", lang1(""),
            /* 240 */ "ERROR_RECORD_NOT_LOCKED",
                      lang3( /* ENGLISH */ "record not locked"),
            /* 241 */ "ERROR_LOCK_COLLISION",
                      lang3( /* ENGLISH */ "record lock collision"),
            /* 242 */ "ERROR_LOCK_TIMEOUT",
                      lang3( /* ENGLISH */ "record lock timeout"),
            /* 243 */ "ERROR_UNLOCK_ERROR",
                      lang3( /* ENGLISH */ "record unlock error"),
            };
          local const char* error300_msg_table[6*(1+langcount)] = {
            /* 300 */ "", lang1(""),
            /* 301 */ "", lang1(""),
            /* 302 */ "", lang1(""),
            /* 303 */ "ERROR_BUFFER_OVERFLOW",
                      lang3( /* ENGLISH */ "buffer overflow"),
            /* 304 */ "ERROR_BREAK",
                      lang3( /* ENGLISH */ "break"),
            /* 305 */ "ERROR_NOT_EXECUTABLE",
                      lang3( /* ENGLISH */ "file not executable"),
            };
          var const char* errorname = "";
          var const char* errormsg = "";
          var uintC index;
          if (errcode == 0)
            { errorname = "";
              errormsg =
                /*  0 */ GETTEXT("Ok, No error");
            }
          elif ((index = errcode-100) < 23)
            { errorname = error100_msg_table[index*(1+langcount)];
              errormsg = translate(error100_msg_table[index*(1+langcount)+1+language]);
            }
          elif ((index = errcode-200) < 44)
            { errorname = error200_msg_table[index*(1+langcount)];
              errormsg = translate(error200_msg_table[index*(1+langcount)+1+language]);
            }
          elif ((index = errcode-300) < 6)
            { errorname = error300_msg_table[index*(1+langcount)];
              errormsg = translate(error300_msg_table[index*(1+langcount)+1+language]);
            }
          if (!(errorname[0] == 0)) # bekannter Name?
            { write_errorasciz(" (");
              write_errorasciz(errorname);
              write_errorasciz(")");
            }
          if (!(errormsg[0] == 0)) # nichtleere Meldung?
            { write_errorasciz(": ");
              write_errorasciz(errormsg);
            }
        }
        SetIoErr(0L); # Fehlercode löschen (fürs nächste Mal)
      }
    global void OS_error()
      { var uintC errcode; # Fehlernummer
        end_system_call(); # just in case
        begin_system_call();
        errcode = IoErr();
        end_system_call();
        clr_break_sem_4(); # keine AMIGAOS-Operation mehr aktiv
        begin_error(); # Fehlermeldung anfangen
        if (!nullp(STACK_3)) # *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ?
          { STACK_3 = S(simple_os_error); }
        OS_error_internal(errcode);
        end_error(args_end_pointer STACKop 7); # Fehlermeldung beenden
      }
    global void OS_file_error(pathname)
      var object pathname;
      { var uintC errcode; # Fehlernummer
        begin_system_call();
        errcode = IoErr();
        end_system_call();
        clr_break_sem_4(); # keine AMIGAOS-Operation mehr aktiv
        pushSTACK(pathname); # Wert von PATHNAME für FILE-ERROR
        begin_error(); # Fehlermeldung anfangen
        if (!nullp(STACK_3)) # *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ?
          { STACK_3 = S(simple_file_error); }
        OS_error_internal(errcode);
        end_error(args_end_pointer STACKop 7); # Fehlermeldung beenden
      }

  # Ausgabe eines Fehlers, direkt übers Betriebssystem
  # errno_out(errorcode);
  # > LONG errorcode: Fehlercode
    global void errno_out (LONG errorcode);
    global void errno_out(errorcode)
      var LONG errorcode;
      { asciz_out(" IoErr() = "); dez_out(errorcode); asciz_out("." NLstring); }

