# Diverse Funktionen für CLISP
# Bruno Haible 1990-1999

#include "lispbibl.c"


# Eigenwissen:

LISPFUNN(lisp_implementation_type,0)
# (LISP-IMPLEMENTATION-TYPE), CLTL S. 447
  { value1 = O(lisp_implementation_type_string); mv_count=1; }

LISPFUNN(lisp_implementation_version,0)
# (LISP-IMPLEMENTATION-VERSION), CLTL S. 447
  { value1 = O(lisp_implementation_version_string);
    if (nullp(value1)) # noch unbekannt?
      { pushSTACK(O(lisp_implementation_version_date_string));
        pushSTACK(ascii_to_string(" ("));
        pushSTACK(OLS(lisp_implementation_version_month_string));
        pushSTACK(ascii_to_string(" "));
        pushSTACK(O(lisp_implementation_version_year_string));
        pushSTACK(ascii_to_string(")"));
        value1 = O(lisp_implementation_version_string) = string_concat(6);
      }
    mv_count=1;
  }

LISPFUN(version,0,1,norest,nokey,0,NIL)
# (SYSTEM::VERSION) liefert die Version des Runtime-Systems,
# (SYSTEM::VERSION version) überprüft (am Anfang eines FAS-Files),
# ob die Versionen des Runtime-Systems übereinstimmen.
  { var object arg = popSTACK();
    if (eq(arg,unbound))
      { value1 = O(version); mv_count=1; }
      else
      { if (equal(arg,O(version)) || equal(arg,O(oldversion)))
          { value1 = NIL; mv_count=0; }
          else
          { fehler(error,
                   GETTEXT("This file was produced by another lisp version, must be recompiled.")
                  );
  }   }   }

#ifdef MACHINE_KNOWN

LISPFUNN(machinetype,0)
# (MACHINE-TYPE), CLTL S. 447
  { var object erg = O(machine_type_string);
    if (nullp(erg)) # noch unbekannt?
      { # ja -> holen
        #ifdef UNIX
          #ifdef HAVE_SYS_UTSNAME_H
            var struct utsname utsname;
            begin_system_call();
            if ( uname(&utsname) <0) { OS_error(); }
            end_system_call();
            pushSTACK(asciz_to_string(&!utsname.machine,O(misc_encoding)));
            funcall(L(nstring_upcase),1); # in Großbuchstaben umwandeln
            erg = value1;
          #else
            # Betriebssystem-Kommando 'uname -m' bzw. 'arch' ausführen und
            # dessen Output in einen String umleiten:
            # (string-upcase
            #   (with-open-stream (stream (make-pipe-input-stream "/bin/arch"))
            #     (read-line stream nil nil)
            # ) )
            #if defined(UNIX_SUNOS4)
              pushSTACK(ascii_to_string("/bin/arch"));
            #elif defined(UNIX_NEXTSTEP)
              pushSTACK(ascii_to_string("/usr/bin/arch"));
            #else
              pushSTACK(ascii_to_string("uname -m"));
            #endif
            funcall(L(make_pipe_input_stream),1); # (MAKE-PIPE-INPUT-STREAM "/bin/arch")
            pushSTACK(value1); # Stream retten
            pushSTACK(value1); pushSTACK(NIL); pushSTACK(NIL);
            funcall(L(read_line),3); # (READ-LINE stream NIL NIL)
            pushSTACK(value1); # Ergebnis (kann auch NIL sein) retten
            builtin_stream_close(&STACK_1); # Stream schließen
            if (!nullp(STACK_0))
              { erg = string_upcase(STACK_0); } # in Großbuchstaben umwandeln
              else
              { erg = NIL; }
            skipSTACK(2);
          #endif
        #endif
        #ifdef WIN32_NATIVE
          { var SYSTEM_INFO info;
            begin_system_call();
            GetSystemInfo(&info);
            end_system_call();
            if (info.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_INTEL)
              { erg = ascii_to_string("PC/386"); }
          }
        #endif
        # Das Ergebnis merken wir uns für's nächste Mal:
        O(machine_type_string) = erg;
      }
    value1 = erg; mv_count=1;
  }

LISPFUNN(machine_version,0)
# (MACHINE-VERSION), CLTL S. 447
  { var object erg = O(machine_version_string);
    if (nullp(erg)) # noch unbekannt?
      { # ja -> holen
        #ifdef UNIX
          #ifdef HAVE_SYS_UTSNAME_H
            var struct utsname utsname;
            begin_system_call();
            if ( uname(&utsname) <0) { OS_error(); }
            end_system_call();
            pushSTACK(asciz_to_string(&!utsname.machine,O(misc_encoding)));
            funcall(L(nstring_upcase),1); # in Großbuchstaben umwandeln
          #else
            # Betriebssystem-Kommando 'uname -m' bzw. 'arch -k' ausführen und
            # dessen Output in einen String umleiten:
            # (string-upcase
            #   (with-open-stream (stream (make-pipe-input-stream "/bin/arch -k"))
            #     (read-line stream nil nil)
            # ) )
            #if defined(UNIX_SUNOS4)
              pushSTACK(ascii_to_string("/bin/arch -k"));
            #else
              pushSTACK(ascii_to_string("uname -m"));
            #endif
            funcall(L(make_pipe_input_stream),1); # (MAKE-PIPE-INPUT-STREAM "/bin/arch -k")
            pushSTACK(value1); # Stream retten
            pushSTACK(value1); pushSTACK(NIL); pushSTACK(NIL);
            funcall(L(read_line),3); # (READ-LINE stream NIL NIL)
            pushSTACK(value1); # Ergebnis (kann auch NIL sein) retten
            builtin_stream_close(&STACK_1); # Stream schließen
            funcall(L(string_upcase),1); skipSTACK(1); # in Großbuchstaben umwandeln
          #endif
          erg = value1;
        #endif
        #ifdef WIN32_NATIVE
          { var SYSTEM_INFO info;
            var OSVERSIONINFO v;
            begin_system_call();
            GetSystemInfo(&info);
            if (!GetVersionEx(&v)) { OS_error(); }
            end_system_call();
            if (info.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_INTEL)
              { erg = ascii_to_string("PC/386");
                # Check for Windows NT, since the info.wProcessorLevel is
                # garbage on Windows 95.
                if (v.dwPlatformId == VER_PLATFORM_WIN32_NT)
                  TheSstring(erg)->data[3] = '0'+info.wProcessorLevel;
                else {
                  if (info.dwProcessorType == PROCESSOR_INTEL_386)
                    TheSstring(erg)->data[3] = '3';
                  elif (info.dwProcessorType == PROCESSOR_INTEL_486)
                    TheSstring(erg)->data[3] = '4';
                  elif (info.dwProcessorType == PROCESSOR_INTEL_PENTIUM)
                    TheSstring(erg)->data[3] = '5';
                }
          }   }
        #endif
        # Das Ergebnis merken wir uns für's nächste Mal:
        O(machine_version_string) = erg;
      }
    value1 = erg; mv_count=1;
  }

#endif # MACHINE_KNOWN

#ifdef HAVE_ENVIRONMENT

LISPFUNN(get_env,1)
# (SYSTEM::GETENV string) liefert den zu string im Betriebssystem-Environment
# assoziierten String oder NIL.
  { var object arg = popSTACK();
    if (stringp(arg))
      { var const char* found;
        with_string_0(arg,O(misc_encoding),envvar,
          { begin_system_call();
            found = getenv(envvar);
            end_system_call();
          });
        if (!(found==NULL))
          { value1 = asciz_to_string(found,O(misc_encoding)); } # gefunden -> String als Wert
          else
          { value1 = NIL; } # nicht gefunden -> Wert NIL
      }
      else
      { value1 = NIL; } # Kein String -> Wert NIL
    mv_count=1;
  }

#endif

#ifdef WIN32_NATIVE

LISPFUNN(registry,2)
# (SYSTEM::REGISTRY path name) returns the value of path\\name in the registry.
# Used to implement SHORT-SITE-NAME and LONG-SITE-NAME.
  { if (!stringp(STACK_1)) { fehler_string(STACK_1); }
    if (!stringp(STACK_0)) { fehler_string(STACK_0); }
    with_string_0(STACK_1,O(misc_encoding),pathz,
      with_string_0(STACK_0,O(misc_encoding),namez,
        { LONG err;
          HKEY key;
          DWORD type;
          DWORD size;
          begin_system_call();
          err = RegOpenKeyEx(HKEY_LOCAL_MACHINE,pathz,0,KEY_READ, &key);
          if (!(err == ERROR_SUCCESS))
            { if (err == ERROR_FILE_NOT_FOUND) goto none;
              SetLastError(err); OS_error();
            }
          err = RegQueryValueEx(key,namez,NULL,&type, NULL,&size);
          if (!(err == ERROR_SUCCESS))
            { if (err == ERROR_FILE_NOT_FOUND) { RegCloseKey(key); goto none; }
              SetLastError(err); OS_error();
            }
          switch (type)
            { case REG_SZ:
                { var char* buf = (char*)alloca(size);
                  err = RegQueryValueEx(key,namez,NULL,&type, buf,&size);
                  if (!(err == ERROR_SUCCESS)) { SetLastError(err); OS_error(); }
                  err = RegCloseKey(key);
                  if (!(err == ERROR_SUCCESS)) { SetLastError(err); OS_error(); }
                  end_system_call();
                  value1 = asciz_to_string(buf,O(misc_encoding));
                }
                break;
              default:
                { var object path_name;
                  pushSTACK(STACK_1); pushSTACK(O(backslash_string)); pushSTACK(STACK_(0+2));
                  path_name = string_concat(3);
                  pushSTACK(path_name);
                  pushSTACK(TheSubr(subr_self)->name);
                  fehler(error,
                         GETTEXT("~: type of attribute ~ is unsupported")
                        );
                }
            }
        });
      );
    if (FALSE)
      { none: value1 = NIL; }
    mv_count=1;
    skipSTACK(2);
  }

#endif

LISPFUNN(software_type,0)
# (SOFTWARE-TYPE), CLTL S. 448
  { value1 = OLS(software_type_string); mv_count=1; }

LISPFUNN(software_version,0)
# (SOFTWARE-VERSION), CLTL S. 448
  {
    #if defined(GNU)
      value1 = O(software_version_string);
      if (nullp(value1)) # noch unbekannt?
        { pushSTACK(OLS(c_compiler_name));
          pushSTACK(O(c_compiler_version));
          value1 = O(software_version_string) = string_concat(2);
        }
    #else
      value1 = OLS(software_version_string);
    #endif
    mv_count=1;
  }

LISPFUNN(current_language,0)
# (SYS::CURRENT-LANGUAGE) liefert die aktuelle Sprache.
  {
    #ifndef GNU_GETTEXT
      value1 = (ENGLISH ? S(english) : NIL);
    #else # GNU_GETTEXT
      if (nullp(O(current_language_cache)))
        { O(current_language_cache) = OL(current_language); }
      value1 = O(current_language_cache);
    #endif
    mv_count=1;
  }

LISPFUNN(language,3)
# (SYS::LANGUAGE english deutsch francais) liefert je nach der aktuellen
# Sprache das entsprechende Argument.
  {
    #ifndef GNU_GETTEXT
      value1 = (ENGLISH ? STACK_2 : NIL);
    #else
      if (!stringp(STACK_2)) { fehler_string(STACK_2); }
      value1 = localized_string(STACK_2);
    #endif
    mv_count=1;
    skipSTACK(3);
  }

LISPFUNN(identity,1)
# (IDENTITY object), CLTL S. 448
  { value1 = popSTACK(); mv_count=1; }

LISPFUNN(address_of,1)
# (SYS::ADDRESS-OF object) liefert die Adresse von object
  { var object arg = popSTACK();
    #if defined(WIDE_HARD)
      value1 = UQ_to_I(untype(arg));
    #elif defined(WIDE_SOFT)
      value1 = UL_to_I(untype(arg));
    #else
      value1 = UL_to_I(as_oint(arg));
    #endif
    mv_count=1;
  }

#ifdef HAVE_DISASSEMBLER

LISPFUNN(code_address_of,1)
# (SYS::CODE-ADDRESS-OF object) liefert die Adresse des Maschinencodes von object
  { var object obj = popSTACK();
    if (ulong_p(obj)) # Zahl im Bereich eines aint == ulong -> Zahl selbst
      { value1 = obj; }
    elif (subrp(obj)) # SUBR -> seine Adresse
      { value1 = ulong_to_I((aint)(TheSubr(obj)->function)); }
    elif (fsubrp(obj)) # FSUBR -> seine Adresse
      { value1 = ulong_to_I((aint)(TheFsubr(obj)->function)); }
    #ifdef DYNAMIC_FFI
    elif (ffunctionp(obj))
      { value1 = ulong_to_I((uintP)Faddress_value(TheFfunction(obj)->ff_address)); }
    #endif
    else
      { value1 = NIL; }
    mv_count=1;
  }

LISPFUNN(program_id,0)
# (SYS::PROGRAM-ID) returns the pid
  { begin_system_call();
   {var int pid = getpid();
    end_system_call();
    value1 = L_to_I((sint32)pid);
    mv_count=1;
  }}

#endif

LISPFUNN(ansi,0)
  # (SYS::ANSI)
{ value1 = O(ansi); mv_count = 1; }

LISPFUNN(set_ansi,1)
  # (SYS::SET-ANSI ansi-p)
{
  var object val = (eq(popSTACK(),NIL) ? NIL : T);

  # (SETQ *ANSI* val)
  O(ansi) = val;
  # (SETQ *FLOATING-POINT-CONTAGION-ANSI* val)
  Symbol_value(S(floating_point_contagion_ansi)) = val;
  # (SETQ *MERGE-PATHNAMES-ANSI* val)
  Symbol_value(S(merge_pathnames_ansi)) = val;
  # (SETQ *PRINT-PATHNAMES-ANSI* val)
  Symbol_value(S(print_pathnames_ansi)) = val;
  # (SETQ *SEQUENCE-COUNT-ANSI* val)
  Symbol_value(S(sequence_count_ansi)) = val;
  # (SETQ *COERCE-FIXNUM-CHAR-ANSI* val)
  Symbol_value(S(coerce_fixnum_char_ansi)) = val;
  { # *FEATURES*: PUSHNEW or DELETE
    pushSTACK(S(Kansi_cl)); pushSTACK(Symbol_value(S(features)));
    if (eq(val,T))      # (ADJOIN :ANSI-CL *FEATURES*)
      { funcall(L(adjoin),2); }
    else                # (DELETE :ANSI-CL *FEATURES*)
      { funcall(L(delete),2); }
    Symbol_value(S(features)) = value1;
  }
  value1 = val; mv_count = 1;
}
