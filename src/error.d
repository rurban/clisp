# Error-Handling für CLISP
# Bruno Haible 1990-1999
# Marcus Daniels 8.4.1994

#include "lispbibl.c"


# SYS::*RECURSIVE-ERROR-COUNT* = Rekursionstiefe der Ausgabe von Errormeldungen

# UP: Beginnt die Ausgabe einer Errormeldung.
# begin_error()
# < STACK_0: Stream (i.a. *ERROR-OUTPUT*)
# < STACK_1: Wert von *error-handler*
# < STACK_2: Argumentliste für *error-handler*
# < STACK_3: Condition-Typ (i.a. SIMPLE-ERROR) oder NIL
# erniedrigt STACK um 7
  local void begin_error (void);
  local void begin_error()
    { end_system_call(); # keine Betriebssystem-Operation läuft mehr
      #ifdef PENDING_INTERRUPTS
        interrupt_pending = FALSE; # Ctrl-C-Wartezeit ist gleich beendet
        #ifndef WIN32_NATIVE
          begin_system_call();
          #ifdef HAVE_UALARM
            ualarm(0,0); # SIGALRM-Timer abbrechen
          #else
            alarm(0); # SIGALRM-Timer abbrechen
          #endif
          end_system_call();
        #endif
      #endif
      #if defined(HAVE_SIGNALS) && defined(SIGPIPE)
        writing_to_subprocess = FALSE;
      #endif
      # Error-Count erhöhen, bei >3 Ausgabe-Abbruch:
      dynamic_bind(S(recursive_error_count),fixnum_inc(Symbol_value(S(recursive_error_count)),1));
      if (!posfixnump(Symbol_value(S(recursive_error_count)))) # sollte ein Fixnum >=0 sein
        { Symbol_value(S(recursive_error_count)) = Fixnum_0; } # sonst Notkorrektur
      if (posfixnum_to_L(Symbol_value(S(recursive_error_count))) > 3)
        { # Mehrfach verschachtelte Fehlermeldung.
          Symbol_value(S(recursive_error_count)) = Fixnum_0; # Error-Count löschen
          # *PRINT-PRETTY* an NIL binden (um Speicher zu sparen):
          dynamic_bind(S(print_pretty),NIL);
          fehler(serious_condition,
                 # Note: All translations of this error message should be in
                 # pure ASCII, to avoid endless recursion if *terminal-encoding*
                 # supports only ASCII characters.
                 GETTEXT("Unprintable error message")
                );
        }
     {var object error_handler = Symbol_value(S(error_handler)); # *ERROR-HANDLER*
      if (!nullp(error_handler))
        # *ERROR-HANDER* /= NIL
        { pushSTACK(NIL); pushSTACK(NIL); pushSTACK(error_handler);
          pushSTACK(make_string_output_stream()); # String-Output-Stream
        }
        else
        if (nullp(Symbol_value(S(use_clcs)))) # SYS::*USE-CLCS*
          # *ERROR-HANDER* = NIL, SYS::*USE-CLCS* = NIL
          { pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
            pushSTACK(var_stream(S(error_output),strmflags_wr_ch_B)); # Stream *ERROR-OUTPUT*
            terpri(&STACK_0); # neue Zeile
            write_sstring(&STACK_0,O(error_string1)); # "*** - " ausgeben
          }
          else
          # *ERROR-HANDER* = NIL, SYS::*USE-CLCS* /= NIL
          { pushSTACK(S(simple_error)); pushSTACK(NIL); pushSTACK(unbound);
            pushSTACK(make_string_output_stream()); # String-Output-Stream
          }
    }}

# UP: Gibt ein Error-Objekt aus.
  local void write_errorobject (object obj);
  local void write_errorobject(obj)
    var object obj;
    { if (nullp(STACK_1))
        { dynamic_bind(S(prin_stream),unbound); # SYS::*PRIN-STREAM* an #<UNBOUND> binden
          dynamic_bind(S(print_escape),T); # *PRINT-ESCAPE* an T binden
          prin1(&STACK_(0+3+3),obj); # direkt ausgeben
          dynamic_unbind();
          dynamic_unbind();
        }
        else
        { # obj auf die Argumentliste schieben:
          pushSTACK(obj);
          obj = allocate_cons();
          Car(obj) = popSTACK();
          Cdr(obj) = STACK_2; STACK_2 = obj;
          # und "~S" in den Format-String schreiben:
          write_ascii_char(&STACK_0,'~'); write_ascii_char(&STACK_0,'S');
    }   }

# UP: Gibt ein Error-Character aus.
  local void write_errorchar (object obj);
  local void write_errorchar(obj)
    var object obj;
    { if (nullp(STACK_1))
        { write_char(&STACK_0,obj); } # direkt ausgeben
        else
        { # obj auf die Argumentliste schieben:
          pushSTACK(obj);
          obj = allocate_cons();
          Car(obj) = popSTACK();
          Cdr(obj) = STACK_2; STACK_2 = obj;
          # und "~A" in den Format-String schreiben:
          write_ascii_char(&STACK_0,'~'); write_ascii_char(&STACK_0,'A');
    }   }

# UP: Gibt einen Errorstring unverändert aus.
# write_errorasciz(asciz);
  local void write_errorasciz (const char* asciz);
  local void write_errorasciz(asciz)
    var const char* asciz;
    { 
      #ifdef UNICODE
      var object encoding = O(internal_encoding);
      var const uintB* bptr = (const uintB*)asciz;
      var const uintB* bendptr = bptr + asciz_length(asciz);
      var uintL clen = Encoding_mblen(encoding)(encoding,bptr,bendptr);
      if (clen > 0)
        { var DYNAMIC_ARRAY(charbuf,chart,clen);
          { var chart* cptr = &charbuf[0];
            var chart* cendptr = cptr+clen;
            Encoding_mbstowcs(encoding)(encoding,nullobj,&bptr,bendptr,&cptr,cendptr);
            ASSERT(cptr == cendptr);
          }
          { var const chart* cptr = &charbuf[0];
            dotimespL(clen,clen, { write_code_char(&STACK_0,*cptr); cptr++; });
          }
          FREE_DYNAMIC_ARRAY(charbuf);
        }
      #else
      var const uintB* bptr = (const uintB*)asciz;
      while (*bptr != '\0') { write_code_char(&STACK_0,as_chart(*bptr)); bptr++; }
      #endif
    }

# UP: Gibt einen Errorstring aus. Bei jeder Tilde '~' wird ein Objekt aus dem
# Stack ausgegeben, bei jedem '$' wird ein Character aus dem Stack ausgegeben.
# write_errorstring(errorstring)
# > STACK_0: Stream usw.
# > errorstring: Errorstring (ein unverschieblicher ASCIZ-String)
# > STACK_7, STACK_8, ...: Argumente (für jedes '~' bzw. '$' eines),
#   in umgekehrter Reihenfolge wie bei FUNCALL !
# < ergebnis: STACK-Wert oberhalb des Stream und der Argumente
  local object* write_errorstring (const char* errorstring);
  local object* write_errorstring(errorstring)
    var const char* errorstring;
    { var object* argptr = args_end_pointer STACKop 7; # Pointer übern Stream und Frame
      loop
        { var uintB ch = *errorstring++; # nächstes Zeichen
          if (ch==0) break; # String zu Ende?
          if (ch=='~') # Tilde?
            # ja -> ein Objekt vom Stack ausgeben:
            { write_errorobject(BEFORE(argptr)); }
          elif (ch=='$') # '$' ?
            # ja -> ein Character vom Stack ausgeben:
            { write_errorchar(BEFORE(argptr)); }
          else
            # nein -> Zeichen normal ausgeben:
            {
              #ifdef UNICODE
              var object encoding = O(internal_encoding);
              var chart chbuf[1];
              var const uintB* ptr1 = &ch;
              var chart* ptr2 = &chbuf[0];
              Encoding_mbstowcs(encoding)(encoding,nullobj,&ptr1,ptr1+1,&ptr2,ptr2+1);
              if (ptr2 == &chbuf[1]) { write_code_char(&STACK_0,chbuf[0]); }
              #else
              write_code_char(&STACK_0,as_chart(ch));
              #endif
            }
        }
      return argptr;
    }

# Beendet die Ausgabe einer Fehlermeldung und startet neuen Driver.
# end_error();
  nonreturning_function(local, end_error, (object* stackptr));
  local void end_error(stackptr)
    var object* stackptr;
    { if (nullp(STACK_1))
        # *ERROR-HANDER* = NIL, SYS::*USE-CLCS* = NIL
        { skipSTACK(4); # Fehlermeldung wurde schon ausgegeben
          dynamic_unbind(); # Bindungsframe für sys::*recursive-error-count* auflösen,
                            # da keine Fehlermeldungs-Ausgabe mehr aktiv
          set_args_end_pointer(stackptr);
          break_driver(NIL); # Break-Driver aufrufen (kehrt nicht zurück)
        }
        else
        { STACK_0 = get_output_stream_string(&STACK_0);
         {var object arguments = nreverse(STACK_2);
          # Stackaufbau: type, args, handler, errorstring.
          if (!eq(STACK_1,unbound))
            # *ERROR-HANDER* /= NIL
            { # Stackaufbau: nil, args, handler, errorstring.
              # (apply *error-handler* nil errorstring args) ausführen:
              check_SP(); check_STACK();
              {var object error_handler = STACK_1; STACK_1 = NIL;
               apply(error_handler,2,arguments);
               skipSTACK(2);
              }
              dynamic_unbind(); # Bindungsframe für sys::*recursive-error-count* auflösen,
                                # da keine Fehlermeldungs-Ausgabe mehr aktiv
              set_args_end_pointer(stackptr);
              break_driver(NIL); # Break-Driver aufrufen (kehrt nicht zurück)
            }
            else
            # *ERROR-HANDER* = NIL, SYS::*USE-CLCS* /= NIL
            { # Stackaufbau: type, args, --, errorstring.
              var object type = STACK_3;
              var object errorstring = STACK_0;
              skipSTACK(4);
              dynamic_unbind(); # Bindungsframe für sys::*recursive-error-count* auflösen
              # (APPLY #'coerce-to-condition errorstring args 'error type keyword-arguments)
              # ausführen:
              pushSTACK(errorstring); pushSTACK(arguments); pushSTACK(S(error)); pushSTACK(type);
             {var uintC argcount = 4;
              # arithmetic-error, division-by-zero, floating-point-overflow, floating-point-underflow
              #   --> ergänze :operation :operands ??
              # cell-error, uncound-variable, undefined-function, unbound-slot
              #   --> ergänze :name
              if (eq(type,S(simple_cell_error))
                  || eq(type,S(simple_unbound_variable))
                  || eq(type,S(simple_undefined_function))
                  || eq(type,S(simple_unbound_slot))
                 )
                { pushSTACK(S(Kname)); pushSTACK(BEFORE(stackptr)); # :name ...
                  argcount += 2;
                }
              # unbound-slot --> ergänze :instance
              if (eq(type,S(simple_unbound_slot)))
                { pushSTACK(S(Kinstance)); pushSTACK(BEFORE(stackptr)); # :instance ...
                  argcount += 2;
                }
              # type-error, keyword-error --> ergänze :datum, :expected-type
              if (eq(type,S(simple_type_error))
                  || eq(type,S(simple_keyword_error))
                  || eq(type,S(simple_charset_type_error))
                 )
                { pushSTACK(S(Kexpected_type)); pushSTACK(BEFORE(stackptr)); # :expected-type ...
                  pushSTACK(S(Kdatum)); pushSTACK(BEFORE(stackptr)); # :datum ...
                  argcount += 4;
                }
              # package-error --> ergänze :package
              if (eq(type,S(simple_package_error)))
                { pushSTACK(S(Kpackage)); pushSTACK(BEFORE(stackptr)); # :package ...
                  argcount += 2;
                }
              # print-not-readable --> ergänze :object
              if (eq(type,S(simple_print_not_readable)))
                { pushSTACK(S(Kobject)); pushSTACK(BEFORE(stackptr)); # :object
                  argcount += 2;
                }
              # stream-error, end-of-file --> ergänze :stream
              if (eq(type,S(simple_stream_error))
                  || eq(type,S(simple_end_of_file))
                 )
                { pushSTACK(S(Kstream)); pushSTACK(BEFORE(stackptr)); # :stream ...
                  argcount += 2;
                }
              # file-error --> ergänze :pathname
              if (eq(type,S(simple_file_error)))
                { pushSTACK(S(Kpathname)); pushSTACK(BEFORE(stackptr)); # :pathname ...
                  argcount += 2;
                }
              funcall(S(coerce_to_condition),argcount); # (SYS::COERCE-TO-CONDITION ...)
              # set_args_end_pointer(stackptr); # wozu? macht das Debuggen nur schwieriger!
              pushSTACK(value1); # condition retten
              pushSTACK(value1); funcall(L(clcs_signal),1); # (SIGNAL condition)
              dynamic_bind(S(prin_stream),unbound); # SYS::*PRIN-STREAM* an #<UNBOUND> binden
              pushSTACK(STACK_(0+3)); # condition
              funcall(L(invoke_debugger),1); # (INVOKE-DEBUGGER condition)
            }}
        }}
      NOTREACHED
    }

# Fehlermeldung mit Errorstring. Kehrt nicht zurück.
# fehler(errortype,errorstring);
# > errortype: Condition-Typ
# > errorstring: Konstanter ASCIZ-String.
#   Bei jeder Tilde wird ein LISP-Objekt vom STACK genommen und statt der
#   Tilde ausgegeben.
# > auf dem STACK: Initialisierungswerte für die Condition, je nach errortype
  nonreturning_function(global, fehler, (conditiontype errortype, const char * errorstring));
  global void fehler(errortype,errorstring)
    var conditiontype errortype;
    var const char * errorstring;
    { begin_error(); # Fehlermeldung anfangen
      if (!nullp(STACK_3)) # *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ?
        { # Error-Typ-Symbol zu errortype auswählen:
          var object sym = S(simple_condition); # erster Error-Typ
          sym = objectplus(sym,
                           (soint)(sizeof(*TheSymbol(sym))<<(oint_addr_shift-addr_shift))
                           * (uintL)errortype
                          );
          STACK_3 = sym;
        }
      end_error(write_errorstring(errorstring)); # Fehlermeldung ausgeben, beenden
    }

#undef OS_error
#undef OS_file_error
#undef OS_filestream_error

#ifdef AMIGAOS
#include "erramiga.c"
#endif

#ifdef DJUNIX
#include "errdjgpp.c"
#endif

#if defined(UNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
#include "errunix.c"
#endif # UNIX || EMUNIX || WATCOM || RISCOS

#ifdef WIN32_NATIVE
#include "errwin32.c"
#endif

# Just like OS_error, but takes a file stream and signals a FILE-ERROR.
# OS_filestream_error(stream);
# > stream: a file stream
# > end_system_call() already called
  nonreturning_function(global, OS_filestream_error, (object stream));
  global void OS_filestream_error(stream)
    var object stream;
    { if (TheStream(stream)->strmtype == strmtype_file
          && !nullp(TheStream(stream)->strm_file_truename)
         )
        { OS_file_error(TheStream(stream)->strm_file_truename); }
        else
        { OS_error(); }
    }

LISPFUN(error,1,0,rest,nokey,0,NIL)
# (ERROR errorstring {expr})
# Kehrt nicht zurück.
# (defun error (errorstring &rest args)
#   (if (or *error-handler* (not *use-clcs*))
#     (progn
#       (if *error-handler*
#         (apply *error-handler* nil errorstring args)
#         (progn
#           (terpri *error-output*)
#           (write-string "*** - " *error-output*)
#           (apply #'format *error-output* errorstring args)
#       ) )
#       (funcall *break-driver* nil)
#     )
#     (let ((condition (coerce-to-condition errorstring args 'error 'simple-error)))
#       (signal condition)
#       (invoke-debugger condition)
#     )
# ) )
  { if (!nullp(Symbol_value(S(error_handler))) || nullp(Symbol_value(S(use_clcs))))
      { begin_error(); # Fehlermeldung anfangen
        rest_args_pointer skipSTACKop 1; # Pointer über die Argumente
        {var object fun;
         var object arg1;
         if (nullp(STACK_1))
           { fun = S(format); arg1 = STACK_0; } # (FORMAT *error-output* ...)
           else
           { fun = STACK_1; arg1 = NIL; } # (FUNCALL *error-handler* NIL ...)
         skipSTACK(4);
         # Errormeldung ausgeben:
         #   (FORMAT *ERROR-OUTPUT* errorstring {expr})
         # bzw. ({handler} nil errorstring {expr})
         pushSTACK(arg1);
         { var object* ptr = rest_args_pointer;
           var uintC count;
           dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
         }
         funcall(fun,2+argcount); # fun (= FORMAT bzw. handler) aufrufen
        }
        # Fehlermeldung beenden, vgl. end_error():
        dynamic_unbind(); # Keine Fehlermeldungs-Ausgabe mehr aktiv
        set_args_end_pointer(rest_args_pointer); # STACK aufräumen
        break_driver(NIL); # Break-Driver aufrufen (kehrt nicht zurück)
      }
      else
      { {var object arguments = listof(argcount); pushSTACK(arguments); }
        pushSTACK(S(error));
        pushSTACK(S(simple_error));
        funcall(S(coerce_to_condition),4); # (SYS::COERCE-TO-CONDITION ...)
        pushSTACK(value1); # condition retten
        pushSTACK(value1); funcall(L(clcs_signal),1); # (SIGNAL condition)
        dynamic_bind(S(prin_stream),unbound); # SYS::*PRIN-STREAM* an #<UNBOUND> binden
        pushSTACK(STACK_(0+3)); # condition
        funcall(L(invoke_debugger),1); # (INVOKE-DEBUGGER condition)
      }
    NOTREACHED
  }

LISPFUNN(defclcs,1)
# (SYSTEM::%DEFCLCS error-types)
# setzt die für ERROR-OF-TYPE benötigten Daten.
  { O(error_types) = popSTACK();
    value1 = NIL; mv_count=0;
  }

# Konvertiert einen Condition-Typ zur entsprechenden Simple-Condition.
# convert_simple_condition(type)
  local object convert_simple_condition (object type);
  local object convert_simple_condition(type)
    var object type;
    { # Vektor O(error_types) wie eine Aliste durchlaufen:
      var object v = O(error_types);
      var uintL count = Svector_length(v);
      if (count > 0)
        { var object* ptr = &TheSvector(v)->data[0];
          dotimespL(count,count,
            { if (eq(type,Car(*ptr))) { return Cdr(*ptr); }
              ptr++;
            });
        }
      return type; # nicht gefunden -> Typ unverändert lassen
    }

LISPFUN(cerror_of_type,3,0,rest,nokey,0,NIL)
# (SYSTEM::CERROR-OF-TYPE continue-format-string type {keyword value}* error-format-string {arg}*)
# (defun cerror-of-type (continue-format-string type &rest arguments)
#   (let ((keyword-arguments '()))
#     (loop
#       (unless (and (consp arguments) (symbolp (car arguments))) (return))
#       (push (pop arguments) keyword-arguments)
#       (push (pop arguments) keyword-arguments)
#     )
#     (setq keyword-arguments (nreverse keyword-arguments))
#     (let ((error-format-string (first arguments))
#           (args (rest arguments)))
#       (apply #'cerror
#         continue-format-string
#         (if (or *error-handler* (not *use-clcs*))
#           error-format-string
#           (apply #'coerce-to-condition error-format-string args
#                  'cerror (convert-simple-condition type) keyword-arguments
#         ) )
#         args
# ) ) ) )
  { var object* cfstring_ = &Next(rest_args_pointer STACKop 3);
    var uintC keyword_argcount = 0;
    rest_args_pointer skipSTACKop 1; # Pointer über die Argumente hinter type
    while (argcount>=2)
      { var object next_arg = Next(rest_args_pointer); # nächstes Argument
        if (!symbolp(next_arg)) break; # Keyword?
        rest_args_pointer skipSTACKop -2; argcount -= 2; keyword_argcount += 2;
      }
    # Nächstes Argument hoffentlich ein String.
    if (!nullp(Symbol_value(S(error_handler))) || nullp(Symbol_value(S(use_clcs))))
      { # Der Typ und die Keyword-Argumente werden ignoriert.
        BEFORE(rest_args_pointer) = *cfstring_;
        funcall(S(cerror),argcount+2);
        skipSTACK(keyword_argcount+1);
      }
      else
      { var object arguments = listof(argcount);
        # Stackaufbau: continue-format-string, type, {keyword, value}*, errorstring.
        # Ein wenig im Stack umordnen:
        var object errorstring = STACK_0;
        pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
        { var object* ptr2 = args_end_pointer;
          var object* ptr1 = ptr2 STACKop 4;
          var uintC count;
          dotimesC(count,keyword_argcount, { BEFORE(ptr2) = BEFORE(ptr1); } );
          BEFORE(ptr2) = convert_simple_condition(BEFORE(ptr1));
          BEFORE(ptr2) = S(cerror);
          BEFORE(ptr2) = arguments;
          BEFORE(ptr2) = errorstring;
          BEFORE(ptr2) = arguments;
        }
        # Stackaufbau: continue-format-string, arguments, errorstring, args, CERROR, type, {keyword, value}*.
        funcall(S(coerce_to_condition),4+keyword_argcount); # (SYS::COERCE-TO-CONDITION ...)
        # Stackaufbau: continue-format-string, arguments.
        arguments = STACK_0;
        STACK_0 = value1;
        apply(S(cerror),2,arguments); # (CERROR continue-format-string condition ...)
  }   }

LISPFUN(error_of_type,2,0,rest,nokey,0,NIL)
# (SYSTEM::ERROR-OF-TYPE type {keyword value}* errorstring {expr}*)
# Kehrt nicht zurück.
# (defun error-of-type (type &rest arguments)
#   ; Keyword-Argumente von den anderen Argumenten abspalten:
#   (let ((keyword-arguments '()))
#     (loop
#       (unless (and (consp arguments) (symbolp (car arguments))) (return))
#       (push (pop arguments) keyword-arguments)
#       (push (pop arguments) keyword-arguments)
#     )
#     (setq keyword-arguments (nreverse keyword-arguments))
#     (let ((errorstring (first arguments))
#           (args (rest arguments)))
#       ; Los geht's!
#       (if (or *error-handler* (not *use-clcs*))
#         (progn
#           (if *error-handler*
#             (apply *error-handler* nil errorstring args)
#             (progn
#               (terpri *error-output*)
#               (write-string "*** - " *error-output*)
#               (apply #'format *error-output* errorstring args)
#           ) )
#           (funcall *break-driver* nil)
#         )
#         (let ((condition
#                 (apply #'coerce-to-condition errorstring args
#                        'error (convert-simple-condition type) keyword-arguments
#              )) )
#           (signal condition)
#           (invoke-debugger condition)
#         )
# ) ) ) )
  { var uintC keyword_argcount = 0;
    rest_args_pointer skipSTACKop 1; # Pointer über die Argumente hinter type
    while (argcount>=2)
      { var object next_arg = Next(rest_args_pointer); # nächstes Argument
        if (!symbolp(next_arg)) break; # Keyword?
        rest_args_pointer skipSTACKop -2; argcount -= 2; keyword_argcount += 2;
      }
    # Nächstes Argument hoffentlich ein String.
    if (!nullp(Symbol_value(S(error_handler))) || nullp(Symbol_value(S(use_clcs))))
      { # Der Typ und die Keyword-Argumente werden ignoriert.
        begin_error(); # Fehlermeldung anfangen
        {var object fun;
         var object arg1;
         if (nullp(STACK_1))
           { fun = S(format); arg1 = STACK_0; } # (FORMAT *error-output* ...)
           else
           { fun = STACK_1; arg1 = NIL; } # (FUNCALL *error-handler* NIL ...)
         skipSTACK(4);
         # Errormeldung ausgeben:
         #   (FORMAT *ERROR-OUTPUT* errorstring {expr})
         # bzw. ({handler} nil errorstring {expr})
         pushSTACK(arg1);
         { var object* ptr = rest_args_pointer;
           var uintC count;
           dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
         }
         funcall(fun,2+argcount); # fun (= FORMAT bzw. handler) aufrufen
        }
        # Fehlermeldung beenden, vgl. end_error():
        dynamic_unbind(); # Keine Fehlermeldungs-Ausgabe mehr aktiv
        set_args_end_pointer(rest_args_pointer); # STACK aufräumen
        break_driver(NIL); # Break-Driver aufrufen (kehrt nicht zurück)
      }
      else
      { var object arguments = listof(argcount);
        # Stackaufbau: type, {keyword, value}*, errorstring.
        # Ein wenig im Stack umordnen:
        var object errorstring = STACK_0;
        pushSTACK(NIL); pushSTACK(NIL);
        { var object* ptr2 = args_end_pointer;
          var object* ptr1 = ptr2 STACKop 3;
          var uintC count;
          dotimesC(count,keyword_argcount, { BEFORE(ptr2) = BEFORE(ptr1); } );
          BEFORE(ptr2) = convert_simple_condition(BEFORE(ptr1));
          BEFORE(ptr2) = S(error);
          BEFORE(ptr2) = arguments;
          BEFORE(ptr2) = errorstring;
        }
        # Stackaufbau: errorstring, args, ERROR, type, {keyword, value}*.
        funcall(S(coerce_to_condition),4+keyword_argcount); # (SYS::COERCE-TO-CONDITION ...)
        pushSTACK(value1); # condition retten
        pushSTACK(value1); funcall(L(clcs_signal),1); # (SIGNAL condition)
        dynamic_bind(S(prin_stream),unbound); # SYS::*PRIN-STREAM* an #<UNBOUND> binden
        pushSTACK(STACK_(0+3)); # condition
        funcall(L(invoke_debugger),1); # (INVOKE-DEBUGGER condition)
      }
    NOTREACHED
  }

LISPFUNN(invoke_debugger,1)
# (INVOKE-DEBUGGER condition), CLtL2 S. 915
# Kehrt nicht zurück.
# (defun invoke-debugger (condition)
#   (when *debugger-hook*
#     (let ((debugger-hook *debugger-hook*)
#           (*debugger-hook* nil))
#       (funcall debugger-hook condition debugger-hook)
#   ) )
#   (funcall *break-driver* nil condition t)
# )
  { var object hook = Symbol_value(S(debugger_hook));
    if (!nullp(hook))
      { var object condition = STACK_0;
        dynamic_bind(S(debugger_hook),NIL); # *DEBUGGER-HOOK* an NIL binden
        pushSTACK(condition); pushSTACK(hook); funcall(hook,2); # Debugger-Hook aufrufen
        dynamic_unbind();
      }
    # *BREAK-DRIVER* kann hier als /= NIL angenommen werden.
    pushSTACK(NIL); pushSTACK(STACK_(0+1)); pushSTACK(T);
    funcall(Symbol_value(S(break_driver)),3); # Break-Driver aufrufen
    reset(); # kehrt wider Erwarten zurück -> zur nächsten Schleife zurück
    NOTREACHED
  }

# UP: Führt eine Break-Schleife wegen Tastaturunterbrechung aus.
# > STACK_0 : aufrufende Funktion
# verändert STACK, kann GC auslösen
  global void tast_break (void);
  global void tast_break()
    {
      #ifdef PENDING_INTERRUPTS
        interrupt_pending = FALSE; # Ctrl-C-Wartezeit ist gleich beendet
        #ifndef WIN32_NATIVE
          begin_system_call();
          #ifdef HAVE_UALARM
            ualarm(0,0); # SIGALRM-Timer abbrechen
          #else
            alarm(0); # SIGALRM-Timer abbrechen
          #endif
          end_system_call();
        #endif
      #endif
      #if defined(HAVE_SIGNALS) && defined(SIGPIPE)
        writing_to_subprocess = FALSE;
      #endif
      # Simuliere begin_error(), 7 Elemente auf den STACK:
      pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
      pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
      pushSTACK(var_stream(S(debug_io),strmflags_wr_ch_B)); # Stream *DEBUG-IO*
      terpri(&STACK_0); # neue Zeile
      write_sstring(&STACK_0,O(error_string1)); # "*** - " ausgeben
      # String ausgeben, Aufrufernamen verbrauchen, STACK aufräumen:
      set_args_end_pointer(
        write_errorstring(GETTEXT("~: User break")));
      break_driver(T); # Break-Driver aufrufen
    }

LISPFUN(clcs_signal,1,0,rest,nokey,0,NIL)
# (SIGNAL datum {arg}*), CLtL2 S. 888
# (defun signal (datum &rest arguments)
#   (let ((condition
#           (coerce-to-condition datum arguments 'signal
#                                'simple-condition ; CLtL2 p. 918 specifies this
#        )) )
#     (when (typep condition *break-on-signals*)
#       ; Enter the debugger prior to signalling the condition
#       (restart-case (invoke-debugger condition)
#         (continue ())
#     ) )
#     (invoke-handlers condition)
#     nil
# ) )
  { {var object arguments = listof(argcount); pushSTACK(arguments); }
    pushSTACK(S(clcs_signal));
    pushSTACK(S(simple_condition));
    funcall(S(coerce_to_condition),4); # (SYS::COERCE-TO-CONDITION ...)
    pushSTACK(value1); # condition retten
    pushSTACK(value1); pushSTACK(Symbol_value(S(break_on_signals)));
    funcall(S(safe_typep),2); # (SYS::SAFE-TYPEP condition *BREAK-ON-SIGNALS*)
    if (!nullp(value1))
      # Break-Driver aufrufen: (funcall *break-driver* t condition t)
      { # *BREAK-DRIVER* kann hier als /= NIL angenommen werden.
        pushSTACK(T); pushSTACK(STACK_(0+1)); pushSTACK(T);
        funcall(Symbol_value(S(break_driver)),3);
      }
   {var object condition = popSTACK(); # condition zurück
    invoke_handlers(condition); # Handler aufrufen
    value1 = NIL; mv_count=1; # Wert NIL
  }}

# Fehlermeldung, wenn ein Objekt keine Liste ist.
# fehler_list(obj);
# > obj: Nicht-Liste
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_list, (object obj));
  global void fehler_list(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(list)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a list")
            );
    }

# Fehlermeldung, wenn ein Objekt keine echte Liste ist.
# fehler_proper_list(obj);
# > obj: Ende der Liste, Nicht-Liste
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_proper_list, (object obj));
  global void fehler_proper_list(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(list)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: A true list must not end with ~")
            );
    }

# Fehlermeldung, wenn ein Objekt kein Symbol ist.
# fehler_kein_symbol(caller,obj);
# > caller: Aufrufer (ein Symbol)
# > obj: Nicht-Symbol
  nonreturning_function(global, fehler_kein_symbol, (object caller, object obj));
  global void fehler_kein_symbol(caller,obj)
    var object caller;
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(symbol)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(caller);
      fehler(type_error,
             GETTEXT("~: ~ is not a symbol")
            );
    }

# Fehlermeldung, wenn ein Objekt kein Symbol ist.
# fehler_symbol(obj);
# > subr_self: Aufrufer (ein SUBR oder FSUBR)
# > obj: Nicht-Symbol
  nonreturning_function(global, fehler_symbol, (object obj));
  global void fehler_symbol(obj)
    var object obj;
    { var object aufrufer = subr_self;
      aufrufer = (subrp(aufrufer) ? TheSubr(aufrufer)->name : TheFsubr(aufrufer)->name);
      fehler_kein_symbol(aufrufer,obj);
    }

# Fehlermeldung, wenn ein Objekt kein Simple-Vector ist.
# fehler_kein_svector(caller,obj);
# > caller: Aufrufer (ein Symbol)
# > obj: Nicht-Svector
  nonreturning_function(global, fehler_kein_svector, (object caller, object obj));
  global void fehler_kein_svector(caller,obj)
    var object caller;
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(simple_vector)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(caller);
      fehler(type_error,
             GETTEXT("~: ~ is not a simple-vector")
            );
    }

# Fehlermeldung, wenn ein Objekt kein Vektor ist.
# fehler_vector(obj);
# > subr_self: Aufrufer (ein SUBR)
# > obj: Nicht-Vektor
  nonreturning_function(global, fehler_vector, (object obj));
  global void fehler_vector(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(vector)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a vector")
            );
    }

# Fehlermeldung, falls ein Argument kein Fixnum >=0 ist:
# fehler_posfixnum(obj);
# > obj: Das fehlerhafte Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_posfixnum, (object obj));
  global void fehler_posfixnum(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_posfixnum)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ should be a nonnegative fixnum")
            );
    }

# Fehlermeldung, falls ein Argument kein Character ist:
# fehler_char(obj);
# > obj: Das fehlerhafte Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_char, (object obj));
  global void fehler_char(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(character)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ is not a character")
            );
    }

# Fehlermeldung, falls ein Argument kein String ist:
# > obj: Das fehlerhafte Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_string, (object obj));
  global void fehler_string(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(string)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ is not a string")
            );
    }

# Fehlermeldung, falls ein Argument kein Simple-String ist:
# > obj: Das fehlerhafte Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_sstring, (object obj));
  global void fehler_sstring(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(simple_string)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ is not a simple string")
            );
    }

#ifndef TYPECODES
# Fehlermeldung, falls ein Simple-String immutable ist:
# fehler_sstring_immutable(obj);
# > obj: der String
  nonreturning_function(global, fehler_sstring_immutable, (object obj));
  global void fehler_sstring_immutable(obj)
    var object obj;
    { pushSTACK(obj);
      fehler(error,
             GETTEXT("Attempt to modify a read-only string: ~")
            );
    }
#endif

# Error message, if an argument is not of type (OR STRING INTEGER).
# fehler_string_integer(obj);
# > subr_self: caller (a SUBR)
  nonreturning_function(global, fehler_string_integer, (object obj));
  global void fehler_string_integer(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_string_integer)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ is neither a string nor an integer")
            );
    }

# Fehlermeldung, wenn ein Argument kein Stream ist:
# fehler_stream(obj);
# > obj: Das fehlerhafte Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_stream, (object obj));
  global void fehler_stream(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ should be a stream")
            );
    }

# Fehlermeldung, wenn ein Argument kein Stream vom geforderten Stream-Typ ist:
# fehler_streamtype(obj,type);
# > obj: Das fehlerhafte Argument
# > type: geforderten Stream-Typ
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_streamtype, (object obj, object type));
  global void fehler_streamtype(obj,type)
    var object obj;
    var object type;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(type); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(type); pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ should be a stream of type ~")
            );
    }

# Fehlermeldung, wenn ein Argument ein Lambda-Ausdruck statt einer Funktion ist:
# fehler_lambda_expression(obj);
# obj: Das fehlerhafte Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_lambda_expression, (object obj));
  global void fehler_lambda_expression(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(function)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ is not a function." NLstring "To get a function in the current environment, write (FUNCTION ...)." NLstring "To get a function in the global environment, write (COERCE '... 'FUNCTION).")
            );
    }

#ifdef HAVE_FFI

# Fehler, wenn Argument kein Integer vom Typ `uint8' ist.
# fehler_uint8(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_uint8, (object obj));
  global void fehler_uint8(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_uint8)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not an 8-bit number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `sint8' ist.
# fehler_sint8(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_sint8, (object obj));
  global void fehler_sint8(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_sint8)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not an 8-bit number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `uint16' ist.
# fehler_uint16(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_uint16, (object obj));
  global void fehler_uint16(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_uint16)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a 16-bit number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `sint16' ist.
# fehler_sint16(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_sint16, (object obj));
  global void fehler_sint16(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_sint16)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a 16-bit number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `uint32' ist.
# fehler_uint32(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_uint32, (object obj));
  global void fehler_uint32(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_uint32)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not an 32-bit number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `sint32' ist.
# fehler_sint32(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_sint32, (object obj));
  global void fehler_sint32(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_sint32)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not an 32-bit number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `uint64' ist.
# fehler_uint64(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_uint64, (object obj));
  global void fehler_uint64(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_uint64)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not an 64-bit number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `sint64' ist.
# fehler_sint64(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_sint64, (object obj));
  global void fehler_sint64(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_sint64)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not an 64-bit number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `uint' ist.
# fehler_uint(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_uint, (object obj));
  global void fehler_uint(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      #if (int_bitsize==16)
      pushSTACK(O(type_uint16)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      #else # (int_bitsize==32)
      pushSTACK(O(type_uint32)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      #endif
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not an `unsigned int' number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `sint' ist.
# fehler_sint(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_sint, (object obj));
  global void fehler_sint(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      #if (int_bitsize==16)
      pushSTACK(O(type_sint16)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      #else # (int_bitsize==32)
      pushSTACK(O(type_sint32)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      #endif
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not an `int' number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `ulong' ist.
# fehler_ulong(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_ulong, (object obj));
  global void fehler_ulong(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      #if (long_bitsize==32)
      pushSTACK(O(type_uint32)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      #else # (long_bitsize==64)
      pushSTACK(O(type_uint64)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      #endif
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a `unsigned long' number")
            );
    }

# Fehler, wenn Argument kein Integer vom Typ `slong' ist.
# fehler_slong(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_slong, (object obj));
  global void fehler_slong(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      #if (long_bitsize==32)
      pushSTACK(O(type_sint32)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      #else # (long_bitsize==64)
      pushSTACK(O(type_sint64)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      #endif
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a `long' number")
            );
    }

# Fehler, wenn Argument kein Single-Float ist.
# fehler_ffloat(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_ffloat, (object obj));
  global void fehler_ffloat(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(single_float)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a single-float")
            );
    }

# Fehler, wenn Argument kein Double-Float ist.
# fehler_dfloat(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_dfloat, (object obj));
  global void fehler_dfloat(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(double_float)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a double-float")
            );
    }

#endif

