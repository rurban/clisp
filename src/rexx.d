# Rexx-Interface für CLISP
# Jörg Höhle 15.4.1997


#include "lispbibl.c"

#ifdef AMIGAOS

#include "amiga2.c"

# ARexx 'library base' pointer:
# (Muss global sichtbar sein und diesen Namen tragen, damit's der Linker findet!)
  global struct RxsLib * RexxSysBase = NULL;


#ifdef DEBUG_REXX
  #define debug_asciz_out  asciz_out
  #define debug_asciz_out_s  asciz_out_s
  #define debug_asciz_out_1  asciz_out_1
#else
  #define debug_asciz_out(x)
  #define debug_asciz_out_s(x,y)
  #define debug_asciz_out_1(x,y)
#endif


# Fehlermeldung wenn kein Rexx möglich
# fehler_norexx();
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_norexx, (void));
  local void fehler_norexx()
    { fehler(error,
             GETTEXT("Communication with ARexx isn't possible.")
            );
    }

# Speicher freigeben, der wegen Fehler nicht freigegeben wurde.
  local UBYTE* rexxLostArgstr = NULL;
  local void handle_lost_argstr (void);
  local void handle_lost_argstr()
    { if (rexxLostArgstr)
        { begin_system_call();
          DeleteArgstring(rexxLostArgstr);
          end_system_call();
          rexxLostArgstr = NULL;
    }   }

# Die Anzahl der auf Antwort (durch andere Prozesse) wartenden Messages:
  local uintC rexxNeededReplies = 0;

# O(rexx_inmsg_list) ist eine Liste von Foreigns, die jeweils die ein-
# gegangenen und auf Antwort (durch CLISP) wartenden Messages repräsentieren.

# Sucht eine gegebene Message in O(rexx_inmsg_list):
  local object find_inmsg (FOREIGN pointer);
  local object find_inmsg(pointer)
    var FOREIGN pointer;
    { var object current;
      for (current = O(rexx_inmsg_list); consp(current); current = Cdr(current))
        { if (TheFpointer(Car(current))->fp_pointer == pointer) { return Car(current); } }
      return NIL;
    }

# Der Message Port, auf dem wir arbeiten:
  local struct MsgPort * rexxPort = NULL;
# Sein Name:
  local UBYTE rexxPortName[] = {'C','L','I','S','P','1','\0','\0'};
# Position der Ziffer darin:
  #define NRPOSITION 5
# Default-Extension für ARexx-Kommandofiles:
  local UBYTE rexxExtension[] = "cl";
# Signalnummer, mit der wir auf Ereignisse an diesem Port warten können:
  local ULONG rexxPortBit = 0UL;

LISPFUN(rexx_put,1,0,norest,key,5,\
        (kw(result),kw(string),kw(token),kw(host),kw(io)) )
  { # Stackaufbau: string/array, resultp, stringp, tokenp, host, iop.
    # > string/array: String für Kommando inklusive Argumente oder
    #                 Array von Strings für Funktion und Argumente
    # > resultp: Flag: Antwort merken?
    # > stringp: Flag: ARexx Argument als Befehle oder
    #                  erstes Token als Dateiname verstehen?
    # > tokenp: Flag: Soll ARexx Tokens erzeugen?
    # > host: ARexx Portname, bzw. NIL ("REXX") oder T ("AREXX", asynchrone
    #         Bearbeitung)
    # > iop: Flag: E/A Kanäle übernehmen?
    # Es sind nicht alle Kombinationen sinvoll.
    var uintL fargs; # 1 + Zahl Funktionsargumente
    var boolean functionp; # Funktions- statt Kommandoaufruf
    var UBYTE* portname;
    if (rexxPort == NULL) { fehler_norexx(); }
    # vorsorglich ein Foreign allozieren:
    pushSTACK(allocate_fpointer(NULL));
   {var object* fargs_pointer = STACK; # Pointer unter alle Argumente inkl. Foreign.
    # Darunter werden bei Funktion alle Strings aus dem Array abgelegt.
    # Erstes Argument verarbeiten:
    if (stringp(STACK_(5+1)))
      { # String
        functionp = FALSE;
        STACK_(5+1) = coerce_ss(STACK_(5+1));
      }
      else
      { functionp = TRUE;
        # sollte (Simple-)Vector sein:
        # evtl.: STACK_(5+1) = coerce_sequence(STACK_(5+1),S(simple_vector));
        if (!simple_vector_p(STACK_(5+1)))
          { pushSTACK(STACK_(5+1)); # Wert für Slot DATUM von TYPE-ERROR
            pushSTACK(S(simple_vector)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
            pushSTACK(STACK_1);
            fehler(type_error,
                   GETTEXT("~ must be a string for commands or a vector of strings for a function")
                  );
          }
        fargs = Svector_length(STACK_(5+1));
        if (!( /* fargs > 0 && */ fargs-1 <= MAXRMARG))
          { pushSTACK(STACK_(5+1));
            pushSTACK(fixnum(MAXRMARG));
            pushSTACK(S(rexx_put));
            fehler(error,
                   GETTEXT("~: an ARexx function must have 0 to ~ arguments: ~")
                  );
          }
        # Alle Argumentstrings aus dem Vektor auf dem Stack ablegen:
       {var object* vptr = &STACK_(5+1);
        var uintL index;
        for (index = 0; index < fargs; index++)
          { var object arg = TheSvector(*vptr)->data[index];
            if (!stringp(arg))
              { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
                pushSTACK(S(string)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
                pushSTACK(arg);
                pushSTACK(S(rexx_put));
                fehler(type_error,
                       GETTEXT("~: must be a string for ARexx: ~")
                      );
              }
            # Argument in Simple-String umwandeln:
            pushSTACK(coerce_ss(arg));
          }
      }}
    # :HOST-Argument umsetzen:
    { var object host = Before(fargs_pointer STACKop (1+1));
      if (eq(host,unbound) || nullp(host))
        { portname = RXSDIR; }
      elif (eq(host,T))
        { portname = RXADIR; }
      elif (stringp(host))
        { portname = TheAsciz(string_to_asciz(host,O(misc_encoding))); } # Ab hier für eine Weile keine GC mehr!
      else
        { pushSTACK(host); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_rexx_host)); # (OR STRING BOOLEAN), Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(host);
          pushSTACK(S(Khost));
          fehler(type_error,
                 GETTEXT("Only NIL, T and strings are accepted for ~ : ~")
                );
    }   }
    # Stackaufbau: ... string/vector ..(5 keyword-args).. foreign ..(fargs).. .
    # Ab hier für eine Weile keine GC mehr
    { var struct RexxMsg * rexxmsg;
      debug_asciz_out("%REXX-PUT: ");
      begin_system_call();
      rexxmsg = CreateRexxMsg(rexxPort,rexxExtension,rexxPortName);
      end_system_call();
      if (!(rexxmsg == NULL))
        # vorerst erfolgreich
        { var boolean success;
          if (functionp)
            { # ARexx Funktionsaufruf
              debug_asciz_out("function ");
              # Argumente einfüllen:
              { var uintL i;
                var object* argptr = fargs_pointer;
                success = TRUE;
                with_sstring(NEXT(argptr),O(misc_encoding),asciz,len,
                  { begin_system_call();
                    for (i=0; i<fargs; i++)
                      { var object s = NEXT(argptr);
                        if ((rexxmsg->rm_Args[i] = CreateArgstring(asciz,len)) == NULL)
                          { if (i>0) ClearRexxMsg(rexxmsg,i);
                            success = FALSE;
                            break;
                      }   }
                    end_system_call();
                  });
              }
              setSTACK(STACK = fargs_pointer); # Stack aufräumen
            }
            else
            { # ARexx Kommando
              debug_asciz_out("command ");
              with_sstring(STACK_(5+1),O(misc_encoding),asciz,len,
                { begin_system_call();
                  if (rexxmsg->rm_Args[0] = CreateArgstring(asciz,len))
                    { success = TRUE; }
                    else
                    { success = FALSE; }
                  end_system_call();
                });
            }
          # Stackaufbau: ... string/vector ..(5 keyword-args).. foreign.
          if (success)
            # vorerst immer noch erfolgreich
            { rexxmsg->rm_Action = (functionp ? (RXFUNC | (fargs-1)) : RXCOMM);
              # Keyword-Argumente verarbeiten:
              #define is_set(obj)  (!(eq(obj,unbound) || nullp(obj)))
              # :RESULT-Argument:
              if (is_set(STACK_(4+1))) { rexxmsg->rm_Action |= RXFF_RESULT; }
              # :STRING-Argument:
              if (is_set(STACK_(3+1))) { rexxmsg->rm_Action |= RXFF_STRING; }
              # :TOKEN-Argument:
              if (is_set(STACK_(2+1))) { rexxmsg->rm_Action |= RXFF_TOKEN; }
              # :IO-Argument:
              if (!is_set(STACK_(0+1))) { rexxmsg->rm_Action |= RXFF_NOIO; }
              rexxmsg->rm_Node.mn_Node.ln_Name = RXSDIR;
              { var struct MsgPort* arexxport;
                begin_system_call();
                Forbid();
                arexxport = FindPort(portname);
                if (!(arexxport==NULL))
                  # Message abschicken:
                  { PutMsg(arexxport,(struct Message *)rexxmsg); }
                  else
                  { success = FALSE; }
                Permit();
                end_system_call();
              }
              #undef is_set
              if (success)
                # erfolgreich -> mitzählen:
                { rexxNeededReplies++;
                  TheFpointer(STACK_0)->fp_pointer = rexxmsg;
                  debug_asciz_out_1("%x",rexxmsg);
                }
                else
                # nicht erfolgreich -> aufräumen:
                { begin_system_call();
                  if (functionp)
                    { ClearRexxMsg(rexxmsg,fargs); }
                  else
                    { DeleteArgstring(rexxmsg->rm_Args[0]); }
                  end_system_call();
            }   }
          if (success)
            { value1 = STACK_0; } # Wert ist das Foreign zu rexxmsg
            else
            # Nachricht konnte nicht erfolgreich abgeschickt werden, also löschen
            { begin_system_call();
              DeleteRexxMsg(rexxmsg);
              end_system_call();
              value1 = NIL;
        }   }
        else
        { setSTACK(STACK = fargs_pointer); # Stack aufräumen
          value1 = NIL;
        }
      debug_asciz_out(NLstring);
    }
    mv_count=1; skipSTACK(1+5+1);
  }}

# Warten, bis am Port eine Message eintrifft oder Ctrl-C.
# Ergebnis ist ein Flag, das angibt, ob eine Message eintraf.
  local boolean rexx_wait (void);
  local boolean rexx_wait()
    { start:
      begin_system_call();
     {var LONG wait_erg = Wait(rexxPortBit | SIGBREAKF_CTRL_C);
      end_system_call();
      #if 0 # spätere asynchrone DOS-Packet-Bearbeitung ??
      if (wait_erg & ioPortBit)
        { flush_io_queue(); }
      #endif
      if (wait_erg & SIGBREAKF_CTRL_C)
        { # Bearbeitung einer evtl. Message verschieben:
          if (wait_erg & rexxPortBit)
            { begin_system_call(); SetSignal(wait_erg,rexxPortBit); end_system_call(); }
          # Ctrl-C behandeln:
          pushSTACK(S(rexx_wait_input)); tast_break();
          goto start;
        }
        else
        { if (wait_erg & rexxPortBit)
            return TRUE;
            else
            return FALSE; # eigentlich nicht möglich
        }
    }}

# (SYSTEM::REXX-WAIT-INPUT) wartet bis am AREXX-Port etwas anliegt,
# und liefert dann T.
LISPFUNN(rexx_wait_input,0)
  { if (!(rexxPort == NULL))
      { if (rexx_wait())
          { value1 = T; mv_count=1; return; }
      }
    value1 = NIL; mv_count=1;
  }

# Flag, ob sich das ARexx-Interface gerade in der Endphase befindet und
# deswegen keine neuen Nachrichten entgegennimmt:
  local boolean rexxShutdown = TRUE;

# Empfängt ARexx Nachrichten.
# Liefert eine Liste (MsgId ...) oder T, wenn eine Nachricht empfangen wurde.
# Falls rexxShutdown gesetzt ist, werden keine neuen Nachrichten, nur noch
# Antworten, angenommen.
# Kann GC auslösen, falls nicht im rexxShutdown Modus.
  local object rexx_getmsg(void);
  local object rexx_getmsg()
    { if (rexxPort == NULL)
        { return NIL; }
        else
        { var struct RexxMsg * rexxmsg;
          handle_lost_argstr();
          # Resource-tracking für einkommende Nachrichten
          # (Benutzt eine globale Variable O(rexx_prefetch_inmsg),
          # um nicht jedes Mal ein neues Cons erzeugen zu müssen.)
          if (!rexxShutdown
              && matomp(O(rexx_prefetch_inmsg)))
            { pushSTACK(allocate_fpointer(NULL));
             {var object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
              O(rexx_prefetch_inmsg) = new_cons;
            }}
          # O(rexx_prefetch_inmsg) ist nun garantiert ein brauchbares Cons.
          # Bereich gegen GC geschützt.
          begin_system_call();
          rexxmsg = (struct RexxMsg *)GetMsg(rexxPort);
          end_system_call();
          if (rexxmsg == NULL) # keine Nachricht vorhanden?
            { return NIL; }
            else
            { debug_asciz_out_1("rexx_getmsg: %x",rexxmsg->rm_Action);
              if (rexxmsg->rm_Node.mn_Node.ln_Type == NT_REPLYMSG)
                # Antwort auf eine von uns geschickte Message
                { var LONG result1 = rexxmsg->rm_Result1;
                  begin_system_call();
                  if (rexxmsg->rm_Action & RXCOMM)
                    { DeleteArgstring(rexxmsg->rm_Args[0]); }
                    else
                    { ClearRexxMsg(rexxmsg,1+(rexxmsg->rm_Action & RXARGMASK)); }
                  if ((rexxmsg->rm_Action & RXFF_RESULT)
                      && (rexxmsg->rm_Result1 == 0)
                      && rexxmsg->rm_Result2
                      && !rexxShutdown
                     )
                    { # DeleteArgstring(rexxmsg->rm_Result2); kommt später
                      rexxLostArgstr = (UBYTE*)(rexxmsg->rm_Result2);
                    }
                  DeleteRexxMsg(rexxmsg);
                  end_system_call();
                  rexxNeededReplies--;
                  debug_asciz_out(" a reply ");
                  # ab hier GC wieder möglich
                  if (rexxShutdown)
                    { handle_lost_argstr(); return T; }
                    else
                    # Ergebnis ist eine 2- oder 3-elementige Liste (Msg-ID RC [RESULT]),
                    { pushSTACK(allocate_fpointer(rexxmsg));
                      pushSTACK(L_to_I(result1));
                      if (rexxLostArgstr)
                        { pushSTACK(n_char_to_string((char*)rexxLostArgstr,LengthArgstring(rexxLostArgstr),O(misc_encoding)));
                          handle_lost_argstr();
                          return listof(3);
                        }
                        else
                        { /* handle_lost_argstr(); */ # hier unnötig
                          return listof(2);
                    }   }
                }
                else
                { rexxmsg->rm_Result2 = 0;
                  debug_asciz_out_s(" incoming is " NLstring "%s",rexxmsg->rm_Args[0]);
                  # Eingehender Befehl
                  if (rexxShutdown)
                    { # Schluss, nichts läuft mehr
                      rexxmsg->rm_Result1 = RXERRORIMGONE;
                      begin_system_call();
                      ReplyMsg((struct Message *)rexxmsg);
                      end_system_call();
                      return T;
                    }
                    else
                    { var object new_cons = O(rexx_prefetch_inmsg);
                      # Resource-tracking, bis dahin keine GC.
                      TheFpointer(Car(new_cons))->fp_pointer = rexxmsg;
                      Cdr(new_cons) = O(rexx_inmsg_list);
                      O(rexx_inmsg_list) = new_cons;
                      O(rexx_prefetch_inmsg) = NIL;
                      # Resource-tracking beendet, ab hier wieder GC möglich
                      # Ergebnis ist 2/3-elementige Liste (Msg-ID "Msg-string" [:RESULT])
                      pushSTACK(Car(new_cons));
                      pushSTACK(n_char_to_string((char*)rexxmsg->rm_Args[0],LengthArgstring(rexxmsg->rm_Args[0]),O(misc_encoding)));
                      if (rexxmsg->rm_Action & RXFF_RESULT)
                        # Client is actually interested in RESULT string
                        { pushSTACK(S(Kresult));
                          return listof(3);
                        }
                        else
                        { return listof(2); }
                    }
                }
            }
        }
    }

# (SYSTEM::%REXX-GET) empfängt eine Nachricht und liefert sie im
# Format (MsgId ...). Ergebnis NIL falls keine Nachricht vorliegt.
LISPFUNN(rexx_get,0)
  { if (rexxPort == NULL) { fehler_norexx(); }
    value1 = rexx_getmsg(); mv_count=1;
  }

# Antwortet auf eine eingegangene Nachricht.
# > foreign: Foreign mit der Message-Adresse
# > rc, result,result_length: Return-Code und Ergebnis-String
  local void rexx_replymsg (object foreign, LONG rc, UBYTE* result, ULONG result_length);
  local void rexx_replymsg(foreign,rc,result,result_length)
    var object foreign;
    var LONG rc;
    var UBYTE* result;
    var ULONG result_length;
    { var struct RexxMsg* rexxmsg = TheFpointer(foreign)->fp_pointer;
      debug_asciz_out_1("rexx_replymsg: %x",rexxmsg);
      rexxmsg->rm_Result1 = rc;
      begin_system_call();
      rexxmsg->rm_Result2 = (ULONG)
        (((rc == 0) && (rexxmsg->rm_Action & RXFF_RESULT))
         ? CreateArgstring(result,result_length)
         : NULL
        );
      ReplyMsg((struct Message *)rexxmsg);
      end_system_call();
      # Die Message foreign ist nun beantwortet.
      O(rexx_inmsg_list) = deleteq(O(rexx_inmsg_list),foreign);
      mark_fp_invalid(TheFpointer(foreign)); # prohibit further use
      debug_asciz_out(NLstring);
    }

# (SYS::%REXX-REPLY message-id return-code return-string)
# antwortet auf eine Message.
LISPFUNN(rexx_reply,3)
  { # Stackaufbau: ..., message-id, return-code, return-string.
    if (rexxPort == NULL) { fehler_norexx(); }
    # Argumente überprüfen:
    # return-code sollte ein Fixnum sein:
    if (!fixnump(STACK_1))
      { pushSTACK(STACK_1); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(fixnum)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(STACK_(1+2));
        pushSTACK(S(rexx_reply));
        fehler(type_error,
               GETTEXT("~: Not a Fixnum: ~")
              );
      }
    # return-string sollte ein String oder NIL sein.
    # message-id sollte ein Foreign sein:
   {var object foreign;
    if (!(fpointerp(STACK_2) && !nullp(foreign = find_inmsg(TheFpointer(STACK_2)->fp_pointer))))
      { pushSTACK(STACK_2);
        pushSTACK(S(rexx_reply));
        fehler(error,
               GETTEXT("~: Not an incoming Rexx message: ~")
              );
      }
    # Beantworten:
    { var object retcode = STACK_1;
      var LONG result1 = fixnum_to_L(retcode);
      var object retstring = STACK_0;
      if (stringp(retstring))
        { with_string(retstring,O(misc_encoding),asciz,len,
            { rexx_replymsg(foreign,result1,asciz,len); }
        }
        else
        { rexx_replymsg(foreign,result1,NULL,0); }
    }
    skipSTACK(3);
    value1 = NIL; mv_count=0;
  }}

# Initialisiert das REXX-Interface.
# < ergebnis: Flag, ob erfolgreich initialisiert.
# Kann mehrfach aufgerufen werden.
  global boolean init_rexx(void)
    { if (rexxPort == NULL) # noch was zu tun?
        { if (RexxSysBase == NULL)
            { begin_system_call();
              RexxSysBase = (struct RxsLib *) OpenLibrary(RXSNAME,0L);
              end_system_call();
              if (RexxSysBase == NULL) { return FALSE; }
            }
         {var uintC nr = 1; # wir probieren verschiedene Ports
          loop
            { if (!(rexxPort == NULL)) break;
             {var boolean existent;
              rexxPortName[NRPOSITION] = '0' + nr;
              { begin_system_call();
                Forbid();
                if (FindPort(rexxPortName) == NULL)
                  # Port existiert noch nicht, wir machen einen (öffentlichen):
                  { rexxPort = CreatePort(rexxPortName,0L);
                    existent = FALSE;
                  }
                  else
                  { existent = TRUE; }
                Permit();
                end_system_call();
              }
              if (!existent)
                # Wir haben's wenigstens probiert...
                { if (rexxPort == NULL) { return FALSE; }
                  rexxPortBit = bit(rexxPort->mp_SigBit);
                  rexxNeededReplies = 0;
                  rexxShutdown = FALSE;
                  break;
                }
              # Wir versuchen es mit einem anderem Namen erneut.
              nr++; if (nr==10) { return FALSE; }
            }}
        }}
      return TRUE;
    }

# Schließt das REXX-Interface.
# Kann nur einmal aufgerufen werden.
  global void close_rexx(void)
    { rexxShutdown = TRUE;
      debug_asciz_out_1("close_rexx: %d messages waiting." NLstring,rexxNeededReplies);
      if (!(rexxPort == NULL))
        { # Port unbekannt machen (abmelden):
          begin_system_call();
          RemPort(rexxPort);
          end_system_call();
          handle_lost_argstr();
          # Ausstehende Nachrichten mit Fehler zurückschicken:
          while (mconsp(O(rexx_inmsg_list)))
            { rexx_replymsg(Car(O(rexx_inmsg_list)),RXERRORIMGONE,NULL,0L); }
          # Eingegangene Nachrichten mit Fehler zurückschicken:
          loop
            { until (nullp(rexx_getmsg())) { /* loop until empty */ }
              if (rexxNeededReplies == 0) break;
              begin_system_call();
              Wait(rexxPortBit);
              end_system_call();
              debug_asciz_out("Looping" NLstring);
            }
          begin_system_call();
          rexxPort->mp_Node.ln_Name = NULL;
          DeletePort(rexxPort);
          end_system_call();
          rexxPort = NULL;
          rexxPortBit = 0;
        }
      if (!(RexxSysBase == NULL))
        { begin_system_call();
          CloseLibrary((struct Library *)RexxSysBase);
          end_system_call();
          RexxSysBase = NULL;
        }
    }

#endif # AMIGAOS

