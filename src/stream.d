# Streams für CLISP
# Bruno Haible 6.12.1997
# Generic Streams: Marcus Daniels 8.4.1994

#include "lispbibl.c"
#include "arilev0.c" # für R_sign

#ifdef GNU_READLINE
  #define READLINE_LIBRARY # Hinweis, wo die Include-Files gesucht werden müssen
  #include "readline.h"
  #include "history.h"
  #undef READLINE_LIBRARY
  #ifdef STDC_HEADERS
    #include <string.h>  # deklariert strcpy()
  #endif
#endif


# Nochmals zum Aufbau von Streams:
# strmflags = Flags
  # Bits in den Flags:
  #                             0,1 # gesetzt, falls Integer-Stream
  # define strmflags_reval_bit_B 2  # gesetzt, falls Read-Eval erlaubt ist
  #define strmflags_unread_bit_B 3  # gesetzt, während strm_rd_ch_last zurück ist
  #define strmflags_rd_by_bit_B  4  # gesetzt, falls READ-BYTE möglich ist
  #define strmflags_wr_by_bit_B  5  # gesetzt, falls WRITE-BYTE möglich ist
  # define strmflags_rd_ch_bit_B 6  # gesetzt, falls READ-CHAR möglich ist
  # define strmflags_wr_ch_bit_B 7  # gesetzt, falls WRITE-CHAR möglich ist
  # Bitmasken in den Flags:
  #define strmflags_i_B      (bit(1)|bit(0))  # benutzt bei Integer-Streams
  #define strmflags_ia_B     (       bit(0))  # Integer-Stream der Art a
  #define strmflags_ib_B     (bit(1)       )  # Integer-Stream der Art b
  #define strmflags_ic_B     (bit(1)|bit(0))  # Integer-Stream der Art c
  #define strmflags_unread_B bit(strmflags_unread_bit_B)
  #define strmflags_reval_B  bit(strmflags_reval_bit_B)
  #define strmflags_rd_by_B  bit(strmflags_rd_by_bit_B)
  #define strmflags_wr_by_B  bit(strmflags_wr_by_bit_B)
  # define strmflags_rd_ch_B bit(strmflags_rd_ch_bit_B)
  # define strmflags_wr_ch_B bit(strmflags_wr_ch_bit_B)
  #define strmflags_rd_B  (strmflags_rd_by_B | strmflags_rd_ch_B)
  #define strmflags_wr_B  (strmflags_wr_by_B | strmflags_wr_ch_B)
  #define strmflags_by_B  (strmflags_rd_by_B | strmflags_wr_by_B)
  #define strmflags_ch_B  (strmflags_rd_ch_B | strmflags_wr_ch_B)
  # strmflags_open_B: die 4 oberen Bits
# strmtype = Nähere Typinfo. Siehe LISPBIBL.D.

# einzelne Komponenten:
  # strm_rd_by       Pseudofunktion für READ-BYTE
  # strm_wr_by       Pseudofunktion für WRITE-BYTE
  # strm_rd_ch       Pseudofunktion für READ-CHAR
  # strm_pk_ch       Pseudofunktion für PEEK-CHAR
  # strm_rd_ch_last  letztes von READ-CHAR gelesenes Zeichen
  #                  (NIL falls noch keines gelesen), nach UNREAD-CHAR
  #                  ist zusätzlich strmflags_unread_bit_B gesetzt.
  # strm_wr_ch       Pseudofunktion für WRITE-CHAR
  # strm_wr_ch_lpos  Line-Position in der Zeile nach dem letzten WRITE-CHAR
  # strm_wr_ss       Pseudofunktion für WRITE-SIMPLE-STRING
# weitere (typspezifische) Komponenten:
  # siehe in LISPBIBL.D und bei den einzelnen Stream-Typen.


# ==============================================================================
#                           S T R E A M S

# Da MAKE-TWO-WAY-STREAM eventuell einen Stream liefern kann, der z.B.
# Character-Input und Byte-Output ist, und damit insbesondere alle
# READ-/WRITE-Operationen effizient laufen, werden Streams folgendermaßen
# aufgebaut:
#    - Typ des Streams,
#    - Komponenten für READ-BYTE,
#    - Komponenten für WRITE-BYTE,
#    - Komponenten für READ-CHAR,
#    - Komponenten für WRITE-CHAR,
#    - vom Typ des Streams abhängige Komponenten.

# Feste Komponenten
# -----------------
# für READ-BYTE:
#     RD_BY          Pseudofunktion zum Lesen eines Bytes
# für WRITE-BYTE:
#     WR_BY          Pseudofunktion zum Schreiben eines Bytes
# für READ-CHAR:
#     RD_CH          Pseudofunktion zum Lesen eines Characters
#     RD_CH_LAST     letztes gelesenes Zeichen und Flag
#                    (NIL zu Beginn, eof_value nach End-of-File,
#                    sonst: letztes gelesenes Zeichen)
# für WRITE-CHAR:
#     WR_CH          Pseudofunktion zum Schreiben eines Characters
#     WR_CH_LPOS     Position in der Zeile (Fixnum >=0) (für FORMAT ~T)

# Pseudofunktionen sind Adressen von C-Funktionen, die direkt angesprungen
# werden können, mit dem Stream als erstem Argument, und ein Objekt
# als Ergebnis liefern.
  #ifdef TYPECODES
    #define P(fun)  (type_constpointer_object(machine_type,(Pseudofun)&(fun)))
  #else
    #define P(fun)  make_machine_code((Pseudofun)&(fun))
  #endif

# Spezifikation der vier Typen von Pseudofunktionen:
  #
  # Spezifikation für READ-BYTE - Pseudofunktion:
  # fun(stream)
  # > stream: Stream
  # < ergebnis: gelesener Integer (eof_value bei EOF)
  # kann GC auslösen
    typedef object (* rd_by_Pseudofun) (object stream);
  #
  # Spezifikation für WRITE-BYTE - Pseudofunktion:
  # fun(stream,obj)
  # > stream: Stream
  # > obj: auszugebender Integer
  # kann GC auslösen
    typedef void (* wr_by_Pseudofun) (object stream, object obj);
  #
  # Spezifikation für READ-CHAR - Pseudofunktion:
  # fun(&stream)
  # > stream: Stream
  # < stream: Stream
  # < ergebnis: gelesenes Character (eof_value bei EOF)
  # kann GC auslösen
    typedef object (* rd_ch_Pseudofun) (const object* stream_);
  #
  # Spezifikation für PEEK-CHAR - Pseudofunktion:
  # fun(&stream)
  # Wie READ-CHAR mit nachfolgendem UNREAD-CHAR, nur dass Seiteneffekte bis
  # zum nächsten wirklichen READ-CHAR hinausgezögert werden (soweit möglich).
  # > stream: Stream (mit strmflags_unread_bit_B gelöscht)
  # < stream: Stream
  # < ergebnis: gelesenes Character (eof_value bei EOF)
  # kann GC auslösen
    typedef object (* pk_ch_Pseudofun) (const object* stream_);
  #
  # Spezifikation für WRITE-CHAR - Pseudofunktion:
  # fun(&stream,obj)
  # > stream: Stream
  # < stream: Stream
  # > obj: auszugebendes Character
  # kann GC auslösen
    typedef void (* wr_ch_Pseudofun) (const object* stream_, object obj);
  #
  # Spezifikation für WRITE-SIMPLE-STRING - Pseudofunktion:
  # fun(&stream,string,start,len)
  # > string: Simple-String
  # > start: Startindex
  # > len: Anzahl der auszugebenden Zeichen
  # > stream: Stream
  # < stream: Stream
  # kann GC auslösen
    typedef void (* wr_ss_Pseudofun) (const object* stream_, object string, uintL start, uintL len);

# Pseudofunktionen aus einem Stream herausgreifen:
  #define rd_by(strm)  (*(rd_by_Pseudofun)(ThePseudofun(TheStream(strm)->strm_rd_by)))
  #define wr_by(strm)  (*(wr_by_Pseudofun)(ThePseudofun(TheStream(strm)->strm_wr_by)))
  #define rd_ch(strm)  (*(rd_ch_Pseudofun)(ThePseudofun(TheStream(strm)->strm_rd_ch)))
  #define pk_ch(strm)  (*(pk_ch_Pseudofun)(ThePseudofun(TheStream(strm)->strm_pk_ch)))
  #define wr_ch(strm)  (*(wr_ch_Pseudofun)(ThePseudofun(TheStream(strm)->strm_wr_ch)))
  #define wr_ss(strm)  (*(wr_ss_Pseudofun)(ThePseudofun(TheStream(strm)->strm_wr_ss)))

#  Mögliche Typen von Streams                 Zusatzkomponenten
#  --------------------------                 -----------------
#  1. Keyboard-Stream
#  2. Interaktiver Terminalstream             Eingabebuffer, Zeichenzähler
#  3. File-Stream für Characters              Handle, Pathname, File-Position,
#     (Input, Output, I/O, Closed=Probe)      Buffer
#  4. File-Stream für Unsigned-Bytes          Handle, Pathname, File-Position,
#     (Input, Output, I/O, Closed=Probe)      Buffer, Bit-Buffer
#  5. File-Stream für Signed-Bytes            Handle, Pathname, File-Position,
#     (Input, Output, I/O, Closed=Probe)      Buffer, Bit-Buffer
#  6. Synonym-Stream                          Symbol
#  7. Broadcast-(Output-)Stream               Liste von Streams
#  8. Concatenated-(Input-)Stream             Liste von Streams
#  9. Two-Way-Stream                          Stream für Input, Stream für Output
# 10. Echo-Stream                             Stream für Input, Stream für Output
# 11. String-Input-Stream                     Gesamtstring, Zeichenzähler
# 12. String-Output-Stream                    Buffer (Semi-Simple-String)
# 13. String-Push-Stream                      String mit Fill-Pointer
# 14. Pretty-Printer-Hilfs-Stream             Liste von Buffers, Modus
# 15. Buffered-Input-Stream                   fun, mode, String, Zeichenzähler
# 16. Buffered-Output-Stream                  fun, Buffer (Semi-Simple-String)
# 17. Window-Stream                           ---
#ifdef PRINTER
# 18. Printer-Stream
#endif
#ifdef HANDLES
# 19. File-Handle-Stream                      Handle, Pathname
#endif
#ifdef PIPES
# 20. Pipe-Input-Stream                       Pid, Handle
# 21. Pipe-Output-Stream                      Pid, Handle
#endif
#ifdef X11SOCKETS
# 22. X11-Socket-Stream                       Info, Handle
#endif
#ifdef SOCKET_STREAMS
# 23. Socket-Stream                           Host, Port
#endif
#ifdef GENERIC_STREAMS
# 24. Generic-Stream                          Private Controller Object
#endif

# Zusätzlich wird (sicherheitshalber) eine Liste aller offenen File-Streams
# geführt.

# Fehlermeldung, wenn eine Stream-Operation auf einem Stream nicht erlaubt ist.
# fehler_illegal_streamop(caller,stream);
# > caller: Aufrufer (ein Symbol)
# > stream: Stream
  nonreturning_function(global, fehler_illegal_streamop, (object caller, object stream));
  global void fehler_illegal_streamop(caller,stream)
    var object caller;
    var object stream;
    { pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(stream);
      pushSTACK(caller);
      fehler(stream_error,
             DEUTSCH ? "~ auf ~ ist unzulässig." :
             ENGLISH ? "~ on ~ is illegal" :
             FRANCAIS ? "~ de/sur ~ est impossible." :
             ""
            );
    }

# Dummy-Pseudo-Funktionen, die Errors liefern:
  local object rd_by_dummy (object stream);
  local object rd_by_dummy(stream)
    var object stream;
    { fehler_illegal_streamop(S(read_byte),stream); }
  local void wr_by_dummy (object stream, object obj);
  local void wr_by_dummy(stream,obj)
    var object stream;
    var object obj;
    { fehler_illegal_streamop(S(write_byte),stream); }
  local object rd_ch_dummy (const object* stream_);
  local object rd_ch_dummy(stream_)
    var const object* stream_;
    { fehler_illegal_streamop(S(read_char),*stream_); }
  local object pk_ch_dummy (const object* stream_);
  local object pk_ch_dummy(stream_)
    var const object* stream_;
    { var object newch = rd_ch(*stream_)(stream_);
      TheStream(*stream_)->strm_rd_ch_last = newch;
      if (!eq(newch,eof_value))
        { TheStream(*stream_)->strmflags |= strmflags_unread_B; }
      return newch;
    }
  local void wr_ch_dummy (const object* stream_, object obj);
  local void wr_ch_dummy(stream_,obj)
    var const object* stream_;
    var object obj;
    { fehler_illegal_streamop(S(write_char),*stream_); }
  local void wr_ss_dummy (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_dummy(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { if (len==0) return;
     {var uintL index = start;
      pushSTACK(string); # Simple-String retten
      dotimespL(len,len,
        { write_schar(stream_,TheSstring(STACK_0)->data[index]);
          index++;
        });
      skipSTACK(1);
    }}
  # Dasselbe, wenn write_char auf diesem Stream keine GC auslösen kann:
  local void wr_ss_dummy_nogc (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_dummy_nogc(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { if (len==0) return;
     {var uintB* ptr = &TheSstring(string)->data[start];
      dotimespL(len,len, { write_schar(stream_,*ptr++); } );
    }}
  # Am Ende eines wr_ss die Line-Position aktualisieren:
  # wr_ss_lpos(stream,ptr,len);
  # > stream: Stream, nicht der Terminal-Stream
  # > ptr: Pointer ans Ende(!) der bereits auf den Stream ausgegebenen Zeichen
  # > len: Anzahl der Zeichen, >0
  # < ergebnis: TRUE, falls ein NL unter den Zeichen ist, FALSE sonst
  local boolean wr_ss_lpos (object stream, const uintB* ptr, uintL len);
  local boolean wr_ss_lpos(stream,ptr,len)
    var object stream;
    var const uintB* ptr;
    var uintL len;
    {
      #ifdef TERMINAL_USES_KEYBOARD
      if (TheStream(stream)->strmtype == strmtype_terminal)
        return FALSE; # Auf dem Atari machte dies wr_ch_terminal() selbst.
      #endif
      # Zähle die Zahl der Zeichen seit dem letzten NL:
     {var boolean result;
      var uintL pos = 0;
      var uintL count;
      dotimespL(count,len, { if (*--ptr == NL) goto found_NL; pos++; } );
      if (FALSE)
        found_NL: # pos Zeichen seit dem letzten NL
        { ptr++; len = pos; pos = 0; result = TRUE; }
        else /* pos==len */
        { pos = posfixnum_to_L(TheStream(stream)->strm_wr_ch_lpos); result = FALSE; }
      # Es gab len Zeichen ab ptr, pos ist die Position dort.
      #ifndef TERMINAL_USES_KEYBOARD
      if (TheStream(stream)->strmtype == strmtype_terminal)
        { dotimesL(count,len,
            { var uintB c = *ptr++;
              # Wie wirken sich die Steuerzeichen in der Position aus?
              if (graphic_char_p(c))
                # normales druckendes Zeichen -> Line Position incrementieren:
                { pos++; }
              elif (c == BS)
                # Backspace -> Line Position, wenn möglich, decrementieren:
                { if (pos > 0) { pos--; } }
            });
        }
        else
      #endif
        { pos += len; }
      TheStream(stream)->strm_wr_ch_lpos = fixnum(pos);
      return result;
    }}

# Liest ein Byte von einem Stream.
# read_byte(stream)
# > stream: Stream
# < ergebnis: gelesener Integer (eof_value bei EOF)
# kann GC auslösen
  global object read_byte (object stream);
  global object read_byte(stream)
    var object stream;
    { return rd_by(stream)(stream); }

# Schreibt ein Byte auf einen Stream.
# write_byte(stream,byte);
# > stream: Stream
# > byte: auszugebender Integer
# kann GC auslösen
  global void write_byte(object stream, object byte);
  global void write_byte(stream,byte)
    var object stream;
    var object byte;
    { wr_by(stream)(stream,byte); }

# Liest ein Character von einem Stream.
# read_char(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Character (eof_value bei EOF)
# kann GC auslösen
  global object read_char (const object* stream_);
  global object read_char(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      if (!(TheStream(stream)->strmflags & strmflags_unread_B)) # Char nach UNREAD ?
        # nein -> neues Zeichen holen:
        { var object newch = rd_ch(stream)(stream_);
          stream = *stream_;
          TheStream(stream)->strm_rd_ch_last = newch; # und abspeichern
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
          return newch;
        }
        else
        # ja -> Flagbit löschen und letztes Zeichen holen:
        { TheStream(stream)->strmflags &= ~strmflags_unread_B;
          return TheStream(stream)->strm_rd_ch_last;
    }   }

# Schiebt das letzte gelesene Character auf einen Stream zurück.
# unread_char(&stream,ch);
# > ch: letztes gelesenes Character
# > stream: Stream
# < stream: Stream
  global void unread_char (const object* stream_, object ch);
  global void unread_char(stream_,ch)
    var const object* stream_;
    var object ch;
    { var object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,ch)
          && !(TheStream(stream)->strmflags & strmflags_unread_B)
         )
        { TheStream(stream)->strmflags |= strmflags_unread_B; } # Flagbit setzen
        else
        { if (!nullp(TheStream(stream)->strm_rd_ch_last)
              && !(TheStream(stream)->strmflags & strmflags_unread_B)
             )
            { pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
              pushSTACK(ch);
              pushSTACK(stream);
              pushSTACK(S(unread_char));
              fehler(stream_error,
                     DEUTSCH ? "~: Das letzte von ~ gelesene Zeichen war nicht ~." :
                     ENGLISH ? "~: the last character read from ~ was not ~" :
                     FRANCAIS ? "~ : Le dernier caractère lu dans ~ n'était pas ~." :
                     ""
                    );
            }
            else
            { pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
              pushSTACK(S(read_char));
              pushSTACK(stream);
              pushSTACK(S(unread_char));
              fehler(stream_error,
                     DEUTSCH ? "~ von ~ ohne vorheriges ~." :
                     ENGLISH ? "~ from ~ without ~ before it" :
                     FRANCAIS ? "~ de ~ sans précédent ~." :
                     ""
                    );
            }
    }   }

# Liest ein Character von einem Stream, ohne es zu verbrauchen.
# peek_char(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Character (eof_value bei EOF)
# kann GC auslösen
  global object peek_char (const object* stream_);
  global object peek_char(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      if (!(TheStream(stream)->strmflags & strmflags_unread_B)) # Char nach UNREAD ?
        # nein -> neues Zeichen holen:
        { return pk_ch(stream)(stream_); }
        else
        # ja -> letztes Zeichen holen:
        { return TheStream(stream)->strm_rd_ch_last; }
    }

# Schreibt ein Character auf einen Stream.
# write_char(&stream,ch);
# > ch: auszugebendes Character
# > stream: Stream
# < stream: Stream
# kann GC auslösen
  global void write_char (const object* stream_, object ch);
  global void write_char(stream_,ch)
    var const object* stream_;
    var object ch;
    { var uintB c = char_code(ch);
      # Char schreiben:
      wr_ch(*stream_)(stream_,ch);
      # Line Position aktualisieren:
     {var object stream = *stream_;
      if (!(TheStream(stream)->strmtype == strmtype_terminal))
        # nicht der Terminal-Stream
        { if (c == NL)
            # Nach Newline: Line Position := 0
            { TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; }
            else
            # Line Position incrementieren:
            { TheStream(stream)->strm_wr_ch_lpos =
                fixnum_inc(TheStream(stream)->strm_wr_ch_lpos,1);
        }   }
        else
        # es ist der Terminal-Stream
        #ifdef TERMINAL_USES_KEYBOARD
        { ; } # Auf dem Atari machte dies wr_ch_terminal() selbst.
        #else
        # Wie wirken sich die Steuerzeichen in der Position aus?
        { if (graphic_char_p(c))
            # normales druckendes Zeichen -> Line Position incrementieren:
            { TheStream(stream)->strm_wr_ch_lpos =
                fixnum_inc(TheStream(stream)->strm_wr_ch_lpos,1);
            }
          elif (c == NL)
            # Newline -> Line Position := 0
            { TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; }
          elif (c == BS)
            # Backspace -> Line Position, wenn möglich, decrementieren:
            { if (!eq(TheStream(stream)->strm_wr_ch_lpos,Fixnum_0))
                { TheStream(stream)->strm_wr_ch_lpos =
                    fixnum_inc(TheStream(stream)->strm_wr_ch_lpos,-1);
            }   }
        }
        #endif
    }}

# UP: Füllt beim Schließen eines Streams die Dummy-Pseudofunktionen ein.
# close_dummys(stream);
# > stream: Stream
  local void close_dummys (object stream);
  local void close_dummys(stream)
    var object stream;
    { TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
      TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
      TheStream(stream)->strmflags &= ~(strmflags_open_B|strmflags_unread_B); # Fähigkeiten-Flags löschen
    }

# Liefert Fehlermeldung, wenn der Wert des Symbols sym kein Stream ist.
  nonreturning_function(local, fehler_value_stream, (object sym));
  # siehe unten

# UP: Liefert den Stream, der der Wert einer Variablen ist.
# var_stream(sym,strmflags)
# > sym: Variable (Symbol)
# > strmflags: Menge von Operationen, die auf dem Stream möglich sein sollen
# < ergebnis: Stream
  global object var_stream (object sym, uintB strmflags);
  global object var_stream(sym,strmflags)
    var object sym;
    var uintB strmflags;
    { var object result = Symbol_value(sym);
      var object stream;
      recurse:
        stream = Symbol_value(sym);
        if (!streamp(stream)) { fehler_value_stream(sym); }
        if (strmflags & ~ TheStream(stream)->strmflags) { fehler_value_stream(sym); }
        if (TheStream(stream)->strmtype == strmtype_synonym)
          { sym = TheStream(stream)->strm_synonym_symbol; goto recurse; }
      return result;
    }

# (SYSTEM::SYMBOL-STREAM symbol [direction])
# liefert den Stream, der der Wert des Symbols ist, und überprüft, ob es ein
# offener Stream der Richtung direction (:PROBE, :INPUT, :OUTPUT oder :IO) ist.
LISPFUN(symbol_stream,1,1,norest,nokey,0,NIL)
  { var object direction = popSTACK();
    var object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    value1 = var_stream(symbol,
                        eq(direction,S(Kinput)) ? strmflags_rd_ch_B : # :INPUT
                        eq(direction,S(Koutput)) ? strmflags_wr_ch_B : # :OUTPUT
                        eq(direction,S(Kio)) ? strmflags_rd_ch_B | strmflags_wr_ch_B : # :IO
                        0 # :PROBE oder nicht angegeben
                       );
    mv_count=1;
  }

#if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32_NATIVE)
# Fehler, wenn aus einem obskuren Grunde ein WRITE nicht gehen sollte:
  nonreturning_function(local, fehler_unwritable, (object caller, object stream));
  local void fehler_unwritable(caller,stream)
    var object caller;
    var object stream;
    { pushSTACK(stream); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(stream);
      pushSTACK(caller);
      fehler(file_error,
             DEUTSCH ? "~: Kann nichts auf ~ ausgeben." :
             ENGLISH ? "~: cannot output to ~" :
             FRANCAIS ? "~ : Ne peux rien écrire sur ~." :
             ""
            );
    }
#endif

# Fehler, wenn ein Objekt kein Character ist:
# fehler_wr_char(stream,obj);
  nonreturning_function(local, fehler_wr_char, (object stream, object obj));
  local void fehler_wr_char(stream,obj)
    var object stream;
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(character)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(stream);
      pushSTACK(obj);
      fehler(type_error,
             DEUTSCH ? "~ ist kein Character und kann daher nicht auf ~ ausgegeben werden." :
             ENGLISH ? "~ is not a character, cannot be output onto ~" :
             FRANCAIS ? "~, n'étant pas de type CHARACTER, ne peut pas être écrit dans ~." :
             ""
            );
    }

# Fehler, wenn ein Objekt kein Integer ist:
# fehler_wr_integer(stream,obj);
  nonreturning_function(local, fehler_wr_integer, (object stream, object obj));
  local void fehler_wr_integer(stream,obj)
    var object stream;
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(integer)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(stream);
      pushSTACK(obj);
      fehler(type_error,
             DEUTSCH ? "~ ist kein Integer und kann daher nicht auf ~ ausgegeben werden." :
             ENGLISH ? "~ is not an integer, cannot be output onto ~" :
             FRANCAIS ? "~, n'étant pas un entier, ne peut pas être écrit dans ~." :
             ""
            );
    }

# Fehler, wenn ein Integer nicht im passenden Bereich ist:
# fehler_bad_integer(stream,obj);
  nonreturning_function(local, fehler_bad_integer, (object stream, object obj));
  local void fehler_bad_integer(stream,obj)
    var object stream;
    var object obj;
    { pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(stream);
      pushSTACK(obj);
      fehler(stream_error,
             DEUTSCH ? "Integer ~ ist zu groß oder zu klein und kann daher nicht auf ~ ausgegeben werden." :
             ENGLISH ? "integer ~ is out of range, cannot be output onto ~" :
             FRANCAIS ? "L'entier ~, n'étant pas dans l'intervalle souhaité, ne peut pas être écrit dans ~." :
             ""
            );
    }

# UP: Überprüft Argumente, ob sie Streams sind.
# test_stream_args(args_pointer,argcount);
# > args_pointer: Pointer über die Argumente
# > argcount: Anzahl der Argumente
# > subr_self: Aufrufer (ein SUBR)
  local void test_stream_args (object* args_pointer, uintC argcount);
  local void test_stream_args(args_pointer, argcount)
    var object* args_pointer;
    var uintC argcount;
    { dotimesC(argcount,argcount,
        { var object next_arg = NEXT(args_pointer);
          if (!streamp(next_arg)) { fehler_stream(next_arg); }
        });
    }

# UP: Überprüft einen Input-Stream.
# test_input_stream(stream);
# > stream: Stream
# > subr_self: Aufrufer (ein SUBR)
  #define test_input_stream(stream)  \
    if ((TheStream(stream)->strmflags & strmflags_rd_B) == 0) \
      fehler_input_stream(stream);
  nonreturning_function(local, fehler_input_stream, (object stream));
  local void fehler_input_stream(stream)
    var object stream;
    { pushSTACK(stream); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_input_stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(stream); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             DEUTSCH ? "~: Argument muss ein Input-Stream sein, nicht ~" :
             ENGLISH ? "~: argument ~ should be an input stream" :
             FRANCAIS ? "~ : L'argument doit être un «stream» d'entrée et non pas ~." :
             ""
            );
    }

# UP: Überprüft einen Output-Stream.
# test_output_stream(stream);
# > stream: Stream
# > subr_self: Aufrufer (ein SUBR)
  #define test_output_stream(stream)  \
    if ((TheStream(stream)->strmflags & strmflags_wr_B) == 0) \
      fehler_output_stream(stream);
  nonreturning_function(local, fehler_output_stream, (object stream));
  local void fehler_output_stream(stream)
    var object stream;
    { pushSTACK(stream); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_output_stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(stream); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             DEUTSCH ? "~: Argument muss ein Output-Stream sein, nicht ~" :
             ENGLISH ? "~: argument ~ should be an output stream" :
             FRANCAIS ? "~ : L'argument doit être un «stream» de sortie et non pas ~." :
             ""
            );
    }

# UP: Überprüft Argumente, ob sie Input-Streams sind.
# test_input_stream_args(args_pointer,argcount);
# > args_pointer: Pointer über die Argumente
# > argcount: Anzahl der Argumente
# > subr_self: Aufrufer (ein SUBR)
  #define test_input_stream_args(args_pointer,argcount)  \
    { var object* pointer = (args_pointer);          \
      var uintC count;                               \
      dotimesC(count,argcount,                       \
        { var object arg = NEXT(pointer);            \
          if (!streamp(arg)) { fehler_stream(arg); } \
          test_input_stream(arg);                    \
        });                                          \
    }

# UP: Überprüft Argumente, ob sie Output-Streams sind.
# test_output_stream_args(args_pointer,argcount);
# > args_pointer: Pointer über die Argumente
# > argcount: Anzahl der Argumente
# > subr_self: Aufrufer (ein SUBR)
  #define test_output_stream_args(args_pointer,argcount)  \
    { var object* pointer = (args_pointer);          \
      var uintC count;                               \
      dotimesC(count,argcount,                       \
        { var object arg = NEXT(pointer);            \
          if (!streamp(arg)) { fehler_stream(arg); } \
          test_output_stream(arg);                   \
        });                                          \
    }


#if defined(UNIX) || defined(EMUNIX) || defined(DJUNIX) || defined(WATCOM) || defined(RISCOS)

# UP: Löscht bereits eingegebenen interaktiven Input von einem Handle.
  local void clear_tty_input (Handle handle);
  #if !(defined(DJUNIX) || defined(RISCOS))
  local void clear_tty_input(handle)
    var Handle handle;
    { # Methode 1: tcflush TCIFLUSH, siehe TERMIOS(3V)
      # Methode 2: ioctl TCFLSH TCIFLUSH, siehe TERMIO(4)
      # Methode 3: ioctl TIOCFLUSH FREAD, siehe TTCOMPAT(4)
      begin_system_call();
      #ifdef UNIX_TERM_TERMIOS
      if (!( TCFLUSH(handle,TCIFLUSH) ==0))
        { if (!((errno==ENOTTY)||(errno==EINVAL))) # kein TTY: OK
            { local boolean flag = FALSE;
              if (!flag) { flag = TRUE; OS_error(); } # sonstigen Error melden, aber nur einmal
        }   }
      #endif
      #ifdef UNIX_TERM_TERMIO
      #ifdef TCIFLUSH # !RISCOS
      if (!( ioctl(handle,TCFLSH,(CADDR_T)TCIFLUSH) ==0))
        { if (!(errno==ENOTTY)) # kein TTY: OK
            { local boolean flag = FALSE;
              if (!flag) { flag = TRUE; OS_error(); } # sonstigen Error melden, aber nur einmal
        }   }
      #endif
      #endif
      #ifdef UNIX_TERM_SGTTY
      #ifdef FREAD # !UNIX_MINT
      { var int arg = FREAD;
        if (!( ioctl(handle,TIOCFLUSH,&arg) ==0))
          { if (!(errno==ENOTTY)) # kein TTY: OK
              { local boolean flag = FALSE;
                if (!flag) { flag = TRUE; OS_error(); } # sonstigen Error melden, aber nur einmal
      }   }   }
      #endif
      #endif
      #ifdef EMUNIX
      # Eberhard Mattes sagt, das funktioniert nur, wenn IDEFAULT nicht
      # gesetzt ist. ??
      if (!( ioctl(handle,TCFLSH,0) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } } # kein TTY: OK, sonstigen Erro
      #endif
      end_system_call();
    }
  #else
    #define clear_tty_input(handle)
  #endif

# UP: Bringt den wartenden Output eines Handles ans Ziel.
  local void finish_tty_output (Handle handle);
  #if !(defined(DJUNIX) || defined(WATCOM) || defined(RISCOS))
  local void finish_tty_output(handle)
    var Handle handle;
    { # Methode 1: fsync, siehe FSYNC(2)
      # Methode 2: tcdrain, siehe TERMIOS(3V)
      # Methode 3: ioctl TCSBRK 1, siehe TERMIO(4)
      # evtl. Methode 3: ioctl TCGETS/TCSETSW, siehe TERMIO(4)
      # oder (fast äquivalent) ioctl TIOCGETP/TIOCSETP, siehe TTCOMPAT(4)
      begin_system_call();
      #if !(defined(UNIX) && !defined(HAVE_FSYNC))
      if (!( fsync(handle) ==0))
        #if defined(UNIX_IRIX) || defined(EMUNIX)
        if (!(errno==ENOSYS))
        #endif
        if (!(errno==EINVAL)) { OS_error(); }
      #endif
      #ifdef UNIX_TERM_TERMIOS
      if (!( TCDRAIN(handle) ==0))
        { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      #endif
      #ifdef UNIX_TERM_TERMIO
      if (!( ioctl(handle,TCSBRK,(CADDR_T)1) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
      #endif
      #if defined(UNIX_TERM_TERMIOS) && defined(TCGETS) && defined(TCSETSW)
      {var struct termios term_parameters;
       if (!(   ( ioctl(handle,TCGETS,&term_parameters) ==0)
             && ( ioctl(handle,TCSETSW,&term_parameters) ==0)
          ) )
         { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      }
      #endif
      #ifdef EMUNIX
      {var struct termio term_parameters;
       if (!(   ( ioctl(handle,TCGETA,&term_parameters) ==0)
             && ( ioctl(handle,TCSETAW,&term_parameters) ==0)
          ) )
         { if (!(errno==ENOTTY)) { OS_error(); } }
      }
      #endif
      #if 0 # Vorsicht: das müsste FINISH-OUTPUT und CLEAR-INPUT bewirken!
      {var struct sgttyb tty_parameters;
       if (!(   ( ioctl(handle,TIOCGETP,&tty_parameters) ==0)
             && ( ioctl(handle,TIOCSETP,&tty_parameters) ==0)
          ) )
         { if (!(errno==ENOTTY)) { OS_error(); } }
      }
      #endif
      end_system_call();
    }
  #else
    #define finish_tty_output(handle)
  #endif

# UP: Bringt den wartenden Output eines Handles ans Ziel.
  local void force_tty_output (Handle handle);
  #if !(defined(DJUNIX) || defined(WATCOM) || (defined(UNIX) && !defined(HAVE_FSYNC)) || defined(RISCOS))
  local void force_tty_output(handle)
    var Handle handle;
    { # Methode: fsync, siehe FSYNC(2)
      begin_system_call();
      if (!( fsync(handle) ==0))
        #if defined(UNIX_IRIX) || defined(EMUNIX)
        if (!(errno==ENOSYS))
        #endif
        #ifdef UNIX_CYGWIN32 # needed for Win95 only
        if (!(errno==EBADF))
        #endif
        if (!(errno==EINVAL)) { OS_error(); }
      end_system_call();
    }
  #else
    #define force_tty_output(handle)
  #endif

# UP: Löscht den wartenden Output eines Handles.
  local void clear_tty_output (Handle handle);
  #if !(defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS))
  local void clear_tty_output(handle)
    var Handle handle;
    { # Methode 1: tcflush TCOFLUSH, siehe TERMIOS(3V)
      # Methode 2: ioctl TCFLSH TCOFLUSH, siehe TERMIO(4)
      # Methode 3: ioctl TIOCFLUSH FWRITE, siehe TTCOMPAT(4)
      begin_system_call();
      #ifdef UNIX_TERM_TERMIOS
      if (!( TCFLUSH(handle,TCOFLUSH) ==0))
        {
          #ifdef UNIX_IRIX
          if (!(errno==ENOSYS))
          #endif
          if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } # kein TTY: OK, sonstigen Error melden
        }
      #endif
      #ifdef UNIX_TERM_TERMIO
      #ifdef TCOFLUSH # !RISCOS
      if (!( ioctl(handle,TCFLSH,(CADDR_T)TCOFLUSH) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      #endif
      #endif
      #ifdef UNIX_TERM_SGTTY
      #ifdef FWRITE # !UNIX_MINT
      {var int arg = FWRITE;
       if (!( ioctl(handle,TIOCFLUSH,&arg) ==0))
         { if (!(errno==ENOTTY)) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      }
      #endif
      #endif
      end_system_call();
    }
  #else
    #define clear_tty_output(handle)
  #endif

#endif

#if defined(WIN32_NATIVE)

# UP: Löscht bereits eingegebenen interaktiven Input von einem Handle.
  local void clear_tty_input (Handle handle);
  local void clear_tty_input(handle)
    var Handle handle;
    { begin_system_call();
      # Maybe it's a serial communication.
      if (!PurgeComm(handle,PURGE_RXABORT|PURGE_RXCLEAR))
        { if (!(GetLastError()==ERROR_INVALID_HANDLE || GetLastError()==ERROR_INVALID_FUNCTION))
            { OS_error(); }
        }
      # Maybe it's a console.
      if (!FlushConsoleInputBuffer(handle))
        { if (!(GetLastError()==ERROR_INVALID_HANDLE))
            { OS_error(); }
        }
      end_system_call();
    }

# UP: Bringt den wartenden Output eines Handles ans Ziel.
  local void finish_tty_output (Handle handle);
  # Maybe call WaitCommEvent with argument EV_TXEMPTY ?
  #define finish_tty_output(handle)

# UP: Bringt den wartenden Output eines Handles ans Ziel.
  local void force_tty_output (Handle handle);
  #define force_tty_output(handle)  finish_tty_output(handle)

# UP: Löscht den wartenden Output eines Handles.
  local void clear_tty_output (Handle handle);
  local void clear_tty_output(handle)
    var Handle handle;
    { begin_system_call();
      # Maybe it's a serial communication.
      if (!PurgeComm(handle,PURGE_TXABORT|PURGE_TXCLEAR))
        { if (!(GetLastError()==ERROR_INVALID_HANDLE || GetLastError()==ERROR_INVALID_FUNCTION))
            { OS_error(); }
        }
      end_system_call();
    }

#endif

#if defined(AMIGAOS)

# UP: Bringt den wartenden Output eines Handles ans Ziel.
  local void finish_tty_output (Handle handle);
  # Wir können nichts tun, da wir handle nicht schließen dürfen und
  # kein fsync() haben.
  #define finish_tty_output(handle)

# UP: Bringt den wartenden Output eines Handles ans Ziel.
  local void force_tty_output (Handle handle);
  #define force_tty_output(handle)  finish_tty_output(handle)

# UP: Löscht den wartenden Output eines Handles.
  local void clear_tty_output (Handle handle);
  # Nichts zu tun.
  #define clear_tty_output(handle)

#endif

# UP: Stellt fest, ob ein Handle auf ein (statisches) File verweist.
# regular_handle_p(handle)
# > handle: Handle des geöffneten Files
# < ergebnis: TRUE wenn ein (statisches) File
  local boolean regular_handle_p (Handle handle);
  local boolean regular_handle_p(handle)
    var Handle handle;
    {
      #if defined(UNIX) || defined(MSDOS) || defined(RISCOS)
        var struct stat statbuf;
        begin_system_call();
        if (!( fstat(handle,&statbuf) ==0)) { OS_error(); }
        end_system_call();
        return (S_ISREG(statbuf.st_mode) ? TRUE : FALSE);
      #endif
      #ifdef AMIGAOS
        var LONG not_regular_p;
        begin_system_call();
        not_regular_p = IsInteractive(handle); # Behandlung nicht interaktiver, nicht regulärer Files??
        end_system_call();
        return !not_regular_p;
      #endif
      #ifdef WIN32_NATIVE
        var DWORD filetype;
        begin_system_call();
        filetype = GetFileType(handle);
        end_system_call();
        return (filetype == FILE_TYPE_DISK);
      #endif
    }


#if (defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32_NATIVE)) && (!defined(TERMINAL_USES_KEYBOARD) || defined(HANDLES) || defined(PIPES) || ((defined(X11SOCKETS) || defined(SOCKET_STREAMS)) && !defined(WIN32_NATIVE)))
#define XHANDLES

# Handle-Streams
# ==============

# sind ein gemeinsamer Rahmen für Streams, deren Input/Output ungebuffert
# über ein Handle des Betriebssystems abgewickelt wird. Umfasst:
# Input: Terminal-Stream, File-Handle-Stream, Pipe-Input-Stream, Socket-Stream.
# Output: File-Handle-Stream, Pipe-Output-Stream, Socket-Stream.

#define strm_isatty   strm_other[0]  # Flag, ob das Input-Handle ein TTY ist
#define strm_ihandle  strm_other[1]  # Input-Handle immer als zweite Komponente
#define strm_ohandle  strm_other[2]  # Output-Handle immer als dritte Komponente

# Dass beim Input EOF erreicht ist, erkennt man an
# TheStream(stream)->strm_rd_ch_last = eof_value.

# READ-CHAR - Pseudofunktion für Handle-Streams:
  local object rd_ch_handle (const object* stream_);
  local object rd_ch_handle(stream_)
    var const object* stream_;
    {   restart_it:
     {  var object stream = *stream_;
        if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF?
          { return eof_value; }
      { var Handle handle = TheHandle(TheStream(stream)->strm_ihandle);
        var uintB c;
        #if defined(AMIGAOS)
        interruptp({ pushSTACK(S(read_char)); tast_break(); # Ctrl-C -> Break-Schleife aufrufen
                     return read_char(stream_);
                   });
        #endif
        run_time_stop(); # Run-Time-Stoppuhr anhalten
        begin_system_call();
        #ifdef GRAPHICS_SWITCH
        if (handle == stdin_handle) switch_text_mode();
        #endif
       {var int ergebnis = read(handle,&c,1); # Zeichen lesen versuchen
        end_system_call();
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        if (ergebnis<0)
          {
            #if !(defined(AMIGAOS) || defined(WIN32_NATIVE))
            begin_system_call();
            if (errno==EINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
              { end_system_call();
                interruptp({ pushSTACK(S(read_char)); tast_break(); }); # Break-Schleife aufrufen
                goto restart_it;
              }
            #endif
            #ifdef WIN32_NATIVE
            begin_system_call();
            if (GetLastError()==ERROR_SIGINT) # Unterbrechung durch Ctrl-C ?
              { end_system_call();
                pushSTACK(S(read_char)); tast_break(); # Break-Schleife aufrufen
                goto restart_it;
              }
            #endif
            OS_error();
          }
        if (ergebnis==0)
          # kein Zeichen verfügbar -> muss EOF sein
          { return eof_value; }
          else
          {
            #if defined(AMIGAOS)
            # Ctrl-C wird meist während des Read()-Aufrufs festgestellt, und
            # Read() liefert dann "unschuldig" ein Zeichen ab. Wir behandeln
            # das Ctrl-C jetzt. Damit das Zeichen nicht verlorengeht, wird
            # es wie durch unread_char() zurückgelegt.
            interruptp(
              { TheStream(stream)->strm_rd_ch_last = code_char(c);
                TheStream(stream)->strmflags |= strmflags_unread_B;
                pushSTACK(S(read_char)); tast_break(); # Break-Schleife aufrufen
                return read_char(stream_);
              });
            #endif
            return code_char(c);
          }
    }}}}

# Stellt fest, ob ein Handle-Stream ein Zeichen verfügbar hat.
# listen_handle(stream)
# > stream: Handle-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_handle (object stream);
  local signean listen_handle(stream)
    var object stream;
    { if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        { return signean_minus; }
      # Methode 1: select, siehe SELECT(2)
      # Methode 2: ioctl FIONREAD, siehe FILIO(4)
      # Methode 3: kurzzeitig auf non-blocking I/O schalten und read versuchen,
      #            siehe READ(2V) und FILIO(4), bzw.
      #            siehe READ(2V), FCNTL(2V) und FCNTL(5)
     {var Handle handle = TheHandle(TheStream(stream)->strm_ihandle);
      #if defined(MSDOS) && !defined(EMUNIX_PORTABEL)
      { var uintB status;
        begin_system_call();
        get_handle_input_status(handle,status);
        end_system_call();
        if (status) { return signean_null; } # Zeichen verfügbar
      }
      if (!nullp(TheStream(stream)->strm_isatty))
        # Terminal
        { return signean_plus; } # kein Zeichen verfügbar
        else
        # File
        # kein Zeichen verfügbar -> EOF erkennen
        { TheStream(stream)->strm_rd_ch_last = eof_value;
          return signean_minus;
        }
      #elif defined(EMUNIX_PORTABEL)
      { var struct termio oldtermio;
        var struct termio newtermio;
        begin_system_call();
        if (!( ioctl(handle,TCGETA,&oldtermio) ==0))
          { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } }
        newtermio = oldtermio;
        newtermio.c_lflag &= ~IDEFAULT & ~ICANON;
        if (!( ioctl(handle,TCSETA,&newtermio) ==0))
          { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } }
       {var unsigned long chars_ready = 0;
        var int result = ioctl(handle,FIONREAD,&chars_ready); # abfragen
        # (Seit emx 0.8f könnte man das auch mit select() machen.)
        if (!( ioctl(handle,TCSETA,&oldtermio) ==0))
          { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } }
        end_system_call();
        if (result == 0)
          # Abfrage gelungen
          { if (chars_ready > 0) { return signean_null; } } # welche verfügbar?
        begin_system_call();
        if (!isatty(handle))
          { result = eof(handle);
            if (result<0)
              { if (!(errno==ESPIPE)) { OS_error(); } } # "Illegal seek error" ist OK
              else
              { end_system_call();
                if (result>0) # EOF erreicht?
                  { return signean_minus; }
                  else
                  { return signean_null; }
          }   }
        end_system_call();
        return signean_plus; # offenbar kein Zeichen verfügbar
      }}
      #elif !(defined(AMIGAOS) || defined(WIN32_NATIVE))
      #ifdef HAVE_SELECT
      { # Verwende select mit readfds = einelementige Menge {handle}
        # und timeout = Null-Zeitintervall.
        var fd_set handle_menge; # Menge von Handles := {handle}
        var struct timeval zero_time; # Zeitintervall := 0
        begin_system_call();
        FD_ZERO(&handle_menge); FD_SET(handle,&handle_menge);
        restart_select:
        zero_time.tv_sec = 0; zero_time.tv_usec = 0;
       {var int ergebnis;
        ergebnis = select(FD_SETSIZE,&handle_menge,NULL,NULL,&zero_time);
        if (ergebnis<0)
          { if (errno==EINTR) goto restart_select;
            if (!(errno==EBADF)) { OS_error(); } # UNIX_LINUX liefert bei Files EBADF !
            end_system_call();
          }
          else
          { end_system_call();
            # ergebnis = Anzahl der Handles in handle_menge, bei denen read
            # sofort ein Ergebnis liefern würde.
            if (ergebnis==0)
              { return signean_plus; } # kein Zeichen verfügbar
            # ergebnis=1
            # Wenn read() sofort ein Ergebnis liefern würde, kann das auch EOF
            # sein! (Beispiel: Linux und Pipes.) Wir hüten uns daher vor
            # einem  { return signean_null; }  und versuchen stattdessen
            # erst noch Methoden 2 und 3.
      }}  }
      #endif
      begin_system_call();
      #ifdef HAVE_FIONREAD
      # versuche die Zahl der verfügbaren Zeichen abzufragen:
      {var unsigned long chars_ready;
       if ( ioctl(handle,FIONREAD,&chars_ready) <0) # abfragen
         # Abfrage misslungen, war wohl kein File
         { if (!((errno == ENOTTY)
                 || (errno == EINVAL)
                 #ifdef ENOSYS # for UNIX_IRIX
                 || (errno == ENOSYS)
                 #endif
              ) )
             { OS_error(); }
         }
         else
         # Abfrage gelungen, also war's ein File
         { end_system_call();
           if (chars_ready > 0) { return signean_null; } # welche verfügbar?
           #ifdef HAVE_RELIABLE_FIONREAD
           # sonst EOF des File erkennen:
           TheStream(stream)->strm_rd_ch_last = eof_value;
           return signean_minus;
           #endif
         }
      }
      #endif
      #ifdef GRAPHICS_SWITCH
      if (handle == stdin_handle) switch_text_mode();
      #endif
      #ifndef HAVE_SELECT
      if (!nullp(TheStream(stream)->strm_isatty))
        # Terminal
        { # in Non-blocking-Modus umschalten, dann read() versuchen:
          var uintB c;
          var int ergebnis;
          restart_read_tty:
          #ifdef FIONBIO # non-blocking I/O à la BSD 4.2
          { var int non_blocking_io;
            non_blocking_io = 1;
            if (!( ioctl(handle,FIONBIO,&non_blocking_io) ==0))
              { OS_error(); }
            ergebnis = read(handle,&c,1); # Zeichen lesen versuchen
            non_blocking_io = 0;
            if (!( ioctl(handle,FIONBIO,&non_blocking_io) ==0))
              { OS_error(); }
          }
          #else # non-blocking I/O à la SYSV
          { var int fcntl_flags;
            if (( fcntl_flags = fcntl(handle,F_GETFL,0) )<0) { OS_error(); }
            if ( fcntl(handle,F_SETFL,fcntl_flags|O_NDELAY) <0) { OS_error(); }
            ergebnis = read(handle,&c,1); # Zeichen lesen versuchen
            if ( fcntl(handle,F_SETFL,fcntl_flags) <0) { OS_error(); }
          }
          #endif
          if (ergebnis < 0)
            { if (errno==EINTR) goto restart_read_tty;
              #ifdef FIONBIO
              if (errno==EWOULDBLOCK) # BSD 4.2 Error-Code
              #else
              if ((errno==EAGAIN) # Posix Error-Code
                  #ifdef EWOULDBLOCK
                  || (errno==EWOULDBLOCK)
                  #endif
                 )
              #endif
                { return signean_plus; } # kein Zeichen verfügbar
              OS_error();
            }
          end_system_call();
          if (ergebnis==0)
            # kein Zeichen verfügbar
            { return signean_plus; }
            else
            # Zeichen verfügbar
            { TheStream(stream)->strm_rd_ch_last = code_char(c);
              TheStream(stream)->strmflags |= strmflags_unread_B;
              return signean_null;
            }
          # Sollte das nicht gehen, einen Timer von 1/10 sec verwenden??
        }
        else
      #endif
        # File (oder Pipe)
        { # ein Zeichen lesen versuchen (wie bei peek_char):
          restart_read_other:
         {var uintB c;
          var int ergebnis = read(handle,&c,1); # Zeichen lesen versuchen
          if (ergebnis<0)
            { if (errno==EINTR) goto restart_read_other;
              OS_error();
            }
          end_system_call();
          if (ergebnis==0)
            # kein Zeichen verfügbar -> EOF erkennen
            { TheStream(stream)->strm_rd_ch_last = eof_value;
              return signean_minus;
            }
            else # Zeichen verfügbar
            { TheStream(stream)->strm_rd_ch_last = code_char(c);
              TheStream(stream)->strmflags |= strmflags_unread_B;
              return signean_null;
            }
        }}
      #elif defined(AMIGAOS)
      begin_system_call();
      if (!nullp(TheStream(stream)->strm_isatty))
        # interaktiv
        { if (WaitForChar(handle,0L)) # 0 usec auf ein Zeichen warten
            { end_system_call(); return signean_null; } # eins da
            else
            { end_system_call(); return signean_plus; } # keins da
        }
        else
        # nicht interaktiv
        { # ein Zeichen lesen versuchen (wie bei peek_char):
          var uintB c;
          var long ergebnis = Read(handle,&c,1L); # Zeichen lesen versuchen
          end_system_call();
          if (ergebnis<0) { OS_error(); }
          if (ergebnis==0)
            # kein Zeichen verfügbar -> EOF erkennen
            { TheStream(stream)->strm_rd_ch_last = eof_value;
              return signean_minus;
            }
            else # Zeichen verfügbar
            { TheStream(stream)->strm_rd_ch_last = code_char(c);
              TheStream(stream)->strmflags |= strmflags_unread_B;
              return signean_null;
            }
        }
      #elif defined(WIN32_NATIVE)
      # This is pretty complex. To test this, create a file "listen.lsp"
      # containing the code
      #   (tagbody 1 (prin1 (listen *terminal-io*)) (sys::%sleep 0 500) (go 1))
      # and execute "lisp.exe -q -i listen.lsp" with redirected standard input.
      begin_system_call();
      switch (GetFileType(handle))
        {
          case FILE_TYPE_CHAR:
            { var DWORD nevents;
              if (GetNumberOfConsoleInputEvents(handle,&nevents))
                # It's a console.
                { if (nevents==0)
                    # kein Zeichen verfügbar
                    { end_system_call(); return signean_plus; }
                 {var INPUT_RECORD* events = (INPUT_RECORD*)alloca(nevents*sizeof(INPUT_RECORD));
                  var DWORD nevents_read;
                  var DWORD mode;
                  if (!PeekConsoleInput(handle,events,nevents,&nevents_read))
                    { OS_error(); }
                  if (nevents_read==0)
                    # kein Zeichen verfügbar
                    { end_system_call(); return signean_plus; }
                  if (!GetConsoleMode(handle,&mode)) { OS_error(); }
                  if (mode & ENABLE_LINE_INPUT)
                    { # Look out for a Key-Down event corresponding to CR/LF.
                      var DWORD i;
                      for (i = 0; i < nevents_read; i++)
                        { if (events[i].EventType == KEY_EVENT
                              && events[i].Event.KeyEvent.bKeyDown
                              && events[i].Event.KeyEvent.uAsciiChar == CR)
                            # Zeichen verfügbar
                            { end_system_call(); return signean_null; }
                    }   }
                    else
                    { # Look out for any Key-Down event.
                      var DWORD i;
                      for (i = 0; i < nevents_read; i++)
                        { if (events[i].EventType == KEY_EVENT
                              && events[i].Event.KeyEvent.bKeyDown
                              && events[i].Event.KeyEvent.uAsciiChar != 0)
                            # Zeichen verfügbar
                            { end_system_call(); return signean_null; }
                    }   }
                  # kein Zeichen verfügbar
                  end_system_call(); return signean_plus;
                }}
              elif (!(GetLastError()==ERROR_INVALID_HANDLE)) { OS_error(); }
            }
            # Not a console.
            switch (WaitForSingleObject(handle,0))
              { case WAIT_OBJECT_0:
                  # Zeichen verfügbar oder EOF
                  break;
                case WAIT_TIMEOUT:
                  # kein Zeichen verfügbar
                  end_system_call(); return signean_plus;
                default:
                  OS_error();
              }
            /*FALLTHROUGH*/
          case FILE_TYPE_DISK:
          default:
            # It's a file (or something unknown).
            { var uintB c;
              var int ergebnis = read(handle,&c,1); # Zeichen lesen versuchen
              if (ergebnis<0) { OS_error(); }
              end_system_call();
              if (ergebnis==0)
                # kein Zeichen verfügbar -> EOF erkennen
                { TheStream(stream)->strm_rd_ch_last = eof_value;
                  return signean_minus;
                }
                else # Zeichen verfügbar
                { TheStream(stream)->strm_rd_ch_last = code_char(c);
                  TheStream(stream)->strmflags |= strmflags_unread_B;
                  return signean_null;
            }   }
          case FILE_TYPE_PIPE:
            { var DWORD nchars;
              if (PeekNamedPipe(handle,NULL,0,NULL,&nchars,NULL))
                # It's a pipe (input).
                { end_system_call();
                  if (nchars > 0)
                    # Zeichen verfügbar
                    return signean_null;
                  else
                    # kein Zeichen verfügbar
                    return signean_plus;
                }
              elif (GetLastError()==ERROR_BROKEN_PIPE)
                # EOF reached
                { end_system_call();
                  TheStream(stream)->strm_rd_ch_last = eof_value;
                  return signean_minus;
                }
              elif (GetLastError()==ERROR_ACCESS_DENIED)
                # It's a pipe (output). Let's fake EOF.
                { end_system_call();
                  TheStream(stream)->strm_rd_ch_last = eof_value;
                  return signean_minus;
                }
              else
                # What about sockets??
                { OS_error(); }
            }
        }
      #endif
    }}

# UP: Löscht bereits eingegebenen interaktiven Input von einem Handle-Stream.
# clear_input_handle(stream);
# > stream: Handle-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_handle (object stream);
  local boolean clear_input_handle(stream)
    var object stream;
    { var Handle handle = TheHandle(TheStream(stream)->strm_ihandle);
      if (nullp(TheStream(stream)->strm_isatty))
        # File -> nichts tun
        { return FALSE; }
      #if !defined(AMIGAOS)
      # Terminal
      TheStream(stream)->strm_rd_ch_last = NIL; # gewesenes EOF vergessen
      clear_tty_input(handle);
      # Für den Fall, das das nicht funktionierte:
      # Zeichen lesen, solange listen_handle() 0 liefert.
      pushSTACK(stream);
      while (listen_handle(STACK_0) == 0) { read_char(&STACK_0); }
      skipSTACK(1);
      return TRUE;
      #else # defined(AMIGAOS)
      # interaktiv
      { begin_system_call();
        loop
          { if (!WaitForChar(handle,0L)) # 0 usec auf ein Zeichen warten
              break; # keins mehr da -> fertig
           {var uintB c;
            var long ergebnis = Read(handle,&c,1L); # Zeichen lesen versuchen
            if (ergebnis<0) { OS_error(); }
          }}
        end_system_call();
        return TRUE;
      }
      #endif
    }

# WRITE-CHAR - Pseudofunktion für Handle-Streams:
  local void wr_ch_handle (const object* stream_, object ch);
  local void wr_ch_handle(stream_,ch)
    var const object* stream_;
    var object ch;
    { var Handle handle = TheHandle(TheStream(*stream_)->strm_ohandle);
      # ch sollte Character sein:
      if (!charp(ch)) { fehler_wr_char(*stream_,ch); }
     {var uintB c = char_code(ch); # Code des Zeichens
      restart_it:
      begin_system_call();
      #ifdef GRAPHICS_SWITCH
      if (handle == stdout_handle) switch_text_mode();
      #endif
      {
       #if !defined(AMIGAOS)
       var int ergebnis = write(handle,&c,1); # Zeichen auszugeben versuchen
       if (ergebnis<0)
         {
           #if !defined(WIN32_NATIVE)
           if (errno==EINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
             { end_system_call();
               interruptp({ pushSTACK(S(write_char)); tast_break(); }); # Break-Schleife aufrufen
               goto restart_it;
             }
           #endif
           OS_error(); # Error melden
         }
       end_system_call();
       #else # defined(AMIGAOS)
       var long ergebnis = Write(handle,&c,1L); # Zeichen auszugeben versuchen
       end_system_call();
       if (ergebnis<0) { OS_error(); } # Error melden
       interruptp({ pushSTACK(S(write_char)); tast_break(); }); # Ctrl-C -> Break-Schleife aufrufen
       #endif
       if (ergebnis==0) # nicht erfolgreich?
         { fehler_unwritable(S(write_char),*stream_); }
      }
    }}

# WRITE-CHAR-SEQUENCE für Handle-Streams:
  local const uintB* write_char_array_handle (object stream, const uintB* ptr, uintL len);
  local const uintB* write_char_array_handle(stream,ptr,len)
    var object stream;
    var const uintB* ptr;
    var uintL len;
    { var Handle handle = TheHandle(TheStream(stream)->strm_ohandle);
      begin_system_call();
      #ifdef GRAPHICS_SWITCH
      if (handle == stdout_handle) switch_text_mode();
      #endif
     {var sintL ergebnis = full_write(handle,ptr,len); # Zeichen auszugeben versuchen
      if (ergebnis<0) { OS_error(); } # Error melden
      end_system_call();
      if (!(ergebnis==(sintL)len)) # nicht erfolgreich?
        { fehler_unwritable(S(write_string),stream); }
      ptr += len;
      wr_ss_lpos(stream,ptr,len); # Line-Position aktualisieren
      return ptr;
    }}

# WRITE-SIMPLE-STRING - Pseudofunktion für Handle-Streams:
  local void wr_ss_handle (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_handle(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { if (len==0) return;
      write_char_array_handle(*stream_,&TheSstring(string)->data[start],len);
    }

#if defined(MSDOS) || defined(WIN32) || (defined(UNIX) && (O_BINARY != 0))
# WRITE-CHAR - Pseudofunktion für Handle-Streams, mit NL -> CR/LF - Umwandlung:
  local void wr_ch_handle_x (const object* stream_, object ch);
  local void wr_ch_handle_x(stream_,ch)
    var const object* stream_;
    var object ch;
    { if (eq(ch,code_char(NL)))
        # Newline als CR/LF ausgeben
        { wr_ch_handle(stream_,code_char(CR));
          wr_ch_handle(stream_,code_char(LF));
        }
        else
        # alle anderen Zeichen unverändert ausgeben
        { wr_ch_handle(stream_,ch); }
    }
  #define wr_ss_handle_x wr_ss_dummy
#else
  #define wr_ch_handle_x wr_ch_handle
  #define wr_ss_handle_x wr_ss_handle
#endif

# UP: Bringt den wartenden Output eines Handle-Stream ans Ziel.
# finish_output_handle(stream);
# > stream: Handle-Stream
# kann GC auslösen
  local void finish_output_handle (object stream);
  local void finish_output_handle(stream)
    var object stream;
    { finish_tty_output(TheHandle(TheStream(stream)->strm_ohandle)); }

# UP: Bringt den wartenden Output eines Handle-Stream ans Ziel.
# force_output_handle(stream);
# > stream: Handle-Stream
# kann GC auslösen
  local void force_output_handle (object stream);
  local void force_output_handle(stream)
    var object stream;
    { force_tty_output(TheHandle(TheStream(stream)->strm_ohandle)); }

# UP: Löscht den wartenden Output eines Handle-Stream.
# clear_output_handle(stream);
# > stream: Handle-Stream
# kann GC auslösen
  local void clear_output_handle (object stream);
  local void clear_output_handle(stream)
    var object stream;
    { clear_tty_output(TheHandle(TheStream(stream)->strm_ohandle)); }

#if defined(HANDLES) || (defined(PIPES) && (defined(UNIX) || defined(WIN32_NATIVE))) || ((defined(X11SOCKETS) || defined(SOCKET_STREAMS)) && !defined(WIN32_NATIVE))

# READ-BYTE - Pseudofunktion für Handle-Streams:
  local object rd_by_handle (object stream);
  local object rd_by_handle(stream)
    var object stream;
    { pushSTACK(stream);
     {var object obj = read_char(&STACK_0);
      skipSTACK(1);
      if (!eq(obj,eof_value)) { obj = char_to_fixnum(obj); }
      return obj;
    }}

# WRITE-BYTE - Pseudofunktion für Handle-Streams:
  local void wr_by_handle (object stream, object obj);
  local void wr_by_handle(stream,obj)
    var object stream;
    var object obj;
    { # obj überprüfen:
      if (!integerp(obj)) { fehler_wr_integer(stream,obj); }
      if (!(posfixnump(obj) && (posfixnum_to_L(obj) < char_code_limit)))
        { fehler_bad_integer(stream,obj); }
      pushSTACK(stream);
      wr_ch_handle(&STACK_0,fixnum_to_char(obj));
      skipSTACK(1);
    }

#endif

#if defined(HANDLES) || (defined(PIPES) && (defined(UNIX) || defined(WIN32_NATIVE))) || ((defined(X11SOCKETS) || defined(SOCKET_STREAMS)) && !defined(WIN32_NATIVE))

# Schließt einen Handle-Stream.
# close_ihandle(stream);
# close_ohandle(stream);
# > stream : Handle-Stream
  local void close_ihandle (object stream);
  local void close_ohandle (object stream);
  local void close_ihandle(stream)
    var object stream;
    { var Handle handle = TheHandle(TheStream(stream)->strm_ihandle);
      begin_system_call();
      #if defined(UNIX) || defined(MSDOS) || defined(AMIGAOS) || defined(RISCOS)
      if (!( CLOSE(handle) ==0)) { OS_error(); }
      #endif
      #ifdef WIN32_NATIVE
      if (!CloseHandle(handle)) { OS_error(); }
      #endif
      end_system_call();
    }
  local void close_ohandle(stream)
    var object stream;
    { var Handle handle = TheHandle(TheStream(stream)->strm_ohandle);
      begin_system_call();
      #if defined(UNIX) || defined(MSDOS) || defined(AMIGAOS) || defined(RISCOS)
      if (!( CLOSE(handle) ==0)) { OS_error(); }
      #endif
      #ifdef WIN32_NATIVE
      if (!CloseHandle(handle)) { OS_error(); }
      #endif
      end_system_call();
    }

#endif

#if defined(HANDLES)

#define close_handle  close_ihandle

#ifdef AMIGAOS
  #define strm_handle_rawp strm_other[5]  # Flag, ob im Raw-Modus oder nicht
  #define strm_handle_len  (strm_len+6)
#else
  #define strm_handle_len  (strm_len+5)
#endif

# UP: erzeugt ein File-Handle-Stream
# make_handle_stream(handle,direction)
# > handle: Handle des geöffneten Files
# > STACK_1: Filename, ein Pathname
# > STACK_0: Truename, ein Pathname
# > direction: Modus (0 = :PROBE, 1 = :INPUT, 4 = :OUTPUT, 5 = :IO, 3 = :INPUT-IMMUTABLE)
# < ergebnis: File-Handle-Stream
# < STACK: aufgeräumt
# kann GC auslösen
  local object make_handle_stream (object handle, uintB direction);
  local object make_handle_stream(handle,direction)
    var object handle;
    var uintB direction;
    { # Flags:
      var uintB flags =
          ((direction & bit(0)) ? strmflags_rd_B : 0) # evtl. READ-CHAR, READ-BYTE erlaubt
        | ((direction & bit(2)) ? strmflags_wr_B : 0) # evtl. WRITE-CHAR, WRITE-BYTE erlaubt
        ;
      #if defined(FOREIGN_HANDLE) || !NIL_IS_CONSTANT
      pushSTACK(handle); # Handle retten
      #endif
     {# Stream allozieren:
      var object stream = allocate_stream(flags,strmtype_handle,strm_handle_len,0);
      # und füllen:
      if (direction & bit(0))
        { TheStream(stream)->strm_rd_by = P(rd_by_handle);
          TheStream(stream)->strm_rd_ch = P(rd_ch_handle);
        }
        else
        { TheStream(stream)->strm_rd_by = P(rd_by_dummy);
          TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
        }
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      if (direction & bit(2))
        { TheStream(stream)->strm_wr_by = P(wr_by_handle);
          TheStream(stream)->strm_wr_ch = P(wr_ch_handle_x);
          TheStream(stream)->strm_wr_ss = P(wr_ss_handle_x);
        }
        else
        { TheStream(stream)->strm_wr_by = P(wr_by_dummy);
          TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
          TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
        }
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      #if defined(FOREIGN_HANDLE) || !NIL_IS_CONSTANT
      handle = popSTACK(); # Handle zurück
      #endif
      TheStream(stream)->strm_ihandle =
      TheStream(stream)->strm_ohandle = handle; # Handle eintragen
      # Flag isatty = (handle_tty ? T : NIL) bestimmen:
      begin_system_call();
      TheStream(stream)->strm_isatty = (isatty(TheHandle(handle)) ? T : NIL);
      end_system_call();
      # File-Handle-Streams werden für Pathname-Zwecke wie File-Streams behandelt.
      # Daher ist (vgl. file_write_date) strm_file_handle == strm_ohandle,
      # und wir tragen nun die Pathnames ein:
      TheStream(stream)->strm_file_truename = popSTACK(); # Truename eintragen
      TheStream(stream)->strm_file_name = popSTACK(); # Filename eintragen
      #ifdef AMIGAOS
      TheStream(stream)->strm_handle_rawp = NIL;
      #endif
      # Liste der offenen Streams um stream erweitern:
      pushSTACK(stream);
      {var object new_cons = allocate_cons();
       Car(new_cons) = stream = popSTACK();
       Cdr(new_cons) = O(open_files);
       O(open_files) = new_cons;
      }
      return stream;
    }}

#endif

#endif # (UNIX || DJUNIX || EMUNIX || WATCOM || AMIGAOS || RISCOS || WIN32_NATIVE) && (brauche Handle-Streams)


#ifdef KEYBOARD

# Keyboard-Stream
# ===============

# Funktionsweise:
# Liest ein Zeichen von Tastatur.
# Liefert ein Character mit Font=0 und folgenden Bits:
#   HYPER      falls Sondertaste.
#              Zu den Sondertasten zählen die Non-Standard-Tasten.
#              MSDOS:
#                Funktionstasten, Cursorblöcke, Ziffernblock.
#   CHAR-CODE  Bei normalen Tasten der Ascii-Code,
#              bei Sondertasten:
#              MSDOS:
#                F1 -> #\F1, ..., F10 -> #\F10, F11 -> #\F11, F12 -> #\F12,
#                Insert -> #\Insert, Delete -> #\Delete,
#                Home -> #\Home, End -> #\End, PgUp -> #\PgUp, PgDn -> #\PgDn,
#                Pfeiltasten -> #\Up, #\Down, #\Left, #\Right.
#   SUPER      falls mit Shift-Taste(n) gedrückt und sich ohne Shift
#              ein anderer Code ergeben hätte,
#   CONTROL    falls mit Control-Taste gedrückt,
#   META       falls mit Alternate-Taste gedrückt.

#if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)
  # Zusätzliche Komponenten:
  #define strm_keyboard_isatty  strm_isatty   # Flag, ob stdin ein Terminal ist
  #define strm_keyboard_handle  strm_ihandle  # Handle für listen_handle()
  #define strm_keyboard_buffer  strm_other[2] # Liste der noch zu liefernden Zeichen
  #define strm_keyboard_keytab  strm_other[3] # Liste aller Tastenzuordnungen
                                              # jeweils (char1 ... charn . result)
  #define strm_keyboard_len  4
#elif defined(WIN32_NATIVE)
  # Zusätzliche Komponenten:
  #define strm_keyboard_isatty  strm_isatty   # Flag, ob stdin ein Terminal ist
  #define strm_keyboard_handle  strm_ihandle  # Handle für listen_handle()
  #define strm_keyboard_len  2
#else
  # Keine zusätzlichen Komponenten.
  #define strm_keyboard_len  0
#endif

# The keyboard events are instances of INPUT-CHARACTER. We create them by
# calling MAKE-INPUT-CHARACTER or MAKE-CHAR. The following structure describes
# the arguments to MAKE-INPUT-CHARACTER.
typedef struct {
  const char * key;
  uintB code;
  uintB bits;
} key_event;

# Creates a keyboard event.
local object make_key_event (const key_event* event);
local object make_key_event(event)
  var const key_event* event;
  { if ((event->key == NULL) && (event->bits == 0))
      { pushSTACK(S(Kchar)); pushSTACK(code_char(event->code));
        funcall(S(make_input_character),2);
      }
      else
      { pushSTACK(S(Kkey));
        if (event->key == NULL)
          { pushSTACK(code_char(event->code)); }
          else
          { pushSTACK(intern_keyword(asciz_to_string(event->key))); }
        pushSTACK(S(Kbits)); pushSTACK(fixnum(event->bits));
        funcall(S(make_input_character),4);
      }
    return value1;
  }

# Values for the bits, must agree with xcharin.lsp.
  #define char_control_c  1
  #define char_meta_c     2
  #define char_super_c    4
  #define char_hyper_c    8

#ifdef MSDOS

# Für Tastaturabfrage unter DOS:
#
# INT 16 documentation:
#   INT 16,00 - Wait for keystroke and read
#   INT 16,01 - Get keystroke status
#   INT 16,02 - Get shift status
#   INT 16,03 - Set keyboard typematic rate (AT+)
#   INT 16,04 - Keyboard click adjustment (AT+)
#   INT 16,05 - Keyboard buffer write  (AT,PS/2 enhanced keyboards)
#   INT 16,10 - Wait for keystroke and read  (AT,PS/2 enhanced keyboards)
#   INT 16,11 - Get keystroke status  (AT,PS/2 enhanced keyboards)
#   INT 16,12 - Get shift status  (AT,PS/2 enhanced keyboards)
#
# INT 16,00 - Wait for Keypress and Read Character
#     AH = 00
#     on return:
#     AH = keyboard scan code
#     AL = ASCII character or zero if special function key
#     - halts program until key with a scancode is pressed
#     - see  SCAN CODES
#
# INT 16,01 - Get Keyboard Status
#     AH = 01
#     on return:
#     ZF = 0 if a key pressed (even Ctrl-Break)
#     AX = 0 if no scan code is available
#     AH = scan code
#     AL = ASCII character or zero if special function key
#     - data code is not removed from buffer
#     - Ctrl-Break places a zero word in the keyboard buffer but does
#       register a keypress.
#
# INT 16,10 - Extended Wait for Keypress and Read Character  (AT+)
#     AH = 10h
#     on return:
#     AH = scan code
#     AL = ASCII character or zero if special function key
#     - available on AT and PS/2 machines with extended keyboard support
#     - similar to INT 16,00
#
# INT 16,11 - Extended Get Keyboard Status  (AT+)
#       AH = 11h
#       on return:
#       ZF = 0 if key pressed (data waiting)
#       AX = 0 if no scan code is available
#       AH = scan code
#       AL = ASCII character or zero if special function key
#       - available on AT and PS/2 machines with extended keyboard support
#       - data is not removed from buffer
#       - similar to INT 16,01
#

#if defined(DJUNIX) || defined(WATCOM)

  # Liefert den nächsten Tastendruck incl. Scan-Code:
  # high byte = Scan-Code oder 0, low byte = Ascii-Code oder 0 oder 0xE0.
  local boolean kbhit()
    { var union REGS in;
      var union REGS out;
      in.regB.ah = 0x11;
      int86(0x16,&in,&out);
      return ((out.reg_flags & 0x40) == 0); # Zero-Flag abfragen
    }
  local uintW getch()
    { var union REGS in;
      var union REGS out;
      in.regB.ah = 0x10;
      int86(0x16,&in,&out);
      return out.regW.ax;
    }

#endif

#ifdef EMUNIX

  # Unter DOS:
  #   Bis emx 0.8e ist uns der INT 16,10 offenbar versperrt.
  #   Wir bekommen keine Extended-Keystrokes, können aber immerhin die Return-
  #   von der Enter-Taste unterscheiden.
  # Unter OS/2:
  #   INT 16 funktioniert nicht, dafür geht _read_kbd() präziser als unter DOS.

  # Liefert unter DOS den nächsten Tastendruck incl. Scan-Code:
  # high byte = Scan-Code oder 0, low byte = Ascii-Code oder 0 oder 0xE0.
  # (Note: Must push/pop %ebx because this is the STACK_register.)
  local boolean kbhit()
    { var boolean result;
      __asm__ __volatile__ ("pushl %%ebx ; "
                            "movb $0x11,%%ah ; .byte 0xcd ; .byte 0x16 ; "
                            "movl $0,%%eax ; jz 1f ; incl %%eax ; 1: "
                            "popl %%ebx"
                            : "=a" /* %eax */ (result) /* OUT */
                            :                          /* IN */
                            : "cx","dx","si","di" /* %ecx,%edx,%esi,%edi */ /* CLOBBER */
                           );
      return result;
    }
  local uintW getch()
    { var uintW ch;
      __asm__ __volatile__ ("pushl %%ebx ; "
                            "movb $0x10,%%ah ; .byte 0xcd ; .byte 0x16 ; "
                            "popl %%ebx"
                            : "=a" /* %ax */ (ch)      /* OUT */
                            :                          /* IN */
                            : "cx","dx","si","di" /* %ecx,%edx,%esi,%edi */ /* CLOBBER */
                           );
      return ch;
    }

#endif

  # Tabelle der Characters, die den Scan-Codes 0..166 (als Sondertasten)
  # entsprechen:
  local const key_event scancode_table [167] =
    { { NULL, 0, 0, },
      { NULL, ESC, char_meta_c }, # 1 -> Alt-Escape
      { NULL, '1', char_control_c }, # [2 = Ctrl-1 -> #\CONTROL-1]
      { NULL, '2', char_control_c }, # 3 = Ctrl-2 -> #\CONTROL-2
      { NULL, '3', char_control_c }, # [4 = Ctrl-3 -> #\CONTROL-3]
      { NULL, '4', char_control_c }, # [5 = Ctrl-4 -> #\CONTROL-4]
      { NULL, '5', char_control_c }, # [6 = Ctrl-5 -> #\CONTROL-5]
      { NULL, '6', char_control_c }, # 7 = Ctrl-6 -> #\CONTROL-6
      { NULL, '7', char_control_c }, # [8 = Ctrl-7 -> #\CONTROL-7]
      { NULL, '8', char_control_c }, # [9 = Ctrl-8 -> #\CONTROL-8]
      { NULL, '9', char_control_c }, # [10 = Ctrl-9 -> #\CONTROL-9]
      { NULL, '0', char_control_c }, # [11 = Ctrl-0 -> #\CONTROL-0]
      { NULL, '-', char_meta_c }, # [12 = Ctrl-- -> #\CONTROL-- # nicht international portabel]
      { NULL, '=', char_meta_c }, # [13 = Ctrl-= -> #\CONTROL-= # nicht international portabel]
      { NULL,  BS, char_meta_c }, # 14 -> Alt-Backspace
      { NULL,   9, char_super_c }, # 15 -> Shift-Tab
      { NULL, 'Q', char_meta_c }, # 16 -> Alt-Q
      { NULL, 'W', char_meta_c }, # 17 -> Alt-W
      { NULL, 'E', char_meta_c }, # 18 -> Alt-E
      { NULL, 'R', char_meta_c }, # 19 -> Alt-R
      { NULL, 'T', char_meta_c }, # 20 -> Alt-T
      { NULL, 'Y', char_meta_c }, # 21 -> Alt-Y
      { NULL, 'U', char_meta_c }, # 22 -> Alt-U
      { NULL, 'I', char_meta_c }, # 23 -> Alt-I
      { NULL, 'O', char_meta_c }, # 24 -> Alt-O
      { NULL, 'P', char_meta_c }, # 25 -> Alt-P
      { NULL, '[', char_meta_c }, # 26 -> Alt-[ # nicht international portabel
      { NULL, ']', char_meta_c }, # 27 -> Alt-] # nicht international portabel
      { NULL,  CR, char_meta_c }, # 28 = Alt-Return -> #\META-Return
      { NULL, 0, 0 },
      { NULL, 'A', char_meta_c }, # 30 -> Alt-A
      { NULL, 'S', char_meta_c }, # 31 -> Alt-S
      { NULL, 'D', char_meta_c }, # 32 -> Alt-D
      { NULL, 'F', char_meta_c }, # 33 -> Alt-F
      { NULL, 'G', char_meta_c }, # 34 -> Alt-G
      { NULL, 'H', char_meta_c }, # 35 -> Alt-H
      { NULL, 'J', char_meta_c }, # 36 -> Alt-J
      { NULL, 'K', char_meta_c }, # 37 -> Alt-K
      { NULL, 'L', char_meta_c }, # 38 -> Alt-L oder Alt-\ ??
      { NULL, ';', char_meta_c }, # 39 -> Alt-; # nicht international portabel
      { NULL, '\'', char_meta_c }, # 40 -> Alt-' # nicht international portabel
      { NULL, '`', char_meta_c }, # 41 -> Alt-` # nicht international portabel
      { NULL, 0, 0 },
      { NULL, '\\', char_meta_c }, # 43 -> Alt-\ # nicht international portabel
      { NULL, 'Z', char_meta_c }, # 44 -> Alt-Z
      { NULL, 'X', char_meta_c }, # 45 -> Alt-X
      { NULL, 'C', char_meta_c }, # 46 -> Alt-C
      { NULL, 'V', char_meta_c }, # 47 -> Alt-V
      { NULL, 'B', char_meta_c }, # 48 -> Alt-B
      { NULL, 'N', char_meta_c }, # 49 -> Alt-N
      { NULL, 'M', char_meta_c }, # 50 -> Alt-M
      { NULL, ',', char_meta_c }, # 51 = Alt-, -> #\META-',' # nicht international portabel
      { NULL, '.', char_meta_c }, # 52 = Alt-. -> #\META-'.' # nicht international portabel
      { NULL, '/', char_meta_c }, # 53 = Alt-/ -> #\META-'/' # nicht international portabel
      { NULL, 0, 0 },
      { NULL, '*', char_meta_c | char_hyper_c }, # 55 = Alt-* -> #\META-HYPER-'*'
      { NULL, 0, 0 },
      { NULL, ' ', char_meta_c }, # 57 = Alt-Space -> #\META-Space
      { NULL, 0, 0 },
      { "F1", 0, char_hyper_c }, #  59 = F1 -> #\F1
      { "F2", 0, char_hyper_c }, #  60 = F2 -> #\F2
      { "F3", 0, char_hyper_c }, #  61 = F3 -> #\F3
      { "F4", 0, char_hyper_c }, #  62 = F4 -> #\F4
      { "F5", 0, char_hyper_c }, #  63 = F5 -> #\F5
      { "F6", 0, char_hyper_c }, #  64 = F6 -> #\F6
      { "F7", 0, char_hyper_c }, #  65 = F7 -> #\F7
      { "F8", 0, char_hyper_c }, #  66 = F8 -> #\F8
      { "F9", 0, char_hyper_c }, #  67 = F9 -> #\F9
      { "F10", 0, char_hyper_c }, #  68 = F10 -> #\F10
      { "F11", 0, char_hyper_c }, # [69 = F11 -> #\F11
      { "F12", 0, char_hyper_c }, # [70 = F12 -> #\F12
      { "HOME", 0, char_hyper_c }, #  71 = Home -> #\Home
      { "UP", 0, char_hyper_c }, #  72 = Up -> #\Up
      { "PGUP", 0, char_hyper_c }, #  73 = PgUp -> #\PgUp
      { NULL, '-', char_meta_c | char_hyper_c }, #  74 = Alt-- -> #\META-HYPER--
      { "LEFT", 0, char_hyper_c }, #  75 = Left -> #\Left
      { "CENTER", 0, char_hyper_c }, # [76 -> #\HYPER-Code21]
      { "RIGHT", 0, char_hyper_c }, #  77 = Right -> #\Right
      { NULL, '+', char_meta_c | char_hyper_c }, #  78 = Alt-+ -> #\META-HYPER-+
      { "END", 0, char_hyper_c }, #  79 = End -> #\End
      { "DOWN", 0, char_hyper_c }, #  80 = Down -> #\Down
      { "PGDN", 0, char_hyper_c }, #  81 = PgDn -> #\PgDn
      { "INSERT", 0, char_hyper_c }, #  82 = Insert -> #\Insert
      { "DELETE", 0, char_hyper_c }, #  83 = Delete -> #\Delete
      { "F1", 0, char_super_c | char_hyper_c }, #  84 = Shift-F1 -> #\S-F1
      { "F2", 0, char_super_c | char_hyper_c }, #  85 = Shift-F2 -> #\S-F2
      { "F3", 0, char_super_c | char_hyper_c }, #  86 = Shift-F3 -> #\S-F3
      { "F4", 0, char_super_c | char_hyper_c }, #  87 = Shift-F4 -> #\S-F4
      { "F5", 0, char_super_c | char_hyper_c }, #  88 = Shift-F5 -> #\S-F5
      { "F6", 0, char_super_c | char_hyper_c }, #  89 = Shift-F6 -> #\S-F6
      { "F7", 0, char_super_c | char_hyper_c }, #  90 = Shift-F7 -> #\S-F7
      { "F8", 0, char_super_c | char_hyper_c }, #  91 = Shift-F8 -> #\S-F8
      { "F9", 0, char_super_c | char_hyper_c }, #  92 = Shift-F9 -> #\S-F9
      { "F10", 0, char_super_c | char_hyper_c }, #  93 = Shift-F10 -> #\S-F10
      { "F1", 0, char_control_c | char_hyper_c }, #  94 = Control-F1 -> #\C-F1
      { "F2", 0, char_control_c | char_hyper_c }, #  95 = Control-F2 -> #\C-F2
      { "F3", 0, char_control_c | char_hyper_c }, #  96 = Control-F3 -> #\C-F3
      { "F4", 0, char_control_c | char_hyper_c }, #  97 = Control-F4 -> #\C-F4
      { "F5", 0, char_control_c | char_hyper_c }, #  98 = Control-F5 -> #\C-F5
      { "F6", 0, char_control_c | char_hyper_c }, #  99 = Control-F6 -> #\C-F6
      { "F7", 0, char_control_c | char_hyper_c }, #  100 = Control-F7 -> #\C-F7
      { "F8", 0, char_control_c | char_hyper_c }, #  101 = Control-F8 -> #\C-F8
      { "F9", 0, char_control_c | char_hyper_c }, #  102 = Control-F9 -> #\C-F9
      { "F10", 0, char_control_c | char_hyper_c }, #  103 = Control-F10 -> #\C-F10
      { "F1", 0, char_meta_c | char_hyper_c }, #  104 = Alt-F1 -> #\M-F1
      { "F2", 0, char_meta_c | char_hyper_c }, #  105 = Alt-F2 -> #\M-F2
      { "F3", 0, char_meta_c | char_hyper_c }, #  106 = Alt-F3 -> #\M-F3
      { "F4", 0, char_meta_c | char_hyper_c }, #  107 = Alt-F4 -> #\M-F4
      { "F5", 0, char_meta_c | char_hyper_c }, #  108 = Alt-F5 -> #\M-F5
      { "F6", 0, char_meta_c | char_hyper_c }, #  109 = Alt-F6 -> #\M-F6
      { "F7", 0, char_meta_c | char_hyper_c }, #  110 = Alt-F7 -> #\M-F7
      { "F8", 0, char_meta_c | char_hyper_c }, #  111 = Alt-F8 -> #\M-F8
      { "F9", 0, char_meta_c | char_hyper_c }, #  112 = Alt-F9 -> #\M-F9
      { "F10", 0, char_meta_c | char_hyper_c }, #  113 = Alt-F10 -> #\M-F10
      { "PRTSCR", 0, char_control_c | char_hyper_c }, # 114 = Control-PrtScr -> #\C-PrtScr
      { "LEFT", 0, char_control_c | char_hyper_c }, # 115 = Control-Left -> #\C-Left
      { "RIGHT", 0, char_control_c | char_hyper_c }, # 116 = Control-Right -> #\C-Right
      { "END", 0, char_control_c | char_hyper_c }, # 117 = Control-End -> #\C-End
      { "PGDN", 0, char_control_c | char_hyper_c }, # 118 = Control-PgDn -> #\C-PgDn
      { "HOME", 0, char_control_c | char_hyper_c }, # 119 = Control-Home -> #\C-Home
      { NULL, '1', char_meta_c }, #  120 = Alt-1 -> #\META-1
      { NULL, '2', char_meta_c }, #  121 = Alt-2 -> #\META-2
      { NULL, '3', char_meta_c }, #  122 = Alt-3 -> #\META-3
      { NULL, '4', char_meta_c }, #  123 = Alt-4 -> #\META-4
      { NULL, '5', char_meta_c }, #  124 = Alt-5 -> #\META-5
      { NULL, '6', char_meta_c }, #  125 = Alt-6 -> #\META-6
      { NULL, '7', char_meta_c }, #  126 = Alt-7 -> #\META-7
      { NULL, '8', char_meta_c }, #  127 = Alt-8 -> #\META-8
      { NULL, '9', char_meta_c }, #  128 = Alt-9 -> #\META-9
      { NULL, '0', char_meta_c }, #  129 = Alt-0 -> #\META-0
      { NULL, '-', char_meta_c }, #  130 = Alt-- -> #\META-- # nicht international portabel
      { NULL, '=', char_meta_c }, #  131 = Alt-= -> #\META-= # nicht international portabel
      { "PGUP", 0, char_control_c | char_hyper_c }, # 132 = Control-PgUp -> #\C-PgUp
      { "F11", 0, char_hyper_c }, #  133 = F11 -> #\F11
      { "F12", 0, char_hyper_c }, #  134 = F12 -> #\F12
      { "F11", 0, char_super_c | char_hyper_c }, #  135 = Shift-F11 -> #\S-F11
      { "F12", 0, char_super_c | char_hyper_c }, #  136 = Shift-F12 -> #\S-F12
      { "F11", 0, char_control_c | char_hyper_c }, #  137 = Control-F11 -> #\C-F11
      { "F12", 0, char_control_c | char_hyper_c }, #  138 = Control-F12 -> #\C-F12
      { "F11", 0, char_meta_c | char_hyper_c }, #  139 = Alt-F1 -> #\M-F11
      { "F12", 0, char_meta_c | char_hyper_c }, #  140 = Alt-F2 -> #\M-F12
      { "UP", 0, char_control_c | char_hyper_c }, # 141 = Control-Up -> #\C-Up
      { NULL, '-', char_control_c | char_hyper_c }, # 142 = Control-- -> #\CONTROL-HYPER--
      { "CENTER", 0, char_control_c | char_hyper_c }, # 143 = Control-Keypad5 -> #\C-Center
      { NULL, '+', char_control_c | char_hyper_c }, # 142 = Control-+ -> #\CONTROL-HYPER-+
      { "DOWN", 0, char_control_c | char_hyper_c }, # 145 = Control-Down -> #\C-Down
      { "INSERT", 0, char_control_c | char_hyper_c }, # 146 = Control-Insert -> #\C-Insert
      { "DELETE", 0, char_control_c | char_hyper_c }, # 147 = Control-Delete -> #\C-Delete
      { NULL,   9, char_control_c }, # 148 = Control-Tab -> #\CONTROL-Tab
      { NULL, '/', char_control_c | char_hyper_c }, # 149 = Control-/ -> #\CONTROL-HYPER-'/'
      { NULL, '*', char_control_c | char_hyper_c }, # 150 = Control-* -> #\CONTROL-HYPER-'*'
      { "HOME", 0, char_meta_c | char_hyper_c }, # 151 = Alt-Home -> #\M-Home
      { "UP", 0, char_meta_c | char_hyper_c }, # 152 = Alt-Up -> #\M-Up
      { "PGUP", 0, char_meta_c | char_hyper_c }, # 153 = Alt-PgUp -> #\M-PgUp
      { NULL, 0, 0 },
      { "LEFT", 0, char_meta_c | char_hyper_c }, # 155 = Alt-Left -> #\M-Left
      { "CENTER", 0, char_meta_c | char_hyper_c }, # [156 -> #\META-Center]
      { "RIGHT", 0, char_meta_c | char_hyper_c }, # 157 = Alt-Right -> #\M-Right
      { NULL, 0, 0 },
      { "END", 0, char_meta_c | char_hyper_c }, # 159 = Alt-End -> #\M-End
      { "DOWN", 0, char_meta_c | char_hyper_c }, # 160 = Alt-Down -> #\M-Down
      { "PGDN", 0, char_meta_c | char_hyper_c }, # 161 = Alt-PgDn -> #\M-PgDn
      { "INSERT", 0, char_meta_c | char_hyper_c }, # 162 = Alt-Insert -> #\M-Insert
      { "DELETE", 0, char_meta_c | char_hyper_c }, # 163 = Alt-Delete -> #\M-Delete
      { NULL, '/', char_meta_c | char_hyper_c }, # 164 = Alt-/ -> #\META-HYPER-'/'
      { NULL,   9, char_meta_c }, # 165 = Alt-Tab -> #\META-Tab
      { NULL,  CR, char_meta_c | char_hyper_c }, # 166 = Alt-Enter -> #\META-HYPER-Return
    };

#ifdef EMUNIX_PORTABEL

# Wir haben, um portabel zu bleiben, nur die Funktion _read_kbd zur Verfügung.
# Diese erkennt unter DOS aber nur recht wenige Sondertasten: nur die mit
# Scan-Codes 3, 7, 15-25, 30-38, 44-50, 59-131 (ungefähr).
# Insbesondere fehlen F11, F12, Ctrl-Up, Ctrl-Down, und man kann
# Enter von Return, Tab von Ctrl-I, Backspace von Ctrl-H nicht unterscheiden.
# Trotzdem!
# Da INT 16,10 unter DOS seit emx 0.8f nun endlich befriedigend funktioniert,
# verwenden wir dieses. Zur Laufzeit wird _osmode abgefragt.

#endif # EMUNIX_PORTABEL

#endif # MSDOS

# Stellt fest, ob der Keyboard-Stream ein Zeichen verfügbar hat.
# listen_keyboard(stream)
# > stream: Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_keyboard (object stream);
  #ifdef MSDOS
  local signean listen_keyboard(stream)
    var object stream;
    {
      #ifdef EMUNIX_PORTABEL
      if (!(_osmode == DOS_MODE))
        # OS/2
        { var int ch = _read_kbd(FALSE,FALSE,FALSE);
          if (ch < 0) { return signean_plus; } # nein
          pushSTACK(stream);
         {var object c;
          if (ch==0)
            { c = make_key_event(&scancode_table[(uintB)_read_kbd(FALSE,TRUE,FALSE)]); }
          elif ((ch <= 26) && !(ch == BS) && !(ch == CR) && !(ch == TAB))
            { # Ctrl-A bis Ctrl-Z -> Buchstabe mit CONTROL-Bit draus machen:
              var key_event event;
              event.key = NULL;
              event.code = (ch==LF ? CR : (ch | bit(6)));
              event.bits = char_control_c;
              c = make_key_event(&event);
            }
          else
            { pushSTACK(code_char(ch)); funcall(S(make_char),1); c = value1; }
          stream = popSTACK();
          TheStream(stream)->strm_rd_ch_last = c;
          TheStream(stream)->strmflags |= strmflags_unread_B;
          return signean_null;
        }}
        else
      #endif
      # DOS
      if (kbhit()) # inzwischen wieder Tasten gedrückt?
        { return signean_null; } # ja
        else
        { return signean_plus; } # nein
    }
  #endif
  #ifdef WIN32_NATIVE
  local signean listen_keyboard(stream)
    var object stream;
    { var Handle handle = TheHandle(TheStream(stream)->strm_keyboard_handle);
      # See the implementation of listen_handle() for consoles.
      var DWORD nevents;
      begin_system_call();
      if (!GetNumberOfConsoleInputEvents(handle,&nevents)) { OS_error(); }
      # It's a console.
      if (nevents==0)
        # kein Zeichen verfügbar
        { end_system_call(); return signean_plus; }
     {var INPUT_RECORD* events = (INPUT_RECORD*)alloca(nevents*sizeof(INPUT_RECORD));
      var DWORD nevents_read;
      if (!PeekConsoleInput(handle,events,nevents,&nevents_read))
        { OS_error(); }
      if (nevents_read==0)
        # kein Zeichen verfügbar
        { end_system_call(); return signean_plus; }
      { # Look out for any Key-Down event.
        var DWORD i;
        for (i = 0; i < nevents_read; i++)
          { if (events[i].EventType == KEY_EVENT
                && events[i].Event.KeyEvent.bKeyDown
                && events[i].Event.KeyEvent.uAsciiChar != 0)
              # Zeichen verfügbar
              { end_system_call(); return signean_null; }
      }   }
      # kein Zeichen verfügbar
      end_system_call(); return signean_plus;
    }}
  #endif
  #if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)
    #define listen_keyboard  listen_handle
  #endif
  #if defined(NEXTAPP)
    #define listen_keyboard(stream)  (stream, signean_minus)
  #endif

# UP: Löscht bereits eingegebenen interaktiven Input vom Keyboard-Stream.
# clear_input_keyboard(stream);
# > stream: Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_keyboard (object stream);
  local boolean clear_input_keyboard(stream)
    var object stream;
    {
      #ifdef MSDOS
        #ifdef EMUNIX_PORTABEL
        if (!(_osmode == DOS_MODE))
          # OS/2
           { while (listen_keyboard(stream)) { /* das Zeichen wurde schon geholt! */ } }
          else
        #endif
        # DOS
        while (kbhit()) { getch(); }
      #endif
      #ifdef WIN32_NATIVE
        clear_tty_input(TheHandle(TheStream(stream)->strm_keyboard_handle));
        pushSTACK(stream);
        while (listen_keyboard(STACK_0) == 0) { read_char(&STACK_0); }
        skipSTACK(1);
      #endif
      #if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)
        if (nullp(TheStream(stream)->strm_keyboard_isatty))
          # File -> nichts tun
          { return FALSE; }
        # Terminal
        TheStream(stream)->strm_rd_ch_last = NIL; # gewesenes EOF vergessen
        clear_tty_input(stdin_handle);
        pushSTACK(stream);
        while (listen_keyboard(STACK_0) == 0) { read_char(&STACK_0); }
        skipSTACK(1);
      #endif
      return TRUE;
    }

# Lesen eines Zeichens vom Keyboard:
  local object rd_ch_keyboard (const object* stream_);

  #ifdef MSDOS
  local object rd_ch_keyboard(stream_)
    var const object* stream_;
    {
      #ifdef EMUNIX_PORTABEL
      if (!(_osmode == DOS_MODE))
        # OS/2
        { run_time_stop(); # Run-Time-Stoppuhr anhalten
         {var object c;
          var int ch = _read_kbd(FALSE,TRUE,FALSE);
          if (ch==0)
            { c = make_key_event(&scancode_table[(uintB)_read_kbd(FALSE,TRUE,FALSE)]); }
          elif ((ch <= 26) && !(ch == BS) && !(ch == CR) && !(ch == TAB))
            { # Ctrl-A bis Ctrl-Z -> Buchstabe mit CONTROL-Bit draus machen:
              var key_event event;
              event.key = NULL;
              event.code = (ch==LF ? CR : (ch | bit(6)));
              event.bits = char_control_c;
              c = make_key_event(&event);
            }
          else
            { pushSTACK(code_char(ch)); funcall(S(make_char),1); c = value1; }
          # noch zu behandeln: ??
          # Ctrl-2 -> #\Control-2, Ctrl-6 -> #\Code30, Ctrl-ß -> #\Code28,
          # Ctrl-+ -> #\Code29, Ctrl-ü -> #\Code27 = #\Escape
          run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
          return c;
        }}
        else
      #endif
      # DOS
     {var object c;
      run_time_stop(); # Run-Time-Stoppuhr anhalten
      { # Tastendruck abwarten, nichts ausgeben:
        var uintW erg = getch();
        var uintB code = (uintB)erg; # Ascii-Code
        var uintB scancode = (uintB)(erg>>8); # Scan-Code
        if (scancode == 0)
          # Multikey-Event, z.B. accent+space oder Alt xyz
          { pushSTACK(code_char(code)); funcall(S(make_char),1); c = value1; }
        else
          { if ((code == 0) || (code == 0xE0))
              # Sondertaste
              { if (scancode < 167)
                  { c = make_key_event(&scancode_table[scancode]); }
                  else
                  { var key_event event = { NULL, 0, 0 };
                    c = make_key_event(&event);
              }   }
              else
              { if (((scancode >= 71) && (scancode < 84)) || (scancode == 55)
                    || ((scancode == 0xE0) && (code >= 32))
                   )
                  # Ziffernblocktaste außer Enter (auch nicht F1 bis F12 !)
                  { var key_event event;
                    event.key = NULL;
                    event.code = code;
                    event.bits = char_hyper_c;
                    c = make_key_event(&event);
                  }
                elif ((scancode == 14) || (scancode == 28)
                      || ((scancode == 0xE0) && (code < 32))
                     )
                  # Backspace-Taste, Return-Taste, Enter-Taste
                  { var uintB defaultcode = (scancode==14 ? BS : CR);
                    var key_event event;
                    event.key = NULL;
                    event.code = defaultcode;
                    event.bits = (scancode == 0xE0 ? char_hyper_c : 0)
                                 | (!(code == defaultcode) ? char_control_c : 0);
                    c = make_key_event(&event);
                  }
                else
                  { if ((code < 32) && ((scancode >= 16) && (scancode <= 53)))
                      { # Ctrl-A bis Ctrl-Z -> Buchstabe mit CONTROL-Bit draus machen:
                        var key_event event;
                        event.key = NULL;
                        event.code = code | bit(6);
                        event.bits = char_control_c;
                        c = make_key_event(&event);
                      }
                    else
                      # normales Zeichen
                      { pushSTACK(code_char(code)); funcall(S(make_char),1); c = value1; }
          }   }   }
        # noch zu behandeln: ??
        # Ctrl-2          0300
        # Ctrl-6          071E
        # Ctrl-ß          0C1C
        # Ctrl--          0C1F
      }
      run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
      return c;
    }}
  #endif

  #ifdef WIN32_NATIVE
  local object rd_ch_keyboard(stream_)
    var const object* stream_;
    { var INPUT_RECORD event;
      var DWORD nevents_read;
      var Handle handle;
     restart_it:
      handle = TheHandle(TheStream(*stream_)->strm_keyboard_handle);
      begin_system_call();
      loop
        { if (!ReadConsoleInput1(handle,&event,&nevents_read))
            { if (GetLastError()==ERROR_SIGINT) # Unterbrechung durch Ctrl-C ?
                { end_system_call();
                  pushSTACK(S(read_char)); tast_break(); # Break-Schleife aufrufen
                  goto restart_it;
                }
              OS_error();
            }
          ASSERT(nevents_read==1);
          if (event.EventType == KEY_EVENT
              && event.Event.KeyEvent.bKeyDown)
            { var key_event ev;
              if (event.Event.KeyEvent.wRepeatCount > 1)
                { var DWORD nevents_written;
                  event.Event.KeyEvent.wRepeatCount--;
                  if (!WriteConsoleInput(handle,&event,1,&nevents_written))
                    { OS_error(); }
                }
              if (event.Event.KeyEvent.uAsciiChar <= ' ')
                { # Translate Virtual Keycode.
                  local struct { WORD vkcode; key_event myevent; } vktable[] =
                    { VK_BACK,    { NULL,  BS, 0 },               # #\Backspace
                      VK_TAB,     { NULL, TAB, 0 },               # #\Tab
                      VK_RETURN,  { NULL,  CR, 0 },               # #\Return
                      VK_ESCAPE,  { NULL, ESC, 0 },               # #\Escape
                      VK_LEFT,    { "LEFT", 0, char_hyper_c },    # #\Left
                      VK_RIGHT,   { "RIGHT", 0, char_hyper_c },   # #\Right
                      VK_UP,      { "UP", 0, char_hyper_c },      # #\Up
                      VK_DOWN,    { "DOWN", 0, char_hyper_c },    # #\Down
                      VK_PRIOR,   { "PGUP", 0, char_hyper_c },    # #\PgUp
                      VK_NEXT,    { "PGDN", 0, char_hyper_c },    # #\PgDn
                      VK_HOME,    { "HOME", 0, char_hyper_c },    # #\Home
                      VK_END,     { "END", 0, char_hyper_c },     # #\End
                      VK_INSERT,  { "INSERT", 0, char_hyper_c },  # #\Insert
                      VK_DELETE,  { "DELETE", 0, char_hyper_c },  # #\Delete
                      12,         { "CENTER", 0, char_hyper_c },  # #\Center
                      VK_F1,      { "F1", 0, char_hyper_c },      # #\F1
                      VK_F2,      { "F2", 0, char_hyper_c },      # #\F2
                      VK_F3,      { "F3", 0, char_hyper_c },      # #\F3
                      VK_F4,      { "F4", 0, char_hyper_c },      # #\F4
                      VK_F5,      { "F5", 0, char_hyper_c },      # #\F5
                      VK_F6,      { "F6", 0, char_hyper_c },      # #\F6
                      VK_F7,      { "F7", 0, char_hyper_c },      # #\F7
                      VK_F8,      { "F8", 0, char_hyper_c },      # #\F8
                      VK_F9,      { "F9", 0, char_hyper_c },      # #\F9
                      VK_F10,     { "F10", 0, char_hyper_c },     # #\F10
                      VK_F11,     { "F11", 0, char_hyper_c },     # #\F11
                      VK_F12,     { "F12", 0, char_hyper_c },     # #\F12
                      ' ',        { NULL, ' ', 0 },               # #\Space
                      '0',        { NULL, '0', 0 },               # #\0
                      '1',        { NULL, '1', 0 },               # #\1
                      '2',        { NULL, '2', 0 },               # #\2
                      '3',        { NULL, '3', 0 },               # #\3
                      '4',        { NULL, '4', 0 },               # #\4
                      '5',        { NULL, '5', 0 },               # #\5
                      '6',        { NULL, '6', 0 },               # #\6
                      '7',        { NULL, '7', 0 },               # #\7
                      '8',        { NULL, '8', 0 },               # #\8
                      '9',        { NULL, '9', 0 },               # #\9
                      'A',        { NULL, 'A', 0 },               # #\A
                      'B',        { NULL, 'B', 0 },               # #\B
                      'C',        { NULL, 'C', 0 },               # #\C
                      'D',        { NULL, 'D', 0 },               # #\D
                      'E',        { NULL, 'E', 0 },               # #\E
                      'F',        { NULL, 'F', 0 },               # #\F
                      'G',        { NULL, 'G', 0 },               # #\G
                      'H',        { NULL, 'H', 0 },               # #\H
                      'I',        { NULL, 'I', 0 },               # #\I
                      'J',        { NULL, 'J', 0 },               # #\J
                      'K',        { NULL, 'K', 0 },               # #\K
                      'L',        { NULL, 'L', 0 },               # #\L
                      'M',        { NULL, 'M', 0 },               # #\M
                      'N',        { NULL, 'N', 0 },               # #\N
                      'O',        { NULL, 'O', 0 },               # #\O
                      'P',        { NULL, 'P', 0 },               # #\P
                      'Q',        { NULL, 'Q', 0 },               # #\Q
                      'R',        { NULL, 'R', 0 },               # #\R
                      'S',        { NULL, 'S', 0 },               # #\S
                      'T',        { NULL, 'T', 0 },               # #\T
                      'U',        { NULL, 'U', 0 },               # #\U
                      'V',        { NULL, 'V', 0 },               # #\V
                      'W',        { NULL, 'W', 0 },               # #\W
                      'X',        { NULL, 'X', 0 },               # #\X
                      'Y',        { NULL, 'Y', 0 },               # #\Y
                      'Z',        { NULL, 'Z', 0 },               # #\Z
                      107,        { NULL, '+', char_hyper_c },    # #\HYPER-+
                      109,        { NULL, '-', char_hyper_c },    # #\HYPER--
                      106,        { NULL, '*', char_hyper_c },    # #\HYPER-*
                      111,        { NULL, '/', char_hyper_c },    # #\HYPER-/
                      186,        { NULL, ';', 0 },               # #\;
                      187,        { NULL, '=', 0 },               # #\=
                      188,        { NULL, ',', 0 },               # #\,
                      189,        { NULL, '-', 0 },               # #\-
                      190,        { NULL, '.', 0 },               # #\.
                      191,        { NULL, '/', 0 },               # #\/
                      192,        { NULL, '`', 0 },               # #\`
                      219,        { NULL, '[', 0 },               # #\[
                      220,        { NULL, '\\', 0 },              # #\\
                      221,        { NULL, ']', 0 },               # #\]
                      222,        { NULL, '\'', 0 },              # #\'
                    };
                  var int i;
                  for (i = 0; i < sizeof(vktable)/sizeof(vktable[0]); i++)
                    { if (event.Event.KeyEvent.wVirtualKeyCode == vktable[i].vkcode)
                        { ev = vktable[i].myevent; goto found_keycode; }
                    }
                  switch (event.Event.KeyEvent.wVirtualKeyCode)
                    { case VK_SHIFT:
                      case VK_CONTROL:
                      case 18: case 20:
                        break;
                      default:
                        asciz_out_3("Unknown keyboard event, VKeyCode = %d, VScanCode = %d, AsciiChar = %d\n",event.Event.KeyEvent.wVirtualKeyCode,event.Event.KeyEvent.wVirtualScanCode,event.Event.KeyEvent.uAsciiChar);
                    }
                  continue;
                  found_keycode:
                  if (event.Event.KeyEvent.dwControlKeyState & SHIFT_PRESSED)
                    { ev.bits |= char_super_c; }
                  if (event.Event.KeyEvent.dwControlKeyState & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED))
                    { ev.bits |= char_control_c; }
                  if (event.Event.KeyEvent.dwControlKeyState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED))
                    { ev.bits |= char_meta_c; }
                }
                else
                { ev.key = NULL;
                  ev.code = (uintB)event.Event.KeyEvent.uAsciiChar;
                  ev.bits = 0;
                  if (event.Event.KeyEvent.dwControlKeyState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED))
                    { # c = 'a'..'z' -> translate to 'A'..'Z'
                      # c = 'A'..'Z' -> add "Shift"
                      # c = '<','>' etc. -> don't add "Shift"
                      ev.code = up_case(ev.code);
                      if (!(ev.code == down_case(ev.code)))
                        { if (event.Event.KeyEvent.dwControlKeyState & SHIFT_PRESSED)
                            { ev.bits |= char_super_c; }
                        }
                      ev.bits |= char_meta_c;
                }   }
              end_system_call();
              return make_key_event(&ev);
            }
          # Other events are silently thrown away.
        }
    }
  #endif

  #if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)

  # vgl. rd_ch_handle() :
  local object rd_ch_keyboard(stream_)
    var const object* stream_;
    { restart_it:
     {var object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF?
        { return eof_value; }
      # Noch etwas im Buffer?
      if (mconsp(TheStream(stream)->strm_keyboard_buffer))
        goto empty_buffer;
      # Ein Zeichen lesen:
      { var uintB c;
        read_next_char:
        {run_time_stop(); # Run-Time-Stoppuhr anhalten
         begin_system_call();
         {var int ergebnis = read(stdin_handle,&c,1); # Zeichen lesen versuchen
          end_system_call();
          run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
          if (ergebnis<0)
            { begin_system_call();
              if (errno==EINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
                { end_system_call();
                  interruptp({ pushSTACK(S(read_char)); tast_break(); }); # Break-Schleife aufrufen
                  goto restart_it;
                }
              OS_error();
            }
          if (ergebnis==0)
            # kein Zeichen verfügbar -> EOF erkennen
            { TheStream(stream)->strm_rd_ch_last = eof_value; return eof_value; }
        }}
        next_char_is_read:
        # Es verlängert den Buffer:
        {var object new_cons = allocate_cons();
         Car(new_cons) = code_char(c);
         stream = *stream_;
         {var object* last_ = &TheStream(stream)->strm_keyboard_buffer;
          while (mconsp(*last_)) { last_ = &Cdr(*last_); }
          *last_ = new_cons;
        }}
        # Ist der Buffer eine vollständige Zeichenfolge zu einer Taste,
        # so liefern wir diese Taste. Ist der Buffer ein echtes Anfangsstück
        # einer Zeichenfolge zu einer Taste, so warten wir noch ein wenig.
        # Ansonsten fangen wir an, den Buffer Zeichen für Zeichen zu leeren.
        { var object keytab = TheStream(stream)->strm_keyboard_keytab;
          while (consp(keytab))
            { var object L1 = Car(keytab);
              keytab = Cdr(keytab);
             {var object L2 = TheStream(stream)->strm_keyboard_buffer;
              while (consp(L1) && consp(L2) && eq(Car(L1),Car(L2)))
                { L1 = Cdr(L1); L2 = Cdr(L2); }
              if (atomp(L2))
                { if (atomp(L1))
                    # vollständige Zeichenfolge
                    { TheStream(stream)->strm_keyboard_buffer = NIL;
                      return L1;
        }   }}  }   }
        { var object keytab = TheStream(stream)->strm_keyboard_keytab;
          while (consp(keytab))
            { var object L1 = Car(keytab);
              keytab = Cdr(keytab);
             {var object L2 = TheStream(stream)->strm_keyboard_buffer;
              while (consp(L1) && consp(L2) && eq(Car(L1),Car(L2)))
                { L1 = Cdr(L1); L2 = Cdr(L2); }
              if (atomp(L2))
                # Da consp(L1), liegt ein Anfangsstück einer Zeichenfolge vor.
                goto wait_for_another;
        }   }}
        goto empty_buffer;
        wait_for_another:
        #ifdef HAVE_SELECT
        { # Verwende select mit readfds = einelementige Menge {stdin_handle}
          # und timeout = kleines Zeitintervall.
          var fd_set handle_menge; # Menge von Handles := {stdin_handle}
          var struct timeval small_time; # Zeitintervall := 0
          FD_ZERO(&handle_menge); FD_SET(stdin_handle,&handle_menge);
          restart_select:
          small_time.tv_sec = 0; small_time.tv_usec = 1000000/10; # 1/10 sec
          run_time_stop(); # Run-Time-Stoppuhr anhalten
          begin_system_call();
         {var int ergebnis;
          ergebnis = select(FD_SETSIZE,&handle_menge,NULL,NULL,&small_time);
          end_system_call();
          run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
          if (ergebnis<0)
            { begin_system_call();
              if (errno==EINTR) { end_system_call(); goto restart_select; }
              if (!(errno == EBADF)) { OS_error(); }
              end_system_call();
            }
            else
            { # ergebnis = Anzahl der Handles in handle_menge, bei denen read
              # sofort ein Ergebnis liefern würde.
              if (ergebnis==0)
                goto empty_buffer; # kein Zeichen verfügbar
              # ergebnis=1 -> Zeichen verfügbar
        }}  }
        #else
        #if defined(UNIX_TERM_TERMIOS) || defined(UNIX_TERM_TERMIO)
        { # Verwende die Termio-Elemente VMIN und VTIME.
          #ifdef UNIX_TERM_TERMIOS
          var struct termios oldtermio;
          var struct termios newtermio;
          #else # UNIX_TERM_TERMIO
          var struct termio oldtermio;
          var struct termio newtermio;
          #endif
          run_time_stop(); # Run-Time-Stoppuhr anhalten
          begin_system_call();
          #ifdef UNIX_TERM_TERMIOS
          if (!( tcgetattr(stdin_handle,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #else
          if (!( ioctl(stdin_handle,TCGETA,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #endif
          # Wir gehen nun davon aus, dass oldtermio nun mit dem newtermio aus
          # term_raw() (s.u.) identisch ist. Das ist dann gewährleistet, wenn
          # 1. (SYS::TERMINAL-RAW T) aufgerufen wurde und
          # 2. stdin_handle und stdout_handle beide dasselbe Terminal sind. ??
          newtermio = oldtermio;
          newtermio.c_cc[VMIN] = 0;
          newtermio.c_cc[VTIME] = 1; # 1/10 Sekunde Timeout
          #ifdef UNIX_TERM_TERMIOS
          if (!( TCSETATTR(stdin_handle,TCSANOW,&newtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #else
          if (!( ioctl(stdin_handle,TCSETA,&newtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #endif
         {var int ergebnis = read(stdin_handle,&c,1); # Zeichen lesen versuchen, mit Timeout
          #ifdef UNIX_TERM_TERMIOS
          if (!( TCSETATTR(stdin_handle,TCSANOW,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #else
          if (!( ioctl(stdin_handle,TCSETA,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #endif
          end_system_call();
          run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
          if (ergebnis<0)
            { begin_system_call();
              if (errno==EINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
                { end_system_call();
                  interruptp({ pushSTACK(S(read_char)); tast_break(); }); # Break-Schleife aufrufen
                  goto restart_it;
                }
              OS_error();
            }
          if (ergebnis==0)
            goto empty_buffer; # kein Zeichen verfügbar
          goto next_char_is_read; # ergebnis=1 -> Zeichen verfügbar
        }}
        #else
        # Man könnte hier fcntl(stdin_handle,F_SETFL,...|FASYNC) verwenden
        # und auf Signal SIGIO warten. Allerdings funktioniert das auf so
        # wenigen Systemen (siehe Emacs), dass es sich wohl nicht lohnt.
        #endif
        #endif
        goto read_next_char;
      }
      # Buffer Zeichen für Zeichen liefern:
      empty_buffer:
      { var object l = TheStream(stream)->strm_keyboard_buffer;
        TheStream(stream)->strm_keyboard_buffer = Cdr(l);
       {var uintB c = char_code(Car(l));
        if ((c >= ' ') || (c == ESC) || (c == TAB) || (c == CR) || (c == BS))
          { pushSTACK(code_char(c)); funcall(S(make_char),1); return value1; }
          else
          # Taste vermutlich mit Ctrl getippt
          { var key_event event;
            event.key = NULL;
            event.code = (c == 0 ? ' ' : (c | bit(6)));
            event.bits = char_control_c;
            return make_key_event(&event);
          }
      }}
    }}

  # UP: Erweitert die Liste STACK_0 um eine Tastenzuordnung.
  # kann GC auslösen
    local void add_keybinding (const char* cap, const key_event* event);
    local void add_keybinding(cap,event)
      var const char* cap;
      var const key_event* event;
      { var const uintB* ptr = (const uintB*)cap;
        if (*ptr=='\0') return; # leere Tastenfolge vermeiden
        pushSTACK(allocate_cons());
        # Liste (char1 ... charn . key) bilden:
        {var uintC count = 0;
         do { pushSTACK(code_char(*ptr)); ptr++; count++; } until (*ptr=='\0');
         pushSTACK(make_key_event(event)); count++;
         funcall(L(liststern),count);
        }
        # und auf STACK_0 pushen:
        {var object l = popSTACK();
         Car(l) = value1; Cdr(l) = STACK_0; STACK_0 = l;
      } }
  #define keybinding(cap,ev1,ev2,ev3)  \
    { var key_event event = ev1,ev2,ev3; add_keybinding(cap,&event); }

  #endif

  #ifdef NEXTAPP
    #define rd_ch_keyboard  rd_ch_dummy
  #endif

# Liefert einen Keyboard-Stream.
# make_keyboard_stream()
# kann GC auslösen
  local object make_keyboard_stream (void);
  local object make_keyboard_stream()
    {
     #if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)
      # Tabelle aller Zuordnungen Zeichenfolge -> Taste bilden:
      pushSTACK(NIL);
      # Terminal-Typ abfragen:
      begin_system_call();
     {var const char* s = getenv("TERM");
      if (s==NULL)
        { end_system_call(); }
        else
        { var char tbuf[4096]; # interner Buffer für die Termcap-Routinen
          if (!(tgetent(tbuf,s)==1))
            { end_system_call(); }
            else
            { var char tentry[4096]; # Buffer für von mir benötigte Capabilities und Pointer da hinein
              var char* tp = &tentry[0];
              var const char* cap;
              end_system_call();
              # Backspace:
              begin_system_call(); cap = tgetstr("kb",&tp); end_system_call();
              if (cap) { keybinding(cap, { NULL, BS, 0 } ); } # #\Backspace
              # Insert, Delete:
              begin_system_call(); cap = tgetstr("kI",&tp); end_system_call();
              if (cap) { keybinding(cap, { "INSERT", 0, char_hyper_c } ); } # #\Insert
              begin_system_call(); cap = tgetstr("kD",&tp); end_system_call();
              if (cap) { keybinding(cap, { "DELETE", 0, char_hyper_c } ); } # #\Delete
              # Pfeiltasten:
              begin_system_call(); cap = tgetstr("ku",&tp); end_system_call();
              if (cap) { keybinding(cap, { "UP", 0, char_hyper_c } ); } # #\Up
              if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'A') && (cap[3] == '\0'))
                { keybinding(ESCstring"[A", { "UP", 0, char_hyper_c } ); } # #\Up
              begin_system_call(); cap = tgetstr("kd",&tp); end_system_call();
              if (cap) { keybinding(cap, { "DOWN", 0, char_hyper_c } ); } # #\Down
              if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'B') && (cap[3] == '\0'))
                { keybinding(ESCstring"[B", { "DOWN", 0, char_hyper_c } ); } # #\Down
              begin_system_call(); cap = tgetstr("kr",&tp); end_system_call();
              if (cap) { keybinding(cap, { "RIGHT", 0, char_hyper_c } ); } # #\Right
              if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'C') && (cap[3] == '\0'))
                { keybinding(ESCstring"[C", { "RIGHT", 0, char_hyper_c } ); } # #\Right
              begin_system_call(); cap = tgetstr("kl",&tp); end_system_call();
              if (cap) { keybinding(cap, { "LEFT", 0, char_hyper_c } ); } # #\Left
              if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'D') && (cap[3] == '\0'))
                { keybinding(ESCstring"[D", { "LEFT", 0, char_hyper_c } ); } # #\Left
              # sonstige Cursorblock-Tasten:
              begin_system_call(); cap = tgetstr("kh",&tp); end_system_call();
              if (cap) { keybinding(cap, { "HOME", 0, char_hyper_c } ); } # #\Home
              begin_system_call(); cap = tgetstr("K1",&tp); end_system_call();
              if (cap) { keybinding(cap, { "HOME", 0, char_hyper_c } ); } # #\Home
              begin_system_call(); cap = tgetstr("KH",&tp); end_system_call();
              if (cap) { keybinding(cap, { "END", 0, char_hyper_c } ); } # #\End
              begin_system_call(); cap = tgetstr("K4",&tp); end_system_call();
              if (cap) { keybinding(cap, { "END", 0, char_hyper_c } ); } # #\End
              begin_system_call(); cap = tgetstr("kP",&tp); end_system_call();
              if (cap) { keybinding(cap, { "PGUP", 0, char_hyper_c } ); } # #\PgUp
              begin_system_call(); cap = tgetstr("K3",&tp); end_system_call();
              if (cap) { keybinding(cap, { "PGUP", 0, char_hyper_c } ); } # #\PgUp
              begin_system_call(); cap = tgetstr("kN",&tp); end_system_call();
              if (cap) { keybinding(cap, { "PGDN", 0, char_hyper_c } ); } # #\PgDn
              begin_system_call(); cap = tgetstr("K5",&tp); end_system_call();
              if (cap) { keybinding(cap, { "PGDN", 0, char_hyper_c } ); } # #\PgDn
              begin_system_call(); cap = tgetstr("K2",&tp); end_system_call();
              if (cap) { keybinding(cap, { "CENTER", 0, char_hyper_c } ); } # #\Center
              # Funktionstasten:
              { typedef struct { const char* capname; key_event key; } funkey;
                local var const funkey funkey_tab[] = {
                  { "k1", { "F1", 0, char_hyper_c } }, # #\F1
                  { "k2", { "F2", 0, char_hyper_c } }, # #\F2
                  { "k3", { "F3", 0, char_hyper_c } }, # #\F3
                  { "k4", { "F4", 0, char_hyper_c } }, # #\F4
                  { "k5", { "F5", 0, char_hyper_c } }, # #\F5
                  { "k6", { "F6", 0, char_hyper_c } }, # #\F6
                  { "k7", { "F7", 0, char_hyper_c } }, # #\F7
                  { "k8", { "F8", 0, char_hyper_c } }, # #\F8
                  { "k9", { "F9", 0, char_hyper_c } }, # #\F9
                  { "k0", { "F10", 0, char_hyper_c } }, # #\F10
                  { "k;", { "F10", 0, char_hyper_c } }, # #\F10
                  { "F1", { "F11", 0, char_hyper_c } }, # #\F11
                  { "F2", { "F12", 0, char_hyper_c } }, # #\F12
                  };
                var uintL i;
                for (i=0; i < sizeof(funkey_tab)/sizeof(funkey); i++)
                  { begin_system_call();
                    cap = tgetstr(funkey_tab[i].capname,&tp);
                    end_system_call();
                    if (cap) { add_keybinding(cap,&funkey_tab[i].key); }
              }   }
              # Special Linux console handling:
              begin_system_call();
              cap = tgetstr("kh",&tp); # Home
              if (!(cap && (cap[0] == ESC) && (cap[1] == '[') && (cap[2] == '1') && (cap[3] == '~') && (cap[4] == '\0')))
                goto not_linux;
              cap = tgetstr("kI",&tp); # Insert
              if (!(cap && (cap[0] == ESC) && (cap[1] == '[') && (cap[2] == '2') && (cap[3] == '~') && (cap[4] == '\0')))
                goto not_linux;
              cap = tgetstr("kD",&tp); # Delete
              if (!(cap && (cap[0] == ESC) && (cap[1] == '[') && (cap[2] == '3') && (cap[3] == '~') && (cap[4] == '\0')))
                goto not_linux;
              end_system_call();
              keybinding(ESCstring"[4~", { "END", 0, char_hyper_c } ); # #\End
              if (FALSE)
                not_linux:
                { end_system_call(); }
              # Spezial xterm handling:
              begin_system_call();
              cap = tgetstr("ku",&tp);
              if (!(cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'A') && (cap[3] == '\0')))
                goto not_xterm;
              cap = tgetstr("kd",&tp);
              if (!(cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'B') && (cap[3] == '\0')))
                goto not_xterm;
              cap = tgetstr("kr",&tp);
              if (!(cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'C') && (cap[3] == '\0')))
                goto not_xterm;
              cap = tgetstr("kl",&tp);
              if (!(cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'D') && (cap[3] == '\0')))
                goto not_xterm;
              end_system_call();
              { # Insert, Delete:
                keybinding(ESCstring"[2~", { "INSERT", 0, char_hyper_c } ); # #\Insert
                keybinding(ESCstring"[3~", { "DELETE", 0, char_hyper_c } ); # #\Delete
              }
              { # Application Keypad: ESC O M -> Return,
                # ESC O k -> +, ESC O m -> -, ESC O j -> *, ESC O o -> /
                # (ohne Hyper-Bit, da das zu Terminal-abhängig würde)
                # ESC O x -> Up, ESC O r -> Down,
                # ESC O v -> Right, ESC O t -> Left,
                # ESC O p -> Insert, ESC O l -> Delete.
                var char cap[4];
                cap[0] = ESC; cap[1] = 'O'; cap[3] = '\0';
                cap[2] = 'M'; keybinding(&!cap, { NULL, 'M'-64, 0 } );
                cap[2] = '+'+64; keybinding(&!cap, { NULL, '+', 0 } );
                cap[2] = '-'+64; keybinding(&!cap, { NULL, '-', 0 } );
                cap[2] = '*'+64; keybinding(&!cap, { NULL, '*', 0 } );
                cap[2] = '/'+64; keybinding(&!cap, { NULL, '/', 0 } );
                cap[2] = '8'+64; keybinding(&!cap, { "UP", 0, char_hyper_c } ); # #\Up
                cap[2] = '2'+64; keybinding(&!cap, { "DOWN", 0, char_hyper_c } ); # #\Down
                cap[2] = '6'+64; keybinding(&!cap, { "RIGHT", 0, char_hyper_c } ); # #\Right
                cap[2] = '4'+64; keybinding(&!cap, { "LEFT", 0, char_hyper_c } ); # #\Left
                cap[2] = '0'+64; keybinding(&!cap, { "INSERT", 0, char_hyper_c } ); # #\Insert
                cap[2] = '.'+64; keybinding(&!cap, { "DELETE", 0, char_hyper_c } ); # #\Delete
                cap[2] = ','+64; keybinding(&!cap, { "DELETE", 0, char_hyper_c } ); # #\Delete
                # "7" -> #\Home, "1" -> #\End, "9" -> #\PgUp, "3" -> #\PgDn,
                # "5" -> #\Center are already handled above.
              }
              xterm:
              { # Pfeiltasten s.o.
                # sonstige Cursorblock-Tasten:
                keybinding(ESCstring"[5~", { "PGUP", 0, char_hyper_c } ); # #\PgUp
                keybinding(ESCstring"[6~", { "PGDN", 0, char_hyper_c } ); # #\PgDn
                keybinding(ESCstring"[7~", { "HOME", 0, char_hyper_c } ); # #\Home
                keybinding(ESCstring"[8~", { "END", 0, char_hyper_c } ); # #\End
                keybinding(ESCstring"OH", { "HOME", 0, char_hyper_c } ); # #\Home
                keybinding(ESCstring"[H", { "HOME", 0, char_hyper_c } ); # #\Home
                keybinding(ESCstring"OF", { "END", 0, char_hyper_c } ); # #\End
                keybinding(ESCstring"[F", { "END", 0, char_hyper_c } ); # #\End
                # Funktionstasten:
                keybinding(ESCstring"[11~", { "F1", 0, char_hyper_c } ); # #\F1
                keybinding(ESCstring"[12~", { "F2", 0, char_hyper_c } ); # #\F2
                keybinding(ESCstring"[13~", { "F3", 0, char_hyper_c } ); # #\F3
                keybinding(ESCstring"[14~", { "F4", 0, char_hyper_c } ); # #\F4
                keybinding(ESCstring"[15~", { "F5", 0, char_hyper_c } ); # #\F5
                keybinding(ESCstring"[17~", { "F6", 0, char_hyper_c } ); # #\F6
                keybinding(ESCstring"[18~", { "F7", 0, char_hyper_c } ); # #\F7
                keybinding(ESCstring"[19~", { "F8", 0, char_hyper_c } ); # #\F8
                keybinding(ESCstring"[20~", { "F9", 0, char_hyper_c } ); # #\F9
                keybinding(ESCstring"[21~", { "F10", 0, char_hyper_c } ); # #\F10
                keybinding(ESCstring"[23~", { "F11", 0, char_hyper_c } ); # #\F11
                keybinding(ESCstring"[24~", { "F12", 0, char_hyper_c } ); # #\F12
              }
              if (FALSE)
                not_xterm:
                { end_system_call(); }
     }  }   }
     #endif
     #ifdef WIN32_NATIVE
     # Console-Handle bilden:
     # Maybe use CREATE_ALWAYS ?? Maybe use AllocConsole() ??
     { var Handle handle = CreateFile("CONIN$", GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
       if (handle==INVALID_HANDLE_VALUE) { OS_error(); }
       end_system_call();
       pushSTACK(allocate_handle(handle));
     }
     #endif
     {# neuen Stream allozieren:
      var object stream =
        allocate_stream(strmflags_rd_ch_B,strmtype_keyboard,strm_len+strm_keyboard_len,0);
        # Flags: nur READ-CHAR erlaubt
      # und füllen:
      var Stream s = TheStream(stream);
        s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
        s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
        s->strm_rd_ch = P(rd_ch_keyboard); # READ-CHAR-Pseudofunktion
        s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
        s->strm_rd_ch_last = NIL; # Lastchar := NIL
        s->strm_wr_ch = P(wr_ch_dummy); # WRITE-CHAR unmöglich
        s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
        s->strm_wr_ss = P(wr_ss_dummy);
        #if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)
        # Flag isatty = (stdin_tty ? T : NIL) bestimmen:
        begin_system_call();
        s->strm_keyboard_isatty = (isatty(stdin_handle) ? T : NIL);
        end_system_call();
        s->strm_keyboard_handle = allocate_handle(stdin_handle);
        s->strm_keyboard_buffer = NIL;
        s->strm_keyboard_keytab = popSTACK();
        #endif
        #ifdef WIN32_NATIVE
        s->strm_keyboard_isatty = T;
        s->strm_keyboard_handle = popSTACK();
        #endif
      return stream;
    }}

LISPFUNN(make_keyboard_stream,0)
# (SYSTEM::MAKE-KEYBOARD-STREAM) creates a new keyboard stream.
# Should be called once only, and the result assigned to *KEYBOARD-INPUT*.
  { value1 = make_keyboard_stream(); mv_count=1; }

#endif # KEYBOARD


# Interaktiver Terminalstream
# ===========================

#if defined(GNU_READLINE) || defined(NEXTAPP)
# Vervollständigung von Lisp-Symbolen
  global char** lisp_completion (char* text, int start, int end);
  global char** lisp_completion(text,start,end)
    var char* text; # text[0..end-start-1] = the_line[start..end-1]
    var int start;
    var int end;
    { # (SYS::COMPLETION text start end) aufrufen:
      pushSTACK(asciz_to_string(text));
      pushSTACK(fixnum((uintL)start));
      pushSTACK(fixnum((uintL)end));
      funcall(S(completion),3);
     {var object mlist = value1; # Liste der Möglichkeiten
      end_callback();
      # Liste von Simple-Strings in mallozierten Array von mallozierten
      # Asciz-Strings umbauen:
      if (nullp(mlist)) { return NULL; }
      {var char** array = (char**) malloc((llength(mlist)+1)*sizeof(char*));
       if (array==NULL) { return NULL; }
       {var char** ptr = array;
        while (consp(mlist))
          { var uintC count = Sstring_length(Car(mlist));
            var uintB* ptr1 = &TheSstring(Car(mlist))->data[0];
            var char* ptr2 = (char*) malloc((count+1)*sizeof(char));
            if (ptr2==NULL) # malloc scheitert -> alles zurückgeben
              { until (ptr==array) { free(*--ptr); }
                free(array);
                return NULL;
              }
            *ptr++ = ptr2;
            dotimesC(count,count, { *ptr2++ = *ptr1++; });
            *ptr2 = '\0';
            mlist = Cdr(mlist);
          }
        *ptr = NULL;
       }
       return array;
    }}}
#endif

#ifdef NEXTAPP

# Benutze das von nxterminal.m zur Verfügung gestellte Interface, siehe unix.d.

# UP: Ein Zeichen von einem Terminal-Stream lesen.
# rd_ch_terminal(&stream)
# > stream: Terminal-Stream
# < object ch: eingegebenes Zeichen
  local object rd_ch_terminal (const object* stream_);
  local object rd_ch_terminal(stream_)
    var const object* stream_;
    { var int linepos;
      var uintB ch;
      begin_call();
      ch = nxterminal_read_char(&linepos);
      end_call();
      TheStream(*stream_)->strm_wr_ch_lpos = fixnum(linepos);
      return code_char(ch);
    }

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal(stream)
# > stream: Terminal-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_terminal (object stream);
  local signean listen_terminal(stream)
    var object stream;
    { var signean result;
      begin_call();
      result = (nxterminal_listen() ? signean_null : signean_plus);
      end_call();
      return result;
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_terminal (object stream);
  local boolean clear_input_terminal(stream)
    var object stream;
    { # Wir wollen im Eingabefenster nichts löschen.
      return FALSE;
    }

# UP: Ein Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_terminal(&stream,ch);
# > stream: Terminal-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_terminal (const object* stream_, object ch);
  local void wr_ch_terminal(stream_,ch)
    var const object* stream_;
    var object ch;
    { var object stream = *stream_;
      if (!charp(ch)) { fehler_wr_char(stream,ch); } # ch sollte Character sein
      begin_call();
      nxterminal_write_char(char_code(ch));
      end_call();
    }

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# finish_output_terminal(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  local void finish_output_terminal (object stream);
  local void finish_output_terminal(stream)
    var object stream;
    { begin_call();
      nxterminal_send_output();
      end_call();
    }

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# force_output_terminal(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  #define force_output_terminal(stream)  finish_output_terminal(stream)

# Liefert einen interaktiven Terminal-Stream.
# kann GC auslösen
  local object make_terminal_stream_ (void);
  local object make_terminal_stream_()
    { # neuen Stream allozieren:
      var object stream =
        allocate_stream(strmflags_ch_B,strmtype_terminal,strm_len+0,0);
        # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
      # und füllen:
      var Stream s = TheStream(stream);
        s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
        s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
        s->strm_rd_ch = P(rd_ch_terminal); # READ-CHAR-Pseudofunktion
        s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
        s->strm_rd_ch_last = NIL; # Lastchar := NIL
        s->strm_wr_ch = P(wr_ch_terminal); # WRITE-CHAR-Pseudofunktion
        s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
        s->strm_wr_ss = P(wr_ss_dummy_nogc);
      return stream;
    }

#endif # NEXTAPP

#if (defined(UNIX) && !defined(NEXTAPP)) || defined(MSDOS) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32_NATIVE)

# Funktionsweise:
# Es wird auf Standard-Input und Standard-Output zugegriffen.
# Wegen Umleite-Möglichkeit müssen manche Funktionen unterscheiden, ob es
# sich bei Standard-Input um ein Terminal handelt oder nicht.
# Ob Standard-Output ein Terminal ist oder nicht, ist hier irrelevant.
# Relevant ist nur, ob Standard-Input und Standard-Output dasselbe Terminal
# sind; in diesem Falle nehmen wir an, dass nach Beendigung einer Eingabezeile
# (durch NL) von Standard-Input der Cursor von Standard-Output in Spalte 0
# steht, und in diesem Falle können wir auch die GNU readline()-Library
# benutzen.

# Es gibt drei mögliche Varianten des Terminal-Streams:
# Wenn Standard-Input und Standard-Output nicht dasselbe Terminal sind:
#   * terminal1 normalerweise,
#   * terminal2 mit zeilenweiser Bufferung der Eingabe,
# Wenn Standard-Input und Standard-Output dasselbe Terminal sind:
#   * terminal3 benutzt readline()-Library, mit zeilenweiser Bufferung der
#     Eingabe und der Ausgabe.

#define HAVE_TERMINAL1
  # define TERMINAL_LINEBUFFERED  0
  # define TERMINAL_OUTBUFFERED   0

#ifdef MSDOS
  # Bei Eingabe einer Zeile von Tastatur wird das <Enter> am Ende der Zeile als
  # CR/LF ausgegeben. Jedoch: Das CR sofort, das LF jedoch erst dann, wenn das
  # <Enter> mit read() gelesen wird - das ist bei uns manchmal erst viel später.
  # [Wer diesen Schwachsinn programmiert hat - im DOS vermutlich -
  # gehört an die Wand gestellt und erschossen! :-(]
  # Aus diesem Grund müssen wir den Terminal-Stream auf der Input-Seite
  # zeilengepuffert machen.
#define HAVE_TERMINAL2
  # define TERMINAL_LINEBUFFERED  1
  # define TERMINAL_OUTBUFFERED   0
#endif

#ifdef GNU_READLINE
  # Wir benutzen die GNU Readline-Library. Sie liefert den Input zeilenweise,
  # mit Editiermöglichkeit, Vervollständigung und History. Leider müssen wir
  # den Output zeilenweise zwischenspeichern, um die letzte angefangene Zeile
  # als "Prompt" verwenden zu können.
#define HAVE_TERMINAL3
  # define TERMINAL_LINEBUFFERED  1
  # define TERMINAL_OUTBUFFERED   1
#endif

# Zusätzliche Komponenten:
  # ISATTY : Flag, ob stdin ein TTY ist und ob stdin und stdout dasselbe sind:
  #          NIL: stdin ein File o.ä.
  #          T, EQUAL: stdin ein Terminal
  #          EQUAL: stdin und stdout dasselbe Terminal
  #define strm_terminal_isatty   strm_isatty
  #define strm_terminal_ihandle  strm_ihandle
  #define strm_terminal_ohandle  strm_ohandle
#if defined(HAVE_TERMINAL2) || defined(HAVE_TERMINAL3)
  # Komponenten wegen TERMINAL_LINEBUFFERED:
  # INBUFF : Eingabebuffer, ein Semi-Simple-String
  #define strm_terminal_inbuff  strm_other[3]
  # COUNT = sein Fill-Pointer : Anzahl der Zeichen im Eingabebuffer
  # INDEX : Anzahl der bereits verbrauchten Zeichen
  #define strm_terminal_index   strm_other[4]
#endif
#ifdef HAVE_TERMINAL3
  # Komponenten wegen TERMINAL_OUTBUFFERED:
  # OUTBUFF : Ausgabebuffer, ein Semi-Simple-String
  #define strm_terminal_outbuff strm_other[5]
#endif

# Längen der unterschiedlichen Terminal-Streams:
  #define strm_terminal1_len  (strm_len+3)
  #define strm_terminal2_len  (strm_len+5)
  #define strm_terminal3_len  (strm_len+6)

# Unterscheidung nach Art des Terminal-Streams:
# terminalcase(stream, statement1,statement2,statement3);
  #if defined(HAVE_TERMINAL2) && defined(HAVE_TERMINAL3)
    #define terminalcase(stream,statement1,statement2,statement3)  \
      switch (Stream_length(stream))                 \
        { case strm_terminal1_len: statement1 break; \
          case strm_terminal2_len: statement2 break; \
          case strm_terminal3_len: statement3 break; \
          default: NOTREACHED                        \
        }
  #elif defined(HAVE_TERMINAL2)
    #define terminalcase(stream,statement1,statement2,statement3)  \
      if (Stream_length(stream) == strm_terminal2_len) { statement2 } else { statement1 }
  #elif defined(HAVE_TERMINAL3)
    #define terminalcase(stream,statement1,statement2,statement3)  \
      if (Stream_length(stream) == strm_terminal3_len) { statement3 } else { statement1 }
  #else
    #define terminalcase(stream,statement1,statement2,statement3)  \
      statement1
  #endif

#ifdef MSDOS

  # get_handle_info(handle)
  # > handle
  # < ergebnis: Handle-Info (INT 21,44,00)
  #ifdef DJUNIX
    #define get_handle_info(handle)  \
      ({ var uintW __info;                                                  \
         __asm__ (# DOS-Funktion 44H, Code 00H                              \
                  " pushl %%ebx ;"                                          \
                  " movw %1,%%bx ; movw $0x4400,%%ax ; int $0x21 ;"         \
                  " popl %%ebx "                                            \
                  : "=d" /* %dx */ (__info)                       # OUT     \
                  : "ri" ((uintW)(handle))                        # IN      \
                  : "ax","cx","si","di" /* %eax,%ecx,%esi,%edi */ # CLOBBER \
                 );                                                         \
         __info;                                                            \
       })
  #endif
  #ifdef EMUNIX
    #define get_handle_info(handle)  __ioctl1(handle,0x00)
  #endif
  #ifdef WATCOM
    local uintW get_handle_info (uintW handle);
    local uintW get_handle_info(handle)
      var uintW handle;
      { var union REGS in;
        var union REGS out;
        in.regW.ax = 0x4400; in.regW.bx = handle;
        intdos(&in,&out);
        return out.regW.dx;
      }
  #endif

#endif

#ifdef HAVE_TERMINAL1

# Lesen eines Zeichens von einem Terminal-Stream.
  local object rd_ch_terminal1 (const object* stream_);
  local object rd_ch_terminal1(stream_)
    var const object* stream_;
    { var object ch = rd_ch_handle(stream_);
      #ifdef WIN32
      # CR/LF zu NL zusammenfassen.
      # Was soll man machen, wenn nach CR kein LF kommt??
      # Was soll man machen, wenn CR gelesen wurde und das nächste Zeichen
      # noch nicht bereit ist (aber evtl. doch ein LF ist)??
      if (eq(ch,code_char(CR)))
        { ch = rd_ch_handle(stream_); } # NB: LF == NL
      #endif
      # Wenn stdin und stdout beide dasselbe Terminal sind,
      # und wir lesen ein NL, so können wir davon ausgehen,
      # dass der Cursor danach in Spalte 0 steht.
      if (eq(ch,code_char(NL)))
        { var object stream = *stream_;
          if (eq(TheStream(stream)->strm_terminal_isatty,S(equal)))
            { TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; }
        }
      return ch;
    }

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal1(stream)
# > stream: Terminal-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  #define listen_terminal1  listen_handle

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal1(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  #define clear_input_terminal1  clear_input_handle

# UP: Ein Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_terminal1(&stream,ch);
# > stream: Terminal-Stream
# > ch: auszugebendes Zeichen
 #if !defined(AMIGAOS)
  #define wr_ch_terminal1  wr_ch_handle
 #else # defined(AMIGAOS)
  local void wr_ch_terminal1 (const object* stream_, object ch);
  local void wr_ch_terminal1(stream_,ch)
    var const object* stream_;
    var object ch;
    { # ch sollte ein Character mit höchstens Font, aber ohne Bits sein:
      #error "FIXME character fonts don't exist in this form any more"
      if (!((as_oint(ch) & ~(((oint)char_code_mask_c|(oint)char_font_mask_c)<<oint_data_shift)) == as_oint(type_data_object(char_type,0))))
        { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
          pushSTACK(*stream_);
          pushSTACK(ch);
          fehler(stream_error,
                 DEUTSCH ? "Character ~ enthält Bits und kann daher nicht auf ~ ausgegeben werden." :
                 ENGLISH ? "character ~ contains bits, cannot be output onto ~" :
                 FRANCAIS ? "Le caractère ~ contient des «bits» et ne peut pas être écrit dans ~." :
                 ""
                );
        }
     #if (!(char_font_len_c == 4))
       #error "char_font_len_c neu einstellen oder wr_ch_terminal neu schreiben!"
     #endif
     {var uintB outbuffer[14];
      var uintB* ptr = &outbuffer[0];
      var uintL count = 1;
      var uintB f = (char_int(ch) & char_font_mask_c) >> char_font_shift_c; # Font des Zeichens
      var uintB c = char_code(ch); # Code des Zeichens
      if (f==0)
        { *ptr++ = c; }
        else
        { *ptr++ = CSI; # Kontroll-Sequenz zum Umschalten auf den richtigen Font:
          if (f & bit(0)) { *ptr++ = ';'; *ptr++ = '1'; count += 2; } # Fettschrift ein
          if (f & bit(1)) { *ptr++ = ';'; *ptr++ = '3'; count += 2; } # Kursiv ein
          if (f & bit(2)) { *ptr++ = ';'; *ptr++ = '4'; count += 2; } # Unterstreichung ein
          if (f & bit(3)) { *ptr++ = ';'; *ptr++ = '7'; count += 2; } # Reverse ein
          *ptr++ = 0x6D;
          *ptr++ = c; # dann das Zeichen ausgeben
          *ptr++ = CSI; *ptr++ = '0'; *ptr++ = 0x6D; # Wieder Normalschrift
          count += 5;
        }
      begin_system_call();
      {var long ergebnis = Write(stdout_handle,&outbuffer[0],count); # Zeichen auszugeben versuchen
       end_system_call();
       if (ergebnis<0) { OS_error(); } # Error melden
       if (ergebnis<count) # nicht erfolgreich?
         { fehler_unwritable(S(write_char),*stream_); }
    }}}
 #endif

# UP: Mehrere Zeichen auf einen Terminal-Stream ausgeben.
# wr_ss_terminal1(&stream,string,start,len);
# > stream: Terminal-Stream
# > string: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
  #define wr_ss_terminal1  wr_ss_handle

# UP: Löscht den wartenden Output eines Terminal-Stream.
# clear_output_terminal1(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  #define clear_output_terminal1  clear_output_handle

#endif # HAVE_TERMINAL1

#ifdef HAVE_TERMINAL2

#define TERMINAL_LINEBUFFERED  TRUE

# Lesen eines Zeichens von einem Terminal-Stream.
  local object rd_ch_terminal2 (const object* stream_);
  # vgl. rd_ch_handle() :
  local object rd_ch_terminal2(stream_)
    var const object* stream_;
    { restart_it:
     {var object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        { return eof_value; }
      #if TERMINAL_LINEBUFFERED
      { var object inbuff = TheStream(stream)->strm_terminal_inbuff; # Eingabebuffer
        if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
            < TheIarray(inbuff)->dims[1]
           )
          # index<count -> Es sind noch Zeichen im Buffer
          { var uintL index =
              posfixnum_to_L(TheStream(stream)->strm_terminal_index); # Index
            TheStream(stream)->strm_terminal_index =
              fixnum_inc(TheStream(stream)->strm_terminal_index,1); # Index erhöhen
            return code_char(TheSstring(TheIarray(inbuff)->data)->data[index]); # nächstes Character
          }
        # index=count -> muss eine ganze Zeile von Tastatur lesen:
        TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
        TheIarray(inbuff)->dims[1] = 0; # count := 0
      }
      continue_line:
      #endif
      {var uintB c;
       run_time_stop(); # Run-Time-Stoppuhr anhalten
       begin_system_call();
       #ifdef GRAPHICS_SWITCH
       switch_text_mode();
       #endif
       {var int ergebnis = read(stdin_handle,&c,1); # Zeichen lesen versuchen
        end_system_call();
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        if (ergebnis<0)
          { begin_system_call();
            if (errno==EINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
              { end_system_call();
                interruptp({ pushSTACK(S(read_char)); tast_break(); }); # Break-Schleife aufrufen
                goto restart_it;
              }
            OS_error();
          }
        if (ergebnis==0)
          # kein Zeichen verfügbar -> EOF erkennen
          #if TERMINAL_LINEBUFFERED
          if (TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1] > 0)
            goto restart_it; # Zeichen des Buffers liefern, dann erst eof_value liefern
            else
          #endif
            { TheStream(stream)->strm_rd_ch_last = eof_value; return eof_value; }
       }
       #if TERMINAL_LINEBUFFERED
       # Zeichen c zur Eingabezeile dazunehmen, evtl. die Zeile vergrößern:
       ssstring_push_extend(TheStream(stream)->strm_terminal_inbuff,c);
       stream = *stream_;
       #endif
       # Wenn stdin und stdout beide dasselbe Terminal sind,
       # und wir lesen ein NL, so können wir davon ausgehen,
       # dass der Cursor danach in Spalte 0 steht.
       if (c==NL)
         { if (eq(TheStream(stream)->strm_terminal_isatty,S(equal)))
             { TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; }
         }
       #if TERMINAL_LINEBUFFERED
         else
         goto continue_line; # so lang weiterlesen, bis ein NL kommt...
       # Kam ein NL, so fangen wir an, die Zeichen des Buffers zu liefern:
       goto restart_it;
       #endif
      }
    }}

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal2(stream)
# > stream: Terminal-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_terminal2 (object stream);
  local signean listen_terminal2(stream)
    var object stream;
    { if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        { return signean_minus; }
      if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
          < TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1]
         )
        # index<count -> Es sind noch Zeichen im Buffer
        { return signean_null; }
      return listen_handle(stream);
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal2(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_terminal2 (object stream);
  local boolean clear_input_terminal2(stream)
    var object stream;
    { if (nullp(TheStream(stream)->strm_terminal_isatty))
        # File -> nichts tun
        { return FALSE; }
      # Terminal
      TheStream(stream)->strm_rd_ch_last = NIL; # gewesenes EOF vergessen
      #if TERMINAL_LINEBUFFERED
      TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
      TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1] = 0; # count := 0
      #endif
      clear_tty_input(stdin_handle);
      pushSTACK(stream);
      while (listen_terminal2(STACK_0) == 0) { read_char(&STACK_0); }
      skipSTACK(1);
      return TRUE;
    }

# UP: Ein Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_terminal2(&stream,ch);
# > stream: Terminal-Stream
# > ch: auszugebendes Zeichen
  #define wr_ch_terminal2  wr_ch_handle

# UP: Mehrere Zeichen auf einen Terminal-Stream ausgeben.
# wr_ss_terminal2(&stream,string,start,len);
# > stream: Terminal-Stream
# > string: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
  #define wr_ss_terminal2  wr_ss_handle

# UP: Löscht den wartenden Output eines Terminal-Stream.
# clear_output_terminal2(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  #define clear_output_terminal2  clear_output_handle

#endif # HAVE_TERMINAL2

#ifdef HAVE_TERMINAL3

#define TERMINAL_LINEBUFFERED  TRUE
#define TERMINAL_OUTBUFFERED   TRUE

# Unsere eigene Vervollständigungs-Funktion, imitiert completion_matches()
# aus readline.c.
  local char** lisp_completion_matches (char* text, int start, int end);
  local boolean want_filename_completion;
  extern_C char* filename_completion_function (char* text, int state); # siehe readline.c
  local char** lisp_completion_matches(text,start,end)
    var char* text; # text[0..end-start-1] = the_line[start..end-1]
    var int start;
    var int end;
    { if ((start>=2) && (rl_line_buffer[start-2]=='#') && (rl_line_buffer[start-1]== '\"'))
        # Vervollständigung nach #" bezieht sich auf Filenamen:
        { want_filename_completion = TRUE; return NULL; }
      # Dies ist eine Callback-Funktion, wir müssen den Stack wieder korrekt setzen:
      begin_callback();
     {var char** result = lisp_completion(rl_line_buffer,start,end);
      want_filename_completion = FALSE;
      return result;
    }}

# Falls obige Funktion NULL (keine Matches) lieferte, wird die folgende
# Funktion so lange aufgerufen, bis sie ihrerseits NULL liefert.
  local char* lisp_completion_more (char* text, int state);
  local char* lisp_completion_more(text,state)
    var char* text;
    var int state;
    { if (want_filename_completion)
        { return filename_completion_function(text,state); }
        else
        { return NULL; }
    }

# Lesen eines Zeichens von einem Terminal-Stream.
  local object rd_ch_terminal3 (const object* stream_);
  # vgl. rd_ch_handle() :
  local object rd_ch_terminal3(stream_)
    var const object* stream_;
    { restart_it:
     {var object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        { return eof_value; }
      #if TERMINAL_LINEBUFFERED
      { var object inbuff = TheStream(stream)->strm_terminal_inbuff; # Eingabebuffer
        if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
            < TheIarray(inbuff)->dims[1]
           )
          # index<count -> Es sind noch Zeichen im Buffer
          { var uintL index =
              posfixnum_to_L(TheStream(stream)->strm_terminal_index); # Index
            TheStream(stream)->strm_terminal_index =
              fixnum_inc(TheStream(stream)->strm_terminal_index,1); # Index erhöhen
            return code_char(TheSstring(TheIarray(inbuff)->data)->data[index]); # nächstes Character
          }
        # index=count -> muss eine ganze Zeile von Tastatur lesen:
        TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
        TheIarray(inbuff)->dims[1] = 0; # count := 0
      }
      #endif
      { var char* prompt; # Prompt: letzte bisher ausgegebene Zeile
       {var object lastline = string_to_asciz(TheStream(*stream_)->strm_terminal_outbuff);
        begin_system_call();
        prompt = (char*) malloc(Sstring_length(lastline)+1);
        if (!(prompt==NULL))
          { strcpy(prompt,TheAsciz(lastline));
            #ifndef NO_MATCH  # not needed any more in readline-2.2-clisp or newer
            # Die readline()-Library arbeitet mit einer anderen Bildschirmbreite,
            # als sie bei der Ausgabe des Prompts benutzt wurde. Bei Prompts
            # länger als eine Bildschirmzeile gibt das Probleme. Wir behelfen
            # uns, indem wir an passender Stelle ein '\n' einfügen.
            { var uintL prompt_length = asciz_length(prompt);
              var uintL screenwidth = posfixnum_to_L(Symbol_value(S(prin_linelength)))+1;
              if (prompt_length >= screenwidth)
                { var uintL insertpos = round_down(prompt_length,screenwidth);
                  var uintL i;
                  for (i = prompt_length; i >= insertpos; i--)
                    { prompt[i+1] = prompt[i]; }
                  prompt[insertpos] = '\n';
            }   }
            #endif
       }  }
       end_system_call();
       # Lexem-trennende Characters: die mit Syntaxcode whsp,tmac,nmac
       # (siehe IO.D, eigentlich von der Readtable abhängig):
       rl_basic_word_break_characters = "\t" NLstring " \"#'(),;`";
       rl_basic_quote_characters = "\"|";
       rl_completer_quote_characters = "\\|";
       run_time_stop(); # Run-Time-Stoppuhr anhalten
       begin_call();
       #ifdef GRAPHICS_SWITCH
       switch_text_mode();
       #endif
       rl_already_prompted = TRUE;
       {var uintB* line = (uintB*)readline(prompt==NULL ? "" : prompt); # Zeile lesen
        end_call();
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        if (!(prompt==NULL))
          { begin_system_call(); free(prompt); end_system_call(); }
        if (line==NULL)
          # EOF (am Zeilenanfang) erkennen
          { TheStream(stream)->strm_rd_ch_last = eof_value; return eof_value; }
        # gelesene Zeile zur Eingabezeile dazunehmen:
        {var uintB* ptr = line;
         until (*ptr == '\0')
           { ssstring_push_extend(TheStream(*stream_)->strm_terminal_inbuff,*ptr++); }
         ssstring_push_extend(TheStream(*stream_)->strm_terminal_inbuff,NL);
        }
        # und in die History übernehmen, falls nicht leer:
        if (!(line[0]=='\0'))
          { begin_system_call(); add_history(line); end_system_call(); }
        # Freigeben müssen wir die Zeile!
        begin_system_call(); free(line); end_system_call();
      }}
      # Wenn stdin und stdout beide dasselbe Terminal sind, können
      # wir davon ausgehen, dass der Cursor in Spalte 0 steht.
      if (eq(TheStream(*stream_)->strm_terminal_isatty,S(equal)))
        { TheStream(*stream_)->strm_wr_ch_lpos = Fixnum_0;
          TheIarray(TheStream(*stream_)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
        }
      # Nun fangen wir an, die Zeichen des Buffers zu liefern:
      goto restart_it;
    }}

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal3(stream)
# > stream: Terminal-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_terminal3 (object stream);
  local signean listen_terminal3(stream)
    var object stream;
    { if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        { return signean_minus; }
      if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
          < TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1]
         )
        # index<count -> Es sind noch Zeichen im Buffer
        { return signean_null; }
      return listen_handle(stream);
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal3(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_terminal3 (object stream);
  local boolean clear_input_terminal3(stream)
    var object stream;
    { if (nullp(TheStream(stream)->strm_terminal_isatty))
        # File -> nichts tun
        { return FALSE; }
      # Terminal
      TheStream(stream)->strm_rd_ch_last = NIL; # gewesenes EOF vergessen
      #if TERMINAL_LINEBUFFERED
      TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
      TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1] = 0; # count := 0
      #endif
      clear_tty_input(stdin_handle);
      pushSTACK(stream);
      while (listen_terminal3(STACK_0) == 0) { read_char(&STACK_0); }
      skipSTACK(1);
      return TRUE;
    }

# UP: Ein Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_terminal3(&stream,ch);
# > stream: Terminal-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_terminal3 (const object* stream_, object ch);
  local void wr_ch_terminal3(stream_,ch)
    var const object* stream_;
    var object ch;
    { if (!charp(ch)) { fehler_wr_char(*stream_,ch); } # ch sollte Character sein
     {var uintB c = char_code(ch); # Code des Zeichens
      #if TERMINAL_OUTBUFFERED
      if (c==NL)
        TheIarray(TheStream(*stream_)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
        else
        ssstring_push_extend(TheStream(*stream_)->strm_terminal_outbuff,c);
      #endif
      restart_it:
      begin_system_call();
      #ifdef GRAPHICS_SWITCH
      switch_text_mode();
      #endif
      {var int ergebnis = write(stdout_handle,&c,1); # Zeichen auszugeben versuchen
       if (ergebnis<0)
         { if (errno==EINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
             { end_system_call();
               interruptp({ pushSTACK(S(write_char)); tast_break(); }); # Break-Schleife aufrufen
               goto restart_it;
             }
           OS_error(); # Error melden
         }
       end_system_call();
       if (ergebnis==0) # nicht erfolgreich?
         { fehler_unwritable(S(write_char),*stream_); }
      }
    }}

# UP: Mehrere Zeichen auf einen Terminal-Stream ausgeben.
# wr_ss_terminal3(&stream,string,start,len);
# > stream: Terminal-Stream
# > string: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
  local void wr_ss_terminal3 (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_terminal3(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { if (len==0) return;
     {var uintB* ptr = &TheSstring(string)->data[start];
      begin_system_call();
      #ifdef GRAPHICS_SWITCH
      switch_text_mode();
      #endif
      {var sintL ergebnis = full_write(stdout_handle,ptr,len); # Zeichen auszugeben versuchen
       if (ergebnis<0) { OS_error(); } # Error melden
       end_system_call();
       if (!(ergebnis==(sintL)len)) # nicht erfolgreich?
         { fehler_unwritable(S(write_string),*stream_); }
       ptr += len;
      }
      #if TERMINAL_OUTBUFFERED
      # Zeichen seit dem letzten NL in den Buffer:
      { var uintL pos = 0; # zähle die Zahl der Zeichen seit dem letzten NL
        var uintL count;
        dotimespL(count,len, { if (*--ptr == NL) goto found_NL; pos++; } );
        if (FALSE)
          found_NL: # pos Zeichen seit dem letzten NL
          { ptr++;
            TheIarray(TheStream(*stream_)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
          }
       pushSTACK(string);
       {var uintL index = start + len - pos;
        dotimesL(count,pos,
          { ssstring_push_extend(TheStream(*stream_)->strm_terminal_outbuff,
                                 TheSstring(STACK_0)->data[index]);
            index++;
          });
        string = popSTACK();
      }}
      ptr = &TheSstring(string)->data[start+len];
      #endif
      wr_ss_lpos(*stream_,ptr,len); # Line-Position aktualisieren
    }}

# UP: Löscht den wartenden Output eines Terminal-Stream.
# clear_output_terminal3(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  local void clear_output_terminal3 (object stream);
  local void clear_output_terminal3(stream)
    var object stream;
    { clear_output_handle(stream);
      #if TERMINAL_OUTBUFFERED
      TheIarray(TheStream(stream)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
      #endif
    }

#endif # HAVE_TERMINAL3

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# finish_output_terminal(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  #define finish_output_terminal  finish_output_handle

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# force_output_terminal(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  #define force_output_terminal  force_output_handle

# Liefert einen interaktiven Terminal-Stream.
# kann GC auslösen
  local object make_terminal_stream_ (void);
  local object make_terminal_stream_()
    {
     #ifdef AMIGAOS
      # nur HAVE_TERMINAL1
      { pushSTACK(allocate_handle(stdout_handle));
        pushSTACK(allocate_handle(stdin_handle));
       {var object stream =
          allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal1_len,0);
        # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
        # und füllen:
        var Stream s = TheStream(stream);
          s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
          s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
          s->strm_rd_ch = P(rd_ch_terminal1); # READ-CHAR-Pseudofunktion
          s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
          s->strm_rd_ch_last = NIL; # Lastchar := NIL
          s->strm_wr_ch = P(wr_ch_terminal1); # WRITE-CHAR-Pseudofunktion
          s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
          s->strm_wr_ss = P(wr_ss_terminal1);
          begin_system_call();
          s->strm_terminal_isatty =
            (IsInteractive(stdin_handle)
              ? (IsInteractive(stdout_handle)
                  ? S(equal) # Input und Output Terminals -> vermutlich dasselbe
                  : T
                )
              : NIL
            );
          end_system_call();
          s->strm_terminal_ihandle = popSTACK();
          s->strm_terminal_ohandle = popSTACK();
        return stream;
      }}
     #else
      { var int stdin_tty;
        var int stdout_tty;
        var int same_tty;
        begin_system_call();
        stdin_tty = isatty(stdin_handle); # stdin ein Terminal?
        stdout_tty = isatty(stdout_handle); # stdout ein Terminal?
        same_tty = FALSE; # vorläufig
        if (stdin_tty && stdout_tty)
          # stdin und stdout Terminals.
          {
           #if defined(UNIX) || defined(RISCOS)
            #if 0
            var const char* ergebnis;
            var object filename;
            ergebnis = ttyname(stdin_handle); # Filename von stdin holen
            if (!(ergebnis==NULL))
              { end_system_call();
                filename = asciz_to_string(ergebnis);
                begin_system_call();
                ergebnis = ttyname(stdout_handle); # Filename von stdout holen
                if (!(ergebnis==NULL))
                  { end_system_call();
                    pushSTACK(filename);
                    filename = asciz_to_string(ergebnis);
                    if (string_gleich(popSTACK(),filename)) # gleiche Filenamen?
                      { same_tty = TRUE; }
              }   }
            #else # ttyname() ist recht langsam, fstat() geht schneller.
            struct stat stdin_stat;
            struct stat stdout_stat;
            if ((fstat(stdin_handle,&stdin_stat) >= 0) && (fstat(stdout_handle,&stdout_stat) >= 0))
              if ((stdin_stat.st_dev == stdout_stat.st_dev) && (stdin_stat.st_ino == stdout_stat.st_ino))
                { same_tty = TRUE; }
            #endif
           #endif
           #ifdef MSDOS
            if (   ((get_handle_info(stdin_handle) & (bit(7)|bit(0))) == (bit(7)|bit(0))) # stdin == console_input ?
                && ((get_handle_info(stdout_handle) & (bit(7)|bit(1))) == (bit(7)|bit(1))) # stdout == console_output ?
               )
              { same_tty = TRUE; }
           #endif
           #ifdef WIN32_NATIVE
            var DWORD console_mode;
            if (   GetConsoleMode(stdin_handle,&console_mode)
                && GetConsoleMode(stdout_handle,&console_mode)
               )
              { same_tty = TRUE; }
           #endif
          }
        end_system_call();
        #ifdef HAVE_TERMINAL3
        if (rl_present_p && same_tty)
          # Baue einen TERMINAL3-Stream:
          { pushSTACK(make_ssstring(80)); # Zeilenbuffer allozieren
            pushSTACK(make_ssstring(80)); # Zeilenbuffer allozieren
            # neuen Stream allozieren:
           {var object stream =
              allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal3_len,0);
              # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
            # und füllen:
            var Stream s = TheStream(stream);
              s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
              s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
              s->strm_rd_ch = P(rd_ch_terminal3); # READ-CHAR-Pseudofunktion
              s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
              s->strm_rd_ch_last = NIL; # Lastchar := NIL
              s->strm_wr_ch = P(wr_ch_terminal3); # WRITE-CHAR-Pseudofunktion
              s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
              s->strm_wr_ss = P(wr_ss_terminal3);
              s->strm_terminal_isatty = S(equal); # stdout=stdin
              s->strm_terminal_ihandle = allocate_handle(stdin_handle); # Handle für listen_handle()
              s->strm_terminal_ohandle = allocate_handle(stdout_handle); # Handle für Output
              #if 1 # TERMINAL_LINEBUFFERED
              s->strm_terminal_inbuff = popSTACK(); # Zeilenbuffer eintragen, count := 0
              s->strm_terminal_index = Fixnum_0; # index := 0
              #endif
              #if 1 # TERMINAL_OUTBUFFERED
              s->strm_terminal_outbuff = popSTACK(); # Zeilenbuffer eintragen
              #endif
            return stream;
          }}
        #endif
        #ifdef HAVE_TERMINAL2
        if (stdin_tty)
          # Baue einen TERMINAL2-Stream:
          { pushSTACK(make_ssstring(80)); # Zeilenbuffer allozieren
            # neuen Stream allozieren:
           {var object stream =
              allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal2_len,0);
              # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
            # und füllen:
            var Stream s = TheStream(stream);
              s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
              s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
              s->strm_rd_ch = P(rd_ch_terminal2); # READ-CHAR-Pseudofunktion
              s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
              s->strm_rd_ch_last = NIL; # Lastchar := NIL
              s->strm_wr_ch = P(wr_ch_terminal2); # WRITE-CHAR-Pseudofunktion
              s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
              s->strm_wr_ss = P(wr_ss_terminal2);
              s->strm_terminal_isatty = (stdin_tty ? (same_tty ? S(equal) : T) : NIL);
              s->strm_terminal_ihandle = allocate_handle(stdin_handle); # Handle für listen_handle()
              s->strm_terminal_ohandle = allocate_handle(stdout_handle); # Handle für Output
              #if 1 # TERMINAL_LINEBUFFERED
              s->strm_terminal_inbuff = popSTACK(); # Zeilenbuffer eintragen, count := 0
              s->strm_terminal_index = Fixnum_0; # index := 0
              #endif
            return stream;
          }}
        #endif
        # Baue einen TERMINAL1-Stream:
        { # neuen Stream allozieren:
          var object stream =
            allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal1_len,0);
            # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
          # und füllen:
          var Stream s = TheStream(stream);
            s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
            s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
            s->strm_rd_ch = P(rd_ch_terminal1); # READ-CHAR-Pseudofunktion
            s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
            s->strm_rd_ch_last = NIL; # Lastchar := NIL
            s->strm_wr_ch = P(wr_ch_terminal1); # WRITE-CHAR-Pseudofunktion
            s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
            s->strm_wr_ss = P(wr_ss_terminal1);
            s->strm_terminal_isatty = (stdin_tty ? (same_tty ? S(equal) : T) : NIL);
            s->strm_terminal_ihandle = allocate_handle(stdin_handle); # Handle für listen_handle()
            s->strm_terminal_ohandle = allocate_handle(stdout_handle); # Handle für Output
          return stream;
        }
      }
     #endif
    }

#ifdef AMIGAOS

# Fehler, wenn TERMINAL-RAW nicht geht.
  nonreturning_function(local, fehler_terminal_raw, (object stream));
  local void fehler_terminal_raw(stream)
    var object stream;
    { pushSTACK(stream);
      fehler(error,
             DEUTSCH ? "RAW-Modus wird auf ~ nicht unterstützt." :
             ENGLISH ? "RAW mode not supported on ~" :
             FRANCAIS ? "Le mode «RAW» n'est pas supporté par ~." :
             ""
            );
    }

#endif

#if defined(UNIX) || defined(AMIGAOS) || defined(RISCOS)

# (SYS::TERMINAL-RAW *terminal-io* flag [errorp])
# flag /= NIL: versetzt das Terminal in cbreak/noecho-Modus,
# flag = NIL: versetzt das Terminal in nocbreak/echo-Modus zurück.
# Wenn es nicht geht und errorp angegeben und /= NIL ist, wird Error gemeldet.
# Liefert den alten Modus.

# (SYS::TERMINAL-RAW *terminal-io* t) entspricht im wesentlichen
# (progn
#   ; keine Editiermöglichkeiten, kein Echo, keine CR<-->NL-Umwandlungen:
#   (shell "stty -icanon -echo -icrnl -inlcr")
#   ; nichts abfangen:
#   ;              C-S   C-Q      Del     C-U       C-W      C-R      C-O      C-V     C-Y     C-C     C-\      C-Q     C-S    C-D
#   (shell "stty -ixon -ixoff erase ^- kill ^- werase ^- rprnt ^- flush ^- lnext ^- susp ^- intr ^- quit ^- start ^- stop ^- eof ^-")
#   ; 1 Zeichen auf einmal verlangen (nicht 4!):
#   (shell "stty min 1") ; das muss seltsamerweise zuletzt kommen...
# )
# (SYS::TERMINAL-RAW *terminal-io* nil) entspricht im wesentlichen
# (shell "stty sane")

#if defined(UNIX) || defined(RISCOS)

local void term_raw (void);
local void term_unraw (void);

local boolean oldterm_initialized = FALSE;

#if defined(UNIX_TERM_TERMIOS)
  local struct termios oldtermio; # ursprünglicher TTY-Modus
  local void term_raw()
    { if (!oldterm_initialized)
        { if (!( tcgetattr(stdout_handle,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          oldterm_initialized = TRUE;
        }
     {var struct termios newtermio;
      newtermio = oldtermio;
      newtermio.c_iflag &= ( /* IXON|IXOFF|IXANY| */ ISTRIP|IGNBRK);
      /* newtermio.c_oflag &= ~OPOST; */ # Curses stört sich dran!
      newtermio.c_lflag &= ISIG;
      { var uintC i;
        for (i=0; i<NCCS; i++) { newtermio.c_cc[i] = 0; }
      }
      newtermio.c_cc[VMIN] = 1;
      newtermio.c_cc[VTIME] = 0;
      if (!( TCSETATTR(stdout_handle,TCSAFLUSH,&newtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
    }}
  local void term_unraw()
    { if (oldterm_initialized)
        { if (!( TCSETATTR(stdout_handle,TCSAFLUSH,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
    }   }
  # Manche machen's so:
    # define crmode()    (_tty.c_lflag &=~ICANON,_tty.c_cc[VMIN]=1,tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
    # define nocrmode()  (_tty.c_lflag |= ICANON,_tty.c_cc[VEOF] = CEOF,tcsetattr(_tty_ch, TCSAFLUSH,&_tty))
    # define echo()      (_tty.c_lflag |= ECHO, tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
    # define noecho()    (_tty.c_lflag &=~ECHO, tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
    # define nl()        (_tty.c_iflag |= ICRNL,_tty.c_oflag |= ONLCR,tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
    # define nonl()      (_tty.c_iflag &=~ICRNL,_tty.c_oflag &=~ONLCR,tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
    # define savetty()   (tcgetattr(_tty_ch, &_oldtty),tcgetattr(_tty_ch, &_tty))
    # define resetty()   (tcsetattr(_tty_ch, TCSAFLUSH, &_oldtty))
#elif defined(UNIX_TERM_TERMIO) || defined(EMUNIX)
  local struct termio oldtermio; # ursprünglicher TTY-Modus
  local void term_raw()
    { if (!oldterm_initialized)
        { if (!( ioctl(stdout_handle,TCGETA,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          oldterm_initialized = TRUE;
        }
     {var struct termio newtermio;
      newtermio = oldtermio;
      newtermio.c_iflag &= ( /* IXON|IXOFF|IXANY| */ ISTRIP|IGNBRK);
      /* newtermio.c_oflag &= ~OPOST; */ # Curses stört sich dran!
      newtermio.c_lflag &= ISIG;
      { var uintC i;
        for (i=0; i<NCCS; i++) { newtermio.c_cc[i] = 0; }
      }
      newtermio.c_cc[VMIN] = 1;
      newtermio.c_cc[VTIME] = 0;
      if (!( ioctl(stdout_handle,TCSETAF,&newtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
    }}
  local void term_unraw()
    { if (oldterm_initialized)
        { if (!( ioctl(stdout_handle,TCSETAF,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
    }   }
  # Manche machen's so:
    # define crmode()    (_tty.c_lflag &=~ICANON,_tty.c_cc[VMIN] = 1,ioctl(_tty_ch,TCSETAF,&_tty))
    # define nocrmode()  (_tty.c_lflag |= ICANON,_tty.c_cc[VEOF] = CEOF,stty(_tty_ch,&_tty))
    # define echo()      (_tty.c_lflag |= ECHO, ioctl(_tty_ch, TCSETA, &_tty))
    # define noecho()    (_tty.c_lflag &=~ECHO, ioctl(_tty_ch, TCSETA, &_tty))
    # define nl()        (_tty.c_iflag |= ICRNL,_tty.c_oflag |= ONLCR,ioctl(_tty_ch, TCSETAW, &_tty))
    # define nonl()      (_tty.c_iflag &=~ICRNL,_tty.c_oflag &=~ONLCR,ioctl(_tty_ch, TCSETAW, &_tty))
#elif defined(UNIX_TERM_SGTTY)
  local struct sgttyb oldsgttyb; # ursprünglicher TTY-Modus
  local struct tchars oldtchars; # ursprüngliche Steuerzeichen
  #ifdef TIOCSLTC
  local struct ltchars oldltchars; # ursprüngliche Editierzeichen
  #endif
  local void term_raw()
    { if (!oldterm_initialized)
        { if (!( ioctl(stdout_handle,TIOCGETP,&oldsgttyb) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          if (!( ioctl(stdout_handle,TIOCGETC,&oldtchars) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #ifdef TIOCSLTC
          if (!( ioctl(stdout_handle,TIOCGLTC,&oldltchars) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #endif
          oldterm_initialized = TRUE;
        }
     {var struct sgttyb newsgttyb;
      newsgttyb = oldsgttyb;
      newsgttyb.sg_flags |= CBREAK;
      newsgttyb.sg_flags &= ~(CRMOD|ECHO|XTABS);
      if (!( ioctl(stdout_handle,TIOCSETP,&newsgttyb) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
     }
     {var struct tchars newtchars;
      local var union { char a [sizeof(struct tchars)];
                        struct tchars b;
                      }
                zero_tchars = {{0,}};
      newtchars = zero_tchars.b;
      if (!( ioctl(stdout_handle,TIOCSETC,&newtchars) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
     }
     #ifdef TIOCSLTC
     {var struct ltchars newltchars;
      local var union { char a [sizeof(struct ltchars)];
                        struct ltchars b;
                      }
                zero_ltchars = {{0,}};
      newltchars = zero_ltchars.b;
      if (!( ioctl(stdout_handle,TIOCSLTC,&newltchars) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
     }
     #endif
    }
  local void term_unraw()
    { if (oldterm_initialized)
        { if (!( ioctl(stdout_handle,TIOCSETP,&oldsgttyb) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          if (!( ioctl(stdout_handle,TIOCSETC,&oldtchars) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #ifdef TIOCSLTC
          if (!( ioctl(stdout_handle,TIOCSLTC,&oldltchars) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #endif
    }   }
  # Manche machen's so:
    # define raw()       (_tty.sg_flags|=RAW, stty(_tty_ch,&_tty))
    # define noraw()     (_tty.sg_flags&=~RAW,stty(_tty_ch,&_tty))
    # define crmode()    (_tty.sg_flags |= CBREAK, stty(_tty_ch,&_tty))
    # define nocrmode()  (_tty.sg_flags &= ~CBREAK,stty(_tty_ch,&_tty))
    # define echo()      (_tty.sg_flags |= ECHO, stty(_tty_ch, &_tty))
    # define noecho()    (_tty.sg_flags &= ~ECHO, stty(_tty_ch, &_tty))
    # define nl()        (_tty.sg_flags |= CRMOD,stty(_tty_ch, &_tty))
    # define nonl()      (_tty.sg_flags &= ~CRMOD, stty(_tty_ch, &_tty))
    # define savetty()   (gtty(_tty_ch, &_tty), _res_flg = _tty.sg_flags)
    # define resetty()   (_tty.sg_flags = _res_flg, stty(_tty_ch, &_tty))
#endif

# Wir speichern, ob zuletzt term_raw() oder term_unraw() ausgeführt wurde,
# damit wir bei Programm-Ausstieg zurückschalten können.
local boolean terminal_raw = FALSE;

global void terminal_sane (void);
global void terminal_sane()
  { if (terminal_raw) { term_unraw(); terminal_raw = FALSE; } }

LISPFUN(terminal_raw,2,1,norest,nokey,0,NIL)
  { var object errorp = popSTACK();
    var object flag = popSTACK();
    var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    while (TheStream(stream)->strmtype == strmtype_synonym) # Synonym-Stream verfolgen
      { var object sym = TheStream(stream)->strm_synonym_symbol;
        stream = Symbol_value(sym);
        if (!streamp(stream)) { fehler_value_stream(sym); }
      }
    value1 = NIL;
    if (TheStream(stream)->strmtype == strmtype_terminal)
      # der Terminal-Stream
      { if (!nullp(TheStream(stream)->strm_terminal_isatty))
          # Terminal
          { value1 = (terminal_raw ? T : NIL);
            begin_system_call();
            if (!nullp(flag))
              # Umschalten in cbreak/noecho-Modus:
              { term_raw(); terminal_raw = TRUE; }
              else
              # Umschalten in nocbreak/echo-Modus:
              { term_unraw(); terminal_raw = FALSE; }
            end_system_call();
      }   }
    mv_count=1;
  }

#endif # UNIX || RISCOS

#ifdef AMIGAOS

# Genauso wie den Terminal-Stream können wir auch beliebige interaktive
# Handle-Streams (andere Text-Fenster) in den Raw-Modus schalten.

# Beim Terminal-Stream speichern wir den momentanen Zustand (um so wenig wie
# möglich umschalten zu müssen), bei den Handle-Streams wird das von screen.lsp
# übernommen.
local LONG terminal_mode = 0; # 0 = CON, 1 = RAW

global void terminal_sane (void);
global void terminal_sane()
  { if (!(terminal_mode == 0))
      { begin_system_call(); SetMode(stdin_handle,0); end_system_call();
        terminal_mode = 0;
  }   }

LISPFUN(terminal_raw,2,1,norest,nokey,0,NIL)
  { var object errorp = popSTACK();
    var object flag = popSTACK();
    var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    while (TheStream(stream)->strmtype == strmtype_synonym) # Synonym-Stream verfolgen
      { var object sym = TheStream(stream)->strm_synonym_symbol;
        stream = Symbol_value(sym);
        if (!streamp(stream)) { fehler_value_stream(sym); }
      }
    if (!(TheStream(stream)->strmflags & strmflags_open_B)) # Stream geschlossen?
      { fehler_illegal_streamop(S(terminal_raw),stream); }
    value1 = NIL;
   {var LONG new_mode = (nullp(flag) ? 0 : 1);
    var LONG success;
    if ((TheStream(stream)->strmtype == strmtype_terminal) # der Terminal-Stream
        || (TheStream(stream)->strmtype == strmtype_handle) # ein File-Handle-Stream
       )
      { if (!nullp(TheStream(stream)->strm_isatty))
          { if (TheStream(stream)->strmtype == strmtype_terminal)
              # Terminal
              { value1 = (terminal_mode ? T : NIL);
                if (new_mode == terminal_mode)
                  { success = TRUE; }
                  else
                  { begin_system_call();
                    success = SetMode(stdin_handle,new_mode);
                    end_system_call();
                    terminal_mode = new_mode;
              }   }
              else
              # Handle-Stream
              { value1 = TheStream(stream)->strm_handle_rawp;
                if (new_mode == (nullp(TheStream(stream)->strm_handle_rawp) ? 0 : 1))
                  { success = TRUE; }
                  else
                  { begin_system_call();
                    success = SetMode(TheHandle(TheStream(stream)->strm_ihandle),new_mode);
                    end_system_call();
                    TheStream(stream)->strm_handle_rawp = (new_mode ? T : NIL);
              }   }
          }
          else
          { success = TRUE; }
      }
      else
      { success = FALSE; }
    if (!success && (!eq(errorp,unbound) && !nullp(errorp)))
      { fehler_terminal_raw(stream); }
    mv_count=1;
  }}

#endif # AMIGAOS

#endif # UNIX || AMIGAOS || RISCOS

#endif # (UNIX && !NEXTAPP) || MSDOS || AMIGAOS || RISCOS || WIN32_NATIVE

#if !((defined(UNIX) && !defined(NEXTAPP)) || defined(AMIGAOS) || defined(RISCOS))

LISPFUN(terminal_raw,2,1,norest,nokey,0,NIL)
  { value1 = NIL; mv_count=1; skipSTACK(3); } # Nichts tun

#endif

# Liefert einen interaktiven Terminal-Stream.
# kann GC auslösen
  local object make_terminal_stream (void);
  local object make_terminal_stream()
    { var object stream = make_terminal_stream_();
      # Liste der offenen Streams um stream erweitern:
      pushSTACK(stream);
      {var object new_cons = allocate_cons();
       Car(new_cons) = stream = popSTACK();
       Cdr(new_cons) = O(open_files);
       O(open_files) = new_cons;
      }
      return stream;
    }


# Window-Stream
# =============

#ifdef SCREEN

# Editor-Unterstützung:
# MSDOS: Übers BIOS.
# OS/2: Mit der Video-Library von Eberhard Mattes.
# CURSES: Ein Window-Stream ist im wesentlichen ein Curses-WINDOW.
#
# (SCREEN:MAKE-WINDOW)
#   liefert einen Window-Stream. Solange bis dieser wieder geschlossen wird,
#   ist das Terminal im cbreak-noecho-Modus; weitere Ein-/Ausgabe über
#   *terminal-io* sollte in dieser Zeit nicht erfolgen.
#
# (SCREEN:WINDOW-SIZE window-stream)
#   liefert die Größe des Windows,
#   als 2 Werte: Höhe (= Ymax+1), Breite (= Xmax+1).
#
# (SCREEN:WINDOW-CURSOR-POSITION window-stream)
#   liefert die Position des Cursors im Window
#   als 2 Werte: Zeile (>=0, <=Ymax, 0=oben), Spalte (>=0, <=Xmax, 0=links).
#
# (SCREEN:SET-WINDOW-CURSOR-POSITION window-stream line column)
#   setzt die Position des Cursors im Window.
#
# (SCREEN:CLEAR-WINDOW window-stream)
#   löscht den Inhalt des Window und setzt den Cursor an die linke obere Ecke
#
# (SCREEN:CLEAR-WINDOW-TO-EOT window-stream)
#   löscht den Inhalt des Window ab Cursor-Position bis Bildschirmende
#
# (SCREEN:CLEAR-WINDOW-TO-EOL window-stream)
#   löscht den Inhalt des Window ab Cursor-Position bis Zeilenende
#
# (SCREEN:DELETE-WINDOW-LINE window-stream)
#   löscht die Cursorzeile, schiebt die Zeilen drunter um 1 nach oben
#   und löscht die letzte Bildschirmzeile.
#
# (SCREEN:INSERT-WINDOW-LINE window-stream)
#   fügt in der Zeile des Cursors eine neue Zeile ein und schiebt dabei alle
#   Zeilen ab der Cursorzeile um 1 nach unten.
#
# (SCREEN:HIGHLIGHT-ON window-stream)
#   schaltet "hervorgehobene" Ausgabe ein.
#
# (SCREEN:HIGHLIGHT-OFF window-stream)
#   schaltet "hervorgehobene" Ausgabe wieder aus.
#
# (SCREEN:WINDOW-CURSOR-ON window-stream)
#   macht den Cursor(block) sichtbar.
#
# (SCREEN:WINDOW-CURSOR-OFF window-stream)
#   macht den Cursor(block) wieder unsichtbar.

# Überprüft, ob das Argument ein Window-Stream ist.
  local void check_window_stream (object stream);
  local void check_window_stream(stream)
    var object stream;
    { if (streamp(stream)
          && (TheStream(stream)->strmtype == strmtype_window)
         )
        return;
      pushSTACK(stream);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             DEUTSCH ? "~: Argument ~ sollte ein Window-Stream sein." :
             ENGLISH ? "~: argument ~ should be a window stream" :
             FRANCAIS ? "~ : L'argument ~ devrait être un WINDOW-STREAM." :
             ""
            );
    }

#if defined(MSDOS) && !defined(EMUNIX_PORTABEL)

# Aus der Distribution von ELVIS 1.4, File PC.C :

# Author:
#      Guntram Blohm
#      Buchenstraße 19
#      W 7904 Erbach
#      Germany
#      Tel. ++49-7305-6997
#      sorry - no regular network connection

# This file implements the ibm pc bios interface. See IBM documentation
# for details.
# If TERM is set upon invocation of CLISP, this code is ignored completely,
# and the standard termcap functions are used, thus, even not-so-close
# compatibles can run CLISP. For close compatibles however, bios output
# is much faster (and permits reverse scrolling, adding and deleting lines,
# and much more ansi.sys isn't capable of). GB.

local uintL screentype; # 0 = monochrome, 1 = color

local uintW screenattr; # screen attribute index

# Documentation of attributes:
# bit 7    : foreground character blinking,
# bit 6..4 : background color,
# bit 3    : foreground intensity,
# bit 2..0 : foreground color,
# color table:
#   0 black, 1 blue, 2 green, 3 cyan, 4 red, 5 magenta, 6 brown, 7 lightgray,
# and as foreground color with intensity bit set, it is light:
#   8 darkgray, ..., E yelloe, F white.
  #define col_black    0  # schwarz
  #define col_blue     1  # blau
  #define col_green    2  # grün
  #define col_cyan     3  # blaugrün
  #define col_red      4  # rot
  #define col_magenta  5  # lila
  #define col_brown    6  # braun
  #define col_white    7  # weiß
  #define col_light(x)  (8 | x)  # hell
  #define FG(x)  x         # foreground color
  #define BG(x)  (x << 4)  # background color
local uintB attr_table[2][5] =
  { # monochrome:
    { /* no standout   */  BG(col_black) | FG(col_white),
      /* standout      */  BG(col_white) | FG(col_black),
      /* visible bell  */  BG(col_black) | FG(col_light(col_white)),
      /* underline     */  BG(col_black) | FG(1), # underline
      /* alt. char set */  BG(col_black) | FG(col_light(col_white)),
    },
    # color:
    { /* no standout   */  BG(col_blue) | FG(col_light(col_white)),
      /* standout      */  BG(col_blue) | FG(col_light(col_magenta)),
      /* visible bell  */  BG(col_blue) | FG(col_light(col_brown)),
      /* underline     */  BG(col_blue) | FG(col_light(col_green)),
      /* alt. char set */  BG(col_blue) | FG(col_light(col_red)),
    },
  };
local uintB attr; # = attr_table[screentype][screenattr];

# INT 10 documentation:
#   INT 10,01 - Set cursor type
#   INT 10,02 - Set cursor position
#   INT 10,03 - Read cursor position
#   INT 10,06 - Scroll active page up
#   INT 10,07 - Scroll active page down
#   INT 10,09 - Write character and attribute at cursor
#   INT 10,0E - Write text in teletype mode
#   INT 10,0F - Get current video state
#
# INT 10,01 - Set Cursor Type
#     AH = 01
#     CH = cursor starting scan line (cursor top) (low order 5 bits)
#     CL = cursor ending scan line (cursor bottom) (low order 5 bits)
#     returns nothing
#     - cursor scan lines are zero based
#     - the following is a list of the cursor scan lines associated with
#       most common adapters;  screen sizes over 40 lines may differ
#       depending on adapters.
#               Line     Starting     Ending      Character
#       Video   Count    Scan Line    Scan Line   Point Size
#       CGA      25         06           07           08
#       MDA      25         0B           0C           0E
#       EGA      25         06           07           0E
#       EGA      43       04/06          07           08
#       VGA      25         0D           0E           10
#       VGA      40         08           09           0A
#       VGA      50         06           07           08
#     - use CX = 2000h to disable cursor
#
# INT 10,02 - Set Cursor Position
#     AH = 02
#     BH = page number (0 for graphics modes)
#     DH = row
#     DL = column
#     returns nothing
#     - positions relative to 0,0 origin
#     - 80x25 uses coordinates 0,0 to 24,79;  40x25 uses 0,0 to 24,39
#
# INT 10,03 - Read Cursor Position and Size
#     AH = 03
#     BH = video page
#     on return:
#     CH = cursor starting scan line (low order 5 bits)
#     CL = cursor ending scan line (low order 5 bits)
#     DH = row
#     DL = column
#
# INT 10,06 - Scroll Window Up
#     AH = 06
#     AL = number of lines to scroll, previous lines are
#          blanked, if 0 or AL > screen size, window is blanked
#     BH = attribute to be used on blank line
#     CH = row of upper left corner of scroll window
#     CL = column of upper left corner of scroll window
#     DH = row of lower right corner of scroll window
#     DL = column of lower right corner of scroll window
#     returns nothing
#     - in video mode 4 (300x200 4 color) on the EGA, MCGA and VGA
#       this function scrolls page 0 regardless of the current page
#
# INT 10,07 - Scroll Window Down
#     AH = 07
#     AL = number of lines to scroll, previous lines are
#          blanked, if 0 or AL > screen size, window is blanked
#     BH = attribute to be used on blank line
#     CH = row of upper left corner of scroll window
#     CL = column of upper left corner of scroll window
#     DH = row of lower right corner of scroll window
#     DL = column of lower right corner of scroll window
#     returns nothing
#     - in video mode 4 (300x200 4 color) on the EGA, MCGA and VGA
#       this function scrolls page 0 regardless of the current page
#
# INT 10,09 - Write Character and Attribute at Cursor Position
#     AH = 09
#     AL = ASCII character to write
#     BH = display page  (or mode 13h, background pixel value)
#     BL = character attribute (text) foreground color (graphics)
#     CX = count of characters to write (CX >= 1)
#     returns nothing
#     - does not move the cursor
#     - in graphics mode (except mode 13h), if BL bit 7=1 then
#       value of BL is XOR'ed with the background color
#
# INT 10,0E - Write Text in Teletype Mode
#     AH = 0E
#     AL = ASCII character to write
#     BH = page number (text modes)
#     BL = foreground pixel color (graphics modes)
#     returns nothing
#     - cursor advances after write
#     - characters BEL (7), BS (8), LF (A), and CR (D) are
#       treated as control codes
#     - for some older BIOS (10/19/81), the BH register must point
#       to the currently displayed page
#     - on CGA adapters this function can disable the video signal while
#       performing the output which causes flitter.
#
# INT 10,0F - Get Video State
#     AH = 0F
#     on return:
#     AH = number of screen columns
#     AL = mode currently set (see ~VIDEO MODES~)
#     BH = current display page
#     - video modes greater than 13h on EGA, MCGA and VGA indicate
#       ~INT 10,0~ was called with the high bit of the mode (AL) set
#       to 1, meaning the display does not need cleared

# low-level BIOS interface

#if defined(DJUNIX) || defined(WATCOM)
  #define intvideo(in_ptr,out_ptr)  int86(0x10,in_ptr,out_ptr)
#endif
#ifdef EMUNIX
  local void intvideo (const union REGS * in_regs, union REGS * out_regs);
  local void intvideo(in_regs,out_regs)
    var const register union REGS * in_regs;
    var register union REGS * out_regs;
    { __asm__ __volatile__ ( "pushl %%ebx ; "
                             "movl 0(%%esi),%%eax ; "
                             "movl 4(%%esi),%%ebx ; "
                             "movl 8(%%esi),%%ecx ; "
                             "movl 12(%%esi),%%edx ; "
                             "pushl %%edi ; "
                             ".byte 0xcd ; .byte 0x10 ; "
                             "popl %%edi ; "
                             "movl %%eax,0(%%edi) ; "
                             "movl %%ebx,4(%%edi) ; "
                             "movl %%ecx,8(%%edi) ; "
                             "movl %%edx,12(%%edi) ; "
                             "popl %%ebx"
                             :                                                     # OUT
                             : "S" /* %esi */ (in_regs), "D" /* %edi */ (out_regs) # IN
                             : "ax","cx","si","di" /* %eax,%ecx,%esi,%edi */       # CLOBBER
                           );
    }
#endif

local void video (uintW ax, uintW* cx, uintW* dx);
local void video(ax,cx,dx)
  var uintW ax;
  var uintW* cx;
  var uintW* dx;
  { var union REGS in;
    var union REGS out;
    in.regW.ax = ax;
    { var uintB ah = in.regB.ah;
      if (ah==0x06 || ah==0x07)
        { in.regB.bh = attr; }
        else
        { in.regB.bh = 0; # "active page"
          if (ah==0x09 || ah==0x0e) { in.regB.bl = attr; }
    }   }
    if (cx) { in.regW.cx = *cx; }
    if (dx) { in.regW.dx = *dx; }
    begin_system_call();
    intvideo(&in,&out);
    end_system_call();
    if (dx) { *dx = out.regW.dx; }
    if (cx) { *cx = out.regW.cx; }
  }

global uintW v_cols()
  { # determine number of screen columns. Also set screentype according
    # to monochrome/color screen.
    var union REGS in;
    var union REGS out;
    in.regB.ah=0x0f;
    intvideo(&in,&out); # INT 10,0F : get current video state
   {var uintB videomode = out.regB.al & 0x7f;
    # Text modes are 0,1,2,3,7, and others (depending on the graphics card).
    # Only modes 0 and 7 are mono. (Well, mode 2 is gray shaded.)
    screentype = (((videomode==0) || (videomode==7))
                  ? 0 # monochrome
                  : 1 # color
                 );
    return out.regB.ah;
  }}

local uintW v_rows()
  { # Getting the number of rows is hard. Most screens support 25 only,
    # EGA/VGA also support 43/50 lines, and some OEM's even more.
    # Unfortunately, there is no standard bios variable for the number
    # of lines, and the bios screen memory size is always rounded up
    # to 0x1000. So, we'll really have to cheat.
    # When using the screen memory size, keep in mind that each character
    # byte has an associated attribute byte.
    # uses:        word at 40:4c contains  memory size
    #              byte at 40:84           # of rows-1 (sometimes)
    #              byte at 40:4a           # of columns
    #if 0 # cannot execute 8086 code!
    # screen size less then 4K? then we have 25 lines only
    if (*(uintW far *)(0x0040004CL)<=4096)
      return 25;
    # VEGA vga uses the bios byte at 0x40:0x84 for # of rows.
    # Use that byte, if it makes sense together with memory size.
    if ((((*(uintB far *)(0x0040004AL)*2*(*(uintB far *)(0x00400084L)+1))
          +0xfff
         )
         &(~0xfff)
        )
        == *(uintW far *)(0x0040004CL)
       )
      return *(uintB far *)(0x00400084L)+1;
    #endif
    # uh oh. Emit LFs until screen starts scrolling.
    { var uintW line;
      var uintW oldline = 0;
      video(0x0200,NULL,&oldline); # INT 10,02 : set cursor position to (0,0)
      loop
        { video(0x0e0a,NULL,NULL); # INT 10,0E : write LF in teletype mode
          video(0x0300,NULL,&line); # INT 10,03 : read cursor position
          line>>=8;
          if (oldline==line) { return line+1; }
          oldline = line;
  } }   }

# High-level BIOS interface

local uintW LINES;
local uintW COLS;

void v_up()
  { # cursor up: determine current position, decrement row, set position
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    dx -= 0x100;
    video(0x0200,NULL,&dx); # INT 10,02 : set cursor position
  }

#if 1

void v_cb()
  { # cursor big: set begin scan to end scan - 4
    var uintW cx;
    video(0x0300,&cx,NULL); # INT 10,03 : read cursor position
    cx=((cx&0xff)|(((cx&0xff)-4)<<8));
    video(0x0100,&cx,NULL); # INT 10,01 : set cursor type
  }

void v_cs()
  { # cursor small: set begin scan to end scan - 1
    var uintW cx;
    video(0x0300,&cx,NULL); # INT 10,03 : read cursor position
    cx=((cx&0xff)|(((cx&0xff)-1)<<8));
    video(0x0100,&cx,NULL); # INT 10,01 : set cursor type
  }

#endif

void v_ce()
  { # clear to end: get cursor position and emit the aproppriate number
    # of spaces, without moving cursor.
    var uintW cx;
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    cx = COLS - (dx&0xff);
    video(0x0920,&cx,NULL); # INT 10,09 : write character at cursor, cx times 0x20
  }

void v_cl()
  { # clear screen: clear all and set cursor home
    var uintW cx = 0;
    var uintW dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0600,&cx,&dx); # INT 10,06 : scroll active page up
    dx = 0;
    video(0x0200,&cx,&dx); # INT 10,02 : set cursor position
  }

void v_cd()
  { # clear to bottom: get position, clear to eol, clear next line to end
    var uintW cx;
    var uintW dx;
    var uintW dxtmp;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    dxtmp = (dx&0xff00)|(COLS-1);
    cx = dx;
    video(0x0600,&cx,&dxtmp); # INT 10,06 : scroll active page up
    cx = (dx&0xff00)+0x100;
    dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0600,&cx,&dx); # INT 10,06 : scroll active page up
  }

void v_al()
  { # add line: scroll rest of screen down
    var uintW cx;
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    cx = (dx&0xff00);
    dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0701,&cx,&dx); # INT 10,06 : scroll active page down
  }

void v_dl()
  { # delete line: scroll rest up
    var uintW cx;
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    cx = (dx&0xff00) /* + 0x100 */ ;
    dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0601,&cx,&dx); # INT 10,06 : scroll active page up
  }

void v_sr()
  { # scroll reverse: scroll whole screen
    var uintW cx = 0;
    var uintW dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0701,&cx,&dx); # INT 10,06 : scroll active page down
  }

void v_move(y,x)
  var uintW y;
  var uintW x;
  { # set cursor
    var uintW dx = (y<<8)+x;
    video(0x0200,NULL,&dx); # INT 10,02 : set cursor position
  }

uintW v_put(ch)
  var uintW ch;
  { # put character:
    # put attribute and char (no scroll!), then update cursor position.
    var uintW cx=1;
    ch &= 0xff;
    if (ch==NL)
      { video(0x0e00|CR,NULL,NULL); # INT 10,0E : write in teletype mode
        video(0x0e00|LF,NULL,NULL); # INT 10,0E : write in teletype mode
      }
      else
      { video(0x0900|ch,&cx,NULL); # INT 10,09 : write character at cursor
       {# cursor right: determine current position, increment column, set position
        var uintW dx;
        video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
        dx += 0x1; # increment column
        if ((dx & 0xff) == COLS) # at right margin?
          { dx &= 0xff00; # set column to 0
            dx += 0x100; # increment row
            if ((dx >> 8) == LINES) # at bottom margin?
              goto no_scroll; # do not scroll at right bottom corner!!
          }
        video(0x0200,NULL,&dx); # INT 10,02 : set cursor position
        no_scroll: ;
      }}
    return ch;
  }

# Lisp-Funktionen:

# UP: Ein Zeichen auf einen Window-Stream ausgeben.
# wr_ch_window(&stream,ch);
# > stream: Window-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_window (const object* stream_, object ch);
  local void wr_ch_window(stream_,ch)
    var const object* stream_;
    var object ch;
    { if (!charp(ch)) { fehler_wr_char(*stream_,ch); } # ch sollte Character sein
     {var uintB c = char_code(ch); # Code des Zeichens
      # Code c übers BIOS auf den Bildschirm ausgeben:
      v_put(c);
    }}

LISPFUNN(make_window,0)
  { var object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+0,0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_dummy); # READ-CHAR unmöglich
      s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      s->strm_wr_ss = P(wr_ss_dummy_nogc);
    LINES = v_rows(); COLS = v_cols();
    screenattr = 0; attr = attr_table[screentype][screenattr];
    v_cs();
    value1 = stream; mv_count=1;
  }

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var object stream;
    { v_cs();
      attr = BG(col_black) | FG(col_white); v_cl(); # clear screen black
    }

LISPFUNN(window_size,1)
  { check_window_stream(popSTACK());
    value1 = fixnum(LINES);
    value2 = fixnum(COLS);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  { check_window_stream(popSTACK());
   {var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    value1 = fixnum(dx>>8);
    value2 = fixnum(dx&0xff);
    mv_count=2;
  }}

LISPFUNN(set_window_cursor_position,3)
  { check_window_stream(STACK_2);
   {var uintL line = posfixnum_to_L(STACK_1);
    var uintL column = posfixnum_to_L(STACK_0);
    if ((line < (uintL)LINES) && (column < (uintL)COLS))
      { v_move(line,column); }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }}

LISPFUNN(clear_window,1)
  { check_window_stream(popSTACK());
    v_cl();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  { check_window_stream(popSTACK());
    v_cd();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  { check_window_stream(popSTACK());
    v_ce();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  { check_window_stream(popSTACK());
    v_dl();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  { check_window_stream(popSTACK());
    v_al();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  { check_window_stream(popSTACK());
    screenattr = 1; attr = attr_table[screentype][screenattr];
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  { check_window_stream(popSTACK());
    screenattr = 0; attr = attr_table[screentype][screenattr];
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  { check_window_stream(popSTACK());
    v_cb();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  { check_window_stream(popSTACK());
    v_cs();
    value1 = NIL; mv_count=0;
  }

#endif # MSDOS && !EMUNIX_PORTABEL

#if defined(MSDOS) && defined(EMUNIX_PORTABEL)

# Benutze die Video-Library von Eberhard Mattes.
# Vorzüge:
# - einfaches Interface,
# - ruft unter OS/2 die Vio-Funktionen auf, unter DOS wird der Bildschirm-
#   speicher direkt angesprochen (schnell!), falls einer der Standard-Textmodi
#   vorliegt, sonst wird das BIOS bemüht (portabel!).

local uintL screentype; # 0 = monochrome, 1 = color

local uintB attr_table[2][5] =
  { # monochrome:
    { /* no standout   */  BW_NORMAL,
      /* standout      */  BW_REVERSE,
      /* visible bell  */  BW_NORMAL | INTENSITY,
      /* underline     */  BW_UNDERLINE,
      /* alt. char set */  BW_NORMAL | INTENSITY,
    },
    # color:
    { /* no standout   */  B_BLUE | F_WHITE | INTENSITY,
      /* standout      */  B_BLUE | F_MAGENTA | INTENSITY,
      /* visible bell  */  B_BLUE | F_BROWN | INTENSITY,
      /* underline     */  B_BLUE | F_GREEN | INTENSITY,
      /* alt. char set */  B_BLUE | F_RED | INTENSITY,
    },
  };

local int cursor_scanlines_start;
local int cursor_scanlines_end;

local int LINES; # Anzahl Zeilen
local int COLS;  # Anzahl Spalten, Anzahl Zeichen pro Zeile

# UP: Ein Zeichen auf einen Window-Stream ausgeben.
# wr_ch_window(&stream,ch);
# > stream: Window-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_window (const object* stream_, object ch);
  local void wr_ch_window(stream_,ch)
    var const object* stream_;
    var object ch;
    { if (!charp(ch)) { fehler_wr_char(*stream_,ch); } # ch sollte Character sein
     {var uintB c = char_code(ch); # Code des Zeichens
      # Code c über die Video-Library auf den Bildschirm ausgeben:
      if (c==NL)
        { v_putc(c); }
        else
        { var int current_x;
          var int current_y;
          v_getxy(&current_x,&current_y); # get current cursor position
          if ((current_x==COLS-1) && (current_y==LINES-1))
            { v_putn(c,1); } # do not scroll at right bottom corner!!
            else
            { v_putc(c); }
        }
    }}

LISPFUNN(make_window,0)
  { var object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+0,0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_dummy); # READ-CHAR unmöglich
      s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      s->strm_wr_ss = P(wr_ss_dummy_nogc);
    v_init(); # Initialisieren
    #if 1
    screentype = (v_hardware()==V_MONOCHROME ? 0 : 1); # Bildschirmtyp abfragen
    #else
    videomode abfragen wie in vinit.c, dann
    screentype = (((videomode==0) || (videomode==7))
                  ? 0 # monochrome
                  : 1 # color
                 );
    #endif
    v_dimen(&COLS,&LINES); # Bildschirmgröße abfragen
    v_getctype(&cursor_scanlines_start,&cursor_scanlines_end); # Cursorform abfragen
    v_attrib(attr_table[screentype][0]); # Highlight off
    v_ctype(cursor_scanlines_end-1,cursor_scanlines_end); # cursor small
    value1 = stream; mv_count=1;
  }

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var object stream;
    { v_gotoxy(0,0); # Cursor home
      v_attrib(screentype==0 ? BW_NORMAL : (B_BLACK | F_WHITE));
      v_putn(' ',LINES*COLS); # Bildschirm löschen
      v_ctype(cursor_scanlines_start,cursor_scanlines_end); # Cursorform zurücksetzen
    }

LISPFUNN(window_size,1)
  { check_window_stream(popSTACK());
    value1 = fixnum((uintW)LINES);
    value2 = fixnum((uintW)COLS);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  { check_window_stream(popSTACK());
   {var int current_x;
    var int current_y;
    v_getxy(&current_x,&current_y); # get current cursor position
    value1 = fixnum((uintW)current_y);
    value2 = fixnum((uintW)current_x);
    mv_count=2;
  }}

LISPFUNN(set_window_cursor_position,3)
  { check_window_stream(STACK_2);
   {var uintL line = posfixnum_to_L(STACK_1);
    var uintL column = posfixnum_to_L(STACK_0);
    if ((line < (uintL)LINES) && (column < (uintL)COLS))
      { v_gotoxy((int)column,(int)line); }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }}

LISPFUNN(clear_window,1)
  { check_window_stream(popSTACK());
    v_gotoxy(0,0);
    v_clear();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  { check_window_stream(popSTACK());
   {var int current_x;
    var int current_y;
    v_getxy(&current_x,&current_y); # get current cursor position
    v_putn(' ',COLS*(LINES-current_y)-current_x);
    value1 = NIL; mv_count=0;
  }}

LISPFUNN(clear_window_to_eol,1)
  { check_window_stream(popSTACK());
    v_clreol();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  { check_window_stream(popSTACK());
    v_delline(1);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  { check_window_stream(popSTACK());
    v_insline(1);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  { check_window_stream(popSTACK());
    v_attrib(attr_table[screentype][1]);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  { check_window_stream(popSTACK());
    v_attrib(attr_table[screentype][0]);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  { check_window_stream(popSTACK());
    # cursor big: set begin scan to end scan - 4
    v_ctype(cursor_scanlines_end-4,cursor_scanlines_end);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  { check_window_stream(popSTACK());
    # cursor small: set begin scan to end scan - 1
    v_ctype(cursor_scanlines_end-1,cursor_scanlines_end);
    value1 = NIL; mv_count=0;
  }

#endif # MSDOS && EMUNIX_PORTABEL

#if (defined(UNIX) && !defined(NEXTAPP)) || (defined(EMUNIX_PORTABEL) && 0) || defined(RISCOS)

# ------------------------------------------------------------------------------

# Routinen zur Emulation aller VT100-Features auf normalen Terminals.
# Idee: Oliver Laumann 1987

# Benutzt die TERMCAP-Library:
  # Besorgt die Capability-Informationen zu Terminal-Type name.
  # Ergebnis: 1 falls OK, 0 falls name unbekannt, -1 bei sonstigem Fehler.
    extern_C int tgetent (const char* bp, const char* name);
  # Besorgt den Wert einer numerischen Capability (-1 falls nicht vorhanden).
    extern_C int tgetnum (const char* id);
  # Besorgt den Wert einer booleschen Capability (1 falls vorhanden, 0 sonst).
    extern_C int tgetflag (const char* id);
  # Besorgt den Wert einer String-wertigen Capability und (falls area/=NULL)
  # kopiert es nach *area und rückt dabei *area weiter.
    extern_C const char* tgetstr (const char* id, char** area);
  # Besorgt den String, der eine Cursor-Positionierung an Stelle (destcol,destline)
  # bewirkt. (Nötig, da tgetstr("cm") ein spezielles Format hat!)
    extern_C const char* tgoto (const char* cm, int destcol, int destline);
  # Führt eine String-Capability aus. Dazu wird für jedes Character die
  # Ausgabefunktion *outcharfun aufgerufen. (Nötig, da String-Capabilities
  # Padding-Befehle enthalten können!)
    #ifdef __cplusplus
      typedef void (*outcharfun_t) (...);
    #else
      typedef void (*outcharfun_t) ();
    #endif
    extern_C const char* tputs (const char* cp, int affcnt, outcharfun_t outcharfun);

# Einstellbare Wünsche:
  #define WANT_INSERT  FALSE  # Insert-Modus
  #define WANT_SAVE    FALSE  # Save/Restore für die Cursor-Position
  #define WANT_ATTR    TRUE   # Attribute (fett, reverse etc.)
  #define WANT_CHARSET FALSE  # Fonts = Charsets
  # zu definierende Funktionen:
  #define WANT_CURSOR_MOVE         FALSE
  #define WANT_CURSOR_BACKSPACE    FALSE
  #define WANT_CURSOR_RETURN       TRUE
  #define WANT_CURSOR_LINEFEED     TRUE
  #define WANT_CURSOR_REVLINEFEED  FALSE
  #define WANT_CLEAR_SCREEN        TRUE
  #define WANT_CLEAR_FROM_BOS      FALSE
  #define WANT_CLEAR_TO_EOS        TRUE
  #define WANT_CLEAR_LINE          FALSE
  #define WANT_CLEAR_FROM_BOL      FALSE
  #define WANT_CLEAR_TO_EOL        TRUE
  #define WANT_INSERT_1CHAR        FALSE
  #define WANT_INSERT_CHAR         FALSE
  #define WANT_INSERT_LINE         TRUE
  #define WANT_DELETE_CHAR         FALSE
  #define WANT_DELETE_LINE         TRUE
  #define WANT_OUTPUT_1CHAR        TRUE
  # kleine Korrekturen:
  #define WANT_CLEAR_SCREEN        TRUE
  #if WANT_OUTPUT_1CHAR && WANT_INSERT
  #define WANT_INSERT_1CHAR        TRUE
  #endif

# Ausgabe eines Zeichens, direkt.
  local void out_char (uintB c);
  local void out_char(c)
    var uintB c;
    {
      #ifdef GRAPHICS_SWITCH
      switch_text_mode();
      #endif
      restart_it:
     {var int ergebnis = write(stdout_handle,&c,1); # Zeichen auszugeben versuchen
      if (ergebnis<0)
        { if (errno==EINTR) goto restart_it;
          OS_error(); # Error melden
        }
      if (ergebnis==0) # nicht erfolgreich?
        { pushSTACK(var_stream(S(terminal_io),0)); # Wert für Slot PATHNAME von FILE-ERROR
          fehler(file_error,
                 DEUTSCH ? "Kann nichts auf Standard-Output ausgeben." :
                 ENGLISH ? "cannot output to standard output" :
                 FRANCAIS ? "Ne peut rien écrire sur la sortie principale." :
                 ""
                );
        }
    }}

# Ausgabe eines Capability-Strings.
  local void out_capstring (const char* s);
  local void out_capstring(s)
    var const char* s;
    { if (!(s==NULL)) # Absichern gegen nicht vorhandene Capability
        { tputs(s,1,(outcharfun_t) &out_char); }
    }

# Ausgabe eines Capability-Strings mit einem Argument.
  local void out_cap1string (const char* s, int arg);
  local void out_cap1string(s,arg)
    var const char* s;
    var int arg;
    { if (!(s==NULL)) # Absichern gegen nicht vorhandene Capability
        { tputs(tgoto(s,0,arg),1,(outcharfun_t) &out_char); }
    }

# Kosten der Ausführung einer Capability:
  #define EXPENSIVE 1000
  local uintC cost_counter; # Zähler
  # Funktion, die nicht ausgibt, sondern nur zählt:
  local void count_char (char c);
  local void count_char(c)
    var char c;
    { cost_counter++; }
  # Berechnet die Kosten der Ausgabe einer Capability:
  local uintC cap_cost (const char* s);
  local uintC cap_cost(s)
    var const char* s;
    { if (s==NULL)
        { return EXPENSIVE; } # Capability nicht vorhanden
        else
        { cost_counter = 0;
          tputs(s,1,(outcharfun_t) &count_char);
          return cost_counter;
        }
    }

# Buffer für von mir benötigte Capabilities und Pointer da hinein:
  local char tentry[4096];
  local char* tp = &tentry[0];
# Einige ausgewählte Capabilities (NULL oder Pointer in tentry hinein):
  # Insert-Modus:
  local const char* IMcap; # Enter Insert Mode
  local uintC IMcost;
  local const char* EIcap; # End Insert Mode
  local uintC EIcost;
  #if WANT_ATTR
  # Attribute:
  local const char* SOcap; # Enter standout mode
  local const char* SEcap; # End standout mode
  local const char* UScap; # Enter underline mode
  local const char* UEcap; # End underline mode
  local const char* MBcap; # Turn on blinking
  local const char* MDcap; # Turn on bold (extra-bright) mode
  local const char* MHcap; # Turn on half-bright mode
  local const char* MRcap; # Turn on reverse mode
  local const char* MEcap; # Turn off all attributes
  #endif
  #if WANT_CHARSET
  # Zeichensätze:
  local boolean ISO2022; # ob Zeichensatzwechsel nach ISO2022 unterstützt wird
  #endif
  # Cursor-Bewegung:
  local const char* CMcap; # Cursor motion, allgemeine Cursor-Positionierung
  local const char* TIcap; # Initialize mode where CM is usable
  local const char* TEcap; # Exit mode where CM is usable
  local const char* BCcap; # Backspace Cursor
  local uintC BCcost;
  local const char* NDcap; # cursor right
  local uintC NDcost;
  local const char* DOcap; # cursor down
  local uintC DOcost;
  local const char* UPcap; # cursor up
  local uintC UPcost;
  local const char* NLcap; # Newline
  local const char* CRcap; # Carriage Return
  local uintC CRcost;
  # Scrolling:
  local const char* CScap; # change scroll region
  #if WANT_DELETE_LINE
  local const char* SFcap; # Scroll (text up)
  #endif
  #if WANT_CURSOR_REVLINEFEED || WANT_INSERT_LINE
  local const char* SRcap; # Scroll reverse (text down)
  #endif
  # Sonstige:
  local const char* IScap; # Terminal Initialization 2
#  local const char* BLcap; # Bell
#  local const char* VBcap; # Visible Bell (Flash)
  local const char* CLcap; # clear screen, cursor home
  #if WANT_CLEAR_FROM_BOS || WANT_CLEAR_TO_EOS || WANT_CLEAR_LINE || WANT_CLEAR_FROM_BOL || WANT_CLEAR_TO_EOL
  local const char* CEcap; # clear to end of line
  #endif
  #if WANT_CLEAR_TO_EOS
  local const char* CDcap; # clear to end of screen
  #endif
  #if WANT_CURSOR_REVLINEFEED || WANT_INSERT_LINE
  local const char* ALcap; # add new blank line
  #endif
  #if WANT_DELETE_LINE
  local const char* DLcap; # delete line
  #endif
  #if WANT_DELETE_CHAR
  local const char* DCcap; # delete character
  #endif
  #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR
  local const char* ICcap; # insert character
  #endif
  #if WANT_INSERT_CHAR
  local const char* CICcap; # insert count characters
  #endif
  #if WANT_INSERT_LINE
  local const char* CALcap; # add count blank lines
  #endif
  #if WANT_DELETE_CHAR
  local const char* CDCcap; # delete count chars
  #endif
  #if WANT_DELETE_LINE
  local const char* CDLcap; # delete count lines
  #endif
  local boolean AM; # automatic margins, ob an rechterer unterer Ecke scrollt
  local int rows; # Anzahl der Zeilen des Bildschirms, >0
  local int cols; # Anzahl der Spalten des Bildschirms, >0
  # Obere Zeile ist Zeile 0, untere Zeile ist Zeile rows-1.
  # Linke Spalte ist Spalte 0, rechte Spalte ist Spalte cols-1.
  #if WANT_ATTR || WANT_CHARSET
  local uintB* null; # Pointer auf cols Nullen
  #endif
  local uintB* blank; # Pointer auf cols Blanks

# Beschreibung einer Terminal-Ausgabe-Einheit:
typedef struct { uintB** image; # image[y][x] ist das Zeichen an Position (x,y)
                 #if WANT_ATTR
                 uintB** attr;  # attr[y][x] ist sein Attribut
                 uintB curr_attr; # welches Attribut gerade aktuell ist
                 #endif
                 #if WANT_CHARSET
                 uintB** font;  # font[y][x] ist sein Font (Charset)
                 #define charset_count 4
                 uintB charsets[charset_count]; # Tabelle von Zeichensätzen
                 uintC curr_charset; # welcher der Zeichensätze gerade aktuell ist
                                     # (>=0, <charset_count)
                 #endif
                 int x; # Cursorposition (>=0, <=cols)
                 int y; # Cursorposition (>=0, <rows)
                        # (Bei x=cols wird der Cursor in Spalte cols-1 dargestellt.)
                 int top, bot; # Scroll-Region = Zeilen y mit top <= y <= bot,
                               # es ist 0 <= top <= bot <= rows-1.
                 #if WANT_INSERT
                 boolean insert; # ob die Ausgabe-Einheit im Insert-Modus arbeitet
                                 # (dann ist das Terminal meist im Insert-Modus)
                 #endif
                 #if WANT_SAVE
                 boolean saved;
                 #if WANT_ATTR
                 uintB saved_curr_attr;
                 #endif
                 #if WANT_CHARSET
                 uintB saved_charsets[charset_count];
                 uintC saved_curr_charset;
                 #endif
                 int saved_x, saved_y;
                 #endif
               }
        win;

# aktuelle Ausgabe-Einheit:
  local win currwin; # es gibt nur eine!
  #define curr (&currwin)

#if WANT_INSERT

# Insert-Modus ein- bzw. ausschalten:
  # Flag, ob das Terminal im Insert-Modus ist (falls es einen solchen gibt):
  local boolean insert;
  local void set_insert_mode (boolean flag);
  local void set_insert_mode(flag)
    var boolean flag;
    { if (flag)
        # Einschalten
        { if (!insert) { out_capstring(IMcap); } }
        else
        # Ausschalten
        { if (insert) { out_capstring(EIcap); } }
      insert = flag;
    }

#endif

#if WANT_ATTR

# Ausgabe-Attribute des Terminals umschalten:
  local uintB term_attr; # aktuelle Attribute des Terminals
  # mögliche Attribute sind ein ODER aus:
    #define A_SO    bit(0)  # Standout mode
    #define A_US    bit(1)  # Underscore mode
    #define A_BL    bit(2)  # Blinking
    #define A_BD    bit(3)  # Bold mode
    #define A_DI    bit(4)  # Dim mode
    #define A_RV    bit(5)  # Reverse mode
  local void change_attr (uintB new_attr);
  local void change_attr(new_attr)
    var uintB new_attr;
    { var uintB old_attr = term_attr;
      if (old_attr == new_attr) { return; }
      if (   ((old_attr & A_SO) && !(new_attr & A_SO))
          || ((old_attr & A_US) && !(new_attr & A_US))
          || ((old_attr & A_BL) && !(new_attr & A_BL))
          || ((old_attr & A_BD) && !(new_attr & A_BD))
          || ((old_attr & A_DI) && !(new_attr & A_DI))
          || ((old_attr & A_RV) && !(new_attr & A_RV))
         )
        # Muss Attribute ausschalten.
        { out_capstring(UEcap); # alle aus
          out_capstring(SEcap);
          out_capstring(MEcap);
          if (new_attr & A_SO) out_capstring(SOcap); # und selektiv wieder an
          if (new_attr & A_US) out_capstring(UScap);
          if (new_attr & A_BL) out_capstring(MBcap);
          if (new_attr & A_BD) out_capstring(MDcap);
          if (new_attr & A_DI) out_capstring(MHcap);
          if (new_attr & A_RV) out_capstring(MRcap);
        }
        else
        { # selektiv einschalten:
          if ((new_attr & A_SO) && !(old_attr & A_SO)) out_capstring(SOcap);
          if ((new_attr & A_US) && !(old_attr & A_US)) out_capstring(UScap);
          if ((new_attr & A_BL) && !(old_attr & A_BL)) out_capstring(MBcap);
          if ((new_attr & A_BD) && !(old_attr & A_BD)) out_capstring(MDcap);
          if ((new_attr & A_DI) && !(old_attr & A_DI)) out_capstring(MHcap);
          if ((new_attr & A_RV) && !(old_attr & A_RV)) out_capstring(MRcap);
        }
      term_attr = new_attr;
    }

#endif

#if WANT_CHARSET

# Ausgabe-Zeichensatz des Terminals umschalten:
  local uintB term_charset; # aktueller Zeichensatz des Terminals
                            # = curr->charsets[curr->curr_charset]
  #define ASCII 0  # Abkürzung für den Zeichensatz 'B'
  local void change_charset (uintB new);
  local void change_charset(new)
    var uintB new;
    { if (term_charset==new) { return; }
      if (ISO2022)
        { out_char(ESC); out_char('('); out_char(new==ASCII ? 'B' : new); } /*)*/
      term_charset = new;
    }
  # Charset Nr. n auf c schalten:
  local void choose_charset (uintB c, uintC n);
  local void choose_charset(c,n)
    var uintB c;
    var uintC n;
    { if (c=='B') { c = ASCII; }
      if (curr->charsets[n] == c) return;
      curr->charsets[n] = c;
      if (curr->curr_charset == n) # der aktuelle?
        { change_charset(c); }
    }
  # Charset Nr. n aktuell machen:
  local void set_curr_charset (uintC n);
  local void set_curr_charset(n)
    var uintC n;
    { if (curr->curr_charset == n) return;
      curr->curr_charset = n;
      change_charset(curr->charsets[n]);
    }

#endif

# Kosten des Neu-Anzeigens von Zeile y, Zeichen x1..x2-1 berechnen:
# (0 <= y < rows, 0 <= x1 <= x2 <= cols)
  local uintC rewrite_cost (int y, int x1, int x2);
  local uintC rewrite_cost(y,x1,x2)
    var int y;
    var int x1;
    var int x2;
    { if (AM && (y==rows-1) && (x2==cols)) # rechte untere Ecke kann scrollen?
        { return EXPENSIVE; }
     {var int dx = x2-x1;
      if (dx==0) { return 0; }
      #if WANT_ATTR
      {var uintB* p = &curr->attr[y][x1];
       var uintC count;
       dotimespC(count,dx,
         { if (!(*p++ == term_attr)) # Attribut-Wechsel nötig?
             { return EXPENSIVE; }
         });
      }
      #endif
      #if WANT_CHARSET
      {var uintB* p = &curr->font[y][x1];
       var uintC count;
       dotimespC(count,dx,
         { if (!(*p++ == term_charset)) # Font-Wechsel nötig?
             { return EXPENSIVE; }
         });
      }
      #endif
      {var uintC cost = dx;
       #if WANT_INSERT
       if (curr->insert) { cost += EIcost + IMcost; }
       #endif
       return cost;
    }}}

# Bewegt den Cursor von Position (y1,x1) an Position (y2,x2).
# (x1,y1) = (-1,-1) falls aktuelle Position unbekannt.
  local void gofromto (int y1, int x1, int y2, int x2);
  local void gofromto(y1,x1,y2,x2)
    var int y1;
    var int x1;
    var int y2;
    var int x2;
    { if (x2==cols) # Cursor an den rechten Rand?
        { x2--; out_capstring(tgoto(CMcap,x2,y2)); return; } # Bleibt in der letzten Spalte
      if (x1==cols) # Cursor ist am rechten Rand?
        { out_capstring(tgoto(CMcap,x2,y2)); return; } # absolut adressieren
     {var int dy = y2-y1;
      var int dx = x2-x1;
      if ((dy==0) && (dx==0)) { return; }
      if ((y1==-1) || (x1==-1) || (y2 > curr->bot) || (y2 < curr->top))
        { out_capstring(tgoto(CMcap,x2,y2)); return; }
      { var enum { MX_NONE, MX_LE, MX_RI, MX_RW, MX_CR } mx = MX_NONE;
        var enum { MY_NONE, MY_UP, MY_DO } my = MY_NONE;
        # Möglichkeit 1: mit CMcap
        var uintC CMcost = cap_cost(tgoto(CMcap,x2,y2));
        # Möglichkeit 2: mit getrennten x- und y-Bewegungen:
        var uintC xycost = 0;
        if (dx > 0)
          { var uintC cost1 = rewrite_cost(y1,x1,x2);
            var uintC cost2 = dx * NDcost;
            if (cost1 < cost2)
              { mx = MX_RW; xycost += cost1; }
              else
              { mx = MX_RI; xycost += cost2; }
          }
        elif (dx < 0)
          { mx = MX_LE; xycost += (-dx) * BCcost; }
        if (!(dx==0))
          { var uintC cost1 = CRcost + rewrite_cost(y1,0,x2);
            if (cost1 < xycost) { mx = MX_CR; xycost = cost1; }
          }
        if (dy > 0)
          { my = MY_DO; xycost += dy * DOcost; }
        elif (dy < 0)
          { my = MY_UP; xycost += (-dy) * UPcost; }
        if (xycost >= CMcost)
          { out_capstring(tgoto(CMcap,x2,y2)); return; }
        if (!(mx==MX_NONE))
          { if ((mx==MX_LE) || (mx==MX_RI))
              { var const char* s;
                if (mx==MX_LE) { dx = -dx; s = BCcap; } else { s = NDcap; }
                do { out_capstring(s); } until (--dx == 0);
              }
              else
              { if (mx==MX_CR) { out_capstring(CRcap); x1=0; }
                # Hiervon wurden die Kosten mit rewrite_cost berechnet:
                if (x1<x2)
                  {
                    #if WANT_INSERT
                    if (curr->insert) { set_insert_mode(FALSE); }
                    #endif
                    {var uintB* ptr = &curr->image[y1][x1];
                     var uintC count;
                     dotimespC(count,x2-x1, { out_char(*ptr++); });
                    }
                    #if WANT_INSERT
                    if (curr->insert) { set_insert_mode(TRUE); }
                    #endif
              }   }
          }
        if (!(my==MY_NONE))
          { var const char* s;
            if (my==MY_UP) { dy = -dy; s = UPcap; } else { s = DOcap; }
            do { out_capstring(s); } until (--dy == 0);
          }
    }}}

# Redisplay
  # lokale Variablen:
  local int last_x;
  local int last_y;
  # Eine Zeile neu anzeigen, die sich verändert haben kann:
    # nur benötigte Parameter wirklich übergeben:
    #if WANT_ATTR && WANT_CHARSET
      #define RHargs(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,oap,ofp,nsp,nap,nfp,y,x1,x2)
      #define RHparms(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,oap,ofp,nsp,nap,nfp,y,x1,x2)
    #endif
    #if !WANT_ATTR && WANT_CHARSET
      #define RHargs(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,ofp,nsp,nfp,y,x1,x2)
      #define RHparms(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,ofp,nsp,nfp,y,x1,x2,oap,nap)
    #endif
    #if WANT_ATTR && !WANT_CHARSET
      #define RHargs(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,oap,nsp,nap,y,x1,x2)
      #define RHparms(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,oap,nsp,nap,y,x1,x2,ofp,nfp)
    #endif
    #if !WANT_ATTR && !WANT_CHARSET
      #define RHargs(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,nsp,y,x1,x2)
      #define RHparms(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,nsp,y,x1,x2,oap,ofp,nap,nfp)
    #endif
    #undef RHparms
    #define RHparms  RHargs  # korrekt deklarieren
    local void redisplay_help RHparms (uintB* osp, uintB* oap, uintB* ofp, # old
                                       uintB* nsp, uintB* nap, uintB* nfp, # new
                                       int y, int x1, int x2); # Zeile y, von x1 bis x2-1
    local void redisplay_help RHparms(osp,oap,ofp,nsp,nap,nfp,y,x1,x2)
      var uintB* osp;
      var uintB* oap;
      var uintB* ofp;
      var uintB* nsp;
      var uintB* nap;
      var uintB* nfp;
      var int y;
      var int x1;
      var int x2;
      { if (AM && (y == rows-1) && (x2 == cols)) { x2--; }
       {
        #if WANT_ATTR
        var uintB a = term_attr; # letztes Attribut
        #endif
        #if WANT_CHARSET
        var uintB f = term_charset; # letzter Font
        #endif
        var int x = x1;
        osp = &osp[x1]; nsp = &nsp[x1];
        #if WANT_ATTR
        oap = &oap[x1]; nap = &nap[x1];
        #endif
        #if WANT_CHARSET
        ofp = &ofp[x1]; nfp = &nfp[x1];
        #endif
        while (x < x2)
          { if (!((*nsp==*osp)
                  #if WANT_ATTR
                  && (*nap==*oap) && (*nap==a)
                  #endif
                  #if WANT_CHARSET
                  && (*nfp==*nap) && (*nfp==f)
                  #endif
               ) )
              { gofromto(last_y,last_x,y,x);
                #if WANT_ATTR
                a = *nap; if (!(a==term_attr)) { change_attr(a); }
                #endif
                #if WANT_CHARSET
                f = *nfp; if (!(f==term_charset)) { change_charset(f); }
                #endif
                out_char(*nsp);
                last_y = y; last_x = x+1;
              }
            x++;
            osp++; nsp++;
            #if WANT_ATTR
            oap++; nap++;
            #endif
            #if WANT_CHARSET
            ofp++; nfp++;
            #endif
          }
      }}
  #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR || WANT_DELETE_CHAR
  # Eine Zeile neu anzeigen:
    # nur benötigte Parameter wirklich übergeben:
    #if WANT_ATTR && WANT_CHARSET
      #define RLargs(osp,oap,ofp,y,x1,x2) (osp,oap,ofp,y,x1,x2)
      #define RLparms(osp,oap,ofp,y,x1,x2) (osp,oap,ofp,y,x1,x2)
    #endif
    #if !WANT_ATTR && WANT_CHARSET
      #define RLargs(osp,oap,ofp,y,x1,x2) (osp,ofp,y,x1,x2)
      #define RLparms(osp,oap,ofp,y,x1,x2) (osp,ofp,y,x1,x2,oap)
    #endif
    #if WANT_ATTR && !WANT_CHARSET
      #define RLargs(osp,oap,ofp,y,x1,x2) (osp,oap,y,x1,x2)
      #define RLparms(osp,oap,ofp,y,x1,x2) (osp,oap,y,x1,x2,ofp)
    #endif
    #if !WANT_ATTR && !WANT_CHARSET
      #define RLargs(osp,oap,ofp,y,x1,x2) (osp,y,x1,x2)
      #define RLparms(osp,oap,ofp,y,x1,x2) (osp,y,x1,x2,oap,ofp)
    #endif
    #undef RHparms
    #define RHparms  RHargs  # korrekt deklarieren
    local void redisplay_line RLparms (uintB* osp, uintB* oap, uintB* ofp, # old
                                       int y, int x1, int x2); # Zeile y, von x1 bis x2-1
    local void redisplay_line RLparms(osp,oap,ofp,y,x1,x2)
      var uintB* osp;
      var uintB* oap;
      var uintB* ofp;
      var int y;
      var int x1;
      var int x2;
      {
        #if WANT_INSERT
        if (curr->insert) { set_insert_mode(FALSE); }
        #endif
        #if WANT_ATTR
        {var uintB saved_attr = term_attr; change_attr(0);
        #endif
        #if WANT_CHARSET
        {var uintB saved_charset = term_charset; change_charset(ASCII);
        #endif
        last_y = y; last_x = x1;
        redisplay_help RHargs(osp,           oap,          ofp,
                              curr->image[y],curr->attr[y],curr->font[y],
                              y, x1,x2
                             );
        #if WANT_CHARSET
        change_charset(saved_charset); }
        #endif
        #if WANT_ATTR
        change_attr(saved_attr); }
        #endif
        #if WANT_INSERT
        if (curr->insert) { set_insert_mode(TRUE); }
        #endif
      }
  #endif
  # Den ganzen Schirm neu anzeigen:
    local void redisplay (void);
    local void redisplay()
      {
        #if WANT_INSERT
        set_insert_mode(FALSE);
        #endif
        #if WANT_ATTR
        {var uintB saved_attr = term_attr; change_attr(0);
        #endif
        #if WANT_CHARSET
        {var uintB saved_charset = term_charset; change_charset(ASCII);
        #endif
        out_capstring(CLcap); last_x = 0; last_y = 0;
        {var uintC y = 0;
         while (y<rows)
           { redisplay_help RHargs(blank,         null,         null,          # old
                                   curr->image[y],curr->attr[y],curr->font[y], # new
                                   y,                                          # Zeile y
                                   0,cols                                      # alle Spalten
                                  );
             y++;
        }  }
        #if WANT_CHARSET
        change_charset(saved_charset); }
        #endif
        #if WANT_ATTR
        change_attr(saved_attr); }
        #endif
        #if WANT_INSERT
        if (curr->insert) { set_insert_mode(TRUE); }
        #endif
        gofromto(last_y,last_x,curr->y,curr->x);
      }

# Weitere Cursor-Bewegungen:
#if WANT_CURSOR_MOVE

  local void cursor_right (int n);
  local void cursor_right(n)
    var int n;
    { var int x = curr->x;
      if (x==cols) { return; }
     {var int new_x = x + n;
      if (new_x > cols) { new_x = cols; }
      gofromto(curr->y,x,curr->y,curr->x = new_x);
    }}

  local void cursor_left (int n);
  local void cursor_left(n)
    var int n;
    { var int x = curr->x;
      var int new_x = x - n;
      if (new_x < 0) { new_x = 0; }
      gofromto(curr->y,x,curr->y,curr->x = new_x);
    }

  local void cursor_up (int n);
  local void cursor_up(n)
    var int n;
    { var int y = curr->y;
      var int new_y = y - n;
      if (new_y < 0) { new_y = 0; }
      gofromto(y,curr->x,curr->y = new_y,curr->x);
    }

  local void cursor_down (int n);
  local void cursor_down(n)
    var int n;
    { var int y = curr->y;
      var int new_y = y + n;
      if (new_y >= rows) { new_y = rows-1; }
      gofromto(y,curr->x,curr->y = new_y,curr->x);
    }

#endif

# Backspace (Cursor um 1 nach links, innerhalb einer Zeile)
#if WANT_CURSOR_BACKSPACE
  local void cursor_backspace (void);
  local void cursor_backspace()
    { if (curr->x > 0)
        { if (curr->x < cols)
            { if (BCcap)
                { out_capstring(BCcap); }
                else
                { gofromto(curr->y,curr->x,curr->y,curr->x - 1); }
            }
          curr->x = curr->x - 1;
    }   }
#endif

# Return (Cursor an den Anfang der Zeile)
#if WANT_CURSOR_RETURN
  local void cursor_return (void);
  local void cursor_return()
    { if (curr->x > 0) { out_capstring(CRcap); curr->x = 0; } }
#endif

# Hilfroutinen zum Scrollen:
#if WANT_CURSOR_LINEFEED || WANT_DELETE_LINE
  local void scroll_up_help (uintB** pp, uintB filler);
  local void scroll_up_help(pp,filler)
    var uintB** pp;
    var uintB filler;
    { # pp[top..bot] um eins nach links verschieben,
      # pp[top] herausnehmen, löschen und als pp[bot] wieder einhängen:
      pp = &pp[curr->top];
     {var uintC count;
      var uintB* tmp = *pp;
      dotimesC(count,curr->bot - curr->top, { pp[0] = pp[1]; pp++; } );
      {var uintB* p = tmp;
       dotimesC(count,cols, { *p++ = filler; } );
      }
      *pp = tmp;
    }}
  local void scroll_up (void);
  local void scroll_up()
    { scroll_up_help(curr->image,' ');
      #if WANT_ATTR
      scroll_up_help(curr->attr,0);
      #endif
      #if WANT_CHARSET
      scroll_up_help(curr->font,0);
      #endif
    }
#endif
#if WANT_CURSOR_REVLINEFEED || WANT_INSERT_LINE
  local void scroll_down_help (uintB** pp, uintB filler);
  local void scroll_down_help(pp,filler)
    var uintB** pp;
    var uintB filler;
    { # pp[top..bot] um eins nach rechts verschieben,
      # pp[top] herausnehmen, löschen und als pp[bot] wieder einhängen:
      pp = &pp[curr->bot];
     {var uintC count;
      var uintB* tmp = *pp;
      dotimesC(count,curr->bot - curr->top, { pp[0] = pp[-1]; pp--; } );
      {var uintB* p = tmp;
       dotimesC(count,cols, { *p++ = filler; } );
      }
      *pp = tmp;
    }}
  local void scroll_down (void);
  local void scroll_down()
    { scroll_down_help(curr->image,' ');
      #if WANT_ATTR
      scroll_down_help(curr->attr,0);
      #endif
      #if WANT_CHARSET
      scroll_down_help(curr->font,0);
      #endif
    }
#endif

# Linefeed (Cursor um 1 nach unten):
#if WANT_CURSOR_LINEFEED
  local void cursor_linefeed (void);
  local void cursor_linefeed()
    { if (curr->y == curr->bot) { scroll_up(); }
      elif (curr->y < rows-1) { curr->y++; }
      out_capstring(NLcap);
    }
#endif

# Reverse Linefeed (Cursor um 1 nach oben):
#if WANT_CURSOR_REVLINEFEED
  local void cursor_revlinefeed (void);
  local void cursor_revlinefeed()
    { if (curr->y == curr->top)
        { scroll_down();
          if (SRcap)
            { out_capstring(SRcap); }
          elif (ALcap)
            { gofromto(curr->top,curr->x,curr->top,0); # Cursor nach links
              out_capstring(ALcap);
              gofromto(curr->top,0,curr->top,curr->x); # Cursor wieder zurück
            }
          else
            { redisplay(); }
        }
      elif (curr->y > 0)
        { cursor_up(1); }
    }
#endif

# Lösch-Operationen:

# Stück einer Zeile löschen:
#if WANT_CLEAR_SCREEN || WANT_CLEAR_FROM_BOS
  local void cleared_linepart (int y, int x1, int x2);
  local void cleared_linepart(y,x1,x2)
    var int y;
    var int x1;
    var int x2;
    { var int n = x2-x1;
      if (n>0)
        { {var uintB* sp = &curr->image[y][x1];
           var uintC count;
           dotimespC(count,n, { *sp++ = ' '; } );
          }
          #if WANT_ATTR
          {var uintB* ap = &curr->attr[y][x1];
           var uintC count;
           dotimespC(count,n, { *ap++ = 0; } );
          }
          #endif
          #if WANT_CHARSET
          {var uintB* fp = &curr->font[y][x1];
           var uintC count;
           dotimespC(count,n, { *fp++ = 0; } );
          }
          #endif
    }   }
#endif

# Bildschirm löschen:
#if WANT_CLEAR_SCREEN
  local void clear_screen (void);
  local void clear_screen()
    { out_capstring(CLcap);
     {var uintC y = 0;
      while (y<rows) { cleared_linepart(y,0,cols); y++; }
    }}
#endif

# Stück einer Zeile löschen:
#if WANT_CLEAR_FROM_BOS || WANT_CLEAR_TO_EOS || WANT_CLEAR_LINE || WANT_CLEAR_FROM_BOL || WANT_CLEAR_TO_EOL
  local void clear_linepart (int y, int x1, int x2);
  local void clear_linepart(y,x1,x2)
    var int y;
    var int x1;
    var int x2;
    { var int n = x2-x1;
      if (n>0)
        { {var uintB* sp = &curr->image[y][x1];
           var uintC count;
           dotimesC(count,n, { *sp++ = ' '; } );
          }
          #if WANT_ATTR
          {var uintB* ap = &curr->attr[y][x1];
           var uintC count;
           dotimesC(count,n, { *ap++ = 0; } );
          }
          #endif
          #if WANT_CHARSET
          {var uintB* fp = &curr->font[y][x1];
           var uintC count;
           dotimesC(count,n, { *fp++ = 0; } );
          }
          #endif
          if ((x2==cols) && CEcap)
            { gofromto(curr->y,curr->x,y,x1); curr->y = y; curr->x = x1;
              out_capstring(CEcap);
            }
            else
            { if ((x2==cols) && (y==rows-1) && AM) { n--; }
              if (n>0)
                {
                  #if WANT_ATTR
                  {var uintB saved_attr = term_attr; change_attr(0);
                  #endif
                  #if WANT_CHARSET
                  {var uintB saved_charset = term_charset; change_charset(ASCII);
                  #endif
                  #if WANT_INSERT
                  if (curr->insert) { set_insert_mode(FALSE); }
                  #endif
                  gofromto(curr->y,curr->x,y,x1);
                  {var uintC count;
                   dotimespC(count,n, { out_char(' '); } );
                  }
                  curr->y = y; curr->x = x1+n;
                  #if WANT_CHARSET
                  change_charset(saved_charset); }
                  #endif
                  #if WANT_ATTR
                  change_attr(saved_attr); }
                  #endif
                  #if WANT_INSERT
                  if (curr->insert) { set_insert_mode(TRUE); }
                  #endif
    }   }   }   }
#endif

# Bildschirm bis zum Cursor (ausschließlich) löschen:
#if WANT_CLEAR_FROM_BOS
  local void clear_from_BOS (void);
  local void clear_from_BOS()
    { var int y0 = curr->y;
      var int x0 = curr->x;
      var int y = 0;
      while (y<y0) { clear_linepart(y,0,cols); y++; }
      clear_linepart(y0,0,x0);
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Bildschirm ab Cursor (einschließlich) löschen:
#if WANT_CLEAR_TO_EOS
  local void clear_to_EOS (void);
  local void clear_to_EOS()
    { var int y0 = curr->y;
      var int x0 = curr->x;
      if (CDcap)
        { out_capstring(CDcap);
          cleared_linepart(y0,x0,cols);
         {var int y = y0;
          while (++y < rows) { cleared_linepart(y,0,cols); }
        }}
        else
        { clear_linepart(y0,x0,cols);
         {var int y = y0;
          while (++y < rows) { clear_linepart(y,0,cols); }
        }}
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Cursorzeile löschen:
#if WANT_CLEAR_LINE
  local void clear_line (void);
  local void clear_line()
    { var int y0 = curr->y;
      var int x0 = curr->x;
      clear_linepart(y0,0,cols);
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Cursorzeile bis Cursor (ausschließlich) löschen:
#if WANT_CLEAR_FROM_BOL
  local void clear_from_BOL (void);
  local void clear_from_BOL()
    { var int y0 = curr->y;
      var int x0 = curr->x;
      clear_linepart(y0,0,x0);
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Cursorzeile ab Cursor (einschließlich) löschen:
#if WANT_CLEAR_TO_EOL
  local void clear_to_EOL (void);
  local void clear_to_EOL()
    { var int y0 = curr->y;
      var int x0 = curr->x;
      clear_linepart(y0,x0,cols);
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Einfüge-Operationen:

# alter Zeileninhalt:
#if WANT_INSERT_1CHAR || WANT_INSERT_CHAR || WANT_DELETE_CHAR
  local uintB* old_image_y;
  #if WANT_ATTR
  local uintB* old_attr_y;
  #endif
  #if WANT_CHARSET
  local uintB* old_font_y;
  #endif
  local void save_line_old (int y);
  local void save_line_old(y)
    var int y;
    { {var uintB* p1 = &curr->image[y][0];
       var uintB* p2 = &old_image_y[0];
       var uintC count;
       dotimesC(count,cols, { *p2++ = *p1++; } );
      }
      #if WANT_ATTR
      {var uintB* p1 = &curr->attr[y][0];
       var uintB* p2 = &old_attr_y[0];
       var uintC count;
       dotimesC(count,cols, { *p2++ = *p1++; } );
      }
      #endif
      #if WANT_CHARSET
      {var uintB* p1 = &curr->font[y][0];
       var uintB* p2 = &old_font_y[0];
       var uintC count;
       dotimesC(count,cols, { *p2++ = *p1++; } );
      }
      #endif
    }
#endif

# Ein Zeichen einfügen:
#if WANT_INSERT_1CHAR
  local void insert_1char (uintB c);
  local void insert_1char(c)
    var uintB c;
    { var int y = curr->y;
      var int x = curr->x;
      if (x==cols) { x--; } # nicht über den rechten Rand schreiben!
      if (ICcap || IMcap)
        { curr->image[y][x] = c;
          #if WANT_ATTR
          curr->attr[y][x] = curr->curr_attr;
          #endif
          #if WANT_CHARSET
          curr->font[y][x] = curr->charsets[curr->curr_charset]; # = term_charset
          #endif
          #if WANT_INSERT
          if (!curr->insert)
          #endif
            { set_insert_mode(TRUE); }
          out_capstring(ICcap); out_char(c);
          #if WANT_INSERT
          if (!curr->insert)
          #endif
            { set_insert_mode(FALSE); }
          curr->x = x+1;
        }
        else
        { # alten Zeileninhalt retten:
          save_line_old(y);
          # neuen Zeileninhalt bilden:
          {var uintB* p1 = &curr->image[y][x];
           var uintB* p2 = &old_image[x];
           var uintC count;
           *p1++ = c;
           dotimesC(count,cols-1-x, { *p1++ = *p2++; } );
          }
          #if WANT_ATTR
          {var uintB* p1 = &curr->attr[y][x];
           var uintB* p2 = &old_attr[x];
           var uintC count;
           *p1++ = curr->curr_attr;
           dotimesC(count,cols-1-x, { *p1++ = *p2++; } );
          }
          #endif
          #if WANT_CHARSET
          {var uintB* p1 = &curr->font[y][x];
           var uintB* p2 = &old_font[x];
           var uintC count;
           *p1++ = term_charset; # = curr->charsets[curr->curr_charset]
           dotimesC(count,cols-1-x, { *p1++ = *p2++; } );
          }
          #endif
          # Zeile anzeigen:
          redisplay_line RLargs(old_image,old_attr,old_font,y,x,cols);
          x++;
          gofromto(last_y,last_x,y,x); curr->x = x;
    }   }
#endif

# Platz für n Zeichen machen:
#if WANT_INSERT_CHAR
  local void insert_char (uintC n);
  local void insert_char(n)
    var uintC n;
    { var int y = curr->y;
      var int x = curr->x;
      if (n > cols-x) { n = cols-x; }
      if (n==0) return;
      # alten Zeileninhalt retten:
      save_line_old(y);
      # neuen Zeileninhalt bilden:
      {var uintB* p1 = &curr->image[y][x];
       var uintB* p2 = &old_image[x];
       var uintC count;
       dotimespC(count,n, { *p1++ = ' '; } );
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
      }
      #if WANT_ATTR
      {var uintB* p1 = &curr->attr[y][x];
       var uintB* p2 = &old_attr[x];
       var uintC count;
       dotimespC(count,n, { *p1++ = 0; } );
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
      }
      #endif
      #if WANT_CHARSET
      {var uintB* p1 = &curr->font[y][x];
       var uintB* p2 = &old_font[x];
       var uintC count;
       dotimespC(count,n, { *p1++ = 0; } );
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
      }
      #endif
      if (CICcap && (n > 1))
        {
          #if WANT_INSERT
          if (curr->insert) { set_insert_mode(FALSE); }
          #endif
          out_cap1string(CICcap,n);
          {var uintC count;
           dotimespC(count,n, { out_char(' '); } );
          }
          #if WANT_INSERT
          if (curr->insert) { set_insert_mode(TRUE); }
          #endif
          gofromto(y,x+n,y,x);
        }
      elif (ICcap || IMcap)
        {
          #if WANT_INSERT
          if (!curr->insert)
          #endif
            { set_insert_mode(TRUE); }
          {var uintC count;
           dotimespC(count,n, { out_capstring(ICcap); out_char(' '); } );
          }
          #if WANT_INSERT
          if (!curr->insert)
          #endif
            { set_insert_mode(FALSE); }
          gofromto(y,x+n,y,x);
        }
      else
        { redisplay_line RLargs(old_image,old_attr,old_font,y,x,cols);
          gofromto(last_y,last_x,y,x);
        }
    }
#endif

# Zeilen einfügen:
#if WANT_INSERT_LINE
  local void insert_line (uintC n);
  local void insert_line(n)
    var uintC n;
    { if (n > curr->bot - curr->y + 1) { n = curr->bot - curr->y + 1; }
      if (n==0) return;
     {var int oldtop = curr->top;
      curr->top = curr->y;
      {var uintC count;
       dotimespC(count,n, { scroll_down(); } );
      }
      if (ALcap || CALcap)
        { gofromto(curr->y,curr->x,curr->y,0); # an den Zeilenanfang
          if ((CALcap && (n>1)) || !ALcap)
            { out_cap1string(CALcap,n); }
            else
            { var uintC count;
              dotimespC(count,n, { out_capstring(ALcap); } );
            }
          gofromto(curr->y,0,curr->y,curr->x);
        }
      elif (CScap && SRcap)
        { out_capstring(tgoto(CScap,curr->bot,curr->top));
          gofromto(-1,-1,curr->top,0);
          {var uintC count;
           dotimespC(count,n, { out_capstring(SRcap); } );
          }
          out_capstring(tgoto(CScap,curr->bot,oldtop));
          gofromto(-1,-1,curr->y,curr->x);
        }
      else
        { redisplay(); }
      curr->top = oldtop;
    }}
#endif

# Lösch-Operationen:

# Characters löschen:
#if WANT_DELETE_CHAR
  local void delete_char (uintC n);
  local void delete_char(n)
    var uintC n;
    { var int y = curr->y;
      var int x = curr->x;
      if (n > cols-x) { n = cols-x; }
      if (n==0) return;
      # alten Zeileninhalt retten:
      save_line_old(y);
      # neuen Zeileninhalt bilden:
      {var uintB* p1 = &curr->image[y][x];
       var uintB* p2 = &old_image[x];
       var uintC count;
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
       dotimespC(count,n, { *p1++ = ' '; } );
      }
      #if WANT_ATTR
      {var uintB* p1 = &curr->attr[y][x];
       var uintB* p2 = &old_attr[x];
       var uintC count;
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
       dotimespC(count,n, { *p1++ = 0; } );
      }
      #endif
      #if WANT_CHARSET
      {var uintB* p1 = &curr->font[y][x];
       var uintB* p2 = &old_font[x];
       var uintC count;
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
       dotimespC(count,n, { *p1++ = 0; } );
      }
      #endif
      if (CDCcap && ((n>1) || !DCcap))
        { out_cap1string(CDCcap,n); }
      elif (DCcap)
        { var uintC count;
          dotimespC(count,n, { out_capstring(DCcap); } );
        }
      else
        { redisplay_line RLargs(old_image,old_attr,old_font,y,x,cols);
          gofromto(last_y,last_x,y,x);
        }
    }
#endif

# Zeilen löschen:
#if WANT_DELETE_LINE
  local void delete_line (uintC n);
  local void delete_line(n)
    var uintC n;
    { if (n > curr->bot - curr->y + 1) { n = curr->bot - curr->y + 1; }
      if (n==0) return;
     {var int oldtop = curr->top;
      curr->top = curr->y;
      {var uintC count;
       dotimespC(count,n, { scroll_up(); } );
      }
      if (DLcap || CDLcap)
        { gofromto(curr->y,curr->x,curr->y,0); # an den Zeilenanfang
          if ((CDLcap && (n>1)) || !DLcap)
            { out_cap1string(CDLcap,n); }
            else
            { var uintC count;
              dotimespC(count,n, { out_capstring(DLcap); } );
            }
          gofromto(curr->y,0,curr->y,curr->x);
        }
      elif (CScap)
        { out_capstring(tgoto(CScap,curr->bot,curr->top));
          gofromto(-1,-1,curr->bot,0);
          {var uintC count;
           dotimespC(count,n, { out_capstring(SFcap); } );
          }
          out_capstring(tgoto(CScap,curr->bot,oldtop));
          gofromto(-1,-1,curr->y,curr->x);
        }
      else
        { redisplay(); }
      curr->top = oldtop;
    }}
#endif

# Ein Zeichen ausgeben:
#if WANT_OUTPUT_1CHAR
  local void output_1char (uintB c);
  local void output_1char(c)
    var uintB c;
    {
      #if WANT_INSERT
      if (curr->insert)
        { insert_1char(c); }
        else
      #endif
        { var int y = curr->y;
          var int x = curr->x;
          if (x==cols) { x--; } # nicht über den rechten Rand schreiben!
          curr->image[y][x] = c;
          #if WANT_ATTR
          curr->attr[y][x] = curr->curr_attr;
          #endif
          #if WANT_CHARSET
          curr->font[y][x] = curr->charsets[curr->curr_charset]; # = term_charset
          #endif
          x++;
          if (!(AM && (x==cols) && (curr->y==curr->bot))) # rechte untere Ecke evtl. freilassen
            { out_char(c); } # Zeichen ausgeben
          curr->x = x; # Cursor rückt um eins weiter
          if (x==cols) # außer wenn er schon ganz rechts war
            { gofromto(-1,-1,curr->y,curr->x); }
    }   }
#endif

#if WANT_SAVE

# gespeicherte Cursor-Position:
  local void save_cursor (void);
  local void save_cursor()
    { curr->saved_x = curr->x;
      curr->saved_y = curr->y;
      #if WANT_ATTR
      curr->saved_curr_attr = curr->curr_attr;
      #endif
      #if WANT_CHARSET
      curr->saved_curr_charset = curr->curr_charset;
      {var uintC i = 0;
       while (i<charset_count) { curr->saved_charsets[i] = curr->charsets[i]; i++; }
      }
      #endif
      curr->saved = TRUE;
    }
  local void restore_cursor (void);
  local void restore_cursor()
    { if (curr->saved)
        { gofromto(curr->y,curr->x,curr->saved_y,curr->saved_x);
          curr->y = curr->saved_y; curr->x = curr->saved_x;
          #if WANT_ATTR
          curr->curr_attr = curr->saved_curr_attr;
          change_attr(curr->curr_attr);
          #endif
          #if WANT_CHARSET
          curr->curr_charset = curr->saved_curr_charset;
          {var uintC i = 0;
           while (i<charset_count) { curr->charsets[i] = curr->saved_charsets[i]; i++; }
          }
          change_charset(curr->charsets[curr->curr_charset]);
          #endif
    }   }

#endif

# Initialisiert das Terminal.
# Liefert NULL falls OK, einen Fehlerstring sonst.
  local boolean term_initialized = FALSE;
  local const char * init_term (void);
  local const char * init_term()
    { var char tbuf[4096]; # interner Buffer für die Termcap-Routinen
      if (term_initialized) { return NULL; } # schon initialisiert -> OK
      # Terminal-Typ abfragen:
      begin_system_call();
      { var const char* s = getenv("TERM");
        if (s==NULL)
          { end_system_call();
            return (DEUTSCH ? "Environment enthält keine TERM-Variable." :
                    ENGLISH ? "environment has no TERM variable" :
                    FRANCAIS ? "L'environnment ne contient pas de variable TERM." :
                    ""
                   );
          }
        if (!(tgetent(tbuf,s)==1))
          { end_system_call();
            pushSTACK(asciz_to_string(s));
            return (DEUTSCH ? "TERMCAP kennt Terminal-Typ ~ nicht." :
                    ENGLISH ? "terminal type ~ unknown to termcap" :
                    FRANCAIS ? "TERMCAP ne connait pas le type d'écran ~." :
                    ""
                   );
          }
      }
      { var int i = tgetnum("co");
        cols = (i>0 ? i : 80);
      }
      { var int i = tgetnum("li");
        rows = (i>0 ? i : 24);
      }
      #ifdef EMUNIX
      # Obwohl das eigentlich unsauber ist, holen wir uns die aktuelle Bildschirm-
      # größe mit _scrsize().
      { var int scrsize[2];
        _scrsize(&!scrsize);
        if (scrsize[0] > 0) { cols = scrsize[0]; }
        if (scrsize[1] > 0) { rows = scrsize[1]; }
      }
      #endif
      if (tgetflag("hc"))
        { end_system_call();
          return (DEUTSCH ? "Unzureichendes Terminal: Hardcopy-Terminal." :
                  ENGLISH ? "insufficient terminal: hardcopy terminal" :
                  FRANCAIS ? "Terminal insuffisant : imprimante au lieu d'écran." :
                  ""
                 );
        }
      if (tgetflag("os"))
        { end_system_call();
          return (DEUTSCH ? "Unzureichendes Terminal: Kann Ausgegebenes nicht mehr löschen." :
                  ENGLISH ? "insufficient terminal: overstrikes, cannot clear output" :
                  FRANCAIS ? "Terminal insuffisant : ne peut rien effacer." :
                  ""
                 );
        }
      if (tgetflag("ns"))
        { end_system_call();
          return (DEUTSCH ? "Unzureichendes Terminal: Kann nicht scrollen." :
                  ENGLISH ? "insufficient terminal: cannot scroll" :
                  FRANCAIS ? "Terminal insuffisant : pas de défilement." :
                  ""
                 );
        }
      if (!(CLcap = tgetstr("cl",&tp)))
        { # Könnte CLcap = "\n\n\n\n"; als Default nehmen ('weird HPs')
          end_system_call();
          return (DEUTSCH ? "Unzureichendes Terminal: Kann Bildschirm nicht löschen." :
                  ENGLISH ? "insufficient terminal: cannot clear screen" :
                  FRANCAIS ? "Terminal insuffisant : ne peut pas effacer l'écran." :
                  ""
                 );
        }
      if (!(CMcap = tgetstr("cm",&tp)))
        { end_system_call();
          return (DEUTSCH ? "Unzureichendes Terminal: Kann Cursor nicht willkürlich positionieren." :
                  ENGLISH ? "insufficient terminal: cannot position cursor randomly" :
                  FRANCAIS ? "Terminal insuffisant : ne peut pas placer le curseur n'importe où." :
                  ""
                 );
        }
      # Capabilities initialisieren:
      AM = tgetflag("am"); if (tgetflag("LP")) { AM = FALSE; }
      TIcap = tgetstr("ti",&tp);
      TEcap = tgetstr("te",&tp);
      # BLcap = tgetstr("bl",&tp); if (!BLcap) BLcap = "\007";
      # VBcap = tgetstr("vb",&tp);
      BCcap = tgetstr("bc",&tp); if (!BCcap) BCcap = (tgetflag("bs") ? "\b" : tgetstr("le",&tp));
      CRcap = tgetstr("cr",&tp); if (!CRcap) CRcap = "\r";
      NLcap = tgetstr("nl",&tp); if (!NLcap) NLcap = "\n";
      DOcap = tgetstr("do",&tp); if (!DOcap) DOcap = NLcap;
      UPcap = tgetstr("up",&tp);
      NDcap = tgetstr("nd",&tp);
      IScap = tgetstr("is",&tp);
      #if WANT_ATTR
      if ((tgetnum("sg") > 0) || (tgetnum("ug") > 0))
        # Beim Umschalten in Standout-Mode oder beim Umschalten in den
        # Underline-Mode gibt's Leerstellen -> unbrauchbar!
        { SOcap = NULL; SEcap = NULL; UScap = NULL; UEcap = NULL;
          MBcap = NULL; MDcap = NULL; MHcap = NULL; MRcap = NULL; MEcap = NULL;
        }
        else
        { SOcap = tgetstr("so",&tp);
          SEcap = tgetstr("se",&tp);
          UScap = tgetstr("us",&tp);
          UEcap = tgetstr("ue",&tp);
          if (!UScap && !UEcap) # kein Underline?
            { UScap = SOcap; UEcap = SEcap; } # nimm Standout als Ersatz
          MBcap = tgetstr("mb",&tp);
          MDcap = tgetstr("md",&tp);
          MHcap = tgetstr("mh",&tp);
          MRcap = tgetstr("mr",&tp);
          MEcap = tgetstr("me",&tp);
          # Does ME also reverse the effect of SO and/or US?  This is not
          # clearly specified by the termcap manual.
          # Anyway, we should at least look whether ME/SE/UE are equal:
          if (UEcap && SEcap && asciz_equal(UEcap,SEcap)) { UEcap = NULL; }
          if (UEcap && MEcap && asciz_equal(UEcap,MEcap)) { UEcap = NULL; }
          if (SEcap && MEcap && asciz_equal(SEcap,MEcap)) { SEcap = NULL; }
          # tgetstr("uc",&tp) liefert ein underline-character. Dann jeweils
          # in redisplay_help() und output_1char() nach dem out_char() noch
          # backspace() und out_capstring(UCcap) durchführen.
          # Für welche Terminals lohnt sich das??
        }
      #endif
      #if WANT_CHARSET
      ISO2022 = tgetflag("G0");
      #endif
      CScap = tgetstr("cs",&tp);
      #if WANT_DELETE_LINE
      SFcap = tgetstr("sf",&tp); if (!SFcap) SFcap = NLcap;
      #endif
      #if WANT_CURSOR_REVLINEFEED || WANT_INSERT_LINE
      SRcap = tgetstr("sr",&tp);
      #endif
      #if WANT_CLEAR_FROM_BOS || WANT_CLEAR_TO_EOS || WANT_CLEAR_LINE || WANT_CLEAR_FROM_BOL || WANT_CLEAR_TO_EOL
      CEcap = tgetstr("ce",&tp);
      #endif
      #if WANT_CLEAR_TO_EOS
      CDcap = tgetstr("cd",&tp);
      #endif
      #if WANT_CURSOR_REVLINEFEED || WANT_INSERT_LINE
      ALcap = tgetstr("al",&tp);
      #endif
      #if WANT_DELETE_LINE
      DLcap = tgetstr("dl",&tp);
      #endif
      #if WANT_DELETE_CHAR
      DCcap = tgetstr("dc",&tp);
      #endif
      #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR
      ICcap = tgetstr("ic",&tp);
      #endif
      #if WANT_INSERT_CHAR
      CICcap = tgetstr("IC",&tp);
      #endif
      #if WANT_INSERT_LINE
      CALcap = tgetstr("AL",&tp);
      #endif
      #if WANT_DELETE_CHAR
      CDCcap = tgetstr("DC",&tp);
      #endif
      #if WANT_DELETE_LINE
      CDLcap = tgetstr("DL",&tp);
      #endif
      IMcap = tgetstr("im",&tp);
      EIcap = tgetstr("ei",&tp);
      if (tgetflag ("in")) # Insert-Modus unbrauchbar?
        { IMcap = NULL; EIcap = NULL;
          #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR
          ICcap = NULL;
          #endif
          #if WANT_INSERT_CHAR
          CICcap = NULL;
          #endif
        }
      if (IMcap && (IMcap[0]==0)) { IMcap = NULL; } # IMcap leer?
      if (EIcap && (EIcap[0]==0)) { EIcap = NULL; } # EIcap leer?
      #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR
      if (ICcap && (ICcap[0]==0)) { ICcap = NULL; } # ICcap leer?
      #endif
      # Kosten der Capabilities berechnen:
      IMcost = cap_cost(IMcap);
      EIcost = cap_cost(EIcap);
      BCcost = cap_cost(BCcap);
      NDcost = cap_cost(NDcap);
      DOcost = cap_cost(DOcap);
      #ifndef NL_HACK
      # Falls DOcap ein LF ausgibt, ist nicht sicher, ob dies auch als solches
      # (und nicht als CR/LF) beim Terminal ankommt. In diesem Fall erklären
      # wir DOcap für unbrauchbar. Das erspart uns den NL_HACK.
      if (DOcap[0]=='\n') { DOcost = EXPENSIVE; }
      #endif
      UPcost = cap_cost(UPcap);
      CRcost = cap_cost(CRcap);
      # Hilfs-Datenstrukturen bereitstellen:
      {var uintB* ptr = (uintB*) malloc(cols*sizeof(uintB));
       var uintC count;
       blank = ptr;
       dotimespC(count,cols, { *ptr++ = ' '; } );
      }
      #if WANT_ATTR || WANT_CHARSET
      {var uintB* ptr = (uintB*) malloc(cols*sizeof(uintB));
       var uintC count;
       null = ptr;
       dotimespC(count,cols, { *ptr++ = 0; } );
      }
      #endif
      #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR || WANT_DELETE_CHAR
      old_image_y = (uintB*) malloc(cols*sizeof(uintB));
      #if WANT_ATTR
      old_attr_y = (uintB*) malloc(cols*sizeof(uintB));
      #endif
      #if WANT_CHARSET
      old_font_y = (uintB*) malloc(cols*sizeof(uintB));
      #endif
      #endif
      end_system_call();
      term_initialized = TRUE;
      return NULL;
    }

#ifdef NL_HACK

# Wenn NLcap = "\n" ist, müssen wir ein "stty -onlcr" durchführen, weil sonst
# das NL vom Terminal-Driver in ein CR umgewandelt wird, bevor es beim
# Terminal ankommt.
  local void term_nlraw (void);
  local void term_nlunraw (void);
#if defined(UNIX_TERM_TERMIOS)
  static unsigned long old_c_oflag = 0;
  local void term_nlraw()
    { var struct termios oldtermio;
      if (!( tcgetattr(stdout_handle,&oldtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
      old_c_oflag = oldtermio.c_oflag;
      oldtermio.c_oflag &= ~ONLCR;
      if (!( TCSETATTR(stdout_handle,TCSAFLUSH,&oldtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
    }
  local void term_nlunraw()
    { if (old_c_oflag & ONLCR)
        { var struct termios oldtermio;
          if (!( tcgetattr(stdout_handle,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          oldtermio.c_oflag |= ONLCR;
          if (!( TCSETATTR(stdout_handle,TCSAFLUSH,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
    }   }
#elif defined(UNIX_TERM_TERMIO) || defined(EMUNIX)
  static unsigned long old_c_oflag = 0;
  local void term_nlraw()
    { var struct termio oldtermio;
      if (!( ioctl(stdout_handle,TCGETA,&oldtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
      old_c_oflag = oldtermio.c_oflag;
      oldtermio.c_oflag &= ~ONLCR;
      if (!( ioctl(stdout_handle,TCSETAF,&oldtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
    }
  local void term_nlunraw()
    { if (old_c_oflag & ONLCR)
        { var struct termio oldtermio;
          if (!( ioctl(stdout_handle,TCGETA,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          oldtermio.c_oflag |= ONLCR;
          if (!( ioctl(stdout_handle,TCSETAF,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
    }   }
#elif defined(UNIX_TERM_SGTTY)
  static unsigned long old_sg_flags = 0;
  local void term_nlraw()
    { var struct sgttyb oldsgttyb;
      if (!( ioctl(stdout_handle,TIOCGETP,&oldsgttyb) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
      old_sg_flags = oldsgttyb.sg_flags;
      oldsgttyb.sg_flags &= ~CRMOD;
      if (!( ioctl(stdout_handle,TIOCSETP,&oldsgttyb) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
    }
  local void term_nlunraw()
    { if (old_sg_flags & CRMOD)
        { var struct sgttyb oldsgttyb;
          if (!( ioctl(stdout_handle,TIOCGETP,&oldsgttyb) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          oldsgttyb.sg_flags |= CRMOD;
          if (!( ioctl(stdout_handle,TIOCSETP,&oldsgttyb) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
    }   }
#endif

#endif # NL_HACK

# Beginn des Arbeitens mit diesem Paket:
  local void start_term (void);
  local void start_term()
    {
      #ifdef NL_HACK
      if (NLcap[0] == '\n') { term_nlraw(); }
      #endif
      out_capstring (IScap);
      out_capstring (TIcap);
    }

# Ende des Arbeitens mit diesem Paket:
  local void end_term (void);
  local void end_term()
    { out_capstring (TEcap);
      out_capstring (IScap);
      #ifdef MSDOS # wie testet man auf Farb-ANSI-Terminal??
      # Auf ANSI-Terminals mit mehreren Farben: TEcap setzt die Farben zurück.
      out_capstring(CLcap); # Bildschirm löschen, diesmal in der normalen Farbe
      #endif
      #ifdef NL_HACK
      if (NLcap[0] == '\n') { term_nlunraw(); }
      #endif
    }

# Initialisiert das Window curr.
  local void init_curr (void);
  local void init_curr()
    { {var uintB** ptr = (uintB**) malloc(rows*sizeof(uintB*));
       var uintC count;
       curr->image = ptr;
       dotimespC(count,rows, { *ptr++ = (uintB*) malloc(cols*sizeof(uintB)); } );
      }
      #if WANT_ATTR
      {var uintB** ptr = (uintB**) malloc(rows*sizeof(uintB*));
       var uintC count;
       curr->attr = ptr;
       dotimespC(count,rows, { *ptr++ = (uintB*) malloc(cols*sizeof(uintB)); } );
      }
      # Attribute ausschalten:
      out_capstring(UEcap); # alle aus
      out_capstring(SEcap);
      out_capstring(MEcap);
      term_attr = curr->curr_attr = 0;
      #endif
      #if WANT_CHARSET
      {var uintB** ptr = (uintB**) malloc(rows*sizeof(uintB*));
       var uintC count;
       curr->font = ptr;
       dotimespC(count,rows, { *ptr++ = (uintB*) malloc(cols*sizeof(uintB)); } );
      }
      {var uintC i = 0;
       while (i<charset_count) { curr->charsets[i] = ASCII; i++; }
      }
      curr->curr_charset = 0;
      if (ISO2022) { out_char(ESC); out_char('('); out_char('B'); } /*)*/
      term_charset = ASCII;
      #endif
      curr->x = 0; curr->y = 0;
      curr->top = 0; curr->bot = rows-1;
      #if WANT_INSERT
      curr->insert = FALSE;
      #endif
      #if WANT_SAVE
      curr->saved = FALSE;
      #endif
      if (CScap) { out_capstring(tgoto(CScap,curr->bot,curr->top)); }
      clear_screen();
    }

# ------------------------------------------------------------------------------

# UP: Ein Zeichen auf einen Window-Stream ausgeben.
# wr_ch_window(&stream,ch);
# > stream: Window-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_window (const object* stream_, object ch);
  local void wr_ch_window(stream_,ch)
    var const object* stream_;
    var object ch;
    { if (!charp(ch)) { fehler_wr_char(*stream_,ch); } # ch sollte Character sein
     {var uintB c = char_code(ch); # Code des Zeichens
      begin_system_call();
      if (graphic_char_p(c))
        { if (curr->x == cols) { cursor_return(); cursor_linefeed(); } # Wrap!
          output_1char(c);
        }
      elif (c == NL)
        { cursor_return(); cursor_linefeed(); }
      elif (c == BS)
        { var int x0 = curr->x;
          if (x0>0)
            { var int y0 = curr->y;
              clear_linepart(y0,x0-1,x0);
              gofromto(curr->y,curr->x,y0,x0-1); curr->y = y0; curr->x = x0-1;
        }   }
      end_system_call();
    }}

LISPFUNN(make_window,0)
  { var object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+1,0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_dummy); # READ-CHAR unmöglich
      s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      s->strm_wr_ss = P(wr_ss_dummy_nogc);
    # Initialisieren:
    begin_system_call();
    {var const char * ergebnis = init_term();
     if (!(ergebnis==NULL)) { fehler(error,ergebnis); }
    }
    start_term();
    init_curr();
    end_system_call();
    value1 = stream; mv_count=1;
  }

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var object stream;
    { begin_system_call();
      end_term();
      end_system_call();
    }

LISPFUNN(window_size,1)
  { check_window_stream(popSTACK());
    value1 = fixnum(rows); # Variablen rows,cols abfragen
    value2 = fixnum(cols);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  { check_window_stream(popSTACK());
    value1 = fixnum(curr->y);
    value2 = fixnum(curr->x);
    mv_count=2;
  }

LISPFUNN(set_window_cursor_position,3)
  { check_window_stream(STACK_2);
   {var uintL line = posfixnum_to_L(STACK_1);
    var uintL column = posfixnum_to_L(STACK_0);
    if ((line < rows) && (column < cols))
      { begin_system_call();
        gofromto(curr->y,curr->x,line,column); # Cursor positionieren
        curr->y = line; curr->x = column;
        end_system_call();
      }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }}

LISPFUNN(clear_window,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    clear_screen();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    clear_to_EOS();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    clear_to_EOL();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    delete_line(1);
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    insert_line(1);
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    change_attr(curr->curr_attr |= A_US);
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    change_attr(curr->curr_attr &= ~A_US);
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  { check_window_stream(popSTACK());
    # Cursor ist permanent an!
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  { check_window_stream(popSTACK());
    # geht nicht, da Cursor permanent an!
    value1 = NIL; mv_count=0;
  }

#endif # (UNIX && !NEXTAPP) || (EMUNIX_PORTABEL && 0) || RISCOS

#if defined(MAYBE_NEXTAPP) && defined(NEXTAPP)

# Alles unimplementiert.

# Fehlermeldung.
  nonreturning_function(local, fehler_screen, (void));
  local void fehler_screen()
    { pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             DEUTSCH ? "~: Paket SCREEN ist nicht implementiert." :
             ENGLISH ? "~: package SCREEN is not implemented" :
             FRANCAIS ? "~ : Le paquet SCREEN n'est pas implémenté." :
             ""
            );
    }

LISPFUNN(make_window,0)
  { fehler_screen(); }

#define close_window(stream)  fehler_screen()

LISPFUNN(window_size,1)
  { fehler_screen(); }

LISPFUNN(window_cursor_position,1)
  { fehler_screen(); }

LISPFUNN(set_window_cursor_position,3)
  { fehler_screen(); }

LISPFUNN(clear_window,1)
  { fehler_screen(); }

LISPFUNN(clear_window_to_eot,1)
  { fehler_screen(); }

LISPFUNN(clear_window_to_eol,1)
  { fehler_screen(); }

LISPFUNN(delete_window_line,1)
  { fehler_screen(); }

LISPFUNN(insert_window_line,1)
  { fehler_screen(); }

LISPFUNN(highlight_on,1)
  { fehler_screen(); }

LISPFUNN(highlight_off,1)
  { fehler_screen(); }

LISPFUNN(window_cursor_on,1)
  { fehler_screen(); }

LISPFUNN(window_cursor_off,1)
  { fehler_screen(); }

#endif # NEXTAPP

#if defined(UNIX) && 0

# Normales CURSES-Paket, wir benutzen nur stdscr.

#undef BS
#undef CR
#undef NL
#include <curses.h>
#undef OK
#define CR  13
#define NL  10

# UP: Ein Zeichen auf einen Window-Stream ausgeben.
# wr_ch_window(&stream,ch);
# > stream: Window-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_window (const object* stream_, object ch);
  local void wr_ch_window(stream_,ch)
    var const object* stream_;
    var object ch;
    { if (!charp(ch)) { fehler_wr_char(*stream_,ch); } # ch sollte Character sein
     {var uintB c = char_code(ch); # Code des Zeichens
      begin_system_call();
      if (graphic_char_p(c)) # nur druckbare Zeichen auf den Bildschirm lassen
        { addch(c); }
      elif (c == NL) # NL in CR/LF umwandeln
        { addch(CR); addch(LF); }
      else # etwas ausgeben, damit die Cursorposition stimmt
        { addch('?'); }
      end_system_call();
    }}

LISPFUNN(make_window,0)
  { var object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+1,0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_dummy); # READ-CHAR unmöglich
      s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      s->strm_wr_ss = P(wr_ss_dummy_nogc);
    begin_system_call();
    initscr(); # Curses initialisieren # Was ist, wenn das abstürzt?? newterm() benutzen??
    cbreak(); noecho(); # Input nicht zeilengebuffert, ohne Echo
    #if defined(SUN3) || defined(SUN4)
    keypad(stdscr,TRUE); # Funktionstasten-Erkennung einschalten
    #endif
    end_system_call();
    value1 = stream; mv_count=1;
  }

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var object stream;
    { begin_system_call();
      nocbreak(); echo(); # Input wieder zeilengebuffert, mit Echo
      #if defined(SUN3) || defined(SUN4)
      keypad(stdscr,FALSE); # Funktionstasten-Erkennung wieder ausschalten
      #endif
      endwin(); # Curses abschalten
      end_system_call();
    }

LISPFUNN(window_size,1)
  { check_window_stream(popSTACK());
    value1 = fixnum(LINES); # Curses-Variablen LINES, COLS abfragen
    value2 = fixnum(COLS);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  { check_window_stream(popSTACK());
   {var int y;
    var int x;
    begin_system_call();
    getyx(stdscr,y,x); # (y,x) := cursor position
    end_system_call();
    value1 = fixnum(y);
    value2 = fixnum(x);
    mv_count=2;
  }}

LISPFUNN(set_window_cursor_position,3)
  { check_window_stream(STACK_2);
   {var uintL line = posfixnum_to_L(STACK_1);
    var uintL column = posfixnum_to_L(STACK_0);
    if ((line < LINES) && (column < COLS))
      { begin_system_call();
        move(line,column); refresh(); # Cursor positionieren
        end_system_call();
      }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }}

LISPFUNN(clear_window,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    clear(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    clrtobot(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    clrtoeol(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    deleteln(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    insertln(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  { check_window_stream(popSTACK());
    #ifdef A_STANDOUT # geht nur, wenn Curses Attribute verwaltet
    begin_system_call();
    attron(A_STANDOUT); # Attribut A_STANDOUT bei addch() hineinoderieren
    end_system_call();
    #endif
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  { check_window_stream(popSTACK());
    #ifdef A_STANDOUT # geht nur, wenn Curses Attribute verwaltet
    begin_system_call();
    attroff(A_STANDOUT); # kein Attribut mehr bei addch() hineinoderieren
    end_system_call();
    #endif
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  { check_window_stream(popSTACK());
    # Cursor ist permanent an!
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  { check_window_stream(popSTACK());
    # geht nicht, da Cursor permanent an!
    value1 = NIL; mv_count=0;
  }

#endif # UNIX

#ifdef AMIGAOS

# Terminal-Emulation: ANSI-Steuerzeichen, siehe console.doc

# UP: Ausgabe mehrerer Zeichen auf den Bildschirm
  local void wr_window (const uintB* outbuffer, uintL count);
  local void wr_window(outbuffer,count)
    var const uintB* outbuffer;
    var uintL count;
    { set_break_sem_1();
      begin_system_call();
     {var long ergebnis = Write(stdout_handle,outbuffer,count);
      end_system_call();
      if (ergebnis<0) { OS_error(); } # Error melden
      if (ergebnis<count) # nicht erfolgreich?
        { ?? }
      clr_break_sem_1();
    }}

#define WR_WINDOW(characters)  \
  { local var uintB outbuffer[] = characters; \
     wr_window(&outbuffer,sizeof(outbuffer)); \
  }

# UP: Ein Zeichen auf einen Window-Stream ausgeben.
# wr_ch_window(&stream,ch);
# > stream: Window-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_window (const object* stream_, object ch);
  local void wr_ch_window(stream_,ch)
    var const object* stream_;
    var object ch;
    { if (!charp(ch)) { fehler_wr_char(*stream_,ch); } # ch sollte Character sein
     {var uintB c = char_code(ch); # Code des Zeichens
      ??
    }}

LISPFUNN(make_window,0)
  { finish_output_terminal(var_stream(S(terminal_io),strmflags_wr_ch_B)); # evtl. wartendes NL jetzt ausgeben
   {var object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+0,0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_dummy); # READ-CHAR unmöglich
      s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      s->strm_wr_ss = P(wr_ss_dummy_nogc);
    # size: aWSR? aWBR??
    # Wrap off ?? ASM? AWM?
    WR_WINDOW({CSI,'0',0x6D}); # Set Graphics Rendition Normal
    value1 = stream; mv_count=1;
  }}

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var object stream;
    { # Wrap on ?? ASM? AWM?
      WR_WINDOW({CSI,'0',0x6D}); # Set Graphics Rendition Normal
    }

LISPFUNN(window_size,1)
  { check_window_stream(popSTACK());
    value1 = fixnum(window_size.y); ??
    value2 = fixnum(window_size.x); ??
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  { check_window_stream(popSTACK());
    # aWSR? CPR??
    value1 = fixnum(_y); ??
    value2 = fixnum(_x); ??
    mv_count=2;
  }

LISPFUNN(set_window_cursor_position,3)
  { check_window_stream(STACK_2);
   {var uintL line = posfixnum_to_L(STACK_1);
    var uintL column = posfixnum_to_L(STACK_0);
    if ((line < (uintL)window_size.y) && (column < (uintL)window_size.x))
      { var uintB outbuffer[23]; # Buffer für  CSI <line> ; <column> H
        var uintB* ptr = &outbuffer[sizeof(outbuffer)];
        var uintL count = 0;
        count++; *--ptr = 'H';
        do { count++; *--ptr = '0'+(column%10); column = floor(column,10); }
           until (column==0);
        count++; *--ptr = ';';
        do { count++; *--ptr = '0'+(line%10); line = floor(line,10); }
           until (line==0);
        count++; *--ptr = CSI;
        wr_window(ptr,count);
      }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }}

LISPFUNN(clear_window,1)
  { check_window_stream(popSTACK());
    WR_WINDOW({CSI,'0',';','0','H',CSI,'J'});
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  { check_window_stream(popSTACK());
    WR_WINDOW({CSI,'J'});
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  { check_window_stream(popSTACK());
    WR_WINDOW({CSI,'K'});
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  { check_window_stream(popSTACK());
    WR_WINDOW({CSI,'M'});
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  { check_window_stream(popSTACK());
    WR_WINDOW({CSI,'L'});
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  { check_window_stream(popSTACK());
    WR_WINDOW({CSI,'1',0x6D}); # Set Graphics Rendition Bold
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  { check_window_stream(popSTACK());
    WR_WINDOW({CSI,'0',0x6D}); # Set Graphics Rendition Normal
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  { check_window_stream(popSTACK());
    # aSCR ??
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  { check_window_stream(popSTACK());
    # aSCR ??
    value1 = NIL; mv_count=0;
  }

#endif # AMIGAOS

#endif # SCREEN


# File-Stream
# ===========

# Um nicht für jedes Character das UNIX/AMIGADOS bemühen zu müssen,
# wird ein eigener Buffer geführt.
# (Dies bewirkte z.B. beim Einlesen eines 408 KByte- Files auf dem Atari
# eine Beschleunigung um einen Faktor 2.7 von 500 sec auf 180 sec.)

# Additional fields:
  # define strm_file_name       strm_other[3] # Filename, a pathname or NIL
  # define strm_file_truename   strm_other[4] # Truename, ein non-logical pathname or NIL
  # define strm_file_handle     strm_other[2] # a wrapped Handle
  #define strm_file_bufflen     4096          # buffer length, a power of 2, <2^16
  #define strm_file_buffer      strm_other[0] # our own buffer, a simple-string
                                              # with strm_file_bufflen bytes
  #define strm_file_extrafields strm_other[5] # here some binary fields begin
#define strm_file_length  (strm_len+5)

# Additional binary (not GCed) fields:
typedef struct strm_file_extrafields_struct {
  uintL buffstart;              # start position of buffer
  sintL eofindex;               # index up to which the data is valid
                                # (for recognizing EOF)
  #define eofindex_all_invalid  (-1)
  #define eofindex_all_valid    (-2)
  uintL index;                  # index into buffer (>=0, <=strm_file_bufflen)
  boolean modified;             # TRUE if the buffer contains modified data, else FALSE
  # Three cases:
  #  eofindex = eofindex_all_invalid: buffer contents completely invalid,
  #                                   index = 0.
  #  eofindex >= 0: 0 <= index <= eofindex <= strm_file_bufflen.
  #  eofindex = eofindex_all_valid: buffer contents completely valid,
  #                                 0 <= index <= strm_file_bufflen.
  # buffstart = (sector number) * strm_file_bufflen, if read access permitted.
  # The position of handle, known to the OS, set via lseek, is normally (but
  # not always!) the end of the current buffer:
  #   if eofindex = eofindex_all_valid: buffstart + strm_file_bufflen,
  #   if eofindex >= 0: buffstart + eofindex,
  #   if eofindex = eofindex_all_invalid: buffstart.
# Up to now a file is considered built from bytes à 8 bits.
# Logically, it is built up from other units:
  uintL position;               # position in logical units
} strm_file_extrafields_struct;

# For file streams with element type CHARACTER (ch_file)
# every character is 1 byte.
typedef struct strm_ch_file_extrafields_struct {
  strm_file_extrafields_struct _parent;
  uintL lineno;   # line number during read, >0
} strm_ch_file_extrafields_struct;

# For file streams with element type INTEGER ("byte files") every integer
# uses the same amount of bits.
typedef struct strm_i_file_extrafields_struct {
  strm_file_extrafields_struct _parent;
  uintL bitsize;                # number of bits, >0, <intDsize*uintWC_max
  #define strm_file_bitbuffer   strm_other[1] # buffer, a simple-bit-vector
                                              # with ceiling(bitsize/8)*8 bits
  # If bitsize is not a multiple of 8:
  uintL bitindex;               # index in the current byte, >=0, <=8
  # The buffer contains 8*index+bitindex bits. The bits are ordered in the
  # order bit0,....,bit7. If bitsize<8, the length of the file (measured in
  # bits) is stored in the first 4 bytes of the files when the file is closed.
  # The actual data then begins in the 5th byte.
  uintL eofposition;            # position of logical EOF
} strm_i_file_extrafields_struct;

# In closed file streams only the fields `name' and `truename' are relevant.

# Accessors.
#define FileStream_name(stream)  TheStream(stream)->strm_file_name
#define FileStream_truename(stream)  TheStream(stream)->strm_file_truename
#define FileStream_handle(stream)  TheStream(stream)->strm_file_handle
#define FileStream_buffer(stream)  TheStream(stream)->strm_file_buffer
#define FileStream_buffstart(stream)  \
  ((strm_file_extrafields_struct*)&TheStream(stream)->strm_file_extrafields)->buffstart
#define FileStream_eofindex(stream)  \
  ((strm_file_extrafields_struct*)&TheStream(stream)->strm_file_extrafields)->eofindex
#define FileStream_index(stream)  \
  ((strm_file_extrafields_struct*)&TheStream(stream)->strm_file_extrafields)->index
#define FileStream_modified(stream)  \
  ((strm_file_extrafields_struct*)&TheStream(stream)->strm_file_extrafields)->modified
#define FileStream_position(stream)  \
  ((strm_file_extrafields_struct*)&TheStream(stream)->strm_file_extrafields)->position
#define FileStream_lineno(stream)  \
  ((strm_ch_file_extrafields_struct*)&TheStream(stream)->strm_file_extrafields)->lineno
#define FileStream_bitsize(stream)  \
  ((strm_i_file_extrafields_struct*)&TheStream(stream)->strm_file_extrafields)->bitsize
#define FileStream_bitindex(stream)  \
  ((strm_i_file_extrafields_struct*)&TheStream(stream)->strm_file_extrafields)->bitindex
#define FileStream_eofposition(stream)  \
  ((strm_i_file_extrafields_struct*)&TheStream(stream)->strm_file_extrafields)->eofposition

# File-Stream allgemein
# =====================

#if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
# Annahme: Alle von OPEN(2) gelieferten File-Descriptoren (hier Handles
# genannt) passen in ein uintW.
# Begründung: Bekanntlich ist 0 <= fd < getdtablesize() .
#endif

# Handle positionieren:
# file_lseek(stream,offset,mode,ergebnis_zuweisung);
# > mode: Positionierungsmodus:
#         SEEK_SET  "absolut"
#         SEEK_CUR  "relativ"
#         SEEK_END  "ab Ende"
# < ergebnis: neue Position
  #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
    #define file_lseek(stream,offset,mode,ergebnis_zuweisung)  \
      { var sintL ergebnis =                  \
          lseek(TheHandle(TheStream(stream)->strm_file_handle), # Handle \
                offset,                       \
                mode                          \
               );                             \
        if (ergebnis<0) # Fehler aufgetreten? \
          { end_system_call(); OS_filestream_error(stream); } \
        unused (ergebnis_zuweisung ergebnis); \
      }
  #endif
  #ifdef AMIGAOS
    #define file_lseek(stream,offset,mode,ergebnis_zuweisung)  \
      { var uintL _offset = (offset);                          \
        var sintL ergebnis =                                   \
          Seek(TheHandle(TheStream(stream)->strm_file_handle), \
               _offset,                                        \
               mode                                            \
              );                                               \
        if (ergebnis<0) # Fehler aufgetreten?                  \
          { end_system_call(); OS_filestream_error(stream); }  \
        if (mode==SEEK_SET) { unused (ergebnis_zuweisung _offset); } \
        elif (mode==SEEK_CUR) { unused (ergebnis_zuweisung ergebnis+_offset); } \
        else /* mode==SEEK_END */                                      \
          { ergebnis = Seek(TheHandle(TheStream(stream)->strm_file_handle),0,SEEK_CUR); \
            if (ergebnis<0) # Fehler aufgetreten?                      \
              { end_system_call(); OS_filestream_error(stream); }      \
            unused (ergebnis_zuweisung ergebnis);                      \
      }   }
    #define SEEK_SET  OFFSET_BEGINNING
    #define SEEK_CUR  OFFSET_CURRENT
    #define SEEK_END  OFFSET_END
  #endif
  #ifdef WIN32_NATIVE
    #define file_lseek(stream,offset,mode,ergebnis_zuweisung)  \
      { var DWORD ergebnis =                                   \
          SetFilePointer(TheHandle(TheStream(stream)->strm_file_handle), # Handle \
                         offset, NULL,                         \
                         mode                                  \
                        );                                     \
        if (ergebnis == (DWORD)(-1))                           \
          { end_system_call(); OS_filestream_error(stream); }  \
        unused (ergebnis_zuweisung ergebnis);                  \
       }
  #endif

# UP: Beendet das Zurückschreiben des Buffers.
# b_file_finish_flush(stream,bufflen);
# > stream : (offener) Byte-basierter File-Stream.
# > bufflen : Anzahl der zu schreibenden Bytes
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void b_file_finish_flush (object stream, uintL bufflen);
  local void b_file_finish_flush(stream,bufflen)
    var object stream;
    var uintL bufflen;
    { begin_system_call();
     {var sintL ergebnis = # Buffer hinausschreiben
        full_write(TheHandle(TheStream(stream)->strm_file_handle), # Handle
                   &TheSstring(TheStream(stream)->strm_file_buffer)->data[0], # Bufferadresse
                   bufflen
                  );
      if (ergebnis==bufflen)
        # alles korrekt geschrieben
        { end_system_call(); FileStream_modified(stream) = FALSE; }
        else
        # Nicht alles geschrieben
        {
          #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
          if (ergebnis<0) # Fehler aufgetreten?
            #ifdef ENOSPC
            if (!(errno == ENOSPC))
            #endif
            #ifdef EDQUOT
            if (!(errno == EDQUOT))
            #endif
              { end_system_call(); OS_filestream_error(stream); }
          #endif
          #if defined(AMIGAOS) || defined(WIN32_NATIVE)
          if (ergebnis<0) # Fehler aufgetreten?
            { end_system_call(); OS_filestream_error(stream); }
          #endif
          end_system_call();
          # Nicht alles geschrieben, wohl wegen voller Diskette.
          # Um Inkonsistenzen zu vermeiden, muss man das File schließen.
          FileStream_modified(stream) = FALSE; # Hierbei gehen Daten verloren!
          pushSTACK(stream);
          stream_close(&STACK_0); # File schließen
          clr_break_sem_4(); # keine UNIX-Operation mehr aktiv
          # Fehler melden.
          pushSTACK(!nullp(TheStream(STACK_0)->strm_file_truename) ? TheStream(STACK_0)->strm_file_truename : STACK_0); # Wert für Slot PATHNAME von FILE-ERROR
          pushSTACK(STACK_(0+1)); # stream
          fehler(file_error,
                 DEUTSCH ? "Diskette/Platte voll. Deswegen wurde ~ geschlossen." :
                 ENGLISH ? "Closed ~ because disk is full." :
                 FRANCAIS ? "Ai fermé ~, parce que le disque est sans doute plein." :
                 ""
                );
    }}  }

# UP: Schreibt den vollen, modifizierten Buffer zurück.
# b_file_full_flush(stream);
# > stream : (offener) Byte-basierter File-Stream.
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void b_file_full_flush (object stream);
  local void b_file_full_flush(stream)
    var object stream;
    { # erst zurückpositionieren, dann schreiben.
      if (!((TheStream(stream)->strmflags & strmflags_rd_B) == 0))
        { begin_system_call();
          file_lseek(stream,-(long)strm_file_bufflen,SEEK_CUR,); # Zurückpositionieren
          end_system_call();
        }
      b_file_finish_flush(stream,strm_file_bufflen);
    }

# UP: Schreibt den halbvollen, modifizierten Buffer zurück.
# b_file_full_flush(stream);
# > stream : (offener) Byte-basierter File-Stream.
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void b_file_half_flush (object stream);
  local void b_file_half_flush(stream)
    var object stream;
    { if (!((TheStream(stream)->strmflags & strmflags_rd_B) == 0))
        { begin_system_call();
          file_lseek(stream,FileStream_buffstart(stream),SEEK_SET,); # Zurückpositionieren
          end_system_call();
        }
      # eofindex Bytes schreiben:
      b_file_finish_flush(stream,FileStream_eofindex(stream));
    }

# UP: Schreibt den modifizierten Buffer zurück.
# b_file_flush(stream);
# > stream : (offener) Byte-basierter File-Stream.
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void b_file_flush (object stream);
  local void b_file_flush(stream)
    var object stream;
    { if (FileStream_eofindex(stream) == eofindex_all_valid) # Buffer ganz gültig ?
        { b_file_full_flush(stream); }
        else
        { b_file_half_flush(stream); }
    }

# UP: Positioniert einen Byte-basierten File-Stream so, dass das nächste Byte
# gelesen oder überschrieben werden kann.
# b_file_nextbyte(stream)
# > stream : (offener) Byte-basierter File-Stream.
# < ergebnis : NULL falls EOF (und dann ist index=eofindex),
#              sonst: Pointer auf nächstes Byte
# verändert in stream: index, eofindex, buffstart
  local uintB* b_file_nextbyte (object stream);
  local uintB* b_file_nextbyte(stream)
    var object stream;
    { var sintL eofindex = FileStream_eofindex(stream);
      var uintL index = FileStream_index(stream);
      if (!(eofindex == eofindex_all_valid))
        # Bufferdaten nur halb gültig
        { if (eofindex == eofindex_all_invalid)
            # Bufferdaten ganz ungültig
            { goto reread; }
            else
            # EOF tritt in diesem Sector auf
            { goto eofsector; }
        }
      # Bufferdaten ganz gültig
      if (!(index == strm_file_bufflen)) # index = bufflen ?
        # nein, also 0 <= index < strm_file_bufflen -> OK
        { return &TheSstring(TheStream(stream)->strm_file_buffer)->data[index]; }
      # Buffer muss neu gefüllt werden.
      if (FileStream_modified(stream))
        # Zuvor muss der Buffer hinausgeschrieben werden:
        { b_file_full_flush(stream); }
      FileStream_buffstart(stream) += strm_file_bufflen;
      reread: # Ab hier den Buffer neu lesen:
      { var sintL ergebnis;
        if ((TheStream(stream)->strmflags & strmflags_rd_B) == 0)
          { ergebnis = 0; }
          else
          { begin_system_call();
            ergebnis = # Buffer füllen
              full_read(TheHandle(TheStream(stream)->strm_file_handle), # Handle
                        &TheSstring(TheStream(stream)->strm_file_buffer)->data[0], # Bufferadresse
                        strm_file_bufflen
                       );
            end_system_call();
            if (ergebnis==strm_file_bufflen)
              # der ganze Buffer wurde gefüllt
              { FileStream_index(stream) = 0; # Index := 0
                FileStream_modified(stream) = FALSE; # Buffer unmodifiziert
                FileStream_eofindex(stream) = eofindex_all_valid; # eofindex := all_valid
                return &TheSstring(TheStream(stream)->strm_file_buffer)->data[0];
              }
            if (ergebnis<0) { OS_filestream_error(stream); } # Fehler aufgetreten?
          }
        # Es wurden ergebnis (< strm_file_bufflen) Bytes gelesen.
        # Nicht der ganze Buffer wurde gefüllt -> EOF ist erreicht.
        FileStream_index(stream) = index = 0; # Index := 0
        FileStream_modified(stream) = FALSE; # Buffer unmodifiziert
        FileStream_eofindex(stream) = eofindex = ergebnis; # eofindex := ergebnis
      }
      eofsector: # eofindex ist ein Fixnum, d.h. EOF tritt in diesem Sector auf.
      if (index == eofindex)
        { return (uintB*)NULL; } # EOF erreicht
        else
        { return &TheSstring(TheStream(stream)->strm_file_buffer)->data[index]; }
    }

# UP: Bereitet das Schreiben eines Bytes am EOF vor.
# b_file_eofbyte(stream);
# > stream : (offener) Byte-basierter File-Stream,
#            bei dem gerade b_file_nextbyte(stream)==NULL ist.
# < ergebnis : Pointer auf nächstes (freies) Byte
# verändert in stream: index, eofindex, buffstart
  local uintB* b_file_eofbyte (object stream);
  local uintB* b_file_eofbyte(stream)
    var object stream;
    { # EOF. Es ist eofindex=index.
      if (FileStream_eofindex(stream) == strm_file_bufflen)
        # eofindex = strm_file_bufflen
        { # Buffer muss neu gefüllt werden. Da nach ihm sowieso EOF kommt,
          # genügt es, ihn hinauszuschreiben:
          if (FileStream_modified(stream)) { b_file_half_flush(stream); }
          FileStream_buffstart(stream) += strm_file_bufflen;
          FileStream_eofindex(stream) = 0; # eofindex := 0
          FileStream_index(stream) = 0; # index := 0
          FileStream_modified(stream) = FALSE; # unmodifiziert
        }
      # eofindex erhöhen:
      FileStream_eofindex(stream) += 1;
      return &TheSstring(TheStream(stream)->strm_file_buffer)->data[FileStream_index(stream)];
    }

# UP: Schreibt ein Byte auf einen Byte-basierten File-Stream.
# b_file_writebyte(stream,b);
# > stream : (offener) Byteblock-basierter File-Stream.
# > b : zu schreibendes Byte
# verändert in stream: index, eofindex, buffstart
  local void b_file_writebyte (object stream, uintB b);
  local void b_file_writebyte(stream,b)
    var object stream;
    var uintB b;
    { var uintB* ptr = b_file_nextbyte(stream);
      if (!(ptr == (uintB*)NULL))
        { if (*ptr == b) goto no_modification; } # keine wirkliche Modifikation?
        else
        { ptr = b_file_eofbyte(stream); } # EOF -> 1 Byte Platz machen
      # nächstes Byte in den Buffer schreiben:
      *ptr = b; FileStream_modified(stream) = TRUE;
      no_modification:
      # index incrementieren:
      FileStream_index(stream) += 1;
    }

# File-Stream, Byte-basiert (b_file)
# ===========  ============

# Fehler wegen Positionierung hinter EOF.
# fehler_position_beyond_EOF(stream);
  nonreturning_function(local, fehler_position_beyond_EOF, (object stream));
  local void fehler_position_beyond_EOF(stream)
    var object stream;
    { pushSTACK(!nullp(TheStream(stream)->strm_file_truename) ? TheStream(stream)->strm_file_truename : stream); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(stream);
      fehler(file_error,
             DEUTSCH ? "Positionierung von ~ hinter EOF unmöglich." :
             ENGLISH ? "cannot position ~ beyond EOF" :
             FRANCAIS ? "Ne peux pas positionner ~ au-delà de la fin du fichier." :
             ""
            );
    }

# UP: Positioniert einen (offenen) Byte-basierten File-Stream an eine
# gegebene Position.
# position_b_file(stream,position);
# > stream : (offener) Byte-basierter File-Stream.
# > position : neue Position
# verändert in stream: index, eofindex, buffstart
  local void position_b_file (object stream, uintL position);
  local void position_b_file(stream,position)
    var object stream;
    var uintL position;
    { # Liegt die neue Position im selben Sector?
      { var sintL eofindex = FileStream_eofindex(stream);
        var uintL newindex = position - FileStream_buffstart(stream);
        if (newindex
            <= ((eofindex == eofindex_all_valid) ? strm_file_bufflen :
                (!(eofindex == eofindex_all_invalid)) ? eofindex :
                0
           )   )
          { # ja -> brauche nur index zu verändern:
            FileStream_index(stream) = newindex;
            return;
      }   }
      # evtl. Buffer hinausschreiben:
      if (FileStream_modified(stream)) { b_file_flush(stream); }
      # Nun ist modified_flag gelöscht.
      if ((TheStream(stream)->strmflags & strmflags_rd_B) == 0)
        { # Positionieren:
          begin_system_call();
          file_lseek(stream,position,SEEK_SET,);
          end_system_call();
          FileStream_buffstart(stream) = position;
          FileStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
          FileStream_index(stream) = 0; # index := 0
          FileStream_modified(stream) = FALSE; # unmodifiziert
        }
        else
        { var uintL oldposition = FileStream_buffstart(stream) + FileStream_index(stream);
          # Positionieren:
          { var uintL newposition;
            begin_system_call();
            file_lseek(stream,floor(position,strm_file_bufflen)*strm_file_bufflen,SEEK_SET,newposition=);
            end_system_call();
            FileStream_buffstart(stream) = newposition;
          }
          # Sector lesen:
          FileStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
          FileStream_index(stream) = 0; # index := 0
          FileStream_modified(stream) = FALSE; # unmodifiziert
          { var uintL newindex = position % strm_file_bufflen; # gewünschter Index im Sector
            if (!(newindex==0)) # Position zwischen Sectoren -> brauche nichts zu lesen
              { b_file_nextbyte(stream);
                # Jetzt ist index=0.
                # index auf (position mod bufflen) setzen, vorher überprüfen:
               {var sintL eofindex = FileStream_eofindex(stream);
                # Es muss entweder eofindex=all_valid oder 0<=newindex<=eofindex sein:
                if (!((eofindex == eofindex_all_valid) || (newindex <= eofindex)))
                  # Fehler. Aber erst an die alte Position zurückpositionieren:
                  { check_SP();
                    position_b_file(stream,oldposition); # zurückpositionieren
                    fehler_position_beyond_EOF(stream);
                  }
                FileStream_index(stream) = newindex;
        } }   }}
    }

# File-Stream für Characters
# ==========================

# Funktionsweise:
# Beim Schreiben: Characters werden unverändert durchgereicht, nur NL wird auf
# MSDOS und WIN32 und bei O_BINARY != 0 in CR/LF umgewandelt.
# Beim Lesen: CR/LF wird in NL umgewandelt.

# READ-CHAR - Pseudofunktion für File-Streams für Characters
  local object rd_ch_ch_file (const object* stream_);
  local object rd_ch_ch_file(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      var uintB* charptr = b_file_nextbyte(stream);
      if (charptr == (uintB*)NULL) { return eof_value; } # EOF ?
      # nächstes Zeichen holen:
     {var object ch = code_char(*charptr); # Character aus dem Buffer holen
      # index und position incrementieren:
      FileStream_index(stream) += 1;
      FileStream_position(stream) += 1;
      # ch = nächstes Zeichen
      if (!eq(ch,code_char(CR))) # Ist es CR ?
        { # nein -> OK
          if (eq(ch,code_char(NL))) # Ist es NL, dann lineno incrementieren
            { FileStream_lineno(stream) += 1; }
          return ch;
        }
      # ja -> nächstes Zeichen auf LF untersuchen
      charptr = b_file_nextbyte(stream);
      if (charptr == (uintB*)NULL) { return ch; } # EOF -> bleibt CR
      if (!(*charptr == LF)) { return ch; } # kein LF -> bleibt CR
      # LF übergehen, index und position incrementieren:
      FileStream_index(stream) += 1;
      FileStream_position(stream) += 1;
      # lineno incrementieren:
      FileStream_lineno(stream) += 1;
      # NL als Ergebnis:
      return code_char(NL);
    }}

# Stellt fest, ob ein File-Stream ein Zeichen verfügbar hat.
# listen_ch_file(stream)
# > stream: File-Stream für Characters
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_ch_file (object stream);
  local signean listen_ch_file(stream)
    var object stream;
    { if (b_file_nextbyte(stream) == (uintB*)NULL)
        { return signean_minus; } # EOF
        else
        { return signean_null; }
    }

# UP: Schreibt ein Byte auf einen Byte-basierten File-Stream.
# write_b_file(stream,b);
# > stream : (offener) Byte-basierter File-Stream.
# > b : zu schreibendes Byte
# verändert in stream: index, eofindex, buffstart, position
  local void write_b_file (object stream, uintB b);
  local void write_b_file(stream,b)
    var object stream;
    var uintB b;
    { b_file_writebyte(stream,b);
      # position incrementieren:
      FileStream_position(stream) += 1;
    }

# WRITE-CHAR - Pseudofunktion für File-Streams für Characters
  local void wr_ch_ch_file (const object* stream_, object obj);
  local void wr_ch_ch_file(stream_,obj)
    var const object* stream_;
    var object obj;
    { var object stream = *stream_;
      # obj muss ein Character sein:
      if (!charp(obj)) { fehler_wr_char(stream,obj); }
     {var uintB ch = char_code(obj);
      #if defined(MSDOS) || defined(WIN32) || (defined(UNIX) && (O_BINARY != 0))
      if (ch==NL)
        # Newline als CR/LF ausgeben
        { write_b_file(stream,CR); write_b_file(stream,LF); }
        else
        # alle anderen Zeichen unverändert ausgeben
        { write_b_file(stream,ch); }
      #else
      write_b_file(stream,ch); # unverändert ausgeben
      #endif
    }}

# WRITE-CHAR-SEQUENCE für File-Streams für Characters:
  local const uintB* write_char_array_ch_file (object stream, const uintB* strptr, uintL len);
  #if defined(MSDOS) || defined(WIN32) || (defined(UNIX) && (O_BINARY != 0))
  # Wegen NL->CR/LF-Umwandlung keine Optimierung möglich.
  local inline const uintB* write_char_array_ch_file(stream,strptr,len)
    var object stream;
    var const uintB* strptr;
    var uintL len;
    { var uintL remaining = len;
      do { var uintB ch = *strptr++;
           if (ch==NL)
             # Newline als CR/LF ausgeben
             { write_b_file(stream,CR); write_b_file(stream,LF); }
             else
             # alle anderen Zeichen unverändert ausgeben
             { write_b_file(stream,ch); }
           remaining--;
         }
         until (remaining == 0);
      wr_ss_lpos(stream,strptr,len); # Line-Position aktualisieren
      return strptr;
    }
  #else
  local const uintB* write_char_array_ch_file(stream,strptr,len)
    var object stream;
    var const uintB* strptr;
    var uintL len;
    { var uintL remaining = len;
      var uintB* ptr;
      do # Noch remaining>0 Bytes abzulegen.
        { ptr = b_file_nextbyte(stream);
          if (ptr == (uintB*)NULL) goto eof_reached;
         {var sintL eofindex = FileStream_eofindex(stream);
          var uintL next = # so viel wie noch in den Buffer oder bis EOF passt
            ((eofindex==eofindex_all_valid) ? strm_file_bufflen : eofindex)
            - FileStream_index(stream); # > 0 !
          if (next > remaining) { next = remaining; }
          # next Bytes in den Buffer kopieren:
          {var uintL count;
           dotimespL(count,next,
             { var uintB b = *strptr++; # nächstes Byte
               if (!(*ptr == b)) { *ptr = b; FileStream_modified(stream) = TRUE;; } # in den Buffer
               ptr++;
             });
          }
          remaining = remaining - next;
          # index incrementieren:
          FileStream_index(stream) += next;
        }}
        until (remaining == 0);
      if (FALSE)
        eof_reached: # Schreiben am EOF, eofindex = index
        do # Noch remaining>0 Bytes abzulegen.
          { var uintL next = # so viel wie noch Platz im Buffer ist
              strm_file_bufflen - FileStream_index(stream);
            if (next==0)
              { # Buffer muss neu gefüllt werden. Da nach ihm sowieso EOF kommt,
                # genügt es, ihn hinauszuschreiben:
                if (FileStream_modified(stream)) { b_file_half_flush(stream); }
                FileStream_buffstart(stream) += strm_file_bufflen;
                FileStream_eofindex(stream) = 0; # eofindex := 0
                FileStream_index(stream) = 0; # index := 0
                FileStream_modified(stream) = FALSE; # unmodifiziert
                # Dann nochmals versuchen:
                next = strm_file_bufflen;
              }
            if (next > remaining) { next = remaining; }
            # next Bytes in den Buffer kopieren:
            {var uintL count;
             ptr = &TheSstring(TheStream(stream)->strm_file_buffer)->data[FileStream_index(stream)];
             dotimespL(count,next, { *ptr++ = *strptr++; } );
             FileStream_modified(stream) = TRUE;
            }
            remaining = remaining - next;
            # index und eofindex incrementieren:
            FileStream_index(stream) += next;
            FileStream_eofindex(stream) += next;
          }
          until (remaining == 0);
      # position incrementieren:
      FileStream_position(stream) += len;
      wr_ss_lpos(stream,strptr,len); # Line-Position aktualisieren
      return strptr;
    }
  #endif

# WRITE-SIMPLE-STRING - Pseudofunktion für File-Streams für Characters
  local void wr_ss_ch_file (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_ch_file(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { if (len==0) return;
      write_char_array_ch_file(*stream_,&TheSstring(string)->data[start],len);
    }

# File-Stream, Bit-basiert
# ========================

# Davon gibt es insgesamt 6 Arten:
# Drei Fälle
#   a - bitsize durch 8 teilbar,
#   b - bitsize < 8,
#   c - bitsize nicht durch 8 teilbar und >= 8,
# jeweils unterschieden durch
#   s - Elementtyp (signed-byte bitsize),
#       dazu zählt auch signed-byte = (signed-byte 8)
#   u - Elementtyp (unsigned-byte bitsize),
#       dazu zählen auch unsigned-byte = (unsigned-byte 8)
#       und bit = (unsigned-byte 1)
#       und (mod n) = (unsigned-byte (integer-length n))

# UP: Positioniert einen (offenen) Bit-basierten File-Stream an eine
# gegebene Position.
# position_i_file(stream,position);
# > stream : (offener) Byte-basierter File-Stream.
# > position : neue (logische) Position
# verändert in stream: index, eofindex, buffstart, bitindex
  local void position_i_file (object stream, uintL position);
  local void position_i_file(stream,position)
    var object stream;
    var uintL position;
    { var uintB flags = TheStream(stream)->strmflags;
      var uintL bitsize = FileStream_bitsize(stream);
      var uintL position_bits = position * bitsize;
      if ((flags & strmflags_i_B) == strmflags_ib_B)
        { position_bits += sizeof(uintL)*8; } # Header berücksichtigen
      # An Bit Nummer position_bits positionieren.
      position_b_file(stream,floor(position_bits,8)); # Aufs Byte positionieren
      if ((flags & strmflags_i_B) == strmflags_ia_B) return; # Bei Art a war's das.
      if (# Liegt die angesprochene Position im ersten Byte nach EOF ?
          ((!((position_bits%8)==0))
           && (b_file_nextbyte(stream) == (uintB*)NULL)
          )
          ||
          # Liegt die angesprochene Position im letzten Byte, aber zu weit?
          (((flags & strmflags_i_B) == strmflags_ib_B)
           && (position > FileStream_eofposition(stream))
         ))
        # Fehler. Aber erst an die alte Position zurückpositionieren:
        { var uintL oldposition = FileStream_position(stream);
          check_SP();
          position_i_file(stream,oldposition); # zurückpositionieren
          fehler_position_beyond_EOF(stream);
        }
      FileStream_bitindex(stream) = position_bits%8;
    }

# UP für READ-BYTE auf File-Streams für Integers, Art u :
# Liefert die im Bitbuffer enthaltenen bytesize Bytes als Integer >=0.
# kann GC auslösen
  local object rd_by_iu_I (object stream, uintL bitsize, uintL bytesize);
  local object rd_by_iu_I(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    { var object bitbuffer = TheStream(stream)->strm_file_bitbuffer;
      # Zahl im bitbuffer normalisieren:
      var uintB* bitbufferptr = &TheSbvector(bitbuffer)->data[0];
      *bitbufferptr &= (bit(((bitsize-1)%8)+1)-1); # High byte maskieren
     {var uintL count = bytesize;
      while ((!(count==0)) && (*bitbufferptr==0)) { count--; bitbufferptr++; }
      # Zahl bilden:
      if # höchstens oint_data_len Bits ?
         ((count <= floor(oint_data_len,8))
          || ((count == floor(oint_data_len,8)+1)
              && (*bitbufferptr < bit(oint_data_len%8))
         )   )
        # ja -> Fixnum >=0 bilden:
        { var uintL wert = 0;
          until (count==0) { wert = (wert<<8) | *bitbufferptr++; count--; }
          return fixnum(wert);
        }
        else
        # nein -> Bignum >0 bilden:
        { pushSTACK(bitbuffer);
         {var uintL digitcount = floor(count,(intDsize/8));
          if (((count%(intDsize/8)) > 0) || (*bitbufferptr & bit(7)))
            { digitcount++; }
          # Da bitsize < intDsize*uintWC_max, ist
          # digitcount <= ceiling((bitsize+1)/intDsize) <= uintWC_max .
          { var object big = allocate_bignum(digitcount,0); # neues Bignum >0
            TheBignum(big)->data[0] = 0; # höchstes Digit auf 0 setzen
            # restliche Digits von rechts füllen, dabei Folge von Bytes in
            # Folge von uintD übersetzen:
            bitbuffer = popSTACK();
            bitbufferptr = &TheSbvector(bitbuffer)->data[bytesize];
            #if BIG_ENDIAN_P
            {var uintB* bigptr = (uintB*)(&TheBignum(big)->data[digitcount]);
             dotimespL(count,count, { *--bigptr = *--bitbufferptr; } );
            }
            #else
            {var uintD* bigptr = &TheBignum(big)->data[digitcount];
             var uintL count2;
             #define GET_NEXT_BYTE(i)  \
               digit |= ((uintD)(*--bitbufferptr) << (8*i));
             dotimespL(count2,floor(count,intDsize/8),
               { var uintD digit = 0;
                 DOCONSTTIMES(intDsize/8,GET_NEXT_BYTE); # GET_NEXT_BYTE(0..intDsize/8-1)
                 *--bigptr = digit;
               });
             #undef GET_NEXT_BYTE
             count2 = count % (intDsize/8);
             if (count2>0)
               { var uintL shiftcount = 0;
                 var uintD digit = (uintD)(*--bitbufferptr);
                 dotimesL(count2,count2-1,
                   { shiftcount += 8;
                     digit |= ((uintD)(*--bitbufferptr) << shiftcount);
                   });
                 *--bigptr = digit;
               }
            }
            #endif
            # Wegen (intDsize/8)*(digitcount-1) <= count <= (intDsize/8)*digitcount
            # ist alles gefüllt.
            return big;
        }}}
    }}

# UP für READ-BYTE auf File-Streams für Integers, Art s :
# Liefert die im Bitbuffer enthaltenen bytesize Bytes als Integer.
# kann GC auslösen
  local object rd_by_is_I (object stream, uintL bitsize, uintL bytesize);
  local object rd_by_is_I(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    { var object bitbuffer = TheStream(stream)->strm_file_bitbuffer;
      # Zahl im bitbuffer normalisieren:
      var uintB* bitbufferptr = &TheSbvector(bitbuffer)->data[0];
      var sintD sign;
      var uintL signbitnr = (bitsize-1)%8;
      var uintL count = bytesize;
      if (!(*bitbufferptr & bit(signbitnr)))
        { sign = 0;
          *bitbufferptr &= (bitm(signbitnr+1)-1); # High byte sign-extenden
          # normalisieren, höchstes Bit muss 0 bleiben:
          while ((count>=2) && (*bitbufferptr==0) && !(*(bitbufferptr+1) & bit(7)))
            { count--; bitbufferptr++; }
          # Zahl bilden:
          if # höchstens oint_data_len+1 Bits, Zahl <2^oint_data_len ?
             ((count <= floor(oint_data_len,8))
              || ((count == floor(oint_data_len,8)+1)
                  && (*bitbufferptr < bit(oint_data_len%8))
             )   )
            # ja -> Fixnum >=0 bilden:
            { var uintL wert = 0;
              until (count==0) { wert = (wert<<8) | *bitbufferptr++; count--; }
              return posfixnum(wert);
            }
        }
        else
        { sign = -1;
          *bitbufferptr |= minus_bitm(signbitnr+1); # High byte sign-extenden
          # normalisieren, höchstes Bit muss 1 bleiben:
          while ((count>=2) && (*bitbufferptr==(uintB)(-1)) && (*(bitbufferptr+1) & bit(7)))
            { count--; bitbufferptr++; }
          # Zahl bilden:
          if # höchstens oint_data_len+1 Bits, Zahl >=-2^oint_data_len ?
             ((count <= floor(oint_data_len,8))
              || ((count == floor(oint_data_len,8)+1)
                  && (*bitbufferptr >= (uintB)(-bit(oint_data_len%8)))
             )   )
            # ja -> Fixnum <0 bilden:
            { var uintL wert = (uintL)(-1);
              until (count==0) { wert = (wert<<8) | *bitbufferptr++; count--; }
              return negfixnum(wbitm(intLsize)+(oint)wert);
            }
        }
      # Bignum bilden:
      pushSTACK(bitbuffer);
      { var uintL digitcount = ceiling(count,(intDsize/8));
        # Da bitsize < intDsize*uintWC_max, ist
        # digitcount <= ceiling(bitsize/intDsize) <= uintWC_max .
        var object big = allocate_bignum(digitcount,sign); # neues Bignum
        TheBignum(big)->data[0] = sign; # höchstes Word auf sign setzen
        # restliche Digits von rechts füllen, dabei Folge von Bytes in
        # Folge von uintD übersetzen:
        bitbuffer = popSTACK();
        bitbufferptr = &TheSbvector(bitbuffer)->data[bytesize];
        #if BIG_ENDIAN_P
        {var uintB* bigptr = (uintB*)(&TheBignum(big)->data[digitcount]);
         dotimespL(count,count, { *--bigptr = *--bitbufferptr; } );
        }
        #else
        {var uintD* bigptr = &TheBignum(big)->data[digitcount];
         var uintL count2;
         #define GET_NEXT_BYTE(i)  \
           digit |= ((uintD)(*--bitbufferptr) << (8*i));
         dotimespL(count2,floor(count,intDsize/8),
           { var uintD digit = 0;
             DOCONSTTIMES(intDsize/8,GET_NEXT_BYTE); # GET_NEXT_BYTE(0..intDsize/8-1)
             *--bigptr = digit;
           });
         #undef GET_NEXT_BYTE
         count2 = count % (intDsize/8);
         if (count2>0)
           { var uintL shiftcount = 0;
             var uintD digit = (uintD)(*--bitbufferptr);
             dotimesL(count2,count2-1,
               { shiftcount += 8;
                 digit |= ((uintD)(*--bitbufferptr) << shiftcount);
               });
             *--bigptr = digit;
           }
        }
        #endif
        # Wegen (intDsize/8)*(digitcount-1) < count <= (intDsize/8)*digitcount
        # ist alles gefüllt.
        return big;
      }
    }

# Typ rd_by_ix_I: eines dieser beiden Unterprogramme:
  typedef object rd_by_ix_I (object stream, uintL bitsize, uintL bytesize);

# UP für READ-BYTE auf File-Streams für Integers, Art a :
# Füllt den Bitbuffer mit den nächsten bitsize Bits.
# > stream : File-Stream für Integers, Art a
# > finisher : Beendigungsroutine
# < ergebnis : gelesener Integer oder eof_value
  local object rd_by_iax_file (object stream, rd_by_ix_I* finisher);
  local object rd_by_iax_file(stream,finisher)
    var object stream;
    var rd_by_ix_I* finisher;
    { var uintL bitsize = FileStream_bitsize(stream);
      var uintL bytesize = bitsize/8;
      # genügend viele Bytes in den Bitbuffer übertragen:
     {var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
      var uintL count;
      dotimespL(count,bytesize,
        { var uintB* ptr = b_file_nextbyte(stream);
          if (ptr == (uintB*)NULL) goto eof;
          # nächstes Byte holen:
          *--bitbufferptr = *ptr;
          # index incrementieren:
          FileStream_index(stream) += 1;
        });
      # position incrementieren:
      FileStream_position(stream) += 1;
      # in Zahl umwandeln:
      return (*finisher)(stream,bitsize,bytesize);
      eof: # EOF erreicht
      position_b_file(stream,FileStream_position(stream)*bytesize);
      return eof_value;
    }}

# UP für READ-BYTE auf File-Streams für Integers, Art b :
# Füllt den Bitbuffer mit den nächsten bitsize Bits.
# > stream : File-Stream für Integers, Art b
# > finisher : Beendigungsroutine
# < ergebnis : gelesener Integer oder eof_value
  local object rd_by_ibx_file (object stream, rd_by_ix_I* finisher);
  local object rd_by_ibx_file(stream,finisher)
    var object stream;
    var rd_by_ix_I* finisher;
    { # Nur bei position < eofposition gibt's was zu lesen:
      if (FileStream_position(stream) == FileStream_eofposition(stream))
        goto eof;
      { var uintL bitsize = FileStream_bitsize(stream); # bitsize (>0, <8)
        # genügend viele Bits in den Bitbuffer übertragen:
        var uintL bitindex = FileStream_bitindex(stream);
        var uintL count = bitindex + bitsize;
        var uint8 bit_akku;
        var uintB* ptr = b_file_nextbyte(stream);
        if (ptr == (uintB*)NULL) goto eof;
        # angefangenes Byte holen:
        bit_akku = (*ptr)>>bitindex;
        # bitshift := 8-bitindex
        # Von bit_akku sind die Bits (bitshift-1)..0 gültig.
        if (count > 8)
          { # index incrementieren, da gerade *ptr verarbeitet:
            FileStream_index(stream) += 1;
            count -= 8; # Noch count (>0) Bits zu holen.
           {var uintB* ptr = b_file_nextbyte(stream);
            if (ptr == (uintB*)NULL) goto eof1;
            # nächstes Byte holen:
            # (8-bitindex < 8, da sonst count = 0+bitsize < 8 gewesen wäre!)
            bit_akku |= (*ptr)<<(8-bitindex);
          }}# Von bit_akku sind alle 8 Bits gültig.
        # 8 Bit abspeichern:
        TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[0] = bit_akku;
        FileStream_bitindex(stream) = count;
        # position incrementieren:
        FileStream_position(stream) += 1;
        # in Zahl umwandeln:
        return (*finisher)(stream,bitsize,1);
        eof1:
          # Wieder zurückpositionieren:
          position_i_file(stream,FileStream_position(stream));
      }
      eof: # EOF erreicht gewesen
        return eof_value;
    }

# UP für READ-BYTE auf File-Streams für Integers, Art c :
# Füllt den Bitbuffer mit den nächsten bitsize Bits.
# > stream : File-Stream für Integers, Art c
# > finisher : Beendigungsroutine
# < ergebnis : gelesener Integer oder eof_value
  local object rd_by_icx_file (object stream, rd_by_ix_I* finisher);
  local object rd_by_icx_file(stream,finisher)
    var object stream;
    var rd_by_ix_I* finisher;
    { var uintL bitsize = FileStream_bitsize(stream);
      var uintL bytesize = ceiling(bitsize,8);
      # genügend viele Bits in den Bitbuffer übertragen:
      var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
      var uintL count = bitsize;
      var uintL bitshift = FileStream_bitindex(stream);
      var uintB* ptr = b_file_nextbyte(stream);
      if (ptr == (uintB*)NULL) goto eof;
      if (bitshift==0)
        { loop
            { *--bitbufferptr = *ptr; # 8 Bits holen und abspeichern
              # index incrementieren, da gerade *ptr verarbeitet:
              FileStream_index(stream) += 1;
              count -= 8;
              # Noch count (>0) Bits zu holen.
              ptr = b_file_nextbyte(stream);
              if (ptr == (uintB*)NULL) goto eof;
              if (count<=8) break; # Sind damit count Bits fertig?
            }
          # Noch count = bitsize mod 8 (>0,<8) Bits zu holen.
          *--bitbufferptr = *ptr; # count Bits holen und abspeichern
        }
        else # 0<bitindex<8
        { var uint16 bit_akku;
          # angefangenes Byte holen:
          bit_akku = (*ptr)>>bitshift;
          bitshift = 8-bitshift; # bitshift := 8-bitindex
          count -= bitshift;
          loop
            { # index incrementieren, da gerade *ptr verarbeitet:
              FileStream_index(stream) += 1;
              # Von bit_akku sind die Bits (bitshift-1)..0 gültig.
              # Noch count (>0) Bits zu holen.
             {var uintB* ptr = b_file_nextbyte(stream);
              if (ptr == (uintB*)NULL) goto eof;
              # nächstes Byte holen:
              bit_akku |= (uint16)(*ptr)<<bitshift;
             }# Von bit_akku sind die Bits (7+bitshift)..0 gültig.
              *--bitbufferptr = (uint8)bit_akku; # 8 Bit abspeichern
              if (count<=8) break; # Sind damit count Bits fertig?
              count -= 8;
              bit_akku = bit_akku>>8;
            }
        }
      FileStream_bitindex(stream) = count;
      # position incrementieren:
      FileStream_position(stream) += 1;
      # in Zahl umwandeln:
      return (*finisher)(stream,bitsize,bytesize);
      eof: # EOF erreicht
      position_i_file(stream,FileStream_position(stream));
      return eof_value;
    }

# UP für WRITE-BYTE auf File-Streams für Integers, Art a :
# Schreibt den Bitbuffer-Inhalt aufs File.
  local void wr_by_ia (object stream, uintL bitsize, uintL bytesize);
  local void wr_by_ia(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    { var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
      var uintL count;
      dotimespL(count,bytesize, { b_file_writebyte(stream,*--bitbufferptr); } );
      # position incrementieren:
      FileStream_position(stream) += 1;
    }

# UP für WRITE-BYTE auf File-Streams für Integers, Art b :
# Schreibt den Bitbuffer-Inhalt aufs File.
  local void wr_by_ib (object stream, uintL bitsize, uintL bytesize);
  local void wr_by_ib(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    { var uintL bitshift = FileStream_bitindex(stream);
      var uint16 bit_akku = (uint16)(TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[0])<<bitshift;
      var uintL count = bitsize;
      var uintB* ptr = b_file_nextbyte(stream);
      # angefangenes Byte holen:
      if (!(ptr == (uintB*)NULL)) { bit_akku |= (*ptr)&(bit(bitshift)-1); }
      count += bitshift;
      # evtl. einzelnes Byte schreiben:
      if (count>=8)
        { b_file_writebyte(stream,(uint8)bit_akku);
          bit_akku = bit_akku>>8;
          count -= 8;
        }
      # letztes Byte (count Bits) schreiben:
      if (!(count==0))
        { ptr = b_file_nextbyte(stream);
          if (ptr == (uintB*)NULL) # EOF ?
            { ptr = b_file_eofbyte(stream); # 1 Byte Platz machen
              *ptr = (uint8)bit_akku; # Byte schreiben
            }
            else
            # nächstes Byte nur teilweise überschreiben:
            { var uint8 diff = (*ptr ^ (uint8)bit_akku) & (uint8)(bit(count)-1);
              if (diff == 0) goto no_modification;
              *ptr ^= diff;
            }
          FileStream_modified(stream) = TRUE;
          no_modification: ;
        }
      FileStream_bitindex(stream) = count;
      # position und evtl. eofposition incrementieren:
      if (FileStream_eofposition(stream) == FileStream_position(stream))
        { FileStream_eofposition(stream) += 1; }
      FileStream_position(stream) += 1;
    }

# UP für WRITE-BYTE auf File-Streams für Integers, Art c :
# Schreibt den Bitbuffer-Inhalt aufs File.
  local void wr_by_ic (object stream, uintL bitsize, uintL bytesize);
  local void wr_by_ic(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    { var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
      var uintL bitshift = FileStream_bitindex(stream);
      var uintL count = bitsize;
      var uint16 bit_akku;
      var uintB* ptr = b_file_nextbyte(stream);
      # angefangenes Byte holen:
      bit_akku = (ptr==(uintB*)NULL ? 0 : (*ptr)&(bit(bitshift)-1) );
      count += bitshift;
      # einzelne Bytes schreiben:
      loop
        { bit_akku |= (uint16)(*--bitbufferptr)<<bitshift;
          if (count<8) break;
          b_file_writebyte(stream,(uint8)bit_akku);
          bit_akku = bit_akku>>8;
          count -= 8;
          if (count<=bitshift) break;
        }
      # letztes Byte (count Bits) schreiben:
      if (!(count==0))
        { ptr = b_file_nextbyte(stream);
          if (ptr == (uintB*)NULL) # EOF ?
            { ptr = b_file_eofbyte(stream); # 1 Byte Platz machen
              *ptr = (uint8)bit_akku; # Byte schreiben
            }
            else
            # nächstes Byte nur teilweise überschreiben:
            { var uint8 diff = (*ptr ^ (uint8)bit_akku) & (uint8)(bit(count)-1);
              if (diff == 0) goto no_modification;
              *ptr ^= diff;
            }
          FileStream_modified(stream) = TRUE;
          no_modification: ;
        }
      FileStream_bitindex(stream) = count;
      # position incrementieren:
      FileStream_position(stream) += 1;
    }

# Typ wr_by_ix: eines dieser drei Unterprogramme:
  typedef void wr_by_ix (object stream, uintL bitsize, uintL bytesize);

# UP für WRITE-BYTE auf File-Streams für Integers, Art u :
# Legt das Objekt (ein Integer >=0) als bytesize Bytes im Bitbuffer ab.
# > stream : File-Stream für Integers, Art u
# > obj : auszugebendes Objekt
# > finisher : Beendigungsroutine
  local void wr_by_ixu_file (object stream, object obj, wr_by_ix* finisher);
  local void wr_by_ixu_file(stream,obj,finisher)
    var object stream;
    var object obj;
    var wr_by_ix* finisher;
    { # obj überprüfen:
      if (!integerp(obj)) { fehler_wr_integer(stream,obj); }
      if (!positivep(obj)) { fehler_bad_integer(stream,obj); }
      # obj ist jetzt ein Integer >=0
     {var uintL bitsize = FileStream_bitsize(stream);
      var uintL bytesize = ceiling(bitsize,8);
      # obj in den Bitbuffer übertragen:
      { var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
        var uintL count = bytesize;
        if (posfixnump(obj))
          # obj ist ein Fixnum >=0
          { var uintL wert = posfixnum_to_L(obj);
            # wert < 2^bitsize überprüfen:
            if (!((bitsize>=oint_data_len) || (wert < bit(bitsize))))
              { fehler_bad_integer(stream,obj); }
            # wert im Bitbuffer ablegen:
            until (wert==0)
              { *--bitbufferptr = (uint8)wert; wert = wert>>8; count--; }
          }
          else
          # obj ist ein Bignum >0
          { var uintL len = (uintL)Bignum_length(obj);
            # obj < 2^bitsize überprüfen:
            if (!((floor(bitsize,intDsize) >= len)
                  || ((floor(bitsize,intDsize) == len-1)
                      && (TheBignum(obj)->data[0] < bit(bitsize%intDsize))
               ) )   )
              { fehler_bad_integer(stream,obj); }
            #if BIG_ENDIAN_P
            {var uintB* ptr = (uintB*)&TheBignum(obj)->data[len];
             # Digit-Länge in Byte-Länge umrechnen:
             len = (intDsize/8)*len;
             #define CHECK_NEXT_BYTE(i)  \
               if (!( ((uintB*)(&TheBignum(obj)->data[0]))[i] ==0)) goto len_ok; \
               len--;
             DOCONSTTIMES(intDsize/8,CHECK_NEXT_BYTE); # CHECK_NEXT_BYTE(0..intDsize/8-1)
             #undef CHECK_NEXT_BYTE
             len_ok:
             # obj im Bitbuffer ablegen:
             count = count - len;
             dotimespL(len,len, { *--bitbufferptr = *--ptr; } );
            }
            #else
            {var uintD* ptr = &TheBignum(obj)->data[len];
             len--;
             count -= (intDsize/8)*len;
             dotimesL(len,len,
               { var uintD digit = *--ptr;
                 doconsttimes(intDsize/8,
                   { *--bitbufferptr = (uintB)digit; digit = digit >> 8; }
                   );
               });
             {var uintD digit = *--ptr;
              doconsttimes(intDsize/8,
                { if (digit==0) goto ok;
                  *--bitbufferptr = (uintB)digit; digit = digit >> 8;
                  count--;
                });
              ok: ;
            }}
            #endif
          }
        dotimesL(count,count, { *--bitbufferptr = 0; } );
      }
      (*finisher)(stream,bitsize,bytesize);
    }}

# UP für WRITE-BYTE auf File-Streams für Integers, Art s :
# Legt das Objekt (ein Integer) als bytesize Bytes im Bitbuffer ab.
# > stream : File-Stream für Integers, Art s
# > obj : auszugebendes Objekt
# > finisher : Beendigungsroutine
  local void wr_by_ixs_file (object stream, object obj, wr_by_ix* finisher);
  local void wr_by_ixs_file(stream,obj,finisher)
    var object stream;
    var object obj;
    var wr_by_ix* finisher;
    { # obj überprüfen:
      if (!integerp(obj)) { fehler_wr_integer(stream,obj); }
      # obj ist jetzt ein Integer
     {var uintL bitsize = FileStream_bitsize(stream);
      var uintL bytesize = ceiling(bitsize,8);
      # obj in den Bitbuffer übertragen:
      { var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
        var uintL count = bytesize;
        var uintL sign = (sintL)R_sign(obj);
        if (fixnump(obj))
          # obj ist ein Fixnum
          { var uintL wert = fixnum_to_L(obj); # >=0 oder <0, je nach sign
            # 0 <= wert < 2^(bitsize-1) bzw. -2^(bitsize-1) <= wert < 0 überprüfen:
            wert = wert^sign;
            if (!((bitsize>oint_data_len) || (wert < bit(bitsize-1))))
              { fehler_bad_integer(stream,obj); }
            # wert^sign im Bitbuffer ablegen:
            until (wert == 0)
              { *--bitbufferptr = (uint8)(wert^sign); wert = wert>>8; count--; }
            dotimesL(count,count, { *--bitbufferptr = (uint8)sign; } );
          }
          else
          # obj ist ein Bignum
          { var uintL len = (uintL)Bignum_length(obj);
            # -2^(bitsize-1) <= obj < 2^(bitsize-1) überprüfen:
            if (!((floor(bitsize,intDsize) >= len)
                  || ((bitsize > intDsize*(len-1))
                      && ((TheBignum(obj)->data[0] ^ (uintD)sign) < bit((bitsize%intDsize)-1))
               ) )   )
              { fehler_bad_integer(stream,obj); }
            #if BIG_ENDIAN_P
            {var uintB* ptr = (uintB*)&TheBignum(obj)->data[len];
             # Digit-Länge in Byte-Länge umrechnen:
             len = (intDsize/8)*len;
             #define CHECK_NEXT_BYTE(i)  \
               if (!( ((uintB*)(&TheBignum(obj)->data[0]))[i] == (uintB)sign)) goto len_ok; \
               len--;
             DOCONSTTIMES(intDsize/8,CHECK_NEXT_BYTE); # CHECK_NEXT_BYTE(0..intDsize/8-1)
             #undef CHECK_NEXT_BYTE
             len_ok:
             # obj im Bitbuffer ablegen:
             count = count - len;
             dotimespL(len,len, { *--bitbufferptr = *--ptr; } );
            }
            #else
            {var uintD* ptr = &TheBignum(obj)->data[len];
             len--;
             count -= (intDsize/8)*len;
             dotimesL(len,len,
               { var uintD digit = *--ptr;
                 doconsttimes(intDsize/8,
                   { *--bitbufferptr = (uintB)digit; digit = digit >> 8; }
                   );
               });
             {var sintD digit = *--ptr;
              doconsttimes(intDsize/8,
                { if (digit == (sintD)sign) goto ok;
                  *--bitbufferptr = (uintB)digit; digit = digit >> 8;
                  count--;
                });
              ok: ;
            }}
            #endif
            dotimesL(count,count, { *--bitbufferptr = (uintB)sign; } );
          }
      }
      (*finisher)(stream,bitsize,bytesize);
    }}

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art au :
  local object rd_by_iau_file (object stream);
  local object rd_by_iau_file(stream)
    var object stream;
    { return rd_by_iax_file(stream,&rd_by_iu_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art au :
  local void wr_by_iau_file (object stream, object obj);
  local void wr_by_iau_file(stream,obj)
    var object stream;
    var object obj;
    { wr_by_ixu_file(stream,obj,&wr_by_ia); }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art as :
  local object rd_by_ias_file (object stream);
  local object rd_by_ias_file(stream)
    var object stream;
    { return rd_by_iax_file(stream,&rd_by_is_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art as :
  local void wr_by_ias_file (object stream, object obj);
  local void wr_by_ias_file(stream,obj)
    var object stream;
    var object obj;
    { wr_by_ixs_file(stream,obj,&wr_by_ia); }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art bu :
  local object rd_by_ibu_file (object stream);
  local object rd_by_ibu_file(stream)
    var object stream;
    { return rd_by_ibx_file(stream,&rd_by_iu_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art bu :
  local void wr_by_ibu_file (object stream, object obj);
  local void wr_by_ibu_file(stream,obj)
    var object stream;
    var object obj;
    { wr_by_ixu_file(stream,obj,&wr_by_ib); }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art bs :
  local object rd_by_ibs_file (object stream);
  local object rd_by_ibs_file(stream)
    var object stream;
    { return rd_by_ibx_file(stream,&rd_by_is_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art bs :
  local void wr_by_ibs_file (object stream, object obj);
  local void wr_by_ibs_file(stream,obj)
    var object stream;
    var object obj;
    { wr_by_ixs_file(stream,obj,&wr_by_ib); }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art cu :
  local object rd_by_icu_file (object stream);
  local object rd_by_icu_file(stream)
    var object stream;
    { return rd_by_icx_file(stream,&rd_by_iu_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art cu :
  local void wr_by_icu_file (object stream, object obj);
  local void wr_by_icu_file(stream,obj)
    var object stream;
    var object obj;
    { wr_by_ixu_file(stream,obj,&wr_by_ic); }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art cs :
  local object rd_by_ics_file (object stream);
  local object rd_by_ics_file(stream)
    var object stream;
    { return rd_by_icx_file(stream,&rd_by_is_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art cs :
  local void wr_by_ics_file (object stream, object obj);
  local void wr_by_ics_file(stream,obj)
    var object stream;
    var object obj;
    { wr_by_ixs_file(stream,obj,&wr_by_ic); }

# WRITE-BYTE-SEQUENCE für File-Streams für Integers, Art au, bitsize = 8 :
  local const uintB* write_byte_array_iau8_file (object stream, const uintB* byteptr, uintL len);
  local const uintB* write_byte_array_iau8_file(stream,byteptr,len)
    var object stream;
    var const uintB* byteptr;
    var uintL len;
    { var uintL remaining = len;
      var uintB* ptr;
      do # Noch remaining>0 Bytes abzulegen.
        { ptr = b_file_nextbyte(stream);
          if (ptr == (uintB*)NULL) goto eof_reached;
         {var sintL eofindex = FileStream_eofindex(stream);
          var uintL next = # so viel wie noch in den Buffer oder bis EOF passt
            ((eofindex==eofindex_all_valid) ? strm_file_bufflen : eofindex)
            - FileStream_index(stream); # > 0 !
          if (next > remaining) { next = remaining; }
          # next Bytes in den Buffer kopieren:
          {var uintL count;
           dotimespL(count,next,
             { var uintB b = *byteptr++; # nächstes Byte
               if (!(*ptr == b)) { *ptr = b; FileStream_modified(stream) = TRUE; } # in den Buffer
               ptr++;
             });
          }
          remaining = remaining - next;
          # index incrementieren:
          FileStream_index(stream) += next;
        }}
        until (remaining == 0);
      if (FALSE)
        eof_reached: # Schreiben am EOF, eofindex = index
        do # Noch remaining>0 Bytes abzulegen.
          { var uintL next = # so viel wie noch Platz im Buffer ist
              strm_file_bufflen - FileStream_index(stream);
            if (next==0)
              { # Buffer muss neu gefüllt werden. Da nach ihm sowieso EOF kommt,
                # genügt es, ihn hinauszuschreiben:
                if (FileStream_modified(stream)) { b_file_half_flush(stream); }
                FileStream_buffstart(stream) += strm_file_bufflen;
                FileStream_eofindex(stream) = 0; # eofindex := 0
                FileStream_index(stream) = 0; # index := 0
                FileStream_modified(stream) = FALSE; # unmodifiziert
                # Dann nochmals versuchen:
                next = strm_file_bufflen;
              }
            if (next > remaining) { next = remaining; }
            # next Bytes in den Buffer kopieren:
            {var uintL count;
             ptr = &TheSstring(TheStream(stream)->strm_file_buffer)->data[FileStream_index(stream)];
             dotimespL(count,next, { *ptr++ = *byteptr++; } );
             FileStream_modified(stream) = TRUE;
            }
            remaining = remaining - next;
            # index und eofindex incrementieren:
            FileStream_index(stream) += next;
            FileStream_eofindex(stream) += next;
          }
          until (remaining == 0);
      # position incrementieren:
      FileStream_position(stream) += len;
      return byteptr;
    }

# File-Stream allgemein
# =====================

# UP: Positioniert einen (offenen) File-Stream an den Anfang.
# position_file_start(stream);
# > stream : (offener) File-Stream.
# verändert in stream: index, eofindex, buffstart, ..., position, rd_ch_last
  local void position_file_start (object stream);
  local void position_file_start(stream)
    var object stream;
    { position_b_file(stream,
                      (TheStream(stream)->strmflags & strmflags_i_B) == strmflags_ib_B # Integer-Stream vom Typ b ?
                      ? sizeof(uintL) : 0 # ja -> Position 4, sonst Position 0
                     );
      switch (TheStream(stream)->strmflags & strmflags_i_B)
        { case strmflags_ib_B: case strmflags_ic_B:
            # Integer-Stream der Art b,c
            FileStream_bitindex(stream) = 0; # bitindex := 0
          default: break;
        }
      FileStream_position(stream) = 0; # position := 0
      TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
      TheStream(stream)->strmflags &= ~strmflags_unread_B;
    }

# UP: Positioniert einen (offenen) File-Stream an eine gegebene Position.
# position_file(stream,position);
# > stream : (offener) File-Stream.
# > position : neue (logische) Position
# verändert in stream: index, eofindex, buffstart, ..., position, rd_ch_last
  local void position_file (object stream, uintL position);
  local void position_file(stream,position)
    var object stream;
    var uintL position;
    { var uintB flags = TheStream(stream)->strmflags;
      if (flags & strmflags_i_B) # Integer-Stream ?
        { if ((flags & strmflags_i_B) == strmflags_ia_B)
            # Art a
            { var uintL bitsize = FileStream_bitsize(stream);
              position_b_file(stream,position*(bitsize/8));
            }
            else
            # Art b,c
            { position_i_file(stream,position); }
        }
        else
        { # Character-Stream
          position_b_file(stream,position);
          TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
        }
      FileStream_position(stream) = position;
    }

# UP: Positioniert einen (offenen) File-Stream ans Ende.
# position_file_end(stream);
# > stream : (offener) File-Stream.
# verändert in stream: index, eofindex, buffstart, ..., position, rd_ch_last
  local void position_file_end (object stream);
  local void position_file_end(stream)
    var object stream;
    { # evtl. Buffer hinausschreiben:
      if (FileStream_modified(stream)) { b_file_flush(stream); }
     {var uintL eofbytes; # EOF-Position, gemessen in Bytes
      # ans Ende positionieren:
      begin_system_call();
      file_lseek(stream,0,SEEK_END,eofbytes=);
      end_system_call();
      # logische Position berechnen und eofbytes korrigieren:
      { var uintL position; # logische Position
        var uintL eofbits = 0; # Bit-Ergänzung zu eofbytes
        var uintB flags = TheStream(stream)->strmflags;
        if (flags & strmflags_i_B) # Integer-Stream ?
          { var uintL bitsize = FileStream_bitsize(stream);
            if ((flags & strmflags_i_B) == strmflags_ia_B)
              # Art a
              { var uintL bytesize = bitsize/8;
                position = floor(eofbytes,bytesize);
                eofbytes = position*bytesize;
              }
            elif ((flags & strmflags_i_B) == strmflags_ib_B)
              # Art b
              { eofbytes -= sizeof(uintL); # Header berücksichtigen
                # Ist die gemerkte EOF-Position plausibel?
                position = FileStream_eofposition(stream);
                if (!(ceiling(position*bitsize,8)==eofbytes)) # ja -> verwende sie
                  { position = floor(eofbytes*8,bitsize); } # nein -> rechne sie neu aus
                # Rechne eofbytes und eofbits neu aus:
                eofbytes = floor(position*bitsize,8);
                eofbits = (position*bitsize)%8;
                eofbytes += sizeof(uintL); # Header berücksichtigen
              }
            else
              # Art c
              { position = floor(eofbytes*8,bitsize);
                eofbytes = floor(position*bitsize,8);
                eofbits = (position*bitsize)%8;
              }
          }
          else
          { # Character-Stream
            position = eofbytes;
          }
        if ((TheStream(stream)->strmflags & strmflags_rd_B) == 0)
          { # Positionieren:
            begin_system_call();
            file_lseek(stream,eofbytes,SEEK_SET,);
            end_system_call();
            FileStream_buffstart(stream) = eofbytes;
            FileStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
            FileStream_index(stream) = 0; # index := 0
            FileStream_modified(stream) = FALSE; # unmodifiziert
          }
          else
          { # auf den Anfang des letzten Sectors positionieren:
            { var uintL buffstart;
              begin_system_call();
              file_lseek(stream,floor(eofbytes,strm_file_bufflen)*strm_file_bufflen,SEEK_SET,buffstart=);
              end_system_call();
              FileStream_buffstart(stream) = buffstart;
            }
            # Sector lesen:
            FileStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
            FileStream_index(stream) = 0; # index := 0
            FileStream_modified(stream) = FALSE; # unmodifiziert
            { var uintL eofindex = eofbytes % strm_file_bufflen;
              if (!((eofindex==0) && (eofbits==0))) # EOF am Sectorende -> brauche nichts zu lesen
                { b_file_nextbyte(stream);
                  # Jetzt ist index=0. index und eofindex setzen:
                  FileStream_index(stream) = eofindex;
                  if (!(eofbits==0)) { eofindex += 1; }
                  FileStream_eofindex(stream) = eofindex;
            }   }
          }
        switch (flags & strmflags_i_B)
          { case strmflags_ib_B: case strmflags_ic_B:
              # Integer-Stream der Art b,c
              FileStream_bitindex(stream) = eofbits;
            default: break;
          }
        # position setzen:
        FileStream_position(stream) = position;
        TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
        TheStream(stream)->strmflags &= ~strmflags_unread_B;
    }}}

# UP: erzeugt ein File-Stream
# make_file_stream(handle,direction,type,eltype_size,append_flag)
# > handle: Handle des geöffneten Files
# > STACK_1: Filename, ein Pathname oder NIL
# > STACK_0: Truename, ein Pathname oder NIL
# > direction: Modus (0 = :PROBE, 1 = :INPUT, 4 = :OUTPUT, 5 = :IO, 3 = :INPUT-IMMUTABLE)
# > type: nähere Typinfo
#         (STRMTYPE_SCH_FILE oder STRMTYPE_CH_FILE oder
#          STRMTYPE_IU_FILE oder STRMTYPE_IS_FILE)
# > eltype_size: (bei Integer-Streams) Größe der Elemente in Bits,
#         ein Fixnum >0 und <intDsize*uintWC_max
# > append_flag: TRUE falls der Stream gleich ans Ende positioniert werden
#         soll, FALSE sonst
# < ergebnis: File-Stream (oder evtl. File-Handle-Stream)
# < STACK: aufgeräumt
# kann GC auslösen
  global object make_file_stream (object handle, uintB direction, uintB type, object eltype_size, boolean append_flag);
  global object make_file_stream(handle,direction,type,eltype_size,append_flag)
    var object handle;
    var uintB direction;
    var uintB type;
    var object eltype_size;
    var boolean append_flag;
    {
      #if defined(HANDLES)
      # Nur reguläre Files zu gebufferten File-Streams machen.
      # Alles andere gibt File-Handle-Streams, weil vermutlich lseek() nicht geht.
      if (!nullp(handle))
        { if (!regular_handle_p(TheHandle(handle)))
            { if (((type == strmtype_ch_file)
                   || ((type == strmtype_iu_file) && eq(eltype_size,fixnum(8)))
                  )
                  && !append_flag
                 )
                { return make_handle_stream(handle,direction); }
                else
                { # Truename noch in STACK_0, Wert für Slot PATHNAME von FILE-ERROR
                  pushSTACK(STACK_0);
                  pushSTACK(S(open));
                  fehler(file_error,
                         DEUTSCH ? "~: ~ ist kein reguläres File." :
                         ENGLISH ? "~: ~ is not a regular file." :
                         FRANCAIS ? "~: ~ n'est pas un fichier régulier." :
                         ""
                        );
        }   }   }
      #endif
     { # Flags:
       var uintB flags =
         (direction==0 ? 0 : # bei Modus :PROBE sind alle Flags =0
           # sonst:
           (direction<4 ? strmflags_rd_B : # Modus :INPUT[-IMMUTABLE] -> nur Read
            direction==4 && nullp(STACK_0) ? strmflags_wr_B : # Modus :OUTPUT ohne Filename -> nur Write
            strmflags_open_B # sonst Read/Write
           )
           &
           (type>=strmtype_iu_file ? strmflags_by_B : strmflags_ch_B) # auf Integers oder Characters
         );
       # Art von Integer-Streams:
       var uintB art;
       # Länge:
       var uintC xlen = sizeof(strm_file_extrafields_struct); # Das haben alle File-Streams
       if (type==strmtype_ch_file)
         { xlen = sizeof(strm_ch_file_extrafields_struct); } # Das haben die File-Streams für Characters
       elif (type>=strmtype_iu_file)
         { xlen = sizeof(strm_i_file_extrafields_struct); # Das haben die File-Streams für Integers maximal
           {var uintL bitsize = posfixnum_to_L(eltype_size);
            if ((bitsize%8)==0)
              { art = strmflags_ia_B; } # Art a
              else
              { if (bitsize<8)
                  { art = strmflags_ib_B; } # Art b
                  else
                  { art = strmflags_ic_B; } # Art c
           }  }
           flags |= art; # Art in die Flags mit aufnehmen
         }
       #if defined(FOREIGN_HANDLE) || !NIL_IS_CONSTANT
       pushSTACK(handle); # Handle retten
       #endif
      {# Stream allozieren:
       var object stream = allocate_stream(flags,type,strm_file_length,xlen);
       # und füllen:
       # Komponenten aller Streams:
       switch (type)
         { case strmtype_ch_file:
             TheStream(stream)->strm_rd_ch = P(rd_ch_ch_file);
             TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
             TheStream(stream)->strm_wr_ch = P(wr_ch_ch_file);
             TheStream(stream)->strm_wr_ss = P(wr_ss_ch_file);
             break;
           case strmtype_iu_file:
             TheStream(stream)->strm_rd_by =
               (art==strmflags_ia_B ? P(rd_by_iau_file) :
                art==strmflags_ib_B ? P(rd_by_ibu_file) :
                                      P(rd_by_icu_file)
               );
             TheStream(stream)->strm_wr_by =
               (art==strmflags_ia_B ? P(wr_by_iau_file) :
                art==strmflags_ib_B ? P(wr_by_ibu_file) :
                                      P(wr_by_icu_file)
               );
             break;
           case strmtype_is_file:
             TheStream(stream)->strm_rd_by =
               (art==strmflags_ia_B ? P(rd_by_ias_file) :
                art==strmflags_ib_B ? P(rd_by_ibs_file) :
                                      P(rd_by_ics_file)
               );
             TheStream(stream)->strm_wr_by =
               (art==strmflags_ia_B ? P(wr_by_ias_file) :
                art==strmflags_ib_B ? P(wr_by_ibs_file) :
                                      P(wr_by_ics_file)
               );
             break;
           default: NOTREACHED
         }
       # Default für READ-BYTE-Pseudofunktion:
       if ((flags & strmflags_rd_by_B)==0)
         { TheStream(stream)->strm_rd_by = P(rd_by_dummy); }
       # Default für WRITE-BYTE-Pseudofunktion:
       if ((flags & strmflags_wr_by_B)==0)
         { TheStream(stream)->strm_wr_by = P(wr_by_dummy); }
       # Default für READ-CHAR-Pseudofunktion:
       if ((flags & strmflags_rd_ch_B)==0)
         { TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
           TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
         }
       TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
       # Default für WRITE-CHAR-Pseudofunktion:
       if ((flags & strmflags_wr_ch_B)==0)
         { TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
           TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
         }
       TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
       # Komponenten von File-Streams:
       #if defined(FOREIGN_HANDLE) || !NIL_IS_CONSTANT
       handle = popSTACK(); # Handle zurück
       #endif
       TheStream(stream)->strm_file_truename = popSTACK(); # Truename eintragen
       TheStream(stream)->strm_file_name = popSTACK(); # Filename eintragen
       if (!nullp(handle)) # Handle=NIL -> Rest bereits mit NIL initialisiert, fertig
         { TheStream(stream)->strm_file_handle = handle; # Handle eintragen
           FileStream_buffstart(stream) = 0; # buffstart := 0
           # Buffer allozieren:
           pushSTACK(stream);
          {var object buffer = allocate_string(strm_file_bufflen); # neuer String
           stream = popSTACK();
           TheStream(stream)->strm_file_buffer = buffer;
          }
           FileStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
           FileStream_index(stream) = 0; # index := 0
           FileStream_modified(stream) = FALSE; # Buffer unmodifiziert
           FileStream_position(stream) = 0; # position := 0
           if (type==strmtype_ch_file)
             # File-Stream für Characters
             { FileStream_lineno(stream) = 1; }
           elif (type>=strmtype_iu_file)
             # File-Stream für Integers
             { FileStream_bitsize(stream) = posfixnum_to_L(eltype_size);
               # Bitbuffer allozieren:
               pushSTACK(stream);
              {var object bitbuffer = allocate_bit_vector(ceiling(posfixnum_to_L(eltype_size),8)*8);
               stream = popSTACK();
               TheStream(stream)->strm_file_bitbuffer = bitbuffer;
              }
               if (!(art==strmflags_ia_B))
                 # Arten b,c
                 { FileStream_bitindex(stream) = 0; # bitindex := 0
                   if (art==strmflags_ib_B)
                     # Art b
                     { # eofposition lesen:
                       var uintL eofposition = 0;
                       var uintC count;
                       for (count=0; count < 8*sizeof(uintL); count += 8 )
                         { var uintB* ptr = b_file_nextbyte(stream);
                           if (ptr == (uintB*)NULL) goto too_short;
                           eofposition |= ((*ptr) << count);
                           # index incrementieren, da gerade *ptr verarbeitet:
                           FileStream_index(stream) += 1;
                         }
                       if (FALSE)
                         { too_short:
                           # File zu kurz (< sizeof(uintL) Bytes)
                           if ((TheStream(stream)->strmflags & strmflags_wr_by_B) == 0) # Read-Only-Stream?
                             goto bad_eofposition;
                           # File Read/Write -> setze eofposition := 0
                           eofposition = 0;
                           position_b_file(stream,0); # an Position 0 positionieren
                          {var uintC count; # und eofposition = 0 herausschreiben
                           dotimespC(count,sizeof(uintL), { b_file_writebyte(stream,0); } );
                         }}
                       elif (eofposition > (uintL)(bitm(oint_data_len)-1))
                         { bad_eofposition:
                           # Keine gültige EOF-Position.
                           # File schließen und Error melden:
                           TheStream(stream)->strmflags &= ~strmflags_wr_by_B; # Stream Read-Only machen
                           pushSTACK(stream);
                           stream_close(&STACK_0);
                           # STACK_0 = Wert für Slot STREAM von STREAM-ERROR
                           pushSTACK(!nullp(TheStream(STACK_0)->strm_file_truename) ? TheStream(STACK_0)->strm_file_truename : STACK_0);
                           fehler(stream_error,
                                  DEUTSCH ? "File ~ hat nicht das Format eines Integer-Files." :
                                  ENGLISH ? "file ~ is not an integer file" :
                                  FRANCAIS ? "Le fichier ~ n'a pas le format d'un fichier d'entiers." :
                                  ""
                                 );
                         }
                       # Auf die gelesene EOF-Position verlassen wir uns jetzt!
                       FileStream_eofposition(stream) = eofposition;
             }   }   }
           # Liste der offenen File-Streams um stream erweitern:
           pushSTACK(stream);
          {var object new_cons = allocate_cons();
           Car(new_cons) = stream = popSTACK();
           Cdr(new_cons) = O(open_files);
           O(open_files) = new_cons;
          }# Modus :APPEND behandeln:
           if (append_flag) { position_file_end(stream); }
         }
       return stream;
    }}}

# UP: Bereitet das Schließen eines File-Streams vor.
# Dabei wird der Buffer und evtl. eofposition hinausgeschrieben.
# file_flush(stream);
# > stream : (offener) File-Stream.
# verändert in stream: index, eofindex, buffstart, ...
  local void file_flush (object stream);
  local void file_flush(stream)
    var object stream;
    { # Bei Integer-Streams (Art b) eofposition abspeichern:
      if ((TheStream(stream)->strmflags & strmflags_i_B) == strmflags_ib_B)
        if (TheStream(stream)->strmflags & strmflags_wr_by_B) # nur falls nicht Read-Only
          { position_b_file(stream,0); # an Position 0 positionieren
           {var uintL eofposition = FileStream_eofposition(stream);
            var uintC count;
            dotimespC(count,sizeof(uintL),
              { b_file_writebyte(stream,(uintB)eofposition);
                eofposition = eofposition>>8;
              });
          }}
      # evtl. Buffer hinausschreiben:
      if (FileStream_modified(stream)) { b_file_flush(stream); }
      # Nun ist das modified_flag gelöscht.
    }

# UP: Bringt den wartenden Output eines File-Stream ans Ziel.
# Schreibt dazu den Buffer des File-Streams (auch physikalisch) aufs File.
# finish_output_file(stream);
# > stream : File-Stream.
# verändert in stream: handle, index, eofindex, buffstart, ..., rd_ch_last
# kann GC auslösen
  local void finish_output_file (object stream);
  local void finish_output_file(stream)
    var object stream;
    { # Handle=NIL (Stream bereits geschlossen) -> fertig:
      if (nullp(TheStream(stream)->strm_file_handle)) { return; }
      # kein File mit Schreibzugriff -> gar nichts zu tun:
      if (!(TheStream(stream)->strmflags & strmflags_wr_B)) { return; }
      # evtl. Buffer und evtl. eofposition hinausschreiben:
      file_flush(stream);
      # Nun ist das modified_flag gelöscht.
      #ifdef UNIX
       #ifdef HAVE_FSYNC
        begin_system_call();
        if (!( fsync(TheHandle(TheStream(stream)->strm_file_handle)) ==0))
          { end_system_call(); OS_filestream_error(stream); }
        end_system_call();
       #endif
      #else
        if (!nullp(TheStream(stream)->strm_file_truename)) # avoid closing stdout_handle
          {
            #ifdef MSDOS
              # File-Handle duplizieren und schließen:
              { var uintW handle = TheHandle(TheStream(stream)->strm_file_handle);
                begin_system_call();
               {var sintW handle2 = dup(handle);
                if (handle2 < 0) { end_system_call(); OS_filestream_error(stream); } # Error melden
                if ( CLOSE(handle2) <0) { end_system_call(); OS_filestream_error(stream); }
                end_system_call();
              }}
            #endif
            #ifdef RISCOS # || MSDOS, wenn wir da nicht schon was besseres hätten
              # File schließen (DOS schreibt physikalisch):
              begin_system_call();
              if ( CLOSE(TheHandle(TheStream(stream)->strm_file_handle)) <0)
                { end_system_call(); OS_filestream_error(stream); }
              end_system_call();
              # File neu öffnen:
              pushSTACK(stream); # stream retten
              pushSTACK(TheStream(stream)->strm_file_truename); # Filename
             {# Directory existiert schon:
              var object namestring = assume_dir_exists(); # Filename als ASCIZ-String
              var sintW handle;
              begin_system_call();
              handle = OPEN(TheAsciz(namestring),O_RDWR); # Datei neu öffnen
              if (handle < 0) # Error melden
                { end_system_call(); OS_filestream_error(STACK_1); }
              #ifdef MSDOS
              setmode(handle,O_BINARY);
              #endif
              end_system_call();
              # Nun enthält handle das Handle des geöffneten Files.
              skipSTACK(1);
              stream = popSTACK(); # stream zurück
              # neues Handle eintragen:
              TheStream(stream)->strm_file_handle = allocate_handle(handle);
             }
            #endif
            #ifdef AMIGAOS
              #if 0 # Manche Devices vertragen es nicht, wenn man geöffnete Dateien
                    # zu- und wieder aufmacht. Z.B. bei Pipes hat das eine besondere
                    # Bedeutung.
              begin_system_call();
              {var Handle handle = TheHandle(TheStream(stream)->strm_file_handle);
               if (!IsInteractive(handle))
                 { # File schließen (OS schreibt physikalisch):
                   Close(handle);
                   # File neu öffnen:
                   pushSTACK(stream); # stream retten
                   pushSTACK(TheStream(stream)->strm_file_truename); # Filename
                  {# Directory existiert schon, Datei neu öffnen:
                   var object namestring = assume_dir_exists(); # Filename als ASCIZ-String
                   handle = Open(TheAsciz(namestring),MODE_OLDFILE);
                   if (handle==NULL) # Error melden
                     { end_system_call(); OS_filestream_error(STACK_1); }
                   skipSTACK(1);
                   stream = popSTACK(); # stream zurück
                   # neues Handle eintragen:
                   TheHandle(TheStream(stream)->strm_file_handle) = handle;
              }  }}
              end_system_call();
              #endif
            #endif
          }
      #endif
      # und neu positionieren:
     {var uintL position = FileStream_buffstart(stream) + FileStream_index(stream);
      FileStream_buffstart(stream) = 0; # buffstart := 0
      FileStream_index(stream) = 0; # index := 0
      FileStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
      position_b_file(stream,position);
     }# Komponenten position, ..., lastchar bleiben unverändert
    }

# UP: Bringt den wartenden Output eines File-Stream ans Ziel.
# Schreibt dazu den Buffer des File-Streams (auch physikalisch) aufs File.
# force_output_file(stream);
# > stream : File-Stream.
# verändert in stream: handle, index, eofindex, buffstart, ..., rd_ch_last
# kann GC auslösen
  #define force_output_file  finish_output_file

# UP: Erklärt einen File-Stream für geschlossen.
# closed_file(stream);
# > stream : (offener) File-Stream.
# verändert in stream: alle Komponenten außer name und truename
  local void closed_file (object stream);
  local void closed_file(stream)
    var object stream;
    { TheStream(stream)->strm_file_handle = NIL; # Handle wird ungültig
      TheStream(stream)->strm_file_buffer = NIL; # Buffer freimachen
      FileStream_buffstart(stream) = 0; # buffstart löschen (unnötig)
      FileStream_eofindex(stream) = eofindex_all_invalid; # eofindex löschen (unnötig)
      FileStream_index(stream) = 0; # index löschen (unnötig)
      FileStream_modified(stream) = FALSE; # modified_flag löschen (unnötig)
      FileStream_position(stream) = 0; # position löschen (unnötig)
      if (TheStream(stream)->strmflags & strmflags_i_B)
        { FileStream_bitsize(stream) = 0; # bitsize löschen (unnötig)
          TheStream(stream)->strm_file_bitbuffer = NIL; # Bitbuffer freimachen
        }
    }

# UP: Schließt einen File-Stream.
# close_file(stream);
# > stream : File-Stream.
# verändert in stream: alle Komponenten außer name und truename
  local void close_file (object stream);
  local void close_file(stream)
    var object stream;
    { # Handle=NIL (Stream bereits geschlossen) -> fertig:
      if (nullp(TheStream(stream)->strm_file_handle)) { return; }
      # evtl. Buffer und evtl. eofposition hinausschreiben:
      file_flush(stream);
      # Nun ist das modified_flag gelöscht.
      # File schließen:
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
      begin_system_call();
      if (!( CLOSE(TheHandle(TheStream(stream)->strm_file_handle)) ==0))
        { end_system_call(); OS_filestream_error(stream); }
      end_system_call();
      #endif
      #ifdef AMIGAOS
      begin_system_call();
      Close(TheHandle(TheStream(stream)->strm_file_handle));
      end_system_call();
      #endif
      #ifdef WIN32_NATIVE
      begin_system_call();
      if (!CloseHandle(TheHandle(TheStream(stream)->strm_file_handle)))
        { end_system_call(); OS_filestream_error(stream); }
      end_system_call();
      #endif
      # Komponenten ungültig machen (close_dummys kommt später):
      closed_file(stream);
      # stream aus der Liste aller offenen File-Streams streichen:
      O(open_files) = deleteq(O(open_files),stream);
    }

LISPFUNN(file_stream_p,1)
# (SYS::FILE-STREAM-P stream) == (TYPEP stream 'FILE-STREAM)
  { var object arg = popSTACK();
    if (streamp(arg))
      { if_strm_file_p(arg, { value1 = T; } , { value1 = NIL; } ); }
      else
      { value1 = NIL; }
    mv_count=1;
  }


# Synonym-Stream
# ==============

# Zusätzliche Komponenten:
  # define strm_synonym_symbol  strm_other[0]  # Symbol, auf dessen Wert verwiesen wird

# Macro: Liefert den Wert eines Symbols, ein Stream.
# get_synonym_stream(sym)
# > sym: Symbol
# < ergebnis: sein Wert, ein Stream
  #define get_synonym_stream(sym)  \
    ((!streamp(Symbol_value(sym))) ?         \
       (fehler_value_stream(sym), unbound) : \
       Symbol_value(sym)                     \
    )

# READ-BYTE - Pseudofunktion für Synonym-Streams:
  local object rd_by_synonym (object stream);
  local object rd_by_synonym(stream)
    var object stream;
    { check_SP();
     {var object symbol = TheStream(stream)->strm_synonym_symbol;
      return read_byte(get_synonym_stream(symbol));
    }}

# WRITE-BYTE - Pseudofunktion für Synonym-Streams:
  local void wr_by_synonym (object stream, object obj);
  local void wr_by_synonym(stream,obj)
    var object stream;
    var object obj;
    { check_SP();
     {var object symbol = TheStream(stream)->strm_synonym_symbol;
      write_byte(get_synonym_stream(symbol),obj);
    }}

# READ-CHAR - Pseudofunktion für Synonym-Streams:
  local object rd_ch_synonym (const object* stream_);
  local object rd_ch_synonym(stream_)
    var const object* stream_;
    {  check_SP(); check_STACK();
     { var object stream = *stream_;
       var object symbol = TheStream(stream)->strm_synonym_symbol;
       pushSTACK(get_synonym_stream(symbol));
      {var object ergebnis = read_char(&STACK_0);
       skipSTACK(1);
       return ergebnis;
    }}}

# PEEK-CHAR - Pseudofunktion für Synonym-Streams:
  local object pk_ch_synonym (const object* stream_);
  local object pk_ch_synonym(stream_)
    var const object* stream_;
    {  check_SP(); check_STACK();
     { var object stream = *stream_;
       var object symbol = TheStream(stream)->strm_synonym_symbol;
       pushSTACK(get_synonym_stream(symbol));
      {var object ergebnis = peek_char(&STACK_0);
       skipSTACK(1);
       return ergebnis;
    }}}

# WRITE-CHAR - Pseudofunktion für Synonym-Streams:
  local void wr_ch_synonym (const object* stream_, object obj);
  local void wr_ch_synonym(stream_,obj)
    var const object* stream_;
    var object obj;
    { check_SP(); check_STACK();
     {var object stream = *stream_;
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      write_char(&STACK_0,obj);
      skipSTACK(1);
    }}

# WRITE-SIMPLE-STRING - Pseudofunktion für Synonym-Streams:
  local void wr_ss_synonym (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_synonym(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { check_SP(); check_STACK();
     {var object symbol = TheStream(*stream_)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      wr_ss(STACK_0)(&STACK_0,string,start,len);
      skipSTACK(1);
      # Line-Position aktualisieren kann hier entfallen.
    }}

# Schließt einen Synonym-Stream.
# close_synonym(stream);
# > stream : Synonym-Stream
#ifdef X3J13_014
  #define close_synonym(stream)
#else
  local void close_synonym (object stream);
  local void close_synonym(stream)
    var object stream;
    { check_SP(); check_STACK();
     {var object symbol = TheStream(stream)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      stream_close(&STACK_0);
      skipSTACK(1);
    }}
#endif

# Stellt fest, ob ein Synonym-Stream ein Zeichen verfügbar hat.
# listen_synonym(stream)
# > stream : Synonym-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_synonym (object stream);
  local signean listen_synonym(stream)
    var object stream;
    { check_SP();
     {var object symbol = TheStream(stream)->strm_synonym_symbol;
      return stream_listen(get_synonym_stream(symbol));
    }}

# UP: Löscht bereits eingegebenen interaktiven Input von einem Synonym-Stream.
# clear_input_synonym(stream)
# > stream: Synonym-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# kann GC auslösen
  local boolean clear_input_synonym (object stream);
  local boolean clear_input_synonym(stream)
    var object stream;
    { check_SP();
     {var object symbol = TheStream(stream)->strm_synonym_symbol;
      return clear_input(get_synonym_stream(symbol));
    }}

# UP: Wartenden Output eines Synonym-Stream ans Ziel bringen.
# finish_output_synonym(stream);
# > stream: Synonym-Stream
# kann GC auslösen
  local void finish_output_synonym (object stream);
  local void finish_output_synonym(stream)
    var object stream;
    { check_SP();
     {var object symbol = TheStream(stream)->strm_synonym_symbol;
      finish_output(get_synonym_stream(symbol));
    }}

# UP: Wartenden Output eines Synonym-Stream ans Ziel bringen.
# force_output_synonym(stream);
# > stream: Synonym-Stream
# kann GC auslösen
  local void force_output_synonym (object stream);
  local void force_output_synonym(stream)
    var object stream;
    { check_SP();
     {var object symbol = TheStream(stream)->strm_synonym_symbol;
      force_output(get_synonym_stream(symbol));
    }}

# UP: Löscht den wartenden Output eines Synonym-Stream.
# clear_output_synonym(stream);
# > stream: Synonym-Stream
# kann GC auslösen
  local void clear_output_synonym (object stream);
  local void clear_output_synonym(stream)
    var object stream;
    { check_SP();
     {var object symbol = TheStream(stream)->strm_synonym_symbol;
      clear_output(get_synonym_stream(symbol));
    }}

# Liefert einen Synonym-Stream zu einem Symbol.
# make_synonym_stream(symbol)
# > symbol : Symbol
# < ergebnis : neuer Synonym-Stream
# kann GC auslösen
  local object make_synonym_stream (object symbol);
  local object make_synonym_stream(symbol)
    var object symbol;
    { pushSTACK(symbol); # Symbol retten
     {var object stream = # neuer Stream, alle Operationen erlaubt
        allocate_stream(strmflags_open_B,strmtype_synonym,strm_len+1,0);
      TheStream(stream)->strm_rd_by = P(rd_by_synonym);
      TheStream(stream)->strm_wr_by = P(wr_by_synonym);
      TheStream(stream)->strm_rd_ch = P(rd_ch_synonym);
      TheStream(stream)->strm_pk_ch = P(pk_ch_synonym);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_synonym);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_synonym);
      TheStream(stream)->strm_synonym_symbol = popSTACK();
      return stream;
    }}

LISPFUNN(make_synonym_stream,1)
# (MAKE-SYNONYM-STREAM symbol), CLTL S. 329
  { var object arg = popSTACK();
    if (!symbolp(arg))
      { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(symbol)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               DEUTSCH ? "~: Argument muss ein Symbol sein, nicht ~" :
               ENGLISH ? "~: argument should be a symbol, not ~" :
               FRANCAIS ? "~ : L'argument doit être un symbole et non ~":
               ""
              );
      }
    value1 = make_synonym_stream(arg); mv_count=1;
  }

LISPFUNN(synonym_stream_p,1)
# (SYS::SYNONYM-STREAM-P stream) == (TYPEP stream 'SYNONYM-STREAM)
  { var object arg = popSTACK();
    value1 = (streamp(arg) && (TheStream(arg)->strmtype == strmtype_synonym)
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(synonym_stream_symbol,1)
# (SYNONYM-STREAM-SYMBOL stream), CLtL2 S. 507
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (!(TheStream(stream)->strmtype == strmtype_synonym))
      { fehler_streamtype(stream,S(synonym_stream)); }
    value1 = TheStream(stream)->strm_synonym_symbol; mv_count=1;
  }


# Broadcast-Stream
# ================

# Zusätzliche Komponenten:
  # define strm_broad_list  strm_other[0] # Liste von Streams

# WRITE-BYTE - Pseudofunktion für Broadcast-Streams:
  local void wr_by_broad (object stream, object obj);
  local void wr_by_broad(stream,obj)
    var object stream;
    var object obj;
    { check_SP(); check_STACK();
      pushSTACK(obj);
      { var object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
        # obj auf jeden Stream aus der Liste ausgeben:
        while (consp(streamlist))
          { pushSTACK(Cdr(streamlist)); # restliche Streams
            write_byte(Car(streamlist),STACK_1); # obj ausgeben
            streamlist = popSTACK();
      }   }
      skipSTACK(1);
    }

# WRITE-CHAR - Pseudofunktion für Broadcast-Streams:
  local void wr_ch_broad (const object* stream_, object obj);
  local void wr_ch_broad(stream_,obj)
    var const object* stream_;
    var object obj;
    { check_SP(); check_STACK();
      pushSTACK(obj);
      pushSTACK(NIL); # dummy
      pushSTACK(TheStream(*stream_)->strm_broad_list); # Liste von Streams
      # obj auf jeden Stream aus der Liste ausgeben:
      while (mconsp(STACK_0))
        { # Stackaufbau: obj, dummy, streamlistr.
          STACK_1 = Car(STACK_0); # ein Stream aus der Liste
          write_char(&STACK_1,STACK_2); # obj ausgeben
          STACK_0 = Cdr(STACK_0);
        }
      skipSTACK(3);
    }

# WRITE-CHAR - Pseudofunktion für Broadcast-Streams:
  local void wr_ss_broad (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_broad(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { check_SP(); check_STACK();
      pushSTACK(string);
      pushSTACK(NIL); # dummy
      pushSTACK(TheStream(*stream_)->strm_broad_list); # Liste von Streams
      # string auf jeden Stream aus der Liste ausgeben:
      while (mconsp(STACK_0))
        { # Stackaufbau: string, dummy, streamlistr.
          STACK_1 = Car(STACK_0); # ein Stream aus der Liste
          wr_ss(STACK_1)(&STACK_1,STACK_2,start,len); # string-Stück ausgeben
          STACK_0 = Cdr(STACK_0);
        }
      skipSTACK(3);
      # Line-Position aktualisieren kann hier entfallen.
    }

# UP: Bringt den wartenden Output eines Broadcast-Stream ans Ziel.
# finish_output_broad(stream);
# > stream: Broadcast-Stream
# kann GC auslösen
  local void finish_output_broad (object stream);
  local void finish_output_broad(stream)
    var object stream;
    { check_SP(); check_STACK();
      { var object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
        # Jeden Stream aus der Liste einzeln behandeln:
        while (consp(streamlist))
          { pushSTACK(Cdr(streamlist)); # restliche Streams
            finish_output(Car(streamlist));
            streamlist = popSTACK();
      }   }
    }

# UP: Bringt den wartenden Output eines Broadcast-Stream ans Ziel.
# force_output_broad(stream);
# > stream: Broadcast-Stream
# kann GC auslösen
  local void force_output_broad (object stream);
  local void force_output_broad(stream)
    var object stream;
    { check_SP(); check_STACK();
      { var object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
        # Jeden Stream aus der Liste einzeln behandeln:
        while (consp(streamlist))
          { pushSTACK(Cdr(streamlist)); # restliche Streams
            force_output(Car(streamlist));
            streamlist = popSTACK();
      }   }
    }

# UP: Löscht den wartenden Output eines Broadcast-Stream.
# clear_output_broad(stream);
# > stream: Broadcast-Stream
# kann GC auslösen
  local void clear_output_broad (object stream);
  local void clear_output_broad(stream)
    var object stream;
    { check_SP(); check_STACK();
      { var object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
        # Jeden Stream aus der Liste einzeln behandeln:
        while (consp(streamlist))
          { pushSTACK(Cdr(streamlist)); # restliche Streams
            clear_output(Car(streamlist));
            streamlist = popSTACK();
      }   }
    }

# Liefert einen Broadcast-Stream zu einer Streamliste.
# make_broadcast_stream(list)
# > list : Liste von Streams
# < ergebnis : Broadcast-Stream
# Die Liste list wird dabei zerstört.
# kann GC auslösen
  local object make_broadcast_stream (object list);
  local object make_broadcast_stream(list)
    var object list;
    { pushSTACK(list); # list retten
     {var object stream = # neuer Stream, nur WRITEs erlaubt
        allocate_stream(strmflags_wr_B,strmtype_broad,strm_len+1,0);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_broad);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_broad);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_broad);
      TheStream(stream)->strm_broad_list = popSTACK();
      return stream;
    }}

# Liefert einen Broadcast-Stream zum Stream stream.
# make_broadcast1_stream(stream)
# > stream : Stream
# < ergebnis : Broadcast-Stream
# kann GC auslösen
  global object make_broadcast1_stream (object stream);
  global object make_broadcast1_stream(oldstream)
    var object oldstream;
    {  pushSTACK(oldstream);
       # oldstream in eine einelementige Liste packen:
     { var object new_cons = allocate_cons();
       Car(new_cons) = STACK_0;
      {var object stream = make_broadcast_stream(new_cons); # neuer Stream
       oldstream = popSTACK();
       # Line-Position übernehmen:
       TheStream(stream)->strm_wr_ch_lpos = TheStream(oldstream)->strm_wr_ch_lpos;
       return stream;
    }}}

LISPFUN(make_broadcast_stream,0,0,rest,nokey,0,NIL)
# (MAKE-BROADCAST-STREAM {stream}), CLTL S. 329
  { # Überprüfen, ob alle Argumente Streams sind:
    test_output_stream_args(rest_args_pointer,argcount);
    # zu einer Liste zusammenfassen:
   {var object list = listof(argcount);
    # Stream bauen:
    value1 = make_broadcast_stream(list); mv_count=1;
  }}

LISPFUNN(broadcast_stream_p,1)
# (SYS::BROADCAST-STREAM-P stream) == (TYPEP stream 'BROADCAST-STREAM)
  { var object arg = popSTACK();
    value1 = (streamp(arg) && (TheStream(arg)->strmtype == strmtype_broad)
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(broadcast_stream_streams,1)
# (BROADCAST-STREAM-STREAMS stream), CLtL2 S. 507
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (!(TheStream(stream)->strmtype == strmtype_broad))
      { fehler_streamtype(stream,S(broadcast_stream)); }
    # Liste der Streams sicherheitshalber kopieren
    value1 = copy_list(TheStream(stream)->strm_broad_list); mv_count=1;
  }


# Concatenated-Stream
# ===================

# Zusätzliche Komponenten:
  # define strm_concat_list   strm_other[0]  # Liste von Streams
  #define strm_concat_list2  strm_other[1]  # Liste der verbrauchten Streams

# READ-BYTE - Pseudofunktion für Concatenated-Streams:
  local object rd_by_concat (object stream);
  local object rd_by_concat(stream)
    var object stream;
    { check_SP(); check_STACK();
      pushSTACK(stream);
     {var object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
      var object ergebnis;
      while (consp(streamlist))
        { ergebnis = read_byte(Car(streamlist)); # Integer lesen
          if (!eq(ergebnis,eof_value)) { goto OK; } # nicht EOF ?
          # EOF erreicht -> verbrauchten Stream aus der Liste nehmen
          # und in die zweite Liste stecken:
          stream = STACK_0;
         {var object first_cons = TheStream(stream)->strm_concat_list;
          streamlist = Cdr(first_cons);
          Cdr(first_cons) = TheStream(stream)->strm_concat_list2;
          TheStream(stream)->strm_concat_list2 = first_cons;
          TheStream(stream)->strm_concat_list = streamlist;
        }}
      # alle Streams verbraucht -> liefere EOF:
      ergebnis = eof_value;
      OK: # ergebnis fertig
      skipSTACK(1);
      return ergebnis;
    }}

# READ-CHAR - Pseudofunktion für Concatenated-Streams:
  local object rd_ch_concat (const object* stream_);
  local object rd_ch_concat(stream_)
    var const object* stream_;
    { check_SP(); check_STACK();
     {var object streamlist = TheStream(*stream_)->strm_concat_list; # Liste von Streams
      while (consp(streamlist))
        { pushSTACK(Car(streamlist));
         {var object ergebnis = read_char(&STACK_0); # Character lesen
          skipSTACK(1);
          if (!eq(ergebnis,eof_value)) { return ergebnis; }
          # EOF erreicht -> verbrauchten Stream aus der Liste nehmen
          # und in die zweite Liste stecken:
          {var object stream = *stream_;
           var object first_cons = TheStream(stream)->strm_concat_list;
           streamlist = Cdr(first_cons);
           Cdr(first_cons) = TheStream(stream)->strm_concat_list2;
           TheStream(stream)->strm_concat_list2 = first_cons;
           TheStream(stream)->strm_concat_list = streamlist;
        }}}
      # alle Streams verbraucht -> liefere EOF:
      return eof_value;
    }}

# PEEK-CHAR - Pseudofunktion für Concatenated-Streams:
  local object pk_ch_concat (const object* stream_);
  local object pk_ch_concat(stream_)
    var const object* stream_;
    { check_SP(); check_STACK();
     {var object streamlist = TheStream(*stream_)->strm_concat_list; # Liste von Streams
      while (consp(streamlist))
        { pushSTACK(Car(streamlist));
         {var object ergebnis = peek_char(&STACK_0); # Character lesen
          skipSTACK(1);
          if (!eq(ergebnis,eof_value)) { return ergebnis; }
          # EOF erreicht -> verbrauchten Stream aus der Liste nehmen
          # und in die zweite Liste stecken:
          {var object stream = *stream_;
           var object first_cons = TheStream(stream)->strm_concat_list;
           streamlist = Cdr(first_cons);
           Cdr(first_cons) = TheStream(stream)->strm_concat_list2;
           TheStream(stream)->strm_concat_list2 = first_cons;
           TheStream(stream)->strm_concat_list = streamlist;
        }}}
      # alle Streams verbraucht -> liefere EOF:
      return eof_value;
    }}

# Stellt fest, ob ein Concatenated-Stream ein Zeichen verfügbar hat.
# listen_concat(stream)
# > stream : Concatenated-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_concat (object stream);
  local signean listen_concat(stream)
    var object stream;
    { pushSTACK(stream);
     {var object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
      var signean ergebnis;
      while (consp(streamlist))
        { ergebnis = stream_listen(Car(streamlist));
          if (ergebnis>=0) { goto OK; } # nicht EOF ?
          # EOF erreicht -> verbrauchten Stream aus der Liste nehmen
          # und in die zweite Liste stecken:
          stream = STACK_0;
         {var object first_cons = TheStream(stream)->strm_concat_list;
          streamlist = Cdr(first_cons);
          Cdr(first_cons) = TheStream(stream)->strm_concat_list2;
          TheStream(stream)->strm_concat_list2 = first_cons;
          TheStream(stream)->strm_concat_list = streamlist;
        }}
      # alle Streams verbraucht -> liefere EOF:
      ergebnis = signean_minus;
      OK: # ergebnis fertig
      skipSTACK(1);
      return ergebnis;
    }}

# UP: Löscht bereits eingegebenen interaktiven Input von einem
# Concatenated-Stream.
# clear_input_concat(stream)
# > stream: Concatenated-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# kann GC auslösen
  local boolean clear_input_concat (object stream);
  local boolean clear_input_concat(stream)
    var object stream;
    { var boolean ergebnis = FALSE; # noch kein Input gelöscht
      # alle Streams einzeln behandeln:
      var object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
      while (consp(streamlist))
        { pushSTACK(Cdr(streamlist)); # restliche Streamliste
          ergebnis |= clear_input(Car(streamlist)); # allen Input des Teilstreams löschen
          streamlist = popSTACK();
        }
      return ergebnis;
    }

# Liefert einen Concatenated-Stream zu einer Streamliste.
# make_concatenated_stream(list)
# > list : Liste von Streams
# < ergebnis : Concatenated-Stream
# Die Liste list wird dabei zerstört.
# kann GC auslösen
  local object make_concatenated_stream (object list);
  local object make_concatenated_stream(list)
    var object list;
    { pushSTACK(list); # list retten
     {var object stream = # neuer Stream, nur READs erlaubt
        allocate_stream(strmflags_rd_B,strmtype_concat,strm_len+2,0);
      TheStream(stream)->strm_rd_by = P(rd_by_concat);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_concat);
      TheStream(stream)->strm_pk_ch = P(pk_ch_concat);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
      TheStream(stream)->strm_concat_list = popSTACK();
      TheStream(stream)->strm_concat_list2 = NIL;
      return stream;
    }}

LISPFUN(make_concatenated_stream,0,0,rest,nokey,0,NIL)
# (MAKE-CONCATENATED-STREAM {stream}), CLTL S. 329
  { # Überprüfen, ob alle Argumente Streams sind:
    test_input_stream_args(rest_args_pointer,argcount);
    # zu einer Liste zusammenfassen:
   {var object list = listof(argcount);
    # Stream bauen:
    value1 = make_concatenated_stream(list); mv_count=1;
  }}

LISPFUNN(concatenated_stream_p,1)
# (SYS::CONCATENATED-STREAM-P stream) == (TYPEP stream 'CONCATENATED-STREAM)
  { var object arg = popSTACK();
    value1 = (streamp(arg) && (TheStream(arg)->strmtype == strmtype_concat)
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(concatenated_stream_streams,1)
# (CONCATENATED-STREAM-STREAMS stream), CLtL2 S. 507
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (!(TheStream(stream)->strmtype == strmtype_concat))
      { fehler_streamtype(stream,S(concatenated_stream)); }
    # Liste der Streams sicherheitshalber kopieren:
    # (revappend list2 (copy-list list))
    pushSTACK(TheStream(stream)->strm_concat_list);
    pushSTACK(copy_list(TheStream(stream)->strm_concat_list2));
    funcall(L(revappend),2);
  }


# Two-Way-Stream, Echo-Stream
# ===========================

# Zusätzliche Komponenten:
  #define strm_twoway_input   strm_other[0]  # Stream für Input
  #define strm_twoway_output  strm_other[1]  # Stream für Output

# WRITE-BYTE - Pseudofunktion für Two-Way- und Echo-Streams:
  local void wr_by_twoway (object stream, object obj);
  local void wr_by_twoway(stream,obj)
    var object stream;
    var object obj;
    { check_SP();
      write_byte(TheStream(stream)->strm_twoway_output,obj);
    }

# WRITE-CHAR - Pseudofunktion für Two-Way- und Echo-Streams:
  local void wr_ch_twoway (const object* stream_, object obj);
  local void wr_ch_twoway(stream_,obj)
    var const object* stream_;
    var object obj;
    { check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_output);
      write_char(&STACK_0,obj);
      skipSTACK(1);
    }

# WRITE-SIMPLE-STRING - Pseudofunktion für Two-Way- und Echo-Streams:
  local void wr_ss_twoway (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_twoway(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_output);
      wr_ss(STACK_0)(&STACK_0,string,start,len);
      skipSTACK(1);
      # Line-Position aktualisieren kann hier entfallen.
    }

# Stellt fest, ob ein Two-Way- oder Echo-Stream ein Zeichen verfügbar hat.
# listen_twoway(stream)
# > stream : Two-Way- oder Echo-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_twoway (object stream);
  local signean listen_twoway(stream)
    var object stream;
    { check_SP();
      return stream_listen(TheStream(stream)->strm_twoway_input);
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Two-Way-
# oder Echo-Stream.
# clear_input_twoway(stream)
# > stream: Two-Way- oder Echo-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# kann GC auslösen
  local boolean clear_input_twoway (object stream);
  local boolean clear_input_twoway(stream)
    var object stream;
    { check_SP();
      return clear_input(TheStream(stream)->strm_twoway_input);
    }

# UP: Bringt den wartenden Output eines Two-Way- oder Echo-Stream ans Ziel.
# finish_output_twoway(stream);
# > stream: Two-Way- oder Echo-Stream
# kann GC auslösen
  local void finish_output_twoway (object stream);
  local void finish_output_twoway(stream)
    var object stream;
    { check_SP();
      finish_output(TheStream(stream)->strm_twoway_output);
    }

# UP: Bringt den wartenden Output eines Two-Way- oder Echo-Stream ans Ziel.
# force_output_twoway(stream);
# > stream: Two-Way- oder Echo-Stream
# kann GC auslösen
  local void force_output_twoway (object stream);
  local void force_output_twoway(stream)
    var object stream;
    { check_SP();
      force_output(TheStream(stream)->strm_twoway_output);
    }

# UP: Löscht den wartenden Output eines Two-Way- oder Echo-Stream.
# clear_output_twoway(stream);
# > stream: Two-Way- oder Echo-Stream
# kann GC auslösen
  local void clear_output_twoway (object stream);
  local void clear_output_twoway(stream)
    var object stream;
    { check_SP();
      clear_output(TheStream(stream)->strm_twoway_output);
    }


# Two-Way-Stream
# ==============

# READ-BYTE - Pseudofunktion für Two-Way-Streams:
  local object rd_by_twoway (object stream);
  local object rd_by_twoway(stream)
    var object stream;
    { check_SP();
      return read_byte(TheStream(stream)->strm_twoway_input);
    }

# READ-CHAR - Pseudofunktion für Two-Way-Streams:
  local object rd_ch_twoway (const object* stream_);
  local object rd_ch_twoway(stream_)
    var const object* stream_;
    { check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
     {var object ergebnis = read_char(&STACK_0);
      skipSTACK(1);
      return ergebnis;
    }}

# PEEK-CHAR - Pseudofunktion für Two-Way-Streams:
  local object pk_ch_twoway (const object* stream_);
  local object pk_ch_twoway(stream_)
    var const object* stream_;
    { check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
     {var object ergebnis = peek_char(&STACK_0);
      skipSTACK(1);
      return ergebnis;
    }}

# Liefert einen Two-Way-Stream zu einem Input-Stream und einem Output-Stream.
# make_twoway_stream(input_stream,output_stream)
# > input_stream : Input-Stream
# > output_stream : Output-Stream
# < ergebnis : Two-Way-Stream
# kann GC auslösen
  global object make_twoway_stream (object input_stream, object output_stream);
  global object make_twoway_stream(input_stream,output_stream)
    var object input_stream;
    var object output_stream;
    { pushSTACK(input_stream); pushSTACK(output_stream); # Streams retten
     {var object stream = # neuer Stream, alle Operationen erlaubt
        allocate_stream(strmflags_open_B,strmtype_twoway,strm_len+2,0);
      TheStream(stream)->strm_rd_by = P(rd_by_twoway);
      TheStream(stream)->strm_wr_by = P(wr_by_twoway);
      TheStream(stream)->strm_rd_ch = P(rd_ch_twoway);
      TheStream(stream)->strm_pk_ch = P(pk_ch_twoway);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_twoway);
      output_stream = popSTACK(); input_stream = popSTACK(); # Streams zurück
      TheStream(stream)->strm_wr_ch_lpos = TheStream(output_stream)->strm_wr_ch_lpos;
      TheStream(stream)->strm_wr_ss = P(wr_ss_twoway);
      TheStream(stream)->strm_twoway_input = input_stream;
      TheStream(stream)->strm_twoway_output = output_stream;
      return stream;
    }}

LISPFUNN(make_two_way_stream,2)
# (MAKE-TWO-WAY-STREAM input-stream output-stream), CLTL S. 329
  { # Überprüfen, ob beides Streams sind:
    test_stream_args(args_end_pointer STACKop 2, 2);
   {var object output_stream = popSTACK();
    var object input_stream = popSTACK();
    test_input_stream(input_stream);
    test_output_stream(output_stream);
    # Stream bauen:
    value1 = make_twoway_stream(input_stream,output_stream); mv_count=1;
  }}

LISPFUNN(two_way_stream_p,1)
# (SYS::TWO-WAY-STREAM-P stream) == (TYPEP stream 'TWO-WAY-STREAM)
  { var object arg = popSTACK();
    value1 = (streamp(arg) && (TheStream(arg)->strmtype == strmtype_twoway)
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(two_way_stream_input_stream,1)
# (TWO-WAY-STREAM-INPUT-STREAM stream), CLtL2 S. 507
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (!(TheStream(stream)->strmtype == strmtype_twoway))
      { fehler_streamtype(stream,S(two_way_stream)); }
    value1 = TheStream(stream)->strm_twoway_input; mv_count=1;
  }

LISPFUNN(two_way_stream_output_stream,1)
# (TWO-WAY-STREAM-OUTPUT-STREAM stream), CLtL2 S. 507
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (!(TheStream(stream)->strmtype == strmtype_twoway))
      { fehler_streamtype(stream,S(two_way_stream)); }
    value1 = TheStream(stream)->strm_twoway_output; mv_count=1;
  }


# Echo-Stream
# ===========

# READ-BYTE - Pseudofunktion für Echo-Streams:
  local object rd_by_echo (object stream);
  local object rd_by_echo(stream)
    var object stream;
    { check_SP(); check_STACK();
      pushSTACK(stream);
     {var object obj = read_byte(TheStream(stream)->strm_twoway_input);
      stream = popSTACK();
      if (!eq(obj,eof_value))
        { pushSTACK(obj);
          write_byte(TheStream(stream)->strm_twoway_output,obj);
          obj = popSTACK();
        }
      return obj;
    }}

# READ-CHAR - Pseudofunktion für Echo-Streams:
  local object rd_ch_echo (const object* stream_);
  local object rd_ch_echo(stream_)
    var const object* stream_;
    { check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
     {var object obj = read_char(&STACK_0);
      if (!eq(obj,eof_value))
        { STACK_0 = TheStream(*stream_)->strm_twoway_output;
          pushSTACK(obj);
          write_char(&STACK_1,obj);
          obj = popSTACK();
        }
      skipSTACK(1);
      return obj;
    }}

# Liefert einen Echo-Stream zu einem Input-Stream und einem Output-Stream.
# make_echo_stream(input_stream,output_stream)
# > input_stream : Input-Stream
# > output_stream : Output-Stream
# < ergebnis : Echo-Stream
# kann GC auslösen
  local object make_echo_stream (object input_stream, object output_stream);
  local object make_echo_stream(input_stream,output_stream)
    var object input_stream;
    var object output_stream;
    { pushSTACK(input_stream); pushSTACK(output_stream); # Streams retten
     {var uintB flags = strmflags_open_B;
      var object stream = # neuer Stream, alle Operationen erlaubt
        allocate_stream(flags,strmtype_echo,strm_len+2,0);
      TheStream(stream)->strm_rd_by = P(rd_by_echo);
      TheStream(stream)->strm_wr_by = P(wr_by_twoway);
      TheStream(stream)->strm_rd_ch = P(rd_ch_echo);
      TheStream(stream)->strm_pk_ch = P(pk_ch_twoway);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_twoway);
      output_stream = popSTACK(); input_stream = popSTACK(); # Streams zurück
      TheStream(stream)->strm_wr_ch_lpos = TheStream(output_stream)->strm_wr_ch_lpos;
      TheStream(stream)->strm_wr_ss = P(wr_ss_twoway);
      TheStream(stream)->strm_twoway_input = input_stream;
      TheStream(stream)->strm_twoway_output = output_stream;
      return stream;
    }}

LISPFUNN(make_echo_stream,2)
# (MAKE-ECHO-STREAM input-stream output-stream), CLTL S. 330
  { # Überprüfen, ob beides Streams sind:
    test_stream_args(args_end_pointer STACKop 2, 2);
   {var object output_stream = popSTACK();
    var object input_stream = popSTACK();
    test_input_stream(input_stream);
    test_output_stream(output_stream);
    # Stream bauen:
    value1 = make_echo_stream(input_stream,output_stream); mv_count=1;
  }}

LISPFUNN(echo_stream_p,1)
# (SYS::ECHO-STREAM-P stream) == (TYPEP stream 'ECHO-STREAM)
  { var object arg = popSTACK();
    value1 = (streamp(arg) && (TheStream(arg)->strmtype == strmtype_echo)
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(echo_stream_input_stream,1)
# (ECHO-STREAM-INPUT-STREAM stream), CLtL2 S. 507
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (!(TheStream(stream)->strmtype == strmtype_echo))
      { fehler_streamtype(stream,S(echo_stream)); }
    value1 = TheStream(stream)->strm_twoway_input; mv_count=1;
  }

LISPFUNN(echo_stream_output_stream,1)
# (ECHO-STREAM-OUTPUT-STREAM stream), CLtL2 S. 507
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (!(TheStream(stream)->strmtype == strmtype_echo))
      { fehler_streamtype(stream,S(echo_stream)); }
    value1 = TheStream(stream)->strm_twoway_output; mv_count=1;
  }


# String-Input-Stream
# ===================

# Zusätzliche Komponenten:
  #define strm_str_in_string    strm_other[0]  # String für Input
  #define strm_str_in_index     strm_other[1]  # Index in den String (Fixnum >=0)
  #define strm_str_in_endindex  strm_other[2]  # Endindex (Fixnum >= index >=0)

# Fehlermeldung, wenn index >= length(string):
# fehler_str_in_adjusted(stream);
# > stream: problematischer String-Input-Stream
  nonreturning_function(local, fehler_str_in_adjusted, (object stream));
  local void fehler_str_in_adjusted(stream)
    var object stream;
    { pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(TheStream(stream)->strm_str_in_string);
      pushSTACK(stream);
      fehler(stream_error,
             DEUTSCH ? "~ hinterm Stringende angelangt, weil String ~ adjustiert wurde." :
             ENGLISH ? "~ is beyond the end because the string ~ has been adjusted" :
             FRANCAIS ? "~ est arrivé après la fin de la chaîne, parce que la chaîne ~ a été ajustée." :
             ""
            );
    }

# READ-CHAR - Pseudofunktion für String-Input-Streams:
  local object rd_ch_str_in (const object* stream_);
  local object rd_ch_str_in(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      var uintL index = posfixnum_to_L(TheStream(stream)->strm_str_in_index); # Index
      var uintL endindex = posfixnum_to_L(TheStream(stream)->strm_str_in_endindex);
      if (index >= endindex)
        { return eof_value; } # EOF erreicht
        else
        # index < eofindex
        { var uintL len;
          var uintB* charptr = unpack_string(TheStream(stream)->strm_str_in_string,&len);
          # Ab charptr kommen len Zeichen.
          if (index >= len) # Index zu groß ?
            { fehler_str_in_adjusted(stream); }
         {var object ch = code_char(charptr[index]); # Zeichen aus dem String holen
          # Index erhöhen:
          TheStream(stream)->strm_str_in_index = fixnum_inc(TheStream(stream)->strm_str_in_index,1);
          return ch;
        }}
    }

# Schließt einen String-Input-Stream.
# close_str_in(stream);
# > stream : String-Input-Stream
  local void close_str_in (object stream);
  local void close_str_in(stream)
    var object stream;
    { TheStream(stream)->strm_str_in_string = NIL; } # String := NIL

# Stellt fest, ob ein String-Input-Stream ein Zeichen verfügbar hat.
# listen_str_in(stream)
# > stream : String-Input-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_str_in (object stream);
  local signean listen_str_in(stream)
    var object stream;
    { var uintL index = posfixnum_to_L(TheStream(stream)->strm_str_in_index); # Index
      var uintL endindex = posfixnum_to_L(TheStream(stream)->strm_str_in_endindex);
      if (index >= endindex)
        { return signean_minus; } # EOF erreicht
        else
        { return signean_null; }
    }

LISPFUN(make_string_input_stream,1,2,norest,nokey,0,NIL)
# (MAKE-STRING-INPUT-STREAM string [start [end]]), CLTL S. 330
  { # String holen und Grenzen überprüfen:
    var object string;
    var uintL start;
    var uintL len;
    test_string_limits(&string,&start,&len);
   {var object start_arg = fixnum(start); # start-Argument (Fixnum >=0)
    var object end_arg = fixnum_inc(start_arg,len); # end-Argument (Fixnum >=0)
    pushSTACK(string); # String retten
    { var object stream = # neuer Stream, nur READ-CHAR erlaubt
        allocate_stream(strmflags_rd_ch_B,strmtype_str_in,strm_len+3,0);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_str_in);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
      TheStream(stream)->strm_str_in_string = popSTACK();
      TheStream(stream)->strm_str_in_index = start_arg; # Index := start-Argument
      TheStream(stream)->strm_str_in_endindex = end_arg; # Endindex := end-Argument
      value1 = stream; mv_count=1; # stream als Wert
  }}}

LISPFUNN(string_input_stream_index,1)
# (SYSTEM::STRING-INPUT-STREAM-INDEX string-input-stream) liefert den Index
  { var object stream = popSTACK(); # Argument
    # muss ein String-Input-Stream sein:
    if (!(streamp(stream) && (TheStream(stream)->strmtype == strmtype_str_in)))
      { pushSTACK(stream);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               DEUTSCH ? "~: ~ ist kein String-Input-Stream." :
               ENGLISH ? "~: ~ is not a string input stream" :
               FRANCAIS ? "~ : ~ n'est pas un «stream» lisant d'une chaîne." :
               ""
              );
      }
   {var object index = TheStream(stream)->strm_str_in_index;
    # Falls ein Character mit UNREAD-CHAR zurückgeschoben wurde,
    # verwende (1- index), ein Fixnum >=0, als Wert:
    if (TheStream(stream)->strmflags & strmflags_unread_B)
      { index = fixnum_inc(index,-1); }
    value1 = index; mv_count=1;
  }}


# String-Output-Stream
# ====================

# Zusätzliche Komponenten:
  #define strm_str_out_string  strm_other[0]  # Semi-Simple-String für Output

# WRITE-CHAR - Pseudofunktion für String-Output-Streams:
  local void wr_ch_str_out (const object* stream_, object ch);
  local void wr_ch_str_out(stream_,ch)
    var const object* stream_;
    var object ch;
    { var object stream = *stream_;
      # obj sollte Character sein:
      if (!charp(ch)) { fehler_wr_char(stream,ch); }
      # Character in den String schieben:
      ssstring_push_extend(TheStream(stream)->strm_str_out_string,char_code(ch));
    }

# WRITE-SIMPLE-STRING - Pseudofunktion für String-Output-Streams:
  local void wr_ss_str_out (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_str_out(stream_,srcstring,start,len)
    var const object* stream_;
    var object srcstring;
    var uintL start;
    var uintL len;
    { if (len==0) return;
     {var object ssstring = TheStream(*stream_)->strm_str_out_string; # Semi-Simple-String
      var uintL old_len = TheIarray(ssstring)->dims[1]; # jetzige Länge = Fill-Pointer
      if (old_len + len > TheIarray(ssstring)->dims[0]) # passen keine len Bytes mehr hinein
        { pushSTACK(srcstring);
          ssstring = ssstring_extend(ssstring,old_len+len); # dann länger machen
          srcstring = popSTACK();
        }
      # Zeichen hineinschieben:
      {var uintB* srcptr = &TheSstring(srcstring)->data[start];
       var uintL count;
       {var uintB* ptr = &TheSstring(TheIarray(ssstring)->data)->data[old_len];
        dotimespL(count,len, { *ptr++ = *srcptr++; } );
       }
       # und Fill-Pointer erhöhen:
       TheIarray(ssstring)->dims[1] = old_len + len;
       wr_ss_lpos(*stream_,srcptr,len); # Line-Position aktualisieren
    }}}

# Liefert einen String-Output-Stream.
# make_string_output_stream()
# kann GC auslösen
  global object make_string_output_stream (void);
  global object make_string_output_stream()
    { # kleinen Semi-Simple-String der Länge 50 allozieren:
      pushSTACK(make_ssstring(50));
     {var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_str_out,strm_len+1,0);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_str_out);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_str_out);
      TheStream(stream)->strm_str_out_string = popSTACK(); # String eintragen
      return stream;
    }}

LISPFUN(make_string_output_stream,0,0,norest,key,2, (kw(element_type),kw(line_position)))
# (MAKE-STRING-OUTPUT-STREAM [:element-type] [:line-position])
  { # line-position überprüfen:
    if (eq(STACK_0,unbound))
      { STACK_0 = Fixnum_0; } # Defaultwert 0
      else
      # line-position angegeben, sollte ein Fixnum >=0 sein:
      { if (!posfixnump(STACK_0)) { fehler_posfixnum(STACK_0); } }
    # element-type überprüfen:
    if (!eq(STACK_1,unbound))
      { var object eltype = STACK_1;
        if (!eq(eltype,S(character)))
          { # Verify (SUBTYPEP eltype 'CHARACTER):
            pushSTACK(eltype); pushSTACK(S(character)); funcall(S(subtypep),2);
            if (nullp(value1))
              { pushSTACK(STACK_1); # eltype
                pushSTACK(S(character)); # CHARACTER
                pushSTACK(S(Kelement_type)); # :ELEMENT-TYPE
                pushSTACK(S(make_string_output_stream));
                fehler(error,
                       DEUTSCH ? "~: ~-Argument muss ein Untertyp von ~ sein, nicht ~" :
                       ENGLISH ? "~: ~ argument must be a subtype of ~, not ~" :
                       FRANCAIS ? "~ : L'argument ~ doit être un sous-type de ~, et non ~" :
                       ""
                      );
      }   }   }
   {var object stream = make_string_output_stream(); # String-Output-Stream
    TheStream(stream)->strm_wr_ch_lpos = popSTACK(); # Line Position eintragen
    value1 = stream; mv_count=1; # stream als Wert
    skipSTACK(1);
  }}

# UP: Liefert das von einem String-Output-Stream Angesammelte.
# get_output_stream_string(&stream)
# > stream: String-Output-Stream
# < stream: geleerter Stream
# < ergebnis: Angesammeltes, ein Simple-String
# kann GC auslösen
  global object get_output_stream_string (const object* stream_);
  global object get_output_stream_string(stream_)
    var const object* stream_;
    { var object string = TheStream(*stream_)->strm_str_out_string; # alter String
      string = coerce_ss(string); # in Simple-String umwandeln (erzwingt ein Kopieren)
      # alten String durch Fill-Pointer:=0 leeren:
      TheIarray(TheStream(*stream_)->strm_str_out_string)->dims[1] = 0;
      return string;
    }

LISPFUNN(get_output_stream_string,1)
# (GET-OUTPUT-STREAM-STRING string-output-stream), CLTL S. 330
  { var object stream = STACK_0; # Argument
    # muss ein String-Output-Stream sein:
    if (!(streamp(stream) && (TheStream(stream)->strmtype == strmtype_str_out)))
      { # stream in STACK_0
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               DEUTSCH ? "~: ~ ist kein String-Output-Stream." :
               ENGLISH ? "~: ~ is not a string output stream" :
               FRANCAIS ? "~ : ~ n'est pas un «stream» écrivant dans une chaîne." :
               ""
              );
      }
   {value1 = get_output_stream_string(&STACK_0); mv_count=1; # Angesammeltes als Wert
    skipSTACK(1);
  }}


# String-Push-Stream
# ==================

# Zusätzliche Komponenten:
  #define strm_str_push_string  strm_other[0]  # String mit Fill-Pointer für Output

# WRITE-CHAR - Pseudofunktion für String-Push-Streams:
  local void wr_ch_str_push (const object* stream_, object ch);
  local void wr_ch_str_push(stream_,ch)
    var const object* stream_;
    var object ch;
    { var object stream = *stream_;
      # ch sollte Character sein:
      if (!charp(ch)) { fehler_wr_char(stream,ch); }
      # Character in den String schieben:
      pushSTACK(ch); pushSTACK(TheStream(stream)->strm_str_push_string);
      funcall(L(vector_push_extend),2); # (VECTOR-PUSH-EXTEND ch string)
    }

# (SYSTEM::MAKE-STRING-PUSH-STREAM string) liefert einen Stream, dessen
# WRITE-CHAR-Operation mit einem VECTOR-PUSH-EXTEND auf den gegebenen String
# äquivalent ist.
LISPFUNN(make_string_push_stream,1)
  { {var object arg = STACK_0; # Argument
     # muss ein String mit Fill-Pointer sein:
     if (!(stringp(arg) && array_has_fill_pointer_p(arg)))
       { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
         pushSTACK(O(type_string_with_fill_pointer)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
         pushSTACK(S(with_output_to_string));
         fehler(type_error,
                DEUTSCH ? "~: Argument muss ein String mit Fill-Pointer sein, nicht ~" :
                ENGLISH ? "~: argument ~ should be a string with fill pointer" :
                FRANCAIS ? "~ : L'argument doit être une chaîne munie d'un pointeur de remplissage et non ~." :
                ""
               );
    }  }
    {var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
       allocate_stream(strmflags_wr_ch_B,strmtype_str_push,strm_len+1,0);
     TheStream(stream)->strm_rd_by = P(rd_by_dummy);
     TheStream(stream)->strm_wr_by = P(wr_by_dummy);
     TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
     TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
     TheStream(stream)->strm_rd_ch_last = NIL;
     TheStream(stream)->strm_wr_ch = P(wr_ch_str_push);
     TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
     TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
     TheStream(stream)->strm_str_push_string = popSTACK(); # String eintragen
     value1 = stream; mv_count=1; # stream als Wert
  } }


# String-Stream allgemein
# =======================

LISPFUNN(string_stream_p,1)
# (SYS::STRING-STREAM-P stream) == (TYPEP stream 'STRING-STREAM)
  { var object arg = popSTACK();
    if (streamp(arg))
      { switch (TheStream(arg)->strmtype)
          { case strmtype_str_in:   # String-Input-Stream
            case strmtype_str_out:  # String-Output-Stream
            case strmtype_str_push: # String-Push-Stream
              value1 = T; break;
            default:
              value1 = NIL; break;
      }   }
      else
      { value1 = NIL; }
    mv_count=1;
  }


# Pretty-Printer-Hilfs-Stream
# ===========================

# Zusätzliche Komponenten:
  # define strm_pphelp_strings  strm_other[0]   # Semi-Simple-Strings für Output
  # define strm_pphelp_modus    strm_other[1]   # Modus (NIL=Einzeiler, T=Mehrzeiler)

# WRITE-CHAR - Pseudofunktion für Pretty-Printer-Hilfs-Streams:
  local void wr_ch_pphelp (const object* stream_, object ch);
  local void wr_ch_pphelp(stream_,ch)
    var const object* stream_;
    var object ch;
    { var object stream = *stream_;
      # ch sollte Character sein:
      if (!charp(ch)) { fehler_wr_char(stream,ch); }
     {var uintB c = char_code(ch); # Character
      # Bei NL: Ab jetzt  Modus := Mehrzeiler
      if (c == NL) { TheStream(stream)->strm_pphelp_modus = T; }
      # Character in den ersten String schieben:
      ssstring_push_extend(Car(TheStream(stream)->strm_pphelp_strings),c);
    }}

# WRITE-SIMPLE-STRING - Pseudofunktion für Pretty-Printer-Hilfs-Streams:
  local void wr_ss_pphelp (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_pphelp(stream_,srcstring,start,len)
    var const object* stream_;
    var object srcstring;
    var uintL start;
    var uintL len;
    { if (len==0) return;
     {var object ssstring = Car(TheStream(*stream_)->strm_pphelp_strings); # Semi-Simple-String
      var uintL old_len = TheIarray(ssstring)->dims[1]; # jetzige Länge = Fill-Pointer
      if (old_len + len > TheIarray(ssstring)->dims[0]) # passen keine len Bytes mehr hinein
        { pushSTACK(srcstring);
          ssstring = ssstring_extend(ssstring,old_len+len); # dann länger machen
          srcstring = popSTACK();
        }
      # Zeichen hineinschieben:
      {var uintB* srcptr = &TheSstring(srcstring)->data[start];
       var uintL count;
       {var uintB* ptr = &TheSstring(TheIarray(ssstring)->data)->data[old_len];
        dotimespL(count,len, { *ptr++ = *srcptr++; } );
       }
       # und Fill-Pointer erhöhen:
       TheIarray(ssstring)->dims[1] = old_len + len;
       if (wr_ss_lpos(*stream_,srcptr,len)) # Line-Position aktualisieren
         { TheStream(*stream_)->strm_pphelp_modus = T; } # Nach NL: Modus := Mehrzeiler
    }}}

# UP: Liefert einen Pretty-Printer-Hilfs-Stream.
# make_pphelp_stream()
# kann GC auslösen
  global object make_pphelp_stream (void);
  global object make_pphelp_stream()
    { # kleinen Semi-Simple-String der Länge 50 allozieren:
      pushSTACK(make_ssstring(50));
      # einelementige Stringliste bauen:
     {var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK();
      pushSTACK(new_cons);
     }
     {var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_pphelp,strm_len+2,0);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_pphelp);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_pphelp);
      TheStream(stream)->strm_pphelp_strings = popSTACK(); # String-Liste eintragen
      TheStream(stream)->strm_pphelp_modus = NIL; # Modus := Einzeiler
      return stream;
    }}


# Buffered-Input-Stream
# =====================

# Elementtyp: character
# Richtungen: nur input
# (make-buffered-input-stream fun mode) liefert einen solchen.
#   Dabei ist fun eine Funktion von 0 Argumenten, die bei Aufruf
#   entweder NIL (steht für EOF) oder bis zu drei Werte string, start, end
#   zurückliefert.
#   Funktionsweise: (read-char ...) liefert nacheinander die Zeichen des
#   aktuellen Strings; ist der zu Ende, wird fun aufgerufen, und ist der
#   Wert davon ein String, so wird der neue aktuelle String gegeben durch
#     (multiple-value-bind (str start end) (funcall fun)
#       (subseq str (or start 0) (or end 'NIL))
#     )
#   Der von fun zurückgegebene String sollte nicht verändert werden.
#   (Ansonsten sollte fun vorher den String mit COPY-SEQ kopieren.)
#   mode bestimmt, wie sich der Stream bezüglich LISTEN verhält.
#   mode = NIL: Stream verhält sich wie ein File-Stream, d.h. bei LISTEN
#               und leerem aktuellen String wird fun aufgerufen.
#   mode = T: Stream verhält sich wie ein interaktiver Stream ohne EOF,
#             d.h. man kann davon ausgehen, das stets noch weitere Zeichen
#             kommen, auch ohne fun aufzurufen.
#   mode eine Funktion: Diese Funktion teilt, wenn aufgerufen, mit, ob
#             noch weitere nichtleere Strings zu erwarten sind.
#   (clear-input ...) beendet die Bearbeitung des aktuellen Strings.

# Zusätzliche Komponenten:
  # define strm_buff_in_fun      strm_other[0]  # Lesefunktion
  #define strm_buff_in_mode      strm_other[1]  # Modus oder Listen-Funktion
  #define strm_buff_in_string    strm_other[2]  # aktueller String für Input
  #define strm_buff_in_index     strm_other[3]  # Index in den String (Fixnum >=0)
  #define strm_buff_in_endindex  strm_other[4]  # Endindex (Fixnum >= index >=0)

# READ-CHAR - Pseudofunktion für Buffered-Input-Streams:
  local object rd_ch_buff_in (const object* stream_);
  local object rd_ch_buff_in(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      var uintL index = posfixnum_to_L(TheStream(stream)->strm_buff_in_index); # Index
      var uintL endindex = posfixnum_to_L(TheStream(stream)->strm_buff_in_endindex);
      loop
        { if (index < endindex) break; # noch was im aktuellen String?
          # String-Ende erreicht
          # fun aufrufen:
          funcall(TheStream(stream)->strm_buff_in_fun,0);
          if (!stringp(value1))
            { return eof_value; } # EOF erreicht
          # neuen String holen und Grenzen überprüfen:
          pushSTACK(value1); # String
          pushSTACK(mv_count >= 2 ? value2 : unbound); # start
          pushSTACK(mv_count >= 3 ? value3 : unbound); # end
         {var object string;
          var uintL start;
          var uintL len;
          subr_self = L(read_char);
          test_string_limits(&string,&start,&len);
          stream = *stream_;
          index = start;
          endindex = index+len;
          TheStream(stream)->strm_buff_in_string = string;
          TheStream(stream)->strm_buff_in_index = fixnum(index);
          TheStream(stream)->strm_buff_in_endindex = fixnum(endindex);
        }}
      # index < eofindex
      { var uintL len;
        var uintB* charptr = unpack_string(TheStream(stream)->strm_buff_in_string,&len);
        # Ab charptr kommen len Zeichen.
        if (index >= len) # Index zu groß ?
          { pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(TheStream(stream)->strm_buff_in_string);
            pushSTACK(stream);
            fehler(stream_error,
                   DEUTSCH ? "~ hinterm Stringende angelangt, weil String ~ adjustiert wurde." :
                   ENGLISH ? "~ is beyond the end because the string ~ has been adjusted" :
                   FRANCAIS ? "~ est arrivé après la fin de la chaîne, parce que la chaîne ~ a été ajustée." :
                   ""
                  );
          }
       {var object ch = code_char(charptr[index]); # Zeichen aus dem String holen
        # Index erhöhen:
        TheStream(stream)->strm_buff_in_index = fixnum_inc(TheStream(stream)->strm_buff_in_index,1);
        return ch;
      }}
    }

# Schließt einen Buffered-Input-Stream.
# close_buff_in(stream);
# > stream : Buffered-Input-Stream
  local void close_buff_in (object stream);
  local void close_buff_in(stream)
    var object stream;
    { TheStream(stream)->strm_buff_in_fun = NIL; # Funktion := NIL
      TheStream(stream)->strm_buff_in_mode = NIL; # Mode := NIL
      TheStream(stream)->strm_buff_in_string = NIL; # String := NIL
    }

# Stellt fest, ob ein Buffered-Input-Stream ein Zeichen verfügbar hat.
# listen_buff_in(stream)
# > stream : Buffered-Input-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_buff_in (object stream);
  local signean listen_buff_in(stream)
    var object stream;
    { var uintL index = posfixnum_to_L(TheStream(stream)->strm_buff_in_index); # Index
      var uintL endindex = posfixnum_to_L(TheStream(stream)->strm_buff_in_endindex);
      if (index < endindex) { return signean_null; }
     {var object mode = TheStream(stream)->strm_buff_in_mode;
      if (eq(mode,S(nil)))
        { pushSTACK(stream);
          mode = peek_char(&STACK_0); # peek_char macht read_char, ruft fun auf
          skipSTACK(1);
          if (eq(mode,eof_value))
            { return signean_minus; } # EOF erreicht
            else
            { return signean_null; }
        }
      elif (eq(mode,S(t)))
        { return signean_null; }
      else
        { funcall(mode,0); # mode aufrufen
          if (nullp(value1)) # keine Strings mehr zu erwarten?
            { return signean_minus; } # ja -> EOF erreicht
            else
            { return signean_null; }
        }
    }}

# UP: Löscht bereits eingegebenen interaktiven Input von einem Buffered-Input-Stream.
# clear_input_buff_in(stream)
# > stream: Buffered-Input-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# kann GC auslösen
  local boolean clear_input_buff_in (object stream);
  local boolean clear_input_buff_in(stream)
    var object stream;
    { # Bearbeitung des aktuellen Strings beenden:
      var object index = TheStream(stream)->strm_buff_in_index; # Index
      var object endindex = TheStream(stream)->strm_buff_in_endindex;
      TheStream(stream)->strm_buff_in_index = endindex; # index := endindex
      if (eq(index,endindex)) { return FALSE; } else { return TRUE; }
    }

LISPFUNN(make_buffered_input_stream,2)
# (MAKE-BUFFERED-INPUT-STREAM fun mode)
  { var object stream = # neuer Stream, nur READ-CHAR erlaubt
      allocate_stream(strmflags_rd_ch_B,strmtype_buff_in,strm_len+5,0);
    TheStream(stream)->strm_rd_by = P(rd_by_dummy);
    TheStream(stream)->strm_wr_by = P(wr_by_dummy);
    TheStream(stream)->strm_rd_ch = P(rd_ch_buff_in);
    TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
    TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
    TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
    TheStream(stream)->strm_buff_in_mode = popSTACK();
    TheStream(stream)->strm_buff_in_fun = popSTACK();
    TheStream(stream)->strm_buff_in_string = O(leer_string); # String := ""
    TheStream(stream)->strm_buff_in_index = Fixnum_0; # Index := 0
    TheStream(stream)->strm_buff_in_endindex = Fixnum_0; # Endindex := 0
    value1 = stream; mv_count=1; # stream als Wert
  }

LISPFUNN(buffered_input_stream_index,1)
# (SYSTEM::BUFFERED-INPUT-STREAM-INDEX buffered-input-stream) liefert den Index
  { var object stream = popSTACK(); # Argument
    # muss ein Buffered-Input-Stream sein:
    if (!(streamp(stream) && (TheStream(stream)->strmtype == strmtype_buff_in)))
      { pushSTACK(stream);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               DEUTSCH ? "~: ~ ist kein Buffered-Input-Stream." :
               ENGLISH ? "~: ~ is not a buffered input stream" :
               FRANCAIS ? "~ : ~ n'est pas un «stream» d'entrée bufferisé." :
               ""
              );
      }
   {var object index = TheStream(stream)->strm_buff_in_index;
    # Falls ein Character mit UNREAD-CHAR zurückgeschoben wurde,
    # verwende (1- index), ein Fixnum >=0, als Wert:
    if (TheStream(stream)->strmflags & strmflags_unread_B)
      { index = fixnum_inc(index,-1); }
    value1 = index; mv_count=1;
  }}


# Buffered-Output-Stream
# ======================

# Elementtyp: character
# Richtungen: nur output
# (make-buffered-output-stream fun) liefert einen solchen.
#   Dabei ist fun eine Funktion von einem Argument, die, mit einem
#   Simple-String als Argument aufgerufen, dessen Inhalt in Empfang nimmt.
#   Funktionsweise: (write-char ...) sammelt die geschriebenen Zeichen in
#   einem String, bis ein #\Newline oder eine FORCE-/FINISH-OUTPUT-
#   Anforderung kommt, und ruft dann fun mit einem Simple-String, der das
#   bisher Angesammelte enthält, als Argument auf.
#   (clear-output ...) wirft die bisher angesammelten Zeichen weg.

# Zusätzliche Komponenten:
  # define strm_buff_out_fun    strm_other[0]  # Ausgabefunktion
  #define strm_buff_out_string  strm_other[1]  # Semi-Simple-String für Output

# UP: Bringt den wartenden Output eines Buffered-Output-Stream ans Ziel.
# finish_output_buff_out(stream);
# > stream: Buffered-Output-Stream
# kann GC auslösen
  local void finish_output_buff_out (object stream);
  local void finish_output_buff_out(stream)
    var object stream;
    { pushSTACK(stream);
     {var object string = TheStream(stream)->strm_buff_out_string; # String
      string = coerce_ss(string); # in Simple-String umwandeln (erzwingt ein Kopieren)
      stream = STACK_0; STACK_0 = string;
      # String durch Fill-Pointer:=0 leeren:
      TheIarray(TheStream(stream)->strm_buff_out_string)->dims[1] = 0;
      funcall(TheStream(stream)->strm_buff_out_fun,1); # Funktion aufrufen
    }}

# UP: Bringt den wartenden Output eines Buffered-Output-Stream ans Ziel.
# force_output_buff_out(stream);
# > stream: Buffered-Output-Stream
# kann GC auslösen
  #define force_output_buff_out  finish_output_buff_out

# UP: Löscht den wartenden Output eines Buffered-Output-Stream.
# clear_output_buff_out(stream);
# > stream: Buffered-Output-Stream
# kann GC auslösen
  local void clear_output_buff_out (object stream);
  local void clear_output_buff_out(stream)
    var object stream;
    { # String durch Fill-Pointer:=0 leeren:
      TheIarray(TheStream(stream)->strm_buff_out_string)->dims[1] = 0;
      # Line-Position unverändert lassen??
    }

# WRITE-CHAR - Pseudofunktion für Buffered-Output-Streams:
  local void wr_ch_buff_out (const object* stream_, object ch);
  local void wr_ch_buff_out(stream_,ch)
    var const object* stream_;
    var object ch;
    { var object stream = *stream_;
      # obj sollte Character sein:
      if (!charp(ch)) { fehler_wr_char(stream,ch); }
      # Character in den String schieben:
      ssstring_push_extend(TheStream(stream)->strm_buff_out_string,char_code(ch));
      # Nach #\Newline den Buffer durchreichen:
      if (char_code(ch) == NL) { force_output_buff_out(*stream_); }
    }

# Schließt einen Buffered-Output-Stream.
# close_buff_out(stream);
# > stream : Buffered-Output-Stream
# kann GC auslösen
  local void close_buff_out (object stream);
  local void close_buff_out(stream)
    var object stream;
    { pushSTACK(stream); # stream retten
      finish_output_buff_out(stream);
      stream = popSTACK(); # stream zurück
      TheStream(stream)->strm_buff_out_fun = NIL; # Funktion := NIL
      TheStream(stream)->strm_buff_out_string = NIL; # String := NIL
    }

LISPFUN(make_buffered_output_stream,1,1,norest,nokey,0,NIL)
# (MAKE-BUFFERED-OUTPUT-STREAM fun [line-position])
  { # line-position überprüfen:
    if (eq(STACK_0,unbound))
      { STACK_0 = Fixnum_0; } # Defaultwert 0
      else
      # line-position angegeben, sollte ein Fixnum >=0 sein:
      { if (!posfixnump(STACK_0)) { fehler_posfixnum(STACK_0); } }
    # kleinen Semi-Simple-String der Länge 50 allozieren:
    pushSTACK(make_ssstring(50));
   {var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
      allocate_stream(strmflags_wr_ch_B,strmtype_buff_out,strm_len+2,0);
    TheStream(stream)->strm_rd_by = P(rd_by_dummy);
    TheStream(stream)->strm_wr_by = P(wr_by_dummy);
    TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
    TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_buff_out);
    TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
    TheStream(stream)->strm_buff_out_string = popSTACK(); # String eintragen
    TheStream(stream)->strm_wr_ch_lpos = popSTACK(); # Line Position eintragen
    TheStream(stream)->strm_buff_out_fun = popSTACK(); # Funktion eintragen
    value1 = stream; mv_count=1; # stream als Wert
  }}


#ifdef PRINTER_AMIGAOS

# Printer-Stream
# ==============

# Zusätzliche Komponenten:
  #define strm_printer_handle  strm_other[0]  # Handle von "PRT:"

# WRITE-CHAR - Pseudofunktion für Printer-Streams:
  local void wr_ch_printer (const object* stream_, object ch);
  local void wr_ch_printer(stream_,ch)
    var const object* stream_;
    var object ch;
    { var object stream = *stream_;
      # ch sollte Character sein:
      if (!charp(ch)) { fehler_wr_char(stream,ch); }
      begin_system_call();
     {var uintB c = char_code(ch);
      var long ergebnis = # Zeichen auszugeben versuchen
        Write(TheHandle(TheStream(stream)->strm_printer_handle),&c,1L);
      end_system_call();
      if (ergebnis<0) { OS_error(); } # Error melden
      # ergebnis = Anzahl der ausgegebenen Zeichen (0 oder 1)
      if (ergebnis==0) # nicht erfolgreich?
        { fehler_unwritable(S(write_char),stream); }
    }}

# Schließt einen Printer-Stream.
  local void close_printer (object stream);
  local void close_printer(stream)
    var object stream;
    { begin_system_call();
      Close(TheHandle(TheStream(stream)->strm_printer_handle));
      end_system_call();
    }

# UP: Liefert einen Printer-Stream.
# kann GC auslösen
  local object make_printer_stream (void);
  local object make_printer_stream()
    { pushSTACK(allocate_cons()); # Cons für Liste
      pushSTACK(allocate_handle(Handle_NULL)); # Handle-Verpackung
     {var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_printer,strm_len+1,0);
      set_break_sem_4();
      begin_system_call();
      {var Handle handle = Open("PRT:",MODE_NEWFILE);
       if (handle==Handle_NULL) { OS_error(); } # Error melden
       end_system_call();
       TheHandle(STACK_0) = handle; # Handle verpacken
      }
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_printer);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy_nogc);
      TheStream(stream)->strm_printer_handle = popSTACK();
      # Liste der offenen Streams um stream erweitern:
      {var object new_cons = popSTACK();
       Car(new_cons) = stream;
       Cdr(new_cons) = O(open_files);
       O(open_files) = new_cons;
      }
      clr_break_sem_4();
      return stream;
    }}

LISPFUNN(make_printer_stream,0)
# (SYSTEM::MAKE-PRINTER-STREAM) liefert einen Printer-Stream.
# Für die verstandenen Escape-Sequenzen siehe in PRINTER.DOC.
  { value1 = make_printer_stream(); mv_count=1; return; }

#endif


#ifdef PIPES

# Pipe-Input-Stream
# =================

# Zusätzliche Komponenten:
  # define strm_pipe_pid       strm_other[3] # Prozess-Id, ein Fixnum >=0
  #define strm_pipe_in_handle  strm_ihandle  # Handle für Input
  #if defined(EMUNIX) && defined(PIPES2)
  #define strm_pipe_in_other   strm_ohandle  # Pipe-Stream in Gegenrichtung
  #define strm_pipe_out_other  strm_ihandle  # Pipe-Stream in Gegenrichtung
  #endif

# READ-CHAR - Pseudofunktion für Pipe-Input-Streams:
  #define rd_ch_pipe_in  rd_ch_handle

# READ-BYTE - Pseudofunktion für Pipe-Input-Streams:
  #define rd_by_pipe_in  rd_by_handle

# Schließt einen Pipe-Input-Stream.
# close_pipe_in(stream);
# > stream : Pipe-Input-Stream
  #ifdef EMUNIX
    local void close_pipe_in (object stream);
    local void close_pipe_in(stream)
      var object stream;
      { var Handle handle = TheHandle(TheStream(stream)->strm_pipe_in_handle);
        #ifdef PIPES2
        if (streamp(TheStream(stream)->strm_pipe_in_other))
          # Der andere Pipe-Stream ist noch offen. Wir dürfen nicht pclose()
          # aufrufen, da das ein waitpid() ausführt.
          { TheStream(TheStream(stream)->strm_pipe_in_other)->strm_pipe_out_other = NIL;
            TheStream(stream)->strm_pipe_in_other = NIL;
            begin_system_call();
            if ( fclose(&_streamv[handle]) != 0) { OS_error(); }
            end_system_call();
            # Die Pipes sind nun getrennt, so dass beim Schließen der anderen
            # Pipe das pclose() ausgeführt werden wird.
            return;
          }
        #endif
        begin_system_call();
        if ( pclose(&_streamv[handle]) == -1) { OS_error(); }
        end_system_call();
      }
  #endif
  #if defined(UNIX) || defined(WIN32_NATIVE)
    #define close_pipe_in  close_ihandle
  #endif

# Stellt fest, ob ein Pipe-Input-Stream ein Zeichen verfügbar hat.
# listen_pipe_in(stream)
# > stream : Pipe-Input-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  #define listen_pipe_in  listen_handle

LISPFUNN(make_pipe_input_stream,1)
# (MAKE-PIPE-INPUT-STREAM command)
# ruft eine Shell auf, die command ausführt, wobei deren Standard-Output
# in unsere Pipe hinein geht.
  { # command überprüfen:
    var object command;
    funcall(L(string),1); # (STRING command)
    command = string_to_asciz(value1); # als ASCIZ-String
   {var int child;
    #ifdef EMUNIX
    var int handles[2];
    { begin_system_call();
     {var FILE* f = popen(TheAsciz(command),"r");
      if (f==NULL) { OS_error(); }
      child = f->_pid;
      handles[0] = fileno(f);
      end_system_call();
    }}
    #endif
    #ifdef UNIX
    var int handles[2]; # zwei Handles für die Pipe
    { # Als Shell nehmen wir immer die Kommando-Shell.
      # command in den Stack kopieren:
      var uintL command_length = Sstring_length(command);
      var DYNAMIC_ARRAY(command_data,char,command_length);
      { var const char* ptr1 = TheAsciz(command);
        var char* ptr2 = &command_data[0];
        dotimespL(command_length,command_length, { *ptr2++ = *ptr1++; } );
      }
      begin_system_call();
      # Pipe aufbauen:
      if (!( pipe(handles) ==0))
        { FREE_DYNAMIC_ARRAY(command_data); OS_error(); }
      # Alles, was in handles[1] reingeschoben wird, kommt bei handles[0]
      # wieder raus. Wir werden dies so benutzen:
      #
      #       write            system            read
      # child  ->   handles[1]   ->   handles[0]  ->  parent
      #
      # einen neuen Prozess starten:
      if ((child = vfork()) ==0)
        # Dieses Programmstück wird vom Child-Prozess ausgeführt:
        { if ( dup2(handles[1],stdout_handle) >=0) # Standard-Output umleiten
            if ( CLOSE(handles[1]) ==0) # Wir wollen nur über stdout_handle schreiben
              if ( CLOSE(handles[0]) ==0) # Wir wollen von der Pipe nicht lesen
                # (Muss das dem Betriebssystem sagen, damit - wenn der Child
                # die Pipe gefüllt hat - der Parent-Prozess und nicht etwa der
                # Child-Prozess aufgerufen wird, um die Pipe zu leeren.)
                { # Child-Prozess zum Hintergrundprozess machen:
                  SETSID(); # er bekommt eine eigene Process Group
                  execl(SHELL,            # Shell aufrufen
                        SHELL,            # =: argv[0]
                        "-c",             # =: argv[1]
                        &command_data[0], # =: argv[2]
                        NULL
                       );
                }
          _exit(-1); # sollte dies misslingen, Child-Prozess beenden
        }
      # Dieses Programmstück wird wieder vom Aufrufer ausgeführt:
      if (child==-1)
        # Etwas ist misslungen, entweder beim vfork oder beim execl.
        # In beiden Fällen wurde errno gesetzt.
        { var int saved_errno = errno;
          CLOSE(handles[1]); CLOSE(handles[0]);
          FREE_DYNAMIC_ARRAY(command_data);
          errno = saved_errno; OS_error();
        }
      # Wir wollen von der Pipe nur lesen, nicht schreiben:
      if (!( CLOSE(handles[1]) ==0))
        { var int saved_errno = errno;
          CLOSE(handles[0]);
          FREE_DYNAMIC_ARRAY(command_data);
          errno = saved_errno; OS_error();
        }
      # (Muss das dem Betriebssystem sagen, damit - wenn der Parent-Prozess
      # die Pipe geleert hat - der Child-Prozess und nicht etwa der
      # Parent-Prozess aufgerufen wird, um die Pipe wieder zu füllen.)
      end_system_call();
      FREE_DYNAMIC_ARRAY(command_data);
    }
    #endif
    #ifdef WIN32_NATIVE
    var Handle handles[2]; # zwei Handles für die Pipe
    begin_system_call();
    { var Handle child_write_handle;
      # Create a pipe and make one of the two handles inheritable.
      if (!CreatePipe(&handles[0],&handles[1],NULL,0)) { OS_error(); }
      if (!DuplicateHandle(GetCurrentProcess(),handles[1],
                           GetCurrentProcess(),&child_write_handle,
                           0, TRUE, DUPLICATE_SAME_ACCESS))
        { OS_error(); }
      if (!CloseHandle(handles[1])) { OS_error(); }
     {var HANDLE stdinput;
      var PROCESS_INFORMATION pinfo;
      stdinput = GetStdHandle(STD_INPUT_HANDLE);
      if (stdinput == INVALID_HANDLE_VALUE) { OS_error(); }
      if (!MyCreateProcess(TheAsciz(command),stdinput,child_write_handle,&pinfo))
        { OS_error(); }
      # Close our copy of the child's handle, so that the OS knows
      # that we won't write on it.
      if (!CloseHandle(child_write_handle)) { OS_error(); }
      if (!CloseHandle(pinfo.hThread)) { OS_error(); }
      if (!CloseHandle(pinfo.hProcess)) { OS_error(); }
      child = pinfo.dwProcessId;
    }}
    end_system_call();
    #endif
    pushSTACK(allocate_handle(handles[0]));
    pushSTACK(UL_to_I(child));
    # Stream allozieren:
    { var object stream = # neuer Stream, nur READ-CHAR und READ-BYTE erlaubt
        allocate_stream(strmflags_rd_B,strmtype_pipe_in,strm_len+4,0);
      TheStream(stream)->strm_rd_by = P(rd_by_pipe_in);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_pipe_in);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
      TheStream(stream)->strm_pipe_pid = popSTACK(); # Child-Pid
      TheStream(stream)->strm_pipe_in_handle = popSTACK(); # Read-Handle
      TheStream(stream)->strm_isatty = NIL;
      value1 = stream; mv_count=1; # stream als Wert
  }}}


# Pipe-Output-Stream
# ==================

# Zusätzliche Komponenten:
  # define strm_pipe_pid          strm_other[3] # Prozess-Id, ein Fixnum >=0
  #define strm_pipe_out_handle    strm_ohandle  # Handle für Output
  #if defined(EMUNIX) && defined(PIPES2)
  # define strm_pipe_out_other    strm_ihandle  # Pipe-Stream in Gegenrichtung
  #endif

# WRITE-CHAR - Pseudofunktion für Pipe-Output-Streams:
  #define wr_ch_pipe_out  wr_ch_handle_x

# WRITE-SIMPLE-STRING - Pseudofunktion für Pipe-Output-Streams:
  #define wr_ss_pipe_out  wr_ss_handle_x

# WRITE-BYTE - Pseudofunktion für Pipe-Output-Streams:
  #define wr_by_pipe_out  wr_by_handle

# Schließt einen Pipe-Output-Stream.
# close_pipe_out(stream);
# > stream : Pipe-Output-Stream
  #ifdef EMUNIX
    local void close_pipe_out (object stream);
    local void close_pipe_out(stream)
      var object stream;
      { var Handle handle = TheHandle(TheStream(stream)->strm_pipe_out_handle);
        #ifdef PIPES2
        if (streamp(TheStream(stream)->strm_pipe_out_other))
          # Der andere Pipe-Stream ist noch offen. Wir dürfen nicht pclose()
          # aufrufen, da das ein waitpid() ausführt.
          { TheStream(TheStream(stream)->strm_pipe_out_other)->strm_pipe_in_other = NIL;
            TheStream(stream)->strm_pipe_out_other = NIL;
            begin_system_call();
            if ( fclose(&_streamv[handle]) != 0) { OS_error(); }
            end_system_call();
            # Die Pipes sind nun getrennt, so dass beim Schließen der anderen
            # Pipe das pclose() ausgeführt werden wird.
            return;
          }
        #endif
        begin_system_call();
        if ( pclose(&_streamv[handle]) == -1) { OS_error(); }
        end_system_call();
      }
  #endif
  #if defined(UNIX) || defined(WIN32_NATIVE)
    #define close_pipe_out  close_ohandle
  #endif

LISPFUNN(make_pipe_output_stream,1)
# (MAKE-PIPE-OUTPUT-STREAM command)
# ruft eine Shell auf, die command ausführt, wobei unsere Pipe in deren
# Standard-Input hinein geht.
  { # command überprüfen:
    var object command;
    funcall(L(string),1); # (STRING command)
    command = string_to_asciz(value1); # als ASCIZ-String
   {var int child;
    #ifdef EMUNIX
    var int handles[2];
    { begin_system_call();
     {var FILE* f = popen(TheAsciz(command),"w");
      if (f==NULL) { OS_error(); }
      child = f->_pid;
      handles[1] = fileno(f);
      end_system_call();
    }}
    #endif
    #ifdef UNIX
    var int handles[2]; # zwei Handles für die Pipe
    { # Als Shell nehmen wir immer die Kommando-Shell.
      # command in den Stack kopieren:
      var uintL command_length = Sstring_length(command);
      var DYNAMIC_ARRAY(command_data,char,command_length);
      { var const char* ptr1 = TheAsciz(command);
        var char* ptr2 = &command_data[0];
        dotimespL(command_length,command_length, { *ptr2++ = *ptr1++; } );
      }
      begin_system_call();
      if (!( pipe(handles) ==0))
        { FREE_DYNAMIC_ARRAY(command_data); OS_error(); }
      # Alles, was in handles[1] reingeschoben wird, kommt bei handles[0]
      # wieder raus. Wir werden dies so benutzen:
      #
      #        write            system            read
      # parent  ->   handles[1]   ->   handles[0]  ->  child
      #
      # einen neuen Prozess starten:
      if ((child = vfork()) ==0)
        # Dieses Programmstück wird vom Child-Prozess ausgeführt:
        { if ( dup2(handles[0],stdin_handle) >=0) # Standard-Input umleiten
            if ( CLOSE(handles[0]) ==0) # Wir wollen nur über stdin_handle lesen
              if ( CLOSE(handles[1]) ==0) # Wir wollen auf die Pipe nicht schreiben
                # (Muss das dem Betriebssystem sagen, damit - wenn der Child
                # die Pipe geleert hat - der Parent-Prozess und nicht etwa der
                # Child-Prozess aufgerufen wird, um die Pipe zu wieder zu füllen.)
                { # Child-Prozess zum Hintergrundprozess machen:
                  SETSID(); # er bekommt eine eigene Process Group
                  execl(SHELL,            # Shell aufrufen
                        SHELL,            # =: argv[0]
                        "-c",             # =: argv[1]
                        &command_data[0], # =: argv[2]
                        NULL
                       );
                }
          _exit(-1); # sollte dies misslingen, Child-Prozess beenden
        }
      # Dieses Programmstück wird wieder vom Aufrufer ausgeführt:
      if (child==-1)
        # Etwas ist misslungen, entweder beim vfork oder beim execl.
        # In beiden Fällen wurde errno gesetzt.
        { var int saved_errno = errno;
          CLOSE(handles[1]); CLOSE(handles[0]);
          FREE_DYNAMIC_ARRAY(command_data);
          errno = saved_errno; OS_error();
        }
      # Wir wollen auf die Pipe nur schreiben, nicht lesen:
      if (!( CLOSE(handles[0]) ==0))
        { var int saved_errno = errno;
          CLOSE(handles[1]);
          FREE_DYNAMIC_ARRAY(command_data);
          errno = saved_errno; OS_error();
        }
      # (Muss das dem Betriebssystem sagen, damit - wenn der Parent-Prozess
      # die Pipe gefüllt hat - der Child-Prozess und nicht etwa der
      # Parent-Prozess aufgerufen wird, um die Pipe wieder zu leeren.)
      end_system_call();
      FREE_DYNAMIC_ARRAY(command_data);
    }
    #endif
    #ifdef WIN32_NATIVE
    var Handle handles[2]; # zwei Handles für die Pipe
    begin_system_call();
    { var Handle child_read_handle;
      # Create a pipe and make one of the two handles inheritable.
      if (!CreatePipe(&handles[0],&handles[1],NULL,0)) { OS_error(); }
      if (!DuplicateHandle(GetCurrentProcess(),handles[0],
                           GetCurrentProcess(),&child_read_handle,
                           0, TRUE, DUPLICATE_SAME_ACCESS))
        { OS_error(); }
      if (!CloseHandle(handles[0])) { OS_error(); }
     {var HANDLE stdoutput;
      var PROCESS_INFORMATION pinfo;
      stdoutput = GetStdHandle(STD_OUTPUT_HANDLE);
      if (stdoutput == INVALID_HANDLE_VALUE) { OS_error(); }
      if (!MyCreateProcess(TheAsciz(command),child_read_handle,stdoutput,&pinfo))
        { OS_error(); }
      # Close our copy of the child's handle, so that the OS knows
      # that we won't read from it.
      if (!CloseHandle(child_read_handle)) { OS_error(); }
      if (!CloseHandle(pinfo.hThread)) { OS_error(); }
      if (!CloseHandle(pinfo.hProcess)) { OS_error(); }
      child = pinfo.dwProcessId;
    }}
    end_system_call();
    #endif
    pushSTACK(allocate_handle(handles[1]));
    pushSTACK(UL_to_I(child));
    # Stream allozieren:
    { var object stream = # neuer Stream, nur WRITE-CHAR und WRITE-BYTE erlaubt
        allocate_stream(strmflags_wr_B,strmtype_pipe_out,strm_len+4,0);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_pipe_out);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_pipe_out);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_pipe_out);
      TheStream(stream)->strm_pipe_pid = popSTACK(); # Child-Pid
      TheStream(stream)->strm_pipe_out_handle = popSTACK(); # Write-Handle
      value1 = stream; mv_count=1; # stream als Wert
  }}}

#ifdef PIPES2

# Bidirektionale Pipes
# ====================

LISPFUNN(make_pipe_io_stream,1)
# (MAKE-PIPE-IO-STREAM command)
# ruft eine Shell auf, die command ausführt, wobei der Output unserer Pipe
# in deren Standard-Input hinein und deren Standard-Output wiederum in
# unsere Pipe hinein geht.
  { # command überprüfen:
    var object command;
    funcall(L(string),1); # (STRING command)
    command = string_to_asciz(value1); # als ASCIZ-String
   {var int child;
    #ifdef EMUNIX
    var int in_handles[2];
    var int out_handles[2];
    { # Stackaufbau: command.
      var FILE* f_in;
      var FILE* f_out;
      begin_system_call();
      if (popenrw(TheAsciz(command),&f_in,&f_out) <0) { OS_error(); }
      child = f_in->_pid; # = f_out->_pid;
      in_handles[0] = fileno(f_in);
      out_handles[1] = fileno(f_out);
    }
    #endif
    #ifdef UNIX
    var int in_handles[2]; # zwei Handles für die Pipe zum Input-Stream
    var int out_handles[2]; # zwei Handles für die Pipe zum Output-Stream
    { # Als Shell nehmen wir immer die Kommando-Shell.
      # command in den Stack kopieren:
      var uintL command_length = Sstring_length(command);
      var DYNAMIC_ARRAY(command_data,char,command_length);
      { var const char* ptr1 = TheAsciz(command);
        var char* ptr2 = &command_data[0];
        dotimespL(command_length,command_length, { *ptr2++ = *ptr1++; } );
      }
      begin_system_call();
      # Pipes aufbauen:
      if (!( pipe(in_handles) ==0))
        { FREE_DYNAMIC_ARRAY(command_data); OS_error(); }
      if (!( pipe(out_handles) ==0))
        { var int saved_errno = errno;
          CLOSE(in_handles[1]); CLOSE(in_handles[0]);
          FREE_DYNAMIC_ARRAY(command_data);
          errno = saved_errno; OS_error();
        }
      # Alles, was in handles[1] reingeschoben wird, kommt bei handles[0]
      # wieder raus. Wir werden dies so benutzen:
      #
      #        write                system                read
      # parent  ->   out_handles[1]   ->   out_handles[0]  ->   child
      # parent  <-   in_handles[0]    <-   in_handles[1]   <-   child
      #        read                 system                write
      #
      # einen neuen Prozess starten:
      if ((child = vfork()) ==0)
        # Dieses Programmstück wird vom Child-Prozess ausgeführt:
        { if ( dup2(out_handles[0],stdin_handle) >=0) # Standard-Input umleiten
            if ( dup2(in_handles[1],stdout_handle) >=0) # Standard-Output umleiten
              if ( CLOSE(out_handles[0]) ==0) # Wir wollen nur über stdin_handle lesen
                if ( CLOSE(in_handles[1]) ==0) # Wir wollen nur über stdout_handle schreiben
                  if ( CLOSE(out_handles[1]) ==0) # Wir wollen auf die Pipe nicht schreiben
                    # (Muss das dem Betriebssystem sagen, damit - wenn der Child
                    # die Pipe geleert hat - der Parent-Prozess und nicht etwa der
                    # Child-Prozess aufgerufen wird, um die Pipe zu wieder zu füllen.)
                    if ( CLOSE(in_handles[0]) ==0) # Wir wollen von der Pipe nicht lesen
                      # (Muss das dem Betriebssystem sagen, damit - wenn der Child
                      # die Pipe gefüllt hat - der Parent-Prozess und nicht etwa der
                      # Child-Prozess aufgerufen wird, um die Pipe zu leeren.)
                      { # Child-Prozess zum Hintergrundprozess machen:
                        SETSID(); # er bekommt eine eigene Process Group
                        execl(SHELL,            # Shell aufrufen
                              SHELL,            # =: argv[0]
                              "-c",             # =: argv[1]
                              &command_data[0], # =: argv[2]
                              NULL
                             );
                      }
          _exit(-1); # sollte dies misslingen, Child-Prozess beenden
        }
      # Dieses Programmstück wird wieder vom Aufrufer ausgeführt:
      if (child==-1)
        # Etwas ist misslungen, entweder beim vfork oder beim execl.
        # In beiden Fällen wurde errno gesetzt.
        { var int saved_errno = errno;
          CLOSE(in_handles[1]); CLOSE(in_handles[0]);
          CLOSE(out_handles[1]); CLOSE(out_handles[0]);
          FREE_DYNAMIC_ARRAY(command_data);
          errno = saved_errno; OS_error();
        }
      # Wir wollen auf die Pipe nur schreiben, nicht lesen:
      if (!( CLOSE(out_handles[0]) ==0))
        { var int saved_errno = errno;
          CLOSE(in_handles[1]); CLOSE(in_handles[0]);
          CLOSE(out_handles[1]);
          FREE_DYNAMIC_ARRAY(command_data);
          errno = saved_errno; OS_error();
        }
      # (Muss das dem Betriebssystem sagen, damit - wenn der Parent-Prozess
      # die Pipe gefüllt hat - der Child-Prozess und nicht etwa der
      # Parent-Prozess aufgerufen wird, um die Pipe wieder zu leeren.)
      # Wir wollen von der Pipe nur lesen, nicht schreiben:
      if (!( CLOSE(in_handles[1]) ==0))
        { var int saved_errno = errno;
          CLOSE(in_handles[0]);
          CLOSE(out_handles[1]);
          FREE_DYNAMIC_ARRAY(command_data);
          errno = saved_errno; OS_error();
        }
      # (Muss das dem Betriebssystem sagen, damit - wenn der Parent-Prozess
      # die Pipe geleert hat - der Child-Prozess und nicht etwa der
      # Parent-Prozess aufgerufen wird, um die Pipe wieder zu füllen.)
      end_system_call();
      FREE_DYNAMIC_ARRAY(command_data);
    }
    #endif
    #ifdef WIN32_NATIVE
    var Handle in_handles[2]; # zwei Handles für die Pipe zum Input-Stream
    var Handle out_handles[2]; # zwei Handles für die Pipe zum Output-Stream
    begin_system_call();
    { var Handle child_read_handle;
      var Handle child_write_handle;
      # Create two pipes and make two of the four handles inheritable.
      if (!CreatePipe(&in_handles[0],&in_handles[1],NULL,0)) { OS_error(); }
      if (!DuplicateHandle(GetCurrentProcess(),in_handles[1],
                           GetCurrentProcess(),&child_write_handle,
                           0, TRUE, DUPLICATE_SAME_ACCESS))
        { OS_error(); }
      if (!CloseHandle(in_handles[1])) { OS_error(); }
      if (!CreatePipe(&out_handles[0],&out_handles[1],NULL,0)) { OS_error(); }
      if (!DuplicateHandle(GetCurrentProcess(),out_handles[0],
                           GetCurrentProcess(),&child_read_handle,
                           0, TRUE, DUPLICATE_SAME_ACCESS))
        { OS_error(); }
      if (!CloseHandle(out_handles[0])) { OS_error(); }
     {var PROCESS_INFORMATION pinfo;
      if (!MyCreateProcess(TheAsciz(command),child_read_handle,child_write_handle,&pinfo))
        { OS_error(); }
      # Close our copies of the child's handles, so that the OS knows
      # that we won't use them.
      if (!CloseHandle(child_read_handle)) { OS_error(); }
      if (!CloseHandle(child_write_handle)) { OS_error(); }
      if (!CloseHandle(pinfo.hThread)) { OS_error(); }
      if (!CloseHandle(pinfo.hProcess)) { OS_error(); }
      child = pinfo.dwProcessId;
    }}
    end_system_call();
    #endif
    pushSTACK(UL_to_I(child));
    pushSTACK(allocate_handle(in_handles[0]));
    pushSTACK(allocate_handle(out_handles[1]));
    # Input-Stream allozieren:
    { var object stream = # neuer Stream, nur READ-CHAR erlaubt
        allocate_stream(strmflags_rd_ch_B,strmtype_pipe_in,strm_len+4,0);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_pipe_in);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
      TheStream(stream)->strm_pipe_pid = STACK_2; # Child-Pid
      TheStream(stream)->strm_pipe_in_handle = STACK_1; # Read-Handle
      TheStream(stream)->strm_isatty = NIL;
      STACK_1 = stream;
    }
    # Output-Stream allozieren:
    { var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_pipe_out,strm_len+4,0);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_pipe_out);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_wr_ss = P(wr_ss_pipe_out);
      TheStream(stream)->strm_pipe_pid = STACK_2; # Child-Pid
      TheStream(stream)->strm_pipe_out_handle = STACK_0; # Write-Handle
      STACK_0 = stream;
    }
    #ifdef EMUNIX
    # Beide Pipes miteinander verknüpfen, zum reibungslosen close:
    TheStream(STACK_1)->strm_pipe_in_other = STACK_0;
    TheStream(STACK_0)->strm_pipe_out_other = STACK_1;
    #endif
    # 3 Werte:
    # (make-two-way-stream input-stream output-stream), input-stream, output-stream.
    STACK_2 = make_twoway_stream(STACK_1,STACK_0);
    funcall(L(values),3);
  }}

#endif # PIPES2

#endif # PIPES


#if defined(X11SOCKETS) || defined(SOCKET_STREAMS)

# X11-Socket-Stream, Socket-Stream
# ================================

# READ-CHAR - Pseudofunktion für Socket-Streams:
  #ifdef WIN32_NATIVE
    local object rd_ch_socket (const object* stream_);
    local object rd_ch_socket(stream_)
      var const object* stream_;
      {   restart_it:
       {  var object stream = *stream_;
          if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF?
            { return eof_value; }
        { var SOCKET handle = TheSocket(TheStream(stream)->strm_ihandle);
          var uintB c;
          begin_system_call();
         {var int ergebnis = sock_read(handle,&c,1); # Zeichen lesen versuchen
          if (ergebnis<0)
            { if (WSAGetLastError()==WSAEINTR) # Unterbrechung durch Ctrl-C ?
                { end_system_call();
                  pushSTACK(S(read_char)); tast_break(); # Break-Schleife aufrufen
                  goto restart_it;
                }
              SOCK_error();
            }
          end_system_call();
          if (ergebnis==0)
            # kein Zeichen verfügbar -> muss EOF sein
            { return eof_value; }
            else
            { return code_char(c); }
      }}}}
  #else
    #define rd_ch_socket  rd_ch_handle
  #endif

# Stellt fest, ob ein Socket-Stream ein Zeichen verfügbar hat.
# listen_socket(stream)
# > stream : Socket-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  #ifdef WIN32_NATIVE
    local signean listen_socket (object stream);
    local signean listen_socket(stream)
      var object stream;
      { restart_it:
        if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
          { return signean_minus; }
       {var SOCKET handle = TheSocket(TheStream(stream)->strm_ihandle);
        # Verwende select mit readfds = einelementige Menge {handle}
        # und timeout = Null-Zeitintervall.
        var fd_set handle_menge; # Menge von Handles := {handle}
        var struct timeval zero_time; # Zeitintervall := 0
        var int ergebnis;
        begin_system_call();
        FD_ZERO(&handle_menge); FD_SET(handle,&handle_menge);
        zero_time.tv_sec = 0; zero_time.tv_usec = 0;
        ergebnis = select(FD_SETSIZE,&handle_menge,NULL,NULL,&zero_time);
        if (ergebnis<0)
          { if (WSAGetLastError()==WSAEINTR) # Unterbrechung durch Ctrl-C ?
              { end_system_call();
                pushSTACK(stream);
                pushSTACK(S(read_char)); tast_break(); # Break-Schleife aufrufen
                stream = popSTACK();
                goto restart_it;
              }
            SOCK_error();
          }
          else
          { # ergebnis = Anzahl der Handles in handle_menge, bei denen read
            # sofort ein Ergebnis liefern würde.
            if (ergebnis==0)
              { end_system_call(); return signean_plus; } # kein Zeichen verfügbar
            # ergebnis=1
            # Wenn read() sofort ein Ergebnis liefern würde, kann das auch EOF
            # sein!
            # ein Zeichen lesen versuchen (wie bei peek_char):
           {var uintB c;
            var int ergebnis = sock_read(handle,&c,1); # Zeichen lesen versuchen
            if (ergebnis<0)
              { if (WSAGetLastError()==WSAEINTR) # Unterbrechung durch Ctrl-C ?
                  { end_system_call();
                    pushSTACK(stream);
                    pushSTACK(S(read_char)); tast_break(); # Break-Schleife aufrufen
                    stream = popSTACK();
                    goto restart_it;
                  }
                SOCK_error();
              }
            end_system_call();
            if (ergebnis==0)
              # kein Zeichen verfügbar -> EOF erkennen
              { TheStream(stream)->strm_rd_ch_last = eof_value;
                return signean_minus;
              }
              else # Zeichen verfügbar
              { TheStream(stream)->strm_rd_ch_last = code_char(c);
                TheStream(stream)->strmflags |= strmflags_unread_B;
                return signean_null;
              }
          }}
      }}
  #else
    #define listen_socket  listen_handle
  #endif

# WRITE-CHAR - Pseudofunktion für Socket-Streams:
  #ifdef WIN32_NATIVE
    local void wr_ch_socket (const object* stream_, object ch);
    local void wr_ch_socket(stream_,ch)
      var const object* stream_;
      var object ch;
      { restart_it:
       {  var SOCKET handle = TheSocket(TheStream(*stream_)->strm_ohandle);
          # ch sollte Character sein:
          if (!charp(ch)) { fehler_wr_char(*stream_,ch); }
        { var uintB c = char_code(ch); # Code des Zeichens
          begin_system_call();
         {var int ergebnis = sock_write(handle,&c,1); # Zeichen auszugeben versuchen
          if (ergebnis<0)
            { if (WSAGetLastError()==WSAEINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
                { end_system_call();
                  interruptp({ pushSTACK(S(write_char)); tast_break(); }); # Break-Schleife aufrufen
                  goto restart_it;
                }
              SOCK_error(); # Error melden
            }
          end_system_call();
          if (ergebnis==0) # nicht erfolgreich?
            { fehler_unwritable(S(write_char),*stream_); }
      }}}}
  #else
    #define wr_ch_socket  wr_ch_handle
  #endif

# WRITE-CHAR-SEQUENCE für X11-Socket-Streams:
  #ifdef WIN32_NATIVE
    local const uintB* write_char_array_socket (object stream, const uintB* ptr, uintL len);
    local const uintB* write_char_array_socket(stream,ptr,len)
      var object stream;
      var const uintB* ptr;
      var uintL len;
      { var SOCKET handle = TheSocket(TheStream(stream)->strm_ohandle);
        var uintL remaining = len;
        begin_system_call();
        # This loop is probably not needed, because sock_write() returns
        # only when done or EOF. Keep it nevertheless.
        loop
          { var int ergebnis = sock_write(handle,ptr,remaining); # Zeichen auszugeben versuchen
            if (ergebnis<0) { SOCK_error(); } # Error melden
            if (ergebnis==0) # nicht erfolgreich?
              { end_system_call(); fehler_unwritable(S(write_string),stream); }
            ptr += ergebnis; remaining -= ergebnis;
            if (remaining==0) break; # fertig?
          }
        end_system_call();
        wr_ss_lpos(stream,ptr,len); # Line-Position aktualisieren
        return ptr;
      }
  #endif

# WRITE-SIMPLE-STRING - Pseudofunktion für Socket-Streams:
  #ifdef WIN32_NATIVE
    local void wr_ss_socket (const object* stream_, object string, uintL start, uintL len);
    local void wr_ss_socket(stream_,string,start,len)
      var const object* stream_;
      var object string;
      var uintL start;
      var uintL len;
      { if (len==0) return;
        write_char_array_socket(*stream_,&TheSstring(string)->data[start],len);
      }
  #else
    #define wr_ss_socket  wr_ss_handle
  #endif

# READ-BYTE - Pseudofunktion für Socket-Streams:
  #ifdef WIN32_NATIVE
    local object rd_by_socket (object stream);
    # This code is identical to rd_by_handle, but nevertheless a distinct
    # function, so that function pointers can be compared.
    local object rd_by_socket(stream)
      var object stream;
      { pushSTACK(stream);
       {var object obj = read_char(&STACK_0);
        skipSTACK(1);
        if (!eq(obj,eof_value)) { obj = char_to_fixnum(obj); }
        return obj;
      }}
  #else
    #define rd_by_socket  rd_by_handle
  #endif

# WRITE-BYTE - Pseudofunktion für Socket-Streams:
  #ifdef WIN32_NATIVE
    local void wr_by_socket (object stream, object obj);
    local void wr_by_socket(stream,obj)
      var object stream;
      var object obj;
      { # obj überprüfen:
        if (!integerp(obj)) { fehler_wr_integer(stream,obj); }
        if (!(posfixnump(obj) && (posfixnum_to_L(obj) < char_code_limit)))
          { fehler_bad_integer(stream,obj); }
        pushSTACK(stream);
        wr_ch_socket(&STACK_0,fixnum_to_char(obj));
        skipSTACK(1);
      }
  #else
    #define wr_by_socket  wr_by_handle
  #endif

# Schließt einen Socket-Stream.
# close_socket(stream);
# > stream : Socket-Stream
  #ifdef WIN32_NATIVE
    local void close_socket (object stream);
    local void close_socket(stream)
      var object stream;
      { var SOCKET handle = TheSocket(TheStream(stream)->strm_ihandle);
        begin_system_call();
        if (!( closesocket(handle) ==0)) { SOCK_error(); }
        end_system_call();
      }
  #else
    #define close_socket  close_ihandle
  #endif

#endif


#ifdef X11SOCKETS

# X11-Socket-Stream
# =================

# Verwendung: für X-Windows.

# Zusätzliche Komponenten:
  # define strm_x11socket_connect strm_other[3] # Liste (host display)

#define rd_ch_x11socket  rd_ch_socket
#define listen_x11socket  listen_socket
#define wr_ch_x11socket  wr_ch_socket
#define wr_ss_x11socket  wr_ss_socket
#define rd_by_x11socket  rd_by_socket
#define wr_by_x11socket  wr_by_socket
#define close_x11socket  close_socket

extern SOCKET connect_to_x_server (const char* host, int display); # ein Stück X-Source...

LISPFUNN(make_x11socket_stream,2)
# (SYS::MAKE-SOCKET-STREAM host display)
# liefert einen X11-Socket-Stream für X-Windows oder NIL.
  { if (!stringp(STACK_1))
      { pushSTACK(STACK_1); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(string)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(STACK_(1+2));
        fehler(type_error,
               DEUTSCH ? "Host muss ein String sein, nicht ~" :
               ENGLISH ? "host should be string, not ~" :
               FRANCAIS ? "L'hôte devrait être un chaîne et non ~" :
               ""
              );
      }
    if (!posfixnump(STACK_0))
      { pushSTACK(STACK_0); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_posfixnum)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(STACK_(0+2));
        fehler(type_error,
               DEUTSCH ? "Display sollte ein Fixnum >=0 sein, nicht ~" :
               ENGLISH ? "display should be a nonnegative fixnum, not ~" :
               FRANCAIS ? "Le «display» doit être de type FIXNUM >= 0 et non ~" :
               ""
              );
     }
   {var const char* host = TheAsciz(string_to_asciz(STACK_1));
    var SOCKET handle;
    begin_system_call();
    handle = connect_to_x_server(host,posfixnum_to_L(STACK_0));
    end_system_call();
    if (handle == INVALID_SOCKET) { SOCK_error(); }
    # Liste bilden:
    {var object list = listof(2); pushSTACK(list); }
    pushSTACK(allocate_socket(handle));
    # Stream allozieren:
    {var object stream = # neuer Stream, alles erlaubt
       allocate_stream(strmflags_open_B,strmtype_x11socket,strm_len+4,0);
     TheStream(stream)->strm_rd_by = P(rd_by_x11socket);
     TheStream(stream)->strm_wr_by = P(wr_by_x11socket);
     TheStream(stream)->strm_rd_ch = P(rd_ch_x11socket);
     TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
     TheStream(stream)->strm_rd_ch_last = NIL;
     TheStream(stream)->strm_wr_ch = P(wr_ch_x11socket);
     TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
     TheStream(stream)->strm_wr_ss = P(wr_ss_x11socket);
     TheStream(stream)->strm_ihandle =
     TheStream(stream)->strm_ohandle = popSTACK(); # Handle eintragen
     TheStream(stream)->strm_x11socket_connect = popSTACK(); # zweielementige Liste
     TheStream(stream)->strm_isatty = NIL;
     value1 = stream; mv_count=1; # stream als Wert
  }}}

# Die beiden folgenden Funktionen sollten
# 1. nicht nur auf Handle- und Socket-Streams, sondern auch auf Synonym-
#    und Concatenated-Streams funktionieren, idealerweise auch auf File-Streams.
# 2. das rd_ch_lastchar ebenso verändern wie READ-BYTE.
# 3. auch nicht-simple Byte-Vektoren akzeptieren.
# Für CLX reicht aber die vorliegende Implementation.

# (SYS::READ-N-BYTES stream vector start count)
# liest n Bytes auf einmal.
# Quelle:
#   stream: Handle- oder Socket-Stream
# Ziel: (aref vector start), ..., (aref vector (+ start (- count 1))), wobei
#   vector: semi-simpler 8Bit-Byte-Vektor
#   start: Start-Index in den Vektor
#   count: Anzahl der Bytes

# (SYS::WRITE-N-BYTES stream vector start count)
# schreibt n Bytes auf einmal.
# Quelle: (aref vector start), ..., (aref vector (+ start (- count 1))), wobei
#   vector: semi-simpler 8Bit-Byte-Vektor
#   start: Start-Index in den Vektor
#   count: Anzahl der Bytes
# Ziel:
#   stream: Handle- oder Socket-Stream

# Argumentüberprüfungen:
# Liefert den Index in *index_, den count in *count_, den Datenvektor im
# Stack statt des Vektors, und räumt den Stack um 2 auf.
  local void test_n_bytes_args (uintL* index_, uintL* count_);
  local void test_n_bytes_args(index_,count_)
    var uintL* index_;
    var uintL* count_;
    { if (!streamp(STACK_3)) { fehler_stream(STACK_3); }
      {var object stream = STACK_3;
       if (!(   eq(TheStream(stream)->strm_rd_by,P(rd_by_socket))
             && eq(TheStream(stream)->strm_wr_by,P(wr_by_socket))
          ) )
         { pushSTACK(stream);
           pushSTACK(TheSubr(subr_self)->name);
           fehler(error,
                  DEUTSCH ? "~: Stream muss ein Socket-Stream sein, nicht ~" :
                  ENGLISH ? "~: stream must be a socket-stream, not ~" :
                  FRANCAIS ? "~ : Le stream doit être un «socket-stream» et non ~" :
                  ""
                 );
      }  }
      {var object vector = STACK_2;
       if (!(general_byte_vector_p(vector) # Bit/Byte-Vektor?
             && ((Iarray_flags(vector) & arrayflags_atype_mask) == Atype_8Bit) # 8Bit
          ) )
         { pushSTACK(vector); # Wert für Slot DATUM von TYPE-ERROR
           pushSTACK(O(type_uint8_vector)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
           pushSTACK(vector);
           pushSTACK(TheSubr(subr_self)->name);
           fehler(type_error,
                  DEUTSCH ? "~: Argument ~ sollte ein Vektor vom Typ (ARRAY (UNSIGNED-BYTE 8) (*)) sein." :
                  ENGLISH ? "~: argument ~ should be a vector of type (ARRAY (UNSIGNED-BYTE 8) (*))" :
                  FRANCAIS ? "~ : l'argument ~ doit être un vecteur de type (ARRAY (UNSIGNED-BYTE 8) (*))." :
                  ""
                 );
         }
       if (!posfixnump(STACK_0)) { fehler_posfixnum(STACK_0); }
       *count_ = posfixnum_to_L(popSTACK());
       if (!posfixnump(STACK_0)) { fehler_posfixnum(STACK_0); }
       *index_ = posfixnum_to_L(popSTACK());
       STACK_0 = iarray_displace_check(vector,*count_,index_);
    } }

LISPFUNN(read_n_bytes,4)
  { var uintL startindex;
    var uintL totalcount;
    test_n_bytes_args(&startindex,&totalcount);
    if (!(totalcount==0))
      { var SOCKET handle = TheSocket(TheStream(STACK_1)->strm_ihandle);
        var uintL remaining = totalcount;
        var uintB* ptr = &TheSbvector(TheIarray(STACK_0)->data)->data[startindex];
        begin_system_call();
        # This loop is probably not needed, because sock_read() returns
        # only when done or EOF. Keep it nevertheless.
        loop
          { var sintL ergebnis = sock_read(handle,ptr,remaining);
            if (ergebnis<0) { SOCK_error(); } # Error melden
            ptr += ergebnis; startindex += ergebnis; remaining -= ergebnis;
            if (remaining==0) break; # fertig?
          }
        end_system_call();
      }
    skipSTACK(2);
    value1 = T; mv_count=1; # Wert T
  }

LISPFUNN(write_n_bytes,4)
  { var uintL startindex;
    var uintL totalcount;
    test_n_bytes_args(&startindex,&totalcount);
    if (!(totalcount==0))
      { var SOCKET handle = TheSocket(TheStream(STACK_1)->strm_ihandle);
        var uintL remaining = totalcount;
        var uintB* ptr = &TheSbvector(TheIarray(STACK_0)->data)->data[startindex];
        begin_system_call();
        # This loop is probably not needed, because sock_write() returns
        # only when done or EOF. Keep it nevertheless.
        loop
          { var sintL ergebnis = sock_write(handle,ptr,remaining);
            if (ergebnis<0) { SOCK_error(); } # Error melden
            if (ergebnis==0) # nicht erfolgreich?
              { end_system_call(); fehler_unwritable(S(write_n_bytes),STACK_1); }
            ptr += ergebnis; startindex += ergebnis; remaining -= ergebnis;
            if (remaining==0) break; # fertig?
          }
        end_system_call();
      }
    skipSTACK(2);
    value1 = T; mv_count=1; # Wert T
  }

#endif


#ifdef SOCKET_STREAMS

# Socket-Streams
# ==============

  # define strm_socket_port strm_other[3] # port, a fixnum >=0
  # define strm_socket_host strm_other[4] # host, NIL or a string

# Creates a socket stream.
local object make_socket_stream (SOCKET handle, object host, object port);
local object make_socket_stream(handle,host,port)
  var SOCKET handle;
  var object host; # string
  var object port; # fixnum >=0
  { pushSTACK(allocate_socket(handle));
    # Stream allozieren:
   {var object stream = # neuer Stream, alles erlaubt
      allocate_stream(strmflags_open_B,strmtype_socket,strm_len+5,0);
    TheStream(stream)->strm_rd_by = P(rd_by_socket);
    TheStream(stream)->strm_wr_by = P(wr_by_socket);
    TheStream(stream)->strm_rd_ch = P(rd_ch_socket);
    TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_socket);
    TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
    TheStream(stream)->strm_wr_ss = P(wr_ss_socket);
    TheStream(stream)->strm_socket_port = port;
    TheStream(stream)->strm_socket_host = host;
    TheStream(stream)->strm_ihandle =
    TheStream(stream)->strm_ohandle = popSTACK(); # Handle eintragen
    TheStream(stream)->strm_isatty = NIL;
    return stream;
  }}

local void test_socket_server (object obj);
local void test_socket_server(obj)
  var object obj;
  {
    if (!socket_server_p(obj))
      { pushSTACK(obj);
        pushSTACK(S(socket_server));
        pushSTACK(obj);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               DEUTSCH ? "~: ~ ist kein SOCKET-SERVER" :
               ENGLISH ? "~: ~ is not a SOCKET-SERVER" :
               FRANCAIS ? "~ : ~ n'est pas un SOCKET-SERVER" :
               ""
              );
      }
  }

# Called when some socket server dies.
LISPFUNN(socket_server_close,1)
  { test_socket_server(STACK_0);
   {var SOCKET s = TheSocket(TheSocketServer(popSTACK())->socket_handle);
    begin_system_call();
    loop
      { if (closesocket(s) < 0)
          { if (!sock_errno_is(EINTR)) { SOCK_error(); } }
          else
          break;
      }
    end_system_call();
    value1 = NIL; mv_count=0;
  }}

extern SOCKET create_server_socket (host_data *hd, SOCKET sock, unsigned int port);

LISPFUN(socket_server,0,1,norest,nokey,0,NIL)
# (SOCKET-SERVER [port-or-sock])
  {
    var SOCKET sk;
    var SOCKET sock = (socket_stream_p(STACK_0) ?
                       TheSocket(TheStream(STACK_0)->strm_ihandle) :
                       INVALID_SOCKET);
    var host_data myname;

    if (posfixnump(STACK_0) || eq(STACK_0,unbound) ||
        socket_stream_p(STACK_0)) {
      var unsigned int port = (posfixnump(STACK_0) ? posfixnum_to_L(STACK_0) : 0);
      begin_system_call();
      sk = create_server_socket(&myname, sock, port);
      end_system_call();
      if (sk == INVALID_SOCKET) { SOCK_error(); }
    } else {
      pushSTACK(STACK_0);
      pushSTACK(S(stream));
      pushSTACK(STACK_0);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             DEUTSCH ? "~: Argument ~ ist kein SOCKET-STREAM oder positive FIXNUM" :
             ENGLISH ? "~: argument ~ is neither a SOCKET-STREAM nor a positive FIXNUM" :
             FRANCAIS ? "~ : L'argument ~ n'est pas un SOCKET-STREAM ou FIXNUM positif" :
             ""
             );
    }

    pushSTACK(allocate_socket(sk));
    pushSTACK(allocate_socket_server());
    TheSocketServer(STACK_0)->socket_handle = STACK_1;
    TheSocketServer(STACK_0)->port = fixnum(myname.port);
    { var object host = asciz_to_string(myname.hostname); # for GC-safety
      TheSocketServer(STACK_0)->host = host;}
    pushSTACK(STACK_0);
    pushSTACK(L(socket_server_close));
    funcall(L(finalize),2); # (FINALIZE socket-server #'socket-server-close)
    value1 = popSTACK();
    mv_count = 1;
    skipSTACK(2);
  }

LISPFUNN(socket_server_port,1)
# (SOCKET-SERVER-PORT socket-server)
  { test_socket_server(STACK_0);
    value1 = TheSocketServer(STACK_0)->port;
    mv_count = 1;
    skipSTACK(1);
  }

LISPFUNN(socket_server_host,1)
# (SOCKET-SERVER-HOST socket-server)
  {test_socket_server(STACK_0);
    value1 = TheSocketServer(STACK_0)->host;
    mv_count = 1;
    skipSTACK(1);
  }

extern SOCKET accept_connection (SOCKET socket_handle);

LISPFUNN(socket_accept,1)
# (SOCKET-ACCEPT socket-server)
  {
    var SOCKET sock;
    var SOCKET handle;

    test_socket_server(STACK_0);
    sock = TheSocket(TheSocketServer(STACK_0)->socket_handle);
    begin_system_call();
    handle = accept_connection (sock);
    end_system_call();
    if (handle == INVALID_SOCKET) { SOCK_error(); }
    value1 = make_socket_stream(handle,TheSocketServer(STACK_0)->host,
                                TheSocketServer(STACK_0)->port);
    mv_count = 1;
    skipSTACK(1);
  }

LISPFUN(socket_wait,1,2,norest,nokey,0,NIL)
# (SOCKET-WAIT socket-server [seconds [microseconds]])
  {
    #if defined(HAVE_SELECT) || defined(WIN32_NATIVE)
    var SOCKET handle;
    var struct timeval timeout;
    var struct timeval * timeout_ptr;
    test_socket_server(STACK_2);
    handle = TheSocket(TheSocketServer(STACK_2)->socket_handle);
    restart_select:
    if (eq(STACK_1,unbound))
      { timeout_ptr = NULL; }
    else
      { if (!posfixnump(STACK_1)) { fehler_posfixnum(STACK_1); }
        timeout.tv_sec = posfixnum_to_L(STACK_1);
        if (eq(STACK_0,unbound))
          { timeout.tv_usec = 0; }
        else
          { if (!posfixnump(STACK_0)) { fehler_posfixnum(STACK_0); }
            timeout.tv_usec = posfixnum_to_L(STACK_0);
          }
        timeout_ptr = &timeout;
      }
    begin_system_call();
    {var int ret;
     var fd_set handle_set;
     FD_ZERO(&handle_set); FD_SET(handle,&handle_set);
     ret = select(FD_SETSIZE,&handle_set,NULL,NULL,timeout_ptr);
     if (ret < 0)
       { if (sock_errno_is(EINTR)) { end_system_call(); goto restart_select; }
         SOCK_error();
       }
     end_system_call();
     value1 = (ret == 0) ? NIL : T;
    }
    #else
    value1 = NIL;
    #endif
    mv_count = 1;
    skipSTACK(3);
  }

extern SOCKET create_client_socket (const char* host, int port);

LISPFUN(socket_connect,1,1,norest,nokey,0,NIL)
# (SOCKET-CONNECT port [host])
  {
    var SOCKET handle;
    var char *hostname;

    if (eq(STACK_0,unbound))
      hostname = "localhost";
    elif (stringp(STACK_0))
      hostname = TheAsciz(string_to_asciz(STACK_0));
    else
      fehler_string(STACK_0);

    if (!posfixnump(STACK_1)) { fehler_posfixnum(STACK_1); }

    begin_system_call();
    handle = create_client_socket(hostname,posfixnum_to_L(STACK_1));
    if (handle == INVALID_SOCKET) { SOCK_error(); }
    end_system_call();
    value1 = make_socket_stream(handle,asciz_to_string(hostname),STACK_1);
    skipSTACK(2);
    mv_count = 1;
  }

extern int resolve_service (const char* name_or_number, const char* *name);

LISPFUNN(socket_service_port,1)
# (SOCKET-SERVICE-PORT service-name)
  {
    var const char* service_name;
    var int port;

    if (stringp(STACK_0))
      service_name = TheAsciz(string_to_asciz(STACK_0));
    else
      fehler_string(STACK_0);
    begin_system_call();
    port = resolve_service(service_name,&service_name);
    end_system_call();
    value1 = L_to_I(port);
    skipSTACK(1);
    mv_count=1;
  }

local void test_socket_stream (object obj);
local void test_socket_stream(obj)
  var object obj;
  {
    if (!socket_stream_p(obj))
      {
        pushSTACK(obj);
        pushSTACK(S(stream));
        pushSTACK(obj);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               DEUTSCH ? "~: Argument ~ ist kein SOCKET-STREAM" :
               ENGLISH ? "~: argument ~ is not a SOCKET-STREAM" :
               FRANCAIS ? "~ : L'argument ~ n'est pas un SOCKET-STREAM" :
               ""
              );
      }
  }

LISPFUNN(socket_stream_port,1)
# (SOCKET-STREAM-PORT socket-stream)
  {
    test_socket_stream(STACK_0);
    value1 = TheStream(STACK_0)->strm_socket_port;
    skipSTACK(1);
    mv_count=1;
  }

LISPFUNN(socket_stream_host,1)
# (SOCKET-STREAM-HOST socket-stream)
  {
    test_socket_stream(STACK_0);
    value1 = TheStream(STACK_0)->strm_socket_host;
    skipSTACK(1);
    mv_count=1;
  }

extern host_data * socket_getpeername (SOCKET socket_handle, host_data * hd);
extern host_data * socket_getlocalname (SOCKET socket_handle, host_data * hd);

local void publish_host_data (host_data * (*func)());
local void publish_host_data (func)
  var host_data * (*func)();
  {
    var SOCKET sk;
    var host_data hd;

    test_socket_stream(STACK_0);
    sk = TheSocket(TheStream(STACK_0)->strm_ihandle);

    begin_system_call();
    if (NULL == (*func)(sk, &hd)) { SOCK_error(); }
    end_system_call();
    skipSTACK(1);
    if (NULL == hd.truename) value1 = asciz_to_string (hd.truename);
    else {
      var char* tmp_str = malloc (strlen (hd.truename) + strlen (hd.hostname) + 4);
      strcpy(tmp_str, hd.hostname);
      strcat(tmp_str, " (");
      strcat(tmp_str, hd.truename);
      strcat(tmp_str, ")");
      value1 = asciz_to_string (tmp_str);
      free (tmp_str);
    }
    value2 = fixnum (hd.port);

    mv_count=2;
  }

LISPFUNN(socket_stream_peer,1)
# (SOCKET-STREAM-PEER socket-stream)
  {
    publish_host_data (&socket_getpeername);
  }

LISPFUNN(socket_stream_local,1)
# (SOCKET-STREAM-LOCAL socket-stream)
  {
    publish_host_data (&socket_getlocalname);
  }

#ifndef WIN32_NATIVE

LISPFUNN(socket_stream_handle,1)
# (SOCKET-STREAM-HANDLE socket-stream)
  {
    test_socket_stream(STACK_0);
    value1 = fixnum(TheHandle(TheStream(STACK_0)->strm_ihandle));
    skipSTACK(1);
    mv_count=1;
  }

#endif

#endif


#ifdef GENERIC_STREAMS

# Generic Streams
# ===============

  # Contains a "controller object".
  # define strm_controller_object  strm_other[0]  # see lispbibl.d

  # The function GENERIC-STREAM-CONTROLLER will return some
  # object c associated with the stream s.

  #   (GENERIC-STREAM-READ-CHAR c)                      --> character or NIL
  #   (GENERIC-STREAM-PEEK-CHAR c)                      --> character or NIL
  #   (GENERIC-STREAM-LISTEN c)                         --> {0,1,-1}
  #   (GENERIC-STREAM-CLEAR-INPUT c)                    --> {T,NIL}
  #   (GENERIC-STREAM-WRITE-CHAR c ch)                  -->
  #   (GENERIC-STREAM-WRITE-STRING c string start len)  -->
  #   (GENERIC-STREAM-FINISH-OUTPUT c)                  -->
  #   (GENERIC-STREAM-FORCE-OUTPUT c)                   -->
  #   (GENERIC-STREAM-CLEAR-OUTPUT c)                   -->
  #   (GENERIC-STREAM-READ-BYTE c)                      --> integer or NIL
  #   (GENERIC-STREAM-WRITE-BYTE c i)                   -->
  #   (GENERIC-STREAM-CLOSE c)                          -->

  # (READ-CHAR s) ==
  # (GENERIC-STREAM-READ-CHAR c)
  local object rd_ch_generic (const object* stream_);
  local object rd_ch_generic(stream_)
    var const object* stream_;
    { pushSTACK(*stream_); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_rdch),1);
      return nullp(value1) ? eof_value : value1;
    }

  # (PEEK-CHAR s) ==
  # (GENERIC-STREAM-PEEK-CHAR c)
  local object pk_ch_generic (const object* stream_);
  local object pk_ch_generic(stream_)
    var const object* stream_;
    { pushSTACK(*stream_); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_pkch),1);
      if (nullp(value1)) { value1 = eof_value; }
      if ((mv_count >= 2) && !nullp(value2))
        { # READ-CHAR schon ausgeführt -> muss ein implizites UNREAD-CHAR
          # ausführen (d.h. das Ergebnis für das nächste READ-CHAR/PEEK-CHAR
          # aufheben).
          TheStream(*stream_)->strm_rd_ch_last = value1;
          if (!eq(value1,eof_value))
            { TheStream(*stream_)->strmflags |= strmflags_unread_B; }
        }
      return value1;
    }

  # (LISTEN s) ==
  # (GENERIC-STREAM-LISTEN c)
  local signean listen_generic (object stream);
  local signean listen_generic(stream)
    var object stream;
    { pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_listen),1);
      if (eq (value1, S(Keof))) return -1;
      if (eq (value1, S(Kinput_available))) return 0;
      if (eq (value1, S(Kwait))) return 1;
      # For compatibility with old code -1,0,+1 are still accepted -- depreciated!
      if (eq (value1, Fixnum_minus1) ||
          eq (value1, fixnum (0)) ||
          eq (value1, fixnum (1)))
        return I_to_L(value1);
      # Otherwise raise error:
      pushSTACK (value1);               # DATUM
        pushSTACK (S(member));
        pushSTACK (S(Keof));
        pushSTACK (S(Kinput_available));
        pushSTACK (S(Kwait));
        funcall (L(list),4);
      pushSTACK (value1);               # EXPECTED-TYPE
      pushSTACK (value1);               # once again for the error message.
      pushSTACK (S(generic_stream_listen));
      pushSTACK (STACK_3);              # DATUM
      fehler (type_error, "Return value, ~, of call to ~ is not of type ~.");
    }

  # (CLEAR-INPUT s) ==
  # (GENERIC-STREAM-CLEAR-INPUT c)
  local boolean clear_input_generic (object stream);
  local boolean clear_input_generic(stream)
    var object stream;
    { pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_clear_input),1);
      return !nullp(value1);
    }

  # (WRITE-CHAR ch s) ==
  # (GENERIC-STREAM-WRITE-CHAR c ch)
  local void wr_ch_generic (const object* stream_, object ch);
  local void wr_ch_generic(stream_,ch)
    var const object* stream_;
    var object ch;
    { # ch is a character, need not save it
      pushSTACK(*stream_); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); pushSTACK(ch); funcall(S(generic_stream_wrch),2);
    }

  # (WRITE-SIMPLE-STRING s string start len) ==
  # (GENERIC-STREAM-WRITE-STRING c string start len)
  local void wr_ss_generic (const object* stream_, object string, uintL start, uintL len);
  local void wr_ss_generic(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { pushSTACK(string); # save string
      pushSTACK(*stream_); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); pushSTACK(STACK_(0+1));
      pushSTACK(UL_to_I(start)); pushSTACK(UL_to_I(len));
      funcall(S(generic_stream_wrss),4);
      string = popSTACK();
      wr_ss_lpos(*stream_,&TheSstring(string)->data[start],len);
    }

  # (FINISH-OUTPUT s) ==
  # (GENERIC-STREAM-FINISH-OUTPUT c)
  local void finish_output_generic (object stream);
  local void finish_output_generic(stream)
    var object stream;
    { pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_finish_output),1);
    }

  # (FORCE-OUTPUT s) ==
  # (GENERIC-STREAM-FORCE-OUTPUT c)
  local void force_output_generic (object stream);
  local void force_output_generic(stream)
    var object stream;
    { pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_force_output),1);
    }

  # (CLEAR-OUTPUT s) ==
  # (GENERIC-STREAM-CLEAR-OUTPUT c)
  local void clear_output_generic (object stream);
  local void clear_output_generic(stream)
    var object stream;
    { pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_clear_output),1);
    }

  # (READ-BYTE s) ==
  # (GENERIC-STREAM-READ-BYTE c)
  local object rd_by_generic (object stream);
  local object rd_by_generic(stream)
    var object stream;
    { pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_rdby),1);
      return (nullp(value1) ? eof_value : value1);
    }

  # (WRITE-BYTE s i) ==
  # (GENERIC-STREAM-WRITE-BYTE c i)
  local void wr_by_generic (object stream, object obj);
  local void wr_by_generic(stream,obj)
    var object stream;
    var object obj;
    { pushSTACK(obj); # save obj
      pushSTACK(stream); funcall(L(generic_stream_controller),1);
      obj = STACK_0;
      STACK_0 = value1; pushSTACK(obj); funcall(S(generic_stream_wrby),2);
    }

  # (CLOSE s) ==
  # (GENERIC-STREAM-CLOSE c)
  local void close_generic(stream)
    var object stream;
    { pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_close),1);
    }

LISPFUNN(generic_stream_controller,1)
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (!(   eq(TheStream(stream)->strm_rd_by,P(rd_by_generic))
          && eq(TheStream(stream)->strm_wr_by,P(wr_by_generic))))
      { pushSTACK(stream);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               DEUTSCH ? "~: Stream muss ein Generic-Stream sein, nicht ~" :
               ENGLISH ? "~: stream must be a generic-stream, not ~" :
               FRANCAIS ? "~ : Le stream doit être un «generic-stream» et non ~" :
               ""
              );
      }
    value1=TheStream(stream)->strm_controller_object;
    mv_count=1;
  }

LISPFUNN(make_generic_stream,1)
  { var object stream =
      allocate_stream(strmflags_open_B,strmtype_generic,strm_len+1,0);
    TheStream(stream)->strm_rd_by = P(rd_by_generic);
    TheStream(stream)->strm_wr_by = P(wr_by_generic);
    TheStream(stream)->strm_rd_ch = P(rd_ch_generic);
    TheStream(stream)->strm_pk_ch = P(pk_ch_generic);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_generic);
    TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
    TheStream(stream)->strm_wr_ss = P(wr_ss_generic);
    TheStream(stream)->strm_controller_object = popSTACK();
    value1 = stream; mv_count=1; # stream als Wert
  }

LISPFUNN(generic_stream_p,1)
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if ((eq(TheStream(stream)->strm_rd_by,P(rd_by_generic))
      && eq(TheStream(stream)->strm_wr_by,P(wr_by_generic))))
      { value1 = T; mv_count=1; }
    else
      { value1 = NIL; mv_count=1; }
  }

#endif


# Streams allgemein
# =================

# UP: Return the default value for *terminal-io*.
# kann GC auslösen
  local object make_terminal_io (void);
  local object make_terminal_io()
    { # If stdin or stdout is a file, use a buffered stream instead of an
      # unbuffered terminal stream. For the ud2cd program used as filter,
      # this reduces the runtime on Solaris from 165 sec to 47 sec.
      var boolean stdin_file = regular_handle_p(stdin_handle);
      var boolean stdout_file = regular_handle_p(stdout_handle);
      if (stdin_file || stdout_file)
        { var object stream;
          var uintL position;
          # Allocate stream for stdin:
          if (stdin_file)
            {
              #ifdef UNIX
              pushSTACK(asciz_to_string("/dev/fd/0")); funcall(L(pathname),1);
              pushSTACK(value1);
              #else
              pushSTACK(NIL);
              #endif
              pushSTACK(NIL);
              stream = make_file_stream(allocate_handle(stdin_handle),1,strmtype_ch_file,NIL,FALSE);
              file_lseek(stream,0,SEEK_CUR,position=);
              position_b_file(stream,position);
            }
            else
            { stream = make_terminal_stream(); }
          pushSTACK(stream);
          # Allocate stream for stdout:
          if (stdout_file)
            {
              #ifdef UNIX
              pushSTACK(asciz_to_string("/dev/fd/1")); funcall(L(pathname),1);
              pushSTACK(value1);
              #else
              pushSTACK(NIL);
              #endif
              pushSTACK(NIL);
              stream = make_file_stream(allocate_handle(stdout_handle),4,strmtype_ch_file,NIL,FALSE);
              file_lseek(stream,0,SEEK_CUR,position=);
              position_b_file(stream,position);
            }
            else
            { stream = make_terminal_stream(); }
          # Build a two-way-stream:
          return make_twoway_stream(popSTACK(),stream);
        }
      return make_terminal_stream();
    }

# UP: Initialisiert die Stream-Variablen.
# init_streamvars(unixyp);
# > unixyp: Flag, ob *error-output* nach Unix-Art (vom Standard abweichend)
#           initialisiert werden soll
# kann GC auslösen
  global void init_streamvars (boolean unixyp);
  global void init_streamvars(unixyp)
    var boolean unixyp;
    {
     #ifdef KEYBOARD
      #if defined(UNIX) || defined(RISCOS)
       # Building the keyboard stream is a costly operation. Delay it
       # until we really need it.
       define_variable(S(keyboard_input),NIL);     # *KEYBOARD-INPUT*
      #else
       {var object stream = make_keyboard_stream();
        define_variable(S(keyboard_input),stream); # *KEYBOARD-INPUT*
       }
      #endif
     #endif
     #ifdef GNU_READLINE
     begin_call();
     rl_readline_name = "Clisp";
     if (ilisp_mode)
       # Simuliere folgende Anweisung im .inputrc:
       #   Control-i: self-insert
       { rl_bind_key(CTRL('I'),rl_named_function("self-insert")); }
     rl_attempted_completion_function = &lisp_completion_matches;
     rl_completion_entry_function = &lisp_completion_more;
     end_call();
     #endif
     {var object stream = make_terminal_io();
      define_variable(S(terminal_io),stream);      # *TERMINAL-IO*
     }
     {var object stream = make_synonym_stream(S(terminal_io)); # Synonym-Stream auf *TERMINAL-IO*
      define_variable(S(query_io),stream);         # *QUERY-IO*
      define_variable(S(debug_io),stream);         # *DEBUG-IO*
      define_variable(S(standard_input),stream);   # *STANDARD-INPUT*
      define_variable(S(standard_output),stream);  # *STANDARD-OUTPUT*
      define_variable(S(trace_output),stream);     # *TRACE-OUTPUT*
      #ifdef UNIX
      if (unixyp)
        { # Für *ERROR-OUTPUT* einen anderen Stream verwenden. Auf den
          # Filenamen kommt es nicht an, /dev/fd/2 existiert auch nicht überall.
          pushSTACK(asciz_to_string("/dev/fd/2")); funcall(L(pathname),1);
          pushSTACK(value1); pushSTACK(value1);
          stream = make_handle_stream(allocate_handle(2),4);
        }
      #endif
      define_variable(S(error_output),stream);     # *ERROR-OUTPUT*
    }}

# Liefert Fehlermeldung, wenn der Wert des Symbols sym kein Stream ist.
  local void fehler_value_stream(sym)
    var object sym;
    { # Vor der Fehlermeldung eventuell noch reparieren
      # (so wie bei init_streamvars bzw. init_pathnames initialisiert):
      var object stream;
      pushSTACK(sym); # sym retten
      #ifdef KEYBOARD
      if (eq(sym,S(keyboard_input)))
        # Keyboard-Stream als Default
        { stream = make_keyboard_stream(); }
      else
      #endif
      if (eq(sym,S(terminal_io)))
        # Terminal-Stream als Default
        # (Use make_terminal_stream() here, not make_terminal_io(), because
        # that might have been a file stream and got closed when the disk
        # became full.)
        { stream = make_terminal_stream(); }
      elif (eq(sym,S(query_io)) || eq(sym,S(debug_io)) ||
            eq(sym,S(standard_input)) || eq(sym,S(standard_output)) ||
            eq(sym,S(error_output)) || eq(sym,S(trace_output))
           )
        # Synonym-Stream auf *TERMINAL-IO* als Default
        { stream = make_synonym_stream(S(terminal_io)); }
      else
        # sonstiges Symbol, nicht reparierbar -> sofort Fehlermeldung:
        { pushSTACK(Symbol_value(sym)); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(S(stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(Symbol_value(sym)); # Variablenwert
          pushSTACK(sym); # Variable
          if (!streamp(Symbol_value(sym)))
            { fehler(type_error,
                     DEUTSCH ? "Der Wert von ~ ist kein Stream: ~" :
                     ENGLISH ? "The value of ~ is not a stream: ~" :
                     FRANCAIS ? "La valeur de ~ n'est pas de type STREAM : ~" :
                     ""
                    );
            }
            else
            { fehler(type_error,
                     DEUTSCH ? "Der Wert von ~ ist kein passender Stream: ~" :
                     ENGLISH ? "The value of ~ is not an appropriate stream: ~" :
                     FRANCAIS ? "La valeur de ~ n'est pas un STREAM acceptable : ~" :
                     ""
                    );
        }   }
      sym = popSTACK();
      # Reparatur beendet: stream ist der neue Wert von sym.
     {var object oldvalue = Symbol_value(sym);
      Symbol_value(sym) = stream;
      pushSTACK(oldvalue); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(stream); # neuer Variablenwert
      pushSTACK(oldvalue); # alter Variablenwert
      pushSTACK(sym); # Variable
      fehler(type_error,
             DEUTSCH ? "Der Wert von ~ war kein Stream: ~. Wurde zurückgesetzt auf ~." :
             ENGLISH ? "The value of ~ was not a stream: ~. It has been changed to ~." :
             FRANCAIS ? "La valeur de ~ n'était pas de type STREAM : ~. Changé en ~." :
             ""
            );
    }}

#ifdef GNU_READLINE

# Hilfsfunktionen für die GNU readline Library:

nonreturning_function(local, rl_memory_abort, (void));
local void rl_memory_abort()
  { # Wenn für die Readline-Library der Speicher nicht mehr reicht,
    # schmeißen wir sie raus und ersetzen den Terminal-Stream durch einen,
    # der ohne sie auskommt.
    rl_deprep_terminal(); # alle komischen ioctl()s rückgängig machen
    begin_callback(); # STACK wieder auf einen vernünftigen Wert setzen
    rl_present_p = FALSE;
    Symbol_value(S(terminal_io)) = make_terminal_stream();
    fehler(storage_condition,
           DEUTSCH ? "Readline-Library: kein freier Speicher mehr da." :
           ENGLISH ? "readline library: out of memory." :
           FRANCAIS ? "Bibliothèque readline: mémoire épuisée." :
           ""
          );
  }

global char* xmalloc (int count);
global char* xmalloc(count)
  var int count;
  { var char* tmp = (char*)malloc(count);
    if (tmp) return tmp; else rl_memory_abort();
  }

#ifdef NO_MATCH  # readline-2.2-clisp or newer
  global char* xrealloc (void* ptr, int count);
  global char* xrealloc(ptr,count)
    var void* ptr;
    var int count;
    { var char* tmp = (ptr==NULL ? (char*)malloc(count) : (char*)realloc((char*)ptr,count));
      if (tmp) return tmp; else rl_memory_abort();
    }
#else
  global char* xrealloc (char* ptr, int count);
  global char* xrealloc(ptr,count)
    var char* ptr;
    var int count;
    { var char* tmp = (ptr==NULL ? (char*)malloc(count) : (char*)realloc(ptr,count));
      if (tmp) return tmp; else rl_memory_abort();
    }
#endif

#endif

LISPFUNN(open_stream_p,1)
# (OPEN-STREAM-P stream), CLtL2 S. 505
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (TheStream(stream)->strmflags & strmflags_open_B) # Stream offen?
      { value1 = T; mv_count=1; } # Wert T
      else
      { value1 = NIL; mv_count=1; } # Wert NIL
  }

LISPFUNN(input_stream_p,1)
# (INPUT-STREAM-P stream), CLTL S. 332, CLtL2 S. 505
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (TheStream(stream)->strmflags & strmflags_rd_B) # READ-BYTE oder READ-CHAR erlaubt ?
      { value1 = T; mv_count=1; } # Wert T
      else
      { value1 = NIL; mv_count=1; } # Wert NIL
  }

LISPFUNN(output_stream_p,1)
# (OUTPUT-STREAM-P stream), CLTL S. 332, CLtL2 S. 505
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (TheStream(stream)->strmflags & strmflags_wr_B) # WRITE-BYTE oder WRITE-CHAR erlaubt ?
      { value1 = T; mv_count=1; } # Wert T
      else
      { value1 = NIL; mv_count=1; } # Wert NIL
  }

LISPFUNN(stream_element_type,1)
# (STREAM-ELEMENT-TYPE stream), CLTL S. 332, CLtL2 S. 505
# liefert NIL (für geschlossene Streams) oder CHARACTER oder INTEGER oder T
# oder (spezieller) (UNSIGNED-BYTE n) oder (SIGNED-BYTE n).
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
   {var object eltype;
    if ((TheStream(stream)->strmflags & strmflags_open_B) == 0)
      # Stream geschlossen
      { eltype = NIL; }
      else
      # Stream offen
      { switch (TheStream(stream)->strmtype)
          { # erst die Streamtypen mit eingeschränkten Element-Typen:
            #ifdef KEYBOARD
            case strmtype_keyboard:
              eltype = T;
              break;
            #endif
            case strmtype_terminal:
            case strmtype_ch_file:
            case strmtype_str_in:
            case strmtype_str_out:
            case strmtype_str_push:
            case strmtype_pphelp:
            case strmtype_buff_in:
            case strmtype_buff_out:
            #ifdef SCREEN
            case strmtype_window:
            #endif
            #ifdef PRINTER
            case strmtype_printer:
            #endif
              # CHARACTER
              eltype = S(character); break;
            case strmtype_iu_file:
              # (UNSIGNED-BYTE bitsize)
              pushSTACK(S(unsigned_byte));
              pushSTACK(UL_to_I(FileStream_bitsize(stream)));
              eltype = listof(2);
              break;
            case strmtype_is_file:
              # (SIGNED-BYTE bitsize)
              pushSTACK(S(signed_byte));
              pushSTACK(UL_to_I(FileStream_bitsize(stream)));
              eltype = listof(2);
              break;
            # dann die allgemeinen Streams:
            #ifdef HANDLES
            case strmtype_handle:
            #endif
            #ifdef PIPES
            case strmtype_pipe_in:
            case strmtype_pipe_out:
            #endif
            #ifdef X11SOCKETS
            case strmtype_x11socket:
            #endif
            #ifdef SOCKET_STREAMS
            case strmtype_socket:
            #endif
            #ifdef GENERIC_STREAMS
            case strmtype_generic:
            #endif
            default:
              { var uintB flags = TheStream(stream)->strmflags;
                if (flags & strmflags_by_B)
                  { if (flags & strmflags_ch_B)
                      # (OR CHARACTER INTEGER)
                      { pushSTACK(S(or)); pushSTACK(S(character)); pushSTACK(S(integer));
                        eltype = listof(3);
                      }
                      else
                      # INTEGER
                      { eltype = S(integer); }
                  }
                  else
                  { if (flags & strmflags_ch_B)
                      # CHARACTER
                      { eltype = S(character); }
                      else
                      # NIL
                      { eltype = NIL; }
                  }
                break;
              }
      }   }
    value1 = eltype; mv_count=1;
  }}

LISPFUNN(stream_external_format,1)
# (STREAM-EXTERNAL-FORMAT stream)
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    value1 = S(Kdefault); mv_count=1; # 1 Wert :DEFAULT
  }

# UP: Stellt fest, ob ein Stream "interaktiv" ist, d.h. ob Input vom Stream
# vermutlich von einem vorher ausgegebenen Prompt abhängen wird.
# interactive_stream_p(stream)
# > stream: Stream
# NB: Relation zwischen clear_input, listen, interactive_stream_p:
#   Wenn nach clear_input(stream) stream_listen(stream) > 0 ist
#   (d.h. kein Zeichen mehr verfügbar und nicht EOF), dann ist
#   interactive_stream_p(stream) TRUE.
#   (Denn dann ist stream effektiv ein Keyboard-Stream, Terminal-Stream,
#   Handle-Stream mit !regular_handle_p(ihandle), Pipe-Input-Stream,
#   X11-Socket-Stream, Socket-Stream oder Generic-Stream.)
#   (Bei einem Concatenated-Stream, der gerade am Ende eines nicht interaktiven
#   Teil-Stream ist und wo der nächste Teil-Stream nicht interaktiv ist, gilt
#   das evtl. nicht. Aber das kann man abfangen, indem man vor der Abfrage
#   noch ein stream_listen(stream) einschiebt.)
#   Aber nicht umgekehrt: Bei Streams vom Typ strmtype_pipe_in,
#   strmtype_x11socket, strmtype_socket (die interactive_stream_p(stream)
#   erfüllen) tut clear_input(stream) nichts, und stream_listen(stream) kann
#   0 liefern.
  global boolean interactive_stream_p (object stream);
  global boolean interactive_stream_p(stream)
    var object stream;
    { start:
      if ((TheStream(stream)->strmflags & strmflags_rd_B) == 0)
        # Stream für Input geschlossen
        { return FALSE; }
      # Stream offen
      switch (TheStream(stream)->strmtype)
        {
          #if !(defined(NEXTAPP)) || defined(HANDLES)
          #if !defined(NEXTAPP)
          case strmtype_terminal:
          #endif
          #ifdef HANDLES
          case strmtype_handle:
            if (nullp(TheStream(stream)->strm_isatty))
              # Reguläre Files sind sicher nicht interaktiv.
              if (regular_handle_p(TheHandle(TheStream(stream)->strm_ihandle)))
                return FALSE;
          #endif
          #endif
          #ifdef KEYBOARD
          case strmtype_keyboard:
          #endif
          #if defined(NEXTAPP)
          case strmtype_terminal:
          #endif
          case strmtype_buff_in:
          #ifdef PIPES
          case strmtype_pipe_in:
          #endif
          #ifdef X11SOCKETS
          case strmtype_x11socket:
          #endif
          #ifdef SOCKET_STREAMS
          case strmtype_socket:
          #endif
          #ifdef GENERIC_STREAMS
          case strmtype_generic:
          #endif
            return TRUE;
          case strmtype_synonym:
            # Synonym-Stream: weiterverfolgen
            { var object symbol = TheStream(stream)->strm_synonym_symbol;
              stream = get_synonym_stream(symbol);
              /* return interactive_stream_p(stream); */ # entrekursiviert:
              goto start;
            }
          case strmtype_concat:
            # Hier könnte man stream_listen(stream) machen, um Streams, die
            # am EOF angelangt sind, zu ignorieren. Aber es ist nicht gut,
            # wenn interactive_stream_p Systemaufrufe und I/O macht.??
            # Den ersten der Streams abfragen:
            { var object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
              if (consp(streamlist))
                { stream = Car(streamlist);
                  /* return interactive_stream_p(stream); */ # entrekursiviert:
                  goto start;
                }
                else
                { return FALSE; }
            }
          case strmtype_twoway:
          case strmtype_echo:
            { # Two-Way-Stream oder Echo-Stream: Input-Stream anschauen
              stream = TheStream(stream)->strm_twoway_input;
              /* return interactive_stream_p(stream); */ # entrekursiviert:
              goto start;
            }
          case strmtype_ch_file:
          case strmtype_iu_file:
          case strmtype_is_file:
          case strmtype_str_in:
          default:
            return FALSE;
    }   }

LISPFUNN(interactive_stream_p,1)
# (INTERACTIVE-STREAM-P stream), CLTL2 S. 507/508
# stellt fest, ob stream interaktiv ist.
  { var object arg = popSTACK();
    if (!streamp(arg)) { fehler_stream(arg); }
    value1 = (interactive_stream_p(arg) ? T : NIL); mv_count=1;
  }

# UP: Schließt einen Stream.
# stream_close(&stream);
# > stream: Stream
# < stream: Stream
# kann GC auslösen
  global void stream_close (const object* stream_);
  global void stream_close(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      if ((TheStream(stream)->strmflags & strmflags_open_B) == 0) # Stream schon geschlossen?
        return;
      # Typspezifische Routine aufrufen (darf GC auslösen):
      switch (TheStream(stream)->strmtype)
        {
          #ifdef KEYBOARD
          case strmtype_keyboard: break;
          #endif
          case strmtype_terminal: break;
          case strmtype_ch_file:
          case strmtype_iu_file:
          case strmtype_is_file:
            close_file(stream); break;
          case strmtype_synonym:
            close_synonym(stream); break; # X3J13_014 sagt: nichtrekursiv
          case strmtype_broad: break; # nichtrekursiv
          case strmtype_concat: break; # nichtrekursiv
          case strmtype_twoway: break; # nichtrekursiv
          case strmtype_echo: break; # nichtrekursiv
          case strmtype_str_in:
            close_str_in(stream); break;
          case strmtype_str_out: break;
          case strmtype_str_push: break;
          case strmtype_pphelp: break;
          case strmtype_buff_in:
            close_buff_in(stream); break;
          case strmtype_buff_out:
            close_buff_out(stream); break;
          #ifdef SCREEN
          case strmtype_window:
            close_window(stream); break;
          #endif
          #ifdef PRINTER_AMIGAOS
          case strmtype_printer:
            close_printer(stream); break;
          #endif
          #ifdef HANDLES
          case strmtype_handle:
            close_handle(stream); break;
          #endif
          #ifdef PIPES
          case strmtype_pipe_in:
            close_pipe_in(stream); break;
          case strmtype_pipe_out:
            close_pipe_out(stream); break;
          #endif
          #ifdef X11SOCKETS
          case strmtype_x11socket:
            close_x11socket(stream); break;
          #endif
          #ifdef SOCKET_STREAMS
          case strmtype_socket:
            close_socket(stream); break;
          #endif
          #ifdef GENERIC_STREAMS
          case strmtype_generic:
            close_generic(stream); break;
          #endif
          default: NOTREACHED
        }
      # Dummys eintragen:
      close_dummys(*stream_);
    }

# UP: Schließt eine Liste offener Files.
# close_some_files(list);
# > list: Liste von offenen Streams
# kann GC auslösen
  global void close_some_files (object list);
  global void close_some_files(list)
    var object list;
    { pushSTACK(NIL); # dummy
      pushSTACK(list); # list
      while (mconsp(STACK_0))
        { var object streamlist = STACK_0;
          STACK_0 = Cdr(streamlist); # restliche Streams
          STACK_1 = Car(streamlist); # ein Stream aus der Liste
          stream_close(&STACK_1); # schließen
        }
      skipSTACK(2);
    }

# UP: Schließt alle offenen Files.
# close_all_files();
# kann GC auslösen
  global void close_all_files (void);
  global void close_all_files()
    { close_some_files(O(open_files)); } # Liste aller offenen File-Streams

# UP: Erklärt alle offenen File-Streams für geschlossen.
# closed_all_files();
  global void closed_all_files (void);
  global void closed_all_files()
    { var object streamlist = O(open_files); # Liste aller offenen File-Streams
      while (consp(streamlist))
        { var object stream = Car(streamlist); # ein Stream aus der Liste
          if_strm_bfile_p(stream, # File-Stream ?
            { if (!nullp(TheStream(stream)->strm_file_handle)) # mit Handle /= NIL ?
                # ja: Stream noch offen
                { closed_file(stream); }
            },
            ; );
          close_dummys(stream);
          streamlist = Cdr(streamlist); # restliche Streams
        }
      O(open_files) = NIL; # keine offenen Files mehr
    }

LISPFUN(close,1,0,norest,key,1, (kw(abort)) )
# (CLOSE stream :abort), CLTL S. 332
  { skipSTACK(1); # :ABORT-Argument ignorieren
   {var object stream = STACK_0; # Argument
    if (!streamp(stream)) { fehler_stream(stream); } # muss ein Stream sein
    stream_close(&STACK_0); # schließen
    skipSTACK(1);
    value1 = T; mv_count=1; # T als Ergebnis
  }}

# UP: Stellt fest, ob im Stream stream ein Zeichen sofort verfügbar ist.
# stream_listen(stream)
# > stream: Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  global signean stream_listen (object stream);
  global signean stream_listen(stream)
    var object stream;
    { check_SP(); check_STACK();
      if (TheStream(stream)->strmflags & strmflags_unread_B) # Char nach UNREAD ?
        { return signean_null; } # ja -> verfügbar
        else
        # sonst nach Streamtyp verzweigen.
        # Jede Einzelroutine darf GC auslösen. Außer beim Keyboard-Stream
        # oder Terminal-Stream handelt es sich um einen reinen EOF-Test.
        { switch (TheStream(stream)->strmtype)
            {
              #ifdef KEYBOARD
              case strmtype_keyboard: return listen_keyboard(stream);
              #endif
              case strmtype_terminal:
                #if defined(NEXTAPP)
                return listen_terminal(stream);
                #endif
                #if (defined(UNIX) && !defined(NEXTAPP)) || defined(MSDOS) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32_NATIVE)
                terminalcase(stream,
                             { return listen_terminal1(stream); },
                             { return listen_terminal2(stream); },
                             { return listen_terminal3(stream); }
                            );
                #endif
                NOTREACHED
              case strmtype_ch_file:
                if (TheStream(stream)->strmflags & strmflags_rd_ch_B)
                  { return listen_ch_file(stream); }
                  else
                  { return signean_minus; } # kein READ-CHAR
              case strmtype_iu_file:  return signean_minus; # kein READ-CHAR
              case strmtype_is_file:  return signean_minus; # kein READ-CHAR
              case strmtype_synonym:  return listen_synonym(stream);
              case strmtype_broad:    return signean_minus; # kein READ-CHAR
              case strmtype_concat:   return listen_concat(stream);
              case strmtype_twoway:   return listen_twoway(stream);
              case strmtype_echo:     return listen_twoway(stream);
              case strmtype_str_in:   return listen_str_in(stream);
              case strmtype_str_out:  return signean_minus; # kein READ-CHAR
              case strmtype_str_push: return signean_minus; # kein READ-CHAR
              case strmtype_pphelp:   return signean_minus; # kein READ-CHAR
              case strmtype_buff_in:  return listen_buff_in(stream);
              case strmtype_buff_out: return signean_minus; # kein READ-CHAR
              #ifdef SCREEN
              case strmtype_window:   return signean_minus; # kein READ-CHAR
              #endif
              #ifdef PRINTER
              case strmtype_printer:  return signean_minus; # kein READ-CHAR
              #endif
              #ifdef HANDLES
              case strmtype_handle:
                if (TheStream(stream)->strmflags & strmflags_rd_ch_B)
                  { return listen_handle(stream); }
                  else
                  { return signean_minus; } # kein READ-CHAR
              #endif
              #ifdef PIPES
              case strmtype_pipe_in:  return listen_pipe_in(stream);
              case strmtype_pipe_out: return signean_minus; # kein READ-CHAR
              #endif
              #ifdef X11SOCKETS
              case strmtype_x11socket:   return listen_x11socket(stream);
              #endif
              #ifdef SOCKET_STREAMS
              case strmtype_socket:   return listen_socket(stream);
              #endif
              #ifdef GENERIC_STREAMS
              case strmtype_generic:  return listen_generic(stream);
              #endif
              default: # Allgemein: nur EOF abfragen
                if (TheStream(stream)->strmflags & strmflags_rd_ch_B)
                  { pushSTACK(stream);
                   {var object nextchar = peek_char(&STACK_0);
                    skipSTACK(1);
                    if (eq(nextchar,eof_value))
                      { return signean_minus; } # EOF erreicht
                      else
                      { return signean_null; }
                  }}
                  else
                  { return signean_minus; } # kein READ-CHAR
    }   }   }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Stream stream.
# clear_input(stream)
# > stream: Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# kann GC auslösen
  global boolean clear_input (object stream);
  global boolean clear_input(stream)
    var object stream;
    { check_SP(); check_STACK();
      pushSTACK(stream); # Stream retten
      # Typspezifische Routine aufrufen (darf GC auslösen).
      # Nur beim Keyboard-Stream und Terminal-Stream wird etwas getan.
     {var boolean ergebnis;
      switch (TheStream(stream)->strmtype)
        {
          #ifdef KEYBOARD
          case strmtype_keyboard:
            ergebnis = clear_input_keyboard(stream); break;
          #endif
          case strmtype_terminal:
            #if defined(NEXTAPP)
            ergebnis = clear_input_terminal(stream);
            #endif
            #if (defined(UNIX) && !defined(NEXTAPP)) || defined(MSDOS) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32_NATIVE)
            terminalcase(stream,
                         { ergebnis = clear_input_terminal1(stream); },
                         { ergebnis = clear_input_terminal2(stream); },
                         { ergebnis = clear_input_terminal3(stream); }
                        );
            #endif
            break;
          case strmtype_synonym:
            ergebnis = clear_input_synonym(stream); break;
          case strmtype_concat:
            ergebnis = clear_input_concat(stream); break;
          case strmtype_twoway:
          case strmtype_echo:
            ergebnis = clear_input_twoway(stream); break;
          case strmtype_buff_in:
            ergebnis = clear_input_buff_in(stream); break;
          #ifdef HANDLES
          case strmtype_handle:
            if (TheStream(stream)->strmflags & strmflags_rd_ch_B)
              { ergebnis = clear_input_handle(stream); }
              else
              { ergebnis = FALSE; }
            break;
          #endif
          #ifdef GENERIC_STREAMS
          case strmtype_generic:
            ergebnis = clear_input_generic(stream); break;
          #endif
          #ifdef PIPES
          case strmtype_pipe_in: # Pipe: nichts löschen??
          #endif
          #ifdef X11SOCKETS
          case strmtype_x11socket: # Socket: nichts löschen??
          #endif
          #ifdef SOCKET_STREAMS
          case strmtype_socket: # Socket: nichts löschen??
          #endif
          default:
            ergebnis = FALSE; break;
        }
      stream = popSTACK();
      if (ergebnis)
        # Input wurde gelöscht -> auch das Lastchar muss gelöscht werden.
        # Dabei wird auch ein schon gesehenes EOF vergessen.
        { TheStream(stream)->strm_rd_ch_last = NIL;
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
        }
      return ergebnis;
    }}

# UP: Wartenden Output eines Stream stream ans Ziel bringen.
# finish_output(stream);
# > stream: Stream
# kann GC auslösen
  global void finish_output (object stream);
  global void finish_output(stream)
    var object stream;
    { if (TheStream(stream)->strmflags & strmflags_wr_B) # Output-Stream?
        # nein -> fertig, ja -> nach Streamtyp verzweigen:
        { switch (TheStream(stream)->strmtype)
            { case strmtype_terminal:
                finish_output_terminal(stream); break;
              case strmtype_ch_file:
              case strmtype_iu_file:
              case strmtype_is_file:
                finish_output_file(stream); break;
              case strmtype_synonym:
                finish_output_synonym(stream); break;
              case strmtype_broad:
                finish_output_broad(stream); break;
              case strmtype_twoway:
              case strmtype_echo:
                finish_output_twoway(stream); break;
              case strmtype_buff_out:
                finish_output_buff_out(stream); break;
              #ifdef PRINTER_AMIGAOS
              case strmtype_printer: # Printer:
                # Schließen und neu aufmachen würde vermutlich einen
                # Seitenvorschub ausgeben, und das ist ja wohl nicht erwünscht.
                break; # Daher nichts tun.
              #endif
              #ifdef HANDLES
              case strmtype_handle:
                finish_output_handle(stream); break;
              #endif
              #ifdef GENERIC_STREAMS
              case strmtype_generic:
                finish_output_generic(stream); break;
              #endif
              #ifdef PIPES
              case strmtype_pipe_out: # Pipe: kann nichts tun
              #endif
              #ifdef X11SOCKETS
              case strmtype_x11socket: # Socket: kann nichts tun
              #endif
              #ifdef SOCKET_STREAMS
              case strmtype_socket: # Socket: kann nichts tun
              #endif
              default: # nichts tun
                break;
        }   }
    }

# UP: Wartenden Output eines Stream stream ans Ziel bringen.
# force_output(stream);
# > stream: Stream
# kann GC auslösen
  global void force_output (object stream);
  global void force_output(stream)
    var object stream;
    { if (TheStream(stream)->strmflags & strmflags_wr_B) # Output-Stream?
        # nein -> fertig, ja -> nach Streamtyp verzweigen:
        { switch (TheStream(stream)->strmtype)
            { case strmtype_terminal:
                force_output_terminal(stream); break;
              case strmtype_ch_file:
              case strmtype_iu_file:
              case strmtype_is_file:
                force_output_file(stream); break;
              case strmtype_synonym:
                force_output_synonym(stream); break;
              case strmtype_broad:
                force_output_broad(stream); break;
              case strmtype_twoway:
              case strmtype_echo:
                force_output_twoway(stream); break;
              case strmtype_buff_out:
                force_output_buff_out(stream); break;
              #ifdef PRINTER_AMIGAOS
              case strmtype_printer: # Printer:
                # Schließen und neu aufmachen würde vermutlich einen
                # Seitenvorschub ausgeben, und das ist ja wohl nicht erwünscht.
                break; # Daher nichts tun.
              #endif
              #ifdef HANDLES
              case strmtype_handle:
                force_output_handle(stream); break;
              #endif
              #ifdef GENERIC_STREAMS
              case strmtype_generic:
                force_output_generic(stream); break;
              #endif
              #ifdef PIPES
              case strmtype_pipe_out: # Pipe: kann nichts tun
              #endif
              #ifdef X11SOCKETS
              case strmtype_x11socket: # Socket: kann nichts tun
              #endif
              #ifdef SOCKET_STREAMS
              case strmtype_socket: # Socket: kann nichts tun
              #endif
              default: # nichts tun
                break;
        }   }
    }

# UP: Wartenden Output eines Stream stream löschen.
# clear_output(stream);
# > stream: Stream
# kann GC auslösen
  global void clear_output (object stream);
  global void clear_output(stream)
    var object stream;
    { # Unter DOS ist zwar bei keinem File- oder Terminal-Stream etwas zu tun,
      # aber das kann man nicht ausnutzen, denn clear_output auf
      # Buffered-Output-Streams geht immer.
      if (TheStream(stream)->strmflags & strmflags_wr_B) # Output-Stream?
        # nein -> fertig, ja -> nach Streamtyp verzweigen:
        { switch (TheStream(stream)->strmtype)
            { case strmtype_terminal:
                #if (defined(UNIX) && !defined(NEXTAPP)) || defined(MSDOS) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32_NATIVE)
                terminalcase(stream,
                             { clear_output_terminal1(stream); },
                             { clear_output_terminal2(stream); },
                             { clear_output_terminal3(stream); }
                            );
                #endif
                break;
              case strmtype_ch_file:
              case strmtype_iu_file:
              case strmtype_is_file:
                # File: nichts tun (würde die File-Verwaltung durcheinanderbringen)
                break;
              case strmtype_synonym:
                clear_output_synonym(stream); break;
              case strmtype_broad:
                clear_output_broad(stream); break;
              case strmtype_twoway:
              case strmtype_echo:
                clear_output_twoway(stream); break;
              case strmtype_buff_out:
                clear_output_buff_out(stream); break;
              #ifdef PRINTER_AMIGAOS
              case strmtype_printer: # Printer: ungebuffert, also nichts zu tun
                break;
              #endif
              #ifdef HANDLES
              case strmtype_handle:
                clear_output_handle(stream); break;
              #endif
              #ifdef PIPES
              case strmtype_pipe_out: # Pipe: geht nicht
                break;
              #endif
              #ifdef X11SOCKETS
              case strmtype_x11socket: # Socket: geht nicht
                break;
              #endif
              #ifdef SOCKET_STREAMS
              case strmtype_socket: # Socket: geht nicht
                break;
              #endif
              #ifdef GENERIC_STREAMS
              case strmtype_generic:
                clear_output_generic(stream);
                break;
              #endif
              default: # nichts tun
                break;
        }   }
    }

# UP: Liefert die Line-Position eines Streams.
# get_line_position(stream)
# > stream: Stream
# < ergebnis: Line-Position (Fixnum >=0)
  global object get_line_position (object stream);
  global object get_line_position(stream)
    var object stream;
    { check_SP();
      start:
      switch (TheStream(stream)->strmtype)
        { case strmtype_synonym:
            # Synonym-Stream: weiterverfolgen
            { var object symbol = TheStream(stream)->strm_synonym_symbol;
              stream = get_synonym_stream(symbol);
              /* return get_line_position(stream); */ # entrekursiviert:
              goto start;
            }
          case strmtype_broad:
            # Broadcast-Stream:
            # Maximum der Line-Positions der einzelnen Streams
            { var object streamlist = TheStream(stream)->strm_broad_list;
              var uintL maximum = 0; # bisheriges Maximum := 0
              while (consp(streamlist))
                { var uintL next = # Line-Position des nächsten Teilstreams
                    posfixnum_to_L(get_line_position(Car(streamlist)));
                  if (next > maximum) { maximum = next; } # Maximum nehmen
                  streamlist = Cdr(streamlist);
                }
              return fixnum(maximum); # Maximum als Ergebnis
            }
          case strmtype_twoway:
          case strmtype_echo:
            { # Two-Way-Stream oder Echo-Stream: Output-Stream anschauen
              stream = TheStream(stream)->strm_twoway_output;
              /* return get_line_position(stream); */ # entrekursiviert:
              goto start;
            }
          default: # normaler Stream
            return TheStream(stream)->strm_wr_ch_lpos;
    }   }

# UP: Liest mehrere Bytes von einem Stream.
# read_byte_array(stream,byteptr,len)
# > stream: Stream
# > uintB* byteptr: Adresse der zu füllenden Bytefolge
# > uintL len: Länge der zu füllenden Bytefolge
# < uintB* ergebnis: Pointer ans Ende des gefüllten Bereiches oder NULL
  global uintB* read_byte_array (object stream, uintB* byteptr, uintL len);
  global uintB* read_byte_array(stream,byteptr,len)
    var object stream;
    var uintB* byteptr;
    var uintL len;
    { if (len==0) { return byteptr; }
      start:
      if (eq(TheStream(stream)->strm_rd_by,P(rd_by_synonym))) # synonym
        { stream = get_synonym_stream(TheStream(stream)->strm_synonym_symbol);
          goto start;
        }
      elif (eq(TheStream(stream)->strm_rd_by,P(rd_by_twoway))) # twoway
        { stream = TheStream(stream)->strm_twoway_input;
          goto start;
        }
      #if defined(HANDLES) || defined(X11SOCKETS) || defined(SOCKET_STREAMS)
      elif (eq(TheStream(stream)->strm_rd_by,P(rd_by_handle))) # handle, x11socket, socket
        { var Handle handle = TheHandle(TheStream(stream)->strm_ihandle);
          begin_system_call();
         {var sintL ergebnis = full_read(handle,byteptr,len);
          if (ergebnis<0)
            {
              #if !(defined(AMIGAOS) || defined(WIN32_NATIVE))
              if (errno==EINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
                interruptp(
                  { end_system_call();
                    pushSTACK(S(read_byte_sequence));
                    fehler(serious_condition,
                           DEUTSCH ? "~: Ctrl-C: Tastatur-Interrupt" :
                           ENGLISH ? "~: Ctrl-C: User break" :
                           FRANCAIS ? "~ : Ctrl-C : Interruption clavier" :
                           ""
                          );
                  });
              #endif
              OS_error(); # Error melden
            }
          byteptr += ergebnis;
          end_system_call();
          return byteptr;
        }}
      #endif
      #if (defined(X11SOCKETS) || defined(SOCKET_STREAMS)) && defined(WIN32_NATIVE)
      elif (eq(TheStream(stream)->strm_rd_by,P(rd_by_socket))) # x11socket, socket
        { var SOCKET handle = TheSocket(TheStream(stream)->strm_ihandle);
          begin_system_call();
          # This loop is probably not needed, because sock_read() returns
          # only when done or EOF. Keep it nevertheless.
          loop
            { var sintL ergebnis = sock_read(handle,byteptr,len);
              if (ergebnis<0) { SOCK_error(); } # Error melden
              if (ergebnis==0) break; # EOF -> fertig
              byteptr += ergebnis; len -= ergebnis;
              if (len==0) break; # fertig?
            }
          end_system_call();
          return byteptr;
        }
      #endif
      elif (eq(TheStream(stream)->strm_rd_by,P(rd_by_iau_file)) # file
            && (FileStream_bitsize(stream)==8) # eltype = (UNSIGNED-BYTE 8)
           )
        { dotimespL(len,len,
            { var uintB* ptr = b_file_nextbyte(stream);
              if (ptr == (uintB*)NULL) break;
              *byteptr++ = *ptr;
              # index und position incrementieren:
              FileStream_index(stream) += 1;
              FileStream_position(stream) += 1;
            });
          return byteptr;
        }
      else # keine Optimierung möglich
        { return NULL; }
    }

# UP: Schreibt mehrere Bytes auf einen Stream.
# write_byte_array(stream,byteptr,len)
# > stream: Stream
# > uintB* byteptr: Adresse der zu schreibenden Bytefolge
# > uintL len: Länge der zu schreibenden Bytefolge
# < uintB* ergebnis: Pointer ans Ende des geschriebenen Bereiches oder NULL
  global const uintB* write_byte_array (object stream, const uintB* byteptr, uintL len);
  global const uintB* write_byte_array(stream,byteptr,len)
    var object stream;
    var const uintB* byteptr;
    var uintL len;
    { if (len==0) { return byteptr; }
      start:
      if (eq(TheStream(stream)->strm_wr_by,P(wr_by_synonym))) # synonym
        { stream = get_synonym_stream(TheStream(stream)->strm_synonym_symbol);
          goto start;
        }
      elif (eq(TheStream(stream)->strm_wr_by,P(wr_by_twoway))) # twoway, echo
        { stream = TheStream(stream)->strm_twoway_output;
          goto start;
        }
      #if defined(HANDLES) || defined(X11SOCKETS) || defined(SOCKET_STREAMS)
      elif (eq(TheStream(stream)->strm_wr_by,P(wr_by_handle))) # handle, x11socket, socket
        { var Handle handle = TheHandle(TheStream(stream)->strm_ohandle);
          begin_system_call();
         {var sintL ergebnis = full_write(handle,byteptr,len);
          if (ergebnis<0) { OS_error(); } # Error melden
          end_system_call();
          if (!(ergebnis==(sintL)len)) # nicht erfolgreich?
            { fehler_unwritable(S(write_byte_sequence),stream); }
          return byteptr+ergebnis;
        }}
      #endif
      #if (defined(X11SOCKETS) || defined(SOCKET_STREAMS)) && defined(WIN32_NATIVE)
      elif (eq(TheStream(stream)->strm_wr_by,P(wr_by_socket))) # x11socket, socket
        { var SOCKET handle = TheSocket(TheStream(stream)->strm_ohandle);
          begin_system_call();
          # This loop is probably not needed, because sock_write() returns
          # only when done or EOF. Keep it nevertheless.
          loop
            { var sintL ergebnis = sock_write(handle,byteptr,len);
              if (ergebnis<0) { SOCK_error(); } # Error melden
              if (ergebnis==0) # nicht erfolgreich?
                { end_system_call(); fehler_unwritable(S(write_byte_sequence),stream); }
              byteptr += ergebnis; len -= ergebnis;
              if (len==0) break; # fertig?
            }
          end_system_call();
          return byteptr;
        }
      #endif
      elif (eq(TheStream(stream)->strm_wr_by,P(wr_by_iau_file)) # file
            && (FileStream_bitsize(stream)==8) # eltype = (UNSIGNED-BYTE 8)
           )
        { return write_byte_array_iau8_file(stream,byteptr,len); }
      else # keine Optimierung möglich
        { return NULL; }
    }

# UP: Liest mehrere Characters von einem Stream.
# read_char_array(stream,charptr,len)
# > stream: Stream
# > uintB* charptr: Adresse der zu füllenden Zeichenfolge
# > uintL len: Länge der zu füllenden Zeichenfolge
# < uintB* ergebnis: Pointer ans Ende des gefüllten Bereiches oder NULL
  global uintB* read_char_array (object stream, uintB* charptr, uintL len);
  global uintB* read_char_array(stream,charptr,len)
    var object stream;
    var uintB* charptr;
    var uintL len;
    { if (len==0) { return charptr; }
     {var object lastchar = TheStream(stream)->strm_rd_ch_last;
      if (eq(lastchar,eof_value)) # EOF ?
        { return charptr; }
      if ((TheStream(stream)->strmflags & strmflags_unread_B) # Char nach UNREAD ?
          && !charp(lastchar) # aber kein Character?
         )
        { return NULL; }
      if (eq(TheStream(stream)->strm_rd_ch,P(rd_ch_synonym))) # synonym
        { var object substream = get_synonym_stream(TheStream(stream)->strm_synonym_symbol);
          check_SP();
         {var uintB* endptr =
            (TheStream(stream)->strmflags & strmflags_unread_B
             ? read_char_array(substream,charptr+1,len-1)
             : read_char_array(substream,charptr,len)
            );
          if (endptr==NULL) { return NULL; }
          if (TheStream(stream)->strmflags & strmflags_unread_B)
            { charptr[0] = char_code(lastchar); }
          TheStream(stream)->strm_rd_ch_last =
            (endptr == charptr+len ? code_char(endptr[-1]) : eof_value);
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
          return endptr;
        }}
      elif (eq(TheStream(stream)->strm_rd_ch,P(rd_ch_twoway))) # twoway
        { var object substream = TheStream(stream)->strm_twoway_input;
          check_SP();
         {var uintB* endptr =
            (TheStream(stream)->strmflags & strmflags_unread_B
             ? read_char_array(substream,charptr+1,len-1)
             : read_char_array(substream,charptr,len)
            );
          if (endptr==NULL) { return NULL; }
          if (TheStream(stream)->strmflags & strmflags_unread_B)
            { charptr[0] = char_code(lastchar); }
          TheStream(stream)->strm_rd_ch_last =
            (endptr == charptr+len ? code_char(endptr[-1]) : eof_value);
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
          return endptr;
        }}
      #ifdef XHANDLES
      elif (eq(TheStream(stream)->strm_rd_ch,P(rd_ch_handle))) # handle, pipe_in, socket
        { if (TheStream(stream)->strmflags & strmflags_unread_B)
            { *charptr++ = char_code(lastchar); len--; }
          if (len>0)
            { var Handle handle = TheHandle(TheStream(stream)->strm_ihandle);
              run_time_stop(); # Run-Time-Stoppuhr anhalten
              begin_system_call();
              #ifdef GRAPHICS_SWITCH
              if (handle == stdin_handle) switch_text_mode();
              #endif
             {var sintL ergebnis = full_read(handle,charptr,len);
              if (ergebnis<0)
                {
                  #if !(defined(AMIGAOS) || defined(WIN32_NATIVE))
                  if (errno==EINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
                    interruptp(
                      { end_system_call();
                        run_time_restart();
                        pushSTACK(S(read_char_sequence));
                        fehler(serious_condition,
                               DEUTSCH ? "~: Ctrl-C: Tastatur-Interrupt" :
                               ENGLISH ? "~: Ctrl-C: User break" :
                               FRANCAIS ? "~ : Ctrl-C : Interruption clavier" :
                               ""
                              );
                      });
                  #endif
                  OS_error(); # Error melden
                }
              charptr += ergebnis; len -= ergebnis;
              end_system_call();
              run_time_restart();
            }}
          TheStream(stream)->strm_rd_ch_last =
            (len==0 ? code_char(charptr[-1]) : eof_value);
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
          return charptr;
        }
      #endif
      #if (defined(X11SOCKETS) || defined(SOCKET_STREAMS)) && defined(WIN32_NATIVE)
      elif (eq(TheStream(stream)->strm_rd_ch,P(rd_ch_socket))) # socket
        { if (TheStream(stream)->strmflags & strmflags_unread_B)
            { *charptr++ = char_code(lastchar); len--; }
          if (len>0)
            { var SOCKET handle = TheSocket(TheStream(stream)->strm_ihandle);
              run_time_stop(); # Run-Time-Stoppuhr anhalten
              begin_system_call();
              # This loop is probably not needed, because sock_read() returns
              # only when done or EOF. Keep it nevertheless, for the sake of EINTR.
              loop
                { var sintL ergebnis = sock_read(handle,charptr,len);
                  if (ergebnis<0)
                    { if (WSAGetLastError()==WSAEINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
                        { end_system_call();
                          run_time_restart();
                          pushSTACK(S(read_char_sequence));
                          fehler(serious_condition,
                                 DEUTSCH ? "~: Ctrl-C: Tastatur-Interrupt" :
                                 ENGLISH ? "~: Ctrl-C: User break" :
                                 FRANCAIS ? "~ : Ctrl-C : Interruption clavier" :
                                 ""
                                );
                        }
                      SOCK_error(); # Error melden
                    }
                  if (ergebnis==0) break; # EOF -> fertig
                  charptr += ergebnis; len -= ergebnis;
                  if (len==0) break; # fertig?
                }
              end_system_call();
              run_time_restart();
            }
          TheStream(stream)->strm_rd_ch_last =
            (len==0 ? code_char(charptr[-1]) : eof_value);
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
          return charptr;
        }
      #endif
      elif (eq(TheStream(stream)->strm_rd_ch,P(rd_ch_ch_file))) # file
        { if (TheStream(stream)->strmflags & strmflags_unread_B)
            { *charptr++ = char_code(lastchar); len--; }
          while (len>0)
            { var uintB* ptr = b_file_nextbyte(stream);
              if (ptr == (uintB*)NULL) break; # EOF -> fertig
             {var uintB ch = *ptr;
              # index und position incrementieren:
              FileStream_index(stream) += 1;
              FileStream_position(stream) += 1;
              # CR/LF -> NL umwandeln:
              if (ch==CR)
                { # nächstes Zeichen auf LF untersuchen
                  ptr = b_file_nextbyte(stream);
                  if (!(ptr == (uintB*)NULL) && (*ptr == LF))
                    { # index und position incrementieren:
                      FileStream_index(stream) += 1;
                      FileStream_position(stream) += 1;
                      ch = NL;
                }   }
              if (ch==NL)
                # lineno incrementieren:
                { FileStream_lineno(stream) += 1; }
              *charptr++ = ch; len--;
            }}
          TheStream(stream)->strm_rd_ch_last =
            (len==0 ? code_char(charptr[-1]) : eof_value);
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
          return charptr;
        }
      elif (eq(TheStream(stream)->strm_rd_ch,P(rd_ch_str_in))) # str_in
        { if (TheStream(stream)->strmflags & strmflags_unread_B)
            { *charptr++ = char_code(lastchar); len--; }
          if (len>0)
            { var uintL index = posfixnum_to_L(TheStream(stream)->strm_str_in_index); # Index
              var uintL endindex = posfixnum_to_L(TheStream(stream)->strm_str_in_endindex);
              if (index < endindex)
                { var uintL srclen;
                  var uintB* srcptr = unpack_string(TheStream(stream)->strm_str_in_string,&srclen);
                  # Ab srcptr kommen srclen Zeichen.
                  if (srclen < endindex) { fehler_str_in_adjusted(stream); }
                  srcptr += index;
                 {var uintL count = endindex - index;
                  if (count > len) { count = len; }
                  # count = min(len,endindex-index) > 0.
                  len -= count;
                  dotimespL(count,count, { *charptr++ = *srcptr++; } );
            }   }}
          TheStream(stream)->strm_rd_ch_last =
            (len==0 ? code_char(charptr[-1]) : eof_value);
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
          return charptr;
        }
      else # keine Optimierung möglich
        { return NULL; }
    }}

# UP: Schreibt mehrere Characters auf einen Stream.
# write_char_array(stream,charptr,len)
# > stream: Stream
# > uintB* charptr: Adresse der zu schreibenden Zeichenfolge
# > uintL len: Länge der zu schreibenden Zeichenfolge
# < uintB* ergebnis: Pointer ans Ende des geschriebenen Bereiches oder NULL
  global const uintB* write_char_array (object stream, const uintB* charptr, uintL len);
  global const uintB* write_char_array(stream,charptr,len)
    var object stream;
    var const uintB* charptr;
    var uintL len;
    { if (len==0) { return charptr; }
      start:
      if (eq(TheStream(stream)->strm_wr_ch,P(wr_ch_synonym))) # synonym
        { stream = get_synonym_stream(TheStream(stream)->strm_synonym_symbol);
          goto start;
          # Line-Position aktualisieren kann hier entfallen.
        }
      elif (eq(TheStream(stream)->strm_wr_ch,P(wr_ch_twoway))) # twoway, echo
        { stream = TheStream(stream)->strm_twoway_output;
          goto start;
          # Line-Position aktualisieren kann hier entfallen.
        }
      #ifdef XHANDLES
      elif (eq(TheStream(stream)->strm_wr_ch,P(wr_ch_handle))) # handle, pipe_out, socket
        { return write_char_array_handle(stream,charptr,len); }
      #endif
      #if (defined(X11SOCKETS) || defined(SOCKET_STREAMS)) && defined(WIN32_NATIVE)
      elif (eq(TheStream(stream)->strm_wr_ch,P(wr_ch_socket))) # socket
        { return write_char_array_socket(stream,charptr,len); }
      #endif
      elif (eq(TheStream(stream)->strm_wr_ch,P(wr_ch_ch_file))) # file
        { return write_char_array_ch_file(stream,charptr,len); }
      else # keine Optimierung möglich
        { return NULL; }
    }

LISPFUN(read_byte,1,2,norest,nokey,0,NIL)
# (READ-BYTE stream [eof-error-p [eof-value]]), CLTL S. 382
  { # Stream überprüfen:
    var object stream = STACK_2;
    if (!streamp(stream)) { fehler_stream(stream); }
    # Integer lesen:
   {var object obj = read_byte(stream);
    if (eq(obj,eof_value))
      # EOF-Behandlung
      { if (!nullp(STACK_1)) # eof-error-p /= NIL (z.B. = #<UNBOUND>) ?
          # Error melden:
          { pushSTACK(STACK_2); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(STACK_(2+1)); # Stream
            pushSTACK(S(read_byte));
            fehler(end_of_file,
                   DEUTSCH ? "~: Eingabestream ~ ist zu Ende." :
                   ENGLISH ? "~: input stream ~ has reached its end" :
                   FRANCAIS ? "~ : Le «stream» d'entrée ~ est épuisé." :
                   ""
                  );
          }
          else
          # EOF verarzten:
          { var object eofval = STACK_0;
            if (eq(eofval,unbound)) { eofval = eof_value; } # Default ist #<EOF>
            value1 = eofval; mv_count=1; skipSTACK(3); # eofval als Wert
          }
      }
      else
      { value1 = obj; mv_count=1; skipSTACK(3); } # obj als Wert
  }}

LISPFUNN(write_byte,2)
# (WRITE-BYTE integer stream), CLTL S. 385
  { # Stream überprüfen:
    var object stream = STACK_0;
    if (!streamp(stream)) { fehler_stream(stream); }
   {# Integer überprüfen:
    var object obj = STACK_1;
    if (!integerp(obj)) { fehler_wr_integer(stream,obj); }
    # Integer schreiben:
    write_byte(stream,obj);
    value1 = STACK_1; mv_count=1; skipSTACK(2); # obj als Wert
  }}

# UP: Überprüft, ob ein Argument ein offener File-Stream ist.
# check_open_file_stream(obj);
# > obj: Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: offener File-Stream
  local object check_open_file_stream (object obj);
  local object check_open_file_stream(obj)
    var object obj;
    { loop
        { if (!streamp(obj)) goto fehler_bad_obj; # Stream ?
          if (TheStream(obj)->strmtype == strmtype_synonym) # Synonym-Stream verfolgen
            { var object sym = TheStream(obj)->strm_synonym_symbol;
              obj = Symbol_value(sym);
            }
          else
            break;
        }
      if_strm_bfile_p(obj, ; , goto fehler_bad_obj; ); # Streamtyp File-Stream ?
      if ((TheStream(obj)->strmflags & strmflags_open_B) == 0) goto fehler_bad_obj; # Stream offen ?
      if (nullp(TheStream(obj)->strm_file_handle)) goto fehler_bad_obj; # und Handle /= NIL ?
      return obj; # ja -> OK
      fehler_bad_obj:
        pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_open_file_stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(obj);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               DEUTSCH ? "~: Argument muss ein offener File-Stream sein, nicht ~" :
               ENGLISH ? "~: argument ~ is not an open file stream" :
               FRANCAIS ? "~ : L'argument ~ doit être un «stream» ouvert sur un fichier." :
               ""
              );
    }

LISPFUN(file_position,1,1,norest,nokey,0,NIL)
# (FILE-POSITION file-stream [position]), CLTL S. 425
  { var object position = popSTACK();
    var object stream = popSTACK();
    stream = check_open_file_stream(stream); # stream überprüfen
    if (eq(position,unbound))
      # position nicht angegeben -> Position als Wert:
      { value1 = UL_to_I(FileStream_position(stream)); mv_count=1; }
      else
      { if (eq(position,S(Kstart)))
          # :START -> an den Anfang positionieren:
          { position_file_start(stream); }
        elif (eq(position,S(Kend)))
          # :END -> ans Ende positionieren:
          { position_file_end(stream); }
        elif (uint32_p(position))
          # an die angegebene Position positionieren:
          { position_file(stream,I_to_UL(position)); }
        else
          # Unzulässiges Position-Argument
          { pushSTACK(position); # Wert für Slot DATUM von TYPE-ERROR
            pushSTACK(O(type_position)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
            pushSTACK(position); pushSTACK(S(Kend)); pushSTACK(S(Kstart));
            pushSTACK(TheSubr(subr_self)->name);
            fehler(type_error,
                   DEUTSCH ? "~: Position-Argument muss ~ oder ~ oder ein Integer >=0 sein, nicht ~" :
                   ENGLISH ? "~: position argument should be ~ or ~ or a nonnegative integer, not ~" :
                   FRANCAIS ? "~ : L'argument position doit être ~, ~ ou un entier positif ou zéro, mais non ~." :
                   ""
                  );
          }
        value1 = T; mv_count=1; # Wert T
      }
  }

LISPFUNN(file_length,1)
# (FILE-LENGTH file-stream), CLTL S. 425
  { var object stream = popSTACK();
    stream = check_open_file_stream(stream); # stream überprüfen
    # Position merken:
   {var uintL position = FileStream_position(stream);
    # ans Ende positionieren:
    position_file_end(stream);
    # Ende-Position merken:
    {var uintL endposition = FileStream_position(stream);
     # an die alte Position zurückpositionieren:
     position_file(stream,position);
     value1 = UL_to_I(endposition); mv_count=1; # Ende-Position als Wert
  }}}

LISPFUNN(file_string_length,2)
# (FILE-STRING-LENGTH stream object)
  { var object stream = check_open_file_stream(STACK_1); # stream überprüfen
    var object obj = STACK_0;
    skipSTACK(2);
    if (!(TheStream(stream)->strmflags & strmflags_wr_ch_B))
      { fehler_illegal_streamop(S(file_string_length),stream); }
    if (eq(TheStream(stream)->strm_wr_ch,P(wr_ch_ch_file)))
      { # Possibly take into account the NL -> CR/LF translation.
        #if defined(MSDOS) || defined(WIN32) || (defined(UNIX) && (O_BINARY != 0))
        if (stringp(obj))
          { var uintL len;
            var uintB* charptr = unpack_string(obj,&len);
            var uintL result = len;
            var uintL count;
            dotimesL(count,len, { if (*charptr++ == NL) result++; } );
            value1 = UL_to_I(result); mv_count=1; return;
          }
        elif (charp(obj))
          { var uintL result = 1;
            if (char_code(obj) == NL) result++;
            value1 = fixnum(result); mv_count=1; return;
          }
        #else
        if (stringp(obj))
          { var uintL result = vector_length(obj);
            value1 = fixnum(result); mv_count=1; return;
          }
        elif (charp(obj))
          { value1 = fixnum(1); mv_count=1; return; }
        #endif
        else
          { fehler_wr_char(stream,obj); }
      }
    else # Shouldn't happen, since all cases have been covered.
      { value1 = NIL; mv_count=1; return; }
  }

# UP: Returns the line current number of a stream.
# stream_line_number(stream)
# > stream: a stream
# < result: an integer or NIL
  global object stream_line_number (object stream);
  global object stream_line_number(stream)
    var object stream;
    { return (TheStream(stream)->strmtype == strmtype_ch_file
              ? UL_to_I(FileStream_lineno(stream)) # aktuelle Zeilennummer
              : NIL                                # NIL falls unbekannt
             );
    }

LISPFUNN(line_number,1)
# (SYS::LINE-NUMBER stream) liefert die aktuelle Zeilennummer (falls stream
# ein Character-File-Input-Stream ist, von dem nur gelesen wurde).
  { var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); } # stream überprüfen
    value1 = stream_line_number(stream); mv_count=1;
  }

LISPFUN(allow_read_eval,1,1,norest,nokey,0,NIL)
# (SYS::ALLOW-READ-EVAL stream) returns the stream's READ-EVAL flag.
# (SYS::ALLOW-READ-EVAL stream flag) sets the stream's READ-EVAL flag.
# T means #. is allowed regardless of the value of *READ-EVAL*, NIL
# (the default) means that *READ-EVAL* is respected.
  { var object flag = popSTACK();
    var object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); } # stream überprüfen
    if (eq(flag,unbound))
      { value1 = ((TheStream(stream)->strmflags & strmflags_reval_B) == 0 ? NIL : T); }
      else
      { if (nullp(flag))
          { TheStream(stream)->strmflags &= ~strmflags_reval_B; value1 = NIL; }
          else
          { TheStream(stream)->strmflags |= strmflags_reval_B; value1 = T; }
      }
    mv_count=1;
  }

# ==============================================================================

# Binärkompatibilität zwischen .mem-Files mit und ohne NEXTAPP erreichen:
  #ifdef MAYBE_NEXTAPP
    #ifndef NEXTAPP
      #define wr_ch_terminal  wr_ch_dummy
      #define rd_ch_terminal  rd_ch_dummy
    #else
      #define wr_ch_terminal1  wr_ch_dummy
      #define rd_ch_terminal1  rd_ch_dummy
      #define wr_ss_terminal1  wr_ss_dummy_nogc
    #endif
    #ifndef GNU_READLINE
      #define wr_ch_terminal3  wr_ch_dummy
      #define rd_ch_terminal3  rd_ch_dummy
      #define wr_ss_terminal3  wr_ss_dummy_nogc
    #endif
    #ifdef NEXTAPP
      #define wr_ch_window  wr_ch_dummy
    #endif
  #endif

# Tabelle aller Pseudofunktionen
  global struct pseudofun_tab_ pseudofun_tab =
    {
      #define PSEUDOFUN  PSEUDOFUN_B
      #include "pseudofun.c"
      #undef PSEUDOFUN
    };

# ==============================================================================

#ifdef EMUNIX_PORTABEL

# Eine Hilfsfunktion für bidirektionale Pipes: popenrw()
#undef stdin_handle
#undef stdout_handle
#include "../os2/popenrw.c"

#endif

# ==============================================================================

# filestatus/if_file_exists, file_datetime durch break_sem_4 schützen??
# Signalbehandlung bei EXECUTE, SHELL, MAKE-PIPE-INPUT-STREAM, MAKE-PIPE-OUTPUT-STREAM, MAKE-PIPE-IO-STREAM ??

