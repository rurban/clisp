# Streams für CLISP
# Bruno Haible 1990-1999
# Generic Streams: Marcus Daniels 8.4.1994

#include "lispbibl.c"
#include "arilev0.c" # für R_sign

#ifdef GNU_READLINE
  #define READLINE_LIBRARY # Hinweis, wo die Include-Files gesucht werden müssen
  #include "readline.h"
  #include "history.h"
  #undef READLINE_LIBRARY
#endif
#ifdef STDC_HEADERS
  #include <string.h>  # declares strcpy(), strcat()
#endif


# Nochmals zum Aufbau von Streams:
# strmflags = Flags
  # Bits in den Flags:
  # define strmflags_immut_bit_B 1  # set if read literals are immutable
  # define strmflags_reval_bit_B 2  # gesetzt, falls Read-Eval erlaubt ist
  #define strmflags_unread_bit_B 3  # gesetzt, während strm_rd_ch_last zurück ist
  #define strmflags_rd_by_bit_B  4  # gesetzt, falls READ-BYTE möglich ist
  #define strmflags_wr_by_bit_B  5  # gesetzt, falls WRITE-BYTE möglich ist
  # define strmflags_rd_ch_bit_B 6  # gesetzt, falls READ-CHAR möglich ist
  # define strmflags_wr_ch_bit_B 7  # gesetzt, falls WRITE-CHAR möglich ist
  # Bitmasken in den Flags:
  #define strmflags_immut_B  bit(strmflags_immut_bit_B)
  #define strmflags_reval_B  bit(strmflags_reval_bit_B)
  #define strmflags_unread_B bit(strmflags_unread_bit_B)
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

# individual fields:
  # strm_rd_by         pseudofunction for READ-BYTE
  # strm_rd_by_array   pseudofunction for READ-BYTE-SEQUENCE
  # strm_wr_by         pseudofunction for WRITE-BYTE
  # strm_wr_by_array   pseudofunction for WRITE-BYTE-SEQUENCE
  # strm_rd_ch         pseudofunction for READ-CHAR
  # strm_pk_ch         pseudofunction for PEEK-CHAR
  # strm_rd_ch_array   pseudofunction for READ-CHAR-SEQUENCE
  # strm_rd_ch_last    last character read by READ-CHAR, NIL if none has been
  #                    read upto now, eof_value after EOF has been seen.
  #                    After UNREAD-CHAR, additionally the bit
  #                    strmflags_unread_bit_B is set.
  # strm_wr_ch         pseudofunction for WRITE-CHAR
  # strm_wr_ch_array   pseudofunction for WRITE-CHAR-SEQUENCE
  # strm_wr_ch_lpos    line-position in the current line after last WRITE-CHAR,
  #                    a fixnum >=0
# weitere (typspezifische) Komponenten:
  # siehe in LISPBIBL.D und bei den einzelnen Stream-Typen.


# =============================================================================
#                           S T R E A M S

# Da MAKE-TWO-WAY-STREAM eventuell einen Stream liefern kann, der z.B.
# Character-Input und Byte-Output ist, und damit insbesondere alle
# READ-/WRITE-Operationen effizient laufen, werden Streams folgendermaßen
# aufgebaut:
#    - Typ des Streams,
#    - Komponenten für READ-BYTE, READ-BYTE-SEQUENCE,
#    - Komponenten für WRITE-BYTE, WRITE-BYTE-SEQUENCE,
#    - Komponenten für READ-CHAR, READ-CHAR-SEQUENCE,
#    - Komponenten für WRITE-CHAR, WRITE-CHAR-SEQUENCE,
#    - vom Typ des Streams abhängige Komponenten.

# Spezifikation der neun Typen von Pseudofunktionen:
  #
  # Spezifikation für READ-BYTE - Pseudofunktion:
  # fun(stream)
  # > stream: Stream
  # < ergebnis: gelesener Integer (eof_value bei EOF)
  # kann GC auslösen
    typedef object (* rd_by_Pseudofun) (object stream);
  #
  # Spezifikation für READ-BYTE-ARRAY - Pseudofunktion:
  # fun(&stream,&bytearray,start,len)
  # > stream: stream
  # > object bytearray: simple-bit-vector
  # > uintL start: start index of byte sequence to be filled
  # > uintL len: length of byte sequence to be filled, >0
  # < uintL result: number of bytes that have been filled
  # can trigger GC
    typedef uintL (* rd_by_array_Pseudofun) (const object* stream_, const object* bytearray_, uintL start, uintL len);
  #
  # Spezifikation für WRITE-BYTE - Pseudofunktion:
  # fun(stream,obj)
  # > stream: Stream
  # > obj: auszugebender Integer
  # kann GC auslösen
    typedef void (* wr_by_Pseudofun) (object stream, object obj);
  #
  # Spezifikation für WRITE-BYTE-ARRAY - Pseudofunktion:
  # fun(&stream,&bytearray,start,len)
  # > stream: stream
  # > object bytearray: simple-bit-vector
  # > uintL start: start index of byte sequence to be written
  # > uintL len: length of byte sequence to be written, >0
  # can trigger GC
    typedef void (* wr_by_array_Pseudofun) (const object* stream_, const object* bytearray_, uintL start, uintL len);
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
  # Spezifikation für READ-CHAR-ARRAY - Pseudofunktion:
  # fun(&stream,&chararray,start,len)
  # > stream: stream
  # > object chararray: mutable simple-string
  # > uintL start: start index of character sequence to be filled
  # > uintL len: length of character sequence to be filled, >0
  # < uintL result: number of characters that have been filled
  # can trigger GC
    typedef uintL (* rd_ch_array_Pseudofun) (const object* stream_, const object* chararray_, uintL start, uintL len);
  #
  # Spezifikation für WRITE-CHAR - Pseudofunktion:
  # fun(&stream,obj)
  # > stream: Stream
  # < stream: Stream
  # > obj: auszugebendes Character
  # kann GC auslösen
    typedef void (* wr_ch_Pseudofun) (const object* stream_, object obj);
  #
  # Spezifikation für WRITE-CHAR-ARRAY - Pseudofunktion:
  # fun(&stream,&chararray,start,len)
  # > stream: stream 
  # > object chararray: simple-string
  # > uintL start: start index of character sequence to be written
  # > uintL len: length of character sequence to be written, >0
    typedef void (* wr_ch_array_Pseudofun) (const object* stream_, const object* chararray_, uintL start, uintL len);

# Pseudofunktionen aus einem Stream herausgreifen:
  #define rd_by(strm)  (*(rd_by_Pseudofun)(ThePseudofun(TheStream(strm)->strm_rd_by)))
  #define rd_by_array(strm)  (*(rd_by_array_Pseudofun)(ThePseudofun(TheStream(strm)->strm_rd_by_array)))
  #define wr_by(strm)  (*(wr_by_Pseudofun)(ThePseudofun(TheStream(strm)->strm_wr_by)))
  #define wr_by_array(strm)  (*(wr_by_array_Pseudofun)(ThePseudofun(TheStream(strm)->strm_wr_by_array)))
  #define rd_ch(strm)  (*(rd_ch_Pseudofun)(ThePseudofun(TheStream(strm)->strm_rd_ch)))
  #define pk_ch(strm)  (*(pk_ch_Pseudofun)(ThePseudofun(TheStream(strm)->strm_pk_ch)))
  #define rd_ch_array(strm)  (*(rd_ch_array_Pseudofun)(ThePseudofun(TheStream(strm)->strm_rd_ch_array)))
  #define wr_ch(strm)  (*(wr_ch_Pseudofun)(ThePseudofun(TheStream(strm)->strm_wr_ch)))
  #define wr_ch_array(strm)  (*(wr_ch_array_Pseudofun)(ThePseudofun(TheStream(strm)->strm_wr_ch_array)))

#  Mögliche Typen von Streams              Zusatzkomponenten
#  --------------------------              -----------------
#
#  Synonym-Stream                          Symbol
#  Broadcast-(Output-)Stream               Liste von Streams
#  Concatenated-(Input-)Stream             Liste von Streams
#  Two-Way-Stream                          Stream für Input, Stream für Output
#  Echo-Stream                             Stream für Input, Stream für Output
#  String-Input-Stream                     Gesamtstring, Zeichenzähler
#  String-Output-Stream                    Buffer (Semi-Simple-String)
#  String-Push-Stream                      String mit Fill-Pointer
#  Pretty-Printer-Hilfs-Stream             Liste von Buffers, Modus
#  Buffered-Input-Stream                   fun, mode, String, Zeichenzähler
#  Buffered-Output-Stream                  fun, Buffer (Semi-Simple-String)
#ifdef GENERIC_STREAMS
#  Generic-Stream                          Private Controller Object
#endif
#
#  Keyboard-Stream
#  Interaktiver Terminalstream             Eingabebuffer, Zeichenzähler
#  File-Stream                             Handle, Pathname, File-Position,
#  (Input, Output, I/O, Closed=Probe)      Buffer, [Bit-Buffer]
#  Window-Stream                           ---
#ifdef PRINTER
#  Printer-Stream
#endif
#  File-Handle-Stream                      Handle, Pathname
#ifdef PIPES
#  Pipe-Input-Stream                       Pid, Handle
#  Pipe-Output-Stream                      Pid, Handle
#endif
#ifdef X11SOCKETS
#  X11-Socket-Stream                       Info, Handle
#endif
#ifdef SOCKET_STREAMS
#  Socket-Stream                           Host, Port
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
    {
      pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(stream);
      pushSTACK(caller);
      fehler(stream_error,
             GETTEXT("~ on ~ is illegal")
            );
    }

# Dummy-Pseudo-Funktionen, die Errors liefern:

  local object rd_by_error (object stream);
  local object rd_by_error(stream)
    var object stream;
    {
      fehler_illegal_streamop(S(read_byte),stream);
    }

  local uintL rd_by_array_error (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local uintL rd_by_array_error(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      fehler_illegal_streamop(S(read_byte),*stream_);
    }

  local uintL rd_by_array_dummy (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local uintL rd_by_array_dummy(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      var uintL end = start + len;
      var uintL index = start;
      do {
        var object stream = *stream_;
        var object obj = rd_by(stream)(stream);
        if (eq(obj,eof_value))
          break;
        if (!uint8_p(obj))
          fehler_uint8(obj);
        TheSbvector(*bytearray_)->data[index] = (uintB)(as_oint(obj) >> oint_data_shift);
        index++;
      } while (index < end);
      return index - start;
    }

  local void wr_by_error (object stream, object obj);
  local void wr_by_error(stream,obj)
    var object stream;
    var object obj;
    {
      fehler_illegal_streamop(S(write_byte),stream);
    }

  local void wr_by_array_error (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local void wr_by_array_error(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      fehler_illegal_streamop(S(write_byte),*stream_);
    }

  local void wr_by_array_dummy (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local void wr_by_array_dummy(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      var uintL end = start + len;
      var uintL index = start;
      do {
        var object stream = *stream_;
        wr_by(stream)(stream,fixnum(TheSbvector(*bytearray_)->data[index]));
        index++;
      } while (index < end);
    }

  local object rd_ch_error (const object* stream_);
  local object rd_ch_error(stream_)
    var const object* stream_;
    {
      fehler_illegal_streamop(S(read_char),*stream_);
    }

  local object pk_ch_dummy (const object* stream_);
  local object pk_ch_dummy(stream_)
    var const object* stream_;
    {
      var object newch = rd_ch(*stream_)(stream_);
      TheStream(*stream_)->strm_rd_ch_last = newch;
      if (!eq(newch,eof_value))
        TheStream(*stream_)->strmflags |= strmflags_unread_B;
      return newch;
    }

  local uintL rd_ch_array_error (const object* stream_, const object* chararray_, uintL start, uintL len);
  local uintL rd_ch_array_error(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      fehler_illegal_streamop(S(read_char),*stream_);
    }

  local uintL rd_ch_array_dummy (const object* stream_, const object* chararray_, uintL start, uintL len);
  local uintL rd_ch_array_dummy(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var uintL end = start + len;
      var uintL index = start;
      do {
        var object obj = rd_ch(*stream_)(stream_);
        if (eq(obj,eof_value))
          break;
        if (!charp(obj))
          fehler_char(obj);
        TheSstring(*chararray_)->data[index] = char_code(obj);
        index++;
      } while (index < end);
      return index - start;
    }

  local void wr_ch_error (const object* stream_, object obj);
  local void wr_ch_error(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      fehler_illegal_streamop(S(write_char),*stream_);
    }

  local void wr_ch_array_error (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_error(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      fehler_illegal_streamop(S(write_char),*stream_);
    }

  local void wr_ch_array_dummy (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_dummy(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var uintL end = start + len;
      var uintL index = start;
      SstringDispatch(*chararray_,
        {
          do {
            write_char(stream_,code_char(TheSstring(*chararray_)->data[index]));
            index++;
          } while (index < end);
        },
        {
          do {
            write_char(stream_,code_char(as_chart(TheSmallSstring(*chararray_)->data[index])));
            index++;
          } while (index < end);
        }
        );
    }

  # Am Ende eines wr_ch_array die Line-Position aktualisieren:
  # wr_ss_lpos(stream,ptr,len);
  # > stream: Builtin-Stream, nicht der Terminal-Stream
  # > ptr: Pointer ans Ende(!) der bereits auf den Stream ausgegebenen Zeichen
  # > len: Anzahl der Zeichen, >0
  # < ergebnis: TRUE, falls ein NL unter den Zeichen ist, FALSE sonst
  local boolean wr_ss_lpos (object stream, const chart* ptr, uintL len);
  local boolean wr_ss_lpos(stream,ptr,len)
    var object stream;
    var const chart* ptr;
    var uintL len;
    {
      #ifdef TERMINAL_USES_KEYBOARD
      if (TheStream(stream)->strmtype == strmtype_terminal)
        return FALSE; # Auf dem Atari machte dies wr_ch_terminal() selbst.
      #endif
      # Zähle die Breiten der Zeichen seit dem letzten NL zusammen:
      var boolean result;
      var uintL pos = 0;
      var uintL count;
      dotimespL(count,len, {
        if (chareq(*--ptr,ascii(NL)))
          goto found_NL;
        pos++;
      });
      if (FALSE) {
       found_NL: # pos Zeichen seit dem letzten NL
        ptr++; len = pos; pos = 0; result = TRUE;
      } else { # pos==len
        pos = posfixnum_to_L(TheStream(stream)->strm_wr_ch_lpos); result = FALSE;
      }
      # Es gab len Zeichen ab ptr, pos ist die Position dort.
      #ifdef TERMINAL_USES_KEYBOARD
      pos += len;
      #else
      if (len > 0) {
        if (TheStream(stream)->strmtype == strmtype_terminal) {
          dotimespL(count,len, {
            var chart c = *ptr++;
            # Wie wirken sich die Steuerzeichen in der Position aus?
            if (chareq(c,ascii(BS))) {
              # Backspace -> Line Position, wenn möglich, decrementieren:
              if (pos > 0)
                pos--;
            } else
              pos += char_width(c);
          });
        } else {
          dotimespL(count,len, {
            var chart c = *ptr++;
            pos += char_width(c);
          });
        }
      }
      #endif
      TheStream(stream)->strm_wr_ch_lpos = fixnum(pos);
      return result;
    }

# Liest ein Byte von einem Stream.
# read_byte(stream)
# > stream: Stream
# < ergebnis: gelesener Integer (eof_value bei EOF)
# can trigger GC
  global object read_byte (object stream);
  global object read_byte(stream)
    var object stream;
    {
      if (builtin_stream_p(stream)) {
        return rd_by(stream)(stream);
      } else {
        # Call the generic function (STREAM-READ-BYTE stream):
        pushSTACK(stream); funcall(S(stream_read_byte),1);
        var object result = value1;
        if (eq(result,S(Keof)))
          return eof_value;
        else
          return result;
      }
    }

# Function: Reads several bytes from a stream.
# read_byte_array(&stream,&bytearray,start,len)
# > stream: stream (on the STACK)
# > object bytearray: simple-bit-vector (on the STACK)
# > uintL start: start index of byte sequence to be filled
# > uintL len: length of byte sequence to be filled
# < uintL result: number of bytes that have been filled
# can trigger GC
  global uintL read_byte_array (const object* stream_, const object* bytearray_, uintL start, uintL len);
  global uintL read_byte_array(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      if (len==0)
        return 0;
      var object stream = *stream_;
      if (builtin_stream_p(stream)) {
        return rd_by_array(stream)(stream_,bytearray_,start,len);
      } else {
        # Call the generic function (STREAM-READ-BYTE-SEQUENCE stream bytearray start start+len):
        pushSTACK(stream); pushSTACK(*bytearray_); pushSTACK(fixnum(start)); pushSTACK(fixnum(start+len));
        funcall(S(stream_read_byte_sequence),4);
        var uintL result;
        if (!(posfixnump(value1)
              && (result = posfixnum_to_L(value1),
                  result >= start && result <= start+len
           ) )   ) {
          pushSTACK(fixnum(start+len));
          pushSTACK(fixnum(start));
          pushSTACK(S(stream_read_byte_sequence));
          pushSTACK(value1);
          fehler(error,
                 GETTEXT("Return value ~ of call to ~ should be an integer between ~ and ~.")
                );
        }
        return result-start;
      }
    }

# Schreibt ein Byte auf einen Stream.
# write_byte(stream,byte);
# > stream: Stream
# > byte: auszugebender Integer
# can trigger GC
  global void write_byte (object stream, object byte);
  global void write_byte(stream,byte)
    var object stream;
    var object byte;
    {
      if (builtin_stream_p(stream)) {
        wr_by(stream)(stream,byte);
      } else {
        # Call the generic function (STREAM-WRITE-BYTE stream byte):
        pushSTACK(stream); pushSTACK(byte); funcall(S(stream_write_byte),2);
      }
    }

# Function: Writes several bytes to a stream.
# write_byte_array(&stream,&bytearray,start,len)
# > stream: Stream (on the STACK)
# > object bytearray: simple-bit-vector (on the STACK)
# > uintL start: start index of byte sequence to be written
# > uintL len: length of byte sequence to be written
  global void write_byte_array (const object* stream_, const object* bytearray_, uintL start, uintL len);
  global void write_byte_array(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      if (len==0)
        return;
      var object stream = *stream_;
      if (builtin_stream_p(stream)) {
        wr_by_array(stream)(stream_,bytearray_,start,len);
      } else {
        # Call the generic function (STREAM-WRITE-BYTE-SEQUENCE stream bytearray start start+len):
        pushSTACK(stream); pushSTACK(*bytearray_); pushSTACK(fixnum(start)); pushSTACK(fixnum(start+len));
        funcall(S(stream_write_byte_sequence),4);
      }
    }

# Liest ein Character von einem Stream.
# read_char(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Character (eof_value bei EOF)
# can trigger GC
  global object read_char (const object* stream_);
  global object read_char(stream_)
    var const object* stream_;
    {
      var object stream = *stream_;
      if (builtin_stream_p(stream)) {
        if (!(TheStream(stream)->strmflags & strmflags_unread_B)) { # Char nach UNREAD ?
          # nein -> neues Zeichen holen:
          var object newch = rd_ch(stream)(stream_);
          stream = *stream_;
          TheStream(stream)->strm_rd_ch_last = newch; # und abspeichern
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
          return newch;
        } else {
          # ja -> Flagbit löschen und letztes Zeichen holen:
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
          return TheStream(stream)->strm_rd_ch_last;
        }
      } else {
        # Call the generic function (STREAM-READ-CHAR stream):
        pushSTACK(stream); funcall(S(stream_read_char),1);
        var object result = value1;
        if (eq(result,S(Keof)))
          return eof_value;
        else
          return result;
      }
    }

# Schiebt das letzte gelesene Character auf einen Stream zurück.
# unread_char(&stream,ch);
# > ch: letztes gelesenes Character
# > stream: Stream
# < stream: Stream
  global void unread_char (const object* stream_, object ch);
  global void unread_char(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      var object stream = *stream_;
      if (builtin_stream_p(stream)) {
        if (eq(TheStream(stream)->strm_rd_ch_last,ch)
            && !(TheStream(stream)->strmflags & strmflags_unread_B)
           ) {
          TheStream(stream)->strmflags |= strmflags_unread_B; # Flagbit setzen
        } else {
          if (!nullp(TheStream(stream)->strm_rd_ch_last)
              && !(TheStream(stream)->strmflags & strmflags_unread_B)
             ) {
            pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(ch);
            pushSTACK(stream);
            pushSTACK(S(unread_char));
            fehler(stream_error,
                   GETTEXT("~: the last character read from ~ was not ~")
                  );
          } else {
            pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(S(read_char));
            pushSTACK(stream);
            pushSTACK(S(unread_char));
            fehler(stream_error,
                   GETTEXT("~ from ~ without ~ before it")
                  );
          }
        }
      } else {
        # Call the generic function (STREAM-UNREAD-CHAR stream ch):
        pushSTACK(stream); pushSTACK(ch); funcall(S(stream_unread_char),2);
      }
    }

# Liest ein Character von einem Stream, ohne es zu verbrauchen.
# peek_char(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Character (eof_value bei EOF)
# can trigger GC
  global object peek_char (const object* stream_);
  global object peek_char(stream_)
    var const object* stream_;
    {
      var object stream = *stream_;
      if (builtin_stream_p(stream)) {
        if (!(TheStream(stream)->strmflags & strmflags_unread_B)) # Char nach UNREAD ?
          # nein -> neues Zeichen holen:
          return pk_ch(stream)(stream_);
        else
          # ja -> letztes Zeichen holen:
          return TheStream(stream)->strm_rd_ch_last;
      } else {
        # Call the generic function (STREAM-PEEK-CHAR stream):
        pushSTACK(stream); funcall(S(stream_peek_char),1);
        var object result = value1;
        if (eq(result,S(Keof)))
          return eof_value;
        else
          return result;
      }
    }

# Function: Reads several characters from a stream.
# read_char_array(&stream,&chararray,start,len)
# > stream: stream (on the STACK)
# > object chararray: mutable simple-string (on the STACK)
# > uintL start: start index of character sequence to be filled
# > uintL len: length of character sequence to be filled
# < uintL result: number of characters that have been filled
# can trigger GC
  global uintL read_char_array (const object* stream_, const object* chararray_, uintL start, uintL len);
  global uintL read_char_array(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      if (len==0)
        return 0;
      var object stream = *stream_;
      if (builtin_stream_p(stream)) {
        var object lastchar = TheStream(stream)->strm_rd_ch_last;
        if (eq(lastchar,eof_value)) # EOF ?
          return 0;
        var uintL index = start;
        if (TheStream(stream)->strmflags & strmflags_unread_B) {
          if (!charp(lastchar))
            fehler_char(lastchar);
          TheSstring(*chararray_)->data[index++] = char_code(lastchar);
          len--;
          if (len==0) {
            TheStream(stream)->strmflags &= ~strmflags_unread_B;
            return 1;
          }
        }
        var uintL count = rd_ch_array(stream)(stream_,chararray_,index,len);
        index += count;
        stream = *stream_;
        TheStream(stream)->strm_rd_ch_last =
          (count == len ? code_char(TheSstring(*chararray_)->data[index-1]) : eof_value);
        TheStream(stream)->strmflags &= ~strmflags_unread_B;
        return index - start;
      } else {
        # Call the generic function (STREAM-READ-CHAR-SEQUENCE stream chararray start start+len):
        pushSTACK(stream); pushSTACK(*chararray_); pushSTACK(fixnum(start)); pushSTACK(fixnum(start+len));
        funcall(S(stream_read_char_sequence),4);
        var uintL result;
        if (!(posfixnump(value1)
              && (result = posfixnum_to_L(value1),
                  result >= start && result <= start+len
           ) )   ) {
          pushSTACK(fixnum(start+len));
          pushSTACK(fixnum(start));
          pushSTACK(S(stream_read_char_sequence));
          pushSTACK(value1);
          fehler(error,
                 GETTEXT("Return value ~ of call to ~ should be an integer between ~ and ~.")
                );
        }
        return result-start;
      }
    }

# Schreibt ein Character auf einen Stream.
# write_char(&stream,ch);
# > ch: auszugebendes Character
# > stream: Stream
# < stream: Stream
# can trigger GC
  global void write_char (const object* stream_, object ch);
  global void write_char(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      var object stream = *stream_;
      if (builtin_stream_p(stream)) {
        var chart c = char_code(ch);
        # Char schreiben:
        wr_ch(stream)(stream_,ch);
        # Line Position aktualisieren:
        var object stream = *stream_;
        if (!(TheStream(stream)->strmtype == strmtype_terminal)) {
          # nicht der Terminal-Stream
          if (chareq(c,ascii(NL)))
            # Nach Newline: Line Position := 0
            TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
          else
            # Line Position incrementieren:
            TheStream(stream)->strm_wr_ch_lpos =
              fixnum_inc(TheStream(stream)->strm_wr_ch_lpos,char_width(c));
        } else {
          # es ist der Terminal-Stream
          #ifdef TERMINAL_USES_KEYBOARD
          ; # Auf dem Atari machte dies wr_ch_terminal() selbst.
          #else
          # Wie wirken sich die Steuerzeichen in der Position aus?
          if (chareq(c,ascii(NL))) {
            # Newline -> Line Position := 0
            TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
          } elif (chareq(c,ascii(BS))) {
            # Backspace -> Line Position, wenn möglich, decrementieren:
            if (!eq(TheStream(stream)->strm_wr_ch_lpos,Fixnum_0))
              TheStream(stream)->strm_wr_ch_lpos =
                fixnum_inc(TheStream(stream)->strm_wr_ch_lpos,-1);
          } else
            # Line Position incrementieren:
            TheStream(stream)->strm_wr_ch_lpos =
              fixnum_inc(TheStream(stream)->strm_wr_ch_lpos,char_width(c));
          #endif
        }
      } else {
        # Call the generic function (STREAM-WRITE-CHAR stream ch):
        pushSTACK(stream); pushSTACK(ch); funcall(S(stream_write_char),2);
      }
    }

# Function: Writes several characters to a stream.
# write_char_array(&stream,&chararray,start,len)
# > stream: stream (on the STACK)
# > object chararray: simple-string (on the STACK)
# > uintL start: start index of character sequence to be written
# > uintL len: length of character sequence to be written
  global void write_char_array (const object* stream_, const object* chararray_, uintL start, uintL len);
  global void write_char_array(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      if (len==0)
        return;
      var object stream = *stream_;
      if (builtin_stream_p(stream)) {
        wr_ch_array(stream)(stream_,chararray_,start,len);
      } else {
        # Call the generic function (STREAM-WRITE-CHAR-SEQUENCE stream chararray start start+len):
        pushSTACK(stream); pushSTACK(*chararray_); pushSTACK(fixnum(start)); pushSTACK(fixnum(start+len));
        funcall(S(stream_write_char_sequence),4);
      }
    }

# UP: Füllt beim Schließen eines Streams die Dummy-Pseudofunktionen ein.
# close_dummys(stream);
# > stream: Stream
  local void close_dummys (object stream);
  local void close_dummys(stream)
    var object stream;
    {
      TheStream(stream)->strm_rd_by = P(rd_by_error);
      TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
      TheStream(stream)->strm_wr_by = P(wr_by_error);
      TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
      TheStream(stream)->strm_rd_ch = P(rd_ch_error);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_error);
      TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
      TheStream(stream)->strm_wr_ch = P(wr_ch_error);
      TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_error);
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
    {
      var object result = Symbol_value(sym);
      var object stream;
     recurse:
      stream = Symbol_value(sym);
      if (builtin_stream_p(stream)) {
        if (strmflags & ~ TheStream(stream)->strmflags)
          fehler_value_stream(sym);
        if (TheStream(stream)->strmtype == strmtype_synonym) {
          sym = TheStream(stream)->strm_synonym_symbol;
          goto recurse;
        }
      } elif (instanceof(stream,O(class_fundamental_stream))) {
        # Among instances of FUNDAMENTAL-STREAM:
        # Only instances of FUNDAMENTAL-INPUT-STREAM can do input.
        # Only instances of FUNDAMENTAL-OUTPUT-STREAM can do output.
        if (((strmflags & strmflags_rd_B)
             && !instanceof(stream,O(class_fundamental_input_stream))
            )
            ||
            ((strmflags & strmflags_wr_B)
             && !instanceof(stream,O(class_fundamental_output_stream))
            )
           )
          fehler_value_stream(sym);
      } else {
        fehler_value_stream(sym);
      }
      return result;
    }

# (SYSTEM::SYMBOL-STREAM symbol [direction])
# liefert den Stream, der der Wert des Symbols ist, und überprüft, ob es ein
# offener Stream der Richtung direction (:PROBE, :INPUT, :OUTPUT oder :IO) ist.
LISPFUN(symbol_stream,1,1,norest,nokey,0,NIL)
  {
    var object direction = popSTACK();
    var object symbol = popSTACK();
    if (!symbolp(symbol))
      fehler_symbol(symbol);
    value1 = var_stream(symbol,
                        eq(direction,S(Kinput)) ? strmflags_rd_ch_B : # :INPUT
                        eq(direction,S(Koutput)) ? strmflags_wr_ch_B : # :OUTPUT
                        eq(direction,S(Kio)) ? strmflags_rd_ch_B | strmflags_wr_ch_B : # :IO
                        0 # :PROBE oder nicht angegeben
                       );
    mv_count=1;
  }

# Fehler, wenn aus einem obskuren Grunde ein WRITE nicht gehen sollte:
  nonreturning_function(local, fehler_unwritable, (object caller, object stream));
  local void fehler_unwritable(caller,stream)
    var object caller;
    var object stream;
    {
      pushSTACK(stream); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(stream);
      pushSTACK(caller);
      fehler(file_error,
             GETTEXT("~: cannot output to ~")
            );
    }

# Fehler, wenn ein Objekt kein Character ist:
# fehler_wr_char(stream,obj);
  nonreturning_function(local, fehler_wr_char, (object stream, object obj));
  local void fehler_wr_char(stream,obj)
    var object stream;
    var object obj;
    {
      pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(character)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(stream);
      pushSTACK(obj);
      fehler(type_error,
             GETTEXT("~ is not a character, cannot be output onto ~")
            );
    }

# Fehler, wenn ein Objekt kein Integer ist:
# fehler_wr_integer(stream,obj);
  nonreturning_function(local, fehler_wr_integer, (object stream, object obj));
  local void fehler_wr_integer(stream,obj)
    var object stream;
    var object obj;
    {
      pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(integer)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(stream);
      pushSTACK(obj);
      fehler(type_error,
             GETTEXT("~ is not an integer, cannot be output onto ~")
            );
    }

# Fehler, wenn ein Integer nicht im passenden Bereich ist:
# fehler_bad_integer(stream,obj);
  nonreturning_function(local, fehler_bad_integer, (object stream, object obj));
  local void fehler_bad_integer(stream,obj)
    var object stream;
    var object obj;
    {
      pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(stream);
      pushSTACK(obj);
      fehler(stream_error,
             GETTEXT("integer ~ is out of range, cannot be output onto ~")
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
    {
      dotimesC(argcount,argcount, {
        var object next_arg = NEXT(args_pointer);
        if (!streamp(next_arg))
          fehler_stream(next_arg);
      });
    }

# Macro: Tests whether an object is an input-stream.
# input_stream_p(stream)
# > stream: object
  #define input_stream_p(stream)  \
    (builtin_stream_p(stream)                                  \
     ? !((TheStream(stream)->strmflags & strmflags_rd_B) == 0) \
     : instanceof(stream,O(class_fundamental_input_stream))    \
    )

# Macro: Tests whether an object is an output-stream.
# output_stream_p(stream)
# > stream: object
  #define output_stream_p(stream)  \
    (builtin_stream_p(stream)                                  \
     ? !((TheStream(stream)->strmflags & strmflags_wr_B) == 0) \
     : instanceof(stream,O(class_fundamental_output_stream))   \
    )

# UP: Überprüft einen Input-Stream.
# test_input_stream(stream);
# > stream: Stream
# > subr_self: Aufrufer (ein SUBR)
  #define test_input_stream(stream)  \
    if (!input_stream_p(stream)) fehler_input_stream(stream);
  nonreturning_function(local, fehler_input_stream, (object stream));
  local void fehler_input_stream(stream)
    var object stream;
    {
      pushSTACK(stream); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_input_stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(stream); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ should be an input stream")
            );
    }

# UP: Überprüft einen Output-Stream.
# test_output_stream(stream);
# > stream: Stream
# > subr_self: Aufrufer (ein SUBR)
  #define test_output_stream(stream)  \
    if (!output_stream_p(stream)) fehler_output_stream(stream);
  nonreturning_function(local, fehler_output_stream, (object stream));
  local void fehler_output_stream(stream)
    var object stream;
    {
      pushSTACK(stream); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_output_stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(stream); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ should be an output stream")
            );
    }

# UP: Überprüft Argumente, ob sie Input-Streams sind.
# test_input_stream_args(args_pointer,argcount);
# > args_pointer: Pointer über die Argumente
# > argcount: Anzahl der Argumente
# > subr_self: Aufrufer (ein SUBR)
  #define test_input_stream_args(args_pointer,argcount)  \
    if (argcount > 0) {                     \
      var object* pointer = (args_pointer); \
      var uintC count;                      \
      dotimespC(count,argcount, {           \
        var object arg = NEXT(pointer);     \
        if (!streamp(arg))                  \
          fehler_stream(arg);               \
        test_input_stream(arg);             \
      });                                   \
    }

# UP: Überprüft Argumente, ob sie Output-Streams sind.
# test_output_stream_args(args_pointer,argcount);
# > args_pointer: Pointer über die Argumente
# > argcount: Anzahl der Argumente
# > subr_self: Aufrufer (ein SUBR)
  #define test_output_stream_args(args_pointer,argcount)  \
    if (argcount > 0) {                     \
      var object* pointer = (args_pointer); \
      var uintC count;                      \
      dotimespC(count,argcount, {           \
        var object arg = NEXT(pointer);     \
        if (!streamp(arg))                  \
          fehler_stream(arg);               \
        test_output_stream(arg);            \
      });                                   \
    }


# Synonym-Stream
# ==============

# Zusätzliche Komponenten:
  # define strm_synonym_symbol  strm_other[0]  # Symbol, auf dessen Wert verwiesen wird

# Macro: Liefert den Wert eines Symbols, ein Stream.
# get_synonym_stream(sym)
# > sym: Symbol, a variable
# < ergebnis: sein Wert, ein Stream
  #define get_synonym_stream(sym)  \
    (!streamp(Symbol_value(sym))           \
     ? (fehler_value_stream(sym), unbound) \
     : Symbol_value(sym)                   \
    )

# READ-BYTE - Pseudofunktion für Synonym-Streams:
  local object rd_by_synonym (object stream);
  local object rd_by_synonym(stream)
    var object stream;
    {
      check_SP();
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      return read_byte(get_synonym_stream(symbol));
    }

# READ-BYTE-ARRAY - Pseudofunktion für Synonym-Streams:
  local uintL rd_by_array_synonym (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local uintL rd_by_array_synonym(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      var object symbol = TheStream(*stream_)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      var uintL result = read_byte_array(&STACK_0,bytearray_,start,len);
      skipSTACK(1);
      return result;
    }

# WRITE-BYTE - Pseudofunktion für Synonym-Streams:
  local void wr_by_synonym (object stream, object obj);
  local void wr_by_synonym(stream,obj)
    var object stream;
    var object obj;
    {
      check_SP();
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      write_byte(get_synonym_stream(symbol),obj);
    }

# WRITE-BYTE-ARRAY - Pseudofunktion für Synonym-Streams:
  local void wr_by_array_synonym (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local void wr_by_array_synonym(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      var object symbol = TheStream(*stream_)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      write_byte_array(&STACK_0,bytearray_,start,len);
      skipSTACK(1);
    }

# READ-CHAR - Pseudofunktion für Synonym-Streams:
  local object rd_ch_synonym (const object* stream_);
  local object rd_ch_synonym(stream_)
    var const object* stream_;
    {
      check_SP(); check_STACK();
      var object stream = *stream_;
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      var object ergebnis = read_char(&STACK_0);
      skipSTACK(1);
      return ergebnis;
    }

# PEEK-CHAR - Pseudofunktion für Synonym-Streams:
  local object pk_ch_synonym (const object* stream_);
  local object pk_ch_synonym(stream_)
    var const object* stream_;
    {
      check_SP(); check_STACK();
      var object stream = *stream_;
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      var object ergebnis = peek_char(&STACK_0);
      skipSTACK(1);
      return ergebnis;
    }

# READ-CHAR-ARRAY - Pseudofunktion für Synonym-Streams:
  local uintL rd_ch_array_synonym (const object* stream_, const object* chararray_, uintL start, uintL len);
  local uintL rd_ch_array_synonym(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      var object symbol = TheStream(*stream_)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      var uintL result = read_char_array(&STACK_0,chararray_,start,len);
      skipSTACK(1);
      return result;
    }

# WRITE-CHAR - Pseudofunktion für Synonym-Streams:
  local void wr_ch_synonym (const object* stream_, object obj);
  local void wr_ch_synonym(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      check_SP(); check_STACK();
      var object stream = *stream_;
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      write_char(&STACK_0,obj);
      skipSTACK(1);
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für Synonym-Streams:
  local void wr_ch_array_synonym (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_synonym(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      var object symbol = TheStream(*stream_)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      write_char_array(&STACK_0,chararray_,start,len);
      skipSTACK(1);
      # No need to update wr_ch_lpos here. (See get_line_position.)
    }

# Schließt einen Synonym-Stream.
# close_synonym(stream);
# > stream : Synonym-Stream
#ifdef X3J13_014
  #define close_synonym(stream)
#else
  local void close_synonym (object stream);
  local void close_synonym(stream)
    var object stream;
    {
      check_SP(); check_STACK();
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      funcall(S(close),1);
    }
#endif

# Reads a line of characters from a synonym-stream.
# read_line_synonym(stream,&buffer)
# > stream: synonym-stream
# > buffer: a semi-simple string
# < buffer: contains the read characters, excluding the terminating #\Newline
# < result: TRUE is EOF was seen before newline, else FALSE
# can trigger GC
  local boolean read_line_synonym (object stream, const object* buffer_);
  local boolean read_line_synonym(stream,buffer_)
    var object stream;
    var const object* buffer_;
    {
      check_SP(); check_STACK();
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      var boolean eofp = read_line(&STACK_0,buffer_);
      skipSTACK(1);
      return eofp;
    }

# Stellt fest, ob ein Synonym-Stream ein Zeichen verfügbar hat.
# listen_synonym(stream)
# > stream : Synonym-Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
# can trigger GC
  local signean listen_synonym (object stream);
  local signean listen_synonym(stream)
    var object stream;
    {
      check_SP();
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      return stream_listen(get_synonym_stream(symbol));
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Synonym-Stream.
# clear_input_synonym(stream)
# > stream: Synonym-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# can trigger GC
  local boolean clear_input_synonym (object stream);
  local boolean clear_input_synonym(stream)
    var object stream;
    {
      check_SP();
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      return clear_input(get_synonym_stream(symbol));
    }

# UP: Wartenden Output eines Synonym-Stream ans Ziel bringen.
# finish_output_synonym(stream);
# > stream: Synonym-Stream
# can trigger GC
  local void finish_output_synonym (object stream);
  local void finish_output_synonym(stream)
    var object stream;
    {
      check_SP();
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      finish_output(get_synonym_stream(symbol));
    }

# UP: Wartenden Output eines Synonym-Stream ans Ziel bringen.
# force_output_synonym(stream);
# > stream: Synonym-Stream
# can trigger GC
  local void force_output_synonym (object stream);
  local void force_output_synonym(stream)
    var object stream;
    {
      check_SP();
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      force_output(get_synonym_stream(symbol));
    }

# UP: Löscht den wartenden Output eines Synonym-Stream.
# clear_output_synonym(stream);
# > stream: Synonym-Stream
# can trigger GC
  local void clear_output_synonym (object stream);
  local void clear_output_synonym(stream)
    var object stream;
    {
      check_SP();
      var object symbol = TheStream(stream)->strm_synonym_symbol;
      clear_output(get_synonym_stream(symbol));
    }

# Liefert einen Synonym-Stream zu einem Symbol.
# make_synonym_stream(symbol)
# > symbol : Symbol
# < ergebnis : neuer Synonym-Stream
# can trigger GC
  local object make_synonym_stream (object symbol);
  local object make_synonym_stream(symbol)
    var object symbol;
    {
      pushSTACK(symbol); # Symbol retten
      var object stream = # neuer Stream, alle Operationen erlaubt
        allocate_stream(strmflags_open_B,strmtype_synonym,strm_len+1,0);
      TheStream(stream)->strm_rd_by = P(rd_by_synonym);
      TheStream(stream)->strm_rd_by_array = P(rd_by_array_synonym);
      TheStream(stream)->strm_wr_by = P(wr_by_synonym);
      TheStream(stream)->strm_wr_by_array = P(wr_by_array_synonym);
      TheStream(stream)->strm_rd_ch = P(rd_ch_synonym);
      TheStream(stream)->strm_pk_ch = P(pk_ch_synonym);
      TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_synonym);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_synonym);
      TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_synonym);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_synonym_symbol = popSTACK();
      return stream;
    }

LISPFUNN(make_synonym_stream,1)
# (MAKE-SYNONYM-STREAM symbol), CLTL S. 329
  {
    var object arg = popSTACK();
    if (!symbolp(arg)) {
      pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(symbol)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument should be a symbol, not ~")
            );
    }
    value1 = make_synonym_stream(arg); mv_count=1;
  }

LISPFUNN(synonym_stream_p,1)
# (SYS::SYNONYM-STREAM-P stream) == (TYPEP stream 'SYNONYM-STREAM)
  {
    var object arg = popSTACK();
    value1 = (builtin_stream_p(arg)
              && (TheStream(arg)->strmtype == strmtype_synonym)
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(synonym_stream_symbol,1)
# (SYNONYM-STREAM-SYMBOL stream), CLtL2 S. 507
  {
    var object stream = popSTACK();
    if (!(builtin_stream_p(stream)
          && (TheStream(stream)->strmtype == strmtype_synonym)
       ) ) {
      if (!streamp(stream))
        fehler_stream(stream);
      else
        fehler_streamtype(stream,S(synonym_stream));
    }
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
    {
      check_SP(); check_STACK();
      pushSTACK(obj);
      {
        var object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
        # obj auf jeden Stream aus der Liste ausgeben:
        while (consp(streamlist)) {
          pushSTACK(Cdr(streamlist)); # restliche Streams
          write_byte(Car(streamlist),STACK_1); # obj ausgeben
          streamlist = popSTACK();
        }
      }
      skipSTACK(1);
    }

# WRITE-BYTE-ARRAY - Pseudofunktion für Broadcast-Streams:
  local void wr_by_array_broad (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local void wr_by_array_broad(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_broad_list); # list of streams
      var object streamlist;
      while (streamlist = STACK_0, consp(streamlist)) {
        STACK_0 = Cdr(streamlist);
        pushSTACK(Car(streamlist));
        write_byte_array(&STACK_0,bytearray_,start,len);
        skipSTACK(1);
      }
      skipSTACK(1);
    }

# WRITE-CHAR - Pseudofunktion für Broadcast-Streams:
  local void wr_ch_broad (const object* stream_, object obj);
  local void wr_ch_broad(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      check_SP(); check_STACK();
      pushSTACK(obj);
      pushSTACK(NIL); # dummy
      pushSTACK(TheStream(*stream_)->strm_broad_list); # Liste von Streams
      # obj auf jeden Stream aus der Liste ausgeben:
      while (mconsp(STACK_0)) {
        # Stackaufbau: obj, dummy, streamlistr.
        STACK_1 = Car(STACK_0); # ein Stream aus der Liste
        write_char(&STACK_1,STACK_2); # obj ausgeben
        STACK_0 = Cdr(STACK_0);
      }
      skipSTACK(3);
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für Broadcast-Streams:
  local void wr_ch_array_broad (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_broad(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_broad_list); # list of streams
      pushSTACK(NIL); # dummy
      var object streamlist;
      while (streamlist = STACK_1, consp(streamlist)) {
        STACK_1 = Cdr(streamlist);
        STACK_0 = Car(streamlist);
        write_char_array(&STACK_0,chararray_,start,len);
      }
      skipSTACK(2);
      # No need to update wr_ch_lpos here. (See get_line_position.)
    }

# UP: Bringt den wartenden Output eines Broadcast-Stream ans Ziel.
# finish_output_broad(stream);
# > stream: Broadcast-Stream
# can trigger GC
  local void finish_output_broad (object stream);
  local void finish_output_broad(stream)
    var object stream;
    {
      check_SP(); check_STACK();
      var object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
      # Jeden Stream aus der Liste einzeln behandeln:
      while (consp(streamlist)) {
        pushSTACK(Cdr(streamlist)); # restliche Streams
        finish_output(Car(streamlist));
        streamlist = popSTACK();
      }
    }

# UP: Bringt den wartenden Output eines Broadcast-Stream ans Ziel.
# force_output_broad(stream);
# > stream: Broadcast-Stream
# can trigger GC
  local void force_output_broad (object stream);
  local void force_output_broad(stream)
    var object stream;
    {
      check_SP(); check_STACK();
      var object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
      # Jeden Stream aus der Liste einzeln behandeln:
      while (consp(streamlist)) {
        pushSTACK(Cdr(streamlist)); # restliche Streams
        force_output(Car(streamlist));
        streamlist = popSTACK();
      }
    }

# UP: Löscht den wartenden Output eines Broadcast-Stream.
# clear_output_broad(stream);
# > stream: Broadcast-Stream
# can trigger GC
  local void clear_output_broad (object stream);
  local void clear_output_broad(stream)
    var object stream;
    {
      check_SP(); check_STACK();
      var object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
      # Jeden Stream aus der Liste einzeln behandeln:
      while (consp(streamlist)) {
        pushSTACK(Cdr(streamlist)); # restliche Streams
        clear_output(Car(streamlist));
        streamlist = popSTACK();
      }
    }

# Liefert einen Broadcast-Stream zu einer Streamliste.
# make_broadcast_stream(list)
# > list : Liste von Streams
# < ergebnis : Broadcast-Stream
# Die Liste list wird dabei zerstört.
# can trigger GC
  local object make_broadcast_stream (object list);
  local object make_broadcast_stream(list)
    var object list;
    {
      pushSTACK(list); # list retten
      var object stream = # neuer Stream, nur WRITEs erlaubt
        allocate_stream(strmflags_wr_B,strmtype_broad,strm_len+1,0);
      list = popSTACK();
      TheStream(stream)->strm_rd_by = P(rd_by_error);
      TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
      TheStream(stream)->strm_wr_by = P(wr_by_broad);
      TheStream(stream)->strm_wr_by_array = P(wr_by_array_broad);
      TheStream(stream)->strm_rd_ch = P(rd_ch_error);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_error);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_broad);
      TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_broad);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_broad_list = list;
      return stream;
    }

# Liefert einen Broadcast-Stream zum Stream stream.
# make_broadcast1_stream(stream)
# > stream : Stream
# < ergebnis : Broadcast-Stream
# can trigger GC
  global object make_broadcast1_stream (object stream);
  global object make_broadcast1_stream(oldstream)
    var object oldstream;
    {
      pushSTACK(oldstream);
      # oldstream in eine einelementige Liste packen:
      var object new_cons = allocate_cons();
      Car(new_cons) = STACK_0;
      var object stream = make_broadcast_stream(new_cons); # neuer Stream
      oldstream = popSTACK();
      # Line-Position übernehmen:
      TheStream(stream)->strm_wr_ch_lpos = TheStream(oldstream)->strm_wr_ch_lpos;
      return stream;
    }

LISPFUN(make_broadcast_stream,0,0,rest,nokey,0,NIL)
# (MAKE-BROADCAST-STREAM {stream}), CLTL S. 329
  {
    # Überprüfen, ob alle Argumente Streams sind:
    test_output_stream_args(rest_args_pointer,argcount);
    # zu einer Liste zusammenfassen:
    var object list = listof(argcount);
    # Stream bauen:
    value1 = make_broadcast_stream(list); mv_count=1;
  }

LISPFUNN(broadcast_stream_p,1)
# (SYS::BROADCAST-STREAM-P stream) == (TYPEP stream 'BROADCAST-STREAM)
  {
    var object arg = popSTACK();
    value1 = (builtin_stream_p(arg)
              && (TheStream(arg)->strmtype == strmtype_broad)
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(broadcast_stream_streams,1)
# (BROADCAST-STREAM-STREAMS stream), CLtL2 S. 507
  {
    var object stream = popSTACK();
    if (!(builtin_stream_p(stream)
          && (TheStream(stream)->strmtype == strmtype_broad)
       ) ) {
      if (!streamp(stream))
        fehler_stream(stream);
      else
        fehler_streamtype(stream,S(broadcast_stream));
    }
    # Liste der Streams sicherheitshalber kopieren
    value1 = copy_list(TheStream(stream)->strm_broad_list); mv_count=1;
  }


# Concatenated-Stream
# ===================

# Zusätzliche Komponenten:
  # define strm_concat_list      strm_other[0]  # list of not exhausted streams
  #define strm_concat_totallist  strm_other[1]  # list of all streams

# READ-BYTE - Pseudofunktion für Concatenated-Streams:
  local object rd_by_concat (object stream);
  local object rd_by_concat(stream)
    var object stream;
    {
      check_SP(); check_STACK();
      pushSTACK(stream);
      var object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
      var object ergebnis;
      while (consp(streamlist)) {
        ergebnis = read_byte(Car(streamlist)); # Integer lesen
        if (!eq(ergebnis,eof_value)) # nicht EOF ?
          goto OK;
        # EOF erreicht -> verbrauchten Stream aus der Liste nehmen:
        stream = STACK_0;
        streamlist =
        TheStream(stream)->strm_concat_list = Cdr(TheStream(stream)->strm_concat_list);
      }
      # alle Streams verbraucht -> liefere EOF:
      ergebnis = eof_value;
     OK: # ergebnis fertig
      skipSTACK(1);
      return ergebnis;
    }

# READ-BYTE-ARRAY - Pseudofunktion für Concatenated-Streams:
  local uintL rd_by_array_concat (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local uintL rd_by_array_concat(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      var uintL result = 0;
      var object stream = *stream_;
      var object streamlist = TheStream(stream)->strm_concat_list; # list of streams
      loop {
        if (atomp(streamlist))
          break;
        pushSTACK(Car(streamlist));
        var uintL count = read_byte_array(&STACK_0,bytearray_,start,len);
        skipSTACK(1);
        result += count;
        start += count; len -= count;
        if (len == 0)
          break;
        # EOF reached -> remove emptied stream from the list:
        stream = *stream_;
        streamlist =
        TheStream(stream)->strm_concat_list = Cdr(TheStream(stream)->strm_concat_list);
      }
      return result;
    }

# READ-CHAR - Pseudofunktion für Concatenated-Streams:
  local object rd_ch_concat (const object* stream_);
  local object rd_ch_concat(stream_)
    var const object* stream_;
    {
      check_SP(); check_STACK();
      var object streamlist = TheStream(*stream_)->strm_concat_list; # Liste von Streams
      while (consp(streamlist)) {
        pushSTACK(Car(streamlist));
        var object ergebnis = read_char(&STACK_0); # Character lesen
        skipSTACK(1);
        if (!eq(ergebnis,eof_value))
          return ergebnis;
        # EOF erreicht -> verbrauchten Stream aus der Liste nehmen:
        var object stream = *stream_;
        streamlist =
        TheStream(stream)->strm_concat_list = Cdr(TheStream(stream)->strm_concat_list);
      }
      # alle Streams verbraucht -> liefere EOF:
      return eof_value;
    }

# PEEK-CHAR - Pseudofunktion für Concatenated-Streams:
  local object pk_ch_concat (const object* stream_);
  local object pk_ch_concat(stream_)
    var const object* stream_;
    {
      check_SP(); check_STACK();
      var object streamlist = TheStream(*stream_)->strm_concat_list; # Liste von Streams
      while (consp(streamlist)) {
        pushSTACK(Car(streamlist));
        var object ergebnis = peek_char(&STACK_0); # Character lesen
        skipSTACK(1);
        if (!eq(ergebnis,eof_value))
          return ergebnis;
        # EOF erreicht -> verbrauchten Stream aus der Liste nehmen:
        var object stream = *stream_;
        streamlist =
        TheStream(stream)->strm_concat_list = Cdr(TheStream(stream)->strm_concat_list);
      }
      # alle Streams verbraucht -> liefere EOF:
      return eof_value;
    }

# READ-CHAR-ARRAY - Pseudofunktion für Concatenated-Streams:
  local uintL rd_ch_array_concat (const object* stream_, const object* chararray_, uintL start, uintL len);
  local uintL rd_ch_array_concat(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      var uintL result = 0;
      var object stream = *stream_;
      var object streamlist = TheStream(stream)->strm_concat_list; # list of streams
      loop {
        if (atomp(streamlist))
          break;
        pushSTACK(Car(streamlist));
        var uintL count = read_char_array(&STACK_0,chararray_,start,len);
        skipSTACK(1);
        result += count;
        start += count; len -= count;
        if (len == 0)
          break;
        # EOF reached -> remove emptied stream from the list:
        stream = *stream_;
        streamlist =
        TheStream(stream)->strm_concat_list = Cdr(TheStream(stream)->strm_concat_list);
      }
      return result;
    }

# Stellt fest, ob ein Concatenated-Stream ein Zeichen verfügbar hat.
# listen_concat(stream)
# > stream : Concatenated-Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
# can trigger GC
  local signean listen_concat (object stream);
  local signean listen_concat(stream)
    var object stream;
    {
      pushSTACK(stream);
      var object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
      var signean ergebnis;
      while (consp(streamlist)) {
        ergebnis = stream_listen(Car(streamlist));
        if (!ls_eof_p(ergebnis)) # nicht EOF ?
          goto OK;
        # EOF erreicht -> verbrauchten Stream aus der Liste nehmen:
        stream = STACK_0;
        streamlist =
        TheStream(stream)->strm_concat_list = Cdr(TheStream(stream)->strm_concat_list);
      }
      # alle Streams verbraucht -> liefere EOF:
      ergebnis = ls_eof;
     OK: # ergebnis fertig
      skipSTACK(1);
      return ergebnis;
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem
# Concatenated-Stream.
# clear_input_concat(stream)
# > stream: Concatenated-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# can trigger GC
  local boolean clear_input_concat (object stream);
  local boolean clear_input_concat(stream)
    var object stream;
    {
      var boolean ergebnis = FALSE; # noch kein Input gelöscht
      # alle Streams einzeln behandeln:
      var object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
      while (consp(streamlist)) {
        pushSTACK(Cdr(streamlist)); # restliche Streamliste
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
# can trigger GC
  local object make_concatenated_stream (object list);
  local object make_concatenated_stream(list)
    var object list;
    {
      pushSTACK(list); # list retten
      var object stream = # neuer Stream, nur READs erlaubt
        allocate_stream(strmflags_rd_B,strmtype_concat,strm_len+2,0);
      TheStream(stream)->strm_rd_by = P(rd_by_concat);
      TheStream(stream)->strm_rd_by_array = P(rd_by_array_concat);
      TheStream(stream)->strm_wr_by = P(wr_by_error);
      TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
      TheStream(stream)->strm_rd_ch = P(rd_ch_concat);
      TheStream(stream)->strm_pk_ch = P(pk_ch_concat);
      TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_concat);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_error);
      TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_error);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_concat_list =
      TheStream(stream)->strm_concat_totallist = popSTACK();
      return stream;
    }

LISPFUN(make_concatenated_stream,0,0,rest,nokey,0,NIL)
# (MAKE-CONCATENATED-STREAM {stream}), CLTL S. 329
  {
    # Überprüfen, ob alle Argumente Streams sind:
    test_input_stream_args(rest_args_pointer,argcount);
    # zu einer Liste zusammenfassen:
    var object list = listof(argcount);
    # Stream bauen:
    value1 = make_concatenated_stream(list); mv_count=1;
  }

LISPFUNN(concatenated_stream_p,1)
# (SYS::CONCATENATED-STREAM-P stream) == (TYPEP stream 'CONCATENATED-STREAM)
  {
    var object arg = popSTACK();
    value1 = (builtin_stream_p(arg)
              && (TheStream(arg)->strmtype == strmtype_concat)
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(concatenated_stream_streams,1)
# (CONCATENATED-STREAM-STREAMS stream), CLtL2 S. 507
  {
    var object stream = popSTACK();
    if (!(builtin_stream_p(stream)
          && (TheStream(stream)->strmtype == strmtype_concat)
       ) ) {
      if (!streamp(stream))
        fehler_stream(stream);
      else
        fehler_streamtype(stream,S(concatenated_stream));
    }
    # Liste der Streams sicherheitshalber kopieren:
    value1 = copy_list(TheStream(stream)->strm_concat_totallist); mv_count=1;
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
    {
      check_SP();
      write_byte(TheStream(stream)->strm_twoway_output,obj);
    }

# WRITE-BYTE-ARRAY - Pseudofunktion für Two-Way- und Echo-Streams:
  local void wr_by_array_twoway (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local void wr_by_array_twoway(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_output);
      write_byte_array(&STACK_0,bytearray_,start,len);
      skipSTACK(1);
    }

# WRITE-CHAR - Pseudofunktion für Two-Way- und Echo-Streams:
  local void wr_ch_twoway (const object* stream_, object obj);
  local void wr_ch_twoway(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_output);
      write_char(&STACK_0,obj);
      skipSTACK(1);
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für Two-Way- und Echo-Streams:
  local void wr_ch_array_twoway (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_twoway(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_output);
      write_char_array(&STACK_0,chararray_,start,len);
      skipSTACK(1);
      # No need to update wr_ch_lpos here. (See get_line_position.)
    }

# Stellt fest, ob ein Two-Way- oder Echo-Stream ein Zeichen verfügbar hat.
# listen_twoway(stream)
# > stream : Two-Way- oder Echo-Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
# can trigger GC
  local signean listen_twoway (object stream);
  local signean listen_twoway(stream)
    var object stream;
    {
      check_SP();
      return stream_listen(TheStream(stream)->strm_twoway_input);
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Two-Way-
# oder Echo-Stream.
# clear_input_twoway(stream)
# > stream: Two-Way- oder Echo-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# can trigger GC
  local boolean clear_input_twoway (object stream);
  local boolean clear_input_twoway(stream)
    var object stream;
    {
      check_SP();
      return clear_input(TheStream(stream)->strm_twoway_input);
    }

# UP: Bringt den wartenden Output eines Two-Way- oder Echo-Stream ans Ziel.
# finish_output_twoway(stream);
# > stream: Two-Way- oder Echo-Stream
# can trigger GC
  local void finish_output_twoway (object stream);
  local void finish_output_twoway(stream)
    var object stream;
    {
      check_SP();
      finish_output(TheStream(stream)->strm_twoway_output);
    }

# UP: Bringt den wartenden Output eines Two-Way- oder Echo-Stream ans Ziel.
# force_output_twoway(stream);
# > stream: Two-Way- oder Echo-Stream
# can trigger GC
  local void force_output_twoway (object stream);
  local void force_output_twoway(stream)
    var object stream;
    {
      check_SP();
      force_output(TheStream(stream)->strm_twoway_output);
    }

# UP: Löscht den wartenden Output eines Two-Way- oder Echo-Stream.
# clear_output_twoway(stream);
# > stream: Two-Way- oder Echo-Stream
# can trigger GC
  local void clear_output_twoway (object stream);
  local void clear_output_twoway(stream)
    var object stream;
    {
      check_SP();
      clear_output(TheStream(stream)->strm_twoway_output);
    }


# Two-Way-Stream
# ==============

# READ-BYTE - Pseudofunktion für Two-Way-Streams:
  local object rd_by_twoway (object stream);
  local object rd_by_twoway(stream)
    var object stream;
    {
      check_SP();
      return read_byte(TheStream(stream)->strm_twoway_input);
    }

# READ-BYTE-ARRAY - Pseudofunktion für Two-Way-Streams:
  local uintL rd_by_array_twoway (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local uintL rd_by_array_twoway(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
      var uintL result = read_byte_array(&STACK_0,bytearray_,start,len);
      skipSTACK(1);
      return result;
    }

# READ-CHAR - Pseudofunktion für Two-Way-Streams:
  local object rd_ch_twoway (const object* stream_);
  local object rd_ch_twoway(stream_)
    var const object* stream_;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
      var object ergebnis = read_char(&STACK_0);
      skipSTACK(1);
      return ergebnis;
    }

# PEEK-CHAR - Pseudofunktion für Two-Way-Streams:
  local object pk_ch_twoway (const object* stream_);
  local object pk_ch_twoway(stream_)
    var const object* stream_;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
      var object ergebnis = peek_char(&STACK_0);
      skipSTACK(1);
      return ergebnis;
    }

# READ-CHAR-ARRAY - Pseudofunktion für Two-Way-Streams:
  local uintL rd_ch_array_twoway (const object* stream_, const object* chararray_, uintL start, uintL len);
  local uintL rd_ch_array_twoway(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
      var uintL result = read_char_array(&STACK_0,chararray_,start,len);
      skipSTACK(1);
      return result;
    }

# Reads a line of characters from a two-way-stream.
# read_line_twoway(stream,&buffer)
# > stream: two-way-stream
# > buffer: a semi-simple string
# < buffer: contains the read characters, excluding the terminating #\Newline
# < result: TRUE is EOF was seen before newline, else FALSE
# can trigger GC
  local boolean read_line_twoway (object stream, const object* buffer_);
  local boolean read_line_twoway(stream,buffer_)
    var object stream;
    var const object* buffer_;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(stream)->strm_twoway_input);
      var boolean eofp = read_line(&STACK_0,buffer_);
      skipSTACK(1);
      return eofp;
    }

# Liefert einen Two-Way-Stream zu einem Input-Stream und einem Output-Stream.
# make_twoway_stream(input_stream,output_stream)
# > input_stream : Input-Stream
# > output_stream : Output-Stream
# < ergebnis : Two-Way-Stream
# can trigger GC
  global object make_twoway_stream (object input_stream, object output_stream);
  global object make_twoway_stream(input_stream,output_stream)
    var object input_stream;
    var object output_stream;
    {
      pushSTACK(input_stream); pushSTACK(output_stream); # Streams retten
      var uintB flags =
        strmflags_open_B | (TheStream(input_stream)->strmflags & strmflags_immut_B);
      var object stream = # neuer Stream, alle Operationen erlaubt
        allocate_stream(flags,strmtype_twoway,strm_len+2,0);
      TheStream(stream)->strm_rd_by = P(rd_by_twoway);
      TheStream(stream)->strm_rd_by_array = P(rd_by_array_twoway);
      TheStream(stream)->strm_wr_by = P(wr_by_twoway);
      TheStream(stream)->strm_wr_by_array = P(wr_by_array_twoway);
      TheStream(stream)->strm_rd_ch = P(rd_ch_twoway);
      TheStream(stream)->strm_pk_ch = P(pk_ch_twoway);
      TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_twoway);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_twoway);
      TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_twoway);
      output_stream = popSTACK(); input_stream = popSTACK(); # Streams zurück
      TheStream(stream)->strm_wr_ch_lpos = TheStream(output_stream)->strm_wr_ch_lpos;
      TheStream(stream)->strm_twoway_input = input_stream;
      TheStream(stream)->strm_twoway_output = output_stream;
      return stream;
    }

LISPFUNN(make_two_way_stream,2)
# (MAKE-TWO-WAY-STREAM input-stream output-stream), CLTL S. 329
  {
    # Überprüfen, ob beides Streams sind:
    test_stream_args(args_end_pointer STACKop 2, 2);
    var object output_stream = popSTACK();
    var object input_stream = popSTACK();
    test_input_stream(input_stream);
    test_output_stream(output_stream);
    # Stream bauen:
    value1 = make_twoway_stream(input_stream,output_stream); mv_count=1;
  }

LISPFUNN(two_way_stream_p,1)
# (SYS::TWO-WAY-STREAM-P stream) == (TYPEP stream 'TWO-WAY-STREAM)
  {
    var object arg = popSTACK();
    value1 = (builtin_stream_p(arg)
              && (TheStream(arg)->strmtype == strmtype_twoway)
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(two_way_stream_input_stream,1)
# (TWO-WAY-STREAM-INPUT-STREAM stream), CLtL2 S. 507
  {
    var object stream = popSTACK();
    if (!(builtin_stream_p(stream)
          && (TheStream(stream)->strmtype == strmtype_twoway)
       ) ) {
      if (!streamp(stream))
        fehler_stream(stream);
      else
        fehler_streamtype(stream,S(two_way_stream));
    }
    value1 = TheStream(stream)->strm_twoway_input; mv_count=1;
  }

LISPFUNN(two_way_stream_output_stream,1)
# (TWO-WAY-STREAM-OUTPUT-STREAM stream), CLtL2 S. 507
  {
    var object stream = popSTACK();
    if (!(builtin_stream_p(stream)
          && (TheStream(stream)->strmtype == strmtype_twoway)
       ) ) {
      if (!streamp(stream))
        fehler_stream(stream);
      else
        fehler_streamtype(stream,S(two_way_stream));
    }
    value1 = TheStream(stream)->strm_twoway_output; mv_count=1;
  }


# Echo-Stream
# ===========

# READ-BYTE - Pseudofunktion für Echo-Streams:
  local object rd_by_echo (object stream);
  local object rd_by_echo(stream)
    var object stream;
    {
      check_SP(); check_STACK();
      pushSTACK(stream);
      var object obj = read_byte(TheStream(stream)->strm_twoway_input);
      stream = popSTACK();
      if (!eq(obj,eof_value)) {
        pushSTACK(obj);
        write_byte(TheStream(stream)->strm_twoway_output,obj);
        obj = popSTACK();
      }
      return obj;
    }

# READ-BYTE-ARRAY - Pseudofunktion für Echo-Streams:
  local uintL rd_by_array_echo (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local uintL rd_by_array_echo(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
      var uintL result = read_byte_array(&STACK_0,bytearray_,start,len);
      STACK_0 = TheStream(*stream_)->strm_twoway_output;
      write_byte_array(&STACK_0,bytearray_,start,result);
      skipSTACK(1);
      return result;
    }

# READ-CHAR - Pseudofunktion für Echo-Streams:
  local object rd_ch_echo (const object* stream_);
  local object rd_ch_echo(stream_)
    var const object* stream_;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
      var object obj = read_char(&STACK_0);
      if (!eq(obj,eof_value)) {
        STACK_0 = TheStream(*stream_)->strm_twoway_output;
        pushSTACK(obj);
        write_char(&STACK_1,obj);
        obj = popSTACK();
      }
      skipSTACK(1);
      return obj;
    }

# READ-CHAR-ARRAY - Pseudofunktion für Echo-Streams:
  local uintL rd_ch_array_echo (const object* stream_, const object* chararray_, uintL start, uintL len);
  local uintL rd_ch_array_echo(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
      var uintL result = read_char_array(&STACK_0,chararray_,start,len);
      STACK_0 = TheStream(*stream_)->strm_twoway_output;
      write_char_array(&STACK_0,chararray_,start,result);
      skipSTACK(1);
      return result;
    }

# Liefert einen Echo-Stream zu einem Input-Stream und einem Output-Stream.
# make_echo_stream(input_stream,output_stream)
# > input_stream : Input-Stream
# > output_stream : Output-Stream
# < ergebnis : Echo-Stream
# can trigger GC
  local object make_echo_stream (object input_stream, object output_stream);
  local object make_echo_stream(input_stream,output_stream)
    var object input_stream;
    var object output_stream;
    {
      pushSTACK(input_stream); pushSTACK(output_stream); # Streams retten
      var uintB flags =
        strmflags_open_B | (TheStream(input_stream)->strmflags & strmflags_immut_B);
      var object stream = # neuer Stream, alle Operationen erlaubt
        allocate_stream(flags,strmtype_echo,strm_len+2,0);
      TheStream(stream)->strm_rd_by = P(rd_by_echo);
      TheStream(stream)->strm_rd_by_array = P(rd_by_array_echo);
      TheStream(stream)->strm_wr_by = P(wr_by_twoway);
      TheStream(stream)->strm_wr_by_array = P(wr_by_array_twoway);
      TheStream(stream)->strm_rd_ch = P(rd_ch_echo);
      TheStream(stream)->strm_pk_ch = P(pk_ch_twoway);
      TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_echo);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_twoway);
      TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_twoway);
      output_stream = popSTACK(); input_stream = popSTACK(); # Streams zurück
      TheStream(stream)->strm_wr_ch_lpos = TheStream(output_stream)->strm_wr_ch_lpos;
      TheStream(stream)->strm_twoway_input = input_stream;
      TheStream(stream)->strm_twoway_output = output_stream;
      return stream;
    }

LISPFUNN(make_echo_stream,2)
# (MAKE-ECHO-STREAM input-stream output-stream), CLTL S. 330
  {
    # Überprüfen, ob beides Streams sind:
    test_stream_args(args_end_pointer STACKop 2, 2);
    var object output_stream = popSTACK();
    var object input_stream = popSTACK();
    test_input_stream(input_stream);
    test_output_stream(output_stream);
    # Stream bauen:
    value1 = make_echo_stream(input_stream,output_stream); mv_count=1;
  }

LISPFUNN(echo_stream_p,1)
# (SYS::ECHO-STREAM-P stream) == (TYPEP stream 'ECHO-STREAM)
  {
    var object arg = popSTACK();
    value1 = (builtin_stream_p(arg)
              && (TheStream(arg)->strmtype == strmtype_echo)
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(echo_stream_input_stream,1)
# (ECHO-STREAM-INPUT-STREAM stream), CLtL2 S. 507
  {
    var object stream = popSTACK();
    if (!(builtin_stream_p(stream)
          && (TheStream(stream)->strmtype == strmtype_echo)
       ) ) {
      if (!streamp(stream))
        fehler_stream(stream);
      else
        fehler_streamtype(stream,S(echo_stream));
    }
    value1 = TheStream(stream)->strm_twoway_input; mv_count=1;
  }

LISPFUNN(echo_stream_output_stream,1)
# (ECHO-STREAM-OUTPUT-STREAM stream), CLtL2 S. 507
  {
    var object stream = popSTACK();
    if (!(builtin_stream_p(stream)
          && (TheStream(stream)->strmtype == strmtype_echo)
       ) ) {
      if (!streamp(stream))
        fehler_stream(stream);
      else
        fehler_streamtype(stream,S(echo_stream));
    }
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
    {
      pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(TheStream(stream)->strm_str_in_string);
      pushSTACK(stream);
      fehler(stream_error,
             GETTEXT("~ is beyond the end because the string ~ has been adjusted")
            );
    }

# READ-CHAR - Pseudofunktion für String-Input-Streams:
  local object rd_ch_str_in (const object* stream_);
  local object rd_ch_str_in(stream_)
    var const object* stream_;
    {
      var object stream = *stream_;
      var uintL index = posfixnum_to_L(TheStream(stream)->strm_str_in_index); # Index
      var uintL endindex = posfixnum_to_L(TheStream(stream)->strm_str_in_endindex);
      if (index >= endindex) {
        return eof_value; # EOF erreicht
      } else {
        # index < eofindex
        var uintL len;
        var uintL offset;
        var object string = unpack_string_ro(TheStream(stream)->strm_str_in_string,&len,&offset);
        if (index >= len) # Index zu groß ?
          fehler_str_in_adjusted(stream);
        var object ch; # Zeichen aus dem String holen
        SstringDispatch(string,
          { ch = code_char(TheSstring(string)->data[offset+index]); },
          { ch = code_char(as_chart(TheSmallSstring(string)->data[offset+index])); }
          );
        # Index erhöhen:
        TheStream(stream)->strm_str_in_index = fixnum_inc(TheStream(stream)->strm_str_in_index,1);
        return ch;
      }
    }

# READ-CHAR-ARRAY - Pseudofunktion für String-Input-Streams:
  local uintL rd_ch_array_str_in (const object* stream_, const object* chararray_, uintL start, uintL len);
  local uintL rd_ch_array_str_in(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var object stream = *stream_;
      var uintL index = posfixnum_to_L(TheStream(stream)->strm_str_in_index); # Index
      var uintL endindex = posfixnum_to_L(TheStream(stream)->strm_str_in_endindex);
      if (index < endindex) {
        var chart* charptr = &TheSstring(*chararray_)->data[start];
        var uintL srclen;
        var uintL srcoffset;
        var object string = unpack_string_ro(TheStream(stream)->strm_str_in_string,&srclen,&srcoffset);
        if (srclen < endindex)
          fehler_str_in_adjusted(stream);
        var uintL count = endindex - index;
        if (count > len)
          count = len;
        # count = min(len,endindex-index) > 0.
        TheStream(stream)->strm_str_in_index = fixnum_inc(TheStream(stream)->strm_str_in_index,count);
        SstringDispatch(string,
          { chartcopy(&TheSstring(string)->data[srcoffset+index],charptr,count); },
          { scintcopy(&TheSmallSstring(string)->data[srcoffset+index],charptr,count); }
          );
        return count;
      } else {
        return 0;
      }
    }

# Schließt einen String-Input-Stream.
# close_str_in(stream);
# > stream : String-Input-Stream
  local void close_str_in (object stream);
  local void close_str_in(stream)
    var object stream;
    {
      TheStream(stream)->strm_str_in_string = NIL; # String := NIL
    }

# Stellt fest, ob ein String-Input-Stream ein Zeichen verfügbar hat.
# listen_str_in(stream)
# > stream : String-Input-Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
# can trigger GC
  local signean listen_str_in (object stream);
  local signean listen_str_in(stream)
    var object stream;
    {
      var uintL index = posfixnum_to_L(TheStream(stream)->strm_str_in_index); # Index
      var uintL endindex = posfixnum_to_L(TheStream(stream)->strm_str_in_endindex);
      if (index >= endindex)
        return ls_eof; # EOF erreicht
      else
        return ls_avail;
    }

LISPFUN(make_string_input_stream,1,2,norest,nokey,0,NIL)
# (MAKE-STRING-INPUT-STREAM string [start [end]]), CLTL S. 330
  {
    # String holen und Grenzen überprüfen:
    var stringarg arg;
    var object string = test_string_limits_ro(&arg);
    var object start_arg = fixnum(arg.index); # start-Argument (Fixnum >=0)
    var object end_arg = fixnum_inc(start_arg,arg.len); # end-Argument (Fixnum >=0)
    pushSTACK(string); # String retten
    var object stream = # neuer Stream, nur READ-CHAR erlaubt
      allocate_stream(strmflags_rd_ch_B,strmtype_str_in,strm_len+3,0);
    TheStream(stream)->strm_rd_by = P(rd_by_error);
    TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
    TheStream(stream)->strm_wr_by = P(wr_by_error);
    TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
    TheStream(stream)->strm_rd_ch = P(rd_ch_str_in);
    TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
    TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_str_in);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_error);
    TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_error);
    TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
    TheStream(stream)->strm_str_in_string = popSTACK();
    TheStream(stream)->strm_str_in_index = start_arg; # Index := start-Argument
    TheStream(stream)->strm_str_in_endindex = end_arg; # Endindex := end-Argument
    value1 = stream; mv_count=1; # stream als Wert
  }

LISPFUNN(string_input_stream_index,1)
# (SYSTEM::STRING-INPUT-STREAM-INDEX string-input-stream) liefert den Index
  {
    var object stream = popSTACK(); # Argument
    # muss ein String-Input-Stream sein:
    if (!(builtin_stream_p(stream) && (TheStream(stream)->strmtype == strmtype_str_in))) {
      pushSTACK(stream);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: ~ is not a string input stream")
            );
    }
    var object index = TheStream(stream)->strm_str_in_index;
    # Falls ein Character mit UNREAD-CHAR zurückgeschoben wurde,
    # verwende (1- index), ein Fixnum >=0, als Wert:
    if (TheStream(stream)->strmflags & strmflags_unread_B)
      index = fixnum_inc(index,-1);
    value1 = index; mv_count=1;
  }


# String-Output-Stream
# ====================

# Zusätzliche Komponenten:
  #define strm_str_out_string  strm_other[0]  # Semi-Simple-String für Output

# WRITE-CHAR - Pseudofunktion für String-Output-Streams:
  local void wr_ch_str_out (const object* stream_, object ch);
  local void wr_ch_str_out(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      var object stream = *stream_;
      # obj sollte Character sein:
      if (!charp(ch))
        fehler_wr_char(stream,ch);
      # Character in den String schieben:
      ssstring_push_extend(TheStream(stream)->strm_str_out_string,char_code(ch));
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für String-Output-Streams:
  local void wr_ch_array_str_out (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_str_out(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var object ssstring = TheStream(*stream_)->strm_str_out_string; # Semi-Simple-String
      ssstring = ssstring_append_extend(ssstring,*chararray_,start,len);
      wr_ss_lpos(*stream_,&TheSstring(TheIarray(ssstring)->data)->data[TheIarray(ssstring)->dims[1]],len); # Line-Position aktualisieren
    }

# Liefert einen String-Output-Stream.
# make_string_output_stream()
# can trigger GC
  global object make_string_output_stream (void);
  global object make_string_output_stream()
    {
      # kleinen Semi-Simple-String der Länge 50 allozieren:
      pushSTACK(make_ssstring(50));
      var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_str_out,strm_len+1,0);
      TheStream(stream)->strm_rd_by = P(rd_by_error);
      TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
      TheStream(stream)->strm_wr_by = P(wr_by_error);
      TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
      TheStream(stream)->strm_rd_ch = P(rd_ch_error);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_error);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_str_out);
      TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_str_out);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_str_out_string = popSTACK(); # String eintragen
      return stream;
    }

LISPFUN(make_string_output_stream,0,0,norest,key,2, (kw(element_type),kw(line_position)))
# (MAKE-STRING-OUTPUT-STREAM [:element-type] [:line-position])
  {
    # line-position überprüfen:
    if (eq(STACK_0,unbound) || nullp(STACK_0)) {
      STACK_0 = Fixnum_0; # Defaultwert 0
    } else {
      # line-position angegeben, sollte ein Fixnum >=0 sein:
      if (!posfixnump(STACK_0))
        fehler_posfixnum(STACK_0);
    }
    # element-type überprüfen:
    if (!eq(STACK_1,unbound)) {
      var object eltype = STACK_1;
      if (!eq(eltype,S(character))) {
        # Verify (SUBTYPEP eltype 'CHARACTER):
        pushSTACK(eltype); pushSTACK(S(character)); funcall(S(subtypep),2);
        if (nullp(value1)) {
          pushSTACK(STACK_1); # eltype
          pushSTACK(S(character)); # CHARACTER
          pushSTACK(S(Kelement_type)); # :ELEMENT-TYPE
          pushSTACK(S(make_string_output_stream));
          fehler(error,
                 GETTEXT("~: ~ argument must be a subtype of ~, not ~")
                );
        }
      }
    }
    var object stream = make_string_output_stream(); # String-Output-Stream
    TheStream(stream)->strm_wr_ch_lpos = popSTACK(); # Line Position eintragen
    value1 = stream; mv_count=1; # stream als Wert
    skipSTACK(1);
  }

# UP: Liefert das von einem String-Output-Stream Angesammelte.
# get_output_stream_string(&stream)
# > stream: String-Output-Stream
# < stream: geleerter Stream
# < ergebnis: Angesammeltes, ein Simple-String
# can trigger GC
  global object get_output_stream_string (const object* stream_);
  global object get_output_stream_string(stream_)
    var const object* stream_;
    {
      var object string = TheStream(*stream_)->strm_str_out_string; # alter String
      string = coerce_ss(string); # in Simple-String umwandeln (erzwingt ein Kopieren)
      # alten String durch Fill-Pointer:=0 leeren:
      TheIarray(TheStream(*stream_)->strm_str_out_string)->dims[1] = 0;
      return string;
    }

LISPFUNN(get_output_stream_string,1)
# (GET-OUTPUT-STREAM-STRING string-output-stream), CLTL S. 330
  {
    var object stream = STACK_0; # Argument
    # muss ein String-Output-Stream sein:
    if (!(builtin_stream_p(stream) && (TheStream(stream)->strmtype == strmtype_str_out))) {
      # stream in STACK_0
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: ~ is not a string output stream")
            );
    }
    value1 = get_output_stream_string(&STACK_0); mv_count=1; # Angesammeltes als Wert
    skipSTACK(1);
  }


# String-Push-Stream
# ==================

# Zusätzliche Komponenten:
  #define strm_str_push_string  strm_other[0]  # String mit Fill-Pointer für Output

# WRITE-CHAR - Pseudofunktion für String-Push-Streams:
  local void wr_ch_str_push (const object* stream_, object ch);
  local void wr_ch_str_push(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      var object stream = *stream_;
      # ch sollte Character sein:
      if (!charp(ch))
        fehler_wr_char(stream,ch);
      # Character in den String schieben:
      pushSTACK(ch); pushSTACK(TheStream(stream)->strm_str_push_string);
      funcall(L(vector_push_extend),2); # (VECTOR-PUSH-EXTEND ch string)
    }

# (SYSTEM::MAKE-STRING-PUSH-STREAM string) liefert einen Stream, dessen
# WRITE-CHAR-Operation mit einem VECTOR-PUSH-EXTEND auf den gegebenen String
# äquivalent ist.
LISPFUNN(make_string_push_stream,1)
  {
    {
      var object arg = STACK_0; # Argument
      # muss ein String mit Fill-Pointer sein:
      if (!(stringp(arg) && array_has_fill_pointer_p(arg))) {
        pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_string_with_fill_pointer)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(S(with_output_to_string));
        fehler(type_error,
               GETTEXT("~: argument ~ should be a string with fill pointer")
              );
      }
    }
    var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
      allocate_stream(strmflags_wr_ch_B,strmtype_str_push,strm_len+1,0);
    TheStream(stream)->strm_rd_by = P(rd_by_error);
    TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
    TheStream(stream)->strm_wr_by = P(wr_by_error);
    TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
    TheStream(stream)->strm_rd_ch = P(rd_ch_error);
    TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
    TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_error);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_str_push);
    TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_dummy);
    TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
    TheStream(stream)->strm_str_push_string = popSTACK(); # String eintragen
    value1 = stream; mv_count=1; # stream als Wert
  }


# String-Stream allgemein
# =======================

LISPFUNN(string_stream_p,1)
# (SYS::STRING-STREAM-P stream) == (TYPEP stream 'STRING-STREAM)
  {
    var object arg = popSTACK();
    if (builtin_stream_p(arg)) {
      switch (TheStream(arg)->strmtype) {
        case strmtype_str_in:   # String-Input-Stream
        case strmtype_str_out:  # String-Output-Stream
        case strmtype_str_push: # String-Push-Stream
          value1 = T; break;
        default:
          value1 = NIL; break;
      }
    } else {
      value1 = NIL;
    }
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
    {
      var object stream = *stream_;
      # ch sollte Character sein:
      if (!charp(ch))
        fehler_wr_char(stream,ch);
      var chart c = char_code(ch); # Character
      # Bei NL: Ab jetzt  Modus := Mehrzeiler
      if (chareq(c,ascii(NL)))
        TheStream(stream)->strm_pphelp_modus = T;
      # Character in den ersten String schieben:
      ssstring_push_extend(Car(TheStream(stream)->strm_pphelp_strings),c);
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für Pretty-Printer-Hilfs-Streams:
  local void wr_ch_array_pphelp (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_pphelp(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var object ssstring = Car(TheStream(*stream_)->strm_pphelp_strings); # Semi-Simple-String
      ssstring = ssstring_append_extend(ssstring,*chararray_,start,len);
      if (wr_ss_lpos(*stream_,&TheSstring(TheIarray(ssstring)->data)->data[TheIarray(ssstring)->dims[1]],len)) # Line-Position aktualisieren
        TheStream(*stream_)->strm_pphelp_modus = T; # Nach NL: Modus := Mehrzeiler
    }

# UP: Liefert einen Pretty-Printer-Hilfs-Stream.
# make_pphelp_stream()
# can trigger GC
  global object make_pphelp_stream (void);
  global object make_pphelp_stream()
    {
      # kleinen Semi-Simple-String der Länge 50 allozieren:
      pushSTACK(make_ssstring(50));
      # einelementige Stringliste bauen:
      {
        var object new_cons = allocate_cons();
        Car(new_cons) = popSTACK();
        pushSTACK(new_cons);
      }
      var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_pphelp,strm_len+2,0);
      TheStream(stream)->strm_rd_by = P(rd_by_error);
      TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
      TheStream(stream)->strm_wr_by = P(wr_by_error);
      TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
      TheStream(stream)->strm_rd_ch = P(rd_ch_error);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_error);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_pphelp);
      TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_pphelp);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_pphelp_strings = popSTACK(); # String-Liste eintragen
      TheStream(stream)->strm_pphelp_modus = NIL; # Modus := Einzeiler
      return stream;
    }


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
    {
      var object stream = *stream_;
      var uintL index = posfixnum_to_L(TheStream(stream)->strm_buff_in_index); # Index
      var uintL endindex = posfixnum_to_L(TheStream(stream)->strm_buff_in_endindex);
      loop {
        if (index < endindex) # noch was im aktuellen String?
          break;
        # String-Ende erreicht
        # fun aufrufen:
        funcall(TheStream(stream)->strm_buff_in_fun,0);
        if (!stringp(value1))
          return eof_value; # EOF erreicht
        # neuen String holen und Grenzen überprüfen:
        pushSTACK(value1); # String
        pushSTACK(mv_count >= 2 ? value2 : unbound); # start
        pushSTACK(mv_count >= 3 ? value3 : unbound); # end
        subr_self = L(read_char);
        var stringarg val;
        var object string = test_string_limits_ro(&val);
        stream = *stream_;
        index = val.index;
        endindex = index+val.len;
        TheStream(stream)->strm_buff_in_string = string;
        TheStream(stream)->strm_buff_in_index = fixnum(index);
        TheStream(stream)->strm_buff_in_endindex = fixnum(endindex);
      }
      # index < eofindex
      var uintL len;
      var uintL offset;
      var object string = unpack_string_ro(TheStream(stream)->strm_buff_in_string,&len,&offset);
      if (index >= len) { # Index zu groß ?
        pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(TheStream(stream)->strm_buff_in_string);
        pushSTACK(stream);
        fehler(stream_error,
               GETTEXT("~ is beyond the end because the string ~ has been adjusted")
              );
      }
      var object ch; # Zeichen aus dem String holen
      SstringDispatch(string,
        { ch = code_char(TheSstring(string)->data[offset+index]); },
        { ch = code_char(as_chart(TheSmallSstring(string)->data[offset+index])); }
        );
      # Index erhöhen:
      TheStream(stream)->strm_buff_in_index = fixnum_inc(TheStream(stream)->strm_buff_in_index,1);
      return ch;
    }

# Schließt einen Buffered-Input-Stream.
# close_buff_in(stream);
# > stream : Buffered-Input-Stream
  local void close_buff_in (object stream);
  local void close_buff_in(stream)
    var object stream;
    {
      TheStream(stream)->strm_buff_in_fun = NIL; # Funktion := NIL
      TheStream(stream)->strm_buff_in_mode = NIL; # Mode := NIL
      TheStream(stream)->strm_buff_in_string = NIL; # String := NIL
    }

# Stellt fest, ob ein Buffered-Input-Stream ein Zeichen verfügbar hat.
# listen_buff_in(stream)
# > stream : Buffered-Input-Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
# can trigger GC
  local signean listen_buff_in (object stream);
  local signean listen_buff_in(stream)
    var object stream;
    {
      var uintL index = posfixnum_to_L(TheStream(stream)->strm_buff_in_index); # Index
      var uintL endindex = posfixnum_to_L(TheStream(stream)->strm_buff_in_endindex);
      if (index < endindex)
        return ls_avail;
      var object mode = TheStream(stream)->strm_buff_in_mode;
      if (eq(mode,S(nil))) {
        pushSTACK(stream);
        mode = peek_char(&STACK_0); # peek_char macht read_char, ruft fun auf
        skipSTACK(1);
        if (eq(mode,eof_value))
          return ls_eof; # EOF erreicht
        else
          return ls_avail;
      } elif (eq(mode,S(t))) {
        return ls_avail;
      } else {
        funcall(mode,0); # mode aufrufen
        if (nullp(value1)) # keine Strings mehr zu erwarten?
          return ls_eof; # ja -> EOF erreicht
        else
          return ls_avail;
      }
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Buffered-Input-Stream.
# clear_input_buff_in(stream)
# > stream: Buffered-Input-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# can trigger GC
  local boolean clear_input_buff_in (object stream);
  local boolean clear_input_buff_in(stream)
    var object stream;
    {
      # Bearbeitung des aktuellen Strings beenden:
      var object index = TheStream(stream)->strm_buff_in_index; # Index
      var object endindex = TheStream(stream)->strm_buff_in_endindex;
      TheStream(stream)->strm_buff_in_index = endindex; # index := endindex
      if (eq(index,endindex))
        return FALSE;
      else
        return TRUE;
    }

LISPFUNN(make_buffered_input_stream,2)
# (MAKE-BUFFERED-INPUT-STREAM fun mode)
  {
    var object stream = # neuer Stream, nur READ-CHAR erlaubt
      allocate_stream(strmflags_rd_ch_B,strmtype_buff_in,strm_len+5,0);
    TheStream(stream)->strm_rd_by = P(rd_by_error);
    TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
    TheStream(stream)->strm_wr_by = P(wr_by_error);
    TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
    TheStream(stream)->strm_rd_ch = P(rd_ch_buff_in);
    TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
    TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_dummy);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_error);
    TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_error);
    TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
    TheStream(stream)->strm_buff_in_mode = popSTACK();
    TheStream(stream)->strm_buff_in_fun = popSTACK();
    TheStream(stream)->strm_buff_in_string = O(leer_string); # String := ""
    TheStream(stream)->strm_buff_in_index = Fixnum_0; # Index := 0
    TheStream(stream)->strm_buff_in_endindex = Fixnum_0; # Endindex := 0
    value1 = stream; mv_count=1; # stream als Wert
  }

LISPFUNN(buffered_input_stream_index,1)
# (SYSTEM::BUFFERED-INPUT-STREAM-INDEX buffered-input-stream) liefert den Index
  {
    var object stream = popSTACK(); # Argument
    # muss ein Buffered-Input-Stream sein:
    if (!(builtin_stream_p(stream) && (TheStream(stream)->strmtype == strmtype_buff_in))) {
      pushSTACK(stream);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: ~ is not a buffered input stream")
            );
    }
    var object index = TheStream(stream)->strm_buff_in_index;
    # Falls ein Character mit UNREAD-CHAR zurückgeschoben wurde,
    # verwende (1- index), ein Fixnum >=0, als Wert:
    if (TheStream(stream)->strmflags & strmflags_unread_B)
      index = fixnum_inc(index,-1);
    value1 = index; mv_count=1;
  }


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
# can trigger GC
  local void finish_output_buff_out (object stream);
  local void finish_output_buff_out(stream)
    var object stream;
    {
      pushSTACK(stream);
      var object string = TheStream(stream)->strm_buff_out_string; # String
      string = coerce_ss(string); # in Simple-String umwandeln (erzwingt ein Kopieren)
      stream = STACK_0; STACK_0 = string;
      # String durch Fill-Pointer:=0 leeren:
      TheIarray(TheStream(stream)->strm_buff_out_string)->dims[1] = 0;
      funcall(TheStream(stream)->strm_buff_out_fun,1); # Funktion aufrufen
    }

# UP: Bringt den wartenden Output eines Buffered-Output-Stream ans Ziel.
# force_output_buff_out(stream);
# > stream: Buffered-Output-Stream
# can trigger GC
  #define force_output_buff_out  finish_output_buff_out

# UP: Löscht den wartenden Output eines Buffered-Output-Stream.
# clear_output_buff_out(stream);
# > stream: Buffered-Output-Stream
# can trigger GC
  local void clear_output_buff_out (object stream);
  local void clear_output_buff_out(stream)
    var object stream;
    {
      # String durch Fill-Pointer:=0 leeren:
      TheIarray(TheStream(stream)->strm_buff_out_string)->dims[1] = 0;
      # Line-Position unverändert lassen??
    }

# WRITE-CHAR - Pseudofunktion für Buffered-Output-Streams:
  local void wr_ch_buff_out (const object* stream_, object ch);
  local void wr_ch_buff_out(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      var object stream = *stream_;
      # obj sollte Character sein:
      if (!charp(ch))
        fehler_wr_char(stream,ch);
      # Character in den String schieben:
      ssstring_push_extend(TheStream(stream)->strm_buff_out_string,char_code(ch));
      # Nach #\Newline den Buffer durchreichen:
      if (chareq(char_code(ch),ascii(NL)))
        force_output_buff_out(*stream_);
    }

# Schließt einen Buffered-Output-Stream.
# close_buff_out(stream);
# > stream : Buffered-Output-Stream
# can trigger GC
  local void close_buff_out (object stream);
  local void close_buff_out(stream)
    var object stream;
    {
      pushSTACK(stream); # stream retten
      finish_output_buff_out(stream);
      stream = popSTACK(); # stream zurück
      TheStream(stream)->strm_buff_out_fun = NIL; # Funktion := NIL
      TheStream(stream)->strm_buff_out_string = NIL; # String := NIL
    }

LISPFUN(make_buffered_output_stream,1,1,norest,nokey,0,NIL)
# (MAKE-BUFFERED-OUTPUT-STREAM fun [line-position])
  {
    # line-position überprüfen:
    if (eq(STACK_0,unbound)) {
      STACK_0 = Fixnum_0; # Defaultwert 0
    } else {
      # line-position angegeben, sollte ein Fixnum >=0 sein:
      if (!posfixnump(STACK_0))
        fehler_posfixnum(STACK_0);
    }
    # kleinen Semi-Simple-String der Länge 50 allozieren:
    pushSTACK(make_ssstring(50));
    var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
      allocate_stream(strmflags_wr_ch_B,strmtype_buff_out,strm_len+2,0);
    TheStream(stream)->strm_rd_by = P(rd_by_error);
    TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
    TheStream(stream)->strm_wr_by = P(wr_by_error);
    TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
    TheStream(stream)->strm_rd_ch = P(rd_ch_error);
    TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
    TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_error);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_buff_out);
    TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_dummy);
    TheStream(stream)->strm_buff_out_string = popSTACK(); # String eintragen
    TheStream(stream)->strm_wr_ch_lpos = popSTACK(); # Line Position eintragen
    TheStream(stream)->strm_buff_out_fun = popSTACK(); # Funktion eintragen
    value1 = stream; mv_count=1; # stream als Wert
  }


#ifdef GENERIC_STREAMS

# Generic Streams
# ===============

  # Contains a "controller object".
  # define strm_controller_object  strm_other[0]  # see lispbibl.d

  # The function GENERIC-STREAM-CONTROLLER will return some
  # object c associated with the stream s.

  #   (GENERIC-STREAM-READ-CHAR c)                      --> character or NIL
  #   (GENERIC-STREAM-PEEK-CHAR c)                      --> character or NIL
  #   (GENERIC-STREAM-READ-CHAR-WILL-HANG-P c)          --> {T,NIL}
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
    {
      pushSTACK(*stream_); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_rdch),1);
      return nullp(value1) ? eof_value : value1;
    }

  # (PEEK-CHAR s) ==
  # (GENERIC-STREAM-PEEK-CHAR c)
  local object pk_ch_generic (const object* stream_);
  local object pk_ch_generic(stream_)
    var const object* stream_;
    {
      pushSTACK(*stream_); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_pkch),1);
      if (nullp(value1))
        value1 = eof_value;
      if ((mv_count >= 2) && !nullp(value2)) {
        # READ-CHAR schon ausgeführt -> muss ein implizites UNREAD-CHAR
        # ausführen (d.h. das Ergebnis für das nächste READ-CHAR/PEEK-CHAR
        # aufheben).
        TheStream(*stream_)->strm_rd_ch_last = value1;
        if (!eq(value1,eof_value))
          TheStream(*stream_)->strmflags |= strmflags_unread_B;
      }
      return value1;
    }

  # (LISTEN s) ==
  # (IF (GENERIC-STREAM-READ-CHAR-WILL-HANG-P c)
  #   :WAIT
  #   (IF (GENERIC-STREAM-PEEK-CHAR c)
  #     :INPUT-AVAILABLE
  #     :EOF
  # ) )
  local signean listen_generic (object stream);
  local signean listen_generic(stream)
    var object stream;
    {
      pushSTACK(stream);
      pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_read_char_will_hang_p),1);
      if (!nullp(value1)) {
        skipSTACK(1); return ls_wait;
      }
      var object nextchar = pk_ch_generic(&STACK_0);
      skipSTACK(1);
      if (eq(nextchar,eof_value))
        return ls_eof;
      else
        return ls_avail;
    }

  # (CLEAR-INPUT s) ==
  # (GENERIC-STREAM-CLEAR-INPUT c)
  local boolean clear_input_generic (object stream);
  local boolean clear_input_generic(stream)
    var object stream;
    {
      pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_clear_input),1);
      return !nullp(value1);
    }

  # (WRITE-CHAR ch s) ==
  # (GENERIC-STREAM-WRITE-CHAR c ch)
  local void wr_ch_generic (const object* stream_, object ch);
  local void wr_ch_generic(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      # ch is a character, need not save it
      pushSTACK(*stream_); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); pushSTACK(ch); funcall(S(generic_stream_wrch),2);
    }

  # (WRITE-CHAR-ARRAY s string start len) ==
  # (GENERIC-STREAM-WRITE-STRING c string start len)
  local void wr_ch_array_generic (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_generic(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      pushSTACK(*stream_); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); pushSTACK(*chararray_);
      pushSTACK(UL_to_I(start)); pushSTACK(UL_to_I(len));
      funcall(S(generic_stream_wrss),4);
      var const chart* charptr;
      unpack_sstring_alloca(*chararray_,len,start, charptr=);
      wr_ss_lpos(*stream_,&charptr[len],len);
    }

  # (FINISH-OUTPUT s) ==
  # (GENERIC-STREAM-FINISH-OUTPUT c)
  local void finish_output_generic (object stream);
  local void finish_output_generic(stream)
    var object stream;
    {
      pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_finish_output),1);
    }

  # (FORCE-OUTPUT s) ==
  # (GENERIC-STREAM-FORCE-OUTPUT c)
  local void force_output_generic (object stream);
  local void force_output_generic(stream)
    var object stream;
    {
      pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_force_output),1);
    }

  # (CLEAR-OUTPUT s) ==
  # (GENERIC-STREAM-CLEAR-OUTPUT c)
  local void clear_output_generic (object stream);
  local void clear_output_generic(stream)
    var object stream;
    {
      pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_clear_output),1);
    }

  # (READ-BYTE s) ==
  # (GENERIC-STREAM-READ-BYTE c)
  local object rd_by_generic (object stream);
  local object rd_by_generic(stream)
    var object stream;
    {
      pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_rdby),1);
      return (nullp(value1) ? eof_value : value1);
    }

  # (WRITE-BYTE s i) ==
  # (GENERIC-STREAM-WRITE-BYTE c i)
  local void wr_by_generic (object stream, object obj);
  local void wr_by_generic(stream,obj)
    var object stream;
    var object obj;
    {
      pushSTACK(obj); # save obj
      pushSTACK(stream); funcall(L(generic_stream_controller),1);
      obj = STACK_0;
      STACK_0 = value1; pushSTACK(obj); funcall(S(generic_stream_wrby),2);
    }

  # (CLOSE s) ==
  # (GENERIC-STREAM-CLOSE c)
  local void close_generic(stream)
    var object stream;
    {
      pushSTACK(stream); funcall(L(generic_stream_controller),1);
      pushSTACK(value1); funcall(S(generic_stream_close),1);
    }

LISPFUNN(generic_stream_controller,1)
  {
    var object stream = popSTACK();
    if (!(builtin_stream_p(stream)
          && eq(TheStream(stream)->strm_rd_by,P(rd_by_generic))
          && eq(TheStream(stream)->strm_wr_by,P(wr_by_generic)))) {
      if (!streamp(stream)) {
        fehler_stream(stream);
      } else {
        pushSTACK(stream);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: stream must be a generic-stream, not ~")
              );
      }
    }
    value1=TheStream(stream)->strm_controller_object;
    mv_count=1;
  }

LISPFUNN(make_generic_stream,1)
  {
    var object stream =
      allocate_stream(strmflags_open_B,strmtype_generic,strm_len+1,0);
    TheStream(stream)->strm_rd_by = P(rd_by_generic);
    TheStream(stream)->strm_rd_by_array = P(rd_by_array_dummy);
    TheStream(stream)->strm_wr_by = P(wr_by_generic);
    TheStream(stream)->strm_wr_by_array = P(wr_by_array_dummy);
    TheStream(stream)->strm_rd_ch = P(rd_ch_generic);
    TheStream(stream)->strm_pk_ch = P(pk_ch_generic);
    TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_dummy);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_generic);
    TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_generic);
    TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
    TheStream(stream)->strm_controller_object = popSTACK();
    value1 = stream; mv_count=1; # stream als Wert
  }

LISPFUNN(generic_stream_p,1)
  {
    var object stream = popSTACK();
    if (!streamp(stream))
      fehler_stream(stream);
    if (builtin_stream_p(stream)
        && eq(TheStream(stream)->strm_rd_by,P(rd_by_generic))
        && eq(TheStream(stream)->strm_wr_by,P(wr_by_generic)))
      { value1 = T; mv_count=1; }
    else
      { value1 = NIL; mv_count=1; }
  }

#endif


# Streams communicating with the exterior world, based on bytes
# =============================================================

# They can be classified in three ways:
# According to strmtype:
#
#                      file  ----  strmtype_file
#                    /
#             handle
#           /        \           /  strmtype_pipe_in
#   channel            pipe  -----  strmtype_pipe_out
#           \
#             socket  -----   strmtype_x11socket
#                         \   strmtype_socket
#
# According to buffering:
#
#             unbuffered
#           /
#   channel
#           \
#             buffered
#
# According to element type:
#
#             CHARACTER or ([UN]SIGNED-BYTE n), n a multiple of 8 (setfable!)
#           /
#   channel
#           \
#             ([UN]SIGNED-BYTE n), n not a multiple of 8 (only if buffered)
#

# UP: Check a :BUFFERED argument.
# test_buffered_arg(arg)
# > object arg: argument
# > subr_self: calling function
# < signean buffered: +1 for T, -1 for NIL, 0 for :DEFAULT
  local signean test_buffered_arg (object arg);
  local signean test_buffered_arg(arg)
    var object arg;
    {
      if (eq(arg,unbound) || eq(arg,S(Kdefault)))
        return 0;
      if (eq(arg,NIL))
        return -1;
      if (eq(arg,T))
        return 1;
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: illegal :BUFFERED argument ~")
            );
    }

# Classification of possible :ELEMENT-TYPEs.
  typedef enum {
    eltype_ch,    # CHARACTER
    eltype_iu,    # (UNSIGNED-BYTE n)
    eltype_is     # (SIGNED-BYTE n)
  } eltype_kind;

# An analyzed :ELEMENT-TYPE argument.
  typedef struct {
    eltype_kind kind;
    uintL       size; # the n in ([UN]SIGNED-BYTE n),
                      # >0, <intDsize*uintWC_max,
                      # but 0 for eltype_ch
  } decoded_eltype;

# UP: Check a :ELEMENT-TYPE argument.
# test_eltype_arg(&eltype,&decoded);
# > object eltype: argument (in the STACK)
# > subr_self: calling function
# < subr_self: unchanged
# < decoded: decoded eltype
# can trigger GC
  local void test_eltype_arg (object* eltype_, decoded_eltype* decoded);
  local void test_eltype_arg(eltype_,decoded)
    var object* eltype_;
    var decoded_eltype* decoded;
    {
      var object arg = *eltype_;
      if (eq(arg,unbound) || eq(arg,S(character)) || eq(arg,S(string_char)) || eq(arg,S(Kdefault))) { # CHARACTER, STRING-CHAR, :DEFAULT
        decoded->kind = eltype_ch; decoded->size = 0; return;
      }
      if (eq(arg,S(bit))) { # BIT
        decoded->kind = eltype_iu; decoded->size = 1; return;
      }
      if (eq(arg,S(unsigned_byte))) { # UNSIGNED-BYTE
        decoded->kind = eltype_iu; decoded->size = 8; return;
      }
      if (eq(arg,S(signed_byte))) { # SIGNED-BYTE
        decoded->kind = eltype_is; decoded->size = 8; return;
      }
      var object eltype_size;
      if (consp(arg) && mconsp(Cdr(arg)) && nullp(Cdr(Cdr(arg)))) { # zweielementige Liste
        var object h = Car(arg);
        if (eq(h,S(mod))) { # (MOD n)
          decoded->kind = eltype_iu;
          h = Car(Cdr(arg)); # n
          # muss ein Integer >0 sein:
          if (!(integerp(h) && positivep(h) && !eq(h,Fixnum_0)))
            goto bad_eltype;
          # eltype_size := (integer-length (1- n)) bilden:
          pushSTACK(subr_self); # subr_self retten
          pushSTACK(h); funcall(L(einsminus),1); # (1- n)
          pushSTACK(value1); funcall(L(integer_length),1); # (integer-length (1- n))
          eltype_size = value1;
          subr_self = popSTACK(); # subr_self zurück
          goto eltype_integer;
        }
        if (eq(h,S(unsigned_byte))) { # (UNSIGNED-BYTE n)
          decoded->kind = eltype_iu;
          eltype_size = Car(Cdr(arg));
          goto eltype_integer;
        }
        if (eq(h,S(signed_byte))) { # (SIGNED-BYTE n)
          decoded->kind = eltype_is;
          eltype_size = Car(Cdr(arg));
          goto eltype_integer;
        }
      }
      pushSTACK(subr_self); # subr_self retten
      # Erstmal ein wenig kanonischer machen (damit die verschiedenen
      # SUBTYPEP dann nicht dreimal dasselbe machen müssen):
      pushSTACK(arg); funcall(S(canonicalize_type),1); # (SYS::CANONICALIZE-TYPE arg)
      pushSTACK(value1); # canon-arg retten
      pushSTACK(STACK_0); pushSTACK(S(character)); funcall(S(subtypep),2); # (SUBTYPEP canon-arg 'CHARACTER)
      if (!nullp(value1)) {
        skipSTACK(1);
        subr_self = popSTACK();
        decoded->kind = eltype_ch; decoded->size = 0;
        return;
      }
      funcall(S(subtype_integer),1); # (SYS::SUBTYPE-INTEGER canon-arg)
      subr_self = popSTACK(); # subr_self zurück
      if (!((mv_count>1) && integerp(value1) && integerp(value2)))
        goto bad_eltype;
      {
        # arg is a subtype of `(INTEGER ,low ,high) and
        # value1 = low, value2 = high.
        var uintL l;
        if (positivep(value1)) {
          l = I_integer_length(value2); # (INTEGER-LENGTH high)
          decoded->kind = eltype_iu;
        } else {
          var uintL l1 = I_integer_length(value1); # (INTEGER-LENGTH low)
          var uintL l2 = I_integer_length(value2); # (INTEGER-LENGTH high)
          l = (l1>l2 ? l1 : l2) + 1;
          decoded->kind = eltype_is;
        }
        eltype_size = fixnum(l);
      }
     eltype_integer:
      # eltype_size überprüfen:
      if (!(posfixnump(eltype_size) && !eq(eltype_size,Fixnum_0)
            && ((oint_data_len < log2_intDsize+intWCsize) # (Bei oint_data_len <= log2(intDsize)+intWCsize-1
                # ist stets eltype_size < 2^oint_data_len < intDsize*(2^intWCsize-1).)
                || (as_oint(eltype_size) < as_oint(fixnum(intDsize*(uintL)(bitm(intWCsize)-1))))
         ) )   )
        goto bad_eltype;
      decoded->size = posfixnum_to_L(eltype_size);
      return;
     bad_eltype:
      pushSTACK(*eltype_); pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: illegal :ELEMENT-TYPE argument ~")
            );
    }

# UP: Returns a canonical representation for a :ELEMENT-TYPE.
# canon_eltype(&decoded)
# > decoded: decoded eltype
# < result: either CHARACTER or ([UN]SIGNED-BYTE n)
# can trigger GC
  local object canon_eltype (const decoded_eltype* decoded);
  local object canon_eltype(decoded)
    var const decoded_eltype* decoded;
    {
      switch (decoded->kind) {
        case eltype_ch:
          # CHARACTER
          return S(character);
        case eltype_iu:
          # (UNSIGNED-BYTE bitsize)
          pushSTACK(S(unsigned_byte));
          pushSTACK(fixnum(decoded->size));
          return listof(2);
        case eltype_is:
          # (SIGNED-BYTE bitsize)
          pushSTACK(S(signed_byte));
          pushSTACK(fixnum(decoded->size));
          return listof(2);
        default: NOTREACHED;
      }
    }

# UP: Check a :EXTERNAL-FORMAT argument.
# test_external_format_arg(arg)
# > arg: argument
# > subr_self: calling function
# < subr_self: unchanged
# < result: an encoding
# can trigger GC
  local object test_external_format_arg (object arg);
  local object test_external_format_arg(arg)
    var object arg;
    {
      if (eq(arg,unbound) || eq(arg,S(Kdefault)))
        return O(default_file_encoding);
      if (encodingp(arg))
        return arg;
      #ifdef UNICODE
      if (symbolp(arg) && constantp(TheSymbol(arg))
          && encodingp(Symbol_value(arg))
         )
        return Symbol_value(arg);
      #if defined(GNU_LIBICONV) || defined(HAVE_ICONV)
      if (stringp(arg)) {
        # (make-encoding :charset arg)
        pushSTACK(arg); pushSTACK(unbound); pushSTACK(unbound); pushSTACK(unbound);
        C_make_encoding();
        return value1;
      }
      #endif
      #endif
      if (eq(arg,S(Kunix)) || eq(arg,S(Kmac)) || eq(arg,S(Kdos))) {
        # (make-encoding :charset default-file-encoding :line-terminator arg)
        pushSTACK(O(default_file_encoding)); pushSTACK(arg); pushSTACK(unbound); pushSTACK(unbound);
        C_make_encoding();
        return value1;
      }
      pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_external_format)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: illegal :EXTERNAL-FORMAT argument ~")
            );
    }

#if defined(UNIX) || defined(EMUNIX) || defined(DJUNIX) || defined(WATCOM) || defined(RISCOS)

# UP: Löscht bereits eingegebenen interaktiven Input von einem Handle.
  local void clear_tty_input (Handle handle);
  #if !(defined(DJUNIX) || defined(RISCOS))
  local void clear_tty_input(handle)
    var Handle handle;
    {
      # Methode 1: tcflush TCIFLUSH, siehe TERMIOS(3V)
      # Methode 2: ioctl TCFLSH TCIFLUSH, siehe TERMIO(4)
      # Methode 3: ioctl TIOCFLUSH FREAD, siehe TTCOMPAT(4)
      begin_system_call();
      #ifdef UNIX_TERM_TERMIOS
      if (!( TCFLUSH(handle,TCIFLUSH) ==0)) {
        if (!((errno==ENOTTY)||(errno==EINVAL))) { # kein TTY: OK
          local boolean flag = FALSE;
          if (!flag) { flag = TRUE; OS_error(); } # sonstigen Error melden, aber nur einmal
        }
      }
      #endif
      #ifdef UNIX_TERM_TERMIO
      #ifdef TCIFLUSH # !RISCOS
      if (!( ioctl(handle,TCFLSH,(CADDR_T)TCIFLUSH) ==0)) {
        if (!(errno==ENOTTY)) { # kein TTY: OK
          local boolean flag = FALSE;
          if (!flag) { flag = TRUE; OS_error(); } # sonstigen Error melden, aber nur einmal
        }
      }
      #endif
      #endif
      #ifdef UNIX_TERM_SGTTY
      #ifdef FREAD # !UNIX_MINT
      {
        var int arg = FREAD;
        if (!( ioctl(handle,TIOCFLUSH,&arg) ==0)) {
          if (!(errno==ENOTTY)) { # kein TTY: OK
            local boolean flag = FALSE;
            if (!flag) { flag = TRUE; OS_error(); } # sonstigen Error melden, aber nur einmal
          }
        }
      }
      #endif
      #endif
      #ifdef EMUNIX
      # Eberhard Mattes sagt, das funktioniert nur, wenn IDEFAULT nicht
      # gesetzt ist. ??
      if (!( ioctl(handle,TCFLSH,0) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); } # kein TTY: OK, sonstigen Error
      }
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
    {
      # Methode 1: fsync, siehe FSYNC(2)
      # Methode 2: tcdrain, siehe TERMIOS(3V)
      # Methode 3: ioctl TCSBRK 1, siehe TERMIO(4)
      # evtl. Methode 3: ioctl TCGETS/TCSETSW, siehe TERMIO(4)
      # oder (fast äquivalent) ioctl TIOCGETP/TIOCSETP, siehe TTCOMPAT(4)
      begin_system_call();
      #if !(defined(UNIX) && !defined(HAVE_FSYNC))
      if (!( fsync(handle) ==0)) {
        #if defined(UNIX_IRIX) || defined(EMUNIX)
        if (!(errno==ENOSYS))
        #endif
        if (!(errno==EINVAL)) { OS_error(); }
      }
      #endif
      #ifdef UNIX_TERM_TERMIOS
      if (!( TCDRAIN(handle) ==0)) {
        if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } # kein TTY: OK, sonstigen Error melden
      }
      #endif
      #ifdef UNIX_TERM_TERMIO
      if (!( ioctl(handle,TCSBRK,(CADDR_T)1) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); }
      }
      #endif
      #if defined(UNIX_TERM_TERMIOS) && defined(TCGETS) && defined(TCSETSW)
      {
        var struct termios term_parameters;
        if (!(   ( ioctl(handle,TCGETS,&term_parameters) ==0)
              && ( ioctl(handle,TCSETSW,&term_parameters) ==0)
           ) ) {
          if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } # kein TTY: OK, sonstigen Error melden
        }
      }
      #endif
      #ifdef EMUNIX
      {
        var struct termio term_parameters;
        if (!(   ( ioctl(handle,TCGETA,&term_parameters) ==0)
              && ( ioctl(handle,TCSETAW,&term_parameters) ==0)
           ) ) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
      }
      #endif
      #if 0 # Vorsicht: das müsste FINISH-OUTPUT und CLEAR-INPUT bewirken!
      {
        var struct sgttyb tty_parameters;
        if (!(   ( ioctl(handle,TIOCGETP,&tty_parameters) ==0)
              && ( ioctl(handle,TIOCSETP,&tty_parameters) ==0)
           ) ) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
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
    {
      # Methode: fsync, siehe FSYNC(2)
      begin_system_call();
      if (!( fsync(handle) ==0)) {
        #if defined(UNIX_IRIX) || defined(EMUNIX)
        if (!(errno==ENOSYS))
        #endif
        #ifdef UNIX_CYGWIN32 # needed for Win95 only
        if (!(errno==EBADF))
        #endif
        if (!(errno==EINVAL)) { OS_error(); }
      }
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
    {
      # Methode 1: tcflush TCOFLUSH, siehe TERMIOS(3V)
      # Methode 2: ioctl TCFLSH TCOFLUSH, siehe TERMIO(4)
      # Methode 3: ioctl TIOCFLUSH FWRITE, siehe TTCOMPAT(4)
      begin_system_call();
      #ifdef UNIX_TERM_TERMIOS
      if (!( TCFLUSH(handle,TCOFLUSH) ==0)) {
        #ifdef UNIX_IRIX
        if (!(errno==ENOSYS))
        #endif
        if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } # kein TTY: OK, sonstigen Error melden
      }
      #endif
      #ifdef UNIX_TERM_TERMIO
      #ifdef TCOFLUSH # !RISCOS
      if (!( ioctl(handle,TCFLSH,(CADDR_T)TCOFLUSH) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); } # kein TTY: OK, sonstigen Error melden
      }
      #endif
      #endif
      #ifdef UNIX_TERM_SGTTY
      #ifdef FWRITE # !UNIX_MINT
      {
        var int arg = FWRITE;
        if (!( ioctl(handle,TIOCFLUSH,&arg) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); } # kein TTY: OK, sonstigen Error melden
        }
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
    {
      begin_system_call();
      # Maybe it's a serial communication.
      if (!PurgeComm(handle,PURGE_RXABORT|PURGE_RXCLEAR)) {
        if (!(GetLastError()==ERROR_INVALID_HANDLE || GetLastError()==ERROR_INVALID_FUNCTION))
          { OS_error(); }
      }
      # Maybe it's a console.
      if (!FlushConsoleInputBuffer(handle)) {
        if (!(GetLastError()==ERROR_INVALID_HANDLE))
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
      if (!PurgeComm(handle,PURGE_TXABORT|PURGE_TXCLEAR)) {
        if (!(GetLastError()==ERROR_INVALID_HANDLE || GetLastError()==ERROR_INVALID_FUNCTION))
          { OS_error(); }
      }
      end_system_call();
    }

#endif

#if defined(AMIGAOS)

# UP: Löscht bereits eingegebenen interaktiven Input von einem Handle.
  local void clear_tty_input (Handle handle);
  #define clear_tty_input(handle)

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
      #if defined(UNIX)
        var struct stat statbuf;
        begin_system_call();
        if (!( fstat(handle,&statbuf) ==0)) { OS_error(); }
        end_system_call();
        return (S_ISREG(statbuf.st_mode) || S_ISBLK(statbuf.st_mode) ? TRUE : FALSE);
      #endif
      #if defined(MSDOS) || defined(RISCOS)
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


# Channel-Streams
# ===============

# Channel streams are a common framework which perform their input/output
# via a channel from the operating system. Encompasses: terminal stream,
# file stream, pipe stream, socket stream.

# Because the input side has some non-GCed fields, all channel streams must
# have the same number of GCed fields.

# Fields used for both the input side and the output side:

  # define strm_eltype   strm_other[0] # CHARACTER or ([UN]SIGNED-BYTE n)

  # define strm_encoding strm_other[1] # an Encoding
                                       # (used if eltype = CHARACTER only)

  #define strm_bitbuffer strm_other[2] # (used if eltype /= CHARACTER only)

  #define strm_buffer    strm_other[3] # (used by buffered streams only)

# Fields used for the input side only:

  #define strm_isatty    strm_other[3] # /=NIL or NIL, depending whether the
                                       # input channel is a tty handle and
                                       # therefore needs special treatment in
                                       # the low_listen function on some OSes
                                       # (used by unbuffered streams only)
  #define strm_ichannel  strm_other[4] # the input channel,
                                       # an encapsulated handle, or, on
                                       # WIN32_NATIVE, an encapsulated SOCKET

# Fields used for the output side only:

  #define strm_ochannel  strm_other[5] # the output channel,
                                       # an encapsulated handle, or, on
                                       # WIN32_NATIVE, an encapsulated SOCKET

# Fields reserved for the specialized stream:

  #define strm_field1    strm_other[6]
  #define strm_field2    strm_other[7]

# Binary fields start here.
  #define strm_channel_extrafields  strm_other[8]
#define strm_channel_len  (strm_len+8)

# Additional binary (not GCed) fields:
typedef struct strm_channel_extrafields_struct {
  boolean buffered;                    # FALSE for unbuffered streams,
                                       # TRUE for buffered streams
  uintL bitsize;                       # If the element-type is ([UN]SIGNED-BYTE n):
                                       #   n = number of bits per unit,
                                       #   >0, <intDsize*uintWC_max.
                                       # If the element-type is CHARACTER: 0.
  void (* low_close) (object stream, object handle);
  # Fields used if the element-type is CHARACTER:
  uintL lineno;                        # line number during read, >0
  #if defined(UNICODE) && (defined(GNU_LIBICONV) || defined(HAVE_ICONV))
  iconv_t iconvdesc;                   # input conversion descriptor and state
  iconv_t oconvdesc;                   # output conversion descriptor and state
  #endif
} strm_channel_extrafields_struct;

# Accessors.
#define ChannelStream_eltype(stream)  TheStream(stream)->strm_eltype
#define ChannelStream_isatty(stream)  TheStream(stream)->strm_isatty
#define ChannelStream_ichannel(stream)  TheStream(stream)->strm_ichannel
#define ChannelStream_ochannel(stream)  TheStream(stream)->strm_ochannel
#define ChannelStream_buffered(stream)   ((strm_channel_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->buffered
#define ChannelStream_bitsize(stream)   ((strm_channel_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->bitsize
#define ChannelStreamLow_close(stream)   ((strm_channel_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_close
#define ChannelStream_lineno(stream)   ((strm_channel_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->lineno
#if defined(UNICODE) && (defined(GNU_LIBICONV) || defined(HAVE_ICONV))
#define ChannelStream_iconvdesc(stream)   ((strm_channel_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->iconvdesc
#define ChannelStream_oconvdesc(stream)   ((strm_channel_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->oconvdesc
#endif

# Additional binary (not GCed) fields, used by unbuffered streams only:
typedef struct strm_unbuffered_extrafields_struct {
  strm_channel_extrafields_struct _parent;
  # The low_... operations operate on bytes only, and independently of the
  # stream's element type. They cannot cause GC.
  # Fields used for the input side only:
  sintL        (* low_read)          (object stream);
  signean      (* low_listen)        (object stream);
  boolean      (* low_clear_input)   (object stream);
  uintB*       (* low_read_array)    (object stream, uintB* byteptr, uintL len);
  sintL status;                        # -1 means EOF reached
                                       # 0 means unknown, bytebuf invalid
                                       # >0 means the number of valid bytes in
                                       #    bytebuf, to be consumed
  uintB bytebuf[max_bytes_per_chart];  # the last bytes read but not yet consumed
  #ifdef AMIGAOS
  LONG rawp;                           # current mode: 0 = CON, 1 = RAW
  #endif
    # For general interoperability with Win32 systems, we recognize all possible
    # line-terminators: LF, CR/LF and CR, independently of strm_encoding. This
    # is because, when confronted to Unix-style text files (eol = LF), some
    # Microsoft editors insert new lines with eol = CR/LF, while other Microsoft
    # editors insert new lines with eol = CR. Java learned the lesson and
    # understands all three line-terminators. So do we.
  boolean ignore_next_LF : 8;          # TRUE after reading a CR
  # Fields used for the output side only:
  void         (* low_write)         (object stream, uintB b);
  const uintB* (* low_write_array)   (object stream, const uintB* byteptr, uintL len);
  void         (* low_finish_output) (object stream);
  void         (* low_force_output)  (object stream);
  void         (* low_clear_output)  (object stream);
} strm_unbuffered_extrafields_struct;

# Accessors.
#define UnbufferedStreamLow_read(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_read
#define UnbufferedStreamLow_listen(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_listen
#define UnbufferedStreamLow_clear_input(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_clear_input
#define UnbufferedStreamLow_read_array(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_read_array
#define UnbufferedStream_status(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->status
#define UnbufferedStream_bytebuf(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->bytebuf
#ifdef AMIGAOS
#define UnbufferedStream_rawp(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->rawp
#endif
#define UnbufferedStream_ignore_next_LF(stream)   ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->ignore_next_LF
#define UnbufferedStreamLow_write(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_write
#define UnbufferedStreamLow_write_array(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_write_array
#define UnbufferedStreamLow_finish_output(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_finish_output
#define UnbufferedStreamLow_force_output(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_force_output
#define UnbufferedStreamLow_clear_output(stream)  ((strm_unbuffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_clear_output

# Error message after user interrupt.
# fehler_interrupt();
# > subr_self: calling function
  nonreturning_function(local, fehler_interrupt, (void));
  local void fehler_interrupt()
    {
      pushSTACK(TheSubr(subr_self)->name);
      fehler(serious_condition,
             GETTEXT("~: Ctrl-C: User break")
            );
    }

# General Subroutines
# ===================

# iconv-based encodings
# ---------------------

# Here enc_charset is a simple-string, not a symbol. The system decides
# which encodings are available, and there is no API for getting them all.

#if defined(UNICODE) && (defined(GNU_LIBICONV) || defined(HAVE_ICONV))

# Our internal encoding is UCS-2 with platform dependent endianness.
#ifdef GNU_LIBICONV
  #define CLISP_INTERNAL_CHARSET  "UCS-2-INTERNAL"
#else
  #ifdef __GLIBC__
    #if BIG_ENDIAN_P
      #define CLISP_INTERNAL_CHARSET  "UNICODEBIG"
    #else
      #define CLISP_INTERNAL_CHARSET  "UNICODELITTLE"
    #endif
  #elif defined(UNIX_HPUX) && BIG_ENDIAN_P
    #define CLISP_INTERNAL_CHARSET  "ucs2"
  #else
    #if BIG_ENDIAN_P
      #define CLISP_INTERNAL_CHARSET  "UCS-2"
    #else
      #define CLISP_INTERNAL_CHARSET  "UCS-2"  # FIXME: This is probably wrong
    #endif
  #endif
#endif

# min. bytes per character = 1
# max. bytes per character unknown, assume it's <= max_bytes_per_chart

global uintL iconv_mblen (object encoding, const uintB* src, const uintB* srcend);
global void iconv_mbstowcs (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend);
global uintL iconv_wcslen (object encoding, const chart* src, const chart* srcend);
global void iconv_wcstombs (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend);
global object iconv_range (object encoding, uintL start, uintL end);

# fehler_iconv_invalid_charset(encoding);
  nonreturning_function(local, fehler_iconv_invalid_charset, (object encoding));
  local void fehler_iconv_invalid_charset(encoding)
    var object encoding;
    {
      pushSTACK(TheEncoding(encoding)->enc_charset);
      fehler(error,
             GETTEXT("unknown character set ~")
            );
    }

# Error, when a character cannot be converted to an encoding.
# fehler_unencodable(encoding);
  nonreturning_function(extern, fehler_unencodable, (object encoding, chart ch));

# Initialize an iconv_t as we need it.
# iconv_init(cd);
  #ifdef GNU_LIBICONV
    /* We want reversible conversion, no transliteration. */
    #define iconv_init(cd)  \
      { int zero = 0; iconvctl(cd,ICONV_SET_TRANSLITERATE,&zero); }
  #else
    #define iconv_init(cd)
  #endif

# Bytes to characters.

global uintL iconv_mblen(encoding,src,srcend)
  var object encoding;
  var const uintB* src;
  var const uintB* srcend;
  {
    var uintL count = 0;
    #define tmpbufsize 4096
    var chart tmpbuf[tmpbufsize];
    with_sstring_0(TheEncoding(encoding)->enc_charset,Symbol_value(S(ascii)),charset_asciz, {
      begin_system_call();
      var iconv_t cd = iconv_open(CLISP_INTERNAL_CHARSET,charset_asciz);
      if (cd == (iconv_t)(-1)) {
        if (errno == EINVAL) { end_system_call(); fehler_iconv_invalid_charset(encoding); }
        OS_error();
      }
      iconv_init(cd);
      {
        var const char* inptr = src;
        var size_t insize = srcend-src;
        while (insize > 0) {
          var char* outptr = (char*)tmpbuf;
          var size_t outsize = tmpbufsize*sizeof(chart);
          var size_t res = iconv(cd,&inptr,&insize,&outptr,&outsize);
          if (res == (size_t)(-1) && errno != E2BIG) {
            if (errno == EINVAL) # incomplete input?
              break;
            elif (errno == EILSEQ) {
              ASSERT(insize > 0);
              var object action = TheEncoding(encoding)->enc_towcs_error;
              if (eq(action,S(Kignore))) {
                inptr++; insize--;
              } elif (eq(action,S(Kerror))) {
                iconv_close(cd); errno = EILSEQ; OS_error();
              } else {
                outptr += sizeof(chart);
                inptr++; insize--;
              }
            } else {
              var int saved_errno = errno;
              iconv_close(cd);
              errno = saved_errno;
              OS_error();
            }
          }
          count += (outptr-(char*)tmpbuf);
        }
      }
      if (iconv_close(cd) < 0) { OS_error(); }
      end_system_call();
    });
    #undef tmpbufsize
    return count/sizeof(chart);
  }

global void iconv_mbstowcs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const uintB* *srcp;
  var const uintB* srcend;
  var chart* *destp;
  var chart* destend;
  {
    var const char* inptr = (const char*)*srcp;
    var size_t insize = srcend-*srcp;
    var char* outptr = (char*)*destp;
    var size_t outsize = (char*)destend-(char*)*destp;
    if (eq(stream,nullobj)) {
      # Standalone call, must be consistent with iconv_mblen:
      with_sstring_0(TheEncoding(encoding)->enc_charset,Symbol_value(S(ascii)),charset_asciz, {
        begin_system_call();
        var iconv_t cd = iconv_open(CLISP_INTERNAL_CHARSET,charset_asciz);
        if (cd == (iconv_t)(-1)) {
          if (errno == EINVAL) { end_system_call(); fehler_iconv_invalid_charset(encoding); }
          OS_error();
        }
        iconv_init(cd);
        while (insize > 0 && outsize > 0) {
          var size_t res = iconv(cd,&inptr,&insize,&outptr,&outsize);
          if (res == (size_t)(-1)) {
            if (errno == EINVAL) { # incomplete input?
              inptr += insize; break;
            } elif (errno == EILSEQ) {
              ASSERT(insize > 0);
              var object action = TheEncoding(encoding)->enc_towcs_error;
              if (eq(action,S(Kignore))) {
                inptr++; insize--;
              } elif (eq(action,S(Kerror))) {
                iconv_close(cd); errno = EILSEQ; OS_error();
              } else {
                if (outsize < sizeof(chart))
                  break;
                *(chart*)outptr = char_code(action); outptr += sizeof(chart); outsize -= sizeof(chart);
                inptr++; insize--;
              }
            } else {
              var int saved_errno = errno;
              iconv_close(cd);
              errno = saved_errno;
              OS_error();
            }
          }
        }
        if (iconv_close(cd) < 0) { OS_error(); }
        end_system_call();
        ASSERT(insize == 0 && outsize == 0);
      });
    } else {
      # Called from a channel-stream.
      var iconv_t cd = ChannelStream_iconvdesc(stream);
      begin_system_call();
      while (insize > 0) {
        var size_t res = iconv(cd,&inptr,&insize,&outptr,&outsize);
        if (res == (size_t)(-1)) {
          if (errno == EINVAL) # incomplete input?
            break;
          elif (errno == E2BIG) # output buffer full?
            break;
          elif (errno == EILSEQ) {
            ASSERT(insize > 0);
            var object action = TheEncoding(encoding)->enc_towcs_error;
            if (eq(action,S(Kignore))) {
              inptr++; insize--;
            } elif (eq(action,S(Kerror))) {
              if (inptr > (const char*)*srcp)
                break;
              OS_error();
            } else {
              if (outsize < sizeof(chart))
                break;
              *(chart*)outptr = char_code(action); outptr += sizeof(chart); outsize -= sizeof(chart);
              inptr++; insize--;
            }
          } else {
            OS_error();
          }
        }
      }
      end_system_call();
    }
    *srcp = (const uintB*)inptr;
    *destp = (chart*)outptr;
  }

# Characters to bytes.

global uintL iconv_wcslen(encoding,src,srcend)
  var object encoding;
  var const chart* src;
  var const chart* srcend;
  {
    var uintL count = 0;
    #define tmpbufsize 4096
    var uintB tmpbuf[tmpbufsize];
    with_sstring_0(TheEncoding(encoding)->enc_charset,Symbol_value(S(ascii)),charset_asciz, {
      begin_system_call();
      var iconv_t cd = iconv_open(charset_asciz,CLISP_INTERNAL_CHARSET);
      if (cd == (iconv_t)(-1)) {
        if (errno == EINVAL) { end_system_call(); fehler_iconv_invalid_charset(encoding); }
        OS_error();
      }
      iconv_init(cd);
      {
        var const char* inptr = (const char*)src;
        var size_t insize = (char*)srcend-(char*)src;
        while (insize > 0) {
          var char* outptr = (char*)tmpbuf;
          var size_t outsize = tmpbufsize;
          var size_t res = iconv(cd,&inptr,&insize,&outptr,&outsize);
          if (res == (size_t)(-1) && errno != E2BIG) {
            if (errno == EILSEQ) { # invalid input?
              ASSERT(insize >= sizeof(chart));
              var object action = TheEncoding(encoding)->enc_tombs_error;
              if (eq(action,S(Kignore))) {
                inptr += sizeof(chart); insize -= sizeof(chart);
              } elif (uint8_p(action)) {
                outptr++; outsize--;
                inptr += sizeof(chart); insize -= sizeof(chart);
              } elif (!eq(action,S(Kerror))) {
                var chart c = char_code(action);
                var const char* inptr1 = (const char*)&c;
                var size_t insize1 = sizeof(c);
                if (!(iconv(cd,&inptr1,&insize1,&outptr,&outsize) == (size_t)(-1))) {
                  inptr += sizeof(chart); insize -= sizeof(chart);
                } else {
                  if (errno != EILSEQ) {
                    OS_error();
                  } else {
                    end_system_call();
                    fehler_unencodable(encoding,*(const chart*)inptr);
                  }
                }
              } else {
                end_system_call();
                fehler_unencodable(encoding,*(const chart*)inptr);
              }
            } elif (errno == EINVAL) { # incomplete input?
              NOTREACHED
            } else {
              var int saved_errno = errno;
              iconv_close(cd);
              errno = saved_errno;
              OS_error();
            }
          }
          count += (outptr-(char*)tmpbuf);
        }
      }
      {
        var char* outptr = (char*)tmpbuf;
        var size_t outsize = tmpbufsize;
        var size_t res = iconv(cd,NULL,NULL,&outptr,&outsize);
        if (res == (size_t)(-1)) {
          if (errno == E2BIG) { # output buffer too small?
            NOTREACHED
          } else {
            var int saved_errno = errno;
            iconv_close(cd);
            errno = saved_errno;
            OS_error();
          }
        }
        count += (outptr-(char*)tmpbuf);
      }
      if (iconv_close(cd) < 0) { OS_error(); }
      end_system_call();
    });
    #undef tmpbufsize
    return count;
  }

global void iconv_wcstombs(encoding,stream,srcp,srcend,destp,destend)
  var object encoding;
  var object stream;
  var const chart* *srcp;
  var const chart* srcend;
  var uintB* *destp;
  var uintB* destend;
  {
    var const char* inptr = (char*)*srcp;
    var size_t insize = (char*)srcend-(char*)*srcp;
    var char* outptr = *destp;
    var size_t outsize = destend-*destp;
    if (eq(stream,nullobj)) {
      # Standalone call, must be consistent with iconv_wcslen:
      with_sstring_0(TheEncoding(encoding)->enc_charset,Symbol_value(S(ascii)),charset_asciz, {
        begin_system_call();
        var iconv_t cd = iconv_open(charset_asciz,CLISP_INTERNAL_CHARSET);
        if (cd == (iconv_t)(-1)) {
          if (errno == EINVAL) { end_system_call(); fehler_iconv_invalid_charset(encoding); }
          OS_error();
        }
        iconv_init(cd);
        while (insize > 0) {
          var size_t res = iconv(cd,&inptr,&insize,&outptr,&outsize);
          if (res == (size_t)(-1)) {
            if (errno == EILSEQ) { # invalid input?
              ASSERT(insize >= sizeof(chart));
              var object action = TheEncoding(encoding)->enc_tombs_error;
              if (eq(action,S(Kignore))) {
                inptr += sizeof(chart); insize -= sizeof(chart);
              } elif (uint8_p(action)) {
                *outptr++ = I_to_uint8(action); outsize--;
                inptr += sizeof(chart); insize -= sizeof(chart);
              } elif (!eq(action,S(Kerror))) {
                var chart c = char_code(action);
                var const char* inptr1 = (const char*)&c;
                var size_t insize1 = sizeof(c);
                if (!(iconv(cd,&inptr1,&insize1,&outptr,&outsize) == (size_t)(-1))) {
                  inptr += sizeof(chart); insize -= sizeof(chart);
                } else {
                  if (errno != EILSEQ) {
                    OS_error();
                  } else {
                    end_system_call();
                    fehler_unencodable(encoding,*(const chart*)inptr);
                  }
                }
              } else {
                end_system_call();
                fehler_unencodable(encoding,*(const chart*)inptr);
              }
            } elif (errno == EINVAL) { # incomplete input?
              NOTREACHED
            } elif (errno == E2BIG) { # output buffer too small?
              NOTREACHED
            } else {
              var int saved_errno = errno;
              iconv_close(cd);
              errno = saved_errno;
              OS_error();
            }
          }
        }
        {
          var size_t res = iconv(cd,NULL,NULL,&outptr,&outsize);
          if (res == (size_t)(-1)) {
            if (errno == E2BIG) { # output buffer too small?
              NOTREACHED
            } else {
              var int saved_errno = errno;
              iconv_close(cd);
              errno = saved_errno;
              OS_error();
            }
          }
        }
        if (iconv_close(cd) < 0) { OS_error(); }
        end_system_call();
        # Now insize == 0, and if iconv_wcslen has been used to determine
        # the destination size, then also outsize == 0.
      });
    } else {
      # Called from a channel-stream.
      var iconv_t cd = ChannelStream_oconvdesc(stream);
      begin_system_call();
      while (insize > 0) {
        var size_t res = iconv(cd,&inptr,&insize,&outptr,&outsize);
        if (res == (size_t)(-1)) {
          if (errno == EILSEQ) { # invalid input?
            ASSERT(insize >= sizeof(chart));
            var object action = TheEncoding(encoding)->enc_tombs_error;
            if (eq(action,S(Kignore))) {
              inptr += sizeof(chart); insize -= sizeof(chart);
            } elif (uint8_p(action)) {
              if (outsize == 0)
                break;
              *outptr++ = I_to_uint8(action); outsize--;
              inptr += sizeof(chart); insize -= sizeof(chart);
            } elif (!eq(action,S(Kerror))) {
              var chart c = char_code(action);
              var const char* inptr1 = (const char*)&c;
              var size_t insize1 = sizeof(c);
              if (!(iconv(cd,&inptr1,&insize1,&outptr,&outsize) == (size_t)(-1))) {
                inptr += sizeof(chart); insize -= sizeof(chart);
              } else {
                if (errno == E2BIG)
                  break;
                elif (errno != EILSEQ) {
                  OS_error();
                } else {
                  if (inptr > (char*)*srcp)
                    break;
                  end_system_call();
                  fehler_unencodable(encoding,*(const chart*)inptr);
                }
              }
            } else {
              if (inptr > (char*)*srcp)
                break;
              end_system_call();
              fehler_unencodable(encoding,*(const chart*)inptr);
            }
          } elif (errno == EINVAL) { # incomplete input?
            NOTREACHED
          } elif (errno == E2BIG) { # output buffer full?
            break;
          } else {
            OS_error();
          }
        }
      }
      end_system_call();
    }
    *srcp = (const chart*)inptr;
    *destp = (uintB*)outptr;
  }

# Determining the range of encodable characters.
global object iconv_range(encoding,start,end)
  var object encoding;
  var uintL start;
  var uintL end;
  {
    var uintL count = 0; # number of intervals already on the STACK
    with_sstring_0(TheEncoding(encoding)->enc_charset,Symbol_value(S(ascii)),charset_asciz, {
      begin_system_call();
      var iconv_t cd = iconv_open(charset_asciz,CLISP_INTERNAL_CHARSET);
      if (cd == (iconv_t)(-1)) {
        if (errno == EINVAL) { end_system_call(); fehler_iconv_invalid_charset(encoding); }
        OS_error();
      }
      iconv_init(cd);
      end_system_call();
      {
        var uintL i1;
        var uintL i2;
        var boolean have_i1_i2 = FALSE; # [i1,i2] = interval being built
        var uintL i;
        for (i = start;;) {
          var chart ch = as_chart(i);
          var uintB buf[max_bytes_per_chart];
          var const char* inptr = (const char*)&ch;
          var size_t insize = sizeof(chart);
          var char* outptr = (char*)&buf[0];
          var size_t outsize = max_bytes_per_chart;
          begin_system_call();
          {
            var size_t res = iconv(cd,&inptr,&insize,&outptr,&outsize);
            if (res == (size_t)(-1)) {
              if (errno == EILSEQ) { # invalid input?
                end_system_call();
                # ch not encodable -> finish the interval
                if (have_i1_i2) {
                  pushSTACK(code_char(as_chart(i1))); pushSTACK(code_char(as_chart(i2)));
                  check_STACK(); count++;
                }
                have_i1_i2 = FALSE;
              } elif (errno == EINVAL) { # incomplete input?
                NOTREACHED
              } elif (errno == E2BIG) { # output buffer too small?
                NOTREACHED
              } else {
                var int saved_errno = errno;
                iconv_close(cd);
                errno = saved_errno;
                OS_error();
              }
            } else {
              end_system_call();
              # ch encodable -> extend the interval
              if (!have_i1_i2) {
                have_i1_i2 = TRUE;
                i1 = i;
              }
              i2 = i;
            }
          }
          if (i == end)
            break;
          i++;
        }
        if (have_i1_i2) {
          pushSTACK(code_char(as_chart(i1))); pushSTACK(code_char(as_chart(i2)));
          check_STACK(); count++;
        }
      }
      begin_system_call();
      if (iconv_close(cd) < 0) { OS_error(); }
      end_system_call();
    });
    return stringof(2*count);
  }

#endif # UNICODE && (GNU_LIBICONV || HAVE_ICONV)

# Initializes some ChannelStream fields.
# ChannelStream_init(stream);
# > stream: channel-stream with encoding
#if defined(UNICODE) && (defined(GNU_LIBICONV) || defined(HAVE_ICONV))
  local void ChannelStream_init (object stream);
  local void ChannelStream_init(stream)
    var object stream;
    {
      var object encoding = TheStream(stream)->strm_encoding;
      if (simple_string_p(TheEncoding(encoding)->enc_charset)) {
        with_sstring_0(TheEncoding(encoding)->enc_charset,Symbol_value(S(ascii)),charset_asciz, {
          var uintB flags = TheStream(stream)->strmflags;
          if (flags & strmflags_rd_B) {
            begin_system_call();
            var iconv_t cd = iconv_open(CLISP_INTERNAL_CHARSET,charset_asciz);
            if (cd == (iconv_t)(-1)) {
              if (errno == EINVAL) { end_system_call(); fehler_iconv_invalid_charset(encoding); }
              OS_error();
            }
            iconv_init(cd);
            ChannelStream_iconvdesc(stream) = cd;
          } else {
            ChannelStream_iconvdesc(stream) = (iconv_t)0;
          }
          if (flags & strmflags_wr_B) {
            begin_system_call();
            var iconv_t cd = iconv_open(charset_asciz,CLISP_INTERNAL_CHARSET);
            if (cd == (iconv_t)(-1)) {
              if (errno == EINVAL) { end_system_call(); fehler_iconv_invalid_charset(encoding); }
              OS_error();
            }
            iconv_init(cd);
            ChannelStream_oconvdesc(stream) = cd;
          } else {
            ChannelStream_oconvdesc(stream) = (iconv_t)0;
          }
        });
      } else {
        ChannelStream_iconvdesc(stream) = (iconv_t)0;
        ChannelStream_oconvdesc(stream) = (iconv_t)0;
      }
    }
#else
  #define ChannelStream_init(stream)
#endif

# Cleans up some ChannelStream fields.
# ChannelStream_fini(stream);
#if defined(UNICODE) && (defined(GNU_LIBICONV) || defined(HAVE_ICONV))
  local void ChannelStream_fini (object stream);
  local void ChannelStream_fini(stream)
    var object stream;
    {
      if (ChannelStream_iconvdesc(stream) != (iconv_t)0) {
        begin_system_call();
        if (iconv_close(ChannelStream_iconvdesc(stream)) < 0) { OS_error(); }
        end_system_call();
        ChannelStream_iconvdesc(stream) = (iconv_t)0;
      }
      if (ChannelStream_oconvdesc(stream) != (iconv_t)0) {
        begin_system_call();
        if (iconv_close(ChannelStream_oconvdesc(stream)) < 0) { OS_error(); }
        end_system_call();
        ChannelStream_oconvdesc(stream) = (iconv_t)0;
      }
    }
#else
  #define ChannelStream_fini(stream)
#endif

# Closes a handle.
  local void low_close_handle (object stream, object handle);
  local void low_close_handle(stream,handle)
    var object stream;
    var object handle;
    {
      begin_system_call();
      #if defined(UNIX) || defined(MSDOS) || defined(AMIGAOS) || defined(RISCOS)
      if (!( CLOSE(TheHandle(handle)) ==0)) { end_system_call(); OS_filestream_error(stream); }
      #endif
      #ifdef WIN32_NATIVE
      if (!CloseHandle(TheHandle(handle))) { end_system_call(); OS_filestream_error(stream); }
      #endif
      end_system_call();
    }

# Subroutines for Integer-Streams
# ===============================

# For file streams with element type INTEGER ("byte files") every integer
# uses the same amount of bits. The bits and bytes are stored in little-endian
# order, because big-endian order would lead to madness. So the bit number i
# of element number j is = bit number (i+j*bitsize) of the entire bit stream
# = bit number ((i+j*bitsize) mod 8) in byte number floor((i+j*bitsize)/8).

# strm_bitbuffer is a simple-bit-vector with ceiling(bitsize/8)*8 bits,
# filled in little-endian order.

# All subroutines below get passed as arguments: bitsize (size per unit, it is
# also stored in the stream), bytesize = ceiling(bitsize/8) = number of bytes
# the bitbuffer can hold.

# Note that unbuffered file-streams cannot be of type ib or ic (too
# complicated for the moment), only type ia is supported for them.

# Subroutines for the Input side
# ------------------------------

# UP für READ-BYTE auf File-Streams für Integers, Art u :
# Liefert die im Bitbuffer enthaltenen bytesize Bytes als Integer >=0.
# can trigger GC
  local object rd_by_iu_I (object stream, uintL bitsize, uintL bytesize);
  local object rd_by_iu_I(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    {
      var object bitbuffer = TheStream(stream)->strm_bitbuffer;
      # Zahl im bitbuffer normalisieren:
      var uintB* bitbufferptr = &TheSbvector(bitbuffer)->data[bytesize-1];
      *bitbufferptr &= (bit(((bitsize-1)%8)+1)-1); # High byte maskieren
      var uintL count = bytesize;
      while ((!(count==0)) && (*bitbufferptr==0)) { count--; bitbufferptr--; }
      # Zahl bilden:
      if # höchstens oint_data_len Bits ?
         ((count <= floor(oint_data_len,8))
          || ((count == floor(oint_data_len,8)+1)
              && (*bitbufferptr < bit(oint_data_len%8))
         )   ) {
        # ja -> Fixnum >=0 bilden:
        var uintL wert = 0;
        until (count==0) { wert = (wert<<8) | *bitbufferptr--; count--; }
        return fixnum(wert);
      } else {
        # nein -> Bignum >0 bilden:
        pushSTACK(bitbuffer);
        var uintL digitcount = floor(count,(intDsize/8));
        if (((count%(intDsize/8)) > 0) || (*bitbufferptr & bit(7)))
          digitcount++;
        # Da bitsize < intDsize*uintWC_max, ist
        # digitcount <= ceiling((bitsize+1)/intDsize) <= uintWC_max .
        var object big = allocate_bignum(digitcount,0); # neues Bignum >0
        TheBignum(big)->data[0] = 0; # höchstes Digit auf 0 setzen
        # restliche Digits von rechts füllen, dabei Folge von Bytes in
        # Folge von uintD übersetzen:
        bitbuffer = popSTACK();
        bitbufferptr = &TheSbvector(bitbuffer)->data[0];
        #if BIG_ENDIAN_P
        {
          var uintB* bigptr = (uintB*)(&TheBignum(big)->data[digitcount]);
          dotimespL(count,count, { *--bigptr = *bitbufferptr++; } );
        }
        #else
        {
          var uintD* bigptr = &TheBignum(big)->data[digitcount];
          var uintL count2;
          #define GET_NEXT_BYTE(i)  \
            digit |= ((uintD)(*bitbufferptr++) << (8*i));
          dotimespL(count2,floor(count,intDsize/8), {
            var uintD digit = 0;
            DOCONSTTIMES(intDsize/8,GET_NEXT_BYTE); # GET_NEXT_BYTE(0..intDsize/8-1)
            *--bigptr = digit;
          });
          #undef GET_NEXT_BYTE
          count2 = count % (intDsize/8);
          if (count2>0) {
            var uintL shiftcount = 0;
            var uintD digit = (uintD)(*bitbufferptr++);
            dotimesL(count2,count2-1, {
              shiftcount += 8;
              digit |= ((uintD)(*bitbufferptr++) << shiftcount);
            });
            *--bigptr = digit;
          }
        }
        #endif
        # Wegen (intDsize/8)*(digitcount-1) <= count <= (intDsize/8)*digitcount
        # ist alles gefüllt.
        return big;
      }
    }

# UP für READ-BYTE auf File-Streams für Integers, Art s :
# Liefert die im Bitbuffer enthaltenen bytesize Bytes als Integer.
# can trigger GC
  local object rd_by_is_I (object stream, uintL bitsize, uintL bytesize);
  local object rd_by_is_I(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    {
      var object bitbuffer = TheStream(stream)->strm_bitbuffer;
      # Zahl im bitbuffer normalisieren:
      var uintB* bitbufferptr = &TheSbvector(bitbuffer)->data[bytesize-1];
      var sintD sign;
      var uintL signbitnr = (bitsize-1)%8;
      var uintL count = bytesize;
      if (!(*bitbufferptr & bit(signbitnr))) {
        sign = 0;
        *bitbufferptr &= (bitm(signbitnr+1)-1); # High byte sign-extenden
        # normalisieren, höchstes Bit muss 0 bleiben:
        while ((count>=2) && (*bitbufferptr==0) && !(*(bitbufferptr-1) & bit(7))) {
          count--; bitbufferptr--;
        }
        # Zahl bilden:
        if # höchstens oint_data_len+1 Bits, Zahl <2^oint_data_len ?
           ((count <= floor(oint_data_len,8))
            || ((count == floor(oint_data_len,8)+1)
                && (*bitbufferptr < bit(oint_data_len%8))
           )   ) {
          # ja -> Fixnum >=0 bilden:
          var uintL wert = 0;
          until (count==0) { wert = (wert<<8) | *bitbufferptr--; count--; }
          return posfixnum(wert);
        }
      } else {
        sign = -1;
        *bitbufferptr |= minus_bitm(signbitnr+1); # High byte sign-extenden
        # normalisieren, höchstes Bit muss 1 bleiben:
        while ((count>=2) && (*bitbufferptr==(uintB)(-1)) && (*(bitbufferptr-1) & bit(7))) {
          count--; bitbufferptr--;
        }
        # Zahl bilden:
        if # höchstens oint_data_len+1 Bits, Zahl >=-2^oint_data_len ?
           ((count <= floor(oint_data_len,8))
            || ((count == floor(oint_data_len,8)+1)
                && (*bitbufferptr >= (uintB)(-bit(oint_data_len%8)))
           )   ) {
          # ja -> Fixnum <0 bilden:
          var uintL wert = (uintL)(-1);
          until (count==0) { wert = (wert<<8) | *bitbufferptr--; count--; }
          return negfixnum(wbitm(intLsize)+(oint)wert);
        }
      }
      # Bignum bilden:
      pushSTACK(bitbuffer);
      var uintL digitcount = ceiling(count,(intDsize/8));
      # Da bitsize < intDsize*uintWC_max, ist
      # digitcount <= ceiling(bitsize/intDsize) <= uintWC_max .
      var object big = allocate_bignum(digitcount,sign); # neues Bignum
      TheBignum(big)->data[0] = sign; # höchstes Word auf sign setzen
      # restliche Digits von rechts füllen, dabei Folge von Bytes in
      # Folge von uintD übersetzen:
      bitbuffer = popSTACK();
      bitbufferptr = &TheSbvector(bitbuffer)->data[0];
      #if BIG_ENDIAN_P
      {
        var uintB* bigptr = (uintB*)(&TheBignum(big)->data[digitcount]);
        dotimespL(count,count, { *--bigptr = *bitbufferptr++; } );
      }
      #else
      {
        var uintD* bigptr = &TheBignum(big)->data[digitcount];
        var uintL count2;
        #define GET_NEXT_BYTE(i)  \
          digit |= ((uintD)(*bitbufferptr++) << (8*i));
        dotimespL(count2,floor(count,intDsize/8), {
          var uintD digit = 0;
          DOCONSTTIMES(intDsize/8,GET_NEXT_BYTE); # GET_NEXT_BYTE(0..intDsize/8-1)
          *--bigptr = digit;
        });
        #undef GET_NEXT_BYTE
        count2 = count % (intDsize/8);
        if (count2>0) {
          var uintL shiftcount = 0;
          var uintD digit = (uintD)(*bitbufferptr++);
          dotimesL(count2,count2-1, {
            shiftcount += 8;
            digit |= ((uintD)(*bitbufferptr++) << shiftcount);
          });
          *--bigptr = digit;
        }
      }
      #endif
      # Wegen (intDsize/8)*(digitcount-1) < count <= (intDsize/8)*digitcount
      # ist alles gefüllt.
      return big;
    }

# Typ rd_by_ix_I: eines dieser beiden Unterprogramme:
  typedef object rd_by_ix_I (object stream, uintL bitsize, uintL bytesize);

# Subroutines for the Output side
# -------------------------------

# Function type of a subroutine which writes the bitbuffer contents to the
# stream.
  typedef void wr_by_aux_ix (object stream, uintL bitsize, uintL bytesize);

# UP für WRITE-BYTE auf File-Streams für Integers, Art u :
# Legt das Objekt (ein Integer >=0) als bytesize Bytes im Bitbuffer ab.
# > stream : File-Stream für Integers, Art u
# > obj : auszugebendes Objekt
# > finisher : Beendigungsroutine
  local void wr_by_ixu_sub (object stream, object obj, wr_by_aux_ix* finisher);
  local void wr_by_ixu_sub(stream,obj,finisher)
    var object stream;
    var object obj;
    var wr_by_aux_ix* finisher;
    {
      # obj überprüfen:
      if (!integerp(obj))
        fehler_wr_integer(stream,obj);
      if (!positivep(obj))
        fehler_bad_integer(stream,obj);
      # obj ist jetzt ein Integer >=0
      var uintL bitsize = ChannelStream_bitsize(stream);
      var uintL bytesize = ceiling(bitsize,8);
      # obj in den Bitbuffer übertragen:
      {
        var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_bitbuffer)->data[0];
        var uintL count = bytesize;
        if (posfixnump(obj)) {
          # obj ist ein Fixnum >=0
          var uintL wert = posfixnum_to_L(obj);
          # wert < 2^bitsize überprüfen:
          if (!((bitsize>=oint_data_len) || (wert < bit(bitsize))))
            fehler_bad_integer(stream,obj);
          # wert im Bitbuffer ablegen:
          until (wert==0) {
            *bitbufferptr++ = (uint8)wert; wert = wert>>8; count--;
          }
        } else {
          # obj ist ein Bignum >0
          var uintL len = (uintL)Bignum_length(obj);
          # obj < 2^bitsize überprüfen:
          if (!((floor(bitsize,intDsize) >= len)
                || ((floor(bitsize,intDsize) == len-1)
                    && (TheBignum(obj)->data[0] < bit(bitsize%intDsize))
             ) )   )
            fehler_bad_integer(stream,obj);
          #if BIG_ENDIAN_P
          {
            var uintB* ptr = (uintB*)&TheBignum(obj)->data[len];
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
            dotimespL(len,len, { *bitbufferptr++ = *--ptr; } );
          }
          #else
          {
            var uintD* ptr = &TheBignum(obj)->data[len];
            len--;
            count -= (intDsize/8)*len;
            dotimesL(len,len, {
              var uintD digit = *--ptr;
              doconsttimes(intDsize/8, {
                *bitbufferptr++ = (uintB)digit; digit = digit >> 8;
              });
            });
            var uintD digit = *--ptr;
            doconsttimes(intDsize/8, {
              if (digit==0) goto ok;
              *bitbufferptr++ = (uintB)digit; digit = digit >> 8;
              count--;
            });
           ok: ;
          }
          #endif
        }
        dotimesL(count,count, { *bitbufferptr++ = 0; } );
      }
      (*finisher)(stream,bitsize,bytesize);
    }

# UP für WRITE-BYTE auf File-Streams für Integers, Art s :
# Legt das Objekt (ein Integer) als bytesize Bytes im Bitbuffer ab.
# > stream : File-Stream für Integers, Art s
# > obj : auszugebendes Objekt
# > finisher : Beendigungsroutine
  local void wr_by_ixs_sub (object stream, object obj, wr_by_aux_ix* finisher);
  local void wr_by_ixs_sub(stream,obj,finisher)
    var object stream;
    var object obj;
    var wr_by_aux_ix* finisher;
    {
      # obj überprüfen:
      if (!integerp(obj))
        fehler_wr_integer(stream,obj);
      # obj ist jetzt ein Integer
      var uintL bitsize = ChannelStream_bitsize(stream);
      var uintL bytesize = ceiling(bitsize,8);
      # obj in den Bitbuffer übertragen:
      {
        var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_bitbuffer)->data[0];
        var uintL count = bytesize;
        var uintL sign = (sintL)R_sign(obj);
        if (fixnump(obj)) {
          # obj ist ein Fixnum
          var uintL wert = fixnum_to_L(obj); # >=0 oder <0, je nach sign
          # 0 <= wert < 2^(bitsize-1) bzw. -2^(bitsize-1) <= wert < 0 überprüfen:
          wert = wert^sign;
          if (!((bitsize>oint_data_len) || (wert < bit(bitsize-1))))
            fehler_bad_integer(stream,obj);
          # wert^sign im Bitbuffer ablegen:
          until (wert == 0) {
            *bitbufferptr++ = (uint8)(wert^sign); wert = wert>>8; count--;
          }
          dotimesL(count,count, { *bitbufferptr++ = (uint8)sign; } );
        } else {
          # obj ist ein Bignum
          var uintL len = (uintL)Bignum_length(obj);
          # -2^(bitsize-1) <= obj < 2^(bitsize-1) überprüfen:
          if (!((floor(bitsize,intDsize) >= len)
                || ((bitsize > intDsize*(len-1))
                    && ((TheBignum(obj)->data[0] ^ (uintD)sign) < bit((bitsize%intDsize)-1))
             ) )   )
            fehler_bad_integer(stream,obj);
          #if BIG_ENDIAN_P
          {
            var uintB* ptr = (uintB*)&TheBignum(obj)->data[len];
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
            dotimespL(len,len, { *bitbufferptr++ = *--ptr; } );
          }
          #else
          {
            var uintD* ptr = &TheBignum(obj)->data[len];
            len--;
            count -= (intDsize/8)*len;
            dotimesL(len,len, {
              var uintD digit = *--ptr;
              doconsttimes(intDsize/8, {
                *bitbufferptr++ = (uintB)digit; digit = digit >> 8;
              });
            });
            var sintD digit = *--ptr;
            doconsttimes(intDsize/8, {
              if (digit == (sintD)sign) goto ok;
              *bitbufferptr++ = (uintB)digit; digit = digit >> 8;
              count--;
            });
           ok: ;
          }
          #endif
          dotimesL(count,count, { *bitbufferptr++ = (uintB)sign; } );
        }
      }
      (*finisher)(stream,bitsize,bytesize);
    }

# Handle-Streams, Input side
# ==========================

# Low-level
# ---------

  # Push a byte into bytebuf.
  # UnbufferedStreamLow_push_byte(stream,b);
  # Assumes 0 <= UnbufferedStream_status(stream) < max_bytes_per_chart.
  #if (max_bytes_per_chart > 1) # i.e. defined(UNICODE)
    #define UnbufferedStreamLow_push_byte(stream,b)  \
      ASSERT((uintL)UnbufferedStream_status(stream) < max_bytes_per_chart);    \
      UnbufferedStream_bytebuf(stream)[UnbufferedStream_status(stream)++] = b;
  #else
    #define UnbufferedStreamLow_push_byte(stream,b)  \
      UnbufferedStream_bytebuf(stream)[0] = b; \
      UnbufferedStream_status(stream) = 1;
  #endif

  # Push a byte to the front of bytebuf.
  # UnbufferedStreamLow_pushfront_byte(stream,b);
  # Assumes 0 <= UnbufferedStream_status(stream) < max_bytes_per_chart.
  #if (max_bytes_per_chart > 1) # i.e. defined(UNICODE)
    #define UnbufferedStreamLow_pushfront_byte(stream,b)  \
      ASSERT((uintL)UnbufferedStream_status(stream) < max_bytes_per_chart); \
      { var uintL _count = UnbufferedStream_status(stream)++;               \
        var uintB* _ptr = &UnbufferedStream_bytebuf(stream)[_count];        \
        if (_count > 0)                                                     \
          { do { _ptr[0] = _ptr[-1]; _ptr--; } while (--_count > 0); }      \
        _ptr[0] = b;                                                        \
      }
  #else
    #define UnbufferedStreamLow_pushfront_byte(stream,b)  \
      UnbufferedStream_bytebuf(stream)[0] = b; \
      UnbufferedStream_status(stream) = 1;
  #endif

  #ifdef UNICODE
  # Push a number of bytes to the front of bytebuf.
  # UnbufferedStreamLow_pushfront_bytes(stream,byteptr,bytecount);
    #define UnbufferedStreamLow_pushfront_bytes(stream,byteptr,bytecount)  \
      { var uintL _push = (bytecount);                                    \
        if (_push > 0)                                                    \
          { var uintL _count = UnbufferedStream_status(stream);           \
            ASSERT(_push + _count <= max_bytes_per_chart);                \
            UnbufferedStream_status(stream) = _push + _count;             \
           {var const uintB* _ptr1 = (byteptr);                           \
            var uintB* _ptr2 = &UnbufferedStream_bytebuf(stream)[_count]; \
            if (_count > 0)                                               \
              { do { _ptr2--; _ptr2[_push] = _ptr2[0]; } while (--_count > 0); } \
            do { *_ptr2++ = *_ptr1++; } while (--_push > 0);              \
          }}                                                              \
      }
  #endif

  # Pop a byte from bytebuf.
  # UnbufferedStreamLow_pop_byte(stream,b);
  # declares and assigns a value to b.
  # Assumes UnbufferedStream_status(stream) > 0.
  #if (max_bytes_per_chart > 1) # i.e. defined(UNICODE)
    #define UnbufferedStreamLow_pop_byte(stream,b)  \
      var uintB b = UnbufferedStream_bytebuf(stream)[0];            \
      { var uintL _count = --UnbufferedStream_status(stream);       \
        if (_count > 0)                                             \
          { var uintB* _ptr = &UnbufferedStream_bytebuf(stream)[0]; \
            do { _ptr[0] = _ptr[1]; _ptr++; } while (--_count > 0); \
      }   }
  #else
    #define UnbufferedStreamLow_pop_byte(stream,b)  \
      var uintB b;                             \
      UnbufferedStream_status(stream) = 0;     \
      b = UnbufferedStream_bytebuf(stream)[0];
  #endif

  local sintL low_read_unbuffered_handle (object stream);
  local sintL low_read_unbuffered_handle(stream)
    var object stream;
    {
      if (UnbufferedStream_status(stream) < 0) { # already EOF?
        return -1;
      }
      if (UnbufferedStream_status(stream) > 0) { # bytebuf contains valid bytes?
        UnbufferedStreamLow_pop_byte(stream,b); return b;
      }
      var Handle handle = TheHandle(TheStream(stream)->strm_ichannel);
      var uintB b;
     restart_it:
      #if defined(AMIGAOS)
      interruptp({ fehler_interrupt(); });
      #endif
      run_time_stop(); # Run-Time-Stoppuhr anhalten
      begin_system_call();
      var int result = read(handle,&b,1); # Zeichen lesen versuchen
      end_system_call();
      run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
      if (result<0) {
        #if !(defined(AMIGAOS) || defined(WIN32_NATIVE))
        begin_system_call();
        if (errno==EINTR) { # Unterbrechung (evtl. durch Ctrl-C) ?
          end_system_call();
          interruptp({ fehler_interrupt(); });
          goto restart_it;
        }
        #endif
        #ifdef WIN32_NATIVE
        begin_system_call();
        if (GetLastError()==ERROR_SIGINT) { # Unterbrechung durch Ctrl-C ?
          end_system_call();
          fehler_interrupt();
        }
        #endif
        OS_error();
      }
      if (result==0) {
        # no byte available -> must be EOF
        UnbufferedStream_status(stream) = -1; return -1;
      } else {
        #if defined(AMIGAOS)
        # Ctrl-C wird meist während des Read()-Aufrufs festgestellt, und
        # Read() liefert dann "unschuldig" ein Zeichen ab. Wir behandeln
        # das Ctrl-C jetzt. Damit das Byte nicht verlorengeht, wird
        # es in bytebuf zurückgelegt.
        interruptp({
          UnbufferedStreamLow_push_byte(stream,b);
          fehler_interrupt();
        });
        #endif
        return b;
      }
    }

  local signean low_listen_unbuffered_handle (object stream);
  local signean low_listen_unbuffered_handle(stream)
    var object stream;
    {
      if (UnbufferedStream_status(stream) < 0) # already EOF?
        return ls_eof;
      if (UnbufferedStream_status(stream) > 0) # bytebuf contains valid bytes?
        return ls_avail;
      # Method 1: select, see SELECT(2)
      # Method 2: ioctl FIONREAD, see FILIO(4)
      # Method 3: switch temporarily to non-blocking I/O and try read(),
      #           see READ(2V), FILIO(4), or
      #           see READ(2V), FCNTL(2V), FCNTL(5)
      var Handle handle = TheHandle(TheStream(stream)->strm_ichannel);
      #if defined(MSDOS) && !defined(EMUNIX_PORTABEL)
      {
        var uintB status;
        begin_system_call();
        get_handle_input_status(handle,status);
        end_system_call();
        if (status)
          return ls_avail;
      }
      if (!nullp(TheStream(stream)->strm_isatty)) {
        # Terminal
        return ls_wait;
      } else {
        # File
        UnbufferedStream_status(stream) = -1; return ls_eof;
      }
      #elif defined(EMUNIX_PORTABEL)
      {
        var struct termio oldtermio;
        var struct termio newtermio;
        begin_system_call();
        if (!( ioctl(handle,TCGETA,&oldtermio) ==0)) {
          if (!((errno==ENOTTY)||(errno==EINVAL))) {
            OS_error();
          }
        }
        newtermio = oldtermio;
        newtermio.c_lflag &= ~IDEFAULT & ~ICANON;
        if (!( ioctl(handle,TCSETA,&newtermio) ==0)) {
          if (!((errno==ENOTTY)||(errno==EINVAL))) {
            OS_error();
          }
        }
        var unsigned long bytes_ready = 0;
        var int result = ioctl(handle,FIONREAD,&bytes_ready); # abfragen
        # (Starting with emx 0.8f this could also be done using select().)
        if (!( ioctl(handle,TCSETA,&oldtermio) ==0)) {
          if (!((errno==ENOTTY)||(errno==EINVAL))) {
            OS_error();
          }
        }
        end_system_call();
        if (result == 0) {
          # Enquiry succeeded.
          if (bytes_ready > 0)
            return ls_avail;
        }
        begin_system_call();
        if (!isatty(handle)) {
          result = eof(handle);
          if (result<0) {
            if (!(errno==ESPIPE)) { OS_error(); } # "Illegal seek error" is OK
          } else {
            end_system_call();
            if (result>0) # EOF reached?
              return ls_eof;
            else
              return ls_avail;
          }
        }
        end_system_call();
        return ls_wait;
      }
      #elif !(defined(AMIGAOS) || defined(WIN32_NATIVE))
      #ifdef HAVE_SELECT
      {
        # Use select() with readfds = singleton set {handle}
        # and timeout = zero interval.
        var fd_set handle_menge; # set of handles := {handle}
        var struct timeval zero_time; # time interval := 0
        begin_system_call();
        FD_ZERO(&handle_menge); FD_SET(handle,&handle_menge);
       restart_select:
        zero_time.tv_sec = 0; zero_time.tv_usec = 0;
        var int result = select(FD_SETSIZE,&handle_menge,NULL,NULL,&zero_time);
        if (result<0) {
          if (errno==EINTR)
            goto restart_select;
          if (!(errno==EBADF)) { OS_error(); } # UNIX_LINUX liefert bei Files EBADF !
          end_system_call();
        } else {
          end_system_call();
          # result = number of handles in handle_menge for which read() would
          # return without blocking.
          if (result==0)
            return ls_wait;
          # result=1
          # When read() returns a result without blocking, this can also be
          # EOF! (Example: Linux and pipes.) We therefore refrain from simply
          # doing  { return ls_avail; }  and instead try methods 2 and 3.
        }
      }
      #endif
      begin_system_call();
      #ifdef HAVE_FIONREAD
      # Try to enquire the number of available bytes:
      {
        var unsigned long bytes_ready;
        if ( ioctl(handle,FIONREAD,&bytes_ready) <0) {
          # Enquiry failed, probably wasn't a file
          if (!((errno == ENOTTY)
                || (errno == EINVAL)
                #ifdef ENOSYS # for UNIX_IRIX
                || (errno == ENOSYS)
                #endif
             ) ) {
            OS_error();
          }
        } else {
          # Enquiry succeeded, so it was a file
          end_system_call();
          if (bytes_ready > 0)
            return ls_avail;
          #ifdef HAVE_RELIABLE_FIONREAD
          # else we have reached the file's EOF:
          UnbufferedStream_status(stream) = -1; return ls_eof;
          #endif
        }
      }
      #endif
      #ifndef HAVE_SELECT
      if (!nullp(TheStream(stream)->strm_isatty)) {
        # Terminal
        # switch to non-blocking mode, then try read():
        var uintB b;
        var int result;
       restart_read_tty:
        #ifdef FIONBIO # non-blocking I/O à la BSD 4.2
        {
          var int non_blocking_io;
          non_blocking_io = 1;
          if (!( ioctl(handle,FIONBIO,&non_blocking_io) ==0)) {
            OS_error();
          }
          result = read(handle,&b,1);
          non_blocking_io = 0;
          if (!( ioctl(handle,FIONBIO,&non_blocking_io) ==0)) {
            OS_error();
          }
        }
        #else # non-blocking I/O à la SYSV
        {
          var int fcntl_flags;
          if (( fcntl_flags = fcntl(handle,F_GETFL,0) )<0) {
            OS_error();
          }
          if ( fcntl(handle,F_SETFL,fcntl_flags|O_NDELAY) <0) {
            OS_error();
          }
          result = read(handle,&b,1);
          if ( fcntl(handle,F_SETFL,fcntl_flags) <0) {
            OS_error();
          }
        }
        #endif
        if (result < 0) {
          if (errno==EINTR)
            goto restart_read_tty;
          #ifdef FIONBIO
          if (errno==EWOULDBLOCK) # BSD 4.2 Error-Code
          #else
          if ((errno==EAGAIN) # Posix Error-Code
              #ifdef EWOULDBLOCK
              || (errno==EWOULDBLOCK)
              #endif
             )
          #endif
            return ls_wait;
          OS_error();
        }
        end_system_call();
        if (result==0) {
          return ls_wait;
        } else {
          # Stuff the read byte into the buffer, for next low_read call.
          UnbufferedStreamLow_push_byte(stream,b);
          return ls_avail;
        }
        # If this doesn't work, should be use a timer 0.1 sec ??
      } else
      #endif
        # file (or pipe)
        {
          # try to read a byte:
         restart_read_other:
          var uintB b;
          var int result = read(handle,&b,1);
          if (result<0) {
            if (errno==EINTR)
              goto restart_read_other;
            OS_error();
          }
          end_system_call();
          if (result==0) {
            UnbufferedStream_status(stream) = -1; return ls_eof;
          } else {
            # Stuff the read byte into the buffer, for next low_read call.
            UnbufferedStreamLow_push_byte(stream,b);
            return ls_avail;
          }
        }
      #elif defined(AMIGAOS)
      begin_system_call();
      if (!nullp(TheStream(stream)->strm_isatty)) {
        # interactive
        if (WaitForChar(handle,0)) { # wait 0 usec for a byte
          end_system_call(); return ls_avail;
        } else {
          end_system_call(); return ls_wait;
        }
      } else {
        # not interactive
        # try to read a byte:
        var uintB b;
        var long result = Read(handle,&b,1);
        end_system_call();
        if (result<0) {
          OS_error();
        }
        if (result==0) {
          UnbufferedStream_status(stream) = -1; return ls_eof;
        } else {
          # Stuff the read byte into the buffer, for next low_read call.
          UnbufferedStreamLow_push_byte(stream,b);
          return ls_avail;
        }
      }
      #elif defined(WIN32_NATIVE)
      # This is pretty complex. To test this, create a file "listen.lsp"
      # containing the code
      #   (tagbody 1 (prin1 (listen *terminal-io*)) (sys::%sleep 0 500) (go 1))
      # and execute "lisp.exe -q -i listen.lsp" with redirected standard input.
      begin_system_call();
      switch (GetFileType(handle)) {
        case FILE_TYPE_CHAR:
          {
            var DWORD nevents;
            if (GetNumberOfConsoleInputEvents(handle,&nevents)) {
              # It's a console.
              if (nevents==0) {
                end_system_call(); return ls_wait;
              }
              var INPUT_RECORD* events = (INPUT_RECORD*)alloca(nevents*sizeof(INPUT_RECORD));
              var DWORD nevents_read;
              var DWORD mode;
              if (!PeekConsoleInput(handle,events,nevents,&nevents_read)) {
                OS_error();
              }
              if (nevents_read==0) {
                end_system_call(); return ls_wait;
              }
              if (!GetConsoleMode(handle,&mode)) {
                OS_error();
              }
              if (mode & ENABLE_LINE_INPUT) {
                # Look out for a Key-Down event corresponding to CR/LF.
                var DWORD i;
                for (i = 0; i < nevents_read; i++) {
                  if (events[i].EventType == KEY_EVENT
                      && events[i].Event.KeyEvent.bKeyDown
                      && events[i].Event.KeyEvent.uAsciiChar == CR)
                    # probably a byte available (except if it is Ctrl-Z)
                    goto peek_one;
                }
              } else {
                # Look out for any Key-Down event.
                var DWORD i;
                for (i = 0; i < nevents_read; i++) {
                  if (events[i].EventType == KEY_EVENT
                      && events[i].Event.KeyEvent.bKeyDown
                      && events[i].Event.KeyEvent.uAsciiChar != 0)
                    # probably a byte available (except if it is Ctrl-Z)
                    goto peek_one;
                }
              }
              end_system_call(); return ls_wait;
            } elif (!(GetLastError()==ERROR_INVALID_HANDLE)) {
              OS_error();
            }
          }
          # Not a console.
          switch (WaitForSingleObject(handle,0)) {
            case WAIT_OBJECT_0:
              # a byte is available, or EOF
              break;
            case WAIT_TIMEOUT:
              end_system_call(); return ls_wait;
            default:
              OS_error();
          }
          /*FALLTHROUGH*/
        case FILE_TYPE_DISK:
        default:
          # It's a file (or something unknown).
         peek_one:
          # try to read a byte
          {
            var uintB b;
            var int result = read(handle,&b,1);
            if (result<0) {
              OS_error();
            }
            end_system_call();
            if (result==0) {
              UnbufferedStream_status(stream) = -1; return ls_eof;
            } else {
              # Stuff the read byte into the buffer, for next low_read call.
              UnbufferedStreamLow_push_byte(stream,b);
              return ls_avail;
            }
          }
        case FILE_TYPE_PIPE:
          {
            var DWORD nbytes;
            if (PeekNamedPipe(handle,NULL,0,NULL,&nbytes,NULL)) {
              # It's a pipe (input).
              end_system_call();
              if (nbytes > 0)
                return ls_avail;
              else
                return ls_wait;
            } elif (GetLastError()==ERROR_BROKEN_PIPE) {
              # EOF reached
              end_system_call();
              UnbufferedStream_status(stream) = -1;
              return ls_eof;
            } elif (GetLastError()==ERROR_ACCESS_DENIED) {
              # It's a pipe (output). Let's fake EOF.
              end_system_call();
              UnbufferedStream_status(stream) = -1;
              return ls_eof;
            } else {
              # What about sockets??
              OS_error();
            }
          }
      }
      #endif
    }

  local boolean low_clear_input_unbuffered_handle (object stream);
  local boolean low_clear_input_unbuffered_handle(stream)
    var object stream;
    {
      if (nullp(TheStream(stream)->strm_isatty))
        return FALSE; # it's a file -> nothing to do
      UnbufferedStream_status(stream) = 0; # forget about past EOF
      # Terminal (interactive on AMIGAOS)
      clear_tty_input(TheHandle(TheStream(stream)->strm_ichannel));
      # In case this didn't work, and as a general method for platforms on
      # which clear_tty_input() does nothing: read a byte, as long as listen
      # says that a byte is available.
      while (ls_avail_p(low_listen_unbuffered_handle(stream))) {
        #ifdef WIN32_NATIVE
        # Our low_listen_unbuffered_handle function, when applied to a WinNT
        # console, cannot tell when there is an LF pending after the
        # preceding CR has been eaten. Therefore be careful to set
        # UnbufferedStream_ignore_next_LF to TRUE when we read a LF.
        var uintL c = low_read_unbuffered_handle(stream);
        if (c >= 0)
          UnbufferedStream_ignore_next_LF(stream) = (c == CR);
        #else
        low_read_unbuffered_handle(stream);
        #endif
      }
      return TRUE;
    }

  local uintB* low_read_array_unbuffered_handle (object stream, uintB* byteptr, uintL len);
  local uintB* low_read_array_unbuffered_handle(stream,byteptr,len)
    var object stream;
    var uintB* byteptr;
    var uintL len;
    {
      if (UnbufferedStream_status(stream) < 0) # already EOF?
        return byteptr;
      while (UnbufferedStream_status(stream) > 0) { # bytebuf contains valid bytes?
        UnbufferedStreamLow_pop_byte(stream,b);
        *byteptr++ = b;
        len--;
        if (len == 0)
          return byteptr;
      }
      var Handle handle = TheHandle(TheStream(stream)->strm_ichannel);
      run_time_stop(); # Run-Time-Stoppuhr anhalten
      begin_system_call();
      var sintL result = full_read(handle,byteptr,len);
      end_system_call();
      run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
      if (result<0) {
        #if !(defined(AMIGAOS) || defined(WIN32_NATIVE))
        begin_system_call();
        if (errno==EINTR) # Unterbrechung (evtl. durch Ctrl-C) ?
          interruptp({ end_system_call(); fehler_interrupt(); });
        #endif
        #ifdef WIN32_NATIVE
        begin_system_call();
        if (GetLastError()==ERROR_SIGINT) { # Unterbrechung durch Ctrl-C ?
          end_system_call(); fehler_interrupt();
        }
        #endif
        OS_error(); # Error melden
      }
      byteptr += result;
      return byteptr;
    }

# Integer streams
# ---------------

# UP für READ-BYTE auf File-Streams für Integers, Art a :
# Füllt den Bitbuffer mit den nächsten bitsize Bits.
# > stream : File-Stream für Integers, Art a
# > finisher : Beendigungsroutine
# < ergebnis : gelesener Integer oder eof_value
  local object rd_by_aux_iax_unbuffered (object stream, rd_by_ix_I* finisher);
  local object rd_by_aux_iax_unbuffered(stream,finisher)
    var object stream;
    var rd_by_ix_I* finisher;
    {
      var uintL bitsize = ChannelStream_bitsize(stream);
      var uintL bytesize = bitsize/8;
      # genügend viele Bytes in den Bitbuffer übertragen:
      var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_bitbuffer)->data[0];
      if (!(UnbufferedStreamLow_read_array(stream)(stream,bitbufferptr,bytesize) == bitbufferptr+bytesize))
        goto eof;
      # in Zahl umwandeln:
      return (*finisher)(stream,bitsize,bytesize);
     eof: # EOF erreicht
      return eof_value;
    }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art au :
  local object rd_by_iau_unbuffered (object stream);
  local object rd_by_iau_unbuffered(stream)
    var object stream;
    {
      return rd_by_aux_iax_unbuffered(stream,&rd_by_iu_I);
    }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art as :
  local object rd_by_ias_unbuffered (object stream);
  local object rd_by_ias_unbuffered(stream)
    var object stream;
    {
      return rd_by_aux_iax_unbuffered(stream,&rd_by_is_I);
    }

# READ-BYTE - Pseudofunktion für Handle-Streams, Art au, bitsize = 8 :
  local object rd_by_iau8_unbuffered (object stream);
  local object rd_by_iau8_unbuffered(stream)
    var object stream;
    {
      var sintL b = UnbufferedStreamLow_read(stream)(stream);
      if (b < 0)
        return eof_value;
      return fixnum((uintB)b);
    }

# READ-BYTE-ARRAY - Pseudofunktion für Handle-Streams, Art au, bitsize = 8 :
  local uintL rd_by_array_iau8_unbuffered (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local uintL rd_by_array_iau8_unbuffered(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      var object stream = *stream_;
      var uintB* startptr = &TheSbvector(*bytearray_)->data[start];
      var uintB* endptr = UnbufferedStreamLow_read_array(stream)(stream,startptr,len);
      return endptr-startptr;
    }

# Character streams
# -----------------

# READ-CHAR - Pseudofunktion für Unbuffered-Channel-Streams:
  local object rd_ch_unbuffered (const object* stream_);
  local object rd_ch_unbuffered(stream_)
    var const object* stream_;
    {
      var object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF?
        return eof_value;
     retry:
      var chart c;
      #ifdef UNICODE
      var object encoding = TheStream(stream)->strm_encoding;
      var uintB buf[max_bytes_per_chart];
      var uintL buflen = 0;
      loop {
        var sintL b = UnbufferedStreamLow_read(stream)(stream);
        if (b < 0)
          return eof_value;
        ASSERT(buflen < max_bytes_per_chart);
        buf[buflen++] = (uintB)b;
        var const uintB* bptr = &buf[0];
        var chart* cptr = &c;
        Encoding_mbstowcs(encoding)(encoding,stream,&bptr,&buf[buflen],&cptr,cptr+1);
        if (cptr == &c) {
          # Not a complete character.
          # Shift the buffer
          if (!(bptr == &buf[0])) {
            var const uintB* ptr1 = bptr;
            var uintB* ptr2 = &buf[0];
            until (ptr1 == &buf[buflen]) { *ptr2++ = *ptr1++; }
            buflen = ptr2 - &buf[0];
          }
        } else {
          # Read a complete character.
          # Move the remainder of the buffer into bytebuf.
          UnbufferedStreamLow_pushfront_bytes(stream,bptr,&buf[buflen]-bptr);
          break;
        }
      }
      #else
      {
        var sintL b = UnbufferedStreamLow_read(stream)(stream);
        if (b < 0)
          return eof_value;
        c = as_chart((uintB)b);
      }
      #endif
      if (chareq(c,ascii(NL))) {
        if (UnbufferedStream_ignore_next_LF(stream)) {
          UnbufferedStream_ignore_next_LF(stream) = FALSE;
          goto retry;
        }
        # lineno incrementieren:
        ChannelStream_lineno(stream) += 1;
      } elif (chareq(c,ascii(CR))) {
        UnbufferedStream_ignore_next_LF(stream) = TRUE;
        c = ascii(NL);
        # lineno incrementieren:
        ChannelStream_lineno(stream) += 1;
      } else {
        UnbufferedStream_ignore_next_LF(stream) = FALSE;
      }
      return code_char(c);
    }

# Stellt fest, ob ein Unbuffered-Channel-Stream ein Zeichen verfügbar hat.
# listen_unbuffered(stream)
# > stream: Unbuffered-Channel-Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
  local signean listen_unbuffered (object stream);
  local signean listen_unbuffered(stream)
    var object stream;
    {
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        return ls_eof;
      var signean result;
      #ifdef UNICODE
      var chart c;
      var object encoding = TheStream(stream)->strm_encoding;
      var uintB buf[max_bytes_per_chart];
      var uintL buflen = 0;
      loop {
        result = UnbufferedStreamLow_listen(stream)(stream);
        if (ls_eof_p(result))
          break;
        if (!ls_avail_p(result)) {
          # Stop reading.
          # Move the buffer into bytebuf.
          UnbufferedStreamLow_pushfront_bytes(stream,&buf[0],buflen);
          break;
        }
        var sintL b = UnbufferedStreamLow_read(stream)(stream);
        if (b < 0) {
          result = ls_eof; break;
        }
        ASSERT(buflen < max_bytes_per_chart);
        buf[buflen++] = (uintB)b;
        var const uintB* bptr = &buf[0];
        var chart* cptr = &c;
        Encoding_mbstowcs(encoding)(encoding,stream,&bptr,&buf[buflen],&cptr,cptr+1);
        if (cptr == &c) {
          # Not a complete character.
          # Shift the buffer
          if (!(bptr == &buf[0])) {
            var const uintB* ptr1 = bptr;
            var uintB* ptr2 = &buf[0];
            until (ptr1 == &buf[buflen]) { *ptr2++ = *ptr1++; }
            buflen = ptr2 - &buf[0];
          }
        } else {
          # Read a complete character.
          if (UnbufferedStream_ignore_next_LF(stream) && chareq(c,ascii(NL))) {
            # Move the remainder of the buffer into bytebuf.
            UnbufferedStreamLow_pushfront_bytes(stream,bptr,&buf[buflen]-bptr);
            UnbufferedStream_ignore_next_LF(stream) = FALSE;
          } else {
            # Move the buffer into bytebuf.
            UnbufferedStreamLow_pushfront_bytes(stream,&buf[0],buflen);
            UnbufferedStream_ignore_next_LF(stream) = FALSE;
            result = ls_avail;
            break;
          }
        }
      }
      #else
     retry:
      result = UnbufferedStreamLow_listen(stream)(stream);
      if (ls_avail_p(result) && UnbufferedStream_ignore_next_LF(stream)) {
        var sintL b = UnbufferedStreamLow_read(stream)(stream);
        if (b < 0)
          return ls_eof;
        UnbufferedStream_ignore_next_LF(stream) = FALSE;
        if (b == NL)
          goto retry;
        UnbufferedStreamLow_pushfront_byte(stream,b);
      }
      #endif
      return result;
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem
# Unbuffered-Channel-Stream.
# clear_input_unbuffered(stream);
# > stream: Unbuffered-Channel-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_unbuffered (object stream);
  local boolean clear_input_unbuffered(stream)
    var object stream;
    {
      if (nullp(TheStream(stream)->strm_isatty))
        return FALSE; # it's a file -> nothing to do
      TheStream(stream)->strm_rd_ch_last = NIL; # forget about past EOF
      #ifdef WIN32_NATIVE
      # Our low_listen_unbuffered_handle function, when applied to a WinNT
      # console, cannot tell when there is an LF pending after the
      # preceding CR has been eaten. Therefore be careful not to reset
      # UnbufferedStream_ignore_next_LF.
      #else
      UnbufferedStream_ignore_next_LF(stream) = FALSE;
      #endif
      UnbufferedStreamLow_clear_input(stream)(stream);
      return TRUE;
    }

# READ-CHAR-ARRAY - Pseudofunktion für Unbuffered-Channel-Streams:
  local uintL rd_ch_array_unbuffered (const object* stream_, const object* chararray_, uintL start, uintL len);
  local uintL rd_ch_array_unbuffered(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      # Need a temporary buffer for CR/LF->NL translation.
      #define tmpbufsize 4096
      var chart tmpbuf[tmpbufsize];
      var object stream = *stream_;
      var chart* startptr = &TheSstring(*chararray_)->data[start];
      var chart* endptr = startptr+len;
      var chart* charptr = startptr;
      loop {
        var uintL remaining = endptr - charptr;
        if (remaining == 0)
          break;
        if (remaining > tmpbufsize)
          remaining = tmpbufsize;
        var uintL count;
        #ifdef UNICODE
        # In order to read n characters, we read n bytes. (Fewer than n bytes
        # will not suffice.) If these aren't enough bytes, the next round
        # will provide them.
        # FIXME: Could use TheEncoding(encoding)->min_bytes_per_char here.
        {
          var object encoding = TheStream(stream)->strm_encoding;
          var uintB tmptmpbuf[tmpbufsize];
          var uintB* tmptmpendptr =
            UnbufferedStreamLow_read_array(stream)(stream,tmptmpbuf,remaining);
          var const uintB* tmptmpptr = &tmptmpbuf[0];
          var chart* tmpptr = &tmpbuf[0];
          Encoding_mbstowcs(encoding)(encoding,stream,&tmptmpptr,tmptmpendptr,&tmpptr,&tmpbuf[tmpbufsize]);
          count = tmpptr - &tmpbuf[0];
          ASSERT(tmptmpendptr-tmptmpptr < max_bytes_per_chart);
          # Move the remainder of tmptmpbuf into bytebuf.
          UnbufferedStreamLow_pushfront_bytes(stream,tmptmpptr,tmptmpendptr-tmptmpptr);
        }
        if (count == 0) {
          # Filling the last few characters must be done one by one, in
          # order not to overrun the goal.
          pushSTACK(stream);
          do {
            var object ch = rd_ch_unbuffered(&STACK_0);
            if (eq(ch,eof_value))
              break;
            tmpbuf[count++] = char_code(ch);
            remaining--;
          } while (remaining > 0);
          skipSTACK(1);
        }
        #else
        count = UnbufferedStreamLow_read_array(stream)(stream,tmpbuf,remaining) - &tmpbuf[0];
        #endif
        if (count == 0)
          break;
        var const chart* tmpptr = &tmpbuf[0];
        do {
          var chart c = *tmpptr++;
          count--;
          if (chareq(c,ascii(NL))) {
            if (UnbufferedStream_ignore_next_LF(stream)) {
              UnbufferedStream_ignore_next_LF(stream) = FALSE;
            } else {
              ChannelStream_lineno(stream) += 1; *charptr++ = ascii(NL);
            }
          } elif (chareq(c,ascii(CR))) {
            if (count > 0) {
              if (chareq(*tmpptr,ascii(NL))) {
                tmpptr++; count--;
              }
              UnbufferedStream_ignore_next_LF(stream) = FALSE;
            } else {
              UnbufferedStream_ignore_next_LF(stream) = TRUE;
            }
            ChannelStream_lineno(stream) += 1; *charptr++ = ascii(NL);
          } else {
            UnbufferedStream_ignore_next_LF(stream) = FALSE;
            *charptr++ = c;
          }
        } while (count > 0);
      }
      return charptr - startptr;
      #undef tmpbufsize
    }

# Initializes the input side fields of an unbuffered stream.
# UnbufferedHandleStream_input_init(stream);
  #define UnbufferedHandleStream_input_init(stream)  \
    { UnbufferedStreamLow_read(stream) = &low_read_unbuffered_handle;               \
      UnbufferedStreamLow_listen(stream) = &low_listen_unbuffered_handle;           \
      UnbufferedStreamLow_clear_input(stream) = &low_clear_input_unbuffered_handle; \
      UnbufferedStreamLow_read_array(stream) = &low_read_array_unbuffered_handle;   \
      UnbufferedHandleStream_input_init_data(stream);                               \
    }
  #define UnbufferedHandleStream_input_init_data(stream)  \
    UnbufferedStream_status(stream) = 0;             \
    UnbufferedHandleStream_input_init_amiga(stream);
  #ifdef AMIGAOS
    #define UnbufferedHandleStream_input_init_amiga(stream)  UnbufferedStream_rawp(stream) = 0;
  #else
    #define UnbufferedHandleStream_input_init_amiga(stream)
  #endif

# Schließt einen Channel-Stream.
# close_ichannel(stream);
# > stream : Channel-Stream
  local void close_ichannel (object stream);
  local void close_ichannel(stream)
    var object stream;
    {
      ChannelStreamLow_close(stream)(stream,TheStream(stream)->strm_ichannel);
      ChannelStream_fini(stream);
      if (ChannelStream_bitsize(stream) > 0) {
        ChannelStream_bitsize(stream) = 0; # bitsize löschen
        TheStream(stream)->strm_bitbuffer = NIL; # Bitbuffer freimachen
      }
    }

# Handle-Streams, Output side
# ===========================

# Low-level
# ---------

  local void low_write_unbuffered_handle (object stream, uintB b);
  local void low_write_unbuffered_handle(stream,b)
    var object stream;
    var uintB b;
    {
      var Handle handle = TheHandle(TheStream(stream)->strm_ochannel);
     restart_it:
      begin_system_call();
      # Try to output the byte.
      #if !defined(AMIGAOS)
      var int result = write(handle,&b,1);
      if (result<0) {
        #if !defined(WIN32_NATIVE)
        if (errno==EINTR) { # Unterbrechung (evtl. durch Ctrl-C) ?
          end_system_call();
          interruptp({ fehler_interrupt(); });
          goto restart_it;
        }
        #endif
        OS_error(); # Error melden
      }
      end_system_call();
      #else # defined(AMIGAOS)
      var long result = Write(handle,&b,1);
      end_system_call();
      if (result<0) {
        OS_error(); # Error melden
      }
      interruptp({ fehler_interrupt(); }); # Ctrl-C -> Break-Schleife aufrufen
      #endif
      if (result==0) # not successful?
        fehler_unwritable(TheSubr(subr_self)->name,stream);
    }

  local const uintB* low_write_array_unbuffered_handle (object stream, const uintB* byteptr, uintL len);
  local const uintB* low_write_array_unbuffered_handle(stream,byteptr,len)
    var object stream;
    var const uintB* byteptr;
    var uintL len;
    {
      var Handle handle = TheHandle(TheStream(stream)->strm_ochannel);
      begin_system_call();
      var sintL result = full_write(handle,byteptr,len);
      if (result<0) {
        OS_error(); # Error melden
      }
      end_system_call();
      if (!(result==(sintL)len)) # not successful?
        fehler_unwritable(TheSubr(subr_self)->name,stream);
      return byteptr+result;
    }

  local void low_finish_output_unbuffered_handle (object stream);
  local void low_finish_output_unbuffered_handle(stream)
    var object stream;
    {
      finish_tty_output(TheHandle(TheStream(stream)->strm_ochannel));
    }

  local void low_force_output_unbuffered_handle (object stream);
  local void low_force_output_unbuffered_handle(stream)
    var object stream;
    {
      force_tty_output(TheHandle(TheStream(stream)->strm_ochannel));
    }

  local void low_clear_output_unbuffered_handle (object stream);
  local void low_clear_output_unbuffered_handle(stream)
    var object stream;
    {
      clear_tty_output(TheHandle(TheStream(stream)->strm_ochannel));
    }

# Integer streams
# ---------------

# UP für WRITE-BYTE auf File-Streams für Integers, Art a :
# Schreibt den Bitbuffer-Inhalt aufs File.
  local void wr_by_aux_ia_unbuffered (object stream, uintL bitsize, uintL bytesize);
  local void wr_by_aux_ia_unbuffered(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    {
      var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_bitbuffer)->data[0];
      UnbufferedStreamLow_write_array(stream)(stream,bitbufferptr,bytesize);
    }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art au :
  local void wr_by_iau_unbuffered (object stream, object obj);
  local void wr_by_iau_unbuffered(stream,obj)
    var object stream;
    var object obj;
    {
      wr_by_ixu_sub(stream,obj,&wr_by_aux_ia_unbuffered);
    }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art as :
  local void wr_by_ias_unbuffered (object stream, object obj);
  local void wr_by_ias_unbuffered(stream,obj)
    var object stream;
    var object obj;
    {
      wr_by_ixs_sub(stream,obj,&wr_by_aux_ia_unbuffered);
    }

# WRITE-BYTE - Pseudofunktion für Handle-Streams, Art au, bitsize = 8 :
  local void wr_by_iau8_unbuffered (object stream, object obj);
  local void wr_by_iau8_unbuffered(stream,obj)
    var object stream;
    var object obj;
    {
      # obj überprüfen:
      if (!integerp(obj))
        fehler_wr_integer(stream,obj);
      if (!(posfixnump(obj) && (posfixnum_to_L(obj) < bit(8))))
        fehler_bad_integer(stream,obj);
      UnbufferedStreamLow_write(stream)(stream,posfixnum_to_L(obj));
    }

# WRITE-BYTE-ARRAY - Pseudofunktion für Handle-Streams, Art au, bitsize = 8 :
  local void wr_by_array_iau8_unbuffered (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local void wr_by_array_iau8_unbuffered(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      var object stream = *stream_;
      UnbufferedStreamLow_write_array(stream)(stream,&TheSbvector(*bytearray_)->data[start],len);
    }

# Character streams
# -----------------

# Three versions, one for each kind of line-terminator: :unix, :mac, :dos.

# WRITE-CHAR - Pseudofunktion für Unbuffered-Channel-Streams:
  local void wr_ch_unbuffered_unix (const object* stream_, object ch);
  local void wr_ch_unbuffered_unix(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      var object stream = *stream_;
      # ch sollte Character sein:
      if (!charp(ch))
        fehler_wr_char(stream,ch);
      var chart c = char_code(ch); # Code des Zeichens
      #ifdef UNICODE
      var uintB buf[max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      var const chart* cptr = &c;
      var uintB* bptr = &buf[0];
      Encoding_wcstombs(encoding)(encoding,stream,&cptr,cptr+1,&bptr,&buf[max_bytes_per_chart]);
      ASSERT(cptr == &c+1);
      UnbufferedStreamLow_write_array(stream)(stream,&buf[0],bptr-&buf[0]);
      #else
      UnbufferedStreamLow_write(stream)(stream,as_cint(c));
      #endif
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für Unbuffered-Channel-Streams:
  local void wr_ch_array_unbuffered_unix (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_unbuffered_unix(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var object stream = *stream_;
      var const chart* charptr;
      unpack_sstring_alloca(*chararray_,len,start, charptr=);
      #ifdef UNICODE
      #define tmpbufsize 4096
      var const chart* endptr = charptr + len;
      var uintB tmptmpbuf[tmpbufsize*max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      do {
        var uintB* bptr = &tmptmpbuf[0];
        Encoding_wcstombs(encoding)(encoding,stream,&charptr,endptr,&bptr,&tmptmpbuf[tmpbufsize*max_bytes_per_chart]);
        UnbufferedStreamLow_write_array(stream)(stream,&tmptmpbuf[0],bptr-&tmptmpbuf[0]);
      } until (charptr == endptr);
      #undef tmpbufsize
      #else
      var const chart* endptr = UnbufferedStreamLow_write_array(stream)(stream,charptr,len);
      #endif
      wr_ss_lpos(stream,endptr,len); # Line-Position aktualisieren
    }

# WRITE-CHAR - Pseudofunktion für Unbuffered-Channel-Streams:
  local void wr_ch_unbuffered_mac (const object* stream_, object ch);
  local void wr_ch_unbuffered_mac(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      var object stream = *stream_;
      # ch sollte Character sein:
      if (!charp(ch))
        fehler_wr_char(stream,ch);
      var chart c = char_code(ch); # Code des Zeichens
      if (chareq(c,ascii(NL)))
        c = ascii(CR);
      #ifdef UNICODE
      var uintB buf[max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      var const chart* cptr = &c;
      var uintB* bptr = &buf[0];
      Encoding_wcstombs(encoding)(encoding,stream,&cptr,cptr+1,&bptr,&buf[max_bytes_per_chart]);
      ASSERT(cptr == &c+1);
      UnbufferedStreamLow_write_array(stream)(stream,&buf[0],bptr-&buf[0]);
      #else
      UnbufferedStreamLow_write(stream)(stream,as_cint(c));
      #endif
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für Unbuffered-Channel-Streams:
  local void wr_ch_array_unbuffered_mac (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_unbuffered_mac(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var object stream = *stream_;
      var const chart* charptr;
      unpack_sstring_alloca(*chararray_,len,start, charptr=);
      # Need a temporary buffer for NL->CR translation.
      #define tmpbufsize 4096
      var chart tmpbuf[tmpbufsize];
      #ifdef UNICODE
      var uintB tmptmpbuf[tmpbufsize*max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      #endif
      var uintL remaining = len;
      do {
        var uintL n = remaining;
        if (n > tmpbufsize)
          n = tmpbufsize;
        {
          var chart* tmpptr = &tmpbuf[0];
          var uintL count;
          dotimespL(count,n, {
            var chart c = *charptr++;
            if (chareq(c,ascii(NL)))
              c = ascii(CR);
            *tmpptr++ = c;
          });
          #ifdef UNICODE
          var const chart* cptr = tmpbuf;
          var uintB* bptr = &tmptmpbuf[0];
          Encoding_wcstombs(encoding)(encoding,stream,&cptr,tmpptr,&bptr,&tmptmpbuf[tmpbufsize*max_bytes_per_chart]);
          ASSERT(cptr == tmpptr);
          UnbufferedStreamLow_write_array(stream)(stream,&tmptmpbuf[0],bptr-&tmptmpbuf[0]);
          #else
          UnbufferedStreamLow_write_array(stream)(stream,tmpbuf,n);
          #endif
        }
        remaining -= n;
      } while (remaining > 0);
      #undef tmpbufsize
      wr_ss_lpos(stream,charptr,len); # Line-Position aktualisieren
    }

# WRITE-CHAR - Pseudofunktion für Unbuffered-Channel-Streams:
  local void wr_ch_unbuffered_dos (const object* stream_, object ch);
  local void wr_ch_unbuffered_dos(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      var object stream = *stream_;
      # ch sollte Character sein:
      if (!charp(ch))
        fehler_wr_char(stream,ch);
      var chart c = char_code(ch); # Code des Zeichens
      static chart const crlf[2] = { ascii(CR), ascii(LF) };
      #ifdef UNICODE
      var uintB buf[2*max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      var const chart* cp;
      var uintL n;
      if (chareq(c,ascii(NL))) {
        cp = crlf; n = 2;
      } else {
        cp = &c; n = 1;
      }
      var const chart* cptr = cp;
      var uintB* bptr = &buf[0];
      Encoding_wcstombs(encoding)(encoding,stream,&cptr,cp+n,&bptr,&buf[2*max_bytes_per_chart]);
      ASSERT(cptr == cp+n);
      UnbufferedStreamLow_write_array(stream)(stream,&buf[0],bptr-&buf[0]);
      #else
      if (chareq(c,ascii(NL))) {
        UnbufferedStreamLow_write_array(stream)(stream,crlf,2);
      } else {
        UnbufferedStreamLow_write(stream)(stream,as_cint(c));
      }
      #endif
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für Unbuffered-Channel-Streams:
  local void wr_ch_array_unbuffered_dos (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_unbuffered_dos(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var object stream = *stream_;
      var const chart* charptr;
      unpack_sstring_alloca(*chararray_,len,start, charptr=);
      # Need a temporary buffer for NL->CR/LF translation.
      #define tmpbufsize 4096
      var chart tmpbuf[2*tmpbufsize];
      #ifdef UNICODE
      var uintB tmptmpbuf[2*tmpbufsize*max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      #endif
      var uintL remaining = len;
      do {
        var uintL n = remaining;
        if (n > tmpbufsize)
          n = tmpbufsize;
        {
          var chart* tmpptr = &tmpbuf[0];
          var uintL count;
          dotimespL(count,n, {
            var chart c = *charptr++;
            if (chareq(c,ascii(NL))) {
              *tmpptr++ = ascii(CR); *tmpptr++ = ascii(LF);
            } else {
              *tmpptr++ = c;
            }
          });
          #ifdef UNICODE
          var const chart* cptr = tmpbuf;
          var uintB* bptr = &tmptmpbuf[0];
          Encoding_wcstombs(encoding)(encoding,stream,&cptr,tmpptr,&bptr,&tmptmpbuf[2*tmpbufsize*max_bytes_per_chart]);
          ASSERT(cptr == tmpptr);
          UnbufferedStreamLow_write_array(stream)(stream,&tmptmpbuf[0],bptr-&tmptmpbuf[0]);
          #else
          UnbufferedStreamLow_write_array(stream)(stream,tmpbuf,tmpptr-&tmpbuf[0]);
          #endif
        }
        remaining -= n;
      } while (remaining > 0);
      #undef tmpbufsize
      wr_ss_lpos(stream,charptr,len); # Line-Position aktualisieren
    }

# Macro: Emits a shift sequence to let the output conversion descriptor of an
# Unbuffered-Channel-Stream return to the initial state.
# oconv_unshift_output_unbuffered(stream);
# > stream: Unbuffered-Channel-Stream
  #if defined(UNICODE) && (defined(GNU_LIBICONV) || defined(HAVE_ICONV))
    #define oconv_unshift_output_unbuffered(stream)  \
      if (ChannelStream_oconvdesc(stream) != (iconv_t)0) { \
        oconv_unshift_output_unbuffered_(stream);          \
      }
    local void oconv_unshift_output_unbuffered_ (object stream);
    local void oconv_unshift_output_unbuffered_(stream)
      var object stream;
      {
        #define tmpbufsize 4096
        var uintB tmpbuf[tmpbufsize];
        var char* outptr = (char*)tmpbuf;
        var size_t outsize = tmpbufsize;
        begin_system_call();
        var size_t res = iconv(ChannelStream_oconvdesc(stream),NULL,NULL,&outptr,&outsize);
        if (res == (size_t)(-1)) {
          if (errno == E2BIG) { # output buffer too small?
            NOTREACHED
          } else {
            OS_error();
          }
        }
        end_system_call();
        var uintL outcount = outptr-(char*)tmpbuf;
        if (outcount > 0) {
          UnbufferedStreamLow_write_array(stream)(stream,&tmpbuf[0],outcount);
        }
        #undef tmpbufsize;
      }
  #else
    #define oconv_unshift_output_unbuffered(stream)
  #endif

# UP: Bringt den wartenden Output eines Unbuffered-Channel-Stream ans Ziel.
# finish_output_unbuffered(stream);
# > stream: Handle-Stream
# can trigger GC
  local void finish_output_unbuffered (object stream);
  local void finish_output_unbuffered(stream)
    var object stream;
    {
      oconv_unshift_output_unbuffered(stream);
      UnbufferedStreamLow_finish_output(stream)(stream);
    }

# UP: Bringt den wartenden Output eines Unbuffered-Channel-Stream ans Ziel.
# force_output_unbuffered(stream);
# > stream: Handle-Stream
# can trigger GC
  local void force_output_unbuffered (object stream);
  local void force_output_unbuffered(stream)
    var object stream;
    {
      oconv_unshift_output_unbuffered(stream);
      UnbufferedStreamLow_force_output(stream)(stream);
    }

# UP: Löscht den wartenden Output eines Unbuffered-Channel-Stream.
# clear_output_unbuffered(stream);
# > stream: Handle-Stream
# can trigger GC
  local void clear_output_unbuffered (object stream);
  local void clear_output_unbuffered(stream)
    var object stream;
    {
      UnbufferedStreamLow_clear_output(stream)(stream);
    }

# Initializes the output side fields of an unbuffered handle stream.
# UnbufferedHandleStream_output_init(stream);
  #define UnbufferedHandleStream_output_init(stream)  \
    { UnbufferedStreamLow_write(stream) = &low_write_unbuffered_handle;                 \
      UnbufferedStreamLow_write_array(stream) = &low_write_array_unbuffered_handle;     \
      UnbufferedStreamLow_finish_output(stream) = &low_finish_output_unbuffered_handle; \
      UnbufferedStreamLow_force_output(stream) = &low_force_output_unbuffered_handle;   \
      UnbufferedStreamLow_clear_output(stream) = &low_clear_output_unbuffered_handle;   \
    }

# Schließt einen Channel-Stream.
# close_ochannel(stream);
# > stream : Channel-Stream
  local void close_ochannel (object stream);
  local void close_ochannel(stream)
    var object stream;
    {
      oconv_unshift_output_unbuffered(stream);
      ChannelStreamLow_close(stream)(stream,TheStream(stream)->strm_ochannel);
      ChannelStream_fini(stream);
      if (ChannelStream_bitsize(stream) > 0) {
        ChannelStream_bitsize(stream) = 0; # bitsize löschen
        TheStream(stream)->strm_bitbuffer = NIL; # Bitbuffer freimachen
      }
    }


# Unbuffered File-Stream
# ======================

# UP: Überprüft ein Element-Type für einen Unbuffered-Stream
# check_unbuffered_eltype(&eltype);
# > eltype: Element-Type in decoded form
  local void check_unbuffered_eltype (const decoded_eltype* eltype);
  local void check_unbuffered_eltype(eltype)
    var const decoded_eltype* eltype;
    {
      if (!((eltype->kind == eltype_ch) || ((eltype->size % 8) == 0))) {
        pushSTACK(canon_eltype(eltype));
        pushSTACK(S(Kelement_type));
        fehler(error,
               GETTEXT("Unbuffered streams need an ~ with a bit size being a multiple of 8, not ~")
              );
      }
    }

# UP: Fills in the pseudofunctions for an unbuffered stream.
# fill_pseudofuns_unbuffered(stream,&eltype);
# > stream: stream being built up, with correct strmflags and encoding
# > eltype: Element-Type in decoded form
  local void fill_pseudofuns_unbuffered (object stream, const decoded_eltype* eltype);
  local void fill_pseudofuns_unbuffered(stream,eltype)
    var object stream;
    var const decoded_eltype* eltype;
    {
      var uintB flags = TheStream(stream)->strmflags;
      if (flags & strmflags_rd_B) {
        if (eltype->kind==eltype_ch) {
          TheStream(stream)->strm_rd_by = P(rd_by_error);
          TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
          TheStream(stream)->strm_rd_ch = P(rd_ch_unbuffered);
          TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_unbuffered);
        } else {
          TheStream(stream)->strm_rd_by =
            (eltype->kind == eltype_iu
             ? (eltype->size == 8
                ? P(rd_by_iau8_unbuffered)
                : P(rd_by_iau_unbuffered)
               )
             : P(rd_by_ias_unbuffered)
            );
          TheStream(stream)->strm_rd_by_array =
            ((eltype->kind == eltype_iu) && (eltype->size == 8)
             ? P(rd_by_array_iau8_unbuffered)
             : P(rd_by_array_dummy)
            );
          TheStream(stream)->strm_rd_ch = P(rd_ch_error);
          TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_error);
        }
      } else {
        TheStream(stream)->strm_rd_by = P(rd_by_error);
        TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
        TheStream(stream)->strm_rd_ch = P(rd_ch_error);
        TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_error);
      }
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      if (flags & strmflags_wr_B) {
        if (eltype->kind == eltype_ch) {
          TheStream(stream)->strm_wr_by = P(wr_by_error);
          TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
          var object eol = TheEncoding(TheStream(stream)->strm_encoding)->enc_eol;
          if (eq(eol,S(Kunix))) {
            TheStream(stream)->strm_wr_ch = P(wr_ch_unbuffered_unix);
            TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_unbuffered_unix);
          } elif (eq(eol,S(Kmac))) {
            TheStream(stream)->strm_wr_ch = P(wr_ch_unbuffered_mac);
            TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_unbuffered_mac);
          } elif (eq(eol,S(Kdos))) {
            TheStream(stream)->strm_wr_ch = P(wr_ch_unbuffered_dos);
            TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_unbuffered_dos);
          } else {
            NOTREACHED;
          }
        } else {
          TheStream(stream)->strm_wr_by =
            (eltype->kind == eltype_iu
             ? (eltype->size == 8
                ? P(wr_by_iau8_unbuffered)
                : P(wr_by_iau_unbuffered)
               )
             : P(wr_by_ias_unbuffered)
            );
          TheStream(stream)->strm_wr_by_array =
            ((eltype->kind == eltype_iu) && (eltype->size == 8)
             ? P(wr_by_array_iau8_unbuffered)
             : P(wr_by_array_dummy)
            );
          TheStream(stream)->strm_wr_ch = P(wr_ch_error);
          TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_error);
        }
      } else {
        TheStream(stream)->strm_wr_by = P(wr_by_error);
        TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
        TheStream(stream)->strm_wr_ch = P(wr_ch_error);
        TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_error);
      }
    }

# UP: erzeugt ein Unbuffered-Channel-Stream
# make_unbuffered_stream(type,direction,&eltype,handle_tty)
# > STACK_2: Encoding
# > STACK_1: Element-Type
# > STACK_0: Handle des geöffneten Files
# > type: stream type
# > direction: Modus (0 = :PROBE, 1 = :INPUT, 4 = :OUTPUT, 5 = :IO, 3 = :INPUT-IMMUTABLE)
# > eltype: Element-Type in decoded form
# > handle_tty: ob das Handle ein tty ist (nur nötig falls direction & bit(0))
# < ergebnis: File-Handle-Stream, Handle_{input,output}_init noch aufzurufen
# < STACK: aufgeräumt
# can trigger GC
  local object make_unbuffered_stream (uintB type, uintB direction, const decoded_eltype* eltype, boolean handle_tty);
  local object make_unbuffered_stream(type,direction,eltype,handle_tty)
    var uintB type;
    var uintB direction;
    var const decoded_eltype* eltype;
    var boolean handle_tty;
    {
      # Flags:
      var uintB flags =
          ((direction & bit(0)) ? strmflags_rd_B : 0) # evtl. READ-CHAR, READ-BYTE erlaubt
        | ((direction & bit(2)) ? strmflags_wr_B : 0) # evtl. WRITE-CHAR, WRITE-BYTE erlaubt
        | ((direction & bit(1)) ? strmflags_immut_B : 0) # evtl. immutable Objekte
        ;
      if (eltype->kind == eltype_ch)
        flags &= strmflags_ch_B | strmflags_immut_B;
      else
        flags &= strmflags_by_B | strmflags_immut_B;
      # Stream allozieren:
      var object stream = allocate_stream(flags,type,strm_channel_len,sizeof(strm_unbuffered_extrafields_struct));
      # und füllen:
      TheStream(stream)->strm_encoding = STACK_2;
      fill_pseudofuns_unbuffered(stream,eltype);
      UnbufferedStream_ignore_next_LF(stream) = FALSE;
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      {
        var object handle = popSTACK();
        if (direction & bit(0))
          TheStream(stream)->strm_ichannel = handle; # Handle eintragen
        if (direction & bit(2))
          TheStream(stream)->strm_ochannel = handle; # Handle eintragen
        if (type == strmtype_file)
          TheStream(stream)->strm_buffered_channel = handle; # Handle eintragen
      }
      # Flag isatty = (handle_tty ? T : NIL) eintragen:
      TheStream(stream)->strm_isatty = (handle_tty ? T : NIL);
      TheStream(stream)->strm_eltype = popSTACK();
      ChannelStream_buffered(stream) = FALSE;
      ChannelStream_init(stream);
      # element-type dependent initializations:
      ChannelStream_bitsize(stream) = eltype->size;
      ChannelStream_lineno(stream) = 1; # initialize always (cf. set-stream-element-type)
      if (!(eltype->kind == eltype_ch)) {
        # File-Stream für Integers
        # Bitbuffer allozieren:
        pushSTACK(stream);
        var object bitbuffer = allocate_bit_vector(Atype_Bit,eltype->size);
        stream = popSTACK();
        TheStream(stream)->strm_bitbuffer = bitbuffer;
      }
      skipSTACK(1);
      return stream;
    }


# File-Stream
# ===========

# Um nicht für jedes Character das UNIX/AMIGADOS bemühen zu müssen,
# wird ein eigener Buffer geführt.
# (Dies bewirkte z.B. beim Einlesen eines 408 KByte- Files auf dem Atari
# eine Beschleunigung um einen Faktor 2.7 von 500 sec auf 180 sec.)

# Additional fields:
  # define strm_file_name        strm_field1   # Filename, a pathname or NIL
  # define strm_file_truename    strm_field2   # Truename, ein non-logical pathname or NIL
  # define strm_buffered_channel strm_ochannel # a wrapped Handle
  #define strm_buffered_bufflen  4096          # buffer length, a power of 2, <2^16
  #define strm_buffered_buffer   strm_buffer   # our own buffer, a simple-bit-vector
                                               # with strm_buffered_bufflen bytes

# Additional binary (not GCed) fields:
typedef struct strm_buffered_extrafields_struct {
  strm_channel_extrafields_struct _parent;
  uintL (* low_fill)  (object stream);
  void  (* low_flush) (object stream, uintL bufflen);
  uintL buffstart;              # start position of buffer
  sintL eofindex;               # index up to which the data is valid
                                # (for recognizing EOF)
  #define eofindex_all_invalid  (-1)
  #define eofindex_all_valid    (-2)
  uintL index;                  # index into buffer (>=0, <=strm_buffered_bufflen)
  boolean modified : 8;         # TRUE if the buffer contains modified data, else FALSE
  boolean regular : 8;          # whether the handle refers to a regular file
  boolean blockpositioning : 8; # whether the handle refers to a regular file
                                # and permits to position the buffer at
                                # buffstart = (sector number) * strm_buffered_bufflen
  # Three cases:
  #  eofindex = eofindex_all_invalid: buffer contents completely invalid,
  #                                   index = 0.
  #  eofindex >= 0: 0 <= index <= eofindex <= strm_buffered_bufflen.
  #  eofindex = eofindex_all_valid: buffer contents completely valid,
  #                                 0 <= index <= strm_buffered_bufflen.
  # buffstart = (sector number) * strm_buffered_bufflen, if blockpositioning permitted.
  # The position of handle, known to the OS, set via lseek, is normally (but
  # not always!) the end of the current buffer:
  #   if eofindex = eofindex_all_valid: buffstart + strm_buffered_bufflen,
  #   if eofindex >= 0: buffstart + eofindex,
  #   if eofindex = eofindex_all_invalid: buffstart.
# Up to now a file is considered built from bytes à 8 bits.
# Logically, it is built up from other units:
  uintL position;               # position in logical units
} strm_buffered_extrafields_struct;

# More fields in file streams with element type INTEGER, type ib or ic.
typedef struct strm_i_buffered_extrafields_struct {
  strm_buffered_extrafields_struct _parent;
  # If bitsize is not a multiple of 8:
  uintL bitindex;               # index in the current byte, >=0, <=8
  # The buffer contains 8*index+bitindex bits. The bits are ordered in the
  # order bit0,....,bit7. If bitsize<8, the length of the file (measured in
  # bits) is stored in the first 4 bytes of the files [in little-endian order]
  # when the file is closed. The actual data then begins in the 5th byte.
  uintL eofposition;            # position of logical EOF
} strm_i_buffered_extrafields_struct;

# In closed file streams only the fields `name' and `truename' are relevant.

# Accessors.
#define FileStream_name(stream)  TheStream(stream)->strm_file_name
#define FileStream_truename(stream)  TheStream(stream)->strm_file_truename
#define BufferedStream_channel(stream)  TheStream(stream)->strm_buffered_channel
#define BufferedStream_buffer(stream)  TheStream(stream)->strm_buffered_buffer
#define BufferedStreamLow_fill(stream)  \
  ((strm_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_fill
#define BufferedStreamLow_flush(stream)  \
  ((strm_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->low_flush
#define BufferedStream_buffstart(stream)  \
  ((strm_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->buffstart
#define BufferedStream_eofindex(stream)  \
  ((strm_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->eofindex
#define BufferedStream_index(stream)  \
  ((strm_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->index
#define BufferedStream_modified(stream)  \
  ((strm_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->modified
#define BufferedStream_regular(stream)  \
  ((strm_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->regular
#define BufferedStream_blockpositioning(stream)  \
  ((strm_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->blockpositioning
#define BufferedStream_position(stream)  \
  ((strm_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->position
#define BufferedStream_bitindex(stream)  \
  ((strm_i_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->bitindex
#define BufferedStream_eofposition(stream)  \
  ((strm_i_buffered_extrafields_struct*)&TheStream(stream)->strm_channel_extrafields)->eofposition

# File-Stream allgemein
# =====================

#if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
# Annahme: Alle von OPEN(2) gelieferten File-Descriptoren (hier Handles
# genannt) passen in ein uintW.
# Begründung: Bekanntlich ist 0 <= fd < getdtablesize() .
#endif

# Note about regular and non-regular files:
# - For regular files that were opened with O_RDONLY or O_RDWR but not O_WRONLY,
#   we assume that it makes sense to read a block, modify it, reposition the
#   handle back to the beginning of the block and write it back.
# - For regular files opened with O_WRONLY, we use a simple output buffer.
# - For non-regular files, we don't call handle_lseek. Therefore mixed I/O is
#   not possible. Only input-only and output-only modes are possible.

# Handle positionieren:
# handle_lseek(stream,handle,offset,mode,ergebnis_zuweisung);
# > mode: Positionierungsmodus:
#         SEEK_SET  "absolut"
#         SEEK_CUR  "relativ"
#         SEEK_END  "ab Ende"
# < ergebnis: neue Position
  #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
    #define handle_lseek(stream,handle,offset,mode,ergebnis_zuweisung)  \
      { var sintL ergebnis =                                  \
          lseek(TheHandle(handle),                            \
                offset,                                       \
                mode                                          \
               );                                             \
        if (ergebnis<0) # Fehler aufgetreten?                 \
          { end_system_call(); OS_filestream_error(stream); } \
        unused (ergebnis_zuweisung ergebnis);                 \
      }
  #endif
  #ifdef AMIGAOS
    #define handle_lseek(stream,handle,offset,mode,ergebnis_zuweisung)  \
      { var uintL _offset = (offset);                                \
        var sintL ergebnis =                                         \
          Seek(TheHandle(handle),                                    \
               _offset,                                              \
               mode                                                  \
              );                                                     \
        if (ergebnis<0) # Fehler aufgetreten?                        \
          { end_system_call(); OS_filestream_error(stream); }        \
        if (mode==SEEK_SET) { unused (ergebnis_zuweisung _offset); } \
        elif (mode==SEEK_CUR) { unused (ergebnis_zuweisung ergebnis+_offset); } \
        else /* mode==SEEK_END */                                    \
          { ergebnis = Seek(TheHandle(handle),0,SEEK_CUR);           \
            if (ergebnis<0) # Fehler aufgetreten?                    \
              { end_system_call(); OS_filestream_error(stream); }    \
            unused (ergebnis_zuweisung ergebnis);                    \
      }   }
    #define SEEK_SET  OFFSET_BEGINNING
    #define SEEK_CUR  OFFSET_CURRENT
    #define SEEK_END  OFFSET_END
  #endif
  #ifdef WIN32_NATIVE
    #define handle_lseek(stream,handle,offset,mode,ergebnis_zuweisung)  \
      { var DWORD ergebnis =                                  \
          SetFilePointer(TheHandle(handle),                   \
                         offset, NULL,                        \
                         mode                                 \
                        );                                    \
        if (ergebnis == (DWORD)(-1))                          \
          { end_system_call(); OS_filestream_error(stream); } \
        unused (ergebnis_zuweisung ergebnis);                 \
       }
  #endif

# UP: Fills the buffer, up to strm_buffered_bufflen bytes.
# low_fill_buffered_handle(stream)
# > stream: (open) byte-based file stream
# < result: number of bytes read
  local uintL low_fill_buffered_handle (object stream);
  local uintL low_fill_buffered_handle(stream)
    var object stream;
    {
      begin_system_call();
      var sintL result = # Buffer füllen
        full_read(TheHandle(TheStream(stream)->strm_buffered_channel), # Handle
                  &TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[0], # Bufferadresse
                  strm_buffered_bufflen
                 );
      end_system_call();
      if (result<0) # Fehler aufgetreten?
        OS_filestream_error(stream);
      return result;
    }

# Functions for writing the buffer.
# low_flush_buffered_handle(stream,bufflen);
# buffered_full_flush(stream);
# buffered_half_flush(stream);
# buffered_flush(stream);
# These are called only if the buffer is modified.
# Of course, the buffer is modified only by the WRITE-BYTE/WRITE-CHAR
# operations.

# UP: Beendet das Zurückschreiben des Buffers.
# low_flush_buffered_handle(stream,bufflen);
# > stream : (offener) Byte-basierter File-Stream.
# > bufflen : Anzahl der zu schreibenden Bytes
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void low_flush_buffered_handle (object stream, uintL bufflen);
  local void low_flush_buffered_handle(stream,bufflen)
    var object stream;
    var uintL bufflen;
    {
      begin_system_call();
      var sintL ergebnis = # Buffer hinausschreiben
        full_write(TheHandle(TheStream(stream)->strm_buffered_channel), # Handle
                   &TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[0], # Bufferadresse
                   bufflen
                  );
      if (ergebnis==bufflen) {
        # alles korrekt geschrieben
        end_system_call(); BufferedStream_modified(stream) = FALSE;
      } else {
        # Nicht alles geschrieben
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
        if (ergebnis<0) { # Fehler aufgetreten?
            end_system_call(); OS_filestream_error(stream);
        }
        #endif
        end_system_call();
        # Nicht alles geschrieben, wohl wegen voller Diskette.
        # Um Inkonsistenzen zu vermeiden, muss man das File schließen.
        BufferedStream_modified(stream) = FALSE; # Hierbei gehen Daten verloren!
        pushSTACK(stream);
        builtin_stream_close(&STACK_0); # File schließen
        clr_break_sem_4(); # keine UNIX-Operation mehr aktiv
        # Fehler melden.
        pushSTACK(!nullp(TheStream(STACK_0)->strm_file_truename) ? TheStream(STACK_0)->strm_file_truename : STACK_0); # Wert für Slot PATHNAME von FILE-ERROR
        pushSTACK(STACK_(0+1)); # stream
        fehler(file_error,
               GETTEXT("Closed ~ because disk is full.")
              );
      }
    }

#define BufferedHandleStream_init(stream)  \
  { BufferedStreamLow_fill(stream) = &low_fill_buffered_handle;   \
    BufferedStreamLow_flush(stream) = &low_flush_buffered_handle; \
  }

# UP: Schreibt den vollen, modifizierten Buffer zurück.
# buffered_full_flush(stream);
# > stream : (offener) Byte-basierter File-Stream.
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void buffered_full_flush (object stream);
  local void buffered_full_flush(stream)
    var object stream;
    {
      # erst zurückpositionieren, dann schreiben.
      if (BufferedStream_blockpositioning(stream)) {
        begin_system_call();
        handle_lseek(stream,TheStream(stream)->strm_buffered_channel,
                     -(long)strm_buffered_bufflen,SEEK_CUR,); # Zurückpositionieren
        end_system_call();
      }
      BufferedStreamLow_flush(stream)(stream,strm_buffered_bufflen);
    }

# UP: Schreibt den halbvollen, modifizierten Buffer zurück.
# buffered_half_flush(stream);
# > stream : (offener) Byte-basierter File-Stream.
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void buffered_half_flush (object stream);
  local void buffered_half_flush(stream)
    var object stream;
    {
      if (BufferedStream_blockpositioning(stream)) {
        begin_system_call();
        handle_lseek(stream,TheStream(stream)->strm_buffered_channel,
                     BufferedStream_buffstart(stream),SEEK_SET,); # Zurückpositionieren
        end_system_call();
      }
      # eofindex Bytes schreiben:
      BufferedStreamLow_flush(stream)(stream,BufferedStream_eofindex(stream));
    }

# UP: Schreibt den modifizierten Buffer zurück.
# buffered_flush(stream);
# > stream : (offener) Byte-basierter File-Stream.
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void buffered_flush (object stream);
  local void buffered_flush(stream)
    var object stream;
    {
      if (BufferedStream_eofindex(stream) == eofindex_all_valid) # Buffer ganz gültig ?
        buffered_full_flush(stream);
      else
        buffered_half_flush(stream);
    }

# UP: Positioniert einen Byte-basierten File-Stream so, dass das nächste Byte
# gelesen oder überschrieben werden kann.
# buffered_nextbyte(stream)
# > stream : (offener) Byte-basierter File-Stream.
# < ergebnis : NULL falls EOF (und dann ist index=eofindex),
#              sonst: Pointer auf nächstes Byte
# verändert in stream: index, eofindex, buffstart
  local uintB* buffered_nextbyte (object stream);
  local uintB* buffered_nextbyte(stream)
    var object stream;
    {
      var sintL eofindex = BufferedStream_eofindex(stream);
      var uintL index = BufferedStream_index(stream);
      if (!(eofindex == eofindex_all_valid)) {
        # Bufferdaten nur halb gültig
        if (eofindex == eofindex_all_invalid)
          # Bufferdaten ganz ungültig
          goto reread;
        else
          # EOF tritt in diesem Sector auf
          goto eofsector;
      }
      # Bufferdaten ganz gültig
      if (!(index == strm_buffered_bufflen)) { # index = bufflen ?
        # nein, also 0 <= index < strm_buffered_bufflen -> OK
        return &TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[index];
      }
      # Buffer muss neu gefüllt werden.
      if (BufferedStream_modified(stream))
        # Zuvor muss der Buffer hinausgeschrieben werden:
        buffered_full_flush(stream);
      BufferedStream_buffstart(stream) += strm_buffered_bufflen;
     reread: # Ab hier den Buffer neu lesen:
      {
        var sintL ergebnis;
        if (BufferedStream_blockpositioning(stream)
            || !((TheStream(stream)->strmflags & strmflags_rd_B) == 0)
           ) {
          ergebnis = BufferedStreamLow_fill(stream)(stream);
          if (ergebnis==strm_buffered_bufflen) {
            # der ganze Buffer wurde gefüllt
            BufferedStream_index(stream) = 0; # Index := 0
            BufferedStream_modified(stream) = FALSE; # Buffer unmodifiziert
            BufferedStream_eofindex(stream) = eofindex_all_valid; # eofindex := all_valid
            return &TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[0];
          }
        } else {
          ergebnis = 0;
        }
        # Es wurden ergebnis (< strm_buffered_bufflen) Bytes gelesen.
        # Nicht der ganze Buffer wurde gefüllt -> EOF ist erreicht.
        BufferedStream_index(stream) = index = 0; # Index := 0
        BufferedStream_modified(stream) = FALSE; # Buffer unmodifiziert
        BufferedStream_eofindex(stream) = eofindex = ergebnis; # eofindex := ergebnis
      }
     eofsector: # eofindex ist ein Fixnum, d.h. EOF tritt in diesem Sector auf.
      if (index == eofindex)
        return (uintB*)NULL; # EOF erreicht
      else
        return &TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[index];
    }

# UP: Bereitet das Schreiben eines Bytes am EOF vor.
# buffered_eofbyte(stream);
# > stream : (offener) Byte-basierter File-Stream,
#            bei dem gerade buffered_nextbyte(stream)==NULL ist.
# < ergebnis : Pointer auf nächstes (freies) Byte
# verändert in stream: index, eofindex, buffstart
  local uintB* buffered_eofbyte (object stream);
  local uintB* buffered_eofbyte(stream)
    var object stream;
    {
      # EOF. Es ist eofindex=index.
      if (BufferedStream_eofindex(stream) == strm_buffered_bufflen) {
        # eofindex = strm_buffered_bufflen
        # Buffer muss neu gefüllt werden. Da nach ihm sowieso EOF kommt,
        # genügt es, ihn hinauszuschreiben:
        if (BufferedStream_modified(stream))
          buffered_half_flush(stream);
        BufferedStream_buffstart(stream) += strm_buffered_bufflen;
        BufferedStream_eofindex(stream) = 0; # eofindex := 0
        BufferedStream_index(stream) = 0; # index := 0
        BufferedStream_modified(stream) = FALSE; # unmodifiziert
      }
      # eofindex erhöhen:
      BufferedStream_eofindex(stream) += 1;
      return &TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[BufferedStream_index(stream)];
    }

# UP: Schreibt ein Byte auf einen Byte-basierten File-Stream.
# buffered_writebyte(stream,b);
# > stream : (offener) Byteblock-basierter File-Stream.
# > b : zu schreibendes Byte
# verändert in stream: index, eofindex, buffstart
  local void buffered_writebyte (object stream, uintB b);
  local void buffered_writebyte(stream,b)
    var object stream;
    var uintB b;
    {
      var uintB* ptr = buffered_nextbyte(stream);
      if (!(ptr == (uintB*)NULL)) {
        if (*ptr == b) # keine wirkliche Modifikation?
          goto no_modification;
      } else {
        ptr = buffered_eofbyte(stream); # EOF -> 1 Byte Platz machen
      }
      # nächstes Byte in den Buffer schreiben:
      *ptr = b; BufferedStream_modified(stream) = TRUE;
     no_modification:
      # index incrementieren:
      BufferedStream_index(stream) += 1;
    }

# File-Stream, Byte-basiert (b_file)
# ===========  ============

# Fehler wegen Positionierung hinter EOF.
# fehler_position_beyond_EOF(stream);
  nonreturning_function(local, fehler_position_beyond_EOF, (object stream));
  local void fehler_position_beyond_EOF(stream)
    var object stream;
    {
      pushSTACK(!nullp(TheStream(stream)->strm_file_truename) ? TheStream(stream)->strm_file_truename : stream); # Wert für Slot PATHNAME von FILE-ERROR
      pushSTACK(stream);
      fehler(file_error,
             GETTEXT("cannot position ~ beyond EOF")
            );
    }

# UP: Positioniert einen (offenen) Byte-basierten File-Stream an eine
# gegebene Position.
# position_file_buffered(stream,position);
# > stream : (offener) Byte-basierter File-Stream.
# > position : neue Position
# verändert in stream: index, eofindex, buffstart
  local void position_file_buffered (object stream, uintL position);
  local void position_file_buffered(stream,position)
    var object stream;
    var uintL position;
    {
      # Liegt die neue Position im selben Sector?
      {
        var sintL eofindex = BufferedStream_eofindex(stream);
        var uintL newindex = position - BufferedStream_buffstart(stream);
        if (newindex
            <= ((eofindex == eofindex_all_valid) ? strm_buffered_bufflen :
                (!(eofindex == eofindex_all_invalid)) ? eofindex :
                0
           )   ) {
          # ja -> brauche nur index zu verändern:
          BufferedStream_index(stream) = newindex;
          return;
        }
      }
      # evtl. Buffer hinausschreiben:
      if (BufferedStream_modified(stream))
        buffered_flush(stream);
      # Nun ist modified_flag gelöscht.
      if (!BufferedStream_blockpositioning(stream)) {
        # Positionieren:
        begin_system_call();
        handle_lseek(stream,TheStream(stream)->strm_buffered_channel,
                     position,SEEK_SET,);
        end_system_call();
        BufferedStream_buffstart(stream) = position;
        BufferedStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
        BufferedStream_index(stream) = 0; # index := 0
        BufferedStream_modified(stream) = FALSE; # unmodifiziert
      } else {
        var uintL oldposition = BufferedStream_buffstart(stream) + BufferedStream_index(stream);
        # Positionieren:
        {
          var uintL newposition;
          begin_system_call();
          handle_lseek(stream,TheStream(stream)->strm_buffered_channel,
                       floor(position,strm_buffered_bufflen)*strm_buffered_bufflen,SEEK_SET,newposition=);
          end_system_call();
          BufferedStream_buffstart(stream) = newposition;
        }
        # Sector lesen:
        BufferedStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
        BufferedStream_index(stream) = 0; # index := 0
        BufferedStream_modified(stream) = FALSE; # unmodifiziert
        var uintL newindex = position % strm_buffered_bufflen; # gewünschter Index im Sector
        if (!(newindex==0)) { # Position zwischen Sectoren -> brauche nichts zu lesen
          buffered_nextbyte(stream);
          # Jetzt ist index=0.
          # index auf (position mod bufflen) setzen, vorher überprüfen:
          var sintL eofindex = BufferedStream_eofindex(stream);
          # Es muss entweder eofindex=all_valid oder 0<=newindex<=eofindex sein:
          if (!((eofindex == eofindex_all_valid) || (newindex <= eofindex))) {
            # Fehler. Aber erst an die alte Position zurückpositionieren:
            check_SP();
            position_file_buffered(stream,oldposition); # zurückpositionieren
            fehler_position_beyond_EOF(stream);
          }
          BufferedStream_index(stream) = newindex;
        }
      }
    }

# UP: Liest einen Array von Bytes von einem (offenen) Byte-basierten
# File-Stream.
# read_byte_array_buffered(stream,byteptr,len)
# > stream : (offener) Byte-basierter File-Stream.
# > byteptr[0..len-1] : Platz
# > len : > 0
# < byteptr[0..count-1] : eingelesene Bytes.
# < result: &byteptr[count] (with count = len, or count < len if EOF reached)
# verändert in stream: index, eofindex, buffstart
  local uintB* read_byte_array_buffered (object stream, uintB* byteptr, uintL len);
  local uintB* read_byte_array_buffered(stream,byteptr,len)
    var object stream;
    var uintB* byteptr;
    var uintL len;
    {
      do {
        var uintB* ptr = buffered_nextbyte(stream);
        if (ptr == (uintB*)NULL)
          break;
        var sintL eofindex = BufferedStream_eofindex(stream);
        var uintL available =
          (eofindex == eofindex_all_valid ? strm_buffered_bufflen : eofindex)
          - BufferedStream_index(stream);
        if (available > len)
          available = len;
        # Alle verfügbaren Bytes kopieren:
        {
          var uintL count;
          dotimespL(count,available, { *byteptr++ = *ptr++; } );
        }
        # index incrementieren:
        BufferedStream_index(stream) += available;
        len -= available;
      } while (len > 0);
      return byteptr;
    }

# UP: Schreibt einen Array von Bytes auf einen (offenen) Byte-basierten
# File-Stream.
# write_byte_array_buffered(stream,byteptr,len)
# > stream : (offener) Byte-basierter File-Stream.
# > byteptr[0..len-1] : auszugebende Bytes.
# > len : > 0
# < result: &byteptr[len]
# verändert in stream: index, eofindex, buffstart
  local const uintB* write_byte_array_buffered (object stream, const uintB* byteptr, uintL len);
  local const uintB* write_byte_array_buffered(stream,byteptr,len)
    var object stream;
    var const uintB* byteptr;
    var uintL len;
    {
      var uintL remaining = len;
      var uintB* ptr;
      do { # Noch remaining>0 Bytes abzulegen.
        ptr = buffered_nextbyte(stream);
        if (ptr == (uintB*)NULL)
          goto eof_reached;
        var sintL eofindex = BufferedStream_eofindex(stream);
        var uintL next = # so viel wie noch in den Buffer oder bis EOF passt
          ((eofindex==eofindex_all_valid) ? strm_buffered_bufflen : eofindex)
          - BufferedStream_index(stream); # > 0 !
        if (next > remaining)
          next = remaining;
        # next Bytes in den Buffer kopieren:
        {
          var uintL count;
          dotimespL(count,next, {
            var uintB b = *byteptr++; # nächstes Byte
            if (!(*ptr == b)) {
              *ptr = b; BufferedStream_modified(stream) = TRUE; # in den Buffer
            }
            ptr++;
          });
        }
        remaining = remaining - next;
        # index incrementieren:
        BufferedStream_index(stream) += next;
      } until (remaining == 0);
      if (FALSE) {
       eof_reached: # Schreiben am EOF, eofindex = index
        do { # Noch remaining>0 Bytes abzulegen.
          var uintL next = # so viel wie noch Platz im Buffer ist
            strm_buffered_bufflen - BufferedStream_index(stream);
          if (next==0) {
            # Buffer muss neu gefüllt werden. Da nach ihm sowieso EOF kommt,
            # genügt es, ihn hinauszuschreiben:
            if (BufferedStream_modified(stream))
              buffered_half_flush(stream);
            BufferedStream_buffstart(stream) += strm_buffered_bufflen;
            BufferedStream_eofindex(stream) = 0; # eofindex := 0
            BufferedStream_index(stream) = 0; # index := 0
            BufferedStream_modified(stream) = FALSE; # unmodifiziert
            # Dann nochmals versuchen:
            next = strm_buffered_bufflen;
          }
          if (next > remaining)
            next = remaining;
          # next Bytes in den Buffer kopieren:
          {
            var uintL count;
            ptr = &TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[BufferedStream_index(stream)];
            dotimespL(count,next, { *ptr++ = *byteptr++; } );
            BufferedStream_modified(stream) = TRUE;
          }
          remaining = remaining - next;
          # index und eofindex incrementieren:
          BufferedStream_index(stream) += next;
          BufferedStream_eofindex(stream) += next;
        } until (remaining == 0);
      }
      return byteptr;
    }

# File-Stream für Characters
# ==========================

# Input side
# ----------

# READ-CHAR - Pseudofunktion für File-Streams für Characters
  local object rd_ch_buffered (const object* stream_);
  local object rd_ch_buffered(stream_)
    var const object* stream_;
    {
      var object stream = *stream_;
      var uintB* bufferptr = buffered_nextbyte(stream);
      if (bufferptr == (uintB*)NULL) # EOF ?
        return eof_value;
      # nächstes Zeichen holen:
      var chart c;
      #ifdef UNICODE
      var object encoding = TheStream(stream)->strm_encoding;
      # Does the buffer contain a complete character?
      {
        var sintL eofindex = BufferedStream_eofindex(stream);
        var uintL available =
          (eofindex == eofindex_all_valid ? strm_buffered_bufflen : eofindex)
          - BufferedStream_index(stream);
        var const uintB* bptr = bufferptr;
        var chart* cptr = &c;
        Encoding_mbstowcs(encoding)(encoding,stream,&bptr,bufferptr+available,&cptr,&c+1);
        if (cptr == &c+1) {
          var uintL n = bptr-bufferptr;
          # index und position incrementieren:
          BufferedStream_index(stream) += n;
          BufferedStream_position(stream) += n;
        } else {
          var uintB buf[max_bytes_per_chart];
          var uintL buflen = 0;
          loop {
            ASSERT(buflen < max_bytes_per_chart);
            buf[buflen++] = *bufferptr;
            # index und position incrementieren:
            BufferedStream_index(stream) += 1;
            BufferedStream_position(stream) += 1;
            var const uintB* bptr = &buf[0];
            var chart* cptr = &c;
            Encoding_mbstowcs(encoding)(encoding,stream,&bptr,&buf[buflen],&cptr,cptr+1);
            if (cptr == &c) {
              # Not a complete character.
              # Shift the buffer
              if (!(bptr == &buf[0])) {
                var const uintB* ptr1 = bptr;
                var uintB* ptr2 = &buf[0];
                until (ptr1 == &buf[buflen]) { *ptr2++ = *ptr1++; }
                buflen = ptr2 - &buf[0];
              }
            } else {
              # Read a complete character.
              if (!(bptr == &buf[buflen])) {
                # At most one lookahead byte. Make it unread.
                ASSERT(bptr == &buf[buflen-1]);
                # index und position wieder decrementieren:
                BufferedStream_index(stream) -= 1;
                BufferedStream_position(stream) -= 1;
              }
              break;
            }
            bufferptr = buffered_nextbyte(stream);
            if (bufferptr == (uintB*)NULL)
              return eof_value;
          }
        }
      }
      #else
      c = as_chart(*bufferptr); # Character aus dem Buffer
      # index und position incrementieren:
      BufferedStream_index(stream) += 1;
      BufferedStream_position(stream) += 1;
      #endif
      if (chareq(c,ascii(NL))) {
        ChannelStream_lineno(stream) += 1;
      } elif (chareq(c,ascii(CR))) {
        # nächstes Zeichen auf LF untersuchen
        bufferptr = buffered_nextbyte(stream);
        if (!(bufferptr == (uintB*)NULL) && chareq(as_chart(*bufferptr),ascii(LF))) {
          # index und position incrementieren:
          BufferedStream_index(stream) += 1;
          BufferedStream_position(stream) += 1;
        }
        c = ascii(NL);
        ChannelStream_lineno(stream) += 1;
      }
      return code_char(c);
    }

# Stellt fest, ob ein File-Stream ein Zeichen verfügbar hat.
# listen_buffered(stream)
# > stream: File-Stream für Characters
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
  local signean listen_buffered (object stream);
  local signean listen_buffered(stream)
    var object stream;
    {
      if (buffered_nextbyte(stream) == (uintB*)NULL)
        return ls_eof; # EOF
      # In case of UNICODE, the presence of a byte does not guarantee the
      # presence of a multi-byte character. Returning ls_avail here is
      # therefore not correct. But this doesn't matter since programs seeing
      # ls_avail will call read-char, and this will do the right thing anyway.
      return ls_avail;
    }

# READ-CHAR-ARRAY - Pseudofunktion für File-Streams für Characters:
  local uintL rd_ch_array_buffered (const object* stream_, const object* chararray_, uintL start, uintL len);
  local uintL rd_ch_array_buffered(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var object stream = *stream_;
      var chart* startptr = &TheSstring(*chararray_)->data[start];
      var chart* charptr = startptr;
      #ifdef UNICODE
      var object encoding = TheStream(stream)->strm_encoding;
      var chart* endptr = startptr+len;
      loop {
        var chart* startptr = charptr;
        var uintB* bufferptr = buffered_nextbyte(stream);
        if (bufferptr == (uintB*)NULL) # EOF -> fertig
          break;
          # Read as many complete characters from the buffer as possible.
        {
          var sintL eofindex = BufferedStream_eofindex(stream);
          var uintL available =
            (eofindex == eofindex_all_valid ? strm_buffered_bufflen : eofindex)
            - BufferedStream_index(stream);
          var const uintB* bptr = bufferptr;
          var chart* cptr = charptr;
          Encoding_mbstowcs(encoding)(encoding,stream,&bptr,bufferptr+available,&cptr,endptr);
          if (!(cptr == charptr)) {
            var uintL n = bptr-bufferptr;
            # index und position incrementieren:
            BufferedStream_index(stream) += n;
            BufferedStream_position(stream) += n;
          } else {
            var uintB buf[max_bytes_per_chart];
            var uintL buflen = 0;
            loop {
              ASSERT(buflen < max_bytes_per_chart);
              buf[buflen++] = *bufferptr;
              # index und position incrementieren:
              BufferedStream_index(stream) += 1;
              BufferedStream_position(stream) += 1;
              var const uintB* bptr = &buf[0];
              var chart* cptr = charptr;
              Encoding_mbstowcs(encoding)(encoding,stream,&bptr,&buf[buflen],&cptr,cptr+1);
              if (cptr == charptr) {
                # Not a complete character.
                # Shift the buffer
                if (!(bptr == &buf[0])) {
                  var const uintB* ptr1 = bptr;
                  var uintB* ptr2 = &buf[0];
                  until (ptr1 == &buf[buflen]) { *ptr2++ = *ptr1++; }
                  buflen = ptr2 - &buf[0];
                }
              } else {
                # Read a complete character.
                if (!(bptr == &buf[buflen])) {
                  # At most one lookahead byte. Make it unread.
                  ASSERT(bptr == &buf[buflen-1]);
                  # index und position wieder decrementieren:
                  BufferedStream_index(stream) -= 1;
                  BufferedStream_position(stream) -= 1;
                }
                break;
              }
              bufferptr = buffered_nextbyte(stream);
              if (bufferptr == (uintB*)NULL) # EOF -> fertig
                break;
            }
            if (cptr == charptr) # EOF -> fertig
              break;
          }
          charptr = cptr;
        }
        # Now apply CR/LF->NL and CR->NL conversion to the characters
        # [startptr..charptr).
        {
          var const chart* ptr1 = startptr;
          var chart* ptr2 = startptr;
          do {
            var chart c = *ptr1++;
            if (chareq(c,ascii(NL))) {
              ChannelStream_lineno(stream) += 1;
            } elif (chareq(c,ascii(CR))) {
              # nächstes Zeichen auf LF untersuchen
              if (ptr1 == charptr) {
                var uintB* bufferptr = buffered_nextbyte(stream);
                if (!(bufferptr == (uintB*)NULL) && chareq(as_chart(*bufferptr),ascii(LF))) {
                  # index und position incrementieren:
                  BufferedStream_index(stream) += 1;
                  BufferedStream_position(stream) += 1;
                }
              } else {
                if (chareq(*ptr1,ascii(LF)))
                  ptr1++;
              }
              c = ascii(NL);
              ChannelStream_lineno(stream) += 1;
            }
            *ptr2++ = c;
          } until (ptr1 == charptr);
          charptr = ptr2;
        }
        if (charptr == endptr)
          break;
      }
      return charptr - startptr;
      #else
      do {
        var uintB* ptr = buffered_nextbyte(stream);
        if (ptr == (uintB*)NULL) # EOF -> fertig
          break;
        var chart ch = as_chart(*ptr);
        # index und position incrementieren:
        BufferedStream_index(stream) += 1;
        BufferedStream_position(stream) += 1;
        if (chareq(ch,ascii(NL))) {
          ChannelStream_lineno(stream) += 1;
        } elif (chareq(ch,ascii(CR))) {
          # nächstes Zeichen auf LF untersuchen
          ptr = buffered_nextbyte(stream);
          if (!(ptr == (uintB*)NULL) && chareq(as_chart(*ptr),ascii(LF))) {
            # index und position incrementieren:
            BufferedStream_index(stream) += 1;
            BufferedStream_position(stream) += 1;
          }
          ch = ascii(NL);
          ChannelStream_lineno(stream) += 1;
        }
        *charptr++ = ch; len--;
      } while (len > 0);
      return charptr - startptr;
      #endif
    }

# Output side
# -----------

# UP: Schreibt ein Byte auf einen Byte-basierten File-Stream.
# write_byte_buffered(stream,b);
# > stream : (offener) Byte-basierter File-Stream.
# > b : zu schreibendes Byte
# verändert in stream: index, eofindex, buffstart, position
  local void write_byte_buffered (object stream, uintB b);
  local void write_byte_buffered(stream,b)
    var object stream;
    var uintB b;
    {
      buffered_writebyte(stream,b);
      # position incrementieren:
      BufferedStream_position(stream) += 1;
    }

# WRITE-CHAR - Pseudofunktion für File-Streams für Characters
  local void wr_ch_buffered_unix (const object* stream_, object obj);
  local void wr_ch_buffered_unix(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      var object stream = *stream_;
      # obj muss ein Character sein:
      if (!charp(obj))
        fehler_wr_char(stream,obj);
      var chart c = char_code(obj);
      #ifdef UNICODE
      var uintB buf[max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      var const chart* cptr = &c;
      var uintB* bptr = &buf[0];
      Encoding_wcstombs(encoding)(encoding,stream,&cptr,cptr+1,&bptr,&buf[max_bytes_per_chart]);
      ASSERT(cptr == &c+1);
      var uintL buflen = bptr-&buf[0];
      write_byte_array_buffered(stream,&buf[0],buflen);
      # position incrementieren:
      BufferedStream_position(stream) += buflen;
      #else
      write_byte_buffered(stream,as_cint(c)); # unverändert ausgeben
      #endif
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für File-Streams für Characters:
  local void wr_ch_array_buffered_unix (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_buffered_unix(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var object stream = *stream_;
      var const chart* charptr;
      unpack_sstring_alloca(*chararray_,len,start, charptr=);
      var const chart* endptr = charptr + len;
      #ifdef UNICODE
      #define tmpbufsize 4096
      var uintB tmptmpbuf[tmpbufsize*max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      do {
        var uintB* bptr = &tmptmpbuf[0];
        Encoding_wcstombs(encoding)(encoding,stream,&charptr,endptr,&bptr,&tmptmpbuf[tmpbufsize*max_bytes_per_chart]);
        var uintL tmptmpbuflen = bptr-&tmptmpbuf[0];
        write_byte_array_buffered(stream,&tmptmpbuf[0],tmptmpbuflen);
        # position incrementieren:
        BufferedStream_position(stream) += tmptmpbuflen;
      } until (charptr == endptr);
      #undef tmpbufsize
      #else
      write_byte_array_buffered(stream,charptr,len);
      # position incrementieren:
      BufferedStream_position(stream) += len;
      #endif
      wr_ss_lpos(stream,endptr,len); # Line-Position aktualisieren
    }

# WRITE-CHAR - Pseudofunktion für File-Streams für Characters
  local void wr_ch_buffered_mac (const object* stream_, object obj);
  local void wr_ch_buffered_mac(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      var object stream = *stream_;
      # obj muss ein Character sein:
      if (!charp(obj))
        fehler_wr_char(stream,obj);
      var chart c = char_code(obj);
      if (chareq(c,ascii(NL)))
        c = ascii(CR);
      #ifdef UNICODE
      var uintB buf[max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      var const chart* cptr = &c;
      var uintB* bptr = &buf[0];
      Encoding_wcstombs(encoding)(encoding,stream,&cptr,cptr+1,&bptr,&buf[max_bytes_per_chart]);
      ASSERT(cptr == &c+1);
      var uintL buflen = bptr-&buf[0];
      write_byte_array_buffered(stream,&buf[0],buflen);
      # position incrementieren:
      BufferedStream_position(stream) += buflen;
      #else
      write_byte_buffered(stream,as_cint(c));
      #endif
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für File-Streams für Characters:
  local void wr_ch_array_buffered_mac (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_buffered_mac(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var object stream = *stream_;
      var const chart* charptr;
      unpack_sstring_alloca(*chararray_,len,start, charptr=);
      #ifdef UNICODE
      # Need a temporary buffer for NL->CR translation.
      #define tmpbufsize 4096
      var chart tmpbuf[tmpbufsize];
      var uintB tmptmpbuf[tmpbufsize*max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      var uintL remaining = len;
      do {
        var uintL n = remaining;
        if (n > tmpbufsize)
          n = tmpbufsize;
        {
          var chart* tmpptr = &tmpbuf[0];
          var uintL count;
          dotimespL(count,n, {
            var chart c = *charptr++;
            if (chareq(c,ascii(NL)))
              c = ascii(CR);
            *tmpptr++ = c;
          });
          var const chart* cptr = tmpbuf;
          var uintB* bptr = &tmptmpbuf[0];
          Encoding_wcstombs(encoding)(encoding,stream,&cptr,tmpptr,&bptr,&tmptmpbuf[tmpbufsize*max_bytes_per_chart]);
          ASSERT(cptr == tmpptr);
          var uintL tmptmpbuflen = bptr-&tmptmpbuf[0];
          write_byte_array_buffered(stream,&tmptmpbuf[0],tmptmpbuflen);
          # position incrementieren:
          BufferedStream_position(stream) += tmptmpbuflen;
        }
        remaining -= n;
      } while (remaining > 0);
      #undef tmpbufsize
      #else
      var uintL remaining = len;
      do {
        var chart c = *charptr++;
        if (chareq(c,ascii(NL)))
          c = ascii(CR);
        write_byte_buffered(stream,as_cint(c));
        remaining--;
      } until (remaining == 0);
      #endif
      wr_ss_lpos(stream,charptr,len); # Line-Position aktualisieren
    }

# WRITE-CHAR - Pseudofunktion für File-Streams für Characters
  local void wr_ch_buffered_dos (const object* stream_, object obj);
  local void wr_ch_buffered_dos(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      var object stream = *stream_;
      # obj muss ein Character sein:
      if (!charp(obj))
        fehler_wr_char(stream,obj);
      var chart c = char_code(obj);
      #ifdef UNICODE
      static chart const crlf[2] = { ascii(CR), ascii(LF) };
      var uintB buf[2*max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      var const chart* cp;
      var uintL n;
      if (chareq(c,ascii(NL))) {
        cp = crlf; n = 2;
      } else {
        cp = &c; n = 1;
      }
      var const chart* cptr = cp;
      var uintB* bptr = &buf[0];
      Encoding_wcstombs(encoding)(encoding,stream,&cptr,cp+n,&bptr,&buf[2*max_bytes_per_chart]);
      ASSERT(cptr == cp+n);
      var uintL buflen = bptr-&buf[0];
      write_byte_array_buffered(stream,&buf[0],buflen);
      # position incrementieren:
      BufferedStream_position(stream) += buflen;
      #else
      if (chareq(c,ascii(NL))) {
        write_byte_buffered(stream,CR); write_byte_buffered(stream,LF);
      } else {
        write_byte_buffered(stream,as_cint(c));
      }
      #endif
    }

# WRITE-CHAR-ARRAY - Pseudofunktion für File-Streams für Characters:
  local void wr_ch_array_buffered_dos (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_buffered_dos(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      var object stream = *stream_;
      var const chart* charptr;
      unpack_sstring_alloca(*chararray_,len,start, charptr=);
      #ifdef UNICODE
      # Need a temporary buffer for NL->CR translation.
      #define tmpbufsize 4096
      var chart tmpbuf[2*tmpbufsize];
      var uintB tmptmpbuf[2*tmpbufsize*max_bytes_per_chart];
      var object encoding = TheStream(stream)->strm_encoding;
      var uintL remaining = len;
      do {
        var uintL n = remaining;
        if (n > tmpbufsize)
          n = tmpbufsize;
        {
          var chart* tmpptr = &tmpbuf[0];
          var uintL count;
          dotimespL(count,n, {
            var chart c = *charptr++;
            if (chareq(c,ascii(NL))) {
              *tmpptr++ = ascii(CR); *tmpptr++ = ascii(LF);
            } else {
              *tmpptr++ = c;
            }
          });
          var const chart* cptr = tmpbuf;
          var uintB* bptr = &tmptmpbuf[0];
          Encoding_wcstombs(encoding)(encoding,stream,&cptr,tmpptr,&bptr,&tmptmpbuf[2*tmpbufsize*max_bytes_per_chart]);
          ASSERT(cptr == tmpptr);
          var uintL tmptmpbuflen = bptr-&tmptmpbuf[0];
          write_byte_array_buffered(stream,&tmptmpbuf[0],tmptmpbuflen);
          # position incrementieren:
          BufferedStream_position(stream) += tmptmpbuflen;
        }
        remaining -= n;
      } while (remaining > 0);
      #undef tmpbufsize
      #else
      var uintL remaining = len;
      do {
        var chart c = *charptr++;
        if (chareq(c,ascii(NL))) {
          write_byte_buffered(stream,CR); write_byte_buffered(stream,LF);
        } else {
          write_byte_buffered(stream,as_cint(c));
        }
        remaining--;
      } until (remaining == 0);
      #endif
      wr_ss_lpos(stream,charptr,len); # Line-Position aktualisieren
    }

# Macro: Emits a shift sequence to let the output conversion descriptor of an
# Buffered-Channel-Stream return to the initial state.
# oconv_unshift_output_buffered(stream);
# > stream: Buffered-Channel-Stream
  #if defined(UNICODE) && (defined(GNU_LIBICONV) || defined(HAVE_ICONV))
    #define oconv_unshift_output_buffered(stream)  \
      if (ChannelStream_oconvdesc(stream) != (iconv_t)0) { \
        oconv_unshift_output_buffered_(stream);            \
      }
    local void oconv_unshift_output_buffered_ (object stream);
    local void oconv_unshift_output_buffered_(stream)
      var object stream;
      {
        #define tmpbufsize 4096
        var uintB tmpbuf[tmpbufsize];
        var char* outptr = (char*)tmpbuf;
        var size_t outsize = tmpbufsize;
        begin_system_call();
        var size_t res = iconv(ChannelStream_oconvdesc(stream),NULL,NULL,&outptr,&outsize);
        if (res == (size_t)(-1)) {
          if (errno == E2BIG) { # output buffer too small?
            NOTREACHED
          } else {
            OS_error();
          }
        }
        end_system_call();
        var uintL outcount = outptr-(char*)tmpbuf;
        if (outcount > 0) {
          write_byte_array_buffered(stream,&tmpbuf[0],outcount);
          # position incrementieren:
          BufferedStream_position(stream) += outcount;
        }
        #undef tmpbufsize;
      }
  #else
    #define oconv_unshift_output_buffered(stream)
  #endif

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
# position_file_i_buffered(stream,position);
# > stream : (offener) Byte-basierter File-Stream.
# > position : neue (logische) Position
# verändert in stream: index, eofindex, buffstart, bitindex
  local void position_file_i_buffered (object stream, uintL position);
  local void position_file_i_buffered(stream,position)
    var object stream;
    var uintL position;
    {
      var uintL bitsize = ChannelStream_bitsize(stream);
      var uintL position_bits = position * bitsize;
      if (bitsize < 8)
        position_bits += sizeof(uintL)*8; # Header berücksichtigen
      # An Bit Nummer position_bits positionieren.
      position_file_buffered(stream,floor(position_bits,8)); # Aufs Byte positionieren
      if ((bitsize % 8) == 0) # Bei Art a war's das.
        return;
      if (# Liegt die angesprochene Position im ersten Byte nach EOF ?
          ((!((position_bits%8)==0))
           && (buffered_nextbyte(stream) == (uintB*)NULL)
          )
          ||
          # Liegt die angesprochene Position im letzten Byte, aber zu weit?
          ((bitsize < 8)
           && (position > BufferedStream_eofposition(stream))
         )) {
        # Fehler. Aber erst an die alte Position zurückpositionieren:
        var uintL oldposition = BufferedStream_position(stream);
        check_SP();
        position_file_i_buffered(stream,oldposition); # zurückpositionieren
        fehler_position_beyond_EOF(stream);
      }
      BufferedStream_bitindex(stream) = position_bits%8;
    }

# Input side
# ----------

# UP für READ-BYTE auf File-Streams für Integers, Art a :
# Füllt den Bitbuffer mit den nächsten bitsize Bits.
# > stream : File-Stream für Integers, Art a
# > finisher : Beendigungsroutine
# < ergebnis : gelesener Integer oder eof_value
  local object rd_by_aux_iax_buffered (object stream, rd_by_ix_I* finisher);
  local object rd_by_aux_iax_buffered(stream,finisher)
    var object stream;
    var rd_by_ix_I* finisher;
    {
      var uintL bitsize = ChannelStream_bitsize(stream);
      var uintL bytesize = bitsize/8;
      # genügend viele Bytes in den Bitbuffer übertragen:
      var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_bitbuffer)->data[0];
      #if 0 # equivalent, but slower
      var uintL count;
      dotimespL(count,bytesize, {
        var uintB* ptr = buffered_nextbyte(stream);
        if (ptr == (uintB*)NULL)
          goto eof;
        # nächstes Byte holen:
        *bitbufferptr++ = *ptr;
        # index incrementieren:
        BufferedStream_index(stream) += 1;
      });
      #else
      if (!(read_byte_array_buffered(stream,bitbufferptr,bytesize) == bitbufferptr+bytesize))
        goto eof;
      #endif
      # position incrementieren:
      BufferedStream_position(stream) += 1;
      # in Zahl umwandeln:
      return (*finisher)(stream,bitsize,bytesize);
     eof: # EOF erreicht
      position_file_buffered(stream,BufferedStream_position(stream)*bytesize);
      return eof_value;
    }

# UP für READ-BYTE auf File-Streams für Integers, Art b :
# Füllt den Bitbuffer mit den nächsten bitsize Bits.
# > stream : File-Stream für Integers, Art b
# > finisher : Beendigungsroutine
# < ergebnis : gelesener Integer oder eof_value
  local object rd_by_aux_ibx_buffered (object stream, rd_by_ix_I* finisher);
  local object rd_by_aux_ibx_buffered(stream,finisher)
    var object stream;
    var rd_by_ix_I* finisher;
    {
      # Nur bei position < eofposition gibt's was zu lesen:
      if (BufferedStream_position(stream) == BufferedStream_eofposition(stream))
        goto eof;
      {
        var uintL bitsize = ChannelStream_bitsize(stream); # bitsize (>0, <8)
        # genügend viele Bits in den Bitbuffer übertragen:
        var uintL bitindex = BufferedStream_bitindex(stream);
        var uintL count = bitindex + bitsize;
        var uint8 bit_akku;
        var uintB* ptr = buffered_nextbyte(stream);
        if (ptr == (uintB*)NULL)
          goto eof;
        # angefangenes Byte holen:
        bit_akku = (*ptr)>>bitindex;
        # bitshift := 8-bitindex
        # Von bit_akku sind die Bits (bitshift-1)..0 gültig.
        if (count > 8) {
          # index incrementieren, da gerade *ptr verarbeitet:
          BufferedStream_index(stream) += 1;
          count -= 8; # Noch count (>0) Bits zu holen.
          var uintB* ptr = buffered_nextbyte(stream);
          if (ptr == (uintB*)NULL)
            goto eof1;
          # nächstes Byte holen:
          # (8-bitindex < 8, da sonst count = 0+bitsize < 8 gewesen wäre!)
          bit_akku |= (*ptr)<<(8-bitindex);
        }
        # Von bit_akku sind alle 8 Bits gültig.
        # 8 Bit abspeichern:
        TheSbvector(TheStream(stream)->strm_bitbuffer)->data[0] = bit_akku;
        BufferedStream_bitindex(stream) = count;
        # position incrementieren:
        BufferedStream_position(stream) += 1;
        # in Zahl umwandeln:
        return (*finisher)(stream,bitsize,1);
       eof1:
        # Wieder zurückpositionieren:
        position_file_i_buffered(stream,BufferedStream_position(stream));
      }
     eof: # EOF erreicht gewesen
      return eof_value;
    }

# UP für READ-BYTE auf File-Streams für Integers, Art c :
# Füllt den Bitbuffer mit den nächsten bitsize Bits.
# > stream : File-Stream für Integers, Art c
# > finisher : Beendigungsroutine
# < ergebnis : gelesener Integer oder eof_value
  local object rd_by_aux_icx_buffered (object stream, rd_by_ix_I* finisher);
  local object rd_by_aux_icx_buffered(stream,finisher)
    var object stream;
    var rd_by_ix_I* finisher;
    {
      var uintL bitsize = ChannelStream_bitsize(stream);
      var uintL bytesize = ceiling(bitsize,8);
      # genügend viele Bits in den Bitbuffer übertragen:
      var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_bitbuffer)->data[0];
      var uintL count = bitsize;
      var uintL bitshift = BufferedStream_bitindex(stream);
      var uintB* ptr = buffered_nextbyte(stream);
      if (ptr == (uintB*)NULL)
        goto eof;
      if (bitshift==0) {
        loop {
          *bitbufferptr++ = *ptr; # 8 Bits holen und abspeichern
          # index incrementieren, da gerade *ptr verarbeitet:
          BufferedStream_index(stream) += 1;
          count -= 8;
          # Noch count (>0) Bits zu holen.
          ptr = buffered_nextbyte(stream);
          if (ptr == (uintB*)NULL)
            goto eof;
          if (count<=8) # Sind damit count Bits fertig?
            break;
        }
        # Noch count = bitsize mod 8 (>0,<8) Bits zu holen.
        *bitbufferptr++ = *ptr; # count Bits holen und abspeichern
      } else { # 0<bitindex<8
        var uint16 bit_akku;
        # angefangenes Byte holen:
        bit_akku = (*ptr)>>bitshift;
        bitshift = 8-bitshift; # bitshift := 8-bitindex
        count -= bitshift;
        loop {
          # index incrementieren, da gerade *ptr verarbeitet:
          BufferedStream_index(stream) += 1;
          # Von bit_akku sind die Bits (bitshift-1)..0 gültig.
          # Noch count (>0) Bits zu holen.
          {
            var uintB* ptr = buffered_nextbyte(stream);
            if (ptr == (uintB*)NULL)
              goto eof;
            # nächstes Byte holen:
            bit_akku |= (uint16)(*ptr)<<bitshift;
          }
          # Von bit_akku sind die Bits (7+bitshift)..0 gültig.
          *bitbufferptr++ = (uint8)bit_akku; # 8 Bit abspeichern
          if (count<=8) # Sind damit count Bits fertig?
            break;
          count -= 8;
          bit_akku = bit_akku>>8;
        }
      }
      BufferedStream_bitindex(stream) = count;
      # position incrementieren:
      BufferedStream_position(stream) += 1;
      # in Zahl umwandeln:
      return (*finisher)(stream,bitsize,bytesize);
     eof: # EOF erreicht
      position_file_i_buffered(stream,BufferedStream_position(stream));
      return eof_value;
    }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art au :
  local object rd_by_iau_buffered (object stream);
  local object rd_by_iau_buffered(stream)
    var object stream;
    {
      return rd_by_aux_iax_buffered(stream,&rd_by_iu_I);
    }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art as :
  local object rd_by_ias_buffered (object stream);
  local object rd_by_ias_buffered(stream)
    var object stream;
    {
      return rd_by_aux_iax_buffered(stream,&rd_by_is_I);
    }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art bu :
  local object rd_by_ibu_buffered (object stream);
  local object rd_by_ibu_buffered(stream)
    var object stream;
    {
      return rd_by_aux_ibx_buffered(stream,&rd_by_iu_I);
    }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art bs :
  local object rd_by_ibs_buffered (object stream);
  local object rd_by_ibs_buffered(stream)
    var object stream;
    {
      return rd_by_aux_ibx_buffered(stream,&rd_by_is_I);
    }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art cu :
  local object rd_by_icu_buffered (object stream);
  local object rd_by_icu_buffered(stream)
    var object stream;
    {
      return rd_by_aux_icx_buffered(stream,&rd_by_iu_I);
    }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art cs :
  local object rd_by_ics_buffered (object stream);
  local object rd_by_ics_buffered(stream)
    var object stream;
    {
      return rd_by_aux_icx_buffered(stream,&rd_by_is_I);
    }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art au, bitsize = 8 :
  local object rd_by_iau8_buffered (object stream);
  local object rd_by_iau8_buffered(stream)
    var object stream;
    {
      var uintB* ptr = buffered_nextbyte(stream);
      if (!(ptr == (uintB*)NULL)) {
        var object obj = fixnum(*ptr);
        # index und position incrementieren:
        BufferedStream_index(stream) += 1;
        BufferedStream_position(stream) += 1;
        return obj;
      } else {
        return eof_value;
      }
    }

# READ-BYTE-SEQUENCE für File-Streams für Integers, Art au, bitsize = 8 :
  local uintL rd_by_array_iau8_buffered (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local uintL rd_by_array_iau8_buffered(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      var uintB* startptr = &TheSbvector(*bytearray_)->data[start];
      var uintB* endptr = read_byte_array_buffered(*stream_,startptr,len);
      var uintL result = endptr-startptr;
      # position incrementieren:
      BufferedStream_position(*stream_) += result;
      return result;
    }

# Output side
# -----------

# UP für WRITE-BYTE auf File-Streams für Integers, Art a :
# Schreibt den Bitbuffer-Inhalt aufs File.
  local void wr_by_aux_ia_buffered (object stream, uintL bitsize, uintL bytesize);
  local void wr_by_aux_ia_buffered(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    {
      var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_bitbuffer)->data[0];
      #if 0 # equivalent, but slow
      var uintL count;
      dotimespL(count,bytesize, {
        buffered_writebyte(stream,*bitbufferptr++);
      });
      #else
      write_byte_array_buffered(stream,bitbufferptr,bytesize);
      #endif
      # position incrementieren:
      BufferedStream_position(stream) += 1;
    }

# UP für WRITE-BYTE auf File-Streams für Integers, Art b :
# Schreibt den Bitbuffer-Inhalt aufs File.
  local void wr_by_aux_ib_buffered (object stream, uintL bitsize, uintL bytesize);
  local void wr_by_aux_ib_buffered(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    {
      var uintL bitshift = BufferedStream_bitindex(stream);
      var uint16 bit_akku = (uint16)(TheSbvector(TheStream(stream)->strm_bitbuffer)->data[0])<<bitshift;
      var uintL count = bitsize;
      var uintB* ptr = buffered_nextbyte(stream);
      # angefangenes Byte holen:
      if (!(ptr == (uintB*)NULL))
        bit_akku |= (*ptr)&(bit(bitshift)-1);
      count += bitshift;
      # evtl. einzelnes Byte schreiben:
      if (count>=8) {
        buffered_writebyte(stream,(uint8)bit_akku);
        bit_akku = bit_akku>>8;
        count -= 8;
      }
      # letztes Byte (count Bits) schreiben:
      if (!(count==0)) {
        ptr = buffered_nextbyte(stream);
        if (ptr == (uintB*)NULL) { # EOF ?
          ptr = buffered_eofbyte(stream); # 1 Byte Platz machen
          *ptr = (uint8)bit_akku; # Byte schreiben
        } else {
          # nächstes Byte nur teilweise überschreiben:
          var uint8 diff = (*ptr ^ (uint8)bit_akku) & (uint8)(bit(count)-1);
          if (diff == 0)
            goto no_modification;
          *ptr ^= diff;
        }
        BufferedStream_modified(stream) = TRUE;
       no_modification: ;
      }
      BufferedStream_bitindex(stream) = count;
      # position und evtl. eofposition incrementieren:
      if (BufferedStream_eofposition(stream) == BufferedStream_position(stream))
        BufferedStream_eofposition(stream) += 1;
      BufferedStream_position(stream) += 1;
    }

# UP für WRITE-BYTE auf File-Streams für Integers, Art c :
# Schreibt den Bitbuffer-Inhalt aufs File.
  local void wr_by_aux_ic_buffered (object stream, uintL bitsize, uintL bytesize);
  local void wr_by_aux_ic_buffered(stream,bitsize,bytesize)
    var object stream;
    var uintL bitsize;
    var uintL bytesize;
    {
      var uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_bitbuffer)->data[0];
      var uintL bitshift = BufferedStream_bitindex(stream);
      var uintL count = bitsize;
      var uint16 bit_akku;
      var uintB* ptr = buffered_nextbyte(stream);
      # angefangenes Byte holen:
      bit_akku = (ptr==(uintB*)NULL ? 0 : (*ptr)&(bit(bitshift)-1) );
      count += bitshift;
      # einzelne Bytes schreiben:
      loop {
        bit_akku |= (uint16)(*bitbufferptr++)<<bitshift;
        if (count<8)
          break;
        buffered_writebyte(stream,(uint8)bit_akku);
        bit_akku = bit_akku>>8;
        count -= 8;
        if (count<=bitshift)
          break;
      }
      # letztes Byte (count Bits) schreiben:
      if (!(count==0)) {
        ptr = buffered_nextbyte(stream);
        if (ptr == (uintB*)NULL) { # EOF ?
          ptr = buffered_eofbyte(stream); # 1 Byte Platz machen
          *ptr = (uint8)bit_akku; # Byte schreiben
        } else {
          # nächstes Byte nur teilweise überschreiben:
          var uint8 diff = (*ptr ^ (uint8)bit_akku) & (uint8)(bit(count)-1);
          if (diff == 0)
            goto no_modification;
          *ptr ^= diff;
        }
        BufferedStream_modified(stream) = TRUE;
       no_modification: ;
      }
      BufferedStream_bitindex(stream) = count;
      # position incrementieren:
      BufferedStream_position(stream) += 1;
    }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art au :
  local void wr_by_iau_buffered (object stream, object obj);
  local void wr_by_iau_buffered(stream,obj)
    var object stream;
    var object obj;
    {
      wr_by_ixu_sub(stream,obj,&wr_by_aux_ia_buffered);
    }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art as :
  local void wr_by_ias_buffered (object stream, object obj);
  local void wr_by_ias_buffered(stream,obj)
    var object stream;
    var object obj;
    {
      wr_by_ixs_sub(stream,obj,&wr_by_aux_ia_buffered);
    }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art bu :
  local void wr_by_ibu_buffered (object stream, object obj);
  local void wr_by_ibu_buffered(stream,obj)
    var object stream;
    var object obj;
    {
      wr_by_ixu_sub(stream,obj,&wr_by_aux_ib_buffered);
    }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art bs :
  local void wr_by_ibs_buffered (object stream, object obj);
  local void wr_by_ibs_buffered(stream,obj)
    var object stream;
    var object obj;
    {
      wr_by_ixs_sub(stream,obj,&wr_by_aux_ib_buffered);
    }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art cu :
  local void wr_by_icu_buffered (object stream, object obj);
  local void wr_by_icu_buffered(stream,obj)
    var object stream;
    var object obj;
    {
      wr_by_ixu_sub(stream,obj,&wr_by_aux_ic_buffered);
    }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art cs :
  local void wr_by_ics_buffered (object stream, object obj);
  local void wr_by_ics_buffered(stream,obj)
    var object stream;
    var object obj;
    {
      wr_by_ixs_sub(stream,obj,&wr_by_aux_ic_buffered);
    }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art au, bitsize = 8 :
  local void wr_by_iau8_buffered (object stream, object obj);
  local void wr_by_iau8_buffered(stream,obj)
    var object stream;
    var object obj;
    {
      # obj überprüfen:
      if (!integerp(obj))
        fehler_wr_integer(stream,obj);
      if (!(posfixnump(obj) && (posfixnum_to_L(obj) < bit(8))))
        fehler_bad_integer(stream,obj);
      write_byte_buffered(stream,posfixnum_to_L(obj));
    }

# WRITE-BYTE-SEQUENCE für File-Streams für Integers, Art au, bitsize = 8 :
  local void wr_by_array_iau8_buffered (const object* stream_, const object* bytearray_, uintL start, uintL len);
  local void wr_by_array_iau8_buffered(stream_,bytearray_,start,len)
    var const object* stream_;
    var const object* bytearray_;
    var uintL start;
    var uintL len;
    {
      write_byte_array_buffered(*stream_,&TheSbvector(*bytearray_)->data[start],len);
      # position incrementieren:
      BufferedStream_position(*stream_) += len;
    }

# File-Stream allgemein
# =====================

# UP: Positioniert einen (offenen) File-Stream an den Anfang.
# logical_position_file_start(stream);
# > stream : (offener) File-Stream.
# verändert in stream: index, eofindex, buffstart, ..., position, rd_ch_last
  local void logical_position_file_start (object stream);
  local void logical_position_file_start(stream)
    var object stream;
    {
      var uintL bitsize = ChannelStream_bitsize(stream);
      position_file_buffered(stream,
                             bitsize > 0 && bitsize < 8 # Integer-Stream vom Typ b ?
                             ? sizeof(uintL) : 0 # ja -> Position 4, sonst Position 0
                            );
      if (!((bitsize % 8) == 0))
        # Integer-Stream der Art b,c
        BufferedStream_bitindex(stream) = 0; # bitindex := 0
      BufferedStream_position(stream) = 0; # position := 0
      TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
      TheStream(stream)->strmflags &= ~strmflags_unread_B;
    }

# UP: Positioniert einen (offenen) File-Stream an eine gegebene Position.
# logical_position_file(stream,position);
# > stream : (offener) File-Stream.
# > position : neue (logische) Position
# verändert in stream: index, eofindex, buffstart, ..., position, rd_ch_last
  local void logical_position_file (object stream, uintL position);
  local void logical_position_file(stream,position)
    var object stream;
    var uintL position;
    {
      var uintL bitsize = ChannelStream_bitsize(stream);
      if (bitsize > 0) { # Integer-Stream ?
        if ((bitsize % 8) == 0) {
          # Art a
          position_file_buffered(stream,position*(bitsize/8));
        } else {
          # Art b,c
          position_file_i_buffered(stream,position);
        }
      } else {
        # Character-Stream
        position_file_buffered(stream,position);
        TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
        TheStream(stream)->strmflags &= ~strmflags_unread_B;
      }
      BufferedStream_position(stream) = position;
    }

# UP: Positioniert einen (offenen) File-Stream ans Ende.
# logical_position_file_end(stream);
# > stream : (offener) File-Stream.
# verändert in stream: index, eofindex, buffstart, ..., position, rd_ch_last
  local void logical_position_file_end (object stream);
  local void logical_position_file_end(stream)
    var object stream;
    {
      # evtl. Buffer hinausschreiben:
      if (BufferedStream_modified(stream))
        buffered_flush(stream);
      var uintL eofbytes; # EOF-Position, gemessen in Bytes
      # ans Ende positionieren:
      begin_system_call();
      handle_lseek(stream,TheStream(stream)->strm_buffered_channel,
                   0,SEEK_END,eofbytes=);
      end_system_call();
      # logische Position berechnen und eofbytes korrigieren:
      var uintL position; # logische Position
      var uintL eofbits = 0; # Bit-Ergänzung zu eofbytes
      var uintL bitsize = ChannelStream_bitsize(stream);
      if (bitsize > 0) { # Integer-Stream ?
        if ((bitsize % 8) == 0) {
          # Art a
          var uintL bytesize = bitsize/8;
          position = floor(eofbytes,bytesize);
          eofbytes = position*bytesize;
        } elif (bitsize < 8) {
          # Art b
          eofbytes -= sizeof(uintL); # Header berücksichtigen
          # Ist die gemerkte EOF-Position plausibel?
          position = BufferedStream_eofposition(stream);
          if (!(ceiling(position*bitsize,8)==eofbytes)) # ja -> verwende sie
            position = floor(eofbytes*8,bitsize); # nein -> rechne sie neu aus
          # Rechne eofbytes und eofbits neu aus:
          eofbytes = floor(position*bitsize,8);
          eofbits = (position*bitsize)%8;
          eofbytes += sizeof(uintL); # Header berücksichtigen
        } else {
          # Art c
          position = floor(eofbytes*8,bitsize);
          eofbytes = floor(position*bitsize,8);
          eofbits = (position*bitsize)%8;
        }
      } else {
        # Character-Stream
        position = eofbytes;
      }
      if (!BufferedStream_blockpositioning(stream)) {
        # Jetzt am Ende positioniert:
        BufferedStream_buffstart(stream) = eofbytes;
        BufferedStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
        BufferedStream_index(stream) = 0; # index := 0
        BufferedStream_modified(stream) = FALSE; # unmodifiziert
      } else {
        # auf den Anfang des letzten Sectors positionieren:
        {
          var uintL buffstart;
          begin_system_call();
          handle_lseek(stream,TheStream(stream)->strm_buffered_channel,
                       floor(eofbytes,strm_buffered_bufflen)*strm_buffered_bufflen,SEEK_SET,buffstart=);
          end_system_call();
          BufferedStream_buffstart(stream) = buffstart;
        }
        # Sector lesen:
        BufferedStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
        BufferedStream_index(stream) = 0; # index := 0
        BufferedStream_modified(stream) = FALSE; # unmodifiziert
        var uintL eofindex = eofbytes % strm_buffered_bufflen;
        if (!((eofindex==0) && (eofbits==0))) { # EOF am Sectorende -> brauche nichts zu lesen
          buffered_nextbyte(stream);
          # Jetzt ist index=0. index und eofindex setzen:
          BufferedStream_index(stream) = eofindex;
          if (!(eofbits==0))
            eofindex += 1;
          BufferedStream_eofindex(stream) = eofindex;
        }
      }
      if (!((bitsize % 8) == 0)) {
        # Integer-Stream der Art b,c
        BufferedStream_bitindex(stream) = eofbits;
      }
      # position setzen:
      BufferedStream_position(stream) = position;
      TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
      TheStream(stream)->strmflags &= ~strmflags_unread_B;
    }

# UP: Fills in the pseudofunctions for a buffered stream.
# fill_pseudofuns_buffered(stream,&eltype);
# > stream: stream being built up, with correct strmflags and encoding
# > eltype: Element-Type in decoded form
  local void fill_pseudofuns_buffered (object stream, const decoded_eltype* eltype);
  local void fill_pseudofuns_buffered(stream,eltype)
    var object stream;
    var const decoded_eltype* eltype;
    {
      var uintB flags = TheStream(stream)->strmflags;
      switch (eltype->kind) {
        case eltype_ch:
          TheStream(stream)->strm_rd_ch = P(rd_ch_buffered);
          TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
          TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_buffered);
          {
            var object eol = TheEncoding(TheStream(stream)->strm_encoding)->enc_eol;
            if (eq(eol,S(Kunix))) {
              TheStream(stream)->strm_wr_ch = P(wr_ch_buffered_unix);
              TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_buffered_unix);
            } elif (eq(eol,S(Kmac))) {
              TheStream(stream)->strm_wr_ch = P(wr_ch_buffered_mac);
              TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_buffered_mac);
            } elif (eq(eol,S(Kdos))) {
              TheStream(stream)->strm_wr_ch = P(wr_ch_buffered_dos);
              TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_buffered_dos);
            } else {
              NOTREACHED;
            }
          }
          break;
        case eltype_iu:
          TheStream(stream)->strm_rd_by =
            ((eltype->size % 8) == 0
             ? (eltype->size == 8 ? P(rd_by_iau8_buffered) : P(rd_by_iau_buffered))
             : eltype->size < 8 ? P(rd_by_ibu_buffered) : P(rd_by_icu_buffered)
            );
          TheStream(stream)->strm_rd_by_array =
            (eltype->size == 8
             ? P(rd_by_array_iau8_buffered)
             : P(rd_by_array_dummy)
            );
          TheStream(stream)->strm_wr_by =
            ((eltype->size % 8) == 0
             ? (eltype->size == 8 ? P(wr_by_iau8_buffered) : P(wr_by_iau_buffered))
             : eltype->size < 8 ? P(wr_by_ibu_buffered) : P(wr_by_icu_buffered)
            );
          TheStream(stream)->strm_wr_by_array =
            (eltype->size == 8
             ? P(wr_by_array_iau8_buffered)
             : P(wr_by_array_dummy)
            );
          break;
        case eltype_is:
          TheStream(stream)->strm_rd_by =
            ((eltype->size % 8) == 0 ? P(rd_by_ias_buffered) :
             eltype->size < 8 ? P(rd_by_ibs_buffered) :
                                P(rd_by_ics_buffered)
            );
          TheStream(stream)->strm_rd_by_array = P(rd_by_array_dummy);
          TheStream(stream)->strm_wr_by =
            ((eltype->size % 8) == 0 ? P(wr_by_ias_buffered) :
             eltype->size < 8 ? P(wr_by_ibs_buffered) :
                                P(wr_by_ics_buffered)
            );
          TheStream(stream)->strm_wr_by_array = P(wr_by_array_dummy);
          break;
        default: NOTREACHED
      }
      # Default für READ-BYTE-Pseudofunktion:
      if ((flags & strmflags_rd_by_B)==0) {
        TheStream(stream)->strm_rd_by = P(rd_by_error);
        TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
      }
      # Default für WRITE-BYTE-Pseudofunktion:
      if ((flags & strmflags_wr_by_B)==0) {
        TheStream(stream)->strm_wr_by = P(wr_by_error);
        TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
      }
      # Default für READ-CHAR-Pseudofunktion:
      if ((flags & strmflags_rd_ch_B)==0) {
        TheStream(stream)->strm_rd_ch = P(rd_ch_error);
        TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
        TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_error);
      }
      # Default für WRITE-CHAR-Pseudofunktion:
      if ((flags & strmflags_wr_ch_B)==0) {
        TheStream(stream)->strm_wr_ch = P(wr_ch_error);
        TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_error);
      }
    }

# UP: creates a buffered file stream
# make_buffered_stream(type,direction,&eltype,handle_regular,handle_blockpositioning)
# > STACK_2: Encoding
# > STACK_1: Element-Type
# > STACK_0: open file handle
# > type: stream type
# > direction: mode (0 = :PROBE, 1 = :INPUT, 4 = :OUTPUT, 5 = :IO, 3 = :INPUT-IMMUTABLE)
# > eltype: Element-Type in decoded form
# > handle_regular: whether the handle refers to a regular file
# > handle_blockpositioning: whether the handle refers to a regular file which
#                            can be positioned at n*strm_buffered_bufflen
# If direction==5, handle_blockpositioning must be TRUE.
# < result: buffered file stream, Handle_{input,output}_init still to be called,
#           for eltype.size<8 also eofposition still to be to determined
# < STACK: cleaned up
# can trigger GC
  local object make_buffered_stream (uintB type, uintB direction, const decoded_eltype* eltype, boolean handle_regular, boolean handle_blockpositioning);
  local object make_buffered_stream(type,direction,eltype,handle_regular,handle_blockpositioning)
    var uintB type;
    var uintB direction;
    var const decoded_eltype* eltype;
    var boolean handle_regular;
    var boolean handle_blockpositioning;
    {
      var uintB flags =
          ((direction & bit(0)) ? strmflags_rd_B : 0) # evtl. READ-CHAR, READ-BYTE erlaubt
        | ((direction & bit(2)) ? strmflags_wr_B : 0) # evtl. WRITE-CHAR, WRITE-BYTE erlaubt
        | ((direction & bit(1)) ? strmflags_immut_B : 0) # evtl. immutable Objekte
        ;
      var uintC xlen = sizeof(strm_buffered_extrafields_struct); # Das haben alle File-Streams
      if (eltype->kind == eltype_ch) {
        flags &= strmflags_ch_B | strmflags_immut_B;
      } else {
        flags &= strmflags_by_B | strmflags_immut_B;
        if ((eltype->size % 8) == 0) {
          # Art a
        } else {
          xlen = sizeof(strm_i_buffered_extrafields_struct); # Das haben die File-Streams für Integers maximal
        }
      }
      # Stream allozieren:
      var object stream = allocate_stream(flags,type,strm_channel_len,xlen);
      # und füllen:
      TheStream(stream)->strm_encoding = STACK_2;
      fill_pseudofuns_buffered(stream,eltype);
      TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      # Komponenten von File-Streams:
      {
        var object handle = popSTACK(); # Handle zurück
        TheStream(stream)->strm_eltype = popSTACK(); # Element-Type eintragen
        ChannelStream_buffered(stream) = TRUE;
        ChannelStream_init(stream);
        if (!nullp(handle)) { # Handle=NIL -> Rest bereits mit NIL initialisiert, fertig
          TheStream(stream)->strm_buffered_channel = handle; # Handle eintragen
          BufferedStream_regular(stream) = handle_regular;
          BufferedStream_blockpositioning(stream) = handle_blockpositioning;
          BufferedStream_buffstart(stream) = 0; # buffstart := 0
          # Buffer allozieren:
          pushSTACK(stream);
          {
            var object buffer = allocate_bit_vector(Atype_8Bit,strm_buffered_bufflen);
            stream = popSTACK();
            TheStream(stream)->strm_buffered_buffer = buffer;
          }
          BufferedStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
          BufferedStream_index(stream) = 0; # index := 0
          BufferedStream_modified(stream) = FALSE; # Buffer unmodifiziert
          BufferedStream_position(stream) = 0; # position := 0
          ChannelStream_bitsize(stream) = eltype->size;
          ChannelStream_lineno(stream) = 1; # initialize always (cf. set-stream-element-type)
          if (!(eltype->kind == eltype_ch)) {
            # File-Stream für Integers
            # Bitbuffer allozieren:
            pushSTACK(stream);
            {
              var object bitbuffer = allocate_bit_vector(Atype_Bit,ceiling(eltype->size,8)*8);
              stream = popSTACK();
              TheStream(stream)->strm_bitbuffer = bitbuffer;
            }
            if (!((eltype->size % 8) == 0)) {
              # Arten b,c
              BufferedStream_bitindex(stream) = 0; # bitindex := 0
            }
          }
        }
      }
      skipSTACK(1);
      return stream;
    }

# UP: erzeugt ein File-Stream
# make_file_stream(direction,append_flag,handle_fresh)
# > STACK_5: Filename, ein Pathname oder NIL
# > STACK_4: Truename, ein Pathname oder NIL
# > STACK_3: :BUFFERED argument
# > STACK_2: :EXTERNAL-FORMAT argument
# > STACK_1: :ELEMENT-TYPE argument
# > STACK_0: Handle des geöffneten Files
# > direction: Modus (0 = :PROBE, 1 = :INPUT, 4 = :OUTPUT, 5 = :IO, 3 = :INPUT-IMMUTABLE)
# > append_flag: TRUE falls der Stream gleich ans Ende positioniert werden
#         soll, FALSE sonst
# > handle_fresh: whether the handle is freshly created.
#                 This means 1. that it is currently positioned at position 0,
#                 2. if (direction & bit(2)), it is opened for read/write, not
#                 only for write.
#                 If the handle refers to a regular file, this together means
#                 that it supports handle_lseek, reading/repositioning/writing
#                 and close/reopen.
# > subr_self: calling function
# If direction==5, handle_fresh must be TRUE.
# < ergebnis: File-Stream (oder evtl. File-Handle-Stream)
# < STACK: aufgeräumt
# can trigger GC
  global object make_file_stream (uintB direction, boolean append_flag, boolean handle_fresh);
  global object make_file_stream(direction,append_flag,handle_fresh)
    var uintB direction;
    var boolean append_flag;
    var boolean handle_fresh;
    {
      var decoded_eltype eltype;
      var signean buffered;
      # Check and canonicalize the :ELEMENT-TYPE argument:
      test_eltype_arg(&STACK_1,&eltype);
      STACK_1 = canon_eltype(&eltype);
      # Check and canonicalize the :EXTERNAL-FORMAT argument:
      STACK_2 = test_external_format_arg(STACK_2);
      # Stackaufbau: filename, truename, buffered, encoding, eltype, handle.
      var object stream;
      var object handle = STACK_0;
      var boolean handle_regular = TRUE;
      if (!nullp(handle))
        handle_regular = regular_handle_p(TheHandle(handle));
      # Check and canonicalize the :BUFFERED argument:
      # Default is T for regular files, NIL for non-regular files because they
      # probably don't support lseek().
      buffered = test_buffered_arg(STACK_3);
      if (buffered == 0)
        buffered = (handle_regular ? 1 : -1);
      if (buffered < 0) {
        if (!(eltype.kind == eltype_ch) && !((eltype.size % 8) == 0)) {
          pushSTACK(STACK_4); # Truename, Wert für Slot PATHNAME von FILE-ERROR
          pushSTACK(STACK_0);
          pushSTACK(STACK_(1+2));
          pushSTACK(S(Kelement_type));
          pushSTACK(TheSubr(subr_self)->name);
          fehler(file_error,
                 GETTEXT("~: argument ~ ~ was specified, but ~ is not a regular file.")
                );
        }
        var boolean handle_tty = FALSE;
        if (direction & bit(0)) # only needed for input handles
          if (!handle_regular) { # regular files are certainly not ttys
            begin_system_call();
            handle_tty = isatty(TheHandle(handle));
            end_system_call();
          }
        stream = make_unbuffered_stream(strmtype_file,direction,&eltype,handle_tty);
        # File-Handle-Streams werden für Pathname-Zwecke wie File-Streams behandelt.
        # Daher ist (vgl. file_write_date) strm_buffered_channel == strm_ochannel,
        # und wir tragen nun die Pathnames ein:
        TheStream(stream)->strm_file_truename = STACK_1; # Truename eintragen
        TheStream(stream)->strm_file_name = STACK_2; # Filename eintragen
        if (direction & bit(0)) {
          UnbufferedHandleStream_input_init(stream);
        }
        if (direction & bit(2)) {
          UnbufferedHandleStream_output_init(stream);
        }
        ChannelStreamLow_close(stream) = &low_close_handle;
      } else {
        if (direction==5 && !handle_regular) {
          # FIXME: Instead of signalling an error, we could return some kind
          # of two-way-stream (cf. make_socket_stream).
          pushSTACK(STACK_4); # Truename, Wert für Slot PATHNAME von FILE-ERROR
          pushSTACK(STACK_0);
          pushSTACK(T);
          pushSTACK(S(Kbuffered));
          pushSTACK(S(Kio));
          pushSTACK(S(Kdirection));
          pushSTACK(TheSubr(subr_self)->name);
          fehler(file_error,
                 GETTEXT("~: arguments ~ ~ and ~ ~ were specified, but ~ is not a regular file.")
                );
        }
        # Positioning the buffer on block boundaries is possible only if
        # 1. the handle refers to a regular file (otherwise read() and
        #    write() on the handle may be unrelated),
        # 2. if write access is requested, the handle is known to have
        #    read access as well (O_RDWR vs. O_WRONLY).
        var boolean handle_blockpositioning =
          (handle_regular && (direction & bit(2) ? handle_fresh : TRUE));
        # Now, if direction==5, handle_blockpositioning is TRUE.
        # Stream allozieren:
        stream = make_buffered_stream(strmtype_file,direction,&eltype,
                                      handle_regular,handle_blockpositioning);
        TheStream(stream)->strm_file_truename = STACK_1; # Truename eintragen
        TheStream(stream)->strm_file_name = STACK_2; # Filename eintragen
        BufferedHandleStream_init(stream);
        ChannelStreamLow_close(stream) = &low_close_handle;
        if (handle_regular && !handle_fresh) {
          var uintL position;
          begin_system_call();
          handle_lseek(stream,TheStream(stream)->strm_buffered_channel,
                       0,SEEK_CUR,position=);
          end_system_call();
          position_file_buffered(stream,position);
        }
        if (!nullp(TheStream(stream)->strm_buffered_channel)
            && !(eltype.kind == eltype_ch)
            && (eltype.size < 8)
           ) {
          # Art b
          # eofposition lesen:
          var uintL eofposition = 0;
          var uintC count;
          for (count=0; count < 8*sizeof(uintL); count += 8 ) {
            var uintB* ptr = buffered_nextbyte(stream);
            if (ptr == (uintB*)NULL)
              goto too_short;
            eofposition |= ((*ptr) << count);
            # index incrementieren, da gerade *ptr verarbeitet:
            BufferedStream_index(stream) += 1;
          }
          if (FALSE) {
           too_short:
            # File zu kurz (< sizeof(uintL) Bytes)
            if ((TheStream(stream)->strmflags & strmflags_wr_by_B) == 0) # Read-Only-Stream?
              goto bad_eofposition;
            # File Read/Write -> setze eofposition := 0
            eofposition = 0;
            position_file_buffered(stream,0); # an Position 0 positionieren
            var uintC count; # und eofposition = 0 herausschreiben
            dotimespC(count,sizeof(uintL), { buffered_writebyte(stream,0); } );
          } elif (eofposition > (uintL)(bitm(oint_data_len)-1)) {
           bad_eofposition:
            # Keine gültige EOF-Position.
            # File schließen und Error melden:
            TheStream(stream)->strmflags &= ~strmflags_wr_by_B; # Stream Read-Only machen
            pushSTACK(stream);
            builtin_stream_close(&STACK_0);
            # STACK_0 = Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(!nullp(TheStream(STACK_0)->strm_file_truename) ? TheStream(STACK_0)->strm_file_truename : STACK_0);
            fehler(stream_error,
                   GETTEXT("file ~ is not an integer file")
                  );
          }
          # Auf die gelesene EOF-Position verlassen wir uns jetzt!
          BufferedStream_eofposition(stream) = eofposition;
        }
      }
      skipSTACK(3);
      # Liste der offenen File-Streams um stream erweitern:
      pushSTACK(stream);
      {
        var object new_cons = allocate_cons();
        Car(new_cons) = stream = popSTACK();
        Cdr(new_cons) = O(open_files);
        O(open_files) = new_cons;
      }
      # Modus :APPEND behandeln:
      # CLHS says that :APPEND implies that "the file pointer is _initially_
      # positioned at the end of the file". Note that this is different from
      # the Unix O_APPEND semantics.
      if (append_flag) {
        if (buffered < 0) {
          # ans Ende positionieren:
          begin_system_call();
          handle_lseek(stream,TheStream(stream)->strm_ochannel,
                       0,SEEK_END,);
          end_system_call();
        } else {
          logical_position_file_end(stream);
        }
      }
      # Done.
      return stream;
    }

# UP: Bereitet das Schließen eines File-Streams vor.
# Dabei wird der Buffer und evtl. eofposition hinausgeschrieben.
# buffered_flush_everything(stream);
# > stream : (offener) File-Stream.
# verändert in stream: index, eofindex, buffstart, ...
  local void buffered_flush_everything (object stream);
  local void buffered_flush_everything(stream)
    var object stream;
    {
      # Bei Integer-Streams (Art b) eofposition abspeichern:
      if (ChannelStream_bitsize(stream) > 0 && ChannelStream_bitsize(stream) < 8)
        if (TheStream(stream)->strmflags & strmflags_wr_by_B) { # nur falls nicht Read-Only
          position_file_buffered(stream,0); # an Position 0 positionieren
          var uintL eofposition = BufferedStream_eofposition(stream);
          var uintC count;
          dotimespC(count,sizeof(uintL), {
            buffered_writebyte(stream,(uintB)eofposition);
            eofposition = eofposition>>8;
          });
        }
      # evtl. Buffer hinausschreiben:
      if (BufferedStream_modified(stream))
        buffered_flush(stream);
      # Nun ist das modified_flag gelöscht.
    }

# UP: Bringt den wartenden Output eines File-Stream ans Ziel.
# Schreibt dazu den Buffer des File-Streams (auch physikalisch) aufs File.
# finish_output_buffered(stream);
# > stream : File-Stream.
# verändert in stream: handle, index, eofindex, buffstart, ..., rd_ch_last
# can trigger GC
  local void finish_output_buffered (object stream);
  local void finish_output_buffered(stream)
    var object stream;
    {
      # Handle=NIL (Stream bereits geschlossen) -> fertig:
      if (nullp(TheStream(stream)->strm_buffered_channel))
        return;
      # kein File mit Schreibzugriff -> gar nichts zu tun:
      if (!(TheStream(stream)->strmflags & strmflags_wr_B))
        return;
      # Wartenden Output im iconv-Deskriptor hinausschreiben:
      oconv_unshift_output_buffered(stream);
      # evtl. Buffer und evtl. eofposition hinausschreiben:
      buffered_flush_everything(stream);
      # Nun ist das modified_flag gelöscht.
      if (BufferedStream_regular(stream)) {
        #ifdef UNIX
          #ifdef HAVE_FSYNC
            begin_system_call();
            if (!( fsync(TheHandle(TheStream(stream)->strm_buffered_channel)) ==0)) {
              end_system_call();
              OS_filestream_error(stream);
            }
            end_system_call();
          #endif
        #else
          if (!nullp(TheStream(stream)->strm_file_truename)) { # avoid closing stdout_handle
            #ifdef MSDOS
              # File-Handle duplizieren und schließen:
              var uintW handle = TheHandle(TheStream(stream)->strm_buffered_channel);
              begin_system_call();
              var sintW handle2 = dup(handle);
              if (handle2 < 0) {
                end_system_call(); OS_filestream_error(stream); # Error melden
              }
              if ( CLOSE(handle2) <0) {
                end_system_call(); OS_filestream_error(stream);
              }
              end_system_call();
            #endif
            #ifdef RISCOS # || MSDOS, wenn wir da nicht schon was besseres hätten
              # File schließen (DOS schreibt physikalisch):
              begin_system_call();
              if ( CLOSE(TheHandle(TheStream(stream)->strm_buffered_channel)) <0) {
                end_system_call(); OS_filestream_error(stream);
              }
              end_system_call();
              # File neu öffnen:
              pushSTACK(stream); # stream retten
              pushSTACK(TheStream(stream)->strm_file_truename); # Filename
              # Directory existiert schon:
              var object namestring = assume_dir_exists(); # Filename als ASCIZ-String
              var sintW handle;
              with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
                begin_system_call();
                handle = OPEN(namestring_asciz,O_RDWR); # Datei neu öffnen
                if (handle < 0) { # Error melden
                  end_system_call(); OS_filestream_error(STACK_1);
                }
                end_system_call();
              });
              #ifdef MSDOS
              begin_system_call();
              setmode(handle,O_BINARY);
              end_system_call();
              #endif
              # Nun enthält handle das Handle des geöffneten Files.
              var object handlobj = allocate_handle(handle);
              skipSTACK(1);
              stream = popSTACK(); # stream zurück
              # neues Handle eintragen:
              TheStream(stream)->strm_buffered_channel = handlobj;
            #endif
            #ifdef AMIGAOS
              #if 0 # Manche Devices vertragen es nicht, wenn man geöffnete Dateien
                    # zu- und wieder aufmacht. Z.B. bei Pipes hat das eine besondere
                    # Bedeutung.
              begin_system_call();
              var Handle handle = TheHandle(TheStream(stream)->strm_buffered_channel);
              if (!IsInteractive(handle)) {
                # File schließen (OS schreibt physikalisch):
                Close(handle);
                end_system_call();
                # File neu öffnen:
                pushSTACK(stream); # stream retten
                pushSTACK(TheStream(stream)->strm_file_truename); # Filename
                # Directory existiert schon, Datei neu öffnen:
                var object namestring = assume_dir_exists(); # Filename als ASCIZ-String
                with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
                  begin_system_call();
                  handle = Open(namestring_asciz,MODE_OLDFILE);
                  end_system_call();
                });
                if (handle==NULL) { # Error melden
                  OS_filestream_error(STACK_1);
                }
                skipSTACK(1);
                stream = popSTACK(); # stream zurück
                # neues Handle eintragen:
                TheHandle(TheStream(stream)->strm_buffered_channel) = handle;
              } else {
                end_system_call();
              }
              #endif
            #endif
          }
        #endif
      }
      # und neu positionieren:
      var uintL position = BufferedStream_buffstart(stream) + BufferedStream_index(stream);
      BufferedStream_index(stream) = 0; # index := 0
      BufferedStream_eofindex(stream) = eofindex_all_invalid; # eofindex := all_invalid
      if (!BufferedStream_blockpositioning(stream)) {
        BufferedStream_buffstart(stream) = position;
      } else {
        BufferedStream_buffstart(stream) = 0; # buffstart := 0
        position_file_buffered(stream,position);
      }
      # Komponenten position, ..., lastchar bleiben unverändert
    }

# UP: Bringt den wartenden Output eines File-Stream ans Ziel.
# Schreibt dazu den Buffer des File-Streams (auch physikalisch) aufs File.
# force_output_buffered(stream);
# > stream : File-Stream.
# verändert in stream: handle, index, eofindex, buffstart, ..., rd_ch_last
# can trigger GC
  #define force_output_buffered  finish_output_buffered

# UP: Erklärt einen File-Stream für geschlossen.
# closed_buffered(stream);
# > stream : (offener) File-Stream.
# verändert in stream: alle Komponenten außer name und truename
  local void closed_buffered (object stream);
  local void closed_buffered(stream)
    var object stream;
    {
      TheStream(stream)->strm_buffered_channel = NIL; # Handle wird ungültig
      TheStream(stream)->strm_buffered_buffer = NIL; # Buffer freimachen
      BufferedStream_buffstart(stream) = 0; # buffstart löschen (unnötig)
      BufferedStream_eofindex(stream) = eofindex_all_invalid; # eofindex löschen (unnötig)
      BufferedStream_index(stream) = 0; # index löschen (unnötig)
      BufferedStream_modified(stream) = FALSE; # modified_flag löschen (unnötig)
      BufferedStream_position(stream) = 0; # position löschen (unnötig)
      if (ChannelStream_bitsize(stream) > 0) {
        ChannelStream_bitsize(stream) = 0; # bitsize löschen
        TheStream(stream)->strm_bitbuffer = NIL; # Bitbuffer freimachen
      }
      #if defined(UNICODE) && (defined(GNU_LIBICONV) || defined(HAVE_ICONV))
      ChannelStream_iconvdesc(stream) = (iconv_t)0; # iconvdesc löschen
      ChannelStream_oconvdesc(stream) = (iconv_t)0; # oconvdesc löschen
      #endif
    }

# UP: Schließt einen File-Stream.
# close_buffered(stream);
# > stream : File-Stream.
# verändert in stream: alle Komponenten außer name und truename
  local void close_buffered (object stream);
  local void close_buffered(stream)
    var object stream;
    {
      # Handle=NIL (Stream bereits geschlossen) -> fertig:
      if (nullp(TheStream(stream)->strm_buffered_channel))
        return;
      # Wartenden Output im iconv-Deskriptor hinausschreiben:
      oconv_unshift_output_buffered(stream);
      # evtl. Buffer und evtl. eofposition hinausschreiben:
      buffered_flush_everything(stream);
      # Nun ist das modified_flag gelöscht.
      # File schließen:
      ChannelStreamLow_close(stream)(stream,TheStream(stream)->strm_buffered_channel);
      ChannelStream_fini(stream);
      # Komponenten ungültig machen (close_dummys kommt später):
      closed_buffered(stream);
      # stream aus der Liste aller offenen File-Streams streichen:
      O(open_files) = deleteq(O(open_files),stream);
    }

LISPFUNN(file_stream_p,1)
# (SYS::FILE-STREAM-P stream) == (TYPEP stream 'FILE-STREAM)
  {
    var object arg = popSTACK();
    if (builtin_stream_p(arg) && (TheStream(arg)->strmtype == strmtype_file))
      value1 = T;
    else
      value1 = NIL;
    mv_count=1;
  }


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
  #define strm_keyboard_handle  strm_ichannel  # Handle für listen_unbuffered()
  #define strm_keyboard_buffer  strm_field1   # Liste der noch zu liefernden Zeichen
  #define strm_keyboard_keytab  strm_field2   # Liste aller Tastenzuordnungen
                                              # jeweils (char1 ... charn . result)
  #define strm_keyboard_len  strm_channel_len
  #define strm_keyboard_xlen  sizeof(strm_unbuffered_extrafields_struct)
#elif defined(WIN32_NATIVE)
  # Zusätzliche Komponenten:
  #define strm_keyboard_isatty  strm_isatty   # Flag, ob stdin ein Terminal ist
  #define strm_keyboard_handle  strm_ichannel  # Handle für listen_unbuffered()
  #define strm_keyboard_len  strm_channel_len
  #define strm_keyboard_xlen  sizeof(strm_unbuffered_extrafields_struct)
#else
  # Keine zusätzlichen Komponenten.
  #define strm_keyboard_len  strm_len
  #define strm_keyboard_xlen  0
#endif

# The keyboard events are instances of INPUT-CHARACTER. We create them by
# calling MAKE-INPUT-CHARACTER or MAKE-CHAR. The following structure describes
# the arguments to MAKE-INPUT-CHARACTER.
typedef struct {
  const char * key;
  chart code;
  uintB bits;
} key_event;

# Initializers for the two most common kinds of keyboard events.
  #define key_ascii(asc)  { NULL, ascii(asc), 0 }
  #define key_special(name)  { name, ascii(0), char_hyper_c }

# Creates a keyboard event.
local object make_key_event (const key_event* event);
local object make_key_event(event)
  var const key_event* event;
  {
    if ((event->key == NULL) && (event->bits == 0)) {
      pushSTACK(S(Kchar)); pushSTACK(code_char(event->code));
      funcall(S(make_input_character),2);
    } else {
      pushSTACK(S(Kkey));
      if (event->key == NULL)
        pushSTACK(code_char(event->code));
      else
        pushSTACK(intern_keyword(ascii_to_string(event->key)));
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
    {
      var union REGS in;
      var union REGS out;
      in.regB.ah = 0x11;
      int86(0x16,&in,&out);
      return ((out.reg_flags & 0x40) == 0); # Zero-Flag abfragen
    }
  local uintW getch()
    {
      var union REGS in;
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
    {
      var boolean result;
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
    {
      var uintW ch;
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
  local const key_event scancode_table [167] = {
    { NULL, 0, 0, },
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
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
  local signean listen_keyboard (object stream);
  #ifdef MSDOS
  local signean listen_keyboard(stream)
    var object stream;
    {
      #ifdef EMUNIX_PORTABEL
      if (!(_osmode == DOS_MODE)) {
        # OS/2
        var int ch = _read_kbd(FALSE,FALSE,FALSE);
        if (ch < 0)
          return ls_wait; # nein
        pushSTACK(stream);
        var object c;
        if (ch==0) {
          c = make_key_event(&scancode_table[(uintB)_read_kbd(FALSE,TRUE,FALSE)]);
        } elif ((ch <= 26) && !(ch == BS) && !(ch == CR) && !(ch == TAB)) {
          # Ctrl-A bis Ctrl-Z -> Buchstabe mit CONTROL-Bit draus machen:
          var key_event event;
          event.key = NULL;
          event.code = ascii(ch==LF ? CR : (ch | bit(6)));
          event.bits = char_control_c;
          c = make_key_event(&event);
        } else {
          pushSTACK(code_char(ch)); funcall(S(make_char),1); c = value1;
        }
        stream = popSTACK();
        TheStream(stream)->strm_rd_ch_last = c;
        TheStream(stream)->strmflags |= strmflags_unread_B;
        return ls_avail;
      } else
      #endif
      # DOS
      if (kbhit()) { # inzwischen wieder Tasten gedrückt?
        return ls_avail; # ja
      } else {
        return ls_wait; # nein
      }
    }
  #endif
  #ifdef WIN32_NATIVE
  local signean listen_keyboard(stream)
    var object stream;
    {
      var Handle handle = TheHandle(TheStream(stream)->strm_keyboard_handle);
      # See the implementation of listen_unbuffered() for consoles.
      var DWORD nevents;
      begin_system_call();
      if (!GetNumberOfConsoleInputEvents(handle,&nevents)) {
        OS_error();
      }
      # It's a console.
      if (nevents==0) {
        # kein Zeichen verfügbar
        end_system_call(); return ls_wait;
      }
      var INPUT_RECORD* events = (INPUT_RECORD*)alloca(nevents*sizeof(INPUT_RECORD));
      var DWORD nevents_read;
      if (!PeekConsoleInput(handle,events,nevents,&nevents_read)) {
        OS_error();
      }
      if (nevents_read==0) {
        # kein Zeichen verfügbar
        end_system_call(); return ls_wait;
      }
      {
        # Look out for any Key-Down event.
        var DWORD i;
        for (i = 0; i < nevents_read; i++) {
          if (events[i].EventType == KEY_EVENT
              && events[i].Event.KeyEvent.bKeyDown
              && events[i].Event.KeyEvent.uAsciiChar != 0) {
            # Zeichen verfügbar
            end_system_call(); return ls_avail;
          }
        }
      }
      # kein Zeichen verfügbar
      end_system_call(); return ls_wait;
    }
  #endif
  #if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)
    #define listen_keyboard  listen_unbuffered
  #endif
  #if defined(NEXTAPP)
    #define listen_keyboard(stream)  (stream, ls_eof)
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
        if (!(_osmode == DOS_MODE)) {
          # OS/2
          while (listen_keyboard(stream)) {
            # das Zeichen wurde schon geholt!
          }
        } else
        #endif
        # DOS
        while (kbhit()) {
          getch();
        }
      #endif
      #ifdef WIN32_NATIVE
        clear_tty_input(TheHandle(TheStream(stream)->strm_keyboard_handle));
        pushSTACK(stream);
        while (ls_avail_p(listen_keyboard(STACK_0))) {
          read_char(&STACK_0);
        }
        skipSTACK(1);
      #endif
      #if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)
        if (nullp(TheStream(stream)->strm_keyboard_isatty))
          # File -> nichts tun
          return FALSE;
        # Terminal
        TheStream(stream)->strm_rd_ch_last = NIL; # gewesenes EOF vergessen
        clear_tty_input(stdin_handle);
        pushSTACK(stream);
        while (ls_avail_p(listen_keyboard(STACK_0))) {
          read_char(&STACK_0);
        }
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
      if (!(_osmode == DOS_MODE)) {
        # OS/2
        run_time_stop(); # Run-Time-Stoppuhr anhalten
        var object c;
        var int ch = _read_kbd(FALSE,TRUE,FALSE);
        if (ch==0) {
          c = make_key_event(&scancode_table[(uintB)_read_kbd(FALSE,TRUE,FALSE)]);
        } elif ((ch <= 26) && !(ch == BS) && !(ch == CR) && !(ch == TAB)) {
          # Ctrl-A bis Ctrl-Z -> Buchstabe mit CONTROL-Bit draus machen:
          var key_event event;
          event.key = NULL;
          event.code = ascii(ch==LF ? CR : (ch | bit(6)));
          event.bits = char_control_c;
          c = make_key_event(&event);
        } else {
          pushSTACK(code_char(ch)); funcall(S(make_char),1); c = value1;
        }
        # noch zu behandeln: ??
        # Ctrl-2 -> #\Control-2, Ctrl-6 -> #\Code30, Ctrl-ß -> #\Code28,
        # Ctrl-+ -> #\Code29, Ctrl-ü -> #\Code27 = #\Escape
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        return c;
      } else
      #endif
      # DOS
      {
        var object c;
        run_time_stop(); # Run-Time-Stoppuhr anhalten
        {
          # Tastendruck abwarten, nichts ausgeben:
          var uintW erg = getch();
          var uintB code = (uintB)erg; # Ascii-Code
          var uintB scancode = (uintB)(erg>>8); # Scan-Code
          if (scancode == 0) {
            # Multikey-Event, z.B. accent+space oder Alt xyz
            # FIXME: This should take into account the encoding.
            pushSTACK(code_char(as_chart(code))); funcall(S(make_char),1); c = value1;
          } else {
            if ((code == 0) || (code == 0xE0)) {
              # Sondertaste
              if (scancode < 167) {
                c = make_key_event(&scancode_table[scancode]);
              } else {
                var key_event event = { NULL, 0, 0 };
                c = make_key_event(&event);
              }
            } else {
              if (((scancode >= 71) && (scancode < 84)) || (scancode == 55)
                  || ((scancode == 0xE0) && (code >= 32))
                 ) {
                # Ziffernblocktaste außer Enter (auch nicht F1 bis F12 !)
                var key_event event;
                event.key = NULL;
                event.code = as_chart(code);
                event.bits = char_hyper_c;
                c = make_key_event(&event);
              } elif ((scancode == 14) || (scancode == 28)
                      || ((scancode == 0xE0) && (code < 32))
                     ) {
                # Backspace-Taste, Return-Taste, Enter-Taste
                var uintB defaultcode = (scancode==14 ? BS : CR);
                var key_event event;
                event.key = NULL;
                event.code = as_chart(defaultcode);
                event.bits = (scancode == 0xE0 ? char_hyper_c : 0)
                             | (!(code == defaultcode) ? char_control_c : 0);
                c = make_key_event(&event);
              } else {
                if ((code < 32) && ((scancode >= 16) && (scancode <= 53))) {
                  # Ctrl-A bis Ctrl-Z -> Buchstabe mit CONTROL-Bit draus machen:
                  var key_event event;
                  event.key = NULL;
                  event.code = ascii(code | bit(6));
                  event.bits = char_control_c;
                  c = make_key_event(&event);
                } else {
                  # normales Zeichen
                  # FIXME: This should take into account the encoding.
                  pushSTACK(code_char(as_chart(code))); funcall(S(make_char),1); c = value1;
                }
              }
            }
          }
          # noch zu behandeln: ??
          # Ctrl-2          0300
          # Ctrl-6          071E
          # Ctrl-ß          0C1C
          # Ctrl--          0C1F
        }
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        return c;
      }
    }
  #endif

  #ifdef WIN32_NATIVE
  local object rd_ch_keyboard(stream_)
    var const object* stream_;
    {
      var INPUT_RECORD event;
      var DWORD nevents_read;
      var Handle handle;
     restart_it:
      handle = TheHandle(TheStream(*stream_)->strm_keyboard_handle);
      begin_system_call();
      loop {
        if (!ReadConsoleInput1(handle,&event,&nevents_read)) {
          if (GetLastError()==ERROR_SIGINT) { # Unterbrechung durch Ctrl-C ?
            end_system_call();
            pushSTACK(S(read_char)); tast_break(); # Break-Schleife aufrufen
            goto restart_it;
          }
          OS_error();
        }
        ASSERT(nevents_read==1);
        if (event.EventType == KEY_EVENT && event.Event.KeyEvent.bKeyDown) {
          var key_event ev;
          if (event.Event.KeyEvent.wRepeatCount > 1) {
            var DWORD nevents_written;
            event.Event.KeyEvent.wRepeatCount--;
            if (!WriteConsoleInput(handle,&event,1,&nevents_written)) {
              OS_error();
            }
          }
          if (event.Event.KeyEvent.uAsciiChar <= ' ') {
            # Translate Virtual Keycode.
            local struct { WORD vkcode; key_event myevent; } vktable[] = {
              VK_BACK,    { NULL,  BS, 0 },               # #\Backspace
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
            for (i = 0; i < sizeof(vktable)/sizeof(vktable[0]); i++) {
              if (event.Event.KeyEvent.wVirtualKeyCode == vktable[i].vkcode) {
                ev = vktable[i].myevent; goto found_keycode;
              }
            }
            switch (event.Event.KeyEvent.wVirtualKeyCode) {
              case VK_SHIFT:
              case VK_CONTROL:
              case 18: case 20:
                break;
              default:
                asciz_out_3("Unknown keyboard event, VKeyCode = %d, VScanCode = %d, AsciiChar = %d\n",event.Event.KeyEvent.wVirtualKeyCode,event.Event.KeyEvent.wVirtualScanCode,event.Event.KeyEvent.uAsciiChar);
            }
            continue;
           found_keycode:
            if (event.Event.KeyEvent.dwControlKeyState & SHIFT_PRESSED)
              ev.bits |= char_super_c;
            if (event.Event.KeyEvent.dwControlKeyState & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED))
              ev.bits |= char_control_c;
            if (event.Event.KeyEvent.dwControlKeyState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED))
              ev.bits |= char_meta_c;
          } else {
            ev.key = NULL;
            ev.code = as_chart((uintB)event.Event.KeyEvent.uAsciiChar); # FIXME: This should take into account the encoding.
            ev.bits = 0;
            if (event.Event.KeyEvent.dwControlKeyState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED)) {
              # c = 'a'..'z' -> translate to 'A'..'Z'
              # c = 'A'..'Z' -> add "Shift"
              # c = '<','>' etc. -> don't add "Shift"
              ev.code = up_case(ev.code);
              if (!chareq(ev.code,down_case(ev.code))) {
                if (event.Event.KeyEvent.dwControlKeyState & SHIFT_PRESSED)
                  ev.bits |= char_super_c;
              }
              ev.bits |= char_meta_c;
            }
          }
          end_system_call();
          return make_key_event(&ev);
        }
        # Other events are silently thrown away.
      }
    }
  #endif

  #if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)

  # vgl. rd_ch_unbuffered() :
  local object rd_ch_keyboard(stream_)
    var const object* stream_;
    {
     restart_it:
      var object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF?
        return eof_value;
      # Noch etwas im Buffer?
      if (mconsp(TheStream(stream)->strm_keyboard_buffer))
        goto empty_buffer;
      # Ein Zeichen lesen:
      {
        var uintB c;
       read_next_char:
        {
          run_time_stop(); # Run-Time-Stoppuhr anhalten
          begin_system_call();
          var int ergebnis = read(stdin_handle,&c,1); # Zeichen lesen versuchen
          end_system_call();
          run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
          if (ergebnis<0) {
            begin_system_call();
            if (errno==EINTR) { # Unterbrechung (evtl. durch Ctrl-C) ?
              end_system_call();
              interruptp({ pushSTACK(S(read_char)); tast_break(); }); # Break-Schleife aufrufen
              goto restart_it;
            }
            OS_error();
          }
          if (ergebnis==0) {
            # kein Zeichen verfügbar -> EOF erkennen
            TheStream(stream)->strm_rd_ch_last = eof_value; return eof_value;
          }
        }
       next_char_is_read:
        # Es verlängert den Buffer:
        {
          var object new_cons = allocate_cons();
          Car(new_cons) = code_char(as_chart(c)); # FIXME: This should take into account the encoding.
          stream = *stream_;
          var object* last_ = &TheStream(stream)->strm_keyboard_buffer;
          while (mconsp(*last_)) { last_ = &Cdr(*last_); }
          *last_ = new_cons;
        }
        # Ist der Buffer eine vollständige Zeichenfolge zu einer Taste,
        # so liefern wir diese Taste. Ist der Buffer ein echtes Anfangsstück
        # einer Zeichenfolge zu einer Taste, so warten wir noch ein wenig.
        # Ansonsten fangen wir an, den Buffer Zeichen für Zeichen zu leeren.
        {
          var object keytab = TheStream(stream)->strm_keyboard_keytab;
          while (consp(keytab)) {
            var object L1 = Car(keytab);
            keytab = Cdr(keytab);
            var object L2 = TheStream(stream)->strm_keyboard_buffer;
            while (consp(L1) && consp(L2) && eq(Car(L1),Car(L2))) {
              L1 = Cdr(L1); L2 = Cdr(L2);
            }
            if (atomp(L2)) {
              if (atomp(L1)) {
                # vollständige Zeichenfolge
                TheStream(stream)->strm_keyboard_buffer = NIL;
                return L1;
              }
            }
          }
        }
        {
          var object keytab = TheStream(stream)->strm_keyboard_keytab;
          while (consp(keytab)) {
            var object L1 = Car(keytab);
            keytab = Cdr(keytab);
            var object L2 = TheStream(stream)->strm_keyboard_buffer;
            while (consp(L1) && consp(L2) && eq(Car(L1),Car(L2))) {
              L1 = Cdr(L1); L2 = Cdr(L2);
            }
            if (atomp(L2))
              # Da consp(L1), liegt ein Anfangsstück einer Zeichenfolge vor.
              goto wait_for_another;
          }
        }
        goto empty_buffer;
       wait_for_another:
        #ifdef HAVE_SELECT
        {
          # Verwende select mit readfds = einelementige Menge {stdin_handle}
          # und timeout = kleines Zeitintervall.
          var fd_set handle_menge; # Menge von Handles := {stdin_handle}
          var struct timeval small_time; # Zeitintervall := 0
          FD_ZERO(&handle_menge); FD_SET(stdin_handle,&handle_menge);
          restart_select:
          small_time.tv_sec = 0; small_time.tv_usec = 1000000/10; # 1/10 sec
          run_time_stop(); # Run-Time-Stoppuhr anhalten
          begin_system_call();
          var int ergebnis;
          ergebnis = select(FD_SETSIZE,&handle_menge,NULL,NULL,&small_time);
          end_system_call();
          run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
          if (ergebnis<0) {
            begin_system_call();
            if (errno==EINTR) {
              end_system_call(); goto restart_select;
            }
            if (!(errno == EBADF)) {
              OS_error();
            }
            end_system_call();
          } else {
            # ergebnis = Anzahl der Handles in handle_menge, bei denen read
            # sofort ein Ergebnis liefern würde.
            if (ergebnis==0)
              goto empty_buffer; # kein Zeichen verfügbar
            # ergebnis=1 -> Zeichen verfügbar
          }
        }
        #else
        #if defined(UNIX_TERM_TERMIOS) || defined(UNIX_TERM_TERMIO)
        {
          # Verwende die Termio-Elemente VMIN und VTIME.
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
          if (!( tcgetattr(stdin_handle,&oldtermio) ==0)) {
            if (!(errno==ENOTTY)) { OS_error(); }
          }
          #else
          if (!( ioctl(stdin_handle,TCGETA,&oldtermio) ==0)) {
            if (!(errno==ENOTTY)) { OS_error(); }
          }
          #endif
          # Wir gehen nun davon aus, dass oldtermio nun mit dem newtermio aus
          # term_raw() (s.u.) identisch ist. Das ist dann gewährleistet, wenn
          # 1. (SYS::TERMINAL-RAW T) aufgerufen wurde und
          # 2. stdin_handle und stdout_handle beide dasselbe Terminal sind. ??
          newtermio = oldtermio;
          newtermio.c_cc[VMIN] = 0;
          newtermio.c_cc[VTIME] = 1; # 1/10 Sekunde Timeout
          #ifdef UNIX_TERM_TERMIOS
          if (!( TCSETATTR(stdin_handle,TCSANOW,&newtermio) ==0)) {
            if (!(errno==ENOTTY)) { OS_error(); }
          }
          #else
          if (!( ioctl(stdin_handle,TCSETA,&newtermio) ==0)) {
            if (!(errno==ENOTTY)) { OS_error(); }
          }
          #endif
          var int ergebnis = read(stdin_handle,&c,1); # Zeichen lesen versuchen, mit Timeout
          #ifdef UNIX_TERM_TERMIOS
          if (!( TCSETATTR(stdin_handle,TCSANOW,&oldtermio) ==0)) {
            if (!(errno==ENOTTY)) { OS_error(); }
          }
          #else
          if (!( ioctl(stdin_handle,TCSETA,&oldtermio) ==0)) {
            if (!(errno==ENOTTY)) { OS_error(); }
          }
          #endif
          end_system_call();
          run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
          if (ergebnis<0) {
            begin_system_call();
            if (errno==EINTR) { # Unterbrechung (evtl. durch Ctrl-C) ?
              end_system_call();
              interruptp({ pushSTACK(S(read_char)); tast_break(); }); # Break-Schleife aufrufen
              goto restart_it;
            }
            OS_error();
          }
          if (ergebnis==0)
            goto empty_buffer; # kein Zeichen verfügbar
          goto next_char_is_read; # ergebnis=1 -> Zeichen verfügbar
        }
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
      {
        var object l = TheStream(stream)->strm_keyboard_buffer;
        TheStream(stream)->strm_keyboard_buffer = Cdr(l);
        var cint c = as_cint(char_code(Car(l)));
        if ((c >= ' ') || (c == ESC) || (c == TAB) || (c == CR) || (c == BS)) {
          # FIXME: This should take into account the encoding.
          pushSTACK(code_char(as_chart(c))); funcall(S(make_char),1);
          return value1;
        } else {
          # Taste vermutlich mit Ctrl getippt
          var key_event event;
          event.key = NULL;
          event.code = ascii(c == 0 ? ' ' : (c | bit(6)));
          event.bits = char_control_c;
          return make_key_event(&event);
        }
      }
    }

  # UP: Erweitert die Liste STACK_0 um eine Tastenzuordnung.
  # kann GC auslösen
    local void add_keybinding (const char* cap, const key_event* event);
    local void add_keybinding(cap,event)
      var const char* cap;
      var const key_event* event;
      {
        var const uintB* ptr = (const uintB*)cap;
        if (*ptr=='\0') # leere Tastenfolge vermeiden
          return;
        # FIXME: This should take into account the encoding.
        pushSTACK(allocate_cons());
        # Liste (char1 ... charn . key) bilden:
        {
          var uintC count = 0;
          do {
            pushSTACK(code_char(as_chart(*ptr))); ptr++; count++;
          } until (*ptr=='\0');
          pushSTACK(make_key_event(event)); count++;
          funcall(L(liststern),count);
        }
        # und auf STACK_0 pushen:
        {
          var object l = popSTACK();
          Car(l) = value1; Cdr(l) = STACK_0; STACK_0 = l;
        }
      }
  #define keybinding(cap,initializer)  \
    { var key_event event = initializer; add_keybinding(cap,&event); }

  #endif

  #ifdef NEXTAPP
    #define rd_ch_keyboard  rd_ch_error
  #endif

# Liefert einen Keyboard-Stream.
# make_keyboard_stream()
# can trigger GC
  local object make_keyboard_stream (void);
  local object make_keyboard_stream()
    {
      #if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)
      {
        # Tabelle aller Zuordnungen Zeichenfolge -> Taste bilden:
        pushSTACK(NIL);
        # Terminal-Typ abfragen:
        begin_system_call();
        var const char* s = getenv("TERM");
        if (s==NULL) {
          end_system_call();
        } else {
          var char tbuf[4096]; # interner Buffer für die Termcap-Routinen
          if (!(tgetent(tbuf,s)==1)) {
            end_system_call();
          } else {
            var char tentry[4096]; # Buffer für von mir benötigte Capabilities und Pointer da hinein
            var char* tp = &tentry[0];
            var const char* cap;
            end_system_call();
            # Backspace:
            begin_system_call(); cap = tgetstr("kb",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_ascii(BS)); # #\Backspace
            # Insert, Delete:
            begin_system_call(); cap = tgetstr("kI",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("INSERT")); # #\Insert
            begin_system_call(); cap = tgetstr("kD",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("DELETE")); # #\Delete
            # Pfeiltasten:
            begin_system_call(); cap = tgetstr("ku",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("UP")); # #\Up
            if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'A') && (cap[3] == '\0'))
              keybinding(ESCstring"[A", key_special("UP")); # #\Up
            begin_system_call(); cap = tgetstr("kd",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("DOWN")); # #\Down
            if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'B') && (cap[3] == '\0'))
              keybinding(ESCstring"[B", key_special("DOWN")); # #\Down
            begin_system_call(); cap = tgetstr("kr",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("RIGHT")); # #\Right
            if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'C') && (cap[3] == '\0'))
              keybinding(ESCstring"[C", key_special("RIGHT")); # #\Right
            begin_system_call(); cap = tgetstr("kl",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("LEFT")); # #\Left
            if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'D') && (cap[3] == '\0'))
              keybinding(ESCstring"[D", key_special("LEFT")); # #\Left
            # sonstige Cursorblock-Tasten:
            begin_system_call(); cap = tgetstr("kh",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("HOME")); # #\Home
            begin_system_call(); cap = tgetstr("K1",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("HOME")); # #\Home
            begin_system_call(); cap = tgetstr("KH",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("END")); # #\End
            begin_system_call(); cap = tgetstr("K4",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("END")); # #\End
            begin_system_call(); cap = tgetstr("kP",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("PGUP")); # #\PgUp
            begin_system_call(); cap = tgetstr("K3",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("PGUP")); # #\PgUp
            begin_system_call(); cap = tgetstr("kN",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("PGDN")); # #\PgDn
            begin_system_call(); cap = tgetstr("K5",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("PGDN")); # #\PgDn
            begin_system_call(); cap = tgetstr("K2",&tp); end_system_call();
            if (cap)
              keybinding(cap, key_special("CENTER")); # #\Center
            # Funktionstasten:
            {
              typedef struct { const char* capname; key_event key; } funkey;
              local var const funkey funkey_tab[] = {
                { "k1", key_special("F1") }, # #\F1
                { "k2", key_special("F2") }, # #\F2
                { "k3", key_special("F3") }, # #\F3
                { "k4", key_special("F4") }, # #\F4
                { "k5", key_special("F5") }, # #\F5
                { "k6", key_special("F6") }, # #\F6
                { "k7", key_special("F7") }, # #\F7
                { "k8", key_special("F8") }, # #\F8
                { "k9", key_special("F9") }, # #\F9
                { "k0", key_special("F10") }, # #\F10
                { "k;", key_special("F10") }, # #\F10
                { "F1", key_special("F11") }, # #\F11
                { "F2", key_special("F12") }, # #\F12
                };
              var uintL i;
              for (i=0; i < sizeof(funkey_tab)/sizeof(funkey); i++) {
                begin_system_call();
                cap = tgetstr(funkey_tab[i].capname,&tp);
                end_system_call();
                if (cap)
                  add_keybinding(cap,&funkey_tab[i].key);
              }
            }
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
            keybinding(ESCstring"[4~", key_special("END")); # #\End
            if (FALSE) {
             not_linux:
              end_system_call();
            }
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
            # Insert, Delete:
            keybinding(ESCstring"[2~", key_special("INSERT")); # #\Insert
            keybinding(ESCstring"[3~", key_special("DELETE")); # #\Delete
            {
              # Application Keypad: ESC O M -> Return,
              # ESC O k -> +, ESC O m -> -, ESC O j -> *, ESC O o -> /
              # (ohne Hyper-Bit, da das zu Terminal-abhängig würde)
              # ESC O x -> Up, ESC O r -> Down,
              # ESC O v -> Right, ESC O t -> Left,
              # ESC O p -> Insert, ESC O l -> Delete.
              var char cap[4];
              cap[0] = ESC; cap[1] = 'O'; cap[3] = '\0';
              cap[2] = 'M'; keybinding(&!cap, key_ascii('M'-64));
              cap[2] = '+'+64; keybinding(&!cap, key_ascii('+'));
              cap[2] = '-'+64; keybinding(&!cap, key_ascii('-'));
              cap[2] = '*'+64; keybinding(&!cap, key_ascii('*'));
              cap[2] = '/'+64; keybinding(&!cap, key_ascii('/'));
              cap[2] = '8'+64; keybinding(&!cap, key_special("UP")); # #\Up
              cap[2] = '2'+64; keybinding(&!cap, key_special("DOWN")); # #\Down
              cap[2] = '6'+64; keybinding(&!cap, key_special("RIGHT")); # #\Right
              cap[2] = '4'+64; keybinding(&!cap, key_special("LEFT")); # #\Left
              cap[2] = '0'+64; keybinding(&!cap, key_special("INSERT")); # #\Insert
              cap[2] = '.'+64; keybinding(&!cap, key_special("DELETE")); # #\Delete
              cap[2] = ','+64; keybinding(&!cap, key_special("DELETE")); # #\Delete
              # "7" -> #\Home, "1" -> #\End, "9" -> #\PgUp, "3" -> #\PgDn,
              # "5" -> #\Center are already handled above.
            }
           xterm:
            # Pfeiltasten s.o.
            # sonstige Cursorblock-Tasten:
            keybinding(ESCstring"[5~", key_special("PGUP")); # #\PgUp
            keybinding(ESCstring"[6~", key_special("PGDN")); # #\PgDn
            keybinding(ESCstring"[7~", key_special("HOME")); # #\Home
            keybinding(ESCstring"[8~", key_special("END")); # #\End
            keybinding(ESCstring"OH", key_special("HOME")); # #\Home
            keybinding(ESCstring"[H", key_special("HOME")); # #\Home
            keybinding(ESCstring"OF", key_special("END")); # #\End
            keybinding(ESCstring"[F", key_special("END")); # #\End
            # Funktionstasten:
            keybinding(ESCstring"[11~", key_special("F1")); # #\F1
            keybinding(ESCstring"[12~", key_special("F2")); # #\F2
            keybinding(ESCstring"[13~", key_special("F3")); # #\F3
            keybinding(ESCstring"[14~", key_special("F4")); # #\F4
            keybinding(ESCstring"[15~", key_special("F5")); # #\F5
            keybinding(ESCstring"[17~", key_special("F6")); # #\F6
            keybinding(ESCstring"[18~", key_special("F7")); # #\F7
            keybinding(ESCstring"[19~", key_special("F8")); # #\F8
            keybinding(ESCstring"[20~", key_special("F9")); # #\F9
            keybinding(ESCstring"[21~", key_special("F10")); # #\F10
            keybinding(ESCstring"[23~", key_special("F11")); # #\F11
            keybinding(ESCstring"[24~", key_special("F12")); # #\F12
            if (FALSE) {
             not_xterm:
              end_system_call();
            }
          }
        }
      }
      pushSTACK(allocate_handle(stdin_handle));
     #endif
     #ifdef WIN32_NATIVE
      # Console-Handle bilden:
      # Maybe use CREATE_ALWAYS ?? Maybe use AllocConsole() ??
      {
        var Handle handle = CreateFile("CONIN$", GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
        if (handle==INVALID_HANDLE_VALUE) {
          OS_error();
        }
        end_system_call();
        pushSTACK(allocate_handle(handle));
      }
     #endif
      # neuen Stream allozieren:
      var object stream =
        allocate_stream(strmflags_rd_ch_B,strmtype_keyboard,strm_keyboard_len,strm_keyboard_xlen);
        # Flags: nur READ-CHAR erlaubt
      # und füllen:
      var Stream s = TheStream(stream);
        #ifdef UNICODE
        s->strm_encoding = O(terminal_encoding);
        #endif
        s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
        s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
        s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
        s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
        s->strm_rd_ch = P(rd_ch_keyboard); # READ-CHAR-Pseudofunktion
        s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
        s->strm_rd_ch_array = P(rd_ch_array_dummy); # READ-CHAR-SEQUENCE-Pseudofunktion
        s->strm_rd_ch_last = NIL; # Lastchar := NIL
        s->strm_wr_ch = P(wr_ch_error); # WRITE-CHAR unmöglich
        s->strm_wr_ch_array = P(wr_ch_array_error); # WRITE-CHAR unmöglich
        s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
        #if (defined(UNIX) && !defined(NEXTAPP)) || defined(RISCOS)
        # Flag isatty = (stdin_tty ? T : NIL) bestimmen:
        begin_system_call();
        s->strm_keyboard_isatty = (isatty(stdin_handle) ? T : NIL);
        end_system_call();
        s->strm_keyboard_handle = popSTACK();
        s->strm_keyboard_buffer = NIL;
        s->strm_keyboard_keytab = popSTACK();
        ChannelStream_buffered(stream) = FALSE;
        ChannelStream_init(stream);
        UnbufferedHandleStream_input_init(stream);
        #endif
        #ifdef WIN32_NATIVE
        s->strm_keyboard_isatty = T;
        s->strm_keyboard_handle = popSTACK();
        ChannelStream_buffered(stream) = FALSE;
        ChannelStream_init(stream);
        UnbufferedHandleStream_input_init(stream);
        #endif
      return stream;
    }

LISPFUNN(make_keyboard_stream,0)
# (SYSTEM::MAKE-KEYBOARD-STREAM) creates a new keyboard stream.
# Should be called once only, and the result assigned to *KEYBOARD-INPUT*.
  {
    value1 = make_keyboard_stream(); mv_count=1;
  }

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
    {
      # Dies ist eine Callback-Funktion, wir müssen den Stack wieder korrekt setzen:
      begin_callback();
      # (SYS::COMPLETION text start end) aufrufen:
      pushSTACK(asciz_to_string(text,O(terminal_encoding)));
      pushSTACK(fixnum((uintL)start));
      pushSTACK(fixnum((uintL)end));
      funcall(S(completion),3);
      var object mlist = value1; # Liste der Möglichkeiten
      end_callback();
      # Liste von Simple-Strings in mallozierten Array von mallozierten
      # Asciz-Strings umbauen:
      if (nullp(mlist))
        return NULL;
      var char** array = (char**) malloc((llength(mlist)+1)*sizeof(char*));
      if (array==NULL)
        return NULL;
      {
        var char** ptr = array;
        while (consp(mlist)) {
          var uintL charcount = Sstring_length(Car(mlist));
          var const chart* ptr1;
          unpack_sstring_alloca(Car(mlist),charcount,0, ptr1=);
          var uintL bytecount = cslen(O(terminal_encoding),ptr1,charcount);
          var char* ptr2 = (char*) malloc((bytecount+1)*sizeof(char));
          if (ptr2==NULL) { # malloc scheitert -> alles zurückgeben
            until (ptr==array) { free(*--ptr); }
            free(array);
            return NULL;
          }
          cstombs(O(terminal_encoding),ptr1,charcount,(uintB*)ptr2,bytecount);
          ptr2[bytecount] = '\0';
          *ptr++ = ptr2;
          mlist = Cdr(mlist);
        }
        *ptr = NULL;
      }
      return array;
    }
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
    {
      var int linepos;
      var uintB ch;
      begin_call();
      ch = nxterminal_read_char(&linepos);
      end_call();
      TheStream(*stream_)->strm_wr_ch_lpos = fixnum(linepos);
      return code_char(as_chart(ch)); # FIXME: This should take into account the encoding.
    }

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal(stream)
# > stream: Terminal-Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
  local signean listen_terminal (object stream);
  local signean listen_terminal(stream)
    var object stream;
    {
      var signean result;
      begin_call();
      result = (nxterminal_listen() ? ls_avail : ls_wait);
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
    {
      # Wir wollen im Eingabefenster nichts löschen.
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
    {
      var object stream = *stream_;
      if (!charp(ch)) # ch sollte Character sein
        fehler_wr_char(stream,ch);
      begin_call();
      nxterminal_write_char(char_code(ch));
      end_call();
    }

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# finish_output_terminal(stream);
# > stream: Terminal-Stream
# can trigger GC
  local void finish_output_terminal (object stream);
  local void finish_output_terminal(stream)
    var object stream;
    {
      begin_call();
      nxterminal_send_output();
      end_call();
    }

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# force_output_terminal(stream);
# > stream: Terminal-Stream
# can trigger GC
  #define force_output_terminal(stream)  finish_output_terminal(stream)

# Liefert einen interaktiven Terminal-Stream.
# can trigger GC
  local object make_terminal_stream_ (void);
  local object make_terminal_stream_()
    {
      # neuen Stream allozieren:
      var object stream =
        allocate_stream(strmflags_ch_B,strmtype_terminal,strm_len+0,0);
        # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
      # und füllen:
      var Stream s = TheStream(stream);
        s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
        s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
        s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
        s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
        s->strm_rd_ch = P(rd_ch_terminal); # READ-CHAR-Pseudofunktion
        s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
        s->strm_rd_ch_array = P(rd_ch_array_dummy); # READ-CHAR-SEQUENCE-Pseudofunktion
        s->strm_rd_ch_last = NIL; # Lastchar := NIL
        s->strm_wr_ch = P(wr_ch_terminal); # WRITE-CHAR-Pseudofunktion
        s->strm_wr_ch_array = P(wr_ch_array_dummy); # WRITE-CHAR-SEQUENCE-Pseudofunktion
        s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
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
  #define strm_terminal_ihandle  strm_ichannel
  #define strm_terminal_ohandle  strm_ochannel
#if defined(HAVE_TERMINAL2) || defined(HAVE_TERMINAL3)
  # Komponenten wegen TERMINAL_LINEBUFFERED:
  # INBUFF : Eingabebuffer, ein Semi-Simple-String
  #define strm_terminal_inbuff  strm_field1
  # COUNT = sein Fill-Pointer : Anzahl der Zeichen im Eingabebuffer
  # INDEX : Anzahl der bereits verbrauchten Zeichen
  #define strm_terminal_index   strm_other[2]  # FIXME: this is ugly
#endif
#ifdef HAVE_TERMINAL3
  # Komponenten wegen TERMINAL_OUTBUFFERED:
  # OUTBUFF : Ausgabebuffer, ein Semi-Simple-String
  #define strm_terminal_outbuff strm_field2
#endif
#define strm_terminal_len  strm_channel_len

# Unterscheidung nach Art des Terminal-Streams:
# terminalcase(stream, statement1,statement2,statement3);
  #if defined(HAVE_TERMINAL2) && defined(HAVE_TERMINAL3)
    #define terminalcase(stream,statement1,statement2,statement3)  \
      if (nullp(TheStream(stream)->strm_field2))     \
        { if (nullp(TheStream(stream)->strm_field1)) \
            { statement1 }                           \
            else                                     \
            { statement2 }                           \
        }                                            \
        else                                         \
        { statement3 }
  #elif defined(HAVE_TERMINAL2)
    #define terminalcase(stream,statement1,statement2,statement3)  \
      if (nullp(TheStream(stream)->strm_field1)) { statement1 } else { statement2 }
  #elif defined(HAVE_TERMINAL3)
    #define terminalcase(stream,statement1,statement2,statement3)  \
      if (nullp(TheStream(stream)->strm_field2)) { statement1 } else { statement3 }
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
      {
        var union REGS in;
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
    {
      var object ch = rd_ch_unbuffered(stream_);
      # Wenn stdin und stdout beide dasselbe Terminal sind,
      # und wir lesen ein NL, so können wir davon ausgehen,
      # dass der Cursor danach in Spalte 0 steht.
      if (eq(ch,ascii_char(NL))) {
        var object stream = *stream_;
        if (eq(TheStream(stream)->strm_terminal_isatty,S(equal)))
          TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      }
      return ch;
    }

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal1(stream)
# > stream: Terminal-Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
  #define listen_terminal1  listen_unbuffered

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal1(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  #define clear_input_terminal1  clear_input_unbuffered

# UP: Ein Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_terminal1(&stream,ch);
# > stream: Terminal-Stream
# > ch: auszugebendes Zeichen
 #if !defined(AMIGAOS)
  #define wr_ch_terminal1  wr_ch_unbuffered_unix
 #else # defined(AMIGAOS)
  local void wr_ch_terminal1 (const object* stream_, object ch);
  local void wr_ch_terminal1(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      # ch sollte ein Character mit höchstens Font, aber ohne Bits sein:
      #error "FIXME character fonts don't exist in this form any more"
      if (!((as_oint(ch) & ~(((oint)char_code_mask_c|(oint)char_font_mask_c)<<oint_data_shift)) == as_oint(type_data_object(char_type,0))))
        { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
          pushSTACK(*stream_);
          pushSTACK(ch);
          fehler(stream_error,
                 GETTEXT("character ~ contains bits, cannot be output onto ~")
                );
        }
      #if (!(char_font_len_c == 4))
        #error "char_font_len_c neu einstellen oder wr_ch_terminal neu schreiben!"
      #endif
      var uintB outbuffer[14];
      var uintB* ptr = &outbuffer[0];
      var uintL count = 1;
      var uintB f = (char_int(ch) & char_font_mask_c) >> char_font_shift_c; # Font des Zeichens
      var uintB c = char_code(ch); # Code des Zeichens
      if (f==0) {
        *ptr++ = c;
      } else {
        *ptr++ = CSI; # Kontroll-Sequenz zum Umschalten auf den richtigen Font:
        if (f & bit(0)) {
          *ptr++ = ';'; *ptr++ = '1'; count += 2; # Fettschrift ein
        }
        if (f & bit(1)) {
          *ptr++ = ';'; *ptr++ = '3'; count += 2; # Kursiv ein
        }
        if (f & bit(2)) {
          *ptr++ = ';'; *ptr++ = '4'; count += 2; # Unterstreichung ein
        }
        if (f & bit(3)) {
          *ptr++ = ';'; *ptr++ = '7'; count += 2; # Reverse ein
        }
        *ptr++ = 0x6D;
        *ptr++ = c; # dann das Zeichen ausgeben
        *ptr++ = CSI; *ptr++ = '0'; *ptr++ = 0x6D; # Wieder Normalschrift
        count += 5;
      }
      begin_system_call();
      var long ergebnis = Write(stdout_handle,&outbuffer[0],count); # Zeichen auszugeben versuchen
      end_system_call();
      if (ergebnis<0) {
        OS_error(); # Error melden
      }
      if (ergebnis<count) # nicht erfolgreich?
        fehler_unwritable(S(write_char),*stream_);
    }
 #endif

# UP: Mehrere Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_array_terminal1(&stream,&chararray,start,len);
# > stream: Terminal-Stream
# > chararray: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
  #define wr_ch_array_terminal1  wr_ch_array_unbuffered_unix

# UP: Löscht den wartenden Output eines Terminal-Stream.
# clear_output_terminal1(stream);
# > stream: Terminal-Stream
# can trigger GC
  #define clear_output_terminal1  clear_output_unbuffered

#endif # HAVE_TERMINAL1

#ifdef HAVE_TERMINAL2

#define TERMINAL_LINEBUFFERED  TRUE

# Lesen eines Zeichens von einem Terminal-Stream.
  local object rd_ch_terminal2 (const object* stream_);
  local object rd_ch_terminal2(stream_)
    var const object* stream_;
    {
      var object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        return eof_value;
      if (!(posfixnum_to_L(TheStream(stream)->strm_terminal_index)
            < TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1]
         ) ) {
        # index=count -> muss eine ganze Zeile von Tastatur lesen:
        TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
        TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1] = 0; # count := 0
        loop {
          var object ch = rd_ch_unbuffered(stream_);
          if (eq(ch,eof_value)) {
            if (TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1] > 0)
              break; # Zeichen des Buffers liefern, dann erst eof_value liefern
            else
              return eof_value;
          }
          # Zeichen ch zur Eingabezeile dazunehmen, evtl. die Zeile vergrößern:
          ssstring_push_extend(TheStream(stream)->strm_terminal_inbuff,char_code(ch));
          stream = *stream_;
          # Wenn stdin und stdout beide dasselbe Terminal sind,
          # und wir lesen ein NL, so können wir davon ausgehen,
          # dass der Cursor danach in Spalte 0 steht.
          if (chareq(char_code(ch),ascii(NL))) {
            if (eq(TheStream(stream)->strm_terminal_isatty,S(equal)))
              TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
            break; # Zeichen des Buffers liefern
          }
        }
        ASSERT(posfixnum_to_L(TheStream(stream)->strm_terminal_index)
               < TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1]
              );
      }
      # index<count -> Es sind noch Zeichen im Buffer
      var uintL index =
        posfixnum_to_L(TheStream(stream)->strm_terminal_index); # Index
      TheStream(stream)->strm_terminal_index =
        fixnum_inc(TheStream(stream)->strm_terminal_index,1); # Index erhöhen
      return code_char(TheSstring(TheIarray(TheStream(stream)->strm_terminal_inbuff)->data)->data[index]); # nächstes Character
    }

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal2(stream)
# > stream: Terminal-Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
  local signean listen_terminal2 (object stream);
  local signean listen_terminal2(stream)
    var object stream;
    {
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        return ls_eof;
      if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
          < TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1]
         )
        # index<count -> Es sind noch Zeichen im Buffer
        return ls_avail;
      return listen_unbuffered(stream);
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal2(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_terminal2 (object stream);
  local boolean clear_input_terminal2(stream)
    var object stream;
    {
      if (nullp(TheStream(stream)->strm_terminal_isatty))
        # File -> nichts tun
        return FALSE;
      # Terminal
      clear_input_unbuffered(stream); # forget about past EOF, call clear_tty_input
      #if TERMINAL_LINEBUFFERED
      TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
      TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1] = 0; # count := 0
      #endif
      pushSTACK(stream);
      while (ls_avail_p(listen_terminal2(STACK_0))) {
        read_char(&STACK_0);
      }
      skipSTACK(1);
      return TRUE;
    }

# UP: Ein Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_terminal2(&stream,ch);
# > stream: Terminal-Stream
# > ch: auszugebendes Zeichen
  #define wr_ch_terminal2  wr_ch_unbuffered_dos

# UP: Mehrere Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_array_terminal2(&stream,&chararray,start,len);
# > stream: Terminal-Stream
# > chararray: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
  #define wr_ch_array_terminal2  wr_ch_array_unbuffered_dos

# UP: Löscht den wartenden Output eines Terminal-Stream.
# clear_output_terminal2(stream);
# > stream: Terminal-Stream
# can trigger GC
  #define clear_output_terminal2  clear_output_unbuffered

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
    {
      if (((start>=2)
           && (rl_line_buffer[start-2]=='#')
           && (rl_line_buffer[start-1]== '\"')
          )
          ||
          ((start>=3)
           && (rl_line_buffer[start-3]=='#')
           && (rl_line_buffer[start-2]=='P' || rl_line_buffer[start-2]=='p')
           && (rl_line_buffer[start-1]== '\"')
         )) {
        # Vervollständigung nach #" oder #P" bezieht sich auf Filenamen:
        want_filename_completion = TRUE; return NULL;
      }
      var char** result = lisp_completion(rl_line_buffer,start,end);
      want_filename_completion = FALSE;
      return result;
    }

# Falls obige Funktion NULL (keine Matches) lieferte, wird die folgende
# Funktion so lange aufgerufen, bis sie ihrerseits NULL liefert.
  local char* lisp_completion_more (char* text, int state);
  local char* lisp_completion_more(text,state)
    var char* text;
    var int state;
    {
      if (want_filename_completion)
        return filename_completion_function(text,state);
      else
        return NULL;
    }

# In the implementation of rd_ch_terminal3 and listen_terminal3, we should
# not use the corresponding rd_ch_unbuffered and listen_unbuffered functions,
# because they store intermediately read bytes in
# UnbufferedStream_bytebuf(stream), where readline() will not see them.
# As a workaround, we use rl_stuff_char before calling readline().
#
# However, there is a deeper problem with the rd_ch_terminal3/listen_terminal3
# implementation: readline() terminates when `rl_done' gets set to 1, whereas
# listen_unbuffered normally returns ls_avail when the user has entered a line
# of characters followed by #\Newline. Normally this is the same condition,
# but if the user modifies his readline key bindings so that newline does not
# always cause `rl_done' to become 1, then rd_ch_terminal3() might block
# although listen_terminal3() returned ls_avail.
# One possible fix would be to use the READLINE_CALLBACK functions, see
# readline.dvi p. 29, but in order to get this right, RUN-PROGRAM and
# MAKE-PIPE-INPUT-STREAM might need to be modified to temporarily turn off
# readline.

# Lesen eines Zeichens von einem Terminal-Stream.
  local object rd_ch_terminal3 (const object* stream_);
  # vgl. rd_ch_unbuffered() :
  local object rd_ch_terminal3(stream_)
    var const object* stream_;
    {
      var object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        return eof_value;
      if (!(posfixnum_to_L(TheStream(stream)->strm_terminal_index)
            < TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1]
         ) ) {
        # index=count -> muss eine ganze Zeile von Tastatur lesen:
        TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
        TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1] = 0; # count := 0
        # Pass bytes that we have already read down into readline's buffer.
        while (UnbufferedStream_status(stream) > 0) {
          UnbufferedStreamLow_pop_byte(stream,b);
          begin_system_call(); rl_stuff_char(b); end_system_call();
        }
        {
          var char* prompt; # Prompt: letzte bisher ausgegebene Zeile
          {
            var object lastline = string_to_asciz(TheStream(stream)->strm_terminal_outbuff,TheStream(stream)->strm_encoding);
            begin_system_call();
            prompt = (char*) malloc(Sbvector_length(lastline)+1);
            if (!(prompt==NULL)) {
              strcpy(prompt,TheAsciz(lastline));
              #ifndef NO_MATCH  # not needed any more in readline-2.2-clisp or newer
              # Die readline()-Library arbeitet mit einer anderen Bildschirmbreite,
              # als sie bei der Ausgabe des Prompts benutzt wurde. Bei Prompts
              # länger als eine Bildschirmzeile gibt das Probleme. Wir behelfen
              # uns, indem wir an passender Stelle ein '\n' einfügen.
              {
                var uintL prompt_length = asciz_length(prompt);
                var uintL screenwidth = posfixnum_to_L(Symbol_value(S(prin_linelength)))+1;
                if (prompt_length >= screenwidth) {
                  var uintL insertpos = round_down(prompt_length,screenwidth);
                  var uintL i;
                  for (i = prompt_length; i >= insertpos; i--)
                    prompt[i+1] = prompt[i];
                  prompt[insertpos] = '\n';
                }
              }
              #endif
            }
            end_system_call();
          }
          # Lexem-trennende Characters: die mit Syntaxcode whsp,tmac,nmac
          # (siehe IO.D, eigentlich von der Readtable abhängig):
          rl_basic_word_break_characters = "\t" NLstring " \"#'(),;`";
          rl_basic_quote_characters = "\"|";
          rl_completer_quote_characters = "\\|";
          run_time_stop(); # Run-Time-Stoppuhr anhalten
          begin_call();
          rl_already_prompted = TRUE;
          var uintB* line = (uintB*)readline(prompt==NULL ? "" : prompt); # Zeile lesen
          end_call();
          run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
          if (!(prompt==NULL)) {
            begin_system_call(); free(prompt); end_system_call();
          }
          if (line==NULL)
            # EOF (am Zeilenanfang) erkennen
            return eof_value;
          # gelesene Zeile zur Eingabezeile dazunehmen:
          #ifdef UNICODE
          {
            var object inbuff = TheStream(*stream_)->strm_terminal_inbuff;
            var object encoding = TheStream(*stream_)->strm_encoding;
            var const uintB* bptr = line;
            var const uintB* bendptr = bptr+asciz_length(bptr);
            var uintL clen = Encoding_mblen(encoding)(encoding,bptr,bendptr);
            ssstring_extend(inbuff,TheIarray(inbuff)->dims[1]+clen);
            inbuff = TheStream(*stream_)->strm_terminal_inbuff;
            encoding = TheStream(*stream_)->strm_encoding;
            var chart* cptr = &TheSstring(TheIarray(inbuff)->data)->data[TheIarray(inbuff)->dims[1]];
            var chart* cendptr = cptr+clen;
            Encoding_mbstowcs(encoding)(encoding,nullobj,&bptr,bendptr,&cptr,cendptr);
            ASSERT(cptr == cendptr);
            TheIarray(inbuff)->dims[1] += clen;
          }
          #else
          {
            var const uintB* ptr = line;
            until (*ptr == '\0') {
              ssstring_push_extend(TheStream(*stream_)->strm_terminal_inbuff,as_chart(*ptr++));
            }
          }
          #endif
          ssstring_push_extend(TheStream(*stream_)->strm_terminal_inbuff,ascii(NL));
          # und in die History übernehmen, falls nicht leer:
          if (!(line[0]=='\0')) {
            begin_system_call(); add_history(line); end_system_call();
          }
          # Freigeben müssen wir die Zeile!
          begin_system_call(); free(line); end_system_call();
        }
        stream = *stream_;
        # Wenn stdin und stdout beide dasselbe Terminal sind, können
        # wir davon ausgehen, dass der Cursor in Spalte 0 steht.
        if (eq(TheStream(stream)->strm_terminal_isatty,S(equal))) {
          TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
          TheIarray(TheStream(stream)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
        }
        ASSERT(posfixnum_to_L(TheStream(stream)->strm_terminal_index)
               < TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1]
              );
      }
      # index<count -> Es sind noch Zeichen im Buffer
      var uintL index =
        posfixnum_to_L(TheStream(stream)->strm_terminal_index); # Index
      TheStream(stream)->strm_terminal_index =
        fixnum_inc(TheStream(stream)->strm_terminal_index,1); # Index erhöhen
      return code_char(TheSstring(TheIarray(TheStream(stream)->strm_terminal_inbuff)->data)->data[index]); # nächstes Character
    }

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal3(stream)
# > stream: Terminal-Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
  local signean listen_terminal3 (object stream);
  local signean listen_terminal3(stream)
    var object stream;
    {
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        return ls_eof;
      if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
          < TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1]
         )
        # index<count -> Es sind noch Zeichen im Buffer
        return ls_avail;
      return listen_unbuffered(stream);
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal3(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_terminal3 (object stream);
  local boolean clear_input_terminal3(stream)
    var object stream;
    {
      if (nullp(TheStream(stream)->strm_terminal_isatty))
        # File -> nichts tun
        return FALSE;
      # Terminal
      clear_input_unbuffered(stream); # forget about past EOF, call clear_tty_input
      #if TERMINAL_LINEBUFFERED
      TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
      TheIarray(TheStream(stream)->strm_terminal_inbuff)->dims[1] = 0; # count := 0
      #endif
      pushSTACK(stream);
      while (ls_avail_p(listen_terminal3(STACK_0))) {
        read_char(&STACK_0);
      }
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
    {
      if (!charp(ch)) # ch sollte Character sein
        fehler_wr_char(*stream_,ch);
      #if TERMINAL_OUTBUFFERED
      {
        var chart c = char_code(ch); # Code des Zeichens
        if (chareq(c,ascii(NL)))
          TheIarray(TheStream(*stream_)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
        else
          ssstring_push_extend(TheStream(*stream_)->strm_terminal_outbuff,c);
      }
      #endif
      wr_ch_unbuffered_unix(stream_,ch);
    }

# UP: Mehrere Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_array_terminal3(&stream,&chararray,start,len);
# > stream: Terminal-Stream
# > chararray: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
  local void wr_ch_array_terminal3 (const object* stream_, const object* chararray_, uintL start, uintL len);
  local void wr_ch_array_terminal3(stream_,chararray_,start,len)
    var const object* stream_;
    var const object* chararray_;
    var uintL start;
    var uintL len;
    {
      wr_ch_array_unbuffered_unix(stream_,chararray_,start,len);
      #if TERMINAL_OUTBUFFERED
      {
        var object string = *chararray_;
        var const chart* ptr;
        unpack_sstring_alloca(string,len,start, ptr =);
        # Zeichen seit dem letzten NL in den Buffer:
        var uintL pos = 0; # zähle die Zahl der Zeichen seit dem letzten NL
        var uintL count;
        ptr += len;
        dotimespL(count,len, {
          if (chareq(*--ptr,ascii(NL)))
            goto found_NL;
          pos++;
        });
        if (FALSE) {
         found_NL: # pos Zeichen seit dem letzten NL
          ptr++;
          TheIarray(TheStream(*stream_)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
        }
        if (pos > 0) {
          SstringDispatch(string,
            {
              # ptr points into the string, not GC-safe.
              var uintL index = start + len - pos;
              dotimespL(count,pos, {
                ssstring_push_extend(TheStream(*stream_)->strm_terminal_outbuff,
                                     TheSstring(*chararray_)->data[index]);
                index++;
              });
            },
            {
              # ptr points into the stack, not the string, so it's GC-safe.
              dotimespL(count,pos, {
                ssstring_push_extend(TheStream(*stream_)->strm_terminal_outbuff,
                                     *ptr++);
              });
            }
            );
        }
      }
      #endif
    }

# UP: Löscht den wartenden Output eines Terminal-Stream.
# clear_output_terminal3(stream);
# > stream: Terminal-Stream
# can trigger GC
  local void clear_output_terminal3 (object stream);
  local void clear_output_terminal3(stream)
    var object stream;
    {
      clear_output_unbuffered(stream);
      #if TERMINAL_OUTBUFFERED
      TheIarray(TheStream(stream)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
      #endif
    }

#endif # HAVE_TERMINAL3

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# finish_output_terminal(stream);
# > stream: Terminal-Stream
# can trigger GC
  #define finish_output_terminal  finish_output_unbuffered

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# force_output_terminal(stream);
# > stream: Terminal-Stream
# can trigger GC
  #define force_output_terminal  force_output_unbuffered

# Liefert einen interaktiven Terminal-Stream.
# can trigger GC
  local object make_terminal_stream_ (void);
  local object make_terminal_stream_()
    {
      #ifdef AMIGAOS
      # nur HAVE_TERMINAL1
      {
        pushSTACK(allocate_handle(stdout_handle));
        pushSTACK(allocate_handle(stdin_handle));
        var object stream =
          allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal_len,sizeof(strm_unbuffered_extrafields_struct));
        # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
        # und füllen:
        var Stream s = TheStream(stream);
          #ifdef UNICODE
          s->strm_encoding = O(terminal_encoding);
          #endif
          s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
          s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
          s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
          s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
          s->strm_rd_ch = P(rd_ch_terminal1); # READ-CHAR-Pseudofunktion
          s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
          s->strm_rd_ch_array = P(rd_ch_array_dummy); # READ-CHAR-SEQUENCE-Pseudofunktion
          s->strm_rd_ch_last = NIL; # Lastchar := NIL
          s->strm_wr_ch = P(wr_ch_terminal1); # WRITE-CHAR-Pseudofunktion
          s->strm_wr_ch_array = P(wr_ch_array_terminal1); # WRITE-CHAR-SEQUENCE-Pseudofunktion
          s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
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
        ChannelStream_buffered(stream) = FALSE;
        ChannelStream_init(stream);
        UnbufferedHandleStream_input_init(stream);
        UnbufferedHandleStream_output_init(stream);
        return stream;
      }
      #else
      {
        var int stdin_tty;
        var int stdout_tty;
        var int same_tty;
        begin_system_call();
        stdin_tty = isatty(stdin_handle); # stdin ein Terminal?
        stdout_tty = isatty(stdout_handle); # stdout ein Terminal?
        same_tty = FALSE; # vorläufig
        if (stdin_tty && stdout_tty) {
          # stdin und stdout Terminals.
          #if defined(UNIX) || defined(RISCOS)
            #if 0
            var const char* ergebnis;
            var object filename;
            ergebnis = ttyname(stdin_handle); # Filename von stdin holen
            if (!(ergebnis==NULL)) {
              end_system_call();
              filename = asciz_to_string(ergebnis,O(pathname_encoding));
              begin_system_call();
              ergebnis = ttyname(stdout_handle); # Filename von stdout holen
              if (!(ergebnis==NULL)) {
                end_system_call();
                pushSTACK(filename);
                filename = asciz_to_string(ergebnis,O(pathname_encoding));
                if (string_gleich(popSTACK(),filename)) # gleiche Filenamen?
                  same_tty = TRUE;
              }
            }
            #else # ttyname() ist recht langsam, fstat() geht schneller.
            struct stat stdin_stat;
            struct stat stdout_stat;
            if ((fstat(stdin_handle,&stdin_stat) >= 0) && (fstat(stdout_handle,&stdout_stat) >= 0))
              if ((stdin_stat.st_dev == stdout_stat.st_dev) && (stdin_stat.st_ino == stdout_stat.st_ino))
                same_tty = TRUE;
            #endif
          #endif
          #ifdef MSDOS
            if (   ((get_handle_info(stdin_handle) & (bit(7)|bit(0))) == (bit(7)|bit(0))) # stdin == console_input ?
                && ((get_handle_info(stdout_handle) & (bit(7)|bit(1))) == (bit(7)|bit(1))) # stdout == console_output ?
               )
              same_tty = TRUE;
          #endif
          #ifdef WIN32_NATIVE
            var DWORD console_mode;
            if (   GetConsoleMode(stdin_handle,&console_mode)
                && GetConsoleMode(stdout_handle,&console_mode)
               )
              same_tty = TRUE;
          #endif
        }
        end_system_call();
        #ifdef HAVE_TERMINAL3
        if (rl_present_p && same_tty) {
          # Baue einen TERMINAL3-Stream:
          pushSTACK(make_ssstring(80)); # Zeilenbuffer allozieren
          pushSTACK(make_ssstring(80)); # Zeilenbuffer allozieren
          pushSTACK(allocate_handle(stdout_handle));
          pushSTACK(allocate_handle(stdin_handle));
          # neuen Stream allozieren:
          var object stream =
            allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal_len,sizeof(strm_unbuffered_extrafields_struct));
            # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
          # und füllen:
          var Stream s = TheStream(stream);
            #ifdef UNICODE
            s->strm_encoding = O(terminal_encoding);
            #endif
            s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
            s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
            s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
            s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
            s->strm_rd_ch = P(rd_ch_terminal3); # READ-CHAR-Pseudofunktion
            s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
            s->strm_rd_ch_array = P(rd_ch_array_dummy); # READ-CHAR-SEQUENCE-Pseudofunktion
            s->strm_rd_ch_last = NIL; # Lastchar := NIL
            s->strm_wr_ch = P(wr_ch_terminal3); # WRITE-CHAR-Pseudofunktion
            s->strm_wr_ch_array = P(wr_ch_array_terminal3); # WRITE-CHAR-SEQUENCE-Pseudofunktion
            s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
            s->strm_terminal_isatty = S(equal); # stdout=stdin
            s->strm_terminal_ihandle = popSTACK(); # Handle für listen_unbuffered()
            s->strm_terminal_ohandle = popSTACK(); # Handle für Output
            #if 1 # TERMINAL_LINEBUFFERED
            s->strm_terminal_inbuff = popSTACK(); # Zeilenbuffer eintragen, count := 0
            s->strm_terminal_index = Fixnum_0; # index := 0
            #endif
            #if 1 # TERMINAL_OUTBUFFERED
            s->strm_terminal_outbuff = popSTACK(); # Zeilenbuffer eintragen
            #endif
          ChannelStream_buffered(stream) = FALSE;
          ChannelStream_init(stream);
          UnbufferedHandleStream_input_init(stream);
          UnbufferedHandleStream_output_init(stream);
          return stream;
        }
        #endif
        #ifdef HAVE_TERMINAL2
        if (stdin_tty) {
          # Baue einen TERMINAL2-Stream:
          pushSTACK(make_ssstring(80)); # Zeilenbuffer allozieren
          pushSTACK(allocate_handle(stdout_handle));
          pushSTACK(allocate_handle(stdin_handle));
          # neuen Stream allozieren:
          var object stream =
            allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal_len,sizeof(strm_unbuffered_extrafields_struct));
            # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
          # und füllen:
          var Stream s = TheStream(stream);
            #ifdef UNICODE
            s->strm_encoding = O(terminal_encoding);
            #endif
            s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
            s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
            s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
            s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
            s->strm_rd_ch = P(rd_ch_terminal2); # READ-CHAR-Pseudofunktion
            s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
            s->strm_rd_ch_array = P(rd_ch_array_dummy); # READ-CHAR-SEQUENCE-Pseudofunktion
            s->strm_rd_ch_last = NIL; # Lastchar := NIL
            s->strm_wr_ch = P(wr_ch_terminal2); # WRITE-CHAR-Pseudofunktion
            s->strm_wr_ch_array = P(wr_ch_array_terminal2); # WRITE-CHAR-SEQUENCE-Pseudofunktion
            s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
            s->strm_terminal_isatty = (stdin_tty ? (same_tty ? S(equal) : T) : NIL);
            s->strm_terminal_ihandle = popSTACK(); # Handle für listen_unbuffered()
            s->strm_terminal_ohandle = popSTACK(); # Handle für Output
            #if 1 # TERMINAL_LINEBUFFERED
            s->strm_terminal_inbuff = popSTACK(); # Zeilenbuffer eintragen, count := 0
            s->strm_terminal_index = Fixnum_0; # index := 0
            #endif
          ChannelStream_buffered(stream) = FALSE;
          ChannelStream_init(stream);
          UnbufferedHandleStream_input_init(stream);
          UnbufferedHandleStream_output_init(stream);
          return stream;
        }
        #endif
        # Baue einen TERMINAL1-Stream:
        {
          pushSTACK(allocate_handle(stdout_handle));
          pushSTACK(allocate_handle(stdin_handle));
          # neuen Stream allozieren:
          var object stream =
            allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal_len,sizeof(strm_unbuffered_extrafields_struct));
            # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
          # und füllen:
          var Stream s = TheStream(stream);
            #ifdef UNICODE
            s->strm_encoding = O(terminal_encoding);
            #endif
            s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
            s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
            s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
            s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
            s->strm_rd_ch = P(rd_ch_terminal1); # READ-CHAR-Pseudofunktion
            s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
            s->strm_rd_ch_array = P(rd_ch_array_dummy); # READ-CHAR-SEQUENCE-Pseudofunktion
            s->strm_rd_ch_last = NIL; # Lastchar := NIL
            s->strm_wr_ch = P(wr_ch_terminal1); # WRITE-CHAR-Pseudofunktion
            s->strm_wr_ch_array = P(wr_ch_array_terminal1); # WRITE-CHAR-SEQUENCE-Pseudofunktion
            s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
            s->strm_terminal_isatty = (stdin_tty ? (same_tty ? S(equal) : T) : NIL);
            s->strm_terminal_ihandle = popSTACK(); # Handle für listen_unbuffered()
            s->strm_terminal_ohandle = popSTACK(); # Handle für Output
          ChannelStream_buffered(stream) = FALSE;
          ChannelStream_init(stream);
          UnbufferedHandleStream_input_init(stream);
          UnbufferedHandleStream_output_init(stream);
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
    {
      pushSTACK(stream);
      fehler(error,
             GETTEXT("RAW mode not supported on ~")
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
    {
      if (!oldterm_initialized) {
        if (!( tcgetattr(stdout_handle,&oldtermio) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        oldterm_initialized = TRUE;
      }
      var struct termios newtermio;
      newtermio = oldtermio;
      newtermio.c_iflag &= ( /* IXON|IXOFF|IXANY| */ ISTRIP|IGNBRK);
      /* newtermio.c_oflag &= ~OPOST; */ # Curses stört sich dran!
      newtermio.c_lflag &= ISIG;
      {
        var uintC i;
        for (i=0; i<NCCS; i++)
          newtermio.c_cc[i] = 0;
      }
      newtermio.c_cc[VMIN] = 1;
      newtermio.c_cc[VTIME] = 0;
      if (!( TCSETATTR(stdout_handle,TCSAFLUSH,&newtermio) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); }
      }
    }
  local void term_unraw()
    {
      if (oldterm_initialized) {
        if (!( TCSETATTR(stdout_handle,TCSAFLUSH,&oldtermio) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
      }
    }
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
    {
      if (!oldterm_initialized) {
        if (!( ioctl(stdout_handle,TCGETA,&oldtermio) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        oldterm_initialized = TRUE;
      }
      var struct termio newtermio;
      newtermio = oldtermio;
      newtermio.c_iflag &= ( /* IXON|IXOFF|IXANY| */ ISTRIP|IGNBRK);
      /* newtermio.c_oflag &= ~OPOST; */ # Curses stört sich dran!
      newtermio.c_lflag &= ISIG;
      {
        var uintC i;
        for (i=0; i<NCCS; i++)
          newtermio.c_cc[i] = 0;
      }
      newtermio.c_cc[VMIN] = 1;
      newtermio.c_cc[VTIME] = 0;
      if (!( ioctl(stdout_handle,TCSETAF,&newtermio) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); }
      }
    }
  local void term_unraw()
    {
      if (oldterm_initialized) {
        if (!( ioctl(stdout_handle,TCSETAF,&oldtermio) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
      }
    }
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
    {
      if (!oldterm_initialized) {
        if (!( ioctl(stdout_handle,TIOCGETP,&oldsgttyb) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        if (!( ioctl(stdout_handle,TIOCGETC,&oldtchars) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        #ifdef TIOCSLTC
        if (!( ioctl(stdout_handle,TIOCGLTC,&oldltchars) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        #endif
        oldterm_initialized = TRUE;
      }
      {
        var struct sgttyb newsgttyb;
        newsgttyb = oldsgttyb;
        newsgttyb.sg_flags |= CBREAK;
        newsgttyb.sg_flags &= ~(CRMOD|ECHO|XTABS);
        if (!( ioctl(stdout_handle,TIOCSETP,&newsgttyb) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
      }
      {
        var struct tchars newtchars;
        var local union {
          char a [sizeof(struct tchars)];
          struct tchars b;
        } zero_tchars = {{0,}};
        newtchars = zero_tchars.b;
        if (!( ioctl(stdout_handle,TIOCSETC,&newtchars) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
      }
      #ifdef TIOCSLTC
      {
        var struct ltchars newltchars;
        var local union {
          char a [sizeof(struct ltchars)];
          struct ltchars b;
        } zero_ltchars = {{0,}};
        newltchars = zero_ltchars.b;
        if (!( ioctl(stdout_handle,TIOCSLTC,&newltchars) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
      }
      #endif
    }
  local void term_unraw()
    {
      if (oldterm_initialized) {
        if (!( ioctl(stdout_handle,TIOCSETP,&oldsgttyb) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        if (!( ioctl(stdout_handle,TIOCSETC,&oldtchars) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        #ifdef TIOCSLTC
        if (!( ioctl(stdout_handle,TIOCSLTC,&oldltchars) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        #endif
      }
    }
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
  {
    if (terminal_raw) {
      term_unraw();
      terminal_raw = FALSE;
    }
  }

LISPFUN(terminal_raw,2,1,norest,nokey,0,NIL)
  {
    var object errorp = popSTACK();
    var object flag = popSTACK();
    var object stream = popSTACK();
    if (!streamp(stream))
      fehler_stream(stream);
    while (builtin_stream_p(stream)
           && TheStream(stream)->strmtype == strmtype_synonym) { # Synonym-Stream verfolgen
      var object sym = TheStream(stream)->strm_synonym_symbol;
      stream = Symbol_value(sym);
      if (!streamp(stream))
        fehler_value_stream(sym);
    }
    value1 = NIL;
    if (builtin_stream_p(stream)
        && TheStream(stream)->strmtype == strmtype_terminal) {
      # der Terminal-Stream
      if (!nullp(TheStream(stream)->strm_terminal_isatty)) {
        # Terminal
        value1 = (terminal_raw ? T : NIL);
        begin_system_call();
        if (!nullp(flag)) {
          # Umschalten in cbreak/noecho-Modus:
          term_raw(); terminal_raw = TRUE;
        } else {
          # Umschalten in nocbreak/echo-Modus:
          term_unraw(); terminal_raw = FALSE;
        }
        end_system_call();
      }
    }
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
  {
    if (!(terminal_mode == 0)) {
      begin_system_call(); SetMode(stdin_handle,0); end_system_call();
      terminal_mode = 0;
    }
  }

LISPFUN(terminal_raw,2,1,norest,nokey,0,NIL)
  {
    var object errorp = popSTACK();
    var object flag = popSTACK();
    var object stream = popSTACK();
    if (!streamp(stream))
      fehler_stream(stream);
    while (builtin_stream_p(stream)
           && TheStream(stream)->strmtype == strmtype_synonym) { # Synonym-Stream verfolgen
      var object sym = TheStream(stream)->strm_synonym_symbol;
      stream = Symbol_value(sym);
      if (!streamp(stream))
        fehler_value_stream(sym);
    }
    if (!(builtin_stream_p(stream)
          && (TheStream(stream)->strmflags & strmflags_open_B))) # Stream geschlossen?
      fehler_illegal_streamop(S(terminal_raw),stream);
    value1 = NIL;
    var LONG new_mode = (nullp(flag) ? 0 : 1);
    var LONG success;
    if (builtin_stream_p(stream)
        && ((TheStream(stream)->strmtype == strmtype_terminal) # der Terminal-Stream
            || (TheStream(stream)->strmtype == strmtype_file # ein ungebufferter File-Stream
                && !ChannelStream_buffered(stream)
       )   )   ) {
      if (!nullp(TheStream(stream)->strm_isatty)) {
        if (TheStream(stream)->strmtype == strmtype_terminal) {
          # Terminal
          value1 = (terminal_mode ? T : NIL);
          if (new_mode == terminal_mode) {
            success = TRUE;
          } else {
            begin_system_call();
            success = SetMode(stdin_handle,new_mode);
            end_system_call();
            terminal_mode = new_mode;
          }
        } else {
          # unbuffered File-Stream
          value1 = (UnbufferedStream_rawp(stream) ? T : NIL);
          if (new_mode == UnbufferedStream_rawp(stream)) {
            success = TRUE;
          } else {
            begin_system_call();
            success = SetMode(TheHandle(TheStream(stream)->strm_ichannel),new_mode);
            end_system_call();
            UnbufferedStream_rawp(stream) = new_mode;
          }
        }
      } else {
        success = TRUE;
      }
    } else {
      success = FALSE;
    }
    if (!success && (!eq(errorp,unbound) && !nullp(errorp)))
      fehler_terminal_raw(stream);
    mv_count=1;
  }

#endif # AMIGAOS

#endif # UNIX || AMIGAOS || RISCOS

#endif # (UNIX && !NEXTAPP) || MSDOS || AMIGAOS || RISCOS || WIN32_NATIVE

#if !((defined(UNIX) && !defined(NEXTAPP)) || defined(AMIGAOS) || defined(RISCOS))

LISPFUN(terminal_raw,2,1,norest,nokey,0,NIL)
  {
    value1 = NIL; mv_count=1; skipSTACK(3); # Nichts tun
  }

#endif

# Liefert einen interaktiven Terminal-Stream.
# can trigger GC
  local object make_terminal_stream (void);
  local object make_terminal_stream()
    {
      var object stream = make_terminal_stream_();
      # Liste der offenen Streams um stream erweitern:
      pushSTACK(stream);
      {
        var object new_cons = allocate_cons();
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
    {
      if (builtin_stream_p(stream)
          && (TheStream(stream)->strmtype == strmtype_window)
         )
        return;
      pushSTACK(stream);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: argument ~ should be a window stream")
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
local uintB attr_table[2][5] = {
  # monochrome:
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
    {
      __asm__ __volatile__ ( "pushl %%ebx ; "
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
  {
    var union REGS in;
    var union REGS out;
    in.regW.ax = ax;
    {
      var uintB ah = in.regB.ah;
      if (ah==0x06 || ah==0x07) {
        in.regB.bh = attr;
      } else {
        in.regB.bh = 0; # "active page"
        if (ah==0x09 || ah==0x0e)
          in.regB.bl = attr;
      }
    }
    if (cx)
      in.regW.cx = *cx;
    if (dx)
      in.regW.dx = *dx;
    begin_system_call();
    intvideo(&in,&out);
    end_system_call();
    if (dx)
      *dx = out.regW.dx;
    if (cx)
      *cx = out.regW.cx;
  }

global uintW v_cols()
  {
    # determine number of screen columns. Also set screentype according
    # to monochrome/color screen.
    var union REGS in;
    var union REGS out;
    in.regB.ah=0x0f;
    intvideo(&in,&out); # INT 10,0F : get current video state
    var uintB videomode = out.regB.al & 0x7f;
    # Text modes are 0,1,2,3,7, and others (depending on the graphics card).
    # Only modes 0 and 7 are mono. (Well, mode 2 is gray shaded.)
    screentype = (((videomode==0) || (videomode==7))
                  ? 0 # monochrome
                  : 1 # color
                 );
    return out.regB.ah;
  }

local uintW v_rows()
  {
    # Getting the number of rows is hard. Most screens support 25 only,
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
    {
      var uintW line;
      var uintW oldline = 0;
      video(0x0200,NULL,&oldline); # INT 10,02 : set cursor position to (0,0)
      loop {
        video(0x0e0a,NULL,NULL); # INT 10,0E : write LF in teletype mode
        video(0x0300,NULL,&line); # INT 10,03 : read cursor position
        line>>=8;
        if (oldline==line)
          return line+1;
        oldline = line;
      }
    }
  }

# High-level BIOS interface

local uintW LINES;
local uintW COLS;

void v_up()
  {
    # cursor up: determine current position, decrement row, set position
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    dx -= 0x100;
    video(0x0200,NULL,&dx); # INT 10,02 : set cursor position
  }

#if 1

void v_cb()
  {
    # cursor big: set begin scan to end scan - 4
    var uintW cx;
    video(0x0300,&cx,NULL); # INT 10,03 : read cursor position
    cx=((cx&0xff)|(((cx&0xff)-4)<<8));
    video(0x0100,&cx,NULL); # INT 10,01 : set cursor type
  }

void v_cs()
  {
    # cursor small: set begin scan to end scan - 1
    var uintW cx;
    video(0x0300,&cx,NULL); # INT 10,03 : read cursor position
    cx=((cx&0xff)|(((cx&0xff)-1)<<8));
    video(0x0100,&cx,NULL); # INT 10,01 : set cursor type
  }

#endif

void v_ce()
  {
    # clear to end: get cursor position and emit the aproppriate number
    # of spaces, without moving cursor.
    var uintW cx;
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    cx = COLS - (dx&0xff);
    video(0x0920,&cx,NULL); # INT 10,09 : write character at cursor, cx times 0x20
  }

void v_cl()
  {
    # clear screen: clear all and set cursor home
    var uintW cx = 0;
    var uintW dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0600,&cx,&dx); # INT 10,06 : scroll active page up
    dx = 0;
    video(0x0200,&cx,&dx); # INT 10,02 : set cursor position
  }

void v_cd()
  {
    # clear to bottom: get position, clear to eol, clear next line to end
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
  {
    # add line: scroll rest of screen down
    var uintW cx;
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    cx = (dx&0xff00);
    dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0701,&cx,&dx); # INT 10,06 : scroll active page down
  }

void v_dl()
  {
    # delete line: scroll rest up
    var uintW cx;
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    cx = (dx&0xff00) /* + 0x100 */ ;
    dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0601,&cx,&dx); # INT 10,06 : scroll active page up
  }

void v_sr()
  {
    # scroll reverse: scroll whole screen
    var uintW cx = 0;
    var uintW dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0701,&cx,&dx); # INT 10,06 : scroll active page down
  }

void v_move(y,x)
  var uintW y;
  var uintW x;
  {
    # set cursor
    var uintW dx = (y<<8)+x;
    video(0x0200,NULL,&dx); # INT 10,02 : set cursor position
  }

uintW v_put(ch)
  var uintW ch;
  {
    # put character:
    # put attribute and char (no scroll!), then update cursor position.
    var uintW cx=1;
    ch &= 0xff;
    if (ch==NL) {
      video(0x0e00|CR,NULL,NULL); # INT 10,0E : write in teletype mode
      video(0x0e00|LF,NULL,NULL); # INT 10,0E : write in teletype mode
    } else {
      video(0x0900|ch,&cx,NULL); # INT 10,09 : write character at cursor
      # cursor right: determine current position, increment column, set position
      var uintW dx;
      video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
      dx += 0x1; # increment column
      if ((dx & 0xff) == COLS) { # at right margin?
        dx &= 0xff00; # set column to 0
        dx += 0x100; # increment row
        if ((dx >> 8) == LINES) # at bottom margin?
          goto no_scroll; # do not scroll at right bottom corner!!
      }
      video(0x0200,NULL,&dx); # INT 10,02 : set cursor position
     no_scroll: ;
    }
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
    {
      if (!charp(ch)) # ch sollte Character sein
        fehler_wr_char(*stream_,ch);
      var uintB c = as_cint(char_code(ch)); # FIXME: This should take into account the encoding.
      # Code c übers BIOS auf den Bildschirm ausgeben:
      v_put(c);
    }

LISPFUNN(make_window,0)
  {
    var object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+0,0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
      s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
      s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_error); # READ-CHAR unmöglich
      s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
      s->strm_rd_ch_array = P(rd_ch_array_error); # READ-CHAR unmöglich
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_array = P(wr_ch_array_dummy); # WRITE-CHAR-SEQUENCE-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
    LINES = v_rows(); COLS = v_cols();
    screenattr = 0; attr = attr_table[screentype][screenattr];
    v_cs();
    value1 = stream; mv_count=1;
  }

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var object stream;
    {
      v_cs();
      attr = BG(col_black) | FG(col_white); v_cl(); # clear screen black
    }

LISPFUNN(window_size,1)
  {
    check_window_stream(popSTACK());
    value1 = fixnum(LINES);
    value2 = fixnum(COLS);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  {
    check_window_stream(popSTACK());
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    value1 = fixnum(dx>>8);
    value2 = fixnum(dx&0xff);
    mv_count=2;
  }

LISPFUNN(set_window_cursor_position,3)
  {
    check_window_stream(STACK_2);
    var uintL line = posfixnum_to_L(STACK_1);
    var uintL column = posfixnum_to_L(STACK_0);
    if ((line < (uintL)LINES) && (column < (uintL)COLS))
      v_move(line,column);
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }

LISPFUNN(clear_window,1)
  {
    check_window_stream(popSTACK());
    v_cl();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  {
    check_window_stream(popSTACK());
    v_cd();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  {
    check_window_stream(popSTACK());
    v_ce();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  {
    check_window_stream(popSTACK());
    v_dl();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  {
    check_window_stream(popSTACK());
    v_al();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  {
    check_window_stream(popSTACK());
    screenattr = 1; attr = attr_table[screentype][screenattr];
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  {
    check_window_stream(popSTACK());
    screenattr = 0; attr = attr_table[screentype][screenattr];
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  {
    check_window_stream(popSTACK());
    v_cb();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  {
    check_window_stream(popSTACK());
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

local uintB attr_table[2][5] = {
  # monochrome:
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
    {
      if (!charp(ch)) # ch sollte Character sein
        fehler_wr_char(*stream_,ch);
      var uintB c = as_cint(char_code(ch)); # FIXME: This should take into account the encoding.
      # Code c über die Video-Library auf den Bildschirm ausgeben:
      if (c==NL) {
        v_putc(c);
      } else {
        var int current_x;
        var int current_y;
        v_getxy(&current_x,&current_y); # get current cursor position
        if ((current_x==COLS-1) && (current_y==LINES-1))
          v_putn(c,1); # do not scroll at right bottom corner!!
        else
          v_putc(c);
      }
    }

LISPFUNN(make_window,0)
  {
    var object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+0,0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
      s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
      s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_error); # READ-CHAR unmöglich
      s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
      s->strm_rd_ch_array = P(rd_ch_array_error); # READ-CHAR unmöglich
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_array = P(wr_ch_array_dummy); # WRITE-CHAR-SEQUENCE-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
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
    {
      v_gotoxy(0,0); # Cursor home
      v_attrib(screentype==0 ? BW_NORMAL : (B_BLACK | F_WHITE));
      v_putn(' ',LINES*COLS); # Bildschirm löschen
      v_ctype(cursor_scanlines_start,cursor_scanlines_end); # Cursorform zurücksetzen
    }

LISPFUNN(window_size,1)
  {
    check_window_stream(popSTACK());
    value1 = fixnum((uintW)LINES);
    value2 = fixnum((uintW)COLS);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  {
    check_window_stream(popSTACK());
    var int current_x;
    var int current_y;
    v_getxy(&current_x,&current_y); # get current cursor position
    value1 = fixnum((uintW)current_y);
    value2 = fixnum((uintW)current_x);
    mv_count=2;
  }

LISPFUNN(set_window_cursor_position,3)
  {
    check_window_stream(STACK_2);
    var uintL line = posfixnum_to_L(STACK_1);
    var uintL column = posfixnum_to_L(STACK_0);
    if ((line < (uintL)LINES) && (column < (uintL)COLS))
      v_gotoxy((int)column,(int)line);
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }

LISPFUNN(clear_window,1)
  {
    check_window_stream(popSTACK());
    v_gotoxy(0,0);
    v_clear();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  {
    check_window_stream(popSTACK());
    var int current_x;
    var int current_y;
    v_getxy(&current_x,&current_y); # get current cursor position
    v_putn(' ',COLS*(LINES-current_y)-current_x);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  {
    check_window_stream(popSTACK());
    v_clreol();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  {
    check_window_stream(popSTACK());
    v_delline(1);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  {
    check_window_stream(popSTACK());
    v_insline(1);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  {
    check_window_stream(popSTACK());
    v_attrib(attr_table[screentype][1]);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  {
    check_window_stream(popSTACK());
    v_attrib(attr_table[screentype][0]);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  {
    check_window_stream(popSTACK());
    # cursor big: set begin scan to end scan - 4
    v_ctype(cursor_scanlines_end-4,cursor_scanlines_end);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  {
    check_window_stream(popSTACK());
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
     restart_it:
      var int ergebnis = write(stdout_handle,&c,1); # Zeichen auszugeben versuchen
      if (ergebnis<0) {
        if (errno==EINTR)
          goto restart_it;
        OS_error(); # Error melden
      }
      if (ergebnis==0) { # nicht erfolgreich?
        pushSTACK(var_stream(S(terminal_io),0)); # Wert für Slot PATHNAME von FILE-ERROR
        fehler(file_error,
               GETTEXT("cannot output to standard output")
              );
      }
    }

# Ausgabe eines Capability-Strings.
  local void out_capstring (const char* s);
  local void out_capstring(s)
    var const char* s;
    {
      if (!(s==NULL)) # Absichern gegen nicht vorhandene Capability
        tputs(s,1,(outcharfun_t) &out_char);
    }

# Ausgabe eines Capability-Strings mit einem Argument.
  local void out_cap1string (const char* s, int arg);
  local void out_cap1string(s,arg)
    var const char* s;
    var int arg;
    {
      if (!(s==NULL)) # Absichern gegen nicht vorhandene Capability
        tputs(tgoto(s,0,arg),1,(outcharfun_t) &out_char);
    }

# Kosten der Ausführung einer Capability:
  #define EXPENSIVE 1000
  local uintC cost_counter; # Zähler
  # Funktion, die nicht ausgibt, sondern nur zählt:
  local void count_char (char c);
  local void count_char(c)
    var char c;
    {
      cost_counter++;
    }
  # Berechnet die Kosten der Ausgabe einer Capability:
  local uintC cap_cost (const char* s);
  local uintC cap_cost(s)
    var const char* s;
    {
      if (s==NULL) {
        return EXPENSIVE; # Capability nicht vorhanden
      } else {
        cost_counter = 0;
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
typedef struct {
  uintB** image; # image[y][x] ist das Zeichen an Position (x,y)
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
} win;

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
    {
      if (flag) {
        # Einschalten
        if (!insert)
          out_capstring(IMcap);
      } else {
        # Ausschalten
        if (insert)
          out_capstring(EIcap);
      }
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
    {
      var uintB old_attr = term_attr;
      if (old_attr == new_attr)
        return;
      if (   ((old_attr & A_SO) && !(new_attr & A_SO))
          || ((old_attr & A_US) && !(new_attr & A_US))
          || ((old_attr & A_BL) && !(new_attr & A_BL))
          || ((old_attr & A_BD) && !(new_attr & A_BD))
          || ((old_attr & A_DI) && !(new_attr & A_DI))
          || ((old_attr & A_RV) && !(new_attr & A_RV))
         ) {
        # Muss Attribute ausschalten.
        out_capstring(UEcap); # alle aus
        out_capstring(SEcap);
        out_capstring(MEcap);
        if (new_attr & A_SO) out_capstring(SOcap); # und selektiv wieder an
        if (new_attr & A_US) out_capstring(UScap);
        if (new_attr & A_BL) out_capstring(MBcap);
        if (new_attr & A_BD) out_capstring(MDcap);
        if (new_attr & A_DI) out_capstring(MHcap);
        if (new_attr & A_RV) out_capstring(MRcap);
      } else {
        # selektiv einschalten:
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
    {
      if (term_charset==new)
        return;
      if (ISO2022) {
        out_char(ESC); out_char('('); out_char(new==ASCII ? 'B' : new); /*)*/
      }
      term_charset = new;
    }
  # Charset Nr. n auf c schalten:
  local void choose_charset (uintB c, uintC n);
  local void choose_charset(c,n)
    var uintB c;
    var uintC n;
    {
      if (c=='B')
        c = ASCII;
      if (curr->charsets[n] == c)
        return;
      curr->charsets[n] = c;
      if (curr->curr_charset == n) # der aktuelle?
        change_charset(c);
    }
  # Charset Nr. n aktuell machen:
  local void set_curr_charset (uintC n);
  local void set_curr_charset(n)
    var uintC n;
    {
      if (curr->curr_charset == n)
        return;
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
    {
      if (AM && (y==rows-1) && (x2==cols)) # rechte untere Ecke kann scrollen?
        return EXPENSIVE;
      var int dx = x2-x1;
      if (dx==0)
        return 0;
      #if WANT_ATTR
      {
        var uintB* p = &curr->attr[y][x1];
        var uintC count;
        dotimespC(count,dx, {
         if (!(*p++ == term_attr)) # Attribut-Wechsel nötig?
           return EXPENSIVE;
        });
      }
      #endif
      #if WANT_CHARSET
      {
        var uintB* p = &curr->font[y][x1];
        var uintC count;
        dotimespC(count,dx, {
          if (!(*p++ == term_charset)) # Font-Wechsel nötig?
            return EXPENSIVE;
        });
      }
      #endif
      var uintC cost = dx;
      #if WANT_INSERT
      if (curr->insert)
        cost += EIcost + IMcost;
      #endif
      return cost;
    }

# Bewegt den Cursor von Position (y1,x1) an Position (y2,x2).
# (x1,y1) = (-1,-1) falls aktuelle Position unbekannt.
  local void gofromto (int y1, int x1, int y2, int x2);
  local void gofromto(y1,x1,y2,x2)
    var int y1;
    var int x1;
    var int y2;
    var int x2;
    {
      if (x2==cols) { # Cursor an den rechten Rand?
        x2--; out_capstring(tgoto(CMcap,x2,y2)); return; # Bleibt in der letzten Spalte
      }
      if (x1==cols) { # Cursor ist am rechten Rand?
        out_capstring(tgoto(CMcap,x2,y2)); return; # absolut adressieren
      }
      var int dy = y2-y1;
      var int dx = x2-x1;
      if ((dy==0) && (dx==0))
        return;
      if ((y1==-1) || (x1==-1) || (y2 > curr->bot) || (y2 < curr->top)) {
        out_capstring(tgoto(CMcap,x2,y2)); return;
      }
      var enum { MX_NONE, MX_LE, MX_RI, MX_RW, MX_CR } mx = MX_NONE;
      var enum { MY_NONE, MY_UP, MY_DO } my = MY_NONE;
      # Möglichkeit 1: mit CMcap
      var uintC CMcost = cap_cost(tgoto(CMcap,x2,y2));
      # Möglichkeit 2: mit getrennten x- und y-Bewegungen:
      var uintC xycost = 0;
      if (dx > 0) {
        var uintC cost1 = rewrite_cost(y1,x1,x2);
        var uintC cost2 = dx * NDcost;
        if (cost1 < cost2) {
          mx = MX_RW; xycost += cost1;
        } else {
          mx = MX_RI; xycost += cost2;
        }
      } elif (dx < 0) {
        mx = MX_LE; xycost += (-dx) * BCcost;
      }
      if (!(dx==0)) {
        var uintC cost1 = CRcost + rewrite_cost(y1,0,x2);
        if (cost1 < xycost) {
          mx = MX_CR; xycost = cost1;
        }
      }
      if (dy > 0) {
        my = MY_DO; xycost += dy * DOcost;
      } elif (dy < 0) {
        my = MY_UP; xycost += (-dy) * UPcost;
      }
      if (xycost >= CMcost) {
        out_capstring(tgoto(CMcap,x2,y2)); return;
      }
      if (!(mx==MX_NONE)) {
        if ((mx==MX_LE) || (mx==MX_RI)) {
          var const char* s;
          if (mx==MX_LE) {
            dx = -dx; s = BCcap;
          } else {
            s = NDcap;
          }
          do {
            out_capstring(s);
          } until (--dx == 0);
        } else {
          if (mx==MX_CR) {
            out_capstring(CRcap); x1=0;
          }
          # Hiervon wurden die Kosten mit rewrite_cost berechnet:
          if (x1<x2) {
            #if WANT_INSERT
            if (curr->insert)
              set_insert_mode(FALSE);
            #endif
            {
              var uintB* ptr = &curr->image[y1][x1];
              var uintC count;
              dotimespC(count,x2-x1, { out_char(*ptr++); });
            }
            #if WANT_INSERT
            if (curr->insert)
              set_insert_mode(TRUE);
            #endif
          }
        }
      }
      if (!(my==MY_NONE)) {
        var const char* s;
        if (my==MY_UP) {
          dy = -dy; s = UPcap;
        } else {
          s = DOcap;
        }
        do {
          out_capstring(s);
        } until (--dy == 0);
      }
    }

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
      {
        if (AM && (y == rows-1) && (x2 == cols))
          x2--;
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
        while (x < x2) {
          if (!((*nsp==*osp)
                #if WANT_ATTR
                && (*nap==*oap) && (*nap==a)
                #endif
                #if WANT_CHARSET
                && (*nfp==*nap) && (*nfp==f)
                #endif
             ) ) {
            gofromto(last_y,last_x,y,x);
            #if WANT_ATTR
            a = *nap;
            if (!(a==term_attr))
              change_attr(a);
            #endif
            #if WANT_CHARSET
            f = *nfp;
            if (!(f==term_charset))
              change_charset(f);
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
      }
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
        if (curr->insert)
          set_insert_mode(FALSE);
        #endif
        #if WANT_ATTR
        var uintB saved_attr = term_attr;
        change_attr(0);
        #endif
        #if WANT_CHARSET
        var uintB saved_charset = term_charset;
        change_charset(ASCII);
        #endif
        last_y = y; last_x = x1;
        redisplay_help RHargs(osp,           oap,          ofp,
                              curr->image[y],curr->attr[y],curr->font[y],
                              y, x1,x2
                             );
        #if WANT_CHARSET
        change_charset(saved_charset);
        #endif
        #if WANT_ATTR
        change_attr(saved_attr);
        #endif
        #if WANT_INSERT
        if (curr->insert)
          set_insert_mode(TRUE);
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
        var uintB saved_attr = term_attr;
        change_attr(0);
        #endif
        #if WANT_CHARSET
        var uintB saved_charset = term_charset;
        change_charset(ASCII);
        #endif
        out_capstring(CLcap); last_x = 0; last_y = 0;
        {
          var uintC y = 0;
          while (y<rows) {
            redisplay_help RHargs(blank,         null,         null,          # old
                                  curr->image[y],curr->attr[y],curr->font[y], # new
                                  y,                                          # Zeile y
                                  0,cols                                      # alle Spalten
                                 );
            y++;
          }
        }
        #if WANT_CHARSET
        change_charset(saved_charset);
        #endif
        #if WANT_ATTR
        change_attr(saved_attr);
        #endif
        #if WANT_INSERT
        if (curr->insert)
          set_insert_mode(TRUE);
        #endif
        gofromto(last_y,last_x,curr->y,curr->x);
      }

# Weitere Cursor-Bewegungen:
#if WANT_CURSOR_MOVE

  local void cursor_right (int n);
  local void cursor_right(n)
    var int n;
    {
      var int x = curr->x;
      if (x==cols)
        return;
      var int new_x = x + n;
      if (new_x > cols)
        new_x = cols;
      gofromto(curr->y,x,curr->y,curr->x = new_x);
    }

  local void cursor_left (int n);
  local void cursor_left(n)
    var int n;
    {
      var int x = curr->x;
      var int new_x = x - n;
      if (new_x < 0)
        new_x = 0;
      gofromto(curr->y,x,curr->y,curr->x = new_x);
    }

  local void cursor_up (int n);
  local void cursor_up(n)
    var int n;
    {
      var int y = curr->y;
      var int new_y = y - n;
      if (new_y < 0)
        new_y = 0;
      gofromto(y,curr->x,curr->y = new_y,curr->x);
    }

  local void cursor_down (int n);
  local void cursor_down(n)
    var int n;
    {
      var int y = curr->y;
      var int new_y = y + n;
      if (new_y >= rows)
        new_y = rows-1;
      gofromto(y,curr->x,curr->y = new_y,curr->x);
    }

#endif

# Backspace (Cursor um 1 nach links, innerhalb einer Zeile)
#if WANT_CURSOR_BACKSPACE
  local void cursor_backspace (void);
  local void cursor_backspace()
    {
      if (curr->x > 0) {
        if (curr->x < cols) {
          if (BCcap)
            out_capstring(BCcap);
          else
            gofromto(curr->y,curr->x,curr->y,curr->x - 1);
        }
        curr->x = curr->x - 1;
      }
    }
#endif

# Return (Cursor an den Anfang der Zeile)
#if WANT_CURSOR_RETURN
  local void cursor_return (void);
  local void cursor_return()
    {
      if (curr->x > 0) {
        out_capstring(CRcap); curr->x = 0;
      }
    }
#endif

# Hilfroutinen zum Scrollen:
#if WANT_CURSOR_LINEFEED || WANT_DELETE_LINE
  local void scroll_up_help (uintB** pp, uintB filler);
  local void scroll_up_help(pp,filler)
    var uintB** pp;
    var uintB filler;
    {
      # pp[top..bot] um eins nach links verschieben,
      # pp[top] herausnehmen, löschen und als pp[bot] wieder einhängen:
      pp = &pp[curr->top];
      var uintC count;
      var uintB* tmp = *pp;
      dotimesC(count,curr->bot - curr->top, { pp[0] = pp[1]; pp++; } );
      {
        var uintB* p = tmp;
        dotimesC(count,cols, { *p++ = filler; } );
      }
      *pp = tmp;
    }
  local void scroll_up (void);
  local void scroll_up()
    {
      scroll_up_help(curr->image,' ');
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
    {
      # pp[top..bot] um eins nach rechts verschieben,
      # pp[top] herausnehmen, löschen und als pp[bot] wieder einhängen:
      pp = &pp[curr->bot];
      var uintC count;
      var uintB* tmp = *pp;
      dotimesC(count,curr->bot - curr->top, { pp[0] = pp[-1]; pp--; } );
      {
        var uintB* p = tmp;
        dotimesC(count,cols, { *p++ = filler; } );
      }
      *pp = tmp;
    }
  local void scroll_down (void);
  local void scroll_down()
    {
      scroll_down_help(curr->image,' ');
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
    {
      if (curr->y == curr->bot)
        scroll_up();
      elif (curr->y < rows-1)
        curr->y++;
      out_capstring(NLcap);
    }
#endif

# Reverse Linefeed (Cursor um 1 nach oben):
#if WANT_CURSOR_REVLINEFEED
  local void cursor_revlinefeed (void);
  local void cursor_revlinefeed()
    {
      if (curr->y == curr->top) {
        scroll_down();
        if (SRcap) {
          out_capstring(SRcap);
        } elif (ALcap) {
          gofromto(curr->top,curr->x,curr->top,0); # Cursor nach links
          out_capstring(ALcap);
          gofromto(curr->top,0,curr->top,curr->x); # Cursor wieder zurück
        } else {
          redisplay();
        }
      } elif (curr->y > 0) {
        cursor_up(1);
      }
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
    {
      var int n = x2-x1;
      if (n>0) {
        {
          var uintB* sp = &curr->image[y][x1];
          var uintC count;
          dotimespC(count,n, { *sp++ = ' '; } );
        }
        #if WANT_ATTR
        {
          var uintB* ap = &curr->attr[y][x1];
          var uintC count;
          dotimespC(count,n, { *ap++ = 0; } );
        }
        #endif
        #if WANT_CHARSET
        {
          var uintB* fp = &curr->font[y][x1];
          var uintC count;
          dotimespC(count,n, { *fp++ = 0; } );
        }
        #endif
      }
    }
#endif

# Bildschirm löschen:
#if WANT_CLEAR_SCREEN
  local void clear_screen (void);
  local void clear_screen()
    {
      out_capstring(CLcap);
      var uintC y = 0;
      while (y<rows) { cleared_linepart(y,0,cols); y++; }
    }
#endif

# Stück einer Zeile löschen:
#if WANT_CLEAR_FROM_BOS || WANT_CLEAR_TO_EOS || WANT_CLEAR_LINE || WANT_CLEAR_FROM_BOL || WANT_CLEAR_TO_EOL
  local void clear_linepart (int y, int x1, int x2);
  local void clear_linepart(y,x1,x2)
    var int y;
    var int x1;
    var int x2;
    {
      var int n = x2-x1;
      if (n>0) {
        {
          var uintB* sp = &curr->image[y][x1];
          var uintC count;
          dotimespC(count,n, { *sp++ = ' '; } );
        }
        #if WANT_ATTR
        {
          var uintB* ap = &curr->attr[y][x1];
          var uintC count;
          dotimespC(count,n, { *ap++ = 0; } );
        }
        #endif
        #if WANT_CHARSET
        {
          var uintB* fp = &curr->font[y][x1];
          var uintC count;
          dotimespC(count,n, { *fp++ = 0; } );
        }
        #endif
        if ((x2==cols) && CEcap) {
          gofromto(curr->y,curr->x,y,x1); curr->y = y; curr->x = x1;
          out_capstring(CEcap);
        } else {
          if ((x2==cols) && (y==rows-1) && AM)
            n--;
          if (n>0) {
            #if WANT_ATTR
            var uintB saved_attr = term_attr;
            change_attr(0);
            #endif
            #if WANT_CHARSET
            var uintB saved_charset = term_charset;
            change_charset(ASCII);
            #endif
            #if WANT_INSERT
            if (curr->insert)
              set_insert_mode(FALSE);
            #endif
            gofromto(curr->y,curr->x,y,x1);
            {
              var uintC count;
              dotimespC(count,n, { out_char(' '); } );
            }
            curr->y = y; curr->x = x1+n;
            #if WANT_CHARSET
            change_charset(saved_charset);
            #endif
            #if WANT_ATTR
            change_attr(saved_attr);
            #endif
            #if WANT_INSERT
            if (curr->insert)
              set_insert_mode(TRUE);
            #endif
          }
        }
      }
    }
#endif

# Bildschirm bis zum Cursor (ausschließlich) löschen:
#if WANT_CLEAR_FROM_BOS
  local void clear_from_BOS (void);
  local void clear_from_BOS()
    {
      var int y0 = curr->y;
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
    {
      var int y0 = curr->y;
      var int x0 = curr->x;
      if (CDcap) {
        out_capstring(CDcap);
        cleared_linepart(y0,x0,cols);
        var int y = y0;
        while (++y < rows) { cleared_linepart(y,0,cols); }
      } else {
        clear_linepart(y0,x0,cols);
        var int y = y0;
        while (++y < rows) { clear_linepart(y,0,cols); }
      }
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Cursorzeile löschen:
#if WANT_CLEAR_LINE
  local void clear_line (void);
  local void clear_line()
    {
      var int y0 = curr->y;
      var int x0 = curr->x;
      clear_linepart(y0,0,cols);
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Cursorzeile bis Cursor (ausschließlich) löschen:
#if WANT_CLEAR_FROM_BOL
  local void clear_from_BOL (void);
  local void clear_from_BOL()
    {
      var int y0 = curr->y;
      var int x0 = curr->x;
      clear_linepart(y0,0,x0);
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Cursorzeile ab Cursor (einschließlich) löschen:
#if WANT_CLEAR_TO_EOL
  local void clear_to_EOL (void);
  local void clear_to_EOL()
    {
      var int y0 = curr->y;
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
    {
      if (cols > 0) {
        {
          var uintB* p1 = &curr->image[y][0];
          var uintB* p2 = &old_image_y[0];
          var uintC count;
          dotimespC(count,cols, { *p2++ = *p1++; } );
        }
        #if WANT_ATTR
        {
          var uintB* p1 = &curr->attr[y][0];
          var uintB* p2 = &old_attr_y[0];
          var uintC count;
          dotimespC(count,cols, { *p2++ = *p1++; } );
        }
        #endif
        #if WANT_CHARSET
        {
          var uintB* p1 = &curr->font[y][0];
          var uintB* p2 = &old_font_y[0];
          var uintC count;
          dotimespC(count,cols, { *p2++ = *p1++; } );
        }
        #endif
      }
    }
#endif

# Ein Zeichen einfügen:
#if WANT_INSERT_1CHAR
  local void insert_1char (uintB c);
  local void insert_1char(c)
    var uintB c;
    {
      var int y = curr->y;
      var int x = curr->x;
      if (x==cols)
        x--; # nicht über den rechten Rand schreiben!
      if (ICcap || IMcap) {
        curr->image[y][x] = c;
        #if WANT_ATTR
        curr->attr[y][x] = curr->curr_attr;
        #endif
        #if WANT_CHARSET
        curr->font[y][x] = curr->charsets[curr->curr_charset]; # = term_charset
        #endif
        #if WANT_INSERT
        if (!curr->insert)
        #endif
          set_insert_mode(TRUE);
        out_capstring(ICcap); out_char(c);
        #if WANT_INSERT
        if (!curr->insert)
        #endif
          set_insert_mode(FALSE);
        curr->x = x+1;
      } else {
        # alten Zeileninhalt retten:
        save_line_old(y);
        # neuen Zeileninhalt bilden:
        {
          var uintB* p1 = &curr->image[y][x];
          var uintB* p2 = &old_image[x];
          var uintC count;
          *p1++ = c;
          dotimesC(count,cols-1-x, { *p1++ = *p2++; } );
        }
        #if WANT_ATTR
        {
          var uintB* p1 = &curr->attr[y][x];
          var uintB* p2 = &old_attr[x];
          var uintC count;
          *p1++ = curr->curr_attr;
          dotimesC(count,cols-1-x, { *p1++ = *p2++; } );
        }
        #endif
        #if WANT_CHARSET
        {
          var uintB* p1 = &curr->font[y][x];
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
      }
    }
#endif

# Platz für n Zeichen machen:
#if WANT_INSERT_CHAR
  local void insert_char (uintC n);
  local void insert_char(n)
    var uintC n;
    {
      var int y = curr->y;
      var int x = curr->x;
      if (n > cols-x)
        n = cols-x;
      if (n==0)
        return;
      # alten Zeileninhalt retten:
      save_line_old(y);
      # neuen Zeileninhalt bilden:
      {
        var uintB* p1 = &curr->image[y][x];
        var uintB* p2 = &old_image[x];
        var uintC count;
        dotimespC(count,n, { *p1++ = ' '; } );
        dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
      }
      #if WANT_ATTR
      {
        var uintB* p1 = &curr->attr[y][x];
        var uintB* p2 = &old_attr[x];
        var uintC count;
        dotimespC(count,n, { *p1++ = 0; } );
        dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
      }
      #endif
      #if WANT_CHARSET
      {
        var uintB* p1 = &curr->font[y][x];
        var uintB* p2 = &old_font[x];
        var uintC count;
        dotimespC(count,n, { *p1++ = 0; } );
        dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
      }
      #endif
      if (CICcap && (n > 1)) {
        #if WANT_INSERT
        if (curr->insert)
          set_insert_mode(FALSE);
        #endif
        out_cap1string(CICcap,n);
        {
          var uintC count;
          dotimespC(count,n, { out_char(' '); } );
        }
        #if WANT_INSERT
        if (curr->insert)
          set_insert_mode(TRUE);
        #endif
        gofromto(y,x+n,y,x);
      } elif (ICcap || IMcap) {
        #if WANT_INSERT
        if (!curr->insert)
        #endif
          set_insert_mode(TRUE);
        {
          var uintC count;
          dotimespC(count,n, { out_capstring(ICcap); out_char(' '); } );
        }
        #if WANT_INSERT
        if (!curr->insert)
        #endif
          set_insert_mode(FALSE);
        gofromto(y,x+n,y,x);
      } else {
        redisplay_line RLargs(old_image,old_attr,old_font,y,x,cols);
        gofromto(last_y,last_x,y,x);
      }
    }
#endif

# Zeilen einfügen:
#if WANT_INSERT_LINE
  local void insert_line (uintC n);
  local void insert_line(n)
    var uintC n;
    {
      if (n > curr->bot - curr->y + 1)
        n = curr->bot - curr->y + 1;
      if (n==0)
        return;
      var int oldtop = curr->top;
      curr->top = curr->y;
      {
        var uintC count;
        dotimespC(count,n, { scroll_down(); } );
      }
      if (ALcap || CALcap) {
        gofromto(curr->y,curr->x,curr->y,0); # an den Zeilenanfang
        if ((CALcap && (n>1)) || !ALcap) {
          out_cap1string(CALcap,n);
        } else {
          var uintC count;
          dotimespC(count,n, { out_capstring(ALcap); } );
        }
        gofromto(curr->y,0,curr->y,curr->x);
      } elif (CScap && SRcap) {
        out_capstring(tgoto(CScap,curr->bot,curr->top));
        gofromto(-1,-1,curr->top,0);
        {
          var uintC count;
          dotimespC(count,n, { out_capstring(SRcap); } );
        }
        out_capstring(tgoto(CScap,curr->bot,oldtop));
        gofromto(-1,-1,curr->y,curr->x);
      } else {
        redisplay();
      }
      curr->top = oldtop;
    }
#endif

# Lösch-Operationen:

# Characters löschen:
#if WANT_DELETE_CHAR
  local void delete_char (uintC n);
  local void delete_char(n)
    var uintC n;
    {
      var int y = curr->y;
      var int x = curr->x;
      if (n > cols-x)
        n = cols-x;
      if (n==0)
        return;
      # alten Zeileninhalt retten:
      save_line_old(y);
      # neuen Zeileninhalt bilden:
      {
        var uintB* p1 = &curr->image[y][x];
        var uintB* p2 = &old_image[x];
        var uintC count;
        dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
        dotimespC(count,n, { *p1++ = ' '; } );
      }
      #if WANT_ATTR
      {
        var uintB* p1 = &curr->attr[y][x];
        var uintB* p2 = &old_attr[x];
        var uintC count;
        dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
        dotimespC(count,n, { *p1++ = 0; } );
      }
      #endif
      #if WANT_CHARSET
      {
        var uintB* p1 = &curr->font[y][x];
        var uintB* p2 = &old_font[x];
        var uintC count;
        dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
        dotimespC(count,n, { *p1++ = 0; } );
      }
      #endif
      if (CDCcap && ((n>1) || !DCcap)) {
        out_cap1string(CDCcap,n);
      } elif (DCcap) {
        var uintC count;
        dotimespC(count,n, { out_capstring(DCcap); } );
      } else {
        redisplay_line RLargs(old_image,old_attr,old_font,y,x,cols);
        gofromto(last_y,last_x,y,x);
      }
    }
#endif

# Zeilen löschen:
#if WANT_DELETE_LINE
  local void delete_line (uintC n);
  local void delete_line(n)
    var uintC n;
    {
      if (n > curr->bot - curr->y + 1)
        n = curr->bot - curr->y + 1;
      if (n==0)
        return;
      var int oldtop = curr->top;
      curr->top = curr->y;
      {
        var uintC count;
        dotimespC(count,n, { scroll_up(); } );
      }
      if (DLcap || CDLcap) {
        gofromto(curr->y,curr->x,curr->y,0); # an den Zeilenanfang
        if ((CDLcap && (n>1)) || !DLcap) {
          out_cap1string(CDLcap,n);
        } else {
          var uintC count;
          dotimespC(count,n, { out_capstring(DLcap); } );
        }
        gofromto(curr->y,0,curr->y,curr->x);
      } elif (CScap) {
        out_capstring(tgoto(CScap,curr->bot,curr->top));
        gofromto(-1,-1,curr->bot,0);
        {
          var uintC count;
          dotimespC(count,n, { out_capstring(SFcap); } );
        }
        out_capstring(tgoto(CScap,curr->bot,oldtop));
        gofromto(-1,-1,curr->y,curr->x);
      } else {
        redisplay();
      }
      curr->top = oldtop;
    }
#endif

# Ein Zeichen ausgeben:
#if WANT_OUTPUT_1CHAR
  local void output_1char (uintB c);
  local void output_1char(c)
    var uintB c;
    {
      #if WANT_INSERT
      if (curr->insert) {
        insert_1char(c);
        return;
      }
      #endif
      var int y = curr->y;
      var int x = curr->x;
      if (x==cols)
        x--; # nicht über den rechten Rand schreiben!
      curr->image[y][x] = c;
      #if WANT_ATTR
      curr->attr[y][x] = curr->curr_attr;
      #endif
      #if WANT_CHARSET
      curr->font[y][x] = curr->charsets[curr->curr_charset]; # = term_charset
      #endif
      x++;
      if (!(AM && (x==cols) && (curr->y==curr->bot))) # rechte untere Ecke evtl. freilassen
        out_char(c); # Zeichen ausgeben
      curr->x = x; # Cursor rückt um eins weiter
      if (x==cols) # außer wenn er schon ganz rechts war
        gofromto(-1,-1,curr->y,curr->x);
    }
#endif

#if WANT_SAVE

# gespeicherte Cursor-Position:
  local void save_cursor (void);
  local void save_cursor()
    {
      curr->saved_x = curr->x;
      curr->saved_y = curr->y;
      #if WANT_ATTR
      curr->saved_curr_attr = curr->curr_attr;
      #endif
      #if WANT_CHARSET
      curr->saved_curr_charset = curr->curr_charset;
      {
        var uintC i = 0;
        while (i<charset_count) { curr->saved_charsets[i] = curr->charsets[i]; i++; }
      }
      #endif
      curr->saved = TRUE;
    }
  local void restore_cursor (void);
  local void restore_cursor()
    {
      if (curr->saved) {
        gofromto(curr->y,curr->x,curr->saved_y,curr->saved_x);
        curr->y = curr->saved_y; curr->x = curr->saved_x;
        #if WANT_ATTR
        curr->curr_attr = curr->saved_curr_attr;
        change_attr(curr->curr_attr);
        #endif
        #if WANT_CHARSET
        curr->curr_charset = curr->saved_curr_charset;
        {
          var uintC i = 0;
          while (i<charset_count) { curr->charsets[i] = curr->saved_charsets[i]; i++; }
        }
        change_charset(curr->charsets[curr->curr_charset]);
        #endif
      }
    }

#endif

# Initialisiert das Terminal.
# Liefert NULL falls OK, einen Fehlerstring sonst.
  local boolean term_initialized = FALSE;
  local const char * init_term (void);
  local const char * init_term()
    {
      var char tbuf[4096]; # interner Buffer für die Termcap-Routinen
      if (term_initialized)
        return NULL; # schon initialisiert -> OK
      # Terminal-Typ abfragen:
      begin_system_call();
      {
        var const char* s = getenv("TERM");
        if (s==NULL) {
          end_system_call();
          return GETTEXT("environment has no TERM variable");
        }
        if (!(tgetent(tbuf,s)==1)) {
          end_system_call();
          pushSTACK(asciz_to_string(s,O(misc_encoding)));
          return GETTEXT("terminal type ~ unknown to termcap");
        }
      }
      {
        var int i = tgetnum("co");
        cols = (i>0 ? i : 80);
      }
      {
        var int i = tgetnum("li");
        rows = (i>0 ? i : 24);
      }
      #ifdef EMUNIX
      # Obwohl das eigentlich unsauber ist, holen wir uns die aktuelle Bildschirm-
      # größe mit _scrsize().
      {
        var int scrsize[2];
        _scrsize(&!scrsize);
        if (scrsize[0] > 0)
          cols = scrsize[0];
        if (scrsize[1] > 0)
          rows = scrsize[1];
      }
      #endif
      if (tgetflag("hc")) {
        end_system_call();
        return GETTEXT("insufficient terminal: hardcopy terminal");
      }
      if (tgetflag("os")) {
        end_system_call();
        return GETTEXT("insufficient terminal: overstrikes, cannot clear output");
      }
      if (tgetflag("ns")) {
        end_system_call();
        return GETTEXT("insufficient terminal: cannot scroll");
      }
      if (!(CLcap = tgetstr("cl",&tp))) {
        # Könnte CLcap = "\n\n\n\n"; als Default nehmen ('weird HPs')
        end_system_call();
        return GETTEXT("insufficient terminal: cannot clear screen");
      }
      if (!(CMcap = tgetstr("cm",&tp))) {
        end_system_call();
        return GETTEXT("insufficient terminal: cannot position cursor randomly");
      }
      # Capabilities initialisieren:
      AM = tgetflag("am"); if (tgetflag("LP")) AM = FALSE;
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
      if ((tgetnum("sg") > 0) || (tgetnum("ug") > 0)) {
        # Beim Umschalten in Standout-Mode oder beim Umschalten in den
        # Underline-Mode gibt's Leerstellen -> unbrauchbar!
        SOcap = NULL; SEcap = NULL; UScap = NULL; UEcap = NULL;
        MBcap = NULL; MDcap = NULL; MHcap = NULL; MRcap = NULL; MEcap = NULL;
      } else {
        SOcap = tgetstr("so",&tp);
        SEcap = tgetstr("se",&tp);
        UScap = tgetstr("us",&tp);
        UEcap = tgetstr("ue",&tp);
        if (!UScap && !UEcap) { # kein Underline?
          UScap = SOcap; UEcap = SEcap; # nimm Standout als Ersatz
        }
        MBcap = tgetstr("mb",&tp);
        MDcap = tgetstr("md",&tp);
        MHcap = tgetstr("mh",&tp);
        MRcap = tgetstr("mr",&tp);
        MEcap = tgetstr("me",&tp);
        # Does ME also reverse the effect of SO and/or US?  This is not
        # clearly specified by the termcap manual.
        # Anyway, we should at least look whether ME/SE/UE are equal:
        if (UEcap && SEcap && asciz_equal(UEcap,SEcap)) UEcap = NULL;
        if (UEcap && MEcap && asciz_equal(UEcap,MEcap)) UEcap = NULL;
        if (SEcap && MEcap && asciz_equal(SEcap,MEcap)) SEcap = NULL;
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
      if (tgetflag ("in")) { # Insert-Modus unbrauchbar?
        IMcap = NULL; EIcap = NULL;
        #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR
        ICcap = NULL;
        #endif
        #if WANT_INSERT_CHAR
        CICcap = NULL;
        #endif
      }
      if (IMcap && (IMcap[0]==0)) IMcap = NULL; # IMcap leer?
      if (EIcap && (EIcap[0]==0)) EIcap = NULL; # EIcap leer?
      #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR
      if (ICcap && (ICcap[0]==0)) ICcap = NULL; # ICcap leer?
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
      if (DOcap[0]=='\n')
        DOcost = EXPENSIVE;
      #endif
      UPcost = cap_cost(UPcap);
      CRcost = cap_cost(CRcap);
      # Hilfs-Datenstrukturen bereitstellen:
      {
        var uintB* ptr = (uintB*) malloc(cols*sizeof(uintB));
        var uintC count;
        blank = ptr;
        dotimespC(count,cols, { *ptr++ = ' '; } );
      }
      #if WANT_ATTR || WANT_CHARSET
      {
        var uintB* ptr = (uintB*) malloc(cols*sizeof(uintB));
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
    {
      var struct termios oldtermio;
      if (!( tcgetattr(stdout_handle,&oldtermio) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); }
      }
      old_c_oflag = oldtermio.c_oflag;
      oldtermio.c_oflag &= ~ONLCR;
      if (!( TCSETATTR(stdout_handle,TCSAFLUSH,&oldtermio) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); }
      }
    }
  local void term_nlunraw()
    {
      if (old_c_oflag & ONLCR) {
        var struct termios oldtermio;
        if (!( tcgetattr(stdout_handle,&oldtermio) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        oldtermio.c_oflag |= ONLCR;
        if (!( TCSETATTR(stdout_handle,TCSAFLUSH,&oldtermio) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
      }
    }
#elif defined(UNIX_TERM_TERMIO) || defined(EMUNIX)
  static unsigned long old_c_oflag = 0;
  local void term_nlraw()
    {
      var struct termio oldtermio;
      if (!( ioctl(stdout_handle,TCGETA,&oldtermio) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); }
      }
      old_c_oflag = oldtermio.c_oflag;
      oldtermio.c_oflag &= ~ONLCR;
      if (!( ioctl(stdout_handle,TCSETAF,&oldtermio) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); }
      }
    }
  local void term_nlunraw()
    {
      if (old_c_oflag & ONLCR) {
        var struct termio oldtermio;
        if (!( ioctl(stdout_handle,TCGETA,&oldtermio) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        oldtermio.c_oflag |= ONLCR;
        if (!( ioctl(stdout_handle,TCSETAF,&oldtermio) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
      }
    }
#elif defined(UNIX_TERM_SGTTY)
  static unsigned long old_sg_flags = 0;
  local void term_nlraw()
    {
      var struct sgttyb oldsgttyb;
      if (!( ioctl(stdout_handle,TIOCGETP,&oldsgttyb) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); }
      }
      old_sg_flags = oldsgttyb.sg_flags;
      oldsgttyb.sg_flags &= ~CRMOD;
      if (!( ioctl(stdout_handle,TIOCSETP,&oldsgttyb) ==0)) {
        if (!(errno==ENOTTY)) { OS_error(); }
      }
    }
  local void term_nlunraw()
    {
      if (old_sg_flags & CRMOD) {
        var struct sgttyb oldsgttyb;
        if (!( ioctl(stdout_handle,TIOCGETP,&oldsgttyb) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
        oldsgttyb.sg_flags |= CRMOD;
        if (!( ioctl(stdout_handle,TIOCSETP,&oldsgttyb) ==0)) {
          if (!(errno==ENOTTY)) { OS_error(); }
        }
      }
    }
#endif

#endif # NL_HACK

# Beginn des Arbeitens mit diesem Paket:
  local void start_term (void);
  local void start_term()
    {
      #ifdef NL_HACK
      if (NLcap[0] == '\n')
        term_nlraw();
      #endif
      out_capstring (IScap);
      out_capstring (TIcap);
    }

# Ende des Arbeitens mit diesem Paket:
  local void end_term (void);
  local void end_term()
    {
      out_capstring (TEcap);
      out_capstring (IScap);
      #ifdef MSDOS # wie testet man auf Farb-ANSI-Terminal??
      # Auf ANSI-Terminals mit mehreren Farben: TEcap setzt die Farben zurück.
      out_capstring(CLcap); # Bildschirm löschen, diesmal in der normalen Farbe
      #endif
      #ifdef NL_HACK
      if (NLcap[0] == '\n')
        term_nlunraw();
      #endif
    }

# Initialisiert das Window curr.
  local void init_curr (void);
  local void init_curr()
    {
      {
        var uintB** ptr = (uintB**) malloc(rows*sizeof(uintB*));
        var uintC count;
        curr->image = ptr;
        dotimespC(count,rows, { *ptr++ = (uintB*) malloc(cols*sizeof(uintB)); } );
      }
      #if WANT_ATTR
      {
        var uintB** ptr = (uintB**) malloc(rows*sizeof(uintB*));
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
      {
        var uintB** ptr = (uintB**) malloc(rows*sizeof(uintB*));
        var uintC count;
        curr->font = ptr;
        dotimespC(count,rows, { *ptr++ = (uintB*) malloc(cols*sizeof(uintB)); } );
      }
      {
        var uintC i = 0;
        while (i<charset_count) { curr->charsets[i] = ASCII; i++; }
      }
      curr->curr_charset = 0;
      if (ISO2022) {
        out_char(ESC); out_char('('); out_char('B'); /*)*/
      }
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
      if (CScap)
        out_capstring(tgoto(CScap,curr->bot,curr->top));
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
    {
      if (!charp(ch)) # ch sollte Character sein
        fehler_wr_char(*stream_,ch);
      var uintB c = as_cint(char_code(ch)); # FIXME: This should take into account the encoding.
      begin_system_call();
      if (graphic_char_p(as_chart(c))) {
        if (curr->x == cols) {
          cursor_return(); cursor_linefeed(); # Wrap!
        }
        output_1char(c);
      } elif (c == NL) {
        cursor_return(); cursor_linefeed();
      } elif (c == BS) {
        var int x0 = curr->x;
        if (x0>0) {
          var int y0 = curr->y;
          clear_linepart(y0,x0-1,x0);
          gofromto(curr->y,curr->x,y0,x0-1); curr->y = y0; curr->x = x0-1;
        }
      }
      end_system_call();
    }

LISPFUNN(make_window,0)
  {
    var object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+1,0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
      s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
      s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_error); # READ-CHAR unmöglich
      s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
      s->strm_rd_ch_array = P(rd_ch_array_error); # READ-CHAR unmöglich
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_array = P(wr_ch_array_dummy); # WRITE-CHAR-SEQUENCE-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
    # Initialisieren:
    begin_system_call();
    {
      var const char * ergebnis = init_term();
      if (!(ergebnis==NULL))
        fehler(error,ergebnis);
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
    {
      begin_system_call();
      end_term();
      end_system_call();
    }

LISPFUNN(window_size,1)
  {
    check_window_stream(popSTACK());
    value1 = fixnum(rows); # Variablen rows,cols abfragen
    value2 = fixnum(cols);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  {
    check_window_stream(popSTACK());
    value1 = fixnum(curr->y);
    value2 = fixnum(curr->x);
    mv_count=2;
  }

LISPFUNN(set_window_cursor_position,3)
  {
    check_window_stream(STACK_2);
    var uintL line = posfixnum_to_L(STACK_1);
    var uintL column = posfixnum_to_L(STACK_0);
    if ((line < rows) && (column < cols)) {
      begin_system_call();
      gofromto(curr->y,curr->x,line,column); # Cursor positionieren
      curr->y = line; curr->x = column;
      end_system_call();
    }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }

LISPFUNN(clear_window,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    clear_screen();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    clear_to_EOS();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    clear_to_EOL();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    delete_line(1);
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    insert_line(1);
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    change_attr(curr->curr_attr |= A_US);
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    change_attr(curr->curr_attr &= ~A_US);
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  {
    check_window_stream(popSTACK());
    # Cursor ist permanent an!
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  {
    check_window_stream(popSTACK());
    # geht nicht, da Cursor permanent an!
    value1 = NIL; mv_count=0;
  }

#endif # (UNIX && !NEXTAPP) || (EMUNIX_PORTABEL && 0) || RISCOS

#if defined(MAYBE_NEXTAPP) && defined(NEXTAPP)

# Alles unimplementiert.

# Fehlermeldung.
  nonreturning_function(local, fehler_screen, (void));
  local void fehler_screen()
    {
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: package SCREEN is not implemented")
            );
    }

LISPFUNN(make_window,0)
  {
    fehler_screen();
  }

#define close_window(stream)  fehler_screen()

LISPFUNN(window_size,1)
  {
    fehler_screen();
  }

LISPFUNN(window_cursor_position,1)
  {
    fehler_screen();
  }

LISPFUNN(set_window_cursor_position,3)
  {
    fehler_screen();
  }

LISPFUNN(clear_window,1)
  {
    fehler_screen();
  }

LISPFUNN(clear_window_to_eot,1)
  {
    fehler_screen();
  }

LISPFUNN(clear_window_to_eol,1)
  {
    fehler_screen();
  }

LISPFUNN(delete_window_line,1)
  {
    fehler_screen();
  }

LISPFUNN(insert_window_line,1)
  {
    fehler_screen();
  }

LISPFUNN(highlight_on,1)
  {
    fehler_screen();
  }

LISPFUNN(highlight_off,1)
  {
    fehler_screen();
  }

LISPFUNN(window_cursor_on,1)
  {
    fehler_screen();
  }

LISPFUNN(window_cursor_off,1)
  {
    fehler_screen();
  }

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
    {
      if (!charp(ch)) # ch sollte Character sein
        fehler_wr_char(*stream_,ch);
      var uintB c = as_cint(char_code(ch)); # FIXME: This should take into account the encoding.
      begin_system_call();
      if (graphic_char_p(as_chart(c))) { # nur druckbare Zeichen auf den Bildschirm lassen
        addch(c);
      } elif (c == NL) { # NL in CR/LF umwandeln
        addch(CR); addch(LF);
      } else { # etwas ausgeben, damit die Cursorposition stimmt
        addch('?');
      }
      end_system_call();
    }

LISPFUNN(make_window,0)
  {
    var object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+1,0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
      s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
      s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_error); # READ-CHAR unmöglich
      s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
      s->strm_rd_ch_array = P(rd_ch_array_error); # READ-CHAR unmöglich
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_array = P(wr_ch_array_dummy); # WRITE-CHAR-SEQUENCE-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
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
    {
      begin_system_call();
      nocbreak(); echo(); # Input wieder zeilengebuffert, mit Echo
      #if defined(SUN3) || defined(SUN4)
      keypad(stdscr,FALSE); # Funktionstasten-Erkennung wieder ausschalten
      #endif
      endwin(); # Curses abschalten
      end_system_call();
    }

LISPFUNN(window_size,1)
  {
    check_window_stream(popSTACK());
    value1 = fixnum(LINES); # Curses-Variablen LINES, COLS abfragen
    value2 = fixnum(COLS);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  {
    check_window_stream(popSTACK());
    var int y;
    var int x;
    begin_system_call();
    getyx(stdscr,y,x); # (y,x) := cursor position
    end_system_call();
    value1 = fixnum(y);
    value2 = fixnum(x);
    mv_count=2;
  }

LISPFUNN(set_window_cursor_position,3)
  {
    check_window_stream(STACK_2);
    var uintL line = posfixnum_to_L(STACK_1);
    var uintL column = posfixnum_to_L(STACK_0);
    if ((line < LINES) && (column < COLS)) {
      begin_system_call();
      move(line,column); refresh(); # Cursor positionieren
      end_system_call();
    }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }

LISPFUNN(clear_window,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    clear(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    clrtobot(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    clrtoeol(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    deleteln(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  {
    check_window_stream(popSTACK());
    begin_system_call();
    insertln(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  {
    check_window_stream(popSTACK());
    #ifdef A_STANDOUT # geht nur, wenn Curses Attribute verwaltet
    begin_system_call();
    attron(A_STANDOUT); # Attribut A_STANDOUT bei addch() hineinoderieren
    end_system_call();
    #endif
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  {
    check_window_stream(popSTACK());
    #ifdef A_STANDOUT # geht nur, wenn Curses Attribute verwaltet
    begin_system_call();
    attroff(A_STANDOUT); # kein Attribut mehr bei addch() hineinoderieren
    end_system_call();
    #endif
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  {
    check_window_stream(popSTACK());
    # Cursor ist permanent an!
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  {
    check_window_stream(popSTACK());
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
    {
      set_break_sem_1();
      begin_system_call();
      var long ergebnis = Write(stdout_handle,outbuffer,count);
      end_system_call();
      if (ergebnis<0) { OS_error(); } # Error melden
      if (ergebnis<count) # nicht erfolgreich?
        { ?? }
      clr_break_sem_1();
    }

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
    {
      if (!charp(ch)) # ch sollte Character sein
        fehler_wr_char(*stream_,ch);
      var uintB c = as_cint(char_code(ch)); # FIXME: This should take into account the encoding.
      ??
    }

LISPFUNN(make_window,0)
  {
    finish_output_terminal(var_stream(S(terminal_io),strmflags_wr_ch_B)); # evtl. wartendes NL jetzt ausgeben
    var object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+0,0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_error); # READ-BYTE unmöglich
      s->strm_rd_by_array = P(rd_by_array_error); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_error); # WRITE-BYTE unmöglich
      s->strm_wr_by_array = P(wr_by_array_error); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_error); # READ-CHAR unmöglich
      s->strm_pk_ch = P(pk_ch_dummy); # PEEK-CHAR-Pseudofunktion
      s->strm_rd_ch_array = P(rd_ch_array_error); # READ-CHAR unmöglich
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_array = P(wr_ch_array_dummy); # WRITE-CHAR-SEQUENCE-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
    # size: aWSR? aWBR??
    # Wrap off ?? ASM? AWM?
    WR_WINDOW({CSI,'0',0x6D}); # Set Graphics Rendition Normal
    value1 = stream; mv_count=1;
  }

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var object stream;
    {
      # Wrap on ?? ASM? AWM?
      WR_WINDOW({CSI,'0',0x6D}); # Set Graphics Rendition Normal
    }

LISPFUNN(window_size,1)
  {
    check_window_stream(popSTACK());
    value1 = fixnum(window_size.y); ??
    value2 = fixnum(window_size.x); ??
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  {
    check_window_stream(popSTACK());
    # aWSR? CPR??
    value1 = fixnum(_y); ??
    value2 = fixnum(_x); ??
    mv_count=2;
  }

LISPFUNN(set_window_cursor_position,3)
  {
    check_window_stream(STACK_2);
    var uintL line = posfixnum_to_L(STACK_1);
    var uintL column = posfixnum_to_L(STACK_0);
    if ((line < (uintL)window_size.y) && (column < (uintL)window_size.x)) {
      var uintB outbuffer[23]; # Buffer für  CSI <line> ; <column> H
      var uintB* ptr = &outbuffer[sizeof(outbuffer)];
      var uintL count = 0;
      count++; *--ptr = 'H';
      do {
        count++; *--ptr = '0'+(column%10); column = floor(column,10);
      } until (column==0);
      count++; *--ptr = ';';
      do {
        count++; *--ptr = '0'+(line%10); line = floor(line,10);
      } until (line==0);
      count++; *--ptr = CSI;
      wr_window(ptr,count);
    }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }

LISPFUNN(clear_window,1)
  {
    check_window_stream(popSTACK());
    WR_WINDOW({CSI,'0',';','0','H',CSI,'J'});
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  {
    check_window_stream(popSTACK());
    WR_WINDOW({CSI,'J'});
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  {
    check_window_stream(popSTACK());
    WR_WINDOW({CSI,'K'});
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  {
    check_window_stream(popSTACK());
    WR_WINDOW({CSI,'M'});
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  {
    check_window_stream(popSTACK());
    WR_WINDOW({CSI,'L'});
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  {
    check_window_stream(popSTACK());
    WR_WINDOW({CSI,'1',0x6D}); # Set Graphics Rendition Bold
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  {
    check_window_stream(popSTACK());
    WR_WINDOW({CSI,'0',0x6D}); # Set Graphics Rendition Normal
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  {
    check_window_stream(popSTACK());
    # aSCR ??
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  {
    check_window_stream(popSTACK());
    # aSCR ??
    value1 = NIL; mv_count=0;
  }

#endif # AMIGAOS

#endif # SCREEN


#ifdef PRINTER_AMIGAOS

# Printer-Stream
# ==============

# Zusätzliche Komponenten:
  #define strm_printer_handle  strm_other[0]  # Handle von "PRT:"

# FIXME: Should be based on an encoding.

# WRITE-CHAR - Pseudofunktion für Printer-Streams:
  local void wr_ch_printer (const object* stream_, object ch);
  local void wr_ch_printer(stream_,ch)
    var const object* stream_;
    var object ch;
    {
      var object stream = *stream_;
      # ch sollte Character sein:
      if (!charp(ch))
        fehler_wr_char(stream,ch);
      begin_system_call();
      var uintB c = as_cint(char_code(ch)); # FIXME: This should take into account the encoding.
      var long ergebnis = # Zeichen auszugeben versuchen
        Write(TheHandle(TheStream(stream)->strm_printer_handle),&c,1L);
      end_system_call();
      if (ergebnis<0) { OS_error(); } # Error melden
      # ergebnis = Anzahl der ausgegebenen Zeichen (0 oder 1)
      if (ergebnis==0) # nicht erfolgreich?
        fehler_unwritable(S(write_char),stream);
    }

# Schließt einen Printer-Stream.
  local void close_printer (object stream);
  local void close_printer(stream)
    var object stream;
    {
      begin_system_call();
      Close(TheHandle(TheStream(stream)->strm_printer_handle));
      end_system_call();
    }

# UP: Liefert einen Printer-Stream.
# can trigger GC
  local object make_printer_stream (void);
  local object make_printer_stream()
    {
      pushSTACK(allocate_cons()); # Cons für Liste
      pushSTACK(allocate_handle(Handle_NULL)); # Handle-Verpackung
      var object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_printer,strm_len+1,0);
      set_break_sem_4();
      begin_system_call();
      {
        var Handle handle = Open("PRT:",MODE_NEWFILE);
        if (handle==Handle_NULL) { OS_error(); } # Error melden
        end_system_call();
        TheHandle(STACK_0) = handle; # Handle verpacken
      }
      TheStream(stream)->strm_rd_by = P(rd_by_error);
      TheStream(stream)->strm_rd_by_array = P(rd_by_array_error);
      TheStream(stream)->strm_wr_by = P(wr_by_error);
      TheStream(stream)->strm_wr_by_array = P(wr_by_array_error);
      TheStream(stream)->strm_rd_ch = P(rd_ch_error);
      TheStream(stream)->strm_pk_ch = P(pk_ch_dummy);
      TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_error);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_printer);
      TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_dummy);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_printer_handle = popSTACK();
      # Liste der offenen Streams um stream erweitern:
      {
        var object new_cons = popSTACK();
        Car(new_cons) = stream;
        Cdr(new_cons) = O(open_files);
        O(open_files) = new_cons;
      }
      clr_break_sem_4();
      return stream;
    }

LISPFUNN(make_printer_stream,0)
# (SYSTEM::MAKE-PRINTER-STREAM) liefert einen Printer-Stream.
# Für die verstandenen Escape-Sequenzen siehe in PRINTER.DOC.
  {
    value1 = make_printer_stream(); mv_count=1; return;
  }

#endif


#ifdef PIPES

# Pipe-Input-Stream, Pipe-Output-Stream
# =====================================

# Zusätzliche Komponenten:
  # define strm_pipe_pid  strm_field1   # Prozess-Id, ein Fixnum >=0
  #if defined(EMUNIX) && defined(PIPES2)
  #define strm_pipe_other strm_field2   # Pipe-Stream in Gegenrichtung
  #endif

  #ifdef EMUNIX
    local void low_close_pipe (object stream, object handlobj);
    local void low_close_pipe(stream,handlobj)
      var object stream;
      var object handlobj;
      {
        var Handle handle = TheHandle(handlobj);
        #ifdef PIPES2
        if (builtin_stream_p(TheStream(stream)->strm_pipe_other)) {
          # Der andere Pipe-Stream ist noch offen. Wir dürfen nicht pclose()
          # aufrufen, da das ein waitpid() ausführt.
          TheStream(TheStream(stream)->strm_pipe_other)->strm_pipe_other = NIL;
          TheStream(stream)->strm_pipe_other = NIL;
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
    #define low_close_pipe  low_close_handle
  #endif

#if defined(HAVE_SIGNALS) && defined(SIGPIPE)

  # Be careful to disable SIGPIPE during write() to a subprocess.

  local void low_flush_buffered_pipe (object stream, uintL bufflen);
  local void low_flush_buffered_pipe(stream,bufflen)
    var object stream;
    var uintL bufflen;
    {
      begin_system_call();
      writing_to_subprocess = TRUE;
      var sintL ergebnis = # Buffer hinausschreiben
        full_write(TheHandle(TheStream(stream)->strm_buffered_channel), # Handle
                   &TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[0], # Bufferadresse
                   bufflen
                  );
      writing_to_subprocess = FALSE;
      if (ergebnis==bufflen) {
        # alles korrekt geschrieben
        end_system_call(); BufferedStream_modified(stream) = FALSE;
      } else {
        # Nicht alles geschrieben
        if (ergebnis<0) { # Fehler aufgetreten?
          end_system_call(); OS_filestream_error(stream);
        }
        end_system_call();
        fehler_unwritable(TheSubr(subr_self)->name,stream);
      }
    }

#else

  #define low_flush_buffered_pipe  low_flush_buffered_handle

#endif

#define BufferedPipeStream_init(stream)  \
  { BufferedStreamLow_fill(stream) = &low_fill_buffered_handle; \
    BufferedStreamLow_flush(stream) = &low_flush_buffered_pipe; \
  }

# Pipe-Input-Stream
# =================

# Low-level.

  #define UnbufferedPipeStream_input_init(stream)  UnbufferedHandleStream_input_init(stream)

local inline void create_input_pipe (const char* command);
local inline void create_input_pipe(command)
  var const char* command;
  {
    var int child;
    #ifdef EMUNIX
    var int handles[2];
    {
      begin_system_call();
      var FILE* f = popen(command,"r");
      if (f==NULL) { OS_error(); }
      child = f->_pid;
      handles[0] = fileno(f);
      end_system_call();
    }
    #endif
    #ifdef UNIX
    var int handles[2]; # zwei Handles für die Pipe
    {
      # Als Shell nehmen wir immer die Kommando-Shell.
      # command in den Stack kopieren:
      var uintL command_length = asciz_length(command)+1;
      var DYNAMIC_ARRAY(command_data,char,command_length);
      {
        var const char* ptr1 = command;
        var char* ptr2 = &command_data[0];
        dotimespL(command_length,command_length, { *ptr2++ = *ptr1++; } );
      }
      begin_system_call();
      # Pipe aufbauen:
      if (!( pipe(handles) ==0)) {
        FREE_DYNAMIC_ARRAY(command_data); OS_error();
      }
      # Alles, was in handles[1] reingeschoben wird, kommt bei handles[0]
      # wieder raus. Wir werden dies so benutzen:
      #
      #       write            system            read
      # child  ->   handles[1]   ->   handles[0]  ->  parent
      #
      # einen neuen Prozess starten:
      if ((child = vfork()) ==0) {
        # Dieses Programmstück wird vom Child-Prozess ausgeführt:
        if ( dup2(handles[1],stdout_handle) >=0) # Standard-Output umleiten
          if ( CLOSE(handles[1]) ==0) # Wir wollen nur über stdout_handle schreiben
            if ( CLOSE(handles[0]) ==0) { # Wir wollen von der Pipe nicht lesen
              # (Muss das dem Betriebssystem sagen, damit - wenn der Child
              # die Pipe gefüllt hat - der Parent-Prozess und nicht etwa der
              # Child-Prozess aufgerufen wird, um die Pipe zu leeren.)
              # Child-Prozess zum Hintergrundprozess machen:
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
      if (child==-1) {
        # Etwas ist misslungen, entweder beim vfork oder beim execl.
        # In beiden Fällen wurde errno gesetzt.
        var int saved_errno = errno;
        CLOSE(handles[1]); CLOSE(handles[0]);
        FREE_DYNAMIC_ARRAY(command_data);
        errno = saved_errno; OS_error();
      }
      # Wir wollen von der Pipe nur lesen, nicht schreiben:
      if (!( CLOSE(handles[1]) ==0)) {
        var int saved_errno = errno;
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
    {
      begin_system_call();
      var Handle child_write_handle;
      # Create a pipe and make one of the two handles inheritable.
      if (!CreatePipe(&handles[0],&handles[1],NULL,0)) { OS_error(); }
      if (!DuplicateHandle(GetCurrentProcess(),handles[1],
                           GetCurrentProcess(),&child_write_handle,
                           0, TRUE, DUPLICATE_SAME_ACCESS)) {
        OS_error();
      }
      if (!CloseHandle(handles[1])) { OS_error(); }
      var HANDLE stdinput;
      var PROCESS_INFORMATION pinfo;
      stdinput = GetStdHandle(STD_INPUT_HANDLE);
      if (stdinput == INVALID_HANDLE_VALUE) { OS_error(); }
      if (!MyCreateProcess(command,stdinput,child_write_handle,&pinfo)) {
        OS_error();
      }
      # Close our copy of the child's handle, so that the OS knows
      # that we won't write on it.
      if (!CloseHandle(child_write_handle)) { OS_error(); }
      if (!CloseHandle(pinfo.hThread)) { OS_error(); }
      if (!CloseHandle(pinfo.hProcess)) { OS_error(); }
      child = pinfo.dwProcessId;
      end_system_call();
    }
    #endif
    pushSTACK(UL_to_I(child));
    pushSTACK(STACK_(1+1));
    pushSTACK(STACK_(2+2));
    pushSTACK(allocate_handle(handles[0]));
  }

LISPFUN(make_pipe_input_stream,1,0,norest,key,3,\
        (kw(element_type),kw(external_format),kw(buffered)) )
# (MAKE-PIPE-INPUT-STREAM command [:element-type] [:external-format] [:buffered])
# ruft eine Shell auf, die command ausführt, wobei deren Standard-Output
# in unsere Pipe hinein geht.
  {
    var decoded_eltype eltype;
    var signean buffered;
    # command überprüfen:
    pushSTACK(STACK_3); funcall(L(string),1); # (STRING command)
    STACK_3 = value1;
    # Check and canonicalize the :BUFFERED argument:
    buffered = test_buffered_arg(STACK_0); # default is NIL
    # Check and canonicalize the :ELEMENT-TYPE argument:
    test_eltype_arg(&STACK_2,&eltype);
    STACK_2 = canon_eltype(&eltype);
    if (buffered <= 0) { check_unbuffered_eltype(&eltype); }
    # Check and canonicalize the :EXTERNAL-FORMAT argument:
    STACK_1 = test_external_format_arg(STACK_1);
    # Now create the pipe.
    with_string_0(STACK_3,O(misc_encoding),command_asciz, {
      create_input_pipe(command_asciz);
    });
    # Stream allozieren:
    var object stream;
    if (!eq(STACK_(0+4),T)) { # (buffered <= 0) ?
      stream = make_unbuffered_stream(strmtype_pipe_in,1,&eltype,FALSE);
      UnbufferedPipeStream_input_init(stream);
    } else {
      stream = make_buffered_stream(strmtype_pipe_in,1,&eltype,FALSE,FALSE);
      BufferedPipeStream_init(stream);
    }
    ChannelStreamLow_close(stream) = &low_close_pipe;
    TheStream(stream)->strm_pipe_pid = popSTACK(); # Child-Pid
    skipSTACK(4);
    value1 = stream; mv_count=1; # stream als Wert
  }


# Pipe-Output-Stream
# ==================

# Low-level.

#if defined(HAVE_SIGNALS) && defined(SIGPIPE)

  # Be careful to disable SIGPIPE during write() to a subprocess.

  local void low_write_unbuffered_pipe (object stream, uintB b);
  local void low_write_unbuffered_pipe(stream,b)
    var object stream;
    var uintB b;
    {
      var Handle handle = TheHandle(TheStream(stream)->strm_ochannel);
     restart_it:
      begin_system_call();
      # Try to output the byte.
      writing_to_subprocess = TRUE;
      var int result = write(handle,&b,1);
      writing_to_subprocess = FALSE;
      if (result<0) {
        if (errno==EINTR) { # Unterbrechung (evtl. durch Ctrl-C) ?
          end_system_call();
          interruptp({ fehler_interrupt(); });
          goto restart_it;
        }
        OS_error(); # Error melden
      }
      end_system_call();
      if (result==0) # not successful?
        fehler_unwritable(TheSubr(subr_self)->name,stream);
    }

  local const uintB* low_write_array_unbuffered_pipe (object stream, const uintB* byteptr, uintL len);
  local const uintB* low_write_array_unbuffered_pipe(stream,byteptr,len)
    var object stream;
    var const uintB* byteptr;
    var uintL len;
    {
      var Handle handle = TheHandle(TheStream(stream)->strm_ochannel);
      begin_system_call();
      writing_to_subprocess = TRUE;
      var sintL result = full_write(handle,byteptr,len);
      writing_to_subprocess = FALSE;
      if (result<0) { OS_error(); } # Error melden
      end_system_call();
      if (!(result==(sintL)len)) # not successful?
        fehler_unwritable(TheSubr(subr_self)->name,stream);
      return byteptr+result;
    }

#else

  #define low_write_unbuffered_pipe  low_write_unbuffered_handle
  #define low_write_array_unbuffered_pipe  low_write_array_unbuffered_handle

#endif

  local void low_finish_output_unbuffered_pipe (object stream);
  local void low_finish_output_unbuffered_pipe(stream)
    var object stream;
    { } # cannot do anything

  local void low_force_output_unbuffered_pipe (object stream);
  local void low_force_output_unbuffered_pipe(stream)
    var object stream;
    { } # cannot do anything

  local void low_clear_output_unbuffered_pipe (object stream);
  local void low_clear_output_unbuffered_pipe(stream)
    var object stream;
    { } # impossible to clear past output

  #define UnbufferedPipeStream_output_init(stream)  \
    { UnbufferedStreamLow_write(stream) = &low_write_unbuffered_pipe;                 \
      UnbufferedStreamLow_write_array(stream) = &low_write_array_unbuffered_pipe;     \
      UnbufferedStreamLow_finish_output(stream) = &low_finish_output_unbuffered_pipe; \
      UnbufferedStreamLow_force_output(stream) = &low_force_output_unbuffered_pipe;   \
      UnbufferedStreamLow_clear_output(stream) = &low_clear_output_unbuffered_pipe;   \
    }

local inline void create_output_pipe (const char* command);
local inline void create_output_pipe(command)
  var const char* command;
  {
    var int child;
    #ifdef EMUNIX
    var int handles[2];
    {
      begin_system_call();
      var FILE* f = popen(command,"w");
      if (f==NULL) { OS_error(); }
      child = f->_pid;
      handles[1] = fileno(f);
      end_system_call();
    }
    #endif
    #ifdef UNIX
    var int handles[2]; # zwei Handles für die Pipe
    {
      # Als Shell nehmen wir immer die Kommando-Shell.
      # command in den Stack kopieren:
      var uintL command_length = asciz_length(command)+1;
      var DYNAMIC_ARRAY(command_data,char,command_length);
      {
        var const char* ptr1 = command;
        var char* ptr2 = &command_data[0];
        dotimespL(command_length,command_length, { *ptr2++ = *ptr1++; } );
      }
      begin_system_call();
      if (!( pipe(handles) ==0)) {
        FREE_DYNAMIC_ARRAY(command_data); OS_error();
      }
      # Alles, was in handles[1] reingeschoben wird, kommt bei handles[0]
      # wieder raus. Wir werden dies so benutzen:
      #
      #        write            system            read
      # parent  ->   handles[1]   ->   handles[0]  ->  child
      #
      # einen neuen Prozess starten:
      if ((child = vfork()) ==0) {
        # Dieses Programmstück wird vom Child-Prozess ausgeführt:
        if ( dup2(handles[0],stdin_handle) >=0) # Standard-Input umleiten
          if ( CLOSE(handles[0]) ==0) # Wir wollen nur über stdin_handle lesen
            if ( CLOSE(handles[1]) ==0) { # Wir wollen auf die Pipe nicht schreiben
              # (Muss das dem Betriebssystem sagen, damit - wenn der Child
              # die Pipe geleert hat - der Parent-Prozess und nicht etwa der
              # Child-Prozess aufgerufen wird, um die Pipe zu wieder zu füllen.)
              # Child-Prozess zum Hintergrundprozess machen:
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
      if (child==-1) {
        # Etwas ist misslungen, entweder beim vfork oder beim execl.
        # In beiden Fällen wurde errno gesetzt.
        var int saved_errno = errno;
        CLOSE(handles[1]); CLOSE(handles[0]);
        FREE_DYNAMIC_ARRAY(command_data);
        errno = saved_errno; OS_error();
      }
      # Wir wollen auf die Pipe nur schreiben, nicht lesen:
      if (!( CLOSE(handles[0]) ==0)) {
        var int saved_errno = errno;
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
    {
      begin_system_call();
      var Handle child_read_handle;
      # Create a pipe and make one of the two handles inheritable.
      if (!CreatePipe(&handles[0],&handles[1],NULL,0)) { OS_error(); }
      if (!DuplicateHandle(GetCurrentProcess(),handles[0],
                           GetCurrentProcess(),&child_read_handle,
                           0, TRUE, DUPLICATE_SAME_ACCESS)) {
        OS_error();
      }
      if (!CloseHandle(handles[0])) { OS_error(); }
      var HANDLE stdoutput;
      var PROCESS_INFORMATION pinfo;
      stdoutput = GetStdHandle(STD_OUTPUT_HANDLE);
      if (stdoutput == INVALID_HANDLE_VALUE) { OS_error(); }
      if (!MyCreateProcess(command,child_read_handle,stdoutput,&pinfo)) {
        OS_error();
      }
      # Close our copy of the child's handle, so that the OS knows
      # that we won't read from it.
      if (!CloseHandle(child_read_handle)) { OS_error(); }
      if (!CloseHandle(pinfo.hThread)) { OS_error(); }
      if (!CloseHandle(pinfo.hProcess)) { OS_error(); }
      child = pinfo.dwProcessId;
      end_system_call();
    }
    #endif
    pushSTACK(UL_to_I(child));
    pushSTACK(STACK_(1+1));
    pushSTACK(STACK_(2+2));
    pushSTACK(allocate_handle(handles[1]));
  }

LISPFUN(make_pipe_output_stream,1,0,norest,key,3,\
        (kw(element_type),kw(external_format),kw(buffered)) )
# (MAKE-PIPE-OUTPUT-STREAM command [:element-type] [:external-format] [:buffered])
# ruft eine Shell auf, die command ausführt, wobei unsere Pipe in deren
# Standard-Input hinein geht.
  {
    var decoded_eltype eltype;
    var signean buffered;
    # command überprüfen:
    pushSTACK(STACK_3); funcall(L(string),1); # (STRING command)
    STACK_3 = value1;
    # Check and canonicalize the :BUFFERED argument:
    buffered = test_buffered_arg(STACK_0); # default is NIL
    # Check and canonicalize the :ELEMENT-TYPE argument:
    test_eltype_arg(&STACK_2,&eltype);
    STACK_2 = canon_eltype(&eltype);
    if (buffered <= 0) { check_unbuffered_eltype(&eltype); }
    # Check and canonicalize the :EXTERNAL-FORMAT argument:
    STACK_1 = test_external_format_arg(STACK_1);
    # Now create the pipe.
    with_string_0(STACK_3,O(misc_encoding),command_asciz, {
      create_output_pipe(command_asciz);
    });
    # Stream allozieren:
    var object stream;
    if (!eq(STACK_(0+4),T)) { # (buffered <= 0) ?
      stream = make_unbuffered_stream(strmtype_pipe_out,4,&eltype,FALSE);
      UnbufferedPipeStream_output_init(stream);
    } else {
      stream = make_buffered_stream(strmtype_pipe_out,4,&eltype,FALSE,FALSE);
      BufferedPipeStream_init(stream);
    }
    ChannelStreamLow_close(stream) = &low_close_pipe;
    TheStream(stream)->strm_pipe_pid = popSTACK(); # Child-Pid
    skipSTACK(4);
    value1 = stream; mv_count=1; # stream als Wert
  }

#ifdef PIPES2

# Bidirektionale Pipes
# ====================

local inline void create_io_pipe (const char* command);
local inline void create_io_pipe(command)
  var const char* command;
  {
    var int child;
    #ifdef EMUNIX
    var int in_handles[2];
    var int out_handles[2];
    {
      # Stackaufbau: command.
      var FILE* f_in;
      var FILE* f_out;
      begin_system_call();
      if (popenrw(command,&f_in,&f_out) <0) { OS_error(); }
      child = f_in->_pid; # = f_out->_pid;
      in_handles[0] = fileno(f_in);
      out_handles[1] = fileno(f_out);
    }
    #endif
    #ifdef UNIX
    var int in_handles[2]; # zwei Handles für die Pipe zum Input-Stream
    var int out_handles[2]; # zwei Handles für die Pipe zum Output-Stream
    {
      # Als Shell nehmen wir immer die Kommando-Shell.
      # command in den Stack kopieren:
      var uintL command_length = asciz_length(command)+1;
      var DYNAMIC_ARRAY(command_data,char,command_length);
      {
        var const char* ptr1 = command;
        var char* ptr2 = &command_data[0];
        dotimespL(command_length,command_length, { *ptr2++ = *ptr1++; } );
      }
      begin_system_call();
      # Pipes aufbauen:
      if (!( pipe(in_handles) ==0)) {
        FREE_DYNAMIC_ARRAY(command_data); OS_error();
      }
      if (!( pipe(out_handles) ==0)) {
        var int saved_errno = errno;
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
      if ((child = vfork()) ==0) {
        # Dieses Programmstück wird vom Child-Prozess ausgeführt:
        if ( dup2(out_handles[0],stdin_handle) >=0) # Standard-Input umleiten
          if ( dup2(in_handles[1],stdout_handle) >=0) # Standard-Output umleiten
            if ( CLOSE(out_handles[0]) ==0) # Wir wollen nur über stdin_handle lesen
              if ( CLOSE(in_handles[1]) ==0) # Wir wollen nur über stdout_handle schreiben
                if ( CLOSE(out_handles[1]) ==0) # Wir wollen auf die Pipe nicht schreiben
                  # (Muss das dem Betriebssystem sagen, damit - wenn der Child
                  # die Pipe geleert hat - der Parent-Prozess und nicht etwa der
                  # Child-Prozess aufgerufen wird, um die Pipe zu wieder zu füllen.)
                  if ( CLOSE(in_handles[0]) ==0) { # Wir wollen von der Pipe nicht lesen
                    # (Muss das dem Betriebssystem sagen, damit - wenn der Child
                    # die Pipe gefüllt hat - der Parent-Prozess und nicht etwa der
                    # Child-Prozess aufgerufen wird, um die Pipe zu leeren.)
                    # Child-Prozess zum Hintergrundprozess machen:
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
      if (child==-1) {
        # Etwas ist misslungen, entweder beim vfork oder beim execl.
        # In beiden Fällen wurde errno gesetzt.
        var int saved_errno = errno;
        CLOSE(in_handles[1]); CLOSE(in_handles[0]);
        CLOSE(out_handles[1]); CLOSE(out_handles[0]);
        FREE_DYNAMIC_ARRAY(command_data);
        errno = saved_errno; OS_error();
      }
      # Wir wollen auf die Pipe nur schreiben, nicht lesen:
      if (!( CLOSE(out_handles[0]) ==0)) {
        var int saved_errno = errno;
        CLOSE(in_handles[1]); CLOSE(in_handles[0]);
        CLOSE(out_handles[1]);
        FREE_DYNAMIC_ARRAY(command_data);
        errno = saved_errno; OS_error();
      }
      # (Muss das dem Betriebssystem sagen, damit - wenn der Parent-Prozess
      # die Pipe gefüllt hat - der Child-Prozess und nicht etwa der
      # Parent-Prozess aufgerufen wird, um die Pipe wieder zu leeren.)
      # Wir wollen von der Pipe nur lesen, nicht schreiben:
      if (!( CLOSE(in_handles[1]) ==0)) {
        var int saved_errno = errno;
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
    {
      begin_system_call();
      var Handle child_read_handle;
      var Handle child_write_handle;
      # Create two pipes and make two of the four handles inheritable.
      if (!CreatePipe(&in_handles[0],&in_handles[1],NULL,0)) { OS_error(); }
      if (!DuplicateHandle(GetCurrentProcess(),in_handles[1],
                           GetCurrentProcess(),&child_write_handle,
                           0, TRUE, DUPLICATE_SAME_ACCESS)) {
        OS_error();
      }
      if (!CloseHandle(in_handles[1])) { OS_error(); }
      if (!CreatePipe(&out_handles[0],&out_handles[1],NULL,0)) { OS_error(); }
      if (!DuplicateHandle(GetCurrentProcess(),out_handles[0],
                           GetCurrentProcess(),&child_read_handle,
                           0, TRUE, DUPLICATE_SAME_ACCESS)) {
        OS_error();
      }
      if (!CloseHandle(out_handles[0])) { OS_error(); }
      var PROCESS_INFORMATION pinfo;
      if (!MyCreateProcess(command,child_read_handle,child_write_handle,&pinfo)) {
        OS_error();
      }
      # Close our copies of the child's handles, so that the OS knows
      # that we won't use them.
      if (!CloseHandle(child_read_handle)) { OS_error(); }
      if (!CloseHandle(child_write_handle)) { OS_error(); }
      if (!CloseHandle(pinfo.hThread)) { OS_error(); }
      if (!CloseHandle(pinfo.hProcess)) { OS_error(); }
      child = pinfo.dwProcessId;
      end_system_call();
    }
    #endif
    pushSTACK(UL_to_I(child));
    pushSTACK(allocate_handle(in_handles[0]));
    pushSTACK(allocate_handle(out_handles[1]));
  }

LISPFUN(make_pipe_io_stream,1,0,norest,key,3,\
        (kw(element_type),kw(external_format),kw(buffered)) )
# (MAKE-PIPE-IO-STREAM command [:element-type] [:external-format] [:buffered])
# ruft eine Shell auf, die command ausführt, wobei der Output unserer Pipe
# in deren Standard-Input hinein und deren Standard-Output wiederum in
# unsere Pipe hinein geht.
  {
    var decoded_eltype eltype;
    var signean buffered;
    # command überprüfen:
    pushSTACK(STACK_3); funcall(L(string),1); # (STRING command)
    STACK_3 = value1;
    # Check and canonicalize the :BUFFERED argument:
    buffered = test_buffered_arg(STACK_0); # default is NIL
    # Check and canonicalize the :ELEMENT-TYPE argument:
    test_eltype_arg(&STACK_2,&eltype);
    STACK_2 = canon_eltype(&eltype);
    if (buffered <= 0) { check_unbuffered_eltype(&eltype); }
    # Check and canonicalize the :EXTERNAL-FORMAT argument:
    STACK_1 = test_external_format_arg(STACK_1);
    # Now create the pipe.
    with_string_0(STACK_3,O(misc_encoding),command_asciz, {
      create_io_pipe(command_asciz);
    });
    # Input-Stream allozieren:
    {
      pushSTACK(STACK_(1+3)); # encoding
      pushSTACK(STACK_(2+3+1)); # eltype
      pushSTACK(STACK_(1+2));
      var object stream;
      if (!eq(STACK_(0+6),T)) { # (buffered <= 0) ?
        stream = make_unbuffered_stream(strmtype_pipe_in,1,&eltype,FALSE);
        UnbufferedPipeStream_input_init(stream);
      } else {
        stream = make_buffered_stream(strmtype_pipe_in,1,&eltype,FALSE,FALSE);
        BufferedPipeStream_init(stream);
      }
      ChannelStreamLow_close(stream) = &low_close_pipe;
      TheStream(stream)->strm_pipe_pid = STACK_2; # Child-Pid
      STACK_1 = stream;
    }
    # Output-Stream allozieren:
    {
      pushSTACK(STACK_(1+3)); # encoding
      pushSTACK(STACK_(2+3+1)); # eltype
      pushSTACK(STACK_(0+2));
      var object stream;
      if (!eq(STACK_(0+6),T)) { # (buffered <= 0) ?
        stream = make_unbuffered_stream(strmtype_pipe_out,4,&eltype,FALSE);
        UnbufferedPipeStream_output_init(stream);
      } else {
        stream = make_buffered_stream(strmtype_pipe_out,4,&eltype,FALSE,FALSE);
        BufferedPipeStream_init(stream);
      }
      ChannelStreamLow_close(stream) = &low_close_pipe;
      TheStream(stream)->strm_pipe_pid = STACK_2; # Child-Pid
      STACK_0 = stream;
    }
    #ifdef EMUNIX
    # Beide Pipes miteinander verknüpfen, zum reibungslosen close:
    TheStream(STACK_1)->strm_pipe_other = STACK_0;
    TheStream(STACK_0)->strm_pipe_other = STACK_1;
    #endif
    # 3 Werte:
    # (make-two-way-stream input-stream output-stream), input-stream, output-stream.
    STACK_2 = make_twoway_stream(STACK_1,STACK_0);
    funcall(L(values),3);
    skipSTACK(4);
  }

#endif # PIPES2

#endif # PIPES


#if defined(X11SOCKETS) || defined(SOCKET_STREAMS)

# X11-Socket-Stream, Socket-Stream
# ================================

# Socket streams are just like handle streams (unbuffered file streams),
# except that on WIN32_NATIVE, the low-level functions are different.

# Both sides
# ----------

# Closes a socket handle.
  #ifdef WIN32_NATIVE
    local void low_close_socket (object stream, object handle);
    local void low_close_socket(stream,handle)
      var object stream;
      var object handle;
      {
        begin_system_call();
        if (!( closesocket(TheSocket(handle)) ==0)) { SOCK_error(); }
        end_system_call();
      }
  #else
    #define low_close_socket  low_close_handle
  #endif

# Input side
# ----------

#ifdef WIN32_NATIVE

  local sintL low_read_unbuffered_socket (object stream);
  local sintL low_read_unbuffered_socket(stream)
    var object stream;
    {
      if (UnbufferedStream_status(stream) < 0) # already EOF?
        return -1;
      if (UnbufferedStream_status(stream) > 0) { # bytebuf contains valid bytes?
        UnbufferedStreamLow_pop_byte(stream,b); return b;
      }
      var SOCKET handle = TheSocket(TheStream(stream)->strm_ichannel);
      var uintB b;
      begin_system_call();
      var int result = sock_read(handle,&b,1); # Zeichen lesen versuchen
      if (result<0) {
        if (WSAGetLastError()==WSAEINTR) { # Unterbrechung durch Ctrl-C ?
          end_system_call(); fehler_interrupt();
        }
        SOCK_error();
      }
      end_system_call();
      if (result==0) {
        # no byte available -> must be EOF
        UnbufferedStream_status(stream) = -1; return -1;
      } else {
        return b;
      }
    }

  local signean low_listen_unbuffered_socket (object stream);
  local signean low_listen_unbuffered_socket(stream)
    var object stream;
    {
      if (UnbufferedStream_status(stream) < 0) # already EOF?
        return ls_eof;
      if (UnbufferedStream_status(stream) > 0) # bytebuf contains valid bytes?
        return ls_avail;
      var SOCKET handle = TheSocket(TheStream(stream)->strm_ichannel);
      # Use select() with readfds = singleton set {handle}
      # and timeout = zero interval.
      var fd_set handle_menge; # set of handles := {handle}
      var struct timeval zero_time; # time interval := 0
      begin_system_call();
      FD_ZERO(&handle_menge); FD_SET(handle,&handle_menge);
      zero_time.tv_sec = 0; zero_time.tv_usec = 0;
      var int result;
      result = select(FD_SETSIZE,&handle_menge,NULL,NULL,&zero_time);
      if (result<0) {
        if (WSAGetLastError()==WSAEINTR) { # Unterbrechung durch Ctrl-C ?
          end_system_call(); fehler_interrupt();
        }
        SOCK_error();
      } else {
        # result = number of handles in handle_menge for which read() would
        # return without blocking.
        if (result==0) {
          end_system_call(); return ls_wait;
        }
        # result=1
        # When read() returns a result without blocking, this can also be EOF!
        # try to read a byte:
        var uintB b;
        var int result = sock_read(handle,&b,1);
        if (result<0) {
          if (WSAGetLastError()==WSAEINTR) { # Unterbrechung durch Ctrl-C ?
            end_system_call(); fehler_interrupt();
          }
          SOCK_error();
        }
        end_system_call();
        if (result==0) {
          UnbufferedStream_status(stream) = -1; return ls_eof;
        } else {
          # Stuff the read byte into the buffer, for next low_read call.
          UnbufferedStreamLow_push_byte(stream,b);
          return ls_avail;
        }
      }
    }

  local boolean low_clear_input_unbuffered_socket (object stream);
  local boolean low_clear_input_unbuffered_socket(stream)
    var object stream;
    {
      # This is not called anyway, because TheStream(stream)->strm_isatty = NIL.
      return FALSE; # Not sure whether this is the correct behaviour??
    }

  local uintB* low_read_array_unbuffered_socket (object stream, uintB* byteptr, uintL len);
  local uintB* low_read_array_unbuffered_socket(stream,byteptr,len)
    var object stream;
    var uintB* byteptr;
    var uintL len;
    {
      if (UnbufferedStream_status(stream) < 0) # already EOF?
        return byteptr;
      while (UnbufferedStream_status(stream) > 0) { # bytebuf contains valid bytes?
        UnbufferedStreamLow_pop_byte(stream,b);
        *byteptr++ = b;
        len--;
        if (len == 0)
          return byteptr;
      }
      var SOCKET handle = TheSocket(TheStream(stream)->strm_ichannel);
      begin_system_call();
      var sintL result = sock_read(handle,byteptr,len);
      if (result<0) {
        if (WSAGetLastError()==WSAEINTR) { # Unterbrechung (evtl. durch Ctrl-C) ?
          end_system_call(); fehler_interrupt();
        }
        OS_error(); # Error melden
      }
      end_system_call();
      byteptr += result;
      return byteptr;
    }

# Initializes the input side fields of a socket stream.
# UnbufferedSocketStream_input_init(stream);
  #define UnbufferedSocketStream_input_init(stream)  \
    { UnbufferedStreamLow_read(stream) = &low_read_unbuffered_socket;               \
      UnbufferedStreamLow_listen(stream) = &low_listen_unbuffered_socket;           \
      UnbufferedStreamLow_clear_input(stream) = &low_clear_input_unbuffered_socket; \
      UnbufferedStreamLow_read_array(stream) = &low_read_array_unbuffered_socket;   \
      UnbufferedHandleStream_input_init_data(stream);                               \
    }

#else

  #define UnbufferedSocketStream_input_init(stream)  UnbufferedHandleStream_input_init(stream)

#endif

# Output side
# -----------

#ifdef WIN32_NATIVE

  local void low_write_unbuffered_socket (object stream, uintB b);
  local void low_write_unbuffered_socket(stream,b)
    var object stream;
    var uintB b;
    {
      var SOCKET handle = TheSocket(TheStream(stream)->strm_ochannel);
      begin_system_call();
      # Try to output the byte.
      var int result = sock_write(handle,&b,1);
      if (result<0) {
        if (WSAGetLastError()==WSAEINTR) { # Unterbrechung (evtl. durch Ctrl-C) ?
          end_system_call(); fehler_interrupt();
        }
        SOCK_error(); # Error melden
      }
      end_system_call();
      if (result==0) # not successful?
        fehler_unwritable(TheSubr(subr_self)->name,stream);
    }

  local const uintB* low_write_array_unbuffered_socket (object stream, const uintB* byteptr, uintL len);
  local const uintB* low_write_array_unbuffered_socket(stream,byteptr,len)
    var object stream;
    var const uintB* byteptr;
    var uintL len;
    {
      var SOCKET handle = TheSocket(TheStream(stream)->strm_ochannel);
      begin_system_call();
      var sintL result = sock_write(handle,byteptr,len);
      if (result<0) {
        if (WSAGetLastError()==WSAEINTR) { # Unterbrechung (evtl. durch Ctrl-C) ?
          end_system_call(); fehler_interrupt();
        }
        SOCK_error(); # Error melden
      }
      end_system_call();
      if (!(result==(sintL)len)) # not successful?
        fehler_unwritable(TheSubr(subr_self)->name,stream);
      return byteptr+result;
    }

#endif

# Initializes the output side fields of a socket stream.
# UnbufferedSocketStream_output_init(stream);
#ifdef WIN32_NATIVE
  #define UnbufferedSocketStream_output_init(stream)  \
    { UnbufferedStreamLow_write(stream) = &low_write_unbuffered_socket;               \
      UnbufferedStreamLow_write_array(stream) = &low_write_array_unbuffered_socket;   \
      UnbufferedStreamLow_finish_output(stream) = &low_finish_output_unbuffered_pipe; \
      UnbufferedStreamLow_force_output(stream) = &low_force_output_unbuffered_pipe;   \
      UnbufferedStreamLow_clear_output(stream) = &low_clear_output_unbuffered_pipe;   \
    }
#else
  # Use low_write_unbuffered_pipe, not low_write_unbuffered_handle, here because
  # writing to a closed socket generates a SIGPIPE signal, just like for pipes.
  #define UnbufferedSocketStream_output_init(stream)  \
    { UnbufferedStreamLow_write(stream) = &low_write_unbuffered_pipe;                 \
      UnbufferedStreamLow_write_array(stream) = &low_write_array_unbuffered_pipe;     \
      UnbufferedStreamLow_finish_output(stream) = &low_finish_output_unbuffered_pipe; \
      UnbufferedStreamLow_force_output(stream) = &low_force_output_unbuffered_pipe;   \
      UnbufferedStreamLow_clear_output(stream) = &low_clear_output_unbuffered_pipe;   \
    }
#endif

#endif


#ifdef X11SOCKETS

# X11-Socket-Stream
# =================

# Verwendung: für X-Windows.

# Zusätzliche Komponenten:
  # define strm_x11socket_connect strm_field1 # Liste (host display)

extern SOCKET connect_to_x_server (const char* host, int display); # ein Stück X-Source...

LISPFUNN(make_x11socket_stream,2)
# (SYS::MAKE-SOCKET-STREAM host display)
# liefert einen X11-Socket-Stream für X-Windows oder NIL.
  {
    if (!stringp(STACK_1)) {
      pushSTACK(STACK_1); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(string)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(STACK_(1+2));
      fehler(type_error,
             GETTEXT("host should be string, not ~")
            );
    }
    if (!posfixnump(STACK_0)) {
      pushSTACK(STACK_0); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_posfixnum)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(STACK_(0+2));
      fehler(type_error,
             GETTEXT("display should be a nonnegative fixnum, not ~")
            );
    }
    var const char* host = TheAsciz(string_to_asciz(STACK_1,O(misc_encoding)));
    var SOCKET handle;
    begin_system_call();
    handle = connect_to_x_server(host,posfixnum_to_L(STACK_0));
    end_system_call();
    if (handle == INVALID_SOCKET) { SOCK_error(); }
    # Liste bilden:
    { var object list = listof(2); pushSTACK(list); }
    pushSTACK(test_external_format_arg(S(Kunix)));
    pushSTACK(O(strmtype_ubyte8));
    pushSTACK(allocate_socket(handle));
    # Stream allozieren:
    var decoded_eltype eltype = { eltype_iu, 8 };
    var object stream = make_unbuffered_stream(strmtype_x11socket,5,&eltype,FALSE);
    UnbufferedSocketStream_input_init(stream);
    UnbufferedSocketStream_output_init(stream);
    ChannelStreamLow_close(stream) = &low_close_socket;
    TheStream(stream)->strm_x11socket_connect = popSTACK(); # zweielementige Liste
    value1 = stream; mv_count=1; # stream als Wert
  }

# A general LISTEN-BYTE function would be hairy (at least the case where you
# want to know whether a multibyte integer is pending, and the stream is
# unbuffered). For CLX, it is sufficient to deal with a socket stream with
# element type (UNSIGNED-BYTE 8).

LISPFUNN(listen_byte,1)
# (SYS::LISTEN-BYTE stream)
  {
    var object stream = popSTACK();
    if (!(builtin_stream_p(stream)
          && eq(TheStream(stream)->strm_rd_by,P(rd_by_iau8_unbuffered))
       ) ) {
      if (!streamp(stream)) {
        fehler_stream(stream);
      } else {
        pushSTACK(stream);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: stream must be a socket-stream, not ~")
              );
      }
    }
    value1 = (ls_avail_p(UnbufferedStreamLow_listen(stream)(stream)) ? T : NIL);
    mv_count=1;
  }

# Die beiden folgenden Funktionen sollten
# 1. nicht nur auf Handle- und Socket-Streams, sondern auch auf Synonym-
#    und Concatenated-Streams funktionieren, idealerweise auch auf File-Streams.
# 2. auch nicht-simple Byte-Vektoren akzeptieren.
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
    {
      {
        var object stream = STACK_3;
        if (!(builtin_stream_p(stream)
              && eq(TheStream(stream)->strm_rd_by,P(rd_by_iau8_unbuffered))
              && eq(TheStream(stream)->strm_wr_by,P(wr_by_iau8_unbuffered))
           ) ) {
          if (!streamp(stream)) {
            fehler_stream(stream);
          } else {
            pushSTACK(stream);
            pushSTACK(TheSubr(subr_self)->name);
            fehler(error,
                   GETTEXT("~: stream must be a socket-stream, not ~")
                  );
          }
        }
      }
      {
        var object vector = STACK_2;
        if (!bit_vector_p(Atype_8Bit,vector)) {
          pushSTACK(vector); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_uint8_vector)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(vector);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,
                 GETTEXT("~: argument ~ should be a vector of type (ARRAY (UNSIGNED-BYTE 8) (*))")
                );
        }
        if (!posfixnump(STACK_0))
          fehler_posfixnum(STACK_0);
        *count_ = posfixnum_to_L(popSTACK());
        if (!posfixnump(STACK_0))
          fehler_posfixnum(STACK_0);
        *index_ = posfixnum_to_L(popSTACK());
        STACK_0 = array_displace_check(vector,*count_,index_);
      }
    }

LISPFUNN(read_n_bytes,4)
  {
    var uintL startindex;
    var uintL totalcount;
    test_n_bytes_args(&startindex,&totalcount);
    if (!(totalcount==0)) {
      if (!(read_byte_array(&STACK_1,&STACK_0,startindex,totalcount) == totalcount)) {
        pushSTACK(STACK_1); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(STACK_(1+1)); # Stream
        pushSTACK(S(read_n_bytes));
        fehler(end_of_file,
               GETTEXT("~: input stream ~ has reached its end")
              );
      }
    }
    skipSTACK(2);
    value1 = T; mv_count=1; # Wert T
  }

LISPFUNN(write_n_bytes,4)
  {
    var uintL startindex;
    var uintL totalcount;
    test_n_bytes_args(&startindex,&totalcount);
    if (!(totalcount==0)) {
      write_byte_array(&STACK_1,&STACK_0,startindex,totalcount);
    }
    skipSTACK(2);
    value1 = T; mv_count=1; # Wert T
  }

#endif


#ifdef SOCKET_STREAMS

# Socket-Streams
# ==============

  # define strm_socket_port strm_field1 # port, a fixnum >=0
  # define strm_socket_host strm_field2 # host, NIL or a string

# Low-level functions for buffered socket streams.

#ifdef WIN32_NATIVE

# UP: Fills the buffer, up to strm_buffered_bufflen bytes.
# low_fill_buffered_socket(stream)
# > stream: (open) byte-based socket stream
# < result: number of bytes read
  local uintL low_fill_buffered_socket (object stream);
  local uintL low_fill_buffered_socket(stream)
    var object stream;
    {
      var sintL result;
      begin_system_call();
      result = # Buffer füllen
        sock_read(TheSocket(TheStream(stream)->strm_buffered_channel), # Handle
                  &TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[0], # Bufferadresse
                  strm_buffered_bufflen
                 );
      if (result<0) {
        if (WSAGetLastError()==WSAEINTR) { # Unterbrechung (evtl. durch Ctrl-C) ?
          end_system_call(); fehler_interrupt();
        }
        OS_error(); # Error melden
      }
      end_system_call();
      return result;
    }

# UP: Beendet das Zurückschreiben des Buffers.
# low_flush_buffered_socket(stream,bufflen);
# > stream : (offener) Byte-basierter File-Stream.
# > bufflen : Anzahl der zu schreibenden Bytes
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void low_flush_buffered_socket (object stream, uintL bufflen);
  local void low_flush_buffered_socket(stream,bufflen)
    var object stream;
    var uintL bufflen;
    {
      begin_system_call();
      var sintL result = # Buffer hinausschreiben
        sock_write(TheSocket(TheStream(stream)->strm_buffered_channel), # Handle
                   &TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[0], # Bufferadresse
                   bufflen
                  );
      if (result==bufflen) {
        # alles korrekt geschrieben
        end_system_call(); BufferedStream_modified(stream) = FALSE;
      } else {
        # Nicht alles geschrieben
        if (result<0) {
          if (WSAGetLastError()==WSAEINTR) { # Unterbrechung (evtl. durch Ctrl-C) ?
            end_system_call(); fehler_interrupt();
          }
          SOCK_error(); # Error melden
        }
        end_system_call();
        fehler_unwritable(TheSubr(subr_self)->name,stream);
      }
    }

#else

# Use low_flush_buffered_pipe, not low_flush_buffered_handle, here because
# writing to a closed socket generates a SIGPIPE signal, just like for pipes.
  #define low_fill_buffered_socket  low_fill_buffered_handle
  #define low_flush_buffered_socket  low_flush_buffered_pipe

#endif

#define BufferedSocketStream_init(stream)  \
  { BufferedStreamLow_fill(stream) = &low_fill_buffered_socket;   \
    BufferedStreamLow_flush(stream) = &low_flush_buffered_socket; \
  }

# Twoway-Socket-Streams are twoway streams with both input and output side
# being socket streams. (They are needed because the input and output side
# need different buffers. Sockets are not regular files.)
  # define strm_twoway_socket_input  strm_twoway_input  # input side, a socket stream
  #define strm_twoway_socket_output  strm_twoway_output # output side, a socket stream

# Hack for avoiding that the handle is closed twice.
  local void low_close_socket_nop (object stream, object handle);
  local void low_close_socket_nop(stream,handle)
    var object stream;
    var object handle;
    { }

# Creates a socket stream.
# > STACK_2: element-type
# > STACK_1: encoding
local object make_socket_stream (SOCKET handle, decoded_eltype* eltype, signean buffered, object host, object port);
local object make_socket_stream(handle,eltype,buffered,host,port)
  var SOCKET handle;
  var decoded_eltype* eltype;
  var signean buffered;
  var object host; # string
  var object port; # fixnum >=0
  {
    pushSTACK(host);
    pushSTACK(STACK_(1+1)); # encoding
    pushSTACK(STACK_(2+2)); # eltype
    pushSTACK(allocate_socket(handle));
    # Stream allozieren:
    var object stream;
    if (buffered <= 0) {
      stream = make_unbuffered_stream(strmtype_socket,5,eltype,FALSE);
      UnbufferedSocketStream_input_init(stream);
      UnbufferedSocketStream_output_init(stream);
      ChannelStreamLow_close(stream) = &low_close_socket;
      TheStream(stream)->strm_socket_port = port;
      TheStream(stream)->strm_socket_host = popSTACK();
    } else {
      # Input-Stream allozieren:
      pushSTACK(STACK_2); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
      stream = make_buffered_stream(strmtype_socket,1,eltype,FALSE,FALSE);
      BufferedSocketStream_init(stream);
      ChannelStreamLow_close(stream) = &low_close_socket;
      TheStream(stream)->strm_socket_port = port;
      TheStream(stream)->strm_socket_host = STACK_3;
      pushSTACK(stream);
      # Output-Stream allozieren:
      pushSTACK(STACK_(2+1)); pushSTACK(STACK_(1+2)); pushSTACK(STACK_(0+3));
      stream = make_buffered_stream(strmtype_socket,4,eltype,FALSE,FALSE);
      BufferedSocketStream_init(stream);
      ChannelStreamLow_close(stream) = &low_close_socket;
      TheStream(stream)->strm_socket_port = port;
      TheStream(stream)->strm_socket_host = STACK_(3+1);
      pushSTACK(stream);
      # Allocate a Two-Way-Socket-Stream:
      stream = allocate_stream(strmflags_open_B,strmtype_twoway_socket,strm_len+2,0);
      TheStream(stream)->strm_rd_by = P(rd_by_twoway);
      TheStream(stream)->strm_rd_by_array = P(rd_by_array_twoway);
      TheStream(stream)->strm_wr_by = P(wr_by_twoway);
      TheStream(stream)->strm_wr_by_array = P(wr_by_array_twoway);
      TheStream(stream)->strm_rd_ch = P(rd_ch_twoway);
      TheStream(stream)->strm_pk_ch = P(pk_ch_twoway);
      TheStream(stream)->strm_rd_ch_array = P(rd_ch_array_twoway);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_twoway);
      TheStream(stream)->strm_wr_ch_array = P(wr_ch_array_twoway);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      TheStream(stream)->strm_twoway_socket_input = STACK_1;
      TheStream(stream)->strm_twoway_socket_output = STACK_0;
      skipSTACK(6);
    }
    return stream;
  }

local void test_socket_server (object obj, boolean check_open);
local void test_socket_server(obj,check_open)
  var object obj;
  var boolean check_open;
  {
    if (!socket_server_p(obj)) {
      pushSTACK(obj);
      pushSTACK(S(socket_server));
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a SOCKET-SERVER")
            );
    }
    if (check_open && nullp(TheSocketServer(obj)->socket_handle)) {
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~ on ~ is illegal")
            );
    }
  }

# Called when some socket server dies.
LISPFUNN(socket_server_close,1)
  {
    test_socket_server(STACK_0,FALSE);
    var object ss = popSTACK();
    if (!nullp(TheSocketServer(ss)->socket_handle)) {
      var SOCKET s = TheSocket(TheSocketServer(ss)->socket_handle);
      begin_system_call();
      loop {
        if (closesocket(s) < 0) {
          if (!sock_errno_is(EINTR)) { SOCK_error(); }
        } else
          break;
      }
      end_system_call();
      TheSocketServer(ss)->socket_handle = NIL;
    }
    value1 = NIL; mv_count=1;
  }

extern SOCKET create_server_socket (host_data *hd, SOCKET sock, unsigned int port);

LISPFUN(socket_server,0,1,norest,nokey,0,NIL)
# (SOCKET-SERVER [port-or-sock])
  {
    var SOCKET sock;        # a hint for create_server_socket
    var unsigned int port;  # another hint for create_server_socket

    if (eq(STACK_0,unbound)) {
      sock = INVALID_SOCKET; port = 0; goto doit;
    }
    if (posfixnump(STACK_0)) {
      sock = INVALID_SOCKET; port = posfixnum_to_L(STACK_0); goto doit;
    }
    if (builtin_stream_p(STACK_0)) {
      var object stream = STACK_0;
      switch (TheStream(stream)->strmtype) {
        case strmtype_twoway_socket:
          stream = TheStream(stream)->strm_twoway_socket_input;
          /*FALLTHROUGH*/
        case strmtype_socket:
          if (TheStream(stream)->strmflags & strmflags_open_B) {
            sock = TheSocket(TheStream(stream)->strm_ichannel); port = 0; goto doit;
          }
          break;
        default:
          break;
      }
    }
    pushSTACK(STACK_0);
    pushSTACK(S(stream)); # ??
    pushSTACK(STACK_(0+2));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~: argument ~ is neither an open SOCKET-STREAM nor a positive FIXNUM")
          );

   doit:
    var SOCKET sk;
    var host_data myname;
    begin_system_call();
    sk = create_server_socket(&myname, sock, port);
    end_system_call();
    if (sk == INVALID_SOCKET) { SOCK_error(); }

    pushSTACK(allocate_socket(sk));
    pushSTACK(allocate_socket_server());
    TheSocketServer(STACK_0)->socket_handle = STACK_1;
    TheSocketServer(STACK_0)->port = fixnum(myname.port);
    {
      var object host = asciz_to_string(myname.hostname,O(misc_encoding)); # for GC-safety
      TheSocketServer(STACK_0)->host = host;
    }
    pushSTACK(STACK_0);
    pushSTACK(L(socket_server_close));
    funcall(L(finalize),2); # (FINALIZE socket-server #'socket-server-close)
    value1 = popSTACK();
    mv_count = 1;
    skipSTACK(2);
  }

LISPFUNN(socket_server_port,1)
# (SOCKET-SERVER-PORT socket-server)
  {
    test_socket_server(STACK_0,FALSE);
    value1 = TheSocketServer(STACK_0)->port;
    mv_count = 1;
    skipSTACK(1);
  }

LISPFUNN(socket_server_host,1)
# (SOCKET-SERVER-HOST socket-server)
  {
    test_socket_server(STACK_0,FALSE);
    value1 = TheSocketServer(STACK_0)->host;
    mv_count = 1;
    skipSTACK(1);
  }

extern SOCKET accept_connection (SOCKET socket_handle);

LISPFUN(socket_accept,1,0,norest,key,3,\
        (kw(element_type),kw(external_format),kw(buffered)) )
# (SOCKET-ACCEPT socket-server [:element-type] [:external-format] [:buffered])
  {
    var SOCKET sock;
    var decoded_eltype eltype;
    var signean buffered;
    var SOCKET handle;

    test_socket_server(STACK_3,TRUE);

    # Check and canonicalize the :BUFFERED argument:
    buffered = test_buffered_arg(STACK_0); # default is NIL

    # Check and canonicalize the :ELEMENT-TYPE argument:
    test_eltype_arg(&STACK_2,&eltype);
    STACK_2 = canon_eltype(&eltype);
    if (buffered <= 0) { check_unbuffered_eltype(&eltype); }

    # Check and canonicalize the :EXTERNAL-FORMAT argument:
    STACK_1 = test_external_format_arg(STACK_1);

    sock = TheSocket(TheSocketServer(STACK_3)->socket_handle);
    begin_system_call();
    handle = accept_connection (sock);
    end_system_call();
    if (handle == INVALID_SOCKET) { SOCK_error(); }
    value1 = make_socket_stream(handle,&eltype,buffered,
                                TheSocketServer(STACK_3)->host,
                                TheSocketServer(STACK_3)->port);
    mv_count = 1;
    skipSTACK(4);
  }

LISPFUN(socket_wait,1,2,norest,nokey,0,NIL)
# (SOCKET-WAIT socket-server [seconds [microseconds]])
  {
    #if defined(HAVE_SELECT) || defined(WIN32_NATIVE)
    var SOCKET handle;
    var struct timeval timeout;
    var struct timeval * timeout_ptr;
    test_socket_server(STACK_2,TRUE);
    handle = TheSocket(TheSocketServer(STACK_2)->socket_handle);
   restart_select:
    if (eq(STACK_1,unbound)) {
      timeout_ptr = NULL;
    } else {
      if (!posfixnump(STACK_1))
        fehler_posfixnum(STACK_1);
      timeout.tv_sec = posfixnum_to_L(STACK_1);
      if (eq(STACK_0,unbound)) {
        timeout.tv_usec = 0;
      } else {
        if (!posfixnump(STACK_0))
          fehler_posfixnum(STACK_0);
        timeout.tv_usec = posfixnum_to_L(STACK_0);
      }
      timeout_ptr = &timeout;
    }
    begin_system_call();
    {
      var int ret;
      var fd_set handle_set;
      FD_ZERO(&handle_set); FD_SET(handle,&handle_set);
      ret = select(FD_SETSIZE,&handle_set,NULL,NULL,timeout_ptr);
      if (ret < 0) {
        if (sock_errno_is(EINTR)) {
          end_system_call(); goto restart_select;
        }
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

extern SOCKET create_client_socket (const char* host, unsigned int port);

LISPFUN(socket_connect,1,1,norest,key,3,\
        (kw(element_type),kw(external_format),kw(buffered)) )
# (SOCKET-CONNECT port [host] [:element-type] [:external-format] [:buffered])
  {
    var char *hostname;
    var decoded_eltype eltype;
    var signean buffered;
    var SOCKET handle;

    if (!posfixnump(STACK_4))
      fehler_posfixnum(STACK_4);

    # Check and canonicalize the :BUFFERED argument:
    buffered = test_buffered_arg(STACK_0); # default is NIL

    # Check and canonicalize the :ELEMENT-TYPE argument:
    test_eltype_arg(&STACK_2,&eltype);
    STACK_2 = canon_eltype(&eltype);
    if (buffered <= 0) { check_unbuffered_eltype(&eltype); }

    # Check and canonicalize the :EXTERNAL-FORMAT argument:
    STACK_1 = test_external_format_arg(STACK_1);

    if (eq(STACK_3,unbound))
      hostname = "localhost";
    elif (stringp(STACK_3))
      hostname = TheAsciz(string_to_asciz(STACK_3,O(misc_encoding)));
    else
      fehler_string(STACK_3);

    begin_system_call();
    handle = create_client_socket(hostname,posfixnum_to_L(STACK_4));
    if (handle == INVALID_SOCKET) { SOCK_error(); }
    end_system_call();
    value1 = make_socket_stream(handle,&eltype,buffered,
                                asciz_to_string(hostname,O(misc_encoding)),STACK_4);
    skipSTACK(5);
    mv_count = 1;
  }

local object test_socket_stream (object obj, boolean check_open);
local object test_socket_stream(obj,check_open)
  var object obj;
  var boolean check_open;
  {
    if (builtin_stream_p(obj)) {
      switch (TheStream(obj)->strmtype) {
        case strmtype_twoway_socket:
          obj = TheStream(obj)->strm_twoway_socket_input;
          /*FALLTHROUGH*/
        case strmtype_socket:
          if (check_open && ((TheStream(obj)->strmflags & strmflags_open_B) == 0)) {
            pushSTACK(obj);
            pushSTACK(S(stream)); # ??
            pushSTACK(obj);
            pushSTACK(TheSubr(subr_self)->name);
            fehler(type_error,
                   GETTEXT("~: argument ~ is not an open SOCKET-STREAM")
                  );
          }
          return obj;
        default:
          break;
      }
    }
    pushSTACK(obj);
    pushSTACK(S(stream)); # ??
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~: argument ~ is not a SOCKET-STREAM")
          );
  }

LISPFUNN(socket_stream_port,1)
# (SOCKET-STREAM-PORT socket-stream)
  {
    var object stream = test_socket_stream(STACK_0,FALSE);
    value1 = TheStream(stream)->strm_socket_port;
    skipSTACK(1);
    mv_count=1;
  }

LISPFUNN(socket_stream_host,1)
# (SOCKET-STREAM-HOST socket-stream)
  {
    var object stream = test_socket_stream(STACK_0,FALSE);
    value1 = TheStream(stream)->strm_socket_host;
    skipSTACK(1);
    mv_count=1;
  }

extern host_data * socket_getpeername (SOCKET socket_handle, host_data * hd);
extern host_data * socket_getlocalname (SOCKET socket_handle, host_data * hd);

typedef host_data * host_data_fetcher (SOCKET, host_data *);
local void publish_host_data (host_data_fetcher* func);
local void publish_host_data (func)
  var host_data_fetcher* func;
  {
    var object stream = test_socket_stream(popSTACK(),TRUE);
    var SOCKET sk = TheSocket(TheStream(stream)->strm_ichannel);
    var host_data hd;

    begin_system_call();
    if ((*func)(sk, &hd) == NULL) { SOCK_error(); }
    end_system_call();
    if (hd.truename[0] == '\0') {
      value1 = asciz_to_string(hd.hostname,O(misc_encoding));
    } else {
      var DYNAMIC_ARRAY(tmp_str,char,strlen(hd.truename)+2+strlen(hd.hostname)+1+1);
      strcpy(tmp_str, hd.hostname);
      strcat(tmp_str, " (");
      strcat(tmp_str, hd.truename);
      strcat(tmp_str, ")");
      value1 = asciz_to_string(tmp_str,O(misc_encoding));
      FREE_DYNAMIC_ARRAY(tmp_str);
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
    var object stream = test_socket_stream(STACK_0,TRUE);
    value1 = fixnum(TheSocket(TheStream(stream)->strm_ichannel));
    skipSTACK(1);
    mv_count=1;
  }

#endif

#endif


# Streams allgemein
# =================

# UP: Return the default value for *terminal-io*.
# can trigger GC
  local object make_terminal_io (void);
  local object make_terminal_io()
    {
      # If stdin or stdout is a file, use a buffered stream instead of an
      # unbuffered terminal stream. For the ud2cd program used as filter,
      # this reduces the runtime on Solaris from 165 sec to 47 sec.
      var boolean stdin_file = regular_handle_p(stdin_handle);
      var boolean stdout_file = regular_handle_p(stdout_handle);
      if (stdin_file || stdout_file) {
        var object stream;
        # Allocate stream for stdin:
        if (stdin_file) {
          #ifdef UNIX
          pushSTACK(ascii_to_string("/dev/fd/0")); funcall(L(pathname),1);
          pushSTACK(value1);
          #else
          pushSTACK(NIL);
          #endif
          pushSTACK(NIL);
          pushSTACK(S(Kdefault));
          pushSTACK(S(Kdefault)); # not O(terminal-encoding), since it's a file
          pushSTACK(S(character));
          pushSTACK(allocate_handle(stdin_handle));
          stream = make_file_stream(1,FALSE,FALSE);
        } else {
          stream = make_terminal_stream();
        }
        pushSTACK(stream);
        # Allocate stream for stdout:
        if (stdout_file) {
          #ifdef UNIX
          pushSTACK(ascii_to_string("/dev/fd/1")); funcall(L(pathname),1);
          pushSTACK(value1);
          #else
          pushSTACK(NIL);
          #endif
          pushSTACK(NIL);
          pushSTACK(S(Kdefault));
          pushSTACK(S(Kdefault)); # not O(terminal-encoding), since it's a file
          pushSTACK(S(character));
          pushSTACK(allocate_handle(stdout_handle));
          stream = make_file_stream(4,FALSE,FALSE);
        } else {
          stream = make_terminal_stream();
        }
        # Build a two-way-stream:
        return make_twoway_stream(popSTACK(),stream);
      }
      return make_terminal_stream();
    }

# UP: Initialisiert die Stream-Variablen.
# init_streamvars(unixyp);
# > unixyp: Flag, ob *error-output* nach Unix-Art (vom Standard abweichend)
#           initialisiert werden soll
# can trigger GC
  global void init_streamvars (boolean unixyp);
  global void init_streamvars(unixyp)
    var boolean unixyp;
    {
      #ifdef GNU_READLINE
      begin_call();
      rl_readline_name = "Clisp";
      if (ilisp_mode) {
        # Simuliere folgende Anweisung im .inputrc:
        #   Control-i: self-insert
        rl_bind_key(CTRL('I'),rl_named_function("self-insert"));
      }
      rl_attempted_completion_function = &lisp_completion_matches;
      rl_completion_entry_function = &lisp_completion_more;
      #ifdef NO_MATCH  # readline-2.2-clisp or newer
      _rl_comment_begin = ";";
      #endif
      end_call();
      #endif
      {
        var object stream = make_terminal_io();
        define_variable(S(terminal_io),stream);      # *TERMINAL-IO*
      }
      {
        var object stream = make_synonym_stream(S(terminal_io)); # Synonym-Stream auf *TERMINAL-IO*
        define_variable(S(query_io),stream);         # *QUERY-IO*
        define_variable(S(debug_io),stream);         # *DEBUG-IO*
        define_variable(S(standard_input),stream);   # *STANDARD-INPUT*
        define_variable(S(standard_output),stream);  # *STANDARD-OUTPUT*
        define_variable(S(trace_output),stream);     # *TRACE-OUTPUT*
        #ifdef UNIX
        if (unixyp) {
          # Für *ERROR-OUTPUT* einen anderen Stream verwenden. Auf den
          # Filenamen kommt es nicht an, /dev/fd/2 existiert auch nicht überall.
          pushSTACK(ascii_to_string("/dev/fd/2")); funcall(L(pathname),1);
          pushSTACK(value1);
          pushSTACK(test_external_format_arg(S(Kunix)));
          pushSTACK(S(character));
          pushSTACK(allocate_handle(2));
          var decoded_eltype eltype = { eltype_ch, 0 };
          stream = make_unbuffered_stream(strmtype_file,4,&eltype,FALSE);
          UnbufferedHandleStream_output_init(stream);
          ChannelStreamLow_close(stream) = &low_close_handle;
          TheStream(stream)->strm_file_name =
          TheStream(stream)->strm_file_truename = popSTACK();
        }
        #endif
        define_variable(S(error_output),stream);     # *ERROR-OUTPUT*
      }
      #ifdef KEYBOARD
        # Initialize the *KEYBOARD-INPUT* stream. This can fail in some cases,
        # therefore we do it after the standard streams are in place, so that
        # the user will get a reasonable error message.
        #if defined(UNIX) || defined(RISCOS)
          # Building the keyboard stream is a costly operation. Delay it
          # until we really need it.
          define_variable(S(keyboard_input),NIL);     # *KEYBOARD-INPUT*
        #else
        {
          var object stream = make_keyboard_stream();
          define_variable(S(keyboard_input),stream); # *KEYBOARD-INPUT*
        }
        #endif
      #endif
    }

# Liefert Fehlermeldung, wenn der Wert des Symbols sym kein Stream ist.
  local void fehler_value_stream(sym)
    var object sym;
    {
      # Vor der Fehlermeldung eventuell noch reparieren
      # (so wie bei init_streamvars bzw. init_pathnames initialisiert):
      var object stream;
      pushSTACK(sym); # sym retten
      #ifdef KEYBOARD
      if (eq(sym,S(keyboard_input))) {
        # Keyboard-Stream als Default
        stream = make_keyboard_stream();
      } else
      #endif
      if (eq(sym,S(terminal_io))) {
        # Terminal-Stream als Default
        # (Use make_terminal_stream() here, not make_terminal_io(), because
        # that might have been a file stream and got closed when the disk
        # became full.)
        stream = make_terminal_stream();
      } elif (eq(sym,S(query_io)) || eq(sym,S(debug_io)) ||
              eq(sym,S(standard_input)) || eq(sym,S(standard_output)) ||
              eq(sym,S(error_output)) || eq(sym,S(trace_output))
             ) {
        # Synonym-Stream auf *TERMINAL-IO* als Default
        stream = make_synonym_stream(S(terminal_io));
      } else {
        # sonstiges Symbol, nicht reparierbar -> sofort Fehlermeldung:
        pushSTACK(Symbol_value(sym)); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(Symbol_value(sym)); # Variablenwert
        pushSTACK(sym); # Variable
        if (!streamp(Symbol_value(sym))) {
          fehler(type_error,
                 GETTEXT("The value of ~ is not a stream: ~")
                );
        } else {
          fehler(type_error,
                 GETTEXT("The value of ~ is not an appropriate stream: ~")
                );
        }
      }
      sym = popSTACK();
      # Reparatur beendet: stream ist der neue Wert von sym.
      var object oldvalue = Symbol_value(sym);
      Symbol_value(sym) = stream;
      pushSTACK(oldvalue); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(stream); # neuer Variablenwert
      pushSTACK(oldvalue); # alter Variablenwert
      pushSTACK(sym); # Variable
      fehler(type_error,
             GETTEXT("The value of ~ was not a stream: ~. It has been changed to ~.")
            );
    }

#ifdef GNU_READLINE

# Hilfsfunktionen für die GNU readline Library:

nonreturning_function(local, rl_memory_abort, (void));
local void rl_memory_abort()
  {
    # Wenn für die Readline-Library der Speicher nicht mehr reicht,
    # schmeißen wir sie raus und ersetzen den Terminal-Stream durch einen,
    # der ohne sie auskommt.
    rl_deprep_terminal(); # alle komischen ioctl()s rückgängig machen
    begin_callback(); # STACK wieder auf einen vernünftigen Wert setzen
    rl_present_p = FALSE;
    Symbol_value(S(terminal_io)) = make_terminal_stream();
    fehler(storage_condition,
           GETTEXT("readline library: out of memory.")
          );
  }

global char* xmalloc (int count);
global char* xmalloc(count)
  var int count;
  {
    var char* tmp = (char*)malloc(count);
    if (tmp)
      return tmp;
    else
      rl_memory_abort();
  }

#ifdef NO_MATCH  # readline-2.2-clisp or newer
  global char* xrealloc (void* ptr, int count);
  global char* xrealloc(ptr,count)
    var void* ptr;
    var int count;
    {
      var char* tmp = (ptr==NULL ? (char*)malloc(count) : (char*)realloc((char*)ptr,count));
      if (tmp)
        return tmp;
      else
        rl_memory_abort();
    }
#else
  global char* xrealloc (char* ptr, int count);
  global char* xrealloc(ptr,count)
    var char* ptr;
    var int count;
    {
      var char* tmp = (ptr==NULL ? (char*)malloc(count) : (char*)realloc(ptr,count));
      if (tmp)
        return tmp;
      else
        rl_memory_abort();
    }
#endif

#endif

LISPFUNN(built_in_stream_open_p,1)
# (SYS::BUILT-IN-STREAM-OPEN-P stream)
  {
    var object stream = popSTACK();
    if (!builtin_stream_p(stream))
      fehler_streamtype(stream,O(type_builtin_stream));
    if (TheStream(stream)->strmflags & strmflags_open_B) { # Stream offen?
      value1 = T; mv_count=1; # Wert T
    } else {
      value1 = NIL; mv_count=1; # Wert NIL
    }
  }

LISPFUNN(input_stream_p,1)
# (INPUT-STREAM-P stream), CLTL S. 332, CLtL2 S. 505
  {
    var object stream = popSTACK();
    if (!streamp(stream))
      fehler_stream(stream);
    if (input_stream_p(stream)) {
      value1 = T; mv_count=1; # Wert T
    } else {
      value1 = NIL; mv_count=1; # Wert NIL
    }
  }

LISPFUNN(output_stream_p,1)
# (OUTPUT-STREAM-P stream), CLTL S. 332, CLtL2 S. 505
  {
    var object stream = popSTACK();
    if (!streamp(stream))
      fehler_stream(stream);
    if (output_stream_p(stream)) {
      value1 = T; mv_count=1; # Wert T
    } else {
      value1 = NIL; mv_count=1; # Wert NIL
    }
  }

LISPFUNN(built_in_stream_element_type,1)
# (SYS::BUILT-IN-STREAM-ELEMENT-TYPE stream)
# liefert NIL (für geschlossene Streams) oder CHARACTER oder INTEGER oder T
# oder (spezieller) (UNSIGNED-BYTE n) oder (SIGNED-BYTE n).
  {
    var object stream = popSTACK();
    if (!builtin_stream_p(stream))
      fehler_streamtype(stream,O(type_builtin_stream));
    var object eltype;
    if ((TheStream(stream)->strmflags & strmflags_open_B) == 0) {
      # Stream geschlossen
      eltype = NIL;
    } else {
      # Stream offen
      switch (TheStream(stream)->strmtype) {
        # erst die Streamtypen mit eingeschränkten Element-Typen:
        case strmtype_str_in:
        case strmtype_str_out:
        case strmtype_str_push:
        case strmtype_pphelp:
        case strmtype_buff_in:
        case strmtype_buff_out:
          # CHARACTER
          eltype = S(character); break;
        #ifdef KEYBOARD
        case strmtype_keyboard:
          eltype = T;
          break;
        #endif
        case strmtype_terminal:
        #ifdef SCREEN
        case strmtype_window:
        #endif
        #ifdef PRINTER
        case strmtype_printer:
        #endif
          # CHARACTER
          eltype = S(character); break;
        case strmtype_file:
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
          # CHARACTER or ([UN]SIGNED-BYTE n)
          eltype = TheStream(stream)->strm_eltype; break;
        #ifdef SOCKET_STREAMS
        case strmtype_twoway_socket:
          # CHARACTER or ([UN]SIGNED-BYTE n)
          stream = TheStream(stream)->strm_twoway_socket_input;
          eltype = TheStream(stream)->strm_eltype; break;
        #endif
        # dann die allgemeinen Streams:
        #ifdef GENERIC_STREAMS
        case strmtype_generic:
        #endif
        default:
          {
            var uintB flags = TheStream(stream)->strmflags;
            if (flags & strmflags_by_B) {
              if (flags & strmflags_ch_B) {
                # (OR CHARACTER INTEGER)
                pushSTACK(S(or)); pushSTACK(S(character)); pushSTACK(S(integer));
                eltype = listof(3);
              } else {
                # INTEGER
                eltype = S(integer);
              }
            } else {
              if (flags & strmflags_ch_B) {
                # CHARACTER
                eltype = S(character);
              } else {
                # NIL
                eltype = NIL;
              }
            }
          }
          break;
      }
    }
    value1 = eltype; mv_count=1;
  }

LISPFUNN(built_in_stream_set_element_type,2)
# (SYSTEM::BUILT-IN-STREAM-SET-ELEMENT-TYPE stream element-type)
  {
    var object stream = STACK_1;
    if (!builtin_stream_p(stream))
      fehler_streamtype(stream,O(type_builtin_stream));
    var decoded_eltype eltype;
    test_eltype_arg(&STACK_0,&eltype);
    pushSTACK(canon_eltype(&eltype));
    # Stack contents: stream, element-type, canon-element-type.
    stream = STACK_2;
   start:
    switch (TheStream(stream)->strmtype) {
      case strmtype_synonym:
        # Synonym-Stream: weiterverfolgen
        {
          var object symbol = TheStream(stream)->strm_synonym_symbol;
          stream = get_synonym_stream(symbol);
          if (builtin_stream_p(stream))
            goto start;
          else {
            # Call ((SETF STREAM-ELEMENT-TYPE) element-type stream):
            pushSTACK(STACK_1); pushSTACK(stream);
            funcall(O(setf_stream_element_type),2);
          }
        }
        break;
      case strmtype_file:
      #ifdef PIPES
      case strmtype_pipe_in:
      case strmtype_pipe_out:
      #endif
      #ifdef SOCKET_STREAMS
      case strmtype_socket:
      #endif
        if (!equal(STACK_0,TheStream(stream)->strm_eltype)) { # nothing to change?
          # Check eltype.
          if (!ChannelStream_buffered(stream))
            check_unbuffered_eltype(&eltype);
          # The FILE-POSITION return value is constrained by CLHS to
          #   - be an integer,
          #   - represent a position into the file (and therefore be
          #     independent of the stream's current element type),
          #   - increment by 1 when READ-BYTE or WRITE-BYTE is called.
          # In order to achieve these constraints altogether, we allow
          # switching only (UNSIGNED-BYTE n) and (SIGNED-BYTE n) with
          # the same n, and between ([UN]SIGNED-BYTE 8) and CHARACTER.
          # Reading (UNSIGNED-BYTE 8) and (UNSIGNED-BYTE 16) and
          # (UNSIGNED-BYTE 32) values from the same stream in succession
          # can be achieved through READ-INTEGER and WRITE-INTEGER.
          if (!((ChannelStream_bitsize(stream) > 0 ? ChannelStream_bitsize(stream) : 8)
                ==
                (eltype.size > 0 ? eltype.size : 8)
             ) ) {
            # canon-element-type in STACK_0.
            pushSTACK(TheStream(stream)->strm_eltype);
            pushSTACK(stream);
            pushSTACK(S(Kelement_type));
            pushSTACK(O(setf_stream_element_type));
            fehler(error,
                   GETTEXT("~: The ~ of ~ cannot be changed from ~ to ~.")
                  );
          }
          # Transform the lastchar back, if possible.
          if (TheStream(stream)->strmflags & strmflags_open_B) # stream open?
            if (eltype.size > 0)
              # New element type is an integer type.
              if (ChannelStream_bitsize(stream) == 0) {
                # Old element type was CHARACTER.
                # Transform the lastchar back to bytes.
                if (charp(TheStream(stream)->strm_rd_ch_last)
                    && (TheStream(stream)->strmflags & strmflags_unread_B)
                   ) {
                  # FIXME: This should take into account the encoding.
                  var uintB b = as_cint(char_code(TheStream(stream)->strm_rd_ch_last));
                  if (ChannelStream_buffered(stream)) {
                    if ((BufferedStream_index(stream) > 0)
                        && (BufferedStream_position(stream) > 0)
                        && (TheSbvector(TheStream(stream)->strm_buffered_buffer)->data[BufferedStream_index(stream)-1] == b)
                       ) {
                      # index und position decrementieren:
                      BufferedStream_index(stream) -= 1;
                      BufferedStream_position(stream) -= 1;
                      TheStream(stream)->strm_rd_ch_last = NIL;
                      TheStream(stream)->strmflags &= ~strmflags_unread_B;
                    }
                  } else {
                    if (UnbufferedStream_status(stream) == 0) {
                      UnbufferedStreamLow_push_byte(stream,b);
                      UnbufferedStream_ignore_next_LF(stream) = FALSE;
                      TheStream(stream)->strm_rd_ch_last = NIL;
                      TheStream(stream)->strmflags &= ~strmflags_unread_B;
                    }
                  }
                }
              }
          # Actually change the stream's element type.
          {
            var uintB flags = TheStream(stream)->strmflags;
            flags = (flags & ~strmflags_open_B)
                    | (flags & strmflags_rd_B ? strmflags_rd_B : 0)
                    | (flags & strmflags_wr_B ? strmflags_wr_B : 0);
            ChannelStream_bitsize(stream) = eltype.size;
            if (eltype.kind == eltype_ch) {
              # New element type is CHARACTER.
              flags &= ~(strmflags_open_B & ~strmflags_ch_B);
            } else {
              # New element type is an integer type.
              # Bitbuffer allozieren:
              pushSTACK(stream);
              var object bitbuffer = allocate_bit_vector(Atype_Bit,eltype.size);
              stream = popSTACK();
              TheStream(stream)->strm_bitbuffer = bitbuffer;
              flags &= ~(strmflags_open_B & ~strmflags_by_B);
            }
            TheStream(stream)->strmflags = flags;
          }
          if (ChannelStream_buffered(stream)) {
            fill_pseudofuns_buffered(stream,&eltype);
          } else {
            fill_pseudofuns_unbuffered(stream,&eltype);
            UnbufferedStream_ignore_next_LF(stream) = FALSE;
          }
          TheStream(stream)->strm_eltype = STACK_0;
        }
        break;
      #ifdef SOCKET_STREAMS
      case strmtype_twoway_socket:
        # Apply to the input and output side individually.
        pushSTACK(TheStream(STACK_1)->strm_twoway_socket_input); pushSTACK(STACK_(0+1));
        funcall(L(built_in_stream_set_element_type),2);
        pushSTACK(TheStream(STACK_1)->strm_twoway_socket_output); pushSTACK(STACK_(0+1));
        funcall(L(built_in_stream_set_element_type),2);
        break;
      #endif
      default:
        fehler_illegal_streamop(O(setf_stream_element_type),stream);
    }
    value1 = STACK_1; mv_count=1;
    skipSTACK(3);
  }

LISPFUNN(stream_external_format,1)
# (STREAM-EXTERNAL-FORMAT stream)
  {
    var object stream = popSTACK();
    if (!streamp(stream))
      fehler_stream(stream);
   start:
    if (builtin_stream_p(stream))
      switch (TheStream(stream)->strmtype) {
        case strmtype_synonym:
          # Synonym-Stream: weiterverfolgen
          {
            var object symbol = TheStream(stream)->strm_synonym_symbol;
            stream = get_synonym_stream(symbol);
            goto start;
          }
        case strmtype_file:
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
          value1 = TheStream(stream)->strm_encoding; break;
        default:
          value1 = S(Kdefault); break;
      }
    else
      value1 = S(Kdefault);
    mv_count=1;
  }

LISPFUN(set_stream_external_format,2,1,norest,nokey,0,NIL)
# (SYSTEM::SET-STREAM-EXTERNAL-FORMAT stream external-format [direction])
# direction can be :INPUT or :OUTPUT or NIL.
# If no direction is given, the operation is nonrecursive.
  {
    var object stream = STACK_2;
    if (!streamp(stream))
      fehler_stream(stream);
    var object encoding = test_external_format_arg(STACK_1);
    var object direction = STACK_0;
   start:
    if (builtin_stream_p(stream))
      switch (TheStream(stream)->strmtype) {
        case strmtype_synonym:
          # Synonym-Stream: weiterverfolgen
          {
            var object symbol = TheStream(stream)->strm_synonym_symbol;
            stream = get_synonym_stream(symbol);
            goto start;
          }
        case strmtype_broad:
          if (eq(direction,S(Kinput)))
            goto done;
          if (eq(direction,S(Koutput))) {
            # Recurse.
            check_SP(); check_STACK();
            pushSTACK(TheStream(stream)->strm_broad_list);
            while (consp(STACK_0)) {
              pushSTACK(Car(STACK_0)); pushSTACK(STACK_(1+2)); pushSTACK(STACK_(0+3));
              C_set_stream_external_format();
              STACK_0 = Cdr(STACK_0);
            }
            skipSTACK(1);
            encoding = STACK_1;
            goto done;
          }
          goto unchangeable_external_format;
        case strmtype_concat:
          if (eq(direction,S(Kinput))) {
            # Recurse.
            check_SP(); check_STACK();
            pushSTACK(TheStream(stream)->strm_concat_totallist);
            while (consp(STACK_0)) {
              pushSTACK(Car(STACK_0)); pushSTACK(STACK_(1+2)); pushSTACK(STACK_(0+3));
              C_set_stream_external_format();
              STACK_0 = Cdr(STACK_0);
            }
            skipSTACK(1);
            encoding = STACK_1;
            goto done;
          }
          if (eq(direction,S(Koutput)))
            goto done;
          goto unchangeable_external_format;
        case strmtype_twoway:
        case strmtype_echo:
          if (eq(direction,S(Kinput))) {
            # Recurse.
            stream = TheStream(stream)->strm_twoway_input; goto start;
          }
          if (eq(direction,S(Koutput))) {
            # Recurse.
            stream = TheStream(stream)->strm_twoway_output; goto start;
          }
          goto unchangeable_external_format;
        case strmtype_str_in:
        case strmtype_str_out:
        case strmtype_str_push:
        case strmtype_pphelp:
        case strmtype_buff_in:
        case strmtype_buff_out:
        #ifdef GENERIC_STREAMS
        case strmtype_generic:
        #endif
          if (eq(direction,S(Kinput)) || eq(direction,S(Koutput)))
            goto done;
          goto unchangeable_external_format;
        case strmtype_file:
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
          {
            var decoded_eltype eltype;
            test_eltype_arg(&TheStream(stream)->strm_eltype,&eltype); # no GC here!
            ChannelStream_fini(stream);
            value1 = TheStream(stream)->strm_encoding = encoding;
            if (ChannelStream_buffered(stream)) {
              fill_pseudofuns_buffered(stream,&eltype);
            } else {
              fill_pseudofuns_unbuffered(stream,&eltype);
              UnbufferedStream_ignore_next_LF(stream) = FALSE;
            }
            ChannelStream_init(stream);
          }
          break;
        #ifdef SOCKET_STREAMS
        case strmtype_twoway_socket:
          if (eq(direction,S(Kinput))) {
            # Recurse.
            stream = TheStream(stream)->strm_twoway_input; goto start;
          }
          if (eq(direction,S(Koutput))) {
            # Recurse.
            stream = TheStream(stream)->strm_twoway_output; goto start;
          }
          # Recurse twice.
          pushSTACK(TheStream(stream)->strm_twoway_output); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
          pushSTACK(TheStream(stream)->strm_twoway_input); pushSTACK(STACK_(1+4)); pushSTACK(STACK_(0+5));
          C_set_stream_external_format();
          C_set_stream_external_format();
          encoding = STACK_1;
          goto done;
        #endif
        default:
          if (eq(direction,S(Kinput)))
            if ((TheStream(stream)->strmflags & strmflags_rd_B) == 0)
              goto done;
          if (eq(direction,S(Koutput)))
            if ((TheStream(stream)->strmflags & strmflags_wr_B) == 0)
              goto done;
        unchangeable_external_format:
          if (!eq(encoding,S(Kdefault)))
            fehler_illegal_streamop(S(set_stream_external_format),stream);
        done:
          value1 = encoding; break;
      }
    else {
      if (eq(direction,S(Kinput)))
        if (!instanceof(stream,O(class_fundamental_input_stream)))
          goto done2;
      if (eq(direction,S(Koutput)))
        if (!instanceof(stream,O(class_fundamental_output_stream)))
          goto done2;
      if (!eq(encoding,S(Kdefault)))
        fehler_illegal_streamop(S(set_stream_external_format),stream);
     done2:
      value1 = encoding;
    }
    mv_count=1;
    skipSTACK(3);
  }

#ifdef UNICODE

# Changes a terminal stream's external format.
# > stream: a stream
# > encoding: an encoding
# can trigger GC
  global void set_terminalstream_external_format (object stream, object encoding);
  global void set_terminalstream_external_format(stream,encoding)
    var object stream;
    var object encoding;
    {
      if (builtin_stream_p(stream)
          && TheStream(stream)->strmtype == strmtype_terminal
          && eq(TheStream(stream)->strm_encoding,O(terminal_encoding))
         ) {
        # This is the only place which is allowed to modify the terminal
        # stream's encoding.
        # The terminal stream's end-of-line coding is hardwired, therefore we
        # don't neet to do the equivalent of fill_pseudofuns_unbuffered here.
        ChannelStream_fini(stream);
        TheStream(stream)->strm_encoding = encoding;
        ChannelStream_init(stream);
      } else {
        pushSTACK(stream); pushSTACK(encoding);
        funcall(L(set_stream_external_format),2);
      }
    }

#endif

# UP: Stellt fest, ob ein Stream "interaktiv" ist, d.h. ob Input vom Stream
# vermutlich von einem vorher ausgegebenen Prompt abhängen wird.
# interactive_stream_p(stream)
# > stream: Stream
# NB: Relation zwischen clear_input, listen, interactive_stream_p:
#   Wenn nach clear_input(stream) ls_wait_p(stream_listen(stream)) ist
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
#   ls_avail liefern.
  global boolean interactive_stream_p (object stream);
  global boolean interactive_stream_p(stream)
    var object stream;
    {
     start:
      if (!builtin_stream_p(stream))
        # Assume the worst.
        return TRUE;
      if ((TheStream(stream)->strmflags & strmflags_rd_B) == 0)
        # Stream für Input geschlossen
        return FALSE;
      # Stream offen
      switch (TheStream(stream)->strmtype) {
        case strmtype_synonym:
          # Synonym-Stream: weiterverfolgen
          {
            var object symbol = TheStream(stream)->strm_synonym_symbol;
            stream = get_synonym_stream(symbol);
            /* return interactive_stream_p(stream); */ # entrekursiviert:
            goto start;
          }
        case strmtype_concat:
          # Hier könnte man stream_listen(stream) machen, um Streams, die
          # am EOF angelangt sind, zu ignorieren. Aber es ist nicht gut,
          # wenn interactive_stream_p Systemaufrufe und I/O macht.??
          # Den ersten der Streams abfragen:
          {
            var object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
            if (consp(streamlist)) {
              stream = Car(streamlist);
              /* return interactive_stream_p(stream); */ # entrekursiviert:
              goto start;
            } else
              return FALSE;
          }
        case strmtype_twoway:
        case strmtype_echo:
          # Two-Way-Stream oder Echo-Stream: Input-Stream anschauen
          stream = TheStream(stream)->strm_twoway_input;
          /* return interactive_stream_p(stream); */ # entrekursiviert:
           goto start;
        case strmtype_str_in:
          return FALSE;
        case strmtype_buff_in:
        #ifdef GENERIC_STREAMS
        case strmtype_generic:
        #endif
          return TRUE;
        #if !defined(NEXTAPP)
        case strmtype_terminal:
        #endif
        case strmtype_file:
          if (ChannelStream_buffered(stream))
            # Buffered file streams are not considered to be interactive.
            return FALSE;
          if (nullp(TheStream(stream)->strm_isatty))
            # Reguläre Files sind sicher nicht interaktiv.
            if (regular_handle_p(TheHandle(TheStream(stream)->strm_ichannel)))
              return FALSE;
        #ifdef KEYBOARD
        case strmtype_keyboard:
        #endif
        #if defined(NEXTAPP)
        case strmtype_terminal:
        #endif
        #ifdef PIPES
        case strmtype_pipe_in:
        #endif
        #ifdef X11SOCKETS
        case strmtype_x11socket:
        #endif
        #ifdef SOCKET_STREAMS
        case strmtype_socket:
        case strmtype_twoway_socket:
        #endif
          return TRUE;
        default:
          return FALSE;
      }
    }

LISPFUNN(interactive_stream_p,1)
# (INTERACTIVE-STREAM-P stream), CLTL2 S. 507/508
# stellt fest, ob stream interaktiv ist.
  {
    var object arg = popSTACK();
    if (!streamp(arg))
      fehler_stream(arg);
    value1 = (interactive_stream_p(arg) ? T : NIL); mv_count=1;
  }

# UP: Schließt einen Stream.
# builtin_stream_close(&stream);
# > stream: Builtin-Stream
# < stream: Builtin-Stream
# can trigger GC
  global void builtin_stream_close (const object* stream_);
  global void builtin_stream_close(stream_)
    var const object* stream_;
    {
      var object stream = *stream_;
      if ((TheStream(stream)->strmflags & strmflags_open_B) == 0) # Stream schon geschlossen?
        return;
      # Typspezifische Routine aufrufen (darf GC auslösen):
      switch (TheStream(stream)->strmtype) {
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
        #ifdef GENERIC_STREAMS
        case strmtype_generic:
          close_generic(stream); break;
        #endif
        case strmtype_file:
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
          if (ChannelStream_buffered(stream)) {
            close_buffered(stream);
          } else {
            if (TheStream(stream)->strmflags & strmflags_wr_B)
              close_ochannel(stream);
            else
              close_ichannel(stream);
          }
          break;
        #ifdef SOCKET_STREAMS
        case strmtype_twoway_socket:
          # Close the two substreams, but close the handle once only.
          ChannelStreamLow_close(TheStream(stream)->strm_twoway_socket_input) = &low_close_socket_nop;
          pushSTACK(TheStream(stream)->strm_twoway_socket_input);
          pushSTACK(TheStream(stream)->strm_twoway_socket_output);
          builtin_stream_close(&STACK_1);
          builtin_stream_close(&STACK_0);
          skipSTACK(2);
          break;
        #endif
        #ifdef KEYBOARD
        case strmtype_keyboard: break;
        #endif
        case strmtype_terminal: break;
        #ifdef SCREEN
        case strmtype_window:
          close_window(stream); break;
        #endif
        #ifdef PRINTER_AMIGAOS
        case strmtype_printer:
          close_printer(stream); break;
        #endif
        default: NOTREACHED
      }
      # Dummys eintragen:
      close_dummys(*stream_);
    }

# UP: Schließt eine Liste offener Files.
# close_some_files(list);
# > list: Liste von offenen Builtin-Streams
# can trigger GC
  global void close_some_files (object list);
  global void close_some_files(list)
    var object list;
    {
      pushSTACK(NIL); # dummy
      pushSTACK(list); # list
      while (mconsp(STACK_0)) {
        var object streamlist = STACK_0;
        STACK_0 = Cdr(streamlist); # restliche Streams
        STACK_1 = Car(streamlist); # ein Stream aus der Liste
        builtin_stream_close(&STACK_1); # schließen
      }
      skipSTACK(2);
    }

# UP: Schließt alle offenen Files.
# close_all_files();
# can trigger GC
  global void close_all_files (void);
  global void close_all_files()
    {
      close_some_files(O(open_files)); # Liste aller offenen File-Streams
    }

# UP: Erklärt alle offenen File-Streams für geschlossen.
# closed_all_files();
  global void closed_all_files (void);
  global void closed_all_files()
    {
      var object streamlist = O(open_files); # Liste aller offenen File-Streams
      while (consp(streamlist)) {
        var object stream = Car(streamlist); # ein Stream aus der Liste
        if (TheStream(stream)->strmtype == strmtype_file) { # File-Stream ?
          if (!nullp(TheStream(stream)->strm_buffered_channel)) # mit Handle /= NIL ?
            # ja: Stream noch offen
            closed_buffered(stream);
        }
        close_dummys(stream);
        streamlist = Cdr(streamlist); # restliche Streams
      }
      O(open_files) = NIL; # keine offenen Files mehr
    }

LISPFUN(built_in_stream_close,1,0,norest,key,1, (kw(abort)) )
# (SYS::BUILT-IN-STREAM-CLOSE stream :abort)
  {
    skipSTACK(1); # :ABORT-Argument ignorieren
    var object stream = STACK_0; # Argument
    if (!builtin_stream_p(stream)) # muss ein Stream sein
      fehler_streamtype(stream,O(type_builtin_stream));
    builtin_stream_close(&STACK_0); # schließen
    skipSTACK(1);
    value1 = T; mv_count=1; # T als Ergebnis
  }

# Reads a line of characters from a stream.
# read_line(&stream,&buffer)
# > stream: stream
# > buffer: a semi-simple string
# < stream: stream
# < buffer: contains the read characters, excluding the terminating #\Newline
# < result: TRUE is EOF was seen before newline, else FALSE
# can trigger GC
  global boolean read_line (const object* stream_, const object* buffer_);
  global boolean read_line(stream_,buffer_)
    var const object* stream_;
    var const object* buffer_;
    {
      var object stream = *stream_;
      if (builtin_stream_p(stream)) {
        if (TheStream(stream)->strmflags & strmflags_unread_B) { # Char nach UNREAD ?
          # ja -> Flagbit löschen und letztes Zeichen holen:
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
          var object ch = TheStream(stream)->strm_rd_ch_last;
          if (!charp(ch)) {
            subr_self = L(read_line); fehler_char(ch);
          }
          if (eq(ch,ascii_char(NL)))
            return FALSE;
          ssstring_push_extend(*buffer_,char_code(ch));
          stream = *stream_;
        }
        var uintL oldfillptr = TheIarray(*buffer_)->dims[1];
        var boolean eofp;
        switch (TheStream(stream)->strmtype) {
          case strmtype_synonym:
            eofp = read_line_synonym(stream,buffer_);
            break;
          case strmtype_twoway:
          #ifdef SOCKET_STREAMS
          case strmtype_twoway_socket:
          #endif
            eofp = read_line_twoway(stream,buffer_);
            break;
          # No special-casing of strmtype_echo, because the echo-stream may
          # be interactive, and delaying the echo in this case is undesirable.
          default:
            loop {
              var object ch = rd_ch(*stream_)(stream_); # nächstes Zeichen lesen
              if (eq(ch,eof_value)) { # EOF ?
                eofp = TRUE; break;
              }
              # sonst auf Character überprüfen:
              if (!charp(ch)) {
                subr_self = L(read_line); fehler_char(ch);
              }
              if (eq(ch,ascii_char(NL))) { # NL -> End of Line
                eofp = FALSE; break;
              }
              # sonstiges Character in den Buffer schreiben:
              ssstring_push_extend(*buffer_,char_code(ch));
            }
            break;
        }
        stream = *stream_;
        if (!eofp) {
          TheStream(stream)->strm_rd_ch_last = ascii_char(NL);
        } else {
          var uintL newfillptr = TheIarray(*buffer_)->dims[1];
          TheStream(stream)->strm_rd_ch_last =
            (newfillptr == oldfillptr ? code_char(TheSstring(TheIarray(*buffer_)->data)->data[newfillptr-1]) : eof_value);
        }
        TheStream(stream)->strmflags &= ~strmflags_unread_B;
        return eofp;
      } else {
        # Call the generic function (STREAM-READ-LINE stream):
        pushSTACK(stream); funcall(S(stream_read_line),1);
        if (!stringp(value1)) {
          pushSTACK(value1); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(S(string)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(S(stream_read_line));
          pushSTACK(value1);
          fehler(type_error,
                 GETTEXT("Return value ~ of call to ~ is not a string.")
                );
        }
        var boolean eofp = (mv_count >= 2 && !nullp(value2));
        # Add the line to the buffer:
        var uintL len;
        var uintL offset;
        var object srcstring = unpack_string_ro(value1,&len,&offset);
        ssstring_append_extend(*buffer_,srcstring,offset,len);
        return eofp;
      }
    }

# UP: Stellt fest, ob im Stream stream ein Zeichen sofort verfügbar ist.
# stream_listen(stream)
# > stream: Stream
# < ergebnis: ls_avail if a character is available,
#             ls_eof   if EOF is reached,
#             ls_wait  if no character is available, but not because of EOF
# can trigger GC
  global signean stream_listen (object stream);
  global signean stream_listen(stream)
    var object stream;
    {
      if (builtin_stream_p(stream)) {
        check_SP(); check_STACK();
        if (TheStream(stream)->strmflags & strmflags_unread_B) { # Char nach UNREAD ?
          return ls_avail; # ja -> verfügbar
        } else {
          # sonst nach Streamtyp verzweigen.
          # Jede Einzelroutine darf GC auslösen. Außer beim Keyboard-Stream
          # oder Terminal-Stream handelt es sich um einen reinen EOF-Test.
          switch (TheStream(stream)->strmtype) {
            case strmtype_synonym:  return listen_synonym(stream);
            case strmtype_broad:    return ls_eof; # kein READ-CHAR
            case strmtype_concat:   return listen_concat(stream);
            case strmtype_twoway:
            case strmtype_echo:
            #ifdef SOCKET_STREAMS
            case strmtype_twoway_socket:
            #endif
              return listen_twoway(stream);
            case strmtype_str_in:   return listen_str_in(stream);
            case strmtype_str_out:  return ls_eof; # kein READ-CHAR
            case strmtype_str_push: return ls_eof; # kein READ-CHAR
            case strmtype_pphelp:   return ls_eof; # kein READ-CHAR
            case strmtype_buff_in:  return listen_buff_in(stream);
            case strmtype_buff_out: return ls_eof; # kein READ-CHAR
            #ifdef GENERIC_STREAMS
            case strmtype_generic:  return listen_generic(stream);
            #endif
            case strmtype_file:
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
              if (TheStream(stream)->strmflags & strmflags_rd_ch_B) {
                if (ChannelStream_buffered(stream))
                  return listen_buffered(stream);
                else
                  return listen_unbuffered(stream);
              } else {
                return ls_eof; # kein READ-CHAR
              }
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
            #ifdef SCREEN
            case strmtype_window:   return ls_eof; # kein READ-CHAR
            #endif
            #ifdef PRINTER
            case strmtype_printer:  return ls_eof; # kein READ-CHAR
            #endif
            default: # Allgemein: nur EOF abfragen
              if (TheStream(stream)->strmflags & strmflags_rd_ch_B) {
                pushSTACK(stream);
                var object nextchar = peek_char(&STACK_0);
                skipSTACK(1);
                if (eq(nextchar,eof_value))
                  return ls_eof; # EOF erreicht
                else
                  return ls_avail;
              } else {
                return ls_eof; # kein READ-CHAR
              }
          }
        }
      } else {
        # Call the generic function (STREAM-READ-CHAR-WILL-HANG-P stream),
        # then call (PEEK-CHAR NIL STREAM):
        pushSTACK(stream);
        pushSTACK(stream); funcall(S(stream_read_char_will_hang_p),1);
        if (!nullp(value1)) {
          skipSTACK(1); return ls_wait;
        }
        var object nextchar = peek_char(&STACK_0);
        skipSTACK(1);
        if (eq(nextchar,eof_value))
          return ls_eof;
        else
          return ls_avail;
      }
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Stream stream.
# clear_input(stream)
# > stream: Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# can trigger GC
  global boolean clear_input (object stream);
  global boolean clear_input(stream)
    var object stream;
    {
      check_SP(); check_STACK();
      pushSTACK(stream); # Stream retten
      # Typspezifische Routine aufrufen (darf GC auslösen).
      if (builtin_stream_p(stream)) {
        # Nur beim Keyboard-Stream und Terminal-Stream wird etwas getan.
        var boolean ergebnis;
        switch (TheStream(stream)->strmtype) {
          case strmtype_synonym:
            ergebnis = clear_input_synonym(stream); break;
          case strmtype_concat:
            ergebnis = clear_input_concat(stream); break;
          case strmtype_twoway:
          case strmtype_echo:
          #ifdef SOCKET_STREAMS
          case strmtype_twoway_socket:
          #endif
            ergebnis = clear_input_twoway(stream); break;
          case strmtype_buff_in:
            ergebnis = clear_input_buff_in(stream); break;
          #ifdef GENERIC_STREAMS
          case strmtype_generic:
            ergebnis = clear_input_generic(stream); break;
          #endif
          case strmtype_file:
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
            if (TheStream(stream)->strmflags & strmflags_rd_ch_B
                && !ChannelStream_buffered(stream)
               )
              ergebnis = clear_input_unbuffered(stream);
            else
              ergebnis = FALSE;
            break;
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
          default:
            ergebnis = FALSE; break;
        }
        stream = popSTACK();
        if (ergebnis) {
          # Input wurde gelöscht -> auch das Lastchar muss gelöscht werden.
          # Dabei wird auch ein schon gesehenes EOF vergessen.
          TheStream(stream)->strm_rd_ch_last = NIL;
          TheStream(stream)->strmflags &= ~strmflags_unread_B;
        }
        return ergebnis;
      } else {
        # Call the generic function (STREAM-CLEAR-INPUT stream):
        funcall(S(stream_clear_input),1);
        return !nullp(value1);
      }
    }

# UP: Wartenden Output eines Stream stream ans Ziel bringen.
# finish_output(stream);
# > stream: Stream
# can trigger GC
  global void finish_output (object stream);
  global void finish_output(stream)
    var object stream;
    {
      if (builtin_stream_p(stream)) {
        if (TheStream(stream)->strmflags & strmflags_wr_B) { # Output-Stream?
          # nein -> fertig, ja -> nach Streamtyp verzweigen:
          switch (TheStream(stream)->strmtype) {
            case strmtype_synonym:
              finish_output_synonym(stream); break;
            case strmtype_broad:
              finish_output_broad(stream); break;
            case strmtype_twoway:
            case strmtype_echo:
            #ifdef SOCKET_STREAMS
            case strmtype_twoway_socket:
            #endif
              finish_output_twoway(stream); break;
            case strmtype_buff_out:
              finish_output_buff_out(stream); break;
            #ifdef GENERIC_STREAMS
            case strmtype_generic:
              finish_output_generic(stream); break;
            #endif
            case strmtype_file:
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
              if (ChannelStream_buffered(stream))
                finish_output_buffered(stream);
              else
                finish_output_unbuffered(stream);
              break;
            case strmtype_terminal:
              finish_output_terminal(stream); break;
            #ifdef PRINTER_AMIGAOS
            case strmtype_printer: # Printer:
              # Schließen und neu aufmachen würde vermutlich einen
              # Seitenvorschub ausgeben, und das ist ja wohl nicht erwünscht.
              break; # Daher nichts tun.
            #endif
            default: # nichts tun
              break;
          }
        }
      } else {
        # Call the generic function (STREAM-FINISH-OUTPUT stream):
        pushSTACK(stream); funcall(S(stream_finish_output),1);
      }
    }

# UP: Wartenden Output eines Stream stream ans Ziel bringen.
# force_output(stream);
# > stream: Stream
# can trigger GC
  global void force_output (object stream);
  global void force_output(stream)
    var object stream;
    {
      if (builtin_stream_p(stream)) {
        if (TheStream(stream)->strmflags & strmflags_wr_B) { # Output-Stream?
          # nein -> fertig, ja -> nach Streamtyp verzweigen:
          switch (TheStream(stream)->strmtype) {
            case strmtype_synonym:
              force_output_synonym(stream); break;
            case strmtype_broad:
              force_output_broad(stream); break;
            case strmtype_twoway:
            case strmtype_echo:
            #ifdef SOCKET_STREAMS
            case strmtype_twoway_socket:
            #endif
              force_output_twoway(stream); break;
            case strmtype_buff_out:
              force_output_buff_out(stream); break;
            #ifdef GENERIC_STREAMS
            case strmtype_generic:
              force_output_generic(stream); break;
            #endif
            case strmtype_file:
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
              if (ChannelStream_buffered(stream))
                force_output_buffered(stream);
              else
                force_output_unbuffered(stream);
              break;
            case strmtype_terminal:
              force_output_terminal(stream); break;
            #ifdef PRINTER_AMIGAOS
            case strmtype_printer: # Printer:
              # Schließen und neu aufmachen würde vermutlich einen
              # Seitenvorschub ausgeben, und das ist ja wohl nicht erwünscht.
              break; # Daher nichts tun.
            #endif
            default: # nichts tun
              break;
          }
        }
      } else {
        # Call the generic function (STREAM-FORCE-OUTPUT stream):
        pushSTACK(stream); funcall(S(stream_force_output),1);
      }
    }

# UP: Wartenden Output eines Stream stream löschen.
# clear_output(stream);
# > stream: Stream
# can trigger GC
  global void clear_output (object stream);
  global void clear_output(stream)
    var object stream;
    {
      # Unter DOS ist zwar bei keinem File- oder Terminal-Stream etwas zu tun,
      # aber das kann man nicht ausnutzen, denn clear_output auf
      # Buffered-Output-Streams geht immer.
      if (builtin_stream_p(stream)) {
        if (TheStream(stream)->strmflags & strmflags_wr_B) { # Output-Stream?
          # nein -> fertig, ja -> nach Streamtyp verzweigen:
          switch (TheStream(stream)->strmtype) {
            case strmtype_synonym:
              clear_output_synonym(stream); break;
            case strmtype_broad:
              clear_output_broad(stream); break;
            case strmtype_twoway:
            case strmtype_echo:
            #ifdef SOCKET_STREAMS
            case strmtype_twoway_socket:
            #endif
              clear_output_twoway(stream); break;
            case strmtype_buff_out:
              clear_output_buff_out(stream); break;
            #ifdef GENERIC_STREAMS
            case strmtype_generic:
              clear_output_generic(stream); break;
            #endif
            case strmtype_file:
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
              if (ChannelStream_buffered(stream)) {
                # File: nichts tun (würde die File-Verwaltung durcheinanderbringen)
              } else {
                clear_output_unbuffered(stream);
              }
              break;
            case strmtype_terminal:
              #if (defined(UNIX) && !defined(NEXTAPP)) || defined(MSDOS) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32_NATIVE)
              terminalcase(stream,
                           { clear_output_terminal1(stream); },
                           { clear_output_terminal2(stream); },
                           { clear_output_terminal3(stream); }
                          );
              #endif
              break;
            #ifdef PRINTER_AMIGAOS
            case strmtype_printer: # Printer: ungebuffert, also nichts zu tun
              break;
            #endif
            default: # nichts tun
              break;
          }
        }
      } else {
        # Call the generic function (STREAM-CLEAR-OUTPUT stream):
        pushSTACK(stream); funcall(S(stream_clear_output),1);
      }
    }

# UP: Liefert die Line-Position eines Streams.
# get_line_position(stream)
# > stream: Stream
# < ergebnis: Line-Position (Fixnum >=0 or NIL)
# can trigger GC
  global object get_line_position (object stream);
  global object get_line_position(stream)
    var object stream;
    {
      check_SP();
     start:
      if (builtin_stream_p(stream))
        switch (TheStream(stream)->strmtype) {
          case strmtype_synonym:
            # Synonym-Stream: weiterverfolgen
            {
              var object symbol = TheStream(stream)->strm_synonym_symbol;
              stream = get_synonym_stream(symbol);
              /* return get_line_position(stream); */ # entrekursiviert:
              goto start;
            }
          case strmtype_broad:
            # Broadcast-Stream:
            # Maximum der Line-Positions der einzelnen Streams
            {
              pushSTACK(TheStream(stream)->strm_broad_list);
              var uintL maximum = 0; # bisheriges Maximum := 0
              while (consp(STACK_0)) {
                var object next = # Line-Position des nächsten Teilstreams
                  get_line_position(Car(STACK_0));
                if (nullp(next)) {
                  skipSTACK(1); return NIL;
                }
                if (posfixnum_to_L(next) > maximum)
                  maximum = posfixnum_to_L(next); # Maximum nehmen
                STACK_0 = Cdr(STACK_0);
              }
              skipSTACK(1); return fixnum(maximum); # Maximum als Ergebnis
            }
          case strmtype_twoway:
          case strmtype_echo:
          #ifdef SOCKET_STREAMS
          case strmtype_twoway_socket:
          #endif
            # Two-Way-Stream oder Echo-Stream: Output-Stream anschauen
            stream = TheStream(stream)->strm_twoway_output;
            /* return get_line_position(stream); */ # entrekursiviert:
            goto start;
          default: # normaler Stream
            return TheStream(stream)->strm_wr_ch_lpos;
        }
      else {
        # Call the generic function (STREAM-LINE-COLUMN stream):
        pushSTACK(stream); funcall(S(stream_line_column),1);
        if (!(posfixnump(value1) || nullp(value1))) {
          pushSTACK(S(stream_line_column));
          pushSTACK(value1);
          fehler(error,
                 GETTEXT("Return value ~ of call to ~ is not a fixnum >= 0 or NIL.")
                );
        }
        return value1;
      }
    }

# UP: Check an element-type for READ-INTEGER/WRITE-INTEGER.
# check_multiple8_eltype(&eltype);
# > eltype: Element-Type in decoded form
  local void check_multiple8_eltype (const decoded_eltype* eltype);
  local void check_multiple8_eltype(eltype)
    var const decoded_eltype* eltype;
    {
      if (!((eltype->size > 0) && ((eltype->size % 8) == 0))) {
        pushSTACK(canon_eltype(eltype));
        pushSTACK(S(Kelement_type));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~ needs an ~ with a bit size being a multiple of 8, not ~")
              );
      }
    }

# UP: Check an element-type for READ-FLOAT/WRITE-FLOAT.
# check_float_eltype(&eltype)
# > object eltype: argument (in the STACK)
# > subr_self: calling function
# < subr_self: unchanged
# < result: sizeof(ffloatjanus) or sizeof(dfloatjanus)
  local uintL check_float_eltype (object* eltype_);
  local uintL check_float_eltype(eltype_)
    var object* eltype_;
    {
      var object arg = *eltype_;
      if (eq(arg,S(single_float)))
        return sizeof(ffloatjanus);
      if (eq(arg,S(double_float)))
        return sizeof(dfloatjanus);
      var boolean is_ffloat_subtype;
      var boolean is_dfloat_subtype;
      pushSTACK(subr_self); # subr_self retten
      # Erstmal ein wenig kanonischer machen (damit die verschiedenen
      # SUBTYPEP dann nicht zweimal dasselbe machen müssen):
      pushSTACK(arg); funcall(S(canonicalize_type),1); # (SYS::CANONICALIZE-TYPE arg)
      pushSTACK(value1); # canon-arg retten
      pushSTACK(STACK_0); pushSTACK(S(single_float)); funcall(S(subtypep),2); # (SUBTYPEP canon-arg 'SINGLE-FLOAT)
      is_ffloat_subtype = !nullp(value1);
      pushSTACK(S(double_float)); funcall(S(subtypep),2); # (SUBTYPEP canon-arg 'DOUBLE-FLOAT)
      is_dfloat_subtype = !nullp(value1);
      subr_self = popSTACK(); # subr_self zurück
      if (is_ffloat_subtype) {
        if (!is_dfloat_subtype)
          return sizeof(ffloatjanus);
      } else {
        if (is_dfloat_subtype)
          return sizeof(dfloatjanus);
      }
      pushSTACK(*eltype_); pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: illegal :ELEMENT-TYPE argument ~")
            );
    }

# UP: Check an endianness argument.
# test_endianness_arg(arg)
# > arg: the argument
# > subr_self: calling function
# < boolean result: endianness (BIG = TRUE, LITTLE = FALSE)
  local boolean test_endianness_arg (object arg);
  local boolean test_endianness_arg(arg)
    var object arg;
    {
      if (eq(arg,unbound) || eq(arg,S(Klittle)) || eq(arg,S(Kdefault)))
        return FALSE;
      if (eq(arg,S(Kbig)))
        return TRUE;
      pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_endianness)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: illegal endianness argument ~")
            );
    }

LISPFUN(read_byte,1,2,norest,nokey,0,NIL)
# (READ-BYTE stream [eof-error-p [eof-value]]), CLTL S. 382
  {
    # Stream überprüfen:
    var object stream = STACK_2;
    if (!streamp(stream))
      fehler_stream(stream);
    # Integer lesen:
    var object obj = read_byte(stream);
    if (eq(obj,eof_value)) {
      # EOF-Behandlung
      if (!nullp(STACK_1)) { # eof-error-p /= NIL (z.B. = #<UNBOUND>) ?
        # Error melden:
        pushSTACK(STACK_2); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(STACK_(2+1)); # Stream
        pushSTACK(S(read_byte));
        fehler(end_of_file,
               GETTEXT("~: input stream ~ has reached its end")
              );
      } else {
        # EOF verarzten:
        var object eofval = STACK_0;
        if (eq(eofval,unbound))
          eofval = eof_value; # Default ist #<EOF>
        value1 = eofval; mv_count=1; skipSTACK(3); # eofval als Wert
      }
    } else {
      value1 = obj; mv_count=1; skipSTACK(3); # obj als Wert
    }
  }

LISPFUN(read_integer,2,3,norest,nokey,0,NIL)
# (READ-INTEGER stream element-type [endianness [eof-error-p [eof-value]]])
# is a generalized READ-BYTE.
  {
    # Stream überprüfen:
    var object stream = STACK_4;
    if (!streamp(stream))
      fehler_stream(stream);
    # Element-Type überprüfen:
    var decoded_eltype eltype;
    test_eltype_arg(&STACK_3,&eltype);
    check_multiple8_eltype(&eltype);
    # Endianness überprüfen:
    var boolean endianness = test_endianness_arg(STACK_2);
    var uintL bitsize = eltype.size;
    var uintL bytesize = bitsize/8;
    var DYNAMIC_BIT_VECTOR(bitbuffer,bitsize);
    pushSTACK(bitbuffer);
    # Stack layout: stream, element-type, endianness, eof-error-p, eof-value, bitbuffer.
    # Read the data.
    if (!(read_byte_array(&STACK_5,&STACK_0,0,bytesize) == bytesize))
      goto eof;
    bitbuffer = STACK_0;
    if (endianness) {
      # Byte-Swap the data.
      var uintL count = floor(bytesize,2);
      if (count > 0) {
        var uintB* ptr1 = &TheSbvector(bitbuffer)->data[0];
        var uintB* ptr2 = &TheSbvector(bitbuffer)->data[bytesize-1];
        dotimespL(count,count, {
          var uintB x1 = *ptr1;
          var uintB x2 = *ptr2;
          *ptr1 = x2; *ptr2 = x1;
          ptr1++; ptr2--;
        });
      }
    }
    # The data is now in little-endian order. Convert it to an integer.
    {
      var object result;
      switch (eltype.kind) {
        case eltype_iu:
          # cf. rd_by_iu_I
          {
            # Zahl im bitbuffer normalisieren:
            var uintB* bitbufferptr = &TheSbvector(bitbuffer)->data[bytesize-1];
            var uintL count = bytesize;
            while ((!(count==0)) && (*bitbufferptr==0)) { count--; bitbufferptr--; }
            # Zahl bilden:
            if # höchstens oint_data_len Bits ?
               ((count <= floor(oint_data_len,8))
                || ((count == floor(oint_data_len,8)+1)
                    && (*bitbufferptr < bit(oint_data_len%8))
               )   ) {
              # ja -> Fixnum >=0 bilden:
              var uintL wert = 0;
              until (count==0) { wert = (wert<<8) | *bitbufferptr--; count--; }
              result = fixnum(wert);
            } else {
              # nein -> Bignum >0 bilden:
              var uintL digitcount = floor(count,(intDsize/8));
              if (((count%(intDsize/8)) > 0) || (*bitbufferptr & bit(7)))
                digitcount++;
              # Da bitsize < intDsize*uintWC_max, ist
              # digitcount <= ceiling((bitsize+1)/intDsize) <= uintWC_max .
              var object big = allocate_bignum(digitcount,0); # neues Bignum >0
              TheBignum(big)->data[0] = 0; # höchstes Digit auf 0 setzen
              # restliche Digits von rechts füllen, dabei Folge von Bytes in
              # Folge von uintD übersetzen:
              bitbufferptr = &TheSbvector(STACK_0)->data[0];
              #if BIG_ENDIAN_P
              {
                var uintB* bigptr = (uintB*)(&TheBignum(big)->data[digitcount]);
                dotimespL(count,count, { *--bigptr = *bitbufferptr++; } );
              }
              #else
              {
                var uintD* bigptr = &TheBignum(big)->data[digitcount];
                var uintL count2;
                #define GET_NEXT_BYTE(i)  \
                  digit |= ((uintD)(*bitbufferptr++) << (8*i));
                dotimespL(count2,floor(count,intDsize/8), {
                  var uintD digit = 0;
                  DOCONSTTIMES(intDsize/8,GET_NEXT_BYTE); # GET_NEXT_BYTE(0..intDsize/8-1)
                  *--bigptr = digit;
                });
                #undef GET_NEXT_BYTE
                count2 = count % (intDsize/8);
                if (count2>0) {
                  var uintL shiftcount = 0;
                  var uintD digit = (uintD)(*bitbufferptr++);
                  dotimesL(count2,count2-1, {
                    shiftcount += 8;
                    digit |= ((uintD)(*bitbufferptr++) << shiftcount);
                  });
                  *--bigptr = digit;
                }
              }
              #endif
              # Wegen (intDsize/8)*(digitcount-1) <= count <= (intDsize/8)*digitcount
              # ist alles gefüllt.
              result = big;
            }
          }
          break;
        case eltype_is:
          # cf. rd_by_is_I
          {
            # Zahl im bitbuffer normalisieren:
            var uintB* bitbufferptr = &TheSbvector(bitbuffer)->data[bytesize-1];
            var sintD sign;
            var uintL count = bytesize;
            if (!(*bitbufferptr & bit(7))) {
              sign = 0;
              # normalisieren, höchstes Bit muss 0 bleiben:
              while ((count>=2) && (*bitbufferptr==0) && !(*(bitbufferptr-1) & bit(7))) {
                count--; bitbufferptr--;
              }
              # Zahl bilden:
              if # höchstens oint_data_len+1 Bits, Zahl <2^oint_data_len ?
                 ((count <= floor(oint_data_len,8))
                  || ((count == floor(oint_data_len,8)+1)
                      && (*bitbufferptr < bit(oint_data_len%8))
                 )   ) {
                # ja -> Fixnum >=0 bilden:
                var uintL wert = 0;
                until (count==0) { wert = (wert<<8) | *bitbufferptr--; count--; }
                result = posfixnum(wert);
                break;
              }
            } else {
              sign = -1;
              # normalisieren, höchstes Bit muss 1 bleiben:
              while ((count>=2) && (*bitbufferptr==(uintB)(-1)) && (*(bitbufferptr-1) & bit(7))) {
                count--; bitbufferptr--;
              }
              # Zahl bilden:
              if # höchstens oint_data_len+1 Bits, Zahl >=-2^oint_data_len ?
                 ((count <= floor(oint_data_len,8))
                  || ((count == floor(oint_data_len,8)+1)
                      && (*bitbufferptr >= (uintB)(-bit(oint_data_len%8)))
                 )   ) {
                # ja -> Fixnum <0 bilden:
                var uintL wert = (uintL)(-1);
                until (count==0) { wert = (wert<<8) | *bitbufferptr--; count--; }
                result = negfixnum(wbitm(intLsize)+(oint)wert);
                break;
              }
            }
            # Bignum bilden:
            var uintL digitcount = ceiling(count,(intDsize/8));
            # Da bitsize < intDsize*uintWC_max, ist
            # digitcount <= ceiling(bitsize/intDsize) <= uintWC_max .
            var object big = allocate_bignum(digitcount,sign); # neues Bignum
            TheBignum(big)->data[0] = sign; # höchstes Word auf sign setzen
            # restliche Digits von rechts füllen, dabei Folge von Bytes in
            # Folge von uintD übersetzen:
            bitbufferptr = &TheSbvector(STACK_0)->data[0];
            #if BIG_ENDIAN_P
            {
              var uintB* bigptr = (uintB*)(&TheBignum(big)->data[digitcount]);
              dotimespL(count,count, { *--bigptr = *bitbufferptr++; } );
            }
            #else
            {
              var uintD* bigptr = &TheBignum(big)->data[digitcount];
              var uintL count2;
              #define GET_NEXT_BYTE(i)  \
                digit |= ((uintD)(*bitbufferptr++) << (8*i));
              dotimespL(count2,floor(count,intDsize/8), {
                var uintD digit = 0;
                DOCONSTTIMES(intDsize/8,GET_NEXT_BYTE); # GET_NEXT_BYTE(0..intDsize/8-1)
                *--bigptr = digit;
              });
              #undef GET_NEXT_BYTE
              count2 = count % (intDsize/8);
              if (count2>0) {
                var uintL shiftcount = 0;
                var uintD digit = (uintD)(*bitbufferptr++);
                dotimesL(count2,count2-1, {
                  shiftcount += 8;
                  digit |= ((uintD)(*bitbufferptr++) << shiftcount);
                });
                *--bigptr = digit;
              }
            }
            #endif
            # Wegen (intDsize/8)*(digitcount-1) < count <= (intDsize/8)*digitcount
            # ist alles gefüllt.
            result = big;
          }
          break;
        default: NOTREACHED
      }
      FREE_DYNAMIC_BIT_VECTOR(STACK_0);
      value1 = result; mv_count=1;
      skipSTACK(6);
      return;
    }
    # EOF-Behandlung
   eof:
    if (!nullp(STACK_2)) { # eof-error-p /= NIL (z.B. = #<UNBOUND>) ?
      # Error melden:
      pushSTACK(STACK_5); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(STACK_(5+1)); # Stream
      pushSTACK(S(read_integer));
      fehler(end_of_file,
             GETTEXT("~: input stream ~ has reached its end")
            );
    } else {
      # EOF verarzten:
      var object eofval = STACK_1;
      if (eq(eofval,unbound))
        eofval = eof_value; # Default ist #<EOF>
      value1 = eofval; mv_count=1; skipSTACK(6); # eofval als Wert
    }
  }

LISPFUN(read_float,2,3,norest,nokey,0,NIL)
# (READ-FLOAT stream element-type [endianness [eof-error-p [eof-value]]])
# reads a float in IEEE binary representation.
  {
    # Stream überprüfen:
    var object stream = STACK_4;
    if (!streamp(stream))
      fehler_stream(stream);
    # Element-Type überprüfen:
    var uintL bytesize = check_float_eltype(&STACK_3);
    # Endianness überprüfen:
    var boolean endianness = test_endianness_arg(STACK_2);
    var DYNAMIC_BIT_VECTOR(bitbuffer,bytesize*8);
    pushSTACK(bitbuffer);
    # Stack layout: stream, element-type, endianness, eof-error-p, eof-value, bitbuffer.
    # Read the data.
    if (!(read_byte_array(&STACK_5,&STACK_0,0,bytesize) == bytesize))
      goto eof;
    bitbuffer = STACK_0;
    if (BIG_ENDIAN_P ? !endianness : endianness) {
      # Byte-Swap the data.
      var uintL count = floor(bytesize,2);
      if (count > 0) {
        var uintB* ptr1 = &TheSbvector(bitbuffer)->data[0];
        var uintB* ptr2 = &TheSbvector(bitbuffer)->data[bytesize-1];
        dotimespL(count,count, {
          var uintB x1 = *ptr1;
          var uintB x2 = *ptr2;
          *ptr1 = x2; *ptr2 = x1;
           ptr1++; ptr2--;
        });
      }
    }
    # The data is now in machine-dependent order. Convert it to a float.
    switch (bytesize) {
      case sizeof(ffloatjanus):
        if (((varobject_alignment % alignof(ffloatjanus)) == 0)
            && ((offsetofa(sbvector_,data) % alignof(ffloatjanus)) == 0)) {
          value1 = c_float_to_FF((ffloatjanus*)&TheSbvector(bitbuffer)->data[0]);
        } else {
          var ffloatjanus tmp;
          memcpy(&tmp,&TheSbvector(bitbuffer)->data[0],sizeof(ffloatjanus));
          value1 = c_float_to_FF(&tmp);
        }
        break;
      case sizeof(dfloatjanus):
        if (((varobject_alignment % alignof(dfloatjanus)) == 0)
            && ((offsetofa(sbvector_,data) % alignof(dfloatjanus)) == 0)) {
          value1 = c_double_to_DF((dfloatjanus*)&TheSbvector(bitbuffer)->data[0]);
        } else {
          var dfloatjanus tmp;
          memcpy(&tmp,&TheSbvector(bitbuffer)->data[0],sizeof(dfloatjanus));
          value1 = c_double_to_DF(&tmp);
        }
        break;
      default: NOTREACHED
    }
    FREE_DYNAMIC_BIT_VECTOR(STACK_0);
    mv_count=1;
    skipSTACK(6);
    return;
    # EOF-Behandlung
   eof:
    if (!nullp(STACK_2)) { # eof-error-p /= NIL (z.B. = #<UNBOUND>) ?
      # Error melden:
      pushSTACK(STACK_5); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(STACK_(5+1)); # Stream
      pushSTACK(S(read_float));
      fehler(end_of_file,
             GETTEXT("~: input stream ~ has reached its end")
            );
    } else {
      # EOF verarzten:
      var object eofval = STACK_1;
      if (eq(eofval,unbound))
        eofval = eof_value; # Default ist #<EOF>
      value1 = eofval; mv_count=1; skipSTACK(6); # eofval als Wert
    }
  }

LISPFUNN(write_byte,2)
# (WRITE-BYTE integer stream), CLTL S. 385
  {
    # Stream überprüfen:
    var object stream = STACK_0;
    if (!streamp(stream))
      fehler_stream(stream);
    # Integer überprüfen:
    var object obj = STACK_1;
    if (!integerp(obj))
      fehler_wr_integer(stream,obj);
    # Integer schreiben:
    write_byte(stream,obj);
    value1 = STACK_1; mv_count=1; skipSTACK(2); # obj als Wert
  }

LISPFUN(write_integer,3,1,norest,nokey,0,NIL)
# (WRITE-INTEGER integer stream element-type [endianness])
# is a generalized WRITE-BYTE.
  {
    # Stream überprüfen:
    var object stream = STACK_2;
    if (!streamp(stream))
      fehler_stream(stream);
    # Element-Type überprüfen:
    var decoded_eltype eltype;
    test_eltype_arg(&STACK_1,&eltype);
    check_multiple8_eltype(&eltype);
    # Endianness überprüfen:
    var boolean endianness = test_endianness_arg(STACK_0);
    # Integer überprüfen:
    var object obj = STACK_3;
    if (!integerp(obj))
      fehler_wr_integer(STACK_2,obj);
    var uintL bitsize = eltype.size;
    var uintL bytesize = bitsize/8;
    var DYNAMIC_BIT_VECTOR(bitbuffer,bitsize);
    pushSTACK(bitbuffer);
    # Stack layout: obj, stream, element-type, endianness, bitbuffer.
    obj = STACK_4;
    # Copy the integer's data into the buffer.
    switch (eltype.kind) {
      case eltype_iu:
        # cf. wr_by_ixu_sub
        {
          # obj überprüfen:
          if (!integerp(obj))
            fehler_wr_integer(STACK_3,obj);
          if (!positivep(obj))
            fehler_bad_integer(STACK_3,obj);
          # obj ist jetzt ein Integer >=0
          # obj in den Bitbuffer übertragen:
          var uintB* bitbufferptr = &TheSbvector(bitbuffer)->data[0];
          var uintL count = bytesize;
          if (posfixnump(obj)) {
            # obj ist ein Fixnum >=0
            var uintL wert = posfixnum_to_L(obj);
            # wert < 2^bitsize überprüfen:
            if (!((bitsize>=oint_data_len) || (wert < bit(bitsize))))
              fehler_bad_integer(STACK_3,obj);
            # wert im Bitbuffer ablegen:
            until (wert==0) {
              *bitbufferptr++ = (uint8)wert; wert = wert>>8; count--;
            }
          } else {
            # obj ist ein Bignum >0
            var uintL len = (uintL)Bignum_length(obj);
            # obj < 2^bitsize überprüfen:
            if (!((floor(bitsize,intDsize) >= len)
                  || ((floor(bitsize,intDsize) == len-1)
                      && (TheBignum(obj)->data[0] < bit(bitsize%intDsize))
               ) )   )
              fehler_bad_integer(STACK_3,obj);
            #if BIG_ENDIAN_P
            {
              var uintB* ptr = (uintB*)&TheBignum(obj)->data[len];
              # Digit-Länge in Byte-Länge umrechnen:
              len = (intDsize/8)*len;
              #define CHECK_NEXT_BYTE(i)  \
                if (!( ((uintB*)(&TheBignum(obj)->data[0]))[i] ==0)) goto u_len_ok; \
                len--;
              DOCONSTTIMES(intDsize/8,CHECK_NEXT_BYTE); # CHECK_NEXT_BYTE(0..intDsize/8-1)
              #undef CHECK_NEXT_BYTE
              u_len_ok:
              # obj im Bitbuffer ablegen:
              count = count - len;
              dotimespL(len,len, { *bitbufferptr++ = *--ptr; } );
            }
            #else
            {
              var uintD* ptr = &TheBignum(obj)->data[len];
              len--;
              count -= (intDsize/8)*len;
              dotimesL(len,len, {
                var uintD digit = *--ptr;
                doconsttimes(intDsize/8, {
                  *bitbufferptr++ = (uintB)digit; digit = digit >> 8;
                });
              });
              var uintD digit = *--ptr;
              doconsttimes(intDsize/8, {
                if (digit==0)
                  goto u_ok;
                *bitbufferptr++ = (uintB)digit; digit = digit >> 8;
                count--;
              });
              u_ok: ;
            }
            #endif
          }
          dotimesL(count,count, { *bitbufferptr++ = 0; } );
        }
        break;
      case eltype_is:
        # cf. wr_by_ixs_sub
        {
          # obj überprüfen:
          if (!integerp(obj))
            fehler_wr_integer(STACK_3,obj);
          # obj ist jetzt ein Integer
          # obj in den Bitbuffer übertragen:
          var uintB* bitbufferptr = &TheSbvector(bitbuffer)->data[0];
          var uintL count = bytesize;
          var uintL sign = (sintL)R_sign(obj);
          if (fixnump(obj)) {
            # obj ist ein Fixnum
            var uintL wert = fixnum_to_L(obj); # >=0 oder <0, je nach sign
            # 0 <= wert < 2^(bitsize-1) bzw. -2^(bitsize-1) <= wert < 0 überprüfen:
            wert = wert^sign;
            if (!((bitsize>oint_data_len) || (wert < bit(bitsize-1))))
              fehler_bad_integer(STACK_3,obj);
            # wert^sign im Bitbuffer ablegen:
            until (wert == 0) {
              *bitbufferptr++ = (uint8)(wert^sign); wert = wert>>8; count--;
            }
            dotimesL(count,count, { *bitbufferptr++ = (uint8)sign; } );
          } else {
            # obj ist ein Bignum
            var uintL len = (uintL)Bignum_length(obj);
            # -2^(bitsize-1) <= obj < 2^(bitsize-1) überprüfen:
            if (!((floor(bitsize,intDsize) >= len)
                  || ((bitsize > intDsize*(len-1))
                      && ((TheBignum(obj)->data[0] ^ (uintD)sign) < bit((bitsize%intDsize)-1))
               ) )   )
              fehler_bad_integer(STACK_3,obj);
            #if BIG_ENDIAN_P
            {
              var uintB* ptr = (uintB*)&TheBignum(obj)->data[len];
              # Digit-Länge in Byte-Länge umrechnen:
              len = (intDsize/8)*len;
              #define CHECK_NEXT_BYTE(i)  \
                if (!( ((uintB*)(&TheBignum(obj)->data[0]))[i] == (uintB)sign)) goto s_len_ok; \
                len--;
              DOCONSTTIMES(intDsize/8,CHECK_NEXT_BYTE); # CHECK_NEXT_BYTE(0..intDsize/8-1)
              #undef CHECK_NEXT_BYTE
              s_len_ok:
              # obj im Bitbuffer ablegen:
              count = count - len;
              dotimespL(len,len, { *bitbufferptr++ = *--ptr; } );
            }
            #else
            {
              var uintD* ptr = &TheBignum(obj)->data[len];
              len--;
              count -= (intDsize/8)*len;
              dotimesL(len,len, {
                var uintD digit = *--ptr;
                doconsttimes(intDsize/8, {
                  *bitbufferptr++ = (uintB)digit; digit = digit >> 8;
                });
              });
              var sintD digit = *--ptr;
              doconsttimes(intDsize/8, {
                if (digit == (sintD)sign)
                  goto s_ok;
                *bitbufferptr++ = (uintB)digit; digit = digit >> 8;
                count--;
              });
              s_ok: ;
            }
            #endif
            dotimesL(count,count, { *bitbufferptr++ = (uintB)sign; } );
          }
        }
        break;
      default: NOTREACHED
    }
    # The data is now in little-endian order.
    if (endianness) {
      # Byte-Swap the data.
      var uintL count = floor(bytesize,2);
      if (count > 0) {
        var uintB* ptr1 = &TheSbvector(bitbuffer)->data[0];
        var uintB* ptr2 = &TheSbvector(bitbuffer)->data[bytesize-1];
        dotimespL(count,count, {
          var uintB x1 = *ptr1;
          var uintB x2 = *ptr2;
          *ptr1 = x2; *ptr2 = x1;
          ptr1++; ptr2--;
        });
      }
    }
    # Write the data.
    write_byte_array(&STACK_3,&STACK_0,0,bytesize);
    FREE_DYNAMIC_BIT_VECTOR(STACK_0);
    value1 = STACK_4; mv_count=1; # obj als Wert
    skipSTACK(5);
  }

LISPFUN(write_float,3,1,norest,nokey,0,NIL)
# (WRITE-FLOAT float stream element-type [endianness])
# writes a float in IEEE binary representation.
  {
    # Stream überprüfen:
    var object stream = STACK_2;
    if (!streamp(stream))
      fehler_stream(stream);
    # Element-Type überprüfen:
    var uintL bytesize = check_float_eltype(&STACK_1);
    # Endianness überprüfen:
    var boolean endianness = test_endianness_arg(STACK_0);
    # Float überprüfen:
    var object obj = STACK_3;
    switch (bytesize) {
      case sizeof(ffloatjanus):
        if (!single_float_p(obj)) {
          pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(S(single_float)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(STACK_(2+2));
          pushSTACK(S(single_float));
          pushSTACK(obj);
          fehler(type_error,
                 GETTEXT("~ is not a ~, cannot be output onto ~")
                );
        }
        break;
      case sizeof(dfloatjanus):
        if (!double_float_p(obj)) {
          pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(S(double_float)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(STACK_(2+2));
          pushSTACK(S(double_float));
          pushSTACK(obj);
          fehler(type_error,
                 GETTEXT("~ is not a ~, cannot be output onto ~")
                );
        }
        break;
      default: NOTREACHED
    }
    var DYNAMIC_BIT_VECTOR(bitbuffer,bytesize*8);
    pushSTACK(bitbuffer);
    # Stack layout: obj, stream, element-type, endianness, bitbuffer.
    obj = STACK_4;
    # Copy the float's data into the buffer.
    switch (bytesize) {
      case sizeof(ffloatjanus):
        if (((varobject_alignment % alignof(ffloatjanus)) == 0)
            && ((offsetofa(sbvector_,data) % alignof(ffloatjanus)) == 0)) {
          FF_to_c_float(obj,(ffloatjanus*)&TheSbvector(bitbuffer)->data[0]);
        } else {
          var ffloatjanus tmp;
          FF_to_c_float(obj,&tmp);
          memcpy(&TheSbvector(bitbuffer)->data[0],&tmp,sizeof(ffloatjanus));
        }
        break;
      case sizeof(dfloatjanus):
        if (((varobject_alignment % alignof(dfloatjanus)) == 0)
            && ((offsetofa(sbvector_,data) % alignof(dfloatjanus)) == 0)) {
          DF_to_c_double(obj,(dfloatjanus*)&TheSbvector(bitbuffer)->data[0]);
        } else {
          var dfloatjanus tmp;
          DF_to_c_double(obj,&tmp);
          memcpy(&TheSbvector(bitbuffer)->data[0],&tmp,sizeof(dfloatjanus));
        }
        break;
      default: NOTREACHED
    }
    # The data is now in machine-dependent order.
    if (BIG_ENDIAN_P ? !endianness : endianness) {
      # Byte-Swap the data.
      var uintL count = floor(bytesize,2);
      if (count > 0) {
        var uintB* ptr1 = &TheSbvector(bitbuffer)->data[0];
        var uintB* ptr2 = &TheSbvector(bitbuffer)->data[bytesize-1];
        dotimespL(count,count, {
          var uintB x1 = *ptr1;
          var uintB x2 = *ptr2;
          *ptr1 = x2; *ptr2 = x1;
          ptr1++; ptr2--;
        });
      }
    }
    # Write the data.
    write_byte_array(&STACK_3,&STACK_0,0,bytesize);
    FREE_DYNAMIC_BIT_VECTOR(STACK_0);
    value1 = STACK_4; mv_count=1; # obj als Wert
    skipSTACK(5);
  }

# UP: Überprüft, ob ein Argument ein offener File-Stream ist.
# check_open_file_stream(obj);
# > obj: Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: offener File-Stream
  local object check_open_file_stream (object obj);
  local object check_open_file_stream(obj)
    var object obj;
    {
      loop {
        if (!builtin_stream_p(obj)) # Stream ?
          goto fehler_bad_obj;
        if (TheStream(obj)->strmtype == strmtype_synonym) { # Synonym-Stream verfolgen
          var object sym = TheStream(obj)->strm_synonym_symbol;
          obj = Symbol_value(sym);
        } else
          break;
      }
      if (!(TheStream(obj)->strmtype == strmtype_file)) # Streamtyp File-Stream ?
        goto fehler_bad_obj;
      if ((TheStream(obj)->strmflags & strmflags_open_B) == 0) # Stream offen ?
        goto fehler_bad_obj;
      if (nullp(TheStream(obj)->strm_buffered_channel)) # und Handle /= NIL ?
        goto fehler_bad_obj;
      return obj; # ja -> OK
     fehler_bad_obj:
      pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_open_file_stream)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ is not an open file stream")
            );
    }

LISPFUN(file_position,1,1,norest,nokey,0,NIL)
# (FILE-POSITION file-stream [position]), CLTL S. 425
  {
    var object position = popSTACK();
    var object stream = popSTACK();
    stream = check_open_file_stream(stream); # stream überprüfen
    if (!ChannelStream_buffered(stream)) {
      # Don't know how to deal with the file position on unbuffered streams.
      value1 = NIL; mv_count=1;
    } else {
      if (eq(position,unbound)) {
        # position nicht angegeben -> Position als Wert:
        value1 = UL_to_I(BufferedStream_position(stream)); mv_count=1;
      } else {
        if (eq(position,S(Kstart))) {
          # :START -> an den Anfang positionieren:
          logical_position_file_start(stream);
        } elif (eq(position,S(Kend))) {
          # :END -> ans Ende positionieren:
          logical_position_file_end(stream);
        } elif (uint32_p(position)) {
          # an die angegebene Position positionieren:
          logical_position_file(stream,I_to_UL(position));
        } else {
          # Unzulässiges Position-Argument
          pushSTACK(position); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_position)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(position); pushSTACK(S(Kend)); pushSTACK(S(Kstart));
          pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,
                 GETTEXT("~: position argument should be ~ or ~ or a nonnegative integer, not ~")
                );
        }
        value1 = T; mv_count=1; # Wert T
      }
    }
  }

LISPFUNN(file_length,1)
# (FILE-LENGTH file-stream), CLTL S. 425
  {
    var object stream = popSTACK();
    stream = check_open_file_stream(stream); # stream überprüfen
    if (!ChannelStream_buffered(stream)) {
      # Don't know how to deal with the file position on unbuffered streams.
      value1 = NIL; mv_count=1;
    } else {
      # Position merken:
      var uintL position = BufferedStream_position(stream);
      # ans Ende positionieren:
      logical_position_file_end(stream);
      # Ende-Position merken:
      var uintL endposition = BufferedStream_position(stream);
      # an die alte Position zurückpositionieren:
      logical_position_file(stream,position);
      value1 = UL_to_I(endposition); mv_count=1; # Ende-Position als Wert
    }
  }

LISPFUNN(file_string_length,2)
# (FILE-STRING-LENGTH stream object)
  {
    var object stream = check_open_file_stream(STACK_1); # stream überprüfen
    var object obj = STACK_0;
    skipSTACK(2);
    if (!(TheStream(stream)->strmflags & strmflags_wr_ch_B))
      fehler_illegal_streamop(S(file_string_length),stream);
    var object encoding = TheStream(stream)->strm_encoding;
    #if defined(UNICODE) && (defined(GNU_LIBICONV) || defined(HAVE_ICONV))
    if (simple_string_p(TheEncoding(encoding)->enc_charset)) {
      # iconv-based encodings have state. Since we cannot duplicate an iconv_t
      # we have no way to know for sure how many bytes the string will span.
      if (stringp(obj)) {
        value1 = (vector_length(obj) == 0 ? Fixnum_0 : NIL); mv_count=1;
      } elif (charp(obj)) {
        value1 = NIL; mv_count=1;
      } else {
        fehler_wr_char(stream,obj);
      }
      return;
    }
    #endif
    if (TheEncoding(encoding)->min_bytes_per_char == TheEncoding(encoding)->max_bytes_per_char) {
      # Easy case: a fixed number of bytes per character.
      var uintL bytes_per_char = TheEncoding(encoding)->min_bytes_per_char;
      if (eq(TheEncoding(encoding)->enc_eol,S(Kunix))
          || eq(TheEncoding(encoding)->enc_eol,S(Kmac))
         ) {
        if (stringp(obj)) {
          var uintL result = vector_length(obj);
          value1 = UL_to_I(result*bytes_per_char); mv_count=1; return;
        } elif (charp(obj)) {
          value1 = fixnum(bytes_per_char); mv_count=1; return;
        } else {
          fehler_wr_char(stream,obj);
        }
      }
      if (eq(TheEncoding(encoding)->enc_eol,S(Kdos))) {
        # Take into account the NL -> CR/LF translation.
        if (stringp(obj)) {
          var uintL len;
          var uintL offset;
          var object string = unpack_string_ro(obj,&len,&offset);
          var uintL result = len;
          if (len > 0) {
            SstringDispatch(string,
              {
                var const chart* charptr = &TheSstring(string)->data[offset];
                var uintL count;
                dotimespL(count,len, {
                  if (chareq(*charptr++,ascii(NL)))
                    result++;
                });
              },
              {
                var const scint* charptr = &TheSmallSstring(string)->data[offset];
                var uintL count;
                dotimespL(count,len, {
                  if (chareq(as_chart(*charptr++),ascii(NL)))
                    result++;
                });
              }
              );
          }
          value1 = UL_to_I(result*bytes_per_char); mv_count=1; return;
        } elif (charp(obj)) {
          var uintL result = 1;
          if (chareq(char_code(obj),ascii(NL)))
            result++;
          value1 = fixnum(result*bytes_per_char); mv_count=1; return;
        } else {
          fehler_wr_char(stream,obj);
        }
      }
      NOTREACHED
    } else {
      # Have to look at each character individually.
      var const chart* charptr;
      var uintL len;
      var chart auxch;
      if (stringp(obj)) {
        var uintL offset;
        var object string = unpack_string_ro(obj,&len,&offset);
        unpack_sstring_alloca(string,len,offset, charptr=);
      } elif (charp(obj)) {
        auxch = char_code(obj); charptr = &auxch; len = 1;
      } else {
        fehler_wr_char(stream,obj);
      }
      if (eq(TheEncoding(encoding)->enc_eol,S(Kunix))) {
        # Treat all the characters all at once.
        var uintL result = cslen(encoding,charptr,len);
        value1 = UL_to_I(result); mv_count=1; return;
      } else {
        # Treat line-by-line.
        var const chart* eol_charptr;
        var uintL eol_len;
        if (eq(TheEncoding(encoding)->enc_eol,S(Kmac))) {
          static const chart eol_mac[1] = { ascii(CR) };
          eol_charptr = &eol_mac[0]; eol_len = 1;
        } elif (eq(TheEncoding(encoding)->enc_eol,S(Kdos))) {
          static const chart eol_dos[2] = { ascii(CR), ascii(LF) };
          eol_charptr = &eol_dos[0]; eol_len = 2;
        } else {
          NOTREACHED
        }
        var const chart* endptr = charptr+len;
        var uintL result = 0;
        while (charptr < endptr) {
          # Search the next NL.
          var const chart* ptr = charptr;
          while (!chareq(*ptr,ascii(NL))) {
            ptr++;
            if (ptr == endptr)
              break;
          }
          # Count the bytes needed for the characters before the NL.
          if (!(ptr == charptr))
            result += cslen(encoding,charptr,ptr-charptr);
          charptr = ptr;
          # Count the bytes needed for the NL.
          if (charptr < endptr) {
            # *charptr is ascii(NL).
            result += cslen(encoding,eol_charptr,eol_len);
            charptr++;
          }
        }
        value1 = UL_to_I(result); mv_count=1; return;
      }
    }
  }

# UP: Tells whether a stream is buffered.
# stream_isbuffered(stream)
# > stream: a channel or socket stream
# < result: TRUE if stream is buffered, else FALSE
  global boolean stream_isbuffered (object stream);
  global boolean stream_isbuffered(stream)
    var object stream;
    {
      switch (TheStream(stream)->strmtype) {
        case strmtype_file:
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
          return ChannelStream_buffered(stream);
        #ifdef SOCKET_STREAMS
        case strmtype_twoway_socket:
          return TRUE;
        #endif
        default:
          return FALSE;
      }
    }

# UP: Returns the current line number of a stream.
# stream_line_number(stream)
# > stream: a stream
# < result: an integer or NIL
# can trigger GC
  global object stream_line_number (object stream);
  global object stream_line_number(stream)
    var object stream;
    {
      return (builtin_stream_p(stream)
              && TheStream(stream)->strmtype == strmtype_file
              && eq(TheStream(stream)->strm_eltype,S(character))
              ? UL_to_I(ChannelStream_lineno(stream)) # aktuelle Zeilennummer
              : NIL                                   # NIL falls unbekannt
             );
    }

LISPFUNN(line_number,1)
# (SYS::LINE-NUMBER stream) liefert die aktuelle Zeilennummer (falls stream
# ein Character-File-Input-Stream ist, von dem nur gelesen wurde).
  {
    var object stream = popSTACK();
    if (!streamp(stream)) # stream überprüfen
      fehler_stream(stream);
    value1 = stream_line_number(stream); mv_count=1;
  }

# Function: Returns TRUE if a stream allows read-eval.
# stream_get_read_eval(stream)
# > stream: a stream
# < result: TRUE if read-eval is allowed from the stream, else FALSE
  global boolean stream_get_read_eval (object stream);
  global boolean stream_get_read_eval(stream)
    var object stream;
    {
      if (builtin_stream_p(stream)) {
        return ((TheStream(stream)->strmflags & strmflags_reval_B) != 0);
      } else {
        # (SLOT-VALUE stream '$reval):
        var object clas = TheInstance(stream)->inst_class;
        var object slotinfo = gethash(S(reval),TheClass(clas)->slot_location_table);
        var object value = TheSrecord(stream)->recdata[posfixnum_to_L(slotinfo)];
        return !nullp(value);
      }
    }

# Function: Changes the read-eval state of a stream.
# stream_set_read_eval(stream,value);
# > stream: a stream
# > value: TRUE if read-eval shall be allowed from the stream, else FALSE
  global void stream_set_read_eval (object stream, boolean value);
  global void stream_set_read_eval(stream,value)
    var object stream;
    var boolean value;
    {
      if (builtin_stream_p(stream)) {
        if (value)
          TheStream(stream)->strmflags |= strmflags_reval_B;
        else
          TheStream(stream)->strmflags &= ~strmflags_reval_B;
      } else {
        # (SETF (SLOT-VALUE stream '$reval) value):
        var object clas = TheInstance(stream)->inst_class;
        var object slotinfo = gethash(S(reval),TheClass(clas)->slot_location_table);
        TheSrecord(stream)->recdata[posfixnum_to_L(slotinfo)] = (value ? T : NIL);
      }
    }

LISPFUN(allow_read_eval,1,1,norest,nokey,0,NIL)
# (SYS::ALLOW-READ-EVAL stream) returns the stream's READ-EVAL flag.
# (SYS::ALLOW-READ-EVAL stream flag) sets the stream's READ-EVAL flag.
# T means #. is allowed regardless of the value of *READ-EVAL*, NIL
# (the default) means that *READ-EVAL* is respected.
  {
    var object flag = popSTACK();
    var object stream = popSTACK();
    if (!streamp(stream)) # stream überprüfen
      fehler_stream(stream);
    if (eq(flag,unbound)) {
      value1 = (stream_get_read_eval(stream) ? T : NIL);
    } else {
      if (nullp(flag)) {
        stream_set_read_eval(stream,FALSE); value1 = NIL;
      } else {
        stream_set_read_eval(stream,TRUE); value1 = T;
      }
    }
    mv_count=1;
  }

LISPFUNN(defgray,1)
# (SYS::%DEFGRAY fundamental-stream-classes)
# Initializes O(class_fundamental*_stream).
  {
    var const object* ptr1 = &TheSvector(STACK_0)->data[0];
    var object* ptr2 = &O(class_fundamental_stream);
    var uintC count;
    dotimesC(count,Svector_length(STACK_0), { *ptr2++ = *ptr1++; });
    value1 = NIL; mv_count=0; skipSTACK(1);
  }

# =============================================================================

#ifdef EXPORT_SYSCALLS
#ifdef UNIX

global object stream_fd (object stream);
global object stream_fd (stream)
  var object stream;
{
  stream = check_open_file_stream(stream);
  return UL_to_I(TheHandle(TheStream(stream)->strm_ochannel));
}

#endif # UNIX
#endif # EXPORT_SYSCALLS

# =============================================================================

# Binärkompatibilität zwischen .mem-Files mit und ohne NEXTAPP erreichen:
  #ifdef MAYBE_NEXTAPP
    #ifndef NEXTAPP
      #define wr_ch_terminal  wr_ch_error
      #define rd_ch_terminal  rd_ch_error
    #else
      #define wr_ch_terminal1  wr_ch_error
      #define rd_ch_terminal1  rd_ch_error
      #define wr_ch_array_terminal1  wr_ch_array_dummy
    #endif
    #ifndef GNU_READLINE
      #define wr_ch_terminal3  wr_ch_error
      #define rd_ch_terminal3  rd_ch_error
      #define wr_ch_array_terminal3  wr_ch_array_dummy
    #endif
    #ifdef NEXTAPP
      #define wr_ch_window  wr_ch_error
    #endif
  #endif

# Tabelle aller Pseudofunktionen
  #define PSEUDO  PSEUDO_C
  #include "pseudofun.c"
  #undef PSEUDO
  global struct pseudocode_tab_ pseudocode_tab =
    {
      #define PSEUDO  PSEUDO_D
      #include "pseudofun.c"
      #undef PSEUDO
    };
  global struct pseudodata_tab_ pseudodata_tab =
    {
      #define PSEUDO  PSEUDO_E
      #include "pseudofun.c"
      #undef PSEUDO
    };

# =============================================================================

#ifdef EMUNIX_PORTABEL

# Eine Hilfsfunktion für bidirektionale Pipes: popenrw()
#undef stdin_handle
#undef stdout_handle
#include "../os2/popenrw.c"

#endif

# =============================================================================

# filestatus/if_file_exists, file_datetime durch break_sem_4 schützen??
# Signalbehandlung bei EXECUTE, SHELL, MAKE-PIPE-INPUT-STREAM, MAKE-PIPE-OUTPUT-STREAM, MAKE-PIPE-IO-STREAM ??
# naming of file/handle/buffered/b_file/unbuffered stuff
# do not access strm_file_truename on pipe and socket streams
# implement FILE-POSITION for unbuffered file-streams (regular handle, direction != 5)
# LISTEN on unbuffered (non-regular) file and socket streams can cause the process to block
