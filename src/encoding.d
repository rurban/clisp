# Encodings (character sets and conversions) for CLISP
# Bruno Haible 8.11.1998

#include "lispbibl.c"

# =============================================================================
#                              General functions

LISPFUN(make_encoding,0,0,norest,key,2,
        (kw(charset),kw(line_terminator)) )
# (MAKE-ENCODING [:charset] [:line-terminator])
# creates a new encoding.
  { var object arg;
    # Check the :CHARSET argument.
    arg = STACK_1;
    if (eq(arg,unbound) || eq(arg,S(Kdefault)))
      { arg = O(default_file_encoding); }
    elif (encodingp(arg))
      { }
    else
      { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(encoding)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(arg); pushSTACK(S(make_encoding));
        fehler(type_error,
               DEUTSCH ? "~: Als :CHARSET-Argument ist ~ unzulässig." :
               ENGLISH ? "~: illegal :CHARSET argument ~" :
               FRANCAIS ? "~ : ~ n'est pas permis comme argument pour :CHARSET." :
               ""
              );
      }
    STACK_1 = arg;
    # Check the :LINE-TERMINATOR argument.
    arg = STACK_0;
    if (!(eq(arg,unbound)
          || eq(arg,S(Kunix)) || eq(arg,S(Kmac)) || eq(arg,S(Kdos))
       ) )
      { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_line_terminator)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(arg); pushSTACK(S(make_encoding));
        fehler(type_error,
               DEUTSCH ? "~: Als :LINE-TERMINATOR-Argument ist ~ unzulässig." :
               ENGLISH ? "~: illegal :LINE-TERMINATOR argument ~" :
               FRANCAIS ? "~ : ~ n'est pas permis comme argument pour :LINE-TERMINATOR." :
               ""
              );
      }
    # Create a new encoding.
    if (eq(arg,unbound) || eq(arg,TheEncoding(STACK_1)->enc_eol))
      { value1 = STACK_1; }
      else
      { var object encoding = allocate_encoding();
        var object old_encoding = STACK_1;
        { var const object* ptr1 = &TheRecord(old_encoding)->recdata[0];
          var object* ptr2 = &TheRecord(encoding)->recdata[0];
          var uintC count;
          dotimesC(count,encoding_length, { *ptr2++ = *ptr1++; } );
        }
        TheEncoding(encoding)->enc_eol = STACK_0;
        value1 = encoding;
      }
    mv_count=1;
    skipSTACK(2);
  }

# Initialize the encodings.
# init_encodings();
  global void init_encodings (void);
  global void init_encodings()
    { # Initialize O(default_encoding):
      pushSTACK(unbound);
      #if defined(MSDOS) || defined(WIN32) || (defined(UNIX) && (O_BINARY != 0))
      pushSTACK(S(Kdos));
      #else
      pushSTACK(S(Kunix));
      #endif
      C_make_encoding();
      O(default_file_encoding) = value1;
    }

