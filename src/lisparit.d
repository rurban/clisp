# Arithmetik für CLISP
# Bruno Haible 1990-1999

#include "lispbibl.c"

#define LISPARIT      # im folgenden nicht nur die Macros, auch die Funktionen

#undef LF             # LF bedeutet hier nicht 'Linefeed', sondern 'LongFloat'


# UP: entscheidet auf Zahlgleichheit
# number_gleich(x,y)
# > x,y: zwei Zahlen
# < ergebnis: TRUE, falls (= x y) gilt
  global boolean number_gleich (object x, object y);
  #define N_N_gleich  number_gleich  # N_N_gleich wird später definiert


# zur Arithmetik allgemein:
#include "aridecl.c"  # Deklarationen
#include "arilev0.c"  # Maschinen-Arithmetik
#include "arilev1.c"  # Digit-Sequences
# zu Integers:
#include "intelem.c"  # Elementaroperationen auf Integers
#include "intlog.c"   # logische Operationen auf Integers
#include "intplus.c"  # Addition, Subtraktion auf Integers
#include "intcomp.c"  # Vergleichsoperationen auf Integers
#include "intbyte.c"  # Byte-Operationen LDB, LOAD-BYTE, ...
#include "intmal.c"   # Multiplikation von Integers
#include "intdiv.c"   # Division von Integers
#include "intgcd.c"   # ggT und kgV
#include "int2adic.c" # Operationen mit 2-adischen Integers
#include "intsqrt.c"  # Wurzel, ISQRT
#include "intprint.c" # Hilfsfunktion zur Ausgabe von Integers
#include "intread.c"  # Hilfsfunktion zur Eingabe von Integers
# zu rationalen Zahlen:
#include "rational.c" # Rationale Zahlen
# zu Floats:
#include "sfloat.c"   # Short-Float-Grundfunktionen
#include "ffloat.c"   # Single-Float-Grundfunktionen
#include "dfloat.c"   # Double-Float-Grundfunktionen
#include "lfloat.c"   # Long-Float-Grundfunktionen
#include "flo_konv.c" # Float-Konversionen
#include "flo_rest.c" # Floats allgemein
# zu reellen Zahlen:
#include "realelem.c" # elementare Funktionen für reelle Zahlen
#include "realrand.c" # Funktionen für Zufallszahlen
#include "realtran.c" # transzendente Funktionen für reelle Zahlen
# zu komplexen Zahlen:
#include "compelem.c" # elementare Funktionen für komplexe Zahlen
#include "comptran.c" # transzendente Funktionen für komplexe Zahlen


# =========================================================================== #
#                       Einleseroutinen für Zahlen

# UP: Multipliziert ein Integer mit 10 und addiert eine weitere Ziffer.
# mal_10_plus_x(y,x)
# > y: Integer Y (>=0)
# > x: Ziffernwert X (>=0,<10)
# < ergebnis: Integer Y*10+X (>=0)
# can trigger GC
  global object mal_10_plus_x (object y, uintB x);
  global object mal_10_plus_x(y,x)
    var object y;
    var uintB x;
    { SAVE_NUM_STACK # num_stack retten
      var uintD* MSDptr;
      var uintC len;
      var uintD* LSDptr;
      I_to_NDS_1(y, MSDptr=,len=,LSDptr=); # NDS zu Y
      begin_arith_call();
     {var uintD carry = mulusmall_loop_down(10,LSDptr,len,x); # mal 10, plus x
      end_arith_call();
      if (!(carry==0))
        { *--MSDptr = carry; len++;
          if (uintWCoverflow(len)) { RESTORE_NUM_STACK; BN_ueberlauf(); } # Überlauf der Länge?
        }
      {var object result = UDS_to_I(MSDptr,len); # UDS als Integer
       RESTORE_NUM_STACK # num_stack zurück
       return result;
    }}}

# UP: Wandelt eine Zeichenkette mit Integer-Syntax in ein Integer um.
# Punkte werden überlesen.
# read_integer(base,sign,string,index1,index2)
# > base: Lesebasis (>=2, <=36)
# > sign: Vorzeichen (/=0 falls negativ)
# > string: Simple-String (enthält Ziffern mit Wert <base und evtl. Punkt)
# > index1: Index der ersten Ziffer
# > index2: Index nach der letzten Ziffer
#   (also index2-index1 Ziffern, incl. evtl. Dezimalpunkt am Schluss)
# < ergebnis: Integer
# can trigger GC
  global object read_integer (uintWL base,
         signean sign, object string, uintL index1, uintL index2);
  global object read_integer(base,sign,string,index1,index2)
    var uintWL base;
    var signean sign;
    var object string;
    var uintL index1;
    var uintL index2;
    { var const chart* charptr;
      unpack_sstring_alloca(string,index2-index1,index1, charptr=);
     {var object x = # in Integer umwandeln:
        DIGITS_to_I(charptr,index2-index1,(uintD)base);
      if (sign==0)
        { return x; }
        else
        { return I_minus_I(x); } # negatives Vorzeichen -> Vorzeichenwechsel
    }}

# UP: Wandelt eine Zeichenkette mit Rational-Syntax in eine rationale Zahl um.
# read_rational(base,sign,string,index1,index3,index2)
# > base: Lesebasis (>=2, <=36)
# > sign: Vorzeichen (/=0 falls negativ)
# > string: Normal-Simple-String (enthält Ziffern mit Wert <base und Bruchstrich)
# > index1: Index der ersten Ziffer
# > index3: Index von '/'
# > index2: Index nach der letzten Ziffer
#   (also index3-index1 Zähler-Ziffern, index2-index3-1 Nenner-Ziffern)
# < ergebnis: rationale Zahl
# can trigger GC
  global object read_rational (uintWL base,
         signean sign, object string, uintL index1, uintL index3, uintL index2);
  global object read_rational(base,sign,string,index1,index3,index2)
    var uintWL base;
    var signean sign;
    var object string;
    var uintL index1;
    var uintL index3;
    var uintL index2;
    { pushSTACK(string); # string retten
     {var uintL index3_1 = index3+1; # Index der ersten Nennerziffer
      var object x = # Nenner
        DIGITS_to_I(&TheSstring(string)->data[index3_1],index2-index3_1,(uintD)base);
      if (eq(x,Fixnum_0)) { divide_0(); } # Division durch 0 abfangen
      string = STACK_0; STACK_0 = x;
     }
     {var object x = # Zähler
        DIGITS_to_I(&TheSstring(string)->data[index1],index3-index1,(uintD)base);
      if (!(sign==0)) { x = I_minus_I(x); } # incl. Vorzeichen
      return I_posI_durch_RA(x,popSTACK()); # Zähler/Nenner als Bruch
    }}

# UP: Wandelt eine Zeichenkette mit Float-Syntax in ein Float um.
# read_float(base,sign,string,index1,index4,index2,index3)
# > base: Lesebasis (=10)
# > sign: Vorzeichen (/=0 falls negativ)
# > string: Normal-Simple-String (enthält Ziffern und evtl. Punkt und Exponentmarker)
# > index1: Index vom Mantissenanfang (excl. Vorzeichen)
# > index4: Index nach dem Mantissenende
# > index2: Index beim Ende der Characters
# > index3: Index nach dem Dezimalpunkt (=index4 falls keiner da)
#   (also Mantisse mit index4-index1 Characters: Ziffern und max. 1 '.')
#   (also index4-index3 Nachkommaziffern)
#   (also bei index4<index2: index4 = Index des Exponent-Markers,
#    index4+1 = Index des Exponenten-Vorzeichens oder der ersten
#    Exponenten-Ziffer)
# < ergebnis: Float
# can trigger GC
  global object read_float (uintWL base,
         signean sign, object string, uintL index1, uintL index4, uintL index2, uintL index3);
  global object read_float(base,sign,string,index1,index4,index2,index3)
    var uintWL base;
    var signean sign;
    var object string;
    var uintL index1;
    var uintL index4;
    var uintL index2;
    var uintL index3;
    { pushSTACK(string); # string retten
      # Exponent:
     {var chart exp_marker;
      var object exponent;
      {var uintL exp_len = index2-index4; # Anzahl Stellen des Exponenten
       if (exp_len > 0)
         { var const chart* ptr = &TheSstring(string)->data[index4]; # zeigt auf den Exponentmarker
           exp_marker = *ptr++; exp_len--; # Exponentmarker überlesen
                        # (als Großbuchstabe, da vom Aufrufer umgewandelt)
          {var signean exp_sign = 0; # Exponenten-Vorzeichen
           switch (as_cint(*ptr))
             { case '-': exp_sign = ~exp_sign; # Vorzeichen := negativ
               case '+': ptr++; exp_len--; # Exponenten-Vorzeichen überlesen
               default: ;
             }
           exponent = DIGITS_to_I(ptr,exp_len,(uintD)base); # Exponent in Integer umwandeln
           if (!(exp_sign==0)) { exponent = I_minus_I(exponent); } # incl. Vorzeichen
         }}
         else
         # kein Exponent da
         { exp_marker = ascii('E'); exponent = Fixnum_0; }
       # exp_marker = Exponentmarker als Großbuchtabe,
       # exponent = Exponent als Integer.
       exponent = # Exponent - Anzahl der Nachkommaziffern
         I_I_minus_I(exponent,fixnum(index4-index3));
       exponent = # 10^exponent = zu multiplizierende Zehnerpotenz
         R_I_expt_R(fixnum(base),exponent);
       string = STACK_0; STACK_0 = exponent;
       # Mantisse:
       {var object mantisse = # Mantisse als Integer
         DIGITS_to_I(&TheSstring(string)->data[index1],index4-index1,(uintD)base);
        exponent = popSTACK();
        # Mantisse (Integer) und Exponent (rational >0) unelegant zusammenmultiplizieren:
        if (RA_integerp(exponent))
          { mantisse = I_I_mal_I(mantisse,exponent); }
          else
          { # falls mantisse/=0, in exponent=1/10^i den Zähler durch mantisse
            # ersetzen (liefert ungekürzten Bruch, Vorsicht!)
            if (!(eq(mantisse,Fixnum_0)))
              { TheRatio(exponent)->rt_num = mantisse; mantisse = exponent; }
          }
        # mantisse = Mantisse * Zehnerpotenz, als ungekürzte rationale Zahl!
        switch (as_cint(exp_marker))
          { case 'S': SF: # in Short-Float umwandeln
              {var object x = RA_to_SF(mantisse);
               return (sign==0 ? x : SF_minus_SF(x)); # evtl. noch Vorzeichenwechsel
              }
            case 'F': FF: # in Single-Float umwandeln
              {var object x = RA_to_FF(mantisse);
               return (sign==0 ? x : FF_minus_FF(x)); # evtl. noch Vorzeichenwechsel
              }
            case 'D': DF: # in Double-Float umwandeln
              {var object x = RA_to_DF(mantisse);
               return (sign==0 ? x : DF_minus_DF(x)); # evtl. noch Vorzeichenwechsel
              }
            case 'L': LF: # in Long-Float der Default-Genauigkeit umwandeln
              {var object x = RA_to_LF(mantisse,I_to_UL(O(LF_digits)));
               return (sign==0 ? x : LF_minus_LF(x)); # evtl. noch Vorzeichenwechsel
              }
            default: # case 'E':
              defaultfloatcase(S(read_default_float_format),
                               goto SF; , goto FF; , goto DF; , goto LF; ,
                               pushSTACK(mantisse); , mantisse = popSTACK();
                              );
          }
    }}}}


# =========================================================================== #
#                       Ausgaberoutinen für Zahlen

# UP: Gibt ein Integer aus.
# print_integer(z,base,&stream);
# > z: Integer
# > base: Basis (>=2, <=36)
# > stream: Stream
# < stream: Stream
# can trigger GC
  global void print_integer (object z, uintWL base, const object* stream_);
  global void print_integer(z,base,stream_)
    var object z;
    var uintWL base;
    var const object* stream_;
    { if (R_minusp(z))
        # z<0 -> Vorzeichen ausgeben:
        { pushSTACK(z);
          write_ascii_char(stream_,'-');
          z = I_minus_I(popSTACK());
        }
     {  SAVE_NUM_STACK # num_stack retten
        var uintD* MSDptr;
        var uintC len;
        I_to_NDS(z, MSDptr=,len=,); # z als UDS
      { var uintL need = digits_need(len,base);
        var DYNAMIC_STRING(digits,need);
        pushSTACK(digits);
       {var DIGITS erg; erg.LSBptr = &TheSstring(digits)->data[need];
        UDS_to_DIGITS(MSDptr,len,(uintD)base,&erg); # Umwandlung in Ziffern
        # Ziffern ausgeben:
        write_char_array(stream_,&STACK_0,erg.MSBptr-&TheSstring(digits)->data[0],erg.len);
        FREE_DYNAMIC_STRING(STACK_0);
        skipSTACK(1);
        RESTORE_NUM_STACK # num_stack zurück
    }}}}

# UP: Gibt ein Float aus.
# print_float(z,&stream);
# > z: Float
# > stream: Stream
# < stream: Stream
# can trigger GC
  global void print_float (object z, const object* stream_);
  global void print_float(z,stream_)
    var object z;
    var const object* stream_;
    { # Falls SYS::WRITE-FLOAT definiert ist, (SYS::WRITE-FLOAT stream z) aufrufen:
      var object fun = Symbol_function(S(write_float));
      if (!eq(fun,unbound))
        # Funktion aufrufen
        { pushSTACK(*stream_); pushSTACK(z); funcall(fun,2); }
        else
        # eigene Routine: gibt
        # Vorzeichen, Punkt, Mantisse (binär), (Zweiersystem-)Exponent (dezimal)
        # aus.
        { pushSTACK(z);
          F_integer_decode_float_I_I_I(z);
          # Stackaufbau: z, m, e, s.
          # Vorzeichen ausgeben, falls <0:
          if (eq(STACK_0,Fixnum_minus1)) { write_ascii_char(stream_,'-'); }
          # Mantisse binär(!) ausgeben:
          write_ascii_char(stream_,'.');
          print_integer(STACK_2,2,stream_);
          # Exponent-Marker ausgeben:
          {var object exp_marker;
           floatcase(STACK_3,
                     { exp_marker = ascii_char('s'); },
                     { exp_marker = ascii_char('f'); },
                     { exp_marker = ascii_char('d'); },
                     { exp_marker = ascii_char('L'); }
                    );
           write_char(stream_,exp_marker);
          }
          # Exponenten dezimal ausgeben:
          print_integer(L_to_I(F_exponent_L(STACK_3)),10,stream_);
          skipSTACK(4);
        }
    }


# =========================================================================== #
#                           Lisp-Funktionen

# Fehlermeldung, wenn keine Zahl kommt.
# > obj: Objekt, keine Zahl
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_not_N, (object obj));
  local void fehler_not_N(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(number)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("argument to ~ should be a number: ~")
            );
    }

# Fehlermeldung, wenn keine reelle Zahl kommt.
# > obj: Objekt, keine reelle Zahl
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_not_R, (object obj));
  local void fehler_not_R(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(real)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("argument to ~ should be a real number: ~")
            );
    }

# Fehlermeldung, wenn keine Floating-Point-Zahl kommt.
# > obj: Objekt, keine Floating-Point-Zahl
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_not_F, (object obj));
  local void fehler_not_F(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(float)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("argument to ~ should be a floating point number: ~")
            );
    }

# Fehlermeldung, wenn keine rationale Zahl kommt.
# > obj: Objekt, keine rationale Zahl
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_not_RA, (object obj));
  local void fehler_not_RA(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(rational)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("argument to ~ should be a rational number: ~")
            );
    }

# Fehlermeldung, wenn keine ganze Zahl kommt.
# > obj: Objekt, keine ganze Zahl
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_not_I, (object obj));
  local void fehler_not_I(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(integer)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("argument to ~ should be an integer: ~")
            );
    }

# Fehlermeldung wegen illegalem Digits-Argument obj.
# > obj: Objekt
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_digits, (object obj));
  local void fehler_digits(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_posfixnum1)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument should be a positive fixnum, not ~")
            );
    }

# check_number(obj) überprüft, ob obj eine Zahl ist.
# > subr_self: Aufrufer (ein SUBR)
  #define check_number(obj)  { if (!numberp(obj)) { fehler_not_N(obj); } }

# check_real(obj) überprüft, ob obj eine reelle Zahl ist.
# > subr_self: Aufrufer (ein SUBR)
  #define check_real(obj)  if_realp(obj, ; , { fehler_not_R(obj); } );

# check_float(obj) überprüft, ob obj eine Floating-Point-Zahl ist.
# > subr_self: Aufrufer (ein SUBR)
  #define check_float(obj)  { if (!floatp(obj)) { fehler_not_F(obj); } }

# check_rational(obj) überprüft, ob obj eine rationale Zahl ist.
# > subr_self: Aufrufer (ein SUBR)
  #define check_rational(obj)  if_rationalp(obj, ; , { fehler_not_RA(obj); } );

# check_integer(obj) überprüft, ob obj eine ganze Zahl ist.
# > subr_self: Aufrufer (ein SUBR)
  #define check_integer(obj)  { if (!integerp(obj)) { fehler_not_I(obj); } }

# UP: Returns the decimal string representation of an integer >= 0.
# decimal_string(x)
# > object x: an integer >= 0
# < object result: a normal-simple-string containing the digits
# can trigger GC
  global object decimal_string (object x);
  global object decimal_string(x)
    var object x;
    {  SAVE_NUM_STACK # num_stack retten
       var uintD* MSDptr;
       var uintC len;
       I_to_NDS(x, MSDptr=,len=,); # x (>=0) als UDS
     { var uintL need = digits_need(len,10);
       var DYNAMIC_ARRAY(ziffern,chart,need); # Platz für die Ziffern
       var DIGITS erg; erg.LSBptr = &ziffern[need];
       UDS_to_DIGITS(MSDptr,len,10,&erg); # Umwandlung in Ziffern
       # Ziffern in Normal-Simple-String schreiben:
      {var object string = allocate_string(erg.len);
       chartcopy(erg.MSBptr,&TheSstring(string)->data[0],erg.len);
       FREE_DYNAMIC_ARRAY(ziffern);
       RESTORE_NUM_STACK # num_stack zurück
       return string;
    }}}

LISPFUNN(decimal_string,1)
# (SYS::DECIMAL-STRING integer)
# liefert zu einem Integer >=0  (write-to-string integer :base 10 :radix nil),
# also die Ziffernfolge als Simple-String.
  { var object x = popSTACK();
    check_integer(x);
    value1 = decimal_string(x); mv_count=1;
  }

LISPFUNN(zerop,1)
# (ZEROP number), CLTL S. 195
  { var object x = popSTACK();
    check_number(x);
    value1 = (N_zerop(x) ? T : NIL); mv_count=1;
  }

LISPFUNN(plusp,1)
# (PLUSP real), CLTL S. 196
  { var object x = popSTACK();
    check_real(x);
    value1 = (R_plusp(x) ? T : NIL); mv_count=1;
  }

LISPFUNN(minusp,1)
# (MINUSP real), CLTL S. 196
  { var object x = popSTACK();
    check_real(x);
    value1 = (R_minusp(x) ? T : NIL); mv_count=1;
  }

LISPFUNN(oddp,1)
# (ODDP integer), CLTL S. 196
  { var object x = popSTACK();
    check_integer(x);
    value1 = (I_oddp(x) ? T : NIL); mv_count=1;
  }

LISPFUNN(evenp,1)
# (EVENP integer), CLTL S. 196
  { var object x = popSTACK();
    check_integer(x);
    value1 = (I_oddp(x) ? NIL : T); mv_count=1;
  }

# UP: Testet, ob alle argcount+1 Argumente unterhalb von args_pointer
# Zahlen sind. Wenn nein, Error.
# > argcount: Argumentezahl-1
# > args_pointer: Pointer über die Argumente
# > subr_self: Aufrufer (ein SUBR)
  local void test_number_args (uintC argcount, object* args_pointer);
  local void test_number_args(argcount,args_pointer)
    var uintC argcount;
    var object* args_pointer;
    { dotimespC(argcount,argcount+1,
        { var object arg = NEXT(args_pointer); # nächstes Argument
          check_number(arg); # muss eine Zahl sein
        });
    }

# UP: Testet, ob alle argcount+1 Argumente unterhalb von args_pointer
# reelle Zahlen sind. Wenn nein, Error.
# > argcount: Argumentezahl-1
# > args_pointer: Pointer über die Argumente
# > subr_self: Aufrufer (ein SUBR)
  local void test_real_args (uintC argcount, object* args_pointer);
  local void test_real_args(argcount,args_pointer)
    var uintC argcount;
    var object* args_pointer;
    { dotimespC(argcount,argcount+1,
        { var object arg = NEXT(args_pointer); # nächstes Argument
          check_real(arg); # muss eine reelle Zahl sein
        });
    }

# UP: Testet, ob alle argcount+1 Argumente unterhalb von args_pointer
# ganze Zahlen sind. Wenn nein, Error.
# > argcount: Argumentezahl-1
# > args_pointer: Pointer über die Argumente
# > subr_self: Aufrufer (ein SUBR)
  local void test_integer_args (uintC argcount, object* args_pointer);
  local void test_integer_args(argcount,args_pointer)
    var uintC argcount;
    var object* args_pointer;
    { dotimespC(argcount,argcount+1,
        { var object arg = NEXT(args_pointer); # nächstes Argument
          check_integer(arg); # muss eine ganze Zahl sein
        });
    }

LISPFUN(gleich,1,0,rest,nokey,0,NIL)
# (= number {number}), CLTL S. 196
  { var object* args_pointer = rest_args_pointer STACKop 1;
    test_number_args(argcount,args_pointer); # Alle Argumente Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=0 to n-1 do ( if Arg[i]/=Arg[i+1] then return(NIL) ), return(T).
    if (argcount > 0)
      { var const object* arg_i_ptr = args_pointer;
        dotimespC(argcount,argcount,
          { var object arg_i = NEXT(arg_i_ptr);
            if (!N_N_gleich(arg_i,Next(arg_i_ptr))) goto no;
          });
      }
    yes: value1 = T; goto ok;
    no: value1 = NIL; goto ok;
    ok: mv_count=1; set_args_end_pointer(args_pointer);
  }

LISPFUN(ungleich,1,0,rest,nokey,0,NIL)
# (/= number {number}), CLTL S. 196
  { var object* args_pointer = rest_args_pointer STACKop 1;
    test_number_args(argcount,args_pointer); # Alle Argumente Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for j:=1 to n do
    #   for i:=0 to j-1 do
    #     if Arg[i]=Arg[j] then return(NIL),
    # return(T).
    if (argcount > 0)
      { var const object* arg_j_ptr = rest_args_pointer;
        dotimespC(argcount,argcount,
          { var const object* arg_i_ptr = args_pointer;
            do { if (N_N_gleich(NEXT(arg_i_ptr),Next(arg_j_ptr))) goto no; }
               until (arg_i_ptr==arg_j_ptr);
            arg_j_ptr skipSTACKop -1;
          });
      }
    yes: value1 = T; goto ok;
    no: value1 = NIL; goto ok;
    ok: mv_count=1; set_args_end_pointer(args_pointer);
  }

LISPFUN(kleiner,1,0,rest,nokey,0,NIL)
# (< real {real}), CLTL S. 196
  { var object* args_pointer = rest_args_pointer STACKop 1;
    test_real_args(argcount,args_pointer); # Alle Argumente reelle Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=0 to n-1 do ( if Arg[i]>=Arg[i+1] then return(NIL) ), return(T).
    if (argcount > 0)
      { var const object* arg_i_ptr = args_pointer;
        dotimespC(argcount,argcount,
          { var object arg_i = NEXT(arg_i_ptr);
            if (R_R_comp(arg_i,Next(arg_i_ptr))>=0) goto no;
          });
      }
    yes: value1 = T; goto ok;
    no: value1 = NIL; goto ok;
    ok: mv_count=1; set_args_end_pointer(args_pointer);
  }

LISPFUN(groesser,1,0,rest,nokey,0,NIL)
# (> real {real}), CLTL S. 196
  { var object* args_pointer = rest_args_pointer STACKop 1;
    test_real_args(argcount,args_pointer); # Alle Argumente reelle Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=0 to n-1 do ( if Arg[i]<=Arg[i+1] then return(NIL) ), return(T).
    if (argcount > 0)
      { var const object* arg_i_ptr = args_pointer;
        dotimespC(argcount,argcount,
          { var object arg_i = NEXT(arg_i_ptr);
            if (R_R_comp(arg_i,Next(arg_i_ptr))<=0) goto no;
          });
      }
    yes: value1 = T; goto ok;
    no: value1 = NIL; goto ok;
    ok: mv_count=1; set_args_end_pointer(args_pointer);
  }

LISPFUN(klgleich,1,0,rest,nokey,0,NIL)
# (<= real {real}), CLTL S. 196
  { var object* args_pointer = rest_args_pointer STACKop 1;
    test_real_args(argcount,args_pointer); # Alle Argumente reelle Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=0 to n-1 do ( if Arg[i]>Arg[i+1] then return(NIL) ), return(T).
    if (argcount > 0)
      { var const object* arg_i_ptr = args_pointer;
        dotimespC(argcount,argcount,
          { var object arg_i = NEXT(arg_i_ptr);
            if (R_R_comp(arg_i,Next(arg_i_ptr))>0) goto no;
          });
      }
    yes: value1 = T; goto ok;
    no: value1 = NIL; goto ok;
    ok: mv_count=1; set_args_end_pointer(args_pointer);
  }

LISPFUN(grgleich,1,0,rest,nokey,0,NIL)
# (>= real {real}), CLTL S. 196
  { var object* args_pointer = rest_args_pointer STACKop 1;
    test_real_args(argcount,args_pointer); # Alle Argumente reelle Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # for i:=0 to n-1 do ( if Arg[i]<Arg[i+1] then return(NIL) ), return(T).
    if (argcount > 0)
      { var const object* arg_i_ptr = args_pointer;
        dotimespC(argcount,argcount,
          { var object arg_i = NEXT(arg_i_ptr);
            if (R_R_comp(arg_i,Next(arg_i_ptr))<0) goto no;
          });
      }
    yes: value1 = T; goto ok;
    no: value1 = NIL; goto ok;
    ok: mv_count=1; set_args_end_pointer(args_pointer);
  }

LISPFUN(max,1,0,rest,nokey,0,NIL)
# (MAX real {real}), CLTL S. 198
# Methode:
# (max x1 x2 x3 ... xn) = (max ...(max (max x1 x2) x3)... xn)
  { var object* args_pointer = rest_args_pointer STACKop 1;
    test_real_args(argcount,args_pointer); # Alle Argumente reelle Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # x:=Arg[0], for i:=1 to n do ( x := max(x,Arg[i]) ), return(x).
    { var object* arg_i_ptr = args_pointer;
      var object x = NEXT(arg_i_ptr); # bisheriges Maximum
      dotimesC(argcount,argcount, { x = R_R_max_R(x,NEXT(arg_i_ptr)); } );
      value1 = x; mv_count=1; set_args_end_pointer(args_pointer);
  } }

LISPFUN(min,1,0,rest,nokey,0,NIL)
# (MIN real {real}), CLTL S. 198
# Methode:
# (min x1 x2 x3 ... xn) = (min ...(min (min x1 x2) x3)... xn)
  { var object* args_pointer = rest_args_pointer STACKop 1;
    test_real_args(argcount,args_pointer); # Alle Argumente reelle Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # x:=Arg[0], for i:=1 to n do ( x := min(x,Arg[i]) ), return(x).
    { var object* arg_i_ptr = args_pointer;
      var object x = NEXT(arg_i_ptr); # bisheriges Minimum
      dotimesC(argcount,argcount, { x = R_R_min_R(x,NEXT(arg_i_ptr)); } );
      value1 = x; mv_count=1; set_args_end_pointer(args_pointer);
  } }

LISPFUN(plus,0,0,rest,nokey,0,NIL)
# (+ {number}), CLTL S. 199
# Methode:
# (+) = 0
# (+ x1 x2 x3 ... xn) = (+ ...(+ (+ x1 x2) x3)... xn)
  { if (argcount==0) { value1 = Fixnum_0; mv_count=1; return; }
    argcount--;
    test_number_args(argcount,rest_args_pointer); # Alle Argumente Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # x:=Arg[0], for i:=1 to n do ( x := x+Arg[i] ), return(x).
    { var object* arg_i_ptr = rest_args_pointer;
      var object x = NEXT(arg_i_ptr); # bisherige Summe
      dotimesC(argcount,argcount, { x = N_N_plus_N(x,NEXT(arg_i_ptr)); } );
      value1 = x; mv_count=1; set_args_end_pointer(rest_args_pointer);
  } }

LISPFUN(minus,1,0,rest,nokey,0,NIL)
# (- number {number}), CLTL S. 199
# Methode:
# (- x) extra.
# (- x1 x2 x3 ... xn) = (- ...(- (- x1 x2) x3)... xn)
  { var object* args_pointer = rest_args_pointer STACKop 1;
    test_number_args(argcount,args_pointer); # Alle Argumente Zahlen?
    if (argcount==0)
      # unäres Minus
      { value1 = N_minus_N(Next(args_pointer)); }
      else
      # Methode:
      # n+1 Argumente Arg[0..n].
      # x:=Arg[0], for i:=1 to n do ( x := x-Arg[i] ), return(x).
      { var object* arg_i_ptr = args_pointer;
        var object x = NEXT(arg_i_ptr); # bisherige Differenz
        dotimespC(argcount,argcount, { x = N_N_minus_N(x,NEXT(arg_i_ptr)); } );
        value1 = x;
      }
    mv_count=1; set_args_end_pointer(args_pointer);
  }

LISPFUN(mal,0,0,rest,nokey,0,NIL)
# (* {number}), CLTL S. 199
# Methode:
# (*) = 1
# (* x1 x2 x3 ... xn) = (* ...(* (* x1 x2) x3)... xn)
  { if (argcount==0) { value1 = Fixnum_1; mv_count=1; return; }
    argcount--;
    test_number_args(argcount,rest_args_pointer); # Alle Argumente Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # x:=Arg[0], for i:=1 to n do ( x := x*Arg[i] ), return(x).
    { var object* arg_i_ptr = rest_args_pointer;
      var object x = NEXT(arg_i_ptr); # bisheriges Produkt
      dotimesC(argcount,argcount,
        { var object arg = NEXT(arg_i_ptr);
          x = (eq(x,arg) ? N_square_N(x) : N_N_mal_N(x,arg));
        });
      value1 = x; mv_count=1; set_args_end_pointer(rest_args_pointer);
  } }

LISPFUN(durch,1,0,rest,nokey,0,NIL)
# (/ number {number}), CLTL S. 200
# Methode:
# (/ x) extra.
# (/ x1 x2 x3 ... xn) = (/ ...(/ (/ x1 x2) x3)... xn)
  { var object* args_pointer = rest_args_pointer STACKop 1;
    test_number_args(argcount,args_pointer); # Alle Argumente Zahlen?
    if (argcount==0)
      # unäres Durch
      { value1 = N_durch_N(Next(args_pointer)); }
      else
      # Methode:
      # n+1 Argumente Arg[0..n].
      # x:=Arg[0], for i:=1 to n do ( x := x/Arg[i] ), return(x).
      { var object* arg_i_ptr = args_pointer;
        var object x = NEXT(arg_i_ptr); # bisherige Differenz
        dotimespC(argcount,argcount, { x = N_N_durch_N(x,NEXT(arg_i_ptr)); } );
        value1 = x;
      }
    mv_count=1; set_args_end_pointer(args_pointer);
  }

LISPFUNN(einsplus,1)
# (1+ number), CLTL S. 200
  { var object x = popSTACK();
    check_number(x);
    value1 = N_1_plus_N(x); mv_count=1;
  }

LISPFUNN(einsminus,1)
# (1- number), CLTL S. 200
  { var object x = popSTACK();
    check_number(x);
    value1 = N_minus1_plus_N(x); mv_count=1;
  }

LISPFUNN(conjugate,1)
# (CONJUGATE number), CLTL S. 201
  { var object x = popSTACK();
    check_number(x);
    value1 = N_conjugate_N(x); mv_count=1;
  }

LISPFUN(gcd,0,0,rest,nokey,0,NIL)
# (GCD {integer}), CLTL S. 202
# Methode:
# (gcd) = 0
# (gcd x) = (abs x)
# (gcd x1 x2 x3 ... xn) = (gcd ...(gcd (gcd x1 x2) x3)... xn)
  { if (argcount==0) { value1 = Fixnum_0; mv_count=1; return; }
    argcount--;
    test_integer_args(argcount,rest_args_pointer); # Alle Argumente ganze Zahlen?
    if (argcount==0)
      { value1 = I_abs_I(Next(rest_args_pointer)); }
      else
      # Methode:
      # n+1 Argumente Arg[0..n].
      # x:=Arg[0], for i:=1 to n do ( x := gcd(x,Arg[i]) ), return(x).
      { var object* arg_i_ptr = rest_args_pointer;
        var object x = NEXT(arg_i_ptr); # bisheriger ggT
        dotimespC(argcount,argcount, { x = I_I_gcd_I(x,NEXT(arg_i_ptr)); } );
        value1 = x;
      }
    mv_count=1; set_args_end_pointer(rest_args_pointer);
  }

LISPFUN(xgcd,0,0,rest,nokey,0,NIL)
# (XGCD {integer})
# (XGCD x1 ... xn) liefert n+1 Werte: g = (gcd x1 ... xn), ein Integer >=0,
# und n Integers u1,...,un mit g = u1*x1+...+un*xn.
# Methode:
# (xgcd) = 0
# (xgcd x) = (abs x), (signum x)
# (xgcd x1 x2 x3 ... xn) mit n>=2:
#   (g,u[1],u[2]) := (xgcd x1 x2),
#   für i=3,...,n:
#     (g',u,v) := (xgcd g xi),
#     (g,u[1],...,u[i]) := (g',u*u[1],...,u*u[i-1],v).
  { if (argcount==0) { value1 = Fixnum_0; mv_count=1; return; }
    argcount--;
    test_integer_args(argcount,rest_args_pointer); # Alle Argumente ganze Zahlen?
    if (argcount==0)
      { var object arg = Next(rest_args_pointer);
        if (R_minusp(arg))
          { value1 = arg; value2 = Fixnum_minus1; }
          else
          { value1 = I_minus_I(arg); value2 = Fixnum_1; }
        mv_count=2;
      }
      else
      # Methode:
      # n+1 Argumente Arg[0..n].
      # (g,u,v):=xgcd(Arg[0],Arg[1]), Arg[0]:=u, Arg[1]:=v,
      # for i:=2 to n do
      #   ( (g,u,v):=xgcd(g,Arg[i]), Arg[i]:=v,
      #     for j:=i-1 downto 0 do Arg[j]:=u*Arg[j],
      #   ),
      # return values(g,Arg[0],...,Arg[n]).
      { var object* arg_i_ptr = rest_args_pointer;
        var object g; # bisheriger ggT
        {var object arg_0 = NEXT(arg_i_ptr);
         var object arg_1 = Next(arg_i_ptr);
         I_I_xgcd_I_I_I(arg_0,arg_1);
         Before(arg_i_ptr) = STACK_2;
        }
        loop
          { NEXT(arg_i_ptr) = STACK_1;
            g = STACK_0; skipSTACK(3);
            if (arg_i_ptr == args_end_pointer) break;
            I_I_xgcd_I_I_I(g,Next(arg_i_ptr));
           {var object* arg_j_ptr = arg_i_ptr;
            do { var object arg_j = Before(arg_j_ptr);
                 BEFORE(arg_j_ptr) = I_I_mal_I(STACK_2,arg_j);
               }
               until (arg_j_ptr == rest_args_pointer);
          }}
        value1 = g; # g als 1. Wert
        # Beifaktoren als weitere Werte:
        {var object* mvp = &value2;
         var object* arg_i_ptr = rest_args_pointer;
         if (argcount >= mv_limit-2) { fehler_mv_zuviel(S(xgcd)); }
         mv_count = argcount+2;
         dotimespC(argcount,argcount+1, { *mvp++ = NEXT(arg_i_ptr); } );
      } }
    set_args_end_pointer(rest_args_pointer);
  }

LISPFUN(lcm,0,0,rest,nokey,0,NIL)
# (LCM {integer})
# Methode:
# (lcm) = 1 (neutrales Element der lcm-Operation)
# (lcm x) = (abs x)
# (lcm x1 x2 x3 ... xn) = (lcm ...(lcm (lcm x1 x2) x3)... xn)
  { if (argcount==0) { value1 = Fixnum_1; mv_count=1; return; }
    argcount--;
    test_integer_args(argcount,rest_args_pointer); # Alle Argumente ganze Zahlen?
    if (argcount==0)
      { value1 = I_abs_I(Next(rest_args_pointer)); }
      else
      # Methode:
      # n+1 Argumente Arg[0..n].
      # x:=Arg[0], for i:=1 to n do ( x := lcm(x,Arg[i]) ), return(x).
      { var object* arg_i_ptr = rest_args_pointer;
        var object x = NEXT(arg_i_ptr); # bisheriges kgV
        dotimespC(argcount,argcount, { x = I_I_lcm_I(x,NEXT(arg_i_ptr)); } );
        value1 = x;
      }
    mv_count=1; set_args_end_pointer(rest_args_pointer);
  }

LISPFUNN(exp,1)
# (EXP number), CLTL S. 203
  { var object x = popSTACK();
    check_number(x);
    value1 = N_exp_N(x); mv_count=1;
  }

LISPFUNN(expt,2)
# (EXPT number number), CLTL S. 203
  { var object x = STACK_1;
    var object y = STACK_0;
    check_number(x); check_number(y); skipSTACK(2);
    value1 = N_N_expt_N(x,y); mv_count=1;
  }

LISPFUN(log,1,1,norest,nokey,0,NIL)
# (LOG number [base-number]), CLTL S. 204
  { var object base = popSTACK();
    var object arg = popSTACK();
    check_number(arg);
    if (eq(base,unbound))
      # LOG mit einem Argument
      { value1 = N_log_N(arg); }
      else
      # LOG mit zwei Argumenten
      { check_number(base);
        value1 = N_N_log_N(arg,base);
      }
    mv_count=1;
  }

LISPFUNN(sqrt,1)
# (SQRT number), CLTL S. 205
  { var object x = popSTACK();
    check_number(x);
    value1 = N_sqrt_N(x); mv_count=1;
  }

LISPFUNN(isqrt,1)
# (ISQRT integer), CLTL S. 205
  { var object x = popSTACK();
    check_integer(x);
    value1 = (I_isqrt_I(x), popSTACK()); mv_count=1;
  }

LISPFUNN(abs,1)
# (ABS number), CLTL S. 205
  { var object x = popSTACK();
    check_number(x);
    value1 = N_abs_R(x); mv_count=1;
  }

LISPFUNN(phase,1)
# (PHASE number), CLTL S. 206
  { var object x = popSTACK();
    check_number(x);
    value1 = N_phase_R(x); mv_count=1;
  }

LISPFUNN(signum,1)
# (SIGNUM number), CLTL S. 206
  { var object x = popSTACK();
    check_number(x);
    value1 = N_signum_N(x); mv_count=1;
  }

LISPFUNN(sin,1)
# (SIN number), CLTL S. 207
  { var object x = popSTACK();
    check_number(x);
    value1 = N_sin_N(x); mv_count=1;
  }

LISPFUNN(cos,1)
# (COS number), CLTL S. 207
  { var object x = popSTACK();
    check_number(x);
    value1 = N_cos_N(x); mv_count=1;
  }

LISPFUNN(tan,1)
# (TAN number), CLTL S. 207
  { var object x = popSTACK();
    check_number(x);
    value1 = N_tan_N(x); mv_count=1;
  }

LISPFUNN(cis,1)
# (CIS number), CLTL S. 207
  { var object x = popSTACK();
    check_number(x);
    value1 = N_cis_N(x); mv_count=1;
  }

LISPFUNN(asin,1)
# (ASIN number), CLTL S. 207
  { var object x = popSTACK();
    check_number(x);
    value1 = N_asin_N(x); mv_count=1;
  }

LISPFUNN(acos,1)
# (ACOS number), CLTL S. 207
  { var object x = popSTACK();
    check_number(x);
    value1 = N_acos_N(x); mv_count=1;
  }

LISPFUN(atan,1,1,norest,nokey,0,NIL)
# (ATAN number [real]), CLTL S. 207
  { var object arg2 = popSTACK();
    var object arg1 = popSTACK();
    if (eq(arg2,unbound))
      # 1 Argument
      { check_number(arg1);
        value1 = N_atan_N(arg1);
      }
      else
      # 2 Argumente
      { check_real(arg1); check_real(arg2);
        value1 = R_R_atan_R(arg2,arg1); # atan(X=arg2,Y=arg1)
      }
    mv_count=1;
  }

LISPFUNN(sinh,1)
# (SINH number), CLTL S. 209
  { var object x = popSTACK();
    check_number(x);
    value1 = N_sinh_N(x); mv_count=1;
  }

LISPFUNN(cosh,1)
# (COSH number), CLTL S. 209
  { var object x = popSTACK();
    check_number(x);
    value1 = N_cosh_N(x); mv_count=1;
  }

LISPFUNN(tanh,1)
# (TANH number), CLTL S. 209
  { var object x = popSTACK();
    check_number(x);
    value1 = N_tanh_N(x); mv_count=1;
  }

LISPFUNN(asinh,1)
# (ASINH number), CLTL S. 209
  { var object x = popSTACK();
    check_number(x);
    value1 = N_asinh_N(x); mv_count=1;
  }

LISPFUNN(acosh,1)
# (ACOSH number), CLTL S. 209
  { var object x = popSTACK();
    check_number(x);
    value1 = N_acosh_N(x); mv_count=1;
  }

LISPFUNN(atanh,1)
# (ATANH number), CLTL S. 209
  { var object x = popSTACK();
    check_number(x);
    value1 = N_atanh_N(x); mv_count=1;
  }

LISPFUN(float,1,1,norest,nokey,0,NIL)
# (FLOAT number [float]), CLTL S. 214
  { var object arg2 = popSTACK();
    var object arg1 = popSTACK();
    check_real(arg1);
    if (eq(arg2,unbound))
      # 1 Argument
      { value1 = R_float_F(arg1); }
      else
      # 2 Argumente
      { check_float(arg2); value1 = R_F_float_F(arg1,arg2); }
    mv_count=1;
  }

# UP: Wandelt ein Objekt in ein Float von gegebenem Typ um.
# coerce_float(obj,type)
# > obj: Objekt
# > type: Eines der Symbole
#         FLOAT, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: (coerce obj type)
# can trigger GC
  global object coerce_float (object obj, object type);
  global object coerce_float(obj,type)
    var object obj;
    var object type;
    { check_real(obj);
      if (eq(type,S(short_float))) # SHORT-FLOAT
        { return R_to_SF(obj); }
      elif (eq(type,S(single_float))) # SINGLE-FLOAT
        { return R_to_FF(obj); }
      elif (eq(type,S(double_float))) # DOUBLE-FLOAT
        { return R_to_DF(obj); }
      elif (eq(type,S(long_float))) # LONG-FLOAT
        { return R_to_LF(obj,I_to_UL(O(LF_digits))); } # Default-Genauigkeit
      else # FLOAT
        { return R_float_F(obj); }
    }

LISPFUNN(rational,1)
# (RATIONAL real), CLTL S. 214
  { var object x = popSTACK();
    check_real(x);
    value1 = R_rational_RA(x); mv_count=1;
  }

LISPFUNN(rationalize,1)
# (RATIONALIZE real), CLTL S. 214
  { var object x = popSTACK();
    check_real(x);
    value1 = R_rationalize_RA(x); mv_count=1;
  }

LISPFUNN(numerator,1)
# (NUMERATOR rational), CLTL S. 215
  { var object x = popSTACK();
    check_rational(x);
    value1 = (RA_integerp(x) ? x : TheRatio(x)->rt_num); mv_count=1;
  }

LISPFUNN(denominator,1)
# (DENOMINATOR rational), CLTL S. 215
  { var object x = popSTACK();
    check_rational(x);
    value1 = (RA_integerp(x) ? Fixnum_1 : TheRatio(x)->rt_den); mv_count=1;
  }

LISPFUN(floor,1,1,norest,nokey,0,NIL)
# (FLOOR real [real]), CLTL S. 215
  { var object y = popSTACK();
    var object x = popSTACK();
    check_real(x);
    if (eq(y,unbound) || eq(y,Fixnum_1))
      # 1 Argument oder 2. Argument =1
      { R_floor_I_R(x); }
      else
      # 2 Argumente
      { check_real(y);
        R_R_floor_I_R(x,y);
      }
    # Stackaufbau: q, r.
    value1 = STACK_1; value2 = STACK_0; skipSTACK(2); mv_count=2;
  }

LISPFUN(ceiling,1,1,norest,nokey,0,NIL)
# (CEILING real [real]), CLTL S. 215
  { var object y = popSTACK();
    var object x = popSTACK();
    check_real(x);
    if (eq(y,unbound) || eq(y,Fixnum_1))
      # 1 Argument oder 2. Argument =1
      { R_ceiling_I_R(x); }
      else
      # 2 Argumente
      { check_real(y);
        R_R_ceiling_I_R(x,y);
      }
    # Stackaufbau: q, r.
    value1 = STACK_1; value2 = STACK_0; skipSTACK(2); mv_count=2;
  }

LISPFUN(truncate,1,1,norest,nokey,0,NIL)
# (TRUNCATE real [real]), CLTL S. 215
  { var object y = popSTACK();
    var object x = popSTACK();
    check_real(x);
    if (eq(y,unbound) || eq(y,Fixnum_1))
      # 1 Argument oder 2. Argument =1
      { R_truncate_I_R(x); }
      else
      # 2 Argumente
      { check_real(y);
        R_R_truncate_I_R(x,y);
      }
    # Stackaufbau: q, r.
    value1 = STACK_1; value2 = STACK_0; skipSTACK(2); mv_count=2;
  }

LISPFUN(round,1,1,norest,nokey,0,NIL)
# (ROUND real [real]), CLTL S. 215
  { var object y = popSTACK();
    var object x = popSTACK();
    check_real(x);
    if (eq(y,unbound) || eq(y,Fixnum_1))
      # 1 Argument oder 2. Argument =1
      { R_round_I_R(x); }
      else
      # 2 Argumente
      { check_real(y);
        R_R_round_I_R(x,y);
      }
    # Stackaufbau: q, r.
    value1 = STACK_1; value2 = STACK_0; skipSTACK(2); mv_count=2;
  }

LISPFUNN(mod,2)
# (MOD real real), CLTL S. 217
  { var object y = popSTACK();
    var object x = popSTACK();
    check_real(x);
    check_real(y);
    value1 = R_R_mod_R(x,y); mv_count=1;
  }

LISPFUNN(rem,2)
# (REM real real), CLTL S. 217
  { var object y = popSTACK();
    var object x = popSTACK();
    check_real(x);
    check_real(y);
    value1 = R_R_rem_R(x,y); mv_count=1;
  }

LISPFUN(ffloor,1,1,norest,nokey,0,NIL)
# (FFLOOR real [real]), CLTL S. 217
  { var object y = popSTACK();
    var object x = popSTACK();
    check_real(x);
    if (eq(y,unbound) || eq(y,Fixnum_1))
      # 1 Argument oder 2. Argument =1
      { R_ffloor_F_R(x); }
      else
      # 2 Argumente
      { check_real(y);
        R_R_ffloor_F_R(x,y);
      }
    # Stackaufbau: q, r.
    value1 = STACK_1; value2 = STACK_0; skipSTACK(2); mv_count=2;
  }

LISPFUN(fceiling,1,1,norest,nokey,0,NIL)
# (FCEILING real [real]), CLTL S. 217
  { var object y = popSTACK();
    var object x = popSTACK();
    check_real(x);
    if (eq(y,unbound) || eq(y,Fixnum_1))
      # 1 Argument oder 2. Argument =1
      { R_fceiling_F_R(x); }
      else
      # 2 Argumente
      { check_real(y);
        R_R_fceiling_F_R(x,y);
      }
    # Stackaufbau: q, r.
    value1 = STACK_1; value2 = STACK_0; skipSTACK(2); mv_count=2;
  }

LISPFUN(ftruncate,1,1,norest,nokey,0,NIL)
# (FTRUNCATE real [real]), CLTL S. 217
  { var object y = popSTACK();
    var object x = popSTACK();
    check_real(x);
    if (eq(y,unbound) || eq(y,Fixnum_1))
      # 1 Argument oder 2. Argument =1
      { R_ftruncate_F_R(x); }
      else
      # 2 Argumente
      { check_real(y);
        R_R_ftruncate_F_R(x,y);
      }
    # Stackaufbau: q, r.
    value1 = STACK_1; value2 = STACK_0; skipSTACK(2); mv_count=2;
  }

LISPFUN(fround,1,1,norest,nokey,0,NIL)
# (FROUND real [real]), CLTL S. 217
  { var object y = popSTACK();
    var object x = popSTACK();
    check_real(x);
    if (eq(y,unbound) || eq(y,Fixnum_1))
      # 1 Argument oder 2. Argument =1
      { R_fround_F_R(x); }
      else
      # 2 Argumente
      { check_real(y);
        R_R_fround_F_R(x,y);
      }
    # Stackaufbau: q, r.
    value1 = STACK_1; value2 = STACK_0; skipSTACK(2); mv_count=2;
  }

LISPFUNN(decode_float,1)
# (DECODE-FLOAT float), CLTL S. 218
  { var object f = popSTACK();
    check_float(f);
    F_decode_float_F_I_F(f);
    value1 = STACK_2; value2 = STACK_1; value3 = STACK_0; skipSTACK(3);
    mv_count=3;
  }

LISPFUNN(scale_float,2)
# (SCALE-FLOAT float integer), CLTL S. 218
  { var object f = STACK_1;
    var object i = STACK_0;
    check_float(f); check_integer(i); skipSTACK(2);
    value1 = F_I_scale_float_F(f,i); mv_count=1;
  }

LISPFUNN(float_radix,1)
# (FLOAT-RADIX float), CLTL S. 218
  { var object f = popSTACK();
    check_float(f);
    value1 = F_float_radix_I(f); mv_count=1;
  }

LISPFUN(float_sign,1,1,norest,nokey,0,NIL)
# (FLOAT-SIGN float [float]), CLTL S. 218
  { var object arg2 = popSTACK();
    var object arg1 = popSTACK();
    check_float(arg1);
    if (eq(arg2,unbound))
      # 1 Argument
      { value1 = F_float_sign_F(arg1); }
      else
      # 2 Argumente
      { check_float(arg2);
        value1 = F_F_float_sign_F(arg1,arg2);
      }
  }

LISPFUN(float_digits,1,1,norest,nokey,0,NIL)
# (FLOAT-DIGITS number [digits]), CLTL S. 218
  { var object arg2 = popSTACK();
    var object arg1 = popSTACK();
    if (eq(arg2,unbound))
      # 1 Argument: (FLOAT-DIGITS float)
      { check_float(arg1);
        value1 = F_float_digits_I(arg1);
      }
      else
      # 2 Argumente: (FLOAT-DIGITS number digits)
      { if (!posfixnump(arg2)) { fehler_digits(arg2); } # nicht notwendig Fixnum!??
       {var uintL d = posfixnum_to_L(arg2); # = I_to_UL(arg2); ??
        if (d==0) { fehler_digits(arg2); } # sollte >0 sein
        check_real(arg1);
        # arg1 in ein Float mit mindestens d Bits umwandeln:
        if (d > DF_mant_len+1)
          # -> Long-Float
          { d = ceiling(d,intDsize);
            if ((intWCsize<32) && (d > (bitc(intWCsize)-1))) { fehler_LF_toolong(); }
            value1 = R_to_LF(arg1,d);
          }
          else
          # ein Double-Float reicht
          if (d > FF_mant_len+1)
            # -> Double-Float
            { value1 = R_to_DF(arg1); }
            else
            # ein Single-Float reicht
            if (d > SF_mant_len+1)
              # -> Single-Float
              { value1 = R_to_FF(arg1); }
              else
              # ein Short-Float reicht
              { value1 = R_to_SF(arg1); }
      }}
    mv_count=1;
  }

LISPFUNN(float_precision,1)
# (FLOAT-PRECISION float), CLTL S. 218
  { var object f = popSTACK();
    check_float(f);
    value1 = F_float_precision_I(f); mv_count=1;
  }

LISPFUNN(integer_decode_float,1)
# (INTEGER-DECODE-FLOAT float), CLTL S. 218
  { var object f = popSTACK();
    check_float(f);
    F_integer_decode_float_I_I_I(f);
    value1 = STACK_2; value2 = STACK_1; value3 = STACK_0; skipSTACK(3);
    mv_count=3;
  }

LISPFUN(complex,1,1,norest,nokey,0,NIL)
# (COMPLEX real [real]), CLTL S. 220
# Abweichung von CLTL:
# Bei uns ist für reelle x stets (COMPLEX x) = x.
# Grund: Dass (COMPLEX 1) = 1 sein soll, zeigt, dass (COMPLEX x) als (COMPLEX x 0)
# zu interpretieren ist. Bei uns können komplexe Zahlen einen Realteil
# und einen Imaginärteil verschiedenen Typs haben (vgl. CLTL, Seite 19),
# und es ist dann (COMPLEX x 0) = x.
  { var object arg2 = popSTACK();
    var object arg1 = popSTACK();
    check_real(arg1);
    if (eq(arg2,unbound))
      # 1 Argument
      { value1 = arg1; }
      else
      # 2 Argumente
      { check_real(arg2);
        value1 = R_R_complex_N(arg1,arg2);
      }
    mv_count=1;
  }

LISPFUNN(realpart,1)
# (REALPART number), CLTL S. 220
  { var object x = popSTACK();
    check_number(x);
    value1 = N_realpart_R(x); mv_count=1;
  }

LISPFUNN(imagpart,1)
# (IMAGPART number), CLTL S. 220
  { var object x = popSTACK();
    check_number(x);
    value1 = N_imagpart_R(x); mv_count=1;
  }

LISPFUN(logior,0,0,rest,nokey,0,NIL)
# (LOGIOR {integer}), CLTL S. 221
# Methode:
# (logior) = 0
# (logior x1 x2 x3 ... xn) = (logior ...(logior (logior x1 x2) x3)... xn)
  { if (argcount==0) { value1 = Fixnum_0; mv_count=1; return; }
    argcount--;
    test_integer_args(argcount,rest_args_pointer); # Alle Argumente ganze Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # x:=Arg[0], for i:=1 to n do ( x := logior(x,Arg[i]) ), return(x).
    { var object* arg_i_ptr = rest_args_pointer;
      var object x = NEXT(arg_i_ptr); # bisheriges Oder
      dotimesC(argcount,argcount, { x = I_I_logior_I(x,NEXT(arg_i_ptr)); } );
      value1 = x; mv_count=1; set_args_end_pointer(rest_args_pointer);
  } }

LISPFUN(logxor,0,0,rest,nokey,0,NIL)
# (LOGXOR {integer}), CLTL S. 221
# Methode:
# (logxor) = 0
# (logxor x1 x2 x3 ... xn) = (logxor ...(logxor (logxor x1 x2) x3)... xn)
  { if (argcount==0) { value1 = Fixnum_0; mv_count=1; return; }
    argcount--;
    test_integer_args(argcount,rest_args_pointer); # Alle Argumente ganze Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # x:=Arg[0], for i:=1 to n do ( x := logxor(x,Arg[i]) ), return(x).
    { var object* arg_i_ptr = rest_args_pointer;
      var object x = NEXT(arg_i_ptr); # bisheriges Xor
      dotimesC(argcount,argcount, { x = I_I_logxor_I(x,NEXT(arg_i_ptr)); } );
      value1 = x; mv_count=1; set_args_end_pointer(rest_args_pointer);
  } }

LISPFUN(logand,0,0,rest,nokey,0,NIL)
# (LOGAND {integer}), CLTL S. 221
# Methode:
# (logand) = -1
# (logand x1 x2 x3 ... xn) = (logand ...(logand (logand x1 x2) x3)... xn)
  { if (argcount==0) { value1 = Fixnum_minus1; mv_count=1; return; }
    argcount--;
    test_integer_args(argcount,rest_args_pointer); # Alle Argumente ganze Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # x:=Arg[0], for i:=1 to n do ( x := logand(x,Arg[i]) ), return(x).
    { var object* arg_i_ptr = rest_args_pointer;
      var object x = NEXT(arg_i_ptr); # bisheriges And
      dotimesC(argcount,argcount, { x = I_I_logand_I(x,NEXT(arg_i_ptr)); } );
      value1 = x; mv_count=1; set_args_end_pointer(rest_args_pointer);
  } }

LISPFUN(logeqv,0,0,rest,nokey,0,NIL)
# (LOGEQV {integer}), CLTL S. 221
# Methode:
# (logeqv) = -1
# (logeqv x1 x2 x3 ... xn) = (logeqv ...(logeqv (logeqv x1 x2) x3)... xn)
  { if (argcount==0) { value1 = Fixnum_minus1; mv_count=1; return; }
    argcount--;
    test_integer_args(argcount,rest_args_pointer); # Alle Argumente ganze Zahlen?
    # Methode:
    # n+1 Argumente Arg[0..n].
    # x:=Arg[0], for i:=1 to n do ( x := logeqv(x,Arg[i]) ), return(x).
    { var object* arg_i_ptr = rest_args_pointer;
      var object x = NEXT(arg_i_ptr); # bisheriges Zwischen-EQV
      dotimesC(argcount,argcount, { x = I_I_logeqv_I(x,NEXT(arg_i_ptr)); } );
      value1 = x; mv_count=1; set_args_end_pointer(rest_args_pointer);
  } }

LISPFUNN(lognand,2)
# (LOGNAND integer integer), CLTL S. 221
  { var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(2);
    value1 = I_I_lognand_I(x,y); mv_count=1;
  }

LISPFUNN(lognor,2)
# (LOGNOR integer integer), CLTL S. 221
  { var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(2);
    value1 = I_I_lognor_I(x,y); mv_count=1;
  }

LISPFUNN(logandc1,2)
# (LOGANDC1 integer integer), CLTL S. 221
  { var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(2);
    value1 = I_I_logandc1_I(x,y); mv_count=1;
  }

LISPFUNN(logandc2,2)
# (LOGANDC2 integer integer), CLTL S. 221
  { var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(2);
    value1 = I_I_logandc2_I(x,y); mv_count=1;
  }

LISPFUNN(logorc1,2)
# (LOGORC1 integer integer), CLTL S. 221
  { var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(2);
    value1 = I_I_logorc1_I(x,y); mv_count=1;
  }

LISPFUNN(logorc2,2)
# (LOGORC2 integer integer), CLTL S. 221
  { var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(2);
    value1 = I_I_logorc2_I(x,y); mv_count=1;
  }

LISPFUNN(boole,3)
# (BOOLE op integer integer), CLTL S. 222
  { var object op = STACK_2; # Operator, kein Typtest
    var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(3);
    value1 = OP_I_I_boole_I(op,x,y); mv_count=1;
  }

LISPFUNN(lognot,1)
# (LOGNOT integer), CLTL S. 223
  { var object x = popSTACK();
    check_integer(x);
    value1 = I_lognot_I(x); mv_count=1;
  }

LISPFUNN(logtest,2)
# (LOGTEST integer integer), CLTL S. 223
  { var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(2);
    value1 = (I_I_logtest(x,y) ? T : NIL); mv_count=1;
  }

LISPFUNN(logbitp,2)
# (LOGBITP integer integer), CLTL S. 224
  { var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(2);
    value1 = (I_I_logbitp(x,y) ? T : NIL); mv_count=1;
  }

LISPFUNN(ash,2)
# (ASH integer integer), CLTL S. 224
  { var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(2);
    value1 = I_I_ash_I(x,y); mv_count=1;
  }

LISPFUNN(logcount,1)
# (LOGCOUNT integer), CLTL S. 224
  { var object x = popSTACK();
    check_integer(x);
    value1 = I_logcount_I(x); mv_count=1;
  }

LISPFUNN(integer_length,1)
# (INTEGER-LENGTH integer), CLTL S. 224
  { var object x = popSTACK();
    check_integer(x);
    value1 = I_integer_length_I(x); mv_count=1;
  }

LISPFUNN(byte,2)
# (BYTE size position), CLTL S. 225
  { var object s = STACK_1;
    var object p = STACK_0;
    skipSTACK(2);
    value1 = I_I_Byte(s,p); mv_count=1; # Typprüfungen dort. Wieso Fixnums??
  }

LISPFUNN(bytesize,1)
# (BYTE-SIZE bytespec), CLTL S. 226
  { var object b = popSTACK();
    value1 = Byte_size(b); mv_count=1; # Typprüfung dort
  }

LISPFUNN(byteposition,1)
# (BYTE-POSITION bytespec), CLTL S. 226
  { var object b = popSTACK();
    value1 = Byte_position(b); mv_count=1; # Typprüfung dort
  }

LISPFUNN(ldb,2)
# (LDB bytespec integer), CLTL S. 226
  { var object b = STACK_1; # Typprüfung erfolgt später
    var object x = STACK_0;
    check_integer(x); skipSTACK(2);
    value1 = I_Byte_ldb_I(x,b); mv_count=1;
  }

LISPFUNN(ldb_test,2)
# (LDB-TEST bytespec integer), CLTL S. 226
  { var object b = STACK_1; # Typprüfung erfolgt später
    var object x = STACK_0;
    check_integer(x); skipSTACK(2);
    value1 = (I_Byte_ldb_test(x,b) ? T : NIL); mv_count=1;
  }

LISPFUNN(mask_field,2)
# (MASK_FIELD bytespec integer), CLTL S. 226
  { var object b = STACK_1; # Typprüfung erfolgt später
    var object x = STACK_0;
    check_integer(x); skipSTACK(2);
    value1 = I_Byte_mask_field_I(x,b); mv_count=1;
  }

LISPFUNN(dpb,3)
# (DPB integer bytespec integer), CLTL S. 227
  { var object x = STACK_2;
    var object b = STACK_1; # Typprüfung erfolgt später
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(3);
    value1 = I_I_Byte_dpb_I(x,y,b); mv_count=1;
  }

LISPFUNN(deposit_field,3)
# (DEPOSIT-FIELD integer bytespec integer), CLTL S. 227
  { var object x = STACK_2;
    var object b = STACK_1; # Typprüfung erfolgt später
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(3);
    value1 = I_I_Byte_deposit_field_I(x,y,b); mv_count=1;
  }

# Überprüft ein optionales Random-State-Argument obj.
# check_random_state(obj)
# > obj: optionales Random-State-Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: das gemeinte Random-State
  local object check_random_state (object obj);
  local object check_random_state(obj)
    var object obj;
    { if (!eq(obj,unbound))
        # angegeben -> muss Random-State sein:
        { if (random_state_p(obj))
            { return obj; }
            else
            { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
              pushSTACK(S(random_state)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
              pushSTACK(obj);
              pushSTACK(TheSubr(subr_self)->name);
              fehler(type_error,
                     GETTEXT("~: argument should be a random-state, not ~")
                    );
        }   }
        else
        # nicht angegeben -> Default aus *RANDOM-STATE*
        { obj = Symbol_value(S(random_state_stern)); # Wert von *RANDOM-STATE*
          if (random_state_p(obj))
            { return obj; }
            else
            { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
              pushSTACK(S(random_state)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
              pushSTACK(obj);
              pushSTACK(S(random_state_stern));
              pushSTACK(TheSubr(subr_self)->name);
              fehler(type_error,
                     GETTEXT("~: the value of ~ should be a random-state, not ~")
                    );
        }   }
    }

LISPFUN(random,1,1,norest,nokey,0,NIL)
# (RANDOM number [state]), CLTL S. 228
  { var object x = STACK_1;
    var object r = check_random_state(STACK_0);
    skipSTACK(2);
    check_real(x); # x muss eine reelle Zahl sein, >0 und Float oder Integer
    if (R_plusp(x))
      { if (R_floatp(x)) { value1 = F_random_F(r,x); mv_count=1; return; }
        elif (RA_integerp(x)) { value1 = I_random_I(r,x); mv_count=1; return; }
      }
    pushSTACK(x); # Wert für Slot DATUM von TYPE-ERROR
    pushSTACK(O(type_random_arg)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
    pushSTACK(x); pushSTACK(S(random));
    fehler(type_error,
           GETTEXT("~: argument should be positive and an integer or float, not ~")
          );
  }

# make_random_state(r) liefert ein neues Random-State mit Initialzustand
# - zufällig, falls r=T,
# - aus Random-State *RANDOM-STATE*, falls r=NIL oder r=unbound,
# - aus Random-State r selbst, sonst.
# can trigger GC
  local object make_random_state (object r);
  local object make_random_state(r)
    var object r;
    { var uint32 seed_hi;
      var uint32 seed_lo;
      if (eq(r,T))
        # mit Random-Bits vom Betriebssystem initialisieren:
        {
          #if defined(AMIGAOS)
          seed_lo = get_real_time(); # Uhrzeit
          begin_system_call(); seed_hi = FindTask(NULL); end_system_call(); # Pointer auf eigene Task
          #elif defined(MSDOS) || defined(RISCOS)
          # Keine Zufallszahlen, keine PID, nichts Zufälliges da.
          seed_lo = get_real_time(); # Uhrzeit, 100 Hz
          begin_system_call(); seed_hi = time(NULL); end_system_call(); # Uhrzeit, 1 Hz
          #elif defined(UNIX)
          #ifdef TIME_UNIX
          var internal_time real_time; # Uhrzeit
          get_real_time(&real_time);
          seed_lo = highlow32(real_time.tv_sec,real_time.tv_usec); # 16+16 zufällige Bits
          #endif
          #ifdef TIME_UNIX_TIMES
          seed_lo = get_real_time(); # Uhrzeit, CLK_TCK Hz
          #endif
          begin_system_call();
          seed_hi = (rand() # zufällige 31 Bit (bei UNIX_BSD) bzw. 16 Bit (bei UNIX_SYSV)
                           << 8) ^ (uintL)(getpid()); # ca. 8 Bit von der Process ID
          end_system_call();
          #elif defined(WIN32_NATIVE)
          var internal_time real_time; # Uhrzeit
          get_real_time(&real_time);
          seed_lo = real_time.dwHighDateTime ^ real_time.dwLowDateTime;
          begin_system_call();
          seed_hi = CoGetCurrentProcess();
          end_system_call();
          #else
          #error "make_random_state() anpassen!"
          #endif
        }
        else
        { # Random-State überprüfen:
          r = check_random_state( (eq(r,NIL) ? unbound : r) );
          # dessen Zustand herausholen:
         {var object seed = The_Random_state(r)->random_state_seed;
          var uintD* seedMSDptr = (uintD*)(&TheSbvector(seed)->data[0]);
          seed_hi = get_32_Dptr(seedMSDptr);
          seed_lo = get_32_Dptr(&seedMSDptr[32/intDsize]);
        }}
      # neuen Zustands-Bitvektor holen und füllen:
      {var object seed = allocate_bit_vector(64);
       var uintD* seedMSDptr = (uintD*)(&TheSbvector(seed)->data[0]);
       set_32_Dptr(seedMSDptr,seed_hi);
       set_32_Dptr(&seedMSDptr[32/intDsize],seed_lo);
       pushSTACK(seed);
      }
      {var object state = allocate_random_state(); # neuen Random-State
       The_Random_state(state)->random_state_seed = popSTACK(); # mit Bit-Vektor füllen
       return state;
    } }

LISPFUN(make_random_state,0,1,norest,nokey,0,NIL)
# (MAKE-RANDOM-STATE [state]), CLTL S. 230
  { value1 = make_random_state(popSTACK()); mv_count=1; }

LISPFUNN(fakultaet,1)
# (! integer)
  { var object x = popSTACK();
    check_integer(x);
    if (!posfixnump(x))
      { pushSTACK(x); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_posfixnum)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(x); pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~ : argument should be a fixnum >=0, not ~")
              );
      }
    # x ist ein Fixnum >=0.
    value1 = FN_fak_I(x); mv_count=1;
  }

LISPFUNN(exquo,2)
# (EXQUO integer integer) dividiert zwei Integers. Die Division muss aufgehen.
# (EXQUO x y) == (THE INTEGER (/ (THE INTEGER x) (THE INTEGER y)))
  { var object x = STACK_1;
    var object y = STACK_0;
    check_integer(x); check_integer(y); skipSTACK(2);
    value1 = I_I_exquo_I(x,y); mv_count=1;
  }

LISPFUNN(long_float_digits,0)
# (LONG-FLOAT-DIGITS) liefert die Default-Bitzahl von Long-Floats.
  { value1 = UL_to_I(intDsize * I_to_UL(O(LF_digits))); mv_count=1; }

# Setzt die Default-Long-Float-Länge auf den Wert len (>= LF_minlen).
# set_lf_digits(len);
# can trigger GC
  local void set_lf_digits (uintC len);
  local void set_lf_digits(len)
    var uintC len;
    { O(LF_digits) = UL_to_I(len);
      # MOST-POSITIVE-LONG-FLOAT und MOST-NEGATIVE-LONG-FLOAT :
      { # Exponent so groß wie möglich, Mantisse 1...1
        var object x = allocate_lfloat(len,LF_exp_high,0);
        fill_loop_up(&TheLfloat(x)->data[0],len,~(uintD)0);
        define_variable(S(most_positive_long_float),x);
        x = LF_minus_LF(x);
        define_variable(S(most_negative_long_float),x);
      }
      # LEAST-POSITIVE-LONG-FLOAT und LEAST-NEGATIVE-LONG-FLOAT :
      { # Exponent so klein wie möglich, Mantisse 10...0
        var object x = allocate_lfloat(len,LF_exp_low,0);
        var uintD* ptr = &TheLfloat(x)->data[0];
        *ptr++ = bit(intDsize-1);
        clear_loop_up(ptr,len-1);
        define_variable(S(least_positive_long_float),x);
        define_variable(S(least_positive_normalized_long_float),x); # X3J13 vote <79>
        x = LF_minus_LF(x);
        define_variable(S(least_negative_long_float),x);
        define_variable(S(least_negative_normalized_long_float),x); # X3J13 vote <79>
      }
      # LONG-FLOAT-EPSILON = 2^-16n*(1+2^(1-16n)) :
      { # Exponent 1-16n, Mantisse 10...01
        var object x = allocate_lfloat(len,LF_exp_mid+1-intDsize*(uintL)len,0);
        var uintD* ptr = &TheLfloat(x)->data[0];
        *ptr++ = bit(intDsize-1);
        ptr = clear_loop_up(ptr,len-2);
        *ptr = bit(0);
        define_variable(S(long_float_epsilon),x);
      }
      # LONG-FLOAT-NEGATIVE-EPSILON = 2^(-16n-1)*(1+2^(1-16n)) :
      { # Exponent -16n, Mantisse 10...01
        var object x = allocate_lfloat(len,LF_exp_mid-intDsize*(uintL)len,0);
        var uintD* ptr = &TheLfloat(x)->data[0];
        *ptr++ = bit(intDsize-1);
        ptr = clear_loop_up(ptr,len-2);
        *ptr = bit(0);
        define_variable(S(long_float_negative_epsilon),x);
      # PI :
        x = O(pi) = pi_F_float_F(x);
        define_variable(S(pi),x);
      }
    }

LISPFUNN(set_long_float_digits,1)
# (SETF (LONG-FLOAT-DIGITS) digits) = (SYS::%SET-LONG-FLOAT-DIGITS digits)
  { var object arg = STACK_0;
    if (!posfixnump(arg)) { fehler_digits(arg); } # nicht notwendig Fixnum!??
   {var uintL d = posfixnum_to_L(arg); # = I_to_UL(arg); ??
    if (d==0) { fehler_digits(arg); } # sollte >0 sein
    d = ceiling(d,intDsize);
    if ((intWCsize<32) && (d > (bitc(intWCsize)-1))) { fehler_LF_toolong(); }
    if (d < LF_minlen) { d = LF_minlen; } # d>=LF_minlen erzwingen
    set_lf_digits(d);
    value1 = popSTACK(); mv_count=1; # digits als Wert
  }}

# UP für LOG2 und LOG10: Logarithmus des Fixnums x mit mindestens digits
# Bits berechnen und - wenn nötig - den Wert in *objptr aktualisieren.
  local object log_digits (object x, object digits, object* objptr);
  local object log_digits(x,digits,objptr)
    var object x;
    var object digits;
    var object* objptr;
    { # digits-Argument überprüfen:
      if (!posfixnump(digits)) { fehler_digits(digits); } # nicht notwendig Fixnum!??
     {var uintL d = posfixnum_to_L(digits); # = I_to_UL(digits); ??
      if (d==0) { fehler_digits(digits); } # sollte >0 sein
      # bisher bekannten Wert holen:
      { var object ln_x = *objptr;
        # ln_x in ein Float mit mindestens d Bits umwandeln:
        if (d > DF_mant_len+1)
          # -> Long-Float
          { d = ceiling(d,intDsize);
            if ((intWCsize<32) && (d > (bitc(intWCsize)-1))) { fehler_LF_toolong(); }
           {var uintC oldlen = Lfloat_length(ln_x); # vorhandene Länge
            if (d < oldlen) { return LF_shorten_LF(ln_x,d); }
            if (d == oldlen) { return ln_x; }
            # gewünschte > vorhandene Länge -> muss nachberechnen:
            # Lfloat_length(ln_x) um mindestens einen konstanten Faktor
            # > 1 wachsen lassen, damit es nicht zu häufig nachberechnet wird:
            oldlen += floor(oldlen,2); # oldlen * 3/2
            {var uintC newlen = (d < oldlen ? oldlen : d);
             ln_x = *objptr = R_ln_R(I_to_LF(x,newlen)); # (ln x) als LF mit newlen Digits berechnen
             return (d < newlen ? LF_shorten_LF(ln_x,d) : ln_x);
          }}}
          else
          # ein Double-Float reicht
          if (d > FF_mant_len+1)
            # -> Double-Float
            { return LF_to_DF(ln_x); }
            else
            # ein Single-Float reicht
            if (d > SF_mant_len+1)
              # -> Single-Float
              { return LF_to_FF(ln_x); }
              else
              # ein Short-Float reicht
              { return LF_to_SF(ln_x); }
    }}}

LISPFUNN(log2,1)
# (SYS::LOG2 digits) liefert ln(2) mit mindestens digits Bits.
  { value1 = log_digits(fixnum(2),popSTACK(),&O(LF_ln2));
    mv_count=1;
  }

LISPFUNN(log10,1)
# (SYS::LOG10 digits) liefert ln(10) mit mindestens digits Bits.
  { value1 = log_digits(fixnum(10),popSTACK(),&O(LF_ln10));
    mv_count=1;
  }


# =========================================================================== #
#                             Initialisierung

# Mantisse von pi :
  local const uintD pi_mantisse [2048/intDsize] =
    { D(0xC9,0x0F,0xDA,0xA2,) D(0x21,0x68,0xC2,0x34,) D(0xC4,0xC6,0x62,0x8B,)
      D(0x80,0xDC,0x1C,0xD1,) D(0x29,0x02,0x4E,0x08,) D(0x8A,0x67,0xCC,0x74,)
      D(0x02,0x0B,0xBE,0xA6,) D(0x3B,0x13,0x9B,0x22,) D(0x51,0x4A,0x08,0x79,)
      D(0x8E,0x34,0x04,0xDD,) D(0xEF,0x95,0x19,0xB3,) D(0xCD,0x3A,0x43,0x1B,)
      D(0x30,0x2B,0x0A,0x6D,) D(0xF2,0x5F,0x14,0x37,) D(0x4F,0xE1,0x35,0x6D,)
      D(0x6D,0x51,0xC2,0x45,) D(0xE4,0x85,0xB5,0x76,) D(0x62,0x5E,0x7E,0xC6,)
      D(0xF4,0x4C,0x42,0xE9,) D(0xA6,0x37,0xED,0x6B,) D(0x0B,0xFF,0x5C,0xB6,)
      D(0xF4,0x06,0xB7,0xED,) D(0xEE,0x38,0x6B,0xFB,) D(0x5A,0x89,0x9F,0xA5,)
      D(0xAE,0x9F,0x24,0x11,) D(0x7C,0x4B,0x1F,0xE6,) D(0x49,0x28,0x66,0x51,)
      D(0xEC,0xE4,0x5B,0x3D,) D(0xC2,0x00,0x7C,0xB8,) D(0xA1,0x63,0xBF,0x05,)
      D(0x98,0xDA,0x48,0x36,) D(0x1C,0x55,0xD3,0x9A,) D(0x69,0x16,0x3F,0xA8,)
      D(0xFD,0x24,0xCF,0x5F,) D(0x83,0x65,0x5D,0x23,) D(0xDC,0xA3,0xAD,0x96,)
      D(0x1C,0x62,0xF3,0x56,) D(0x20,0x85,0x52,0xBB,) D(0x9E,0xD5,0x29,0x07,)
      D(0x70,0x96,0x96,0x6D,) D(0x67,0x0C,0x35,0x4E,) D(0x4A,0xBC,0x98,0x04,)
      D(0xF1,0x74,0x6C,0x08,) D(0xCA,0x18,0x21,0x7C,) D(0x32,0x90,0x5E,0x46,)
      D(0x2E,0x36,0xCE,0x3B,) D(0xE3,0x9E,0x77,0x2C,) D(0x18,0x0E,0x86,0x03,)
      D(0x9B,0x27,0x83,0xA2,) D(0xEC,0x07,0xA2,0x8F,) D(0xB5,0xC5,0x5D,0xF0,)
      D(0x6F,0x4C,0x52,0xC9,) D(0xDE,0x2B,0xCB,0xF6,) D(0x95,0x58,0x17,0x18,)
      D(0x39,0x95,0x49,0x7C,) D(0xEA,0x95,0x6A,0xE5,) D(0x15,0xD2,0x26,0x18,)
      D(0x98,0xFA,0x05,0x10,) D(0x15,0x72,0x8E,0x5A,) D(0x8A,0xAA,0xC4,0x2D,)
      D(0xAD,0x33,0x17,0x0D,) D(0x04,0x50,0x7A,0x33,) D(0xA8,0x55,0x21,0xAB,)
      D(0xDF,0x1C,0xBA,0x65,) } ;

# Mantisse von ln(2) :
  local const uintD ln2_mantisse [64/intDsize] =
    { D(0xB1,0x72,0x17,0xF7,) D(0xD1,0xCF,0x79,0xAC,) } ;

# Mantisse von ln(10) :
  local const uintD ln10_mantisse [64/intDsize] =
    { D(0x93,0x5D,0x8D,0xDD,) D(0xAA,0xA8,0xAC,0x17,) } ;

# UP: Initialisiert die Arithmetik.
# init_arith();
# can trigger GC
  global void init_arith (void);
  global void init_arith()
    { # verschiedene konstante Zahlen:
      #ifndef WIDE
      O(FF_zero) = allocate_ffloat(0); # 0.0F0
      # encode_FF(0,1,bit(FF_mant_len), O(FF_one)=); # 1.0F0
      # encode_FF(-1,1,bit(FF_mant_len), O(FF_minusone)=); # -1.0F0
      #endif
      #ifdef intQsize
      O(DF_zero) = allocate_dfloat(0); # 0.0D0
      # encode_DF(0,1,bit(DF_mant_len), O(DF_one)=); # 1.0D0
      # encode_DF(-1,1,bit(DF_mant_len), O(DF_minusone)=); # -1.0D0
      #else
      O(DF_zero) = allocate_dfloat(0,0); # 0.0D0
      # encode2_DF(0,1,bit(DF_mant_len-32),0, O(DF_one)=); # 1.0D0
      # encode2_DF(-1,1,bit(DF_mant_len-32),0, O(DF_minusone)=); # -1.0D0
      #endif
      # variable Long-Floats:
      encode_LF(0,2,&pi_mantisse[0],2048/intDsize, O(LF_pi)=); # pi auf 2048 Bits
      encode_LF(0,0,&ln2_mantisse[0],64/intDsize, O(LF_ln2)=); # ln(2) auf 64 Bits
      encode_LF(0,2,&ln10_mantisse[0],64/intDsize, O(LF_ln10)=); # ln(10) auf 64 Bits
      # Defaultlänge von Long-Floats so klein wie möglich:
      set_lf_digits(LF_minlen);
      # pi als Short-, Single-, Double-Float:
      O(SF_pi) = LF_to_SF(O(pi));
      O(FF_pi) = LF_to_FF(O(pi));
      O(DF_pi) = LF_to_DF(O(pi));
      # MOST-POSITIVE-FIXNUM, MOST-NEGATIVE-FIXNUM :
      define_constant(S(most_positive_fixnum),Fixnum_mpos);
      define_constant(S(most_negative_fixnum),Fixnum_mneg);
      # MOST/LEAST-POSITIVE/NEGATIVE-SHORT-FLOAT:
      define_constant(S(most_positive_short_float),make_SF(0,SF_exp_high,bit(SF_mant_len+1)-1));
      define_constant(S(least_positive_short_float),make_SF(0,SF_exp_low,bit(SF_mant_len)));
      define_constant(S(least_negative_short_float),make_SF(-1,SF_exp_low,bit(SF_mant_len)));
      define_constant(S(most_negative_short_float),make_SF(-1,SF_exp_high,bit(SF_mant_len+1)-1));
      # MOST/LEAST-POSITIVE/NEGATIVE-SINGLE-FLOAT:
      {var object obj; encode_FF(0,FF_exp_high-FF_exp_mid,bit(FF_mant_len+1)-1, obj=);
       define_constant(S(most_positive_single_float),obj); }
      {var object obj; encode_FF(0,FF_exp_low-FF_exp_mid,bit(FF_mant_len), obj=);
       define_constant(S(least_positive_single_float),obj); }
      {var object obj; encode_FF(-1,FF_exp_low-FF_exp_mid,bit(FF_mant_len), obj=);
       define_constant(S(least_negative_single_float),obj); }
      {var object obj; encode_FF(-1,FF_exp_high-FF_exp_mid,bit(FF_mant_len+1)-1, obj=);
       define_constant(S(most_negative_single_float),obj); }
      # MOST/LEAST-POSITIVE/NEGATIVE-DOUBLE-FLOAT:
      {var object obj;
       #ifdef intQsize
       encode_DF(0,DF_exp_high-DF_exp_mid,bit(DF_mant_len+1)-1, obj=);
       #else
       encode2_DF(0,DF_exp_high-DF_exp_mid,bit(DF_mant_len-32+1)-1,bitm(32)-1, obj=);
       #endif
       define_constant(S(most_positive_double_float),obj); }
      {var object obj;
       #ifdef intQsize
       encode_DF(0,DF_exp_low-DF_exp_mid,bit(DF_mant_len), obj=);
       #else
       encode2_DF(0,DF_exp_low-DF_exp_mid,bit(DF_mant_len-32),0, obj=);
       #endif
       define_constant(S(least_positive_double_float),obj); }
      {var object obj;
       #ifdef intQsize
       encode_DF(-1,DF_exp_low-DF_exp_mid,bit(DF_mant_len), obj=);
       #else
       encode2_DF(-1,DF_exp_low-DF_exp_mid,bit(DF_mant_len-32),0, obj=);
       #endif
       define_constant(S(least_negative_double_float),obj); }
      {var object obj;
       #ifdef intQsize
       encode_DF(-1,DF_exp_high-DF_exp_mid,bit(DF_mant_len+1)-1, obj=);
       #else
       encode2_DF(-1,DF_exp_high-DF_exp_mid,bit(DF_mant_len-32+1)-1,bitm(32)-1, obj=);
       #endif
       define_constant(S(most_negative_double_float),obj); }
      # Bei Floats mit d Bits (incl. Hiddem Bit, also d = ?F_mant_len+1)
      # ist ...-FLOAT-EPSILON = 2^-d*(1+2^(1-d))
      # und ...-FLOAT-NEGATIVE-EPSILON = 2^(-d-1)*(1+2^(1-d)) .
      define_constant(S(short_float_epsilon),make_SF(0,SF_exp_mid-SF_mant_len,bit(SF_mant_len)+1));
      define_constant(S(short_float_negative_epsilon),make_SF(0,SF_exp_mid-SF_mant_len-1,bit(SF_mant_len)+1));
      {var object obj; encode_FF(0,-FF_mant_len,bit(FF_mant_len)+1, obj=);
       define_constant(S(single_float_epsilon),obj); }
      {var object obj; encode_FF(0,-FF_mant_len-1,bit(FF_mant_len)+1, obj=);
       define_constant(S(single_float_negative_epsilon),obj); }
      {var object obj;
       #ifdef intQsize
       encode_DF(0,-DF_mant_len,bit(DF_mant_len)+1, obj=);
       #else
       encode2_DF(0,-DF_mant_len,bit(DF_mant_len-32),1, obj=);
       #endif
       define_constant(S(double_float_epsilon),obj); }
      {var object obj;
       #ifdef intQsize
       encode_DF(0,-DF_mant_len-1,bit(DF_mant_len)+1, obj=);
       #else
       encode2_DF(0,-DF_mant_len-1,bit(DF_mant_len-32),1, obj=);
       #endif
       define_constant(S(double_float_negative_epsilon),obj); }
      # weitere Variablen:
      define_variable(S(default_float_format),S(single_float)); # *DEFAULT-FLOAT-FORMAT* := 'SINGLE-FLOAT
      define_variable(S(read_default_float_format),S(single_float)); # *READ-DEFAULT-FLOAT-FORMAT* := 'SINGLE-FLOAT
      {var object obj = make_random_state(T); # neuer zufälliger Random-State
       define_variable(S(random_state_stern),obj); } # =: *RANDOM-STATE*
      # SYS::*INHIBIT-FLOATING-POINT-UNDERFLOW* := NIL
        define_variable(S(inhibit_floating_point_underflow),NIL);
      # *WARN-ON-FLOATING-POINT-CONTAGION* := T
        define_variable(S(warn_on_floating_point_contagion),T);
      # *FLOATING-POINT-CONTAGION-ANSI* := NIL
        define_variable(S(floating_point_contagion_ansi),NIL);
    }

