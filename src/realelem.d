# Elementare Funktionen für reelle Zahlen

# R_zerop(x) stellt fest, ob (= x 0), wo x eine reelle Zahl ist.
  local boolean R_zerop (object x);
  local boolean R_zerop(x)
    var object x;
    { if (R_rationalp(x))
        # bei rationalen Zahlen: Test auf 0
        { if (eq(x,Fixnum_0)) { goto yes; } else { goto no; } }
        # bei Floats: Fallunterscheidung
        { floatcase(x,
                    { if (SF_zerop(x)) { goto yes; } else { goto no; } },
                    { if (FF_zerop(x)) { goto yes; } else { goto no; } },
                    { if (DF_zerop(x)) { goto yes; } else { goto no; } },
                    { if (LF_zerop(x)) { goto yes; } else { goto no; } }
                   );
        }
      yes: return TRUE;
      no: return FALSE;
    }

# R_plusp(x) stellt fest, ob (> x 0), wo x eine reelle Zahl ist.
  local boolean R_plusp (object x);
  local boolean R_plusp(x)
    var object x;
    { if (R_minusp(x)) { return FALSE; } # x<0 -> nein
      elif (R_zerop(x)) { return FALSE; } # x=0 -> nein
      else { return TRUE; } # sonst ist x>0.
    }

# R_minusp(x) stellt fest, ob (< x 0), wo x eine reelle Zahl ist.
# (Macro in LISPBIBL.D)

# I_F_float_F(x,y) wandelt ein Integer x in das Float-Format des Floats y um
# und rundet dabei nötigenfalls.
# > x: ein Integer
# > y: ein Float
# < ergebnis: (float x y)
# can trigger GC
  local object I_F_float_F (object x, object y);
  local object I_F_float_F(x,y)
    var object x;
    var object y;
    { floatcase(y,
                { return I_to_SF(x); },
                { return I_to_FF(x); },
                { return I_to_DF(x); },
                { return I_to_LF(x,Lfloat_length(y)); }
               );
    }

# RA_F_float_F(x,y) wandelt eine rationale Zahl x in das Float-Format des
# Floats y um und rundet dabei nötigenfalls.
# > x: eine rationale Zahl
# > y: ein Float
# < ergebnis: (float x y)
# can trigger GC
  local object RA_F_float_F (object x, object y);
  local object RA_F_float_F(x,y)
    var object x;
    var object y;
    { floatcase(y,
                { return RA_to_SF(x); },
                { return RA_to_FF(x); },
                { return RA_to_DF(x); },
                { return RA_to_LF(x,Lfloat_length(y)); }
               );
    }

# R_F_float_F(x,y) wandelt eine reelle Zahl x in das Float-Format des Floats
# y um und rundet dabei nötigenfalls.
# > x: eine reelle Zahl
# > y: ein Float
# < ergebnis: (float x y)
# can trigger GC
  local object R_F_float_F (object x, object y);
  local object R_F_float_F(x,y)
    var object x;
    var object y;
    { return (R_rationalp(x) ? RA_F_float_F(x,y) : F_F_float_F(x,y)); }

# R_to_SF(x) wandelt eine reelle Zahl x in ein Short-Float um.
# < ergebnis: (coerce x 'short-float)
# can trigger GC
  local object R_to_SF (object x);
  local object R_to_SF(x)
    var object x;
    { return (R_rationalp(x) ? RA_to_SF(x) : F_to_SF(x)); }

# R_to_FF(x) wandelt eine reelle Zahl x in ein Single-Float um.
# < ergebnis: (coerce x 'single-float)
# can trigger GC
  local object R_to_FF (object x);
  local object R_to_FF(x)
    var object x;
    { return (R_rationalp(x) ? RA_to_FF(x) : F_to_FF(x)); }

# R_to_DF(x) wandelt eine reelle Zahl x in ein Double-Float um.
# < ergebnis: (coerce x 'double-float)
# can trigger GC
  local object R_to_DF (object x);
  local object R_to_DF(x)
    var object x;
    { return (R_rationalp(x) ? RA_to_DF(x) : F_to_DF(x)); }

# R_to_LF(x,len) wandelt eine reelle Zahl x in ein Long-Float mit len Digits um.
# > uintC len: gewünschte Anzahl Digits, >=LF_minlen
# < ergebnis: (coerce x `(long-float ,len))
# can trigger GC
  local object R_to_LF (object x, uintC len);
  local object R_to_LF(x,len)
    var object x;
    var uintC len;
    { return (R_rationalp(x) ? RA_to_LF(x,len) : F_to_LF(x,len)); }

# R_R_contagion_R(x,y) liefert eine reelle Zahl, die so ungenau ist wie die
# ungenauere der beiden reellen Zahlen x und y.
  local object R_R_contagion_R (object x, object y);
  local object R_R_contagion_R(x,y)
    var object x;
    var object y;
    {
      #define X  { return x; }
      #define Y  { return y; }
      #define WX  goto warn_x;
      #define WY  goto warn_y;
      if (R_rationalp(x)) Y
      elif (R_rationalp(y)) X
      else
        floatcase(x,
        /* x SF */ floatcase(y, X,WX,WX,WX),
        /* x FF */ floatcase(y, WY,X,WX,WX),
        /* x DF */ floatcase(y, WY,WY,X,WX),
        /* x LF */ floatcase(y, WY,WY,WY,
                   /* y LF */ { if (Lfloat_length(x) == Lfloat_length(y)) X
                                elif (Lfloat_length(x) <= Lfloat_length(y)) WX
                                else                                        WY
                              })
                 );
      warn_x:
        if (!nullp(Symbol_value(S(warn_on_floating_point_contagion))))
          { pushSTACK(x); warn_floating_point_contagion(); x = popSTACK(); }
        return nullp(Symbol_value(S(floating_point_contagion_ansi))) ? x : y;
      warn_y:
        if (!nullp(Symbol_value(S(warn_on_floating_point_contagion))))
          { pushSTACK(y); warn_floating_point_contagion(); y = popSTACK(); }
        return nullp(Symbol_value(S(floating_point_contagion_ansi))) ? y : x;
      #undef WY
      #undef WX
      #undef Y
      #undef X
    }

# Macro: verteilt je nach Default-Float-Typ auf 4 Statements.
# defaultfloatcase(symbol, SF_statement,FF_statement,DF_statement,LF_statement, save_statement,restore_statement);
# symbol sollte ein S(..)-Symbol sein. Dessen Wert sollte SHORT-FLOAT oder
# SINGLE-FLOAT oder DOUBLE-FLOAT oder LONG-FLOAT sein. Sollte es das nicht
# sein, wird der Wert auf SINGLE-FLOAT gesetzt und eine Warnung ausgegeben.
# kann GC auslösen, aber nur zwischen save_statement und restore_statement.
  #define defaultfloatcase(symbol, SF_statement,FF_statement,DF_statement,LF_statement, save_statement,restore_statement) \
    {var object def = Symbol_value(symbol); # Wert holen            \
     if (eq(def,S(short_float))) { SF_statement }                   \
     elif (eq(def,S(single_float))) { FF_statement }                \
     elif (eq(def,S(double_float))) { DF_statement }                \
     elif (eq(def,S(long_float))) { LF_statement }                  \
     else                                                           \
       { Symbol_value(symbol) = S(single_float); # Wert korrigieren \
         save_statement                                             \
         # Warnung ausgeben:                                        \
         # (WARN "In ~S wurde ein illegaler Wert vorgefunden,       \
         #        ~S wird auf ~S zurückgesetzt."                    \
         #       symbol symbol (symbol-value symbol)                \
         # )                                                        \
         pushSTACK(NIL);                                            \
         pushSTACK(symbol);                                         \
         pushSTACK(symbol);                                         \
         pushSTACK(Symbol_value(symbol));                           \
         STACK_3 = OLS(default_float_format_warnung_string);        \
         funcall(S(warn),4);                                        \
         restore_statement                                          \
         { FF_statement }                                           \
    }  }

# I_float_F(x) wandelt ein Integer x in ein Float um und rundet dabei.
# > x: ein Integer
# < ergebnis: (float x)
# can trigger GC
  local object I_float_F (object x);
  local object I_float_F(x)
    var object x;
    { defaultfloatcase(S(default_float_format),
                       return I_to_SF(x); ,
                       return I_to_FF(x); ,
                       return I_to_DF(x); ,
                       return I_to_LF(x,I_to_UL(O(LF_digits))); ,
                       pushSTACK(x); , x = popSTACK();
                      );
    }

# RA_float_F(x) wandelt eine rationale Zahl x in ein Float um und rundet dabei.
# > x: eine rationale Zahl
# < ergebnis: (float x)
# can trigger GC
  local object RA_float_F (object x);
  local object RA_float_F(x)
    var object x;
    { defaultfloatcase(S(default_float_format),
                       return RA_to_SF(x); ,
                       return RA_to_FF(x); ,
                       return RA_to_DF(x); ,
                       return RA_to_LF(x,I_to_UL(O(LF_digits))); ,
                       pushSTACK(x); , x = popSTACK();
                      );
    }

# R_float_F(x) wandelt eine reelle Zahl x in ein Float um
# und rundet dabei nötigenfalls.
# > x: eine reelle Zahl
# < ergebnis: (float x)
# can trigger GC
  local object R_float_F (object x);
  local object R_float_F(x)
    var object x;
    { return (R_rationalp(x) ? RA_float_F(x) : x); }

# Generiert eine Funktion wie R_floor_I_R
  #define GEN_R_round(rounding)  \
    # Liefert ganzzahligen und gebrochenen Anteil einer reellen Zahl. \
    # (q,r) := (rounding x)                                           \
    # R_rounding_I_R(x);                                              \
    # > x: reelle Zahl                                                \
    # < STACK_1: Quotient q, ein Integer                              \
    # < STACK_0: Rest r, eine reelle Zahl                             \
    # Erniedrigt STACK um 2                                           \
    # kann GC auslösen                                                \
    # Methode:                                          \
    # x rational -> RA_rounding_I_RA(x)                 \
    # x Float -> F_rounding_I_F(x)                      \
    local void CONCAT3(R_,rounding,_I_R) (var object x) \
      { if (R_rationalp(x))                             \
          { CONCAT3(RA_,rounding,_I_RA) (x); }          \
          else                                          \
          { CONCAT3(F_,rounding,_I_F) (x); }            \
      }

# R_floor_I_R(x) liefert (floor x), wo x eine reelle Zahl ist.
# Beide Werte in den Stack.
# can trigger GC
  local void R_floor_I_R (object x);
  GEN_R_round(floor)

# R_ceiling_I_R(x) liefert (ceiling x), wo x eine reelle Zahl ist.
# Beide Werte in den Stack.
# can trigger GC
  local void R_ceiling_I_R (object x);
  GEN_R_round(ceiling)

# R_truncate_I_R(x) liefert (truncate x), wo x eine reelle Zahl ist.
# Beide Werte in den Stack.
# can trigger GC
  local void R_truncate_I_R (object x);
  GEN_R_round(truncate)

# R_round_I_R(x) liefert (round x), wo x eine reelle Zahl ist.
# Beide Werte in den Stack.
# can trigger GC
  local void R_round_I_R (object x);
  GEN_R_round(round)

# Generiert eine Funktion wie R_ffloor_F_R
  #define GEN_R_fround(rounding)  \
    # Liefert ganzzahligen und gebrochenen Anteil einer reellen Zahl. \
    # (q,r) := (frounding x)                                          \
    # R_frounding_F_R(x);                                             \
    # > x: reelle Zahl                                                \
    # < STACK_1: Quotient q, ein integer-wertiges Float               \
    # < STACK_0: Rest r, eine reelle Zahl                             \
    # Erniedrigt STACK um 2                                           \
    # kann GC auslösen                                                \
    # Methode:                                                          \
    # x rational -> RA_rounding_I_RA(x), Quotienten in Float umwandeln. \
    # x Float -> F_frounding_F_F(x).                                    \
    local void CONCAT3(R_f,rounding,_F_R) (var object x)               \
      { if (R_rationalp(x))                                            \
          { CONCAT3(RA_,rounding,_I_RA) (x); # Rational-Routine        \
            STACK_1 = I_float_F(STACK_1); # 1. Wert in Float umwandeln \
          }                                                            \
          else                                                         \
          { CONCAT3(F_f,rounding,_F_F) (x); } # Float-Routine          \
      }

# R_ffloor_F_R(x) liefert (ffloor x), wo x eine reelle Zahl ist.
# Beide Werte in den Stack.
# can trigger GC
  local void R_ffloor_F_R (object x);
  GEN_R_fround(floor)

# R_fceiling_F_R(x) liefert (fceiling x), wo x eine reelle Zahl ist.
# Beide Werte in den Stack.
# can trigger GC
  local void R_fceiling_F_R (object x);
  GEN_R_fround(ceiling)

# R_ftruncate_F_R(x) liefert (ftruncate x), wo x eine reelle Zahl ist.
# Beide Werte in den Stack.
# can trigger GC
  local void R_ftruncate_F_R (object x);
  GEN_R_fround(truncate)

# R_fround_F_R(x) liefert (fround x), wo x eine reelle Zahl ist.
# Beide Werte in den Stack.
# can trigger GC
  local void R_fround_F_R (object x);
  GEN_R_fround(round)

# Generiert eine Funktion wie R_R_plus_R
  #define GEN_R_op21(arg1,arg2,op,ergebnis_zuweisung)  \
    { if (R_rationalp(arg1))                                                      \
        { if (R_rationalp(arg2))                                                  \
            # beides rationale Zahlen                                             \
            { ergebnis_zuweisung CONCAT3(RA_RA_,op,_RA) (arg1,arg2); }            \
            else                                                                  \
            # arg1 rational, arg2 Float -> arg1 in Float umwandeln                \
            { pushSTACK(arg2); arg1 = RA_F_float_F(arg1,arg2); arg2 = popSTACK(); \
              ergebnis_zuweisung CONCAT3(F_F_,op,_F) (arg1,arg2);                 \
            }                                                                     \
        }                                                                         \
        else                                                                      \
        { if (R_rationalp(arg2))                                                  \
            # arg1 Float, arg2 rational -> arg2 in Float umwandeln                \
            { pushSTACK(arg1); arg2 = RA_F_float_F(arg2,arg1); arg1 = popSTACK(); \
              ergebnis_zuweisung CONCAT3(F_F_,op,_F) (arg1,arg2);                 \
            }                                                                     \
            else                                                                  \
            # beides Floats                                                       \
            { ergebnis_zuweisung CONCAT3(F_F_,op,_F) (arg1,arg2); }               \
        }                                                                         \
    }

# R_minus_R(x) liefert (- x), wo x eine reelle Zahl ist.
# can trigger GC
  local object R_minus_R (object x);
  local object R_minus_R(x)
    var object x;
    { return (R_rationalp(x) ? RA_minus_RA(x) : F_minus_F(x)); }

# R_abs_R(x) liefert (abs x), wo x eine reelle Zahl ist.
# can trigger GC
  local object R_abs_R (object x);
  local object R_abs_R(x)
    var object x;
    { return (R_minusp(x) ? R_minus_R(x) : x); } # x<0 -> (- x), x>=0 -> x

# R_R_plus_R(x,y) liefert (+ x y), wo x und y reelle Zahlen sind.
# can trigger GC
  local object R_R_plus_R (object x, object y);
  local object R_R_plus_R(x,y)
    var object x;
    var object y;
    { if (eq(y,Fixnum_0)) { return x; }
      elif (eq(x,Fixnum_0)) { return y; }
      else
        GEN_R_op21(x,y,plus,return)
    }

# R_R_minus_R(x,y) liefert (- x y), wo x und y reelle Zahlen sind.
# can trigger GC
  local object R_R_minus_R (object x, object y);
  local object R_R_minus_R(x,y)
    var object x;
    var object y;
    { if (eq(y,Fixnum_0)) { return x; }
      elif (eq(x,Fixnum_0)) { return R_minus_R(y); }
      else
        GEN_R_op21(x,y,minus,return)
    }

# R_square_R(x) liefert (* x x), wo x eine reelle Zahl ist.
# can trigger GC
  local object R_square_R (object x);
  local object R_square_R(x)
    var object x;
    { return (R_rationalp(x) ? RA_square_RA(x) : F_square_F(x)); }

# R_R_mal_R(x,y) liefert (* x y), wo x und y reelle Zahlen sind.
# can trigger GC
  local object R_R_mal_R (object x, object y);
  local object R_R_mal_R(x,y)
    var object x;
    var object y;
    { if (eq(x,Fixnum_0)) { return x; } # 0 * y = exakte 0
      elif (eq(y,Fixnum_0)) { return y; } # x * 0 = exakte 0
      else
        GEN_R_op21(x,y,mal,return)
    }

# R_durch_R(x) liefert (/ x), wo x eine reelle Zahl ist.
# can trigger GC
  local object R_durch_R (object x);
  local object R_durch_R(x)
    var object x;
    { return (R_rationalp(x) ? RA_durch_RA(x) : F_durch_F(x)); }

# R_R_durch_R(x,y) liefert (/ x y), wo x und y reelle Zahlen sind.
# can trigger GC
  local object R_R_durch_R (object x, object y);
  local object R_R_durch_R(x,y)
    var object x;
    var object y;
    { if (eq(x,Fixnum_0))
        # 0 / y = exakte 0, außer wenn y=0
        { if (R_zerop(y)) { divide_0(); } else { return x; } }
      else
        GEN_R_op21(x,y,durch,return)
    }

# Generiert eine Funktion wie R_R_floor_I_R
  #define GEN_R_R_round(rounding)  \
    # Liefert ganzzahligen Quotienten und Rest \
    # einer Division reeller Zahlen.           \
    # (q,r) := (rounding x y)                  \
    # R_R_rounding_I_R(x,y);                   \
    # > x,y: reelle Zahlen                     \
    # < STACK_1: Quotient q, ein Integer       \
    # < STACK_0: Rest r, eine reelle Zahl      \
    # Erniedrigt STACK um 2                    \
    # kann GC auslösen                         \
    # Methode:                                                      \
    # Beides Integers -> I_I_rounding_I_I(x,y).                     \
    # Sonst: R_rounding_I_R(x/y) -> (q,r). Liefere q und x-y*q=y*r. \
    local void CONCAT3(R_R_,rounding,_I_R) (var object x, var object y) \
      { if (N_integerp(x) && N_integerp(y)) # beides Integers? \
          { CONCAT3(I_I_,rounding,_I_I) (x,y); } # ja -> Integer-Routine \
          else                                        \
          { pushSTACK(y);                             \
            CONCAT3(R_,rounding,_I_R) (R_R_durch_R(x,y)); # ganzzahligen Anteil des Quotienten bilden \
            y = STACK_2; STACK_2 = STACK_1;           \
            STACK_1 = R_R_mal_R(y,STACK_0); # Nachkommateil mit y multiplizieren \
            skipSTACK(1);                             \
      }   }

# R_R_floor_I_R(x,y) liefert (floor x y), wo x und y reelle Zahlen sind.
# Beide Werte in den Stack.
# can trigger GC
  local void R_R_floor_I_R (object x, object y);
  GEN_R_R_round(floor)

# R_R_ceiling_I_R(x,y) liefert (ceiling x y), wo x und y reelle Zahlen sind.
# Beide Werte in den Stack.
# can trigger GC
  local void R_R_ceiling_I_R (object x, object y);
  GEN_R_R_round(ceiling)

# R_R_truncate_I_R(x,y) liefert (truncate x y), wo x und y reelle Zahlen sind.
# Beide Werte in den Stack.
# can trigger GC
  local void R_R_truncate_I_R (object x, object y);
  GEN_R_R_round(truncate)

# R_R_round_I_R(x,y) liefert (round x y), wo x und y reelle Zahlen sind.
# Beide Werte in den Stack.
# can trigger GC
  local void R_R_round_I_R (object x, object y);
  GEN_R_R_round(round)

# Generiert eine Funktion wie R_R_mod_R
  #define GEN_R_R_mod(remainder,rounding)  \
    # Liefert den Rest einer Division reeller Zahlen.      \
    # (remainder x y) = (- x (* y (rounding x y)))         \
    #                 = (* y (nth-value 1 (rounding x y))) \
    # R_R_remainder_R(x,y)                                 \
    # > x,y: reelle Zahlen                                 \
    # < ergebnis: Rest r, eine reelle Zahl                 \
    # kann GC auslösen                                     \
    # Methode:                                                \
    # Beides Integers -> I_I_remainder_I(x,y).                \
    # Sonst: R_rounding_I_R(x/y) -> (q,r). Liefere x-y*q=y*r. \
    local object CONCAT3(R_R_,remainder,_R) (var object x, var object y) \
      { if (N_integerp(x) && N_integerp(y)) # beides Integers? \
          { return CONCAT3(I_I_,remainder,_I) (x,y); } # ja -> Integer-Routine \
          else                                         \
          { pushSTACK(y);                              \
            CONCAT3(R_,rounding,_I_R) (R_R_durch_R(x,y)); # ganzzahligen Anteil des Quotienten bilden \
            y = STACK_2; x = STACK_0; skipSTACK(3);    \
            return R_R_mal_R(y,x); # Nachkommateil mit y multiplizieren \
      }   }

# R_R_mod_R(x,y) = (mod x y), wo x und y reelle Zahlen sind.
# can trigger GC
  local object R_R_mod_R (object x, object y);
  GEN_R_R_mod(mod,floor)

# R_R_rem_R(x,y) = (rem x y), wo x und y reelle Zahlen sind.
# can trigger GC
  local object R_R_rem_R (object x, object y);
  GEN_R_R_mod(rem,truncate)

# Generiert eine Funktion wie R_R_ffloor_F_R
  #define GEN_R_R_fround(rounding)  \
    # Liefert ganzzahligen Quotienten (als Float) und Rest \
    # einer Division reeller Zahlen.                       \
    # (q,r) := (frounding x y)                             \
    # R_R_frounding_F_R(x,y);                              \
    # > x,y: reelle Zahlen                                 \
    # < STACK_1: Quotient q, ein integer-wertiges Float    \
    # < STACK_0: Rest r, eine reelle Zahl                  \
    # Erniedrigt STACK um 2                                \
    # kann GC auslösen                                     \
    # Methode:                                                            \
    # x,y beide rational:                                                 \
    #   R_R_rounding_I_R(x,y), Quotienten in Float umwandeln.             \
    # Sonst:                                                              \
    #   R_frounding_F_R(x/y) -> q,r. Liefere die Werte q und x-y*q = y*r. \
    local void CONCAT3(R_R_f,rounding,_F_R) (var object x, var object y)         \
      { if (R_rationalp(x) && R_rationalp(y)) # beides rationale Zahlen?         \
          { CONCAT3(R_R_,rounding,_I_R) (x,y); # Division mit Rest               \
            STACK_1 = I_float_F(STACK_1); # Quotienten zum Float machen          \
          }                                                                      \
          else                                                                   \
          { pushSTACK(y);                                                        \
            CONCAT3(R_f,rounding,_F_R) (R_R_durch_R(x,y)); # ganzzahligen Anteil des Quotienten bilden \
            y = STACK_2; STACK_2 = STACK_1;                                      \
            STACK_1 = R_R_mal_R(y,STACK_0); # Nachkommateil mit y multiplizieren \
            skipSTACK(1);                                                        \
      }   }

# R_R_ffloor_F_R(x,y) liefert (ffloor x y), wo x und y reelle Zahlen sind.
# Beide Werte in den Stack.
# can trigger GC
  local void R_R_ffloor_F_R (object x, object y);
  GEN_R_R_fround(floor)

# R_R_fceiling_F_R(x,y) liefert (fceiling x y), wo x und y reelle Zahlen sind.
# Beide Werte in den Stack.
# can trigger GC
  local void R_R_fceiling_F_R (object x, object y);
  GEN_R_R_fround(ceiling)

# R_R_ftruncate_F_R(x,y) liefert (ftruncate x y), wo x und y reelle Zahlen sind.
# Beide Werte in den Stack.
# can trigger GC
  local void R_R_ftruncate_F_R (object x, object y);
  GEN_R_R_fround(truncate)

# R_R_fround_F_R(x,y) liefert (fround x y), wo x und y reelle Zahlen sind.
# Beide Werte in den Stack.
# can trigger GC
  local void R_R_fround_F_R (object x, object y);
  GEN_R_R_fround(round)

# R_1_plus_R(x) liefert (1+ x), wo x eine reelle Zahl ist.
# can trigger GC
  local object R_1_plus_R (object x);
  local object R_1_plus_R(x)
    var object x;
    { return (R_rationalp(x) ? RA_1_plus_RA(x) : R_R_plus_R(x,Fixnum_1)); }

# R_minus1_plus_R(x) liefert (1- x), wo x eine reelle Zahl ist.
# can trigger GC
  local object R_minus1_plus_R (object x);
  local object R_minus1_plus_R(x)
    var object x;
    { return (R_rationalp(x) ? RA_minus1_plus_RA(x) : R_R_plus_R(x,Fixnum_minus1)); }

# F_rational_RA(x) liefert (rational x), wo x ein Float ist.
# can trigger GC
  local object F_rational_RA (object x);
  # Methode:
  # Der mathematische Wert eines Float ist, wenn INTEGER-DECODE-FLOAT die
  # drei Zahlen m,e,s (Mantisse, Exponent, Vorzeichen) liefert,
  # = s * 2^e * m.
  # n:=m. Falls s<0, setze n:=-m.
  # Falls e>=0, ist (ash n e) das Ergebnis,
  # sonst ist die rationale Zahl (/ n (ash 1 (- e))) das Ergebnis.
  local object F_rational_RA(x)
    var object x;
    { F_integer_decode_float_I_I_I(x);
      # Stackaufbau: m, e, s.
     {var object n = STACK_2;
      if (R_minusp(STACK_0)) { n = I_minus_I(n); } # s<0 -> setze n := (- n)
      {var object e = STACK_1;
       skipSTACK(3);
       if (!R_minusp(e))
         { return I_I_ash_I(n,e); } # e>=0 -> (ash n e)
         else
         { pushSTACK(n);
           e = I_I_ash_I(Fixnum_1,I_minus_I(e)); # (ash 1 (- e))
           return I_posI_durch_RA(popSTACK(),e); # Bruch (/ n (ash 1 (- e)))
         }
    }}}

# R_rational_RA(x) liefert (rational x), wo x eine reelle Zahl ist.
# can trigger GC
  local object R_rational_RA (object x);
  local object R_rational_RA(x)
    var object x;
    { return (R_rationalp(x) ? x : F_rational_RA(x)); }

# R_R_comp(x,y) vergleicht zwei reelle Zahlen x und y.
# Ergebnis: 0 falls x=y, +1 falls x>y, -1 falls x<y.
# can trigger GC
  local signean R_R_comp (object x, object y);
  # Methode:
  # Beide rational oder beide Floats -> klar.
  # Eine rational, eine Float ->
  #   Die rationale Zahl zum Float machen, vergleichen.
  #   Verschieden -> Das war's.
  #   Gleich -> Das Float mit RATIONAL rational machen, nochmals vergleichen.
  local signean R_R_comp(x,y)
    var object x;
    var object y;
    { if (R_rationalp(x))
        { if (R_rationalp(y))
            # beides rationale Zahlen
            { return RA_RA_comp(x,y); }
            else
            # x rational, y Float -> x in Float umwandeln
            { pushSTACK(x); pushSTACK(y); x = RA_F_float_F(x,y); # x in Float umwandeln
             {var signean erg = F_F_comp(x,STACK_0); # und mit y vergleichen
              if (!(erg==0)) { skipSTACK(2); return erg; } # ungleich -> fertig
              y = F_rational_RA(popSTACK()); # y in rationale Zahl umwandeln
              return RA_RA_comp(popSTACK(),y); # nochmals vergleichen
            }}
        }
        else
        { if (R_rationalp(y))
            # x Float, y rational -> y in Float umwandeln
            { pushSTACK(y); pushSTACK(x); y = RA_F_float_F(y,x); # y in Float umwandeln
             {var signean erg = F_F_comp(STACK_0,y); # und mit x vergleichen
              if (!(erg==0)) { skipSTACK(2); return erg; } # ungleich -> fertig
              x = F_rational_RA(popSTACK()); # x in rationale Zahl umwandeln
              return RA_RA_comp(x,popSTACK()); # nochmals vergleichen
            }}
            else
            # beides Floats
            { return F_F_comp(x,y); }
        }
    }

# R_R_gleich(x,y) vergleicht zwei reelle Zahlen x und y.
# Ergebnis: TRUE falls x=y, FALSE sonst.
  local boolean R_R_gleich (object x, object y);
  # Methode:
  # Wann sind x und y gleich? Nach CLTL, 2nd ed., S. 290 sind die exakten
  # mathematischen Werte zu vergleichen.
  # x,y beide rational: (da x,y als gekürzte Brüche mit positivem Nenner
  #   vorliegen) genau dann, wenn die Nenner und die Zähler übereinstimmen.
  # x,y beide Floats: genau dann, wenn die Vorzeichen und die Exponenten
  #   übereinstimmen und die Mantisse des längeren aus der Mantisse des
  #   kürzeren und sonst lauter Nullen besteht.
  # x rational, y Float: (da der exakte Wert von y ein Integer * 2^Exponent
  #   ist) genau dann, wenn die Vorzeichen übereinstimmen, der Nenner von x
  #   eine Zweierpotenz ist und zwischen y = (-1)^s * m * 2^e und x = a / 2^c
  #   die Gleichung m * 2^(e+c) = |a| besteht.
  #
  # Test von zwei Integers auf Gleichheit: entweder beide EQ oder beide
  # Bignums, derselben Länge und mit denselben Digits (Vorzeichen inbegriffen).
  # Springt mit false_statement weg, falls nicht gleich.
  # define I_I_gleich(x,y) (eq(x,y) || (I_bignump(x) && I_bignump(y) && (x_len==y_len) && (compare_loop_up(x_data,y_data)==0)))
  #define I_I_gleich(x_,y_,false_statement)  \
    { var object _x = (x_);                      \
      var object _y = (y_);                      \
      if (!eq(_x,_y))                            \
        { if (!I_I_bignums_p(_x,_y)) { false_statement } \
         {var uintC xlen = Bignum_length(_x);    \
          var uintC ylen = Bignum_length(_y);    \
          if (!(xlen==ylen)) { false_statement } \
          if (!(compare_loop_up(&TheBignum(_x)->data[0],&TheBignum(_y)->data[0],xlen)==0)) { false_statement } \
    }   }}
  local boolean R_R_gleich(x,y)
    var object x;
    var object y;
    { if (R_rationalp(x))
        # x rational
        { if (R_rationalp(y))
            # x,y beide rational
            { if (RA_integerp(x))
                { if (!RA_integerp(y)) return FALSE;
                  # x,y beide Integers
                  I_I_gleich(x,y, { return FALSE; } );
                  return TRUE;
                }
                else
                { if (RA_integerp(y)) return FALSE;
                  # x,y beide Ratio
                  # Nenner vergleichen:
                  I_I_gleich(TheRatio(x)->rt_den,TheRatio(y)->rt_den, { return FALSE; } );
                  # Zähler vergleichen:
                  I_I_gleich(TheRatio(x)->rt_num,TheRatio(y)->rt_num, { return FALSE; } );
                  return TRUE;
            }   }
            else
            # x rational, y Float
            { var object tmp = x; x = y; y = tmp; }
        }
      # x Float, y Float oder rational.
      # x und y auspacken, liefert jeweils ein Vorzeichen, eine Mantisse
      # (NUDS mit gesetztem höchstem Bit) und einen Exponenten.
     {SAVE_NUM_STACK # num_stack retten
      var signean x_sign;
      var uintD* x_MSDptr;
      var uintC x_len;
      var sintL x_exp;
      var signean y_sign;
      var uintD* y_MSDptr;
      var uintC y_len;
      var sintL y_exp;
      floatcase(x,
      /* x SF */ { var uint32 x_mant;
                   SF_decode(x, { goto x_zero; }, x_sign=,x_exp=,x_mant=);
                   x_mant = x_mant << (32-(SF_mant_len+1));
                   num_stack_need(32/intDsize, x_MSDptr=,); x_len = 32/intDsize;
                   set_32_Dptr(x_MSDptr,x_mant);
                 },
      /* x FF */ { var uint32 x_mant;
                   FF_decode(x, { goto x_zero; }, x_sign=,x_exp=,x_mant=);
                   x_mant = x_mant << (32-(FF_mant_len+1));
                   num_stack_need(32/intDsize, x_MSDptr=,); x_len = 32/intDsize;
                   set_32_Dptr(x_MSDptr,x_mant);
                 },
      /* x DF */ { ifdef_intQsize(
                     { var uint64 x_mant;
                       DF_decode(x, { goto x_zero; }, x_sign=,x_exp=,x_mant=);
                       x_mant = x_mant << (64-(DF_mant_len+1));
                       num_stack_need(64/intDsize, x_MSDptr=,);
                       x_len = 64/intDsize;
                       set_32_Dptr(&x_MSDptr[0],(uint32)(x_mant>>32));
                       set_32_Dptr(&x_MSDptr[32/intDsize],(uint32)x_mant);
                     },
                     { var uint32 x_manthi;
                       var uint32 x_mantlo;
                       DF_decode2(x, { goto x_zero; }, x_sign=,x_exp=,x_manthi=,x_mantlo=);
                       x_manthi = (x_manthi << (64-(DF_mant_len+1)))
                                  | (x_mantlo >> ((DF_mant_len+1)-32));
                       x_mantlo = x_mantlo << (64-(DF_mant_len+1));
                       num_stack_need(64/intDsize, x_MSDptr=,);
                       x_len = 64/intDsize;
                       set_32_Dptr(&x_MSDptr[0],x_manthi);
                       set_32_Dptr(&x_MSDptr[32/intDsize],x_mantlo);
                     });
                 },
      /* x LF */ { LF_decode(x, { goto x_zero; }, x_sign=,x_exp=,x_MSDptr=,x_len=,); }
               );
      if (!R_rationalp(y))
        { floatcase(y,
          /* y SF */ { var uint32 y_mant;
                       SF_decode(y, { goto y_zero; }, y_sign=,y_exp=,y_mant=);
                       y_mant = y_mant << (32-(SF_mant_len+1));
                       num_stack_need(32/intDsize, y_MSDptr=,); y_len = 32/intDsize;
                       set_32_Dptr(y_MSDptr,y_mant);
                     },
          /* y FF */ { var uint32 y_mant;
                       FF_decode(y, { goto y_zero; }, y_sign=,y_exp=,y_mant=);
                       y_mant = y_mant << (32-(FF_mant_len+1));
                       num_stack_need(32/intDsize, y_MSDptr=,); y_len = 32/intDsize;
                       set_32_Dptr(y_MSDptr,y_mant);
                     },
          /* y DF */ { ifdef_intQsize(
                         { var uint64 y_mant;
                           DF_decode(y, { goto y_zero; }, y_sign=,y_exp=,y_mant=);
                           y_mant = y_mant << (64-(DF_mant_len+1));
                           num_stack_need(64/intDsize, y_MSDptr=,);
                           y_len = 64/intDsize;
                           set_32_Dptr(&y_MSDptr[0],(uint32)(y_mant>>32));
                           set_32_Dptr(&y_MSDptr[32/intDsize],(uint32)y_mant);
                         },
                         { var uint32 y_manthi;
                           var uint32 y_mantlo;
                           DF_decode2(y, { goto y_zero; }, y_sign=,y_exp=,y_manthi=,y_mantlo=);
                           y_manthi = (y_manthi << (64-(DF_mant_len+1)))
                                      | (y_mantlo >> ((DF_mant_len+1)-32));
                           y_mantlo = y_mantlo << (64-(DF_mant_len+1));
                           num_stack_need(64/intDsize, y_MSDptr=,);
                           y_len = 64/intDsize;
                           set_32_Dptr(&y_MSDptr[0],y_manthi);
                           set_32_Dptr(&y_MSDptr[32/intDsize],y_mantlo);
                         });
                     },
          /* y LF */ { LF_decode(y, { goto y_zero; }, y_sign=,y_exp=,y_MSDptr=,y_len=,); }
                   );
        }
        else
        { var uintL y_den_exp;
          var uintD* y_LSDptr;
          var uintL s;
          if (RA_integerp(y))
            { y_den_exp = 0; }
            else
            { y_den_exp = I_power2p(TheRatio(y)->rt_den);
              if (y_den_exp == 0)
                { goto no; } # x Float, y's denominator not a power of 2
              y_den_exp--;
              y = TheRatio(y)->rt_num;
            }
          I_to_NDS(y,y_MSDptr=,y_len=,y_LSDptr=); # NDS holen
          if (y_len == 0) { goto y_zero; }
          # Nicht alle führenden intDsize+1 Bits sind gleich.
          if ((sintD)y_MSDptr[0] < 0) # falls <0, negieren
            { y_sign = -1; neg_loop_down(y_LSDptr,y_len); }
            else
            { y_sign = 0; }
          # Nicht alle führenden intDsize+1 Bits sind =0.
          if (y_MSDptr[0] == 0) # normalisieren (max. 1 Nulldigit entfernen)
            { y_MSDptr++; y_len--; }
          # Nun ist y_MSDptr[0]/=0 und y_len>0.
          # Führendes Bit auf 1 normalisieren:
          integerlengthD(y_MSDptr[0], s = intDsize - );
          if (s > 0)
            { begin_arith_call();
              shiftleft_loop_down(y_LSDptr,y_len,s,0);
              end_arith_call();
            }
          y_exp = (sintL)((uintL)y_len * intDsize - s) - (sintL)y_den_exp;
        }
      # Vergleiche Vorzeichen, Exponenten und Mantissen:
      if ((x_sign ^ y_sign) < 0) { goto no; }
      if (!(x_exp == y_exp)) { goto no; }
      if (x_len > y_len)
        { if (test_loop_up(&x_MSDptr[y_len],x_len-y_len)) goto no;
          x_len = y_len;
        }
      else if (y_len > x_len)
        { if (test_loop_up(&y_MSDptr[x_len],y_len-x_len)) goto no; }
      if (compare_loop_up(x_MSDptr,y_MSDptr,x_len)) goto no;
      # Vergleich erfüllt.
      RESTORE_NUM_STACK # num_stack zurück
      return TRUE;
      x_zero:
        RESTORE_NUM_STACK # num_stack zurück
        if (R_zerop(y)) { return TRUE; } else { return FALSE; }
      y_zero:
      no:
        RESTORE_NUM_STACK # num_stack zurück
        return FALSE;
    }}

# EQUALP-Hash-Code einer reellen Zahl:
# Mischung aus Exponent, Länge, erste 32 Bit,
# aber so gemacht, dass (hashcode (rational x)) = (hashcode x)
# und (hashcode 0.0) = 0 (wichtig wegen "complex canonicalization").
  global uint32 hashcode4_real (object obj);
  global uint32 hashcode4_uint32 (uint32 x);
  #define hashcode4_(msd,exp,sign)  \
    (((((uint32)(msd) << 7) | ((uint32)(msd) >> 25)) ^ ((sint32)(sign) << 30)) + (uintL)(exp))
  #define hashcode4_one  hashcode4_(bit(31),1,0)
  global uint32 hashcode4_real(obj)
    var object obj;
    { var signean sign;
      var uint32 msd;
      var sintL exp;
      if (ratiop(obj))
        { # Making sure that a float and its rational equivalent have
          # the same hash code is tricky. This code depends on the fact
          # that the above hashcode4_() macro is linear in `exp'.
          return hashcode4_real(TheRatio(obj)->rt_num)
                 - hashcode4_real(TheRatio(obj)->rt_den)
                 + hashcode4_one;
        }
     {SAVE_NUM_STACK # num_stack retten
      if (R_rationalp(obj))
        # obj Integer
        { var uintD* MSDptr;
          var uintC len;
          var uint32 msd2;
          I_to_NDS_nocopy(obj,MSDptr=,len=,);
          # Nicht alle führenden intDsize+1 Bits sind gleich.
          if (len >= 64/intDsize)
            { msd = get_32_Dptr(&MSDptr[0]);
              msd2 = get_32_Dptr(&MSDptr[32/intDsize]);
            }
          else if (len > 32/intDsize)
            { msd = get_32_Dptr(&MSDptr[0]);
              msd2 = get_max32_Dptr(intDsize*len-32,&MSDptr[32/intDsize])
                     << (64-intDsize*len);
            }
          else if (len == 32/intDsize)
            { msd = get_32_Dptr(&MSDptr[0]);
              msd2 = 0;
            }
          else if (len > 0) # 0 < len < 32/intDsize
            { msd = get_max32_Dptr(intDsize*len,&MSDptr[0])
                    << (32-intDsize*len);
              msd2 = 0;
            }
          else # (len == 0)
            { goto zero; }
          if ((sint32)msd < 0) # falls <0, negieren
            { sign = -1;
              # msd|msd2 := - msd|msd2 - (1 falls noch weitere Bits /= 0)
              msd = ~msd; msd2 = ~msd2;
              if ((len <= 64/intDsize)
                  || !test_loop_up(&MSDptr[64/intDsize],len-64/intDsize)
                 )
                { msd2++; if (msd2==0) { msd++; } }
            }
            else
            { sign = 0; }
          exp = (uintL)len * intDsize;
          # Nicht alle führenden intDsize+1 Bits sind =0.
          # Wegen intDsize<=32: Nicht alle führenden 33 Bits sind =0.
          if (msd==0)
            { msd = msd2; msd2 = 0; exp -= 32; }
          # Nicht alle führenden 32 Bits sind =0.
          # Führendes Bit auf 1 normalisieren:
            else
            { var uintL s;
              integerlength32(msd, s = 32 - );
              if (s > 0) { msd = (msd << s) | (msd2 >> (32-s)); }
              exp -= s;
        }   }
        else
        # obj Float
        { floatcase(obj,
          /* SF */ { var uint32 mant;
                     SF_decode(obj, { goto zero; }, sign=,exp=,mant=);
                     msd = mant << (32-(SF_mant_len+1));
                   },
          /* FF */ { var uint32 mant;
                     FF_decode(obj, { goto zero; }, sign=,exp=,mant=);
                     msd = mant << (32-(FF_mant_len+1));
                   },
          /* DF */ { ifdef_intQsize(
                       { var uint64 mant;
                         DF_decode(obj, { goto zero; }, sign=,exp=,mant=);
                         msd = mant >> ((DF_mant_len+1)-32);
                       },
                       { var uint32 manthi;
                         var uint32 mantlo;
                         DF_decode2(obj, { goto zero; }, sign=,exp=,manthi=,mantlo=);
                         msd = (manthi << (64-(DF_mant_len+1)))
                               | (mantlo >> ((DF_mant_len+1)-32));
                       });
                   },
          /* LF */ { var uintD* MSDptr;
                     LF_decode(obj, { goto zero; }, sign=,exp=,MSDptr=,,);
                     msd = get_32_Dptr(MSDptr);
                   }
                   );
        }
      RESTORE_NUM_STACK # num_stack zurück
      return hashcode4_(msd,exp,sign);
      zero:
        RESTORE_NUM_STACK # num_stack zurück
        return 0;
    }}
  global uint32 hashcode4_uint32(x)
    var uint32 x;
    { if (x == 0) return 0;
      # Führendes Bit auf 1 normalisieren:
     {var uintL exp;
      integerlength32(x, exp = );
      {var uint32 msd = x << (32-exp);
       return hashcode4_(msd,exp,0);
    }}}
  global uint32 hashcode4_uint4 [16] =
    { 0,
      hashcode4_( 1*(uint32)bit(31),1,0),
      hashcode4_( 2*(uint32)bit(30),2,0),
      hashcode4_( 3*(uint32)bit(30),2,0),
      hashcode4_( 4*(uint32)bit(29),3,0),
      hashcode4_( 5*(uint32)bit(29),3,0),
      hashcode4_( 6*(uint32)bit(29),3,0),
      hashcode4_( 7*(uint32)bit(29),3,0),
      hashcode4_( 8*(uint32)bit(28),4,0),
      hashcode4_( 9*(uint32)bit(28),4,0),
      hashcode4_(10*(uint32)bit(28),4,0),
      hashcode4_(11*(uint32)bit(28),4,0),
      hashcode4_(12*(uint32)bit(28),4,0),
      hashcode4_(13*(uint32)bit(28),4,0),
      hashcode4_(14*(uint32)bit(28),4,0),
      hashcode4_(15*(uint32)bit(28),4,0)
    };

# R_R_max_R(x,y) liefert (max x y), wo x und y reelle Zahlen sind.
# can trigger GC
  local object R_R_max_R (object x, object y);
  local object R_R_max_R(x,y)
    var object x;
    var object y;
    { pushSTACK(x); pushSTACK(y); # beide retten
     {var object erg =
        (R_R_comp(x,y) >= 0 # vergleichen
         ? STACK_1 # x>=y -> x
         : STACK_0 # x<y -> y
        );
      skipSTACK(2);
      return erg;
    }}

# R_R_min_R(x,y) liefert (min x y), wo x und y reelle Zahlen sind.
# can trigger GC
  local object R_R_min_R (object x, object y);
  local object R_R_min_R(x,y)
    var object x;
    var object y;
    { pushSTACK(x); pushSTACK(y); # beide retten
     {var object erg =
        (R_R_comp(x,y) <= 0 # vergleichen
         ? STACK_1 # x<=y -> x
         : STACK_0 # x>y -> y
        );
      skipSTACK(2);
      return erg;
    }}

# R_signum_R(x) liefert (signum x), wo x eine reelle Zahl ist.
# can trigger GC
  local object R_signum_R (object x);
  local object R_signum_R(x)
    var object x;
    { if (R_rationalp(x))
        # x rational
        { if (R_minusp(x)) { return Fixnum_minus1; } # x<0 -> -1
          elif (eq(x,Fixnum_0)) { return x; } # x=0 -> 0
          else { return Fixnum_1; } # x>0 -> +1
        }
        else
        # x Float
        { floatcase(x,
          /* x SF */ { if (R_minusp(x)) { return SF_minus1; } # x<0 -> -1.0
                       elif (SF_zerop(x)) { return x; } # x=0 -> 0.0
                       else { return SF_1; } # x>0 -> +1.0
                     },
          /* x FF */ { if (R_minusp(x)) { return FF_minus1; } # x<0 -> -1.0
                       elif (FF_zerop(x)) { return x; } # x=0 -> 0.0
                       else { return FF_1; } # x>0 -> +1.0
                     },
          /* x DF */ { if (R_minusp(x)) { return DF_minus1; } # x<0 -> -1.0
                       elif (DF_zerop(x)) { return x; } # x=0 -> 0.0
                       else { return DF_1; } # x>0 -> +1.0
                     },
          /* x LF */ { if (LF_zerop(x)) { return x; } #  # x=0 -> 0.0
                       else { encode_LF1s(LF_sign(x),Lfloat_length(x), return); } # je nach Vorzeichen von x
                     }
                   );
    }   }

# R_sqrt_R(x) = (sqrt x) zieht die Wurzel aus einer reellen Zahl x >=0.
# can trigger GC
  local object R_sqrt_R (object x);
  local object R_sqrt_R(x)
    var object x;
    { if (R_rationalp(x))
        # x rationale Zahl >=0
        { pushSTACK(x); # x retten
          x = RA_sqrtp(x); # auf Quadrat testen
          if (!eq(x,nullobj))
            { skipSTACK(1); return x; } # war Quadrat, x ist die Wurzel
            else
            # x in Float umwandeln, dann die Wurzel ziehen:
            { return F_sqrt_F(RA_float_F(popSTACK())); }
        }
        else
        { return F_sqrt_F(x); }
    }
  #define RA_sqrt_R  R_sqrt_R

# R_I_expt_R(x,y) = (expt x y), wo x eine reelle Zahl und y ein Integer ist.
# can trigger GC
  local object R_I_expt_R (object x, object y);
  # Methode:
  # Für y>0:
  #   a:=x, b:=y.
  #   Solange b gerade, setze a:=a*a, b:=b/2. [a^b bleibt invariant, = x^y.]
  #   c:=a.
  #   Solange b:=floor(b/2) >0 ist,
  #     setze a:=a*a, und falls b ungerade, setze c:=a*c.
  #   Ergebnis c.
  # Für y=0: Ergebnis 1.
  # Für y<0: (/ (expt x (- y))).
  local object R_I_expt_R(x,y)
    var object x;
    var object y;
    { if (eq(y,Fixnum_0)) { return Fixnum_1; } # y=0 -> Ergebnis 1
      pushSTACK(x);
     {var boolean y_negative = FALSE;
      if (R_minusp(y)) { y = I_minus_I(y); y_negative = TRUE; } # Betrag von y nehmen
      # Nun ist y>0.
      if (R_rationalp(x)) # x rational (Abfrage nicht GC-gefährdet!) ?
        { x = RA_I_expt_RA(popSTACK(),y); } # ja -> schnellere Routine
        else
        { pushSTACK(y);
          # Stackaufbau: a, b.
          while (!I_oddp(y))
            { STACK_1 = R_square_R(STACK_1); # a:=a*a
              STACK_0 = y = I_I_ash_I(STACK_0,Fixnum_minus1); # b := (ash b -1)
            }
          pushSTACK(STACK_1); # c:=a
          # Stackaufbau: a, b, c.
          until (eq(y=STACK_1,Fixnum_1)) # Solange b/=1
            { STACK_1 = I_I_ash_I(y,Fixnum_minus1); # b := (ash b -1)
             {var object a = STACK_2 = R_square_R(STACK_2); # a:=a*a
              if (I_oddp(STACK_1)) { STACK_0 = R_R_mal_R(a,STACK_0); } # evtl. c:=a*c
            }}
          x = STACK_0; skipSTACK(3);
        }
      # (expt x (abs y)) ist jetzt in x.
      return (y_negative ? R_durch_R(x) : x); # evtl. noch Kehrwert nehmen
    }}

# R_rationalize_RA(x) liefert (rationalize x), wo x eine reelle Zahl ist.
# can trigger GC
  local object R_rationalize_RA (object x);
  # Methode (rekursiv dargestellt):
  # Falls x rational ist: x.
  # Falls x=0.0: 0.
  # Falls x<0.0: (- (rationalize (- x)))
  # Falls x>0.0:
  #   (Integer-Decode-Float x) liefert m,e,s=1.
  #   Falls e>=0 : Liefere x=m*2^e als Ergebnis.
  #   Suche rationale Zahl zwischen a=(m-1/2)*2^e und b=(m+1/2)*2^e mit
  #   möglichst kleinem Zähler und Nenner. (a,b einschließlich, aber da a,b
  #   den Nenner 2^(|e|+1) haben, während x selbst den Nenner <=2^|e| hat,
  #   können weder a noch b als Ergebnis herauskommen.)
  #   Suche also bei gegebenem a,b (0<a<b) Bruch y mit a <= y <= b.
  #   Rekursiv:
  #     c:=(ceiling a)
  #     if c<b then return c      ; weil a<=c<b, c ganz
  #            else ; a nicht ganz (sonst c=a<b)
  #              k:=c-1 ; k=floor(a), k < a < b <= k+1
  #              return y = k + 1/(Bruch zwischen 1/(b-k) und 1/(a-k))
  #                                ; wobei 1 <= 1/(b-k) < 1/(a-k)
  # Man sieht, dass hierbei eine Kettenbruchentwicklung auftritt.
  # Methode (iterativ):
  # Falls x rational: x.
  # (Integer-Decode-Float x) liefert m,e,s.
  # e>=0 -> m*2^e*s als Ergebnis (darin ist x=0.0 inbegriffen).
  # Bilde a:=(2*m-1)*2^(e-1) und b:=(2*m+1)*2^(e-1), rationale Zahlen >0,
  #   (unkürzbar, da Nenner Zweierpotenz und Zähler ungerade).
  # Starte Kettenbruchentwicklung (d.h. p[-1]:=0, p[0]:=1, q[-1]:=1, q[0]:=0, i:=0.)
  # Schleife:
  #   c:=(ceiling a)
  #   if c>=b then k:=c-1, "Ziffer k", (a,b) := (1/(b-k),1/(a-k)), goto Schleife
  # "Ziffer c".
  # (Dabei bedeutet "Ziffer a" die Iteration
  #   i:=i+1, p[i]:=a*p[i-1]+p[i-2], q[i]:=a*q[i-1]+q[i-2].)
  # Ende, liefere s * (p[i]/q[i]), das ist wegen der Invarianten
  #   p[i]*q[i-1]-p[i-1]*q[i]=(-1)^i  ein bereits gekürzter Bruch.
  local object R_rationalize_RA(x)
    var object x;
    { if (R_rationalp(x)) { return x; } # x rational -> x als Ergebnis.
      F_integer_decode_float_I_I_I(x);
      # Stackaufbau: m, e, s.
      if (!R_minusp(STACK_1))
        # e>=0.
        { var object y = I_I_ash_I(STACK_2,STACK_1); # (ash m e) bilden
          if (R_minusp(STACK_0)) { y = I_minus_I(y); } # Bei s<0: y := (- y)
          skipSTACK(3); return y;
        }
      # e<0.
      {var object m2 = I_I_ash_I(STACK_2,Fixnum_1); # 2*m
       pushSTACK(m2); pushSTACK(I_minus1_plus_I(m2)); # 2*m-1 bilden
       STACK_1 = I_1_plus_I(STACK_1); # 2*m+1 bilden
      }
      # Stackaufbau: -, e, s, 2*m+1, 2*m-1.
      STACK_3 = I_I_ash_I(Fixnum_1,I_1_plus_I(I_minus_I(STACK_3))); # (ash 1 (1+ (- e)))
      # Stackaufbau: -, 2^(1-e), s, 2*m+1, 2*m-1.
      STACK_0 = I_I_to_RT(STACK_0,STACK_3); # (2*m-1)/(2^(1-e)) = a
      STACK_1 = I_I_to_RT(STACK_1,STACK_3); # (2*m+1)/(2^(1-e)) = b
      # Stackaufbau: -, 2^(1-e), s, b, a.
      pushSTACK(Fixnum_0); pushSTACK(Fixnum_1);
      pushSTACK(Fixnum_1); pushSTACK(Fixnum_0);
      # Stackaufbau: -, -, s, b, a, p[i-1], p[i], q[i-1], q[i].
      loop
        { RA_ceiling_I_RA(STACK_4); # c := (ceiling a)
          # Stackaufbau: ..., c, -.
          if (RA_RA_comp(STACK_1,STACK_(5+2))<0) break; # bei c<b Schleifenende
         {var object k = I_minus1_plus_I(STACK_1); # k = c-1
          skipSTACK(2);
          # "Ziffer" k :
          STACK_7 = k; # k retten
          k = I_I_mal_I(k,STACK_2); # mit p[i] multiplizieren
          k = I_I_plus_I(k,STACK_3); # und p[i-1] addieren
          STACK_3 = STACK_2; STACK_2 = k; # als p[i+1] ablegen
          k = STACK_7;
          k = I_I_mal_I(k,STACK_0); # mit q[i] multiplizieren
          k = I_I_plus_I(k,STACK_1); # und q[i-1] addieren
          STACK_1 = STACK_0; STACK_0 = k; # als q[i+1] ablegen
         }# neues b ausrechnen: b := (/ (- a k))
         {var object new_b = RA_durch_RA(RA_RA_minus_RA(STACK_4,STACK_7));
          var object old_b = STACK_5;
          STACK_5 = new_b;
          # neues a ausrechnen: a := (/ (- b k))
          STACK_4 = RA_durch_RA(RA_RA_minus_RA(old_b,STACK_7));
        }}
      # letzte "Ziffer" k=c :
     {var object q = I_I_mal_I(STACK_1,STACK_(0+2)); # c mit q[i] multiplizieren
      q = I_I_plus_I(q,STACK_(1+2)); # und q[i-1] addieren
      STACK_(0+2) = q; # als letztes q[i] ablegen
     }
     { var object p = I_I_mal_I(STACK_1,STACK_(2+2)); # c mit p[i] multiplizieren
       p = I_I_plus_I(p,STACK_(3+2)); # und p[i-1] addieren, gibt letztes p[i]
       # Ergebnis ist (s*p[i])/q[i]:
       if (R_minusp(STACK_(6+2))) { p = I_minus_I(p); } # bei s<0: (- p[i]) statt p[i]
      {var object q = STACK_(0+2);
       skipSTACK(9+2); # Stack aufräumen
       return I_I_to_RA(p,q); # (/ +-p[i] q[i]) bilden
    }}}

