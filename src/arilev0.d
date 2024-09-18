# Arithmetic, Level 0
# works with 16-bit words and unsigned 32-bit words.

# Get sign of a 32-bit int
# sign_of_sint32(value)
# > value: sint32
# < sint16 result: 0 if value>=0, -1 if value<0.
  extern sint16 sign_of_sint32 (sint32 value);
  #if defined(SPARC64)
    #define sign_of_sint32(value)  (((sint64)(sint32)(value)) >> 63)
  #elif defined(SPARC) || defined(ARM)
    #define sign_of_sint32(value)  (((sint32)(value)) >> 31)
  #else
    #define sign_of_sint32(value)  ((sint32)(value) >= 0 ? 0 : -1)
  #endif

# Get sign of a sint16
# sign_of_sint16(value)
# > value: sint16
# < sint16 result: 0 if value>=0, -1 if value<0.
  extern sint16 sign_of_sint16 (sint16 value);
  #if defined(SPARC64)
    #define sign_of_sint16(value)  (((sint64)(sint16)(value)) >> 63)
  #elif defined(SPARC) || defined(ARM)
    #define sign_of_sint16(value)  (((sint32)(sint16)(value)) >> 31)
  #else
    #define sign_of_sint16(value)  ((sint16)(value) >= 0 ? 0 : -1)
  #endif

# Get high-word of a uint32
# high16(value)
  extern uint16 high16 (uint32 value);
  #define high16(value)  ((uint16)((uint32)(value)>>16))

# Get low-word of a uint32
# low16(value)
  extern uint16 low16 (uint32 value);
  #define low16(value)  ((uint16)(uint32)(value))

# Eine 32-Bit-Zahl aus ihrem High-Word und ihrem Low-Word bestimmen:
# highlow32(uint16 high, uint16 low)
  extern uint32 highlow32 (uint16 high, uint16 low);
  #define highlow32(high,low)  \
    (((uint32)(uint16)(high) << 16) | (uint32)(uint16)(low))

# Eine 32-Bit-Zahl aus ihrem High-Word und ihrem Low-Word 0 bestimmen:
# highlow32_0(uint16 high)
  extern uint32 highlow32_0 (uint16 high);
  # define highlow32_0(high)  highlow32(high,0)
  #define highlow32_0(high)  ((uint32)(uint16)(high) << 16)

#if (intVsize>32)

# High-Word einer 64-Bit-Zahl bestimmen
# high32(value)
  extern uint32 high32 (uint64 value);
  #define high32(value)  ((uint32)((uint64)(value)>>32))

# Low-Word einer 64-Bit-Zahl bestimmen
# low32(value)
  extern uint32 low32 (uint64 value);
  #define low32(value)  ((uint32)(uint64)(value))

# Eine 64-Bit-Zahl aus ihrem High-Word und ihrem Low-Word bestimmen:
# highlow64(uint32 high, uint32 low)
  extern uint64 highlow64 (uint32 high, uint32 low);
  #define highlow64(high,low)  \
    (((uint64)(uint32)(high) << 32) | (uint64)(uint32)(low))

# Eine 64-Bit-Zahl aus ihrem High-Word und ihrem Low-Word 0 bestimmen:
# highlow64_0(uint32 high)
  extern uint64 highlow64_0 (uint32 high);
  #define highlow64_0(high)  ((uint64)(uint32)(high) << 32)

#endif

#ifdef ARM

# Returns the r0 part of a 64-bit (uint64) return value returned in r0,r1.
# retval64_r0(value)
# Returns the r1 part of a 64-bit (uint64) return value returned in r0,r1.
# retval64_r1(value)
  #ifdef __ARMEL__
    /* little-endian ARM: uint64 retval = 2^32*r1+r0 */
    #define retval64_r0(value)  ((uint32)(uint64)(value))
    #define retval64_r1(value)  ((uint32)((uint64)(value)>>32))
  #else
    /* big-endian ARM: uint64 retval = 2^32*r0+r1 */
    #define retval64_r0(value)  ((uint32)((uint64)(value)>>32))
    #define retval64_r1(value)  ((uint32)(uint64)(value))
  #endif

#endif

# Multipliziert zwei 16-Bit-Zahlen miteinander und liefert eine 32-Bit-Zahl:
# mulu16(arg1,arg2)
# > arg1, arg2 : zwei 16-Bit-Zahlen
# < result: eine 32-Bit-Zahl
  extern uint32 mulu16 (uint16 arg1, uint16 arg2);
  #if defined(GNU) || defined(INTEL)
    #if defined(SPARC) && !defined(SPARC64) && defined(FAST_DOUBLE) # Ist das schneller als _mulu16 ??
      #define mulu16(arg1,arg2)  \
        ({var union { double f; uint32 i[2]; } __fi; \
          __fi.f = (double)(sint32)(uint16)(arg1)*(double)(sint32)(uint16)(arg2) \
                   + (double)(4503599627370496.0L); # + 2^52, zum Normalisieren  \
          __fi.i[1]; # untere 32 Bit herausholen (benutzt BIG_ENDIAN_P !)        \
         })
    #elif defined(SPARC64) && !defined(NO_ASM)
      #define mulu16(arg1,arg2)  \
        ({ var register uint64 _result;                                         \
           __asm__("umul %1,%2,%0"                                              \
                   : "=&r" (_result)                                            \
                   : "r" ((uint32)(uint16)(arg1)), "r" ((uint32)(uint16)(arg2)) \
                  );                                                            \
           (uint32)_result;                                                     \
         })
    #elif defined(I80386) && !defined(NO_ASM)
      #define mulu16(arg1,arg2)  \
        ({ var register uint16 _hi;                                         \
           var register uint16 _lo;                                         \
           __asm__("mulw %2"                                                \
                   : "=d" /* %dx */ (_hi), "=a" /* %ax */ (_lo)             \
                   : "rm" ((uint16)(arg1)), "1" /* %eax */ ((uint16)(arg2)) \
                  );                                                        \
           highlow32(_hi,_lo);                                              \
         })
    #endif
  #else
    #if (defined(SPARC) || defined(SPARC64)) && !defined(NO_ARI_ASM)
      extern_C uint32 asm_mulu16_ (uint16 arg1, uint16 arg2); # extern in Assembler
      #define mulu16 asm_mulu16_
    #endif
  #endif
  #ifndef mulu16
    #define mulu16(arg1,arg2)  ((uint32)(uint16)(arg1)*(uint32)(uint16)(arg2))
  #endif

# Multipliziert zwei 24-Bit-Zahlen zusammen und liefert eine 48-Bit-Zahl.
# mulu24(arg1,arg2,hi=,lo=);
# > arg1, arg2 : zwei 24-Bit-Zahlen
# < 2^32*hi+lo : eine 48-Bit-Zahl
  #if defined(SPARC) && !defined(SPARC64) && defined(FAST_DOUBLE)
    #define mulu24(x,y,hi_assignment,lo_assignment)  \
      { var uint32 _x = (x);                                                            \
        var uint32 _y = (y);                                                            \
        var union { double f; uint32 i[2]; uint16 s[4]; } __fi;                         \
        __fi.f = (double)(sint32)(_x)*(double)(sint32)(_y)                              \
                 + (double)(4503599627370496.0L); # + 2^52, zum Normalisieren           \
        hi_assignment __fi.s[1]; # mittlere 16 Bit herausholen, (benutzt BIG_ENDIAN_P !) \
        lo_assignment __fi.i[1]; # untere 32 Bit herausholen (benutzt BIG_ENDIAN_P !)    \
      }
  #else
    #define mulu24  mulu32
  #endif

# Multipliziert zwei 32-Bit-Zahlen miteinander und liefert eine 64-Bit-Zahl:
# mulu32(arg1,arg2,hi=,lo=);
# > arg1, arg2 : zwei 32-Bit-Zahlen
# < 2^32*hi+lo : eine 64-Bit-Zahl
  extern uint32 mulu32_high;                          # -> High-Teil
  #if defined(GNU) || defined(INTEL)
    #ifdef M68K
      #define mulu32(x,y,hi_assignment,lo_assignment)  \
        ({ var uint32 _x = (x);                                          \
           var uint32 _y = (y);                                          \
           var uint16 _x1 = high16(_x);                                  \
           var uint16 _x0 = low16(_x);                                   \
           var uint16 _y1 = high16(_y);                                  \
           var uint16 _y0 = low16(_y);                                   \
           var uint32 _hi = mulu16(_x1,_y1); # obere Portion             \
           var uint32 _lo = mulu16(_x0,_y0); # untere Portion            \
           {var uint32 _mid = mulu16(_x0,_y1); # 1. mittlere Portion     \
            _hi += high16(_mid); _mid = highlow32_0(low16(_mid));        \
            _lo += _mid; if (_lo < _mid) { _hi += 1; } # 64-Bit-Addition \
           }                                                             \
           {var uint32 _mid = mulu16(_x1,_y0); # 2. mittlere Portion     \
            _hi += high16(_mid); _mid = highlow32_0(low16(_mid));        \
            _lo += _mid; if (_lo < _mid) { _hi += 1; } # 64-Bit-Addition \
           }                                                             \
           hi_assignment _hi;                                            \
           lo_assignment _lo;                                            \
         })
    #elif defined(I80386) && !defined(NO_ASM)
      #define mulu32(x,y,hi_assignment,lo_assignment)  \
        ({ var register uint32 _hi;                                   \
           var register uint32 _lo;                                   \
           __asm__("mull %2"                                          \
                   : "=d" /* %edx */ (_hi), "=a" /* %eax */ (_lo)     \
                   : "rm" ((uint32)(x)), "1" /* %eax */ ((uint32)(y)) \
                  );                                                  \
           hi_assignment _hi; lo_assignment _lo;                        \
         })
    #elif defined(SPARC64) && !defined(NO_ASM)
      #define mulu32(x,y,hi_assignment,lo_assignment)  \
        ({ var register uint64 _hi;                            \
           var register uint64 _lo;                            \
           __asm__("umul %2,%3,%0\n\trd %%y,%1"                \
                   : "=&r" (_lo), "=r" (_hi)                   \
                   : "r" ((uint32)(x)), "r" ((uint32)(y))      \
                  );                                           \
           hi_assignment (uint32)_hi; lo_assignment (uint32)_lo; \
         })
    #elif defined(MIPS) && !defined(NO_ASM)
      #define mulu32(x,y,hi_assignment,lo_assignment)  \
        ({ var register uint32 _hi;                       \
           var register uint32 _lo;                       \
           __asm__("multu %3,%2 ; mfhi %0 ; mflo %1"      \
                   : "=r" (_hi), "=r" (_lo)               \
                   : "r" ((uint32)(x)), "r" ((uint32)(y)) \
                  );                                      \
           hi_assignment _hi; lo_assignment _lo;            \
         })
    #elif (defined(SPARC) || defined(SPARC64)) && !defined(NO_ARI_ASM)
      extern_C uint64 asm_mulu32_64 (uint32 arg1, uint32 arg2); # extern in Assembler
      #define mulu32(x,y,hi_assignment,lo_assignment)      \
        ({ var register uint64 _prod = asm_mulu32_64(x,y); \
           hi_assignment (uint32)(_prod>>32);              \
           lo_assignment (uint32)(_prod);                  \
         })
    #elif defined(ARM) && !defined(NO_ARI_ASM)
      extern_C uint64 asm_mulu32_64 (uint32 arg1, uint32 arg2); # extern in Assembler
      #define mulu32(x,y,hi_assignment,lo_assignment)      \
        ({ var register uint64 _prod = asm_mulu32_64(x,y); \
           hi_assignment retval64_r1(_prod);               \
           lo_assignment retval64_r0(_prod);               \
         })
    #elif defined(HAVE_LONG_LONG_INT)
      #define mulu32(x,y,hi_assignment,lo_assignment)  \
        ({ var register uint64 _prod = (uint64)(x) * (uint64)(y); \
           hi_assignment (uint32)(_prod>>32);                      \
           lo_assignment (uint32)(_prod);                          \
         })
    #endif
  #endif
  #ifndef mulu32
    # Use this function if you are not interested in the high part:
    # extern_C uint32 mulu32_ (uint32 arg1, uint32 arg2); # -> Low-Teil
    #if (defined(I80386) || defined(SPARC) || defined(SPARC64) || (defined(HPPA) && !defined(HPPA64))) && !defined(NO_ARI_ASM)
      extern_C uint64 asm_mulu32_64 (uint32 arg1, uint32 arg2); # extern in Assembler
      #define mulu32(x,y,hi_assignment,lo_assignment)  \
        { var uint64 _prod_from_mulu32 = asm_mulu32_64(x,y); \
          hi_assignment (uint32)(_prod_from_mulu32>>32);     \
          lo_assignment (uint32)(_prod_from_mulu32);         \
        }
      #define mulu32_(x,y) ((uint32)asm_mulu32_64(x,y))
    #elif defined(ARM) && !defined(NO_ARI_ASM)
      extern_C uint64 asm_mulu32_64 (uint32 arg1, uint32 arg2); # extern in Assembler
      #define mulu32(x,y,hi_assignment,lo_assignment)  \
        { var uint64 _prod_from_mulu32 = asm_mulu32_64(x,y); \
          hi_assignment retval64_r1(_prod_from_mulu32);      \
          lo_assignment retval64_r0(_prod_from_mulu32);      \
        }
      #define mulu32_(x,y) retval64_r0(asm_mulu32_64(x,y))
    #else
      #define mulu32(x,y,hi_assignment,lo_assignment)  \
        { lo_assignment mulu32_(x,y); hi_assignment mulu32_high; }
      #if defined(MIPS) && !defined(NO_ARI_ASM)
        extern_C uint32 asm_mulu32_ (uint32 arg1, uint32 arg2, uint32* hi_ptr); # extern in Assembler
        #define mulu32_(x,y) asm_mulu32_(x,y,&mulu32_high)
        #if defined(LISPARIT)
          global uint32 mulu32_high;
        #endif
      #else
        extern_C uint32 mulu32_ (uint32 arg1, uint32 arg2);
        #ifdef LISPARIT
        global uint32 mulu32_high;
        global uint32 mulu32_ (uint32 x, uint32 y) {
          var uint16 x1 = high16(x);
          var uint16 x0 = low16(x);
          var uint16 y1 = high16(y);
          var uint16 y0 = low16(y);
          var uint32 hi = mulu16(x1,y1); # obere Portion
          var uint32 lo = mulu16(x0,y0); # untere Portion
          {
            var uint32 mid = mulu16(x0,y1); # 1. mittlere Portion
            hi += high16(mid); mid = highlow32_0(low16(mid));
            lo += mid; if (lo < mid) { hi += 1; } # 64-Bit-Addition
          }
          {
            var uint32 mid = mulu16(x1,y0); # 2. mittlere Portion
            hi += high16(mid); mid = highlow32_0(low16(mid));
            lo += mid; if (lo < mid) { hi += 1; } # 64-Bit-Addition
          }
          mulu32_high = hi; return lo;
        }
        #endif
      #endif
    #endif
  #endif

# Multipliziert zwei 32-Bit-Zahlen miteinander und liefert eine 32-Bit-Zahl:
# mulu32_unchecked(arg1,arg2)
# > arg1, arg2 : zwei 32-Bit-Zahlen
# < result : eine 32-Bit-Zahl
# Es wird vorausgesetzt, dass arg1*arg2 < 2^32.
  #if defined(GNU) && defined(SPARC64) && !defined(NO_ASM)
    #define mulu32_unchecked(arg1,arg2)  \
      ({ var register uint64 _result;                         \
         __asm__("umul %1,%2,%0"                              \
                 : "=&r" (_result)                            \
                 : "r" ((uint32)(arg1)), "r" ((uint32)(arg2)) \
                );                                            \
         (uint32)_result;                                     \
       })
  #elif defined(SPARC) && !defined(NO_ARI_ASM)
    extern_C uint32 asm_mulu32_unchecked (uint32 x, uint32 y); # extern in Assembler
    #define mulu32_unchecked asm_mulu32_unchecked
  #else
    # Wir können dafür auch die Bibliotheksroutine des C-Compilers nehmen:
    #define mulu32_unchecked(x,y)  ((uint32)((uint32)(x)*(uint32)(y)))
  #endif

#if (intVsize>32)

# Multipliziert zwei 32-Bit-Zahlen miteinander und liefert eine 64-Bit-Zahl:
# mulu32_64(arg1,arg2)
# > arg1, arg2 : zwei 32-Bit-Zahlen
# < result : eine 64-Bit-Zahl
  extern_C uint64 mulu32_64 (uint32 arg1, uint32 arg2);
  #if defined(GNU) || defined(INTEL)
    #if defined(I80386) && !defined(NO_ASM)
      #define mulu32_64(x,y)  \
        ({ var register uint32 _hi;                                   \
           var register uint32 _lo;                                   \
           __asm__("mull %2"                                          \
                   : "=d" /* %edx */ (_hi), "=a" /* %eax */ (_lo)     \
                   : "rm" ((uint32)(x)), "1" /* %eax */ ((uint32)(y)) \
                  );                                                  \
           highlow64(_hi,_lo);                                        \
         })
    #elif defined(SPARC64) && !defined(NO_ASM)
      #define mulu32_64(x,y)  \
        ({ var register uint64 _hi;                            \
           var register uint64 _lo;                            \
           __asm__("umul %2,%3,%0\n\trd %%y,%1"                \
                   : "=&r" (_lo), "=r" (_hi)                   \
                   : "r" ((uint32)(x)), "r" ((uint32)(y))      \
                  );                                           \
           highlow64((uint32)_hi,(uint32)_lo);                 \
         })
    #elif defined(MIPS) && !defined(NO_ASM)
      #define mulu32_64(x,y)  \
        ({ var register uint32 _hi;                       \
           var register uint32 _lo;                       \
           __asm__("multu %3,%2 ; mfhi %0 ; mflo %1"      \
                   : "=r" (_hi), "=r" (_lo)               \
                   : "r" ((uint32)(x)), "r" ((uint32)(y)) \
                  );                                      \
           highlow64(_hi,_lo);                            \
         })
    #elif (defined(SPARC) || defined(SPARC64)) && !defined(NO_ARI_ASM)
      extern_C uint64 asm_mulu32_64 (uint32 arg1, uint32 arg2);
      #define mulu32_64 asm_mulu32_64
    #endif
  #endif
  #ifndef mulu32_64
    #define mulu32_64(x,y)  \
      ((uint64)(x) * (uint64)(y))
  #endif

#endif

# Dividiert eine 16-Bit-Zahl durch eine 16-Bit-Zahl und
# liefert einen 16-Bit-Quotienten und einen 16-Bit-Rest.
# divu_1616_1616(x,y,q=,r=);
# > uint16 x: Zähler
# > uint16 y: Nenner
# < uint16 q: floor(x/y)
# < uint16 r: x mod y
# < x = q*y+r
  #define divu_1616_1616(x,y,q_assignment,r_assignment)  \
    { var uint16 __x = (x);       \
      var uint16 __y = (y);       \
      q_assignment floor(__x,__y); \
      r_assignment (__x % __y);    \
    }

# Dividiert eine 32-Bit-Zahl durch eine 16-Bit-Zahl und
# liefert einen 16-Bit-Quotienten und einen 16-Bit-Rest.
# divu_3216_1616(x,y,q=,r=);
# > uint32 x: Zähler
# > uint16 y: Nenner
# > Es sei bekannt, dass 0 <= x < 2^16*y .
# < uint16 q: floor(x/y)
# < uint16 r: x mod y
# < x = q*y+r
  #if (defined(SPARC) || defined(SPARC64)) && !defined(NO_ARI_ASM)
    extern_C uint32 asm_divu_3216_1616_ (uint32 x, uint16 y); # -> Quotient q, Rest r
  #elif defined(ARM) && !defined(NO_ARI_ASM)
    extern_C uint64 asm_divu_3216_1616_ (uint32 x, uint16 y); # -> Quotient q, Rest r
  #else
    extern uint16 divu_16_rest;                                 # -> Rest r
  #endif
  #if defined(GNU) || defined(INTEL)
    #if defined(SPARC64) && !defined(NO_ASM)
      #define divu_3216_1616(x,y,q_assignment,r_assignment)  \
        ({var uint32 __x = (x);         \
          var uint32 __y = (uint16)(y); \
          var uint64 __q;               \
          var uint64 __r;               \
          __asm__ __volatile__ (        \
            "wr %%g0,%%g0,%%y\n\t"      \
            "udiv %2,%3,%0\n\t"         \
            "umul %0,%3,%1\n\t"         \
            "sub %2,%1,%1"              \
            : "=&r" (__q), "=&r" (__r)  \
            : "r" (__x), "r" (__y));    \
          q_assignment (uint16)__q;     \
          r_assignment (uint16)__r;     \
         })
    #elif (defined(SPARC) || defined(SPARC64)) && !defined(NO_ARI_ASM)
      #define divu_3216_1616(x,y,q_assignment,r_assignment)  \
        ({ var uint32 __qr = asm_divu_3216_1616_(x,y); # extern in Assembler \
           q_assignment low16(__qr);  \
           r_assignment high16(__qr); \
         })
    #elif defined(I80386) && !defined(NO_ASM)
      #define divu_3216_1616(x,y,q_assignment,r_assignment)  \
        ({var uint32 __x = (x);  \
          var uint16 __y = (y);  \
          var uint16 __q;        \
          var uint16 __r;        \
          __asm__("divw %4"      \
                  : "=a" /* %ax */ (__q), "=d" /* %dx */ (__r) \
                  : "1" /* %dx */ ((uint16)(high16(__x))), "0" /* %ax */ ((uint16)(low16(__x))), "rm" (__y) \
                 );              \
          q_assignment __q;       \
          r_assignment __r;       \
         })
    #elif defined(ARM) && !defined(NO_ARI_ASM)
      #define divu_3216_1616(x,y,q_assignment,r_assignment)  \
        ({ var uint64 __qr = asm_divu_3216_1616_(x,y); # extern in Assembler \
           q_assignment retval64_r0(__qr); \
           r_assignment retval64_r1(__qr); \
         })
    #elif !defined(ARM)
      #define divu_3216_1616(x,y,q_assignment,r_assignment)  \
        ({var uint32 __x = (x);            \
          var uint16 __y = (y);            \
          var uint16 __q = floor(__x,__y); \
          q_assignment __q;                 \
          r_assignment (__x - __q * __y);   \
         })
    #endif
  #endif
  #ifndef divu_3216_1616
    #if (defined(SPARC) || defined(SPARC64)) && !defined(NO_ARI_ASM)
      #define divu_3216_1616(x,y,q_assignment,r_assignment)  \
        { var uint32 __qr = asm_divu_3216_1616_(x,y); # extern in Assembler \
          q_assignment low16(__qr);  \
          r_assignment high16(__qr); \
        }
    #elif defined(ARM) && !defined(NO_ARI_ASM)
      #define divu_3216_1616(x,y,q_assignment,r_assignment)  \
        { var uint64 __qr = asm_divu_3216_1616_(x,y); # extern in Assembler \
          q_assignment retval64_r0(__qr); \
          r_assignment retval64_r1(__qr); \
        }
    #else
      #ifdef LISPARIT
      global uint16 divu_16_rest;
      #endif
      #define divu_3216_1616(x,y,q_assignment,r_assignment)  \
        { q_assignment divu_3216_1616_(x,y); r_assignment divu_16_rest; }
      extern_C uint16 divu_3216_1616_ (uint32 x, uint16 y);
      #ifdef LISPARIT
      global uint16 divu_3216_1616_ (uint32 x, uint16 y) {
        var uint16 q = floor(x,(uint32)y);
        divu_16_rest = x - (uint32)q * (uint32)y;
        return q;
      }
      #endif
    #endif
  #endif

# Dividiert eine 32-Bit-Zahl durch eine 16-Bit-Zahl und
# liefert einen 32-Bit-Quotienten und einen 16-Bit-Rest.
# divu_3216_3216(x,y,q=,r=);
# > uint32 x: Zähler
# > uint16 y: Nenner
# Es sei bekannt, dass y>0.
# < uint32 q: floor(x/y)
# < uint16 r: x mod y
# < x = q*y+r
  extern uint16 divu_16_rest;                           # -> Rest r
  #if defined(GNU) && defined(SPARC64) && !defined(NO_ASM)
    #define divu_3216_3216(x,y,q_assignment,r_assignment)  \
      ({var uint32 __x = (x);         \
        var uint32 __y = (uint16)(y); \
        var uint64 __q;               \
        var uint64 __r;               \
        __asm__ __volatile__ (        \
          "wr %%g0,%%g0,%%y\n\t"      \
          "udiv %2,%3,%0\n\t"         \
          "umul %0,%3,%1\n\t"         \
          "sub %2,%1,%1"              \
          : "=&r" (__q), "=&r" (__r)  \
          : "r" (__x), "r" (__y));    \
        q_assignment (uint32)__q;     \
        r_assignment (uint16)__r;     \
       })
  #elif defined(SPARC) || defined(SPARC64) || defined(I80386)
    #define divu_3216_3216  divu_3232_3232
  #elif 1
    # Methode: (beta = 2^16)
    # x = x1*beta+x0 schreiben.
    # Division mit Rest: x1 = q1*y + r1, wobei 0 <= x1 < beta <= beta*y.
    # Also 0 <= q1 < beta, 0 <= r1 < y.
    # Division mit Rest: (r1*beta+x0) = q0*y + r0, wobei 0 <= r1*beta+x0 < beta*y.
    # Also 0 <= q0 < beta, 0 <= r0 < y
    # und x = x1*beta+x0 = (q1*beta+q0)*y + r0.
    # Setze q := q1*beta+q0 und r := r0.
    #ifdef GNU
      #define divu_3216_3216(x,y,q_assignment,r_assignment)  \
        ({var uint32 _x = (x);            \
          var uint16 _y = (y);            \
          var uint16 _q1;                 \
          var uint16 _q0;                 \
          var uint16 _r1;                 \
          divu_3216_1616(high16(_x),_y, _q1 = , _r1 = ); \
          divu_3216_1616(highlow32(_r1,low16(_x)),_y, _q0 = , _EMA_ r_assignment); \
          q_assignment highlow32(_q1,_q0); \
         })
    #else
      #define divu_3216_3216(x,y,q_assignment,r_assignment)  \
        {var uint32 _x = (x);            \
         var uint16 _y = (y);            \
         var uint16 _q1;                 \
         var uint16 _q0;                 \
         var uint16 _r1;                 \
         divu_3216_1616(high16(_x),_y, _q1 = , _r1 = ); \
         divu_3216_1616(highlow32(_r1,low16(_x)),_y, _q0 = , _EMA_ r_assignment); \
         q_assignment highlow32(_q1,_q0); \
        }
    #endif
  #else
    # Use this function if you are not interested in the remainder:
    # extern_C uint32 divu_3216_3216_ (uint32 x, uint16 y); # -> Quotient q
    #define divu_3216_3216(x,y,q_assignment,r_assignment)  \
      { q_assignment divu_3216_3216_(x,y); r_assignment divu_16_rest; }
    #if 0 && !defined(NO_ARI_ASM)
      # asm_divu_3216_3216_ extern in Assembler
      #define divu_3216_3216_ asm_divu_3216_3216_
    #else
      extern_C uint32 divu_3216_3216_ (uint32 x, uint16 y);
      #ifdef LISPARIT
      global uint32 divu_3216_3216_ (uint32 x, uint16 y) {
        var uint16 q1;
        var uint16 q0;
        var uint16 r1;
        divu_3216_1616(high16(x),y, q1 = , r1 = );
        divu_3216_1616(highlow32(r1,low16(x)),y, q0 = , divu_16_rest =);
        return highlow32(q1,q0);
      }
      #endif
    #endif
  #endif

# Dividiert eine 32-Bit-Zahl durch eine 32-Bit-Zahl und
# liefert einen 32-Bit-Quotienten und einen 32-Bit-Rest.
# divu_3232_3232(x,y,q=,r=);
# > uint32 x: Zähler
# > uint32 y: Nenner
# Es sei bekannt, dass y>0.
# < uint32 q: floor(x/y)
# < uint32 r: x mod y
# < x = q*y+r
# Use this function if you are not interested in the remainder:
# extern_C uint32 divu_3232_3232_ (uint32 x, uint32 y); # -> Quotient q
  extern uint32 divu_32_rest;                           # -> Rest r
  #if defined(GNU) && defined(SPARC64) && !defined(NO_ASM)
    #define divu_3232_3232(x,y,q_assignment,r_assignment)  \
      ({var uint32 __x = (x);        \
        var uint32 __y = (y);        \
        var uint64 __q;              \
        var uint64 __r;              \
        __asm__ __volatile__ (       \
          "wr %%g0,%%g0,%%y\n\t"     \
          "udiv %2,%3,%0\n\t"        \
          "umul %0,%3,%1\n\t"        \
          "sub %2,%1,%1"             \
          : "=&r" (__q), "=&r" (__r) \
          : "r" (__x), "r" (__y));   \
        q_assignment (uint32)__q;     \
        r_assignment (uint32)__r;     \
       })
    #define divu_3232_3232_(x,y)  \
      ({var uint32 __x = (x);        \
        var uint32 __y = (y);        \
        var uint64 __q;              \
        __asm__ __volatile__ (       \
          "wr %%g0,%%g0,%%y\n\t"     \
          "udiv %1,%2,%0"            \
          : "=&r" (__q)              \
          : "r" (__x), "r" (__y));   \
        (uint32)__q;                 \
       })
  #elif defined(SPARC) || defined(SPARC64) || defined(I80386)
    #define divu_3232_3232(x,y,q_assignment,r_assignment)  \
      divu_6432_3232(0,x,y,_EMA_ q_assignment,_EMA_ r_assignment)
    #define divu_3232_3232_(x,y) divu_6432_3232_(0,x,y)
  #elif 1
    # Methode: (beta = 2^n = 2^16, n = 16)
    # Falls y < beta, handelt es sich um eine 32-durch-16-Bit-Division.
    # Falls y >= beta:
    # Quotient  q = floor(x/y) < beta  (da 0 <= x < beta^2, y >= beta).
    # y habe genau n+k Bits (1 <= k <= n), d.h. 2^(n+k-1) <= y < 2^(n+k).
    # Schreibe  x = 2^k*x1 + x0  mit  x1 := floor(x/2^k)
    # und       y = 2^k*y1 + y0  mit  y1 := floor(y/2^k)
    # und bilde den Näherungs-Quotienten floor(x1/y1)
    # oder (noch besser) floor(x1/(y1+1)).
    # Wegen 0 <= x1 < 2^(2n) und 0 < 2^(n-1) <= y1 < 2^n
    # und  x1/(y1+1) <= x/y < x1/(y1+1) + 2
    # (denn x1/(y1+1) = (x1*2^k)/((y1+1)*2^k) <= (x1*2^k)/y <= x/y
    # und x/y - x1/(y1+1) = (x+x*y1-x1*y)/(y*(y1+1))
    # = (x+x0*y1-x1*y0)/(y*(y1+1)) <= (x+x0*y1)/(y*(y1+1))
    # <= x/(y*(y1+1)) + x0/y
    # <= 2^(2n)/(2^(n+k-1)*(2^(n-1)+1)) + 2^k/2^(n+k-1)
    # = 2^(n-k+1)/(2^(n-1)+1) + 2^(1-n) <= 2^n/(2^(n-1)+1) + 2^(1-n) < 2 )
    # gilt  floor(x1/(y1+1)) <= floor(x/y) <= floor(x1/(y1+1)) + 2  .
    # Man bildet also  q:=floor(x1/(y1+1))  (ein Shift um n Bit oder
    # eine (2n)-durch-n-Bit-Division, mit Ergebnis q <= floor(x/y) < beta)
    # und x-q*y und muss hiervon noch höchstens 2 mal y abziehen und q
    # incrementieren, um den Quotienten  q = floor(x/y)  und den Rest
    # x-floor(x/y)*y  der Division zu bekommen.
    #define divu_3232_3232(x,y,q_assignment,r_assignment)  \
      { var uint32 _x = (x);                                            \
        var uint32 _y = (y);                                            \
        if (_y <= (uint32)(bit(16)-1))                                  \
          { var uint16 _q1;                                             \
            var uint16 _q0;                                             \
            var uint16 _r1;                                             \
            divu_3216_1616(high16(_x),_y, _q1 = , _r1 = );              \
            divu_3216_1616(highlow32(_r1,low16(_x)),_y, _q0 = , _EMA_ r_assignment); \
            q_assignment highlow32(_q1,_q0);                            \
          }                                                             \
          else                                                          \
          { var uint32 _x1 = _x; # x1 := x                              \
            var uint32 _y1 = _y; # y1 := y                              \
            var uint16 _q;                                              \
            do { _x1 = floor(_x1,2); _y1 = floor(_y1,2); } # k erhöhen  \
            while (_y1 > (uint32)(bit(16)-1)); # bis y1 < beta          \
            { var uint16 _y2 = low16(_y1)+1; # y1+1 bilden              \
              if (_y2==0)                                               \
                { _q = high16(_x1); } # y1+1=beta -> ein Shift          \
                else                                                    \
                { divu_3216_1616(_x1,_y2,_q=,); } # Division von x1 durch y1+1 \
            }                                                           \
            # _q = q = floor(x1/(y1+1))                                 \
            # x-q*y bilden (eine 16-mal-32-Bit-Multiplikation ohne Überlauf): \
            _x -= highlow32_0(mulu16(_q,high16(_y))); # q * high16(y) * beta \
            # gefahrlos, da q*high16(y) <= q*y/beta <= x/beta < beta    \
            _x -= mulu16(_q,low16(_y)); # q * low16(y)                  \
            # gefahrlos, da q*high16(y)*beta + q*low16(y) = q*y <= x    \
            # Noch höchstens 2 mal y abziehen:                          \
            if (_x >= _y)                                               \
              { _q += 1; _x -= _y;                                      \
                if (_x >= _y)                                           \
                  { _q += 1; _x -= _y;                                  \
              }   }                                                     \
            r_assignment _x;                                            \
            q_assignment (uint32)(_q);                                  \
      }   }
    #define divu_3232_3232_ divu_3232_3232_stub
    extern_C uint32 divu_3232_3232_ (uint32 x, uint32 y);
    #ifdef LISPARIT
    # Dies dient nur noch als Hilfsfunktion für arilev1.d.
    # Die Rückgabe des Restes in divu_32_rest ist also hier nicht nötig.
    global uint32 divu_3232_3232_ (uint32 x, uint32 y) {
      var uint32 q;
      divu_3232_3232(x,y,q=,unused);
      return q;
    }
    #endif
  #else
    #define divu_3232_3232(x,y,q_assignment,r_assignment)  \
      { q_assignment divu_3232_3232_(x,y); r_assignment divu_32_rest; }
    #if 0 && !defined(NO_ARI_ASM)
      # asm_divu_3232_3232_ extern in Assembler
      #define divu_3232_3232_ asm_divu_3232_3232_
    #else
      extern_C uint32 divu_3232_3232_ (uint32 x, uint32 y);
      #ifdef LISPARIT
      global uint32 divu_3232_3232_ (uint32 x, uint32 y) {
        var uint32 q = floor(x,y);
        divu_32_rest = x - q*y;
        return q;
      }
      #endif
    #endif
  #endif

# Dividiert eine 64-Bit-Zahl durch eine 32-Bit-Zahl und
# liefert einen 32-Bit-Quotienten und einen 32-Bit-Rest.
# divu_6432_3232(xhi,xlo,y,q=,r=);
# > uint32 xhi,xlo: x = 2^32*xhi+xlo = Zähler
# > uint32 y: Nenner
# > Es sei bekannt, dass 0 <= x < 2^32*y .
# < uint32 q: floor(x/y)
# < uint32 r: x mod y
# < x = q*y+r
# Use this function if you are not interested in the remainder:
# extern_C uint32 divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y); # -> Quotient q
  extern uint32 divu_32_rest;                                         # -> Rest r
  #if defined(GNU) || defined(INTEL)
    #if defined(SPARC64) && !defined(NO_ASM)
      #define divu_6432_3232(xhi,xlo,y,q_assignment,r_assignment)  \
        ({var uint32 __xhi = (xhi);    \
          var uint32 __xlo = (xlo);    \
          var uint32 __y = (y);        \
          var uint64 __q;              \
          var uint64 __r;              \
          __asm__ __volatile__ (       \
            "wr %2,%%g0,%%y\n\t"       \
            "udiv %3,%4,%0\n\t"        \
            "umul %0,%4,%1\n\t"        \
            "sub %3,%1,%1"             \
            : "=&r" (__q), "=&r" (__r) \
            : "r" (__xhi), "r" (__xlo), "r" (__y)); \
          q_assignment (uint32)__q;     \
          r_assignment (uint32)__r;     \
         })
    #elif (defined(SPARC) || defined(SPARC64)) && !defined(NO_ARI_ASM)
      extern_C uint64 asm_divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
      #define divu_6432_3232(xhi,xlo,y,q_assignment,r_assignment)  \
        ({ var uint64 _qr = asm_divu_6432_3232_(xhi,xlo,y); # extern in Assembler \
           q_assignment (uint32)_qr; r_assignment (uint32)(_qr>>32);              \
         })
      #define divu_6432_3232_(xhi,xlo,y) \
        ((uint32)asm_divu_6432_3232_(xhi,xlo,y))
    #elif defined(I80386) && !defined(NO_ASM)
      #define divu_6432_3232(xhi,xlo,y,q_assignment,r_assignment)  \
        ({var uint32 __xhi = (xhi);  \
          var uint32 __xlo = (xlo);  \
          var uint32 __y = (y);      \
          var uint32 __q;            \
          var uint32 __r;            \
          __asm__ __volatile__ (     \
             "divl %4"               \
             : "=a" /* %eax */ (__q), "=d" /* %edx */ (__r)               \
             : "1" /* %edx */ (__xhi), "0" /* %eax */ (__xlo), "rm" (__y) \
             );                      \
          q_assignment __q;           \
          r_assignment __r;           \
         })
    #elif defined(ARM) && !defined(NO_ARI_ASM)
      extern_C uint64 asm_divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
      #define divu_6432_3232(xhi,xlo,y,q_assignment,r_assignment)  \
        ({ var uint64 _qr = asm_divu_6432_3232_(xhi,xlo,y); # extern in Assembler \
           q_assignment retval64_r0(_qr); r_assignment retval64_r1(_qr);          \
         })
      #define divu_6432_3232_(xhi,xlo,y) \
        retval64_r0(asm_divu_6432_3232_(xhi,xlo,y))
    #elif defined(HAVE_LONG_LONG_INT)
      #define divu_6432_3232(xhi,xlo,y,q_assignment,r_assignment) \
        ({var uint32 __xhi = (xhi);                           \
          var uint32 __xlo = (xlo);                           \
          var uint64 __x = (uint64)__xhi<<32 | (uint64)__xlo; \
          var uint32 __y = (y);                               \
          var uint32 __q = floor(__x,(uint64)__y);            \
          q_assignment __q; r_assignment __xlo - __q * __y;     \
         })
    #endif
    #if defined(divu_6432_3232) && !defined(divu_6432_3232_)
      #define divu_6432_3232_(xhi,xlo,y) \
        ({var uint32 ___q; divu_6432_3232(xhi,xlo,y,___q=,); ___q; })
    #endif
  #endif
  #ifndef divu_6432_3232
    #define divu_6432_3232(xhi,xlo,y,q_assignment,r_assignment)  \
      { q_assignment divu_6432_3232_(xhi,xlo,y); r_assignment divu_32_rest; }
    #if (defined(I80386) || defined(SPARC) || defined(SPARC64)) && !defined(NO_ARI_ASM)
      extern_C uint64 asm_divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y); # extern in Assembler
      extern_C uint32 divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
      #ifdef LISPARIT
      global uint32 divu_32_rest;
      global uint32 divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y) {
        var uint64 qr = asm_divu_6432_3232_(xhi,xlo,y);
        divu_32_rest = (uint32)(qr>>32);
        return (uint32)qr;
      }
      #endif
    #elif defined(ARM) && !defined(NO_ARI_ASM)
      extern_C uint64 asm_divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y); # extern in Assembler
      extern_C uint32 divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
      #ifdef LISPARIT
      global uint32 divu_32_rest;
      global uint32 divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y) {
        var uint64 qr = asm_divu_6432_3232_(xhi,xlo,y);
        divu_32_rest = retval64_r1(qr);
        return retval64_r0(qr);
      }
      #endif
    #else
      extern_C uint32 divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
      #ifdef LISPARIT
      # Methode:
      # Wie UDS_divide mit intDsize=16, a_len=4, b_len=2.
      global uint32 divu_32_rest;
      global uint32 divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y) {
        if (y <= (uint32)(bit(16)-1)) {
          # 48-durch-16-Bit-Division,
          # aufgebaut aus zwei 32-durch-16-Bit-Divisionen:
          var uint16 q1;
          var uint16 q0;
          var uint16 r1;
          divu_3216_1616(highlow32(low16(xhi),high16(xlo)),y, q1=,r1=);
          divu_3216_1616(highlow32(r1,low16(xlo)),y, q0=, divu_32_rest=(uint32) );
          return highlow32(q1,q0);
        }
        # y>=2^16
        # y shiften:
        var uintL s = 0;
        while ((sint32)y >= 0) {
          y = y<<1; s++;
        }
        # x entsprechend shiften:
        if (!(s==0)) {
          xhi = (xhi << s) | (xlo >> (32-s)); xlo = xlo << s;
        }
        # 64-durch-32-Bit-Division,
        # aufgebaut aus zwei 48-durch-32-Bit-Divisionen.
        # Methode für eine 48-durch-32-Bit-Division x/y mit 0 <= x < 2^16*y :
        # (beta = 2^n = 2^16, n = 16)
        # Wir wissen beta^2/2 <= y < beta^2, Quotient  q = floor(x/y) < beta.
        # Schreibe  x = beta*x1 + x0  mit  x1 := floor(x/beta)
        # und       y = beta*y1 + y0  mit  y1 := floor(y/beta)
        # und bilde den Näherungs-Quotienten floor(x1/y1)
        # oder (noch besser) floor(x1/(y1+1)).
        # Wegen 0 <= x1 < 2^(2n) und 0 < 2^(n-1) <= y1 < 2^n
        # und  x1/(y1+1) <= x/y < x1/(y1+1) + 2
        # (denn x1/(y1+1) = (x1*beta)/((y1+1)*beta) <= (x1*beta)/y <= x/y
        # und x/y - x1/(y1+1) = (x+x*y1-x1*y)/(y*(y1+1))
        # = (x+x0*y1-x1*y0)/(y*(y1+1)) <= (x+x0*y1)/(y*(y1+1))
        # <= x/(y*(y1+1)) + x0/y = (x/y)/(y1+1) + x0/y
        # <= 2^n/(2^(n-1)+1) + 2^n/2^(2n-1) = 2^n/(2^(n-1)+1) + 2^(1-n) < 2 )
        # gilt  floor(x1/(y1+1)) <= floor(x/y) <= floor(x1/(y1+1)) + 2  .
        # Man bildet also  q:=floor(x1/(y1+1))  (ein Shift um n Bit oder
        # eine (2n)-durch-n-Bit-Division, mit Ergebnis q <= floor(x/y) < beta)
        # und x-q*y und muss hiervon noch höchstens 2 mal y abziehen und q
        # incrementieren, um den Quotienten  q = floor(x/y)  und den Rest
        # x-floor(x/y)*y  der Division zu bekommen.
        var uint16 y1_1 = high16(y)+1; # y1+1
        var uint16 q1;
        var uint16 q0;
        var uint32 r;
        # 2^16*xhi+high16(xlo) durch y dividieren:
        {
          var uint16 r16;
          var uint32 r2;
          if (y1_1==0) {
            q1 = high16(xhi); r16 = low16(xhi);
          } else {
            divu_3216_1616(xhi,y1_1, q1=,r16=);
          }
          # q1 = floor(xhi/(y1+1)), r16 = xhi - (y1+1)*q1 (>=0, <=y1)
          # Bilde r := (2^16*xhi+high16(xlo)) - y*q1
          #          = 2^16*(xhi-y1*q1) + high16(xlo) - y0*q1
          #          = 2^16*r16 + 2^16*q1 + high16(xlo) - y0*q1 (>=0)
          # Dies ist < 2^16*y1 + 2^32 <= y + 2^32 <= 3*y, kann überlaufen!
          r = highlow32(r16,high16(xlo)); # 2^16*r16 + high16(xlo) < 2^32
          r2 = highlow32_0(q1) - mulu16(low16(y),q1); # 2^16*q1 - y0*q1 < 2^32
          # 0 <= r+r2 < 3*y. Bei der Addition auf Carry testen!
          # Carry -> jedenfalls y <= r+r2 < y + 2^32 <= 3*y.
          # kein Carry -> jedenfalls 0 <= r+r2 < 2^32 <= 2*y.
          if ((r += r2) < r2) { # addieren, r >= 2^32 ?
            q1 += 1; r -= y;
          }
          # jetzt noch 0 <= r < 2^32 <= 2*y
          if (r >= y) {
            q1 += 1; r -= y;
          }
        } # Quotient q1, Rest r fertig.
        # 2^16*r+low16(xlo) durch y dividieren:
        {
          var uint16 r16;
          var uint32 r2;
          if (y1_1==0) {
            q0 = high16(r); r16 = low16(r);
          } else {
            divu_3216_1616(r,y1_1, q0=,r16=);
          }
          # q0 = floor(r/(y1+1)), r16 = r - (y1+1)*q0 (>=0, <=y1)
          # Bilde r := (2^16*r+low16(xlo)) - y*q0
          #          = 2^16*(r-y1*q0) + low16(xlo) - y0*q0
          #          = 2^16*r16 + 2^16*q0 + low16(xlo) - y0*q0 (>=0)
          # Dies ist < 2^16*y1 + 2^32 <= y + 2^32 <= 3*y, kann überlaufen!
          r = highlow32(r16,low16(xlo)); # 2^16*r16 + low16(xlo) < 2^32
          r2 = highlow32_0(q0) - mulu16(low16(y),q0); # 2^16*q0 - y0*q0 < 2^32
          # 0 <= r+r2 < 3*y. Bei der Addition auf Carry testen!
          # Carry -> jedenfalls y <= r+r2 < y + 2^32 <= 3*y.
          # kein Carry -> jedenfalls 0 <= r+r2 < 2^32 <= 2*y.
          if ((r += r2) < r2) { # addieren, r >= 2^32 ?
            q0 += 1; r -= y;
          }
          # jetzt noch 0 <= r < 2^32 <= 2*y
          if (r >= y) {
            q0 += 1; r -= y;
          }
        } # Quotient q0, Rest r fertig.
        divu_32_rest = r >> s; # Rest
        return highlow32(q1,q0); # Quotient
      }
      #endif
    #endif
  #endif

#if (intVsize>32)

# Dividiert eine 64-Bit-Zahl durch eine 32-Bit-Zahl und
# liefert einen 64-Bit-Quotienten und einen 32-Bit-Rest.
# divu_6432_6432(x,y,q=,r=);
# > uint64 x: Zähler
# > uint32 y: Nenner
# Es sei bekannt, dass y>0.
# < uint64 q: floor(x/y)
# < uint32 r: x mod y
# < x = q*y+r
  extern uint32 divu_32_rest;                           # -> Rest r
  #if 1
    # Methode: (beta = 2^32)
    # x = x1*beta+x0 schreiben.
    # Division mit Rest: x1 = q1*y + r1, wobei 0 <= x1 < beta <= beta*y.
    # Also 0 <= q1 < beta, 0 <= r1 < y.
    # Division mit Rest: (r1*beta+x0) = q0*y + r0, wobei 0 <= r1*beta+x0 < beta*y.
    # Also 0 <= q0 < beta, 0 <= r0 < y
    # und x = x1*beta+x0 = (q1*beta+q0)*y + r0.
    # Setze q := q1*beta+q0 und r := r0.
    #ifdef GNU
      #define divu_6432_6432(x,y,q_assignment,r_assignment)  \
        ({var uint64 _x = (x);            \
          var uint32 _y = (y);            \
          var uint32 _q1;                 \
          var uint32 _q0;                 \
          var uint32 _r1;                 \
          divu_6432_3232(0,high32(_x),_y, _q1 = , _r1 = ); \
          divu_6432_3232(_r1,low32(_x),_y, _q0 = , _EMA_ r_assignment); \
          q_assignment highlow64(_q1,_q0); \
         })
    #else
      #define divu_6432_6432(x,y,q_assignment,r_assignment)  \
        {var uint64 _x = (x);            \
         var uint32 _y = (y);            \
         var uint32 _q1;                 \
         var uint32 _q0;                 \
         var uint32 _r1;                 \
         divu_6432_3232(0,high32(_x),_y, _q1 = , _r1 = ); \
         divu_6432_3232(_r1,low32(_x),_y, _q0 = , _EMA_ r_assignment); \
         q_assignment highlow64(_q1,_q0); \
        }
    #endif
  #else
    #define divu_6432_6432(x,y,q_assignment,r_assignment)  \
      { q_assignment divu_6432_6432_(x,y); r_assignment divu_32_rest; }
    #if 0 && !defined(NO_ARI_ASM)
      # asm_divu_6432_6432_ extern in Assembler
      #define divu_6432_6432_ asm_divu_6432_6432_
    #else
      extern_C uint64 divu_6432_6432_ (uint64 x, uint32 y);
      #ifdef LISPARIT
      global uint64 divu_6432_6432_ (uint64 x, uint32 y) {
        var uint32 q1;
        var uint32 q0;
        var uint32 r1;
        divu_6432_3232(0,high32(x),y, q1 = , r1 = );
        divu_6432_3232(r1,low32(x),y, q0 = , divu_32_rest =);
        return highlow64(q1,q0);
      }
      #endif
    #endif
  #endif

# Dividiert eine 64-Bit-Zahl durch eine 64-Bit-Zahl und
# liefert einen 64-Bit-Quotienten und einen 64-Bit-Rest.
# divu_6464_6464(x,y,q=,r=);
# > uint64 x: Zähler
# > uint64 y: Nenner
# Es sei bekannt, dass y>0.
# < uint64 q: floor(x/y)
# < uint64 r: x mod y
# < x = q*y+r
  extern uint64 divu_64_rest;                           # -> Rest r
  #if 1
    # Methode: (beta = 2^n = 2^32, n = 32)
    # Falls y < beta, handelt es sich um eine 64-durch-32-Bit-Division.
    # Falls y >= beta:
    # Quotient  q = floor(x/y) < beta  (da 0 <= x < beta^2, y >= beta).
    # y habe genau n+k Bits (1 <= k <= n), d.h. 2^(n+k-1) <= y < 2^(n+k).
    # Schreibe  x = 2^k*x1 + x0  mit  x1 := floor(x/2^k)
    # und       y = 2^k*y1 + y0  mit  y1 := floor(y/2^k)
    # und bilde den Näherungs-Quotienten floor(x1/y1)
    # oder (noch besser) floor(x1/(y1+1)).
    # Wegen 0 <= x1 < 2^(2n) und 0 < 2^(n-1) <= y1 < 2^n
    # und  x1/(y1+1) <= x/y < x1/(y1+1) + 2
    # (denn x1/(y1+1) = (x1*2^k)/((y1+1)*2^k) <= (x1*2^k)/y <= x/y
    # und x/y - x1/(y1+1) = (x+x*y1-x1*y)/(y*(y1+1))
    # = (x+x0*y1-x1*y0)/(y*(y1+1)) <= (x+x0*y1)/(y*(y1+1))
    # <= x/(y*(y1+1)) + x0/y
    # <= 2^(2n)/(2^(n+k-1)*(2^(n-1)+1)) + 2^k/2^(n+k-1)
    # = 2^(n-k+1)/(2^(n-1)+1) + 2^(1-n) <= 2^n/(2^(n-1)+1) + 2^(1-n) < 2 )
    # gilt  floor(x1/(y1+1)) <= floor(x/y) <= floor(x1/(y1+1)) + 2  .
    # Man bildet also  q:=floor(x1/(y1+1))  (ein Shift um n Bit oder
    # eine (2n)-durch-n-Bit-Division, mit Ergebnis q <= floor(x/y) < beta)
    # und x-q*y und muss hiervon noch höchstens 2 mal y abziehen und q
    # incrementieren, um den Quotienten  q = floor(x/y)  und den Rest
    # x-floor(x/y)*y  der Division zu bekommen.
    #define divu_6464_6464(x,y,q_assignment,r_assignment)  \
      { var uint64 _x = (x);                                            \
        var uint64 _y = (y);                                            \
        if (_y <= (uint64)(((uint64)1<<32)-1))                          \
          { var uint32 _q1;                                             \
            var uint32 _q0;                                             \
            var uint32 _r1;                                             \
            divu_6432_3232(0,high32(_x),_y, _q1 = , _r1 = );            \
            divu_6432_3232(_r1,low32(_x),_y, _q0 = , _EMA_ r_assignment); \
            q_assignment highlow64(_q1,_q0);                            \
          }                                                             \
          else                                                          \
          { var uint64 _x1 = _x; # x1 := x                              \
            var uint64 _y1 = _y; # y1 := y                              \
            var uint32 _q;                                              \
            do { _x1 = floor(_x1,2); _y1 = floor(_y1,2); } # k erhöhen  \
            while (_y1 > (uint64)(((uint64)1<<32)-1)); # bis y1 < beta  \
            { var uint32 _y2 = low32(_y1)+1; # y1+1 bilden              \
              if (_y2==0)                                               \
                { _q = high32(_x1); } # y1+1=beta -> ein Shift          \
                else                                                    \
                { divu_6432_3232(high32(_x1),low32(_x1),_y2,_q=,); } # Division von x1 durch y1+1 \
            }                                                           \
            # _q = q = floor(x1/(y1+1))                                 \
            # x-q*y bilden (eine 32-mal-64-Bit-Multiplikation ohne Überlauf): \
            _x -= highlow64_0(mulu32_64(_q,high32(_y))); # q * high32(y) * beta \
            # gefahrlos, da q*high32(y) <= q*y/beta <= x/beta < beta    \
            _x -= mulu32_64(_q,low32(_y)); # q * low32(y)               \
            # gefahrlos, da q*high32(y)*beta + q*low32(y) = q*y <= x    \
            # Noch höchstens 2 mal y abziehen:                          \
            if (_x >= _y)                                               \
              { _q += 1; _x -= _y;                                      \
                if (_x >= _y)                                           \
                  { _q += 1; _x -= _y;                                  \
              }   }                                                     \
            r_assignment _x;                                            \
            q_assignment (uint64)(_q);                                  \
      }   }
  #else
    #define divu_6464_6464(x,y,q_assignment,r_assignment)  \
      { q_assignment divu_6464_6464_(x,y); r_assignment divu_64_rest; }
    #if 0 && !defined(NO_ARI_ASM)
      # asm_divu_6464_6464_ extern in Assembler
      #define divu_6464_6464_ asm_divu_6464_6464_
    #else
      extern_C uint64 divu_6464_6464_ (uint64 x, uint64 y);
      #ifdef LISPARIT
      global uint64 divu_6464_6464_ (uint64 x, uint64 y) {
        var uint64 q = floor(x,y);
        divu_64_rest = x - q*y;
        return q;
      }
      #endif
    #endif
  #endif

#endif

# Zieht die Ganzzahl-Wurzel aus einer 32-Bit-Zahl und
# liefert eine 16-Bit-Wurzel und einen Rest.
# isqrt_32_16(x,y=,sqrtp=);
# > uint32 x: Radikand, >= 2^30, < 2^32
# < uint16 y: floor(sqrt(x)), >= 2^15, < 2^16
# < bool sqrtp: /=0, falls x=y^2
  # Methode:
  # y := 2^16 als Anfangswert,
  # y := floor((y + floor(x/y))/2) als nächster Wert,
  # solange z := floor(x/y) < y, setze y := floor((y+z)/2).
  # y ist fertig; x=y^2 genau dann, wenn z=y und die letzte Division aufging.
  # (Beweis:
  #  1. Die Folge der y ist streng monoton fallend.
  #  2. Stets gilt y >= floor(sqrt(x)) (denn für alle y>0 ist
  #     y + x/y >= 2*sqrt(x) und daher  floor((y + floor(x/y))/2) =
  #     floor(y/2 + x/(2*y)) >= floor(sqrt(x)) ).
  #  3. Am Schluss gilt x >= y^2.
  # )
  #define isqrt_32_16(x,y_assignment,sqrtp_assignment)  \
    { var uint32 _x = (x);                                               \
      var uint16 _x1 = high16(_x);                                       \
      var uint16 _y = floor(_x1,2) | bit(16-1);                          \
      while (1) {                                                       \
        var uint16 _z;                                                  \
        var uint16 _r;                                                  \
        if (_x1 >= _y) # Division _x/_y ergäbe Überlauf -> _z > _y      \
          { unused (sqrtp_assignment false); break; }                   \
        divu_3216_1616(_x,_y, _z=,_r=); # Dividiere _x/_y               \
        if (_z >= _y)                                                   \
          { unused (sqrtp_assignment (_z == _y) && (_r == 0)); break; } \
        _y = floor((uint16)(_z+_y),2) | bit(16-1); # _y muss >= 2^15 bleiben \
      }                                                                 \
      y_assignment _y;                                                  \
    }

# Zieht die Ganzzahl-Wurzel aus einer 64-Bit-Zahl und
# liefert eine 32-Bit-Wurzel und einen Rest.
# isqrt_64_32(xhi,xlo,y=,sqrtp=);
# > uint32 xhi,xlo: Radikand x = 2^32*xhi+xlo, >= 2^62, < 2^64
# < uint32 y: floor(sqrt(x)), >= 2^31, < 2^32
# < bool sqrtp: /=0, falls x=y^2
  #if (defined(SPARC) || defined(SPARC64) || defined(M68K) || defined(HPPA) || defined(HPPA64))
    # Methode:
    # y := 2^32 als Anfangswert,
    # y := floor((y + floor(x/y))/2) als nächster Wert,
    # solange z := floor(x/y) < y, setze y := floor((y+z)/2).
    # y ist fertig; x=y^2 genau dann, wenn z=y und die letzte Division aufging.
    # (Beweis:
    #  1. Die Folge der y ist streng monoton fallend.
    #  2. Stets gilt y >= floor(sqrt(x)) (denn für alle y>0 ist
    #     y + x/y >= 2*sqrt(x) und daher  floor((y + floor(x/y))/2) =
    #     floor(y/2 + x/(2*y)) >= floor(sqrt(x)) ).
    #  3. Am Schluss gilt x >= y^2.
    # )
    #define isqrt_64_32(xhi,xlo,y_assignment,sqrtp_assignment)  \
      { var uint32 _xhi = (xhi);                                        \
        var uint32 _xlo = (xlo);                                        \
        var uint32 _y = floor(_xhi,2) | bit(32-1);                      \
        while (1) {                                                     \
          var uint32 _z;                                                \
          var uint32 _rest;                                             \
          if (_xhi >= _y) # Division _x/_y ergäbe Überlauf -> _z > _y   \
            { sqrtp_assignment false; break; }                          \
          divu_6432_3232(_xhi,_xlo,_y, _z=,_rest=); # Dividiere _x/_y   \
          if (_z >= _y)                                                 \
            { sqrtp_assignment (_z == _y) && (_rest == 0); break; }     \
          _y = floor(_z+_y,2) | bit(32-1); # _y muss >= 2^31 bleiben    \
        }                                                               \
        y_assignment _y;                                                \
      }
  #else
    # Methode:
    # Wie bei UDS_sqrt mit n=2.
    # y = 2^16*yhi + ylo ansetzen.
    # Dann muss
    #   yhi = floor(y/2^16) = floor(floor(sqrt(x))/2^16)
    #       = floor(sqrt(x)/2^16) = floor(sqrt(x/2^32)) = isqrt(xhi)
    # sein. Es folgt yhi >= 2^15.
    # Danach sucht man das größte ylo >=0 mit
    # x - 2^32*yhi^2 >= 2*2^16*yhi*ylo + ylo^2.
    # Dazu setzen wir  xhi*2^32+xlo := x - 2^32*yhi^2
    # (also xhi := xhi - yhi^2, das ist >=0, <=2*yhi).
    # Die Schätzung für die zweite Ziffer
    #     ylo' := min(2^16-1,floor((xhi*2^32+xlo)/(2*2^16*yhi)))
    # erfüllt ylo'-1 <= ylo <= ylo', ist also um höchstens 1 zu groß.
    # (Beweis: Rechte Ungleichung klar, da  ylo < 2^16  und
    #   xhi*2^32+xlo >= 2*2^16*yhi*ylo + ylo^2 >= 2*2^16*yhi*ylo
    #   ==> (xhi*2^32+xlo)/(2*2^16*yhi) >= ylo  gelten muss.
    #   Linke Ungleichung: Falls floor(...)>=2^16, ist
    #   xhi*2^32+xlo >= 2*2^16*2^16*yhi >= 2*2^16*yhi*(2^16-1) + 2^32
    #                >= 2*2^16*yhi*(2^16-1) + (2^16-1)^2
    #   und xhi*2^32+xlo < 2*2^16*2^16*yhi + (2^16)^2, also
    #   ylo = 2^16-1 = ylo'.
    #   Sonst ist ylo' = floor((xhi*2^32+xlo)/(2*2^16*yhi)), also
    #   xhi*2^32+xlo >= 2*2^16*yhi*ylo' >= 2*2^16*yhi*(ylo'-1) + 2^32
    #                >= 2*2^16*yhi*(ylo'-1) + (ylo'-1)^2,
    #   also ylo >= ylo'-1 nach Definition von ylo.)
    #define isqrt_64_32(xhi,xlo,y_assignment,sqrtp_assignment)  \
      { var uint32 _xhi = (xhi);                                            \
        var uint32 _xlo = (xlo);                                            \
        var uint16 _yhi;                                                    \
        var uint16 _ylo;                                                    \
        # erste Ziffer berechnen:                                           \
        isqrt_32_16(_xhi,_yhi=,); # yhi := isqrt(xhi)                       \
        _xhi -= mulu16(_yhi,_yhi); # jetzt 0 <= xhi <= 2*yhi                \
        # x = 2^32*yhi^2 + 2^32*xhi + xlo                                   \
        # Schätzung für die zweite Ziffer berechnen:                        \
        # ylo := min(2^16-1,floor((xhi*2^32+xlo)/(2*2^16*yhi))) bilden:     \
       {var uint32 _z = (_xhi << 15) | (_xlo >> 17); # < 2^15*(2*yhi+1)     \
        var uint32 _r = highlow32_0(_yhi);                                  \
        if (_z >= _r)                                                       \
          { _ylo = bit(16)-1; _r = _z - _r + (uint32)_yhi; }                \
          else                                                              \
          { divu_3216_1616(_z,_yhi, _ylo=,_r=); }                           \
        # x = 2^32*yhi^2 + 2*2^16*yhi*ylo + 2^17*r + (xlo mod 2^17),        \
        # 0 <= r < yhi + 2^15                                               \
        _xlo = (_r << 17) | (_xlo & (bit(17)-1));                           \
        # x = 2^32*yhi^2 + 2*2^16*yhi*ylo + 2^32*floor(r/2^15) + xlo        \
        _z = mulu16(_ylo,_ylo); # z = ylo^2                                 \
        # Versuche vom Rest 2^32*floor(r/2^15) + xlo  z zu subtrahieren.    \
        # Falls Rest >= z (d.h. r>=2^15 oder xlo>=z), ist ylo fertig,       \
        # und es gilt x=y^2 genau dann, wenn r<2^15 und xlo=z.              \
        # Sonst (d.h. r<2^15 und xlo<z), muss man ylo erniedrigen. Dazu     \
        # setzt man  ylo := ylo-1, z := z-(2*ylo+1),                        \
        # Rest := Rest + 2^17*yhi = xlo + 2^17*yhi >= 2^32 > z, also x>y^2. \
        if (_r < bit(15))                                                   \
          { if (_xlo < _z)                                                  \
              { _ylo -= 1; sqrtp_assignment false; }                         \
              else                                                          \
              { sqrtp_assignment (_xlo == _z); }                             \
          }                                                                 \
          else                                                              \
          { sqrtp_assignment false; }                                        \
        y_assignment highlow32(_yhi,_ylo);                                   \
      }}
  #endif

# Eine 32-Bit-Zahl aus zwei aufeinanderfolgenden 16-Bit-Digits einer UDS
# zusammensetzen: highlow32_at(ptr)
  #if BIG_ENDIAN_P && defined(M68K)
    # ptr als 32-Bit-Pointer auffassen und darauf zugreifen
    #define highlow32_at(ptr)  (*(uint32*)(ptr))
  #else
    #define highlow32_at(ptr)  highlow32(((uint16*)(ptr))[0],((uint16*)(ptr))[1])
  #endif

# Eine 32-Bit-Zahl in zwei aufeinanderfolgende 16-Bit-Digits einer UDS abspeichern:
# set_highlow32_at(ptr,value32); wobei ptr und value32 Variablen.
  #if BIG_ENDIAN_P && defined(M68K)
    # ptr als 32-Bit-Pointer auffassen und darauf zugreifen
    #define set_highlow32_at(ptr,value32)  (*(uint32*)(ptr)=(value32))
  #else
    #define set_highlow32_at(ptr,value32)  (((uint16*)(ptr))[0]=high16(value32),((uint16*)(ptr))[1]=low16(value32))
  #endif

