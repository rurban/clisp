# Moved out of memory management:
# table of all SUBRs
# Bruno Haible 1990-1999

#include "lispbibl.c"

#undef LISPFUN

# table of all SUBRs
global struct subr_tab_ subr_tab_data
    #if defined(INIT_SUBR_TAB)
    = {
        #if NIL_IS_CONSTANT
          #define LISPFUN  LISPFUN_G
        #else
          #define LISPFUN  LISPFUN_F
        #endif
        #include "subr.c"
        #undef LISPFUN
      }
    #endif
    ;
global uintC subr_tab_data_size = sizeof(subr_tab_data)/sizeof(subr_t);

