# Moved out of memory management:
# table of all fixed symbols
# Bruno Haible 1990-2001

#include "lispbibl.c"

#undef LISPSYM

# Yet another macro-problem. Grr...
  #undef inline
# Cygwin32 does "#define listen ...". Grr...
  #undef listen
# WIN32.D does "#define read" and "#define write"
  #undef read
  #undef write
# LISPBIBL.D does "#define export"
  #undef export

# Table of all fixed symbols:
global struct symbol_tab_ symbol_tab_data
    #if defined(INIT_SYMBOL_TAB) && NIL_IS_CONSTANT
    = {
        #define LISPSYM  LISPSYM_B
        #include "constsym.c"
        #undef LISPSYM
      }
    #endif
    ;

