# Aus der Speicherverwaltung ausgelagert:
# Tabelle aller festen Symbole
# Bruno Haible 1990-2001

#include "lispbibl.c"

#undef LISPSYM

# Noch so ein Macro-Problem. Grr...
  #undef inline
# Cygwin32 macht "#define listen ...". Grr...
  #undef listen
# WIN32.D macht "#define read" und "#define write"
  #undef read
  #undef write
# LISPBIBL.D does "#define export"
  #undef export

# Tabelle aller festen Symbole:
  global struct symbol_tab_ symbol_tab_data
    #if defined(INIT_SYMBOL_TAB) && NIL_IS_CONSTANT
    = {
        #define LISPSYM  LISPSYM_B
        #include "constsym.c"
        #undef LISPSYM
      }
    #endif
    ;

