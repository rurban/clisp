# Aus der Speicherverwaltung ausgelagert:
# Tabelle aller festen Objekte
# Bruno Haible 10.7.1994

#include "lispbibl.c"

#undef LISPOBJ

# Tabelle aller festen Objekte:
  global struct object_tab_ object_tab
    #if defined(INIT_OBJECT_TAB) && NIL_IS_CONSTANT
    = {
        #define LISPOBJ LISPOBJ_B
        #include "constobj.c"
        #undef LISPOBJ
      }
    #endif
    ;
  global uintC object_tab_size = sizeof(object_tab)/sizeof(object);

