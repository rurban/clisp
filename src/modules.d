# Module für CLISP
# Bruno Haible 1994-1999

# Alle Abhängigkeiten von modules.h werden hier gesammelt!


#ifdef NO_CLISP_H
  #include "lispbibl.c"
#else
  #include "clisp.h"
#endif

# Anzahl dazugelinkter Module

global var uintC module_count =
  #define MODULE(module_name)  1+
  #include "modules.h"
  #undef MODULE
  0;

# Tabelle der Module:
  extern uintC subr_tab_data_size;
  extern uintC object_tab_size;
  #define MODULE(module_name)  \
    extern subr_t module__##module_name##__subr_tab[]; \
    extern uintC module__##module_name##__subr_tab_size; \
    extern object module__##module_name##__object_tab[]; \
    extern uintC module__##module_name##__object_tab_size; \
    extern subr_initdata_t module__##module_name##__subr_tab_initdata[]; \
    extern object_initdata_t module__##module_name##__object_tab_initdata[]; \
    extern void module__##module_name##__init_function_1(struct module_t *); \
    extern void module__##module_name##__init_function_2(struct module_t *);
  #include "modules.h"
  #undef MODULE
  #ifdef DYNAMIC_MODULES
    #define _NEXT_NULL  , NULL
  #else
    #define _NEXT_NULL
  #endif
  global module_t modules[] = {
    { "clisp",
      (subr_t*)&subr_tab_data, &subr_tab_data_size,
      (object*)&object_tab, &object_tab_size,
      true, NULL, NULL, NULL, NULL
      _NEXT_NULL
    },
    #define MODULE(module_name)  \
      { STRING(module_name), \
        &module__##module_name##__subr_tab[0], &module__##module_name##__subr_tab_size, \
        &module__##module_name##__object_tab[0], &module__##module_name##__object_tab_size, \
        false, \
        &module__##module_name##__subr_tab_initdata[0], \
        &module__##module_name##__object_tab_initdata[0], \
        &module__##module_name##__init_function_1, \
        &module__##module_name##__init_function_2 \
        _NEXT_NULL \
      },
    #include "modules.h"
    #undef MODULE
    { NULL, NULL, NULL, NULL, NULL, false, NULL, NULL, NULL, NULL _NEXT_NULL }
    };

