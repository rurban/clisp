/*
 * Information about the build environment
 * Bruno Haible 2004-2005
 * Sam Steingold 2004-2005
 */

#include "lispbibl.c"

#include "cflags.h"

/* Returns a multiline string containing some info about the flags with which
   the executable was built. */
global object built_flags (void) {
  var const char * part1 =
    /* string concatenation done by the C compiler */
    CC
    " "CFLAGS
    " "CLFLAGS
    " "LIBS
    " "X_LIBS"\n"
    "SAFETY=" STRINGIFY(SAFETY)
  #ifdef TYPECODES
    " TYPECODES"
  #endif
  #ifdef HEAPCODES
    " HEAPCODES"
   #ifdef STANDARD_HEAPCODES
    " STANDARD_HEAPCODES"
   #endif
   #ifdef LINUX_NOEXEC_HEAPCODES
    " LINUX_NOEXEC_HEAPCODES"
   #endif
  #endif
  #ifdef WIDE
    " WIDE"
  #endif
  #ifdef GENERATIONAL_GC
    " GENERATIONAL_GC"
  #endif
  #ifdef SPVW_BLOCKS
    " SPVW_BLOCKS"
  #endif
  #ifdef SPVW_PAGES
    " SPVW_PAGES"
  #endif
  #ifdef SPVW_MIXED
    " SPVW_MIXED"
  #endif
  #ifdef SPVW_PURE
    " SPVW_PURE"
  #endif
  #ifdef MULTIMAP_MEMORY
    " MULTIMAP_MEMORY"
   #ifdef MULTIMAP_MEMORY_VIA_SHM
    " MULTIMAP_MEMORY_VIA_SHM"
   #endif
   #ifdef MULTIMAP_MEMORY_VIA_FILE
    " MULTIMAP_MEMORY_VIA_FILE"
   #endif
   #endif
  #ifdef SINGLEMAP_MEMORY
    " SINGLEMAP_MEMORY"
  #endif
  #ifdef TRIVIALMAP_MEMORY
    " TRIVIALMAP_MEMORY"
  #endif
    ;
  var object result = ascii_to_string(part1);
 #ifdef LIBSIGSEGV_VERSION
  var char libsigsegv_ver[BUFSIZ];
  sprintf(libsigsegv_ver, "\nlibsigsegv %d.%d",
          LIBSIGSEGV_VERSION >> 8, LIBSIGSEGV_VERSION & 0xff);
  pushSTACK(result); pushSTACK(ascii_to_string(libsigsegv_ver));
  result = string_concat(2);
 #endif
 #ifdef _LIBICONV_VERSION
  var char libiconv_ver[BUFSIZ];
  sprintf(libiconv_ver, "\nlibiconv %d.%d",
          _LIBICONV_VERSION >> 8, _LIBICONV_VERSION & 0xff);
  pushSTACK(result); pushSTACK(ascii_to_string(libiconv_ver));
  result = string_concat(2);
 #endif
  return result;
}
