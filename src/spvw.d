# Speicherverwaltung für CLISP
# Bruno Haible 6.12.1997

# Inhalt:
# Modulverwaltung
# Debug-Hilfen
# Speichergröße
# Speicherlängenbestimmung
# Page Fault and Protection handling
# Garbage Collection
# Speicherbereitstellungsfunktionen
# Zirkularitätenfeststellung
# Speicher durchlaufen
# elementare Stringfunktionen
# andere globale Hilfsfunktionen
# Initialisierung
# Speichern und Laden von MEM-Files
# Dynamisches Laden von Modulen
# Version

#include "lispbibl.c"
#include "aridecl.c" # für NUM_STACK

#include "version.h" # für O(lisp_implementation_version_{month,year}_string)

#if defined(UNIX_LINUX) && (defined(FAST_FLOAT) || defined(FAST_DOUBLE)) && defined(HAVE_SETFPUCW)
  #include <fpu_control.h>
#endif

#ifdef MULTITHREAD
  #ifdef HAVE_MEMSET
    #include <string.h>
    extern_C RETMEMSETTYPE memset (void* ptr, int c, size_t len); # siehe MEMORY(3)
    #define bzero(ptr,len)  memset(ptr,0,len)
    #define bcopy(source,dest,len)  memcpy(dest,source,len)
  #else
    extern_C void bzero (void* ptr, int len); # siehe BZERO(3)
    extern_C void bcopy (void* source, void* dest, int len);
  #endif
#endif

# In diesem File haben die Tabellenmacros eine andere Verwendung:
  #undef LISPSPECFORM
  #undef LISPFUN
  #undef LISPSYM
  #undef LISPOBJ

# Tabelle aller SUBRs: ausgelagert nach SPVWTABF
# Größe dieser Tabelle:
  #define subr_anz  (sizeof(subr_tab)/sizeof(subr_))

# Tabelle aller FSUBRs: ausgelagert nach CONTROL
# Größe dieser Tabelle:
  #define fsubr_anz  (sizeof(fsubr_tab)/sizeof(fsubr_))

# Tabelle aller Pseudofunktionen: ausgelagert nach STREAM
# Größe dieser Tabelle:
  #define pseudofun_anz  (sizeof(pseudofun_tab)/sizeof(Pseudofun))

# Tabelle aller festen Symbole: ausgelagert nach SPVWTABS
# Größe dieser Tabelle:
  #define symbol_anz  (sizeof(symbol_tab)/sizeof(symbol_))

# Tabelle aller sonstigen festen Objekte: ausgelagert nach SPVWTABO
# Größe dieser Tabelle:
  #define object_anz  (sizeof(object_tab)/sizeof(object))

# Durchlaufen durch subr_tab:
# (NB: subr_tab_ptr_as_object(ptr) wandelt einen durchlaufenden Pointer
# in ein echtes Lisp-Objekt um.)
  #ifdef MAP_MEMORY_TABLES
    local uintC total_subr_anz;
    #define for_all_subrs(statement)  \
      { var subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen \
        var uintC count;                                          \
        dotimesC(count,total_subr_anz, { statement; ptr++; } );   \
      }
  #else
    #define for_all_subrs(statement)  \
      { var module_* module; # modules durchgehen                           \
        for_modules(all_modules,                                            \
          { if (module->initialized)                                        \
              { var subr_* ptr = module->stab;                              \
                var uintC count;                                            \
                dotimesC(count,*module->stab_size, { statement; ptr++; } ); \
              });                                                           \
      }   }
  #endif

# Beim Durchlaufen durch symbol_tab:
# Wandelt einen durchlaufenden Pointer in ein echtes Lisp-Objekt um.
  #ifdef MAP_MEMORY_TABLES
    #define symbol_tab_ptr_as_object(ptr)  as_object((oint)(ptr))
  #else
    #ifdef TYPECODES
      #define symbol_tab_ptr_as_object(ptr)  type_pointer_object(symbol_type,ptr)
    #else
      #define symbol_tab_ptr_as_object(ptr)  as_object((oint)(ptr)+varobject_bias)
    #endif
  #endif
# Durchlaufen durch symbol_tab:
  #define for_all_constsyms(statement)  \
    { var symbol_* ptr = (symbol_*)&symbol_tab; # symbol_tab durchgehen \
      var uintC count;                                                  \
      dotimesC(count,symbol_anz, { statement; ptr++; } );               \
    }

# Durchlaufen durch object_tab:
  #define for_all_constobjs(statement)  \
    { var module_* module; # modules durchgehen                               \
      for_modules(all_modules,                                                \
        { if (module->initialized)                                            \
            { var object* objptr = module->otab; # object_tab durchgehen      \
              var uintC count;                                                \
              dotimesC(count,*module->otab_size, { statement; objptr++; } );  \
            });                                                               \
    }   }

# Semaphoren: entscheiden, ob eine Unterbrechung unwirksam (/=0) oder
# wirksam (alle = 0) ist.
# Werden mit set_break_sem_x gesetzt und mit clr_break_sem_x wieder gelöscht.
  global break_sems_ break_sems;
  # break_sem_0 == break_sems.einzeln[0]
  #   gesetzt, solange eine Page-Fault-Behandlung im Gange ist
  # break_sem_1 == break_sems.einzeln[1]
  #   gesetzt, solange die Speicherverwaltung eine Unterbrechung verbietet
  #   (damit leerer Speicher nicht von der GC durchlaufen werden kann)
  # break_sem_2 == break_sems.einzeln[2]
  #   für Package-Verwaltung auf unterem Niveau und Hashtable-Verwaltung
  # break_sem_3 == break_sems.einzeln[3]
  #   für Package-Verwaltung auf höherem Niveau
  # break_sem_4 == break_sems.einzeln[4]
  #   gesetzt, solange (AMIGAOS) DOS oder externe Funktionen aufgerufen werden.
  # break_sem_5 == break_sems.einzeln[5]
  #   gesetzt, solange (UNIX) ein Signal-Handler aufgerufen wird.

# ------------------------------------------------------------------------------
#                          Modulverwaltung

#include "spvw_module.c"

# ------------------------------------------------------------------------------
#                            Debug-Hilfen

#include "spvw_debug.c"

# ------------------------------------------------------------------------------
#                          Eigenes alloca()

#include "spvw_alloca.c"

# ------------------------------------------------------------------------------
#                         Schnelles Programm-Ende

# jmp_buf zur Rückkehr zum Original-Wert des SP beim Programmstart:
  local jmp_buf original_context;

# LISP sofort verlassen:
# quit_sofort(exitcode);
# > exitcode: 0 bei normalem, 1 bei abnormalem Programmende
  # Wir müssen den SP auf den ursprünglichen Wert setzen.
  # (Bei manchen Betriebssystemen wird erst der vom Programm belegte
  # Speicher mit free() zurückgegeben, bevor ihm die Kontrolle entzogen
  # wird. Für diese kurze Zeit muß man den SP vernünftig setzen.)
  local int exitcode;
  #define quit_sofort(xcode)  exitcode = xcode; longjmp(&!original_context,1)

# ------------------------------------------------------------------------------
#                         Speicherverwaltung allgemein

/*

Overview over CLISP's garbage collection
----------------------------------------

Knowing that most malloc() implementations are buggy and/or slow, and
because CLISP needs to perform garbage collection, CLISP has its own memory
management subsystem in spvw.d.

Three kinds of storage are distinguished:
  * Lisp data (the "heap"), i.e. storage which contains Lisp objects and
    is managed by the garbage collector.
  * Lisp stack (called STACK), contains Lisp objects,
  * C data (including program text, data, malloc()ed memory).

A Lisp object is one word, containing a tag (partial type information)
and either immediate data (e.g. fixnums or short floats) or a pointer
to storage. Pointers to C data have tag = machine_type = 0, pointers to
Lisp stack have tag = system_type, most other pointers point to Lisp data.

Let's turn to these Lisp objects that consume regular Lisp memory.
Every Lisp object has a size which is determined when the object is
allocated (using one of the allocate_... routines). The size can be
computed from the type tag and - if necessary - the length field of
the object's header. The length field always contains the number of
elements of the object. The number of bytes is given by the function
objsize().

Lisp objects which contain exactly 2 Lisp objects (i.e. conses, complex
numbers, ratios) are stored in a separate area and occupy 2 words each.
All other Lisp objects have "varying length" (well, more precisely,
not a fixed length) and include a word for garbage collection purposes
at their beginning.

The garbage collector is invoked when an allocate_...() request
cannot be fulfilled. It marks all objects which are "live" (may be
reached from the "roots"), compacts these objects and unmarks them.
Non-live objects are lost; their storage is reclaimed.

2-pointer objects are compacted by a simple hole-filling algorithm:
fill the most-left object into the most-right hole, and so on, until
the objects are contiguous at the right and the hole is contiguous at the
left.

Variable-length objects are compacted by sliding them down (their address
decreases).

There are 6 memory models. Which one is used, depends on the operating system.

SPVW_MIXED_BLOCKS_OPPOSITE: The heap consists of one block of fixed length
(allocated at startup). The variable-length objects are allocated from
the left, the 2-pointer objects are allocated from the right. There is a
hole between them. When the hole shrinks to 0, GC is invoked. GC slides
the variable-length objects to the left and concentrates the 2-pointer
objects at the right end of the block again.
When no more room is available, some reserve area beyond the right end
of the block is halved, and the 2-pointer objects are moved to the right
accordingly.
(+) Simple management.
(+) No fragmentation at all.
(-) The total heap size is limited.

SPVW_MIXED_BLOCKS_OPPOSITE && TRIVIALMAP_MEMORY: The heap consists of two
big blocks, one for variable-length objects and one for 2-pointer objects.
The first one has a hole to the right and is extensible to the right, the
latter one has a hole to the left and is extensible to the left. Similar
to the previous model, except that the hole is unmapped.
(+) Total heap size grows depending on the application's needs.
(+) No fragmentation at all.
(*) Works only when SINGLEMAP_MEMORY were possible as well.

SPVW_MIXED_BLOCKS_STAGGERED && TRIVIALMAP_MEMORY: The heap consists of two
big blocks, one for variable-length objects and one for 2-pointer objects.
Both have a hole to the right, but are extensible to the right.
(+) Total heap size grows depending on the application's needs.
(+) No fragmentation at all.
(*) Works only when SINGLEMAP_MEMORY were possible as well.

SPVW_MIXED_PAGES: The heap consists of many small pages (usually around
8 KB). There are two kinds of pages: one for 2-pointer objects, one for
variable-length objects. The set of all pages of a fixed kind is called
a "Heap". Each page has its hole (free space) at its end. For every heap,
the pages are kept sorted according to the size of their hole, using AVL
trees. Garbage collection is invoked when the used space has grown by
25% since the last GC; until that point new pages are allocated from
the operating system. The GC compacts the data in each page separately:
data is moved to the left. Emptied pages are given back to the OS.
If the holes then make up more than 25% of the occupied storage, a second
GC turn moves objects across pages, from nearly empty ones to nearly full
ones, with the aim to free as most pages as possible.

(-) every allocation requires AVL tree operations -> slower
(+) Total heap size grows depending on the application's needs.
(+) Works on operating systems which don't provide large contiguous areas.

SPVW_PURE_PAGES: Just like SPVW_MIXED_PAGES, except that every page contains
data of only a single type tag, i.e. there is a Heap for every type tag.

(-) every allocation requires AVL tree operations -> slower
(+) Total heap size grows depending on the application's needs.
(+) Works on operating systems which don't provide large contiguous areas.
(-) More fragmentation because objects of different type never fit into
    the same page.

SPVW_PURE_BLOCKS: There is a big block of storage for each type tag.
Each of these blocks has its data to the left and the hole to the right,
but these blocks are extensible to the right (because there's enough room
between them). A garbage collection is triggered when the allocation amount
since the last GC reaches 50% of the amount of used space at the last GC,
but at least 512 KB. The garbage collection cleans up each block separately:
data is moved left.

(+) Total heap size grows depending on the application's needs.
(+) No 16 MB total size limit.
(*) Works only in combination with SINGLEMAP_MEMORY.


The following combinations of memory model and mmap tricks are possible:

With TYPECODES:
                       GENERATIONAL_GC -------------+
                                                     \
                    MULTIMAP_MEMORY -------------+    \
                  SINGLEMAP_MEMORY -----------+   \    \
                TRIVIALMAP_MEMORY ---------+   \   \    \
               no MAP_MEMORY -----------+   \   \   \    \
                                         \   \   \   \    \
SPVW_MIXED_BLOCKS_OPPOSITE              | X | X |   | X | X |
SPVW_MIXED_BLOCKS_STAGGERED             |   | X |   |   | X |
SPVW_PURE_BLOCKS                        |   |   | X |   | X |
SPVW_MIXED_PAGES                        | X |   |   |   |   |
SPVW_PURE_PAGES                         | X |   |   |   |   |

Historically, the different memory models were developed in the following
order (1 = first, ...):
SPVW_MIXED_BLOCKS_OPPOSITE              | 1 |10 |   | 2 | 9 |
SPVW_MIXED_BLOCKS_STAGGERED             |   | 7 |   |   | 8 |
SPVW_PURE_BLOCKS                        |   |   | 5 |   | 6 |
SPVW_MIXED_PAGES                        | 3 |   |   |   |   |
SPVW_PURE_PAGES                         | 4 |   |   |   |   |

With no TYPECODES:
                   GENERATIONAL_GC ----------+
                                              \
                TRIVIALMAP_MEMORY ---------+   \
               no MAP_MEMORY -----------+   \   \
                                         \   \   \
SPVW_MIXED_BLOCKS_OPPOSITE              | X | X | X |
SPVW_MIXED_BLOCKS_STAGGERED             |   | X | X |
SPVW_MIXED_PAGES                        | X |   |   |


The burden of GC upon the rest of CLISP:

Every subroutine marked with "kann GC auslösen" may invoke GC. GC moves
all the Lisp objects and updates the pointers. But the GC looks only
on the STACK and not in the C variables. (Anything else wouldn't be portable.)
Therefore at every "unsafe" point - i.e. every call to such a subroutine -
all the C variables of type `object' MUST BE ASSUMED TO BECOME GARBAGE.
(Except for `object's that are known to be unmovable, e.g. immediate data
or Subrs.) Pointers inside Lisp data (e.g. to the characters of a string or
to the elements of a simple-vector) become INVALID as well.

The workaround is usually to allocate all the needed Lisp data first and
do the rest of the computation with C variables, without calling unsafe
routines, and without worrying about GC.


Foreign Pointers
----------------

Pointers to C functions and to malloc()ed data can be hidden in Lisp
objects of type machine_type; GC will not modify its value. But one should
not dare to assume that a C stack pointer or the address of a C function
in a shared library fulfills the same requirements.

If another pointer is to be viewed as a Lisp object, it is best to box it,
e.g. in a simple-bit-vector or in an Fpointer. (See allocate_fpointer().)

*/


# Methode der Speicherverwaltung:
#if defined(SPVW_BLOCKS) && defined(SPVW_MIXED) # z.B. ATARI
  #define SPVW_MIXED_BLOCKS
  #if !defined(TRIVIALMAP_MEMORY)
    # Blocks grow like this:         |******-->     <--****|
    #define SPVW_MIXED_BLOCKS_OPPOSITE
  #else # defined(TRIVIALMAP_MEMORY)
    #if !defined(WIDE_SOFT) && !defined(SELFMADE_MMAP)
      # Blocks grow like this:       |******-->     <--****|
      #define SPVW_MIXED_BLOCKS_OPPOSITE
    #else
      # Blocks grow like this:       |******-->      |***-->
      #define SPVW_MIXED_BLOCKS_STAGGERED
    #endif
  #endif
#endif
#if defined(SPVW_BLOCKS) && defined(SPVW_PURE) # z.B. UNIX_LINUX ab Linux 0.99.7
  #define SPVW_PURE_BLOCKS
#endif
#if defined(SPVW_PAGES) && defined(SPVW_MIXED) # z.B. SUN3, AMIGA, HP9000_800
  #define SPVW_MIXED_PAGES
#endif
#if defined(SPVW_PAGES) && defined(SPVW_PURE) # z.B. SUN4, SUN386
  #define SPVW_PURE_PAGES
#endif

# Kanonische Adressen:
# Bei MULTIMAP_MEMORY kann man über verschiedene Pointer auf dieselbe Speicher-
# stelle zugreifen. Die Verwaltung der Heaps benötigt einen "kanonischen"
# Pointer. Über diesen kann zugegriffen werden, und er kann mit >=, <=
# verglichen werden. heap_start und heap_end sind kanonische Adressen.
  #ifdef MULTIMAP_MEMORY
    #define canonaddr(obj)  upointer(obj)
    #define canon(address)  ((address) & oint_addr_mask)
  #else
    #define canonaddr(obj)  (aint)ThePointer(obj)
    #define canon(address)  (address)
  #endif
  # Es gilt canonaddr(obj) == canon((aint)ThePointer(obj)).

# ------------------------------------------------------------------------------
#                          Page-Allozierung

#if defined(SINGLEMAP_MEMORY) || defined(TRIVIALMAP_MEMORY) || defined(MULTITHREAD)

#include "spvw_mmap.c"

#endif # SINGLEMAP_MEMORY || TRIVIALMAP_MEMORY || MULTITHREAD

#ifdef MULTIMAP_MEMORY

#include "spvw_multimap.c"

#if defined(MAP_MEMORY_TABLES)
  # symbol_tab is multimapped, always at the same address. This speeds up
  # loadmem() a little.
  #define MULTIMAP_MEMORY_SYMBOL_TAB
#endif

#endif # MULTIMAP_MEMORY

#if defined(SINGLEMAP_MEMORY) || defined(TRIVIALMAP_MEMORY)

#include "spvw_singlemap.c"

#if defined(SINGLEMAP_MEMORY) && defined(HAVE_WIN32_VM)
  # Despite of SINGLEMAP_MEMORY, a relocation may be necessary at loadmem() time.
  #define SINGLEMAP_MEMORY_RELOCATE
#endif

#endif # SINGLEMAP_MEMORY || TRIVIALMAP_MEMORY

# ------------------------------------------------------------------------------
#                           Multithreading

#ifndef MULTITHREAD

  # Global variables.

  # Der STACK:
    #if !defined(STACK_register)
      global object* STACK;
    #endif
    #ifdef HAVE_SAVED_STACK
      global object* saved_STACK;
    #endif

  # MULTIPLE-VALUE-SPACE:
    #if !defined(mv_count_register)
      global uintC mv_count;
    #endif
    #ifdef NEED_temp_mv_count
      global uintC temp_mv_count;
    #endif
    #ifdef HAVE_SAVED_mv_count
      global uintC saved_mv_count;
    #endif
    global object mv_space [mv_limit-1];
    #ifdef NEED_temp_value1
      global object temp_value1;
    #endif
    #ifdef HAVE_SAVED_value1
      global object saved_value1;
    #endif

  # Während der Ausführung eines SUBR, FSUBR: das aktuelle SUBR bzw. FSUBR
    #if !defined(subr_self_register)
      global object subr_self;
    #endif
    #ifdef HAVE_SAVED_subr_self
      global object saved_subr_self;
    #endif

  # Während Callbacks die geretteten Register:
    #if defined(HAVE_SAVED_REGISTERS)
      global struct registers * callback_saved_registers = NULL;
    #endif

  # Stack-Grenzen:
  #ifndef NO_SP_CHECK
    global void* SP_bound;  # SP-Wachstumsgrenze
  #endif
  global void* STACK_bound; # STACK-Wachstumsgrenze

  # Das lexikalische Environment:
    global environment aktenv;

  global unwind_protect_caller unwind_protect_to_save;

  # Variablen zur Übergabe von Information an den Beginn des Handlers:
  global handler_args_t handler_args;

  # Da immer nur ganze Bereiche von Handlers deaktiviert und wieder aktiviert
  # werden, behandeln wir die Handler beim Deaktivieren nicht einzeln, sondern
  # führen eine Liste der STACK-Bereiche, in denen die Handler deaktiviert sind.
  global stack_range* inactive_handlers = NULL;
  # Ein Handler gilt genau dann als inaktiv, wenn für einen der in
  # inactive_handlers aufgeführten Bereiche gilt:
  # low_limit <= handler < high_limit.

  #define for_all_threadobjs(statement)  \
    { var object* objptr = (object*)&aktenv;              \
      var uintC count;                                    \
      dotimespC(count,sizeof(environment)/sizeof(object), \
        { statement; objptr++; });                        \
    }

  #define for_all_STACKs(statement)         \
    { var object* objptr = &STACK_0;        \
      { statement; }                        \
    }

#else

  # Mutex protecting the set of threads.
    local xmutex_t allthreads_lock;

  # Set of threads.
    #define MAXNTHREADS  128
    local uintC nthreads = 0;
    local thread_* allthreads[MAXNTHREADS];

  # Number of symbol values currently in use in every thread.
    local uintL num_symvalues = 0;
  # Maximum number of symbol values in every thread before new thread-local
  # storage must be added.
  # = floor(round_up(thread_size(num_symvalues),mmap_pagesize)-offsetofa(thread_,_symvalues),sizeof(object))
    local uintL maxnum_symvalues;

  # Initialization.
  local void init_multithread (void);
  local void init_multithread()
    { xthread_init();
      xmutex_init(&allthreads_lock);
      maxnum_symvalues = floor(((thread_size(0)+mmap_pagesize-1)&-mmap_pagesize)-offsetofa(thread_,_symvalues),sizeof(object));
    }

  # Create a new thread.
  local thread_* create_thread (void* sp);
  local thread_* create_thread(sp)
    var void* sp;
    { var thread_* thread;
      xmutex_lock(&allthreads_lock);
      if (nthreads >= MAXNTHREADS) { thread = NULL; goto done; }
      thread = sp_to_thread(sp);
      if (mmap_zeromap(thread,(thread_size(num_symvalues)+mmap_pagesize-1)&-mmap_pagesize) < 0) { thread = NULL; goto done; }
      thread->_index = nthreads;
      { var object* objptr = (object*)((uintP)thread+thread_objects_offset(num_symvalues));
        var uintC count;
        dotimespC(count,thread_objects_anz(num_symvalues),
          { *objptr++ = NIL; objptr++; });
      }
      allthreads[nthreads] = thread;
      nthreads++;
     done:
      xmutex_unlock(&allthreads_lock);
      return thread;
    }

  # Delete a thread.
  local void delete_thread (thread_* thread);
  local void delete_thread(thread)
    var thread_* thread;
    { xmutex_lock(&allthreads_lock);
      ASSERT(thread->_index < nthreads);
      ASSERT(allthreads[thread->_index] == thread);
      allthreads[thread->_index] = allthreads[nthreads-1];
      nthreads--;
      xmutex_unlock(&allthreads_lock);
    }

  #define for_all_threads(statement)  \
    { var thread_** _pthread = &allthreads[nthreads];     \
      until (_pthread == &allthreads[0])                  \
        { var thread_* thread = *--_pthread; statement; } \
    }

  # Add a new symbol value.
  # > value: the default value in all threads
  # < returns: the index in the every thread's _symvalues[] array
  local uintL make_symvalue_perthread (object value);
  local uintL make_symvalue_perthread(value)
    var object value;
    { var uintL index;
      xmutex_lock(&allthreads_lock);
      if (num_symvalues == maxnum_symvalues)
        { for_all_threads(
            { if (mmap_zeromap((void*)((uintP)thread+((thread_size(num_symvalues)+mmap_pagesize-1)&-mmap_pagesize)),mmap_pagesize) < 0) goto failed; }
            );
          maxnum_symvalues += mmap_pagesize/sizeof(object);
        }
      index = num_symvalues++;
      for_all_threads({ thread->_symvalues[index] = value; });
      xmutex_unlock(&allthreads_lock);
      return index;
     failed:
      xmutex_unlock(&allthreads_lock);
      fehler(error,
             DEUTSCH ? "Konnte Symbol-Wert nicht per-Thread machen." :
             ENGLISH ? "could not make symbol value per-thread" :
             FRANCAIS ? "Ne peux pas rendre la valeur d'un symbole dépendant du thread." :
             ""
            );
    }

  #define for_all_threadobjs(statement)  \
    for_all_threads(                                       \
      { var object* objptr = (object*)((uintP)thread+thread_objects_offset(num_symvalues)); \
        var uintC count;                                   \
        dotimespC(count,thread_objects_anz(num_symvalues), \
          { statement; objptr++; });                       \
      })

  #define for_all_STACKs(statement)  \
    for_all_threads(                                         \
      { var object* objptr = STACKpointable(thread->_STACK); \
        { statement; }                                       \
      })

#endif

# ------------------------------------------------------------------------------
#                           Page-Verwaltung

#include "spvw_page.c"
#include "spvw_heap.c"
#include "spvw_global.c"

#ifdef SPVW_PAGES

# Eine Dummy-Page für lastused:
  local NODE dummy_NODE;
  #define dummy_lastused  (&dummy_NODE)

#endif

#ifdef SPVW_BLOCKS

#ifdef SELFMADE_MMAP
# Pages from the memfile are read in when they are first used.
# We manage this ourselves by trapping page faults.
# Works only with SPVW_PURE_BLOCKS or SPVW_MIXED_BLOCKS_STAGGERED.
#endif

#endif

# ------------------------------------------------------------------------------

# Bei Überlauf eines der Stacks:
  nonreturning_function(global, SP_ueber, (void));
  global void SP_ueber()
    { asciz_out( DEUTSCH ? NLstring "*** - " "Programmstack-Überlauf: RESET" :
                 ENGLISH ? NLstring "*** - " "Program stack overflow. RESET" :
                 FRANCAIS ? NLstring "*** - " "Débordement de pile de programme : RAZ" :
                 ""
               );
      reset();
    }
  nonreturning_function(global, STACK_ueber, (void));
  global void STACK_ueber()
    { asciz_out( DEUTSCH ? NLstring "*** - " "LISP-Stack-Überlauf: RESET" :
                 ENGLISH ? NLstring "*** - " "Lisp stack overflow. RESET" :
                 FRANCAIS ? NLstring "*** - " "Débordement de pile Lisp : RAZ" :
                 ""
               );
      reset();
    }

# ------------------------------------------------------------------------------
#                       GC-Statistik

#include "spvw_gcstat.c"

# ------------------------------------------------------------------------------
#                       Speichergröße

#include "spvw_space.c"

# ------------------------------------------------------------------------------
#                       Markierungen

#include "spvw_mark.c"

# ------------------------------------------------------------------------------
#                   Speicherlängenbestimmung

#include "spvw_objsize.c"

# ------------------------------------------------------------------------------
#                    Speicher-Aktualisierung

#include "spvw_update.c"

# ------------------------------------------------------------------------------
#                      Page Fault and Protection handling

#if defined(SELFMADE_MMAP) || defined(GENERATIONAL_GC)

#include "spvw_fault.c"

#endif # SELFMADE_MMAP || GENERATIONAL_GC

# ------------------------------------------------------------------------------
#                      Signal handlers

#include "spvw_sigsegv.c"
#include "spvw_sigcld.c"
#include "spvw_sigint.c"
#include "spvw_sigwinch.c"

# ------------------------------------------------------------------------------
#                       Garbage-Collector

#include "spvw_garcol.c"

# ------------------------------------------------------------------------------
#                 Speicherbereitstellungsfunktionen

#include "spvw_allocate.c"
#include "spvw_typealloc.c"

# ------------------------------------------------------------------------------
#                   Zirkularitätenfeststellung

#include "spvw_circ.c"

# ------------------------------------------------------------------------------
#                     Speicher durchlaufen

# UP: Läuft durch den gesamten Speicher durch, und ruft dabei für jedes
# Objekt obj: fun(arg,obj,bytelen) auf.
# map_heap_objects(fun,arg)
# > fun: C-Funktion
# > arg: beliebiges vorgegebenes Argument
  global void map_heap_objects (map_heap_function* fun, void* arg);
  global void map_heap_objects(fun,arg)
    var map_heap_function* fun;
    var void* arg;
    { # Programmkonstanten:
      for_all_subrs( fun(arg,subr_tab_ptr_as_object(ptr),sizeof(subr_)) );
      for_all_constsyms( fun(arg,symbol_tab_ptr_as_object(ptr),sizeof(symbol_)) );
      #if defined(SPVW_PURE_BLOCKS) # && defined(SINGLEMAP_MEMORY)
        #define varobject_typecode_at(type,p)
        #define cons_typecode_at(type,p)
        #define with_typecode(p)  as_object(p)
      #else
        #ifdef SPVW_MIXED
          #ifdef TYPECODES
            #define varobject_typecode_at(type,p)  \
              var tint type = typecode_at(p);                      \
              switch (type)                                        \
                { case_symbolwithflags: type = symbol_type; break; \
                  default: break;                                  \
                }
            #define cons_typecode_at(type,p)  \
              var tint type = cons_type;
          #else
            #define varobject_typecode_at(type,p)  \
              var oint type = varobject_bias;
            #define cons_typecode_at(type,p)  \
              var oint type = cons_bias;
          #endif
        #endif
        #ifdef SPVW_PURE
          #define varobject_typecode_at(type,p)  \
            var tint type = heapnr;
          #define cons_typecode_at(type,p)  \
            var tint type = heapnr;
        #endif
        #ifdef TYPECODES
          #define with_typecode(p)  type_pointer_object(type,p)
        #else
          #define with_typecode(p)  as_object((oint)(p)+(oint)type)
        #endif
      #endif
      #ifdef GENERATIONAL_GC
        # Objekte variabler Länge:
        for_each_varobject_heap(heap,
          { var_prepare_objsize;
            { var aint p = heap->heap_gen0_start;
              var aint p_end = heap->heap_gen0_end;
              until (p==p_end)
                { varobject_typecode_at(type,p);
                 {var uintL laenge = objsize((Varobject)p);
                  fun(arg,with_typecode(p),laenge);
                  p += laenge;
            }   }}
            { var aint p = heap->heap_gen1_start;
              var aint p_end = heap->heap_end;
              until (p==p_end)
                { varobject_typecode_at(type,p);
                 {var uintL laenge = objsize((Varobject)p);
                  fun(arg,with_typecode(p),laenge);
                  p += laenge;
            }   }}
          });
        # Zwei-Pointer-Objekte:
        #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
        for_each_cons_heap(heap,
          { { var aint p = heap->heap_start;
              var aint p_end = heap->heap_gen1_end;
              until (p==p_end)
                { cons_typecode_at(type,p);
                  fun(arg,with_typecode(p),sizeof(cons_));
                  p += sizeof(cons_);
            }   }
            { var aint p = heap->heap_gen0_start;
              var aint p_end = heap->heap_gen0_end;
              until (p==p_end)
                { cons_typecode_at(type,p);
                  fun(arg,with_typecode(p),sizeof(cons_));
                  p += sizeof(cons_);
            }   }
          });
        #else
        for_each_cons_heap(heap,
          { { var aint p = heap->heap_gen0_start;
              var aint p_end = heap->heap_gen0_end;
              until (p==p_end)
                { cons_typecode_at(type,p);
                  fun(arg,with_typecode(p),sizeof(cons_));
                  p += sizeof(cons_);
            }   }
            { var aint p = heap->heap_gen1_start;
              var aint p_end = heap->heap_end;
              until (p==p_end)
                { cons_typecode_at(type,p);
                  fun(arg,with_typecode(p),sizeof(cons_));
                  p += sizeof(cons_);
            }   }
          });
        #endif
      #else
        # Objekte variabler Länge:
        for_each_varobject_page(page,
          { var aint p = page->page_start;
            var aint p_end = page->page_end;
            var_prepare_objsize;
            until (p==p_end)
              { varobject_typecode_at(type,p);
               {var uintL laenge = objsize((Varobject)p);
                fun(arg,with_typecode(p),laenge);
                p += laenge;
              }}
          });
        # Zwei-Pointer-Objekte:
        for_each_cons_page(page,
          { var aint p = page->page_start;
            var aint p_end = page->page_end;
            until (p==p_end)
              { cons_typecode_at(type,p);
                fun(arg,with_typecode(p),sizeof(cons_));
                p += sizeof(cons_);
              }
          });
      #endif
      #undef varobject_typecode_at
      #undef cons_typecode_at
      #undef with_typecode
    }

# ------------------------------------------------------------------------------
#                  Elementare Stringfunktionen

# UP: Liefert einen LISP-String mit vorgegebenem Inhalt.
# make_string(charptr,len)
# > uintB* charptr: Adresse einer Zeichenfolge
# > uintL len: Länge der Zeichenfolge
# < ergebnis: Simple-String mit den len Zeichen ab charptr als Inhalt
# kann GC auslösen
  global object make_string (const uintB* charptr, uintL len);
  global object make_string(charptr,len)
    var const uintB* charptr;
    var uintL len;
    { var object obj = allocate_string(len); # String allozieren
      var uintB* ptr = &TheSstring(obj)->data[0];
      # Zeichenfolge von charptr nach ptr kopieren:
      dotimesL(len,len, { *ptr++ = *charptr++; } );
      return(obj);
    }

#ifndef asciz_length
# UP: Liefert die Länge eines ASCIZ-Strings.
# asciz_length(asciz)
# > char* asciz: ASCIZ-String
#       (Adresse einer durch ein Nullbyte abgeschlossenen Zeichenfolge)
# < ergebnis: Länge der Zeichenfolge (ohne Nullbyte)
  global uintL asciz_length (const char * asciz);
  global uintL asciz_length(asciz)
    var const char* asciz;
    { var const char* ptr = asciz;
      var uintL len = 0;
      # Nullbyte suchen und dabei Länge hochzählen:
      while (!( *ptr++ == 0 )) { len++; }
      return len;
    }
#endif

#ifndef asciz_equal
# UP: Vergleicht zwei ASCIZ-Strings.
# asciz_equal(asciz1,asciz2)
# > char* asciz1: erster ASCIZ-String
# > char* asciz2: zweiter ASCIZ-String
# < ergebnis: TRUE falls die Zeichenfolgen gleich sind
  global boolean asciz_equal (const char * asciz1, const char * asciz2);
  global boolean asciz_equal(asciz1,asciz2)
    var const char* asciz1;
    var const char* asciz2;
    { # Bytes vergleichen, solange bis das erste Nullbyte kommt:
      loop
        { var char ch1 = *asciz1++;
          if (!(ch1 == *asciz2++)) goto no;
          if (ch1 == '\0') goto yes;
        }
      yes: return TRUE;
      no: return FALSE;
    }
#endif

# UP: Wandelt einen ASCIZ-String in einen LISP-String um.
# asciz_to_string(asciz)
# > char* asciz: ASCIZ-String
#       (Adresse einer durch ein Nullbyte abgeschlossenen Zeichenfolge)
# < ergebnis: String mit der Zeichenfolge (ohne Nullbyte) als Inhalt
# kann GC auslösen
  global object asciz_to_string (const char * asciz);
  global object asciz_to_string(asciz)
    var const char* asciz;
    { return make_string((const uintB*)asciz,asciz_length(asciz)); }

# UP: Wandelt einen String in einen ASCIZ-String um.
# string_to_asciz(obj)
# > object obj: String
# < ergebnis: Simple-String mit denselben Zeichen und einem Nullbyte mehr am Schluß
# kann GC auslösen
  global object string_to_asciz (object obj);
  global object string_to_asciz (obj)
    var object obj;
    { # (vgl. copy_string in CHARSTRG)
      pushSTACK(obj); # String retten
     {var object newstring = allocate_string(vector_length(obj)+1);
          # neuer Simple-String mit einem Byte mehr Länge
      obj = popSTACK(); # String zurück
      { var uintL len;
        var uintB* sourceptr = unpack_string(obj,&len);
        # Source-String: Länge in len, Bytes ab sourceptr
        var uintB* destptr = &TheSstring(newstring)->data[0];
        # Destination-String: Bytes ab destptr
        { # Kopierschleife:
          var uintL count;
          dotimesL(count,len, { *destptr++ = *sourceptr++; } );
          *destptr++ = 0; # Nullbyte anfügen
      } }
      return newstring;
    }}

# ------------------------------------------------------------------------------
#                  Andere globale Hilfsfunktionen

#if (int_bitsize < long_bitsize)
# Übergabewert an setjmpl() von longjmpl():
  global long jmpl_value;
#endif

#ifndef SP
# Bestimmung (einer Approximation) des SP-Stackpointers.
  global void* SP (void);
  global void* SP()
    { var long dummy;
      return &dummy;
    }
#endif

# Fehlermeldung wegen Erreichen einer unerreichbaren Programmstelle.
# Kehrt nicht zurück.
# fehler_notreached(file,line);
# > file: Filename (mit Anführungszeichen) als konstanter ASCIZ-String
# > line: Zeilennummer
  nonreturning_function(global, fehler_notreached, (const char * file, uintL line));
  global void fehler_notreached(file,line)
    var const char * file;
    var uintL line;
    { end_system_call(); # just in case
      pushSTACK(fixnum(line));
      pushSTACK(asciz_to_string(file));
      fehler(serious_condition,
             DEUTSCH ? "Interner Fehler: Anweisung in File ~, Zeile ~ wurde ausgeführt!!" NLstring
                       "Bitte schicken Sie eine Mitteilung an die Programm-Autoren, "
                       "mit der Beschreibung, wie Sie diesen Fehler erzeugt haben!" :
             ENGLISH ? "internal error: statement in file ~, line ~ has been reached!!" NLstring
                       "Please send the authors of the program "
                       "a description how you produced this error!" :
             FRANCAIS ? "Erreur interne : Dans le fichier ~, la ligne ~ fut exécutée!" NLstring
                        "Veuillez signaler aux auteurs du programme comment "
                        "vous avez pu faire apparaître cette erreur, s.v.p.!" :
             ""
            );
    }

#include "spvw_language.c"

# ------------------------------------------------------------------------------
#                        Initialisierung

# Name des Programms (für Fehlermeldungszwecke)
  local char* program_name;

# Flag, ob SYS::READ-FORM sich ILISP-kompatibel verhalten soll:
  global boolean ilisp_mode = FALSE;

#ifdef UNIX

# Real User ID des laufenden Prozesses.
  global uid_t user_uid;

#endif

# Umwandlung der Argumenttypen eines FSUBR in einen Code:
  local fsubr_argtype_t fsubr_argtype (uintW req_anz, uintW opt_anz, fsubr_body_t body_flag);
  local fsubr_argtype_t fsubr_argtype(req_anz,opt_anz,body_flag)
    var uintW req_anz;
    var uintW opt_anz;
    var fsubr_body_t body_flag;
    { switch (body_flag)
        { case fsubr_nobody:
            switch (opt_anz)
              { case 0:
                  switch (req_anz)
                    { case 1: return(fsubr_argtype_1_0_nobody);
                      case 2: return(fsubr_argtype_2_0_nobody);
                      default: goto illegal;
                    }
                case 1:
                  switch (req_anz)
                    { case 1: return(fsubr_argtype_1_1_nobody);
                      case 2: return(fsubr_argtype_2_1_nobody);
                      default: goto illegal;
                    }
                default: goto illegal;
              }
          case fsubr_body:
            switch (opt_anz)
              { case 0:
                  switch (req_anz)
                    { case 0: return(fsubr_argtype_0_body);
                      case 1: return(fsubr_argtype_1_body);
                      case 2: return(fsubr_argtype_2_body);
                      default: goto illegal;
                    }
                default: goto illegal;
              }
          default: goto illegal;
        }
      illegal:
        asciz_out(DEUTSCH ? "Unbekannter FSUBR-Argumenttyp" NLstring :
                  ENGLISH ? "Unknown signature of an FSUBR" NLstring :
                  FRANCAIS ? "Type d'argument inconnu pour FSUBR" NLstring :
                  ""
                 );
        quit_sofort(1);
    }

# Umwandlung der Argumenttypen eines SUBR in einen Code:
  local subr_argtype_t subr_argtype (uintW req_anz, uintW opt_anz, subr_rest_t rest_flag, subr_key_t key_flag);
  local subr_argtype_t subr_argtype(req_anz,opt_anz,rest_flag,key_flag)
    var uintW req_anz;
    var uintW opt_anz;
    var subr_rest_t rest_flag;
    var subr_key_t key_flag;
    { switch (key_flag)
        { case subr_nokey:
            switch (rest_flag)
              { case subr_norest:
                  switch (opt_anz)
                    { case 0:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_0);
                            case 1: return(subr_argtype_1_0);
                            case 2: return(subr_argtype_2_0);
                            case 3: return(subr_argtype_3_0);
                            case 4: return(subr_argtype_4_0);
                            case 5: return(subr_argtype_5_0);
                            case 6: return(subr_argtype_6_0);
                            default: goto illegal;
                          }
                      case 1:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_1);
                            case 1: return(subr_argtype_1_1);
                            case 2: return(subr_argtype_2_1);
                            case 3: return(subr_argtype_3_1);
                            case 4: return(subr_argtype_4_1);
                            default: goto illegal;
                          }
                      case 2:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_2);
                            case 1: return(subr_argtype_1_2);
                            case 2: return(subr_argtype_2_2);
                            default: goto illegal;
                          }
                      case 3:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_3);
                            default: goto illegal;
                          }
                      case 4:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_4);
                            default: goto illegal;
                          }
                      case 5:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_5);
                            default: goto illegal;
                          }
                      default: goto illegal;
                    }
                case subr_rest:
                  switch (opt_anz)
                    { case 0:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_0_rest);
                            case 1: return(subr_argtype_1_0_rest);
                            case 2: return(subr_argtype_2_0_rest);
                            case 3: return(subr_argtype_3_0_rest);
                            default: goto illegal;
                          }
                      default: goto illegal;
                    }
                default: goto illegal;
              }
          case subr_key:
            switch (rest_flag)
              { case subr_norest:
                  switch (opt_anz)
                    { case 0:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_0_key);
                            case 1: return(subr_argtype_1_0_key);
                            case 2: return(subr_argtype_2_0_key);
                            case 3: return(subr_argtype_3_0_key);
                            case 4: return(subr_argtype_4_0_key);
                            default: goto illegal;
                          }
                      case 1:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_1_key);
                            case 1: return(subr_argtype_1_1_key);
                            default: goto illegal;
                          }
                      case 2:
                        switch (req_anz)
                          { case 1: return(subr_argtype_1_2_key);
                            default: goto illegal;
                          }
                      default: goto illegal;
                    }
                case subr_rest:
                default: goto illegal;
              }
          case subr_key_allow: goto illegal;
          default: goto illegal;
        }
      illegal:
        asciz_out(DEUTSCH ? "Unbekannter SUBR-Argumenttyp" NLstring :
                  ENGLISH ? "Unknown signature of a SUBR" NLstring :
                  FRANCAIS ? "Type d'argument inconnu pour SUBR" NLstring :
                  ""
                 );
        quit_sofort(1);
    }

# Verify that a code address has the C_CODE_ALIGNMENT.
# This is important for calling make_machine_code, but it's easiest verified
# on Fsubrs and Subrs.
#ifdef TYPECODES
  #define verify_code_alignment(ptr,symbol)  # not needed
#else
  #define verify_code_alignment(ptr,symbol)  \
    if ((uintP)(void*)(ptr) & (C_CODE_ALIGNMENT-1))     \
      fehler_code_alignment((uintP)(void*)(ptr),symbol)
  nonreturning_function(local, fehler_code_alignment, (uintP address, object symbol));
  local void fehler_code_alignment(address,symbol)
    var uintP address;
    var object symbol;
    { asciz_out("C_CODE_ALIGNMENT is wrong. ");
      asciz_out_s("&%s",TheAsciz(string_to_asciz(Symbol_name(symbol))));
      asciz_out_1(" = 0x%x." NLstring,address);
      abort();
    }
#endif

# Initialisierungs-Routinen für die Tabellen
# während des 1. Teils der Initialisierungsphase:
  # subr_tab initialisieren:
    local void init_subr_tab_1 (void);
    local void init_subr_tab_1()
      {
        #if defined(INIT_SUBR_TAB)
          #ifdef MAP_MEMORY_TABLES
            # Tabelle in den vorgesehenen Bereich kopieren:
            subr_tab = subr_tab_data;
          #endif
          #if !NIL_IS_CONSTANT
          # Erst noch den name-Slot initialisieren:
          { var subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
            #define LISPFUN  LISPFUN_E
            #include "subr.c"
            #undef LISPFUN
          }
          # und den keywords-Slot vorläufig initialisieren:
          { var subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
            var uintC count = subr_anz;
            dotimesC(count,subr_anz, { ptr->keywords = NIL; ptr++; });
          }
          #endif
          # Durch SPVWTABF sind schon alle Slots außer keywords und argtype
          # initialisiert.
          # Nun den argtype-Slot initialisieren:
          { var subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
            var uintC count;
            dotimesC(count,subr_anz,
              { ptr->argtype =
                  (uintW)subr_argtype(ptr->req_anz,ptr->opt_anz,(subr_rest_t)(ptr->rest_flag),(subr_key_t)(ptr->key_flag));
                ptr++;
              });
          }
        #else
          # Alle Slots außer keywords initialisieren:
          { var subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
            #define LISPFUN  LISPFUN_D
            #include "subr.c"
            #undef LISPFUN
          }
        #endif
        { var module_* module;
          for_modules(all_other_modules,
            { var subr_* ptr = module->stab; # subr_tab durchgehen
              var uintC count;
              dotimesC(count,*module->stab_size,
                { ptr->argtype =
                    (uintW)subr_argtype(ptr->req_anz,ptr->opt_anz,(subr_rest_t)(ptr->rest_flag),(subr_key_t)(ptr->key_flag));
                  ptr++;
                });
            });
        }
        #ifdef MAP_MEMORY_TABLES
        # Andere Tabellen ebenfalls in den gemappten Bereich kopieren:
        { var subr_* newptr = (subr_*)&subr_tab;
          var module_* module;
          main_module.stab = newptr; newptr += subr_anz;
          for_modules(all_other_modules,
            { var subr_* oldptr = module->stab;
              var uintC count;
              module->stab = newptr;
              dotimesC(count,*module->stab_size, { *newptr++ = *oldptr++; } );
            });
          ASSERT(newptr == (subr_*)&subr_tab + total_subr_anz);
        }
        #endif
      }
  # symbol_tab initialisieren:
    local void init_symbol_tab_1 (void);
    local void init_symbol_tab_1()
      {
        #if defined(INIT_SYMBOL_TAB) && NIL_IS_CONSTANT
          #ifdef MAP_MEMORY_TABLES
            # Tabelle in den vorgesehenen Bereich kopieren:
            symbol_tab = symbol_tab_data;
          #endif
        #else
          { var symbol_* ptr = (symbol_*)&symbol_tab; # symbol_tab durchgehen
            var uintC count;
            for (count = symbol_anz; count > 0; count--)
              { ptr->GCself = symbol_tab_ptr_as_object(ptr);
                #ifndef TYPECODES
                ptr->tfl = xrecord_tfl(Rectype_Symbol,0,5,0);
                #endif
                ptr->symvalue = unbound;
                ptr->symfunction = unbound;
                ptr->proplist = NIL;
                ptr->pname = NIL;
                ptr->homepackage = NIL;
                ptr++;
              }
          }
        #endif
      }
  # object_tab initialisieren:
    local void init_object_tab_1 (void);
    local void init_object_tab_1()
      { var module_* module;
        #if defined(INIT_OBJECT_TAB) && NIL_IS_CONSTANT # object_tab schon vorinitialisiert?
          for_modules(all_other_modules,
            { var object* ptr = module->otab; # object_tab durchgehen
              var uintC count;
              dotimesC(count,*module->otab_size, { *ptr++ = NIL; });
            });
        #else
          for_modules(all_modules,
            { var object* ptr = module->otab; # object_tab durchgehen
              var uintC count;
              dotimesC(count,*module->otab_size, { *ptr++ = NIL; });
            });
        #endif
      }
  # andere Module grob initialisieren:
    local void init_other_modules_1 (void);
    local void init_other_modules_1()
      { var module_* module;
        for_modules(all_other_modules,
          { # Pointer in der Subr-Tabelle mit NIL füllen, damit GC möglich wird:
            var subr_* ptr = module->stab;
            var uintC count;
            dotimesC(count,*module->stab_size,
              { ptr->name = NIL; ptr->keywords = NIL; ptr++; }
              );
            # Die Pointer in der Objekt-Tabelle hat init_object_tab_1() schon vorinitialisiert.
          });
      }

# Initialisierungs-Routinen für die Tabellen
# während des 2. Teils der Initialisierungsphase:
  # subr_tab fertig initialisieren: Keyword-Vektoren eintragen.
    local void init_subr_tab_2 (void);
    local void init_subr_tab_2()
      #if 0
        # Ich hätt's gern so einfach, aber
        # bei TURBO-C reicht der Speicher zum Compilieren nicht!
        { # subr_tab durchgehen
          var object vec;
          var object* vecptr;
          #define LISPFUN  LISPFUN_H
          #define kw(name)  *vecptr++ = S(K##name)
          #include "subr.c"
          #undef LISPFUN
          #undef kw
        }
      #else
        { # Keyword-Vektoren einzeln erzeugen:
          var object vec;
          var object* vecptr;
          # füllt ein einzelnes Keyword mehr in den Vektor ein:
            #define kw(name)  *vecptr++ = S(K##name)
          # bildet Vektor mit gegebenen Keywords:
            #define v(key_anz,keywords)  \
              vec = allocate_vector(key_anz); \
              vecptr = &TheSvector(vec)->data[0]; \
              keywords;
          # setzt den Vektor als Keyword-Vektor zum SUBR name fest:
            #define s(name)  subr_tab.D_##name.keywords = vec;
          #include "subrkw.c"
          #undef s
          #undef v
          #undef kw
        }
      #endif
  # symbol_tab zu Ende initialisieren: Printnamen und Home-Package eintragen.
    local void init_symbol_tab_2 (void);
    local void init_symbol_tab_2()
      { # Tabelle der Printnamen:
        local const char * const pname_table[symbol_anz] =
          {
            #define LISPSYM  LISPSYM_C
            #include "constsym.c"
            #undef LISPSYM
          };
        # Tabelle der Packages:
        enum { # Die Werte dieser Aufzählung sind der Reihe nach 0,1,2,...
               enum_lisp_index,
               enum_system_index,
               enum_keyword_index,
               #define LISPPACK  LISPPACK_A
               #include "constpack.c"
               #undef LISPPACK
               enum_dummy_index
          };
        #define package_anz  ((uintL)enum_dummy_index)
        local const uintB package_index_table[symbol_anz] =
          {
            #define LISPSYM  LISPSYM_D
            #include "constsym.c"
            #undef LISPSYM
          };
        {var object list = O(all_packages); # Liste der Packages
         # kurz nach der Initialisierung:
         # (#<PACKAGE LISP> #<PACKAGE SYSTEM> #<PACKAGE KEYWORD> ...)
         var uintC count;
         dotimespC(count,package_anz, { pushSTACK(Car(list)); list = Cdr(list); });
        }
       {var symbol_* ptr = (symbol_*)&symbol_tab; # symbol_tab durchgehen
        var const char * const * pname_ptr = &pname_table[0]; # pname_table durchgehen
        var const uintB* index_ptr = &package_index_table[0]; # package_index_table durchgehen
        var uintC count;
        dotimesC(count,symbol_anz,
          { ptr->pname = asciz_to_string(*pname_ptr++); # Printnamen eintragen
           {var uintB index = *index_ptr++;
            var object* package_ = &STACK_(package_anz-1) STACKop -(uintP)index; # Pointer auf Package
            pushSTACK(symbol_tab_ptr_as_object(ptr)); # Symbol
            import(&STACK_0,package_); # erst normal importieren
            if (index == (uintB)enum_lisp_index) # in #<PACKAGE LISP> ?
              { export(&STACK_0,package_); } # ja -> auch exportieren
            Symbol_package(popSTACK()) = *package_; # und die Home-Package setzen
            ptr++;
          }});
        skipSTACK(package_anz);
      }}
  # FSUBRs/SUBRs in ihre Symbole eintragen:
    local void init_symbol_functions (void);
    local void init_symbol_functions()
      {# FSUBRs eintragen:
       {typedef struct {
                        #if defined(INIT_SUBR_TAB) && NIL_IS_CONSTANT
                          #define LISPSPECFORM LISPSPECFORM_F
                          object name;
                          #define fsubr_name(p)  (p)->name
                        #else
                          #define LISPSPECFORM LISPSPECFORM_E
                          uintL name_offset;
                          #define fsubr_name(p)  symbol_tab_ptr_as_object((char*)&symbol_tab+(p)->name_offset)
                        #endif
                        uintW req_anz;
                        uintW opt_anz;
                        uintW body_flag;
                       }
                fsubr_data;
        local const fsubr_data fsubr_data_tab[] = {
                                                    #include "fsubr.c"
                                                  };
        #undef LISPSPECFORM
        var const fsubr_* ptr1 = (const fsubr_ *)&fsubr_tab; # fsubr_tab durchgehen
        var const fsubr_data * ptr2 = &fsubr_data_tab[0]; # fsubr_data_tab durchgehen
        var uintC count;
        dotimesC(count,fsubr_anz,
          { var object sym = fsubr_name(ptr2);
            var object obj = allocate_fsubr();
            TheFsubr(obj)->name = sym;
            TheFsubr(obj)->argtype = fixnum((uintW)fsubr_argtype(ptr2->req_anz,ptr2->opt_anz,(fsubr_body_t)(ptr2->body_flag)));
            TheFsubr(obj)->function = (void*)(*ptr1);
            Symbol_function(sym) = obj;
            verify_code_alignment(*ptr1,sym);
            ptr1++; ptr2++;
          });
       }
       # SUBRs eintragen:
       {var subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
        var uintC count;
        dotimesC(count,subr_anz,
          { Symbol_function(ptr->name) = subr_tab_ptr_as_object(ptr);
            verify_code_alignment(ptr->function,ptr->name);
            ptr++;
          });
      }}
  # Konstanten/Variablen ihre Werte zuweisen:
    local void init_symbol_values (void);
    local void init_symbol_values()
      { # Hilfsmacro: Konstante := wert+1
        #define define_constant_UL1(symbol,wert)  \
          { var object x = # wert+1 als Integer                  \
              ( ((uintL)(wert) < (uintL)(bitm(oint_data_len)-1)) \
                ? fixnum(wert+1)                                 \
                : I_1_plus_I(UL_to_I(wert))                      \
              );                                                 \
            define_constant(symbol,x);                           \
          }
        # allgemein:
        define_constant(S(nil),S(nil));                 # NIL := NIL
        define_constant(S(t),S(t));                     # T := T
        define_variable(S(gc_statistics_stern),Fixnum_minus1); # SYS::*GC-STATISTICS* := -1
        # zu EVAL/CONTROL:
        define_constant_UL1(S(lambda_parameters_limit),lp_limit_1); # LAMBDA-PARAMETERS-LIMIT := lp_limit_1 + 1
        define_constant_UL1(S(call_arguments_limit),ca_limit_1); # CALL-ARGUMENTS-LIMIT := ca_limit_1 + 1
        define_constant(S(multiple_values_limit),       # MULTIPLE-VALUES-LIMIT
          fixnum(mv_limit));      # := mv_limit
        define_constant(S(jmpbuf_size),                 # SYS::*JMPBUF-SIZE* := Größe eines jmp_buf
          fixnum(jmpbufsize));
        define_constant(S(big_endian),(BIG_ENDIAN_P ? T : NIL)); # SYS::*BIG-ENDIAN* := NIL bzw. T
        define_variable(S(macroexpand_hook),L(pfuncall)); # *MACROEXPAND-HOOK* := #'SYS::%FUNCALL
        define_variable(S(evalhookstern),NIL);          # *EVALHOOK*
        define_variable(S(applyhookstern),NIL);         # *APPLYHOOK*
        # zu PACKAGE:
        define_variable(S(packagestern),Car(O(all_packages))); # *PACKAGE* := '#<PACKAGE LISP>
        # zu SYMBOL:
        define_variable(S(gensym_counter),Fixnum_1);    # *GENSYM-COUNTER* := 1
        # zu LISPARIT:
        init_arith(); # definiert folgende:
        # define_variable(S(pi),);                      # PI
        # define_constant(S(most_positive_fixnum),);    # MOST-POSITIVE-FIXNUM
        # define_constant(S(most_negative_fixnum),);    # MOST-NEGATIVE-FIXNUM
        # define_constant(S(most_positive_short_float),); # MOST-POSITIVE-SHORT-FLOAT
        # define_constant(S(least_positive_short_float),); # LEAST-POSITIVE-SHORT-FLOAT
        # define_constant(S(least_negative_short_float),); # LEAST-NEGATIVE-SHORT-FLOAT
        # define_constant(S(most_negative_short_float),); # MOST-NEGATIVE-SHORT-FLOAT
        # define_constant(S(most_positive_single_float),); # MOST-POSITIVE-SINGLE-FLOAT
        # define_constant(S(least_positive_single_float),); # LEAST-POSITIVE-SINGLE-FLOAT
        # define_constant(S(least_negative_single_float),); # LEAST-NEGATIVE-SINGLE-FLOAT
        # define_constant(S(most_negative_single_float),); # MOST-NEGATIVE-SINGLE-FLOAT
        # define_constant(S(most_positive_double_float),); # MOST-POSITIVE-DOUBLE-FLOAT
        # define_constant(S(least_positive_double_float),); # LEAST-POSITIVE-DOUBLE-FLOAT
        # define_constant(S(least_negative_double_float),); # LEAST-NEGATIVE-DOUBLE-FLOAT
        # define_constant(S(most_negative_double_float),); # MOST-NEGATIVE-DOUBLE-FLOAT
        # define_variable(S(most_positive_long_float),); # MOST-POSITIVE-LONG-FLOAT
        # define_variable(S(least_positive_long_float),); # LEAST-POSITIVE-LONG-FLOAT
        # define_variable(S(least_negative_long_float),); # LEAST-NEGATIVE-LONG-FLOAT
        # define_variable(S(most_negative_long_float),); # MOST-NEGATIVE-LONG-FLOAT
        # define_variable(S(least_positive_normalized_long_float),); # LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
        # define_variable(S(least_negative_normalized_long_float),); # LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
        # define_constant(S(short_float_epsilon),);     # SHORT-FLOAT-EPSILON
        # define_constant(S(single_float_epsilon),);    # SINGLE-FLOAT-EPSILON
        # define_constant(S(double_float_epsilon),);    # DOUBLE-FLOAT-EPSILON
        # define_variable(S(long_float_epsilon),);      # LONG-FLOAT-EPSILON
        # define_constant(S(short_float_negative_epsilon),); # SHORT-FLOAT-NEGATIVE-EPSILON
        # define_constant(S(single_float_negative_epsilon),); # SINGLE-FLOAT-NEGATIVE-EPSILON
        # define_constant(S(double_float_negative_epsilon),); # DOUBLE-FLOAT-NEGATIVE-EPSILON
        # define_variable(S(long_float_negative_epsilon),); # LONG-FLOAT-NEGATIVE-EPSILON
        # define_variable(S(read_default_float_format),); # *READ-DEFAULT-FLOAT-FORMAT*
        # define_variable(S(random_state),);            # *RANDOM-STATE*
        # zu ARRAY:
        define_constant_UL1(S(array_total_size_limit),arraysize_limit_1); # ARRAY-TOTAL-SIZE-LIMIT := arraysize_limit_1 + 1
        define_constant_UL1(S(array_dimension_limit),arraysize_limit_1); # ARRAY-DIMENSION-LIMIT := arraysize_limit_1 + 1
        define_constant_UL1(S(array_rank_limit),arrayrank_limit_1); # ARRAY-RANK-LIMIT := arrayrank_limit_1 + 1
        # zu DEBUG:
        define_variable(S(plus),NIL);                   # +
        define_variable(S(plus2),NIL);                  # ++
        define_variable(S(plus3),NIL);                  # +++
        define_variable(S(minus),NIL);                  # -
        define_variable(S(mal),NIL);                    # *
        define_variable(S(mal2),NIL);                   # **
        define_variable(S(mal3),NIL);                   # ***
        define_variable(S(durch),NIL);                  # /
        define_variable(S(durch2),NIL);                 # //
        define_variable(S(durch3),NIL);                 # ///
        define_variable(S(driverstern),NIL);            # *DRIVER* := NIL
        define_variable(S(break_driver),NIL);           # *BREAK-DRIVER* := NIL
        define_variable(S(break_count),Fixnum_0);       # SYS::*BREAK-COUNT* := 0
        define_variable(S(recurse_count_standard_output),Fixnum_0); # SYS::*RECURSE-COUNT-STANDARD-OUTPUT* := 0
        define_variable(S(recurse_count_debug_io),Fixnum_0); # SYS::*RECURSE-COUNT-DEBUG-IO* := 0
        # zu STREAM:
        # später: init_streamvars(); # definiert folgende:
        # define_variable(S(standard_input),);          # *STANDARD-INPUT*
        # define_variable(S(standard_output),);         # *STANDARD-OUTPUT*
        # define_variable(S(error_output),);            # *ERROR-OUTPUT*
        # define_variable(S(query_io),);                # *QUERY-IO*
        # define_variable(S(debug_io),);                # *DEBUG-IO*
        # define_variable(S(terminal_io),);             # *TERMINAL-IO*
        # define_variable(S(trace_output),);            # *TRACE-OUTPUT*
        # define_variable(S(keyboard_input),);          # *KEYBOARD-INPUT*
        define_variable(S(default_pathname_defaults),unbound); # *DEFAULT-PATHNAME-DEFAULTS*
        # zu IO:
        init_reader(); # definiert folgende:
        # define_variable(S(read_base),);               # *READ-BASE* := 10
        # define_variable(S(read_suppress),);           # *READ-SUPPRESS* := NIL
        # define_variable(S(read_eval),);               # *READ-EVAL* := T
        # define_variable(S(readtablestern),);          # *READTABLE*
        define_variable(S(read_preserve_whitespace),unbound); # SYS::*READ-PRESERVE-WHITESPACE*
        define_variable(S(read_recursive_p),unbound);   # SYS::*READ-RECURSIVE-P*
        define_variable(S(read_reference_table),unbound); # SYS::*READ-REFERENCE-TABLE*
        define_variable(S(backquote_level),unbound);    # SYS::*BACKQUOTE-LEVEL*
        define_variable(S(compiling),NIL);              # SYS::*COMPILING* ;= NIL
        define_variable(S(print_case),S(Kupcase));      # *PRINT-CASE* := :UPCASE
        define_variable(S(print_level),NIL);            # *PRINT-LEVEL* := NIL
        define_variable(S(print_length),NIL);           # *PRINT-LENGTH* := NIL
        define_variable(S(print_gensym),T);             # *PRINT-GENSYM* := T
        define_variable(S(print_escape),T);             # *PRINT-ESCAPE* := T
        define_variable(S(print_radix),NIL);            # *PRINT-RADIX* := NIL
        define_variable(S(print_base),fixnum(10));      # *PRINT-BASE* := 10
        define_variable(S(print_array),T);              # *PRINT-ARRAY* := T
        define_variable(S(print_circle),NIL);           # *PRINT-CIRCLE* := NIL
        define_variable(S(print_pretty),NIL);           # *PRINT-PRETTY* := NIL
        define_variable(S(print_closure),NIL);          # *PRINT-CLOSURE* := NIL
        define_variable(S(print_readably),NIL);         # *PRINT-READABLY* := NIL
        define_variable(S(print_right_margin),NIL);     # *PRINT-RIGHT-MARGIN* := NIL
        define_variable(S(print_rpars),T);              # *PRINT-RPARS* := T
        define_variable(S(print_indent_lists),fixnum(2)); # *PRINT-INDENT-LISTS* := 2
        define_variable(S(print_circle_table),unbound); # SYS::*PRINT-CIRCLE-TABLE*
        define_variable(S(prin_level),unbound);         # SYS::*PRIN-LEVEL*
        define_variable(S(prin_stream),unbound);        # SYS::*PRIN-STREAM*
        define_variable(S(prin_linelength),fixnum(79)); # SYS::*PRIN-LINELENGTH* := 79 (vorläufig)
        define_variable(S(prin_l1),unbound);            # SYS::*PRIN-L1*
        define_variable(S(prin_lm),unbound);            # SYS::*PRIN-LM*
        define_variable(S(prin_rpar),unbound);          # SYS::*PRIN-RPAR*
        define_variable(S(prin_jblocks),unbound);       # SYS::*PRIN-JBLOCKS*
        define_variable(S(prin_jbstrings),unbound);     # SYS::*PRIN-JBSTRINGS*
        define_variable(S(prin_jbmodus),unbound);       # SYS::*PRIN-JBMODUS*
        define_variable(S(prin_jblpos),unbound);        # SYS::*PRIN-JBLPOS*
        # zu EVAL:
        define_variable(S(evalhookstern),NIL);          # *EVALHOOK* := NIL
        define_variable(S(applyhookstern),NIL);         # *APPLYHOOK* := NIL
        # zu MISC:
        define_constant(S(internal_time_units_per_second),  # INTERNAL-TIME-UNITS-PER-SECOND
          fixnum(ticks_per_second) ); # := 200 bzw. 1000000
        # zu PREDTYPE:
        define_variable(S(recurse_count_gc_statistics),Fixnum_0); # SYS::*RECURSE-COUNT-GC-STATISTICS* := 0
        # zu ERROR:
        define_variable(S(use_clcs),NIL);               # SYS::*USE-CLCS* := NIL
        define_variable(S(recursive_error_count),Fixnum_0); # SYS::*RECURSIVE-ERROR-COUNT* := 0
        define_variable(S(error_handler),NIL);          # *ERROR-HANDLER* := NIL
        # zu SPVW:
        define_variable(S(init_hooks),NIL);             # SYS::*INIT-HOOKS* := NIL
        define_variable(S(quiet),NIL);                  # SYS::*QUIET* := NIL
        # zu FOREIGN:
        #ifdef DYNAMIC_FFI
        define_constant(S(fv_flag_readonly),fixnum(fv_readonly));  # FFI::FV-FLAG-READONLY
        define_constant(S(fv_flag_malloc_free),fixnum(fv_malloc)); # FFI::FV-FLAG-MALLOC-FREE
        define_constant(S(ff_flag_alloca),fixnum(ff_alloca));      # FFI::FF-FLAG-ALLOCA
        define_constant(S(ff_flag_malloc_free),fixnum(ff_malloc)); # FFI::FF-FLAG-MALLOC-FREE
        define_constant(S(ff_flag_out),fixnum(ff_out));            # FFI::FF-FLAG-OUT
        define_constant(S(ff_flag_in_out),fixnum(ff_inout));       # FFI::FF-FLAG-IN-OUT
        define_constant(S(ff_language_asm),fixnum(ff_lang_asm));       # FFI::FF-LANGUAGE-ASM
        define_constant(S(ff_language_c),fixnum(ff_lang_c));           # FFI::FF-LANGUAGE-C
        define_constant(S(ff_language_ansi_c),fixnum(ff_lang_ansi_c)); # FFI::FF-LANGUAGE-ANSI-C
        define_constant(S(ff_language_stdcall),fixnum(ff_lang_stdcall)); # FFI::FF-LANGUAGE-STDCALL
        #endif
        # zu PATHNAME:
        #ifdef LOGICAL_PATHNAMES
        { # SYS::*LOGICAL-PATHNAME-TRANSLATIONS* := (MAKE-HASH-TABLE :TEST #'EQUAL)
          pushSTACK(S(Ktest)); pushSTACK(L(equal)); funcall(L(make_hash_table),2);
          define_variable(S(logpathname_translations),value1);
        }
        O(empty_logical_pathname) = allocate_logpathname();
        #endif
        # *DEFAULT-PATHNAME-DEFAULTS* vorläufig initialisieren:
        define_variable(S(default_pathname_defaults),allocate_pathname());
        #undef define_constant_UL1
      }
  # sonstige Objekte kreieren und Objekttabelle füllen:
    local void init_object_tab (void);
    local void init_object_tab()
      { # Tabelle mit Initialisierungsstrings:
        local var const char * const object_initstring_tab []
          = {
             #define LISPOBJ LISPOBJ_C
             #include "constobj.c"
             #undef LISPOBJ
            };
        # *FEATURES* initialisieren:
        { var const char * features_initstring =
            "(CLISP CLTL1 COMMON-LISP INTERPRETER"
            #ifdef LOGICAL_PATHNAMES
              " LOGICAL-PATHNAMES"
            #endif
            #ifdef DYNAMIC_FFI
              " FFI"
            #endif
            #ifdef GNU_GETTEXT
              " GETTEXT"
            #endif
            #ifdef AMIGA
              " AMIGA"
            #endif
            #ifdef PC386
              " PC386"
            #endif
            #ifdef MSDOS
             #ifdef OS2
              " OS/2"
             #else
              " DOS"
             #endif
            #endif
            #ifdef RISCOS
              " ACORN-RISCOS"
            #endif
            #ifdef UNIX
              " UNIX"
            #endif
            #ifdef WIN32
              " WIN32"
            #endif
            ")"
            ;
          pushSTACK(asciz_to_string(features_initstring));
         {var object list = (funcall(L(read_from_string),1), value1);
          define_variable(S(features),list);             # *FEATURES*
        }}
        # Objekte aus den Strings lesen:
        { var object* objptr = (object*)&object_tab; # object_tab durchgehen
          var const char * const * stringptr = &object_initstring_tab[0]; # Stringtabelle durchgehen
          var uintC count;
          dotimesC(count,object_anz,
            { var const char * string = *stringptr++;
              if (*string == '@')
                # Kein READ-FROM-STRING für LISPOBJ_L && GNU_GETTEXT
                { *objptr = asciz_to_string(&string[1]); }
                else
                { pushSTACK(asciz_to_string(string)); # String
                  funcall(L(make_string_input_stream),1); # in Stream verpacken
                  pushSTACK(value1);
                 {var object obj = stream_read(&STACK_0,NIL,NIL); # Objekt lesen
                  skipSTACK(1);
                  if (!eq(obj,dot_value)) { *objptr = obj; } # und eintragen (außer ".")
                }}
              objptr++;
            });
        }
        TheSstring(O(null_string))->data[0] = 0; # Nullbyte in den Null-String einfügen
        Car(O(top_decl_env)) = O(declaration_types); # Toplevel-Deklarations-Environment bauen
      }
  # Zu-Fuß-Initialisierung aller LISP-Daten:
    local void initmem (void);
    local void initmem()
      { init_symbol_tab_1(); # symbol_tab initialisieren
        init_object_tab_1(); # object_tab initialisieren
        init_other_modules_1(); # andere Module grob initialisieren
        { aktenv.var_env = NIL; aktenv.fun_env = NIL; aktenv.block_env = NIL;
          aktenv.go_env = NIL; aktenv.decl_env = NIL;
        }
        # Jetzt sind die Tabellen erst einmal grob initialisiert, bei GC
        # kann nichts passieren.
        # subr_tab fertig initialisieren:
        init_subr_tab_2();
        # Packages initialisieren:
        init_packages();
        # symbol_tab fertig initialisieren:
        init_symbol_tab_2();
        # SUBRs/FSUBRs in ihre Symbole eintragen:
        init_symbol_functions();
        # Konstanten/Variablen: Wert in die Symbole eintragen:
        init_symbol_values();
        # sonstige Objekte kreieren:
        init_object_tab();
      }
  # Laden vom MEM-File:
    local void loadmem (char* filename); # siehe unten
  # Initialiserung der anderen, noch nicht initialisierten Module:
    local void init_other_modules_2 (void);
    local void init_module_2 (module_* module);
    local void init_module_2(module)
      var module_* module;
      { # subr_tab, object_tab vorinitialisieren, damit GC möglich wird:
        { var subr_* ptr = module->stab; # subr_tab durchgehen
          var uintC count;
          dotimesC(count,*module->stab_size, { ptr->name = NIL; ptr->keywords = NIL; ptr++; });
        }
        { var object* ptr = module->otab; # object_tab durchgehen
          var uintC count;
          dotimesC(count,*module->otab_size, { *ptr++ = NIL; });
        }
        # GC darf dieses subr_tab, object_tab nun sehen:
        module->initialized = TRUE;
        # Subr-Symbole eintragen:
        { var subr_* subr_ptr = module->stab;
          var subr_initdata* init_ptr = module->stab_initdata;
          var uintC count;
          dotimesC(count,*module->stab_size,
            { var char* packname = init_ptr->packname;
              var object symname = asciz_to_string(init_ptr->symname);
              var object symbol;
              if (packname==NULL)
                { symbol = make_symbol(symname); }
                else
                { var object pack = find_package(asciz_to_string(packname));
                  if (nullp(pack)) # Package nicht gefunden?
                    { asciz_out_ss(DEUTSCH ? "Modul `%s' benötigt Package %s." NLstring :
                                   ENGLISH ? "module `%s' requires package %s." NLstring :
                                   FRANCAIS ? "Pas de module «%s» sans le paquetage %s." NLstring :
                                   "",
                                   module->name, packname
                                  );
                      quit_sofort(1);
                    }
                  intern(symname,pack,&symbol);
                }
              subr_ptr->name = symbol; # Subr komplett machen
              Symbol_function(symbol) = subr_tab_ptr_as_object(subr_ptr); # Funktion definieren
              init_ptr++; subr_ptr++;
            });
        }
        # Objekte eintragen:
        { var object* object_ptr = module->otab;
          var object_initdata* init_ptr = module->otab_initdata;
          var uintC count;
          dotimesC(count,*module->otab_size,
            { pushSTACK(asciz_to_string(init_ptr->initstring)); # String
              funcall(L(make_string_input_stream),1); # in Stream verpacken
              pushSTACK(value1);
              *object_ptr = stream_read(&STACK_0,NIL,NIL); # Objekt lesen
              skipSTACK(1);
              object_ptr++; init_ptr++;
            });
        }
        # Initialisierungsfunktion aufrufen:
        (*module->initfunction1)(module);
      }
    local void init_other_modules_2()
      { var module_* module; # modules durchgehen
        for_modules(all_other_modules,
          { if (!module->initialized)
              init_module_2(module);
          });
      }

# Hauptprogramm trägt den Namen 'main'.
  #ifdef NEXTAPP
    # main() existiert schon in Lisp_main.m
    #define main  clisp_main
  #endif
  #ifndef argc_t
    #define argc_t int  # Typ von argc ist meist 'int'.
  #endif
  global int main (argc_t argc, char* argv[]);
  local boolean argv_quiet = FALSE; # ob beim Start Quiet-Option angegeben
  global int main(argc,argv)
    var argc_t argc;
    var char* * argv;
    { # Initialisierung der Speicherverwaltung.
      # Gesamtvorgehen:
      # Command-Line-Argumente verarbeiten.
      # Speicheraufteilung bestimmen.
      # Commandstring anschauen und entweder LISP-Daten vom .MEM-File
      #   laden oder zu Fuß erzeugen und statische LISP-Daten initialisieren.
      # Interrupt-Handler aufbauen.
      # Begrüßung ausgeben.
      # In den Driver springen.
      #
      #ifdef AMIGAOS
      init_amiga();
      #endif
      #ifdef EMUNIX
      # Wildcards und Response-Files in der Kommandozeile expandieren:
      _response(&argc,&argv);
      _wildcard(&argc,&argv);
      #endif
      #ifdef DJUNIX
      # Ctrl-Break verbieten, so weit es geht:
      local var int cbrk;
      cbrk = getcbrk();
      if (cbrk) { setcbrk(0); }
      # Ctrl-Break wollen wir abfangen:
      _go32_want_ctrl_break(1);
      #endif
      #if defined(MSDOS) && 0 # normalerweise unnötig
      # Auf stdin und stdout im Text-Modus zugreifen:
      begin_system_call();
      setmode(stdin_handle,O_TEXT);
      setmode(stdout_handle,O_TEXT);
      end_system_call();
      #endif
      #ifdef WIN32_NATIVE
      init_win32();
      #endif
      #ifdef RISCOS
      # Disable UnixLib's automatic name munging:
      __uname_control = 1;
      #if !defined(HAVE_FFI)
      # Disable save/restore of floating-point registers in setjmp(), longjmp().
      # This gives a substantial performance increase, especially in the
      # interpreter. However, it is extremely hairy: It relies on the fact
      # that we don't use floating-point operations (except possibly in ffloat.d
      # or dfloat.d - where we don't use longjmp() and don't call any C code
      # which could perform a longjmp()). This optimization is not possible
      # if we intend to call foreign functions (and maybe longjmp out of a
      # Lisp callback, thus unwinding the stack of a C function which uses
      # floating-point registers).
      { extern int __fpflag; __fpflag = 0; }
      #endif
      # Attach "delete" behaviour to the "backspace" key.
      if (!getenv("Clisp$Backspace_Backspaces")) # this is user-configurable
        { # Fix UnixLib's interpretation of the normal "delete" key being
          # delete (and "backspace" key being Ctrl-H ?? - the Emacs disease).
          struct termio tin;
          begin_system_call();
          if (!( ioctl(0,TCGETA,&tin) ==0))
            { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } }
          tin.c_cc[VERASE] = BS;
          if (!( ioctl(0,TCSETA,&tin) ==0))
            { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } }
          end_system_call();
        }
      #endif
      #if defined(UNIX_LINUX) && (defined(FAST_FLOAT) || defined(FAST_DOUBLE)) && defined(HAVE_SETFPUCW)
      # Damit Division durch 0.0 ein NaN und kein SIGFPE liefert.
      __setfpucw(_FPU_IEEE);
      #endif
      #ifdef UNIX
      user_uid = getuid();
      #ifdef GRAPHICS_SWITCH
      # Programm muß mit "setuid root"-Privileg installiert werden:
      # (chown root, chmod 4755). Vom root-Privileg befreien wir uns so schnell
      # wie möglich - sicherheitshalber.
      { extern uid_t root_uid;
        root_uid = geteuid();
        setreuid(root_uid,user_uid);
      }
      #endif
      find_executable(argv[0]);
      #endif
     {var uintL argv_memneed = 0;
      #ifndef NO_SP_MALLOC
      var uintL argv_stackneed = 0;
      #endif
      #ifdef MULTIMAP_MEMORY_VIA_FILE
      var local char* argv_tmpdir = NULL;
      #endif
      var local boolean argv_wide = FALSE; # for backward compatibility
      var local char* argv_memfile = NULL;
      var local boolean argv_load_compiling = FALSE;
      var local uintL argv_init_filecount = 0;
      var local char** argv_init_files;
      var local boolean argv_compile = FALSE;
      var local boolean argv_compile_listing = FALSE;
      var local uintL argv_compile_filecount = 0;
      typedef struct { char* input_file; char* output_file; } argv_compile_file;
      var local argv_compile_file* argv_compile_files;
      var local char* argv_package = NULL;
      var local char* argv_expr = NULL;
      var local char* argv_execute_file = NULL;
      var local char** argv_execute_args = NULL;
      var local uintL argv_execute_arg_count;
      var local char* argv_language = NULL;
      var local char* argv_localedir = NULL;
      {var DYNAMIC_ARRAY(argv_init_files_array,char*,(uintL)argc); # maximal argc Init-Files
       argv_init_files = argv_init_files_array;
      {var DYNAMIC_ARRAY(argv_compile_files_array,argv_compile_file,(uintL)argc); # maximal argc File-Argumente
       argv_compile_files = argv_compile_files_array;
      if (!(setjmp(&!original_context) == 0)) goto end_of_main;
      #
      # Argumente argv[0..argc-1] abarbeiten:
      #   -h              Help
      #   -m size         Memory size (size = xxxxxxxB oder xxxxKB oder xMB)
      #   -s size         Stack size (size = xxxxxxxB oder xxxxKB oder xMB)
      #   -t directory    temporäres Directory
      #   -W              WIDE-Version wählen
      #   -M file         MEM-File laden
      #   -L language     sets the user language
      #   -N directory    NLS catalog directory
      #   -q              quiet: keine Copyright-Meldung
      #   -I              ILISP-freundlich
      #   -C              *LOAD-COMPILING* setzen
      #   -i file ...     LISP-File zur Initialisierung laden
      #   -c file ...     LISP-Files compilieren, dann LISP verlassen
      #   -l              Beim Compilieren: Listings anlegen
      #   -p package      *PACKAGE* setzen
      #   -x expr         LISP-Expressions ausführen, dann LISP verlassen
      #   file [arg ...]  LISP-File im Batch-Modus laden und ausführen,
      #                   dann LISP verlassen
      #
      # Neu hinzukommende Optionen müssen aufgeführt werden:
      # - in obiger Tabelle,
      # - in der usage-Meldung hier,
      # - im Optionsparser hier,
      # - im Optionsparser in _clisp.c,
      # - in den Manual-Pages _clisp.1 und _clisp.html.
      #
      program_name = argv[0]; # argv[0] ist der Programmname
      if (FALSE)
        { usage:
          asciz_out("Usage:  ");
          asciz_out(program_name);
          asciz_out(" [-h] [-m memsize]");
          #ifndef NO_SP_MALLOC
          asciz_out(" [-s stacksize]");
          #endif
          #ifdef MULTIMAP_MEMORY_VIA_FILE
          asciz_out(" [-t tmpdir]");
          #endif
          asciz_out(" [-W] [-M memfile] [-L language] [-N nlsdir] [-q] [-I] [-C]"
                    " [-i initfile ...] [-c [-l] lispfile [-o outputfile] ...]"
                    " [-p packagename] [-x expression] [lispfile [argument ...]]"
                    NLstring);
          quit_sofort(1); # anormales Programmende
        }
     {var char** argptr = &argv[1];
      var char** argptr_limit = &argv[argc];
      var enum { for_exec, for_init, for_compile } argv_for = for_exec;
      # Durchlaufen und Optionen abarbeiten, alles Abgearbeitete durch NULL
      # ersetzen:
      while (argptr < argptr_limit)
        { var char* arg = *argptr++; # nächstes Argument
          if ((arg[0] == '-') && !(arg[1] == '\0'))
            { switch (arg[1])
                { case 'h': # Help
                    goto usage;
                  # Liefert nach einem einbuchstabigen Kürzel den Rest der
                  # Option in arg. Evtl. Space wird übergangen.
                  #define OPTION_ARG  \
                    if (arg[2] == '\0') \
                      { if (argptr < argptr_limit) arg = *argptr++; else goto usage; } \
                      else { arg = &arg[2]; }
                  # Parst den Rest einer Option, die eine Byte-Größe angibt.
                  # Überprüft auch, ob gewisse Grenzen eingehalten werden.
                  #define SIZE_ARG(docstring,sizevar,limit_low,limit_high)  \
                    # arg sollte aus einigen Dezimalstellen, dann   \
                    # evtl. K oder M, dann evtl. B oder W bestehen. \
                    {var uintL val = 0;                             \
                     while ((*arg >= '0') && (*arg <= '9'))         \
                       { val = 10*val + (uintL)(*arg++ - '0'); }    \
                     switch (*arg)                                  \
                       { case 'k': case 'K': # Angabe in Kilobytes  \
                           val = val * 1024; arg++; break;          \
                         case 'm': case 'M': # Angabe in Megabytes  \
                           val = val * 1024*1024; arg++; break;     \
                       }                                            \
                     switch (*arg)                                  \
                       { case 'w': case 'W': # Angabe in Worten     \
                           val = val * sizeof(object);              \
                         case 'b': case 'B': # Angabe in Bytes      \
                           arg++; break;                            \
                       }                                            \
                     if (!(*arg == '\0')) # Argument zu Ende?       \
                       { asciz_out_s(DEUTSCH ? "Syntax für %s: nnnnnnn oder nnnnKB oder nMB" NLstring : \
                                     ENGLISH ? "Syntax for %s: nnnnnnn or nnnnKB or nMB" NLstring : \
                                     FRANCAIS ? "syntaxe pour %s: nnnnnnn ou nnnnKB ou nMB" NLstring : \
                                     "", docstring);                \
                         goto usage;                                \
                       }                                            \
                     if (!((val >= limit_low) && (val <= limit_high))) \
                       { asciz_out_s(DEUTSCH ? "%s ist nicht im gültigen Bereich" NLstring : \
                                     ENGLISH ? "%s out of range" NLstring : \
                                     FRANCAIS ? "%s n'est pas entre les bornes" NLstring : \
                                     "", docstring);                \
                         goto usage;                                \
                       }                                            \
                     # Bei mehreren -m bzw. -s Argumenten zählt nur das letzte. \
                     sizevar = val;                                 \
                    }
                  case 'm': # Memory size
                    #ifdef WIN32_NATIVE
                    if (arg[2]=='m' && arg[3]=='\0') # "-mm" -> print a memory map
                      { DumpProcessMemoryMap(); quit_sofort(1); }
                    #endif
                    OPTION_ARG
                    SIZE_ARG(DEUTSCH ? "Speichervorrat" :
                             ENGLISH ? "memory size" :
                             FRANCAIS ? "taille mémoire" :
                             "",
                             argv_memneed,100000,
                             (oint_addr_len+addr_shift < intLsize-1 # memory size begrenzt durch
                              ? bitm(oint_addr_len+addr_shift)      # Adreßraum in oint_addr_len+addr_shift Bits
                              : (uintL)bit(intLsize-1)-1            # (bzw. große Dummy-Grenze)
                            ))
                    break;
                  #ifndef NO_SP_MALLOC
                  case 's': # Stack size
                    OPTION_ARG
                    SIZE_ARG(DEUTSCH ? "Stackspeichervorrat" :
                             ENGLISH ? "stack size" :
                             FRANCAIS ? "taille de pile" :
                             "",
                             argv_stackneed,40000,8*1024*1024)
                    break;
                  #endif
                  #ifdef MULTIMAP_MEMORY_VIA_FILE
                  case 't': # temporäres Directory
                    OPTION_ARG
                    if (!(argv_tmpdir == NULL)) goto usage;
                    argv_tmpdir = arg;
                    break;
                  #endif
                  case 'W': # WIDE-Version wählen, for backward compatibility
                    argv_wide = TRUE;
                    if (!(arg[2] == '\0')) goto usage;
                    break;
                  case 'M': # MEM-File
                    OPTION_ARG
                    # Bei mehreren -M Argumenten zählt nur das letzte.
                    argv_memfile = arg;
                    break;
                  case 'L': # Language
                    OPTION_ARG
                    # Bei mehreren -L Argumenten zählt nur das letzte.
                    argv_language = arg;
                    break;
                  case 'N': # NLS-Directory
                    OPTION_ARG
                    # Bei mehreren -N Argumenten zählt nur das letzte.
                    argv_localedir = arg;
                    break;
                  case 'q': # keine Copyright-Meldung
                    argv_quiet = TRUE;
                    if (!(arg[2] == '\0')) goto usage;
                    break;
                  case 'I': # ILISP-freundlich
                    ilisp_mode = TRUE;
                    if (!(arg[2] == '\0')) goto usage;
                    break;
                  case 'C': # *LOAD-COMPILING* setzen
                    argv_load_compiling = TRUE;
                    if (!(arg[2] == '\0')) goto usage;
                    break;
                  case 'i': # Initialisierungs-Files
                    argv_for = for_init;
                    if (!(arg[2] == '\0')) goto usage;
                    break;
                  case 'c': # Zu compilierende Files
                    argv_compile = TRUE;
                    argv_for = for_compile;
                    if (arg[2] == 'l')
                      { argv_compile_listing = TRUE;
                        if (!(arg[3] == '\0')) goto usage;
                      }
                      else
                      { if (!(arg[2] == '\0')) goto usage; }
                    break;
                  case 'l': # Compilate und Listings
                    argv_compile_listing = TRUE;
                    if (!(arg[2] == '\0')) goto usage;
                    break;
                  case 'o': # Ziel für zu compilierendes File
                    if (!(arg[2] == '\0')) goto usage;
                    OPTION_ARG
                    if (!((argv_compile_filecount > 0) && (argv_compile_files[argv_compile_filecount-1].output_file==NULL))) goto usage;
                    argv_compile_files[argv_compile_filecount-1].output_file = arg;
                    break;
                  case 'p': # Package
                    OPTION_ARG
                    # Bei mehreren -p Argumenten zählt nur das letzte.
                    argv_package = arg;
                    break;
                  case 'x': # LISP-Expression ausführen
                    OPTION_ARG
                    if (!(argv_expr == NULL)) goto usage;
                    argv_expr = arg;
                    break;
                  case '-': # -- Optionen im GNU-Stil
                    if (asciz_equal(&arg[2],"help"))
                      goto usage;
                    elif (asciz_equal(&arg[2],"version"))
                      { if (!(argv_expr == NULL)) goto usage;
                        argv_quiet = TRUE;
                        argv_expr = "(PROGN (FORMAT T \"CLISP ~A\" (LISP-IMPLEMENTATION-VERSION)) (LISP:EXIT))";
                        break;
                      }
                    elif (asciz_equal(&arg[2],"quiet") || asciz_equal(&arg[2],"silent"))
                      { argv_quiet = TRUE; break; }
                    else
                      goto usage; # Unbekannte Option
                    break;
                  default: # Unbekannte Option
                    goto usage;
            }   }
            else
            # keine Option,
            # wird als zu ladendes / zu compilierendes / auszuführendes File
            # interpretiert.
            { switch (argv_for)
                { case for_init:
                    argv_init_files[argv_init_filecount++] = arg; break;
                  case for_compile:
                    argv_compile_files[argv_compile_filecount].input_file = arg;
                    argv_compile_files[argv_compile_filecount].output_file = NULL;
                    argv_compile_filecount++;
                    break;
                  case for_exec:
                    argv_execute_file = arg;
                    # Alle weiteren Argumente sind Argumente zu argv_execute_file.
                    argv_execute_args = argptr;
                    argv_execute_arg_count = argptr_limit - argptr;
                    argptr = argptr_limit; # Schleife abbrechen
                    break;
                  default:
                    NOTREACHED;
            }   }
        }
      # Optionen semantisch überprüfen und Defaults eintragen:
      if (argv_memneed == 0)
        #if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && defined(GENERATIONAL_GC)
        # Wegen GENERATIONAL_GC wird der Speicherbereich kaum ausgeschöpft.
        { argv_memneed = 3584*1024*sizeof(object); } # 3584 KW = 14 MB Default
        #else
        # normalerweise
        { argv_memneed = 512*1024*sizeof(object); } # 512 KW = 2 MB Default
        #endif
      #ifdef MULTIMAP_MEMORY_VIA_FILE
      if (argv_tmpdir == NULL)
        { argv_tmpdir = getenv("TMPDIR"); # Environment-Variable probieren
          if (argv_tmpdir == NULL)
            { argv_tmpdir = "/tmp"; }
        }
      #endif
      #ifdef UNIX
      if (!(argv_memfile == NULL))
        { # Search a ':' in argv_memfile, for backward compatibility.
          var char* ptr = argv_memfile;
          until (*ptr == '\0' || *ptr == ':') { ptr++; }
          if (*ptr != '\0')
            { if (argv_wide)
                # Choose second pathname, after the colon.
                { argv_memfile = ptr+1; }
                else
                # Choose first pathname, before the colon.
                { *ptr = '\0'; }
        }   }
      #endif
      #ifndef LANGUAGE_STATIC
      init_language(argv_language,argv_localedir);
      #endif
      if (!argv_compile)
        # Manche Optionen sind nur zusammen mit '-c' sinnvoll:
        { if (argv_compile_listing) goto usage; }
        else
        # Andere Optionen sind nur ohne '-c' sinnvoll:
        { if (!(argv_expr == NULL)) goto usage; }
      if (argv_expr && argv_execute_file) goto usage;
     }
     # Tabelle von Fehlermeldungen initialisieren:
     if (init_errormsg_table()<0) goto no_mem;
     # <ctype.h>-Funktionen 8-bit clean machen, sofern die Environment-Variable
     # LC_CTYPE passend gesetzt ist:
     # (Wir verwenden diese Funktionen zwar nicht direkt, aber Zusatzmodule wie
     # z.B. regexp profitieren davon.)
     #ifdef HAVE_LOCALE_H
     { var const char * locale;
       { locale = getenv("CLISP_LC_CTYPE");
         if (!locale)
           { locale = getenv("GNU_LC_CTYPE");
             if (!locale)
               { locale = getenv("LC_CTYPE"); }
       }   }
       if (locale)
         { setlocale(LC_CTYPE,locale); }
     }
     #endif
     # Speicher holen:
     #if (defined(SINGLEMAP_MEMORY) || defined(TRIVIALMAP_MEMORY) || defined(MULTITHREAD)) && (defined(HAVE_MMAP_ANON) || defined(HAVE_MMAP_DEVZERO) || defined(HAVE_MACH_VM) || defined(HAVE_WIN32_VM))
     mmap_init_pagesize();
     #endif
     #if defined(MULTIMAP_MEMORY) || defined(SINGLEMAP_MEMORY) || defined(TRIVIALMAP_MEMORY)
     init_map_pagesize();
     #endif
     #ifdef SPVW_PURE
     init_mem_heaptypes();
     init_objsize_table();
     #endif
     #if defined(SPVW_MIXED_BLOCKS) && defined(TYPECODES) && defined(GENERATIONAL_GC)
     init_mem_heapnr_from_type();
     #endif
     init_modules_0(); # Liste der Module zusammensetzen
     #ifdef MULTITHREAD
     init_multithread();
     create_thread((void*)roughly_SP());
     #endif
     #ifdef MAP_MEMORY_TABLES
     # total_subr_anz bestimmen:
     { var uintC total = 0;
       var module_* module;
       for_modules(all_modules, { total += *module->stab_size; } );
       total_subr_anz = total;
     }
     #endif
     {# Aufteilung des Gesamtspeichers in Teile:
      #define teile             16  # 16/16
        #ifdef NO_SP_MALLOC # wird SP vom Betriebssystem bereitgestellt?
        #define teile_SP         0
        #else
        #define teile_SP         2  # 2/16 (1/16 reicht oft nicht)
        #endif
        #define teile_STACK      2  # 2/16
        #ifdef HAVE_NUM_STACK
        #define teile_NUM_STACK  1  # 1/16
        #else
        #define teile_NUM_STACK  0
        #endif
        #define teile_stacks     (teile_SP + teile_STACK + teile_NUM_STACK)
        #ifdef SPVW_MIXED_BLOCKS
        #define teile_objects    (teile - teile_stacks)  # Rest
        #else
        #define teile_objects    0
        #endif
      var uintL pagesize = # Länge einer Speicherseite
        #if defined(MULTIMAP_MEMORY)
        map_pagesize
        #elif defined(SELFMADE_MMAP) || defined(GENERATIONAL_GC)
        mmap_pagesize
        #else # wenn die System-Speicherseiten-Länge keine Rolle spielt
        teile*varobject_alignment
        #endif
        ;
      var uintL memneed = argv_memneed; # benötigter Speicher
      var aint memblock; # untere Adresse des bereitgestellten Speicherblocks
      #if !(defined(SPVW_MIXED_BLOCKS_OPPOSITE) && !defined(TRIVIALMAP_MEMORY))
      memneed = teile_stacks*floor(memneed,teile); # noch keinen Speicher für objects berechnen
      #undef teile
      #define teile  teile_stacks
      #endif
      #ifndef NO_SP_MALLOC
      if (!(argv_stackneed==0))
        { memneed = memneed*(teile-teile_SP)/teile;
          # Die mit Option -s angegebene SP-Größe ist noch nicht in memneed inbegriffen.
          memneed = memneed + argv_stackneed;
        }
      #endif
      #if defined(TRIVIALMAP_MEMORY) && defined(WIN32_NATIVE)
      # Somehow the RESERVE_FOR_MALLOC limit for mallocs after prepare_zeromap() seems
      # also to encompass the mallocs before prepare_zeromap(). Don't know why.
      if (memneed > RESERVE_FOR_MALLOC*3/4) { memneed = RESERVE_FOR_MALLOC*3/4; }
      #endif
      #if defined(MULTIMAP_MEMORY_VIA_SHM) && (defined(UNIX_SUNOS4) || defined(UNIX_SUNOS5))
      # SunOS 4 weigert sich, ein shmat() in einen vorher mallozierten Bereich
      # hinein zu machen, selbst wenn dawischen ein munmap() liegt:
      # errno = EINVAL. Auch das Umgekehrte, erst shmat() zu machen und dann
      # mit sbrk() oder brk() den belegten Bereich dem Datensegment einzu-
      # verleiben, scheitert mit errno = ENOMEM.
      # Der einzige Ausweg ist, sich den benötigten Speicher von weit weg,
      # möglichst außer Reichweite von malloc(), zu holen.
      { var uintL memhave = round_down(bit(oint_addr_len) - (aint)sbrk(0),SHMLBA);
        if (memhave < memneed) { memneed = memhave; }
        memblock = round_down(bit(oint_addr_len) - memneed,SHMLBA);
      }
      #else
      loop
        { memblock = (aint)mymalloc(memneed); # Speicher allozieren versuchen
          if (!((void*)memblock == NULL)) break; # gelungen -> OK
          memneed = floor(memneed,8)*7; # sonst mit 7/8 davon nochmals versuchen
          if (memneed == 0) break;
        }
      if (memneed == 0)
        { begin_system_call();
          memblock = (aint)malloc(1);
          end_system_call();
          asciz_out_1(DEUTSCH ? "Ergebnis von malloc() = %x ist nicht mit der Typcodeverteilung kompatibel." NLstring :
                      ENGLISH ? "Return value of malloc() = %x is not compatible with type code distribution." NLstring :
                      FRANCAIS ? "La valeur de malloc() = %x n'est pas compatible avec le codage des types." NLstring :
                      "",
                      memblock
                     );
          goto no_mem;
        }
      if (memneed < MINIMUM_SPACE+RESERVE) # aber mit weniger als MINIMUM_SPACE
        # geben wir uns nicht zufrieden:
        { asciz_out_1(DEUTSCH ? "Nur %d Bytes verfügbar." NLstring :
                      ENGLISH ? "Only %d bytes available." NLstring :
                      FRANCAIS ? "Seuls %d octets libres." NLstring :
                      "",
                      memneed
                     );
          goto no_mem;
        }
      #endif
      #ifdef MULTIMAP_MEMORY
      # Wir brauchen zwar nur diesen Adreßraum und nicht seinen Inhalt, dürfen
      # ihn aber nicht freigeben, da er in unserer Kontrolle bleiben soll.
      #endif
      # Aufrunden zur nächsten Speicherseitengrenze:
      {var uintL unaligned = (uintL)(-memblock) % pagesize;
       memblock += unaligned; memneed -= unaligned;
      }
      # Abrunden zur letzen Speicherseitengrenze:
      {var uintL unaligned = memneed % pagesize;
       memneed -= unaligned;
      }
      # Der Speicherbereich [memblock,memblock+memneed-1] ist nun frei,
      # und seine Grenzen liegen auf Speicherseitengrenzen.
      #ifdef MULTIMAP_MEMORY
        #ifdef MULTIMAP_MEMORY_VIA_FILE
        if ( initmap(argv_tmpdir) <0) goto no_mem;
        #else
        if ( initmap() <0) goto no_mem;
        #endif
        multimap(case_machine: MM_TYPECASES, memblock, memneed, FALSE);
        #ifdef MAP_MEMORY_TABLES
        # Dazu noch symbol_tab an die Adresse 0 legen:
        {var uintL memneed = round_up(sizeof(symbol_tab),pagesize); # Länge aufrunden
         multimap(case_symbolflagged: , 0, memneed, FALSE);
        }
        # Dazu noch subr_tab an die Adresse 0 legen:
        if ( zeromap(&subr_tab,round_up(total_subr_anz*sizeof(subr_),pagesize)) <0) goto no_mem;
        #else
        # Dazu noch symbol_tab und subr_tab multimappen:
        # Die symbol_tab und subr_tab behalten dabei ihre Adresse. Der Bereich,
        # in dem sie liegen (im Datensegment des Programms!!), wird zu Shared
        # Memory bzw. Shared-mmap-Attach gemacht. Was für ein Hack!
        # Dies ist mit der Existenz externer Module (DYNAMIC_MODULES) unvereinbar! ??
        {var aint symbol_tab_start = round_down((aint)&symbol_tab,pagesize);
         var aint symbol_tab_end = round_up((aint)&symbol_tab+sizeof(symbol_tab),pagesize);
         var aint subr_tab_start = round_down((aint)&subr_tab,pagesize);
         var aint subr_tab_end = round_up((aint)&subr_tab+sizeof(subr_tab),pagesize);
         if ((symbol_tab_end <= subr_tab_start) || (subr_tab_end <= symbol_tab_start))
           # zwei getrennte Intervalle
           { multimap(case_machine: case_symbolflagged: , symbol_tab_start, symbol_tab_end-symbol_tab_start, TRUE);
             multimap(case_machine: case_subr: , subr_tab_start, subr_tab_end-subr_tab_start, TRUE);
           }
           else
           # die Tabellen überlappen sich!
           { var aint tab_start = (symbol_tab_start < subr_tab_start ? symbol_tab_start : subr_tab_start);
             var aint tab_end = (symbol_tab_end > subr_tab_end ? symbol_tab_end : subr_tab_end);
             multimap(case_machine: case_symbolflagged: case_subr: , tab_start, tab_end-tab_start, TRUE);
           }
        }
        #endif
      #endif
      #if defined(SINGLEMAP_MEMORY) || defined(TRIVIALMAP_MEMORY) # <==> SPVW_PURE_BLOCKS || TRIVIALMAP_MEMORY
        if ( initmap() <0) goto no_mem;
        #ifdef SINGLEMAP_MEMORY
        # Alle Heaps vor-initialisieren:
        { var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { var Heap* heapptr = &mem.heaps[heapnr];
              heapptr->heap_limit = (aint)type_zero_oint(heapnr);
              heapptr->heap_hardlimit = (aint)type_zero_oint(heapnr+1);
              if (mem.heaptype[heapnr] >= -1)
                if ( prepare_zeromap(&heapptr->heap_limit,&heapptr->heap_hardlimit,TRUE) <0) goto no_mem;
        }   }
        # Dazu noch symbol_tab, subr_tab an die Adresse 0 legen:
        # (Hierzu muß case_symbolflagged mit case_symbol äquivalent sein!)
        #define map_tab(tab,size)  \
          { var uintL map_len = round_up(size,map_pagesize); \
            if ( zeromap(&tab,map_len) <0) goto no_mem;      \
            mem.heaps[typecode(as_object((oint)&tab))].heap_limit += map_len; \
          }
        map_tab(symbol_tab,sizeof(symbol_tab));
        map_tab(subr_tab,total_subr_anz*sizeof(subr_));
        #endif
        #ifdef TRIVIALMAP_MEMORY
        # Alle Heaps als leer initialisieren.
        # Dabei den gesamten zur Verfügung stehenden Platz im Verhältnis
        # 1:1 aufteilen, falls er knapp ist. Sonst die beiden Heaps bei
        # 1/5 bzw. 2/5 des Adreßbereiches ansetzen. (Ein "krummer" Nenner,
        # um diversen Shared-Library-Regionen aus dem Weg zu gehen.)
        { var void* malloc_addr = malloc(1);
          var aint start = round_up((aint)malloc_addr+RESERVE_FOR_MALLOC,map_pagesize); # Reserve für malloc()
          #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
           #if defined(SUN4_29)
           var aint end = bitm(oint_addr_len+addr_shift < 29 ? oint_addr_len+addr_shift : 29);
           mem.heaps[0].heap_limit = start + round_down(floor(end-start,5),map_pagesize);
           mem.heaps[1].heap_limit = round_down(end,map_pagesize);
           #elif defined(UNIX_LINUX) && defined(WIDE_SOFT) && !defined(SPARC)
           mem.heaps[0].heap_limit = 0x2E000000; # room until at least 0x40000000
           mem.heaps[1].heap_limit = 0x7F000000; # room until at least 0x64000000
           #else
           #ifdef TYPECODES
           var aint end = bitm(oint_addr_len+addr_shift);
           #else
           var aint end = bitm(oint_addr_len-1); # keep garcol_bit zero
           #endif
           var aint part = floor(end - (start & (end-1)),5);
           mem.heaps[0].heap_limit = start + round_down(1*part,map_pagesize);
           mem.heaps[1].heap_limit = start + round_down(4*part,map_pagesize);
           #endif
           if ( prepare_zeromap(&mem.heaps[0].heap_limit,&mem.heaps[1].heap_limit,FALSE) <0) goto no_mem;
          #else # SPVW_MIXED_BLOCKS_STAGGERED
           #if defined(SUN4_29)
           var aint end = bitm(oint_addr_len+addr_shift < 29 ? oint_addr_len+addr_shift : 29);
           mem.heaps[0].heap_limit = start + round_down(floor(end-start,5),map_pagesize);
           mem.heaps[0].heap_hardlimit =
           mem.heaps[1].heap_limit = start + round_down(floor((end-start)*3,5),map_pagesize);
           mem.heaps[1].heap_hardlimit = end;
           #elif defined(UNIX_LINUX) && defined(WIDE_SOFT) && !defined(SPARC)
           mem.heaps[0].heap_limit = 0x2E000000; # room until at least 0x40000000
           mem.heaps[0].heap_hardlimit = 0x40000000;
           mem.heaps[1].heap_limit = 0x64000000; # room until at least 0x7F000000
           mem.heaps[1].heap_hardlimit = 0x7F000000;
           #else
           #ifdef TYPECODES
           var aint end = bitm(oint_addr_len+addr_shift);
           #else
           var aint end = bitm(oint_addr_len-1); # keep garcol_bit zero
           #endif
           var aint part = floor(end - (start & (end-1)),5);
           mem.heaps[0].heap_limit = start + round_down(1*part,map_pagesize);
           mem.heaps[0].heap_hardlimit =
           mem.heaps[1].heap_limit = start + round_down(2*part,map_pagesize);
           mem.heaps[1].heap_hardlimit = start + round_down(3*part,map_pagesize);
           #endif
           if ( prepare_zeromap(&mem.heaps[0].heap_limit,&mem.heaps[1].heap_hardlimit,FALSE) <0) goto no_mem;
          #endif
          free(malloc_addr);
        }
        #endif
        # Alle Heaps als leer initialisieren:
        { var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { var Heap* heapptr = &mem.heaps[heapnr];
              heapptr->heap_start = heapptr->heap_end = heapptr->heap_limit;
              #ifdef SELFMADE_MMAP
              heapptr->memfile_numpages = 0;
              # heapptr->memfile_pages = NULL; # irrelevant
              # heapptr->memfile_offset = 0; # irrelevant
              #endif
              #ifdef GENERATIONAL_GC
              heapptr->heap_gen0_start = heapptr->heap_gen0_end = heapptr->heap_gen1_start = heapptr->heap_limit;
              heapptr->physpages = NULL;
              #endif
        }   }
       #ifdef SINGLEMAP_MEMORY_STACK
        # STACK initialisieren:
        { var uintL map_len = round_up(memneed * teile_STACK/teile, map_pagesize);
          # Der Stack belegt das Intervall von 0 bis map_len bei Typcode = system_type:
          var aint low = (aint)type_zero_oint(system_type);
          var aint high = low + map_len;
          if ( prepare_zeromap(&low,&high,TRUE) <0) goto no_mem;
          if ( zeromap((void*)low,map_len) <0) goto no_mem;
          #ifdef STACK_DOWN
            STACK_bound = (object*)low + 0x40; # 64 Pointer Sicherheitsmarge
            setSTACK(STACK = (object*)high); # STACK initialisieren
          #endif
          #ifdef STACK_UP
            setSTACK(STACK = (object*)low); # STACK initialisieren
            STACK_bound = (object*)high - 0x40; # 64 Pointer Sicherheitsmarge
          #endif
        }
        #undef teile_STACK
        #define teile_STACK 0  # brauche keinen Platz mehr für den STACK
        #if (teile==0)
          #undef teile
          #define teile 1  # Division durch 0 vermeiden
        #endif
       #endif
      #endif
      #if defined(SELFMADE_MMAP) || defined(GENERATIONAL_GC)
      init_physpagesize();
      #endif
      # Speicherblock aufteilen:
      { var uintL free_reserved; # Anzahl reservierter Bytes
        #ifndef NO_SP_MALLOC
        var void* initial_SP; # Initialwert für SP-Stackpointer
        var uintL for_SP = 0; # Anzahl Bytes für SP-Stack
        #define min_for_SP  40000 # minimale SP-Stack-Größe
        #endif
        var uintL for_STACK; # Anzahl Bytes für Lisp-STACK
        var uintL for_NUM_STACK; # Anzahl Bytes für Zahlen-STACK
        var uintL for_objects; # Anzahl Bytes für Lisp-Objekte
        # Der STACK braucht Alignment, da bei Frame-Pointern das letzte Bit =0 sein muß:
        #define STACK_alignment  bit(addr_shift+1)
        #define alignment  (varobject_alignment>STACK_alignment ? varobject_alignment : STACK_alignment)
        free_reserved = memneed;
        #ifndef NO_SP_MALLOC
        if (!(argv_stackneed==0))
          if (2*argv_stackneed <= free_reserved) # nicht zu viel für den SP-Stack reservieren
            { for_SP = round_down(argv_stackneed,varobject_alignment);
              free_reserved -= argv_stackneed;
            }
        #endif
        # Durch teile*alignment teilbar machen, damit jedes Sechzehntel aligned ist:
        free_reserved = round_down(free_reserved,teile*alignment);
        free_reserved = free_reserved - RESERVE;
       {var uintL teil = free_reserved/teile; # ein Teilblock, ein Sechzehntel des Platzes
        var aint ptr = memblock;
        mem.MEMBOT = ptr;
        #ifdef NO_SP_MALLOC
          #ifdef UNIX_NEXTSTEP
            # Set the stack size limit to 8 MB if possible to prevent
            # crashes from machine stack overflow.
            # (If the stack is large enough, the Lisp STACK will overflow
            # first, and the error will be handled in a reasonable way.)
            { var struct rlimit rl;
              var long need = 0x800000; # 8 Megabyte
              getrlimit(RLIMIT_STACK,&rl);
              if (rl.rlim_max < need) { need = rl.rlim_max; }
              if (rl.rlim_cur < need)
                { rl.rlim_cur = need; setrlimit(RLIMIT_STACK,&rl); }
            }
          #endif
          #ifdef AMIGAOS
          { var struct Process * myprocess = (struct Process *)FindTask(NULL);
            var aint original_SP = process->pr_ReturnAddr; # SP beim Programmstart
            # Die Shell legt die Stackgröße vor dem Start auf den SP.
            var aint SP_bottom = original_SP - *(ULONG*)original_SP;
            SP_bound = SP_bottom + 0x1000; # 1024 Pointer Sicherheitsmarge
          }
          #endif
          #ifdef WIN32_NATIVE
            # Even if the NOCOST_SP_CHECK stack overflow detection (using a
            # guard page) works, we set SP_bound.
            # Normally, the stack's `AllocationBase' is = 0x30000, the guard
            # page is 0x32000-0x32FFF, hence we can set SP_bound = 0x34000.
            { var MEMORY_BASIC_INFORMATION info;
              if (!(VirtualQuery((void*)SP(),&info,sizeof(info)) == sizeof(info)))
                { asciz_out(DEUTSCH ? "Konnte das Ende des SP-Stacks nicht herausfinden!" NLstring :
                            ENGLISH ? "Couldn't determine the end of the SP stack!" NLstring :
                            FRANCAIS ? "Impossible de savoir où se termine la pile SP!" NLstring :
                            ""
                           );
                  SP_bound = 0;
                }
                else
                { # 0x4000 might be enough, but 0x8000 will be better.
                  SP_bound = (void*)((aint)info.AllocationBase + 0x8000);
            }   }
          #endif
        #else
          # SP allozieren:
          if (for_SP==0)
            { for_SP = teile_SP*teil; } # 2/16 für Programmstack
            else
            # Platz für SP ist schon abgezwackt.
            { # teile := teile-teile_SP; # geht nicht mehr, stattdessen:
              teil = round_down(free_reserved/(teile-teile_SP),alignment);
            }
          if (for_SP < min_for_SP) { for_SP = round_up(min_for_SP,alignment); } # aber nicht zu wenig
          #ifdef SP_DOWN
            SP_bound = (void*)(ptr + 0x800); # 512 Pointer Sicherheitsmarge
            ptr += for_SP;
            initial_SP = (void*)ptr;
          #endif
          #ifdef SP_UP
            initial_SP = (void*)ptr;
            ptr += for_SP;
            SP_bound = (void*)(ptr - 0x800); # 512 Pointer Sicherheitsmarge
          #endif
        #endif
        # STACK allozieren:
        #ifdef SINGLEMAP_MEMORY_STACK
        for_STACK = 0; # STACK ist schon woanders alloziert.
        #else
        #ifdef STACK_DOWN
          STACK_bound = (object*)ptr + 0x40; # 64 Pointer Sicherheitsmarge
          ptr += for_STACK = teile_STACK*teil; # 2/16 für Lisp-STACK
          setSTACK(STACK = (object*)ptr); # STACK initialisieren
        #endif
        #ifdef STACK_UP
          setSTACK(STACK = (object*)ptr); # STACK initialisieren
          ptr += for_STACK = teile_STACK*teil; # 2/16 für Lisp-STACK
          STACK_bound = (object*)ptr - 0x40; # 64 Pointer Sicherheitsmarge
        #endif
        #endif
        #ifdef HAVE_NUM_STACK
        # NUM_STACK allozieren:
        #ifdef NUM_STACK_DOWN
          NUM_STACK_bound = (uintD*)ptr;
          ptr += for_NUM_STACK = teile_NUM_STACK*teil; # 1/16 für Zahlen-STACK
          NUM_STACK = NUM_STACK_normal = (uintD*)round_down(ptr,sizeof(uintD)); # NUM_STACK initialisieren
        #endif
        #ifdef NUM_STACK_UP
          NUM_STACK = NUM_STACK_normal = (uintD*)round_up(ptr,sizeof(uintD)); # NUM_STACK initialisieren
          ptr += for_NUM_STACK = teile_NUM_STACK*teil; # 1/16 für Zahlen-STACK
          NUM_STACK_bound = (uintD*)ptr;
        #endif
        #else
        for_NUM_STACK = 0; # kein Zahlen-Stack vorhanden
        #endif
        #if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && !defined(TRIVIALMAP_MEMORY)
        # Nun fangen die Lisp-Objekte an:
        #ifdef GENERATIONAL_GC
        mem.varobjects.heap_gen0_start = mem.varobjects.heap_gen0_end =
          mem.varobjects.heap_gen1_start =
            mem.varobjects.heap_start = (ptr + (physpagesize-1)) & -physpagesize;
        #else
        mem.varobjects.heap_start = ptr;
        #endif
        mem.varobjects.heap_end = mem.varobjects.heap_start; # Noch gibt es keine Objekte variabler Länge
        # Rest (14/16 oder etwas weniger) für Lisp-Objekte:
        for_objects = memblock+free_reserved - ptr; # etwa = teile_objects*teil
        ptr += for_objects;
        #ifdef GENERATIONAL_GC
        mem.conses.heap_gen0_start = mem.conses.heap_gen0_end =
          mem.conses.heap_gen1_end =
            mem.conses.heap_end = ptr & -physpagesize;
        #else
        mem.conses.heap_end = ptr;
        #endif
        mem.conses.heap_start = mem.conses.heap_end; # Noch gibt es keine Conses
        # ptr = memblock+free_reserved, da 2/16 + 14/16 = 1
        # Reservespeicher allozieren:
        ptr += RESERVE;
        # oberes Speicherende erreicht.
        mem.MEMTOP = ptr;
        # Darüber (weit weg) der Maschinenstack.
        #endif
        #if defined(SPVW_PURE_BLOCKS) || defined(TRIVIALMAP_MEMORY) || defined(GENERATIONAL_GC)
        mem.total_room = 0;
        #ifdef GENERATIONAL_GC
        mem.last_gcend_space0 = 0;
        mem.last_gcend_space1 = 0;
        #endif
        #endif
        #ifdef SPVW_PAGES
        for_each_heap(heap, { heap->inuse = EMPTY; } );
        for_each_cons_heap(heap, { heap->lastused = dummy_lastused; } );
        dummy_lastused->page_room = 0;
        mem.free_pages = NULL;
        mem.total_space = 0;
        mem.used_space = 0;
        mem.last_gcend_space = 0;
        mem.gctrigger_space = 0;
        #endif
        # Stacks initialisieren:
        #ifndef NO_SP_MALLOC
          #ifdef GNU
            # eine kleine Dummy-Aktion, die ein hinausgezögertes Aufräumen des SP
            # zu einem späteren Zeitpunkt verhindert:
            if (mem.MEMBOT) { asciz_out(""); }
          #endif
          setSP(initial_SP); # SP setzen! Dabei gehen alle lokalen Variablen verloren!
        #endif
        #ifdef NOCOST_SP_CHECK
          install_stackoverflow_handler(0x4000); # 16 KB reserve should be enough
        #endif
        pushSTACK(nullobj); pushSTACK(nullobj); # Zwei Nullpointer als STACKende-Kennung
     }}}
      init_subr_tab_1(); # subr_tab initialisieren
      if (argv_memfile==NULL)
        # Zu-Fuß-Initialisierung:
        { initmem(); }
        else
        # Speicherfile laden:
        { loadmem(argv_memfile); }
      init_other_modules_2(); # die noch uninitialisierten Module initialisieren
      # aktuelle Evaluator-Environments auf den Toplevel-Wert setzen:
      aktenv.var_env   = NIL;
      aktenv.fun_env   = NIL;
      aktenv.block_env = NIL;
      aktenv.go_env    = NIL;
      aktenv.decl_env  = O(top_decl_env);
      # Alles fertig initialisiert.
      subr_self = NIL; # irgendein gültiges Lisp-Objekt
      clear_break_sems(); set_break_sem_1();
      # Interrupt-Handler einrichten:
      #if defined(HAVE_SIGNALS) && defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
        install_sigwinch_handler();
      #endif
      # Die Größe des Terminal-Fensters auch jetzt beim Programmstart erfragen:
      #if defined(HAVE_SIGNALS)
        begin_system_call();
        update_linelength();
        end_system_call();
      #endif
      #if defined(MSDOS)
        # Die Breite des Bildschirms im aktuellen Bildschirm-Modus
        # jetzt beim Programmstart erfragen:
        if (isatty(stdout_handle)) # Standard-Output ein Terminal?
          { extern uintW v_cols(); # siehe STREAM.D
            #ifdef EMUNIX_PORTABEL
            var int scrsize[2];
            var uintL columns;
            columns = (_scrsize(&!scrsize), scrsize[0]);
            #else
            var uintL columns = v_cols();
            #endif
            if (columns > 0)
              { # Wert von SYS::*PRIN-LINELENGTH* verändern:
                Symbol_value(S(prin_linelength)) = fixnum(columns-1);
          }   }
      #endif
      #if defined(AMIGAOS) && 0
        # frage beim console.driver nach??
        if (IsInteractive(stdin_handle) && IsInteractive(stdout_handle)) # ??
          { var uintL len;
            var uintB question[4] = { CSI, '0', ' ', 'q' };
            var uintB response[30+1];
            Write(stdout_handle,question,4);
            len = Read(stdin_handle,response,30);
            response[len] = `\0`; sscanf(&response[5],"%d;%d", &lines, &columns); # ??
          }
      #endif
      #if (defined(HAVE_SIGNALS) && (defined(UNIX) || defined(EMUNIX) || defined(RISCOS))) || defined(WIN32_NATIVE)
        # Ctrl-C-Handler einsetzen:
        install_sigint_handler();
      #endif
      #if defined(SELFMADE_MMAP) || defined(GENERATIONAL_GC)
        # Page-Fault-Handler einsetzen:
        install_segv_handler();
      #endif
      #ifdef HAVE_SIGNALS
        install_sigcld_handler();
      #endif
      # Zeitvariablen initialisieren:
      init_time();
      # Stream-Variablen initialisieren:
      init_streamvars(!(argv_execute_file == NULL));
      #ifdef NEXTAPP
      # nxterminal-Stream funktionsfähig machen:
      if (nxterminal_init())
        { final_exitcode = 17; quit(); }
      #endif
      # Break ermöglichen:
      end_system_call();
      clr_break_sem_1();
      # Pathnames initialisieren:
      init_pathnames();
      #ifdef REXX
      # Rexx-Interface initialisieren:
      init_rexx();
      # Auf eine Fehlermeldung im Falle des Scheiterns verzichten wir.
      # Deswegen wollen wir das CLISP doch nicht unbrauchbar machen!
      #endif
      #ifdef DYNAMIC_FFI
      # FFI initialisieren:
      init_ffi();
      #endif
      # Modul-Initialisierungen:
      { var module_* module; # modules durchgehen
        for_modules(all_other_modules,
          { if (module->initfunction2)
              # Initialisierungsfunktion aufrufen:
              (*module->initfunction2)(module);
          });
      }
      # Sonstige Initialisierungen:
      { pushSTACK(Symbol_value(S(init_hooks))); # SYS::*INIT-HOOKS*
        while (mconsp(STACK_0)) # abarbeiten
          { var object obj = STACK_0;
            STACK_0 = Cdr(obj); funcall(Car(obj),0);
          }
        skipSTACK(1);
      }
      # Begrüßung ausgeben:
      if (!nullp(Symbol_value(S(quiet)))) # SYS::*QUIET* /= NIL ?
        { argv_quiet = TRUE; } # verhindert die Begrüßung
      if (!(argv_execute_file == NULL)) # Batch-Modus ?
        { argv_quiet = TRUE; } # verhindert die Begrüßung
      if (!argv_quiet)
        { local const char * const banner[] = { # einige Zeilen à 66 Zeichen
          #  |Spalte 0           |Spalte 20                                    |Spalte 66
            "  i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo   " NLstring,
            "  I I I I I I I      8     8   8           8     8     o  8    8  " NLstring,
            "  I I I I I I I      8         8           8     8        8    8  " NLstring,
            "  I I I I I I I      8         8           8      ooooo   8oooo   " NLstring,
           "  I  \\ `+' /  I      8         8           8           8  8       " NLstring,
           "   \\  `-+-'  /       8     o   8           8     o     8  8       " NLstring,
            "    `-__|__-'         ooooo    8oooooo  ooo8ooo   ooooo   8       " NLstring,
            "        |                                                         " NLstring,
            "  ------+------     Copyright (c) Bruno Haible, Michael Stoll 1992, 1993" NLstring,
            "                    Copyright (c) Bruno Haible, Marcus Daniels 1994-1997" NLstring,
            "                    Copyright (c) Pierpaolo Bernardi, Sam Steingold 1998" NLstring,
            };
          #ifdef AMIGA
          var const char * banner2 =
            DEUTSCH ?
            "                    Amiga-Version: Jörg Höhle                     " NLstring :
            ENGLISH ?
            "                    Amiga version: Jörg Höhle                     " NLstring :
            FRANCAIS ?
            "                    version Amiga: Jörg Höhle                     " NLstring :
            "";
          #endif
          #ifdef RISCOS
          var const char * banner2 =
            DEUTSCH ?
            "                    RISCOS-Portierung: Peter Burwood, Bruno Haible" NLstring :
            ENGLISH ?
            "                    RISCOS port: Peter Burwood, Bruno Haible      " NLstring :
            FRANCAIS ?
            "                    portage RISCOS: Peter Burwood et Bruno Haible " NLstring :
            "";
          #endif
          #ifdef DJUNIX
          var const char * banner2 =
            DEUTSCH ?
            "                    DOS-Portierung: Jürgen Weber, Bruno Haible    " NLstring :
            ENGLISH ?
            "                    DOS port: Jürgen Weber, Bruno Haible          " NLstring :
            FRANCAIS ?
            "                    adapté à DOS par Jürgen Weber et Bruno Haible " NLstring :
            "";
          #endif
          var const char * banner3 =
            "                                                                  " NLstring ;
          var uintL offset = (posfixnum_to_L(Symbol_value(S(prin_linelength))) >= 73 ? 0 : 20);
          var const char * const * ptr = &banner[0];
          var uintC count;
          pushSTACK(var_stream(S(standard_output),strmflags_wr_ch_B)); # auf *STANDARD-OUTPUT*
          dotimesC(count,sizeof(banner)/sizeof(banner[0]),
            { write_sstring(&STACK_0,asciz_to_string(&(*ptr++)[offset])); }
            );
          #if defined(AMIGA) || defined(RISCOS) || defined(DJUNIX)
          write_sstring(&STACK_0,asciz_to_string(&banner2[offset]));
          #endif
          write_sstring(&STACK_0,asciz_to_string(&banner3[offset]));
          skipSTACK(1);
        }
      if ((argv_memfile == NULL) && (argv_expr == NULL))
        # Warnung für Anfänger
        { pushSTACK(var_stream(S(standard_output),strmflags_wr_ch_B)); # auf *STANDARD-OUTPUT*
          write_sstring(&STACK_0,
            asciz_to_string(DEUTSCH ? NLstring "WARNUNG: Kein Initialisierungsfile angegeben." NLstring :
                            ENGLISH ? NLstring "WARNING: No initialisation file specified." NLstring :
                            FRANCAIS ? NLstring "AVERTISSEMENT : Pas de fichier d'initialisation." NLstring :
                            ""
                           ));
          write_sstring(&STACK_0,
            asciz_to_string(DEUTSCH ? "Versuchen Sie: " :
                            ENGLISH ? "Please try: " :
                            FRANCAIS ? "Essayez: " :
                            ""
                           ));
          write_string(&STACK_0,asciz_to_string(program_name));
          #ifdef RISCOS
          write_string(&STACK_0,asciz_to_string(" -M mem.lispinit" NLstring));
          #else
          write_string(&STACK_0,asciz_to_string(" -M lispinit.mem" NLstring));
          #endif
          skipSTACK(1);
        }
      if (argv_compile || !(argv_expr == NULL) || !(argv_execute_file == NULL))
        # '-c' oder '-x' oder file angegeben -> LISP läuft im Batch-Modus:
        { # (setq *debug-io*
          #   (make-two-way-stream (make-string-input-stream "") *query-io*)
          # )
          funcall(L(make_concatenated_stream),0); # (MAKE-CONCATENATED-STREAM)
          pushSTACK(value1); # leerer Input-Stream
         {var object stream = var_stream(S(query_io),strmflags_wr_ch_B);
          Symbol_value(S(debug_io)) = make_twoway_stream(popSTACK(),stream);
        }}
      if (!(argv_package == NULL))
        # (IN-PACKAGE packagename) ausführen:
        { var object packname = asciz_to_string(argv_package);
          pushSTACK(packname); funcall(L(in_package),1);
        }
      if (argv_load_compiling)
        # (SETQ *LOAD-COMPILING* T) ausführen:
        { Symbol_value(S(load_compiling)) = T; }
      # für jedes initfile (LOAD initfile) ausführen:
      { var char** fileptr = &argv_init_files[0];
        var uintL count;
        dotimesL(count,argv_init_filecount,
          { var object filename = asciz_to_string(*fileptr++);
            pushSTACK(filename); funcall(S(load),1);
          });
      }
      if (argv_compile)
        # für jedes File
        #   (EXIT-ON-ERROR
        #     (APPEASE-CERRORS
        #       (COMPILE-FILE (setq file (MERGE-PATHNAMES file (MERGE-PATHNAMES '#".lsp" (CD))))
        #                     [:OUTPUT-FILE (setq output-file (MERGE-PATHNAMES (MERGE-PATHNAMES output-file (MERGE-PATHNAMES '#".fas" (CD))) file))]
        #                     [:LISTING (MERGE-PATHNAMES '#".lis" (or output-file file))]
        #   ) ) )
        # durchführen:
        { var argv_compile_file* fileptr = &argv_compile_files[0];
          var uintL count;
          dotimesL(count,argv_compile_filecount,
            { var uintC argcount = 1;
              var object filename = asciz_to_string(fileptr->input_file);
              pushSTACK(S(compile_file));
              pushSTACK(filename);
              pushSTACK(O(source_file_type)); # #".lsp"
              funcall(L(cd),0); pushSTACK(value1); # (CD)
              funcall(L(merge_pathnames),2); # (MERGE-PATHNAMES '#".lsp" (CD))
              pushSTACK(value1);
              funcall(L(merge_pathnames),2); # (MERGE-PATHNAMES file ...)
              pushSTACK(value1);
              if (fileptr->output_file)
                { filename = asciz_to_string(fileptr->output_file);
                  pushSTACK(S(Koutput_file));
                  pushSTACK(filename);
                  pushSTACK(O(compiled_file_type)); # #".fas"
                  funcall(L(cd),0); pushSTACK(value1); # (CD)
                  funcall(L(merge_pathnames),2); # (MERGE-PATHNAMES '#".fas" (CD))
                  pushSTACK(value1);
                  funcall(L(merge_pathnames),2); # (MERGE-PATHNAMES output-file ...)
                  pushSTACK(value1);
                  pushSTACK(STACK_2); # file
                  funcall(L(merge_pathnames),2); # (MERGE-PATHNAMES ... file)
                  pushSTACK(value1);
                  argcount += 2;
                }
              if (argv_compile_listing)
                { pushSTACK(S(Klisting));
                  pushSTACK(O(listing_file_type)); # #".lis"
                  pushSTACK(STACK_2); # (or output-file file)
                  funcall(L(merge_pathnames),2); # (MERGE-PATHNAMES '#".lis" ...)
                  pushSTACK(value1);
                  argcount += 2;
                }
              # Alle Argumente quotieren:
             {var object* ptr = args_end_pointer;
              var uintC c;
              dotimesC(c,argcount,
                { pushSTACK(S(quote)); pushSTACK(Before(ptr));
                  BEFORE(ptr) = listof(2);
                });
             }
             {var object form = listof(1+argcount); # `(COMPILE-FILE ',...)
              pushSTACK(S(batchmode_errors));
              pushSTACK(form);
              form = listof(2); # `(SYS::BATCHMODE-ERRORS (COMPILE-FILE ',...))
              eval_noenv(form); # ausführen
              fileptr++;
            }});
          quit();
        }
      if (!(argv_execute_file == NULL))
        # (PROGN
        #   #+UNIX (SET-DISPATCH-MACRO-CHARACTER #\# #\!
        #            #'SYS::UNIX-EXECUTABLE-READER)
        #   (SETQ *LOAD-VERBOSE* NIL)
        #   (DEFPARAMETER *ARGS* argv_execute_args)
        #   (EXIT-ON-ERROR (APPEASE-CERRORS (LOAD argv_execute_file)))
        #   (EXIT)
        # )
        # durchführen:
        {
          #ifdef UNIX
          # Make clisp ignore the leading #! line.
          pushSTACK(code_char('#')); pushSTACK(code_char('!'));
          pushSTACK(L(unix_executable_reader));
          funcall(L(set_dispatch_macro_character),3);
          #endif
          Symbol_value(S(load_verbose)) = NIL;
          { var char** argsptr = argv_execute_args;
            var uintL count;
            dotimesL(count,argv_execute_arg_count,
              { pushSTACK(asciz_to_string(*argsptr++)); });
            define_variable(S(args),listof(argv_execute_arg_count));
          }
          { var object form;
            pushSTACK(S(load));
            if (asciz_equal(argv_execute_file,"-"))
              { pushSTACK(S(standard_input)); } # *STANDARD-INPUT*
              else
              { pushSTACK(asciz_to_string(argv_execute_file)); } # "..."
            form = listof(2);
            pushSTACK(S(batchmode_errors)); pushSTACK(form);
            form = listof(2); # `(SYS::BATCHMODE-ERRORS (LOAD "..."))
            eval_noenv(form); # ausführen
          }
          quit();
        }
      if (!(argv_expr == NULL))
        # *STANDARD-INPUT* auf einen Stream setzen, der argv_expr produziert:
        { pushSTACK(asciz_to_string(argv_expr));
          funcall(L(make_string_input_stream),1);
          Symbol_value(S(standard_input)) = value1;
          # Dann den Driver aufrufen. Stringende -> EOF -> Programmende.
        }
      # Read-Eval-Print-Schleife aufrufen:
      driver();
      quit();
      /*NOTREACHED*/
      # Falls der Speicher nicht ausreichte:
      no_mem:
      asciz_out(program_name); asciz_out(": ");
      asciz_out(
        DEUTSCH ? "Nicht genug Speicher für LISP" NLstring :
        ENGLISH ? "Not enough memory for Lisp." NLstring :
        FRANCAIS ? "Il n'y a pas assez de mémoire pour LISP." NLstring :
        ""
        );
      quit_sofort(1);
      /*NOTREACHED*/
     # Beendigung des Programms durch quit_sofort():
      end_of_main:
      #ifdef MULTIMAP_MEMORY
      exitmap();
      #endif
      FREE_DYNAMIC_ARRAY(argv_compile_files); }
      FREE_DYNAMIC_ARRAY(argv_init_files); }
      #ifdef GRAPHICS_SWITCH
      switch_text_mode(); # Rückkehr zum normalen Text-Modus
      #endif
      #if (defined(UNIX) && !defined(NEXTAPP)) || defined(AMIGAOS) || defined(RISCOS)
      terminal_sane(); # Terminal wieder in Normalzustand schalten
      #endif
      #ifdef DJUNIX
      if (cbrk) { setcbrk(cbrk); } # Ctrl-Break wieder zulassen
      _go32_want_ctrl_break(0); # Ctrl-Break wieder normal
      #endif
      #if defined(UNIX) || defined(RISCOS)
        exit(exitcode); # Calling exit(), not _exit(), allows profiling to work.
      #endif
      #if defined(MSDOS) || defined(WIN32_NATIVE)
        _exit(exitcode);
      #endif
      #ifdef AMIGAOS
        exit_amiga(exitcode ? RETURN_ERROR : RETURN_OK);
      #endif
      # Wenn das nichts geholfen haben sollte:
      return exitcode;
    }}

# LISP-Interpreter verlassen
# > final_exitcode: 0 bei normalem Ende, 1 bei Abbruch
  nonreturning_function(global, quit, (void));
  global boolean final_exitcode = 0;
  local int quit_retry = 0;
  global void quit()
    { # Erst den STACK bis STACK-Ende "unwinden":
      value1 = NIL; mv_count=0; # Bei UNWIND-PROTECT-Frames keine Werte retten
      unwind_protect_to_save.fun = (restart)&quit;
      loop
        { # Hört der STACK hier auf?
          if (eq(STACK_0,nullobj) && eq(STACK_1,nullobj)) break;
          if (framecode(STACK_0) & bit(frame_bit_t))
            # Bei STACK_0 beginnt ein Frame
            { unwind(); } # Frame auflösen
            else
            # STACK_0 enthält ein normales LISP-Objekt
            { skipSTACK(1); }
        }
      # Dann eine Abschiedsmeldung:
      if (quit_retry==0)
        { quit_retry++; # If this fails, don't retry it. For robustness.
          funcall(L(fresh_line),0); # (FRESH-LINE [*standard-output*])
          if (!argv_quiet)
            { # (WRITE-LINE "Bye." [*standard-output*]) :
              pushSTACK(OLS(bye_string)); funcall(L(write_line),1);
            }
          pushSTACK(var_stream(S(error_output),strmflags_wr_ch_B)); # Stream *ERROR-OUTPUT*
          funcall(L(fresh_line),1); # (FRESH-LINE *error-output*)
        }
      close_all_files(); # alle Files schließen
      #ifdef DYNAMIC_FFI
      exit_ffi(); # FFI herunterfahren
      #endif
      #ifdef REXX
      close_rexx(); # Rexx-Kommunikation herunterfahren
      #endif
      #ifdef NEXTAPP
      nxterminal_exit(); # Terminal-Stream-Kommunikation herunterfahren
      #endif
      quit_sofort(final_exitcode); # Programm verlassen
    }

# ------------------------------------------------------------------------------
#                  Speichern und Laden von MEM-Files

# Flags, von denen das Format eines MEM-Files abhängt:
  local uint32 memflags =
    # Typcodeverteilung:
    #ifdef WIDE
      bit(0) |
    #endif
    #ifdef TYPECODES
      bit(1) |
    #endif
    #ifdef STANDARD_TYPECODES
      bit(2) |
    #endif
    #ifdef PACKED_TYPECODES
      bit(3) |
    #endif
    #ifdef SEVENBIT_TYPECODES
      bit(4) |
    #endif
    #ifdef SIXBIT_TYPECODES
      bit(5) |
    #endif
    #ifdef case_structure
      bit(6) |
    #endif
    #ifdef case_stream
      bit(7) |
    #endif
    # Codierung von Zahlen:
    #ifdef FAST_FLOAT
      bit(8) |
    #endif
    #ifdef FAST_DOUBLE
      bit(9) |
    #endif
    # Codierung von Streams:
    #ifdef STRM_WR_SS
      bit(10) |
    #endif
    # Codierung von strmtype:
    #ifdef HANDLES
      bit(11) |
    #endif
    #ifdef KEYBOARD
      bit(12) |
    #endif
    #ifdef SCREEN
      bit(13) |
    #endif
    #ifdef PRINTER
      bit(14) |
    #endif
    #ifdef PIPES
      bit(15) |
    #endif
    #ifdef X11SOCKETS
      bit(16) |
    #endif
    #ifdef GENERIC_STREAMS
      bit(17) |
    #endif
    #ifdef SOCKET_STREAMS
      bit(18) |
    #endif
    0;

# Format:
# ein Header:
  typedef struct { uintL _magic; # Erkennung
                     #define memdump_magic  0x70768BD2UL
                   uint32 _memflags;
                   oint _oint_type_mask;
                   oint _oint_addr_mask;
                   #ifdef TYPECODES
                   tint _cons_type, _complex_type, _symbol_type, _system_type;
                   #endif
                   uintC _varobject_alignment;
                   uintC _hashtable_length;
                   uintC _pathname_length;
                   uintC _intDsize;
                   uintC _module_count;
                   uintL _module_names_size;
                   uintC _fsubr_anz;
                   uintC _pseudofun_anz;
                   uintC _symbol_anz;
                   uintL _page_alignment;
                   aint _subr_tab_addr;
                   aint _symbol_tab_addr;
                   #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
                   aint _mem_varobjects_start;
                   aint _mem_varobjects_end;
                   aint _mem_conses_start;
                   aint _mem_conses_end;
                   #endif
                   #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
                   uintC _heapcount;
                   #endif
                 }
          memdump_header;
  # dann die Modulnamen,
  # dann fsubr_tab, pseudofun_tab, symbol_tab,
  # und zu jedem Modul subr_addr, subr_anz, object_anz, subr_tab, object_tab,
#ifdef SPVW_MIXED_BLOCKS_OPPOSITE
  # dann die Objekte variabler Länge (zwischen mem.varobjects.heap_start und mem.varobjects.heap_end),
  # dann die Conses (zwischen mem.conses.heap_start und mem.conses.heap_end).
#else
  #if defined(SPVW_PURE_BLOCKS) || defined(SPVW_MIXED_BLOCKS_STAGGERED)
    # dann zu jedem Heap (Block) die Start- und Endadresse,
  #endif
  #ifdef SPVW_PAGES
    # SPVW_PAGES: dann zu jedem Heap die Anzahl der Pages,
    # dann zu jedem Heap und zu jeder Page des Heaps die Start- und Endadresse,
  #endif
  typedef struct { aint _page_start; aint _page_end; } memdump_page;
  #if defined(SPVW_PURE_BLOCKS) && defined(GENERATIONAL_GC)
    # dann zu jedem Heap die Länge von physpages,
    # dann zu jedem Heap den ganzen physpages-Array,
    typedef struct { object* continued_addr; uintC continued_count; aint firstobject; }
            memdump_physpage_state;
  #endif
  # dann der Inhalt der Pages in derselben Reihenfolge.
  #ifdef SPVW_PURE_BLOCKS
    # Schließlich die Adressen aller von loadmem_update() zu
    # aktualisierenden Objekte innerhalb der Heaps, die Adressen der
    # mit mark_ht_invalid() zu markierenden Hashtabellen, die Adressen
    # der mit mark_fp_invalid() zu markierenden Foreign-Pointer, die
    # Adressen der mit loadmem_update_fsubr() zu relozierenden Fsubrs.
    # Zuvor deren Anzahlen.
    # (Das ist redundant, reduziert aber die Startup-Zeiten.)
    typedef struct { uintL reloccount;
                     uintL htcount;
                     uintL fpcount;
                     uintL fscount;
                   }
            memdump_reloc_header;
  #endif
#endif

# page_alignment = Alignment für die Page-Inhalte im File.
#if ((defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))) && (defined(HAVE_MMAP) || defined(SELFMADE_MMAP))
  #define page_alignment  map_pagesize
  #define WRITE_page_alignment(position)  \
    { var uintL aligncount = (uintL)(-position) % page_alignment; \
      if (aligncount > 0)                                         \
        { # Ein Stück durchgenullten Speicher besorgen:           \
          var DYNAMIC_ARRAY(zeroes,uintB,aligncount);             \
          var uintB* ptr = &zeroes[0];                            \
          var uintL count;                                        \
          dotimespL(count,aligncount, { *ptr++ = 0; } );          \
          # und schreiben:                                        \
          WRITE(&zeroes[0],aligncount);                           \
          FREE_DYNAMIC_ARRAY(zeroes);                             \
    }   }
  #define READ_page_alignment(position)  \
    { var uintL aligncount = (uintL)(-position) % page_alignment; \
      if (aligncount > 0)                                         \
        { var DYNAMIC_ARRAY(dummy,uintB,aligncount);              \
          READ(&dummy[0],aligncount);                             \
          FREE_DYNAMIC_ARRAY(dummy);                              \
    }   }
#else
  #define page_alignment  1
  #define WRITE_page_alignment(position)
  #define READ_page_alignment(position)
#endif

# UP, speichert Speicherabbild auf Diskette
# savemem(stream);
# > object stream: offener File-Output-Stream, wird geschlossen
# kann GC auslösen
  global void savemem (object stream);
  global void savemem(stream)
    var object stream;
    { # Wir brauchen den Stream nur wegen des für ihn bereitgestellten Handles.
      # Wir müssen ihn aber im Fehlerfalle schließen (der Aufrufer macht kein
      # WITH-OPEN-FILE, sondern nur OPEN). Daher bekommen wir den ganzen
      # Stream übergeben, um ihn schließen zu können.
      var Handle handle = TheHandle(TheStream(stream)->strm_file_handle);
      pushSTACK(stream); # Stream retten
      # Erst eine GC ausführen:
      gar_col();
      #define WRITE(buf,len)  \
        { begin_system_call();                                       \
         {var sintL ergebnis = full_write(handle,(RW_BUF_T)buf,len); \
          if (!(ergebnis==(sintL)(len)))                             \
            { end_system_call();                                     \
              stream_close(&STACK_0);                                \
              if (ergebnis<0) { OS_file_error(TheStream(STACK_0)->strm_file_truename); } # Fehler aufgetreten?  \
              pushSTACK(TheStream(STACK_0)->strm_file_truename); # Wert für Slot PATHNAME von FILE-ERROR \
              fehler(file_error,                                     \
                     DEUTSCH ? "Diskette/Platte voll." :             \
                     ENGLISH ? "disk full" :                         \
                     FRANCAIS ? "Disque plein." :                    \
                     ""                                              \
                    );                                               \
            }                                                        \
          end_system_call();                                         \
        }}
      # Grundinformation rausschreiben:
     {var memdump_header header;
      var uintL module_names_size;
      header._magic = memdump_magic;
      header._memflags = memflags;
      header._oint_type_mask = oint_type_mask;
      header._oint_addr_mask = oint_addr_mask;
      #ifdef TYPECODES
      header._cons_type    = cons_type;
      header._complex_type = complex_type;
      header._symbol_type  = symbol_type;
      header._system_type  = system_type;
      #endif
      header._varobject_alignment = varobject_alignment;
      header._hashtable_length = hashtable_length;
      header._pathname_length = pathname_length;
      header._intDsize = intDsize;
      header._module_count = module_count;
      { var module_* module;
        module_names_size = 0;
        for_modules(all_modules,
          { module_names_size += asciz_length(module->name)+1; }
          );
        module_names_size = round_up(module_names_size,varobject_alignment);
      }
      header._module_names_size = module_names_size;
      header._fsubr_anz     = fsubr_anz;
      header._pseudofun_anz = pseudofun_anz;
      header._symbol_anz    = symbol_anz;
      header._page_alignment = page_alignment;
      header._subr_tab_addr   = (aint)(&subr_tab);
      header._symbol_tab_addr = (aint)(&symbol_tab);
      #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
      #if !defined(GENERATIONAL_GC)
      header._mem_varobjects_start = mem.varobjects.heap_start;
      header._mem_varobjects_end   = mem.varobjects.heap_end;
      header._mem_conses_start     = mem.conses.heap_start;
      header._mem_conses_end       = mem.conses.heap_end;
      #else # defined(GENERATIONAL_GC)
      header._mem_varobjects_start = mem.varobjects.heap_gen0_start;
      header._mem_varobjects_end   = mem.varobjects.heap_gen0_end;
      header._mem_conses_start     = mem.conses.heap_gen0_start;
      header._mem_conses_end       = mem.conses.heap_gen0_end;
      #endif
      #endif
      #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
      header._heapcount = heapcount;
      #endif
      WRITE(&header,sizeof(header));
      # Modulnamen rausschreiben:
      { var DYNAMIC_ARRAY(module_names_buffer,char,module_names_size);
       {var char* ptr2 = &module_names_buffer[0];
        var module_* module;
        var uintC count;
        for_modules(all_modules,
          { var char* ptr1 = module->name;
            until ((*ptr2++ = *ptr1++) == '\0') ;
          });
        dotimesC(count,&module_names_buffer[module_names_size] - ptr2,
          { *ptr2++ = 0; }
          );
        WRITE(module_names_buffer,module_names_size);
        FREE_DYNAMIC_ARRAY(module_names_buffer);
      }}
      # fsubr_tab, pseudofun_tab, symbol_tab rausschreiben:
      WRITE(&fsubr_tab,sizeof(fsubr_tab));
      WRITE(&pseudofun_tab,sizeof(pseudofun_tab));
      WRITE(&symbol_tab,sizeof(symbol_tab));
      # Zu jedem Modul subr_addr, subr_anz, object_anz, subr_tab, object_tab rausschreiben:
      { var module_* module;
        for_modules(all_modules,
          { WRITE(&module->stab,sizeof(subr_*));
            WRITE(module->stab_size,sizeof(uintC));
            WRITE(module->otab_size,sizeof(uintC));
            WRITE(module->stab,*module->stab_size*sizeof(subr_));
            WRITE(module->otab,*module->otab_size*sizeof(object));
          });
      }
      #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
      # Objekte variabler Länge rausschreiben:
      {var uintL len = header._mem_varobjects_end - header._mem_varobjects_start;
       WRITE(header._mem_varobjects_start,len);
      }
      # Conses rausschreiben:
      {var uintL len = header._mem_conses_end - header._mem_conses_start;
       WRITE(header._mem_conses_start,len);
      }
      #endif
      #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
      #ifdef SPVW_PAGES
      {var uintL heapnr;
       for (heapnr=0; heapnr<heapcount; heapnr++)
         { var uintC pagecount = 0;
           map_heap(mem.heaps[heapnr],page, { pagecount++; } );
           WRITE(&pagecount,sizeof(pagecount));
      }  }
      #endif
      {var uintL heapnr;
       for (heapnr=0; heapnr<heapcount; heapnr++)
         {
           #if !defined(GENERATIONAL_GC)
           map_heap(mem.heaps[heapnr],page,
             { var memdump_page _page;
               _page._page_start = page->page_start;
               _page._page_end = page->page_end;
               WRITE(&_page,sizeof(_page));
             });
           #else # defined(GENERATIONAL_GC)
           var Heap* heap = &mem.heaps[heapnr];
           var memdump_page _page;
           _page._page_start = heap->heap_gen0_start;
           _page._page_end = heap->heap_gen0_end;
           WRITE(&_page,sizeof(_page));
           #endif
      }  }
      #if defined(SPVW_PURE_BLOCKS) && defined(GENERATIONAL_GC)
      { var uintL numphyspages[heapcount];
        var uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++)
          { var Heap* heap = &mem.heaps[heapnr];
            numphyspages[heapnr] =
              (heap->physpages==NULL ? 0 :
               (((heap->heap_gen0_end + (physpagesize-1)) & -physpagesize)
                - (heap->heap_gen0_start & -physpagesize)
               ) >> physpageshift
              );
          }
        WRITE(&numphyspages,sizeof(numphyspages));
        for (heapnr=0; heapnr<heapcount; heapnr++)
          if (numphyspages[heapnr] > 0)
            { var uintL count = numphyspages[heapnr];
              var Heap* heap = &mem.heaps[heapnr];
              var physpage_state* physpages = heap->physpages;
              var DYNAMIC_ARRAY(_physpages,memdump_physpage_state,count);
              var uintL i;
              for (i=0; i<count; i++)
                { _physpages[i].continued_addr  = physpages[i].continued_addr;
                  _physpages[i].continued_count = physpages[i].continued_count;
                  _physpages[i].firstobject     = physpages[i].firstobject;
                }
              WRITE(_physpages,count*sizeof(memdump_physpage_state));
              FREE_DYNAMIC_ARRAY(_physpages);
            }
      }
      #endif
      #if (defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))
       #if defined(HAVE_MMAP) || defined(SELFMADE_MMAP) # sonst ist page_alignment sowieso = 1
        # Alignment verwirklichen:
        { begin_system_call();
         {var sintL ergebnis = lseek(handle,0,SEEK_CUR); # File-Position holen
          end_system_call();
          if (ergebnis<0) { stream_close(&STACK_0); OS_file_error(TheStream(STACK_0)->strm_file_truename); } # Fehler?
          WRITE_page_alignment(ergebnis);
        }}
       #endif
      #endif
      {var uintL heapnr;
       for (heapnr=0; heapnr<heapcount; heapnr++)
         {
           #if !defined(GENERATIONAL_GC)
           map_heap(mem.heaps[heapnr],page,
             { var uintL len = page->page_end - page->page_start;
               WRITE(page->page_start,len);
               WRITE_page_alignment(len);
             });
           #else # defined(GENERATIONAL_GC)
           var Heap* heap = &mem.heaps[heapnr];
           var uintL len = heap->heap_gen0_end - heap->heap_gen0_start;
           WRITE(heap->heap_gen0_start,len);
           WRITE_page_alignment(len);
           #endif
      }  }
      #ifdef SPVW_PURE_BLOCKS
      # Relozierungen rausschreiben:
      # (Nur Frame-Pointer, Subr, Machine müssen reloziert werden, und
      # Hashtabellen und Fpointer müssen markiert werden, siehe
      # update_varobjects(), update_record(), loadmem_update().)
      { var memdump_reloc_header rheader;
        rheader.reloccount = 0;
        rheader.htcount = 0;
        rheader.fpcount = 0;
        rheader.fscount = 0;
        #if !defined(GENERATIONAL_GC)
        #define update_conspage  update_conspage_normal
        #define update_page  update_page_normal
        #else # defined(GENERATIONAL_GC)
        #define update_conspage(page)  # ignoriert page, benutzt heapnr \
          { var aint objptr = mem.heaps[heapnr].heap_gen0_start;  \
            var aint objptrend = mem.heaps[heapnr].heap_gen0_end; \
            # alle Pointer im (neuen) CONS-Bereich start <= Adresse < end aktualisieren: \
            until (objptr==objptrend)                             \
              { update((object*)objptr);                          \
                objptr += sizeof(object);                         \
                update((object*)objptr);                          \
                objptr += sizeof(object);                         \
          }   }
        #define update_page(page,updater)  # ignoriert page, benutzt heapnr \
          { var aint ptr = mem.heaps[heapnr].heap_gen0_start;            \
            var aint ptrend = mem.heaps[heapnr].heap_gen0_end;           \
            # alle Objekte mit Adresse >=ptr, <ptrend durchgehen:        \
            until (ptr==ptrend) # solange bis ptr am Ende angekommen ist \
              { # nächstes Objekt mit Adresse ptr (< ptrend) durchgehen: \
                updater(typecode_at(ptr)); # und weiterrücken            \
          }   }
        #endif
        #ifdef FOREIGN
        #define update_fpointer_invalid  TRUE
        #else
        #define update_fpointer_invalid  FALSE
        #endif
        #define update_fsubr_function  TRUE
        #define update(objptr)  \
          { switch (mtypecode(*(object*)objptr))                          \
              { case_system:                                              \
                  if (wbit_test(*(oint*)objptr,0+oint_addr_shift)) break; \
                case_subr:                                                \
                case_machine:                                             \
                  rheader.reloccount++;                                   \
                default:                                                  \
                  break;                                                  \
          }   }
        #define update_ht_invalid(obj)  rheader.htcount++;
        #define update_fp_invalid(obj)  rheader.fpcount++;
        #define update_fs_function(obj)  rheader.fscount++;
        update_conses();
        update_varobjects();
        #undef update_fs_function
        #undef update_fp_invalid
        #undef update_ht_invalid
        #undef update
       {var DYNAMIC_ARRAY(relocbuf,object*,rheader.reloccount);
       {var DYNAMIC_ARRAY(htbuf,Hashtable,rheader.htcount);
       {var DYNAMIC_ARRAY(fpbuf,Record,rheader.fpcount);
       {var DYNAMIC_ARRAY(fsbuf,Fsubr,rheader.fscount);
       {var object** relocbufptr = &relocbuf[0];
        var Hashtable* htbufptr = &htbuf[0];
        var Record* fpbufptr = &fpbuf[0];
        var Fsubr* fsbufptr = &fsbuf[0];
        #define update(objptr)  \
          { switch (mtypecode(*(object*)objptr))                          \
              { case_system:                                              \
                  if (wbit_test(*(oint*)objptr,0+oint_addr_shift)) break; \
                case_subr:                                                \
                case_machine:                                             \
                  *relocbufptr++ = (object*)objptr;                       \
                default:                                                  \
                  break;                                                  \
          }   }
        #define update_ht_invalid(obj)  *htbufptr++ = (obj);
        #define update_fp_invalid(obj)  *fpbufptr++ = (obj);
        #define update_fs_function(obj)  *fsbufptr++ = (obj);
        update_conses();
        update_varobjects();
        #undef update_fs_function
        #undef update_fp_invalid
        #undef update_ht_invalid
        #undef update
        #undef update_fsubr_function
        #undef update_fpointer_invalid
        #undef update_page
        #undef update_conspage
        WRITE(&rheader,sizeof(rheader));
        WRITE(&relocbuf[0],rheader.reloccount*sizeof(object*));
        WRITE(&htbuf[0],rheader.htcount*sizeof(Hashtable));
        WRITE(&fpbuf[0],rheader.fpcount*sizeof(Record));
        WRITE(&fsbuf[0],rheader.fscount*sizeof(Fsubr));
       }FREE_DYNAMIC_ARRAY(fsbuf);
       }FREE_DYNAMIC_ARRAY(fpbuf);
       }FREE_DYNAMIC_ARRAY(htbuf);
       }FREE_DYNAMIC_ARRAY(relocbuf);
      }}
      #endif
      #endif
      #undef WRITE
      # Stream schließen (Stream-Buffer ist unverändert, aber dadurch wird
      # auch das Handle beim Betriebssystem geschlossen):
      stream_close(&STACK_0);
      skipSTACK(1);
    }}

# UP, lädt Speicherabbild von Diskette
# loadmem(filename);
# Zerstört alle LISP-Daten.
  #ifdef UNIX
  local void loadmem_from_handle (int handle);
  #endif
  # Aktualisierung eines Objektes im Speicher:
  #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
  local var oint offset_varobjects_o;
  local var oint offset_conses_o;
  #endif
  #ifdef SPVW_MIXED_BLOCKS_STAGGERED
  local var oint offset_heaps_o[heapcount];
  #define offset_varobjects_o  offset_heaps_o[0]
  #define offset_conses_o      offset_heaps_o[1]
  #endif
  #ifdef SINGLEMAP_MEMORY_RELOCATE
  local var oint offset_heaps_o[heapcount];
  local var boolean offset_heaps_all_zero;
  #endif
  #ifdef SPVW_PAGES
  local var struct { aint old_page_start; oint offset_page_o; } *offset_pages;
  #define addr_mask  ~(((oint_addr_mask>>oint_addr_shift) & ~ (wbitm(oint_addr_relevant_len)-1)) << addr_shift) # meist = ~0
  #define pagenr_of(addr)  floor(addr,min_page_size_brutto)
  #define offset_pages_len  (pagenr_of((oint)((wbitm(oint_addr_relevant_len)-1)<<addr_shift))+1)
  #endif
  #if !defined(SINGLEMAP_MEMORY)
  local var oint offset_symbols_o;
  #if !defined(MULTIMAP_MEMORY_SYMBOL_TAB)
  local var oint old_symbol_tab_o;
  #endif
  #endif
  typedef struct { oint low_o; oint high_o; oint offset_o; } offset_subrs_t;
  local var offset_subrs_t* offset_subrs;
  local var uintC offset_subrs_anz;
  local var struct fsubr_tab_ old_fsubr_tab;
  local var struct pseudofun_tab_ old_pseudofun_tab;
  local void loadmem_update (object* objptr);
  local void loadmem_update(objptr)
    var object* objptr;
    {
      #ifdef TYPECODES
      switch (mtypecode(*objptr))
      #else
      if (orecordp(*objptr)) { goto case_record; }
      elif (consp(*objptr)) { goto case_cons; }
      elif (subrp(*objptr)) { goto case_subr; }
      elif (machinep(*objptr)) { goto case_machine; }
      else { return; }
      switch (0)
      #endif
        {
          #ifdef TYPECODES
          case_symbol: # Symbol
            #ifndef SPVW_PURE_BLOCKS
            #if !defined(MULTIMAP_MEMORY_SYMBOL_TAB)
            if (as_oint(*objptr) - old_symbol_tab_o
                < ((oint)sizeof(symbol_tab)<<(oint_addr_shift-addr_shift))
               )
              # Symbol aus symbol_tab
              { *(oint*)objptr += offset_symbols_o; break; }
            #else
            if (as_oint(*objptr) - (oint)(&symbol_tab)
                < (sizeof(symbol_tab)<<(oint_addr_shift-addr_shift))
               )
              # Symbol aus symbol_tab erfährt keine Verschiebung
              { break; }
            #endif
            # sonstige Symbole sind Objekte variabler Länge.
            #endif
          #endif
          case_record:
            #ifndef TYPECODES
            if (as_oint(*objptr) - old_symbol_tab_o
                < ((oint)sizeof(symbol_tab)<<(oint_addr_shift-addr_shift))
               )
              # Symbol aus symbol_tab
              { *(oint*)objptr += offset_symbols_o; break; }
            #endif
          #ifdef TYPECODES
          case_array:
          case_bignum:
          #ifndef WIDE
          case_ffloat:
          #endif
          case_dfloat:
          case_lfloat:
          #endif
            # Objekt variabler Länge
            #ifdef SPVW_MIXED_BLOCKS
            *(oint*)objptr += offset_varobjects_o; break;
            #endif
          case_pair:
            # Zwei-Pointer-Objekt
            #ifdef SPVW_MIXED_BLOCKS
            *(oint*)objptr += offset_conses_o; break;
            #endif
            #ifdef SPVW_PAGES
            {var aint addr = # Adresse
               #ifdef TYPECODES
                 upointer(*(object*)objptr);
               #else
                 as_oint(*(object*)objptr);
               #endif
             # Da Pages eine minimale Länge haben, also die Anfangsadressen
             # unterschiedlicher Pages sich um mindestens min_page_size_brutto
             # unterscheiden, ist es ganz einfach, aus der Adresse auf die
             # Page zurückzuschließen:
             var uintL pagenr = pagenr_of(addr & addr_mask);
             if (addr < offset_pages[pagenr].old_page_start) { pagenr--; }
             *(oint*)objptr += offset_pages[pagenr].offset_page_o;
            }
            break;
            #endif
            #ifdef SPVW_PURE_BLOCKS # SINGLEMAP_MEMORY
            #ifdef SINGLEMAP_MEMORY_RELOCATE
            *(oint*)objptr += offset_heaps_o[mtypecode(*objptr)]; break;
            #else
            break; # Alles Bisherige erfährt keine Verschiebung
            #endif
            #endif
          case_subr: # SUBR
            {var oint addr = *(oint*)objptr;
             var offset_subrs_t* ptr = offset_subrs;
             var uintC count;
             dotimespC(count,offset_subrs_anz,
               { if ((ptr->low_o <= addr) && (addr < ptr->high_o))
                   { *(oint*)objptr += ptr->offset_o; goto found_subr; }
                 ptr++;
               });
            }
            # SUBR nicht gefunden -> #<UNBOUND>
            *objptr = unbound;
            found_subr:
            break;
          #ifdef TYPECODES
          case_system: # Frame-Pointer oder Read-Label oder System-Konstante
            if ((*(oint*)objptr & wbit(0+oint_addr_shift)) ==0)
              # Frame-Pointer -> #<DISABLED>
              { *objptr = disabled; }
            break;
          #endif
          case_machine: # Pseudo-Funktion oder sonstiger Maschinenpointer
            # Umsetzung old_pseudofun_tab -> pseudofun_tab :
            {
              #if (machine_type==0)
              var void* addr = (void*)ThePseudofun(*objptr);
              #else # muß zum Vergleichen die Typinfo wegnehmen
              var void* addr = (void*)upointer(*objptr);
              #endif
              { var uintC i = pseudofun_anz;
                var Pseudofun* ptr = &((Pseudofun*)(&old_pseudofun_tab))[pseudofun_anz];
                until (i==0)
                  { i--;
                    if ((void*) *--ptr == addr)
                      { # Pseudo-Funktion
                        *objptr = make_machine_code(((Pseudofun*)(&pseudofun_tab))[i]);
                        break;
              }   }   }
              # sonstiger Maschinenpointer
              break;
            }
          #ifdef TYPECODES
          case_char:
          case_fixnum:
          case_sfloat:
          #ifdef WIDE
          case_ffloat:
          #endif
          #endif
            break;
          default: /*NOTREACHED*/ abort();
    }   }
  local void loadmem_update_fsubr (Fsubr fsubrptr);
  local void loadmem_update_fsubr(fsubrptr)
    var Fsubr fsubrptr;
    { var void* addr = fsubrptr->function;
      var uintC i = fsubr_anz;
      var fsubr_* p = &((fsubr_*)(&old_fsubr_tab))[fsubr_anz];
      until (i==0)
        { i--;
          if ((void*) *--p == addr)
            { fsubrptr->function = ((const fsubr_ *)(&fsubr_tab))[i];
              break;
    }   }   }
  local void loadmem(filename)
    char* filename;
    { # File zum Lesen öffnen:
      begin_system_call();
     {
      #ifdef AMIGAOS
      var Handle handle = Open(filename,MODE_OLDFILE);
      if (handle==Handle_NULL) goto abbruch1;
      #endif
      #if defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM)
      var int handle = open(filename,O_RDONLY);
      if (handle<0) goto abbruch1;
      setmode(handle,O_BINARY);
      #endif
      #if defined(UNIX) || defined(RISCOS)
      var int handle = OPEN(filename,O_RDONLY|O_BINARY,my_open_mask);
      if (handle<0) goto abbruch1;
      #endif
      #if defined(WIN32_NATIVE)
      var Handle handle = CreateFile(filename, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
      if (handle==INVALID_HANDLE_VALUE) goto abbruch1;
      #endif
      end_system_call();
  #ifdef UNIX
      loadmem_from_handle(handle);
      return;
      abbruch1:
        {var int abbruch_errno = errno;
         asciz_out(program_name); asciz_out(": ");
         asciz_out_s(
           DEUTSCH ? "Betriebssystem-Fehler beim Versuch, das Initialisierungsfile `%s' zu laden." NLstring :
           ENGLISH ? "operating system error during load of initialisation file `%s'" NLstring :
           FRANCAIS ? "Erreur système pendant le chargement du fichier d'initialisation `%s'." NLstring :
           "",
           filename
           );
         errno_out(abbruch_errno);
        }
        goto abbruch_quit;
      abbruch_quit:
        # Abbruch.
        quit_sofort(1);
    }}
  local void loadmem_from_handle(handle)
    var int handle;
    {{
  #endif
      {
       #if (defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))
         #if defined(HAVE_MMAP) || defined(SELFMADE_MMAP)
         local var boolean use_mmap = TRUE;
         #endif
         var uintL file_offset;
         #define set_file_offset(x)  file_offset = (x)
         #define inc_file_offset(x)  file_offset += (uintL)(x)
       #else
         #define set_file_offset(x)
         #define inc_file_offset(x)
       #endif
       #define READ(buf,len)  \
         { begin_system_call();                                      \
          {var sintL ergebnis = full_read(handle,(RW_BUF_T)buf,len); \
           end_system_call();                                        \
           if (ergebnis<0) goto abbruch1;                            \
           if (!(ergebnis==(sintL)(len))) goto abbruch2;             \
           inc_file_offset(len);                                     \
         }}
       begin_read:
       set_file_offset(0);
       # Grundinformation lesen:
       {var memdump_header header;
        READ(&header,sizeof(header));
        if (!(header._magic == memdump_magic))
          {
            #ifdef UNIX
            # Versuche, das File on the fly mit GZIP zu dekomprimieren.
            var uintB* file_header = (uintB*)&header; # benutze sizeof(header) >= 2
            if (file_header[0] == '#' && file_header[1] == '!') # executable magic ?
              { # erste Textzeile überlesen
                var char c;
                begin_system_call();
                if ( lseek(handle,-(long)sizeof(header),SEEK_CUR) <0) goto abbruch1; # im File zurück an den Anfang
                do { READ(&c,1); } until (c=='\n');
                end_system_call();
                #if ((defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))) && (defined(HAVE_MMAP) || defined(SELFMADE_MMAP))
                use_mmap = FALSE; # Die File-Offsets haben sich verschoben!
                #endif
                goto begin_read;
              }
            if (file_header[0] == 0x1F && file_header[1] == 0x8B) # gzip magic ?
              { # Pipe aufmachen, siehe make_pipe_input_stream in STREAM.D
                var int handles[2];
                var int child;
                begin_system_call();
                if ( lseek(handle,-(long)sizeof(header),SEEK_CUR) <0) goto abbruch1; # im File zurück an den Anfang
                if (!( pipe(handles) ==0)) goto abbruch1;
                if ((child = vfork()) ==0)
                  { if ( dup2(handles[1],stdout_handle) >=0)
                      if ( CLOSE(handles[1]) ==0)
                        if ( CLOSE(handles[0]) ==0)
                          if ( dup2(handle,stdin_handle) >=0) # Das File sei der Input der Dekompression
                            # Dekompressor aufrufen. NB: "gzip -d" == "gunzip"
                            #if 0
                               execl("/bin/sh","/bin/sh","-c","gzip -d -c",NULL);
                            #else # so geht's auch ohne die Shell
                              execlp("gzip","gzip","-d","-c",NULL);
                            #endif
                    _exit(-1);
                  }
                if (child==-1)
                  { CLOSE(handles[1]); CLOSE(handles[0]); goto abbruch1; }
                if (!( CLOSE(handles[1]) ==0)) goto abbruch1;
                if (!( CLOSE(handle) ==0)) goto abbruch1;
                end_system_call();
                #if ((defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))) && (defined(HAVE_MMAP) || defined(SELFMADE_MMAP))
                use_mmap = FALSE; # Von einer Pipe kann man kein mmap() machen!
                #endif
                loadmem_from_handle(handles[0]); # Wir lesen ab jetzt von der Pipe
                begin_system_call();
                wait2(child); # Zombie-Child entfernen
                end_system_call();
                return;
              }
            #endif
            goto abbruch2;
          }
        if (!(header._memflags == memflags)) goto abbruch2;
        if (!(header._oint_type_mask == oint_type_mask)) goto abbruch2;
        if (!(header._oint_addr_mask == oint_addr_mask)) goto abbruch2;
        #ifdef TYPECODES
        if (!(header._cons_type == cons_type)) goto abbruch2;
        if (!(header._complex_type == complex_type)) goto abbruch2;
        if (!(header._symbol_type == symbol_type)) goto abbruch2;
        if (!(header._system_type == system_type)) goto abbruch2;
        #endif
        if (!(header._varobject_alignment == varobject_alignment)) goto abbruch2;
        if (!(header._hashtable_length == hashtable_length)) goto abbruch2;
        if (!(header._pathname_length == pathname_length)) goto abbruch2;
        if (!(header._intDsize == intDsize)) goto abbruch2;
        if (!(header._fsubr_anz == fsubr_anz)) goto abbruch2;
        if (!(header._pseudofun_anz == pseudofun_anz)) goto abbruch2;
        if (!(header._symbol_anz == symbol_anz)) goto abbruch2;
        if (!(header._page_alignment == page_alignment)) goto abbruch2;
        #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
        if (!(header._heapcount == heapcount)) goto abbruch2;
        #endif
        #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
        # Offsets berechnen (Offset = neue Adresse - alte Adresse):
        {var sintL offset_varobjects = # Offset für Objekte variabler Länge
           mem.varobjects.heap_start - header._mem_varobjects_start;
         var sintL offset_conses = # Offset für Zwei-Pointer-Objekte
           mem.conses.heap_end - header._mem_conses_end;
         # neue Speicheraufteilung berechnen:
         mem.varobjects.heap_end = header._mem_varobjects_end + offset_varobjects;
         mem.conses.heap_start = header._mem_conses_start + offset_conses;
         # Feststellen, ob der Speicherplatz reicht:
         # Er reicht genau dann, wenn
         # geforderter Platz <= vorhandener Platz  <==>
         # header._mem_conses_end-header._mem_conses_start + header._mem_varobjects_end-header._mem_varobjects_start
         #   <= mem.conses.heap_end - mem.varobjects.heap_start  <==>
         # header._mem_varobjects_end + mem.varobjects.heap_start-header._mem_varobjects_start
         #   <= header._mem_conses_start + mem.conses.heap_end-header._mem_conses_end  <==>
         # mem.varobjects.heap_end <= mem.conses.heap_start
         if (!( (saint)(mem.varobjects.heap_end) <= (saint)(mem.conses.heap_start) )) goto abbruch3;
         # Aktualisierung vorbereiten:
         offset_varobjects_o = (oint)offset_varobjects << (oint_addr_shift-addr_shift);
         offset_conses_o = (oint)offset_conses << (oint_addr_shift-addr_shift);
        }
        #endif
        #ifdef SPVW_PURE_BLOCKS # SINGLEMAP_MEMORY
        if (!((aint)(&subr_tab) == header._subr_tab_addr)) goto abbruch2;
        if (!((aint)(&symbol_tab) == header._symbol_tab_addr)) goto abbruch2;
        #else
        offset_symbols_o = ((oint)(aint)(&symbol_tab) - (oint)header._symbol_tab_addr) << (oint_addr_shift-addr_shift);
        #ifdef MULTIMAP_MEMORY_SYMBOL_TAB
        if (!(offset_symbols_o == 0)) goto abbruch2;
        #else
        #ifdef TYPECODES
        old_symbol_tab_o = as_oint(type_pointer_object(symbol_type,header._symbol_tab_addr));
        #else
        old_symbol_tab_o = (oint)header._symbol_tab_addr;
        #endif
        #endif
        #endif
        # Offset-der-SUBRs-Tabelle initialisieren:
        offset_subrs_anz = 1+header._module_count;
        begin_system_call();
        offset_subrs = (offset_subrs_t*) malloc(offset_subrs_anz*sizeof(offset_subrs_t));
        end_system_call();
        if (offset_subrs==NULL) goto abbruch3;
        # Modulnamen lesen und mit den existierenden Modulen vergleichen:
        {var DYNAMIC_ARRAY(old_modules,module_*,1+header._module_count);
         {var DYNAMIC_ARRAY(module_names_buffer,char,header._module_names_size);
          READ(module_names_buffer,header._module_names_size);
          { var module_* * old_module = &old_modules[0];
            var char* old_name = &module_names_buffer[0];
            var uintC count;
            dotimespC(count,1+header._module_count,
              { var module_* module;
                for_modules(all_modules,
                  { if (asciz_equal(old_name,module->name))
                      goto found_module;
                  });
                # old_name nicht gefunden
                goto abbruch2;
                found_module:
                # Das Lesen der Moduldaten vom File initialisiert das Modul.
                module->initialized = TRUE;
                *old_module++ = module;
                old_name += asciz_length(old_name)+1;
              });
          }
          FREE_DYNAMIC_ARRAY(module_names_buffer);
         }
         # fsubr_tab, pseudofun_tab, symbol_tab lesen:
         READ(&old_fsubr_tab,sizeof(fsubr_tab));
         READ(&old_pseudofun_tab,sizeof(pseudofun_tab));
         READ(&symbol_tab,sizeof(symbol_tab));
         # Zu jedem Modul subr_addr, subr_anz, object_anz, subr_tab, object_tab lesen:
         {var module_* * old_module = &old_modules[0];
          var offset_subrs_t* offset_subrs_ptr = &offset_subrs[0];
          var uintC count;
          dotimespC(count,1+header._module_count,
            { var subr_* old_subr_addr;
              var uintC old_subr_anz;
              var uintC old_object_anz;
              READ(&old_subr_addr,sizeof(subr_*));
              READ(&old_subr_anz,sizeof(uintC));
              READ(&old_object_anz,sizeof(uintC));
              if (!(old_subr_anz == *(*old_module)->stab_size)) goto abbruch2;
              if (!(old_object_anz == *(*old_module)->otab_size)) goto abbruch2;
              offset_subrs_ptr->low_o = as_oint(subr_tab_ptr_as_object(old_subr_addr));
              offset_subrs_ptr->high_o = as_oint(subr_tab_ptr_as_object(old_subr_addr+old_subr_anz));
              offset_subrs_ptr->offset_o = as_oint(subr_tab_ptr_as_object((*old_module)->stab)) - offset_subrs_ptr->low_o;
              if (old_subr_anz > 0)
                { var DYNAMIC_ARRAY(old_subr_tab,subr_,old_subr_anz);
                  READ(old_subr_tab,old_subr_anz*sizeof(subr_));
                 {var subr_* ptr1 = old_subr_tab;
                  var subr_* ptr2 = (*old_module)->stab;
                  var uintC count;
                  dotimespC(count,old_subr_anz,
                    { if (!(   (ptr1->req_anz == ptr2->req_anz)
                            && (ptr1->opt_anz == ptr2->opt_anz)
                            && (ptr1->rest_flag == ptr2->rest_flag)
                            && (ptr1->key_flag == ptr2->key_flag)
                            && (ptr1->key_anz == ptr2->key_anz)
                         ) )
                        goto abbruch2;
                      ptr2->name = ptr1->name; ptr2->keywords = ptr1->keywords;
                      ptr2->argtype = ptr1->argtype;
                      ptr1++; ptr2++;
                    });
                  FREE_DYNAMIC_ARRAY(old_subr_tab);
                }}
              if (old_object_anz > 0)
                { READ((*old_module)->otab,old_object_anz*sizeof(object)); }
              old_module++; offset_subrs_ptr++;
            });
         }
         #ifdef SPVW_PURE_BLOCKS
         #ifdef SINGLEMAP_MEMORY_RELOCATE
         # Start- und Endadressen jedes Heaps lesen und mit mem.heaps[] vergleichen:
         {var memdump_page old_pages[heapcount];
          var memdump_page* old_page;
          var uintL heapnr;
          READ(&old_pages,sizeof(old_pages));
          offset_heaps_all_zero = TRUE;
          old_page = &old_pages[0];
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { var Heap* heapptr = &mem.heaps[heapnr];
              if (!(old_page->_page_end - old_page->_page_start <= heapptr->heap_hardlimit - heapptr->heap_limit)) goto abbruch3;
              heapptr->heap_start = heapptr->heap_limit;
              heapptr->heap_end = heapptr->heap_limit + (old_page->_page_end - old_page->_page_start);
              offset_heaps_o[heapnr] = (oint)heapptr->heap_start - (oint)old_page->_page_start;
              if (!(offset_heaps_o[heapnr] == 0)) { offset_heaps_all_zero = FALSE; }
              old_page++;
            }
          #if defined(HAVE_MMAP) || defined(SELFMADE_MMAP)
          if (!offset_heaps_all_zero) { use_mmap = FALSE; }
          #endif
         }
         #else
         # Start- und Endadressen jedes Heaps gleich in mem.heaps[] übernehmen:
         {var uintL heapnr;
          var memdump_page old_pages[heapcount];
          var memdump_page* old_page;
          READ(&old_pages,sizeof(old_pages));
          old_page = &old_pages[0];
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { map_heap(mem.heaps[heapnr],page,
                { page->page_start = old_page->_page_start;
                  page->page_end = old_page->_page_end;
                  old_page++;
                });
         }  }
         #endif
         #endif
         #ifdef SPVW_MIXED_BLOCKS_STAGGERED
         # Start- und Endadressen jedes Heaps lesen und die Größe in mem.heaps[]
         # auf dieselbe Länge bringen:
         {var uintL heapnr;
          var memdump_page old_pages[heapcount];
          var memdump_page* old_page;
          READ(&old_pages,sizeof(old_pages));
          old_page = &old_pages[0];
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { map_heap(mem.heaps[heapnr],page,
                { page->page_end = page->page_start + (old_page->_page_end - old_page->_page_start);
                  offset_heaps_o[heapnr] = (oint)(sintL)(page->page_start - old_page->_page_start) << (oint_addr_shift-addr_shift);
                  old_page++;
                });
         }  }
         #endif
         #if defined(SPVW_PURE_BLOCKS) && defined(GENERATIONAL_GC)
         { var uintL numphyspages[heapcount];
           var uintL heapnr;
           READ(&numphyspages,sizeof(numphyspages));
           for (heapnr=0; heapnr<heapcount; heapnr++)
             { var uintL count = numphyspages[heapnr];
               var Heap* heap = &mem.heaps[heapnr];
               if (count > 0)
                 { var DYNAMIC_ARRAY(_physpages,memdump_physpage_state,count);
                   var physpage_state* physpages;
                   READ(_physpages,count*sizeof(memdump_physpage_state));
                   physpages = (physpage_state*) malloc(count*sizeof(physpage_state));
                   if (!(physpages==NULL))
                     { var uintL i;
                       for (i=0; i<count; i++)
                         { physpages[i].continued_addr  = _physpages[i].continued_addr;
                           physpages[i].continued_count = _physpages[i].continued_count;
                           physpages[i].firstobject     = _physpages[i].firstobject;
                           physpages[i].protection = PROT_READ;
                           physpages[i].cache_size = 0; physpages[i].cache = NULL;
                     }   }
                   FREE_DYNAMIC_ARRAY(_physpages);
                   heap->physpages = physpages;
                 }
                 else
                 { heap->physpages = NULL; }
         }   }
         #endif
         #ifdef SPVW_PAGES
         {var uintC total_pagecount;
          #ifdef SPVW_BLOCKS
          total_pagecount = heapcount;
          #endif
          #ifdef SPVW_PAGES
          var uintC pagecounts[heapcount];
          # Pages-per-Heap-Tabelle initialisieren:
          READ(&pagecounts,sizeof(pagecounts));
          # total_pagecount berechnen:
          {var uintL heapnr;
           total_pagecount = 0;
           for (heapnr=0; heapnr<heapcount; heapnr++)
             { total_pagecount += pagecounts[heapnr]; }
          }
          #endif
          # Offset-per-Page-Tabelle initialisieren:
          begin_system_call();
          offset_pages = malloc(offset_pages_len*sizeof(*offset_pages));
          end_system_call();
          if (offset_pages==NULL) goto abbruch3;
          {var uintL pagenr;
           for (pagenr=0; pagenr<offset_pages_len; pagenr++)
             { offset_pages[pagenr].old_page_start = ~0L;
               offset_pages[pagenr].offset_page_o = 0;
          }  }
          # Adressen und Größen der Pages lesen und Pages allozieren:
          { var DYNAMIC_ARRAY(old_pages,memdump_page,total_pagecount);
            READ(old_pages,total_pagecount*sizeof(memdump_page));
           {var DYNAMIC_ARRAY(new_pages,aint,total_pagecount);
            {var memdump_page* old_page_ptr = &old_pages[0];
             var aint* new_page_ptr = &new_pages[0];
             var uintL heapnr;
             for (heapnr=0; heapnr<heapcount; heapnr++)
               {var Pages* pages_ptr = &mem.heaps[heapnr].inuse;
                #ifdef SPVW_PAGES
                var uintC pagecount = pagecounts[heapnr];
                until (pagecount==0)
                  {
                #endif
                    var uintL need = old_page_ptr->_page_end - old_page_ptr->_page_start;
                    var uintL size1 = round_up(need,sizeof(cons_));
                    if (size1 < std_page_size) { size1 = std_page_size; }
                    { var uintL size2 = size1 + sizeof_NODE + (varobject_alignment-1);
                      var aint addr = (aint)mymalloc(size2);
                      var Pages page;
                      if ((void*)addr == NULL) goto abbruch3;
                      #if !defined(AVL_SEPARATE)
                      page = (Pages)addr;
                      #else
                      begin_system_call();
                      page = (NODE*)malloc(sizeof(NODE));
                      end_system_call();
                      if (page == NULL) goto abbruch3;
                      #endif
                      # Page vom Betriebssystem bekommen.
                      page->m_start = addr; page->m_length = size2;
                      # Initialisieren:
                      page->page_start = page_start0(page);
                      page->page_end = page->page_start + need;
                      page->page_room = size1 - need;
                      # Diesem Heap zuschlagen:
                      *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);
                      *new_page_ptr = page->page_start;
                      {var aint old_page_start = old_page_ptr->_page_start;
                       var aint old_page_end = old_page_ptr->_page_end;
                       var oint offset_page_o = ((oint)page->page_start - (oint)old_page_start) << (oint_addr_shift-addr_shift);
                       var uintL pagenr = pagenr_of(old_page_start & addr_mask);
                       do { if (!(offset_pages[pagenr].old_page_start == ~0L)) { abort(); }
                            offset_pages[pagenr].old_page_start = old_page_start;
                            offset_pages[pagenr].offset_page_o = offset_page_o;
                            pagenr++;
                          }
                          while (pagenr < pagenr_of(old_page_end & addr_mask));
                    } }
                    old_page_ptr++; new_page_ptr++;
                #ifdef SPVW_PAGES
                    pagecount--;
                  }
                #endif
            }  }
            # Inhalt der Pages lesen:
            {var memdump_page* old_page_ptr = &old_pages[0];
             var aint* new_page_ptr = &new_pages[0];
             until (total_pagecount == 0)
               { var uintL len = old_page_ptr->_page_end - old_page_ptr->_page_start;
                 READ(*new_page_ptr,len);
                 old_page_ptr++; new_page_ptr++;
                 total_pagecount--;
            }  }
            FREE_DYNAMIC_ARRAY(new_pages);
           }
           FREE_DYNAMIC_ARRAY(old_pages);
          }
         }
         #endif
         #if defined(SPVW_PURE_BLOCKS) || defined(SPVW_MIXED_BLOCKS_STAGGERED) # SINGLEMAP_MEMORY || TRIVIALMAP_MEMORY && !SPVW_MIXED_BLOCKS_OPPOSITE
         #ifdef SELFMADE_MMAP
         mem.memfile_handle = handle;
         mem.memfile_still_being_read = TRUE;
         #endif
         # Alignment verwirklichen:
         READ_page_alignment(file_offset);
         # Inhalt der Blöcke lesen:
         {var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { var Heap* heapptr = &mem.heaps[heapnr];
              var uintL len = heapptr->heap_end - heapptr->heap_start;
              var uintL map_len = round_up(len,map_pagesize);
              heapptr->heap_limit = heapptr->heap_start + map_len;
              if (map_len > 0)
                { if (heapptr->heap_limit-1 > heapptr->heap_hardlimit-1) goto abbruch3;
                  #if defined(HAVE_MMAP) || defined(SELFMADE_MMAP)
                  # Wenn möglich, legen wir uns das Initialisierungsfile in den Speicher.
                  # Das sollte den Start beschleunigen und unnötiges Laden bis zur
                  # ersten GC verzögern.
                  # Hierzu ist das page_alignment nötig!
                  if (use_mmap)
                    {
                      #ifdef HAVE_MMAP
                      if (!( filemap((void*)(heapptr->heap_start),map_len,
                                     handle,file_offset
                                    )
                             == (void*)(-1)
                         ) )
                      #endif
                      #ifdef SELFMADE_MMAP
                      if ( selfmade_mmap(heapptr,map_len,file_offset) >=0)
                      #endif
                        {
                          #if 0 # unnötig, da mmap() kein lseek() braucht und danach nur noch CLOSE(handle) kommt.
                          if ( lseek(handle,map_len,SEEK_CUR) <0) goto abbruch1;
                          #endif
                          inc_file_offset(map_len);
                          goto block_done;
                        }
                        else
                        { asciz_out(DEUTSCH ? "Kann das Initialisierungsfile nicht in den Speicher legen." :
                                    ENGLISH ? "Cannot map the initialisation file into memory." :
                                    FRANCAIS ? "Ne peux placer le fichier d'initialisation en mémoire." :
                                    ""
                                   );
                          #ifdef HAVE_MMAP
                          errno_out(errno);
                          #else
                          asciz_out(NLstring);
                          #endif
                          use_mmap = FALSE;
                          # Bevor es mit READ(handle) weitergeht, ist evtl. ein lseek() nötig.
                          if ( lseek(handle,file_offset,SEEK_SET) <0) goto abbruch1;
                        }
                    }
                  #endif
                  if (zeromap((void*)(heapptr->heap_start),map_len) <0) goto abbruch3;
                  READ(heapptr->heap_start,len);
                  READ_page_alignment(len);
                  block_done: ;
         }  }   }
         #if defined(HAVE_MMAP) || defined(SELFMADE_MMAP)
         if (use_mmap)
           { # Länge des gemmapten Files überprüfen:
             #ifdef UNIX
             var struct stat statbuf;
             if (fstat(handle,&statbuf) < 0) goto abbruch1;
             if (!((uintL)statbuf.st_size >= file_offset)) goto abbruch2;
             #endif
             #ifdef WIN32_NATIVE
             var DWORD fsize = GetFileSize(handle,NULL);
             if (fsize == 0xFFFFFFFF) goto abbruch1;
             if (!(fsize >= file_offset)) goto abbruch2;
             #endif
           }
         #endif
         #endif
         #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
         # Objekte variabler Länge lesen:
         {var uintL len = header._mem_varobjects_end - header._mem_varobjects_start;
          #ifdef TRIVIALMAP_MEMORY
          var uintL map_len = round_up(len,map_pagesize);
          mem.varobjects.heap_limit = mem.varobjects.heap_start + map_len;
          if (zeromap((void*)mem.varobjects.heap_start,map_len) <0) goto abbruch3;
          #endif
          READ(mem.varobjects.heap_start,len);
         }
         # Conses lesen:
         {var uintL len = header._mem_conses_end - header._mem_conses_start;
          #ifdef TRIVIALMAP_MEMORY
          var uintL map_len = round_up(len,map_pagesize);
          mem.conses.heap_limit = mem.conses.heap_end - map_len;
          if (zeromap((void*)mem.conses.heap_limit,map_len) <0) goto abbruch3;
          #endif
          READ(mem.conses.heap_start,len);
         }
         #endif
         #ifdef GENERATIONAL_GC
         # Den SIGSEGV-Handler funktionsfähig machen.
         { var uintL heapnr;
           for (heapnr=0; heapnr<heapcount; heapnr++)
             { var Heap* heapptr = &mem.heaps[heapnr];
               heapptr->heap_gen0_start = heapptr->heap_start;
               heapptr->heap_gen0_end = heapptr->heap_end;
               #ifndef SPVW_PURE_BLOCKS
               heapptr->physpages = NULL;
               #endif
         }   }
         #endif
         #ifdef SELFMADE_MMAP
         # Ab jetzt brauchen wir den SIGSEGV-Handler.
         install_segv_handler();
         #endif
         # Durchlaufen durch alle LISP-Objekte und aktualisieren:
           #define update  loadmem_update
           # Programmkonstanten aktualisieren:
             update_tables();
           #ifdef SINGLEMAP_MEMORY_RELOCATE
           if (!offset_heaps_all_zero)
           #endif
           #if !defined(SPVW_PURE_BLOCKS) || defined(SINGLEMAP_MEMORY_RELOCATE)
           { # Pointer in den Cons-Zellen aktualisieren:
               #define update_conspage  update_conspage_normal
               update_conses();
               #undef update_conspage
             # Pointer in den Objekten variabler Länge aktualisieren:
               #define update_page  update_page_normal
               #ifdef FOREIGN
                 #define update_fpointer_invalid  TRUE
               #else
                 #define update_fpointer_invalid  FALSE
               #endif
               #define update_fsubr_function  TRUE
               #define update_ht_invalid  mark_ht_invalid
               #define update_fp_invalid  mark_fp_invalid
               #define update_fs_function  loadmem_update_fsubr
               update_varobjects();
               #undef update_fs_function
               #undef update_fp_invalid
               #undef update_ht_invalid
               #undef update_fsubr_function
               #undef update_fpointer_invalid
               #undef update_page
           }
           #endif
           #ifdef SINGLEMAP_MEMORY_RELOCATE
           else # i.e. if (offset_heaps_all_zero)
           #endif
           #ifdef SPVW_PURE_BLOCKS
           { # Pointer in den Cons-Zellen und Objekten variabler Länge
             # aktualisieren. Es gibt nur sehr wenige solcher Pointer, und
             # sie wurden beim Abspeichern aufgelistet.
             #if defined(HAVE_MMAP) || defined(SELFMADE_MMAP)
             if (use_mmap)
               { if ( lseek(handle,file_offset,SEEK_SET) <0) goto abbruch1; }
             #endif
             { var memdump_reloc_header rheader;
               READ(&rheader,sizeof(rheader));
               if (rheader.reloccount > 0)
                 { var DYNAMIC_ARRAY(relocbuf,object*,rheader.reloccount);
                  {var object** relocbufptr = &relocbuf[0];
                   var uintL count;
                   READ(&relocbuf[0],rheader.reloccount*sizeof(object*));
                   dotimespL(count,rheader.reloccount, { update(*relocbufptr++); });
                   FREE_DYNAMIC_ARRAY(relocbuf);
                 }}
               if (rheader.htcount > 0)
                 { var DYNAMIC_ARRAY(htbuf,Hashtable,rheader.htcount);
                  {var Hashtable* htbufptr = &htbuf[0];
                   var uintL count;
                   READ(&htbuf[0],rheader.htcount*sizeof(Hashtable));
                   dotimespL(count,rheader.htcount,
                     { var Hashtable ptr = *htbufptr++; mark_ht_invalid(ptr); });
                   FREE_DYNAMIC_ARRAY(htbuf);
                 }}
               if (rheader.fpcount > 0)
                 { var DYNAMIC_ARRAY(fpbuf,Record,rheader.fpcount);
                  {var Record* fpbufptr = &fpbuf[0];
                   var uintL count;
                   READ(&fpbuf[0],rheader.fpcount*sizeof(Record));
                   dotimespL(count,rheader.fpcount,
                     { var Record ptr = *fpbufptr++; mark_fp_invalid(ptr); });
                   FREE_DYNAMIC_ARRAY(fpbuf);
                 }}
               if (rheader.fscount > 0)
                 { var DYNAMIC_ARRAY(fsbuf,Fsubr,rheader.fscount);
                  {var Fsubr* fsbufptr = &fsbuf[0];
                   var uintL count;
                   READ(&fsbuf[0],rheader.fscount*sizeof(Fsubr));
                   dotimespL(count,rheader.fscount,
                     { var Fsubr fsubrptr = *fsbufptr++; loadmem_update_fsubr(fsubrptr); });
                   FREE_DYNAMIC_ARRAY(fsbuf);
                 }}
           } }
           #endif
           #undef update
         # File schließen:
         #undef READ
         #ifdef SELFMADE_MMAP
           mem.memfile_still_being_read = FALSE;
         #else
           begin_system_call();
           #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
           if ( CLOSE(handle) <0) goto abbruch1;
           #elif defined(AMIGAOS)
           # Never close handles twice
           if ( CLOSE(handle) <0) { handle = Handle_NULL; goto abbruch1; }
           #elif defined(WIN32_NATIVE)
           if (!CloseHandle(handle)) { handle = INVALID_HANDLE_VALUE; goto abbruch1; }
           #endif
           end_system_call();
         #endif
         #ifdef SPVW_PAGES
         begin_system_call(); free(offset_pages); end_system_call();
         recalc_space(FALSE);
         #endif
         #if defined(SPVW_PURE_BLOCKS) || defined(TRIVIALMAP_MEMORY) || defined(GENERATIONAL_GC) # SINGLEMAP_MEMORY || TRIVIALMAP_MEMORY || GENERATIONAL_GC
         #ifdef GENERATIONAL_GC
         { var uintL heapnr;
           for (heapnr=0; heapnr<heapcount; heapnr++)
             { var Heap* heap = &mem.heaps[heapnr];
               #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
               if (is_cons_heap(heapnr))
                 { heap->heap_start = heap->heap_gen1_end = heap->heap_start & -physpagesize; }
                 else
                 { heap->heap_gen1_start = heap->heap_end = (heap->heap_end + (physpagesize-1)) & -physpagesize; }
               #else # defined(SPVW_PURE_BLOCKS) || defined(SPVW_MIXED_BLOCKS_STAGGERED)
               heap->heap_gen1_start = heap->heap_end = heap->heap_limit;
               #endif
               #ifdef SPVW_PURE_BLOCKS
               # Don't need to rebuild the cache.
               xmmprotect_old_generation_cache(heapnr);
               #else
               if (!is_unused_heap(heapnr))
                 { build_old_generation_cache(heapnr); }
               #endif
         }   }
         #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
         if (!(mem.varobjects.heap_end <= mem.conses.heap_start)) goto abbruch3;
         #endif
         #ifndef SELFMADE_MMAP
         # Ab jetzt brauchen wir den SIGSEGV-Handler.
         install_segv_handler();
         #endif
         #endif
         { var uintL space = used_space();
           set_total_room(space); # bis zur nächsten GC haben wir viel Zeit
           #ifdef GENERATIONAL_GC
           mem.last_gcend_space0 = space;
           mem.last_gcend_space1 = 0;
           #endif
         }
         #endif
         FREE_DYNAMIC_ARRAY(old_modules);
        }
        begin_system_call(); free(offset_subrs); end_system_call();
      }}
      # offene Files für geschlossen erklären:
      closed_all_files();
      #ifdef GENERATIONAL_GC
      # bisher keine GCs:
      O(gc_count) = Fixnum_0;
      #endif
      #ifdef MACHINE_KNOWN
        # (MACHINE-TYPE), (MACHINE-VERSION), (MACHINE-INSTANCE)
        # wieder für unbekannt erklären:
        O(machine_type_string) = NIL;
        O(machine_version_string) = NIL;
        O(machine_instance_string) = NIL;
      #endif
      #ifndef LANGUAGE_STATIC
        #ifdef GNU_GETTEXT
          # Cache von (SYS::CURRENT-LANGUAGE) löschen:
          O(current_language_cache) = NIL;
        #endif
        # Cache von (LISP-IMPLEMENTATION-VERSION) löschen
        # (hängt von (SYS::CURRENT-LANGUAGE) ab):
        O(lisp_implementation_version_string) = NIL;
        #if defined(GNU)
        O(software_version_string) = NIL;
        #endif
      #endif
      CHECK_AVL_CONSISTENCY();
      CHECK_GC_CONSISTENCY();
      CHECK_GC_UNMARKED(); CHECK_NULLOBJ(); CHECK_GC_CACHE(); CHECK_GC_GENERATIONAL(); SAVE_GC_DATA();
      CHECK_PACK_CONSISTENCY();
      return;
      abbruch1:
        {var int abbruch_errno = OS_errno;
         asciz_out(program_name); asciz_out(": ");
         asciz_out(
           DEUTSCH ? "Betriebssystem-Fehler beim Versuch, das Initialisierungsfile zu laden." NLstring :
           ENGLISH ? "operating system error during load of initialisation file" NLstring :
           FRANCAIS ? "Erreur système pendant le chargement du fichier d'initialisation." NLstring :
           ""
           );
         errno_out(abbruch_errno);
        }
        goto abbruch_quit;
      abbruch2:
        asciz_out(program_name); asciz_out(": ");
        asciz_out(
          DEUTSCH ? "Initialisierungsfile wurde nicht von dieser LISP-Version erzeugt." NLstring :
          ENGLISH ? "initialisation file was not created by this version of LISP" NLstring :
          FRANCAIS ? "Le fichier d'initialisation ne provient pas de cette version de LISP." NLstring :
          ""
          );
        goto abbruch_quit;
      abbruch3:
        asciz_out(program_name); asciz_out(": ");
        asciz_out(
          DEUTSCH ? "Speicherplatz reicht für Initialisierung nicht aus." NLstring :
          ENGLISH ? "not enough memory for initialisation" NLstring :
          FRANCAIS ? "Il n'y a pas assez de mémoire pour l'initialisation." NLstring :
          ""
          );
        goto abbruch_quit;
      abbruch_quit:
        # Abbruch.
        # Zuvor die Datei schließen, falls sie erfolgreich geöffnet worden war.
        # (Hierbei werden Fehler nun aber wirklich ignoriert!)
        #ifdef AMIGAOS
        if (!(handle==Handle_NULL))
          { begin_system_call(); CLOSE(handle); end_system_call(); }
        #endif
        #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(WATCOM) || defined(RISCOS)
        if (!(handle<0))
          { begin_system_call(); CLOSE(handle); end_system_call(); }
        #endif
        #ifdef WIN32_NATIVE
        if (!(handle==INVALID_HANDLE_VALUE))
          { begin_system_call(); CloseHandle(handle); end_system_call(); }
        #endif
        quit_sofort(1);
    }}

# ------------------------------------------------------------------------------
#                       Dynamisches Laden von Modulen

#ifdef DYNAMIC_MODULES

# Attaches a shared library to this process' memory, and attempts to load
# a number of clisp modules from it.
  global void dynload_modules (const char * library, uintC modcount, const char * const * modnames);
  nonreturning_function(local, fehler_dlerror, (const char * func, const char * symbol, const char * errstring));
  local void fehler_dlerror(func,symbol,errstring)
    var const char * func;
    var const char * symbol;
    var const char * errstring;
    { end_system_call();
      if (errstring == NULL) { errstring = "Unknown error"; }
      pushSTACK(asciz_to_string(errstring));
      if (!(symbol == NULL)) { pushSTACK(asciz_to_string(symbol)); }
      pushSTACK(asciz_to_string(func));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error, (symbol == NULL ? "~: ~ -> ~" : "~: ~(~) -> ~"));
    }
  global void dynload_modules(library,modcount,modnames)
    var const char * library;
    var uintC modcount;
    var const char * const * modnames;
    { var void* libhandle;
      begin_system_call();
      # Open the library.
      libhandle = dlopen(library,RTLD_NOW);
      if (libhandle == NULL) fehler_dlerror("dlopen",NULL,dlerror());
      end_system_call();
      if (modcount > 0)
        { # What's the longest module name? What's their total size?
          var uintL max_modname_length = 0;
          var uintL total_modname_length = 0;
          begin_system_call();
          { var const char * const * modnameptr = modnames;
            var uintC count;
            dotimespC(count,modcount,
              { var uintL len = asciz_length(*modnameptr);
                if (len > max_modname_length) { max_modname_length = len; }
                total_modname_length += len+1;
                modnameptr++;
              });
          }
          # Make room for the module descriptors.
         {var module_* modules = (module_*) malloc(modcount*sizeof(module_)+total_modname_length);
          if (modules==NULL) fehler_dlerror("malloc",NULL,"out of memory");
          { var char* modnamebuf = (char*)(&modules[modcount]);
            var DYNAMIC_ARRAY(symbolbuf,char,8+max_modname_length+21+1);
            var const char * const * modnameptr = modnames;
            var module_* module = modules;
            var uintC count;
            dotimespC(count,modcount,
              { var const char * modname = *modnameptr;
                var uintL len = asciz_length(modname);
                var const char * err;
                # Copy modname into modnamebuf:
                module->name = modnamebuf;
                { var const char * ptr = modname;
                  until ((*modnamebuf++ = *ptr++) == '\0') {}
                }
                # Find the addresses of some C data in the shared library:
                { sprintf(symbolbuf,"module__%s__subr_tab",modname);
                  module->stab = (subr_*) dlsym(libhandle,symbolbuf);
                  err = dlerror();
                  if (err) fehler_dlerror("dlsym",symbolbuf,err);
                }
                { sprintf(symbolbuf,"module__%s__subr_tab_size",modname);
                  module->stab_size = (uintC*) dlsym(libhandle,symbolbuf);
                  err = dlerror();
                  if (err) fehler_dlerror("dlsym",symbolbuf,err);
                }
                { sprintf(symbolbuf,"module__%s__object_tab",modname);
                  module->otab = (object*) dlsym(libhandle,symbolbuf);
                  err = dlerror();
                  if (err) fehler_dlerror("dlsym",symbolbuf,err);
                }
                { sprintf(symbolbuf,"module__%s__object_tab_size",modname);
                  module->otab_size = (uintC*) dlsym(libhandle,symbolbuf);
                  err = dlerror();
                  if (err) fehler_dlerror("dlsym",symbolbuf,err);
                }
                module->initialized = FALSE;
                { sprintf(symbolbuf,"module__%s__subr_tab_initdata",modname);
                  module->stab_initdata = (subr_initdata*) dlsym(libhandle,symbolbuf);
                  err = dlerror();
                  if (err) fehler_dlerror("dlsym",symbolbuf,err);
                }
                { sprintf(symbolbuf,"module__%s__object_tab_initdata",modname);
                  module->otab_initdata = (object_initdata*) dlsym(libhandle,symbolbuf);
                  err = dlerror();
                  if (err) fehler_dlerror("dlsym",symbolbuf,err);
                }
                # Find the addresses of some C functions in the shared library:
                { sprintf(symbolbuf,"module__%s__init_function_1",modname);
                  module->initfunction1 = (void (*) (module_*)) dlsym(libhandle,symbolbuf);
                  err = dlerror();
                  if (err) fehler_dlerror("dlsym",symbolbuf,err);
                }
                { sprintf(symbolbuf,"module__%s__init_function_2",modname);
                  module->initfunction2 = (void (*) (module_*)) dlsym(libhandle,symbolbuf);
                  err = dlerror();
                  if (err) fehler_dlerror("dlsym",symbolbuf,err);
                }
                module->next = NULL;
                modnameptr++; module++;
              });
            FREE_DYNAMIC_ARRAY(symbolbuf);
          }
          end_system_call();
          # We found all the necessary symbols. Now register the modules.
          { var module_* module = modules;
            var uintC mcount = modcount;
            while (mcount-- > 0)
              { add_module(module);
                # Vorinitialisierung, vgl. init_subr_tab_1.
                { var subr_* ptr = module->stab; # subr_tab durchgehen
                  var uintC count;
                  dotimesC(count,*module->stab_size,
                    { ptr->argtype =
                        (uintW)subr_argtype(ptr->req_anz,ptr->opt_anz,(subr_rest_t)(ptr->rest_flag),(subr_key_t)(ptr->key_flag));
                      ptr++;
                    });
                }
                #if (defined(MULTIMAP_MEMORY) || defined(SINGLEMAP_MEMORY)) && defined(MAP_MEMORY_TABLES)
                { var subr_* newptr = (subr_*)&subr_tab + total_subr_anz;
                  var uintC count = *module->stab_size;
                  if (count > 0)
                    { { var uintL old_map_len = round_up(total_subr_anz*sizeof(subr_),map_pagesize);
                        var uintL new_map_len = round_up((total_subr_anz+count)*sizeof(subr_),map_pagesize);
                        if (old_map_len < new_map_len)
                          { if (zeromap((void*)((aint)&subr_tab+old_map_len),new_map_len-old_map_len) <0)
                              fehler_dlerror("zeromap",NULL,"out of memory for subr_tab");
                          }
                      }
                      { var subr_* oldptr = module->stab;
                        module->stab = newptr;
                        dotimespC(count,count,
                          { *newptr = *oldptr++;
                            newptr->name = NIL; newptr->keywords = NIL; # damit GC möglich bleibt
                            newptr++;
                          });
                      }
                      total_subr_anz += *module->stab_size;
                }   }
                #elif defined(MULTIMAP_MEMORY)
                if (*module->stab_size > 0)
                  # Die subr_tab des geladenen Moduls multimappen.
                  # Die zu mappenden Pages gehören zum Datensegment der neu
                  # geladenen Shared-Library, sind also sicher noch nicht gemultimappt.
                  { var aint subr_tab_start = round_down((aint)module->stab,pagesize);
                    var aint subr_tab_end = round_up((aint)module->stab+(*module->stab_size)*sizeof(subr_),pagesize);
                    multimap(case_machine: case_subr: , subr_tab_start, subr_tab_end-subr_tab_start, TRUE);
                    if (FALSE)
                      no_mem:
                      fehler_dlerror("multimap",NULL,"out of memory for subr_tab");
                  }
                #endif
                # Hauptinitialisierung.
                init_module_2(module);
                module++;
              }
          }
          # Now start the modules' life.
          { var module_* module = modules;
            var uintC count;
            dotimespC(count,modcount,
              { if (module->initfunction2)
                  # Initialisierungsfunktion aufrufen:
                  (*module->initfunction2)(module);
                module++;
              });
          }
        }}
    }

#endif

# ------------------------------------------------------------------------------
#                                Version
#ifdef AMIGAOS
# Es gibt eine Utility, die ein Executable nach einem Versionsstring absucht.
# Format "name version.revision (date)\r\n"
  global const char version_string[] =
    "$VER: CLISP"
    #if defined(WIDE)
      "-wide"
    #elif !defined(TYPECODES)
      "-typ2"
    #elif defined(AMIGA3000)
      "-high"
    #elif defined(MC68000)
      "-68000"
    #else
      "-low"
    #endif
    " "STRINGIFY(VERSION_YYYY)"."STRINGIFY(VERSION_MM) # Datum als Versionsnummer
    " (" VERSION ")\r\n"; # Datum in Klammern
#endif

static const char * const copyright_notice[] = {
  "                                                                    ",
  "Copyright (c) Bruno Haible, Michael Stoll, Marcus Daniels 1992-1998 ",
  "                                                                     ",
  "This program is free software; you can redistribute it and/or modify",
  "it under the terms of the GNU General Public License as published by",
  "the Free Software Foundation; either version 2, or (at your option) ",
  "any later version.                                                  ",
  "                                                                      ",
  "This program is distributed in the hope that it will be useful, but ",
  "WITHOUT ANY WARRANTY; without even the implied warranty of          ",
  "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   ",
  "General Public License for more details.                            ",
  "                                                                       ",
  "You should have received a copy of the GNU General Public License   ",
  "along with this program; if not, write to the Free Software         ",
  "Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.",
  "                                                                        ",
  (const char *) &copyright_notice
};

# ------------------------------------------------------------------------------

