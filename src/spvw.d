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

# GC-Statistik:
  global uintL  gc_count = 0;      # Zähler für GC-Aufrufe
  global uintL2 gc_space =         # Größe des von der GC insgesamt bisher
                                   # wiederbeschafften Platzes (64-Bit-Akku)
    #ifdef intQsize
      0
    #else
      {0,0}
    #endif
    ;
# Zeit, die die GC verbraucht:
  global internal_time gc_time =        # GC-Zeitverbrauch bisher insgesamt
    #ifdef TIME_1
    0
    #endif
    #ifdef TIME_2
    {0,0}
    #endif
    ;

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

local /* uintL */ aint physpagesize;  # = map_pagesize
local uintL physpageshift; # 2^physpageshift = physpagesize

typedef enum { handler_failed, handler_done }
        handle_fault_result;
local handle_fault_result handle_fault (aint address);

#ifdef SELFMADE_MMAP # impliziert SPVW_PURE_BLOCKS <==> SINGLEMAP_MEMORY
                     # oder       SPVW_MIXED_BLOCKS_STAGGERED

# Unterroutine fürs Lesen einer Page vom mem-File.
local int handle_mmap_fault (uintL offset, aint address, uintB* memfile_page);
local int handle_mmap_fault(offset,address,memfile_page)
  var uintL offset;
  var aint address;
  var uintB* memfile_page;
  { if (*memfile_page == 0)
      # Page already in memory, nothing to be done.
      return 0;
    # Fetch the page from the file.
    { var Handle handle = mem.memfile_handle;
      var sintL orig_offset = 0;
      # If loadmem() is still reading from the memfile, we must be careful
      # to restore the handle's file position. (This could be avoided under
      # UNIX by using dup(), but not on WIN32_NATIVE.)
      if (mem.memfile_still_being_read)
        { orig_offset = lseek(handle,0,SEEK_CUR);
          if (orig_offset < 0)
            { asciz_out("selfmade_mmap: lseek() failed.");
              errno_out(OS_errno);
              return -1;
            }
        }
      if (zeromap((void*)address,physpagesize) < 0)
        { asciz_out("selfmade_mmap: zeromap() failed.");
          return -1;
        }
      if (lseek(handle,offset,SEEK_SET) < 0)
        { asciz_out_1("selfmade_mmap: lseek(0x%x) failed.",offset);
          errno_out(OS_errno);
          return -1;
        }
      #ifdef DEBUG_SPVW
      asciz_out_2("selfmade_mmap: address=0x%x <-- offset=0x%x" NLstring,address,offset);
      #endif
     {var sintL res;
      #ifdef WIN32_NATIVE
      # Call ReadFile(), not full_read(), because we don't want to handle Ctrl-C now.
      if (!ReadFile(handle,(void*)address,physpagesize,&res,NULL)) { res = -1; }
      #else
      res = full_read(handle,(void*)address,physpagesize);
      #endif
      if (!(res == physpagesize))
        { asciz_out_3("selfmade_mmap: full_read(offset=0x%x,count=%d) failed, returned %d.",offset,physpagesize,res);
          if (res < 0) errno_out(OS_errno);
          return -1;
        }
      if (mem.memfile_still_being_read)
        { if (lseek(handle,orig_offset,SEEK_SET) < 0)
            { asciz_out_1("selfmade_mmap: lseek(0x%x) failed.",orig_offset);
              errno_out(OS_errno);
              return -1;
            }
        }
      # Done.
      *memfile_page = 0;
      return 1;
  } }}

#endif # SELFMADE_MMAP

#ifdef GENERATIONAL_GC # impliziert SPVW_PURE_BLOCKS <==> SINGLEMAP_MEMORY
                       # oder       SPVW_MIXED_BLOCKS_STAGGERED
                       # oder       SPVW_MIXED_BLOCKS_OPPOSITE

# Unterroutine für protection: PROT_NONE -> PROT_READ
local int handle_read_fault (aint address, physpage_state* physpage);
local int handle_read_fault(address,physpage)
  var aint address;
  var physpage_state* physpage;
  { # Seite auf den Stand des Cache bringen:
    { var uintL count = physpage->cache_size;
      if (count > 0)
        { var old_new_pointer* ptr = physpage->cache;
          #if !defined(MULTIMAP_MEMORY)
          if (mprotect((MMAP_ADDR_T)address, physpagesize, PROT_READ_WRITE) < 0)
            return -1;
          #endif
          dotimespL(count,count, { *(ptr->p) = ptr->o; ptr++; } );
    }   }
    # Seite read-only einblenden:
    #if !defined(MULTIMAP_MEMORY)
    if (mprotect((MMAP_ADDR_T)address, physpagesize, PROT_READ) < 0)
      return -1;
    #else # MULTIMAP_MEMORY
    #if !defined(WIDE_SOFT)
    ASSERT(address == upointer(address));
    #endif
    { var uintL type;
      for (type = 0; type < typecount; type++)
        if (mem.heapnr_from_type[type] >= 0) # type in MM_TYPECASES aufgeführt?
          { if (mprotect((MMAP_ADDR_T)combine(type,address), physpagesize, PROT_READ) < 0)
              return -1;
    }     }
    #endif
    physpage->protection = PROT_READ;
    return 0;
  }

# Unterroutine für protection: PROT_READ -> PROT_READ_WRITE
local int handle_readwrite_fault (aint address, physpage_state* physpage);
local int handle_readwrite_fault(address,physpage)
  var aint address;
  var physpage_state* physpage;
  { # Seite read-write einblenden:
    #if !defined(MULTIMAP_MEMORY)
    if (mprotect((MMAP_ADDR_T)address, physpagesize, PROT_READ_WRITE) < 0)
      return -1;
    #else # MULTIMAP_MEMORY
    ASSERT(address == upointer(address));
    { var uintL type;
      for (type = 0; type < typecount; type++)
        if (mem.heapnr_from_type[type] >= 0) # type in MM_TYPECASES aufgeführt?
          if (mprotect((MMAP_ADDR_T)combine(type,address), physpagesize, PROT_READ_WRITE) < 0)
            return -1;
    }
    #endif
    physpage->protection = PROT_READ_WRITE;
    return 0;
  }

# mapped generation: the old one
#define heap_mgen_start  heap_gen0_start
#define heap_mgen_end    heap_gen0_end

#else

#define heap_mgen_start  heap_start
#define heap_mgen_end    heap_end

#endif # GENERATIONAL_GC

local handle_fault_result handle_fault(address)
  var aint address;
  { var uintL heapnr;
    var object obj = as_object((oint)address << oint_addr_shift);
    var aint uaddress = canon(address); # hoffentlich = canonaddr(obj);
    var aint pa_uaddress = uaddress & -physpagesize; # page aligned address
    #ifdef SPVW_PURE_BLOCKS
    heapnr = typecode(obj);
    #elif defined(SPVW_MIXED_BLOCKS_STAGGERED)
    heapnr = (uaddress >= mem.heaps[1].heap_mgen_start ? 1 : 0);
    #else # SPVW_MIXED_BLOCKS_OPPOSITE
    heapnr = (uaddress >= mem.heaps[1].heap_start ? 1 : 0);
    #endif
    {var Heap* heap = &mem.heaps[heapnr];
     var uintL pageno;
     #ifdef SELFMADE_MMAP
     if (is_unused_heap(heapnr)) goto error1;
     if (!((heap->heap_mgen_start <= uaddress) && (uaddress < heap->heap_mgen_end)))
       goto error2;
     pageno = (pa_uaddress>>physpageshift)-(heap->heap_mgen_start>>physpageshift);
     if (pageno < heap->memfile_numpages)
       { var int did_pagein =
           handle_mmap_fault(heap->memfile_offset+(pageno<<physpageshift),
                             pa_uaddress,
                             &heap->memfile_pages[pageno]);
         if (did_pagein < 0) goto error3;
         if (did_pagein)
           {
             #ifdef GENERATIONAL_GC
             if (heap->physpages == NULL) # implicitly PROT_READ_WRITE
               return handler_done;
             switch (heap->physpages[pageno].protection)
               { case PROT_READ_WRITE:
                   return handler_done;
                 #ifdef SPVW_PURE_BLOCKS
                 case PROT_READ:
                   if (mprotect((MMAP_ADDR_T)pa_uaddress,physpagesize,PROT_READ) < 0) goto error3;
                   return handler_done;
                 case PROT_NONE:
                   if (mprotect((MMAP_ADDR_T)pa_uaddress,physpagesize,PROT_NONE) < 0) goto error3;
                   return handler_done;
                 #endif
                 default:
                   goto error4;
               }
             #else
             return handler_done;
             #endif
           }
       }
     #endif
     #ifdef GENERATIONAL_GC
     if (!is_heap_containing_objects(heapnr)) goto error1;
     if (!((heap->heap_gen0_start <= uaddress) && (uaddress < heap->heap_gen0_end))) goto error2;
     if (heap->physpages == NULL) goto error5;
     #ifndef SELFMADE_MMAP
     pageno = (pa_uaddress>>physpageshift)-(heap->heap_gen0_start>>physpageshift);
     #endif
     {var physpage_state* physpage = &heap->physpages[pageno];
      switch (physpage->protection)
        { case PROT_NONE:
            # protection: PROT_NONE -> PROT_READ
            if (handle_read_fault(pa_uaddress,physpage) < 0) goto error6;
            return handler_done;
          case PROT_READ:
            # protection: PROT_READ -> PROT_READ_WRITE
            if (handle_readwrite_fault(pa_uaddress,physpage) < 0) goto error7;
            return handler_done;
          case PROT_READ_WRITE:
            goto error8;
          default:
            goto error9;
        }
      error6: # handle_read_fault() failed
        { var int saved_errno = OS_errno;
          asciz_out_2(NLstring "*** - " "handle_fault error6 ! mprotect(0x%x,0x%x,...) -> ", address & -physpagesize, physpagesize);
          errno_out(saved_errno);
        }
        goto error;
      error7: # handle_readwrite_fault() failed
        { var int saved_errno = OS_errno;
          asciz_out_3(NLstring "*** - " "handle_fault error7 ! mprotect(0x%x,0x%x,%d) -> ", address & -physpagesize, physpagesize, PROT_READ_WRITE);
          errno_out(saved_errno);
        }
        goto error;
      error8: # fault on a read-write page
        asciz_out_1(NLstring "*** - " "handle_fault error8 ! protection = %d", physpage->protection);
        goto error;
      error9: # invalid protection value
        asciz_out_1(NLstring "*** - " "handle_fault error9 ! protection = %d", physpage->protection);
        goto error;
     }
     error5: # fault on a read-write page with no physpages array
       asciz_out(NLstring "*** - " "handle_fault error5 !");
       goto error;
     #endif
     error1: # A fault was not expected on this type of heap.
       asciz_out(NLstring "*** - " "handle_fault error1 !");
       goto error;
     error2: # The address is outside of the used address range for this heap.
       asciz_out_3(NLstring "*** - " "handle_fault error2 ! address = 0x%x not in [0x%x,0x%x) !", address, heap->heap_mgen_start, heap->heap_mgen_end);
       goto error;
     #ifdef SELFMADE_MMAP
     error3: # handle_mmap_fault() failed
       asciz_out(NLstring "*** - " "handle_fault error3 !");
       goto error;
     #endif
     #if defined(SELFMADE_MMAP) && defined(GENERATIONAL_GC)
     error4: # The page ought not to be read-write, although we just paged it in.
       asciz_out_1(NLstring "*** - " "handle_fault error4 ! protection = %d",heap->physpages[pageno].protection);
       goto error;
     #endif
    }
    error:
    return handler_failed;
  }

#if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED_BLOCKS)) || defined(SELFMADE_MMAP)
# Systemaufrufe wie read() und write() melden kein SIGSEGV, sondern EFAULT.
# handle_fault_range(PROT_READ,start,end) macht einen Adreßbereich lesbar,
# handle_fault_range(PROT_READ_WRITE,start,end) macht ihn schreibbar.
global boolean handle_fault_range (int prot, aint start_address, aint end_address);
global boolean handle_fault_range(prot,start_address,end_address)
  var int prot;
  var aint start_address;
  var aint end_address;
  { start_address = canon(start_address);
    end_address = canon(end_address);
    if (!(start_address < end_address)) { return TRUE; }
   {var Heap* heap = &mem.heaps[0]; # varobject_heap
    var boolean did_pagein = FALSE;
    if ((end_address <= heap->heap_mgen_start) || (heap->heap_mgen_end <= start_address))
      return TRUE; # nichts zu tun, aber seltsam, daß überhaupt ein Fehler kam
    #ifdef SELFMADE_MMAP
    if (heap->memfile_numpages > 0)
      { var aint pa_uaddress;
        for (pa_uaddress = start_address & -physpagesize; pa_uaddress < end_address; pa_uaddress += physpagesize)
          if ((heap->heap_mgen_start <= pa_uaddress) && (pa_uaddress < heap->heap_mgen_end))
            { var uintL pageno = (pa_uaddress>>physpageshift)-(heap->heap_mgen_start>>physpageshift);
              if (pageno < heap->memfile_numpages)
                switch (handle_mmap_fault(heap->memfile_offset+(pageno<<physpageshift),
                                          pa_uaddress,
                                          &heap->memfile_pages[pageno]))
                  { case 1:
                      did_pagein = TRUE;
                      #if defined(GENERATIONAL_GC) && defined(SPVW_PURE_BLOCKS)
                      if (!(heap->physpages == NULL))
                        switch (heap->physpages[pageno].protection)
                          { case PROT_NONE:
                              if (!(prot == PROT_READ || prot == PROT_READ_WRITE))
                                { if (mprotect((MMAP_ADDR_T)pa_uaddress,physpagesize,PROT_NONE) < 0)
                                    return FALSE;
                                  break;
                                }
                              if (handle_read_fault(pa_uaddress,&heap->physpages[pageno]) < 0)
                                return FALSE;
                              /* fallthrough */
                            case PROT_READ:
                              if (!(prot == PROT_READ_WRITE))
                                { if (mprotect((MMAP_ADDR_T)pa_uaddress,physpagesize,PROT_READ) < 0)
                                    return FALSE;
                                  break;
                                }
                              if (handle_readwrite_fault(pa_uaddress,&heap->physpages[pageno]) < 0)
                                return FALSE;
                              /* fallthrough */
                            case PROT_READ_WRITE:
                              break;
                          }
                      #endif
                      break;
                    case 0: break;
                    default: return FALSE;
            }     }
      }
    #endif
    #ifdef GENERATIONAL_GC
    if (heap->physpages == NULL)
      { if (did_pagein) { return TRUE; }
        return FALSE;
      }
    { var aint pa_uaddress;
      for (pa_uaddress = start_address & -physpagesize; pa_uaddress < end_address; pa_uaddress += physpagesize)
        if ((heap->heap_gen0_start <= pa_uaddress) && (pa_uaddress < heap->heap_gen0_end))
          { var uintL pageno = (pa_uaddress>>physpageshift)-(heap->heap_gen0_start>>physpageshift);
            var physpage_state* physpage = &heap->physpages[pageno];
            if ((physpage->protection == PROT_NONE) && (prot == PROT_READ || prot == PROT_READ_WRITE))
              # protection: PROT_NONE -> PROT_READ
              { if (handle_read_fault(pa_uaddress,physpage) < 0)
                  return FALSE;
              }
            if (!(physpage->protection == PROT_READ_WRITE) && (prot == PROT_READ_WRITE))
              # protection: PROT_READ -> PROT_READ_WRITE
              { if (handle_readwrite_fault(pa_uaddress,physpage) < 0)
                  return FALSE;
              }
    }     }
    return TRUE;
    #else
    return did_pagein;
    #endif
  }}
#endif

#ifdef SELFMADE_MMAP

# Simulate an mmap for the given heap, of length map_len (a positive multiple of physpagesize).
local int selfmade_mmap (Heap* heap, uintL map_len, uintL offset);
local int selfmade_mmap(heap,map_len,offset)
  var Heap* heap;
  var uintL map_len;
  var uintL offset;
  { var uintL pagecount = map_len>>physpageshift;
    var uintB* pages = (uintB*)malloc(pagecount*sizeof(uintB));
    if (pages == NULL) { return -1; }
    heap->memfile_numpages = pagecount;
    heap->memfile_pages = pages;
    heap->memfile_offset = offset;
    # Initially, all pages are paged out.
    dotimespL(pagecount,pagecount, { *pages++ = 1; } );
    return 0;
  }

#endif

#ifdef GENERATIONAL_GC

# mprotect() mit Ausstieg im Falle des Scheiterns
local void xmprotect (aint addr, uintL len, int prot);
local void xmprotect(addr,len,prot)
  var aint addr;
  var uintL len;
  var int prot;
  { if (mprotect((MMAP_ADDR_T)addr,len,prot) < 0)
      { asciz_out(DEUTSCH ? "mprotect() klappt nicht." :
                  ENGLISH ? "mprotect() fails." :
                  FRANCAIS ? "mprotect() ne fonctionne pas." :
                  ""
                 );
        errno_out(OS_errno);
        abort();
  }   }

#ifdef MULTIMAP_MEMORY
  # mehrfaches mprotect() auf alle Mappings eines Adreßbereiches
  local void xmmprotect (Heap* heap, aint addr, uintL len, int prot);
  local void xmmprotect(heap,addr,len,prot)
    var Heap* heap;
    var aint addr;
    var uintL len;
    var int prot;
    { unused heap;
      { var uintL type;
        for (type = 0; type < typecount; type++)
          if (mem.heapnr_from_type[type] >= 0) # type in MM_TYPECASES aufgeführt?
            { xmprotect((aint)combine(type,addr),len,prot); }
      }
    }
#else
  # Nur ein Adreßbereich
  #ifdef SELFMADE_MMAP
    local void xmmprotect (Heap* heap, aint addr, uintL len, int prot);
    local void xmmprotect(heap,addr,len,prot)
      var Heap* heap;
      var aint addr;
      var uintL len;
      var int prot;
      { # Überspringe die noch nicht eingeblendeten Seiten und mimimiere
        # dabei die Anzahl der nötigen mprotect()-Aufrufe: Auf Halde steht
        # ein mprotect-Aufruf für das Intervall [todo_address,address-1].
        var aint todo_address = 0;
        #define do_todo()  \
          { if (todo_address)                                        \
              { if (todo_address < address)                          \
                  xmprotect(todo_address,address-todo_address,prot); \
                todo_address = 0;                                    \
          }   }
        #define addto_todo()  \
          { if (todo_address)             \
              {} # incrementiere address  \
              else                        \
              { todo_address = address; } \
          }
        var aint address = addr; # multiple of physpagesize
        var uintL pageno = (address - (heap->heap_gen0_start & -physpagesize)) >> physpageshift;
        var uintL pagecount = len >> physpageshift;
        var uintL count;
        dotimesL(count,pagecount,
          { if (!(pageno < heap->memfile_numpages && heap->memfile_pages[pageno]))
              { addto_todo(); }
              else
              { do_todo(); }
            address += physpagesize;
            pageno++;
          });
        do_todo();
        #undef addto_todo
        #undef do_todo
      }
  #else
    #define xmmprotect(heap,addr,len,prot)  xmprotect(addr,len,prot)
  #endif
#endif

# Versionen von malloc() und realloc(), bei denen der Input auch = NULL sein darf:
  #define xfree(ptr)  \
    if (!((ptr)==NULL)) free(ptr);
  #define xrealloc(ptr,size)  \
    (((ptr)==NULL) ? (void*)malloc(size) : (void*)realloc(ptr,size))

#endif # GENERATIONAL_GC

#endif # SELFMADE_MMAP || GENERATIONAL_GC

# ------------------------------------------------------------------------------
#                       Garbage-Collector

# Gesamtstrategie:
# 1. Pseudorekursives Markieren durch Setzen von garcol_bit.
# 2. Verschieben der Objekte fester Länge (Conses u.ä.),
#    Durchrechnen der Verschiebungen der Objekte variabler Länge.
# 3. Aktualisieren der Pointer.
# 4. Durchführen der Verschiebungen der Objekte variabler Länge.

#ifdef GENERATIONAL_GC
  # Alte Generation mit Hilfe des Cache auf den aktuellen Stand bringen:
  local void prepare_old_generation (void);
  local void prepare_old_generation()
    { var uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++)
        if (is_heap_containing_objects(heapnr))
          { var Heap* heap = &mem.heaps[heapnr];
            var aint gen0_start = heap->heap_gen0_start;
            var aint gen0_end = heap->heap_gen0_end;
            gen0_start = gen0_start & -physpagesize;
            gen0_end = (gen0_end + (physpagesize-1)) & -physpagesize;
            if (gen0_start < gen0_end)
              { if (!(heap->physpages==NULL))
                  { # Erst read-write einblenden:
                    xmmprotect(heap, gen0_start, gen0_end-gen0_start, PROT_READ_WRITE);
                    # Dann den Cache entleeren:
                    {var physpage_state* physpage = heap->physpages;
                     var uintL physpagecount;
                     dotimespL(physpagecount, (gen0_end-gen0_start) >> physpageshift,
                       { if (physpage->protection == PROT_NONE)
                           { var uintL count = physpage->cache_size;
                             if (count > 0)
                               { var old_new_pointer* ptr = physpage->cache;
                                 dotimespL(count,count, { *(ptr->p) = ptr->o; ptr++; } );
                           }   }
                         physpage->protection = PROT_READ_WRITE;
                         xfree(physpage->cache); physpage->cache = NULL;
                         physpage++;
                       });
                     /* xfree(heap->physpages); heap->physpages = NULL; */
                  } }
                # Dann die Lücke zwischen der alten und der neuen Generation so
                # füllen, daß die Kompaktierungs-Algorithmen funktionieren:
                if (is_cons_heap(heapnr))
                  { var object* ptr;
                    var uintL count;
                    #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
                    ptr = (object*) heap->heap_gen1_end;
                    count = (heap->heap_gen0_start - heap->heap_gen1_end)/sizeof(object);
                    dotimesL(count,count, { *ptr++ = nullobj; } );
                    #else # SPVW_PURE_BLOCKS || SPVW_MIXED_BLOCKS_STAGGERED
                    ptr = (object*) heap->heap_gen0_end;
                    count = (heap->heap_gen1_start - heap->heap_gen0_end)/sizeof(object);
                    #ifndef SELFMADE_MMAP
                    dotimesL(count,count, { *ptr++ = nullobj; } );
                    #else
                    if (count > 0)
                      { var aint gen0_end = heap->heap_gen0_end;
                        heap->heap_gen0_end = heap->heap_gen1_start; # temporary - for handle_fault() if SELFMADE_MMAP
                        dotimespL(count,count, { *ptr++ = nullobj; } );
                        heap->heap_gen0_end = gen0_end; # temporary - end
                      }
                    #endif
                    #endif
                  }
              }
    }     }
#endif

# Test, ob ein Objekt obj in der gerade ignorierten Generation liegt.
# in_old_generation(obj,type,heapnr)
# > obj: Objekt mit !gcinvariant_type_p(type = typecode(obj))
# > heapnr: 0 bei Objekt variabler Länge, 1 bei Cons o.ä.
# < TRUE falls man eine "kleine" Generational GC durchführt und
#   obj in der alten Generation liegt.
# Vorsicht bei Symbolen: Ist obj eines der konstanten Symbole, so ist das
# Ergebnis nicht spezifiziert!
#ifdef GENERATIONAL_GC
  #ifdef SPVW_PURE_BLOCKS
    #define in_old_generation(obj,type,heapnr)  \
      (canonaddr(obj) < mem.heaps[type].heap_start)
  #else # SPVW_MIXED_BLOCKS
    #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
      #define in_old_generation_0(obj)  \
        (canonaddr(obj) < mem.varobjects.heap_start)
      #define in_old_generation_1(obj)  \
        (canonaddr(obj) >= mem.conses.heap_end)
      #define in_old_generation_general(obj)  \
        (in_old_generation_0(obj) || in_old_generation_1(obj))
      #ifdef GNU
        # meist ist heapnr konstant, das erlaubt Optimierung:
        #define in_old_generation(obj,type,heapnr)  \
          (__builtin_constant_p(heapnr)                                          \
           ? ((heapnr)==0 ? in_old_generation_0(obj) : in_old_generation_1(obj)) \
           : in_old_generation_general(obj)                                      \
          )
      #else
        #define in_old_generation(obj,type,heapnr)  \
          in_old_generation_general(obj)
      #endif
    #else # SPVW_MIXED_BLOCKS_STAGGERED
      #define in_old_generation(obj,type,heapnr)  \
        (canonaddr(obj) < mem.heaps[heapnr].heap_start)
    #endif
  #endif
#else
  #define in_old_generation(obj,type,heapnr)  FALSE
#endif

# Markierungs-Unterprogramm
  # Verfahren: Markierungsroutine ohne Stackbenutzung (d.h.
  #  nicht "rekursiv") durch Abstieg in die zu markierende
  #  Struktur mit Pointermodifikation (Pointer werden umgedreht,
  #  damit sie als "Ariadnefaden" zurück dienen können)
  # Konvention: ein Objekt X gilt als markiert, wenn
  #  - ein Objekt variabler Länge: Bit garcol_bit,(X) gesetzt
  #  - ein Zwei-Pointer-Objekt: Bit garcol_bit,(X) gesetzt
  #  - ein SUBR/FSUBR: Bit garcol_bit,(X+const_offset) gesetzt
  #  - Character, Short-Float, Fixnum etc.: stets.
  local void gc_mark (object obj);
  local void gc_mark(obj)
    var object obj;
    { var object dies = obj; # aktuelles Objekt
      var object vorg = nullobj; # Vorgänger-Objekt

      #define down_pair()  \
        if (in_old_generation(dies,typecode(dies),1))           \
          goto up; # ältere Generation nicht markieren          \
        { var oint* dies_ = (oint*)ThePointer(dies);            \
          if (marked(dies_)) goto up; # markiert -> hoch        \
          mark(dies_); # markieren                              \
        }                                                       \
        { var object dies_ = objectplus(dies,(soint)(sizeof(cons_)-sizeof(object))<<(oint_addr_shift-addr_shift)); \
                           # mit dem letzten Pointer anfangen   \
          var object nachf = *(object*)ThePointer(dies_); # Nachfolger \
          *(object*)ThePointer(dies_) = vorg; # Vorgänger eintragen \
          vorg = dies_; # aktuelles Objekt wird neuer Vorgänger \
          dies = nachf; # Nachfolger wird aktuelles Objekt      \
          goto down; # und absteigen                            \
        }
      #define up_pair()  \
        { mark(ThePointer(vorg)); # wieder markieren            \
          dies = vorg; # Cons wird aktuelles Objekt             \
          vorg = vorvorg; goto up; # weiter aufsteigen          \
        }
      #define down_varobject(The,first_offset,last_offset)  \
        if (in_old_generation(dies,typecode(dies),0))           \
          goto up; # ältere Generation nicht markieren          \
        { var oint* dies_ = (oint*)The(dies);                   \
          if (marked(dies_)) goto up; # markiert -> hoch        \
          mark(dies_); # markieren                              \
          mark(pointerplus(dies_,first_offset)); # ersten Pointer markieren \
        }                                                       \
        { var object dies_ = objectplus(dies,(soint)(last_offset)<<(oint_addr_shift-addr_shift)); \
                           # mit dem letzten Pointer anfangen   \
          var object nachf = *(object*)The(dies_); # Nachfolger \
          *(object*)The(dies_) = vorg; # Vorgänger eintragen    \
          vorg = dies_; # aktuelles Objekt wird neuer Vorgänger \
          dies = nachf; # Nachfolger wird aktuelles Objekt      \
          goto down; # und absteigen                            \
        }
      #define up_varobject(first_offset)  \
        { dies = objectplus(vorg,-(soint)(first_offset)<<(oint_addr_shift-addr_shift)); # wird aktuelles Objekt \
          vorg = vorvorg; goto up; # weiter aufsteigen          \
        }
      #define down_nopointers(The)  \
        if (in_old_generation(dies,typecode(dies),0))           \
          goto up; # ältere Generation nicht markieren          \
        mark(The(dies)); # markieren                            \
        goto up; # und hoch
      #define down_iarray()  \
        if (in_old_generation(dies,typecode(dies),0))           \
          goto up; # ältere Generation nicht markieren          \
        { var oint* dies_ = (oint*)TheIarray(dies);             \
          if (marked(dies_)) goto up; # markiert -> hoch        \
          mark(dies_); # markieren                              \
        }                                                       \
        { var object dies_ = objectplus(dies,(soint)(iarray_data_offset)<<(oint_addr_shift-addr_shift)); \
                           # Datenvektor ist der erste und einzige Pointer \
          var object nachf = *(object*)TheIarray(dies_); # Nachfolger \
          *(object*)TheIarray(dies_) = vorg; # Vorgänger eintragen \
          mark(TheIarray(dies_)); # ersten und einzigen Pointer markieren \
          vorg = dies_; # aktuelles Objekt wird neuer Vorgänger \
          dies = nachf; # Nachfolger wird aktuelles Objekt      \
          goto down; # und absteigen                            \
        }
      #define up_iarray()  \
        { dies = objectplus(vorg,-(soint)iarray_data_offset<<(oint_addr_shift-addr_shift)); # Array wird aktuelles Objekt \
          vorg = vorvorg; goto up; # weiter aufsteigen          \
        }
      #define down_svector()  \
        if (in_old_generation(dies,typecode(dies),0))           \
          goto up; # ältere Generation nicht markieren          \
        { var oint* dies_ = (oint*)TheSvector(dies);            \
          if (marked(dies_)) goto up; # markiert -> hoch        \
          mark(dies_); # markieren                              \
        }                                                       \
        { var uintL len = Svector_length(dies);                 \
          if (len==0) goto up; # Länge 0: wieder hoch           \
         {var object dies_ = objectplus(dies,                   \
                               ((soint)offsetofa(svector_,data) << (oint_addr_shift-addr_shift)) \
                               # the "<< 1" and "/2" are a workaround against a gcc-2.7.2 missed optimization in WIDE_SOFT mode \
                               + (((soint)len << 1) * (soint)(sizeof(object)/2) << (oint_addr_shift-addr_shift)) \
                               - ((soint)sizeof(object) << (oint_addr_shift-addr_shift)) ); \
                               # mit dem letzten Pointer anfangen \
          var object nachf = *(object*)TheSvector(dies_); # Nachfolger \
          *(object*)TheSvector(dies_) = vorg; # Vorgänger eintragen \
          mark(&TheSvector(dies)->data[0]); # ersten Pointer markieren \
          vorg = dies_; # aktuelles Objekt wird neuer Vorgänger \
          dies = nachf; # Nachfolger wird aktuelles Objekt      \
          goto down; # und absteigen                            \
        }}
      #define up_svector()  \
        { dies = objectplus(vorg,-(soint)offsetofa(svector_,data)<<(oint_addr_shift-addr_shift)); # Svector wird aktuelles Objekt \
          vorg = vorvorg; goto up; # weiter aufsteigen          \
        }
      #define down_record()  \
        if (in_old_generation(dies,typecode(dies),0))           \
          goto up; # ältere Generation nicht markieren          \
        { var oint* dies_ = (oint*)TheRecord(dies);             \
          if (marked(dies_)) goto up; # markiert -> hoch        \
          mark(dies_); # markieren                              \
        }                                                       \
        { var uintL len = Record_length(dies);                  \
          if (len==0) goto up; # Länge 0: wieder hoch           \
         {var object dies_ = objectplus(dies,                   \
                               ((soint)offsetofa(record_,recdata) << (oint_addr_shift-addr_shift)) \
                               # the "<< 1" and "/2" are a workaround against a gcc-2.7.2 missed optimization in WIDE_SOFT mode \
                               + (((soint)len << 1) * (soint)(sizeof(object)/2) << (oint_addr_shift-addr_shift)) \
                               - ((soint)sizeof(object) << (oint_addr_shift-addr_shift)) ); \
                               # mit dem letzten Pointer anfangen \
          var object nachf = *(object*)TheRecord(dies_); # Nachfolger \
          *(object*)TheRecord(dies_) = vorg; # Vorgänger eintragen \
          mark(&TheRecord(dies)->recdata[0]); # ersten Pointer markieren \
          vorg = dies_; # aktuelles Objekt wird neuer Vorgänger \
          dies = nachf; # Nachfolger wird aktuelles Objekt      \
          goto down; # und absteigen                            \
        }}
      #define up_record()  \
        { dies = objectplus(vorg,-(soint)offsetofa(record_,recdata)<<(oint_addr_shift-addr_shift)); # Record wird aktuelles Objekt \
          vorg = vorvorg; goto up; # weiter aufsteigen          \
        }
      #define down_subr()  \
        { var oint* dies_ = (oint*)pointerplus(TheSubr(dies),subr_const_offset); \
          if (marked(dies_)) goto up; # markiert -> hoch        \
          # markieren später                                    \
        }                                                       \
        { var object dies_ = objectplus(dies,                   \
                               (soint)(subr_const_offset+(subr_const_anz-1)*sizeof(object))<<(oint_addr_shift-addr_shift)); \
                               # mit dem letzten Pointer anfangen \
          var object nachf = *(object*)TheSubr(dies_); # Nachfolger \
          *(object*)TheSubr(dies_) = vorg; # Vorgänger eintragen \
          # ersten Pointer (und damit das SUBR selbst) markieren: \
          mark(pointerplus(TheSubr(dies),subr_const_offset));   \
          vorg = dies_; # aktuelles Objekt wird neuer Vorgänger \
          dies = nachf; # Nachfolger wird aktuelles Objekt      \
          goto down; # und absteigen                            \
        }
      #define up_subr()  \
        { mark(TheSubr(vorg)); # wieder markieren               \
          dies = objectplus(vorg,-(soint)subr_const_offset<<(oint_addr_shift-addr_shift)); # SUBR wird aktuelles Objekt \
          vorg = vorvorg; goto up; # weiter aufsteigen          \
        }

      down: # Einsprung für Abstieg.
            # dies = zu markierendes Objekt, vorg = sein Vorgänger
            #ifdef TYPECODES
            switch (typecode(dies))
              { case_pair:
                  # Objekt mit genau 2 Pointern (Cons u.ä.)
                  down_pair();
                case_symbol: # Symbol
                  down_varobject(TheSymbol,symbol_objects_offset,sizeof(symbol_)-sizeof(object));
                case_sbvector: # simple-bit-vector
                case_sstring: # simple-string
                case_bignum: # Bignum
                #ifndef WIDE
                case_ffloat: # Single-Float
                #endif
                case_dfloat: # Double-Float
                case_lfloat: # Long-Float
                  # Objekte variabler Länge, die keine Pointer enthalten:
                  down_nopointers(TheVarobject);
                case_mdarray: case_obvector: case_ostring: case_ovector:
                  # Arrays, die nicht simple sind:
                  down_iarray();
                case_svector: # simple-vector
                  down_svector();
                case_record: # Srecord/Xrecord
                  down_record();
                case_machine: # Maschinenadresse
                case_char: # Character
                case_system: # Frame-Pointer, Read-Label, System
                case_fixnum: # Fixnum
                case_sfloat: # Short-Float
                #ifdef WIDE
                case_ffloat: # Single-Float
                #endif
                  # Das sind direkte Objekte, keine Pointer.
                  goto up;
                case_subr: # SUBR
                  down_subr();
                default:
                  # Das sind keine Objekte.
                  /*NOTREACHED*/ abort();
              }
            #else
            switch (as_oint(dies) & nonimmediate_bias_mask)
              { case cons_bias: # Cons
                  # NB: (immediate_bias & nonimmediate_bias_mask) == cons_bias.
                  if (immediate_object_p(dies)) goto up;
                  down_pair();
                case varobject_bias:
                  switch (Record_type(dies))
                    { case Rectype_Sbvector:
                      case Rectype_Sstring:
                      case Rectype_Bignum:
                      case Rectype_Ffloat:
                      case Rectype_Dfloat:
                      case Rectype_Lfloat:
                        down_nopointers(TheRecord);
                      case Rectype_Svector:
                        down_svector();
                      case Rectype_mdarray:
                      case Rectype_bvector:
                      case Rectype_string:
                      case Rectype_vector:
                        down_iarray();
                      default: # Srecord/Xrecord
                        down_record();
                    }
                case subr_bias: # SUBR
                  down_subr();
                case machine_bias:
                  # Das sind direkte Objekte, keine Pointer.
                  goto up;
                default:
                  /*NOTREACHED*/ abort();
              }
            #endif
      up:   # Einsprung zum Aufstieg.
            # dies = gerade markiertes Objekt, vorg = sein Vorgänger
            if (eq(vorg,nullobj)) # Endekennzeichen erreicht?
              return; # ja -> fertig
            if (!marked(ThePointer(vorg))) # schon durch?
              # nein ->
              # nächstes Element weiter links (Komme von up, gehe nach down)
              # dies = gerade markiertes Objekt, in *vorg einzutragen
              { var object vorvorg = *(object*)ThePointer(vorg); # alter Vorgänger
                *(object*)ThePointer(vorg) = dies; # Komponente zurückschreiben
                vorg = objectplus(vorg,-(soint)(sizeof(object))<<(oint_addr_shift-addr_shift)); # zur nächsten Komponente
                if (marked(ThePointer(vorg))) # dort schon markiert?
                  { dies = # nächste Komponente, ohne Markierung
                           without_mark_bit(*(object*)ThePointer(vorg));
                    *(object*)ThePointer(vorg) = # alten Vorgänger weiterschieben, dabei Markierung erneuern
                           with_mark_bit(vorvorg);
                  }
                  else
                  { dies = *(object*)ThePointer(vorg); # nächste Komponente, ohne Markierung
                    *(object*)ThePointer(vorg) = vorvorg; # alten Vorgänger weiterschieben
                  }
                goto down;
              }
            # schon durch -> wieder aufsteigen
            { var object vorvorg = # alten Vorgänger holen, ohne Markierungsbit
                                   without_mark_bit(*(object*)ThePointer(vorg));
              *(object*)ThePointer(vorg) = dies; # erste Komponente zurückschreiben
              #ifdef TYPECODES
              switch (typecode(vorg))
                { case_pair:
                    # Objekt mit genau 2 Pointern (Cons u.ä.)
                    up_pair();
                  case_symbol: # Symbol
                    up_varobject(symbol_objects_offset);
                  case_svector:
                    # simple-vector mit mindestens 1 Komponente
                    up_svector();
                  case_mdarray: case_obvector: case_ostring: case_ovector:
                    # Nicht-simple Arrays:
                    up_iarray();
                  case_record: # Srecord/Xrecord
                    up_record();
                  case_subr: # SUBR
                    up_subr();
                  case_machine: # Maschinenadresse
                  case_char: # Character
                  case_system: # Frame-Pointer, Read-Label, System
                  case_fixnum: # Fixnum
                  case_sfloat: # Short-Float
                  #ifdef WIDE
                  case_ffloat: # Single-Float
                  #endif
                    # Das sind direkte Objekte, keine Pointer.
                  case_sbvector: # simple-bit-vector
                  case_sstring: # simple-string
                  case_bignum: # Bignum
                  #ifndef WIDE
                  case_ffloat: # Single-Float
                  #endif
                  case_dfloat: # Double-Float
                  case_lfloat: # Long-Float
                    # Objekte variabler Länge, die keine Pointer enthalten.
                  default:
                    # Das sind keine Objekte.
                    /*NOTREACHED*/ abort();
                }
              #else
              switch (as_oint(vorg) & nonimmediate_bias_mask)
                { case cons_bias: # Cons
                    up_pair();
                  case subr_bias: # SUBR
                    up_subr();
                  case varobject_bias:
                    # This works only because all varobjects have the same
                    # objects_offset!
                    up_record();
                  default:
                    # Das sind keine Objekte.
                    /*NOTREACHED*/ abort();
                }
              #endif
            }
      #undef up_subr
      #undef down_subr
      #undef up_record
      #undef down_record
      #undef up_svector
      #undef down_svector
      #undef up_iarray
      #undef down_iarray
      #undef down_nopointers
      #undef up_varobject
      #undef down_varobject
      #undef up_pair
      #undef down_pair
    }

# Verpackt einen Pointer in ein object, ohne Typinfo.
# pointer_as_object(ptr): void* --> object
# pointer_was_object(obj): object --> void*
  #ifdef TYPECODES
    #define pointer_as_object(ptr)  type_pointer_object(0,ptr)
    #define pointer_was_object(obj)  type_pointable(0,obj)
  #else
    #define pointer_as_object(ptr)  as_object((oint)(ptr))
    #define pointer_was_object(obj)  ((void*)as_oint(obj))
  #endif

#ifdef GENERATIONAL_GC

# Nummer der Generation, die bereinigt wird.
# 0 : alles (Generation 0 + Generation 1)
# 1 : nur Generation 1
local uintC generation;

# Sparsames Durchlaufen durch alle Pointer einer physikalischen Seite:
# walk_physpage(heapnr,physpage,pageend,heapend,walkfun);
# Hierfür ist wesentlich, daß varobject_alignment ein Vielfaches
# von sizeof(object) ist.
  #define walk_physpage(heapnr,physpage,pageend,heapend,walkfun)  \
    { { var uintC count = physpage->continued_count;                  \
        if (count > 0)                                                \
          { var object* ptr = physpage->continued_addr;               \
            dotimespC(count,count, { walkfun(*ptr); ptr++; } );       \
      }   }                                                           \
      { var aint physpage_end =                                       \
          (pageend < heapend ? pageend : heapend);                    \
        walk_area(heapnr,physpage->firstobject,physpage_end,walkfun); \
    } }
  # Unterroutinen:
  #define walk_area_cons(objptr,physpage_end,walkfun)  \
    { var object* ptr = (object*)objptr;                      \
      while ((aint)ptr < physpage_end)                        \
        { walkfun(*ptr); ptr++; }                             \
    }
  #define walk_area_symbol(objptr,physpage_end,walkfun)  \
      { var object* ptr = (object*)(objptr+symbol_objects_offset); \
        var uintC count;                                      \
        dotimespC(count,(sizeof(symbol_)-symbol_objects_offset)/sizeof(object), \
          { if ((aint)ptr < physpage_end)                     \
              { walkfun(*ptr); ptr++; }                       \
              else break;                                     \
          });                                                 \
        objptr += size_symbol();                              \
      }
  #define walk_area_iarray(objptr,physpage_end,walkfun)  \
      { var object* ptr = &((Iarray)objptr)->data;            \
        if ((aint)ptr < physpage_end)                         \
          { walkfun(*ptr); }                                  \
        objptr += objsize_iarray((Iarray)objptr);             \
      }
  #define walk_area_svector(objptr,physpage_end,walkfun)  \
      { var uintL count = svector_length((Svector)objptr);    \
        var object* ptr = &((Svector)objptr)->data[0];        \
        objptr += size_svector(count);                        \
        dotimesL(count,count,                                 \
          { if ((aint)ptr < physpage_end)                     \
              { walkfun(*ptr); ptr++; }                       \
              else break;                                     \
          });                                                 \
      }
  #define walk_area_record(objptr,physpage_end,walkfun)  \
      { var uintC count;                                      \
        var object* ptr = &((Record)objptr)->recdata[0];      \
        objptr += (record_type((Record)objptr) < rectype_limit \
                   ? (count = srecord_length((Srecord)objptr), size_srecord(count)) \
                   : (count = xrecord_length((Xrecord)objptr), size_xrecord(count,xrecord_xlength((Xrecord)objptr))) \
                  );                                          \
        dotimesC(count,count,                                 \
          { if ((aint)ptr < physpage_end)                     \
              { walkfun(*ptr); ptr++; }                       \
              else break;                                     \
          });                                                 \
      }
  #ifdef SPVW_PURE
    #define walk_area(heapnr,physpage_start,physpage_end,walkfun)  \
      { var aint objptr = physpage_start;                            \
        switch (heapnr)                                              \
          { case_pair:                                               \
              # Objekt mit genau 2 Pointern (Cons u.ä.)              \
              walk_area_cons(objptr,physpage_end,walkfun);           \
              break;                                                 \
            case_symbol: # Symbol                                    \
              while (objptr < physpage_end)                          \
                walk_area_symbol(objptr,physpage_end,walkfun);       \
              break;                                                 \
            case_mdarray: case_obvector: case_ostring: case_ovector: \
              # Arrays, die nicht simple sind:                       \
              while (objptr < physpage_end)                          \
                walk_area_iarray(objptr,physpage_end,walkfun);       \
              break;                                                 \
            case_svector: # simple-vector                            \
              while (objptr < physpage_end)                          \
                walk_area_svector(objptr,physpage_end,walkfun);      \
              break;                                                 \
            case_record: # Record                                    \
              while (objptr < physpage_end)                          \
                walk_area_record(objptr,physpage_end,walkfun);       \
              break;                                                 \
            default:                                                 \
              # Solche Objekte kommen nicht vor.                     \
              /*NOTREACHED*/ abort();                                \
      }   }
  #endif
  #ifdef SPVW_MIXED
    #ifdef TYPECODES
      #define walk_area(heapnr,physpage_start,physpage_end,walkfun)  \
        { var aint objptr = physpage_start;                                        \
          switch (heapnr)                                                          \
            { case 0: # Objekte variabler Länge                                    \
                while (objptr < physpage_end)                                      \
                  { switch (typecode_at(objptr)) # Typ des nächsten Objekts        \
                      { case_symbolwithflags: # Symbol                             \
                          walk_area_symbol(objptr,physpage_end,walkfun);           \
                          break;                                                   \
                        case_mdarray: case_obvector: case_ostring: case_ovector:   \
                          # Arrays, die nicht simple sind:                         \
                          walk_area_iarray(objptr,physpage_end,walkfun);           \
                          break;                                                   \
                        case_svector: # simple-vector                              \
                          walk_area_svector(objptr,physpage_end,walkfun);          \
                          break;                                                   \
                        case_record: # Record                                      \
                          walk_area_record(objptr,physpage_end,walkfun);           \
                          break;                                                   \
                        default: # simple-bit-vector, simple-string, bignum, float \
                          objptr += objsize((Varobject)objptr);                    \
                          break;                                                   \
                  }   }                                                            \
                break;                                                             \
              case 1: # 2-Pointer-Objekte                                          \
                walk_area_cons(objptr,physpage_end,walkfun);                       \
                break;                                                             \
              default: /*NOTREACHED*/ abort();                                     \
        }   }
    #else
      #define walk_area(heapnr,physpage_start,physpage_end,walkfun)  \
        { var aint objptr = physpage_start;                                         \
          switch (heapnr)                                                           \
            { case 0: # Objekte variabler Länge                                     \
                while (objptr < physpage_end)                                       \
                  { switch (record_type((Record)objptr)) # Typ des nächsten Objekts \
                      { case Rectype_mdarray:                                       \
                        case Rectype_bvector:                                       \
                        case Rectype_string:                                        \
                        case Rectype_vector:                                        \
                          # Arrays, die nicht simple sind:                          \
                          walk_area_iarray(objptr,physpage_end,walkfun);            \
                          break;                                                    \
                        case Rectype_Svector: # simple-vector                       \
                          walk_area_svector(objptr,physpage_end,walkfun);           \
                          break;                                                    \
                        case Rectype_Sbvector:                                      \
                        case Rectype_Sstring:                                       \
                        case Rectype_Bignum:                                        \
                        case Rectype_Ffloat:                                        \
                        case Rectype_Dfloat:                                        \
                        case Rectype_Lfloat:                                        \
                          # simple-bit-vector, simple-string, bignum, float         \
                          objptr += objsize((Varobject)objptr);                     \
                          break;                                                    \
                        default: # Srecord/Xrecord                                  \
                          walk_area_record(objptr,physpage_end,walkfun);            \
                          break;                                                    \
                  }   }                                                             \
                break;                                                              \
              case 1: # 2-Pointer-Objekte                                           \
                walk_area_cons(objptr,physpage_end,walkfun);                        \
                break;                                                              \
              default: /*NOTREACHED*/ abort();                                      \
        }   }
    #endif
  #endif
# Dasselbe als Funktion:
# walk_physpage_(heapnr,physpage,pageend,heapend,walkstep);
# bzw. walk_area_(heapnr,physpage_start,physpage_end,walkstep);
  typedef void (*walkstep_fun)(object* ptr);
  local void walk_physpage_ (uintL heapnr, physpage_state* physpage, aint pageend, aint heapend, walkstep_fun walkstep);
  local void walk_physpage_(heapnr,physpage,pageend,heapend,walkstep)
    var uintL heapnr;
    var physpage_state* physpage;
    var aint pageend;
    var aint heapend;
    var walkstep_fun walkstep;
    {
      #define walkstep1(obj)  walkstep(&(obj))
      walk_physpage(heapnr,physpage,pageend,heapend,walkstep1);
      #undef walkstep1
    }
  local void walk_area_ (uintL heapnr, aint physpage_start, aint physpage_end, walkstep_fun walkstep);
  local void walk_area_(heapnr,physpage_start,physpage_end,walkstep)
    var uintL heapnr;
    var aint physpage_start;
    var aint physpage_end;
    var walkstep_fun walkstep;
    {
      #define walkstep1(obj)  walkstep(&(obj))
      walk_area(heapnr,physpage_start,physpage_end,walkstep1);
      #undef walkstep1
    }

  local void gc_mark_at (object* ptr);
  local void gc_mark_at(ptr)
    var object* ptr;
    { gc_mark(*ptr); }

#endif

# Markierungsphase:
  # Es werden alle "aktiven" Strukturen markiert.
  # Aktiv ist alles, was erreichbar ist
  # - vom LISP-Stack aus  oder
  # - bei Generational-GC: von der alten Generation aus  oder
  # - als Programmkonstanten (dazu gehört auch die Liste aller Packages).
  local void gc_markphase (void);
  local void gc_markphase()
    { { var object* objptr = &STACK_0; # Pointer, der durch den STACK läuft
        until (eq(*objptr,nullobj)) # bis STACK zu Ende ist:
          { if ( *((oint*)objptr) & wbit(frame_bit_o) ) # Beginnt hier ein Frame?
             { if (( *((oint*)objptr) & wbit(skip2_bit_o) ) == 0) # Ohne skip2-Bit?
                objptr skipSTACKop 2; # ja -> um 2 weiterrücken
                else
                objptr skipSTACKop 1; # nein -> um 1 weiterrücken
             }
             else
             { # normales Objekt, markieren:
               var object obj = *objptr;
               #ifndef NO_symbolflags
               switch (typecode(obj)) # evtl. Symbol-Flags entfernen
                 { case_symbolflagged:
                     obj = symbol_without_flags(obj);
                   default: break;
                 }
               #endif
               gc_mark(obj);
               objptr skipSTACKop 1; # weiterrücken
      }   }  }
      #ifdef GENERATIONAL_GC
      # Alte Generation markieren, wobei man sie sehr sparsam durchläuft:
      if (generation > 0)
        { var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            if (is_heap_containing_objects(heapnr)) # Objekte, die keine Pointer enthalten,
                                                    # braucht man nicht zu durchlaufen.
              { var Heap* heap = &mem.heaps[heapnr];
                var aint gen0_start = heap->heap_gen0_start;
                var aint gen0_end = heap->heap_gen0_end;
                if (gen0_start < gen0_end)
                  if (heap->physpages==NULL)
                    { walk_area_(heapnr,gen0_start,gen0_end,gc_mark_at); } # fallback
                    else
                    { var physpage_state* physpage = heap->physpages;
                      gen0_start &= -physpagesize;
                      do { gen0_start += physpagesize;
                           if ((physpage->protection == PROT_NONE)
                               || (physpage->protection == PROT_READ)
                              )
                             # Cache ausnutzen, gecachte Pointer markieren:
                             { var uintL count = physpage->cache_size;
                               if (count > 0)
                                 { var old_new_pointer* ptr = physpage->cache;
                                   dotimespL(count,count, { gc_mark(ptr->o); ptr++; } );
                             }   }
                             else
                             # ganzen Page-Inhalt markieren:
                             { walk_physpage_(heapnr,physpage,gen0_start,gen0_end,gc_mark_at); }
                           physpage++;
                         }
                         while (gen0_start < gen0_end);
        }     }     }
      #endif
      # Alle Programmkonstanten markieren:
      for_all_subrs( gc_mark(subr_tab_ptr_as_object(ptr)); ); # subr_tab durchgehen
      #if !defined(GENERATIONAL_GC)
      for_all_constsyms( gc_mark(symbol_tab_ptr_as_object(ptr)); ); # symbol_tab durchgehen
      #else
      # gc_mark() betrachtet wegen des Macros in_old_generation() alle konstanten
      # Symbole als zur alten Generation zugehörig und durchläuft sie nicht.
      for_all_constsyms( # symbol_tab durchgehen
        { gc_mark(ptr->symvalue);
          gc_mark(ptr->symfunction);
          gc_mark(ptr->proplist);
          gc_mark(ptr->pname);
          gc_mark(ptr->homepackage);
        });
      #endif
      for_all_constobjs( gc_mark(*objptr); ); # object_tab durchgehen
      for_all_threadobjs( gc_mark(*objptr); ); # Threads durchgehen
    }

# UP: Stellt fest, ob ein Objekt noch "lebt".
# D.h. ob nach der Markierungsphase das Markierungsbit gesetzt ist.
  local boolean alive (object obj);
  local boolean alive(obj)
    var object obj;
    {
      #ifdef TYPECODES
      switch (typecode(obj)) # je nach Typ
        { case_pair: # Cons
            if (in_old_generation(obj,typecode(obj),1)) return TRUE;
            if (marked(ThePointer(obj))) return TRUE; else return FALSE;
          case_symbol: # Symbol
          case_array: # Array
          case_bignum: # Bignum
          #ifndef WIDE
          case_ffloat: # Single-Float
          #endif
          case_dfloat: # Double-Float
          case_lfloat: # Long-Float
          case_record: # Record
            if (in_old_generation(obj,typecode(obj),0)) return TRUE;
            if (marked(ThePointer(obj))) return TRUE; else return FALSE;
          case_subr: # Subr
            if (marked((oint*)pointerplus(TheSubr(obj),subr_const_offset)))
              return TRUE; else return FALSE;
          case_machine: # Maschinenpointer
          case_char: # Character
          case_system: # Frame-pointer, Read-label, system
          case_fixnum: # Fixnum
          case_sfloat: # Short-Float
          #ifdef WIDE
          case_ffloat: # Single-Float
          #endif
            return TRUE;
          default:
            # Das sind keine Objekte.
            /*NOTREACHED*/ abort();
        }
      #else
      switch (as_oint(obj) & nonimmediate_bias_mask)
        { case varobject_bias:
            if (in_old_generation(obj,,0)) return TRUE;
            if (marked(ThePointer(obj))) return TRUE; else return FALSE;
          case cons_bias:
            if (in_old_generation(obj,,1)) return TRUE;
            if (marked(ThePointer(obj))) return TRUE; else return FALSE;
          case subr_bias:
            if (marked((oint*)pointerplus(TheSubr(obj),subr_const_offset)))
              return TRUE; else return FALSE;
          default:
            return TRUE;
        }
      #endif
    }

# SUBRs und feste Symbole demarkieren:
  local void unmark_fixed_varobjects (void);
  local void unmark_fixed_varobjects()
    { for_all_subrs( unmark((aint)ptr+subr_const_offset); ); # jedes Subr demarkieren
      #if !defined(GENERATIONAL_GC)
      for_all_constsyms( unmark(&((Symbol)ptr)->GCself); ); # jedes Symbol in symbol_tab demarkieren
      #else
      # Da wir die konstanten Symbole nicht markiert haben, sondern nur ihren
      # Inhalt, brauchen wir sie auch nicht zu demarkieren.
      #endif
    }

#if !defined(MORRIS_GC)

 #ifdef SPVW_MIXED_BLOCKS_OPPOSITE

  # CONS-Zellen zwischen page->page_start und page->page_end oben
  # konzentrieren:
  local void gc_compact_cons_page (Page* page);
  local void gc_compact_cons_page(page)
    var Page* page;
    # Dabei wandert der Pointer p1 von unten und der Pointer p2 von
    # oben durch den Speicherbereich, bis sie kollidieren. Es
    # werden dabei markierte Strukturen über unmarkierte geschoben.
    { var aint p1 = page->page_start; # untere Grenze
      var aint p2 = page->page_end; # obere Grenze
      sweeploop:
        # Suche nächstobere unmarkierte Zelle <p2 und demarkiere dabei alle:
        sweeploop1:
          if (p1==p2) goto sweepok2; # Grenzen gleich geworden -> fertig
          p2 -= sizeof(cons_); # nächste Zelle von oben erfassen
          if (marked(p2)) # markiert?
            { unmark(p2); # demarkieren
              goto sweeploop1;
            }
        # p1 <= p2, p2 zeigt auf eine unmarkierte Zelle.
        # Suche nächstuntere markierte Zelle >=p1:
        sweeploop2:
          if (p1==p2) goto sweepok1; # Grenzen gleich geworden -> fertig
          if (!marked(p1)) # unmarkiert?
            { p1 += sizeof(cons_); # bei der nächstunteren Zelle
              goto sweeploop2; # weitersuchen
            }
        # p1 < p2, p1 zeigt auf eine markierte Zelle.
        unmark(p1); # demarkieren
        # Zelleninhalt in die unmarkierte Zelle kopieren:
        ((object*)p2)[0] = ((object*)p1)[0];
        ((object*)p2)[1] = ((object*)p1)[1];
        *(object*)p1 = pointer_as_object(p2); # neue Adresse hinterlassen
        mark(p1); # und markieren (als Erkennung fürs Aktualisieren)
        p1 += sizeof(cons_); # Diese Zelle ist fertig.
        goto sweeploop; # weiter
      sweepok1: p1 += sizeof(cons_); # letztes unmarkiertes Cons übergehen
      sweepok2:
      # p1 = neue untere Grenze des Cons-Bereiches
      page->page_start = p1;
    }

 #else

  # CONS-Zellen zwischen page->page_start und page->page_end unten
  # konzentrieren:
  local void gc_compact_cons_page (Page* page);
  local void gc_compact_cons_page(page)
    var Page* page;
    # Dabei wandert der Pointer p1 von unten und der Pointer p2 von
    # oben durch den Speicherbereich, bis sie kollidieren. Es
    # werden dabei markierte Strukturen über unmarkierte geschoben.
    { var aint p1 = page->page_start; # untere Grenze
      var aint p2 = page->page_end; # obere Grenze
      sweeploop:
        # Suche nächstobere markierte Zelle <p2:
        sweeploop1:
          if (p1==p2) goto sweepok2; # Grenzen gleich geworden -> fertig
          p2 -= sizeof(cons_); # nächste Zelle von oben erfassen
          if (!marked(p2)) goto sweeploop1; # unmarkiert?
        # p1 <= p2, p2 zeigt auf eine markierte Zelle.
        unmark(p2); # demarkieren
        # Suche nächstuntere unmarkierte Zelle >=p1 und demarkiere dabei alle:
        sweeploop2:
          if (p1==p2) goto sweepok1; # Grenzen gleich geworden -> fertig
          if (marked(p1)) # markiert?
            { unmark(p1); # demarkieren
              p1 += sizeof(cons_); # bei der nächstoberen Zelle
              goto sweeploop2; # weitersuchen
            }
        # p1 < p2, p1 zeigt auf eine unmarkierte Zelle.
        # Zelleninhalt von der markierten in die unmarkierte Zelle kopieren:
        ((object*)p1)[0] = ((object*)p2)[0];
        ((object*)p1)[1] = ((object*)p2)[1];
        *(object*)p2 = pointer_as_object(p1); # neue Adresse hinterlassen
        mark(p2); # und markieren (als Erkennung fürs Aktualisieren)
        p1 += sizeof(cons_); # Diese Zelle ist fertig.
        goto sweeploop; # weiter
      sweepok1: p1 += sizeof(cons_); # letztes markiertes Cons übergehen
      sweepok2:
      # p1 = neue obere Grenze des Cons-Bereiches
      page->page_end = p1;
    }

 #endif

#else # defined(MORRIS_GC)

# Algorithmus siehe:
# [F. Lockwood Morris: A time- and space-efficient garbage collection algorithm.
#  CACM 21,8 (August 1978), 662-665.]

  # Alle unmarkierten CONS-Zellen löschen und die markierten CONS-Zellen demarkieren,
  # damit das Markierungsbit für die Rückwärtspointer zur Verfügung steht.
  local void gc_morris1 (Page* page);
  local void gc_morris1(page)
    var Page* page;
    { var aint p1 = page->page_start; # untere Grenze
      var aint p2 = page->page_end; # obere Grenze
      var aint d = 0; # freien Speicher mitzählen
      until (p1==p2)
        { if (!marked(p1))
            { ((object*)p1)[0] = nullobj;
              ((object*)p1)[1] = nullobj;
              d += sizeof(cons_);
            }
            else
            { unmark(p1);
              #ifdef DEBUG_SPVW
              if (eq(((object*)p1)[0],nullobj) || eq(((object*)p1)[1],nullobj))
                abort();
              #endif
            }
          p1 += sizeof(cons_); # Diese Zelle ist fertig.
        }
      page->page_gcpriv.d = d; # freien Speicher abspeichern
    }

 #ifdef SPVW_MIXED_BLOCKS_OPPOSITE

  # Es gibt nur eine einzige Page mit Zwei-Pointer-Objekten.

  local void gc_morris2 (Page* page);
  local void gc_morris2(page)
    var Page* page;
    { # Jede Zelle innerhalb eines Cons enthält nun eine Liste aller
      # Adressen von Pointern auf diese Zelle, die aus einer Wurzel heraus
      # oder aus einem Varobject heraus auf diese Zelle zeigen.
      #
      # Die nicht gelöschten Conses von links nach rechts durchlaufen:
      # (Zwischendurch enthält jede Zelle eine Liste aller Adressen
      # von Pointern auf diese Zelle, die aus einer Wurzel heraus,
      # aus einem Varobject heraus oder aus einem weiter links liegenden
      # Cons auf diese Zelle zeigen.)
      var aint p1 = page->page_start; # untere Grenze
      var aint p2 = p1 + page->page_gcpriv.d; # spätere untere Grenze
      var aint p1limit = page->page_end; # obere Grenze
      until (p1==p1limit) # stets p1 <= p2 <= p1limit
        { # Beide Zellen eines Cons werden genau gleich behandelt.
          var object obj = *(object*)p1;
          if (!eq(obj,nullobj))
            { # p1 wird nach p2 verschoben.
              #ifdef TYPECODES
                # Die bisher registrierten Pointer auf diese Zelle werden aktualisiert:
                until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) # Liste abarbeiten
                  { obj = without_mark_bit(obj);
                   {var aint p = upointer(obj);
                    var object next_obj = *(object*)p;
                    *(object*)p = type_pointer_object(typecode(obj),p2);
                    obj = next_obj;
                  }}
                # Falls die Zelle einen Pointer "nach rechts" enthält, wird er umgedreht.
                { var tint type = typecode(obj);
                  switch (type)
                    { case_pair:
                        { var aint p = upointer(obj);
                          if (!in_old_generation(obj,type,1) && (p > p1))
                            { # Für spätere Aktualisierung
                              # p1 in die Liste der Pointer auf p einhängen:
                              *(object*)p1 = *(object*)p;
                              *(object*)p = with_mark_bit(type_pointer_object(type,p1));
                              break;
                        }   }
                      default:
                        *(object*)p1 = obj;
                }   }
              #else # no TYPECODES
                # Die bisher registrierten Pointer auf diese Zelle werden aktualisiert:
                until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) # Liste abarbeiten
                  { obj = without_mark_bit(obj);
                   {var aint p = (aint)ThePointer(obj);
                    var object next_obj = *(object*)p;
                    *(object*)p = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2);
                    obj = next_obj;
                  }}
                # Falls die Zelle einen Pointer "nach rechts" enthält, wird er umgedreht.
                if (consp(obj))
                  { var aint p = (aint)ThePointer(obj);
                    if (!in_old_generation(obj,,1) && (p > p1))
                      { # Für spätere Aktualisierung
                        # p1 in die Liste der Pointer auf p einhängen:
                        *(object*)p1 = *(object*)p;
                        *(object*)p = with_mark_bit(as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p1));
                      }
                      else
                      { *(object*)p1 = obj; }
                  }
                  else
                  { *(object*)p1 = obj; }
              #endif
              p2 += sizeof(object);
            }
          p1 += sizeof(object);
        }
      if (!(p2==p1limit)) abort();
    }
  local void gc_morris3 (Page* page);
  local void gc_morris3(page)
    var Page* page;
    { # Jede Zelle innerhalb eines Cons enthält nun wieder den ursprünglichen
      # Inhalt.
      #
      # Die nicht gelöschten Conses von rechts nach links durchlaufen
      # und dabei rechts kompaktieren:
      # (Zwischendurch enthält jede Zelle eine Liste aller Adressen
      # von Pointern auf diese Zelle, die aus einem weiter rechts liegenden
      # Cons auf diese Zelle zeigen.)
      var aint p1limit = page->page_start; # untere Grenze
      var aint p1 = page->page_end; # obere Grenze
      var aint p2 = p1; # obere Grenze
      #ifdef DEBUG_SPVW
      until (p1==p1limit)
        { p1 -= 2*sizeof(object);
          if (eq(*(object*)p1,nullobj)+eq(*(object*)(p1^sizeof(object)),nullobj)==1)
            abort();
        }
      p1 = page->page_end;
      #endif
      until (p1==p1limit) # stets p1limit <= p1 <= p2
        { # Beide Zellen eines Cons werden genau gleich behandelt.
          p1 -= sizeof(object);
          #ifdef DEBUG_SPVW
          if (eq(*(object*)p1,nullobj)+eq(*(object*)(p1^sizeof(object)),nullobj)==1)
            abort();
          if (!((p1 % (2*sizeof(object))) == 0))
            { if (!((p2 % (2*sizeof(object))) == 0))
                abort();
            }
          #endif
         {var object obj = *(object*)p1;
          if (!eq(obj,nullobj))
            { p2 -= sizeof(object);
              # p1 wird nach p2 verschoben.
              #ifdef TYPECODES
                # Die neu registrierten Pointer auf diese Zelle werden aktualisiert:
                until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) # Liste abarbeiten
                  { obj = without_mark_bit(obj);
                   {var aint p = upointer(obj);
                    var object next_obj = *(object*)p;
                    *(object*)p = type_pointer_object(typecode(obj),p2);
                    obj = next_obj;
                  }}
                #ifdef DEBUG_SPVW
                if (eq(obj,nullobj)) abort();
                #endif
                *(object*)p2 = obj;
                { var tint type = typecode(obj);
                  if (!gcinvariant_type_p(type)) # unverschieblich -> nichts tun
                    switch (type)
                      { case_pair: # Zwei-Pointer-Objekt
                          { var aint p = upointer(obj);
                            if (p < p1) # Pointer nach links?
                              { # Für spätere Aktualisierung
                                # p2 in die Liste der Pointer auf p einhängen:
                                #ifdef DEBUG_SPVW
                                if (eq(*(object*)p,nullobj)) abort();
                                #endif
                                *(object*)p2 = *(object*)p;
                                *(object*)p = with_mark_bit(type_pointer_object(type,p2));
                              }
                            elif (p == p1) # Pointer auf sich selbst?
                              { *(object*)p2 = type_pointer_object(type,p2); }
                          }
                          break;
                        default: # Objekt variabler Länge
                          if (marked(ThePointer(obj))) # markiert?
                            *(object*)p2 = type_untype_object(type,untype(*(object*)ThePointer(obj)));
                          break;
                }     }
              #else # no TYPECODES
                # Die neu registrierten Pointer auf diese Zelle werden aktualisiert:
                until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) # Liste abarbeiten
                  { obj = without_mark_bit(obj);
                   {var aint p = (aint)ThePointer(obj);
                    var object next_obj = *(object*)p;
                    *(object*)p = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2);
                    obj = next_obj;
                  }}
                #ifdef DEBUG_SPVW
                if (eq(obj,nullobj)) abort();
                #endif
                *(object*)p2 = obj;
                if (!gcinvariant_object_p(obj)) # unverschieblich -> nichts tun
                  { if (consp(obj))
                      # Zwei-Pointer-Objekt
                      { var aint p = (aint)ThePointer(obj);
                        if (p < p1) # Pointer nach links?
                          { # Für spätere Aktualisierung
                            # p2 in die Liste der Pointer auf p einhängen:
                            #ifdef DEBUG_SPVW
                            if (eq(*(object*)p,nullobj)) abort();
                            #endif
                            *(object*)p2 = *(object*)p;
                            *(object*)p = with_mark_bit(as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2));
                          }
                        elif (p == p1) # Pointer auf sich selbst?
                          { *(object*)p2 = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2); }
                      }
                      else
                        # Objekt variabler Länge
                        { if (marked(ThePointer(obj))) # markiert?
                            *(object*)p2 = as_object((as_oint(obj) & nonimmediate_bias_mask) | (*(oint*)ThePointer(obj) & ~wbit(garcol_bit_o) & ~(oint)nonimmediate_bias_mask));
                        }
                  }
              #endif
            }
        }}
      # p2 = neue untere Grenze des Cons-Bereiches
      if (!(p2 == page->page_start + page->page_gcpriv.d)) abort();
      page->page_start = p2;
    }

 #elif defined(SPVW_MIXED_BLOCKS_STAGGERED)

  local void gc_morris2 (Page* page);
  local void gc_morris2(page)
    var Page* page;
    { # Jede Zelle innerhalb eines Cons enthält nun eine Liste aller
      # Adressen von Pointern auf diese Zelle, die aus einer Wurzel heraus
      # oder aus einem Varobject heraus auf diese Zelle zeigen.
      #
      # Die nicht gelöschten Conses von rechts nach links durchlaufen:
      # (Zwischendurch enthält jede Zelle eine Liste aller Adressen
      # von Pointern auf diese Zelle, die aus einer Wurzel heraus,
      # aus einem Varobject heraus oder aus einem weiter rechts liegenden
      # Cons auf diese Zelle zeigen.)
      var aint p1 = page->page_end; # obere Grenze
      var aint p2 = p1 - page->page_gcpriv.d; # spätere obere Grenze
      var aint p1limit = page->page_start; # untere Grenze
      #ifdef DEBUG_SPVW
      until (p1==p1limit)
        { p1 -= 2*sizeof(object);
          if (eq(*(object*)p1,nullobj)+eq(*(object*)(p1^sizeof(object)),nullobj)==1)
            abort();
        }
      p1 = page->page_end;
      #endif
      until (p1==p1limit) # stets p1limit <= p2 <= p1
        { # Beide Zellen eines Cons werden genau gleich behandelt.
          p1 -= sizeof(object);
          #ifdef DEBUG_SPVW
          if (eq(*(object*)p1,nullobj)+eq(*(object*)(p1^sizeof(object)),nullobj)==1)
            abort();
          #endif
         {var object obj = *(object*)p1;
          if (!eq(obj,nullobj))
            { p2 -= sizeof(object);
              # p1 wird nach p2 verschoben.
              #ifdef TYPECODES
                # Die bisher registrierten Pointer auf diese Zelle werden aktualisiert:
                until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) # Liste abarbeiten
                  { obj = without_mark_bit(obj);
                   {var aint p = upointer(obj);
                    var object next_obj = *(object*)p;
                    *(object*)p = type_pointer_object(typecode(obj),p2);
                    obj = next_obj;
                  }}
                # obj = ursprünglicher Inhalt der Zelle p1.
                #ifdef DEBUG_SPVW
                if (eq(obj,nullobj)) abort();
                #endif
                # Falls die Zelle einen Pointer "nach links" enthält, wird er umgedreht.
                { var tint type = typecode(obj);
                  switch (type)
                    { case_pair:
                        { var aint p = upointer(obj);
                          if (!in_old_generation(obj,type,1) && (p < p1))
                            { # Für spätere Aktualisierung
                              # p1 in die Liste der Pointer auf p einhängen:
                              *(object*)p1 = *(object*)p;
                              *(object*)p = with_mark_bit(type_pointer_object(type,p1));
                              break;
                        }   }
                      default:
                        *(object*)p1 = obj;
                }   }
              #else
                # Die bisher registrierten Pointer auf diese Zelle werden aktualisiert:
                until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) # Liste abarbeiten
                  { obj = without_mark_bit(obj);
                   {var aint p = (aint)ThePointer(obj);
                    var object next_obj = *(object*)p;
                    *(object*)p = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2);
                    obj = next_obj;
                  }}
                # obj = ursprünglicher Inhalt der Zelle p1.
                #ifdef DEBUG_SPVW
                if (eq(obj,nullobj)) abort();
                #endif
                # Falls die Zelle einen Pointer "nach links" enthält, wird er umgedreht.
                if (consp(obj))
                  { var aint p = (aint)ThePointer(obj);
                    if (!in_old_generation(obj,,1) && (p < p1))
                      { # Für spätere Aktualisierung
                        # p1 in die Liste der Pointer auf p einhängen:
                        *(object*)p1 = *(object*)p;
                        *(object*)p = with_mark_bit(as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p1));
                      }
                      else
                      { *(object*)p1 = obj; }
                  }
                  else
                  { *(object*)p1 = obj; }
              #endif
            }
        }}
      if (!(p2==p1limit)) abort();
    }
  local void gc_morris3 (Page* page);
  local void gc_morris3(page)
    var Page* page;
    { # Jede Zelle innerhalb eines Cons enthält nun wieder den ursprünglichen
      # Inhalt.
      #
      # Die nicht gelöschten Conses von links nach rechts durchlaufen
      # und dabei links kompaktieren:
      # (Zwischendurch enthält jede Zelle eine Liste aller Adressen
      # von Pointern auf diese Zelle, die aus einem weiter links liegenden
      # Cons auf diese Zelle zeigen.)
      var aint p1limit = page->page_end; # obere Grenze
      var aint p1 = page->page_start; # untere Grenze
      var aint p2 = p1; # untere Grenze
      until (p1==p1limit) # stets p1limit <= p1 <= p2
        { # Beide Zellen eines Cons werden genau gleich behandelt.
          var object obj = *(object*)p1;
          if (!eq(obj,nullobj))
            { # p1 wird nach p2 verschoben.
              #ifdef TYPECODES
                # Die neu registrierten Pointer auf diese Zelle werden aktualisiert:
                until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) # Liste abarbeiten
                  { obj = without_mark_bit(obj);
                   {var aint p = upointer(obj);
                    var object next_obj = *(object*)p;
                    *(object*)p = type_pointer_object(typecode(obj),p2);
                    obj = next_obj;
                  }}
                # obj = richtiger Inhalt der Zelle p1.
                { var tint type = typecode(obj);
                  if (!gcinvariant_type_p(type)) # unverschieblich -> nichts tun
                    switch (type)
                      { case_pair: # Zwei-Pointer-Objekt
                          { var aint p = upointer(obj);
                            if (p > p1) # Pointer nach rechts?
                              { # Für spätere Aktualisierung
                                # p2 in die Liste der Pointer auf p einhängen:
                                #ifdef DEBUG_SPVW
                                if (eq(*(object*)p,nullobj)) abort();
                                #endif
                                *(object*)p2 = *(object*)p;
                                *(object*)p = with_mark_bit(type_pointer_object(type,p2));
                              }
                            elif (p == p1) # Pointer auf sich selbst?
                              { *(object*)p2 = type_pointer_object(type,p2); }
                            else
                              { *(object*)p2 = obj; }
                          }
                          break;
                        default: # Objekt variabler Länge
                          if (marked(ThePointer(obj))) # markiert?
                            *(object*)p2 = type_untype_object(type,untype(*(object*)ThePointer(obj)));
                            else
                            *(object*)p2 = obj;
                          break;
                      }
                    else # unverschieblich oder Pointer in die alte Generation -> nichts tun
                    { *(object*)p2 = obj; }
                }
              #else
                # Die neu registrierten Pointer auf diese Zelle werden aktualisiert:
                until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) # Liste abarbeiten
                  { obj = without_mark_bit(obj);
                   {var aint p = (aint)ThePointer(obj);
                    var object next_obj = *(object*)p;
                    *(object*)p = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2);
                    obj = next_obj;
                  }}
                # obj = richtiger Inhalt der Zelle p1.
                if (!gcinvariant_object_p(obj)) # unverschieblich -> nichts tun
                  { if (consp(obj))
                      # Zwei-Pointer-Objekt
                      { var aint p = (aint)ThePointer(obj);
                        if (p > p1) # Pointer nach rechts?
                          { # Für spätere Aktualisierung
                            # p2 in die Liste der Pointer auf p einhängen:
                            #ifdef DEBUG_SPVW
                            if (eq(*(object*)p,nullobj)) abort();
                            #endif
                            *(object*)p2 = *(object*)p;
                            *(object*)p = with_mark_bit(as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2));
                          }
                        elif (p == p1) # Pointer auf sich selbst?
                          { *(object*)p2 = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2); }
                        else
                          { *(object*)p2 = obj; }
                      }
                      else
                      # Objekt variabler Länge
                      { if (marked(ThePointer(obj))) # markiert?
                          *(object*)p2 = as_object((as_oint(obj) & nonimmediate_bias_mask) | (*(oint*)ThePointer(obj) & ~wbit(garcol_bit_o) & ~(oint)nonimmediate_bias_mask));
                          else
                          *(object*)p2 = obj;
                      }
                  }
                  else # unverschieblich oder Pointer in die alte Generation -> nichts tun
                  { *(object*)p2 = obj; }
              #endif
              p2 += sizeof(object);
            }
          p1 += sizeof(object);
        }
      # p2 = neue obere Grenze des Cons-Bereiches
      if (!(p2 == page->page_end - page->page_gcpriv.d)) abort();
      page->page_end = p2;
    }

 #else # SPVW_PURE_BLOCKS <==> SINGLEMAP_MEMORY

  # gc_morris2 und gc_morris3 müssen je einmal für jede Page aufgerufen werden,
  # und zwar gc_morris2 von rechts nach links, dann gc_morris3 von links nach rechts
  # (im Sinne der Anordnung der Adressen)!

  local void gc_morris2 (Page* page);
  local void gc_morris2(page)
    var Page* page;
    { # Jede Zelle innerhalb eines Cons enthält nun eine Liste aller
      # Adressen von Pointern auf diese Zelle, die aus einer Wurzel heraus
      # oder aus einem Varobject heraus auf diese Zelle zeigen.
      #
      # Die nicht gelöschten Conses von rechts nach links durchlaufen:
      # (Zwischendurch enthält jede Zelle eine Liste aller Adressen
      # von Pointern auf diese Zelle, die aus einer Wurzel heraus,
      # aus einem Varobject heraus oder aus einem weiter rechts liegenden
      # Cons auf diese Zelle zeigen.)
      var aint p1 = page->page_end; # obere Grenze
      var aint p2 = p1 - page->page_gcpriv.d; # spätere obere Grenze
      var aint p1limit = page->page_start; # untere Grenze
      until (p1==p1limit) # stets p1limit <= p2 <= p1
        { # Beide Zellen eines Cons werden genau gleich behandelt.
          p1 -= sizeof(object);
         {var object obj = *(object*)p1;
          if (!eq(obj,nullobj))
            { p2 -= sizeof(object);
              # p1 wird nach p2 verschoben.
              # Die bisher registrierten Pointer auf diese Zelle werden aktualisiert:
              until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) # Liste abarbeiten
                { obj = without_mark_bit(obj);
                 {var object next_obj = *(object*)pointable(obj);
                  *(object*)pointable(obj) = as_object(p2);
                  obj = next_obj;
                }}
              # obj = ursprünglicher Inhalt der Zelle p1.
              # Falls die Zelle einen Pointer "nach links" enthält, wird er umgedreht.
              if (is_cons_heap(typecode(obj))
                  && !in_old_generation(obj,typecode(obj),1)
                  && ((aint)pointable(obj) < p1)
                 )
                { # Für spätere Aktualisierung
                  # p1 in die Liste der Pointer auf obj einhängen:
                  *(object*)p1 = *(object*)pointable(obj);
                  *(object*)pointable(obj) = with_mark_bit(as_object(p1));
                }
                else
                { *(object*)p1 = obj; }
            }
        }}
      if (!(p2==p1limit)) abort();
    }
  local void gc_morris3 (Page* page);
  local void gc_morris3(page)
    var Page* page;
    { # Jede Zelle innerhalb eines Cons enthält nun wieder den ursprünglichen
      # Inhalt.
      #
      # Die nicht gelöschten Conses von links nach rechts durchlaufen
      # und dabei links kompaktieren:
      # (Zwischendurch enthält jede Zelle eine Liste aller Adressen
      # von Pointern auf diese Zelle, die aus einem weiter links liegenden
      # Cons auf diese Zelle zeigen.)
      var aint p1limit = page->page_end; # obere Grenze
      var aint p1 = page->page_start; # untere Grenze
      var aint p2 = p1; # untere Grenze
      until (p1==p1limit) # stets p1limit <= p1 <= p2
        { # Beide Zellen eines Cons werden genau gleich behandelt.
          var object obj = *(object*)p1;
          if (!eq(obj,nullobj))
            { # p1 wird nach p2 verschoben.
              # Die neu registrierten Pointer auf diese Zelle werden aktualisiert:
              until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) # Liste abarbeiten
                { obj = without_mark_bit(obj);
                 {var object next_obj = *(object*)pointable(obj);
                  *(object*)pointable(obj) = as_object(p2);
                  obj = next_obj;
                }}
              # obj = richtiger Inhalt der Zelle p1.
              { var tint type = typecode(obj);
                if (!is_unused_heap(type) && !in_old_generation(obj,type,?))
                  if (is_cons_heap(type))
                    # Zwei-Pointer-Objekt
                    { if ((aint)pointable(obj) > p1) # Pointer nach rechts?
                        { # Für spätere Aktualisierung
                          # p2 in die Liste der Pointer auf obj einhängen:
                          *(object*)p2 = *(object*)pointable(obj);
                          *(object*)pointable(obj) = with_mark_bit(as_object(p2));
                        }
                      elif ((aint)pointable(obj) == p1) # Pointer auf sich selbst?
                        { *(object*)p2 = as_object(p2); }
                      else
                        { *(object*)p2 = obj; }
                    }
                    else
                    # Objekt variabler Länge
                    { if (marked(ThePointer(obj))) # markiert?
                        *(object*)p2 = type_untype_object(type,untype(*(object*)ThePointer(obj)));
                        else
                        *(object*)p2 = obj;
                    }
                  else # unverschieblich oder Pointer in die alte Generation -> nichts tun
                  { *(object*)p2 = obj; }
              }
              p2 += sizeof(object);
            }
          p1 += sizeof(object);
        }
      # p2 = neue obere Grenze des Cons-Bereiches
      if (!(p2 == page->page_end - page->page_gcpriv.d)) abort();
      page->page_end = p2;
    }

 #endif

#endif

# Den Selbstpointer eines Objekts variabler Länge modifizieren:
# set_GCself(p,type,addr);
# setzt p->GCself auf type_pointer_object(type,addr).
  #ifdef TYPECODES
    #if !(exact_uint_size_p(oint_type_len) && ((oint_type_shift%hfintsize)==0) && (tint_type_mask == bit(oint_type_len)-1))
      #ifdef MAP_MEMORY
        # addr enthält Typinfo
        #define set_GCself(p,type,addr)  \
          ((Varobject)(p))->GCself = type_pointer_object((type)&(tint_type_mask),(addr)&(oint_addr_mask))
      #else
        # addr enthält keine Typinfo
        #define set_GCself(p,type,addr)  \
          ((Varobject)(p))->GCself = type_pointer_object((type)&(tint_type_mask),addr)
      #endif
    #else # besser: zwar zwei Speicherzugriffe, jedoch weniger Arithmetik
      #define set_GCself(p,type,addr)  \
        ((Varobject)(p))->GCself = type_pointer_object(0,addr), \
        ((Varobject)(p))->header_flags = (type)
    #endif
  #else
    #define set_GCself(p,type,addr)  /* ignore type */ \
      ((Varobject)(p))->GCself = as_object((oint)(addr))
  #endif

# Objekte variabler Länge zwischen page->page_start und page->page_end zur
# Zusammenschiebung nach unten vorbereiten. Dabei wird in jedes markierte
# Objekt vorne der Pointer auf die Stelle eingetragen, wo das
# Objekt später stehen wird (samt Typinfo). Ist das darauffolgende
# Objekt unmarkiert, so wird in dessen erstem Pointer die Adresse
# des nächsten markierten Objekts eingetragen.
  #ifdef SPVW_PURE
  local aint gc_sweep1_varobject_page (uintL heapnr, aint start, aint end, object* firstmarked, aint dest);
  local aint gc_sweep1_varobject_page (
    var uintL heapnr,
    var aint start,
    var aint end,
    var object* firstmarked,
    var aint dest)
  #elif defined(GENERATIONAL_GC)
  local aint gc_sweep1_varobject_page (aint start, aint end, object* firstmarked, aint dest);
  local aint gc_sweep1_varobject_page (
    var aint start,
    var aint end,
    var object* firstmarked,
    var aint dest)
  #else
  local void gc_sweep1_varobject_page (Page* page);
  local void gc_sweep1_varobject_page (
    var Page* page)
  #endif
    {
      #if defined(SPVW_PURE) || defined(GENERATIONAL_GC)
      var object* last_open_ptr = firstmarked;
      var aint p2 = start; # Source-Pointer
      var aint p2end = end; # obere Grenze des Source-Bereiches
      var aint p1 = dest; # Ziel-Pointer
      #else
      var object* last_open_ptr = &page->page_gcpriv.firstmarked;
        # In *last_open_ptr ist stets die Adresse des nächsten markierten
        # Objekts (als oint) einzutragen.
        # Durch verkettete-Liste-Mechanismus: Am Schluß enthält
        # page->page_gcpriv.firstmarked die Adresse des 1. markierten Objekts
      var aint p2 = page->page_start; # Source-Pointer
      var aint p2end = page->page_end; # obere Grenze des Source-Bereiches
      var aint p1 = p2; # Ziel-Pointer
      #endif
      # start <= p1 <= p2 <= end, p1 und p2 wachsen, p2 schneller als p1.
      var_prepare_objsize;
      sweeploop1:
        # Nächstes markiertes Objekt suchen.
        # Adresse des nächsten markierten Objekts in *last_open_ptr eintragen.
        if (p2==p2end) goto sweepok1; # obere Grenze erreicht -> fertig
        {
          #ifdef TYPECODES
          var tint flags = mtypecode(((Varobject)p2)->GCself);
          # Typinfo (und Flags bei Symbolen) retten
          #endif
          var uintL laenge = objsize((Varobject)p2); # Byte-Länge bestimmen
          if (!marked(p2)) # Objekt unmarkiert?
            { p2 += laenge; goto sweeploop1; } # ja -> zum nächsten Objekt
          # Objekt markiert
          *last_open_ptr = pointer_as_object(p2); # Adresse ablegen
          set_GCself(p2, flags,p1); # neue Adresse eintragen, mit alter
                         # Typinfo (darin ist auch das Markierungsbit enthalten)
          #ifndef TYPECODES
          mark(p2);
          #endif
          p2 += laenge; # Sourceadresse für nächstes Objekt
          p1 += laenge; # Zieladresse für nächstes Objekt
        }
      sweeploop2:
        # Nächstes unmarkiertes Objekt suchen.
        if (p2==p2end) goto sweepok2; # obere Grenze erreicht -> fertig
        {
          #ifdef TYPECODES
          var tint flags = mtypecode(((Varobject)p2)->GCself);
          # Typinfo (und Flags bei Symbolen) retten
          #endif
          var uintL laenge = objsize((Varobject)p2); # Byte-Länge bestimmen
          if (!marked(p2)) # Objekt unmarkiert?
            { last_open_ptr = (object*)p2; # ja -> Hier den nächsten Pointer ablegen
              p2 += laenge; goto sweeploop1; # und zum nächsten Objekt
            }
          # Objekt markiert
          set_GCself(p2, flags,p1); # neue Adresse eintragen, mit alter
                         # Typinfo (darin ist auch das Markierungsbit enthalten)
          #ifndef TYPECODES
          mark(p2);
          #endif
          p2 += laenge; # Sourceadresse für nächstes Objekt
          p1 += laenge; # Zieladresse für nächstes Objekt
          goto sweeploop2;
        }
      sweepok1: *last_open_ptr = pointer_as_object(p2);
      sweepok2: ;
      #if defined(SPVW_PURE) || defined(GENERATIONAL_GC)
      return p1;
      #endif
    }

# Aktualisierungsphase:
  # Der gesamte LISP-Speicher wird durchgegangen und dabei alte durch
  # neue Adressen ersetzt.
  # Aktualisierung eines Objekts *objptr :
    #if !defined(MORRIS_GC)
      #ifdef TYPECODES
        #define update(objptr)  \
          { var tint type = mtypecode(*(object*)objptr);                          \
            if (!gcinvariant_type_p(type)) # unverschieblich -> nichts tun        \
              { var object obj = *(object*)objptr; # fragliches Objekt            \
                if (!in_old_generation(obj,type,mem.heapnr_from_type[type]))      \
                  # ältere Generation -> nichts zu tun (Objekt blieb stehen)      \
                  if (marked(ThePointer(obj))) # markiert?                        \
                    # nein -> nichts zu tun (Objekt blieb stehen)                 \
                    # ja -> neue Adresse eintragen und Typinfobyte (incl.         \
                    #       evtl. Symbol-Bindungsflags) zurückschreiben           \
                    *(object*)objptr =                                            \
                      type_untype_object(type,untype(*(object*)ThePointer(obj))); \
          }   }
      #else
        #define update(objptr)  \
          { var object obj = *(object*)objptr; # fragliches Objekt          \
            if (!gcinvariant_object_p(obj)) # unverschieblich -> nichts tun \
              if (!in_old_generation(obj,,))                                \
                # ältere Generation -> nichts zu tun (Objekt blieb stehen)  \
                if (marked(ThePointer(obj))) # markiert?                    \
                  # nein -> nichts zu tun (Objekt blieb stehen)             \
                  # ja -> neue Adresse eintragen                            \
                  *(object*)objptr =                                        \
                    as_object((as_oint(obj) & nonimmediate_bias_mask) | (*(oint*)ThePointer(obj) & ~wbit(garcol_bit_o))); \
          }
      #endif
    #else # defined(MORRIS_GC)
      #if defined(SPVW_MIXED_BLOCKS)
        #ifdef TYPECODES
          #define update(objptr)  \
            { var tint type = mtypecode(*(object*)objptr);                          \
              if (!gcinvariant_type_p(type)) # unverschieblich -> nichts tun        \
                switch (type)                                                       \
                  { default: # Objekt variabler Länge                               \
                      { var object obj = *(object*)objptr; # fragliches Objekt      \
                        if (!in_old_generation(obj,type,0))                         \
                          if (marked(ThePointer(obj))) # markiert?                  \
                            *(object*)objptr = type_untype_object(type,untype(*(object*)ThePointer(obj))); \
                      }                                                             \
                      break;                                                        \
                    case_pair: # Zwei-Pointer-Objekt                                \
                      { var object obj = *(object*)objptr; # fragliches Objekt      \
                        if (!in_old_generation(obj,type,1))                         \
                          { # Für spätere Aktualisierung in dessen Liste einhängen: \
                            *(object*)objptr = *(object*)ThePointer(obj);           \
                            *(object*)ThePointer(obj) = with_mark_bit(type_pointer_object(type,objptr)); \
                      }   }                                                         \
                      break;                                                        \
            }     }
        #else
          #define update(objptr)  \
            { var object obj = *(object*)objptr; # fragliches Objekt              \
              if (!gcinvariant_object_p(obj))                                     \
                { if (consp(obj))                                                 \
                    # Zwei-Pointer-Objekt                                         \
                    { if (!in_old_generation(obj,,1))                             \
                        { # Für spätere Aktualisierung in dessen Liste einhängen: \
                          *(object*)objptr = *(object*)ThePointer(obj);           \
                          *(object*)ThePointer(obj) = with_mark_bit(as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)objptr)); \
                    }   }                                                         \
                    else                                                          \
                    # Objekt variabler Länge                                      \
                    { if (!in_old_generation(obj,,0))                             \
                        { if (marked(ThePointer(obj))) # markiert?                \
                            *(object*)objptr = as_object((as_oint(obj) & nonimmediate_bias_mask) | (*(oint*)ThePointer(obj) & ~wbit(garcol_bit_o) & ~(oint)nonimmediate_bias_mask)); \
                    }   }                                                         \
            }   }
        #endif
      #else # defined(SPVW_PURE_BLOCKS) # && defined(SINGLEMAP_MEMORY)
        #define update(objptr)  \
          { var tint type = mtypecode(*(object*)objptr);                      \
            if (!is_unused_heap(type)) # unverschieblich -> nichts tun        \
              { var object obj = *(object*)objptr; # fragliches Objekt        \
                if (!in_old_generation(obj,type,?))                           \
                  # ältere Generation -> nichts zu tun (Objekt blieb stehen)  \
                  if (is_varobject_heap(type))                                \
                    # Objekt variabler Länge                                  \
                    { if (marked(ThePointer(obj))) # markiert?                \
                        *(object*)objptr = type_untype_object(type,untype(*(object*)ThePointer(obj))); \
                    }                                                         \
                    else                                                      \
                    # Zwei-Pointer-Objekt                                     \
                    { # Für spätere Aktualisierung in dessen Liste einhängen: \
                      *(object*)objptr = *(object*)ThePointer(obj);           \
                      *(object*)ThePointer(obj) = with_mark_bit(pointer_as_object(objptr)); \
                    }                                                         \
          }   }
      #endif
    #endif
    #ifndef NO_symbolflags
      #define update_stackobj(objptr)  \
        switch (mtypecode(*objptr))                               \
          { case_symbolflagged: # Symbol mit evtl. Flags          \
              { var object obj1 = *objptr;                        \
                var object obj2 = symbol_without_flags(obj1);     \
                var oint flags = as_oint(obj1) ^ as_oint(obj2);   \
                *objptr = obj2; # vorerst Flags löschen           \
                update(objptr); # dann aktualisieren              \
                *(oint*)objptr |= flags; # dann Flags wieder rein \
                break;                                            \
              }                                                   \
            default: update(objptr); break;                       \
          }
    #else
      #define update_stackobj(objptr)  \
        update(objptr);
    #endif
  # Durchlaufen durch alle LISP-Objekte und aktualisieren:
    #ifdef GENERATIONAL_GC
    # Pointer in den Objekten der alten Generation aktualisieren:
      local void update_old_generation (void);
      local void update_at (object* ptr);
      local void update_at(ptr)
        var object* ptr;
        { update(ptr); }
      local void update_old_generation()
        { var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            if (is_heap_containing_objects(heapnr)) # Objekte, die keine Pointer enthalten,
                                                    # braucht man nicht zu durchlaufen.
              { var Heap* heap = &mem.heaps[heapnr];
                var aint gen0_start = heap->heap_gen0_start;
                var aint gen0_end = heap->heap_gen0_end;
                if (gen0_start < gen0_end)
                  if (heap->physpages==NULL)
                    { walk_area_(heapnr,gen0_start,gen0_end,update_at); } # fallback
                    else
                    { var physpage_state* physpage = heap->physpages;
                      gen0_start &= -physpagesize;
                      do { if ((physpage->protection == PROT_NONE)
                               || (physpage->protection == PROT_READ)
                              )
                             # Cache ausnutzen, gecachte Pointer aktualisieren:
                             { var uintL count = physpage->cache_size;
                               if (count > 0)
                                 { var old_new_pointer* ptr = physpage->cache;
                                   dotimespL(count,count, { update(&ptr->o); ptr++; } );
                                   if (!(physpage->protection == PROT_NONE))
                                     { xmmprotect(heap, gen0_start,physpagesize,PROT_NONE);
                                       physpage->protection = PROT_NONE;
                             }   }   }
                             else
                             # ganzen Page-Inhalt aktualisieren:
                             { walk_physpage_(heapnr,physpage,gen0_start+physpagesize,gen0_end,update_at); }
                           gen0_start += physpagesize;
                           physpage++;
                         }
                         while (gen0_start < gen0_end);
        }     }     }
    #endif

# Zweite SWEEP-Phase:
  # Verschiebung eines Objekts variabler Länge, p1 und p2 weiterrücken:
  # move_aligned_p1_p2(count);
  #if (varobject_alignment==1)
    #define uintV  uintB
  #elif (varobject_alignment==2)
    #define uintV  uintW
  #elif (varobject_alignment==4)
    #define uintV  uintL
  #elif (varobject_alignment==8)
    #define uintV  uintL2
  #else
    #error "Unbekannter Wert von 'varobject_alignment'!"
  #endif
  #ifdef GNU # so läßt sich's besser optimieren
    #ifdef fast_dotimesL
      #define move_aligned_p1_p2(count)  \
        dotimespL(count,count/varobject_alignment, *((uintV*)p2)++ = *((uintV*)p1)++; )
    #else
      #define move_aligned_p1_p2(count)  \
        do { *((uintV*)p2)++ = *((uintV*)p1)++; count -= varobject_alignment; } until (count==0)
    #endif
  #else # andere Compiler akzeptieren ((type*)p)++ nicht.
    # Wie effizient ist das hier ??
    #define move_aligned_p1_p2(count)  \
      do { *(uintV*)p2 = *(uintV*)p1;                            \
           p1 += varobject_alignment; p2 += varobject_alignment; \
           count -= varobject_alignment;                         \
         }                                                                              \
         until (count==0)
  #endif
  # Die Objekte variabler Länge werden an die vorher berechneten
  # neuen Plätze geschoben.
  #ifdef SPVW_PURE
  local void gc_sweep2_varobject_page (Page* page, uintL heapnr);
  local void gc_sweep2_varobject_page (
    var Page* page,
    var uintL heapnr)
  #else
  local void gc_sweep2_varobject_page (Page* page);
  local void gc_sweep2_varobject_page (
    var Page* page)
  #endif
    # Von unten nach oben durchgehen und dabei runterschieben:
    { var aint p1 = (aint)pointer_was_object(page->page_gcpriv.firstmarked); # Source-Pointer, erstes markiertes Objekt
      var aint p1end = page->page_end;
      var aint p2 = page->page_start; # Ziel-Pointer
      var_prepare_objsize;
      until (p1==p1end) # obere Grenze erreicht -> fertig
        { # nächstes Objekt hat Adresse p1
          if (marked(p1)) # markiert?
            { unmark(p1); # Markierung löschen
              # Objekt behalten und verschieben:
             {var uintL count = objsize((Varobject)p1); # Länge (durch varobject_alignment teilbar, >0)
              if (!(p1==p2)) # falls Verschiebung nötig
                { move_aligned_p1_p2(count); } # verschieben und weiterrücken
                else # sonst nur weiterrücken:
                { p1 += count; p2 += count; }
            }}
            else
            { p1 = (aint)pointer_was_object(*(object*)p1); } # mit Pointer (Typinfo=0) zum nächsten markierten Objekt
        }
      page->page_end = p2; # obere Grenze der Objekte variabler Länge neu setzen
    }

#ifdef GENERATIONAL_GC

  # Baut einen Cache aller Pointer in der alten Generation.
  # Die neue Generation ist leer; Pointer in die neue Generation gibt es daher keine!
  local void build_old_generation_cache (uintL heapnr);
  local void build_old_generation_cache(heapnr)
    var uintL heapnr;
    { if (is_heap_containing_objects(heapnr)) # Objekte, die keine Pointer enthalten, brauchen keinen Cache.
        { var Heap* heap = &mem.heaps[heapnr];
          var aint gen0_start = heap->heap_gen0_start;
          var aint gen0_end = heap->heap_gen0_end;
          var aint gen0_start_pa = gen0_start & -physpagesize; # page-aligned
          var aint gen0_end_pa = (gen0_end + (physpagesize-1)) & -physpagesize; # page-aligned
         {var uintL physpage_count = (gen0_end_pa - gen0_start_pa) >> physpageshift;
          if (physpage_count==0)
            { xfree(heap->physpages); heap->physpages = NULL; }
            else
            { # NB: The algorithms below work in terms of "page boundary crossings".
              # An object occupying the memory range [objptr,nextptr) is considered to
              # cover the page boundaries  addr  with  physpagesize | addr  and
              # objptr < addr <= nextptr. (*Not* objptr <= addr < nextptr.) When a
              # page boundary is crossed, the continued_addr, continued_count, firstobject
              # fields of the physpage after it are set. Therefore, if gen0_end happens
              # to lie on a page boundary, we need room for one more physpage_state.
              # It will only be written to, never really be used (because the page after
              # this last page boundary doesn't really exist).
              heap->physpages = (physpage_state*) xrealloc(heap->physpages,(physpage_count+(gen0_end==gen0_end_pa))*sizeof(physpage_state));
              if (!(heap->physpages==NULL))
                {
                  #if defined(SELFMADE_MMAP) && !defined(SPVW_PURE_BLOCKS)
                  # Spätestens jetzt muß man den Speicherinhalt vom mem-File holen.
                  # (Die Conses könnte man noch weiter verzögern, aber bringt das viel?)
                  { var uintL pageno;
                    for (pageno = 0; pageno < heap->memfile_numpages; pageno++)
                      if (handle_mmap_fault(heap->memfile_offset+(pageno<<physpageshift),
                                            gen0_start+(pageno<<physpageshift),
                                            &heap->memfile_pages[pageno])
                          <0)
                        abort();
                  }
                  #endif
                  # Wenn wir fertig sind, wird sowohl Cache als auch Speicherinhalt
                  # gültig sein:
                  xmmprotect(heap, gen0_start_pa, gen0_end_pa-gen0_start_pa, PROT_READ);
                  # heap->physpages[0..physpage_count-1] füllen:
                  { var physpage_state* physpage = heap->physpages;
                    var uintL count;
                    dotimespL(count,physpage_count,
                      { physpage->protection = PROT_READ;
                        physpage->cache_size = 0; physpage->cache = NULL;
                        physpage++;
                      });
                  }
                  if (is_cons_heap(heapnr))
                    # Conses u.ä.
                    { # Von gen0_start bis gen0_end sind alles Pointer.
                      var physpage_state* physpage = heap->physpages;
                      var uintL count;
                      #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
                      # Alle Seiten bis auf die letzte voll, die letzte teilweise voll.
                      dotimesL(count,physpage_count-1,
                        { # für i=0,1,...:
                          #   gen0_start = heap->heap_gen0_start + i*physpagesize
                          #   physpage = &heap->physpages[i]
                          physpage->continued_addr = (object*)gen0_start;
                          physpage->continued_count = physpagesize/sizeof(object);
                          gen0_start += physpagesize;
                          physpage->firstobject = gen0_start;
                          physpage++;
                        });
                      physpage->continued_addr = (object*)gen0_start;
                      physpage->continued_count = (gen0_end-gen0_start)/sizeof(object);
                      physpage->firstobject = gen0_end;
                      #else
                      # Alle Seiten bis auf die erste voll, die erste teilweise voll.
                      physpage->continued_addr = (object*)gen0_start;
                      physpage->continued_count = ((gen0_start_pa+physpagesize)-gen0_start)/sizeof(object);
                      physpage->firstobject = gen0_start = gen0_start_pa+physpagesize;
                      dotimesL(count,physpage_count-1,
                        { physpage++;
                          # für i=1,...:
                          #   gen0_start = (heap->heap_gen0_start & -physpagesize) + i*physpagesize
                          #   physpage = &heap->physpages[i]
                          physpage->continued_addr = (object*)gen0_start;
                          physpage->continued_count = physpagesize/sizeof(object);
                          gen0_start += physpagesize;
                          physpage->firstobject = gen0_start;
                        });
                      #endif
                    }
                    else
                    # is_varobject_heap(heapnr), Objekte variabler Länge
                    { var physpage_state* physpage = heap->physpages;
                      var aint objptr = gen0_start;
                      # Für i=0,1,... ist
                      #   gen0_start = heap->heap_gen0_start + i*physpagesize
                      #   physpage = &heap->physpages[i]
                      # Mit wachsendem i geht man von einer Seite zur nächsten.
                      # Gleichzeitig geht man von einem Objekt zum nächsten und markiert
                      # alle Pointer zwischen objptr (Pointer auf das aktuelle Objekt)
                      # und nextptr (Pointer auf das nächste Objekt). Glücklicherweise
                      # kommen in allen unseren Objekten die Pointer am Stück:
                      # ab ptr kommen count Pointer.
                      # Das Intervall ptr...ptr+count*sizeof(object) wird nun zerlegt.
                      #ifdef SPVW_PURE
                      switch (heapnr)
                        { case_symbol: # Symbol
                            physpage->continued_addr = (object*)gen0_start; # irrelevant
                            physpage->continued_count = 0;
                            physpage->firstobject = gen0_start;
                            gen0_start += physpagesize; physpage++;
                            while (objptr < gen0_end)
                              { var aint nextptr = objptr + size_symbol();
                                # Hier ist gen0_start-physpagesize <= objptr < gen0_start.
                                if (nextptr >= gen0_start)
                                  { var aint ptr = objptr+symbol_objects_offset;
                                    var uintC count = (sizeof(symbol_)-symbol_objects_offset)/sizeof(object);
                                    if (ptr < gen0_start)
                                      { physpage->continued_addr = (object*)gen0_start;
                                        physpage->continued_count = count - (gen0_start-ptr)/sizeof(object);
                                      }
                                      else
                                      { physpage->continued_addr = (object*)ptr;
                                        physpage->continued_count = count;
                                      }
                                    physpage->firstobject = nextptr;
                                    # Man überquert höchstens eine Seitengrenze auf einmal.
                                    gen0_start += physpagesize; physpage++;
                                  }
                                objptr = nextptr;
                              }
                            if (!(objptr == gen0_end)) abort();
                            break;
                          case_mdarray: case_obvector: case_ostring: case_ovector: # nicht-simple Arrays:
                            physpage->continued_addr = (object*)gen0_start; # irrelevant
                            physpage->continued_count = 0;
                            physpage->firstobject = gen0_start;
                            gen0_start += physpagesize; physpage++;
                            while (objptr < gen0_end)
                              { var aint nextptr = objptr + objsize_iarray((Iarray)objptr);
                                # Hier ist gen0_start-physpagesize <= objptr < gen0_start.
                                if (nextptr >= gen0_start)
                                  { var aint ptr = (aint)&((Iarray)objptr)->data;
                                    # count = 1;
                                    if (ptr < gen0_start)
                                      { physpage->continued_addr = (object*)gen0_start; # irrelevant
                                        physpage->continued_count = 0;
                                      }
                                      else
                                      { physpage->continued_addr = (object*)ptr;
                                        physpage->continued_count = 1;
                                      }
                                    # Man überquerte höchstens eine Seitengrenze.
                                    # Danach kommen (bis nextptr) keine Pointer mehr.
                                    loop
                                      { physpage->firstobject = nextptr;
                                        gen0_start += physpagesize; physpage++;
                                        if (nextptr < gen0_start) break;
                                        physpage->continued_addr = (object*)gen0_start; # irrelevant
                                        physpage->continued_count = 0;
                                      }
                                  }
                                objptr = nextptr;
                              }
                            if (!(objptr == gen0_end)) abort();
                            break;
                          case_svector: # simple-vector
                            physpage->continued_addr = (object*)gen0_start; # irrelevant
                            physpage->continued_count = 0;
                            physpage->firstobject = gen0_start;
                            gen0_start += physpagesize; physpage++;
                            while (objptr < gen0_end)
                              { var uintL count = svector_length((Svector)objptr);
                                var aint nextptr = objptr + size_svector(count);
                                # Hier ist gen0_start-physpagesize <= objptr < gen0_start.
                                if (nextptr >= gen0_start)
                                  { var aint ptr = (aint)&((Svector)objptr)->data[0];
                                    if (ptr < gen0_start)
                                      { var uintL count_thispage = (gen0_start-ptr)/sizeof(object);
                                        if ((varobject_alignment == sizeof(object)) # das erzwingt count >= count_thispage
                                            || (count >= count_thispage)
                                           )
                                          { count -= count_thispage; }
                                          else
                                          { count = 0; }
                                        ptr = gen0_start;
                                      }
                                    do { physpage->continued_addr = (object*)ptr;
                                         gen0_start += physpagesize;
                                        {var uintL count_thispage = (gen0_start-ptr)/sizeof(object);
                                         if (count >= count_thispage)
                                           { physpage->continued_count = count_thispage;
                                             count -= count_thispage;
                                           }
                                           else
                                           { physpage->continued_count = count; count = 0; }
                                         physpage->firstobject = nextptr;
                                         physpage++;
                                         ptr = gen0_start;
                                       }}
                                       until (nextptr < gen0_start);
                                  }
                                objptr = nextptr;
                              }
                            if (!(objptr == gen0_end)) abort();
                            break;
                          case_record: # Record
                            physpage->continued_addr = (object*)gen0_start; # irrelevant
                            physpage->continued_count = 0;
                            physpage->firstobject = gen0_start;
                            gen0_start += physpagesize; physpage++;
                            while (objptr < gen0_end)
                              { var uintC count;
                                var aint nextptr;
                                if (record_type((Record)objptr) < rectype_limit)
                                  { count = srecord_length((Srecord)objptr); nextptr = objptr + size_srecord(count); }
                                  else
                                  { count = xrecord_length((Xrecord)objptr); nextptr = objptr + size_xrecord(count,xrecord_xlength((Xrecord)objptr)); }
                                if (nextptr >= gen0_start)
                                  { var aint ptr = (aint)&((Record)objptr)->recdata[0];
                                    if (ptr < gen0_start)
                                      { var uintL count_thispage = (gen0_start-ptr)/sizeof(object);
                                        if (count >= count_thispage)
                                          { count -= count_thispage; }
                                          else
                                          { count = 0; }
                                        ptr = gen0_start;
                                      }
                                    do { physpage->continued_addr = (object*)ptr;
                                         gen0_start += physpagesize;
                                        {var uintL count_thispage = (gen0_start-ptr)/sizeof(object);
                                         if (count >= count_thispage)
                                           { physpage->continued_count = count_thispage;
                                             count -= count_thispage;
                                           }
                                           else
                                           { physpage->continued_count = count; count = 0; }
                                         physpage->firstobject = nextptr;
                                         physpage++;
                                         ptr = gen0_start;
                                       }}
                                       until (nextptr < gen0_start);
                                  }
                                objptr = nextptr;
                              }
                            if (!(objptr == gen0_end)) abort();
                            break;
                          default:
                            # Solche Objekte kommen nicht vor.
                            abort();
                        }
                      #else # SPVW_MIXED
                      physpage->continued_addr = (object*)gen0_start; # irrelevant
                      physpage->continued_count = 0;
                      physpage->firstobject = gen0_start;
                      gen0_start += physpagesize; physpage++;
                      while (objptr < gen0_end)
                        {
                          #ifdef TYPECODES
                          switch (typecode_at(objptr)) # Typ des nächsten Objekts
                          #else
                          goto case_record;
                          switch (0)
                          #endif
                            {
                              #ifdef TYPECODES
                              case_symbolwithflags: # Symbol
                                { var aint nextptr = objptr + size_symbol();
                                  # Hier ist gen0_start-physpagesize <= objptr < gen0_start.
                                  if (nextptr >= gen0_start)
                                    { var aint ptr = objptr+symbol_objects_offset;
                                      var uintC count = (sizeof(symbol_)-symbol_objects_offset)/sizeof(object);
                                      if (ptr < gen0_start)
                                        { physpage->continued_addr = (object*)gen0_start;
                                          physpage->continued_count = count - (gen0_start-ptr)/sizeof(object);
                                        }
                                        else
                                        { physpage->continued_addr = (object*)ptr;
                                          physpage->continued_count = count;
                                        }
                                      physpage->firstobject = nextptr;
                                      # Man überquert höchstens eine Seitengrenze auf einmal.
                                      gen0_start += physpagesize; physpage++;
                                    }
                                  objptr = nextptr;
                                }
                                break;
                              #endif
                              case_mdarray: case_obvector: case_ostring: case_ovector: # nicht-simple Arrays:
                                { var aint nextptr = objptr + objsize((Iarray)objptr);
                                  # Hier ist gen0_start-physpagesize <= objptr < gen0_start.
                                  if (nextptr >= gen0_start)
                                    { var aint ptr = (aint)&((Iarray)objptr)->data;
                                      # count = 1;
                                      if (ptr < gen0_start)
                                        { physpage->continued_addr = (object*)gen0_start; # irrelevant
                                          physpage->continued_count = 0;
                                        }
                                        else
                                        { physpage->continued_addr = (object*)ptr;
                                          physpage->continued_count = 1;
                                        }
                                      # Man überquerte höchstens eine Seitengrenze.
                                      # Danach kommen (bis nextptr) keine Pointer mehr.
                                      loop
                                        { physpage->firstobject = nextptr;
                                          gen0_start += physpagesize; physpage++;
                                          if (nextptr < gen0_start) break;
                                          physpage->continued_addr = (object*)gen0_start; # irrelevant
                                          physpage->continued_count = 0;
                                        }
                                    }
                                  objptr = nextptr;
                                }
                                break;
                              case_svector: # simple-vector
                                { var uintL count = svector_length((Svector)objptr);
                                  var aint nextptr = objptr + size_svector(count);
                                  # Hier ist gen0_start-physpagesize <= objptr < gen0_start.
                                  if (nextptr >= gen0_start)
                                    { var aint ptr = (aint)&((Svector)objptr)->data[0];
                                      if (ptr < gen0_start)
                                        { var uintL count_thispage = (gen0_start-ptr)/sizeof(object);
                                          if ((varobject_alignment == sizeof(object)) # das erzwingt count >= count_thispage
                                              || (count >= count_thispage)
                                             )
                                            { count -= count_thispage; }
                                            else
                                            { count = 0; }
                                          ptr = gen0_start;
                                        }
                                      do { physpage->continued_addr = (object*)ptr;
                                           gen0_start += physpagesize;
                                          {var uintL count_thispage = (gen0_start-ptr)/sizeof(object);
                                           if (count >= count_thispage)
                                             { physpage->continued_count = count_thispage;
                                               count -= count_thispage;
                                             }
                                             else
                                             { physpage->continued_count = count; count = 0; }
                                           physpage->firstobject = nextptr;
                                           physpage++;
                                           ptr = gen0_start;
                                         }}
                                         until (nextptr < gen0_start);
                                    }
                                  objptr = nextptr;
                                }
                                break;
                              case_record: # Record
                                #ifndef TYPECODES
                                switch (record_type((Record)objptr))
                                  { case_Rectype_mdarray_above;
                                    case_Rectype_obvector_above;
                                    case_Rectype_ostring_above;
                                    case_Rectype_ovector_above;
                                    case_Rectype_Svector_above;
                                    case Rectype_Sbvector: case Rectype_Sstring:
                                    case Rectype_Bignum:
                                    case Rectype_Ffloat: case Rectype_Dfloat: case Rectype_Lfloat:
                                      goto case_nopointers;
                                    default: ;
                                  }
                                #endif
                                { var uintC count;
                                  var aint nextptr;
                                  if (record_type((Record)objptr) < rectype_limit)
                                    { count = srecord_length((Srecord)objptr); nextptr = objptr + size_srecord(count); }
                                    else
                                    { count = xrecord_length((Xrecord)objptr); nextptr = objptr + size_xrecord(count,xrecord_xlength((Xrecord)objptr)); }
                                  if (nextptr >= gen0_start)
                                    { var aint ptr = (aint)&((Record)objptr)->recdata[0];
                                      if (ptr < gen0_start)
                                        { var uintL count_thispage = (gen0_start-ptr)/sizeof(object);
                                          if (count >= count_thispage)
                                            { count -= count_thispage; }
                                            else
                                            { count = 0; }
                                          ptr = gen0_start;
                                        }
                                      do { physpage->continued_addr = (object*)ptr;
                                           gen0_start += physpagesize;
                                          {var uintL count_thispage = (gen0_start-ptr)/sizeof(object);
                                           if (count >= count_thispage)
                                             { physpage->continued_count = count_thispage;
                                               count -= count_thispage;
                                             }
                                             else
                                             { physpage->continued_count = count; count = 0; }
                                           physpage->firstobject = nextptr;
                                           physpage++;
                                           ptr = gen0_start;
                                         }}
                                         until (nextptr < gen0_start);
                                    }
                                  objptr = nextptr;
                                }
                                break;
                              case_nopointers:
                              default: # simple-bit-vector, simple-string, bignum, float
                                # Keine Pointer.
                                objptr += objsize((Varobject)objptr);
                                while (objptr >= gen0_start)
                                  { physpage->continued_addr = (object*)gen0_start; # irrelevant
                                    physpage->continued_count = 0;
                                    physpage->firstobject = objptr;
                                    gen0_start += physpagesize; physpage++;
                                  }
                                break;
                        }   }
                      if (!(objptr == gen0_end)) abort();
                      #endif
                    }
                }
    }   }}  }

  # Baut einen Cache aller Pointer von der alten in die neue Generation.
  local void rebuild_old_generation_cache (uintL heapnr);
  local void rebuild_old_generation_cache(heapnr)
    var uintL heapnr;
    { if (is_heap_containing_objects(heapnr)) # Objekte, die keine Pointer enthalten, brauchen keinen Cache.
        { var Heap* heap = &mem.heaps[heapnr];
          var aint gen0_start = heap->heap_gen0_start;
          var aint gen0_end = heap->heap_gen0_end;
          if ((gen0_start < gen0_end) && !(heap->physpages==NULL))
            { var physpage_state* physpage = heap->physpages;
              gen0_start &= -physpagesize;
              do { if (physpage->protection == PROT_READ_WRITE)
                     { var DYNAMIC_ARRAY(cache_buffer,old_new_pointer,physpagesize/sizeof(object));
                       var old_new_pointer* cache_ptr = &cache_buffer[0];
                       #ifdef TYPECODES
                         #define cache_at(obj)  \
                           { var tint type = mtypecode(obj);                                   \
                             if (!gcinvariant_type_p(type)) # unverschieblich?                 \
                               if (!in_old_generation(obj,type,mem.heapnr_from_type[type]))    \
                                 # obj ist ein Pointer in die neue Generation -> merken        \
                                 { cache_ptr->p = &(obj); cache_ptr->o = (obj); cache_ptr++; } \
                           }
                       #else
                         #define cache_at(obj)  \
                           { if (!gcinvariant_object_p(obj))                                   \
                               if (!in_old_generation(obj,,(as_oint(obj)>>1)&1))               \
                                 # obj ist ein Pointer in die neue Generation -> merken        \
                                 { cache_ptr->p = &(obj); cache_ptr->o = (obj); cache_ptr++; } \
                           }
                       #endif
                       walk_physpage(heapnr,physpage,gen0_start+physpagesize,gen0_end,cache_at);
                       #undef cache_at
                      {var uintL cache_size = cache_ptr - &cache_buffer[0];
                       if (cache_size <= (physpagesize/sizeof(object))/4)
                         # Wir cachen eine Seite nur, falls maximal 25% mit Pointern auf
                         # die neue Generation belegt ist. Sonst ist das Anlegen eines Cache
                         # Platzverschwendung.
                         { physpage->cache_size = cache_size;
                           if (cache_size == 0)
                             { xfree(physpage->cache); physpage->cache = NULL; }
                             else
                             { physpage->cache = (old_new_pointer*) xrealloc(physpage->cache,cache_size*sizeof(old_new_pointer));
                               if (physpage->cache == NULL)
                                 goto no_cache;
                               { var old_new_pointer* ptr1 = &cache_buffer[0];
                                 var old_new_pointer* ptr2 = physpage->cache;
                                 dotimespL(cache_size,cache_size, { *ptr2++ = *ptr1++; } );
                             } }
                           xmmprotect(heap,gen0_start,physpagesize,PROT_READ);
                           physpage->protection = PROT_READ;
                         }
                         else
                         { xfree(physpage->cache); physpage->cache = NULL;
                           no_cache: ;
                         }
                       FREE_DYNAMIC_ARRAY(cache_buffer);
                     }}
                   gen0_start += physpagesize;
                   physpage++;
                 }
                 while (gen0_start < gen0_end);
    }   }   }

#endif

#if defined(DEBUG_SPVW) && defined(GENERATIONAL_GC)
  # Kontrolle des Cache der old_new_pointer:
  #define CHECK_GC_CACHE()  gc_cache_check()
  local void gc_cache_check (void);
  local void gc_cache_check()
    { var uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++)
        if (is_heap_containing_objects(heapnr))
          { var Heap* heap = &mem.heaps[heapnr];
            var aint gen0_start = heap->heap_gen0_start;
            var aint gen0_end = heap->heap_gen0_end;
            var aint gen0_start_pa = gen0_start & -physpagesize; # page-aligned
            var aint gen0_end_pa = (gen0_end + (physpagesize-1)) & -physpagesize; # page-aligned
            var uintL physpage_count = (gen0_end_pa - gen0_start_pa) >> physpageshift;
            if (physpage_count > 0)
              { var physpage_state* physpage = heap->physpages;
                if (!(physpage==NULL))
                  { var uintL count;
                    dotimespL(count,physpage_count,
                      { var aint end = (gen0_start & -physpagesize) + physpagesize;
                        if (gen0_end < end) { end = gen0_end; }
                        if (physpage->firstobject < end) { end = physpage->firstobject; }
                        if (!(gen0_start <= (aint)physpage->continued_addr)) abort();
                        if (!((aint)physpage->continued_addr + physpage->continued_count*sizeof(object) <= end)) abort();
                        gen0_start &= -physpagesize;
                        gen0_start += physpagesize;
                        physpage++;
                      });
    }     }   }   }
  # Kontrolle, ob alle Pointer im Cache aufgeführt sind und nicht in den Wald zeigen.
  #define CHECK_GC_GENERATIONAL()  gc_overall_check()
  local void gc_overall_check (void);
    # Kontrolle eines einzelnen Pointers:
    local boolean gc_check_at (object* objptr);
    local boolean gc_check_at(objptr)
      var object* objptr;
      { var object obj = *objptr;
        var tint type = typecode(obj);
        #ifdef SPVW_PURE
        if (is_unused_heap(type))
          return FALSE;
        #else
        if (gcinvariant_type_p(type))
          return FALSE;
        #endif
       {var aint addr = canonaddr(obj);
        var Heap* heap;
        #ifdef SPVW_PURE
        heap = &mem.heaps[type];
        #else # SPVW_MIXED
        heap = &mem.heaps[mem.heapnr_from_type[type]];
        #endif
        if ((addr >= heap->heap_gen0_start) && (addr < heap->heap_gen0_end))
          return FALSE;
        #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
        if (is_cons_heap(mem.heapnr_from_type[type]))
          { if ((addr >= heap->heap_start) && (addr < heap->heap_gen1_end))
              return TRUE; # Pointer in die neue Generation
          }
          else
        #endif
          { if ((addr >= heap->heap_gen1_start) && (addr < heap->heap_end))
              return TRUE; # Pointer in die neue Generation
          }
        if ((type == symbol_type)
            && (as_oint(obj) - as_oint(symbol_tab_ptr_as_object(&symbol_tab))
                < (sizeof(symbol_tab)<<(oint_addr_shift-addr_shift))
           )   )
          return FALSE;
        abort();
      }}
  local void gc_overall_check()
    { var uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++)
        if (is_heap_containing_objects(heapnr))
          { var Heap* heap = &mem.heaps[heapnr];
            var aint gen0_start = heap->heap_gen0_start;
            var aint gen0_end = heap->heap_gen0_end;
            if (gen0_start < gen0_end)
              if (heap->physpages==NULL)
                { walk_area_(heapnr,gen0_start,gen0_end,gc_check_at); } # fallback
                else
                { var physpage_state* physpage = heap->physpages;
                  gen0_start &= -physpagesize;
                  do { if (physpage->protection == PROT_READ)
                         # Stimmen die Pointer im Cache und in der Seite überein?
                         { var uintL count = physpage->cache_size;
                           if (count > 0)
                             { var old_new_pointer* ptr = physpage->cache;
                               var aint last_p = gen0_start-1;
                               dotimespL(count,count,
                                 { if (!eq(*(ptr->p),ptr->o))
                                     abort();
                                   if (!(last_p < (aint)ptr->p))
                                     abort();
                                   last_p = (aint)ptr->p;
                                   ptr++;
                                 });
                         }   }
                       gen0_start += physpagesize;
                       if (physpage->protection == PROT_NONE)
                         # Cache ausnutzen, gecachte Pointer durchlaufen:
                         { var uintL count = physpage->cache_size;
                           if (count > 0)
                             { var old_new_pointer* ptr = physpage->cache;
                               dotimespL(count,count, { gc_check_at(&ptr->o); ptr++; } );
                         }   }
                         else
                         # ganzen Page-Inhalt durchlaufen:
                         { walk_physpage_(heapnr,physpage,gen0_start,gen0_end,gc_check_at); }
                       physpage++;
                     }
                     while (gen0_start < gen0_end);
    }     }     }
  # Zur Fehlersuche: Verwaltungsdaten vor und nach der GC retten.
  #define SAVE_GC_DATA()  save_gc_data()
  local void save_gc_data (void);
  typedef struct gc_data { struct gc_data * next; Heap heaps[heapcount]; } *
          gc_data_list;
  local var gc_data_list gc_history;
  local void save_gc_data()
    { # Kopiere die aktuellen GC-Daten an den Kopf der Liste gc_history :
      var gc_data_list new_data = (struct gc_data *) malloc(sizeof(struct gc_data));
      if (!(new_data==NULL))
        { var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { var Heap* heap = &new_data->heaps[heapnr];
              *heap = mem.heaps[heapnr];
              if (!(heap->physpages==NULL))
                { var uintL physpagecount =
                    (((heap->heap_gen0_end + (physpagesize-1)) & -physpagesize)
                     - (heap->heap_gen0_start & -physpagesize)
                    ) >> physpageshift;
                  var physpage_state* physpages = NULL;
                  if (physpagecount > 0)
                    physpages = (physpage_state*) malloc(physpagecount*sizeof(physpage_state));
                  if (!(physpages==NULL))
                    { var uintL i;
                      for (i=0; i<physpagecount; i++)
                        { physpages[i] = heap->physpages[i];
                          if (!(physpages[i].cache==NULL))
                            { var uintC cache_size = physpages[i].cache_size;
                              if (cache_size > 0)
                                { var old_new_pointer* cache = (old_new_pointer*) malloc(cache_size*sizeof(old_new_pointer));
                                  if (!(cache==NULL))
                                    { var old_new_pointer* old_cache = physpages[i].cache;
                                      var uintC j;
                                      for (j=0; j<cache_size; j++)
                                        { cache[j] = old_cache[j]; }
                                    }
                                  physpages[i].cache = cache;
                    }   }   }   }
                  heap->physpages = physpages;
            }   }
          new_data->next = gc_history;
          gc_history = new_data;
    }   }
#else
  #define CHECK_GC_CACHE()
  #define CHECK_GC_GENERATIONAL()
  #define SAVE_GC_DATA()
#endif

#if defined(DEBUG_SPVW) && !defined(GENERATIONAL_GC)
  # Kontrolle, ob auch alles unmarkiert ist:
  #define CHECK_GC_UNMARKED()  gc_unmarkcheck()
  local void gc_unmarkcheck (void);
  local void gc_unmarkcheck()
    { for_each_varobject_page(page,
        # Von unten nach oben durchgehen:
        { var aint p1 = page->page_start;
          var aint p1end = page->page_end;
          var_prepare_objsize;
          until (p1==p1end) # obere Grenze erreicht -> fertig
            { # nächstes Objekt hat Adresse p1
              if (marked(p1)) # markiert?
                { asciz_out_1("\nObjekt 0x%x markiert!!\n",p1);
                  abort();
                }
              p1 += objsize((Varobject)p1);
        }   }
        );
      for_each_cons_page(page,
        # Von unten nach oben durchgehen:
        { var aint p1 = page->page_start;
          var aint p1end = page->page_end;
          until (p1==p1end) # obere Grenze erreicht -> fertig
            { # nächstes Objekt hat Adresse p1
              if (marked(p1)) # markiert?
                { asciz_out_1("\nObjekt 0x%x markiert!!\n",p1);
                  abort();
                }
              p1 += sizeof(cons_);
        }   }
        );
    }
#else
  #define CHECK_GC_UNMARKED()
#endif

#ifdef DEBUG_SPVW
  # Kontrolle gegen Nullpointer:
  #define CHECK_NULLOBJ()  nullobjcheck(FALSE)
  local void nullobjcheck (boolean in_gc);
  local void nullobjcheck_range (aint p1, aint p1end, boolean in_gc);
  local void nullobjcheck_range(p1,p1end,in_gc)
    var aint p1;
    var aint p1end;
    var boolean in_gc;
    { until (p1==p1end) # obere Grenze erreicht -> fertig
        { # nächstes Objekt hat Adresse p1
          if (eq(((Cons)p1)->cdr,nullobj) || eq(((Cons)p1)->car,nullobj))
            if (!(in_gc && eq(((Cons)p1)->cdr,nullobj) && eq(((Cons)p1)->car,nullobj)))
              abort();
          p1 += sizeof(cons_);
    }   }
  local void nullobjcheck(in_gc)
    var boolean in_gc;
    { # Von unten nach oben durchgehen:
      #ifdef GENERATIONAL_GC
      #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
      for_each_cons_heap(heap,
        { nullobjcheck_range(heap->heap_start,heap->heap_gen1_end,in_gc);
          nullobjcheck_range(heap->heap_gen0_start,heap->heap_gen0_end,in_gc);
        });
      #else
      for_each_cons_heap(heap,
        { nullobjcheck_range(heap->heap_gen0_start,heap->heap_gen0_end,in_gc);
          nullobjcheck_range(heap->heap_gen1_start,heap->heap_end,in_gc);
        });
      #endif
      #else
      for_each_cons_page(page,
        { nullobjcheck_range(page->page_start,page->page_end,in_gc); });
      #endif
    }
#else
  #define CHECK_NULLOBJ()
#endif

#ifdef SPVW_PAGES
  # Überflüssige Pages freigeben:
  # Falls nach einer GC der Platz, der uns in mem.free_pages zur Verfügung
  # steht, mehr als 25% dessen ausmacht, was wir momentan brauchen, wird der
  # Rest ans Betriebssystem zurückgegeben.
  local void free_some_unused_pages (void);
  local void free_some_unused_pages()
    { var uintL needed_space = floor(mem.last_gcend_space,4); # 25%
      var uintL accu_space = 0;
      var Pages* pageptr = &mem.free_pages;
      var Pages page = *pageptr;
      until (page==NULL)
        { var Pages nextpage = page->page_gcpriv.next;
          if (accu_space < needed_space)
            # page behalten
            { accu_space += page->page_room;
              pageptr = (Pages*)&page->page_gcpriv.next; page = nextpage;
            }
            else
            # page freigeben
            { free_page(page); page = *pageptr = nextpage; }
    }   }
#endif

# GC-Timer ein- und ausschalten: gc_timer_on(); ... gc_timer_off();
# Die dazwischen verstrichene Zeit wird auf gc_time addiert.
  #define gc_timer_on()  \
    { var internal_time gcstart_time; \
      get_running_time(gcstart_time); # aktuelle verbrauchte Zeit abfragen und retten
  #define gc_timer_off()  \
     {var internal_time gcend_time;                           \
      get_running_time(gcend_time);                           \
      # Differenz von gcend_time und gcstart_time bilden:     \
      sub_internal_time(gcend_time,gcstart_time, gcend_time); \
      # diese Differenz zu gc_time addieren:                  \
      add_internal_time(gc_time,gcend_time, gc_time);         \
    }}

# GC-bedingt Signale disablen: gc_signalblock_on(); ... gc_signalblock_off();
  #if defined(HAVE_SIGNALS) && defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
    # Signal SIGWINCH blockieren, denn eine Veränderung des Wertes von
    # SYS::*PRIN-LINELENGTH* können wir während der GC nicht brauchen.
    # Dann Signal SIGWINCH wieder freigeben.
    #define gc_signalblock_on()  signalblock_on(SIGWINCH)
    #define gc_signalblock_off()  signalblock_off(SIGWINCH)
  #else
    #define gc_signalblock_on()
    #define gc_signalblock_off()
  #endif

# Normale Garbage Collection durchführen:
  local void gar_col_normal(void);
  local void gar_col_normal()
    { var uintL gcstart_space; # belegter Speicher bei GC-Start
      var uintL gcend_space; # belegter Speicher bei GC-Ende
      var object all_finalizers; # Liste der Finalisierer
      #ifdef GC_CLOSES_FILES
      var object files_to_close; # Liste der zu schließenden Files
      #endif
      set_break_sem_1(); # BREAK während Garbage Collection sperren
      gc_signalblock_on(); # Signale während Garbage Collection sperren
      gc_timer_on();
      gcstart_space = used_space(); # belegten Speicherplatz ermitteln
      #ifdef HAVE_VADVISE
        begin_system_call();
        vadvise(VA_ANOM); # Paging-Verhalten wird jetzt etwas ungewöhnlich
        end_system_call();
      #endif
      CHECK_GC_UNMARKED(); CHECK_NULLOBJ(); CHECK_GC_CACHE(); CHECK_GC_GENERATIONAL(); SAVE_GC_DATA();
      #ifdef SPVW_PAGES
        { var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { AVL_map(mem.heaps[heapnr].inuse,page,
                      page->page_room += page->page_end;
                     );
              # In page_room steht jetzt jeweils das Ende des benutzbaren Speichers.
        }   }
      #endif
      #ifdef GENERATIONAL_GC
      if (generation == 0)
        # Alte Generation mit Hilfe des Cache auf den aktuellen Stand bringen:
        { prepare_old_generation(); }
        else
        # Nur die neue Generation behandeln. Alte Generation verstecken:
        #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
        { mem.varobjects.heap_start = mem.varobjects.heap_gen1_start;
          mem.conses.heap_end = mem.conses.heap_gen1_end;
        }
        #else
        { var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            mem.heaps[heapnr].heap_start = mem.heaps[heapnr].heap_gen1_start;
        }
        #endif
      #endif
      CHECK_GC_GENERATIONAL();
      # Markierungsphase:
        all_finalizers = O(all_finalizers); O(all_finalizers) = NIL;
        #ifdef GC_CLOSES_FILES
        files_to_close = O(open_files); O(open_files) = NIL; # O(files_to_close) = NIL;
        #endif
        gc_markphase();
        # (noch unmarkierte Liste all_finalizers aufspalten in zwei Listen:
        { var object Lu = all_finalizers;
          var object* L1 = &O(all_finalizers);
          var object* L2 = &O(pending_finalizers);
          until (symbolp(*L2)) # eigentlich: until (nullp(*L2))
            { L2 = &TheFinalizer(*L2)->fin_cdr; }
          until (symbolp(Lu)) # eigentlich: until (nullp(Lu))
            { # Wenn fin_alive tot ist, wird der Finalisierer weggeworfen,
              # ohne ausgeführt zu werden:
              if (!alive(TheFinalizer(Lu)->fin_alive))
                { Lu = TheFinalizer(Lu)->fin_cdr; }
                else
                { # Wenn fin_trigger stirbt, wird der Finalisierer ausgeführt:
                  if (alive(TheFinalizer(Lu)->fin_trigger)) # Lebt fin_trigger noch?
                    # ja -> in O(all_finalizers) übernehmen:
                    { *L1 = Lu; L1 = &TheFinalizer(Lu)->fin_cdr; Lu = *L1; }
                    else
                    # nein -> in O(pending_finalizers) übernehmen:
                    { *L2 = Lu; L2 = &TheFinalizer(Lu)->fin_cdr; Lu = *L2; }
                }
            }
          *L1 = NIL; *L2 = NIL;
        }
        gc_mark(O(all_finalizers)); gc_mark(O(pending_finalizers)); # Beide Listen jetzt markieren
        #ifdef GC_CLOSES_FILES
        # (noch unmarkierte) Liste files_to_close aufspalten in zwei Listen:
        { var object Lu = files_to_close;
          var object* L1 = &O(open_files);
          var object* L2 = &O(files_to_close);
          while (consp(Lu))
            { if (in_old_generation(Car(Lu),stream_type,0)
                  || marked(TheStream(Car(Lu))) # (car Lu) markiert?
                 )
                # ja -> in O(open_files) übernehmen:
                { *L1 = Lu; L1 = &Cdr(Lu); Lu = *L1; }
                else
                # nein -> in O(files_to_close) übernehmen:
                { *L2 = Lu; L2 = &Cdr(Lu); Lu = *L2; }
            }
          *L1 = NIL; *L2 = NIL;
        }
        gc_mark(O(open_files)); gc_mark(O(files_to_close)); # Beide Listen jetzt markieren
        #endif
      # Jetzt sind alle aktiven Objekte markiert:
      # Aktive Objekte variabler Länge wie auch aktive Zwei-Pointer-Objekte tragen
      # in ihrem ersten Byte ein gesetztes Markierungsbit, aktive SUBRs tragen
      # in ihrem ersten Konstantenpointer ein gesetztes Markierungsbit, sonst sind
      # alle Markierungsbits gelöscht.
      # "Sweep"-Phase:
        # Die CONSes u.ä. (Objekte mit 2 Pointern) werden kompaktiert.
        # Von den Objekten variabler Länge werden die Zielplätze für die
        # Phase 4 errechnet und abgespeichert.
        # SUBRs und feste Symbole (sie sind alle aktiv) werden als erstes demarkiert:
          unmark_fixed_varobjects();
        #ifndef MORRIS_GC
        # CONS-Zellen kompaktieren:
          for_each_cons_page(page, { gc_compact_cons_page(page); } );
        #endif
        # Objekte variabler Länge zur Zusammenschiebung nach unten vorbereiten:
          #ifdef SPVW_PURE
          #ifdef GENERATIONAL_GC
          if (generation == 0)
            { for_each_varobject_heap(heap,
                { if (heap->heap_gen0_end < heap->heap_gen1_start)
                    # Lücke durch einen Pointer überspringen
                    { var aint tmp =
                        gc_sweep1_varobject_page(heapnr,
                                                 heap->heap_gen0_start,heap->heap_gen0_end,
                                                 &heap->pages.page_gcpriv.firstmarked,
                                                 heap->heap_gen0_start);
                      var aint gen0_end = heap->heap_gen0_end;
                      heap->heap_gen0_end = heap->heap_gen1_start; # temporary - for handle_fault() if SELFMADE_MMAP
                      gc_sweep1_varobject_page(heapnr,
                                               heap->heap_gen1_start,heap->heap_end,
                                               (object*)gen0_end,
                                               tmp);
                      heap->heap_gen0_end = gen0_end; # temporary - end
                    }
                    else
                    # keine Lücke
                    { gc_sweep1_varobject_page(heapnr,
                                               heap->heap_gen0_start,heap->heap_end,
                                               &heap->pages.page_gcpriv.firstmarked,
                                               heap->heap_gen0_start);
                    }
                });
            }
            else
          #endif
          for_each_varobject_page(page,
            { gc_sweep1_varobject_page(heapnr,
                                       page->page_start,page->page_end,
                                       &page->page_gcpriv.firstmarked,
                                       page->page_start);
            });
          #else # SPVW_MIXED
          #ifdef GENERATIONAL_GC
          if (generation == 0)
            { for_each_varobject_heap(heap,
                { if (heap->heap_gen0_end < heap->heap_gen1_start)
                    # Lücke durch einen Pointer überspringen
                    { var aint tmp =
                        gc_sweep1_varobject_page(heap->heap_gen0_start,heap->heap_gen0_end,
                                                 &heap->pages.page_gcpriv.firstmarked,
                                                 heap->heap_gen0_start);
                      gc_sweep1_varobject_page(heap->heap_gen1_start,heap->heap_end,
                                               (object*)(heap->heap_gen0_end),
                                               tmp);
                    }
                    else
                    # keine Lücke
                    { gc_sweep1_varobject_page(heap->heap_gen0_start,heap->heap_end,
                                               &heap->pages.page_gcpriv.firstmarked,
                                               heap->heap_gen0_start);
                    }
                });
            }
            else
            for_each_varobject_page(page,
              { gc_sweep1_varobject_page(page->page_start,page->page_end,
                                         &page->page_gcpriv.firstmarked,
                                         page->page_start);
              });
          #else
          for_each_varobject_page(page, { gc_sweep1_varobject_page(page); } );
          #endif
          #endif
      # Jetzt sind alle aktiven Objekte für die Aktualisierung vorbereitet:
      # Bei aktiven Objekten variabler Länge A2 ist (A2).L die Adresse, wo das
      # Objekt nach der GC stehen wird (incl. Typinfo und Markierungsbit und evtl.
      # Symbol-Flags). Bei aktiven Zwei-Pointer-Objekten A2 bleibt entweder A2
      # stehen (dann ist das Markierungsbit in (A2) gelöscht), oder A2 wird
      # verschoben (dann ist (A2).L die neue Adresse, ohne Typinfo, aber incl.
      # Markierungsbit).
      # Aktualisierungsphase:
        # Der gesamte LISP-Speicher wird durchgegangen und dabei alte durch
        # neue Adressen ersetzt.
        #ifdef MORRIS_GC
        for_each_cons_page(page, { gc_morris1(page); } );
        #endif
        # Durchlaufen durch alle LISP-Objekte und aktualisieren:
          # Update pointers in all LISP-stacks:
            update_STACKs();
            #undef update_stackobj
          # Programmkonstanten aktualisieren:
            update_tables();
          #ifndef MORRIS_GC
          # Pointer in den Cons-Zellen aktualisieren:
            #define update_conspage  update_conspage_normal
            update_conses();
            #undef update_conspage
          #endif
          # Pointer in den Objekten variabler Länge aktualisieren:
            #define update_page(page,updater)  \
              { var aint ptr = (aint)pointer_was_object(page->page_gcpriv.firstmarked); \
                var aint ptrend = page->page_end;                              \
                # alle Objekte mit Adresse >=ptr, <ptrend durchgehen:          \
                until (ptr==ptrend) # solange bis ptr am Ende angekommen ist   \
                  { # nächstes Objekt mit Adresse ptr (< ptrend) durchgehen:   \
                    if (marked(ptr)) # markiert?                               \
                      # Typinfo ohne Markierungsbit nehmen!                    \
                      { updater(typecode_at(ptr) & ~bit(garcol_bit_t)); }      \
                      else                                                     \
                      # mit Pointer (Typinfo=0) zum nächsten markierten Objekt \
                      { ptr = (aint)pointer_was_object(*(object*)ptr); }        \
              }   }
            #define update_fpointer_invalid  FALSE
            #define update_fsubr_function FALSE
            #define update_ht_invalid  mark_ht_invalid
            #define update_fp_invalid  mark_fp_invalid
            #define update_fs_function(ptr)
            update_varobjects();
            #undef update_fs_function
            #undef update_fp_invalid
            #undef update_ht_invalid
            #undef update_fsubr_function
            #undef update_fpointer_invalid
            #undef update_page
          #ifdef GENERATIONAL_GC
          # Pointer in den Objekten der alten Generation aktualisieren:
            if (generation > 0)
              { update_old_generation(); }
          #endif
        #ifdef MORRIS_GC
        # Zum Schluß werden die Conses verschoben und gleichzeitig alle
        # Pointer auf sie (z.Zt. in Listen geführt!) aktualisiert.
        for_each_cons_page_reversed(page, { gc_morris2(page); } );
        for_each_cons_page(page, { gc_morris3(page); } );
        #endif
      # Jetzt sind alle aktiven Objekte mit korrektem Inhalt versehen (alle darin
      # vorkommenden Pointer zeigen auf die nach der GC korrekten Adressen).
      # Die aktiven Zwei-Pointer-Objekte sind bereits am richtigen Ort und
      # unmarkiert; die Objekte variabler Länge sind noch am alten Ort und
      # markiert, falls aktiv.
      # Zweite SWEEP-Phase:
        # Die Objekte variabler Länge werden an die vorher berechneten
        # neuen Plätze geschoben.
        #if !defined(GENERATIONAL_GC)
        #ifdef SPVW_MIXED
        for_each_varobject_page(page, { gc_sweep2_varobject_page(page); } );
        #else # SPVW_PURE
        for_each_varobject_page(page, { gc_sweep2_varobject_page(page,heapnr); } );
        #endif
        #else # defined(GENERATIONAL_GC)
        { var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { var Heap* heap = &mem.heaps[heapnr];
              if (!is_unused_heap(heapnr))
                { if (is_varobject_heap(heapnr))
                    {
                      #ifdef SPVW_MIXED
                      gc_sweep2_varobject_page(&heap->pages);
                      #else # SPVW_PURE
                      gc_sweep2_varobject_page(&heap->pages,heapnr);
                      #endif
                    }
                  if (generation == 0)
                    { # Alles Übriggebliebene bildet die neue Generation 0.
                      #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
                      if (is_cons_heap(heapnr))
                        { var aint start = heap->heap_start;
                          heap->heap_gen0_start = start;
                          start = start & -physpagesize;
                          heap->heap_start = heap->heap_gen1_end = start;
                        }
                        else
                      #endif
                        { var aint end = heap->heap_end;
                          heap->heap_gen0_end = end;
                          end = (end + (physpagesize-1)) & -physpagesize;
                          heap->heap_gen1_start = heap->heap_end = end;
                        }
                      build_old_generation_cache(heapnr);
                    }
                    else
                    { rebuild_old_generation_cache(heapnr); }
                }
              #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
              if (is_cons_heap(heapnr))
                { heap->heap_end = heap->heap_gen0_end; }
                else
              #endif
                { heap->heap_start = heap->heap_gen0_start; }
        }   }
        #endif
      # Jetzt sind alle aktiven Objekte mit korrektem Inhalt versehen, am richtigen
      # Ort und wieder unmarkiert.
      #ifdef SPVW_PAGES
        { var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { var Pages* heapptr = &mem.heaps[heapnr].inuse;
              AVL_map(*heapptr,page,
                      page->page_room -= page->page_end;
                     );
              # In page_room steht jetzt jeweils wieder der verfügbare Platz.
              # Pages wieder nach dem verfügbaren Platz sortieren:
              *heapptr = AVL(AVLID,sort)(*heapptr);
        }   }
        for_each_cons_heap(heap, { heap->lastused = dummy_lastused; } );
        # .reserve behandeln??
      #endif
      CHECK_AVL_CONSISTENCY();
      CHECK_GC_CONSISTENCY();
      CHECK_GC_UNMARKED(); CHECK_NULLOBJ(); CHECK_GC_CACHE(); CHECK_GC_GENERATIONAL(); SAVE_GC_DATA();
      CHECK_PACK_CONSISTENCY();
      # Ende der Garbage Collection.
      #ifdef HAVE_VADVISE
        begin_system_call();
        vadvise(VA_NORM); # Paging-Verhalten wird ab jetzt wieder normal
        end_system_call();
      #endif
      gc_count += 1; # GCs mitzählen
      # belegten Speicherplatz ermitteln:
      #ifdef SPVW_PAGES
      recalc_space(FALSE);
      #endif
      gcend_space = used_space();
      #ifdef SPVW_PAGES
      mem.last_gcend_space = gcend_space;
      # Um bis zu 25% lassen wir den benutzten Platz anwachsen, dann erst
      # kommt die nächste GC:
      { var uintL total_room = floor(mem.last_gcend_space,4);
        if (total_room < 512*1024) { total_room = 512*1024; } # mindestens 512 KB
        mem.gctrigger_space = mem.last_gcend_space + total_room;
      }
      #endif
      #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
      # make_space() erwartet, daß mem.total_room <= Länge der großen Lücke.
      #define set_total_room(space_used_now)  \
        { set_total_room_(space_used_now);                                      \
          if (mem.total_room > mem.conses.heap_start-mem.varobjects.heap_end)   \
            { mem.total_room = mem.conses.heap_start-mem.varobjects.heap_end; } \
        }
      #else
      #define set_total_room  set_total_room_
      #endif
      #if (defined(SPVW_PURE_BLOCKS) || defined(TRIVIALMAP_MEMORY)) && !defined(GENERATIONAL_GC)
      # Um bis zu 50% lassen wir den benutzten Platz anwachsen, dann erst
      # kommt die nächste GC:
      #define set_total_room_(space_used_now)  \
        { mem.total_room = floor(space_used_now,2); # 50% des jetzt benutzten Platzes       \
          if (mem.total_room < 512*1024) { mem.total_room = 512*1024; } # mindestens 512 KB \
        }
      set_total_room(gcend_space);
      #endif
      #if defined(GENERATIONAL_GC)
      # Um bis zu 25% lassen wir den benutzten Platz anwachsen, dann erst
      # kommt die nächste GC:
      #define set_total_room_(space_used_now)  \
        { mem.total_room = floor(space_used_now,4); # 25% des jetzt benutzten Platzes       \
          if (mem.total_room < 512*1024) { mem.total_room = 512*1024; } # mindestens 512 KB \
        }
      { var uintL gen0_sum = 0; # momentane Größe der alten Generation
        var uintL gen1_sum = 0; # momentane Größe der neuen Generation
        for_each_heap(heap,
          { gen0_sum += heap->heap_gen0_end - heap->heap_gen0_start; });
        #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
        gen1_sum += mem.varobjects.heap_end - mem.varobjects.heap_gen1_start;
        gen1_sum += mem.conses.heap_gen1_end - mem.conses.heap_start;
        #else
        for_each_heap(heap,
          { gen1_sum += heap->heap_end - heap->heap_gen1_start; });
        #endif
        # NB: gcend_space == gen0_sum + gen1_sum.
        set_total_room(gen0_sum);
        mem.last_gcend_space0 = gen0_sum;
        mem.last_gcend_space1 = gen1_sum;
      }
      #endif
      { var uintL freed = gcstart_space - gcend_space; # von dieser GC
                                       # wiederbeschaffter Speicherplatz
        # dies zum 64-Bit-Akku gc_space addieren:
        #ifdef intQsize
        gc_space += freed;
        #else
        gc_space.lo += freed;
        if (gc_space.lo < freed) # Übertrag?
          gc_space.hi += 1;
        #endif
      }
      #ifdef SPVW_PAGES
      free_some_unused_pages();
      #endif
      #if (defined(SINGLEMAP_MEMORY) || defined(TRIVIALMAP_MEMORY)) && defined(VIRTUAL_MEMORY) && defined(HAVE_MUNMAP)
      # Ungebrauchte, leere Seiten freigeben, damit sie vom Betriebssystem
      # nicht irgendwann auf den Swapspace verbracht werden müssen:
        begin_system_call();
        #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
        for_each_heap(heap,
          { var aint needed_limit = round_up(heap->heap_end,map_pagesize);
            if (needed_limit > heap->heap_limit)
              abort();
            if (needed_limit < heap->heap_limit)
              { if (munmap((MMAP_ADDR_T)needed_limit,heap->heap_limit-needed_limit) < 0)
                  goto munmap_failure;
                heap->heap_limit = needed_limit;
          }   });
        #else # SPVW_MIXED_BLOCKS_OPPOSITE
        for_each_heap(heap,
          if (is_cons_heap(heapnr))
            { var aint needed_limit = round_down(heap->heap_start,map_pagesize);
              if (needed_limit < heap->heap_limit)
                abort();
              if (needed_limit > heap->heap_limit)
                { if (munmap((MMAP_ADDR_T)heap->heap_limit,needed_limit-heap->heap_limit) < 0)
                    goto munmap_failure;
                  heap->heap_limit = needed_limit;
            }   }
            else
            { var aint needed_limit = round_up(heap->heap_end,map_pagesize);
              if (needed_limit > heap->heap_limit)
                abort();
              if (needed_limit < heap->heap_limit)
                { if (munmap((MMAP_ADDR_T)needed_limit,heap->heap_limit-needed_limit) < 0)
                    goto munmap_failure;
                  heap->heap_limit = needed_limit;
            }   }
          );
        #endif
        if (FALSE)
          munmap_failure:
          { end_system_call();
            asciz_out(DEUTSCH ? "munmap() klappt nicht." :
                      ENGLISH ? "munmap() fails." :
                      FRANCAIS ? "munmap() ne fonctionne pas." :
                      ""
                     );
            errno_out(OS_errno);
            abort();
          }
        end_system_call();
      #endif
      # von dieser GC benötigte Zeit zur GC-Gesamtzeit addieren:
      gc_timer_off();
      #ifdef GC_CLOSES_FILES
      close_some_files(O(files_to_close)); # vorher unmarkierte Files schließen
      O(files_to_close) = NIL;
      #endif
      #ifdef GENERATIONAL_GC
      O(gc_count) = fixnum_inc(O(gc_count),1); # GCs mitzählen
      #endif
      gc_signalblock_off(); # Signale wieder freigeben
      clr_break_sem_1(); # BREAK wieder ermöglichen
    }

# Ende einer Garbage Collection.
# kann GC auslösen!
  local void gar_col_done (void);
  local void gar_col_done()
    { # Finalisierer-Funktionen abarbeiten:
      until (symbolp(O(pending_finalizers))) # eigentlich: until (nullp(...))
        { var object obj = O(pending_finalizers);
          O(pending_finalizers) = TheFinalizer(obj)->fin_cdr;
          pushSTACK(TheFinalizer(obj)->fin_trigger);
          if (eq(TheFinalizer(obj)->fin_alive,unbound))
            { funcall(TheFinalizer(obj)->fin_function,1); } # (FUNCALL function trigger)
            else
            { pushSTACK(TheFinalizer(obj)->fin_alive);
              funcall(TheFinalizer(obj)->fin_function,2); # (FUNCALL function trigger alive)
        }   }
    }

#ifdef SPVW_PAGES

# Eine kleine Sortier-Routine:
#define SORTID  spvw
#define SORT_ELEMENT  Pages
#define SORT_KEY  uintL
#define SORT_KEYOF(page)  (page)->page_gcpriv.l
#define SORT_COMPARE(key1,key2)  (sintL)((key1)-(key2))
#define SORT_LESS(key1,key2)  ((key1) < (key2))
#include "sort.c"

# Liste von Pages, die freizugeben sind, sobald die Aktualisierung
# abgeschlossen ist:
  local var Page* delayed_pages = NULL;
# Einfügen einer Page in diese Liste:
  #define free_page_later(page)  \
    { (page)->page_gcpriv.next = delayed_pages; delayed_pages = page; }
# Freigeben aller Pages in der Liste:
  #define free_delayed_pages()  \
    { var Page* page = delayed_pages;                     \
      until (page==NULL)                                  \
        { var Page* next = (Page*)page->page_gcpriv.next; \
          free_page(page);                                \
          page = next;                                    \
        }                                                 \
      delayed_pages = NULL;                               \
    }

# Kompaktierung einer Page durch Umfüllen in andere Pages derselben Art:
  #ifdef SPVW_PURE
  local void gc_compact_from_varobject_page (Heap* heapptr, Page* page, uintL heapnr);
  local void gc_compact_from_varobject_page(heapptr,page,heapnr)
    var Heap* heapptr;
    var Page* page;
    var uintL heapnr;
  #else
  local void gc_compact_from_varobject_page (Heap* heapptr, Page* page);
  local void gc_compact_from_varobject_page(heapptr,page)
    var Heap* heapptr;
    var Page* page;
  #endif
    { var aint p1 = page->page_start;
      var aint p1end = page->page_end;
      var_prepare_objsize;
     {var Pages new_page = EMPTY; # Page, in die gefüllt wird
      var AVL(AVLID,stack) stack; # Weg von der Wurzel bis zu ihr
      var aint p2; # Cache von new_page->page_end
      var uintL l2; # Cache von new_page->page_room
      # Versuche alle Objekte zwischen p1 und p1end zu kopieren:
      loop
        { if (p1==p1end) break; # obere Grenze erreicht -> fertig
         {var uintL laenge = objsize((Varobject)p1); # Byte-Länge bestimmen
          # Suche eine Page, die noch mindestens laenge Bytes frei hat:
          if ((new_page == EMPTY) || (l2 < laenge))
            { if (!(new_page == EMPTY)) # Cache leeren?
                { new_page->page_end = p2;
                  new_page->page_room = l2;
                  AVL(AVLID,move)(&stack);
                }
              new_page = AVL(AVLID,least)(laenge,&heapptr->inuse,&stack);
              if (new_page==EMPTY) break;
              new_page->page_gcpriv.d = -1L; # new_page als "zu füllend" kennzeichnen
              p2 = new_page->page_end;
              l2 = new_page->page_room;
            }
          {var aint old_p1 = p1;
           var aint old_p2 = p2;
           # Kopiere das Objekt:
           l2 -= laenge; move_aligned_p1_p2(laenge);
           # Hinterlasse einen Pointer auf die neue Position:
           *(object*)old_p1 = with_mark_bit(pointer_as_object(old_p2));
           # p1 = Sourceadresse für nächstes Objekt
        }}}
      if (!(new_page == EMPTY)) # Cache leeren?
        { new_page->page_end = p2;
          new_page->page_room = l2;
          AVL(AVLID,move)(&stack);
        }
     }
     # Die nicht kopierten Objekte erfahren eine konstante Verschiebung nach unten:
     {var aint p2 = page->page_start;
      page->page_gcpriv.d = p1 - p2; # Verschiebung
      page->page_start = p1; # jetziger Anfang der Page
      if (!(p1==p2)) # falls Verschiebung nötig
        until (p1==p1end) # obere Grenze erreicht -> fertig
          { var uintL laenge = objsize((Varobject)p1); # Byte-Länge bestimmen
            #ifdef TYPECODES
            var tint flags = mtypecode(((Varobject)p1)->GCself); # Typinfo (und Flags bei Symbolen) retten
            #endif
            set_GCself(p1, flags,p2); # neue Adresse eintragen, mit alter Typinfo
            mark(p1); # mit Markierungsbit
            p1 += laenge; p2 += laenge;
          }
    }}
  local void gc_compact_from_cons_page (Heap* heapptr, Page* page);
  local void gc_compact_from_cons_page(heapptr,page)
    var Heap* heapptr;
    var Page* page;
    { var aint p1 = page->page_end;
      var aint p1start = page->page_start;
     {var Pages new_page = EMPTY; # Page, in die gefüllt wird
      var AVL(AVLID,stack) stack; # Weg von der Wurzel bis zu ihr
      var aint p2; # Cache von new_page->page_end
      var uintL l2; # Cache von new_page->page_room
      # Versuche alle Objekte zwischen p1start und p1 zu kopieren:
      loop
        { if (p1==p1start) break; # untere Grenze erreicht -> fertig
          # Suche eine Page, die noch mindestens sizeof(cons_) Bytes frei hat:
          if ((new_page == EMPTY) || (l2 == 0)) # l2 < sizeof(cons_) bedeutet l2 = 0
            { if (!(new_page == EMPTY)) # Cache leeren?
                { new_page->page_end = p2;
                  new_page->page_room = l2;
                  AVL(AVLID,move)(&stack);
                }
              new_page = AVL(AVLID,least)(sizeof(cons_),&heapptr->inuse,&stack);
              if (new_page==EMPTY) break;
              new_page->page_gcpriv.d = -1L; # new_page als "zu füllend" kennzeichnen
              p2 = new_page->page_end;
              l2 = new_page->page_room;
            }
          p1 -= sizeof(cons_); # p1 = Sourceadresse für nächstes Objekt
          # Kopiere das Objekt:
          ((object*)p2)[0] = ((object*)p1)[0];
          ((object*)p2)[1] = ((object*)p1)[1];
          # Hinterlasse einen Pointer auf die neue Position:
          *(object*)p1 = with_mark_bit(pointer_as_object(p2));
          p2 += sizeof(cons_); l2 -= sizeof(cons_);
        }
      if (!(new_page == EMPTY)) # Cache leeren?
        { new_page->page_end = p2;
          new_page->page_room = l2;
          AVL(AVLID,move)(&stack);
        }
     }
     # Die nicht kopierten Objekte bleiben an Ort und Stelle.
     page->page_gcpriv.d = page->page_end - p1; # Zugewinn
     page->page_end = p1; # jetziges Ende der Page
    }

# Kompaktierung aller Pages einer bestimmten Art:
  #ifdef SPVW_PURE
  local void gc_compact_heap (Heap* heapptr, sintB heaptype, uintL heapnr);
  local void gc_compact_heap(heapptr,heaptype,heapnr)
    var Heap* heapptr;
    var sintB heaptype;
    var uintL heapnr;
  #else
  local void gc_compact_heap (Heap* heapptr, sintB heaptype);
  local void gc_compact_heap(heapptr,heaptype)
    var Heap* heapptr;
    var sintB heaptype;
  #endif
    { # Erst eine Liste aller Pages erstellen, aufsteigend sortiert
      # nach der Anzahl der belegten Bytes:
      var uintL pagecount = 0;
      map_heap(*heapptr,page,
               { page->page_gcpriv.l = page->page_end - page->page_start; # Anzahl der belegten Bytes
                 pagecount++;
               }
              );
      # pagecount = Anzahl der Pages.
     {var DYNAMIC_ARRAY(pages_sorted,Pages,pagecount);
      {var uintL index = 0;
       map_heap(*heapptr,page, { pages_sorted[index++] = page; } );
      }
      # pages_sorted = Array der Pages.
      SORT(SORTID,sort)(pages_sorted,pagecount);
      # pages_sorted = Array der Pages, sortiert nach der Anzahl der belegten Bytes.
      # In jeder Page bedeutet page_gcpriv.d die Verschiebung nach unten,
      # die der Page in Phase 3 zuteil werden muß (>=0).
      # page_gcpriv.d = -1L für die zu füllenden Pages.
      # page_gcpriv.d = -2L für die noch unbehandelten Pages.
      map_heap(*heapptr,page, { page->page_gcpriv.d = -2L; } ); # alle Pages noch unbehandelt
      {var uintL index;
       for (index=0; index<pagecount; index++) # Durch alle Pages durchlaufen
         { var Pages page = pages_sorted[index]; # nächste Page
           if (page->page_gcpriv.d == -2L) # noch unbehandelt und
                                           # noch nicht als "zu füllend" markiert?
             { # page wird geleert.
               heapptr->inuse = AVL(AVLID,delete1)(page,heapptr->inuse); # page herausnehmen
               # page leeren:
               if (heaptype==0)
                 { gc_compact_from_cons_page(heapptr,page); }
                 else
                 #ifdef SPVW_PURE
                 { gc_compact_from_varobject_page(heapptr,page,heapnr); }
                 #else
                 { gc_compact_from_varobject_page(heapptr,page); }
                 #endif
      }  }   }
      CHECK_AVL_CONSISTENCY();
      CHECK_GC_CONSISTENCY_2();
      {var uintL index;
       for (index=0; index<pagecount; index++) # Durch alle Pages durchlaufen
         { var Pages page = pages_sorted[index]; # nächste Page
           if (!(page->page_gcpriv.d == -1L)) # eine zu leerende Page
             { page->page_room += page->page_gcpriv.d; # So viel Platz haben wir nun gemacht
               if (page->page_start == page->page_end)
                 # Page ganz geleert
                 { # Page freigeben:
                   if (page->m_length > min_page_size_brutto)
                     # Übergroße Page
                     { free_page_later(page); } # später ans Betriebssystem zurückgeben
                     else
                     # Normalgroße Page
                     { # wieder initialisieren (page->page_room bleibt gleich!):
                       page->page_start = page->page_end = page_start0(page);
                       # in den Pool mem.free_pages einhängen:
                       page->page_gcpriv.next = mem.free_pages;
                       mem.free_pages = page;
                 }   }
                 else
                 # Page konnte nicht ganz geleert werden
                 { heapptr->inuse = AVL(AVLID,insert1)(page,heapptr->inuse); } # Page wieder rein
      }  }   }
      FREE_DYNAMIC_ARRAY(pages_sorted);
      CHECK_AVL_CONSISTENCY();
      CHECK_GC_CONSISTENCY_2();
    }}

# Kompaktierende Garbage Collection durchführen.
# Wird aufgerufen, nachdem gar_col_simple() nicht genügend Platz am Stück
# besorgen konnte.
  local void gar_col_compact (void);
  local void gar_col_compact()
    { # Es werden Lisp-Objekte von fast leeren Pages in andere Pages
      # umgefüllt, um die ganz leer machen und zurückgeben zu können.
      # 1. Für jede Page-Art:
      #    Pages unterteilen in zu leerende und zu füllende Pages und dabei
      #    soviel Daten wie möglich von den zu leerenden in die zu füllenden
      #    Pages umkopieren. Kann eine Page nicht ganz geleert werden, so
      #    wird sie so gelassen, wie sie ist, und in ihr werden dann nachher
      #    die übrigen Daten nur nach unten geschoben.
      #    Rückgabe der ganz geleerten Pages.
      # 2. Aktualisierung der Pointer.
      # 3. Durchführung der Verschiebungen in den nicht ganz geleerten Pages.
      set_break_sem_1(); # BREAK während Garbage Collection sperren
      gc_signalblock_on(); # Signale während Garbage Collection sperren
      gc_timer_on();
      CHECK_GC_UNMARKED(); CHECK_NULLOBJ();
      { var uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++)
          if (!is_unused_heap(heapnr))
            #ifdef SPVW_PURE
            { gc_compact_heap(&mem.heaps[heapnr],mem.heaptype[heapnr],heapnr); }
            #endif
            #ifdef SPVW_MIXED
            { gc_compact_heap(&mem.heaps[heapnr],1-heapnr); }
            #endif
      }
      # Aktualisierungsphase:
        # Der gesamte LISP-Speicher wird durchgegangen und dabei alte durch
        # neue Adressen ersetzt.
        # Durchlaufen durch alle LISP-Objekte und aktualisieren:
          # Pointer im LISP-Stack aktualisieren:
            for_all_STACKs(update_STACK(objptr));
          # Programmkonstanten aktualisieren:
            update_tables();
          # Pointer in den Cons-Zellen aktualisieren:
            #define update_conspage  update_conspage_normal
            update_conses();
            #undef update_conspage
          # Pointer in den Objekten variabler Länge aktualisieren:
            #define update_page(page,updater)  \
              { var aint ptr = page->page_start;                             \
                var aint ptrend = page->page_end;                            \
                # alle Objekte mit Adresse >=ptr, <ptrend durchgehen:        \
                until (ptr==ptrend) # solange bis ptr am Ende angekommen ist \
                  { # nächstes Objekt mit Adresse ptr (< ptrend) durchgehen: \
                    updater(typecode_at(ptr) & ~bit(garcol_bit_t)); # und weiterrücken \
              }   }
            #define update_fpointer_invalid  FALSE
            #define update_fsubr_function FALSE
            #define update_ht_invalid  mark_ht_invalid
            #define update_fp_invalid  mark_fp_invalid
            #define update_fs_function(ptr)
            update_varobjects();
            #undef update_fs_function
            #undef update_fp_invalid
            #undef update_ht_invalid
            #undef update_fsubr_function
            #undef update_fpointer_invalid
            #undef update_page
      # Durchführung der Verschiebungen in den nicht ganz geleerten Pages:
        for_each_varobject_page(page,
          { if (!(page->page_gcpriv.d == -1L))
              { var aint p1 = page->page_start;
                var aint p1end = page->page_end;
                var aint p2 = p1 - page->page_gcpriv.d;
                if (!(p1==p2)) # falls Verschiebung nötig
                  { var_prepare_objsize;
                    page->page_start = p2;
                    until (p1==p1end) # obere Grenze erreicht -> fertig
                      { # nächstes Objekt hat Adresse p1, ist markiert
                        unmark(p1); # Markierung löschen
                        # Objekt behalten und verschieben:
                       {var uintL count = objsize((Varobject)p1); # Länge (durch varobject_alignment teilbar, >0)
                        move_aligned_p1_p2(count); # verschieben und weiterrücken
                      }}
                    page->page_end = p2;
          }   }   }
          );
      for_each_cons_heap(heap, { heap->lastused = dummy_lastused; } );
      recalc_space(TRUE);
      free_delayed_pages();
      free_some_unused_pages();
      CHECK_AVL_CONSISTENCY();
      CHECK_GC_CONSISTENCY();
      CHECK_GC_UNMARKED(); CHECK_NULLOBJ();
      CHECK_PACK_CONSISTENCY();
      gc_timer_off();
      gc_signalblock_off(); # Signale wieder freigeben
      clr_break_sem_1(); # BREAK wieder ermöglichen
    }

#endif

# Garbage Collection durchführen:
  local void gar_col_simple (void);
  local void do_gar_col_simple (void);
  local void do_gar_col_simple()
    {
      #ifdef NOCOST_SP_CHECK
      # Better flag a stack overflow before GC than during GC. (If the
      # stack overflow handler is called during GC, a crash is unavoidable.)
      if (near_SP_overflow()) SP_ueber();
      #endif
      #if !defined(GENERATIONAL_GC)
      gar_col_normal();
      #ifdef SPVW_PAGES
      #if defined(UNIX) || defined(AMIGAOS) || defined(RISCOS) || defined(WIN32)
      # Wenn der in Pages allozierte, aber unbelegte Speicherplatz
      # mehr als 25% dessen ausmacht, was belegt ist, lohnt sich wohl eine
      # Kompaktierung, denn fürs Betriebssystem kostet eine halbleere Page
      # genausoviel wie eine volle Page:
      if (free_space() > floor(mem.last_gcend_space,4))
        { gar_col_compact(); mem.last_gc_compacted = TRUE; }
        else
      #endif
        { mem.last_gc_compacted = FALSE; }
      #endif
      #else # defined(GENERATIONAL_GC)
      # Wenn nach der letzten GC die Objekte in der neuen Generation
      # mehr als 25% der Objekte in der alten Generation ausmachten,
      # dann machen wir diesmal eine volle Garbage-Collection (beide
      # Generationen auf einmal.)
      if (mem.last_gcend_space1 > floor(mem.last_gcend_space0,4))
        { generation = 0; gar_col_normal(); mem.last_gc_full = TRUE; }
        else
        { generation = 1; gar_col_normal(); mem.last_gc_full = FALSE; }
      #endif
      gar_col_done();
    }
  local void gar_col_simple()
    { var uintC saved_mv_count = mv_count; # mv_count retten
      pushSTACK(subr_self); # subr_self retten
      with_gc_statistics(&do_gar_col_simple); # GC und Statistik
      subr_self = popSTACK(); # subr_self zurück
      mv_count = saved_mv_count; # mv_count zurück
    }

# Volle Garbage Collection durchführen:
  global void gar_col (void);
  local void do_gar_col (void);
  local void do_gar_col()
    {
      #ifdef NOCOST_SP_CHECK
      # Better flag a stack overflow before GC than during GC. (If the
      # stack overflow handler is called during GC, a crash is unavoidable.)
      if (near_SP_overflow()) SP_ueber();
      #endif
      #if !defined(GENERATIONAL_GC)
      gar_col_normal();
      #ifdef SPVW_PAGES
      gar_col_compact(); mem.last_gc_compacted = TRUE;
      #endif
      #else # defined(GENERATIONAL_GC)
      generation = 0; gar_col_normal(); mem.last_gc_full = TRUE;
      #endif
      gar_col_done();
    }
  global void gar_col()
    { var uintC saved_mv_count = mv_count; # mv_count retten
      pushSTACK(subr_self); # subr_self retten
      with_gc_statistics(&do_gar_col); # GC und Statistik
      subr_self = popSTACK(); # subr_self zurück
      mv_count = saved_mv_count; # mv_count zurück
    }

# Macro update jetzt unnötig:
  #undef update

#if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && RESERVE

# Zur Reorganisation des Objektspeichers nach GC oder vor und nach EXECUTE:
  # Unterprogramm zum Verschieben der Conses.
  # move_conses(delta);
  # Der Reservespeicher wird um delta Bytes (durch varobject_alignment
  # teilbar) verkleinert, dabei die Conses um delta Bytes nach oben geschoben.
  local void move_conses (sintL delta);
  local void move_conses (delta)
    var sintL delta;
    { if (delta==0) return; # keine Verschiebung nötig?
      set_break_sem_1(); # BREAK währenddessen sperren
      gc_signalblock_on(); # Signale währenddessen sperren
      gc_timer_on();
      if (delta>0)
        # aufwärts schieben, von oben nach unten
        { var object* source = (object*) mem.conses.heap_end;
          var object* source_end = (object*) mem.conses.heap_start;
          #if !(defined(MIPS) && !defined(GNU))
          var object* dest = (object*) (mem.conses.heap_end += delta);
          #else # IRIX 4 "cc -ansi" Compiler-Bug umgehen ??
          var object* dest = (mem.conses.heap_end += delta, (object*)mem.conses.heap_end);
          #endif
          mem.conses.heap_start += delta;
          until (source==source_end)
            { *--dest = *--source; # ein ganzes Cons nach oben kopieren
              *--dest = *--source;
        }   }
        else # delta<0
        # abwärts schieben, von unten nach oben
        { var object* source = (object*) mem.conses.heap_start;
          var object* source_end = (object*) mem.conses.heap_end;
          #if !(defined(MIPS) && !defined(GNU))
          var object* dest = (object*) (mem.conses.heap_start += delta);
          #else # IRIX 4 "cc -ansi" Compiler-Bug umgehen ??
          var object* dest = (mem.conses.heap_start += delta, (object*)mem.conses.heap_start);
          #endif
          mem.conses.heap_end += delta;
          until (source==source_end)
            { *dest++ = *source++; # ein ganzes Cons nach oben kopieren
              *dest++ = *source++;
        }   }
      # Pointer auf Conses u.ä. aktualisieren:
      { var soint odelta = (soint)delta<<(oint_addr_shift-addr_shift); # Offset im oint
        # Der gesamte LISP-Speicher wird durchgegangen und dabei alte durch
        # neue Adressen ersetzt.
        # Aktualisierung eines Objekts *objptr :
          #ifdef TYPECODES
            #define update(objptr)  \
              { switch (mtypecode(*(object*)(objptr)))   \
                  { case_pair: # Zwei-Pointer-Objekt?    \
                      *(oint*)(objptr) += odelta; break; \
                    default: break;                      \
              }   }
          #else
            #define update(objptr)  \
              { if (consp(*(object*)(objptr))) *(oint*)(objptr) += odelta; }
          #endif
        # Durchlaufen durch alle LISP-Objekte und aktualisieren:
          # Update pointers in all LISP-stacks:
            #define update_stackobj  update_stackobj_normal
            update_STACKs();
            #undef update_stackobj
          # Programmkonstanten aktualisieren:
            update_tables();
          # Pointer in den Cons-Zellen aktualisieren:
            #define update_conspage  update_conspage_normal
            update_conses();
            #undef update_conspage
          # Pointer in den Objekten variabler Länge aktualisieren:
            #define update_page  update_page_normal
            #define update_fpointer_invalid  FALSE
            #define update_fsubr_function  FALSE
            #define update_ht_invalid  mark_ht_invalid
            #define update_fp_invalid  mark_fp_invalid
            #define update_fs_function(ptr)
            update_varobjects();
            #undef update_fs_function
            #undef update_fp_invalid
            #undef update_ht_invalid
            #undef update_fsubr_function
            #undef update_fpointer_invalid
            #undef update_page
        # Macro update jetzt unnötig:
          #undef update
      }
      # Ende des Verschiebens und Aktualisierens.
      # benötigte Zeit zur GC-Gesamtzeit addieren:
      gc_timer_off();
      gc_signalblock_off(); # Signale wieder freigeben
      clr_break_sem_1(); # BREAK wieder ermöglichen
    }

#endif

# ------------------------------------------------------------------------------
#                 Speicherbereitstellungsfunktionen

# Fehlermeldung wegen vollen Speichers
  nonreturning_function(local, fehler_speicher_voll, (void));
  local void fehler_speicher_voll()
    { dynamic_bind(S(use_clcs),NIL); # SYS::*USE-CLCS* an NIL binden
      if (posfixnump(Symbol_value(S(gc_statistics_stern))))
        { dynamic_bind(S(gc_statistics_stern),Fixnum_0); } # SYS::*GC-STATISTICS* an 0 binden
      fehler(storage_condition,
             DEUTSCH ? "Speicherplatz für LISP-Objekte ist voll." :
             ENGLISH ? "No more room for LISP objects" :
             FRANCAIS ? "Il n'y a plus de place pour des objets LISP." :
             ""
            );
    }

#if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && RESERVE

# Notmaßnahme, wenn Speicher voll: Anzapfen der Reserve und Fehlermeldung.
  nonreturning_function(local, error_speicher_voll, (void));
  local void error_speicher_voll()
    { # Abhilfe: Reservespeicher wird halbiert.
      var uintL reserve = mem.MEMTOP - mem.MEMRES; # noch freie Reserve
      if (reserve>=8) # Reservespeicher auch voll?
        # nein -> Reservespeicher anzapfen und Fehlermeldung ausgeben
        # halbe Reserve
        { move_conses(round_down(floor(reserve,2),varobject_alignment));
          # halbierte Reserve, aligned: um soviel die Conses nach oben schieben
          fehler_speicher_voll();
        }
        else
        # ja -> harte Fehlermeldung
        { asciz_out(DEUTSCH ? NLstring "*** - " "Speicherplatz für LISP-Objekte ist voll: RESET" :
                    ENGLISH ? NLstring "*** - " "No more room for LISP objects: RESET" :
                    FRANCAIS ? NLstring "*** - " "Il n'y a plus de place pour des objets LISP : RAZ" :
                    ""
                   );
           reset(); # und zum letzten Driver-Frame zurück
    }   }

# Bei entspannter Situation: Reserve wieder auffüllen.
# Invariante: (mem.conses.heap_start-mem.varobjects.heap_end >= need).
  local void relax_reserve (uintL need);
  local void relax_reserve(need)
    var uintL need;
    { # Jetzt ist genügend Platz da. Vielleicht sogar genug, den
      # Reservespeicher auf normale Größe zu bringen?
      var uintL free = (mem.conses.heap_start-mem.varobjects.heap_end) - need;
                       # soviel Bytes noch frei
      var uintL free_reserve = mem.MEMTOP-mem.MEMRES;
                       # soviel Bytes noch in der Reserve frei, <=RESERVE
      var uintL free_total = free + free_reserve;
                       # freier Objektspeicher + freie Reserve
      if (free_total >= RESERVE) # mindestens Normalwert RESERVE ?
        # ja -> Reservespeicher auf normale Größe bringen, indem
        # die Conses um (RESERVE - free_reserve) nach unten geschoben
        # werden:
        move_conses(free_reserve-RESERVE);
        # Dadurch bleibt genügend für need frei.
    }

#else

#define error_speicher_voll()  fehler_speicher_voll()
#define relax_reserve(need)

#endif

# Stellt fest, ob eine Adresse im Intervall [0..2^oint_addr_len-1] liegt:
  #if !defined(TYPECODES) || ((oint_addr_len==32) && !defined(WIDE_HARD)) # d.h. !defined(TYPECODES) || defined(WIDE_SOFT)
    #define pointable_usable_test(a)  TRUE
  #else
    #define pointable_usable_test(a)  \
      ((void*)pointable(type_pointer_object(0,a)) == (void*)(a))
  #endif

# Holt Speicher vom Betriebssystem
  local void* mymalloc (uintL need);
  local void* mymalloc(need)
    var uintL need;
    {
      var void* addr;
      begin_system_call();
      addr = malloc(need);
      end_system_call();
      if (addr==NULL) return NULL;
      # Intervall [addr,addr+need-1] muß in [0..2^oint_addr_len-1] liegen:
      { var aint a = (aint)addr; # a = untere Intervallgrenze
        if (pointable_usable_test(a))
          { a = round_down(a + need-1,bit(addr_shift)); # a = obere Intervallgrenze
            if (pointable_usable_test(a))
              { return addr; }
      }   }
      # Mit diesem Stück Speicher können wir nichts anfangen, wieder zurückgeben:
      begin_system_call();
      free(addr);
      end_system_call();
      #if defined(AMIGAOS) && !(defined(WIDE) || defined(MC68000) || !defined(TYPECODES))
      # Wir machen einen zweiten Versuch mit veränderten Flags.
      if (!(default_allocmemflag == retry_allocmemflag))
        { begin_system_call();
          addr = allocmem(need,retry_allocmemflag);
          end_system_call();
          if (addr==NULL) return NULL;
          # Intervall [addr,addr+need-1] muß in [0..2^oint_addr_len-1] liegen:
          { var aint a = (aint)addr; # a = untere Intervallgrenze
            if (pointable_usable_test(a))
              { a = round_down(a + need-1,bit(addr_shift)); # a = obere Intervallgrenze
                if (pointable_usable_test(a))
                  { return addr; }
          }   }
          # Auch mit diesem Stück Speicher können wir nichts anfangen, wieder zurückgeben:
          begin_system_call();
          freemem(addr);
          end_system_call();
        }
      #endif
      return NULL;
    }

#if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && !defined(TRIVIALMAP_MEMORY)

# Schafft Platz für ein neues Objekt.
# Falls keiner vorhanden -> Fehlermeldung.
# make_space_FLAG(need);
# > flag: ob Objekt variabler Länge oder nicht
# > uintL need: angeforderter Platz in Bytes (eine Variable oder Konstante)
  # Der Test, ob Platz vorhanden ist, als Macro, der Rest als Funktion:
  #define make_space_TRUE(need)  make_space(need)
  #define make_space_FALSE(need)  make_space(need)
  #define make_space(need)  \
    { if (not_enough_room_p(need)) make_space_gc(need); }
  #if !defined(GENERATIONAL_GC)
    #define not_enough_room_p(need)  (mem.conses.heap_start-mem.varobjects.heap_end < (uintP)(need))
  #else
    #define not_enough_room_p(need)  (mem.total_room < (uintL)(need))
  #endif
  local void make_space_gc (uintL need);
  local void make_space_gc(need)
    var uintL need;
    { # (mem.conses.heap_start-mem.varobjects.heap_end < need)  bzw.
      # (mem.total_room < need)  ist schon abgeprüft, also
        # Nicht genügend Platz
        not_enough_room:
        { gar_col_simple(); # Garbage Collector aufrufen
          doing_gc:
          # Teste auf Tastatur-Unterbrechung
          interruptp(
            { pushSTACK(S(gc)); tast_break();
              if (not_enough_room_p(need)) goto not_enough_room;
                else
                return;
            });
          if (mem.conses.heap_start-mem.varobjects.heap_end < (uintP)(need)) # und wieder testen
            # Wirklich nicht genügend Platz da.
            # [Unter UNIX mit 'realloc' arbeiten??]
            # Abhilfe: man versucht eine volle GC.
            {
              #ifdef GENERATIONAL_GC
              if (!mem.last_gc_full)
                { gar_col(); goto doing_gc; }
                else
              #endif
                { error_speicher_voll(); }
            }
            else
            # Jetzt ist genügend Platz da. Vielleicht sogar genug, den
            # Reservespeicher auf normale Größe zu bringen?
            { relax_reserve(need);
              # Jetzt ist sicher (mem.conses.heap_start-mem.varobjects.heap_end >= need).
              #ifdef GENERATIONAL_GC
              # Falls (mem.total_room < need), ignorieren wir das:
              if (mem.total_room < need) { mem.total_room = need; }
              #endif
            }
    }   }

#endif

#if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && defined(TRIVIALMAP_MEMORY)

# Schafft Platz für ein neues Objekt.
# Falls keiner vorhanden -> Fehlermeldung.
# make_space_FLAG(need);
# > flag: ob Objekt variabler Länge oder nicht
# > uintL need: angeforderter Platz in Bytes (eine Variable oder Konstante)
  # Der Test, ob Platz vorhanden ist, als Macro, der Rest als Funktion:
  #define make_space_TRUE(need)  \
    { if ((mem.total_room < (uintL)(need))                                         \
          || (mem.varobjects.heap_limit - mem.varobjects.heap_end < (uintP)(need)) \
         )                                                                         \
        make_space_gc_TRUE(need,&mem.varobjects);                                  \
    }
  #define make_space_FALSE(need)  \
    { if ((mem.total_room < (uintL)(need))                                   \
          || (mem.conses.heap_start - mem.conses.heap_limit < (uintP)(need)) \
         )                                                                   \
        make_space_gc_FALSE(need,&mem.conses);                               \
    }
  local void make_space_gc_TRUE (uintL need, Heap* heapptr);
  local void make_space_gc_TRUE(need,heapptr)
    var uintL need;
    var Heap* heapptr;
    { # (mem.total_room < need) || (heapptr->heap_limit - heapptr->heap_end < need)
      # ist schon abgeprüft, also nicht genügend Platz.
      not_enough_room:
     {var boolean done_gc = FALSE;
      if (mem.total_room < need)
        do_gc:
        { gar_col_simple(); # Garbage Collector aufrufen
          doing_gc:
          # Teste auf Tastatur-Unterbrechung
          interruptp(
            { pushSTACK(S(gc)); tast_break();
              if ((mem.total_room < need) || (heapptr->heap_limit - heapptr->heap_end < need))
                goto not_enough_room;
                else
                return;
            });
          done_gc = TRUE;
        }
      # Entweder ist jetzt (mem.total_room >= need), oder aber wir haben gerade
      # eine GC durchgeführt. In beiden Fällen konzentrieren wir uns nun
      # darauf, heapptr->heap_limit zu vergrößern.
      { var aint needed_limit = heapptr->heap_end + need;
        if (needed_limit <= heapptr->heap_limit) # hat die GC ihre Arbeit getan?
          return; # ja -> fertig
        # Aufrunden bis zur nächsten Seitengrenze:
        #ifndef GENERATIONAL_GC
        needed_limit = round_up(needed_limit,map_pagesize); # sicher > heapptr->heap_limit
        #else # map_pagesize bekanntermaßen eine Zweierpotenz
        needed_limit = (needed_limit + map_pagesize-1) & -map_pagesize; # sicher > heapptr->heap_limit
        #endif
        # neuen Speicher allozieren:
        if (needed_limit <= mem.conses.heap_limit) # avoid crossover
          { begin_system_call();
           {var int ergebnis = zeromap((void*)(heapptr->heap_limit),needed_limit - heapptr->heap_limit);
            end_system_call();
            if (ergebnis >= 0) goto sufficient;
            asciz_out(DEUTSCH ? "Versuche, durch eine GC Platz zu schaffen..." NLstring :
                      ENGLISH ? "Trying to make room through a GC..." NLstring :
                      FRANCAIS ? "Essayons de faire de la place par un GC..." NLstring :
                      ""
                     );
          }}
        # nicht erfolgreich
        if (!done_gc)
          goto do_gc;
        #ifdef GENERATIONAL_GC
        if (!mem.last_gc_full)
          { gar_col(); goto doing_gc; }
        #endif
        error_speicher_voll();
        sufficient:
        heapptr->heap_limit = needed_limit;
      }
      # Jetzt ist sicher (heapptr->heap_limit - heapptr->heap_end >= need).
      # Falls (mem.total_room < need), ignorieren wir das:
      if (mem.total_room < need) { mem.total_room = need; }
    }}
  local void make_space_gc_FALSE (uintL need, Heap* heapptr);
  local void make_space_gc_FALSE(need,heapptr)
    var uintL need;
    var Heap* heapptr;
    { # (mem.total_room < need) || (heapptr->heap_start - heapptr->heap_limit < need)
      # ist schon abgeprüft, also nicht genügend Platz.
      not_enough_room:
     {var boolean done_gc = FALSE;
      if (mem.total_room < need)
        do_gc:
        { gar_col_simple(); # Garbage Collector aufrufen
          doing_gc:
          # Teste auf Tastatur-Unterbrechung
          interruptp(
            { pushSTACK(S(gc)); tast_break();
              if ((mem.total_room < need) || (heapptr->heap_start - heapptr->heap_limit < need))
                goto not_enough_room;
                else
                return;
            });
          done_gc = TRUE;
        }
      # Entweder ist jetzt (mem.total_room >= need), oder aber wir haben gerade
      # eine GC durchgeführt. In beiden Fällen konzentrieren wir uns nun
      # darauf, heapptr->heap_limit zu verkleinern.
      { var aint needed_limit = heapptr->heap_start - need;
        if (needed_limit > heapptr->heap_start) # wraparound?
          goto failed;
        if (needed_limit >= heapptr->heap_limit) # hat die GC ihre Arbeit getan?
          return; # ja -> fertig
        # Abrunden bis zur nächsten Seitengrenze:
        #ifndef GENERATIONAL_GC
        needed_limit = round_down(needed_limit,map_pagesize); # sicher < heapptr->heap_limit
        #else # map_pagesize bekanntermaßen eine Zweierpotenz
        needed_limit = needed_limit & -map_pagesize; # sicher < heapptr->heap_limit
        #endif
        # neuen Speicher allozieren:
        if (needed_limit >= mem.varobjects.heap_limit) # avoid crossover
          { begin_system_call();
           {var int ergebnis = zeromap((void*)needed_limit,heapptr->heap_limit - needed_limit);
            end_system_call();
            if (ergebnis >= 0) goto sufficient;
            asciz_out(DEUTSCH ? "Versuche, durch eine GC Platz zu schaffen..." NLstring :
                      ENGLISH ? "Trying to make room through a GC..." NLstring :
                      FRANCAIS ? "Essayons de faire de la place par un GC..." NLstring :
                      ""
                     );
          }}
        # nicht erfolgreich
        failed:
        if (!done_gc)
          goto do_gc;
        #ifdef GENERATIONAL_GC
        if (!mem.last_gc_full)
          { gar_col(); goto doing_gc; }
        #endif
        error_speicher_voll();
        sufficient:
        heapptr->heap_limit = needed_limit;
      }
      # Jetzt ist sicher (heapptr->heap_start - heapptr->heap_limit >= need).
      # Falls (mem.total_room < need), ignorieren wir das:
      if (mem.total_room < need) { mem.total_room = need; }
    }}

#endif

#if defined(SPVW_PURE_BLOCKS) || defined(SPVW_MIXED_BLOCKS_STAGGERED) # <==> (SINGLEMAP_MEMORY || TRIVIALMAP_MEMORY) && !SPVW_MIXED_BLOCKS_OPPOSITE

# Schafft Platz für ein neues Objekt.
# Falls keiner vorhanden -> Fehlermeldung.
# make_space(need,heapptr);
# > uintL need: angeforderter Platz in Bytes (eine Variable oder Konstante)
# > Heap* heapptr: Pointer auf den Heap, dem der Platz entnommen werden soll
  # Der Test, ob Platz vorhanden ist, als Macro, der Rest als Funktion:
  #define make_space(need,heapptr)  \
    { if ((mem.total_room < (uintL)(need))                                 \
          || ((heapptr)->heap_limit - (heapptr)->heap_end < (uintP)(need)) \
         )                                                                 \
        make_space_gc(need,heapptr);                                       \
    }
  local void make_space_gc (uintL need, Heap* heapptr);
  local void make_space_gc(need,heapptr)
    var uintL need;
    var Heap* heapptr;
    { # (mem.total_room < need) || (heapptr->heap_limit - heapptr->heap_end < need)
      # ist schon abgeprüft, also nicht genügend Platz.
      not_enough_room:
     {var boolean done_gc = FALSE;
      if (mem.total_room < need)
        do_gc:
        { gar_col_simple(); # Garbage Collector aufrufen
          doing_gc:
          # Teste auf Tastatur-Unterbrechung
          interruptp(
            { pushSTACK(S(gc)); tast_break();
              if ((mem.total_room < need) || (heapptr->heap_limit - heapptr->heap_end < need))
                goto not_enough_room;
                else
                return;
            });
          done_gc = TRUE;
        }
      # Entweder ist jetzt (mem.total_room >= need), oder aber wir haben gerade
      # eine GC durchgeführt. In beiden Fällen konzentrieren wir uns nun
      # darauf, heapptr->heap_limit zu vergrößern.
      { var aint needed_limit = heapptr->heap_end + need;
        if (needed_limit <= heapptr->heap_limit) # hat die GC ihre Arbeit getan?
          return; # ja -> fertig
        # Aufrunden bis zur nächsten Seitengrenze:
        #ifndef GENERATIONAL_GC
        needed_limit = round_up(needed_limit,map_pagesize); # sicher > heapptr->heap_limit
        #else # map_pagesize bekanntermaßen eine Zweierpotenz
        needed_limit = (needed_limit + map_pagesize-1) & -map_pagesize; # sicher > heapptr->heap_limit
        #endif
        # neuen Speicher allozieren:
        if (needed_limit-1 <= heapptr->heap_hardlimit-1)
          { begin_system_call();
           {var int ergebnis = zeromap((void*)(heapptr->heap_limit),needed_limit - heapptr->heap_limit);
            end_system_call();
            if (ergebnis >= 0) goto sufficient;
            asciz_out(DEUTSCH ? "Versuche, durch eine GC Platz zu schaffen..." NLstring :
                      ENGLISH ? "Trying to make room through a GC..." NLstring :
                      FRANCAIS ? "Essayons de faire de la place par un GC..." NLstring :
                      ""
                     );
          }}
        # nicht erfolgreich
        if (!done_gc)
          goto do_gc;
        #ifdef GENERATIONAL_GC
        if (!mem.last_gc_full)
          { gar_col(); goto doing_gc; }
        #endif
        fehler_speicher_voll();
        sufficient:
        heapptr->heap_limit = needed_limit;
      }
      # Jetzt ist sicher (heapptr->heap_limit - heapptr->heap_end >= need).
      # Falls (mem.total_room < need), ignorieren wir das:
      if (mem.total_room < need) { mem.total_room = need; }
    }}

#endif

#ifdef SPVW_PAGES

# Schafft Platz für ein neues Objekt.
# Falls keiner vorhanden -> Fehlermeldung.
# make_space(need,heap_ptr,stack_ptr, page);
# > uintL need: angeforderter Platz in Bytes (eine Variable oder Konstante)
# > Heap* heap_ptr: Adresse des Heaps, aus dem der Platz genommen werden soll
# > AVL(AVLID,stack) * stack_ptr: Adressen eines lokalen Stacks,
#   für ein späteres AVL(AVLID,move)
# < Pages page: gefundene Page, wo der Platz ist
  # Der Test, ob Platz vorhanden ist, als Macro, der Rest als Funktion:
  #define make_space(need,heap_ptr,stack_ptr,pagevar)  \
    { pagevar = AVL(AVLID,least)(need,&(heap_ptr)->inuse,stack_ptr);    \
      if (pagevar==EMPTY)                                               \
        { pagevar = make_space_gc(need,&(heap_ptr)->inuse,stack_ptr); } \
    }
  local Pages make_space_gc (uintL need, Pages* pages_ptr, AVL(AVLID,stack) * stack_ptr);
  local Pages make_space_gc(need,pages_ptr,stack_ptr)
    var uintL need;
    var Pages* pages_ptr;
    var AVL(AVLID,stack) * stack_ptr;
    { # AVL(AVLID,least)(need,pages_ptr,stack_ptr) == EMPTY
      # ist schon abgeprüft, also
        # Nicht genügend Platz
        not_enough_room:
        #define handle_interrupt_after_gc()  \
          { # Teste auf Tastatur-Unterbrechung                               \
            interruptp(                                                      \
              { pushSTACK(S(gc)); tast_break();                              \
               {var Pages page = AVL(AVLID,least)(need,pages_ptr,stack_ptr); \
                if (page==EMPTY) goto not_enough_room;                       \
                  else                                                       \
                  return page;                                               \
              }});                                                           \
          }
        #if !defined(AVL_SEPARATE)
        #define make_space_using_malloc()  \
          # versuche, beim Betriebssystem Platz zu bekommen:                        \
          { var uintL size1 = round_up(need,sizeof(cons_));                         \
            if (size1 < std_page_size) { size1 = std_page_size; }                   \
           {var uintL size2 = size1 + sizeof(NODE) + (varobject_alignment-1);       \
            var aint addr = (aint)mymalloc(size2);                                  \
            if (!((void*)addr == NULL))                                             \
              { # Page vom Betriebssystem bekommen.                                 \
                var Pages page = (Pages)addr;                                       \
                page->m_start = addr; page->m_length = size2;                       \
                # Initialisieren:                                                   \
                page->page_start = page->page_end = page_start0(page);              \
                page->page_room = size1;                                            \
                # Diesem Heap zuschlagen:                                           \
                *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);                   \
                if (!(AVL(AVLID,least)(need,pages_ptr,stack_ptr) == page)) abort(); \
                mem.total_space += size1;                                           \
                return page;                                                        \
          }}  }
        #else # AVL_SEPARATE
        #define make_space_using_malloc()  \
          # versuche, beim Betriebssystem Platz zu bekommen:                            \
          { var uintL size1 = round_up(need,sizeof(cons_));                             \
            if (size1 < std_page_size) { size1 = std_page_size; }                       \
            begin_system_call();                                                        \
           {var Pages page = (NODE*)malloc(sizeof(NODE));                               \
            end_system_call();                                                          \
            if (!(page == NULL))                                                        \
              { var uintL size2 = size1 + (varobject_alignment-1);                      \
                var aint addr = (aint)mymalloc(size2);                                  \
                if (!((void*)addr == NULL))                                             \
                  { # Page vom Betriebssystem bekommen.                                 \
                    page->m_start = addr; page->m_length = size2;                       \
                    # Initialisieren:                                                   \
                    page->page_start = page->page_end = page_start0(page);              \
                    page->page_room = size1;                                            \
                    # Diesem Heap zuschlagen:                                           \
                    *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);                   \
                    if (!(AVL(AVLID,least)(need,pages_ptr,stack_ptr) == page)) abort(); \
                    mem.total_space += size1;                                           \
                    return page;                                                        \
                  }                                                                     \
                  else                                                                  \
                  { begin_system_call(); free(page); end_system_call(); }               \
          }}  }
        #endif
        if ((need <= std_page_size) && !(mem.free_pages == NULL))
          { # Eine normalgroße Page aus dem allgemeinen Pool entnehmen:
            var Pages page = mem.free_pages;
            mem.free_pages = page->page_gcpriv.next;
            # page ist bereits korrekt initialisiert:
            # page->page_start = page->page_end = page_start0(page);
            # page->page_room =
            #   round_down(page->m_start + page->m_length,varobject_alignment)
            # und diesem Heap zuschlagen:
            *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);
            if (!(AVL(AVLID,least)(need,pages_ptr,stack_ptr) == page)) abort();
            mem.total_space += page->page_room;
            return page;
          }
        if (used_space()+need < mem.gctrigger_space)
          # Benutzter Platz ist seit der letzten GC noch nicht einmal um 25%
          # angewachsen -> versuche es erstmal beim Betriebssystem;
          # die GC machen wir, wenn die 25%-Grenze erreicht ist.
          { make_space_using_malloc(); }
        { gar_col_simple(); # Garbage Collector aufrufen
          handle_interrupt_after_gc();
          # und wieder testen:
         {var Pages page = AVL(AVLID,least)(need,pages_ptr,stack_ptr);
          if (page==EMPTY)
            { if (!mem.last_gc_compacted)
                { gar_col_compact(); # kompaktierenden Garbage Collector aufrufen
                  handle_interrupt_after_gc();
                  page = AVL(AVLID,least)(need,pages_ptr,stack_ptr);
                }
              if (page==EMPTY)
                # versuche es nun doch beim Betriebssystem:
                { make_space_using_malloc();
                  fehler_speicher_voll();
            }   }
          # .reserve behandeln??
          return page;
        }}
        #undef make_space_using_malloc
        #undef handle_interrupt_after_gc
    }

#endif

# Macro zur Speicher-Allozierung eines Lisp-Objekts:
# allocate(type,flag,size,ptrtype,ptr,statement)
# > type: Expression, die den Typcode liefert
# > flag: ob Objekt variabler Länge oder nicht
# > size: Expression (constant oder var), die die Größe des benötigten
#         Speicherstücks angibt
# ptrtype: C-Typ von ptr
# ptr: C-Variable
# Ein Speicherstück der Länge size, passend zu einem Lisp-Objekt vom Typ type,
# wird geholt und ptr auf seine Anfangsadresse gesetzt. Dann wird statement
# ausgeführt (Initialisierung des Speicherstücks) und schließlich ptr,
# mit der korrekten Typinfo versehen, als Ergebnis geliefert.
  #ifdef TYPECODES
    #define bias_type_pointer_object(bias,type,ptr)  type_pointer_object(type,ptr)
  #else
    #define bias_type_pointer_object(bias,type,ptr)  as_object((oint)(ptr)+(bias))
  #endif
  #ifdef SPVW_BLOCKS
   #if defined(SPVW_PURE_BLOCKS) || defined(TRIVIALMAP_MEMORY) || defined(GENERATIONAL_GC)
    #define decrement_total_room(amount)  mem.total_room -= (amount);
   #else
    #define decrement_total_room(amount)
   #endif
   #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement)  \
      allocate_##flag (type_expr,size_expr,ptrtype,ptrvar,statement)
    # Objekt variabler Länge:
    #define allocate_TRUE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { make_space_TRUE(size_expr);                                                   \
        set_break_sem_1(); # Break sperren                                            \
       {var ptrtype ptrvar;                                                           \
        var object obj;                                                               \
        ptrvar = (ptrtype) mem.varobjects.heap_end; # Pointer auf Speicherstück       \
        mem.varobjects.heap_end += (size_expr); # Speicheraufteilung berichtigen      \
        decrement_total_room(size_expr);                                              \
        ptrvar->GCself = obj = bias_type_pointer_object(varobject_bias,type_expr,ptrvar); # Selbstpointer \
        statement; # Speicherstück initialisieren                                     \
        clr_break_sem_1(); # Break ermöglichen                                        \
        CHECK_GC_CONSISTENCY();                                                       \
        return obj;                                                                   \
      }}
    # Cons o.ä.:
    #define allocate_FALSE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { make_space_FALSE(size_expr);                                                        \
        set_break_sem_1(); # Break sperren                                                  \
       {var ptrtype ptrvar;                                                                 \
        ptrvar = (ptrtype)(mem.conses.heap_start -= size_expr); # Pointer auf Speicherstück \
        decrement_total_room(size_expr);                                                    \
        statement; # Speicherstück initialisieren                                           \
        clr_break_sem_1(); # Break ermöglichen                                              \
        CHECK_GC_CONSISTENCY();                                                             \
        return bias_type_pointer_object(cons_bias,type_expr,ptrvar);                        \
      }}
   #endif
   #ifdef SPVW_MIXED_BLOCKS_STAGGERED
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement)  \
      allocate_##flag (type_expr,size_expr,ptrtype,ptrvar,statement)
    # Objekt variabler Länge:
    #define allocate_TRUE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { make_space(size_expr,&mem.varobjects);                                        \
        set_break_sem_1(); # Break sperren                                            \
       {var ptrtype ptrvar;                                                           \
        var object obj;                                                               \
        ptrvar = (ptrtype) mem.varobjects.heap_end; # Pointer auf Speicherstück       \
        mem.varobjects.heap_end += (size_expr); # Speicheraufteilung berichtigen      \
        decrement_total_room(size_expr);                                              \
        ptrvar->GCself = obj = bias_type_pointer_object(varobject_bias,type_expr,ptrvar); # Selbstpointer \
        statement; # Speicherstück initialisieren                                     \
        clr_break_sem_1(); # Break ermöglichen                                        \
        CHECK_GC_CONSISTENCY();                                                       \
        return obj;                                                                   \
      }}
    # Cons o.ä.:
    #define allocate_FALSE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { make_space(size_expr,&mem.conses);                                              \
        set_break_sem_1(); # Break sperren                                              \
       {var ptrtype ptrvar = (ptrtype) mem.conses.heap_end; # Pointer auf Speicherstück \
        mem.conses.heap_end += (size_expr); # Speicheraufteilung berichtigen            \
        decrement_total_room(size_expr);                                                \
        statement; # Speicherstück initialisieren                                       \
        clr_break_sem_1(); # Break ermöglichen                                          \
        CHECK_GC_CONSISTENCY();                                                         \
        return bias_type_pointer_object(cons_bias,type_expr,ptrvar);                    \
      }}
   #endif
   #ifdef SPVW_PURE
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement)  \
      { var tint _type = (type_expr);                                      \
        var Heap* heapptr = &mem.heaps[_type];                             \
        make_space(size_expr,heapptr);                                     \
        set_break_sem_1(); # Break sperren                                 \
       {var ptrtype ptrvar = (ptrtype)(heapptr->heap_end); # Pointer auf Speicherstück \
        heapptr->heap_end += (size_expr); # Speicheraufteilung berichtigen \
        decrement_total_room(size_expr);                                   \
        allocate_##flag (ptrvar);                                          \
        statement; # Speicherstück initialisieren                          \
        clr_break_sem_1(); # Break ermöglichen                             \
        CHECK_GC_CONSISTENCY();                                            \
        return as_object((oint)ptrvar);                                    \
      }}
    # Objekt variabler Länge:
    #define allocate_TRUE(ptrvar)  \
      ptrvar->GCself = as_object((oint)ptrvar); # Selbstpointer eintragen
    # Cons o.ä.:
    #define allocate_FALSE(ptrvar)
   #endif
  #endif
  #ifdef SPVW_PAGES
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement)  \
      allocate_##flag (type_expr,size_expr,ptrtype,ptrvar,statement)
   #ifdef SPVW_MIXED
    # Objekt variabler Länge:
    #define allocate_TRUE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { # Suche nach der Page mit dem kleinsten page_room >= size_expr:               \
        var AVL(AVLID,stack) stack;                                                   \
        var Pages page;                                                               \
        make_space(size_expr,&mem.varobjects,&stack, page);                           \
        set_break_sem_1(); # Break sperren                                            \
       {var ptrtype ptrvar = (ptrtype)(page->page_end); # Pointer auf Speicherstück   \
        var object obj;                                                               \
        ptrvar->GCself = obj = bias_type_pointer_object(varobject_bias,type_expr,ptrvar); # Selbstpointer \
        statement; # Speicherstück initialisieren                                     \
        page->page_room -= (size_expr); # Speicheraufteilung berichtigen              \
        page->page_end += (size_expr);                                                \
        mem.used_space += (size_expr);                                                \
        AVL(AVLID,move)(&stack); # Page wieder an die richtige Position hängen        \
        clr_break_sem_1(); # Break ermöglichen                                        \
        CHECK_AVL_CONSISTENCY();                                                      \
        CHECK_GC_CONSISTENCY();                                                       \
        return obj;                                                                   \
      }}
    # Cons o.ä.:
    #define allocate_FALSE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { # Suche nach der Page mit dem kleinsten page_room >= size_expr = 8: \
        var Pages page;                                                     \
        # 1. Versuch: letzte benutzte Page                                  \
        page = mem.conses.lastused;                                         \
        if (page->page_room == 0) # Test auf page->page_room < size_expr = sizeof(cons_) \
          { var AVL(AVLID,stack) stack;                                     \
            # 2. Versuch:                                                   \
            make_space(size_expr,&mem.conses,&stack, page);                 \
            mem.conses.lastused = page;                                     \
          }                                                                 \
        set_break_sem_1(); # Break sperren                                  \
       {var ptrtype ptrvar =                                                \
          (ptrtype)(page->page_end); # Pointer auf Speicherstück            \
        statement; # Speicherstück initialisieren                           \
        page->page_room -= (size_expr); # Speicheraufteilung berichtigen    \
        page->page_end += (size_expr);                                      \
        mem.used_space += (size_expr);                                      \
        # Da page_room nun =0 geworden oder >=sizeof(cons_) geblieben ist,  \
        # ist die Sortierreihenfolge der Pages unverändert geblieben.       \
        clr_break_sem_1(); # Break ermöglichen                              \
        CHECK_AVL_CONSISTENCY();                                            \
        CHECK_GC_CONSISTENCY();                                             \
        return bias_type_pointer_object(cons_bias,type_expr,ptrvar);        \
      }}
   #endif
   #ifdef SPVW_PURE
    # Objekt variabler Länge:
    #define allocate_TRUE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { # Suche nach der Page mit dem kleinsten page_room >= size_expr:           \
        var AVL(AVLID,stack) stack;                                               \
        var Pages page;                                                           \
        var tint _type = (type_expr);                                             \
        make_space(size_expr,&mem.heaps[_type],&stack, page);                     \
        set_break_sem_1(); # Break sperren                                        \
       {var ptrtype ptrvar =                                                      \
          (ptrtype)(page->page_end); # Pointer auf Speicherstück                  \
        var object obj;                                                           \
        ptrvar->GCself = obj = type_pointer_object(_type,ptrvar); # Selbstpointer \
        statement; # Speicherstück initialisieren                                 \
        page->page_room -= (size_expr); # Speicheraufteilung berichtigen          \
        page->page_end += (size_expr);                                            \
        mem.used_space += (size_expr);                                            \
        AVL(AVLID,move)(&stack); # Page wieder an die richtige Position hängen    \
        clr_break_sem_1(); # Break ermöglichen                                    \
        CHECK_AVL_CONSISTENCY();                                                  \
        CHECK_GC_CONSISTENCY();                                                   \
        return obj;                                                               \
      }}
    # Cons o.ä.:
    #define allocate_FALSE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { # Suche nach der Page mit dem kleinsten page_room >= size_expr = 8: \
        var Pages page;                                                     \
        var tint _type = (type_expr);                                       \
        var Heap* heapptr = &mem.heaps[_type];                              \
        # 1. Versuch: letzte benutzte Page                                  \
        page = heapptr->lastused;                                           \
        if (page->page_room == 0) # Test auf page->page_room < size_expr = sizeof(cons_) \
          { var AVL(AVLID,stack) stack;                                     \
            # 2. Versuch:                                                   \
            make_space(size_expr,heapptr,&stack, page);                     \
            heapptr->lastused = page;                                       \
          }                                                                 \
        set_break_sem_1(); # Break sperren                                  \
       {var ptrtype ptrvar =                                                \
          (ptrtype)(page->page_end); # Pointer auf Speicherstück            \
        statement; # Speicherstück initialisieren                           \
        page->page_room -= (size_expr); # Speicheraufteilung berichtigen    \
        page->page_end += (size_expr);                                      \
        mem.used_space += (size_expr);                                      \
        # Da page_room nun =0 geworden oder >=sizeof(cons_) geblieben ist,  \
        # ist die Sortierreihenfolge der Pages unverändert geblieben.       \
        clr_break_sem_1(); # Break ermöglichen                              \
        CHECK_AVL_CONSISTENCY();                                            \
        CHECK_GC_CONSISTENCY();                                             \
        return type_pointer_object(_type,ptrvar);                           \
      }}
   #endif
  #endif

# UP, beschafft ein Cons
# allocate_cons()
# < ergebnis: Pointer auf neues CONS, mit CAR und CDR =NIL
# kann GC auslösen
  global object allocate_cons (void);
  global object allocate_cons()
    { allocate(cons_type,FALSE,sizeof(cons_),Cons,ptr,
               { ptr->cdr = NIL; ptr->car = NIL; }
              )
    }

# UP: Liefert ein neu erzeugtes uninterniertes Symbol mit gegebenem Printnamen.
# make_symbol(string)
# > string: Simple-String
# < ergebnis: neues Symbol mit diesem Namen, mit Home-Package=NIL.
# kann GC auslösen
  global object make_symbol (object string);
  global object make_symbol(string)
    var object string;
    { pushSTACK(string); # String retten
      #define FILL  \
        { ptr->symvalue = unbound; # leere Wertzelle         \
          ptr->symfunction = unbound; # leere Funktionszelle \
          ptr->proplist = NIL; # leere Propertyliste         \
          ptr->pname = popSTACK(); # Namen eintragen         \
          ptr->homepackage = NIL; # keine Home-Package       \
        }
      #ifdef TYPECODES
        allocate(symbol_type,TRUE,size_symbol(),Symbol,ptr,
                 { FILL; }
                )
      #else
        allocate(symbol_type,TRUE,size_xrecord(5,0),Symbol,ptr,
                 { ptr->tfl = xrecord_tfl(Rectype_Symbol,0,5,0); FILL; }
                )
      #endif
      #undef FILL
    }

# UP, beschafft Vektor
# allocate_vector(len)
# > len: Länge des Vektors
# < ergebnis: neuer Vektor (Elemente werden mit NIL initialisiert)
# kann GC auslösen
  global object allocate_vector (uintL len);
  global object allocate_vector (len)
    var uintL len;
    { var uintL need = size_svector(len); # benötigter Speicherplatz
      #ifdef TYPECODES
        #define SETTFL  ptr->length = len;
      #else
        #define SETTFL  ptr->tfl = lrecord_tfl(Rectype_Svector,len);
      #endif
      allocate(svector_type,TRUE,need,Svector,ptr,
               { SETTFL
                 { var object* p = &ptr->data[0];
                   dotimesL(len,len, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
               } }
              )
      #undef SETTFL
    }

# UP, beschafft Bit-Vektor
# allocate_bit_vector(len)
# > len: Länge des Bitvektors (in Bits)
# < ergebnis: neuer Bitvektor (LISP-Objekt)
# kann GC auslösen
  global object allocate_bit_vector (uintL len);
  global object allocate_bit_vector (len)
    var uintL len;
    { var uintL need = size_sbvector(len); # benötigter Speicherplatz in Bytes
      #ifdef TYPECODES
        #define SETTFL  ptr->length = len;
      #else
        #define SETTFL  ptr->tfl = lrecord_tfl(Rectype_Sbvector,len);
      #endif
      allocate(sbvector_type,TRUE,need,Sbvector,ptr,
               { SETTFL } # Keine weitere Initialisierung
              )
      #undef SETTFL
    }

# UP, beschafft String
# allocate_string(len)
# > len: Länge des Strings (in Bytes)
# < ergebnis: neuer Simple-String (LISP-Objekt)
# kann GC auslösen
  global object allocate_string (uintL len);
  global object allocate_string (len)
    var uintL len;
    { var uintL need = size_sstring(len); # benötigter Speicherplatz in Bytes
      #ifdef TYPECODES
        #define SETTFL  ptr->length = len;
      #else
        #define SETTFL  ptr->tfl = lrecord_tfl(Rectype_Sstring,len);
      #endif
      allocate(sstring_type,TRUE,need,Sstring,ptr,
               { SETTFL } # Keine weitere Initialisierung
              )
      #undef SETTFL
    }

# UP, beschafft indirekten Array
# allocate_iarray(flags,rank,type)
# > uintB flags: Flags
# > uintC (eigentlich uintWC) rank: Rang
# > tint type: Typinfo
# < ergebnis: LISP-Objekt Array
# kann GC auslösen
  global object allocate_iarray (uintB flags, uintC rank, tint type);
  global object allocate_iarray(flags,rank,type)
    var uintB flags;
    var uintC rank;
    var tint type;
    { var uintL need = rank;
      if (flags & bit(arrayflags_fillp_bit)) { need += 1; }
      if (flags & bit(arrayflags_dispoffset_bit)) { need += 1; }
      need = size_iarray(need);
      #ifdef TYPECODES
        #define SETTFL  ptr->flags = flags; ptr->rank = rank;
      #else
        #define SETTFL  ptr->tfl = srecord_tfl(type,flags,rank);
      #endif
      allocate(type,TRUE,need,Iarray,ptr,
               { SETTFL # Flags und Rang eintragen
                 ptr->data = NIL; # Datenvektor mit NIL initialisieren
               }
              )
      #undef SETTFL
    }

# UP, beschafft Simple-Record
# allocate_srecord_(flags_rectype,reclen,type)
# > uintW flags_rectype: Flags, nähere Typinfo
# > uintC reclen: Länge
# > tint type: Typinfo
# < ergebnis: LISP-Objekt Record (Elemente werden mit NIL initialisiert)
# kann GC auslösen
  #ifdef TYPECODES
  global object allocate_srecord_ (uintW flags_rectype, uintC reclen, tint type);
  global object allocate_srecord_(flags_rectype,reclen,type)
    var uintW flags_rectype;
    var uintC reclen;
    var tint type;
    { ASSERT((sintB)(flags_rectype >> (BIG_ENDIAN_P ? 0 : 8)) < rectype_limit);
     {var uintL need = size_srecord(reclen);
      allocate(type,TRUE,need,Srecord,ptr,
               { *(uintW*)pointerplus(ptr,offsetof(record_,recflags)) = flags_rectype; # Flags, Typ eintragen
                 ptr->reclength = reclen; # Länge eintragen
                {var object* p = &ptr->recdata[0];
                 dotimespC(reclen,reclen, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
               }}
              )
    }}
  #else
  global object allocate_srecord_ (uintW flags_rectype, uintC reclen);
  global object allocate_srecord_(flags_rectype,reclen)
    var uintW flags_rectype;
    var uintC reclen;
    { var uintL need = size_srecord(reclen);
      allocate(type,TRUE,need,Srecord,ptr,
               { ptr->tfl = (uintL)flags_rectype + ((uintL)reclen << 16);
                {var object* p = &ptr->recdata[0];
                 dotimespC(reclen,reclen, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
               }}
              )
    }
  #endif

# UP, beschafft Extended-Record
# allocate_xrecord_(flags_rectype,reclen,recxlen,type)
# > uintW flags_rectype: Flags, nähere Typinfo
# > uintC reclen: Länge
# > uintC recxlen: Extra-Länge
# > tint type: Typinfo
# < ergebnis: LISP-Objekt Record (Elemente werden mit NIL bzw. 0 initialisiert)
# kann GC auslösen
  #ifdef TYPECODES
  global object allocate_xrecord_ (uintW flags_rectype, uintC reclen, uintC recxlen, tint type);
  global object allocate_xrecord_(flags_rectype,reclen,recxlen,type)
    var uintW flags_rectype;
    var uintC reclen;
    var uintC recxlen;
    var tint type;
    { ASSERT((sintB)(flags_rectype >> (BIG_ENDIAN_P ? 0 : 8)) >= rectype_limit);
     {var uintL need = size_xrecord(reclen,recxlen);
      allocate(type,TRUE,need,Xrecord,ptr,
               { *(uintW*)pointerplus(ptr,offsetof(record_,recflags)) = flags_rectype; # Flags, Typ eintragen
                 ptr->reclength = reclen; ptr->recxlength = recxlen; # Längen eintragen
                {var object* p = &ptr->recdata[0];
                 dotimesC(reclen,reclen, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
                 {var uintB* q = (uintB*)p;
                  dotimesC(recxlen,recxlen, { *q++ = 0; } ); # Extra-Elemente mit 0 vollschreiben
               }}}
              )
    }}
  #else
  global object allocate_xrecord_ (uintW flags_rectype, uintC reclen, uintC recxlen);
  global object allocate_xrecord_(flags_rectype,reclen,recxlen)
    var uintW flags_rectype;
    var uintC reclen;
    var uintC recxlen;
    { var uintL need = size_xrecord(reclen,recxlen);
      allocate(type,TRUE,need,Xrecord,ptr,
               { ptr->tfl = (uintL)flags_rectype + ((uintL)reclen << 16) + ((uintL)recxlen << 24); # Flags, Typ, Längen eintragen
                {var object* p = &ptr->recdata[0];
                 dotimesC(reclen,reclen, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
                 {var uintB* q = (uintB*)p;
                  dotimesC(recxlen,recxlen, { *q++ = 0; } ); # Extra-Elemente mit 0 vollschreiben
               }}}
              )
    }
  #endif

#ifndef case_stream

# UP, beschafft Stream
# allocate_stream(strmflags,strmtype,reclen)
# > uintB strmflags: Flags
# > uintB strmtype: nähere Typinfo
# > uintC reclen: Länge
# < ergebnis: LISP-Objekt Stream (Elemente werden mit NIL initialisiert)
# kann GC auslösen
  global object allocate_stream (uintB strmflags, uintB strmtype, uintC reclen);
  global object allocate_stream(strmflags,strmtype,reclen)
    var uintB strmflags;
    var uintB strmtype;
    var uintC reclen;
    { var object obj = allocate_xrecord(0,Rectype_Stream,reclen,0,orecord_type);
      TheRecord(obj)->recdata[0] = Fixnum_0; # Fixnum als Platz für strmflags und strmtype
      TheStream(obj)->strmflags = strmflags; TheStream(obj)->strmtype = strmtype;
      return obj;
    }

#endif

#ifdef FOREIGN

# UP, beschafft Foreign-Pointer-Verpackung
# allocate_fpointer(foreign)
# > foreign: vom Typ FOREIGN
# < ergebnis: LISP-Objekt, das foreign enthält
# kann GC auslösen
  global object allocate_fpointer (FOREIGN foreign);
  global object allocate_fpointer(foreign)
    var FOREIGN foreign;
    { var object result = allocate_xrecord(0,Rectype_Fpointer,fpointer_length,fpointer_xlength,orecord_type);
      TheFpointer(result)->fp_pointer = foreign;
      return result;
    }

#endif

#ifdef FOREIGN_HANDLE

# UP, beschafft Handle-Verpackung
# allocate_handle(handle)
# < ergebnis: LISP-Objekt, das handle enthält
  global object allocate_handle (Handle handle);
  global object allocate_handle(handle)
    var Handle handle;
    { var object result = allocate_bit_vector(sizeof(Handle)*8);
      TheHandle(result) = handle;
      return result;
    }

#endif

# UP, beschafft Bignum
# allocate_bignum(len,sign)
# > uintC (eigentlich uintWC) len: Länge der Zahl (in Digits)
# > sintB sign: Flag für Vorzeichen (0 = +, -1 = -)
# < ergebnis: neues Bignum (LISP-Objekt)
# kann GC auslösen
  global object allocate_bignum (uintC len, sintB sign);
  global object allocate_bignum(len,sign)
    var uintC len;
    var sintB sign;
    { var uintL need = size_bignum(len); # benötigter Speicherplatz in Bytes
      #ifdef TYPECODES
        #define SETTFL  ptr->length = len;
      #else
        #define SETTFL  ptr->tfl = srecord_tfl(Rectype_Bignum,(uintB)sign,len);
      #endif
      allocate(bignum_type | (sign & bit(sign_bit_t)),TRUE,need,Bignum,ptr,
               { SETTFL } # Keine weitere Initialisierung
              )
      #undef SETTFL
    }

# UP, beschafft Single-Float
# allocate_ffloat(value)
# > ffloat value: Zahlwert (Bit 31 = Vorzeichen)
# < ergebnis: neues Single-Float (LISP-Objekt)
# kann GC auslösen
  global object allocate_ffloat (ffloat value);
  #ifndef WIDE
  global object allocate_ffloat(value)
    var ffloat value;
    {
      #ifdef TYPECODES
        #define SETTFL
      #else
        #define SETTFL  ptr->tfl = xrecord_tfl(Rectype_Ffloat,((sint32)value<0 ? 0xFF : 0),0,sizeof(ffloat));
      #endif
      allocate(ffloat_type | ((sint32)value<0 ? bit(sign_bit_t) : 0) # Vorzeichenbit aus value
               ,TRUE,size_ffloat(),Ffloat,ptr,
               { SETTFL; ptr->float_value = value; }
              )
      #undef SETTFL
    }
  #else
  global object allocate_ffloat(value)
    var ffloat value;
    { return
        type_data_object(ffloat_type | ((sint32)value<0 ? bit(sign_bit_t) : 0), # Vorzeichenbit aus value
                         value
                        );
    }
  #endif

# UP, beschafft Double-Float
#ifdef intQsize
# allocate_dfloat(value)
# > dfloat value: Zahlwert (Bit 63 = Vorzeichen)
# < ergebnis: neues Double-Float (LISP-Objekt)
# kann GC auslösen
  global object allocate_dfloat (dfloat value);
  global object allocate_dfloat(value)
    var dfloat value;
    {
      #ifdef TYPECODES
        #define SETTFL
      #else
        #define SETTFL  ptr->tfl = xrecord_tfl(Rectype_Dfloat,((sint64)value<0 ? 0xFF : 0),0,sizeof(dfloat));
      #endif
      allocate(dfloat_type | ((sint64)value<0 ? bit(sign_bit_t) : 0) # Vorzeichenbit aus value
               ,TRUE,size_dfloat(),Dfloat,ptr,
               { SETTFL; ptr->float_value = value; }
              )
      #undef SETTFL
    }
#else
# allocate_dfloat(semhi,mlo)
# > semhi,mlo: Zahlwert (Bit 31 von semhi = Vorzeichen)
# < ergebnis: neues Double-Float (LISP-Objekt)
# kann GC auslösen
  global object allocate_dfloat (uint32 semhi, uint32 mlo);
  global object allocate_dfloat(semhi,mlo)
    var uint32 semhi;
    var uint32 mlo;
    {
      #ifdef TYPECODES
        #define SETTFL
      #else
        #define SETTFL  ptr->tfl = xrecord_tfl(Rectype_Dfloat,((sint32)semhi<0 ? 0xFF : 0),0,sizeof(dfloat));
      #endif
      allocate(dfloat_type | ((sint32)semhi<0 ? bit(sign_bit_t) : 0) # Vorzeichenbit aus value
               ,TRUE,size_dfloat(),Dfloat,ptr,
               { SETTFL; ptr->float_value.semhi = semhi; ptr->float_value.mlo = mlo; }
              )
      #undef SETTFL
    }
#endif

# UP, beschafft Long-Float
# allocate_lfloat(len,expo,sign)
# > uintC (eigentlich uintWC) len: Länge der Mantisse (in Digits)
# > uintL expo: Exponent
# > signean sign: Vorzeichen (0 = +, -1 = -)
# < ergebnis: neues Long-Float, noch ohne Mantisse
# Ein LISP-Objekt liegt erst dann vor, wenn die Mantisse eingetragen ist!
# kann GC auslösen
  global object allocate_lfloat (uintC len, uintL expo, signean sign);
  global object allocate_lfloat(len,expo,sign)
    var uintC len;
    var uintL expo;
    var signean sign;
    { var uintL need = size_lfloat(len); # benötigter Speicherplatz in Bytes
      #ifdef TYPECODES
        #define SETTFL  ptr->len = len;
      #else
        #define SETTFL  ptr->tfl = srecord_tfl(Rectype_Lfloat,(uintB)sign,len);
      #endif
      allocate(lfloat_type | ((tint)sign & bit(sign_bit_t))
               ,TRUE,need,Lfloat,ptr,
               { SETTFL; ptr->expo = expo; } # Keine weitere Initialisierung
              )
      #undef SETTFL
    }

# UP, erzeugt Bruch
# make_ratio(num,den)
# > object num: Zähler (muß Integer /= 0 sein, relativ prim zu den)
# > object den: Nenner (muß Integer > 1 sein)
# < ergebnis: Bruch
# kann GC auslösen
  global object make_ratio (object num, object den);
  global object make_ratio(num,den)
    var object num;
    var object den;
    { pushSTACK(den); pushSTACK(num); # Argumente sichern
     {
      #ifdef TYPECODES
      var tint type = # Vorzeichen von num übernehmen
        #ifdef fast_mtypecode
        ratio_type | (mtypecode(STACK_0) & bit(sign_bit_t))
        #else
        ratio_type | (typecode(num) & bit(sign_bit_t))
        #endif
        ;
      #endif
      #define FILL  \
         ptr->rt_num = popSTACK(); # Zähler eintragen \
         ptr->rt_den = popSTACK(); # Nenner eintragen
      #ifdef SPVW_MIXED
        # see allocate_xrecord
        #ifdef TYPECODES
          #define SETTFL  \
            *(uintW*)pointerplus(ptr,offsetof(record_,recflags)) = ((uintW)Rectype_Ratio << (BIG_ENDIAN_P ? 0 : 8)); \
            ptr->reclength = 2; ptr->recxlength = 0;
        #else
          var uintL tfl = xrecord_tfl(Rectype_Ratio,(positivep(num) ? 0 : 0xFF),2,0);
          #define SETTFL  \
            ptr->tfl = tfl;
        #endif
        allocate(type,TRUE,size_xrecord(2,0),Ratio,ptr,
                 { SETTFL; FILL; }
                )
        #undef SETTFL
      #else
        allocate(type,FALSE,sizeof(ratio_),Ratio,ptr,
                 { FILL; }
                )
      #endif
      #undef FILL
    }}

# UP, erzeugt komplexe Zahl
# make_complex(real,imag)
# > real: Realteil (muß reelle Zahl sein)
# > imag: Imaginärteil (muß reelle Zahl /= Fixnum 0 sein)
# < ergebnis: komplexe Zahl
# kann GC auslösen
  global object make_complex (object real, object imag);
  global object make_complex(real,imag)
    var object real;
    var object imag;
    { pushSTACK(imag); pushSTACK(real);
      #define FILL  \
        ptr->c_real = popSTACK(); # Realteil eintragen \
        ptr->c_imag = popSTACK(); # Imaginärteil eintragen
      #ifdef SPVW_MIXED
        # see allocate_xrecord
        #ifdef TYPECODES
          #define SETTFL  \
            *(uintW*)pointerplus(ptr,offsetof(record_,recflags)) = ((uintW)Rectype_Complex << (BIG_ENDIAN_P ? 0 : 8)); \
            ptr->reclength = 2; ptr->recxlength = 0;
        #else
          #define SETTFL  \
            ptr->tfl = xrecord_tfl(Rectype_Complex,0,2,0);
        #endif
        allocate(complex_type,TRUE,size_xrecord(2,0),Complex,ptr,
                 { SETTFL; FILL; }
                )
        #undef SETTFL
      #else
        allocate(complex_type,FALSE,sizeof(complex_),Complex,ptr,
                 { FILL; }
                )
      #endif
      #undef FILL
    }

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

#ifdef GNU_GETTEXT

  # Modify the environment variables. putenv() is POSIX, but some BSD systems
  # only have setenv(). Therefore (and because it's simpler to use) we
  # implement this interface, but without the third argument.
  # mysetenv(name,value) sets the value of the environment variable `name' to
  # `value' and returns 0. Returns -1 if not enough memory.
  local int mysetenv (const char * name, const char * value);
  local int mysetenv(name,value)
    var const char * name;
    var const char * value;
    { var uintL namelen = asciz_length(name);
      var uintL valuelen = asciz_length(value);
      #if defined(HAVE_PUTENV)
        var char* buffer = malloc(namelen+1+valuelen+1);
        var char* bufptr;
        if (!buffer)
          { return -1; } # no need to set errno = ENOMEM
        bufptr = buffer;
        dotimesL(namelen,namelen, { *bufptr++ = *name++; });
        *bufptr++ = '=';
        dotimesL(valuelen,valuelen, { *bufptr++ = *value++; });
        *bufptr = '\0';
        return putenv(buffer);
      #elif defined(HAVE_SETENV)
        return setenv(name,value,1);
      #else
        # Uh oh, neither putenv() nor setenv(), have to frob the environment
        # ourselves. Routine taken from glibc and fixed in several aspects.
        extern char** environ;
        var char** epp;
        var char* ep;
        var uintL envvar_count = 0;
        for (epp = environ; (ep = *epp) != NULL; epp++)
          { var const char * np = name;
            # Compare *epp and name:
            while (*np != '\0' && *np == *ep) { np++; ep++; }
            if (*np == '\0' && *ep == '=')
              break;
            envvar_count++;
          }
        ep = *epp;
        if (ep == NULL)
          # name not found in environ, add it.
          { # Remember the environ, so that we can free it if we need
            # to reallocate it again next time.
            var static char** last_environ = NULL;
            var char** new_environ = (char**) malloc((envvar_count+2)*sizeof(char*));
            if (!new_environ)
              { return -1; } # no need to set errno = ENOMEM
            { var uintL count;
              epp = environ;
              for (count = 0; count < envvar_count; count++)
                new_environ[count] = epp[count];
            }
            ep = (char*) malloc(namelen+1+valuelen+1);
            if (!ep)
              { free(new_environ); return -1; } # no need to set errno = ENOMEM
            { var char* cp = ep;
              dotimesL(namelen,namelen, { *cp++ = *name++; });
              *cp++ = '=';
              dotimesL(valuelen,valuelen, { *cp++ = *value++; });
              *cp = '\0';
            }
            new_environ[envvar_count] = ep;
            new_environ[envvar_count+1] = NULL;
            environ = new_environ;
            if (last_environ != NULL) { free(last_environ); }
            last_environ = new_environ;
          }
          else
          # name found, replace its value.
          { # We could be tempted to overwrite name's value directly if
            # the new value is not longer than the old value. But that's
            # not a good idea - maybe someone still has a pointer to
            # this area around.
            ep = (char*) malloc(namelen+1+valuelen+1);
            if (!ep)
              { return -1; } # no need to set errno = ENOMEM
            { var char* cp = ep;
              dotimesL(namelen,namelen, { *cp++ = *name++; });
              *cp++ = '=';
              dotimesL(valuelen,valuelen, { *cp++ = *value++; });
              *cp = '\0';
            }
            *epp = ep;
          }
        return 0;
      #endif
    }

#endif

#ifndef LANGUAGE_STATIC

  # Sprache, in der mit dem Benutzer kommuniziert wird:
    global uintL language;

  # Initialisiert die Sprache, gegeben die Sprachbezeichnung.
    local boolean init_language_from (const char* langname);
    #ifdef GNU_GETTEXT
      #define language_spanish  3
    #endif
    local boolean init_language_from(langname)
      var const char* langname;
      { if (asciz_equal(langname,"ENGLISH") || asciz_equal(langname,"english"))
          { language = language_english; return TRUE; }
        if (asciz_equal(langname,"DEUTSCH") || asciz_equal(langname,"deutsch")
            || asciz_equal(langname,"GERMAN") || asciz_equal(langname,"german")
           )
          { language = language_deutsch; return TRUE; }
        if (asciz_equal(langname,"FRANCAIS") || asciz_equal(langname,"francais")
            #ifndef ASCII_CHS
            || asciz_equal(langname,"FRANÇAIS") || asciz_equal(langname,"français")
            #endif
            || asciz_equal(langname,"FRENCH") || asciz_equal(langname,"french")
           )
          { language = language_francais; return TRUE; }
        #ifdef GNU_GETTEXT
        if (asciz_equal(langname,"ESPANOL") || asciz_equal(langname,"espanol")
            #ifndef ASCII_CHS
            || asciz_equal(langname,"ESPAÑOL") || asciz_equal(langname,"español")
            #endif
            || asciz_equal(langname,"SPANISH") || asciz_equal(langname,"spanish")
           )
          { language = language_spanish; return TRUE; }
        #endif
        return FALSE;
      }

  # Initialisiert die Sprache.
    local void init_language (const char* argv_language, const char* argv_localedir);
    local void init_language(argv_language,argv_localedir)
      var const char* argv_language;
      var const char* argv_localedir;
      { # Sprache wird so festgelegt, mit Prioritäten in dieser Reihenfolge:
        #   1. Fest eingebaut, LANGUAGE_STATIC
        #   2. -L Kommandozeilen-Argument
        #   3. Environment-Variable CLISP_LANGUAGE
        #   4. Environment-Variable LANG
        #   5. Default: Englisch
        if (argv_language)
          { if (init_language_from(argv_language)) goto chosen1; }
        #ifdef HAVE_ENVIRONMENT
        { var const char* langname = getenv("CLISP_LANGUAGE");
          if (langname)
            { if (init_language_from(langname)) goto chosen1; }
          #ifdef AMIGAOS
          langname = getenv("Language"); # since OS 3.0
          if (langname)
            { if (init_language_from(langname)) goto chosen1; }
          #endif
        }
        #endif
        #ifdef GNU_GETTEXT
        # The analysis of getenv("LANG") below will be done - in more detail -
        # by bindtextdomain() and textdomain(). No need to do it ourselves.
        # Do we need to call setlocale(LC_MESSAGES,"") or not??
        goto chosen2;
        #else
        #ifdef HAVE_ENVIRONMENT
        { var const char* lang = getenv("LANG");
          if (lang)
            { # LANG hat i.a. die Syntax Sprache[_Land][.Zeichensatz]
              if (lang[0]=='e' && lang[1]=='n' && !alphanumericp((uintB)lang[2])) # "en"
                { language = language_english; goto chosen2; }
              if (lang[0]=='d' && lang[1]=='e' && !alphanumericp((uintB)lang[2])) # "de"
                { language = language_deutsch; goto chosen2; }
              if (lang[0]=='f' && lang[1]=='r' && !alphanumericp((uintB)lang[2])) # "fr"
                { language = language_francais; goto chosen2; }
        }   }
        #endif
        # Default: Englisch
        language = language_english; goto chosen2;
        #endif
        chosen1:
          # At this point we have chosen the language based upon the
          # command-line option or the clisp-specific environment variables.
          #ifdef GNU_GETTEXT
            # GNU gettext chooses the message catalog based upon:
            # 1. environment variable LANGUAGE [only if dcgettext.c, not with
            #    cat-compat.c],
            # 2. environment variable LC_ALL,
            # 3. environment variable LC_MESSAGES,
            # 4. environment variable LANG.
            # We clobber LC_MESSAGES and unset the earlier two variables.
            { var const char * locale =
                language == language_english ? "en" :
                language == language_deutsch ? "de" :
                language == language_francais ? "fr" :
                language == language_spanish ? "es" :
                "";
              if (getenv("LANGUAGE")) { mysetenv("LANGUAGE",""); }
              if (getenv("LC_ALL")) { mysetenv("LC_ALL",""); }
              mysetenv("LC_MESSAGES",locale);
              #ifdef LC_MESSAGES # !(UNIX_NEXTSTEP || ...)
              # Given the above, the following line is probably not needed.
              # (Depends on the behaviour of setlocale(LC_MESSAGES,NULL) on
              # your system.) Anyway it doesn't hurt.
              setlocale(LC_MESSAGES,locale);
              #endif
            }
          #endif
        chosen2:
          # At this point we have chosen the language based upon an
          # environment variable GNU gettext knows about.
          #ifdef GNU_GETTEXT
          { var const char * package = "clisp";
            # We apparently don't need to check whether argv_localedir is
            # not NULL and a valid directory. But since we may call chdir()
            # before the gettext library opens the catalog file, we have to
            # convert argv_localedir to be an absolute pathname, if possible.
            #ifdef UNIX
            if (!(argv_localedir == NULL))
              if (argv_localedir[0] != '\0' && argv_localedir[0] != '/')
                { var char currdir[MAXPATHLEN];
                  if (!(getwd(currdir) == NULL))
                    { var uintL currdirlen = asciz_length(currdir);
                      if (currdirlen > 0 && currdir[0] == '/')
                        { var uintL len = currdirlen + 1 + asciz_length(argv_localedir) + 1;
                          var char* abs_localedir = (char*)malloc(len*sizeof(char));
                          if (!(abs_localedir == NULL))
                            { # Append currdir, maybe '/', and argv_localedir into abs_localedir:
                              var char* ptr = abs_localedir;
                              { var const char * srcptr = currdir;
                                var uintL count;
                                dotimespL(count,currdirlen, { *ptr++ = *srcptr++; });
                              }
                              if (ptr[-1] != '/') { *ptr++ = '/'; }
                              { var const char * srcptr = argv_localedir;
                                while ((*ptr++ = *srcptr++) != '\0') continue;
                              }
                              argv_localedir = abs_localedir;
                }   }   }   }
            #endif
            bindtextdomain(package,argv_localedir);
            textdomain(package);
          }
          #endif
          return;
      }

 #ifdef GNU_GETTEXT

  #ifndef ISOLATIN_CHS
    # Our native character set is not ISOLATIN, the one in which the .gmo
    # files are encoded. Therefore we need to convert the messages to
    # ISOLATIN before calling gettext() and to convert the result back
    # to the local character set.
    #ifdef IBMPC_CHS
      # These tables come from utils/charset/cv-{fr,to}-ibmpc.c.
      # Conversion table from local character set to ISOLATIN character set.
      local uintB to_latin_table[256] =
        { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
          0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0xA4,
          0x10, 0x11, 0x12, 0x13, 0xB6, 0xA7, 0x16, 0x17,
          0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
          0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
          0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
          0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
          0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
          0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
          0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
          0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
          0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
          0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
          0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
          0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
          0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
          0xC7, 0xFC, 0xE9, 0xE2, 0xE4, 0xE0, 0xE5, 0xE7,
          0xEA, 0xEB, 0xE8, 0xEF, 0xEE, 0xEC, 0xC4, 0xC5,
          0xC9, 0xE6, 0xC6, 0xF4, 0xF6, 0xF2, 0xFB, 0xF9,
          0xFF, 0xD6, 0xDC, 0xA2, 0xA3, 0xA5, 0x9E, 0x9F,
          0xE1, 0xED, 0xF3, 0xFA, 0xF1, 0xD1, 0xAA, 0xBA,
          0xBF, 0xA9, 0xAC, 0xBD, 0xBC, 0xA1, 0xAB, 0xBB,
          0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,
          0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
          0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
          0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
          0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
          0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,
          0xE0, 0xDF, 0xE2, 0xE3, 0xE4, 0xE5, 0xB5, 0xE7,
          0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
          0xF0, 0xB1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF7, 0xF7,
          0xA0, 0xF9, 0xB7, 0xFB, 0xFC, 0xA2, 0xFE, 0xFF
        };
      # Conversion table from ISOLATIN character set to local character set.
      local uintB from_latin_table[256] =
        { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
          0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
          0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
          0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
          0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
          0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
          0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
          0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
          0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
          0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
          0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
          0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
          0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
          0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
          0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
          0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
          0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
          0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,
          0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
          0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F,
          0x20, 0xAD, 0x9B, 0x9C, 0x0F, 0xBD, 0x7C, 0x15,
          0x22, 0xA9, 0xA6, 0xAE, 0xAA, 0x2D, 0xAE, 0xAF,
          0xF8, 0xF1, 0xFD, 0xB3, 0x27, 0xE6, 0x14, 0xFA,
          0x2C, 0xB9, 0xA7, 0xAF, 0xAC, 0xAB, 0xBE, 0xA8,
          0x41, 0xC1, 0xC2, 0xC3, 0x8E, 0x8F, 0x92, 0x80,
          0xC8, 0x90, 0xCA, 0xCB, 0xCC, 0x49, 0xCE, 0xCF,
          0xD0, 0xA5, 0xD2, 0x4F, 0xD4, 0xD5, 0x9F, 0xD7,
          0xD8, 0xD9, 0xDA, 0xDB, 0x9A, 0xDD, 0xDE, 0xE1,
          0x85, 0xA0, 0x83, 0xE3, 0x84, 0x86, 0x91, 0x87,
          0x8A, 0x82, 0x88, 0x89, 0x8D, 0xA1, 0x8C, 0x8B,
          0xF0, 0xA4, 0x95, 0xA2, 0x93, 0xF5, 0x94, 0xF6,
          0xF8, 0x97, 0xA3, 0x96, 0x81, 0xFD, 0xFE, 0x98
        };
    #endif # IBMPC_CHS
    local const char * cvgettext (const char * msgid);
    local const char * cvgettext(msgid)
      var const char * msgid;
      { local char resultbuf[1024];
        var uintL len = asciz_length(msgid);
        var DYNAMIC_ARRAY(cvmsgid,char,len+1);
        var const uintB* ptr1;
        var uintB* ptr2;
        # Convert msgid argument to Latin1.
        ptr1 = (const uintB*)msgid;
        ptr2 = (uintB*)cvmsgid;
        while (!(*ptr1 == '\0')) { *ptr2++ = to_latin_table[*ptr1++]; }
        *ptr2 = '\0';
        # Lookup message translation.
        ptr1 = (const uintB*)gettext(cvmsgid);
        # Convert translation to local character set.
        ptr2 = resultbuf;
        while (!(*ptr1 == '\0')) { *ptr2++ = from_latin_table[*ptr1++]; }
        *ptr2 = '\0';
        FREE_DYNAMIC_ARRAY(cvmsgid);
        return resultbuf;
      }
  #else # ISOLATIN_CHS
    # No conversion needed.
    #define cvgettext gettext
  #endif

  global const char * clgettext (const char * msgid);
  global const char * clgettext(msgid)
    var const char * msgid;
    { var const char * translated_msg;
      if (msgid[0] == '\0')
        { # If you ask gettext to translate the empty string, it returns
          # the catalog's header (containing meta information)!
          translated_msg = msgid;
        }
        else
        { begin_system_call();
          translated_msg = cvgettext(msgid);
          end_system_call();
        }
      return translated_msg;
    }

  global object localized_string (object obj);
  global object localized_string(obj)
    var object obj;
    { ASSERT(stringp(obj));
      with_string_0(obj,asciz,
        { obj = asciz_to_string(clgettext(asciz)); });
      return obj;
    }

  global object localized_object (object obj);
  global object localized_object(obj)
    var object obj;
    { ASSERT(stringp(obj));
      with_string_0(obj,asciz,
        { obj = asciz_to_string(clgettext(asciz)); });
      dynamic_bind(S(packagestern),O(default_package)); # *PACKAGE* binden
      pushSTACK(obj); funcall(L(read_from_string),1); # READ-FROM-STRING ausführen
      dynamic_unbind();
      return value1;
    }

 #endif

#endif

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

#ifdef PENDING_INTERRUPTS
  # Flag, ob eine Unterbrechung anliegt.
  global uintB interrupt_pending = FALSE;
#endif

#ifdef HAVE_SIGNALS

# Paßt den Wert von SYS::*PRIN-LINELENGTH* an die aktuelle Breite des
# Terminal-Fensters an.
# update_linelength();
  local void update_linelength (void);
  local void update_linelength()
    { # SYS::*PRIN-LINELENGTH* := Breite des Terminal-Fensters - 1
      #if !defined(NEXTAPP)
      # [vgl. 'term.c' in 'calc' von Hans-J. Böhm, Vernon Lee, Alan J. Demers]
      if (isatty(stdout_handle)) # Standard-Output ein Terminal?
        { /* var int lines = 0; */
          var int columns = 0;
          #ifdef TIOCGWINSZ
          # Probiere erst ioctl:
          { var struct winsize stdout_window_size;
            if (!( ioctl(stdout_handle,TIOCGWINSZ,&stdout_window_size) <0))
              { /* lines = stdout_window_size.ws_row; */
                columns = stdout_window_size.ws_col;
          }   }
          # Das kann - entgegen der Dokumentation - scheitern!
          if (/* (lines > 0) && */ (columns > 0)) goto OK;
          #endif
          #ifndef WATCOM
          # Nun probieren wir's über termcap:
          { var char* term_name = getenv("TERM");
            if (term_name==NULL) { term_name = "unknown"; }
           {var char termcap_entry_buf[10000];
            if ( tgetent(&!termcap_entry_buf,term_name) ==1)
              { /* lines = tgetnum("li"); if (lines<0) { lines = 0; } */
                columns = tgetnum("co"); if (columns<0) { columns = 0; }
              }
          }}
          #endif
          # Hoffentlich enthält columns jetzt einen vernünftigen Wert.
          if (/* (lines > 0) && */ (columns > 0)) goto OK;
          if (FALSE)
            { OK:
              # Wert von SYS::*PRIN-LINELENGTH* verändern:
              Symbol_value(S(prin_linelength)) =
                fixnum(columns-1);
            }
        }
      #else # defined(NEXTAPP)
      if (nxterminal_line_length > 0)
        # Wert von SYS::*PRIN-LINELENGTH* verändern:
        { Symbol_value(S(prin_linelength)) = fixnum(nxterminal_line_length-1); }
      #endif
    }
#if defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
# Signal-Handler für Signal SIGWINCH:
  local void sigwinch_handler (int sig);
  local void sigwinch_handler(sig)
    var int sig; # sig = SIGWINCH
    { inc_break_sem_5();
      signal_acknowledge(SIGWINCH,&sigwinch_handler);
      update_linelength();
      dec_break_sem_5();
    }
#endif

# Our general policy with child processes - in particular child processes
# to which we are connected through pipes - is not to wait for them, but
# instead do what init(1) would do in case our process terminates before
# the child: perform a non-blocking waitpid() and ignore the child's
# termination status.
#   void handle_child () { while (waitpid(-1,NULL,WNOHANG) > 0); }
#   SIGNAL(SIGCLD,handle_child);
# The following is equivalent (but better, since it doesn't interrupt system
# calls):
#   SIGNAL(SIGCLD,SIG_IGN);

  local void install_sigcld_handler (void);
  local void install_sigcld_handler ()
    {
      #if defined(SIGCLD)
        SIGNAL(SIGCLD,SIG_IGN);
      #endif
    }

  global void begin_want_sigcld ()
    {
      #if defined(SIGCLD)
        SIGNAL(SIGCLD,SIG_DFL);
      #endif
    }
  global void end_want_sigcld ()
    {
      #if defined(SIGCLD)
        SIGNAL(SIGCLD,SIG_IGN);
        # Try to remove zombies which may have been created since the last
        # begin_want_sigcld() call.
        #ifdef HAVE_WAITPID
          while (waitpid(-1,NULL,WNOHANG) > 0);
        #endif
      #endif
    }

# Eine Tastatur-Unterbrechung (Signal SIGINT, erzeugt durch Ctrl-C)
# wird eine Sekunde lang aufgehoben. In dieser Zeit kann sie mittels
# 'interruptp' auf fortsetzbare Art behandelt werden. Nach Ablauf dieser
# Zeit wird das Programm nichtfortsetzbar unterbrochen.
# Signal-Handler für Signal SIGINT:
  local void interrupt_handler (int sig);
  local void interrupt_handler(sig)
    var int sig; # sig = SIGINT
    { inc_break_sem_5();
      signal_acknowledge(SIGINT,&interrupt_handler);
  #ifdef PENDING_INTERRUPTS
      if (!interrupt_pending) # Liegt schon ein Interrupt an -> nichts zu tun
        { interrupt_pending = TRUE; # Flag für 'interruptp' setzen
          #ifdef HAVE_UALARM
          # eine halbe Sekunde warten, dann jede 1/20 sec probieren
          ualarm(ticks_per_second/2,ticks_per_second/20);
          #else
          alarm(1); # eine Sekunde warten, weiter geht's dann bei alarm_handler
          #endif
        }
      dec_break_sem_5();
    }
  local void alarm_handler (int sig);
  local void alarm_handler(sig)
    var int sig; # sig = SIGALRM
    { # Die Zeit ist nun abgelaufen.
      inc_break_sem_5();
      #ifdef EMUNIX # Verhindere Programm-Beendigung durch SIGALRM
      #ifndef HAVE_UALARM
      alarm(0); # SIGALRM-Timer abbrechen
      #endif
      #endif
      signal_acknowledge(SIGALRM,&alarm_handler);
  #endif # PENDING_INTERRUPTS (!)
      dec_break_sem_5();
    #ifndef NO_ASYNC_INTERRUPTS
      # Warten, bis Unterbrechung erlaubt:
      if (!break_sems_cleared())
    #endif
        {
          #ifndef WATCOM
          #ifndef HAVE_UALARM
          alarm(1); # Probieren wir's in einer Sekunde nochmal
          #endif
          #endif
          return; # Nach kurzer Zeit wird wieder ein SIGALRM ausgelöst.
        }
    #ifndef NO_ASYNC_INTERRUPTS
      # Wir springen jetzt aus dem signal-Handler heraus, weder mit 'return'
      # noch mit 'longjmp'.
      #
      # Hans-J. Boehm <boehm@parc.xerox.com> weist darauf hin, daß dies
      # Probleme bringen kann, wenn das Signal ein laufendes malloc() oder
      # free() unterbrochen hat und die malloc()-Library nicht reentrant ist.
      # Abhilfe: statt malloc() stets xmalloc() verwenden, das eine Break-
      # Semaphore setzt? Aber was ist mit malloc()-Aufrufen, die von Routinen
      # wie opendir(), getpwnam(), tgetent(), ... abgesetzt werden? Soll man
      # malloc() selber definieren und darauf hoffen, daß es von allen Library-
      # funktionen aufgerufen wird (statisch gelinkt oder per DLL)??
      #
      #ifdef RISCOS
      prepare_signal_handler_exit(sig);
      #endif
      #if (defined(USE_SIGACTION) ? defined(SIGACTION_NEED_UNBLOCK) : defined(SIGNAL_NEED_UNBLOCK)) || (defined(GNU_READLINE) && (defined(SIGNALBLOCK_BSD) || defined(SIGNALBLOCK_POSIX)))
      # Falls entweder [SIGNAL_NEED_UNBLOCK] mit signal() installierte Handler
      # sowieso mit blockiertem Signal aufgerufen werden - das sind üblicherweise
      # BSD-Systeme -, oder falls andere unsichere Komponenten [GNU_READLINE]
      # per sigaction() o.ä. das Blockieren des Signals beim Aufruf veranlassen
      # können, müssen wir das gerade blockierte Signal entblockieren:
        #if defined(SIGNALBLOCK_POSIX)
          { var sigset_t sigblock_mask;
            sigemptyset(&sigblock_mask); sigaddset(&sigblock_mask,sig);
            sigprocmask(SIG_UNBLOCK,&sigblock_mask,NULL);
          }
        #elif defined(SIGNALBLOCK_BSD)
          sigsetmask(sigblock(0) & ~sigmask(sig));
        #endif
      #endif
      #ifdef HAVE_SAVED_STACK
      # STACK auf einen sinnvollen Wert setzen:
      if (!(saved_STACK==NULL)) { setSTACK(STACK = saved_STACK); }
      #endif
      # Über 'fehler' in eine Break-Schleife springen:
      fehler(serious_condition,
             DEUTSCH ? "Ctrl-C: Tastatur-Interrupt" :
             ENGLISH ? "Ctrl-C: User break" :
             FRANCAIS ? "Ctrl-C : Interruption clavier" :
             ""
            );
    #endif
    }

#endif # HAVE_SIGNALS

#if defined(SELFMADE_MMAP) || defined(GENERATIONAL_GC)

  # Put a breakpoint here if you want to catch CLISP just before it dies.
  global void sigsegv_handler_failed(address)
    var void* address;
    { asciz_out_1(DEUTSCH ? NLstring "SIGSEGV kann nicht behoben werden. Fehler-Adresse = 0x%x." NLstring :
                  ENGLISH ? NLstring "SIGSEGV cannot be cured. Fault address = 0x%x." NLstring :
                  FRANCAIS ? NLstring "SIGSEGV ne peut être relevé. Adresse fautive = 0x%x." NLstring :
                  "",
                  address
                 );
    }

  # Signal-Handler für Signal SIGSEGV u.ä.:
  local int sigsegv_handler (void* fault_address)
    { set_break_sem_0();
      switch (handle_fault((aint)fault_address))
        { case handler_done:
            # erfolgreich
            clr_break_sem_0();
            return 1;
          case handler_failed:
            # erfolglos
            sigsegv_handler_failed(fault_address);
            # Der Default-Handler wird uns in den Debugger führen.
          default:
            clr_break_sem_0();
            return 0;
        }
    }

  # Alle Signal-Handler installieren:
  local void install_segv_handler (void);
  local void install_segv_handler()
    { sigsegv_install_handler(&sigsegv_handler); }

#endif # SELFMADE_MMAP || GENERATIONAL_GC

#ifdef NOCOST_SP_CHECK

  local void stackoverflow_handler (int emergency);
  local void stackoverflow_handler(emergency)
    var int emergency;
    { if (emergency)
        { asciz_out(DEUTSCH ? "Szenario Apollo 13: Stack-Überlauf-Behandlung ging schief. Beim nächsten Stack-Überlauf kracht's!!!" NLstring :
                    ENGLISH ? "Apollo 13 scenario: Stack overflow handling failed. On the next stack overflow we will crash!!!" NLstring :
                    FRANCAIS ? "Scénario Apollo 13 : Réparation de débordement de pile a échoué. Au prochain débordement de pile, ça cassera!!!" NLstring :
                    ""
                   );
        }
      SP_ueber();
    }

#endif

#ifdef WIN32_NATIVE

  # This is the Ctrl-C handler. It is executed in the main thread and must
  # not return!
  global void interrupt_handler (void);
  global void interrupt_handler()
    { # asciz_out("Entering interrupt handler.\n");
      #ifdef HAVE_SAVED_STACK
      # STACK auf einen sinnvollen Wert setzen:
      if (!(saved_STACK==NULL)) { setSTACK(STACK = saved_STACK); }
      #endif
      # Über 'fehler' in eine Break-Schleife springen:
      fehler(serious_condition,
             DEUTSCH ? "Ctrl-C: Tastatur-Interrupt" :
             ENGLISH ? "Ctrl-C: User break" :
             FRANCAIS ? "Ctrl-C : Interruption clavier" :
             ""
            );
    }

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
      #ifdef MAP_MEMORY
      physpagesize = map_pagesize;
      #else
      physpagesize = pagesize;
      #endif
      # physpageshift = log2(physpagesize);
      { var uintL x = physpagesize;
        var uintL i = 0;
        until ((x >>= 1) == 0) { i++; }
        if (!((1UL << i) == physpagesize)) abort();
        physpageshift = i;
      }
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
            #ifdef NOCOST_SP_CHECK
            # Must allocate room for a substitute stack for the stack overflow
            # handler itself. This cannot be somewhere in the regular stack,
            # because we want to unwind the stack in case of stack overflow.
            { var aint size = 0x4000; # 16 KB should be enough
              var void* room = alloca(size);
              stackoverflow_install_handler(&stackoverflow_handler,(void*)room,size);
            }
            #endif
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
      #if defined(HAVE_SIGNALS)
        #if defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
        # Eine veränderte Größe des Terminal-Fensters soll sich auch sofort
        # in SYS::*PRIN-LINELENGTH* bemerkbar machen:
        SIGNAL(SIGWINCH,&sigwinch_handler);
        #endif
        # Die Größe des Terminal-Fensters auch jetzt beim Programmstart erfragen:
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
                Symbol_value(S(prin_linelength)) =
                  fixnum(columns-1);
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
      #if defined(HAVE_SIGNALS)
        #if defined(UNIX) || defined(EMUNIX) || defined(RISCOS)
          # Ctrl-C-Handler einsetzen:
          SIGNAL(SIGINT,&interrupt_handler);
          #ifdef PENDING_INTERRUPTS
            SIGNAL(SIGALRM,&alarm_handler);
          #endif
          #if defined(GENERATIONAL_GC)
            install_segv_handler();
          #endif
        #endif
        install_sigcld_handler();
      #endif
      #ifdef WIN32_NATIVE
        # Ctrl-C-Handler einsetzen:
        install_sigint_handler();
        # Stack-Overflow- und Page-Fault-Handler einsetzen:
        install_segv_handler();
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
               # Set the protections as in build_old_generation_cache(),
               # but don't need to rebuild the cache.
               if (is_heap_containing_objects(heapnr))
                 if (!(heap->physpages==NULL))
                   { var aint gen0_start_pa = heap->heap_gen0_start & -physpagesize;
                     var aint gen0_end_pa = (heap->heap_gen0_end + (physpagesize-1)) & -physpagesize;
                     xmmprotect(heap, gen0_start_pa, gen0_end_pa-gen0_start_pa, PROT_READ);
                    {var uintL physpage_count = (gen0_end_pa-gen0_start_pa)>>physpageshift;
                     var physpage_state* physpage = heap->physpages;
                     var uintL count;
                     dotimespL(count,physpage_count,
                       { physpage->protection = PROT_READ;
                         physpage++;
                       });
                   }}
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

