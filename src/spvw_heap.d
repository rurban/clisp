# Memory management data structures, part 2: Heap

# ------------------------------ Specification ---------------------------------

#ifdef SPVW_PAGES
# Pages is a collection of pages.
# typedef ... Pages;
#endif

# Heap is a collection of pages, together with some management information.
# typedef ... Heap;

# Iteration through all pages of a heap.
# map_heap(heap,pagevar,statement);

# ------------------------------ Implementation --------------------------------

#ifdef SPVW_PAGES

typedef Page* Pages;

typedef struct { Pages inuse;     # Die gerade benutzten Pages
                 # _Page reserve; # Eine Reserve-Page ??
                 # Bei Heap für Objekte fester Länge:
                 Pages lastused; # Ein Cache für die letzte benutzte Page
               }
        Heap;

  #define map_heap(heap,pagevar,statement)  \
    { AVL_map((heap).inuse,pagevar,statement); }

#endif

#ifdef SPVW_BLOCKS

typedef Page Pages;

#ifdef GENERATIONAL_GC
# Für jede physikalische Speicherseite der alten Generation merken wir uns,
# um auf diese Seite nicht zugreifen zu müssen, welche Pointer auf Objekte
# der neuen Generation diese enthält.
# Solange man auf die Seite nicht schreibend zugreift, bleibt diese Information
# aktuell. Nachdem man auf die Seite aber schreibend zugegriffen hat, muss man
# diese Information bei der nächsten GC neu erstellen. Dies sollte man aber
# machen, ohne auf die Seite davor oder danach zugreifen zu müssen.
typedef struct { object* p; # Adresse des Pointers, innerhalb eines alten Objekts
                 object o;  # o = *p, Pointer auf ein neues Objekt
               }
        old_new_pointer;
typedef struct { # Durchlaufen der Pointer in der Seite benötigt Folgendes:
                   # Fortsetzung des letzten Objekts der Seite davor:
                   object* continued_addr;
                   uintC continued_count;
                   # Erstes Objekt, das in dieser Seite (oder später) beginnt:
                   aint firstobject;
                 # Der Cache der Pointer auf Objekte der neuen Generation:
                 int protection; # PROT_NONE : Nur der Cache ist gültig.
                                 # PROT_READ : Seite und Cache beide gültig.
                                 # PROT_READ_WRITE : Nur die Seite ist gültig.
                 uintL cache_size; # Anzahl der gecacheten Pointer
                 old_new_pointer* cache; # Cache aller Pointer in die neue
                                         # Generation
               }
        physpage_state;
#endif

typedef struct { Pages pages;
                 #if defined(SPVW_PURE_BLOCKS) || (defined(SPVW_MIXED_BLOCKS) && defined(TRIVIALMAP_MEMORY))
                 aint heap_limit;
                 #if !defined(SPVW_MIXED_BLOCKS_OPPOSITE) # SPVW_PURE_BLOCKS || SPVW_MIXED_BLOCKS_STAGGERED
                 aint heap_hardlimit;
                 #endif
                 #endif
                 #ifdef SELFMADE_MMAP
                 uintL memfile_offset;
                 uintL memfile_numpages;
                 uintB* memfile_pages;
                 #endif
                 #ifdef GENERATIONAL_GC
                 aint heap_gen0_start;
                 aint heap_gen0_end;
                 aint heap_gen1_start;
                 physpage_state* physpages;
                 #endif
               }
        Heap;
#define heap_start  pages.page_start
#define heap_end    pages.page_end
#if defined(SPVW_PURE_BLOCKS) || (defined(SPVW_MIXED_BLOCKS) && defined(TRIVIALMAP_MEMORY))
# Stets heap_start <= heap_end <= heap_limit.
#if defined(SPVW_MIXED_BLOCKS_OPPOSITE)
# bzw. heap_limit <= heap_start <= heap_end.
#endif
# Der Speicher zwischen heap_start und heap_end ist belegt,
# der Speicher zwischen heap_end (bzw. heap_start) und heap_limit ist frei.
# heap_limit wird, wenn nötig, vergrößert (bzw. verkleinert).
#if !defined(SPVW_MIXED_BLOCKS_OPPOSITE)
# heap_hardlimit ist der größte bzw. kleinste zulässige Wert von heap_limit.
#endif
#else # defined(SPVW_MIXED_BLOCKS) && !defined(TRIVIALMAP_MEMORY)
# Stets heap_start <= heap_end.
# Der Speicher zwischen heap_start und heap_end ist belegt,
#endif
#ifdef GENERATIONAL_GC
#ifndef SPVW_MIXED_BLOCKS_OPPOSITE
# Die Generation 0 (ältere Generation) beginnt bei heap_gen0_start,
#                                      geht bis    heap_gen0_end.
# Die Generation 1 (neuere Generation) beginnt bei heap_gen1_start,
#                                      geht bis    heap_end.
# heap_gen0_start und heap_gen1_start sind durch physpagesize teilbar.
# Zwischen heap_gen0_end und heap_gen1_start ist eine Lücke von weniger als
# einer Page.
# heap_start ist entweder = heap_gen0_start oder = heap_gen1_start.
#else
# Die Generation 0 (ältere Generation) beginnt bei heap_gen0_start,
#                                      geht bis    heap_gen0_end.
# Bei mem.varobjects:
#   Generation 1 (neuere Generation) beginnt bei heap_gen1_start,
#                                    geht bis    heap_end.
#   heap_gen0_start und heap_gen1_start sind durch physpagesize teilbar.
#   Zwischen heap_gen0_end und heap_gen1_start ist eine Lücke von weniger als
#   einer Page.
#   heap_start ist entweder = heap_gen0_start oder = heap_gen1_start.
# Bei mem.conses:
    #define heap_gen1_end  heap_gen1_start
#   Generation 1 (neuere Generation) beginnt bei heap_start,
#                                    geht bis    heap_gen1_end.
#   heap_gen1_end und heap_gen0_end sind durch physpagesize teilbar.
#   Zwischen heap_gen1_end und heap_gen0_start ist eine Lücke von weniger als
#   einer Page.
#   heap_end ist entweder = heap_gen1_end oder = heap_gen0_end.
#endif
# Der Status von Adresse addr (heap_gen0_start <= addr < heap_gen0_end) wird
# von physpages[(addr>>physpageshift)-(heap_gen0_start>>physpageshift)] gegeben.
# physpages=NULL ist möglich, wenn nicht genügend Platz da war!
#endif

  #define map_heap(heap,pagevar,statement)  \
    { var Page* pagevar = &(heap).pages; statement; }

#endif
