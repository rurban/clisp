# Memory management data structures, part 3: global data

# ------------------------------ Specification ---------------------------------

#ifdef TYPECODES
# Number of possible typecodes.
  #define typecount  bit(oint_type_len<=8 ? oint_type_len : 8)
#endif

# Number of heaps.
# heapcount
  #ifdef SPVW_MIXED
    # Two heaps: One for varobjects, one for two-pointer objects.
    #define heapcount  2
  #endif
  #ifdef SPVW_PURE
    # A heap for each possible typecode.
    #define heapcount  typecount
  #endif

# Global memory management data structures.
  local struct {

        # Lower limit of big allocated memory block.
        aint MEMBOT;

        # now comes the Lisp STACK

        # now room for the heaps containing Lisp objects.
        Heap heaps[heapcount];
        #ifdef SPVW_PURE
          sintB heaptype[heapcount];
          # for every typecode:
          #   0 for conses
          #   1 for varobjects containing object pointers
          #   2 for varobjects containing no pointers (only immediate data)
          #  -1 for SUBRs (gcinvariant)
          #  -2 for unused or immediate typecodes
        #endif
        #ifdef SPVW_MIXED
          #define varobjects  heaps[0] # objects of various lengths
          #define conses      heaps[1] # conses and other two-pointer objects
        #endif
        #if defined(SPVW_MIXED_BLOCKS) && defined(TYPECODES) && defined(GENERATIONAL_GC)
          sintB heapnr_from_type[typecount]; # Tabelle type -> heapnr
        #endif

        #if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && !defined(TRIVIALMAP_MEMORY)
          # now empty, free for Lisp objects.
          #define MEMRES  conses.heap_end
          # now the emergency reserve
          # Upper limit of big allocated memory block.
          aint MEMTOP;
        #endif

        # Statistical data, used for deciding when to start a GC.
        #if defined(SPVW_PURE_BLOCKS) || defined(TRIVIALMAP_MEMORY) || defined(GENERATIONAL_GC)
          uintL total_room; # wieviel Platz belegt werden darf, ohne dass GC nötig wird
          #ifdef GENERATIONAL_GC
            boolean last_gc_full; # ob die letzte GC eine volle war
            uintL last_gcend_space0; # wieviel Platz am Ende der letzten GC belegt war
            uintL last_gcend_space1; # (von Generation 0 bzw. Generation 1)
          #endif
        #endif
        #ifdef SPVW_PAGES
          Pages free_pages; # eine Liste freier normalgroßer Pages
          uintL total_space; # wieviel Platz die belegten Pages überhaupt enthalten
          uintL used_space; # wieviel Platz gerade belegt ist
          uintL last_gcend_space; # wieviel Platz am Ende der letzten GC belegt war
          boolean last_gc_compacted; # ob die letzte GC schon kompaktiert hat
          uintL gctrigger_space; # wieviel Platz belegt werden darf, bis die nächste GC nötig wird
        #endif

        #ifdef SELFMADE_MMAP
          Handle memfile_handle;
          boolean memfile_still_being_read;
        #endif

      }
      mem;

  #if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && !defined(TRIVIALMAP_MEMORY) && !defined(GENERATIONAL_GC)
    #define RESERVE       0x00800L  # 2 KByte Speicherplatz als Reserve
  #else
    #define RESERVE             0   # brauche keine präallozierte Reserve
  #endif
  #define MINIMUM_SPACE 0x10000L  # 64 KByte als minimaler Speicherplatz für LISP-Daten
  #ifdef TRIVIALMAP_MEMORY
    #define RESERVE_FOR_MALLOC 0x100000L  # lasse 1 MByte Adressraum frei, für malloc
  #endif

# Iteration through all heaps.
# for_each_heap(heapvar,statement);

# Iteration through all heaps containing varobjects.
# for_each_varobject_heap(heapvar,statement);

# Iteration through all heaps containing conses.
# for_each_cons_heap(heapvar,statement);

# Iteration through all pages.
# for_each_page(page, statement using 'var Page* page');

# Iteration through all pages containing varobjects.
# for_each_varobject_page(page, statement using 'var Page* page');

# Iteration through all pages containing conses.
# for_each_cons_page(page, statement using 'var Page* page');
# for_each_cons_page_reversed(page, statement using 'var Page* page');

# While iterating through all heaps (0 <= heapnr < heapcount):
# Determine the type of a heap.
# is_heap_containing_objects(heapnr)
# is_varobject_heap(heapnr)
# is_cons_heap(heapnr)
# is_unused_heap(heapnr)

# Consistency checks.
# CHECK_AVL_CONSISTENCY();
# CHECK_GC_CONSISTENCY();
# CHECK_GC_CONSISTENCY_2();
# CHECK_PACK_CONSISTENCY();

# Initializations.
  #ifdef SPVW_PURE
    local inline void init_mem_heaptypes (void);
  #endif
  #if defined(SPVW_MIXED_BLOCKS) && defined(TYPECODES) && defined(GENERATIONAL_GC)
    local inline void init_mem_heapnr_from_type (void);
  #endif

# ------------------------------ Implementation --------------------------------

# Gesamtspeicheraufteilung (teilweise veraltet):
# 1. C-Programm. Speicher wird vom Betriebssystem zugeteilt.
#    Nach Programmstart unverschieblich.
# 2. C-Stack. Speicher wird vom C-Programm geholt.
#    Unverschieblich.
# 3. C-Heap. Hier unbenutzt.
#ifdef SPVW_MIXED_BLOCKS
# 4. LISP-Stack und LISP-Daten.
#    4a. LISP-Stack. Unverschieblich.
#    4b. Objekte variabler Länge. (Unverschieblich).
#    4c. Conses u.ä. Verschieblich mit move_conses.
#    Speicher hierfür wird vom Betriebssystem angefordert (hat den Vorteil,
#    dass bei EXECUTE dem auszuführenden Fremdprogramm der ganze Speicher
#    zur Verfügung gestellt werden kann, den LISP gerade nicht braucht).
#    Auf eine Unterteilung in einzelne Pages wird hier verzichtet.
#          || LISP-      |Objekte         |->    leer  <-|Conses| Reserve |
#          || Stack      |variabler Länge !              ! u.ä. |         |
#          |STACK_BOUND  |         objects.end     conses.start |         |
#        MEMBOT   objects.start                           conses.end    MEMTOP
#endif
#ifdef SPVW_PURE_BLOCKS
# 4. LISP-Stack. Unverschieblich.
# 5. LISP-Daten. Für jeden Typ ein großer Block von Objekten.
#endif
#ifdef SPVW_MIXED_PAGES
# 4. LISP-Stack. Unverschieblich.
# 5. LISP-Daten.
#    Unterteilt in Pages für Objekte variabler Länge und Pages für Conses u.ä.
#endif
#ifdef SPVW_PURE_PAGES
# 4. LISP-Stack. Unverschieblich.
# 5. LISP-Daten. Unterteilt in Pages, die nur Objekte desselben Typs enthalten.
#endif

#ifdef SPVW_MIXED

# Iteration through heaps.
#define for_each_heap(heapvar,statement)  \
  { var uintL heapnr;                                        \
    for (heapnr=0; heapnr<heapcount; heapnr++)               \
      { var Heap* heapvar = &mem.heaps[heapnr]; statement; } \
  }
#define for_each_varobject_heap(heapvar,statement)  \
  { var Heap* heapvar = &mem.varobjects; statement; }
#define for_each_cons_heap(heapvar,statement)  \
  { var Heap* heapvar = &mem.conses; statement; }

# Iteration through pages.
#define for_each_page(pagevar,statement)  \
  { var uintL heapnr;                                \
    for (heapnr=0; heapnr<heapcount; heapnr++)       \
      map_heap(mem.heaps[heapnr],pagevar,statement); \
  }
#define for_each_varobject_page(pagevar,statement)  \
  map_heap(mem.varobjects,pagevar,statement)
#define for_each_cons_page(pagevar,statement)  \
  map_heap(mem.conses,pagevar,statement)
#define for_each_cons_page_reversed for_each_cons_page

# Heap classification.
  #define is_heap_containing_objects(heapnr)  (TRUE)
  #define is_varobject_heap(heapnr)  ((heapnr)==0)
  #define is_cons_heap(heapnr)  ((heapnr)==1)
  #define is_unused_heap(heapnr)  (FALSE)

#endif

#ifdef SPVW_PURE

# During iterations, `heapnr' is the number of the heap.

# Iteration through heaps.
#define for_each_heap(heapvar,statement)  \
  { var uintL heapnr;                                          \
    for (heapnr=0; heapnr<heapcount; heapnr++)                 \
      if (mem.heaptype[heapnr] >= 0)                           \
        { var Heap* heapvar = &mem.heaps[heapnr]; statement; } \
  }
#define for_each_varobject_heap(heapvar,statement)  \
  { var uintL heapnr;                                          \
    for (heapnr=0; heapnr<heapcount; heapnr++)                 \
      if (mem.heaptype[heapnr] > 0)                            \
        { var Heap* heapvar = &mem.heaps[heapnr]; statement; } \
  }
#define for_each_cons_heap(heapvar,statement)  \
  { var uintL heapnr;                                          \
    for (heapnr=0; heapnr<heapcount; heapnr++)                 \
      if (mem.heaptype[heapnr] == 0)                           \
        { var Heap* heapvar = &mem.heaps[heapnr]; statement; } \
  }

# Iteration through pages.
#define for_each_page(pagevar,statement)  \
  { var uintL heapnr;                                  \
    for (heapnr=0; heapnr<heapcount; heapnr++)         \
      if (mem.heaptype[heapnr] >= 0)                   \
        map_heap(mem.heaps[heapnr],pagevar,statement); \
  }
#define for_each_varobject_page(pagevar,statement)  \
  { var uintL heapnr;                                  \
    for (heapnr=0; heapnr<heapcount; heapnr++)         \
      if (mem.heaptype[heapnr] > 0)                    \
        map_heap(mem.heaps[heapnr],pagevar,statement); \
  }
#define for_each_cons_page(pagevar,statement)  \
  { var uintL heapnr;                                  \
    for (heapnr=0; heapnr<heapcount; heapnr++)         \
      if (mem.heaptype[heapnr] == 0)                   \
        map_heap(mem.heaps[heapnr],pagevar,statement); \
  }
#define for_each_cons_page_reversed(pagevar,statement)  \
  { var uintL heapnr;                                  \
    for (heapnr=heapcount; heapnr-- > 0; )             \
      if (mem.heaptype[heapnr] == 0)                   \
        map_heap(mem.heaps[heapnr],pagevar,statement); \
  }

# Heap classification.
  #define is_heap_containing_objects(heapnr)  ((mem.heaptype[heapnr] >= 0) && (mem.heaptype[heapnr] < 2))
  #define is_cons_heap(heapnr)  (mem.heaptype[heapnr] == 0)
  #define is_varobject_heap(heapnr)  (mem.heaptype[heapnr] > 0)
  #define is_unused_heap(heapnr)  (mem.heaptype[heapnr] < 0)

#endif

# Überprüfung des Speicherinhalts auf GC-Festigkeit:
  #if defined(SPVW_PAGES) && defined(DEBUG_SPVW)
    # Überprüfen, ob die Verwaltung der Pages in Ordnung ist:
      #define CHECK_AVL_CONSISTENCY()  check_avl_consistency()
      local void check_avl_consistency (void);
      local void check_avl_consistency()
        {
          #ifdef DEBUG_AVL
          var uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { AVL(AVLID,check) (mem.heaps[heapnr].inuse); }
          #endif
        }
    # Überprüfen, ob die Grenzen der Pages in Ordnung sind:
      #define CHECK_GC_CONSISTENCY()  check_gc_consistency()
      local void check_gc_consistency (void);
      local void check_gc_consistency()
        { for_each_page(page,
            if ((sintL)page->page_room < 0)
              { asciz_out_1("\nPage bei Adresse 0x%x übergelaufen!!\n",page); abort(); }
            if (!(page->page_start == page_start0(page)))
              { asciz_out_1("\nPage bei Adresse 0x%x inkonsistent!!\n",page); abort(); }
            if (!(page->page_end + page->page_room
                  == round_down(page->m_start + page->m_length,varobject_alignment)
               ) )
              { asciz_out_1("\nPage bei Adresse 0x%x inkonsistent!!\n",page); abort(); }
            );
        }
    # Überprüfen, ob während der kompaktierenden GC
    # die Grenzen der Pages in Ordnung sind:
      #define CHECK_GC_CONSISTENCY_2()  check_gc_consistency_2()
      local void check_gc_consistency_2 (void);
      local void check_gc_consistency_2()
        { for_each_page(page,
            if ((sintL)page->page_room < 0)
              { asciz_out_1("\nPage bei Adresse 0x%x übergelaufen!!\n",page); abort(); }
            if (!(page->page_end + page->page_room - (page->page_start - page_start0(page))
                  == round_down(page->m_start + page->m_length,varobject_alignment)
               ) )
              { asciz_out_1("\nPage bei Adresse 0x%x inkonsistent!!\n",page); abort(); }
            );
        }
  #else
    #define CHECK_AVL_CONSISTENCY()
    #define CHECK_GC_CONSISTENCY()
    #define CHECK_GC_CONSISTENCY_2()
  #endif
  #ifdef DEBUG_SPVW
    # Überprüfen, ob die Tabellen der Packages halbwegs in Ordnung sind:
      #define CHECK_PACK_CONSISTENCY()  check_pack_consistency()
      global void check_pack_consistency (void);
      global void check_pack_consistency()
        { var object plist = O(all_packages);
          while (consp(plist))
            { var object pack = Car(plist);
              var object symtabs[2];
              var uintC i;
              symtabs[0] = ThePackage(pack)->pack_external_symbols;
              symtabs[1] = ThePackage(pack)->pack_internal_symbols;
              for (i = 0; i < 2; i++)
                { var object symtab = symtabs[i];
                  var object table = TheSvector(symtab)->data[1];
                  var uintL index = Svector_length(table);
                  until (index==0)
                    { var object entry = TheSvector(table)->data[--index];
                      var uintC count = 0;
                      while (consp(entry))
                        { if (!symbolp(Car(entry))) abort();
                          entry = Cdr(entry);
                          count++; if (count>=10000) abort();
                }   }   }
              plist = Cdr(plist);
        }   }
  #else
      #define CHECK_PACK_CONSISTENCY()
  #endif

# Initializations.
  #ifdef SPVW_PURE
    local inline void init_mem_heaptypes (void);
    local inline void init_mem_heaptypes()
      { var uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++)
          { switch (heapnr)
              { case_sstring:
                case_sbvector:
                case_bignum:
                #ifndef WIDE
                case_ffloat:
                #endif
                case_dfloat:
                case_lfloat:
                  mem.heaptype[heapnr] = 2; break;
                case_ostring:
                case_obvector:
                case_vector:
                case_mdarray:
                case_record:
                case_symbol:
                  mem.heaptype[heapnr] = 1; break;
                case_pair:
                  mem.heaptype[heapnr] = 0; break;
                case_subr:
                  mem.heaptype[heapnr] = -1; break;
                default:
                  mem.heaptype[heapnr] = -2; break;
          }   }
      }
  #endif
  #if defined(SPVW_MIXED_BLOCKS) && defined(TYPECODES) && defined(GENERATIONAL_GC)
    local inline void init_mem_heapnr_from_type (void);
    local inline void init_mem_heapnr_from_type()
      { var uintL type;
        for (type = 0; type < typecount; type++)
          {
            #ifdef MULTIMAP_MEMORY
            switch (type)
              { MM_TYPECASES break;
                default: mem.heapnr_from_type[type] = -1; continue;
              }
            #endif
            switch (type)
              { case_pair: mem.heapnr_from_type[type] = 1; break;
                default:   mem.heapnr_from_type[type] = 0; break;
      }   }   }
  #endif
