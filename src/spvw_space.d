# Statistics about heap space usage.

# ------------------------------ Specification ---------------------------------

# Returns the size of the memory occupied by static (gcinvariant) heap objects.
  global uintL static_space (void);

# Returns the size of the memory occupied by (dynamically) allocated heap
# objects.
  global uintL used_space (void);

# Returns the size of memory available until the next GC occurs.
  global uintL free_space (void);

#ifdef SPVW_PAGES
# Recomputes mem.used_space and mem.total_space.
# > boolean check: if true, mem.used_space must remain the same.
  local void recalc_space (boolean check);
#endif

# ------------------------------ Implementation --------------------------------

  global uintL static_space (void);
  global uintL static_space()
    { var uintL sum = 0;
      # Platz von symbol_tab: vgl. Macro for_all_constsyms
      sum += symbol_anz * sizeof(symbol_);
      # Platz von subr_tab: vgl. Macro for_all_subrs
        #ifdef MAP_MEMORY_TABLES
          sum += total_subr_anz * sizeof(subr_);
        #else
          { var module_* module; # modules durchgehen
            for_modules(all_modules,
              { if (module->initialized)
                  { sum += *module->stab_size * sizeof(subr_); }
              });
          }
        #endif
      return sum;
    }

  global uintL used_space (void);
  #ifdef SPVW_BLOCKS
   #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
    global uintL used_space()
      {
        #if !defined(GENERATIONAL_GC)
          #define Heap_used_space(h)  ((uintL)((h).pages.end - (h).pages.start))
          return Heap_used_space(mem.varobjects) # Platz für Objekte variabler Länge
                 + Heap_used_space(mem.conses); # Platz für Conses
        #else # defined(GENERATIONAL_GC)
          return (uintL)(mem.varobjects.heap_gen0_end - mem.varobjects.heap_gen0_start)
                 + (uintL)(mem.varobjects.heap_end - mem.varobjects.heap_gen1_start)
                 + (uintL)(mem.conses.heap_gen1_end - mem.conses.heap_start)
                 + (uintL)(mem.conses.heap_gen0_end - mem.conses.heap_gen0_start);
        #endif
      }
   #else
    global uintL used_space()
      { var uintL sum = 0;
        #if !defined(GENERATIONAL_GC)
          for_each_page(page, { sum += page->page_end - page->page_start; } );
        #else # defined(GENERATIONAL_GC)
          for_each_heap(heap,
            { sum += (heap->heap_gen0_end - heap->heap_gen0_start)
                     + (heap->heap_end - heap->heap_gen1_start);
            });
        #endif
        return sum;
      }
   #endif
  #endif
  #ifdef SPVW_PAGES
    #if 0
    global uintL used_space()
      { var uintL sum = 0;
        for_each_page(page, { sum += page->page_end - page->page_start; } );
        return sum;
      }
    #else
    # Da die Berechnung von used_space() auf jede Page einmal zugreift, was
    # viel Paging bedeuten kann, wird das Ergebnis in mem.used_space gerettet.
    global uintL used_space()
      { return mem.used_space; }
    #endif
  #endif

  global uintL free_space (void);
  #ifdef SPVW_BLOCKS
   #if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && !defined(TRIVIALMAP_MEMORY) && !defined(GENERATIONAL_GC)
    global uintL free_space()
      { return (mem.conses.heap_start-mem.varobjects.heap_end); } # Platz in der großen Lücke
   #else
    global uintL free_space()
      { return mem.total_room; } # Platz, der bis zur nächsten GC verbraucht werden darf
   #endif
  #endif
  #ifdef SPVW_PAGES
    #if 0
    global uintL free_space()
      { var uintL sum = 0;
        for_each_page(page, { sum += page->page_room; } );
        return sum;
      }
    #else
    # Da die Berechnung von free_space() auf jede Page einmal zugreift, was
    # viel Paging bedeuten kann, wird das Ergebnis mit Hilfe von mem.used_space
    # berechnet.
    global uintL free_space()
      { return mem.total_space - mem.used_space; }
    #endif
  #endif

#ifdef SPVW_PAGES
  local void recalc_space (boolean check);
  local void recalc_space(check)
    var boolean check;
    { var uintL sum_used = 0;
      var uintL sum_free = 0;
      for_each_page(page,
                    { sum_used += page->page_end - page->page_start;
                      sum_free += page->page_room;
                    }
                   );
      if (check)
        { if (!(mem.used_space == sum_used)) abort(); }
        else
        { mem.used_space = sum_used; }
      mem.total_space = sum_used + sum_free;
    }
#endif
