# Support for GENERATIONAL_GC, part 2.

# ------------------------------ Specification ---------------------------------

#ifdef GENERATIONAL_GC

# Mark the objects pointed to by the old generation.
  local void gc_mark_old_generation (void);

#endif

# ------------------------------ Implementation --------------------------------

#ifdef GENERATIONAL_GC

  local void gc_mark_at (object* ptr);
  local void gc_mark_at(ptr)
    var object* ptr;
    { gc_mark(*ptr); }

  local void gc_mark_old_generation (void);
  local void gc_mark_old_generation()
    { var uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++)
        if (is_heap_containing_objects(heapnr)) # Objekte, die keine Pointer enthalten,
                                                # braucht man nicht zu durchlaufen.
          { var Heap* heap = &mem.heaps[heapnr];
            var aint gen0_start = heap->heap_gen0_start;
            var aint gen0_end = heap->heap_gen0_end;
            if (gen0_start < gen0_end)
              { if (heap->physpages==NULL)
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
    }     }   }   }

#endif
