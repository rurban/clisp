# Support for GENERATIONAL_GC, part 3.

# ------------------------------ Specification ---------------------------------

#ifdef GENERATIONAL_GC

# Update the pointers inside the old generation.
# This assumes the current meaning of the `update' macro!
  local void update_old_generation (void);

#endif

# ------------------------------ Implementation --------------------------------

#ifdef GENERATIONAL_GC

  local void update_at (object* ptr);
  local void update_at(ptr)
    var object* ptr;
    { update(ptr); }

  local void update_old_generation (void);
  local void update_old_generation()
    { var uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++)
        if (is_heap_containing_objects(heapnr)) # Objekte, die keine Pointer enthalten,
                                                # braucht man nicht zu durchlaufen.
          { var Heap* heap = &mem.heaps[heapnr];
            var aint gen0_start = heap->heap_gen0_start;
            var aint gen0_end = heap->heap_gen0_end;
            if (gen0_start < gen0_end)
              { if (heap->physpages==NULL)
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
    }     }   }   }

#endif
