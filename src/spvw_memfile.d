# Saving and loading of memory images.

# ------------------------------ Specification ---------------------------------

# Saves a memory image on diskette.
# savemem(stream);
# > object stream: open file output stream
# As a side effect, the stream is closed.
# can trigger GC
  global void savemem (object stream);

# Restores a memory image from diskette.
# loadmem(filename);
# This overwrites all Lisp data.
  local void loadmem (const char* filename);

# ------------------------------ Implementation --------------------------------

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
    #if 1 # defined(STRM_WR_SS)
      bit(10) |
    #endif
    # Codierung von strmtype:
    #if 1 # defined(HANDLES)
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
    #ifdef UNICODE
      bit(19) |
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
# can trigger GC
  global void savemem (object stream);
  global void savemem(stream)
    var object stream;
    { # Wir brauchen den Stream nur wegen des für ihn bereitgestellten Handles.
      # Wir müssen ihn aber im Fehlerfalle schließen (der Aufrufer macht kein
      # WITH-OPEN-FILE, sondern nur OPEN). Daher bekommen wir den ganzen
      # Stream übergeben, um ihn schließen zu können.
      var Handle handle = TheHandle(TheStream(stream)->strm_buffered_channel);
      pushSTACK(stream); # Stream retten
      # Erst eine GC ausführen:
      gar_col();
      #define WRITE(buf,len)  \
        { begin_system_call();                                       \
         {var sintL ergebnis = full_write(handle,(RW_BUF_T)buf,len); \
          if (!(ergebnis==(sintL)(len)))                             \
            { end_system_call();                                     \
              builtin_stream_close(&STACK_0);                        \
              if (ergebnis<0) { OS_file_error(TheStream(STACK_0)->strm_file_truename); } # Fehler aufgetreten?  \
              pushSTACK(TheStream(STACK_0)->strm_file_truename); # Wert für Slot PATHNAME von FILE-ERROR \
              fehler(file_error,                                     \
                     GETTEXT("disk full")                            \
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
          { var const char* ptr1 = module->name;
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
          if (ergebnis<0) { builtin_stream_close(&STACK_0); OS_file_error(TheStream(STACK_0)->strm_file_truename); } # Fehler?
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
          { switch (mtypecode(*(object*)objptr))                                     \
              { case_system:                                                         \
                  if (wbit_test(as_oint(*(object*)objptr),0+oint_addr_shift)) break; \
                case_subr:                                                           \
                case_machine:                                                        \
                  rheader.reloccount++;                                              \
                default:                                                             \
                  break;                                                             \
          }   }
        #define update_ht_invalid(obj)  rheader.htcount++;
        #define update_fp_invalid(obj)  rheader.fpcount++;
        #define update_fs_function(obj)  rheader.fscount++;
        update_conses();
        update_varobjects();
        update_weakpointers();
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
          { switch (mtypecode(*(object*)objptr))                                     \
              { case_system:                                                         \
                  if (wbit_test(as_oint(*(object*)objptr),0+oint_addr_shift)) break; \
                case_subr:                                                           \
                case_machine:                                                        \
                  *relocbufptr++ = (object*)objptr;                                  \
                default:                                                             \
                  break;                                                             \
          }   }
        #define update_ht_invalid(obj)  *htbufptr++ = (obj);
        #define update_fp_invalid(obj)  *fpbufptr++ = (obj);
        #define update_fs_function(obj)  *fsbufptr++ = (obj);
        update_conses();
        update_varobjects();
        update_weakpointers();
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
      builtin_stream_close(&STACK_0);
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
              { *objptr = as_object(as_oint(*objptr) + offset_symbols_o); break; }
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
              { *objptr = as_object(as_oint(*objptr) + offset_symbols_o); break; }
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
            *objptr = as_object(as_oint(*objptr) + offset_varobjects_o); break;
            #endif
          case_pair:
            # Zwei-Pointer-Objekt
            #ifdef SPVW_MIXED_BLOCKS
            *objptr = as_object(as_oint(*objptr) + offset_conses_o); break;
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
             *objptr = as_object(as_oint(*objptr) + offset_pages[pagenr].offset_page_o);
            }
            break;
            #endif
            #ifdef SPVW_PURE_BLOCKS # SINGLEMAP_MEMORY
            #ifdef SINGLEMAP_MEMORY_RELOCATE
            *objptr = as_object(as_oint(*objptr) + offset_heaps_o[mtypecode(*objptr)]); break;
            #else
            break; # Alles Bisherige erfährt keine Verschiebung
            #endif
            #endif
          case_subr: # SUBR
            {var oint addr = as_oint(*objptr);
             var offset_subrs_t* ptr = offset_subrs;
             var uintC count;
             dotimespC(count,offset_subrs_anz,
               { if ((ptr->low_o <= addr) && (addr < ptr->high_o))
                   { *objptr = as_object(as_oint(*objptr) + ptr->offset_o); goto found_subr; }
                 ptr++;
               });
            }
            # SUBR nicht gefunden -> #<UNBOUND>
            *objptr = unbound;
            found_subr:
            break;
          #ifdef TYPECODES
          case_system: # Frame-Pointer oder Read-Label oder System-Konstante
            if ((as_oint(*objptr) & wbit(0+oint_addr_shift)) ==0)
              # Frame-Pointer -> #<DISABLED>
              { *objptr = disabled; }
            break;
          #endif
          case_machine: # Pseudo-Funktion oder sonstiger Maschinenpointer
            # Umsetzung old_pseudofun_tab -> pseudofun_tab :
            { var object addr = *objptr;
              { var uintC i = pseudofun_anz;
                var const object* ptr = &old_pseudofun_tab.pointer[pseudofun_anz];
                until (i==0)
                  { i--;
                    if (eq(*--ptr,addr))
                      { *objptr = pseudofun_tab.pointer[i]; break; }
              }   }
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
    var const char* filename;
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
           GETTEXT("operating system error during load of initialisation file `%s'" NLstring),
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
            var const char* old_name = &module_names_buffer[0];
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
                        { asciz_out(GETTEXT("Cannot map the initialisation file into memory."));
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
           # Update weak-pointers:
             update_weakpointers();
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
         asciz_out(GETTEXT("operating system error during load of initialisation file" NLstring));
         errno_out(abbruch_errno);
        }
        goto abbruch_quit;
      abbruch2:
        asciz_out(program_name); asciz_out(": ");
        asciz_out(GETTEXT("initialisation file was not created by this version of LISP" NLstring));
        goto abbruch_quit;
      abbruch3:
        asciz_out(program_name); asciz_out(": ");
        asciz_out(GETTEXT("not enough memory for initialisation" NLstring));
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
