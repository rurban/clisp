# General macro for object allocation.

# ------------------------------ Specification ---------------------------------

# Asks the OS for a piece of memory of need bytes, and verifies that it lies
# in the address range usable for Lisp objects. Returns NULL if couldn't
# satisfy the request.
  local void* mymalloc (uintL need);

# Allocates a Lisp object.
# allocate(type,flag,size,ptrtype,ptr,statement)
# > type: the typecode
# > flag: a literal boolean, TRUE for varobject, FALSE for two-pointer-object
# > size: the needed memory size (including header and alignment). Must be
#         a constant expression or a variable.
# ptrtype: the C type of `ptr'
# ptr: a C variable
# Fetches a piece of memory of size `size', suitable for a Lisp object of type
# `type', sets `ptr' to its start address. Then `statement' is executed.
# Finally `ptr' is combined with the type info and returned from the current
# function (via `return').

# ------------------------------ Implementation --------------------------------

# Fehlermeldung wegen vollen Speichers
  nonreturning_function(local, fehler_speicher_voll, (void));
  local void fehler_speicher_voll()
    { dynamic_bind(S(use_clcs),NIL); # SYS::*USE-CLCS* an NIL binden
      if (posfixnump(Symbol_value(S(gc_statistics_stern))))
        { dynamic_bind(S(gc_statistics_stern),Fixnum_0); } # SYS::*GC-STATISTICS* an 0 binden
      fehler(storage_condition,
             GETTEXT("No more room for LISP objects")
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
        { asciz_out(GETTEXT(NLstring "*** - " "No more room for LISP objects: RESET"));
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
      # Intervall [addr,addr+need-1] muss in [0..2^oint_addr_len-1] liegen:
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
          # Intervall [addr,addr+need-1] muss in [0..2^oint_addr_len-1] liegen:
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
            asciz_out(GETTEXT("Trying to make room through a GC..." NLstring));
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
            asciz_out(GETTEXT("Trying to make room through a GC..." NLstring));
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
            asciz_out(GETTEXT("Trying to make room through a GC..." NLstring));
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
