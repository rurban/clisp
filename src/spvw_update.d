# Updating all heap objects in the world (i.e. in the heap and stack).

# ------------------------------ Specification ---------------------------------

# For the following macros, the macro update(objptr) must be defined, with the
# signature:  local void update (object* objptr);

# Update all the world, except the heaps and the stacks.
#   update_tables();

# Update the cons heaps.
#   #define update_conspage ...
#   update_conses();
#   #undef update_conspage
# Some possible implementation of update_conspage.
#   update_conspage_normal

# Update the varobject heaps.
#   #define update_ht_invalid ...
#   #define update_fpointer_invalid ...
#   #define update_fp_invalid ...
#   #define update_fsubr_function ...
#   #define update_fs_function ...
#   #define update_page ...
#   update_varobjects();
#   #undef update_page
#   #undef update_fs_function
#   #undef update_fsubr_function
#   #undef update_fp_invalid
#   #undef update_fpointer_invalid
#   #undef update_ht_invalid
# Some possible implementation of update_page.
#   update_page_normal

# Update the list of pending weak pointers.
#   update_weakpointers();
# Same, but here update(objptr) may modify *objptr. and the
# value before update should be taken while following the list.
#   update_weakpointers_mod();

# Update the stacks.
#   #define update_stackobj ...
#   update_stacks();
#   #undef update_stackobj
# Some possible implementation of update_stackobj.
#   update_stackobj_normal

# ------------------------------ Implementation --------------------------------

    # Programmkonstanten aktualisieren:
      #define update_subr_tab()  \
        for_all_subrs(                                              \
          { var object* p = (object*)((aint)ptr+subr_const_offset); \
            var uintC c;                                            \
            dotimespC(c,subr_const_anz, { update(p); p++; } );      \
          }                                                         \
          );
      #define update_symbol_tab()  \
        for_all_constsyms( # symbol_tab durchgehen \
          { var object* p;                         \
            p = &ptr->symvalue; update(p);         \
            p = &ptr->symfunction; update(p);      \
            p = &ptr->proplist; update(p);         \
            p = &ptr->pname; update(p);            \
            p = &ptr->homepackage; update(p);      \
          }                                        \
          );
      #define update_object_tab()  \
        for_all_constobjs( update(objptr); ); # object_tab durchgehen \
        for_all_threadobjs( update(objptr); ); # Threads durchgehen
      #define update_tables()  \
        { update_subr_tab();   \
          update_symbol_tab(); \
          update_object_tab(); \
        }

    # Pointer in den Cons-Zellen aktualisieren:
      #define update_conspage_normal(page)  \
        { var aint objptr = page->page_start;  \
          var aint objptrend = page->page_end; \
          # alle Pointer im (neuen) CONS-Bereich start <= Adresse < end aktualisieren: \
          until (objptr==objptrend)            \
            { update((object*)objptr);         \
              objptr += sizeof(object);        \
              update((object*)objptr);         \
              objptr += sizeof(object);        \
        }   }
      #define update_conses()  \
        for_each_cons_page(page, \
          update_conspage(page)  \
          );

    # Pointer in den Objekten variabler Länge aktualisieren:
      #define update_page_normal(page,updater)  \
        { var aint ptr = page->page_start;                             \
          var aint ptrend = page->page_end;                            \
          # alle Objekte mit Adresse >=ptr, <ptrend durchgehen:        \
          until (ptr==ptrend) # solange bis ptr am Ende angekommen ist \
            { # nächstes Objekt mit Adresse ptr (< ptrend) durchgehen: \
              updater(typecode_at(ptr)); # und weiterrücken            \
        }   }
      # Unterroutinen:
        #define do_update_symbol()  \
          { var object* p = (object*)pointerplus(ptr,symbol_objects_offset);          \
            var uintC count;                                                          \
            dotimespC(count,((sizeof(symbol_)-symbol_objects_offset)/sizeof(object)), \
              { update(p); p++; } );                                                  \
          }
        #define do_update_svector()  \
          { var uintL count = svector_length((Svector)ptr);  \
            if (!(count==0))                                 \
              {var object* p = &((Svector)ptr)->data[0];     \
               dotimespL(count,count, { update(p); p++; } ); \
          }   }
        #define do_update_iarray()  \
          { var object* p = &((Iarray)ptr)->data; \
            update(p);                            \
          }
        #define do_update_record()  \
          { # Beim Aktualisieren von Pointern verliert der Aufbau von              \
            # Hash-Tables seine Gültigkeit (denn die Hashfunktion eines            \
            # Objekts hängt von seiner Adresse ab, die sich ja jetzt               \
            # verändert).                                                          \
            if (record_type((Record)ptr) == Rectype_Hashtable) # eine Hash-Table ? \
              { update_ht_invalid((Hashtable)ptr); } # ja -> für Reorganisation vormerken \
            elif (update_fpointer_invalid && (record_type((Record)ptr) == Rectype_Fpointer)) # Foreign-Pointer ? \
              { update_fp_invalid((Record)ptr); } # ja -> evtl. ungültig machen    \
            elif (update_fsubr_function && (record_type((Record)ptr) == Rectype_Fsubr)) # Fsubr ? \
              { update_fs_function((Fsubr)ptr); } # ja -> evtl. Adresse updaten    \
           {var uintC count = (record_type((Record)ptr) < rectype_limit ? srecord_length((Srecord)ptr) : xrecord_length((Xrecord)ptr)); \
            if (!(count==0))                                                       \
              { var object* p = &((Record)ptr)->recdata[0];                        \
                dotimespC(count,count, { update(p); p++; } );                      \
          }}  }
      # Aktualisiert das Objekt bei 'ptr', dessen Typcode durch 'type_expr'
      # gegeben wird, und rückt ptr weiter:
      #ifdef SPVW_MIXED
        #ifdef TYPECODES
          #define update_varobject(type_expr)  \
            { var tint type = (type_expr); # Typinfo                                  \
              var uintL laenge = objsize((Varobject)ptr); # Länge bestimmen           \
              var aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt              \
              # Fallunterscheidung nach:                                              \
                # Symbol; Simple-Vector; Nicht-simpler Array;                         \
                # Record (insbes. Hash-Table); Rest.                                  \
              switch (type)                                                           \
                { case_symbolwithflags:                                               \
                    # Symbol: alle Pointer aktualisieren                              \
                    do_update_symbol();                                               \
                    break;                                                            \
                  case_svector:                                                       \
                    # Simple-vector: alle Pointer aktualisieren                       \
                    do_update_svector();                                              \
                    break;                                                            \
                  case_mdarray: case_obvector: case_ostring: case_ovector:            \
                    # nicht-simpler Array: Datenvektor aktualisieren                  \
                    do_update_iarray();                                               \
                    break;                                                            \
                  case_record:                                                        \
                    # Record: alle Pointer aktualisieren                              \
                    do_update_record();                                               \
                    break;                                                            \
                  default:                                                            \
                    break; # alle anderen enthalten keine zu aktualisierenden Pointer \
                           # -> nichts tun                                            \
                }                                                                     \
              # zum nächsten Objekt weiterrücken                                      \
              ptr = newptr;                                                           \
            }
        #else
          #define update_varobject(type_expr)  \
            { var uintL laenge = objsize((Varobject)ptr); # Länge bestimmen     \
              var aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt        \
              switch (record_type((Record)ptr)) # Typ des nächsten Objekts      \
                { case Rectype_mdarray:                                         \
                  case Rectype_bvector:                                         \
                  case Rectype_string:                                          \
                  case Rectype_vector:                                          \
                    # nicht-simpler Array: Datenvektor aktualisieren            \
                    do_update_iarray();                                         \
                    break;                                                      \
                  case Rectype_Svector:                                         \
                    # Simple-vector: alle Pointer aktualisieren                 \
                    do_update_svector();                                        \
                    break;                                                      \
                  case Rectype_Sbvector:                                        \
                  case Rectype_Sstring: case Rectype_Imm_Sstring:               \
                  case Rectype_Imm_SmallSstring:                                \
                  case Rectype_Bignum: case Rectype_Ffloat:                     \
                  case Rectype_Dfloat: case Rectype_Lfloat:                     \
                    # enthalten keine zu aktualisierenden Pointer -> nichts tun \
                    break;                                                      \
                  default:                                                      \
                    # Record: alle Pointer aktualisieren                        \
                    do_update_record();                                         \
                    break;                                                      \
                }                                                               \
              # zum nächsten Objekt weiterrücken                                \
              ptr = newptr;                                                     \
            }
        #endif
        #define update_varobjects()  \
          for_each_varobject_page(page,        \
            update_page(page,update_varobject) \
            );
      #endif
      #ifdef SPVW_PURE
        #define update_symbol(type_expr)  # ignoriert type_expr \
          { var uintL laenge = objsize_symbol((void*)ptr); # Länge bestimmen \
            var aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt       \
            # Symbol: alle Pointer aktualisieren                             \
            do_update_symbol();                                              \
            ptr = newptr; # zum nächsten Objekt weiterrücken                 \
          }
        #define update_svector(type_expr)  # ignoriert type_expr \
          { var uintL laenge = objsize_svector((void*)ptr); # Länge bestimmen \
            var aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt        \
            # Simple-vector: alle Pointer aktualisieren                       \
            do_update_svector();                                              \
            ptr = newptr; # zum nächsten Objekt weiterrücken                  \
          }
        #define update_iarray(type_expr)  # ignoriert type_expr \
          { var uintL laenge = objsize_iarray((void*)ptr); # Länge bestimmen \
            var aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt       \
            # nicht-simpler Array: Datenvektor aktualisieren                 \
            do_update_iarray();                                              \
            ptr = newptr; # zum nächsten Objekt weiterrücken                 \
          }
        #define update_record(type_expr)  # ignoriert type_expr \
          { var uintL laenge = objsize_record((void*)ptr); # Länge bestimmen \
            var aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt       \
            # Record: alle Pointer aktualisieren                             \
            do_update_record();                                              \
            ptr = newptr; # zum nächsten Objekt weiterrücken                 \
          }
        #define update_varobjects()  \
          for_each_varobject_page(page,                                               \
            { # Fallunterscheidung nach:                                              \
                # Symbol; Simple-Vector; Nicht-simpler Array;                         \
                # Record (insbes. Hash-Table); Rest.                                  \
              switch (heapnr)                                                         \
                { case_symbol:                                                        \
                    update_page(page,update_symbol); break;                           \
                  case_svector:                                                       \
                    update_page(page,update_svector); break;                          \
                  case_mdarray: case_obvector: case_ostring: case_ovector:            \
                    update_page(page,update_iarray); break;                           \
                  case_record:                                                        \
                    update_page(page,update_record); break;                           \
                  default:                                                            \
                    break; # alle anderen enthalten keine zu aktualisierenden Pointer \
                           # -> nichts tun                                            \
            }   }                                                                     \
            );
      #endif

    # Weak-Pointer-Liste aktualisieren:
      #define update_weakpointers()  \
        { var object L = O(all_weakpointers);                             \
          until (eq(L,Fixnum_0))                                          \
            { var object* p = &TheRecord(L)->recdata[weakpointer_length]; \
              var uintC count;                                            \
              dotimespC(count,weakpointer_xlength/sizeof(object),         \
                { update(p); p++; }                                       \
                );                                                        \
              L = TheWeakpointer(L)->wp_cdr;                              \
        }   }
      #define update_weakpointers_mod()  \
        { var object L = O(all_weakpointers);                             \
          until (eq(L,Fixnum_0))                                          \
            { var object next = TheWeakpointer(L)->wp_cdr;                \
              var object* p = &TheRecord(L)->recdata[weakpointer_length]; \
              var uintC count;                                            \
              dotimespC(count,weakpointer_xlength/sizeof(object),         \
                { update(p); p++; }                                       \
                );                                                        \
              L = next;                                                   \
        }   }

    # STACKs aktualisieren:
      #define update_stackobj_normal(objptr)  \
        update(objptr);
      #define update_STACKs()  \
        for_all_STACKs(                                                               \
          until (eq(*objptr,nullobj)) # bis STACK zu Ende ist:                        \
            { if ( as_oint(*objptr) & wbit(frame_bit_o) ) # Beginnt hier ein Frame?   \
               { if (( as_oint(*objptr) & wbit(skip2_bit_o) ) == 0) # Ohne skip2-Bit? \
                  objptr skipSTACKop 2; # ja -> um 2 weiterrücken                     \
                  else                                                                \
                  objptr skipSTACKop 1; # nein -> um 1 weiterrücken                   \
               }                                                                      \
               else                                                                   \
               { # normales Objekt, aktualisieren:                                    \
                 update_stackobj(objptr);                                             \
                 objptr skipSTACKop 1; # weiterrücken                                 \
           }   }                                                                      \
         );
