# Garbage collector.

# ------------------------------ Specification ---------------------------------

# Execute a simple garbage collection.
# can trigger GC
  local void gar_col_simple (void);

# Execute a full garbage collection.
# can trigger GC
  global void gar_col (void);

#ifdef SPVW_PAGES
# Supplement a simple garbage collection with a compaction.
# can trigger GC
  local void gar_col_compact (void);
#endif

#if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && RESERVE
# Move the conses, to make a little more room.
  local void move_conses (sintL delta);
#endif

# ------------------------------ Implementation --------------------------------

# Gesamtstrategie:
# 1. Pseudorekursives Markieren durch Setzen von garcol_bit.
# 2. Verschieben der Objekte fester Länge (Conses u.ä.),
#    Durchrechnen der Verschiebungen der Objekte variabler Länge.
# 3. Aktualisieren der Pointer.
# 4. Durchführen der Verschiebungen der Objekte variabler Länge.

#include "spvw_genera1.c"

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
        { var object* dies_ = (object*)ThePointer(dies);        \
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
        { var object* dies_ = (object*)The(dies);               \
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
        { var object* dies_ = (object*)TheIarray(dies);         \
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
        { var object* dies_ = (object*)TheSvector(dies);        \
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
        { var object* dies_ = (object*)TheRecord(dies);         \
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
        { var object* dies_ = (object*)pointerplus(TheSubr(dies),subr_const_offset); \
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
                      case Rectype_Sstring: case Rectype_Imm_Sstring:
                      case Rectype_Imm_SmallSstring:
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

# Markierungsphase:
  # Es werden alle "aktiven" Strukturen markiert.
  # Aktiv ist alles, was erreichbar ist
  # - vom LISP-Stack aus  oder
  # - bei Generational-GC: von der alten Generation aus  oder
  # - als Programmkonstanten (dazu gehört auch die Liste aller Packages).
    local void gc_mark_stack (object* objptr);
    local void gc_mark_stack(objptr)
      var object* objptr;
      { until (eq(*objptr,nullobj)) # bis STACK zu Ende ist:
          { if ( as_oint(*objptr) & wbit(frame_bit_o) ) # Beginnt hier ein Frame?
             { if (( as_oint(*objptr) & wbit(skip2_bit_o) ) == 0) # Ohne skip2-Bit?
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

  #include "spvw_genera2.c"

  local void gc_markphase (void);
  local void gc_markphase()
    { 
      # Mark all the STACKs
      for_all_STACKs(gc_mark_stack(objptr));
      #ifdef GENERATIONAL_GC
      # Alte Generation markieren, wobei man sie sehr sparsam durchläuft:
      if (generation > 0) { gc_mark_old_generation(); }
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
            if (marked(pointerplus(TheSubr(obj),subr_const_offset)))
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
          case (cons_bias & immediate_bias):
            if (immediate_object_p(obj)) return TRUE;
            if (in_old_generation(obj,,1)) return TRUE;
            if (marked(ThePointer(obj))) return TRUE; else return FALSE;
          case subr_bias:
            if (marked(pointerplus(TheSubr(obj),subr_const_offset)))
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
                            *(object*)p2 = as_object((as_oint(obj) & nonimmediate_bias_mask) | (as_oint(*(object*)ThePointer(obj)) & ~wbit(garcol_bit_o) & ~(oint)nonimmediate_bias_mask));
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
                          *(object*)p2 = as_object((as_oint(obj) & nonimmediate_bias_mask) | (as_oint(*(object*)ThePointer(obj)) & ~wbit(garcol_bit_o) & ~(oint)nonimmediate_bias_mask));
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
        # Durch verkettete-Liste-Mechanismus: Am Schluss enthält
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
                    as_object((as_oint(obj) & nonimmediate_bias_mask) | (as_oint(*(object*)ThePointer(obj)) & ~wbit(garcol_bit_o))); \
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
                            *(object*)objptr = as_object((as_oint(obj) & nonimmediate_bias_mask) | (as_oint(*(object*)ThePointer(obj)) & ~wbit(garcol_bit_o) & ~(oint)nonimmediate_bias_mask)); \
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
                *objptr = as_object(as_oint(*objptr) | flags); # dann Flags wieder rein \
                break;                                            \
              }                                                   \
            default: update(objptr); break;                       \
          }
    #else
      #define update_stackobj(objptr)  \
        update(objptr);
    #endif
  # Aktualisieren der alten Generation:
    #include "spvw_genera3.c"

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
  #ifdef GNU # so lässt sich's besser optimieren
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

# Normale Garbage Collection durchführen:
  local void gar_col_normal(void);
  local void gar_col_normal()
    { var uintL gcstart_space; # belegter Speicher bei GC-Start
      var uintL gcend_space; # belegter Speicher bei GC-Ende
      var object all_weakpointers; # Liste der aktiven Weak-Pointer
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
        all_weakpointers = O(all_weakpointers); O(all_weakpointers) = Fixnum_0;
        all_finalizers = O(all_finalizers); O(all_finalizers) = Fixnum_0;
        #ifdef GC_CLOSES_FILES
        files_to_close = O(open_files); O(open_files) = NIL; # O(files_to_close) = NIL;
        #endif
        gc_markphase();
        # (noch unmarkierte) Liste all_finalizers aufspalten in zwei Listen:
        { var object Lu = all_finalizers;
          var object* L1 = &O(all_finalizers);
          var object* L2 = &O(pending_finalizers);
          until (eq(*L2,Fixnum_0))
            { L2 = &TheFinalizer(*L2)->fin_cdr; }
          until (eq(Lu,Fixnum_0))
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
          *L1 = Fixnum_0; *L2 = Fixnum_0;
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
        # (noch unmarkierte) Liste all_weakpointers verkürzen:
        { var object Lu = all_weakpointers;
          var object* L1 = &O(all_weakpointers);
          until (eq(Lu,Fixnum_0))
            { if (!alive(Lu))
                # The weak-pointer itself is being GCed. Don't care about its
                # contents. Remove it from the list.
                { Lu = TheWeakpointer(Lu)->wp_cdr; }
                else
                { if (!alive(TheWeakpointer(Lu)->wp_value))
                    # The referenced value is being GCed. Break the
                    # weak-pointer and remove it from the list.
                    { var object next = TheWeakpointer(Lu)->wp_cdr;
                      TheWeakpointer(Lu)->wp_cdr = unbound;
                      TheWeakpointer(Lu)->wp_value = unbound;
                      Lu = next;
                    }
                    else
                    # The referenced value is still alive. Keep the
                    # weak-pointer in the list.
                    { *L1 = Lu; L1 = &TheWeakpointer(Lu)->wp_cdr; Lu = *L1; }
                }
            }
          *L1 = Fixnum_0;
        }
        { var object L = O(all_weakpointers); # mark the list
          until (eq(L,Fixnum_0))
            { gc_mark(L); L = TheWeakpointer(L)->wp_cdr; }
        }
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
          # Update weak-pointers:
            update_weakpointers_mod();
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
                      { ptr = (aint)pointer_was_object(*(object*)ptr); }       \
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
        # Zum Schluss werden die Conses verschoben und gleichzeitig alle
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
      inc_gc_count(); # GCs mitzählen
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
      # make_space() erwartet, dass mem.total_room <= Länge der großen Lücke.
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
        inc_gc_space(freed); # dies zum 64-Bit-Akku gc_space addieren
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
            asciz_out(GETTEXT("munmap() fails."));
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
      until (eq(O(pending_finalizers),Fixnum_0))
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
      # die der Page in Phase 3 zuteil werden muss (>=0).
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
# Note: This function does not garbage collect anything; it only reorganizes
# the existing objects in fewer pages. Therefore it does not need to be
# wrapped in with_gc_statistics() calls like do_gar_col_simple and do_gar_col.
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
            update_STACKs();
          # Update weak-pointers:
            update_weakpointers_mod();
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
  #undef update_stackobj
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
              { switch (mtypecode(*(object*)(objptr)))                                      \
                  { case_pair: # Zwei-Pointer-Objekt?                                       \
                      *(object*)(objptr) = as_object(as_oint(*(object*)(objptr)) + odelta); \
                      break;                                                                \
                    default: break;                                                         \
              }   }
          #else
            #define update(objptr)  \
              { if (consp(*(object*)(objptr)))                                          \
                  *(object*)(objptr) = as_object(as_oint(*(object*)(objptr)) + odelta); \
              }
          #endif
        # Durchlaufen durch alle LISP-Objekte und aktualisieren:
          # Update pointers in all LISP-stacks:
            #define update_stackobj  update_stackobj_normal
            update_STACKs();
            #undef update_stackobj
          # Update weak-pointers:
            update_weakpointers_mod();
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
