# Listenfunktionen von CLISP
# Bruno Haible 1990-1999
# Marcus Daniels 8.4.1994

#include "lispbibl.c"

# UP: Kopiert eine Liste
# copy_list(list)
# > list: Liste
# < ergebnis: Kopie der Liste
# can trigger GC
  global object copy_list (object list);
  global object copy_list(old_list)
    var object old_list;
    { # Methode: (copy-list l) = (mapcar #'identity l), mapcar vorwärts
      if (atomp(old_list))
        return old_list;
        else # Liste mit mindestens einem Element
        { var object lauf;
          pushSTACK(old_list);
          #define old_list  STACK_0
          { var object new_list = allocate_cons();
            lauf = old_list; # lauf läuft durch die alte Liste durch
            #undef old_list
            Car(new_list) = Car(lauf);
            STACK_0 = new_list;
            pushSTACK(new_list);
          }
          # Schleife: STACK_1 ist die Gesamtkopie, STACK_0 = LAST davon,
          # lauf = das entsprechende Cons der Original-Liste.
          while ( lauf=Cdr(lauf), consp(lauf) )
            { # es kommt noch ein Cons
              pushSTACK(lauf); # lauf retten
             {var object new_cons = allocate_cons(); # neues Cons allozieren
              lauf = popSTACK(); # lauf zurück
              Cdr(STACK_0) = new_cons; # und als CDR des LAST einhängen
              Car(new_cons) = Car(lauf); # CAR kopieren
              STACK_0 = new_cons; # das ist nun das neue LAST
            }}
          Cdr(popSTACK()) = lauf; # selben (CDR (LAST old_list)) beibehalten
          return popSTACK();
    }   }

# UP: Dreht eine Liste konstruktiv um.
# reverse(list)
# > list: Liste (x1 ... xm)
# < ergebnis: umgedrehte Liste (xm ... x1)
# can trigger GC
  global object reverse (object list);
  global object reverse(list)
    var object list;
    { pushSTACK(list); pushSTACK(NIL);
      loop
        { # Hier ist für r=0,1,...,m
            # STACK_0 = (xr ... x1), STACK_1 = list = (xr+1 ... xm)
          if atomp(list) break;
          # Hier ist für r=1,...,m:
            # STACK_0 = (xr-1 ... x1), list = (xr ... xm)
          STACK_1 = Cdr(list);
          # Hier ist für r=1,...,m:
            # STACK_0 = (xr-1 ... x1), STACK_1 = (xr+1 ... xm)
          pushSTACK(Car(list));
         {var object new_cons = allocate_cons();
          Car(new_cons) = popSTACK(); # = xr
          Cdr(new_cons) = STACK_0; # = (xr-1 ... x1)
          STACK_0 = new_cons; # = (xr ... x1)
         }
          list = STACK_1; # list := (xr+1 ... xm)
        }
      list = popSTACK(); skipSTACK(1); return(list);
    }
#if 0
# andere Möglichkeit:
  global object reverse(list)
    var object list;
    { pushSTACK(list); pushSTACK(NIL);
      while (mconsp(STACK_1))
        { var object new_cons = allocate_cons();
          var object old_cons = STACK_1;
          STACK_1 = Cdr(old_cons);
          Car(new_cons) = Car(old_cons);
          Cdr(new_cons) = STACK_0;
          STACK_0 = new_cons;
        }
      list = popSTACK(); skipSTACK(1); return(list);
    }
#endif

# UP: Bestimmt die Länge einer Liste
# llength(obj)
# > obj: Objekt
# < uintL ergebnis: Länge von obj, als Liste aufgefasst
# Testet nicht auf zyklische Listen.
  global uintL llength (object obj);
  global uintL llength(list)
    var object list;
    { var uintL count = 0;
      while (consp(list)) { count++; list=Cdr(list); }
      return count;
    }

# UP: Bildet eine Liste mit genau len Elementen
# make_list(len)
# > (STACK): Initialisierungswert für die Elemente
# > uintL len: gewünschte Listenlänge
# < ergebnis: Liste mit D1.L Elementen
# can trigger GC
  global object make_list (uintL len);
  global object make_list(len)
    var uintL len;
    { pushSTACK(NIL);
      dotimesL(len,len,
        { # STACK_0 = bisherige Liste, STACK_1 = Initialisierungswert
          var object new_cons = allocate_cons();
          Car(new_cons) = STACK_1; Cdr(new_cons) = STACK_0;
          STACK_0 = new_cons;
        });
      return popSTACK();
    }

# UP: Dreht eine Liste destruktiv um.
# nreverse(list)
# > list: Liste (x1 ... xm)
# < ergebnis: Liste (xm ... x1), EQ zur alten
  global object nreverse (object list);
  global object nreverse(list)
    var object list;
    # Methode:
    # (lambda (L)
    #   (cond ((atom L) L)
    #         ((atom (cdr L)) L)
    #         ((atom (cddr L)) (rotatef (car L) (cadr L)) L)
    #         (t (let ((L1 (cdr L)))
    #              (do ((L3 L1 (cdr L3))
    #                   (L2 nil (rplacd L3 L2)))
    #                  ((atom (cdr L3))
    #                   (setf (cdr L) L2)
    #                   (setf (cdr L1) L3)
    #                   (rotatef (car L) (car L3))
    #              )   )
    #              L
    # ) )     )  )
    { if (consp(list)) # (atom L) -> L
        { var object list3 = Cdr(list); # L3 := (cdr L)
          if (consp(list3)) # (atom (cdr L)) -> L
            { if (mconsp(Cdr(list3)))
                { var object list1 = list3; # mit L1 = L3 = (cdr L)
                  var object list2 = NIL; # und L2 = NIL anfangen
                  do { var object h = Cdr(list3); # (cdr L3) retten,
                       Cdr(list3) = list2; # durch L2 ersetzen,
                       list2 = list3; # L2 := altes L3
                       list3 = h; # L3 := altes (cdr L3)
                     }
                     while (mconsp(Cdr(list3))); # (atom (cdr L3)) -> beenden
                  # L3 ist das letzte und L2 das vorletzte Listen-Cons.
                  Cdr(list) = list2; # (setf (cdr L) L2)
                  Cdr(list1) = list3; # (setf (cdr L1) L3)
                }
              # vertausche (car list) und (car list3):
              { var object h = Car(list);
                Car(list) = Car(list3);
                Car(list3) = h;
        }   } }
      return list;
    }

# UP: A0 := (nreconc A0 A1)
# nreconc(list,obj)
# > list: Liste
# > obj: Objekt
# < ergebnis: (nreconc A0 A1)
  global object nreconc (object list, object obj);
  global object nreconc(list,obj)
    var object list;
    var object obj;
  {
    if (consp(list)) { # (atom L) -> L
      var object list3 = Cdr(list); # L3 := (cdr L)
      if (consp(list3)) { # (atom (cdr L)) -> L
        if (mconsp(Cdr(list3))) {
          var object list1 = list3; # mit L1 = L3 = (cdr L)
          var object list2 = NIL; # und L2 = NIL anfangen
          do {
            var object h = Cdr(list3); # (cdr L3) retten,
            Cdr(list3) = list2; # durch L2 ersetzen,
            list2 = list3; # L2 := altes L3
            list3 = h; # L3 := altes (cdr L3)
          } while (mconsp(Cdr(list3))); # (atom (cdr L3)) -> beenden
          # L3 ist das letzte und L2 das vorletzte Listen-Cons.
          Cdr(list) = list2; # (setf (cdr L) L2)
          Cdr(list1) = list3; # (setf (cdr L1) L3)
        }
        # vertausche (car list) und (car list3):
        { var object h = Car(list);
          Car(list) = Car(list3);
          Car(list3) = h;
        }
        Cdr(list3) = obj; # (setf (cdr L3) O)
      } else {
        Cdr(list) = obj;
      }
      return list;
    } else
      return obj;
  }

# UP: Bilde (delete obj (the list list) :test #'EQ)
# deleteq(list,obj)
# Entferne aus der Liste list alle Elemente, die EQ zu obj sind.
# > obj: zu streichendes Element
# > list: Liste
# < ergebnis: modifizierte Liste
  global object deleteq (object list, object obj);
  global object deleteq(list,obj)
    var object list;
    var object obj;
    { var object list1 = list;
      var object list2 = list;
      loop
        { # Hier ist entweder list1=list2=list oder (cdr list1) = list2.
          if (atomp(list2)) break;
          if (eq(Car(list2),obj))
            # Streiche (car list2):
            if (eq(list2,list))
              # noch am Listenanfang
              { list2 = list1 = list = Cdr(list2); }
              else
              # weiter hinten in der Liste
              { Cdr(list1) = list2 = Cdr(list2); }
            else
            # Nichts streichen, weiterrücken:
            { list1 = list2; list2 = Cdr(list2); }
        }
      return list;
    }

# UP: Liefert (car obj), mit Typprüfung
# > subr_self: Aufrufer (ein SUBR)
  local object car (object obj);
  local object car(obj)
    var object obj;
    { if (consp(obj)) return Car(obj);
      else if (nullp(obj)) return obj;
           else fehler_list(obj);
    }

# UP: Liefert (cdr obj), mit Typprüfung
# > subr_self: Aufrufer (ein SUBR)
  local object cdr (object obj);
  local object cdr(obj)
    var object obj;
    { if (consp(obj)) return Cdr(obj);
      else if (nullp(obj)) return obj;
           else fehler_list(obj);
    }

LISPFUNN(car,1) # (CAR list), CLTL S. 262
  { value1 = car(popSTACK()); mv_count=1; }

LISPFUNN(cdr,1) # (CDR list), CLTL S. 262
  { value1 = cdr(popSTACK()); mv_count=1; }

LISPFUNN(caar,1) # (CAAR list), CLTL S. 263
  { value1 = car(car(popSTACK())); mv_count=1; }

LISPFUNN(cadr,1) # (CADR list), CLTL S. 263
  { value1 = car(cdr(popSTACK())); mv_count=1; }

LISPFUNN(cdar,1) # (CDAR list), CLTL S. 263
  { value1 = cdr(car(popSTACK())); mv_count=1; }

LISPFUNN(cddr,1) # (CDDR list), CLTL S. 263
  { value1 = cdr(cdr(popSTACK())); mv_count=1; }

LISPFUNN(caaar,1) # (CAAAR list), CLTL S. 263
  { value1 = car(car(car(popSTACK()))); mv_count=1; }

LISPFUNN(caadr,1) # (CAADR list), CLTL S. 263
  { value1 = car(car(cdr(popSTACK()))); mv_count=1; }

LISPFUNN(cadar,1) # (CADAR list), CLTL S. 263
  { value1 = car(cdr(car(popSTACK()))); mv_count=1; }

LISPFUNN(caddr,1) # (CADDR list), CLTL S. 263
  { value1 = car(cdr(cdr(popSTACK()))); mv_count=1; }

LISPFUNN(cdaar,1) # (CDAAR list), CLTL S. 263
  { value1 = cdr(car(car(popSTACK()))); mv_count=1; }

LISPFUNN(cdadr,1) # (CDADR list), CLTL S. 263
  { value1 = cdr(car(cdr(popSTACK()))); mv_count=1; }

LISPFUNN(cddar,1) # (CDDAR list), CLTL S. 263
  { value1 = cdr(cdr(car(popSTACK()))); mv_count=1; }

LISPFUNN(cdddr,1) # (CDDDR list), CLTL S. 263
  { value1 = cdr(cdr(cdr(popSTACK()))); mv_count=1; }

LISPFUNN(caaaar,1) # (CAAAAR list), CLTL S. 263
  { value1 = car(car(car(car(popSTACK())))); mv_count=1; }

LISPFUNN(caaadr,1) # (CAAADR list), CLTL S. 263
  { value1 = car(car(car(cdr(popSTACK())))); mv_count=1; }

LISPFUNN(caadar,1) # (CAADAR list), CLTL S. 263
  { value1 = car(car(cdr(car(popSTACK())))); mv_count=1; }

LISPFUNN(caaddr,1) # (CAADDR list), CLTL S. 263
  { value1 = car(car(cdr(cdr(popSTACK())))); mv_count=1; }

LISPFUNN(cadaar,1) # (CADAAR list), CLTL S. 263
  { value1 = car(cdr(car(car(popSTACK())))); mv_count=1; }

LISPFUNN(cadadr,1) # (CADADR list), CLTL S. 263
  { value1 = car(cdr(car(cdr(popSTACK())))); mv_count=1; }

LISPFUNN(caddar,1) # (CADDAR list), CLTL S. 263
  { value1 = car(cdr(cdr(car(popSTACK())))); mv_count=1; }

LISPFUNN(cadddr,1) # (CADDDR list), CLTL S. 263
  { value1 = car(cdr(cdr(cdr(popSTACK())))); mv_count=1; }

LISPFUNN(cdaaar,1) # (CDAAAR list), CLTL S. 263
  { value1 = cdr(car(car(car(popSTACK())))); mv_count=1; }

LISPFUNN(cdaadr,1) # (CDAADR list), CLTL S. 263
  { value1 = cdr(car(car(cdr(popSTACK())))); mv_count=1; }

LISPFUNN(cdadar,1) # (CDADAR list), CLTL S. 263
  { value1 = cdr(car(cdr(car(popSTACK())))); mv_count=1; }

LISPFUNN(cdaddr,1) # (CDADDR list), CLTL S. 263
  { value1 = cdr(car(cdr(cdr(popSTACK())))); mv_count=1; }

LISPFUNN(cddaar,1) # (CDDAAR list), CLTL S. 263
  { value1 = cdr(cdr(car(car(popSTACK())))); mv_count=1; }

LISPFUNN(cddadr,1) # (CDDADR list), CLTL S. 263
  { value1 = cdr(cdr(car(cdr(popSTACK())))); mv_count=1; }

LISPFUNN(cdddar,1) # (CDDDAR list), CLTL S. 263
  { value1 = cdr(cdr(cdr(car(popSTACK())))); mv_count=1; }

LISPFUNN(cddddr,1) # (CDDDDR list), CLTL S. 263
  { value1 = cdr(cdr(cdr(cdr(popSTACK())))); mv_count=1; }

LISPFUNN(cons,2) # (CONS obj1 obj2), CLTL S. 264
  { var object new_cons = allocate_cons();
    Cdr(new_cons) = popSTACK();
    Car(new_cons) = popSTACK();
    value1 = new_cons; mv_count=1;
  }

# Unterprogramm zum Ausführen des Tests :TEST
# up2_test(stackptr,arg1,arg2)
# > *(stackptr+1): die Testfunktion
# > arg1,arg2: Argumente
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# can trigger GC
  local boolean up2_test (const object* stackptr, object arg1, object arg2);
  local boolean up2_test(stackptr,arg1,arg2)
    var const object* stackptr;
    var object arg1;
    var object arg2;
    { pushSTACK(arg1); pushSTACK(arg2); funcall(*(stackptr STACKop 1),2);
      if (nullp(value1)) return FALSE; else return TRUE;
    }

# Unterprogramm zum Ausführen des Tests :TEST-NOT
# up2_test_not(stackptr,arg1,arg2)
# > *(stackptr+0): die Testfunktion
# > arg1,arg2: Argumente
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# can trigger GC
  local boolean up2_test_not (const object* stackptr, object arg1, object arg2);
  local boolean up2_test_not(stackptr,arg1,arg2)
    var const object* stackptr;
    var object arg1;
    var object arg2;
    { pushSTACK(arg1); pushSTACK(arg2); funcall(*(stackptr STACKop 0),2);
      if (nullp(value1)) return TRUE; else return FALSE;
    }

# UP: Überprüft die :TEST, :TEST-NOT - Argumente
# test_test2_args(stackptr)
# > stackptr: Pointer in den STACK
# > *(stackptr+1): :TEST-Argument
# > *(stackptr+0): :TEST-NOT-Argument
# > subr_self: Aufrufer (ein SUBR)
# < *(stackptr+1): verarbeitetes :TEST-Argument
# < *(stackptr+0): verarbeitetes :TEST-NOT-Argument
# < up2_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#       > stackptr: derselbe Pointer in den Stack, arg1, arg2: Argumente
#       < TRUE, falls der Test erfüllt ist, FALSE sonst.
  # up2_function sei der Typ der Adresse einer solchen Testfunktion:
  typedef boolean (*up2_function) (const object* stackptr, object arg1, object arg2);
  local up2_function test_test2_args (object* stackptr);
  local up2_function test_test2_args(stackptr)
    var object* stackptr;
    { var object test_arg = *(stackptr STACKop 1);
      if (eq(test_arg,unbound)) { test_arg=NIL; }
      # test_arg ist das :TEST-Argument
     {var object test_not_arg = *(stackptr STACKop 0);
      if (eq(test_not_arg,unbound)) { test_not_arg=NIL; }
      # test_not_arg ist das :TEST-NOT-Argument
      if (nullp(test_not_arg))
        # :TEST-NOT wurde nicht angegeben
        { if (nullp(test_arg))
            *(stackptr STACKop 1) = L(eql); # #'EQL als Default für :TEST
          return(&up2_test);
        }
        # :TEST-NOT wurde angegeben
        { if (nullp(test_arg))
            return(&up2_test_not);
          else
            fehler_both_tests();
    }}  }

# UP: Testet, ob zwei Bäume gleich sind.
# tree_equal(stackptr,up2_fun,arg1,arg2)
# > arg1,arg2: Bäume
# > stackptr: Pointer in den Stack
# > A5: Adresse einer Testfunktion, die arg1 und arg2 vergleicht und dabei auf
#       die :TEST/:TEST-NOT-Argumente in *(stackptr+1).L bzw.
#       *(stackprt+0).L zugreifen kann.
# < ergebnis: TRUE, falls gleich, FALSE sonst
# can trigger GC
  local boolean tree_equal (const object* stackptr, up2_function up2_fun, object arg1, object arg2);
  local boolean tree_equal(stackptr,up2_fun,arg1,arg2)
    var const object* stackptr;
    var up2_function up2_fun;
    var object arg1;
    var object arg2;
    { start:
      if (atomp(arg1))
        if (atomp(arg2))
          # arg1 und arg2 sind beide Atome
          return(up2_fun(stackptr,arg1,arg2));
          else
          return FALSE;
        else
        if (atomp(arg2))
          return FALSE;
          else
          # arg1 und arg2 sind beides Conses
          { check_STACK(); check_SP();
            pushSTACK(Cdr(arg1)); pushSTACK(Cdr(arg2));
            if (tree_equal(stackptr,up2_fun,Car(arg1),Car(arg2))) # rekursiv die CARs vergleichen
              # falls gleich, tail-end-rekursiv die CDRs vergleichen
              { arg2=popSTACK(); arg1=popSTACK(); goto start; }
              else
              { skipSTACK(2); return FALSE; }
    }     }

LISPFUN(tree_equal,2,0,norest,key,2, (kw(test),kw(test_not)) )
  # (TREE-EQUAL x y :test :test-not), CLTL S. 264
  { var object* stackptr = &STACK_0;
    var up2_function up2_fun = test_test2_args(stackptr); # :TEST/:TEST-NOT-Argumente überprüfen
    value1 = tree_equal(stackptr,up2_fun,STACK_3,STACK_2) ? T : NIL;
    mv_count=1;
    skipSTACK(4);
  }

# UP: Testet auf Listenende
# endp(obj)
# > obj: Objekt
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: TRUE falls obj ein Listenende NIL ist,
#             FALSE falls obj ein Cons ist.
  local boolean endp (object obj);
  local boolean endp(obj)
    var object obj;
    { if (consp(obj)) return FALSE;
      elif (nullp(obj)) return TRUE;
      else { fehler_proper_list(obj); }
    }

LISPFUNN(endp,1) # (ENDP object), CLTL S. 264
  { value1 = endp(popSTACK()) ? T : NIL;
    mv_count=1;
  }

LISPFUNN(list_length,1) # (LIST-LENGTH list), CLTL S. 265
  # (defun list-length (list)  ; vgl. CLTL, S. 265
  #   (do ((n 0 (+ n 2))
  #        (fast list (cddr fast))
  #        (slow list (cdr slow))
  #       )
  #       (nil)
  #     (when (endp fast) (return n))
  #     (when (endp (cdr fast)) (return (1+ n)))
  #     (when (eq (cdr fast) slow) (return nil))
  # ) )
  { var object fast = popSTACK();
    var object slow = fast;
    var uintL n = 0;
    loop
      { if (endp(fast)) break;
        fast = Cdr(fast); n++;
        if (endp(fast)) break;
        if (eq(fast,slow)) # (eq (cdr fast) slow)
          { value1=NIL; mv_count=1; return; }
        fast = Cdr(fast); n++;
        slow = Cdr(slow);
      }
    value1 = fixnum(n); # n als Fixnum
    mv_count=1;
  }

# Fehlermeldung für NTH und NTHCDR
# fehler_nth()
# > STACK_0: fehlerhafter Index
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_nth, (void));
  local void fehler_nth()
    { pushSTACK(STACK_0); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_posfixnum)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(STACK_(0+2));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a nonnegative fixnum and therefore not a valid index")
            );
    }

LISPFUNN(nth,2) # (NTH integer list), CLTL S. 265
  { var object list = popSTACK();
    if (posfixnump(STACK_0)) # integer muss ein Fixnum >=0 sein
      { var uintL count = posfixnum_to_L(popSTACK()); # Wert des Fixnum
        # count mal den CDR von list nehmen:
        dotimesL(count,count, { list = cdr(list); } );
        # 1 mal den CAR nehmen:
        value1 = car(list); mv_count=1;
      }
      else
      fehler_nth();
  }

LISPFUNN(first,1) # (FIRST list), CLTL S. 266
  { value1 = car(popSTACK()); mv_count=1; }

LISPFUNN(second,1) # (SECOND list), CLTL S. 266
  { value1 = car(cdr(popSTACK())); mv_count=1; }

LISPFUNN(third,1) # (THIRD list), CLTL S. 266
  { value1 = car(cdr(cdr(popSTACK()))); mv_count=1; }

LISPFUNN(fourth,1) # (FOURTH list), CLTL S. 266
  { value1 = car(cdr(cdr(cdr(popSTACK())))); mv_count=1; }

LISPFUNN(fifth,1) # (FIFTH list), CLTL S. 266
  { value1 = car(cdr(cdr(cdr(cdr(popSTACK()))))); mv_count=1; }

LISPFUNN(sixth,1) # (SIXTH list), CLTL S. 266
  { value1 = car(cdr(cdr(cdr(cdr(cdr(popSTACK())))))); mv_count=1; }

LISPFUNN(seventh,1) # (SEVENTH list), CLTL S. 266
  { value1 = car(cdr(cdr(cdr(cdr(cdr(cdr(popSTACK()))))))); mv_count=1; }

LISPFUNN(eighth,1) # (EIGHTH list), CLTL S. 266
  { value1 = car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(popSTACK())))))))); mv_count=1; }

LISPFUNN(ninth,1) # (NINTH list), CLTL S. 266
  { value1 = car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(popSTACK()))))))))); mv_count=1; }

LISPFUNN(tenth,1) # (TENTH list), CLTL S. 266
  { value1 = car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(popSTACK())))))))))); mv_count=1; }

LISPFUNN(rest,1) # (REST list), CLTL S. 266
  { value1 = cdr(popSTACK()); mv_count=1; }

LISPFUNN(nthcdr,2) # (NTHCDR integer list), CLTL S. 267
  { var object list = popSTACK();
    if (posfixnump(STACK_0)) # integer muss ein Fixnum >=0 sein
      { var uintL count = posfixnum_to_L(popSTACK()); # Wert des Fixnum
        # count mal den CDR von list nehmen:
        dotimesL(count,count, { list = cdr(list); } );
        value1 = list; mv_count=1;
      }
      else
      fehler_nth();
  }

# Fehlermeldung für LAST, BUTLAST und NBUTLAST
# fehler_butlast(badindex)
# > badindex: fehlerhaftes 2. Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_butlast, (object badindex));
  local void fehler_butlast(badindex)
    var object badindex;
    { pushSTACK(badindex); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_posinteger)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(badindex); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a nonnegative integer and therefore not a valid argument")
            );
    }

LISPFUN(last,1,1,norest,nokey,0,NIL) # (LAST list [n]), CLtL2 S. 416-417, dpANS p. 14-34
  # (defun last (list &optional (n 1))
  #   (check-type n (integer 0 *))
  #   (do ((l list (cdr l))
  #        (r list)
  #        (i 0 (+ i 1)))
  #       ((atom l) r)
  #     (when (>= i n) (pop r))
  # ) )
  { var object intarg = popSTACK();
    # optionales Integer-Argument überprüfen:
    var uintL count; # Anzahl der zu kopierenden Elemente
    if (eq(intarg,unbound))
      { count = 1; }
      else
      { if (!(integerp(intarg) && positivep(intarg))) fehler_butlast(intarg);
        count = (posfixnump(intarg) ? posfixnum_to_L(intarg) : ~(uintL)0);
      }
   {var object list = popSTACK();
    # Optimierung der beiden häufigsten Fälle count=1 und count=0:
    switch (count)
      { case 0:
          while (consp(list)) { list = Cdr(list); }
          break;
        case 1:
          { var object list2;
            if (consp(list))
              { loop
                  { # Hier ist list ein Cons.
                    list2 = Cdr(list);
                    if (atomp(list2)) break;
                    list = list2;
              }   }
          }
          break;
        default:
          { var object list2 = list;
            dotimespL(count,count,
              { if (atomp(list2)) goto done;
                list2 = Cdr(list2);
              });
            while (consp(list2)) { list2 = Cdr(list2); list = Cdr(list); }
            done: ;
          }
          break;
      }
    value1 = list; mv_count=1;
  }}

# UP: Bildet eine Liste mit gegebenen Elementen.
# listof(len)
# > uintC len: gewünschte Listenlänge
# > auf STACK: len Objekte, erstes zuoberst
# < ergebnis: Liste dieser Objekte
# Erhöht STACK
# verändert STACK, kann GC auslösen
  global object listof (uintC len);
  global object listof(len)
    var uintC len;
    { pushSTACK(NIL); # bisherige Gesamtliste
      # die len Argumente vor diese Liste consen:
      dotimesC(len,len,
        { var object new_cons = allocate_cons();
          Cdr(new_cons) = popSTACK();
          Car(new_cons) = STACK_0;
          STACK_0 = new_cons;
        });
      return popSTACK();
    }

LISPFUN(list,0,0,rest,nokey,0,NIL)
  # (LIST {object}), CLTL S. 267
  { pushSTACK(NIL); # bisherige Gesamtliste
    # die argcount Argumente vor diese Liste consen:
    dotimesC(argcount,argcount,
      { var object new_cons = allocate_cons();
        Cdr(new_cons) = popSTACK(); # nächstes Argument davor
        Car(new_cons) = STACK_0;
        STACK_0 = new_cons;
      });
    value1 = popSTACK(); mv_count=1;
  }

LISPFUN(liststern,1,0,rest,nokey,0,NIL)
  # (LIST* obj1 {object}), CLTL S. 267
  { # bisherige Gesamtliste bereits im Stack
    # die argcount restlichen Argumente vor diese Liste consen:
    dotimesC(argcount,argcount,
      { var object new_cons = allocate_cons();
        Cdr(new_cons) = popSTACK(); # nächstes Argument davor
        Car(new_cons) = STACK_0;
        STACK_0 = new_cons;
      });
    value1 = popSTACK(); mv_count=1;
  }

LISPFUN(make_list,1,0,norest,key,1, (kw(initial_element)) )
  # (MAKE-LIST size :initial-element), CLTL S. 268
  { # :initial-element überprüfen:
    if (eq(STACK_0,unbound))
      STACK_0 = NIL; # Default-Initialisierung für initial-element
    # :size überprüfen:
    if (posfixnump(STACK_1))
      { value1 = make_list(posfixnum_to_L(STACK_1)); mv_count=1;
        skipSTACK(2);
      }
      else
      { # size in STACK_1
        pushSTACK(STACK_1); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_posfixnum)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(STACK_(1+2)); # size
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: ~ is not a nonnegative fixnum and therefore not a valid list length")
              );
  }   }

LISPFUN(append,0,0,rest,nokey,0,NIL) # (APPEND {list}), CLTL S. 268
  { if (argcount==0)
      { value1=NIL; mv_count=1; } # keine Argumente -> NIL als Ergebnis
      else
      # Argumente aneinanderhängen. Dazu Schleife argcount-1 mal durchlaufen:
      { dotimesC(argcount,argcount-1,
          # STACK_0 = bisherige Gesamtliste von rechts.
          # STACK_1 := (append STACK_1 STACK_0), STACK um 1 erhöhen:
          { var object list1;
           {var object list2 = popSTACK(); # bisherige Gesamtliste (von rechts)
            list1 = STACK_0; # vorne anzuhängendes Argument
            STACK_0 = list2; # bisherige Gesamtliste wieder stacken
           }
            # list1 muss Liste sein:
            if (atomp(list1))
              if (nullp(list1))
                ; # falls list1=NIL: (append nil x) = x, nichts tun
                else
                fehler_list(list1);
              else
              # (append list1 STACK_0), wobei list1 ein Cons ist:
              # Kopiere list1 und halte das letzte Cons fest:
              { var object lauf;
                pushSTACK(list1);
                { var object new_list = allocate_cons();
                  lauf = STACK_0; # lauf läuft durch die alte Liste list1 durch
                  Car(new_list) = Car(lauf);
                  STACK_0 = new_list;
                  pushSTACK(new_list);
                }
                # Schleife: STACK_1 ist die Gesamtkopie, STACK_0 = LAST davon,
                # lauf = das entsprechende Cons der Original-Liste list1.
                while ( lauf=Cdr(lauf), consp(lauf) )
                  { # es kommt noch ein Cons
                    pushSTACK(lauf); # lauf retten
                   {var object new_cons = allocate_cons(); # neues Cons allozieren
                    lauf = popSTACK(); # lauf zurück
                    Cdr(STACK_0) = new_cons; # und als CDR des LAST einhängen
                    Car(new_cons) = Car(lauf); # CAR kopieren
                    STACK_0 = new_cons; # das ist nun das neue LAST
                  }}
                # Kopie fertig. STACK_2 = bisherige Gesamtliste,
                # STACK_1 = Kopie von list1, STACK_0 = LAST davon.
                lauf = popSTACK(); # Ende der Kopie
                list1 = popSTACK(); # ganze Kopie
                Cdr(lauf) = STACK_0; # bisherige Gesamtkopie einhängen
                STACK_0 = list1; # und die Kopie ist die neue Gesamtliste
              }
          });
        value1 = popSTACK(); mv_count=1; # Gesamtliste als Wert
  }   }

LISPFUNN(copy_list,1) # (COPY-LIST list), CLTL S. 268
  { var object list = popSTACK();
    if (listp(list))
      { value1 = copy_list(list); mv_count=1; }
      else
      { fehler_list(list); }
  }

# UP: Kopiert eine Aliste
# copy_alist(alist)
# > alist: Aliste
# < ergebnis: Kopie der Aliste
# can trigger GC
  local object copy_alist (object alist);
# Methode:
# Statt
#   (mapcar #'(lambda (x) (if (consp x) (cons (car x) (cdr x)) x)) l)
# wird die Liste erst kopiert mit copy-list, dann die Top-Level-Elemente
# der Kopie, die Conses sind, durch neue Conses mit selbem CAR und CDR
# ersetzt.
  local object copy_alist(alist)
    var object alist;
    { alist = copy_list(alist);
      pushSTACK(alist); # Gesamtliste retten
      # alist läuft durch die Gesamtliste
      until (atomp(alist))
        { if (mconsp(Car(alist)))
            { pushSTACK(alist); # alist retten
             {var object new_cons = allocate_cons(); # neues Cons
              alist = popSTACK(); # alist zurück
              {var object old_cons = Car(alist);
               Car(new_cons) = Car(old_cons); Cdr(new_cons) = Cdr(old_cons);
              }
              Car(alist) = new_cons;
            }}
          alist = Cdr(alist);
        }
      return popSTACK();
    }

LISPFUNN(copy_alist,1) # (COPY-ALIST alist), CLTL S. 268
  { value1 = copy_alist(popSTACK()); mv_count=1; }

# UP: Kopiert einen Baum.
  local object copy_tree (object tree);
  local object copy_tree(tree)
    var object tree;
    { if (atomp(tree))
        return tree; # Atome unverändert zurückgeben
        else
        { check_STACK(); check_SP();
          pushSTACK(Cdr(tree)); # CDR retten
         {var object temp = copy_tree(Car(tree)); # den CAR rekursiv kopieren
          tree = STACK_0;
          STACK_0 = temp; # CAR-Kopie retten
          temp = copy_tree(tree); # den CDR rekursiv kopieren
          pushSTACK(temp); # CDR-Kopie retten
         }
         {var object new_cons = allocate_cons(); # neues Cons
          Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK(); # füllen
          return new_cons;
    }   }}

LISPFUNN(copy_tree,1) # (COPY-TREE tree), CLTL S. 269
  { value1 = copy_tree(popSTACK()); mv_count=1; }

LISPFUNN(revappend,2) # (REVAPPEND list object), CLTL S. 269
  { until (matomp(STACK_1))
      { var object new_cons = allocate_cons(); # neues Cons
        Car(new_cons) = Car(STACK_1); Cdr(new_cons) = STACK_0; # (cons (car list) object)
        STACK_0 = new_cons; # das ist das neue, verlängerte object
        STACK_1 = Cdr(STACK_1); # list verkürzen
      }
    value1 = popSTACK(); mv_count=1;
    skipSTACK(1);
  }

LISPFUN(nconc,0,0,rest,nokey,0,NIL) # (NCONC {list}), CLTL S. 269
  { if (argcount==0)
      { value1=NIL; mv_count=1; } # keine Argumente -> NIL als Ergebnis
      else
      # Argumente aneinanderhängen. Dazu Schleife argcount-1 mal durchlaufen:
      { dotimesC(argcount,argcount-1,
          # STACK_0 = bisherige Gesamtliste von rechts.
          # STACK_1 := (nconc STACK_1 STACK_0), STACK um 1 erhöhen:
          { if (matomp(STACK_1))
              if (nullp(STACK_1))
                { STACK_1 = STACK_0; skipSTACK(1); } # Gesamtliste bleibt, Argument vergessen
                else
                fehler_list(STACK_1);
              else
                { # Gesamtliste in (cdr (last STACK_1)) einhängen:
                  var object list1 = STACK_1;
                  var object list2;
                  loop
                    { # Hier ist list1 ein Cons.
                      list2 = Cdr(list1);
                      if (atomp(list2)) break;
                      list1 = list2;
                    }
                  # list1 ist das letzte Cons des Arguments STACK_1
                  Cdr(list1) = popSTACK(); # bisherige Gesamtliste einhängen
                  # STACK_0 = neue Gesamtliste
                }
          });
        value1 = popSTACK(); mv_count=1;
  }   }

LISPFUNN(nreconc,2) # (NRECONC list1 list2), CLTL S. 269
  { var object list2 = popSTACK();
    var object list1 = popSTACK();
    if (listp(list1))
      { value1 = nreconc(list1,list2); mv_count=1; }
      else
      fehler_list(list1);
  }

LISPFUNN(list_nreverse,1) # (SYS::LIST-NREVERSE list)
# wie (NREVERSE list), wenn list eine Liste ist.
  { value1 = nreverse(popSTACK()); mv_count=1; }

LISPFUN(butlast,1,1,norest,nokey,0,NIL)
  # (BUTLAST list [integer]), CLTL S. 271
  { var object intarg = popSTACK();
    # optionales Integer-Argument überprüfen:
    var uintL count; # Anzahl der zu entfernenden Elemente
    if (eq(intarg,unbound))
      { count = 1; }
      else
      { if (!(integerp(intarg) && positivep(intarg))) fehler_butlast(intarg);
        count = (posfixnump(intarg) ? posfixnum_to_L(intarg) : ~(uintL)0);
      }
   {var uintL len = llength(STACK_0); # Anzahl der Elemente der Liste
    # Give an error if the argument is not a list. (It's stupid to allow
    # dotted lists of length > 0 but to forbid dotted lists of length 0,
    # but that's how ANSI CL specifies it.)
    if (len==0 && !nullp(STACK_0)) { fehler_list(STACK_0); }
    if (len<=count)
      { value1=NIL; mv_count=1; skipSTACK(1); } # Länge(list)<=count -> NIL als Wert
      else
      { var uintL new_len = len - count; # >0
        # Liefere eine Kopie der ersten new_len Conses der Liste STACK_0:
        var object new_list = make_list(new_len); # neue Liste allozieren
        # Listenelemente einzeln kopieren, bis new_list voll ist:
        var object new_lauf = new_list; # läuft durch die neue Liste
        var object old_lauf = popSTACK(); # läuft durch die alte Liste
        do { Car(new_lauf) = Car(old_lauf);
             old_lauf = Cdr(old_lauf); new_lauf = Cdr(new_lauf);
           }
           until (atomp(new_lauf));
        value1 = new_list; mv_count=1;
  }}  }

LISPFUN(nbutlast,1,1,norest,nokey,0,NIL)
  # (NBUTLAST list [integer]), CLTL S. 271
  { var object intarg = popSTACK();
    # optionales Integer-Argument überprüfen:
    var uintL count; # Anzahl der zu entfernenden Elemente
    if (eq(intarg,unbound))
      { count = 1; }
      else
      { if (!(integerp(intarg) && positivep(intarg))) fehler_butlast(intarg);
        count = (posfixnump(intarg) ? posfixnum_to_L(intarg) : ~(uintL)0);
      }
   {var uintL len = llength(STACK_0); # Anzahl der Elemente der Liste
    # Give an error if the argument is not a list. (It's stupid to allow
    # dotted lists of length > 0 but to forbid dotted lists of length 0,
    # but that's how ANSI CL specifies it.)
    if (len==0 && !nullp(STACK_0)) { fehler_list(STACK_0); }
    if (len<=count)
      { value1=NIL; mv_count=1; skipSTACK(1); } # Länge(list)<=count -> NIL als Wert
      else
      { var uintL new_len = len - count; # >0
        var object lauf = STACK_0; # läuft durch die Liste
        # new_len-1 mal den CDR nehmen und dann den CDR auf NIL setzen:
        dotimesL(new_len,new_len-1, { lauf = Cdr(lauf); } );
        Cdr(lauf) = NIL;
        value1 = popSTACK(); mv_count=1; # Liste als Wert
  }}  }

LISPFUNN(ldiff,2) # (LDIFF list sublist), CLTL S. 272
  { var object sublist = popSTACK();
    # Suche, wo sublist in list beginnt:
    var uintL new_len = 0;
   {var object listr = STACK_0;
    #ifndef X3J13_175
    until (endp(listr) || eq(listr,sublist)) { listr = Cdr(listr); new_len++; }
    #else
    if (!listp(listr)) { fehler_list(listr); }
    until (atomp(listr) || eq(listr,sublist)) { listr = Cdr(listr); new_len++; }
    #endif
   }
    # Liefere eine Kopie der ersten new_len Conses der Liste STACK_0:
   {var object new_list = make_list(new_len); # neue Liste allozieren
    # Listenelemente einzeln kopieren, bis new_list voll ist:
    var object new_lauf = new_list; # läuft durch die neue Liste
    var object old_lauf = popSTACK(); # läuft durch die alte Liste
    until (atomp(new_lauf))
      { Car(new_lauf) = Car(old_lauf);
        old_lauf = Cdr(old_lauf); new_lauf = Cdr(new_lauf);
      }
    value1 = new_list; mv_count=1;
  }}

# Fehlermeldung für RPLACA und RPLACD u.ä.
# fehler_cons(badobject)
# > badobject: Nicht-Cons
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_cons, (object badobject));
  local void fehler_cons(badobject)
    object badobject;
    { pushSTACK(badobject); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(cons)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(badobject); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not a pair")
            );
    }

LISPFUNN(rplaca,2) # (RPLACA cons object), CLTL S. 272
  { if (matomp(STACK_1))
      fehler_cons(STACK_1);
      else
      { var object arg2 = popSTACK();
        var object arg1 = popSTACK();
        Car(arg1) = arg2;
        value1 = arg1; mv_count=1;
  }   }

LISPFUNN(prplaca,2) # (SYS::%RPLACA cons object)
  # Wie (RPLACA cons object), hier jedoch object als Wert
  { if (matomp(STACK_1))
      fehler_cons(STACK_1);
      else
      { var object arg2 = popSTACK();
        var object arg1 = popSTACK();
        Car(arg1) = arg2;
        value1 = arg2; mv_count=1;
  }   }

LISPFUNN(rplacd,2) # (RPLACD cons object), CLTL S. 272
  { if (matomp(STACK_1))
      fehler_cons(STACK_1);
      else
      { var object arg2 = popSTACK();
        var object arg1 = popSTACK();
        Cdr(arg1) = arg2;
        value1 = arg1; mv_count=1;
  }   }

LISPFUNN(prplacd,2) # (SYS::%RPLACD cons object)
  # Wie (RPLACD cons object), hier jedoch object als Wert
  { if (matomp(STACK_1))
      fehler_cons(STACK_1);
      else
      { var object arg2 = popSTACK();
        var object arg1 = popSTACK();
        Cdr(arg1) = arg2;
        value1 = arg2; mv_count=1;
  }   }

# Unterprogramm zum Ausführen des Tests :TEST
# up_test(stackptr,x)
# > *(stackptr+1): die Testfunktion
# > *(stackptr+3): das zu vergleichende Item
# > x: Argument
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# can trigger GC
  local boolean up_test (const object* stackptr, object x);
  local boolean up_test(stackptr,x)
    var const object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (funcall testfun item x) ausführen:
      pushSTACK(*(stackptr STACKop 3)); # item
      pushSTACK(x); # x
      funcall(*(stackptr STACKop 1),2);
      if (nullp(value1)) return FALSE; else return TRUE;
    }

# Unterprogramm zum Ausführen des Tests :TEST-NOT
# up_test_not(stackptr,x)
# > *(stackptr+0): die Testfunktion
# > *(stackptr+3): das zu vergleichende Item
# > x: Argument
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# can trigger GC
  local boolean up_test_not (const object* stackptr, object x);
  local boolean up_test_not(stackptr,x)
    var const object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (not (funcall testfun item x)) ausführen:
      pushSTACK(*(stackptr STACKop 3)); # item
      pushSTACK(x); # x
      funcall(*(stackptr STACKop 0),2);
      if (nullp(value1)) return TRUE; else return FALSE;
    }

# Unterprogramm zum Ausführen des Tests -IF
# up_if(stackptr,x)
# > *(stackptr+1): das Testprädikat
# > x: Argument
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# can trigger GC
  local boolean up_if (const object* stackptr, object x);
  local boolean up_if(stackptr,x)
    var const object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (funcall predicate x) ausführen:
      pushSTACK(x); funcall(*(stackptr STACKop 1),1);
      if (nullp(value1)) return FALSE; else return TRUE;
    }

# Unterprogramm zum Ausführen des Tests -IF-NOT
# up_if_not(stackptr,x)
# > *(stackptr+1): das Testprädikat
# > x: Argument
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# can trigger GC
  local boolean up_if_not (const object* stackptr, object x);
  local boolean up_if_not(stackptr,x)
    var const object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (not (funcall predicate x)) ausführen:
      pushSTACK(x); funcall(*(stackptr STACKop 1),1);
      if (nullp(value1)) return TRUE; else return FALSE;
    }

# UP: Überprüft das :KEY-Argument
# test_key_arg()
# > STACK_0: optionales Argument
# < STACK_0: korrekte KEY-Funktion
  local void test_key_arg (void);
  local void test_key_arg()
    { var object key_arg = STACK_0;
      if (eq(key_arg,unbound) || nullp(key_arg))
        STACK_0 = L(identity); # #'IDENTITY als Default für :KEY
    }

# UP: Überprüft die :TEST, :TEST-NOT - Argumente
# test_test_args()
# > stackptr:=&STACK_1 : Pointer in den STACK
# > STACK_2: :TEST-Argument
# > STACK_1: :TEST-NOT-Argument
# > subr_self: Aufrufer (ein SUBR)
# < STACK_2: verarbeitetes :TEST-Argument
# < STACK_1: verarbeitetes :TEST-NOT-Argument
# < up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#       > stackptr: derselbe Pointer in den Stack, *(stackptr+3) = item,
#         *(stackptr+1) = :test-Argument, *(stackptr+0) = :test-not-Argument,
#       > x: Argument
#       < TRUE, falls der Test erfüllt ist, FALSE sonst.
  # up_function sei der Typ der Adresse einer solchen Testfunktion:
  typedef boolean (*up_function) (const object* stackptr, object x);
  local up_function test_test_args (void);
  local up_function test_test_args()
    { var object test_arg = STACK_2;
      if (eq(test_arg,unbound)) { test_arg=NIL; }
      # test_arg ist das :TEST-Argument
     {var object test_not_arg = STACK_1;
      if (eq(test_not_arg,unbound)) { test_not_arg=NIL; }
      # test_not_arg ist das :TEST-NOT-Argument
      if (nullp(test_not_arg))
        # :TEST-NOT wurde nicht angegeben
        { if (nullp(test_arg))
            STACK_2 = L(eql); # #'EQL als Default für :TEST
          return(&up_test);
        }
        # :TEST-NOT wurde angegeben
        { if (nullp(test_arg))
            return(&up_test_not);
          else
            fehler_both_tests();
    }}  }

# UP: Ersetzt im Baum tree alle x, deren KEY der TESTFUNktion genügen,
# durch NEW. Konstruktiv.
# subst(tree,stackptr,up_fun)
# > tree: Baum
# > stackptr: *(stackptr-2) = NEW, *(stackptr-1) = KEY
# > up_fun: TESTFUN = Adresse der Testfunktion,
#       wird selbem stackptr und mit (KEY x) als Argument angesprungen.
#       Sie liefert TRUE, falls der Test erfüllt ist, FALSE sonst.
# < ergebnis: (evtl. neuer) Baum
# can trigger GC
  local object subst (object tree, object* stackptr, up_function up_fun);
  local object subst(tree,stackptr,up_fun)
    var object tree;
    var object* stackptr;
    var up_function up_fun;
    { # erst (KEY tree) berechnen und TESTFUN aufrufen:
      pushSTACK(tree); # tree retten
      pushSTACK(tree); funcall(*(stackptr STACKop -1),1); # (KEY tree)
      if (up_fun(stackptr,value1)) # TESTFUN aufrufen
        # Test erfüllt
        { skipSTACK(1); return *(stackptr STACKop -2); } # NEW als Wert
        else
        # Test nicht erfüllt
        if (matomp(STACK_0))
          # Argument Atom -> unverändert lassen
          { return popSTACK(); }
          else
          # Argument ist ein Cons -> SUBST rekursiv aufrufen:
          {  check_STACK(); check_SP();
           { # rekursiv für den CDR aufrufen:
             var object new_cdr = subst(Cdr(STACK_0),stackptr,up_fun);
             pushSTACK(new_cdr); # CDR-Ergebnis retten
            {# rekursiv für den CAR aufrufen:
             var object new_car = subst(Car(STACK_1),stackptr,up_fun);
             if (eq(new_car,Car(STACK_1)) && eq(STACK_0,Cdr(STACK_1)))
               # beides unverändert
               { skipSTACK(1); # CDR-Ergebnis vergessen
                 return popSTACK();
               }
               else
               { STACK_1 = new_car; # CAR-Ergebnis retten
                {var object new_cons = allocate_cons(); # neue Cons-Zelle
                 Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK();
                 return new_cons;
    }     }}}  }}

LISPFUN(subst,3,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
  # (SUBST new old tree :test :test-not :key), CLTL S. 273
  { test_key_arg(); # :KEY-Argument in STACK_0
   {var up_function up_fun = test_test_args(); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    {var object newobj = STACK_5; pushSTACK(newobj); }
    # Stackaufbau: new, old, tree, test, test_not, key, new.
    value1 = subst(STACK_4,&STACK_2,up_fun); # Ersetzung durchführen
    mv_count=1;
    skipSTACK(7);
  }}

LISPFUN(subst_if,3,0,norest,key,1, (kw(key)) )
  # (SUBST-IF new pred tree :key), CLTL S. 273
  { test_key_arg(); # :KEY-Argument in STACK_0
    {var object newobj = STACK_3; pushSTACK(newobj); }
    # Stackaufbau: new, pred, tree, key, new.
    value1 = subst(STACK_2,&STACK_2,&up_if); # Ersetzung durchführen
    mv_count=1;
    skipSTACK(5);
  }

LISPFUN(subst_if_not,3,0,norest,key,1, (kw(key)) )
  # (SUBST-IF-NOT new pred tree :key), CLTL S. 273
  { test_key_arg(); # :KEY-Argument in STACK_0
    {var object newobj = STACK_3; pushSTACK(newobj); }
    # Stackaufbau: new, pred, tree, key, new.
    value1 = subst(STACK_2,&STACK_2,&up_if_not); # Ersetzung durchführen
    mv_count=1;
    skipSTACK(5);
  }

# UP: Ersetzt im Baum tree alle x, deren KEY der TESTFUNktion genügen,
# durch NEW. Destruktiv.
# nsubst(tree,stackptr,up_fun)
# > tree: Baum
# > stackptr: *(stackptr-2) = NEW, *(stackptr-1) = KEY
# > up_fun: TESTFUN = Adresse der Testfunktion,
#       wird selbem stackptr und mit (KEY x) als Argument angesprungen.
#       Sie liefert TRUE, falls der Test erfüllt ist, FALSE sonst.
# < ergebnis: Baum
# can trigger GC
  local object nsubst (object tree, object* stackptr, up_function up_fun);
  local object nsubst(tree,stackptr,up_fun)
    var object tree;
    var object* stackptr;
    var up_function up_fun;
    { # erst (KEY tree) berechnen und TESTFUN aufrufen:
      pushSTACK(tree); # tree retten
      pushSTACK(tree); funcall(*(stackptr STACKop -1),1); # (KEY tree)
      if (up_fun(stackptr,value1)) # TESTFUN aufrufen
        # Test erfüllt
        { skipSTACK(1); return *(stackptr STACKop -2); } # NEW als Wert
        else
        # Test nicht erfüllt
        { if (mconsp(STACK_0))
            # Argument ist ein Cons -> NSUBST rekursiv aufrufen:
            { check_STACK(); check_SP();
              # rekursiv für den CDR aufrufen:
              {var object modified_cdr = nsubst(Cdr(STACK_0),stackptr,up_fun);
               Cdr(STACK_0) = modified_cdr;
              }
              # rekursiv für den CAR aufrufen:
              {var object modified_car = nsubst(Car(STACK_0),stackptr,up_fun);
               Car(STACK_0) = modified_car;
            } }
          return popSTACK(); # ursprünglicher Baum zurück
    }   }

LISPFUN(nsubst,3,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
  # (NSUBST new old tree :test :test-not :key), CLTL S. 274
  { test_key_arg(); # :KEY-Argument in STACK_0
   {var up_function up_fun = test_test_args(); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    {var object newobj = STACK_5; pushSTACK(newobj); }
    # Stackaufbau: new, old, tree, test, test_not, key, new.
    value1 = nsubst(STACK_4,&STACK_2,up_fun); # Ersetzung durchführen
    mv_count=1;
    skipSTACK(7);
  }}

LISPFUN(nsubst_if,3,0,norest,key,1, (kw(key)) )
  # (NSUBST-IF new pred tree :key), CLTL S. 274
  { test_key_arg(); # :KEY-Argument in STACK_0
    {var object newobj = STACK_3; pushSTACK(newobj); }
    # Stackaufbau: new, pred, tree, key, new.
    value1 = nsubst(STACK_2,&STACK_2,&up_if); # Ersetzung durchführen
    mv_count=1;
    skipSTACK(5);
  }

LISPFUN(nsubst_if_not,3,0,norest,key,1, (kw(key)) )
  # (NSUBST-IF-NOT new pred tree :key), CLTL S. 274
  { test_key_arg(); # :KEY-Argument in STACK_0
    {var object newobj = STACK_3; pushSTACK(newobj); }
    # Stackaufbau: new, pred, tree, key, new.
    value1 = nsubst(STACK_2,&STACK_2,&up_if_not); # Ersetzung durchführen
    mv_count=1;
    skipSTACK(5);
  }

# UP: Liefert das erste Listenelement, dessen CAR der TESTFUNktion genügt.
# sublis_assoc(stackptr)
# > *(stackptr+3) = alist: Aliste
# > stackptr: *(stackptr-1) = KEY
# > *(stackptr-3) = TESTFUN = Testfunktion, wird für alle Listenelemente
#       (u . v) mit selbem stackptr und mit (KEY x) und u als Argumenten angesprungen.
#       Sie liefert TRUE, falls der Test erfüllt ist, FALSE sonst.
# < ergebnis: Listenelement (ein Cons) oder NIL
# can trigger GC
  local object sublis_assoc (object* stackptr);
  local object sublis_assoc(stackptr)
    var object* stackptr;
    { var object alist = *(stackptr STACKop 3);
      while (consp(alist))
        { # How to treat atoms in the list?
          # a. One can ignore them.
          # b. One can signal an error on them.
          # c. One can signal an error only for non-NIL atoms.
          # Obviously (b) is best, because it provides the best possible
          # error checking. But CLtL2 and CLHS both contain a "note" that
          # suggests to some people that atoms are ignored, therefore I
          # assume that there is code outside which assumes this behaviour,
          # and we must not signal an error on it.
          # Note: To other people this note suggests that only NILs are
          # ignored, and they suggest (c). This is inconsistent with the
          # definition of "association list" in the CLHS glossary and with
          # the general use of alists as lookup tables.
          # Therefore we implement (a).
          if (mconsp(Car(alist))) # atomare Listenelemente überspringen
            { pushSTACK(alist); # Listenrest ((u . v) ...) retten
              # Testen, ob die zweiargumentige Testfunktion
              # *(stackptr-3) (eine Adresse!), angewandt auf u und das
              # vorher in *(stackptr-2) abgelegte Argument, erfüllt ist:
             {var boolean erg =
                (*(up2_function)TheMachineCode(*(stackptr STACKop -3))) # zweiargumentige Testfunktion, wurde abgelegt
                  ( stackptr, *(stackptr STACKop -2), Car(Car(alist)) ); # auf (KEY x) und u anwenden
              alist = popSTACK();
              if (erg)
                # Test erfüllt -> x = (u . v) = (CAR alist) als Ergebnis
                return Car(alist);
              # Test nicht erfüllt
            }}
          alist = Cdr(alist); # Tail-End-Rekursion
        }
      # Listenende erreicht -> ergibt Ergebnis NIL
      return NIL;
    }

# UP: Ersetzt im Baum tree alle x durch ihr ALIST-Abbild (mittels ASSOC):
# x wird durch das erste v ersetzt, so dass (u . v) in ALIST vorkommt und
# (KEY x) und u der TESTFUNktion genügen. Konstruktiv.
# sublis(tree,stackptr)
# > tree: Baum
# > stackptr: *(stackptr-1) = KEY, *(stackptr+3) = ALIST,
#             *(stackptr-2) ist frei für (KEY x)
# < ergebnis: (evtl. neuer) Baum
# can trigger GC
  local object sublis (object tree, object* stackptr);
  local object sublis(tree,stackptr)
    var object tree;
    var object* stackptr;
    { # erst (KEY tree) berechnen und ASSOC aufrufen:
      pushSTACK(tree); # tree retten
      pushSTACK(tree); funcall(*(stackptr STACKop -1),1); # (KEY tree)
      *(stackptr STACKop -2) = value1; # retten für sublis_assoc
     {var object assoc_erg = sublis_assoc(stackptr);
      if (consp(assoc_erg))
        # Test erfüllt
        { skipSTACK(1); return Cdr(assoc_erg); } # (CDR (ASSOC ...)) als Wert
        else
        # Test nicht erfüllt
        if (matomp(STACK_0))
          # Argument Atom -> unverändert lassen
          { return popSTACK(); }
          else
          # Argument ist ein Cons -> SUBLIS rekursiv aufrufen:
          {  check_STACK(); check_SP();
           { # rekursiv für den CDR aufrufen:
             var object new_cdr = sublis(Cdr(STACK_0),stackptr);
             pushSTACK(new_cdr); # CDR-Ergebnis retten
            {# rekursiv für den CAR aufrufen:
             var object new_car = sublis(Car(STACK_1),stackptr);
             if (eq(new_car,Car(STACK_1)) && eq(STACK_0,Cdr(STACK_1)))
               # beides unverändert
               { skipSTACK(1); # CDR-Ergebnis vergessen
                 return popSTACK();
               }
               else
               { STACK_1 = new_car; # CAR-Ergebnis retten
                {var object new_cons = allocate_cons(); # neue Cons-Zelle
                 Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK();
                 return new_cons;
    }}    }}}  }}

LISPFUN(sublis,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
  # (SUBLIS alist tree :test :test-not :key), CLTL S. 274
  { test_key_arg(); # :KEY-Argument in STACK_0
   {var object* stackptr = &STACK_1;
    var up2_function up2_fun = test_test2_args(stackptr); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    # up2_fun = Testfunktion, wird mit stackptr und (KEY x) und u als
    # Argumenten angesprungen. Sie liefert TRUE, falls der Test erfüllt ist.
    if (nullp(STACK_4)) # shortcut: nothing to do if alist = ()
      { value1 = STACK_3; mv_count=1;
        skipSTACK(5);
      }
      else
      { pushSTACK(NIL); # Dummy
        pushSTACK(make_machine_code(up2_fun)); # Testfunktion, wegen Typinfo=machine_type GC-sicher!
        # Stackaufbau: alist, tree, test, test_not, key, dummy, up2_fun.
        value1 = sublis(STACK_5,stackptr); # Ersetzung durchführen
        mv_count=1;
        skipSTACK(7);
  }}  }

# UP: Ersetzt im Baum tree alle x durch ihr ALIST-Abbild (mittels ASSOC):
# x wird durch das erste v ersetzt, so dass (u . v) in ALIST vorkommt und
# (KEY x) und u der TESTFUNktion genügen. Destruktiv.
# nsublis(tree,stackptr)
# > tree: Baum
# > stackptr: *(stackptr-1) = KEY, *(stackptr+3) = ALIST,
#             *(stackptr-2) ist frei für (KEY x)
# < ergebnis: Baum
# can trigger GC
  local object nsublis (object tree, object* stackptr);
  local object nsublis(tree,stackptr)
    var object tree;
    var object* stackptr;
    { # erst (KEY tree) berechnen und ASSOC aufrufen:
      pushSTACK(tree); # tree retten
      pushSTACK(tree); funcall(*(stackptr STACKop -1),1); # (KEY tree)
      *(stackptr STACKop -2) = value1; # retten für sublis_assoc
     {var object assoc_erg = sublis_assoc(stackptr);
      if (consp(assoc_erg))
        # Test erfüllt
        { skipSTACK(1); return Cdr(assoc_erg); } # (CDR (ASSOC ...)) als Wert
        else
        # Test nicht erfüllt
        { if (mconsp(STACK_0))
            # Argument ist ein Cons -> NSUBLIS rekursiv aufrufen:
            { check_STACK(); check_SP();
              # rekursiv für den CDR aufrufen:
              {var object modified_cdr = nsublis(Cdr(STACK_0),stackptr);
               Cdr(STACK_0) = modified_cdr;
              }
              # rekursiv für den CAR aufrufen:
              {var object modified_car = nsublis(Car(STACK_0),stackptr);
               Car(STACK_0) = modified_car;
            } }
          return popSTACK(); # ursprünglicher Baum zurück
    }}  }

LISPFUN(nsublis,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
  # (NSUBLIS alist tree :test :test-not :key), CLTL S. 275
  { test_key_arg(); # :KEY-Argument in STACK_0
   {var object* stackptr = &STACK_1;
    var up2_function up2_fun = test_test2_args(stackptr); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    # up2_fun = Testfunktion, wird mit stackptr und (KEY x) und u als
    # Argumenten angesprungen. Sie liefert TRUE, falls der Test erfüllt ist.
    if (nullp(STACK_4)) # shortcut: nothing to do if alist = ()
      { value1 = STACK_3; mv_count=1;
        skipSTACK(5);
      }
      else
      { pushSTACK(NIL); # Dummy
        pushSTACK(make_machine_code(up2_fun)); # Testfunktion, wegen Typinfo=machine_type GC-sicher!
        # Stackaufbau: alist, tree, test, test_not, key, dummy, up2_fun.
        value1 = nsublis(STACK_5,stackptr); # Ersetzung durchführen
        mv_count=1;
        skipSTACK(7);
  }}  }

# UP: Liefert den Listenrest ab dem Listenelement, das der TESTFUNktion
# genügt.
# member(list,stackptr,up_fun)
# > list: Liste
# > STACK_0: Aufrufer (ein SUBR)
# > stackptr: *(stackptr-1) = KEY
# > up_fun: TESTFUN = Adresse der Testfunktion,
#       wird selbem stackptr und mit (KEY x) als Argument angesprungen.
#       Sie liefert TRUE, falls der Test erfüllt ist, FALSE sonst.
# < ergebnis: Listenrest
# can trigger GC
  local object member (object list, object* stackptr, up_function up_fun);
  local object member(list,stackptr,up_fun)
    var object list;
    var object* stackptr;
    var up_function up_fun;
    { until ((
              subr_self = STACK_0, # Aufrufer (für Fehlermeldung bei ENDP)
              endp(list) # Listenende erreicht?
            ))
        { pushSTACK(list); # Listenrest retten
          pushSTACK(Car(list)); funcall(*(stackptr STACKop -1),1); # (KEY x)
         {var boolean erg = up_fun(stackptr,value1); # TESTFUN aufrufen
          list = popSTACK();
          if (erg) { return list; } # Test erfüllt -> list als Ergebnis
         }
          # Test nicht erfüllt -> (member ... (cdr list)) aufrufen:
          list = Cdr(list); # tail-end-rekursiv
        }
      return list; # NIL als Ergebnis
    }

LISPFUN(member,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
  # (MEMBER item list :test :test-not :key), CLTL S. 275
  { test_key_arg(); # :KEY-Argument in STACK_0
   {var up_function up_fun = test_test_args(); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    pushSTACK(L(member)); # Aufrufer
    value1 = member(STACK_4,&STACK_2,up_fun); # Suche durchführen
    mv_count=1;
    skipSTACK(6);
  }}

LISPFUN(member_if,2,0,norest,key,1, (kw(key)) )
  # (MEMBER-IF pred list :key), CLTL S. 275
  { test_key_arg(); # :KEY-Argument in STACK_0
    pushSTACK(L(member_if)); # Aufrufer
    value1 = member(STACK_2,&STACK_2,&up_if); # Suche durchführen
    mv_count=1;
    skipSTACK(4);
  }

LISPFUN(member_if_not,2,0,norest,key,1, (kw(key)) )
  # (MEMBER-IF-NOT pred list :key), CLTL S. 275
  { test_key_arg(); # :KEY-Argument in STACK_0
    pushSTACK(L(member_if_not)); # Aufrufer
    value1 = member(STACK_2,&STACK_2,&up_if_not); # Suche durchführen
    mv_count=1;
    skipSTACK(4);
  }

LISPFUNN(tailp,2) # (TAILP sublist list), CLTL S. 275
  #ifndef X3J13_175
  # (defun tailp (sublist list)
  #   (do ((l list (rest l)))
  #       ((endp l) (null sublist))
  #     (when (eq l sublist) (return t))
  # ) )
  #else
  # (defun tailp (sublist list)
  #   (loop
  #     (when (eq sublist list) (return t))
  #     (when (atom list) (return nil))
  #     (setq list (cdr list))
  # ) )
  #endif
  { var object list = popSTACK();
    var object sublist = popSTACK();
    #ifndef X3J13_175
    loop
      { if (endp(list)) goto end;
        if (eq(list,sublist)) goto yes;
        list = Cdr(list);
      }
    end:
      if (nullp(sublist)) goto yes;
    #else
    loop
      { if (eq(list,sublist)) goto yes;
        if (atomp(list)) break;
        list = Cdr(list);
      }
    #endif
      value1 = NIL; mv_count=1; return; # NIL als Wert
    yes:
      value1 = T; mv_count=1; return; # T als Wert
  }

LISPFUN(adjoin,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
  # (ADJOIN item list :test :test-not :key), CLTL S. 276
  { # erst Test auf (MEMBER (key item) list :test :test-not :key):
    test_key_arg(); # :KEY-Argument in STACK_0
   {var up_function up_fun = test_test_args(); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    {var object item = STACK_4;
     pushSTACK(item); # item retten
     pushSTACK(item); funcall(STACK_2,1); STACK_5 = value1; # item := (funcall key item)
    }
    pushSTACK(L(adjoin)); # Aufrufer
    # Stackaufbau: (key item), list, test, test-not, key, item, #'adjoin.
    if (nullp(member(STACK_5,&STACK_3,up_fun))) # Suche durchführen
      # item noch nicht in list gefunden: muss consen
      { var object new_cons = allocate_cons();
        Cdr(new_cons) = STACK_5; # = list
        Car(new_cons) = STACK_1; # = item
        value1 = new_cons;
      }
      else
      { value1 = STACK_5; } # list als Wert
    mv_count=1; skipSTACK(7); return;
  }}

LISPFUNN(acons,3)
  # (ACONS key val alist) = (CONS (CONS key val) alist), CLTL S. 279
  {{var object new_cons = allocate_cons();
    Car(new_cons) = STACK_2; # key
    Cdr(new_cons) = STACK_1; # value
    STACK_1 = new_cons;
   }
   {var object new_cons = allocate_cons();
    Cdr(new_cons) = popSTACK(); # alist
    Car(new_cons) = popSTACK(); # (key . val)
    value1 = new_cons; mv_count=1;
    skipSTACK(1);
  }}

LISPFUN(pairlis,2,1,norest,nokey,0,NIL)
  # (PAIRLIS keys data [alist]), CLTL S. 280
  { if (eq(STACK_0,unbound)) { STACK_0=NIL; } # NIL als Default für alist
   {var object keys_list = STACK_2;
    var object data_list = STACK_1;
    pushSTACK(keys_list);
    pushSTACK(data_list);
   }
    loop # Stackaufbau: keys, data, alist, keysr, datar.
      { if (matomp(STACK_0)) # data-Liste zu Ende?
          # ja
          if (matomp(STACK_1)) # keys-Liste auch zu Ende?
            goto end;
            else
            goto fehler_lengths;
          # nein
          else
          if (matomp(STACK_1)) # aber keys-Liste zu Ende?
            goto fehler_lengths;
            else
            { var object new_cons = allocate_cons();
              Car(new_cons) = Car(STACK_1); # nächstes key als CAR
              Cdr(new_cons) = Car(STACK_0); # nächstes data als CDR
              STACK_1 = Cdr(STACK_1); # keys verkürzen
              STACK_0 = Cdr(STACK_0); # data verkürzen
              pushSTACK(new_cons);
              new_cons = allocate_cons(); # weiteres neues Cons
              Car(new_cons) = popSTACK(); # mit (key . data) als CAR
              Cdr(new_cons) = STACK_2; # und alist als CDR
              STACK_2 = new_cons; # ergibt neues alist
      }     }
    fehler_lengths:
      skipSTACK(3);
      { var object data_list = popSTACK();
        var object keys_list = popSTACK();
        pushSTACK(data_list); pushSTACK(keys_list); pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: lists ~ and ~ are not of same length")
              );
      }
    end: value1 = STACK_2; mv_count=1; skipSTACK(5); # alist als Wert
  }

# UP: Liefert das erste Listenelement, dessen CAR der TESTFUNktion genügt.
# assoc(alist,stackptr)
# > alist: Aliste
# > stackptr: *(stackptr-1) = KEY
# > up_fun: TESTFUN = Adresse der Testfunktion, wird für alle Listenelemente
#       (u . v) mit selbem stackptr und mit (KEY u) als Argument angesprungen.
#       Sie liefert TRUE, falls der Test erfüllt ist, FALSE sonst.
# < ergebnis: Listenelement (ein Cons) oder NIL
# can trigger GC
  local object assoc (object alist, object* stackptr, up_function up_fun);
  local object assoc(alist,stackptr,up_fun)
    var object alist;
    var object* stackptr;
    var up_function up_fun;
    { start:
      if (atomp(alist))
        # Listenende erreicht -> ergibt Ergebnis NIL
        return NIL;
        else
        { if (mconsp(Car(alist))) # atomare Listenelemente überspringen
            { pushSTACK(alist); # Listenrest ((u . v) ...) retten
              pushSTACK(Car(Car(alist))); funcall(*(stackptr STACKop -1),1); # (KEY u)
             {var boolean erg = up_fun(stackptr,value1); # TESTFUN aufrufen
              alist = popSTACK();
              if (erg)
                # Test erfüllt -> x = (u . v) = (CAR alist) als Ergebnis
                return Car(alist);
              # Test nicht erfüllt
            }}
          # tail-end-rekursiv (assoc ... (cdr alist)) aufrufen:
          alist = Cdr(alist); goto start;
    }   }

LISPFUN(assoc,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
  # (ASSOC item alist :test :test-not :key), CLTL S. 280
  { test_key_arg(); # :KEY-Argument in STACK_0
   {var up_function up_fun = test_test_args(); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    value1 = assoc(STACK_3,&STACK_1,up_fun); # Suche durchführen
    mv_count=1;
    skipSTACK(5);
  }}

LISPFUN(assoc_if,2,0,norest,key,1, (kw(key)) )
  # (ASSOC-IF pred alist :key), CLTL S. 280
  { test_key_arg(); # :KEY-Argument in STACK_0
    value1 = assoc(STACK_1,&STACK_1,&up_if); # Suche durchführen
    mv_count=1;
    skipSTACK(3);
  }

LISPFUN(assoc_if_not,2,0,norest,key,1, (kw(key)) )
  # (ASSOC-IF-NOT pred alist :key), CLTL S. 280
  { test_key_arg(); # :KEY-Argument in STACK_0
    value1 = assoc(STACK_1,&STACK_1,&up_if_not); # Suche durchführen
    mv_count=1;
    skipSTACK(3);
  }

# UP: Liefert das erste Listenelement, dessen CDR der TESTFUNktion genügt.
# rassoc(alist,stackptr)
# > alist: Aliste
# > stackptr: *(stackptr-1) = KEY
# > up_fun: TESTFUN = Adresse der Testfunktion, wird für alle Listenelemente
#       (u . v) mit selbem stackptr und mit (KEY v) als Argument angesprungen.
#       Sie liefert TRUE, falls der Test erfüllt ist, FALSE sonst.
# < ergebnis: Listenelement (ein Cons) oder NIL
# can trigger GC
  local object rassoc (object alist, object* stackptr, up_function up_fun);
  local object rassoc(alist,stackptr,up_fun)
    var object alist;
    var object* stackptr;
    var up_function up_fun;
    { start:
      if (atomp(alist))
        # Listenende erreicht -> ergibt Ergebnis NIL
        return NIL;
        else
        { if (mconsp(Car(alist))) # atomare Listenelemente überspringen
            { pushSTACK(alist); # Listenrest ((u . v) ...) retten
              pushSTACK(Cdr(Car(alist))); funcall(*(stackptr STACKop -1),1); # (KEY v)
             {var boolean erg = up_fun(stackptr,value1); # TESTFUN aufrufen
              alist = popSTACK();
              if (erg)
                # Test erfüllt -> x = (u . v) = (CAR alist) als Ergebnis
                return Car(alist);
              # Test nicht erfüllt
            }}
          # tail-end-rekursiv (rassoc ... (cdr alist)) aufrufen:
          alist = Cdr(alist); goto start;
    }   }

LISPFUN(rassoc,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
  # (RASSOC item alist :test :test-not :key), CLTL S. 281
  { test_key_arg(); # :KEY-Argument in STACK_0
   {var up_function up_fun = test_test_args(); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    value1 = rassoc(STACK_3,&STACK_1,up_fun); # Suche durchführen
    mv_count=1;
    skipSTACK(5);
  }}

LISPFUN(rassoc_if,2,0,norest,key,1, (kw(key)) )
  # (RASSOC-IF pred alist :key), CLTL S. 281
  { test_key_arg(); # :KEY-Argument in STACK_0
    value1 = rassoc(STACK_1,&STACK_1,&up_if); # Suche durchführen
    mv_count=1;
    skipSTACK(3);
  }

LISPFUN(rassoc_if_not,2,0,norest,key,1, (kw(key)) )
  # (RASSOC-IF-NOT pred alist :key), CLTL S. 281
  { test_key_arg(); # :KEY-Argument in STACK_0
    value1 = rassoc(STACK_1,&STACK_1,&up_if_not); # Suche durchführen
    mv_count=1;
    skipSTACK(3);
  }

# Funktionen, die Listen zu Sequences machen:

LISPFUNN(list_upd,2)
  # #'(lambda (seq pointer) (cdr pointer))
  { value1 = cdr(popSTACK()); mv_count=1; skipSTACK(1); }

LISPFUNN(list_endtest,2)
  # #'(lambda (seq pointer) (atom pointer))
  { value1 = (matomp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(2); }

LISPFUNN(list_fe_init,1)
  # #'(lambda (seq) (revappend seq nil))
  { pushSTACK(NIL); C_revappend(); }

LISPFUNN(list_access,2)
  # #'(lambda (seq pointer) (car pointer))
  { var object pointer = popSTACK();
    if (atomp(pointer)) fehler_cons(pointer);
    value1 = Car(pointer); mv_count=1;
    skipSTACK(1);
  }

LISPFUNN(list_access_set,3)
  # #'(lambda (seq pointer value) (rplaca pointer value))
  { if (matomp(STACK_1)) fehler_cons(STACK_1);
   {var object value = popSTACK();
    var object pointer = popSTACK();
    Car(pointer) = value;
    value1 = value; mv_count=1;
    skipSTACK(1);
  }}

LISPFUNN(list_llength,1)
  # #'(lambda (seq) (do ((L seq (cdr L)) (N 0 (1+ N))) ((atom L) N)))
  { value1 = fixnum(llength(popSTACK())); mv_count=1; }

# UP: Läuft bis zum Element index in einer Liste.
# elt_up(seq,index)
# > seq
# > index
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Listenendstück ab diesem Index
  local object elt_up (object seq, object index);
  local object elt_up(seq,index)
    var object seq;
    var object index;
    # (do ((L seq (cdr L)) (N 0 (1+ N)))
    #     (nil)
    #   (if (atom L) (error "Zu großer Index in ELT: ~S" index))
    #   (if (= N index) (return L))
    # )
    { var object l = seq;
      var object n = Fixnum_0;
      loop
        { if (atomp(l)) goto index_too_large;
          if (eq(n,index)) break;
          l = Cdr(l);
          n = fixnum_inc(n,1);
        }
      return l;
      index_too_large:
        pushSTACK(index); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(NIL);
        pushSTACK(seq); pushSTACK(index); pushSTACK(TheSubr(subr_self)->name);
        { var object tmp;
          pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(n);
          tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
          STACK_3 = tmp; # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        }
        fehler(type_error,
               GETTEXT("~: index ~ too large for ~")
              );
    }

LISPFUNN(list_elt,2)
  # #'(lambda (seq index)
  #     (do ((L seq (cdr L)) (N 0 (1+ N)))
  #         (nil)
  #       (if (atom L) (error "Zu großer Index in ELT: ~S" index))
  #       (if (= N index) (return (car L)))
  #   ) )
  { var object index = popSTACK();
    var object seq = popSTACK();
    value1 = Car(elt_up(seq,index)); mv_count=1;
  }

LISPFUNN(list_set_elt,3)
  # #'(lambda (seq index value)
  #     (do ((L seq (cdr L)) (N 0 (1+ N)))
  #         (nil)
  #       (if (atom L) (error "Zu großer Index in ELT: ~S" index))
  #       (if (= N index) (return (rplaca L value)))
  #   ) )
  { var object nthcdr = elt_up(STACK_2,STACK_1);
    value1 = Car(nthcdr) = popSTACK(); mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(list_init_start,2)
  # #'(lambda (seq index)
  #     (do ((L seq (cdr L)) (N 0 (1+ N)))
  #         ((= N index) (return L))
  #       (if (atom L) (error "Unzulässiger :START - Index : ~S" index))
  #   ) )
    { var object index = popSTACK();
      var object seq = popSTACK();
     {var object l = seq;
      var object n = Fixnum_0;
      loop
        { if (eq(n,index)) break;
          if (atomp(l)) goto index_too_large;
          l = Cdr(l);
          n = fixnum_inc(n,1);
        }
      value1 = l; mv_count=1; return;
      index_too_large:
        pushSTACK(seq);
        pushSTACK(index); # Wert für Slot DATUM von TYPE-ERROR
        { var object tmp;
          pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(n);
          tmp = listof(3); pushSTACK(tmp); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        }
        pushSTACK(STACK_2); # seq
        pushSTACK(STACK_2); # index
        pushSTACK(S(list_init_start));
        fehler(type_error,
               GETTEXT("~: start index ~ too large for ~")
              );
    }}

LISPFUNN(list_fe_init_end,2)
  # #'(lambda (seq index)
  #     (if (<= 0 index)
  #       (do* ((L1 nil (cons (car L2) L1))
  #             (L2 seq (cdr L2))
  #             (i index (1- i)))
  #            ((zerop i) L1)
  #         (if (atom L2)
  #           (error "Unzulässiger :END - Index : ~S" index)
  #       ) )
  #       (error "Unzulässiger :END - Index : ~S" index)
  #   ) )
  { # index ist sowieso ein Integer >=0.
    pushSTACK(NIL); # L1 := nil
    {var object seq = STACK_2; pushSTACK(seq); } # L2 := seq
    pushSTACK(Fixnum_0); # i := 0
    loop
      { # Stackaufbau: seq, index, L1, L2, i
        if (eq(STACK_0,STACK_3)) # i=index ?
          goto end;
        if (matomp(STACK_1)) # (atom L2) ?
          goto index_too_large;
       {var object new_cons = allocate_cons(); # neues Cons
        var object L2 = STACK_1; STACK_1 = Cdr(L2); # (pop L2)
        Car(new_cons) = Car(L2); # als CAR
        Cdr(new_cons) = STACK_2; # L1 als CDR
        STACK_2 = new_cons; # L1 := neues Cons
        STACK_0 = fixnum_inc(STACK_0,1); # i := i+1
      }}
    index_too_large:
      pushSTACK(STACK_3); # Wert für Slot DATUM von TYPE-ERROR
      { var object tmp;
        pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(STACK_(0+3));
        tmp = listof(3); pushSTACK(tmp); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      }
      pushSTACK(STACK_(4+2));
      pushSTACK(STACK_(3+3));
      pushSTACK(S(list_fe_init_end));
      fehler(type_error,
             GETTEXT("~: end index ~ too large for ~")
            );
    end:
      value1 = STACK_2; mv_count=1; # L1 als Wert
      skipSTACK(5);
  }

