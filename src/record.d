# Functions for records and structures in CLISP
# Bruno Haible 1990-2002
# Sam Steingold 1998-2002

#include "lispbibl.c"


# ===========================================================================
# general records:

# (SYS::%RECORD-REF record index) return the index'th entry in the record.
# (SYS::%RECORD-STORE record index value) store value as the index'th
#   entry in the record and return value.
# (SYS::%RECORD-LENGTH record) return the length of the record.

# Error message
# > STACK_1: record
# > STACK_0: (bad) index
# > limit: exclusive upper bound on the index
# > subr_self: caller (a SUBR)
nonreturning_function(local, fehler_index, (uintL limit)) {
  pushSTACK(STACK_0); # TYPE-ERROR slot DATUM
  {
    var object tmp;
    pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(limit));
    tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
    pushSTACK(tmp); # TYPE-ERROR slot EXPECTED-TYPE
  }
  pushSTACK(STACK_(1+2)); # record
  pushSTACK(STACK_(0+3)); # index
  pushSTACK(TheSubr(subr_self)->name); # function name
  fehler(type_error,GETTEXT("~: ~ is not a valid index into ~"));
}

# Error message
# > STACK_0: (bad) record
# > subr_self: caller (a SUBR)
nonreturning_function(local, fehler_record, (void)) {
  pushSTACK(TheSubr(subr_self)->name); # function name
  fehler(error, # type_error ??
         GETTEXT("~: ~ is not a record"));
}

# Subroutine for record access functions
# > STACK_1: record argument
# > STACK_0: index argument
# > subr_self: caller (a SUBR)
# < STACK: cleared up
# < returns: the address of the referred record item
local object* record_up (void) {
  # the record must be a Closure/Structure/Stream/OtherRecord:
  if_recordp(STACK_1, ; , { skipSTACK(1); fehler_record(); } );
  var object record = STACK_1;
  var uintL length = Record_length(record);
  var uintL index;
  if (!(posfixnump(STACK_0) && ((index = posfixnum_to_L(STACK_0)) < length)))
    # extract and check index
    fehler_index(length);
  skipSTACK(2); # clear up stack
  return &TheRecord(record)->recdata[index]; # record element address
}

# (SYS::%RECORD-REF record index) return the index'th entry in the record
LISPFUNN(record_ref,2) {
  value1 = *(record_up()); mv_count=1; # record element as value
}

# (SYS::%RECORD-STORE record index value) store value as the index'th
#   entry in the record and return value.
LISPFUNN(record_store,3) {
  var object value = popSTACK();
  value1 = *(record_up()) = value; mv_count=1; # set record element
}

# (SYS::%RECORD-LENGTH record) return the length of the record.
LISPFUNN(record_length,1) {
  # the record must be a Closure/Structure/Stream/OtherRecord:
  if_recordp(STACK_0, ; , { fehler_record(); } );
  var object record = popSTACK();
  var uintL length = Record_length(record);
  value1 = fixnum(length); mv_count=1; # Länge als Fixnum
}

# check that the length is of type (INTEGER (0) (65536))
# > STACK_0: length
# > subr_self: caller (a SUBR)
# < uintL length: checked length
#define test_record_length(length)                                           \
  if (!(posfixnump(STACK_0)                                                  \
        && ((length = posfixnum_to_L(STACK_0)) <= (uintL)(bitm(intWsize)-1)) \
        && (length>0)))                                                      \
    fehler_record_length()
nonreturning_function(local, fehler_record_length, (void)) {
  # STACK_0 = length, TYPE-ERROR slot DATUM
  pushSTACK(O(type_posint16)); # TYPE-ERROR slot EXPECTED-TYPE
  pushSTACK(O(type_posint16)); # type
  pushSTACK(STACK_1); # length
  pushSTACK(TheSubr(subr_self)->name); # function name
  fehler(type_error,GETTEXT("~: length ~ is illegal, should be of type ~"));
}

# UP: find OBJ in LIS
local inline bool obj_in_list (const object obj, const object lis) {
  var object l = lis;
  while (consp(l)) {
    if (eq(Car(l),obj)) return true;
    l = Cdr(l);
  }
  return false;
}

# ===========================================================================
# Structures:

# (SYS::%STRUCTURE-REF type structure index) liefert zu einer Structure vom
#   gegebenen Typ type (ein Symbol) den Eintrag index>=1.
# (SYS::%STRUCTURE-STORE type structure index object) speichert object als
#   Eintrag index in einer Structure vom gegebenen Typ type und liefert object.
# (SYS::%MAKE-STRUCTURE type length) erzeugt eine Structure mit length>=1
#   Elementen, vom Typ type.
# (COPY-STRUCTURE structure) liefert eine Kopie der Structure structure,
#   vom selben Typ.
# (SYS::%STRUCTURE-TYPE-P type object) überprüft, ob object eine
#   Structure ist, die vom Typ type ist, was daran erkennbar ist, dass in
#   der Komponente 0 ein Objekt (name_1 ... name_i-1 name_i) steht, wobei
#   einer der Namen EQ zu type ist.

# Unterprogramm für Structure-Zugriffsfunktionen:
# > STACK_2: type-Argument
# > STACK_1: structure-Argument
# > STACK_0: index-Argument
# > subr_self: caller (a SUBR)
# < ergebnis: Adresse des angesprochenen Structure-Elements
  local object* structure_up (void);
  local object* structure_up ()
    {
      # structure muss vom Typ Structure sein:
      if (!structurep(STACK_1)) {
        fehler_bad_structure: # STACK_2 = type, STACK_1 = structure
        pushSTACK(STACK_1); # TYPE-ERROR slot DATUM
        pushSTACK(STACK_(2+1)); # TYPE-ERROR slot EXPECTED-TYPE
        pushSTACK(STACK_(2+2));
        pushSTACK(STACK_(1+3));
        pushSTACK(TheSubr(subr_self)->name); # function name
        fehler(type_error,
               GETTEXT("~: ~ is not a structure of type ~")
              );
      }
      var object structure = STACK_1;
      # Teste, ob in namelist = (name_1 ... name_i-1 name_i) type vorkommt:
      if (obj_in_list(STACK_2,TheStructure(structure)->structure_types))
        goto yes;
      # type kam nicht vor -> Error:
      goto fehler_bad_structure;
      # type kam vor:
     yes:
      {
        var uintL length = (uintL)Structure_length(structure);
        var uintL index;
        if (!(posfixnump(STACK_0) && ((index = posfixnum_to_L(STACK_0)) < length))) # Index holen und prüfen
          fehler_index(length);
        return &TheStructure(structure)->recdata[index]; # Structure-Komponente adressieren
      }
    }

LISPFUNN(pstructure_ref,3)
# (SYS::%%STRUCTURE-REF type structure index) liefert zu einer Structure vom
#   gegebenen Typ type (ein Symbol) den Eintrag index>=1.
# #<UNBOUND> ist möglich.
  {
    value1 = *(structure_up()); mv_count=1; # Structure-Element als Wert
    skipSTACK(3); # Stack aufräumen
  }

LISPFUNN(structure_ref,3)
# (SYS::%STRUCTURE-REF type structure index) liefert zu einer Structure vom
#   gegebenen Typ type (ein Symbol) den Eintrag index>=1.
  {
    value1 = *(structure_up()); # Structure-Element als Wert
    if (eq(value1,unbound)) { # Könnte = #<UNBOUND> sein, nach Gebrauch von SLOT-MAKUNBOUND oder nach unvollständigem INITIALIZE-INSTANCE
      dynamic_bind(S(print_length),Fixnum_0); # *PRINT-LENGTH* an 0 binden
      pushSTACK(STACK_(1+3)); # Wert für Slot INSTANCE von UNBOUND-SLOT
      # (clos::slotdef-name (find index (clos::class-slots (find-class type)) :key #'clos::slotdef-location))
      pushSTACK(STACK_(2+3+1)); funcall(L(find_class),1);
      pushSTACK(value1); funcall(S(class_slots),1);
      pushSTACK(STACK_(0+3+1)); pushSTACK(value1); pushSTACK(S(Kkey)); pushSTACK(Symbol_function(S(slotdef_location))); funcall(L(find),4);
      pushSTACK(value1); funcall(S(slotdef_name),1);
      pushSTACK(value1); # Wert für Slot NAME von UNBOUND-SLOT
      pushSTACK(STACK_(1+3+2));
      pushSTACK(value1);
      pushSTACK(S(structure_ref));
      fehler(unbound_slot,
             GETTEXT("~: Slot ~ of ~ has no value")
            );
    }
    mv_count=1;
    skipSTACK(3); # Stack aufräumen
  }

LISPFUNN(structure_store,4)
# (SYS::%STRUCTURE-STORE type structure index object) speichert object als
#   Eintrag index in einer Structure vom gegebenen Typ type und liefert object.
  {
    var object value = popSTACK();
    value1 = *(structure_up()) = value; mv_count=1; # Structure-Element eintragen
    skipSTACK(3); # Stack aufräumen
  }

LISPFUNN(make_structure,2)
# (SYS::%MAKE-STRUCTURE type length) erzeugt eine Structure mit length>=1
#   Elementen, vom Typ type.
  {
    # Länge überprüfen, sollte ein Fixnum /=0 sein, das in ein uintW passt:
    var uintL length;
    test_record_length(length);
    skipSTACK(1);
    var object structure = allocate_structure(length);
    # neue Structure, mit NILs gefüllt
    TheStructure(structure)->structure_types = popSTACK(); # Typ-Komponente eintragen
    value1 = structure; mv_count=1; # structure als Wert
  }

LISPFUNN(copy_structure,1)
# (COPY-STRUCTURE structure) liefert eine Kopie der Structure structure,
#   vom selben Typ.
  {
    if (!structurep(STACK_0)) {
      # STACK_0 = TYPE-ERROR slot DATUM
      pushSTACK(S(structure_object)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(STACK_1); # structure
      pushSTACK(TheSubr(subr_self)->name); # function name
      fehler(type_error,
             GETTEXT("~: ~ is not a structure")
            );
    }
    var uintC length = Structure_length(STACK_0);
    var object new_structure = allocate_structure(length); # neue Structure
    # und füllen:
    {
      var object* old_ptr = &TheStructure(popSTACK())->structure_types;
      var object* new_ptr = &TheStructure(new_structure)->structure_types;
      dotimespC(length,length, { *new_ptr++ = *old_ptr++; });
    }
    # und als Wert zurück:
    value1 = new_structure; mv_count=1;
  }

LISPFUNN(structure_type_p,2)
# (SYS::%STRUCTURE-TYPE-P type object) überprüft, ob object eine
#   Structure ist, die vom Typ type ist, was daran erkennbar ist, dass in
#   der Komponente 0 ein Objekt (name_1 ... name_i-1 name_i) steht, wobei
#   einer der Namen EQ zu type ist.
  {
    # object auf Structure testen:
    if (!structurep(STACK_0)) { skipSTACK(2); goto no; }
    {
      var object namelist = TheStructure(popSTACK())->structure_types;
      var object type = popSTACK();
      # Teste, ob in namelist = (name_1 ... name_i-1 name_i) type vorkommt:
      if (obj_in_list(type,namelist))
        goto yes;
    }
    # type kam nicht vor:
   no:
    value1 = NIL; mv_count=1; return; # 1 Wert NIL
    # type kam vor:
   yes:
    value1 = T; mv_count=1; return; # 1 Wert T
  }

# ===========================================================================
# Closures:

# (SYS::CLOSURE-NAME closure) liefert den Namen einer Closure.
# (SYS::CLOSURE-CODEVEC closure) liefert den Code-Vektor einer compilierten
#   Closure, als Liste von Fixnums >=0, <256.
# (SYS::CLOSURE-CONSTS closure) liefert eine Liste aller Konstanten einer
#   compilierten Closure.
# (SYS::MAKE-CODE-VECTOR list) liefert zu einer Liste von Fixnums >=0, <256
#   einen Simple-Bit-Vector der 8-fachen Länge, der diese Zahlen als Bytes
#   enthält.
# (SYS::%MAKE-CLOSURE name codevec consts) liefert eine Closure mit gegebenem
#   Namen (einem Symbol), gegebenem Code-Vektor (einem Simple-Bit-Vector) und
#   gegebenen weiteren Konstanten.
# (SYS::%COPY-GENERIC-FUNCTION venv closure) copies the closure, which must be
#   a generic function with venv slot, copying in the given venv.
# (SYS::GENERIC-FUNCTION-EFFECTIVE-METHOD-FUNCTION generic-function)
#   liefert eine Funktion, die die effektiven Methoden liefert, d.h. so dass
#   (APPLY generic-function arguments)
#   == (APPLY (APPLY ergebnis arguments) arguments)  wäre.

LISPFUNN(closure_name,1)
# (SYS::CLOSURE-NAME closure) liefert den Namen einer Closure.
  {
    var object closure = popSTACK();
    if (!closurep(closure)) {
      pushSTACK(closure);
      pushSTACK(TheSubr(subr_self)->name); # function name
      fehler(error, # type_error ??
             GETTEXT("~: ~ is not a closure")
            );
    }
    value1 = TheClosure(closure)->clos_name; mv_count=1;
  }

# Fehler, wenn Argument keine compilierte Closure
  nonreturning_function(local, fehler_cclosure, (object obj)) {
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); # function name
    fehler(error, # type_error ??
           GETTEXT("~: This is not a compiled closure: ~")
          );
  }

LISPFUNN(closure_codevec,1)
# (SYS::CLOSURE-CODEVEC closure) liefert den Code-Vektor einer compilierten
#   Closure, als Liste von Fixnums >=0, <256.
  {
    var object closure = popSTACK();
    if (!(cclosurep(closure))) fehler_cclosure(closure);
    var object codevec = TheCclosure(closure)->clos_codevec;
    var uintL index = Sbvector_length(codevec); # index := Länge in Bytes
    # Codevektor codevec von hinten durchgehen und Bytes auf eine Liste pushen:
    pushSTACK(codevec); # Codevektor
    pushSTACK(NIL); # Liste := ()
    until (index==0) {
      index--; # Index decrementieren
      # neues Cons vor die Liste setzen:
      var object new_cons = allocate_cons();
      Cdr(new_cons) = popSTACK();
      Car(new_cons) = fixnum((uintL)(TheSbvector(STACK_0)->data[index])); # Byte herausholen
      pushSTACK(new_cons);
    }
    value1 = STACK_0; mv_count=1; skipSTACK(2); # Liste als Wert
  }

LISPFUNN(closure_consts,1)
# (SYS::CLOSURE-CONSTS closure) liefert eine Liste aller Konstanten einer
#   compilierten Closure.
  {
    var object closure = popSTACK();
    if (!(cclosurep(closure))) fehler_cclosure(closure);
    # Elemente 2,3,... zu einer Liste zusammenfassen:
    var uintC index = Cclosure_length(closure)-2; # index := Länge
    # Closure von hinten durchgehen und Konstanten auf eine Liste pushen:
    pushSTACK(closure); # Closure
    pushSTACK(NIL); # Liste := ()
    until (index==0) {
      index--; # Index decrementieren
      # neues Cons vor die Liste setzen:
      var object new_cons = allocate_cons();
      Cdr(new_cons) = popSTACK();
      Car(new_cons) = TheCclosure(STACK_0)->clos_consts[(uintP)index]; # Konstante herausholen
      pushSTACK(new_cons);
    }
    value1 = STACK_0; mv_count=1; skipSTACK(2); # Liste als Wert
  }

LISPFUNN(make_code_vector,1)
# (SYS::MAKE-CODE-VECTOR list) liefert zu einer Liste von Fixnums >=0, <256
#   einen Simple-Bit-Vector der 8-fachen Länge, der diese Zahlen als Bytes
#   enthält.
  {
    var object bv = allocate_bit_vector(Atype_8Bit,llength(STACK_0)); # Simple-8Bit-Vektor
    # füllen:
    var object listr = popSTACK(); # Liste
    var uintB* ptr = &TheSbvector(bv)->data[0]; # läuft durch den Bit-Vektor
    while (consp(listr)) {
      var uintL byte;
      # Listenelement muss ein Fixnum >=0, <256 sein:
      if (!(posfixnump(Car(listr))
            && ((byte = posfixnum_to_L(Car(listr))) < (1<<intBsize))
         ) )
        goto bad_byte;
      # in den Bit-Vektor stecken:
      *ptr++ = (uintB)byte;
      listr = Cdr(listr);
    }
    value1 = bv; mv_count=1; return; # bv als Wert
   bad_byte:
    pushSTACK(Car(listr)); # TYPE-ERROR slot DATUM
    pushSTACK(O(type_uint8)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(STACK_1);
    fehler(type_error,
           GETTEXT("~ is not a valid code-vector byte")
          );
  }

LISPFUNN(make_closure,3)
# (SYS::%MAKE-CLOSURE name codevec consts) liefert eine Closure mit gegebenem
#   Namen (einem Symbol), gegebenem Code-Vektor (einem Simple-Bit-Vector) und
#   gegebenen weiteren Konstanten.
  {
    # codevec muss ein Simple-Bit-Vector sein:
    if (!simple_bit_vector_p(Atype_8Bit,STACK_1)) {
      # STACK_1 = codevec
      pushSTACK(STACK_1); # TYPE-ERROR slot DATUM
      pushSTACK(S(simple_bit_vector)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(STACK_(1+2));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: invalid code-vector ~")
            );
    }
    # neue Closure der Länge (+ 2 (length consts)) erzeugen:
    var uintL length = 2+llength(STACK_0);
    if (!(length <= (uintL)(bitm(intWsize)-1))) { # sollte in ein uintW passen
      # STACK_0 = consts
      pushSTACK(STACK_2); # name
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: function ~ is too big: ~")
            );
    }
    var object closure = allocate_closure(length);
    TheCclosure(closure)->clos_name = STACK_2; # Namen einfüllen
    TheCclosure(closure)->clos_codevec = STACK_1; # Codevektor einfüllen
    # Konstanten einfüllen:
    {
      var object constsr = popSTACK();
      var object* ptr = &TheCclosure(closure)->clos_consts[0];
      while (consp(constsr)) {
        *ptr++ = Car(constsr); constsr = Cdr(constsr);
      }
    }
    value1 = closure; mv_count=1; skipSTACK(2);
  }

LISPFUNN(copy_generic_function,2)
# (SYS::%COPY-GENERIC-FUNCTION venv closure) copies the closure, which must be
#   a generic function with venv slot, copying in the given venv.
  {
    # Note: closure's clos_consts[0] is a simple-vector #(NIL c1 ... cn) where
    # c1,...,cn are constant objects, and NIL is the placeholder to be replaced
    # with the passed venv.
    var object oldclos = STACK_0;
    if (!genericfunctionp(oldclos)) {
      pushSTACK(oldclos); # TYPE-ERROR slot DATUM
      pushSTACK(S(standard_generic_function)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(oldclos);
      pushSTACK(TheSubr(subr_self)->name); # function name
      fehler(type_error,
             GETTEXT("~: This is not a generic function: ~")
            );
    }
    var object vector = TheCclosure(oldclos)->clos_consts[0];
    if (!(simple_vector_p(vector)
          && (Svector_length(vector) > 0)
          && nullp(TheSvector(vector)->data[0]))) {
      pushSTACK(oldclos);
      pushSTACK(TheSubr(subr_self)->name); # function name
      fehler(error,
             GETTEXT("~: This is not a prototype of a generic function: ~")
            );
    }
    vector = copy_svector(vector); # copy the vector
    TheSvector(vector)->data[0] = STACK_1; # put in venv
    STACK_1 = vector;
    # Copy the function:
    var object newclos = allocate_cclosure_copy(STACK_0);
    oldclos = STACK_0;
    do_cclosure_copy(newclos,oldclos);
    # Put in the copied vector with venv:
    TheCclosure(newclos)->clos_consts[0] = STACK_1;
    value1 = newclos; mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(generic_function_effective_method_function,1)
# (SYS::GENERIC-FUNCTION-EFFECTIVE-METHOD-FUNCTION generic-function)
#   liefert eine Funktion, die die effektiven Methoden liefert, d.h. so dass
#   (APPLY generic-function arguments)
#   == (APPLY (APPLY ergebnis arguments) arguments)  wäre.
#   Verwendet für CALL-NEXT-METHOD; kann deswegen voraussetzen, dass die
#   generic-function schon aufgerufen wurde, d.h. dass der Dispatch schon
#   installiert ist.
  {
    var object oldclos = STACK_0;
    var object newclos;
    if (!genericfunctionp(oldclos)) {
      pushSTACK(oldclos); # TYPE-ERROR slot DATUM
      pushSTACK(S(standard_generic_function)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(oldclos);
      pushSTACK(TheSubr(subr_self)->name); # function name
      fehler(type_error,
             GETTEXT("~: This is not a generic function: ~")
            );
    }
    # Closure gleicher Länge allozieren:
    newclos = allocate_cclosure_copy(oldclos);
    oldclos = STACK_0;
    do_cclosure_copy(newclos,oldclos);
    STACK_0 = newclos;
    # Code-Vektor ebenfalls kopieren:
    var object newcodevec = copy_sbvector(TheClosure(newclos)->clos_codevec);
    # und darin das Bit setzen, das von der RETGF-Instruktion abgefragt wird:
    TheCodevec(newcodevec)->ccv_flags |= bit(3);
    newclos = popSTACK();
    TheClosure(newclos)->clos_codevec = newcodevec;
    value1 = newclos; mv_count=1;
  }

# ===========================================================================
# Load-Time-Eval:

# (SYS::MAKE-LOAD-TIME-EVAL form) liefert ein Load-Time-Eval-Objekt, das
#   - wenn ausgegeben und wieder eingelesen - form auswertet.

LISPFUNN(make_load_time_eval,1)
# (SYS::MAKE-LOAD-TIME-EVAL form) liefert ein Load-Time-Eval-Objekt, das
#   - wenn ausgegeben und wieder eingelesen - form auswertet.
  {
    var object lte = allocate_loadtimeeval();
    TheLoadtimeeval(lte)->loadtimeeval_form = popSTACK();
    value1 = lte; mv_count=1;
  }

# ===========================================================================
# Symbol-Macro:

# (SYS::MAKE-SYMBOL-MACRO expansion) liefert ein Symbol-Macro-Objekt,
#   das die gegebene Expansion repräsentiert.
# (SYS::SYMBOL-MACRO-P object) testet auf Symbol-Macro.

# Wegen ihrer besonderen Bedeutung im Interpreter sind Symbol-Macro-Objekte
# - genauso wie #<UNBOUND> und #<SPECDECL> - keine Objekte erster Klasse.
# Sie können nur als Werte durchgereicht, nicht aber an Variablen zugewiesen
# werden.

# (SYMBOL-MACRO-EXPAND symbol) testet, ob ein Symbol ein Symbol-Macro
# repräsentiert, und liefert T und die Expansion wenn ja, NIL wenn nein.

LISPFUNN(make_symbol_macro,1)
# (SYS::MAKE-SYMBOL-MACRO expansion) liefert ein Symbol-Macro-Objekt,
#   das die gegebene Expansion repräsentiert.
  {
    var object sm = allocate_symbolmacro();
    TheSymbolmacro(sm)->symbolmacro_expansion = popSTACK();
    value1 = sm; mv_count=1;
  }

LISPFUNN(symbol_macro_p,1)
# (SYS::SYMBOL-MACRO-P object) testet auf Symbol-Macro.
  {
    var object obj = popSTACK();
    value1 = (symbolmacrop(obj) ? T : NIL); mv_count=1;
  }

LISPFUNN(symbol_macro_expand,1)
# (SYMBOL-MACRO-EXPAND symbol) testet, ob ein Symbol ein Symbol-Macro
# repräsentiert, und liefert T und die Expansion wenn ja, NIL wenn nein.
# (defun symbol-macro-expand (v)
#   (unless (symbolp v) (error ...))
#   (and (boundp v) (symbol-macro-p (%symbol-value v))
#        (values t (sys::%record-ref (%symbol-value v) 0))
# ) )
  {
    var object obj = popSTACK();
    if (!symbolp(obj))
      fehler_symbol(obj);
    obj = Symbol_value(obj);
    if (!symbolmacrop(obj)) {
      value1 = NIL; mv_count=1; return;
    }
    value1 = T; value2 = TheSymbolmacro(obj)->symbolmacro_expansion; mv_count=2;
  }

# ===========================================================================
# Macro:

# (SYS::MAKE-MACRO expander) returns a Macro object with the given expander
# function.
# (SYS::MACROP object) tests for a Macro.
# (SYS::MACRO-EXPANDER macro) returns the macro's expander function.

LISPFUNN(make_macro,1)
# (SYS::MAKE-MACRO expander) returns a Macro object with the given expander
# function.
  {
    var object m = allocate_macro();
    var object arg = popSTACK();
    if (!functionp(arg))
      fehler_function(arg);
    TheMacro(m)->macro_expander = arg;
    value1 = m; mv_count=1;
  }

LISPFUNN(macrop,1)
# (SYS::MACROP object) tests for a Macro.
  {
    var object obj = popSTACK();
    value1 = (macrop(obj) ? T : NIL); mv_count=1;
  }

LISPFUNN(macro_expander,1)
# (SYS::MACRO-EXPANDER macro) returns the macro's expander function.
  {
    var object obj = popSTACK();
    if (!macrop(obj)) {
      pushSTACK(obj);
      pushSTACK(S(macro_expander)); # function name
      fehler(error, # type_error ??
             GETTEXT("~: ~ is not a Macro")
            );
    }
    value1 = TheMacro(obj)->macro_expander; mv_count=1;
  }

# ===========================================================================
# FunctionMacro:

# (SYS::MAKE-FUNCTION-MACRO function expander) returns a FunctionMacro object
# for the given function and with the given expander function.
# (SYS::FUNCTION-MACRO-P object) tests for a FunctionMacro.
# (SYS::FUNCTION-MACRO-FUNCTION macro) returns the functionmacro's function.
# (SYS::FUNCTION-MACRO-EXPANDER macro) returns the functionmacro's expander.

LISPFUNN(make_function_macro,2)
# (SYS::MAKE-FUNCTION-MACRO function expander) returns a FunctionMacro object
# for the given function and with the given expander function.
  {
    var object m = allocate_functionmacro();
    {
      var object arg = STACK_1;
      if (!functionp(arg))
        fehler_function(arg);
      TheFunctionMacro(m)->functionmacro_function = arg;
    }
    {
      var object arg = STACK_0;
      if (!functionp(arg))
        fehler_function(arg);
      TheFunctionMacro(m)->functionmacro_macro_expander = arg;
    }
    value1 = m; mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(function_macro_p,1)
# (SYS::FUNCTION-MACRO-P object) tests for a FunctionMacro.
  {
    var object obj = popSTACK();
    value1 = (functionmacrop(obj) ? T : NIL); mv_count=1;
  }

LISPFUNN(function_macro_function,1)
# (SYS::FUNCTION-MACRO-FUNCTION macro) returns the functionmacro's function.
  {
    var object obj = popSTACK();
    if (!functionmacrop(obj)) {
      pushSTACK(obj);
      pushSTACK(S(function_macro_function)); # function name
      fehler(error, # type_error ??
             GETTEXT("~: ~ is not a FunctionMacro")
            );
    }
    value1 = TheFunctionMacro(obj)->functionmacro_function; mv_count=1;
  }

LISPFUNN(function_macro_expander,1)
# (SYS::FUNCTION-MACRO-EXPANDER macro) returns the functionmacro's expander.
  {
    var object obj = popSTACK();
    if (!functionmacrop(obj)) {
      pushSTACK(obj);
      pushSTACK(S(function_macro_expander)); # function name
      fehler(error, # type_error ??
             GETTEXT("~: ~ is not a FunctionMacro")
            );
    }
    value1 = TheFunctionMacro(obj)->functionmacro_macro_expander; mv_count=1;
  }

# ===========================================================================
# Weak-Pointer:

# UP: make a weakpointer to popSTACK()
# can trigger GC, modifies STACK
local object mk_weakpointer () {
  var object wp = allocate_xrecord(0,Rectype_Weakpointer,weakpointer_length,
                                   weakpointer_xlength,orecord_type);
  var object obj = popSTACK();
  TheWeakpointer(wp)->wp_value = obj;
  if (gcinvariant_object_p(obj)) {
    TheWeakpointer(wp)->wp_cdr = Fixnum_0; # a GC-invariant dummy
  } else {
    TheWeakpointer(wp)->wp_cdr = O(all_weakpointers);
    O(all_weakpointers) = wp;
  }
  return wp;
}

# UP: allocates a Weakpointer to the given object
# allocate_weakpointer(obj)
# > obj: a Lisp object to which the result should point
# < result: a fresh weak-pointer
# can trigger GC
global object allocate_weakpointer (object obj) {
  pushSTACK(obj);
  return mk_weakpointer();
}

# (MAKE-WEAK-POINTER value) returns a fresh weak pointer referring to value.
LISPFUNN(make_weak_pointer,1) {
  value1 = mk_weakpointer(); mv_count=1;
}

# (WEAK-POINTER-P object) returns true if the object is of type WEAK-POINTER.
LISPFUNN(weak_pointer_p,1) {
  var object obj = popSTACK();
  value1 = (weakpointerp(obj) ? T : NIL); mv_count=1;
}

# (WEAK-POINTER-VALUE weak-pointer) returns two values: The original value
# and T, if the value has not yet been garbage collected, else NIL and NIL.
LISPFUNN(weak_pointer_value,1) {
  var object wp = popSTACK();
  if (!weakpointerp(wp)) {
    pushSTACK(wp); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(S(weak_pointer)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(wp);
    pushSTACK(TheSubr(subr_self)->name); # function name
    fehler(type_error,GETTEXT("~: ~ is not a weak pointer"));
  }
  if (weakpointer_broken_p(wp)) {
    value1 = NIL; value2 = NIL;
  } else {
    value1 = TheWeakpointer(wp)->wp_value; value2 = T;
  }
  mv_count=2;
}

# ===========================================================================
# Finalisierer:

LISPFUN(finalize,2,1,norest,nokey,0,NIL)
# (FINALIZE object function &optional alive)
# registiert, dass, wenn object durch GC stirbt, function aufgerufen wird, mit
# object und evtl. alive als Argument. Wenn alive stirbt, bevor object stirbt,
# wird gar nichts getan.
  {
    STACK_1 = coerce_function(STACK_1);
    if (!gcinvariant_object_p(STACK_2)) {
      var object f = allocate_finalizer();
      TheFinalizer(f)->fin_trigger = STACK_2;
      TheFinalizer(f)->fin_function = STACK_1;
      TheFinalizer(f)->fin_alive = STACK_0; # Der Default #<UNBOUND> lebt ewig.
      TheFinalizer(f)->fin_cdr = O(all_finalizers);
      O(all_finalizers) = f;
    }
    skipSTACK(3); value1 = NIL; mv_count=1;
  }

# ===========================================================================
# CLOS-Object:

LISPFUNN(structure_object_p,1)
# (CLOS::STRUCTURE-OBJECT-P object) überprüft, ob object eine Structure ist.
  {
    var object obj = popSTACK();
    value1 = (structurep(obj) ? T : NIL); mv_count=1;
  }

LISPFUNN(std_instance_p,1)
# (CLOS::STD-INSTANCE-P object) testet, ob ein Objekt ein CLOS-Objekt ist.
  {
    var object obj = popSTACK();
    value1 = (instancep(obj) ? T : NIL); mv_count=1;
  }

# Liefert (CLOS:CLASS-OF object), aber besonders effizient für CLOS-Objekte.
  #define class_of(obj)  \
    (instancep(obj) ? TheInstance(obj)->inst_class           \
                    : (pushSTACK(obj), C_class_of(), value1) \
    )

# Fehlermeldung, wenn ein Objekt keine Klasse ist.
# fehler_keine_klasse(caller,obj);
# > subr_self: Aufrufer
# > obj: Nicht-Klasse
  nonreturning_function(local, fehler_keine_klasse, (object obj)) {
    pushSTACK(obj); # TYPE-ERROR slot DATUM
    pushSTACK(S(class)); # CLOS:CLASS, TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); # function name
    fehler(type_error,
           GETTEXT("~: ~ is not a class")
          );
  }

LISPFUNN(allocate_std_instance,2)
# (CLOS::ALLOCATE-STD-INSTANCE class n) liefert eine CLOS-Instanz der Länge n,
# mit Klasse class und n-1 zusätzlichen Slots.
  {
    # Länge überprüfen, sollte ein Fixnum >0 sein, das in ein uintW passt:
    var uintL length;
    test_record_length(length);
    skipSTACK(1);
    var object instance = allocate_srecord(0,Rectype_Instance,length,instance_type);
    var object clas = popSTACK();
    if_classp(clas, ; , fehler_keine_klasse(clas); );
    TheInstance(instance)->inst_class = clas;
    # Slots der Instanz mit #<UNBOUND> füllen:
    if (--length > 0) {
      var object* ptr = &TheInstance(instance)->other[0];
      dotimespL(length,length, { *ptr++ = unbound; } );
    }
    value1 = instance; mv_count=1; # instance als Wert
  }

local Values do_allocate_instance (object clas);
LISPFUN(pallocate_instance,1,0,rest,nokey,0,NIL)
# (CLOS::%ALLOCATE-INSTANCE class &rest initargs) returns an instance of the class.
# class must be an instance of <standard-class> or <structure-class>.
{
  if (!((argcount%2) == 0)) {
    var object arglist = listof(argcount);
    pushSTACK(arglist);
    fehler(program_error,
           GETTEXT("ALLOCATE-INSTANCE: keyword argument list ~ has an odd length")
          );
  }
  set_args_end_pointer(rest_args_pointer); # STACK aufräumen
  return_Values do_allocate_instance(popSTACK());
}
local Values do_allocate_instance(clas)
  var object clas;
  {
    # Für allocate-instance zwischen <standard-class> und <structure-class>
    # unterscheiden: (class-shared-slots class) ein Vektor oder NIL, oder
    # (class-names class) ein Cons?
    if (matomp(TheClass(clas)->shared_slots)) {
      # class ist eine <standard-class>.
      # (CLOS::ALLOCATE-STD-INSTANCE class (class-instance-size class))
      pushSTACK(clas); pushSTACK(TheClass(clas)->instance_size);
      C_allocate_std_instance();
    } else {
      # class ist eine <structure-class>.
      # (SYS::%MAKE-STRUCTURE (class-names class) (class-instance-size class))
      pushSTACK(TheClass(clas)->shared_slots); pushSTACK(TheClass(clas)->instance_size);
      C_make_structure();
      # Slots der Structure mit #<UNBOUND> füllen, damit nachher
      # INITIALIZE-INSTANCE die Default-Werte einträgt:
      var uintL count = Structure_length(value1)-1;
      if (count > 0) {
        var object* ptr = &TheStructure(value1)->recdata[1];
        dotimespL(count,count, { *ptr++ = unbound; } );
      }
    }
  }

# (CLOS:SLOT-VALUE instance slot-name)
# (CLOS::SET-SLOT-VALUE instance slot-name new-value)
# (CLOS:SLOT-BOUNDP instance slot-name)
# (CLOS:SLOT-MAKUNBOUND instance slot-name)
# (CLOS:SLOT-EXISTS-P instance slot-name)
# CLtL2 S. 855,857

# Liefert aus einer Slot-Location-Info die Adresse eines existierenden Slots
# in einer Instanz einer Standard- oder Structure-Klasse.
  #define ptr_to_slot(instance,slotinfo)  \
    (atomp(slotinfo)                                            \
     # local slot, slotinfo ist Index                           \
     ? &TheSrecord(instance)->recdata[posfixnum_to_L(slotinfo)] \
     # shared slot, slotinfo ist (class . index)                \
     : &TheSvector(TheClass(Car(slotinfo))->shared_slots)       \
                  ->data[posfixnum_to_L(Cdr(slotinfo))]         \
    )

# UP: Sucht einen Slot auf.
# slot_up()
# > STACK_1: instance
# > STACK_0: slot-name
# < ergebnis: Pointer auf den Slot (dann ist value1 = (class-of instance)),
#             oder NULL (dann wurde SLOT-MISSING aufgerufen).
  local object* slot_up (void);
  #ifdef RISCOS_CCBUG
    #pragma -z0
  #endif
  local object* slot_up()
    {
      pushSTACK(STACK_1); C_class_of(); # (CLASS-OF instance) bestimmen
      var object slotinfo = # (GETHASH slot-name (class-slot-location-table class))
        gethash(STACK_0,TheClass(value1)->slot_location_table);
      if (!eq(slotinfo,nullobj)) { # gefunden?
        return ptr_to_slot(STACK_1,slotinfo);
      } else {
        # missing slot -> (SLOT-MISSING class instance slot-name caller)
        pushSTACK(value1); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
        pushSTACK(TheSubr(subr_self)->name);
        funcall(S(slot_missing),4);
        return NULL;
      }
    }
  #ifdef RISCOS_CCBUG
    #pragma -z1
  #endif

LISPFUNN(slot_value,2)
  {
    var object* slot = slot_up();
    if (slot) {
      var object value = *slot;
      if (!eq(value,unbound)) {
        value1 = value;
      } else {
        # (SLOT-UNBOUND class instance slot-name)
        pushSTACK(value1); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
        funcall(S(slot_unbound),3);
      }
    }
    mv_count=1;
    skipSTACK(2);
  }

#ifdef RISCOS_CCBUG
  #pragma -z0
#endif
LISPFUNN(set_slot_value,3)
  {
    # Stackaufbau: instance, slot-name, new-value.
    pushSTACK(STACK_2); C_class_of(); # (CLASS-OF instance) bestimmen
    var object slotinfo = # (GETHASH slot-name (class-slot-location-table class))
      gethash(STACK_1,TheClass(value1)->slot_location_table);
    if (!eq(slotinfo,nullobj)) { # gefunden?
      value1 = *ptr_to_slot(STACK_2,slotinfo) = STACK_0;
    } else {
      # missing slot -> (SLOT-MISSING class instance slot-name 'setf new-value)
      pushSTACK(value1); pushSTACK(STACK_(2+1)); pushSTACK(STACK_(1+2));
      pushSTACK(S(setf)); pushSTACK(STACK_(0+4));
      funcall(S(slot_missing),5);
      value1 = STACK_0;
    }
    mv_count=1;
    skipSTACK(3);
  }
#ifdef RISCOS_CCBUG
  #pragma -z1
#endif

LISPFUNN(slot_boundp,2)
  {
    var object* slot = slot_up();
    if (slot) {
      value1 = (eq(*slot,unbound) ? NIL : T);
    }
    mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(slot_makunbound,2)
  {
    var object* slot = slot_up();
    if (slot) {
      *slot = unbound;
    }
    value1 = STACK_1; mv_count=1; # instance als Wert
    skipSTACK(2);
  }

#ifdef RISCOS_CCBUG
  #pragma -z0
#endif
LISPFUNN(slot_exists_p,2)
  {
    pushSTACK(STACK_1); C_class_of(); # (CLASS-OF instance) bestimmen
    var object slotinfo = # (GETHASH slot-name (class-slot-location-table class))
      gethash(STACK_0,TheClass(value1)->slot_location_table);
    value1 = (eq(slotinfo,nullobj) ? NIL : T); mv_count=1; skipSTACK(2);
  }
#ifdef RISCOS_CCBUG
  #pragma -z1
#endif

# UP: Keywords überprüfen, vgl. SYSTEM::KEYWORD-TEST
# keyword_test(caller,rest_args_pointer,argcount,valid_keywords);
# > caller: caller (a symbol)
# > rest_args_pointer: Pointer über die Argumente
# > argcount: Anzahl der Argumente / 2
# > valid_keywords: Liste der gültigen Keywords, oder T wenn alle gültig sind
  local void keyword_test (object caller, object* rest_args_pointer, uintC argcount, object valid_keywords);
  local void keyword_test(caller,rest_args_pointer,argcount,valid_keywords)
    var object caller;
    var object* rest_args_pointer;
    var uintC argcount;
    var object valid_keywords;
    {
      if (argcount==0)
        return;
      if (eq(valid_keywords,T))
        return;
      # Suche, ob :ALLOW-OTHER-KEYS kommt:
      {
        var object* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,argcount, {
          if (eq(NEXT(ptr),S(Kallow_other_keys)))
            if (!nullp(Next(ptr)))
              return;
          NEXT(ptr);
        });
      }
      # Suche, ob alle angegebenen Keywords in valid_keywords vorkommen:
      {
        var object* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,argcount, {
          var object key = NEXT(ptr);
          if (obj_in_list(key,valid_keywords))
            goto kw_found;
          # nicht gefunden
          pushSTACK(key); # Wert für Slot DATUM von KEYWORD-ERROR
          pushSTACK(valid_keywords);
          pushSTACK(valid_keywords);
          pushSTACK(Next(ptr));
          pushSTACK(key);
          pushSTACK(caller);
          {
            var object type = allocate_cons();
            Car(type) = S(member); Cdr(type) = STACK_4;
            STACK_4 = type; # `(MEMBER ,@valid_keywords) = Wert für Slot EXPECTED-TYPE von KEYWORD-ERROR
          }
          fehler(keyword_error,
                 GETTEXT("~: illegal keyword/value pair ~, ~ in argument list." NLstring "The allowed keywords are ~")
                );
         kw_found: # gefunden. Weiter:
          NEXT(ptr);
        });
      }
    }

# UP: find initarg of the slot in the arglist
local inline object* slot_in_arglist (const object slot, uintC argcount,
                                      object* rest_args_pointer) {
  var object l = TheSvector(slot)->data[1]; # (slotdef-initargs slot)
  var object* ptr = rest_args_pointer;
  var uintC count;
  dotimespC(count,argcount, {
    var object initarg = NEXT(ptr);
    if (obj_in_list(initarg,l))
      return ptr;
    NEXT(ptr);
  });
  return NULL;
}

LISPFUN(shared_initialize,2,0,rest,nokey,0,NIL)
# (CLOS::%SHARED-INITIALIZE instance slot-names &rest initargs)
# instance ist eine Instanz von <standard-object> oder <structure-object>,
# initargs eine paarige Liste.
# Das ist die primäre Methode von CLOS:SHARED-INITIALIZE.
# vgl. clos.lisp
# (defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
#   (dolist (slot (class-slots (class-of instance)))
#     (let ((slotname (slotdef-name slot)))
#       (multiple-value-bind (init-key init-value foundp)
#           (get-properties initargs (slotdef-initargs slot))
#         (declare (ignore init-key))
#         (if foundp
#           (setf (slot-value instance slotname) init-value)
#           (unless (slot-boundp instance slotname)
#             (let ((init (slotdef-initer slot)))
#               (when init
#                 (when (or (eq slot-names 'T) (member slotname slot-names :test #'eq))
#                   (setf (slot-value instance slotname)
#                         (if (car init) (funcall (car init)) (cdr init))
#   ) ) ) ) ) ) ) ) )
#   instance
# )
  {
    if (!((argcount%2) == 0)) {
      var object arglist = listof(argcount);
      pushSTACK(arglist);
      fehler(program_error,
             GETTEXT("SHARED-INITIALIZE: keyword argument list ~ has an odd length")
            );
    }
    argcount = argcount/2; # Anzahl der Initarg/Wert-Paare
    # Stackaufbau: instance, slot-names, argcount Initarg/Wert-Paare.
    {
      var object instance = Before(rest_args_pointer STACKop 1);
      var object clas = class_of(instance); # Instanz der <standard-class> oder <structure-class>
      var object slots = TheClass(clas)->slots; # Liste aller Slots (als Slot-Definitionen)
      while (consp(slots)) {
        var object slot = Car(slots);
        slots = Cdr(slots);
        # Suche ob der Slot durch die Initargs initialisiert wird:
        if (argcount > 0) {
          var object* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
          if (NULL == ptr)
            goto initarg_not_found;
          value1 = NEXT(ptr);
          goto fill_slot;
        }
       initarg_not_found:
        # Nicht gefunden -> erst auf (slot-boundp instance slotname) testen:
        {
          var object slotinfo = TheSvector(slot)->data[2]; # (slotdef-location slot)
          if (!eq(*ptr_to_slot(Before(rest_args_pointer STACKop 1),slotinfo),unbound))
            goto slot_done;
        }
        # Slot hat noch keinen Wert. Evtl. die Initform auswerten:
        {
          var object init = TheSvector(slot)->data[3]; # (slotdef-initer slot)
          if (atomp(init))
            goto slot_done;
          # Slot in slot-names suchen:
          {
            var object slotnames = Before(rest_args_pointer);
            if (eq(slotnames,T))
              goto eval_init;
            var object slotname = TheSvector(slot)->data[0]; # (slotdef-name slot)
            if (obj_in_list(slotname,slotnames))
              goto eval_init;
            goto slot_done;
          }
         eval_init:
          # Die Initform auswerten:
          if (!nullp(Car(init))) {
            pushSTACK(slots); pushSTACK(slot);
            funcall(Car(init),0);
            slot = popSTACK(); slots = popSTACK();
          } else {
            value1 = Cdr(init);
          }
        }
       fill_slot:
        # Slot mit value1 initialisieren:
        {
          var object slotinfo = TheSvector(slot)->data[2]; # (slotdef-location slot)
          *ptr_to_slot(Before(rest_args_pointer STACKop 1),slotinfo) = value1;
        }
       slot_done: ;
      }
    }
    value1 = Before(rest_args_pointer STACKop 1); mv_count=1; # Instanz als Wert
    set_args_end_pointer(rest_args_pointer STACKop 2); # STACK aufräumen
  }

LISPFUN(reinitialize_instance,1,0,rest,nokey,0,NIL)
# (CLOS::%REINITIALIZE-INSTANCE instance &rest initargs)
# instance ist eine Instanz von <standard-object> oder <structure-object>,
# initargs eine paarige Liste.
# Das ist die primäre Methode von CLOS:REINITIALIZE-INSTANCE.
# vgl. clos.lisp
# (defmethod reinitialize-instance ((instance standard-object) &rest initargs)
#   (let ((h (gethash (class-of instance) *reinitialize-instance-table*)))
#     (if h
#       (progn
#         ; 28.1.9.2. validity of initialization arguments
#         (let ((valid-keywords (car h)))
#           (sys::keyword-test initargs valid-keywords)
#         )
#         (if (not (eq (cdr h) #'clos::%shared-initialize))
#           ; effektive Methode von shared-initialize anwenden:
#           (apply (cdr h) instance 'NIL initargs)
#           ; clos::%shared-initialize mit slot-names=NIL lässt sich vereinfachen:
#           (progn
#             (dolist (slot (class-slots (class-of instance)))
#               (let ((slotname (slotdef-name slot)))
#                 (multiple-value-bind (init-key init-value foundp)
#                     (get-properties initargs (slotdef-initargs slot))
#                   (declare (ignore init-key))
#                   (if foundp
#                     (setf (slot-value instance slotname) init-value)
#             ) ) ) )
#             instance
#       ) ) )
#       (apply #'initial-reinitialize-instance instance initargs)
# ) ) )
  {
    var object instance = Before(rest_args_pointer);
    var object clas = class_of(instance); # Instanz der <standard-class> oder <structure-class>
    # (GETHASH class *REINITIALIZE-INSTANCE-TABLE*) suchen:
    {
      var object info = gethash(clas,Symbol_value(S(reinitialize_instance_table)));
      if (eq(info,nullobj)) {
        # Hash-Tabellen-Eintrag neu berechnen. Siehe clos.lisp.
        funcall(S(initial_reinitialize_instance),argcount+1); return;
      }
      # Keywords überprüfen:
      if (!((argcount%2) == 0)) {
        var object arglist = listof(argcount);
        pushSTACK(arglist);
        fehler(program_error,
               GETTEXT("REINITIALIZE-INSTANCE: keyword argument list ~ has an odd length")
              );
      }
      argcount = argcount/2; # Anzahl der Initarg/Wert-Paare
      keyword_test(S(reinitialize_instance),rest_args_pointer,argcount,Car(info));
      # Stackaufbau: instance, slot-names, argcount Initarg/Wert-Paare.
      var object fun = Cdr(info);
      if (!eq(fun,L(shared_initialize))) {
        # initargs im Stack um 1 nach unten schieben, dann fun aufrufen:
        var object last = NIL;
        if (argcount > 0) {
          var object* ptr = rest_args_pointer;
          var uintC count;
          dotimespC(count,argcount, {
            var object next = Next(ptr); NEXT(ptr) = last;
            last = Next(ptr); NEXT(ptr) = next;
          });
        }
        pushSTACK(last);
        funcall(fun,2*argcount+2);
        return;
      }
    }
    # CLOS::%SHARED-INITIALIZE mit slot-names=NIL lässt sich vereinfachen:
    {
      var object slots = TheClass(clas)->slots; # Liste aller Slots (als Slot-Definitionen)
      while (consp(slots)) {
        var object slot = Car(slots);
        slots = Cdr(slots);
        # Suche ob der Slot durch die Initargs initialisiert wird:
        if (argcount > 0) {
          var object* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
          if (NULL != ptr) {
            var object value = NEXT(ptr);
            # Slot mit value initialisieren:
            var object slotinfo = TheSvector(slot)->data[2]; # (slotdef-location slot)
            *ptr_to_slot(Before(rest_args_pointer),slotinfo) = value;
          }
        }
      }
    }
    value1 = Before(rest_args_pointer); mv_count=1; # Instanz als Wert
    set_args_end_pointer(rest_args_pointer STACKop 1); # STACK aufräumen
  }

# (CLOS::%INITIALIZE-INSTANCE instance &rest initargs)
# instance ist eine Instanz von <standard-object> oder <structure-object>,
# initargs eine paarige Liste.
# Das ist die primäre Methode von CLOS:INITIALIZE-INSTANCE.
# vgl. clos.lisp
# (defmethod initialize-instance ((instance standard-object) &rest initargs)
#   (let ((h (gethash class *make-instance-table*)))
#     (if h
#       (if (not (eq (svref h 3) #'clos::%shared-initialize))
#         ; effektive Methode von shared-initialize anwenden:
#         (apply (svref h 3) instance 'T initargs)
#         ; clos::%shared-initialize mit slot-names=T lässt sich vereinfachen:
#         (progn
#           (dolist (slot (class-slots (class-of instance)))
#             (let ((slotname (slotdef-name slot)))
#               (multiple-value-bind (init-key init-value foundp)
#                   (get-properties initargs (slotdef-initargs slot))
#                 (declare (ignore init-key))
#                 (if foundp
#                   (setf (slot-value instance slotname) init-value)
#                   (unless (slot-boundp instance slotname)
#                     (let ((init (slotdef-initer slot)))
#                       (when init
#                         (setf (slot-value instance slotname)
#                               (if (car init) (funcall (car init)) (cdr init))
#           ) ) ) ) ) ) ) )
#           instance
#       ) )
#       (apply #'initial-initialize-instance instance initargs)
# ) ) )
local Values do_initialize_instance (object info, object* rest_args_pointer, uintC argcount);
LISPFUN(initialize_instance,1,0,rest,nokey,0,NIL)
  {
    var object instance = Before(rest_args_pointer);
    var object clas = class_of(instance); # Instanz der <standard-class> oder <structure-class>
    # (GETHASH class *MAKE-INSTANCE-TABLE*) suchen:
    {
      var object info = gethash(clas,Symbol_value(S(make_instance_table)));
      if (eq(info,nullobj)) {
        # Hash-Tabellen-Eintrag neu berechnen. Siehe clos.lisp.
        funcall(S(initial_initialize_instance),argcount+1); return;
      }
      if (!((argcount%2) == 0)) {
        var object arglist = listof(argcount);
        pushSTACK(arglist);
        fehler(program_error,
               GETTEXT("INITIALIZE-INSTANCE: keyword argument list ~ has an odd length")
              );
      }
      argcount = argcount/2; # Anzahl der Initarg/Wert-Paare
      return_Values do_initialize_instance(info,rest_args_pointer,argcount);
    }
  }
local Values do_initialize_instance(info,rest_args_pointer,argcount)
  var object info;
  var object* rest_args_pointer;
  var uintC argcount;
  {
    # Stackaufbau: instance, argcount Initarg/Wert-Paare.
    {
      var object fun = TheSvector(info)->data[3];
      if (!eq(fun,L(shared_initialize))) {
        # initargs im Stack um 1 nach unten schieben, dann fun aufrufen:
        var object last = T;
        if (argcount > 0) {
          var object* ptr = rest_args_pointer;
          var uintC count;
          dotimespC(count,argcount, {
            var object next = Next(ptr); NEXT(ptr) = last;
            last = Next(ptr); NEXT(ptr) = next;
          });
        }
        pushSTACK(last);
        funcall(fun,2*argcount+2);
        return;
      }
    }
    # CLOS::%SHARED-INITIALIZE mit slot-names=T lässt sich vereinfachen:
    {
      var object instance = Before(rest_args_pointer);
      var object clas = class_of(instance); # Instanz der <standard-class> oder <structure-class>
      var object slots = TheClass(clas)->slots; # Liste aller Slots (als Slot-Definitionen)
      while (consp(slots)) {
        var object slot = Car(slots);
        slots = Cdr(slots);
        # Suche ob der Slot durch die Initargs initialisiert wird:
        if (argcount > 0) {
          var object* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
          if (NULL == ptr)
            goto initarg_not_found;
          value1 = NEXT(ptr);
          goto fill_slot;
        }
       initarg_not_found:
        # Nicht gefunden -> erst auf (slot-boundp instance slotname) testen:
        {
          var object slotinfo = TheSvector(slot)->data[2]; # (slotdef-location slot)
          if (!eq(*ptr_to_slot(Before(rest_args_pointer),slotinfo),unbound))
            goto slot_done;
        }
        # Slot hat noch keinen Wert. Die Initform auswerten:
        {
          var object init = TheSvector(slot)->data[3]; # (slotdef-initer slot)
          if (atomp(init))
            goto slot_done;
          if (!nullp(Car(init))) {
            pushSTACK(slots); pushSTACK(slot);
            funcall(Car(init),0);
            slot = popSTACK(); slots = popSTACK();
          } else {
            value1 = Cdr(init);
          }
        }
       fill_slot:
        # Slot mit value1 initialisieren:
        {
          var object slotinfo = TheSvector(slot)->data[2]; # (slotdef-location slot)
          *ptr_to_slot(Before(rest_args_pointer),slotinfo) = value1;
        }
       slot_done: ;
      }
    }
    value1 = Before(rest_args_pointer); mv_count=1; # Instanz als Wert
    set_args_end_pointer(rest_args_pointer STACKop 1); # STACK aufräumen
  }

#ifdef RISCOS_CCBUG
  #pragma -z0
#endif
LISPFUN(make_instance,1,0,rest,nokey,0,NIL)
# (CLOS::%MAKE-INSTANCE class &rest initargs)
# class ist eine Instanz der <standard-class> oder <structure-class>,
# initargs eine (hoffentlich paarige) Liste.
# vgl. clos.lisp
# (defun %make-instance (class &rest initargs)
#   ; 28.1.9.3., 28.1.9.4. default-initargs zur Kenntnis nehmen:
#   (dolist (default-initarg (class-default-initargs class))
#     (let ((nothing default-initarg))
#       (when (eq (getf initargs (car default-initarg) nothing) nothing)
#         (setq initargs
#               (append initargs
#                 (list (car default-initarg)
#                       (let ((init (cdr default-initarg)))
#                         (if (car init) (funcall (car init)) (cdr init))
#   ) ) ) )     ) )     )
#   (let ((h (gethash class *make-instance-table*)))
#     (if h
#       (progn
#         ; 28.1.9.2. validity of initialization arguments
#         (let ((valid-keywords (svref h 0)))
#           (sys::keyword-test initargs valid-keywords)
#         )
#         (let ((instance (apply #'allocate-instance class initargs)))
#           (if (not (eq (svref h 2) #'clos::%initialize-instance))
#             ; effektive Methode von initialize-instance anwenden:
#             (apply (svref h 2) instance initargs)
#             ; clos::%initialize-instance lässt sich vereinfachen (man braucht
#             ; nicht nochmal in *make-instance-table* nachzusehen):
#             (if (not (eq (svref h 3) #'clos::%shared-initialize))
#               ; effektive Methode von shared-initialize anwenden:
#               (apply (svref h 3) instance 'T initargs)
#               ...
#             )
#       ) ) )
#       (apply #'initial-make-instance class initargs)
# ) ) )
  {
    if (!((argcount%2) == 0)) {
      var object arglist = listof(argcount);
      pushSTACK(arglist);
      fehler(program_error,
             GETTEXT("MAKE-INSTANCE: keyword argument list ~ has an odd length"));
    }
    argcount = argcount/2; # Anzahl der Initarg/Wert-Paare
    # Stackaufbau: class, argcount Initarg/Wert-Paare.
    # Default-Initargs anfügen:
    {
      var object clas = Before(rest_args_pointer);
      var object l = TheClass(clas)->default_initargs;
      while (consp(l)) {
        var object default_initarg = Car(l);
        l = Cdr(l);
        var object key = Car(default_initarg);
        # Suche key unter den bisherigen Initargs:
        if (argcount > 0) {
          var object* ptr = rest_args_pointer;
          var uintC count;
          dotimespC(count,argcount, {
            if (eq(NEXT(ptr),key))
              goto key_found;
            NEXT(ptr);
          });
        }
        # Nicht gefunden
        pushSTACK(key); # Initarg in den Stack
        {
          var object init = Cdr(default_initarg);
          if (!nullp(Car(init))) {
            pushSTACK(l);
            funcall(Car(init),0); # Default auswerten
            l = STACK_0;
            STACK_0 = value1; # Wert in den Stack
          } else {
            pushSTACK(Cdr(init)); # Default in den Stack
          }
        }
        argcount++;
       key_found: ;
      }
    }
    # (GETHASH class *MAKE-INSTANCE-TABLE*) suchen:
    {
      var object clas = Before(rest_args_pointer);
      var object info = gethash(clas,Symbol_value(S(make_instance_table)));
      if (eq(info,nullobj)) {
        # Hash-Tabellen-Eintrag neu berechnen. Siehe clos.lisp.
        return_Values funcall(S(initial_make_instance),2*argcount+1);
      } else {
        # check keywords:
        keyword_test(S(make_instance),rest_args_pointer,
                     argcount,TheSvector(info)->data[0]);
        # call the effective method of ALLOCATE-INSTANCE:
        pushSTACK(info);
        {
          var object fun = TheSvector(info)->data[1];
          if (!eq(fun,L(pallocate_instance))) {
            var object* ptr = rest_args_pointer STACKop 1;
            var uintC count;
            dotimespC(count,2*argcount+1, { pushSTACK(NEXT(ptr)); });
            funcall(fun,2*argcount+1);
            pushSTACK(value1); # save instance
            pushSTACK(value1); C_class_of();
            if (!eq(value1,Before(rest_args_pointer))) {
              # instance already in STACK_0
              pushSTACK(Before(rest_args_pointer));
              pushSTACK(S(allocate_instance));
              fehler(error,GETTEXT("~ method for ~ returned ~"));
            }
            value1 = popSTACK(); # restore instance
          } else {
            do_allocate_instance(clas);
          }
        }
        info = popSTACK();
        # call the effective method of INITIALIZE-INSTANCE:
        # instance as the 1st argument instead of class:
        Before(rest_args_pointer) = value1;
        var object fun = TheSvector(info)->data[2];
        # save the instance in case INITIALIZE-INSTANCE returns junk
        # see 7.1.7 "Definitions of Make-Instance and Initialize-Instance"
        # http://www.lisp.org/HyperSpec/Body/sec_7-1-7.html
        pushSTACK(value1);
        if (argcount>0) { # (rotatef STACK_0 ... STACK_(2*argcount))
          var uintC count;
          var object* ptr = &(STACK_0);
          dotimespC(count,2*argcount,
            { *ptr = *(ptr STACKop 1); ptr skipSTACKop 1; });
          *ptr = value1;
        }
        rest_args_pointer skipSTACKop -1;
        if (eq(fun,L(initialize_instance)))
          # CLOS::%INITIALIZE-INSTANCE can be simplified
          # (do not have to look into *make-instance-table* again):
          do_initialize_instance(info,rest_args_pointer,argcount);
        else
          funcall(fun,2*argcount+1);
        value1 = popSTACK(); mv_count = 1;
      }
    }
  }
#ifdef RISCOS_CCBUG
  #pragma -z1
#endif

# ===========================================================================

