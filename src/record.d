/*
 * Functions for records and structures in CLISP
 * Bruno Haible 1990-2004
 * Sam Steingold 1998-2004
 * German comments translated into English: Stefan Kain 2002-04-16
 */
#include "lispbibl.c"

/* ===========================================================================
 * general records:

 (SYS::%RECORD-REF record index) return the index'th entry in the record.
 (SYS::%RECORD-STORE record index value) store value as the index'th
   entry in the record and return value.
 (SYS::%RECORD-LENGTH record) return the length of the record.
*/
/* Error message
 > STACK_1: record
 > STACK_0: (bad) index
 > limit: exclusive upper bound on the index */
nonreturning_function(local, fehler_index, (uintL limit)) {
  pushSTACK(STACK_0); /* TYPE-ERROR slot DATUM */
  {
    var object tmp;
    pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(limit));
    tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
    pushSTACK(tmp); /* TYPE-ERROR slot EXPECTED-TYPE */
  }
  pushSTACK(STACK_(1+2)); /* record */
  pushSTACK(STACK_(0+3)); /* index */
  pushSTACK(TheSubr(subr_self)->name); /* function name */
  fehler(type_error,GETTEXT("~S: ~S is not a valid index into ~S"));
}

/* Error message
 > STACK_0: (bad) record */
nonreturning_function(local, fehler_record, (void)) {
  pushSTACK(TheSubr(subr_self)->name); /* function name */
  fehler(error, /* type_error ?? */
         GETTEXT("~S: ~S is not a record"));
}

/* Subroutine for record access functions
 > STACK_1: record argument
 > STACK_0: index argument
 < STACK: cleared up
 < returns: the address of the referred record item */
local gcv_object_t* record_up (void) {
  /* the record must be a Closure/Structure/Stream/OtherRecord: */
  if_recordp(STACK_1, ; , { skipSTACK(1); fehler_record(); } );
  var object record = STACK_1;
  var uintL length = Record_length(record);
  var uintL index;
  if (!(posfixnump(STACK_0) && ((index = posfixnum_to_L(STACK_0)) < length)))
    /* extract and check index */
    fehler_index(length);
  skipSTACK(2); /* clear up stack */
  return &TheRecord(record)->recdata[index]; /* record element address */
}

LISPFUNNR(record_ref,2)
{ /* (SYS::%RECORD-REF record index) return the index'th entry in the record */
  VALUES1(*(record_up())); /* record element as value */
}

/* (SYS::%RECORD-STORE record index value) store value as the index'th
   entry in the record and return value. */
LISPFUNN(record_store,3) {
  var object value = popSTACK();
  VALUES1(*(record_up()) = value); /* set record element */
}

LISPFUNNR(record_length,1)
{ /* (SYS::%RECORD-LENGTH record) return the length of the record. */
  /* the record must be a Closure/Structure/Stream/OtherRecord: */
  if_recordp(STACK_0, ; , { fehler_record(); } );
  var object record = popSTACK();
  var uintL length = Record_length(record);
  VALUES1(fixnum(length)); /* length as Fixnum */
}

/* check that the length is of type (INTEGER (0) (65536))
 > STACK_0: length
 < uintL length: checked length */
#define test_record_length(length)                                           \
  if (!(posfixnump(STACK_0)                                                  \
        && ((length = posfixnum_to_L(STACK_0)) <= (uintL)(bitm(intWsize)-1)) \
        && (length>0)))                                                      \
    fehler_record_length()
nonreturning_function(local, fehler_record_length, (void)) {
  /* STACK_0 = length, TYPE-ERROR slot DATUM */
  pushSTACK(O(type_posint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(O(type_posint16)); /* type */
  pushSTACK(STACK_2); /* length */
  pushSTACK(TheSubr(subr_self)->name); /* function name */
  fehler(type_error,GETTEXT("~S: length ~S should be of type ~S"));
}

/* ===========================================================================
 * Structures:

 (SYS::%STRUCTURE-REF type structure index) returns for a structure of
   given Type type (a Symbol) the entry at index>=1.
 (SYS::%STRUCTURE-STORE type structure index object) stores object as
   Entry index in a structure of given Type type and returns object.
 (SYS::%MAKE-STRUCTURE type length) creates a structure with length>=1
   elements of Type type.
 (COPY-STRUCTURE structure) returns a copy of the Structure structure,
   of the same type.
 (SYS::%STRUCTURE-TYPE-P type object) checks if object is a
   structure that has the Type type, which can be recognized in
   component 0. There, an object (name_1 ... name_i-1 name_i) should
   be located with one of the names EQ to type.
*/
/* subroutine for structure-access-functions:
 > STACK_2: type-argument
 > STACK_1: structure-argument
 > STACK_0: index-argument
 < result: Address of the structure-element */
local gcv_object_t* structure_up (void) {
  /* structure must be of Type structure: */
  if (!structurep(STACK_1)) {
   fehler_bad_structure: /* STACK_2 = type, STACK_1 = structure */
    pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
    pushSTACK(STACK_(2+1)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(STACK_(2+2));
    pushSTACK(STACK_(1+3));
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    fehler(type_error,GETTEXT("~S: ~S is not a structure of type ~S"));
  }
  var object structure = STACK_1;
  /* check if type occurs in namelist = (name_1 ... name_i-1 name_i) : */
  if (!nullp(memq(STACK_2,TheStructure(structure)->structure_types)))
    goto yes;
  /* type did not occur -> Error: */
  goto fehler_bad_structure;
 yes: { /* type did occur: */
    var uintL length = (uintL)Structure_length(structure);
    var uintL index;
    /* fetch index and check */
    if (!(posfixnump(STACK_0) && ((index = posfixnum_to_L(STACK_0)) < length)))
      fehler_index(length);
    /* address of the structure-component */
    return &TheStructure(structure)->recdata[index];
  }
}

/* (SYS::%%STRUCTURE-REF type structure index) returns for a structure of
   the given Type type (a symbol) the entry index>=1.
 #<UNBOUND> is possible. */
LISPFUNNR(pstructure_ref,3) {
  VALUES1(*(structure_up())); /* structure-element as value */
  skipSTACK(3); /* clean up stack */
}

/* (SYS::%STRUCTURE-REF type structure index) returns for a structure of
   the given Type type (a symbol) the entry index>=1. */
LISPFUNNR(structure_ref,3) {
  VALUES1(*(structure_up())); /* structure-element as value */
  if (!boundp(value1)) {
    /* could be = #<UNBOUND> , after use of SLOT-MAKUNBOUND
       or after incomplete INITIALIZE-INSTANCE */
    dynamic_bind(S(print_length),Fixnum_0); /* bind *PRINT-LENGTH* to 0 */
    pushSTACK(STACK_(1+3)); /* UNBOUND-SLOT slot INSTANCE */
    /* (clos::slotdef-name (find index (clos::class-slots (find-class type))
                           :key #'clos::slotdef-location)) */
    pushSTACK(STACK_(2+3+1)); funcall(L(find_class),1);
    pushSTACK(value1); funcall(S(class_slots),1);
    pushSTACK(STACK_(0+3+1)); pushSTACK(value1); pushSTACK(S(Kkey));
    pushSTACK(Symbol_function(S(slotdef_location))); funcall(L(find),4);
    pushSTACK(value1); funcall(S(slotdef_name),1);
    pushSTACK(value1); /* UNBOUND-SLOT slot NAME */
    pushSTACK(STACK_(1+3+2));
    pushSTACK(value1);
    pushSTACK(S(structure_ref));
    fehler(unbound_slot,GETTEXT("~S: Slot ~S of ~S has no value"));
  }
  skipSTACK(3); /* clean up stack */
}

/* (SYS::%STRUCTURE-STORE type structure index object) stores object as
   entry index in a structure of given Type type and returns object. */
LISPFUNN(structure_store,4) {
  var object value = popSTACK();
  VALUES1(*(structure_up()) = value); /* enter structure-element */
  skipSTACK(3); /* clean up stack */
}

/* (SYS::%MAKE-STRUCTURE type length) creates a structure with length>=1
   elements of Type type. */
LISPFUNNR(make_structure,2) {
  /* check length, should be a fixnum /=0  that fits into a uintW: */
  var uintL length;
  test_record_length(length);
  skipSTACK(1);
  var object structure = allocate_structure(length);
  /* new structure, filled with NILs */
  TheStructure(structure)->structure_types = popSTACK(); /* type component */
  VALUES1(structure); /* structure as value */
}

/* ensure that OBJ is a structure object and return it
 can trigger GC */
global object check_structure (object obj) {
  while (!structurep(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(structure_object)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(structure_object)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  }
  return obj;
}

/* (COPY-STRUCTURE structure) returns a copy of the Structure structure
   of the same type. */
LISPFUNNR(copy_structure,1) {
  STACK_0 = check_structure(STACK_0);
  var uintC length = Structure_length(STACK_0);
  var object new_structure = allocate_structure(length);
  copy_mem_o(&TheStructure(new_structure)->structure_types,
             &TheStructure(popSTACK())->structure_types,length);
  VALUES1(new_structure);
}

/* (SYS::%STRUCTURE-TYPE-P type object) checks if object is a
   structure that has the Type type, which can be recognized in
   component 0. There, an object (name_1 ... name_i-1 name_i) should
   be located with one of the names EQ to type. */
LISPFUNNR(structure_type_p,2) {
  /* check object for structure: */
  if (!structurep(STACK_0)) { skipSTACK(2); goto no; }
  {
    var object namelist = TheStructure(popSTACK())->structure_types;
    var object type = popSTACK();
    /* test, if type occurs in namelist = (name_1 ... name_i-1 name_i) : */
    if (!nullp(memq(type,namelist)))
      goto yes;
  }
 no: /* type did not occur: */
  VALUES1(NIL); return; /* 1 value NIL */
 yes: /* type did occur: */
  VALUES1(T); return;
}

/* ===========================================================================
 * Closures:

 (SYS::CLOSURE-NAME closure) returns the name of a closure.
 (SYS::CLOSURE-CODEVEC closure) returns the code-vector of a compiled
   closure as a list of fixnums >=0, <256.
 (SYS::CLOSURE-CONSTS closure) returns a list of all constants of a
   compiled closure.
 (SYS::MAKE-CODE-VECTOR list) returns for a list of fixnums >=0, <256
   a simple-bit-vector of eight fold length, that contains these numbers
    as bytes.
 (SYS::%MAKE-CLOSURE name codevec consts) returns a closure with given
   name (a symbol), given code-vector (a simple-bit-vector) and
   further given constants.
 (SYS::%COPY-GENERIC-FUNCTION venv closure) copies the closure, which must be
   a generic function with venv slot, copying in the given venv.
 (SYS::GENERIC-FUNCTION-EFFECTIVE-METHOD-FUNCTION generic-function)
   returns a function, which delivers the effective methods, so that
   (APPLY generic-function arguments)
   == (APPLY (APPLY ergebnis arguments) arguments) .
*/
/* (SYS::CLOSURE-NAME closure) returns the name of a closure. */
LISPFUNNR(closure_name,1) {
  var object closure = popSTACK();
  if (!closurep(closure)) {
    pushSTACK(closure);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    fehler(error, /* type_error ?? */
           GETTEXT("~S: ~S is not a closure"));
  }
  VALUES1(TheClosure(closure)->clos_name);
}

/* error, if argument is not a compiled closure */
nonreturning_function(local, fehler_cclosure, (object obj)) {
  pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name); /* function name */
  fehler(error, /* type_error ?? */
         GETTEXT("~S: This is not a compiled closure: ~S"));
}

/* (SYS::CLOSURE-CODEVEC closure) returns the code-vector of a compiled
   closure, as list of fixnums >=0, <256. */
LISPFUNNR(closure_codevec,1) {
  var object closure = popSTACK();
  if (!(cclosurep(closure))) fehler_cclosure(closure);
  var object codevec = TheCclosure(closure)->clos_codevec;
  var uintL index = Sbvector_length(codevec); /* index := length in bytes */
  /* step through codevector codevec from behind and push bytes onto a list: */
  pushSTACK(codevec); /* codevector */
  pushSTACK(NIL); /* list := () */
  while (index != 0) {
    index--; /* decrement index */
    /* put new cons in front of the list: */
    var object new_cons = allocate_cons();
    Cdr(new_cons) = popSTACK();
    Car(new_cons) = fixnum((uintL)(TheSbvector(STACK_0)->data[index])); /* fetch byte */
    pushSTACK(new_cons);
  }
  VALUES1(STACK_0); skipSTACK(2); /* list as value */
}

/* (SYS::CLOSURE-CONSTS closure) returns a list of all constants of a
   compiled closure. */
LISPFUNNR(closure_consts,1) {
  var object closure = popSTACK();
  if (!(cclosurep(closure))) fehler_cclosure(closure);
  /* comprise elements 2,3,... to a list: */
  var uintC index = Cclosure_length(closure)-2; /* index := length */
  /* step through closure from behind and push constants onto a list: */
  pushSTACK(closure); /* closure */
  pushSTACK(NIL); /* list := () */
  while (index != 0) {
    index--; /* decrement index */
    /* put new cons in front of the list: */
    var object new_cons = allocate_cons();
    Cdr(new_cons) = popSTACK();
    Car(new_cons) = TheCclosure(STACK_0)->clos_consts[(uintP)index]; /* fetch constant */
    pushSTACK(new_cons);
  }
  VALUES1(STACK_0); skipSTACK(2); /* list as value */
}

/* (SYS::MAKE-CODE-VECTOR list) returns for a list of fixnums >=0, <256
   a simple-bit-vector of eight fold length, that contains these
   numbers as bytes. */
LISPFUNNR(make_code_vector,1) {
  var object bv = allocate_bit_vector(Atype_8Bit,llength(STACK_0)); /* simple-8bit-vector */
  /* fill: */
  var object listr = popSTACK(); /* list */
  var uintB* ptr = &TheSbvector(bv)->data[0]; /* loop through the bit-vector */
  while (consp(listr)) {
    var uintL byte;
    /* list element must be a fixnum >=0, <256 : */
    if (!(posfixnump(Car(listr))
          && ((byte = posfixnum_to_L(Car(listr))) < (1<<intBsize))))
      goto bad_byte;
    /* put into the bit-vector: */
    *ptr++ = (uintB)byte;
    listr = Cdr(listr);
  }
  VALUES1(bv); return;
 bad_byte:
  pushSTACK(Car(listr)); /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_uint8)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(STACK_1);
  fehler(type_error,GETTEXT("~S is not a valid code-vector byte"));
}

/* parse the seclass object (NIL or SECLASS, see compiler.lisp)
   into a seclass_t */
local seclass_t parse_seclass (object sec, object closure)
{
  if (nullp(sec)) return seclass_foldable;
  if (!consp(sec) || !consp(Cdr(sec)) || !consp(Cdr(Cdr(sec)))) {
    pushSTACK(closure); pushSTACK(sec);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: invalid side-effect class ~S for function ~S"));
  }
  var object modifies = Car(Cdr(sec));
  return (nullp(Car(sec))
          ? (nullp(modifies) ? seclass_no_se : seclass_write)
          : (nullp(modifies) ? seclass_read : seclass_default));
}

/* (SYS::%MAKE-CLOSURE name codevec consts seclass) returns a closure
   with given name (a symbol), given code-vector (a simple-bit-vector),
   given constants, and given side-effect class. */
LISPFUNNR(make_closure,4) {
  var seclass_t seclass = parse_seclass(STACK_0,STACK_3); skipSTACK(1);
  /* codevec must be a simple-bit-vector: */
  if (!simple_bit_vector_p(Atype_8Bit,STACK_1)) {
    /* STACK_1 = codevec */
    pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(simple_bit_vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(STACK_(1+2));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: invalid code-vector ~S"));
  }
  /* create a new closure of length (+ 2 (length consts)) : */
  var uintL length = 2+llength(STACK_0);
  if (!(length <= (uintL)(bitm(intWsize)-1))) { /* should fit into a uintW */
    /* STACK_0 = consts */
    pushSTACK(STACK_2); /* name */
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: function ~S is too big: ~S"));
  }
  var object closure = allocate_closure(length,seclass);
  TheCclosure(closure)->clos_name = STACK_2; /* fill name */
  TheCclosure(closure)->clos_codevec = STACK_1; /* fill codevector */
  { /* fill constants: */
    var object constsr = popSTACK();
    var gcv_object_t* ptr = &TheCclosure(closure)->clos_consts[0];
    while (consp(constsr)) {
      *ptr++ = Car(constsr); constsr = Cdr(constsr);
    }
  }
  VALUES1(closure); skipSTACK(2);
}

LISPFUNN(closure_set_seclass,2)
{ /* (CLOSURE-SET-SECLASS closure new-seclass)
 - for adding methods to generic functions; return the old seclass */
  var object closure = STACK_1;
  if (!cclosurep(closure)) fehler_cclosure(closure);
  var seclass_t new_seclass = parse_seclass(STACK_0,closure);
  VALUES1(seclass_object((seclass_t)Cclosure_seclass(closure)));
  Cclosure_set_seclass(closure,new_seclass);
  skipSTACK(2);
}

/* ensure that OBJ is a generic function and return it
 can trigger GC */
local object check_generic_function (object obj) {
  while (!genericfunctionp(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(standard_generic_function)); /* slot EXPECTED-TYPE */
    pushSTACK(S(standard_generic_function)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  }
  return obj;
}

/* (SYS::%COPY-GENERIC-FUNCTION venv closure) copies the closure, which must be
   a generic function with venv slot, copying in the given venv. */
LISPFUNN(copy_generic_function,2) {
  /* Note: closure's clos_consts[0] is a simple-vector #(NIL c1 ... cn) where
     c1,...,cn are constant objects, and NIL is the placeholder to be replaced
     with the passed venv. */
  var object oldclos = check_generic_function(STACK_0);
  var object vector = TheCclosure(oldclos)->clos_consts[0];
  if (!(simple_vector_p(vector)
        && (Svector_length(vector) > 0)
        && nullp(TheSvector(vector)->data[0]))) {
    pushSTACK(oldclos);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    fehler(error,
           GETTEXT("~S: This is not a prototype of a generic function: ~S"));
  }
  vector = copy_svector(vector); /* copy the vector */
  TheSvector(vector)->data[0] = STACK_1; /* put in venv */
  STACK_1 = vector;
  /* Copy the function: */
  var object newclos = allocate_cclosure_copy(STACK_0);
  oldclos = STACK_0;
  do_cclosure_copy(newclos,oldclos);
  /* Put in the copied vector with venv: */
  TheCclosure(newclos)->clos_consts[0] = STACK_1;
  VALUES1(newclos);
  skipSTACK(2);
}

/* (SYS::GENERIC-FUNCTION-EFFECTIVE-METHOD-FUNCTION generic-function)
   returns a function, which delivers the effective methods, so that
   (APPLY generic-function arguments)
   == (APPLY (APPLY ergebnis arguments) arguments) .
   is used for CALL-NEXT-METHOD; can assume that the
   generic-function has already been called, i.e. that the dispatch has
   already been installed. */
LISPFUNN(generic_function_effective_method_function,1) {
  var object oldclos = STACK_0 = check_generic_function(STACK_0);
  /* allocate closure of equal length: */
  var object newclos = allocate_cclosure_copy(oldclos);
  oldclos = STACK_0;
  do_cclosure_copy(newclos,oldclos);
  STACK_0 = newclos;
  /* copy the code-vector likewise: */
  var object newcodevec = copy_sbvector(TheClosure(newclos)->clos_codevec);
  /* set the bit therein which is queried by the RETGF-instruction: */
  TheCodevec(newcodevec)->ccv_flags |= bit(3);
  newclos = popSTACK();
  TheClosure(newclos)->clos_codevec = newcodevec;
  VALUES1(newclos);
}

/* ===========================================================================
 * load-time-eval:

 (SYS::MAKE-LOAD-TIME-EVAL form) returns a load-time-eval-object that
  - if printed and read again - evaluates form. */
LISPFUN(make_load_time_eval,seclass_no_se,1,0,norest,nokey,0,NIL) {
  var object lte = allocate_loadtimeeval();
  TheLoadtimeeval(lte)->loadtimeeval_form = popSTACK();
  VALUES1(lte);
}

/* ===========================================================================
 * symbol-macro:

 (SYS::MAKE-SYMBOL-MACRO expansion) returns a symbol-macro-object
   that represents the given expansion.
 (SYS::SYMBOL-MACRO-P object) tests for symbol-macro.

 Due to their special meaning in the interpreter, symbol-macro-objects
 - like #<UNBOUND> and #<SPECDECL> - are not first class objects.
 They can be passed only as values. They cannot be assigned to
 variables, however.

 (SYMBOL-MACRO-EXPAND symbol) tests if a symbol represents a symbol-macro
 and returns T and the expansion if true, NIL if false.
*/
/* (SYS::MAKE-SYMBOL-MACRO expansion) returns a symbol-macro-object,
   that represents the given expansion. */
LISPFUN(make_symbol_macro,seclass_no_se,1,0,norest,nokey,0,NIL) {
  var object sm = allocate_symbolmacro();
  TheSymbolmacro(sm)->symbolmacro_expansion = popSTACK();
  VALUES1(sm);
}

LISPFUNNF(symbol_macro_p,1)
{ /* (SYS::SYMBOL-MACRO-P object) tests for symbol-macro. */
  var object obj = popSTACK();
  VALUES_IF(symbolmacrop(obj));
}

/* (SYMBOL-MACRO-EXPAND symbol) tests if a symbol represents a symbol-macro
 and returns T and the expansion if true, NIL if false.
 (defun symbol-macro-expand (v)
   (unless (symbolp v) (error ...))
   (and (boundp v) (symbol-macro-p (%symbol-value v))
        (values t (sys::%record-ref (%symbol-value v) 0)))) */
LISPFUNN(symbol_macro_expand,1) {
  var object obj = check_symbol(popSTACK());
  obj = Symbol_value(obj);
  if (!symbolmacrop(obj))
    VALUES1(NIL);
  else
    VALUES2(T, TheSymbolmacro(obj)->symbolmacro_expansion);
}

/* ===========================================================================
 * Macro:

 (SYS::MAKE-MACRO expander) returns a Macro object with the given expander
 function.
 (SYS::MACROP object) tests for a Macro.
 (SYS::MACRO-EXPANDER macro) returns the macro's expander function. */

/* (SYS::MAKE-MACRO expander) returns a Macro object with the given expander
 function. */
LISPFUNN(make_macro,1) {
  STACK_0 = check_function(STACK_0);
  var object m = allocate_macro();
  TheMacro(m)->macro_expander = popSTACK();
  VALUES1(m);
}

/* (SYS::MACROP object) tests for a Macro. */
LISPFUNN(macrop,1) {
  var object obj = popSTACK();
  VALUES_IF(macrop(obj));
}

/* (SYS::MACRO-EXPANDER macro) returns the macro's expander function. */
LISPFUNN(macro_expander,1) {
  var object obj = popSTACK();
  while (!macrop(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(macro)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(macro)); pushSTACK(obj);
    pushSTACK(S(macro_expander)); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  }
  VALUES1(TheMacro(obj)->macro_expander);
}

/* ===========================================================================
 * FunctionMacro:

 (SYS::MAKE-FUNCTION-MACRO function expander) returns a FunctionMacro object
 for the given function and with the given expander function.
 (SYS::FUNCTION-MACRO-P object) tests for a FunctionMacro.
 (SYS::FUNCTION-MACRO-FUNCTION macro) returns the functionmacro's function.
 (SYS::FUNCTION-MACRO-EXPANDER macro) returns the functionmacro's expander. */

/* (SYS::MAKE-FUNCTION-MACRO function expander) returns a FunctionMacro object
 for the given function and with the given expander function. */
LISPFUNN(make_function_macro,2) {
  STACK_0 = check_function(STACK_0);
  STACK_1 = check_function(STACK_1);
  var object m = allocate_functionmacro();
  TheFunctionMacro(m)->functionmacro_macro_expander = popSTACK();
  TheFunctionMacro(m)->functionmacro_function = popSTACK();
  VALUES1(m);
}

/* (SYS::FUNCTION-MACRO-P object) tests for a FunctionMacro. */
LISPFUNN(function_macro_p,1) {
  var object obj = popSTACK();
  VALUES_IF(functionmacrop(obj));
}

/* ensure that the OBJ is a FUNCTION-MACRO and return it
 can trigger GC */
local object check_function_macro (object obj) {
  while (!functionmacrop(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(function_macro)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(function_macro)); pushSTACK(obj);
    pushSTACK(S(function_macro_expander)); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  }
  return obj;
}

/* (SYS::FUNCTION-MACRO-FUNCTION macro)
   returns the FunctionMacro's function. */
LISPFUNN(function_macro_function,1) {
  var object obj = check_function_macro(popSTACK());
  VALUES1(TheFunctionMacro(obj)->functionmacro_function);
}

/* (SYS::FUNCTION-MACRO-EXPANDER macro)
   returns the FunctionMacro's expander. */
LISPFUNN(function_macro_expander,1) {
  var object obj = check_function_macro(popSTACK());
  VALUES1(TheFunctionMacro(obj)->functionmacro_macro_expander);
}

/* ===========================================================================
 * Weak-Pointer:
 We keep all WEAK-POINTER objects on the O(all_weakpointers) list unless the
 value that the WEAK-POINTER points to is GC-invariant.  This requires that
 we add the WEAK-POINTER to O(all_weakpointers) when the value is changed
 to a non-GC-invariant one, and GC removes the WEAK-POINTERs with GC-invariant
 values from O(all_weakpointers).
 The alternative it to keep all the WEAK-POINTERs on the list.
 We do not do that because we assume that the lifetime of a WEAK-POINTER is
 relatively high compared to GC timeout, so there will be several GCs while
 the given WEAK-POINTER is alive (why would one use a WEAK-POINTER otherwise?)
 and therefore it is worth the effort to keep O(all_weakpointers) as short
 as possible. */

/* UP: make a weakpointer to popSTACK()
 can trigger GC, modifies STACK */
local object mk_weakpointer () {
  var object wp = allocate_xrecord(0,Rectype_Weakpointer,weakpointer_length,
                                   weakpointer_xlength,orecord_type);
  var object obj = popSTACK();
  TheWeakpointer(wp)->wp_value = obj;
  if (gcinvariant_object_p(obj)) {
    TheWeakpointer(wp)->wp_cdr = unbound; /* a GC-invariant dummy */
  } else {
    TheWeakpointer(wp)->wp_cdr = O(all_weakpointers);
    O(all_weakpointers) = wp;
  }
  return wp;
}

/* (MAKE-WEAK-POINTER value)
   returns a fresh weak pointer referring to value. */
LISPFUN(make_weak_pointer,seclass_no_se,1,0,norest,nokey,0,NIL) {
  VALUES1(mk_weakpointer());
}

/* (WEAK-POINTER-P object)
   returns true if the object is of type WEAK-POINTER. */
LISPFUNNF(weak_pointer_p,1) {
  var object obj = popSTACK();
  VALUES_IF(weakpointerp(obj));
}

/* UP: make sure that the argument is a WEAK-POINTER and return it
 can trigger GC */
local object check_weak_pointer (object wp) {
  while (!weakpointerp(wp)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(wp); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_pointer)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_pointer)); pushSTACK(wp);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    wp = value1;
  }
  return wp;
}

/* (WEAK-POINTER-VALUE weak-pointer) returns two values: The original value
 and T, if the value has not yet been garbage collected, else NIL and NIL. */
LISPFUNNR(weak_pointer_value,1) {
  var object wp = check_weak_pointer(popSTACK());
  if (weakpointer_broken_p(wp))
    VALUES2(NIL,NIL);
  else
    VALUES2(TheWeakpointer(wp)->wp_value, T);
}

LISPFUNN(set_weak_pointer_value,2)
{ /* (SETF (WEAK-POINTER-VALUE wp) value) */
  var object wp = check_weak_pointer(STACK_1);
  var object value = STACK_0; skipSTACK(2);
  if (!gcinvariant_object_p(value)) {
    /* make sure wp is on the O(all_weakpointers) list */
    if (!boundp(TheWeakpointer(wp)->wp_cdr)) { /* put wp on the list */
      TheWeakpointer(wp)->wp_cdr = O(all_weakpointers);
      O(all_weakpointers) = wp;
    }
  }
  VALUES1(TheWeakpointer(wp)->wp_value = value);
}

/* ===========================================================================
 * Finalizer: */

/* (FINALIZE object function &optional alive)
 records that function is called if object dies through GC, with
 object and poss. alive as argument. If alive dies before object dies,
 nothing will be done. */
LISPFUN(finalize,seclass_default,2,1,norest,nokey,0,NIL) {
  STACK_1 = coerce_function(STACK_1);
  if (!gcinvariant_object_p(STACK_2)) {
    var object f = allocate_finalizer();
    TheFinalizer(f)->fin_trigger = STACK_2;
    TheFinalizer(f)->fin_function = STACK_1;
    TheFinalizer(f)->fin_alive = STACK_0; /* The default #<UNBOUND> lives forever. */
    TheFinalizer(f)->fin_cdr = O(all_finalizers);
    O(all_finalizers) = f;
  }
  skipSTACK(3); VALUES1(NIL);
}

/* ===========================================================================
 * CLOS objects: */

LISPFUNNF(structure_object_p,1)
{ /* (CLOS::STRUCTURE-OBJECT-P object) checks if object is a structure. */
  var object obj = popSTACK();
  VALUES_IF(structurep(obj));
}

LISPFUNNF(std_instance_p,1)
{ /* (CLOS::STD-INSTANCE-P object) checks if object is a CLOS-object. */
  var object obj = popSTACK();
  VALUES_IF(instancep(obj));
}

/* returns (CLOS:CLASS-OF object). Especially efficient for CLOS objects. */
local inline object class_of (object obj) {
  if (instancep(obj)) {
    instance_un_realloc(obj);
    instance_update(obj);
    return (object)TheInstance(obj)->inst_class;
  } else {
    pushSTACK(obj); C_class_of(); return value1;
  }
}

/* error-message if an object is not a class.
 fehler_keine_klasse(caller,obj);
 > obj: non-class */
nonreturning_function(local, fehler_keine_klasse, (object obj)) {
  pushSTACK(obj); /* TYPE-ERROR slot DATUM */
  pushSTACK(S(class)); /* CLOS:CLASS, TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name); /* function name */
  fehler(type_error,GETTEXT("~S: ~S is not a class"));
}

/* (CLOS::ALLOCATE-STD-INSTANCE class n) returns a CLOS-instance of length n,
 with Class class and n-1 additional slots. */
LISPFUNN(allocate_std_instance,2) {
  /* check length, should be a fixnum >0 that fits into a uintW: */
  var uintL length;
  test_record_length(length);
  skipSTACK(1);
  var object instance =
    allocate_srecord(0,Rectype_Instance,length,instance_type);
  var object clas = popSTACK();
  if_classp(clas, ; , fehler_keine_klasse(clas); );
  TheInstance(instance)->inst_class = clas;
  TheInstance(instance)->inst_cl_id = TheClass(clas)->class_id;
  /* fill the slots of the instance with #<UNBOUND> : */
  length-=2;
  if (length > 0) {
    var gcv_object_t* ptr = &TheInstance(instance)->other[0];
    dotimespL(length,length, { *ptr++ = unbound; } );
  }
  VALUES1(instance); /* instance as value */
}

#define check_keywords(argcount,caller)                 \
  if (argcount%2 != 0) fehler_key_odd(argcount,caller);

local Values do_allocate_instance (object clas);
/* (CLOS::%ALLOCATE-INSTANCE class &rest initargs)
  returns an instance of the class.
  class must be an instance of <standard-class> or <structure-class>. */
LISPFUN(pallocate_instance,seclass_read,1,0,rest,nokey,0,NIL) {
  check_keywords(argcount,S(allocate_instance));
  set_args_end_pointer(rest_args_pointer); /* clean up STACK */
  return_Values do_allocate_instance(popSTACK());
}
local Values do_allocate_instance (object clas) {
  /* Make a distinction between <standard-class> and <structure-class> for
     allocate-instance: Is (class-shared-slots class) a vector or NIL, or
     is (class-names class) a cons? */
  if (matomp(TheClass(clas)->shared_slots)) { /* <standard-class>. */
    pushSTACK(clas); /* save for ALLOCATE-STD-INSTANCE */
    if (nullp(TheClass(clas)->precedence_list)) { /* finalize */
      pushSTACK(clas); pushSTACK(T);
      funcall(S(class_finalize),2);
    }
    /* (CLOS::ALLOCATE-STD-INSTANCE class (class-instance-size class)) */
    pushSTACK(TheClass(clas)->instance_size);
    C_allocate_std_instance();
  } else { /* <structure-class>. */
    /* (SYS::%MAKE-STRUCTURE (class-names class) (class-instance-size class))*/
    pushSTACK(TheClass(clas)->shared_slots);
    pushSTACK(TheClass(clas)->instance_size);
    C_make_structure();
    /* fill the slots of the structure with #<UNBOUND> for
       INITIALIZE-INSTANCE to enter the default-values later: */
    var uintL count = Structure_length(value1)-1;
    if (count > 0) {
      var gcv_object_t* ptr = &TheStructure(value1)->recdata[1];
      dotimespL(count,count, { *ptr++ = unbound; } );
    }
  }
}

/* (CLOS:SLOT-VALUE instance slot-name)
 (CLOS::SET-SLOT-VALUE instance slot-name new-value)
 (CLOS:SLOT-BOUNDP instance slot-name)
 (CLOS:SLOT-MAKUNBOUND instance slot-name)
 (CLOS:SLOT-EXISTS-P instance slot-name)
 CLtL2 p. 855,857 */

/* Derives the address of an existing slot in an instance of a standard-
 or structure-class from a slot-location-info. */
local inline gcv_object_t* ptr_to_slot (object instance, object slotinfo) {
  instance_un_realloc(instance); /* by this time update_instance() is done */
  return (atomp(slotinfo)
          /* local slot, slotinfo is index */
          ? &TheSrecord(instance)->recdata[posfixnum_to_L(slotinfo)]
          /* shared slot, slotinfo is (class . index) */
          : &TheSvector(TheClass(Car(slotinfo))->shared_slots)
               ->data[posfixnum_to_L(Cdr(slotinfo))]);
}

/* UP: visits a slot.
 slot_up()
 > STACK_1: instance
 > STACK_0: slot-name
 < result: pointer to the slot (value1 = (class-of instance)),
             or NULL (then SLOT-MISSING was called). */
local gcv_object_t* slot_up (void) {
  pushSTACK(STACK_1); C_class_of(); /* determine (CLASS-OF instance) */
  var object slotinfo = /* (GETHASH slot-name (class-slot-location-table class)) */
    gethash(STACK_0,TheClass(value1)->slot_location_table);
  if (!eq(slotinfo,nullobj)) { /* found? */
    return ptr_to_slot(STACK_1,slotinfo);
  } else { /* missing slot -> (SLOT-MISSING class instance slot-name caller) */
    pushSTACK(value1); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
    pushSTACK(TheSubr(subr_self)->name);
    funcall(S(slot_missing),4);
    return NULL;
  }
}

LISPFUNN(slot_value,2) {
  var gcv_object_t* slot = slot_up();
  if (slot) {
    var object value = *slot;
    if (boundp(value)) {
      value1 = value;
    } else { /* (SLOT-UNBOUND class instance slot-name) */
      pushSTACK(value1); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
      funcall(S(slot_unbound),3);
    }
  }
  mv_count=1;
  skipSTACK(2);
}

LISPFUNN(set_slot_value,3) {
  /* stack layout: instance, slot-name, new-value. */
  pushSTACK(STACK_2); C_class_of(); /* determine(CLASS-OF instance) */
  var object slotinfo = /* (GETHASH slot-name (class-slot-location-table class)) */
    gethash(STACK_1,TheClass(value1)->slot_location_table);
  if (!eq(slotinfo,nullobj)) { /* found? */
    value1 = *ptr_to_slot(STACK_2,slotinfo) = STACK_0;
  } else { /* missing slot
              -> (SLOT-MISSING class instance slot-name 'setf new-value) */
    pushSTACK(value1); pushSTACK(STACK_(2+1)); pushSTACK(STACK_(1+2));
    pushSTACK(S(setf)); pushSTACK(STACK_(0+4));
    funcall(S(slot_missing),5);
    value1 = STACK_0;
  }
  mv_count=1;
  skipSTACK(3);
}

LISPFUNN(slot_boundp,2) {
  var gcv_object_t* slot = slot_up();
  VALUES_IF(slot ? boundp(*slot) : !nullp(value1));
  skipSTACK(2);
}

LISPFUNN(slot_makunbound,2) {
  var gcv_object_t* slot = slot_up();
  if (slot) { *slot = unbound; }
  VALUES1(STACK_1); /* instance as value */
  skipSTACK(2);
}

LISPFUNNR(slot_exists_p,2) {
  pushSTACK(STACK_1); C_class_of(); /* determine (CLASS-OF instance) */
  var object slotinfo = /* (GETHASH slot-name (class-slot-location-table class)) */
    gethash(STACK_0,TheClass(value1)->slot_location_table);
  VALUES_IF(! eq(slotinfo,nullobj)); skipSTACK(2);
}

/* (CLOS::%CHANGE-CLASS instance new-class do-copy-p)
   copy instance (and return the copy)
   make instance point to a new instance of new-class */
LISPFUNN(pchange_class,3) {
  var bool do_copy_p = !nullp(popSTACK());
  /* STACK: instance, new-class */
  instance_un_realloc(STACK_0);
  instance_un_realloc(STACK_1);
  do_allocate_instance(STACK_0);
  pushSTACK(value1); /* the new object, to be filled in Lisp */
  if (do_copy_p) {
    /* a copy of the old instance - the return value of CHANGE-CLASS */
    var object clas = class_of(STACK_2);
    pushSTACK(clas);
    do_allocate_instance(STACK_0); /* these values are returned */
    if (structurep(value1))
      copy_mem_o(&TheStructure(value1)->structure_types,
                 &TheStructure(STACK_3)->structure_types,
                 Structure_length(STACK_3));
    else /* CLOS class instance */
      copy_mem_o(&TheInstance(value1)->inst_class,
                 &TheInstance(STACK_3)->inst_class,
                 posfixnum_to_L(TheClass(STACK_0)->instance_size));
    skipSTACK(1);
  } else
    VALUES1(NIL);
  /* STACK: instance, new-class, new-instance */
  { /* Turn instance into a realloc (see the instance_un_realloc macro): */
    set_break_sem_1(); /* forbid interrupts */
    var Instance ptr = TheInstance(STACK_2);
    record_flags_set(ptr,instflags_forwarded_B);
    ptr->inst_class = STACK_0;
    clr_break_sem_1(); /* permit interrupts again */
  }
  ASSERT(Record_flags(STACK_2) == 1);
  skipSTACK(3);
}

/* update-instance-for-redefined-class
 can trigger GC */
global object update_instance (object obj) {
 #if defined(STACKCHECKS) || defined(STACKCHECKC)
  var gcv_object_t *saved_stack = STACK;
 #endif
  var object new_class = (object)TheInstance(obj)->inst_class;
  var object old_id = (object)TheInstance(obj)->inst_cl_id;
  var object old_class = new_class;
  while (!nullp(old_class) && !eq(old_id,TheClass(old_class)->class_id))
    old_class = TheClass(old_class)->previous_definition;
  if (nullp(old_class)) NOTREACHED;
  /* update instance class id to avoid infinite loop/stack overflow */
  TheInstance(obj)->inst_cl_id = TheClass(new_class)->class_id;
  pushSTACK(obj); pushSTACK(old_class); pushSTACK(new_class); /* save */
  { /* compute property-list */
    var object slots_tab = TheClass(old_class)->slot_location_table;
    var uintL index = 2*posfixnum_to_L(TheHashtable(slots_tab)->ht_maxcount);
    var uintL num_slots = 0;
    var gcv_object_t* kvt = kvtable_data(TheHashtable(slots_tab)->ht_kvtable);
    while (index) {
      index -= 2;
      if (boundp(kvt[index])) { /* non-void entry */
        /* value is slotinfo */
        var gcv_object_t *slot_ = ptr_to_slot(obj,kvt[index+1]);
        if (boundp(*slot_)) {
          pushSTACK(kvt[index]); /* key=slot-name */
          pushSTACK(*slot_); /* value=slot-value */
          num_slots++;
        }
      }
    }
    var object property_list = listof(2*num_slots);
    pushSTACK(property_list); /* save */
  } /* stack: obj old_class new_class property_list */
  /* change class */
  pushSTACK(STACK_3);/*obj*/ pushSTACK(STACK_(1+1));/*new_class*/
  pushSTACK(NIL); funcall(L(pchange_class),3);
  { /* update-instance-for-redefined-class */
    var object property_list = popSTACK(); /* restore */
    var object added_discarded = TheClass(STACK_1)->prototype; /* old_class */
    pushSTACK(STACK_2); /*obj*/ pushSTACK(Car(added_discarded));
    pushSTACK(Cdr(added_discarded)); pushSTACK(property_list);
    funcall(S(update_instance_frc),4);
  }
  obj = STACK_2; skipSTACK(3); /* drop new_class, old_class, obj */
 #if defined(STACKCHECKS) || defined(STACKCHECKC)
  if (saved_stack != STACK) abort();
 #endif
  /* obj is a reallocated instance, so we need to unrealloc it */
  instance_un_realloc(obj);
  return obj; /* return the argument */
}

/* UP: check keywords, cf. SYSTEM::KEYWORD-TEST
 keyword_test(caller,rest_args_pointer,argcount,valid_keywords);
 > caller: caller (a symbol)
 > rest_args_pointer: pointer to the arguments
 > argcount: number of arguments / 2
 > valid_keywords: list of valid keywords or T if all are valid */
local void keyword_test (object caller, gcv_object_t* rest_args_pointer,
                         uintC argcount, object valid_keywords) {
  if (argcount==0)
    return;
  if (eq(valid_keywords,T))
    return;
  { /* check whether all specified keywords occur in valid_keywords: */
    var gcv_object_t* ptr = rest_args_pointer;
    var uintC count = argcount;
    do {
      var object key = NEXT(ptr);
      var object val = NEXT(ptr);
      if (eq(key,S(Kallow_other_keys))) {
        if (nullp(val)) break;  /* need a check */
        else return;            /* no check */
      }
    } while(--count);
    ptr = rest_args_pointer;
    count = argcount;
    do {
      var object key = NEXT(ptr);
      var object val = NEXT(ptr);
      if (!symbolp(key))
        fehler_key_notkw(key,caller);
      if (!eq(key,S(Kallow_other_keys))
          && nullp(memq(key,valid_keywords))) /* not found */
        fehler_key_badkw(caller,key,val,valid_keywords);
    } while(--count);
  }
}

/* UP: find initarg of the slot in the arglist */
local inline gcv_object_t* slot_in_arglist (const object slot, uintC argcount,
                                            gcv_object_t* rest_args_pointer) {
  var object l = TheSvector(slot)->data[1]; /* (slotdef-initargs slot) */
  var gcv_object_t* ptr = rest_args_pointer;
  var uintC count;
  dotimespC(count,argcount, {
    var object initarg = NEXT(ptr);
    if (!nullp(memq(initarg,l)))
      return ptr;
    NEXT(ptr);
  });
  return NULL;
}

/* (CLOS::%SHARED-INITIALIZE instance slot-names &rest initargs)
 instance is an Instance of <standard-object> or <structure-object>,
 initargs is a list of pairs.
 This is the primary method of CLOS:SHARED-INITIALIZE.
 cf. clos.lisp
 (defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
   (dolist (slot (class-slots (class-of instance)))
     (let ((slotname (slotdef-name slot)))
       (multiple-value-bind (init-key init-value foundp)
           (get-properties initargs (slotdef-initargs slot))
         (declare (ignore init-key))
         (if foundp
           (setf (slot-value instance slotname) init-value)
           (unless (slot-boundp instance slotname)
             (let ((init (slotdef-initer slot)))
               (when init
                 (when (or (eq slot-names 'T)
                           (member slotname slot-names :test #'eq))
                   (setf (slot-value instance slotname)
                         (if (car init) (funcall (car init))
                             (cdr init)))))))))))
   instance) */
LISPFUN(pshared_initialize,seclass_default,2,0,rest,nokey,0,NIL) {
  check_keywords(argcount,S(shared_initialize));
  argcount = argcount/2; /* number of Initarg/Value-pairs */
  { /* stack layout: instance, slot-names, argcount Initarg/Value-Pairs. */
    var object instance = Before(rest_args_pointer STACKop 1);
    /* Instance of <standard-class> or <structure-class>: */
    var object clas = class_of(instance); /* instance var is now invalid */
    /* list of all slots (as slot-definitions): */
    var object slots = TheClass(clas)->slots;
    while (consp(slots)) {
      var object slot = Car(slots);
      slots = Cdr(slots);
      /* search if the slot is initialized by the initargs: */
      if (argcount > 0) {
        var gcv_object_t* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
        if (NULL == ptr)
          goto initarg_not_found;
        value1 = NEXT(ptr);
        goto fill_slot;
      }
     initarg_not_found:
      { /* not found -> test for (slot-boundp instance slotname) first:
           (slotdef-location slot): */
        var object slotinfo = TheSvector(slot)->data[2];
        if (!eq(*ptr_to_slot(Before(rest_args_pointer STACKop 1),slotinfo),
                unbound))
          goto slot_done;
      }
      { /* slot does not have a value yet. Poss. evaluate the initform: */
        var object init = TheSvector(slot)->data[3]; /* (slotdef-initer slot) */
        if (atomp(init))
          goto slot_done;
        { /* search slot in slot-names: */
          var object slotnames = Before(rest_args_pointer);
          if (eq(slotnames,T))
            goto eval_init;
          var object slotname = TheSvector(slot)->data[0]; /* (slotdef-name slot) */
          if (!nullp(memq(slotname,slotnames)))
            goto eval_init;
          goto slot_done;
        }
       eval_init:
        /* evaluate the initform: */
        if (!nullp(Car(init))) {
          pushSTACK(slots); pushSTACK(slot);
          funcall(Car(init),0);
          slot = popSTACK(); slots = popSTACK();
        } else {
          value1 = Cdr(init);
        }
      }
     fill_slot: { /* initialize slot with value1: */
        /* (slotdef-location slot) */
        var object slotinfo = TheSvector(slot)->data[2];
        *ptr_to_slot(Before(rest_args_pointer STACKop 1),slotinfo) = value1;
      }
     slot_done: ;
    }
  }
  VALUES1(Before(rest_args_pointer STACKop 1)); /* instance as value */
  set_args_end_pointer(rest_args_pointer STACKop 2); /* clean up STACK */
}

/* UP: call the non-%SHARED-INITIALIZE init function */
local inline void call_init_fun (object fun, object last,
                                 gcv_object_t* rest_args_pointer,
                                 uintC argcount) {
  /* shift initargs in the stack down by 1, then call fun: */
  if (argcount > 0) {
    var gcv_object_t* ptr = rest_args_pointer;
    var uintC count;
    dotimespC(count,argcount, {
      var object next = Next(ptr); NEXT(ptr) = last;
      last = Next(ptr); NEXT(ptr) = next;
    });
  }
  pushSTACK(last);
  funcall(fun,2*argcount+2);
}

/* (CLOS::%REINITIALIZE-INSTANCE instance &rest initargs)
 instance is an Instance of <standard-object> or <structure-object>,
 initargs as list of pairs.
 This is the primary method of CLOS:REINITIALIZE-INSTANCE.
 cf. clos.lisp
 (defmethod reinitialize-instance ((instance standard-object) &rest initargs
                                   &key &allow-other-keys)
   (let ((h (gethash (class-of instance) *reinitialize-instance-table*)))
     (if h
       (progn
         ; 28.1.9.2. validity of initialization arguments
         (let ((valid-keywords (car h)))
           (sys::keyword-test initargs valid-keywords))
         (if (not (eq (cdr h) #'clos::%shared-initialize))
           ; apply effective method of shared-initialize:
           (apply (cdr h) instance 'NIL initargs)
           ; clos::%shared-initialize with slot-names=NIL can be simplified:
           (progn
             (dolist (slot (class-slots (class-of instance)))
               (let ((slotname (slotdef-name slot)))
                 (multiple-value-bind (init-key init-value foundp)
                     (get-properties initargs (slotdef-initargs slot))
                   (declare (ignore init-key))
                   (if foundp
                     (setf (slot-value instance slotname) init-value)))))
             instance)))
       (apply #'initial-reinitialize-instance instance initargs)))) */
LISPFUN(preinitialize_instance,seclass_default,1,0,rest,nokey,0,NIL) {
  var object instance = Before(rest_args_pointer);
  /* instance of <standard-class> or <structure-class>: */
  var object clas = class_of(instance); /* instance var is now invalid */
  { /* search (GETHASH class *REINITIALIZE-INSTANCE-TABLE*): */
    var object info =
      gethash(clas,Symbol_value(S(reinitialize_instance_table)));
    if (eq(info,nullobj)) {
      /* calculate hash-table-entry freshly. See clos.lisp. */
      funcall(S(initial_reinitialize_instance),argcount+1); return;
    }
    check_keywords(argcount,S(reinitialize_instance));
    argcount = argcount/2; /* number of Initarg/Value-pairs */
    keyword_test(S(reinitialize_instance),rest_args_pointer,
                 argcount,Car(info));
    /* stack layout: instance, slot-names, argcount Initarg/Value-pairs. */
    var object fun = Cdr(info);
    if (!eq(fun,L(pshared_initialize))) {
      call_init_fun(fun,NIL,rest_args_pointer,argcount);
      return;
    }
  }
  { /* CLOS::%SHARED-INITIALIZE with slot-names=NIL can be simplified:
       list of all slots (as slot-definitions): */
    var object slots = TheClass(clas)->slots;
    while (consp(slots)) {
      var object slot = Car(slots);
      slots = Cdr(slots);
      /* search if the slot is initialized by the initargs: */
      if (argcount > 0) {
        var gcv_object_t* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
        if (NULL != ptr) {
          var object value = NEXT(ptr);
          /* initialize slot with value:
             (slotdef-location slot): */
          var object slotinfo = TheSvector(slot)->data[2];
          *ptr_to_slot(Before(rest_args_pointer),slotinfo) = value;
        }
      }
    }
  }
  VALUES1(Before(rest_args_pointer)); /* instance as value */
  set_args_end_pointer(rest_args_pointer STACKop 1); /* clean up STACK */
}

/* (CLOS::%INITIALIZE-INSTANCE instance &rest initargs)
 instance is an Instance of <standard-object> or <structure-object>,
 initargs is a list of pairs.
 This is the primary method of CLOS:INITIALIZE-INSTANCE.
 cf. clos.lisp
 (defmethod initialize-instance ((instance standard-object) &rest initargs
                                 &key &allow-other-keys)
   (let ((h (gethash class *make-instance-table*)))
     (if h
       (if (not (eq (svref h 3) #'clos::%shared-initialize))
         ; apply effective method of shared-initialize:
         (apply (svref h 3) instance 'T initargs)
         ; clos::%shared-initialize with slot-names=T can be simplified:
         (progn
           (dolist (slot (class-slots (class-of instance)))
             (let ((slotname (slotdef-name slot)))
               (multiple-value-bind (init-key init-value foundp)
                   (get-properties initargs (slotdef-initargs slot))
                 (declare (ignore init-key))
                 (if foundp
                   (setf (slot-value instance slotname) init-value)
                   (unless (slot-boundp instance slotname)
                     (let ((init (slotdef-initer slot)))
                       (when init
                         (setf (slot-value instance slotname)
                               (if (car init) (funcall (car init))
                                   (cdr init))))))))))
           instance))
       (apply #'initial-initialize-instance instance initargs)))) */
local Values do_initialize_instance (object info,
                                     gcv_object_t* rest_args_pointer,
                                     uintC argcount);
LISPFUN(pinitialize_instance,seclass_default,1,0,rest,nokey,0,NIL) {
  var object instance = Before(rest_args_pointer);
  /* instance of <standard-class> or <structure-class>: */
  var object clas = class_of(instance); /* instance var is not invalid */
  { /* search (GETHASH class *MAKE-INSTANCE-TABLE*): */
    var object info = gethash(clas,Symbol_value(S(make_instance_table)));
    if (eq(info,nullobj)) {
      /* calculate hash-table-entry freshly. See clos.lisp. */
      funcall(S(initial_initialize_instance),argcount+1); return;
    }
    check_keywords(argcount,S(initialize_instance));
    argcount = argcount/2; /* number of Initarg/Value-pairs */
    return_Values do_initialize_instance(info,rest_args_pointer,argcount);
  }
}
local Values do_initialize_instance (object info,
                                     gcv_object_t* rest_args_pointer,
                                     uintC argcount) {
  { /* stack layout: instance, argcount Initarg/Value-pairs. */
    var object fun = TheSvector(info)->data[3];
    if (!eq(fun,L(pshared_initialize))) {
      call_init_fun(fun,T,rest_args_pointer,argcount);
      return;
    }
  }
  { /* CLOS::%SHARED-INITIALIZE with slot-names=T can be simplified: */
    var object instance = Before(rest_args_pointer);
    var object clas = class_of(instance); /* instance of <standard-class> or <structure-class> */
    var object slots = TheClass(clas)->slots; /* list of all slots (as slot-definitions) */
    while (consp(slots)) {
      var object slot = Car(slots);
      slots = Cdr(slots);
      /* search if the slot is initialized by the initargs: */
      if (argcount > 0) {
        var gcv_object_t* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
        if (NULL == ptr)
          goto initarg_not_found;
        value1 = NEXT(ptr);
        goto fill_slot;
      }
     initarg_not_found:
      { /* not found -> first test for (slot-boundp instance slotname): */
        var object slotinfo = TheSvector(slot)->data[2]; /* (slotdef-location slot) */
        if (boundp(*ptr_to_slot(Before(rest_args_pointer),slotinfo)))
          goto slot_done;
      }
      { /* Slot has no value yet. Evaluate the initform: */
        var object init = TheSvector(slot)->data[3]; /* (slotdef-initer slot) */
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
     fill_slot: { /* initialize slot with value1: */
        var object slotinfo = TheSvector(slot)->data[2]; /* (slotdef-location slot) */
        *ptr_to_slot(Before(rest_args_pointer),slotinfo) = value1;
      }
     slot_done: ;
    }
  }
  VALUES1(Before(rest_args_pointer)); /* instance as value */
  set_args_end_pointer(rest_args_pointer STACKop 1); /* clean up STACK */
}

/* (CLOS::%MAKE-INSTANCE class &rest initargs)
 class is an Instance of <standard-class> or <structure-class>,
 initargs is a list (of pairs, hopefully).
 cf. clos.lisp
 (defun %make-instance (class &rest initargs &key &allow-other-keys)
   ; take note of 28.1.9.3., 28.1.9.4. default-initargs:
   (dolist (default-initarg (class-default-initargs class))
     (let ((nothing default-initarg))
       (when (eq (getf initargs (car default-initarg) nothing) nothing)
         (setq initargs
               (append initargs
                 (list (car default-initarg)
                       (let ((init (cdr default-initarg)))
                         (if (car init) (funcall (car init)) (cdr init)))))))))
   (let ((h (gethash class *make-instance-table*)))
     (if h
       (progn
         ; 28.1.9.2. validity of initialization arguments
         (let ((valid-keywords (svref h 0)))
           (sys::keyword-test initargs valid-keywords))
         (let ((instance (apply #'allocate-instance class initargs)))
           (if (not (eq (svref h 2) #'clos::%initialize-instance))
             ; apply effective method of initialize-instance:
             (apply (svref h 2) instance initargs)
             ; clos::%initialize-instance can be simplified (one does not
             ; even have to look it up in *make-instance-table*):
             (if (not (eq (svref h 3) #'clos::%shared-initialize))
               ; apply effective method of shared-initialize:
               (apply (svref h 3) instance 'T initargs)
               ...))))
       (apply #'initial-make-instance class initargs)))) */
LISPFUN(pmake_instance,seclass_default,1,0,rest,nokey,0,NIL) {
  check_keywords(argcount,S(make_instance));
  argcount = argcount/2; /* number of Initarg/Value-pairs */
  /* stack layout: class, argcount Initarg/Value-pairs. */
  { /* add default-initargs: */
    var object clas = Before(rest_args_pointer);
    if (nullp(TheClass(clas)->precedence_list)) { /* finalize */
      pushSTACK(clas); pushSTACK(T);
      funcall(S(class_finalize),2);
      clas = Before(rest_args_pointer);
    }
    var object l = TheClass(clas)->default_initargs;
    while (consp(l)) {
      var object default_initarg = Car(l);
      l = Cdr(l);
      var object key = Car(default_initarg);
      /* search key among the initargs so far: */
      if (argcount > 0) {
        var gcv_object_t* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,argcount, {
          if (eq(NEXT(ptr),key))
            goto key_found;
          NEXT(ptr);
        });
      }
      /* not found */
      pushSTACK(key); /* Initarg in the stack */
      {
        var object init = Cdr(default_initarg);
        if (!nullp(Car(init))) {
          pushSTACK(l);
          funcall(Car(init),0); /* evaluate the default */
          l = STACK_0;
          STACK_0 = value1; /* value in the stack */
        } else {
          pushSTACK(Cdr(init)); /* default in the stack */
        }
      }
      argcount++;
     key_found: ;
    }
  }
  { /* search (GETHASH class *MAKE-INSTANCE-TABLE*): */
    var object clas = Before(rest_args_pointer);
    var object info = gethash(clas,Symbol_value(S(make_instance_table)));
    if (eq(info,nullobj)) {
      /* calculate hash-table-entry freshly. See clos.lisp. */
      return_Values funcall(S(initial_make_instance),2*argcount+1);
    } else { /* check keywords: */
      keyword_test(S(make_instance),rest_args_pointer,
                   argcount,TheSvector(info)->data[0]);
      /* call the effective method of ALLOCATE-INSTANCE: */
      pushSTACK(info);
      {
        var object fun = TheSvector(info)->data[1];
        if (!eq(fun,L(pallocate_instance))) {
          var gcv_object_t* ptr = rest_args_pointer STACKop 1;
          var uintC count;
          dotimespC(count,2*argcount+1, { pushSTACK(NEXT(ptr)); });
          funcall(fun,2*argcount+1);
          pushSTACK(value1); /* save instance */
          pushSTACK(value1); C_class_of();
          if (!eq(value1,Before(rest_args_pointer))) {
            /* instance already in STACK_0 */
            pushSTACK(Before(rest_args_pointer));
            pushSTACK(S(allocate_instance));
            fehler(error,GETTEXT("~S method for ~S returned ~S"));
          }
          value1 = popSTACK(); /* restore instance */
        } else {
          do_allocate_instance(clas);
        }
      }
      info = popSTACK();
      /* call the effective method of INITIALIZE-INSTANCE:
         instance as the 1st argument instead of class: */
      Before(rest_args_pointer) = value1;
      var object fun = TheSvector(info)->data[2];
      /* save the instance in case INITIALIZE-INSTANCE returns junk
         see 7.1.7 "Definitions of Make-Instance and Initialize-Instance"
         http://www.lisp.org/HyperSpec/Body/sec_7-1-7.html */
      pushSTACK(value1);
      if (argcount>0) { /* (rotatef STACK_0 ... STACK_(2*argcount)) */
        var uintC count;
        var gcv_object_t* ptr = &STACK_0;
        dotimespC(count,2*argcount,
        { *ptr = *(ptr STACKop 1); ptr skipSTACKop 1; });
        *ptr = value1;
      }
      rest_args_pointer skipSTACKop -1;
      if (eq(fun,L(pinitialize_instance)))
        /* CLOS::%INITIALIZE-INSTANCE can be simplified
           (do not have to look into *make-instance-table* again): */
        do_initialize_instance(info,rest_args_pointer,argcount);
      else
        funcall(fun,2*argcount+1);
      VALUES1(popSTACK());
    }
  }
}
