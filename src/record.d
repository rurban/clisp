# Functions for records and structures in CLISP
# Bruno Haible 1990-2002
# Sam Steingold 1998-2002
# German comments translated into English: Stefan Kain 2002-04-16

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
  value1 = fixnum(length); mv_count=1; # length as Fixnum
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

# (SYS::%STRUCTURE-REF type structure index) returns for a structure of
#   given Type type (a Symbol) the entry at index>=1.
# (SYS::%STRUCTURE-STORE type structure index object) stores object as
#   Entry index in a structure of given Type type and returns object.
# (SYS::%MAKE-STRUCTURE type length) creates a structure with length>=1
#   elements of Type type.
# (COPY-STRUCTURE structure) returns a copy of the Structure structure,
#   of the same type.
# (SYS::%STRUCTURE-TYPE-P type object) checks if object is a
#   structure that has the Type type, which can be recognized in
#   component 0. There, an object (name_1 ... name_i-1 name_i) should
#   be located with one of the names EQ to type.

# subroutine for structure-access-functions:
# > STACK_2: type-argument
# > STACK_1: structure-argument
# > STACK_0: index-argument
# > subr_self: caller (a SUBR)
# < result: Address of the structure-element
local object* structure_up (void) {
  # structure must be of Type structure:
  if (!structurep(STACK_1)) {
   fehler_bad_structure: # STACK_2 = type, STACK_1 = structure
    pushSTACK(STACK_1); # TYPE-ERROR slot DATUM
    pushSTACK(STACK_(2+1)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(STACK_(2+2));
    pushSTACK(STACK_(1+3));
    pushSTACK(TheSubr(subr_self)->name); # function name
    fehler(type_error,GETTEXT("~: ~ is not a structure of type ~"));
  }
  var object structure = STACK_1;
  # check if type occurs in namelist = (name_1 ... name_i-1 name_i) :
  if (obj_in_list(STACK_2,TheStructure(structure)->structure_types))
    goto yes;
  # type did not occur -> Error:
  goto fehler_bad_structure;
 yes: { # type did occur:
    var uintL length = (uintL)Structure_length(structure);
    var uintL index;
    # fetch index and check
    if (!(posfixnump(STACK_0) && ((index = posfixnum_to_L(STACK_0)) < length)))
      fehler_index(length);
    # address of the structure-component
    return &TheStructure(structure)->recdata[index];
  }
}

# (SYS::%%STRUCTURE-REF type structure index) returns for a structure of
#   given Type type (a symbol) the entry index>=1.
# #<UNBOUND> is possible.
LISPFUNN(pstructure_ref,3) {
  value1 = *(structure_up()); mv_count=1; # structure-element as value
  skipSTACK(3); # clean up stack
}

# (SYS::%STRUCTURE-REF type structure index) returns for a structure of
#   given Type type (a symbol) the entry index>=1.
LISPFUNN(structure_ref,3) {
  value1 = *(structure_up()); # structure-element as value
  if (eq(value1,unbound)) {
    # could be = #<UNBOUND> , after use of SLOT-MAKUNBOUND
    # or after incomplete INITIALIZE-INSTANCE
    dynamic_bind(S(print_length),Fixnum_0); # bind *PRINT-LENGTH* to 0
    pushSTACK(STACK_(1+3)); # UNBOUND-SLOT slot INSTANCE
    # (clos::slotdef-name (find index (clos::class-slots (find-class type))
    #                     :key #'clos::slotdef-location))
    pushSTACK(STACK_(2+3+1)); funcall(L(find_class),1);
    pushSTACK(value1); funcall(S(class_slots),1);
    pushSTACK(STACK_(0+3+1)); pushSTACK(value1); pushSTACK(S(Kkey));
    pushSTACK(Symbol_function(S(slotdef_location))); funcall(L(find),4);
    pushSTACK(value1); funcall(S(slotdef_name),1);
    pushSTACK(value1); # UNBOUND-SLOT slot NAME
    pushSTACK(STACK_(1+3+2));
    pushSTACK(value1);
    pushSTACK(S(structure_ref));
    fehler(unbound_slot,GETTEXT("~: Slot ~ of ~ has no value"));
  }
  mv_count=1;
  skipSTACK(3); # clean up stack
}

# (SYS::%STRUCTURE-STORE type structure index object) stores object as
#   entry index in a structure of given Type type and returns object.
LISPFUNN(structure_store,4) {
  var object value = popSTACK();
  value1 = *(structure_up()) = value; mv_count=1; # enter structure-element
  skipSTACK(3); # clean up stack
}

# (SYS::%MAKE-STRUCTURE type length) creates a structure with length>=1
#   elements of Type type.
LISPFUNN(make_structure,2) {
  # check length, should be a fixnum /=0  that fits into a uintW:
  var uintL length;
  test_record_length(length);
  skipSTACK(1);
  var object structure = allocate_structure(length);
  # new structure, filled with NILs
  TheStructure(structure)->structure_types = popSTACK(); # enter type-component
  value1 = structure; mv_count=1; # structure as value
}

# (COPY-STRUCTURE structure) returns a copy of the Structure structure
#   of the same type.
LISPFUNN(copy_structure,1) {
  if (!structurep(STACK_0)) {
    # STACK_0 = TYPE-ERROR slot DATUM
    pushSTACK(S(structure_object)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(STACK_1); # structure
    pushSTACK(TheSubr(subr_self)->name); # function name
    fehler(type_error,GETTEXT("~: ~ is not a structure"));
  }
  var uintC length = Structure_length(STACK_0);
  var object new_structure = allocate_structure(length); # new structure
  { # fill:
    var object* old_ptr = &TheStructure(popSTACK())->structure_types;
    var object* new_ptr = &TheStructure(new_structure)->structure_types;
    dotimespC(length,length, { *new_ptr++ = *old_ptr++; });
  }
  # return as value:
  value1 = new_structure; mv_count=1;
}

# (SYS::%STRUCTURE-TYPE-P type object) checks if object is a
#   structure that has the Type type, which can be recognized in
#   component 0. There, an object (name_1 ... name_i-1 name_i) should
#   be located with one of the names EQ to type.
LISPFUNN(structure_type_p,2) {
  # check object for structure:
  if (!structurep(STACK_0)) { skipSTACK(2); goto no; }
  {
    var object namelist = TheStructure(popSTACK())->structure_types;
    var object type = popSTACK();
    # test, if type occurs in namelist = (name_1 ... name_i-1 name_i) :
    if (obj_in_list(type,namelist))
      goto yes;
  }
 no: # type did not occur:
  value1 = NIL; mv_count=1; return; # 1 value NIL
 yes: # type did occur:
  value1 = T; mv_count=1; return; # 1 value T
}

# ===========================================================================
# Closures:

# (SYS::CLOSURE-NAME closure) returns the name of a closure.
# (SYS::CLOSURE-CODEVEC closure) returns the code-vector of a compiled
#   closure as a list of fixnums >=0, <256.
# (SYS::CLOSURE-CONSTS closure) returns a list of all constants of a
#   compiled closure.
# (SYS::MAKE-CODE-VECTOR list) returns for a list of fixnums >=0, <256
#   a simple-bit-vector of eight fold length, that contains these numbers
#    as bytes.
# (SYS::%MAKE-CLOSURE name codevec consts) returns a closure with given
#   name (a symbol), given code-vector (a simple-bit-vector) and
#   further given constants.
# (SYS::%COPY-GENERIC-FUNCTION venv closure) copies the closure, which must be
#   a generic function with venv slot, copying in the given venv.
# (SYS::GENERIC-FUNCTION-EFFECTIVE-METHOD-FUNCTION generic-function)
#   returns a function, which delivers the effective methods, so that
#   (APPLY generic-function arguments)
#   == (APPLY (APPLY ergebnis arguments) arguments) .

# (SYS::CLOSURE-NAME closure) returns the name of a closure.
LISPFUNN(closure_name,1) {
  var object closure = popSTACK();
  if (!closurep(closure)) {
    pushSTACK(closure);
    pushSTACK(TheSubr(subr_self)->name); # function name
    fehler(error, # type_error ??
           GETTEXT("~: ~ is not a closure"));
  }
  value1 = TheClosure(closure)->clos_name; mv_count=1;
}

# error, if argument is not a compiled closure
nonreturning_function(local, fehler_cclosure, (object obj)) {
  pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name); # function name
  fehler(error, # type_error ??
         GETTEXT("~: This is not a compiled closure: ~"));
}

# (SYS::CLOSURE-CODEVEC closure) returns the code-vector of a compiled
#   closure, as list of fixnums >=0, <256.
LISPFUNN(closure_codevec,1) {
  var object closure = popSTACK();
  if (!(cclosurep(closure))) fehler_cclosure(closure);
  var object codevec = TheCclosure(closure)->clos_codevec;
  var uintL index = Sbvector_length(codevec); # index := length in bytes
  # step through codevector codevec from behind and push bytes onto a list:
  pushSTACK(codevec); # codevector
  pushSTACK(NIL); # list := ()
  while (index != 0) {
    index--; # decrement index
    # put new cons in front of the list:
    var object new_cons = allocate_cons();
    Cdr(new_cons) = popSTACK();
    Car(new_cons) = fixnum((uintL)(TheSbvector(STACK_0)->data[index])); # fetch byte
    pushSTACK(new_cons);
  }
  value1 = STACK_0; mv_count=1; skipSTACK(2); # list as value
}

# (SYS::CLOSURE-CONSTS closure) returns a list of all constants of a
#   compiled closure.
LISPFUNN(closure_consts,1) {
  var object closure = popSTACK();
  if (!(cclosurep(closure))) fehler_cclosure(closure);
  # comprise elements 2,3,... to a list:
  var uintC index = Cclosure_length(closure)-2; # index := length
  # step through closure from behind and push constants onto a list:
  pushSTACK(closure); # closure
  pushSTACK(NIL); # list := ()
  while (index != 0) {
    index--; # decrement index
    # put new cons in front of the list:
    var object new_cons = allocate_cons();
    Cdr(new_cons) = popSTACK();
    Car(new_cons) = TheCclosure(STACK_0)->clos_consts[(uintP)index]; # fetch constant
    pushSTACK(new_cons);
  }
  value1 = STACK_0; mv_count=1; skipSTACK(2); # list as value
}

# (SYS::MAKE-CODE-VECTOR list) returns for a list of fixnums >=0, <256
#   a simple-bit-vector of eight fold length, that contains these
#   numbers as bytes.
LISPFUNN(make_code_vector,1) {
  var object bv = allocate_bit_vector(Atype_8Bit,llength(STACK_0)); # simple-8bit-vector
  # fill:
  var object listr = popSTACK(); # list
  var uintB* ptr = &TheSbvector(bv)->data[0]; # loop through the bit-vector
  while (consp(listr)) {
    var uintL byte;
    # list element must be a fixnum >=0, <256 :
    if (!(posfixnump(Car(listr))
          && ((byte = posfixnum_to_L(Car(listr))) < (1<<intBsize))))
      goto bad_byte;
    # put into the bit-vector:
    *ptr++ = (uintB)byte;
    listr = Cdr(listr);
  }
  value1 = bv; mv_count=1; return; # bv as value
 bad_byte:
  pushSTACK(Car(listr)); # TYPE-ERROR slot DATUM
  pushSTACK(O(type_uint8)); # TYPE-ERROR slot EXPECTED-TYPE
  pushSTACK(STACK_1);
  fehler(type_error,GETTEXT("~ is not a valid code-vector byte"));
}

# (SYS::%MAKE-CLOSURE name codevec consts) returns a closure with given
#   name (a symbol), given code-vector (a simple-bit-vector) and
#   further given constants.
LISPFUNN(make_closure,3) {
  # codevec must be a simple-bit-vector:
  if (!simple_bit_vector_p(Atype_8Bit,STACK_1)) {
    # STACK_1 = codevec
    pushSTACK(STACK_1); # TYPE-ERROR slot DATUM
    pushSTACK(S(simple_bit_vector)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(STACK_(1+2));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~: invalid code-vector ~"));
  }
  # create a new closure of length (+ 2 (length consts)) :
  var uintL length = 2+llength(STACK_0);
  if (!(length <= (uintL)(bitm(intWsize)-1))) { # should fit into a uintW
    # STACK_0 = consts
    pushSTACK(STACK_2); # name
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~: function ~ is too big: ~"));
  }
  var object closure = allocate_closure(length);
  TheCclosure(closure)->clos_name = STACK_2; # fill name
  TheCclosure(closure)->clos_codevec = STACK_1; # fill codevector
  { # fill constants:
    var object constsr = popSTACK();
    var object* ptr = &TheCclosure(closure)->clos_consts[0];
    while (consp(constsr)) {
      *ptr++ = Car(constsr); constsr = Cdr(constsr);
    }
  }
  value1 = closure; mv_count=1; skipSTACK(2);
}

# (SYS::%COPY-GENERIC-FUNCTION venv closure) copies the closure, which must be
#   a generic function with venv slot, copying in the given venv.
LISPFUNN(copy_generic_function,2) {
  # Note: closure's clos_consts[0] is a simple-vector #(NIL c1 ... cn) where
  # c1,...,cn are constant objects, and NIL is the placeholder to be replaced
  # with the passed venv.
  var object oldclos = STACK_0;
  if (!genericfunctionp(oldclos)) {
    pushSTACK(oldclos); # TYPE-ERROR slot DATUM
    pushSTACK(S(standard_generic_function)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(oldclos);
    pushSTACK(TheSubr(subr_self)->name); # function name
    fehler(type_error,GETTEXT("~: This is not a generic function: ~"));
  }
  var object vector = TheCclosure(oldclos)->clos_consts[0];
  if (!(simple_vector_p(vector)
        && (Svector_length(vector) > 0)
        && nullp(TheSvector(vector)->data[0]))) {
    pushSTACK(oldclos);
    pushSTACK(TheSubr(subr_self)->name); # function name
    fehler(error,
           GETTEXT("~: This is not a prototype of a generic function: ~"));
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

# (SYS::GENERIC-FUNCTION-EFFECTIVE-METHOD-FUNCTION generic-function)
#   returns a function, which delivers the effective methods, so that
#   (APPLY generic-function arguments)
#   == (APPLY (APPLY ergebnis arguments) arguments) .
#   is used for CALL-NEXT-METHOD; can assume that the
#   generic-function has already been called, i.e. that the dispatch has
#   already been installed.
LISPFUNN(generic_function_effective_method_function,1) {
  var object oldclos = STACK_0;
  var object newclos;
  if (!genericfunctionp(oldclos)) {
    pushSTACK(oldclos); # TYPE-ERROR slot DATUM
    pushSTACK(S(standard_generic_function)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(oldclos);
    pushSTACK(TheSubr(subr_self)->name); # function name
    fehler(type_error,GETTEXT("~: This is not a generic function: ~"));
  }
  # allocate closure of equal length:
  newclos = allocate_cclosure_copy(oldclos);
  oldclos = STACK_0;
  do_cclosure_copy(newclos,oldclos);
  STACK_0 = newclos;
  # copy the code-vector likewise:
  var object newcodevec = copy_sbvector(TheClosure(newclos)->clos_codevec);
  # set the bit therein which is queried by the RETGF-instruction:
  TheCodevec(newcodevec)->ccv_flags |= bit(3);
  newclos = popSTACK();
  TheClosure(newclos)->clos_codevec = newcodevec;
  value1 = newclos; mv_count=1;
}

# ===========================================================================
# load-time-eval:

# (SYS::MAKE-LOAD-TIME-EVAL form) returns a load-time-eval-object that
#   - if printed and read again - evaluates form.

# (SYS::MAKE-LOAD-TIME-EVAL form) liefert ein Load-Time-Eval-Objekt, das
#   - wenn ausgegeben und wieder eingelesen - form auswertet.
LISPFUNN(make_load_time_eval,1) {
  var object lte = allocate_loadtimeeval();
  TheLoadtimeeval(lte)->loadtimeeval_form = popSTACK();
  value1 = lte; mv_count=1;
}

# ===========================================================================
# symbol-macro:

# (SYS::MAKE-SYMBOL-MACRO expansion) returns a symbol-macro-object
#   that represents the given expansion.
# (SYS::SYMBOL-MACRO-P object) tests for symbol-macro.

# Due to their special meaning in the interpreter, symbol-macro-objects
# - like #<UNBOUND> and #<SPECDECL> - are not first class objects.
# They can be passed only as values. They cannot be assigned to
# variables, however.

# (SYMBOL-MACRO-EXPAND symbol) tests if a symbol represents a symbol-macro
# and returns T and the expansion if true, NIL if false.

# (SYS::MAKE-SYMBOL-MACRO expansion) returns a symbol-macro-object,
#   that represents the given expansion.
LISPFUNN(make_symbol_macro,1) {
  var object sm = allocate_symbolmacro();
  TheSymbolmacro(sm)->symbolmacro_expansion = popSTACK();
  value1 = sm; mv_count=1;
}

# (SYS::SYMBOL-MACRO-P object) tests for symbol-macro.
LISPFUNN(symbol_macro_p,1) {
  var object obj = popSTACK();
  value1 = (symbolmacrop(obj) ? T : NIL); mv_count=1;
}

# (SYMBOL-MACRO-EXPAND symbol) tests if a symbol represents a symbol-macro
# and returns T and the expansion if true, NIL if false.
# (defun symbol-macro-expand (v)
#   (unless (symbolp v) (error ...))
#   (and (boundp v) (symbol-macro-p (%symbol-value v))
#        (values t (sys::%record-ref (%symbol-value v) 0))
# ) )
LISPFUNN(symbol_macro_expand,1) {
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

# (SYS::MAKE-MACRO expander) returns a Macro object with the given expander
# function.
LISPFUNN(make_macro,1) {
  var object m = allocate_macro();
  var object arg = popSTACK();
  if (!functionp(arg))
    fehler_function(arg);
  TheMacro(m)->macro_expander = arg;
  value1 = m; mv_count=1;
}

# (SYS::MACROP object) tests for a Macro.
LISPFUNN(macrop,1) {
  var object obj = popSTACK();
  value1 = (macrop(obj) ? T : NIL); mv_count=1;
}

# (SYS::MACRO-EXPANDER macro) returns the macro's expander function.
LISPFUNN(macro_expander,1) {
  var object obj = popSTACK();
  if (!macrop(obj)) {
    pushSTACK(obj);
    pushSTACK(S(macro_expander)); # function name
    fehler(error, # type_error ??
           GETTEXT("~: ~ is not a Macro"));
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

# (SYS::MAKE-FUNCTION-MACRO function expander) returns a FunctionMacro object
# for the given function and with the given expander function.
LISPFUNN(make_function_macro,2) {
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

# (SYS::FUNCTION-MACRO-P object) tests for a FunctionMacro.
LISPFUNN(function_macro_p,1) {
  var object obj = popSTACK();
  value1 = (functionmacrop(obj) ? T : NIL); mv_count=1;
}

# (SYS::FUNCTION-MACRO-FUNCTION macro) returns the functionmacro's function.
LISPFUNN(function_macro_function,1) {
  var object obj = popSTACK();
  if (!functionmacrop(obj)) {
    pushSTACK(obj);
    pushSTACK(S(function_macro_function)); # function name
    fehler(error, # type_error ??
           GETTEXT("~: ~ is not a FunctionMacro"));
  }
  value1 = TheFunctionMacro(obj)->functionmacro_function; mv_count=1;
}

# (SYS::FUNCTION-MACRO-EXPANDER macro) returns the functionmacro's expander.
LISPFUNN(function_macro_expander,1) {
  var object obj = popSTACK();
  if (!functionmacrop(obj)) {
    pushSTACK(obj);
    pushSTACK(S(function_macro_expander)); # function name
    fehler(error, # type_error ??
           GETTEXT("~: ~ is not a FunctionMacro"));
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
# Finalizer:

# (FINALIZE object function &optional alive)
# records that function is called if object dies through GC, with
# object and poss. alive as argument. If alive dies before object dies,
# nothing will be done.
LISPFUN(finalize,2,1,norest,nokey,0,NIL) {
  STACK_1 = coerce_function(STACK_1);
  if (!gcinvariant_object_p(STACK_2)) {
    var object f = allocate_finalizer();
    TheFinalizer(f)->fin_trigger = STACK_2;
    TheFinalizer(f)->fin_function = STACK_1;
    TheFinalizer(f)->fin_alive = STACK_0; # The default #<UNBOUND> lives eternally.
    TheFinalizer(f)->fin_cdr = O(all_finalizers);
    O(all_finalizers) = f;
  }
  skipSTACK(3); value1 = NIL; mv_count=1;
}

# ===========================================================================
# CLOS objects:

# (CLOS::STRUCTURE-OBJECT-P object) checks if object is a structure.
LISPFUNN(structure_object_p,1) {
  var object obj = popSTACK();
  value1 = (structurep(obj) ? T : NIL); mv_count=1;
}

# (CLOS::STD-INSTANCE-P object) checks if object is a CLOS-object.
LISPFUNN(std_instance_p,1) {
  var object obj = popSTACK();
  value1 = (instancep(obj) ? T : NIL); mv_count=1;
}

# returns (CLOS:CLASS-OF object). Is especially efficient for CLOS-objects.
#define class_of(obj)  \
    (instancep(obj) ? TheInstance(obj)->inst_class           \
                    : (pushSTACK(obj), C_class_of(), value1))

# error-message if an object is not a class.
# fehler_keine_klasse(caller,obj);
# > subr_self: caller
# > obj: non-class
nonreturning_function(local, fehler_keine_klasse, (object obj)) {
  pushSTACK(obj); # TYPE-ERROR slot DATUM
  pushSTACK(S(class)); # CLOS:CLASS, TYPE-ERROR slot EXPECTED-TYPE
  pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name); # function name
  fehler(type_error,GETTEXT("~: ~ is not a class"));
}

# (CLOS::ALLOCATE-STD-INSTANCE class n) returns a CLOS-instance of length n,
# with Class class and n-1 additional slots.
LISPFUNN(allocate_std_instance,2) {
  # check length, should be a fixnum >0 that fits into a uintW:
  var uintL length;
  test_record_length(length);
  skipSTACK(1);
  var object instance =
    allocate_srecord(0,Rectype_Instance,length,instance_type);
  var object clas = popSTACK();
  if_classp(clas, ; , fehler_keine_klasse(clas); );
  TheInstance(instance)->inst_class = clas;
  # fill the slots of the instance with #<UNBOUND> :
  if (--length > 0) {
    var object* ptr = &TheInstance(instance)->other[0];
    dotimespL(length,length, { *ptr++ = unbound; } );
  }
  value1 = instance; mv_count=1; # instance as value
}

# report un-paired keywords error
nonreturning_function(local, fehler_key_odd, (uintC argcount, object caller)) {
  var object arglist = listof(argcount);
  pushSTACK(arglist); pushSTACK(caller);
  fehler(program_error,
         GETTEXT("~: keyword arguments in ~ should occur pairwise"));
}
#define check_keywords(argcount,caller) \
  if (argcount%2 != 0) fehler_key_odd(argcount,caller);

local Values do_allocate_instance (object clas);
# (CLOS::%ALLOCATE-INSTANCE class &rest initargs)
#   returns an instance of the class.
#   class must be an instance of <standard-class> or <structure-class>.
LISPFUN(pallocate_instance,1,0,rest,nokey,0,NIL) {
  check_keywords(argcount,S(allocate_instance));
  set_args_end_pointer(rest_args_pointer); # clean up STACK
  return_Values do_allocate_instance(popSTACK());
}
local Values do_allocate_instance (object clas) {
  # Make a distinction between <standard-class> and <structure-class> for
  # allocate-instance: Is (class-shared-slots class) a vector or NIL, or
  # is (class-names class) a cons?
  if (matomp(TheClass(clas)->shared_slots)) {
    # class is a <standard-class>.
    # (CLOS::ALLOCATE-STD-INSTANCE class (class-instance-size class))
    pushSTACK(clas); pushSTACK(TheClass(clas)->instance_size);
    C_allocate_std_instance();
  } else {
    # class is a <structure-class>.
    # (SYS::%MAKE-STRUCTURE (class-names class) (class-instance-size class))
    pushSTACK(TheClass(clas)->shared_slots);
    pushSTACK(TheClass(clas)->instance_size);
    C_make_structure();
    # fill the slots of the structure with #<UNBOUND> for
    # INITIALIZE-INSTANCE to enter the default-values later:
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
# CLtL2 p. 855,857

# Derives the address of an existing slot in an instance of a standard-
# or structure-class from a slot-location-info.
#define ptr_to_slot(instance,slotinfo)  \
    (atomp(slotinfo)                                            \
     # local slot, slotinfo is index                            \
     ? &TheSrecord(instance)->recdata[posfixnum_to_L(slotinfo)] \
     # shared slot, slotinfo is (class . index)                 \
     : &TheSvector(TheClass(Car(slotinfo))->shared_slots)       \
                  ->data[posfixnum_to_L(Cdr(slotinfo))])

# UP: visits a slot.
# slot_up()
# > STACK_1: instance
# > STACK_0: slot-name
# < result: pointer to the slot (value1 = (class-of instance)),
#             or NULL (then SLOT-MISSING was called).
local object* slot_up (void);
#ifdef RISCOS_CCBUG
  #pragma -z0
#endif
local object* slot_up (void) {
  pushSTACK(STACK_1); C_class_of(); # determine (CLASS-OF instance)
  var object slotinfo = # (GETHASH slot-name (class-slot-location-table class))
    gethash(STACK_0,TheClass(value1)->slot_location_table);
  if (!eq(slotinfo,nullobj)) { # found?
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

LISPFUNN(slot_value,2) {
  var object* slot = slot_up();
  if (slot) {
    var object value = *slot;
    if (!eq(value,unbound)) {
      value1 = value;
    } else { # (SLOT-UNBOUND class instance slot-name)
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
LISPFUNN(set_slot_value,3) {
  # stack layout: instance, slot-name, new-value.
  pushSTACK(STACK_2); C_class_of(); # determine(CLASS-OF instance)
  var object slotinfo = # (GETHASH slot-name (class-slot-location-table class))
    gethash(STACK_1,TheClass(value1)->slot_location_table);
  if (!eq(slotinfo,nullobj)) { # found?
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

LISPFUNN(slot_boundp,2) {
  var object* slot = slot_up();
  if (slot) { value1 = (eq(*slot,unbound) ? NIL : T); }
  mv_count=1;
  skipSTACK(2);
}

LISPFUNN(slot_makunbound,2) {
  var object* slot = slot_up();
  if (slot) { *slot = unbound; }
  value1 = STACK_1; mv_count=1; # instance as value
  skipSTACK(2);
}

#ifdef RISCOS_CCBUG
  #pragma -z0
#endif
LISPFUNN(slot_exists_p,2) {
  pushSTACK(STACK_1); C_class_of(); # determine (CLASS-OF instance)
  var object slotinfo = # (GETHASH slot-name (class-slot-location-table class))
    gethash(STACK_0,TheClass(value1)->slot_location_table);
  value1 = (eq(slotinfo,nullobj) ? NIL : T); mv_count=1; skipSTACK(2);
}
#ifdef RISCOS_CCBUG
  #pragma -z1
#endif

# UP: check keywords, cf. SYSTEM::KEYWORD-TEST
# keyword_test(caller,rest_args_pointer,argcount,valid_keywords);
# > caller: caller (a symbol)
# > rest_args_pointer: pointer to the arguments
# > argcount: number of arguments / 2
# > valid_keywords: list of valid keywords or T if all are valid
local void keyword_test (object caller, object* rest_args_pointer,
                         uintC argcount, object valid_keywords) {
  if (argcount==0)
    return;
  if (eq(valid_keywords,T))
    return;
  { # search if :ALLOW-OTHER-KEYS comes:
    var object* ptr = rest_args_pointer;
    var uintC count;
    dotimespC(count,argcount, {
      if (eq(NEXT(ptr),S(Kallow_other_keys)))
        if (!nullp(Next(ptr)))
          return;
      NEXT(ptr);
    });
  }
  { # search if all specified keywords occur in valid_keywords:
    var object* ptr = rest_args_pointer;
    var uintC count;
    dotimespC(count,argcount, {
      var object key = NEXT(ptr);
      if (obj_in_list(key,valid_keywords))
        goto kw_found;
      # not found
      pushSTACK(key); # KEYWORD-ERROR slot DATUM
      pushSTACK(valid_keywords);
      pushSTACK(valid_keywords);
      pushSTACK(Next(ptr));
      pushSTACK(key);
      pushSTACK(caller);
      {
        var object type = allocate_cons();
        Car(type) = S(member); Cdr(type) = STACK_4;
        STACK_4 = type; # `(MEMBER ,@valid_keywords) = KEYWORD-ERROR slot EXPECTED-TYPE
      }
      fehler(keyword_error,
             GETTEXT("~: illegal keyword/value pair ~, ~ in argument list."
                     NLstring "The allowed keywords are ~"));
     kw_found: # found. continue:
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

# (CLOS::%SHARED-INITIALIZE instance slot-names &rest initargs)
# instance is an Instance of <standard-object> or <structure-object>,
# initargs is a list of pairs.
# This is the primary method of CLOS:SHARED-INITIALIZE.
# cf. clos.lisp
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
LISPFUN(pshared_initialize,2,0,rest,nokey,0,NIL) {
  check_keywords(argcount,S(shared_initialize));
  argcount = argcount/2; # number of Initarg/Value-pairs
  { # stack layout: instance, slot-names, argcount Initarg/Value-Pairs.
    var object instance = Before(rest_args_pointer STACKop 1);
    # Instance of <standard-class> or <structure-class>:
    var object clas = class_of(instance);
    # list of all slots (as slot-definitions):
    var object slots = TheClass(clas)->slots;
    while (consp(slots)) {
      var object slot = Car(slots);
      slots = Cdr(slots);
      # search if the slot is initialized by the initargs:
      if (argcount > 0) {
        var object* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
        if (NULL == ptr)
          goto initarg_not_found;
        value1 = NEXT(ptr);
        goto fill_slot;
      }
     initarg_not_found:
      { # not found -> test for (slot-boundp instance slotname) first:
        # (slotdef-location slot):
        var object slotinfo = TheSvector(slot)->data[2];
        if (!eq(*ptr_to_slot(Before(rest_args_pointer STACKop 1),slotinfo),
                unbound))
          goto slot_done;
      }
      { # slot does not have a value yet. Poss. evaluate the initform:
        var object init = TheSvector(slot)->data[3]; # (slotdef-initer slot)
        if (atomp(init))
          goto slot_done;
        { # search slot in slot-names:
          var object slotnames = Before(rest_args_pointer);
          if (eq(slotnames,T))
            goto eval_init;
          var object slotname = TheSvector(slot)->data[0]; # (slotdef-name slot)
          if (obj_in_list(slotname,slotnames))
            goto eval_init;
          goto slot_done;
        }
       eval_init:
        # evaluate the initform:
        if (!nullp(Car(init))) {
          pushSTACK(slots); pushSTACK(slot);
          funcall(Car(init),0);
          slot = popSTACK(); slots = popSTACK();
        } else {
          value1 = Cdr(init);
        }
      }
     fill_slot: { # initialize slot with value1:
        # (slotdef-location slot)
        var object slotinfo = TheSvector(slot)->data[2];
        *ptr_to_slot(Before(rest_args_pointer STACKop 1),slotinfo) = value1;
      }
     slot_done: ;
    }
  }
  value1 = Before(rest_args_pointer STACKop 1); mv_count=1; # instance as value
  set_args_end_pointer(rest_args_pointer STACKop 2); # clean up STACK
}

# (CLOS::%REINITIALIZE-INSTANCE instance &rest initargs)
# instance is an Instance of <standard-object> or <structure-object>,
# initargs as list of pairs.
# This is the primary method of CLOS:REINITIALIZE-INSTANCE.
# cf. clos.lisp
# (defmethod reinitialize-instance ((instance standard-object) &rest initargs)
#   (let ((h (gethash (class-of instance) *reinitialize-instance-table*)))
#     (if h
#       (progn
#         ; 28.1.9.2. validity of initialization arguments
#         (let ((valid-keywords (car h)))
#           (sys::keyword-test initargs valid-keywords)
#         )
#         (if (not (eq (cdr h) #'clos::%shared-initialize))
#           ; apply effective method of shared-initialize:
#           (apply (cdr h) instance 'NIL initargs)
#           ; clos::%shared-initialize with slot-names=NIL can be simplified:
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
LISPFUN(preinitialize_instance,1,0,rest,nokey,0,NIL) {
  var object instance = Before(rest_args_pointer);
  # instance of <standard-class> or <structure-class>:
  var object clas = class_of(instance);
  { # search (GETHASH class *REINITIALIZE-INSTANCE-TABLE*):
    var object info =
      gethash(clas,Symbol_value(S(reinitialize_instance_table)));
    if (eq(info,nullobj)) {
      # calculate hash-table-entry freshly. See clos.lisp.
      funcall(S(initial_reinitialize_instance),argcount+1); return;
    }
    check_keywords(argcount,S(reinitialize_instance));
    argcount = argcount/2; # number of Initarg/Value-pairs
    keyword_test(S(reinitialize_instance),rest_args_pointer,argcount,Car(info));
    # stack layout: instance, slot-names, argcount Initarg/Value-pairs.
    var object fun = Cdr(info);
    if (!eq(fun,L(pshared_initialize))) {
      # shift initargs in the stack down by 1, then call fun:
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
  { # CLOS::%SHARED-INITIALIZE with slot-names=NIL can be simplified:
    # list of all slots (as slot-definitions):
    var object slots = TheClass(clas)->slots;
    while (consp(slots)) {
      var object slot = Car(slots);
      slots = Cdr(slots);
      # search if the slot is initialized by the initargs:
      if (argcount > 0) {
        var object* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
        if (NULL != ptr) {
          var object value = NEXT(ptr);
          # initialize slot with value:
          # (slotdef-location slot):
          var object slotinfo = TheSvector(slot)->data[2];
          *ptr_to_slot(Before(rest_args_pointer),slotinfo) = value;
        }
      }
    }
  }
  value1 = Before(rest_args_pointer); mv_count=1; # instance as value
  set_args_end_pointer(rest_args_pointer STACKop 1); # clean up STACK
}

# (CLOS::%INITIALIZE-INSTANCE instance &rest initargs)
# instance is an Instance of <standard-object> or <structure-object>,
# initargs is a list of pairs.
# This is the primary method of CLOS:INITIALIZE-INSTANCE.
# cf. clos.lisp
# (defmethod initialize-instance ((instance standard-object) &rest initargs)
#   (let ((h (gethash class *make-instance-table*)))
#     (if h
#       (if (not (eq (svref h 3) #'clos::%shared-initialize))
#         ; apply effective method of shared-initialize:
#         (apply (svref h 3) instance 'T initargs)
#         ; clos::%shared-initialize with slot-names=T can be simplified:
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
local Values do_initialize_instance (object info, object* rest_args_pointer,
                                     uintC argcount);
LISPFUN(pinitialize_instance,1,0,rest,nokey,0,NIL) {
  var object instance = Before(rest_args_pointer);
  # instance of <standard-class> or <structure-class>:
  var object clas = class_of(instance);
  { # search (GETHASH class *MAKE-INSTANCE-TABLE*):
    var object info = gethash(clas,Symbol_value(S(make_instance_table)));
    if (eq(info,nullobj)) {
      # calculate hash-table-entry freshly. See clos.lisp.
      funcall(S(initial_initialize_instance),argcount+1); return;
    }
    check_keywords(argcount,S(initialize_instance));
    argcount = argcount/2; # number of Initarg/Value-pairs
    return_Values do_initialize_instance(info,rest_args_pointer,argcount);
  }
}
local Values do_initialize_instance (object info, object* rest_args_pointer,
                                     uintC argcount) {
  # stack layout: instance, argcount Initarg/Value-pairs.
  {
    var object fun = TheSvector(info)->data[3];
    if (!eq(fun,L(pshared_initialize))) {
      # shift initargs in the stack down by 1, then call fun:
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
  { # CLOS::%SHARED-INITIALIZE with slot-names=T can be simplified:
    var object instance = Before(rest_args_pointer);
    var object clas = class_of(instance); # instance of <standard-class> or <structure-class>
    var object slots = TheClass(clas)->slots; # list of all slots (as slot-definitions)
    while (consp(slots)) {
      var object slot = Car(slots);
      slots = Cdr(slots);
      # search if the slot is initialized by the initargs:
      if (argcount > 0) {
        var object* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
        if (NULL == ptr)
          goto initarg_not_found;
        value1 = NEXT(ptr);
        goto fill_slot;
      }
     initarg_not_found:
      { # not found -> first test for (slot-boundp instance slotname):
        var object slotinfo = TheSvector(slot)->data[2]; # (slotdef-location slot)
        if (!eq(*ptr_to_slot(Before(rest_args_pointer),slotinfo),unbound))
          goto slot_done;
      }
      { # Slot has no value yet. Evaluate the initform:
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
     fill_slot: { # initialize slot with value1:
        var object slotinfo = TheSvector(slot)->data[2]; # (slotdef-location slot)
        *ptr_to_slot(Before(rest_args_pointer),slotinfo) = value1;
      }
     slot_done: ;
    }
  }
  value1 = Before(rest_args_pointer); mv_count=1; # instance as value
  set_args_end_pointer(rest_args_pointer STACKop 1); # clean up STACK
}

#ifdef RISCOS_CCBUG
  #pragma -z0
#endif
# (CLOS::%MAKE-INSTANCE class &rest initargs)
# class is an Instance of <standard-class> or <structure-class>,
# initargs is a list (of pairs, hopefully).
# cf. clos.lisp
# (defun %make-instance (class &rest initargs)
#   ; take note of 28.1.9.3., 28.1.9.4. default-initargs:
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
#             ; apply effective method of initialize-instance:
#             (apply (svref h 2) instance initargs)
#             ; clos::%initialize-instance can be simplified (one does not
#             ; even have to look it up in *make-instance-table*):
#             (if (not (eq (svref h 3) #'clos::%shared-initialize))
#               ; apply effective method of shared-initialize:
#               (apply (svref h 3) instance 'T initargs)
#               ...
#             )
#       ) ) )
#       (apply #'initial-make-instance class initargs)
# ) ) )
LISPFUN(pmake_instance,1,0,rest,nokey,0,NIL) {
  check_keywords(argcount,S(make_instance));
  argcount = argcount/2; # number of Initarg/Value-pairs
  # stack layout: class, argcount Initarg/Value-pairs.
  { # add default-initargs:
    var object clas = Before(rest_args_pointer);
    var object l = TheClass(clas)->default_initargs;
    while (consp(l)) {
      var object default_initarg = Car(l);
      l = Cdr(l);
      var object key = Car(default_initarg);
      # search key among the initargs so far:
      if (argcount > 0) {
        var object* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,argcount, {
          if (eq(NEXT(ptr),key))
            goto key_found;
          NEXT(ptr);
        });
      }
      # not found
      pushSTACK(key); # Initarg in the stack
      {
        var object init = Cdr(default_initarg);
        if (!nullp(Car(init))) {
          pushSTACK(l);
          funcall(Car(init),0); # evaluate the default
          l = STACK_0;
          STACK_0 = value1; # value in the stack
        } else {
          pushSTACK(Cdr(init)); # default in the stack
        }
      }
      argcount++;
     key_found: ;
    }
  }
  { # search (GETHASH class *MAKE-INSTANCE-TABLE*):
    var object clas = Before(rest_args_pointer);
    var object info = gethash(clas,Symbol_value(S(make_instance_table)));
    if (eq(info,nullobj)) {
      # calculate hash-table-entry freshly. See clos.lisp.
      return_Values funcall(S(initial_make_instance),2*argcount+1);
    } else { # check keywords:
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
      if (eq(fun,L(pinitialize_instance)))
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
