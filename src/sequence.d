# Sequences for CLISP
# Bruno Haible 1987-2001
# Sam Steingold 2001

#include "lispbibl.c"


# O(seq_types) contains a list of type descriptors for sequences.
# These are simple-vectors of length 16, containing:
#  SEQ-TYPE        ; the type of the sequence, usually a symbol
#  access functions:
#  SEQ-INIT
#  SEQ-UPD
#  SEQ-ENDTEST
#  SEQ-FE-INIT
#  SEQ-FE-UPD
#  SEQ-FE-ENDTEST
#  SEQ-ACCESS
#  SEQ-ACCESS-SET
#  SEQ-COPY
#  SEQ-LENGTH
#  SEQ-MAKE
#  SEQ-ELT
#  SEQ-SET-ELT
#  SEQ-INIT-START
#  SEQ-FE-INIT-END

/*

 Explanation of the functions SEQ-XXX:

A "Pointer" is something, that can step through a sequence.
There are pointers, that move from left to right;
  they are created with INIT or INIT-START, copied with COPY,
    UPD to advance one step,
    ENDTEST for testing, if they have reached the end of the Sequence,
    ACCESS  for fetching the element, which is pointed to by the pointer,
    ACCESS-SET for setting the element, which is pointed to by the pointer.
There are also pointers, that move from right to left;
  they are created with FE-INIT or FE-INIT-END, copied with COPY,
    FE-UPD for moving them one step to the left,
    FE-ENDTEST for testing, if they have reached the end of the Sequence,
    ACCESS for fetching the element, which is pointed to by the pointer.
  For them, ACCESS-SET does not work.

Movement operations:
INIT          (lambda (seq) ...) -> pointer
              returns the leftmost pointer of SEQ.
UPD           (lambda (seq pointer) ...) -> pointer
              returns a pointer to the adjacent neighbor at the right.
              SEQ-UPD can assume, that the right border of
              SEQ is not stepped over.
ENDTEST       (lambda (seq pointer) ...) -> bool
              tests, if this pointer is at the right end of SEQ.
the same "FROM END" :
FE-INIT       (lambda (seq) ...) -> pointer
              returns the rightmost pointer of SEQ.
FE-UPD        (lambda (seq pointer) ...) -> pointer
              returns a pointer to the adjacent neighbor at the left.
              SEQ-FE-UPD can assume, that the left border of
              SEQ is not stepped over.
FE-ENDTEST    (lambda (seq pointer) ...) -> bool
              tests, if this pointer is at the left end of SEQ.
Access via pointer:
ACCESS        (lambda (seq pointer) ...) -> value
              returns the element in SEQ the pointer is pointing to.
ACCESS-SET    (lambda (seq pointer value) ...) ->
              sets the element where the pointer is pointing to in SEQ, to the
              specified value. Works only for pointers that move from left to right!
COPY          (lambda (pointer) ...) -> pointer
              returns a copy of the Pointer to SEQ (because UPD and FE-UPD
              can operate destructively on the pointers)
total length:
LENGTH        (lambda (seq) ...) -> size
              returns the (active) length of the Sequence SEQ.
MAKE          (lambda (size) ...) -> sequence
              returns a newly allocated, empty sequence, that has the type
              SEQ-TYPE and the specified length.
Access via index (usually more inefficient than via pointer):
ELT           (lambda (seq index) ...) -> value
              returns (ELT SEQ index)
SET-ELT       (lambda (seq index value) ...) ->
              sets (ELT SEQ index) to value.
INIT-START    (lambda (seq index) ...) -> pointer
              returns a pointer which moves in SEQ from left to right
              from Position index. Must execute the Range-test by itself.
FE-INIT-END   (lambda (seq index) ...) -> pointer
              returns a pointer which moves in SEQ from right to left
              from Position index. Must execute the Range-test by itself.

*/

#define seq_type(seqdesc)         (TheSvector(seqdesc)->data[0])
#define seq_init(seqdesc)         (TheSvector(seqdesc)->data[1])
#define seq_upd(seqdesc)          (TheSvector(seqdesc)->data[2])
#define seq_endtest(seqdesc)      (TheSvector(seqdesc)->data[3])
#define seq_fe_init(seqdesc)      (TheSvector(seqdesc)->data[4])
#define seq_fe_upd(seqdesc)       (TheSvector(seqdesc)->data[5])
#define seq_fe_endtest(seqdesc)   (TheSvector(seqdesc)->data[6])
#define seq_access(seqdesc)       (TheSvector(seqdesc)->data[7])
#define seq_access_set(seqdesc)   (TheSvector(seqdesc)->data[8])
#define seq_copy(seqdesc)         (TheSvector(seqdesc)->data[9])
#define seq_length(seqdesc)       (TheSvector(seqdesc)->data[10])
#define seq_make(seqdesc)         (TheSvector(seqdesc)->data[11])
#define seq_elt(seqdesc)          (TheSvector(seqdesc)->data[12])
#define seq_set_elt(seqdesc)      (TheSvector(seqdesc)->data[13])
#define seq_init_start(seqdesc)   (TheSvector(seqdesc)->data[14])
#define seq_fe_init_end(seqdesc)  (TheSvector(seqdesc)->data[15])

# find sequence type NAME in O(seq_types)
# return a typedescr or NIL if no such sequence
local object find_seq_type (object name) {
  var object list = O(seq_types);
  while (consp(list)) {
    var object typdescr = Car(list); list = Cdr(list); # (pop list)
    if (eq(name,seq_type(typdescr))) return typdescr;
  }
  return NIL;
}

# UP: überprüft, ob name ein gültiger Sequence-Typ-Bezeichner ist
# (sonst Error) und liefert den dazugehörigen Typdescriptor.
# valid_type(name)
# > name: Sequence-Typ-Bezeichner
# < ergebnis: dazugehöriger Typdescriptor
# < -(STACK): durch den Typ erzwungene Länge, oder unbound.
# can trigger GC
local object valid_type1 (object name) {
  # Unsere elementaren Sequence-Typen sind LIST, VECTOR, STRING, BIT-VECTOR.
  # Wir erkennen aber auch gewisse Alias-Namen:
  # - DEFTYPE-defininierte Typen werden expandiert.
  # - ([SIMPLE-]ARRAY [eltype [1 | (dim)]]), (VECTOR [eltype [size]]) ergeben
  #   STRING falls eltype = CHARACTER,
  #   BIT-VECTOR falls eltype = BIT,
  #   n [steht für (VECTOR (UNSIGNED-BYTE n))] falls eltype = n BIT,
  #   VECTOR sonst.
  # - (SIMPLE-VECTOR [size]), VECTOR, SIMPLE-VECTOR ergeben VECTOR.
  # - ([SIMPLE-]STRING [size]), [SIMPLE-]STRING ergeben STRING.
  # - ([SIMPLE-]BASE-STRING [size]), [SIMPLE-]BASE-STRING ergeben STRING.
  # - ([SIMPLE-]BIT-VECTOR [size]), [SIMPLE-]BIT-VECTOR ergeben BIT-VECTOR.
  # - Zusätzlich (nicht sehr schön): [SIMPLE-]ARRAY ergibt VECTOR.
  name = expand_deftype(name,false);
  if (symbolp(name)) {
    if (eq(name,S(list))) { goto expanded_unconstrained; }
    if (eq(name,S(null)) || eq(name,S(cons)))
      { name = S(list); goto expanded_unconstrained; }
    if (eq(name,S(vector))) { goto expanded_unconstrained; }
    if (eq(name,S(simple_vector)))
      { name = S(vector); goto expanded_unconstrained; }
    if (eq(name,S(string))) { goto expanded_unconstrained; }
    if (eq(name,S(simple_string)) || eq(name,S(base_string))
        || eq(name,S(simple_base_string)))
      { name = S(string); goto expanded_unconstrained; }
    if (eq(name,S(bit_vector))) { goto expanded_unconstrained; }
    if (eq(name,S(simple_bit_vector)))
      { name = S(bit_vector); goto expanded_unconstrained; }
    if (eq(name,S(array)) || eq(name,S(simple_array)))
      { name = S(vector); goto expanded_unconstrained; }
    goto expanded_unconstrained; # sonstige Symbole können DEFSTRUCT-Typen sein
  } else if (consp(name)) {
    var object name1 = Car(name);
    if (symbolp(name1)) {
      var object name2 = Cdr(name);
      if (nullp(name2) || (consp(name2) && nullp(Cdr(name2)))) {
        if (eq(name1,S(simple_vector)))
          { name = S(vector); goto expanded_maybe_constrained; }
        if (eq(name1,S(string)) || eq(name1,S(simple_string))
            || eq(name1,S(base_string)) || eq(name1,S(simple_base_string)))
          { name = S(string); goto expanded_maybe_constrained; }
        if (eq(name1,S(bit_vector)) || eq(name1,S(simple_bit_vector)))
          { name = S(bit_vector); goto expanded_maybe_constrained; }
        if (false) {
        expanded_maybe_constrained:
          if (consp(name2) && integerp(Car(name2)))
            { pushSTACK(Car(name2)); goto expanded; }
          else goto expanded_unconstrained;
        }
      }
      {
        var object name3;
        if (nullp(name2)) { name2 = S(mal); name3 = S(mal); goto try_vector; }
        if (consp(name2))
          { name3=Cdr(name2); name2 = Car(name2);
          if (nullp(name3)) { name3 = S(mal); goto try_vector; }
          if (consp(name3) && nullp(Cdr(name3)))
            { name3 = Car(name3); goto try_vector; }
          }
        if (false) {
        try_vector: # Hier ist name2 = (second name), name3 = (third name), Defaults: *
          if (eq(name1,S(vector))
              || (   (eq(name1,S(array)) || eq(name1,S(simple_array)))
                  && (eq(name3,S(mal)) || eq(name3,Fixnum_1)
                      || (consp(name3) && nullp(Cdr(name3)))))) {
            if (eq(name1,S(vector))) {
              if (integerp(name3)) pushSTACK(name3); else pushSTACK(unbound);
            } else {
              if (consp(name3) && integerp(Car(name3))) pushSTACK(Car(name3));
              else pushSTACK(unbound);
            }
            var uintB atype = eltype_code(name2);
            if (atype==Atype_T) { # (VECTOR T)
              name = S(vector); goto expanded;
            } else if (atype==Atype_Char) { # (VECTOR CHARACTER)
              name = S(string); goto expanded;
            } else if (atype==Atype_Bit) { # (VECTOR BIT)
              name = S(bit_vector); goto expanded;
            } else { # (VECTOR (UNSIGNED-BYTE n))
              name = fixnum(bit(atype)); goto expanded; }
          }
        }
      }
    }
  }
  return NIL;
 expanded_unconstrained:
  pushSTACK(unbound); # no length constraint
 expanded:
  # SEQ-TYPES-Liste durchgehen:
  return find_seq_type(name);
}

# same as valid_type1, but signal an error instead of returning NIL
# when name does not name a sequence
local object valid_type (object name) {
  var object typedescr = valid_type1(name);
  if (!nullp(typedescr))
    return typedescr;
  # otherwise -- signal an error
  pushSTACK(name); # TYPE-ERROR slot DATUM
  pushSTACK(O(type_recognizable_sequence_type));# TYPE-ERROR slot EXPECTED-TYPE
  pushSTACK(name);
  fehler(type_error,GETTEXT("There are no sequences of type ~"));
}

# UP: liefert den Typdescriptor einer Sequence
# get_seq_type(seq)
# > seq: eine Sequence
# < ergebnis: Typdescriptor oder NIL
local object get_seq_type (object seq) { var object name;
 if (listp(seq)) name = S(list); # Typ LIST
 else if (vectorp(seq)) {
   switch (Array_type(seq)) {
     case Array_type_sstring: case Array_type_string:
       name = S(string); break; # Typ STRING
     case Array_type_sbvector: case Array_type_bvector:
       name = S(bit_vector); break; # Typ BIT-VECTOR
     case Array_type_sb2vector:
     case Array_type_sb4vector:
     case Array_type_sb8vector:
     case Array_type_sb16vector:
     case Array_type_sb32vector: # Typ n, bedeutet (VECTOR (UNSIGNED-BYTE n))
       name = fixnum(bit(sbNvector_atype(seq))); break;
     case Array_type_b2vector:
     case Array_type_b4vector:
     case Array_type_b8vector:
     case Array_type_b16vector:
     case Array_type_b32vector: # Typ n, bedeutet (VECTOR (UNSIGNED-BYTE n))
       name = fixnum(bit(bNvector_atype(seq))); break;
     default:
       name = S(vector); break; # Typ [GENERAL-]VECTOR
   }
 } else if (structurep(seq)) {
   name = TheStructure(seq)->structure_types; # Structure-Typen-List*e
   while (consp(name)) { name = Cdr(name); } # davon den letzten Typ nehmen
 } else return NIL;
 # SEQ-TYPES-Liste durchgehen:
 return find_seq_type(name);
}

# UP: liefert den Typdescriptor einer Sequence, evtl. Fehlermeldung
# get_valid_seq_type(seq)
# > seq: eine Sequence
# < ergebnis: Typdescriptor
  local object get_valid_seq_type (object seq);
  local object get_valid_seq_type(seq)
    var object seq;
    { var object typdescr = get_seq_type(seq); # Typdescriptor bestimmen
      if (!(nullp(typdescr))) { return typdescr; } # gefunden -> OK
      # sonst Fehler melden:
      pushSTACK(seq);         # TYPE-ERROR slot DATUM
      pushSTACK(S(sequence)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(seq);
      fehler(type_error,
             GETTEXT("~ is not a sequence")
            );
    }

# Fehler, wenn der Sequence-Typ eine andere Länge vorgibt als die, die
# herauskommt.
  nonreturning_function(local, fehler_seqtype_length, (object seqtype_length, object computed_length)) {
    pushSTACK(computed_length); # TYPE-ERROR slot DATUM
    pushSTACK(NIL);
    pushSTACK(computed_length);
    pushSTACK(seqtype_length);
    pushSTACK(S(eql)); pushSTACK(seqtype_length);
    { var object type = listof(2); STACK_2 = type; } # TYPE-ERROR slot EXPECTED-TYPE
    fehler(type_error,
           GETTEXT("sequence type forces length ~, but result has length ~")
          );
  }

# Fehler, wenn Argument kein Integer >=0
  nonreturning_function(local, fehler_posint, (object fun, object kw, object obj)) {
    pushSTACK(obj);                # TYPE-ERROR slot DATUM
    pushSTACK(O(type_posinteger)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(obj);
    pushSTACK(kw);
    pushSTACK(fun);
    fehler(type_error,
           GETTEXT("~: ~ should be an integer >=0, not ~")
          );
  }

# Macro: Trägt NIL als Defaultwert eines Parameters in den Stack ein:
# default_NIL(par);
  #define default_NIL(par)  \
    if (!boundp(par)) { par = NIL; }

# Macro: Trägt 0 als Defaultwert von START in den Stack ein:
# start_default_0(start);
  #define start_default_0(start)  \
    if (!boundp(start)) { start = Fixnum_0; }

# Macro: Trägt (SEQ-LENGTH sequence) als Defaultwert von END in den Stack ein:
# end_default_len(end,seq,typdescr);
# can trigger GC
  #define end_default_len(end,seq,typdescr)  \
    if (missingp(end))                               \
      { var object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet! \
        var object lengthfun = seq_length(typdescr); \
        pushSTACK(seq); funcall(lengthfun,1);        \
        end = value1;                                \
        subr_self = old_subr_self;                   \
      }

# UP: Überprüft START- und END- Argumente
# > subr_self: Aufrufer (ein SUBR)
# > kwptr: kwptr[0] = START-Keyword,
#          kwptr[1] = END-Keyword
# > argptr: *(argptr STACKop 1) = START-Argument,
#           *(argptr STACKop 0) = END-Argument
  local void test_start_end (const object* kwptr, const object* argptr);
  local void test_start_end(kwptr,argptr)
    var const object* kwptr;
    var const object* argptr;
    { # START-Argument muss ein Integer >= 0 sein:
      var object start = *(argptr STACKop 1);
      if (!(integerp(start) && positivep(start)))
        { fehler_posint(TheSubr(subr_self)->name,kwptr[0],start); }
      # END-Argument muss ein Integer >= 0 sein:
     {var object end = *(argptr STACKop 0);
      if (!(integerp(end) && positivep(end)))
        { fehler_posint(TheSubr(subr_self)->name,kwptr[1],end); }
      # Argumente vergleichen:
      if (!(I_I_comp(end,start)>=0)) # end >= start ?
        { # nein -> Fehler melden:
          pushSTACK(end); pushSTACK(kwptr[1]);
          pushSTACK(start); pushSTACK(kwptr[0]);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: ~ = ~ should not be greater than ~ = ~")
                );
        }
    }}

# UP: Überprüft START- und END- Argumente (END-Argument evtl. NIL)
# > subr_self: Aufrufer (ein SUBR)
# > kwptr: kwptr[0] = START-Keyword,
#          kwptr[1] = END-Keyword
# > argptr: *(argptr STACKop 1) = START-Argument,
#           *(argptr STACKop 0) = END-Argument
  local void test_start_end_1 (const object* kwptr, const object* argptr);
  local void test_start_end_1(kwptr,argptr)
    var const object* kwptr;
    var const object* argptr;
    { # START-Argument muss ein Integer >= 0 sein:
      var object start = *(argptr STACKop 1);
      if (!(integerp(start) && positivep(start)))
        { fehler_posint(TheSubr(subr_self)->name,kwptr[0],start); }
      # END-Argument muss NIL oder ein Integer >= 0 sein:
     {var object end = *(argptr STACKop 0);
      if (nullp(end)) { return; } # end=NIL -> OK, fertig
      if (!(integerp(end) && positivep(end)))
        { fehler_posint(TheSubr(subr_self)->name,kwptr[1],end); }
      # Argumente vergleichen:
      if (!(I_I_comp(end,start)>=0)) # end >= start ?
        { # nein -> Fehler melden:
          pushSTACK(end); pushSTACK(kwptr[1]);
          pushSTACK(start); pushSTACK(kwptr[0]);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: ~ = ~ should not be greater than ~ = ~")
                );
        }
    }}

# Macro: Incrementiert eine Integer-Variable (im Stack).
# increment(var)
# > var: alter Wert
# < var: neuer Wert
# < ergebnis: neuer Wert
# can trigger GC
  #define increment(var)  (var = I_1_plus_I(var)) # var := (1+ var)

# Macro: Decrementiert eine Integer-Variable (im Stack).
# decrement(var)
# > var: alter Wert
# < var: neuer Wert
# < ergebnis: neuer Wert
# can trigger GC
  #define decrement(var)  (var = I_minus1_plus_I(var)) # var := (1- var)

# Macro: Rückt einen Vorwärts-Pointer (im Stack) weiter.
# pointer_update(pointer,sequence,typdescr);
# pointer muss von der Form STACK_i sein!
# can trigger GC
  #define pointer_update(pointer,sequence,typdescr)  \
    { var object updatefun = seq_upd(typdescr);          \
      pushSTACK(sequence); # sequence                    \
      pushSTACK(*(&(pointer) STACKop 1)); # pointer      \
      funcall(updatefun,2); # (SEQ-UPD sequence pointer) \
      pointer = value1; # =: pointer                     \
    }

# Macro: Rückt einen Rückwärts-Pointer (im Stack) weiter.
# pointer_fe_update(pointer,sequence,typdescr);
# pointer muss von der Form STACK_i sein!
# can trigger GC
  #define pointer_fe_update(pointer,sequence,typdescr)  \
    { var object updatefun = seq_fe_upd(typdescr);          \
      pushSTACK(sequence); # sequence                       \
      pushSTACK(*(&(pointer) STACKop 1)); # pointer         \
      funcall(updatefun,2); # (SEQ-FE-UPD sequence pointer) \
      pointer = value1; # =: pointer                        \
    }

# Error message when trying to access past the end of a vector.
# > vector: the vector
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_vector_index_range, (object vector)) {
    var uintL len = vector_length(vector);
    pushSTACK(vector);
    pushSTACK(UL_to_I(len));
    fehler_index_range(len);
  }

# UP: kopiert einen Teil einer Sequence in eine andere Sequence.
# > STACK_6: sequence1
# > STACK_5: typdescr1
# > STACK_4: sequence2
# > STACK_3: typdescr2
# > STACK_2: count (ein Integer >=0)
# > STACK_1: pointer1
# > STACK_0: pointer2
# kopiert count Elemente von sequence1 nach sequence2 und rückt dabei
# pointer1 und pointer2 um count Stellen weiter (mit SEQ-UPD), setzt count:=0.
# can trigger GC
  local void copy_seqpart_into (void);
  local void copy_seqpart_into()
    {
      # Optimization for vectors:
      if (vectorp(STACK_6) && vectorp(STACK_4) && posfixnump(STACK_2)) {
        var uintL count = posfixnum_to_L(STACK_2);
        if (count > 0) {
          var uintL index1 = posfixnum_to_L(STACK_1);
          var uintL index2 = posfixnum_to_L(STACK_0);
          if (index1+count > vector_length(STACK_6)) {
            subr_self = L(aref); fehler_vector_index_range(STACK_6);
          }
          if (index2+count > vector_length(STACK_4)) {
            subr_self = L(store); fehler_vector_index_range(STACK_4);
          }
          var object dv1 = array_displace_check(STACK_6,count,&index1);
          var object dv2 = array_displace_check(STACK_4,count,&index2);
          if (eq(dv1,dv2))
            elt_move(dv1,index1,dv2,index2,count);
          else
            elt_copy(dv1,index1,dv2,index2,count);
          STACK_1 = I_I_plus_I(STACK_1,STACK_2);
          STACK_0 = I_I_plus_I(STACK_0,STACK_2);
        }
      } else {
        # Methode etwa so:
        # (loop
        #   (when (zerop count) (return))
        #   (SEQ2-ACCESS-SET sequence2 pointer2 (SEQ1-ACCESS sequence1 pointer1))
        #   (setq pointer1 (SEQ1-UPD pointer1))
        #   (setq pointer2 (SEQ2-UPD pointer2))
        #   (decf count)
        # )
        until (eq(STACK_2,Fixnum_0)) { # count (ein Integer) = 0 -> Ende
          # (SEQ1-ACCESS seq1 pointer1) bilden:
          pushSTACK(STACK_(6+0)); # seq1
          pushSTACK(STACK_(1+1)); # pointer1
          funcall(seq_access(STACK_(5+2)),2);
          # (SEQ2-ACCESS-SET seq2 pointer2 ...) ausführen:
          pushSTACK(STACK_(4+0)); # seq2
          pushSTACK(STACK_(0+1)); # pointer2
          pushSTACK(value1);
          funcall(seq_access_set(STACK_(3+3)),3);
          # pointer1 := (SEQ1-UPD seq1 pointer1) :
          pointer_update(STACK_1,STACK_6,STACK_5);
          # pointer2 := (SEQ2-UPD seq2 pointer2) :
          pointer_update(STACK_0,STACK_4,STACK_3);
          # count := (1- count) :
          decrement(STACK_2);
        }
      }
    }

LISPFUNN(sequencep,1)
# (SYS::SEQUENCEP object) testet, ob object eine Sequence ist.
  { var object typdescr = get_seq_type(popSTACK()); # Typdescriptor oder NIL
    VALUES_IF(!(nullp(typdescr)));
  }

LISPFUNN(defseq,1)
# (SYSTEM::%DEFSEQ typdescr) erweitert die Liste der Sequencetypen um
# typdescr (muss ein Simple-Vector der Länge 16 sein).
  { # (list typdescr) bilden:
    var object new_cons = allocate_cons();
    Car(new_cons) = STACK_0;
    # (nconc SEQ_TYPES (list typdescr)) bilden:
    Cdr(new_cons) = nreverse(O(seq_types)); # (nreverse SEQ_TYPES)
    O(seq_types) = nreverse(new_cons);
    # Typ (als Symbol) zurück:
    VALUES1(seq_type(popSTACK()));
  }

# Check the index argument for ELT and SETF ELT.
# > seq: the sequence
# > index: the index argument
local void seq_check_index (object seq, object index) {
  if (!(posfixnump(index))) {
    pushSTACK(index);             # TYPE-ERROR slot DATUM
    pushSTACK(O(type_posfixnum)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(index); pushSTACK(S(elt));
    fehler(type_error,GETTEXT("~: the index should be a fixnum >=0, not ~"));
  }
  if (vectorp(seq)) { # vector ==>
    # check index against active length (may be smaller than total size)
    var uintL len = vector_length(seq);
    if (posfixnum_to_L(index) >= len) {
      pushSTACK(seq);
      pushSTACK(index);
      fehler_index_range(len);
    }
  }
}


LISPFUNN(elt,2) # (ELT sequence index), CLTL S. 248
  { # check sequence:
    var object typdescr = get_valid_seq_type(STACK_1);
    # check index:
    seq_check_index(STACK_1,STACK_0);
    # call SEQ-ELT:
    funcall(seq_elt(typdescr),2); # (SEQ-ELT sequence index)
    # value1 als Wert
  }

LISPFUNN(setelt,3) # (SYSTEM::%SETELT sequence index value), vgl. CLTL S. 248
  { # check sequence:
    var object typdescr = get_valid_seq_type(STACK_2);
    # check index:
    seq_check_index(STACK_2,STACK_1);
    # call SEQ-SET-ELT:
    pushSTACK(STACK_(2+0)); # sequence
    pushSTACK(STACK_(1+1)); # index
    pushSTACK(STACK_(0+2)); # value
    funcall(seq_set_elt(typdescr),3); # (SEQ-SET-ELT sequence index value)
    VALUES1(popSTACK()); # value als Wert
    skipSTACK(2);
  }

# UP: Kopiert ein sequence1 - Teilstück in sequence2 hinein
# und liefert sequence2 als Wert.
# copy_seqpart_onto()
# > Stackaufbau: seq1, typdescr1, seq2, typdescr2, count, pointer1
# < STACK: aufgeräumt
# < Wert: gefüllte seq2
  local Values copy_seqpart_onto (void);
  local Values copy_seqpart_onto()
    { # Stackaufbau: seq1, typdescr1, seq2, typdescr2, count, pointer1.
      pushSTACK(STACK_3); funcall(seq_init(STACK_(2+1)),1); # (SEQ2-INIT seq2)
      pushSTACK(value1);
      # Stackaufbau: seq1, typdescr1, seq2, typdescr2, count, pointer1, pointer2.
      copy_seqpart_into(); # Teilstück von seq1 nach seq2 kopieren
      VALUES1(STACK_4); # seq2 als Wert
      skipSTACK(7);
    }

# UP: Liefert ein neu alloziertes sequence-Teilstück als Wert.
# subseq()
# > Stackaufbau: sequence, start, end, typdescr,
#   mit überprüften Argumenten (start,end Integers >=0, start<=end)
# < STACK: aufgeräumt
# < Wert: Kopie des angegebenen Teilstücks von sequence
  local Values subseq (void);
  local Values subseq()
    { STACK_1 = I_I_minus_I(STACK_1,STACK_2); # count := (- end start)
      # Stackaufbau: sequence, start, count, typdescr.
      pushSTACK(STACK_1); funcall(seq_make(STACK_(0+1)),1); # (SEQ-MAKE count)
     {var object start = STACK_2;
      var object typdescr = STACK_0;
      STACK_2 = typdescr;
      pushSTACK(STACK_1);
      STACK_2 = value1;
      # Stackaufbau: sequence, typdescr, seq2, typdescr, count.
      pushSTACK(STACK_4); pushSTACK(start); funcall(seq_init_start(typdescr),2);
      pushSTACK(value1); # (SEQ-INIT-START sequence start)
      # Stackaufbau; seq1, typdescr, seq2, typdescr, count, pointer1.
      return_Values copy_seqpart_onto(); # kopieren, seq2 als Wert
    }}

LISPFUN(subseq,2,1,norest,nokey,0,NIL)
# (SUBSEQ sequence start &optional end), CLTL S. 248
  { # Stackaufbau: sequence, start, end.
    # sequence überprüfen:
    var object typdescr = get_valid_seq_type(STACK_2);
    pushSTACK(typdescr);
    # Stackaufbau: sequence, start, end, typdescr.
    # Defaultwert für end ist (length sequence):
    if (!boundp(STACK_1)
        #ifdef X3J13_149
        || nullp(STACK_1)
        #endif
       )
      { var object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
        # end nicht angegeben -> muss end:=(length sequence) setzen:
        pushSTACK(STACK_3); funcall(seq_length(typdescr),1); # (SEQ-LENGTH sequence)
        STACK_1 = value1;
        subr_self = old_subr_self;
      }
    # Stackaufbau: sequence, start, end, typdescr.
    # Start- und End-Argumente überprüfen:
    test_start_end(&O(kwpair_start),&STACK_1);
    # Teilstück bilden:
    return_Values subseq();
  }

# UP: Kopiert sequence1 in sequence2 hinein und liefert sequence2 als Wert.
# copy_seq_onto()
# > Stackaufbau: seq1, typdescr1, seq2, typdescr2, len
# < STACK: aufgeräumt
# < Wert: gefüllte seq2
  local Values copy_seq_onto (void);
  local Values copy_seq_onto()
    { # Stackaufbau: seq1, typdescr1, seq2, typdescr2, len.
      pushSTACK(STACK_4); funcall(seq_init(STACK_(3+1)),1); # (SEQ1-INIT seq1)
      pushSTACK(value1);
      # Stackaufbau: seq1, typdescr1, seq2, typdescr2, len, pointer1.
      return_Values copy_seqpart_onto();
    }

LISPFUNN(copy_seq,1) # (COPY-SEQ sequence), CLTL S. 248
  { # Stackaufbau: sequence.
    # sequence überprüfen:
    var object typdescr = get_valid_seq_type(STACK_0);
    pushSTACK(typdescr);
    # Stackaufbau: sequence, typdescr.
    pushSTACK(STACK_1); funcall(seq_length(typdescr),1);
    pushSTACK(value1); # (SEQ-LENGTH sequence)
    # Stackaufbau: sequence, typdescr, len.
    pushSTACK(STACK_0); funcall(seq_make(STACK_(1+1)),1); # (SEQ-MAKE len)
    pushSTACK(STACK_1); pushSTACK(STACK_(0+1)); STACK_2 = value1;
    # Stackaufbau: seq1, typdescr, seq2, typdescr, len.
    return_Values copy_seq_onto();
  }

LISPFUNN(length,1)
{ /* (LENGTH sequence), CLTL p. 248 */
  var object arg = popSTACK();
  if (consp(arg)) { /* arg is a Cons */
    var object last;
    var uintL len = llength1(arg,&last);
    if (!nullp(last)) fehler_proper_list(last);
    VALUES1(fixnum(len));
    return;
  } else if (symbolp(arg)) { /* arg is a symbol */
    if (nullp(arg)) { /* other symbols are not sequences */
      VALUES1(Fixnum_0); /* NIL is a list of length 0 */
      return;
    }
  } else if (vectorp(arg)) { /* arg is a vector */
    VALUES1(fixnum(vector_length(arg))); /* vector length as fixnum */
    return;
  } else { /* arg is neither a list nor a vector */
    var object typdescr = get_valid_seq_type(arg); /* maybe error */
    /* other sequences: */
    pushSTACK(arg); funcall(seq_length(typdescr),1); /* (SEQ-LENGTH arg) */
    return;
  }
  /* arg is not a sequence */
  pushSTACK(arg);         /* TYPE-ERROR slot DATUM */
  pushSTACK(S(sequence)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(arg); pushSTACK(S(length));
  fehler(type_error,GETTEXT("~: ~ is not a sequence"));
}

LISPFUNN(reverse,1) # (REVERSE sequence), CLTL S. 248
  { var object arg = STACK_0;
    if (listp(arg))
      { # arg ist eine Liste
        VALUES1(reverse(arg)); skipSTACK(1);
      }
      else
      { var object typdescr = get_valid_seq_type(arg);
        # arg ist eine sonstige Sequence
        pushSTACK(typdescr);
        # Stackaufbau: seq1, typdescr.
        pushSTACK(arg); funcall(seq_length(typdescr),1); # (SEQ-LENGTH seq1)
        pushSTACK(value1);
        # Stackaufbau: seq1, typdescr, len.
        pushSTACK(STACK_0); funcall(seq_make(STACK_(1+1)),1); # (SEQ-MAKE len)
        pushSTACK(value1);
        # Stackaufbau: seq1, typdescr, count, seq2.
        if (vectorp(STACK_3) && posfixnump(STACK_1)) {
          var uintL count = posfixnum_to_L(STACK_1);
          if (count > 0) {
            var uintL index1 = 0;
            var object dv1 = array_displace_check(STACK_3,count,&index1);
            var uintL index2 = 0;
            var object dv2 = array_displace_check(STACK_0,count,&index1); # = STACK_0
            elt_reverse(dv1,index1,dv2,index2,count);
          }
        } else {
          pushSTACK(STACK_3); funcall(seq_fe_init(STACK_(2+1)),1); # (SEQ-FE-INIT seq1)
          pushSTACK(value1);
          # Stackaufbau: seq1, typdescr, count, seq2, pointer1.
          pushSTACK(STACK_1); funcall(seq_init(STACK_(3+1)),1); # (SEQ-INIT seq2)
          pushSTACK(value1);
          # Stackaufbau: seq1, typdescr, count, seq2, pointer1, pointer2.
          until (eq(STACK_3,Fixnum_0)) { # count (ein Integer) = 0 -> Ende
            # (SEQ-ACCESS seq1 pointer1) bilden:
            pushSTACK(STACK_5); pushSTACK(STACK_(1+1));
            funcall(seq_access(STACK_(4+2)),2); # (SEQ-ACCESS seq1 pointer1)
            # (SEQ-ACCESS-SET seq2 pointer2 ...) ausführen:
            pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); pushSTACK(value1);
            funcall(seq_access_set(STACK_(4+3)),3); # (SEQ-ACCESS-SET seq2 pointer2 ...)
            # pointer1 := (SEQ-FE-UPD seq1 pointer1) :
            pointer_fe_update(STACK_1,STACK_5,STACK_4);
            # pointer2 := (SEQ-UPD seq2 pointer2) :
            pointer_update(STACK_0,STACK_2,STACK_4);
            # count := (1- count) :
            decrement(STACK_3);
          }
          skipSTACK(2);
        }
        VALUES1(STACK_0); /* return seq2 */
        skipSTACK(4);
  }   }

LISPFUNN(nreverse,1) # (NREVERSE sequence), CLTL S. 248
  { var object seq = STACK_0;
    if (listp(seq))
      { # seq ist eine Liste
        VALUES1(nreverse(seq));
        skipSTACK(1);
      }
    elif (vectorp(seq)) {
      if (true) {
        var uintL count = vector_length(seq);
        if (count > 0) {
          var uintL index = 0;
          var object dv = array_displace_check(seq,count,&index);
          elt_nreverse(dv,index,count);
        }
      } else {
        # seq ist ein Vektor
        var object typdescr = get_valid_seq_type(seq);
        pushSTACK(typdescr);
        # Stackaufbau: seq, typdescr.
        pushSTACK(seq); funcall(seq_length(typdescr),1); # (SEQ-LENGTH seq)
        { var object len = value1;
          var object len2 = I_I_ash_I(len,Fixnum_minus1);
          pushSTACK(len2); # (ASH len -1) = (FLOOR len 2)
        }
        # Stackaufbau: seq, typdescr, count.
        pushSTACK(STACK_2); funcall(seq_init(STACK_(1+1)),1); # (SEQ-INIT seq)
        pushSTACK(value1);
        # Stackaufbau: seq, typdescr, count, pointer1.
        pushSTACK(STACK_3); funcall(seq_fe_init(STACK_(2+1)),1); # (SEQ-FE-INIT seq)
        pushSTACK(value1);
        # Stackaufbau: seq, typdescr, count, pointer1, pointer2.
        until (eq(STACK_2,Fixnum_0)) # count (ein Integer) = 0 -> Ende
          { # (SEQ-ACCESS seq pointer1) bilden:
            pushSTACK(STACK_4); pushSTACK(STACK_(1+1));
            funcall(seq_access(STACK_(3+2)),2); # (SEQ-ACCESS seq pointer1)
            pushSTACK(value1); # und retten
            # (SEQ-ACCESS seq pointer2) bilden:
            pushSTACK(STACK_(4+1)); pushSTACK(STACK_(0+1+1));
            funcall(seq_access(STACK_(3+1+2)),2); # (SEQ-ACCESS seq pointer2)
            # (SEQ-ACCESS-SET seq pointer1 ...) ausführen:
            pushSTACK(STACK_(4+1)); pushSTACK(STACK_(1+1+1)); pushSTACK(value1);
            funcall(seq_access_set(STACK_(3+1+3)),3); # (SEQ-ACCESS-SET seq pointer1 ...)
            # (SEQ-ACCESS-SET seq pointer2 ...) ausführen:
           {var object element1 = popSTACK(); # gerettetes ELement
            pushSTACK(STACK_4); pushSTACK(STACK_(0+1)); pushSTACK(element1); }
            funcall(seq_access_set(STACK_(3+3)),3); # (SEQ-ACCESS-SET seq pointer2 ...)
            # pointer1 := (SEQ-UPD seq pointer1) :
            pointer_update(STACK_1,STACK_4,STACK_3);
            # pointer2 := (SEQ-FE-UPD seq pointer2) :
            pointer_fe_update(STACK_0,STACK_4,STACK_3);
            # count := (1- count) :
            decrement(STACK_2);
          }
        skipSTACK(4);
      }
      VALUES1(popSTACK()); /* return modified seq */
    } else
      { var object typdescr = get_valid_seq_type(seq);
        # seq ist eine allgemeine Sequence
        pushSTACK(typdescr);
        # Stackaufbau: seq, typdescr.
        pushSTACK(seq); funcall(seq_length(typdescr),1); # (SEQ-LENGTH seq)
        if (!(posfixnump(value1))) # sollte ein Fixnum >=0 sein
          { pushSTACK(value1); pushSTACK(S(nreverse));
            fehler(error,
                   GETTEXT("~: bad length ~")
                  );
          }
        {var uintL len = posfixnum_to_L(value1); # len
         # Grundidee: Um eine Sequence mit len Elementen umzudrehen, müssen
         # der linke und der rechte Block mit je floor(len/2) Elementen
         # vertauscht und dann einzeln umgedreht werden (rekursiv!); das
         # mittlere Element (bei ungeradem len) bleibt unverändert.
         # Entrekursivierter Algorithmus:
         # Für j=0,1,2,... sind 2^j mal zwei (fast) adjazente Blöcke
         # der Länge k2=floor(len/2^(j+1)) zu vertauschen.
         var uintL j = 0; # j := 0
         var uintL k = len; # k = floor(len/2^j) := len
         var uintL k2; # k2 = floor(k/2)
         var uintL k1; # k1 = ceiling(k/2)
         until ((k2 = floor(k,2)) == 0) # k halbiert =0 -> Schleifenende
           { k1 = k - k2; # k1 = (altes k) - (neues k) = ceiling((altes k)/2)
            {var uintL pstack = 0; # ein Pseudo-Stack
             # Stackaufbau: seq, typdescr.
             pushSTACK(STACK_1); funcall(seq_init(STACK_(0+1)),1); # (SEQ-INIT seq)
             pushSTACK(value1);
             # Stackaufbau: seq, typdescr, pointer1.
             pushSTACK(STACK_2); pushSTACK(fixnum(k1));
             funcall(seq_init_start(STACK_(1+2)),2); # (SEQ-INIT-START seq k1)
             pushSTACK(value1);
             # Stackaufbau: seq, typdescr, pointer1, pointer2.
             # pointer1 und pointer2 laufen gemeinsam durch seq, dabei hat
             # pointer2 einen Vorsprung von k1.
             loop
               { # Zwei Blöcke der Länge k2 = floor(len/2^(j+1)) vertauschen:
                 {var uintL i = k2; # i:=k2 >0
                  do { # (SEQ-ACCESS seq pointer1) bilden:
                       pushSTACK(STACK_3); pushSTACK(STACK_(1+1));
                       funcall(seq_access(STACK_(2+2)),2); # (SEQ-ACCESS seq pointer1)
                       pushSTACK(value1); # und retten
                       # (SEQ-ACCESS seq pointer2) bilden:
                       pushSTACK(STACK_(3+1)); pushSTACK(STACK_(0+1+1));
                       funcall(seq_access(STACK_(2+1+2)),2); # (SEQ-ACCESS seq pointer2)
                       # (SEQ-ACCESS-SET seq pointer1 ...) ausführen:
                       pushSTACK(STACK_(3+1)); pushSTACK(STACK_(1+1+1)); pushSTACK(value1);
                       funcall(seq_access_set(STACK_(2+1+3)),3); # (SEQ-ACCESS-SET seq pointer1 ...)
                       # (SEQ-ACCESS-SET seq pointer2 ...) ausführen:
                      {var object element1 = popSTACK(); # gerettetes ELement
                       pushSTACK(STACK_3); pushSTACK(STACK_(0+1)); pushSTACK(element1); }
                       funcall(seq_access_set(STACK_(2+3)),3); # (SEQ-ACCESS-SET seq pointer2 ...)
                       # pointer1 := (SEQ-UPD seq pointer1) :
                       pointer_update(STACK_1,STACK_3,STACK_2);
                       # pointer2 := (SEQ-FE-UPD seq pointer2) :
                       pointer_fe_update(STACK_0,STACK_3,STACK_2);
                       --i; # i:=i-1
                     }
                     until (i==0); # bei i=0 Schleifenende
                 }
                 pstack = pstack+1; # stack:=stack+1
                 if (pstack == (1UL<<j)) break; # stack=2^j geworden -> Schleifenabbruch
                 # pointer1 und pointer2 um k1+(0 oder 1) Stellen weiterrücken:
                 { var uintL skipcount = k1;
                   { var uintL r1 = 1;
                     # r := Anzahl der Nullbits am Ende der Dualdarstellung von stack:
                     { var uintL pstackr = pstack;
                       while ((pstackr & bit(0))==0) { pstackr = pstackr>>1; r1=r1+1; }
                     }
                     # r1 = r+1
                     if (len & bit(j-r1)) # Bit j-r-1 in len gesetzt?
                       { skipcount++; } # falls ja: skipcount=k1+1, sonst skipcount=k1
                   }
                   # skipcount >= k1 >= k2 > 0
                   do { # pointer1 := (SEQ-UPD seq pointer1) :
                        pointer_update(STACK_1,STACK_3,STACK_2);
                        # pointer2 := (SEQ-FE-UPD seq pointer2) :
                        pointer_fe_update(STACK_0,STACK_3,STACK_2);
                        --skipcount;
                      }
                      until (skipcount==0);
               } }
             skipSTACK(2); # pointer1 und pointer2 vergessen
            }
            j=j+1; k=k2; # j:=j+1, k halbieren
        }  }
        skipSTACK(1); # typdescr vergessen
        VALUES1(popSTACK()); /* return modified seq */
      }
  }

LISPFUN(make_sequence,2,0,norest,key,2,
        (kw(initial_element),kw(update)) )
# (MAKE-SEQUENCE type size [:initial-element] [:update]), CLTL S. 249
# mit zusätzlichem Argument :update, z.B.
# (make-sequence 'vector 5 :initial-element 3 :update #'1+) ==> #(3 4 5 6 7)
  { # Stackaufbau: type, size, initial-element, updatefun.
    # type überprüfen:
    var object typdescr = valid_type(STACK_3);
    # Stackaufbau: type, size, initial-element, updatefun, type-len.
    STACK_4 = typdescr;
    # size überprüfen, muss Integer >=0 sein:
   {var object size = STACK_3;
    if (!(integerp(size) && positivep(size)))
      { pushSTACK(size);               # TYPE-ERROR slot DATUM
        pushSTACK(O(type_posinteger)); # TYPE-ERROR slot EXPECTED-TYPE
        pushSTACK(size); pushSTACK(S(make_sequence));
        fehler(type_error,
               GETTEXT("~: size should be an integer >=0, not ~")
              );
      }
    # initial-element bei Strings defaultmäßig ergänzen:
    if (!boundp(STACK_2)) /* :initial-element not supplied? */
      { if (boundp(STACK_1)) /* :update without :initial-element -> Error */
          { pushSTACK(S(make_sequence));
            fehler(error,
                   GETTEXT("~: :update must not be specified without :initial-element")
                  );
          }
        else if (posfixnump(seq_type(typdescr))) /* type name integer? (means byte-vector) */
          { STACK_2 = Fixnum_0; } # initial-element := 0
      }
    if (boundp(STACK_0) && !eql(STACK_0,size))
      { fehler_seqtype_length(STACK_0,size); }
    STACK_0 = size; funcall(seq_make(typdescr),1); # (SEQ-MAKE size)
    # Stackaufbau: typdescr, size, initial-element, updatefun.
   }
    if (boundp(STACK_1)) /* :initial-element supplied? */
      if (!(eq(STACK_2,Fixnum_0))) { # size (ein Integer) = 0 -> nichts zu tun
        if (!boundp(STACK_0)
            && vectorp(value1) && array_simplep(value1) && posfixnump(STACK_2)) {
          if (elt_fill(value1,0,posfixnum_to_L(STACK_2),STACK_1))
            fehler_store(value1,STACK_1);
        } else {
          pushSTACK(value1);
          # Stackaufbau: typdescr, count, element, updatefun, seq.
          pushSTACK(STACK_0); funcall(seq_init(STACK_(4+1)),1); # (SEQ-INIT seq)
          pushSTACK(value1);
          # Stackaufbau: typdescr, count, element, updatefun, seq, pointer.
          loop
            { pushSTACK(STACK_(1+0)); pushSTACK(STACK_(0+1)); pushSTACK(STACK_(3+2));
              funcall(seq_access_set(STACK_(5+3)),3); # (SEQ-ACCESS-SET seq pointer element)
              # pointer := (SEQ-UPD seq pointer) :
              pointer_update(STACK_0,STACK_1,STACK_5);
              # count := (1- count) :
              decrement(STACK_4);
              if (eq(STACK_4,Fixnum_0)) break; # count (ein Integer) = 0 -> Schleifenende
              {var object updatefun = STACK_2;
               if (boundp(updatefun)) /* if supplied, */
                 { pushSTACK(STACK_3); funcall(updatefun,1); # (FUNCALL updatefun element)
                   STACK_3 = value1; # =: element
            } }  }
          skipSTACK(1); # pointer vergessen
          value1 = popSTACK(); # seq
        }
      }
    mv_count=1; # seq als Wert
    skipSTACK(4);
  }

# UP: Wandelt ein Objekt in eine Sequence gegebenen Typs um.
# coerce_sequence(obj,result_type,error_p)
# > obj: Objekt, sollte eine Sequence sein
# > result_type: Bezeichner (Symbol) des Sequence-Typs
# > error_p: when result_type is not a sequence:
#              when true, signal an error; when false, return nullobj
# < Wert: Sequence vom Typ result_type
# can trigger GC
global Values coerce_sequence (object sequence, object result_type,
                               bool error_p) {
  pushSTACK(sequence);
  pushSTACK(result_type);
  { # check result-type:
    var object typdescr2 = (error_p ? valid_type(result_type)
                            : valid_type1(result_type));
    if (!error_p && nullp(typdescr2)) { /* result_type is not a sequence */
      VALUES1(nullobj); skipSTACK(1); return;
    }
    pushSTACK(typdescr2);
    # Stackaufbau: seq1, result-type, typdescr2-len, typdescr2.
    { var object typdescr1 = get_valid_seq_type(STACK_3); # Typ von seq1
      if (eq(seq_type(typdescr1),seq_type(typdescr2))) {
        # beide Typen dieselben -> nichts zu tun
        if (boundp(STACK_1)) {
          pushSTACK(STACK_3); funcall(seq_length(typdescr1),1); # (SEQ1-LENGTH seq1)
          if (!eql(value1,STACK_1))
            fehler_seqtype_length(STACK_1,value1);
        }
        skipSTACK(3); VALUES1(popSTACK()); /* return seq1 */
      } else {
        STACK_2 = typdescr1;
        # Stackaufbau: seq1, typdescr1, typdescr2-len, typdescr2.
        pushSTACK(STACK_3); funcall(seq_length(typdescr1),1); # (SEQ1-LENGTH seq1)
        if (boundp(STACK_1) && !eql(value1,STACK_1))
          fehler_seqtype_length(STACK_1,value1);
        pushSTACK(value1);
        # Stackaufbau: seq1, typdescr1, typdescr2-len, typdescr2, len.
        pushSTACK(STACK_0); funcall(seq_make(STACK_(1+1)),1); # (SEQ2-MAKE len)
        STACK_2 = value1;
        # Stackaufbau: seq1, typdescr1, seq2, typdescr2, len.
        return_Values copy_seq_onto();
      }
    }
  }
}

LISPFUN(coerced_subseq,2,0,norest,key,2, (kw(start),kw(end)) )
# (SYSTEM::COERCED-SUBSEQ sequence result-type [:start] [:end])
# == (COERCE (SUBSEQ sequence start end) result-type)
# except that only one sequence is allocated, not two.
# Note: result-type = ARRAY and = VECTOR are interpreted to mean GENERAL-VECTOR.
  {
    # Stack layout: sequence, result-type, start, end.
    {
      # Check sequence:
      var object typdescr = get_valid_seq_type(STACK_3);
      pushSTACK(typdescr);
    }
    # Stack layout: sequence, result-type, start, end, typdescr.
    {
      # Check result-type:
      var object typdescr2 = valid_type(STACK_3);
      pushSTACK(typdescr2);
    }
    # Stack layout: sequence, result-type, start, end, typdescr, typdescr2-len, typdescr2.
    # Default value for start is 0:
    start_default_0(STACK_4);
    # Default value for end is (length sequence):
    end_default_len(STACK_3,STACK_6,STACK_2);
    # Check start and end arguments:
    test_start_end(&O(kwpair_start),&STACK_3);
    # Determine result sequence length.
    STACK_3 = I_I_minus_I(STACK_3,STACK_4); # count := (- end start)
    # Stack layout: sequence, result-type, start, count, typdescr, typdescr2-len, typdescr2.
    if (boundp(STACK_1) && !eql(STACK_3,STACK_1))
      fehler_seqtype_length(STACK_1,STACK_3);
    if (eq(seq_type(STACK_2),seq_type(STACK_0))) {
      # Same types of sequences.
      if (eq(STACK_4,Fixnum_0)) {
        # With start = 0.
        var object old_subr_self = subr_self; # current SUBR, GC invariant!
        # Test (= count (length sequence))
        # via (SEQ-ENDTEST sequence (SEQ-INIT-START sequence count)):
        pushSTACK(STACK_6); pushSTACK(STACK_(3+1)); funcall(seq_init_start(STACK_(2+2)),2);
        pushSTACK(STACK_6); pushSTACK(value1); funcall(seq_endtest(STACK_(2+2)),2);
        subr_self = old_subr_self;
        if (!nullp(value1)) {
          # With end = (length sequence).
          # Nothing to do.
          skipSTACK(6); VALUES1(popSTACK()); /* return sequence */
          return;
        }
      }
    }
    # Allocate new sequence.
    pushSTACK(STACK_3); funcall(seq_make(STACK_(0+1)),1); # (SEQ2-MAKE count)
    STACK_5 = STACK_2; STACK_2 = STACK_3; STACK_3 = STACK_0;
    STACK_1 = STACK_6; STACK_0 = STACK_4;
    STACK_4 = value1;
    # Stack layout: sequence, typdescr, seq2, typdescr2, count, sequence, start.
    funcall(seq_init_start(STACK_5),2); pushSTACK(value1);
    # Stack layout: sequence, typdescr, seq2, typdescr2, count, pointer1.
    return_Values copy_seqpart_onto(); # copy, return seq2
  }

LISPFUN(concatenate,1,0,rest,nokey,0,NIL)
# (CONCATENATE result-type {sequence}), CLTL S. 249
  { var object* args_pointer = rest_args_pointer;
    # result-type in Typdescriptor umwandeln:
    { var object type = Before(args_pointer);
      type = valid_type(type);
      BEFORE(args_pointer) = type;
    }
    # args_pointer = Pointer über die Argumente,
    # rest_args_pointer = Pointer über die argcount Sequence-Argumente.
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence}, result-type-len, [STACK].
    # Brauche 2*argcount STACK-Einträge:
    get_space_on_STACK(sizeof(object) * 2*(uintL)argcount);
   {var object* behind_args_pointer = args_end_pointer; # Pointer unter die Argumente
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence}, result-type-len, [behind_args_pointer].
    # Typdescriptoren und Längen bestimmen und im STACK ablegen:
    if (argcount > 0)
      { var object* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,argcount,
          { var object seq = NEXT(ptr); # nächste Sequence
            var object typdescr = get_valid_seq_type(seq);
            pushSTACK(typdescr); # Typdescriptor in den Stack
            pushSTACK(seq); funcall(seq_length(typdescr),1); # (SEQ-LENGTH seq)
            pushSTACK(value1); # Länge in den Stack
          });
      }
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence}, result-type-len,
    #              [behind_args_pointer] {typdescr, len}, [STACK].
    # Längen addieren:
    { var object total_length = Fixnum_0;
      if (argcount > 0)
        { var object* ptr = behind_args_pointer;
          var uintC count;
          dotimespC(count,argcount,
            { NEXT(ptr); # typdescr überspringen
             {var object len = NEXT(ptr); # nächste Länge
              if (!(posfixnump(len)))
                { pushSTACK(len); pushSTACK(S(concatenate));
                  fehler(error,
                         GETTEXT("~: bad length ~")
                        );
                }
              total_length = I_I_plus_I(total_length,len); # total_length = total_length + len
            }});
        }
      { var object result_type_len = Before(behind_args_pointer);
        if (boundp(result_type_len) && !eql(total_length,result_type_len))
          { fehler_seqtype_length(result_type_len,total_length); }
      }
      pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL); # Dummies
      # neue Sequence allozieren:
      {var object* ptr = args_pointer;
       var object typdescr2 = NEXT(ptr);
       pushSTACK(typdescr2);
       pushSTACK(total_length); funcall(seq_make(typdescr2),1); # (SEQ2-MAKE total_length)
       STACK_1 = value1; # =: seq2
    } }
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence}, result-type-len,
    #              [behind_args_pointer] {typdescr, len},
    #              NIL, NIL, seq2, typdescr2, [STACK].
    pushSTACK(NIL); pushSTACK(NIL); # Dummies
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence}, result-type-len,
    #              [behind_args_pointer] {typdescr, len},
    #              NIL, NIL, seq2, typdescr2, NIL, NIL, [STACK].
    pushSTACK(STACK_(3)); funcall(seq_init(STACK_(2+1)),1); # (SEQ-INIT seq2)
    pushSTACK(value1);
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence}, result-type-len,
    #              [behind_args_pointer] {typdescr, len},
    #              NIL, NIL, seq2, typdescr2, NIL, NIL, pointer2, [STACK].
    # Schleife über die argcount Sequences: in seq2 hineinkopieren
    dotimesC(argcount,argcount,
      { STACK_6 = NEXT(rest_args_pointer); # seq1 = nächste Sequence
        STACK_5 = NEXT(behind_args_pointer); # deren typdescr1
        STACK_2 = NEXT(behind_args_pointer); # deren Länge
        pushSTACK(STACK_6); funcall(seq_init(STACK_(5+1)),1); # (SEQ1-INIT seq1)
        STACK_1 = value1; # =: pointer1
        # Stackaufbau: [args_pointer] typdescr2,
        #              [rest_args_pointer] {sequence}, result-type-len,
        #              [behind_args_pointer] {typdescr, len},
        #              seq1, typdescr1, seq2, typdescr2, count,
        #              pointer1, pointer2, [STACK].
        copy_seqpart_into(); # ganze seq1 in die seq2 hineinkopieren
      });
    VALUES1(STACK_4); /* return seq2 */
    set_args_end_pointer(args_pointer); # STACK aufräumen
  }}

# UP: Läuft durch eine Sequence durch und ruft für jedes Element eine Funktion
# auf.
# map_sequence(obj,fun,arg);
# > obj: Objekt, sollte eine Sequence sein
# > fun: Funktion, fun(arg,element) darf GC auslösen
# > arg: beliebiges vorgegebenes Argument
# can trigger GC
  global void map_sequence (object obj, map_sequence_function_t* fun, void* arg);
  global void map_sequence(obj,fun,arg)
    var object obj;
    var map_sequence_function_t* fun;
    var void* arg;
    { var object typdescr = get_valid_seq_type(obj);
      pushSTACK(typdescr);
      pushSTACK(obj);
      pushSTACK(obj); funcall(seq_init(typdescr),1); # (SEQ-INIT obj)
      pushSTACK(value1);
      # Stackaufbau: typdescr, sequence, pointer.
      loop
        { # (SEQ-ENDTEST sequence pointer) :
          pushSTACK(STACK_1); pushSTACK(STACK_1); funcall(seq_endtest(STACK_4),2);
          if (!nullp(value1)) break;
          # (SEQ-ACCESS sequence pointer) :
          pushSTACK(STACK_1); pushSTACK(STACK_1); funcall(seq_access(STACK_4),2);
          # Funktion aufrufen:
          (*fun)(arg,value1);
          # pointer := (SEQ-UPD sequence pointer) :
          pushSTACK(STACK_1); pushSTACK(STACK_1); funcall(seq_upd(STACK_4),2);
          STACK_0 = value1;
        }
      skipSTACK(3);
    }

# UP: führt eine Boolesche Operation mit Prädikat wie SOME oder EVERY aus.
# > Stackaufbau: [args_pointer] ... predicate sequence,
#                [rest_args_pointer] {sequence} [STACK].
# > fun: Routine, die das predicate-Ergebnis abtestet und
#        true liefert (und in value1 ihr Ergebnis hinterlässt),
#        falls vorzeitig herausgesprungen werden soll.
# > argcount: Anzahl der Sequence-Argumente - 1
# > default: Defaultwert am Schluss
# < 1 Wert: wie von fun beim Hinausspringen vorgegeben, oder default.
# < STACK: aufgeräumt (= args_pointer beim Einsprung)
# can trigger GC
  typedef bool seq_boolop_fun (object pred_ergebnis);
  local Values seq_boolop (seq_boolop_fun* boolop_fun,
                           object* args_pointer,
                           object* rest_args_pointer,
                           uintC argcount,
                           object defolt);
  local Values seq_boolop(boolop_fun,args_pointer,rest_args_pointer,argcount,defolt)
    var seq_boolop_fun* boolop_fun;
    var object* args_pointer;
    var object* rest_args_pointer;
    var uintC argcount;
    var object defolt;
    { BEFORE(rest_args_pointer);
      { var object predicate = Before(rest_args_pointer);
        if (!(symbolp(predicate) || functionp(predicate))) {
          pushSTACK(predicate);   # TYPE-ERROR slot DATUM
          pushSTACK(S(function)); # TYPE-ERROR slot EXPECTED-TYPE
          pushSTACK(predicate);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,GETTEXT("~: ~ is not a function"));
        }
      }
      # rest_args_pointer zeigt jetzt über alle argcount+1 Sequence-Argumente
      pushSTACK(defolt); # Defaultwert retten
      # 3*(argcount+1) Plätze auf dem STACK beanspruchen:
      # (2mal für Typdescriptoren und Pointer, 1mal für Funktionsaufruf)
      get_space_on_STACK(sizeof(object)*3*(uintL)(argcount+1));
     {var object* typdescr_pointer = args_end_pointer; # Pointer über die Typdescriptoren
      # Typdescriptoren und je einen Pointer zu jeder der argcount+1
      # Sequences bestimmen und im STACK ablegen:
      { var object* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,argcount+1,
          { var object seq = NEXT(ptr); # nächste Sequence
            var object typdescr = get_valid_seq_type(seq);
            pushSTACK(typdescr); # Typdescriptor im STACK ablegen
            pushSTACK(seq); funcall(seq_init(typdescr),1); # (SEQ-INIT sequence)
            pushSTACK(value1); # Pointer im STACK ablegen
          });
      }
      # Stackaufbau:
      #         [args_pointer] ... predicate,
      #         [rest_args_pointer] {sequence}, default,
      #         [typdescr_pointer] {typdescr, pointer}, [STACK].
      # Schleife: die Funktion aufrufen:
      loop
        { var object* ptr1 = rest_args_pointer;
          var object* ptr2 = typdescr_pointer;
          # ptr1 läuft von oben durch die Sequences durch,
          # ptr2 läuft von oben durch die Typdescr/Pointer durch.
          var uintC count;
          dotimespC(count,argcount+1,
            { var object* sequence_ = &NEXT(ptr1);
              var object* typdescr_ = &NEXT(ptr2);
              var object* pointer_ = &NEXT(ptr2);
              # (SEQ-ENDTEST sequence pointer) :
              pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_endtest(*typdescr_),2);
              # eine der Sequences zu Ende -> große Schleife beenden:
              if (!(nullp(value1))) goto end_with_default;
              # (SEQ-ACCESS sequence pointer) :
              pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_access(*typdescr_),2);
              # als Argument auf den STACK legen:
              pushSTACK(value1);
              # pointer := (SEQ-UPD sequence pointer) :
              pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_upd(*typdescr_),2);
              *pointer_ = value1;
            });
          # Alle Sequences abgearbeitet.
          # (FUNCALL predicate (SEQ-ACCESS sequence pointer) ...) aufrufen:
          { var object* ptr = rest_args_pointer;
            var object predicate = BEFORE(ptr);
            funcall(predicate,argcount+1);
          }
          # Abtestroutine drauf anwenden:
          if ((*boolop_fun)(value1)) goto end_with_value1;
        }
      end_with_default:
        { var object* ptr = typdescr_pointer;
          value1 = BEFORE(ptr); # default als Wert
        }
      end_with_value1:
        mv_count=1; # 1 Wert
        set_args_end_pointer(args_pointer); # STACK aufräumen
    }}

# Hilfsfunktion für MAP:
  local bool boolop_nothing (object pred_ergebnis);
  local bool boolop_nothing(pred_ergebnis)
    var object pred_ergebnis;
    { return false; } # nie vorzeitig zurückkehren

LISPFUN(map,3,0,rest,nokey,0,NIL)
# (MAP result-type function sequence {sequence}), CLTL S. 249
  { var object* args_pointer = rest_args_pointer STACKop 3;
    # args_pointer = Pointer über die Argumente,
    # rest_args_pointer = Pointer über die argcount weiteren Sequence-Argumente.
    var object* result_type_ = &Next(args_pointer);
    # result_type_ zeigt in den STACK, auf result-type.
    if (!(nullp(*result_type_)))
      # allgemeines result-type
      { BEFORE(rest_args_pointer);
        # rest_args_pointer zeigt jetzt über alle argcount+1 Sequence-Argumente
        # 4*(argcount+1) Plätze auf dem STACK beanspruchen:
        # (3mal für Typdescriptoren und Pointer, 1mal für Funktionsaufruf)
        get_space_on_STACK(sizeof(object)*4*(uintL)(argcount+1));
        # result-type überprüfen:
        *result_type_ = valid_type(*result_type_);
       {var object* typdescr_pointer = args_end_pointer; # Pointer über die Typdescriptoren
        # Typdescriptoren und je zwei Pointer zu jeder der argcount+1
        # Sequences bestimmen und im STACK ablegen:
        { var object* ptr = rest_args_pointer;
          var uintC count;
          dotimespC(count,argcount+1,
            { var object* sequence_ = &NEXT(ptr);
              var object seq = *sequence_; # nächste Sequence
              var object typdescr = get_valid_seq_type(seq);
              pushSTACK(typdescr); # Typdescriptor im STACK ablegen
              pushSTACK(seq); funcall(seq_init(typdescr),1); # (SEQ-INIT sequence)
              pushSTACK(value1); # Pointer im STACK ablegen
              pushSTACK(*sequence_); funcall(seq_init(STACK_(1+1)),1); # (SEQ-INIT sequence)
              pushSTACK(value1); # Pointer im STACK ablegen
            });
        }
        # Stackaufbau:
        #         [args_pointer] *result_type_ = typdescr2, function,
        #         [rest_args_pointer] {sequence}, result-type-len,
        #         [typdescr_pointer] {typdescr, pointer, pointer}, [STACK].
        # Minimale Länge aller Sequences bestimmen, indem jeweils mit dem
        # zweiten Pointer durchgelaufen wird:
        pushSTACK(Fixnum_0); # minlength:=0
        loop
          { var object* ptr1 = rest_args_pointer;
            var object* ptr2 = typdescr_pointer;
            # ptr1 läuft von oben durch die Sequences durch,
            # ptr2 läuft von oben durch die Typdescr/Pointer durch.
            var uintC count;
            dotimespC(count,argcount+1,
              { var object* sequence_ = &NEXT(ptr1);
                var object* typdescr_ = &NEXT(ptr2);
                NEXT(ptr2);
               {var object* pointer_ = &NEXT(ptr2);
                # (SEQ-ENDTEST sequence pointer) :
                pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_endtest(*typdescr_),2);
                # eine der Sequences zu Ende -> große Schleife beenden:
                if (!(nullp(value1))) goto end_found;
                # pointer := (SEQ-UPD sequence pointer) :
                pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_upd(*typdescr_),2);
                *pointer_ = value1;
              }});
            # Keine der Sequences war zu Ende.
            STACK_0 = fixnum_inc(STACK_0,1); # minlength := minlength+1
          }
        end_found:
        # STACK_0 = minimale Länge der Sequences
        # Stackaufbau:
        #         [args_pointer] *result_type_ = typdescr2, function,
        #         [rest_args_pointer] {sequence}, result-type-len,
        #         [typdescr_pointer] {typdescr, pointer, pointer},
        #         size [STACK].
        { var object result_type_len = Before(typdescr_pointer);
          if (boundp(result_type_len) && !eql(STACK_0,result_type_len))
            { fehler_seqtype_length(result_type_len,STACK_0); }
        }
        # Neue Sequence der Länge size allozieren:
        pushSTACK(STACK_0); funcall(seq_make(*result_type_),1); # (SEQ2-MAKE size)
        pushSTACK(value1); # seq2 im STACK ablegen
        pushSTACK(STACK_0); funcall(seq_init(*result_type_),1); # (SEQ2-INIT seq2)
        pushSTACK(value1); # pointer2 im STACK ablegen
        # Stackaufbau:
        #         [args_pointer] *result_type_ = typdescr2, function,
        #         [rest_args_pointer] {sequence}, result-type-len,
        #         [typdescr_pointer] {typdescr, pointer, pointer},
        #         size, seq2, pointer2 [STACK].
        # size mal die Funktion aufrufen, Ergebnis in seq2 eintragen:
        until (eq(STACK_2,Fixnum_0)) # count (ein Integer) = 0 -> fertig
          { var object* ptr1 = rest_args_pointer;
            var object* ptr2 = typdescr_pointer;
            # ptr1 läuft von oben durch die Sequences durch,
            # ptr2 läuft von oben durch die Typdescr/Pointer durch.
            var uintC count;
            dotimespC(count,argcount+1,
              { var object* sequence_ = &NEXT(ptr1);
                var object* typdescr_ = &NEXT(ptr2);
                var object* pointer_ = &NEXT(ptr2);
                NEXT(ptr2);
                # (SEQ-ACCESS sequence pointer) :
                pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_access(*typdescr_),2);
                # als Argument auf den STACK legen:
                pushSTACK(value1);
                # pointer := (SEQ-UPD sequence pointer) :
                pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_upd(*typdescr_),2);
                *pointer_ = value1;
              });
            # Alle Sequences abgearbeitet.
            # (FUNCALL function (SEQ-ACCESS sequence pointer) ...) aufrufen:
            funcall(*(result_type_ STACKop -1),argcount+1);
            # (SEQ2-ACCESS-SET seq2 pointer2 ...) ausführen:
            pushSTACK(STACK_(1+0)); pushSTACK(STACK_(0+1)); pushSTACK(value1);
            funcall(seq_access_set(*result_type_),3);
            # pointer2 := (SEQ2-UPD seq2 pointer2) :
            pointer_update(STACK_0,STACK_1,*result_type_);
            # size := (1- size) :
            STACK_2 = fixnum_inc(STACK_2,-1);
          }
        VALUES1(STACK_1); /* return seq2 */
        set_args_end_pointer(args_pointer); # STACK aufräumen
      }}
      else
      # result-type = NIL -> viel einfacher:
      # seq_boolop mit boolop_nothing als Funktion und NIL als (Default-)Wert.
      # Dadurch wird function auf alle Elemente der Sequences angewandt.
      return_Values seq_boolop(&boolop_nothing,args_pointer,rest_args_pointer,argcount,NIL);
  }

LISPFUN(map_into,2,0,rest,nokey,0,NIL)
# (MAP-INTO result-sequence function {sequence}), CLtL2 S. 395
  { var object* args_pointer = rest_args_pointer STACKop 2;
    # args_pointer = Pointer über die Argumente,
    # rest_args_pointer = Pointer über die argcount Sequence-Argumente.
    # 3*argcount Plätze auf dem STACK beanspruchen:
    # (2mal für Typdescriptoren und Pointer, 1mal für Funktionsaufruf)
    get_space_on_STACK(sizeof(object)*3*(uintL)argcount);
    # result-sequence der Einfachheit halber nochmal in den STACK:
    pushSTACK(Next(args_pointer));
   {var object* typdescr_pointer = args_end_pointer; # Pointer über die Typdescriptoren
    # Typdescriptoren und je einen Pointer zu jeder der argcount+1
    # Sequences bestimmen und im STACK ablegen:
    { var object* ptr = rest_args_pointer;
      var uintC count;
      dotimespC(count,argcount+1,
        { var object seq = NEXT(ptr);
          var object typdescr = get_valid_seq_type(seq);
          pushSTACK(typdescr); # Typdescriptor im STACK ablegen
          pushSTACK(seq); funcall(seq_init(typdescr),1); # (SEQ-INIT sequence)
          pushSTACK(value1); # Pointer im STACK ablegen
        });
    }
    # Stackaufbau:
    #         [args_pointer] result-sequence, function,
    #         [rest_args_pointer] {sequence}, result-sequence,
    #         [typdescr_pointer] {typdescr, pointer},
    #         result-typdescr, result-pointer, [STACK].
    # Sooft wie nötig, die Funktion aufrufen, Ergebnis in result-sequence eintragen:
    loop
      { # Test, ob eine weitere Iteration nötig:
        { var object* ptr1 = rest_args_pointer;
          var object* ptr2 = typdescr_pointer;
          # ptr1 läuft von oben durch die Sequences durch,
          # ptr2 läuft von oben durch die Typdescr/Pointer durch.
          var uintC count;
          dotimesC(count,argcount,
            { var object sequence = NEXT(ptr1);
              var object typdescr = NEXT(ptr2);
              var object pointer = NEXT(ptr2);
              # (SEQ-ENDTEST sequence pointer) :
              pushSTACK(sequence); pushSTACK(pointer); funcall(seq_endtest(typdescr),2);
              # eine der Sequences zu Ende -> große Schleife beenden:
              if (!nullp(value1)) goto end_reached;
            });
          # result-sequence zu Ende -> große Schleife beenden:
          { var object sequence = NEXT(ptr1);
            var object typdescr = NEXT(ptr2);
            var object pointer = NEXT(ptr2);
            if (vectorp(sequence))
              { # Bei der result-sequence wird der Fill-Pointer ignoriert.
                # pointer ist der Index als Fixnum.
                if (posfixnum_to_L(pointer) >= array_total_size(sequence))
                  goto end_reached;
              }
              else
              { # (SEQ-ENDTEST sequence pointer) :
                pushSTACK(sequence); pushSTACK(pointer); funcall(seq_endtest(typdescr),2);
                if (!nullp(value1)) goto end_reached;
          }   }
        }
        # Jetzt die Funktion aufrufen:
        { var object* ptr1 = rest_args_pointer;
          var object* ptr2 = typdescr_pointer;
          # ptr1 läuft von oben durch die Sequences durch,
          # ptr2 läuft von oben durch die Typdescr/Pointer durch.
          var uintC count;
          dotimesC(count,argcount,
            { var object* sequence_ = &NEXT(ptr1);
              var object* typdescr_ = &NEXT(ptr2);
              var object* pointer_ = &NEXT(ptr2);
              # (SEQ-ACCESS sequence pointer) :
              pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_access(*typdescr_),2);
              # als Argument auf den STACK legen:
              pushSTACK(value1);
              # pointer := (SEQ-UPD sequence pointer) :
              pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_upd(*typdescr_),2);
              *pointer_ = value1;
            });
          # Alle Sequences abgearbeitet.
          # (FUNCALL function (SEQ-ACCESS sequence pointer) ...) aufrufen:
          funcall(Before(rest_args_pointer),argcount);
          # (SEQ-ACCESS-SET result-sequence result-pointer ...) ausführen:
          { var object* sequence_ = &NEXT(ptr1);
            var object* typdescr_ = &NEXT(ptr2);
            var object* pointer_ = &NEXT(ptr2);
            pushSTACK(*sequence_); pushSTACK(*pointer_); pushSTACK(value1);
            funcall(seq_access_set(*typdescr_),3);
            # pointer := (SEQ-UPD sequence pointer) :
            pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_upd(*typdescr_),2);
            *pointer_ = value1;
        } }
      }
    end_reached:
    { var object result = Next(args_pointer);
      if (vectorp(result) && array_has_fill_pointer_p(result))
        { # (SYS::SET-FILL-POINTER result-sequence result-pointer)
          pushSTACK(result); pushSTACK(STACK_(0+1)); funcall(L(set_fill_pointer),2);
    }   }
    value1 = Next(args_pointer); # result-sequence als Wert
    set_args_end_pointer(args_pointer); # STACK aufräumen
  }}

# Hilfsfunktion für SOME:
  local bool boolop_some (object pred_ergebnis);
  local bool boolop_some(pred_ergebnis)
    var object pred_ergebnis;
    { if (nullp(pred_ergebnis)) # Funktionsergebnis abtesten
        { return false; } # =NIL -> weitersuchen
        else
        { value1 = pred_ergebnis; # /=NIL -> dies als Wert
          return true;
    }   }

LISPFUN(some,2,0,rest,nokey,0,NIL)
# (SOME predicate sequence {sequence}), CLTL S. 250
  { return_Values seq_boolop(&boolop_some,rest_args_pointer STACKop 2,rest_args_pointer,argcount,NIL); }

# Hilfsfunktion für EVERY:
local bool boolop_every (object pred_ergebnis) {
  if (!(nullp(pred_ergebnis))) { /* chech function return value */
    return false; /* /=NIL -> proceed with search */
  } else {
   value1 = pred_ergebnis; /* =NIL -> return this (= NIL) */
   return true;
  }
}

LISPFUN(every,2,0,rest,nokey,0,NIL)
# (EVERY predicate sequence {sequence}), CLTL S. 250
  { return_Values seq_boolop(&boolop_every,rest_args_pointer STACKop 2,rest_args_pointer,argcount,T); }

# Hilfsfunktion für NOTANY:
local bool boolop_notany (object pred_ergebnis) {
  if (nullp(pred_ergebnis)) { /* chech function return value */
    return false; /* =NIL -> proceed with search */
  } else {
    value1 = NIL; /* /=NIL -> return NIL */
    return true;
  }
}

LISPFUN(notany,2,0,rest,nokey,0,NIL)
# (NOTANY predicate sequence {sequence}), CLTL S. 250
  { return_Values seq_boolop(&boolop_notany,rest_args_pointer STACKop 2,rest_args_pointer,argcount,T); }

# Hilfsfunktion für NOTEVERY:
local bool boolop_notevery (object pred_ergebnis) {
  if (!(nullp(pred_ergebnis))) { /* chech function return value */
    return false; /* /=NIL -> proceed with search */
  } else {
    value1 = T; /* =NIL -> return T */
    return true;
  }
}

LISPFUN(notevery,2,0,rest,nokey,0,NIL)
# (NOTEVERY predicate sequence {sequence}), CLTL S. 250
  { return_Values seq_boolop(&boolop_notevery,rest_args_pointer STACKop 2,rest_args_pointer,argcount,NIL); }

# UP: Überprüft das :KEY-Argument
# test_key_arg(stackptr)
# > *(stackptr-4): optionales Argument
# < *(stackptr-4): korrekte KEY-Funktion
  local void test_key_arg (object* stackptr);
  local void test_key_arg(stackptr)
    var object* stackptr;
    { var object key_arg = *(stackptr STACKop -4);
      if (missingp(key_arg))
        *(stackptr STACKop -4) = L(identity); # #'IDENTITY als Default für :KEY
    }

# Anwenden eines :KEY-Arguments
# funcall_key(key);
# > key: Wert des :KEY-Arguments
# > value1: Element einer Sequence
# < value1: (FUNCALL key value1)
# can trigger GC
  #define funcall_key(key)  \
    { var object _key = (key);                                                \
      if (!eq(_key,L(identity))) # :KEY #'IDENTITY ist sehr häufig, Abkürzung \
        { pushSTACK(value1); funcall(_key,1); }                               \
    }

LISPFUN(reduce,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(initial_value)) )
# (REDUCE function sequence [:from-end] [:start] [:end] [:key] [:initial-value]),
# CLTL S. 251, CLTL2 S. 397
  { # Stackaufbau: function, sequence, from-end, start, end, key, initial-value.
    # sequence überprüfen:
    pushSTACK(get_valid_seq_type(STACK_5));
    # Stackaufbau: function, sequence, from-end, start, end, key, initial-value,
    #              typdescr.
    # key überprüfen:
    test_key_arg(&STACK_(5+1));
    # Defaultwert für start ist 0:
    start_default_0(STACK_(3+1));
    # Defaultwert für end ist die Länge der Sequence:
    end_default_len(STACK_(2+1),STACK_(5+1),STACK_0);
    # start- und end-Argumente überprüfen:
    test_start_end(&O(kwpair_start),&STACK_(2+1));
    # start- und end-Argumente subtrahieren und vergleichen:
    { var object count = I_I_minus_I(STACK_(2+1),STACK_(3+1));
      # count = (- end start), ein Integer >=0.
      if (eq(count,Fixnum_0)) # count = 0 ?
        # start und end sind gleich
        { if (!boundp(STACK_(0+1))) /* initial-value supplied? */
            { # nein -> function mit 0 Argumenten aufrufen:
              funcall(STACK_(6+1),0);
            }
            else
            { # ja -> initial-value als Wert:
              VALUES1(STACK_(0+1));
            }
          skipSTACK(7+1);
          return;
        }
      # allgemeiner Fall: start < end, count > 0
      pushSTACK(count);
    }
    # Stackaufbau: function, sequence, from-end, start, end, key, initial-value,
    #              typdescr, count.
    # from-end abfragen:
    if (boundp(STACK_(4+2)) && !nullp(STACK_(4+2)))
      # from-end ist angegeben und /=NIL
      { # Durchlauf-Pointer bestimmen:
        pushSTACK(STACK_(5+2)); pushSTACK(STACK_(2+2+1));
        funcall(seq_fe_init_end(STACK_(1+2)),2); # (SEQ-FE-INIT-END seq end)
        pushSTACK(value1); # =: pointer
        # Stackaufbau: function, sequence, from-end, start, end, key, initial-value,
        #              typdescr, count, pointer.
        # Startwert bestimmen:
        if (!boundp(STACK_(0+3)))
          # initial-value ist nicht angegeben
          { pushSTACK(STACK_(5+3)); pushSTACK(STACK_(0+1));
            funcall(seq_access(STACK_(2+2)),2); # (SEQ-ACCESS seq pointer)
            funcall_key(STACK_(1+3)); # (FUNCALL key (SEQ-ACCESS seq pointer))
            pushSTACK(value1); # =: value
            goto into_fromend_loop;
          }
          else
          # initial-value ist angegeben
          { pushSTACK(STACK_(0+3)); } # value := initial-value
        # Stackaufbau: function, seq, from-end, start, end, key, initial-value,
        #              typdescr, count, pointer, value.
        do { # nächstes value berechnen:
             pushSTACK(STACK_(5+4)); pushSTACK(STACK_(1+1));
             funcall(seq_access(STACK_(3+2)),2); # (SEQ-ACCESS seq pointer)
             funcall_key(STACK_(1+4)); # (FUNCALL key (SEQ-ACCESS seq pointer))
             pushSTACK(value1); pushSTACK(STACK_(0+1));
             funcall(STACK_(6+4+2),2); # (FUNCALL fun (FUNCALL key (SEQ-ACCESS seq pointer)) value)
             STACK_0 = value1; # =: value
             into_fromend_loop:
             # pointer weiterrücken:
             pointer_fe_update(STACK_1,STACK_(5+4),STACK_3);
             # count := (1- count) :
             decrement(STACK_2);
           }
           until (eq(STACK_2,Fixnum_0)); # count (ein Integer) = 0 ?
        VALUES1(popSTACK()); /* return value */
        skipSTACK(7+3);
      }
      else
      # from-end ist nicht angegeben
      { # Durchlauf-Pointer bestimmen:
        pushSTACK(STACK_(5+2)); pushSTACK(STACK_(3+2+1));
        funcall(seq_init_start(STACK_(1+2)),2); # (SEQ-INIT-START seq start)
        pushSTACK(value1); # =: pointer
        # Stackaufbau: function, sequence, from-end, start, end, key, initial-value,
        #              typdescr, count, pointer.
        # Startwert bestimmen:
        if (!boundp(STACK_(0+3)))
          # initial-value ist nicht angegeben
          { pushSTACK(STACK_(5+3)); pushSTACK(STACK_(0+1));
            funcall(seq_access(STACK_(2+2)),2); # (SEQ-ACCESS seq pointer)
            funcall_key(STACK_(1+3)); # (FUNCALL key (SEQ-ACCESS seq pointer))
            pushSTACK(value1); # =: value
            goto into_fromstart_loop;
          }
          else
          # initial-value ist angegeben
          { pushSTACK(STACK_(0+3)); } # value := initial-value
        # Stackaufbau: function, seq, from-end, start, end, key, initial-value,
        #              typdescr, count, pointer, value.
        do { # nächstes value berechnen:
             pushSTACK(STACK_(5+4)); pushSTACK(STACK_(1+1));
             funcall(seq_access(STACK_(3+2)),2); # (SEQ-ACCESS seq pointer)
             funcall_key(STACK_(1+4)); # (FUNCALL key (SEQ-ACCESS seq pointer))
             pushSTACK(STACK_0); pushSTACK(value1);
             funcall(STACK_(6+4+2),2); # (FUNCALL fun value (FUNCALL key (SEQ-ACCESS seq pointer)))
             STACK_0 = value1; # =: value
             into_fromstart_loop:
             # pointer weiterrücken:
             pointer_update(STACK_1,STACK_(5+4),STACK_3);
             # count := (1- count) :
             decrement(STACK_2);
           }
           until (eq(STACK_2,Fixnum_0)); # count (ein Integer) = 0 ?
        VALUES1(popSTACK()); /* return value */
        skipSTACK(7+3);
      }
  }

LISPFUN(fill,2,0,norest,key,2, (kw(start),kw(end)) )
# (FILL sequence item [:start] [:end]), CLTL S. 252
  { # Stackaufbau: sequence, item, start, end.
    # sequence überprüfen:
    pushSTACK(get_valid_seq_type(STACK_3));
    # Stackaufbau: sequence, item, start, end, typdescr.
    # Defaultwert für start ist 0:
    start_default_0(STACK_2);
    # Defaultwert für end ist die Länge der Sequence:
    end_default_len(STACK_1,STACK_4,STACK_0);
    # start- und end-Argumente überprüfen:
    test_start_end(&O(kwpair_start),&STACK_1);
    # start- und end-Argumente subtrahieren:
    STACK_1 = I_I_minus_I(STACK_1,STACK_2); # (- end start), ein Integer >=0
    # Stackaufbau: sequence, item, start, count, typdescr.
    # Durchlauf-Pointer bestimmen:
    pushSTACK(STACK_4); pushSTACK(STACK_(2+1));
    funcall(seq_init_start(STACK_(0+2)),2); # (SEQ-INIT-START sequence start)
    STACK_2 = value1; # =: pointer
    # Stackaufbau: sequence, item, pointer, count, typdescr.
    if (vectorp(STACK_4) && posfixnump(STACK_1)) {
      var uintL count = posfixnum_to_L(STACK_1);
      if (count > 0) {
        var uintL index = posfixnum_to_L(STACK_2);
        if (index+count > vector_length(STACK_4)) {
          subr_self = L(store); fehler_vector_index_range(STACK_4);
        }
        var object dv = array_displace_check(STACK_4,count,&index);
        if (elt_fill(dv,index,count,STACK_3))
          fehler_store(STACK_4,STACK_3);
      }
    } else {
      until (eq(STACK_1,Fixnum_0)) { # count (ein Integer) = 0 -> fertig
        pushSTACK(STACK_4); pushSTACK(STACK_(2+1)); pushSTACK(STACK_(3+2));
        funcall(seq_access_set(STACK_(0+3)),3); # (SEQ-ACCESS-SET sequence pointer item)
        # pointer := (SEQ-UPD sequence pointer) :
        pointer_update(STACK_2,STACK_4,STACK_0);
        # count := (1- count) :
        decrement(STACK_1);
      }
    }
    skipSTACK(4);
    VALUES1(popSTACK()); /* return sequence */
  }

LISPFUN(replace,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (REPLACE sequence1 sequence2 [:start1] [:end1] [:start2] [:end2]),
# CLTL S. 252
  { # Methode (schematisch):
    # Argumente überprüfen.
    # Anzahl der zu kopierenden Elemente bestimmen:
    #   count1 := (- end1 start1), count2 := (- end2 start2).
    #   count1 < count2  ->  count := count1, end2 := (+ start2 count).
    #   count1 > count2  ->  count := count2, #| end1 := (+ start1 count) |# .
    # Nun ist (= count #|(- end1 start1)|# (- end2 start2)).
    # Falls sequence1 und sequence2 EQ sind, die Indexbereiche sich
    # überschneiden (also nicht (or (>= start2 end1) (>= start1 end2)) gilt)
    # und nach oben kopiert werden soll (also (< start2 start1) gilt):
    #   Das Source-Stück aus sequence2 herauskopieren:
    #   (unless (or #|(>= start2 end1)|# (>= start1 end2) (>= start2 start1))
    #     (psetq sequence2 (subseq sequence2 start2 end2)
    #            start2    0
    #         #| end2      count |#
    #   ) )
    # Dann elementweise kopieren: für i=0,1,...
    #   (setf (elt sequence1 (+ start1 i)) (elt sequence2 (+ start2 i))).
    # Stackaufbau: sequence1, sequence2, start1, end1, start2, end2.
    # sequence1 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_5));
    # sequence1 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_(4+1)));
    # Stackaufbau: sequence1, sequence2, start1, end1, start2, end2,
    #              typdescr1, typdescr2.
    # Defaultwert für start1 ist 0:
    start_default_0(STACK_(3+2));
    # Defaultwert für end1 ist die Länge von sequence1:
    end_default_len(STACK_(2+2),STACK_(5+2),STACK_1);
    # Defaultwert für start2 ist 0:
    start_default_0(STACK_(1+2));
    # Defaultwert für end2 ist die Länge von sequence2:
    end_default_len(STACK_(0+2),STACK_(4+2),STACK_0);
    # start- und end-Argumente überprüfen:
    test_start_end(&O(kwpair_start1),&STACK_(2+2));
    test_start_end(&O(kwpair_start2),&STACK_(0+2));
    # count1 bestimmen:
    STACK_(2+2) = I_I_minus_I(STACK_(2+2),STACK_(3+2)); # (- end1 start1) = count1
    # Stackaufbau: sequence1, sequence2, start1, count1, start2, end2,
    #              typdescr1, typdescr2.
    # count2 bestimmen:
   {var object count2 = I_I_minus_I(STACK_(0+2),STACK_(1+2)); # (- end2 start2)
    # count bestimmen und evtl. end2 herabsetzen:
    if (I_I_comp(STACK_(2+2),count2)<0) # count1 < count2 ?
      { # ja -> count1 ist das Minimum
        STACK_(0+2) = I_I_plus_I(STACK_(1+2),STACK_(2+2)); # end2 := (+ start2 count1)
      }
      else
      { # nein -> count2 ist das Minimum
        STACK_(2+2) = count2; # count := count2
   }  }
    # Stackaufbau: sequence1, sequence2, start1, count, start2, end2,
    #              typdescr1, typdescr2.
    # Falls beide Sequences dieselben sind und die Bereiche sich
    # überschneiden, muss die Source erst herauskopiert werden:
    if (eq(STACK_(5+2),STACK_(4+2)) # (eq sequence1 sequence2)
        && (I_I_comp(STACK_(1+2),STACK_(3+2))<0) # (< start2 start1)
        && (I_I_comp(STACK_(3+2),STACK_(0+2))<0) # (< start1 end2)
       )
      { # Stück aus sequence2 herauskopieren:
        pushSTACK(STACK_(4+2)); pushSTACK(STACK_(1+2+1)); pushSTACK(STACK_(0+2+2));
        pushSTACK(STACK_(0+3)); subseq(); # (SUBSEQ sequence2 start2 end2)
        STACK_(4+2) = value1; # =: sequence2
        # Indizes adjustieren:
        STACK_(1+2) = Fixnum_0; # start2 := 0
      }
    # Stackaufbau: sequence1, sequence2, start1, count, start2, dummy,
    #              typdescr1, typdescr2.
    # Argumente für copy_seqpart_into auf den Stack legen:
    pushSTACK(STACK_(4+2+0)); pushSTACK(STACK_(0+1));
    pushSTACK(STACK_(5+2+2)); pushSTACK(STACK_(1+3));
    pushSTACK(STACK_(2+2+4));
    # Stackaufbau: sequence1, sequence2, start1, count, start2, dummy,
    #              typdescr1, typdescr2,
    #              sequence2, typdescr2, sequence1, typdescr1, count.
    pushSTACK(STACK_4); pushSTACK(STACK_(1+2+5+1));
    funcall(seq_init_start(STACK_(3+2)),2); # (SEQ-INIT-START sequence2 start2)
    pushSTACK(value1); # =: pointer2
    pushSTACK(STACK_(2+1)); pushSTACK(STACK_(3+2+5+1+1));
    funcall(seq_init_start(STACK_(1+1+2)),2); # (SEQ-INIT-START sequence1 start1)
    pushSTACK(value1); # =: pointer1
    # Stackaufbau: sequence1, sequence2, start1, count, start2, dummy,
    #              typdescr1, typdescr2,
    #              sequence2, typdescr2, sequence1, typdescr1, count,
    #              pointer2, pointer1.
    copy_seqpart_into(); # kopiere von sequence2 nach sequence1
    skipSTACK(5+2+5+2);
    VALUES1(popSTACK()); /* return sequence1 */
  }

# Unterprogramm zum Ausführen des Tests :TEST
# up_test(stackptr,x)
# > *(stackptr-5): die Testfunktion
# > *(stackptr+1): das zu vergleichende Item
# > x: Argument
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
  local bool up_test (const object* stackptr, object x);
  local bool up_test(stackptr,x)
    var const object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (funcall testfun item x) ausführen:
      pushSTACK(*(stackptr STACKop 1)); # item
      pushSTACK(x); # x
      funcall(*(stackptr STACKop -5),2);
      if (nullp(value1)) return false; else return true;
    }

# Unterprogramm zum Ausführen des Tests :TEST-NOT
# up_test_not(stackptr,x)
# > *(stackptr-6): die Testfunktion
# > *(stackptr+1): das zu vergleichende Item
# > x: Argument
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
  local bool up_test_not (const object* stackptr, object x);
  local bool up_test_not(stackptr,x)
    var const object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (not (funcall testfun item x)) ausführen:
      pushSTACK(*(stackptr STACKop 1)); # item
      pushSTACK(x); # x
      funcall(*(stackptr STACKop -6),2);
      if (nullp(value1)) return true; else return false;
    }

# Unterprogramm zum Ausführen des Tests -IF
# up_if(stackptr,x)
# > *(stackptr+1): das Testprädikat
# > x: Argument
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
  local bool up_if (const object* stackptr, object x);
  local bool up_if(stackptr,x)
    var const object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (funcall predicate x) ausführen:
      pushSTACK(x); funcall(*(stackptr STACKop 1),1);
      if (nullp(value1)) return false; else return true;
    }

# Unterprogramm zum Ausführen des Tests -IF-NOT
# up_if_not(stackptr,x)
# > *(stackptr+1): das Testprädikat
# > x: Argument
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
  local bool up_if_not (const object* stackptr, object x);
  local bool up_if_not(stackptr,x)
    var const object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (not (funcall predicate x)) ausführen:
      pushSTACK(x); funcall(*(stackptr STACKop 1),1);
      if (nullp(value1)) return true; else return false;
    }

# UP: Überprüft das :COUNT-Argument
# > STACK_1: optionales Argument
# > subr_self: Aufrufer (ein SUBR)
# < STACK_1: korrekter COUNT-Wert: NIL oder ein Integer >=0
  local void test_count_arg (void);
  local void test_count_arg()
    { var object count = STACK_1;
      if (!boundp(count))
        { STACK_1 = NIL; return; } # Defaultwert NIL
      # COUNT-Argument muss NIL oder ein Integer >= 0 sein:
      if (nullp(count)) # NIL is OK
        return;
      if (integerp(count)) {
        if (positivep(count))
          return;
        if (!nullpSv(sequence_count_ansi)) /* if *SEQUENCE-COUNT-ANSI* */
          { STACK_1 = Fixnum_0; return; } # treat negative integers as 0
      }
      fehler_posint(TheSubr(subr_self)->name,S(Kcount),count);
    }

# Fehler, wenn beide :TEST, :TEST-NOT - Argumente angegeben wurden.
# fehler_both_tests();
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_both_tests, (void)) {
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,
           GETTEXT("~: Must not specify both arguments to :TEST and :TEST-NOT")
          );
  }

# UP: Überprüft die :TEST, :TEST-NOT - Argumente
# test_test_args(stackptr)
# > stackptr: Pointer in den STACK
# > *(stackptr-5): :TEST-Argument
# > *(stackptr-6): :TEST-NOT-Argument
# > subr_self: Aufrufer (ein SUBR)
# < *(stackptr-5): verarbeitetes :TEST-Argument
# < *(stackptr-6): verarbeitetes :TEST-NOT-Argument
# < up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#       > stackptr: derselbe Pointer in den Stack, *(stackptr+1) = item,
#         *(stackptr-5) = :test-Argument, *(stackptr-6) = :test-not-Argument,
#       > x: Argument
#       < true, falls der Test erfüllt ist, false sonst.
  # up_function sei der Typ der Adresse einer solchen Testfunktion:
  typedef bool (*up_function) (const object* stackptr, object x);
  local up_function test_test_args (object* stackptr);
  local up_function test_test_args(stackptr)
    var object* stackptr;
    { var object test_arg = *(stackptr STACKop -5);
      if (!boundp(test_arg)) { test_arg=NIL; }
      # test_arg ist das :TEST-Argument
     {var object test_not_arg = *(stackptr STACKop -6);
      if (!boundp(test_not_arg)) { test_not_arg=NIL; }
      # test_not_arg ist das :TEST-NOT-Argument
      if (nullp(test_not_arg))
        # :TEST-NOT wurde nicht angegeben
        { if (nullp(test_arg))
            *(stackptr STACKop -5) = L(eql); # #'EQL als Default für :TEST
          return(&up_test);
        }
        # :TEST-NOT wurde angegeben
        { if (nullp(test_arg))
            return(&up_test_not);
          else
            fehler_both_tests();
    }}  }

# UP: bereitet eine Sequence-Operation mit Test vor.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... [STACK]
#   genauer:
#     ... item sequence [stackptr] from-end start end key test test-not ... [STACK]
#     oder
#     ... test sequence [stackptr] from-end start end key ... [STACK]
#     oder
#     ... test-not sequence [stackptr] from-end start end key ... [STACK]
# > stackptr: Pointer in den Stack
# > subr_self: Aufrufer (ein SUBR)
# < STACK: wird um 1 erniedrigt
# < STACK_0: typdescr zu sequence
  local void seq_prepare_testop (object* stackptr);
  local void seq_prepare_testop(stackptr)
    var object* stackptr;
    { # sequence überprüfen, typdescr auf den Stack:
      pushSTACK(get_valid_seq_type(*(stackptr STACKop 0)));
      # key überprüfen:
      test_key_arg(stackptr);
      # Defaultwert für from-end ist NIL:
      default_NIL(*(stackptr STACKop -1));
      # Defaultwert für start ist 0:
      start_default_0(*(stackptr STACKop -2));
      # Defaultwert für end ist NIL:
      default_NIL(*(stackptr STACKop -3));
      # start und end überprüfen:
      test_start_end_1(&O(kwpair_start),&*(stackptr STACKop -3));
    }

# UP: führt eine Sequence-Filter-Operation aus.
# Eine Sequence wird durchlaufen und dabei in einem Bit-Vektor abgespeichert,
# welche Elemente dem Test genügen. Dann wird eine Routine aufgerufen, die
# den Rest erledigt.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... count typdescr [STACK]
# > stackptr: Pointer in den Stack
# > up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#           > stackptr: derselbe Pointer in den Stack,
#           > x: Argument
#           < true, falls der Test erfüllt ist, false sonst.
# > help_fun: Adresse einer Hilfsroutine, die den Rest erledigt.
#   Spezifiziert durch:
#       > stackptr: Pointer in den Stack,
#         *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
#       > STACK_2: typdescr,
#       > STACK_1: Länge l der Sequence,
#       > STACK_0: Bit-Vektor bv,
#       > bvl: Länge des Bit-Vektors (= end - start),
#       > dl: Anzahl der im Bit-Vektor gesetzten Bits,
#       < ergebnis: Ergebnis
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# can trigger GC
  # help_function sei der Typ der Adresse einer solchen Hilfsfunktion:
  typedef object (*help_function) (object* stackptr, uintL bvl, uintL dl);
  local Values seq_filterop (object* stackptr, up_function up_fun, help_function help_fun);
  local Values seq_filterop(stackptr,up_fun,help_fun)
    var object* stackptr;
    var up_function up_fun;
    var help_function help_fun;
    { # COUNT-Argument muss NIL oder ein Integer >= 0 sein:
      test_count_arg();
     {var object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
      # l = (SEQ-LENGTH sequence) bestimmen:
      pushSTACK(*(stackptr STACKop 0)); # sequence
      funcall(seq_length(STACK_(0+1)),1); # (SEQ-LENGTH sequence)
      pushSTACK(value1); # l in den Stack
      subr_self = old_subr_self;
     }
      # Defaultwert für END ist l:
      if (nullp(*(stackptr STACKop -3))) # end=NIL ?
        { *(stackptr STACKop -3) = STACK_0; # ja -> end:=l
          # Dann nochmals start und end überprüfen:
          test_start_end(&O(kwpair_start),&*(stackptr STACKop -3));
        }
      # Nun sind alle Argumente überprüft.
      pushSTACK(*(stackptr STACKop 0)); # sequence
      pushSTACK(*(stackptr STACKop -4)); # key
      # (- end start) bestimmen und neuen Bitvektor allozieren:
     {var uintL bvl; # Bitvektor-Länge
      var uintL dl = 0; # Anzahl der im Bitvektor gesetzten Bits
      { var object bvsize = I_I_minus_I(*(stackptr STACKop -3),*(stackptr STACKop -2));
        # bvsize = (- end start), ein Integer >=0
        if (!(posfixnump(bvsize))) # Fixnum?
          { pushSTACK(*(stackptr STACKop 0)); # sequence
            pushSTACK(TheSubr(subr_self)->name);
            fehler(error,
                   GETTEXT("~: sequence ~ is too long")
                  );
          }
        bvl = posfixnum_to_L(bvsize); # Länge des Bitvektors als Longword
      }
      pushSTACK(allocate_bit_vector_0(bvl)); # neuer Bitvektor bv
      # Stackaufbau: ... count, typdescr,
      #              l, sequence, key, bv [STACK].
      if (!(nullp(*(stackptr STACKop -1)))) # from-end abfragen
        # from-end ist angegeben
        { pushSTACK(STACK_2); # sequence
          pushSTACK(*(stackptr STACKop -3)); # end
          funcall(seq_fe_init_end(STACK_(0+4+2)),2); # (SEQ-FE-INIT-END sequence end)
          pushSTACK(value1); # =: pointer
          pushSTACK(STACK_(1+4+1)); # countdown := count
          # Stackaufbau: ... count, typdescr,
          #              l, sequence, key, bv,
          #              pointer, countdown [STACK].
         {var uintL bvi = bvl; # Schleife bvl mal durchlaufen
          until (bvi==0)
            { bvi--;
              if (!(nullp(STACK_(1+4+2))) && eq(STACK_0,Fixnum_0))
                # count/=NIL und countdown=0 -> Schleife kann abgebrochen werden
                break;
              # nächstes Element abtesten:
              pushSTACK(STACK_(2+2)); # sequence
              pushSTACK(STACK_(1+1)); # pointer
              funcall(seq_access(STACK_(0+4+2+2)),2); # (SEQ-ACCESS sequence pointer)
              funcall_key(STACK_(1+2)); # (FUNCALL key ...)
              if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                # Test erfüllt
                { sbvector_bset(STACK_(0+2),bvi); # (setf (sbit bv bvi) 1)
                  dl++; # dl := dl+1, ein gesetztes Bit mehr
                  if (!(nullp(STACK_(1+4+2)))) # falls count/=NIL:
                    { decrement(STACK_0); } # (decf countdown)
                }
              # pointer weiterrücken:
              pointer_fe_update(STACK_1,STACK_(2+2),STACK_(0+4+2));
        }}  }
        else
        # from-end ist nicht angegeben
        { pushSTACK(STACK_2); # sequence
          pushSTACK(*(stackptr STACKop -2)); # start
          funcall(seq_init_start(STACK_(0+4+2)),2); # (SEQ-INIT-START sequence start)
          pushSTACK(value1); # =: pointer
          pushSTACK(STACK_(1+4+1)); # countdown := count
          # Stackaufbau: ... count, typdescr,
          #              l, sequence, key, bv,
          #              pointer, countdown [STACK].
         {var uintL bvi = 0; # Schleife bvl mal durchlaufen
          until (bvi==bvl)
            { if (!(nullp(STACK_(1+4+2))) && eq(STACK_0,Fixnum_0))
                # count/=NIL und countdown=0 -> Schleife kann abgebrochen werden
                break;
              # nächstes Element abtesten:
              pushSTACK(STACK_(2+2)); # sequence
              pushSTACK(STACK_(1+1)); # pointer
              funcall(seq_access(STACK_(0+4+2+2)),2); # (SEQ-ACCESS sequence pointer)
              funcall_key(STACK_(1+2)); # (FUNCALL key ...)
              if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                # Test erfüllt
                { sbvector_bset(STACK_(0+2),bvi); # (setf (sbit bv bvi) 1)
                  dl++; # dl := dl+1, ein gesetztes Bit mehr
                  if (!(nullp(STACK_(1+4+2)))) # falls count/=NIL:
                    { decrement(STACK_0); } # (decf countdown)
                }
              # pointer weiterrücken:
              pointer_update(STACK_1,STACK_(2+2),STACK_(0+4+2));
              bvi++;
        }}  }
      skipSTACK(2); # pointer und countdown vergessen
      # Stackaufbau: ... count, typdescr,
      #              l, sequence, key, bv [STACK].
      STACK_2 = STACK_0; skipSTACK(2); # bv hochschieben
      # Stackaufbau: ... count, typdescr, l, bv [STACK].
      VALUES1((*help_fun)(stackptr,bvl,dl));
      skipSTACK(2); # l und bv vergessen
    }}

# UP: Hilfsroutine für REMOVE-Funktionen.
# Bildet zu einer Sequence eine neue Sequence, in der genau die Elemente
# fehlen, die in einem Bitvektor markiert sind.
# > stackptr: Pointer in den Stack,
#   *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
# > STACK_2: typdescr,
# > STACK_1: Länge l der Sequence,
# > STACK_0: Bit-Vektor bv,
# > bvl: Länge des Bit-Vektors (= end - start),
# > dl: Anzahl der im Bit-Vektor gesetzten Bits,
# < ergebnis: Ergebnis
# can trigger GC
  local object remove_help (object* stackptr, uintL bvl, uintL dl);
  local object remove_help(stackptr,bvl,dl)
    var object* stackptr;
    var uintL bvl;
    var uintL dl;
    { # dl=0 -> sequence unverändert zurückgeben:
      if (dl==0) { return *(stackptr STACKop 0); }
      # neue Sequence allozieren:
      pushSTACK(I_I_minus_I(STACK_1,fixnum(dl))); # (- l dl)
      funcall(seq_make(STACK_(2+1)),1); # (SEQ-MAKE (- l dl))
      pushSTACK(value1);
      # Stackaufbau: typdescr, l, bv, sequence2.
      pushSTACK(*(stackptr STACKop 0)); # sequence
      pushSTACK(STACK_(3+1)); # typdescr
      pushSTACK(STACK_(0+2)); # sequence2
      pushSTACK(STACK_(3+3)); # typdescr
      pushSTACK(*(stackptr STACKop -2)); # start
      # Stackaufbau: typdescr, l, bv, sequence2,
      #              seq1, typdescr1, seq2, typdescr2, start.
      pushSTACK(STACK_4); funcall(seq_init(STACK_(3+1)),1); # (SEQ-INIT sequence)
      pushSTACK(value1); # =: pointer1
      pushSTACK(STACK_(2+1)); funcall(seq_init(STACK_(1+1+1)),1); # (SEQ-INIT sequence2)
      pushSTACK(value1); # =: pointer2
      # Stackaufbau: typdescr, l, bv, sequence2,
      #              seq1, typdescr1, seq2, typdescr2, start,
      #              pointer1, pointer2.
      { # Vorderes Teilstück:
        # Elemente mit Index <start von sequence1 nach sequence2
        # unverändert übertragen:
        copy_seqpart_into();
      }
      { # Mittleres Teilstück: sieben.
        var uintL bvi = 0;
        until (bvi==bvl)
          { if (!(sbvector_btst(STACK_(1+5+2),bvi))) # (sbit bv bvi) abfragen
              # Bit ist nicht gesetzt, also Element übernehmen
              { pushSTACK(STACK_(4+2)); pushSTACK(STACK_(1+1));
                funcall(seq_access(STACK_(3+2+2)),2); # (SEQ-ACCESS seq1 pointer1)
                pushSTACK(STACK_(2+2)); pushSTACK(STACK_(0+1)); pushSTACK(value1);
                funcall(seq_access_set(STACK_(1+2+3)),3); # (SEQ-ACCESS-SET seq2 pointer2 ...)
                # pointer2 := (SEQ-UPD seq2 pointer2) :
                pointer_update(STACK_0,STACK_(2+2),STACK_(1+2));
              }
            # pointer1 := (SEQ-UPD seq1 pointer1) :
            pointer_update(STACK_1,STACK_(4+2),STACK_(3+2));
            bvi++;
      }   }
      { # Hinteres Teilstück:
        # Elemente mit Index >=end von sequence1 nach sequence2
        # unverändert übertragen:
        STACK_(0+2) = I_I_minus_I(STACK_(2+5+2),*(stackptr STACKop -3)); # (- l end)
        copy_seqpart_into();
      }
      skipSTACK(5+2);
      return popSTACK(); # sequence2 als Ergebnis
    }

# UP: Hilfsroutine für DELETE-Funktionen.
# Entfernt aus einer Sequence genau die Elemente, die in einem Bitvektor
# markiert sind.
# > stackptr: Pointer in den Stack,
#   *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
# > STACK_2: typdescr,
# > STACK_1: Länge l der Sequence,
# > STACK_0: Bit-Vektor bv,
# > bvl: Länge des Bit-Vektors (= end - start),
# > dl: Anzahl der im Bit-Vektor gesetzten Bits,
# < ergebnis: Ergebnis
# can trigger GC
  local object delete_help (object* stackptr, uintL bvl, uintL dl);
  local object delete_help(stackptr,bvl,dl)
    var object* stackptr;
    var uintL bvl;
    var uintL dl;
    { # dl=0 -> sequence unverändert zurückgeben:
      if (dl==0) { return *(stackptr STACKop 0); }
     {var object type = seq_type(STACK_2);
      if (eq(type,S(list))) # Typ LIST ?
        { # Noch überprüfen, ob sequence wirklich eine Liste ist.
          # Wegen l >= dl > 0 ist zu testen, ob sequence ein Cons ist.
          if (mconsp(*(stackptr STACKop 0)))
            { # Listen speziell behandeln:
              var object whole_list = *(stackptr STACKop 0); # ganze Liste
              var object* list_ = &whole_list;
              var object list = *list_;
              # Stets list = *list_.
              # Vorderes Teilstück:
              # start mal mit list:=Cdr(list) weiterrücken:
              { var uintL count;
                dotimesL(count,posfixnum_to_L(*(stackptr STACKop -2)),
                  { list_ = &Cdr(list); list = *list_; });
              }
              # Mittleres Teilstück:
              # bvl mal ein Bit abfragen und evtl. ein Cons streichen:
              { var uintL bvi = 0;
                until (bvi==bvl)
                  { if (sbvector_btst(STACK_0,bvi)) # (sbit bv bvi) abfragen
                      # Bit ist =1 -> Cons bei list herausnehmen:
                      { *list_ = list = Cdr(list); }
                      else
                      # Bit ist =0 -> nur weiterrücken:
                      { list_ = &Cdr(list); list = *list_; }
                    bvi++;
              }   }
              return whole_list; # modifizierte Liste als Ergebnis
            }
            else
            goto other;
        }
      elif (eq(type,S(vector)) || eq(type,S(string)) || eq(type,S(bit_vector)) || posfixnump(type))
        # Typ [GENERAL-]VECTOR, STRING, BIT-VECTOR, Byte-VECTOR
        { # Noch überprüfen, ob sequence wirklich ein Vektor ist.
          var object sequence = *(stackptr STACKop 0);
          if (!(vectorp(sequence))) { goto other; }
          # Bei Arrays ohne Fill-Pointer kann man nichts Spezielles machen:
          if (!(array_has_fill_pointer_p(sequence))) { goto other; }
          # sequence ist ein Vektor mit Fill-Pointer.
          # Elemente zusammenschieben und dann Fill-Pointer herabsetzen:
          pushSTACK(sequence); # sequence
          pushSTACK(*(stackptr STACKop -2)); # i := start
          pushSTACK(STACK_0); # j := i
          # Stackaufbau: typdescr, l, bv, sequence, i, j.
          # j = Source-Index, i = Destination-Index, start <= i <= j .
          # Mittleres Teilstück:
          { var uintL bvi = 0;
            until (bvi==bvl)
              { if (!(sbvector_btst(STACK_3,bvi))) # (sbit bv bvi) abfragen
                  # Bit gelöscht -> Element übertragen:
                  { # (setf (aref sequence i) (aref sequence j)) :
                    pushSTACK(STACK_2); pushSTACK(STACK_(0+1));
                    funcall(L(aref),2); # (AREF sequence j)
                    pushSTACK(STACK_2); pushSTACK(STACK_(1+1)); pushSTACK(value1);
                    funcall(L(store),3); # (SYS::STORE sequence i ...)
                    # i:=i+1 :
                    STACK_1 = fixnum_inc(STACK_1,1);
                  }
                # j:=j+1 :
                STACK_0 = fixnum_inc(STACK_0,1);
                bvi++;
          }   }
          # Hinteres Teilstück:
          { until (eq(STACK_0,STACK_4)) # solange bis j = l (beides Fixnums)
              # Element übertragen:
              { # (setf (aref sequence i) (aref sequence j)) :
                pushSTACK(STACK_2); pushSTACK(STACK_(0+1));
                funcall(L(aref),2); # (AREF sequence j)
                pushSTACK(STACK_2); pushSTACK(STACK_(1+1)); pushSTACK(value1);
                funcall(L(store),3); # (SYS::STORE sequence i ...)
                # i:=i+1 :
                STACK_1 = fixnum_inc(STACK_1,1);
                # j:=j+1 :
                STACK_0 = fixnum_inc(STACK_0,1);
          }   }
          skipSTACK(1);
          # Stackaufbau: typdescr, l, bv, sequence, i.
          # (setf (fill-pointer sequence) i) :
          funcall(L(set_fill_pointer),2); # (SYS::SET-FILL-POINTER sequence i)
          # Stackaufbau: typdescr, l, bv.
          return *(stackptr STACKop 0); # sequence mit modifiziertem Fill-Pointer
        }
      other: # sonstige Sequences
        return remove_help(stackptr,bvl,dl); # DELETE wie REMOVE behandeln
    }}

LISPFUN(remove,2,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
# (REMOVE item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not] [:count]),
# CLTL S. 253
  { var object* stackptr = &STACK_7;
    var up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,up_fun,&remove_help); # Filtern
    skipSTACK(2+7+1);
  }

LISPFUN(remove_if,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (REMOVE-IF test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 253
  { var object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if,&remove_help); # Filtern
    skipSTACK(2+5+1);
  }

LISPFUN(remove_if_not,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (REMOVE-IF-NOT test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 253
  { var object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if_not,&remove_help); # Filtern
    skipSTACK(2+5+1);
  }

LISPFUN(delete,2,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
# (DELETE item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not] [:count]),
# CLTL S. 254
  { var object* stackptr = &STACK_7;
    var up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,up_fun,&delete_help); # Filtern
    skipSTACK(2+7+1);
  }

LISPFUN(delete_if,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (DELETE-IF test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 254
  { var object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if,&delete_help); # Filtern
    skipSTACK(2+5+1);
  }

LISPFUN(delete_if_not,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (DELETE-IF-NOT test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 254
  { var object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if_not,&delete_help); # Filtern
    skipSTACK(2+5+1);
  }

# Unterprogramm zum Ausführen des Tests :TEST
# up2_test(stackptr,x,y)
# > *(stackptr-5): die Testfunktion
# > x,y: Argumente
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
  local bool up2_test (const object* stackptr, object x, object y);
  local bool up2_test(stackptr,x,y)
    var const object* stackptr;
    var object x;
    var object y;
    { # ein (funcall testfun x y) ausführen:
      pushSTACK(x); # x
      pushSTACK(y); # y
      funcall(*(stackptr STACKop -5),2);
      if (nullp(value1)) return false; else return true;
    }

# Unterprogramm zum Ausführen des Tests :TEST-NOT
# up2_test_not(stackptr,x,y)
# > *(stackptr-6): die Testfunktion
# > x,y: Argumente
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
  local bool up2_test_not (const object* stackptr, object x, object y);
  local bool up2_test_not(stackptr,x,y)
    var const object* stackptr;
    var object x;
    var object y;
    { # ein (not (funcall testfun x y)) ausführen:
      pushSTACK(x); # x
      pushSTACK(y); # y
      funcall(*(stackptr STACKop -6),2);
      if (nullp(value1)) return true; else return false;
    }

# UP: Überprüft die :TEST, :TEST-NOT - Argumente
# test_test2_args(stackptr)
# > stackptr: Pointer in den STACK
# > *(stackptr-5): :TEST-Argument
# > *(stackptr-6): :TEST-NOT-Argument
# > subr_self: Aufrufer (ein SUBR)
# < *(stackptr-5): verarbeitetes :TEST-Argument
# < *(stackptr-6): verarbeitetes :TEST-NOT-Argument
# < up2_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#       > stackptr: derselbe Pointer in den Stack,
#         *(stackptr-5) = :test-Argument, *(stackptr-6) = :test-not-Argument,
#       > x,y: Argumente
#       < true, falls der Test erfüllt ist, false sonst.
  # up2_function sei der Typ der Adresse einer solchen Testfunktion:
  typedef bool (*up2_function) (const object* stackptr, object x, object y);
  local up2_function test_test2_args (object* stackptr);
  local up2_function test_test2_args(stackptr)
    var object* stackptr;
    { var object test_arg = *(stackptr STACKop -5);
      if (!boundp(test_arg)) { test_arg=NIL; }
      # test_arg ist das :TEST-Argument
     {var object test_not_arg = *(stackptr STACKop -6);
      if (!boundp(test_not_arg)) { test_not_arg=NIL; }
      # test_not_arg ist das :TEST-NOT-Argument
      if (nullp(test_not_arg))
        # :TEST-NOT wurde nicht angegeben
        { if (nullp(test_arg))
            *(stackptr STACKop -5) = L(eql); # #'EQL als Default für :TEST
          return(&up2_test);
        }
        # :TEST-NOT wurde angegeben
        { if (nullp(test_arg))
            return(&up2_test_not);
          else
            fehler_both_tests();
    }}  }

# UP: führt eine Sequence-Duplicates-Operation aus.
# seq_duplicates(help_fun)
# Eine Sequence wird durchlaufen und dabei in einem Bit-Vektor abgespeichert,
# welche Elemente doppelt vorkommen. Dann wird eine Routine aufgerufen, die
# den Rest erledigt.
# > Stackaufbau:
#     sequence from-end start end key test test-not [STACK]
# > help_fun: Adresse einer Hilfsroutine, die den Rest erledigt.
#     Spezifiziert durch:
#       > stackptr: Pointer in den Stack,
#         *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
#       > STACK_2: typdescr,
#       > STACK_1: Länge der Sequence,
#       > STACK_0: Bit-Vektor bv,
#       > bvl: Länge des Bit-Vektors (= end - start),
#       > dl: Anzahl der im Bit-Vektor gesetzten Bits,
#       < ergebnis: Ergebnis
#       can trigger GC
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# can trigger GC
  local Values seq_duplicates (help_function help_fun);
  local Values seq_duplicates(help_fun)
    var help_function help_fun;
    { var object* stackptr = &STACK_6;
      # Stackaufbau:
      #   sequence [stackptr], from-end, start, end, key, test, test-not.
      # sequence überprüfen:
      { var object sequence = *(stackptr STACKop 0);
        pushSTACK(get_valid_seq_type(sequence)); # typdescr auf den Stack
      }
      # Stackaufbau:
      #   sequence [stackptr], from-end, start, end, key, test, test-not,
      #   typdescr.
      # :test und :test-not überprüfen:
     {var up2_function up2_fun = test_test2_args(stackptr);
      # key überprüfen:
      test_key_arg(stackptr);
      # Defaultwert für from-end ist NIL:
      default_NIL(*(stackptr STACKop -1));
      # Defaultwert für start ist 0:
      start_default_0(*(stackptr STACKop -2));
      # Defaultwert für end ist nil:
      default_NIL(*(stackptr STACKop -3));
      # start und end überprüfen:
      test_start_end_1(&O(kwpair_start),&*(stackptr STACKop -3));
      # Länge der Sequence bestimmen:
      { var object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
        pushSTACK(STACK_(6+1)); # sequence
        funcall(seq_length(STACK_(0+1)),1); # (SEQ-LENGTH sequence)
        pushSTACK(value1); # l
        subr_self = old_subr_self;
      }
      # Stackaufbau:
      #   sequence [stackptr], from-end, start, end, key, test, test-not,
      #   typdescr, l.
      # Defaultwert für end ist l = (length sequence):
      if (nullp(*(stackptr STACKop -3)))
        { *(stackptr STACKop -3) = STACK_0; # end := l
          # Dann nochmals start und end überprüfen:
          test_start_end(&O(kwpair_start),&*(stackptr STACKop -3));
        }
      # Nun sind alle Argumente überprüft.
      { var uintL bvl; # Bitvektor-Länge
        var uintL dl; # Anzahl der im Bitvektor gesetzten Bits
        # (- end start) bestimmen und neuen Bitvektor allozieren:
        { var object size = I_I_minus_I(STACK_(3+2),STACK_(4+2));
          # size = (- end start), ein Integer >=0
          if (!(posfixnump(size)))
            { pushSTACK(*(stackptr STACKop 0)); # sequence
              fehler(error,
                     GETTEXT("too long sequence ~")
                    );
            }
          bvl = posfixnum_to_L(size);
        }
        pushSTACK(allocate_bit_vector_0(bvl));
        # Stackaufbau:
        #   sequence [stackptr], from-end, start, end, key, test, test-not,
        #   typdescr, l, bv.
        dl = 0; # dl := 0
        # Bei :test #'eq/eql/equal und großer Länge verwende Hashtabelle:
        if (bvl < 10) goto standard;
        if (!(up2_fun == &up2_test)) goto standard;
        { var object test = STACK_(1+3);
          if (!(eq(test,L(eq)) || eq(test,L(eql)) || eq(test,L(equal))
                || eq(test,S(eq)) || eq(test,S(eql)) || eq(test,S(equal))
             ) )
            goto standard;
        }
        if (false)
          standard: # Standardmethode
          { if (!(nullp(STACK_(5+3)))) # from-end abfragen
              # from-end ist angegeben
              {{pushSTACK(STACK_(6+3)); # sequence
                pushSTACK(STACK_(4+3+1)); # start
                funcall(seq_init_start(STACK_(2+2)),2); # (SEQ-INIT-START sequence start)
                pushSTACK(value1); # =: pointer1
               }
               # Stackaufbau:
               #   sequence [stackptr], from-end, start, end, key, test, test-not,
               #   typdescr, l, bv,
               #   pointer1.
               # pointer1 läuft von links nach rechts (von start bis end).
               {var uintL bvi1 = 0; # Schleife bvl mal durchlaufen
                until (bvi1==bvl)
                  { if (!(sbvector_btst(STACK_(0+1),bvi1))) # (sbit bv bvi1) abfragen
                      # falls Bit=0: dieses Element ist noch nicht gestrichen ->
                      # teste, ob es weiter rechts vorkommt:
                      {{pushSTACK(STACK_(6+3+1)); # sequence
                        pushSTACK(STACK_(0+1)); # pointer1
                        funcall(seq_access(STACK_(2+1+2)),2); # (SEQ-ACCESS sequence pointer1)
                        funcall_key(STACK_(2+3+1)); # (FUNCALL key (SEQ-ACCESS sequence pointer1))
                        pushSTACK(value1); # =: item1
                       }
                        # Stackaufbau:
                        #   sequence [stackptr], from-end, start, end, key, test, test-not,
                        #   typdescr, l, bv,
                        #   pointer1, item1.
                        # pointer1 := (SEQ-UPD sequence pointer1) :
                        pointer_update(STACK_1,STACK_(6+3+2),STACK_(2+2));
                        # pointer2 := (SEQ-COPY pointer1) :
                       {pushSTACK(STACK_1); funcall(seq_copy(STACK_(2+2+1)),1); # (SEQ-COPY pointer1)
                        pushSTACK(value1); # =: pointer2
                       }
                        # Stackaufbau:
                        #   sequence [stackptr], from-end, start, end, key, test, test-not,
                        #   typdescr, l, bv,
                        #   pointer1, item1, pointer2.
                        # pointer2 läuft von pointer1 nach rechts.
                       {var uintL bvi2 = bvi1+1; # bvi2 := bvi1+1
                        until (bvi2==bvl)
                          { if (!(sbvector_btst(STACK_(0+3),bvi2))) # (sbit bv bvi2) abfragen
                              # falls Bit=0: dieses Element ist auch noch nicht gestrichen.
                              # vergleiche beide Elemente:
                              { pushSTACK(STACK_(6+3+3)); # sequence
                                pushSTACK(STACK_(0+1)); # pointer2
                                funcall(seq_access(STACK_(2+3+2)),2); # (SEQ-ACCESS sequence pointer2)
                                funcall_key(STACK_(2+3+3)); # (FUNCALL key (SEQ-ACCESS sequence pointer2))
                                # value1 =: item2
                                # item1 und item2 vergleichen:
                                if ((*up2_fun)(stackptr,STACK_1,value1)) # Testroutine aufrufen
                                  # Test erfüllt -> vermerke, dass item2 zu streichen ist:
                                  { sbvector_bset(STACK_(0+3),bvi2); # (setf (sbit bv bvi2) 1)
                                    dl = dl+1; # dl:=dl+1
                                  }
                              }
                            # pointer2 := (SEQ-UPD sequence pointer2) :
                            pointer_update(STACK_0,STACK_(6+3+3),STACK_(2+3));
                            bvi2++; # bvi2 := bvi2+1
                       }  }
                        skipSTACK(2); # item1 und pointer2 vergessen
                      }
                      else
                      # falls Bit=1: dieses Element einfach übergehen
                      { # pointer1 := (SEQ-UPD sequence pointer1) :
                        pointer_update(STACK_0,STACK_(6+3+1),STACK_(2+1));
                      }
                    bvi1++;
               }  }
                skipSTACK(1); # pointer1 vergessen
              }
              else
              # from-end ist nicht angegeben
              {{pushSTACK(STACK_(6+3)); # sequence
                pushSTACK(STACK_(4+3+1)); # start
                funcall(seq_init_start(STACK_(2+2)),2); # (SEQ-INIT-START sequence start)
                pushSTACK(value1); # =: pointer0
               }
               # Stackaufbau:
               #   sequence [stackptr], from-end, start, end, key, test, test-not,
               #   typdescr, l, bv,
               #   pointer0.
               # pointer0 steht links.
               {pushSTACK(STACK_0); funcall(seq_copy(STACK_(2+1+1)),1); # (SEQ-COPY pointer0)
                pushSTACK(value1); # =: pointer2
               }
               # Stackaufbau:
               #   sequence [stackptr], from-end, start, end, key, test, test-not,
               #   typdescr, l, bv,
               #   pointer0, pointer2.
               # pointer2 läuft von links nach rechts (von start bis end).
               {var uintL bvi2 = 0; # Schleife bvl mal durchlaufen
                until (bvi2==bvl)
                  { if (!(sbvector_btst(STACK_(0+2),bvi2))) # (sbit bv bvi2) abfragen
                      # falls Bit=0: dieses Element ist noch nicht gestrichen ->
                      # teste, ob es weiter links vorkommt:
                      {{pushSTACK(STACK_(6+3+2)); # sequence
                        pushSTACK(STACK_(0+1)); # pointer2
                        funcall(seq_access(STACK_(2+2+2)),2); # (SEQ-ACCESS sequence pointer2)
                        funcall_key(STACK_(2+3+2)); # (FUNCALL key (SEQ-ACCESS sequence pointer1))
                        pushSTACK(value1); # =: item2
                       }
                        # Stackaufbau:
                        #   sequence [stackptr], from-end, start, end, key, test, test-not,
                        #   typdescr, l, bv,
                        #   pointer0, pointer2, item2.
                        # pointer1 := (SEQ-COPY pointer0) :
                       {pushSTACK(STACK_2); funcall(seq_copy(STACK_(2+3+1)),1); # (SEQ-COPY pointer0)
                        pushSTACK(value1); # =: pointer1
                       }
                        # Stackaufbau:
                        #   sequence [stackptr], from-end, start, end, key, test, test-not,
                        #   typdescr, l, bv,
                        #   pointer0, pointer2, item2, pointer1.
                        # pointer1 läuft von links bis pointer2.
                       {var uintL bvi1 = 0; # bvi1 := 0
                        until (bvi1==bvi2)
                          { if (!(sbvector_btst(STACK_(0+4),bvi1))) # (sbit bv bvi1) abfragen
                              # falls Bit=0: dieses Element ist auch noch nicht gestrichen.
                              # vergleiche beide Elemente:
                              { pushSTACK(STACK_(6+3+4)); # sequence
                                pushSTACK(STACK_(0+1)); # pointer1
                                funcall(seq_access(STACK_(2+4+2)),2); # (SEQ-ACCESS sequence pointer1)
                                funcall_key(STACK_(2+3+4)); # (FUNCALL key (SEQ-ACCESS sequence pointer1))
                                # value1 =: item1
                                # item1 und item2 vergleichen:
                                if ((*up2_fun)(stackptr,value1,STACK_1)) # Testroutine aufrufen
                                  # Test erfüllt -> vermerke, dass item1 zu streichen ist:
                                  { sbvector_bset(STACK_(0+4),bvi1); # (setf (sbit bv bvi1) 1)
                                    dl = dl+1; # dl:=dl+1
                                  }
                              }
                            # pointer1 := (SEQ-UPD sequence pointer1) :
                            pointer_update(STACK_0,STACK_(6+3+4),STACK_(2+4));
                            bvi1++; # bvi1 := bvi1+1
                       }  }
                        skipSTACK(2); # item2 und pointer1 vergessen
                      }
                    # falls Bit=1: dieses Element einfach übergehen
                    # pointer2 := (SEQ-UPD sequence pointer2) :
                    pointer_update(STACK_0,STACK_(6+3+2),STACK_(2+2));
                    bvi2++; # bvi2 := bvi2+1
               }  }
                skipSTACK(2); # pointer0 und pointer2 vergessen
              }
          }
          else
          # Methode mit Hash-Tabelle
          { # mit (MAKE-HASH-TABLE :test test) eine leere Hash-Tabelle bauen:
            pushSTACK(S(Ktest)); pushSTACK(STACK_(1+3+1)); funcall(L(make_hash_table),2);
            pushSTACK(value1); # ht retten
            {pushSTACK(STACK_(6+3+1)); # sequence
             pushSTACK(STACK_(4+3+2)); # start
             funcall(seq_init_start(STACK_(2+3)),2); # (SEQ-INIT-START sequence start)
             pushSTACK(value1); # =: pointer
            }
            # Stackaufbau:
            #   sequence [stackptr], from-end, start, end, key, test, test-not,
            #   typdescr, l, bv,
            #   ht, pointer.
            if (!(nullp(STACK_(5+3+2)))) # from-end abfragen
              # from-end ist angegeben
              { # pointer läuft von links nach rechts (von start bis end).
                var uintL bvi = 0; # Schleife bvl mal durchlaufen
                until (bvi==bvl)
                  {{pushSTACK(STACK_(6+3+2)); # sequence
                    pushSTACK(STACK_(0+1)); # pointer
                    funcall(seq_access(STACK_(2+2+2)),2); # (SEQ-ACCESS sequence pointer)
                    funcall_key(STACK_(2+3+2)); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                   }# item wird in die Tabelle gesteckt; war es schon
                    # drin, wird bei pointer gestrichen.
                   {var object old_value = shifthash(STACK_1,value1,T);
                    if (!nullp(old_value))
                      # item war schon in ht -> wird jetzt gestrichen
                      { sbvector_bset(STACK_(0+2),bvi); # (setf (sbit bv bvi) 1)
                        dl = dl+1; # dl:=dl+1
                   }  }
                    # pointer := (SEQ-UPD sequence pointer) :
                    pointer_update(STACK_0,STACK_(6+3+2),STACK_(2+2));
                    bvi++; # bvi := bvi+1
                  }
              }
              else
              # from-end ist nicht angegeben
              { # pointer läuft von links nach rechts (von start bis end).
                var uintL bvi = 0; # Schleife bvl mal durchlaufen
                until (bvi==bvl)
                  {{pushSTACK(STACK_(6+3+2)); # sequence
                    pushSTACK(STACK_(0+1)); # pointer
                    funcall(seq_access(STACK_(2+2+2)),2); # (SEQ-ACCESS sequence pointer)
                    funcall_key(STACK_(2+3+2)); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                   }# item wird in die Tabelle gesteckt; war es schon
                    # drin, wird an der vorigen Position gestrichen.
                   {var object old_value =
                      shifthash(STACK_1,value1,fixnum(bvi));
                    if (!nullp(old_value))
                      # item war schon in ht -> wird an der vorigen Position gestrichen
                      { var uintL i = posfixnum_to_L(old_value);
                        sbvector_bset(STACK_(0+2),i); # (setf (sbit bv i) 1)
                        dl = dl+1; # dl:=dl+1
                   }  }
                    # pointer := (SEQ-UPD sequence pointer) :
                    pointer_update(STACK_0,STACK_(6+3+2),STACK_(2+2));
                    bvi++; # bvi := bvi+1
                  }
              }
            skipSTACK(2); # ht und pointer vergessen
          }
        # Stackaufbau:
        #   sequence [stackptr], from-end, start, end, key, test, test-not,
        #   typdescr, l, bv.
        VALUES1((*help_fun)(stackptr,bvl,dl));
        skipSTACK(7+3); # STACK aufräumen
    }}}

LISPFUN(remove_duplicates,1,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
# (REMOVE-DUPLICATES sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not]),
# CLTL S. 254
  { return_Values seq_duplicates(&remove_help); }

LISPFUN(delete_duplicates,1,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
# (DELETE-DUPLICATES sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not]),
# CLTL S. 254
  { return_Values seq_duplicates(&delete_help); }

# UP: Hilfsroutine für SUBSTITUTE-Funktionen.
# Bildet zu einer Sequence eine neue Sequence, in der genau die Elemente
# ersetzt sind, die in einem Bitvektor markiert sind.
# > stackptr: Pointer in den Stack, *(stackptr+2)=newitem,
#   *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
# > STACK_2: typdescr,
# > STACK_1: Länge l der Sequence,
# > STACK_0: Bit-Vektor bv,
# > bvl: Länge des Bit-Vektors (= end - start),
# > dl: Anzahl der im Bit-Vektor gesetzten Bits,
# < ergebnis: Ergebnis
# can trigger GC
  local object substitute_help (object* stackptr, uintL bvl, uintL dl);
  local object substitute_help(stackptr,bvl,dl)
    var object* stackptr;
    var uintL bvl;
    var uintL dl;
    { # dl=0 -> sequence unverändert zurückgeben:
      if (dl==0) { return *(stackptr STACKop 0); }
      if (eq(seq_type(STACK_2),S(list))) # Typ LIST ?
        # Noch überprüfen, ob sequence wirklich eine Liste ist.
        # Wegen l >= dl > 0 ist zu testen, ob sequence ein Cons ist.
        if (mconsp(*(stackptr STACKop 0)))
          { # Listen speziell behandeln:
            pushSTACK(NIL); # L1 := nil
            pushSTACK(*(stackptr STACKop 0)); # L2 := sequence
            # Stackaufbau: ..., typdescr, l, bv,
            #              L1, L2.
            # Erste start Conses kopieren:
            { var uintL count = posfixnum_to_L(*(stackptr STACKop -2)); # 0 <= start <= l ==> start ist Fixnum
              dotimesL(count,count,
                { # Hier gilt (revappend L1 L2) = sequence
                  var object new_cons = allocate_cons();
                  var object L2 = STACK_0;
                  Car(new_cons) = Car(L2);
                  STACK_0 = Cdr(L2); # L2 := (cdr L2)
                  Cdr(new_cons) = STACK_1; STACK_1 = new_cons; # L1 := (cons ... L1)
                });
            }
            # bvl bis über die letzte Eins im Bit-Vector erniedrigen:
            # (Es gibt Einsen, da dl>0.)
            { var object bv = STACK_(0+2);
              loop { var uintL bvl_1 = bvl-1;
                     if (sbvector_btst(bv,bvl_1)) break; #  Bit bvl-1 abfragen
                     bvl = bvl_1; # Bit =0 -> bvl erniedrigen und weitersuchen
            }      }
            # Teilabschnitt kopieren bzw. mit newitem füllen:
            { var uintL bvi = 0; # bvi := 0
              until (bvi==bvl) # Schleife bvl mal durchlaufen
                { if (sbvector_btst(STACK_(0+2),bvi)) # (sbit bv bvi) abfragen
                    { # Bit =1 -> newitem nehmen
                      pushSTACK(*(stackptr STACKop 2)); # newitem
                    }
                    else
                    { # Bit =0 -> (car L2) nehmen
                      pushSTACK(Car(STACK_0));
                    }
                  {var object new_cons = allocate_cons();
                   Car(new_cons) = popSTACK(); # mit Obigem als CAR
                   Cdr(new_cons) = STACK_1; STACK_1 = new_cons; # L1 := (cons ... L1)
                  }
                  STACK_0 = Cdr(STACK_0); # L2 := (cdr L2)
                  bvi++; # bvi:=bvi+1
            }   }
            # letzten Teilabschnitt unverändert dazunehmen:
            { var object L2 = popSTACK();
              var object L1 = popSTACK();
              return nreconc(L1,L2); # (nreconc L1 L2) als Ergebnis
          } }
      # neue Sequence allozieren:
      pushSTACK(STACK_1); # l
      funcall(seq_make(STACK_(2+1)),1); # (SEQ-MAKE l)
      pushSTACK(value1); # =: sequence2
      # Stackaufbau: ..., typdescr, l, bv, sequence2.
      pushSTACK(*(stackptr STACKop 0)); # sequence
      pushSTACK(STACK_(3+1)); # typdescr
      pushSTACK(STACK_(0+2)); # sequence2
      pushSTACK(STACK_(3+3)); # typdescr
      pushSTACK(*(stackptr STACKop -2)); # start
      # Stackaufbau: ..., typdescr, l, bv, sequence2,
      #              seq1, typdescr1, seq2, typdescr2, start.
      pushSTACK(STACK_4); funcall(seq_init(STACK_(3+1)),1); # (SEQ-INIT sequence)
      pushSTACK(value1); # =: pointer1
      pushSTACK(STACK_(2+1)); funcall(seq_init(STACK_(1+1+1)),1); # (SEQ-INIT sequence2)
      pushSTACK(value1); # =: pointer2
      # Stackaufbau: ..., typdescr, l, bv, sequence2,
      #              seq1, typdescr1, seq2, typdescr2, start,
      #              pointer1, pointer2.
      { # Vorderes Teilstück:
        # Elemente mit Index <start von sequence1 nach sequence2
        # unverändert übertragen:
        copy_seqpart_into();
      }
      { # Mittleres Teilstück:
        var uintL bvi = 0;
        until (bvi==bvl)
          { var object item; # zu übernehmendes Element
            if (sbvector_btst(STACK_(1+5+2),bvi)) # (sbit bv bvi) abfragen
              # Bit =1 -> newitem nehmen:
              { item = *(stackptr STACKop 2); }
              else
              # Bit =0 -> Element aus sequence übernehmen:
              { pushSTACK(STACK_(4+2)); pushSTACK(STACK_(1+1));
                funcall(seq_access(STACK_(3+2+2)),2); # (SEQ-ACCESS seq1 pointer1)
                item = value1;
              }
            pushSTACK(STACK_(2+2)); pushSTACK(STACK_(0+1)); pushSTACK(item);
            funcall(seq_access_set(STACK_(1+2+3)),3); # (SEQ-ACCESS-SET seq2 pointer2 ...)
            # pointer1, pointer2, bvi weiterrücken:
            # pointer1 := (SEQ-UPD seq1 pointer1) :
            pointer_update(STACK_1,STACK_(4+2),STACK_(3+2));
            # pointer2 := (SEQ-UPD seq2 pointer2) :
            pointer_update(STACK_0,STACK_(2+2),STACK_(1+2));
            bvi++;
      }   }
      { # Hinteres Teilstück:
        # Elemente mit Index >=end von sequence1 nach sequence2
        # unverändert übertragen:
        STACK_(0+2) = I_I_minus_I(STACK_(2+5+2),*(stackptr STACKop -3)); # (- l end)
        copy_seqpart_into();
      }
      skipSTACK(5+2);
      return popSTACK(); # sequence2 als Ergebnis
    }

LISPFUN(substitute,3,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
# (SUBSTITUTE newitem item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not] [:count]),
# CLTL S. 255
  { var object* stackptr = &STACK_7;
    var up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,up_fun,&substitute_help); # Filtern
    skipSTACK(3+7+1);
  }

LISPFUN(substitute_if,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (SUBSTITUTE-IF newitem test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 255
  { var object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if,&substitute_help); # Filtern
    skipSTACK(3+5+1);
  }

LISPFUN(substitute_if_not,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (SUBSTITUTE-IF-NOT newitem test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 255
  { var object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if_not,&substitute_help); # Filtern
    skipSTACK(3+5+1);
  }

# UP: Hilfsroutine für NSUBSTITUTE-Funktionen im Fall FROM-END.
# Ersetzt in einer Sequence genau die Elemente, die in einem Bitvektor
# markiert sind.
# > stackptr: Pointer in den Stack, *(stackptr+2)=newitem,
#   *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
# > STACK_2: typdescr,
# > STACK_1: Länge l der Sequence,
# > STACK_0: Bit-Vektor bv,
# > bvl: Länge des Bit-Vektors (= end - start),
# > dl: Anzahl der im Bit-Vektor gesetzten Bits,
# < ergebnis: Ergebnis
# can trigger GC
  local object nsubstitute_fe_help (object* stackptr, uintL bvl, uintL dl);
  local object nsubstitute_fe_help(stackptr,bvl,dl)
    var object* stackptr;
    var uintL bvl;
    var uintL dl;
    { {pushSTACK(*(stackptr STACKop 0)); # sequence
       pushSTACK(*(stackptr STACKop -2)); # start
       funcall(seq_init_start(STACK_(2+2)),2); # (SEQ-INIT-START sequence start)
       pushSTACK(value1); # =: pointer
      }
      # Stackaufbau: ..., typdescr, l, bv,
      #                   pointer.
      {var uintL bvi = 0; # bvi := 0
       until (bvi==bvl) # Schleife bvl mal durchlaufen
         { if (sbvector_btst(STACK_(0+1),bvi)) # (sbit bv bvi) abfragen
             # Bit =1 -> ersetze Element durch newitem:
             { pushSTACK(*(stackptr STACKop 0)); # sequence
               pushSTACK(STACK_(0+1)); # pointer
               pushSTACK(*(stackptr STACKop 2)); # newitem
               funcall(seq_access_set(STACK_(2+1+3)),3); # (SEQ-ACCESS-SET sequence pointer newitem)
             }
           # pointer := (SEQ-UPD sequence pointer) :
           pointer_update(STACK_0,*(stackptr STACKop 0),STACK_(2+1));
           bvi++; # bvi:=bvi+1
      }  }
      skipSTACK(1); # pointer vergessen
      return *(stackptr STACKop 0); # sequence als Ergebnis
    }

# Macro: endvar := (and end (- end start)) auf den STACK legen
# init_endvar(stackptr);
# > stackptr: Pointer in den Stack, *(stackptr+1)=start, *(stackptr+0)=end
  #define init_endvar(stackptr)  \
    {var object end = *(stackptr STACKop 0); # end                                        \
     if (!(nullp(end)))                                                                   \
       { end = I_I_minus_I(end,*(stackptr STACKop 1)); } # (- end start), ein Integer >=0 \
     pushSTACK(end);                                                                      \
    }

# Macro: endvar decrementieren falls endvar/=NIL
# decrement_endvar(endvar);
# > object endvar: entweder NIL oder ein Fixnum >0
# < object endvar: entweder immer noch NIL oder (decrementiert) ein Fixnum >=0
  #define decrement_endvar(endvar)  \
    { if (!(nullp(endvar))) # end angegeben ?                \
        { decrement(endvar); } # ja -> endvar := (1- endvar) \
    }

# UP: Führt eine NSUBSTITUTE-Operation durch.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... count typdescr [STACK]
# > stackptr: Pointer in den Stack, *(stackptr+2)=newitem
# > up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#           > stackptr: derselbe Pointer in den Stack,
#           > x: Argument
#           < true, falls der Test erfüllt ist, false sonst.
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# can trigger GC
  local Values nsubstitute_op (object* stackptr, up_function up_fun);
  local Values nsubstitute_op(stackptr,up_fun)
    var object* stackptr;
    var up_function up_fun;
    { if (!(nullp(*(stackptr STACKop -1)))) # from-end abfragen
        # from-end ist angegeben -> Bit-Vector erzeugen und dann ersetzen:
        { return_Values seq_filterop(stackptr,up_fun,&nsubstitute_fe_help); }
        else
        # from-end ist nicht angegeben
        { # COUNT-Argument muss NIL oder ein Integer >= 0 sein:
          test_count_arg();
          # Nun sind alle Argumente überprüft.
          pushSTACK(*(stackptr STACKop 0)); # sequence
          pushSTACK(*(stackptr STACKop -4)); # key
          init_endvar(&*(stackptr STACKop -3)); # endvar := (and end (- end start)) auf den Stack
          pushSTACK(STACK_(1+3)); # countdown := count
          # Stackaufbau: ..., count, typdescr,
          #              sequence, key, endvar, countdown.
          {pushSTACK(STACK_3); # sequence
           pushSTACK(*(stackptr STACKop -2)); # start
           funcall(seq_init_start(STACK_(0+4+2)),2); # (SEQ-INIT-START sequence start)
           pushSTACK(value1); # =: pointer
          }
          # Stackaufbau: ..., count, typdescr,
          #              sequence, key, endvar, countdown, pointer.
          # endvar und countdown sind jeweils entweder =NIL oder ein Integer >=0.
          { until (eq(STACK_2,Fixnum_0)) # endvar = 0 ?
                # (also end angegeben und (- end start) Elemente durchlaufen ?)
                # ja -> fertig
              { pushSTACK(STACK_4); pushSTACK(STACK_(0+1));
                funcall(seq_endtest(STACK_(0+5+2)),2); # (SEQ-ENDTEST sequence pointer)
                if (!(nullp(value1))) break; # Pointer am Ende -> fertig
                if (eq(STACK_1,Fixnum_0)) # countdown=0 ?
                  # (also count angegeben und erschöpft?)
                  break; # ja -> Schleife kann abgebrochen werden
                # item herausgreifen:
                pushSTACK(STACK_4); pushSTACK(STACK_(0+1));
                funcall(seq_access(STACK_(0+5+2)),2); # (SEQ-ACCESS sequence pointer)
                funcall_key(STACK_3); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                # value1 =: item
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  # Test ist erfüllt
                  { pushSTACK(STACK_4); pushSTACK(STACK_(0+1));
                    pushSTACK(*(stackptr STACKop 2)); # newitem
                    funcall(seq_access_set(STACK_(0+5+3)),3); # (SEQ-ACCESS-SET sequence pointer newitem)
                    if (!(nullp(STACK_(1+5)))) # falls count/=NIL:
                      { decrement(STACK_1); } # (decf countdown)
                  }
                # pointer := (SEQ-UPD sequence pointer) :
                pointer_update(STACK_0,STACK_4,STACK_(0+5));
                # endvar eventuell decrementieren:
                decrement_endvar(STACK_2);
          }   }
          skipSTACK(4);
          VALUES1(popSTACK()); /* return modified sequence */
        }
    }

LISPFUN(nsubstitute,3,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
# (NSUBSTITUTE newitem item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not] [:count]),
# CLTL S. 256
  { var object* stackptr = &STACK_7;
    var up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    nsubstitute_op(stackptr,up_fun); # gefiltert ersetzen
    skipSTACK(3+7+1);
  }

LISPFUN(nsubstitute_if,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (NSUBSTITUTE-IF newitem test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 256
  { var object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    nsubstitute_op(stackptr,&up_if); # gefiltert ersetzen
    skipSTACK(3+5+1);
  }

LISPFUN(nsubstitute_if_not,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (NSUBSTITUTE-IF-NOT newitem test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 256
  { var object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    nsubstitute_op(stackptr,&up_if_not); # gefiltert ersetzen
    skipSTACK(3+5+1);
  }

# UP: Führt eine FIND-Operation durch.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... typdescr [STACK]
# > stackptr: Pointer in den Stack
# > up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#           > stackptr: derselbe Pointer in den Stack,
#           > x: Argument
#           < true, falls der Test erfüllt ist, false sonst.
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# can trigger GC
  local Values find_op (object* stackptr, up_function up_fun);
  local Values find_op(stackptr,up_fun)
    var object* stackptr;
    var up_function up_fun;
    { pushSTACK(*(stackptr STACKop 0)); # sequence
      # Stackaufbau: ..., typdescr, sequence.
      if (!(nullp(*(stackptr STACKop -1)))) # from-end abfragen
        # from-end ist angegeben
        { # Defaultwert für end ist die Länge der Sequence:
          if (nullp(*(stackptr STACKop -3)))
            { { var object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
                pushSTACK(STACK_0); funcall(seq_length(STACK_(1+1)),1); # (SEQ-LENGTH sequence)
                *(stackptr STACKop -3) = value1; # =: end
                subr_self = old_subr_self;
              }
              # Dann nochmals start und end überprüfen:
              test_start_end(&O(kwpair_start),&*(stackptr STACKop -3));
            }
          {pushSTACK(STACK_0); pushSTACK(*(stackptr STACKop -3));
           funcall(seq_fe_init_end(STACK_(1+2)),2); # (SEQ-FE-INIT-END sequence end)
           pushSTACK(value1); # =: pointer
          }
          { # count := (- end start), ein Integer >=0 :
            pushSTACK(I_I_minus_I(*(stackptr STACKop -3),*(stackptr STACKop -2)));
          }
          # Stackaufbau: ..., typdescr, sequence, pointer, count.
          { until (eq(STACK_0,Fixnum_0)) # count (ein Integer) = 0 -> fertig
              { # item herausgreifen:
                pushSTACK(STACK_2); pushSTACK(STACK_(1+1));
                funcall(seq_access(STACK_(3+2)),2); # (SEQ-ACCESS sequence pointer)
                pushSTACK(value1); # =: item
                funcall_key(*(stackptr STACKop -4)); # (FUNCALL key item)
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  goto found; # Test erfüllt -> gefunden
                # Test ist nicht erfüllt
                skipSTACK(1); # item vergessen
                # pointer weiterrücken und count decrementieren:
                # pointer := (SEQ-FE-UPD sequence pointer) :
                pointer_fe_update(STACK_1,STACK_2,STACK_3);
                decrement(STACK_0); # count := (1- count)
        } }   }
        else
        # from-end ist nicht angegeben
        { init_endvar(&*(stackptr STACKop -3)); # endvar := (and end (- end start)) auf den Stack
          # Stackaufbau: ..., typdescr, sequence, endvar.
          {pushSTACK(STACK_1); pushSTACK(*(stackptr STACKop -2));
           funcall(seq_init_start(STACK_(2+2)),2); # (SEQ-INIT-START sequence start)
           pushSTACK(value1); # =: pointer
          }
          # Stackaufbau: ... typdescr, sequence, endvar, pointer
          { until (eq(STACK_1,Fixnum_0)) # endvar = 0 ?
                # (also end angegeben und (- end start) Elemente durchlaufen ?)
                # ja -> fertig
              { pushSTACK(STACK_2); pushSTACK(STACK_(0+1));
                funcall(seq_endtest(STACK_(3+2)),2); # (SEQ-ENDTEST sequence pointer)
                if (!(nullp(value1))) break; # Pointer am Ende -> fertig
                # item herausgreifen:
                pushSTACK(STACK_2); pushSTACK(STACK_(0+1));
                funcall(seq_access(STACK_(3+2)),2); # (SEQ-ACCESS sequence pointer)
                pushSTACK(value1); # =: item
                funcall_key(*(stackptr STACKop -4)); # (FUNCALL key item)
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  goto found; # Test erfüllt -> gefunden
                # Test ist nicht erfüllt
                skipSTACK(1); # item vergessen
                # pointer := (SEQ-UPD sequence pointer) :
                pointer_update(STACK_0,STACK_2,STACK_3);
                # endvar eventuell decrementieren:
                decrement_endvar(STACK_1);
        } }   }
      skipSTACK(3); # STACK aufräumen
      VALUES1(NIL); return;
      found: # item gefunden, das den Test erfüllt. STACK_0 = item.
      VALUES1(popSTACK()); /* return item */
      skipSTACK(3); # STACK aufräumen
    }

LISPFUN(find,2,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
# (FIND item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not]),
# CLTL S. 257
  { var object* stackptr = &STACK_6;
    var up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    find_op(stackptr,up_fun); # suchen
    skipSTACK(2+6+1);
  }

LISPFUN(find_if,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (FIND-IF test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    find_op(stackptr,&up_if); # suchen
    skipSTACK(2+4+1);
  }

LISPFUN(find_if_not,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (FIND-IF-NOT test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    find_op(stackptr,&up_if_not); # suchen
    skipSTACK(2+4+1);
  }

# UP: Führt eine POSITION-Operation durch.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... typdescr [STACK]
# > stackptr: Pointer in den Stack
# > up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#           > stackptr: derselbe Pointer in den Stack,
#           > x: Argument
#           < true, falls der Test erfüllt ist, false sonst.
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# can trigger GC
  local Values position_op (object* stackptr, up_function up_fun);
  local Values position_op(stackptr,up_fun)
    var object* stackptr;
    var up_function up_fun;
    { pushSTACK(*(stackptr STACKop 0)); # sequence
      # Stackaufbau: ..., typdescr, sequence.
      if (!(nullp(*(stackptr STACKop -1)))) # from-end abfragen
        # from-end ist angegeben
        { # Defaultwert für end ist die Länge der Sequence:
          if (nullp(*(stackptr STACKop -3)))
            { { var object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
                pushSTACK(STACK_0); funcall(seq_length(STACK_(1+1)),1); # (SEQ-LENGTH sequence)
                *(stackptr STACKop -3) = value1; # =: end
                subr_self = old_subr_self;
              }
              # Dann nochmals start und end überprüfen:
              test_start_end(&O(kwpair_start),&*(stackptr STACKop -3));
            }
          pushSTACK(*(stackptr STACKop -3)); # index := end
          {pushSTACK(STACK_(0+1)); pushSTACK(*(stackptr STACKop -3));
           funcall(seq_fe_init_end(STACK_(1+1+2)),2); # (SEQ-FE-INIT-END sequence end)
           pushSTACK(value1); # =: pointer
          }
          { # count := (- end start), ein Integer >=0 :
            pushSTACK(I_I_minus_I(*(stackptr STACKop -3),*(stackptr STACKop -2)));
          }
          # Stackaufbau: ..., typdescr, sequence, index, pointer, count.
          { until (eq(STACK_0,Fixnum_0)) # count (ein Integer) = 0 -> fertig
              { # index decrementieren:
                decrement(STACK_2);
                # item herausgreifen:
                pushSTACK(STACK_3); pushSTACK(STACK_(1+1));
                funcall(seq_access(STACK_(4+2)),2); # (SEQ-ACCESS sequence pointer)
                funcall_key(*(stackptr STACKop -4)); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  goto found; # Test erfüllt -> gefunden
                # Test ist nicht erfüllt
                # pointer weiterrücken und count decrementieren:
                # pointer := (SEQ-FE-UPD sequence pointer) :
                pointer_fe_update(STACK_1,STACK_3,STACK_4);
                decrement(STACK_0); # count := (1- count)
        } }   }
        else
        # from-end ist nicht angegeben
        { pushSTACK(*(stackptr STACKop -2)); # index := start
          init_endvar(&*(stackptr STACKop -3)); # endvar := (and end (- end start)) auf den Stack
          # Stackaufbau: ..., typdescr, sequence, index, endvar.
          {pushSTACK(STACK_2); pushSTACK(*(stackptr STACKop -2));
           funcall(seq_init_start(STACK_(3+2)),2); # (SEQ-INIT-START sequence start)
           pushSTACK(value1); # =: pointer
          }
          # Stackaufbau: ... typdescr, sequence, index, endvar, pointer
          { until (eq(STACK_1,Fixnum_0)) # endvar = 0 ?
                # (also end angegeben und (- end start) Elemente durchlaufen ?)
                # ja -> fertig
              { pushSTACK(STACK_3); pushSTACK(STACK_(0+1));
                funcall(seq_endtest(STACK_(4+2)),2); # (SEQ-ENDTEST sequence pointer)
                if (!(nullp(value1))) break; # Pointer am Ende -> fertig
                # item herausgreifen:
                pushSTACK(STACK_3); pushSTACK(STACK_(0+1));
                funcall(seq_access(STACK_(4+2)),2); # (SEQ-ACCESS sequence pointer)
                funcall_key(*(stackptr STACKop -4)); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  goto found; # Test erfüllt -> gefunden
                # Test ist nicht erfüllt
                # pointer := (SEQ-UPD sequence pointer) :
                pointer_update(STACK_0,STACK_3,STACK_4);
                # endvar eventuell decrementieren:
                decrement_endvar(STACK_1);
                # index incrementieren:
                increment(STACK_2);
        } }   }
      skipSTACK(4); # STACK aufräumen
      VALUES1(NIL); return;
      found: # item gefunden, das den Test erfüllt. STACK_2 = index.
      VALUES1(STACK_2); /* return index */
      skipSTACK(4); # STACK aufräumen
    }

LISPFUN(position,2,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
# (POSITION item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not]),
# CLTL S. 257
  { var object* stackptr = &STACK_6;
    var up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    position_op(stackptr,up_fun); # suchen
    skipSTACK(2+6+1);
  }

LISPFUN(position_if,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (POSITION-IF test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    position_op(stackptr,&up_if); # suchen
    skipSTACK(2+4+1);
  }

LISPFUN(position_if_not,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (POSITION-IF-NOT test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    position_op(stackptr,&up_if_not); # suchen
    skipSTACK(2+4+1);
  }

# UP: Führt eine COUNT-Operation durch.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... typdescr [STACK]
# > stackptr: Pointer in den Stack
# > up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#           > stackptr: derselbe Pointer in den Stack,
#           > x: Argument
#           < true, falls der Test erfüllt ist, false sonst.
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# can trigger GC
  local Values count_op (object* stackptr, up_function up_fun);
  local Values count_op(stackptr,up_fun)
    var object* stackptr;
    var up_function up_fun;
    { pushSTACK(*(stackptr STACKop 0)); # sequence
      pushSTACK(Fixnum_0); # total := 0
      # Stackaufbau: ..., typdescr, sequence, total.
      if (!(nullp(*(stackptr STACKop -1)))) # from-end abfragen
        # from-end ist angegeben
        { # Defaultwert für end ist die Länge der Sequence:
          if (nullp(*(stackptr STACKop -3)))
            { { var object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
                pushSTACK(STACK_1); funcall(seq_length(STACK_(2+1)),1); # (SEQ-LENGTH sequence)
                *(stackptr STACKop -3) = value1; # =: end
                subr_self = old_subr_self;
              }
              # Dann nochmals start und end überprüfen:
              test_start_end(&O(kwpair_start),&*(stackptr STACKop -3));
            }
          {pushSTACK(STACK_1); pushSTACK(*(stackptr STACKop -3));
           funcall(seq_fe_init_end(STACK_(2+2)),2); # (SEQ-FE-INIT-END sequence end)
           pushSTACK(value1); # =: pointer
          }
          { # count := (- end start), ein Integer >=0 :
            pushSTACK(I_I_minus_I(*(stackptr STACKop -3),*(stackptr STACKop -2)));
          }
          # Stackaufbau: ..., typdescr, sequence, total, pointer, count.
          { until (eq(STACK_0,Fixnum_0)) # count (ein Integer) = 0 -> fertig
              { # item herausgreifen:
                pushSTACK(STACK_3); pushSTACK(STACK_(1+1));
                funcall(seq_access(STACK_(4+2)),2); # (SEQ-ACCESS sequence pointer)
                funcall_key(*(stackptr STACKop -4)); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  { # Test ist erfüllt -> total := total + 1 :
                    STACK_2 = fixnum_inc(STACK_2,1);
                  }
                # pointer weiterrücken und count decrementieren:
                # pointer := (SEQ-FE-UPD sequence pointer) :
                pointer_fe_update(STACK_1,STACK_3,STACK_4);
                decrement(STACK_0); # count := (1- count)
        } }   }
        else
        # from-end ist nicht angegeben
        { init_endvar(&*(stackptr STACKop -3)); # endvar := (and end (- end start)) auf den Stack
          # Stackaufbau: ..., typdescr, sequence, total, endvar.
          {pushSTACK(STACK_2); pushSTACK(*(stackptr STACKop -2));
           funcall(seq_init_start(STACK_(3+2)),2); # (SEQ-INIT-START sequence start)
           pushSTACK(value1); # =: pointer
          }
          # Stackaufbau: ... typdescr, sequence, total, endvar, pointer
          { until (eq(STACK_1,Fixnum_0)) # endvar = 0 ?
                # (also end angegeben und (- end start) Elemente durchlaufen ?)
                # ja -> fertig
              { pushSTACK(STACK_3); pushSTACK(STACK_(0+1));
                funcall(seq_endtest(STACK_(4+2)),2); # (SEQ-ENDTEST sequence pointer)
                if (!(nullp(value1))) break; # Pointer am Ende -> fertig
                # item herausgreifen:
                pushSTACK(STACK_3); pushSTACK(STACK_(0+1));
                funcall(seq_access(STACK_(4+2)),2); # (SEQ-ACCESS sequence pointer)
                funcall_key(*(stackptr STACKop -4)); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  { # Test ist erfüllt -> total := total + 1 :
                    STACK_2 = fixnum_inc(STACK_2,1);
                  }
                # pointer := (SEQ-UPD sequence pointer) :
                pointer_update(STACK_0,STACK_3,STACK_4);
                # endvar eventuell decrementieren:
                decrement_endvar(STACK_1);
        } }   }
      VALUES1(STACK_2); skipSTACK(4); /* return total */
    }

LISPFUN(count,2,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
# (COUNT item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not]),
# CLTL S. 257
  { var object* stackptr = &STACK_6;
    var up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    count_op(stackptr,up_fun); # suchen
    skipSTACK(2+6+1);
  }

LISPFUN(count_if,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (COUNT-IF test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    count_op(stackptr,&up_if); # suchen
    skipSTACK(2+4+1);
  }

LISPFUN(count_if_not,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (COUNT-IF-NOT test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    count_op(stackptr,&up_if_not); # suchen
    skipSTACK(2+4+1);
  }

LISPFUN(mismatch,2,0,norest,key,8,
        (kw(start1),kw(end1),kw(start2),kw(end2),kw(from_end),
         kw(key),kw(test),kw(test_not)) )
# (MISMATCH sequence1 sequence2
#           [:start1] [:end1] [:start2] [:end2] [:from-end] [:key] [:test] [:test-not]),
# CLTL S. 257
  { # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
    #              key, test, test-not.
    var object* stackptr = &STACK_6;
    # key überprüfen:
    test_key_arg(stackptr);
    # test, test-not überprüfen:
   {var up2_function up2_fun = test_test2_args(stackptr);
    # sequence1 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_(6+3)));
    # sequence2 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_(5+3+1)));
    # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
    #              key, test, test-not, typdescr1, typdescr2.
    default_NIL(STACK_(0+5)); # Defaultwert für from-end ist NIL
    start_default_0(STACK_(4+5)); # Defaultwert für start1 ist 0
    default_NIL(STACK_(3+5)); # Defaultwert für end1 ist NIL
    start_default_0(STACK_(2+5)); # Defaultwert für start2 ist 0
    default_NIL(STACK_(1+5)); # Defaultwert für end2 ist NIL
    # from-end abfragen:
    if (!(nullp(STACK_(0+5))))
      # from-end ist angegeben
      { # Defaultwert von end1 ist (SEQ-LENGTH seq1):
        end_default_len(STACK_(3+5),STACK_(6+5),STACK_1);
        # Defaultwert von end2 ist (SEQ-LENGTH seq2):
        end_default_len(STACK_(1+5),STACK_(5+5),STACK_0);
        # start- und end-Argumente überprüfen:
        subr_self = L(mismatch);
        test_start_end(&O(kwpair_start1),&STACK_(3+5));
        test_start_end(&O(kwpair_start2),&STACK_(1+5));
        # pointer1 und pointer2 ans Ende der Sequences setzen:
        { pushSTACK(STACK_(6+5)); pushSTACK(STACK_(3+5+1));
          funcall(seq_fe_init_end(STACK_(1+2)),2); # (SEQ-FE-INIT-END seq1 end1)
          pushSTACK(value1); # =: pointer1
        }
        { pushSTACK(STACK_(5+5+1)); pushSTACK(STACK_(1+5+1+1));
          funcall(seq_fe_init_end(STACK_(0+1+2)),2); # (SEQ-FE-INIT-END seq2 end2)
          pushSTACK(value1); # =: pointer2
        }
        { pushSTACK(STACK_(3+5+2)); } # index := end1
        { var object len1 = I_I_minus_I(STACK_(3+5+3),STACK_(4+5+3)); # (- end1 start1)
          pushSTACK(len1); # =: len1, ein Integer >=0
        }
        { var object len2 = I_I_minus_I(STACK_(1+5+4),STACK_(2+5+4)); # (- end2 start2)
          pushSTACK(len2); # =: len2, ein Integer >=0
        }
        { var object count = (I_I_comp(STACK_1,STACK_0)<0 ? STACK_1 : STACK_0); # (min len1 len2)
          pushSTACK(count); # =: count, ein Integer >=0
        }
        # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
        #              key, test, test-not, typdescr1, typdescr2,
        #              pointer1, pointer2, index, len1, len2, count.
        until (eq(STACK_0,Fixnum_0)) # count (ein Integer) = 0 ?
          { pushSTACK(STACK_(6+5+6)); pushSTACK(STACK_(5+1));
            funcall(seq_access(STACK_(1+6+2)),2); # (SEQ-ACCESS seq1 pointer1)
            funcall_key(STACK_(4+6)); # (FUNCALL key (SEQ-ACCESS seq1 pointer1))
            pushSTACK(value1); # =: item1, retten
            pushSTACK(STACK_(5+5+6+1)); pushSTACK(STACK_(4+1+1));
            funcall(seq_access(STACK_(0+6+1+2)),2); # (SEQ-ACCESS seq2 pointer2)
            funcall_key(STACK_(4+6+1)); # (FUNCALL key (SEQ-ACCESS seq2 pointer2))
            {var object item2 = value1;
             var object item1 = popSTACK();
             # beide vergleichen:
             if (!((*up2_fun)(&STACK_(8+6),item1,item2))) # Testroutine anwenden
               goto fe_found;
            }
            # Test erfüllt -> weitersuchen:
            # pointer1 := (SEQ-FE-UPD seq1 pointer1) :
            pointer_fe_update(STACK_5,STACK_(6+5+6),STACK_(1+6));
            # pointer2 := (SEQ-FE-UPD seq2 pointer2) :
            pointer_fe_update(STACK_4,STACK_(5+5+6),STACK_(0+6));
            # index decrementieren:
            decrement(STACK_3);
            # count decrementieren:
            decrement(STACK_0);
          }
        # Schleife erfolgreich.
        # Bei len1=len2 Ergebnis NIL, sonst index:
        if (I_I_comp(STACK_2,STACK_1)==0) # len1=len2 (Integers) ?
          # Beide Sequence-Stücke sind gleich -> NIL als Wert
          { VALUES1(NIL); skipSTACK(7+5+6); return; }
        fe_found: # Es ist ein Unterschied gefunden -> index als Wert
        { VALUES1(STACK_3); skipSTACK(7+5+6); return; }
      }
      else
      # from-end ist nicht angegeben
      { # start- und end-Argumente überprüfen:
        test_start_end_1(&O(kwpair_start1),&STACK_(3+5));
        test_start_end_1(&O(kwpair_start2),&STACK_(1+5));
        # pointer1 und pointer2 an den Anfang der Sequences setzen:
        { pushSTACK(STACK_(6+5)); pushSTACK(STACK_(4+5+1));
          funcall(seq_init_start(STACK_(1+2)),2); # (SEQ-INIT-START seq1 start1)
          pushSTACK(value1); # =: pointer1
        }
        { pushSTACK(STACK_(5+5+1)); pushSTACK(STACK_(2+5+1+1));
          funcall(seq_init_start(STACK_(0+1+2)),2); # (SEQ-INIT-START seq2 start2)
          pushSTACK(value1); # =: pointer2
        }
        { pushSTACK(STACK_(4+5+2)); } # index := start1
        init_endvar(&STACK_(3+5+3)); # endvar1 := (and end1 (- end1 start1))
        init_endvar(&STACK_(1+5+4)); # endvar2 := (and end2 (- end2 start2))
        # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
        #              key, test, test-not, typdescr1, typdescr2,
        #              pointer1, pointer2, index, endvar1, endvar2.
        { var bool seq1_ended; # Flag, ob seq1-Teilstück zu Ende
          var bool seq2_ended; # Flag, ob seq2-Teilstück zu Ende
          loop
            { # Teste, ob seq1-Teilstück zu Ende:
              if (eq(STACK_1,Fixnum_0)) # endvar1 = 0 (und damit end1 /= nil) ?
                { seq1_ended = true; }
                else
                { pushSTACK(STACK_(6+5+5)); pushSTACK(STACK_(4+1));
                  funcall(seq_endtest(STACK_(1+5+2)),2); # (SEQ-ENDTEST seq1 pointer1)
                  seq1_ended = !nullp(value1);
                }
              # Teste, ob seq2-Teilstück zu Ende:
              if (eq(STACK_0,Fixnum_0)) # endvar2 = 0 (und damit end2 /= nil) ?
                { seq2_ended = true; }
                else
                { pushSTACK(STACK_(5+5+5)); pushSTACK(STACK_(3+1));
                  funcall(seq_endtest(STACK_(0+5+2)),2); # (SEQ-ENDTEST seq2 pointer2)
                  seq2_ended = !nullp(value1);
                }
              # Flags abtesten:
              if (seq1_ended || seq2_ended) break;
              # keines der beiden Flags ist gesetzt
              pushSTACK(STACK_(6+5+5)); pushSTACK(STACK_(4+1));
              funcall(seq_access(STACK_(1+5+2)),2); # (SEQ-ACCESS seq1 pointer1)
              funcall_key(STACK_(4+5)); # (FUNCALL key (SEQ-ACCESS seq1 pointer1))
              pushSTACK(value1); # =: item1, retten
              pushSTACK(STACK_(5+5+5+1)); pushSTACK(STACK_(3+1+1));
              funcall(seq_access(STACK_(0+5+1+2)),2); # (SEQ-ACCESS seq2 pointer2)
              funcall_key(STACK_(4+5+1)); # (FUNCALL key (SEQ-ACCESS seq2 pointer2))
              {var object item2 = value1;
               var object item1 = popSTACK();
               # beide vergleichen:
               if (!((*up2_fun)(&STACK_(8+5),item1,item2))) # Testroutine anwenden
                 goto fs_found;
              }
              # Test erfüllt -> weitersuchen:
              # pointer1 := (SEQ-UPD seq1 pointer1) :
              pointer_update(STACK_4,STACK_(6+5+5),STACK_(1+5));
              # pointer2 := (SEQ-UPD seq2 pointer2) :
              pointer_update(STACK_3,STACK_(5+5+5),STACK_(0+5));
              # index incrementieren:
              increment(STACK_2);
              # endvar1 eventuell decrementieren:
              decrement_endvar(STACK_1);
              # endvar2 eventuell decrementieren:
              decrement_endvar(STACK_0);
            }
          # Falls beide Flags gesetzt sind, Ergebnis NIL, sonst index:
          if (seq1_ended && seq2_ended)
            # Beide Sequence-Stücke sind gleich -> NIL als Wert
            { VALUES1(NIL); skipSTACK(7+5+5); return; }
          fs_found: # Es ist ein Unterschied gefunden -> index als Wert
          { VALUES1(STACK_2); skipSTACK(7+5+5); return; }
      } }
  }}

LISPFUN(search,2,0,norest,key,8,
        (kw(start1),kw(end1),kw(start2),kw(end2),kw(from_end),
         kw(key),kw(test),kw(test_not)) )
# (SEARCH sequence1 sequence2
#         [:start1] [:end1] [:start2] [:end2] [:from-end] [:key] [:test] [:test-not]),
# CLTL S. 258
  # Primitiv-Algorithmus:
  #   Rücke immer in sequence2 um 1 weiter und teste, ob dann sequence1 kommt.
  # Knuth-Algorithmus:
  #   [Donald Ervin Knuth, James H. Morris, Vaughan R. Pratt:
  #    Fast pattern matching in string.
  #    SIAM J. Comput. 6(1977), 323-350.]
  #   Kann hier nicht verwendet werden, weil er die Kommutativität der
  #   Testfunktion erfordert, die nach CLTL S. 247 nicht notwendig gegeben ist.
  { # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
    #              key, test, test-not.
    var object* stackptr = &STACK_6;
    # key überprüfen:
    test_key_arg(stackptr);
    # test, test-not überprüfen:
   {var up2_function up2_fun = test_test2_args(stackptr);
    # sequence1 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_(6+3)));
    # sequence2 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_(5+3+1)));
    # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
    #              key, test, test-not, typdescr1, typdescr2.
    default_NIL(STACK_(0+5)); # Defaultwert für from-end ist NIL
    # Sonderfall für Strings: schnellere Routine aufrufen
    if (eq(seq_type(STACK_1),S(string)) && eq(seq_type(STACK_0),S(string)) # beides STRINGs ?
        && nullp(STACK_(0+5)) # und kein from-end ?
        && eq(STACK_4,L(identity)) # und key = #'identity ?
        && (up2_fun == &up2_test) # und test-not nicht angegeben ?
       )
      { var object test = STACK_3;
        if (eq(test,L(eq)) || eq(test,L(eql)) || eq(test,L(equal)) || eq(test,L(char_gleich)))
          { skipSTACK(6);
            C_search_string_gleich(); # SUBR sys::search-string= mit denselben Argumenten
            return;
          }
        if (eq(test,L(equalp)) || eq(test,L(char_equal)))
          { skipSTACK(6);
            C_search_string_equal(); # SUBR sys::search-string-equal mit denselben Argumenten
            return;
      }   }
    start_default_0(STACK_(4+5)); # Defaultwert für start1 ist 0
    default_NIL(STACK_(3+5)); # Defaultwert für end1 ist NIL
    start_default_0(STACK_(2+5)); # Defaultwert für start2 ist 0
    default_NIL(STACK_(1+5)); # Defaultwert für end2 ist NIL
    # from-end abfragen:
    if (!(nullp(STACK_(0+5))))
      # from-end ist angegeben
      { # Defaultwert von end1 ist (SEQ-LENGTH seq1):
        end_default_len(STACK_(3+5),STACK_(6+5),STACK_1);
        # Defaultwert von end2 ist (SEQ-LENGTH seq2):
        end_default_len(STACK_(1+5),STACK_(5+5),STACK_0);
        # start- und end-Argumente überprüfen:
        subr_self = L(search);
        test_start_end(&O(kwpair_start1),&STACK_(3+5));
        test_start_end(&O(kwpair_start2),&STACK_(1+5));
        # pointer10 und pointer20 ans Ende der Sequences setzen:
        { pushSTACK(STACK_(6+5)); pushSTACK(STACK_(3+5+1));
          funcall(seq_fe_init_end(STACK_(1+2)),2); # (SEQ-FE-INIT-END seq1 end1)
          pushSTACK(value1); # =: pointer10
        }
        { pushSTACK(STACK_(5+5+1)); pushSTACK(STACK_(1+5+1+1));
          funcall(seq_fe_init_end(STACK_(0+1+2)),2); # (SEQ-FE-INIT-END seq2 end2)
          pushSTACK(value1); # =: pointer20
        }
        { var object len1 = I_I_minus_I(STACK_(3+5+2),STACK_(4+5+2)); # (- end1 start1)
          pushSTACK(len1); # =: len1, ein Integer >=0
        }
        { var object len2 = I_I_minus_I(STACK_(1+5+3),STACK_(2+5+3)); # (- end2 start2)
          pushSTACK(len2); # =: len2, ein Integer >=0
        }
        { var object index = I_I_minus_I(STACK_(1+5+4),STACK_1); # (- end2 len1)
          pushSTACK(index); # =: index, ein Integer
        }
        # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
        #              key, test, test-not, typdescr1, typdescr2,
        #              pointer10, pointer20, len1, len2, index.
        loop
          { # pointer1 und pointer2 ab pointer10 bzw. pointer20 laufen lassen:
            { pushSTACK(STACK_4); funcall(seq_copy(STACK_(1+5+1)),1); # (SEQ-COPY pointer10)
              pushSTACK(value1); # =: pointer1
            }
            { pushSTACK(STACK_(3+1)); funcall(seq_copy(STACK_(0+5+1+1)),1); # (SEQ-COPY pointer20)
              pushSTACK(value1); # =: pointer2
            }
            pushSTACK(STACK_(2+2)); # count1 := len1
            pushSTACK(STACK_(1+3)); # count2 := len2
            # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
            #              key, test, test-not, typdescr1, typdescr2,
            #              pointer10, pointer20, len1, len2, index,
            #              pointer1, pointer2, count1, count2.
            loop
              { if (eq(STACK_1,Fixnum_0)) # count1 (ein Integer) = 0 ?
                  goto found; # ja -> seq1 zu Ende, gefunden
                if (eq(STACK_0,Fixnum_0)) # count2 (ein Integer) = 0 ?
                  goto notfound; # ja -> seq2 zu Ende, nicht gefunden
                pushSTACK(STACK_(6+5+5+4)); pushSTACK(STACK_(3+1));
                funcall(seq_access(STACK_(1+5+4+2)),2); # (SEQ-ACCESS seq1 pointer1)
                funcall_key(STACK_(4+5+4)); # (FUNCALL key (SEQ-ACCESS seq1 pointer1))
                pushSTACK(value1); # =: item1, retten
                pushSTACK(STACK_(5+5+5+4+1)); pushSTACK(STACK_(2+1+1));
                funcall(seq_access(STACK_(0+5+4+1+2)),2); # (SEQ-ACCESS seq2 pointer2)
                funcall_key(STACK_(4+5+4+1)); # (FUNCALL key (SEQ-ACCESS seq2 pointer2))
                {var object item2 = value1;
                 var object item1 = popSTACK();
                 # beide vergleichen:
                 if (!((*up2_fun)(&STACK_(8+5+4),item1,item2))) # Testroutine anwenden
                   break;
                }
                # Test erfüllt -> weitervergleichen:
                # pointer1 := (SEQ-FE-UPD seq1 pointer1) :
                pointer_fe_update(STACK_3,STACK_(6+5+5+4),STACK_(1+5+4));
                # pointer2 := (SEQ-FE-UPD seq2 pointer2) :
                pointer_fe_update(STACK_2,STACK_(5+5+5+4),STACK_(0+5+4));
                # count1 decrementieren:
                decrement(STACK_1);
                # count2 decrementieren:
                decrement(STACK_0);
              }
            # Test nicht erfüllt -> weitersuchen
            skipSTACK(4); # pointer1, pointer2, count1, count2 vergessen
            # pointer20 weiterrücken, len2 und index decrementieren:
            pointer_fe_update(STACK_3,STACK_(6+5+5),STACK_(0+5));
            decrement(STACK_1); # len2 := (1- len2)
            decrement(STACK_0); # index := (1- index)
      }   }
      else
      # from-end ist nicht angegeben
      { # start- und end-Argumente überprüfen:
        test_start_end_1(&O(kwpair_start1),&STACK_(3+5));
        test_start_end_1(&O(kwpair_start2),&STACK_(1+5));
        # pointer10 und pointer20 an den Anfang der Sequences setzen:
        { pushSTACK(STACK_(6+5)); pushSTACK(STACK_(4+5+1));
          funcall(seq_init_start(STACK_(1+2)),2); # (SEQ-INIT-START seq1 start1)
          pushSTACK(value1); # =: pointer10
        }
        { pushSTACK(STACK_(5+5+1)); pushSTACK(STACK_(2+5+1+1));
          funcall(seq_init_start(STACK_(0+1+2)),2); # (SEQ-INIT-START seq2 start2)
          pushSTACK(value1); # =: pointer20
        }
        init_endvar(&STACK_(3+5+2)); # endvar10 := (and end1 (- end1 start1))
        init_endvar(&STACK_(1+5+3)); # endvar20 := (and end2 (- end2 start2))
        pushSTACK(STACK_(2+5+4)); # index := start2
        # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
        #              key, test, test-not, typdescr1, typdescr2,
        #              pointer10, pointer20, endvar10, endvar20, index.
        loop
          { # pointer1 und pointer2 ab pointer10 bzw. pointer20 laufen lassen:
            { pushSTACK(STACK_4); funcall(seq_copy(STACK_(1+5+1)),1); # (SEQ-COPY pointer10)
              pushSTACK(value1); # =: pointer1
            }
            { pushSTACK(STACK_(3+1)); funcall(seq_copy(STACK_(0+5+1+1)),1); # (SEQ-COPY pointer20)
              pushSTACK(value1); # =: pointer2
            }
            pushSTACK(STACK_(2+2)); # endvar1 := endvar10
            pushSTACK(STACK_(1+3)); # endvar2 := endvar20
            # Stackaufbau: seq1, seq2, from-end, start1, end1, start2, end2,
            #              key, test, test-not, typdescr1, typdescr2,
            #              pointer10, pointer20, endvar10, endvar20, index,
            #              pointer1, pointer2, endvar1, endvar2.
            loop
              { # Teste, ob seq1-Teilstück zu Ende. Wenn ja: gefunden.
                if (eq(STACK_1,Fixnum_0)) # endvar1 = 0 (und damit end1 /= nil) ?
                  { goto found; }
                  else
                  { pushSTACK(STACK_(6+5+5+4)); pushSTACK(STACK_(3+1));
                    funcall(seq_endtest(STACK_(1+5+4+2)),2); # (SEQ-ENDTEST seq1 pointer1)
                    if (!nullp(value1)) goto found;
                  }
                # seq1 ist noch nicht am Ende.
                # Teste, ob seq2-Teilstück zu Ende. Wenn ja: nicht gefunden.
                if (eq(STACK_0,Fixnum_0)) # endvar2 = 0 (und damit end2 /= nil) ?
                  { goto notfound; }
                  else
                  { pushSTACK(STACK_(5+5+5+4)); pushSTACK(STACK_(2+1));
                    funcall(seq_endtest(STACK_(0+5+4+2)),2); # (SEQ-ENDTEST seq2 pointer2)
                    if (!nullp(value1)) goto notfound;
                  }
                # seq2 ist noch nicht am Ende.
                pushSTACK(STACK_(6+5+5+4)); pushSTACK(STACK_(3+1));
                funcall(seq_access(STACK_(1+5+4+2)),2); # (SEQ-ACCESS seq1 pointer1)
                funcall_key(STACK_(4+5+4)); # (FUNCALL key (SEQ-ACCESS seq1 pointer1))
                pushSTACK(value1); # =: item1, retten
                pushSTACK(STACK_(5+5+5+4+1)); pushSTACK(STACK_(2+1+1));
                funcall(seq_access(STACK_(0+5+4+1+2)),2); # (SEQ-ACCESS seq2 pointer2)
                funcall_key(STACK_(4+5+4+1)); # (FUNCALL key (SEQ-ACCESS seq2 pointer2))
                {var object item2 = value1;
                 var object item1 = popSTACK();
                 # beide vergleichen:
                 if (!((*up2_fun)(&STACK_(8+5+4),item1,item2))) # Testroutine anwenden
                   break;
                }
                # Test erfüllt -> weitervergleichen:
                # pointer1 := (SEQ-UPD seq1 pointer1) :
                pointer_update(STACK_3,STACK_(6+5+5+4),STACK_(1+5+4));
                # pointer2 := (SEQ-UPD seq2 pointer2) :
                pointer_update(STACK_2,STACK_(5+5+5+4),STACK_(0+5+4));
                # endvar1 eventuell decrementieren:
                decrement_endvar(STACK_1);
                # endvar2 eventuell decrementieren:
                decrement_endvar(STACK_0);
              }
            # Test nicht erfüllt -> weitersuchen
            skipSTACK(4); # pointer1, pointer2, endvar1, endvar2 vergessen
            # pointer20 weiterrücken:
            pointer_update(STACK_3,STACK_(6+5+5),STACK_(0+5));
            # endvar20 eventuell decrementieren:
            decrement_endvar(STACK_1);
            # index incrementieren:
            increment(STACK_0);
      }   }
    /*NOTREACHED*/
    found: # index als Wert
      { VALUES1(STACK_4); skipSTACK(7+5+5+4); return; }
    notfound: # NIL als Wert
      { VALUES1(NIL); skipSTACK(7+5+5+4); return; }
  }}

# UP für SORT, STABLE-SORT und MERGE:
# merge(stackptr);
# sortiert zwei sortierte Sequence-Teile in eine dritte Sequence zusammen.
# > STACK_10: sequence1
# > STACK_9: typdescr1
# > STACK_8: sequence2
# > STACK_7: typdescr2
# > STACK_6: sequence3
# > STACK_5: typdescr3
# > STACK_4: count1 (ein Integer >=0)
# > STACK_3: count2 (ein Integer >=0)
# > STACK_2: pointer1
# > STACK_1: pointer2
# > STACK_0: pointer3
# > stackptr: Pointer in den Stack,
#     *(stackptr+0) = predicate, *(stackptr-1) = key
# count1+count2 Elemente aus sequence1 oder sequence2 werden nach sequence3
# übertragen (im Zweifelsfall die aus sequence1 zuerst).
# Dabei wird pointer1 genau  count1  mal weitergerückt (mit SEQ-UPD),
#            pointer2 genau  count2  mal weitergerückt (mit SEQ-UPD),
#            pointer3 genau  count1+count2  mal weitergerückt (mit SEQ-UPD).
# count1 und count2 werden auf 0 gesetzt.
# can trigger GC
  local void merge (object* stackptr);
  local void merge(stackptr)
    var object* stackptr;
    { loop
        { if (eq(STACK_4,Fixnum_0)) goto seq1_end; # count1 = 0 -> seq1 zu Ende
          if (eq(STACK_3,Fixnum_0)) goto seq2_end; # count1 = 0 -> seq2 zu Ende
          # item2 holen:
          { pushSTACK(STACK_8); pushSTACK(STACK_(1+1));
            funcall(seq_access(STACK_(7+2)),2); # (SEQ-ACCESS sequence2 pointer2)
            funcall_key(*(stackptr STACKop -1)); # (FUNCALL key (SEQ-ACCESS sequence2 pointer2))
            pushSTACK(value1); # =: item2
          }
          # item1 holen:
          { pushSTACK(STACK_(10+1)); pushSTACK(STACK_(2+1+1));
            funcall(seq_access(STACK_(9+1+2)),2); # (SEQ-ACCESS sequence1 pointer1)
            funcall_key(*(stackptr STACKop -1)); # (FUNCALL key (SEQ-ACCESS sequence1 pointer1))
            pushSTACK(value1); # =: item1
          }
          funcall(*(stackptr STACKop 0),2); # (FUNCALL predicate item2 item1)
          if (nullp(value1))
            # predicate lieferte NIL, item aus sequence1 übernehmen:
            { pushSTACK(STACK_(10)); pushSTACK(STACK_(2+1));
              funcall(seq_access(STACK_(9+2)),2); # (SEQ-ACCESS sequence1 pointer1)
              pushSTACK(value1); # auf den Stack
              # pointer1 := (SEQ-UPD sequence1 pointer1) :
              pointer_update(STACK_(2+1),STACK_(10+1),STACK_(9+1));
              # count1 := (1- count1) :
              decrement(STACK_(4+1));
            }
            else
            # predicate war erfüllt, item aus sequence2 übernehmen:
            { pushSTACK(STACK_(8)); pushSTACK(STACK_(1+1));
              funcall(seq_access(STACK_(7+2)),2); # (SEQ-ACCESS sequence2 pointer2)
              pushSTACK(value1); # auf den Stack
              # pointer2 := (SEQ-UPD sequence2 pointer2) :
              pointer_update(STACK_(1+1),STACK_(8+1),STACK_(7+1));
              # count2 := (1- count2) :
              decrement(STACK_(3+1));
            }
          {var object item = popSTACK(); # zu übernehmendes item
           pushSTACK(STACK_6); pushSTACK(STACK_(0+1)); pushSTACK(item);
           funcall(seq_access_set(STACK_(5+3)),3); # (SEQ-ACCESS-SET sequence3 pointer3 item)
          }
          # pointer3 := (SEQ-UPD sequence3 pointer3) :
          pointer_update(STACK_0,STACK_6,STACK_5);
        }
      /*NOTREACHED*/
      seq1_end:
        # sequence1 zu Ende. Rest aus sequence2 übernehmen:
        # Falls sequence2 und sequence3 EQ sind, liegt ein Aufruf
        # von SORT oder STABLE-SORT aus vor. Dort sind dann auch die
        # Pointer pointer2 und pointer3 gleich, also braucht gar nicht
        # mehr kopiert zu werden:
        if (eq(STACK_8,STACK_6)) # sequence2 = sequence3 ?
          { return; }
        until (eq(STACK_3,Fixnum_0)) # count2 = 0 ?
          { pushSTACK(STACK_(8)); pushSTACK(STACK_(1+1));
            funcall(seq_access(STACK_(7+2)),2); # (SEQ-ACCESS sequence2 pointer2)
            pushSTACK(STACK_6); pushSTACK(STACK_(0+1)); pushSTACK(value1);
            funcall(seq_access_set(STACK_(5+3)),3); # (SEQ-ACCESS-SET sequence3 pointer3 ...)
            # pointer2 := (SEQ-UPD sequence2 pointer2) :
            pointer_update(STACK_1,STACK_8,STACK_7);
            # count2 := (1- count2) :
            decrement(STACK_3);
            # pointer3 := (SEQ-UPD sequence3 pointer3) :
            pointer_update(STACK_0,STACK_6,STACK_5);
          }
        return;
      seq2_end:
        # sequence2 zu Ende, sequence1 nicht. Rest aus sequence1 nehmen:
        do { pushSTACK(STACK_(10)); pushSTACK(STACK_(2+1));
             funcall(seq_access(STACK_(9+2)),2); # (SEQ-ACCESS sequence1 pointer1)
             pushSTACK(STACK_6); pushSTACK(STACK_(0+1)); pushSTACK(value1);
             funcall(seq_access_set(STACK_(5+3)),3); # (SEQ-ACCESS-SET sequence3 pointer3 ...)
             # pointer1 := (SEQ-UPD sequence1 pointer1) :
             pointer_update(STACK_2,STACK_10,STACK_9);
             # count1 := (1- count1) :
             decrement(STACK_4);
             # pointer3 := (SEQ-UPD sequence3 pointer3) :
             pointer_update(STACK_0,STACK_6,STACK_5);
           }
           until (eq(STACK_4,Fixnum_0)); # count1 = 0 ?
        return;
    }

# UP: Sortiert in sequence ab pointer_left genau k Elemente (k >= 1)
# und liefert einen Pointer nach diesen k Elementen.
# sort_part(pointer_left,k,stackptr)
# pointer_left wird destruktiv verändert.
# > pointer_left
# > k
# > stackptr: Pointer in den Stack:
#       sequence, predicate [stackptr], key, start, end, typdescr, seq2
# < ergebnis: Pointer nach den k Elementen
# can trigger GC
  local object sort_part (object pointer_left, object k, object* stackptr);
  local object sort_part(pointer_left,k,stackptr)
    var object pointer_left;
    var object k;
    var object* stackptr;
    { if (eq(k,Fixnum_1))
        { # k=1. Fast nichts zu tun
          pushSTACK(*(stackptr STACKop 1)); pushSTACK(pointer_left);
          funcall(seq_upd(*(stackptr STACKop -4)),2); # (SEQ-UPD sequence pointer_left)
          return value1; # als Ergebnis
        }
        else
        { # k>1.
          pushSTACK(pointer_left);
          pushSTACK(k);
          pushSTACK(I_I_ash_I(k,Fixnum_minus1)); # (ASH k -1) = (FLOOR k 2) =: kl
          STACK_1 = I_I_minus_I(STACK_1,STACK_0); # (- k (FLOOR k 2)) = (CEILING k 2) =: kr
          # Stackaufbau: pointer_left, kr, kl.
          # mit kl = (floor k 2) und kr = (ceiling k 2), also k = (+ kl kr).
          # rekursiv die linke Hälfte sortieren:
          { pushSTACK(STACK_2); # pointer_left
            funcall(seq_copy(*(stackptr STACKop -4)),1); # (SEQ-COPY pointer_left)
           {var object pointer_mid = sort_part(value1,STACK_0,stackptr);
            pushSTACK(pointer_mid);
          }}
          # Stackaufbau: pointer_left, kr, kl, pointer_mid.
          # rekursiv die rechte Hälfte sortieren:
          { pushSTACK(STACK_0); # pointer_mid
            funcall(seq_copy(*(stackptr STACKop -4)),1); # (SEQ-COPY pointer_mid)
           {var object pointer_right = sort_part(value1,STACK_2,stackptr);
            pushSTACK(pointer_right);
          }}
          # Stackaufbau: pointer_left, kr, kl, pointer_mid, pointer_right.
          # Linke Hälfte (sortiert) nach seq2 kopieren:
          { var object typdescr = *(stackptr STACKop -4);
            pushSTACK(*(stackptr STACKop 1)); # sequence
            pushSTACK(typdescr); # typdescr
            pushSTACK(*(stackptr STACKop -5)); # seq2
            pushSTACK(typdescr); # typdescr
            pushSTACK(STACK_(2+4)); # kl
            { pushSTACK(STACK_(4+5)); # pointer_left
              funcall(seq_copy(typdescr),1); # (SEQ-COPY pointer_left)
              pushSTACK(value1); # =: pointer1
            }
            typdescr = STACK_2;
            { pushSTACK(STACK_3); # seq2
              funcall(seq_init(typdescr),1); # (SEQ-INIT seq2)
              pushSTACK(value1); # =: pointer2
            }
            # Stackaufbau: pointer_left, kr, kl, pointer_mid, pointer_right,
            #              sequence, typdescr, seq2, typdescr, kl, pointer1, pointer2.
            copy_seqpart_into(); # kopieren
            skipSTACK(3);
          }
          # Stackaufbau: pointer_left, kr, kl, pointer_mid, pointer_right,
          #              sequence, typdescr, seq2, typdescr.
          { pushSTACK(STACK_3); # sequence
            pushSTACK(STACK_(2+1)); # typdescr
            pushSTACK(STACK_(3+2)); # sequence
            pushSTACK(STACK_(2+3)); # typdescr
            pushSTACK(STACK_(2+4+4)); # kl
            pushSTACK(STACK_(3+4+5)); # kr
            { pushSTACK(STACK_(1+6)); # seq2
              funcall(seq_init(STACK_(0+6+1)),1); # (SEQ-INIT seq2)
              pushSTACK(value1); # als Source-Pointer in seq2
            }
            pushSTACK(STACK_(1+4+7)); # pointer_mid als Source in sequence
            pushSTACK(STACK_(4+4+8)); # pointer_left als Destination in sequence
            merge(stackptr); # von seq2 nach sequence hineinmergen
            { var object pointer_right = STACK_(0+4+9); # pointer_right
              skipSTACK(5+4+9);
              return pointer_right; # als Ergebnis
        } } }
    }

# UP für SORT und STABLE-SORT: Sortiert einen Teil einer Sequence.
# stable_sort();
# > Stackaufbau: sequence, predicate, key, start, end
# < mv_space/mv_count: Werte
# can trigger GC
  local Values stable_sort (void);
  local Values stable_sort()
    { # Stackaufbau: sequence, predicate, key, start, end.
      # sequence überprüfen:
      pushSTACK(get_valid_seq_type(STACK_4)); # typdescr
      # Stackaufbau: sequence, predicate, key, start, end, typdescr.
      # Defaultwert für start ist 0 :
      start_default_0(STACK_2);
      # Defaultwert für end:
      end_default_len(STACK_1,STACK_5,STACK_0);
      # Argumente start und end überprüfen:
      test_start_end(&O(kwpair_start),&STACK_1);
      # key überprüfen:
      test_key_arg(&STACK_7);
      # l := (- end start), ein Integer >=0
     {var object l = I_I_minus_I(STACK_1,STACK_2);
      pushSTACK(l);
      # Stackaufbau: sequence, predicate, key, start, end, typdescr, l.
      if (!(eq(l,Fixnum_0))) # Bei l=0 ist nichts zu tun
        { # Hilfssequence der Länge (floor l 2) erzeugen:
          { pushSTACK(I_I_ash_I(l,Fixnum_minus1)); # (ASH l -1) = (FLOOR l 2)
            funcall(seq_make(STACK_(1+1)),1); # (SEQ-MAKE (FLOOR l 2))
            pushSTACK(value1); # =: seq2
          }
          # Stackaufbau: sequence, predicate, key, start, end, typdescr, l,
          #              seq2.
          pushSTACK(STACK_(6+1)); pushSTACK(STACK_(3+1+1));
          funcall(seq_init_start(STACK_(1+1+2)),2); # (SEQ-INIT-START sequence start)
          l = STACK_(0+1); STACK_(0+1) = STACK_0; skipSTACK(1); # seq2 ersetzt l im Stack
          sort_part(value1,l,&STACK_5); # Stück der Länge l ab start sortieren
        }
      skipSTACK(6); VALUES1(popSTACK()); /* return sorted sequence */
    }}

LISPFUN(sort,2,0,norest,key,3, (kw(key),kw(start),kw(end)) )
# (SORT sequence predicate [:key] [:start] [:end]), CLTL S. 258
  { return_Values stable_sort(); }

LISPFUN(stable_sort,2,0,norest,key,3, (kw(key),kw(start),kw(end)) )
# (STABLE-SORT sequence predicate [:key] [:start] [:end]), CLTL S. 258
  { return_Values stable_sort(); }

LISPFUN(merge,4,0,norest,key,1, (kw(key)) )
# (MERGE result-type sequence1 sequence2 predicate [:key]), CLTL S. 260
  { # Stackaufbau: result-type, sequence1, sequence2, predicate, key.
    # key-Argument überprüfen:
    test_key_arg(&STACK_4);
    # sequence1 überprüfen:
    {var object seq1 = STACK_3;
     pushSTACK(seq1);
     pushSTACK(get_valid_seq_type(seq1));
    }
    # sequence2 überprüfen:
    {var object seq2 = STACK_(2+2);
     pushSTACK(seq2);
     pushSTACK(get_valid_seq_type(seq2));
    }
    # result-type überprüfen:
    {var object typdescr3 = valid_type(STACK_(4+4));
     pushSTACK(typdescr3);
    }
    # Stackaufbau: result-type, sequence1, sequence2, predicate, key,
    #              sequence1, typdescr1, sequence2, typdescr2, result-type-len, typdescr3.
    # Längen von sequence1 und sequence2 bestimmen:
    { pushSTACK(STACK_5); funcall(seq_length(STACK_(4+1)),1); # (SEQ-LENGTH sequence1)
      pushSTACK(value1); # =: len1
    }
    { pushSTACK(STACK_(3+1)); funcall(seq_length(STACK_(2+1+1)),1); # (SEQ-LENGTH sequence2)
      pushSTACK(value1); # =: len2
    }
    # beide Längen addieren und neue Sequence der Gesamtlänge bilden:
    { pushSTACK(I_I_plus_I(STACK_1,STACK_0)); # (+ len1 len2)
      if (boundp(STACK_(1+3)) && !eql(STACK_0,STACK_(1+3)))
        { fehler_seqtype_length(STACK_(1+3),STACK_0); }
      funcall(seq_make(STACK_(0+2+1)),1); # (SEQ-MAKE (+ len1 len2))
      STACK_(1+2) = value1; # ersetzt result-type-len im Stack
    }
    # Stackaufbau: result-type, sequence1, sequence2, predicate, key,
    #              sequence1, typdescr1, sequence2, typdescr2, sequence3, typdescr3,
    #              len1, len2.
    # Pointer an den Anfang der Sequences bestimmen:
    { pushSTACK(STACK_(5+2)); funcall(seq_init(STACK_(4+2+1)),1); # (SEQ-INIT sequence1)
      pushSTACK(value1); # =: pointer1
    }
    { pushSTACK(STACK_(3+2+1)); funcall(seq_init(STACK_(2+2+1+1)),1); # (SEQ-INIT sequence2)
      pushSTACK(value1); # =: pointer2
    }
    { pushSTACK(STACK_(1+2+2)); funcall(seq_init(STACK_(0+2+2+1)),1); # (SEQ-INIT sequence3)
      pushSTACK(value1); # =: pointer3
    }
    # Stackaufbau: result-type, sequence1, sequence2, predicate, key,
    #              sequence1, typdescr1, sequence2, typdescr2, sequence3, typdescr3,
    #              len1, len2, pointer1, pointer2, pointer3.
    # Merge-Operation durchführen:
    merge(&STACK_(1+6+5));
    VALUES1(STACK_(1+5)); /* return sequence3 */
    skipSTACK(5+6+5);
  }

LISPFUN(read_char_sequence,2,0,norest,key,2, (kw(start),kw(end)) )
# (READ-CHAR-SEQUENCE sequence stream [:start] [:end]), cf. dpANS S. 21-26
  { # Stackaufbau: sequence, stream, start, end.
    # sequence überprüfen:
    pushSTACK(get_valid_seq_type(STACK_3));
    # Stackaufbau: sequence, stream, start, end, typdescr.
    # Stream überprüfen:
    if (!streamp(STACK_3)) { fehler_stream(STACK_3); }
    # Defaultwert für start ist 0:
    start_default_0(STACK_2);
    # Defaultwert für end ist die Länge der Sequence:
    end_default_len(STACK_1,STACK_4,STACK_0);
    # start- und end-Argumente überprüfen:
    test_start_end(&O(kwpair_start),&STACK_1);
    if (eq(seq_type(STACK_0),S(string))) # Typname = STRING ?
      {  var uintL start = posfixnum_to_L(STACK_2);
         var uintL end = posfixnum_to_L(STACK_1);
         if (end-start == 0)
           { VALUES1(Fixnum_0); skipSTACK(5); return; }
       { var uintL index = 0;
         STACK_0 = array_displace_check(STACK_4,end,&index);
         check_sstring_mutable(STACK_0);
        {var uintL result = read_char_array(&STACK_3,&STACK_0,index+start,end-start);
         VALUES1(fixnum(start+result));
         skipSTACK(5);
         return;
      }}}
    # Durchlauf-Pointer bestimmen:
    pushSTACK(STACK_4); pushSTACK(STACK_(2+1));
    funcall(seq_init_start(STACK_(0+2)),2); # (SEQ-INIT-START sequence start)
    pushSTACK(value1); # =: pointer
    # Stackaufbau: sequence, stream, index, end, typdescr, pointer.
    until (eql(STACK_3,STACK_2)) # index = end (beides Integers) -> fertig
      { var object item = read_char(&STACK_4); # ein Element lesen
        if (eq(item,eof_value)) break; # EOF -> fertig
        pushSTACK(STACK_5); pushSTACK(STACK_(0+1)); pushSTACK(item);
        funcall(seq_access_set(STACK_(1+3)),3); # (SEQ-ACCESS-SET sequence pointer item)
        # pointer := (SEQ-UPD sequence pointer) :
        pointer_update(STACK_0,STACK_5,STACK_1);
        # index := (1+ index) :
        increment(STACK_3);
      }
    VALUES1(STACK_3); /* return index */
    skipSTACK(6);
  }

LISPFUN(write_char_sequence,2,0,norest,key,2, (kw(start),kw(end)) )
# (WRITE-CHAR-SEQUENCE sequence stream [:start] [:end]), cf. dpANS S. 21-27
  { # Stackaufbau: sequence, stream, start, end.
    # sequence überprüfen:
    pushSTACK(get_valid_seq_type(STACK_3));
    # Stackaufbau: sequence, stream, start, end, typdescr.
    # Stream überprüfen:
    if (!streamp(STACK_3)) { fehler_stream(STACK_3); }
    # Defaultwert für start ist 0:
    start_default_0(STACK_2);
    # Defaultwert für end ist die Länge der Sequence:
    end_default_len(STACK_1,STACK_4,STACK_0);
    # start- und end-Argumente überprüfen:
    test_start_end(&O(kwpair_start),&STACK_1);
    if (eq(seq_type(STACK_0),S(string))) # Typname = STRING ?
      { var uintL start = posfixnum_to_L(STACK_2);
        var uintL end = posfixnum_to_L(STACK_1);
        var uintL len = end-start;
        if (len == 0) goto done;
       {var uintL index = 0;
        STACK_0 = array_displace_check(STACK_4,end,&index);
        write_char_array(&STACK_3,&STACK_0,index+start,len);
        goto done;
      }}
    # start- und end-Argumente subtrahieren:
    STACK_1 = I_I_minus_I(STACK_1,STACK_2); # (- end start), ein Integer >=0
    # Stackaufbau: sequence, item, start, count, typdescr.
    # Durchlauf-Pointer bestimmen:
    pushSTACK(STACK_4); pushSTACK(STACK_(2+1));
    funcall(seq_init_start(STACK_(0+2)),2); # (SEQ-INIT-START sequence start)
    STACK_2 = value1; # =: pointer
    # Stackaufbau: sequence, stream, pointer, count, typdescr.
    until (eq(STACK_1,Fixnum_0)) # count (ein Integer) = 0 -> fertig
      { pushSTACK(STACK_4); pushSTACK(STACK_(2+1));
        funcall(seq_access(STACK_(0+2)),2); # (SEQ-ACCESS sequence pointer)
        write_char(&STACK_3,value1); # ein Element ausgeben
        # pointer := (SEQ-UPD sequence pointer) :
        pointer_update(STACK_2,STACK_4,STACK_0);
        # count := (1- count) :
        decrement(STACK_1);
      }
    done:
    skipSTACK(4);
    VALUES1(popSTACK()); /* return sequence */
  }

LISPFUN(read_byte_sequence,2,0,norest,key,2, (kw(start),kw(end)) )
# (READ-BYTE-SEQUENCE sequence stream [:start] [:end]), cf. dpANS S. 21-26
  { # Stackaufbau: sequence, stream, start, end.
    # sequence überprüfen:
    pushSTACK(get_valid_seq_type(STACK_3));
    # Stackaufbau: sequence, stream, start, end, typdescr.
    # Stream überprüfen:
    if (!streamp(STACK_3)) { fehler_stream(STACK_3); }
    # Defaultwert für start ist 0:
    start_default_0(STACK_2);
    # Defaultwert für end ist die Länge der Sequence:
    end_default_len(STACK_1,STACK_4,STACK_0);
    # start- und end-Argumente überprüfen:
    test_start_end(&O(kwpair_start),&STACK_1);
    if (eq(seq_type(STACK_0),fixnum(8))) # Typname = (VECTOR (UNSIGNED-BYTE 8)) ?
      { var uintL start = posfixnum_to_L(STACK_2);
        var uintL end = posfixnum_to_L(STACK_1);
        var uintL index = 0;
        STACK_0 = array_displace_check(STACK_4,end,&index);
       {var uintL result = read_byte_array(&STACK_3,&STACK_0,index+start,end-start);
        VALUES1(fixnum(start+result));
        skipSTACK(5);
        return;
      }}
    # Durchlauf-Pointer bestimmen:
    pushSTACK(STACK_4); pushSTACK(STACK_(2+1));
    funcall(seq_init_start(STACK_(0+2)),2); # (SEQ-INIT-START sequence start)
    pushSTACK(value1); # =: pointer
    # Stackaufbau: sequence, stream, index, end, typdescr, pointer.
    until (eql(STACK_3,STACK_2)) # index = end (beides Integers) -> fertig
      { var object item = read_byte(STACK_4); # ein Element lesen
        if (eq(item,eof_value)) break; # EOF -> fertig
        pushSTACK(STACK_5); pushSTACK(STACK_(0+1)); pushSTACK(item);
        funcall(seq_access_set(STACK_(1+3)),3); # (SEQ-ACCESS-SET sequence pointer item)
        # pointer := (SEQ-UPD sequence pointer) :
        pointer_update(STACK_0,STACK_5,STACK_1);
        # index := (1+ index) :
        increment(STACK_3);
      }
    VALUES1(STACK_3); /* return index */
    skipSTACK(6);
  }

LISPFUN(write_byte_sequence,2,0,norest,key,2, (kw(start),kw(end)) )
# (WRITE-BYTE-SEQUENCE sequence stream [:start] [:end]), cf. dpANS S. 21-27
  { # Stackaufbau: sequence, stream, start, end.
    # sequence überprüfen:
    pushSTACK(get_valid_seq_type(STACK_3));
    # Stackaufbau: sequence, stream, start, end, typdescr.
    # Stream überprüfen:
    if (!streamp(STACK_3)) { fehler_stream(STACK_3); }
    # Defaultwert für start ist 0:
    start_default_0(STACK_2);
    # Defaultwert für end ist die Länge der Sequence:
    end_default_len(STACK_1,STACK_4,STACK_0);
    # start- und end-Argumente überprüfen:
    test_start_end(&O(kwpair_start),&STACK_1);
    if (eq(seq_type(STACK_0),fixnum(8))) # Typname = (VECTOR (UNSIGNED-BYTE 8)) ?
      { var uintL start = posfixnum_to_L(STACK_2);
        var uintL end = posfixnum_to_L(STACK_1);
        var uintL index = 0;
        STACK_0 = array_displace_check(STACK_4,end,&index);
        write_byte_array(&STACK_3,&STACK_0,index+start,end-start);
        goto done;
      }
    # start- und end-Argumente subtrahieren:
    STACK_1 = I_I_minus_I(STACK_1,STACK_2); # (- end start), ein Integer >=0
    # Stackaufbau: sequence, item, start, count, typdescr.
    # Durchlauf-Pointer bestimmen:
    pushSTACK(STACK_4); pushSTACK(STACK_(2+1));
    funcall(seq_init_start(STACK_(0+2)),2); # (SEQ-INIT-START sequence start)
    STACK_2 = value1; # =: pointer
    # Stackaufbau: sequence, stream, pointer, count, typdescr.
    until (eq(STACK_1,Fixnum_0)) # count (ein Integer) = 0 -> fertig
      { pushSTACK(STACK_4); pushSTACK(STACK_(2+1));
        funcall(seq_access(STACK_(0+2)),2); # (SEQ-ACCESS sequence pointer)
        write_byte(STACK_3,value1); # ein Element ausgeben
        # pointer := (SEQ-UPD sequence pointer) :
        pointer_update(STACK_2,STACK_4,STACK_0);
        # count := (1- count) :
        decrement(STACK_1);
      }
    done:
    skipSTACK(4);
    VALUES1(popSTACK()); /* return sequence */
  }

