# Array functions
# Bruno Haible 1990-1999

#include "lispbibl.c"
#include "arilev0.c" # for bit_op, also defines mulu24 and mulu32_unchecked

# ============================================================================
#                    Global auxiliary functions

# Function: Copies a simple-vector.
# copy_svector(vector)
# > vector: simple-vector
# < result: fresh simple-vector with the same contents
# can trigger GC
  global object copy_svector (object vector);
  global object copy_svector(vector)
    var object vector;
    {
      var uintL length = Svector_length(vector);
      pushSTACK(vector);
      var object newvector = allocate_vector(length); # vector of same length
      vector = popSTACK();
      # copy the contens of vector into newvector:
      if (!(length==0)) {
        var object* ptr1 = &TheSvector(vector)->data[0];
        var object* ptr2 = &TheSvector(newvector)->data[0];
        dotimespL(length,length, {
          *ptr2++ = *ptr1++;
        });
      }
      return newvector;
    }

# Function: Copies a simple-bit/byte-vector.
# copy_sbvector(vector)
# > vector: simple-bit/byte-vector
# < result: fresh simple-bit/byte-vector with the same contents
# can trigger GC
  global object copy_sbvector (object vector);
  global object copy_sbvector(vector)
    var object vector;
    {
      var uintB atype = sbNvector_atype(vector);
      var uintL length = Sbvector_length(vector);
      pushSTACK(vector);
      var object newvector = allocate_bit_vector(atype,length); # vector of same length
      vector = popSTACK();
      if (!(length==0)) {
        var const uintB* ptr1 = &TheSbvector(vector)->data[0];
        var uintB* ptr2 = &TheSbvector(newvector)->data[0];
        dotimespL(length,ceiling(length<<atype,8), {
          *ptr2++ = *ptr1++;
        });
      }
      return newvector;
    }

LISPFUNN(copy_simple_vector,1)
# (SYS::%COPY-SIMPLE-VECTOR vector) returns a copy of the simple-vector vector.
  {
    var object obj = popSTACK();
    if (!simple_vector_p(obj))
      fehler_kein_svector(S(copy_simple_vector),obj);
    value1 = copy_svector(obj); mv_count=1;
  }

# Function: Returns the active length of a vector (same as LENGTH).
# vector_length(vector)
# > vector: a vector
# < result: its length
  global uintL vector_length (object vector);
  global uintL vector_length(vector)
    var object vector;
    {
      if (array_simplep(vector))
        return Sarray_length(vector);
      # Indirect Array
      var Iarray addr = TheIarray(vector);
      var uintL offset = offsetofa(iarray_,dims);
      if (iarray_flags(addr) & bit(arrayflags_dispoffset_bit))
        offset += sizeof(uintL);
      # The dimensions start at addr+offset.
      if (iarray_flags(addr) & bit(arrayflags_fillp_bit)) # possibly fill-pointer
        offset += sizeof(uintL);
      return *(uintL*)pointerplus(addr,offset);
    }

# Function: Canonicalizes an array element-type and returns its
# element type code.
# eltype_code(element_type)
# > element_type: type specifier
# < result: element type code Atype_xxx
# The canonicalized types are the possible results of ARRAY-ELEMENT-TYPE
# (symbols T, BIT, CHARACTER and lists (UNSIGNED-BYTE n)).
# The result type is a supertype of element_type.
# can trigger GC
  global uintB eltype_code (object element_type);
  global uintB eltype_code(obj)
    var object obj;
    # When this function is changed, also update upgraded-array-element-type in type.lsp!
    {
      # (cond ((eq obj 'BIT) Atype_Bit)
      #       ((eq obj 'CHARACTER) Atype_Char)
      #       ((eq obj 'T) Atype_T)
      #       (t (multiple-value-bind (low high) (sys::subtype-integer obj))
      #            ; Now (or (null low) (subtypep obj `(INTEGER ,low ,high)))
      #            (if (and (integerp low) (not (minusp low)) (integerp high))
      #              (let ((l (integer-length high)))
      #                ; Now (subtypep obj `(UNSIGNED-BYTE ,l))
      #                (cond ((<= l 1) Atype_Bit)
      #                      ((<= l 2) Atype_2Bit)
      #                      ((<= l 4) Atype_4Bit)
      #                      ((<= l 8) Atype_8Bit)
      #                      ((<= l 16) Atype_16Bit)
      #                      ((<= l 32) Atype_32Bit)
      #                      (t Atype_T)
      #              ) )
      #              (if (subtypep type 'CHARACTER)
      #                Atype_Char
      #                Atype_T
      # )     )  ) ) )
      if (eq(obj,S(bit))) { # symbol BIT ?
        return Atype_Bit;
      } elif (eq(obj,S(character))) { # symbol CHARACTER ?
        return Atype_Char;
      } elif (eq(obj,S(t))) { # symbol T ?
        return Atype_T;
      }
      pushSTACK(obj); pushSTACK(subr_self); # save obj and subr_self
      pushSTACK(obj); funcall(S(subtype_integer),1); # (SYS::SUBTYPE-INTEGER obj)
      subr_self = popSTACK(); obj = popSTACK(); # restore obj and subr_self
      if ((mv_count>1) && integerp(value1) && positivep(value1) && integerp(value2)) {
        var uintL l = I_integer_length(value2); # (INTEGER-LENGTH high)
        if (l<=1)
          return Atype_Bit;
        if (l<=2)
          return Atype_2Bit;
        if (l<=4)
          return Atype_4Bit;
        if (l<=8)
          return Atype_8Bit;
        if (l<=16)
          return Atype_16Bit;
        if (l<=32)
          return Atype_32Bit;
      }
      pushSTACK(obj); pushSTACK(S(character)); funcall(S(subtypep),2);
      if (!nullp(value1))
        return Atype_Char;
      return Atype_T;
    }

# Function: Creates a simple-vector with given elements.
# vectorof(len)
# > uintC len: desired vector length
# > STACK_(len-1), ..., STACK_(0): len objects
# < result: simple-vector containing these objects
# Pops n objects off STACK.
# can trigger GC
  global object vectorof (uintC len);
  global object vectorof(len)
    var uintC len;
    {
      var object new_vector = allocate_vector(len);
      if (len > 0) {
        var object* topargptr = STACK STACKop len;
        var object* argptr = topargptr;
        var object* ptr = &TheSvector(new_vector)->data[0];
        dotimespC(len,len, {
          *ptr++ = NEXT(argptr);
        });
        set_args_end_pointer(topargptr);
      }
      return new_vector;
    }

LISPFUN(vector,0,0,rest,nokey,0,NIL) # (VECTOR {object}), CLTL S. 290
  {
    value1 = vectorof(argcount); mv_count=1;
  }

# ============================================================================
#               Index checking, retrieving the storage vector

# An indirect array contains a pointer to another array: TheIarray(array)->data.
# The "storage vector" of an array is the a 1-dimensional array, of the same
# element type as the original array, without fill-pointer or adjustable bit.
# In can be obtained by repeatedly taking TheIarray(array)->data, until
# array satisfies array_simplep.

# Function: Follows the TheIarray(array)->data chain until the storage-vector
# is reached, and thereby sums up displaced-offsets. This function is useful
# for accessing a single array element.
# iarray_displace(array,&index);
# > array: indirect array
# > index: row-major-index
# < result: storage-vector
# < index: absolute index into the storage vector
# It is checked whether the addressed array element lies within the bounds of
# every intermediate array.
# It is not checked whether the chain is ultimately circular.
  local object iarray_displace (object array, uintL* index);
  local object iarray_displace(array,index)
    var object array;
    var uintL* index;
    {
      loop {
        if (*index >= TheIarray(array)->totalsize)
          goto fehler_bad_index;
        if (!(Iarray_flags(array) & bit(arrayflags_displaced_bit)))
          goto notdisplaced;
        # array is displaced
        *index += TheIarray(array)->dims[0]; # add displaced-offset
        array = TheIarray(array)->data; # next array in the chain
        if (array_simplep(array)) # next array indirect?
          goto simple;
      }
     notdisplaced:
      # array is indirect, but not displaced
      array = TheIarray(array)->data; # next array is the storage-vector
     simple:
      # have reached the storage-vector, not indirect
      if (*index >= Sarray_length(array))
        goto fehler_bad_index;
      return array;
     fehler_bad_index:
      fehler(error, # more details??
             GETTEXT("index too large")
            );
    }

# Error, when a displaced array does not fit into its target array.
  nonreturning_function(local, fehler_displaced_inconsistent, (void));
  local void fehler_displaced_inconsistent()
    {
      fehler(error,
             GETTEXT("An array has been shortened by adjusting it while another array was displaced to it.")
            );
    }

# Function: For an indirect array, returns the storage vector and the offset.
# Also verifies that all elements of the array are physically present.
# iarray_displace_check(array,size,&index)
# > object array: indirect array
# > uintL size: size
# < result: storage vector
# < index: is incremented by the offset into the storage vector
  global object iarray_displace_check (object array, uintL size, uintL* index);
  global object iarray_displace_check(array,size,index)
    var object array;
    var uintL size;
    var uintL* index;
    {
      loop {
        if (*index+size > TheIarray(array)->totalsize)
          goto fehler_bad_index;
        if (!(Iarray_flags(array) & bit(arrayflags_displaced_bit)))
          goto notdisplaced;
        # array is displaced
        *index += TheIarray(array)->dims[0]; # add displaced-offset
        array = TheIarray(array)->data; # next array in the chain
        if (array_simplep(array)) # next array indirect?
          goto simple;
      }
     notdisplaced:
      # array is indirect, but not displaced
      array = TheIarray(array)->data; # next array is the storage-vector
     simple:
      # have reached the storage-vector, not indirect
      if (*index+size > Sarray_length(array))
        goto fehler_bad_index;
      return array;
     fehler_bad_index:
      fehler_displaced_inconsistent();
    }

# Function: For an array, returns the storage vector and the offset.
# Also verifies that all elements of the array are physically present.
# array_displace_check(array,size,&index)
# > object array: array
# > uintL size: size
# < result: storage vector
# < index: is incremented by the offset into the storage vector
  global object array_displace_check (object array, uintL size, uintL* index);
  global object array_displace_check(array,size,index)
    var object array;
    var uintL size;
    var uintL* index;
    {
      if (array_simplep(array)) # array indirect?
        goto simple;
      loop {
        if (*index+size > TheIarray(array)->totalsize)
          goto fehler_bad_index;
        if (!(Iarray_flags(array) & bit(arrayflags_displaced_bit)))
          goto notdisplaced;
        # array is displaced
        *index += TheIarray(array)->dims[0]; # add displaced-offset
        array = TheIarray(array)->data; # next array in the chain
        if (array_simplep(array)) # next array indirect?
          goto simple;
      }
     notdisplaced:
      # array is indirect, but not displaced
      array = TheIarray(array)->data; # next array is the storage-vector
     simple:
      # have reached the storage-vector, not indirect
      if (*index+size > Sarray_length(array))
        goto fehler_bad_index;
      return array;
     fehler_bad_index:
      fehler_displaced_inconsistent();
    }

# ============================================================================
#                  Accessing and storing a single element

# Error message.
# > obj: non-array
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_array, (object obj));
  local void fehler_array(obj)
    var object obj;
    {
      pushSTACK(obj); # slot DATUM of TYPE-ERROR
      pushSTACK(S(array)); # slot EXPECTED-TYPE of TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not an array")
            );
    }

# Checks an array argument.
# > object: argument
# > subr_self: caller (a SUBR)
# test_array(object)
  #define test_array(object)  \
    if (!arrayp(object)) { fehler_array(object); }

# Returns the rank of an array.
# arrayrank(array)
# > array: an array
# < object result: rank as a fixnum
  #define arrayrank(array)  \
    (mdarrayp(array)                                               \
     ? fixnum((uintL)Iarray_rank(array)) # multi-dimensional array \
     : Fixnum_1 # vector has rank 1                                \
    )

# Error message
# > array: array
# > argcount: (wrong) number of subscripts
# > subr_self: caller (a SUBR)
  nonreturning_function(local, fehler_subscript_anz, (object array, uintC argcount));
  local void fehler_subscript_anz(array,argcount)
    var object array;
    var uintC argcount;
    {
      pushSTACK(arrayrank(array));
      pushSTACK(array);
      pushSTACK(fixnum(argcount));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: got ~ subscripts, but ~ has rank ~")
            );
    }

# Error message
# > argcount: number of subscripts
# > STACK_(argcount): array
# > STACK_(argcount-1),...,STACK_(0): subscripts
# > subr_self: caller (a SUBR)
  nonreturning_function(local, fehler_subscript_type, (uintC argcount));
  local void fehler_subscript_type(argcount)
    var uintC argcount;
    {
      var object list = listof(argcount); # list of subscripts
      # STACK_0 is now the array.
      pushSTACK(list);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: subscripts ~ for ~ are not of type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))")
            );
    }

# Error message
# > argcount: number of subscripts
# > STACK_(argcount): array
# > STACK_(argcount-1),...,STACK_(0): subscripts
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_subscript_range, (uintC argcount, uintL subscript, uintL bound));
  local void fehler_subscript_range(argcount,subscript,bound)
    var uintC argcount;
    var uintL subscript;
    var uintL bound;
    {
      var object list = listof(argcount); # list of subscripts
      pushSTACK(list);
      # On STACK: array, subscript-list.
      pushSTACK(UL_to_I(subscript)); # slot DATUM of TYPE-ERROR
      {
        var object tmp;
        pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(bound));
        tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
        pushSTACK(tmp); # slot EXPECTED-TYPE of TYPE-ERROR
      }
      pushSTACK(STACK_(1+2));
      pushSTACK(STACK_(0+3));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: subscripts ~ for ~ are out of range")
            );
    }

# Überprüft Subscripts für einen AREF/STORE-Zugriff, entfernt sie vom STACK
# und liefert den Row-Major-Index (>=0, <arraysize_limit).
# test_subscripts(array,argptr,argcount)
# > array : nicht-simpler Array
# > argptr : Pointer über die Subscripts
# > argcount : Anzahl der Subscripts
# < ergebnis : row-major-index
  local uintL test_subscripts (object array, object* argptr, uintC argcount);
  local uintL test_subscripts(array,argptr,argcount)
    var object array;
    var object* argptr;
    var uintC argcount;
    {
      var object* args_pointer = argptr; # argptr retten für später
      # Anzahl der Subscripts überprüfen:
      if (!(argcount == Iarray_rank(array))) # sollte = Rang sein
        fehler_subscript_anz(array,argcount);
      # Subscripts selbst überprüfen:
      var uintL row_major_index = 0;
      var const uintL* dimptr = &TheIarray(array)->dims[0]; # Zeiger auf Dimensionen
      if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
        dimptr++; # evtl. Displaced-Offset überspringen
      {
        var uintC count;
        dotimesC(count,argcount, {
          var object subscriptobj = NEXT(argptr); # Subscript als Objekt
          if (!(posfixnump(subscriptobj))) # Subscript muss Fixnum>=0 sein.
            fehler_subscript_type(argcount);
          var uintL subscript = posfixnum_to_L(subscriptobj); # als uintL
          var uintL dim = *dimptr++; # entsprechende Dimension
          if (!(subscript<dim)) # Subscript muss kleiner als Dimension sein
            fehler_subscript_range(argcount,subscript,dim);
          # Bilde row_major_index := row_major_index*dim+subscript:
          row_major_index =
            mulu32_unchecked(row_major_index,dim)+subscript;
          # Das gibt keinen Überlauf, weil dies
          # < Produkt der bisherigen Dimensionen
          # <= Produkt aller Dimensionen < arraysize_limit <= 2^32
          # ist. (Ausnahme: Falls eine spätere Dimension =0 ist.
          # Aber dann gibt's nachher sowieso eine Fehlermeldung.)
        });
      }
      set_args_end_pointer(args_pointer);
      return row_major_index;
    }

# Fehlermeldung
# > STACK_1: Array (meist Vektor)
# > STACK_0: (fehlerhafter) Index
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_index_type, (void));
  local void fehler_index_type()
    {
      pushSTACK(STACK_0); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_array_index)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(STACK_(1+2));
      pushSTACK(STACK_(0+3));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: index ~ for ~ is not of type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))")
            );
    }

# Fehlermeldung
# > STACK_1: Array (meist Vektor)
# > STACK_0: (fehlerhafter) Index
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_index_range, (uintL bound));
  global void fehler_index_range(bound)
    var uintL bound;
    {
      var object tmp;
      pushSTACK(STACK_0); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(bound));
      tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
      pushSTACK(tmp); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(STACK_(1+2));
      pushSTACK(STACK_(0+3));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: index ~ for ~ is out of range")
            );
    }

# Überprüft einen Index für einen AREF/STORE-Zugriff in einen simplen Vektor.
# test_index()
# > STACK_1: simpler Vektor
# > STACK_0: Index
# < ergebnis: Index als uintL
  local uintL test_index (void);
  local uintL test_index()
    {
      if (!posfixnump(STACK_0)) # Index muss Fixnum>=0 sein.
        fehler_index_type();
      var uintL index = posfixnum_to_L(STACK_0); # Index als uintL
      if (!(index < Sarray_length(STACK_1))) # Index muss kleiner als Länge sein
        fehler_index_range(Sarray_length(STACK_1));
      return index;
    }

# Überprüft Subscripts für einen AREF/STORE-Zugriff, entfernt sie vom STACK
# und liefert den Row-Major-Index (>=0, <arraysize_limit) und den Datenvektor.
# subscripts_to_index(array,argptr,argcount, &index)
# > array : nicht-simpler Array
# > argptr : Pointer über die Subscripts
# > argcount : Anzahl der Subscripts
# < index : Index in den Datenvektor
# < ergebnis : der Datenvektor
  local object subscripts_to_index (object array, object* argptr, uintC argcount, uintL* index_);
  local object subscripts_to_index(array,argptr,argcount,index_)
    var object array;
    var object* argptr;
    var uintC argcount;
    var uintL* index_;
    {
      test_array(array); # Array überprüfen
      if (array_simplep(array)) {
        # simpler Vektor, wird getrennt behandelt:
        # Anzahl der Subscripts überprüfen:
        if (!(argcount == 1)) # sollte = 1 sein
          fehler_subscript_anz(array,argcount);
        # Subscript selbst überprüfen:
        *index_ = test_index(); # Index = Row-Major-Index = Subscript
        skipSTACK(1); return array;
      } else {
        # nicht-simpler Array
        # Subscripts überprüfen, Row-Major-Index errechnen, STACK aufräumen:
        *index_ = test_subscripts(array,argptr,argcount);
        # Datenvektor und absoluten Index holen:
        return iarray_displace(array,&(*index_));
      }
    }

# Function: Performs an AREF access.
# storagevector_aref(storagevector,index)
# > storagevector: a storage vector (simple vector or semi-simple byte vector)
# > index: (already checked) index into the storage vector
# < result: (AREF storagevector index)
# can trigger GC (only for element type (UNSIGNED-BYTE 32))
  global object storagevector_aref (object datenvektor, uintL index);
  global object storagevector_aref(datenvektor,index)
    var object datenvektor;
    var uintL index;
    {
      switch (Array_type(datenvektor)) {
        case Array_type_svector: # Simple-Vector
          return TheSvector(datenvektor)->data[index];
        case Array_type_sbvector: # Simple-Bit-Vector
          return ( sbvector_btst(datenvektor,index) ? Fixnum_1 : Fixnum_0 );
        case Array_type_sb2vector:
          return fixnum((TheSbvector(datenvektor)->data[index/4]>>(2*((~index)%4)))&(bit(2)-1));
        case Array_type_sb4vector:
          return fixnum((TheSbvector(datenvektor)->data[index/2]>>(4*((~index)%2)))&(bit(4)-1));
        case Array_type_sb8vector:
          return fixnum(TheSbvector(datenvektor)->data[index]);
        case Array_type_sb16vector:
          return fixnum(((uint16*)&TheSbvector(datenvektor)->data[0])[index]);
        case Array_type_sb32vector:
          return UL_to_I(((uint32*)&TheSbvector(datenvektor)->data[0])[index]);
        case Array_type_sstring: # Simple-String
          SstringDispatch(datenvektor,
            { return code_char(TheSstring(datenvektor)->data[index]); },
            { return code_char(as_chart(TheSmallSstring(datenvektor)->data[index])); }
            );
        default: NOTREACHED
      }
    }

# Error when attempting to store an invalid value in an array.
# fehler_store(array,value);
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(global, fehler_store, (object array, object value));
  global void fehler_store(array,value)
    var object array;
    var object value;
    {
      pushSTACK(array);
      pushSTACK(value); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(array_element_type(array)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(STACK_(0+2)); # array
      pushSTACK(STACK_2); # value
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ does not fit into ~, bad type")
            );
    }

# Führt einen STORE-Zugriff aus.
# storagevector_store(datenvektor,index,element)
# > datenvektor : ein Datenvektor (simpler Vektor oder semi-simpler Byte-Vektor)
# > index : (geprüfter) Index in den Datenvektor
# > element : (ungeprüftes) einzutragendes Objekt
# > STACK_0 : array (für Fehlermeldung)
# > subr_self: Aufrufer (ein SUBR)
  local void storagevector_store (object datenvektor, uintL index, object element);
  local void storagevector_store(datenvektor,index,element)
    var object datenvektor;
    var uintL index;
    var object element;
    {
      switch (Array_type(datenvektor)) {
        case Array_type_svector: # Simple-Vector
          TheSvector(datenvektor)->data[index] = element;
          return;
        case Array_type_sbvector: # Simple-Bit-Vector
          {
            var uintB* addr = &TheSbvector(datenvektor)->data[index/8];
            var uintL bitnummer = (~index)%8; # 7 - (index mod 8)
            if (eq(element,Fixnum_0)) {
              *addr &= ~bit(bitnummer);
              return;
            } elif (eq(element,Fixnum_1)) {
              *addr |= bit(bitnummer);
              return;
            }
          }
          break;
        case Array_type_sb2vector:
          {
            var uintL wert;
            if (posfixnump(element) && ((wert = posfixnum_to_L(element)) < bit(2))) {
              var uintB* ptr = &TheSbvector(datenvektor)->data[index/4];
              *ptr ^= (*ptr ^ (wert<<(2*((~index)%4)))) & ((bit(2)-1)<<(2*((~index)%4)));
              return;
            }
          }
          break;
        case Array_type_sb4vector:
          {
            var uintL wert;
            if (posfixnump(element) && ((wert = posfixnum_to_L(element)) < bit(4))) {
              var uintB* ptr = &TheSbvector(datenvektor)->data[index/2];
              *ptr ^= (*ptr ^ (wert<<(4*((~index)%2)))) & ((bit(4)-1)<<(4*((~index)%2)));
              return;
            }
          }
          break;
        case Array_type_sb8vector:
          {
            var uintL wert;
            if (posfixnump(element) && ((wert = posfixnum_to_L(element)) < bit(8))) {
              TheSbvector(datenvektor)->data[index] = wert;
              return;
            }
          }
          break;
        case Array_type_sb16vector:
          {
            var uintL wert;
            if (posfixnump(element) && ((wert = posfixnum_to_L(element)) < bit(16))) {
              ((uint16*)&TheSbvector(datenvektor)->data[0])[index] = wert;
              return;
            }
          }
          break;
        case Array_type_sb32vector:
          ((uint32*)&TheSbvector(datenvektor)->data[0])[index] = I_to_UL(element); # evtl. Fehlermeldung macht I_to_UL
          return;
        #ifndef TYPECODES
        case Rectype_Imm_Sstring: case Rectype_Imm_SmallSstring: # immutable Simple-String
          fehler_sstring_immutable(datenvektor);
        case Rectype_Sstring: # mutable Simple-String
        #else
        case Array_type_sstring: # Simple-String
        #endif
          if (charp(element)) {
            TheSstring(datenvektor)->data[index] = char_code(element);
            return;
          }
          break;
        default: NOTREACHED
      }
      # Objekt war vom falschen Typ.
      fehler_store(STACK_0,element);
    }

LISPFUN(aref,1,0,rest,nokey,0,NIL) # (AREF array {subscript}), CLTL S. 290
  {
    var object array = Before(rest_args_pointer); # Array holen
    # Subscripts verarbeiten und Datenvektor und Index holen:
    var uintL index;
    var object datenvektor = subscripts_to_index(array,rest_args_pointer,argcount, &index);
    # Element des Datenvektors holen:
    value1 = storagevector_aref(datenvektor,index); mv_count=1;
    skipSTACK(1);
  }

LISPFUN(store,2,0,rest,nokey,0,NIL) # (SYS::STORE array {subscript} object)
                     # = (SETF (AREF array {subscript}) object), CLTL S. 291
  {
    rest_args_pointer skipSTACKop 1; # Pointer über ersten Subscript
    var object element = popSTACK();
    var object array = Before(rest_args_pointer); # Array holen
    # Subscripts verarbeiten und Datenvektor und Index holen:
    var uintL index;
    var object datenvektor = subscripts_to_index(array,rest_args_pointer,argcount, &index);
    # Element in den Datenvektor eintragen:
    storagevector_store(datenvektor,index,element);
    value1 = element; mv_count=1;
    skipSTACK(1);
  }

# Fehlermeldung
# > STACK_1: Nicht-Simple-Vector
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_svector, (void));
  local void fehler_svector()
    {
      fehler_kein_svector(TheSubr(subr_self)->name,STACK_1);
    }

LISPFUNN(svref,2) # (SVREF simple-vector index), CLTL S. 291
  {
    # simple-vector überprüfen:
    if (!simple_vector_p(STACK_1))
      fehler_svector();
    # index überprüfen:
    var uintL index = test_index();
    # Element holen:
    value1 = TheSvector(STACK_1)->data[index]; mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(svstore,3) # (SYS::SVSTORE simple-vector index element)
                    # = (SETF (SVREF simple-vector index) element), CLTL S. 291
  {
    var object element = popSTACK();
    # simple-vector überprüfen:
    if (!simple_vector_p(STACK_1))
      fehler_svector();
    # index überprüfen:
    var uintL index = test_index();
    # Element ablegen:
    TheSvector(STACK_1)->data[index] = element;
    value1 = element; mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(psvstore,3) # (SYS::%SVSTORE element simple-vector index)
                     # = (SETF (SVREF simple-vector index) element)
  {
    # simple-vector überprüfen:
    if (!simple_vector_p(STACK_1))
      fehler_svector();
    # index überprüfen:
    var uintL index = test_index();
    # Element ablegen:
    value1 = TheSvector(STACK_1)->data[index] = STACK_2; mv_count=1;
    skipSTACK(3);
  }

LISPFUNN(row_major_aref,2)
# (ROW-MAJOR-AREF array index), CLtL2 S. 450
  {
    var object array = STACK_1;
    # Array überprüfen:
    test_array(array);
    # index überprüfen:
    if (!posfixnump(STACK_0))
      fehler_index_type();
    var uintL index = posfixnum_to_L(STACK_0);
    if (!(index < array_total_size(array))) # Index muss kleiner als Größe sein
      fehler_index_range(array_total_size(array));
    if (!array_simplep(array))
      { array = iarray_displace(array,&index); }
    value1 = storagevector_aref(array,index); mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(row_major_store,3)
# (SYS::ROW-MAJOR-STORE array index element)
# = (SETF (ROW-MAJOR-AREF array index) element), CLtL2 S. 450
  {
    var object element = popSTACK();
    var object array = STACK_1;
    # Array überprüfen:
    test_array(array);
    # index überprüfen:
    if (!posfixnump(STACK_0))
      fehler_index_type();
    var uintL index = posfixnum_to_L(STACK_0);
    if (!(index < array_total_size(array))) # Index muss kleiner als Größe sein
      fehler_index_range(array_total_size(array));
    if (!array_simplep(array))
      { array = iarray_displace(array,&index); }
    storagevector_store(array,index,element);
    value1 = element; mv_count=1;
    skipSTACK(2);
  }

# ============================================================================
#                  Information about an array

# Function: Returns the element-type of an array.
# array_element_type(array)
# > array: an array
# < result: element-type, one of the symbols T, BIT, CHARACTER, or a list
# can trigger GC
  global object array_element_type (object array);
  global object array_element_type(array)
    var object array;
    {
      var uintBWL atype;
      switch (Array_type(array)) {
        case Array_type_sstring:
        case Array_type_string: # String -> CHARACTER
          return S(character);
        case Array_type_sbvector: # Simple-Bit-Vector -> BIT
        case Array_type_bvector: # Bit-Vector -> BIT
          return S(bit);
        case Array_type_sb2vector: # Simple-2Bit-Vector -> (UNSIGNED-BYTE 2)
        case Array_type_sb4vector: # Simple-4Bit-Vector -> (UNSIGNED-BYTE 4)
        case Array_type_sb8vector: # Simple-8Bit-Vector -> (UNSIGNED-BYTE 8)
        case Array_type_sb16vector: # Simple-16Bit-Vector -> (UNSIGNED-BYTE 16)
        case Array_type_sb32vector: # Simple-32Bit-Vector -> (UNSIGNED-BYTE 32)
          atype = sbNvector_atype(array);
          break;
        case Array_type_b2vector: # 2Bit-Vector -> (UNSIGNED-BYTE 2)
        case Array_type_b4vector: # 4Bit-Vector -> (UNSIGNED-BYTE 4)
        case Array_type_b8vector: # 8Bit-Vector -> (UNSIGNED-BYTE 8)
        case Array_type_b16vector: # 16Bit-Vector -> (UNSIGNED-BYTE 16)
        case Array_type_b32vector: # 32Bit-Vector -> (UNSIGNED-BYTE 32)
          atype = bNvector_atype(array);
          break;
        case Array_type_svector:
        case Array_type_vector: # allg. Vector -> T
          return S(t);
        case Array_type_mdarray: # allgemeiner Array
          atype = Iarray_flags(array) & arrayflags_atype_mask;
          switch (atype) {
            case Atype_T:           return S(t);         # T
            case Atype_Bit:         return S(bit);       # BIT
            case Atype_Char:        return S(character); # CHARACTER
            case Atype_2Bit:        # (UNSIGNED-BYTE 2)
            case Atype_4Bit:        # (UNSIGNED-BYTE 4)
            case Atype_8Bit:        # (UNSIGNED-BYTE 8)
            case Atype_16Bit:       # (UNSIGNED-BYTE 16)
            case Atype_32Bit:       # (UNSIGNED-BYTE 32)
              break;
            default: NOTREACHED
          }
          break;
        default: NOTREACHED
      }
      pushSTACK(S(unsigned_byte));
      pushSTACK(fixnum(bit(atype)));
      return listof(2);
    }

LISPFUNN(array_element_type,1) # (ARRAY-ELEMENT-TYPE array), CLTL S. 291
  {
    var object array = popSTACK();
    test_array(array);
    value1 = array_element_type(array); mv_count=1;
  }

LISPFUNN(array_rank,1) # (ARRAY-RANK array), CLTL S. 292
  {
    var object array = popSTACK();
    test_array(array);
    value1 = arrayrank(array); mv_count=1;
  }

LISPFUNN(array_dimension,2) # (ARRAY-DIMENSION array axis-number), CLTL S. 292
  {
    var object axis_number = popSTACK();
    var object array = popSTACK();
    test_array(array);
    if (array_simplep(array)) {
      # simpler Vektor: axis-number muss =0 sein, Wert ist dann die Länge.
      if (eq(axis_number,Fixnum_0)) {
        value1 = fixnum(Sarray_length(array));
        mv_count=1; return;
      } else
        goto fehler_axis;
    } else {
      # nicht-simpler Array
      if (posfixnump(axis_number)) { # axis-number muss ein Fixnum >=0,
        var uintL axis = posfixnum_to_L(axis_number);
        if (axis < (uintL)Iarray_rank(array)) { # und <rank sein
          var uintL* dimptr = &TheIarray(array)->dims[0]; # Zeiger auf Dimensionen
          if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
            dimptr++; # evtl. Displaced-Offset überspringen
          value1 = fixnum(dimptr[axis]);
          mv_count=1; return;
        } else
          goto fehler_axis;
      } else
        goto fehler_axis;
    }
   fehler_axis:
    pushSTACK(array);
    pushSTACK(axis_number); # Wert für Slot DATUM von TYPE-ERROR
    {
      var object tmp;
      pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(arrayrank(array));
      tmp = listof(1); pushSTACK(tmp); tmp = listof(3); pushSTACK(tmp); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
    }
    pushSTACK(STACK_2); # array
    pushSTACK(STACK_2); # axis_number
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~: ~ is not an nonnegative integer less than the rank of ~")
          );
  }

# Function: Returns the list of dimensions of an array.
# array_dimensions(array)
# > array: an array
# < result: list of its dimensions
# can trigger GC
  global object array_dimensions (object array);
  global object array_dimensions(array)
    var object array;
    {
      if (array_simplep(array)) {
        # simpler Vektor, bilde (LIST length)
        var object len # Länge als Fixnum (nicht GC-gefährdet)
          = fixnum(Sarray_length(array));
        var object new_cons = allocate_cons();
        Car(new_cons) = len; Cdr(new_cons) = NIL;
        return new_cons;
      } else {
        # nicht-simpler Array: alle Dimensionen als Fixnums auf den STACK,
        # dann eine Liste daraus machen.
        var uintC rank = Iarray_rank(array);
        if (rank > 0) {
          var uintL* dimptr = &TheIarray(array)->dims[0]; # Zeiger auf Dimensionen
          if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
            dimptr++; # evtl. Displaced-Offset überspringen
          get_space_on_STACK(sizeof(object)*(uintL)rank); # STACK überprüfen
          {
            var uintC count;
            dotimespC(count,rank, {
              # nächste Dimension als Fixnum in den Stack:
              pushSTACK(fixnum(*dimptr++));
            });
          }
        }
        return listof(rank); # Liste bilden
      }
    }

LISPFUNN(array_dimensions,1) # (ARRAY-DIMENSIONS array), CLTL S. 292
  {
    var object array = popSTACK();
    test_array(array);
    value1 = array_dimensions(array); mv_count=1;
  }

# Function: Returns the dimensions of an array and their partial products.
# iarray_dims_sizes(array,&dims_sizes);
# > array: indirect array of rank r
# > struct { uintL dim; uintL dimprod; } dims_sizes[r]: room for the result
# < for i=1,...r:  dims_sizes[r-i] = { Dim_i, Dim_i * ... * Dim_r }
  global void iarray_dims_sizes (object array, array_dim_size* dims_sizes);
  global void iarray_dims_sizes(array,dims_sizes)
    var object array;
    var array_dim_size* dims_sizes;
    {
      var uintC r = Iarray_rank(array); # Rang
      if (r > 0) {
        var const uintL* dimptr = &TheIarray(array)->dims[0]; # Zeiger auf Dimensionen
        if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
          dimptr++; # evtl. Displaced-Offset überspringen
        dimptr = &dimptr[(uintL)r]; # Zeiger hinter die Dimensionen
        var uintL produkt = 1;
        dotimespC(r,r, { # Schleife über die r Dimensionen von hinten
          var uintL dim = *--dimptr; # nächste Dimension
          produkt = mulu32_unchecked(produkt,dim); # aufs Produkt multiplizieren
          # Das gibt keinen Überlauf, weil dies
          # < Produkt der bisherigen Dimensionen
          # <= Produkt aller Dimensionen < arraysize_limit <= 2^32 ist.
          # (Ausnahme: Falls eine Dimension kleinerer Nummer =0 ist.
          # Aber dann ist das jetzige Produkt sowieso irrelevant, da
          # jede Schleife über diese Dimension eine Leerschleife ist.)
          dims_sizes->dim = dim; dims_sizes->dimprod = produkt;
          dims_sizes++;
        });
      }
    }

LISPFUNN(array_total_size,1) # (ARRAY-TOTAL-SIZE array), CLTL S. 292
  {
    var object array = popSTACK();
    test_array(array);
    value1 = fixnum(array_total_size(array));
    mv_count=1;
  }

LISPFUN(array_in_bounds_p,1,0,rest,nokey,0,NIL)
# (ARRAY-IN-BOUNDS-P array {subscript}), CLTL S. 292
  {
    var object* argptr = rest_args_pointer;
    var object array = BEFORE(rest_args_pointer); # Array holen
    test_array(array); # Array überprüfen
    if (array_simplep(array)) {
      # simpler Vektor, wird getrennt behandelt:
      # Anzahl der Subscripts überprüfen:
      if (!(argcount == 1)) # sollte = 1 sein
        fehler_subscript_anz(array,argcount);
      # Subscript selbst überprüfen:
      var object subscriptobj = STACK_0; # Subscript als Objekt
      if (!integerp(subscriptobj)) # muss Integer sein
         fehler_index_type();
      # Subscript muss Fixnum>=0 sein,
      # Subscript als uintL muss kleiner als Länge sein:
      if (!( posfixnump(subscriptobj)
             && (posfixnum_to_L(subscriptobj) < Sarray_length(array)) ))
        goto no;
      goto yes;
    } else {
      # nicht-simpler Array
      # Anzahl der Subscripts überprüfen:
      if (!(argcount == Iarray_rank(array))) # sollte = Rang sein
        fehler_subscript_anz(array,argcount);
      # Subscripts selbst überprüfen:
      if (argcount > 0) {
        var uintL* dimptr = &TheIarray(array)->dims[0]; # Zeiger auf Dimensionen
        if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
          dimptr++; # evtl. Displaced-Offset überspringen
        var uintC count;
        dotimespC(count,argcount, {
          var object subscriptobj = NEXT(argptr); # Subscript als Objekt
          if (!integerp(subscriptobj)) # muss Integer sein
            fehler_subscript_type(argcount);
          # Subscript muss Fixnum>=0 sein,
          # Subscript als uintL muss kleiner als die entsprechende Dimension sein:
          if (!( posfixnump(subscriptobj)
                 && (posfixnum_to_L(subscriptobj) < *dimptr++) ))
            goto no;
        });
      }
      goto yes;
    }
   yes:
    value1 = T; mv_count=1; set_args_end_pointer(rest_args_pointer); return;
   no:
    value1 = NIL; mv_count=1; set_args_end_pointer(rest_args_pointer); return;
  }

LISPFUN(array_row_major_index,1,0,rest,nokey,0,NIL)
# (ARRAY-ROW-MAJOR-INDEX array {subscript}), CLTL S. 293
  {
    var object array = Before(rest_args_pointer); # Array holen
    var uintL index;
    test_array(array); # Array überprüfen
    if (array_simplep(array)) {
      # simpler Vektor, wird getrennt behandelt:
      # Anzahl der Subscripts überprüfen:
      if (!(argcount == 1)) # sollte = 1 sein
        fehler_subscript_anz(array,argcount);
      # Subscript selbst überprüfen:
      test_index();
      value1 = popSTACK(); mv_count=1; # Index = Row-Major-Index = Subscript
      skipSTACK(1);
    } else {
      # nicht-simpler Array
      # Subscripts überprüfen, Row-Major-Index errechnen, STACK aufräumen:
      index = test_subscripts(array,rest_args_pointer,argcount);
      # Index als Fixnum zurück:
      value1 = fixnum(index); mv_count=1;
      skipSTACK(1);
    }
  }

LISPFUNN(adjustable_array_p,1) # (ADJUSTABLE-ARRAY-P array), CLTL S. 293
  {
    var object array = popSTACK(); # Argument holen
    test_array(array); # Array überprüfen
    if (array_simplep(array))
      goto no; # simpler Vektor, ist nicht adjustable
    else
      if (Iarray_flags(array) & bit(arrayflags_adjustable_bit))
        goto yes;
      else
        goto no;
   yes:
    value1 = T; mv_count=1; return;
   no:
    value1 = NIL; mv_count=1; return;
  }

LISPFUNN(array_displacement,1) # (ARRAY-DISPLACEMENT array), CLHS
  {
    var object array = popSTACK(); # Argument holen
    test_array(array); # Array überprüfen
    if (!array_simplep(array)
        && (Iarray_flags(array) & bit(arrayflags_displaced_bit))
       ) {
      value1 = TheIarray(array)->data; # nächster Array
      value2 = fixnum(TheIarray(array)->dims[0]); # displaced-Offset
    } else {
      value1 = NIL;
      value2 = Fixnum_0;
    }
    mv_count=2;
  }

# ============================================================================
#                      Bit arrays and bit vectors

# Fehlermeldung
# fehler_bit_array()
# > STACK_0: Array, der kein Bit-Array ist
  nonreturning_function(local, fehler_bit_array, (void));
  local void fehler_bit_array()
    {
      pushSTACK(STACK_0); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_array_bit)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(STACK_(0+2));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: ~ is not an array of bits")
            );
    }

LISPFUN(bit,1,0,rest,nokey,0,NIL) # (BIT bit-array {subscript}), CLTL S. 293
  {
    var object array = Before(rest_args_pointer); # Array holen
    # Subscripts verarbeiten und Datenvektor und Index holen:
    var uintL index;
    var object datenvektor = subscripts_to_index(array,rest_args_pointer,argcount, &index);
    if (!simple_bit_vector_p(Atype_Bit,datenvektor))
      fehler_bit_array();
    # Datenvektor ist ein Simple-Bit-Vector. Element des Datenvektors holen:
    value1 = ( sbvector_btst(datenvektor,index) ? Fixnum_1 : Fixnum_0 ); mv_count=1;
    skipSTACK(1);
  }

LISPFUN(sbit,1,0,rest,nokey,0,NIL) # (SBIT bit-array {subscript}), CLTL S. 293
  {
    var object array = Before(rest_args_pointer); # Array holen
    # Subscripts verarbeiten und Datenvektor und Index holen:
    var uintL index;
    var object datenvektor = subscripts_to_index(array,rest_args_pointer,argcount, &index);
    if (!simple_bit_vector_p(Atype_Bit,datenvektor))
      fehler_bit_array();
    # Datenvektor ist ein Simple-Bit-Vector. Element des Datenvektors holen:
    value1 = ( sbvector_btst(datenvektor,index) ? Fixnum_1 : Fixnum_0 ); mv_count=1;
    skipSTACK(1);
  }

# Für Unterprogramme für Bitvektoren:
  # Man arbeitet mit Bit-Blöcken à bitpack Bits.
  # uint_bitpack ist ein unsigned Integer mit bitpack Bits.
  # uint_2bitpack ist ein unsigned Integer mit 2*bitpack Bits.
  # R_bitpack(x) liefert die rechte (untere) Hälfte eines uint_2bitpack.
  # L_bitpack(x) liefert die linke (obere) Hälfte eines uint_2bitpack.
  # LR_2bitpack(x,y) liefert zu x,y das aus der linken Hälfte x und der
  #                  rechten Hälfte y zusammengesetzte uint_2bitpack.
  # Verwende LR_0_bitpack(y) falls x=0, LR_bitpack_0(x) falls y=0.
  #if defined(WIDE_HARD) && BIG_ENDIAN_P && (varobject_alignment%4 == 0)
    # Bei Big-Endian-64-bit-Maschinen kann man gleich mit 32 Bit auf einmal
    # arbeiten (sofern varobject_alignment durch 4 Byte teilbar ist):
    #define bitpack  32
    #define uint_bitpack  uint32
    #define uint_2bitpack  uint64
    #define R_bitpack(x)  ((uint32)(uint64)(x))
    #define L_bitpack(x)  ((uint32)((uint64)(x)>>32))
    #define LR_2bitpack(x,y)  (((uint64)(uint32)(x)<<32)|(uint64)(uint32)(y))
    #define LR_0_bitpack(y)  ((uint64)(uint32)(y))
    #define LR_bitpack_0(x)  ((uint64)(uint32)(x)<<32)
  #elif BIG_ENDIAN_P && (varobject_alignment%2 == 0)
    # Bei Big-Endian-Maschinen kann man gleich mit 16 Bit auf einmal arbeiten
    # (sofern varobject_alignment durch 2 Byte teilbar ist):
    #define bitpack  16
    #define uint_bitpack  uint16
    #define uint_2bitpack  uint32
    #define R_bitpack(x)  low16(x)
    #define L_bitpack(x)  high16(x)
    #define LR_2bitpack(x,y)  highlow32(x,y)
    #define LR_0_bitpack(y)  ((uint32)(uint16)(y))
    #define LR_bitpack_0(x)  highlow32_0(x)
  #else
    # Sonst kann man nur 8 Bit auf einmal nehmen:
    #define bitpack  8
    #define uint_bitpack  uint8
    #define uint_2bitpack  uint16
    #define R_bitpack(x)  ((uint_bitpack)(uint_2bitpack)(x))
    #define L_bitpack(x)  ((uint_bitpack)((uint_2bitpack)(x) >> bitpack))
    #define LR_2bitpack(x,y)  \
      (((uint_2bitpack)(uint_bitpack)(x) << bitpack)  \
       | (uint_2bitpack)(uint_bitpack)(y)             \
      )
    #define LR_0_bitpack(y)  LR_2bitpack(0,y)
    #define LR_bitpack_0(x)  LR_2bitpack(x,0)
  #endif

# Function: Compares two slices of simple-bit-vectors.
# bit_compare(array1,index1,array2,index2,count)
# > array1: first simple-bit-vector
# > index1: absolute index into array1
# > array2: second simple-bit-vector
# > index2: absolute index into array2
# > count: number of bits to be compared, > 0
# < result: TRUE, if both slices are the same, bit for bit, else FALSE.
  global boolean bit_compare (object array1, uintL index1,
                              object array2, uintL index2,
                              uintL bitcount);
  global boolean bit_compare(array1,index1,array2,index2,bitcount)
    var object array1;
    var uintL index1;
    var object array2;
    var uintL index2;
    var uintL bitcount;
    {
      var const uint_bitpack* ptr1 = &((uint_bitpack*)(&TheSbvector(array1)->data[0]))[index1/bitpack];
      var const uint_bitpack* ptr2 = &((uint_bitpack*)(&TheSbvector(array2)->data[0]))[index2/bitpack];
      # ptr1 zeigt auf das erste teilnehmende Word des 1. Bit-Arrays.
      # ptr2 zeigt auf das erste teilnehmende Word des 2. Bit-Arrays.
      index1 = index1 % bitpack; # Bit-Offset im 1. Bit-Array
      index2 = index2 % bitpack; # Bit-Offset im 2. Bit-Array
      if (index1 == index2) {
        # Erstes Word behandeln:
        if (!(index1 == 0)) {
          var uintL count1 = bitpack - index1;
          if (count1 >= bitcount) {
            # Vergleiche Bits bitpack-index1-1..bitpack-index1-bitcount
            # in *ptr1 und *ptr2.
            return (((*ptr1 ^ *ptr2) & (bit(count1)-bit(count1-bitcount))) == 0);
          }
          if (((*ptr1 ^ *ptr2) & (bit(count1)-1)) != 0)
            return FALSE;
          ptr1++;
          ptr2++;
          bitcount -= count1; # still > 0
        }
        # Ab jetzt kann man index1 = index2 = 0 annehmen.
        var uintL bitpackcount = bitcount / bitpack;
        # bitpackcount = Anzahl der ganzen Words
        var uintL bitcount_rest = bitcount % bitpack;
        # bitcount_rest = Anzahl der übrigbleibenden Bits
        # einfache Schleife, da alle Bit-Offsets im Word =0 sind:
        dotimesL(bitpackcount,bitpackcount, {
          if (!(*ptr1++ == *ptr2++))
            return FALSE;
        });
        # bitcount_rest = Anzahl der noch vergleichenden Bits
        if (!(bitcount_rest==0)) {
          # letztes Word vergleichen:
          if (!(( (*ptr1 ^ *ptr2)
                  & # Bitmaske mit Bits bitpack-1..bitpack-bitcount_rest gesetzt
                    ~( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest)
                ) ==0
             ) )
            return FALSE;
        }
        return TRUE;
      } else {
        # kompliziertere Schleife:
        var uintL bitpackcount = bitcount / bitpack;
        # bitpackcount = Anzahl der ganzen Words
        var uintL bitcount_rest = bitcount % bitpack;
        # bitcount_rest = Anzahl der übrigbleibenden Bits
        # We distinguish three cases in order to avoid a memory overrun bug.
        # The tighter loops are just an added benefit for speed.
        if (index1 == 0) {
          # index1 = 0, index2 > 0.
          var uint_2bitpack carry2 = LR_bitpack_0((*ptr2++) << index2);
          # carry2 hat in seinen oberen bitpack-index2 Bits (Bits 2*bitpack-1..bitpack+index2)
          # die betroffenen Bits des 1. Words des 2. Arrays, sonst Nullen.
          dotimesL(bitpackcount,bitpackcount, {
            # Vergleichsschleife (jeweils wortweise):
            # Nach n>=0 Schleifendurchläufen ist
            # ptr1 um n, und ptr2 um n+1 Words weitergerückt, also Pointer aufs
            # nächste zu lesende Word des 1. bzw. 2. Arrays,
            # bitpackcount = Zahl zu verknüpfender ganzer Words - n,
            # carry2 = Übertrag vom 2. Array
            #          (in den bitpack-index2 oberen Bits, sonst Null).
            if (!(
                  *ptr1++
                  ==
                  ( carry2 |=
                      LR_0_bitpack(*ptr2++) # nächstes Word des 2. Arrays lesen
                      << index2, # zum carry2 dazunehmen
                    L_bitpack(carry2) # und davon das linke Word verwenden
                  )
               ) )
              return FALSE;
            carry2 = LR_bitpack_0(R_bitpack(carry2)); # carry2 := rechtes Word von carry2
          });
          # Noch bitcount_rest Bits zu vergleichen:
          if (!(bitcount_rest==0)) {
            # letztes Word vergleichen:
            if (!(( (
                     *ptr1++
                     ^
                     ( carry2 |=
                         LR_0_bitpack(*ptr2++) # nächstes Word des 2. Arrays lesen
                         << index2, # zum carry2 dazunehmen
                       L_bitpack(carry2) # und davon das linke Word verwenden
                     )
                    )
                    & # Bitmaske mit Bits bitpack-1..bitpack-bitcount_rest gesetzt
                      ~( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest)
                  ) ==0
               ) )
              return FALSE;
          }
          return TRUE;
        } elif (index2 == 0) {
          # index1 > 0, index2 = 0.
          var uint_2bitpack carry1 = LR_bitpack_0((*ptr1++) << index1);
          # carry1 hat in seinen oberen bitpack-index1 Bits (Bits 2*bitpack-1..bitpack+index1)
          # die betroffenen Bits des 1. Words des 1. Arrays, sonst Nullen.
          dotimesL(bitpackcount,bitpackcount, {
            # Vergleichsschleife (jeweils wortweise):
            # Nach n>=0 Schleifendurchläufen ist
            # ptr1 um n+1, und ptr2 um n Words weitergerückt, also Pointer aufs
            # nächste zu lesende Word des 1. bzw. 2. Arrays,
            # bitpackcount = Zahl zu verknüpfender ganzer Words - n,
            # carry1 = Übertrag vom 1. Array
            #          (in den bitpack-index1 oberen Bits, sonst Null),
            if (!(
                  ( carry1 |=
                      LR_0_bitpack(*ptr1++) # nächstes Word des 1. Arrays lesen
                      << index1, # zum carry1 dazunehmen
                    L_bitpack(carry1) # und davon das linke Word verwenden
                  )
                  ==
                  *ptr2++
               ) )
              return FALSE;
            carry1 = LR_bitpack_0(R_bitpack(carry1)); # carry1 := rechtes Word von carry1
          });
          # Noch bitcount_rest Bits zu vergleichen:
          if (!(bitcount_rest==0)) {
            # letztes Word vergleichen:
            if (!(( (
                     ( carry1 |=
                         LR_0_bitpack(*ptr1++) # nächstes Word des 1. Arrays lesen
                         << index1, # zum carry1 dazunehmen
                       L_bitpack(carry1) # und davon das linke Word verwenden
                     )
                     ^
                     *ptr2++
                    )
                    & # Bitmaske mit Bits bitpack-1..bitpack-bitcount_rest gesetzt
                      ~( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest)
                  ) ==0
               ) )
              return FALSE;
          }
          return TRUE;
        } else {
          var uint_2bitpack carry1 = LR_bitpack_0((*ptr1++) << index1);
          # carry1 hat in seinen oberen bitpack-index1 Bits (Bits 2*bitpack-1..bitpack+index1)
          # die betroffenen Bits des 1. Words des 1. Arrays, sonst Nullen.
          var uint_2bitpack carry2 = LR_bitpack_0((*ptr2++) << index2);
          # carry2 hat in seinen oberen bitpack-index2 Bits (Bits 2*bitpack-1..bitpack+index2)
          # die betroffenen Bits des 1. Words des 2. Arrays, sonst Nullen.
          dotimesL(bitpackcount,bitpackcount, {
            # Vergleichsschleife (jeweils wortweise):
            # Nach n>=0 Schleifendurchläufen ist
            # ptr1 und ptr2 um n+1 Words weitergerückt, also Pointer aufs
            # nächste zu lesende Word des 1. bzw. 2. Arrays,
            # bitpackcount = Zahl zu verknüpfender ganzer Words - n,
            # carry1 = Übertrag vom 1. Array
            #          (in den bitpack-index1 oberen Bits, sonst Null),
            # carry2 = Übertrag vom 2. Array
            #          (in den bitpack-index2 oberen Bits, sonst Null).
            if (!(
                  ( carry1 |=
                      LR_0_bitpack(*ptr1++) # nächstes Word des 1. Arrays lesen
                      << index1, # zum carry1 dazunehmen
                    L_bitpack(carry1) # und davon das linke Word verwenden
                  )
                  ==
                  ( carry2 |=
                      LR_0_bitpack(*ptr2++) # nächstes Word des 2. Arrays lesen
                      << index2, # zum carry2 dazunehmen
                    L_bitpack(carry2) # und davon das linke Word verwenden
                  )
               ) )
              return FALSE;
            carry1 = LR_bitpack_0(R_bitpack(carry1)); # carry1 := rechtes Word von carry1
            carry2 = LR_bitpack_0(R_bitpack(carry2)); # carry2 := rechtes Word von carry2
          });
          # Noch bitcount_rest Bits zu vergleichen:
          if (!(bitcount_rest==0)) {
            # letztes Word vergleichen:
            if (!(( (
                     ( carry1 |=
                         LR_0_bitpack(*ptr1++) # nächstes Word des 1. Arrays lesen
                         << index1, # zum carry1 dazunehmen
                       L_bitpack(carry1) # und davon das linke Word verwenden
                     )
                     ^
                     ( carry2 |=
                         LR_0_bitpack(*ptr2++) # nächstes Word des 2. Arrays lesen
                         << index2, # zum carry2 dazunehmen
                       L_bitpack(carry2) # und davon das linke Word verwenden
                     )
                    )
                    & # Bitmaske mit Bits bitpack-1..bitpack-bitcount_rest gesetzt
                      ~( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest)
                  ) ==0
               ) )
              return FALSE;
          }
          return TRUE;
        }
      }
    }

# Function: Copies a slice of a simple-bit-vector into another
# simple-bit-vector.
# bit_copy(array1,index1,array2,index2,count);
# > array1: source simple-bit-vector
# > index1: absolute index into array1
# > array2: destination simple-bit-vector
# > index2: absolute index into array2
# > count: number of bits to be copied, > 0
  local void bit_copy (object array1, uintL index1,
                       object array2, uintL index2,
                       uintL bitcount);
  local void bit_copy(array1,index1,array2,index2,bitcount)
    var object array1;
    var uintL index1;
    var object array2;
    var uintL index2;
    var uintL bitcount;
    {
      var const uint_bitpack* ptr1 = &((uint_bitpack*)(&TheSbvector(array1)->data[0]))[index1/bitpack];
      var uint_bitpack* ptr2 = &((uint_bitpack*)(&TheSbvector(array2)->data[0]))[index2/bitpack];
      # ptr1 point to the first affected word in array1.
      # ptr2 point to the first affected word in array2.
      index1 = index1 % bitpack; # bit-offset in array1
      index2 = index2 % bitpack; # bit-offset in array2
      if (index1 == index2) {
        # Treat the first word.
        if (!(index1 == 0)) {
          var uintL count1 = bitpack - index1;
          if (count1 >= bitcount) {
            # copy bits bitpack-index1-1..bitpack-index1-bitcount
            # from *ptr1 to *ptr2.
            *ptr2 ^= (bit(count1)-bit(count1-bitcount)) & (*ptr2 ^ *ptr1);
            return;
          }
          *ptr2 ^= (bit(count1)-1) & (*ptr2 ^ *ptr1);
          ptr1++;
          ptr2++;
          bitcount -= count1; # still > 0
        }
        # We can now assume index1 = index2 = 0.
        var uintL bitpackcount = bitcount / bitpack;
        # bitpackcount = number of complete words
        var uintL bitcount_rest = bitcount % bitpack;
        # bitcount_rest = number of remaining bits
        # simple loop, since all bit offsets are 0.
        dotimesL(bitpackcount,bitpackcount, {
          *ptr2++ = *ptr1++;
        });
        if (!(bitcount_rest==0))
          *ptr2 ^= ~( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest) & (*ptr2 ^ *ptr1);
      } else {
        var uintL bitpackcount = bitcount / bitpack;
        # bitpackcount = number of complete words
        var uintL bitcount_rest = bitcount % bitpack;
        # bitcount_rest = number of remaining bits
        var uint_2bitpack carry2 =
          LR_bitpack_0( ( ~ ( (uint_bitpack)(bitm(bitpack)-1) >> index2) ) & *ptr2 );
        # The upper index2 bits of carry2 are exactly those bits of *ptr2
        # which must not be modified.
        # We distinguish two cases in order to avoid a memory overrun bug.
        # The tighter loop is just an added benefit for speed.
        if (index1 == 0) {
          # index1 = 0.
          loop {
            # After n>=0 rounds ptr1 has advanced by n words, i.e. it points
            # to the next word to be read, and ptr2 has advanced by n words, i.e.
            # it points to the next word to be written. bitpackcount has been
            # decremented by n.
            carry2 |= LR_bitpack_0(*ptr1++) >> index2;
            if (bitpackcount==0)
              break;
            *ptr2++ = L_bitpack(carry2);
            carry2 = LR_bitpack_0(R_bitpack(carry2));
            bitpackcount--;
          }
        } else {
          # index1 > 0.
          var uint_2bitpack carry1 = LR_bitpack_0((*ptr1++) << index1);
          # The upper bitpack-index1 bits of carry1 are the affected bits of
          # the first word of array1. The other bits in carry1 are zero.
          loop {
            # After n>=0 rounds ptr1 has advanced by n+1 words, i.e. it points
            # to the next word to be read, and ptr2 has advanced by n words, i.e.
            # it points to the next word to be written. bitpackcount has been
            # decremented by n.
            var uint_bitpack temp =
              (carry1 |= LR_0_bitpack(*ptr1++) << index1, L_bitpack(carry1));
            carry1 = LR_bitpack_0(R_bitpack(carry1));
            carry2 |= LR_bitpack_0(temp) >> index2;
            if (bitpackcount==0)
              break;
            *ptr2++ = L_bitpack(carry2);
            carry2 = LR_bitpack_0(R_bitpack(carry2));
            bitpackcount--;
          }
        }
        # Special handling for the last word (now containd in the bits
        # 2*bitpack-index2-1..bitpack-index2 of carry2): Only bitcount_rest
        # bits must be stored in array2.
        bitcount_rest = index2+bitcount_rest;
        var uint_bitpack last_carry;
        if (bitcount_rest>=bitpack) {
          *ptr2++ = L_bitpack(carry2);
          last_carry = R_bitpack(carry2);
          bitcount_rest -= bitpack;
        } else {
          last_carry = L_bitpack(carry2);
        }
        if (!(bitcount_rest==0))
          *ptr2 ^= (*ptr2 ^ last_carry) & (~( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest ));
      }
    }

# Unterprogramm für Bitvektor-Operationen:
# bit_op(array1,index1,array2,index2,array3,index3,op,count);
# > array1: erster Bit-Array,
# > index1: absoluter Index in array1
# > array2: zweiter Bit-Array,
# > index2: absoluter Index in array2
# > array3: dritter Bit-Array,
# > index3: absoluter Index in array3
# > op: Adresse der Operationsroutine
# > count: Anzahl der zu verknüpfenden Bits, > 0
  # bit_op_fun ist eine Funktion, die zwei bitpack-Bit-Wörter verknüpft:
  typedef uint_bitpack bit_op_fun (uint_bitpack x, uint_bitpack y);
  local void bit_op (object array1, uintL index1,
                     object array2, uintL index2,
                     object array3, uintL index3,
                     bit_op_fun* op, uintL bitcount);
  local void bit_op(array1,index1,array2,index2,array3,index3,op,bitcount)
    var object array1;
    var uintL index1;
    var object array2;
    var uintL index2;
    var object array3;
    var uintL index3;
    var bit_op_fun* op;
    var uintL bitcount;
    {
      var const uint_bitpack* ptr1 = &((uint_bitpack*)(&TheSbvector(array1)->data[0]))[index1/bitpack];
      var const uint_bitpack* ptr2 = &((uint_bitpack*)(&TheSbvector(array2)->data[0]))[index2/bitpack];
      var uint_bitpack* ptr3 = &((uint_bitpack*)(&TheSbvector(array3)->data[0]))[index3/bitpack];
      # ptr1 zeigt auf das erste teilnehmende Word des 1. Bit-Arrays.
      # ptr2 zeigt auf das erste teilnehmende Word des 2. Bit-Arrays.
      # ptr3 zeigt auf das erste teilnehmende Word des 3. Bit-Arrays.
      var uintL bitpackcount = bitcount / bitpack;
      # bitpackcount = Anzahl der ganzen Words
      var uintL bitcount_rest = bitcount % bitpack;
      # bitcount_rest = Anzahl der übrigbleibenden Bits
      index1 = index1 % bitpack; # Bit-Offset im 1. Bit-Array
      index2 = index2 % bitpack; # Bit-Offset im 2. Bit-Array
      index3 = index3 % bitpack; # Bit-Offset im 3. Bit-Array
      if ((index1==0) && (index2==0) && (index3==0)) {
        # einfache Schleife, da alle Bit-Offsets im Word =0 sind:
        dotimesL(bitpackcount,bitpackcount, {
          *ptr3++ = (*op)(*ptr1++,*ptr2++);
        });
        # bitcount_rest = Anzahl der noch abzulegenden Bits
        if (!(bitcount_rest==0)) {
          # letztes Word ablegen:
          var uint_bitpack temp = (*op)(*ptr1,*ptr2);
          *ptr3 =
            ( ~
                ( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest)
                # Bitmaske mit Bits bitpack-bitcount_rest-1..0 gesetzt
              # Bitmaske mit Bits bitpack-1..bitpack-bitcount_rest gesetzt
             &
             (*ptr3 ^ temp)
            ) # zu ändernde Bits
            ^ *ptr3
            ;
        }
      } else {
        # kompliziertere Schleife:
        var uint_2bitpack carry3 =
          LR_bitpack_0(
                       (~
                          ( (uint_bitpack)(bitm(bitpack)-1) >> index3)
                          # Bitmaske mit Bits bitpack-index3-1..0 gesetzt
                       ) # Bitmaske mit Bits bitpack-1..bitpack-index3 gesetzt
                       & (*ptr3)
                      );
        # carry3 hat in seinen obersten index3 Bits (Bits 2*bitpack-1..2*bitpack-index3)
        # genau die Bits von *ptr3, die nicht verändert werden dürfen.
        # We distinguish four cases in order to avoid a memory overrun bug.
        # The tighter loops are just an added benefit for speed.
        if (index1 == 0) {
          if (index2 == 0) {
            # index1 = 0, index2 = 0.
            loop {
              # Verknüpfungsschleife (jeweils wortweise):
              # Nach n>=0 Schleifendurchläufen ist
              # ptr1 und ptr2 um n Words weitergerückt, also Pointer aufs
              # nächste zu lesende Word des 1. bzw. 2. Arrays,
              # ptr3 um n Words weitergerückt, also Pointer aufs
              # nächste zu schreibende Word des 3. Arrays,
              # bitpackcount = Zahl zu verknüpfender ganzer Words - n,
              # carry3 = Übertrag noch abzuspeichernder Bits
              #          (in den index3 oberen Bits, sonst Null).
              var uint_bitpack temp =
                (*op)(
                      *ptr1++,
                      *ptr2++
                     ) ; # beide durch *op verknüpfen
              carry3 |= LR_bitpack_0(temp) >> index3;
              # Die oberen bitpack+index3 Bits von carry3 sind abzulegen.
              if (bitpackcount==0)
                break;
              *ptr3++ = L_bitpack(carry3); # bitpack Bits davon ablegen
              carry3 = LR_bitpack_0(R_bitpack(carry3)); # und index3 Bits für später behalten.
              bitpackcount--;
            }
          } else {
            # index1 = 0, index2 > 0.
            var uint_2bitpack carry2 = LR_bitpack_0((*ptr2++) << index2);
            # carry2 hat in seinen oberen bitpack-index2 Bits (Bits 2*bitpack-1..bitpack+index2)
            # die betroffenen Bits des 1. Words des 2. Arrays, sonst Nullen.
            loop {
              # Verknüpfungsschleife (jeweils wortweise):
              # Nach n>=0 Schleifendurchläufen ist
              # ptr1 um n, und ptr2 um n+1 Words weitergerückt, also Pointer aufs
              # nächste zu lesende Word des 1. bzw. 2. Arrays,
              # ptr3 um n Words weitergerückt, also Pointer aufs
              # nächste zu schreibende Word des 3. Arrays,
              # bitpackcount = Zahl zu verknüpfender ganzer Words - n,
              # carry2 = Übertrag vom 2. Array
              #          (in den bitpack-index2 oberen Bits, sonst Null),
              # carry3 = Übertrag noch abzuspeichernder Bits
              #          (in den index3 oberen Bits, sonst Null).
              var uint_bitpack temp =
                (*op)(
                      *ptr1++,
                      ( carry2 |=
                          LR_0_bitpack(*ptr2++) # nächstes Word des 2. Arrays lesen
                          << index2, # zum carry2 dazunehmen
                        L_bitpack(carry2) # und davon das linke Word verwenden
                      )
                     ) ; # beide durch *op verknüpfen
              carry2 = LR_bitpack_0(R_bitpack(carry2)); # carry2 := rechtes Word von carry2
              carry3 |= LR_bitpack_0(temp) >> index3;
              # Die oberen bitpack+index3 Bits von carry3 sind abzulegen.
              if (bitpackcount==0)
                break;
              *ptr3++ = L_bitpack(carry3); # bitpack Bits davon ablegen
              carry3 = LR_bitpack_0(R_bitpack(carry3)); # und index3 Bits für später behalten.
              bitpackcount--;
            }
          }
        } else {
          if (index2 == 0) {
            # index1 > 0, index2 = 0.
            var uint_2bitpack carry1 = LR_bitpack_0((*ptr1++) << index1);
            # carry1 hat in seinen oberen bitpack-index1 Bits (Bits 2*bitpack-1..bitpack+index1)
            # die betroffenen Bits des 1. Words des 1. Arrays, sonst Nullen.
            loop {
              # Verknüpfungsschleife (jeweils wortweise):
              # Nach n>=0 Schleifendurchläufen ist
              # ptr1 um n+1, und ptr2 um n Words weitergerückt, also Pointer aufs
              # nächste zu lesende Word des 1. bzw. 2. Arrays,
              # ptr3 um n Words weitergerückt, also Pointer aufs
              # nächste zu schreibende Word des 3. Arrays,
              # bitpackcount = Zahl zu verknüpfender ganzer Words - n,
              # carry1 = Übertrag vom 1. Array
              #          (in den bitpack-index1 oberen Bits, sonst Null),
              # carry3 = Übertrag noch abzuspeichernder Bits
              #          (in den index3 oberen Bits, sonst Null).
              var uint_bitpack temp =
                (*op)(
                      ( carry1 |=
                          LR_0_bitpack(*ptr1++) # nächstes Word des 1. Arrays lesen
                          << index1, # zum carry1 dazunehmen
                        L_bitpack(carry1) # und davon das linke Word verwenden
                      ),
                      *ptr2++
                     ) ; # beide durch *op verknüpfen
              carry1 = LR_bitpack_0(R_bitpack(carry1)); # carry1 := rechtes Word von carry1
              carry3 |= LR_bitpack_0(temp) >> index3;
              # Die oberen bitpack+index3 Bits von carry3 sind abzulegen.
              if (bitpackcount==0)
                break;
              *ptr3++ = L_bitpack(carry3); # bitpack Bits davon ablegen
              carry3 = LR_bitpack_0(R_bitpack(carry3)); # und index3 Bits für später behalten.
              bitpackcount--;
            }
          } else {
            # index1 > 0, index2 > 0.
            var uint_2bitpack carry1 = LR_bitpack_0((*ptr1++) << index1);
            # carry1 hat in seinen oberen bitpack-index1 Bits (Bits 2*bitpack-1..bitpack+index1)
            # die betroffenen Bits des 1. Words des 1. Arrays, sonst Nullen.
            var uint_2bitpack carry2 = LR_bitpack_0((*ptr2++) << index2);
            # carry2 hat in seinen oberen bitpack-index2 Bits (Bits 2*bitpack-1..bitpack+index2)
            # die betroffenen Bits des 1. Words des 2. Arrays, sonst Nullen.
            loop {
              # Verknüpfungsschleife (jeweils wortweise):
              # Nach n>=0 Schleifendurchläufen ist
              # ptr1 und ptr2 um n+1 Words weitergerückt, also Pointer aufs
              # nächste zu lesende Word des 1. bzw. 2. Arrays,
              # ptr3 um n Words weitergerückt, also Pointer aufs
              # nächste zu schreibende Word des 3. Arrays,
              # bitpackcount = Zahl zu verknüpfender ganzer Words - n,
              # carry1 = Übertrag vom 1. Array
              #          (in den bitpack-index1 oberen Bits, sonst Null),
              # carry2 = Übertrag vom 2. Array
              #          (in den bitpack-index2 oberen Bits, sonst Null),
              # carry3 = Übertrag noch abzuspeichernder Bits
              #          (in den index3 oberen Bits, sonst Null).
              var uint_bitpack temp =
                (*op)(
                      ( carry1 |=
                          LR_0_bitpack(*ptr1++) # nächstes Word des 1. Arrays lesen
                          << index1, # zum carry1 dazunehmen
                        L_bitpack(carry1) # und davon das linke Word verwenden
                      ),
                      ( carry2 |=
                          LR_0_bitpack(*ptr2++) # nächstes Word des 2. Arrays lesen
                          << index2, # zum carry2 dazunehmen
                        L_bitpack(carry2) # und davon das linke Word verwenden
                      )
                     ) ; # beide durch *op verknüpfen
              carry1 = LR_bitpack_0(R_bitpack(carry1)); # carry1 := rechtes Word von carry1
              carry2 = LR_bitpack_0(R_bitpack(carry2)); # carry2 := rechtes Word von carry2
              carry3 |= LR_bitpack_0(temp) >> index3;
              # Die oberen bitpack+index3 Bits von carry3 sind abzulegen.
              if (bitpackcount==0)
                break;
              *ptr3++ = L_bitpack(carry3); # bitpack Bits davon ablegen
              carry3 = LR_bitpack_0(R_bitpack(carry3)); # und index3 Bits für später behalten.
              bitpackcount--;
            }
          }
        }
        # letztes (halbes) Datenword speziell behandeln:
        # Vom letzten Word (nun in den Bits
        # 2*bitpack-index3-1..bitpack-index3 von carry3)
        # dürfen nur bitcount_rest Bits im 3. Array abgelegt werden.
        bitcount_rest = index3+bitcount_rest;
        var uint_bitpack last_carry;
        # Die oberen bitcount_rest Bits ablegen:
        if (bitcount_rest>=bitpack) {
          *ptr3++ = L_bitpack(carry3);
          last_carry = R_bitpack(carry3);
          bitcount_rest -= bitpack;
        } else {
          last_carry = L_bitpack(carry3);
        }
        # Die noch übrigen bitcount_rest Bits von last_carry ablegen:
        if (!(bitcount_rest==0))
          *ptr3 ^=
            (*ptr3 ^ last_carry)
            & (~( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest ));
              # Bitmaske, in der die oberen bitcount_rest Bits gesetzt sind
      }
    }

# Unterprogramm für Bit-Verknüpfung mit 2 Operanden
# bit_up(op)
# > STACK_2: bit-array1
# > STACK_1: bit-array2
# > STACK_0: result-bit-array oder #<UNBOUND>
# > op: Adresse der Verknüpfungsroutine
# < value1/mv_count: Funktionswert
# Testet Argumente, räumt STACK auf.
  local Values bit_up (bit_op_fun* op);
  local Values bit_up(op)
    var bit_op_fun* op;
    {
      # Hauptunterscheidung: Vektor / mehrdimensionaler Array
      var uintL len; # Länge (des 1. Arrays), falls Vektoren
      var uintC rank; # Rang und
      var uintL* dimptr; # Pointer auf Dimensionen, falls mehrdimensional
      # Typ von bit-array1 untersuchen und danach verzweigen:
      #ifndef TYPECODES
      if (!orecordp(STACK_2))
        goto fehler2;
      #endif
      switch (Array_type(STACK_2)) {
        case Array_type_sbvector:
          len = Sbvector_length(STACK_2); goto vector;
        case Array_type_bvector:
          len = TheIarray(STACK_2)->totalsize; goto vector;
        case Array_type_mdarray:
          {
            var Iarray array1 = TheIarray(STACK_2);
            # bit-array1 muss den Elementtyp BIT haben:
            if (!((iarray_flags(array1) & arrayflags_atype_mask) == Atype_Bit))
              goto fehler2;
            # Rang merken:
            rank = iarray_rank(array1);
            # Dimensionen merken:
            dimptr = &array1->dims[0];
            if (iarray_flags(array1) & bit(arrayflags_dispoffset_bit))
              dimptr++;
            # die Anzahl der zu verknüpfenden Bits ist die Totalsize:
            len = array1->totalsize;
            goto array;
          }
        default:
          goto fehler2;
      }
     vector: # Das erste Argument ist ein Bit-Vektor, mit Länge len.
      # Teste, ob dies auch auf den/die anderen zutrifft:
      # bit-array2 überprüfen:
      #ifndef TYPECODES
      if (!orecordp(STACK_1))
        goto fehler2;
      #endif
      switch (Array_type(STACK_1)) {
        case Array_type_sbvector:
          if (!(len == Sbvector_length(STACK_1)))
            goto fehler2;
          break;
        case Array_type_bvector:
          if (!(len == TheIarray(STACK_1)->totalsize))
            goto fehler2;
          break;
        default:
          goto fehler2;
      }
      # bit-array3 überprüfen:
      {
        var object array3 = STACK_0;
        if (eq(array3,unbound) || eq(array3,NIL)) { # nicht angegeben oder NIL?
          # ja -> neuen Vektor erzeugen:
          STACK_0 = allocate_bit_vector(Atype_Bit,len);
        } elif (eq(array3,T)) {
          STACK_0 = STACK_2; # statt T verwende bit-array1
        } else {
          #ifndef TYPECODES
          if (!orecordp(STACK_0))
            goto fehler3;
          #endif
          switch (Array_type(STACK_0)) {
            case Array_type_sbvector:
              if (!(len == Sbvector_length(array3)))
                goto fehler3;
              break;
            case Array_type_bvector:
              if (!(len == TheIarray(array3)->totalsize))
                goto fehler3;
              break;
            default:
              goto fehler3;
          }
        }
      }
      goto weiter;
     array: # erstes Argument war ein mehrdimensionaler Bit-Array
      # mit Rang rank, Dimensionen ab dimptr und Totalsize len.
      # bit-array2 überprüfen:
      #ifndef TYPECODES
      if (!orecordp(STACK_1))
        goto fehler2;
      #endif
      switch (Array_type(STACK_1)) {
        case Array_type_mdarray:
          {
            var Iarray array2 = TheIarray(STACK_1);
            # bit-array2 muss den Elementtyp BIT haben:
            if (!((iarray_flags(array2) & arrayflags_atype_mask) == Atype_Bit))
              goto fehler2;
            # Rang vergleichen:
            if (!(rank == iarray_rank(array2)))
              goto fehler2;
            # Dimensionen vergleichen:
            if (rank > 0) {
              var uintC count;
              var uintL* dimptr1 = dimptr;
              var uintL* dimptr2;
              dimptr2 = &array2->dims[0];
              if (iarray_flags(array2) & bit(arrayflags_dispoffset_bit))
                dimptr2++;
              dotimespC(count,rank, {
                if (!(*dimptr1++==*dimptr2++))
                  goto fehler2;
              });
            }
          }
          break;
        default:
          goto fehler2;
      }
      # bit-array3 überprüfen:
      {
        var object array3 = STACK_0;
        if (eq(array3,unbound) || eq(array3,NIL)) { # nicht angegeben oder NIL?
          # ja -> neuen Array erzeugen:
          STACK_0 = allocate_bit_vector(Atype_Bit,len); # Bitvektor erzeugen
          array3 = allocate_iarray(Atype_Bit,rank,Array_type_mdarray); # Array erzeugen
          TheIarray(array3)->data = STACK_0; # Datenvektor eintragen
          # Dimensionen eintragen:
          if (rank > 0) {
            var uintC count;
            var uintL* dimptr1 = dimptr;
            var uintL* dimptr2 = &TheIarray(array3)->dims[0];
            dotimespC(count,rank, {
              *dimptr1++ = *dimptr2++;
            });
          }
          STACK_0 = array3; # neuen Array ablegen
        } elif (eq(array3,T)) {
          STACK_0 = STACK_2; # statt T verwende bit-array1
        } else {
          #ifndef TYPECODES
          if (!orecordp(STACK_0))
            goto fehler3;
          #endif
          switch (Array_type(STACK_0)) {
            case Array_type_mdarray:
              {
                var Iarray array3 = TheIarray(STACK_0);
                # bit-array3 muss den Elementtyp BIT haben:
                if (!((iarray_flags(array3) & arrayflags_atype_mask) == Atype_Bit))
                  goto fehler3;
                # Rang vergleichen:
                if (!(rank == iarray_rank(array3)))
                  goto fehler3;
                # Dimensionen vergleichen:
                if (rank > 0) {
                  var uintC count;
                  var uintL* dimptr1 = dimptr;
                  var uintL* dimptr2;
                  dimptr2 = &array3->dims[0];
                  if (iarray_flags(array3) & bit(arrayflags_dispoffset_bit))
                    dimptr2++;
                  dotimespC(count,rank, {
                    if (!(*dimptr1++==*dimptr2++))
                      goto fehler3;
                  });
                }
              }
              break;
            default:
              goto fehler3;
          }
        }
      }
     weiter: # Vorbereitungen sind abgeschlossen:
      # STACK_2 = bit-array1, STACK_1 = bit-array2, STACK_0 = bit-array3,
      # alle von denselben Dimensionen, mit je len Bits.
      if (len > 0) {
        var uintL index1 = 0; # Index in Datenvektor von bit-array1
        var object array1 = # Datenvektor von bit-array1
                            (simple_bit_vector_p(Atype_Bit,STACK_2)
                              ? STACK_2
                              : iarray_displace_check(STACK_2,len,&index1)
                            );
        var uintL index2 = 0; # Index in Datenvektor von bit-array2
        var object array2 = # Datenvektor von bit-array2
                            (simple_bit_vector_p(Atype_Bit,STACK_1)
                              ? STACK_1
                              : iarray_displace_check(STACK_1,len,&index2)
                            );
        var uintL index3 = 0; # Index in Datenvektor von bit-array3
        var object array3 = # Datenvektor von bit-array3
                            (simple_bit_vector_p(Atype_Bit,STACK_0)
                              ? STACK_0
                              : iarray_displace_check(STACK_0,len,&index3)
                            );
        # Los geht's:
        bit_op(array1,index1,array2,index2,array3,index3,op,len);
      }
      # Fertig:
      value1 = popSTACK(); mv_count=1; # bit-array3 ist das Ergebnis
      skipSTACK(2);
      return;
     fehler2: # Fehlermeldung bei (mindestens) 2 Argumenten
      {
        var object array1 = STACK_2;
        var object array2 = STACK_1;
        pushSTACK(array2); pushSTACK(array1);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: The arguments ~ and ~ should be arrays of bits with the same dimensions")
              );
      }
     fehler3: # Fehlermeldung bei 3 Argumenten
      {
        var object array1 = STACK_2;
        var object array2 = STACK_1;
        # array3 bereits in STACK_0
        pushSTACK(array2); pushSTACK(array1);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: The arguments ~, ~ and ~ should be arrays of bits with the same dimensions")
              );
      }
    }

# Die einzelnen Operatoren für BIT-AND usw.:
  local uint_bitpack bitpack_and (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_and(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return x&y;
    }
  local uint_bitpack bitpack_ior (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_ior(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return x|y;
    }
  local uint_bitpack bitpack_xor (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_xor(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return x^y;
    }
  local uint_bitpack bitpack_eqv (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_eqv(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return ~(x^y);
    }
  local uint_bitpack bitpack_nand (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_nand(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return ~(x&y);
    }
  local uint_bitpack bitpack_nor (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_nor(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return ~(x|y);
    }
  local uint_bitpack bitpack_andc1 (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_andc1(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return (~x)&y;
    }
  local uint_bitpack bitpack_andc2 (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_andc2(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return x&(~y);
    }
  local uint_bitpack bitpack_orc1 (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_orc1(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return (~x)|y;
    }
  local uint_bitpack bitpack_orc2 (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_orc2(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return x|(~y);
    }
  local uint_bitpack bitpack_not (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_not(x,y) var uint_bitpack x; var uint_bitpack y;
    {
      return ~x;
    }

LISPFUN(bit_and,2,1,norest,nokey,0,NIL)
# (BIT-AND bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  {
    return_Values bit_up(&bitpack_and);
  }

LISPFUN(bit_ior,2,1,norest,nokey,0,NIL)
# (BIT-IOR bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  {
    return_Values bit_up(&bitpack_ior);
  }

LISPFUN(bit_xor,2,1,norest,nokey,0,NIL)
# (BIT-XOR bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  {
    return_Values bit_up(&bitpack_xor);
  }

LISPFUN(bit_eqv,2,1,norest,nokey,0,NIL)
# (BIT-EQV bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  {
    return_Values bit_up(&bitpack_eqv);
  }

LISPFUN(bit_nand,2,1,norest,nokey,0,NIL)
# (BIT-NAND bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  {
    return_Values bit_up(&bitpack_nand);
  }

LISPFUN(bit_nor,2,1,norest,nokey,0,NIL)
# (BIT-NOR bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  {
    return_Values bit_up(&bitpack_nor);
  }

LISPFUN(bit_andc1,2,1,norest,nokey,0,NIL)
# (BIT-ANDC1 bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  {
    return_Values bit_up(&bitpack_andc1);
  }

LISPFUN(bit_andc2,2,1,norest,nokey,0,NIL)
# (BIT-ANDC2 bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  {
    return_Values bit_up(&bitpack_andc2);
  }

LISPFUN(bit_orc1,2,1,norest,nokey,0,NIL)
# (BIT-ORC1 bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  {
    return_Values bit_up(&bitpack_orc1);
  }

LISPFUN(bit_orc2,2,1,norest,nokey,0,NIL)
# (BIT-ORC2 bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  {
    return_Values bit_up(&bitpack_orc2);
  }

LISPFUN(bit_not,1,1,norest,nokey,0,NIL)
# (BIT-NOT bit-array [result-bit-array]), CLTL S. 295
  {
    # erstes Argument verdoppeln (wird bei der Operation ignoriert):
    var object array3 = STACK_0;
    pushSTACK(array3);
    STACK_1 = STACK_2;
    return_Values bit_up(&bitpack_not);
  }

# ============================================================================
#                     Polymorphic copying

# Function: Copies a slice of an array array1 into another array array2.
# elt_copy(dv1,index1,dv2,index2,count);
# > dv1: source storage-vector
# > index1: start index in dv1
# > dv2: destination storage-vector
# > index2: start index in dv2
# > count: number of elements to be copied, > 0
# can trigger GC
  global void elt_copy (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_T_T (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_Char_T (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_Bit_T (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_2Bit_T (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_4Bit_T (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_8Bit_T (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_16Bit_T (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_32Bit_T (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_T_Char (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_Char_Char (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_T_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_Bit_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_2Bit_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_4Bit_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_8Bit_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_16Bit_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_32Bit_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_T_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_Bit_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_2Bit_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_4Bit_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_8Bit_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_16Bit_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_32Bit_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_T_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_Bit_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_2Bit_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_4Bit_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_8Bit_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_16Bit_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_32Bit_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_T_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_Bit_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_2Bit_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_4Bit_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_8Bit_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_16Bit_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_32Bit_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_T_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_2Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_4Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_8Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_16Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_32Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_T_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_2Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_4Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_8Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_16Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_32Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_copy_T_T(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var object* ptr2 = &TheSvector(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    }
  local void elt_copy_Char_T(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var object* ptr2 = &TheSvector(dv2)->data[index2];
      SstringDispatch(dv1,
        {
          var const chart* ptr1 = &TheSstring(dv1)->data[index1];
          dotimespL(count,count, {
            *ptr2++ = code_char(*ptr1); ptr1++;
          });
        },
        {
          var const scint* ptr1 = &TheSmallSstring(dv1)->data[index1];
          dotimespL(count,count, {
            *ptr2++ = code_char(as_chart(*ptr1)); ptr1++;
          });
        }
        );
    }
  local void elt_copy_Bit_T(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var object* ptr2 = &TheSvector(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = fixnum((*ptr1 >> ((~index1)%8)) & (bit(1)-1));
        index1++;
        ptr1 += ((index1%8)==0);
      });
    }
  local void elt_copy_2Bit_T(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
      var object* ptr2 = &TheSvector(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = fixnum((*ptr1 >> (2*((~index1)%4))) & (bit(2)-1));
        index1++;
        ptr1 += ((index1%4)==0);
      });
    }
  local void elt_copy_4Bit_T(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
      var object* ptr2 = &TheSvector(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = fixnum((*ptr1 >> (4*((~index1)%2))) & (bit(4)-1));
        index1++;
        ptr1 += ((index1%2)==0);
      });
    }
  local void elt_copy_8Bit_T(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
      var object* ptr2 = &TheSvector(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = fixnum(*ptr1++);
      });
    }
  local void elt_copy_16Bit_T(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
      var object* ptr2 = &TheSvector(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = fixnum(*ptr1++);
      });
    }
  local void elt_copy_32Bit_T(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      #if (intLsize<=oint_data_len)
        # UL_to_I(x) = fixnum(x), cannot trigger GC
        var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
        var object* ptr2 = &TheSvector(dv2)->data[index2];
        dotimespL(count,count, {
          *ptr2++ = fixnum(*ptr1++);
        });
      #else
        pushSTACK(dv1);
        pushSTACK(dv2);
        dotimespL(count,count, {
          var object x = UL_to_I(((uint32*)&TheSbvector(STACK_1)->data[0])[index1++]);
          TheSvector(STACK_0)->data[index2++] = x;
        });
        skipSTACK(2);
      #endif
    }
  local void elt_copy_T_Char(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const object* ptr1 = &TheSvector(dv1)->data[index1];
      check_sstring_mutable(dv2);
      var chart* ptr2 = &TheSstring(dv2)->data[index2];
      dotimespL(count,count, {
        var object value = *ptr1++;
        if (!charp(value)) fehler_store(dv2,value);
        *ptr2++ = char_code(value);
      });
    }
  local void elt_copy_Char_Char(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      check_sstring_mutable(dv2);
      var chart* ptr2 = &TheSstring(dv2)->data[index2];
      SstringDispatch(dv1,
        {
          # Equivalent to chartcopy, but we inline it here.
          var const chart* ptr1 = &TheSstring(dv1)->data[index1];
          dotimespL(count,count, {
            *ptr2++ = *ptr1++;
          });
        },
        {
          # Equivalent to scintcopy, but we inline it here.
          var const scint* ptr1 = &TheSmallSstring(dv1)->data[index1];
          dotimespL(count,count, {
            *ptr2++ = as_chart(*ptr1++);
          });
        }
        );
    }
  local void elt_copy_T_Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
      dotimespL(count,count, {
        var object value = *ptr1++;
        if (!uint1_p(value)) fehler_store(dv2,value);
        *ptr2 ^= (*ptr2 ^ (I_to_uint8(value) << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
        index2++;
        ptr2 += ((index2%8)==0);
      });
    }
  #define elt_copy_Bit_Bit(dv1,index1,dv2,index2,count)  \
    bit_copy(dv1,index1,dv2,index2,count)
  local void elt_copy_2Bit_Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
        if (!(value < bit(1))) fehler_store(dv2,fixnum(value));
        *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
        index1++;
        ptr1 += ((index1%4)==0);
        index2++;
        ptr2 += ((index2%8)==0);
      });
    }
  local void elt_copy_4Bit_Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
        if (!(value < bit(1))) fehler_store(dv2,fixnum(value));
        *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
        index1++;
        ptr1 += ((index1%2)==0);
        index2++;
        ptr2 += ((index2%8)==0);
      });
    }
  local void elt_copy_8Bit_Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
      dotimespL(count,count, {
        var uint8 value = *ptr1++;
        if (!(value < bit(1))) fehler_store(dv2,fixnum(value));
        *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
        index2++;
        ptr2 += ((index2%8)==0);
      });
    }
  local void elt_copy_16Bit_Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
      dotimespL(count,count, {
        var uint16 value = *ptr1++;
        if (!(value < bit(1))) fehler_store(dv2,fixnum(value));
        *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
        index2++;
        ptr2 += ((index2%8)==0);
      });
    }
  local void elt_copy_32Bit_Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
      dotimespL(count,count, {
        var uint32 value = *ptr1++;
        if (!(value < bit(1))) {
          pushSTACK(dv2);
          var object tmp = UL_to_I(value);
          fehler_store(popSTACK(),tmp);
        }
        *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
        index2++;
        ptr2 += ((index2%8)==0);
      });
    }
  local void elt_copy_T_2Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
      dotimespL(count,count, {
        var object value = *ptr1++;
        if (!uint2_p(value)) fehler_store(dv2,value);
        *ptr2 ^= (*ptr2 ^ (I_to_uint8(value) << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
        index2++;
        ptr2 += ((index2%4)==0);
      });
    }
  local void elt_copy_Bit_2Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
        *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
        index1++;
        ptr1 += ((index1%8)==0);
        index2++;
        ptr2 += ((index2%4)==0);
      });
    }
  #define elt_copy_2Bit_2Bit(dv1,index1,dv2,index2,count)  \
    bit_copy(dv1,(index1)<<1,dv2,(index2)<<1,(count)<<1)
  local void elt_copy_4Bit_2Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
        if (!(value < bit(2))) fehler_store(dv2,fixnum(value));
        *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
        index1++;
        ptr1 += ((index1%2)==0);
        index2++;
        ptr2 += ((index2%4)==0);
      });
    }
  local void elt_copy_8Bit_2Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
      dotimespL(count,count, {
        var uint8 value = *ptr1++;
        if (!(value < bit(2))) fehler_store(dv2,fixnum(value));
        *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
        index2++;
        ptr2 += ((index2%4)==0);
      });
    }
  local void elt_copy_16Bit_2Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
      dotimespL(count,count, {
        var uint16 value = *ptr1++;
        if (!(value < bit(2))) fehler_store(dv2,fixnum(value));
        *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
        index2++;
        ptr2 += ((index2%4)==0);
      });
    }
  local void elt_copy_32Bit_2Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
      dotimespL(count,count, {
        var uint32 value = *ptr1++;
        if (!(value < bit(2))) {
          pushSTACK(dv2);
          var object tmp = UL_to_I(value);
          fehler_store(popSTACK(),tmp);
        }
        *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
        index2++;
        ptr2 += ((index2%4)==0);
      });
    }
  local void elt_copy_T_4Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
      dotimespL(count,count, {
        var object value = *ptr1++;
        if (!uint4_p(value)) fehler_store(dv2,value);
        *ptr2 ^= (*ptr2 ^ (I_to_uint8(value) << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
        index2++;
        ptr2 += ((index2%2)==0);
      });
    }
  local void elt_copy_Bit_4Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
        *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
        index1++;
        ptr1 += ((index1%8)==0);
        index2++;
        ptr2 += ((index2%2)==0);
      });
    }
  local void elt_copy_2Bit_4Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
        *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
        index1++;
        ptr1 += ((index1%4)==0);
        index2++;
        ptr2 += ((index2%2)==0);
      });
    }
  #define elt_copy_4Bit_4Bit(dv1,index1,dv2,index2,count)  \
    bit_copy(dv1,(index1)<<2,dv2,(index2)<<2,(count)<<2)
  local void elt_copy_8Bit_4Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
      dotimespL(count,count, {
        var uint8 value = *ptr1++;
        if (!(value < bit(4))) fehler_store(dv2,fixnum(value));
        *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
        index2++;
        ptr2 += ((index2%2)==0);
      });
    }
  local void elt_copy_16Bit_4Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
      dotimespL(count,count, {
        var uint16 value = *ptr1++;
        if (!(value < bit(4))) fehler_store(dv2,fixnum(value));
        *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
        index2++;
        ptr2 += ((index2%2)==0);
      });
    }
  local void elt_copy_32Bit_4Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
      dotimespL(count,count, {
        var uint32 value = *ptr1++;
        if (!(value < bit(4))) {
          pushSTACK(dv2);
          var object tmp = UL_to_I(value);
          fehler_store(popSTACK(),tmp);
        }
        *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
        index2++;
        ptr2 += ((index2%2)==0);
      });
    }
  local void elt_copy_T_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
      dotimespL(count,count, {
        var object value = *ptr1++;
        if (!uint8_p(value)) fehler_store(dv2,value);
        *ptr2++ = I_to_uint8(value);
      });
    }
  local void elt_copy_Bit_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
        index1++;
        ptr1 += ((index1%8)==0);
      });
    }
  local void elt_copy_2Bit_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
        index1++;
        ptr1 += ((index1%4)==0);
      });
    }
  local void elt_copy_4Bit_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
        index1++;
        ptr1 += ((index1%2)==0);
      });
    }
  local void elt_copy_8Bit_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    }
  local void elt_copy_16Bit_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
      dotimespL(count,count, {
        var uint16 value = *ptr1++;
        if (!(value < bit(8))) fehler_store(dv2,fixnum(value));
        *ptr2++ = value;
      });
    }
  local void elt_copy_32Bit_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
      dotimespL(count,count, {
        var uint32 value = *ptr1++;
        if (!(value < bit(8))) {
          pushSTACK(dv2);
          var object tmp = UL_to_I(value);
          fehler_store(popSTACK(),tmp);
        }
        *ptr2++ = value;
      });
    }
  local void elt_copy_T_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        var object value = *ptr1++;
        if (!uint16_p(value)) fehler_store(dv2,value);
        *ptr2++ = I_to_uint16(value);
      });
    }
  local void elt_copy_Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
        index1++;
        ptr1 += ((index1%8)==0);
      });
    }
  local void elt_copy_2Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
      var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
        index1++;
        ptr1 += ((index1%4)==0);
      });
    }
  local void elt_copy_4Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
      var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
        index1++;
        ptr1 += ((index1%2)==0);
      });
    }
  local void elt_copy_8Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
      var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    }
  local void elt_copy_16Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
      var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    }
  local void elt_copy_32Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
      var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        var uint32 value = *ptr1++;
        if (!(value < bit(16))) {
          pushSTACK(dv2);
          var object tmp = UL_to_I(value);
          fehler_store(popSTACK(),tmp);
        }
        *ptr2++ = value;
      });
    }
  local void elt_copy_T_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        var object value = *ptr1++;
        if (!uint32_p(value)) fehler_store(dv2,value);
        *ptr2++ = I_to_uint32(value);
      });
    }
  local void elt_copy_Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
        index1++;
        ptr1 += ((index1%8)==0);
      });
    }
  local void elt_copy_2Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
      var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
        index1++;
        ptr1 += ((index1%4)==0);
      });
    }
  local void elt_copy_4Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
      var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
        index1++;
        ptr1 += ((index1%2)==0);
      });
    }
  local void elt_copy_8Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
      var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    }
  local void elt_copy_16Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
      var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    }
  local void elt_copy_32Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
      var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    }
  global void elt_copy(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      switch (Array_type(dv1)) {
        case Array_type_svector: # Simple-Vector
          switch (Array_type(dv2)) {
            case Array_type_svector: # Simple-Vector
              elt_copy_T_T(dv1,index1,dv2,index2,count); return;
            case Array_type_sbvector: # Simple-Bit-Vector
              elt_copy_T_Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb2vector:
              elt_copy_T_2Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb4vector:
              elt_copy_T_4Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb8vector:
              elt_copy_T_8Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb16vector:
              elt_copy_T_16Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb32vector:
              elt_copy_T_32Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sstring: # Simple-String
              elt_copy_T_Char(dv1,index1,dv2,index2,count); return;
            default: NOTREACHED
          }
          break;
        case Array_type_sbvector: # Simple-Bit-Vector
          switch (Array_type(dv2)) {
            case Array_type_svector: # Simple-Vector
              elt_copy_Bit_T(dv1,index1,dv2,index2,count); return;
            case Array_type_sbvector: # Simple-Bit-Vector
              elt_copy_Bit_Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb2vector:
              elt_copy_Bit_2Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb4vector:
              elt_copy_Bit_4Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb8vector:
              elt_copy_Bit_8Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb16vector:
              elt_copy_Bit_16Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb32vector:
              elt_copy_Bit_32Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sstring: # Simple-String
              break; # fehler_store because count > 0
            default: NOTREACHED
          }
          break;
        case Array_type_sb2vector:
          switch (Array_type(dv2)) {
            case Array_type_svector: # Simple-Vector
              elt_copy_2Bit_T(dv1,index1,dv2,index2,count); return;
            case Array_type_sbvector: # Simple-Bit-Vector
              elt_copy_2Bit_Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb2vector:
              elt_copy_2Bit_2Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb4vector:
              elt_copy_2Bit_4Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb8vector:
              elt_copy_2Bit_8Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb16vector:
              elt_copy_2Bit_16Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb32vector:
              elt_copy_2Bit_32Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sstring: # Simple-String
              break; # fehler_store because count > 0
            default: NOTREACHED
          }
          break;
        case Array_type_sb4vector:
          switch (Array_type(dv2)) {
            case Array_type_svector: # Simple-Vector
              elt_copy_4Bit_T(dv1,index1,dv2,index2,count); return;
            case Array_type_sbvector: # Simple-Bit-Vector
              elt_copy_4Bit_Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb2vector:
              elt_copy_4Bit_2Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb4vector:
              elt_copy_4Bit_4Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb8vector:
              elt_copy_4Bit_8Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb16vector:
              elt_copy_4Bit_16Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb32vector:
              elt_copy_4Bit_32Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sstring: # Simple-String
              break; # fehler_store because count > 0
            default: NOTREACHED
          }
          break;
        case Array_type_sb8vector:
          switch (Array_type(dv2)) {
            case Array_type_svector: # Simple-Vector
              elt_copy_8Bit_T(dv1,index1,dv2,index2,count); return;
            case Array_type_sbvector: # Simple-Bit-Vector
              elt_copy_8Bit_Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb2vector:
              elt_copy_8Bit_2Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb4vector:
              elt_copy_8Bit_4Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb8vector:
              elt_copy_8Bit_8Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb16vector:
              elt_copy_8Bit_16Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb32vector:
              elt_copy_8Bit_32Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sstring: # Simple-String
              break; # fehler_store because count > 0
            default: NOTREACHED
          }
          break;
        case Array_type_sb16vector:
          switch (Array_type(dv2)) {
            case Array_type_svector: # Simple-Vector
              elt_copy_16Bit_T(dv1,index1,dv2,index2,count); return;
            case Array_type_sbvector: # Simple-Bit-Vector
              elt_copy_16Bit_Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb2vector:
              elt_copy_16Bit_2Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb4vector:
              elt_copy_16Bit_4Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb8vector:
              elt_copy_16Bit_8Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb16vector:
              elt_copy_16Bit_16Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb32vector:
              elt_copy_16Bit_32Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sstring: # Simple-String
              break; # fehler_store because count > 0
            default: NOTREACHED
          }
          break;
        case Array_type_sb32vector:
          switch (Array_type(dv2)) {
            case Array_type_svector: # Simple-Vector
              elt_copy_32Bit_T(dv1,index1,dv2,index2,count); return;
            case Array_type_sbvector: # Simple-Bit-Vector
              elt_copy_32Bit_Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb2vector:
              elt_copy_32Bit_2Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb4vector:
              elt_copy_32Bit_4Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb8vector:
              elt_copy_32Bit_8Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb16vector:
              elt_copy_32Bit_16Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sb32vector:
              elt_copy_32Bit_32Bit(dv1,index1,dv2,index2,count); return;
            case Array_type_sstring: # Simple-String
              break; # fehler_store because count > 0
            default: NOTREACHED
          }
          break;
        case Array_type_sstring: # Simple-String
          switch (Array_type(dv2)) {
            case Array_type_svector: # Simple-Vector
              elt_copy_Char_T(dv1,index1,dv2,index2,count); return;
            case Array_type_sbvector: # Simple-Bit-Vector
            case Array_type_sb2vector:
            case Array_type_sb4vector:
            case Array_type_sb8vector:
            case Array_type_sb16vector:
            case Array_type_sb32vector:
              break; # fehler_store because count > 0
            case Array_type_sstring: # Simple-String
              elt_copy_Char_Char(dv1,index1,dv2,index2,count); return;
            default: NOTREACHED
          }
          break;
        default: NOTREACHED
      }
      pushSTACK(dv2);
      var object elt1 = storagevector_aref(dv1,index1);
      fehler_store(popSTACK(),elt1);
    }

# Function: Copies a slice of an array array1 into another array array2 of
# the same element type. Handles overlapping arrays correctly.
# elt_move(dv1,index1,dv2,index2,count);
# > dv1: source storage-vector
# > index1: start index in dv1
# > dv2: destination storage-vector
# > index2: start index in dv2
# > count: number of elements to be copied, > 0
# can trigger GC
  global void elt_move (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_move_T (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_move_Char (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_move_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_move_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_move_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_move_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_move_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_move_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local void elt_move_T(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      if (dv1 == dv2 && index1 < index2 && index2 < index1+count) {
        var const object* ptr1 = &TheSvector(dv1)->data[index1+count];
        var object* ptr2 = &TheSvector(dv2)->data[index2+count];
        dotimespL(count,count, {
          *--ptr2 = *--ptr1;
        });
      } else {
        var const object* ptr1 = &TheSvector(dv1)->data[index1];
        var object* ptr2 = &TheSvector(dv2)->data[index2];
        dotimespL(count,count, {
          *ptr2++ = *ptr1++;
        });
      }
    }
  local void elt_move_Char(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      check_sstring_mutable(dv2);
      if (dv1 == dv2 && index1 < index2 && index2 < index1+count) {
        var const chart* ptr1 = &TheSstring(dv1)->data[index1+count];
        var chart* ptr2 = &TheSstring(dv2)->data[index2+count];
        dotimespL(count,count, {
          *--ptr2 = *--ptr1;
        });
      } else {
        # elt_copy_Char_Char(dv1,index1,dv2,index2,count) inlined:
        var chart* ptr2 = &TheSstring(dv2)->data[index2];
        SstringDispatch(dv1,
          {
            # Equivalent to chartcopy, but we inline it here.
            var const chart* ptr1 = &TheSstring(dv1)->data[index1];
            dotimespL(count,count, {
              *ptr2++ = *ptr1++;
            });
          },
          {
            # Equivalent to scintcopy, but we inline it here.
            var const scint* ptr1 = &TheSmallSstring(dv1)->data[index1];
            dotimespL(count,count, {
              *ptr2++ = as_chart(*ptr1++);
            });
          }
          );
      }
    }
  local void elt_move_Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      if (dv1 == dv2 && index1 < index2+64 && index2 < index1+count+64) {
        if (index1 < index2 && index2 < index1+count) {
          index1 += count;
          index2 += count;
          var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
          var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
          dotimespL(count,count, {
            ptr1 -= ((index1%8)==0);
            index1--;
            ptr2 -= ((index2%8)==0);
            index2--;
            var uint8 value = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
            *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
          });
        } else {
          var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
          var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
          dotimespL(count,count, {
            var uint8 value = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
            *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
            index1++;
            ptr1 += ((index1%8)==0);
            index2++;
            ptr2 += ((index2%8)==0);
          });
        }
      } else
        elt_copy_Bit_Bit(dv1,index1,dv2,index2,count);
    }
  local void elt_move_2Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      if (dv1 == dv2 && index1 < index2+32 && index2 < index1+count+32) {
        if (index1 < index2 && index2 < index1+count) {
          index1 += count;
          index2 += count;
          var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
          var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
          dotimespL(count,count, {
            ptr1 -= ((index1%4)==0);
            index1--;
            ptr2 -= ((index2%4)==0);
            index2--;
            var uint8 value = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
            *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
          });
        } else {
          var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
          var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
          dotimespL(count,count, {
            var uint8 value = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
            *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
            index1++;
            ptr1 += ((index1%4)==0);
            index2++;
            ptr2 += ((index2%4)==0);
          });
        }
      } else
        elt_copy_2Bit_2Bit(dv1,index1,dv2,index2,count);
    }
  local void elt_move_4Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      if (dv1 == dv2 && index1 < index2+16 && index2 < index1+count+16) {
        if (index1 < index2 && index2 < index1+count) {
          index1 += count;
          index2 += count;
          var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
          var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
          dotimespL(count,count, {
            ptr1 -= ((index1%2)==0);
            index1--;
            ptr2 -= ((index2%2)==0);
            index2--;
            var uint8 value = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
            *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
          });
        } else {
          var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
          var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
          dotimespL(count,count, {
            var uint8 value = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
            *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
            index1++;
            ptr1 += ((index1%2)==0);
            index2++;
            ptr2 += ((index2%2)==0);
          });
        }
      } else
        elt_copy_4Bit_4Bit(dv1,index1,dv2,index2,count);
    }
  local void elt_move_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      if (dv1 == dv2 && index1 < index2 && index2 < index1+count) {
        var const uint8* ptr1 = &TheSbvector(dv1)->data[index1+count];
        var uint8* ptr2 = &TheSbvector(dv2)->data[index2+count];
        dotimespL(count,count, {
          *--ptr2 = *--ptr1;
        });
      } else {
        var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
        var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
        dotimespL(count,count, {
          *ptr2++ = *ptr1++;
        });
      }
    }
  local void elt_move_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      if (dv1 == dv2 && index1 < index2 && index2 < index1+count) {
        var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1+count];
        var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2+count];
        dotimespL(count,count, {
          *--ptr2 = *--ptr1;
        });
      } else {
        var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
        var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
        dotimespL(count,count, {
          *ptr2++ = *ptr1++;
        });
      }
    }
  local void elt_move_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      if (dv1 == dv2 && index1 < index2 && index2 < index1+count) {
        var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1+count];
        var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2+count];
        dotimespL(count,count, {
          *--ptr2 = *--ptr1;
        });
      } else {
        var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
        var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
        dotimespL(count,count, {
          *ptr2++ = *ptr1++;
        });
      }
    }
  global void elt_move(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      switch (Array_type(dv1)) {
        case Array_type_svector: # Simple-Vector
          elt_move_T(dv1,index1,dv2,index2,count);
        case Array_type_sbvector: # Simple-Bit-Vector
          elt_move_Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb2vector:
          elt_move_2Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb4vector:
          elt_move_4Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb8vector:
          elt_move_8Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb16vector:
          elt_move_16Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb32vector:
          elt_move_32Bit(dv1,index1,dv2,index2,count);
        case Array_type_sstring: # Simple-String
          elt_move_Char(dv1,index1,dv2,index2,count);
        default: NOTREACHED
      }
    }

# Function: Fills a slice of an array with an element.
# elt_fill(dv,index,count,element)
# > dv: destination storage-vector
# > index: start index in dv
# > count: number of elements to be filled
# < result: TRUE if element does not fit, FALSE when done
  global boolean elt_fill (object dv, uintL index, uintL count, object element);
  global boolean elt_fill(dv,index,count,element)
    var object dv;
    var uintL index;
    var uintL count;
    var object element;
    {
      switch (Array_type(dv)) {
        case Array_type_svector: # Simple-Vector
          if (count > 0) {
            var object* ptr = &TheSvector(dv)->data[index];
            dotimespL(count,count, {
              *ptr++ = element;
            });
          }
          break;
       #if 0 # unoptimized
        case Array_type_sbvector: # Simple-Bit-Vector
          if (!uint1_p(element)) return TRUE;
          if (count > 0) {
            var uint8 x = I_to_uint8(element);
            var uint8* ptr = &TheSbvector(dv)->data[index/8];
            dotimespL(count,count, {
              *ptr ^= (*ptr ^ (x << ((~index)%8))) & ((bit(1)-1) << ((~index)%8));
              index++;
              ptr += ((index%8)==0);
            });
          }
          break;
        case Array_type_sb2vector:
          if (!uint2_p(element)) return TRUE;
          if (count > 0) {
            var uint8 x = I_to_uint8(element);
            var uint8* ptr = &TheSbvector(dv)->data[index/4];
            dotimespL(count,count, {
              *ptr ^= (*ptr ^ (x << (2*((~index)%4)))) & ((bit(2)-1) << (2*((~index)%4)));
              index++;
              ptr += ((index%4)==0);
            });
          }
          break;
        case Array_type_sb4vector:
          if (!uint4_p(element)) return TRUE;
          if (count > 0) {
            var uint8 x = I_to_uint8(element);
            var uint8* ptr = &TheSbvector(dv)->data[index/2];
            dotimespL(count,count, {
              *ptr ^= (*ptr ^ (x << (4*((~index)%2)))) & ((bit(4)-1) << (4*((~index)%2)));
              index++;
              ptr += ((index%2)==0);
            });
          }
          break;
        case Array_type_sb8vector:
          if (!uint8_p(element)) return TRUE;
          if (count > 0) {
            var uint8 x = I_to_uint8(element);
            var uint8* ptr = &TheSbvector(dv)->data[index];
            dotimespL(count,count, {
              *ptr++ = x;
            });
          }
          break;
        case Array_type_sb16vector:
          if (!uint16_p(element)) return TRUE;
          if (count > 0) {
            var uint16 x = I_to_uint16(element);
            var uint16* ptr = &((uint16*)&TheSbvector(dv)->data[0])[index];
            dotimespL(count,count, {
              *ptr++ = x;
            });
          }
          break;
        case Array_type_sb32vector:
          if (!uint32_p(element)) return TRUE;
          if (count > 0) {
            var uint32 x = I_to_uint32(element);
            var uint32* ptr = &((uint32*)&TheSbvector(dv)->data[0])[index];
            dotimespL(count,count, {
              *ptr++ = x;
            });
          }
          break;
       #else # optimized: use 32-bit accesses where possible
        var uint32 x;
        case Array_type_sbvector: # Simple-Bit-Vector
          if (!uint1_p(element)) return TRUE;
          if (count == 0) break;
          x = I_to_uint8(element);
          x |= x << 1;
          x |= x << 2;
          x |= x << 4;
          if (index & 7) {
            var uint8* ptr = &TheSbvector(dv)->data[index/8];
            var uintL i = 8-(index&7);
            if (i >= count) {
              *ptr ^= (*ptr ^ x) & (bit(i)-bit(i-count));
              break;
            }
            *ptr ^= (*ptr ^ x) & (bit(i)-1);
            count -= i;
            index += i;
          }
          index = index/8;
          if (count & 7) {
            var uint8* ptr = &TheSbvector(dv)->data[index+count/8];
            *ptr ^= (*ptr ^ x) & (bit(8)-bit(8-(count&7)));
            count = count & ~7;
            if (count == 0) break;
          }
          count = count/8;
          goto store8;
        case Array_type_sb2vector:
          if (!uint2_p(element)) return TRUE;
          if (count == 0) break;
          x = I_to_uint8(element);
          x |= x << 2;
          x |= x << 4;
          if (index & 3) {
            var uint8* ptr = &TheSbvector(dv)->data[index/4];
            var uintL i = 4-(index&3);
            if (i >= count) {
              *ptr ^= (*ptr ^ x) & (bit(2*i)-bit(2*(i-count)));
              break;
            }
            *ptr ^= (*ptr ^ x) & (bit(2*i)-1);
            count -= i;
            index += i;
          }
          index = index/4;
          if (count & 3) {
            var uint8* ptr = &TheSbvector(dv)->data[index+count/4];
            *ptr ^= (*ptr ^ x) & (bit(8)-bit(8-2*(count&3)));
            count = count & ~3;
            if (count == 0) break;
          }
          count = count/4;
          goto store8;
        case Array_type_sb4vector:
          if (!uint4_p(element)) return TRUE;
          if (count == 0) break;
          x = I_to_uint8(element);
          x |= x << 4;
          if (index & 1) {
            var uint8* ptr = &TheSbvector(dv)->data[index/2];
            *ptr ^= (*ptr ^ x) & (bit(4)-1);
            index++;
            if (--count == 0) break;
          }
          index = index/2;
          if (count & 1) {
            var uint8* ptr = &TheSbvector(dv)->data[index+count/2];
            *ptr ^= (*ptr ^ x) & (bit(8)-bit(4));
            if (--count == 0) break;
          }
          count = count/2;
          goto store8;
        case Array_type_sb8vector:
          if (!uint8_p(element)) return TRUE;
          if (count == 0) break;
          x = I_to_uint8(element);
        store8:
          if (index & 1) {
            TheSbvector(dv)->data[index] = x;
            index++;
            if (--count == 0) break;
          }
          if (count & 1) {
            TheSbvector(dv)->data[index+count-1] = x;
            if (--count == 0) break;
          }
          count = count/2;
          index = index/2;
          x |= x << 8;
          goto store16;
        case Array_type_sb16vector:
          if (!uint16_p(element)) return TRUE;
          if (count == 0) break;
          x = I_to_uint16(element);
        store16:
          if (index & 1) {
            ((uint16*)&TheSbvector(dv)->data[0])[index] = x;
            index++;
            if (--count == 0) break;
          }
          if (count & 1) {
            ((uint16*)&TheSbvector(dv)->data[0])[index+count-1] = x;
            if (--count == 0) break;
          }
          count = count/2;
          index = index/2;
          x |= x << 16;
          goto store32;
        case Array_type_sb32vector:
          if (!uint32_p(element)) return TRUE;
          if (count == 0) break;
          x = I_to_uint32(element);
        store32:
          {
            var uint32* ptr = &((uint32*)&TheSbvector(dv)->data[0])[index];
            dotimespL(count,count, {
              *ptr++ = x;
            });
          }
          break;
       #endif
        case Array_type_sstring: # Simple-String
          if (!charp(element)) return TRUE;
          if (count > 0) {
            check_sstring_mutable(dv);
            var chart c = char_code(element);
            var chart* ptr = &TheSstring(dv)->data[index];
            dotimespL(count,count, {
              *ptr++ = c;
            });
          }
          break;
        default: NOTREACHED
      }
      return FALSE;
    }

# Function: Reverses a slice of an array, copying it into another array
# of the same element type.
# elt_reverse(dv1,index1,dv2,index2,count);
# > dv1: source storage-vector
# > index1: start index in dv1
# > dv2: destination storage-vector
# > index2: start index in dv2
# > count: number of elements to be copied, > 0
  global void elt_reverse (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  global void elt_reverse(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    {
      index2 += count-1;
      switch (Array_type(dv1)) {
        case Array_type_svector: # Simple-Vector
          {
            var const object* ptr1 = &TheSvector(dv1)->data[index1];
            var object* ptr2 = &TheSvector(dv2)->data[index2];
            dotimespL(count,count, {
              *ptr2-- = *ptr1++;
            });
          }
          break;
        case Array_type_sbvector: # Simple-Bit-Vector
          {
            var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
            var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
            dotimespL(count,count, {
              var uint8 value = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
              *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
              index1++;
              ptr1 += ((index1%8)==0);
              ptr2 -= ((index2%8)==0);
              index2--;
            });
          }
          break;
        case Array_type_sb2vector:
          {
            var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
            var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
            dotimespL(count,count, {
              var uint8 value = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
              *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
              index1++;
              ptr1 += ((index1%4)==0);
              ptr2 -= ((index2%4)==0);
              index2--;
            });
          }
          break;
        case Array_type_sb4vector:
          {
            var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
            var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
            dotimespL(count,count, {
              var uint8 value = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
              *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
              index1++;
              ptr1 += ((index1%2)==0);
              ptr2 -= ((index2%2)==0);
              index2--;
            });
          }
          break;
        case Array_type_sb8vector:
          {
            var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
            var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
            dotimespL(count,count, {
              *ptr2-- = *ptr1++;
            });
          }
          break;
        case Array_type_sb16vector:
          {
            var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
            var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
            dotimespL(count,count, {
              *ptr2-- = *ptr1++;
            });
          }
          break;
        case Array_type_sb32vector:
          {
            var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
            var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
            dotimespL(count,count, {
              *ptr2-- = *ptr1++;
            });
          }
          break;
        case Array_type_sstring: # Simple-String
          check_sstring_mutable(dv2);
          {
            var chart* ptr2 = &TheSstring(dv2)->data[index2];
            SstringDispatch(dv1,
              {
                var const chart* ptr1 = &TheSstring(dv1)->data[index1];
                dotimespL(count,count, {
                  *ptr2-- = *ptr1++;
                });
              },
              {
                var const scint* ptr1 = &TheSmallSstring(dv1)->data[index1];
                dotimespL(count,count, {
                  *ptr2-- = as_chart(*ptr1++);
                });
              }
              );
          }
          break;
        default: NOTREACHED
      }
    }

# Function: Reverses a slice of an array destructively.
# elt_nreverse(dv,index,count);
# > dv: storage-vector
# > index: start index in dv
# > count: number of elements to be reversed, > 0
  global void elt_nreverse (object dv, uintL index, uintL count);
  global void elt_nreverse(dv,index,count)
    var object dv;
    var uintL index;
    var uintL count;
    {
      var uintL index1 = index;
      var uintL index2 = index+count-1;
      count = floor(count,2);
      switch (Array_type(dv)) {
        case Array_type_svector: # Simple-Vector
          if (count > 0) {
            var object* ptr1 = &TheSvector(dv)->data[index1];
            var object* ptr2 = &TheSvector(dv)->data[index2];
            dotimespL(count,count, {
              var object tmp = *ptr1; *ptr1++ = *ptr2; *ptr2-- = tmp;
            });
          }
          break;
        case Array_type_sbvector: # Simple-Bit-Vector
          if (count > 0) {
            var uint8* ptr1 = &TheSbvector(dv)->data[index1/8];
            var uint8* ptr2 = &TheSbvector(dv)->data[index2/8];
            dotimespL(count,count, {
              var uint8 x1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
              var uint8 x2 = (*ptr2 >> ((~index2)%8)) & (bit(1)-1);
              *ptr1 ^= (*ptr1 ^ (x2 << ((~index1)%8))) & ((bit(1)-1) << ((~index1)%8));
              *ptr2 ^= (*ptr2 ^ (x1 << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
              index1++;
              ptr1 += ((index1%8)==0);
              ptr2 -= ((index2%8)==0);
              index2--;
            });
          }
          break;
        case Array_type_sb2vector:
          if (count > 0) {
            var uint8* ptr1 = &TheSbvector(dv)->data[index1/4];
            var uint8* ptr2 = &TheSbvector(dv)->data[index2/4];
            dotimespL(count,count, {
              var uint8 x1 = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
              var uint8 x2 = (*ptr2 >> (2*((~index2)%4))) & (bit(2)-1);
              *ptr1 ^= (*ptr1 ^ (x2 << (2*((~index1)%4)))) & ((bit(2)-1) << (2*((~index1)%4)));
              *ptr2 ^= (*ptr2 ^ (x1 << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
              index1++;
              ptr1 += ((index1%4)==0);
              ptr2 -= ((index2%4)==0);
              index2--;
            });
          }
          break;
        case Array_type_sb4vector:
          if (count > 0) {
            var uint8* ptr1 = &TheSbvector(dv)->data[index1/2];
            var uint8* ptr2 = &TheSbvector(dv)->data[index2/2];
            dotimespL(count,count, {
              var uint8 x1 = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
              var uint8 x2 = (*ptr2 >> (4*((~index2)%2))) & (bit(4)-1);
              *ptr1 ^= (*ptr1 ^ (x2 << (4*((~index1)%2)))) & ((bit(4)-1) << (4*((~index1)%2)));
              *ptr2 ^= (*ptr2 ^ (x1 << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
              index1++;
              ptr1 += ((index1%2)==0);
              ptr2 -= ((index2%2)==0);
              index2--;
            });
          }
          break;
        case Array_type_sb8vector:
          if (count > 0) {
            var uint8* ptr1 = &TheSbvector(dv)->data[index1];
            var uint8* ptr2 = &TheSbvector(dv)->data[index2];
            dotimespL(count,count, {
              var uint8 tmp = *ptr1; *ptr1++ = *ptr2; *ptr2-- = tmp;
            });
          }
          break;
        case Array_type_sb16vector:
          if (count > 0) {
            var uint16* ptr1 = &((uint16*)&TheSbvector(dv)->data[0])[index1];
            var uint16* ptr2 = &((uint16*)&TheSbvector(dv)->data[0])[index2];
            dotimespL(count,count, {
              var uint16 tmp = *ptr1; *ptr1++ = *ptr2; *ptr2-- = tmp;
            });
          }
          break;
        case Array_type_sb32vector:
          if (count > 0) {
            var uint32* ptr1 = &((uint32*)&TheSbvector(dv)->data[0])[index1];
            var uint32* ptr2 = &((uint32*)&TheSbvector(dv)->data[0])[index2];
            dotimespL(count,count, {
              var uint32 tmp = *ptr1; *ptr1++ = *ptr2; *ptr2-- = tmp;
            });
          }
          break;
        case Array_type_sstring: # Simple-String
          check_sstring_mutable(dv);
          if (count > 0) {
            var chart* ptr1 = &TheSstring(dv)->data[index1];
            var chart* ptr2 = &TheSstring(dv)->data[index2];
            dotimespL(count,count, {
              var chart tmp = *ptr1; *ptr1++ = *ptr2; *ptr2-- = tmp;
            });
          }
          break;
        default: NOTREACHED
      }
    }

# ============================================================================
#                 Fill pointers, extendable vectors

# Function: Tests whether an array has a fill-pointer.
# array_has_fill_pointer_p(array)
# > array: ein Array
# < result: TRUE, if it has a fill-pointer, else FALSE.
  global boolean array_has_fill_pointer_p (object array);
  global boolean array_has_fill_pointer_p(array)
    var object array;
    {
      if (simplep(array)) {
        return FALSE;
      } else {
        if (Iarray_flags(array) & bit(arrayflags_fillp_bit))
          return TRUE;
        else
          return FALSE;
      }
    }

LISPFUNN(array_has_fill_pointer_p,1) # (ARRAY-HAS-FILL-POINTER-P array), CLTL S. 296
  {
    var object array = popSTACK();
    test_array(array);
    value1 = (array_has_fill_pointer_p(array) ? T : NIL); mv_count=1;
  }

# Überprüft, ob ein Objekt ein Vektor mit Fill-Pointer ist, und liefert
# die Adresse des Fill-Pointers.
# *get_fill_pointer(obj) ist dann der Fill-Pointer selbst.
# get_fill_pointer(obj)[-1] ist dann die Länge (Dimension 0) des Vektors.
# > subr_self: Aufrufer (ein SUBR)
  local uintL* get_fill_pointer (object obj);
  local uintL* get_fill_pointer(obj)
    var object obj;
    {
      # obj muss ein Vektor sein:
      if (!vectorp(obj))
        fehler_vector(obj);
      # darf nicht simple sein:
      if (simplep(obj))
        goto fehler_fillp;
      # muss einen Fill-Pointer enthalten:
      if (!(Iarray_flags(obj) & bit(arrayflags_fillp_bit)))
        goto fehler_fillp;
      # Wo steht der Fill-Pointer?
      return ((Iarray_flags(obj) & bit(arrayflags_dispoffset_bit))
              ? &TheIarray(obj)->dims[2] # nach Displaced-Offset und Dimension 0
              : &TheIarray(obj)->dims[1] # nach der Dimension 0
             );
     fehler_fillp:
      # Fehlermeldung:
      pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_vector_with_fill_pointer)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: vector ~ has no fill pointer")
            );
    }

LISPFUNN(fill_pointer,1) # (FILL-POINTER vector), CLTL S. 296
  {
    var object obj = popSTACK();
    value1 = fixnum(* get_fill_pointer(obj)); # Fill-Pointer holen, als Fixnum
    mv_count=1;
  }

LISPFUNN(set_fill_pointer,2) # (SYS::SET-FILL-POINTER vector index)
                             # = (SETF (FILL-POINTER vector) index), CLTL S. 296
  {
    var uintL* fillp = get_fill_pointer(STACK_1); # Fillpointer-Adresse
    if (!posfixnump(STACK_0)) # neuer Fill-Pointer muss Fixnum>=0 sein.
      fehler_index_type();
    var uintL newfillp = posfixnum_to_L(STACK_0); # als uintL
    if (!(newfillp <= fillp[-1])) # muss kleinergleich der Länge sein
      fehler_index_range(fillp[-1]+1);
    *fillp = newfillp; # neuen Fill-Pointer eintragen
    value1 = STACK_0; mv_count=1; # neuen Fillpointer zurück
    skipSTACK(2);
  }

LISPFUNN(vector_push,2) # (VECTOR-PUSH new-element vector), CLTL S. 296
  {
    var uintL* fillp = get_fill_pointer(STACK_0); # Fillpointer-Adresse
    var uintL oldfillp = *fillp; # alter Wert des Fillpointers
    if (oldfillp >= fillp[-1]) { # Fill-Pointer am Ende?
      value1 = NIL; mv_count=1; # NIL zurück
    } else {
      var uintL index = oldfillp;
      var object datenvektor = iarray_displace(STACK_0,&index);
      storagevector_store(datenvektor,index,STACK_1); # new-element eintragen
      (*fillp)++; # Fill-Pointer erhöhen
      value1 = fixnum(oldfillp); mv_count=1;
      # alter Fill-Pointer als Wert
    }
    skipSTACK(2);
  }

LISPFUNN(vector_pop,1) # (VECTOR-POP vector), CLTL S. 296
  {
    var object array = popSTACK();
    var uintL* fillp = get_fill_pointer(array);
    if (*fillp==0) {
      # Fill-Pointer war =0 -> Fehlermeldung
      pushSTACK(array); pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~: ~ has length zero")
            );
    } else {
      var uintL index = --(*fillp); # Fill-Pointer erniedrigen
      var object datenvektor = iarray_displace(array,&index);
      value1 = storagevector_aref(datenvektor,index); mv_count=1; # Element zurück
    }
  }

LISPFUN(vector_push_extend,2,1,norest,nokey,0,NIL)
# (VECTOR-PUSH-EXTEND new-element vector [extension]), CLTL S. 296
  {
    var object extension = popSTACK(); # Extension (ungeprüft)
    var uintL* fillp = get_fill_pointer(STACK_0); # Fillpointer-Adresse
    var uintL oldfillp = *fillp; # alter Wert des Fillpointers
    if (oldfillp < fillp[-1]) { # Fill-Pointer noch nicht am Ende?
      var uintL index = oldfillp;
      var object datenvektor = iarray_displace(STACK_0,&index);
      storagevector_store(datenvektor,index,STACK_1); # new-element eintragen
      (*fillp)++; # Fill-Pointer erhöhen
    } else {
      # Fill-Pointer am Ende -> Versuche, den Vektor zu verlängern:
      var object array = STACK_0;
      if (!(Iarray_flags(array) & bit(arrayflags_adjustable_bit))) {
        # Vektor nicht adjustable -> Fehlermeldung:
        # array noch in STACK_0
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~ works only on adjustable arrays, not on ~")
              );
      }
      var uintB atype = Iarray_flags(array) & arrayflags_atype_mask;
      var uintL len = fillp[-1]; # bisherige Länge (Dimension 0)
      var uintL inc; # gewünschter Increment der Länge
      if (!eq(extension,unbound)) {
        # extension sollte ein Fixnum >0, <arraysize_limit sein:
        if ( !posfixnump(extension)
             || ((inc = posfixnum_to_L(extension)) == 0)
             #ifndef UNIX_DEC_ULTRIX_GCCBUG
             || (inc > arraysize_limit_1)
             #endif
           ) {
          pushSTACK(extension); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_posfixnum1)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(extension); pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,
                 GETTEXT("~: extension ~ should be a positive fixnum")
                );
        }
      } else {
        # Default-Verlängerung:
        switch (atype) {
          case Atype_T:    inc =  16; break; # bei general-Vektoren: 16 Objekte
          case Atype_Char: inc =  64; break; # bei Strings: 64 Zeichen
          case Atype_Bit:  inc = 128; break; # bei Bit-Vektoren: 128 Bits
          case Atype_2Bit: case Atype_4Bit: case Atype_8Bit:
          case Atype_16Bit: case Atype_32Bit: # bei Byte-Vektoren: entsprechend
                           inc = bit(floor(14-atype,2)); break;
          default: NOTREACHED
        }
        # mindestens jedoch die bisherige Länge:
        if (inc<len)
          inc = len;
      }
      var uintL newlen = len + inc; # neue Länge
      #ifndef UNIX_DEC_ULTRIX_GCCBUG
      if (newlen > arraysize_limit_1) {
        # Vektor würde zu lang -> Fehlermeldung
        pushSTACK(extension); pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: extending the vector by ~ elements makes it too long")
              );
      }
      #endif
      # Neuen Datenvektor holen. Dazu Fallunterscheidung je nach Typ:
      var object neuer_datenvektor;
      switch (atype) {
        case Atype_T: # array ist ein General-Vector
          neuer_datenvektor = allocate_vector(newlen);
          array = STACK_0; # array wieder holen
          # alten in neuen Datenvektor kopieren:
          if (len>0) {
            var uintL index = 0;
            var object datenvektor = iarray_displace_check(array,len,&index);
            elt_copy_T_T(datenvektor,index,neuer_datenvektor,0,len);
          }
          # dann new_element anfügen:
          TheSvector(neuer_datenvektor)->data[len] = STACK_1;
          break;
        case Atype_Char: # array ist ein String
          neuer_datenvektor = allocate_string(newlen);
          array = STACK_0; # array wieder holen
          # alten in neuen Datenvektor kopieren:
          if (len>0) {
            var uintL index = 0;
            var object datenvektor = iarray_displace_check(array,len,&index);
            elt_copy_Char_Char(datenvektor,index,neuer_datenvektor,0,len);
          }
          # dann new_element anfügen:
          if (!charp(STACK_1))
            goto fehler_type;
          TheSstring(neuer_datenvektor)->data[len] = char_code(STACK_1);
          break;
        case Atype_Bit: # array ist ein Bit-Vektor
        case Atype_2Bit: case Atype_4Bit: case Atype_8Bit:
        case Atype_16Bit: case Atype_32Bit: # array ist ein Byte-Vektor
          neuer_datenvektor = allocate_bit_vector(atype,newlen);
          array = STACK_0; # array wieder holen
          # alten in neuen Datenvektor kopieren:
          if (len>0) {
            var uintL index = 0;
            var object datenvektor = iarray_displace_check(array,len,&index);
            switch (atype) {
              case Atype_Bit:
              case Atype_2Bit:
              case Atype_4Bit:
                bit_copy(datenvektor,index<<atype,neuer_datenvektor,0<<atype,len<<atype);
                break;
              case Atype_8Bit:
                elt_copy_8Bit_8Bit(datenvektor,index,neuer_datenvektor,0,len);
                break;
              case Atype_16Bit:
                elt_copy_16Bit_16Bit(datenvektor,index,neuer_datenvektor,0,len);
                break;
              case Atype_32Bit:
                elt_copy_32Bit_32Bit(datenvektor,index,neuer_datenvektor,0,len);
                break;
              default: NOTREACHED
            }
          }
          # new-element eintragen:
          storagevector_store(neuer_datenvektor,len,STACK_1);
          break;
        default: NOTREACHED
        fehler_type:
          {
            # Stackaufbau: new-element, vector.
            pushSTACK(STACK_1); # Wert für Slot DATUM von TYPE-ERROR
            pushSTACK(array_element_type(STACK_(0+1))); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
            pushSTACK(STACK_(0+2)); pushSTACK(STACK_(1+3));
            pushSTACK(TheSubr(subr_self)->name);
            fehler(type_error,
                   GETTEXT("~: cannot push ~ into array ~ (bad type)")
                  );
          }
      }
      set_break_sem_1(); # Unterbrechungen verbieten
      TheIarray(array)->data = neuer_datenvektor; # neuen Vektor als Datenvektor eintragen
      iarray_flags_clr(TheIarray(array),bit(arrayflags_displaced_bit)); # Displaced-Bit löschen
      TheIarray(array)->dims[2] += 1; # Fillpointer um 1 erhöhen
      TheIarray(array)->dims[1] = newlen; # neue Länge eintragen
      TheIarray(array)->totalsize = newlen; # ist auch neue totalsize
      clr_break_sem_1(); # Unterbrechungen wieder zulassen
    }
    value1 = fixnum(oldfillp); mv_count=1;
    # alter Fill-Pointer als Wert
    skipSTACK(2);
  }

# ============================================================================
#                      Bit vectors

# Function: Allocates a new simple-bit-vector, filled with zeroes.
# allocate_bit_vector_0(len)
# > uintL len: length of the desired bit-vector (number of bits)
# < result: fresh simple-bit-vector, filled with zeroes
# can trigger GC
  global object allocate_bit_vector_0 (uintL len);
  global object allocate_bit_vector_0(len)
    var uintL len;
    {
      var object newvec = allocate_bit_vector(Atype_Bit,len); # neuer Bit-Vektor
      var uintL count = ceiling(len,bitpack); # ceiling(len/bitpack) Worte mit Nullen füllen
      if (!(count==0)) {
        var uint_bitpack* ptr = (uint_bitpack*)(&TheSbvector(newvec)->data[0]);
        dotimespL(count,count, {
          *ptr++ = 0;
        });
      }
      return newvec;
    }

#if 0 # nur als Reserve, für den Fall, dass wir wieder auf ein GCC-Bug stoßen

# UP: löscht ein Bit in einem Simple-Bit-Vector
# sbvector_bclr(sbvector,index);
# > sbvector: ein Simple-Bit-Vector
# > index: Index (Variable, sollte < (length sbvector) sein)
  global void sbvector_bclr (object sbvector, uintL index);
  global void sbvector_bclr(sbvector,index)
    var object sbvector;
    var uintL index;
    {
      # im Byte (index div 8) das Bit 7 - (index mod 8) löschen:
      TheSbvector(sbvector)->data[index/8] &= ~bit((~index) % 8);
    }

# UP: setzt ein Bit in einem Simple-Bit-Vector
# sbvector_bset(sbvector,index);
# > sbvector: ein Simple-Bit-Vector
# > index: Index (Variable, sollte < (length sbvector) sein)
  global void sbvector_bset (object sbvector, uintL index);
  global void sbvector_bset(sbvector,index)
    var object sbvector;
    var uintL index;
    {
      # im Byte (index div 8) das Bit 7 - (index mod 8) setzen:
      TheSbvector(sbvector)->data[index/8] |= bit((~index) % 8);
    }

#endif

# ============================================================================
#                    Semi-simple strings

# The following functions work on "semi-simple string"s.
# That are CHARACTER arrays with FILL-POINTER, (pro forma) not adjustable and
# not displaced, whose storagevector is a normal-simple-string. When their
# length is exceeded, the length is doubled (so that the resizing effort
# becomes unimportant: adding a character is still O(1) on average.)

# Function: Returns a fresh semi-simple-string of given length, with
# fill-pointer = 0.
# make_ssstring(len)
# > uintL len: desired length, must be >0
# < fresh: fresh semi-simple-string of the given length
# can trigger GC
  global object make_ssstring (uintL len);
  global object make_ssstring(len)
    var uintL len;
    {
      var object new_string = allocate_string(len);
      # neuer Normal-Simple-String dieser Länge
      pushSTACK(new_string); # retten
      var object new_array =
        allocate_iarray(bit(arrayflags_fillp_bit)|Atype_Char,1,Array_type_string);
        # Flags: nur FILL_POINTER_BIT, Elementtyp CHARACTER, Rang=1
      TheIarray(new_array)->dims[1] = 0; # Fill-Pointer := 0
      TheIarray(new_array)->totalsize =
        TheIarray(new_array)->dims[0] = len; # Länge und Total-Size eintragen
      TheIarray(new_array)->data = popSTACK(); # Datenvektor eintragen
      return new_array;
    }

# Function: Adds a character to a semi-simple-string, thereby possibly
# extending it.
# ssstring_push_extend(ssstring,ch)
# > ssstring: a semi-simple-string
# > ch: a character
# < result: the same semi-simple-string
# can trigger GC
  global object ssstring_push_extend (object ssstring, chart ch);
  global object ssstring_push_extend(ssstring,ch)
    var object ssstring;
    var chart ch;
    {
      var object sstring = TheIarray(ssstring)->data; # Datenvektor (ein Normal-Simple-String)
      if (TheIarray(ssstring)->dims[1] # Fill-Pointer
          >= Sstring_length(sstring) ) { # >= Länge ?
        # ja -> String wird um den Faktor 2 länger gemacht
        pushSTACK(ssstring); # ssstring retten
        pushSTACK(sstring); # Datenvektor ebenfalls retten
        var object neuer_sstring = allocate_string(2 * Sstring_length(sstring));
        # neuer Normal-Simple-String der doppelten Länge
        sstring = popSTACK(); # sstring zurück
        # Stringinhalt von String sstring nach String neuer_sstring kopieren:
        chartcopy(&TheSstring(sstring)->data[0],&TheSstring(neuer_sstring)->data[0],Sstring_length(sstring));
        ssstring = popSTACK(); # ssstring zurück
        set_break_sem_1(); # Unterbrechungen verbieten
        TheIarray(ssstring)->data = neuer_sstring; # neuen String als Datenvektor abspeichern
        TheIarray(ssstring)->totalsize =
          TheIarray(ssstring)->dims[0] = Sstring_length(neuer_sstring); # neue Länge eintragen
        clr_break_sem_1(); # Unterbrechungen wieder zulassen
        sstring = neuer_sstring;
      }
      # Nun ist wieder sstring der Datenvektor, und es gilt
      # Fill-Pointer < Länge(Datenvektor).
      # Character hineinschieben und Fill-Pointer erhöhen:
      TheSstring(sstring)->data[ TheIarray(ssstring)->dims[1]++ ] = ch;
      return ssstring;
    }

# Function: Ensures that a semi-simple-string has at least a given length,
# possibly extending it.
# ssstring_extend(ssstring,size)
# > ssstring: a semi-simple-string
# > size: desired minimum length
# < ergebnis: the same semi-simple-string
# can trigger GC
  global object ssstring_extend (object ssstring, uintL needed_len);
  global object ssstring_extend(ssstring,needed_len)
    var object ssstring;
    var uintL needed_len;
    {
      var object sstring = TheIarray(ssstring)->data; # Datenvektor (ein Normal-Simple-String)
      var uintL now_len = Sstring_length(sstring); # jetzige Maximal-Länge
      if (needed_len > now_len) {
        # ja -> String wird länger gemacht, mindestens um den Faktor 2:
        pushSTACK(ssstring); # ssstring retten
        pushSTACK(sstring); # Datenvektor ebenfalls retten
        now_len = now_len * 2;
        if (needed_len > now_len)
          now_len = needed_len; # now_len vergrößern
        var object neuer_sstring = allocate_string(now_len);
        # neuer Normal-Simple-String mindestens der gewünschten und der doppelten Länge
        sstring = popSTACK(); # sstring zurück
        # Stringinhalt von String sstring nach String neuer_sstring kopieren:
        chartcopy(&TheSstring(sstring)->data[0],&TheSstring(neuer_sstring)->data[0],Sstring_length(sstring));
        ssstring = popSTACK(); # ssstring zurück
        set_break_sem_1(); # Unterbrechungen verbieten
        TheIarray(ssstring)->data = neuer_sstring; # neuen String als Datenvektor abspeichern
        TheIarray(ssstring)->totalsize =
          TheIarray(ssstring)->dims[0] = now_len; # neue Länge eintragen
        clr_break_sem_1(); # Unterbrechungen wieder zulassen
      }
      return ssstring;
    }

# Function: Adds a substring to a semi-simple-string, thereby possibly
# extending it.
# ssstring_append_extend(ssstring,sstring,start,len)
# > ssstring: a semi-simple-string
# > sstring: a simple-string
# > start: the start index into the sstring
# > len: the number of characters to be pushed, starting from start
# < result: the same semi-simple-string
# can trigger GC
  global object ssstring_append_extend (object ssstring, object sstring, uintL start, uintL len);
  global object ssstring_append_extend(ssstring,srcstring,start,len)
    var object ssstring;
    var object srcstring;
    var uintL start;
    var uintL len;
    {
      var uintL old_len = TheIarray(ssstring)->dims[1]; # jetzige Länge = Fill-Pointer
      if (old_len + len > TheIarray(ssstring)->dims[0]) { # passen keine len Bytes mehr hinein
        pushSTACK(srcstring);
        ssstring = ssstring_extend(ssstring,old_len+len); # dann länger machen
        srcstring = popSTACK();
      }
      # Zeichen hineinschieben:
      var chart* ptr = &TheSstring(TheIarray(ssstring)->data)->data[old_len];
      SstringDispatch(srcstring,
        { chartcopy(&TheSstring(srcstring)->data[start],ptr,len); },
        { scintcopy(&TheSmallSstring(srcstring)->data[start],ptr,len); }
        );
      # und Fill-Pointer erhöhen:
      TheIarray(ssstring)->dims[1] = old_len + len;
      return ssstring;
    }

# ============================================================================
#                 Semi-simple byte vectors

# The following functions work on "semi-simple byte-vector"s.
# That are bit vectors with FILL-POINTER, (pro forma) not adjustable and
# not displaced, whose storagevector is a simple-bit-vector. When their
# length is exceeded, the length is doubled (so that the resizing effort
# becomes unimportant: adding a character is still O(1) on average.)

# Function: Returns a fresh semi-simple byte-vector of given length, with
# fill-pointer = 0.
# make_ssbvector(len)
# > uintL len: length (number of bytes!), must be >0
# < result: fresh semi-simple byte-vector of the given length
# can trigger GC
  global object make_ssbvector (uintL len);
  global object make_ssbvector(len)
    var uintL len;
    {
      pushSTACK(allocate_bit_vector(Atype_8Bit,len));
      var object new_array =
        allocate_iarray(bit(arrayflags_fillp_bit)|Atype_8Bit,1,Array_type_b8vector);
        # Flags: nur FILL_POINTER_BIT, Elementtyp BIT, Rang=1
      TheIarray(new_array)->dims[1] = 0; # Fill-Pointer := 0
      TheIarray(new_array)->totalsize =
        TheIarray(new_array)->dims[0] = len; # Länge und Total-Size eintragen
      TheIarray(new_array)->data = popSTACK(); # Datenvektor eintragen
      return new_array;
    }

# Function: Adds a byte to a semi-simple byte vector, thereby possibly
# extending it.
# ssbvector_push_extend(ssbvector,b)
# > ssbvector: a semi-simple byte-vector
# > b: byte
# < result: the same semi-simple byte-vector
# can trigger GC
  global object ssbvector_push_extend (object ssbvector, uintB b);
  global object ssbvector_push_extend(ssbvector,b)
    var object ssbvector;
    var uintB b;
    {
      var object sbvector = TheIarray(ssbvector)->data; # Datenvektor (ein Simple-8Bit-Vektor)
      if (TheIarray(ssbvector)->dims[1] # Fill-Pointer
          >= Sbvector_length(sbvector) ) { # >= Länge ?
        # ja -> Bit-Vektor wird um den Faktor 2 länger gemacht
        pushSTACK(ssbvector); # ssbvector retten
        pushSTACK(sbvector); # Datenvektor ebenfalls retten
        var object neuer_sbvector = allocate_bit_vector(Atype_8Bit,2*Sbvector_length(sbvector));
        # neuer Simple-8Bit-Vektor der doppelten Länge
        sbvector = popSTACK(); # sbvector zurück
        # Inhalt von sbvector nach neuer_sbvector kopieren:
        elt_copy_8Bit_8Bit(sbvector,0,neuer_sbvector,0,Sbvector_length(sbvector));
        ssbvector = popSTACK(); # ssbvector zurück
        set_break_sem_1(); # Unterbrechungen verbieten
        TheIarray(ssbvector)->data = neuer_sbvector; # neuen Bit-Vektor als Datenvektor abspeichern
        TheIarray(ssbvector)->totalsize =
          TheIarray(ssbvector)->dims[0] = Sbvector_length(neuer_sbvector); # neue Länge eintragen
        clr_break_sem_1(); # Unterbrechungen wieder zulassen
        sbvector = neuer_sbvector;
      }
      # Nun ist wieder sbvector der Datenvektor, und es gilt
      # Fill-Pointer < Länge(Datenvektor).
      # Character hineinschieben und Fill-Pointer erhöhen:
      TheSbvector(sbvector)->data[ TheIarray(ssbvector)->dims[1]++ ] = b;
      return ssbvector;
    }

# ============================================================================
#                            MAKE-ARRAY

# Stackaufbau bei MAKE-ARRAY :
#   dims, adjustable, element-type, initial-element, initial-contents,
#   fill-pointer, displaced-to, displaced-index-offset.
# Stackaufbau bei ADJUST-ARRAY :
#   dims, array, element-type, initial-element, initial-contents,
#   fill-pointer, displaced-to, displaced-index-offset.

# Fehlermeldung
# > dim: fehlerhafte Dimension
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_dim_type, (object dim));
  local void fehler_dim_type(dim)
    var object dim;
    {
      pushSTACK(dim); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_array_index)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(dim);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: dimension ~ is not of type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))")
            );
    }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# Überprüft die Dimensionen und liefert den Rang und die Gesamtgröße.
# test_dims(&totalsize)
# > STACK_7: Dimension oder Dimensionenliste
# > subr_self: Aufrufer (ein SUBR)
# < totalsize: Gesamtgröße = Produkt der Dimensionen
# < ergebnis: Rang = Anzahl der Dimensionen
  local uintL test_dims (uintL* totalsize_);
  local uintL test_dims(totalsize_)
    var uintL* totalsize_;
    { var object dims = STACK_7;
      if (listp(dims)) {
        var uintL rank = 0; # bisherige Anzahl der Dimensionen
        var uintL totalsize = 1; # bisheriges Produkt der Dimensionen,
                                 # bleibt < arraysize_limit
        while (consp(dims)) {
          var object dim = Car(dims); # nächste Dimension
          # if (!integerp(dim)) fehler_dim_type(dim); # muss Integer sein
          if (!posfixnump(dim)) fehler_dim_type(dim); # muss Fixnum >=0 sein
          # totalsize * dim bilden:
          var uintL produkt_hi;
          var uintL produkt_lo;
          #if (oint_data_len<=24)
          mulu24(totalsize,posfixnum_to_L(dim), produkt_hi=,produkt_lo=);
          #else
          mulu32(totalsize,posfixnum_to_L(dim), produkt_hi=,produkt_lo=);
          #endif
          #ifndef UNIX_DEC_ULTRIX_GCCBUG
          if (!((produkt_hi==0) && (produkt_lo<=arraysize_limit_1))) # Produkt < 2^24 ?
          #else
          if (!(produkt_hi==0))
          #endif
            {
              # nein -> (sofern nicht noch eine Dimension=0 kommt)
              # Total-Size zu groß
              pushSTACK(STACK_7); # dims
              pushSTACK(TheSubr(subr_self)->name);
              fehler(error,
                     GETTEXT("~: dimensions ~ produce too large total-size")
                    );
            }
          totalsize = produkt_lo;
          rank++;
          dims = Cdr(dims);
        }
        *totalsize_ = totalsize;
        return rank;
      } else {
        # dims ist keine Liste. Sollte eine einzelne Dimension sein:
        # if (!integerp(dims)) fehler_dim_type(dims); # muss Integer sein
        if (!posfixnump(dims)) fehler_dim_type(dims); # muss Fixnum >=0 sein
        *totalsize_ = posfixnum_to_L(dims); # Totalsize = einzige Dimension
        return 1; # Rang = 1
      }
    }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# Überprüft einige der Keywords.
  local void test_otherkeys (void);
  local void test_otherkeys()
    {
      # fill-pointer hat Defaultwert NIL:
      if (eq(STACK_2,unbound))
        STACK_2 = NIL;
      # displaced-to hat Defaultwert NIL:
      if (eq(STACK_1,unbound))
        STACK_1 = NIL;
      # Testen, ob mehr als eine Initialisierung
      # (:initial-element, :initial-contents, :displaced-to) angegeben wurde:
      {
        var uintC initcount = 0; # Zähler
        if (!(eq(STACK_4,unbound))) # initial-element angegeben?
          initcount++;
        if (!(eq(STACK_3,unbound))) # initial-contents angegeben?
          initcount++;
        if (!nullp(STACK_1)) # displaced-to angegeben?
          initcount++;
        if (initcount > 1) { # Mehr als eine Initialisierung?
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: ambiguous, more than one initialisation specified")
                );
        }
      }
      # Testen, ob :displaced-index-offset ohne :displaced-to verwendet wurde:
      if ((!eq(STACK_0,unbound)) # displaced-index-offset angegeben?
          && (nullp(STACK_1)) # und displaced-to nicht angegeben?
         ) {
        pushSTACK(S(Kdisplaced_to));
        pushSTACK(S(Kdisplaced_index_offset));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: ~ must not be specified without ~")
              );
      }
    }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# erzeugt einen Datenvektor gegebener Länge
# und füllt ihn mit initial-element, falls angegeben.
# make_storagevector(len,eltype)
# > len: Länge
# > eltype: Elementtyp-Code
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: einfacher Vektor des gegebenen Typs, evtl. gefüllt.
# can trigger GC
  local object make_storagevector (uintL len, uintB eltype);
  local object make_storagevector(len,eltype)
    var uintL len;
    var uintB eltype;
    {
      var object vector;
      switch (eltype) {
        case Atype_T: # Simple-Vector erzeugen
          vector = allocate_vector(len);
          break;
        case Atype_Char: # Normal-Simple-String erzeugen
          vector = allocate_string(len);
          break;
        case Atype_Bit:
        case Atype_2Bit:
        case Atype_4Bit:
        case Atype_8Bit:
        case Atype_16Bit:
        case Atype_32Bit: # simplen Bit/Byte-Vektor erzeugen
          vector = allocate_bit_vector(eltype,len);
          break;
        default: NOTREACHED
      }
      if (!eq(STACK_4,unbound)) # initial-element angegeben?
        if (!(len==0)) { # und Länge > 0 ?
          # Fill vector with initial-element:
          if (elt_fill(vector,0,len,STACK_4)) {
            pushSTACK(STACK_4); # Wert für Slot DATUM von TYPE-ERROR
            pushSTACK(STACK_(5+1)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
            pushSTACK(STACK_(5+2)); # element-type
            pushSTACK(STACK_(4+3)); # initial-element
            pushSTACK(TheSubr(subr_self)->name);
            fehler(type_error,
                   GETTEXT("~: the initial-element ~ is not of type ~")
                  );
          }
        }
      return vector;
    }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# Füllt einen Vektor lexikographisch mit dem Inhalt einer verschachtelten
# Sequence-Struktur, wie sie als Argument zum Keyword :initial-contents
# bei MAKE-ARRAY und ADJUST-ARRAY anzugeben ist.
# initial_contents(datenvektor,dims,rank,contents)
# > datenvektor: ein simpler Vektor
# > dims: Dimension oder Dimensionenliste, alle Dimensionen Fixnums,
#         Länge(datenvektor) = Produkt der Dimensionen
# > rank: Anzahl der Dimensionen
# > contents: verschachtelte Sequence-Struktur
# < ergebnis: derselbe Datenvektor
# Nicht reentrant!
# can trigger GC
  local object initial_contents (object datenvektor, object dims, uintL rank, object contents);
  typedef struct { object* localptr; # Pointer auf Datenvektor und Dimensionen
                   uintL index; # Index in den Datenvektor
                   uintL depth; # Rekursionstiefe
                 }
          initial_contents_locals;
  local map_sequence_function initial_contents_aux;
  local object initial_contents(datenvektor,dims,rank,contents)
    var object datenvektor;
    var object dims;
    var uintL rank;
    var object contents;
    {
      # alle Dimensionen auf den Stack:
      get_space_on_STACK(rank*sizeof(object));
      if (listp(dims)) {
        while (consp(dims)) {
          pushSTACK(Car(dims)); dims = Cdr(dims);
        }
      } else {
        pushSTACK(dims);
      }
      var initial_contents_locals locals;
      locals.localptr = &STACK_0; # aktuellen STACK-Wert merken
      locals.index = 0; # Index := 0
      locals.depth = rank; # depth := rank
      pushSTACK(datenvektor); # Datenvektor in den Stack
      pushSTACK(subr_self); # aktuelles SUBR retten
      initial_contents_aux(&locals,contents); # initial_contents_aux aufrufen
      subr_self = popSTACK(); # aktuelles SUBR zurück
      datenvektor = popSTACK(); # Datenvektor zurück
      skipSTACK(rank); # STACK aufräumen
      return datenvektor;
    }

# Hilfsfunktion für initial_contents:
# Arbeitet die Sequence-Struktur rekursiv ab.
local void initial_contents_aux (void* arg, object obj);
local void initial_contents_aux(arg,obj)
  var void* arg;
  var object obj;
  {
    var initial_contents_locals* locals = (initial_contents_locals*)arg;
    # Übergeben wird:
    # locals->depth = Rekursionstiefe,
    # locals->index = Index in den Datenvektor,
    # locals->localptr = Pointer auf die Dimensionen,
    #   bei Tiefe depth>0 ist maßgeblich
    #   Dimension (rank-depth) = *(localptr+depth-1),
    #   Datenvektor = *(localptr-1), Aufrufer = *(localptr-2).
    var object* localptr = locals->localptr;
    if (locals->depth==0) {
      # Tiefe 0 -> Element obj in den Datenvektor eintragen:
      var object datenvektor = *(localptr STACKop -1);
      subr_self = *(localptr STACKop -2);
      pushSTACK(obj);
      pushSTACK(datenvektor);
      storagevector_store(datenvektor,locals->index,STACK_1);
      locals->index++;
      skipSTACK(2); # Stack aufräumen
    } else {
      # Tiefe >0 -> rekursiv aufrufen:
      locals->depth--;
      pushSTACK(obj);
      # obj = STACK_0 muss eine Sequence korrekter Länge sein:
      pushSTACK(STACK_0); funcall(L(length),1); # Länge bestimmen
      # muss EQL (also EQ) zur Dimension *(localptr+depth) sein:
      if (!(eq(value1,*(localptr STACKop locals->depth)))) {
        # fehlerhafte Sequence seq noch in STACK_0.
        pushSTACK(TheSubr(*(localptr STACKop -2))->name);
        fehler(error,
               GETTEXT("~: ~ is of incorrect length")
              );
      }
      # Länge stimmt, nun (MAP NIL #'INITIAL-CONTENTS-AUX seq) ausführen:
      map_sequence(STACK_0,&initial_contents_aux,locals);
      locals->depth++;
      skipSTACK(1); # Stack aufräumen
    }
  }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# Überprüfe ein displaced-to-Argument und den dazugehörigen Offset.
# test_displaced(eltype,totalsize)
# > eltype: Elementtyp-Code des zu erzeugenden Arrays
# > totalsize: Gesamtgröße des zu erzeugenden Arrays
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Wert des displaced-index-offset
  local uintL test_displaced (uintB eltype, uintL totalsize);
  local uintL test_displaced(eltype,totalsize)
    var uintB eltype;
    var uintL totalsize;
    {
      # displaced-to überprüfen, muss ein Array sein:
      var object displaced_to = STACK_1;
      if (!arrayp(displaced_to)) {
        pushSTACK(displaced_to); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(array)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(displaced_to);
        pushSTACK(S(Kdisplaced_to));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: ~-argument ~ is not an array")
              );
      }
      {
        # Elementtyp von displaced_to bestimmen:
        var uintB displaced_eltype;
        switch (Array_type(STACK_1)) {
          case Array_type_mdarray: # allgemeiner Array -> Arrayflags anschauen
            displaced_eltype = Iarray_flags(displaced_to) & arrayflags_atype_mask;
            break;
          # Zuordnung  Vektor-Typinfo -> ATYPE-Byte :
          case Array_type_sbvector:
          case Array_type_sb2vector:
          case Array_type_sb4vector:
          case Array_type_sb8vector:
          case Array_type_sb16vector:
          case Array_type_sb32vector:
            displaced_eltype = sbNvector_atype(STACK_1); break;
          case Array_type_bvector:
          case Array_type_b2vector:
          case Array_type_b4vector:
          case Array_type_b8vector:
          case Array_type_b16vector:
          case Array_type_b32vector:
            displaced_eltype = bNvector_atype(STACK_1); break;
          case Array_type_string:
          case Array_type_sstring:
            displaced_eltype = Atype_Char; break;
          case Array_type_vector:
          case Array_type_svector:
            displaced_eltype = Atype_T; break;
          default: NOTREACHED
        }
        # displaced_eltype ist der ATYPE des :displaced-to-Arguments.
        # Gegebenen Elementtyp damit vergleichen:
        if (!(eltype == displaced_eltype)) {
          pushSTACK(displaced_to); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(S(array)); pushSTACK(STACK_(5+2)); pushSTACK(listof(2)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(STACK_(5+2)); # element-type
          pushSTACK(STACK_2); # displaced_to
          pushSTACK(S(Kdisplaced_to));
          pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,
                 GETTEXT("~: ~-argument ~ does not have element type ~")
                );
        }
      }
      # Displaced-Index-Offset überprüfen:
      var uintL displaced_index_offset;
      if (eq(STACK_0,unbound))
        displaced_index_offset = 0; # Default ist 0
      elif (posfixnump(STACK_0))
        displaced_index_offset = posfixnum_to_L(STACK_0);
      else {
        pushSTACK(STACK_0); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_array_index)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(STACK_(0+2));
        pushSTACK(S(Kdisplaced_index_offset));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: ~-argument ~ is not of type `(INTEGER 0 (,ARRAY-TOTAL-SIZE-LIMIT))")
              );
      }
      {
        # Überprüfen, ob angesprochenes Teilstück ganz in displaced-to passt:
        var uintL displaced_totalsize = array_total_size(displaced_to);
        if (!(displaced_index_offset+totalsize <= displaced_totalsize)) {
          pushSTACK(S(Kdisplaced_to));
          pushSTACK(fixnum(displaced_totalsize));
          pushSTACK(fixnum(displaced_index_offset));
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: array-total-size + displaced-offset (= ~) exceeds total size ~ of ~-argument")
                );
        }
      }
      return displaced_index_offset;
    }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# Überprüfe ein fill-pointer-Argument /=NIL.
# test_fillpointer(len)
# > totalsize: Maximalwert von fill-pointer
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Wert des fill-pointer
  local uintL test_fillpointer (uintL totalsize);
  local uintL test_fillpointer(totalsize)
    var uintL totalsize;
    {
      # fill-pointer war angegeben und /=NIL
      if (eq(STACK_2,S(t))) { # T angegeben ->
        return totalsize; # Fill-Pointer := Länge = Gesamtgröße
      } elif (!posfixnump(STACK_2)) { # kein Fixnum >=0 -> Fehler
        pushSTACK(STACK_2); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_posfixnum)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(STACK_(2+2));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: fill-pointer ~ should be a nonnegative fixnum")
              );
      } else {
        var uintL fillpointer = posfixnum_to_L(STACK_2);
        if (!(fillpointer <= totalsize)) { # mit Länge vergleichen
          pushSTACK(fixnum(totalsize));
          pushSTACK(STACK_(2+1));
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: fill-pointer argument ~ is larger than the length ~")
                );
        }
        return fillpointer;
      }
    }

LISPFUN(make_array,1,0,norest,key,7,\
        (kw(adjustable),kw(element_type),kw(initial_element),\
         kw(initial_contents),kw(fill_pointer),\
         kw(displaced_to),kw(displaced_index_offset)) )
# (MAKE-ARRAY dimensions :adjustable :element-type :initial-element
#   :initial-contents :fill-pointer :displaced-to :displaced-index-offset),
#   CLTL S. 286
  # Stackaufbau:
  #   dims, adjustable, element-type, initial-element, initial-contents,
  #   fill-pointer, displaced-to, displaced-index-offset.
  {
    # Dimensionen überprüfen und Rang und Total-Size berechnen:
    var uintL totalsize;
    var uintL rank = test_dims(&totalsize);
    # adjustable hat Defaultwert NIL:
    if (eq(STACK_6,unbound))
      STACK_6 = NIL;
    # element-type in einen Code umwandeln:
    var uintB eltype;
    if (!(eq(STACK_5,unbound))) {
      eltype = eltype_code(STACK_5);
    } else {
      # Defaultwert ist T.
      STACK_5 = S(t); eltype = Atype_T;
    }
    test_otherkeys(); # einiges überprüfen
    var uintB flags = eltype;
    var uintL displaced_index_offset;
    var uintL fillpointer;
    # Falls nicht displaced, Datenvektor bilden und evtl. füllen:
    if (nullp(STACK_1)) { # displaced-to nicht angegeben?
      # Datenvektor bilden:
      var object datenvektor = make_storagevector(totalsize,eltype);
      if (!eq(STACK_3,unbound)) # und falls initial-contents angegeben:
        datenvektor = initial_contents(datenvektor,STACK_7,rank,STACK_3); # füllen
      # Falls displaced-to nicht angegeben ist
      # und fill-pointer nicht angegeben ist
      # und adjustable nicht angegeben ist
      # und rank=1 ist,
      # ist ein (semi-)simpler Vektor zu liefern:
      if ((rank==1) && (nullp(STACK_6)) && (nullp(STACK_2))) {
        value1 = datenvektor; mv_count=1; # Datenvektor als Ergebnis
        skipSTACK(8); return;
      }
      # Es ist ein allgemeiner Array zu liefern.
      STACK_1 = datenvektor; # datenvektor als "displaced-to" ablegen
      displaced_index_offset = 0; # mit Displacement 0
      # und ohne Displacement-Bit in den Flags
    } else {
      # displaced-to angegeben -> Es ist ein allgemeiner Array zu liefern.
      displaced_index_offset = test_displaced(eltype,totalsize);
      # Flags enthalten das Displacement-Bit:
      flags |= bit(arrayflags_displaced_bit)|bit(arrayflags_dispoffset_bit);
    }
    # Erzeuge einen allgemeinen Array.
    # Rang überprüfen:
    #ifndef UNIX_DEC_ULTRIX_GCCBUG
    if (rank > arrayrank_limit_1) {
      pushSTACK(fixnum(rank)); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_array_rank)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(fixnum(rank));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: attempted rank ~ is too large")
            );
    }
    #endif
    # Flags für allocate_iarray zusammensetzen:
    # flags enthält schon eltype und evtl. Displacement-Bit.
    if (!nullp(STACK_6)) # adjustable angegeben?
      flags |= bit(arrayflags_adjustable_bit)|bit(arrayflags_dispoffset_bit);
    if (!nullp(STACK_2)) { # fill-pointer angegeben?
      if (!(rank==1)) { # Rang muss 1 sein
        pushSTACK(fixnum(rank));
        pushSTACK(S(Kfill_pointer));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: ~ may not be specified for an array of rank ~")
              );
      }
      flags |= bit(arrayflags_fillp_bit);
      fillpointer = test_fillpointer(totalsize); # Fill-Pointer-Wert
    }
    # Typinfo für das zu erzeugende Objekt bestimmen:
    var tint type;
    if (rank==1) {
      # Vektor: Typinfo aus Tabelle bestimmen
      local const tint type_table[8] =
        {
          # Tabelle für Zuordnung  ATYPE-Byte -> Vektor-Typinfo
          Array_type_bvector,   # Atype_Bit   -> Array_type_bvector
          Array_type_b2vector,  # Atype_2Bit  -> Array_type_b2vector
          Array_type_b4vector,  # Atype_4Bit  -> Array_type_b4vector
          Array_type_b8vector,  # Atype_8Bit  -> Array_type_b8vector
          Array_type_b16vector, # Atype_16Bit -> Array_type_b16vector
          Array_type_b32vector, # Atype_32Bit -> Array_type_b32vector
          Array_type_vector,    # Atype_T     -> Array_type_vector
          Array_type_string,    # Atype_Char  -> Array_type_string
                                # restliche ATYPEs unbenutzt
        };
      type = type_table[eltype];
    } else {
      # allgemeiner Array
      type = Array_type_mdarray;
    }
    # Array allozieren:
    var object array = allocate_iarray(flags,rank,type);
    TheIarray(array)->totalsize = totalsize; # Total-Size eintragen
    {
      var uintL* dimptr = &TheIarray(array)->dims[0];
      if (flags & bit(arrayflags_dispoffset_bit))
        *dimptr++ = displaced_index_offset; # Displaced-Index-Offset eintragen
      # Dimensionen eintragen:
      {
        var object dims = STACK_7;
        if (listp(dims)) {
          while (consp(dims)) {
            *dimptr++ = posfixnum_to_L(Car(dims)); dims = Cdr(dims);
          }
        } else {
          *dimptr++ = posfixnum_to_L(dims);
        }
      }
      # evtl. Fill-Pointer eintragen:
      if (flags & bit(arrayflags_fillp_bit))
        # fill-pointer war angegeben und /=NIL
        *dimptr++ = fillpointer;
    }
    # Datenvektor eintragen:
    TheIarray(array)->data = STACK_1; # displaced-to-Argument oder neuer Datenvektor
    # array als Wert:
    value1 = array; mv_count=1; skipSTACK(8);
  }

# ============================================================================
#                            ADJUST-ARRAY

# Hilfsfunktion für die Umfüllaufgabe bei ADJUST-ARRAY:
# Füllt den Datenvektor eines Arrays teilweise mit dem Inhalt eines anderen
# Datenvektors, und zwar so, dass die Elemente zu Indextupeln, die für beide
# Arrays gültig sind, übereinstimmen.
# reshape(newvec,newdims,oldvec,olddims,offset,rank,eltype);
# > newvec: simpler Vektor, in den zu füllen ist.
# > newdims: Dimension(en) des Arrays,
#            in dem newvec Datenvektor ist (mit Offset 0).
# > oldvec: simpler Vektor, aus dem zu füllen ist.
# > olddims: Pointer auf die Dimensionen des Arrays,
#            in dem oldvec Datenvektor ist (mit Offset offset).
# > rank: Dimensionszahl von newdims = Dimensionenzahl von olddims.
# > eltype: Elementtyp von newvec = Elementtyp von oldvec.
  local void reshape (object newvec, object newdims, object oldvec, const uintL* olddims, uintL offset, uintL rank, uintB eltype);
  # Methode: pseudo-rekursiv, mit Pseudo-Stack, der unterhalb von STACK liegt.
  typedef struct {
    uintL olddim; # Dimension aus olddims
    uintL newdim; # Dimension aus newdims
    uintL mindim; # minimale dieser Dimensionen
    uintL subscript; # Subscript, läuft von 0 bis mindim-1
    uintL oldindex; # Row-Major-Index in oldvec
    uintL newindex; # Row-Major-Index in newvec
    uintL olddelta; # Increment von oldindex bei subscript++
    uintL newdelta; # Increment von newindex bei subscript++
  } reshape_data;
  local void reshape(newvec,newdims,oldvec,olddims,offset,rank,eltype)
    var object newvec;
    var object newdims;
    var object oldvec;
    var const uintL* olddims;
    var uintL offset;
    var uintL rank;
    var uintB eltype;
    {
      # Platz für den Pseudo-Stack reservieren:
      get_space_on_STACK(rank*sizeof(reshape_data));
      # Startpunkt:
      var reshape_data* reshape_stack = &STACKblock_(reshape_data,-1);
      # Pseudo-Stack füllen:
      if (!(rank==0)) {
        var reshape_data* ptr;
        var uintC count;
        # jeweils newdim einfüllen:
        ptr = reshape_stack;
        if (consp(newdims)) {
          dotimespC(count,rank, {
            ptr->newdim = posfixnum_to_L(Car(newdims)); newdims = Cdr(newdims);
            ptr = ptr STACKop -1;
          });
        } else {
          ptr->newdim = posfixnum_to_L(newdims);
        }
        # jeweils olddim und mindim einfüllen:
        ptr = reshape_stack;
        dotimespC(count,rank, {
          var uintL olddim;
          var uintL newdim;
          olddim = ptr->olddim = *olddims++;
          newdim = ptr->newdim;
          ptr->mindim = (olddim<newdim ? olddim : newdim);
          ptr = ptr STACKop -1;
        });
        # jeweils olddelta und newdelta einfüllen:
        {
          var uintL olddelta = 1;
          var uintL newdelta = 1;
          dotimespC(count,rank, {
            ptr = ptr STACKop 1;
            ptr->olddelta = olddelta;
            olddelta = mulu32_unchecked(olddelta,ptr->olddim);
            ptr->newdelta = newdelta;
            newdelta = mulu32_unchecked(newdelta,ptr->newdim);
          });
        }
      }
      # Los geht's mit der Pseudo-Rekursion:
      var reshape_data* ptr = reshape_stack;
      var uintL oldindex = offset; # Row-Major-Index in oldvec
      var uintL newindex = 0; # Row-Major-Index in newvec
      var uintL depth = rank;
     entry: # Rekursionseinstieg
      if (depth==0) {
        # Element kopieren:
        # (setf (aref newvec newindex) (aref oldvec oldindex))
        # so kopieren, dass keine GC ausgelöst werden kann:
        if (eltype == Atype_32Bit) {
          ((uint32*)&TheSbvector(newvec)->data[0])[newindex]
            = ((uint32*)&TheSbvector(oldvec)->data[0])[oldindex];
        } else {
          storagevector_store(newvec,newindex,storagevector_aref(oldvec,oldindex));
        }
      } elif (depth==1) {
        # Optimierung: eine ganze Reihe von Elementen kopieren
        # (beachte: ptr->olddelta = ptr->newdelta = 1).
        if (ptr->mindim > 0)
          elt_copy(oldvec,oldindex,newvec,newindex,ptr->mindim);
      } else {
        # Schleife über alle gemeinsamen Indizes:
        ptr->oldindex = oldindex; ptr->newindex = newindex;
        if (ptr->mindim > 0) {
          depth--;
          dotimespL(ptr->subscript,ptr->mindim, {
            oldindex = ptr->oldindex; newindex = ptr->newindex;
            ptr = ptr STACKop -1;
            goto entry;
           reentry:
            ptr = ptr STACKop 1;
            ptr->oldindex += ptr->olddelta;
            ptr->newindex += ptr->newdelta;
          });
          depth++;
        }
      }
      # Rekursionsaustritt:
      if (depth<rank)
        goto reentry;
    }

LISPFUN(adjust_array,2,0,norest,key,6,\
        (kw(element_type),kw(initial_element),\
         kw(initial_contents),kw(fill_pointer),\
         kw(displaced_to),kw(displaced_index_offset)) )
# (ADJUST-ARRAY array dimensions :element-type :initial-element
#   :initial-contents :fill-pointer :displaced-to :displaced-index-offset),
#   CLTL S. 297
  {
    # array überprüfen:
    {
      var object array = STACK_7;
      if (!arrayp(array))
        fehler_array(array);
      if (array_simplep(array)
          || ((Iarray_flags(array) & bit(arrayflags_adjustable_bit)) == 0)
         ) {
        # nicht adjustierbarer Array
        #ifndef X3J13_003
          pushSTACK(array);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: array ~ is not adjustable")
                );
        #else
          ??
        #endif
      }
      STACK_7 = STACK_6; STACK_6 = array; # Stack etwas umordnen
    }
    # Stackaufbau:
    #   dims, array, element-type, initial-element, initial-contents,
    #   fill-pointer, displaced-to, displaced-index-offset.
    # Dimensionen überprüfen und Rang und Total-Size berechnen:
    var uintL totalsize;
    var uintL rank = test_dims(&totalsize);
    # Rang überprüfen, muss = (array-rank array) sein:
    {
      var uintL oldrank = (uintL)Iarray_rank(STACK_6);
      if (!(rank==oldrank)) {
        pushSTACK(STACK_7); # dims
        pushSTACK(STACK_(6+1)); # array
        pushSTACK(fixnum(oldrank));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: rank ~ of array ~ cannot be altered: ~")
              );
      }
    }
    # element-type in einen Code umwandeln und überprüfen:
    var uintB eltype;
    if (!(eq(STACK_5,unbound))) {
      eltype = eltype_code(STACK_5);
      # mit dem Elementtyp des Array-Arguments vergleichen:
      if (!(eltype == (Iarray_flags(STACK_6) & arrayflags_atype_mask))) {
        pushSTACK(STACK_6); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(array)); pushSTACK(STACK_(5+2)); pushSTACK(listof(2)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(STACK_(5+2)); # element-type
        pushSTACK(STACK_(6+3)); # array
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: array ~ does not have element-type ~")
              );
      }
    } else {
      # Defaultwert ist der Elementtyp des Array-Arguments.
      eltype = (Iarray_flags(STACK_6) & arrayflags_atype_mask);
      STACK_5 = array_element_type(STACK_6);
    }
    test_otherkeys(); # einiges überprüfen
    var uintB flags = Iarray_flags(STACK_6);
    # Die Flags enthalten genau eltype als Atype und
    # arrayflags_adjustable_bit und daher auch
    # arrayflags_dispoffset_bit und vielleicht auch arrayflags_fillp_bit
    # (diese werden nicht verändert) und vielleicht auch
    # arrayflags_displaced_bit (dieses kann geändert werden).
    var uintL displaced_index_offset;
    var uintL fillpointer;
    # Falls nicht displaced, Datenvektor bilden und evtl. füllen:
    if (nullp(STACK_1)) { # displaced-to nicht angegeben?
      # Datenvektor bilden:
      var object datenvektor = make_storagevector(totalsize,eltype);
      if (!eq(STACK_3,unbound)) { # und falls initial-contents angegeben:
        # mit dem initial-contents-Argument füllen:
        datenvektor = initial_contents(datenvektor,STACK_7,rank,STACK_3);
      } else {
        # mit dem ursprünglichen Inhalt von array füllen:
        var object oldarray = STACK_6; # array
        var uintL oldoffset = 0;
        var object oldvec = iarray_displace_check(oldarray,TheIarray(oldarray)->totalsize,&oldoffset);
        # oldvec ist der Datenvektor, mit Displaced-Offset oldoffset.
        var uintL* olddimptr = &TheIarray(oldarray)->dims[1];
        # Ab olddimptr kommen die alten Dimensionen von array
        # (beachte: Da arrayflags_adjustable_bit gesetzt ist, ist auch
        # arrayflags_dispoffset_bit gesetzt, also ist
        # TheIarray(array)->data[0] für den Displaced-Offset reserviert.)
        reshape(datenvektor,STACK_7,oldvec,olddimptr,oldoffset,rank,eltype);
      }
      STACK_1 = datenvektor; # datenvektor als "displaced-to" ablegen
      displaced_index_offset = 0; # mit Displacement 0
      flags &= ~bit(arrayflags_displaced_bit); # und ohne Displacement-Bit in den Flags
    } else {
      # displaced-to angegeben.
      displaced_index_offset = test_displaced(eltype,totalsize);
      # Test auf entstehenden Zyklus:
      {
        var object array = STACK_6; # Array, der displaced werden soll
        var object to_array = STACK_1; # Array, auf den displaced werden soll
        # Teste, ob array in der Datenvektorenkette von to_array vorkommt:
        loop {
          # Falls array = to_array, ist ein Zyklus da.
          if (eq(array,to_array)) {
            pushSTACK(array);
            pushSTACK(TheSubr(subr_self)->name);
            fehler(error,
                   GETTEXT("~: cannot displace array ~ to itself")
                  );
          }
          # Falls to_array simple ist (also nicht displaced),
          # liegt kein Zyklus vor.
          if (simplep(to_array))
            break;
          # Displaced-Kette von to_array weiterverfolgen:
          to_array = TheIarray(to_array)->data;
        }
      }
      # Flags enthalten das Displacement-Bit:
      flags |= bit(arrayflags_displaced_bit);
    }
    # Flags sind nun korrekt.
    # Modifiziere den gegebenen Array.
    if (!nullp(STACK_2)) { # fill-pointer angegeben?
      # array muss Fill-Pointer haben:
      if (!(Iarray_flags(STACK_6) & bit(arrayflags_fillp_bit))) {
        pushSTACK(STACK_6); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_vector_with_fill_pointer)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(STACK_(6+2));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: array ~ has no fill-pointer")
              );
      }
      fillpointer = test_fillpointer(totalsize); # Fill-Pointer-Wert
    } else {
      # Hat array einen Fill-Pointer, so muss er <= neue Total-Size sein:
      var object array = STACK_6;
      if (Iarray_flags(array) & bit(arrayflags_fillp_bit))
        if (!(TheIarray(array)->dims[2] <= totalsize)) {
               # dims[0] = displaced-offset, dims[1] = Länge, dims[2] = Fill-Pointer
          pushSTACK(fixnum(totalsize));
          pushSTACK(fixnum(TheIarray(array)->dims[2]));
          pushSTACK(array);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: the fill-pointer of array ~ is ~, greater than ~")
                );
        }
    }
    # Array modifizieren:
    {
      var object array = STACK_6;
      set_break_sem_1(); # Unterbrechungen verbieten
      iarray_flags_replace(TheIarray(array),flags); # neue Flags eintragen
      TheIarray(array)->totalsize = totalsize; # neue Total-Size eintragen
      {
        var uintL* dimptr = &TheIarray(array)->dims[0];
        *dimptr++ = displaced_index_offset; # Displaced-Index-Offset eintragen
        # neue Dimensionen eintragen:
        {
          var object dims = STACK_7;
          if (listp(dims)) {
            while (consp(dims)) {
              *dimptr++ = posfixnum_to_L(Car(dims)); dims = Cdr(dims);
            }
          } else {
            *dimptr++ = posfixnum_to_L(dims);
          }
        }
        # evtl. Fill-Pointer eintragen bzw. korrigieren:
        if (flags & bit(arrayflags_fillp_bit)) # Array mit Fill-Pointer?
          if (!nullp(STACK_2)) # und fill-pointer angegeben?
            # fill-pointer war angegeben und /=NIL
            *dimptr = fillpointer;
      }
      # Datenvektor eintragen:
      TheIarray(array)->data = STACK_1; # displaced-to-Argument oder neuer Datenvektor
      clr_break_sem_1(); # Unterbrechungen wieder zulassen
      # array als Wert:
      value1 = array; mv_count=1; skipSTACK(8);
    }
  }

# ============================================================================
#                       Arrays as sequences

# Funktionen, die Vektoren zu Sequences machen:

LISPFUNN(vector_init,1)
# #'(lambda (seq) 0)
  {
    skipSTACK(1);
    value1 = Fixnum_0; mv_count=1; # 0 als Wert
  }

LISPFUNN(vector_upd,2)
# #'(lambda (seq pointer) (1+ pointer))
  {
    if (posfixnump(STACK_0)) {
      var object newpointer = fixnum_inc(STACK_0,1); # Fixnum >=0 um 1 erhöhen
      if (posfixnump(newpointer)) {
        # ist ein Fixnum >=0 geblieben
        skipSTACK(2);
        value1 = newpointer; mv_count=1; # newpointer als Wert
        return;
      }
    }
    # Pointer ist vor oder nach dem Erhöhen kein Fixnum >=0
    funcall(L(einsplus),1); # (1+ pointer) als Wert
    skipSTACK(1);
  }

LISPFUNN(vector_endtest,2)
# #'(lambda (seq pointer) (= pointer (vector-length seq)))
  {
    var object seq = STACK_1;
    if (!vectorp(seq))
      fehler_vector(seq);
    if (eq(fixnum(vector_length(seq)),STACK_0)) {
      value1 = T; mv_count=1; skipSTACK(2); # 1 Wert T
    } else {
      value1 = NIL; mv_count=1; skipSTACK(2); # 1 Wert NIL
    }
  }

LISPFUNN(vector_fe_init,1)
# #'(lambda (seq) (1- (vector-length seq)))
  {
    var object seq = popSTACK();
    if (!vectorp(seq))
      fehler_vector(seq);
    var uintL len = vector_length(seq);
    # len = (vector-length seq).
    # Als Fixnum, und um 1 erniedrigen:
    value1 = (len==0 ? Fixnum_minus1 : fixnum(len-1));
    mv_count=1;
  }

LISPFUNN(vector_fe_upd,2)
# #'(lambda (seq pointer) (1- pointer))
  {
    if (posfixnump(STACK_0)) {
      var object pointer = popSTACK();
      value1 = (eq(pointer,Fixnum_0)
                ? Fixnum_minus1
                : fixnum_inc(pointer,-1) # Fixnum >0 um 1 erniedrigen
               );
      mv_count=1;
    } else {
      # Pointer ist vor oder nach dem Erniedrigen kein Fixnum >=0
      funcall(L(einsminus),1); # (1- pointer) als Wert
    }
    skipSTACK(1);
  }

LISPFUNN(vector_fe_endtest,2)
# #'(lambda (seq pointer) (minusp pointer))
  {
    value1 = (positivep(STACK_0) ? NIL : T); # Vorzeichen von pointer abfragen
    mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(vector_length,1)
  {
    var object seq = popSTACK();
    if (!vectorp(seq))
      fehler_vector(seq);
    value1 = fixnum(vector_length(seq)); mv_count=1;
  }

LISPFUNN(vector_init_start,2)
# #'(lambda (seq index)
#     (if (<= 0 index (vector-length seq))
#       index
#       (error "Unzulässiger :START - Index : ~S" index)
#   ) )
  {
    var object seq = STACK_1;
    if (!vectorp(seq))
      fehler_vector(seq);
    var uintL len = vector_length(seq);
    # index sollte ein Fixnum zwischen 0 und len (inclusive) sein:
    if (posfixnump(STACK_0) && (posfixnum_to_L(STACK_0)<=len)) {
      value1 = STACK_0; mv_count=1; skipSTACK(2); # index als Wert
    } else {
      # Stackaufbau: seq, index.
      pushSTACK(STACK_0); # Wert für Slot DATUM von TYPE-ERROR
      {
        var object tmp;
        pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(len));
        tmp = listof(3); pushSTACK(tmp); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      }
      pushSTACK(STACK_3); # seq
      pushSTACK(STACK_3); # index
      fehler(type_error,
             GETTEXT("Illegal START index ~ for ~")
            );
    }
  }

LISPFUNN(vector_fe_init_end,2)
# #'(lambda (seq index)
#     (if (<= 0 index (vector-length seq))
#       (1- index)
#       (error "Unzulässiger :END - Index : ~S" index)
#   ) )
  {
    var object seq = STACK_1;
    if (!vectorp(seq))
      fehler_vector(seq);
    var uintL len = vector_length(seq);
    # index sollte ein Fixnum zwischen 0 und len (inclusive) sein:
    if (posfixnump(STACK_0) && (posfixnum_to_L(STACK_0)<=len)) {
      var object index = STACK_0;
      skipSTACK(2);
      value1 = (eq(index,Fixnum_0)
                ? Fixnum_minus1
                : fixnum_inc(index,-1) # Fixnum >0 um 1 erniedrigen
               );
      mv_count=1;
    } else {
      # Stackaufbau: seq, index.
      pushSTACK(STACK_0); # Wert für Slot DATUM von TYPE-ERROR
      {
        var object tmp;
        pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(len));
        tmp = listof(3); pushSTACK(tmp); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      }
      pushSTACK(STACK_3); # seq
      pushSTACK(STACK_3); # index
      fehler(type_error,
             GETTEXT("Illegal END index ~ for ~")
            );
    }
  }

LISPFUNN(make_bit_vector,1)
# (SYS::MAKE-BIT-VECTOR size) liefert einen Bit-Vector mit size Bits.
  {
    if (!posfixnump(STACK_0)) {
      # STACK_0 = size, Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_posfixnum)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(STACK_1); # size
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: invalid bit-vector length ~")
            );
    }
    var uintL size = posfixnum_to_L(popSTACK()); # Länge
    value1 = allocate_bit_vector(Atype_Bit,size); # euen Bit-Vektor beschaffen
    mv_count=1;
  }

