# Determination of the object size (in bytes) of the various heap objects.

# ------------------------------ Specification ---------------------------------

#ifdef TYPECODES

# Returns the typecode of the varobject at a given address.
# typecode_at(addr)

# Because the result of typecode_at may contain symbol flags, any switch
# statement on such a result must contain 'case_symbolwithflags:' instead of
# 'case_symbol:'.

#endif

# Computes the size (in bytes, including header and alignment) of the
# varobject starting at addr. The result is a multiple of varobject_alignment.
#  var uintL heapnr = ...;   [only needed if SPVW_PURE]
#  var_prepare_objsize;      [declaration of some variable, depends on heapnr]
#  objsize(addr)

# Returns the size (in bytes, including header and alignment) of an object.
# varobject_bytelength(obj)
# > obj: heap object of various length
# < result: number of occupied bytes
  global uintL varobject_bytelength (object obj);

# ------------------------------ Implementation --------------------------------

#ifdef TYPECODES

  # Varobjects contain in the first word a pointer to itself, except during GC.
  # (During GC it's a pointer to the new location, but with the same typecode.)
  #define typecode_at(addr)  mtypecode(((Varobject)(addr))->GCself)
  # or (equivalently):
  # define typecode_at(addr)  (((((Varobject)(addr))->header_flags)>>(oint_type_shift%8))&tint_type_mask)

  #define case_symbolwithflags  \
    case symbol_type:                                        \
    case symbol_type|bit(constant_bit_t):                    \
    case symbol_type|bit(special_bit_t):                     \
    case symbol_type|bit(special_bit_t)|bit(constant_bit_t)

#endif

  # Varobject_aligned_size(HS,ES,C) liefert die Länge eines Objekts variabler
  # Länge mit HS=Header-Size, ES=Element-Size, C=Element-Count.
  # Varobject_aligned_size(HS,ES,C) = round_up(HS+ES*C,varobject_alignment) .
    #define Varobject_aligned_size(HS,ES,C)  \
      ((ES % varobject_alignment) == 0               \
       ? # ES ist durch varobject_alignment teilbar  \
         round_up(HS,varobject_alignment) + (ES)*(C) \
       : round_up((HS)+(ES)*(C),varobject_alignment) \
      )

  # Länge eines Objekts, je nach Typ:
    #ifdef TYPECODES
    #define size_symbol()  # Symbol \
      round_up( sizeof(symbol_), varobject_alignment)
    #endif
    #define size_sbvector(length)  # simple-bit-vector \
      ( ceiling( (uintL)(length) + 8*offsetofa(sbvector_,data), 8*varobject_alignment ) \
        * varobject_alignment                                                           \
      )
    #define size_sstring(length)  # normal-simple-string \
      Varobject_aligned_size(offsetofa(sstring_,data),sizeof(chart),(uintL)(length))
    #ifdef HAVE_SMALL_SSTRING
    #define size_small_sstring(length)  # small-simple-string \
      Varobject_aligned_size(offsetofa(small_sstring_,data),sizeof(scint),(uintL)(length))
    #endif
    #define size_svector(length)  # simple-vector \
      Varobject_aligned_size(offsetofa(svector_,data),sizeof(object),(uintL)(length))
    #define size_iarray(size)  # Nicht-simpler Array, mit \
      # size = Dimensionszahl + (1 falls Fill-Pointer) + (1 falls Displaced-Offset) \
      Varobject_aligned_size(offsetofa(iarray_,dims),sizeof(uintL),(uintL)(size))
    #define size_srecord(length)  # Simple-Record \
      Varobject_aligned_size(offsetofa(record_,recdata),sizeof(object),(uintL)(length))
    #define size_xrecord(length,xlength)  # Extended-Record \
      Varobject_aligned_size(offsetofa(record_,recdata),sizeof(uintB),(sizeof(object)/sizeof(uintB))*(uintL)(length)+(uintL)(xlength))
    #define size_bignum(length)  # Bignum \
      Varobject_aligned_size(offsetofa(bignum_,data),sizeof(uintD),(uintL)(length))
    #ifdef TYPECODES
    #ifndef WIDE
    #define size_ffloat()  # Single-Float \
      round_up( sizeof(ffloat_), varobject_alignment)
    #endif
    #define size_dfloat()  # Double-Float \
      round_up( sizeof(dfloat_), varobject_alignment)
    #else
    #define size_ffloat()  # Single-Float \
      size_xrecord(0,sizeof(ffloat))
    #define size_dfloat()  # Double-Float \
      size_xrecord(0,sizeof(dfloat))
    #endif
    #define size_lfloat(length)  # Long-Float \
      Varobject_aligned_size(offsetofa(lfloat_,data),sizeof(uintD),(uintL)(length))

#ifdef SPVW_MIXED

  local uintL objsize (addr)
    var void* addr;
    {
      #ifdef TYPECODES
      switch (typecode_at(addr) & ~bit(garcol_bit_t)) # Typ des Objekts
      #else
      switch (record_type((Record)addr))
        { case_Rectype_Sbvector_above;
          case Rectype_Sstring: case Rectype_Imm_Sstring: goto case_sstring;
          case_Rectype_Svector_above;
          case_Rectype_mdarray_above;
          case_Rectype_obvector_above;
          case_Rectype_ostring_above;
          case_Rectype_ovector_above;
          case_Rectype_Bignum_above;
          case_Rectype_Lfloat_above;
          #ifdef HAVE_SMALL_SSTRING
          case Rectype_Imm_SmallSstring:
            return size_small_sstring(sstring_length((SmallSstring)addr));
          #endif
          default: goto case_record;
        }
      switch (0)
      #endif
        {
          #ifdef TYPECODES
          case_symbolwithflags: # Symbol
            return size_symbol();
          #endif
          case_sbvector: # simple-bit-vector
            return size_sbvector(sbvector_length((Sbvector)addr));
          case_sstring: # normal-simple-string
            return size_sstring(sstring_length((Sstring)addr));
          case_svector: # simple-vector
            return size_svector(svector_length((Svector)addr));
          case_mdarray: case_obvector: case_ostring: case_ovector:
            # Nicht-simpler Array:
            { var uintL size;
              size = (uintL)iarray_rank((Iarray)addr);
              if (iarray_flags((Iarray)addr) & bit(arrayflags_fillp_bit)) { size += 1; }
              if (iarray_flags((Iarray)addr) & bit(arrayflags_dispoffset_bit)) { size += 1; }
              # size = Dimensionszahl + (1 falls Fill-Pointer) + (1 falls Displaced-Offset)
              return size_iarray(size);
            }
          case_record: # Record
            if (record_type((Record)addr) < rectype_limit)
              return size_srecord(srecord_length((Srecord)addr));
              else
              return size_xrecord(xrecord_length((Xrecord)addr),xrecord_xlength((Xrecord)addr));
          case_bignum: # Bignum
            return size_bignum(bignum_length((Bignum)addr));
          #ifdef TYPECODES
          #ifndef WIDE
          case_ffloat: # Single-Float
            return size_ffloat();
          #endif
          case_dfloat: # Double-Float
            return size_dfloat();
          #endif
          case_lfloat: # Long-Float
            return size_lfloat(lfloat_length((Lfloat)addr));
          #ifdef TYPECODES
          case_machine:
          #ifndef SIXBIT_TYPECODES
          case_char:
          case_subr:
          case_system:
          #endif
          case_fixnum:
          case_sfloat:
          #ifdef WIDE
          case_ffloat:
          #endif
            # Das sind direkte Objekte, keine Pointer.
          #endif
          default:
            # Das sind keine Objekte variabler Länge.
            /*NOTREACHED*/ abort();
    }   }

  #define var_prepare_objsize

#endif # SPVW_MIXED

  # spezielle Funktionen für jeden Typ:

  inline local uintL objsize_iarray (addr) # nicht-simpler Array
    var void* addr;
    { var uintL size;
      size = (uintL)iarray_rank((Iarray)addr);
      if (iarray_flags((Iarray)addr) & bit(arrayflags_fillp_bit)) { size += 1; }
      if (iarray_flags((Iarray)addr) & bit(arrayflags_dispoffset_bit)) { size += 1; }
      # size = Dimensionszahl + (1 falls Fill-Pointer) + (1 falls Displaced-Offset)
      return size_iarray(size);
    }

#ifdef SPVW_PURE

  inline local uintL objsize_symbol (addr) # Symbol
    var void* addr;
    { return size_symbol(); }
  inline local uintL objsize_sbvector (addr) # simple-bit-vector
    var void* addr;
    { return size_sbvector(sbvector_length((Sbvector)addr)); }
  inline local uintL objsize_sstring (addr) # simple-string
    var void* addr;
    { return size_sstring(sstring_length((Sstring)addr)); }
  inline local uintL objsize_svector (addr) # simple-vector
    var void* addr;
    { return size_svector(svector_length((Svector)addr)); }
  inline local uintL objsize_record (addr) # Record
    var void* addr;
    { if (record_type((Record)addr) < rectype_limit)
        return size_srecord(srecord_length((Srecord)addr));
        else
        return size_xrecord(xrecord_length((Xrecord)addr),xrecord_xlength((Xrecord)addr));
    }
  inline local uintL objsize_bignum (addr) # Bignum
    var void* addr;
    { return size_bignum(bignum_length((Bignum)addr)); }
  #ifndef WIDE
  inline local uintL objsize_ffloat (addr) # Single-Float
    var void* addr;
    { return size_ffloat(); }
  #endif
  inline local uintL objsize_dfloat (addr) # Double-Float
    var void* addr;
    { return size_dfloat(); }
  inline local uintL objsize_lfloat (addr) # Long-Float
    var void* addr;
    { return size_lfloat(lfloat_length((Lfloat)addr)); }

  # Tabelle von Funktionen:
  typedef uintL (*objsize_func_t) (void* addr);
  local objsize_func_t objsize_table[heapcount];

  local void init_objsize_table (void);
  local void init_objsize_table()
    { var uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++)
        { switch (heapnr)
            { case_symbol:
                objsize_table[heapnr] = &objsize_symbol; break;
              case_sbvector:
                objsize_table[heapnr] = &objsize_sbvector; break;
              case_sstring:
                objsize_table[heapnr] = &objsize_sstring; break;
              case_svector:
                objsize_table[heapnr] = &objsize_svector; break;
              case_mdarray: case_obvector: case_ostring: case_ovector:
                objsize_table[heapnr] = &objsize_iarray; break;
              case_record:
                objsize_table[heapnr] = &objsize_record; break;
              case_bignum:
                objsize_table[heapnr] = &objsize_bignum; break;
              #ifndef WIDE
              case_ffloat:
                objsize_table[heapnr] = &objsize_ffloat; break;
              #endif
              case_dfloat:
                objsize_table[heapnr] = &objsize_dfloat; break;
              case_lfloat:
                objsize_table[heapnr] = &objsize_lfloat; break;
              case_machine:
              case_char:
              case_subr:
              case_system:
              case_fixnum:
              case_sfloat:
              #ifdef WIDE
              case_ffloat:
              #endif
                # Das sind direkte Objekte, keine Pointer.
              /* case_ratio: */
              /* case_complex: */
              default:
                # Das sind keine Objekte variabler Länge.
                objsize_table[heapnr] = (objsize_func_t)&abort; break;
    }   }   }

  #define var_prepare_objsize  \
    var objsize_func_t _objsize_func = objsize_table[heapnr];
  #define objsize(addr)  (*_objsize_func)(addr)

#endif # SPVW_PURE

  global uintL varobject_bytelength (object obj);
  global uintL varobject_bytelength(obj)
    var object obj;
    {
      #ifdef SPVW_PURE
      var uintL heapnr = typecode(obj);
      #endif
      var_prepare_objsize;
      return objsize(TheVarobject(obj));
    }
