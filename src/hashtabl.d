/*
 * Hash-Tables in CLISP
 * Bruno Haible 1990-2004
 * Sam Steingold 1998-2003
 * German comments translated into English: Stefan Kain 2002-01-29
 */

#include "lispbibl.c"
#include "arilev0.c"            /* for Hashcode-Calculation */
#include "aridecl.c"            /* for Short-Floats */


/* Structure of a Hash-Table:
 Pairs (Key . Value) are stored in a vector,
 which is indexed by (hashcode Key).
 For a running MAPHASH to be uninfluenced by a GC, this
 vector is not reorganized because of GC. But as every (hashcode key) can
 change on each GC, we build in an additional indexing-level:
 (hashcode Key) indexes an index-vector; an index points into the
 key-value-vector there, and the (key . value) is located there.
 In order to save memory, we do not store a cons (key . value)
 in the vector, but we simply store key and value consecutively.
 One might want to resolve collisions [several keys have the same
 (hascode Key)] with lists. Due to the fact that the key-value-vector
 (again because of MAPHASH) should be uninfluenced on GC and GC changes
 the set of collisions, we need an additional index-vector,
 called the next-vector, which lies "in parallel with"
 the key-value-vector and which contains a "list"-structure.
 sketch:
   key --> (hashcode key) as index in index-vector.
   Key1 --> 3, Key2 --> 1, Key4 --> 3.
   index-vector      #( nix {indexkey2} nix {indexkey1,indexkey4} nix ... )
                   = #( nix 1 nix 0 nix ... )
   next-vector       #(     3        nix       leer      nix      leer   )
   key-value-vector  #( key1 val1 key2 val2 leer leer key4 val4 leer leer)
 access to a (Key . Value) - pair works as follows:
   index := (aref Index-Vektor (hashcode Key))
   until index = nix
     if (eql Key (aref KVVektor 2*index)) return (aref KVVektor 2*index+1)
     index := (aref Next-Vektor index) ; take "CDR" of the list
   return notfound.
 If the index-vector is enlarged, all hashcodes and the content of
 index-vector and the content of next-vector have to be recalculated.
 If the next-vector and key-value-vector are enlarged, the remaining
 elements can be filled with "leer" , without having to calculate
 a new hashcode.
 In order to have a fast MAPHASH following a CLRHASH or multiple REMHASH,
 when the table contains much fewer elements than its capacity,
 the entries could be kept "left-aligned" in the key-value-vector, i.e.
 all "leer" go to the right. Thus, MAPHASH only needs to graze over the
 elements count-1,...,1,0 of the key-value-vector. But REMHASH must
 - after it has created a gap - copy the last key-value-pair
 (Nummer count-1) into the gap.
 We treat such cases by possibly shrinking the key-value-vector and
 the next-vector on CLRHASH and REMHASH.
 We keep the "leer"-entries in next-vector in a free-"list", so that PUTHASH
 finds a free entry.
 The lengths of index-vector and next-vector do not depend on each other.
 We choose the ratio of their lengths to be 2:1.
 The hash-table is enlarged, when the free-list is empty, i.e.
 COUNT becomes greater than MAXCOUNT. Thereby, MAXCOUNT and SIZE are
 multiplied by REHASH-SIZE (>1).
 The hash-table is reduced, when COUNT < MINCOUNT. Thereby,
 MAXCOUNT and SIZE are multiplied with 1/REHASH-SIZE (<1) . We choose
 MINCOUNT = MAXCOUNT / REHASH-SIZE^2, so that COUNT can vary
 in both directions by the same amount (on a logarithmic scale)
 after the enlargement of the table.

 data-structure of the hash-table (see LISPBIBL.D):
 recflags codes the type and the state of the hash-table:
   Bit 0 set, when EQ-hashtable
   Bit 1 set, when EQL-hashtable
   Bit 2 set, when EQUAL-hashtable
   Bit 3 set, when EQUALP-hashtable
   When all bits 0-3 are not set, user-defined ht_test
   Bit 4-6 =0
   Bit 7 set, when table must be reorganized after GC
 ht_size                Fixnum>0 = length of the ITABLE
 ht_maxcount            Fixnum>0 = length of the NTABLE
 ht_itable              index-vector of length SIZE, contains indices
 ht_ntable              next-vector of length MAXCOUNT, contains indices
 ht_kvtable             key-value-vector, vector of length 2*MAXCOUNT
 ht_freelist            start-index of the free-list in next-vector
 ht_count               number of entries in the table, Fixnum >=0, <=MAXCOUNT
 ht_rehash_size         growth-rate on reorganization. Float >1.1
 ht_mincount_threshold  ratio MINCOUNT/MAXCOUNT = 1/rehash-size^2
 ht_mincount            Fixnum>=0, lower bound for COUNT
 ht_test                hash-table-test - for define-hash-table-test
 ht_hash                hash function  - for define-hash-table-test
 entry "leer" in key-value-vector is = #<UNBOUND>.
 entry "leer" in next-vector is filled by the free-list.
 entry "nix" in index-vector and in next-vector is = #<UNBOUND>. */
#define leer  unbound
#define nix   unbound

/* Rotates a hashcode x by n bits to the left (0<n<32).
 rotate_left(n,x) */
#define rotate_left(n,x)  (((x) << (n)) | ((x) >> (32-(n))))

#define HT_GOOD_P(ht)                                   \
  (posfixnump(TheHashtable(ht)->ht_size) &&             \
   posfixnump(TheHashtable(ht)->ht_maxcount) &&         \
   posfixnump(TheHashtable(ht)->ht_count) &&            \
   posfixnump(TheHashtable(ht)->ht_mincount) &&         \
   simple_vector_p(TheHashtable(ht)->ht_itable) &&      \
   (vector_length(TheHashtable(ht)->ht_itable) ==       \
    posfixnum_to_L(TheHashtable(ht)->ht_size)) &&       \
   simple_vector_p(TheHashtable(ht)->ht_ntable) &&      \
   (vector_length(TheHashtable(ht)->ht_ntable) ==       \
    posfixnum_to_L(TheHashtable(ht)->ht_maxcount)))

/* mixes two hashcodes.
 one is rotated by 5 bits, then the other one is XOR-ed to it. */
#define misch(x1,x2) (rotate_left(5,x1) ^ (x2))

/* UP: Calculates the EQ-hashcode of an object.
 hashcode1(obj)
 It is valid only until the next GC.
 (eq X Y) implies (= (hashcode1 X) (hashcode1 Y)).
 > obj: an object
 < result: hashcode, a 32-Bit-number */
local uint32 hashcode1 (object obj);
#if (defined(WIDE_HARD) || defined(WIDE_SOFT)) && defined(TYPECODES)
 #define hashcode1(obj)  ((uint32)untype(obj))
#else
 #define hashcode1(obj)  ((uint32)as_oint(obj)) /* address (Bits 23..0) and typeinfo */
#endif

/* UP: Calculates the EQL-hashcode of an object.
 hashcode2(obj)
 It is valid only until the next GC.
 (eql X Y) implies (= (hashcode2 X) (hashcode2 Y)).
 > obj: an object
 < result: hashcode, a 32-Bit-number */
local uint32 hashcode2 (object obj);
/* auxiliary functions for known type:
 Fixnum: fixnum-value */
local uint32 hashcode_fixnum (object obj);
#if 0
local uint32 hashcode_fixnum(object obj) { return hashcode1(obj); }
#else
#define hashcode_fixnum(obj)  hashcode1(obj)
#endif
/* Bignum: length*2 + (MSD*2^16 + LSD) */
local uint32 hashcode_bignum (object obj) {
  var uintL len = (uintL)Bignum_length(obj); /* number of Words */
  return
   #if (intDsize==32)
    misch(TheBignum(obj)->data[0],     /* MSD */
          TheBignum(obj)->data[len-1]) /* and LSD */
   #elif (intDsize==16) || (bn_minlength<4)
    highlow32(TheBignum(obj)->data[0],     /* MSD */
              TheBignum(obj)->data[len-1]) /* and LSD */
   #else  /* (intDsize==8) && (bn_minlength>=4) */
    ( (((uint32)TheBignum(obj)->data[0]) << 24)
      |(((uint32)TheBignum(obj)->data[1]) << 16)
      |(((uint32)TheBignum(obj)->data[2]) << 8)
      |((uint32)TheBignum(obj)->data[len-1]))
   #endif
    + 2*len;                    /* and length*2 */
}
/* Short-Float: internal representation */
local uint32 hashcode_sfloat (object obj);
#if 0
local uint32 hashcode_sfloat(object obj) { return hashcode1(obj); }
#else
#define hashcode_sfloat(obj)  hashcode1(obj)
#endif
/* Single-Float: 32 Bit */
local uint32 hashcode_ffloat (object obj) {
  return ffloat_value(obj);
}
/* Double-Float: leading 32 Bits */
local uint32 hashcode_dfloat (object obj) {
 #ifdef intQsize
  return (uint32)(TheDfloat(obj)->float_value >> 32);
 #else
  return TheDfloat(obj)->float_value.semhi;
 #endif
}
/* Long-Float: mixture of exponent, length, first 32 bits */
extern uint32 hashcode_lfloat (object obj); /* see LFLOAT.D */
/* in general: */
local uint32 hashcode2 (object obj) {
 #ifdef TYPECODES
  if (!numberp(obj)) {          /* a number? */
    /* no -> take EQ-hashcode (for characters, EQL == EQ) : */
    return hashcode1(obj);
  } else {              /* yes -> differentiate according to typecode */
    switch (typecode(obj) & ~(bit(number_bit_t)|bit(sign_bit_t))) {
      case fixnum_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /* Fixnum */
        return hashcode_fixnum(obj);
      case bignum_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /* Bignum */
        return hashcode_bignum(obj);
      case sfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /*Short-Float*/
        return hashcode_sfloat(obj);
      case ffloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /*Single-Float*/
        return hashcode_ffloat(obj);
      case dfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /*Double-Float*/
        return hashcode_dfloat(obj);
      case lfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /* Long-Float */
        return hashcode_lfloat(obj);
      case ratio_type & ~(bit(number_bit_t)|bit(sign_bit_t)): { /* Ratio */
        /* hash both components, mix */
        var uint32 code1 = hashcode2(TheRatio(obj)->rt_num);
        var uint32 code2 = hashcode2(TheRatio(obj)->rt_den);
        return misch(code1,code2);
      }
      case complex_type & ~(bit(number_bit_t)|bit(sign_bit_t)): { /* Complex */
        /* hash both components, mix */
        var uint32 code1 = hashcode2(TheComplex(obj)->c_real);
        var uint32 code2 = hashcode2(TheComplex(obj)->c_imag);
        return misch(code1,code2);
      }
      default: NOTREACHED;
    }
  }
 #else
  if (orecordp(obj))
    switch (Record_type(obj)) {
      case Rectype_Bignum:
        return hashcode_bignum(obj);
      case Rectype_Ffloat:
        return hashcode_ffloat(obj);
      case Rectype_Dfloat:
        return hashcode_dfloat(obj);
      case Rectype_Lfloat:
        return hashcode_lfloat(obj);
      case Rectype_Ratio: {     /* hash both components, mix */
        var uint32 code1 = hashcode2(TheRatio(obj)->rt_num);
        var uint32 code2 = hashcode2(TheRatio(obj)->rt_den);
        return misch(code1,code2);
      }
      case Rectype_Complex: {   /* hash both components, mix */
        var uint32 code1 = hashcode2(TheComplex(obj)->c_real);
        var uint32 code2 = hashcode2(TheComplex(obj)->c_imag);
        return misch(code1,code2);
      }
      default:
        break;
    }
  else if (immediate_number_p(obj)) {
    if (as_oint(obj) & wbit(4))
      return hashcode_sfloat(obj);
    else
      return hashcode_fixnum(obj);
  }
  return hashcode1(obj);
 #endif
}

/* UP: Calculates the EQUAL-hashcode of an object.
 hashcode3(obj)
 It is valid only until the next GC, or the next modification
 of the object.
 (equal X Y) implies (= (hashcode3 X) (hashcode3 Y)).
 > obj: an object
 < result: hashcode, a 32-Bit-number */
local uint32 hashcode3 (object obj);
/* auxiliary functions for known type:
 String -> length, first max. 31 characters, utilize last character */
local uint32 hashcode_string (object obj) {
  var uintL len;
  var uintL offset;
  var object string = unpack_string_ro(obj,&len,&offset);
  var uint32 bish_code = 0x33DAE11FUL + len; /* utilize length */
  if (len > 0) {
    SstringDispatch(string,X, {
      var const cintX* ptr = &((SstringX)TheVarobject(string))->data[offset];
      bish_code ^= (uint32)(ptr[len-1]);        /* add last character */
      var uintC count = (len <= 31 ? len : 31); /* min(len,31) */
      dotimespC(count,count, {
        var uint32 next_code = (uint32)(*ptr++); /* next character */
        bish_code = misch(bish_code,next_code);  /* add */
      });
    });
  }
  return bish_code;
}
/* bit-vector -> length, first 16 bits, utilize last 16 bits */
local uint32 hashcode_bvector (object obj) {
  var uintL len = vector_length(obj); /* length */
  var uintL index = 0;
  var object sbv = array_displace_check(obj,len,&index);
  /* sbv is the data-vector, index is the index into the data-vector. */
  len = len << sbNvector_atype(sbv);
 #if BIG_ENDIAN_P && (varobject_alignment%2 == 0)
  /* On big-endian-machines one can work with with 16 Bit at a time
   (so long as varobject_alignment is divisible by 2 byte): */
  #define bitpack  16
  #define uint_bitpack  uint16
  #define get32bits_at  highlow32_at
 #else
  /* else one can take only 8 bit at a time: */
  #define bitpack  8
  #define uint_bitpack  uint8
  #define get32bits_at(p) \
          (((((((uint32)((p)[0])<<8)|(uint32)((p)[1]))<<8)|(uint32)((p)[2]))<<8)|(uint32)((p)[3]))
 #endif
  var uint_bitpack* ptr =       /* pointer to the first used word */
    (uint_bitpack*)(&TheSbvector(sbv)->data[0]) + floor(index,bitpack);
  var uintL offset = index%bitpack; /* offset within the word */
  if (len <= 32) { /* length <= 32 -> take all bits: */
    if (len == 0) {
      return 0x8FA1D564UL;
    } else { /* 0<len<=32 */
      var uintL need = offset+len; /* need 'need' bits for now */
      /* need < 48 */
      var uint32 akku12 = 0;    /* 48-Bit-Akku, part 1 and 2 */
      var uint32 akku3 = 0;     /* 48-Bit-Akku, part 3 */
     #if (bitpack==16)
      if (need > 0) {
        akku12 = highlow32_0(*ptr++); /* first 16 bits */
        if (need > 16) {
          akku12 |= (uint32)(*ptr++); /* next 16 bits */
          if (need > 32)
            akku3 = (uint32)(*ptr++); /* last 16 bits */
        }
      }
     #endif
     #if (bitpack==8)
      if (need > 0) {
        akku12 = (uint32)(*ptr++)<<24; /* first 8 bits */
        if (need > 8) {
          akku12 |= (uint32)(*ptr++)<<16; /* next 8 bits */
          if (need > 16) {
            akku12 |= (uint32)(*ptr++)<<8; /* next 8 bits */
            if (need > 24) {
              akku12 |= (uint32)(*ptr++); /* next 8 bits */
              if (need > 32) {
                akku3 = (uint32)(*ptr++)<<8; /* next 8 bits */
                if (need > 40)
                  akku3 |= (uint32)(*ptr++); /* last 8 bits */
              }
            }
          }
        }
      }
     #endif
      /* shift 'need' bits in akku12,akku3 by offset bits to the left: */
      akku12 = (akku12 << offset) | (uint32)high16(akku3 << offset);
      /* 32 bits in akku12 finished.
       mask out irrelevant bits: */
      akku12 = akku12 & ~(bit(32-len)-1);
      /* utilize length: */
      return akku12+len;
    }
  } else { /* length > 32 -> take first and last 16 bits: */
    var uint32 akku12 =            /* 32-bit-akku */
      get32bits_at(ptr) << offset; /* contains at least the first 16 bits */
    offset += len;                 /* end-offset of the bitvector */
    ptr += floor(offset,bitpack);  /* points to the last used word */
    offset = offset%bitpack;       /* end-offset within the word */
    var uint32 akku34 =            /* 32-bit-akku */
      get32bits_at(ptr-(16/bitpack)) << offset; /* contains at least the last 16 bits */
    /* reach for the first 16, last 16 bits and utilize length: */
    return highlow32(high16(akku12),high16(akku34)) + len;
  }
  #undef get32bits_at
  #undef uint_bitpack
  #undef bitpack
}
/* EQUALP-hashcode of a pathname-component. */
#ifdef PATHNAME_WIN32
local uint32 hashcode4 (object obj);
#define hashcode_pathcomp(obj)  hashcode4(obj)
#else
#define hashcode_pathcomp(obj)  hashcode3(obj)
#endif
/* atom -> differentiation by type */
local uint32 hashcode3_atom (object obj) {
 #ifdef TYPECODES
  if (symbolp(obj)) {           /* a symbol? */
    return hashcode1(obj);      /* yes -> take EQ-hashcode */
  } else if (numberp(obj)) {    /* a number? */
    return hashcode2(obj);      /* yes -> take EQL-hashcode */
  } else {
    var tint type = typecode(obj) /* typeinfo */
      & ~bit(notsimple_bit_t);    /* if simple or not, is irrelevant */
    if (type >= (sbvector_type & ~bit(notsimple_bit_t)) /* bit/byte-vector ? */
        && type <= (sb32vector_type & ~bit(notsimple_bit_t)))
      return hashcode_bvector(obj); /* look at it component-wise */
    if (type == (sstring_type & ~bit(notsimple_bit_t))) /* string ? */
      return hashcode_string(obj); /* look at it component-wise */
    if (xpathnamep(obj)) { /* -> look at it component-wise: */
      check_SP();
      var uint32 bish_code = 0xB0DD939EUL;
      var const gcv_object_t* ptr = &((Record)ThePathname(obj))->recdata[0];
      var uintC count;
      dotimespC(count,Xrecord_length(obj), {
        var uint32 next_code = hashcode_pathcomp(*ptr++); /* hashcode of the next component */
        bish_code = misch(bish_code,next_code);           /* add */
      });
      return bish_code;
    }
    /* else: take EQ-hashcode (for characters: EQL == EQ) */
    return hashcode1(obj);
  }
 #else
  if (orecordp(obj))
    switch (Record_type(obj)) {
      case_Rectype_number_above;
      case Rectype_Sbvector: case Rectype_bvector:
      case Rectype_Sb2vector: case Rectype_b2vector:
      case Rectype_Sb4vector: case Rectype_b4vector:
      case Rectype_Sb8vector: case Rectype_b8vector:
      case Rectype_Sb16vector: case Rectype_b16vector:
      case Rectype_Sb32vector: case Rectype_b32vector:
        return hashcode_bvector(obj);
      case Rectype_S8string: case Rectype_Imm_S8string:
      case Rectype_S16string: case Rectype_Imm_S16string:
      case Rectype_S32string: case Rectype_Imm_S32string:
      case Rectype_reallocstring: case Rectype_string:
        return hashcode_string(obj);
     #ifdef LOGICAL_PATHNAMES
      case Rectype_Logpathname:
     #endif
      case Rectype_Pathname: { /* pathname -> look at it component-wise: */
        check_SP();
        var uint32 bish_code = 0xB0DD939EUL;
        var gcv_object_t* ptr = &((Record)ThePathname(obj))->recdata[0];
        var uintC count;
        dotimespC(count,Xrecord_length(obj), {
          var uint32 next_code = hashcode_pathcomp(*ptr++); /* hashcode of the next component */
          bish_code = misch(bish_code,next_code);           /* add */
        });
        return bish_code;
      }
      default:
        break;
    }
  else if (immediate_number_p(obj)) {
  case_number: return hashcode2(obj);
  }
  return hashcode1(obj);
 #endif
}
/* cons -> look at content up to depth 4:
 determine the hashcode of the CAR and the hashcode of the CDR at a time
 and combine them shifted. As Shifts fit e.g. 16,7,5,3,
 because {0,16} + {0,7} + {0,5} + {0,3} = {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
 consists of 16 different elements of {0,...,31} .
 object, at cons only up to depth 0 */
local uint32 hashcode3_cons0 (object obj) {
  if (atomp(obj)) {
    return hashcode3_atom(obj);
  } else {                      /* cons -> hashcode := 1 */
    return 1;
  }
}
/* object, at cons only up to depth 1 */
local uint32 hashcode3_cons1 (object obj) {
  if (atomp(obj)) {
    return hashcode3_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3_cons0(Car(obj));
    var uint32 code2 = hashcode3_cons0(Cdr(obj));
    return rotate_left(3,code1) ^ code2;
  }
}
/* object, at cons only up to depth 2 */
local uint32 hashcode3_cons2 (object obj) {
  if (atomp(obj)) {
    return hashcode3_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3_cons1(Car(obj));
    var uint32 code2 = hashcode3_cons1(Cdr(obj));
    return rotate_left(5,code1) ^ code2;
  }
}
/* object, at cons only up to depth 3 */
local uint32 hashcode3_cons3 (object obj) {
  if (atomp(obj)) {
    return hashcode3_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3_cons2(Car(obj));
    var uint32 code2 = hashcode3_cons2(Cdr(obj));
    return rotate_left(7,code1) ^ code2;
  }
}
/* object, at cons only up to depth 4 */
local uint32 hashcode3 (object obj) {
  if (atomp(obj)) {
    return hashcode3_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3_cons3(Car(obj));
    var uint32 code2 = hashcode3_cons3(Cdr(obj));
    return rotate_left(16,code1) ^ code2;
  }
}

/* UP: Calculates the EQUALP-hashcode of an object.
 hashcode4(obj)
 Is is valid onyl until the next GC or the next modification
 of the object.
 (equalp X Y) implies (= (hashcode4 X) (hashcode4 Y)). */
local uint32 hashcode4 (object obj);
/* auxiliary functions for known type:
 character -> case-insensitive. */
#define hashcode4_char(c)  (0xCAAEACEFUL + (uint32)as_cint(up_case(c)))
/* number: mixture of exponent, length, first 32 bit */
extern uint32 hashcode4_real (object obj); /* see REALELEM.D */
extern uint32 hashcode4_uint32 (uint32 x); /* see REALELEM.D */
extern uint32 hashcode4_uint4 [16];        /* see REALELEM.D */
/* vectors: look at them component-wise */
local uint32 hashcode4_vector_T (object dv, uintL index,
                                 uintL count, uint32 bish_code);
local uint32 hashcode4_vector_Char (object dv, uintL index,
                                    uintL count, uint32 bish_code);
local uint32 hashcode4_vector_Bit (object dv, uintL index,
                                   uintL count, uint32 bish_code);
local uint32 hashcode4_vector_2Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code);
local uint32 hashcode4_vector_4Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code);
local uint32 hashcode4_vector_8Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code);
local uint32 hashcode4_vector_16Bit (object dv, uintL index,
                                     uintL count, uint32 bish_code);
local uint32 hashcode4_vector_32Bit (object dv, uintL index,
                                     uintL count, uint32 bish_code);
local uint32 hashcode4_vector (object dv, uintL index,
                               uintL count, uint32 bish_code);
local uint32 hashcode4_vector_T (object dv, uintL index,
                                 uintL count, uint32 bish_code) {
  if (count > 0) {
    check_SP();
    var const gcv_object_t* ptr = &TheSvector(dv)->data[index];
    dotimespL(count,count, {
      var uint32 next_code = hashcode4(*ptr++); /* next component's hashcode */
      bish_code = misch(bish_code,next_code);   /* add */
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_Char (object dv, uintL index,
                                    uintL count, uint32 bish_code) {
  if (count > 0) {
    SstringDispatch(dv,X, {
      var const cintX* ptr = &((SstringX)TheVarobject(dv))->data[index];
      dotimespL(count,count, {
        var uint32 next_code = hashcode4_char(as_chart(*ptr++)); /*next char*/
        bish_code = misch(bish_code,next_code); /* add */
      });
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_Bit (object dv, uintL index,
                                   uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uintB* ptr = &TheSbvector(dv)->data[index/8];
    dotimespL(count,count, {
      var uint32 next_code =
        hashcode4_uint4[(*ptr >> ((~index)%8)) & (bit(1)-1)]; /* next byte */
      bish_code = misch(bish_code,next_code);                 /* add */
      index++;
      ptr += ((index%8)==0);
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_2Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uintB* ptr = &TheSbvector(dv)->data[index/4];
    dotimespL(count,count, {
      var uint32 next_code =
        hashcode4_uint4[(*ptr >> ((~index)%4)) & (bit(2)-1)]; /* next byte */
      bish_code = misch(bish_code,next_code);                 /* add */
      index++;
      ptr += ((index%4)==0);
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_4Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uintB* ptr = &TheSbvector(dv)->data[index/2];
    dotimespL(count,count, {
      var uint32 next_code =
        hashcode4_uint4[(*ptr >> ((~index)%2)) & (bit(4)-1)]; /* next byte */
      bish_code = misch(bish_code,next_code);                 /* add */
      index++;
      ptr += ((index%2)==0);
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_8Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uintB* ptr = &TheSbvector(dv)->data[index];
    dotimespL(count,count, {
      var uint32 next_code = hashcode4_uint32(*ptr++); /* next byte */
      bish_code = misch(bish_code,next_code);          /* add */
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_16Bit (object dv, uintL index,
                                     uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uint16* ptr = &((uint16*)&TheSbvector(dv)->data[0])[index];
    dotimespL(count,count, {
      var uint32 next_code = hashcode4_uint32(*ptr++); /* next byte */
      bish_code = misch(bish_code,next_code);          /* add */
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_32Bit (object dv, uintL index,
                                     uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uint32* ptr = &((uint32*)&TheSbvector(dv)->data[0])[index];
    dotimespL(count,count, {
      var uint32 next_code = hashcode4_uint32(*ptr++); /* next byte */
      bish_code = misch(bish_code,next_code);          /* add */
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector (object dv, uintL index,
                               uintL count, uint32 bish_code) {
  switch (Array_type(dv)) {
    case Array_type_svector:    /* simple-vector */
      return hashcode4_vector_T(dv,index,count,bish_code);
    case Array_type_sbvector:   /* simple-bit-vector */
      return hashcode4_vector_Bit(dv,index,count,bish_code);
    case Array_type_sb2vector:
      return hashcode4_vector_2Bit(dv,index,count,bish_code);
    case Array_type_sb4vector:
      return hashcode4_vector_4Bit(dv,index,count,bish_code);
    case Array_type_sb8vector:
      return hashcode4_vector_8Bit(dv,index,count,bish_code);
    case Array_type_sb16vector:
      return hashcode4_vector_16Bit(dv,index,count,bish_code);
    case Array_type_sb32vector:
      return hashcode4_vector_32Bit(dv,index,count,bish_code);
    case Array_type_sstring:    /* simple-string */
      return hashcode4_vector_Char(dv,index,count,bish_code);
    case Array_type_snilvector: /* (VECTOR NIL) */
      return 0x2116ECD0 + bish_code;
    default: NOTREACHED;
  }
}
/* atom -> fall differentiation by type */
local uint32 hashcode4_atom (object obj) {
 #ifdef TYPECODES
  if (symbolp(obj)) {           /* a symbol? */
    return hashcode1(obj);      /* yes -> take EQ-hashcode */
  } else if (numberp(obj)) {    /* a number? */
    /* yes -> take EQUALP-hashcode */
    if (complexp(obj)) {
      var uint32 code1 = hashcode4_real(TheComplex(obj)->c_real);
      var uint32 code2 = hashcode4_real(TheComplex(obj)->c_imag);
      /* impornt for combining, because of "complex canonicalization":
       if imagpart=0.0, then hashcode = hashcode4_real(realpart). */
      return code1 ^ rotate_left(5,code2);
    } else {
      return hashcode4_real(obj);
    }
  } else
    switch (typecode(obj))
 #else
      if (orecordp(obj))
        goto case_orecord;
      else if (immediate_number_p(obj)) {
      case_real: return hashcode4_real(obj);
      } else if (charp(obj))
        goto case_char;
      else
        return hashcode1(obj);
  switch (0)
 #endif
  {
    case_bvector:               /* bit-vector */
    case_b2vector:              /* 2bit-vector */
    case_b4vector:              /* 4bit-vector */
    case_b8vector:              /* 8bit-vector */
    case_b16vector:             /* 16bit-vector */
    case_b32vector:             /* 32bit-vector */
    case_string:                /* string */
    case_vector: {              /* (VECTOR T), (VECTOR NIL) */
      /* look at it component-wise: */
      var uintL len = vector_length(obj); /* length */
      var uintL index = 0;
      var object dv = array_displace_check(obj,len,&index);
      /* dv is the data-vector, index is the index into the data-vector. */
      var uint32 bish_code = 0x724BD24EUL + len; /* utilize length */
      return hashcode4_vector(dv,index,len,bish_code);
    }
    case_mdarray: {             /* array with rank /=1 */
      /* rank and dimensions, then look at it component-wise: */
      var uint32 bish_code = 0xF1C90A73UL;
      {
        var uintC rank = Iarray_rank(obj);
        if (rank > 0) {
          var uintL* dimptr = &TheIarray(obj)->dims[0];
          if (Iarray_flags(obj) & bit(arrayflags_dispoffset_bit))
            dimptr++;
          dotimespC(rank,rank, {
            var uint32 next_code = (uint32)(*dimptr++);
            bish_code = misch(bish_code,next_code);
          });
        }
      }
      {
        var uintL len = TheIarray(obj)->totalsize;
        var uintL index = 0;
        var object dv = iarray_displace_check(obj,len,&index);
        return hashcode4_vector(dv,index,len,bish_code);
      }
    }
   #ifdef TYPECODES
    _case_structure
    _case_stream
   #endif
    case_orecord:
     switch (Record_type(obj)) {
       case_Rectype_bvector_above;
       case_Rectype_b2vector_above;
       case_Rectype_b4vector_above;
       case_Rectype_b8vector_above;
       case_Rectype_b16vector_above;
       case_Rectype_b32vector_above;
       case_Rectype_string_above;
       case_Rectype_vector_above;
       case_Rectype_mdarray_above;
       case_Rectype_Closure_above;
       case_Rectype_Instance_above;
      #ifndef TYPECODES
       case_Rectype_Symbol_above;
       case Rectype_Ratio:
       case Rectype_Ffloat: case Rectype_Dfloat: case Rectype_Lfloat:
       case Rectype_Bignum:
         goto case_real;
       case Rectype_Complex: {
         var uint32 code1 = hashcode4_real(TheComplex(obj)->c_real);
         var uint32 code2 = hashcode4_real(TheComplex(obj)->c_imag);
         /* important for combining, because of "complex canonicalization":
          if imagpart=0.0, then hashcode = hashcode4_real(realpart). */
         return code1 ^ rotate_left(5,code2);
       }
      #endif
       default: ;
     }
    {                           /* look at flags, type, components: */
      var uintC len = Record_length(obj);
      var uint32 bish_code = 0x03168B8D + (Record_flags(obj) << 24)
        + (Record_type(obj) << 16) + len;
      if (len > 0) {
        check_SP();
        var const gcv_object_t* ptr = &TheRecord(obj)->recdata[0];
        var uintC count;
        dotimespC(count,len, {
          var uint32 next_code = hashcode4(*ptr++); /* next component's hashcode */
          bish_code = misch(bish_code,next_code);   /* add */
        });
      }
      if (Record_type(obj) >= rectype_limit) {
        var uintC xlen = Xrecord_xlength(obj);
        if (xlen > 0) {
          var const uintB* ptr = (uintB*)&TheRecord(obj)->recdata[len];
          dotimespC(xlen,xlen, {
            var uint32 next_code = *ptr++;          /* next byte */
            bish_code = misch(bish_code,next_code); /* add */
          });
        }
      }
      return bish_code;
    }
    case_char:                  /* character */
      return hashcode4_char(char_code(obj));
   #ifndef TYPECODES
    case_symbol:                /* symbol */
   #endif
    case_closure:               /* closure */
    case_instance:              /* instance */
      /* take EQ-hashcode */
      return hashcode1(obj);
      default: NOTREACHED;
  }
}
/* cons -> look at content up to depth 4:
 determine hashcode of the CAR and hashcode of the CDR at a time
 and combine them shifted. As shifts fit e.g. 16,7,5,3,
 because {0,16} + {0,7} + {0,5} + {0,3} =
         {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
 consists of 16 different elements of {0,...,31} .
 object, at cons only up to depth 0 */
local uint32 hashcode4_cons0 (object obj) {
  if (atomp(obj)) {
    return hashcode4_atom(obj);
  } else {                      /* cons -> hashcode := 1 */
    return 1;
  }
}
/* object, at cons only up to depth 1 */
local uint32 hashcode4_cons1 (object obj) {
  if (atomp(obj)) {
    return hashcode4_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode4_cons0(Car(obj));
    var uint32 code2 = hashcode4_cons0(Cdr(obj));
    return rotate_left(3,code1) ^ code2;
  }
}
/* object, at cons only up to depth 2 */
local uint32 hashcode4_cons2 (object obj) {
  if (atomp(obj)) {
    return hashcode4_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode4_cons1(Car(obj));
    var uint32 code2 = hashcode4_cons1(Cdr(obj));
    return rotate_left(5,code1) ^ code2;
  }
}
/* object, at cons only up to depth 3 */
local uint32 hashcode4_cons3 (object obj) {
  if (atomp(obj)) {
    return hashcode4_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode4_cons2(Car(obj));
    var uint32 code2 = hashcode4_cons2(Cdr(obj));
    return rotate_left(7,code1) ^ code2;
  }
}
/* object, at cons only up to depth 4 */
local uint32 hashcode4 (object obj) {
  if (atomp(obj)) {
    return hashcode4_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode4_cons3(Car(obj));
    var uint32 code2 = hashcode4_cons3(Cdr(obj));
    return rotate_left(16,code1) ^ code2;
  }
}

/* hashcode for user-defined ht_test */
local uint32 hashcode5 (object fun, object obj) {
  pushSTACK(obj); funcall(fun,1);
  value1 = check_uint32(value1);
  return I_to_UL(value1);
}

/* UP: Calculates the hashcode of an object with reference to a hashtable.
 hashcode(ht,obj)
 > ht: hash-table
 > obj: object
 < result: index into the index-vector
 can trigger GC - for user-defined ht_test */
local inline uintL hashcode_raw (object ht, object obj) {
  var uintB flags = record_flags(TheHashtable(ht));
  return /* raw hashcode according to the hashtable-type: */
    (flags & bit(0) ? hashcode1(obj) : /* EQ-hashcode */
     flags & bit(1) ? hashcode2(obj) : /* EQL-hashcode */
     flags & bit(2) ? hashcode3(obj) : /* EQUAL-hashcode */
     flags & bit(3) ? hashcode4(obj) : /* EQUALP-hashcode */
     hashcode5(TheHashtable(ht)->ht_hash,obj));
}
local inline uintL hashcode_cook (uint32 code, object size) {
  /* divide raw hashcode CODE by SIZE: */
  var uint32 rest;
  divu_3232_3232(code,posfixnum_to_L(size),(void),rest=);
  return rest;
}
local uintL hashcode (object ht, object obj) {
  var object size = TheHashtable(ht)->ht_size;
  return hashcode_cook(hashcode_raw(ht,obj),size);
}

/* UP: Reorganizes a hash-table, after the hashcodes of the keys
 have been modified by a GC.
 rehash(ht);
 > ht: hash-table
 can trigger GC - for user-defined ht_test */
local object rehash (object ht) {
  /* fill index-vector with "nix" : */
  var object Ivektor = TheHashtable(ht)->ht_itable; /* index-vector */
  {
    var gcv_object_t* ptr = &TheSvector(Ivektor)->data[0];
    var uintL count = posfixnum_to_L(TheHashtable(ht)->ht_size); /* SIZE, >0 */
    dotimespL(count,count, { *ptr++ = nix; } );
  }
  /* build up "list"-structure element-wise: */
  var object Nvektor = TheHashtable(ht)->ht_ntable; /* next-vector */
  var object index = TheHashtable(ht)->ht_maxcount; /* MAXCOUNT */
  var uintL maxcount = posfixnum_to_L(index);
  var gcv_object_t* Nptr = &TheSvector(Nvektor)->data[maxcount];
  var gcv_object_t* KVptr = ht_kvt_data(ht) + 2*maxcount; /* end of kvtable */
  var object freelist = nix;
  var object count = Fixnum_0;
  var bool user_defined_p = (ht_test_code(record_flags(TheHashtable(ht)))==0);
  while (!eq(index,Fixnum_0)) { /* index=0 -> loop finished */
    /* traverse the key-value-vector and the next-vector.
       index = MAXCOUNT,...,0 (Fixnum),
       Nptr = &TheSvector(Nvektor)->data[index],
       KVptr = &TheSvector(kvtable)->data[index],
       freelist = freelist up to now,
       count = pair-counter as fixnum. */
    index = fixnum_inc(index,-1); /* decrement index */
    KVptr -= 2;
    var object key = KVptr[0];  /* next key */
    if (!eq(key,leer)) {                 /* /= "leer" ? */
      if (user_defined_p)
        pushSTACK(ht); /* save */
      var uintL hashindex = hashcode(ht,key); /* its hashcode */
      if (user_defined_p) { /* restore - don't have to restore fixnums! */
        /* this implementation favors built-in ht-tests at the expense
           of the user-defined ones */
        var uintL maxcount1 = posfixnum_to_L(index)+1;
        ht = popSTACK();
        Ivektor = TheHashtable(ht)->ht_itable;
        Nvektor = TheHashtable(ht)->ht_ntable;
        Nptr = &TheSvector(Nvektor)->data[maxcount1];
        KVptr = ht_kvt_data(ht) + 2*maxcount1;
      }
      /* "list", that starts at entry hashindex, in order to extend index:
       copy entry from index-vector to the next-vector
       end replace with index (a pointer to this location) : */
      var gcv_object_t* Iptr = &TheSvector(Ivektor)->data[hashindex];
      *--Nptr = *Iptr;          /* copy entry into the next-vector */
      *Iptr = index;               /* and replace pointer to it */
      count = fixnum_inc(count,1); /* count */
    } else {                 /* lengthen freelist in the next-vector: */
      *--Nptr = freelist; freelist = index;
    }
  }
  TheHashtable(ht)->ht_freelist = freelist; /* save frelist */
  TheHashtable(ht)->ht_count = count; /* save number of pairs for consistency*/
  mark_ht_valid(TheHashtable(ht)); /* hashtable is now completely organized */
  return ht;
}

/* UP: Searches a key in a hash-table.
 hash_lookup(ht,obj,&KVptr,&Nptr,&Iptr)
 > ht: hash-table
 > obj: object
 < if found: result=true,
     KVptr[0], KVptr[1] : key, value in key-value-vector,
     *Nptr : associate entry in next-vector,
     *Iptr : previous index pointing to *Nptr
 < if not found: result=false,
     *Iptr : entry belonging to key in index-vector
             or an arbitrary element of the "list" starting there
 can trigger GC - for user-defined ht_test */
local bool hash_lookup (object ht, object obj, gcv_object_t** KVptr_,
                        gcv_object_t** Nptr_, gcv_object_t** Iptr_) {
  var uintB flags = record_flags(TheHashtable(ht));
  var bool user_defined_p = (ht_test_code(flags)==0);
  var uintL hashindex;
  if (user_defined_p) { /* guard for GC */
    pushSTACK(ht); pushSTACK(obj);
    if (!ht_validp(TheHashtable(ht))) /* hash-table must be reorganized */
      ht = rehash(ht);
    obj = STACK_0; /* rehash could trigger GC */
    hashindex = hashcode(ht,obj); /* calculate hashcode */
    obj = popSTACK(); ht = popSTACK();
  } else { /* no GC possible */
    if (!ht_validp(TheHashtable(ht))) /* hash-table must be reorganized */
      ht = rehash(ht);
    hashindex = hashcode(ht,obj); /* calculate hashcode */
  }
  var gcv_object_t* Nptr =      /* pointer to the current entry */
    &TheSvector(TheHashtable(ht)->ht_itable)->data[hashindex];
  var gcv_object_t* kvt_data = ht_kvt_data(ht);
  var uintL i_n; /* Iptr-Nptr */
  var uintL size = posfixnum_to_L(TheHashtable(ht)->ht_size);
  while (!eq(*Nptr,nix)) { /* track "list" : "list" finished -> not found */
    var int index = posfixnum_to_L(*Nptr); /* next index */
    var gcv_object_t* Iptr = Nptr;
    Nptr =                      /* pointer to entry in next-vector */
      TheSvector(TheHashtable(ht)->ht_ntable)->data + index;
    var gcv_object_t* KVptr = /* pointer to entries in key-value-vector */
      kvt_data + 2*index;
    var object key = KVptr[0];
    if (!boundp(key) || !boundp(KVptr[1])) {
      /* weak HT - obsolete key and value */
      set_break_sem_2();        /* protect from breaks */
      TheHashtable(ht)->ht_freelist = *Iptr;
      *Iptr = *Nptr;                         /* shorten "list" */
      *Nptr = TheHashtable(ht)->ht_freelist; /* lengthen free-list */
      /* decrement COUNT : */
      TheHashtable(ht)->ht_count = fixnum_inc(TheHashtable(ht)->ht_count,-1);
      clr_break_sem_2();        /* allow breaks again */
    }
    /* compare key with obj: */
    if (flags & bit(0) ? eq(key,obj) :  /* compare with EQ */
        flags & bit(1) ? eql(key,obj) :   /* compare with EQL */
        flags & bit(2) ? equal(key,obj) :  /* compare with EQUAL */
        flags & bit(3) ? equalp(key,obj) : /* compare with EQUALP */
        (/* here again we favor built-in HTs over user-defined ones */
         pushSTACK(ht), pushSTACK(obj), i_n = Iptr - Nptr,
         pushSTACK(key),pushSTACK(obj),funcall(TheHashtable(ht)->ht_test,2),
         obj = popSTACK(), ht = popSTACK(), kvt_data = ht_kvt_data(ht),
         Nptr = TheSvector(TheHashtable(ht)->ht_ntable)->data + index,
         KVptr = kvt_data + 2*index, Iptr = Nptr + i_n,
         !nullp(value1))) { /* user-defined ht_test */
      /* object obj found */
      *KVptr_ = KVptr; *Nptr_ = Nptr; *Iptr_ = Iptr; return true;
    }
  }
  /* not found */
  *Iptr_ = Nptr; return false;
}

/* Macro: Insers a key-value-pair into a hash-table.
 hash_store(key,value);
 > object ht: hash-table
 > object freelist: Start of the free-list in next-vector, /= nix
 > key: key
 > value: value
 > gcv_object_t* Iptr: arbitrary element of the "list", that belongs to key */
#define hash_store(key,value)                                           \
  do { var uintL index = posfixnum_to_L(freelist);    /* free index */  \
    var gcv_object_t* Nptr = /* address of the free entry in next-vector */ \
      &TheSvector(TheHashtable(ht)->ht_ntable)->data[index];            \
    /* address of the free entries in key-value-vector: */              \
    var gcv_object_t* KVptr = ht_kvt_data(ht) + 2*index;                \
    set_break_sem_2();                       /* protect from breaks */  \
    /* increment COUNT: */                                              \
    TheHashtable(ht)->ht_count = fixnum_inc(TheHashtable(ht)->ht_count,1); \
    /* shorten free-list: */                                            \
    TheHashtable(ht)->ht_freelist = *Nptr;                              \
    /* save key and value: */                                           \
    *KVptr++ = key; *KVptr++ = value;                                   \
    /* insert free list-element index into the "list"                   \
     (put it after resize to the list-start,                            \
       because Iptr points into the index-vector,                       \
     else put it to the list-end,                                       \
       because hash_lookup was ended with *Iptr=nix): */                \
    *Nptr = *Iptr; *Iptr = freelist;                                    \
    clr_break_sem_2();                        /* allow breaks again */  \
  } while(0)

/* UP: Provides the numbers and vectors for a new hash-table.
 prepare_resize(maxcount,mincount_threshold)
 > maxcount: wished new size MAXCOUNT
 > mincount_threshold: short-float MINCOUNT-THRESHOLD
 > weak: :KEY or :VALUE or :EITHER or :BOTH
 < result: maxcount
 < stack-layout: MAXCOUNT, SIZE, MINCOUNT,
                index-vector, next-vector, key-value-vector.
 decreases STACK by 6
 can trigger GC */
local uintL prepare_resize (object maxcount, object mincount_threshold,
                            object weak) {
 prepare_resize_restart:
  /* check, if maxcount is not a too big fixnum >0 : */
  if (!posfixnump(maxcount))
    goto check_maxcount;
  {
    var uintL maxcountL = posfixnum_to_L(maxcount);
    var uintL sizeL = 2*maxcountL+1;
    /* SIZE odd in order to improve the hash-function! */
    if (!(sizeL <= (uintL)(bitm(oint_data_len)-1)))
      /* sizeL should fit into a fixnum */
      goto check_maxcount;
    /* numbers on the stack: */
    pushSTACK(maxcount);        /* MAXCOUNT */
    pushSTACK(fixnum(sizeL));   /* SIZE */
    /* MINCOUNT := (floor (* maxcount mincount-threshold)) */
    pushSTACK(maxcount); pushSTACK(mincount_threshold); funcall(L(mal),2);
    pushSTACK(value1); funcall(L(floor),1);
    pushSTACK(value1);
    /* stack-layout: MAXCOUNT, SIZE, MINCOUNT.
     allocate new vectors: */
    pushSTACK(allocate_vector(sizeL)); /* supply index-vector */
    pushSTACK(allocate_vector(maxcountL)); /* supply next-vector */
    if (!nullp(weak)) /* supply key-value-vector */
      pushSTACK(allocate_weakkvt(2*maxcountL,weak));
    else pushSTACK(allocate_vector(2*maxcountL));
    /* finished. */
    return maxcountL;
  }
 check_maxcount: /* maxcount no fixnum or too big */
  pushSTACK(weak); pushSTACK(mincount_threshold); /* save */
  pushSTACK(NIL); /* no PLACE */
  pushSTACK(maxcount); /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_hashtable_size)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(maxcount);
  check_value(type_error,GETTEXT("Hash table size ~ too large"));
  maxcount = value1;
  mincount_threshold = popSTACK(); weak = popSTACK(); /* restore */
  goto prepare_resize_restart;
}

/* UP: Enlarges or diminishes a hash-table
 resize(ht,maxcount)
 > ht: hash-table
 > maxcount: wished new size MAXCOUNT
 < result: hash-table, EQ to the old one
 can trigger GC */
local object resize (object ht, object maxcount) {
  pushSTACK(ht);
  var uintL maxcountL =
    prepare_resize(maxcount,TheHashtable(ht)->ht_mincount_threshold,
                   ht_weak(ht));
  /* no GC from now on! */
  var object KVvektor = popSTACK(); /* new key-value-vector */
  var object Nvektor = popSTACK();  /* next-vector */
  var object Ivektor = popSTACK();  /* index-vector */
  var object mincount = popSTACK(); /* MINCOUNT */
  var object size = popSTACK();     /* SIZE */
  maxcount = popSTACK();
  ht = popSTACK();
  /* Fill new key-value-vector:
   Loop over the old key-value-vector and
   copy all key-value-pairs with key /= "leer" :
   For traversing the old key-value-vector: */
  var uintL oldcount = posfixnum_to_L(TheHashtable(ht)->ht_maxcount);
  var gcv_object_t* oldKVptr = ht_kvt_data(ht);
  /* For traversing the new key-value-vector: */
  var uintL count = maxcountL;
  var gcv_object_t* KVptr = kvtable_data(KVvektor);
  /* For counting: */
  var object counter = Fixnum_0;
  dotimesL(oldcount,oldcount, {
    var object nextkey = *oldKVptr++;   /* next key */
    var object nextvalue = *oldKVptr++; /* and value */
    if (!eq(nextkey,leer)) {
      /* take over the entry into the new key-value-vector: */
      if (count==0) {           /* is the new vector already full? */
        /* There is not enough room!! */
        pushSTACK(ht);          /* hash-table */
        fehler(serious_condition,
               GETTEXT("internal error occured while resizing ~"));
      }
      count--;
      *KVptr++ = nextkey; *KVptr++ = nextvalue; /* file in new vector */
      counter = fixnum_inc(counter,1);          /* and count */
    }
  });
  /* Mark 'count' pairs of the new key-value-vector as "leer" : */
  dotimesL(count,count, { *KVptr++ = leer; *KVptr++ = leer; } );
  /* modify hash-table: */
  set_break_sem_2();                 /* protect from breaks */
  mark_ht_invalid(TheHashtable(ht)); /* table must still be reorganized */
  TheHashtable(ht)->ht_size = size;  /* enter new SIZE */
  TheHashtable(ht)->ht_itable = Ivektor;    /* enter new index-vector */
  TheHashtable(ht)->ht_maxcount = maxcount; /* enter new MAXCOUNT */
  TheHashtable(ht)->ht_freelist = nix;      /* dummy as free-list */
  TheHashtable(ht)->ht_ntable = Nvektor;    /* enter new next-vector */
  TheHashtable(ht)->ht_kvtable = KVvektor; /* enter new key-value-vector */
  TheHashtable(ht)->ht_count = counter; /* enter COUNT (for consistency) */
  TheHashtable(ht)->ht_mincount = mincount; /* enter new MINCOUNT */
  clr_break_sem_2();                        /* allow breaks again */
  return ht;
}

/* Macro: Enlarges a hash-table until freelist /= nix
 hash_prepare_store(hash_pos,key_pos);
 > int literal: hash-table position in STACK
 > int literal: key position in STACK
 < object ht: hash-table
 < object freelist: start of the free-list in the next-vector, /= nix
 < gcv_object_t* Iptr: arbitrary element of the "list", that belongs to the key
 for EQ/EQL hashtables the hash code changes after GC,
 so the raw hashcode cannot be cached.
 for EQUAL/EQUALP/user-defined hashtables, raw hashcode caching is good
 (especially for the user-defined tables, where hashcode can trigger GC!)
 can trigger GC */
#define hash_prepare_store(hash_pos,key_pos)                            \
  do { ht = STACK_(hash_pos);                                           \
    freelist = TheHashtable(ht)->ht_freelist;                           \
    if (eq(freelist,nix)) { /* free-list = empty "list" ? */            \
      var uintB flags = record_flags(TheHashtable(ht));                 \
      var uintL hc_raw = 0;                                             \
      var bool cacheable = !(flags & (bit(0)|bit(1))); /* not EQ|EQL */ \
      if (cacheable) hc_raw = hashcode_raw(ht,STACK_(key_pos));         \
     retry: /* hash-table must still be enlarged: */                    \
      /* calculate new maxcount: */                                     \
      pushSTACK(TheHashtable(ht)->ht_maxcount);                         \
      pushSTACK(TheHashtable(ht)->ht_rehash_size); /* REHASH-SIZE (>1) */ \
      funcall(L(mal),2); /* (* maxcount rehash-size), is > maxcount */  \
      pushSTACK(value1);                                                \
      funcall(L(ceiling),1); /* (ceiling ...), integer > maxcount */    \
      ht = resize(STACK_(hash_pos),value1); /* enlarge table */         \
      ht = rehash(ht); /* and reorganize */                             \
      /* newly calculate the address of the entry in the index-vector: */ \
     {var uintL hashindex = cacheable                                   \
         ? hashcode_cook(hc_raw,TheHashtable(ht)->ht_size)              \
         : hashcode(ht,STACK_(key_pos));                                \
      Iptr = &TheSvector(TheHashtable(ht)->ht_itable)->data[hashindex];} \
     freelist = TheHashtable(ht)->ht_freelist;                          \
     if (eq(freelist,nix)) goto retry;                                  \
    }                                                                   \
  } while(0)

/* UP: Deletes the content of a hash-table.
 clrhash(ht);
 > ht: hash-table */
local void clrhash (object ht) {
  set_break_sem_2();            /* protect from breaks */
  {
    var uintL count = posfixnum_to_L(TheHashtable(ht)->ht_maxcount);
    if (count > 0) {
      var gcv_object_t* KVptr = ht_kvt_data(ht);
      dotimespL(count,count, {            /* in each entry */
        *KVptr++ = leer; *KVptr++ = leer; /* deplete key and value */
      });
    }
  }
  TheHashtable(ht)->ht_count = Fixnum_0; /* COUNT := 0 */
  mark_ht_invalid(TheHashtable(ht)); /* reorganize hashtable later */
  clr_break_sem_2();                 /* allow breaks again */
}

/* check the :WEAK argument and return it
 can trigger GC */
local gcv_object_t check_weak (gcv_object_t weak) {
 check_weak_restart:
  if (missingp(weak)) return NIL;
  if (eq(weak,S(Kkey)) || eq(weak,S(Kvalue))
      || eq(weak,S(Keither)) || eq(weak,S(Kboth)))
    return weak;
  /* invalid */
  pushSTACK(NIL); /* no PLACE */
  pushSTACK(weak);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_weak_ht)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(NIL); pushSTACK(S(Kkey)); pushSTACK(S(Kvalue));
  pushSTACK(S(Keither)); pushSTACK(S(Kboth));
  pushSTACK(weak); pushSTACK(TheSubr(subr_self)->name);
  check_value(type_error,GETTEXT("~: argument ~ should be ~, ~, ~, ~ or ~."));
  weak = value1;
  goto check_weak_restart;
}

/* (MAKE-HASH-TABLE [:test] [:size] [:rehash-size] [:rehash-threshold]
                  [:initial-contents] [:weak]), CLTL p. 283 */
LISPFUN(make_hash_table,seclass_read,0,0,norest,key,6,
        (kw(weak),kw(initial_contents),
         kw(test),kw(size),kw(rehash_size),kw(rehash_threshold)) )
{ /* The rehash-threshold correlates in our implementation to the
   ratio MAXCOUNT : SIZE = ca. 1 : 2.
   We ignore the rehash-threshold-argument, as both too big values and
   also too small values could be harmful: 0.99 causes on average
   too long access-times; 0.00001 causes, that SIZE = MAXCOUNT/threshold
   could become a bignum too fast.
   The additional initial-contents-argument is an alist = list of
   (key . value) - pairs, that are used to initialize the table.
   stack-layout:
      weak, initial-contents, test, size, rehash-size, rehash-threshold. */
  var uintB flags;
 check_test_restart: { /* check test-argument: */
    var object test = STACK_3;
    if (!boundp(test))
      flags = bit(1);           /* EQL as Default */
    else if (eq(test,S(eq)) || eq(test,L(eq)))
      flags = bit(0);           /* EQ */
    else if (eq(test,S(eql)) || eq(test,L(eql)))
      flags = bit(1);           /* EQL */
    else if (eq(test,S(equal)) || eq(test,L(equal)))
      flags = bit(2);           /* EQUAL */
    else if (eq(test,S(equalp)) || eq(test,L(equalp)))
      flags = bit(3);           /* EQUALP */
    else if (symbolp(test)) {
      var object ht_test = get(test,S(hash_table_test));
      if (!consp(ht_test)) goto test_error;
      STACK_3 = ht_test;
      flags = 0; /* user-defined ht_test */
    } else if (consp(test)) {
      flags = 0; /* ad hoc (user-defined ht_test) */
    } else { test_error:
      pushSTACK(NIL); /* no PLACE */
      pushSTACK(test); /* TYPE-ERROR slot DATUM */
      pushSTACK(O(type_hashtable_test)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(test); pushSTACK(S(Ktest));
      pushSTACK(S(make_hash_table));
      check_value(type_error,GETTEXT("~: illegal ~ argument ~"));
      STACK_3 = value1;
      goto check_test_restart;
    }
  } /* flags contains the flags for the test. */
 check_size: { /* check size-argument: */
    var object size = STACK_2;
    if (!boundp(size)) {
      STACK_2 = Fixnum_1;       /* 1 as default */
    } else {
      if (!posfixnump(size)) {
        pushSTACK(NIL); /* no PLACE */
        pushSTACK(size); /* TYPE-ERROR slot DATUM */
        pushSTACK(O(type_posfixnum)); /* TYPE-ERROR slot EXPECTED-TYPE */
        pushSTACK(size); pushSTACK(S(Ksize));
        pushSTACK(S(make_hash_table));
        check_value(type_error,GETTEXT("~: ~ argument should be a fixnum >=0, not ~"));
        STACK_2 = value1;
        goto check_size;
      }
      /* size is a fixnum >=0 */
      if (eq(size,Fixnum_0))
        STACK_2 = Fixnum_1; /* turn 0 into 1 */
    }
  } /* size is now a fixnum >0. */
  check_rehash_size: { /* (OR (INTEGER 1 *) (FLOAT (1.0) *)) */
    if (!boundp(STACK_1)) { /* default-rehash-size = 1.5s0 */
      STACK_1 = make_SF(0,SF_exp_mid+1,(bit(SF_mant_len)*3)/2);
    } else {
      if (!floatp(STACK_1)) { /* Float is OK */
        if (!integerp(STACK_1) || R_minusp(STACK_1) || eq(STACK_1,Fixnum_0)) {
          /* else it should be a positive integer */
         bad_rehash_size:
          pushSTACK(NIL); /* no PLACE */
          pushSTACK(STACK_(1+1)); /* TYPE-ERROR slot DATUM */
          pushSTACK(O(type_hashtable_rehash_size)); /* EXPECTED-TYPE */
          pushSTACK(STACK_(1+3)); pushSTACK(S(Krehash_size));
          pushSTACK(S(make_hash_table));
          check_value(type_error,GETTEXT("~: ~ argument should be an integer or a float > 1, not ~"));
          STACK_1 = value1;
          goto check_rehash_size;
        }
        /* As it is senseless to enlarge a table always only by a fixed
           number of elements (results in disastrous efficiency), we set
           rehash-size := min(1 + rehash-size/size , 2.0) . */
        pushSTACK(STACK_1); /* rehash-size */
        pushSTACK(STACK_(2+1)); /* size */
        funcall(L(durch),2); /* (/ rehash-size size) */
        pushSTACK(value1);
        funcall(L(einsplus),1); /* (1+ ...) */
        pushSTACK(value1);
        pushSTACK(make_SF(0,SF_exp_mid+2,bit(SF_mant_len))); /* 2.0s0 */
        funcall(L(min),2); /* (MIN ... 2.0s0) */
        STACK_1 = value1; /* =: rehash-size */
      }
      /* check (> rehash-size 1) : */
      pushSTACK(STACK_1); /* rehash-size */
      pushSTACK(Fixnum_1); /* 1 */
      funcall(L(groesser),2); /* (> rehash-size 1) */
      if (nullp(value1)) goto bad_rehash_size;
      /* convert rehash-size into a short-float: */
      pushSTACK(STACK_1); /* rehash-size */
      pushSTACK(SF_0); /* 0.0s0 */
      funcall(L(float),2); /* (FLOAT rehash-size 0.0s0) = (COERCE rehash-size 'SHORT-FLOAT) */
      /* enforce (>= rehash-size 1.125s0) : */
      pushSTACK(value1);
      pushSTACK(make_SF(0,SF_exp_mid+1,(bit(SF_mant_len)/8)*9)); /* 1.125s0 */
      funcall(L(max),2); /* (max rehash-size 1.125s0) */
      STACK_1 = value1; /* =: rehash-size */
    }
  } /* rehash-size is a short-float >= 1.125 . */
 check_rehash_threshold: { /* check rehash-threshold: should be real in [0;1]*/
    var object rehash_threshold = STACK_0;
    if (boundp(rehash_threshold)) { /* not specified -> OK */
      if_realp(rehash_threshold,{},{
       bad_rehash_threshold:
        pushSTACK(NIL); /* no PLACE */
        pushSTACK(rehash_threshold); /* TYPE-ERROR slot DATUM */
        pushSTACK(O(type_hashtable_rehash_threshold)); /* TYPE-ERROR slot EXPECTED-TYPE */
        pushSTACK(STACK_1); pushSTACK(S(Krehash_threshold));
        pushSTACK(S(make_hash_table));
        check_value(type_error,GETTEXT("~: ~ argument should be a real between 0 and 1, not ~"));
        STACK_0 = value1;
        goto check_rehash_threshold;
      });
      pushSTACK(Fixnum_1);
      pushSTACK(rehash_threshold);
      pushSTACK(Fixnum_0);
      funcall(L(grgleich),3); /* (>= 1 rehash-threshold 0) */
      if (nullp(value1)) goto bad_rehash_threshold;
    }
  }
  { /* If the initial-contents-argument is specified, we set
     size := (max size (length initial-contents)) , so afterwards, when
     the initial-contents are written, the table needs not be enlarged: */
    var object initial_contents = STACK_4;
    if (boundp(initial_contents)) { /* specified ? */
      var uintL initial_length = llength(initial_contents); /* length of the alist */
      if (initial_length > posfixnum_to_L(STACK_2)) /* > size ? */
        STACK_2 = fixnum(initial_length); /* yes -> enlarge size */
    }
  } /* size is a fixnum >0, >= (length initial-contents) . */
  { /* calculate MINCOUNT-THRESHOLD = 1/rehash-size^2 : */
    var object rehash_size = STACK_1;
    pushSTACK(rehash_size);
    pushSTACK(rehash_size);
    funcall(L(mal),2); /* (* rehash-size rehash-size) */
    pushSTACK(value1);
    funcall(L(durch),1); /* (/ ...) */
    STACK_0 = value1;
  }
  /* stack-layout:
      weak, initial-contents, test, size, rehash-size, mincount-threshold
    provide vectors etc., with size as MAXCOUNT: [STACK_5 == weak] */
  STACK_5 = check_weak(STACK_5);
  prepare_resize(STACK_2,STACK_0,STACK_5);
  var object ht = allocate_hash_table(); /* new hash-tabelle */
  /* fill: */
  TheHashtable(ht)->ht_kvtable = popSTACK(); /* key-value-vector */
  TheHashtable(ht)->ht_ntable = popSTACK();  /* next-vector */
  TheHashtable(ht)->ht_itable = popSTACK();  /* index-vector */
  TheHashtable(ht)->ht_mincount = popSTACK(); /* MINCOUNT */
  TheHashtable(ht)->ht_size = popSTACK();     /* SIZE */
  TheHashtable(ht)->ht_maxcount = popSTACK(); /* MAXCOUNT */
  /* stack-layout:
     weak, initial-contents, test, size, rehash-size, mincount-threshold. */
  TheHashtable(ht)->ht_mincount_threshold = popSTACK(); /*MINCOUNT-THRESHOLD*/
  TheHashtable(ht)->ht_rehash_size = popSTACK(); /* REHASH-SIZE */
  TheHashtable(ht)->ht_freelist = nix; /* dummy as free-list */
  if (flags==0) { /* user-defined ht_test */
    STACK_0 = ht;
    var object test = coerce_function(Car(STACK_1)); pushSTACK(test);
    var object hash = coerce_function(Cdr(STACK_2)); ht = STACK_1;
    TheHashtable(ht)->ht_test = popSTACK();
    TheHashtable(ht)->ht_hash = hash;
  }
  record_flags_replace(TheHashtable(ht), flags);
  clrhash(ht);                  /* empty table, COUNT := 0 */
  skipSTACK(2);
  /* stack-layout: weak, initial-contents. */
  {
    pushSTACK(ht);
    while (consp(STACK_1)) { /* if it was specified, so long as it was a cons: */
      var object next = Car(STACK_1); /* alist element */
      if (consp(next)) { /* a cons (Key . Value) ? */
        /* execute (SYSTEM::PUTHASH (car next) hashtable (cdr next)) ,
           whereby the table cannot grow: */
        var gcv_object_t* KVptr;
        var gcv_object_t* Nptr;
        var gcv_object_t* Iptr;
        if (hash_lookup(STACK_0,Car(next),&KVptr,&Nptr,&Iptr)) { /* search */
          /* already found -> was already contained in the alist further
             on the left, and in alists the first association (left)
             shadows all other associations of the same key. */
        } else { /* not found -> make a new entry: */
          var object freelist = /* start of the free-list in the next-vector */
            TheHashtable(ht)->ht_freelist;
          if (eq(freelist,nix)) { /* empty "list" ? */
            pushSTACK(ht); /* hash-table */
            pushSTACK(S(make_hash_table));
            fehler(serious_condition,
                   GETTEXT("~: internal error while building ~"));
          }
          next = Car(STACK_1); /* restore next */
          hash_store(Car(next),Cdr(next)); /* make entry */
        }
      }
      STACK_1 = Cdr(STACK_1); /* pop alist */
    }
    skipSTACK(2); /* drop ht, initial-contents */
  }
  skipSTACK(1); /* drop WEAK */
  VALUES1(ht); /* hash-table as value */
}

/* UP: Searches an object in a hash-table.
 gethash(obj,ht)
 > obj: object, as key
 > ht: hash-table
 < result: if found, belonging value, else nullobj
 can trigger GC - for user-defined ht_test */
global object gethash (object obj, object ht) {
  var gcv_object_t* KVptr;
  var gcv_object_t* Nptr;
  var gcv_object_t* Iptr;
  if (hash_lookup(ht,obj,&KVptr,&Nptr,&Iptr))
    return KVptr[1]; /* found -> value */
  else
    return nullobj;
}

/* error, if an argument is not a hash-table
 check_hashtable(obj);
 > obj: object
 < hashtable
 can trigger GC */
local object check_hashtable (object obj) {
  while (!hash_table_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(hash_table)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(hash_table)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: argument ~ is not a ~"));
    obj = value1;
  }
  return obj;
}

LISPFUN(gethash,seclass_read,2,1,norest,nokey,0,NIL)
{ /* (GETHASH key hashtable [default]), CLTL p. 284 */
  var object ht = check_hashtable(STACK_1); /* hashtable argument */
  var gcv_object_t* KVptr;
  var gcv_object_t* Nptr;
  var gcv_object_t* Iptr;
  /* search key STACK_2 in the hash-table: */
  if (hash_lookup(ht,STACK_2,&KVptr,&Nptr,&Iptr)) { /* -> Value as value: */
    VALUES2(KVptr[1], T); /* and T as the 2nd value */
    skipSTACK(3);
  } else {                    /* not found -> default or NIL as value */
    var object def = popSTACK(); /* default */
    VALUES2(!boundp(def) ? NIL : def,
            NIL); /* NIL as the 2nd value */
    skipSTACK(2);
  }
}

LISPFUNN(puthash,3)
{ /* (SYSTEM::PUTHASH key hashtable value) =
 (SETF (GETHASH key hashtable) value), CLTL p. 284 */
  var object ht = check_hashtable(STACK_1); /* hashtable argument */
  var gcv_object_t* KVptr;
  var gcv_object_t* Nptr;
  var gcv_object_t* Iptr;
  /* search key STACK_2 in the hash-table: */
  if (hash_lookup(ht,STACK_2,&KVptr,&Nptr,&Iptr)) { /* -> replace value: */
    VALUES1(KVptr[1] = popSTACK()); skipSTACK(2);
  } else {                      /* not found -> make new entry: */
    var object freelist;
    hash_prepare_store(1,2); /* ht==STACK_1, obj==STACK_2*/
    hash_store(STACK_2,STACK_0); /* make entry */
    VALUES1(popSTACK()); /* value as value */
    skipSTACK(2);
  }
}

/* UP: Searches a key in a hash-table and returns the last value.
 shifthash(ht,obj,value) == (SHIFTF (GETHASH obj ht) value)
 > ht: hash-table
 > obj: object
 > value: new value
 < result: old value
 can trigger GC */
global object shifthash (object ht, object obj, object value) {
  var gcv_object_t* KVptr;
  var gcv_object_t* Nptr;
  var gcv_object_t* Iptr;
  pushSTACK(ht); pushSTACK(obj); pushSTACK(value); /* save args */
  /* search key obj in the hash-table: */
  if (hash_lookup(ht,obj,&KVptr,&Nptr,&Iptr)) { /* found -> replace value: */
    var object oldvalue = KVptr[1];
    KVptr[1] = STACK_0; skipSTACK(3);
    return oldvalue;
  } else { /* not found -> build new entry: */
    var object freelist;
    hash_prepare_store(2,1);  /* ht==STACK_2, obj==STACK_1 */
    hash_store(STACK_1,STACK_0); /* build entry */
    skipSTACK(3);
    return NIL;                 /* default for the old value is NIL */
  }
}

LISPFUNN(remhash,2)
{ /* (REMHASH key hashtable), CLTL p. 284 */
  var object ht = check_hashtable(STACK_0); /* hashtable argument */
  var object key = STACK_1; /* key-argument */
  var gcv_object_t* KVptr;
  var gcv_object_t* Nptr;
  var gcv_object_t* Iptr;
  /* search key in the hash-table: */
  if (hash_lookup(ht,key,&KVptr,&Nptr,&Iptr)) {
    /* found -> drop from the hash-table: */
    var object index = *Iptr;   /* index in next-vector */
    /* with Nptr  = &TheSvector(TheHashtable(ht)->ht_ntable)->data[index]
     and  KVptr = ht_kvt_data(ht) + [2*index] */
    ht = STACK_0; skipSTACK(2);
    set_break_sem_2();          /* protect from breaks */
    *Iptr = *Nptr;                  /* shorten "list" */
    *KVptr++ = leer; *KVptr = leer; /* empty key and value */
    /* lengthen free-list: */
    *Nptr = TheHashtable(ht)->ht_freelist;
    TheHashtable(ht)->ht_freelist = index;
    /* decrement COUNT : */
    TheHashtable(ht)->ht_count = fixnum_inc(TheHashtable(ht)->ht_count,-1);
    clr_break_sem_2();          /* allow breaks again */
    /* shrink the hash-table for COUNT < MINCOUNT : */
    if (  posfixnum_to_L(TheHashtable(ht)->ht_count)
        < posfixnum_to_L(TheHashtable(ht)->ht_mincount)) {
      /* shrink hash-table:
       maxcount := (max (floor (/ maxcount rehash-size)) 1) */
      pushSTACK(ht);            /* save hashtable */
      pushSTACK(TheHashtable(ht)->ht_maxcount);
      pushSTACK(TheHashtable(ht)->ht_rehash_size); /* REHASH-SIZE (>1) */
      funcall(L(durch),2); /* (/ maxcount rehash-size), is < maxcount */
      pushSTACK(value1);
      funcall(L(floor),1); /* (floor ...), an integer >=0, < maxcount */
      var object maxcount = value1;
      if (eq(maxcount,Fixnum_0))
        maxcount = Fixnum_1;       /* turn 0 into 1 */
      resize(popSTACK(),maxcount); /* shrink table */
    }
    VALUES1(T);
  } else {                      /* not found */
    skipSTACK(2); VALUES1(NIL);
  }
}

LISPFUNN(maphash,2)
{ /* (MAPHASH function hashtable), CLTL p. 285 */
  var object ht = check_hashtable(STACK_0); /* hashtable argument */
  /* traverse the key-value-vector in reverse direction and
   call the function for all key-value-pairs with key /= "leer" : */
  var uintL index = 2*posfixnum_to_L(TheHashtable(ht)->ht_maxcount);
  STACK_0 = TheHashtable(ht)->ht_kvtable; /* key-value-vector */
  /* stack-layout: function, key-value-vector. */
  while (index) {
    index -= 2;
    var gcv_object_t* KVptr = kvtable_data(STACK_0) + index;
    if (!eq(KVptr[0],leer)) {   /* key /= "leer" ? */
      pushSTACK(KVptr[0]);      /* key as the 1st argument */
      pushSTACK(KVptr[1]);      /* value as the 2nd argument */
      funcall(STACK_(1+2),2);   /* (FUNCALL function Key Value) */
    }
  }
  skipSTACK(2);
  VALUES1(NIL);
}

LISPFUNN(clrhash,1)
{ /* (CLRHASH hashtable), CLTL p. 285 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  clrhash(ht);                                 /* empty table */
  /* Shrink the hash-table when MINCOUNT > 0 : */
  if (!eq(TheHashtable(ht)->ht_mincount,Fixnum_0))
    ht = resize(ht,Fixnum_1); /* shrink to MAXCOUNT:=1 , so that MINCOUNT:=0 */
  VALUES1(ht); /* hash-table as value */
}

LISPFUNNR(hash_table_count,1)
{ /* (HASH-TABLE-COUNT hashtable), CLTL p. 285, CLtL2 p. 439 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  if (ht_weak_p(ht) && !ht_validp(TheHashtable(ht)))
    ht = rehash(ht); /* update count after GC */
  VALUES1(TheHashtable(ht)->ht_count); /* fixnum COUNT as value */
}

LISPFUNNR(hash_table_rehash_size,1)
{ /* (HASH-TABLE-REHASH-SIZE hashtable), CLtL2 p. 441, dpANS p. 18-7 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  VALUES1(TheHashtable(ht)->ht_rehash_size); /* short-float REHASH-SIZE */
}

LISPFUNNR(hash_table_rehash_threshold,1)
{ /* (HASH-TABLE-REHASH-THRESHOLD hashtable), CLtL2 p. 441, dpANS p. 18-8 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  /* As MAKE-HASH-TABLE ignores the :REHASH-THRESHOLD argument, the value
   is irrelevant here and arbitrary. */
  VALUES1(make_SF(0,SF_exp_mid+0,(bit(SF_mant_len)/2)*3)); /* 0.75s0 */
}

LISPFUNNR(hash_table_size,1)
{ /* (HASH-TABLE-SIZE hashtable), CLtL2 p. 441, dpANS p. 18-9 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  VALUES1(TheHashtable(ht)->ht_maxcount); /* Fixnum MAXCOUNT */
}

/* return the hash table symbol
 or cons (test . hash) for user-defined ht_test
 can trigger GC - for user-defined ht_test */
global object hash_table_test (object ht) {
  switch (ht_test_code(record_flags(TheHashtable(ht)))) {
    case bit(0): return S(eq);
    case bit(1): return S(eql);
    case bit(2): return S(equal);
    case bit(3): return S(equalp);
    case 0: { /* user-defined ==> (test . hash) */
      pushSTACK(ht);
      var object ret = allocate_cons();
      ht = popSTACK();
      Car(ret) = TheHashtable(ht)->ht_test;
      Cdr(ret) = TheHashtable(ht)->ht_hash;
      /* should we do this at all? */
      /*if (subrp(Car(ret))) Car(ret) = TheSubr(Car(ret))->name;
        if (subrp(Cdr(ret))) Cdr(ret) = TheSubr(Cdr(ret))->name;*/
      return ret;
    }
    default: NOTREACHED;
  }
}

LISPFUNNF(hash_table_test,1)
{ /* (HASH-TABLE-TEST hashtable), CLtL2 p. 441, dpANS p. 18-9 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  VALUES1(hash_table_test(ht)); /* symbol as value */
}

/* auxiliary functions for WITH-HASH-TABLE-ITERATOR, CLTL2 p. 439:
 (SYSTEM::HASH-TABLE-ITERATOR hashtable) returns an internal state
 for iterating through a hash-table.
 (SYSTEM::HASH-TABLE-ITERATE internal-state) iterates through a hash-table
 by one, thereby changes internal-state and returns: 3 values
 T, key, value of the next hash-table-entry resp. 1 value NIL at the end. */

LISPFUNNR(hash_table_iterator,1) {
  var object ht = check_hashtable(STACK_0); /* hashtable argument */
  /* An internal state consists of the key-value-vector and an index. */
  STACK_0 = TheHashtable(ht)->ht_kvtable; /* key-value-vector */
  var object maxcount = TheHashtable(ht)->ht_maxcount; /* maxcount */
  var object state = allocate_cons();
  Car(state) = popSTACK();      /* key-value-vector as car */
  Cdr(state) = maxcount;        /* maxcount as cdr */
  VALUES1(state);               /* state as value */
}

LISPFUNN(hash_table_iterate,1) {
  var object state = popSTACK(); /* internal state */
  if (consp(state)) {            /* hopefully a cons */
    var object table = Car(state); /* key-value-vector */
    loop {
      var uintL index = posfixnum_to_L(Cdr(state));
      if (index==0)             /* index=0 -> no more elements */
        break;
      Cdr(state) = fixnum_inc(Cdr(state),-1); /* decrement index */
      var gcv_object_t* KVptr = kvtable_data(table) + 2*index-2;
      if (!eq(KVptr[0],leer)) { /* Key /= "leer" ? */
        VALUES3(T,
                KVptr[0], /* key as the 2nd value */
                KVptr[1]); /* value as the 3rd value */
        return;
      }
    }
  }
  VALUES1(NIL); return; /* 1 value NIL */
}

LISPFUNNR(hash_table_weak_p,1)
{ /* (EXT:HASH-TABLE-WEAK-P ht) */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  VALUES1(ht_weak(ht));
}

LISPFUNN(set_hash_table_weak_p,2)
{ /* (SYS::%SET-HASH-TABLE-WEAK-P ht val)
  == (SETF (HASH-TABLE-WEAK-P ht) val) */
  STACK_1 = check_hashtable(STACK_1);
  var object val = check_weak(popSTACK()); /* weak-p */
  var object ht = STACK_0; /* hashtable argument */
  if (nullp(val) && ht_weak_p(ht)) {
    var uintL len = Weakkvt_length(TheHashtable(ht)->ht_kvtable);
    var object vec = allocate_vector(len);
    copy_mem_o(TheSvector(vec)->data,
               TheWeakKVT(TheHashtable(STACK_0)->ht_kvtable)->data,len);
    TheHashtable(STACK_0)->ht_kvtable = vec;
  } else if (!nullp(val) && !ht_weak_p(ht)) {
    var uintL len = Svector_length(TheHashtable(ht)->ht_kvtable);
    var object wkvt = allocate_weakkvt(len,val);
    copy_mem_o(TheWeakKVT(wkvt)->data,
               TheSvector(TheHashtable(STACK_0)->ht_kvtable)->data,len);
    TheHashtable(STACK_0)->ht_kvtable = wkvt;
  } else if (ht_weak_p(ht))
    TheWeakKVT(TheHashtable(STACK_0)->ht_kvtable)->wkvt_type = val;
  VALUES1(ht_weak(STACK_0)); skipSTACK(1);
}

LISPFUNN(class_gethash,2)
{/* (CLOS::CLASS-GETHASH ht object) is like (GETHASH (CLASS-OF object) ht). */
  var object ht = check_hashtable(STACK_1); /* hashtable argument */
  C_class_of();                 /* value1 := (CLASS-OF object) */
  var gcv_object_t* KVptr;
  var gcv_object_t* Nptr;
  var gcv_object_t* Iptr;
  /* search key value1 in the hash-table: */
  if (hash_lookup(ht,value1,&KVptr,&Nptr,&Iptr)) { /* -> Value as value: */
    VALUES2(KVptr[1], T); /* and T as the 2nd value */
  } else {                      /* not found -> NIL as value */
    VALUES2(NIL, NIL); /* NIL as the 2nd value */
  }
  skipSTACK(1);
}

/* (CLOS::CLASS-TUPLE-GETHASH ht object1 ... objectn)
 is like (GETHASH (funcall (hash-tuple-function n) class1 ... classn) ht)
 with classi = (CLASS-OF objecti).
 Definition: n>0, ht is an EQUAL-hashtable and (hash-tuple-function n) is
 defined in clos.lisp .
 This function is the core of the dispatch for generic functions. It has to
 be fast and must not cons.

 For 1 < n <= 16,
   (hash-tuple-function n ...) =
   (cons (hash-tuple-function n1 ...) (hash-tuple-function n2 ...)) */
local const uintC tuple_half_1 [17] = {0,0,1,1,2,2,2,3,4,4,4,4,4,5,6,7,8};
local const uintC tuple_half_2 [17] = {0,0,1,2,2,3,4,4,4,5,6,7,8,8,8,8,8};

/* auxiliary function: hashcode of a series of atoms, as if they were
 consed together via (hash-tuple-function n) : */
local uint32 hashcode_tuple (uintC n, const gcv_object_t* args_pointer,
                             uintC depth) {
  if (n==1) {
    return hashcode1(Next(args_pointer)); /* hashcode3_atom for classes */
  } else if (n<=16) {
    var uintC n1 = tuple_half_1[n];
    var uintC n2 = tuple_half_2[n]; /* n1 + n2 = n */
    var uint32 code1 = hashcode_tuple(n1,args_pointer,depth+1);
    var uint32 code2 = hashcode_tuple(n2,args_pointer STACKop -(uintP)n1,
                                      depth+1);
    switch (depth) {
      case 0: code1 = rotate_left(16,code1); break;
      case 1: code1 = rotate_left(7,code1); break; /* cf. hashcode3_cons3 */
      case 2: code1 = rotate_left(5,code1); break; /* cf. hashcode3_cons2 */
      case 3: code1 = rotate_left(3,code1); break; /* cf. hashcode3_cons1 */
      default: NOTREACHED;
    }
    return code1 ^ code2;
  } else { /* n>16, depth=0 */
    var uint32 code1 = hashcode_tuple(8,args_pointer,1);
    var uint32 code2 = hashcode_tuple(4,args_pointer STACKop -8,2);
    var uint32 code3 = hashcode_tuple(2,args_pointer STACKop -12,3);
    var uint32 code4 = hashcode_tuple(1,args_pointer STACKop -14,4);
    var uint32 code = 1;                /* cf. hashcode3_cons0 */
    code = rotate_left(3,code4) ^ code; /* cf. hashcode3_cons1 */
    code = rotate_left(5,code3) ^ code; /* cf. hashcode3_cons2 */
    code = rotate_left(7,code2) ^ code; /* cf. hashcode3_cons3 */
    code = rotate_left(16,code1) ^ code;
    return code;
  }
}
/* auxiliary function: Comparison of an object with a series of atoms, as if
 they were consed together via (hash-tuple-function n) : */
local bool equal_tuple (object obj, uintC n, const gcv_object_t* args_pointer) {
  if (n==1) {
    if (eq(obj,Next(args_pointer)))
      return true;
    else
      return false;
  } else if (n<=16) {
    if (consp(obj)) {
      var uintC n1 = tuple_half_1[n];
      var uintC n2 = tuple_half_2[n]; /* n1 + n2 = n */
      if (equal_tuple(Car(obj),n1,args_pointer)
          && equal_tuple(Cdr(obj),n2,args_pointer STACKop -(uintP)n1)
          )
        return true;
    }
    return false;
  } else {                      /* n>16 */
    if (consp(obj) && equal_tuple(Car(obj),8,args_pointer)) {
      obj = Cdr(obj);
      if (consp(obj) && equal_tuple(Car(obj),4,args_pointer STACKop -8)) {
        obj = Cdr(obj);
        if (consp(obj) && equal_tuple(Car(obj),2,args_pointer STACKop -12)) {
          obj = Cdr(obj);
          n-=14; args_pointer skipSTACKop -14;
          /* compare obj with a list of additional atoms: */
          dotimespC(n,n, {
            if (!(consp(obj) && eq(Car(obj),Next(args_pointer))))
              return false;
            obj = Cdr(obj); args_pointer skipSTACKop -1;
          });
          if (nullp(obj))
            /* comparison yields true */
            return true;
        }
      }
    }
    return false;
  }
}

LISPFUN(class_tuple_gethash,seclass_default,2,0,rest,nokey,0,NIL) {
  argcount++; rest_args_pointer skipSTACKop 1; /* arguments: ht {object}+ */
  /* first apply CLASS-OF to each argument: */
  {
    var gcv_object_t* arg_pointer = rest_args_pointer;
    var uintC count;
    dotimespC(count,argcount, {
      pushSTACK(Next(arg_pointer)); C_class_of(); /* (CLASS-OF arg) */
      NEXT(arg_pointer) = value1;                 /* =: arg */
    });
  }
  var object ht = check_hashtable(Before(rest_args_pointer));
  if (!ht_validp(TheHashtable(ht))) /* hash-table must still be reorganized */
    ht = rehash(ht);
  {
    var uint32 code =          /* calculate hashcode of the cons-tree */
      hashcode_tuple(argcount,rest_args_pointer,0);
    var uintL hashindex;
    divu_3232_3232(code,posfixnum_to_L(TheHashtable(ht)->ht_size),
                   (void),hashindex = );
    var gcv_object_t* Nptr =    /* pointer to the current entry */
      &TheSvector(TheHashtable(ht)->ht_itable)->data[hashindex];
    while (!eq(*Nptr,nix)) { /* track "list" : "list" finished -> not found */
      var uintL index = posfixnum_to_L(*Nptr); /* next index */
      Nptr =                    /* pointer to entry in next-vector */
        &TheSvector(TheHashtable(ht)->ht_ntable)->data[index];
      var gcv_object_t* KVptr = /* pointer to entries in key-value-vector */
        ht_kvt_data(ht)+2*index;
      if (equal_tuple(KVptr[0],argcount,rest_args_pointer)) { /* compare key */
        /* found */
        VALUES1(KVptr[1]); goto fertig; /* Value as value */
      }
    }
  }
  /* not found */
  VALUES1(NIL);
 fertig:
  set_args_end_pointer(rest_args_pointer STACKop 1); /* clean up STACK */
}

/* UP: Calculates a portable EQUAL-hashcode of an object.
 sxhash(obj)
 It is valid only until the next modification of the object.
 (equal X Y) implies (= (sxhash X) (sxhash Y)).
 > obj: an object
 < result: hashcode, a 32-bit-number */
/* can trigger GC
   -- if the argument was a CLOS instance that had to be updated */
local uint32 sxhash (object obj);
/* auxiliary functions for known type:
 atom -> fall differentiation by type */
local uint32 sxhash_atom (object obj) {
  #ifdef TYPECODES
  switch (typecode(obj))        /* per type */
  #else
  if (orecordp(obj))
    goto case_orecord;
  else if (consp(obj))
    goto case_cons;
  else if (charp(obj))
    goto case_char;
  else if (fixnump(obj))
    goto case_fixnum;
  else if (short_float_p(obj))
    goto case_sfloat;
  else if (subrp(obj))
    goto case_subr;
  else if (machinep(obj))
    goto case_machine;
  else if (read_label_p(obj) || systemp(obj))
    goto case_system;
  else switch (0)
  #endif
  {
    case_symbol:                /* symbol */
      /* utilize printname
       (not the home-package, because it is changed on UNINTERN) */
      return hashcode_string(Symbol_name(obj))+0x339B0E4CUL;
    case_cons:
    default:
      /* address may not be used, only utilize the type */
      #ifdef TYPECODES
      return highlow32(typecode(obj),0xDABE); /*typeinfo*2^16+identification*/
      #else
      return highlow32((as_oint(obj)>>oint_type_shift)&(oint_type_mask>>oint_type_shift),0xDABE); /* typeinfo*2^16+identification */
      #endif
    case_bvector:               /* bit-vector */
    case_b2vector:              /* 2bit-vector */
    case_b4vector:              /* 4bit-vector */
    case_b8vector:              /* 8bit-vector */
    case_b16vector:             /* 16bit-vector */
    case_b32vector:             /* 32bit-vector */
      /* bit-vector-content */
      return hashcode_bvector(obj);
    case_string:                /* string */
      /* string-content */
      return hashcode_string(obj);
    case_weakkvt:     /* weak key-value table - length is always even */
      return Weakkvt_length(obj)%2 + 0x4ECD0A9FUL; /* const same as svector */
    case_svector:                                  /* simple-vector */
      /* only utilize the length */
      return Svector_length(obj) + 0x4ECD0A9FUL;
    case_ovector:               /* (vector t) */
    case_mdarray:               /* common array */
      /* multi-dimensional array -> utilize only rank */
      return Iarray_rank(obj) + 0xAAFAFAAEUL;
    case_structure:             /* structure */
      /* utilize only structure-type (Liste (name_1 name_2 ... name_n)) */
      check_SP();
      return sxhash(TheStructure(obj)->structure_types) + 0xAD2CD2AEUL;
    case_stream:                /* stream */
      /* utilize only streamtype */
      return TheStream(obj)->strmtype + 0x3DAEAE55UL;
   {var uint32 bish_code;
    case_closure:               /* closure */
      /* utilize all elements ?? */
      bish_code = 0xB0DD939EUL; goto record_all;
    case_orecord: {             /* OtherRecord */
      /* utilize record-type, also:
       package: utilize package-name verwerten (not quite OK, as a
                package can be renamed with RENAME-PACKAGE!)
       pathname, byte, loadtimeeval: utilize all components
       hash-table, readtable, random-state, symbol-macro: nothing else */
      var sintB rectype = Record_type(obj);
      switch (rectype) {
        case_Rectype_Symbol_above;
        case_Rectype_bvector_above;
        case_Rectype_b2vector_above;
        case_Rectype_b4vector_above;
        case_Rectype_b8vector_above;
        case_Rectype_b16vector_above;
        case_Rectype_b32vector_above;
        case_Rectype_string_above;
        case_Rectype_Svector_above;
        case_Rectype_WeakKVT_above;
        case_Rectype_ovector_above;
        case_Rectype_mdarray_above;
        case_Rectype_Structure_above;
        case_Rectype_Stream_above;
        case_Rectype_Closure_above;
        case_Rectype_Instance_above;
        case_Rectype_Bignum_above;
        case_Rectype_Ffloat_above;
        case_Rectype_Dfloat_above;
        case_Rectype_Lfloat_above;
        case_Rectype_Ratio_above;
        case_Rectype_Complex_above;
        default: ;
      }
      bish_code = 0xB04D939EUL + rectype;
      switch (rectype) {
        case Rectype_Package: { /* package */
          /* utilize package-name */
          var uint32 next_code = hashcode_string(ThePackage(obj)->pack_name);
          return rotate_left(1,next_code) + bish_code;
        }
        case Rectype_Fsubr:     /* fsubr */
          /* utilize name */
          check_SP(); return sxhash(TheFsubr(obj)->name) + 0xFF3319BAUL;
        case Rectype_Pathname:  /* pathname */
       #ifdef LOGICAL_PATHNAMES
        case Rectype_Logpathname: /* pathname */
       #endif
        case Rectype_Byte:         /* byte */
        case Rectype_Loadtimeeval: /* loadtimeeval */
          goto record_all;
        default:
          return bish_code;
      }
    }
    record_all:
      /* record, in which all elements can be utilized */
      check_SP();
      {
        var gcv_object_t* ptr = &TheRecord(obj)->recdata[0];
        var uintC count = Record_length(obj);
        dotimespC(count,count, {
          /* combine hashcode of the next component: */
          var uint32 next_code = sxhash(*ptr++);
          bish_code = misch(bish_code,next_code);
        });
        return bish_code;
      }
   }
    case_instance:              /* instance */
      /* utilize only the class */
      instance_un_realloc(obj);
      check_instance(obj);
      return sxhash(TheInstance(obj)->inst_class) + 0x61EFA249;
    case_char:                  /* character */
      /* take EQ-hashcode (for characters EQUAL == EQL == EQ) */
      return hashcode1(obj);
    case_subr:                  /* SUBR */
      /* utilize name */
      check_SP(); return sxhash(TheSubr(obj)->name) + 0xFF3319BAUL;
    case_machine:               /* machine-pointer */
    case_system:                /* frame-pointer, read-label, system */
      /* utilize address */
      return hashcode1(obj);
    /* numbers: according to content, like with EQL */
    case_fixnum:                /* fixnum */
      return hashcode_fixnum(obj);
    case_bignum:                /* bignum */
      return hashcode_bignum(obj);
    case_sfloat:                /* short-float */
      return hashcode_sfloat(obj);
    case_ffloat:                /* single-float */
      return hashcode_ffloat(obj);
    case_dfloat:                /* double-float */
      return hashcode_dfloat(obj);
    case_lfloat:                /* Long-Float */
      return hashcode_lfloat(obj);
    case_ratio: {               /* ratio */
      /* hash both components, mix */
      var uint32 code1 = sxhash(TheRatio(obj)->rt_num);
      var uint32 code2 = sxhash(TheRatio(obj)->rt_den);
      return misch(code1,code2);
    }
    case_complex: {             /* complex */
      /* hash both components, mix */
      var uint32 code1 = sxhash(TheComplex(obj)->c_real);
      var uint32 code2 = sxhash(TheComplex(obj)->c_imag);
      return misch(code1,code2);
    }
  }
}
/* cons -> look at content up to depth 4:
 determine the hashcode of the CAR and the hashcode of the CDR at a time
 and combine them shifted. As shifts fit e.g. 16,7,5,3,
 because {0,16} + {0,7} + {0,5} + {0,3}
       = {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
 consists of 16 different elements of {0,...,31} .
 object, for cons only up to depth 0 */
local uint32 sxhash_cons0 (object obj) {
  if (atomp(obj)) {
    return sxhash_atom(obj);
  } else {                      /* cons -> hashcode := 1 */
    return 1;
  }
}
/* object, for cons only up to depth 1 */
local uint32 sxhash_cons1 (object obj) {
  if (atomp(obj)) {
    return sxhash_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = sxhash_cons0(Car(obj));
    var uint32 code2 = sxhash_cons0(Cdr(obj));
    return rotate_left(3,code1) ^ code2;
  }
}
/* object, for cons only up to depth 2 */
local uint32 sxhash_cons2 (object obj) {
  if (atomp(obj)) {
    return sxhash_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = sxhash_cons1(Car(obj));
    var uint32 code2 = sxhash_cons1(Cdr(obj));
    return rotate_left(5,code1) ^ code2;
  }
}
/* object, for cons only up to depth 3 */
local uint32 sxhash_cons3 (object obj) {
  if (atomp(obj)) {
    return sxhash_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = sxhash_cons2(Car(obj));
    var uint32 code2 = sxhash_cons2(Cdr(obj));
    return rotate_left(7,code1) ^ code2;
  }
}
/* object, for cons only up to depth 4 */
local uint32 sxhash (object obj) {
  if (atomp(obj)) {
    return sxhash_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = sxhash_cons3(Car(obj));
    var uint32 code2 = sxhash_cons3(Cdr(obj));
    return rotate_left(16,code1) ^ code2;
  }
}

LISPFUNN(sxhash,1)
{ /* (SXHASH object), CLTL p. 285 */
  var uint32 sx = sxhash(popSTACK());
  /* ANSI CL (SXHASH doc):
   For any two objects, x and y, both of which are bit vectors,
   characters, conses, numbers, pathnames, strings, or symbols, and which
   are similar, (sxhash x) and (sxhash y) yield the same mathematical
   value even if x and y exist in different Lisp images of the same
   implementation.
   This means that as long as some CLISPs have 24-bit fixnums,
   we have to limit SXHASH to 24 bits on all platforms.
   (assuming that CLISP on Tru64 and CLISP on Win32
   are the same implementations) */
  #if oint_data_len >= 24
    sx = sx % 0xFFFFFF;
    VALUES1(fixnum(sx));
  #else
    #error "sxhash results do not fit in a fixnum"
  #endif
}

