# Allocator functions for the various types.

# ------------------------------ Specification ---------------------------------

# All these are declared in lispbibl.d.
  global object allocate_cons (void);
  global object make_symbol (object string);
  global object allocate_vector (uintL len);
  global object allocate_bit_vector (uintL len);
  global object allocate_string (uintL len);
  global object allocate_iarray (uintB flags, uintC rank, tint type);
  #ifdef TYPECODES
  global object allocate_srecord_ (uintW flags_rectype, uintC reclen, tint type);
  global object allocate_xrecord_ (uintW flags_rectype, uintC reclen, uintC recxlen, tint type);
  #else
  global object allocate_srecord_ (uintW flags_rectype, uintC reclen);
  global object allocate_xrecord_ (uintW flags_rectype, uintC reclen, uintC recxlen);
  #endif
  #ifndef case_stream
  global object allocate_stream (uintB strmflags, uintB strmtype, uintC reclen, uintC recxlen);
  #endif
  #ifdef FOREIGN
  global object allocate_fpointer (FOREIGN foreign);
  #endif
  #ifdef FOREIGN_HANDLE
  global object allocate_handle (Handle handle);
  #endif
  global object allocate_bignum (uintC len, sintB sign);
  global object allocate_ffloat (ffloat value);
  #ifdef intQsize
  global object allocate_dfloat (dfloat value);
  #else
  global object allocate_dfloat (uint32 semhi, uint32 mlo);
  #endif
  global object allocate_lfloat (uintC len, uintL expo, signean sign);
  global object make_ratio (object num, object den);
  global object make_complex (object real, object imag);

# ------------------------------ Implementation --------------------------------

# UP, beschafft ein Cons
# allocate_cons()
# < ergebnis: Pointer auf neues CONS, mit CAR und CDR =NIL
# can trigger GC
  global object allocate_cons (void);
  global object allocate_cons()
    { allocate(cons_type,FALSE,sizeof(cons_),Cons,ptr,
               { ptr->cdr = NIL; ptr->car = NIL; }
              )
    }

# UP: Liefert ein neu erzeugtes uninterniertes Symbol mit gegebenem Printnamen.
# make_symbol(string)
# > string: immutable Simple-String
# < ergebnis: neues Symbol mit diesem Namen, mit Home-Package=NIL.
# can trigger GC
  global object make_symbol (object string);
  global object make_symbol(string)
    var object string;
    { pushSTACK(string); # String retten
      #define FILL  \
        { ptr->symvalue = unbound; # leere Wertzelle         \
          ptr->symfunction = unbound; # leere Funktionszelle \
          ptr->proplist = NIL; # leere Propertyliste         \
          ptr->pname = popSTACK(); # Namen eintragen         \
          ptr->homepackage = NIL; # keine Home-Package       \
        }
      #ifdef TYPECODES
        allocate(symbol_type,TRUE,size_symbol(),Symbol,ptr,
                 { FILL; }
                )
      #else
        allocate(symbol_type,TRUE,size_xrecord(5,0),Symbol,ptr,
                 { ptr->tfl = xrecord_tfl(Rectype_Symbol,0,5,0); FILL; }
                )
      #endif
      #undef FILL
    }

# UP, beschafft Vektor
# allocate_vector(len)
# > len: Länge des Vektors
# < ergebnis: neuer Vektor (Elemente werden mit NIL initialisiert)
# can trigger GC
  global object allocate_vector (uintL len);
  global object allocate_vector (len)
    var uintL len;
    { var uintL need = size_svector(len); # benötigter Speicherplatz
      #ifdef TYPECODES
        #define SETTFL  ptr->length = len;
      #else
        #define SETTFL  ptr->tfl = lrecord_tfl(Rectype_Svector,len);
      #endif
      allocate(svector_type,TRUE,need,Svector,ptr,
               { SETTFL
                 if (len > 0)
                   { var object* p = &ptr->data[0];
                     dotimespL(len,len, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
               }   }
              )
      #undef SETTFL
    }

# UP, beschafft Bit-Vektor
# allocate_bit_vector(len)
# > len: Länge des Bitvektors (in Bits)
# < ergebnis: neuer Bitvektor (LISP-Objekt)
# can trigger GC
  global object allocate_bit_vector (uintL len);
  global object allocate_bit_vector (len)
    var uintL len;
    { var uintL need = size_sbvector(len); # benötigter Speicherplatz in Bytes
      #ifdef TYPECODES
        #define SETTFL  ptr->length = len;
      #else
        #define SETTFL  ptr->tfl = lrecord_tfl(Rectype_Sbvector,len);
      #endif
      allocate(sbvector_type,TRUE,need,Sbvector,ptr,
               { SETTFL } # Keine weitere Initialisierung
              )
      #undef SETTFL
    }

# UP, beschafft String
# allocate_string(len)
# > len: Länge des Strings (in Characters)
# < ergebnis: neuer Normal-Simple-String (LISP-Objekt)
# can trigger GC
  global object allocate_string (uintL len);
  global object allocate_string (len)
    var uintL len;
    { var uintL need = size_sstring(len); # benötigter Speicherplatz in Bytes
      #ifdef TYPECODES
        #define SETTFL  ptr->length = len;
      #else
        #define SETTFL  ptr->tfl = lrecord_tfl(Rectype_Sstring,len);
      #endif
      allocate(sstring_type,TRUE,need,Sstring,ptr,
               { SETTFL } # Keine weitere Initialisierung
              )
      #undef SETTFL
    }

#ifndef TYPECODES
# UP, beschafft immutablen String
# allocate_imm_string(len)
# > len: Länge des Strings (in Characters)
# < ergebnis: neuer immutabler Normal-Simple-String (LISP-Objekt)
# can trigger GC
  global object allocate_imm_string (uintL len);
  global object allocate_imm_string (len)
    var uintL len;
    { var uintL need = size_sstring(len); # benötigter Speicherplatz in Bytes
      #define SETTFL  ptr->tfl = lrecord_tfl(Rectype_Imm_Sstring,len);
      allocate(sstring_type,TRUE,need,Sstring,ptr,
               { SETTFL } # Keine weitere Initialisierung
              )
      #undef SETTFL
    }
#endif

#ifdef HAVE_SMALL_SSTRING
# UP, beschafft immutablen Small-String
# allocate_imm_small_string(len)
# > len: Länge des Strings (in Characters)
# < ergebnis: neuer immutabler Small-Simple-String (LISP-Objekt)
# can trigger GC
  global object allocate_imm_small_string (uintL len);
  global object allocate_imm_small_string (len)
    var uintL len;
    { var uintL need = size_small_sstring(len); # benötigter Speicherplatz in Bytes
      #define SETTFL  ptr->tfl = lrecord_tfl(Rectype_Imm_SmallSstring,len);
      allocate(sstring_type,TRUE,need,SmallSstring,ptr,
               { SETTFL } # Keine weitere Initialisierung
              )
      #undef SETTFL
    }
#endif

# UP, beschafft indirekten Array
# allocate_iarray(flags,rank,type)
# > uintB flags: Flags
# > uintC (eigentlich uintWC) rank: Rang
# > tint type: Typinfo
# < ergebnis: LISP-Objekt Array
# can trigger GC
  global object allocate_iarray (uintB flags, uintC rank, tint type);
  global object allocate_iarray(flags,rank,type)
    var uintB flags;
    var uintC rank;
    var tint type;
    { var uintL need = rank;
      if (flags & bit(arrayflags_fillp_bit)) { need += 1; }
      if (flags & bit(arrayflags_dispoffset_bit)) { need += 1; }
      need = size_iarray(need);
      #ifdef TYPECODES
        #define SETTFL  ptr->flags = flags; ptr->rank = rank;
      #else
        #define SETTFL  ptr->tfl = srecord_tfl(type,flags,rank);
      #endif
      allocate(type,TRUE,need,Iarray,ptr,
               { SETTFL # Flags und Rang eintragen
                 ptr->data = NIL; # Datenvektor mit NIL initialisieren
               }
              )
      #undef SETTFL
    }

# UP, beschafft Simple-Record
# allocate_srecord_(flags_rectype,reclen,type)
# > uintW flags_rectype: Flags, nähere Typinfo
# > uintC reclen: Länge
# > tint type: Typinfo
# < ergebnis: LISP-Objekt Record (Elemente werden mit NIL initialisiert)
# can trigger GC
  #ifdef TYPECODES
  global object allocate_srecord_ (uintW flags_rectype, uintC reclen, tint type);
  global object allocate_srecord_(flags_rectype,reclen,type)
    var uintW flags_rectype;
    var uintC reclen;
    var tint type;
    { ASSERT((sintB)(flags_rectype >> (BIG_ENDIAN_P ? 0 : 8)) < rectype_limit);
     {var uintL need = size_srecord(reclen);
      allocate(type,TRUE,need,Srecord,ptr,
               { *(uintW*)pointerplus(ptr,offsetof(record_,recflags)) = flags_rectype; # Flags, Typ eintragen
                 ptr->reclength = reclen; # Länge eintragen
                {var object* p = &ptr->recdata[0];
                 dotimespC(reclen,reclen, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
               }}
              )
    }}
  #else
  global object allocate_srecord_ (uintW flags_rectype, uintC reclen);
  global object allocate_srecord_(flags_rectype,reclen)
    var uintW flags_rectype;
    var uintC reclen;
    { var uintL need = size_srecord(reclen);
      allocate(type,TRUE,need,Srecord,ptr,
               { ptr->tfl = (uintL)flags_rectype + ((uintL)reclen << 16);
                {var object* p = &ptr->recdata[0];
                 dotimespC(reclen,reclen, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
               }}
              )
    }
  #endif

# UP, beschafft Extended-Record
# allocate_xrecord_(flags_rectype,reclen,recxlen,type)
# > uintW flags_rectype: Flags, nähere Typinfo
# > uintC reclen: Länge
# > uintC recxlen: Extra-Länge
# > tint type: Typinfo
# < ergebnis: LISP-Objekt Record (Elemente werden mit NIL bzw. 0 initialisiert)
# can trigger GC
  #ifdef TYPECODES
  global object allocate_xrecord_ (uintW flags_rectype, uintC reclen, uintC recxlen, tint type);
  global object allocate_xrecord_(flags_rectype,reclen,recxlen,type)
    var uintW flags_rectype;
    var uintC reclen;
    var uintC recxlen;
    var tint type;
    { ASSERT((sintB)(flags_rectype >> (BIG_ENDIAN_P ? 0 : 8)) >= rectype_limit);
     {var uintL need = size_xrecord(reclen,recxlen);
      allocate(type,TRUE,need,Xrecord,ptr,
               { *(uintW*)pointerplus(ptr,offsetof(record_,recflags)) = flags_rectype; # Flags, Typ eintragen
                 ptr->reclength = reclen; ptr->recxlength = recxlen; # Längen eintragen
                {var object* p = &ptr->recdata[0];
                 dotimesC(reclen,reclen, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
                 if (recxlen > 0)
                   { var uintB* q = (uintB*)p;
                     dotimespC(recxlen,recxlen, { *q++ = 0; } ); # Extra-Elemente mit 0 vollschreiben
               }}  }
              )
    }}
  #else
  global object allocate_xrecord_ (uintW flags_rectype, uintC reclen, uintC recxlen);
  global object allocate_xrecord_(flags_rectype,reclen,recxlen)
    var uintW flags_rectype;
    var uintC reclen;
    var uintC recxlen;
    { var uintL need = size_xrecord(reclen,recxlen);
      allocate(type,TRUE,need,Xrecord,ptr,
               { ptr->tfl = (uintL)flags_rectype + ((uintL)reclen << 16) + ((uintL)recxlen << 24); # Flags, Typ, Längen eintragen
                {var object* p = &ptr->recdata[0];
                 dotimesC(reclen,reclen, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
                 if (recxlen > 0)
                   { var uintB* q = (uintB*)p;
                     dotimespC(recxlen,recxlen, { *q++ = 0; } ); # Extra-Elemente mit 0 vollschreiben
               }}  }
              )
    }
  #endif

#ifndef case_stream

# UP, beschafft Stream
# allocate_stream(strmflags,strmtype,reclen)
# > uintB strmflags: Flags
# > uintB strmtype: nähere Typinfo
# > uintC reclen: Länge in Objekten
# > uintC recxlen: Extra-Länge in Bytes
# < ergebnis: LISP-Objekt Stream (Elemente werden mit NIL initialisiert)
# can trigger GC
  global object allocate_stream (uintB strmflags, uintB strmtype, uintC reclen, uintC recxlen);
  global object allocate_stream(strmflags,strmtype,reclen,recxlen)
    var uintB strmflags;
    var uintB strmtype;
    var uintC reclen;
    var uintC recxlen;
    { var object obj = allocate_xrecord(0,Rectype_Stream,reclen,recxlen,orecord_type);
      TheRecord(obj)->recdata[0] = Fixnum_0; # Fixnum als Platz für strmflags und strmtype
      TheStream(obj)->strmflags = strmflags; TheStream(obj)->strmtype = strmtype;
      return obj;
    }

#endif

#ifdef FOREIGN

# UP, beschafft Foreign-Pointer-Verpackung
# allocate_fpointer(foreign)
# > foreign: vom Typ FOREIGN
# < ergebnis: LISP-Objekt, das foreign enthält
# can trigger GC
  global object allocate_fpointer (FOREIGN foreign);
  global object allocate_fpointer(foreign)
    var FOREIGN foreign;
    { var object result = allocate_xrecord(0,Rectype_Fpointer,fpointer_length,fpointer_xlength,orecord_type);
      TheFpointer(result)->fp_pointer = foreign;
      return result;
    }

#endif

#ifdef FOREIGN_HANDLE

# UP, beschafft Handle-Verpackung
# allocate_handle(handle)
# < ergebnis: LISP-Objekt, das handle enthält
# can trigger GC
  global object allocate_handle (Handle handle);
  global object allocate_handle(handle)
    var Handle handle;
    { var object result = allocate_bit_vector(sizeof(Handle)*8);
      TheHandle(result) = handle;
      return result;
    }

#endif

# UP, beschafft Bignum
# allocate_bignum(len,sign)
# > uintC (eigentlich uintWC) len: Länge der Zahl (in Digits)
# > sintB sign: Flag für Vorzeichen (0 = +, -1 = -)
# < ergebnis: neues Bignum (LISP-Objekt)
# can trigger GC
  global object allocate_bignum (uintC len, sintB sign);
  global object allocate_bignum(len,sign)
    var uintC len;
    var sintB sign;
    { var uintL need = size_bignum(len); # benötigter Speicherplatz in Bytes
      #ifdef TYPECODES
        #define SETTFL  ptr->length = len;
      #else
        #define SETTFL  ptr->tfl = srecord_tfl(Rectype_Bignum,(uintB)sign,len);
      #endif
      allocate(bignum_type | (sign & bit(sign_bit_t)),TRUE,need,Bignum,ptr,
               { SETTFL } # Keine weitere Initialisierung
              )
      #undef SETTFL
    }

# UP, beschafft Single-Float
# allocate_ffloat(value)
# > ffloat value: Zahlwert (Bit 31 = Vorzeichen)
# < ergebnis: neues Single-Float (LISP-Objekt)
# can trigger GC
  global object allocate_ffloat (ffloat value);
  #ifndef WIDE
  global object allocate_ffloat(value)
    var ffloat value;
    {
      #ifdef TYPECODES
        #define SETTFL
      #else
        #define SETTFL  ptr->tfl = xrecord_tfl(Rectype_Ffloat,((sint32)value<0 ? 0xFF : 0),0,sizeof(ffloat));
      #endif
      allocate(ffloat_type | ((sint32)value<0 ? bit(sign_bit_t) : 0) # Vorzeichenbit aus value
               ,TRUE,size_ffloat(),Ffloat,ptr,
               { SETTFL; ptr->float_value = value; }
              )
      #undef SETTFL
    }
  #else
  global object allocate_ffloat(value)
    var ffloat value;
    { return
        type_data_object(ffloat_type | ((sint32)value<0 ? bit(sign_bit_t) : 0), # Vorzeichenbit aus value
                         value
                        );
    }
  #endif

# UP, beschafft Double-Float
#ifdef intQsize
# allocate_dfloat(value)
# > dfloat value: Zahlwert (Bit 63 = Vorzeichen)
# < ergebnis: neues Double-Float (LISP-Objekt)
# can trigger GC
  global object allocate_dfloat (dfloat value);
  global object allocate_dfloat(value)
    var dfloat value;
    {
      #ifdef TYPECODES
        #define SETTFL
      #else
        #define SETTFL  ptr->tfl = xrecord_tfl(Rectype_Dfloat,((sint64)value<0 ? 0xFF : 0),0,sizeof(dfloat));
      #endif
      allocate(dfloat_type | ((sint64)value<0 ? bit(sign_bit_t) : 0) # Vorzeichenbit aus value
               ,TRUE,size_dfloat(),Dfloat,ptr,
               { SETTFL; ptr->float_value = value; }
              )
      #undef SETTFL
    }
#else
# allocate_dfloat(semhi,mlo)
# > semhi,mlo: Zahlwert (Bit 31 von semhi = Vorzeichen)
# < ergebnis: neues Double-Float (LISP-Objekt)
# can trigger GC
  global object allocate_dfloat (uint32 semhi, uint32 mlo);
  global object allocate_dfloat(semhi,mlo)
    var uint32 semhi;
    var uint32 mlo;
    {
      #ifdef TYPECODES
        #define SETTFL
      #else
        #define SETTFL  ptr->tfl = xrecord_tfl(Rectype_Dfloat,((sint32)semhi<0 ? 0xFF : 0),0,sizeof(dfloat));
      #endif
      allocate(dfloat_type | ((sint32)semhi<0 ? bit(sign_bit_t) : 0) # Vorzeichenbit aus value
               ,TRUE,size_dfloat(),Dfloat,ptr,
               { SETTFL; ptr->float_value.semhi = semhi; ptr->float_value.mlo = mlo; }
              )
      #undef SETTFL
    }
#endif

# UP, beschafft Long-Float
# allocate_lfloat(len,expo,sign)
# > uintC (eigentlich uintWC) len: Länge der Mantisse (in Digits)
# > uintL expo: Exponent
# > signean sign: Vorzeichen (0 = +, -1 = -)
# < ergebnis: neues Long-Float, noch ohne Mantisse
# Ein LISP-Objekt liegt erst dann vor, wenn die Mantisse eingetragen ist!
# can trigger GC
  global object allocate_lfloat (uintC len, uintL expo, signean sign);
  global object allocate_lfloat(len,expo,sign)
    var uintC len;
    var uintL expo;
    var signean sign;
    { var uintL need = size_lfloat(len); # benötigter Speicherplatz in Bytes
      #ifdef TYPECODES
        #define SETTFL  ptr->len = len;
      #else
        #define SETTFL  ptr->tfl = srecord_tfl(Rectype_Lfloat,(uintB)sign,len);
      #endif
      allocate(lfloat_type | ((tint)sign & bit(sign_bit_t))
               ,TRUE,need,Lfloat,ptr,
               { SETTFL; ptr->expo = expo; } # Keine weitere Initialisierung
              )
      #undef SETTFL
    }

# UP, erzeugt Bruch
# make_ratio(num,den)
# > object num: Zähler (muss Integer /= 0 sein, relativ prim zu den)
# > object den: Nenner (muss Integer > 1 sein)
# < ergebnis: Bruch
# can trigger GC
  global object make_ratio (object num, object den);
  global object make_ratio(num,den)
    var object num;
    var object den;
    { pushSTACK(den); pushSTACK(num); # Argumente sichern
     {
      #ifdef TYPECODES
      var tint type = # Vorzeichen von num übernehmen
        #ifdef fast_mtypecode
        ratio_type | (mtypecode(STACK_0) & bit(sign_bit_t))
        #else
        ratio_type | (typecode(num) & bit(sign_bit_t))
        #endif
        ;
      #endif
      #define FILL  \
         ptr->rt_num = popSTACK(); # Zähler eintragen \
         ptr->rt_den = popSTACK(); # Nenner eintragen
      #ifdef SPVW_MIXED
        # see allocate_xrecord
        #ifdef TYPECODES
          #define SETTFL  \
            *(uintW*)pointerplus(ptr,offsetof(record_,recflags)) = ((uintW)Rectype_Ratio << (BIG_ENDIAN_P ? 0 : 8)); \
            ptr->reclength = 2; ptr->recxlength = 0;
        #else
          var uintL tfl = xrecord_tfl(Rectype_Ratio,(positivep(num) ? 0 : 0xFF),2,0);
          #define SETTFL  \
            ptr->tfl = tfl;
        #endif
        allocate(type,TRUE,size_xrecord(2,0),Ratio,ptr,
                 { SETTFL; FILL; }
                )
        #undef SETTFL
      #else
        allocate(type,FALSE,sizeof(ratio_),Ratio,ptr,
                 { FILL; }
                )
      #endif
      #undef FILL
    }}

# UP, erzeugt komplexe Zahl
# make_complex(real,imag)
# > real: Realteil (muss reelle Zahl sein)
# > imag: Imaginärteil (muss reelle Zahl /= Fixnum 0 sein)
# < ergebnis: komplexe Zahl
# can trigger GC
  global object make_complex (object real, object imag);
  global object make_complex(real,imag)
    var object real;
    var object imag;
    { pushSTACK(imag); pushSTACK(real);
      #define FILL  \
        ptr->c_real = popSTACK(); # Realteil eintragen \
        ptr->c_imag = popSTACK(); # Imaginärteil eintragen
      #ifdef SPVW_MIXED
        # see allocate_xrecord
        #ifdef TYPECODES
          #define SETTFL  \
            *(uintW*)pointerplus(ptr,offsetof(record_,recflags)) = ((uintW)Rectype_Complex << (BIG_ENDIAN_P ? 0 : 8)); \
            ptr->reclength = 2; ptr->recxlength = 0;
        #else
          #define SETTFL  \
            ptr->tfl = xrecord_tfl(Rectype_Complex,0,2,0);
        #endif
        allocate(complex_type,TRUE,size_xrecord(2,0),Complex,ptr,
                 { SETTFL; FILL; }
                )
        #undef SETTFL
      #else
        allocate(complex_type,FALSE,sizeof(complex_),Complex,ptr,
                 { FILL; }
                )
      #endif
      #undef FILL
    }
