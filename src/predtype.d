# Prädikate für Gleichheit und Typtests, Typen, Klassen in CLISP
# Bruno Haible 1990-1999

#include "lispbibl.c"

# UP: testet auf Atomgleichheit EQL
# eql(obj1,obj2)
# > obj1,obj2: Lisp-Objekte
# < ergebnis: TRUE, falls Objekte gleich
  global boolean eql (object obj1, object obj2);
  global boolean eql(obj1,obj2)
    var object obj1;
    var object obj2;
    { start:
      if (eq(obj1,obj2)) { return TRUE; } # (EQ x y) ==> (EQL x y)
      # sonst ist EQL-Gleichheit nur möglich, wenn beides Zahlen sind:
      #ifdef TYPECODES
      if (!(numberp(obj1) && numberp(obj2))) { return FALSE; }
      # und der Typ von beiden muss übereinstimmen:
      if (!(typecode(obj1) == typecode(obj2))) { return FALSE; }
      switch (typecode(obj1))
      #else
      if (!(orecordp(obj1) && orecordp(obj2))) { return FALSE; }
      if (!(Record_type(obj1) == Record_type(obj2))) { return FALSE; }
      switch (Record_type(obj1))
        { case_Rectype_Bignum_above;
          case_Rectype_Ratio_above;
          case_Rectype_Complex_above;
          case_Rectype_Ffloat_above;
          case_Rectype_Dfloat_above;
          case_Rectype_Lfloat_above;
          default: goto no;
        }
      switch (0)
      #endif
        { case_bignum: # Bignums
            # Längen vergleichen:
            { var uintC length1 = Bignum_length(obj1);
              if (!(length1 == Bignum_length(obj2))) goto no;
            # Ziffern vergleichen:
             {var uintD* ptr1 = &TheBignum(obj1)->data[0];
              var uintD* ptr2 = &TheBignum(obj2)->data[0];
              dotimespC(length1,length1, { if (!(*ptr1++ == *ptr2++)) goto no; });
            }}
            return TRUE;
          case_ratio: # Ratio
            # Zähler und Nenner müssen übereinstimmen:
            # (and (eql (numerator obj1) (numerator obj2))
            #      (eql (denominator obj1) (denominator obj2))
            # )
            if (!eql(TheRatio(obj1)->rt_num,TheRatio(obj2)->rt_num)) goto no;
            # return eql(TheRatio(obj1)->rt_den,TheRatio(obj2)->rt_den);
            obj1 = TheRatio(obj1)->rt_den; obj2 = TheRatio(obj2)->rt_den;
            goto start;
          case_complex: # Complex
            # Real- und Imaginärteil müssen übereinstimmen:
            # (and (eql (realpart obj1) (realpart obj2))
            #      (eql (imagpart obj1) (imagpart obj2))
            # )
            if (!eql(TheComplex(obj1)->c_real,TheComplex(obj2)->c_real)) goto no;
            # return eql(TheComplex(obj1)->c_imag,TheComplex(obj2)->c_imag);
            obj1 = TheComplex(obj1)->c_imag; obj2 = TheComplex(obj2)->c_imag;
            goto start;
          case_ffloat: # Single-Floats
            #ifndef WIDE
            if (TheFfloat(obj1)->float_value == TheFfloat(obj2)->float_value)
              return TRUE;
              else
            #endif
              goto no;
          case_dfloat: # Double-Floats
            #ifdef intQsize
            if (TheDfloat(obj1)->float_value == TheDfloat(obj2)->float_value)
            #else
            if ((TheDfloat(obj1)->float_value.semhi == TheDfloat(obj2)->float_value.semhi)
                && (TheDfloat(obj1)->float_value.mlo == TheDfloat(obj2)->float_value.mlo)
               )
            #endif
              return TRUE;
              else
              goto no;
          case_lfloat: # Long-Floats
            # Längen vergleichen:
            { var uintC len1 = Lfloat_length(obj1);
              if (!(len1 == Lfloat_length(obj2))) goto no;
            # Exponenten vergleichen:
              if (!(TheLfloat(obj1)->expo == TheLfloat(obj2)->expo)) goto no;
            # Ziffern vergleichen:
             {var uintD* ptr1 = &TheLfloat(obj1)->data[0];
              var uintD* ptr2 = &TheLfloat(obj2)->data[0];
              dotimespC(len1,len1, { if (!(*ptr1++ == *ptr2++)) goto no; });
            }}
            return TRUE;
          /* case_fixnum: */ # Fixnums: hätten schon EQ sein müssen
          /* case_sfloat: */ # Short-Floats: hätten schon EQ sein müssen
          default:
          no: return FALSE;
        }
    }

# UP: testet auf Gleichheit EQUAL
# equal(obj1,obj2)
# > obj1,obj2: Lisp-Objekte
# < ergebnis: TRUE, falls Objekte gleich
  global boolean equal (object obj1, object obj2);
  global boolean equal(obj1,obj2)
    var object obj1;
    var object obj2;
    { start:
      if (eql(obj1,obj2)) { return TRUE; } # (EQL x y) ==> (EQUAL x y)
      # sonst ist EQUAL-Gleichheit nur möglich, wenn beides strukturierte
      # Typen sind. Typen müssen (bis auf notsimple_bit) übereinstimmen:
      #ifdef TYPECODES
      switch (typecode(obj1))
      #else
      if (consp(obj1)) { goto case_cons; }
      elif (orecordp(obj1)) { goto case_orecord; }
      else { goto no; }
      switch (0)
      #endif
        { case_cons: # Conses rekursiv vergleichen:
            if (!consp(obj2)) { return FALSE; }
            # CAR und CDR müssen übereinstimmen:
            # (and (equal (car obj1) (car obj2)) (equal (cdr obj1) (cdr obj2)))
            check_SP();
            if (!equal(Car(obj1),Car(obj2))) goto no;
            # return equal(Cdr(obj1),Cdr(obj2));
            obj1 = Cdr(obj1); obj2 = Cdr(obj2);
            goto start;
          case_obvector: # Byte-Vektoren
            if (!((Iarray_flags(obj1) & arrayflags_atype_mask) == Atype_Bit))
              { return FALSE; } # hätten schon EQL sein müssen
          case_sbvector: # Bit-Vektoren elementweise vergleichen:
            if (!bit_vector_p(obj2)) { return FALSE; }
            { # Längen vergleichen:
              var uintL len1 = vector_length(obj1);
              if (!(len1 == vector_length(obj2))) goto no;
              # Inhalt vergleichen:
             {var uintL index1 = 0;
              var uintL index2 = 0;
              var object sbv1 = array_displace_check(obj1,len1,&index1);
              var object sbv2 = array_displace_check(obj2,len1,&index2);
              # sbvi ist der Datenvektor, indexi der Index in den Datenvektor
              # zu obji (i=1,2).
              return bit_compare(sbv1,index1,sbv2,index2,len1);
            }}
          case_string: # Strings elementweise vergleichen:
            if (!stringp(obj2)) { return FALSE; }
            { # Längen vergleichen:
              var uintL len1 = vector_length(obj1);
              if (!(len1 == vector_length(obj2))) goto no;
              # Inhalt vergleichen:
              if (!(len1==0))
                { var uintL index1 = 0;
                  var uintL index2 = 0;
                  var object ss1 = array_displace_check(obj1,len1,&index1);
                  var object ss2 = array_displace_check(obj2,len1,&index2);
                  # ssi ist der Datenvektor, indexi der Index in den Datenvektor
                  # zu obji (i=1,2).
                  return string_eqcomp(ss1,index1,ss2,index2,len1);
                }
              return TRUE;
            }
          case_orecord:
            switch (Record_type(obj1))
              { case_Rectype_obvector_above;
                case_Rectype_Sbvector_above;
                case_Rectype_string_above;
                case Rectype_Pathname:
                  # Pathnames komponentenweise vergleichen:
                  if (!pathnamep(obj2)) goto no;
                  { var object* ptr1 = &TheRecord(obj1)->recdata[0];
                    var object* ptr2 = &TheRecord(obj2)->recdata[0];
                    var uintC count;
                    check_SP();
                   #if !(defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32))
                    dotimespC(count,pathname_length,
                      { if (!equal(*ptr1++,*ptr2++)) goto no; }
                      );
                   #else # defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
                    # Pathname-Komponenten bestehen aus Conses, Simple-Strings
                    # und Symbolen. Simple-Strings case-insensitive vergleichen:
                    dotimespC(count,pathname_length,
                      { if (!equalp(*ptr1++,*ptr2++)) goto no; } # (löst keine GC aus!)
                      );
                   #endif
                    return TRUE;
                  }
                #ifdef LOGICAL_PATHNAMES
                case Rectype_Logpathname:
                  # auch Logical Pathnames komponentenweise vergleichen:
                  if (!logpathnamep(obj2)) goto no;
                  { var object* ptr1 = &TheRecord(obj1)->recdata[0];
                    var object* ptr2 = &TheRecord(obj2)->recdata[0];
                    var uintC count;
                    check_SP();
                    dotimespC(count,logpathname_length,
                      { if (!equal(*ptr1++,*ptr2++)) goto no; }
                      );
                    return TRUE;
                  }
                #endif
                default: goto no;
              }
          # Sonst gelten obj1 und obj2 als verschieden.
          default: no:
            return FALSE;
    }   }

# UP: testet auf laschere Gleichheit EQUALP
# equalp(obj1,obj2)
# > obj1,obj2: Lisp-Objekte
# < ergebnis: TRUE, falls Objekte gleich
  global boolean equalp (object obj1, object obj2);
  # Element-by-element comparisons for various vector types. count > 0.
  local boolean elt_compare_T_T (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_T_Char (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_T_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_T_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_T_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_T_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_T_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_T_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_Char_Char (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_Bit_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_Bit_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_Bit_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_Bit_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_2Bit_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_2Bit_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_2Bit_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_2Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_2Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_4Bit_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_4Bit_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_4Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_4Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_8Bit_8Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_8Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_8Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_16Bit_16Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_16Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_32Bit_32Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare (object dv1, uintL index1, object dv2, uintL index2, uintL count);
  local boolean elt_compare_T_T(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { check_SP();
     {var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var const object* ptr2 = &TheSvector(dv2)->data[index2];
      dotimespL(count,count,
        { if (!equalp(*ptr1++,*ptr2++)) goto no; });
      return TRUE;
      no: return FALSE;
    }}
  local boolean elt_compare_T_Char(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const object* ptr1 = &TheSvector(dv1)->data[index1];
      SstringDispatch(dv2,
        { var const chart* ptr2 = &TheSstring(dv2)->data[index2];
          dotimespL(count,count,
            { var object elt1 = *ptr1++;
              var chart elt2 = *ptr2++;
              if (!(charp(elt1) && chareq(up_case(char_code(elt1)),up_case(elt2))))
                goto no;
            });
        },
        { var const scint* ptr2 = &TheSmallSstring(dv2)->data[index2];
          dotimespL(count,count,
            { var object elt1 = *ptr1++;
              var chart elt2 = as_chart(*ptr2++);
              if (!(charp(elt1) && chareq(up_case(char_code(elt1)),up_case(elt2))))
                goto no;
            });
        }
        );
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_T_Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var const uintB* ptr2 = &TheSbvector(dv2)->data[index2/8];
      dotimespL(count,count,
        { var object elt1 = *ptr1++;
          var uintB elt2 = (*ptr2 >> ((~index2)%8)) & (bit(1)-1);
          if (!eq(elt1,fixnum(elt2))) goto no;
          index2++;
          ptr2 += ((index2%8)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_T_2Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var const uintB* ptr2 = &TheSbvector(TheIarray(dv2)->data)->data[index2/4];
      dotimespL(count,count,
        { var object elt1 = *ptr1++;
          var uintB elt2 = (*ptr2 >> (2*((~index2)%4))) & (bit(2)-1);
          if (!eq(elt1,fixnum(elt2))) goto no;
          index2++;
          ptr2 += ((index2%4)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_T_4Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var const uintB* ptr2 = &TheSbvector(TheIarray(dv2)->data)->data[index2/2];
      dotimespL(count,count,
        { var object elt1 = *ptr1++;
          var uintB elt2 = (*ptr2 >> (4*((~index2)%2))) & (bit(4)-1);
          if (!eq(elt1,fixnum(elt2))) goto no;
          index2++;
          ptr2 += ((index2%2)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_T_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var const uintB* ptr2 = &TheSbvector(TheIarray(dv2)->data)->data[index2];
      dotimespL(count,count,
        { var object elt1 = *ptr1++;
          var uintB elt2 = *ptr2++;
          if (!eq(elt1,fixnum(elt2))) goto no;
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_T_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var const uint16* ptr2 = &((uint16*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { var object elt1 = *ptr1++;
          var uint16 elt2 = *ptr2++;
          if (!eq(elt1,fixnum(elt2))) goto no;
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_T_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const object* ptr1 = &TheSvector(dv1)->data[index1];
      var const uint32* ptr2 = &((uint32*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { var object elt1 = *ptr1++;
          var uint32 elt2 = *ptr2++;
          if (!(uint32_p(elt1) && (I_to_uint32(elt1) == elt2)))
            goto no;
        });
      return TRUE;
      no: return FALSE;
    }
  #define elt_compare_Char_Char(dv1,index1,dv2,index2,count)  \
    string_eqcomp_ci(dv1,index1,dv2,index2,count)
  #define elt_compare_Bit_Bit(dv1,index1,dv2,index2,count)  \
    bit_compare(dv1,index1,dv2,index2,count)
  local boolean elt_compare_Bit_2Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var const uintB* ptr2 = &TheSbvector(TheIarray(dv2)->data)->data[index2/4];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
          var uintB elt2 = (*ptr2 >> (2*((~index2)%4))) & (bit(2)-1);
          if (!(elt1 == elt2)) goto no;
          index1++;
          ptr1 += ((index1%8)==0);
          index2++;
          ptr2 += ((index2%4)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_Bit_4Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var const uintB* ptr2 = &TheSbvector(TheIarray(dv2)->data)->data[index2/2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
          var uintB elt2 = (*ptr2 >> (4*((~index2)%2))) & (bit(4)-1);
          if (!(elt1 == elt2)) goto no;
          index1++;
          ptr1 += ((index1%8)==0);
          index2++;
          ptr2 += ((index2%2)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_Bit_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var const uintB* ptr2 = &TheSbvector(TheIarray(dv2)->data)->data[index2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
          if (!(elt1 == *ptr2++)) goto no;
          index1++;
          ptr1 += ((index1%8)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var const uint16* ptr2 = &((uint16*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
          if (!(elt1 == *ptr2++)) goto no;
          index1++;
          ptr1 += ((index1%8)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var const uint32* ptr2 = &((uint32*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
          if (!(elt1 == *ptr2++)) goto no;
          index1++;
          ptr1 += ((index1%8)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  #define elt_compare_2Bit_2Bit(dv1,index1,dv2,index2,count)  \
    bit_compare(TheIarray(dv1)->data,index1<<1,TheIarray(dv2)->data,index2<<1,count<<1)
  local boolean elt_compare_2Bit_4Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(TheIarray(dv1)->data)->data[index1/4];
      var const uintB* ptr2 = &TheSbvector(TheIarray(dv2)->data)->data[index2/2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
          var uintB elt2 = (*ptr2 >> (4*((~index2)%2))) & (bit(4)-1);
          if (!(elt1 == elt2)) goto no;
          index1++;
          ptr1 += ((index1%4)==0);
          index2++;
          ptr2 += ((index2%2)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_2Bit_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(TheIarray(dv1)->data)->data[index1/4];
      var const uintB* ptr2 = &TheSbvector(TheIarray(dv2)->data)->data[index2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
          if (!(elt1 == *ptr2++)) goto no;
          index1++;
          ptr1 += ((index1%4)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_2Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(TheIarray(dv1)->data)->data[index1/4];
      var const uint16* ptr2 = &((uint16*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
          if (!(elt1 == *ptr2++)) goto no;
          index1++;
          ptr1 += ((index1%4)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_2Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(TheIarray(dv1)->data)->data[index1/4];
      var const uint32* ptr2 = &((uint32*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
          if (!(elt1 == *ptr2++)) goto no;
          index1++;
          ptr1 += ((index1%4)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  #define elt_compare_4Bit_4Bit(dv1,index1,dv2,index2,count)  \
    bit_compare(TheIarray(dv1)->data,index1<<2,TheIarray(dv2)->data,index2<<2,count<<2)
  local boolean elt_compare_4Bit_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(TheIarray(dv1)->data)->data[index1/2];
      var const uintB* ptr2 = &TheSbvector(TheIarray(dv2)->data)->data[index2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
          if (!(elt1 == *ptr2++)) goto no;
          index1++;
          ptr1 += ((index1%2)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_4Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(TheIarray(dv1)->data)->data[index1/2];
      var const uint16* ptr2 = &((uint16*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
          if (!(elt1 == *ptr2++)) goto no;
          index1++;
          ptr1 += ((index1%2)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_4Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(TheIarray(dv1)->data)->data[index1/2];
      var const uint32* ptr2 = &((uint32*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { var uintB elt1 = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
          if (!(elt1 == *ptr2++)) goto no;
          index1++;
          ptr1 += ((index1%2)==0);
        });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_8Bit_8Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(TheIarray(dv1)->data)->data[index1];
      var const uintB* ptr2 = &TheSbvector(TheIarray(dv2)->data)->data[index2];
      dotimespL(count,count,
        { if (!(*ptr1++ == *ptr2++)) goto no; });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_8Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(TheIarray(dv1)->data)->data[index1];
      var const uint16* ptr2 = &((uint16*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { if (!(*ptr1++ == *ptr2++)) goto no; });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_8Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uintB* ptr1 = &TheSbvector(TheIarray(dv1)->data)->data[index1];
      var const uint32* ptr2 = &((uint32*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { if (!(*ptr1++ == *ptr2++)) goto no; });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_16Bit_16Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uint16* ptr1 = &((uint16*)&TheSbvector(TheIarray(dv1)->data)->data[0])[index1];
      var const uint16* ptr2 = &((uint16*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { if (!(*ptr1++ == *ptr2++)) goto no; });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_16Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uint16* ptr1 = &((uint16*)&TheSbvector(TheIarray(dv1)->data)->data[0])[index1];
      var const uint32* ptr2 = &((uint32*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { if (!(*ptr1++ == *ptr2++)) goto no; });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare_32Bit_32Bit(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { var const uint32* ptr1 = &((uint32*)&TheSbvector(TheIarray(dv1)->data)->data[0])[index1];
      var const uint32* ptr2 = &((uint32*)&TheSbvector(TheIarray(dv2)->data)->data[0])[index2];
      dotimespL(count,count,
        { if (!(*ptr1++ == *ptr2++)) goto no; });
      return TRUE;
      no: return FALSE;
    }
  local boolean elt_compare(dv1,index1,dv2,index2,count)
    var object dv1;
    var uintL index1;
    var object dv2;
    var uintL index2;
    var uintL count;
    { switch (Array_type(dv1))
        { case Array_type_svector: # Simple-Vector
            switch (Array_type(dv2))
              { case Array_type_svector: # Simple-Vector
                  return elt_compare_T_T(dv1,index1,dv2,index2,count);
                case Array_type_sbvector: # Simple-Bit-Vector
                  return elt_compare_T_Bit(dv1,index1,dv2,index2,count);
                case Array_type_sstring: # Simple-String
                  return elt_compare_T_Char(dv1,index1,dv2,index2,count);
                case Array_type_bvector: # Byte-Vector
                  switch (Iarray_flags(dv2) /* & arrayflags_atype_mask */ )
                    { case Atype_2Bit:
                        return elt_compare_T_2Bit(dv1,index1,dv2,index2,count);
                      case Atype_4Bit:
                        return elt_compare_T_4Bit(dv1,index1,dv2,index2,count);
                      case Atype_8Bit:
                        return elt_compare_T_8Bit(dv1,index1,dv2,index2,count);
                      case Atype_16Bit:
                        return elt_compare_T_16Bit(dv1,index1,dv2,index2,count);
                      case Atype_32Bit:
                        return elt_compare_T_32Bit(dv1,index1,dv2,index2,count);
                      default: NOTREACHED
                    }
                default: NOTREACHED
              }
          case Array_type_sbvector: # Simple-Bit-Vector
            switch (Array_type(dv2))
              { case Array_type_svector: # Simple-Vector
                  return elt_compare_T_Bit(dv2,index2,dv1,index1,count);
                case Array_type_sbvector: # Simple-Bit-Vector
                  return elt_compare_Bit_Bit(dv1,index1,dv2,index2,count);
                case Array_type_sstring: # Simple-String
                  return FALSE; # because count > 0
                case Array_type_bvector: # Byte-Vector
                  switch (Iarray_flags(dv2) /* & arrayflags_atype_mask */ )
                    { case Atype_2Bit:
                        return elt_compare_Bit_2Bit(dv1,index1,dv2,index2,count);
                      case Atype_4Bit:
                        return elt_compare_Bit_4Bit(dv1,index1,dv2,index2,count);
                      case Atype_8Bit:
                        return elt_compare_Bit_8Bit(dv1,index1,dv2,index2,count);
                      case Atype_16Bit:
                        return elt_compare_Bit_16Bit(dv1,index1,dv2,index2,count);
                      case Atype_32Bit:
                        return elt_compare_Bit_32Bit(dv1,index1,dv2,index2,count);
                      default: NOTREACHED
                    }
                default: NOTREACHED
              }
          case Array_type_sstring: # Simple-String
            switch (Array_type(dv2))
              { case Array_type_svector: # Simple-Vector
                  return elt_compare_T_Char(dv2,index2,dv1,index1,count);
                case Array_type_sbvector: # Simple-Bit-Vector
                  return FALSE; # because count > 0
                case Array_type_sstring: # Simple-String
                  return elt_compare_Char_Char(dv1,index1,dv2,index2,count);
                case Array_type_bvector: # Byte-Vector
                  return FALSE; # because count > 0
                default: NOTREACHED
              }
          case Array_type_bvector: # Byte-Vector
            switch (Iarray_flags(dv1) /* & arrayflags_atype_mask */ )
              { case Atype_2Bit:
                  switch (Array_type(dv2))
                    { case Array_type_svector: # Simple-Vector
                        return elt_compare_T_2Bit(dv2,index2,dv1,index1,count);
                      case Array_type_sbvector: # Simple-Bit-Vector
                        return elt_compare_Bit_2Bit(dv2,index2,dv1,index1,count);
                      case Array_type_sstring: # Simple-String
                        return FALSE; # because count > 0
                      case Array_type_bvector: # Byte-Vector
                        switch (Iarray_flags(dv2) /* & arrayflags_atype_mask */ )
                          { case Atype_2Bit:
                              return elt_compare_2Bit_2Bit(dv1,index1,dv2,index2,count);
                            case Atype_4Bit:
                              return elt_compare_2Bit_4Bit(dv1,index1,dv2,index2,count);
                            case Atype_8Bit:
                              return elt_compare_2Bit_8Bit(dv1,index1,dv2,index2,count);
                            case Atype_16Bit:
                              return elt_compare_2Bit_16Bit(dv1,index1,dv2,index2,count);
                            case Atype_32Bit:
                              return elt_compare_2Bit_32Bit(dv1,index1,dv2,index2,count);
                            default: NOTREACHED
                          }
                      default: NOTREACHED
                    }
                case Atype_4Bit:
                  switch (Array_type(dv2))
                    { case Array_type_svector: # Simple-Vector
                        return elt_compare_T_4Bit(dv2,index2,dv1,index1,count);
                      case Array_type_sbvector: # Simple-Bit-Vector
                        return elt_compare_Bit_4Bit(dv2,index2,dv1,index1,count);
                      case Array_type_sstring: # Simple-String
                        return FALSE; # because count > 0
                      case Array_type_bvector: # Byte-Vector
                        switch (Iarray_flags(dv2) /* & arrayflags_atype_mask */ )
                          { case Atype_2Bit:
                              return elt_compare_2Bit_4Bit(dv2,index2,dv1,index1,count);
                            case Atype_4Bit:
                              return elt_compare_4Bit_4Bit(dv1,index1,dv2,index2,count);
                            case Atype_8Bit:
                              return elt_compare_4Bit_8Bit(dv1,index1,dv2,index2,count);
                            case Atype_16Bit:
                              return elt_compare_4Bit_16Bit(dv1,index1,dv2,index2,count);
                            case Atype_32Bit:
                              return elt_compare_4Bit_32Bit(dv1,index1,dv2,index2,count);
                            default: NOTREACHED
                          }
                      default: NOTREACHED
                    }
                case Atype_8Bit:
                  switch (Array_type(dv2))
                    { case Array_type_svector: # Simple-Vector
                        return elt_compare_T_8Bit(dv2,index2,dv1,index1,count);
                      case Array_type_sbvector: # Simple-Bit-Vector
                        return elt_compare_Bit_8Bit(dv2,index2,dv1,index1,count);
                      case Array_type_sstring: # Simple-String
                        return FALSE; # because count > 0
                      case Array_type_bvector: # Byte-Vector
                        switch (Iarray_flags(dv2) /* & arrayflags_atype_mask */ )
                          { case Atype_2Bit:
                              return elt_compare_2Bit_8Bit(dv2,index2,dv1,index1,count);
                            case Atype_4Bit:
                              return elt_compare_4Bit_8Bit(dv2,index2,dv1,index1,count);
                            case Atype_8Bit:
                              return elt_compare_8Bit_8Bit(dv1,index1,dv2,index2,count);
                            case Atype_16Bit:
                              return elt_compare_8Bit_16Bit(dv1,index1,dv2,index2,count);
                            case Atype_32Bit:
                              return elt_compare_8Bit_32Bit(dv1,index1,dv2,index2,count);
                            default: NOTREACHED
                          }
                      default: NOTREACHED
                    }
                case Atype_16Bit:
                  switch (Array_type(dv2))
                    { case Array_type_svector: # Simple-Vector
                        return elt_compare_T_16Bit(dv2,index2,dv1,index1,count);
                      case Array_type_sbvector: # Simple-Bit-Vector
                        return elt_compare_Bit_16Bit(dv2,index2,dv1,index1,count);
                      case Array_type_sstring: # Simple-String
                        return FALSE; # because count > 0
                      case Array_type_bvector: # Byte-Vector
                        switch (Iarray_flags(dv2) /* & arrayflags_atype_mask */ )
                          { case Atype_2Bit:
                              return elt_compare_2Bit_16Bit(dv2,index2,dv1,index1,count);
                            case Atype_4Bit:
                              return elt_compare_4Bit_16Bit(dv2,index2,dv1,index1,count);
                            case Atype_8Bit:
                              return elt_compare_8Bit_16Bit(dv2,index2,dv1,index1,count);
                            case Atype_16Bit:
                              return elt_compare_16Bit_16Bit(dv1,index1,dv2,index2,count);
                            case Atype_32Bit:
                              return elt_compare_16Bit_32Bit(dv1,index1,dv2,index2,count);
                            default: NOTREACHED
                          }
                      default: NOTREACHED
                    }
                case Atype_32Bit:
                  switch (Array_type(dv2))
                    { case Array_type_svector: # Simple-Vector
                        return elt_compare_T_32Bit(dv2,index2,dv1,index1,count);
                      case Array_type_sbvector: # Simple-Bit-Vector
                        return elt_compare_Bit_32Bit(dv2,index2,dv1,index1,count);
                      case Array_type_sstring: # Simple-String
                        return FALSE; # because count > 0
                      case Array_type_bvector: # Byte-Vector
                        switch (Iarray_flags(dv2) /* & arrayflags_atype_mask */ )
                          { case Atype_2Bit:
                              return elt_compare_2Bit_32Bit(dv2,index2,dv1,index1,count);
                            case Atype_4Bit:
                              return elt_compare_4Bit_32Bit(dv2,index2,dv1,index1,count);
                            case Atype_8Bit:
                              return elt_compare_8Bit_32Bit(dv2,index2,dv1,index1,count);
                            case Atype_16Bit:
                              return elt_compare_16Bit_32Bit(dv2,index2,dv1,index1,count);
                            case Atype_32Bit:
                              return elt_compare_32Bit_32Bit(dv1,index1,dv2,index2,count);
                            default: NOTREACHED
                          }
                      default: NOTREACHED
                    }
                default: NOTREACHED
              }
          default: NOTREACHED
    }   }
  # Now EQUALP itself.
  global boolean equalp(obj1,obj2)
    var object obj1;
    var object obj2;
    { start:
      if (eq(obj1,obj2)) { return TRUE; } # (EQ x y) ==> (EQUALP x y)
      # Fallunterscheidung nach dem Typ von obj1:
      if (consp(obj1))
        { if (!consp(obj2)) goto no;
          # Conses rekursiv vergleichen:
          # CAR und CDR müssen übereinstimmen:
          # (and (equalp (car obj1) (car obj2)) (equalp (cdr obj1) (cdr obj2)))
          check_SP();
          if (!equalp(Car(obj1),Car(obj2))) goto no;
          # return equalp(Cdr(obj1),Cdr(obj2));
          obj1 = Cdr(obj1); obj2 = Cdr(obj2);
          goto start;
        }
      elif (symbolp(obj1)) # Symbol ?
        { goto no; } # ja -> hätte schon EQ zu obj2 sein müssen
      elif (numberp(obj1))
        { if (!numberp(obj2)) goto no;
          # Zahlen mit = vergleichen
          return number_gleich(obj1,obj2);
        }
      else
        {
          #ifdef TYPECODES
          switch (typecode(obj1))
          #else
          if (orecordp(obj1)) { goto case_orecord; }
          elif (charp(obj1)) { goto case_char; }
          else { goto no; }
          switch (0)
          #endif
            { case_bvector: # Bit/Byte-Vektor
              case_string: # String
              case_vector: # (VECTOR T)
                if (!vectorp(obj2)) goto no;
                # obj1, obj2 beide Vektoren.
                # Längen vergleichen:
                { var uintL len1 = vector_length(obj1);
                  if (!(len1 == vector_length(obj2))) goto no;
                  # Inhalt vergleichen:
                  if (len1 > 0)
                    { var uintL index1 = 0; # Start-Index in den Datenvektor von obj1
                      var uintL index2 = 0; # Start-Index in den Datenvektor von obj1
                      var object dv1 = array_displace_check(obj1,len1,&index1);
                      var object dv2 = array_displace_check(obj2,len1,&index2);
                      # dvi ist der Datenvektor, indexi der Index in den Datenvektor
                      # zu obji (i=1,2).
                      return elt_compare(dv1,index1,dv2,index2,len1);
                    }
                    else
                    { return TRUE; }
                }
              case_mdarray: # Array vom Rang /=1
                if (!mdarrayp(obj2)) goto no;
                # obj1 und obj2 sind Arrays vom Rang /=1.
                # Ihr Rang und ihre Dimensionen müssen übereinstimmen, und
                # die Elemente werden dann wie bei Vektoren verglichen.
                { # Ränge vergleichen:
                  var uintC rank1 = Iarray_rank(obj1);
                  if (!(rank1 == Iarray_rank(obj2))) goto no;
                  # Dimensionen vergleichen:
                  if (rank1 > 0)
                    { var uintL* dimptr1 = &TheIarray(obj1)->dims[0];
                      if (Iarray_flags(obj1) & bit(arrayflags_dispoffset_bit))
                        dimptr1++;
                     {var uintL* dimptr2 = &TheIarray(obj2)->dims[0];
                      if (Iarray_flags(obj2) & bit(arrayflags_dispoffset_bit))
                        dimptr2++;
                      dotimespC(rank1,rank1,
                        { if (!(*dimptr1++ == *dimptr2++)) goto no; }
                        );
                }   }}
                # Inhalt vergleichen:
                { var uintL len1 = TheIarray(obj1)->totalsize;
                  # muss als Produkt der Dimensionen auch = TheIarray(obj2)->totalsize sein.
                  if (len1 > 0)
                    { var uintL index1 = 0; var uintL index2 = 0;
                      var object dv1 = iarray_displace_check(obj1,len1,&index1);
                      var object dv2 = iarray_displace_check(obj2,len1,&index2);
                      # dvi ist der Datenvektor, indexi der Index in den Datenvektor
                      # zu obji (i=1,2).
                      return elt_compare(dv1,index1,dv2,index2,len1);
                    }
                    else
                    { return TRUE; }
                }
              #ifdef TYPECODES
              _case_structure
              _case_stream
              #endif
              case_orecord:
                # Record, Structure, aber nicht Closure, Instance.
                # obj2 muss vom selben Typ wie obj1, also ein Record, sein
                # und in rectype und recflags und reclength mit obj1 überein-
                # stimmen, und alle Komponenten müssen EQUALP sein.
                switch (Record_type(obj1))
                  { case_Rectype_bvector_above;
                    case_Rectype_string_above;
                    case_Rectype_vector_above;
                    case_Rectype_mdarray_above;
                    case_Rectype_Closure_above;
                    case_Rectype_Instance_above;
                    default: ;
                  }
                #ifdef TYPECODES
                if (!(typecode(obj1) == typecode(obj2))) goto no;
                #endif
                # obj1 und obj2 beide Records.
                { var uintC len;
                  if (!(Record_flags(obj1) == Record_flags(obj2))) goto no;
                  if (!(Record_type(obj1) == Record_type(obj2))) goto no;
                  if (Record_type(obj1) < rectype_limit)
                    { if (!((len = Srecord_length(obj1)) == Srecord_length(obj2))) goto no; }
                    else
                    { if (!((len = Xrecord_length(obj1)) == Xrecord_length(obj2))) goto no;
                      if (!(Xrecord_xlength(obj1) == Xrecord_xlength(obj2))) goto no;
                    }
                  # rekursiv die Elemente vergleichen (auch bei PATHNAMEs):
                  check_SP();
                  if (len > 0)
                    { var object* ptr1 = &TheRecord(obj1)->recdata[0];
                      var object* ptr2 = &TheRecord(obj2)->recdata[0];
                      var uintC count;
                      dotimespC(count,len,
                        { if (!equalp(*ptr1++,*ptr2++)) goto no; });
                    }
                  # Die recxlength Extra-Elemente auch vergleichen:
                  if (Record_type(obj1) >= rectype_limit)
                    { var uintC xlen = Xrecord_xlength(obj1);
                      if (xlen > 0)
                        { var uintB* ptr1 = (uintB*)&TheRecord(obj1)->recdata[len];
                          var uintB* ptr2 = (uintB*)&TheRecord(obj2)->recdata[len];
                          dotimespC(xlen,xlen, { if (!(*ptr1++ == *ptr2++)) goto no; } );
                    }   }
                }
                return TRUE;
              case_closure: # Closure
                goto no; # hätte schon EQ sein müssen
              case_instance: # Instance
                goto no; # hätte schon EQ sein müssen
              case_char:
                # Character
                if (!charp(obj2)) goto no;
                # obj1, obj2 beide Characters.
                # Wie mit CHAR-EQUAL vergleichen: Bits und Font ignorieren,
                # in Großbuchstaben umwandeln und dann vergleichen.
                if (chareq(up_case(char_code(obj1)),up_case(char_code(obj2))))
                  return TRUE;
                  else
                  goto no;
              #ifdef TYPECODES
              case_subr: # SUBR
                goto no; # hätte schon EQ sein müssen
              case_system: # SYSTEM, Read-Label, FRAME-Pointer
                goto no; # hätte schon EQ sein müssen
              case_machine: # Maschinenpointer
                goto no; # hätte schon EQ sein müssen
              #endif
              default: NOTREACHED
        }   }
      no: return FALSE;
    }

LISPFUNN(eq,2)
# (EQ obj1 obj2), CLTL S. 77
  { var object obj2 = popSTACK();
    var object obj1 = popSTACK();
    value1 = (eq(obj1,obj2) ? T : NIL); mv_count=1;
  }

LISPFUNN(eql,2)
# (EQL obj1 obj2), CLTL S. 78
  { var object obj2 = popSTACK();
    var object obj1 = popSTACK();
    value1 = (eql(obj1,obj2) ? T : NIL); mv_count=1;
  }

LISPFUNN(equal,2)
# (EQUAL obj1 obj2), CLTL S. 80
  { var object obj2 = popSTACK();
    var object obj1 = popSTACK();
    value1 = (equal(obj1,obj2) ? T : NIL); mv_count=1;
  }

LISPFUNN(equalp,2)
# (EQUALP obj1 obj2), CLTL S. 81
  { var object obj2 = popSTACK();
    var object obj1 = popSTACK();
    value1 = (equalp(obj1,obj2) ? T : NIL); mv_count=1;
  }

LISPFUNN(consp,1)
# (CONSP object), CLTL S. 74
  { value1 = (mconsp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(atom,1)
# (ATOM object), CLTL S. 73
  { value1 = (matomp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(symbolp,1)
# (SYMBOLP object), CLTL S. 73
  { value1 = (symbolp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(stringp,1)
# (STRINGP object), CLTL S. 75
  { value1 = (stringp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(numberp,1)
# (NUMBERP object), CLTL S. 74
  { value1 = (numberp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(compiled_function_p,1)
# (COMPILED-FUNCTION-P object), CLTL S. 76
  { var object arg = popSTACK();
    # Test auf SUBR oder compilierte Closure oder Foreign-Function:
    value1 = (subrp(arg) || cclosurep(arg) || ffunctionp(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(null,1)
# (NULL object), CLTL S. 73
  { value1 = (nullp(popSTACK()) ? T : NIL); mv_count=1; }

LISPFUNN(not,1)
# (NOT object), CLTL S. 82
  { value1 = (nullp(popSTACK()) ? T : NIL); mv_count=1; }

LISPFUNN(closurep,1)
# (SYS::CLOSUREP object)
  { value1 = (closurep(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(listp,1)
# (LISTP object), CLTL S. 74
  { var object arg = popSTACK();
    value1 = (listp(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(integerp,1)
# (INTEGERP object), CLTL S. 74
  { value1 = (integerp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(fixnump,1)
# (SYS::FIXNUMP object)
  { value1 = (fixnump(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(rationalp,1)
# (RATIONALP object), CLTL S. 74
  { var object arg = popSTACK();
    if_rationalp(arg, value1 = T; , value1 = NIL; ); mv_count=1;
  }

LISPFUNN(floatp,1)
# (FLOATP object), CLTL S. 75
  { value1 = (floatp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(short_float_p,1)
# (SYS::SHORT-FLOAT-P object)
  { value1 = (short_float_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(single_float_p,1)
# (SYS::SINGLE-FLOAT-P object)
  { value1 = (single_float_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(double_float_p,1)
# (SYS::DOUBLE-FLOAT-P object)
  { value1 = (double_float_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(long_float_p,1)
# (SYS::LONG-FLOAT-P object)
  { value1 = (long_float_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(realp,1)
# (REALP object), CLTL2 S. 101
  { var object arg = popSTACK();
    if_realp(arg, value1 = T; , value1 = NIL; ); mv_count=1;
  }

LISPFUNN(complexp,1)
# (COMPLEXP object), CLTL S. 75
  { value1 = (complexp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(streamp,1)
# (STREAMP object), CLTL S. 332
  { value1 = (streamp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(built_in_stream_p,1)
# (SYS::BUILT-IN-STREAM-P object)
  { value1 = (builtin_stream_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(random_state_p,1)
# (RANDOM-STATE-P object), CLTL S. 231
  { var object arg = popSTACK();
    value1 = (random_state_p(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(readtablep,1)
# (READTABLEP object), CLTL S. 361
  { var object arg = popSTACK();
    value1 = (readtablep(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(hash_table_p,1)
# (HASH-TABLE-P object), CLTL S. 284
  { var object arg = popSTACK();
    value1 = (hash_table_p(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(pathnamep,1)
# (PATHNAMEP object), CLTL S. 416
  { var object arg = popSTACK();
    value1 = (xpathnamep(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(logical_pathname_p,1)
# (SYS::LOGICAL-PATHNAME-P object)
  { var object arg = popSTACK();
    value1 = (logpathnamep(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(characterp,1)
# (CHARACTERP object), CLTL S. 75
  { value1 = (charp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(functionp,1)
# (FUNCTIONP object), CLTL S. 76, CLtL2 S. 102-103
  { var object arg = popSTACK();
    # Test auf SUBR, Closure, Foreign-Function, [Symbol, Cons (LAMBDA . ...)]:
    value1 = (subrp(arg) || closurep(arg) || ffunctionp(arg) ? T : NIL);
    mv_count=1;
  }

LISPFUNN(generic_function_p,1)
# (CLOS::GENERIC-FUNCTION-P object)
  { var object arg = popSTACK();
    value1 = (genericfunctionp(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(packagep,1)
# (PACKAGEP object), CLTL S. 76
  { var object arg = popSTACK();
    value1 = (packagep(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(arrayp,1)
# (ARRAYP object), CLTL S. 76
  { var object arg = popSTACK();
    value1 = (arrayp(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(simple_array_p,1)
# (SYSTEM::SIMPLE-ARRAY-P object)
  { var object arg = popSTACK();
    if (simplep(arg))
      { goto yes; } # Simple eindimensionale Arrays -> ja
      else
      { if (arrayp(arg)) # sonstige Arrays, nur falls alle Flagbits =0 sind
          { if ((Iarray_flags(arg)
                 & (  bit(arrayflags_adjustable_bit)
                    | bit(arrayflags_fillp_bit)
                    | bit(arrayflags_displaced_bit)
                    | bit(arrayflags_dispoffset_bit)
                )  )
                == 0
               )
              { yes: value1 = T; }
              else
              goto no; # nicht-simple Arrays -> nein
          }
          else
          { no: value1 = NIL; } # sonstige Objekte -> nein
      }
    mv_count=1;
  }

LISPFUNN(bit_vector_p,1)
# (BIT-VECTOR-P object), CLTL S. 75
  { value1 = (bit_vector_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(vectorp,1)
# (VECTORP object), CLTL S. 75
  { value1 = (vectorp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(simple_vector_p,1)
# (SIMPLE-VECTOR-P object), CLTL S. 75
  { value1 = (simple_vector_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(simple_string_p,1)
# (SIMPLE-STRING-P object), CLTL S. 75
  { value1 = (simple_string_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(simple_bit_vector_p,1)
# (SIMPLE-BIT-VECTOR-P object), CLTL S. 76
  { value1 = (simple_bit_vector_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(commonp,1)
# (COMMONP object), CLTL S. 76
  { var object arg = popSTACK();
    # Fallunterscheidung nach Typ:
    #ifdef TYPECODES
    switch (typecode(arg))
    #else
    if (orecordp(arg)) { goto case_orecord; }
    elif (consp(arg)) { goto case_cons; }
    elif (immediate_number_p(arg)) { goto case_number; }
    elif (charp(arg)) { goto case_char; }
    else switch (0)
    #endif
      { case_cons: goto yes; # Conses ja
        case_symbol: goto yes; # Symbole ja
        case_number: goto yes; # Zahlen ja
        case_array: goto yes; # Arrays ja
        case_structure: goto yes; # Structures ja
        case_stream: goto yes; # Streams ja
        case_char: # Character: nur Standard-Char
          # (STANDARD-CHAR-P object) als Wert:
          pushSTACK(arg); funcall(L(standard_char_p),1); return;
        case_orecord: # sonstige Records:
          # nur Package, Hash-Table, Readtable, Pathname, Random-State [,Structure, Stream]
          switch (Record_type(arg))
            { case_Rectype_Symbol_above;
              case_Rectype_number_above;
              case_Rectype_array_above;
              case_Rectype_Structure_above;
              case_Rectype_Stream_above;
              case Rectype_Hashtable:
              case Rectype_Package:
              case Rectype_Readtable:
              case Rectype_Pathname:
              #ifdef LOGICAL_PATHNAMES
              case Rectype_Logpathname:
              #endif
              case Rectype_Random_State:
                goto yes;
              default:
                goto no;
            }
        default: goto no;
      }
     no: value1 = NIL; mv_count=1; return;
    yes: value1 = T; mv_count=1; return;
  }

LISPFUNN(type_of,1)
# (TYPE-OF object), CLTL S. 52
  { var object arg = popSTACK();
    #ifdef TYPECODES
    switch (typecode(arg))
    #else
    if (orecordp(arg)) { goto case_orecord; }
    elif (consp(arg)) { goto case_cons; }
    elif (subrp(arg)) { goto case_subr; }
    elif (charp(arg)) { goto case_char; }
    elif (fixnump(arg)) { goto case_fixnum; }
    elif (short_float_p(arg)) { goto case_sfloat; }
    elif (machinep(arg)) { goto case_machine; }
    elif (read_label_p(arg)) { goto case_read_label; }
    elif (systemp(arg)) { goto case_system; }
    else { goto unknown; }
    switch (0)
    #endif
      { case_cons: # Cons -> CONS
          value1 = S(cons); break;
        case_symbol: # Symbol -> SYMBOL oder NULL oder BOOLEAN
          value1 = (nullp(arg) ? S(null) :
                    eq(arg,T) ? S(boolean) :
                    S(symbol));
          break;
        case_machine: # Maschinenpointer -> ADDRESS
          # (If not TYPECODES, ADDRESS and FRAME-POINTER are not distinguishable.)
          value1 = S(address); break;
        case_sbvector: # Simple-Bit-Vector -> (SIMPLE-BIT-VECTOR dim0)
          pushSTACK(S(simple_bit_vector)); goto vectors;
        case_sstring: # Simple-String -> (SIMPLE-[BASE-]STRING dim0)
          #if (base_char_code_limit == char_code_limit)
          pushSTACK(S(simple_base_string)); goto vectors;
          #else
          pushSTACK(S(simple_string)); goto vectors;
          #endif
        case_svector: # Simple-Vector -> (SIMPLE-VECTOR dim0)
          pushSTACK(S(simple_vector)); goto vectors;
        case_ostring: # sonstiger String -> ([BASE-]STRING dim0)
          #if (base_char_code_limit == char_code_limit)
          pushSTACK(S(base_string)); goto vectors;
          #else
          pushSTACK(S(string)); goto vectors;
          #endif
        vectors: # Typ des Vektors in STACK_0
          pushSTACK(array_dimensions(arg)); # Dimensionsliste
          {var object new_cons = allocate_cons();
           Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK();
           value1 = new_cons;
          }
          break;
        case_ovector: # sonstiger general-vector -> (VECTOR T dim0)
          pushSTACK(array_dimensions(arg)); # Dimensionenliste
          {var object new_cons = allocate_cons();
           Cdr(new_cons) = popSTACK(); Car(new_cons) = T;
           pushSTACK(new_cons);
          }
          {var object new_cons = allocate_cons();
           Cdr(new_cons) = popSTACK(); Car(new_cons) = S(vector);
           value1 = new_cons;
          }
          break;
        case_obvector: # sonstiger Bit-Vector -> (BIT-VECTOR dim0)
                       # sonstiger Byte-Vector -> ([SIMPLE-]ARRAY (UNSIGNED-BYTE n) (dim0))
          if ((Iarray_flags(arg) & arrayflags_atype_mask) == Atype_Bit)
            { pushSTACK(S(bit_vector)); goto vectors; }
        case_mdarray: # sonstiger Array -> ([SIMPLE-]ARRAY eltype dims)
          pushSTACK( ((Iarray_flags(arg)
                       & (  bit(arrayflags_adjustable_bit)
                          | bit(arrayflags_fillp_bit)
                          | bit(arrayflags_displaced_bit)
                          | bit(arrayflags_dispoffset_bit)
                      )  )
                      == 0
                     )
                     ? S(simple_array)
                     : S(array)
                   );
          pushSTACK(arg);
          pushSTACK(array_dimensions(arg)); # Dimensionenliste
          STACK_1 = array_element_type(STACK_1); # eltype
          value1 = listof(3);
          break;
        case_closure: # Closure -> FUNCTION
          value1 = S(function); break;
        case_structure: # Structure -> Typ der Structure
          {var object type = TheStructure(arg)->structure_types;
           # (name_1 ... name_i-1 name_i). Typ ist name_1.
           value1 = Car(type);
          }
          break;
        case_stream: # Stream -> STREAM oder je nach Stream-Typ
          switchu (TheStream(arg)->strmtype)
            { case strmtype_file:     value1 = S(file_stream); break;
              case strmtype_synonym:  value1 = S(synonym_stream); break;
              case strmtype_broad:    value1 = S(broadcast_stream); break;
              case strmtype_concat:   value1 = S(concatenated_stream); break;
              case strmtype_twoway:   value1 = S(two_way_stream); break;
              case strmtype_echo:     value1 = S(echo_stream); break;
              case strmtype_str_in:
              case strmtype_str_out:
              case strmtype_str_push: value1 = S(string_stream); break;
              default:                value1 = S(stream); break;
            }
          break;
        case_orecord: # OtherRecord -> PACKAGE, ...
          switch (Record_type(arg))
            { case_Rectype_Symbol_above;
              case_Rectype_Sbvector_above;
              case_Rectype_Sstring_above;
              case_Rectype_Svector_above;
              case_Rectype_ostring_above;
              case_Rectype_ovector_above;
              case_Rectype_obvector_above;
              case_Rectype_mdarray_above;
              case_Rectype_Closure_above;
              case_Rectype_Structure_above;
              case_Rectype_Stream_above;
              case_Rectype_Instance_above;
              case_Rectype_Bignum_above;
              case_Rectype_Ratio_above;
              case_Rectype_Ffloat_above;
              case_Rectype_Dfloat_above;
              case_Rectype_Lfloat_above;
              case_Rectype_Complex_above;
              case Rectype_Hashtable: # Hash-Table
                value1 = S(hash_table); break;
              case Rectype_Package: # Package
                value1 = S(package); break;
              case Rectype_Readtable: # Readtable
                value1 = S(readtable); break;
              case Rectype_Pathname: # Pathname
                value1 = S(pathname); break;
              #ifdef LOGICAL_PATHNAMES
              case Rectype_Logpathname: # Logical Pathname
                value1 = S(logical_pathname); break;
              #endif
              case Rectype_Random_State: # Random-State
                value1 = S(random_state); break;
              case Rectype_Byte: # Byte
                value1 = S(byte); break;
              case Rectype_Fsubr: # Fsubr -> SPECIAL-OPERATOR
                value1 = S(special_operator); break;
              case Rectype_Loadtimeeval: # Load-Time-Eval
                value1 = S(load_time_eval); break;
              case Rectype_Symbolmacro: # Symbol-Macro
                value1 = S(symbol_macro); break;
              case Rectype_Encoding: # Encoding
                value1 = S(encoding); break;
              #ifdef FOREIGN
              case Rectype_Fpointer: # Foreign-Pointer-Verpackung
                value1 = S(foreign_pointer); break;
              #endif
              #ifdef DYNAMIC_FFI
              case Rectype_Faddress: # Foreign-Adresse
                value1 = S(foreign_address); break;
              case Rectype_Fvariable: # Foreign-Variable
                value1 = S(foreign_variable); break;
              case Rectype_Ffunction: # Foreign-Function
                value1 = S(foreign_function); break;
              #endif
              case Rectype_Weakpointer: # Weak-Pointer
                value1 = S(weak_pointer); break;
              case Rectype_Finalizer: # Finalisierer (sollte nicht vorkommen)
                value1 = S(finalizer); break;
              #ifdef SOCKET_STREAMS
              case Rectype_Socket_Server: # Socket-Server
                value1 = S(socket_server); break;
              #endif
              #ifdef YET_ANOTHER_RECORD
              case Rectype_Yetanother: # Yetanother -> YET-ANOTHER
                value1 = S(yet_another); break;
              #endif
              default: goto unknown;
            }
          break;
        case_instance: # Instanz -> Name der Klasse oder Klasse selbst
          # (CLtL2 S. 781 oben)
          { var object clas = TheInstance(arg)->inst_class;
            var object name = TheClass(clas)->classname;
            value1 = (eq(get(name,S(closclass)),clas) # (GET name 'CLOS::CLOSCLASS) = class ?
                      ? name
                      : clas
                     );
          }
          break;
        case_char: # Character -> BASE-CHAR or CHARACTER
          #if (base_char_code_limit < char_code_limit)
          if (as_cint(char_code(arg)) >= base_char_code_limit)
            { value1 = S(character); break; }
          #endif
          value1 = S(base_char); break;
        case_subr: # SUBR -> COMPILED-FUNCTION
          value1 = S(compiled_function); break;
        #ifdef TYPECODES
        case_system: # -> FRAME-POINTER, READ-LABEL, SYSTEM-INTERNAL
          if (!wbit_test(as_oint(arg),0+oint_addr_shift))
            { value1 = S(frame_pointer); }
            else
            { if (!wbit_test(as_oint(arg),oint_data_len-1+oint_addr_shift))
                { value1 = S(read_label); }
                else
                { value1 = S(system_internal); }
            }
          break;
        #else
        case_read_label: # -> READ-LABEL
          value1 = S(read_label); break;
        case_system: # -> SYSTEM-INTERNAL
          value1 = S(system_internal); break;
        #endif
        case_fixnum: # Fixnum -> FIXNUM
          value1 = S(fixnum); break;
        case_bignum: # Bignum -> BIGNUM
          value1 = S(bignum); break;
        case_ratio: # Ratio -> RATIO
          value1 = S(ratio); break;
        case_sfloat: # Short-Float -> SHORT-FLOAT
          value1 = S(short_float); break;
        case_ffloat: # Single-Float -> SINGLE-FLOAT
          value1 = S(single_float); break;
        case_dfloat: # Double-Float -> DOUBLE-FLOAT
          value1 = S(double_float); break;
        case_lfloat: # Long-Float -> LONG-FLOAT
          value1 = S(long_float); break;
        case_complex: # Complex -> COMPLEX
          value1 = S(complex); break;
        default:
        unknown: # unbekannter Typ
          pushSTACK(S(type_of));
          fehler(serious_condition,
                 GETTEXT("~: unidentifiable type!!!")
                );
      }
    mv_count=1;
  }

LISPFUNN(defclos,2)
# (CLOS::%DEFCLOS class-type built-in-classes)
# setzt die für CLOS::CLASS-P und CLOS:CLASS-OF benötigten Daten.
  { # Für CLOS::CLASS-P :
    O(class_structure_types) = STACK_1;
    # Für CLOS:CLASS-OF :
    {var object* ptr1 = &TheSvector(STACK_0)->data[0];
     var object* ptr2 = &O(class_array);
     var uintC count;
     dotimesC(count,Svector_length(STACK_0), # = &O(class_vector)-&O(class_array)+1
      { *ptr2++ = *ptr1++; }
      );
    }
    value1 = NIL; mv_count=0; skipSTACK(2);
  }

LISPFUNN(class_p,1)
# (CLOS::CLASS-P object) testet, ob ein Objekt eine Klasse ist.
  { var object obj = popSTACK();
    if_classp(obj, { value1 = T; }, { value1 = NIL; }); mv_count=1;
  }

LISPFUNN(class_of,1)
# (CLOS:CLASS-OF object), CLTL2 S. 822,783
  { var object arg = popSTACK();
    #ifdef TYPECODES
    switch (typecode(arg))
    #else
    if (orecordp(arg)) { goto case_orecord; }
    elif (consp(arg)) { goto case_cons; }
    elif (subrp(arg)) { goto case_subr; }
    elif (charp(arg)) { goto case_char; }
    elif (fixnump(arg)) { goto case_integer; }
    elif (short_float_p(arg)) { goto case_float; }
    elif (machinep(arg)) { goto case_machine; }
    elif (read_label_p(arg)) { goto case_system; }
    elif (systemp(arg)) { goto case_system; }
    else { goto unknown; }
    switch (0)
    #endif
      { case_instance: # Instanz -> deren Klasse
          value1 = TheInstance(arg)->inst_class; break;
        case_structure: # Structure -> Typ der Structure oder <t>
          { var object type = TheStructure(arg)->structure_types;
            # (name_1 ... name_i-1 name_i). Typ ist name_1.
            while (consp(type))
              { var object name = Car(type);
                var object clas = get(name,S(closclass)); # (GET name 'CLOS::CLOSCLASS)
                if_classp(clas, { value1 = clas; goto fertig; }, ; );
                type = Cdr(type);
          }   }
          value1 = O(class_t); break;
        case_cons: # Cons -> <cons>
          value1 = O(class_cons); break;
        case_symbol: # Symbol -> <symbol> oder <null>
          value1 = (nullp(arg) ? O(class_null) : O(class_symbol)); break;
        case_sstring: case_ostring: # String -> <string>
          value1 = O(class_string); break;
        case_obvector: # sonstiger Bit-Vector -> <bit-vector>
                       # sonstiger Byte-Vector -> <vector>
          if ((Iarray_flags(arg) & arrayflags_atype_mask) == Atype_Bit)
            { case_sbvector: # Simple-Bit-Vector -> <bit-vector>
              value1 = O(class_bit_vector); break;
            }
        case_svector: case_ovector: # General-Vector -> <vector>
          value1 = O(class_vector); break;
        case_mdarray: # sonstiger Array -> <array>
          value1 = O(class_array); break;
        case_closure: # Closure -> <function> bzw. <standard-generic-function>
          if (genericfunctionp(arg))
            { value1 = O(class_standard_generic_function); break; }
        case_subr: # SUBR -> <function>
          value1 = O(class_function); break;
        case_stream: # Stream -> <stream> oder je nach Stream-Typ
          switchu (TheStream(arg)->strmtype)
            { case strmtype_file:     value1 = O(class_file_stream); break;
              case strmtype_synonym:  value1 = O(class_synonym_stream); break;
              case strmtype_broad:    value1 = O(class_broadcast_stream); break;
              case strmtype_concat:   value1 = O(class_concatenated_stream); break;
              case strmtype_twoway:   value1 = O(class_two_way_stream); break;
              case strmtype_echo:     value1 = O(class_echo_stream); break;
              case strmtype_str_in:
              case strmtype_str_out:
              case strmtype_str_push: value1 = O(class_string_stream); break;
              default:                value1 = O(class_stream); break;
            }
          break;
        case_orecord: # OtherRecord -> <package>, ...
          switch (Record_type(arg))
            { case_Rectype_Instance_above;
              case_Rectype_Structure_above;
              case_Rectype_Symbol_above;
              case_Rectype_Sstring_above;
              case_Rectype_ostring_above;
              case_Rectype_obvector_above;
              case_Rectype_Sbvector_above;
              case_Rectype_Svector_above;
              case_Rectype_ovector_above;
              case_Rectype_mdarray_above;
              case_Rectype_Closure_above;
              case_Rectype_Stream_above;
              case_Rectype_integer_above;
              case_Rectype_Ratio_above;
              case_Rectype_float_above;
              case_Rectype_Complex_above;
              case Rectype_Hashtable: # Hash-Table
                value1 = O(class_hash_table); break;
              case Rectype_Package: # Package
                value1 = O(class_package); break;
              case Rectype_Readtable: # Readtable
                value1 = O(class_readtable); break;
              case Rectype_Pathname: # Pathname
                value1 = O(class_pathname); break;
              #ifdef LOGICAL_PATHNAMES
              case Rectype_Logpathname: # Logical Pathname
                value1 = O(class_logical_pathname); break;
              #endif
              case Rectype_Random_State: # Random-State
                value1 = O(class_random_state); break;
              case Rectype_Byte: # Byte -> <t>
              case Rectype_Fsubr: # Fsubr -> <t>
              case Rectype_Loadtimeeval: # Load-Time-Eval -> <t>
              case Rectype_Symbolmacro: # Symbol-Macro -> <t>
              case Rectype_Encoding: # Encoding -> <t>
              #ifdef FOREIGN
              case Rectype_Fpointer: # Foreign-Pointer-Verpackung -> <t>
              #endif
              #ifdef DYNAMIC_FFI
              case Rectype_Faddress: # Foreign-Adresse -> <t>
              case Rectype_Fvariable: # Foreign-Variable -> <t>
              #endif
              case Rectype_Weakpointer: # Weak-Pointer -> <t>
              case Rectype_Finalizer: # Finalisierer -> <t>
              #ifdef SOCKET_STREAMS
              case Rectype_Socket_Server: # Socket-Server -> <t>
              #endif
                value1 = O(class_t); break;
              #ifdef DYNAMIC_FFI
              case Rectype_Ffunction: # Foreign-Function -> <function>
                value1 = O(class_function); break;
              #endif
              #ifdef YET_ANOTHER_RECORD
              case Rectype_Yetanother: # Yetanother -> <t>
                value1 = O(class_t); break;
              #endif
              default: goto unknown;
            }
          break;
        case_char: # Character -> <character>
          value1 = O(class_character); break;
        case_machine: # Maschinenpointer -> <t>
        case_system: # -> <t>
          value1 = O(class_t); break;
        case_integer: # Integer -> <integer>
          value1 = O(class_integer); break;
        case_ratio: # Ratio -> <ratio>
          value1 = O(class_ratio); break;
        case_float: # Float -> <float>
          value1 = O(class_float); break;
        case_complex: # Complex -> <complex>
          value1 = O(class_complex); break;
        default:
        unknown: # unbekannter Typ
          pushSTACK(S(class_of));
          fehler(serious_condition,
                 GETTEXT("~: unidentifiable type!!!")
                );
      }
    if_classp(value1, ; ,
      { pushSTACK(value1);
        pushSTACK(S(class_of));
        fehler(error,
               GETTEXT("~: type ~ does not correspond to a class")
              );
      });
  fertig:
    mv_count=1;
  }

LISPFUN(find_class,1,2,norest,nokey,0,NIL)
# (CLOS:FIND-CLASS symbol [errorp [environment]]), CLTL2 S. 843
# (defun find-class (symbol &optional (errorp t) environment)
#   (declare (ignore environment)) ; was sollte das Environment bedeuten?
#   (unless (symbolp symbol)
#     (error-of-type 'type-error
#       (ENGLISH "~S: argument ~S is not a symbol")
#       'find-class symbol
#   ) )
#   (let ((class (get symbol 'CLOS::CLASS)))
#     (if (not (class-p class))
#       (if errorp
#         (error-of-type 'error
#           (ENGLISH "~S: ~S does not name a class")
#           'find-class symbol
#         )
#         nil
#       )
#       class
# ) ) )
{ if (!symbolp(STACK_2))
    { pushSTACK(STACK_2); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(symbol)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(STACK_(2+2));
      pushSTACK(S(find_class));
      fehler(type_error,
             GETTEXT("~: argument ~ is not a symbol")
            );
    }
 {var object clas = get(STACK_2,S(closclass)); # (GET symbol 'CLOS::CLOSCLASS)
  if_classp(clas, { value1 = clas; } ,
    { if (!nullp(STACK_1))
        { pushSTACK(STACK_2);
          pushSTACK(S(find_class));
          fehler(error,
                 GETTEXT("~: ~ does not name a class")
                );
        }
      value1 = NIL;
    });
  mv_count=1;
  skipSTACK(3);
}}

LISPFUNN(coerce,2)
# (COERCE object result-type), CLTL S. 51
# Methode:
# (TYPEP object result-type) -> object zurück
# result-type ein Symbol type:
#   (get type 'DEFTYPE-EXPANDER) /= NIL ->
#          mit (list result-type) als Argument aufrufen, zum Anfang
#   type = T -> object zurück
#   type = CHARACTER, STRING-CHAR -> COERCE_CHAR anwenden
#   type = BASE-CHAR -> COERCE_CHAR anwenden und überprüfen
#   type = FLOAT, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT ->
#          mit der Arithmetik umwandeln
#   type = COMPLEX -> auf Zahl überprüfen
#   type = FUNCTION -> Funktionsname oder Lambda-Ausdruck in Funktion umwandeln
#   type = ARRAY, SIMPLE-ARRAY, VECTOR, SIMPLE-VECTOR, STRING, SIMPLE-STRING,
#          BASE-STRING, SIMPLE-BASE-STRING, BIT-VECTOR, SIMPLE-BIT-VECTOR ->
#          [hier auch result-type an object anpassen wie unten??]
#          mit COERCE-SEQUENCE umwandeln, mit TYPEP überprüfen
#          und evtl. mit COPY-SEQ kopieren.
#   sonst mit COERCE-SEQUENCE umwandeln
# result-type ein Cons mit Symbol type als CAR:
#   type = AND -> (coerce object (second result-type)), mit TYPEP überprüfen
#   (get type 'DEFTYPE-EXPANDER) /= NIL ->
#          mit result-type als Argument aufrufen, zum Anfang
#   type = FLOAT, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT ->
#          mit der Arithmetik umwandeln, mit TYPEP überprüfen
#   type = COMPLEX -> auf Zahl überprüfen,
#          Realteil zum Typ (second result-type) coercen,
#          Imaginärteil zum Typ (third result-type) bzw. (second result-type)
#          coercen, COMPLEX anwenden.
#   type = ARRAY, SIMPLE-ARRAY, VECTOR, SIMPLE-VECTOR, STRING, SIMPLE-STRING,
#          BASE-STRING, SIMPLE-BASE-STRING, BIT-VECTOR, SIMPLE-BIT-VECTOR ->
#          result-type an object anpassen, mit COERCE-SEQUENCE umwandeln (das
#          verarbeitet auch den in result-type angegebenen element-type), auf
#          type überprüfen und evtl. mit COPY-SEQ kopieren. Dann auf
#          result-type überprüfen.
# Sonst Error.
  { # (TYPEP object result-type) abfragen:
    pushSTACK(STACK_1); pushSTACK(STACK_(0+1)); funcall(S(typep),2);
    if (!nullp(value1))
      # object als Wert
      return_object:
      { value1 = STACK_1; mv_count=1; skipSTACK(2); return; }
    anfang: # Los geht's mit der Umwandlung.
    # Stackaufbau: object, result-type.
    if (matomp(STACK_0))
      { if (!symbolp(STACK_0)) goto fehler_type;
        # result-type ist ein Symbol
       {var object result_type = STACK_0;
        {var object expander = get(result_type,S(deftype_expander)); # (GET result-type 'DEFTYPE-EXPANDER)
         if (!eq(expander,unbound))
           { pushSTACK(expander);
            {var object new_cons = allocate_cons();
             expander = popSTACK();
             Car(new_cons) = STACK_0; # new_cons = (list result-type)
             pushSTACK(new_cons); funcall(expander,1); # Expander aufrufen
             STACK_0 = value1; # Ergebnis als neues result-type verwenden
             goto anfang;
        }  }}
        if (eq(result_type,T)) # result-type = T ?
          goto return_object; # ja -> object als Wert
        if (eq(result_type,S(character)) || eq(result_type,S(string_char))
            #if (base_char_code_limit == char_code_limit)
            || eq(result_type,S(base_char))
            #endif
           ) # result-type = CHARACTER oder STRING-CHAR [oder BASE-CHAR] ?
          { var object as_char = coerce_char(STACK_1); # object in Character umzuwandeln versuchen
            if (nullp(as_char))
              { pushSTACK(STACK_1); # Wert für Slot DATUM von TYPE-ERROR
                pushSTACK(O(type_designator_character)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
                goto fehler_object;
              }
            value1 = as_char; mv_count=1; skipSTACK(2); return;
          }
        #if (base_char_code_limit < char_code_limit)
        if (eq(result_type,S(base_char))) # result-type = BASE-CHAR ?
          { var object as_char = coerce_char(STACK_1); # object in Character umzuwandeln versuchen
            if (!base_char_p(as_char))
              { pushSTACK(STACK_1); # Wert für Slot DATUM von TYPE-ERROR
                pushSTACK(O(type_designator_base_char)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
                goto fehler_object;
              }
            value1 = as_char; mv_count=1; skipSTACK(2); return;
          }
        #endif
        if (   eq(result_type,S(float)) # FLOAT ?
            || eq(result_type,S(short_float)) # SHORT-FLOAT ?
            || eq(result_type,S(single_float)) # SINGLE-FLOAT ?
            || eq(result_type,S(double_float)) # DOUBLE-FLOAT ?
            || eq(result_type,S(long_float)) # LONG-FLOAT ?
           )
          { # object in Float umwandeln:
            subr_self = L(coerce);
            value1 = coerce_float(STACK_1,result_type); mv_count=1;
            skipSTACK(2); return;
          }
        if (eq(result_type,S(complex))) # COMPLEX ?
          { if (!numberp(STACK_1)) # object muss eine Zahl sein
              { pushSTACK(STACK_1); # Wert für Slot DATUM von TYPE-ERROR
                pushSTACK(S(number)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
                goto fehler_object;
              }
            goto return_object;
          }
        if (eq(result_type,S(function))) # FUNCTION ?
          { # vgl. coerce_function()
            var object fun = STACK_1;
            if (funnamep(fun)) # Symbol oder (SETF symbol) ?
              { value1 = sym_function(fun,NIL); # globale Funktionsdefinition holen
                if (!(subrp(value1) || closurep(value1) || ffunctionp(value1))) # FUNCTIONP überprüfen
                  { fehler_undef_function(S(coerce),fun); }
                mv_count=1;
                skipSTACK(2); return;
              }
            if (!(consp(fun) && eq(Car(fun),S(lambda)))) # object muss ein Lambda-Ausdruck sein
              { pushSTACK(fun); # Wert für Slot DATUM von TYPE-ERROR
                pushSTACK(O(type_designator_function)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
                goto fehler_object;
              }
            # leeres Environment für get_closure:
           {var environment* env;
            make_STACK_env(NIL,NIL,NIL,NIL,O(top_decl_env), env = );
            # Closure bilden aus lambdabody = (cdr fun), name = :LAMBDA :
            value1 = get_closure(Cdr(fun),S(Klambda),FALSE,env); mv_count=1; # Closure erzeugen
            skipSTACK(2+5); return;
          }}
        if (   eq(result_type,S(array)) # ARRAY ?
            || eq(result_type,S(simple_array)) # SIMPLE-ARRAY ?
            || eq(result_type,S(vector)) # VECTOR ?
            || eq(result_type,S(simple_vector)) # SIMPLE-VECTOR ?
            || eq(result_type,S(string)) # STRING ?
            || eq(result_type,S(simple_string)) # SIMPLE-STRING ?
            || eq(result_type,S(base_string)) # BASE-STRING ?
            || eq(result_type,S(simple_base_string)) # SIMPLE-BASE-STRING ?
            || eq(result_type,S(bit_vector)) # BIT-VECTOR ?
            || eq(result_type,S(simple_bit_vector)) # SIMPLE-BIT-VECTOR ?
           )
          { # result-type an den Typ von object anpassen:
            if (eq(result_type,S(array)) || eq(result_type,S(vector))) # ARRAY oder VECTOR ?
              { if (stringp(STACK_1)) # object ein String
                  goto return_object; # -> ist ein Vektor und Array
                elif (bit_vector_p(STACK_1)) # object ein Bitvektor
                  goto return_object; # -> ist ein Vektor und Array
                # Hier auch Byte-Vektoren behandeln!??
              }
            elif (eq(result_type,S(simple_array))) # SIMPLE-ARRAY ?
              { if (stringp(STACK_1)) # object ein String
                  { result_type = S(simple_string); } # -> result-type := SIMPLE-STRING
                elif (bit_vector_p(STACK_1)) # object ein Bitvektor
                  { result_type = S(simple_bit_vector); } # -> result-type := SIMPLE-BIT-VECTOR
                # Hier auch Byte-Vektoren behandeln!??
              }
            pushSTACK(result_type);
            # neue Sequence bauen:
           {var object new_seq = (coerce_sequence(STACK_2,result_type),value1);
            # und nochmals mit TYPEP überprüfen:
            pushSTACK(new_seq); pushSTACK(STACK_(0+1)); STACK_(0+2) = new_seq;
            funcall(S(typep),2); # (TYPEP new_seq result-type)
            if (!nullp(value1))
              # ja -> new_seq als Wert
              { value1 = STACK_0; mv_count=1; skipSTACK(2+1); return; }
              else
              # Trifft wegen SIMPLE-... nicht zu -> new_seq kopieren:
              { funcall(L(copy_seq),1); # (COPY-SEQ new_seq)
                skipSTACK(2); return;
              }
          }}
        # result-type ist ein sonstiges Symbol
        coerce_sequence(STACK_1,result_type); # (coerce-sequence object result-type)
        skipSTACK(2); return;
      }}
      else
      # result-type ist ein Cons.
      { var object result_type = STACK_0;
        var object type = Car(result_type);
        if (!symbolp(type)) goto fehler_type; # muss ein Symbol sein
        if (eq(type,S(and))) # (AND ...) ?
          { if (matomp(Cdr(result_type))) # (AND)
              goto return_object; # wie T behandeln
            # (COERCE object (second result-type)) ausführen:
            pushSTACK(STACK_1); pushSTACK(Car(Cdr(result_type)));
            funcall(L(coerce),2);
            check_return: # new-object in value1 überprüfen und dann als Wert liefern:
            pushSTACK(value1); # new-object retten
            # (TYPEP new-object result-type) abfragen:
            pushSTACK(value1); pushSTACK(STACK_(0+1+1)); funcall(S(typep),2);
            if (nullp(value1))
              { # STACK_0 = new-object, Wert für Slot DATUM von TYPE-ERROR
                pushSTACK(STACK_(0+1)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
                goto fehler_object;
              }
              else
              { value1 = STACK_0; mv_count=1; skipSTACK(3); return; } # new-object
          }
        {var object expander = get(type,S(deftype_expander)); # (GET type 'DEFTYPE-EXPANDER)
         if (!eq(expander,unbound))
           { pushSTACK(result_type); funcall(expander,1); # Expander aufrufen
             STACK_0 = value1; # Ergebnis als neues result-type verwenden
             goto anfang;
        }  }
        if (   eq(type,S(float)) # FLOAT ?
            || eq(type,S(short_float)) # SHORT-FLOAT ?
            || eq(type,S(single_float)) # SINGLE-FLOAT ?
            || eq(type,S(double_float)) # DOUBLE-FLOAT ?
            || eq(type,S(long_float)) # LONG-FLOAT ?
           )
          { # object in Float umwandeln:
            subr_self = L(coerce);
            value1 = coerce_float(STACK_1,result_type);
            goto check_return; # und auf result-type überprüfen
          }
        if (eq(type,S(complex))) # COMPLEX ?
          { if (!numberp(STACK_1)) # object muss eine Zahl sein
              { pushSTACK(STACK_1); # Wert für Slot DATUM von TYPE-ERROR
                pushSTACK(S(number)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
                goto fehler_object;
              }
            if (!mconsp(Cdr(result_type))) goto fehler_type; # (rest result-type) muss ein Cons sein
            result_type = Cdr(result_type);
           {var object rtype = Car(result_type); # Typ für den Realteil
            var object itype = # Typ für den Imaginärteil, Default ist rtype
              (mconsp(Cdr(result_type)) ? Car(Cdr(result_type)) : rtype);
            pushSTACK(rtype); pushSTACK(itype);
            # Realteil holen und zum Typ rtype coercen:
            pushSTACK(STACK_(1+2)); funcall(L(realpart),1);
            pushSTACK(value1); pushSTACK(STACK_(1+1)); funcall(L(coerce),2);
            STACK_1 = value1;
            # Imaginärteil holen und zum Typ itype coercen:
            pushSTACK(STACK_(1+2)); funcall(L(imagpart),1);
            pushSTACK(value1); pushSTACK(STACK_(0+1)); funcall(L(coerce),2);
            STACK_0 = value1;
            # COMPLEX darauf anwenden:
            funcall(L(complex),2);
            skipSTACK(2); return;
          }}
        if (   eq(type,S(array)) # ARRAY ?
            || eq(type,S(simple_array)) # SIMPLE-ARRAY ?
            || eq(type,S(vector)) # VECTOR ?
            || eq(type,S(simple_vector)) # SIMPLE-VECTOR ?
            || eq(type,S(string)) # STRING ?
            || eq(type,S(simple_string)) # SIMPLE-STRING ?
            || eq(type,S(base_string)) # BASE-STRING ?
            || eq(type,S(simple_base_string)) # SIMPLE-BASE-STRING ?
            || eq(type,S(bit_vector)) # BIT-VECTOR ?
            || eq(type,S(simple_bit_vector)) # SIMPLE-BIT-VECTOR ?
           )
          { # result-type an den Typ von object anpassen:
            if (eq(type,S(array)) || eq(type,S(simple_array)) || eq(type,S(vector))) # [SIMPLE-]ARRAY oder VECTOR ?
              { var object type2 = Cdr(result_type);
                if (nullp(type2)) { goto adjust_eltype; }
                if (!consp(type2)) goto fehler_type;
                if (eq(Car(type2),S(mal))) # element-type = * (unspecified) ?
                  { type2 = Cdr(type2);
                    adjust_eltype: # Hier ist type2 = (cddr result-type)
                    # wird ersetzt durch geeigneten Elementtyp:
                    pushSTACK(type);
                    pushSTACK(type2);
                    if (arrayp(STACK_(1+2)))
                      { pushSTACK(array_element_type(STACK_(1+2))); }
                      else
                      { pushSTACK(T); }
                   {var object new_cons = allocate_cons();
                    Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();
                    pushSTACK(new_cons);
                   }
                   {var object new_cons = allocate_cons();
                    Cdr(new_cons) = popSTACK();
                    Car(new_cons) = type = popSTACK();
                    result_type = new_cons;
              }   }}
            pushSTACK(type);
            # neue Sequence bauen:
           {var object new_seq = (coerce_sequence(STACK_2,result_type),value1);
            # und nochmals mit TYPEP überprüfen:
            pushSTACK(new_seq); pushSTACK(STACK_(0+1)); STACK_(0+2) = new_seq;
            funcall(S(typep),2); # (TYPEP new_seq type)
            if (!nullp(value1))
              { value1 = popSTACK(); }
              else
              # Trifft wegen SIMPLE-... nicht zu -> new_seq kopieren:
              { funcall(L(copy_seq),1); } # (COPY-SEQ new_seq)
            goto check_return;
          }}
        # type ist ein sonstiges Symbol
      }
    fehler_type:
      # result-type in STACK_0
      pushSTACK(S(coerce));
      fehler(error,
             GETTEXT("~: bad type specification ~")
            );
    fehler_object:
      # Stackaufbau: object, result-type, type-error-datum, type-error-expected-type.
      pushSTACK(STACK_2); # result-type
      pushSTACK(STACK_(3+1)); # object
      pushSTACK(S(coerce));
      fehler(type_error,
             GETTEXT("~: ~ cannot be coerced to type ~")
            );
  }

# ==============================================================================
#                               Heap statistics

# Notification from defstruc.lsp and clos.lsp.
LISPFUNN(note_new_structure_class,0)
  { O(structure_class_count_max) = fixnum_inc(O(structure_class_count_max),1); }
LISPFUNN(note_new_standard_class,0)
  { O(standard_class_count_max) = fixnum_inc(O(standard_class_count_max),1); }
# These two ..._count_max variables are provided so that we can do heap
# statistics in one pass, without risking a GC, and without a pre-pass which
# determines the number of occurring types.

# (SYSTEM::HEAP-STATISTICS)
# returns a vector containing statistics records about current heap usage
# for each type:
#    stat = #( ... (classname num-instances . num-bytes) ...)

# (SYSTEM::GC-STATISTICS)
# returns a list, with one element for each GC (the first for the last GC,
# the second for the second-to-last, etc.), where each element is a vector
# containing statistics records about what the GC could reclaim of each type:
#    statlist = ( #( ... (classname num-instances . num-bytes) ...) ...)

# Since GC statistics is a burden on each GC, we perform it only when needed,
# i.e. while the variable SYSTEM::*GC-STATISTICS* is bound to a value > 0.
# When SYSTEM::*GC-STATISTICS* is 0, no statistics are gathered, but old ones
# are still kept. When SYSTEM::*GC-STATISTICS* is negative, no statistics are
# gathered, and old statistics are thrown away.

# The data is gathered in three areas: one for built-in types, one for
# structure types (an AVL tree, indexed by structure name), one for
# standard-class types (an AVL tree, indexed by the class). While the data
# is being gathered, symbols and classes are pushed onto the STACK, but no
# heap allocation takes place.

typedef struct { const object* name; # pointer to a GC-safe object (e.g. in the STACK)
                 sintL n_instances; # number of instances
                 sintL n_bytes;     # number of bytes
               }
        hs_record;

# The type of an object for statistics purposes is a little more detailed
# than CLASS-OF, but, unlike TYPE-OF, nonparametric.
enum { # The values of this enumeration are 0,1,2,...
       # When you change this, update LISPOBJ(hs_...) in constobj.d!
  enum_hs_t,
  enum_hs_cons,
  enum_hs_null,
  enum_hs_symbol,
  enum_hs_simple_bit_vector,
  enum_hs_simple_string,
  enum_hs_simple_vector,
  enum_hs_bit_vector,
  enum_hs_byte_vector,
  enum_hs_string,
  enum_hs_vector,
  enum_hs_simple_array,
  enum_hs_array,
  enum_hs_standard_generic_function,
  enum_hs_function,
  enum_hs_file_stream,
  enum_hs_synonym_stream,
  enum_hs_broadcast_stream,
  enum_hs_concatenated_stream,
  enum_hs_two_way_stream,
  enum_hs_echo_stream,
  enum_hs_string_stream,
  enum_hs_stream,
  enum_hs_hash_table,
  enum_hs_package,
  enum_hs_readtable,
  enum_hs_pathname,
  #ifdef LOGICAL_PATHNAMES
  enum_hs_logical_pathname,
  #endif
  enum_hs_random_state,
  enum_hs_byte,
  enum_hs_special_operator,
  enum_hs_load_time_eval,
  enum_hs_symbol_macro,
  enum_hs_encoding,
  #ifdef FOREIGN
  enum_hs_foreign_pointer,
  #endif
  #ifdef DYNAMIC_FFI
  enum_hs_foreign_address,
  enum_hs_foreign_variable,
  enum_hs_foreign_function,
  #endif
  enum_hs_weakpointer,
  enum_hs_finalizer,
  #ifdef SOCKET_STREAMS
  enum_hs_socket_server,
  #endif
  #ifdef YET_ANOTHER_RECORD
  enum_hs_yetanother,
  #endif
  enum_hs_system_function,
  enum_hs_bignum,
  enum_hs_ratio,
  #ifndef WIDE
  enum_hs_single_float,
  #endif
  enum_hs_double_float,
  enum_hs_long_float,
  enum_hs_complex,
  enum_hs_dummy
};

# Need an AVL tree for rapidly associating a hs_record to its name.

#define AVLID  heapstat
#define AVL_ELEMENT  hs_record
#define AVL_EQUAL(element1,element2)  (eq(*(element1).name,*(element2).name))
#define AVL_KEY  object
#define AVL_KEYOF(element)  (*(element).name)
#define AVL_SIGNED_INT  soint
#define AVL_COMPARE(key1,key2)  (soint)(as_oint(key1)-as_oint(key2))
#define NO_AVL_MEMBER
#define NO_AVL_INSERT
#define NO_AVL_DELETE
#define NO_AVL_DELETE1
#define NO_AVL_LEAST
#define NO_AVL_MOVE
#define NO_AVL_SORT
#include "avl.c"
#include "avl.c"  # This defines the NODE type.

typedef struct { NODE* tree; uintL count; NODE* free_nodes; uintL free_count; }
        hs_sorted;

typedef struct { boolean decrementing; # incrementing or decrementing
                 hs_sorted structure_classes;
                 hs_sorted standard_classes;
                 hs_record builtins[(int)enum_hs_dummy];
               }
        hs_locals;

# Initialize a hs_locals. NB: This does stack allocation on the caller's stack.
#define init_hs_locals(locals)  \
  var DYNAMIC_ARRAY(free_room,NODE, (locals.structure_classes.free_count = posfixnum_to_L(O(structure_class_count_max))) + (locals.standard_classes.free_count = posfixnum_to_L(O(standard_class_count_max)))); \
  init_hs_locals_rest(&locals,free_room);
#define done_hs_locals(locals)  \
  FREE_DYNAMIC_ARRAY(free_room);

local void init_hs_locals_rest (hs_locals* locals, NODE* free_room);
local void init_hs_locals_rest(locals,free_room)
  var hs_locals* locals;
  var NODE* free_room;
  { locals->decrementing = FALSE;
    # Initialize all counters to 0.
    locals->structure_classes.tree = EMPTY;
    locals->standard_classes.tree = EMPTY;
    locals->structure_classes.count = 0;
    locals->standard_classes.count = 0;
    locals->structure_classes.free_nodes = &free_room[0];
    locals->standard_classes.free_nodes = &free_room[locals->structure_classes.free_count];
    { var uintC count;
      var hs_record* ptr = &locals->builtins[0];
      var const object* optr = &O(hs_t);
      dotimesC(count,(uintC)enum_hs_dummy,
        { ptr->name = optr;
          ptr->n_instances = 0;
          ptr->n_bytes = 0;
          ptr++; optr++;
        });
    }
    # Prepare for STACK allocation.
    get_space_on_STACK(sizeof(object) * (locals->structure_classes.free_count + locals->standard_classes.free_count));
  }

# This is the function we pass to map_heap_objects().
local void heap_statistics_mapper (void* arg, object obj, uintL bytelen);
local void heap_statistics_mapper(arg,obj,bytelen)
  var void* arg;
  var object obj;
  var uintL bytelen;
  { var hs_locals* locals = (hs_locals*)arg;
    var hs_record* pighole; # `pighole' stands for `pigeon-hole'
    #ifdef TYPECODES
    switch (typecode(obj))
    #else
    if (orecordp(obj)) { goto case_orecord; }
    elif (consp(obj)) { goto case_cons; }
    elif (subrp(obj)) { goto case_subr; }
    else switch (0)
    #endif
      { case_instance: # Instanz
          { var object clas = TheInstance(obj)->inst_class;
            var NODE* found = AVL(AVLID,member0)(clas,locals->standard_classes.tree);
            if (found == (NODE*)NULL)
              { if (locals->standard_classes.free_count == 0) # shouldn't happen
                  { pighole = &locals->builtins[(int)enum_hs_t]; break; }
                locals->standard_classes.free_count--;
                found = locals->standard_classes.free_nodes++;
                pushSTACK(clas);
                found->nodedata.value.name        = &STACK_0;
                found->nodedata.value.n_instances = 0;
                found->nodedata.value.n_bytes     = 0;
                locals->standard_classes.tree = AVL(AVLID,insert1)(found,locals->standard_classes.tree);
                locals->standard_classes.count++;
              }
            pighole = &found->nodedata.value;
            break;
          }
        case_structure: # Structure
          { var object name = Car(TheStructure(obj)->structure_types);
            var NODE* found = AVL(AVLID,member0)(name,locals->structure_classes.tree);
            if (found == (NODE*)NULL)
              { if (locals->structure_classes.free_count == 0) # shouldn't happen
                  { pighole = &locals->builtins[(int)enum_hs_t]; break; }
                locals->structure_classes.free_count--;
                found = locals->structure_classes.free_nodes++;
                pushSTACK(name);
                found->nodedata.value.name        = &STACK_0;
                found->nodedata.value.n_instances = 0;
                found->nodedata.value.n_bytes     = 0;
                locals->structure_classes.tree = AVL(AVLID,insert1)(found,locals->structure_classes.tree);
                locals->structure_classes.count++;
              }
            pighole = &found->nodedata.value;
            break;
          }
        case_cons: # Cons
          pighole = &locals->builtins[(int)enum_hs_cons];
          break;
        case_symbol: # Symbol
          if (nullp(obj))
            pighole = &locals->builtins[(int)enum_hs_null];
          else
            pighole = &locals->builtins[(int)enum_hs_symbol];
          break;
        case_sbvector: # Simple-Bit-Vector
          pighole = &locals->builtins[(int)enum_hs_simple_bit_vector];
          break;
        case_sstring: # Simple-String
          pighole = &locals->builtins[(int)enum_hs_simple_string];
          break;
        case_svector: # Simple-Vector
          pighole = &locals->builtins[(int)enum_hs_simple_vector];
          break;
        case_obvector:
          if ((Iarray_flags(obj) & arrayflags_atype_mask) == Atype_Bit)
            pighole = &locals->builtins[(int)enum_hs_bit_vector]; # sonstiger Bit-Vector
          else
            pighole = &locals->builtins[(int)enum_hs_byte_vector]; # sonstiger Byte-Vector
          break;
        case_ostring: # sonstiger String
          pighole = &locals->builtins[(int)enum_hs_string];
          break;
        case_ovector: # sonstiger general-vector
          pighole = &locals->builtins[(int)enum_hs_vector];
          break;
        case_mdarray: # sonstiger Array
          if ((Iarray_flags(obj)
               & (  bit(arrayflags_adjustable_bit)
                  | bit(arrayflags_fillp_bit)
                  | bit(arrayflags_displaced_bit)
                  | bit(arrayflags_dispoffset_bit)
              )  )
              == 0
             )
            pighole = &locals->builtins[(int)enum_hs_simple_array];
          else
            pighole = &locals->builtins[(int)enum_hs_array];
          break;
        case_closure: # Closure
          if (genericfunctionp(obj))
            pighole = &locals->builtins[(int)enum_hs_standard_generic_function];
          else
            pighole = &locals->builtins[(int)enum_hs_function];
          break;
        case_stream: # Stream
          switchu (TheStream(obj)->strmtype)
            { case strmtype_file:
                pighole = &locals->builtins[(int)enum_hs_file_stream]; break;
              case strmtype_synonym:
                pighole = &locals->builtins[(int)enum_hs_synonym_stream]; break;
              case strmtype_broad:
                pighole = &locals->builtins[(int)enum_hs_broadcast_stream]; break;
              case strmtype_concat:
                pighole = &locals->builtins[(int)enum_hs_concatenated_stream]; break;
              case strmtype_twoway:
                pighole = &locals->builtins[(int)enum_hs_two_way_stream]; break;
              case strmtype_echo:
                pighole = &locals->builtins[(int)enum_hs_echo_stream]; break;
              case strmtype_str_in:
              case strmtype_str_out:
              case strmtype_str_push:
                pighole = &locals->builtins[(int)enum_hs_string_stream]; break;
              default:
                pighole = &locals->builtins[(int)enum_hs_stream]; break;
            }
          break;
        case_orecord: # OtherRecord
          switch (Record_type(obj))
            { case_Rectype_Instance_above;
              case_Rectype_Structure_above;
              case_Rectype_Symbol_above;
              case_Rectype_Sbvector_above;
              case_Rectype_Sstring_above;
              case_Rectype_Svector_above;
              case_Rectype_obvector_above;
              case_Rectype_ostring_above;
              case_Rectype_ovector_above;
              case_Rectype_mdarray_above;
              case_Rectype_Closure_above;
              case_Rectype_Stream_above;
              case_Rectype_Bignum_above;
              case_Rectype_Ratio_above;
              case_Rectype_Ffloat_above;
              case_Rectype_Dfloat_above;
              case_Rectype_Lfloat_above;
              case_Rectype_Complex_above;
              case Rectype_Hashtable: # Hash-Table
                pighole = &locals->builtins[(int)enum_hs_hash_table]; break;
              case Rectype_Package: # Package
                pighole = &locals->builtins[(int)enum_hs_package]; break;
              case Rectype_Readtable: # Readtable
                pighole = &locals->builtins[(int)enum_hs_readtable]; break;
              case Rectype_Pathname: # Pathname
                pighole = &locals->builtins[(int)enum_hs_pathname]; break;
              #ifdef LOGICAL_PATHNAMES
              case Rectype_Logpathname: # Logical Pathname
                pighole = &locals->builtins[(int)enum_hs_logical_pathname]; break;
              #endif
              case Rectype_Random_State: # Random-State
                pighole = &locals->builtins[(int)enum_hs_random_state]; break;
              case Rectype_Byte: # Byte
                pighole = &locals->builtins[(int)enum_hs_byte]; break;
              case Rectype_Fsubr: # Fsubr
                pighole = &locals->builtins[(int)enum_hs_special_operator]; break;
              case Rectype_Loadtimeeval: # Load-Time-Eval
                pighole = &locals->builtins[(int)enum_hs_load_time_eval]; break;
              case Rectype_Symbolmacro: # Symbol-Macro
                pighole = &locals->builtins[(int)enum_hs_symbol_macro]; break;
              case Rectype_Encoding: # Encoding
                pighole = &locals->builtins[(int)enum_hs_encoding]; break;
              #ifdef FOREIGN
              case Rectype_Fpointer: # Foreign-Pointer-Verpackung
                pighole = &locals->builtins[(int)enum_hs_foreign_pointer]; break;
              #endif
              #ifdef DYNAMIC_FFI
              case Rectype_Faddress: # Foreign-Adresse
                pighole = &locals->builtins[(int)enum_hs_foreign_address]; break;
              case Rectype_Fvariable: # Foreign-Variable
                pighole = &locals->builtins[(int)enum_hs_foreign_variable]; break;
              case Rectype_Ffunction: # Foreign-Function
                pighole = &locals->builtins[(int)enum_hs_foreign_function]; break;
              #endif
              case Rectype_Weakpointer: # Weak-Pointer
                pighole = &locals->builtins[(int)enum_hs_weakpointer]; break;
              case Rectype_Finalizer: # Finalisierer
                pighole = &locals->builtins[(int)enum_hs_finalizer]; break;
              #ifdef SOCKET_STREAMS
              case Rectype_Socket_Server: # Socket-Server
                pighole = &locals->builtins[(int)enum_hs_socket_server]; break;
              #endif
              #ifdef YET_ANOTHER_RECORD
              case Rectype_Yetanother: # Yetanother
                pighole = &locals->builtins[(int)enum_hs_yetanother]; break;
              #endif
              default:
                pighole = &locals->builtins[(int)enum_hs_t]; break;
            }
          break;
        case_subr: # SUBR
          pighole = &locals->builtins[(int)enum_hs_system_function];
          break;
        case_bignum: # Bignum
          pighole = &locals->builtins[(int)enum_hs_bignum];
          break;
        case_ratio: # Ratio
          pighole = &locals->builtins[(int)enum_hs_ratio];
          break;
        #ifndef WIDE
        case_ffloat: # Single-Float
          pighole = &locals->builtins[(int)enum_hs_single_float];
          break;
        #endif
        case_dfloat: # Double-Float
          pighole = &locals->builtins[(int)enum_hs_double_float];
          break;
        case_lfloat: # Long-Float
          pighole = &locals->builtins[(int)enum_hs_long_float];
          break;
        case_complex: # Complex
          pighole = &locals->builtins[(int)enum_hs_complex];
          break;
        default:
          pighole = &locals->builtins[(int)enum_hs_t]; break;
      }
    if (locals->decrementing)
      { pighole->n_instances -= 1;
        pighole->n_bytes -= bytelen;
      }
      else
      { pighole->n_instances += 1;
        pighole->n_bytes += bytelen;
      }
  }

# Creates a vector containing the heap statistics result,
# and pushes it onto the STACK.
# can trigger GC
  local void heap_statistics_result (hs_locals* locals);
  local void heap_statistics_result(locals)
    var hs_locals* locals;
    { # Allocate result vector.
      var uintL length = (uintL)enum_hs_dummy
                         + locals->structure_classes.count
                         + locals->standard_classes.count;
      pushSTACK(allocate_vector(length));
     {var object* result_ = &STACK_0;
      # Fill result vector.
      # (L_to_I cannot call GC here, because the numbers are small.)
      var uintL i = 0;
      { var uintC count;
        var hs_record* ptr = &locals->builtins[0];
        dotimesC(count,(uintC)enum_hs_dummy,
          { var object hsr = make_list(2);
            Car(hsr) = *ptr->name;
            Car(Cdr(hsr)) = L_to_I(ptr->n_instances);
            Cdr(Cdr(hsr)) = L_to_I(ptr->n_bytes);
            TheSvector(*result_)->data[i] = hsr;
            ptr++; i++;
          });
      }
      { var uintL count = locals->structure_classes.count;
        if (count > 0)
          { var NODE* ptr = locals->structure_classes.free_nodes;
            dotimespL(count,count,
              { --ptr;
               {var object hsr = make_list(2);
                Car(hsr) = *ptr->nodedata.value.name;
                Car(Cdr(hsr)) = L_to_I(ptr->nodedata.value.n_instances);
                Cdr(Cdr(hsr)) = L_to_I(ptr->nodedata.value.n_bytes);
                TheSvector(*result_)->data[i] = hsr;
                i++;
              }});
      }   }
      { var uintL count = locals->standard_classes.count;
        if (count > 0)
          { var NODE* ptr = locals->standard_classes.free_nodes;
            dotimespL(count,count,
              { --ptr;
               {var object hsr = make_list(2);
                Car(hsr) = TheClass(*ptr->nodedata.value.name)->classname;
                Car(Cdr(hsr)) = L_to_I(ptr->nodedata.value.n_instances);
                Cdr(Cdr(hsr)) = L_to_I(ptr->nodedata.value.n_bytes);
                TheSvector(*result_)->data[i] = hsr;
                i++;
              }});
      }   }
    }}

LISPFUNN(heap_statistics,0)
  { var hs_locals locals;
    init_hs_locals(locals);
    # Walk through memory.
    map_heap_objects(&heap_statistics_mapper,&locals);
    # Allocate and fill result vector.
    heap_statistics_result(&locals);
    # Done.
    value1 = popSTACK(); mv_count=1;
    skipSTACK(locals.structure_classes.count + locals.standard_classes.count);
    done_hs_locals(locals);
  }

LISPFUNN(gc_statistics,0)
  { value1 = O(gc_statistics_list); mv_count=1; }

# UP: Führt eine Statistik über die Aktion einer GC.
# with_gc_statistics(fun);
# > fun: Funktion, die eine GC ausführt
  global void with_gc_statistics (gc_function* fun);
  global void with_gc_statistics(fun)
    var gc_function* fun;
    { var object flag = Symbol_value(S(gc_statistics_stern));
      if (!posfixnump(flag))
        # No need to do statistics, throw old ones away.
        { O(gc_statistics_list) = NIL; fun(); }
      elif (eq(flag,Fixnum_0))
        # No need to do statistics, but keep old ones.
        { fun(); }
      else
        # Do statistics.
        { var hs_locals locals;
          init_hs_locals(locals);
          # Walk through memory.
          map_heap_objects(&heap_statistics_mapper,&locals);
          # Now do the GC.
          fun();
          # Walk through memory again, this time decrementing.
          locals.decrementing = TRUE;
          map_heap_objects(&heap_statistics_mapper,&locals);
          # Now if in the following allocation requests, a GC occurs, we
          # might mix up the order of the records in O(gc_statistics_list),
          # but that's not relevant.
          # But if memory is full, we might be called recursively. A recursive
          # depth of 1 is OK (that's normal), but a greater recursion depth
          # is a sign of an infinite recursion. In this case we rebind
          # SYSTEM::*GC-STATISTICS* to 0.
         {var boolean danger = FALSE;
          dynamic_bind(S(recurse_count_gc_statistics),fixnum_inc(Symbol_value(S(recurse_count_gc_statistics)),1)); # sys::*recurse-count-gc-statistics* erhöhen
          if (!posfixnump(Symbol_value(S(recurse_count_gc_statistics)))) # sollte ein Fixnum >=0 sein
            { Symbol_value(S(recurse_count_gc_statistics)) = Fixnum_0; } # sonst Notkorrektur
          if (posfixnum_to_L(Symbol_value(S(recurse_count_gc_statistics))) > 3)
            { # Zu große Rekursionstiefe.
              danger = TRUE;
              dynamic_bind(S(gc_statistics_stern),Fixnum_0);
            }
          # Allocate and fill result vector.
          heap_statistics_result(&locals);
          # Push it onto O(gc_statistics_list).
          { var object new_cons = allocate_cons();
            Car(new_cons) = popSTACK(); Cdr(new_cons) = O(gc_statistics_list);
            O(gc_statistics_list) = new_cons;
          }
          # Done.
          if (danger) { dynamic_unbind(); }
          dynamic_unbind();
          skipSTACK(locals.structure_classes.count + locals.standard_classes.count);
          done_hs_locals(locals);
        }}
    }

local Values statistics_statistics (uintL svector_instances, uintL svector_bytes, uintL cons_instances);
local Values statistics_statistics(svector_instances,svector_bytes,cons_instances)
  var uintL svector_instances;
  var uintL svector_bytes;
  var uintL cons_instances;
  { { var object hsr = make_list(2);
      Car(hsr) = O(hs_simple_vector);
      Car(Cdr(hsr)) = fixnum(svector_instances);
      Cdr(Cdr(hsr)) = fixnum(svector_bytes);
      pushSTACK(hsr);
    }
    { var object hsr = make_list(2);
      Car(hsr) = O(hs_cons);
      Car(Cdr(hsr)) = fixnum(cons_instances);
      Cdr(Cdr(hsr)) = fixnum(cons_instances*sizeof(cons_));
      pushSTACK(hsr);
    }
    value1 = vectorof(2); mv_count=1;
  }

LISPFUNN(list_statistics,1)
# (SYSTEM::LIST-STATISTICS list)
# Return statistics about how much a list uses.
  { statistics_statistics(0,0,llength(popSTACK())); }

LISPFUNN(heap_statistics_statistics,1)
# (SYSTEM::HEAP-STATISTICS-STATISTICS statistics)
# Return statistics about how much a statistics vector uses.
  { var object obj = popSTACK();
    ASSERT(simple_vector_p(obj));
    statistics_statistics(1,varobject_bytelength(obj),Svector_length(obj)*2);
  }

LISPFUNN(gc_statistics_statistics,2)
# (SYSTEM::GC-STATISTICS-STATISTICS statlist1 statlist2)
# Return statistics about how much the GC statistics used up between two calls
# to the function SYSTEM::GC-STATISTICS.
  { var object statlist2 = popSTACK();
    var object statlist1 = popSTACK();
    var uintL svector_instances = 0;
    var uintL svector_bytes = 0;
    var uintL cons_instances = 0;
    while (consp(statlist2) && !eq(statlist2,statlist1))
      { var object obj = Car(statlist2);
        ASSERT(simple_vector_p(obj));
        svector_instances += 1;
        svector_bytes += varobject_bytelength(obj);
        cons_instances += 1 + Svector_length(obj)*2;
        statlist2 = Cdr(statlist2);
      }
    statistics_statistics(svector_instances,svector_bytes,cons_instances);
  }

