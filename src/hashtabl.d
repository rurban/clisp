# Hash-Tabellen in CLISP
# Bruno Haible 1990-1999

#include "lispbibl.c"
#include "arilev0.c" # für Hashcode-Berechnung
#include "aridecl.c" # für Short-Floats


# Aufbau einer Hash-Tabelle:
# Es werden Paare (Key . Value) abgelegt.
# In einem Vektor, der durch (hashcode Key) indiziert wird.
# Damit ein laufendes MAPHASH von einer GC unbeeinflusst bleibt, wird dieser
# Vektor bei GC nicht reorganisiert. Da aber bei GC jeder (hashcode Key)
# sich ändern kann, bauen wir eine weitere Indizierungsstufe ein:
# (hashcode Key) indiziert einen Index-Vektor; dort steht ein Index in
# den Key-Value-Vektor, und dort befindet sich (Key . Value).
# Um Speicherplatz zu sparen, legen wir nicht ein Cons (Key . Value)
# im Vektor ab, sondern einfach Key und Value hintereinander.
# Kollisionen [mehrere Keys haben denselben (hascode Key)] möchte man durch
# Listen beheben. Da aber der Key-Value-Vektor (wieder wegen MAPHASH) bei GC
# unbeeinflusst bleiben soll und GC die Menge der Kollisionen verändert,
# brauchen wir einen weiteren Index-Vektor, genannt Next-Vektor, der
# "parallel" zum Key-Value-Vektor liegt und eine "Listen"struktur enthält.
# Skizze:
#   Key --> (hashcode Key) als Index in Index-Vaktor.
#   Key1 --> 3, Key2 --> 1, Key4 --> 3.
#   Index-Vektor      #( nix {IndexKey2} nix {IndexKey1,IndexKey4} nix ... )
#                   = #( nix 1 nix 0 nix ... )
#   Next-Vektor       #(     3        nix       leer      nix      leer   )
#   Key-Value-Vektor  #( Key1 Val1 Key2 Val2 leer leer Key4 Val4 leer leer)
# Zugriff auf ein (Key . Value) - Paar geschieht also folgendermaßen:
#   index := (aref Index-Vektor (hashcode Key))
#   until index = nix
#     if (eql Key (aref KVVektor 2*index)) return (aref KVVektor 2*index+1)
#     index := (aref Next-Vektor index) ; "CDR" der Liste nehmen
#   return notfound.
# Wird Index-Vektor vergrößert, müssen alle Hashcodes und der Inhalt von
# Index-Vektor und der Inhalt von Next-Vektor neu berechnet werden.
# Werden Next-Vektor und Key-Value-Vektor vergrößert, so können die
# restlichen Elemente mit "leer" gefüllt werden, ohne dass ein Hashcode neu
# berechnet werden müsste.
# Damit nach CLRHASH oder vielfachem REMHASH, wenn eine Tabelle viel
# weniger Elemente enthält als ihre Kapazität, ein MAPHASH schnell geht,
# könnte man die Einträge im Key-Value-Vektor "links gepackt" halten, d.h.
# alle "leer" kommen rechts. Dann braucht man bei MAPHASH nur die Elemente
# count-1,...,1,0 des Key-Value-Vektors abzugrasen. Aber REMHASH muss
# - nachdem es eine Lücke gelassen hat - das hinterste Key-Value-Paar
# (Nummer count-1) in die Lücke umfüllen.
# Wir behandeln solche Fälle dadurch, dass wir bei CLRHASH und REMHASH
# eventuell den Key-Value-Vektor und den Next-Vektor verkleinern.
# Damit PUTHASH einen freien Eintrag findet, halten wir die "leer" im
# Next-Vektor in einer Frei"liste".
# Die Längen von Index-Vektor und Next-Vektor sind unabhängig voneinander.
# Wir wählen sie hier im Verhältnis 2:1.
# Die Hash-Tabelle wird vergrößert, wenn die Freiliste leer ist, d.h.
# COUNT > MAXCOUNT wird. Dabei werden MAXCOUNT und SIZE mit REHASH-SIZE (>1)
# multipliziert.
# Die Hash-Tabelle wird verkleinert, wenn COUNT < MINCOUNT wird. Dabei
# werden MAXCOUNT und SIZE mit 1/REHASH-SIZE (<1) multipliziert. Damit nach
# einer Vergrößerung der Tabelle COUNT gleichviel nach oben wie nach unten
# variieren kann (auf einer logarithmischen Skala), wählen wir
# MINCOUNT = MAXCOUNT / REHASH-SIZE^2 .

# Datenstruktur der Hash-Tabelle (siehe LISPBIBL.D):
# recflags codiert den Typ und den Zustand der Hash-Tabelle:
#   Bit 0 gesetzt, wenn EQ-Hashtabelle
#   Bit 1 gesetzt, wenn EQL-Hashtabelle
#   Bit 2 gesetzt, wenn EQUAL-Hashtabelle
#   Bit 3 gesetzt, wenn EQUALP-Hashtabelle
#   Bit 4-6 =0
#   Bit 7 gesetzt, wenn Tabelle nach GC reorganisiert werden muss
# ht_size                Fixnum>0 = Länge der ITABLE
# ht_maxcount            Fixnum>0 = Länge der NTABLE
# ht_itable              Index-Vektor der Länge SIZE, enthält Indizes
# ht_ntable              Next-Vektor der Länge MAXCOUNT, enthält Indizes
# ht_kvtable             Key-Value-Vektor, Vektor der Länge 2*MAXCOUNT
# ht_freelist            Start-Index der Freiliste im Next-Vektor
# ht_count               Anzahl der Einträge in der Table, Fixnum >=0, <=MAXCOUNT
# ht_rehash_size         Wachstumsrate bei Reorganisation. Float >1.1
# ht_mincount_threshold  Verhältnis MINCOUNT/MAXCOUNT = 1/rehash-size^2
# ht_mincount            Fixnum>=0, untere Grenze für COUNT
# Eintrag "leer" im Key-Value-Vektor ist = #<UNBOUND>.
# Eintrag "leer" im Next-Vektor ist durch die Freiliste gefüllt.
# Eintrag "nix" im Index-Vektor und im Next-Vektor ist = #<UNBOUND>.
  #define leer  unbound
  #define nix   unbound

# Rotiert einen Hashcode x um n Bits nach links (0<n<32).
# rotate_left(n,x)
  #if !(defined(WATCOM) && defined(__INLINE_FUNCTIONS__))
    #define rotate_left(n,x)  (((x) << (n)) | ((x) >> (32-(n))))
  #else
    #define rotate_left(n,x)  _lrotl(x,n)
  #endif

# Mischt zwei Hashcodes.
# Der eine wird um 5 Bit rotiert, dann der andere draufgeXORt.
  #define misch(x1,x2) (rotate_left(5,x1) ^ (x2))

# UP: Berechnet den EQ-Hashcode eines Objekts.
# hashcode1(obj)
# Er ist nur bis zur nächsten GC gültig.
# Aus (eq X Y) folgt (= (hashcode1 X) (hashcode1 Y)).
# > obj: ein Objekt
# < ergebnis: Hashcode, eine 32-Bit-Zahl
  local uint32 hashcode1 (object obj);
  #ifdef WIDE
    #define hashcode1(obj)  ((uint32)untype(obj))
  #else
    #define hashcode1(obj)  ((uint32)as_oint(obj)) # Adresse (Bits 23..0) und Typinfo
  #endif

# UP: Berechnet den EQL-Hashcode eines Objekts.
# hashcode2(obj)
# Er ist nur bis zur nächsten GC gültig.
# Aus (eql X Y) folgt (= (hashcode2 X) (hashcode2 Y)).
# > obj: ein Objekt
# < ergebnis: Hashcode, eine 32-Bit-Zahl
  local uint32 hashcode2 (object obj);
# Hilfsfunktionen bei bekanntem Typ:
  # Fixnum: Fixnum-Wert
  local uint32 hashcode_fixnum (object obj);
  #if 0
  local uint32 hashcode_fixnum(obj)
    var object obj;
    {
      return hashcode1(obj);
    }
  #else
  #define hashcode_fixnum(obj)  hashcode1(obj)
  #endif
  # Bignum: Länge*2 + (MSD*2^16 + LSD)
  local uint32 hashcode_bignum (object obj);
  local uint32 hashcode_bignum(obj)
    var object obj;
    {
      var uintL len = (uintL)Bignum_length(obj); # Anzahl Words
      return
        #if (intDsize==32)
          misch(TheBignum(obj)->data[0], # MSD
                TheBignum(obj)->data[len-1]) # und LSD
        #elif (intDsize==16) || (bn_minlength<4)
          highlow32(TheBignum(obj)->data[0], # MSD
                    TheBignum(obj)->data[len-1]) # und LSD
        #else # (intDsize==8) && (bn_minlength>=4)
          ( (((uint32)TheBignum(obj)->data[0]) << 24)
           |(((uint32)TheBignum(obj)->data[1]) << 16)
           |(((uint32)TheBignum(obj)->data[2]) << 8)
           |((uint32)TheBignum(obj)->data[len-1])
          )
        #endif
        + 2*len; # und Länge*2
    }
  # Short-Float: Interne Repräsentation
  local uint32 hashcode_sfloat (object obj);
  #if 0
  local uint32 hashcode_sfloat(obj)
    var object obj;
    {
      return hashcode1(obj);
    }
  #else
  #define hashcode_sfloat(obj)  hashcode1(obj)
  #endif
  # Single-Float: 32 Bit
  local uint32 hashcode_ffloat (object obj);
  local uint32 hashcode_ffloat(obj)
    var object obj;
    {
      return ffloat_value(obj);
    }
  # Double-Float: führende 32 Bit
  local uint32 hashcode_dfloat (object obj);
  local uint32 hashcode_dfloat(obj)
    var object obj;
    {
      #ifdef intQsize
      return (uint32)(TheDfloat(obj)->float_value >> 32);
      #else
      return TheDfloat(obj)->float_value.semhi;
      #endif
    }
  # Long-Float: Mischung aus Exponent, Länge, erste 32 Bit
  extern uint32 hashcode_lfloat (object obj); # siehe LFLOAT.D
# allgemein:
  local uint32 hashcode2(obj)
    var object obj;
    {
      #ifdef TYPECODES
      if (!numberp(obj)) { # eine Zahl?
        # nein -> EQ-Hashcode nehmen (bei Characters ist ja EQL == EQ) :
        return hashcode1(obj);
      } else {
        # ja -> nach Typcode unterscheiden:
        switch (typecode(obj) & ~(bit(number_bit_t)|bit(sign_bit_t))) {
          case fixnum_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Fixnum
            return hashcode_fixnum(obj);
          case bignum_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Bignum
            return hashcode_bignum(obj);
          case sfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Short-Float
            return hashcode_sfloat(obj);
          case ffloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Single-Float
            return hashcode_ffloat(obj);
          case dfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Double-Float
            return hashcode_dfloat(obj);
          case lfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Long-Float
            return hashcode_lfloat(obj);
          case ratio_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Ratio
            # beide Komponenten hashen, mischen
            {
              var uint32 code1 = hashcode2(TheRatio(obj)->rt_num);
              var uint32 code2 = hashcode2(TheRatio(obj)->rt_den);
              return misch(code1,code2);
            }
          case complex_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Complex
            # beide Komponenten hashen, mischen
            {
              var uint32 code1 = hashcode2(TheComplex(obj)->c_real);
              var uint32 code2 = hashcode2(TheComplex(obj)->c_imag);
              return misch(code1,code2);
            }
          default: NOTREACHED
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
          case Rectype_Ratio:
            # beide Komponenten hashen, mischen
            {
              var uint32 code1 = hashcode2(TheRatio(obj)->rt_num);
              var uint32 code2 = hashcode2(TheRatio(obj)->rt_den);
              return misch(code1,code2);
             }
          case Rectype_Complex:
            # beide Komponenten hashen, mischen
            {
              var uint32 code1 = hashcode2(TheComplex(obj)->c_real);
              var uint32 code2 = hashcode2(TheComplex(obj)->c_imag);
              return misch(code1,code2);
            }
          default:
            break;
        }
      elif (immediate_number_p(obj)) {
        if (as_oint(obj) & wbit(4))
          return hashcode_sfloat(obj);
        else
          return hashcode_fixnum(obj);
      }
      return hashcode1(obj);
      #endif
    }

# UP: Berechnet den EQUAL-Hashcode eines Objekts.
# hashcode3(obj)
# Er ist nur bis zur nächsten GC oder der nächsten Modifizierung des Objekts
# gültig.
# Aus (equal X Y) folgt (= (hashcode3 X) (hashcode3 Y)).
# > obj: ein Objekt
# < ergebnis: Hashcode, eine 32-Bit-Zahl
  local uint32 hashcode3 (object obj);
# Hilfsfunktionen bei bekanntem Typ:
  # String -> Länge, erste max. 31 Zeichen, letztes Zeichen verwerten
  local uint32 hashcode_string (object obj);
  local uint32 hashcode_string(obj)
    var object obj;
    {
      var uintL len;
      var uintL offset;
      var object string = unpack_string_ro(obj,&len,&offset);
      var uint32 bish_code = 0x33DAE11FUL + len; # Länge verwerten
      if (len > 0) {
        SstringDispatch(string,
          {
            var const chart* ptr = &TheSstring(string)->data[offset];
            bish_code ^= (uint32)as_cint(ptr[len-1]); # letztes Zeichen dazu
            var uintC count = (len <= 31 ? len : 31); # min(len,31)
            dotimespC(count,count, {
              var uint32 next_code = (uint32)as_cint(*ptr++); # nächstes Zeichen
              bish_code = misch(bish_code,next_code); # dazunehmen
            });
          },
          {
            var const scint* ptr = &TheSmallSstring(string)->data[offset];
            bish_code ^= (uint32)(cint)(ptr[len-1]); # letztes Zeichen dazu
            var uintC count = (len <= 31 ? len : 31); # min(len,31)
            dotimespC(count,count, {
              var uint32 next_code = (uint32)(cint)(*ptr++); # nächstes Zeichen
              bish_code = misch(bish_code,next_code); # dazunehmen
            });
          }
          );
      }
      return bish_code;
    }
  # Bit-Vektor -> Länge, erste 16 Bits, letzte 16 Bits verwerten
  local uint32 hashcode_bvector (object obj);
  local uint32 hashcode_bvector(obj)
    var object obj;
    {
      var uintL len = vector_length(obj); # Länge
      var uintL index = 0;
      var object sbv = array_displace_check(obj,len,&index);
      # sbv der Datenvektor, index ist der Index in den Datenvektor.
      len = len << sbNvector_atype(sbv);
      #if BIG_ENDIAN_P && (varobject_alignment%2 == 0)
        # Bei Big-Endian-Maschinen kann man gleich mit 16 Bit auf einmal arbeiten
        # (sofern varobject_alignment durch 2 Byte teilbar ist):
        #define bitpack  16
        #define uint_bitpack  uint16
        #define get32bits_at  highlow32_at
      #else
        # Sonst kann man nur 8 Bit auf einmal nehmen:
        #define bitpack  8
        #define uint_bitpack  uint8
        #define get32bits_at(p) \
          (((((((uint32)((p)[0])<<8)|(uint32)((p)[1]))<<8)|(uint32)((p)[2]))<<8)|(uint32)((p)[3]))
      #endif
      var uint_bitpack* ptr = # Pointer aufs erste benutzte Word
          (uint_bitpack*)(&TheSbvector(sbv)->data[0]) + floor(index,bitpack);
      var uintL offset = index%bitpack; # Offset innerhalb des Word
      if (len <= 32) {
        # Länge <= 32 -> alle Bits nehmen:
        if (len == 0) {
          return 0x8FA1D564UL;
        } else {
          # 0<len<=32
          var uintL need = offset+len; # Brauche erstmal need Bits
          # need < 48
          var uint32 akku12 = 0; # 48-Bit-Akku, Teil 1 und 2
          var uint32 akku3 = 0; # 48-Bit-Akku, Teil 3
          #if (bitpack==16)
          if (need > 0) {
            akku12 = highlow32_0(*ptr++); # erste 16 Bits
            if (need > 16) {
              akku12 |= (uint32)(*ptr++); # nächste 16 Bits
              if (need > 32)
                akku3 = (uint32)(*ptr++); # letzte 16 Bits
            }
          }
          #endif
          #if (bitpack==8)
          if (need > 0) {
            akku12 = (uint32)(*ptr++)<<24; # erste 8 Bits
            if (need > 8) {
              akku12 |= (uint32)(*ptr++)<<16; # nächste 8 Bits
              if (need > 16) {
                akku12 |= (uint32)(*ptr++)<<8; # nächste 8 Bits
                if (need > 24) {
                  akku12 |= (uint32)(*ptr++); # nächste 8 Bits
                  if (need > 32) {
                    akku3 = (uint32)(*ptr++)<<8; # nächste 8 Bits
                    if (need > 40)
                      akku3 |= (uint32)(*ptr++); # letzte 8 Bits
                  }
                }
              }
            }
          }
          #endif
          # need Bits in akku12,akku3 um offset Bits nach links schieben:
          akku12 = (akku12 << offset) | (uint32)high16(akku3 << offset);
          # 32 Bits in akku12 fertig.
          # irrelevante Bits ausmaskieren:
          akku12 = akku12 & ~(bit(32-len)-1);
          # Länge verwerten:
          return akku12+len;
        }
      } else {
        # Länge > 32 -> erste und letzte 16 Bits nehmen:
        var uint32 akku12 = # 32-Bit-Akku
          get32bits_at(ptr) << offset; # enthält mind. die ersten 16 Bits
        offset += len; # End-Offset des Bitvektor
        ptr += floor(offset,bitpack); # zeigt aufs letzte benutzte Word
        offset = offset%bitpack; # End-Offset innerhalb des Word
        var uint32 akku34 = # 32-Bit-Akku
          get32bits_at(ptr-(16/bitpack)) << offset; # enthält mind. die letzten 16 Bits
        # erste 16, letzte 16 Bits herausgreifen und Länge verwerten:
        return highlow32(high16(akku12),high16(akku34)) + len;
      }
      #undef get32bits_at
      #undef uint_bitpack
      #undef bitpack
    }
  # EQUALP-Hashcode einer Pathname-Komponente.
  #if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
    local uint32 hashcode4 (object obj);
    #define hashcode_pathcomp(obj)  hashcode4(obj)
  #else
    #define hashcode_pathcomp(obj)  hashcode3(obj)
  #endif
  # Atom -> Fallunterscheidung nach Typ
  local uint32 hashcode3_atom (object obj);
  local uint32 hashcode3_atom(obj)
    var object obj;
    {
      #ifdef TYPECODES
      if (symbolp(obj)) { # ein Symbol?
        return hashcode1(obj); # ja -> EQ-Hashcode nehmen
      } elif (numberp(obj)) { # eine Zahl?
        return hashcode2(obj); # ja -> EQL-Hashcode nehmen
      } else {
        var tint type = typecode(obj) # Typinfo
                        & ~bit(notsimple_bit_t); # ob simple oder nicht, ist irrelevant
        if (type >= (sbvector_type & ~bit(notsimple_bit_t)) # Bit/Byte-Vektor ?
            && type <= (sb32vector_type & ~bit(notsimple_bit_t)))
          return hashcode_bvector(obj); # komponentenweise ansehen
        if (type == (sstring_type & ~bit(notsimple_bit_t))) # String ?
          return hashcode_string(obj); # komponentenweise ansehen
        if (xpathnamep(obj)) {
          # Pathname -> komponentenweise ansehen:
          check_SP();
          var uint32 bish_code = 0xB0DD939EUL;
          var object* ptr = &((Record)ThePathname(obj))->recdata[0];
          var uintC count;
          dotimespC(count,Xrecord_length(obj), {
            var uint32 next_code = hashcode_pathcomp(*ptr++); # Hashcode der nächsten Komponente
            bish_code = misch(bish_code,next_code); # dazunehmen
          });
          return bish_code;
        }
        # sonst: EQ-Hashcode nehmen (bei Characters ist ja EQL == EQ)
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
          case Rectype_Sstring: case Rectype_Imm_Sstring: case Rectype_Imm_SmallSstring: case Rectype_string:
            return hashcode_string(obj);
          case Rectype_Pathname:
          #ifdef LOGICAL_PATHNAMES
          case Rectype_Logpathname:
          #endif
            # Pathname -> komponentenweise ansehen:
            {
              check_SP();
              var uint32 bish_code = 0xB0DD939EUL;
              var object* ptr = &((Record)ThePathname(obj))->recdata[0];
              var uintC count;
              dotimespC(count,Xrecord_length(obj), {
                var uint32 next_code = hashcode_pathcomp(*ptr++); # Hashcode der nächsten Komponente
                bish_code = misch(bish_code,next_code); # dazunehmen
              });
              return bish_code;
            }
          default:
            break;
        }
      elif (immediate_number_p(obj)) {
        case_number: return hashcode2(obj);
      }
      return hashcode1(obj);
      #endif
    }
# Cons -> Inhalt bis zur Tiefe 4 ansehen:
# Jeweils Hashcode des CAR und Hashcode des CDR bestimmen
# und geshiftet kombinieren. Als Shifts passen z.B. 16,7,5,3,
# da {0,16} + {0,7} + {0,5} + {0,3} = {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
# aus 16 verschiedenen Elementen von {0,...,31} besteht.
  # Objekt, bei Cons nur bis Tiefe 0
  local uint32 hashcode3_cons0 (object obj);
  local uint32 hashcode3_cons0(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return hashcode3_atom(obj);
      } else {
        # Cons -> Hashcode := 1
        return 1;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 1
  local uint32 hashcode3_cons1 (object obj);
  local uint32 hashcode3_cons1(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return hashcode3_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = hashcode3_cons0(Car(obj));
        var uint32 code2 = hashcode3_cons0(Cdr(obj));
        return rotate_left(3,code1) ^ code2;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 2
  local uint32 hashcode3_cons2 (object obj);
  local uint32 hashcode3_cons2(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return hashcode3_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = hashcode3_cons1(Car(obj));
        var uint32 code2 = hashcode3_cons1(Cdr(obj));
        return rotate_left(5,code1) ^ code2;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 3
  local uint32 hashcode3_cons3 (object obj);
  local uint32 hashcode3_cons3(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return hashcode3_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = hashcode3_cons2(Car(obj));
        var uint32 code2 = hashcode3_cons2(Cdr(obj));
        return rotate_left(7,code1) ^ code2;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 4
  local uint32 hashcode3(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return hashcode3_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = hashcode3_cons3(Car(obj));
        var uint32 code2 = hashcode3_cons3(Cdr(obj));
        return rotate_left(16,code1) ^ code2;
      }
    }

# UP: Berechnet den EQUALP-Hashcode eines Objekts.
# hashcode4(obj)
# Er ist nur bis zur nächsten GC oder der nächsten Modifizierung des Objekts
# gültig.
# Aus (equalp X Y) folgt (= (hashcode4 X) (hashcode4 Y)).
  local uint32 hashcode4 (object obj);
# Hilfsfunktionen bei bekanntem Typ:
  # Character -> case-insensitive.
  #define hashcode4_char(c)  (0xCAAEACEFUL + (uint32)as_cint(up_case(c)))
  # Number: Mischung aus Exponent, Länge, erste 32 Bit
  extern uint32 hashcode4_real (object obj); # siehe REALELEM.D
  extern uint32 hashcode4_uint32 (uint32 x); # siehe REALELEM.D
  extern uint32 hashcode4_uint4 [16]; # siehe REALELEM.D
  # Vektoren: komponentenweise ansehen
  local uint32 hashcode4_vector_T (object dv, uintL index, uintL count, uint32 bish_code);
  local uint32 hashcode4_vector_Char (object dv, uintL index, uintL count, uint32 bish_code);
  local uint32 hashcode4_vector_Bit (object dv, uintL index, uintL count, uint32 bish_code);
  local uint32 hashcode4_vector_2Bit (object dv, uintL index, uintL count, uint32 bish_code);
  local uint32 hashcode4_vector_4Bit (object dv, uintL index, uintL count, uint32 bish_code);
  local uint32 hashcode4_vector_8Bit (object dv, uintL index, uintL count, uint32 bish_code);
  local uint32 hashcode4_vector_16Bit (object dv, uintL index, uintL count, uint32 bish_code);
  local uint32 hashcode4_vector_32Bit (object dv, uintL index, uintL count, uint32 bish_code);
  local uint32 hashcode4_vector (object dv, uintL index, uintL count, uint32 bish_code);
  local uint32 hashcode4_vector_T(dv,index,count,bish_code)
    var object dv;
    var uintL index;
    var uintL count;
    var uint32 bish_code;
    {
      if (count > 0) {
        check_SP();
        var const object* ptr = &TheSvector(dv)->data[index];
        dotimespL(count,count, {
          var uint32 next_code = hashcode4(*ptr++); # Hashcode der nächsten Komponente
          bish_code = misch(bish_code,next_code); # dazunehmen
        });
      }
      return bish_code;
    }
  local uint32 hashcode4_vector_Char(dv,index,count,bish_code)
    var object dv;
    var uintL index;
    var uintL count;
    var uint32 bish_code;
    {
      if (count > 0) {
        SstringDispatch(dv,
          {
            var const chart* ptr = &TheSstring(dv)->data[index];
            dotimespL(count,count, {
              var uint32 next_code = hashcode4_char(*ptr++); # nächstes Zeichen
              bish_code = misch(bish_code,next_code); # dazunehmen
            });
          },
          {
            var const scint* ptr = &TheSmallSstring(dv)->data[index];
            dotimespL(count,count, {
              var uint32 next_code = hashcode4_char(as_chart(*ptr++)); # nächstes Zeichen
              bish_code = misch(bish_code,next_code); # dazunehmen
            });
          }
          );
      }
      return bish_code;
    }
  local uint32 hashcode4_vector_Bit(dv,index,count,bish_code)
    var object dv;
    var uintL index;
    var uintL count;
    var uint32 bish_code;
    {
      if (count > 0) {
        var const uintB* ptr = &TheSbvector(dv)->data[index/8];
        dotimespL(count,count, {
          var uint32 next_code = hashcode4_uint4[(*ptr >> ((~index)%8)) & (bit(1)-1)]; # nächstes Byte
          bish_code = misch(bish_code,next_code); # dazunehmen
          index++;
          ptr += ((index%8)==0);
        });
      }
      return bish_code;
    }
  local uint32 hashcode4_vector_2Bit(dv,index,count,bish_code)
    var object dv;
    var uintL index;
    var uintL count;
    var uint32 bish_code;
    {
      if (count > 0) {
        var const uintB* ptr = &TheSbvector(dv)->data[index/4];
        dotimespL(count,count, {
          var uint32 next_code = hashcode4_uint4[(*ptr >> ((~index)%4)) & (bit(2)-1)]; # nächstes Byte
          bish_code = misch(bish_code,next_code); # dazunehmen
          index++;
          ptr += ((index%4)==0);
        });
      }
      return bish_code;
    }
  local uint32 hashcode4_vector_4Bit(dv,index,count,bish_code)
    var object dv;
    var uintL index;
    var uintL count;
    var uint32 bish_code;
    {
      if (count > 0) {
        var const uintB* ptr = &TheSbvector(dv)->data[index/2];
        dotimespL(count,count, {
          var uint32 next_code = hashcode4_uint4[(*ptr >> ((~index)%2)) & (bit(4)-1)]; # nächstes Byte
          bish_code = misch(bish_code,next_code); # dazunehmen
          index++;
          ptr += ((index%2)==0);
        });
      }
      return bish_code;
    }
  local uint32 hashcode4_vector_8Bit(dv,index,count,bish_code)
    var object dv;
    var uintL index;
    var uintL count;
    var uint32 bish_code;
    {
      if (count > 0) {
        var const uintB* ptr = &TheSbvector(dv)->data[index];
        dotimespL(count,count, {
          var uint32 next_code = hashcode4_uint32(*ptr++); # nächstes Byte
          bish_code = misch(bish_code,next_code); # dazunehmen
        });
      }
      return bish_code;
    }
  local uint32 hashcode4_vector_16Bit(dv,index,count,bish_code)
    var object dv;
    var uintL index;
    var uintL count;
    var uint32 bish_code;
    {
      if (count > 0) {
        var const uint16* ptr = &((uint16*)&TheSbvector(dv)->data[0])[index];
        dotimespL(count,count, {
          var uint32 next_code = hashcode4_uint32(*ptr++); # nächstes Byte
          bish_code = misch(bish_code,next_code); # dazunehmen
        });
      }
      return bish_code;
    }
  local uint32 hashcode4_vector_32Bit(dv,index,count,bish_code)
    var object dv;
    var uintL index;
    var uintL count;
    var uint32 bish_code;
    {
      if (count > 0) {
        var const uint32* ptr = &((uint32*)&TheSbvector(dv)->data[0])[index];
        dotimespL(count,count, {
          var uint32 next_code = hashcode4_uint32(*ptr++); # nächstes Byte
          bish_code = misch(bish_code,next_code); # dazunehmen
        });
      }
      return bish_code;
    }
  local uint32 hashcode4_vector(dv,index,count,bish_code)
    var object dv;
    var uintL index;
    var uintL count;
    var uint32 bish_code;
    {
      switch (Array_type(dv)) {
        case Array_type_svector: # Simple-Vector
          return hashcode4_vector_T(dv,index,count,bish_code);
        case Array_type_sbvector: # Simple-Bit-Vector
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
        case Array_type_sstring: # Simple-String
          return hashcode4_vector_Char(dv,index,count,bish_code);
        default: NOTREACHED
      }
    }
  # Atom -> Fallunterscheidung nach Typ
  local uint32 hashcode4_atom (object obj);
  local uint32 hashcode4_atom(obj)
    var object obj;
    {
      #ifdef TYPECODES
      if (symbolp(obj)) { # ein Symbol?
        return hashcode1(obj); # ja -> EQ-Hashcode nehmen
      } elif (numberp(obj)) { # eine Zahl?
        # ja -> EQUALP-Hashcode nehmen
        if (complexp(obj)) {
          var uint32 code1 = hashcode4_real(TheComplex(obj)->c_real);
          var uint32 code2 = hashcode4_real(TheComplex(obj)->c_imag);
          # Wichtig beim Kombinieren, wegen "complex canonicalization":
          # Ist imagpart=0.0, so ist der Hashcode = hashcode4_real(realpart).
          return code1 ^ rotate_left(5,code2);
        } else {
          return hashcode4_real(obj);
        }
      } else
        switch (typecode(obj))
      #else
      if (orecordp(obj))
        goto case_orecord;
      elif (immediate_number_p(obj)) {
        case_real: return hashcode4_real(obj);
      } elif (charp(obj))
        goto case_char;
      else
        return hashcode1(obj);
      switch (0)
      #endif
      {
        case_bvector: # Bit-Vektor
        case_b2vector: # 2Bit-Vektor
        case_b4vector: # 4Bit-Vektor
        case_b8vector: # 8Bit-Vektor
        case_b16vector: # 16Bit-Vektor
        case_b32vector: # 32Bit-Vektor
        case_string: # String
        case_vector: # (VECTOR T)
          # komponentenweise ansehen:
          {
            var uintL len = vector_length(obj); # Länge
            var uintL index = 0;
            var object dv = array_displace_check(obj,len,&index);
            # dv der Datenvektor, index ist der Index in den Datenvektor.
            var uint32 bish_code = 0x724BD24EUL + len; # Länge verwerten
            return hashcode4_vector(dv,index,len,bish_code);
          }
        case_mdarray: # Array vom Rang /=1
          # Rang und Dimensionen, dann komponentenweise ansehen:
          {
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
            case Rectype_Complex:
              {
                var uint32 code1 = hashcode4_real(TheComplex(obj)->c_real);
                var uint32 code2 = hashcode4_real(TheComplex(obj)->c_imag);
                # Wichtig beim Kombinieren, wegen "complex canonicalization":
                # Ist imagpart=0.0, so ist der Hashcode = hashcode4_real(realpart).
                return code1 ^ rotate_left(5,code2);
              }
            #endif
            default: ;
          }
          # Flags, Typ, Komponenten ansehen:
          {
            var uintC len = Record_length(obj);
            var uint32 bish_code = 0x03168B8D + (Record_flags(obj) << 24) + (Record_type(obj) << 16) + len;
            if (len > 0) {
              check_SP();
              var const object* ptr = &TheRecord(obj)->recdata[0];
              var uintC count;
              dotimespC(count,len, {
                var uint32 next_code = hashcode4(*ptr++); # Hashcode der nächsten Komponente
                bish_code = misch(bish_code,next_code); # dazunehmen
              });
            }
            if (Record_type(obj) >= rectype_limit) {
              var uintC xlen = Xrecord_xlength(obj);
              if (xlen > 0) {
                var const uintB* ptr = (uintB*)&TheRecord(obj)->recdata[len];
                dotimespC(xlen,xlen, {
                  var uint32 next_code = *ptr++; # nächstes Byte
                  bish_code = misch(bish_code,next_code); # dazunehmen
                });
              }
            }
            return bish_code;
          }
        case_char: # Character
          return hashcode4_char(char_code(obj));
        #ifndef TYPECODES
        case_symbol: # Symbol
        #endif
        case_closure: # Closure
        case_instance: # Instance
          # EQ-Hashcode nehmen
          return hashcode1(obj);
      }
    }
# Cons -> Inhalt bis zur Tiefe 4 ansehen:
# Jeweils Hashcode des CAR und Hashcode des CDR bestimmen
# und geshiftet kombinieren. Als Shifts passen z.B. 16,7,5,3,
# da {0,16} + {0,7} + {0,5} + {0,3} = {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
# aus 16 verschiedenen Elementen von {0,...,31} besteht.
  # Objekt, bei Cons nur bis Tiefe 0
  local uint32 hashcode4_cons0 (object obj);
  local uint32 hashcode4_cons0(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return hashcode4_atom(obj);
      } else {
        # Cons -> Hashcode := 1
        return 1;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 1
  local uint32 hashcode4_cons1 (object obj);
  local uint32 hashcode4_cons1(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return hashcode4_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = hashcode4_cons0(Car(obj));
        var uint32 code2 = hashcode4_cons0(Cdr(obj));
        return rotate_left(3,code1) ^ code2;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 2
  local uint32 hashcode4_cons2 (object obj);
  local uint32 hashcode4_cons2(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return hashcode4_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = hashcode4_cons1(Car(obj));
        var uint32 code2 = hashcode4_cons1(Cdr(obj));
        return rotate_left(5,code1) ^ code2;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 3
  local uint32 hashcode4_cons3 (object obj);
  local uint32 hashcode4_cons3(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return hashcode4_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = hashcode4_cons2(Car(obj));
        var uint32 code2 = hashcode4_cons2(Cdr(obj));
        return rotate_left(7,code1) ^ code2;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 4
  local uint32 hashcode4(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return hashcode4_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = hashcode4_cons3(Car(obj));
        var uint32 code2 = hashcode4_cons3(Cdr(obj));
        return rotate_left(16,code1) ^ code2;
      }
    }

# UP: Berechnet den Hashcode eines Objekts bezüglich einer Hashtabelle.
# hashcode(ht,obj)
# > ht: Hash-Table
# > obj: Objekt
# < ergebnis: Index in den Index-Vektor
  local uintL hashcode (object ht, object obj);
  local uintL hashcode(ht,obj)
    var object ht;
    var object obj;
    {
      # Hashcode je nach Hashtabellen-Typ:
      var uintB flags = record_flags(TheHashtable(ht));
      var uint32 code =
        (flags & bit(0) ? hashcode1(obj) : # EQ-Hashcode
         flags & bit(1) ? hashcode2(obj) : # EQL-Hashcode
         flags & bit(2) ? hashcode3(obj) : # EQUAL-Hashcode
         flags & bit(3) ? hashcode4(obj) : # EQUALP-Hashcode
         0 /*NOTREACHED*/
        );
      # dann durch SIZE dividieren:
      var uint32 rest;
      divu_3232_3232(code,posfixnum_to_L(TheHashtable(ht)->ht_size),_EMA_,rest = );
      return rest;
    }

# UP: Reorganisiert eine Hash-Tabelle, nachdem durch eine GC die Hashcodes
# der Keys verändert wurden.
# rehash(ht);
# > ht: Hash-Table
  local void rehash (object ht);
  local void rehash(ht)
    var object ht;
    {
      # Index-Vektor mit "nix" füllen:
      var object Ivektor = TheHashtable(ht)->ht_itable; # Index-Vektor
      {
        var object* ptr = &TheSvector(Ivektor)->data[0];
        var uintL count = posfixnum_to_L(TheHashtable(ht)->ht_size); # SIZE, >0
        dotimespL(count,count, { *ptr++ = nix; } );
      }
      # "Listen"struktur elementweise aufbauen:
      var object Nvektor = TheHashtable(ht)->ht_ntable; # Next-Vektor
      var object KVvektor = TheHashtable(ht)->ht_kvtable; # Key-Value-Vektor
      var object index = TheHashtable(ht)->ht_maxcount; # MAXCOUNT
      var uintL maxcount = posfixnum_to_L(index);
      var object* Nptr = &TheSvector(Nvektor)->data[maxcount];
      var object* KVptr = &TheSvector(KVvektor)->data[2*maxcount];
      var object freelist = nix;
      var object count = Fixnum_0;
      loop {
        # Schleife, läuft durch den Key-Value-Vektor und den Next-Vektor.
        # index = MAXCOUNT,...,0 (Fixnum),
        # Nptr = &TheSvector(Nptr)->data[index],
        # KVptr = &TheSvector(KVptr)->data[index],
        # freelist = bisherige Freiliste,
        # count = Paare-Zähler als Fixnum.
        if (eq(index,Fixnum_0)) # index=0 -> Schleife fertig
          break;
        index = fixnum_inc(index,-1); # index decrementieren
        KVptr -= 2;
        var object key = KVptr[0]; # nächster Key
        if (!eq(key,leer)) { # /= "leer" ?
          var uintL hashindex = hashcode(ht,key); # Hashcode dazu
          # "Liste", die bei Eintrag hashindex anfängt, um index erweitern:
          # Eintrag im Index-Vektor in den Next-Vektor kopieren
          # und durch index (ein Pointer auf diese Stelle) ersetzen:
          var object* Iptr = &TheSvector(Ivektor)->data[hashindex];
          *--Nptr = *Iptr; # Eintrag in den Next-Vektor kopieren
          *Iptr = index; # und durch Zeiger darauf ersetzen
          count = fixnum_inc(count,1); # mitzählen
        } else {
          # Freiliste im Next-Vektor verlängern:
          *--Nptr = freelist; freelist = index;
        }
      }
      TheHashtable(ht)->ht_freelist = freelist; # Freiliste abspeichern
      TheHashtable(ht)->ht_count = count; # Paare-Zahl abspeichern (konsistenzhalber)
      mark_ht_valid(TheHashtable(ht)); # Hashtabelle ist nun fertig organisiert
    }

# UP: Sucht ein Key in einer Hash-Tabelle.
# hash_lookup(ht,obj,&KVptr,&Nptr,&Iptr)
# > ht: Hash-Tabelle
# > obj: Objekt
# < falls gefunden: ergebnis=TRUE,
#     KVptr[0], KVptr[1] : Key, Value im Key-Value-Vektor,
#     *Nptr : zugehöriger Eintrag im Next-Vektor,
#     *Iptr : auf *Nptr zeigender vorheriger Index
# < falls nicht gefunden: ergebnis=FALSE,
#     *Iptr : zum Key gehöriger Eintrag im Index-Vektor
#             oder ein beliebiges Element der dort beginnenden "Liste"
  local boolean hash_lookup (object ht, object obj, object** KVptr_, object** Nptr_, object** Iptr_);
  local boolean hash_lookup(ht,obj,KVptr_,Nptr_,Iptr_)
    var object ht;
    var object obj;
    var object** KVptr_;
    var object** Nptr_;
    var object** Iptr_;
    {
      var uintB flags = record_flags(TheHashtable(ht));
      if (!ht_validp(TheHashtable(ht))) {
        # Hash-Tabelle muss erst noch reorganisiert werden
        rehash(ht);
      }
      var uintL hashindex = hashcode(ht,obj); # Hashcode berechnen
      var object* Nptr = # Pointer auf den aktuellen Eintrag
        &TheSvector(TheHashtable(ht)->ht_itable)->data[hashindex];
      loop {
        # "Liste" weiterverfolgen:
        if (eq(*Nptr,nix)) # "Liste" zu Ende -> nicht gefunden
          break;
        var uintL index = posfixnum_to_L(*Nptr); # nächster Index
        var object* Iptr = Nptr;
        Nptr = # Pointer auf Eintrag im Next-Vektor
          &TheSvector(TheHashtable(ht)->ht_ntable)->data[index];
        var object* KVptr = # Pointer auf Einträge im Key-Value-Vektor
          &TheSvector(TheHashtable(ht)->ht_kvtable)->data[2*index];
        var object key = KVptr[0];
        # key mit obj vergleichen:
        if (flags & bit(0) ? eq(key,obj) : # mit EQ vergleichen
            flags & bit(1) ? eql(key,obj) : # mit EQL vergleichen
            flags & bit(2) ? equal(key,obj) : # mit EQUAL vergleichen
            flags & bit(3) ? equalp(key,obj) : # mit EQUALP vergleichen
            FALSE
           ) {
          # Objekt obj gefunden
          *KVptr_ = KVptr; *Nptr_ = Nptr; *Iptr_ = Iptr; return TRUE;
        }
      }
      # nicht gefunden
      *Iptr_ = Nptr; return FALSE;
    }

# Macro: Trägt ein Key-Value-Paar in einer Hash-Tabelle ein.
# hash_store(key,value);
# > object ht: Hash-Tabelle
# > object freelist: Anfang der Freiliste im Next-Vektor, /= nix
# > key: Key
# > value: Value
# > object* Iptr: beliebiges Element der "Liste", die zu Key gehört
  #define hash_store(key,value)  \
    { var uintL index = posfixnum_to_L(freelist); # freier Index             \
      var object* Nptr = # Adresse des freien Eintrags im Next-Vektor        \
        &TheSvector(TheHashtable(ht)->ht_ntable)->data[index];               \
      var object* KVptr = # Adresse der freien Einträge im Key-Value-Vektor  \
        &TheSvector(TheHashtable(ht)->ht_kvtable)->data[2*index];            \
      set_break_sem_2(); # Vor Unterbrechungen schützen                      \
      # COUNT incrementieren:                                                \
      TheHashtable(ht)->ht_count = fixnum_inc(TheHashtable(ht)->ht_count,1); \
      # Freiliste verkürzen:                                                 \
      TheHashtable(ht)->ht_freelist = *Nptr;                                 \
      # Key und Value abspeichern:                                           \
      *KVptr++ = key; *KVptr++ = value;                                      \
      # freies Listenelement index in die "Liste" einfügen                   \
      # (nach resize an den Listenanfang, da Iptr in den Index-Vektor zeigt, \
      # sonst ans Listenende, da hash_lookup mit *Iptr=nix beendet wurde):   \
      *Nptr = *Iptr; *Iptr = freelist;                                       \
      clr_break_sem_2(); # Unterbrechungen wieder zulassen                   \
    }

# UP: Stellt die Zahlen und Vektoren für eine neue Hash-Tabelle bereit.
# prepare_resize(maxcount,mincount_threshold)
# > maxcount: gewünschte neue Größe MAXCOUNT
# > mincount_threshold: Short-Float MINCOUNT-THRESHOLD
# < ergebnis: maxcount
# < Stackaufbau: MAXCOUNT, SIZE, MINCOUNT,
#                Index-Vektor, Next-Vektor, Key-Value-Vektor.
# Erniedrigt STACK um 6
# can trigger GC
  local uintL prepare_resize (object maxcount, object mincount_threshold);
  local uintL prepare_resize(maxcount,mincount_threshold)
    var object maxcount;
    var object mincount_threshold;
    {
      # Überprüfe, ob maxcount ein nicht zu großes Fixnum >0 ist:
      if (!posfixnump(maxcount))
        goto fehler_maxcount;
      {
        var uintL maxcountL = posfixnum_to_L(maxcount);
        var uintL sizeL = 2*maxcountL+1;
        # SIZE ungerade, damit die Hashfunktion besser wird!
        if (!(sizeL <= (uintL)(bitm(oint_data_len)-1))) # sizeL sollte in ein Fixnum passen
          goto fehler_maxcount;
        # Zahlen auf den Stack:
        pushSTACK(maxcount); # MAXCOUNT
        pushSTACK(fixnum(sizeL)); # SIZE
        # MINCOUNT := (floor (* maxcount mincount-threshold))
        pushSTACK(maxcount); pushSTACK(mincount_threshold); funcall(L(mal),2);
        pushSTACK(value1); funcall(L(floor),1);
        pushSTACK(value1);
        # Stackaufbau: MAXCOUNT, SIZE, MINCOUNT.
        # neue Vektoren allozieren:
        pushSTACK(allocate_vector(sizeL)); # Index-Vektor beschaffen
        pushSTACK(allocate_vector(maxcountL)); # Next-Vektor beschaffen
        pushSTACK(allocate_vector(2*maxcountL)); # Key-Value-Vektor beschaffen
        # fertig.
        return maxcountL;
      }
     fehler_maxcount: # maxcount kein Fixnum oder zu groß
      pushSTACK(maxcount); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_hashtable_size)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(maxcount);
      fehler(type_error,
             GETTEXT("Hash table size ~ too large")
            );
    }

# UP: Vergrößert oder verkleinert eine Hash-Tabelle
# resize(ht,maxcount)
# > ht: Hash-Table
# > maxcount: gewünschte neue Größe MAXCOUNT
# < ergebnis: Hash-Table, EQ zur alten
# can trigger GC
  local object resize (object ht, object maxcount);
  local object resize(ht,maxcount)
    var object ht;
    var object maxcount;
    {
      pushSTACK(ht);
      var uintL maxcountL =
        prepare_resize(maxcount,TheHashtable(ht)->ht_mincount_threshold);
      # Ab jetzt keine GC mehr!
      var object KVvektor = popSTACK(); # neuer Key-Value-Vektor
      var object Nvektor = popSTACK(); # Next-Vektor
      var object Ivektor = popSTACK(); # Index-Vektor
      var object mincount = popSTACK(); # MINCOUNT
      var object size = popSTACK(); # SIZE
      maxcount = popSTACK();
      ht = popSTACK();
      # Neuen Key-Value-Vektor füllen:
      # Durch den alten Key-Value-Vektor durchlaufen und
      # alle Key-Value-Paare mit Key /= "leer" kopieren:
      # Zum Durchlaufen des alten Key-Value-Vektors:
      var uintL oldcount = posfixnum_to_L(TheHashtable(ht)->ht_maxcount);
      var object* oldKVptr = &TheSvector(TheHashtable(ht)->ht_kvtable)->data[0];
      # Zum Durchlaufen des neuen Key-Value-Vektors:
      var uintL count = maxcountL;
      var object* KVptr = &TheSvector(KVvektor)->data[0];
      # Zum Mitzählen:
      var object counter = Fixnum_0;
      dotimesL(oldcount,oldcount, {
        var object nextkey = *oldKVptr++; # nächster Key
        var object nextvalue = *oldKVptr++; # und Value
        if (!eq(nextkey,leer)) {
          # Eintrag in den neuen Key-Value-Vektor übernehmen:
          if (count==0) { # Ist der neue Vektor schon voll?
            # Der Platz reicht nicht!!
            pushSTACK(ht); # Hash-Table
            fehler(serious_condition,
                   GETTEXT("internal error occured while resizing ~")
                  );
          }
          count--;
          *KVptr++ = nextkey; *KVptr++ = nextvalue; # im neuen Vektor ablegen
          counter = fixnum_inc(counter,1); # und mitzählen
        }
      });
      # Noch count Paare des neuen Key-Value-Vektors als "leer" markieren:
      dotimesL(count,count, { *KVptr++ = leer; *KVptr++ = leer; } );
      # Hash-Tabelle modifizieren:
      set_break_sem_2(); # Vor Unterbrechungen schützen
      mark_ht_invalid(TheHashtable(ht)); # Tabelle muss erst noch reorganisiert werden
      TheHashtable(ht)->ht_size = size; # neues SIZE eintragen
      TheHashtable(ht)->ht_itable = Ivektor; # neuen Index-Vektor eintragen
      TheHashtable(ht)->ht_maxcount = maxcount; # neues MAXCOUNT eintragen
      TheHashtable(ht)->ht_freelist = nix; # Dummy als Freiliste
      TheHashtable(ht)->ht_ntable = Nvektor; # neuen Next-Vektor eintragen
      TheHashtable(ht)->ht_kvtable = KVvektor; # neuen Key-Value-Vektor eintragen
      TheHashtable(ht)->ht_count = counter; # COUNT eintragen (konsistenzhalber)
      TheHashtable(ht)->ht_mincount = mincount; # neues MINCOUNT eintragen
      clr_break_sem_2(); # Unterbrechungen wieder zulassen
      return ht;
    }

# Macro: Vergrößert eine Hash-Tabelle so lange, bis freelist /= nix
# hash_prepare_store();
# > object key: Key (im Stack)
# > object ht: Hash-Tabelle
# < object ht: Hash-Tabelle
# < object freelist: Anfang der Freiliste im Next-Vektor, /= nix
# < object* Iptr: beliebiges Element der "Liste", die zu Key gehört
# can trigger GC
  #define hash_prepare_store(key)  \
    { retry:                                                                    \
      freelist = TheHashtable(ht)->ht_freelist;                                 \
      if (eq(freelist,nix)) # Freiliste = leere "Liste" ?                       \
        # ja -> muss die Hash-Tabelle vergrößern:                               \
        { pushSTACK(ht); # Hashtable retten                                     \
          # neues maxcount ausrechnen:                                          \
          pushSTACK(TheHashtable(ht)->ht_maxcount);                             \
          pushSTACK(TheHashtable(ht)->ht_rehash_size); # REHASH-SIZE (>1)       \
          funcall(L(mal),2); # (* maxcount rehash-size), ist > maxcount         \
          pushSTACK(value1);                                                    \
          funcall(L(ceiling),1); # (ceiling ...), Integer > maxcount            \
          ht = resize(popSTACK(),value1); # Tabelle vergrößern                  \
          rehash(ht); # und reorganisieren                                      \
          # Adresse des Eintrags im Index-Vektor neu ausrechnen:                \
         {var uintL hashindex = hashcode(ht,key); # Hashcode berechnen          \
          Iptr = &TheSvector(TheHashtable(ht)->ht_itable)->data[hashindex];     \
          goto retry;                                                           \
        }}                                                                      \
    }

# UP: Löscht den Inhalt einer Hash-Tabelle.
# clrhash(ht);
# > ht: Hash-Tabelle
  local void clrhash (object ht);
  local void clrhash(ht)
    var object ht;
    {
      set_break_sem_2(); # Vor Unterbrechungen schützen
      {
        var uintL count = posfixnum_to_L(TheHashtable(ht)->ht_maxcount);
        if (count > 0) {
          var object* KVptr = &TheSvector(TheHashtable(ht)->ht_kvtable)->data[0];
          dotimespL(count,count, { # in jedem Eintrag
            *KVptr++ = leer; *KVptr++ = leer; # Key und Value leeren
          });
        }
      }
      TheHashtable(ht)->ht_count = Fixnum_0; # COUNT := 0
      mark_ht_invalid(TheHashtable(ht)); # Hashtabelle später noch reorganisieren
      clr_break_sem_2(); # Unterbrechungen wieder zulassen
    }

# (MAKE-HASH-TABLE [:test] [:size] [:rehash-size] [:rehash-threshold]
#                  [:initial-contents]), CLTL S. 283
LISPFUN(make_hash_table,0,0,norest,key,5,\
        (kw(initial_contents),\
         kw(test),kw(size),kw(rehash_size),kw(rehash_threshold)) )
  {
    # Dem Rehash-Threshold entspricht in unserer Implementation das
    # Verhältnis MAXCOUNT : SIZE = ca. 1 : 2.
    # Wir ignorieren das rehash-threshold-Argument, da sowohl zu große als
    # auch zu kleine Werte davon schädlich wären: 0.99 bewirkt im Durchschnitt
    # zu lange Zugriffszeiten; 0.00001 bewirkt, dass SIZE = MAXCOUNT/threshold
    # zu schnell ein Bignum werden könnte.
    # Das zusätzliche initial-contents-Argument ist eine Aliste = Liste von
    # (Key . Value) - Paaren, mit denen die Tabelle initialisiert wird.
    # Stackaufbau: initial-contents, test, size, rehash-size, rehash-threshold.
    var uintB flags;
    # test-Argument überprüfen:
    {
      var object test = STACK_3;
      if (eq(test,unbound))
        flags = bit(1); # EQL als Default
      elif (eq(test,S(eq)) || eq(test,L(eq)))
        flags = bit(0); # EQ
      elif (eq(test,S(eql)) || eq(test,L(eql)))
        flags = bit(1); # EQL
      elif (eq(test,S(equal)) || eq(test,L(equal)))
        flags = bit(2); # EQUAL
      elif (eq(test,S(equalp)) || eq(test,L(equalp)))
        flags = bit(3); # EQUALP
      else {
        pushSTACK(test); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_hashtable_test)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(test);
        pushSTACK(S(make_hash_table));
        fehler(type_error,
               GETTEXT("~: illegal :TEST argument ~")
              );
      }
    }
    # flags enthält die Flags zum Test.
    # size-Argument überprüfen:
    {
      var object size = STACK_2;
      if (eq(size,unbound)) {
        STACK_2 = Fixnum_1; # 1 als Default
      } else {
        if (!posfixnump(size)) {
          pushSTACK(size); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_posfixnum)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(size);
          pushSTACK(S(make_hash_table));
          fehler(type_error,
                 GETTEXT("~: :SIZE argument should be a fixnum >=0, not ~")
                );
        }
        # size ist ein Fixnum >=0
        if (eq(size,Fixnum_0))
          STACK_2 = Fixnum_1; # aus 0 mache 1
      }
    }
    # size ist jetzt ein Fixnum >0.
    # rehash-size überprüfen:
    {
      if (eq(STACK_1,unbound)) {
        # Default-Rehash-Size = 1.5s0
        STACK_1 = make_SF(0,SF_exp_mid+1,(bit(SF_mant_len)*3)/2);
      } else {
        if (!floatp(STACK_1)) { # Float ist OK
          if (!posfixnump(STACK_1)) { # sonst sollte es ein Fixnum >=0 sein
           fehler_rehash_size:
            pushSTACK(STACK_1); # Wert für Slot DATUM von TYPE-ERROR
            pushSTACK(O(type_hashtable_rehash_size)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
            pushSTACK(STACK_(1+2));
            pushSTACK(S(make_hash_table));
            fehler(type_error,
                   GETTEXT("~: :REHASH-SIZE argument should be a float > 1, not ~")
                  );
          }
          # Da es sinnlos ist, eine Tabelle immer nur um eine feste
          # Anzahl von Elementen größer zu machen (führt zu katastrophaler
          # Effizienz), wird rehash-size := min(1 + rehash-size/size , 2.0)
          # gesetzt.
          pushSTACK(STACK_1); # rehash-size
          pushSTACK(STACK_(2+1)); # size
          funcall(L(durch),2); # (/ rehash-size size)
          pushSTACK(value1);
          funcall(L(einsplus),1); # (1+ ...)
          pushSTACK(value1);
          pushSTACK(make_SF(0,SF_exp_mid+2,bit(SF_mant_len))); # 2.0s0
          funcall(L(min),2); # (MIN ... 2.0s0)
          STACK_1 = value1; # =: rehash-size
        }
        # (> rehash-size 1) überprüfen:
        pushSTACK(STACK_1); # rehash-size
        pushSTACK(Fixnum_1); # 1
        funcall(L(groesser),2); # (> rehash-size 1)
        if (nullp(value1)) goto fehler_rehash_size;
        # rehash-size in ein Short-Float umwandeln:
        pushSTACK(STACK_1); # rehash-size
        pushSTACK(SF_0); # 0.0s0
        funcall(L(float),2); # (FLOAT rehash-size 0.0s0) = (COERCE rehash-size 'SHORT-FLOAT)
        # (>= rehash-size 1.125s0) erzwingen:
        pushSTACK(value1);
        pushSTACK(make_SF(0,SF_exp_mid+1,(bit(SF_mant_len)/8)*9)); # 1.125s0
        funcall(L(max),2); # (max rehash-size 1.125s0)
        STACK_1 = value1; # =: rehash-size
      }
    }
    # rehash-size ist ein Short-Float >= 1.125 .
    # rehash-threshold überprüfen: sollte ein Float >=0, <=1 sein
    {
      var object rehash_threshold = STACK_0;
      if (!eq(rehash_threshold,unbound)) { # nicht angegeben -> OK
        if (!floatp(rehash_threshold)) {
         fehler_rehash_threshold:
          # Argument bereits in STACK_0, Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_hashtable_rehash_threshold)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(STACK_1);
          pushSTACK(S(make_hash_table));
          fehler(type_error,
                 GETTEXT("~: :REHASH-THRESHOLD argument should be a float between 0 and 1, not ~")
                );
        }
        pushSTACK(Fixnum_1);
        pushSTACK(rehash_threshold);
        pushSTACK(Fixnum_0);
        funcall(L(grgleich),3); # (>= 1 rehash-threshold 0)
        if (nullp(value1)) goto fehler_rehash_threshold;
      }
    }
    # Nun sind alle Argumente überprüft.
    # Ist das initial-contents-Argument angegeben, so wird
    # size := (max size (length initial-contents)) gesetzt, damit nachher beim
    # Eintragen des initial-contents die Tabelle nicht vergrößert werden muss:
    {
      var object initial_contents = STACK_4;
      if (!eq(initial_contents,unbound)) { # angegeben ?
        var uintL initial_length = llength(initial_contents); # Länge der Aliste
        if (initial_length > posfixnum_to_L(STACK_2)) # > size ?
          STACK_2 = fixnum(initial_length); # ja -> size vergrößern
      }
    }
    # size ist ein Fixnum >0, >= (length initial-contents) .
    # MINCOUNT-THRESHOLD = 1/rehash-size^2 errechnen:
    {
      var object rehash_size = STACK_1;
      pushSTACK(rehash_size);
      pushSTACK(rehash_size);
      funcall(L(mal),2); # (* rehash-size rehash-size)
      pushSTACK(value1);
      funcall(L(durch),1); # (/ ...)
      STACK_0 = value1;
    }
    # Stackaufbau: initial-contents, test, size, rehash-size, mincount-threshold.
    # Vektoren beschaffen usw., mit size als MAXCOUNT:
    prepare_resize(STACK_2,STACK_0);
    var object ht = allocate_hash_table(); # neue Hash-Tabelle
    # füllen:
    TheHashtable(ht)->ht_kvtable = popSTACK(); # Key-Value-Vektor
    TheHashtable(ht)->ht_ntable = popSTACK(); # Next-Vektor
    TheHashtable(ht)->ht_itable = popSTACK(); # Index-Vektor
    TheHashtable(ht)->ht_mincount = popSTACK(); # MINCOUNT
    TheHashtable(ht)->ht_size = popSTACK(); # SIZE
    TheHashtable(ht)->ht_maxcount = popSTACK(); # MAXCOUNT
    # Stackaufbau: initial-contents, test, size, rehash-size, mincount-threshold.
    TheHashtable(ht)->ht_mincount_threshold = popSTACK(); # MINCOUNT-THRESHOLD
    TheHashtable(ht)->ht_rehash_size = popSTACK(); # REHASH-SIZE
    TheHashtable(ht)->ht_freelist = nix; # Dummy als Freiliste
    record_flags_replace(TheHashtable(ht), flags);
    clrhash(ht); # Tabelle leeren, COUNT := 0
    skipSTACK(2);
    # Stackaufbau: initial-contents.
    {
      var object alist = popSTACK(); # initial-contents
      while (consp(alist)) { # Wenn es angegeben war, solange es ein Cons ist:
        var object next = Car(alist); # Alistenelement
        if (consp(next)) { # ein Cons (Key . Value) ?
          # (SYSTEM::PUTHASH (car next) hashtable (cdr next)) ausführen,
          # wobei die Tabelle nicht wachsen kann:
          var object key = Car(next);
          var object* KVptr;
          var object* Nptr;
          var object* Iptr;
          if (hash_lookup(ht,key,&KVptr,&Nptr,&Iptr)) { # in der Hash-Tabelle suchen
            # schon gefunden -> war in der Aliste weiter links schon
            # enthalten, und in Alisten verdeckt die erste Assoziation
            # (links) alle anderen Assoziationen zum selben Key.
          } else {
            # nicht gefunden -> neuen Eintrag basteln:
            var object freelist = # Anfang der Freiliste im Next-Vektor
              TheHashtable(ht)->ht_freelist;
            if (eq(freelist,nix)) { # leere "Liste" ?
              pushSTACK(ht); # Hash-Tabelle
              pushSTACK(S(make_hash_table));
              fehler(serious_condition,
                     GETTEXT("~: internal error while building ~")
                    );
            }
            hash_store(key,Cdr(next)); # Eintrag basteln
          }
        }
        alist = Cdr(alist);
      }
    }
    value1 = ht; mv_count=1; # Hash-Tabelle als Wert
  }

# UP: Sucht ein Objekt in einer Hash-Tabelle.
# gethash(obj,ht)
# > obj: Objekt, als Key
# > ht: Hash-Tabelle
# < ergebnis: zugehöriger Value, falls gefunden, nullobj sonst
  global object gethash (object obj, object ht);
  global object gethash(obj,ht)
    var object obj;
    var object ht;
    {
      var object* KVptr;
      var object* Nptr;
      var object* Iptr;
      if (hash_lookup(ht,obj,&KVptr,&Nptr,&Iptr))
        return KVptr[1]; # gefunden -> Value
      else
        return nullobj;
    }

# Fehler, wenn ein Argument keine Hash-Table ist
# fehler_hashtable(obj);
# > obj: Objekt
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_hashtable, (object obj));
  local void fehler_hashtable(obj)
    var object obj;
    {
      pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(hash_table)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ is not a hash-table")
            );
    }

# (GETHASH key hashtable [default]), CLTL S. 284
LISPFUN(gethash,2,1,norest,nokey,0,NIL)
  {
    var object ht = STACK_1; # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    var object* KVptr;
    var object* Nptr;
    var object* Iptr;
    # Key STACK_2 in der Hash-Tabelle suchen:
    if (hash_lookup(ht,STACK_2,&KVptr,&Nptr,&Iptr)) {
      # gefunden -> Value als Wert:
      value1 = KVptr[1]; value2 = T; mv_count=2; # und T als 2. Wert
      skipSTACK(3);
    } else {
      # nicht gefunden -> default oder NIL als Wert
      var object def = popSTACK(); # default
      value1 = (eq(def,unbound) ? NIL : def); value2 = NIL; mv_count=2; # NIL als 2. Wert
      skipSTACK(2);
    }
  }

# (SYSTEM::PUTHASH key hashtable value) =
# (SETF (GETHASH key hashtable) value), CLTL S. 284
LISPFUNN(puthash,3)
  {
    var object ht = STACK_1; # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    var object* KVptr;
    var object* Nptr;
    var object* Iptr;
    # Key STACK_2 in der Hash-Tabelle suchen:
    if (hash_lookup(ht,STACK_2,&KVptr,&Nptr,&Iptr)) {
      # gefunden -> Value ersetzen:
      value1 = KVptr[1] = popSTACK(); mv_count=1; skipSTACK(2);
    } else {
      # nicht gefunden -> neuen Eintrag basteln:
      var object freelist;
      hash_prepare_store(STACK_2);
      hash_store(STACK_2,STACK_0); # Eintrag basteln
      value1 = popSTACK(); mv_count=1; # value als Wert
      skipSTACK(2);
    }
  }

# UP: Sucht ein Key in einer Hash-Tabelle und liefert den vorigen Wert.
# shifthash(ht,obj,value) == (SHIFTF (GETHASH obj ht) value)
# > ht: Hash-Tabelle
# > obj: Objekt
# > value: neuer Wert
# < ergebnis: alter Wert
# can trigger GC
  global object shifthash (object ht, object obj, object value);
  global object shifthash(ht,obj,value)
    var object ht;
    var object obj;
    var object value;
    {
      var object* KVptr;
      var object* Nptr;
      var object* Iptr;
      # Key obj in der Hash-Tabelle suchen:
      if (hash_lookup(ht,obj,&KVptr,&Nptr,&Iptr)) {
        # gefunden -> Value ersetzen:
        var object oldvalue = KVptr[1];
        KVptr[1] = value;
        return oldvalue;
      } else {
        # nicht gefunden -> neuen Eintrag basteln:
        pushSTACK(obj); pushSTACK(value); # Key und Value retten
        var object freelist;
        hash_prepare_store(STACK_1);
        hash_store(STACK_1,STACK_0); # Eintrag basteln
        skipSTACK(2);
        return NIL; # Default für den alten Wert ist NIL
      }
    }

# (REMHASH key hashtable), CLTL S. 284
LISPFUNN(remhash,2)
  {
    var object ht = popSTACK(); # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    var object key = popSTACK(); # key-Argument
    var object* KVptr;
    var object* Nptr;
    var object* Iptr;
    # Key in der Hash-Tabelle suchen:
    if (hash_lookup(ht,key,&KVptr,&Nptr,&Iptr)) {
      # gefunden -> aus der Hashtabelle streichen:
      var object index = *Iptr; # Index im Next-Vektor
      # mit Nptr = &TheSvector(TheHashtable(ht)->ht_ntable)->data[index]
      # und KVptr = &TheSvector(TheHashtable(ht)->ht_kvtable)->data[2*index]
      set_break_sem_2(); # Vor Unterbrechungen schützen
      *Iptr = *Nptr; # "Liste" verkürzen
      *KVptr++ = leer; *KVptr = leer; # Key und Value leeren
      # Freiliste verlängern:
      *Nptr = TheHashtable(ht)->ht_freelist;
      TheHashtable(ht)->ht_freelist = index;
      # COUNT decrementieren:
      TheHashtable(ht)->ht_count = fixnum_inc(TheHashtable(ht)->ht_count,-1);
      clr_break_sem_2(); # Unterbrechungen wieder zulassen
      # Bei COUNT < MINCOUNT die Hash-Tabelle verkleinern:
      if (posfixnum_to_L(TheHashtable(ht)->ht_count) < posfixnum_to_L(TheHashtable(ht)->ht_mincount)) {
        # Hash-Tabelle verkleinern:
        # maxcount := (max (floor (/ maxcount rehash-size)) 1)
        pushSTACK(ht); # Hashtable retten
        pushSTACK(TheHashtable(ht)->ht_maxcount);
        pushSTACK(TheHashtable(ht)->ht_rehash_size); # REHASH-SIZE (>1)
        funcall(L(durch),2); # (/ maxcount rehash-size), ist < maxcount
        pushSTACK(value1);
        funcall(L(floor),1); # (floor ...), ein Integer >=0, < maxcount
        var object maxcount = value1;
        if (eq(maxcount,Fixnum_0))
          maxcount = Fixnum_1; # aus 0 mache 1
        resize(popSTACK(),maxcount); # Tabelle verkleinern
      }
      value1 = T; mv_count=1; # T als Wert
    } else {
      # nicht gefunden
      value1 = NIL; mv_count=1; # NIL als Wert
    }
  }

# (MAPHASH function hashtable), CLTL S. 285
LISPFUNN(maphash,2)
  {
    var object ht = STACK_0; # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    # Durch den Key-Value-Vektor von hinten durchlaufen und
    # für alle Key-Value-Paare mit Key /= "leer" die Funktion aufrufen:
    var uintL index = 2*posfixnum_to_L(TheHashtable(ht)->ht_maxcount);
    STACK_0 = TheHashtable(ht)->ht_kvtable; # Key-Value-Vektor
    # Stackaufbau: function, Key-Value-Vektor.
    loop {
      if (index==0)
        break;
      index -= 2;
      var object* KVptr = &TheSvector(STACK_0)->data[index];
      if (!eq(KVptr[0],leer)) { # Key /= "leer" ?
        pushSTACK(KVptr[0]); # Key als 1. Argument
        pushSTACK(KVptr[1]); # Value als 2. Argument
        funcall(STACK_(1+2),2); # (FUNCALL function Key Value)
      }
    }
    skipSTACK(2);
    value1 = NIL; mv_count=1; # NIL als Wert
  }

# (CLRHASH hashtable), CLTL S. 285
LISPFUNN(clrhash,1)
  {
    var object ht = popSTACK(); # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    clrhash(ht); # Tabelle leeren
    # Bei MINCOUNT > 0 die Hash-Tabelle verkleinern:
    if (!eq(TheHashtable(ht)->ht_mincount,Fixnum_0))
      ht = resize(ht,Fixnum_1); # auf MAXCOUNT:=1 verkleinern, so dass MINCOUNT:=0
    value1 = ht; mv_count=1; # Hash-Tabelle als Wert
  }

# (HASH-TABLE-COUNT hashtable), CLTL S. 285, CLtL2 S. 439
LISPFUNN(hash_table_count,1)
  {
    var object ht = popSTACK(); # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    value1 = TheHashtable(ht)->ht_count; mv_count=1; # Fixnum COUNT als Wert
  }

# (HASH-TABLE-REHASH-SIZE hashtable), CLtL2 S. 441, dpANS p. 18-7
LISPFUNN(hash_table_rehash_size,1)
  {
    var object ht = popSTACK(); # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    value1 = TheHashtable(ht)->ht_rehash_size; mv_count=1; # Short-Float REHASH-SIZE als Wert
  }

# (HASH-TABLE-REHASH-THRESHOLD hashtable), CLtL2 S. 441, dpANS p. 18-8
LISPFUNN(hash_table_rehash_threshold,1)
  {
    var object ht = popSTACK(); # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    # Da MAKE-HASH-TABLE das :REHASH-THRESHOLD Argument ignoriert, ist der
    # Wert hier egal und willkürlich.
    value1 = make_SF(0,SF_exp_mid+0,(bit(SF_mant_len)/2)*3); mv_count=1; # 0.75s0 als Wert
  }

# (HASH-TABLE-SIZE hashtable), CLtL2 S. 441, dpANS p. 18-9
LISPFUNN(hash_table_size,1)
  {
    var object ht = popSTACK(); # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    value1 = TheHashtable(ht)->ht_maxcount; mv_count=1; # Fixnum MAXCOUNT als Wert
  }

# (HASH-TABLE-TEST hashtable), CLtL2 S. 441, dpANS p. 18-9
LISPFUNN(hash_table_test,1)
  {
    var object ht = popSTACK(); # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    var uintB flags = record_flags(TheHashtable(ht));
    value1 = (flags & bit(0) ? S(eq) : # EQ
              flags & bit(1) ? S(eql) : # EQL
              flags & bit(2) ? S(equal) : # EQUAL
              flags & bit(3) ? S(equalp) : # EQUALP
              NIL /*NOTREACHED*/
             );
    mv_count=1; # Symbol als Wert
  }

# Hilfsfunktionen für WITH-HASH-TABLE-ITERATOR, CLTL2 S. 439:
# (SYSTEM::HASH-TABLE-ITERATOR hashtable) liefert einen internen Zustand
# für das Iterieren durch eine Hash-Tabelle.
# (SYSTEM::HASH-TABLE-ITERATE internal-state) iteriert durch eine Hash-Tabelle
# um eins weiter, verändert dabei internal-state und liefert: 3 Werte
# T, key, value des nächsten Hash-Tabellen-Eintrags bzw. 1 Wert NIL am Schluss.

LISPFUNN(hash_table_iterator,1)
  {
    var object ht = STACK_0; # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    # Ein interner Zustand besteht aus dem Key-Value-Vektor und einem Index.
    STACK_0 = TheHashtable(ht)->ht_kvtable; # Key-Value-Vektor
    var object maxcount = TheHashtable(ht)->ht_maxcount; # maxcount
    var object state = allocate_cons();
    Car(state) = popSTACK(); # Key-Value-Vektor als Car
    Cdr(state) = maxcount; # maxcount als Cdr
    value1 = state; mv_count=1; # state als Wert
  }

LISPFUNN(hash_table_iterate,1)
  {
    var object state = popSTACK(); # interner Zustand
    if (consp(state)) { # hoffentlich ein Cons
      var object table = Car(state); # Key-Value-Vektor
      loop {
        var uintL index = posfixnum_to_L(Cdr(state));
        if (index==0) # index=0 -> keine Elemente mehr
          break;
        Cdr(state) = fixnum_inc(Cdr(state),-1); # Index decrementieren
        var object* KVptr = &TheSvector(table)->data[2*index-2];
        if (!eq(KVptr[0],leer)) { # Key /= "leer" ?
          value2 = KVptr[0]; # Key als 2. Wert
          value3 = KVptr[1]; # Value als 3. Wert
          value1 = T; mv_count=3; return;
        }
      }
    }
    value1 = NIL; mv_count=1; return; # 1 Wert NIL
  }

# (CLOS::CLASS-GETHASH ht object) ist wie (GETHASH (CLASS-OF object) ht).
LISPFUNN(class_gethash,2)
  {
    var object ht = STACK_1; # hashtable-Argument
    if (!hash_table_p(ht)) # überprüfen
      fehler_hashtable(ht);
    C_class_of(); # value1 := (CLASS-OF object)
    var object* KVptr;
    var object* Nptr;
    var object* Iptr;
    # Key value1 in der Hash-Tabelle suchen:
    if (hash_lookup(ht,value1,&KVptr,&Nptr,&Iptr)) {
      # gefunden -> Value als Wert:
      value1 = KVptr[1]; value2 = T; mv_count=2; # und T als 2. Wert
    } else {
      # nicht gefunden -> NIL als Wert
      value1 = NIL; value2 = NIL; mv_count=2; # NIL als 2. Wert
    }
    skipSTACK(1);
  }

# (CLOS::CLASS-TUPLE-GETHASH ht object1 ... objectn)
# ist wie (GETHASH (funcall (hash-tuple-function n) class1 ... classn) ht)
# mit classi = (CLASS-OF objecti).
# Dabei sei n>0, ht eine EQUAL-Hashtabelle und (hash-tuple-function n) wie in
# clos.lsp definiert.
# Diese Funktion ist der Kern des Dispatch für generische Funktionen. Sie soll
# darum schnell sein und nicht consen.
  # Für 1 < n <= 16 ist
  #   (hash-tuple-function n ...) =
  #   (cons (hash-tuple-function n1 ...) (hash-tuple-function n2 ...))
    local const uintC tuple_half_1 [17] = {0,0,1,1,2,2,2,3,4,4,4,4,4,5,6,7,8};
    local const uintC tuple_half_2 [17] = {0,0,1,2,2,3,4,4,4,5,6,7,8,8,8,8,8};
  # Hilfsfunktion: Hashcode einer Reihe von Atomen berechnen, so als wären
  # sie per (hash-tuple-function n) zusammengeconst:
    local uint32 hashcode_tuple (uintC n, const object* args_pointer, uintC depth);
    local uint32 hashcode_tuple(n,args_pointer,depth)
      var uintC n; # n > 0
      var const object* args_pointer;
      var uintC depth;
      {
        if (n==1) {
          return hashcode1(Next(args_pointer)); # hashcode3_atom für Klassen
        } elif (n<=16) {
          var uintC n1 = tuple_half_1[n];
          var uintC n2 = tuple_half_2[n]; # n1 + n2 = n
          var uint32 code1 = hashcode_tuple(n1,args_pointer,depth+1);
          var uint32 code2 = hashcode_tuple(n2,args_pointer STACKop -(uintP)n1,depth+1);
          switch (depth) {
            case 0: code1 = rotate_left(16,code1); break;
            case 1: code1 = rotate_left(7,code1); break; # vgl. hashcode3_cons3
            case 2: code1 = rotate_left(5,code1); break; # vgl. hashcode3_cons2
            case 3: code1 = rotate_left(3,code1); break; # vgl. hashcode3_cons1
            default: NOTREACHED
          }
          return code1 ^ code2;
        } else {
          # n>16, depth=0
          var uint32 code1 = hashcode_tuple(8,args_pointer,1);
          var uint32 code2 = hashcode_tuple(4,args_pointer STACKop -8,2);
          var uint32 code3 = hashcode_tuple(2,args_pointer STACKop -12,3);
          var uint32 code4 = hashcode_tuple(1,args_pointer STACKop -14,4);
          var uint32 code = 1; # vgl. hashcode3_cons0
          code = rotate_left(3,code4) ^ code; # vgl. hashcode3_cons1
          code = rotate_left(5,code3) ^ code; # vgl. hashcode3_cons2
          code = rotate_left(7,code2) ^ code; # vgl. hashcode3_cons3
          code = rotate_left(16,code1) ^ code;
          return code;
        }
      }
  # Hilfsfunktion: Vergleich eines Objekts mit einer Reihe von Atomen, so als
  # wären sie per (hash-tuple-function n) zusammengeconst:
    local boolean equal_tuple (object obj, uintC n, const object* args_pointer);
    local boolean equal_tuple(obj,n,args_pointer)
      var object obj;
      var uintC n; # n > 0
      var const object* args_pointer;
      {
        if (n==1) {
          if (eq(obj,Next(args_pointer)))
            return TRUE;
          else
            return FALSE;
        } elif (n<=16) {
          if (consp(obj)) {
            var uintC n1 = tuple_half_1[n];
            var uintC n2 = tuple_half_2[n]; # n1 + n2 = n
            if (equal_tuple(Car(obj),n1,args_pointer)
                && equal_tuple(Cdr(obj),n2,args_pointer STACKop -(uintP)n1)
               )
              return TRUE;
          }
          return FALSE;
        } else {
          # n>16
          if (consp(obj) && equal_tuple(Car(obj),8,args_pointer)) {
            obj = Cdr(obj);
            if (consp(obj) && equal_tuple(Car(obj),4,args_pointer STACKop -8)) {
              obj = Cdr(obj);
              if (consp(obj) && equal_tuple(Car(obj),2,args_pointer STACKop -12)) {
                obj = Cdr(obj);
                n-=14; args_pointer skipSTACKop -14;
                # obj mit einer Liste der weiteren Atome vergleichen:
                dotimespC(n,n, {
                  if (!(consp(obj) && eq(Car(obj),Next(args_pointer))))
                    return FALSE;
                  obj = Cdr(obj); args_pointer skipSTACKop -1;
                });
                if (nullp(obj))
                  # Vergleich erfüllt
                  return TRUE;
              }
            }
          }
          return FALSE;
        }
      }

LISPFUN(class_tuple_gethash,2,0,rest,nokey,0,NIL)
{
  argcount++; rest_args_pointer skipSTACKop 1; # Argumente: ht {object}+
  # Zuerst CLASS-OF auf die einzelnen Argumente anwenden:
  {
    var object* arg_pointer = rest_args_pointer;
    var uintC count;
    dotimespC(count,argcount, {
      pushSTACK(Next(arg_pointer)); C_class_of(); # (CLASS-OF arg)
      NEXT(arg_pointer) = value1; # =: arg
    });
  }
  var object ht = Before(rest_args_pointer); # hashtable-Argument
  if (!hash_table_p(ht)) # überprüfen
    fehler_hashtable(ht);
  if (!ht_validp(TheHashtable(ht))) {
    # Hash-Tabelle muss erst noch reorganisiert werden
    rehash(ht);
  }
  {
    var uint32 code = # Hashcode des Cons-Baumes berechnen
      hashcode_tuple(argcount,rest_args_pointer,0);
    var uintL hashindex;
    divu_3232_3232(code,posfixnum_to_L(TheHashtable(ht)->ht_size),_EMA_,hashindex = );
    var object* Nptr = # Pointer auf den aktuellen Eintrag
      &TheSvector(TheHashtable(ht)->ht_itable)->data[hashindex];
    loop {
      # "Liste" weiterverfolgen:
      if (eq(*Nptr,nix)) # "Liste" zu Ende -> nicht gefunden
        break;
      var uintL index = posfixnum_to_L(*Nptr); # nächster Index
      Nptr = # Pointer auf Eintrag im Next-Vektor
        &TheSvector(TheHashtable(ht)->ht_ntable)->data[index];
      var object* KVptr = # Pointer auf Einträge im Key-Value-Vektor
        &TheSvector(TheHashtable(ht)->ht_kvtable)->data[2*index];
      if (equal_tuple(KVptr[0],argcount,rest_args_pointer)) { # Key vergleichen
        # gefunden
        value1 = KVptr[1]; goto fertig; # Value als Wert
      }
    }
  }
  # nicht gefunden
  value1 = NIL;
  fertig:
  mv_count=1;
  set_args_end_pointer(rest_args_pointer STACKop 1); # STACK aufräumen
}

# UP: Berechnet einen portablen EQUAL-Hashcode eines Objekts.
# sxhash(obj)
# Er ist nur bis zur nächsten Modifizierung des Objekts gültig.
# Aus (equal X Y) folgt (= (sxhash X) (sxhash Y)).
# > obj: ein Objekt
# < ergebnis: Hashcode, eine 32-Bit-Zahl
  local uint32 sxhash (object obj);
# Hilfsfunktionen bei bekanntem Typ:
  # Atom -> Fallunterscheidung nach Typ
  local uint32 sxhash_atom (object obj);
  local uint32 sxhash_atom(obj)
    var object obj;
    {
      #ifdef TYPECODES
      switch (typecode(obj)) # je nach Typ
      #else
      if (orecordp(obj))
        goto case_orecord;
      elif (consp(obj))
        goto case_cons;
      elif (charp(obj))
        goto case_char;
      elif (fixnump(obj))
        goto case_fixnum;
      elif (short_float_p(obj))
        goto case_sfloat;
      elif (subrp(obj))
        goto case_subr;
      elif (machinep(obj))
        goto case_machine;
      elif (read_label_p(obj) || systemp(obj))
        goto case_system;
      else switch (0)
      #endif
      {
        case_symbol: # Symbol
          # Printname verwerten
          # (nicht auch die Home-Package, da sie sich bei UNINTERN verändert)
          return hashcode_string(Symbol_name(obj))+0x339B0E4CUL;
        case_cons:
        default:
          # Adresse darf nicht verwendet werden, nur den Typ verwerten
          #ifdef TYPECODES
          return highlow32(typecode(obj),0xDABE); # Typinfo*2^16+Kennung
          #else
          return highlow32((as_oint(obj)>>oint_type_shift)&(bitm(oint_type_len)-1),0xDABE); # Typinfo*2^16+Kennung
          #endif
        case_bvector: # bit-vector
        case_b2vector: # 2bit-vector
        case_b4vector: # 4bit-vector
        case_b8vector: # 8bit-vector
        case_b16vector: # 16bit-vector
        case_b32vector: # 32bit-vector
          # Bit-Vektor-Inhalt
          return hashcode_bvector(obj);
        case_string: # String
          # String-Inhalt
          return hashcode_string(obj);
        case_svector: # Simple-Vector
          # nur die Länge verwerten
          return Svector_length(obj) + 0x4ECD0A9FUL;
        case_ovector: # (vector t)
        case_mdarray: # allgemeiner Array
          # mehrdimensionaler Array -> nur Rang verwerten
          return Iarray_rank(obj) + 0xAAFAFAAEUL;
        case_structure: # Structure
          # nur Structure-Typ (Liste (name_1 name_2 ... name_n)) verwerten
          check_SP();
          return sxhash(TheStructure(obj)->structure_types) + 0xAD2CD2AEUL;
        case_stream: # Stream
          # nur Streamtyp verwerten
          return TheStream(obj)->strmtype + 0x3DAEAE55UL;
       {var uint32 bish_code;
        case_closure: # Closure
          # alle Elemente verwerten ??
          bish_code = 0xB0DD939EUL; goto record_all;
        case_orecord: # OtherRecord
          # Record-Typ verwerten, außerdem:
          # Package: Package-Name verwerten (nicht ganz OK, da eine
          #          Package mit RENAME-PACKAGE umbenannt werden kann!)
          # Pathname, Byte, LoadTimeEval: alle Komponenten verwerten
          # Hash-Table, Readtable, Random-State, Symbol-Macro: nichts weiter
          {
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
              case Rectype_Package: # Package
                # Package-Name verwerten
                {
                  var uint32 next_code = hashcode_string(ThePackage(obj)->pack_name);
                  return rotate_left(1,next_code) + bish_code;
                }
              case Rectype_Fsubr: # Fsubr
                # Namen verwerten
                check_SP(); return sxhash(TheFsubr(obj)->name) + 0xFF3319BAUL;
              case Rectype_Pathname: # Pathname
              #ifdef LOGICAL_PATHNAMES
              case Rectype_Logpathname: # Pathname
              #endif
              case Rectype_Byte: # Byte
              case Rectype_Loadtimeeval: # LoadTimeEval
                goto record_all;
              default:
                return bish_code;
            }
          }
        record_all:
          #  Record, in dem man alle Elemente verwerten kann
          check_SP();
          {
            var object* ptr = &TheRecord(obj)->recdata[0];
            var uintC count = Record_length(obj);
            dotimespC(count,count, {
              # Hashcode der nächsten Komponente dazunehmen:
              var uint32 next_code = sxhash(*ptr++);
              bish_code = misch(bish_code,next_code);
            });
            return bish_code;
          }
       }
        case_instance: # Instanz
          # nur Klasse verwerten
          return sxhash(TheInstance(obj)->inst_class) + 0x61EFA249;
        case_char: # Character
          # EQ-Hashcode nehmen (bei Characters ist ja EQUAL == EQL == EQ)
          return hashcode1(obj);
        case_subr: # SUBR
          # Namen verwerten
          check_SP(); return sxhash(TheSubr(obj)->name) + 0xFF3319BAUL;
        case_machine: # Maschinenpointer
        case_system: # Frame-Pointer, Read-Label, System
          # Adresse verwenden
          return hashcode1(obj);
        # Zahlen: nach Inhalt, wie bei EQL
        case_fixnum: # Fixnum
          return hashcode_fixnum(obj);
        case_bignum: # Bignum
          return hashcode_bignum(obj);
        case_sfloat: # Short-Float
          return hashcode_sfloat(obj);
        case_ffloat: # Single-Float
          return hashcode_ffloat(obj);
        case_dfloat: # Double-Float
          return hashcode_dfloat(obj);
        case_lfloat: # Long-Float
          return hashcode_lfloat(obj);
        case_ratio: # Ratio
          # beide Komponenten hashen, mischen
          {
            var uint32 code1 = sxhash(TheRatio(obj)->rt_num);
            var uint32 code2 = sxhash(TheRatio(obj)->rt_den);
            return misch(code1,code2);
          }
        case_complex: # Complex
          # beide Komponenten hashen, mischen
          {
            var uint32 code1 = sxhash(TheComplex(obj)->c_real);
            var uint32 code2 = sxhash(TheComplex(obj)->c_imag);
            return misch(code1,code2);
          }
      }
    }
# Cons -> Inhalt bis zur Tiefe 4 ansehen:
# Jeweils Hashcode des CAR und Hashcode des CDR bestimmen
# und geshiftet kombinieren. Als Shifts passen z.B. 16,7,5,3,
# da {0,16} + {0,7} + {0,5} + {0,3} = {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
# aus 16 verschiedenen Elementen von {0,...,31} besteht.
  # Objekt, bei Cons nur bis Tiefe 0
  local uint32 sxhash_cons0 (object obj);
  local uint32 sxhash_cons0(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return sxhash_atom(obj);
      } else {
        # Cons -> Hashcode := 1
        return 1;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 1
  local uint32 sxhash_cons1 (object obj);
  local uint32 sxhash_cons1(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return sxhash_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = sxhash_cons0(Car(obj));
        var uint32 code2 = sxhash_cons0(Cdr(obj));
        return rotate_left(3,code1) ^ code2;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 2
  local uint32 sxhash_cons2 (object obj);
  local uint32 sxhash_cons2(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return sxhash_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = sxhash_cons1(Car(obj));
        var uint32 code2 = sxhash_cons1(Cdr(obj));
        return rotate_left(5,code1) ^ code2;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 3
  local uint32 sxhash_cons3 (object obj);
  local uint32 sxhash_cons3(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return sxhash_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = sxhash_cons2(Car(obj));
        var uint32 code2 = sxhash_cons2(Cdr(obj));
        return rotate_left(7,code1) ^ code2;
      }
    }
  # Objekt, bei Cons nur bis Tiefe 4
  local uint32 sxhash(obj)
    var object obj;
    {
      if (atomp(obj)) {
        return sxhash_atom(obj);
      } else {
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        var uint32 code1 = sxhash_cons3(Car(obj));
        var uint32 code2 = sxhash_cons3(Cdr(obj));
        return rotate_left(16,code1) ^ code2;
      }
    }

# (SXHASH object), CLTL S. 285
LISPFUNN(sxhash,1)
  {
    value1 = UL_to_I(sxhash(popSTACK())); mv_count=1; # Hashcode als Integer
  }

