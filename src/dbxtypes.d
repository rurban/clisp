# Typen-Definitionen, speziell zum Debuggen von CLISP:
# Bruno Haible 27.3.1995

#include "lispbibl.c"


# Asciz
typedef sintB asciz[30];

# Bignums
typedef struct { VAROBJECT_HEADER  # Selbstpointer für GC
                 uintC length;     # Länge in Digits
                 uintD data[10]; # Zahl in Zweierkomplementdarstellung
               }
        *  dbxBignum;

# Long-Floats
typedef struct { VAROBJECT_HEADER   # Selbstpointer für GC
                 uintC  len;        # Länge der Mantisse in Digits
                 uint32 expo;       # Exponent
                 uintD  data[10]; # Mantisse
               }
        *  dbxLfloat;

# Simple-Bit-Vektor
typedef struct { VAROBJECT_HEADER # Selbstpointer für GC
                 uintL  length;   # Länge in Bits
                 uint8  data[20]; # Bits, in Bytes unterteilt
               }
        *  dbxSbvector;

# Simple-String
typedef struct { VAROBJECT_HEADER # Selbstpointer für GC
                 uintL  length;   # Länge in Bytes
                 sintB  data[50]; # Characters
               }
        *  dbxSstring;

# Simple-Vector
typedef struct { VAROBJECT_HEADER # Selbstpointer für GC
                 uintL  length;   # Länge in Objekten
                 object data[10]; # Elemente
               }
        *  dbxSvector;

# nicht-simpler Array
typedef struct { VAROBJECT_HEADER  # Selbstpointer für GC
                 uintB flags;      # Flags
                                   # dann ein Byte unbenutzt
                 uintC rank;       # Rang n
                 object data;      # Datenvektor
                 uintL totalsize;  # Totalsize = Produkt der n Dimensionen
                 uintL dims[5]; # evtl. displaced-offset,
                                   # n Dimensionen,
                                   # evtl. Fill-Pointer
               }
        *  dbxArray;

# Records
typedef struct { SRECORD_HEADER
                 object recdata[10]; # Elemente
               }
        *  dbxSrecord;
typedef struct { XRECORD_HEADER
                 object recdata[10]; # Elemente
               }
        *  dbxXrecord;

# Streams
typedef struct { VAROBJECT_HEADER # Selbstpointer für GC
                 uintB strmflags; # Flags
                 uintB strmtype;  # Untertyp
                 uintB reclength; # Länge in Objekten
                 uintB recxlength; # Länge der Extra-Elemente
                 object strm_rd_by;
                 object strm_wr_by;
                 object strm_rd_ch;
                 object strm_rd_ch_last;
                 object strm_wr_ch;
                 object strm_wr_ch_lpos;
                 object strmother[5]; # Elemente
               }
        *  dbxStream;

# Structures
#undef structure_types
typedef struct { SRECORD_HEADER
                 object structure_types;
                 object structure_components[5];
               }
        *  dbxStructure;

# Closures
typedef struct { SRECORD_HEADER
                 object clos_name;
                 object clos_codevec;
                 object other[2];
               }
        *  dbxClosure;
typedef struct { SRECORD_HEADER
                 object clos_name;
                 object clos_codevec;
                 object clos_consts[10]; # Closure-Konstanten
               }
        *  dbxCclosure;

