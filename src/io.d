# Ein-/Ausgabe für CLISP
# Bruno Haible 1990-1999
# Marcus Daniels 11.3.1997

#include "lispbibl.c"
#include "arilev0.c" # für Division in pr_uint


# =============================================================================
# Readtable-Funktionen
# =============================================================================

# Tables indexed by characters.
# allocate_perchar_table()
# perchar_table_get(table,c)
# perchar_table_put(table,c,value)
# copy_perchar_table(table)
  #if (small_char_code_limit < char_code_limit)
    # A simple-vector of small_char_code_limit+1 elements, the last entry being
    # a hash table for the non-base characters.
    local object allocate_perchar_table (void);
    local object allocate_perchar_table()
      { # Allocate the hash table.
        pushSTACK(S(Ktest)); pushSTACK(S(eq)); funcall(L(make_hash_table),2);
        pushSTACK(value1);
        # Allocate the simple-vector.
       {var object table = allocate_vector(small_char_code_limit+1);
        TheSvector(table)->data[small_char_code_limit] = popSTACK();
        return table;
      }}
    local object perchar_table_get (object table, chart c);
    local object perchar_table_get(table,c)
      var object table;
      var chart c;
      { if (as_cint(c) < small_char_code_limit)
          { return TheSvector(table)->data[as_cint(c)]; }
        else
          { var object value = gethash(code_char(c),TheSvector(table)->data[small_char_code_limit]);
            return (eq(value,nullobj) ? NIL : value);
      }   }
    local void perchar_table_put (object table, chart c, object value);
    local void perchar_table_put(table,c,value)
      var object table;
      var chart c;
      var object value;
      { if (as_cint(c) < small_char_code_limit)
          { TheSvector(table)->data[as_cint(c)] = value; }
        else
          { shifthash(TheSvector(table)->data[small_char_code_limit],code_char(c),value); }
      }
    local object copy_perchar_table (object table);
    local object copy_perchar_table(table)
      var object table;
      { pushSTACK(copy_svector(table));
        # Allocate a new hash table.
        pushSTACK(S(Ktest)); pushSTACK(S(eq)); funcall(L(make_hash_table),2);
        pushSTACK(value1);
        # Stackaufbau: table, newht.
        map_hashtable(TheSvector(STACK_1)->data[small_char_code_limit],key,value,
                      { shifthash(STACK_(0+1),key,value); }
                     );
       {var object newht = popSTACK();
        var object table = popSTACK();
        TheSvector(table)->data[small_char_code_limit] = newht;
        return table;
      }}
  #else
    # A simple-vector of char_code_limit elements.
    #define allocate_perchar_table()  allocate_vector(char_code_limit)
    #define perchar_table_get(table,c)  TheSvector(table)->data[(uintP)as_cint(c)]
    #define perchar_table_put(table,c,value)  (TheSvector(table)->data[(uintP)as_cint(c)] = (value))
    #define copy_perchar_table(table)  copy_svector(table)
  #endif

# Aufbau von Readtables (siehe LISPBIBL.D):
  # readtable_syntax_table
  #    ein Bitvektor mit char_code_limit Bytes: Zu jedem Character der Syntaxcode
  # readtable_macro_table
  #    ein Vektor mit char_code_limit Elementen: Zu jedem Character
  #    entweder  (wenn das Character keinen Read-Macro darstellt)
  #              NIL
  #    oder      (wenn das Character einen Dispatch-Macro darstellt)
  #              ein Vektor mit char_code_limit Funktionen/NILs,
  #    oder      (wenn das Character einen sonstigen Read-Macro darstellt)
  #              die Funktion, die aufgerufen wird, wenn das Character vorkommt.
  # readtable_case
  #    ein Fixnum in {0,1,2,3}

# Bedeutung von case (mit CONSTOBJ.D abgestimmt!):
  #define case_upcase    0
  #define case_downcase  1
  #define case_preserve  2
  #define case_invert    3

# Bedeutung der Einträge in der syntax_table:
  #define syntax_illegal      0  # nichtdruckende, soweit nicht whitespace
  #define syntax_single_esc   1  # '\' (Single Escape)
  #define syntax_multi_esc    2  # '|' (Multiple Escape)
  #define syntax_constituent  3  # alles übrige (Constituent)
  #define syntax_whitespace   4  # TAB,LF,FF,CR,' ' (Whitespace)
  #define syntax_eof          5  # EOF
  #define syntax_t_macro      6  # '()'"' (Terminating Macro)
  #define syntax_nt_macro     7  # '#' (Non-Terminating Macro)
# <= syntax_constituent : Wenn ein Objekt damit anfängt, ist es ein Token.
#                         (ILL liefert dann einen einen Error.)
# >= syntax_t_macro : Macro-Zeichen.
#                     Wenn ein Objekt damit anfängt: Macro-Funktion aufrufen.

# Syntax tables, indexed by characters.
# allocate_syntax_table()
# syntax_table_get(table,c)
# syntax_table_put(table,c,value) [kann GC auslösen]
  #if (small_char_code_limit < char_code_limit)
    # A cons, consisting of a simple-bit-vector with small_char_code_limit
    # bytes, and a hash table mapping characters to fixnums. Characters not
    # found in the hash table are assumed to have the syntax code
    # (graphic_char_p(ch) ? syntax_constituent : syntax_illegal).
    local object allocate_syntax_table (void);
    local object allocate_syntax_table()
      { # Allocate the hash table.
        pushSTACK(S(Ktest)); pushSTACK(S(eq)); funcall(L(make_hash_table),2);
        pushSTACK(value1);
        # Allocate the simple-bit-vector.
        pushSTACK(allocate_bit_vector(small_char_code_limit*8));
       {var object new_cons = allocate_cons();
        Car(new_cons) = popSTACK();
        Cdr(new_cons) = popSTACK();
        return new_cons;
      }}
    #define syntax_table_get(table,c)  \
      (as_cint(c) < small_char_code_limit           \
       ? TheSbvector(Car(table))->data[as_cint(c)] \
       : syntax_table_get_notinline(table,c)       \
      )
    local uintB syntax_table_get_notinline (object table, chart c);
    local uintB syntax_table_get_notinline(table,c)
      var object table;
      var chart c;
      { var object val = gethash(code_char(c),Cdr(table));
        if (!eq(val,nullobj))
          return posfixnum_to_L(val);
        else
          return (graphic_char_p(c) ? syntax_constituent : syntax_illegal);
      }
    #define syntax_table_put(table,c,value)  \
      (as_cint(c) < small_char_code_limit                            \
       ? (void)(TheSbvector(Car(table))->data[as_cint(c)] = (value)) \
       : syntax_table_put_notinline(table,c,value)                   \
      )
    local void syntax_table_put_notinline (object table, chart c, uintB value);
    local void syntax_table_put_notinline(table,c,value)
      var object table;
      var chart c;
      var uintB value;
      { shifthash(Cdr(table),code_char(c),fixnum(value)); }
  #else
    # A simple-bit-vector with char_code_limit bytes.
    #define allocate_syntax_table()  allocate_bit_vector(char_code_limit*8)
    #define syntax_table_get(table,c)  TheSbvector(table)->data[as_cint(c)]
    #define syntax_table_put(table,c,value)  (TheSbvector(table)->data[as_cint(c)] = (value))
  #endif

# originale Syntaxtabelle für eingelesene Zeichen:
  local const uintB orig_syntax_table [small_char_code_limit] = {
    #define illg  syntax_illegal
    #define sesc  syntax_single_esc
    #define mesc  syntax_multi_esc
    #define cnst  syntax_constituent
    #define whsp  syntax_whitespace
    #define tmac  syntax_t_macro
    #define nmac  syntax_nt_macro
      illg,illg,illg,illg,illg,illg,illg,illg,   # chr(0) bis chr(7)
      cnst,whsp,whsp,illg,whsp,whsp,illg,illg,   # chr(8) bis chr(15)
      illg,illg,illg,illg,illg,illg,illg,illg,   # chr(16) bis chr(23)
      illg,illg,illg,illg,illg,illg,illg,illg,   # chr(24) bis chr(31)
      whsp,cnst,tmac,nmac,cnst,cnst,cnst,tmac,   # ' !"#$%&''
      tmac,tmac,cnst,cnst,tmac,cnst,cnst,cnst,   # '()*+,-./'
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,   # '01234567'
      cnst,cnst,cnst,tmac,cnst,cnst,cnst,cnst,   # '89:;<=>?'
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,   # '@ABCDEFG'
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,   # 'HIJKLMNO'
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,   # 'PQRSTUVW'
      cnst,cnst,cnst,cnst,sesc,cnst,cnst,cnst,   # 'XYZ[\]^_'
      tmac,cnst,cnst,cnst,cnst,cnst,cnst,cnst,   # '`abcdefg'
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,   # 'hijklmno'
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,   # 'pqrstuvw'
      cnst,cnst,cnst,cnst,mesc,cnst,cnst,cnst,   # 'xyz{|}~',chr(127)
    #if defined(UNICODE) || defined(ISOLATIN_CHS) || defined(HPROMAN8_CHS)
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      whsp,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
    #elif defined(IBMPC_CHS)
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
    #elif defined(NEXTSTEP_CHS)
      whsp,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
      cnst,cnst,cnst,cnst,cnst,cnst,cnst,cnst,
    #else # defined(ASCII_CHS) && !defined(UNICODE)
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
      illg,illg,illg,illg,illg,illg,illg,illg,
    #endif
    #undef illg
    #undef sesc
    #undef mesc
    #undef cnst
    #undef whsp
    #undef tmac
    #undef nmac
    };
  #if (small_char_code_limit < char_code_limit)
    #define orig_syntax_table_get(c)  \
      (as_cint(c) < small_char_code_limit                          \
       ? orig_syntax_table[as_cint(c)]                             \
       : (graphic_char_p(c) ? syntax_constituent : syntax_illegal) \
      )
  #else
    #define orig_syntax_table_get(c)  orig_syntax_table[as_cint(c)]
  #endif

# UP: Liefert die originale Readtable.
# orig_readtable()
# < ergebnis: Originale Readtable
# can trigger GC
  local object orig_readtable (void);
  local object orig_readtable()
    { # Syntax-Tabelle initialisieren:
      { var object s_table = allocate_syntax_table(); # neuer Bitvektor
        pushSTACK(s_table); # retten
        # und mit dem Original füllen:
        #if (small_char_code_limit < char_code_limit)
        s_table = Car(s_table);
        #endif
        { var const uintB * ptr1 = &orig_syntax_table[0];
          var uintB* ptr2 = &TheSbvector(s_table)->data[0];
          var uintC count;
          dotimesC(count,small_char_code_limit, { *ptr2++ = *ptr1++; } );
      } }
      # Dispatch-Macro '#' initialisieren:
      { var object d_table = allocate_perchar_table(); # neuer Vektor
        pushSTACK(d_table); # retten
        # und die Sub-Character-Funktionen zu '#' eintragen:
        { var object* table = &TheSvector(d_table)->data[0];
          table['\''] = L(function_reader);
          table['|'] = L(comment_reader);
          table['\\'] = L(char_reader);
          table['B'] = L(binary_reader);
          table['O'] = L(octal_reader);
          table['X'] = L(hexadecimal_reader);
          table['R'] = L(radix_reader);
          table['C'] = L(complex_reader);
          table[':'] = L(uninterned_reader);
          table['*'] = L(bit_vector_reader);
          table['('] = L(vector_reader);
          table['A'] = L(array_reader);
          table['.'] = L(read_eval_reader);
          table[','] = L(load_eval_reader);
          table['='] = L(label_definition_reader);
          table['#'] = L(label_reference_reader);
          table['<'] = L(not_readable_reader);
          table[')'] = L(syntax_error_reader);
          table[' '] = L(syntax_error_reader); # #\Space
          table[NL] = L(syntax_error_reader); # #\Newline = 10 = #\Linefeed
          table[BS] = L(syntax_error_reader); # #\Backspace
          table[TAB] = L(syntax_error_reader); # #\Tab
          table[CR] = L(syntax_error_reader); # #\Return
          table[PG] = L(syntax_error_reader); # #\Page
          table[RUBOUT] = L(syntax_error_reader); # #\Rubout
          table['+'] = L(feature_reader);
          table['-'] = L(not_feature_reader);
          table['S'] = L(structure_reader);
          table['Y'] = L(closure_reader);
          table['"'] = L(clisp_pathname_reader);
          table['P'] = L(ansi_pathname_reader);
      } }
      # READ-Macros initialisieren:
      { var object m_table = allocate_perchar_table(); # neuer Vektor, mit NIL gefüllt
        # und die Macro-Characters eintragen:
        { var object* table = &TheSvector(m_table)->data[0];
          table['('] = L(lpar_reader);
          table[')'] = L(rpar_reader);
          table['"'] = L(string_reader);
          table['\''] = L(quote_reader);
          table['#'] = popSTACK(); # Dispatch-Vektor für '#'
          table[';'] = L(line_comment_reader);
          table['`'] = S(backquote_reader); # siehe BACKQUOT.LSP
          table[','] = S(comma_reader); # siehe BACKQUOT.LSP
        }
        pushSTACK(m_table); # retten
      }
      # Readtable bauen:
      { var object readtable = allocate_readtable(); # neue Readtable
        TheReadtable(readtable)->readtable_macro_table = popSTACK(); # m_table
        TheReadtable(readtable)->readtable_syntax_table = popSTACK(); # s_table
        TheReadtable(readtable)->readtable_case = fixnum(case_upcase); # :UPCASE
        return readtable;
    } }

# UP: Kopiert eine Readtable
# copy_readtable_contents(from_readtable,to_readtable)
# > from-readtable
# > to-readtable
# < ergebnis : to-Readtable desselben Inhalts
# can trigger GC
  local object copy_readtable_contents (object from_readtable, object to_readtable);
  local object copy_readtable_contents(from_readtable,to_readtable)
    var object from_readtable;
    var object to_readtable;
    { # den Case-Slot kopieren:
      TheReadtable(to_readtable)->readtable_case = TheReadtable(from_readtable)->readtable_case;
      # die Syntaxtabelle kopieren:
      { var object stable1;
        var object stable2;
        #if (small_char_code_limit < char_code_limit)
          pushSTACK(to_readtable);
          pushSTACK(from_readtable);
          # Allocate a new hash table.
          pushSTACK(S(Ktest)); pushSTACK(S(eq)); funcall(L(make_hash_table),2);
          pushSTACK(value1);
          # Stackaufbau: to-readtable, from-readtable, newht.
          map_hashtable(Cdr(TheReadtable(STACK_1)->readtable_syntax_table),ch,entry,
                        { shifthash(STACK_(0+1),ch,entry); }
                       );
         {var object newht = popSTACK();
          from_readtable = popSTACK();
          to_readtable = popSTACK();
          stable1 = Car(TheReadtable(from_readtable)->readtable_syntax_table);
          stable2 = TheReadtable(to_readtable)->readtable_syntax_table;
          Cdr(stable2) = newht;
          stable2 = Car(stable2);
         }
        #else
          stable1 = TheReadtable(from_readtable)->readtable_syntax_table;
          stable2 = TheReadtable(to_readtable)->readtable_syntax_table;
        #endif
       {var const uintB* ptr1 = &TheSbvector(stable1)->data[0];
        var uintB* ptr2 = &TheSbvector(stable2)->data[0];
        var uintC count;
        dotimesC(count,small_char_code_limit, { *ptr2++ = *ptr1++; } );
      }}
      # die Macro-Tabelle kopieren:
      pushSTACK(to_readtable); # to-readtable retten
      { var object mtable1 = TheReadtable(from_readtable)->readtable_macro_table;
        var object mtable2 = TheReadtable(to_readtable)->readtable_macro_table;
        var uintL i;
        for (i = 0; i < small_char_code_limit; i++)
          { # Eintrag Nummer i kopieren:
            var object entry = TheSvector(mtable1)->data[i];
            if (simple_vector_p(entry))
              # Simple-Vector wird elementweise kopiert:
              { pushSTACK(mtable1); pushSTACK(mtable2);
                entry = copy_perchar_table(entry);
                mtable2 = popSTACK(); mtable1 = popSTACK();
              }
            TheSvector(mtable2)->data[i] = entry;
          }
        #if (small_char_code_limit < char_code_limit)
          pushSTACK(mtable2);
          pushSTACK(mtable1);
          # Allocate a new hash table.
          pushSTACK(S(Ktest)); pushSTACK(S(eq)); funcall(L(make_hash_table),2);
          mtable1 = STACK_0;
          STACK_0 = value1;
          # Stackaufbau: mtable2, newht.
          map_hashtable(TheSvector(mtable1)->data[small_char_code_limit],ch,entry,
                        { if (simple_vector_p(entry))
                            { entry = copy_perchar_table(entry); }
                          shifthash(STACK_(0+1),ch,entry);
                        });
          TheSvector(STACK_1)->data[small_char_code_limit] = STACK_0;
          skipSTACK(2);
        #endif
      }
      return popSTACK(); # to-readtable als Ergebnis
    }

# UP: Kopiert eine Readtable
# copy_readtable(readtable)
# > readtable: Readtable
# < ergebnis: Kopie der Readtable, semantisch gleich
# can trigger GC
  local object copy_readtable (object from_readtable);
  local object copy_readtable(from_readtable)
    var object from_readtable;
    { pushSTACK(from_readtable); # retten
      pushSTACK(allocate_syntax_table()); # neue leere Syntaxtabelle
      pushSTACK(allocate_perchar_table()); # neue leere Macro-Tabelle
     {var object to_readtable = allocate_readtable(); # neue Readtable
      # füllen:
      TheReadtable(to_readtable)->readtable_macro_table = popSTACK();
      TheReadtable(to_readtable)->readtable_syntax_table = popSTACK();
      # und Inhalt kopieren:
      return copy_readtable_contents(popSTACK(),to_readtable);
    }}

# Fehler bei falschem Wert von *READTABLE*
# fehler_bad_readtable();
  nonreturning_function(local, fehler_bad_readtable, (void));
  local void fehler_bad_readtable()
    { # *READTABLE* korrigieren:
      var object sym = S(readtablestern); # Symbol *READTABLE*
      var object oldvalue = Symbol_value(sym);
      Symbol_value(sym) = O(standard_readtable); # := Standard-Readtable von Common Lisp
      # und Fehler melden:
      pushSTACK(oldvalue); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(readtable)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(sym);
      fehler(type_error,
             GETTEXT("The value of ~ was not a readtable. It has been reset.")
            );
    }

# Macro: Holt die aktuelle Readtable.
# get_readtable(readtable =);
# < readtable : die aktuelle Readtable
  #if 0
    #define get_readtable(zuweisung)  \
      { if (!readtablep(Symbol_value(S(readtablestern)))) { fehler_bad_readtable(); }  \
        zuweisung Symbol_value(S(readtablestern));                                     \
      }
  #else # oder (optimierter):
    #define get_readtable(zuweisung)  \
      { if (!(orecordp(Symbol_value(S(readtablestern)))                                          \
              && (Record_type( zuweisung Symbol_value(S(readtablestern)) ) == Rectype_Readtable) \
           ) )                                                                                   \
          { fehler_bad_readtable(); }                                                            \
      }
  #endif


# =============================================================================
# Initialisierung
# =============================================================================

# UP: Initialisiert den Reader.
# init_reader();
# can trigger GC
  global void init_reader (void);
  global void init_reader()
    { # *READ-BASE* initialisieren:
        define_variable(S(read_base),fixnum(10)); # *READ-BASE* := 10
      # *READ-SUPPRESS* initialisieren:
        define_variable(S(read_suppress),NIL);    # *READ-SUPPRESS* := NIL
      # *READ-EVAL* initialisieren:
        define_variable(S(read_eval),T);          # *READ-EVAL* := T
      # *READTABLE* initialisieren:
      { var object readtable = orig_readtable();
        O(standard_readtable) = readtable; # Das ist die Standard-Readtable,
        readtable = copy_readtable(readtable); # eine Kopie von ihr
        define_variable(S(readtablestern),readtable);   # =: *READTABLE*
      }
      # token_buff_1 und token_buff_2 initialisieren:
        O(token_buff_1) = NIL;
        # token_buff_1 und token_buff_2 werden beim ersten Aufruf von
        # get_buffers (s.u.) mit einem Semi-Simple-String und einem
        # Semi-Simple-Byte-Vektor initialisiert.
      # Displaced-String initialisieren:
        # neuer Array (mit Datenvektor NIL), Displaced, Rang=1
        O(displaced_string) =
          allocate_iarray(bit(arrayflags_displaced_bit)|bit(arrayflags_dispoffset_bit)|
                          bit(arrayflags_notbytep_bit)|Atype_Char,
                          1,
                          Array_type_string
                         );
    }

LISPFUNN(defio,2)
# (SYS::%DEFIO dispatch-reader vector-index) post-initialises the I/O.
  { O(dispatch_reader) = STACK_1;
    O(dispatch_reader_index) = STACK_0;
    value1 = NIL; mv_count=0; skipSTACK(2);
  }


# =============================================================================
# LISP - Funktionen für Readtables
# =============================================================================

# Fehler, wenn Argument keine Readtable ist.
# fehler_readtable(obj);
# > obj: fehlerhaftes Argument
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_readtable, (object obj));
  local void fehler_readtable(obj)
    var object obj;
    { pushSTACK(obj); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(S(readtable)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ is not a readtable")
            );
    }

LISPFUN(copy_readtable,0,2,norest,nokey,0,NIL)
# (COPY-READTABLE [from-readtable [to-readtable]]), CLTL S. 361
  { var object from_readtable = STACK_1;
    if (eq(from_readtable,unbound))
      # gar keine Argumente angegeben
      { get_readtable(from_readtable=); # aktuelle Readtable
        value1 = copy_readtable(from_readtable); # kopieren
      }
      else
      { if (nullp(from_readtable))
          # statt NIL nimm die Standard-Readtable
          { from_readtable = O(standard_readtable); }
          else
          # from-readtable überprüfen:
          { if (!readtablep(from_readtable)) { fehler_readtable(from_readtable); } }
        # from-readtable ist OK
       {var object to_readtable = STACK_0;
        if (eq(to_readtable,unbound) || nullp(to_readtable))
          # kopiere from-readtable, ohne to-readtable
          { value1 = copy_readtable(from_readtable); }
          else
          # to-readtable überprüfen und umkopieren:
          { if (!readtablep(to_readtable)) { fehler_readtable(to_readtable); }
            value1 = copy_readtable_contents(from_readtable,to_readtable);
          }
      }}
    mv_count=1; skipSTACK(2);
  }

LISPFUN(set_syntax_from_char,2,2,norest,nokey,0,NIL)
# (SET-SYNTAX-FROM-CHAR to-char from-char [to-readtable [from-readtable]]),
# CLTL S. 361
  { var object to_char = STACK_3;
    var object from_char = STACK_2;
    var object to_readtable = STACK_1;
    var object from_readtable = STACK_0;
    # to-char überprüfen:
    if (!charp(to_char)) { fehler_char(to_char); } # muss ein Character sein
    # from-char überprüfen:
    if (!charp(from_char)) { fehler_char(from_char); } # muss ein Character sein
    # to-readtable überprüfen:
    if (eq(to_readtable,unbound))
      { get_readtable(to_readtable=); } # Default ist die aktuelle Readtable
      else
      { if (!readtablep(to_readtable)) { fehler_readtable(to_readtable); } }
    # from-readtable überprüfen:
    if (eq(from_readtable,unbound) || nullp(from_readtable))
      { from_readtable = O(standard_readtable); } # Default ist die Standard-Readtable
      else
      { if (!readtablep(from_readtable)) { fehler_readtable(from_readtable); } }
    STACK_1 = to_readtable;
    STACK_0 = from_readtable;
    # Nun sind to_char, from_char, to_readtable, from_readtable OK.
    { var chart to_c = char_code(to_char);
      var chart from_c = char_code(from_char);
      # Syntaxcode kopieren:
      syntax_table_put(TheReadtable(to_readtable)->readtable_syntax_table,to_c,
        syntax_table_get(TheReadtable(from_readtable)->readtable_syntax_table,from_c));
      # Macro-Funktion/Vektor kopieren:
     {var object entry = perchar_table_get(TheReadtable(STACK_0)->readtable_macro_table,from_c);
      if (simple_vector_p(entry))
        # Ist entry ein Simple-Vector, so muss er kopiert werden:
        { entry = copy_perchar_table(entry); }
      perchar_table_put(TheReadtable(STACK_1)->readtable_macro_table,to_c,entry);
    }}
    value1 = T; mv_count=1; # Wert T
    skipSTACK(4);
  }

# UP: Überprüft ein optionales Readtable-Argument,
# mit Default = Current Readtable.
# > STACK_0: Argument
# > subr_self: Aufrufer (ein SUBR)
# < STACK: um 1 erhöht
# < ergebnis: readtable
  local object test_readtable_arg (void);
  local object test_readtable_arg()
    { var object readtable = popSTACK();
      if (eq(readtable,unbound))
        { get_readtable(readtable=); } # Default ist die aktuelle Readtable
        else
        { if (!readtablep(readtable)) { fehler_readtable(readtable); } } # überprüfen
      return readtable;
    }

# UP: Überprüft ein optionales Readtable-Argument,
# mit Default = Current Readtable, NIL = Standard-Readtable.
# > STACK_0: Argument
# > subr_self: Aufrufer (ein SUBR)
# < STACK: um 1 erhöht
# < ergebnis: readtable
  local object test_readtable_null_arg (void);
  local object test_readtable_null_arg()
    { var object readtable = popSTACK();
      if (eq(readtable,unbound))
        { get_readtable(readtable=); } # Default ist die aktuelle Readtable
      elif (nullp(readtable))
        { readtable = O(standard_readtable); } # bzw. die Standard-Readtable
      else
        { if (!readtablep(readtable)) { fehler_readtable(readtable); } } # überprüfen
      return readtable;
    }

# UP: Überprüft das vorletzte optionale Argument von
# SET-MACRO-CHARACTER und MAKE-DISPATCH-MACRO-CHARACTER.
# > STACK_0: non-terminating-p - Argument
# > subr_self: Aufrufer (ein SUBR)
# < STACK: um 1 erhöht
# < ergebnis: neuer Syntaxcode
  local uintB test_nontermp_arg (void);
  local uintB test_nontermp_arg()
    { var object arg = popSTACK();
      if (eq(arg,unbound) || nullp(arg))
        { return syntax_t_macro; } # Default ist terminating
        else
        { return syntax_nt_macro; } # non-terminating-p angegeben und /= NIL
    }

LISPFUN(set_macro_character,2,2,norest,nokey,0,NIL)
# (SET-MACRO-CHARACTER char function [non-terminating-p [readtable]]),
# CLTL S. 362
  { # char überprüfen:
    { var object ch = STACK_3;
      if (!charp(ch)) { fehler_char(ch); }
    }
    # function überprüfen und in ein Objekt vom Typ FUNCTION umwandeln:
    {var object function = coerce_function(STACK_2);
     if (cclosurep(function)
         && eq(TheCclosure(function)->clos_codevec,TheCclosure(O(dispatch_reader))->clos_codevec))
       { var object vector =
           ((Srecord)TheCclosure(function))->recdata[posfixnum_to_L(O(dispatch_reader_index))];
         if (simple_vector_p(vector))
           # It's a clone of #'dispatch-reader. Pull out the vector.
           { function = copy_perchar_table(vector); }
       }
     STACK_2 = function;
    }
   {var object readtable = test_readtable_arg(); # Readtable
    var uintB syntaxcode = test_nontermp_arg(); # neuer Syntaxcode
    var chart c = char_code(STACK_1);
    STACK_1 = readtable;
    # Syntaxcode setzen:
    syntax_table_put(TheReadtable(readtable)->readtable_syntax_table,c,syntaxcode);
    # Macrodefinition eintragen:
    perchar_table_put(TheReadtable(STACK_1)->readtable_macro_table,c,STACK_0);
    value1 = T; mv_count=1; # 1 Wert T
    skipSTACK(2);
  }}

LISPFUN(get_macro_character,1,1,norest,nokey,0,NIL)
# (GET-MACRO-CHARACTER char [readtable]), CLTL S. 362
  { # char überprüfen:
    { var object ch = STACK_1;
      if (!charp(ch)) { fehler_char(ch); }
    }
   {var object readtable = test_readtable_null_arg(); # Readtable
    var object ch = popSTACK();
    var chart c = char_code(ch);
    # Teste den Syntaxcode:
    var object nontermp = NIL; # non-terminating-p Flag
    switch (syntax_table_get(TheReadtable(readtable)->readtable_syntax_table,c))
      { case syntax_nt_macro: nontermp = T;
        case syntax_t_macro: # nontermp = NIL;
          # c ist ein Macro-Character.
          { var object entry = perchar_table_get(TheReadtable(readtable)->readtable_macro_table,c);
            if (simple_vector_p(entry))
              # c ist ein Dispatch-Macro-Character.
              { if (nullp(O(dispatch_reader)))
                  { # Shouldn't happen (bootstrapping problem).
                    pushSTACK(ch);
                    pushSTACK(TheSubr(subr_self)->name);
                    fehler(error,
                           GETTEXT("~: ~ is a dispatch macro character")
                          );
                  }
                # Clone #'dispatch-reader.
                pushSTACK(copy_perchar_table(entry));
                { var object newclos = allocate_cclosure_copy(O(dispatch_reader));
                  do_cclosure_copy(newclos,O(dispatch_reader));
                  ((Srecord)TheCclosure(newclos))->recdata[posfixnum_to_L(O(dispatch_reader_index))] = popSTACK();
                  value1 = newclos;
                }
                break;
              }
              else
              { value1 = entry; break; }
          }
        default: # nontermp = NIL;
          value1 = NIL; break;
      }
    value2 = nontermp; mv_count=2; # nontermp als 2. Wert
  }}

LISPFUN(make_dispatch_macro_character,1,2,norest,nokey,0,NIL)
# (MAKE-DISPATCH-MACRO-CHARACTER char [non-terminating-p [readtable]]),
# CLTL S. 363
  { var object readtable = test_readtable_arg(); # Readtable
    var uintB syntaxcode = test_nontermp_arg(); # neuer Syntaxcode
    # char überprüfen:
    var object ch = popSTACK();
    if (!charp(ch)) { fehler_char(ch); }
   {var chart c = char_code(ch);
    # neue (leere) Dispatch-Macro-Tabelle holen:
    pushSTACK(readtable);
    pushSTACK(allocate_perchar_table()); # Vektor, mit NIL gefüllt
    # alles in der Readtable ablegen:
    # Syntaxcode in die Syntax-Table:
    syntax_table_put(TheReadtable(STACK_1)->readtable_syntax_table,c,syntaxcode);
    # neue Dispatch-Macro-Tabelle in die Macrodefinitionen-Tabelle:
    perchar_table_put(TheReadtable(STACK_1)->readtable_macro_table,c,STACK_0);
    value1 = T; mv_count=1; # 1 Wert T
    skipSTACK(2);
  }}

# UP: Überprüft die Argumente disp-char und sub-char.
# > STACK: STACK_1 = disp-char, STACK_0 = sub-char
# > readtable: Readtable
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: die Dispatch-Macro-Tabelle zu disp-char,
#             nullobj falls sub-char eine Ziffer ist.
  local object test_disp_sub_char (object readtable);
  local object test_disp_sub_char(readtable)
    var object readtable;
    { var object sub_ch = STACK_0; # sub-char
      var object disp_ch = STACK_1; # disp-char
      if (!charp(disp_ch)) { fehler_char(disp_ch); } # disp-char muss ein Character sein
      if (!charp(sub_ch)) { fehler_char(sub_ch); } # sub-char muss ein Character sein
     {var chart disp_c = char_code(disp_ch);
      var object entry = perchar_table_get(TheReadtable(readtable)->readtable_macro_table,disp_c);
      if (!simple_vector_p(entry))
        { pushSTACK(disp_ch);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: ~ is not a dispatch macro character")
                );
        }
      # disp-char ist ein Dispatching-Macro-Character, entry der Vektor.
      {var cint sub_c = as_cint(up_case(char_code(sub_ch))); # sub-char in Großbuchstaben umwandeln
       if ((sub_c >= '0') && (sub_c <= '9'))
         # Ziffer
         { return nullobj; }
         else
         # gültiges sub-char
         { return entry; }
    }}}

LISPFUN(set_dispatch_macro_character,3,1,norest,nokey,0,NIL)
# (SET-DISPATCH-MACRO-CHARACTER disp-char sub-char function [readtable]),
# CLTL S. 364
  { # function überprüfen und in ein Objekt vom Typ FUNCTION umwandeln:
    STACK_1 = coerce_function(STACK_1);
    subr_self = L(set_dispatch_macro_character);
   {var object readtable = test_readtable_arg(); # Readtable
    var object function = popSTACK(); # function
    var object dm_table = test_disp_sub_char(readtable);
    if (eq(dm_table,nullobj))
      { # STACK_0 = sub-char, Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_not_digit)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(STACK_1);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: digit $ not allowed as sub-char")
              );
      }
      else
      { perchar_table_put(dm_table,char_code(STACK_0),function); # Funktion in die Dispatch-Macro-Tabelle eintragen
        value1 = T; mv_count=1; skipSTACK(2); # 1 Wert T
      }
  }}

LISPFUN(get_dispatch_macro_character,2,1,norest,nokey,0,NIL)
# (GET-DISPATCH-MACRO-CHARACTER disp-char sub-char [readtable]), CLTL S. 364
  { var object readtable = test_readtable_null_arg(); # Readtable
    var object dm_table = test_disp_sub_char(readtable);
    value1 = (eq(dm_table,nullobj) ? NIL : perchar_table_get(dm_table,char_code(STACK_0))); # NIL oder Funktion als Wert
    mv_count=1; skipSTACK(2);
  }

LISPFUNN(readtable_case,1)
# (READTABLE-CASE readtable), CLTL2 S. 549
  { var object readtable = popSTACK(); # Readtable
    if (!readtablep(readtable)) { fehler_readtable(readtable); } # überprüfen
    value1 = (&O(rtcase_0))[(uintW)posfixnum_to_L(TheReadtable(readtable)->readtable_case)];
    mv_count=1;
  }

LISPFUNN(set_readtable_case,2)
# (SYSTEM::SET-READTABLE-CASE readtable value), CLTL2 S. 549
  { var object value = popSTACK();
    var object readtable = popSTACK(); # Readtable
    if (!readtablep(readtable)) { fehler_readtable(readtable); } # überprüfen
    # Symbol value in einen Index umwandeln durch Suche in der Tabelle O(rtcase..):
   {var const object* ptr = &O(rtcase_0);
    var object rtcase = Fixnum_0;
    var uintC count;
    dotimesC(count,4,
      { if (eq(*ptr,value)) goto found;
        ptr++; rtcase = fixnum_inc(rtcase,1);
      });
    # kein gültiger Wert
    pushSTACK(value); # Wert für Slot DATUM von TYPE-ERROR
    pushSTACK(O(type_rtcase)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
    pushSTACK(O(rtcase_3)); pushSTACK(O(rtcase_2)); pushSTACK(O(rtcase_1)); pushSTACK(O(rtcase_0));
    pushSTACK(value);
    pushSTACK(S(set_readtable_case));
    fehler(type_error,
           GETTEXT("~: new value ~ should be ~, ~, ~ or ~.")
          );
    found: # in der Tabelle gefunden
    TheReadtable(readtable)->readtable_case = rtcase;
    value1 = value; mv_count=1;
  }}

# =============================================================================
# Einige Hilfsroutinen und Macros für READ und PRINT
# =============================================================================

# Testet den dynamischen Wert eines Symbols auf /=NIL
# < TRUE, wenn /= NIL
# #define test_value(sym)  (!nullp(Symbol_value(sym)))
  #define test_value(sym)  (!eq(NIL,Symbol_value(sym)))

# UP: Holt den Wert eines Symbols. Muss Fixnum >=2, <=36 sein.
# get_base(symbol)
# > symbol: Symbol
# < ergebnis: Wert des Symbols, >=2, <=36.
  local uintL get_base (object symbol);
  local uintL get_base(symbol)
    var object symbol;
    { var object value = Symbol_value(symbol);
      var uintL wert;
      if (posfixnump(value) &&
          (wert = posfixnum_to_L(value), ((wert >= 2) && (wert <= 36)))
         )
        { return wert; }
        else
        { Symbol_value(symbol) = fixnum(10); # Wert auf 10 setzen
          pushSTACK(value); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_radix)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(value);
          pushSTACK(symbol);
          fehler(type_error,
                 GETTEXT("The value of ~ should be an integer between 2 and 36, not ~." NLstring
                         "It has been reset to 10.")
                );
        }
    }

# UP: Holt den Wert von *PRINT-BASE*
# get_print_base()
# < uintL ergebnis: >=2, <=36
  #define get_print_base()  \
    (test_value(S(print_readably)) ? 10 : get_base(S(print_base)))

# UP: Holt den Wert von *READ-BASE*
# get_read_base()
# < uintL ergebnis: >=2, <=36
  #define get_read_base()  get_base(S(read_base))


# =============================================================================
#                              R E A D
# =============================================================================

# Es werden einzelne Characters gelesen.
# Mit Hilfe der Readtable werden Syntaxcodes (vgl. CLTL Table 22-1) gebildet.
# Bei Syntaxcode = constituent wird ein (Extended) Token angefangen.
# Mit Hilfe der Attributtabelle (vgl. CLTL Table 22-3) wird jedem Character
# im Token ein Attribut a_xxxx zugeordnet.
# O(token_buff_1) ist ein Semi-Simple-String, der die Characters des
# gerade eingelesenen Extended-Tokens enthält.
# O(token_buff_2) ist ein Semi-Simple-Byte-Vektor, der die Attribute des
# gerade eingelesenen Extended-Tokens enthält.
# Beide haben dieselbe Länge (in Characters bzw. Bytes).

# Spezielle Objekte, die bei READ als Ergebnis kommen können:
#   eof_value: spezielles Objekt, das EOF anzeigt
#   dot_value: Hilfswert zum Erkennen einzelner Dots

# ------------------------ READ auf Character-Ebene ---------------------------

# Fehler, wenn gelesenes Objekt kein Character ist:
# fehler_charread(ch,&stream);
  nonreturning_function(local, fehler_charread, (object ch, const object* stream_));
  local void fehler_charread(ch,stream_)
    var object ch;
    var const object* stream_;
    { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(ch); # Character
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: character read should be a character: ~")
            );
    }

# UP: Liest ein Zeichen und berechnet seinen Syntaxcode.
# read_char_syntax(ch=,scode=,&stream);
# > stream: Stream
# < stream: Stream
# < object ch: Character oder eof_value
# < uintWL scode: Syntaxcode (aus der aktuellen Readtable) bzw. syntax_eof
# can trigger GC
  #define read_char_syntax(ch_zuweisung,scode_zuweisung,stream_)  \
    { var object ch0 = read_char(stream_); # Character lesen           \
      ch_zuweisung ch0;                                                \
      if (eq(ch0,eof_value)) # EOF ?                                   \
        { scode_zuweisung syntax_eof; }                                \
        else                                                           \
        { # Sonst auf Character überprüfen:                            \
          if (!charp(ch0)) { fehler_charread(ch0,stream_); }           \
         {var object readtable;                                        \
          get_readtable(readtable = );                                 \
          scode_zuweisung # Syntaxcode aus Tabelle holen               \
            syntax_table_get(TheReadtable(readtable)->readtable_syntax_table,char_code(ch0)); \
        }}                                                             \
    }

# Fehlermeldung bei EOF außerhalb von Objekten
# fehler_eof_aussen(&stream);
# > stream: Stream
  nonreturning_function(local, fehler_eof_aussen, (const object* stream_));
  local void fehler_eof_aussen(stream_)
    var const object* stream_;
    { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(end_of_file,
             GETTEXT("~: input stream ~ has reached its end")
            );
    }

# Fehlermeldung bei EOF innerhalb von Objekten
# fehler_eof_innen(&stream);
# > stream: Stream
  nonreturning_function(local, fehler_eof_innen, (const object* stream_));
  local void fehler_eof_innen(stream_)
    var const object* stream_;
    { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
      if (posfixnump(Symbol_value(S(read_line_number)))) # SYS::*READ-LINE-NUMBER* abfragen
        { pushSTACK(Symbol_value(S(read_line_number))); # Zeilennummer
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(end_of_file,
                 GETTEXT("~: input stream ~ ends within an object. Last opening parenthesis probably in line ~.")
                );
        }
        else
        { pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(end_of_file,
                 GETTEXT("~: input stream ~ ends within an object")
                );
    }   }

# Fehlermeldung bei EOF, je nach *READ-RECURSIVE-P*
# fehler_eof(&stream);
# > stream: Stream
  nonreturning_function(local, fehler_eof, (const object* stream_));
  local void fehler_eof(stream_)
    var const object* stream_;
    { if (test_value(S(read_recursive_p))) # *READ-RECURSIVE-P* /= NIL ?
        { fehler_eof_innen(stream_); }
        else
        { fehler_eof_aussen(stream_); }
    }

# UP: Liest bis zum nächsten non-whitespace-Zeichen, ohne dieses zu
# verbrauchen. Bei EOF Error.
# wpeek_char_syntax(ch=,scode=,&stream);
# > stream: Stream
# < stream: Stream
# < object ch: nächstes Character
# < uintWL scode: sein Syntaxcode
# can trigger GC
  #define wpeek_char_syntax(ch_zuweisung,scode_zuweisung,stream_)  \
    { loop                                                                 \
        { var object ch0 = read_char(stream_); # Character lesen           \
          if (eq(ch0,eof_value)) { fehler_eof(stream_); } # EOF -> Error   \
          # Sonst auf Character überprüfen:                                \
          if (!charp(ch0)) { fehler_charread(ch0,stream_); }               \
          {var object readtable;                                           \
           get_readtable(readtable = );                                    \
           if (!((scode_zuweisung # Syntaxcode aus Tabelle holen           \
                    syntax_table_get(TheReadtable(readtable)->readtable_syntax_table,char_code(ch0)) \
                 )                                                         \
                 == syntax_whitespace                                      \
              ) )                                                          \
             # kein Whitespace -> letztes gelesenes Zeichen zurückschieben \
             { unread_char(stream_,ch0); ch_zuweisung ch0; break; }        \
        } }                                                                \
    }

# UP: Liest bis zum nächsten non-whitespace-Zeichen, ohne dieses zu
# verbrauchen.
# wpeek_char_eof(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: nächstes Character oder eof_value
# can trigger GC
  local object wpeek_char_eof (const object* stream_);
  local object wpeek_char_eof(stream_)
    var const object* stream_;
    { loop
        { var object ch = read_char(stream_); # Character lesen
          if (eq(ch,eof_value)) { return ch; } # EOF ?
          # Sonst auf Character überprüfen:
          if (!charp(ch)) { fehler_charread(ch,stream_); }
          {var object readtable;
           get_readtable(readtable = );
           if (!(( # Syntaxcode aus Tabelle holen
                  syntax_table_get(TheReadtable(readtable)->readtable_syntax_table,char_code(ch))
                 )
                 == syntax_whitespace
              ) )
             # kein Whitespace -> letztes gelesenes Zeichen zurückschieben
             { unread_char(stream_,ch); return ch; }
        } }
    }

# ------------------------ READ auf Token-Ebene -------------------------------

# Bei read_token und test_potential_number_syntax, test_number_syntax werden
# die Attribute gemäß CLTL Table 22-3 gebraucht.
# Während test_potential_number_syntax werden Attribute umgewandelt,
# a_digit teilweise in a_alpha oder a_letter oder a_expo_m.

# Bedeutung der Einträge in attribute_table:
  #define a_illg     0   # illegales Constituent
  #define a_pack_m   1   # ':' = Package-marker
  #define a_alpha    2   # Zeichen ohne besondere Eigenschaften (alphabetic)
  #define a_escaped  3   # Zeichen ohne besondere Eigenschaften, nicht case-konvertierbar
  #define a_ratio    4   # '/'
  #define a_dot      5   # '.'
  #define a_plus     6   # '+'
  #define a_minus    7   # '-'
  #define a_extens   8   # '_^' extension characters
  #define a_digit    9   # '0123456789'
  #define a_letter  10   # 'A'-'Z','a'-'z', nicht 'esfdlESFDL'
  #define a_expo_m  11   # 'esfdlESFDL'
  #    >= a_letter       #  'A'-'Z','a'-'z'
  #    >= a_digit        # '0123456789','A'-'Z','a'-'z'
  #    >= a_ratio        # woraus eine potential number bestehen muss

# Attributtabelle für Constituents, Erstinterpretation:
# Anmerkung: 0-9,A-Z,a-z werden erst als a_digit oder a_expo_m interpretiert,
# dann (falls sich kein Integer aus einem Token ergibt) wird a_digit
# oberhalb von *READ-BASE* als a_alpha (alphabetic) interpretiert.
  local const uintB attribute_table[small_char_code_limit] = {
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,   # chr(0) bis chr(7)
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,   # chr(8) bis chr(15)
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,   # chr(16) bis chr(23)
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,   # chr(24) bis chr(31)
    a_illg,  a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,  # ' !"#$%&''
    a_alpha, a_alpha, a_alpha, a_plus,  a_alpha, a_minus, a_dot,   a_ratio,  # '()*+,-./'
    a_digit, a_digit, a_digit, a_digit, a_digit, a_digit, a_digit, a_digit,  # '01234567'
    a_digit, a_digit, a_pack_m,a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,  # '89:;<=>?'
    a_alpha, a_letter,a_letter,a_letter,a_expo_m,a_expo_m,a_expo_m,a_letter, # '@ABCDEFG'
    a_letter,a_letter,a_letter,a_letter,a_expo_m,a_letter,a_letter,a_letter, # 'HIJKLMNO'
    a_letter,a_letter,a_letter,a_expo_m,a_letter,a_letter,a_letter,a_letter, # 'PQRSTUVW'
    a_letter,a_letter,a_letter,a_alpha, a_alpha, a_alpha, a_extens,a_extens, # 'XYZ[\]^_'
    a_alpha, a_letter,a_letter,a_letter,a_expo_m,a_expo_m,a_expo_m,a_letter, # '`abcdefg'
    a_letter,a_letter,a_letter,a_letter,a_expo_m,a_letter,a_letter,a_letter, # 'hijklmno'
    a_letter,a_letter,a_letter,a_expo_m,a_letter,a_letter,a_letter,a_letter, # 'pqrstuvw'
    a_letter,a_letter,a_letter,a_alpha, a_alpha, a_alpha, a_alpha,           # 'xyz{|}~'
    #if defined(UNICODE) || defined(ISOLATIN_CHS) || defined(HPROMAN8_CHS)
                                                                   a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    #elif defined(IBMPC_CHS)
                                                                   a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    #elif defined(NEXTSTEP_CHS)
                                                                   a_illg,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha, a_alpha,
    #else # defined(ASCII_CHS) && !defined(UNICODE)
                                                                   a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,  a_illg,
    #endif
    };

# Returns the attribute code for a character code.
# attribute_of(c)
# > chart c: character code
# < uintB result: attribute code
  #if (small_char_code_limit < char_code_limit) # i.e. defined(UNICODE)
    #define attribute_of(c)  \
      (as_cint(c) < small_char_code_limit       \
       ? attribute_table[as_cint(c)]            \
       : (graphic_char_p(c) ? a_alpha : a_illg) \
      )
  #else
    #define attribute_of(c)  attribute_table[as_cint(c)]
  #endif

# Flag. Zeigt an, ob im letztgelesenen Token
# ein Single-Escape- oder Multiple-Escape-Zeichen vorkam:
  local boolean token_escape_flag;

# UP: Liefert zwei Buffer.
# Falls im Reservoir O(token_buff_1), O(token_buff_2) zwei verfügbar sind,
# werden sie entnommen. Sonst werden neue alloziert.
# Werden die Buffer nicht mehr gebraucht, so können sie in
# O(token_buff_1) und O(token_buff_2) geschrieben werden.
# < STACK_1: ein Semi-Simple String mit Fill-Pointer 0
# < STACK_0: ein Semi-Simple Byte-Vektor mit Fill-Pointer 0
# < STACK: um 2 erniedrigt
# can trigger GC
  local void get_buffers (void);
  local void get_buffers()
    { # Mechanismus:
      # In O(token_buff_1) und O(token_buff_2) stehen ein Semi-Simple-String
      # und ein Semi-Simple-Byte-Vektor, die bei Bedarf entnommen (und mit
      # O(token_buff_1) := NIL als entnommen markiert) und nach Gebrauch
      # wieder hineingesetzt werden können. Reentrant!
      var object buff_1 = O(token_buff_1);
      if (!nullp(buff_1))
        # Buffer entnehmen und leeren:
        { TheIarray(buff_1)->dims[1] = 0; # Fill-Pointer:=0
          pushSTACK(buff_1); # 1. Buffer fertig
         {var object buff_2 = O(token_buff_2);
          TheIarray(buff_2)->dims[1] = 0; # Fill-Pointer:=0
          pushSTACK(buff_2); # 2. Buffer fertig
          O(token_buff_1) = NIL; # Buffer als entnommen markieren
        }}
        else
        # Buffer sind gerade entnommen und müssen neu alloziert werden:
        { pushSTACK(make_ssstring(50)); # neuer Semi-Simple-String mit Fill-Pointer=0
          pushSTACK(make_ssbvector(50)); # neuer Semi-Simple-Byte-Vektor mit Fill-Pointer=0
        }
    }

# UP: Liest ein Extended Token.
# read_token(&stream);
# > stream: Stream
# < stream: Stream
# < O(token_buff_1): gelesene Characters
# < O(token_buff_2): ihre Attributcodes
# < token_escape_flag: Escape-Zeichen-Flag
# can trigger GC
  local void read_token (const object* stream_);

# UP: Liest ein Extended Token, erstes Zeichen bereits gelesen.
# read_token_1(&stream,ch,scode);
# > stream: Stream
# > ch, scode: erstes Zeichen und sein Syntaxcode
# < stream: Stream
# < O(token_buff_1): gelesene Characters
# < O(token_buff_2): ihre Attributcodes
# < token_escape_flag: Escape-Zeichen-Flag
# can trigger GC
  local void read_token_1 (const object* stream_, object ch, uintWL scode);

  local void read_token(stream_)
    var const object* stream_;
    { # erstes Zeichen lesen:
      var object ch;
      var uintWL scode;
      read_char_syntax(ch = ,scode = ,stream_);
      # Token aufbauen:
      read_token_1(stream_,ch,scode);
    }

  local void read_token_1(stream_,ch,scode)
    var const object* stream_;
    var object ch;
    var uintWL scode;
    { # leere Token-Buffer holen, auf den STACK:
      get_buffers(); # (brauche ch nicht zu retten)
      # Bis zum Ende von read_token_1 liegen die beiden Buffer im Stack.
      # (So kann read_char rekursiv read aufrufen...)
      # Danach (während test_potential_number_syntax, test_number_syntax,
      # test_dots, read_internal bis zum Ende von read_internal) liegen
      # die Buffer in O(token_buff_1) und O(token_buff_2). Nach dem Ende von
      # read_internal ist ihr Inhalt wertlos, und sie können für weitere
      # read-Operationen benutzt werden.
     {var boolean multiple_escape_flag = FALSE;
      var boolean escape_flag = FALSE;
      goto char_read;
      loop
        { # Hier wird das Token in STACK_1 (Semi-Simple-String für Characters)
          # und STACK_0 (Semi-Simple-Byte-Vektor für Attributcodes) aufgebaut.
          # Multiple-Escape-Flag zeigt an, ob man sich zwischen |...| befindet.
          # Escape-Flag zeigt an, ob ein Escape-Character vorgekommen ist.
          read_char_syntax(ch = ,scode = ,stream_); # nächstes Zeichen lesen
          char_read:
          switch(scode)
            { case syntax_illegal:
                # illegal -> Error melden:
                pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
                pushSTACK(ch); # Zeichen
                pushSTACK(*stream_); # Stream
                pushSTACK(S(read));
                fehler(stream_error,
                       GETTEXT("~ from ~: illegal character ~")
                      );
                break;
              case syntax_single_esc:
                # Single-Escape-Zeichen ->
                # nächstes Zeichen lesen und unverändert übernehmen
                escape_flag = TRUE;
                read_char_syntax(ch = ,scode = ,stream_); # nächstes Zeichen lesen
                if (scode==syntax_eof) # EOF erreicht?
                  { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
                    pushSTACK(*stream_);
                    pushSTACK(S(read));
                    fehler(end_of_file,
                           GETTEXT("~: input stream ~ ends within a token after single escape character")
                          );
                  }
              escape:
                # nach Escape-Zeichen:
                # Zeichen unverändert ins Token übernehmen
                ssstring_push_extend(STACK_1,char_code(ch));
                ssbvector_push_extend(STACK_0,a_escaped);
                break;
              case syntax_multi_esc:
                # Multiple-Escape-Zeichen
                multiple_escape_flag = !multiple_escape_flag;
                escape_flag = TRUE;
                break;
              case syntax_constituent:
              case syntax_nt_macro:
                # normales Constituent
                if (multiple_escape_flag) # Zwischen Multiple-Escape-Zeichen?
                  goto escape; # ja -> Zeichen unverändert übernehmen
                # ins Token übernehmen (Groß-/Klein-Umwandlung kommt später):
                {var chart c = char_code(ch);
                 ssstring_push_extend(STACK_1,c);
                 ssbvector_push_extend(STACK_0,attribute_of(c));
                }
                break;
              case syntax_whitespace:
              case syntax_t_macro:
                # whitespace oder terminating macro ->
                # Token endet wohl vor diesem Character.
                if (multiple_escape_flag) # Zwischen Multiple-Escape-Zeichen?
                  goto escape; # ja -> Zeichen unverändert übernehmen
                # Token ist zu Ende.
                # Schiebe das Character auf den Stream zurück,
                # falls es kein Whitespace ist oder
                # es ein Whitespace ist und *READ-PRESERVE-WHITESPACE* /= NIL.
                if ((!(scode == syntax_whitespace))
                    || test_value(S(read_preserve_whitespace))
                   )
                  { unread_char(stream_,ch); }
                goto ende;
              case syntax_eof:
                # EOF erreicht.
                if (multiple_escape_flag) # zwischen Multiple-Escape-Zeichen?
                  { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
                    pushSTACK(*stream_);
                    pushSTACK(S(read));
                    fehler(end_of_file,
                           GETTEXT("~: input stream ~ ends within a token after multiple escape character")
                          );
                  }
                # nein -> Token normal zu Ende
                goto ende;
              default: NOTREACHED
        }   }
      ende:
      # Nun ist Token zu Ende, multiple_escape_flag = FALSE.
      token_escape_flag = escape_flag; # Escape-Flag abspeichern
      O(token_buff_2) = popSTACK(); # Attributcode-Buffer
      O(token_buff_1) = popSTACK(); # Character-Buffer
    }}

# --------------- READ zwischen Token-Ebene und Objekt-Ebene ------------------

# UP: Überprüft, ob der Token-Buffer eine potential-number enthält, und
# wandelt, als Vorbereitung auf Zahl-Lese-Routinen, Attributcodes um.
# test_potential_number_syntax(&base,&token_info);
# > O(token_buff_1): gelesene Characters
# > O(token_buff_2): ihre Attributcodes
# > base: Ziffernsystembasis (Wert von *READ-BASE* oder *PRINT-BASE*)
# < base: Ziffernsystembasis (= 10 oder altes base)
# Innerhalb von O(token_buff_2) wird umgewandelt:
#   Falls potential number:
#     >=a_letter oberhalb der Ziffernsystembasis -> a_alpha
#   Falls nicht potential number:
#     Unterscheidung zwischen [a_pack_m | a_dot | sonstiges] bleibt erhalten.
# < ergebnis: TRUE, falls potential number vorliegt
#             (und dann ist token_info mit {charptr, attrptr, len} gefüllt)
  typedef struct { chart* charptr; uintB* attrptr; uintL len; } token_info;
  local boolean test_potential_number_syntax (uintWL* base_, token_info* info);
  local boolean test_potential_number_syntax(base_,info)
    var uintWL* base_;
    var token_info* info;
    # Ein Token ist potential number, wenn (CLTL, S. 341)
    # - es ausschließlich aus Ziffern, '+','-','/','^','_','.' und
    #   Number-Markern besteht. Die Basis für die Ziffern ist dabei vom
    #   Kontext abhängig, jedoch immer 10, wenn ein Punkt '.' vorkommt.
    #   Ein Number-Marker ist ein Buchstabe, der keine Ziffer ist und
    #   nicht neben einem anderen solchen steht.
    # - es mindestens eine Ziffer enthält,
    # - es mit einer Ziffer, '+','-','.','^' oder '_' beginnt,
    # - es nicht mit '+' oder '-' endet.
    # Überprüfung:
    # 1. Suche, ob ein Punkt vorkommt. Falls ja, Basis:=10.
    # 2. Alles >=a_letter (also 'A'-'Z','a'-'z'), was einen Wert <Basis hat,
    #    wird in a_digit umgewandelt.
    # (Jetzt wird a_digit als "digit" und >=a_letter als "letter" interpretiert.)
    # 3. Test, ob nur >=a_ratio vorkommen. Nein -> kein potential number.
    # 4. Test, ob ein a_digit vorkommt. Nein -> kein potential number.
    # (Jetzt ist die Länge >0.)
    # 5. Test, ob nebeneinanderliegende >=a_letter vorkommen.
    #    Ja -> kein potential number.
    # 6. Test, ob erstes Zeichenattribut >=a_dot,<=a_digit.
    #    Nein -> kein potential number.
    # 7. Test, ob letztes Zeichenattribut =a_plus oder =a_minus.
    #    Ja -> kein potential number.
    # 8. Potential number liegt vor.
    { var chart* charptr0; # Pointer auf die Characters
      var uintB* attrptr0; # Pointer auf die Attribute
      var uintL len; # Länge des Token
      # initialisieren:
      { var object buff = O(token_buff_1); # Semi-Simple String
        len = TheIarray(buff)->dims[1]; # Länge = Fill-Pointer
        charptr0 = &TheSstring(TheIarray(buff)->data)->data[0]; # ab hier kommen die Characters
        buff = O(token_buff_2); # Semi-Simple Byte-Vektor
        attrptr0 = &TheSbvector(TheIarray(buff)->data)->data[0]; # ab hier kommen die Attributcodes
      }
      # 1. Suche, ob ein Punkt vorkommt:
      { if (len > 0)
          { var uintB* attrptr = attrptr0;
            var uintL count;
            dotimespL(count,len, { if (*attrptr++ == a_dot) goto dot; } );
          }
        # kein Punkt -> base unverändert lassen
        goto no_dot;
        # Punkt -> base := 10
        dot: *base_ = 10;
        no_dot: ;
      }
      # 2. Alles >=a_letter mit Wert <Basis in a_digit umwandeln:
      if (len > 0)
        { var uintB* attrptr = attrptr0;
          var chart* charptr = charptr0;
          var uintL count;
          dotimespL(count,len,
            { if (*attrptr >= a_letter)
                # Attributcode >= a_letter
                { var cint c = as_cint(*charptr); # Zeichen, muss 'A'-'Z','a'-'Z' sein
                  if (c >= 'a') { c -= 'a'-'A'; }
                  if ((c - 'A') + 10 < *base_) # Wert < Basis ?
                    { *attrptr = a_digit; } # in a_digit umwandeln
                }
              attrptr++; charptr++;
            });
        }
      # 3. Teste, ob nur Attributcodes >=a_ratio vorkommen:
      if (len > 0)
        { var uintB* attrptr = attrptr0;
          var uintL count;
          dotimespL(count,len,
            { if (!(*attrptr++ >= a_ratio))
                { return FALSE; } # nein -> kein potential number
            });
        }
      # 4. Teste, ob ein a_digit vorkommt:
      { if (len > 0)
          { var uintB* attrptr = attrptr0;
            var uintL count;
            dotimespL(count,len, { if (*attrptr++ == a_digit) goto digit_ok; } );
          }
        return FALSE; # kein potential number
        digit_ok: ;
      }
      # Länge len>0.
      # 5. Teste, ob hintereinander zwei Attributcodes >= a_letter kommen:
      if (len > 1)
        { var uintB* attrptr = attrptr0;
          var uintL count;
          dotimespL(count,len-1,
            { if (*attrptr++ >= a_letter)
                { if (*attrptr >= a_letter)
                    { return FALSE; }
                }
            });
        }
      # 6. Teste, ob erster Attributcode >=a_dot, <=a_digit ist:
      { var uintB attr = attrptr0[0];
        if (!((attr >= a_dot) && (attr <= a_digit)))
          { return FALSE; }
      }
      # 7. Teste, ob letzter Attributcode = a_plus oder a_minus ist:
      { var uintB attr = attrptr0[len-1];
        if ((attr == a_plus) || (attr == a_minus))
          { return FALSE; }
      }
      # 8. Potential number liegt vor.
      info->charptr = charptr0; info->attrptr = attrptr0; info->len = len;
      return TRUE;
    }

# UP: Überprüft, ob der Token-Buffer eine Zahl enthält (Syntax gemäß CLTL
# Table 22-2), und stellt gegebenenfalls die für die Umwandlung in eine Zahl
# nötigen Parameter zur Verfügung.
# test_number_syntax(&base,&string,&info)
# > O(token_buff_1): gelesene Characters
# > O(token_buff_2): ihre Attributcodes
# > token_escape_flag: Escape-Zeichen-Flag
# > base: Ziffernsystembasis (Wert von *READ-BASE* oder *PRINT-BASE*)
# < base: Ziffernsystembasis
# < string: Normal-Simple-String mit den Characters
# < info.sign: Vorzeichen (/=0 falls negativ)
# < ergebnis: Zahl-Typ
#     0 : keine Zahl (dann sind auch base,string,info bedeutungslos)
#     1 : Integer
#         < index1: Index der ersten Ziffer
#         < index2: Index nach der letzten Ziffer
#         (also index2-index1 Ziffern, incl. evtl. Dezimalpunkt am Schluss)
#     2 : Rational
#         < index1: Index der ersten Ziffer
#         < index3: Index von '/'
#         < index2: Index nach der letzten Ziffer
#         (also index3-index1 Zähler-Ziffern, index2-index3-1 Nenner-Ziffern)
#     3 : Float
#         < index1: Index vom Mantissenanfang (excl. Vorzeichen)
#         < index4: Index nach dem Mantissenende
#         < index2: Index beim Ende der Characters
#         < index3: Index nach dem Dezimalpunkt (=index4 falls keiner da)
#         (also Mantisse mit index4-index1 Characters: Ziffern und max. 1 '.')
#         (also index4-index3 Nachkommaziffern)
#         (also bei index4<index2: index4 = Index des Exponent-Markers,
#               index4+1 = Index des Exponenten-Vorzeichens oder der ersten
#               Exponenten-Ziffer)
  typedef struct { signean sign;
                   uintL index1;
                   uintL index2;
                   uintL index3;
                   uintL index4;
                 }
          zahl_info;
  local uintWL test_number_syntax (uintWL* base_, object* string_, zahl_info* info);
  local uintWL test_number_syntax(base_,string_,info)
    var uintWL* base_;
    var object* string_;
    var zahl_info* info;
    # Methode:
    # 1. Auf potential number testen.
    #    Dann kommen nur Attributcodes >= a_ratio vor,
    #    und bei a_dot ist base=10.
    # 2. Vorzeichen { a_plus | a_minus | } lesen, merken.
    # 3. versuchen, das Token als rationale Zahl zu interpretieren:
    #    Teste, ob die Syntax
    #    { a_plus | a_minus | }                               # schon gelesen
    #    { a_digit < base }+ { a_ratio { a_digit < base }+ | }
    #    vorliegt.
    # 4. base:=10 setzen, und falls base vorher >10 war, den Characters
    #    'A'-'Z','a'-'z' (waren früher a_letter oder a_expo_m, sind aber evtl.
    #    durch test_potential_number_syntax in a_digit umgewandelt worden)
    #    wieder ihren Attributcode gemäß Tabelle zuordnen (a_letter -> keine
    #    Zahl oder a_expo_m).
    # 5. versuchen, das Token als Floating-Point-Zahl oder Dezimal-Integer
    #    zu interpretieren:
    #    Teste, ob die Syntax
    #    { a_plus | a_minus | }                               # schon gelesen
    #    { a_digit }* { a_dot { a_digit }* | }
    #    { a_expo_m { a_plus | a_minus | } { a_digit }+ | }
    #    vorliegt.
    #    Falls Exponent vorliegt, müssen Vor- oder Nachkommastellen kommen;
    #      es ist ein Float, Typ wird vom Exponent-Marker (e,E liefern den
    #      Wert der Variablen *read-default-float-format* als Typ).
    #    Falls kein Exponent:
    #      Falls kein Dezimalpunkt da, ist es keine Zahl (hätte schon bei
    #        Schritt 3 geliefert werden müssen, aber base hatte offenbar
    #        nicht gepasst).
    #      Falls Dezimalpunkt vorhanden:
    #        Falls Nachkommastellen vorliegen, ist es ein Float (Typ wird
    #          von der Variablen *read-default-float-format* angegeben).
    #        Falls keine Nachkommastellen kommen:
    #          Falls Vorkommastellen da waren, Dezimal-Integer.
    #          Sonst keine Zahl.
    {  var chart* charptr0; # Pointer auf die Characters
       var uintB* attrptr0; # Pointer auf die Attribute
       var uintL len; # Länge des Token
       # 1. Auf potential number testen:
       { if (token_escape_flag) # Token mit Escape-Zeichen ->
           { return 0; } # keine potential number -> keine Zahl
         # Escape-Flag gelöscht.
        {var token_info info;
         if (!test_potential_number_syntax(base_,&info)) # potential number ?
           { return 0; } # nein -> keine Zahl
         # ja -> Ausgabeparameter von test_potential_number_syntax lesen:
         charptr0 = info.charptr;
         attrptr0 = info.attrptr;
         len = info.len;
       }}
       *string_ = TheIarray(O(token_buff_1))->data; # Normal-Simple-String
     { var uintL index0 = 0;
       # 2. Vorzeichen lesen und merken:
       { info->sign = 0; # Vorzeichen:=positiv
         switch (*attrptr0)
           { case a_minus: info->sign = -1; # Vorzeichen:=negativ
             case a_plus:
               # Vorzeichen überlesen:
               charptr0++; attrptr0++; index0++;
             default: break;
           }
       }
       info->index1 = index0; # Startindex
       info->index2 = len; # Endindex
       # info->sign und info->index1 und info->index2 fertig.
       # charptr0 und attrptr0 und index0 ab jetzt unverändert.
      {var uintB flags = 0; # alle Flags löschen
       # 3. Rationale Zahl
       { var chart* charptr = charptr0;
         var uintB* attrptr = attrptr0;
         var uintL index = index0;
         # flags & bit(0)  zeigt an, ob bereits ein a_digit < base
         #                 angetroffen ist.
         # flags & bit(1)  zeigt an, ob bereits ein a_ratio angetroffen ist
         #                 (und dann ist info->index3 dessen Position)
         loop
           { # nächstes Zeichen
             if (index>=len) break;
            {var uintB attr = *attrptr++; # dessen Attributcode
             if (attr==a_digit)
               { var cint c = as_cint(*charptr++); # Character (Digit, also '0'-'9','A'-'Z','a'-'z')
                 # Wert bestimmen:
                 var uintB wert = (c<'A' ? c-'0' : c<'a' ? c-'A'+10 : c-'a'+10);
                 if (wert >= *base_) # Digit mit Wert >=base ?
                   goto schritt4; # ja -> keine rationale Zahl
                 # Digit mit Wert <base
                 flags |= bit(0); # Bit 0 setzen
                 index++;
               }
             elif (attr==a_ratio)
               { if (flags & bit(1)) # nicht der einzige '/' ?
                   goto schritt4; # ja -> keine rationale Zahl
                 flags |= bit(1); # erster '/'
                 if (!(flags & bit(0))) # keine Ziffern vor dem Bruchstrich?
                   goto schritt4; # ja -> keine rationale Zahl
                 flags &= ~bit(0); # Bit 0 löschen, neuer Block fängt an
                 info->index3 = index; # Index des '/' merken
                 charptr++; index++;
               }
             else
               # Attributcode /= a_digit, a_ratio -> keine rationale Zahl
               goto schritt4;
           }}
         # Token zu Ende
         if (!(flags & bit(0))) # keine Ziffern im letzten Block ?
           goto schritt4; # ja -> keine rationale Zahl
         # rationale Zahl
         if (!(flags & bit(1))) # a_ratio aufgetreten?
           # nein -> Integer liegt vor, info ist fertig.
           { return 1; }
           else
           # ja -> Bruch liegt vor, info ist fertig.
           { return 2; }
       }
       schritt4:
       # 4. base:=10, mit Eliminierung von 'A'-'Z','a'-'z'
       if (*base_ > 10)
         { var uintL count = len-index0;
           if (count > 0)
             { var chart* charptr = charptr0;
               var uintB* attrptr = attrptr0;
               dotimespL(count,count,
                 { var chart ch = *charptr++; # nächstes Character
                   var cint c = as_cint(ch);
                   if (((c>='A') && (c<='Z')) || ((c>='a') && (c<='z')))
                     { var uintB attr = attribute_of(ch); # dessen wahrer Attributcode
                       if (attr == a_letter) # Ist er = a_letter ?
                         { return 0; } # ja -> keine Zahl
                       # sonst (muss a_expo_m sein) eintragen:
                       *attrptr = attr;
                     }
                   attrptr++;
                 });
         }   }
       *base_ = 10;
       # 5. Floating-Point-Zahl oder Dezimal-Integer
       { var uintB* attrptr = attrptr0;
         var uintL index = index0;
         # flags & bit(2)  zeigt an, ob bereits ein a_dot angetroffen ist
         #                 (und dann ist info->index3 die Position danach)
         # flags & bit(3)  zeigt an, ob im letzten Ziffernblock bereits ein
         #                 a_digit angetroffen wurde.
         # flags & bit(4)  zeigt an, ob a_dot vorkam und es Vorkommastellen
         #                 gab.
         loop
           { # nächstes Zeichen
             if (index>=len) break;
            {var uintB attr = *attrptr++; # dessen Attributcode
             if (attr==a_digit)
               # Digit ('0'-'9')
               { flags |= bit(3); index++; }
             elif (attr==a_dot)
               { if (flags & bit(2)) # nicht das einzige '.' ?
                   { return 0; } # ja -> keine Zahl
                 flags |= bit(2); # erster '.'
                 if (flags & bit(3)) { flags |= bit(4); } # evtl. mit Vorkommastellen
                 flags &= ~bit(3); # Flag zurücksetzen
                 index++;
                 info->index3 = index; # Index nach dem '.' merken
               }
             elif (attr==a_expo_m)
               { goto expo; } # Nun kommt der Exponent
             else
               { return 0; } # sonst kein Float, also keine Zahl
           }}
         # Token zu Ende, kein Exponent
         if (!(flags & bit(2))) # nur Dezimalziffern ohne '.' ?
           { return 0; } # ja -> keine Zahl
         info->index4 = index;
         if (flags & bit(3)) # mit Nachkommastellen?
           { return 3; } # ja -> Float, info fertig.
         # nein.
         if (!(flags & bit(4))) # auch ohne Vorkommastellen?
           { return 0; } # ja -> nur '.' -> keine Zahl
         # Nur Vorkomma-, keine Nachkommastellen -> Dezimal-Integer.
         # Brauche Dot ganz hinten nicht wegzuschneiden (wird überlesen).
         { return 1; }
         expo:
         # Exponent erreicht.
         info->index4 = index;
         index++; # Exponent-Marker mitzählen
         if (!(flags & bit(2))) { info->index3 = info->index4; } # Default für index3
         if (!(flags & (bit(3)|bit(4)))) # Kamen Vor- oder Nachkommastellen vor?
           { return 0; } # nein -> keine Zahl
         # Exponententeil weiter abarbeiten:
         # flags & bit(5)  zeigt an, ob bereits eine Exponenten-Ziffer da war.
         if (index>=len) { return 0; } # String zu Ende -> keine Zahl
         switch (*attrptr)
           { case a_plus:
             case a_minus:
               attrptr++; index++; # Exponenten-Vorzeichen übergehen
             default: break;
           }
         loop
           { # nächstes Zeichen im Exponenten:
             if (index>=len) break;
             # Es dürfen nur noch Digits kennen:
             if (!(*attrptr++ == a_digit)) { return 0; }
             flags |= bit(5);
             index++;
           }
         # Token nach Exponent zu Ende
         if (!(flags & bit(5))) # keine Ziffer im Exponenten?
           { return 0; } # ja -> keine Zahl
         return 3; # Float, info fertig.
       }
    }}}

# Handler: Signals a READER-ERROR with the same error message as the current
# condition.
  local void signal_reader_error (void* sp, object* frame, object label, object condition);
  local void signal_reader_error(sp,frame,label,condition)
    var void* sp;
    var object* frame;
    var object label;
    var object condition;
    { # (SYS::ERROR-OF-TYPE 'READER-ERROR "~A" condition)
      pushSTACK(S(reader_error)); pushSTACK(O(tildeA)); pushSTACK(condition);
      funcall(L(error_of_type),3);
    }

# UP: Überprüft, ob ein Token nur aus Dots besteht.
# test_dots()
# > O(token_buff_1): gelesene Characters
# > O(token_buff_2): ihre Attributcodes
# < ergebnis: TRUE, falls Token leer ist oder nur aus Dots besteht
  local boolean test_dots (void);
  local boolean test_dots()
    { # Suche nach Attributcode /= a_dot:
      var object bvec = O(token_buff_2); # Semi-Simple-Byte-Vektor
      var uintL len = TheIarray(bvec)->dims[1]/8; # Fill-Pointer
      if (len > 0)
        { var uintB* attrptr = &TheSbvector(TheIarray(bvec)->data)->data[0];
          var uintL count;
          dotimespL(count,len,
            { if (!(*attrptr++ == a_dot)) # Attributcode /= a_dot gefunden?
                { return FALSE; } # ja -> fertig, FALSE
            });
        }
      # alles Dots.
      return TRUE;
    }

# UP: Wandelt ein Zahl-Token in Großbuchstaben um.
# upcase_token();
# > O(token_buff_1): gelesene Characters
# > O(token_buff_2): ihre Attributcodes
  local void upcase_token (void);
  local void upcase_token()
    { var object string = O(token_buff_1); # Semi-Simple-String
      var uintL len = TheIarray(string)->dims[1]; # Fill-Pointer
      if (len > 0)
        { var chart* charptr = &TheSstring(TheIarray(string)->data)->data[0];
          dotimespL(len,len, { *charptr = up_case(*charptr); charptr++; } );
    }   }

# UP: Wandelt ein Stück des gelesenen Tokens in Groß- oder Kleinbuchstaben um.
# case_convert_token(start_index,end_index,direction);
# > O(token_buff_1): gelesene Characters
# > O(token_buff_2): ihre Attributcodes
# > uintL start_index: Startindex des zu konvertierenden Bereiches
# > uintL end_index: Endindex des zu konvertierenden Bereiches
# > uintW direction: Richtung der Konversion
  local void case_convert_token (uintL start_index, uintL end_index, uintW direction);
  local void case_convert_token(start_index,end_index,direction)
    var uintL start_index;
    var uintL end_index;
    var uintW direction;
    { var chart* charptr = &TheSstring(TheIarray(O(token_buff_1))->data)->data[start_index];
      var uintB* attrptr = &TheSbvector(TheIarray(O(token_buff_2))->data)->data[start_index];
      var uintL len = end_index - start_index;
      if (len == 0) return;
      switch (direction)
        { case case_upcase:
            # Nicht-escapte Characters in Großbuchstaben umwandeln:
            do_upcase:
            dotimespL(len,len,
              { if (!(*attrptr == a_escaped)) { *charptr = up_case(*charptr); }
                charptr++; attrptr++;
              });
            break;
          case case_downcase:
            # Nicht-escapte Characters in Kleinbuchstaben umwandeln:
            do_downcase:
            dotimespL(len,len,
              { if (!(*attrptr == a_escaped)) { *charptr = down_case(*charptr); }
                charptr++; attrptr++;
              });
            break;
          case case_preserve:
            # Nichts tun.
            break;
          case case_invert:
            # Falls kein nicht-escapter Kleinbuchstabe vorkommt,
            # alle nicht-escapten Characters in Kleinbuchstaben umwandeln.
            # Falls kein nicht-escapter Großbuchstabe vorkommt,
            # alle nicht-escapten Characters in Großbuchstaben umwandeln.
            # Ansonsten nichts tun.
            { var boolean seen_uppercase = FALSE;
              var boolean seen_lowercase = FALSE;
              var const chart* cptr = charptr;
              var const uintB* aptr = attrptr;
              var uintL count;
              dotimespL(count,len,
                { if (!(*aptr == a_escaped))
                    { var chart c = *cptr;
                      if (!chareq(c,up_case(c))) { seen_lowercase = TRUE; }
                      if (!chareq(c,down_case(c))) { seen_uppercase = TRUE; }
                    }
                  cptr++; aptr++;
                });
              if (seen_uppercase)
                { if (!seen_lowercase) goto do_downcase; }
                else
                { if (seen_lowercase) goto do_upcase; }
            }
            break;
          default: NOTREACHED
    }   }

# UP: Wandelt das gesamte gelesene Token in Groß- oder Kleinbuchstaben um.
# case_convert_token_1();
  local void case_convert_token_1 (void);
  local void case_convert_token_1()
    { var object readtable;
      get_readtable(readtable = );
     {var uintW direction = (uintW)posfixnum_to_L(TheReadtable(readtable)->readtable_case);
      var uintL len = TheIarray(O(token_buff_1))->dims[1]; # Länge = Fill-Pointer
      case_convert_token(0,len,direction);
    }}

# UP: Behandelt ein Read-Macro-Character:
# Ruft die zugehörige Macro-Funktion auf, bei Dispatch-Characters erst noch
# Zahl-Argument und Subchar einlesen.
# read_macro(ch,&stream)
# > ch: Macro-Character, ein Character
# > stream: Stream
# < stream: Stream
# < mv_count/mv_space: max. 1 Wert
# can trigger GC
  local Values read_macro (object ch, const object* stream_);
  local Values read_macro(ch,stream_)
    var object ch;
    var const object* stream_;
    { var object readtable;
      get_readtable(readtable = ); # aktuelle Readtable (brauche ch nicht zu retten)
     {var object macrodef = # Macro-Definition aus Tabelle holen
        perchar_table_get(TheReadtable(readtable)->readtable_macro_table,char_code(ch));
      if (nullp(macrodef)) # =NIL ?
        { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
          pushSTACK(ch);
          pushSTACK(*stream_);
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: ~ has no macro character definition")
                );
        }
      if (!simple_vector_p(macrodef)) # ein Simple-Vector?
        # ch normales Macro-Character, macrodef Funktion
        { pushSTACK(*stream_); # Stream als 1. Argument
          pushSTACK(ch); # Character als 2. Argument
          funcall(macrodef,2); # Funktion aufrufen
          if (mv_count > 1)
            { pushSTACK(fixnum(mv_count)); # Wertezahl als Fixnum
              pushSTACK(ch);
              pushSTACK(*stream_);
              pushSTACK(S(read));
              fehler(error,
                     GETTEXT("~ from ~: macro character definition for ~ may not return ~ values, only one value.")
                    );
            }
          # höchstens 1 Wert.
          return; # mv_space/mv_count belassen
        }
        else
        # Dispatch-Macro-Zeichen.
        # When this changes, keep DISPATCH-READER in defs2.lsp up to date.
        { pushSTACK(macrodef); # Vektor retten
         {var object arg; # Argument (Integer >=0 oder NIL)
          var object subch; # sub-char
          var chart subc; # sub-char
          # Ziffern des Argumentes lesen:
          { var boolean flag = FALSE; # Flag, ob schon eine Ziffer kam
            pushSTACK(Fixnum_0); # bisheriger Integer := 0
            loop
              { var object nextch = read_char(stream_); # Character lesen
                if (eq(nextch,eof_value))
                  { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
                    pushSTACK(ch); # main char
                    pushSTACK(*stream_); # Stream
                    pushSTACK(S(read));
                    fehler(end_of_file,
                           GETTEXT("~: input stream ~ ends within read macro beginning to ~")
                          );
                  }
                # Sonst auf Character überprüfen.
                if (!charp(nextch)) { fehler_charread(nextch,stream_); }
               {var chart ch = char_code(nextch);
                var cint c = as_cint(ch);
                if (!((c>='0') && (c<='9'))) # keine Ziffer -> Schleife fertig
                  { subc = ch; break; }
                # Integer mal 10 nehmen und Ziffer addieren:
                STACK_0 = mal_10_plus_x(STACK_0,(c-'0'));
                flag = TRUE;
              }}
            # Argument in STACK_0 fertig (nur falls flag=TRUE).
            arg = popSTACK();
            if (!flag) { arg = NIL; } # kam keine Ziffer -> Argument := NIL
          }
          # Weiter geht's mit Subchar (Character subc)
          subch = code_char(subc);
          subc = up_case(subc); # Subchar in Großbuchstaben umwandeln
          macrodef = popSTACK(); # Vektor zurück
          macrodef = perchar_table_get(macrodef,subc); # Subchar-Funktion oder NIL
          if (nullp(macrodef))
            # NIL -> undefiniert
            { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
              pushSTACK(subch); # Subchar
              pushSTACK(ch); # Mainchar
              pushSTACK(*stream_); # Stream
              pushSTACK(S(read));
              fehler(stream_error,
                     GETTEXT("~ from ~: After ~ is ~ an undefined dispatch macro character")
                    );
            }
          pushSTACK(*stream_); # Stream als 1. Argument
          pushSTACK(subch); # Subchar als 2. Argument
          pushSTACK(arg); # Argument (NIL oder Integer>=0) als 3. Argument
          funcall(macrodef,3); # Funktion aufrufen
          if (mv_count > 1)
            { pushSTACK(fixnum(mv_count)); # Wertezahl als Fixnum
              pushSTACK(ch); # Mainchar
              pushSTACK(subch); # Subchar
              pushSTACK(*stream_); # Stream
              pushSTACK(S(read));
              fehler(error,
                     GETTEXT("~ from ~: dispatch macro character definition for ~ after ~ may not return ~ values, only one value.")
                    );
            }
          # höchstens 1 Wert.
          return; # mv_space/mv_count belassen
        }}
    }}

# ------------------------ READ auf Objekt-Ebene ------------------------------

# UP: Liest ein Objekt ein.
# Überliest dabei führenden Whitespace und Kommentar.
# Maßgeblich sind die aktuellen Werte von SYS::*READ-PRESERVE-WHITESPACE*
# (fürs evtl. Überlesen des ersten Whitespace nach dem Objekt)
# und SYS::*READ-RECURSIVE-P* (für EOF-Behandlung).
# read_internal(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Objekt (eof_value bei EOF, dot_value bei einzelnem Punkt)
# can trigger GC
  local object read_internal (const object* stream_);
  local object read_internal(stream_)
    var const object* stream_;
    { wloop: # Schleife zum Überlesen von führendem Whitespace/Kommentar:
       {var object ch;
        var uintWL scode;
        read_char_syntax(ch = ,scode = ,stream_); # Zeichen lesen
        switch(scode)
          { case syntax_whitespace:
              # Whitespace -> wegwerfen und weiterlesen
              goto wloop;
            case syntax_t_macro:
            case syntax_nt_macro:
              # Macro-Zeichen am Token-Anfang
              read_macro(ch,stream_); # Macro-Funktion ausführen
              if (mv_count==0)
                # 0 Werte -> weiterlesen
                { goto wloop; }
                else
                # 1 Wert -> als Ergebnis
                { return value1; }
            case syntax_eof:
              # EOF am Token-Anfang
              if (test_value(S(read_recursive_p))) # *READ-RECURSIVE-P* /= NIL ?
                # ja -> EOF innerhalb eines Objektes -> Fehler
                { fehler_eof_innen(stream_); }
              # sonst eof_value als Wert:
              return eof_value;
            case syntax_illegal:
              # read_token_1 liefert Error
            case syntax_single_esc:
            case syntax_multi_esc:
            case syntax_constituent:
              # Token lesen: Mit dem Zeichen ch fängt ein Token an.
              read_token_1(stream_,ch,scode); # Token zu Ende lesen
              break;
            default: NOTREACHED
       }  }
      # Token gelesen
      if (test_value(S(read_suppress))) # *READ-SUPPRESS* /= NIL ?
        { return NIL; } # ja -> Token nicht interpretieren, NIL als Wert
      # Token muss interpretiert werden
      # Der Token liegt in O(token_buff_1), O(token_buff_2), token_escape_flag.
      if ((!token_escape_flag) && test_dots())
        # Token ist eine Folge von Dots, ohne Escape-Characters gelesen.
        # Länge ist damit automatisch >0.
        { var uintL len = TheIarray(O(token_buff_1))->dims[1]; # Länge des Token
          if (len > 1)
            # Länge>1 -> Fehler
            { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
              pushSTACK(*stream_);
              pushSTACK(S(read));
              fehler(stream_error,
                     GETTEXT("~ from ~: a token consisting only of dots cannot be meaningfully read in")
                    );
            }
          # Länge=1 -> dot_value als Wert
          return dot_value;
        }
      # Token ist OK
      { var uintWL base = get_read_base(); # Wert von *READ-BASE*
        # Token als Zahl interpretierbar?
        var object string;
        var zahl_info info;
        var uintWL numtype = test_number_syntax(&base,&string,&info);
        if (!(numtype==0)) # Zahl?
          { upcase_token(); # in Großbuchstaben umwandeln
           {var object result;
            # ANSI CL 2.3.1.1 requires that we transform ARITHMETIC-ERROR into READER-ERROR
            make_HANDLER_frame(O(handler_for_arithmetic_error),&signal_reader_error,NULL);
            switch (numtype)
              { case 1: # Integer
                  result = read_integer(base,info.sign,string,info.index1,info.index2);
                  break;
                case 2: # Rational
                  result = read_rational(base,info.sign,string,info.index1,info.index3,info.index2);
                  break;
                case 3: # Float
                  result = read_float(base,info.sign,string,info.index1,info.index4,info.index2,info.index3);
                  break;
                default: NOTREACHED
              }
            unwind_HANDLER_frame();
            return result;
          }}
      }
      # Token nicht als Zahl interpretierbar.
      # Wir interpretieren das Token als Symbol (auch dann, wenn das Token
      # Potential-number-Syntax hat, also ein 'reserved token' (im Sinne
      # von CLTL S. 341 oben) ist).
      # Dazu erst einmal die Verteilung der Doppelpunkte (Characters mit
      # Attributcode a_pack_m) feststellen:
      # Suche von vorne den ersten Doppelpunkt. Fälle (CLTL S. 343-344):
      # 1. Kein Doppelpunkt -> aktuelle Package
      # 2. Ein oder zwei Doppelpunkte am Anfang -> Keyword
      # 3. Ein Doppelpunkt, nicht am Anfang -> externes Symbol
      # 4. Zwei Doppelpunkte, nicht am Anfang -> internes Symbol
      # In den letzten drei Fällen dürfen keine weiteren Doppelpunkte mehr
      # kommen.
      # (Dass bei 2. der Namensteil bzw. bei 3. und 4. der Packageteil und
      # der Namensteil nicht die Syntax einer Zahl haben, kann hier nicht
      # mehr überprüft werden, weil sich TOKEN_ESCAPE_FLAG auf das ganze
      # Token bezieht. Vergleiche |USER|:: und |USER|::|| )
      { var uintW direction; # Richtung der Case-Konversion
        { var object readtable;
          get_readtable(readtable = );
          direction = (uintW)posfixnum_to_L(TheReadtable(readtable)->readtable_case);
        }
       {var object buff_2 = O(token_buff_2); # Attributcode-Buffer
        var uintL len = TheIarray(buff_2)->dims[1]/8; # Länge = Fill-Pointer
        var uintB* attrptr = &TheSbvector(TheIarray(buff_2)->data)->data[0];
        var uintL index = 0;
        # stets attrptr = &TheSbvector(...)->data[index].
        # Token wird in Packagenamen und Namen zerhackt:
        var uintL pack_end_index;
        var uintL name_start_index;
        var boolean external_internal_flag = FALSE; # vorläufig external
        loop
          { if (index>=len) goto current; # kein Doppelpunkt gefunden -> current package
            if (*attrptr++ == a_pack_m) break;
            index++;
          }
        # erster Doppelpunkt bei Index index gefunden
        pack_end_index = index; # Packagename endet hier
        index++;
        name_start_index = index; # Symbolname fängt (vorläufig) hier an
        # Tokenende erreicht -> externes Symbol:
        if (index>=len) goto ex_in_ternal;
        # Kommt sofort danach ein weiterer Doppelpunkt?
        index++;
        if (*attrptr++ == a_pack_m)
          # zwei Doppelpunkte nebeneinander
          { name_start_index = index; # Symbolname fängt erst hier an
            external_internal_flag = TRUE; # internal
          }
          else
          # erster Doppelpunkt war isoliert
          {} # external
        # Es dürfen keine weiteren Doppelpunkte kommen:
        loop
          { if (index>=len) goto ex_in_ternal; # kein weiterer Doppelpunkt gefunden -> ok
            if (*attrptr++ == a_pack_m) break;
            index++;
          }
        { # Fehlermeldung
          pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
          pushSTACK(copy_string(O(token_buff_1))); # Character-Buffer kopieren
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: too many colons in token ~")
                );
        }
        # Symbol suchen bzw. erzeugen:
        current: # Symbol in der current package suchen.
        # Symbolname = O(token_buff_1) = (subseq O(token_buff_1) 0 len)
        # ist ein nicht-simpler String.
        { var object pack = get_current_package();
          if (!pack_casesensitivep(pack))
            { case_convert_token(0,len,direction); }
          # Symbol internieren (und dabei String kopieren, falls das Symbol
          # neu erzeugt werden muss):
         {var object sym;
          intern(O(token_buff_1),pack,&sym);
          return sym;
        }}
        ex_in_ternal: # externes/internes Symbol bilden
        # Packagename = (subseq O(token_buff_1) 0 pack_end_index),
        # Symbolname = (subseq O(token_buff_1) name_start_index len).
        case_convert_token(0,pack_end_index,direction);
        if (pack_end_index==0)
          # Doppelpunkt(e) am Anfang -> Keyword bilden:
          { # Symbolname = (subseq O(token_buff_1) name_start_index len).
            case_convert_token(name_start_index,len,direction);
            # Hilfs-String adjustieren:
           {var object hstring = O(displaced_string);
            TheIarray(hstring)->data = O(token_buff_1); # Datenvektor
            TheIarray(hstring)->dims[0] = name_start_index; # Displaced-Offset
            TheIarray(hstring)->totalsize =
              TheIarray(hstring)->dims[1] = len - name_start_index; # Länge
            # Symbol in die Keyword-Package internieren (und dabei
            # String kopieren, falls das Symbol neu erzeugt werden muss):
            return intern_keyword(hstring);
          }}
        { # Packagename = (subseq O(token_buff_1) 0 pack_end_index).
          # Hilfs-String adjustieren:
          var object hstring = O(displaced_string);
          TheIarray(hstring)->data = O(token_buff_1); # Datenvektor
          TheIarray(hstring)->dims[0] = 0; # Displaced-Offset
          TheIarray(hstring)->totalsize =
            TheIarray(hstring)->dims[1] = pack_end_index; # Länge
          # Package mit diesem Namen suchen:
         {var object pack = find_package(hstring);
          if (nullp(pack)) # Package nicht gefunden?
            { pushSTACK(copy_string(hstring)); # Displaced-String kopieren, Wert für Slot PACKAGE von PACKAGE-ERROR
              pushSTACK(STACK_0);
              pushSTACK(*stream_); # Stream
              pushSTACK(S(read));
              fehler(package_error,
                     GETTEXT("~ from ~: there is no package with name ~")
                    );
            }
          if (!pack_casesensitivep(pack))
            { case_convert_token(name_start_index,len,direction); }
          # Hilfs-String adjustieren:
          TheIarray(hstring)->dims[0] = name_start_index; # Displaced-Offset
          TheIarray(hstring)->totalsize =
            TheIarray(hstring)->dims[1] = len - name_start_index; # Länge
          if (external_internal_flag)
            # internal
            { # Symbol internieren (und dabei String kopieren,
              # falls das Symbol neu erzeugt werden muss):
              var object sym;
              intern(hstring,pack,&sym);
              return sym;
            }
            else
            # external
            { # externes Symbol mit diesem Printnamen suchen:
              var object sym;
              if (find_external_symbol(hstring,pack,&sym))
                { return sym; } # sym gefunden
                else
                { pushSTACK(pack); # Wert für Slot PACKAGE von PACKAGE-ERROR
                  pushSTACK(copy_string(hstring)); # Displaced-String kopieren
                  pushSTACK(STACK_1); # pack
                  pushSTACK(*stream_); # Stream
                  pushSTACK(S(read));
                  fehler(package_error,
                         GETTEXT("~ from ~: ~ has no external symbol with name ~")
                        );
            }   }
        }}
    } }}

# UP: Liest ein Objekt ein, mit SYS::*READ-RECURSIVE-P* /= NIL
# (und SYS::*READ-PRESERVE-WHITESPACE* = NIL, vgl. CLTL S. 377 mitte).
# Meldet bei EOF einen Error.
# read_recursive(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Objekt (dot_value bei einzelnem Punkt)
# can trigger GC
  local object read_recursive (const object* stream_);
  local object read_recursive(stream_)
    var const object* stream_;
    { check_SP(); check_STACK(); # Stacks auf Überlauf testen
      if (test_value(S(read_recursive_p)))
        # schon rekursiv
        { return read_internal(stream_); }
        else
        { # SYS::*READ-RECURSIVE-P* an T binden:
          dynamic_bind(S(read_recursive_p),T);
          # und SYS::*READ-PRESERVE-WHITESPACE* an NIL binden:
          dynamic_bind(S(read_preserve_whitespace),NIL);
          # und Objekt lesen:
         {var object ergebnis = read_internal(stream_);
          dynamic_unbind();
          dynamic_unbind();
          return ergebnis;
        }}
    }

# Fehlermeldung wegen unpassendem Dot
# fehler_dot(stream);
# > stream: Stream
  nonreturning_function(local, fehler_dot, (object stream));
  local void fehler_dot(stream)
    var object stream;
    { pushSTACK(stream); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(stream); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: token \".\" not allowed here")
            );
    }

# UP: Liest ein Objekt ein, mit SYS::*READ-RECURSIVE-P* /= NIL
# (und SYS::*READ-PRESERVE-WHITESPACE* = NIL, vgl. CLTL S. 377 mitte).
# Meldet Error bei EOF oder Token ".".
# (Das entspricht dem Idiom (read stream t nil t).)
# read_recursive_no_dot(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Objekt
# can trigger GC
  local object read_recursive_no_dot (const object* stream_);
  local object read_recursive_no_dot(stream_)
    var const object* stream_;
    { # READ rekursiv aufrufen:
      var object ergebnis = read_recursive(stream_);
      # und bei "." einen Error melden:
      if (eq(ergebnis,dot_value)) { fehler_dot(*stream_); }
      return ergebnis;
    }

# UP: Entflicht #n# - Referenzen zu #n= - Markierungen in einem Objekt.
# > Wert von SYS::*READ-REFERENCE-TABLE*:
#     Aliste von Paaren (Markierung . markiertes Objekt), wobei
#     jede Markierung ein Objekt  #<READ-LABEL n>  ist.
# > obj: Objekt
# < ergebnis: destruktiv modifiziertes Objekt ohne Referenzen
  local object make_references (object obj);
  local object make_references(obj)
    var object obj;
    { var object alist = Symbol_value(S(read_reference_table));
      # SYS::*READ-REFERENCE-TABLE* = NIL -> nichts zu tun:
      if (nullp(alist))
        { return obj; }
        else
        { # Überprüfen, ob SYS::*READ-REFERENCE-TABLE* eine Aliste ist:
         {var object alistr = alist; # Liste durchlaufen
          while (consp(alistr))
            { # jedes Listenelement muss ein Cons sein:
              if (!mconsp(Car(alistr))) goto fehler_badtable;
              alistr = Cdr(alistr);
            }
          if (!nullp(alistr))
            { fehler_badtable:
              pushSTACK(S(read_reference_table));
              pushSTACK(S(read));
              fehler(error,
                     GETTEXT("~: the value of ~ has been arbitrarily altered")
                    );
            }
         }# Aliste alist ist OK
          pushSTACK(obj);
          {var object bad_reference =
            subst_circ(&STACK_0,alist); # Referenzen durch Objekte substituieren
           if (!eq(bad_reference,nullobj))
             { pushSTACK(unbound); # "Wert" für Slot STREAM von STREAM-ERROR
               pushSTACK(Symbol_value(S(read_reference_table)));
               pushSTACK(S(read_reference_table));
               pushSTACK(obj);
               pushSTACK(bad_reference);
               pushSTACK(S(read));
               fehler(stream_error,
                      GETTEXT("~: no entry for ~ from ~ in ~ = ~")
                     );
          }  }
          return popSTACK();
        }
    }

# UP: Liest ein Objekt ein, mit SYS::*READ-RECURSIVE-P* = NIL .
# (Top-Level-Aufruf des Readers)
# read_top(&stream,whitespace-p)
# > whitespace-p: gibt an, ob danach whitespace zu verbrauchen ist
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Objekt (eof_value bei EOF, dot_value bei einzelnem Punkt)
# can trigger GC
  local object read_top (const object* stream_, object whitespace_p);
  local object read_top(stream_,whitespace_p)
    var const object* stream_;
    var object whitespace_p;
    {
     #if STACKCHECKR
      var object* STACKbefore = STACK; # STACK aufheben für später
     #endif
      # SYS::*READ-RECURSIVE-P* an NIL binden:
      dynamic_bind(S(read_recursive_p),NIL);
      # und SYS::*READ-PRESERVE-WHITESPACE* an whitespace_p binden:
      dynamic_bind(S(read_preserve_whitespace),whitespace_p);
      # SYS::*READ-REFERENCE-TABLE* an die leere Tabelle NIL binden:
      dynamic_bind(S(read_reference_table),NIL);
      # SYS::*BACKQUOTE-LEVEL* an NIL binden:
      dynamic_bind(S(backquote_level),NIL);
      # Objekt lesen:
     {var object obj = read_internal(stream_);
      # Verweise entflechten:
      obj = make_references(obj);
      dynamic_unbind();
      dynamic_unbind();
      dynamic_unbind();
      dynamic_unbind();
     #if STACKCHECKR
      # Überprüfen, ob Stack aufgeräumt:
      if (!(STACK == STACKbefore))
        { abort(); } # wenn nicht, in den Debugger
     #endif
      return obj;
    }}

# UP: Liest ein Objekt ein.
# stream_read(&stream,recursive-p,whitespace-p)
# > recursive-p: gibt an, ob rekursiver Aufruf von READ, mit Error bei EOF
# > whitespace-p: gibt an, ob danach whitespace zu verbrauchen ist
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Objekt (eof_value bei EOF, dot_value bei einzelnem Punkt)
# can trigger GC
  global object stream_read (const object* stream_, object recursive_p, object whitespace_p);
  global object stream_read(stream_,recursive_p,whitespace_p)
    var const object* stream_;
    var object recursive_p;
    var object whitespace_p;
    { if (nullp(recursive_p)) # recursive-p abfragen
        # nein -> Top-Level-Aufruf
        { return read_top(stream_,whitespace_p); }
        else
        # ja -> rekursiver Aufruf
        { return read_recursive(stream_); }
    }

# ----------------------------- READ-Macros -----------------------------------

# UP: Liest eine Liste ein.
# read_delimited_list(&stream,endch,ifdotted)
# > endch: erwartetes Endzeichen, ein Character
# > ifdotted: #DOT_VALUE falls Dotted List erlaubt, #EOF_VALUE sonst
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Objekt
# can trigger GC
  local object read_delimited_list (const object* stream_, object endch, object ifdotted);
# Dito mit gesetztem SYS::*READ-RECURSIVE-P* :
  local object read_delimited_list_recursive (const object* stream_, object endch, object ifdotted);
# Erst die allgemeine Funktion:
  #ifdef RISCOS_CCBUG
    #pragma -z0
  #endif
  local object read_delimited_list(stream_,endch,ifdotted)
    var const object* stream_;
    var object endch;
    var object ifdotted;
    { var object ergebnis;
      # SYS::*READ-LINE-NUMBER* an (SYS::LINE-NUMBER stream) binden
      # (für Fehlermeldung, damit man die Zeile der öffnenden Klammer erfährt):
      var object lineno = stream_line_number(*stream_);
      dynamic_bind(S(read_line_number),lineno);
      # evtl. zuerst noch SYS::*READ-RECURSIVE-P* an T binden:
      if (test_value(S(read_recursive_p))) # schon rekursiv?
        { ergebnis = read_delimited_list_recursive(stream_,endch,ifdotted); }
        else
        # nein -> SYS::*READ-RECURSIVE-P* an T binden:
        { dynamic_bind(S(read_recursive_p),T);
          ergebnis = read_delimited_list_recursive(stream_,endch,ifdotted);
          dynamic_unbind();
        }
      dynamic_unbind();
      return ergebnis;
    }
  #ifdef RISCOS_CCBUG
    #pragma -z1
  #endif
# Dann die speziellere Funktion:
  local object read_delimited_list_recursive(stream_,endch,ifdotted)
    var const object* stream_;
    var object endch;
    var object ifdotted;
    { # Brauche endch und ifdotted nicht zu retten.
      { var object object1; # erstes Listenelement
        loop # Schleife, um erstes Listenelement zu lesen
          { # nächstes non-whitespace Character:
            var object ch;
            var uintWL scode;
            wpeek_char_syntax(ch = ,scode = ,stream_);
            if (eq(ch,endch)) # Ist es das erwartete Endezeichen?
              # ja -> leere Liste als Ergebnis
              { read_char(stream_); # Endezeichen verbrauchen
                return NIL;
              }
            if (scode < syntax_t_macro) # Macro-Character?
              # nein -> 1. Objekt lesen:
              { object1 = read_recursive_no_dot(stream_); break; }
              else
              # ja -> zugehöriges Zeichen lesen und Macro-Funktion ausführen:
              { ch = read_char(stream_);
                read_macro(ch,stream_);
                if (!(mv_count==0)) # Wert zurück?
                  { object1 = value1; break; } # ja -> als 1. Objekt nehmen
                  # nein -> überlesen
              }
          }
        # object1 ist das 1. Objekt
        pushSTACK(object1);
      }
      { var object new_cons = allocate_cons(); # Listenanfang basteln
        Car(new_cons) = popSTACK(); # new_cons = (cons object1 nil)
        pushSTACK(new_cons);
        pushSTACK(new_cons);
      }
      # Stackaufbau: Gesamtliste, (last Gesamtliste).
      loop # Schleife über weitere Listenelemente
        { var object object1; # weiteres Listenelement
          loop # Schleife, um weiteres Listenelement zu lesen
            { # nächstes non-whitespace Character:
              var object ch;
              var uintWL scode;
              wpeek_char_syntax(ch = ,scode = ,stream_);
              if (eq(ch,endch)) # Ist es das erwartete Endezeichen?
                # ja -> Liste beenden
                { finish_list:
                  read_char(stream_); # Endezeichen verbrauchen
                  skipSTACK(1); return popSTACK(); # Gesamtliste als Ergebnis
                }
              if (scode < syntax_t_macro) # Macro-Character?
                # nein -> nächstes Objekt lesen:
                { object1 = read_recursive(stream_);
                  if (eq(object1,dot_value)) goto dot;
                  break;
                }
                else
                # ja -> zugehöriges Zeichen lesen und Macro-Funktion ausführen:
                { ch = read_char(stream_);
                  read_macro(ch,stream_);
                  if (!(mv_count==0)) # Wert zurück?
                    { object1 = value1; break; } # ja -> als nächstes Objekt nehmen
                    # nein -> überlesen
                }
            }
          # nächstes Objekt in die Liste einhängen:
          pushSTACK(object1);
         {var object new_cons = allocate_cons(); # nächstes Listen-Cons
          Car(new_cons) = popSTACK(); # (cons object1 nil)
          Cdr(STACK_0) = new_cons; # =: (cdr (last Gesamtliste))
          STACK_0 = new_cons;
        }}
      dot: # Dot gelesen
      if (!eq(ifdotted,dot_value)) # war keiner erlaubt?
        { fehler_dot(*stream_); }
      { var object object1; # letztes Listenelement
        loop # Schleife, um letztes Listenelement zu lesen
          { # nächstes non-whitespace Character:
            var object ch;
            var uintWL scode;
            wpeek_char_syntax(ch = ,scode = ,stream_);
            if (eq(ch,endch)) # Ist es das erwartete Endezeichen?
              # ja -> Fehler
              { fehler_dot:
                pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
                pushSTACK(*stream_); # Stream
                pushSTACK(S(read_delimited_list));
                fehler(stream_error,
                       GETTEXT("~ from ~: illegal end of dotted list")
                      );
              }
            if (scode < syntax_t_macro) # Macro-Character?
              # nein -> letztes Objekt lesen:
              { object1 = read_recursive_no_dot(stream_); break; }
              else
              # ja -> zugehöriges Zeichen lesen und Macro-Funktion ausführen:
              { ch = read_char(stream_);
                read_macro(ch,stream_);
                if (!(mv_count==0)) # Wert zurück?
                  { object1 = value1; break; } # ja -> als letztes Objekt nehmen
                  # nein -> überlesen
              }
          }
        # object1 ist das letzte Objekt
        # als (cdr (last Gesamtliste)) in die Liste einhängen:
        Cdr(STACK_0) = object1;
      }
      loop # Schleife, um Kommentar nach letztem Listenelement zu lesen
        { # nächstes non-whitespace Character:
          var object ch;
          var uintWL scode;
          wpeek_char_syntax(ch = ,scode = ,stream_);
          if (eq(ch,endch)) # Ist es das erwartete Endezeichen?
            { goto finish_list; } # ja -> Liste fertig
          if (scode < syntax_t_macro) # Macro-Character?
            # nein -> Dot kam zu früh, Fehler
            { goto fehler_dot; }
            else
            # ja -> zugehöriges Zeichen lesen und Macro-Funktion ausführen:
            { ch = read_char(stream_);
              read_macro(ch,stream_);
              if (!(mv_count==0)) # Wert zurück?
                { goto fehler_dot; } # ja -> Dot kam zu früh, Fehler
                # nein -> überlesen
            }
        }
    }

# Macro: Überprüft das Stream-Argument eines SUBRs.
# stream_ = test_stream_arg(stream);
# > stream: Stream-Argument im STACK
# > subr_self: Aufrufer (ein SUBR)
# < stream_: &stream
  #define test_stream_arg(stream)  \
    (!streamp(stream) ? (fehler_stream(stream), (object*)NULL) : &(stream))

# (set-macro-character #\(
#   #'(lambda (stream char)
#       (read-delimited-list #\) stream t :dot-allowed t)
# )   )
LISPFUNN(lpar_reader,2) # liest (
  { var object* stream_ = test_stream_arg(STACK_1);
    # Liste nach '(' bis ')' lesen, Dot erlaubt:
    value1 = read_delimited_list(stream_,ascii_char(')'),dot_value); mv_count=1;
    skipSTACK(2);
  }

# #| ( ( |#
# (set-macro-character #\)
#   #'(lambda (stream char)
#       (error "~ von ~: ~ am Anfang eines Objekts" 'read stream char)
# )   )
LISPFUNN(rpar_reader,2) # liest )
  { var object* stream_ = test_stream_arg(STACK_1);
    pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
    pushSTACK(STACK_(0+1)); # char
    pushSTACK(*stream_); # stream
    pushSTACK(S(read));
    fehler(stream_error,
           GETTEXT("~ from ~: an object cannot start with ~")
          );
  }

# (set-macro-character #\"
#   #'(lambda (stream char)
#       (let ((buffer (make-array 50 :element-type 'character
#                                    :adjustable t :fill-pointer 0
#            ))       )
#         (loop
#           (multiple-value-bind (ch sy) (read-char-syntax stream)
#             (cond ((eq sy 'eof-code)
#                    (error "~: Eingabestream ~ endet innerhalb eines Strings."
#                           'read stream
#                   ))
#                   ((eql ch char) (return (coerce buffer 'simple-string)))
#                   ((eq sy 'single-escape)
#                    (multiple-value-setq (ch sy) (read-char-syntax stream))
#                    (when (eq sy 'eof-code) (error ...))
#                    (vector-push-extend ch buffer)
#                   )
#                   (t (vector-push-extend ch buffer))
#         ) ) )
#         (if *read-suppress* nil (coerce buffer 'simple-string))
# )   ) )
LISPFUNN(string_reader,2) # liest "
  { var object* stream_ = test_stream_arg(STACK_1);
    # Stackaufbau: stream, char.
    if (test_value(S(read_suppress))) # *READ-SUPPRESS* /= NIL ?
      # ja -> String nur überlesen:
      { loop
          { # nächstes Zeichen lesen:
            var object ch;
            var uintWL scode;
            read_char_syntax(ch = ,scode = ,stream_);
            if (scode == syntax_eof) goto fehler_eof; # EOF -> Fehler
            if (eq(ch,STACK_0)) break; # selbes Zeichen wie char -> fertig
            if (scode == syntax_single_esc) # Single-Escape-Character?
              # ja -> nochmal ein Zeichen lesen:
              { read_char_syntax(ch = ,scode = ,stream_);
                if (scode == syntax_eof) goto fehler_eof; # EOF -> Fehler
              }
          }
        value1 = NIL; # NIL als Wert
      }
      else
      # nein -> String wirklich lesen
      { get_buffers(); # zwei leere Buffer auf den Stack
        # Stackaufbau: stream, char, Buffer, andererBuffer.
        loop
          { # nächstes Zeichen lesen:
            var object ch;
            var uintWL scode;
            read_char_syntax(ch = ,scode = ,stream_);
            if (scode == syntax_eof) goto fehler_eof; # EOF -> Fehler
            if (eq(ch,STACK_2)) break; # selbes Zeichen wie char -> fertig
            if (scode == syntax_single_esc) # Single-Escape-Character?
              # ja -> nochmal ein Zeichen lesen:
              { read_char_syntax(ch = ,scode = ,stream_);
                if (scode == syntax_eof) goto fehler_eof; # EOF -> Fehler
              }
            # Zeichen ch in den Buffer schieben:
            ssstring_push_extend(STACK_1,char_code(ch));
          }
        # Buffer kopieren und dabei in Simple-String umwandeln:
        { var object string;
          #ifndef TYPECODES
          if (TheStream(*stream_)->strmflags & bit(strmflags_immut_bit_B))
            { string = coerce_imm_ss(STACK_1); }
            else
          #endif
            { string = copy_string(STACK_1); }
          value1 = string;
        }
        # Buffer zur Wiederverwendung freigeben:
        O(token_buff_2) = popSTACK(); O(token_buff_1) = popSTACK();
      }
    mv_count=1; skipSTACK(2); return;
    fehler_eof:
      pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(end_of_file,
             GETTEXT("~: input stream ~ ends within a string")
            );
  }

# Liest ein Objekt und bildet eine zweielementige Liste.
# list2_reader(stream_);
# > Stackaufbau: stream, symbol.
# erhöht STACK um 2
# verändert STACK, kann GC auslösen
  local Values list2_reader (const object* stream_);
  local Values list2_reader(stream_)
    var const object* stream_;
    { var object obj = read_recursive_no_dot(stream_); # Objekt lesen
      pushSTACK(obj);
      pushSTACK(allocate_cons()); # zweites Listencons
     {var object new_cons1 = allocate_cons(); # erstes Listencons
      var object new_cons2 = popSTACK(); # zweites Listencons
      Car(new_cons2) = popSTACK(); # new_cons2 = (cons obj nil)
      Cdr(new_cons1) = new_cons2; Car(new_cons1) = STACK_0; # new_cons1 = (cons symbol new_cons2)
      value1 = new_cons1; mv_count=1; skipSTACK(2);
    }}

# (set-macro-character #\'
#   #'(lambda (stream char)
#       (list 'QUOTE (read stream t nil t))
# )   )
LISPFUNN(quote_reader,2) # liest '
  { var object* stream_ = test_stream_arg(STACK_1);
    STACK_0 = S(quote); return_Values list2_reader(stream_);
  }

# (set-macro-character #\;
#   #'(lambda (stream char)
#       (loop
#         (let ((ch (read-char stream)))
#           (when (or (eql ch 'eof-code) (eql ch #\Newline)) (return))
#       ) )
#       (values)
# )   )
LISPFUNN(line_comment_reader,2) # liest ;
  { var object* stream_ = test_stream_arg(STACK_1);
    loop
      { var object ch = read_char(stream_); # Zeichen lesen
        if (eq(ch,eof_value) || eq(ch,ascii_char(NL))) break;
      }
    value1 = NIL; mv_count=0; skipSTACK(2); # keine Werte zurück
  }

# ------------------------- READ-Dispatch-Macros ------------------------------

# Fehlermeldung wegen einer unerlaubten Zahl bei Dispatch-Macros
# fehler_dispatch_zahl();
# > STACK_1: Stream
# > STACK_0: sub-char
  nonreturning_function(local, fehler_dispatch_zahl, (void));
  local void fehler_dispatch_zahl()
    { pushSTACK(STACK_1); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(STACK_(0+1)); # sub-char
      pushSTACK(STACK_(1+2)); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: no number allowed between #"" and $")
            );
    }

# UP: Überprüft die Abwesenheit eines Infix-Arguments n
# test_no_infix()
# > Stackaufbau: Stream, sub-char, n.
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: &stream
# erhöht STACK um 1
# verändert STACK
  local object* test_no_infix (void);
  local object* test_no_infix()
    { var object* stream_ = test_stream_arg(STACK_2);
      var object n = popSTACK();
      if ((!nullp(n)) && (!test_value(S(read_suppress))))
        # Bei n/=NIL und *READ-SUPPRESS*=NIL : Fehler melden
        { fehler_dispatch_zahl(); }
      return stream_;
    }

# (set-dispatch-macro-character #\# #\'
#   #'(lambda (stream sub-char n)
#       (when n (error ...))
#       (list 'FUNCTION (read stream t nil t))
# )   )
LISPFUNN(function_reader,3) # liest #'
  { var object* stream_ = test_no_infix(); # n muss NIL sein
    STACK_0 = S(function); return_Values list2_reader(stream_);
  }

# (set-dispatch-macro-character #\# #\|
#   #'(lambda (stream sub-char n) ; mit (not (eql sub-char #\#))
#       (when n (error ...))
#       (prog ((depth 0) ch)
#         1
#         (setq ch (read-char))
#         2
#         (case ch
#           (eof-code (error ...))
#           (sub-char (case (setq ch (read-char))
#                       (eof-code (error ...))
#                       (#\# (when (minusp (decf depth)) (return)))
#                       (t (go 2))
#           )         )
#           (#\# (case (setq ch (read-char))
#                  (eof-code (error ...))
#                  (sub-char (incf depth) (go 1))
#                  (t (go 2))
#           )    )
#           (t (go 1))
#       ) )
#       (values)
# )   )
LISPFUNN(comment_reader,3) # liest #|
  { var object* stream_ = test_no_infix(); # n muss NIL sein
    var uintL depth = 0;
    var object ch;
    loop1:
      ch = read_char(stream_);
    loop2:
      if (eq(ch,eof_value)) goto fehler_eof; # EOF -> Error
      elif (eq(ch,STACK_0))
        # sub-char gelesen
        { ch = read_char(stream_); # nächstes Zeichen
          if (eq(ch,eof_value)) goto fehler_eof; # EOF -> Error
          elif (eq(ch,ascii_char('#')))
            # sub-char und '#' gelesen -> depth erniedrigen:
            { if (depth==0) goto fertig;
              depth--; goto loop1;
            }
          else
            goto loop2;
        }
      elif (eq(ch,ascii_char('#')))
        # '#' gelesen
        { ch = read_char(stream_); # nächstes Zeichen
          if (eq(ch,eof_value)) goto fehler_eof; # EOF -> Error
          elif (eq(ch,STACK_0))
            # '#' und sub-char gelesen -> depth erhöhen:
            { depth++; goto loop1; }
          else
            goto loop2;
        }
      else goto loop1;
    fehler_eof:
      pushSTACK(STACK_1); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(STACK_(0+1)); # sub-char
      pushSTACK(STACK_(0+2)); # sub-char
      pushSTACK(STACK_(1+3)); # Stream
      pushSTACK(S(read));
      fehler(end_of_file,
             GETTEXT("~: input stream ~ ends within a comment #$ ... $#")
            );
    fertig:
      value1 = NIL; mv_count=0; skipSTACK(2); # keine Werte zurück
  }

# (set-dispatch-macro-character #\# #\\
#   #'(lambda (stream sub-char n)
#       (let ((token (read-token-1 stream #\\ 'single-escape)))
#         ; token ist ein String der Länge >=1
#         (unless *read-suppress*
#           (if n
#             (unless (< n char-font-limit) ; sowieso n>=0
#               (error "~ von ~: Fontnummer ~ für Zeichen ist zu groß (muss <~ sein)."
#                       'read stream        n                 char-font-limit
#             ) )
#             (setq n 0)
#           )
#           (let ((pos 0) (bits 0))
#             (loop
#               (if (= (+ pos 1) (length token))
#                 (return (make-char (char token pos) bits n))
#                 (let ((hyphen (position #\- token :start pos)))
#                   (if hyphen
#                     (flet ((equalx (name)
#                              (or (string-equal token name :start1 pos :end1 hyphen)
#                                  (string-equal token name :start1 pos :end1 hyphen :end2 1)
#                           )) )
#                       (cond ((equalx "CONTROL")
#                              (setq bits (logior bits char-control-bit)))
#                             ((equalx "META")
#                              (setq bits (logior bits char-meta-bit)))
#                             ((equalx "SUPER")
#                              (setq bits (logior bits char-super-bit)))
#                             ((equalx "HYPER")
#                              (setq bits (logior bits char-hyper-bit)))
#                             (t (error "~ von ~: Ein Character-Bit mit Namen ~ gibt es nicht."
#                                        'read stream (subseq token pos hyphen)
#                       )     )  )
#                       (setq pos (1+ hyphen))
#                     )
#                     (return
#                       (make-char
#                         (cond ((and (< (+ pos 4) (length token))
#                                     (string-equal token "CODE" :start1 pos :end1 (+ pos 4))
#                                )
#                                (code-char (parse-integer token :start (+ pos 4) :junk-allowed nil)) ; ohne Vorzeichen!
#                               )
#                               ((and (= (+ pos 2) (length token))
#                                     (eql (char token pos) #\^)
#                                     (<= 64 (char-code (char token (+ pos 1))) 95)
#                                )
#                                (code-char (- (char-code (char token (+ pos 1))) 64))
#                               )
#                               ((name-char (subseq token pos)))
#                               (t (error "~ von ~: Ein Character mit Namen ~ gibt es nicht."
#                                          'read stream (subseq token pos)
#                         )     )  )
#                         bits n
#                     ) )
#             ) ) ) )
# )   ) ) ) )
LISPFUNN(char_reader,3) # liest #\
  { # Stackaufbau: Stream, sub-char, n.
    var object* stream_ = test_stream_arg(STACK_2);
    # Token lesen, mit Dummy-Character '\' als Token-Anfang:
    read_token_1(stream_,ascii_char('\\'),syntax_single_esc);
    # bei *READ-SUPPRESS* /= NIL sofort fertig:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=1; skipSTACK(3); return; } # NIL als Wert
    case_convert_token_1();
    # Font bestimmen:
    if (!nullp(STACK_0)) # n=NIL -> Default-Font 0
      if (!eq(STACK_0,Fixnum_0))
        { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
          pushSTACK(STACK_(0+1)); # n
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: font number ~ for character is too large, should be = 0")
                );
        }
    # Font fertig.
    { var object token = O(token_buff_1); # gelesenes Token als Semi-Simple-String
      var uintL len = TheIarray(token)->dims[1]; # Länge = Fill-Pointer
      var object hstring = O(displaced_string); # Hilfsstring
      TheIarray(hstring)->data = token; # Datenvektor := O(token_buff_1)
      token = TheIarray(token)->data; # Normal-Simple-String mit Token
     {var uintL pos = 0; # momentane Position im Token
      loop # Suche nächstes Hyphen
        { if (len-pos == 1) break; # einbuchstabiger Charactername?
          { var uintL hyphen = pos; # hyphen := pos
            loop
              { if (hyphen == len) goto no_more_hyphen; # schon Token-Ende?
                if (chareq(TheSstring(token)->data[hyphen],ascii('-'))) break; # Hyphen gefunden?
                hyphen++; # nein -> weitersuchen
              }
            # Hyphen bei Position hyphen gefunden
           {var uintL sub_len = hyphen-pos;
            TheIarray(hstring)->dims[0] = pos; # Displaced-Offset := pos
            TheIarray(hstring)->totalsize =
              TheIarray(hstring)->dims[1] = sub_len; # Länge := hyphen-pos
            # Jetzt ist hstring = (subseq token pos hyphen)
            # Displaced-String hstring ist kein Bitname -> Error
            { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
              pushSTACK(copy_string(hstring)); # Displaced-String kopieren
              pushSTACK(*stream_); # Stream
              pushSTACK(S(read));
              fehler(stream_error,
                     GETTEXT("~ from ~: there is no character bit with name ~")
                    );
            }
            bit_ok: # Bitname gefunden, Bit gesetzt
            # Mit diesem Bitnamen fertig.
            pos = hyphen+1; # zum nächsten
        } }}
      # einbuchstabiger Charactername
      {var chart code = TheSstring(token)->data[pos]; # (char token pos)
       value1 = code_char(code); mv_count=1; skipSTACK(3); return;
      }
      no_more_hyphen: # kein weiteres Hyphen gefunden.
      {var uintL sub_len = len-pos; # Länge des Characternamens
       TheIarray(hstring)->dims[0] = pos; # Displaced-Offset := pos
       /* TheIarray(hstring)->totalsize =          */
       /*   TheIarray(hstring)->dims[1] = sub_len; */ # Länge := len-pos
       # hstring = (subseq token pos hyphen) = restlicher Charactername
       # Test auf Characternamen "CODExxxx" (xxxx Dezimalzahl <256):
       if (sub_len > 4)
         { TheIarray(hstring)->totalsize =
             TheIarray(hstring)->dims[1] = 4;
           # hstring = (subseq token pos (+ pos 4))
           if (!string_equal(hstring,O(charname_prefix))) # = "Code" ?
             goto not_codexxxx; # nein -> weiter
           # Dezimalzahl entziffern:
          {var uintL code = 0; # bisher gelesenes xxxx (<char_code_limit)
           var uintL index = pos+4;
           var const chart* charptr = &TheSstring(token)->data[index];
           loop
             { if (index == len) break; # Token-Ende erreicht?
              {var cint c = as_cint(*charptr++); # nächstes Character
               # soll Ziffer sein:
               if (!((c>='0') && (c<='9'))) goto not_codexxxx;
               code = 10*code + (c-'0'); # Ziffer dazunehmen
               # code soll < char_code_limit bleiben:
               if (code >= char_code_limit) goto not_codexxxx;
               index++;
             }}
           # Charactername war vom Typ "Codexxxx" mit code = xxxx < char_code_limit
           value1 = code_char(as_chart(code)); mv_count=1; skipSTACK(3); return;
         }}
       not_codexxxx:
       # Test auf Pseudo-Character-Namen ^X:
       if ((sub_len == 2) && chareq(TheSstring(token)->data[pos],ascii('^')))
         { var cint code = as_cint(TheSstring(token)->data[pos+1])-64;
           if (code < 32)
             { value1 = ascii_char(code); mv_count=1; skipSTACK(3); return; }
         }
       # Test auf Characternamen wie NAME-CHAR:
       TheIarray(hstring)->totalsize =
         TheIarray(hstring)->dims[1] = sub_len; # Länge := len-pos
       {var object ch = name_char(hstring); # Character mit diesem Namen suchen
        if (nullp(ch))
          # nicht gefunden -> Error
          { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(copy_string(hstring)); # Charactername kopieren
            pushSTACK(*stream_); # Stream
            pushSTACK(S(read));
            fehler(stream_error,
                   GETTEXT("~ from ~: there is no character with name ~")
                  );
          }
        # gefunden
        value1 = ch; mv_count=1; skipSTACK(3); return;
      }}
  } }}

# (defun radix-1 (stream sub-char n base)
#   (let ((token (read-token stream)))
#     (unless *read-suppress*
#       (when n (error ...))
#       (if (case (test-number-syntax token base)
#             (integer t) (decimal-integer nil) (rational t) (float nil)
#           )
#         (read-number token base)
#         (error "~ von ~: Das Token ~ nach # ~ lässt sich nicht als rationale Zahl in Basis ~ interpretieren."
#                 'read stream token sub-char base
# ) ) ) ) )
  # UP für #B #O #X #R
  # radix_2(base)
  # > base: Basis (>=2, <=36)
  # > Stackaufbau: Stream, sub-char, base.
  # > O(token_buff_1), O(token_buff_2), token_escape_flag: gelesenes Token
  # < STACK: aufgeräumt
  # < mv_space/mv_count: Werte
  # kann GC auslösen
  local Values radix_2 (uintWL base);
  local Values radix_2(base)
    var uintWL base;
    { # Überprüfe, ob das Token eine rationale Zahl darstellt:
      var object string;
      var zahl_info info;
      upcase_token(); # in Großbuchstaben umwandeln
      switch (test_number_syntax(&base,&string,&info))
        { case 1: # Integer
            # letztes Character ein Punkt?
            if (chareq(TheSstring(string)->data[info.index2-1],ascii('.')))
              # ja -> Dezimal-Integer, nicht in Basis base
              goto not_rational;
            # test_number_syntax wurde bereits im Schritt 3 fertig,
            # also ist base immer noch unverändert.
            skipSTACK(3);
            value1 = read_integer(base,info.sign,string,info.index1,info.index2);
            mv_count=1; return;
          case 2: # Rational
            # test_number_syntax wurde bereits im Schritt 3 fertig,
            # also ist base immer noch unverändert.
            skipSTACK(3);
            value1 = read_rational(base,info.sign,string,info.index1,info.index3,info.index2);
            mv_count=1; return;
          case 0: # keine Zahl
          case 3: # Float
          not_rational: # keine rationale Zahl
            pushSTACK(STACK_2); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(STACK_(0+1)); # base
            pushSTACK(STACK_(1+2)); # sub-char
            pushSTACK(copy_string(O(token_buff_1))); # Token
            pushSTACK(STACK_(2+4)); # Stream
            pushSTACK(S(read));
            fehler(stream_error,
                   GETTEXT("~ from ~: token ~ after #$ is not a rational number in base ~")
                  );
          default: NOTREACHED
        }
    }
  # UP für #B #O #X
  # radix_1(base)
  # > base: Basis (>=2, <=36)
  # > Stackaufbau: Stream, sub-char, n.
  # > subr_self: Aufrufer (ein SUBR)
  # < STACK: aufgeräumt
  # < mv_space/mv_count: Werte
  # kann GC auslösen
  local Values radix_1 (uintWL base);
  local Values radix_1(base)
    var uintWL base;
    { var object* stream_ = test_stream_arg(STACK_2);
      read_token(stream_); # Token lesen
      # bei *READ-SUPPRESS* /= NIL sofort fertig:
      if (test_value(S(read_suppress)))
        { value1 = NIL; mv_count=1; skipSTACK(3); return; } # NIL als Wert
      if (!nullp(popSTACK())) { fehler_dispatch_zahl(); } # n/=NIL -> Error
      pushSTACK(fixnum(base)); # base als Fixnum
      return_Values radix_2(base);
    }

# (set-dispatch-macro-character #\# #\B
#   #'(lambda (stream sub-char n) (radix-1 stream sub-char n 2))
# )
LISPFUNN(binary_reader,3) # liest #B
  { return_Values radix_1(2); }

# (set-dispatch-macro-character #\# #\O
#   #'(lambda (stream sub-char n) (radix-1 stream sub-char n 8))
# )
LISPFUNN(octal_reader,3) # liest #O
  { return_Values radix_1(8); }

# (set-dispatch-macro-character #\# #\X
#   #'(lambda (stream sub-char n) (radix-1 stream sub-char n 16))
# )
LISPFUNN(hexadecimal_reader,3) # liest #X
  { return_Values radix_1(16); }

# (set-dispatch-macro-character #\# #\R
#   #'(lambda (stream sub-char n)
#       (if *read-suppress*
#         (if (and n (<= 2 n 36))
#           (radix-1 stream sub-char nil n)
#           (error "~ von ~: Zwischen # und R muss eine Zahlsystembasis zwischen 2 und 36 angegeben werden."
#                   'read stream
#         ) )
#         (progn (read-token stream) nil)
# )   ) )
LISPFUNN(radix_reader,3) # liest #R
  { var object* stream_ = test_stream_arg(STACK_2);
    read_token(stream_); # Token lesen
    # bei *READ-SUPPRESS* /= NIL sofort fertig:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=1; skipSTACK(3); return; } # NIL als Wert
    # n überprüfen:
    if (nullp(STACK_0))
      { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: the number base must be given between #"" and R")
              );
      }
   {var uintL base;
    # n muss ein Fixnum zwischen 2 und 36 (inclusive) sein:
    if (posfixnump(STACK_0) &&
        (base = posfixnum_to_L(STACK_0), (base >= 2) && (base <= 36))
       )
      { return_Values radix_2(base); } # Token als rationale Zahl interpretieren
      else
      { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: The base ~ given between #"" and R should lie between 2 and 36")
              );
      }
  }}

# (set-dispatch-macro-character #\# #\C
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (if *read-suppress*
#         (progn (read stream t nil t) nil)
#         (if n
#           (error "~: Zwischen # und C ist keine Zahl erlaubt." 'read)
#           (let ((h (read stream t nil t)))
#             (if (and (consp h) (consp (cdr h)) (null (cddr h))
#                      (numberp (first h)) (not (complexp (first h)))
#                      (numberp (second h)) (not (complexp (second h)))
#                 )
#               (apply #'complex h)
#               (error "~: Falsche Syntax für komplexe Zahl: #C~" 'read h)
# )   ) ) ) ) )
LISPFUNN(complex_reader,3) # liest #C
  { var object* stream_ = test_no_infix(); # n muss NIL sein
    var object obj = read_recursive_no_dot(stream_); # nächstes Objekt lesen
    # bei *READ-SUPPRESS* /= NIL sofort fertig:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=1; skipSTACK(2); return; } # NIL als Wert
    obj = make_references(obj); # und Verweise vorzeitig entflechten
    # Überprüfen, ob dies eine zweielementige Liste von reellen Zahlen ist:
    if (!consp(obj)) goto bad; # obj muss ein Cons sein !
   {var object obj2 = Cdr(obj);
    if (!consp(obj2)) goto bad; # obj2 muss ein Cons sein !
    if (!nullp(Cdr(obj2))) goto bad; # mit (cdr obj2) = nil !
    if_realp(Car(obj), ; , goto bad; ); # und (car obj) eine reelle Zahl !
    if_realp(Car(obj2), ; , goto bad; ); # und (car obj2) eine reelle Zahl !
    # (apply #'COMPLEX obj) durchführen:
    apply(L(complex),0,obj);
    mv_count=1; skipSTACK(2); return; # value1 als Wert
   }
   {bad:
      pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(obj); # Objekt
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: bad syntax for complex number: #C~")
            );
  }}

# (set-dispatch-macro-character #\# #\:
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (if *read-suppress*
#         (progn (read stream t nil t) nil)
#         (let ((name (read-token stream))) ; eine Form, die nur ein Token ist
#           (when n (error ...))
#           [Überprüfe, ob auch keine Package-Marker im Token vorkommen.]
#           (make-symbol token)
# )   ) ) )
LISPFUNN(uninterned_reader,3) # liest #:
  { var object* stream_ = test_stream_arg(STACK_2);
    # bei *READ-SUPPRESS* /= NIL Form lesen und NIL liefern:
    if (test_value(S(read_suppress)))
      { read_recursive(stream_);
        value1 = NIL; mv_count=1; skipSTACK(3); return;
      }
    {# nächstes Zeichen lesen:
     var object ch;
     var uintWL scode;
     read_char_syntax(ch = ,scode = ,stream_);
     if (scode == syntax_eof) { fehler_eof_innen(stream_); } # EOF -> Error
     if (scode > syntax_constituent)
       # kein Zeichen, das am Token-Anfang stehen kann -> Error
       { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
         pushSTACK(*stream_); # Stream
         pushSTACK(S(read));
         fehler(stream_error,
                GETTEXT("~ from ~: token expected after #:")
               );
       }
     # Token zu Ende lesen:
     read_token_1(stream_,ch,scode);
     case_convert_token_1();
    }
    if (!nullp(popSTACK())) { fehler_dispatch_zahl(); } # n/=NIL -> Error
    {# Token kopieren und dabei in Simple-String umwandeln:
     var object string = coerce_imm_ss(O(token_buff_1));
     # Auf Package-Marker testen:
     {var object buff_2 = O(token_buff_2); # Attributcode-Buffer
      var uintL len = TheIarray(buff_2)->dims[1]/8; # Länge = Fill-Pointer
      if (len > 0)
        { var uintB* attrptr = &TheSbvector(TheIarray(buff_2)->data)->data[0];
          # Teste, ob einer der len Attributcodes ab attrptr ein a_pack_m ist:
          dotimespL(len,len, { if (*attrptr++ == a_pack_m) goto fehler_dopp; } );
     }  }
     # uninterniertes Symbol mit diesem Namen bauen:
     value1 = make_symbol(string); mv_count=1; skipSTACK(2); return;
     fehler_dopp:
       pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
       pushSTACK(string); # Token
       pushSTACK(*stream_); # Stream
       pushSTACK(S(read));
       fehler(stream_error,
              GETTEXT("~ from ~: token ~ after #: should contain no colon")
             );
  } }

# (set-dispatch-macro-character #\# #\*
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (let* ((token (read-token stream)))
#         (unless *read-suppress*
#           (unless (or [Escape-Zeichen im Token verwendet]
#                       (every #'(lambda (ch) (member ch '(#\0 #\1))) token))
#             (error "~ von ~: Nach #* dürfen nur Nullen und Einsen kommen."
#                     'read stream
#           ) )
#           (let ((l (length token)))
#             (if n
#               (cond ((< n l)
#                      (error "~ von ~: Bit-Vektor länger als angegebene Länge ~."
#                              'read stream n
#                     ))
#                     ((and (plusp n) (zerop l))
#                      (error "~ von ~: Element für Bit-Vektor der Länge ~ muss spezifiziert werden."
#                              'read stream n
#               )     ))
#               (setq n l)
#             )
#             (let ((bv (make-array n :element-type 'bit))
#                   (i 0)
#                   b)
#               (loop
#                 (when (= i n) (return))
#                 (when (< i l) (setq b (case (char token i) (#\0 0) (#\1 1))))
#                 (setf (sbit bv i) b)
#                 (incf i)
#               )
#               bv
# )   ) ) ) ) )
LISPFUNN(bit_vector_reader,3) # liest #*
  { var object* stream_ = test_stream_arg(STACK_2);
    read_token(stream_); # Token lesen
    # bei *READ-SUPPRESS* /= NIL sofort fertig:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=1; skipSTACK(3); return; } # NIL als Wert
    # Test, ob kein Escape-Zeichen und nur Nullen und Einsen verwendet:
    if (token_escape_flag)
      { fehler_nur01:
        pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: only zeroes and ones are allowed after #*")
              );
      }
   {var object buff_1 = O(token_buff_1); # Character-Buffer
    var uintL len = TheIarray(buff_1)->dims[1]; # Länge = Fill-Pointer
    if (len > 0)
      { var const chart* charptr = &TheSstring(TheIarray(buff_1)->data)->data[0];
        var uintL count;
        dotimespL(count,len,
          { var chart c = *charptr++; # nächstes Character
            if (!(chareq(c,ascii('0')) || chareq(c,ascii('1')))) # nur '0' und '1' sind OK
              goto fehler_nur01;
          });
      }
    # n überprüfen:
    {var uintL n; # Länge des Bitvektors
     if (nullp(STACK_0))
       { n = len; } # Defaultwert ist die Tokenlänge
       else
       { # n angegeben, ein Integer >=0.
         n = (posfixnump(STACK_0) ? posfixnum_to_L(STACK_0) # Fixnum -> Wert
                                  : bitm(oint_data_len)-1 # Bignum -> großer Wert
             );
         if (n<len)
           { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
             pushSTACK(STACK_(0+1)); # n
             pushSTACK(*stream_); # Stream
             pushSTACK(S(read));
             fehler(stream_error,
                    GETTEXT("~ from ~: bit vector is longer than the explicitly given length ~")
                   );
           }
         if ((n>0) && (len==0))
           { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
             pushSTACK(STACK_(0+1)); # n
             pushSTACK(*stream_); # Stream
             pushSTACK(S(read));
             fehler(stream_error,
                    GETTEXT("~ from ~: must specify element of bit vector of length ~")
                   );
           }
       }
     # Erzeuge neuen Bit-Vektor der Länge n:
     {var object bv = allocate_bit_vector(n);
      # und fülle die Bits ein:
      buff_1 = O(token_buff_1);
      { var const chart* charptr = &TheSstring(TheIarray(buff_1)->data)->data[0];
        var chart ch; # letztes Zeichen ('0' oder '1')
        var uintL index = 0;
        while (index < n)
          { if (index < len) { ch = *charptr++; } # evtl. nächstes Character holen
            if (chareq(ch,ascii('0')))
              { sbvector_bclr(bv,index); } # Null -> Bit löschen
              else
              { sbvector_bset(bv,index); } # Eins -> Bit setzen
            index++;
          }
      }
      value1 = bv; mv_count=1; skipSTACK(3); # bv als Wert
  }}}}

# (set-dispatch-macro-character #\# #\(
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (let* ((elements (read-delimited-list #\) stream t)))
#         (unless *read-suppress*
#           (let ((l (length elements)))
#             (if n
#               (cond ((< n l)
#                      (error "~ von ~: Vektor länger als angegebene Länge ~."
#                              'read stream n
#                     ))
#                     ((and (plusp n) (zerop l))
#                      (error "~ von ~: Element für Vektor der Länge ~ muss spezifiziert werden."
#                              'read stream n
#               )     ))
#               (setq n l)
#             )
#             (let ((v (make-array n))
#                   (i 0)
#                   b)
#               (loop
#                 (when (= i n) (return))
#                 (when (< i l) (setq b (pop elements)))
#                 (setf (svref v i) b)
#                 (incf i)
#               )
#               v
# )   ) ) ) ) )
LISPFUNN(vector_reader,3) # liest #(
  { var object* stream_ = test_stream_arg(STACK_2);
    # Liste bis zur Klammer zu lesen, Dot nicht erlaubt:
    var object elements = read_delimited_list(stream_,ascii_char(')'),eof_value);
    # bei *READ-SUPPRESS* /= NIL sofort fertig:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=1; skipSTACK(3); return; } # NIL als Wert
   {var uintL len = llength(elements); # Listenlänge
    # n überprüfen:
    var uintL n; # Länge des Vektors
    if (nullp(STACK_0))
      { n = len; } # Defaultwert ist die Tokenlänge
      else
      { # n angegeben, ein Integer >=0.
        n = (posfixnump(STACK_0) ? posfixnum_to_L(STACK_0) # Fixnum -> Wert
                                 : bitm(oint_data_len)-1 # Bignum -> großer Wert
            );
        if (n<len)
          { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(STACK_(0+1)); # n
            pushSTACK(*stream_); # Stream
            pushSTACK(S(read));
            fehler(stream_error,
                   GETTEXT("~ from ~: vector is longer than the explicitly given length ~")
                  );
          }
        if ((n>0) && (len==0))
          { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(STACK_(0+1)); # n
            pushSTACK(*stream_); # Stream
            pushSTACK(S(read));
            fehler(stream_error,
                   GETTEXT("~ from ~: must specify element of vector of length ~")
                  );
          }
      }
    # Erzeuge neuen Vektor der Länge n:
    pushSTACK(elements); # Liste retten
    {var object v = allocate_vector(n);
     elements = popSTACK(); # Liste zurück
     # und fülle die Elemente ein:
     { var object* vptr = &TheSvector(v)->data[0];
       var object el; # letztes Element
       var uintL index = 0;
       while (index < n)
         { if (index < len) { el = Car(elements); elements = Cdr(elements); } # evtl. nächstes Element holen
           *vptr++ = el;
           index++;
         }
     }
     value1 = v; mv_count=1; skipSTACK(3); # v als Wert
  }}}

# (set-dispatch-macro-character #\# #\A
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (if *read-suppress*
#         (progn (read stream t nil t) nil)
#         (if (null n)
#           (let ((h (read stream t nil t)))
#             (if (and (consp h) (consp (cdr h)) (consp (cddr h)) (null (cdddr h)))
#               (make-array (second h) :element-type (first h) :initial-contents (third h))
#               (error "~: Falsche Syntax für Array: #A~" 'read h)
#           ) )
#           (let* ((rank n)
#                  (cont (let ((*backquote-level* nil)) (read stream t nil t)))
#                  (dims '())
#                  (eltype 't))
#             (when (plusp rank)
#               (let ((subcont cont) (i 0))
#                 (loop
#                   (let ((l (length subcont)))
#                     (push l dims)
#                     (incf i) (when (>= i rank) (return))
#                     (when (plusp l) (setq subcont (elt subcont 0)))
#                 ) )
#                 (cond ((stringp subcont) (setq eltype 'character))
#                       ((bit-vector-p subcont) (setq eltype 'bit))
#             ) ) )
#             (make-array (nreverse dims) :element-type eltype :initial-contents cont)
# )   ) ) ) )
LISPFUNN(array_reader,3) # liest #A
  { var object* stream_ = test_stream_arg(STACK_2);
    # Stackaufbau: stream, sub-char, n.
    if (test_value(S(read_suppress))) # *READ-SUPPRESS* /= NIL ?
      # ja -> nächstes Objekt überlesen:
      { read_recursive_no_dot(stream_);
        value1 = NIL; mv_count=1; skipSTACK(3); return;
      }
    if (nullp(STACK_0)) # n nicht angegeben?
      # ja -> Liste (eltype dims contents) lesen:
      { var object obj = read_recursive_no_dot(stream_); # Liste lesen
        obj = make_references(obj); # Verweise entflechten
        # (Das ist ungefährlich, da wir diese #A-Syntax für Arrays mit
        # Elementtyp T nicht benutzen, und Byte-Arrays enthalten keine Verweise.)
        if (!consp(obj)) goto bad;
        { var object obj2 = Cdr(obj);
          if (!consp(obj2)) goto bad;
         {var object obj3 = Cdr(obj2);
          if (!consp(obj3)) goto bad;
          if (!nullp(Cdr(obj3))) goto bad;
          # (MAKE-ARRAY dims :element-type eltype :initial-contents contents) aufrufen:
          STACK_2 = Car(obj2); STACK_1 = S(Kelement_type); STACK_0 = Car(obj);
          pushSTACK(S(Kinitial_contents)); pushSTACK(Car(obj3));
          goto call_make_array;
        }}
        bad:
          pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
          pushSTACK(obj); # Objekt
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: bad syntax for array: #A~")
                );
      }
    # n gibt den Rang des Arrays an.
    # Inhalt lesen:
    { dynamic_bind(S(backquote_level),NIL); # SYS::*BACKQUOTE-LEVEL* an NIL binden
     {var object contents = read_recursive_no_dot(stream_);
      dynamic_unbind();
      pushSTACK(contents); pushSTACK(contents);
    }}
    STACK_4 = NIL; # dims := '()
    # Stackaufbau: dims, -, rank, subcontents, contents.
    # Dimensionen und Elementtyp bestimmen:
    if (eq(STACK_2,Fixnum_0)) # rank=0 ?
      { STACK_2 = S(t); } # ja -> eltype := 'T
      else
      { var object i = Fixnum_0; # bisherige Verschachtelungstiefe
        loop
          { pushSTACK(STACK_1); funcall(L(length),1); # (LENGTH subcontents)
            # auf dims pushen:
            STACK_3 = value1;
            {var object new_cons = allocate_cons();
             Car(new_cons) = STACK_3; Cdr(new_cons) = STACK_4;
             STACK_4 = new_cons;
            }
            # Tiefe erhöhen:
            i = fixnum_inc(i,1); if (eql(i,STACK_2)) break;
            # erstes Element von subcontents für die weiteren Dimensionen:
            if (!eq(STACK_3,Fixnum_0)) # (nur falls (length subcontents) >0)
              { pushSTACK(STACK_1); pushSTACK(Fixnum_0); funcall(L(elt),2);
                STACK_1 = value1; # subcontents := (ELT subcontents 0)
              }
          }
        nreverse(STACK_4); # Liste dims umdrehen
        # eltype bestimmen je nach innerstem subcontents:
        STACK_2 = (stringp(STACK_1) ? S(character) : # String: CHARACTER
                   bit_vector_p(STACK_1) ? S(bit) :  # Bitvektor: BIT
                   S(t)                              # sonst (Liste): T
                  );
      }
    # Stackaufbau: dims, -, eltype, -, contents.
    # MAKE-ARRAY aufrufen:
    STACK_3 = S(Kelement_type); STACK_1 = S(Kinitial_contents);
    call_make_array:
    funcall(L(make_array),5);
    mv_count=1; return;
  }

# Fehlermeldung für #. und #, wegen *READ-EVAL*.
# fehler_read_eval_forbidden(&stream,obj);
# > stream: Stream
# > obj: Objekt, dessen Evaluierung versucht wurde
  nonreturning_function(local, fehler_read_eval_forbidden, (object* stream_, object obj));
  local void fehler_read_eval_forbidden(stream_,obj)
    var object* stream_;
    var object obj;
    { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(obj); # Objekt
      pushSTACK(NIL); # NIL
      pushSTACK(S(read_eval)); # *READ-EVAL*
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: ~ = ~ doesn't allow the evaluation of ~")
            );
    }

# (set-dispatch-macro-character #\# #\.
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (let ((h (read stream t nil t)))
#         (unless *read-suppress*
#           (if n
#             (error "~ von ~: Zwischen # und . ist keine Zahl erlaubt."
#                     'read stream
#             )
#             (eval h)
# )   ) ) ) )
LISPFUNN(read_eval_reader,3) # liest #.
  { var object* stream_ = test_stream_arg(STACK_2);
    var object obj = read_recursive_no_dot(stream_); # Form lesen
    # bei *READ-SUPPRESS* /= NIL sofort fertig:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=1; skipSTACK(3); return; }
    if (!nullp(popSTACK())) { fehler_dispatch_zahl(); } # n/=NIL -> Error
    obj = make_references(obj); # Verweise entflechten
    # Entweder *READ-EVAL* oder der Stream muss die Evaluierung erlauben.
    if (!(test_value(S(read_eval)) || stream_get_read_eval(*stream_)))
      { fehler_read_eval_forbidden(stream_,obj); }
    eval_noenv(obj); # Form auswerten
    mv_count=1; skipSTACK(2); # nur 1 Wert zurück
  }

# (set-dispatch-macro-character #\# #\,
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (let ((h (read stream t nil t)))
#         (unless *read-suppress*
#           (if n
#             (error "~ von ~: Zwischen # und , ist keine Zahl erlaubt."
#                     'read stream
#             )
#             (if sys::*compiling* (make-load-time-eval h) (eval h))
# )   ) ) ) )
LISPFUNN(load_eval_reader,3) # liest #,
  { var object* stream_ = test_stream_arg(STACK_2);
    var object obj = read_recursive_no_dot(stream_); # Form lesen
    # bei *READ-SUPPRESS* /= NIL sofort fertig:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=1; skipSTACK(3); return; }
    if (!nullp(popSTACK())) { fehler_dispatch_zahl(); } # n/=NIL -> Error
    obj = make_references(obj); # Verweise entflechten
    if (test_value(S(compiling)))
      # Im Compiler:
      { pushSTACK(obj);
       {var object newobj = allocate_loadtimeeval(); # Load-time-Eval-Objekt
        TheLoadtimeeval(newobj)->loadtimeeval_form = popSTACK(); # mit obj als Form
        value1 = newobj;
      }}
      else
      # Im Interpreter:
      { # Entweder *READ-EVAL* oder der Stream muss die Evaluierung erlauben.
        if (!(test_value(S(read_eval)) || stream_get_read_eval(*stream_)))
          { fehler_read_eval_forbidden(stream_,obj); }
        eval_noenv(obj); # Form auswerten
      }
    mv_count=1; skipSTACK(2); # nur 1 Wert zurück
  }

# (set-dispatch-macro-character #\# #\=
#   #'(lambda (stream sub-char n)
#       (if *read-suppress*
#         (if n
#           (if (sys::fixnump n)
#             (let* ((label (make-internal-label n))
#                    (h (assoc label sys::*read-reference-table* :test #'eq)))
#               (if (consp h)
#                 (error "~ von ~: Label #~= darf nicht zweimal definiert werden." 'read stream n)
#                 (progn
#                   (push (setq h (cons label label)) sys::*read-reference-table*)
#                   (let ((obj (read stream t nil t)))
#                     (if (eq obj label)
#                       (error "~ von ~: #~= #~# ist nicht erlaubt." 'read stream n n)
#                       (setf (cdr h) obj)
#             ) ) ) ) )
#             (error "~ von ~: Label #~= zu groß" 'read stream n)
#           )
#           (error "~ von ~: Zwischen # und = muss eine Zahl angegeben werden." 'read stream)
#         )
#         (values) ; keine Werte (Kommentar)
# )   ) )

# (set-dispatch-macro-character #\# #\#
#   #'(lambda (stream sub-char n)
#       (unless *read-suppress*
#         (if n
#           (if (sys::fixnump n)
#             (let* ((label (make-internal-label n))
#                    (h (assoc label sys::*read-reference-table* :test #'eq)))
#               (if (consp h)
#                 label ; wird später entflochten
#                 ; (man könnte auch (cdr h) zurückliefern)
#                 (error "~ von ~: Label #~= ist nicht definiert." 'read stream n)
#               )
#             (error "~ von ~: Label #~# zu groß" 'read stream n)
#           )
#           (error "~ von ~: Zwischen # und # muss eine Zahl angegeben werden." 'read stream)
# )   ) ) )

# UP: Bildet ein internes Label und sucht es in der *READ-REFERENCE-TABLE* auf.
# lookup_label()
# > Stackaufbau: Stream, sub-char, n.
# < ergebnis: (or (assoc label sys::*read-reference-table* :test #'eq) label)
  local object lookup_label (void);
  local object lookup_label()
    { var object n = STACK_0;
      if (nullp(n)) # nicht angegeben?
        { pushSTACK(STACK_2); # Wert für Slot STREAM von STREAM-ERROR
          pushSTACK(STACK_(1+1)); # sub-char
          pushSTACK(STACK_(2+2)); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: a number must be given between #"" and $")
                );
        }
      # n ist ein Integer >=0
      if (!read_label_integer_p(n))
        # n ist zu groß
        { pushSTACK(STACK_2); # Wert für Slot STREAM von STREAM-ERROR
          pushSTACK(STACK_(1+1)); # sub-char
          pushSTACK(STACK_(0+2)); # n
          pushSTACK(STACK_(2+3)); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: label #~? too large")
                );
        }
     {var object label = make_read_label(posfixnum_to_L(n)); # Internal-Label mit Nummer n
      var object alist = # Wert von SYS::*READ-REFERENCE-TABLE*
        Symbol_value(S(read_reference_table));
      # (assoc label alist :test #'eq) ausführen:
      while (consp(alist))
        { var object acons = Car(alist); # Listenelement
          if (!consp(acons)) goto bad_reftab; # muss ein Cons sein !
          if (eq(Car(acons),label)) # dessen CAR = label ?
            { return acons; } # ja -> fertig
          alist = Cdr(alist);
        }
      if (nullp(alist)) # Listenende mit NIL ?
        { return label; } # ja -> (assoc ...) = NIL -> fertig mit label
      bad_reftab: # Wert von SYS::*READ-REFERENCE-TABLE* ist keine Aliste
        pushSTACK(Symbol_value(S(read_reference_table))); # Wert von SYS::*READ-REFERENCE-TABLE*
        pushSTACK(S(read_reference_table)); # SYS::*READ-REFERENCE-TABLE*
        pushSTACK(STACK_(2+2)); # Stream
        pushSTACK(S(read));
        fehler(error,
               GETTEXT("~ from ~: the value of ~ has been altered arbitrarily, it is not an alist: ~")
              );
    }}

LISPFUNN(label_definition_reader,3) # liest #=
  { # bei *READ-SUPPRESS* /= NIL wird #n= als Kommentar behandelt:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=0; skipSTACK(3); return; } # keine Werte
    # Label bilden und in der Tabelle aufsuchen:
   {var object lookup = lookup_label();
    if (consp(lookup))
      # gefunden -> war schon da -> Fehler:
      { pushSTACK(STACK_2); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(STACK_(2+2)); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: label #~= may not be defined twice")
              );
      }
      else
      # lookup = label, nicht GC-gefährdet.
      # (push (setq h (cons label label)) sys::*read-reference-table*) :
      {var object* stream_ = test_stream_arg(STACK_2);
       {var object new_cons = allocate_cons();
        Car(new_cons) = Cdr(new_cons) = lookup; # h = (cons label label)
        pushSTACK(new_cons); # h retten
       }
       {var object new_cons = allocate_cons(); # neues Listen-Cons
        Car(new_cons) = STACK_0;
        Cdr(new_cons) = Symbol_value(S(read_reference_table));
        Symbol_value(S(read_reference_table)) = new_cons;
       }
       {var object obj = read_recursive_no_dot(stream_); # Objekt lesen
        var object h = popSTACK();
        if (eq(obj,Car(h))) # gelesenes Objekt = (car h) = label ?
          # ja -> zyklische Definition -> Error
          { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(STACK_(0+1)); # n
            pushSTACK(STACK_(0+2)); # n
            pushSTACK(*stream_); # Stream
            pushSTACK(S(read));
            fehler(stream_error,
                   GETTEXT("~ from ~: #~= #~#"" is illegal")
                  );
          }
        # gelesenes Objekt als (cdr h) eintragen:
        Cdr(h) = obj;
        value1 = obj; mv_count=1; skipSTACK(3); # obj als Wert
      }}
  }}

LISPFUNN(label_reference_reader,3) # liest ##
  { # bei *READ-SUPPRESS* /= NIL sofort fertig:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=1; skipSTACK(3); return; }
    # Label bilden und in der Tabelle aufsuchen:
   {var object lookup = lookup_label();
    if (consp(lookup))
      # gefunden -> Label als gelesenes Objekt zurück:
      { value1 = Car(lookup); mv_count=1; skipSTACK(3); }
      else
      # nicht gefunden
      { pushSTACK(STACK_2); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(STACK_(2+2)); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: undefined label #~#")
              );
      }
  }}

# (set-dispatch-macro-character #\# #\<
#   #'(lambda (stream sub-char n)
#       (error "~ von ~: Als #<...> ausgegebene Objekte sind nicht mehr einlesbar."
#               'read stream
# )   ) )
LISPFUNN(not_readable_reader,3) # liest #<
  { var object* stream_ = test_stream_arg(STACK_2);
    pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
    pushSTACK(*stream_); # Stream
    pushSTACK(S(read));
    fehler(stream_error,
           GETTEXT("~ from ~: objects printed as #<...> cannot be read back in")
          );
  }

# (dolist (ch '(#\) #\Space #\Newline #\Linefeed #\Backspace #\Rubout #\Tab #\Return #\Page))
#   (set-dispatch-macro-character #\# ch
#     #'(lambda (stream sub-char n)
#         (error "~ von ~: Wegen ~ als # ausgegebene Objekte sind nicht mehr einlesbar."
#                 'read stream '*print-level*
# ) )   ) )
LISPFUNN(syntax_error_reader,3) # liest #) und #whitespace
  { var object* stream_ = test_stream_arg(STACK_2);
    pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
    pushSTACK(S(print_level));
    pushSTACK(*stream_); # Stream
    pushSTACK(S(read));
    fehler(stream_error,
           GETTEXT("~ from ~: objects printed as #"" in view of ~ cannot be read back in")
          );
  }

# Hilfsfunktion für #+ und #- :
# (defun interpret-feature (feature)
#   (flet ((eqs (x y) (and (symbolp x) (symbolp y)
#                          (string= (symbol-name x) (symbol-name y))
#         ))          )
#     (cond ((symbolp feature) (member feature *features* :test #'eq))
#           ((atom feature)
#            (error "~: Als Feature ist ~ nicht erlaubt." 'read feature)
#           )
#           ((eqs (car feature) 'and)
#            (every #'interpret-feature (cdr feature))
#           )
#           ((eqs (car feature) 'or)
#            (some #'interpret-feature (cdr feature))
#           )
#           ((eqs (car feature) 'not)
#            (not (interpret-feature (second feature)))
#           )
#           (t (error "~: Als Feature ist ~ nicht erlaubt." 'read feature))
# ) ) )

# UP: Stellt das Erfülltsein eines Feature-Ausdruckes fest.
# interpret_feature(expr)
# > expr: ein Feature-Ausdruck
# > STACK_1: Stream
# < ergebnis: Wahrheitswert: 0 falls erfüllt, ~0 falls nicht erfüllt.
  local uintWL interpret_feature (object expr);
  local uintWL interpret_feature(expr)
    var object expr;
    { check_SP();
      if (symbolp(expr))
        # expr Symbol, in *FEATURES* suchen:
        { var object list = Symbol_value(S(features)); # Wert von *FEATURES*
          while (consp(list))
            { if (eq(Car(list),expr)) goto ja;
              list = Cdr(list);
            }
          goto nein;
        }
      elif (consp(expr) && symbolp(Car(expr)))
        { var object opname = Symbol_name(Car(expr));
          var uintWL and_or_flag;
          if (string_gleich(opname,Symbol_name(S(and))))
            # expr = (AND ...)
            { and_or_flag = 0; goto and_or; }
          elif (string_gleich(opname,Symbol_name(S(or))))
            # expr = (OR ...)
            { and_or_flag = ~0;
              and_or:
              # Listenelemente von expr so lange abinterpretieren, bis ein
              # Ergebnis /=and_or_flag kommt. Default ist and_or_flag.
              { var object list = Cdr(expr);
                while (consp(list))
                  { # Listenelement abinterpretieren:
                    var uintWL sub_erg = interpret_feature(Car(list));
                    if (!(sub_erg == and_or_flag)) { return sub_erg; }
                    list = Cdr(list);
                  }
                if (nullp(list)) { return and_or_flag; }
                # expr war eine Dotted List -> Fehler
            } }
          elif (string_gleich(opname,Symbol_name(S(not))))
            { # expr = (NOT ...) soll die Gestalt (NOT obj) haben:
              var object opargs = Cdr(expr);
              if (consp(opargs) && nullp(Cdr(opargs)))
                { return ~interpret_feature(Car(opargs)); }
              # expr hat keine korrekte Gestalt -> Fehler
            }
          # falscher (car expr) -> Fehler
        }
      bad: # Falscher Aufbau eines Feature-Ausdrucks
        pushSTACK(STACK_1); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(expr); # Feature-Ausdruck
        pushSTACK(STACK_(1+2)); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: illegal feature ~")
              );
      ja: return 0; # expr ist erfüllt
      nein: return ~0; # expr ist nicht erfüllt
    }

# UP für #+ und #-
# feature(sollwert)
# > sollwert: gewünschter Wahrheitswert des Feature-Ausdrucks
# > Stackaufbau: Stream, sub-char, n.
# > subr_self: Aufrufer (ein SUBR)
# < STACK: um 3 erhöht
# < mv_space/mv_count: Werte
# can trigger GC
  local Values feature (uintWL sollwert);
  local Values feature(sollwert)
    var uintWL sollwert;
    { var object* stream_ = test_no_infix(); # n muss NIL sein
      dynamic_bind(S(read_suppress),NIL); # *READ-SUPPRESS* an NIL binden
      dynamic_bind(S(packagestern),O(keyword_package)); # bind *PACKAGE* to #<PACKAGE KEYWORD>
     {var object expr = read_recursive_no_dot(stream_); # Feature-Ausdruck lesen
      dynamic_unbind();
      dynamic_unbind();
      # Feature-Ausdruck interpretieren:
      expr = make_references(expr); # zuvor Verweise entflechten
      if (interpret_feature(expr) == sollwert)
        # Wahrheitswert "wahr"
        { # nächstes Objekt lesen und als Wert:
          value1 = read_recursive_no_dot(stream_); mv_count=1;
        }
        else
        # Wahrheitswert "falsch"
        { # *READ-SUPPRESS* an T binden, Objekt lesen, Kommentar
          dynamic_bind(S(read_suppress),T);
          read_recursive_no_dot(stream_);
          dynamic_unbind();
          value1 = NIL; mv_count=0; # keine Werte
        }
      skipSTACK(2);
    }}

# (set-dispatch-macro-character #\# #\+
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (if n
#         (error "~ von ~: Zwischen # und + darf keine Zahl kommen" 'read stream)
#         (let ((feature (let ((*read-suppress* nil)) (read stream t nil t))))
#           (if (interpret-feature feature)
#             (read stream t nil t)
#             (let ((*read-suppress* t))
#               (read stream t nil t)
#               (values)
# )   ) ) ) ) )
LISPFUNN(feature_reader,3) # liest #+
  { return_Values feature(0); }

# (set-dispatch-macro-character #\# #\-
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (if n
#         (error "~ von ~: Zwischen # und - darf keine Zahl kommen" 'read stream)
#         (let ((feature (let ((*read-suppress* nil)) (read stream t nil t))))
#           (if (interpret-feature feature)
#             (let ((*read-suppress* t))
#               (read stream t nil t)
#               (values)
#             )
#             (read stream t nil t)
# )   ) ) ) )
LISPFUNN(not_feature_reader,3) # liest #-
  { return_Values feature(~0); }

# (set-dispatch-macro-character #\# #\S
#   #'(lambda (stream char n)
#       (declare (ignore char))
#       (if *read-suppress*
#         (progn (read stream t nil t) nil)
#         (if n
#           (error "~: Zwischen # und S ist keine Zahl erlaubt." 'read)
#           (let ((args (let ((*backquote-level* nil)) (read stream t nil t))))
#             (if (consp args)
#               (let ((name (first args)))
#                 (if (symbolp name)
#                   (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
#                     (if desc
#                       (if (svref desc 2)
#                         (values
#                           (apply (svref desc 2) ; der Konstruktor
#                                  (structure-arglist-expand name (cdr args))
#                         ) )
#                         (error "~: Structures des Typs ~ können nicht eingelesen werden (Konstruktorfunktion unbekannt)"
#                                'read name
#                       ) )
#                       (error "~: Es ist noch keine Structure des Typs ~ definiert worden"
#                              'read name
#                   ) ) )
#                   (error "~: Der Typ einer Structure muss ein Symbol sein, nicht ~"
#                          'read name
#               ) ) )
#               (error "~: Nach #S muss, in Klammern, der Typ und der Inhalt der Structure kommen, nicht ~"
#                      'read args
# )   ) ) ) ) ) )
# (defun structure-arglist-expand (name args)
#   (cond ((null args) nil)
#         ((atom args) (error "~: Eine Structure ~ darf keine Komponente . enthalten" 'read name))
#         ((not (symbolp (car args)))
#          (error "~: ~ ist kein Symbol und daher kein Slot der Structure ~" 'read (car args) name)
#         )
#         ((null (cdr args)) (error "~: Wert der Komponente ~ in der Structure ~ fehlt" 'read (car args) name))
#         ((atom (cdr args)) (error "~: Eine Structure ~ darf keine Komponente . enthalten" 'read name))
#         (t (let ((kw (intern (symbol-name (car args)) (find-package "KEYWORD"))))
#              (list* kw (cadr args) (structure-arglist-expand name (cddr args)))
# ) )     )  )
LISPFUNN(structure_reader,3) # liest #S
  { var object* stream_ = test_no_infix(); # n muss NIL sein
    # bei *READ-SUPPRESS* /= NIL nur ein Objekt lesen:
    if (test_value(S(read_suppress)))
      { read_recursive_no_dot(stream_); # Objekt lesen und wegwerfen,
        value1 = NIL; mv_count=1; skipSTACK(2); return; # NIL als Wert
      }
    # SYS::*BACKQUOTE-LEVEL* an NIL binden und Objekt lesen:
    dynamic_bind(S(backquote_level),NIL);
   {var object args = read_recursive_no_dot(stream_);
    dynamic_unbind();
    # gelesene Liste überprüfen:
    if (atomp(args))
      { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(args); # Argumente
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: #S must be followed by the type and the contents of the structure, not ~")
              );
      }
    {var object name = Car(args); # Typ der Structure
     STACK_0 = args = Cdr(args); # Restliste retten
     # Stackaufbau: Stream, restl.Args.
     if (!symbolp(name)) # Typ muss ein Symbol sein !
       { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
         pushSTACK(name);
         pushSTACK(*stream_); # Stream
         pushSTACK(S(read));
         fehler(stream_error,
                GETTEXT("~ from ~: the type of a structure should be a symbol, not ~")
               );
       }
     pushSTACK(name);
     # Stackaufbau: Stream, restl.Args, name.
     if (eq(name,S(hash_table))) # Symbol HASH-TABLE ?
       # ja -> speziell behandeln, keine Structure:
       { # Hash-Tabelle
         # Restliche Argumentliste muss ein Cons sein:
         if (!consp(args))
           { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
             pushSTACK(*stream_); # Stream
             pushSTACK(S(read));
             fehler(stream_error,
                    GETTEXT("~ from ~: bad HASH-TABLE")
                   );
           }
         # (MAKE-HASH-TABLE :TEST (car args) :INITIAL-CONTENTS (cdr args))
         pushSTACK(S(Ktest)); # :TEST
         pushSTACK(Car(args)); # Test (Symbol)
         pushSTACK(S(Kinitial_contents)); # :INITIAL-CONTENTS
         pushSTACK(Cdr(args)); # Aliste ((Key_1 . Value_1) ... (Key_n . Value_n))
         funcall(L(make_hash_table),4); # Hash-Tabelle bauen
         mv_count=1; # value1 als Wert
         skipSTACK(3); return;
       }
     if (eq(name,S(random_state))) # Symbol RANDOM-STATE ?
       # ja -> speziell behandeln, keine Structure:
       { # Random-State
         # Restliche Argumentliste muss ein Cons mit NIL als CDR und
         # einem Simple-Bit-Vektor der Länge 64 als CAR sein:
         if (!(consp(args)
               && nullp(Cdr(args))
               && simple_bit_vector_p(Car(args))
               && (Sbvector_length(Car(args)) == 64)
            ) )
           { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
             pushSTACK(name);
             pushSTACK(*stream_); # Stream
             pushSTACK(S(read));
             fehler(stream_error,
                    GETTEXT("~ from ~: bad ~")
                   );
           }
         STACK_0 = Car(args); # Simple-Bit-Vektor retten
        {var object ergebnis = allocate_random_state(); # neuer Random-State
         The_Random_state(ergebnis)->random_state_seed = popSTACK(); # füllen
         value1 = ergebnis; mv_count=1; skipSTACK(2); return;
       }}
     if (eq(name,S(pathname))) # Symbol PATHNAME ?
       # ja -> speziell behandeln, keine Structure:
       { STACK_1 = make_references(args); pushSTACK(L(make_pathname)); }
     #ifdef LOGICAL_PATHNAMES
     elif (eq(name,S(logical_pathname))) # Symbol LOGICAL-PATHNAME ?
       # ja -> speziell behandeln, keine Structure:
       { STACK_1 = make_references(args); pushSTACK(L(make_logical_pathname)); }
     #endif
     elif (eq(name,S(byte))) # Symbol BYTE ?
       # ja -> speziell behandeln, keine Structure:
       { pushSTACK(S(make_byte)); }
     else
       # (GET name 'SYS::DEFSTRUCT-DESCRIPTION) ausführen:
       {var object description = get(name,S(defstruct_description));
        if (eq(description,unbound)) # nichts gefunden?
          # Structure dieses Typs undefiniert
          { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(name);
            pushSTACK(*stream_); # Stream
            pushSTACK(S(read));
            fehler(stream_error,
                   GETTEXT("~ from ~: no structure of type ~ has been defined")
                  );
          }
        # description muss ein Simple-Vector der Länge >=4 sein:
        if (!(simple_vector_p(description) && (Svector_length(description) >= 4)))
          { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(name);
            pushSTACK(S(defstruct_description));
            pushSTACK(*stream_); # Stream
            pushSTACK(S(read));
            fehler(stream_error,
                   GETTEXT("~ from ~: bad ~ for ~")
                  );
          }
        # Konstruktorfunktion holen:
        {var object constructor = # (svref description 2)
           TheSvector(description)->data[2];
         if (nullp(constructor))
           { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
             pushSTACK(name);
             pushSTACK(*stream_); # Stream
             pushSTACK(S(read));
             fehler(stream_error,
                    GETTEXT("~ from ~: structures of type ~ cannot be read in, missing constructor function")
                   );
           }
    # Konstruktorfunktion mit angepasster Argumentliste aufrufen:
         pushSTACK(constructor);
    }  }}# Stackaufbau: Stream, restl.Args, name, Konstruktor.
    {var uintC argcount = 0; # Zahl der Argumente für den Konstruktor
     loop # restliche Argumentliste durchlaufen,
          # Argumente für den Konstruktor auf den STACK legen:
       { check_STACK();
         args = *(stream_ STACKop -1); # restliche Args
         if (nullp(args)) break; # keine mehr -> Argumente im STACK fertig
         if (atomp(args))
           { dotted:
             pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
             pushSTACK(*(stream_ STACKop -2)); # name
             pushSTACK(*stream_); # Stream
             pushSTACK(S(read));
             fehler(stream_error,
                    GETTEXT("~ from ~: a structure ~ may not contain a component \".\"")
                   );
           }
         {var object slot = Car(args);
          if (!symbolp(slot))
            { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
              pushSTACK(*(stream_ STACKop -2)); # name
              pushSTACK(slot);
              pushSTACK(*stream_); # Stream
              pushSTACK(S(read));
              fehler(stream_error,
                     GETTEXT("~ from ~: ~ is not a symbol, not a slot name of structure ~")
                    );
            }
          if (nullp(Cdr(args)))
            { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
              pushSTACK(*(stream_ STACKop -2)); # name
              pushSTACK(slot);
              pushSTACK(*stream_); # Stream
              pushSTACK(S(read));
              fehler(stream_error,
                     GETTEXT("~ from ~: missing value of slot ~ in structure ~")
                    );
            }
          if (matomp(Cdr(args))) goto dotted;
          {var object kw = intern_keyword(Symbol_name(slot)); # Slotname als Keyword
           pushSTACK(kw); # Keyword in den STACK
          }
          args = *(stream_ STACKop -1); # wieder dieselben restlichen Args
          args = Cdr(args);
          pushSTACK(Car(args)); # Slot-value in den STACK
          *(stream_ STACKop -1) = Cdr(args); # Argliste verkürzen
         }
         argcount += 2; # und mitzählen
         if (argcount == 0)
           # Argumentezähler zu groß geworden
           { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
             pushSTACK(*(stream_ STACKop -2)); # name
             pushSTACK(*stream_); # Stream
             pushSTACK(S(read));
             fehler(stream_error,
                    GETTEXT("~ from ~: too many slots for structure ~")
                   );
           }
       }
     funcall(*(stream_ STACKop -3),argcount); # Konstruktor aufrufen
     mv_count=1; skipSTACK(4); return; # value1 als Wert
    }
  }}

# (set-dispatch-macro-character #\# #\Y
#   #'(lambda (stream sub-char arg)
#       (declare (ignore sub-char))
#       (if arg
#         (if (eql arg 0)
#           ; Encoding lesen
#           (let ((obj
#                   (let ((*read-suppress* nil)
#                         (*package* (find-package "CHARSET")))
#                     (read stream t nil t)
#                )) )
#             (setf (stream-external-format stream) obj)
#             (values)
#           )
#           ; Codevector lesen
#           (let ((obj (let ((*read-base* 16.)) (read stream t nil t))))
#             (unless *read-suppress*
#               (unless (= (length obj) arg)
#                 (error "Falsche Länge eines Closure-Vektors: ~S" arg)
#               )
#               (make-code-vector obj) ; Simple-Bit-Vektor, Inhalt: arg Bytes
#         ) ) )
#         ; Closure lesen
#         (let ((obj (read stream t nil t)))
#           (unless *read-suppress*
#             (%make-closure (first obj) (second obj) (cddr obj))
# )   ) ) ) )
  # Fehlermeldung wegen falscher Syntax eines Code-Vektors
  # fehler_closure_badchar();
  # > Stackaufbau: stream, sub-char, arg.
    nonreturning_function(local, fehler_closure_badchar, (void));
    local void fehler_closure_badchar()
      { pushSTACK(STACK_2); # Wert für Slot STREAM von STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(STACK_(2+2)); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: illegal syntax of closure code vector after #~Y")
              );
      }
  # UP: Überprüft, ob Character ch mit Syntaxcode scode eine
  # Hexadezimal-Ziffer ist, und liefert ihren Wert.
  # hexziffer(ch,scode)
  # > ch, scode: Character (oder eof_value) und sein Syntaxcode
  # > Stackaufbau: stream, sub-char, arg.
  # < ergebnis: Wert (>=0, <16) der Hexziffer
    local uintB hexziffer (object ch, uintWL scode);
    local uintB hexziffer(ch,scode)
      var object ch;
      var uintWL scode;
      { if (scode == syntax_eof) { fehler_eof_innen(&STACK_2); }
        # ch ist ein Character
       {var cint c = as_cint(char_code(ch));
        if (c<'0') goto badchar; if (c<='9') { return (c-'0'); } # '0'..'9'
        if (c<'A') goto badchar; if (c<='F') { return (c-'A'+10); } # 'A'..'F'
        if (c<'a') goto badchar; if (c<='f') { return (c-'a'+10); } # 'a'..'f'
        badchar: fehler_closure_badchar();
      }}
LISPFUNN(closure_reader,3) # liest #Y
  { var object* stream_ = test_stream_arg(STACK_2);
    # bei n=0 ein Encoding lesen:
    if (eq(STACK_0,Fixnum_0))
      { dynamic_bind(S(read_suppress),NIL); # *READ-SUPPRESS* an NIL binden
        dynamic_bind(S(packagestern),O(charset_package)); # *PACKAGE* an #<PACKAGE CHARSET> binden
       {var object expr = read_recursive_no_dot(stream_); # Ausdruck lesen
        dynamic_unbind();
        dynamic_unbind();
        expr = make_references(expr); # Verweise entflechten
        pushSTACK(*stream_); pushSTACK(expr); pushSTACK(S(Kinput));
        funcall(L(set_stream_external_format),3); # (SYS::SET-STREAM-EXTERNAL-FORMAT stream expr :input)
        value1 = NIL; mv_count=0; skipSTACK(3); return; # keine Werte
      }}
    # bei *READ-SUPPRESS* /= NIL nur ein Objekt lesen:
    if (test_value(S(read_suppress)))
      { read_recursive_no_dot(stream_); # Objekt lesen, wegwerfen
        value1 = NIL; mv_count=1; skipSTACK(3); return; # NIL als Wert
      }
    # je nach n :
    if (nullp(STACK_0))
      # n=NIL -> Closure lesen:
      { var object obj = read_recursive_no_dot(stream_); # Objekt lesen
        if (!(consp(obj) && mconsp(Cdr(obj)))) # Länge >=2 ?
          { pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
            pushSTACK(obj);
            pushSTACK(*stream_); # Stream
            pushSTACK(S(read));
            fehler(stream_error,
                   GETTEXT("~ from ~: object #Y~ has not the syntax of a compiled closure")
                  );
          }
        skipSTACK(3);
        # (SYS::%MAKE-CLOSURE (first obj) (second obj) (cddr obj)) ausführen:
        pushSTACK(Car(obj)); obj = Cdr(obj); # 1. Argument
        pushSTACK(Car(obj)); obj = Cdr(obj); # 2. Argument
        pushSTACK(obj); # 3. Argument
        funcall(L(make_closure),3);
        mv_count=1; # value1 als Wert
      }
      else
      # n angegeben -> Codevektor lesen:
      # Syntax: #nY(b1 ... bn), wo n ein Fixnum >=0 und b1,...,bn
      # Fixnums >=0, <256 in Basis 16 sind (jeweils ein- oder zweistellig).
      # Beispielsweise #9Y(0 4 F CD 6B8FD1e4 5)
      { # n ist ein Integer >=0.
        var uintL n =
          (posfixnump(STACK_0) ? posfixnum_to_L(STACK_0) # Fixnum -> Wert
                               : bitm(oint_data_len)-1 # Bignum -> großer Wert
          );
        # neuen Bit-Vektor mit n Bytes besorgen:
        STACK_1 = allocate_bit_vector(8*n);
        # Stackaufbau: Stream, Codevektor, n.
       {var object ch;
        var uintWL scode;
        # Whitespace überlesen:
        do { read_char_syntax(ch = ,scode = ,stream_); } # Zeichen lesen
           until (!(scode == syntax_whitespace));
        # Es muss ein '(' folgen:
        if (!eq(ch,ascii_char('('))) { fehler_closure_badchar(); }
        {var uintL index = 0;
         until (index==n)
           { # Whitespace überlesen:
             do { read_char_syntax(ch = ,scode = ,stream_); } # Zeichen lesen
                until (!(scode == syntax_whitespace));
            {# es muss eine Hex-Ziffer folgen:
             var uintB zif = hexziffer(ch,scode);
             # nächstes Character lesen:
             read_char_syntax(ch = ,scode = ,stream_);
             if (scode == syntax_eof) { fehler_eof_innen(stream_); } # EOF -> Error
             if ((scode == syntax_whitespace) || eq(ch,ascii_char(')')))
               # Whitespace oder Klammer zu
               { # wird auf den Stream zurückgeschoben:
                 unread_char(stream_,ch);
               }
               else
               { # es muss eine zweite Hex-Ziffer sein
                 zif = 16*zif + hexziffer(ch,scode); # zur ersten Hex-Ziffer dazu
                 # (Nach der zweiten Hex-Ziffer wird kein Whitespace verlangt.)
               }
             # zif = gelesenes Byte. In den Codevektor eintragen:
             TheSbvector(STACK_1)->data[index] = zif;
             index++;
           }}
        }
        # Whitespace überlesen:
        do { read_char_syntax(ch = ,scode = ,stream_); } # Zeichen lesen
           until (!(scode == syntax_whitespace));
        # Es muss ein ')' folgen:
        if (!eq(ch,ascii_char(')'))) { fehler_closure_badchar(); }
        #if BIG_ENDIAN_P
        # Header von Little-Endian nach Big-Endian konvertieren:
        { var Sbvector v = TheSbvector(STACK_1);
          swap(uintB, v->data[CCV_SPDEPTH_1], v->data[CCV_SPDEPTH_1+1]);
          swap(uintB, v->data[CCV_SPDEPTH_JMPBUFSIZE], v->data[CCV_SPDEPTH_JMPBUFSIZE+1]);
          swap(uintB, v->data[CCV_NUMREQ], v->data[CCV_NUMREQ+1]);
          swap(uintB, v->data[CCV_NUMOPT], v->data[CCV_NUMOPT+1]);
          if (v->data[CCV_FLAGS] & bit(7))
            { swap(uintB, v->data[CCV_NUMKEY], v->data[CCV_NUMKEY+1]);
              swap(uintB, v->data[CCV_KEYCONSTS], v->data[CCV_KEYCONSTS+1]);
        }   }
        #endif
        # Codevektor als Wert:
        value1 = STACK_1; mv_count=1; skipSTACK(3);
      }}
  }

# (set-dispatch-macro-character #\# #\"
#   #'(lambda (stream sub-char n)
#       (unless *read-suppress*
#         (if n
#           (error "~ von ~: Zwischen # und " ist keine Zahl erlaubt."
#                  'read stream
#       ) ) )
#       (unread-char sub-char stream)
#       (let ((obj (read stream t nil t))) ; String lesen
#         (unless *read-suppress* (pathname obj))
# )   ) )
LISPFUNN(clisp_pathname_reader,3) # liest #"
  { test_no_infix(); # n muss NIL sein
    # Stackaufbau: Stream, sub-char #\".
   {var object string = # String lesen, der mit " anfängt
      (funcall(L(string_reader),2),value1);
    # bei *READ-SUPPRESS* /= NIL sofort fertig:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=1; return; } # NIL als Wert
    # Bilde (pathname string) = (values (parse-namestring string)) :
    pushSTACK(string); funcall(L(parse_namestring),1); # (PARSE-NAMESTRING string)
    mv_count=1; # nur 1 Wert
  }}

# (set-dispatch-macro-character #\# #\P
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (if *read-suppress*
#         (progn (read stream t nil t) nil)
#         (if n
#           (error "~ von ~: Zwischen # und P ist keine Zahl erlaubt."
#                  'read stream
#           )
#           (let ((obj (read stream t nil t)))
#             (if (stringp obj)
#               (values (parse-namestring obj))
#               (error "~ von ~: Falsche Syntax für Pathname: #P~"
#                      'read stream obj
# )   ) ) ) ) ) )
LISPFUNN(ansi_pathname_reader,3) # liest #P
  { var object* stream_ = test_no_infix(); # n muss NIL sein
    var object obj = read_recursive_no_dot(stream_); # nächstes Objekt lesen
    # bei *READ-SUPPRESS* /= NIL sofort fertig:
    if (test_value(S(read_suppress)))
      { value1 = NIL; mv_count=1; skipSTACK(2); return; }
    obj = make_references(obj); # und Verweise vorzeitig entflechten (unnötig?)
    if (!stringp(obj)) goto bad; # obj muss ein String sein!
    # Bilde (pathname obj) = (values (parse-namestring obj)) :
    pushSTACK(obj); funcall(L(parse_namestring),1); # (PARSE-NAMESTRING obj)
    mv_count=1; skipSTACK(2); return; # nur 1 Wert
   {bad:
      pushSTACK(*stream_); # Wert für Slot STREAM von STREAM-ERROR
      pushSTACK(obj); # Objekt
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: bad syntax for pathname: #P~")
            );
  }}

#ifdef UNIX

# (set-dispatch-macro-character #\# #\!
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (when n (error ...))
#       (read-line stream)
#       (values)
# )   )
LISPFUNN(unix_executable_reader,3) # liest #!
  { var object* stream_ = test_no_infix(); # n muss NIL sein
    # Stackaufbau: Stream, sub-char #\!.
    loop
      { var object ch = read_char(stream_); # Zeichen lesen
        if (eq(ch,eof_value) || eq(ch,ascii_char(NL))) break;
      }
    value1 = NIL; mv_count=0; skipSTACK(2); # keine Werte zurück
  }

#endif

# ------------------------ LISP-Funktionen des Readers ------------------------

# UP: Überprüft ein Input-Stream-Argument.
# Default ist der Wert von *STANDARD-INPUT*.
# test_istream(&stream);
# > subr_self: Aufrufer (ein SUBR)
# > stream: Input-Stream-Argument
# < stream: Input-Stream (ein Stream)
  local void test_istream (object* stream_);
  local void test_istream(stream_)
    var object* stream_;
    { var object stream = *stream_;
      if (eq(stream,unbound) || nullp(stream))
        # statt #<UNBOUND> oder NIL: Wert von *STANDARD-INPUT*
        { *stream_ = var_stream(S(standard_input),strmflags_rd_ch_B); }
      elif (eq(stream,T))
        # statt T: Wert von *TERMINAL-IO*
        { *stream_ = var_stream(S(terminal_io),strmflags_rd_ch_B); }
      else
        { if (!streamp(stream)) { fehler_stream(stream); } }
    }

# EOF-Handling, beendet Reader-Funktionen.
# eof_handling()
# > STACK_3: Input-Stream
# > STACK_2: eof-error-p
# > STACK_1: eof-value
# > STACK_0: recursive-p
# < mv_space/mv_count: Werte
  local Values eof_handling (void);
  local Values eof_handling()
    { if (!nullp(STACK_2)) # eof-error-p /= NIL (z.B. = #<UNBOUND>) ?
        # Error melden:
        { var object recursive_p = STACK_0;
          if (eq(recursive_p,unbound) || nullp(recursive_p))
            { fehler_eof_aussen(&STACK_3); } # EOF melden
            else
            { fehler_eof_innen(&STACK_3); } # EOF innerhalb Objekt melden
        }
        else
        # EOF verarzten:
        { var object eofval = STACK_1;
          if (eq(eofval,unbound)) { eofval = eof_value; } # Default ist #<EOF>
          value1 = eofval; mv_count=1; skipSTACK(4); # eofval als Wert
        }
    }

# UP für READ und READ-PRESERVING-WHITESPACE
# read_w(whitespace-p)
# > whitespace-p: gibt an, ob danach whitespace zu verbrauchen ist
# > Stackaufbau: input-stream, eof-error-p, eof-value, recursive-p.
# > subr_self: Aufrufer (ein SUBR) (unnötig, falls input-stream ein Stream ist)
# < STACK: aufgeräumt
# < mv_space/mv_count: Werte
  local Values read_w (object whitespace_p);
  local Values read_w(whitespace_p)
    var object whitespace_p;
    { # input-stream überprüfen:
      test_istream(&STACK_3);
      # recursive-p-Argument abfragen:
     {var object recursive_p = STACK_0;
      if (eq(recursive_p,unbound) || nullp(recursive_p))
        # nicht-rekursiver Aufruf
        { var object obj = read_top(&STACK_3,whitespace_p);
          if (eq(obj,dot_value)) { fehler_dot(STACK_3); } # Dot -> Error
          if (eq(obj,eof_value))
            { return_Values eof_handling(); } # EOF-Behandlung
            else
            { value1 = obj; mv_count=1; skipSTACK(4); } # obj als Wert
        }
        else
        # rekursiver Aufruf
        { value1 = read_recursive_no_dot(&STACK_3); mv_count=1; skipSTACK(4); }
    }}

LISPFUN(read,0,4,norest,nokey,0,NIL)
# (READ [input-stream [eof-error-p [eof-value [recursive-p]]]]), CLTL S. 375
  { return_Values read_w(NIL); } # whitespace-p := NIL

LISPFUN(read_preserving_whitespace,0,4,norest,nokey,0,NIL)
# (READ-PRESERVING-WHITESPACE [input-stream [eof-error-p [eof-value [recursive-p]]]]),
# CLTL S. 376
  { return_Values read_w(T); } # whitespace-p := T

LISPFUN(read_delimited_list,1,2,norest,nokey,0,NIL)
# (READ-DELIMITED-LIST char [input-stream [recursive-p]]), CLTL S. 377
  { # char überprüfen:
    var object ch = STACK_2;
    if (!charp(ch)) { fehler_char(ch); }
    # input-stream überprüfen:
    test_istream(&STACK_1);
    # recursive-p-Argument abfragen:
   {var object recursive_p = popSTACK();
    # Stackaufbau: char, input-stream.
    if (eq(recursive_p,unbound) || nullp(recursive_p))
      # nicht-rekursiver Aufruf
      { var object* stream_ = &STACK_0;
        # SYS::*READ-REFERENCE-TABLE* an die leere Tabelle NIL binden:
        dynamic_bind(S(read_reference_table),NIL);
        # SYS::*BACKQUOTE-LEVEL* an NIL binden:
        dynamic_bind(S(backquote_level),NIL);
       {var object obj = read_delimited_list(stream_,ch,eof_value); # Liste lesen
        obj = make_references(obj); # Verweise entflechten
        dynamic_unbind();
        dynamic_unbind();
        value1 = obj; # Liste als Wert
      }}
      else
      # rekursiver Aufruf
      { value1 = read_delimited_list(&STACK_0,ch,eof_value); }
    # (Beide Male Liste gelesen, keine Dotted List zugelassen.)
    mv_count=1; skipSTACK(2);
  }}

LISPFUN(read_line,0,4,norest,nokey,0,NIL)
# (READ-LINE [input-stream [eof-error-p [eof-value [recursive-p]]]]),
# CLTL S. 378
  { # input-stream überprüfen:
    var object* stream_ = &STACK_3;
    test_istream(stream_);
    get_buffers(); # zwei leere Buffer auf den Stack
    if (!read_line(stream_,&STACK_1)) # Zeile lesen
      # End of Line
      { # Buffer kopieren und dabei in Simple-String umwandeln:
        value1 = copy_string(STACK_1);
        # Buffer zur Wiederverwendung freigeben:
        O(token_buff_2) = popSTACK(); O(token_buff_1) = popSTACK();
        value2 = NIL; mv_count=2; # NIL als 2. Wert
        skipSTACK(4); return;
      }
      else
      # End of File
      { # Buffer leer ?
        if (TheIarray(STACK_1)->dims[1] == 0) # Länge (Fill-Pointer) = 0 ?
          { # Buffer zur Wiederverwendung freigeben:
            O(token_buff_2) = popSTACK(); O(token_buff_1) = popSTACK();
            # EOF speziell behandeln:
            return_Values eof_handling();
          }
          else
          { # Buffer kopieren und dabei in Simple-String umwandeln:
            value1 = copy_string(STACK_1);
            # Buffer zur Wiederverwendung freigeben:
            O(token_buff_2) = popSTACK(); O(token_buff_1) = popSTACK();
            value2 = T; mv_count=2; # T als 2. Wert
            skipSTACK(4); return;
      }   }
  }

LISPFUN(read_char,0,4,norest,nokey,0,NIL)
# (READ-CHAR [input-stream [eof-error-p [eof-value [recursive-p]]]]),
# CLTL S. 379
  { # input-stream überprüfen:
    var object* stream_ = &STACK_3;
    test_istream(stream_);
   {var object ch = read_char(stream_); # Character lesen
    if (eq(ch,eof_value))
      { return_Values eof_handling(); }
      else
      { value1 = ch; mv_count=1; skipSTACK(4); return; } # ch als Wert
  }}

LISPFUN(unread_char,1,1,norest,nokey,0,NIL)
# (UNREAD-CHAR char [input-stream]), CLTL S. 379
  { # input-stream überprüfen:
    var object* stream_ = &STACK_0;
    test_istream(stream_);
   {var object ch = STACK_1; # char
    if (!charp(ch)) # muss ein Character sein !
      { pushSTACK(ch); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(character)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(ch);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: ~ is not a character")
              );
      }
    unread_char(stream_,ch); # char auf Stream zurückschieben
    value1 = NIL; mv_count=1; skipSTACK(2); # NIL als Wert
  }}

LISPFUN(peek_char,0,5,norest,nokey,0,NIL)
# (PEEK-CHAR [peek-type [input-stream [eof-error-p [eof-value [recursive-p]]]]]),
# CLTL S. 379
  { # input-stream überprüfen:
    var object* stream_ = &STACK_3;
    test_istream(stream_);
    # Fallunterscheidung nach peek-type:
   {var object peek_type = STACK_4;
    if (eq(peek_type,unbound) || nullp(peek_type))
      # Default NIL: 1 Zeichen peeken
      { var object ch = peek_char(stream_);
        if (eq(ch,eof_value)) goto eof;
        value1 = ch; mv_count=1; skipSTACK(5); return; # ch als Wert
      }
    elif (eq(peek_type,T))
      # T: Whitespace-Peek
      { var object ch = wpeek_char_eof(stream_);
        if (eq(ch,eof_value)) goto eof;
        value1 = ch; mv_count=1; skipSTACK(5); return; # ch als Wert
      }
    elif (charp(peek_type))
      # peek-type ist ein Character
      { var object ch;
        loop
          { ch = read_char(stream_); # Zeichen lesen
            if (eq(ch,eof_value)) goto eof;
            if (eq(ch,peek_type)) break; # das vorgegebene Ende-Zeichen?
          }
        unread_char(stream_,ch); # Zeichen zurückschieben
        value1 = ch; mv_count=1; skipSTACK(5); return; # ch als Wert
      }
    else
      { pushSTACK(peek_type); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(O(type_peektype)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(peek_type);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: peek type should be NIL or T or a character, not ~")
              );
      }
    eof: # EOF liegt vor
      eof_handling(); skipSTACK(1); return;
  }}

LISPFUN(listen,0,1,norest,nokey,0,NIL)
# (LISTEN [input-stream]), CLTL S. 380
  { test_istream(&STACK_0); # input-stream überprüfen
    if (ls_avail_p(stream_listen(popSTACK())))
      { value1 = T; mv_count=1; } # Wert T
      else
      { value1 = NIL; mv_count=1; } # Wert NIL
  }

LISPFUNN(read_char_will_hang_p,1)
# (READ-CHAR-WILL-HANG-P input-stream)
# tests whether READ-CHAR-NO-HANG will return immediately without reading a
# character, but accomplishes this without actually calling READ-CHAR-NO-HANG,
# thus avoiding the need for UNREAD-CHAR and preventing side effects.
  { test_istream(&STACK_0); # input-stream überprüfen
    value1 = (ls_wait_p(stream_listen(popSTACK())) ? T : NIL); mv_count=1;
  }

LISPFUN(read_char_no_hang,0,4,norest,nokey,0,NIL)
# (READ-CHAR-NO-HANG [input-stream [eof-error-p [eof-value [recursive-p]]]]),
# CLTL S. 380
  { # input-stream überprüfen:
    var object* stream_ = &STACK_3;
    test_istream(stream_);
   {var object stream = *stream_;
    if (builtin_stream_p(stream)
        ? !(TheStream(stream)->strmflags & bit(strmflags_rd_ch_bit_B))
        : !instanceof(stream,O(class_fundamental_input_stream))
       )
      { fehler_illegal_streamop(S(read_char_no_hang),stream); }
    { var signean status = stream_listen(stream);
      if (ls_eof_p(status)) # EOF ?
        { return_Values eof_handling(); }
      elif (ls_avail_p(status)) # Zeichen verfügbar
        { var object ch = read_char(stream_); # Character lesen
          if (eq(ch,eof_value)) # sicherheitshalber nochmals auf EOF abfragen
            { return_Values eof_handling(); }
            else
            { value1 = ch; mv_count=1; skipSTACK(4); return; } # ch als Wert
        }
      else # ls_wait_p(status) # kein Zeichen verfügbar
        # statt zu warten, sofort NIL als Wert:
        { value1 = NIL; mv_count=1; skipSTACK(4); return; }
  }}}

LISPFUN(clear_input,0,1,norest,nokey,0,NIL)
# (CLEAR-INPUT [input-stream]), CLTL S. 380
  { test_istream(&STACK_0); # input-stream überprüfen
    clear_input(popSTACK());
    value1 = NIL; mv_count=1; # Wert NIL
  }

LISPFUN(read_from_string,1,2,norest,key,3,\
        (kw(preserve_whitespace),kw(start),kw(end)) )
# (READ-FROM-STRING string [eof-error-p [eof-value [:preserve-whitespace] [:start] [:end]]]),
# CLTL S. 380
# Methode:
# (defun read-from-string (string &optional (eof-error-p t) (eof-value nil)
#                          &key (start 0) (end nil) (preserve-whitespace nil)
#                          &aux index)
#   (values
#     (with-input-from-string (stream string :start start :end end :index index)
#       (funcall (if preserve-whitespace #'read-preserving-whitespace #'read)
#                stream eof-error-p eof-value nil
#     ) )
#     index
# ) )
# oder macroexpandiert:
# (defun read-from-string (string &optional (eof-error-p t) (eof-value nil)
#                          &key (start 0) (end nil) (preserve-whitespace nil))
#   (let ((stream (make-string-input-stream string start end)))
#     (values
#       (unwind-protect
#         (funcall (if preserve-whitespace #'read-preserving-whitespace #'read)
#                  stream eof-error-p eof-value nil
#         )
#         (close stream)
#       )
#       (system::string-input-stream-index stream)
# ) ) )
# oder vereinfacht:
# (defun read-from-string (string &optional (eof-error-p t) (eof-value nil)
#                          &key (start 0) (end nil) (preserve-whitespace nil))
#   (let ((stream (make-string-input-stream string start end)))
#     (values
#       (funcall (if preserve-whitespace #'read-preserving-whitespace #'read)
#                stream eof-error-p eof-value nil
#       )
#       (system::string-input-stream-index stream)
# ) ) )
  { # Stackaufbau: string, eof-error-p, eof-value, preserve-whitespace, start, end.
    # :preserve-whitespace-Argument verarbeiten:
    var object preserve_whitespace = STACK_2;
    if (eq(preserve_whitespace,unbound)) { preserve_whitespace = NIL; }
    # MAKE-STRING-INPUT-STREAM mit Argumenten string, start, end aufrufen:
    STACK_2 = STACK_5; # string
    if (eq(STACK_1,unbound)) { STACK_1 = Fixnum_0; } # start hat Default 0
    if (eq(STACK_0,unbound)) { STACK_0 = NIL; } # end hat Default NIL
    STACK_5 = preserve_whitespace;
    funcall(L(make_string_input_stream),3);
    # Stackaufbau: preserve-whitespace, eof-error-p, eof-value.
    pushSTACK(STACK_1); pushSTACK(STACK_1);
    STACK_3 = STACK_2 = value1;
    # Stackaufbau: preserve-whitespace, stream, stream, eof-error-p, eof-value.
    pushSTACK(NIL); read_w(STACK_5); # READ bzw. READ-PRESERVE-WHITESPACE
    # Stackaufbau: preserve-whitespace, stream.
    STACK_1 = value1; # gelesenes Objekt
    funcall(L(string_input_stream_index),1); # (SYS::STRING-INPUT-STREAM-INDEX stream)
    value2 = value1; value1 = popSTACK(); # Index als 2., Objekt als 1. Wert
    mv_count=2;
  }

LISPFUN(parse_integer,1,0,norest,key,4,\
        (kw(start),kw(end),kw(radix),kw(junk_allowed)) )
# (PARSE-INTEGER string [:start] [:end] [:radix] [:junk-allowed]), CLTL S. 381
  { # :junk-allowed-Argument verarbeiten:
    var boolean junk_allowed;
    {var object arg = popSTACK();
     if (eq(arg,unbound) || nullp(arg))
       { junk_allowed = FALSE; }
       else
       { junk_allowed = TRUE; }
    }
    # junk_allowed = Wert des :junk-allowed-Arguments.
    # :radix-Argument verarbeiten:
   {var uintL base;
    {var object arg = popSTACK();
     if (eq(arg,unbound))
       { base = 10; } # Default 10
       else
       { if (posfixnump(arg) &&
             (base = posfixnum_to_L(arg), ((base >= 2) && (base <= 36)))
            )
           {} # OK
           else
           { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
             pushSTACK(O(type_radix)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
             pushSTACK(arg); # base
             pushSTACK(S(Kradix));
             pushSTACK(TheSubr(subr_self)->name);
             fehler(type_error,
                    GETTEXT("~: ~ argument should be an integer between 2 and 36, not ~")
                   );
    }  }   }
    # base = Wert des :radix-Arguments.
    { # string, :start und :end überprüfen:
      var stringarg arg;
      var object string = test_string_limits_ro(&arg);
      # STACK jetzt aufgeräumt.
      var uintL start = arg.index; # Wert des :start-Arguments
      var uintL len = arg.len; # Anzahl der angesprochenen Characters
      var const chart* charptr;
      unpack_sstring_alloca(arg.string,arg.len,arg.offset+arg.index, charptr=);
      # Schleifenvariablen:
     {var uintL index = start;
      var uintL count = len;
      var uintL start_offset;
      var uintL end_offset;
      # Ab jetzt:
      #   string : der String,
      #   arg.string : sein Datenvektor (ein Simple-String),
      #   start : Index des ersten Characters im String,
      #   charptr : Pointer in den Datenvektor auf das nächste Character,
      #   index : Index in den String,
      #   count : verbleibende Anzahl Characters.
      var signean sign; # Vorzeichen
      { var chart c; # letztes gelesenes Character
        # 1. Schritt: Whitespace übergehen
        loop
          { if (count==0) goto badsyntax; # Stringstück schon zu Ende ?
            c = *charptr; # nächstes Character
            if (!(orig_syntax_table_get(c) == syntax_whitespace)) # kein Whitespace?
              break;
            charptr++; index++; count--; # Whitespacezeichen übergehen
          }
        # 2. Schritt: Vorzeichen lesen
        sign = 0; # Vorzeichen := positiv
        switch (as_cint(c))
          { case '-': sign = -1; # Vorzeichen := negativ
            case '+': # Vorzeichen angetroffen
              charptr++; index++; count--; # übergehen
              if (count==0) goto badsyntax; # Stringstück schon zu Ende ?
            default: break;
          }
      }
      # Vorzeichen fertig, es kommt noch was (count>0).
      start_offset = arg.offset + index;
      # Ab jetzt:  start_offset = Offset der ersten Ziffer im Datenvektor.
      # 3. Schritt: Ziffern lesen
      loop
        { var cint c = as_cint(*charptr); # nächstes Character
          # Test auf Ziffer: (digit-char-p (code-char c) base) ?
          # (vgl. DIGIT-CHAR-P in CHARSTRG.D)
          if (c > 'z') break; # zu groß -> nein
          if (c >= 'a') { c -= 'a'-'A'; } # Character >='a',<='z' in Großbuchstaben wandeln
          # Nun ist $00 <= c <= $60.
          if (c < '0') break;
          # $30 <= c <= $60 in Zahlwert umwandeln:
          if (c <= '9') { c = c - '0'; }
          else if (c >= 'A') { c = c - 'A' + 10; }
          else break;
          # Nun ist c der Zahlwert der Ziffer, >=0, <=41.
          if (c >= (uintB)base) break; # nur gültig, falls 0 <= c < base.
          # *charptr ist eine gültige Ziffer.
          charptr++; index++; count--; # übergehen
          if (count==0) break;
        }
      # Ziffern fertig.
      end_offset = arg.offset + index;
      # Ab jetzt:  end_offset = Offset nach der letzten Ziffer im Datenvektor.
      if (start_offset == end_offset) # gab es keine Ziffern?
        goto badsyntax;
      # 4. Schritt: evtl. Whitespace am Schluss übergehen
      if (!junk_allowed) # (falls junk_allowed, ist nichts zu tun)
        { while (!(count==0))
            { var chart c = *charptr; # nächstes Character
              if (!(orig_syntax_table_get(c) == syntax_whitespace)) # kein Whitespace?
                goto badsyntax;
              charptr++; index++; count--; # Whitespacezeichen übergehen
            }
        }
      # 5. Schritt: Ziffernfolge in Zahl umwandeln
      value1 = read_integer(base,sign,arg.string,start_offset,end_offset);
      value2 = fixnum(index); # Index als 2. Wert
      mv_count=2; return;
      badsyntax: # Illegales Zeichen
      if (!junk_allowed)
        # Error melden:
        { pushSTACK(unbound); # "Wert" für Slot STREAM von STREAM-ERROR
          pushSTACK(string);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(stream_error,
                 GETTEXT("~: string ~ does not have integer syntax")
                );
        }
      value1 = NIL; # NIL als 1. Wert
      value2 = fixnum(index); # Index als 2. Wert
      mv_count=2; return;
  }}}}


# =============================================================================
#                              P R I N T
# =============================================================================

# Grundidee des Printers:
# Vom Datentyp abhängig, wird die externe Repräsentation des Objekts auf den
# Stream ausgegeben, rekursiv. Der Unterschied zwischen PRINT und PPRINT
# besteht darin, dass an einigen Stellen statt einem Space ein Newline und
# einige Spaces ausgegeben werden. Um dies zu bewerkstelligen, wird die
# externe Repräsentation der Teil-Objekte auf einen Pretty-Printer-Hilfs-
# (PPHELP-)Stream ausgegeben, dann überprüft, ob man mehrere Zeilen braucht
# oder eine ausreicht, und schließlich (davon abhängig) Whitespace eingefügt.
# Die genauere Spezifikation der prin_object-Routine:
# > Stream,
# > Zeilenlänge L,
# > Linker Rand für Einzeiler L1,
# > Linker Rand für Mehrzeiler LM,
# > Anzahl der auf der letzten Zeile am Schluss noch zu schließenden Klammern
#   K (Fixnum >=0) und Flag, ob bei Mehrzeilern die letzten schließenden
#   Klammern in einer eigenen Zeile, justiert unterhalb der entsprechenden
#   öffnenden Klammern, erscheinen sollen.
#   [Der Einfachheit halber ist hier stets K=0 und Flag=True, d.h. alle
#   schließenden Klammern von Mehrzeilern erscheinen in einer eigenen Zeile.]
# < Stream, auf den das Objekt ausgegeben wurde,
#   entweder als Einzeiler (der Länge <=L-L1-K)
#   oder als Mehrzeiler (mit Newline und LM Spaces statt Space zwischen
#   Teilobjekten), jede Zeile (wenn's geht) der Länge <=L, letzte Zeile
#   (wenn's geht) der Länge <=L-K.
# < Falls der Stream ein PPHELP-Stream ist, enthält er den Modus und eine
#   nichtleere Liste der ausgegebenen Zeilen (in umgekehrter Reihenfolge).

# Eine pr_xxx-Routine bekommt &stream und obj übergeben:
  typedef void pr_routine (const object* stream_, object obj);

# ---------------------- allgemeine Unterprogramme ----------------------------

# UP: Gibt ein unsigned Integer mit max. 32 Bit dezimal auf einen Stream aus.
# pr_uint(&stream,uint);
# > uint: Unsigned Integer
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_uint (const object* stream_, uintL x);
  local void pr_uint(stream_,x)
    var const object* stream_;
    var uintL x;
    { var uintB ziffern[10]; # max. 10 Ziffern, da 0 <= x < 2^32 <= 10^10
      var uintB* ziffptr = &ziffern[0];
      var uintC ziffcount = 0; # Anzahl der Ziffern
      # Ziffern produzieren:
      do { var uintB zif;
           divu_3216_3216(x,10,x=,zif=); # x := floor(x/10), zif := Rest
           *ziffptr++ = zif; ziffcount++; # Ziffer abspeichern
         }
         until (x==0);
      # Ziffern in umgekehrter Reihenfolge ausgeben:
      dotimespC(ziffcount,ziffcount,
        { write_ascii_char(stream_,'0' + *--ziffptr); }
        );
    }

# UP: Gibt ein Nibble hexadezimal (mit 1 Hex-Ziffer) auf einen Stream aus.
# pr_hex1(&stream,x);
# > x: Nibble (>=0,<16)
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_hex1 (const object* stream_, uint4 x);
  local void pr_hex1(stream_,x)
    var const object* stream_;
    var uint4 x;
    { write_ascii_char(stream_, ( x<10 ? '0'+(uintB)x : 'A'+(uintB)x-10 ) ); }

# UP: Gibt ein Byte hexadezimal (mit 2 Hex-Ziffern) auf einen Stream aus.
# pr_hex2(&stream,x);
# > x: Byte
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_hex2 (const object* stream_, uint8 x);
  local void pr_hex2(stream_,x)
    var const object* stream_;
    var uint8 x;
    { pr_hex1(stream_,(uint4)(x>>4)); # Bits 7..4 ausgeben
      pr_hex1(stream_,(uint4)(x & (bit(4)-1))); # Bits 3..0 ausgeben
    }

# UP: Gibt eine Adresse mit 24 Bit hexadezimal (mit 6 Hex-Ziffern)
# auf einen Stream aus.
# pr_hex6(&stream,obj);
# > Adressbits von obj: Unsigned Integer
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_hex6 (const object* stream_, object obj);
  local void pr_hex6(stream_,obj)
    var const object* stream_;
    var object obj;
    { var oint x = (as_oint(obj) >> oint_addr_shift) << addr_shift;
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'x'); # Präfix für "Hexadezimal"
      #define pr_hexpart(k)  # Bits k+7..k ausgeben:  \
        if (((oint_addr_mask>>oint_addr_shift)<<addr_shift) & minus_wbit(k)) \
          { pr_hex2(stream_,(uint8)(x >> k) & (((oint_addr_mask>>oint_addr_shift)<<addr_shift) >> k) & 0xFF); }
      #ifdef WIDE_HARD
      pr_hexpart(56);
      pr_hexpart(48);
      pr_hexpart(40);
      pr_hexpart(32);
      #endif
      pr_hexpart(24);
      pr_hexpart(16);
      pr_hexpart(8);
      pr_hexpart(0);
      #undef pr_hexpart
    }

#ifdef FOREIGN
# UP: Gibt eine Adresse mit 32 Bit hexadezimal (mit 8 Hex-Ziffern)
# auf einen Stream aus.
# pr_hex8(&stream,x);
# > x: Adresse
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_hex8 (const object* stream_, uintP x);
  local void pr_hex8(stream_,x)
    var const object* stream_;
    var uintP x;
    { write_ascii_char(stream_,'#'); write_ascii_char(stream_,'x'); # Präfix für "Hexadezimal"
     {var sintC k = (sizeof(uintP)-1)*8;
      do { pr_hex2(stream_,(uint8)(x >> k)); } while ((k -= 8) >= 0);
    }}
#endif

# *PRINT-READABLY* /= NIL bewirkt u.a. implizit dasselbe wie
# *PRINT-ESCAPE* = T, *PRINT-BASE* = 10, *PRINT-RADIX* = T,
# *PRINT-CIRCLE* = T, *PRINT-LEVEL* = NIL, *PRINT-LENGTH* = NIL,
# *PRINT-GENSYM* = T, *PRINT-ARRAY* = T, *PRINT-CLOSURE* = T.

# Fehlermeldung bei *PRINT-READABLY* /= NIL.
# fehler_print_readably(obj);
  nonreturning_function(local, fehler_print_readably, (object obj));
  local void fehler_print_readably(obj)
    var object obj;
    # (error-of-type 'print-not-readable
    #        "~: Trotz ~ kann ~ nicht wiedereinlesbar ausgegeben werden."
    #        'print '*print-readably* obj
    # )
    { dynamic_bind(S(print_readably),NIL); # *PRINT-READABLY* an NIL binden
      pushSTACK(obj); # Wert für Slot OBJECT von PRINT-NOT-READABLE
      pushSTACK(obj);
      pushSTACK(S(print_readably));
      pushSTACK(S(print));
      fehler(print_not_readable,
             GETTEXT("~: Despite of ~, ~ cannot be printed readably.")
            );
    }

# Fehlermeldung bei unzulässigem Wert von *PRINT-CASE*.
# fehler_print_case();
  nonreturning_function(local, fehler_print_case, (void));
  local void fehler_print_case()
    # (error "~: Der Wert ~ von ~ ist weder ~ noch ~ noch ~.
    #         Er wird auf ~ gesetzt."
    #        'print *print-case* '*print-case* ':upcase ':downcase ':capitalize
    #        ':upcase
    # )
    { var object print_case = S(print_case);
      pushSTACK(Symbol_value(print_case)); # Wert für Slot DATUM von TYPE-ERROR
      pushSTACK(O(type_printcase)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
      pushSTACK(S(Kupcase)); # :UPCASE
      pushSTACK(S(Kcapitalize)); # :CAPITALIZE
      pushSTACK(S(Kdowncase)); # :DOWNCASE
      pushSTACK(S(Kupcase)); # :UPCASE
      pushSTACK(print_case);
      pushSTACK(Symbol_value(print_case));
      pushSTACK(S(print));
      Symbol_value(print_case) = S(Kupcase); # (setq *PRINT-CASE* ':UPCASE)
      fehler(type_error,
             GETTEXT("~: the value ~ of ~ is neither ~ nor ~ nor ~." NLstring
                     "It is reset to ~.")
            );
    }

# Macro: Fragt den Wert von *PRINT-CASE* ab und verzweigt je nachdem.
# switch_print_case(upcase_statement,downcase_statement,capitalize_statement);
  #define switch_print_case(upcase_statement,downcase_statement,capitalize_statement)  \
    {var object print_case = Symbol_value(S(print_case)); # Wert von *PRINT-CASE* \
     if (eq(print_case,S(Kupcase))) # = :UPCASE ?            \
       { upcase_statement }                                  \
     elif (eq(print_case,S(Kdowncase))) # = :DOWNCASE ?      \
       { downcase_statement }                                \
     elif (eq(print_case,S(Kcapitalize))) # = :CAPITALIZE ?  \
       { capitalize_statement }                              \
     else # keines der drei -> Error                         \
       { fehler_print_case(); }                              \
    }

# UP: Gibt einen Teil eines Simple-String elementweise auf einen Stream aus.
# write_sstring_ab(&stream,string,start,len);
# > string: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void write_sstring_ab (const object* stream_, object string, uintL start, uintL len);
  local void write_sstring_ab(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { if (len==0) return;
      pushSTACK(string);
      write_char_array(stream_,&STACK_0,start,len);
      skipSTACK(1);
    }

# UP: Gibt einen Simple-String elementweise auf einen Stream aus.
# write_sstring(&stream,string);
# > string: Simple-String
# > stream: Stream
# < stream: Stream
# can trigger GC
  global void write_sstring (const object* stream_, object string);
  global void write_sstring(stream_,string)
    var const object* stream_;
    var object string;
    { write_sstring_ab(stream_,string,0,Sstring_length(string)); }

# UP: Gibt einen String elementweise auf einen Stream aus.
# write_string(&stream,string);
# > string: String
# > stream: Stream
# < stream: Stream
# can trigger GC
  global void write_string (const object* stream_, object string);
  global void write_string(stream_,string)
    var const object* stream_;
    var object string;
    { if (simple_string_p(string))
        # Simple-String
        { write_sstring(stream_,string); }
        else
        # nicht-simpler String
        { var uintL len = vector_length(string); # Länge
          var uintL offset = 0; # Offset vom String in den Datenvektor
          var object sstring = iarray_displace_check(string,len,&offset); # Datenvektor
          write_sstring_ab(stream_,sstring,offset,len);
        }
    }

# UP: Gibt einen Simple-String je nach (READTABLE-CASE *READTABLE*) und
# *PRINT-CASE* auf einen Stream aus.
# write_sstring_case(&stream,string);
# > string: Simple-String
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void write_sstring_case (const object* stream_, object string);
  local void write_sstring_case(stream_,string)
    var const object* stream_;
    var object string;
    { # (READTABLE-CASE *READTABLE*) abfragen:
      var object readtable;
      get_readtable(readtable = ); # aktuelle Readtable
      switch ((uintW)posfixnum_to_L(TheReadtable(readtable)->readtable_case))
        { case case_upcase:
            # *PRINT-CASE* abfragen. Danach richtet sich, wie Großbuchstaben
            # ausgegeben werden. Kleinbuchstaben werden immer klein ausgegeben.
            switch_print_case(
              # :UPCASE -> Großbuchstaben in Upcase ausgeben:
              { write_sstring(stream_,string); },
              # :DOWNCASE -> Großbuchstaben in Downcase ausgeben:
              do_downcase:
              { var uintL count = Sstring_length(string);
                if (count > 0)
                  { var uintL index = 0;
                    pushSTACK(string); # Simple-String retten
                    SstringDispatch(string,
                      { dotimespL(count,count,
                          { write_code_char(stream_,down_case(TheSstring(STACK_0)->data[index]));
                            index++;
                          });
                      },
                      { dotimespL(count,count,
                          { write_code_char(stream_,down_case(as_chart(TheSmallSstring(STACK_0)->data[index])));
                            index++;
                          });
                      }
                      );
                    skipSTACK(1);
              }   },
              # :CAPITALIZE -> jeweils den ersten Großbuchstaben eines Wortes
              # als Großbuchstaben, alle anderen als Kleinbuchstaben ausgeben.
              # (Vgl. NSTRING_CAPITALIZE in CHARSTRG.D)
              # Erste Version:
              #   (lambda (s &aux (l (length s)))
              #     (prog ((i 0) c)
              #       1 ; Suche ab hier den nächsten Wortanfang
              #         (if (= i l) (return))
              #         (setq c (char s i))
              #         (unless (alphanumericp c) (write-char c) (incf i) (go 1))
              #       ; Wortanfang gefunden
              #       (write-char c) (incf i) ; Großbuchstaben als Großbuchstaben ausgeben
              #       2 ; mitten im Wort
              #         (if (= i l) (return))
              #         (setq c (char s i))
              #         (unless (alphanumericp c) (write-char c) (incf i) (go 1))
              #         (write-char (char-downcase c)) ; Großbuchstaben klein ausgeben
              #         (incf i) (go 2)
              #   ) )
              # Es werden also genau die Zeichen mit char-downcase ausgegeben, vor
              # denen ein alphanumerisches Zeichen auftrat und die selber
              # alphanumerisch sind.
              # [Da alle Uppercase-Characters (nach CLTL S. 236 oben) alphabetisch
              # und damit auch alphanumerisch sind und auf den anderen Characters
              # char-downcase nichts tut: Es werden genau die Zeichen mit
              # char-downcase ausgegeben, vor denen ein alphanumerisches Zeichen
              # auftrat. Wir benutzen dies aber nicht.]
              # Zweite Version:
              #   (lambda (s &aux (l (length s)))
              #     (prog ((i 0) c (flag nil))
              #       1 (if (= i l) (return))
              #         (setq c (char s i))
              #         (let ((newflag (alphanumericp c)))
              #           (when (and flag newflag) (setq c (char-downcase c)))
              #           (setq flag newflag)
              #         )
              #         (write-char c) (incf i) (go 1)
              #   ) )
              # Dritte Version:
              #   (lambda (s &aux (l (length s)))
              #     (prog ((i 0) c (flag nil))
              #       1 (if (= i l) (return))
              #         (setq c (char s i))
              #         (when (and (shiftf flag (alphanumericp c)) flag)
              #           (setq c (char-downcase c))
              #         )
              #         (write-char c) (incf i) (go 1)
              #   ) )
              { var uintL count = Sstring_length(string);
                if (count > 0)
                  { var boolean flag = FALSE;
                    var uintL index = 0;
                    pushSTACK(string); # Simple-String retten
                    SstringDispatch(string,
                      { dotimespL(count,count,
                          { # flag zeigt an, ob gerade innerhalb eines Wortes
                            var boolean oldflag = flag;
                            var chart c = TheSstring(STACK_0)->data[index]; # nächstes Zeichen
                            if ((flag = alphanumericp(c)) && oldflag)
                              # alphanumerisches Zeichen im Wort:
                              { c = down_case(c); } # Groß- in Kleinbuchstaben umwandeln
                            write_code_char(stream_,c); # und ausgeben
                            index++;
                          });
                      },
                      { dotimespL(count,count,
                          { # flag zeigt an, ob gerade innerhalb eines Wortes
                            var boolean oldflag = flag;
                            var chart c = as_chart(TheSmallSstring(STACK_0)->data[index]); # nächstes Zeichen
                            if ((flag = alphanumericp(c)) && oldflag)
                              # alphanumerisches Zeichen im Wort:
                              { c = down_case(c); } # Groß- in Kleinbuchstaben umwandeln
                            write_code_char(stream_,c); # und ausgeben
                            index++;
                          });
                      }
                      );
                    skipSTACK(1);
              }   }
              );
            break;
          case case_downcase:
            # *PRINT-CASE* abfragen. Danach richtet sich, wie Kleinbuchstaben
            # ausgegeben werden. Großbuchstaben werden immer groß ausgegeben.
            switch_print_case(
              # :UPCASE -> Kleinbuchstaben in Upcase ausgeben:
              do_upcase:
              { var uintL count = Sstring_length(string);
                if (count > 0)
                  { var uintL index = 0;
                    pushSTACK(string); # Simple-String retten
                    SstringDispatch(string,
                      { dotimespL(count,count,
                          { write_code_char(stream_,up_case(TheSstring(STACK_0)->data[index]));
                            index++;
                          });
                      },
                      { dotimespL(count,count,
                          { write_code_char(stream_,up_case(as_chart(TheSmallSstring(STACK_0)->data[index])));
                            index++;
                          });
                      }
                      );
                    skipSTACK(1);
              }   },
              # :DOWNCASE -> Kleinbuchstaben in Downcase ausgeben:
              { write_sstring(stream_,string); },
              # :CAPITALIZE -> jeweils den ersten Kleinbuchstaben eines Wortes
              # als Großbuchstaben, alle anderen als Kleinbuchstaben ausgeben.
              # (Vgl. NSTRING_CAPITALIZE in CHARSTRG.D)
              # Erste Version:
              #   (lambda (s &aux (l (length s)))
              #     (prog ((i 0) c)
              #       1 ; Suche ab hier den nächsten Wortanfang
              #         (if (= i l) (return))
              #         (setq c (char s i))
              #         (unless (alphanumericp c) (write-char c) (incf i) (go 1))
              #       ; Wortanfang gefunden
              #       (write-char (char-upcase c)) ; Kleinbuchstaben groß ausgeben
              #       (incf i)
              #       2 ; mitten im Wort
              #         (if (= i l) (return))
              #         (setq c (char s i))
              #         (unless (alphanumericp c) (write-char c) (incf i) (go 1))
              #         (write-char c) ; Kleinbuchstaben klein ausgeben
              #         (incf i) (go 2)
              #   ) )
              # Es werden also genau die Zeichen mit char-upcase ausgegeben, vor
              # denen kein alphanumerisches Zeichen auftrat und die aber selber
              # alphanumerisch sind.
              # Zweite Version:
              #   (lambda (s &aux (l (length s)))
              #     (prog ((i 0) c (flag nil))
              #       1 (if (= i l) (return))
              #         (setq c (char s i))
              #         (when (and (not (shiftf flag (alphanumericp c))) flag)
              #           (setq c (char-upcase c))
              #         )
              #         (write-char c) (incf i) (go 1)
              #   ) )
              { var uintL count = Sstring_length(string);
                if (count > 0)
                  { var boolean flag = FALSE;
                    var uintL index = 0;
                    pushSTACK(string); # Simple-String retten
                    SstringDispatch(string,
                      { dotimespL(count,count,
                          { # flag zeigt an, ob gerade innerhalb eines Wortes
                            var boolean oldflag = flag;
                            var chart c = TheSstring(STACK_0)->data[index]; # nächstes Zeichen
                            if ((flag = alphanumericp(c)) && !oldflag)
                              # alphanumerisches Zeichen am Wortanfang:
                              { c = up_case(c); } # Klein- in Großbuchstaben umwandeln
                            write_code_char(stream_,c); # und ausgeben
                            index++;
                          });
                      },
                      { dotimespL(count,count,
                          { # flag zeigt an, ob gerade innerhalb eines Wortes
                            var boolean oldflag = flag;
                            var chart c = as_chart(TheSmallSstring(STACK_0)->data[index]); # nächstes Zeichen
                            if ((flag = alphanumericp(c)) && !oldflag)
                              # alphanumerisches Zeichen am Wortanfang:
                              { c = up_case(c); } # Klein- in Großbuchstaben umwandeln
                            write_code_char(stream_,c); # und ausgeben
                            index++;
                          });
                      }
                      );
                    skipSTACK(1);
              }   }
              );
            break;
          case case_preserve:
            # *PRINT-CASE* ignorieren.
            write_sstring(stream_,string);
            break;
          case case_invert:
            # *PRINT-CASE* ignorieren.
            { var boolean seen_uppercase = FALSE;
              var boolean seen_lowercase = FALSE;
              var uintL count = Sstring_length(string);
              if (count > 0)
                { SstringDispatch(string,
                    { var const chart* cptr = &TheSstring(string)->data[0];
                      dotimespL(count,count,
                        { var chart c = *cptr++;
                          if (!chareq(c,up_case(c))) { seen_lowercase = TRUE; }
                          if (!chareq(c,down_case(c))) { seen_uppercase = TRUE; }
                        });
                    },
                    { var const scint* cptr = &TheSmallSstring(string)->data[0];
                      dotimespL(count,count,
                        { var chart c = as_chart(*cptr++);
                          if (!chareq(c,up_case(c))) { seen_lowercase = TRUE; }
                          if (!chareq(c,down_case(c))) { seen_uppercase = TRUE; }
                        });
                    }
                    );
                }
              if (seen_uppercase)
                { if (!seen_lowercase) goto do_downcase; }
                else
                { if (seen_lowercase) goto do_upcase; }
              write_sstring(stream_,string);
            }
            break;
          default: NOTREACHED
    }   }

# UP: Gibt eine Anzahl Spaces auf einen Stream aus.
# spaces(&stream,anzahl);
# > anzahl: Anzahl Spaces (Fixnum>=0)
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void spaces (const object* stream_, object anzahl);
  local void spaces(stream_,anzahl)
    var const object* stream_;
    var object anzahl;
    { var uintL count;
      dotimesL(count,posfixnum_to_L(anzahl), { write_ascii_char(stream_,' '); } );
    }

# ------------------- Unterprogramme für Pretty-Print -------------------------

# Variablen:
# ==========

# Zeilenlänge L                  Wert von SYS::*PRIN-LINELENGTH*,
#                                  Fixnum>=0 oder NIL
# Zeilenposition                 im PPHELP-Stream, Fixnum>=0
# Linker Rand L1 für Einzeiler   Wert von SYS::*PRIN-L1*, Fixnum>=0
# Linker Rand LM für Mehrzeiler  Wert von SYS::*PRIN-LM*, Fixnum>=0
# Modus                          im PPHELP-Stream: NIL für Einzeiler,
#                                                  T für Mehrzeiler
  #define einzeiler NIL
  #define mehrzeiler T

# Komponenten eines Pretty-Print-Hilfs-Streams:
#   strm_pphelp_lpos     Line Position (Fixnum>=0)
#   strm_pphelp_strings  nichtleere Liste von Semi-Simple-Strings. Sie
#                        enthalten den bisherigen Output (in umgekehrter
#                        Reihenfolge: letzte Zeile als CAR).
#   strm_pphelp_modus    Modus: Einzeiler, falls nur 1 String vorkommt und
#                        dieser kein NL enthält, sonst Mehrzeiler.
# WRITE-CHAR schiebt sein Character immer nur auf die letzte Zeile
# und aktualisiert lpos und modus.

# während Justify:
# voriger Inhalt des Streams     Werte von SYS::*PRIN-JBSTRINGS*,
#                                  SYS::*PRIN-JBMODUS*, SYS::*PRIN-JBLPOS*
# bisherige Blöcke (Liste von Blöcken,
# mehrzeiliger Block = nichtleere Liste von Semi-Simple-Strings,
# einzeiliger Block = Semi-Simple-String)
#                                Wert von SYS::*PRIN-JBLOCKS*

# für Einhaltung von *PRINT-LEVEL*:
# SYS::*PRIN-LEVEL*              aktuelle Ausgabetiefe (Fixnum>=0)

# für Wiedereinlesbarkeit von Backquote-Expressions:
# SYS::*PRIN-BQLEVEL*            aktuelle Backquote-Tiefe (Fixnum>=0)

# wenn der Printer nach außen verlassen wird:
# SYS::*PRIN-STREAM*             aktueller Stream (Default: NIL),
# um ein rekursives PRINT oder WRITE zu erkennen.

# für Einhaltung von *PRINT-LENGTH*:
# Längenbegrenzung (uintL >=0 oder ~0)      lokal
# bisherige Länge (uintL >=0)               lokal

# für schöne Ausgabe von Klammern:
# *PRINT-RPARS* (T oder NIL) zeigt an, ob Klammern zu in einer extra Zeile
# als "   ) ) )" ausgegeben werden sollen oder nicht.
# SYS::*PRIN-RPAR* = Position der letzten geöffneten Klammer (Fixnum>=0,
#                    oder NIL falls die schließende Klammer ans Zeilenende
#                    und nicht unter die öffnende Klammer soll)

# Unterprogramme:
# ===============

# These work on the stream and must be undone in the right order,
# because they can modify the STACK.

# Return (or *print-right-margin* sys::*prin-linelength*)
  local object right_margin ();
  local object right_margin()
  { var object prm = Symbol_value(S(print_right_margin));
    if (nullp(prm)) return Symbol_value(S(prin_linelength));
    else if (posfixnump(prm)) return prm;
    else if (posbignump(prm)) return fixnum(bit(oint_data_len)-1);
    else
      { pushSTACK(prm); pushSTACK(S(print_right_margin));
        fehler(error,
               GETTEXT("~: must be a positive integer or NIL, not ~")
              );
  }   }

# UP: Fängt in PPHELP-Stream A5 eine neue Zeile an.
# pphelp_newline(&stream);
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pphelp_newline (const object* stream_);
  local void pphelp_newline(stream_)
    var const object* stream_;
    {  # (push (make-ssstring 50) (strm-pphelp-strings stream)) :
       pushSTACK(make_ssstring(50)); # neuer Semi-Simple-String
     { var object new_cons = allocate_cons(); # neues Cons
       Car(new_cons) = popSTACK();
      {var object stream = *stream_;
       Cdr(new_cons) = TheStream(stream)->strm_pphelp_strings;
       TheStream(stream)->strm_pphelp_strings = new_cons;
       # Line-Position := 0, Modus := Mehrzeiler :
       TheStream(stream)->strm_pphelp_lpos = Fixnum_0;
       TheStream(stream)->strm_pphelp_modus = mehrzeiler;
    }}}

# Klammer auf und Klammer zu
# --------------------------
# Korrekt zu schachteln.
  #define KLAMMER_AUF  klammer_auf(stream_);
  #define KLAMMER_ZU   klammer_zu(stream_);

# UP: Gibt eine Klammer '(' auf den Stream aus und merkt sich eventuell
# die Position.
# klammer_auf(&stream);
# > stream: Stream
# < stream: Stream
# verändert STACK
# can trigger GC
  local void klammer_auf (const object* stream_);
  local void klammer_auf(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      if (!(TheStream(stream)->strmtype == strmtype_pphelp))
        # normaler Stream
        { write_ascii_char(stream_,'('); }
        else
        # Pretty-Print-Hilfs-Stream
        { var object pos = # Position für die Klammer zu
            (test_value(S(print_rpars)) # *PRINT-RPARS* /= NIL ?
              ? TheStream(stream)->strm_pphelp_lpos # ja -> aktuelle Position (Fixnum>=0)
              : NIL                                 # nein -> NIL
            );
          dynamic_bind(S(prin_rpar),pos); # SYS::*PRIN-RPAR* daran binden
          write_ascii_char(stream_,'(');
        }
    }

# UP: Gibt eine Klammer ')' auf den Stream aus, evtl. an der gemerkten
# Position.
# klammer_zu(&stream);
# > stream: Stream
# < stream: Stream
# verändert STACK
# can trigger GC
  local void klammer_zu (const object* stream_);
  local void klammer_zu(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      if (!(TheStream(stream)->strmtype == strmtype_pphelp))
        # normaler Stream
        { write_ascii_char(stream_,')'); }
        else
        # Pretty-Print-Hilfs-Stream
        { # gewünschte Position der Klammer zu holen:
          var object pos = Symbol_value(S(prin_rpar)); # SYS::*PRIN-RPAR*
          if (nullp(pos)) goto hinten; # keine -> Klammer hinten ausgeben
          # Klammer an Position pos ausgeben:
          if (eq(TheStream(stream)->strm_pphelp_modus,mehrzeiler)
              && !nullp(Cdr(TheStream(stream)->strm_pphelp_strings))
             )
            # Mehrzeiler mit mehr als einer Zeile ("echter" Mehrzeiler)
            {  # Klammer an die gewünschte Position ausgeben.
               # Dazu Test, ob die letzte Zeile im Stream
               # 1. bis zur gewünschten Position (einschließlich) nur Spaces
               # und
               # 2. sonst nur Spaces und ')' enthält.
               # Wenn ja, Klammer an die gewünschte Position setzen.
               # Wenn nein, neue Zeile anfangen, Spaces und die Klammer ausgeben.
               var object lastline = # letzte Zeile
                 Car(TheStream(stream)->strm_pphelp_strings);
               var uintL len = TheIarray(lastline)->dims[1]; # Länge = Fill-Pointer der Zeile
               var uintL need = posfixnum_to_L(pos) + 1; # nötige Anzahl Spaces
               if (len < need) # Zeile zu kurz ?
                 goto new_line; # ja -> neue Zeile anfangen
               lastline = TheIarray(lastline)->data; # letzte Zeile, Normal-Simple-String
             { var chart* charptr = &TheSstring(lastline)->data[0];
               # Teste, ob need Spaces kommen:
               {var uintL count;
                dotimespL(count,need,
                  { if (!chareq(*charptr++,ascii(' '))) # Space ?
                      goto new_line; # nein -> neue Zeile anfangen
                  });
               }
              {var chart* charptr1 = charptr; # Position merken
               # Teste, ob len-need mal Space oder ')' kommt:
               {var uintL count;
                dotimesL(count,len-need,
                  { var chart c = *charptr++;
                    if (!(chareq(c,ascii(' ')) || chareq(c,ascii(')')))) # Space oder ')' ?
                      goto new_line; # nein -> neue Zeile anfangen
                  });
               }
               # Klammer an die gewünschte Position pos = need-1 setzen:
               *--charptr1 = ascii(')');
            }}}
            else
            # Einzeiler.
            { # Klammer muss wohl hinten ausgegeben werden.
              # Ausnahme: Wenn Line-Position = SYS::*PRIN-LINELENGTH* ist,
              #           würde über die Zeile hinausgeschrieben;
              #           stattdessen wird eine neue Zeile angefangen.
              # Max Right Margin == Line-Position ?
              if (eq(right_margin(),TheStream(stream)->strm_pphelp_lpos))
                { new_line: # neue Zeile anfangen
                  pphelp_newline(stream_); spaces(stream_,pos);
                }
              hinten: # Klammer hinten ausgeben
              write_ascii_char(stream_,')');
            }
          # Bindung von SYS::*PRIN-RPAR* auflösen:
          dynamic_unbind();
    }   }

# Justify
# -------
# Korrekt zu schachteln,
# jeweils 1 mal JUSTIFY_START,
# dann beliebige Ausgaben, durch JUSTIFY_SPACE getrennt,
# dann 1 mal entweder
#     JUSTIFY_END_ENG (fasst auch in Mehrzeilern kurze Blöcke in eine Zeile)
#     oder
#     JUSTIFY_END_WEIT (in Mehrzeilern belegt jeder Block eine eigene Zeile).
  #define JUSTIFY_START     justify_start(stream_);
  #define JUSTIFY_SPACE     justify_space(stream_);
  #define JUSTIFY_END_ENG   justify_end_eng(stream_);
  #define JUSTIFY_END_WEIT  justify_end_weit(stream_);

# UP: Leert einen Pretty-Print-Hilfsstream.
# justify_empty_1(&stream);
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void justify_empty_1 (const object* stream_);
  local void justify_empty_1(stream_)
    var const object* stream_;
    {  pushSTACK(make_ssstring(50)); # neuer Semi-Simple-String
     { var object new_cons = allocate_cons(); # neues Cons
       Car(new_cons) = popSTACK();
       # new_cons = (list (make-ssstring 50))
      {var object stream = *stream_;
       TheStream(stream)->strm_pphelp_strings = new_cons; # neue, leere Zeile
       TheStream(stream)->strm_pphelp_modus = einzeiler; # Modus := Einzeiler
    }}}

# UP: Beginnt einen Justify-Block.
# justify_start(&stream);
# > stream: Stream
# < stream: Stream
# verändert STACK
  local void justify_start (const object* stream_);
  local void justify_start(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      if (!(TheStream(stream)->strmtype == strmtype_pphelp))
        {} # normaler Stream -> nichts zu tun
        else
        # Pretty-Print-Hilfs-Stream
        { # SYS::*PRIN-JBSTRINGS* an den Inhalt des Streams binden:
          dynamic_bind(S(prin_jbstrings),TheStream(stream)->strm_pphelp_strings);
          # SYS::*PRIN-JBMODUS* an den Modus des Streams binden:
          dynamic_bind(S(prin_jbmodus),TheStream(stream)->strm_pphelp_modus);
          # SYS::*PRIN-JBLPOS* an die Line-Position des Streams binden:
          dynamic_bind(S(prin_jblpos),TheStream(stream)->strm_pphelp_lpos);
          # SYS::*PRIN-JBLOCKS* an () binden:
          dynamic_bind(S(prin_jblocks),NIL);
          # Stream leeren:
          justify_empty_1(stream_);
        }
    }

# UP: Leert Inhalt eines Pretty-Print-Hilfsstream aus in die Variable
# SYS::*PRIN-JBLOCKS*.
# justify_empty_2(&stream);
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void justify_empty_2 (const object* stream_);
  local void justify_empty_2(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      var object new_cons;
      # SYS::*PRIN-JBLOCKS* um den Inhalt des Streams erweitern:
      if (eq(TheStream(stream)->strm_pphelp_modus,mehrzeiler))
        # Mehrzeiler.
        { # (push strings SYS::*PRIN-JBLOCKS*)
          new_cons = allocate_cons(); # neues Cons
          Car(new_cons) = TheStream(*stream_)->strm_pphelp_strings;
        }
        else
        # Einzeiler.
        { # (push (first strings) SYS::*PRIN-JBLOCKS*), oder kürzer:
          # (setq SYS::*PRIN-JBLOCKS* (rplacd strings SYS::*PRIN-JBLOCKS*))
          new_cons = TheStream(stream)->strm_pphelp_strings;
        }
      Cdr(new_cons) = Symbol_value(S(prin_jblocks));
      Symbol_value(S(prin_jblocks)) = new_cons;
    }

# UP: Gibt einen Zwischenraum aus, der bei Justify gedehnt werden kann.
# justify_space(&stream);
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void justify_space (const object* stream_);
  local void justify_space(stream_)
    var const object* stream_;
    { if (!(TheStream(*stream_)->strmtype == strmtype_pphelp))
        # normaler Stream -> nur ein Space
        { write_ascii_char(stream_,' '); }
        else
        # Pretty-Print-Hilfs-Stream
        { justify_empty_2(stream_); # Streaminhalt retten
          justify_empty_1(stream_); # Stream leeren
          # Line-Position := SYS::*PRIN-LM* (Fixnum>=0)
          TheStream(*stream_)->strm_pphelp_lpos = Symbol_value(S(prin_lm));
        }
    }

# UP: Beendet einen Justify-Block, bestimmt die Gestalt des Blockes und
# gibt seinen Inhalt auf den alten Stream aus.
# justify_end_eng(&stream);
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void justify_end_eng (const object* stream_);
  local void justify_end_eng(stream_)
    var const object* stream_;
    { if (!(TheStream(*stream_)->strmtype == strmtype_pphelp))
        {} # normaler Stream -> nichts zu tun
        else
        # Pretty-Print-Hilfs-Stream
        { justify_empty_2(stream_); # Streaminhalt retten
          # Streaminhalt restaurieren, d.h. Werte von SYS::*PRIN-JBSTRINGS*,
          # SYS::*PRIN-JBMODUS*, SYS::*PRIN-JBLPOS* in den Stream zurück:
         {var object stream = *stream_;
          # jetzige Line-Position retten:
          pushSTACK(TheStream(stream)->strm_pphelp_lpos);
          # alten Streaminhalt wiederherstellen:
          TheStream(stream)->strm_pphelp_strings = Symbol_value(S(prin_jbstrings));
          TheStream(stream)->strm_pphelp_modus = Symbol_value(S(prin_jbmodus));
          TheStream(stream)->strm_pphelp_lpos = Symbol_value(S(prin_jblpos));
          # (nichtleere) Liste von Blöcken auf den Stream ausgeben:
          pushSTACK(nreverse(Symbol_value(S(prin_jblocks)))); # (nreverse SYS::*PRIN-JBLOCKS*)
          # Die Blöcke werden einzeln ausgegeben. Mehrzeiler werden
          # voneinander und von den Einzeilern durch Newline getrennt.
          # Es werden jedoch möglichst viele aufeinanderfolgende Einzeiler
          # (durch Space getrennt) in eine Zeile gepackt.
          loop # Blockliste STACK_0 durchlaufen:
            { var object block = Car(STACK_0); # nächster Block
              STACK_0 = Cdr(STACK_0); # Blockliste verkürzen
              if (consp(block))
                # Mehrzeiliger Teilblock
                { # Zeilen in die richtige Reihenfolge bringen:
                  block = nreverse(block);
                  # erste Zeile auf den PPHELP-Stream ausgeben:
                  pushSTACK(block);
                  write_string(stream_,Car(block));
                  block = popSTACK();
                  # restliche Zeilen an die Zeilen im Stream vorne dranhängen:
                  stream = *stream_;
                  TheStream(stream)->strm_pphelp_strings =
                    nreconc(Cdr(block),TheStream(stream)->strm_pphelp_strings);
                  # Modus := Mehrzeiler:
                  TheStream(stream)->strm_pphelp_modus = mehrzeiler;
                  if (matomp(STACK_0)) # Restliste leer?
                    # ja -> Line-Position zurück, fertig
                    { TheStream(stream)->strm_pphelp_lpos = STACK_1;
                      break;
                    }
                  # neue Zeile anfangen und weiter:
                  goto new_line;
                }
                else
                # Einzeiliger Teilblock
                { # auf den PPHELP-Stream ausgeben:
                  write_string(stream_,block);
                  if (matomp(STACK_0)) # Restliste leer?
                    break; # ja -> fertig
                  # nächster Block ein Mehrzeiler?
                  block = Car(STACK_0); # nächster Block
                  if (atomp(block)) # ein Mehrzeiler oder Einzeiler?
                    # Es ist ein Einzeiler.
                    # Passt er noch auf dieselbe Zeile,
                    # d.h. ist  Line-Position + 1 + length(Einzeiler) <= L ?
                    { var object linelength = right_margin();
                      if (nullp(linelength) # =NIL -> passt
                          || (posfixnum_to_L(TheStream(*stream_)->strm_pphelp_lpos) # Line-Position
                              + TheIarray(block)->dims[1] # Länge = Fill-Pointer des Einzeilers
                              < posfixnum_to_L(linelength) # < linelength ?
                         )   )
                        # Passt noch.
                        { # Space statt Newline ausgeben:
                          write_ascii_char(stream_,' ');
                        }
                        else
                        # Passt nicht mehr.
                        goto new_line;
                    }
                    else
                    # Mehrzeiler -> neue Zeile und weiter
                    { new_line: # neue Zeile anfangen
                      pphelp_newline(stream_); # neue Zeile, dabei Modus:=Mehrzeiler
                      spaces(stream_,Symbol_value(S(prin_lm))); # SYS::*PRIN-LM* Spaces
                    }
                }
            }
          skipSTACK(2); # leere Restliste und alte Line-Position vergessen
          # Bindungen von JUSTIFY_START rückgängig machen:
          dynamic_unbind();
          dynamic_unbind();
          dynamic_unbind();
          dynamic_unbind();
        }}
    }

# UP: Beendet einen Justify-Block, bestimmt die Gestalt des Blockes und
# gibt seinen Inhalt auf den alten Stream aus.
# justify_end_weit(&stream);
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void justify_end_weit (const object* stream_);
  local void justify_end_weit(stream_)
    var const object* stream_;
    { if (!(TheStream(*stream_)->strmtype == strmtype_pphelp))
        {} # normaler Stream -> nichts zu tun
        else
        # Pretty-Print-Hilfs-Stream
        { justify_empty_2(stream_); # Streaminhalt retten
          # Streaminhalt restaurieren, d.h. Werte von SYS::*PRIN-JBSTRINGS*,
          # SYS::*PRIN-JBMODUS*, SYS::*PRIN-JBLPOS* in den Stream zurück:
         {var object stream = *stream_;
          # jetzige Line-Position retten:
          pushSTACK(TheStream(stream)->strm_pphelp_lpos);
          # alten Streaminhalt wiederherstellen:
          TheStream(stream)->strm_pphelp_strings = Symbol_value(S(prin_jbstrings));
          TheStream(stream)->strm_pphelp_modus = Symbol_value(S(prin_jbmodus));
          TheStream(stream)->strm_pphelp_lpos = Symbol_value(S(prin_jblpos));
          # Prüfe, ob die Blöcke in SYS::*PRIN-JBLOCKS* alle Einzeiler sind:
          {var object blocks = Symbol_value(S(prin_jblocks)); # SYS::*PRIN-JBLOCKS*
           do # (nichtleere) Blockliste durchgehen:
              { if (mconsp(Car(blocks))) # ein Teilblock Mehrzeiler ?
                  goto gesamt_mehrzeiler; # ja -> insgesamt ein Mehrzeiler
                blocks = Cdr(blocks);
              }
              while (consp(blocks));
          }
          # Prüfe, ob die Blöcke in SYS::*PRIN-JBLOCKS*
          # (jeder Block Einzeiler) zusammen einen Einzeiler ergeben können:
          # Ist L=NIL (keine Randbeschränkung) oder
          # L1 + (Gesamtlänge der Blöcke) + (Anzahl der Blöcke-1) <= L ?
          { var object linelength = right_margin();
            if (nullp(linelength)) goto gesamt_einzeiler; # =NIL -> Einzeiler
           {var uintL totalneed = posfixnum_to_L(Symbol_value(S(prin_l1))); # Summe := L1 = SYS::*PRIN-L1*
            var object blocks = Symbol_value(S(prin_jblocks)); # SYS::*PRIN-JBLOCKS*
            do # (nichtleere) Blockliste durchgehen:
               { var object block = Car(blocks); # Block (Einzeiler)
                 totalneed += TheIarray(block)->dims[1] + 1; # dessen Länge+1 dazu
                 blocks = Cdr(blocks);
               }
               while (consp(blocks));
            # totalneed = L1 + (Gesamtlänge der Blöcke) + (Anzahl der Blöcke)
            # Vergleiche dies mit linelength + 1 :
            if (totalneed <= posfixnum_to_L(linelength)+1)
              { goto gesamt_einzeiler; }
              else
              { goto gesamt_mehrzeiler; }
          }}
          gesamt_einzeiler: # Insgesamt ein Einzeiler.
          # Blöcke einzeln, durch Spaces getrennt, auf den Stream ausgeben:
          { pushSTACK(nreverse(Symbol_value(S(prin_jblocks)))); # (nreverse SYS::*PRIN-JBLOCKS*)
            loop # (nichtleere) Blockliste STACK_0 durchlaufen:
              { var object block = Car(STACK_0); # nächster Block
                # (ein Einzeiler, String ohne #\Newline)
                STACK_0 = Cdr(STACK_0); # Blockliste verkürzen
                write_string(stream_,block); # Block auf den Stream ausgeben
                if (matomp(STACK_0)) break; # Restliste leer -> fertig
                write_ascii_char(stream_,' '); # #\Space ausgeben
              }
            goto fertig;
          }
          gesamt_mehrzeiler: # Insgesamt ein Mehrzeiler.
          # Blöcke einzeln, durch Newline getrennt, auf den Stream ausgeben:
          { pushSTACK(nreverse(Symbol_value(S(prin_jblocks)))); # (nreverse SYS::*PRIN-JBLOCKS*)
            loop # (nichtleere) Blockliste STACK_0 durchlaufen:
              { var object block = Car(STACK_0); # nächster Block
                STACK_0 = Cdr(STACK_0); # Blockliste verkürzen
                if (consp(block))
                  # Mehrzeiliger Teilblock
                  { # Zeilen in die richtige Reihenfolge bringen:
                    block = nreverse(block);
                    # erste Zeile auf den PPHELP-Stream ausgeben:
                    pushSTACK(block);
                    write_string(stream_,Car(block));
                    block = popSTACK();
                    # restliche Zeilen an die Zeilen im Stream vorne dranhängen:
                    stream = *stream_;
                    TheStream(stream)->strm_pphelp_strings =
                      nreconc(Cdr(block),TheStream(stream)->strm_pphelp_strings);
                  }
                  else
                  # Einzeiliger Teilblock
                  { # auf den PPHELP-Stream ausgeben:
                    write_string(stream_,block);
                  }
                if (matomp(STACK_0)) break; # Restliste leer?
                pphelp_newline(stream_); # neue Zeile anfangen
                spaces(stream_,Symbol_value(S(prin_lm))); # SYS::*PRIN-LM* Spaces
              }
            stream = *stream_;
            # Line-Position zurück:
            TheStream(stream)->strm_pphelp_lpos = STACK_1;
            # GesamtModus := Mehrzeiler:
            TheStream(stream)->strm_pphelp_modus = mehrzeiler;
            goto fertig;
          }
          fertig: # Line-Position stimmt nun.
          skipSTACK(2); # leere Restliste und alte Line-Position vergessen
          # Bindungen von JUSTIFY_START rückgängig machen:
          dynamic_unbind();
          dynamic_unbind();
          dynamic_unbind();
          dynamic_unbind();
        }}
    }

# Indent
# ------
# Korrekt zu schachteln, jeweils 1 mal INDENT_START und 1 mal INDENT_END.
  #define INDENT_START(delta)  indent_start(stream_,delta);
  #define INDENT_END           indent_end(stream_);

# UP: Bindet die linken Ränder SYS::*PRIN-L1* und SYS::*PRIN-LM* an um
# delta höhere Werte.
# indent_start(&stream,delta);
# > delta: Einrückungswert
# > stream: Stream
# < stream: Stream
# verändert STACK
  local void indent_start (const object* stream_, uintL delta);
  local void indent_start(stream_,delta)
    var const object* stream_;
    var uintL delta;
    { if (!(TheStream(*stream_)->strmtype == strmtype_pphelp))
        {} # normaler Stream -> nichts zu tun
        else
        # Pretty-Print-Hilfs-Stream
        { # SYS::*PRIN-L1* binden:
          {var object new_L1 = fixnum_inc(Symbol_value(S(prin_l1)),delta);
           dynamic_bind(S(prin_l1),new_L1);
          }
          # SYS::*PRIN-LM* binden:
          {var object new_LM = fixnum_inc(Symbol_value(S(prin_lm)),delta);
           dynamic_bind(S(prin_lm),new_LM);
          }
    }   }

# UP: Beendet einen Indent-Block.
# indent_end(&stream);
# > stream: Stream
# < stream: Stream
# verändert STACK
  local void indent_end (const object* stream_);
  local void indent_end(stream_)
    var const object* stream_;
    { if (!(TheStream(*stream_)->strmtype == strmtype_pphelp))
        {} # normaler Stream -> nichts zu tun
        else
        # Pretty-Print-Hilfs-Stream
        { # die beiden Bindungen von INDENT_START auflösen:
          dynamic_unbind();
          dynamic_unbind();
    }   }

# Indent Preparation
# ------------------
# Dient dazu, um eine variable Zeichenzahl einzurücken.
# Korrekt zu schachteln,
#   erst 1 mal INDENTPREP_START,
#   dann einige Zeichen (kein #\Newline!)
#   und dann 1 mal INDENTPREP_END.
# Danach kann sofort mit INDENT_START fortgefahren werden.
  #define INDENTPREP_START  indentprep_start(stream_);
  #define INDENTPREP_END    indentprep_end(stream_);

# UP: Merkt sich die augenblickliche Position.
# indentprep_start(&stream);
# > stream: Stream
# < stream: Stream
# verändert STACK
  local void indentprep_start (const object* stream_);
  local void indentprep_start(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      if (!(TheStream(stream)->strmtype == strmtype_pphelp))
        {} # normaler Stream -> nichts zu tun
        else
        # Pretty-Print-Hilfs-Stream
        { # Line-Position merken:
          pushSTACK(TheStream(stream)->strm_pphelp_lpos);
    }   }

# UP: Subtrahiert die Positionen, liefert die Einrückungsbreite.
# indentprep_end(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: Einrückungsbreite
# verändert STACK
  local uintL indentprep_end (const object* stream_);
  local uintL indentprep_end(stream_)
    var const object* stream_;
    { var object stream = *stream_;
      if (!(TheStream(stream)->strmtype == strmtype_pphelp))
        { return 0; } # normaler Stream -> nichts zu tun
        else
        # Pretty-Print-Hilfs-Stream
        { var uintL lpos_now = # jetzige Line-Position
            posfixnum_to_L(TheStream(stream)->strm_pphelp_lpos);
          var uintL lpos_before = # gemerkte Line-Position
            posfixnum_to_L(popSTACK());
          return (lpos_now>=lpos_before ? lpos_now-lpos_before : 0);
    }   }

# ------------------ Unterprogramme für *PRINT-LEVEL* -------------------------

# Level
# -----
# Korrekt zu schachteln,
# jeweils 1 mal LEVEL_CHECK am Anfang einer pr_xxx-Routine
#     und 1 mal LEVEL_END am Ende.
  #define LEVEL_CHECK  { if (level_check(stream_)) return; }
  #define LEVEL_END    level_end(stream_);

# UP: Gibt die Darstellung eines LISP-Objektes bei Überschreitung von
# *PRINT-LEVEL* aus.
# pr_level(&stream);
# > stream: Stream
# < stream: Stream
# can trigger GC
  #define pr_level(stream_)     write_ascii_char(stream_,'#')

# UP: Testet, ob SYS::*PRIN-LEVEL* den Wert von *PRINT-LEVEL* erreicht hat.
# Wenn ja, nur '#' ausgeben und Rücksprung aus dem aufrufenden UP (!).
# Wenn nein, wird SYS::*PRIN-LEVEL* incrementiert gebunden.
# if (level_check(&stream)) return;
# > stream: Stream
# < stream: Stream
# Wenn ja: kann GC auslösen
# Wenn nein: verändert STACK
  local boolean level_check (const object* stream_);
  local boolean level_check(stream_)
    var const object* stream_;
    { var object level = Symbol_value(S(prin_level)); # SYS::*PRIN-LEVEL*, ein Fixnum >=0
      var object limit = Symbol_value(S(print_level)); # *PRINT-LEVEL*
      if (!test_value(S(print_readably))
          && posfixnump(limit) # Beschränkung vorhanden?
          && (posfixnum_to_L(level) >= posfixnum_to_L(limit)) # und erreicht oder überschritten?
         )
        # ja -> '#' ausgeben und herausspringen:
        { pr_level(stream_); return TRUE; }
        else
        # nein -> *PRINT-LEVEL* noch unerreicht.
        { # binde SYS::*PRIN-LEVEL* an (1+ SYS::*PRIN-LEVEL*) :
          level = fixnum_inc(level,1); # (incf level)
          dynamic_bind(S(prin_level),level);
          return FALSE;
    }   }

# UP: Beendet einen Block mit erhöhtem SYS::*PRIN-LEVEL*.
# level_end(&stream);
# > stream: Stream
# < stream: Stream
# verändert STACK
  local void level_end (const object* stream_);
  local void level_end(stream_)
    var const object* stream_;
    { dynamic_unbind(); }

# ------------------ Unterprogramme für *PRINT-LENGTH* ------------------------

# Length
# ------

# UP: Liefert die Längengrenze für strukturierte Objekte wie z.B. Listen.
# get_print_length()
# < ergebnis: Längengrenze
  local uintL get_print_length (void);
  local uintL get_print_length()
    { var object limit = Symbol_value(S(print_length)); # *PRINT-LENGTH*
      return (!test_value(S(print_readably))
              && posfixnump(limit) # ein Fixnum >=0 ?
              ? posfixnum_to_L(limit) # ja
              : ~(uintL)0             # nein -> Grenze "unendlich"
             );
    }

# ------------------ Unterprogramme für *PRINT-CIRCLE* ------------------------

# UP: Stellt fest, ob ein Objekt wegen *PRINT-CIRCLE* in #n= oder #n# -
# Schreibweise ausgegeben werden muss.
# circle_p(obj)
# > obj: Objekt
# < ergebnis: NULL, falls obj normal auszugeben ist
#      sonst: ergebnis->flag: TRUE, falls obj als #n=... auszugeben ist
#                             FALSE, falls obj als #n# auszugeben ist
#             ergebnis->n: n
#             ergebnis->ptr: Im Fall #n=... ist vor der Ausgabe
#                            das Fixnum *ptr zu incrementieren.
  typedef struct { boolean flag; uintL n; object* ptr; }  circle_info;
  local circle_info* circle_p (object obj);
  local circle_info* circle_p(obj)
    var object obj;
    { # *PRINT-CIRCLE* abfragen:
      if (test_value(S(print_circle)))
        { var object table = Symbol_value(S(print_circle_table)); # SYS::*PRINT-CIRCLE-TABLE*
          if (!simple_vector_p(table)) # sollte ein Simple-Vector sein !
            { bad_table:
              dynamic_bind(S(print_circle),NIL); # *PRINT-CIRCLE* an NIL binden
              pushSTACK(S(print_circle_table)); # SYS::*PRINT-CIRCLE-TABLE*
              pushSTACK(S(print));
              fehler(error,
                     GETTEXT("~: the value of ~ has been arbitrarily altered")
                    );
            }
          # Durch den Vektor table = #(i ...) mit m+1 (0<=i<=m) Elementen
          # durchlaufen:
          # Kommt obj unter den Elementen 1,...,i vor -> Fall FALSE, n:=Index.
          # Kommt obj unter den Elementen i+1,...,m vor -> bringe
          #   obj an die Stelle i+1, Fall TRUE, n:=i+1, nachher i:=i+1.
          # Sonst Fall NULL.
          { local circle_info info; # Platz für die Rückgabe der Werte
            var uintL m1 = Svector_length(table); # Länge m+1
            if (m1==0) goto bad_table; # sollte >0 sein!
           {var object* ptr = &TheSvector(table)->data[0]; # Pointer in den Vektor
            var uintL i = posfixnum_to_L(*ptr++); # erstes Element i
            var uintL index = 1;
            until (index == m1) # Schleife m mal durchlaufen
              { if (eq(*ptr++,obj)) # obj mit nächstem Vektor-Element vergleichen
                  goto found;
                index++;
              }
            # nicht gefunden -> fertig
            goto normal;
            found: # obj als Vektor-Element index gefunden, 1 <= index <= m,
                   # ptr = &TheSvector(table)->data[index+1] .
            if (index <= i)
              # obj ist als #n# auszugeben, n=index.
              { info.flag = FALSE; info.n = index; return &info; }
              else
              # obj an Position i+1 bringen:
              { i = i+1;
                # (rotatef (svref Vektor i) (svref Vektor index)) :
                { var object* ptr_i = &TheSvector(table)->data[i];
                  *--ptr = *ptr_i; *ptr_i = obj;
                }
                # obj ist als #n=... auszugeben, n=i.
                info.flag = TRUE; info.n = i;
                info.ptr = &TheSvector(table)->data[0]; # nachher i im Vektor erhöhen
                return &info;
              }
        } }}
      normal: # obj ist normal auszugeben
        return (circle_info*)NULL;
    }

# UP: Überprüft, ob ein Objekt eine Zirkularität ist, und gibt es in
# diesem Falle als #n# oder mit #n=-Präfix (und sonst normal) aus.
# pr_circle(&stream,obj,&pr_xxx);
# > obj: Objekt
# > pr_xxx: Ausgabe-Routine, die &stream und obj übergeben bekommt
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_circle (const object* stream_, object obj, pr_routine* pr_xxx);
  local void pr_circle(stream_,obj,pr_xxx)
    var const object* stream_;
    var object obj;
    var pr_routine* pr_xxx;
    { # Feststellen, ob Zirkularität:
      var circle_info* info = circle_p(obj);
      if (info == (circle_info*)NULL)
        # keine Zirkularität, obj normal ausgeben:
        { (*pr_xxx)(stream_,obj); }
        else
        # Zirkularität
        if (info->flag)
          # obj als #n=... ausgeben:
          { # erst noch für circle_p das Fixnum im Vektor incrementieren:
            { var object* ptr = info->ptr;
              *ptr = fixnum_inc(*ptr,1);
            }
            { var uintL n = info->n;
              pushSTACK(obj); # obj retten
              # Präfix ausgeben und Einrückungstiefe berechnen:
              INDENTPREP_START;
              write_ascii_char(stream_,'#');
              pr_uint(stream_,n);
              write_ascii_char(stream_,'=');
            }
            { var uintL indent = INDENTPREP_END;
              obj = popSTACK(); # obj zurück
              # obj (eingerückt) ausgeben:
              INDENT_START(indent);
              (*pr_xxx)(stream_,obj);
              INDENT_END;
          } }
          else
          # obj als #n# ausgeben:
          { var uintL n = info->n;
            write_ascii_char(stream_,'#');
            pr_uint(stream_,n);
            write_ascii_char(stream_,'#');
          }
    }

# ------------------------ Entering the printer -------------------------------

# UP: Bindet die Variablen des Printers und ruft dann eine Printer-Routine
# auf.
# pr_enter(&stream,obj,&pr_xxx);
# > obj: Objekt
# > pr_xxx: Ausgabe-Routine, die &stream und obj übergeben bekommt
# > stream: Stream
# < stream: Stream
# can trigger GC
  # Erstmal nur Behandlung von *PRINT-PRETTY* :
  local void pr_enter_1 (const object* stream_, object obj, pr_routine* pr_xxx);
  local void pr_enter_1(stream_,obj,pr_xxx)
    var const object* stream_;
    var object obj;
    var pr_routine* pr_xxx;
    { # Streamtyp (PPHELP-Stream oder nicht) muss zu *PRINT-PRETTY* passen.
      if (test_value(S(print_pretty)))
        # *PRINT-PRETTY* /= NIL.
        { # Falls *stream_ kein PPHELP-Stream ist,
          # muss er durch einen PPHELP-Stream ersetzt werden:
          if (!(TheStream(*stream_)->strmtype == strmtype_pphelp))
            # noch ein normaler Stream
            { dynamic_bind(S(prin_l1),Fixnum_0); # SYS::*PRIN-L1* an 0 binden
              dynamic_bind(S(prin_lm),Fixnum_0); # SYS::*PRIN-LM* an 0 binden
              pushSTACK(obj); # Objekt retten
              # SYS::*PRIN-L1* auf dessen Line-Position setzen:
              {var object linepos = get_line_position(*stream_);
               if (!posfixnump(linepos)) { linepos = Fixnum_0; }
               Symbol_value(S(prin_l1)) = linepos;
              }
              pushSTACK(make_pphelp_stream()); # neuer PPHELP-Stream, Line-Position = 0
              if (stream_get_read_eval(*stream_))
                { TheStream(STACK_0)->strmflags |= bit(strmflags_reval_bit_B); } # READ-EVAL-Bit übernehmen
              # Objekt auf den neuen Stream ausgeben:
              (*pr_xxx)(&STACK_0,STACK_1);
              # Inhalt des neuen Streams auf den alten Stream ausgeben:
              {var object ppstream = popSTACK(); # der neue Stream
               STACK_0 = nreverse(TheStream(ppstream)->strm_pphelp_strings); # Liste von Output-Zeilen
               # Falls es ein Mehrzeiler wurde, der nicht mit einem Newline
               # anfängt, und die alte Line-Position >0 ist,
               # zuerst noch ein Newline auf den alten Stream ausgeben:
               if (eq(TheStream(ppstream)->strm_pphelp_modus,einzeiler) # Einzeiler ?
                   || nullp(Symbol_value(S(pprint_first_newline))))
                 goto skip_first_NL; # in die Schleife
               {var object firststring = Car(STACK_0); # erste Zeile, ein Semi-Simple-String
                if ((TheIarray(firststring)->dims[1] == 0) # leer?
                    || chareq(TheSstring(TheIarray(firststring)->data)->data[0],ascii(NL)) # oder Newline am Anfang?
                   )
                  goto skip_first_NL; # in die Schleife
               }
               if (eq(Symbol_value(S(prin_l1)),Fixnum_0)) # oder ab Position 0 ?
                 goto skip_first_NL; # in die Schleife
              }
              do { write_ascii_char(stream_,NL); # #\Newline als Trennzeichen zwischen den Zeilen
                   skip_first_NL:
                   # nichtleere Stringliste STACK_0 auf den Stream ausgeben:
                  {var object list = STACK_0;
                   STACK_0 = Cdr(list);
                   write_string(stream_,Car(list)); # einzelnen String ausgeben
                 }}
                 while (mconsp(STACK_0));
              skipSTACK(1);
              dynamic_unbind();
              dynamic_unbind();
            }
            else
            # schon ein PPHELP-Stream
            { (*pr_xxx)(stream_,obj); }
        }
        else
        # *PRINT-PRETTY* = NIL.
        { # Falls *stream_ ein PPHELP-Stream ist, muss er durch einen
          # einelementigen Broadcast-Stream ersetzt werden:
          if (!(TheStream(*stream_)->strmtype == strmtype_pphelp))
            # normaler Stream
            { (*pr_xxx)(stream_,obj); }
            else
            # ein PPHELP-Stream
            { pushSTACK(obj);
              pushSTACK(make_broadcast1_stream(*stream_)); # Broadcast-Stream auf den Stream *stream_
              (*pr_xxx)(&STACK_0,STACK_1);
              skipSTACK(2);
            }
        }
    }
  # Dasselbe mit Behandlung von *PRINT-CIRCLE* und *PRINT-PRETTY* :
  local void pr_enter_2 (const object* stream_, object obj, pr_routine* pr_xxx);
  local void pr_enter_2(stream_,obj,pr_xxx)
    var const object* stream_;
    var object obj;
    var pr_routine* pr_xxx;
    { # Falls *PRINT-CIRCLE* /= NIL, in obj nach Zirkularitäten suchen.
      if (test_value(S(print_circle)) || test_value(S(print_readably)))
        # Zirkularitäten suchen:
        { pushSTACK(obj);
         {var object circularities = # Zirkularitätentabelle
            get_circularities(obj,
                              test_value(S(print_array)) || test_value(S(print_readably)), # /= 0 genau dann wenn *PRINT-ARRAY* /= NIL
                              test_value(S(print_closure)) || test_value(S(print_readably)) # /= 0 genau dann wenn *PRINT-CLOSURE* /= NIL
                             );
          obj = popSTACK();
          if (nullp(circularities))
            # Keine Zirkularitäten festgestellt.
            { # Kann *PRINT-CIRCLE* an NIL binden.
              dynamic_bind(S(print_circle),NIL);
              pr_enter_1(stream_,obj,pr_xxx);
              dynamic_unbind();
            }
          elif (eq(circularities,T))
            # Stacküberlauf trat auf
            { # Überlauf der GET_CIRCULARITIES-Routine behandeln:
              dynamic_bind(S(print_circle),NIL); # *PRINT-CIRCLE* an NIL binden
              pushSTACK(S(print));
              fehler(storage_condition,
                     GETTEXT("~: not enough stack space for carrying out circularity analysis")
                    );
            }
          else
            # Zirkularitätenvektor
            { # Binde SYS::*PRINT-CIRCLE-TABLE* an den Simple-Vector:
              dynamic_bind(S(print_circle_table),circularities);
              if (!test_value(S(print_circle)))
                # *PRINT-READABLY* erzwingt *PRINT-CIRCLE* = T
                { dynamic_bind(S(print_circle),T);
                  pr_enter_1(stream_,obj,pr_xxx);
                  dynamic_unbind();
                }
                else
                { pr_enter_1(stream_,obj,pr_xxx); }
              dynamic_unbind();
            }
        }}
        else
        { pr_enter_1(stream_,obj,pr_xxx); }
    }
  # Dasselbe mit Behandlung von *PRINT-CIRCLE* und *PRINT-PRETTY*
  # und SYS::*PRIN-STREAM* :
  local void pr_enter (const object* stream_, object obj, pr_routine* pr_xxx);
  local void pr_enter(stream_,obj,pr_xxx)
    var const object* stream_;
    var object obj;
    var pr_routine* pr_xxx;
    { # Wert von SYS::*PRIN-STREAM* = dieser Stream ?
      if (eq(Symbol_value(S(prin_stream)),*stream_))
        # ja -> rekursiver Aufruf
        { # Falls SYS::*PRINT-CIRCLE-TABLE* = #<UNBOUND> (was bedeutet, dass
          # *PRINT-CIRCLE* vorher NIL war) und jetzt *PRINT-CIRCLE* /= NIL
          # ist, muss Objekt obj nach Zirkularitäten abgesucht werden.
          if (eq(Symbol_value(S(print_circle_table)),unbound))
            { pr_enter_2(stream_,obj,pr_xxx); }
            else
            { pr_enter_1(stream_,obj,pr_xxx); }
        }
        else
        # nein -> nichtrekursiver Aufruf
        {
         #if STACKCHECKP
          var object* STACKbefore = STACK; # STACK aufheben für später
         #endif
          dynamic_bind(S(prin_level),Fixnum_0); # SYS::*PRIN-LEVEL* an 0 binden
          dynamic_bind(S(prin_bqlevel),Fixnum_0); # SYS::*PRIN-BQLEVEL* an 0 binden
          dynamic_bind(S(prin_l1),Fixnum_0); # SYS::*PRIN-L1* an 0 binden (für Pretty-Print)
          dynamic_bind(S(prin_lm),Fixnum_0); # SYS::*PRIN-LM* an 0 binden (für Pretty-Print)
          pr_enter_2(stream_,obj,pr_xxx);
          dynamic_unbind();
          dynamic_unbind();
          dynamic_unbind();
          dynamic_unbind();
         #if STACKCHECKP
          # Überprüfen, ob Stack aufgeräumt:
          if (!(STACK == STACKbefore))
            { abort(); } # wenn nicht, in den Debugger
         #endif
        }
    }

# --------------- Leaving the printer through an external call ----------------

# Vorbereitung des Aufrufs einer externen Print-Funktion
# pr_external_1(stream)
# > stream: Stream
# < ergebnis: Anzahl dynamische Bindungen, die aufzulösen sind.
  local uintC pr_external_1 (object stream);
  local uintC pr_external_1(stream)
    var object stream;
    { var uintC count = 1;
      # *PRINT-CIRCLE* beachten:
      if (!test_value(S(print_circle)))
        # *PRINT-CIRCLE* = NIL ->
        # Für den Fall, dass *PRINT-CIRCLE* an T gebunden werden wird,
        # muss SYS::*PRINT-CIRCLE-TABLE* an #<UNBOUND> gebunden werden
        # (es sei denn, es ist bereits = #<UNBOUND>).
        if (!eq(Symbol_value(S(print_circle_table)),unbound))
          { dynamic_bind(S(print_circle_table),unbound); count++; }
      # *PRINT-READABLY* beachten:
      if (test_value(S(print_readably)))
        { # Damit die benutzerdefinierten Print-Funktionen, die noch nichts
          # von *PRINT-READABLY* wissen, sich trotzdem danach benehmen,
          # binden wir die anderen Printer-Variablen passend:
          # *PRINT-READABLY* erzwingt *PRINT-ESCAPE* = T :
          if (!test_value(S(print_escape)))
            { dynamic_bind(S(print_escape),T); count++; }
          # *PRINT-READABLY* erzwingt *PRINT-BASE* = 10 :
          if (!eq(Symbol_value(S(print_base)),fixnum(10)))
            { dynamic_bind(S(print_base),fixnum(10)); count++; }
          # *PRINT-READABLY* erzwingt *PRINT-RADIX* = T :
          if (!test_value(S(print_radix)))
            { dynamic_bind(S(print_radix),T); count++; }
          # *PRINT-READABLY* erzwingt *PRINT-CIRCLE* = T :
          if (!test_value(S(print_circle)))
            { dynamic_bind(S(print_circle),T); count++; }
          # *PRINT-READABLY* erzwingt *PRINT-LEVEL* = NIL :
          if (test_value(S(print_level)))
            { dynamic_bind(S(print_level),NIL); count++; }
          # *PRINT-READABLY* erzwingt *PRINT-LENGTH* = NIL :
          if (test_value(S(print_length)))
            { dynamic_bind(S(print_length),NIL); count++; }
          # *PRINT-READABLY* erzwingt *PRINT-GENSYM* = T :
          if (!test_value(S(print_gensym)))
            { dynamic_bind(S(print_gensym),T); count++; }
          # *PRINT-READABLY* erzwingt *PRINT-ARRAY* = T :
          if (!test_value(S(print_array)))
            { dynamic_bind(S(print_array),T); count++; }
          # *PRINT-READABLY* erzwingt *PRINT-CLOSURE* = T :
          if (!test_value(S(print_closure)))
            { dynamic_bind(S(print_closure),T); count++; }
        }
      # SYS::*PRIN-STREAM* an stream binden:
      dynamic_bind(S(prin_stream),stream);
      return count;
    }

# Nachbereitung des Aufrufs einer externen Print-Funktion
# pr_external_2(count);
# > count: Anzahl dynamische Bindungen, die aufzulösen sind.
  #define pr_external_2(countvar)  \
    dotimespC(countvar,countvar, { dynamic_unbind(); } );

# ------------------------ Haupt-PRINT-Routine --------------------------------

# Nun kommen die einzelnen pr_xxx-Routinen:
  local pr_routine prin_object;
  local pr_routine prin_object_dispatch;
  local pr_routine pr_symbol;
  local void pr_symbol_part (const object* stream_, object string, boolean case_sensitive);
  local pr_routine pr_like_symbol;
  local pr_routine pr_character;
  local pr_routine pr_string;
  local pr_routine pr_list;
  local pr_routine pr_cons;
  local pr_routine pr_list_quote;
  local pr_routine pr_list_function;
  local pr_routine pr_list_backquote;
  local pr_routine pr_list_splice;
  local pr_routine pr_list_nsplice;
  local pr_routine pr_list_unquote;
  local pr_routine pr_real_number;
  local pr_routine pr_number;
  local pr_routine pr_array_nil;
  local pr_routine pr_bvector;
  local pr_routine pr_vector;
  local pr_routine pr_array;
  local pr_routine pr_instance;
  local pr_routine pr_structure;
  local pr_routine pr_machine;
  local pr_routine pr_system;
  local pr_routine pr_readlabel;
  local pr_routine pr_framepointer;
  local pr_routine pr_orecord;
  local pr_routine pr_subr;
  local pr_routine pr_fsubr;
  local pr_routine pr_closure;
  local pr_routine pr_cclosure;
  local pr_routine pr_cclosure_lang;
  local pr_routine pr_cclosure_codevector;
  local pr_routine pr_stream;

# UP: Gibt ein Objekt auf einen Stream aus.
# prin_object(&stream,obj);
# > obj: Objekt
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void prin_object(stream_,obj)
    var const object* stream_;
    var object obj;
    { restart_it:
      # Auf Tastatur-Interrupt testen:
      interruptp(
        { pushSTACK(obj); # obj retten, stream ist im STACK sicher
          pushSTACK(S(print)); tast_break(); # PRINT ruft Break-Schleife auf
          obj = popSTACK(); # obj zurück
          goto restart_it;
        }
        );
      # auf Stacküberlauf testen:
      check_SP(); check_STACK();
      # Zirkularität behandeln:
      pr_circle(stream_,obj,&prin_object_dispatch);
    }
  local void prin_object_dispatch(stream_,obj)
    var const object* stream_;
    var object obj;
    { # Nach der Typinfo verzweigen:
      #ifdef TYPECODES
      switch (typecode(obj))
        { case_machine: # Maschinenpointer
            pr_machine(stream_,obj); break;
          case_obvector: # Bit/Byte-Vektor
            if (!((Iarray_flags(obj) & arrayflags_atype_mask) == Atype_Bit))
              { pr_vector(stream_,obj); break; } # Byte-Vektor
          case_sbvector: # Bit-Vektor
            pr_bvector(stream_,obj); break;
          case_string: # String
            pr_string(stream_,obj); break;
          case_vector: # (vector t)
            pr_vector(stream_,obj); break;
          case_mdarray: # allgemeiner Array
            pr_array(stream_,obj); break;
          case_closure: # Closure
            pr_closure(stream_,obj); break;
          case_instance: # CLOS-Instanz
            pr_instance(stream_,obj); break;
          #ifdef case_structure
          case_structure: # Structure
            pr_structure(stream_,obj); break;
          #endif
          #ifdef case_stream
          case_stream: # Stream
            pr_stream(stream_,obj); break;
          #endif
          case_orecord: # OtherRecord
            pr_orecord(stream_,obj); break;
          case_char: # Character
            pr_character(stream_,obj); break;
          case_subr: # SUBR
            pr_subr(stream_,obj); break;
          case_system: # Frame-Pointer, Read-Label, System
            if (as_oint(obj) & wbit(0 + oint_addr_shift))
              if (as_oint(obj) & wbit(oint_data_len-1 + oint_addr_shift))
                # System-Pointer
                { pr_system(stream_,obj); }
                else
                # Read-Label
                { pr_readlabel(stream_,obj); }
              else
              # Frame-Pointer
              { pr_framepointer(stream_,obj); }
            break;
          case_number: # Zahl
            pr_number(stream_,obj); break;
          case_symbol: # Symbol
            pr_symbol(stream_,obj); break;
          case_cons: # Cons
            pr_cons(stream_,obj); break;
          default: NOTREACHED
        }
      #else
      if (orecordp(obj)) { pr_orecord(stream_,obj); }
      elif (consp(obj)) { pr_cons(stream_,obj); }
      elif (immediate_number_p(obj)) { pr_number(stream_,obj); }
      elif (charp(obj)) { pr_character(stream_,obj); }
      elif (subrp(obj)) { pr_subr(stream_,obj); }
      elif (machinep(obj)) { pr_machine(stream_,obj); }
      elif (read_label_p(obj)) { pr_readlabel(stream_,obj); }
      elif (systemp(obj)) { pr_system(stream_,obj); }
      else { NOTREACHED }
      #endif
    }


# ------------- PRINT-Routinen für verschiedene Datentypen --------------------

#                      -------- Symbole --------

# UP: Gibt ein Symbol auf einen Stream aus.
# pr_symbol(&stream,sym);
# > sym: Symbol
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_symbol(stream_,sym)
    var const object* stream_;
    var object sym;
    { # *PRINT-ESCAPE* abfragen:
      if (test_value(S(print_escape)) || test_value(S(print_readably)))
        # mit Escape-Zeichen und evtl. Packagenamen:
        { var boolean case_sensitive = FALSE;
          var object curr_pack = get_current_package();
          if (accessiblep(sym,curr_pack))
            # Falls Symbol accessible und nicht verdeckt,
            # keinen Packagenamen und keine Packagemarker ausgeben.
            { case_sensitive = pack_casesensitivep(curr_pack); }
            else
            # Sonst:
            { var object home;
              pushSTACK(sym); # Symbol retten
              if (keywordp(sym)) # Keyword ?
                goto one_marker; # ja -> nur 1 Packagemarker ausgeben
              home = Symbol_package(sym); # Home-package des Symbols
              if (nullp(home))
                # uninterniertes Symbol ausgeben
                { # *PRINT-GENSYM* abfragen:
                  if (test_value(S(print_gensym)) || test_value(S(print_readably)))
                    # Syntax #:name verwenden
                    { write_ascii_char(stream_,'#'); goto one_marker; }
                    # sonst ohne Präfix ausgeben
                }
                else
                # Symbol mit Packagenamen und 1 oder 2 Packagemarkern ausgeben
                { pushSTACK(home); # Home-Package retten
                  pr_symbol_part(stream_,ThePackage(home)->pack_name,FALSE); # Packagenamen ausgeben
                  home = popSTACK(); # Home-Package zurück
                  case_sensitive = pack_casesensitivep(home);
                  if (externalp(STACK_0,home)) # Symbol extern in seiner Home-Package?
                    goto one_marker; # ja -> 1 Packagemarker
                  write_ascii_char(stream_,':'); # sonst 2 Packagemarker
                  one_marker:
                  write_ascii_char(stream_,':');
                }
              sym = popSTACK(); # sym zurück
            }
          pr_symbol_part(stream_,Symbol_name(sym),case_sensitive); # Symbolnamen ausgeben
        }
        else
        # Symbol ohne Escape-Zeichen ausgeben:
        # nur den Symbolnamen unter Kontrolle von *PRINT-CASE* ausgeben
        { write_sstring_case(stream_,Symbol_name(sym)); }
    }

# UP: Gibt einen Symbol-Teil (Packagename oder Symbolname) mit Escape-Zeichen
# aus.
# pr_symbol_part(&stream,string,case_sensitive);
# > string: Simple-String
# > stream: Stream
# > case_sensitive: Flag, ob das Wiedereinlesen case-sensitiv wäre
# < stream: Stream
# can trigger GC
  local void pr_symbol_part(stream_,string,case_sensitive)
    var const object* stream_;
    var object string;
    var boolean case_sensitive;
    { # Feststellen, ob der Name ohne |...| außenrum ausgegeben werden kann:
      # Dies kann er dann, wenn er:
      # 1. nicht leer ist und
      # 2. mit einem Character mit Syntaxcode Constituent anfängt und
      # 3. nur aus Characters mit Syntaxcode Constituent oder
      #    Nonterminating Macro besteht und
      # 4. keine Klein-/Großbuchstaben (je nach readtable_case)
      #    und keine Doppelpunkte enthält und
      # 5. nicht Potential-Number Syntax (mit *PRINT-BASE* als Basis) hat.
      var uintL len = Sstring_length(string); # Länge
      # Bedingung 1 überprüfen:
      if (len==0) goto surround; # Länge=0 -> muss |...| verwenden
      # Bedingungen 2-4 überprüfen:
      { # Brauche die Attributcodetabelle und die aktuelle Syntaxcodetabelle:
        var object syntax_table; # Syntaxcodetabelle, char_code_limit Elemente
        var uintW rtcase; # readtable-case
        { var object readtable;
          get_readtable(readtable = ); # aktuelle Readtable
          syntax_table = TheReadtable(readtable)->readtable_syntax_table;
          rtcase = posfixnum_to_L(TheReadtable(readtable)->readtable_case);
        }
        # String durchlaufen:
        SstringDispatch(string,
          { var const chart* charptr = &TheSstring(string)->data[0];
            var uintL count = len;
            var chart c = *charptr++; # erstes Character
            # sein Syntaxcode soll Constituent sein:
            if (!(syntax_table_get(syntax_table,c) == syntax_constituent))
              goto surround; # nein -> muss |...| verwenden
            loop
              { if (attribute_of(c) == a_pack_m) # Attributcode Package-Marker ?
                  goto surround; # ja -> muss |...| verwenden
                if (!case_sensitive)
                  switch (rtcase)
                    { case case_upcase:
                        if (!chareq(c,up_case(c))) # war c ein Kleinbuchstabe?
                          goto surround; # ja -> muss |...| verwenden
                        break;
                      case case_downcase:
                        if (!chareq(c,down_case(c))) # war c ein Großbuchstabe?
                          goto surround; # ja -> muss |...| verwenden
                        break;
                      case case_preserve:
                        break;
                      case case_invert:
                        break;
                      default: NOTREACHED
                    }
                count--; if (count == 0) break; # String zu Ende -> Schleifenende
                c = *charptr++; # nächstes Character
                switch (syntax_table_get(syntax_table,c)) # sein Syntaxcode
                  { case syntax_constituent:
                    case syntax_nt_macro:
                      break;
                    default: # Syntaxcode /= Constituent, Nonterminating Macro
                      goto surround; # -> muss |...| verwenden
                  }
          }   },
          { var const scint* charptr = &TheSmallSstring(string)->data[0];
            var uintL count = len;
            var chart c = as_chart(*charptr++); # erstes Character
            # sein Syntaxcode soll Constituent sein:
            if (!(syntax_table_get(syntax_table,c) == syntax_constituent))
              goto surround; # nein -> muss |...| verwenden
            loop
              { if (attribute_of(c) == a_pack_m) # Attributcode Package-Marker ?
                  goto surround; # ja -> muss |...| verwenden
                if (!case_sensitive)
                  switch (rtcase)
                    { case case_upcase:
                        if (!chareq(c,up_case(c))) # war c ein Kleinbuchstabe?
                          goto surround; # ja -> muss |...| verwenden
                        break;
                      case case_downcase:
                        if (!chareq(c,down_case(c))) # war c ein Großbuchstabe?
                          goto surround; # ja -> muss |...| verwenden
                        break;
                      case case_preserve:
                        break;
                      case case_invert:
                        break;
                      default: NOTREACHED
                    }
                count--; if (count == 0) break; # String zu Ende -> Schleifenende
                c = as_chart(*charptr++); # nächstes Character
                switch (syntax_table_get(syntax_table,c)) # sein Syntaxcode
                  { case syntax_constituent:
                    case syntax_nt_macro:
                      break;
                    default: # Syntaxcode /= Constituent, Nonterminating Macro
                      goto surround; # -> muss |...| verwenden
                  }
          }   }
          );
      }
      # Bedingung 5 überprüfen:
      { pushSTACK(string); # String retten
        get_buffers(); # zwei Buffer allozieren, in den STACK
        # und füllen:
        SstringDispatch(STACK_2,
          { var uintL index = 0;
            until (index == len)
              { var chart c = TheSstring(STACK_2)->data[index]; # nächstes Character
                ssstring_push_extend(STACK_1,c); # in den Character-Buffer
                ssbvector_push_extend(STACK_0,attribute_of(c)); # und in den Attributcode-Buffer
                index++;
          }   },
          { var uintL index = 0;
            until (index == len)
              { var chart c = as_chart(TheSmallSstring(STACK_2)->data[index]); # nächstes Character
                ssstring_push_extend(STACK_1,c); # in den Character-Buffer
                ssbvector_push_extend(STACK_0,attribute_of(c)); # und in den Attributcode-Buffer
                index++;
          }   }
          );
        O(token_buff_2) = popSTACK(); # Attributcode-Buffer
        O(token_buff_1) = popSTACK(); # Character-Buffer
        string = popSTACK(); # String zurück
        if (test_dots()) goto surround; # nur Punkte -> muss |...| verwenden
        # Potential-Number-Syntax?
        { var uintWL base = get_print_base(); # Wert von *PRINT-BASE*
          var token_info info;
          if (test_potential_number_syntax(&base,&info))
            goto surround; # ja -> muss |...| verwenden
      } }
      # Name kann ohne Escape-Characters ausgegeben werden.
      if (case_sensitive)
        { write_sstring(stream_,string); }
        else
        # Dabei jedoch *PRINT-CASE* beachten:
        { write_sstring_case(stream_,string); }
      return;
      surround: # Namen unter Verwendung der Escape-Character |...| ausgeben:
      { # Syntaxcodetabelle holen:
        { var object readtable;
          get_readtable(readtable = ); # aktuelle Readtable
          pushSTACK(TheReadtable(readtable)->readtable_syntax_table);
        }
        pushSTACK(string);
        # Stackaufbau: syntax_table, string.
        write_ascii_char(stream_,'|');
        SstringDispatch(STACK_0,
          { var uintL index = 0;
            until (index == len)
              { var chart c = TheSstring(STACK_0)->data[index]; # nächstes Character
                switch (syntax_table_get(STACK_1,c)) # dessen Syntaxcode
                  { case syntax_single_esc:
                    case syntax_multi_esc:
                      # Dem Escape-Character c wird ein '\' vorangestellt:
                      write_ascii_char(stream_,'\\');
                    default: ;
                  }
                write_code_char(stream_,c); # Character ausgeben
                index++;
          }   },
          { var uintL index = 0;
            until (index == len)
              { var chart c = as_chart(TheSmallSstring(STACK_0)->data[index]); # nächstes Character
                switch (syntax_table_get(STACK_1,c)) # dessen Syntaxcode
                  { case syntax_single_esc:
                    case syntax_multi_esc:
                      # Dem Escape-Character c wird ein '\' vorangestellt:
                      write_ascii_char(stream_,'\\');
                    default: ;
                  }
                write_code_char(stream_,c); # Character ausgeben
                index++;
          }   }
          );
        write_ascii_char(stream_,'|');
        skipSTACK(2);
      }
    }

# UP: Gibt einen Simple-String wie einen Symbol-Teil aus.
# pr_like_symbol(&stream,string);
# > string: Simple-String
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_like_symbol(stream_,string)
    var const object* stream_;
    var object string;
    { # *PRINT-ESCAPE* abfragen:
      if (test_value(S(print_escape)) || test_value(S(print_readably)))
        { pr_symbol_part(stream_,string,pack_casesensitivep(get_current_package())); } # mit Escape-Zeichen ausgeben
        else
        { write_sstring_case(stream_,string); } # ohne Escape-Zeichen ausgeben
    }

#                      -------- Characters --------

# UP: Gibt ein Character auf einen Stream aus.
# pr_character(&stream,ch);
# > ch: Character
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_character(stream_,ch)
    var const object* stream_;
    var object ch;
    { # *PRINT-ESCAPE* abfragen:
      if (test_value(S(print_escape)) || test_value(S(print_readably)))
        # Character mit Escape-Zeichen ausgeben.
        # Syntax:  # \ char
        # bzw.     # \ charname
        { write_ascii_char(stream_,'#');
          write_ascii_char(stream_,'\\');
         {var chart code = char_code(ch); # Code
          if (as_cint(code) > 0x20 && as_cint(code) < 0x7F)
            # graphic standard character -> don't even lookup the name
            { write_code_char(stream_,code); }
            else
            { var object charname = char_name(code); # Name des Characters
              if (nullp(charname))
                # kein Name vorhanden
                { write_code_char(stream_,code); }
                else
                # Namen (Simple-String) ausgeben
                { write_sstring_case(stream_,charname); }
        }}  }
        else
        # Character ohne Escape-Zeichen ausgeben
        { write_char(stream_,ch); } # ch selbst ausgeben
    }

#                      -------- Strings --------

# UP: Gibt einen Teil eines Simple-String auf einen Stream aus.
# pr_sstring_ab(&stream,string,start,len);
# > string: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_sstring_ab (const object* stream_, object string, uintL start, uintL len);
  local void pr_sstring_ab(stream_,string,start,len)
    var const object* stream_;
    var object string;
    var uintL start;
    var uintL len;
    { # *PRINT-ESCAPE* abfragen:
      if (test_value(S(print_escape)) || test_value(S(print_readably)))
        # mit Escape-Zeichen:
        { var uintL index = start;
          pushSTACK(string); # Simple-String retten
          write_ascii_char(stream_,'"'); # vorher ein Anführungszeichen
          #if 0
          SstringDispatch(string,
            { dotimesL(len,len,
                { var chart c = TheSstring(STACK_0)->data[index]; # nächstes Zeichen
                  # bei c = #\" oder c = #\\ erst noch ein '\' ausgeben:
                  if (chareq(c,ascii('"')) || chareq(c,ascii('\\')))
                    { write_ascii_char(stream_,'\\'); }
                  write_code_char(stream_,c);
                  index++;
                });
            },
            { dotimesL(len,len,
                { var chart c = as_chart(TheSmallSstring(STACK_0)->data[index]); # nächstes Zeichen
                  # bei c = #\" oder c = #\\ erst noch ein '\' ausgeben:
                  if (chareq(c,ascii('"')) || chareq(c,ascii('\\')))
                    { write_ascii_char(stream_,'\\'); }
                  write_code_char(stream_,c);
                  index++;
                });
            }
            );
          #else # dasselbe, etwas optimiert
          SstringDispatch(string,
            { var uintL index0 = index;
              loop
                { # Suche den nächsten #\" oder #\\ :
                  string = STACK_0;
                  while (len > 0)
                    { var chart c = TheSstring(string)->data[index];
                      if (chareq(c,ascii('"')) || chareq(c,ascii('\\')))
                        break;
                      index++; len--;
                    }
                  if (!(index==index0))
                    { write_sstring_ab(stream_,string,index0,index-index0); }
                  if (len==0) break;
                  write_ascii_char(stream_,'\\');
                  index0 = index; index++; len--;
            }   },
            { var uintL index0 = index;
              loop
                { # Suche den nächsten #\" oder #\\ :
                  string = STACK_0;
                  while (len > 0)
                    { var chart c = as_chart(TheSmallSstring(string)->data[index]);
                      if (chareq(c,ascii('"')) || chareq(c,ascii('\\')))
                        break;
                      index++; len--;
                    }
                  if (!(index==index0))
                    { write_sstring_ab(stream_,string,index0,index-index0); }
                  if (len==0) break;
                  write_ascii_char(stream_,'\\');
                  index0 = index; index++; len--;
            }   }
            );
          #endif
          write_ascii_char(stream_,'"'); # nachher ein Anführungszeichen
          skipSTACK(1);
        }
        else
        # ohne Escape-Zeichen: nur write_sstring_ab
        { write_sstring_ab(stream_,string,start,len); }
    }

# UP: Gibt einen String auf einen Stream aus.
# pr_string(&stream,string);
# > string: String
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_string(stream_,string)
    var const object* stream_;
    var object string;
    { var uintL len = vector_length(string); # Länge
      var uintL offset = 0; # Offset vom String in den Datenvektor
      var object sstring = array_displace_check(string,len,&offset); # Datenvektor
      pr_sstring_ab(stream_,sstring,offset,len);
    }

#                    -------- Conses, Listen --------

# UP: Stellt fest, ob ein Cons auf eine spezielle Art und Weise auszugeben
# ist.
# special_list_p(obj)
# > obj: Objekt, ein Cons
# < ergebnis: Adresse der entsprechenden pr_list_xxx-Routine, falls ja,
#             NULL, falls nein.
  local pr_routine* special_list_p (object obj);
  local pr_routine* special_list_p(obj)
    var object obj;
    { # Spezielle Listen sind die der Form
      # (QUOTE a), (FUNCTION a), (SYS::BACKQUOTE a [b]) und
      # (SYS::SPLICE a), (SYS::NSPLICE a), (SYS::UNQUOTE a)
      # falls SYS::*PRIN-BQLEVEL* > 0
      var object head = Car(obj);
      var pr_routine* pr_xxx;
      if (eq(head,S(quote))) # QUOTE
        { pr_xxx = &pr_list_quote; goto test2; }
      elif (eq(head,S(function))) # FUNCTION
        { pr_xxx = &pr_list_function; goto test2; }
      elif (eq(head,S(backquote))) # SYS::BACKQUOTE
        { pr_xxx = &pr_list_backquote;
          # Teste noch, ob obj eine Liste der Länge 2 oder 3 ist.
          obj = Cdr(obj); # Der CDR
          if (consp(obj) && # muss ein Cons sein,
              (obj = Cdr(obj), # der CDDR
               (atomp(obj) ? nullp(obj) : nullp(Cdr(obj))) # NIL oder eine einelementige Liste
             ))
            { return pr_xxx; }
            else
            { return (pr_routine*)NULL; }
        }
      elif (eq(head,S(splice))) # SYS::SPLICE
        { pr_xxx = &pr_list_splice; goto test2bq; }
      elif (eq(head,S(nsplice))) # SYS::NSPLICE
        { pr_xxx = &pr_list_nsplice; goto test2bq; }
      elif (eq(head,S(unquote))) # SYS::UNQUOTE
        { pr_xxx = &pr_list_unquote; goto test2bq; }
      else
        { return (pr_routine*)NULL; }
      test2bq: # Teste noch, ob SYS::*PRIN-BQLEVEL* > 0 und
               # obj eine Liste der Länge 2 ist.
        { var object bqlevel = Symbol_value(S(prin_bqlevel));
          if (!(posfixnump(bqlevel) && !eq(bqlevel,Fixnum_0)))
            { return (pr_routine*)NULL; }
        }
      test2: # Teste noch, ob obj eine Liste der Länge 2 ist.
        if (mconsp(Cdr(obj)) && nullp(Cdr(Cdr(obj))))
          { return pr_xxx; }
          else
          { return (pr_routine*)NULL; }
    }

# UP: Liefert den Wert des Fixnums *PRINT-INDENT-LISTS*.
# get_indent_lists()
# < ergebnis: Fixnum > 0
  local uintL get_indent_lists (void);
  local uintL get_indent_lists()
    { var object obj = Symbol_value(S(print_indent_lists));
      if (posfixnump(obj))
        { var uintL indent = posfixnum_to_L(obj);
          if (indent > 0)
            { return indent; }
        }
      # Defaultwert ist 1.
      return 1;
    }

# UP: Gibt eine Liste auf einen Stream aus, NIL als ().
# pr_list(&stream,list);
# > list: Liste
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_list(stream_,list)
    var const object* stream_;
    var object list;
    { if (nullp(list))
        # NIL als () ausgeben:
        { write_ascii_char(stream_,'('); write_ascii_char(stream_,')'); }
        else
        # ein Cons
        { pr_cons(stream_,list); }
    }

# UP: Gibt ein Cons auf einen Stream aus.
# pr_cons(&stream,list);
# > list: Cons
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_cons(stream_,list)
    var const object* stream_;
    var object list;
    { # Spezialfälle abfangen:
      { var pr_routine* special = special_list_p(list);
        if (!(special == (pr_routine*)NULL))
          { (*special)(stream_,list); # spezielle pr_list_xxx-Routine aufrufen
            return;
      }   }
      LEVEL_CHECK;
      { var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
        var uintL length = 0; # bisherige Länge := 0
        pushSTACK(list); # Liste retten
       {var object* list_ = &STACK_0; # und merken, wo sie sitzt
        KLAMMER_AUF; # '('
        INDENT_START(get_indent_lists()); # um 1 Zeichen einrücken, wegen '('
        JUSTIFY_START;
        # auf Erreichen von *PRINT-LENGTH* prüfen:
        if (length_limit==0) goto dots;
        loop
          { # ab hier den CAR ausgeben
            list = *list_; *list_ = Cdr(list); # Liste verkürzen
            prin_object(stream_,Car(list)); # den CAR ausgeben
            length++; # Länge incrementieren
            # ab hier den Listenrest ausgeben
            if (nullp(*list_)) goto end_of_list; # Listenrest=NIL -> Listenende
            JUSTIFY_SPACE; # ein Space ausgeben
            if (matomp(*list_)) goto dotted_list; # Dotted List ?
            # auf Erreichen von *PRINT-LENGTH* prüfen:
            if (length >= length_limit) goto dots;
            # Prüfen, ob Dotted-List-Schreibweise nötig:
            list = *list_;
            if (!(circle_p(list) == (circle_info*)NULL)) # wegen Zirkularität nötig?
              goto dotted_list;
            if (!(special_list_p(list) == (pr_routine*)NULL)) # wegen QUOTE o.ä. nötig?
              goto dotted_list;
          }
        dotted_list: # Listenrest in Dotted-List-Schreibweise ausgeben:
          write_ascii_char(stream_,'.');
          JUSTIFY_SPACE;
          prin_object(stream_,*list_);
          goto end_of_list;
        dots: # Listenrest durch '...' abkürzen:
          write_ascii_char(stream_,'.');
          write_ascii_char(stream_,'.');
          write_ascii_char(stream_,'.');
          goto end_of_list;
        end_of_list: # Listeninhalt ausgegeben.
        JUSTIFY_END_ENG;
        INDENT_END;
        KLAMMER_ZU;
        skipSTACK(1);
      }}
      LEVEL_END;
    }

# Ausgabe von ...                              als ...
# (quote object)                               'object
# (function object)                            #'object
# (backquote original-form [expanded-form])    `original-form
# (splice (unquote form))                      ,@form
# (splice form)                                ,@'form
# (nsplice (unquote form))                     ,.form
# (nsplice form)                               ,.'form
# (unquote form)                               ,form

  local void pr_list_quote(stream_,list) # list = (QUOTE object)
    var const object* stream_;
    var object list;
    { pushSTACK(Car(Cdr(list))); # (second list) retten
      write_ascii_char(stream_,'\''); # "'" ausgeben
      list = popSTACK();
      INDENT_START(1); # um 1 Zeichen einrücken wegen "'"
      prin_object(stream_,list); # object ausgeben
      INDENT_END;
    }

  local void pr_list_function(stream_,list) # list = (FUNCTION object)
    var const object* stream_;
    var object list;
    { pushSTACK(Car(Cdr(list))); # (second list) retten
      write_ascii_char(stream_,'#'); # "#" ausgeben
      write_ascii_char(stream_,'\''); # "'" ausgeben
      list = popSTACK();
      INDENT_START(2); # um 2 Zeichen einrücken wegen "#'"
      prin_object(stream_,list); # object ausgeben
      INDENT_END;
    }

  local void pr_list_backquote(stream_,list) # list = (BACKQUOTE original-form [expanded-form])
    var const object* stream_;
    var object list;
    { pushSTACK(Car(Cdr(list))); # (second list) retten
      write_ascii_char(stream_,'`'); # '`' ausgeben
      list = popSTACK();
      # SYS::*PRIN-BQLEVEL* um 1 erhöhen:
      {var object bqlevel = Symbol_value(S(prin_bqlevel));
       if (!posfixnump(bqlevel)) { bqlevel = Fixnum_0; }
       dynamic_bind(S(prin_bqlevel),fixnum_inc(bqlevel,1));
      }
      INDENT_START(1); # um 1 Zeichen einrücken wegen '`'
      prin_object(stream_,list); # original-form ausgeben
      INDENT_END;
      dynamic_unbind();
    }

  local void pr_list_bothsplice (const object* stream_, object list, object ch);
  local void pr_list_bothsplice(stream_,list,ch)
    var const object* stream_;
    var object list;
    var object ch;
    # list = (SPLICE object), ch = '@' oder
    # list = (NSPLICE object), ch = '.'
    { pushSTACK(Car(Cdr(list))); # (second list) retten
      write_ascii_char(stream_,','); # Komma ausgeben
      write_char(stream_,ch); # '@' bzw. '.' ausgeben
      list = popSTACK();
      # SYS::*PRIN-BQLEVEL* um 1 verringern:
      dynamic_bind(S(prin_bqlevel),fixnum_inc(Symbol_value(S(prin_bqlevel)),-1));
      # Ist dies von der Form (UNQUOTE form) ?
      if (consp(list) && eq(Car(list),S(unquote))
          && mconsp(Cdr(list)) && nullp(Cdr(Cdr(list)))
         )
        # ja -> noch die Form ausgeben:
        { list = Car(Cdr(list)); # (second object)
          INDENT_START(2); # um 2 Zeichen einrücken wegen ",@" bzw. ",."
          prin_object(stream_,list); # Form ausgeben
          INDENT_END;
        }
        else
        # nein -> noch ein Quote und object ausgeben:
        { pushSTACK(list); # object retten
          write_ascii_char(stream_,'\''); # "'" ausgeben
          list = popSTACK();
          INDENT_START(3); # um 3 Zeichen einrücken wegen ",@'" bzw. ",.'"
          prin_object(stream_,list); # object ausgeben
          INDENT_END;
        }
      dynamic_unbind();
    }

  local void pr_list_splice(stream_,list) # list = (SPLICE object)
    var const object* stream_;
    var object list;
    { pr_list_bothsplice(stream_,list,ascii_char('@')); }

  local void pr_list_nsplice(stream_,list) # list = (NSPLICE object)
    var const object* stream_;
    var object list;
    { pr_list_bothsplice(stream_,list,ascii_char('.')); }

  local void pr_list_unquote(stream_,list) # list = (UNQUOTE object)
    var const object* stream_;
    var object list;
    { pushSTACK(Car(Cdr(list))); # (second list) retten
      write_ascii_char(stream_,','); # ',' ausgeben
      list = popSTACK();
      # SYS::*PRIN-BQLEVEL* um 1 verringern:
      dynamic_bind(S(prin_bqlevel),fixnum_inc(Symbol_value(S(prin_bqlevel)),-1));
      INDENT_START(1); # um 1 Zeichen einrücken wegen ','
      prin_object(stream_,list); # object ausgeben
      INDENT_END;
      dynamic_unbind();
    }

#                      -------- Zahlen --------

# UP: Gibt eine reelle Zahl auf einen Stream aus.
# pr_real_number(&stream,number);
# > number: reelle Zahl
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_real_number(stream_,number)
    var const object* stream_;
    var object number;
    { if (R_rationalp(number))
        # rationale Zahl
        { var uintWL base = get_print_base(); # Wert von *PRINT-BASE*
          # *PRINT-RADIX* abfragen:
          if (test_value(S(print_radix)) || test_value(S(print_readably)))
            # Radix-Specifier ausgeben:
            { pushSTACK(number); # number retten
              switch (base)
                { case 2: # Basis 2
                    write_ascii_char(stream_,'#'); write_ascii_char(stream_,'b'); break;
                  case 8: # Basis 8
                    write_ascii_char(stream_,'#'); write_ascii_char(stream_,'o'); break;
                  case 16: # Basis 16
                    write_ascii_char(stream_,'#'); write_ascii_char(stream_,'x'); break;
                  case 10: # Basis 10
                    if (RA_integerp(number))
                      { # Basis 10 bei Integers durch nachgestellten Punkt
                        # kennzeichnen:
                        skipSTACK(1);
                        print_integer(number,base,stream_);
                        write_ascii_char(stream_,'.');
                        return;
                      }
                  default: # Basis in #nR-Schreibweise ausgeben:
                    write_ascii_char(stream_,'#');
                    pr_uint(stream_,base);
                    write_ascii_char(stream_,'r');
                    break;
                }
              number = popSTACK();
            }
          if (RA_integerp(number))
            # Integer in Basis base ausgeben:
            { print_integer(number,base,stream_); }
            else
            # Ratio in Basis base ausgeben:
            { pushSTACK(TheRatio(number)->rt_den); # Nenner retten
              print_integer(TheRatio(number)->rt_num,base,stream_); # Zähler ausgeben
              write_ascii_char(stream_,'/'); # Bruchstrich
              print_integer(popSTACK(),base,stream_); # Nenner ausgeben
            }
        }
        else
        # Float
        { print_float(number,stream_); }
    }

# UP: Gibt eine Zahl auf einen Stream aus.
# pr_number(&stream,number);
# > number: Zahl
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_number(stream_,number)
    var const object* stream_;
    var object number;
    { if (N_realp(number))
        # reelle Zahl
        { pr_real_number(stream_,number); }
        else
        # komplexe Zahl
        { pushSTACK(number); # Zahl retten
         {var object* number_ = &STACK_0; # und merken, wo sie sitzt
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'C');
          KLAMMER_AUF;
          INDENT_START(3); # um 3 Zeichen einrücken, wegen '#C('
          JUSTIFY_START;
          pr_real_number(stream_,TheComplex(*number_)->c_real); # Realteil ausgeben
          JUSTIFY_SPACE;
          pr_real_number(stream_,TheComplex(*number_)->c_imag); # Imaginärteil ausgeben
          JUSTIFY_END_ENG;
          INDENT_END;
          KLAMMER_ZU;
          skipSTACK(1);
        }}
    }

#            -------- Arrays bei *PRINT-ARRAY*=NIL --------

# UP: Gibt einen Array in Kurzform auf einen Stream aus.
# pr_array_nil(&stream,obj);
# > obj: Array
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_array_nil(stream_,obj)
    var const object* stream_;
    var object obj;
    { pushSTACK(obj); # Array retten
     {var object* obj_ = &STACK_0; # und merken, wo er sitzt
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
      INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
      JUSTIFY_START;
      write_sstring_case(stream_,O(printstring_array)); # "ARRAY" ausgeben
      JUSTIFY_SPACE;
      prin_object_dispatch(stream_,array_element_type(*obj_)); # Elementtyp (Symbol oder Liste) ausgeben
      JUSTIFY_SPACE;
      pr_list(stream_,array_dimensions(*obj_)); # Dimensionsliste ausgeben
      if (array_has_fill_pointer_p(*obj_))
        # Array mit Fill-Pointer -> auch den Fill-Pointer ausgeben:
        { JUSTIFY_SPACE;
          write_sstring_case(stream_,O(printstring_fill_pointer)); # "FILL-POINTER=" ausgeben
          pr_uint(stream_,vector_length(*obj_)); # Länge (=Fill-Pointer) ausgeben
        }
      JUSTIFY_END_ENG;
      INDENT_END;
      write_ascii_char(stream_,'>');
      skipSTACK(1);
    }}

#                    -------- Bit-Vektoren --------

# UP: Gibt einen Teil eines Simple-Bit-Vektors auf einen Stream aus.
# pr_sbvector_ab(&stream,bv,start,len);
# > bv: Simple-Bit-Vektor
# > start: Startindex
# > len: Anzahl der auszugebenden Bits
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_sbvector_ab (const object* stream_, object bv, uintL start, uintL len);
  local void pr_sbvector_ab(stream_,bv,start,len)
    var const object* stream_;
    var object bv;
    var uintL start;
    var uintL len;
    { var uintL index = start;
      pushSTACK(bv); # Simple-Bit-Vektor retten
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'*');
      dotimesL(len,len,
        { write_char(stream_,
                     (sbvector_btst(STACK_0,index) ? ascii_char('1') : ascii_char('0'))
                    );
          index++;
        });
      skipSTACK(1);
    }

# UP: Gibt einen Bit-Vektor auf einen Stream aus.
# pr_bvector(&stream,bv);
# > bv: Bit-Vektor
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_bvector(stream_,bv)
    var const object* stream_;
    var object bv;
    { # *PRINT-ARRAY* abfragen:
      if (test_value(S(print_array)) || test_value(S(print_readably)))
        # bv elementweise ausgeben:
        { var uintL len = vector_length(bv); # Länge
          var uintL offset = 0; # Offset vom Bit-Vektor in den Datenvektor
          var object sbv = array_displace_check(bv,len,&offset); # Datenvektor
          pr_sbvector_ab(stream_,sbv,offset,len);
        }
        else
        # *PRINT-ARRAY* = NIL -> in Kurzform ausgeben:
        { pr_array_nil(stream_,bv); }
    }

#                -------- Allgemeine Vektoren --------

# UP: Gibt einen allgemeinen Vektor auf einen Stream aus.
# pr_vector(&stream,v);
# > v: allgemeiner Vektor
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_vector(stream_,v)
    var const object* stream_;
    var object v;
    { # *PRINT-ARRAY* abfragen:
      if (test_value(S(print_array)) || test_value(S(print_readably)))
        # v elementweise ausgeben:
        { LEVEL_CHECK;
          { var boolean readable = # Flag, ob Länge und Typ mit ausgegeben werden
              (test_value(S(print_readably)) && !general_vector_p(v) ? TRUE : FALSE);
            var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
            var uintL length = 0; # bisherige Länge := 0
            # Vektor elementweise abarbeiten:
            var uintL len = vector_length(v); # Vektor-Länge
            var uintL offset = 0; # Offset vom Vektor in den Datenvektor
            {var object sv = array_displace_check(v,len,&offset); # Datenvektor
             pushSTACK(sv); # Simple-Vektor retten
            }
           {var object* sv_ = &STACK_0; # und merken, wo er sitzt
            var uintL index = 0 + offset; # Startindex = 0 im Vektor
            if (readable)
              { write_ascii_char(stream_,'#'); write_ascii_char(stream_,'A');
                KLAMMER_AUF; # '(' ausgeben
                INDENT_START(3); # um 3 Zeichen einrücken, wegen '#A('
                JUSTIFY_START;
                prin_object_dispatch(stream_,array_element_type(*sv_)); # Elementtyp ausgeben
                JUSTIFY_SPACE;
                pushSTACK(fixnum(len));
                pr_list(stream_,listof(1)); # Liste mit der Länge ausgeben
                JUSTIFY_SPACE;
                KLAMMER_AUF; # '('
                INDENT_START(1); # um 1 Zeichen einrücken, wegen '('
              }
              else
              { write_ascii_char(stream_,'#');
                KLAMMER_AUF; # '('
                INDENT_START(2); # um 2 Zeichen einrücken, wegen '#('
              }
            JUSTIFY_START;
            dotimesL(len,len,
              { # (außer vorm ersten Element) Space ausgeben:
                if (!(length==0)) { JUSTIFY_SPACE; }
                # auf Erreichen von *PRINT-LENGTH* prüfen:
                if (length >= length_limit)
                  { # Rest durch '...' abkürzen:
                    write_ascii_char(stream_,'.');
                    write_ascii_char(stream_,'.');
                    write_ascii_char(stream_,'.');
                    break;
                  }
                # Vektorelement ausgeben:
                prin_object(stream_,storagevector_aref(*sv_,index));
                length++; # Länge incrementieren
                index++; # dann zum nächsten Vektor-Element
              });
            JUSTIFY_END_ENG;
            INDENT_END;
            KLAMMER_ZU;
            if (readable)
              { JUSTIFY_END_ENG;
                INDENT_END;
                KLAMMER_ZU;
              }
            skipSTACK(1);
          }}
          LEVEL_END;
        }
        else
        # *PRINT-ARRAY* = NIL -> in Kurzform ausgeben:
        { pr_array_nil(stream_,v); }
    }

#               -------- Mehrdimensionale Arrays --------

# (defun %print-array (array stream)
#   (let ((rank (array-rank array))
#         (dims (array-dimensions array))
#         (eltype (array-element-type array)))
#     (write-char #\# stream)
#     (if (zerop (array-total-size array))
#       ; wiedereinlesbare Ausgabe von leeren mehrdimensionalen Arrays
#       (progn
#         (write-char #\A stream)
#         (prin1 dims stream)
#       )
#       (progn
#         (let ((*print-base* 10.)) (prin1 rank stream))
#         (write-char #\A stream)
#         (if (and (plusp rank)
#                  (or (eq eltype 'bit) (eq eltype 'character))
#                  (or (null *print-length*) (>= *print-length* (array-dimension array (1- rank))))
#             )
#           ; kürzere Ausgabe von mehrdimensionalen Bit- und Character-Arrays
#           (let* ((lastdim (array-dimension array (1- rank)))
#                  (offset 0)
#                  (sub-array (make-array 0 :element-type eltype :adjustable t)))
#             (labels ((help (dimsr)
#                        (if (null dimsr)
#                          (progn
#                            (prin1
#                              (adjust-array sub-array lastdim :displaced-to array
#                                            :displaced-index-offset offset
#                              )
#                              stream
#                            )
#                            (setq offset (+ offset lastdim))
#                          )
#                          (let ((dimsrr (rest dimsr)))
#                            (write-char #\( stream)
#                            (dotimes (i (first dimsr))
#                              (unless (zerop i) (write-char #\space stream))
#                              (help dimsrr)
#                            )
#                            (write-char #\) stream)
#                     )) ) )
#               (help (nbutlast dims))
#           ) )
#           ; normale Ausgabe von mehrdimensionalen Arrays
#           (let ((indices (make-list rank))) ; Liste von rank Indizes
#             (labels ((help (dimsr indicesr)
#                        (if (null dimsr)
#                          (prin1 (apply #'aref array indices) stream)
#                          (let ((dimsrr (rest dimsr)) (indicesrr (rest indicesr)))
#                            (write-char #\( stream)
#                            (dotimes (i (first dimsr))
#                              (unless (zerop i) (write-char #\space stream))
#                              (rplaca indicesr i)
#                              (help dimsrr indicesrr)
#                            )
#                            (write-char #\) stream)
#                     )) ) )
#               (help dims indices)
#           ) )
#       ) )
# ) ) )

# UP's zur Ausgabe eines Elements bzw. eines Teil-Arrays:
# pr_array_elt_xxx(&stream,obj,&info);
# > obj: Datenvektor
# > info.index: Index des ersten auszugebenden Elements
# > info.count: Anzahl der auszugebenden Elemente
# > stream: Stream
# < stream: Stream
# < info.index: um info.count erhöht
# can trigger GC
  typedef struct { uintL index; uintL count; }  pr_array_info;
  typedef void pr_array_elt_routine (const object* stream_, object obj, pr_array_info* info);
# UP zur Ausgabe eines Elements:
# Bei ihr ist info.count = 1.
  local pr_array_elt_routine pr_array_elt_t;
# Zwei UPs zur Ausgabe eines Teil-Arrays:
  local pr_array_elt_routine pr_array_elt_bvector; # Teilarray ist Bit-Vektor
  local pr_array_elt_routine pr_array_elt_string; # Teilarray ist String

  local void pr_array_elt_t(stream_,obj,info)
    var const object* stream_;
    var object obj; # Simple-Vektor
    var pr_array_info* info;
    { # Element von allgemeinem Typ holen und ausgeben:
      prin_object(stream_,storagevector_aref(obj,info->index));
      info->index++;
    }

  local void pr_array_elt_bvector(stream_,obj,info)
    var const object* stream_;
    var object obj; # Simple-Bit-Vektor
    var pr_array_info* info;
    { # Teil-Bit-Vektor ausgeben:
      pr_sbvector_ab(stream_,obj,info->index,info->count);
      info->index += info->count;
    }

  local void pr_array_elt_string(stream_,obj,info)
    var const object* stream_;
    var object obj; # Simple-String
    var pr_array_info* info;
    { # Teil-String ausgeben:
      pr_sstring_ab(stream_,obj,info->index,info->count);
      info->index += info->count;
    }

# UP: Gibt einen Teil eines Arrays aus.
# pr_array_rekursion(locals,depth);
# > depth: Rekursionstiefe
# > locals: Variablen:
#     *(locals->stream_) :   Stream
#     *(locals->obj_) :      Datenvektor
#     locals->dims_sizes:    Adresse der Tabelle der Dimensionen des Arrays
#                            und ihrer Teilprodukte
#     *(locals->pr_one_elt): Funktion zur Ausgabe eines Elements/Teil-Arrays
#     locals->info:          Parameter für diese Funktion
#     locals->info.index:    Start-Index im Datenvektor
#     locals->length_limit:  Längenbegrenzung
# < locals->info.index: End-Index im Datenvektor
# can trigger GC
  typedef struct { const object* stream_;
                   const object* obj_;
                   const array_dim_size* dims_sizes;
                   pr_array_elt_routine* pr_one_elt;
                   pr_array_info info;
                   uintL length_limit;
                 }
          pr_array_locals;
  local void pr_array_rekursion (pr_array_locals* locals, uintL depth);
  local void pr_array_rekursion(locals,depth)
    var pr_array_locals* locals;
    var uintL depth;
    { check_SP(); check_STACK();
      if (depth==0)
        # Rekursionstiefe 0 -> Rekursionsbasis
        { (*(locals->pr_one_elt)) # Funktion pr_one_elt aufrufen, mit
            (locals->stream_, # Streamadresse,
             *(locals->obj_), # Datenvektor obj,
             &(locals->info) # Infopointer
            ); # als Argumenten
          # Diese Funktion erhöht locals->info.index selbst.
        }
        else
        { depth--; # Rekursionstiefe verkleinern (noch >=0)
         {var const object* stream_ = locals->stream_;
          var uintL length = 0; # bisherige Länge := 0
          var uintL endindex = locals->info.index # Start-Index im Datenvektor
                               + locals->dims_sizes[depth].dimprod # + Dimensionenprodukt
                               ; # liefert den End-Index dieses Teil-Arrays
          var uintL count = locals->dims_sizes[depth].dim;
          KLAMMER_AUF; # '(' ausgeben
          INDENT_START(1); # um 1 Zeichen einrücken, wegen '('
          JUSTIFY_START;
          # Schleife über Dimension (r-depth): jeweils einen Teil-Array ausgeben
          dotimesL(count,count,
            { # (außer vorm ersten Teil-Array) Space ausgeben:
              if (!(length==0)) { JUSTIFY_SPACE; }
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= locals->length_limit)
                { # Rest durch '...' abkürzen:
                  write_ascii_char(stream_,'.');
                  write_ascii_char(stream_,'.');
                  write_ascii_char(stream_,'.');
                  break;
                }
              # Teil-Array ausgeben:
              # (rekursiv, mit verkleinerter depth, und locals->info.index
              # wird ohne weiteres Zutun von einem Aufruf zum nächsten
              # weitergereicht)
              pr_array_rekursion(locals,depth);
              length++; # Länge incrementieren
              # locals->info.index ist schon incrementiert
            });
          JUSTIFY_END_WEIT;
          INDENT_END;
          KLAMMER_ZU; # ')' ausgeben
          locals->info.index = endindex; # jetzt am End-Index angelangt
        }}
    }

# UP: Gibt einen mehrdimensionalen Array auf einen Stream aus.
# pr_array(&stream,obj);
# > obj: mehrdimensionaler Array
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_array(stream_,obj)
    var const object* stream_;
    var object obj;
    { # *PRINT-ARRAY* abfragen:
      if (test_value(S(print_array)) || test_value(S(print_readably)))
        # obj elementweise ausgeben:
        {   LEVEL_CHECK;
         {  # Rang bestimmen und Dimensionen und Teilprodukte holen:
            var uintL r = (uintL)Iarray_rank(obj); # Rang
            var DYNAMIC_ARRAY(dims_sizes,array_dim_size,r); # dynamisch allozierter Array
            iarray_dims_sizes(obj,dims_sizes); # füllen
          { var uintL depth = r; # Tiefe der Rekursion
            var pr_array_locals locals; # lokale Variablen
            var boolean readable = TRUE; # Flag, ob Dimensionen und Typ mit ausgegeben werden
            locals.stream_ = stream_;
            locals.dims_sizes = dims_sizes;
            locals.length_limit = get_print_length(); # Längenbegrenzung
            # Entscheidung über zu verwendende Routine:
            {var uintB atype = Iarray_flags(obj) & arrayflags_atype_mask;
             if ((r>0) && (locals.length_limit >= dims_sizes[0].dim))
               { switch (atype)
                   { case Atype_Bit:
                       # ganze Bitvektoren statt einzelnen Bits ausgeben
                       locals.pr_one_elt = &pr_array_elt_bvector;
                       goto nicht_einzeln;
                     case Atype_Char:
                       # ganze Strings statt einzelnen Characters ausgeben
                       locals.pr_one_elt = &pr_array_elt_string;
                     nicht_einzeln:
                       # Nicht einzelne Elemente, sondern eindimensionale
                       # Teil-Arrays ausgeben.
                       depth--; # dafür depth := r-1
                       locals.info.count = dims_sizes[0].dim; # Dim_r als "Elementarlänge"
                       locals.dims_sizes++; # betrachte nur noch Dim_1, ..., Dim_(r-1)
                       readable = FALSE; # automatisch wiedereinlesbar
                       goto routine_ok;
                     default: ;
               }   }
             locals.pr_one_elt = &pr_array_elt_t;
             locals.info.count = 1; # 1 als "Elementarlänge"
             if (atype==Atype_T)
               { readable = FALSE; } # automatisch wiedereinlesbar
             routine_ok:
             locals.info.index = 0; # Start-Index ist 0
            }
            if (!test_value(S(print_readably)))
              { readable = FALSE; } # braucht nicht wiedereinlesbar zu sein
            pushSTACK(obj); # Array retten
           {var object* obj_ = &STACK_0; # und merken, wo er sitzt
            # Datenvektor holen:
            var uintL size = TheIarray(obj)->totalsize;
            if (size == 0)
              { readable = TRUE; } # sonst weiß man nicht einmal die Dimensionen
            obj = iarray_displace_check(obj,size,&locals.info.index); # Datenvektor
            # locals.info.index = Offset vom Array in den Datenvektor
            pushSTACK(obj); locals.obj_ = &STACK_0; # obj im Stack unterbringen
            # Los geht's.
            if (readable)
              { write_ascii_char(stream_,'#'); write_ascii_char(stream_,'A');
                KLAMMER_AUF; # '(' ausgeben
                INDENT_START(3); # um 3 Zeichen einrücken, wegen '#A('
                JUSTIFY_START;
                prin_object_dispatch(stream_,array_element_type(*obj_)); # Elementtyp (Symbol oder Liste) ausgeben
                JUSTIFY_SPACE;
                pr_list(stream_,array_dimensions(*obj_)); # Dimensionsliste ausgeben
                JUSTIFY_SPACE;
                pr_array_rekursion(&locals,depth); # Array-Elemente ausgeben
                JUSTIFY_END_ENG;
                INDENT_END;
                KLAMMER_ZU; # ')' ausgeben
              }
              else
              { # Erst Präfix #nA ausgeben:
                INDENTPREP_START;
                write_ascii_char(stream_,'#');
                pr_uint(stream_,r); # Rang dezimal ausgeben
                write_ascii_char(stream_,'A');
                {var uintL indent = INDENTPREP_END;
                # Dann die Array-Elemente ausgeben:
                 INDENT_START(indent);
                }
                pr_array_rekursion(&locals,depth);
                INDENT_END;
              }
            skipSTACK(2);
            FREE_DYNAMIC_ARRAY(dims_sizes);
            LEVEL_END;
        }}}}
        else
        # *PRINT-ARRAY* = NIL -> in Kurzform ausgeben:
        { pr_array_nil(stream_,obj); }
    }

#                    -------- CLOS-Instanzen --------

# UP: Gibt eine CLOS-Instanz auf einen Stream aus.
# pr_instance(&stream,obj);
# > obj: auszugebende CLOS-Instanz
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_instance(stream_,obj)
    var const object* stream_;
    var object obj;
    { var object stream = *stream_;
      var uintC count = pr_external_1(stream); # Bindungen erstellen
      # (CLOS:PRINT-OBJECT obj stream) ausführen:
      pushSTACK(obj); pushSTACK(stream); funcall(S(print_object),2);
      pr_external_2(count); # Bindungen auflösen
    }

#                     -------- Structures --------

# (defun %print-structure (structure stream)
#   (let ((name (type-of structure)))
#     (let ((fun (get name 'STRUCTURE-PRINT)))
#       (if fun
#         (funcall fun structure stream *PRIN-LEVEL*)
#         (print-object structure stream)
# ) ) ) )
# (defmethod print-object ((structure structure-object) stream)
#   (print-structure structure stream)
# )
# (defun print-structure (structure stream)
#   (let ((description (get name 'DEFSTRUCT-DESCRIPTION)))
#     (if description
#       (let ((readable (svref description 2)))
#         (write-string (if readable "#S(" "#<") stream)
#         (prin1 name stream)
#         (dolist (slot (svref description 3))
#           (when (first slot)
#             (write-char #\space stream)
#             (prin1 (intern (symbol-name (first slot)) *KEYWORD-PACKAGE*) stream)
#             (write-char #\space stream)
#             (prin1 (%structure-ref name structure (second slot)) stream)
#         ) )
#         (write-string (if readable ")" ">") stream)
#       )
#       (progn
#         (write-string "#<" stream)
#         (prin1 name stream)
#         (do ((l (%record-length structure))
#              (i 1 (1+ i)))
#             ((>= i l))
#           (write-char #\space stream)
#           (prin1 (%structure-ref name structure i) stream)
#         )
#         (write-string ">" stream)
# ) ) ) )

# UP: Aufruf einer (externen) Print-Funktion für Structures
# pr_structure_external(&stream,structure,function);
# > stream: Stream
# > structure: Structure
# > function: Print-Funktion für Structures dieses Typs
# can trigger GC
  local void pr_structure_external (const object* stream_, object structure, object function);
  local void pr_structure_external(stream_,structure,function)
    var const object* stream_;
    var object structure;
    var object function;
    { var object stream = *stream_;
      var uintC count = pr_external_1(stream); # Bindungen erstellen
      # (funcall fun Structure Stream SYS::*PRIN-LEVEL*) :
      pushSTACK(structure); # Structure als 1. Argument
      pushSTACK(stream); # Stream als 2. Argument
      pushSTACK(Symbol_value(S(prin_level))); # Wert von SYS::*PRIN-LEVEL* als 3. Argument
      funcall(function,3);
      pr_external_2(count); # Bindungen auflösen
    }

# UP: Gibt eine Structure auf einen Stream aus.
# pr_structure(&stream,structure);
# > structure: Structure
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_structure(stream_,structure)
    var const object* stream_;
    var object structure;
    { LEVEL_CHECK;
      # Typ der Structure bestimmen (vgl. TYPE-OF):
      { var object name = Car(TheStructure(structure)->structure_types);
        # name = (car '(name_1 ... name_i-1 name_i)) = name_1.
        # (GET name 'SYS::STRUCTURE-PRINT) ausführen:
        var object fun = get(name,S(structure_print));
        if (!eq(fun,unbound))
          # vorgegebene Print-Funktion aufrufen:
          { pr_structure_external(stream_,structure,fun); }
          else
          # keine vorgegebene Print-Funktion gefunden.
          # CLOS:PRINT-OBJECT aufrufen:
          { pr_instance(stream_,structure); }
      }
      LEVEL_END;
    }

# UP: Gibt eine Structure auf einen Stream aus.
# pr_structure_default(&stream,structure);
# > structure: Structure
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_structure_default (const object* stream_, object structure);
  local void pr_structure_default(stream_,structure)
    var const object* stream_;
    var object structure;
    { var object name = Car(TheStructure(structure)->structure_types);
      # name = (car '(name_1 ... name_i-1 name_i)) = name_1.
      pushSTACK(structure);
      pushSTACK(name);
     {var object* structure_ = &STACK_1;
      # Es ist *(structure_ STACKop 0) = structure
      # und    *(structure_ STACKop -1) = name .
      # (GET name 'SYS::DEFSTRUCT-DESCRIPTION) ausführen:
      var object description = get(name,S(defstruct_description));
      if (!eq(description,unbound))
        # Structure mit Slot-Namen ausgeben:
        { pushSTACK(description);
          # Stackaufbau: structure, name, description.
          # description muss ein Simple-Vector der Länge >=4 sein !
          if (!(simple_vector_p(description)
                && (Svector_length(description) >= 4)
             ) )
            { bad_description:
              pushSTACK(S(defstruct_description));
              pushSTACK(S(print));
              fehler(error,
                     GETTEXT("~: bad ~")
                    );
            }
         {var boolean readable = # TRUE falls (svref description 2) /= NIL
            !nullp(TheSvector(description)->data[2]);
          if (readable)
            # Structure wiedereinlesbar ausgeben:
            { write_ascii_char(stream_,'#'); write_ascii_char(stream_,'S');
              KLAMMER_AUF;
              INDENT_START(3); # um 3 Zeichen einrücken, wegen '#S('
            }
            else
            # Structure nicht wiedereinlesbar ausgeben:
            { if (test_value(S(print_readably))) { fehler_print_readably(*structure_); }
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            }
          JUSTIFY_START;
          prin_object(stream_,*(structure_ STACKop -1)); # name ausgeben
          pushSTACK(TheSvector(*(structure_ STACKop -2))->data[3]);
          # Slot-Liste STACK_0 = (svref description 3) durchlaufen:
          { var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
            var uintL length = 0; # bisherige Länge := 0
            while (mconsp(STACK_0))
              { var object slot = STACK_0;
                STACK_0 = Cdr(slot); # Liste verkürzen
                slot = Car(slot); # ein einzelner slot
                if (!(simple_vector_p(slot)
                      && (Svector_length(slot) >= 7)
                   ) )
                  goto bad_description; # sollte ein ds-slot sein
                if (!nullp(TheSvector(slot)->data[0])) # Slot #(NIL ...) übergehen
                  { pushSTACK(slot); # Slot retten
                    JUSTIFY_SPACE; # Space ausgeben
                    # auf Erreichen von *PRINT-LENGTH* prüfen:
                    if (length >= length_limit)
                      { # Rest durch '...' abkürzen:
                        write_ascii_char(stream_,'.');
                        write_ascii_char(stream_,'.');
                        write_ascii_char(stream_,'.');
                        skipSTACK(1); # slot vergessen
                        break;
                      }
                   {var object* slot_ = &STACK_0; # da sitzt der Slot
                    JUSTIFY_START;
                    write_ascii_char(stream_,':'); # Keyword-Kennzeichen
                    {var object obj = TheSvector(*slot_)->data[0]; # (ds-slot-name slot)
                     if (!symbolp(obj)) goto bad_description; # sollte ein Symbol sein
                     pr_like_symbol(stream_,Symbol_name(obj)); # Symbolnamen der Komponente ausgeben
                    }
                    JUSTIFY_SPACE;
                    # (SYS::%%STRUCTURE-REF name Structure (ds-slot-offset slot)) ausführen:
                    pushSTACK(*(structure_ STACKop -1)); # name als 1. Argument
                    pushSTACK(*(structure_ STACKop 0)); # Structure als 2. Argument
                    pushSTACK(TheSvector(*slot_)->data[2]); # (ds-slot-offset slot) als 3. Argument
                    funcall(L(pstructure_ref),3);
                    prin_object(stream_,value1); # Komponente ausgeben
                    JUSTIFY_END_ENG;
                    skipSTACK(1); # slot vergessen
              }   }}
          }
          skipSTACK(1);
          JUSTIFY_END_ENG;
          if (readable) # Beendung der Fallunterscheidung von oben
            { INDENT_END;
              KLAMMER_ZU;
            }
            else
            { INDENT_END;
              write_ascii_char(stream_,'>');
            }
          skipSTACK(3);
        }}
        else
        # Structure elementweise, ohne Komponenten-Namen ausgeben.
        { if (test_value(S(print_readably))) { fehler_print_readably(*structure_); }
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
          INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
          JUSTIFY_START;
          prin_object(stream_,*(structure_ STACKop -1)); # name ausgeben
         {var uintC len = Structure_length(*structure_); # Länge der Structure (>=1)
          var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
          var uintL length = 0; # Index = bisherige Länge := 0
          dotimesC(len,len-1,
            { JUSTIFY_SPACE; # Space ausgeben
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= length_limit)
                { # Rest durch '...' abkürzen:
                  write_ascii_char(stream_,'.');
                  write_ascii_char(stream_,'.');
                  write_ascii_char(stream_,'.');
                  break;
                }
              length++; # Index erhöhen
              # Komponente ausgeben:
              prin_object(stream_,TheStructure(*structure_)->recdata[length]);
            });
          JUSTIFY_END_ENG;
          INDENT_END;
          write_ascii_char(stream_,'>');
          skipSTACK(2);
        }}
    }}

# Das ist die Default-Funktion, die von CLOS:PRINT-OBJECT aufgerufen wird:
LISPFUNN(print_structure,2)
  { # Stackaufbau: structure, stream.
    var object structure = STACK_1;
    if (!structurep(structure))
      { pushSTACK(structure); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(structure_object)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(structure); # structure
        pushSTACK(TheSubr(subr_self)->name); # Funktionsname
        fehler(type_error,
               GETTEXT("~: ~ is not a structure")
              );
      }
    if (!streamp(STACK_0)) { fehler_stream(STACK_0); }
    pr_enter(&STACK_0,structure,&pr_structure_default);
    skipSTACK(2);
    value1 = NIL; mv_count=1;
  }

#                 -------- Maschinenpointer --------

# UP: Gibt einen Objekt #<BLABLA #x......> auf einen Stream aus.
# pr_hex6_obj(&stream,obj,string);
# > obj: Objekt
# > string: Simple-String "BLABLA"
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_hex6_obj (const object* stream_, object obj, object string);
  local void pr_hex6_obj(stream_,obj,string)
    var const object* stream_;
    var object obj;
    var object string;
    { pushSTACK(string); # String retten
     {var object* string_ = &STACK_0; # und merken, wo er sitzt
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
      INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
      JUSTIFY_START;
      write_sstring_case(stream_,*string_); # String ausgeben
      JUSTIFY_SPACE;
      pr_hex6(stream_,obj); # obj als Adresse ausgeben
      JUSTIFY_END_ENG;
      INDENT_END;
      write_ascii_char(stream_,'>');
      skipSTACK(1);
    }}

# UP: Gibt einen Maschinenpointer auf einen Stream aus.
# pr_machine(&stream,obj);
# > obj: Maschinenpointer
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_machine(stream_,obj)
    var const object* stream_;
    var object obj;
    { # #<ADDRESS #x...>
      if (test_value(S(print_readably))) { fehler_print_readably(obj); }
      pr_hex6_obj(stream_,obj,O(printstring_address));
    }

#        -------- Frame-Pointer, Read-Label, System --------

# UP: Gibt einen Systempointer auf einen Stream aus.
# pr_system(&stream,obj);
# > obj: Systempointer
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_system(stream_,obj)
    var const object* stream_;
    var object obj;
    { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
      if (eq(obj,unbound)) # #<UNBOUND>
        { write_sstring_case(stream_,O(printstring_unbound)); }
      elif (eq(obj,specdecl)) # #<SPECIAL REFERENCE>
        { write_sstring_case(stream_,O(printstring_special_reference)); }
      elif (eq(obj,disabled)) # #<DISABLED POINTER>
        { write_sstring_case(stream_,O(printstring_disabled_pointer)); }
      elif (eq(obj,dot_value)) # #<DOT>
        { write_sstring_case(stream_,O(printstring_dot)); }
      elif (eq(obj,eof_value)) # #<END OF FILE>
        { write_sstring_case(stream_,O(printstring_eof)); }
      else # #<SYSTEM-POINTER #x...>
        { pr_hex6_obj(stream_,obj,O(printstring_system)); }
    }

# UP: Gibt ein Read-Label auf einen Stream aus.
# pr_readlabel(&stream,obj);
# > obj: Read-Label
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_readlabel(stream_,obj)
    var const object* stream_;
    var object obj;
    { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
      # #<READ-LABEL ...>
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
      INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
      JUSTIFY_START;
      write_sstring_case(stream_,O(printstring_read_label)); # "READ-LABEL"
      JUSTIFY_SPACE;
      #ifdef TYPECODES
      pr_uint(stream_,(as_oint(obj) >> (oint_addr_shift+1)) & (bit(oint_data_len-2)-1)); # Bits 21..0 dezimal ausgeben
      #else
      pr_uint(stream_,(as_oint(obj) >> oint_addr_shift) & (bit(oint_data_len)-1)); # Bits dezimal ausgeben
      #endif
      JUSTIFY_END_ENG;
      INDENT_END;
      write_ascii_char(stream_,'>');
    }

# UP: Gibt einen Framepointer auf einen Stream aus.
# pr_framepointer(&stream,obj);
# > obj: Frame-Pointer
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_framepointer(stream_,obj)
    var const object* stream_;
    var object obj;
    { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
      # #<FRAME-POINTER #x...>
      pr_hex6_obj(stream_,obj,O(printstring_frame_pointer));
    }

#                        -------- Records --------

# UP: Gibt den Rest eines Record aus. Nur innerhalb eines JUSTIFY-Blocks!
# Die Ausgabe fängt im Normalfall mit einem JUSTIFY_SPACE an.
# pr_record_ab(&stream,&obj,start,now);
# > obj: Record
# > start: Startindex
# > now: Anzahl der bereits ausgegebenen Items (für *PRINT-LENGTH*)
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_record_ab (const object* stream_, const object* obj_, uintL index, uintL length);
  local void pr_record_ab(stream_,obj_,index,length)
    var const object* stream_;
    var const object* obj_;
    var uintL index;
    var uintL length;
    { var uintL len = Record_length(*obj_); # Länge des Record
      var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
      loop
        { if (index >= len) break; # Index >= Recordlänge -> fertig
          JUSTIFY_SPACE; # Space ausgeben
          # auf Erreichen von *PRINT-LENGTH* prüfen:
          if (length >= length_limit)
            { # Rest durch '...' abkürzen:
              write_ascii_char(stream_,'.');
              write_ascii_char(stream_,'.');
              write_ascii_char(stream_,'.');
              break;
            }
          # Komponente ausgeben:
          prin_object(stream_,TheRecord(*obj_)->recdata[index]);
          length++; # bisherige Länge erhöhen
          index++; # zur nächsten Komponente
        }
    }

# UP: Gibt eine Liste als Rest eines Record aus.
# Nur innerhalb eines JUSTIFY-Blocks!
# Die Ausgabe fängt im Normalfall mit einem JUSTIFY_SPACE an.
# pr_record_rest(&stream,obj,now);
# > obj: Liste
# > now: Anzahl der bereits ausgegebenen Items (für *PRINT-LENGTH*)
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_record_rest (const object* stream_, object obj, uintL length);
  local void pr_record_rest(stream_,obj,length)
    var const object* stream_;
    var object obj;
    var uintL length;
    { var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
      pushSTACK(obj);
      while (mconsp(STACK_0))
        { JUSTIFY_SPACE; # Space ausgeben
          # auf Erreichen von *PRINT-LENGTH* prüfen:
          if (length >= length_limit)
            { # Rest durch '...' abkürzen:
              write_ascii_char(stream_,'.');
              write_ascii_char(stream_,'.');
              write_ascii_char(stream_,'.');
              break;
            }
          {var object list = STACK_0;
           STACK_0 = Cdr(list); # Liste verkürzen
           prin_object(stream_,Car(list)); # Element der Liste ausgeben
          }
          length++; # Länge incrementieren
        }
      skipSTACK(1);
    }

# UP: Gibt einen OtherRecord mit Slotnamen auf einen Stream aus.
# pr_record_descr(&stream,obj,name,readable,slotlist);
# > obj: OtherRecord
# > name: Struktur-Name
# > readable: Flag, ob wiedereinlesbar auszugeben
# > slotlist: Liste ((slotname . accessor) ...)
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_record_descr (const object* stream_, object obj, object name, boolean readable, object slotlist);
  local void pr_record_descr(stream_,obj,name,readable,slotlist)
    var const object* stream_;
    var object obj;
    var object name;
    var boolean readable;
    var object slotlist;
    { LEVEL_CHECK;
      pushSTACK(obj);
      pushSTACK(name);
      pushSTACK(slotlist);
      # Stackaufbau: obj, name, slotlist.
     {var object* obj_ = &STACK_2;
      # Es ist *(obj_ STACKop 0) = obj
      # und    *(obj_ STACKop -1) = name
      # und    *(obj_ STACKop -2) = slotlist .
      if (readable)
        # obj wiedereinlesbar ausgeben:
        { write_ascii_char(stream_,'#'); write_ascii_char(stream_,'S');
          KLAMMER_AUF;
          INDENT_START(3); # um 3 Zeichen einrücken, wegen '#S('
        }
        else
        # obj nicht wiedereinlesbar ausgeben:
        { if (test_value(S(print_readably))) { fehler_print_readably(STACK_2); }
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
          INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
        }
      JUSTIFY_START;
      prin_object(stream_,*(obj_ STACKop -1)); # name ausgeben
      pushSTACK(*(obj_ STACKop -2));
      # Slot-Liste STACK_0 = (svref description 3) durchlaufen:
      { var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
        var uintL length = 0; # bisherige Länge := 0
        while (mconsp(STACK_0))
          { {var object slotlistr = STACK_0;
             STACK_0 = Cdr(slotlistr); # Liste verkürzen
             pushSTACK(Car(slotlistr)); # ein einzelner slot
            }
            JUSTIFY_SPACE; # Space ausgeben
            # auf Erreichen von *PRINT-LENGTH* prüfen:
            if (length >= length_limit)
              { # Rest durch '...' abkürzen:
                write_ascii_char(stream_,'.');
                write_ascii_char(stream_,'.');
                write_ascii_char(stream_,'.');
                skipSTACK(1); # slot vergessen
                break;
              }
           {var object* slot_ = &STACK_0; # da sitzt der Slot
            JUSTIFY_START;
            write_ascii_char(stream_,':'); # Keyword-Kennzeichen
            # (first slot) sollte ein Symbol sein
            pr_like_symbol(stream_,Symbol_name(Car(*slot_))); # Symbolnamen der Komponente ausgeben
            JUSTIFY_SPACE;
            pushSTACK(*(obj_ STACKop 0)); # obj als Argument
            funcall(Cdr(*slot_),1); # accessor aufrufen
            prin_object(stream_,value1); # Komponente ausgeben
            JUSTIFY_END_ENG;
            skipSTACK(1); # slot vergessen
      }   }}
      skipSTACK(1);
      JUSTIFY_END_ENG;
      if (readable) # Beendung der Fallunterscheidung von oben
        { INDENT_END;
          KLAMMER_ZU;
        }
        else
        { INDENT_END;
          write_ascii_char(stream_,'>');
        }
      skipSTACK(3);
      LEVEL_END;
    }}

# UP: Gibt einen OtherRecord auf einen Stream aus.
# pr_orecord(&stream,obj);
# > obj: OtherRecord
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_orecord(stream_,obj)
    var const object* stream_;
    var object obj;
    { switch (Record_type(obj))
        {
          #ifndef TYPECODES
          case Rectype_bvector: # Bit/Byte-Vektor
            if (!((Iarray_flags(obj) & arrayflags_atype_mask) == Atype_Bit))
              { pr_vector(stream_,obj); break; } # Byte-Vektor
          case Rectype_Sbvector: # Bit-Vektor
            pr_bvector(stream_,obj); break;
          case Rectype_string: case Rectype_Sstring: case Rectype_Imm_Sstring: case Rectype_Imm_SmallSstring: # String
            pr_string(stream_,obj); break;
          case Rectype_vector: case Rectype_Svector: # (vector t)
            pr_vector(stream_,obj); break;
          case Rectype_mdarray: # allgemeiner Array
            pr_array(stream_,obj); break;
          case Rectype_Closure: # Closure
            pr_closure(stream_,obj); break;
          case Rectype_Instance: # CLOS-Instanz
            pr_instance(stream_,obj); break;
          case Rectype_Complex: case Rectype_Ratio:
          case Rectype_Dfloat: case Rectype_Ffloat: case Rectype_Lfloat:
          case Rectype_Bignum: # Zahl
            pr_number(stream_,obj); break;
          case Rectype_Symbol: # Symbol
            pr_symbol(stream_,obj); break;
          #endif
          case Rectype_Hashtable:
            # je nach *PRINT-ARRAY* :
            # #<HASH-TABLE #x...> oder
            # #S(HASH-TABLE test (Key_1 . Value_1) ... (Key_n . Value_n))
            if (test_value(S(print_array)) || test_value(S(print_readably)))
              { LEVEL_CHECK;
                pushSTACK(obj); # Hash-Tabelle retten
               {var object* obj_ = &STACK_0; # und merken, wo sie sitzt
                write_ascii_char(stream_,'#'); write_ascii_char(stream_,'S');
                KLAMMER_AUF;
                INDENT_START(3); # um 3 Zeichen einrücken, wegen '#S('
                JUSTIFY_START;
                prin_object(stream_,S(hash_table)); # Symbol HASH-TABLE ausgeben
                obj = *obj_;
                { var uintL index = # Index in den Key-Value-Vektor
                    2*posfixnum_to_L(TheHashtable(obj)->ht_maxcount);
                  pushSTACK(TheHashtable(obj)->ht_kvtable); # Key-Value-Vektor
                 {var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
                  var uintL length = 0; # bisherige Länge := 0
                  JUSTIFY_SPACE; # Space ausgeben
                  # auf Erreichen von *PRINT-LENGTH* prüfen:
                  if (length >= length_limit) goto dots;
                  # Hash-Test ausgeben:
                  { var uintB flags = record_flags(TheHashtable(*obj_));
                    var object test = # Test-Symbol EQ/EQL/EQUAL
                      (flags & bit(0) ? S(eq) :
                       flags & bit(1) ? S(eql) :
                       flags & bit(2) ? S(equal) :
                                        NIL # (Default-Symbol)
                      );
                    prin_object(stream_,test);
                  }
                  loop
                    { length++; # bisherige Länge erhöhen
                      # nächstes auszugebendes Key-Value-Paar suchen:
                      loop
                        { if (index==0) goto kvtable_end; # kvtable zu Ende?
                          index -= 2; # Index verringern
                          if (!eq(TheSvector(STACK_0)->data[index+0],unbound)) # Key /= "leer" ?
                            break;
                        }
                      JUSTIFY_SPACE; # Space ausgeben
                      # auf Erreichen von *PRINT-LENGTH* prüfen:
                      if (length >= length_limit)
                        { dots:
                          # Rest durch '...' abkürzen:
                          write_ascii_char(stream_,'.');
                          write_ascii_char(stream_,'.');
                          write_ascii_char(stream_,'.');
                          break;
                        }
                      # Cons (Key . Value) bilden und ausgeben:
                      obj = allocate_cons();
                      { var object* ptr = &TheSvector(STACK_0)->data[index];
                        Car(obj) = ptr[0]; # Key
                        Cdr(obj) = ptr[1]; # Value
                      }
                      prin_object(stream_,obj);
                    }
                  kvtable_end: # Ende der Ausgabe der Key-Value-Paare
                  skipSTACK(1);
                }}
                JUSTIFY_END_ENG;
                INDENT_END;
                KLAMMER_ZU;
                skipSTACK(1);
                LEVEL_END;
              }}
              else
              { pr_hex6_obj(stream_,obj,O(printstring_hash_table)); }
            break;
          case Rectype_Package:
            # je nach *PRINT-READABLY*:
            # #<PACKAGE name> oder #.(SYSTEM::%FIND-PACKAGE "name")
            { pushSTACK(obj); # Package retten
             {var object* obj_ = &STACK_0; # und merken, wo sie sitzt
              if (!test_value(S(print_readably)))
                { write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
                  INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
                  JUSTIFY_START;
                  if (pack_deletedp(*obj_))
                    { write_sstring_case(stream_,O(printstring_deleted)); } # "DELETED "
                  write_sstring_case(stream_,O(printstring_package)); # "PACKAGE"
                  JUSTIFY_SPACE;
                  pr_like_symbol(stream_,ThePackage(*obj_)->pack_name); # Name ausgeben
                  JUSTIFY_END_ENG;
                  INDENT_END;
                  write_ascii_char(stream_,'>');
                }
                else
                { if (!(test_value(S(read_eval)) || stream_get_read_eval(*stream_)))
                    { fehler_print_readably(*obj_); }
                  if (pack_deletedp(*obj_)) { fehler_print_readably(*obj_); }
                  write_ascii_char(stream_,'#'); write_ascii_char(stream_,'.');
                  KLAMMER_AUF; # '('
                  INDENT_START(3); # um 3 Zeichen einrücken, wegen '#.('
                  JUSTIFY_START;
                  pr_symbol(stream_,S(pfind_package)); # SYSTEM::%FIND-PACKAGE
                  JUSTIFY_SPACE;
                  pr_string(stream_,ThePackage(*obj_)->pack_name); # Name ausgeben
                  JUSTIFY_END_ENG;
                  INDENT_END;
                  KLAMMER_ZU;
                }
              skipSTACK(1);
            }}break;
          case Rectype_Readtable:
            # #<READTABLE #x...>
            if (test_value(S(print_readably))) { fehler_print_readably(obj); }
            pr_hex6_obj(stream_,obj,O(printstring_readtable));
            break;
          case Rectype_Pathname:
            {
              pushSTACK(obj); # pathname
              # call (NAMESTRING pathname)
              pushSTACK(obj); funcall(L(namestring),1); obj = value1;
              ASSERT(stringp(obj));
              if (test_value(S(print_readably))
                  && !test_value(S(print_pathnames_ansi))) {
                var object* obj_;
                pushSTACK(obj); # string
                obj_ = &STACK_0;
                JUSTIFY_START; JUSTIFY_START;
                write_ascii_char(stream_,'#'); write_ascii_char(stream_,'-');
                write_sstring(stream_,O(lisp_implementation_type_string));
                JUSTIFY_SPACE;
                write_ascii_char(stream_,'#'); write_ascii_char(stream_,'P');
                pr_string(stream_,*obj_);
                JUSTIFY_END_ENG; JUSTIFY_SPACE; JUSTIFY_START;
                write_ascii_char(stream_,'#'); write_ascii_char(stream_,'+');
                write_sstring(stream_,O(lisp_implementation_type_string));
                JUSTIFY_SPACE;
                pr_record_descr(stream_,*(obj_ STACKop 1),S(pathname),TRUE,
                                O(pathname_slotlist));
                JUSTIFY_END_ENG; JUSTIFY_END_ENG;
                skipSTACK(1);
              } else {
                STACK_0 = obj; # String
                if (test_value(S(print_escape))
                    || test_value(S(print_readably))) {
                  # print "#P"
                  write_ascii_char(stream_,'#'); write_ascii_char(stream_,'P');
                }
                pr_string(stream_,STACK_0); # print the string
              }
              skipSTACK(1);
            }
            break;
          #ifdef LOGICAL_PATHNAMES
          case Rectype_Logpathname:
            # #S(LOGICAL-PATHNAME :HOST host :DIRECTORY directory :NAME name :TYPE type :VERSION version)
            pr_record_descr(stream_,obj,S(logical_pathname),TRUE,O(pathname_slotlist));
            break;
          #endif
          case Rectype_Random_State:
            # #S(RANDOM-STATE seed)
            { LEVEL_CHECK;
              pushSTACK(obj); # Random-State retten
             {var object* obj_ = &STACK_0; # und merken, wo es sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'S');
              KLAMMER_AUF;
              INDENT_START(3); # um 3 Zeichen einrücken, wegen '#S('
              JUSTIFY_START;
              prin_object(stream_,S(random_state)); # Symbol RANDOM-STATE ausgeben
              pr_record_ab(stream_,obj_,0,0); # Komponente ausgeben
              JUSTIFY_END_ENG;
              INDENT_END;
              KLAMMER_ZU;
              skipSTACK(1);
              LEVEL_END;
            }}break;
          #ifndef case_structure
          case Rectype_Structure: # Structure
            pr_structure(stream_,obj); break;
          #endif
          #ifndef case_stream
          case Rectype_Stream: # Stream
            pr_stream(stream_,obj); break;
          #endif
          case Rectype_Byte:
            #if 0
            # #<BYTE size position>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              LEVEL_CHECK;
              pushSTACK(obj); # Byte retten
             {var object* obj_ = &STACK_0; # und merken, wo es sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START;
              write_sstring_case(stream_,O(printstring_byte)); # "BYTE"
              pr_record_ab(stream_,obj_,0,0); # Komponenten ausgeben
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
              skipSTACK(1);
              LEVEL_END;
            }}
            #else
            # #S(BYTE :SIZE size :POSITION position)
            pr_record_descr(stream_,obj,S(byte),TRUE,O(byte_slotlist));
            #endif
            break;
          case Rectype_Fsubr: # Fsubr
            pr_fsubr(stream_,obj);
            break;
          case Rectype_Loadtimeeval:
            # #.form
            { if (test_value(S(print_readably)))
                if (!(test_value(S(read_eval)) || stream_get_read_eval(*stream_)))
                  { fehler_print_readably(obj); }
              pushSTACK(TheLoadtimeeval(obj)->loadtimeeval_form); # form retten
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'.');
              obj = popSTACK();
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#.'
              prin_object(stream_,obj); # form ausgeben
              INDENT_END;
            } break;
          case Rectype_Symbolmacro:
            # #<SYMBOL-MACRO expansion>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              LEVEL_CHECK;
              pushSTACK(obj); # Symbol-Macro retten
             {var object* obj_ = &STACK_0; # und merken, wo es sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START;
              write_sstring_case(stream_,O(printstring_symbolmacro)); # "SYMBOL-MACRO"
              pr_record_ab(stream_,obj_,0,0); # Komponente ausgeben
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
              skipSTACK(1);
              LEVEL_END;
            }}
            break;
          case Rectype_Encoding:
            # #<ENCODING [charset] line-terminator>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              LEVEL_CHECK;
              pushSTACK(obj); # Encoding retten
             {var object* obj_ = &STACK_0; # und merken, wo es sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START;
              write_sstring_case(stream_,O(printstring_encoding)); # "ENCODING"
              {var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
               var uintL length = 0; # bisherige Länge := 0
               #ifdef UNICODE
               # auf Erreichen von *PRINT-LENGTH* prüfen:
               if (length >= length_limit) goto encoding_end;
               JUSTIFY_SPACE; # Space ausgeben
               # Charset ausgeben:
               prin_object(stream_,TheEncoding(*obj_)->enc_charset);
               length++; # bisherige Länge erhöhen
               #endif
               # auf Erreichen von *PRINT-LENGTH* prüfen:
               if (length >= length_limit) goto encoding_end;
               JUSTIFY_SPACE; # Space ausgeben
               # Line-Terminator ausgeben:
               prin_object(stream_,TheEncoding(*obj_)->enc_eol);
               length++; # bisherige Länge erhöhen
              }
              encoding_end:
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
              skipSTACK(1);
              LEVEL_END;
            }}
            break;
          #ifdef FOREIGN
          case Rectype_Fpointer:
            # #<FOREIGN-POINTER address>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              LEVEL_CHECK;
             {var boolean validp = fp_validp(TheFpointer(obj));
              var uintP val = (uintP)(TheFpointer(obj)->fp_pointer); # Wert holen
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START;
              if (!validp)
                { write_sstring_case(stream_,O(printstring_invalid)); } # "INVALID "
              write_sstring_case(stream_,O(printstring_fpointer)); # "FOREIGN-POINTER"
              {var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
               var uintL length = 0; # bisherige Länge := 0
               # auf Erreichen von *PRINT-LENGTH* prüfen:
               if (length >= length_limit) goto fpointer_end;
               JUSTIFY_SPACE; # Space ausgeben
               # Adresse ausgeben:
               pr_hex8(stream_,val);
               length++; # bisherige Länge erhöhen
              }
              fpointer_end:
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
              LEVEL_END;
            }}break;
          #endif
          #ifdef DYNAMIC_FFI
          case Rectype_Faddress:
            # #<FOREIGN-ADDRESS #x...>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              LEVEL_CHECK;
              pushSTACK(obj); # retten
             {var object* obj_ = &STACK_0; # und merken, wo es sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START;
              if (!fp_validp(TheFpointer(TheFaddress(*obj_)->fa_base)))
                { write_sstring_case(stream_,O(printstring_invalid)); } # "INVALID "
              write_sstring_case(stream_,O(printstring_faddress)); # "FOREIGN-ADDRESS"
              {var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
               var uintL length = 0; # bisherige Länge := 0
               # auf Erreichen von *PRINT-LENGTH* prüfen:
               if (length >= length_limit) goto faddress_end;
               JUSTIFY_SPACE; # Space ausgeben
               # Adresse ausgeben, vgl. Macro Faddress_value():
               pr_hex8(stream_,(uintP)TheFpointer(TheFaddress(*obj_)->fa_base)->fp_pointer
                               +  TheFaddress(*obj_)->fa_offset
                      );
               length++; # bisherige Länge erhöhen
              }
              faddress_end:
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
              skipSTACK(1);
              LEVEL_END;
            }}break;
          case Rectype_Fvariable:
            # #<FOREIGN-VARIABLE name #x...>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              LEVEL_CHECK;
              pushSTACK(obj); # retten
             {var object* obj_ = &STACK_0; # und merken, wo es sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START;
              write_sstring_case(stream_,O(printstring_fvariable)); # "FOREIGN-VARIABLE"
              {var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
               var uintL length = 0; # bisherige Länge := 0
               # auf Erreichen von *PRINT-LENGTH* prüfen:
               if (length >= length_limit) goto fvariable_end;
               JUSTIFY_SPACE; # Space ausgeben
               # Name ausgeben:
               if (!nullp(TheFvariable(*obj_)->fv_name))
                 { prin_object(stream_,TheFvariable(*obj_)->fv_name);
                   length++; # bisherige Länge erhöhen
                   if (length >= length_limit) goto fvariable_end;
                   JUSTIFY_SPACE; # Space ausgeben
                 }
               # Adresse ausgeben:
               pr_hex8(stream_,(uintP)Faddress_value(TheFvariable(*obj_)->fv_address));
               length++; # bisherige Länge erhöhen
              }
              fvariable_end:
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
              skipSTACK(1);
              LEVEL_END;
            }}break;
          case Rectype_Ffunction:
            # #<FOREIGN-FUNCTION name #x...>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              LEVEL_CHECK;
              pushSTACK(obj); # retten
             {var object* obj_ = &STACK_0; # und merken, wo es sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START;
              write_sstring_case(stream_,O(printstring_ffunction)); # "FOREIGN-FUNCTION"
              {var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
               var uintL length = 0; # bisherige Länge := 0
               # auf Erreichen von *PRINT-LENGTH* prüfen:
               if (length >= length_limit) goto ffunction_end;
               JUSTIFY_SPACE; # Space ausgeben
               # Name ausgeben:
               if (!nullp(TheFfunction(*obj_)->ff_name))
                 { prin_object(stream_,TheFfunction(*obj_)->ff_name);
                   length++; # bisherige Länge erhöhen
                   if (length >= length_limit) goto ffunction_end;
                   JUSTIFY_SPACE; # Space ausgeben
                 }
               # Adresse ausgeben:
               pr_hex8(stream_,(uintP)Faddress_value(TheFfunction(*obj_)->ff_address));
               length++; # bisherige Länge erhöhen
              }
              ffunction_end:
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
              skipSTACK(1);
              LEVEL_END;
            }}break;
          #endif
          case Rectype_Weakpointer:
            # #<WEAK-POINTER value> or #<BROKEN WEAK-POINTER>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              if (!eq(TheWeakpointer(obj)->wp_cdr,unbound))
                { LEVEL_CHECK;
                  pushSTACK(TheWeakpointer(obj)->wp_value); # value retten
                 {var object* value_ = &STACK_0; # und merken, wo es sitzt
                  write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
                  INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
                  JUSTIFY_START;
                  write_sstring_case(stream_,O(printstring_weakpointer)); # "WEAK-POINTER"
                  {var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
                   var uintL length = 0; # bisherige Länge := 0
                   # auf Erreichen von *PRINT-LENGTH* prüfen:
                   if (length >= length_limit) goto weakpointer_end;
                   JUSTIFY_SPACE; # Space ausgeben
                   prin_object(stream_,*value_); # output value
                   length++; # bisherige Länge erhöhen
                  }
                  weakpointer_end:
                  JUSTIFY_END_ENG;
                  INDENT_END;
                  write_ascii_char(stream_,'>');
                  skipSTACK(1);
                  LEVEL_END;
                }}
                else
                { write_sstring_case(stream_,O(printstring_broken_weakpointer)); }
            } break;
          case Rectype_Finalizer:
            # #<FINALIZER>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              write_sstring_case(stream_,O(printstring_finalizer));
            } break;
          #ifdef SOCKET_STREAMS
          case Rectype_Socket_Server:
            # #<SOCKET-SERVER host:port>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              LEVEL_CHECK;
              pushSTACK(obj); # retten
             {var object* obj_ = &STACK_0; # und merken, wo es sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START;
              # falls geschlossen, "CLOSED " ausgeben:
              if (nullp(TheSocketServer(*obj_)->socket_handle))
                { write_sstring_case(stream_,O(printstring_closed)); }
              write_sstring_case(stream_,O(printstring_socket_server)); # "SOCKET-SERVER"
              {var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
               var uintL length = 0; # bisherige Länge := 0
               # auf Erreichen von *PRINT-LENGTH* prüfen:
               if (length >= length_limit) goto socket_server_end;
               JUSTIFY_SPACE; # Space ausgeben
               # output host
               write_string(stream_,TheSocketServer(*obj_)->host);
               write_ascii_char(stream_,':'); # Port ausgeben:
               pr_number(stream_,TheSocketServer(*obj_)->port);
               length++; # bisherige Länge erhöhen
              }
              socket_server_end:
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
              skipSTACK(1);
              LEVEL_END;
            }}break;
          #endif
          #ifdef YET_ANOTHER_RECORD
          case Rectype_Yetanother:
            # #<YET-ANOTHER address>
            { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
              LEVEL_CHECK;
              pushSTACK(obj); # Yetanother retten
             {var object* obj_ = &STACK_0; # und merken, wo es sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START;
              write_sstring_case(stream_,O(printstring_yetanother)); # "YET-ANOTHER"
              {var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
               var uintL length = 0; # bisherige Länge := 0
               # auf Erreichen von *PRINT-LENGTH* prüfen:
               if (length >= length_limit) goto yetanother_end;
               JUSTIFY_SPACE; # Space ausgeben
               # x ausgeben:
               pr_hex6(stream_,TheYetanother(*obj_)->yetanother_x);
               length++; # bisherige Länge erhöhen
              }
              yetanother_end:
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
              skipSTACK(1);
              LEVEL_END;
            }}break;
          #endif
          default:
            pushSTACK(S(print));
            fehler(serious_condition,
                   GETTEXT("~: an unknown record type has been generated!")
                  );
    }   }

#                    -------- SUBRs, FSUBRs --------

# UP: Gibt ein Objekt in Form #<BLABLA other> auf einen Stream aus.
# pr_other_obj(&stream,other,string);
# > other: Objekt
# > string: Simple-String "BLABLA"
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_other_obj (const object* stream_, object other, object string);
  local void pr_other_obj(stream_,other,string)
    var const object* stream_;
    var object other;
    var object string;
    { pushSTACK(other); # other retten
      pushSTACK(string); # String retten
     {var object* string_ = &STACK_0; # und merken, wo beides sitzt
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
      INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
      JUSTIFY_START;
      write_sstring_case(stream_,*string_); # String ausgeben
      JUSTIFY_SPACE;
      prin_object(stream_,*(string_ STACKop 1)); # other ausgeben
      JUSTIFY_END_ENG;
      INDENT_END;
      write_ascii_char(stream_,'>');
      skipSTACK(2);
    }}

# UP: Gibt ein SUBR auf einen Stream aus.
# pr_subr(&stream,obj);
# > obj: SUBR
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_subr(stream_,obj)
    var const object* stream_;
    var object obj;
    { # #<SYSTEM-FUNCTION name> bzw. #<ADD-ON-SYSTEM-FUNCTION name>
      # bzw. #.(SYSTEM::%FIND-SUBR 'name)
      if (test_value(S(print_readably)))
        { if (!(test_value(S(read_eval)) || stream_get_read_eval(*stream_)))
            { fehler_print_readably(obj); }
          pushSTACK(obj); # obj retten
         {var object* obj_ = &STACK_0; # und merken, wo es sitzt
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'.');
          KLAMMER_AUF; # '('
          INDENT_START(3); # um 3 Zeichen einrücken, wegen '#.('
          JUSTIFY_START;
          pr_symbol(stream_,S(find_subr)); # SYSTEM::%FIND-SUBR
          JUSTIFY_SPACE;
          write_ascii_char(stream_,'\'');
          pr_symbol(stream_,TheSubr(*obj_)->name); # Name ausgeben
          JUSTIFY_END_ENG;
          INDENT_END;
          KLAMMER_ZU;
          skipSTACK(1);
        }}
        else
        { pr_other_obj(stream_,TheSubr(obj)->name,
                       ((as_oint(subr_tab_ptr_as_object(&subr_tab)) <= as_oint(obj))
                        && (as_oint(obj) < as_oint(subr_tab_ptr_as_object(&subr_tab+1)))
                       ) ? O(printstring_subr) : O(printstring_addon_subr)
                      );
    }   }

# UP: Gibt ein FSUBR auf einen Stream aus.
# pr_fsubr(&stream,obj);
# > obj: FSUBR
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_fsubr(stream_,obj)
    var const object* stream_;
    var object obj;
    { # #<SPECIAL-OPERATOR name>
      if (test_value(S(print_readably))) { fehler_print_readably(obj); }
      pr_other_obj(stream_,TheFsubr(obj)->name,O(printstring_fsubr));
    }

#                       -------- Closures --------

# UP: Gibt eine Closure auf einen Stream aus.
# pr_closure(&stream,obj);
# > obj: Closure
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_closure(stream_,obj)
    var const object* stream_;
    var object obj;
    { if (simple_bit_vector_p(TheClosure(obj)->clos_codevec))
        # compilierte Closure
        { pr_cclosure(stream_,obj); }
        else
        # interpretierte Closure ausgeben: #<CLOSURE ...>
        { # Falls *PRINT-CLOSURE* /= NIL, alles, sonst den Namen und
          # (falls noch vorhanden) Lambdaliste und Formen, ausgeben:
          if (test_value(S(print_readably))) { fehler_print_readably(obj); }
          LEVEL_CHECK;
          pushSTACK(obj); # Closure retten
         {var object* obj_ = &STACK_0; # und merken, wo sie sitzt
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
          INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
          JUSTIFY_START;
          write_sstring_case(stream_,O(printstring_closure));
          if (test_value(S(print_closure))) # *PRINT-CLOSURE* abfragen
            # *PRINT-CLOSURE* /= NIL -> #<CLOSURE komponente1 ...> ausgeben:
            { pr_record_ab(stream_,obj_,0,0); } # alle weiteren Komponenten ausgeben
            else
            # *PRINT-CLOSURE* = NIL -> #<CLOSURE name . form> ausgeben:
            { JUSTIFY_SPACE;
              prin_object(stream_,TheIclosure(*obj_)->clos_name); # Name ausgeben
              # Formenliste elementweise ausgeben:
              pr_record_rest(stream_,TheIclosure(*obj_)->clos_form,1);
            }
          JUSTIFY_END_ENG;
          INDENT_END;
          write_ascii_char(stream_,'>');
          skipSTACK(1);
          LEVEL_END;
        }}
    }

# UP: Gibt eine compilierte Closure auf einen Stream aus.
# pr_cclosure(&stream,obj);
# > obj: compilierte Closure
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_cclosure(stream_,obj)
    var const object* stream_;
    var object obj;
    { # *PRINT-CLOSURE* abfragen:
      if (test_value(S(print_closure)) || test_value(S(print_readably)))
        # *PRINT-CLOSURE /= NIL -> in wiedereinlesbarer Form #Y(...) ausgeben
        { pr_cclosure_lang(stream_,obj); }
        else
        # *PRINT-CLOSURE* = NIL ->
        # nur #<GENERIC-FUNCTION name> bzw. #<COMPILED-CLOSURE name> ausgeben:
        { pr_other_obj(stream_,TheClosure(obj)->clos_name,
                       (TheCodevec(TheClosure(obj)->clos_codevec)->ccv_flags & bit(4) # generische Funktion?
                        ? O(printstring_generic_function)
                        : O(printstring_compiled_closure)
                      ));
        }
    }

# compilierte Closure in wiedereinlesbarer Form ausgeben:
# (defun %print-cclosure (closure)
#   (princ "#Y(")
#   (prin1 (closure-name closure))
#   (princ " #")
#   (let ((L (closure-codevec closure)))
#     (let ((*print-base* 10.)) (prin1 (length L)))
#     (princ "Y(")
#     (let ((*print-base* 16.))
#       (do ((i 0 (1- i))
#            (x L (cdr x)))
#           ((endp x))
#         (when (zerop i) (terpri) (setq i 25))
#         (princ " ")
#         (prin1 (car x))
#     ) )
#     (princ ")")
#   )
#   (terpri)
#   (dolist (x (closure-consts closure))
#     (princ " ")
#     (prin1 x)
#   )
#   (princ ")")
# )
# UP: Gibt eine compilierte Closure in wiedereinlesbarer Form
# auf einen Stream aus.
# pr_cclosure_lang(&stream,obj);
# > obj: compilierte Closure
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_cclosure_lang(stream_,obj)
    var const object* stream_;
    var object obj;
    { LEVEL_CHECK;
      pushSTACK(obj); # Closure retten
     {var object* obj_ = &STACK_0; # und merken, wo sie sitzt
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'Y');
      KLAMMER_AUF;
      INDENT_START(3); # um 3 Zeichen einrücken, wegen '#Y('
      JUSTIFY_START;
      prin_object(stream_,TheClosure(*obj_)->clos_name); # Name ausgeben
      JUSTIFY_SPACE;
      # Codevektor byteweise ausgeben, dabei Zirkularität behandeln:
      pr_circle(stream_,TheClosure(*obj_)->clos_codevec,&pr_cclosure_codevector);
      pr_record_ab(stream_,obj_,2,2); # restliche Komponenten ausgeben
      JUSTIFY_END_ENG;
      INDENT_END;
      KLAMMER_ZU;
      skipSTACK(1);
      LEVEL_END;
    }}

# UP: Gibt einen Closure-Codevektor in #nY(...)-Schreibweise
# auf einen Stream aus.
# pr_cclosure_codevector(&stream,codevec);
# > codevec: ein Simple-Bit-Vektor
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_cclosure_codevector(stream_,codevec)
    var const object* stream_;
    var object codevec;
    { LEVEL_CHECK;
      pushSTACK(codevec); # Codevektor retten
     {var object* codevec_ = &STACK_0; # und merken, wo er sitzt
      var uintL len = Sbvector_length(codevec)/8; # Länge in Bytes
      #if BIG_ENDIAN_P
      var uintL header_end_index =
        (TheSbvector(codevec)->data[CCV_FLAGS] & bit(7) ? CCV_START_KEY : CCV_START_NONKEY);
      #endif
      # Präfix ausgeben:
      INDENTPREP_START;
      write_ascii_char(stream_,'#');
      pr_uint(stream_,len); # Länge dezimal ausgeben
      write_ascii_char(stream_,'Y');
      {var uintL indent = INDENTPREP_END;
      # Hauptteil ausgeben:
       INDENT_START(indent); # einrücken
      }
      KLAMMER_AUF;
      INDENT_START(1); # um 1 Zeichen einrücken, wegen '('
      JUSTIFY_START;
      { var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
        var uintL length = 0; # Index = bisherige Länge := 0
        for ( ; len > 0; len--)
          { # (außer vorm ersten Element) Space ausgeben:
            if (!(length==0)) { JUSTIFY_SPACE; }
            # auf Erreichen von *PRINT-LENGTH* prüfen:
            if (length >= length_limit)
              { # Rest durch '...' abkürzen:
                write_ascii_char(stream_,'.');
                write_ascii_char(stream_,'.');
                write_ascii_char(stream_,'.');
                break;
              }
            codevec = *codevec_;
           {var uintL index = length;
            #if BIG_ENDIAN_P
            # Byte-Index berechnen, dabei Big-Endian -> Little-Endian Konversion machen:
            if (index < header_end_index)
              { switch (index)
                { case CCV_SPDEPTH_1:          case CCV_SPDEPTH_1+1:
                  case CCV_SPDEPTH_JMPBUFSIZE: case CCV_SPDEPTH_JMPBUFSIZE+1:
                  case CCV_NUMREQ:             case CCV_NUMREQ+1:
                  case CCV_NUMOPT:             case CCV_NUMOPT+1:
                  case CCV_NUMKEY:             case CCV_NUMKEY+1:
                  case CCV_KEYCONSTS:          case CCV_KEYCONSTS+1:
                    index = index^1;
                    break;
                  default:
                    break;
              } }
            #endif
            # Byte ausgeben:
            pr_hex2(stream_,TheSbvector(codevec)->data[index]);
            length++; # Index erhöhen
          }}
      }
      JUSTIFY_END_ENG;
      INDENT_END;
      KLAMMER_ZU;
      INDENT_END;
      skipSTACK(1);
      LEVEL_END;
    }}

#                       -------- Streams --------

# UP: Gibt einen Stream auf einen Stream aus.
# pr_stream(&stream,obj);
# > obj: auszugebender Stream
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_stream(stream_,obj)
    var const object* stream_;
    var object obj;
    { if (test_value(S(print_readably))) { fehler_print_readably(obj); }
      pushSTACK(obj); # Stream retten
     {var object* obj_ = &STACK_0; # und merken, wo er sitzt
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
      INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
      JUSTIFY_START;
      # falls Stream geschlossen, "CLOSED " ausgeben:
      if ((TheStream(*obj_)->strmflags & strmflags_open_B) == 0)
        { write_sstring_case(stream_,O(printstring_closed)); }
      # if a file stream, print "BUFFERED " or "UNBUFFERED ":
      {var uintL type = TheStream(*obj_)->strmtype;
       switch (type)
         { case strmtype_file:
           #ifdef PIPES
           case strmtype_pipe_in:
           case strmtype_pipe_out:
           #endif
           #ifdef X11SOCKETS
           case strmtype_x11socket:
           #endif
           #ifdef SOCKET_STREAMS
           case strmtype_socket:
           case strmtype_twoway_socket:
           #endif
             write_sstring_case(stream_,
                                stream_isbuffered(*obj_)
                                ? O(printstring_buffered)
                                : O(printstring_unbuffered)
                               );
             break;
           default:
             break;
         }
      # Streamtyp ausgeben:
       {var const object* stringtable = &O(printstring_strmtype_synonym);
        write_sstring_case(stream_,stringtable[type]); # String aus Tabelle holen
       }
      # "-STREAM" ausgeben:
        write_sstring_case(stream_,O(printstring_stream));
      # Streamspezifische Zusatzinformation:
        switch (type)
          { case strmtype_synonym:
              # Synonym-Stream
              JUSTIFY_SPACE;
              prin_object(stream_,TheStream(*obj_)->strm_synonym_symbol); # Symbol ausgeben
              break;
            case strmtype_broad:
              # Broadcast-Stream
              pr_record_rest(stream_,TheStream(*obj_)->strm_broad_list,0); # Streams ausgeben
              break;
            case strmtype_concat:
              # Concatenated-Stream
              pr_record_rest(stream_,TheStream(*obj_)->strm_concat_list,0); # Streams ausgeben
              break;
            case strmtype_buff_in:
              # Buffered-Input-Stream
              JUSTIFY_SPACE;
              prin_object(stream_,TheStream(*obj_)->strm_buff_in_fun); # Funktion ausgeben
              break;
            case strmtype_buff_out:
              # Buffered-Output-Stream
              JUSTIFY_SPACE;
              prin_object(stream_,TheStream(*obj_)->strm_buff_out_fun); # Funktion ausgeben
              break;
            #ifdef GENERIC_STREAMS
            case strmtype_generic:
              # Generic Streams
              JUSTIFY_SPACE;
              prin_object(stream_,TheStream(*obj_)->strm_controller_object); # Controller ausgeben
              break;
            #endif
            case strmtype_file:
              # File-Stream
              JUSTIFY_SPACE;
              prin_object(stream_,TheStream(*obj_)->strm_eltype); # Stream-Element-Type
              if (!nullp(TheStream(*obj_)->strm_file_name))
                { JUSTIFY_SPACE;
                  prin_object(stream_,TheStream(*obj_)->strm_file_name); # Filename ausgeben
                }
              break;
            #ifdef PIPES
            case strmtype_pipe_in:
            case strmtype_pipe_out:
              # Pipe-In/Out-Stream
              JUSTIFY_SPACE;
              prin_object(stream_,TheStream(*obj_)->strm_eltype); # Stream-Element-Type
              JUSTIFY_SPACE;
              pr_uint(stream_,I_to_UL(TheStream(*obj_)->strm_pipe_pid)); # Prozess-Id ausgeben
              break;
            #endif
            #ifdef X11SOCKETS
            case strmtype_x11socket:
              # X11-Socket-Stream
              JUSTIFY_SPACE;
              prin_object(stream_,TheStream(*obj_)->strm_x11socket_connect); # Verbindungsziel ausgeben
              break;
            #endif
            #ifdef SOCKET_STREAMS
            case strmtype_twoway_socket:
              *obj_ = TheStream(*obj_)->strm_twoway_socket_input;
              /*FALLTHROUGH*/
            case strmtype_socket:
              # Socket-Stream
              JUSTIFY_SPACE;
              prin_object(stream_,TheStream(*obj_)->strm_eltype); # Stream-Element-Type
              JUSTIFY_SPACE;
              { var object host = TheStream(*obj_)->strm_socket_host;
                if (!nullp(host))
                  write_string(stream_,host);
              }
              write_ascii_char(stream_,':');
              pr_number(stream_,TheStream(*obj_)->strm_socket_port);
              break;
            #endif
            default:
              # sonst keine Zusatzinformation
              break;
          }
        if (type==strmtype_file && eq(TheStream(*obj_)->strm_eltype,S(character)))
          { JUSTIFY_SPACE;
            # Zeilennummer ausgeben, in der sich der Stream gerade befindet:
            write_ascii_char(stream_,'@');
            pr_number(stream_,stream_line_number(*obj_));
          }
      }
      JUSTIFY_END_ENG;
      INDENT_END;
      write_ascii_char(stream_,'>');
      skipSTACK(1);
    }}


# ---------------------- Top-Level-Aufruf des Printers ------------------------

# UP: Gibt ein Objekt auf einen Stream aus.
# prin1(&stream,obj);
# > obj: Objekt
# > stream: Stream
# < stream: Stream
# can trigger GC
  global void prin1 (const object* stream_, object obj);
  global void prin1(stream_,obj)
    var const object* stream_;
    var object obj;
    { pr_enter(stream_,obj,&prin_object); }

# UP: Gibt erst Newline, dann ein Objekt auf einen Stream aus.
# print(&stream,obj);
# > obj: Objekt
# > stream: Stream
# < stream: Stream
# can trigger GC
  global void print (const object* stream_, object obj);
  global void print(stream_,obj)
    var const object* stream_;
    var object obj;
    { pushSTACK(obj); # Objekt retten
      write_ascii_char(stream_,NL); # #\Newline ausgeben
      obj = popSTACK();
      prin1(stream_,obj); # Objekt ausgeben
    }


# ----------------------- LISP-Funktionen des Printers ------------------------

# UP: Überprüft ein Output-Stream-Argument.
# Default ist der Wert von *STANDARD-OUTPUT*.
# test_ostream();
# > subr_self: Aufrufer (ein SUBR)
# > STACK_0: Output-Stream-Argument
# < STACK_0: Output-Stream (ein Stream)
  local void test_ostream (void);
  local void test_ostream()
    { var object stream = STACK_0; # Output-Stream-Argument
      if (eq(stream,unbound) || nullp(stream))
        # #<UNBOUND> oder NIL -> Wert von *STANDARD-OUTPUT*
        { STACK_0 = var_stream(S(standard_output),strmflags_wr_ch_B); }
      elif (eq(stream,T))
        # T -> Wert von *TERMINAL-IO*
        { STACK_0 = var_stream(S(terminal_io),strmflags_wr_ch_B); }
      else
        # sollte ein Stream sein
        { if (!streamp(stream)) { fehler_stream(stream); } }
    }

# Print-Variablen (siehe CONSTSYM.D):
#   *PRINT-CASE*     --+
#   *PRINT-LEVEL*      |
#   *PRINT-LENGTH*     |
#   *PRINT-GENSYM*     |
#   *PRINT-ESCAPE*     | Reihenfolge fest!
#   *PRINT-RADIX*      | Dieselbe Reihenfolge in CONSTSYM.D
#   *PRINT-BASE*       | und bei den SUBRs WRITE, WRITE-TO-STRING
#   *PRINT-ARRAY*      |
#   *PRINT-CIRCLE*     |
#   *PRINT-PRETTY*     |
#   *PRINT-CLOSURE*    |
#   *PRINT-READABLY*      |
#   *PRINT-RIGHT-MARGIN* -+
# erste Print-Variable:
  #define first_print_var  S(print_case)
# Anzahl der Print-Variablen:
  #define print_vars_anz  13

# UP für WRITE und WRITE-TO-STRING
# > STACK_(print_vars_anz+1): Objekt
# > STACK_(print_vars_anz)..STACK_(1): Argumente zu den Print-Variablen
# > STACK_0: Stream
  local void write_up (void);
  local void write_up()
    { var object* argptr = args_end_pointer STACKop (1+print_vars_anz+1); # Pointer über die Keyword-Argumente
      var object obj = NEXT(argptr); # erstes Argument = Objekt
      # die angegebenen Variablen binden:
      var uintC bindcount = 0; # Anzahl der Bindungen
      {var object sym = first_print_var; # durchläuft die Symbole
       var uintC count;
       dotimesC(count,print_vars_anz,
         { var object arg = NEXT(argptr); # nächstes Keyword-Argument
           if (!eq(arg,unbound)) # angegeben?
             { dynamic_bind(sym,arg); bindcount++; } # ja -> Variable daran binden
           sym = objectplus(sym,(soint)sizeof(*TheSymbol(sym))<<(oint_addr_shift-addr_shift)); # zum nächsten Symbol
         });
      }
      {var object* stream_ = &NEXT(argptr); # nächstes Argument ist der Stream
       prin1(stream_,obj); # Objekt ausgeben
      }
      # Bindungen auflösen:
      dotimesC(bindcount,bindcount, { dynamic_unbind(); } );
    }

LISPFUN(write,1,0,norest,key,14,\
        (kw(case),kw(level),kw(length),kw(gensym),kw(escape),kw(radix),\
         kw(base),kw(array),kw(circle),kw(pretty),kw(closure),kw(readably),\
         kw(right_margin),kw(stream)))
# (WRITE object [:stream] [:escape] [:radix] [:base] [:circle] [:pretty]
#               [:level] [:length] [:case] [:gensym] [:array] [:closure]
#               [:readably] [:right-margin]),
# CLTL S. 382
  { # Stackaufbau: object, Print-Variablen-Argumente, Stream-Argument.
    test_ostream(); # Output-Stream überprüfen
    write_up(); # WRITE durchführen
    skipSTACK(print_vars_anz+1);
    value1 = popSTACK(); mv_count=1; # object als Wert
  }

# (defun prin1 (object &optional stream)
#   (test-output-stream stream)
#   (let ((*print-escape* t))
#     (prin object stream)
#   )
#   object
# )

# UP für PRIN1 und PRINT und PRIN1-TO-STRING
# > STACK_1: Objekt
# > STACK_0: Stream
  local void prin1_up (void);
  local void prin1_up()
    { var object obj = STACK_1;
      var object* stream_ = &STACK_0;
      dynamic_bind(S(print_escape),T); # *PRINT-ESCAPE* an T binden
      prin1(stream_,obj); # object ausgeben
      dynamic_unbind();
    }

LISPFUN(prin1,1,1,norest,nokey,0,NIL)
# (PRIN1 object [stream]), CLTL S. 383
  { test_ostream(); # Output-Stream überprüfen
    prin1_up(); # PRIN1 durchführen
    skipSTACK(1);
    value1 = popSTACK(); mv_count=1; # object als Wert
  }

# (defun print (object &optional stream)
#   (test-output-stream stream)
#   (terpri stream)
#   (let ((*print-escape* t))
#     (prin object stream)
#   )
#   (write-char #\Space stream)
#   object
# )
LISPFUN(print,1,1,norest,nokey,0,NIL)
# (PRINT object [stream]), CLTL S. 383
  { test_ostream(); # Output-Stream überprüfen
    terpri(&STACK_0); # neue Zeile
    prin1_up(); # PRIN1 durchführen
    write_ascii_char(&STACK_0,' '); # Space danach
    skipSTACK(1);
    value1 = popSTACK(); mv_count=1; # object als Wert
  }

# (defun pprint (object &optional stream)
#   (test-output-stream stream)
#   (terpri stream)
#   (let ((*print-escape* t) (*print-pretty* t))
#     (prin object stream)
#   )
#   (values)
# )
LISPFUN(pprint,1,1,norest,nokey,0,NIL)
# (PPRINT object [stream]), CLTL S. 383
  { test_ostream(); # Output-Stream überprüfen
    terpri(&STACK_0); # neue Zeile
   {var object obj = STACK_1;
    var object* stream_ = &STACK_0;
    dynamic_bind(S(print_pretty),T); # *PRINT-PRETTY* an T binden
    dynamic_bind(S(print_escape),T); # *PRINT-ESCAPE* an T binden
    prin1(stream_,obj); # object ausgeben
    dynamic_unbind();
    dynamic_unbind();
    skipSTACK(2);
    value1 = NIL; mv_count=0; # keine Werte
  }}

# (defun princ (object &optional stream)
#   (test-output-stream stream)
#   (let ((*print-escape* nil)
#         (*print-readably* nil))
#     (prin object stream)
#   )
#   object
# )

# UP für PRINC und PRINC-TO-STRING
# > STACK_1: Objekt
# > STACK_0: Stream
  local void princ_up (void);
  local void princ_up()
    { var object obj = STACK_1;
      var object* stream_ = &STACK_0;
      dynamic_bind(S(print_escape),NIL); # *PRINT-ESCAPE* an NIL binden
      dynamic_bind(S(print_readably),NIL); # *PRINT-READABLY* an NIL binden
      prin1(stream_,obj); # object ausgeben
      dynamic_unbind();
      dynamic_unbind();
    }

LISPFUN(princ,1,1,norest,nokey,0,NIL)
# (PRINC object [stream]), CLTL S. 383
  { test_ostream(); # Output-Stream überprüfen
    princ_up(); # PRINC durchführen
    skipSTACK(1);
    value1 = popSTACK(); mv_count=1; # object als Wert
  }

# (defun write-to-string (object &rest args
#                                &key escape radix base circle pretty level
#                                     length case gensym array closure
#                                     readably right-margin)
#   (with-output-to-string (stream)
#     (apply #'write object :stream stream args)
# ) )
LISPFUN(write_to_string,1,0,norest,key,13,\
        (kw(case),kw(level),kw(length),kw(gensym),kw(escape),kw(radix),\
         kw(base),kw(array),kw(circle),kw(pretty),kw(closure),kw(readably),\
         kw(right_margin)))
# (WRITE-TO-STRING object [:escape] [:radix] [:base] [:circle] [:pretty]
#                         [:level] [:length] [:case] [:gensym] [:array]
#                         [:closure] [:readably] [:right_margin]),
# CLTL S. 383
  { pushSTACK(make_string_output_stream()); # String-Output-Stream
    write_up(); # WRITE durchführen
    value1 = get_output_stream_string(&STACK_0); mv_count=1; # Ergebnis-String als Wert
    skipSTACK(1+print_vars_anz+1);
  }

# (defun prin1-to-string (object)
#   (with-output-to-string (stream) (prin1 object stream))
# )
LISPFUNN(prin1_to_string,1)
# (PRIN1-TO-STRING object), CLTL S. 383
  { pushSTACK(make_string_output_stream()); # String-Output-Stream
    prin1_up(); # PRIN1 durchführen
    value1 = get_output_stream_string(&STACK_0); mv_count=1; # Ergebnis-String als Wert
    skipSTACK(2);
  }

# (defun princ-to-string (object)
#   (with-output-to-string (stream) (princ object stream))
# )
LISPFUNN(princ_to_string,1)
# (PRINC-TO-STRING object), CLTL S. 383
  { pushSTACK(make_string_output_stream()); # String-Output-Stream
    princ_up(); # PRINC durchführen
    value1 = get_output_stream_string(&STACK_0); mv_count=1; # Ergebnis-String als Wert
    skipSTACK(2);
  }

LISPFUN(write_char,1,1,norest,nokey,0,NIL)
# (WRITE-CHAR character [stream]), CLTL S. 384
  { test_ostream(); # Output-Stream überprüfen
   {var object ch = STACK_1; # character-Argument
    if (!charp(ch))
      { pushSTACK(ch); # Wert für Slot DATUM von TYPE-ERROR
        pushSTACK(S(character)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
        pushSTACK(ch);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~: ~ is not a character")
              );
      }
    write_char(&STACK_0,ch);
    value1 = ch; mv_count=1; # ch (nicht GC-gefährdet) als Wert
    skipSTACK(2);
  }}

# UP für WRITE-STRING und WRITE-LINE:
# Überprüft die Argumente und gibt einen String-Teil auf einen Stream aus.
# > subr_self: Aufrufer (ein SUBR)
# > Stackaufbau: String-Argument, Stream-Argument, :START-Argument, :END-Argument.
# < Stackaufbau: Stream, String.
# can trigger GC
  local void write_string_up (void);
  local void write_string_up()
    {{ pushSTACK(STACK_2); # Stream ans STACK-Ende
       test_ostream(); # überprüfen
       STACK_(2+1) = STACK_(3+1);
       STACK_(3+1) = STACK_0;
       skipSTACK(1);
     }# Stackaufbau: stream, string, :START-Argument, :END-Argument.
      # Grenzen überprüfen:
      { var stringarg arg;
        var object string = test_string_limits_ro(&arg);
        pushSTACK(string);
        # Stackaufbau: stream, string.
        write_sstring_ab(&STACK_1,arg.string,arg.offset+arg.index,arg.len);
      }
    }

LISPFUN(write_string,1,1,norest,key,2, (kw(start),kw(end)) )
# (WRITE-STRING string [stream] [:start] [:end]), CLTL S. 384
  { write_string_up(); # überprüfen und ausgeben
    value1 = popSTACK(); mv_count=1; skipSTACK(1); # string als Wert
  }

LISPFUN(write_line,1,1,norest,key,2, (kw(start),kw(end)) )
# (WRITE-LINE string [stream] [:start] [:end]), CLTL S. 384
  { write_string_up(); # überprüfen und ausgeben
    terpri(&STACK_1); # neue Zeile
    value1 = popSTACK(); mv_count=1; skipSTACK(1); # string als Wert
  }

LISPFUN(terpri,0,1,norest,nokey,0,NIL)
# (TERPRI [stream]), CLTL S. 384
  { test_ostream(); # Output-Stream überprüfen
    terpri(&STACK_0); # neue Zeile
    value1 = NIL; mv_count=1; skipSTACK(1); # NIL als Wert
  }

LISPFUN(fresh_line,0,1,norest,nokey,0,NIL)
# (FRESH-LINE [stream]), CLTL S. 384
  { test_ostream(); # Output-Stream überprüfen
    if (eq(get_line_position(STACK_0),Fixnum_0)) # Line-Position = 0 ?
      { value1 = NIL; mv_count=1; } # ja -> NIL als Wert
      else
      { terpri(&STACK_0); # nein -> neue Zeile
        value1 = T; mv_count=1; # und T als Wert
      }
    skipSTACK(1);
  }

LISPFUN(finish_output,0,1,norest,nokey,0,NIL)
# (FINISH-OUTPUT [stream]), CLTL S. 384
  { test_ostream(); # Output-Stream überprüfen
    finish_output(popSTACK()); # Output ans Ziel bringen
    value1 = NIL; mv_count=1; # NIL als Wert
  }

LISPFUN(force_output,0,1,norest,nokey,0,NIL)
# (FORCE-OUTPUT [stream]), CLTL S. 384
  { test_ostream(); # Output-Stream überprüfen
    force_output(popSTACK()); # Output ans Ziel bringen
    value1 = NIL; mv_count=1; # NIL als Wert
  }

LISPFUN(clear_output,0,1,norest,nokey,0,NIL)
# (CLEAR-OUTPUT [stream]), CLTL S. 384
  { test_ostream(); # Output-Stream überprüfen
    clear_output(popSTACK()); # Output löschen
    value1 = NIL; mv_count=1; # NIL als Wert
  }

LISPFUN(write_unreadable,3,0,norest,key,2, (kw(type),kw(identity)) )
# (SYSTEM::WRITE-UNREADABLE function object stream [:type] [:identity]),
# vgl. CLtL2 S. 580
  { var boolean flag_fun = FALSE;
    var boolean flag_type = FALSE;
    var boolean flag_id = FALSE;
    { var object arg = popSTACK(); # :identity - Argument
      if (!(eq(arg,unbound) || nullp(arg))) { flag_id = TRUE; }
    }
    { var object arg = popSTACK(); # :type - Argument
      if (!(eq(arg,unbound) || nullp(arg))) { flag_type = TRUE; }
    }
    if (!nullp(STACK_2)) { flag_fun = TRUE; }
    test_ostream(); # Output-Stream überprüfen
    if (test_value(S(print_readably))) { fehler_print_readably(STACK_1); }
   {var object* stream_ = &STACK_0;
    write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
    INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
    JUSTIFY_START;
    if (flag_type)
      { # (TYPE-OF object) ausgeben:
        pushSTACK(*(stream_ STACKop 1)); funcall(L(type_of),1);
        prin1(stream_,value1);
        if (flag_fun || flag_id) { JUSTIFY_SPACE; }
      }
    if (flag_fun)
      { funcall(*(stream_ STACKop 2),0); } # (FUNCALL function)
    if (flag_id)
      { if (flag_fun) { JUSTIFY_SPACE; }
        pr_hex6(stream_,*(stream_ STACKop 1));
      }
    JUSTIFY_END_ENG;
    INDENT_END;
    write_ascii_char(stream_,'>');
    skipSTACK(3);
    value1 = NIL; mv_count=1;
  }}

LISPFUN(line_position,0,1,norest,nokey,0,NIL)
# (SYS::LINE-POSITION [stream]), Hilfsfunktion für FORMAT ~T,
# liefert die Position eines (Output-)Streams in der momentanen Zeile, oder NIL.
  { test_ostream(); # Output-Stream überprüfen
    value1 = get_line_position(popSTACK()); mv_count=1; # Line-Position als Wert
  }

